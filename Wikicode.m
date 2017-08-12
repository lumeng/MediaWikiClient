(**

* Original version: https://en.wikipedia.org/wiki/User:Wakebrdkid/Wikicode

 **)

BeginPackage["Wikicode`"];

Contents::usage =
  "Contents[] returns a list of the Wikipedia articles that currently \
have associated packages in Wikicode.";

Load::usage =
  "Load[title] imports and evaluates a specific package associated \
with the given article. Returns $Failed if the package was not found.";

Login::usage =
  "Login[username,password] begins an authenticated session on \
Wikipedia and sets the appropriate HTTP cookies in Mathematica. It \
returns True if successful and False otherwise.";

Create::usage =
  "Create[title,code] starts a new package on Wikicode with the given \
title and content. It returns $Failed if a package already exists for \
that article, if the given title does not match an article, or if the \
code has a syntax error. For now, edits and merges should be done \
manually through the Wikipedia interface. If you are creating \
multiple packages it is recommended that you insert a 5 second pause \
between calls or Wikipedia might temporarily block your account if \
you do too many edits.";

Begin["`Private`"];

Contents[] := (Contents[] =
   Flatten@StringCases[
     Import["http://en.wikipedia.org/w/api.php?action=query&titles=\
User:Wakebrdkid/Wikicode&prop=links&pllimit=500&format=json",
       "JSON"][[1, 2, 1, 2, 1, 2, 1, 2, All, 2, 2]],
     "User:Wakebrdkid/" ~~ name : Except@"/" .. ~~ EndOfString ->
      name]; contextsToArticles =
   StringSplit[
       StringReplace[
        StringReplace[#,
         a : Except@LetterCharacter ~~ b : LetterCharacter :>
          a <> ToUpperCase@b], {"(" -> "`",
         Except[LetterCharacter] -> ""}], "`"] -> # & /@ Contents[];
  Contents[])

Load[title_String] :=
 If[ToExpression@#[[1]] > 0,
    ToExpression@
     StringCases[#[[2, 3, 2, 1, 1, 2]],
       "<syntaxhighlight lang=\"objc\">" ~~ code__ ~~ "<" ~~
         "/syntaxhighlight>" -> code][[1]], $Failed] &@
  Import["http://en.wikipedia.org/w/api.php?action=query&titles=User:\
Wakebrdkid/" <> urlEncode[title] <>
     "&prop=revisions&rvprop=content&format=json", "JSON"][[1, 2, 1,
   2, 1]]

Login[username_String, password_String] :=
 Module[{url = "http://en.wikipedia.org/w/api.php",
   parameters = {"action" -> "login", "lgname" -> username,
     "lgpassword" -> password, "format" -> "json"}},
  URLFetch[url, "Method" -> "POST",
   "Parameters" ->
    Append[parameters,
     "lgtoken" ->
      ImportString[
        URLFetch[url, "Method" -> "POST", "Parameters" -> parameters],
         "JSON"][[1, 2, 4, 2]]]];
  editToken =
   Import["http://en.wikipedia.org/w/api.php?action=query&prop=info|\
revisions&intoken=edit&titles=Main%20Page&format=json", "JSON"][[1, 2,
     1, 2, 1, 2, 3, 2]];
  If[MemberQ[$HTTPCookies, {__, "Name" -> "enwikiUserID", __}] &&
    Head@editToken == String, True, False]]

Create[title_String, code_String] :=
 If[! (SyntaxQ@code && articleQ@title &&
     FreeQ[ImportString[
       URLFetch[
        "http://en.wikipedia.org/w/api.php?action=edit&format=json&\
createonly", "Method" -> "POST", "Headers" -> {"Expect" -> ""},
        "MultipartData" -> ({#, "",
             ToCharacterCode@#2} & @@@ {"title" ->
             "User:Wakebrdkid/" <> title,
            "summary" -> "Starting a package for " <> title <> ".",
            "text" -> newPackageTemplate[title, code],
            "token" -> editToken})], "JSON"], "error" -> _] &&
     FreeQ[ImportString[
       URLFetch[
        "http://en.wikipedia.org/w/api.php?action=edit&format=json&\
section=1", "Method" -> "POST", "Headers" -> {"Expect" -> ""},
        "MultipartData" -> ({#, "",
             ToCharacterCode@#2} & @@@ {"title" ->
             "User:Wakebrdkid/Wikicode",
            "summary" -> "Updating package list.",
            "text" ->
             "==Packages==\n" <>
              StringJoin[
               "* [[User:Wakebrdkid/" <> # <> "|" <> # <>
                  "]] [[User:Wakebrdkid/" <> # <>
                  "/tests|(tests)]]\n" & /@
                Sort@Append[Contents[], title]],
            "token" -> editToken})], "JSON"], "error" -> _]), $Failed]

(*This will eventually be moved to the package associated with the \
percent-encoding article,but we need some code on the front page \
people can copy and paste to get started easily.*)
urlEncode[string_String] :=
 StringReplace[string,
  c : Except@
     Flatten@{CharacterRange @@@ {{"A", "Z"}, {"a", "z"}, {"0", "9"}},
        Characters["-_.~"]} :>
   StringJoin[
    "%" <> IntegerString[#, 16, 2] & /@ ToCharacterCode[c, "UTF-8"]]]

(*Returns True if the given string is an article title on the English \
Wikipedia.*)
articleQ[title_String] :=
 ToExpression@("pages" /. ("query" /.
        Import["http://en.wikipedia.org/w/api.php?action=query&titles=\
" <> urlEncode[title] <> "&format=json", "JSON"]))[[1, 1]] >= 0

newPackageTemplate[title_String, code_String] :=
 "[[" <> title <> "|Article]]
  
  [[/tests|Tests]]
  
  <syntaxhighlight lang=\"objc\">
  " <> code <> "
  <" <> "/syntaxhighlight>" (*split string on this line for wiki \
formatting*)

End[];
EndPackage[]
$ContextPath = Rest@$ContextPath;

SetOptions[$FrontEndSession,
 FrontEndEventActions -> {{"KeyDown", "`"} :>
    Module[{i = InputNotebook[], temp},
     Switch[#, {"Wikicode"},
        Symbol["Wikicode`" <>
            StringReplace[#, {"(" -> "`",
              Except[LetterCharacter] -> ""}]] & /@
         Wikicode`Contents[], {"Wikicode", __},
        If[MatchQ[#, _String],
           Wikicode`Load@#; $ContextPath = Rest@$ContextPath] &[
         Rest@# /. Wikicode`Private`contextsToArticles]] &[
      StringSplit[Switch[#2, _String, #2, _, #], "`"] &[
       SelectionMove[i, All, Expression]; NotebookRead@i,
       SelectionMove[i, All, Expression]; NotebookRead@i]];
     SelectionMove[i, After, Character];
     temp = CreateWindow@
       DocumentNotebook[ExpressionCell[Null, "Input"],
        Visible -> False]; SelectionMove[temp, Next, Cell];
     SelectionEvaluate@temp; NotebookClose@temp],
   PassEventsDown -> True}]
