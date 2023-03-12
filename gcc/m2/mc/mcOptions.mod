(* Copyright (C) 2015-2023 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE mcOptions ;

FROM SArgs IMPORT GetArg, Narg ;
FROM mcSearch IMPORT prependSearchPath ;
FROM libc IMPORT exit, printf, time, localtime, time_t, ptrToTM ;
FROM mcPrintf IMPORT printf0, printf1 ;
FROM Debug IMPORT Halt ;
FROM StrLib IMPORT StrLen ;
FROM decl IMPORT setLangC, setLangCP, setLangM2 ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string, char ;

IMPORT FIO ;
IMPORT SFIO ;

VAR
   langC,
   langCPP,
   langM2,
   gplHeader,
   glplHeader,
   summary,
   contributed,
   scaffoldMain,
   scaffoldDynamic,
   caseRuntime,
   arrayRuntime,
   returnRuntime,
   suppressNoReturn,
   useBoolType,
   gccConfigSystem,
   ignoreFQ,
   debugTopological,
   extendedOpaque,
   internalDebugging,
   verbose,
   quiet            : BOOLEAN ;
   projectContents,
   summaryContents,
   contributedContents,
   hPrefix,
   outputFile,
   cppArgs,
   cppProgram       : String ;



(*
   getYear - return the year.
*)

PROCEDURE getYear () : CARDINAL ;
VAR
   epoch    : time_t ;
   localTime: ptrToTM ;
BEGIN
   epoch := time (NIL) ;
   localTime := localtime (epoch) ;
   RETURN localTime^.tm_year + 1900
END getYear ;


(*
   displayVersion - displays the version of the compiler.
*)

PROCEDURE displayVersion (mustExit: BOOLEAN) ;
VAR
   year: CARDINAL ;
BEGIN
   year := getYear () ;
   (* These first three calls to printf hide the first line of text away from the year change script.  *)
   printf0 ('Copyright ') ;
   printf0 ('(C)') ;  (* A unicode char here would be good.  *)
   printf1 (' %d Free Software Foundation, Inc.\n', year) ;
   printf0 ('License GPLv3: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n') ;
   printf0 ('This is free software: you are free to change and redistribute it.\n') ;
   printf0 ('There is NO WARRANTY, to the extent permitted by law.\n') ;
   IF mustExit
   THEN
      exit (0)
   END
END displayVersion ;


(*
   displayHelp - display the mc help summary.
*)

PROCEDURE displayHelp ;
BEGIN
   printf0 ("usage: mc [--cpp] [-g] [--quiet] [--extended-opaque] [-q] [-v]") ;
   printf0 (" [--verbose] [--version] [--help] [-h] [-Ipath] [--olang=c]") ;
   printf0 (" [--olang=c++] [--olang=m2] [--debug-top]") ;
   printf0 (' [--gpl-header] [--glpl-header] [--summary="foo"]') ;
   printf0 (' [--contributed="foo"] [--project="foo"]') ;
   printf0 (" [--h-file-prefix=foo] [--automatic] [-o=foo] filename\n") ;

   printf0 ("  --cpp               preprocess through the C preprocessor\n") ;
   printf0 ("  -g                  emit debugging directives in the output language") ;
   printf0 ("                      so that the debugger will refer to the source\n") ;
   printf0 ("  -q --quiet          no output unless an error occurs\n") ;
   printf0 ("  -v --verbose        display preprocessor if invoked\n") ;
   printf0 ("  --version           display version and exit\n") ;
   printf0 ("  -h --help           display this help message\n") ;
   printf0 ("  -Ipath              set the module search path\n") ;
   printf0 ("  --olang=c           generate ansi C output\n") ;
   printf0 ("  --olang=c++         generate ansi C++ output\n") ;
   printf0 ("  --olang=m2          generate PIM4 output\n") ;
   printf0 ("  --extended-opaque   parse definition and implementation modules to\n") ;
   printf0 ("                      generate full type debugging of opaque types\n") ;
   printf0 ("  --debug-top         debug topological data structure resolving (internal)\n") ;
   printf0 ("  --h-file-prefix=foo set the h file prefix to foo\n") ;
   printf0 ("  -o=foo              set the output file to foo\n") ;
   printf0 ("  --ignore-fq         do not generate fully qualified idents\n") ;
   printf0 ("  --gcc-config-system do not use standard host include files, use gcc config and system instead\n");
   printf0 ("  --gpl-header        generate a GPL3 header comment at the top of the file\n") ;
   printf0 ("  --glpl-header       generate a GLPL3 header comment at the top of the file\n") ;
   printf0 ('  --summary="foo"     generate a one line summary comment at the top of the file\n') ;
   printf0 ('  --contributed="foo" generate a one line contribution comment near the top of the file\n') ;
   printf0 ('  --project="foo"     include the project name within the GPL3 or GLPL3 header\n') ;
   printf0 ('  --automatic         generate a comment at the start of the file warning not to edit as it was automatically generated\n') ;
   printf0 ('  --scaffold-dynamic  generate dynamic module initialization code for C++\n') ;
   printf0 ('  --scaffold-main     generate main function which calls upon the dynamic initialization support in M2RTS\n') ;
   printf0 ('  --suppress-noreturn suppress the emission of any attribute noreturn\n');
   printf0 ('  filename            the source file must be the last option\n') ;
   exit (0)
END displayHelp ;


(*
   commentBegin - issue a start of comment for the appropriate language.
*)

PROCEDURE commentBegin (f: File) ;
BEGIN
   IF langC OR langCPP
   THEN
      FIO.WriteString (f, '/* ')
   ELSIF langM2
   THEN
      FIO.WriteString (f, '(* ')
   END
END commentBegin ;


(*
   commentEnd - issue an end of comment for the appropriate language.
*)

PROCEDURE commentEnd (f: File) ;
BEGIN
   IF langC OR langCPP
   THEN
      FIO.WriteString (f, ' */') ; FIO.WriteLine (f)
   ELSIF langM2
   THEN
      FIO.WriteString (f, ' *)') ; FIO.WriteLine (f)
   END
END commentEnd ;


(*
   comment - write a comment to file, f, and also a newline.
*)

PROCEDURE comment (f: File; a: ARRAY OF CHAR) ;
BEGIN
   FIO.WriteString (f, a) ; FIO.WriteLine (f)
END comment ;


(*
   commentS - write a comment to file, f, and also a newline.
*)

PROCEDURE commentS (f: File; s: String) ;
BEGIN
   s := SFIO.WriteS (f, s) ; FIO.WriteLine (f)
END commentS ;


(*
   gplBody -
*)

PROCEDURE gplBody (f: File) ;
VAR
   year: CARDINAL ;
BEGIN
   year := getYear () ;
   printf1 ('Copyright (C) %d Free Software Foundation, Inc.\n', year) ;
   IF contributed
   THEN
      FIO.WriteString (f, "Contributed by ") ;
      contributedContents := SFIO.WriteS (f, contributedContents) ;
      FIO.WriteString (f, ".") ;
      FIO.WriteLine (f)
   END ;
   FIO.WriteLine (f) ;
   FIO.WriteString (f, "This file is part of ") ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   FIO.WriteString (f, ".") ;
   FIO.WriteLine (f) ; FIO.WriteLine (f) ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, " is software; you can redistribute it and/or modify") ;
   comment (f, "it under the terms of the GNU General Public License as published by") ;
   comment (f, "the Free Software Foundation; either version 3, or (at your option)") ;
   comment (f, "any later version.") ;
   FIO.WriteLine (f) ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, " is distributed in the hope that it will be useful, but") ;
   comment (f, "WITHOUT ANY WARRANTY; without even the implied warranty of") ;
   comment (f, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU") ;
   comment (f, "General Public License for more details.") ;
   FIO.WriteLine (f) ;
   comment (f, "You should have received a copy of the GNU General Public License") ;
   FIO.WriteString (f, "along with ") ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, "; see the file COPYING.  If not,") ;
   FIO.WriteString (f, "see <https://www.gnu.org/licenses/>. ")
END gplBody ;


(*
   glplBody -
*)

PROCEDURE glplBody (f: File) ;
VAR
   year: CARDINAL ;
BEGIN
   year := getYear () ;
   printf1 ('Copyright (C) %d Free Software Foundation, Inc.\n', year) ;
   IF contributed
   THEN
      FIO.WriteString (f, "Contributed by ") ;
      contributedContents := SFIO.WriteS (f, contributedContents) ;
      FIO.WriteString (f, ".") ;
      FIO.WriteLine (f)
   END ;
   FIO.WriteLine (f) ;
   FIO.WriteString (f, "This file is part of ") ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   FIO.WriteString (f, ".") ;
   FIO.WriteLine (f) ; FIO.WriteLine (f) ;

   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, " is free software; you can redistribute it and/or modify") ;
   comment (f, "it under the terms of the GNU General Public License as published by") ;
   comment (f, "the Free Software Foundation; either version 3, or (at your option)") ;
   comment (f, "any later version.") ;
   FIO.WriteLine (f) ;

   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, " is software; you can redistribute it and/or modify") ;
   comment (f, "it under the terms of the GNU Lesser General Public License") ;
   comment (f, "as published by the Free Software Foundation; either version 3,") ;
   comment (f, "or (at your option) any later version.") ;
   FIO.WriteLine (f) ;

   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, " is distributed in the hope that it will be useful, but") ;
   comment (f, "WITHOUT ANY WARRANTY; without even the implied warranty of") ;
   comment (f, "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU") ;
   comment (f, "General Public License for more details.") ;
   FIO.WriteLine (f) ;

   comment (f, "You should have received a copy of the GNU General Public License") ;
   FIO.WriteString (f, "along with ") ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, "; see the file COPYING3.  If not see") ;
   comment (f, "<http://www.gnu.org/licenses/>.") ;

   FIO.WriteLine (f) ;
   comment (f, "You should have received a copy of the GNU Lesser General Public License") ;
   FIO.WriteString (f, "along with ") ;
   projectContents := SFIO.WriteS (f, projectContents) ;
   comment (f, "; see the file COPYING.  If not,") ;
   FIO.WriteString (f, "see <https://www.gnu.org/licenses/>. ")
END glplBody ;


(*
   issueGPL - writes out the summary, GPL/LGPL and/or contributed as a single comment.
*)

PROCEDURE issueGPL (f: File) ;
BEGIN
   IF summary OR contributed OR gplHeader OR glplHeader
   THEN
      commentBegin (f) ;
      IF summary
      THEN
         commentS (f, summaryContents) ;
         FIO.WriteLine (f)
      END ;
      IF gplHeader
      THEN
         gplBody (f)
      END ;
      IF glplHeader
      THEN
         glplBody (f)
      END ;
      commentEnd (f) ;
      FIO.WriteLine (f)
   END
END issueGPL ;


(*
   writeGPLheader - writes out the GPL or the LGPL as a comment.
*)

PROCEDURE writeGPLheader (f: File) ;
BEGIN
   issueGPL (f)
END writeGPLheader ;


(*
   getCppCommandLine - returns the Cpp command line and all arguments.
*)

PROCEDURE getCppCommandLine () : String ;
VAR
   s: String ;
BEGIN
   IF EqualArray (cppProgram, '')
   THEN
      RETURN NIL
   ELSE
      s := Dup (cppProgram) ;
      s := ConCat (ConCatChar(s, ' '), cppArgs) ;
      IF getQuiet ()
      THEN
         s := ConCat (ConCatChar(s, ' '), Mark (InitString ('-quiet')))
      END ;
      RETURN s
   END
END getCppCommandLine ;


(*
   setOutputFile - sets the output filename to output.
*)

PROCEDURE setOutputFile (output: String) ;
BEGIN
   outputFile := output
END setOutputFile ;


(*
   getOutputFile - sets the output filename to output.
*)

PROCEDURE getOutputFile () : String ;
BEGIN
   RETURN outputFile
END getOutputFile ;


(*
   setQuiet - sets the quiet flag to, value.
*)

PROCEDURE setQuiet (value: BOOLEAN) ;
BEGIN
   quiet := value
END setQuiet ;


(*
   getQuiet - return the value of quiet.
*)

PROCEDURE getQuiet () : BOOLEAN ;
BEGIN
   RETURN quiet
END getQuiet ;


(*
   setVerbose - sets the verbose flag to, value.
*)

PROCEDURE setVerbose (value: BOOLEAN) ;
BEGIN
   verbose := value
END setVerbose ;


(*
   getVerbose - return the value of verbose.
*)

PROCEDURE getVerbose () : BOOLEAN ;
BEGIN
   RETURN verbose
END getVerbose ;


(*
   setExtendedOpaque - set extendedOpaque to value.
*)

PROCEDURE setExtendedOpaque (value: BOOLEAN) ;
BEGIN
   extendedOpaque := value
END setExtendedOpaque ;


(*
   getExtendedOpaque - return the extendedOpaque value.
*)

PROCEDURE getExtendedOpaque () : BOOLEAN ;
BEGIN
   RETURN extendedOpaque
END getExtendedOpaque ;


(*
   setSuppressNoReturn - set suppressNoReturn to value.
*)

PROCEDURE setSuppressNoReturn (value: BOOLEAN) ;
BEGIN
   suppressNoReturn := value
END setSuppressNoReturn;


(*
   getSuppressNoReturn - return the suppressNoReturn value.
*)

PROCEDURE getSuppressNoReturn () : BOOLEAN ;
BEGIN
   RETURN suppressNoReturn
END getSuppressNoReturn ;


(*
   setSearchPath - set the search path for the module sources.
*)

PROCEDURE setSearchPath (arg: String) ;
BEGIN
   prependSearchPath (arg)
END setSearchPath ;


(*
   setInternalDebugging - turn on/off internal debugging.
*)

PROCEDURE setInternalDebugging (value: BOOLEAN) ;
BEGIN
   internalDebugging := value
END setInternalDebugging ;


(*
   getInternalDebugging - return the value of internalDebugging.
*)

PROCEDURE getInternalDebugging () : BOOLEAN ;
BEGIN
   RETURN internalDebugging
END getInternalDebugging ;


(*
   setDebugTopological - sets the flag debugTopological to value.
*)

PROCEDURE setDebugTopological (value: BOOLEAN) ;
BEGIN
   debugTopological := value
END setDebugTopological ;


(*
   getDebugTopological - returns the flag value of the command
                         line option --debug-top.
*)

PROCEDURE getDebugTopological () : BOOLEAN ;
BEGIN
   RETURN debugTopological
END getDebugTopological ;


(*
   setHPrefix - saves the H file prefix.
*)

PROCEDURE setHPrefix (s: String) ;
BEGIN
   hPrefix := s
END setHPrefix ;


(*
   getHPrefix - saves the H file prefix.
*)

PROCEDURE getHPrefix () : String ;
BEGIN
   RETURN hPrefix
END getHPrefix ;


(*
   setIgnoreFQ - sets the ignorefq flag.
*)

PROCEDURE setIgnoreFQ (value: BOOLEAN) ;
BEGIN
   ignoreFQ := value
END setIgnoreFQ ;


(*
   getIgnoreFQ - returns the ignorefq flag.
*)

PROCEDURE getIgnoreFQ () : BOOLEAN ;
BEGIN
   RETURN ignoreFQ
END getIgnoreFQ ;


(*
   getGccConfigSystem - return the value of the gccConfigSystem flag.
*)

PROCEDURE getGccConfigSystem () : BOOLEAN ;
BEGIN
   RETURN gccConfigSystem
END getGccConfigSystem ;


(*
   getScaffoldDynamic - return true if the --scaffold-dynamic option was present.
*)

PROCEDURE getScaffoldDynamic () : BOOLEAN ;
BEGIN
   RETURN scaffoldDynamic
END getScaffoldDynamic ;


(*
   getScaffoldMain - return true if the --scaffold-main option was present.
*)

PROCEDURE getScaffoldMain () : BOOLEAN ;
BEGIN
   RETURN scaffoldMain
END getScaffoldMain ;


(*
   useBool - should mc use the bool type instead of int.
*)

PROCEDURE useBool () : BOOLEAN ;
BEGIN
   RETURN useBoolType
END useBool ;


(*
   optionIs - returns TRUE if the first len (right) characters
              match left.
*)

PROCEDURE optionIs (left: ARRAY OF CHAR; right: String) : BOOLEAN ;
VAR
   s: String ;
BEGIN
   IF Length (right) = StrLen (left)
   THEN
      RETURN EqualArray (right, left)
   ELSIF  Length (right) > StrLen (left)
   THEN
      s := Mark (Slice (right, 0, StrLen (left))) ;
      RETURN EqualArray (s, left)
   ELSE
      RETURN FALSE
   END
END optionIs ;


(*
   setLang - set the appropriate output language.
*)

PROCEDURE setLang (arg: String) ;
BEGIN
   (* must check the longest distinctive string first.  *)
   IF optionIs ("c++", arg)
   THEN
      setLangCP ;
      langCPP := TRUE
   ELSIF optionIs ("c", arg)
   THEN
      setLangC ;
      langC := TRUE
   ELSIF optionIs ("m2", arg)
   THEN
      setLangM2 ;
      langM2 := TRUE
   ELSE
      displayHelp
   END
END setLang ;


(*
   handleOption -
*)

PROCEDURE handleOption (arg: String) ;
BEGIN
   IF optionIs ("--quiet", arg) OR optionIs ("-q", arg)
   THEN
      setQuiet (TRUE)
   ELSIF optionIs ("--verbose", arg) OR optionIs ("-v", arg)
   THEN
      setVerbose (TRUE)
   ELSIF optionIs ("--version", arg)
   THEN
      displayVersion (TRUE)
   ELSIF optionIs ("--olang=", arg)
   THEN
      setLang (Slice (arg, 8, 0))
   ELSIF optionIs ("-I", arg)
   THEN
      setSearchPath (Slice (arg, 2, 0))
   ELSIF optionIs ("--help", arg) OR optionIs ("-h", arg)
   THEN
      displayHelp
   ELSIF optionIs ("--cpp", arg)
   THEN
      cppProgram := InitString ('cpp')
   ELSIF optionIs ("-o=", arg)
   THEN
      setOutputFile (Slice (arg, 3, 0))
   ELSIF optionIs ("--extended-opaque", arg)
   THEN
      setExtendedOpaque (TRUE)
   ELSIF optionIs ("--debug-top", arg)
   THEN
      setDebugTopological (TRUE)
   ELSIF optionIs ("--h-file-prefix=", arg)
   THEN
      setHPrefix (Slice (arg, 16, 0))
   ELSIF optionIs ("--ignore-fq", arg)
   THEN
      setIgnoreFQ (TRUE)
   ELSIF optionIs ("--gpl-header", arg)
   THEN
      gplHeader := TRUE
   ELSIF optionIs ("--glpl-header", arg)
   THEN
      glplHeader := TRUE
   ELSIF optionIs ('--summary="', arg)
   THEN
      summary := TRUE ;
      summaryContents := Slice (arg, 11, -1)
   ELSIF optionIs ('--contributed="', arg)
   THEN
      contributed := TRUE ;
      contributedContents := Slice (arg, 13, -1)
   ELSIF optionIs ('--project="', arg)
   THEN
      projectContents := Slice (arg, 10, -1)
   ELSIF optionIs ('--gcc-config-system', arg)
   THEN
      gccConfigSystem := TRUE
   ELSIF optionIs ('--scaffold-main', arg)
   THEN
      scaffoldMain := TRUE
   ELSIF optionIs ('--scaffold-dynamic', arg)
   THEN
      scaffoldDynamic := TRUE
   ELSIF optionIs ('--suppress-noreturn', arg)
   THEN
      suppressNoReturn := TRUE
   END
END handleOption ;


(*
   handleOptions - iterates over all options setting appropriate
                   values and returns the single source file
                   if found at the end of the arguments.
*)

PROCEDURE handleOptions () : String ;
VAR
   i  : CARDINAL ;
   arg: String ;
BEGIN
   i := 1 ;
   WHILE GetArg (arg, i) DO
      IF Length (arg) > 0
      THEN
         IF char (arg, 0)='-'
         THEN
            handleOption (arg)
         ELSE
            IF NOT summary
            THEN
               summaryContents := ConCatChar (ConCat (InitString ('automatically created by mc from '),
                                                      arg), '.') ;
               summary := FALSE
            END ;
            RETURN arg
         END
      END ;
      INC (i)
   END ;
   RETURN NIL
END handleOptions ;


BEGIN
   langC := TRUE ;
   langCPP := FALSE ;
   langM2 := FALSE ;
   gplHeader := FALSE ;
   glplHeader := FALSE ;
   summary := FALSE ;
   contributed := FALSE ;
   caseRuntime := FALSE ;
   arrayRuntime := FALSE ;
   returnRuntime := FALSE ;
   internalDebugging := FALSE ;
   quiet := FALSE ;
   verbose := FALSE ;
   extendedOpaque := FALSE ;
   debugTopological := FALSE ;
   ignoreFQ := FALSE ;
   gccConfigSystem := FALSE ;
   scaffoldMain := FALSE ;
   scaffoldDynamic := FALSE ;
   suppressNoReturn := FALSE ;
   useBoolType := TRUE ;
   hPrefix := InitString ('') ;
   cppArgs := InitString ('') ;
   cppProgram := InitString ('') ;
   outputFile := InitString ('-') ;
   summaryContents := InitString ('') ;
   contributedContents := InitString ('') ;
   projectContents := InitString ('GNU Modula-2')
END mcOptions.
