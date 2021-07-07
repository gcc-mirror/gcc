(* gm2lgen.mod generates the main C function from a list of module names.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE gm2lgen ;

(*
   Author     : Gaius Mulley
   Title      : gm2lgen
   Date       : Fri Sep 15 14:42:17 BST 1989
   Description: Generates the main C function, from a list of module names.
*)

FROM libc IMPORT exit ;
FROM ASCII IMPORT eof ;
FROM SArgs IMPORT GetArg ;

FROM Indexing IMPORT Index, InitIndex, KillIndex, HighIndice, LowIndice,
                     IncludeIndiceIntoIndex, GetIndice ;

FROM FIO IMPORT File, StdIn, StdOut, StdErr, WriteChar,
                ReadString, WriteString, EOF, IsNoError, WriteLine, Close ;

FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, RemoveWhitePrefix,
                    EqualArray, Mark, Assign, Fin, InitStringChar, Length, Slice, Equal,
                    RemoveComment ;

FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2 ;
FROM SFIO IMPORT OpenToWrite, WriteS, ReadS, OpenToRead ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1 ;


CONST
   Comment = '#'  ; (* Comment leader      *)

VAR
   CPlusPlus,
   SharedLibrary,
   NeedInitial,
   NeedTerminate,
   ExitNeeded   : BOOLEAN ;
   MainName     : String ;
   FunctionList : Index ;
   fi, fo       : File ;


(*
   OpenOutputFile - attempts to open an output file.
*)

PROCEDURE OpenOutputFile (s: String) ;
BEGIN
   fo := OpenToWrite(s) ;
   IF NOT IsNoError(fo)
   THEN
      fprintf1(StdErr, 'cannot write to: %s\n', s) ;
      exit(1)
   END
END OpenOutputFile ;


(*
   OpenInputFile - attempts to open an input file.
*)

PROCEDURE OpenInputFile (s: String) ;
BEGIN
   fi := OpenToRead(s) ;
   IF NOT IsNoError(fo)
   THEN
      fprintf1 (StdErr, 'cannot open: %s\n', s) ;
      exit (1)
   END
END OpenInputFile ;


(*
   DisplayHelp - display brief help and exit.
*)

PROCEDURE DisplayHelp ;
BEGIN
   fprintf0 (StdErr, 'gm2lgen [--exit] [-fcpp] [-fshared] [-h] [--help] [--main function]\n');
   fprintf0 (StdErr, '        [-o outputfile] [--terminate] [inputfile]\n');
   exit (0)
END DisplayHelp ;


(*
   ScanArgs -
*)

PROCEDURE ScanArgs ;
VAR
   i: CARDINAL ;
   s: String ;
BEGIN
   i             := 1 ;
   CPlusPlus     := FALSE ;
   NeedTerminate := TRUE ;
   NeedInitial   := TRUE ;
   ExitNeeded    := TRUE ;
   SharedLibrary := FALSE ;
   MainName      := InitString('main') ;
   fi            := StdIn ;
   fo            := StdOut ;
   WHILE GetArg(s, i) DO
      IF EqualArray(s, '--exit')
      THEN
         ExitNeeded := FALSE
      ELSIF EqualArray(s, '--terminate')
      THEN
         NeedTerminate := FALSE
      ELSIF EqualArray(s, '--initial')
      THEN
         NeedInitial := FALSE
      ELSIF EqualArray(s, '-h') OR EqualArray(s, '--help')
      THEN
         DisplayHelp
      ELSIF EqualArray(s, '-fshared')
      THEN
         SharedLibrary := TRUE
      ELSIF EqualArray(s, '-fcpp')
      THEN
         CPlusPlus := TRUE
      ELSIF EqualArray(s, '-o')
      THEN
         INC(i) ;
         IF GetArg(s, i)
         THEN
            OpenOutputFile(s)
         ELSE
            fprintf0(StdErr, 'missing filename option after -o\n') ;
            exit(1)
         END
      ELSIF EqualArray(s, '--main')
      THEN
         INC(i) ;
         IF GetArg(s, i)
         THEN
            MainName := Assign(MainName, s)
         ELSE
            fprintf0(StdErr, 'missing functionname after option -main\n') ;
            exit(1)
         END
      ELSE
         OpenInputFile(s)
      END ;
      INC(i)
   END
END ScanArgs ;


(*
   GenInit -
*)

PROCEDURE GenInit ;
BEGIN
   IF SharedLibrary
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nvoid __attribute__ ((constructor)) init (void);\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nvoid __attribute__ ((constructor)) init (void)\n'))))))
   ELSE
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nstatic void init (int argc, char *argv[])\n')))))) ;
   END ;
   Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('{\n'))))));
   GenInitializationCalls ;
   Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('}\n'))))));
END GenInit ;


(*
   GenFinish -
*)

PROCEDURE GenFinish ;
BEGIN
   IF SharedLibrary
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nvoid __attribute__ ((destructor)) finish (void);\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nvoid __attribute__ ((destructor)) finish (void)\n'))))))
   ELSE
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nstatic void finish (void)\n'))))))
   END ;
   Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('{\n')))))) ;
   GenFinalizationCalls ;
   Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('}\n'))))))
END GenFinish ;


(*
   GenMain - writes out the main() function together with module initialization
             calls.
*)

PROCEDURE GenMain ;
BEGIN
   FunctionList := InitIndex(1) ;
   ScanArgs ;
   BuildFunctionList ;
   GenExternals ;
   GenInit ;
   GenFinish ;
   IF NOT SharedLibrary
   THEN
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('\nint %s (int argc, char *argv[])\n')), MainName)))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('{\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   init (argc, argv);\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   finish ();\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   return (0);\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('}\n'))))))
   END ;
   Close(fo)
END GenMain ;


(*
   GenExternals - writes out the external prototypes for each module initializer.
*)

PROCEDURE GenExternals ;
VAR
   funcname: String ;
   i, n    : CARDINAL ;
BEGIN
   IF ExitNeeded
   THEN
      Fin(WriteS(fo, Mark(InitString('extern ')))) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('"C"'))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString(' void exit(int);\n\n')))))) ;
   END ;
   IF CPlusPlus
   THEN
      Fin(WriteS(fo, Mark(InitString('extern "C"')))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString(' void RTExceptions_DefaultErrorCatch(void);\n'))))))
   END ;
   n := HighIndice(FunctionList) ;
   i := 1 ;
   WHILE i<=n DO
      funcname := GetIndice(FunctionList, i) ;
      Fin(WriteS(fo, Mark(InitString('extern ')))) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('"C"'))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString(' void _M2_%s_init (int argc, char *argv[]);\n')), funcname)))) ;
      Fin(WriteS(fo, Mark(InitString('extern ')))) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('"C"'))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString(' void _M2_%s_finish (void);\n')), funcname)))) ;
      INC(i)
   END ;
   IF NeedTerminate
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nextern ')))))) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('"C"'))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString(' void M2RTS_ExecuteTerminationProcedures(void);\n'))))))
   END ;
   IF NeedInitial
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('\nextern ')))))) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('"C"'))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString(' void M2RTS_ExecuteInitialProcedures(void);\n'))))))
   END
END GenExternals ;


(*
   GenInitializationCalls - writes out the initialization calls for the modules
                            in the application suit.
*)

PROCEDURE GenInitializationCalls ;
VAR
   funcname: String ;
   i, n    : CARDINAL ;
BEGIN
   n := HighIndice(FunctionList) ;
   i := LowIndice(FunctionList) ;
   IF CPlusPlus
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   try {\n'))))))
   END ;
   WHILE i<=n DO
      IF i=n
      THEN
         IF NeedInitial
         THEN
            IF CPlusPlus
            THEN
               Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   '))))))
            END ;
            Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   M2RTS_ExecuteInitialProcedures ();\n'))))))
         END
      END ;
      funcname := GetIndice(FunctionList, i) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(InitString('   '))))
      END ;
      IF SharedLibrary
      THEN
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('    _M2_%s_init (0, (char **)0);\n')),
                                      funcname))))
      ELSE
         Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('    _M2_%s_init (argc, argv);\n')),
                                      funcname))))
      END ;
      INC(i)
   END ;
   IF CPlusPlus
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    }\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    catch (...) {\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('       RTExceptions_DefaultErrorCatch();\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    }\n'))))))
   END ;
END GenInitializationCalls ;


(*
   GenFinalizationCalls - writes out the finalization calls for the modules
                          in the application suit.
*)

PROCEDURE GenFinalizationCalls ;
VAR
   funcname: String ;
   i, n    : CARDINAL ;
BEGIN
   IF CPlusPlus
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   try {\n'))))))
   END ;
   IF NeedTerminate
   THEN
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   '))))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   M2RTS_ExecuteTerminationProcedures ();\n'))))))
   END ;
   n := HighIndice(FunctionList) ;
   i := LowIndice(FunctionList) ;
   WHILE i<=n DO
      funcname := GetIndice(FunctionList, n) ;
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   '))))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf1(Mark(InitString('   _M2_%s_finish ();\n')),
                                   funcname)))) ;
      DEC(n)
   END ;
   IF ExitNeeded
   THEN
      IF CPlusPlus
      THEN
         Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   '))))))
      END ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('   exit (0);\n'))))))
   END ;
   IF CPlusPlus
   THEN
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    }\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    catch (...) {\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('       RTExceptions_DefaultErrorCatch();\n')))))) ;
      Fin(WriteS(fo, Mark(Sprintf0(Mark(InitString('    }\n'))))))
   END
END GenFinalizationCalls ;


(*
   BuildFunctionList - reads in the list of functions and stores them.
*)

PROCEDURE BuildFunctionList ;
VAR
   s: String ;
BEGIN
   REPEAT
      s := RemoveComment(RemoveWhitePrefix(ReadS(fi)), Comment) ;
      IF (NOT Equal(Mark(InitStringChar(Comment)),
                    Mark(Slice(s, 0, Length(Mark(InitStringChar(Comment)))-1)))) AND
         (NOT EqualArray(s, ''))
      THEN
         IncludeIndiceIntoIndex(FunctionList, s)
      END
   UNTIL EOF(fi)
END BuildFunctionList ;


BEGIN
   GenMain
END gm2lgen.
