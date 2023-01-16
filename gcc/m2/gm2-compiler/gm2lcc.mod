(* gm2lcc.mod generates the link command from a list of modules.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

MODULE gm2lcc ;

(*
   Author     : Gaius Mulley
   Title      : gm2lcc
   Date       : Fri Jul 24 11:45:08 BST 1992
   Description: generates the link command from a list of modules.
*)

FROM libc IMPORT system, exit ;
FROM SYSTEM IMPORT ADR ;
FROM NameKey IMPORT Name, MakeKey, WriteKey, GetKey ;
FROM M2Search IMPORT FindSourceFile, PrependSearchPath ;
FROM M2FileName IMPORT CalculateFileName ;
FROM SArgs IMPORT GetArg ;
FROM StrLib IMPORT StrEqual, StrLen, StrCopy, StrConCat, StrRemoveWhitePrefix, IsSubString ;
FROM FIO IMPORT File, StdIn, StdErr, StdOut, Close, IsNoError, EOF, WriteString, WriteLine ;
FROM SFIO IMPORT OpenToRead, WriteS, ReadS ;
FROM ASCII IMPORT nul ;
FROM M2FileName IMPORT ExtractExtension ;
FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, ConCatChar, Length, Slice, Equal, EqualArray, RemoveWhitePrefix, RemoveWhitePostfix, RemoveComment, string, Mark, InitStringChar, Dup, Mult, Assign, char ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;
FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2, fprintf3, fprintf4 ;
FROM ObjectFiles IMPORT FileObjects, InitFileObject, KillFileObject,
                        RegisterModuleObject, IsRegistered ;
FROM Indexing IMPORT Index, InitIndex, GetIndice,
                     HighIndice, LowIndice, IncludeIndiceIntoIndex,
                     ForeachIndiceInIndexDo ;



CONST
   Comment     =     '#' ;      (* Comment leader.                 *)
   MaxSpaces   =      20 ;      (* Maximum spaces after a module   *)
                                (* name.                           *)
   Debugging   =     FALSE ;

VAR
   DebugFound    : BOOLEAN ;
   CheckFound    : BOOLEAN ;
   VerboseFound  : BOOLEAN ;
   ProfileFound  : BOOLEAN ;
   LibrariesFound: BOOLEAN ;
   TargetFound   : BOOLEAN ;
   ExecCommand   : BOOLEAN ;    (* should we execute the final cmd *)
   UseAr         : BOOLEAN ;    (* use 'ar' and create archive     *)
   UseRanlib     : BOOLEAN ;    (* use 'ranlib' to index archive   *)
   IgnoreMain    : BOOLEAN ;    (* ignore main module when linking *)
   UseLibtool    : BOOLEAN ;    (* use libtool and suffixes?       *)
   Shared        : BOOLEAN ;    (* is a shared library required?   *)
   BOption,                  (* the full -B option and directory.  *)
   FOptions,
   CompilerDir,               (* contains the directory after -B.  *)
   RanlibProgram,
   ArProgram,
   Archives,
   Path,
   StartupFile,
   Libraries,
   MainModule,
   MainObject,
   Command,
   Target        : String ;
   CmdLine,
   objects       : FileObjects ;
   CmdLineObjects: Index ;
   fi, fo        : File ;       (* the input and output files      *)


(*
   FlushCommand - flush the command to the output file,
                  or execute the command.
*)

PROCEDURE FlushCommand () : INTEGER ;
BEGIN
   IF ExecCommand
   THEN
      IF VerboseFound
      THEN
         Command := WriteS (StdOut, Command) ;
         fprintf0 (StdOut, '\n')
      END ;
      RETURN system (string (Command))
   ELSE
      Command := WriteS (fo, Command)
   END ;
   RETURN 0
END FlushCommand ;


(*
   GenerateLinkCommand - generate the appropriate linkage command
                         with the correct options.
*)

PROCEDURE GenerateLinkCommand ;
BEGIN
   IF UseAr
   THEN
      Command := ConCat (ArProgram, InitString (' rc ')) ;
      IF TargetFound
      THEN
         Command := ConCat (Command, Target) ;
         Command := ConCatChar (Command, ' ')
      ELSE
         WriteString (StdErr, 'need target with ar') ; WriteLine (StdErr) ; Close (StdErr) ;
         exit (1)
      END
   ELSIF UseLibtool
   THEN
      Command := InitString ('libtool --tag=CC --mode=link gcc ') ;
      IF BOption # NIL
      THEN
         Command := ConCat (Command, Dup (BOption)) ;
         Command := ConCatChar (Command, ' ')
      END ;
      IF DebugFound
      THEN
         Command := ConCat (Command, Mark (InitString ('-g ')))
      END ;
      IF ProfileFound
      THEN
         Command := ConCat(Command, Mark(InitString ('-p ')))
      END ;
      Command := ConCat (Command, FOptions) ;
      IF Shared
      THEN
         Command := ConCat (Command, Mark (InitString ('-shared ')))
      END ;
      IF TargetFound
      THEN
         Command := ConCat (Command, Mark (InitString ('-o '))) ;
         Command := ConCat (Command, Target) ;
         Command := ConCatChar (Command, ' ')
      END ;
      IF ProfileFound
      THEN
         Command := ConCat (Command, Mark (InitString ('-lgmon ')))
      END
   END
END GenerateLinkCommand ;


(*
   GenerateRanlibCommand - generate the appropriate ranlib command.
*)

PROCEDURE GenerateRanlibCommand ;
BEGIN
   Command := ConCat (RanlibProgram, Mark (InitStringChar (' '))) ;
   IF TargetFound
   THEN
      Command := ConCat (Command, Target) ;
      Command := ConCatChar (Command, ' ')
   ELSE
      WriteString (StdErr, 'need target with ranlib') ; WriteLine (StdErr) ; Close (StdErr) ;
      exit (1)
   END
END GenerateRanlibCommand ;


(*
   RemoveLinkOnly - removes the <onlylink> prefix, if present.  This will occur
                    for a definition for "C" module where there is no
                    module constructor/destructor (_init and _finish pairs).
                    Otherwise, s, is returned.
*)

PROCEDURE RemoveLinkOnly (s: String) : String ;
VAR
   t: String ;
BEGIN
   t := InitString ('<onlylink>') ;
   IF Equal (Mark (Slice (s, 0, Length (t)-1)), t)
   THEN
      RETURN RemoveWhitePrefix (Slice (Mark (s), Length (t), 0))
   ELSE
      RETURN s
   END
END RemoveLinkOnly ;


(*
   ConCatStartupFile - check to see if the startup object file has not been added
                       to the command line and if so add it.
*)

PROCEDURE ConCatStartupFile ;
BEGIN
   IF RegisterModuleObject (objects, StartupFile)
   THEN
      IF UseLibtool
      THEN
         Command := ConCat (Command, Mark (Sprintf1 (Mark (InitString ('%s.lo')),
                                                     StartupFile)))
      ELSE
         Command := ConCat (Command, Mark (Sprintf1 (Mark (InitString ('%s.o')),
                                                     StartupFile)))
      END
   END
END ConCatStartupFile ;


(*
   GenObjectSuffix -
*)

PROCEDURE GenObjectSuffix () : String ;
BEGIN
   IF UseLibtool
   THEN
      RETURN InitString ('lo')
   ELSE
      RETURN InitString ('o')
   END
END GenObjectSuffix ;


(*
   GenArchiveSuffix -
*)

PROCEDURE GenArchiveSuffix () : String ;
BEGIN
   IF UseLibtool
   THEN
      RETURN InitString ('la')
   ELSE
      RETURN InitString ('a')
   END
END GenArchiveSuffix ;


(*
   LookupObjectFile - attempts to lookup the location of file name.extension
                      from using the -fobject-path path.  NIL is retured if
                      the object file is not found.  extension will be
                      marked and deleted.
*)

PROCEDURE LookupObjectFile (name, extension: String) : String ;
VAR
   location,
   filename: String ;
BEGIN
   filename := CalculateFileName (name, Mark (extension)) ;
   IF FindSourceFile (filename, location)
   THEN
      RETURN location
   ELSE
      RETURN NIL
   END
END LookupObjectFile ;


(*
   ConCatModuleObject - this object will be associated with a module, therefore
                        we remember it and make sure that it is not duplicated on the
                        command line by the user.
*)

PROCEDURE ConCatModuleObject (module: String) ;
VAR
   location: String ;
BEGIN
   location := LookupObjectFile (module, GenObjectSuffix ()) ;
   IF location = NIL
   THEN
      location := LookupObjectFile (module, GenArchiveSuffix ()) ;
      IF location # NIL
      THEN
         Archives := ConCatChar (ConCat (Archives, location), ' ')
      END
   ELSE
      IF RegisterModuleObject (objects, location)
      THEN
         Command := ConCat (ConCatChar (Command, ' '), location)
      END
   END
END ConCatModuleObject ;


(*
   FindModulesInFileList -
*)

PROCEDURE FindModulesInFileList ;
VAR
   text: String ;
BEGIN
   REPEAT
      text := RemoveComment (RemoveWhitePrefix( ReadS (fi)), Comment) ;
      IF (NOT EqualArray (text, '')) AND (NOT (IgnoreMain AND Equal (text, MainModule)))
      THEN
         text := RemoveLinkOnly (text) ;
         ConCatModuleObject (text)
      END
   UNTIL EOF (fi) ;
   IF (NOT EqualArray (MainObject, "")) AND RegisterModuleObject (objects, MainObject)
   THEN
      Command := ConCat (ConCatChar (Command, ' '), MainObject)
   END
END FindModulesInFileList ;


(*
   CollectObjects -
*)

PROCEDURE CollectObjects (Command: String) : String ;
VAR
   i, h: CARDINAL ;
   name: String ;
BEGIN
   i := 1 ;
   h := HighIndice (CmdLineObjects) ;
   WHILE i <= h DO
      name := GetIndice (CmdLineObjects, i) ;
      IF NOT IsRegistered (objects, name)
      THEN
         Command := ConCat (ConCatChar (Command, ' '), Dup (name))
      END ;
      INC (i)
   END ;
   IF Debugging
   THEN
      fprintf1 (StdErr, "objects on command line: %s\n", Command)
   END ;
   RETURN Command
END CollectObjects ;


(*
   CollectArchives -
*)

PROCEDURE CollectArchives (Command: String) : String ;
BEGIN
   IF LibrariesFound
   THEN
      Command := ConCat (ConCatChar (Command, ' '), Libraries)
   END ;
   RETURN Command
END CollectArchives ;


(*
   AddProgramModule - add the program module to the Command string, providing
                      that the user did not specify it on the command line.
*)

PROCEDURE AddProgramModule (Command: String) : String ;
BEGIN
   IF Debugging
   THEN
      fprintf1 (StdErr, "mainobject: %s\n", MainObject)
   END ;
   IF (NOT EqualArray (MainObject, "")) AND RegisterModuleObject (objects, MainObject)
   THEN
      IF Debugging
      THEN
         fprintf1 (StdErr, "first time: %s\n", MainObject)
      END ;
      Command := ConCat (ConCatChar (Command, ' '), MainObject)
   ELSE
      IF Debugging
      THEN
         fprintf0 (StdErr, "(ignored)\n")
      END
   END ;
   RETURN Command
END AddProgramModule ;


(*
   GenCC - writes out the linkage command for the C compiler.
*)

PROCEDURE GenCC ;
VAR
   Error: INTEGER ;
BEGIN
   GenerateLinkCommand ;
   ConCatStartupFile ;
   FindModulesInFileList ;
   Command := AddProgramModule (Command) ;
   Command := ConCat (Command, Archives) ;
   Command := CollectObjects (Command) ;
   Command := CollectArchives (Command) ;
   Error := FlushCommand () ;
   IF Error=0
   THEN
      IF UseRanlib
      THEN
         GenerateRanlibCommand ;
         Error := FlushCommand () ;
         IF Error#0
         THEN
            fprintf1 (StdErr, 'ranlib failed with exit code %d\n', Error) ;
            Close (StdErr) ;
            exit (Error)
         END
      END
   ELSE
      fprintf1 (StdErr, 'ar failed with exit code %d\n', Error) ;
      Close (StdErr) ;
      exit (Error)
   END
END GenCC ;


(*
   WriteModuleName - displays a module name, ModuleName, with formatted spaces
                     after the string.
*)

(*
PROCEDURE WriteModuleName (ModuleName: String) ;
BEGIN
   ModuleName := WriteS (fo, ModuleName) ;
   IF KillString (WriteS (fo, Mark (Mult (Mark (InitString (' ')), MaxSpaces-Length(ModuleName))))) = NIL
   THEN
   END
END WriteModuleName ;
*)


(*
   CheckCC - checks to see whether all the object files can be found
             for each module.
*)

PROCEDURE CheckCC ;
VAR
   s, t, u: String ;
   Error  : INTEGER ;
BEGIN
   Error := 0 ;
   REPEAT
      s := RemoveComment (RemoveWhitePrefix (ReadS (fi)), Comment) ;
      IF NOT EqualArray (s, '')
      THEN
         s := RemoveLinkOnly (s) ;
         t := Dup (s) ;
         t := CalculateFileName (s, Mark (GenObjectSuffix ())) ;
         IF FindSourceFile (t, u)
         THEN
            IF KillString (WriteS (fo, Mark (Sprintf2 (Mark (InitString ('%-20s : %s\n')), t, u)))) = NIL
            THEN
            END ;
            u := KillString (u)
         ELSE
            t := KillString (t) ;
            (* try finding .a archive *)
            t := CalculateFileName (s, Mark (GenArchiveSuffix ())) ;
            IF FindSourceFile (t, u)
            THEN
               IF KillString (WriteS (fo, Mark (Sprintf2 (Mark (InitString ('%-20s : %s\n')), t, u)))) = NIL
               THEN
               END ;
               u := KillString (u)
            ELSE
               IF KillString (WriteS (fo, Mark (Sprintf1 (InitString ('%-20s : distinct object or archive not found\n'), t)))) = NIL
               THEN
               END ;
               Error := 1
            END
         END
      END
   UNTIL EOF (fi) ;
   Close (fo) ;
   exit (Error)
END CheckCC ;


(*
   ProcessTarget - copies the specified target file into Target
                   and sets the boolean TargetFound.
*)

PROCEDURE ProcessTarget (i: CARDINAL) ;
BEGIN
   IF NOT GetArg (Target, i)
   THEN
      fprintf0 (StdErr, 'cannot get target argument after -o\n') ;
      Close (StdErr) ;
      exit (1)
   END ;
   TargetFound := TRUE
END ProcessTarget ;


(*
   StripModuleExtension - returns a String without an extension from, s.
                          It only considers '.obj', '.o' and '.lo' as
                          extensions.
*)

PROCEDURE StripModuleExtension (s: String) : String ;
VAR
   t: String ;
BEGIN
   t := ExtractExtension (s, Mark (InitString ('.lo'))) ;
   IF s=t
   THEN
      t := ExtractExtension (s, Mark (InitString ('.obj'))) ;
      IF s=t
      THEN
         RETURN ExtractExtension (s, Mark(InitString('.o')))
      END
   END ;
   RETURN t
END StripModuleExtension ;


(*
   ProcessStartupFile - copies the specified startup file name into StartupFile.
*)

PROCEDURE ProcessStartupFile (i: CARDINAL) ;
BEGIN
   IF GetArg (StartupFile, i)
   THEN
      StartupFile := StripModuleExtension (StartupFile)
   ELSE
      fprintf0 (StdErr, 'cannot get startup argument after --startup\n') ;
      Close (StdErr) ;
      exit (1)
   END
END ProcessStartupFile ;


(*
   IsALibrary - returns TRUE if, a, is a library. If TRUE we add it to the
                Libraries string.
*)

PROCEDURE IsALibrary (s: String) : BOOLEAN ;
BEGIN
   IF EqualArray (Mark (Slice (s, 0, 2)), '-l')
   THEN
      LibrariesFound := TRUE ;
      Libraries := ConCat (ConCatChar (Libraries, ' '), s) ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END IsALibrary ;


(*
   IsALibraryPath -
*)

PROCEDURE IsALibraryPath (s: String) : BOOLEAN ;
BEGIN
   IF EqualArray (Mark (Slice (s, 0, 2)), '-L')
   THEN
      IF UseLibtool
      THEN
         LibrariesFound := TRUE ;
         Libraries := ConCat (ConCatChar (Libraries, ' '), s)
      END ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END IsALibraryPath ;


(*
   AddCommandLineObject - adds, s, to a list of objects specified on the command line.
*)

PROCEDURE AddCommandLineObject (s: String) ;
BEGIN
   s := Dup (s) ;
   IncludeIndiceIntoIndex (CmdLineObjects, s) ;
   IF RegisterModuleObject (CmdLine, s)
   THEN
      IF Debugging
      THEN
         fprintf1 (StdErr, "object registered first time: %s\n", s)
      END
   ELSE
      IF Debugging
      THEN
         fprintf1 (StdErr, "  object ignored: %s\n", s)
      END
   END
END AddCommandLineObject ;


(*
   IsAnObject - returns TRUE if, a, is a library.
*)

PROCEDURE IsAnObject (s: String) : BOOLEAN ;
BEGIN
   IF ((Length (s) > 2) AND EqualArray (Mark (Slice (s, -2, 0)), '.o')) OR
      ((Length (s) > 4) AND EqualArray (Mark (Slice (s, -4, 0)), '.obj'))
   THEN
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END IsAnObject ;


(*
   AdditionalFOptions - add an -f option to the compiler.
*)

PROCEDURE AdditionalFOptions (s: String) ;
BEGIN
   FOptions := ConCat (FOptions, Mark (s)) ;
   FOptions := ConCatChar (FOptions, ' ')
END AdditionalFOptions ;


(*
   DisplayHelp -
*)

PROCEDURE DisplayHelp ;
BEGIN
   fprintf0 (StdErr, 'Usage: gm2lcc [-c][-g][-h][--help][--main mainmodule]\n');
   fprintf0 (StdErr, '              [--mainobject objectname][-Bdirectory][-p][--exec][-fshared]\n');
   fprintf0 (StdErr, '              [--ignoremain][--ar][-fobject-path=path][-ftarget-ar=arname]\n');
   fprintf0 (StdErr, '              [-ftarget-ranlib=ranlibname][-o outputname][--startup filename]\n') ;
   fprintf0 (StdErr, '              [-foption][-llibname][-Lpath] filename\n');
   exit (0)
END DisplayHelp ;


(*
   ScanArguments - scans arguments for flags: -fobject-path= -g and -B
*)

PROCEDURE ScanArguments ;
VAR
   filename,
   s        : String ;
   i        : CARDINAL ;
   FoundFile: BOOLEAN ;
BEGIN
   FoundFile := FALSE ;
   filename := NIL ;
   i := 1 ;
   WHILE GetArg (s, i) DO
      IF EqualArray (s, '-h') OR EqualArray (s, '--help')
      THEN
         DisplayHelp
      ELSIF EqualArray (s, '-g')
      THEN
         DebugFound := TRUE
      ELSIF EqualArray (s, '-c')
      THEN
         CheckFound := TRUE
      ELSIF EqualArray (s, '--main')
      THEN
         INC (i) ;
         IF NOT GetArg (MainModule, i)
         THEN
            fprintf0 (StdErr, 'expecting modulename after the --main option\n') ;
            Close (StdErr) ;
            exit (1)
         END
      ELSIF EqualArray (s, '--mainobject')
      THEN
         INC (i) ;
         IF GetArg (MainObject, i)
         THEN
            (* do nothing.  *)
         ELSE
            fprintf0 (StdErr, 'expecting an object file after the --mainobject option\n') ;
            Close (StdErr) ;
            exit (1)
         END
      ELSIF EqualArray (Mark (Slice (s, 0, 2)), '-B')
      THEN
         CompilerDir := KillString (CompilerDir) ;
         IF Length (s) = 2
         THEN
            INC(i) ;
            IF NOT GetArg (CompilerDir, i)
            THEN
               fprintf0 (StdErr, 'expecting path after -B option\n') ;
               Close (StdErr) ;
               exit (1)
            END
         ELSE
            CompilerDir := Slice (s, 2, 0)
         END ;
         BOption := Dup (s)
      ELSIF EqualArray (s, '-p')
      THEN
         ProfileFound := TRUE
      ELSIF EqualArray (s, '-v')
      THEN
         VerboseFound := TRUE
      ELSIF EqualArray (s, '--exec')
      THEN
         ExecCommand := TRUE
      ELSIF EqualArray (s, '-fshared')
      THEN
         Shared := TRUE
      ELSIF EqualArray (s, '--ignoremain')
      THEN
         IgnoreMain := TRUE
      ELSIF EqualArray (s, '--ar')
      THEN
         UseAr := TRUE ;
         UseRanlib := TRUE ;
         UseLibtool := FALSE
      ELSIF EqualArray (Mark (Slice (s, 0, 14)), '-fobject-path=')
      THEN
         PrependSearchPath (Slice (s, 14, 0))
      ELSIF EqualArray (Mark (Slice (s, 0, 12)), '-ftarget-ar=')
      THEN
         ArProgram := KillString (ArProgram) ;
         ArProgram := Slice (s, 12, 0)
      ELSIF EqualArray (Mark (Slice (s, 0, 16)), '-ftarget-ranlib=')
      THEN
         RanlibProgram := KillString (RanlibProgram) ;
         RanlibProgram := Slice (s, 16, 0)
      ELSIF EqualArray (s, '-o')
      THEN
         INC (i) ;                 (* Target found *)
         ProcessTarget (i)
      ELSIF EqualArray (s, '--startup')
      THEN
         INC (i) ;                 (* Target found.  *)
         ProcessStartupFile (i)
      ELSIF EqualArray (Mark (Slice (s, 0, 2)), '-f')
      THEN
         AdditionalFOptions (s)
      ELSIF IsALibrary (s) OR IsALibraryPath (s)
      THEN
      ELSIF IsAnObject (s)
      THEN
         AddCommandLineObject (s)
      ELSE
         IF FoundFile
         THEN
            fprintf2 (StdErr, 'already specified input filename (%s), unknown option (%s)\n', filename, s) ;
            Close (StdErr) ;
            exit (1)
         ELSE
            (* must be input filename.  *)
            Close (StdIn) ;
            fi := OpenToRead (s) ;
            IF NOT IsNoError (fi)
            THEN
               fprintf1 (StdErr, 'failed to open %s\n', s) ;
               Close (StdErr) ;
               exit (1)
            END ;
            FoundFile := TRUE ;
            filename := Dup (s)
         END
      END ;
      INC (i)
   END
END ScanArguments ;


(*
   Init - initializes the global variables.
*)

PROCEDURE Init ;
BEGIN
   DebugFound    := FALSE ;
   CheckFound    := FALSE ;
   TargetFound   := FALSE ;
   ProfileFound  := FALSE ;
   IgnoreMain    := FALSE ;
   UseAr         := FALSE ;
   UseLibtool    := FALSE ;
   UseRanlib     := FALSE ;
   VerboseFound  := FALSE ;
   Shared        := FALSE ;
   ArProgram     := InitString ('ar') ;
   RanlibProgram := InitString ('ranlib') ;
   MainModule    := InitString ('') ;
   StartupFile   := InitString ('mod_init') ;
   fi            := StdIn ;
   fo            := StdOut ;
   ExecCommand   := FALSE ;

   CompilerDir   := InitString ('') ;

   FOptions      := InitString ('') ;
   Archives      := NIL ;
   Path          := NIL ;
   LibrariesFound:= FALSE ;
   Libraries     := InitString ('') ;
   Command       := NIL ;
   Target        := NIL ;
   BOption       := NIL ;
   objects       := InitFileObject () ;
   CmdLine       := InitFileObject () ;
   CmdLineObjects:= InitIndex (1) ;
   MainObject    := InitString ('') ;

   ScanArguments ;
   IF CheckFound
   THEN
      CheckCC
   ELSE
      GenCC
   END ;
   Close (fo)
END Init ;


BEGIN
   Init
END gm2lcc.
