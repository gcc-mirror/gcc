(* M2Comp.mod continually calls the compiler for every source file.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Comp ;


FROM M2Pass IMPORT SetPassToPass0, SetPassToPass1, SetPassToPass2, SetPassToPassC,
                   SetPassToPass3, SetPassToNoPass, SetPassToPassHidden ;

FROM M2Reserved IMPORT toktype ;
FROM M2Search IMPORT FindSourceDefFile, FindSourceModFile ;
FROM M2Code IMPORT Code ;

FROM M2LexBuf IMPORT OpenSource, CloseSource, ResetForNewPass, currenttoken, GetToken,
                     ReInitialize, currentstring, GetTokenNo, BuiltinTokenNo,
                     UnknownTokenNo ;

FROM M2FileName IMPORT CalculateFileName ;
FROM M2Preprocess IMPORT PreprocessModule, MakeSaveTempsFileNameExt, OnExitDelete ;
FROM libc IMPORT exit ;

FROM M2Error IMPORT ErrorStringAt, ErrorStringAt2, ErrorStringsAt2,
                    WriteFormat0, FlushErrors, FlushWarnings, ResetErrorScope ;

FROM M2MetaError IMPORT MetaErrorString0, MetaErrorString1, MetaError0, MetaError1,
                        MetaString0 ;

FROM FormatStrings IMPORT Sprintf1 ;
FROM P0SymBuild IMPORT P0Init, P1Init ;
FROM M2Debug IMPORT Assert ;

IMPORT m2flex ;
IMPORT P0SyntaxCheck ;
IMPORT P1Build ;
IMPORT P2Build ;
IMPORT PCBuild ;
IMPORT P3Build ;
IMPORT PHBuild ;
IMPORT PCSymBuild ;
IMPORT DynamicStrings ;

FROM M2Batch IMPORT GetSource, GetModuleNo, GetDefinitionModuleFile, GetModuleFile,
                    AssociateModule, AssociateDefinition, MakeImplementationSource,
                    MakeProgramSource ;

FROM SymbolTable IMPORT GetSymName, IsDefImp, NulSym,
                        IsHiddenTypeDeclared, GetFirstUsed, GetMainModule, SetMainModule,
                        ResolveConstructorTypes, SanityCheckConstants, IsDefinitionForC,
                        IsBuiltinInModule, PutModLink, IsDefLink, IsModLink, PutLibName,
                        GetModuleDefImportStatementList, GetModuleModImportStatementList,
                        GetImportModule, IsImportStatement, IsImport,
                        GetImportStatementList ;

FROM M2Search IMPORT FindSourceDefFile ;

FROM FIO IMPORT File, StdErr, StdOut, Close, EOF, IsNoError, WriteLine,
                WriteChar, FlushOutErr ;

FROM SFIO IMPORT WriteS, OpenToRead, OpenToWrite, ReadS, WriteS ;
FROM NameKey IMPORT Name, GetKey, KeyToCharStar, makekey ;
FROM M2Printf IMPORT fprintf0, fprintf1 ;
FROM M2Quiet IMPORT qprintf0, qprintf1, qprintf2 ;

FROM M2Options IMPORT Verbose, GetM2Prefix, GetM, GetMM, GetDepTarget, GetMF, GetMP,
                      GetObj, PPonly, Statistics, Quiet, WholeProgram, GetMD, GetMMD,
                      ExtendedOpaque, GenModuleList ;

FROM PathName IMPORT DumpPathName ;
FROM Lists IMPORT List, NoOfItemsInList, GetItemFromList ;
FROM Indexing IMPORT Index, InitIndex, KillIndex, GetIndice, PutIndice, HighIndice ;

FROM DynamicStrings IMPORT String, InitString, KillString, InitStringCharStar,
                           Dup, Mark, EqualArray, string, Length, ConCat, ConCatChar,
                           InitStringChar, RIndex, Slice, Equal, RemoveWhitePrefix ;


CONST
   Debugging = FALSE ;

VAR
   ModuleType : (None, Definition, Implementation, Program) ;
   DepContent : Index ;
   DepOutput  : String ;


(*
   CompilingDefinitionModule - returns true if the current module being
                               compiled is a definition module.
*)

PROCEDURE CompilingDefinitionModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Definition )
END CompilingDefinitionModule ;


(*
   CompilingImplementationModule - returns true if the current module being
                                   compiled is an implementation module.
*)

PROCEDURE CompilingImplementationModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Implementation )
END CompilingImplementationModule ;


(*
   CompilingProgramModule - returns true if the current module being
                            compiled is a program module.
*)

PROCEDURE CompilingProgramModule() : BOOLEAN ;
BEGIN
   RETURN( ModuleType=Program )
END CompilingProgramModule ;


(*
   NeedToParseImplementation -
*)

PROCEDURE NeedToParseImplementation (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (IsDefImp(sym) AND IsHiddenTypeDeclared(sym) AND ExtendedOpaque) OR
          (IsDefImp(sym) AND IsBuiltinInModule(sym)) OR
          (WholeProgram AND (NOT IsDefinitionForC(sym)))
END NeedToParseImplementation ;


(*
   GenerateDefDependency - generate a single dependency for the definition module
                           providing that it can be found and is not blocked by -MM.
*)

PROCEDURE GenerateDefDependency (module: CARDINAL) ;
VAR
   stem,
   fullpath,
   named   : String ;
BEGIN
   stem := InitStringCharStar (KeyToCharStar (GetSymName (module))) ;
   named := NIL ;
   IF FindSourceDefFile (stem, fullpath, named)
   THEN
      IF EqualArray (named, '') OR (NOT GetMM ())
      THEN
         MergeDep (DepContent, fullpath)
      ELSE
         fullpath := KillString (fullpath)
      END
   END ;
   stem := KillString (stem) ;
   named := KillString (named)
END GenerateDefDependency ;


(*
   GenerateDependenciesFromImport - lookup the module associated with the import
                                    and call GenerateDefDependency.
*)

PROCEDURE GenerateDependenciesFromImport (import: CARDINAL) ;
VAR
   module  : CARDINAL ;
BEGIN
   Assert (IsImport (import)) ;
   module := GetImportModule (import) ;
   GenerateDefDependency (module)
END GenerateDependenciesFromImport ;


(*
   GenerateDependenciesFromList - iterative over the import lists and for
                                  each module issue a dependency.
*)

PROCEDURE GenerateDependenciesFromList (dep: List) ;
VAR
   importList: List ;
   import    : CARDINAL ;
   i, n, j, m: CARDINAL ;
BEGIN
   n := NoOfItemsInList (dep) ;
   i := 1 ;
   WHILE i <= n DO
      import := GetItemFromList (dep, i) ;
      IF IsImportStatement (import)
      THEN
         importList := GetImportStatementList (import) ;
         j := 1 ;
         m := NoOfItemsInList (importList) ;
         WHILE j <= m DO
            import := GetItemFromList (importList, j) ;
            GenerateDependenciesFromImport (import) ;
            INC (j)
         END
      ELSE
         GenerateDependenciesFromImport (import)
      END ;
      INC (i)
   END
END GenerateDependenciesFromList ;


(*
   GenerateDependencies - generate a list of dependencies for the main module where
                          the source code is found in sourcefile.
*)

PROCEDURE GenerateDependencies (sourcefile: String) ;
BEGIN
   IF IsDefImp (GetMainModule ())
   THEN
      GenerateDependenciesFromList (GetModuleDefImportStatementList (GetMainModule ())) ;
      GenerateDefDependency (GetMainModule ())
   END ;
   GenerateDependenciesFromList (GetModuleModImportStatementList (GetMainModule ())) ;
   WriteDepContents (DepOutput, DepContent)
END GenerateDependencies ;


(*
   Compile - compile file, s, using a 5 pass technique.
*)

PROCEDURE Compile (s: String) ;
BEGIN
   DoPass0 (s) ;
   FlushWarnings ; FlushErrors ;
   ResetForNewPass ; ResetErrorScope ;
   qprintf0('Pass 1: scopes, enumerated types, imports and exports\n') ;
   DoPass1 ;
   FlushWarnings ; FlushErrors ;
   IF GetM () OR GetMM ()
   THEN
      GenerateDependencies (s)
   END ;
   IF NOT PPonly
   THEN
      qprintf0('Pass 2: constants and types\n') ;
      ResetForNewPass ; ResetErrorScope ;
      DoPass2 ;
      FlushWarnings ; FlushErrors ;
      qprintf0('Pass C: aggregate constants\n') ;
      ResetForNewPass ; ResetErrorScope ;
      DoPassC ;
      FlushWarnings ; FlushErrors ;
      qprintf0('Pass 3: quadruple generation\n') ;
      ResetForNewPass ; ResetErrorScope ;
      DoPass3 ;
      FlushWarnings ; FlushErrors ;
      qprintf0('Pass 4: gcc tree generation\n') ;
      Code ;
      FlushWarnings ; FlushErrors
   END
END Compile ;


(*
   compile - compile the filename.
*)

PROCEDURE compile (filename: ADDRESS) ;
VAR
   f: String ;
BEGIN
   f := InitStringCharStar (filename) ;
   Compile (f) ;
   f := KillString (f)
END compile ;


(*
   ExamineHeader - examines up until the ';', '[' or eof and determines if the source file
                   is a program, implementation/definition module.
*)

PROCEDURE ExamineHeader (VAR name: ADDRESS; VAR isdefimp, module: BOOLEAN) ;
BEGIN
   (* Stop if we see one of eof ';' '['.  *)
   WHILE (currenttoken#eoftok) AND
         (currenttoken#semicolontok) AND (currenttoken#lsbratok) DO
      IF name = NIL
      THEN
         IF (currenttoken=implementationtok) OR (currenttoken=definitiontok)
         THEN
            isdefimp := TRUE ;
            GetToken
         END ;
         IF currenttoken=moduletok
         THEN
            module := TRUE ;
            GetToken ;
            IF currenttoken=identtok
            THEN
               name := currentstring
            END
         END ;
      END ;
      GetToken
   END ;
END ExamineHeader ;


(*
   ExamineCompilationUnit - opens the source file to obtain the module name and kind of module.
*)

PROCEDURE ExamineCompilationUnit () : CARDINAL ;
VAR
   Message : String ;
   name    : ADDRESS ;
   module,
   isdefimp: BOOLEAN ;
BEGIN
   name := NIL ;
   isdefimp := FALSE ;   (* default to program module *)
   module := FALSE ;  (* Seen module keyword?  *)
   ExamineHeader (name, isdefimp, module) ;
   IF name = NIL
   THEN
      IF module
      THEN
         Message := MetaString0 (InitString ('no {%kMODULE} keyword seen'))
      ELSE
         Message := MetaString0 (InitString ('no module ident seen'))
      END ;
      m2flex.M2Error (string (Message)) ;
      exit (1)
   ELSE
      (* The token used is will be overwritten when P0 is underway.
         At this point we are determining the module kind and the tokens
         read will be discarded (see ReInitialize below).  *)
      IF isdefimp
      THEN
         RETURN MakeImplementationSource (BuiltinTokenNo, makekey (name))
      ELSE
         RETURN MakeProgramSource (BuiltinTokenNo, makekey (name))
      END
   END
END ExamineCompilationUnit ;


(*
   PeepInto - peeps into source, s, and initializes a definition/implementation or
              program module accordingly.
*)

PROCEDURE PeepInto (s: String) ;
VAR
   mainModule: CARDINAL ;
BEGIN
   IF OpenSource (s)
   THEN
      mainModule := ExamineCompilationUnit () ;
      IF mainModule # NulSym
      THEN
         SetMainModule (mainModule)
      END ;
      CloseSource ;
      ReInitialize
   ELSE
      fprintf1 (StdErr, 'failed to open %s\n', s) ;
      exit (1)
   END
END PeepInto ;


(*
   qprintLibName - print the libname.
*)

PROCEDURE qprintLibName (LibName: String) ;
BEGIN
   IF (LibName # NIL) AND (NOT EqualArray (LibName, ''))
   THEN
      qprintf1 (' [%s]', LibName)
   END
END qprintLibName ;


(*
   CreateFileStem - create a stem using the template LibName_ModuleName.
*)

PROCEDURE CreateFileStem (SymName, LibName: String) : String ;
BEGIN
   IF Length (LibName) > 0
   THEN
      RETURN ConCat (Dup (LibName), ConCat (InitStringChar ('_'), SymName))
   ELSE
      RETURN SymName
   END
END CreateFileStem ;


(*
   Return basename of path.  CutExt determines whether the .extension
   should be removed.
*)

PROCEDURE BaseName (Path: String; CutExt: BOOLEAN) : String ;
VAR
   ext,
   basename: INTEGER ;
BEGIN
   basename := RIndex (Path, '/', 0) ;
   IF basename = -1
   THEN
      basename := 0
   ELSE
      basename := basename + 1
   END ;
   IF CutExt
   THEN
      ext := RIndex (Path, '.', 0) ;
      IF ext=-1
      THEN
         ext := 0
      END
   ELSE
      ext := 0
   END ;
   RETURN Slice (Path, basename, ext)
END BaseName ;


(*
   IsLibrary - return TRUE if line contains a library module.
*)

PROCEDURE IsLibrary (line: String) : BOOLEAN ;
VAR
   moduleName,
   libname, filename: String ;
   result           : BOOLEAN ;
BEGIN
   result := FALSE ;
   moduleName := BaseName (line, TRUE) ;
   filename := NIL ;
   libname := NIL ;
   IF FindSourceDefFile (moduleName, filename, libname)
   THEN
      moduleName := KillString (moduleName) ;
      IF Length (libname) > 0
      THEN
         moduleName := BaseName (line, FALSE) ;
         line := BaseName (line, FALSE) ;
         result := Equal (line, moduleName) ;
         line := KillString (line) ;
      END
   END ;
   libname := KillString (libname) ;
   filename := KillString (filename) ;
   moduleName := KillString (moduleName) ;
   RETURN result
END IsLibrary ;


(*
   IsUnique - return TRUE if line is unique in array content.
*)

PROCEDURE IsUnique (content: Index; line: String) : BOOLEAN ;
VAR
   high, i: CARDINAL ;
BEGIN
   high := HighIndice (content) ;
   i := 1 ;
   WHILE i <= high DO
      IF Equal (line, GetIndice (content, i))
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   RETURN TRUE
END IsUnique ;


(*
   Append - append line to array content.
*)

PROCEDURE Append (content: Index; line: String) ;
VAR
   high: CARDINAL ;
BEGIN
   high := HighIndice (content) ;
   PutIndice (content, high+1, line)
END Append ;


(*
   MergeDep - if line is unique in array content then append.
              Check to see (and ignore) if line is a library module and -MM
              is present.
*)

PROCEDURE MergeDep (content: Index; line: String) ;
BEGIN
   line := RemoveWhitePrefix (line) ;
   IF (NOT EqualArray (line, "\")) AND (Length (line) > 0)
   THEN
      (* Ignore if -MM and is a library module.  *)
      IF NOT (GetMM () AND IsLibrary (line))
      THEN
         IF IsUnique (content, line)
         THEN
            Append (content, line)
         END
      END
   END
END MergeDep ;


(*
   splitLine - split a line into words separated by spaces
               and call MergeDep on each word.
*)

PROCEDURE splitLine (content: Index; line: String) ;
VAR
   word : String ;
   space: INTEGER ;
BEGIN
   REPEAT
      line := RemoveWhitePrefix (line) ;
      space := DynamicStrings.Index (line, ' ', 0) ;
      IF space > 0
      THEN
         word := Slice (line, 0, space) ;
         word := RemoveWhitePrefix (word) ;
         IF Length (word) > 0
         THEN
            MergeDep (content, word)
         END ;
         line := Slice (line, space, 0) ;
      ELSIF space < 0
      THEN
         MergeDep (content, line)
      END
   UNTIL space <= 0
END splitLine ;


(*
   MergeDeps - foreach dependency in ChildDep do
                  add dependency to ChildDep if not already present.
               ignore all ChildDep if -MM and libname # "".
*)

PROCEDURE MergeDeps (content: Index; ChildDep, LibName: String) ;
VAR
   line: String ;
   in  : File ;
BEGIN
   IF (content # NIL) AND (NOT (GetMM () AND (Length (LibName) > 0)))
   THEN
      in := OpenToRead (ChildDep) ;
      IF IsNoError (in)
      THEN
         line := ReadS (in) ;  (* Skip over first line containing the module object.  *)
         WHILE NOT EOF (in) DO
            line := ReadS (in) ;
            splitLine (content, line)
         END
      END ;
      Close (in)
   END
END MergeDeps ;


(*
   GetRuleTarget - return the rule target which is derived from the -MT arg
                   or -o arg or filename.mod.
*)

PROCEDURE GetRuleTarget (filename: String) : String ;
BEGIN
   IF GetDepTarget () # NIL
   THEN
      RETURN InitStringCharStar (GetDepTarget ())
   ELSIF GetMF () # NIL
   THEN
      RETURN InitStringCharStar (GetMF ())
   ELSE
      RETURN ConCat (BaseName (filename, TRUE), InitString ('.o'))
   END
END GetRuleTarget ;


(*
   ReadDepContents - reads the contents of file dep into a dynamic array
                     and return the array.  The file will be split into words
                     and each word stored as an entry in the array.
*)

PROCEDURE ReadDepContents (filename, dep: String) : Index ;
VAR
   content: Index ;
   line   : String ;
   in     : File ;
BEGIN
   content := NIL ;
   IF GetM () OR GetMM ()
   THEN
      in := OpenToRead (dep) ;
      (* The file might not be created (if -MD or -MMD is used as these options
         operate without preprocessing) in which case we create an dynamic
         array with the source filename and target.  *)
      content := InitIndex (1) ;
      IF GetMD () OR GetMMD () OR (NOT IsNoError (in))
      THEN
         (* No preprocessing done therefore create first two lines using
            target and source.  *)
         PutIndice (content, 1, ConCatChar (GetRuleTarget (filename), ':')) ;
         PutIndice (content, 2, Dup (filename))
      ELSE
         (* Preprocessing (using cc1) has created one for us, so we read it.  *)
         WHILE NOT EOF (in) DO
            line := ReadS (in) ;
            splitLine (content, line)
         END
      END ;
      Close (in)
   END ;
   RETURN content
END ReadDepContents ;


(*
   WriteDep - write the dependencies and target to file out.
*)

PROCEDURE WriteDep (dep: String; contents: Index; out: File) ;
VAR
   i, h: CARDINAL ;
   line: String ;
BEGIN
   i := 1 ;
   h := HighIndice (contents) ;
   WHILE i <= h DO
      line := GetIndice (contents, i) ;
      line := RemoveWhitePrefix (line) ;
      IF Length (line) > 0
      THEN
         IF i = 1
         THEN
            (* First line is always the target.  *)
            IF GetDepTarget () # NIL
            THEN
               line := ConCatChar (InitStringCharStar (GetDepTarget ()), ':')
            END
         ELSIF i > 1
         THEN
            WriteChar (out, ' ')
         END ;
         line := WriteS (out, line) ;
         IF i < h
         THEN
            WriteChar (out, ' ') ;
            WriteChar (out, '\')
         END ;
         WriteLine (out)
      END ;
      INC (i)
   END
END WriteDep ;


(*
   WritePhonyDep - write the dependencies and target to file out.
*)

PROCEDURE WritePhonyDep (dep: String; contents: Index; out: File) ;
VAR
   i, h: CARDINAL ;
   line: String ;
BEGIN
   (* The first line is always the target and the second line is always
      the top level source file.  *)
   i := 3 ;
   h := HighIndice (contents) ;
   WHILE i <= h DO
      line := GetIndice (contents, i) ;
      line := RemoveWhitePrefix (line) ;
      IF Length (line) > 0
      THEN
         line := WriteS (out, line) ;
         WriteChar (out, ':') ;
         WriteLine (out)
      END ;
      INC (i)
   END
END WritePhonyDep ;


(*
   WriteDepContents - write the dynamic array to filename dep (or StdOut) if
                      the GetMF file is NIL.
*)

PROCEDURE WriteDepContents (dep: String; contents: Index) ;
VAR
   out: File ;
BEGIN
   IF (contents # NIL) AND (GetM () OR GetMM ())
   THEN
      IF GetMF () = NIL
      THEN
         out := StdOut ;
         dep := OnExitDelete (dep)
      ELSE
         out := OpenToWrite (dep)
      END ;
      IF IsNoError (out)
      THEN
         WriteDep (dep, contents, out) ;
         IF GetMP ()
         THEN
            WritePhonyDep (dep, contents, out)
         END
      END ;
      IF GetMF () = NIL
      THEN
         FlushOutErr
      ELSE
         Close (out) ;
      END ;
      contents := KillIndex (contents)
   END
END WriteDepContents ;


(*
   CreateDepFilename - return a dependency filename associated with filename or use GetMF.
*)

PROCEDURE CreateDepFilename (filename: String) : String ;
VAR
   depfile: String ;
BEGIN
   IF GetMF () = NIL
   THEN
      depfile := MakeSaveTempsFileNameExt (filename, InitString ('.d')) ;
      RETURN OnExitDelete (depfile)
   ELSE
      RETURN InitStringCharStar (GetMF ())
   END
END CreateDepFilename ;


(*
   Pass0CheckDef -
*)

PROCEDURE Pass0CheckDef (sym: CARDINAL) : BOOLEAN ;
VAR
   ChildDep,
   SymName,
   FileName,
   LibName : String ;
BEGIN
   LibName := NIL ;
   FileName := NIL ;
   SymName := InitStringCharStar (KeyToCharStar (GetSymName (sym))) ;
   IF IsDefImp (sym)
   THEN
      IF FindSourceDefFile (SymName, FileName, LibName)
      THEN
         ModuleType := Definition ;
         ChildDep := MakeSaveTempsFileNameExt (CreateFileStem (SymName, LibName), InitString ('.def.d')) ;
         IF OpenSource (AssociateDefinition (PreprocessModule (FileName, FALSE, TRUE,
                                                               ChildDep), sym))
         THEN
            IF NOT P0SyntaxCheck.CompilationUnit ()
            THEN
               WriteFormat0 ('compilation failed') ;
               CloseSource ;
               SymName := KillString (SymName) ;
               FileName := KillString (FileName) ;
               LibName := KillString (LibName) ;
               RETURN FALSE
            END ;
            qprintf2 ('   Module %-20s : %s', SymName, FileName) ;
            qprintLibName (LibName) ;
            PutLibName (sym, makekey (string (LibName))) ;
            IF IsDefinitionForC (sym)
            THEN
               qprintf0 (' (for C)')
            END ;
            IF IsDefLink (sym)
            THEN
               qprintf0 (' (linking)')
            END ;
            qprintf0 ('\n') ;
            CloseSource ;
            MergeDeps (DepContent, ChildDep, LibName)
         ELSE
            (* Unrecoverable error.  *)
            MetaErrorString1 (Sprintf1 (InitString ('file {%%1EUAF%s} containing module {%%1a} cannot be found'),
                                        FileName), sym)
         END
      ELSE
         (* Unrecoverable error.  *)
         MetaError1 ('the file containing the definition module {%1EMAa} cannot be found', sym)
      END ;
      ModuleType := Implementation
   ELSE
      ModuleType := Program
   END ;
   SymName := KillString (SymName) ;
   FileName := KillString (FileName) ;
   LibName := KillString (LibName) ;
   RETURN TRUE
END Pass0CheckDef ;


(*
   Pass0CheckMod -
*)

PROCEDURE Pass0CheckMod (sym: CARDINAL; PPSource: String) : BOOLEAN ;
VAR
   Main    : CARDINAL ;
   ChildDep,
   SymName,
   FileName,
   LibName : String ;
BEGIN
   SymName := InitStringCharStar (KeyToCharStar (GetSymName (sym))) ;
   FileName := NIL ;
   LibName := NIL ;
   Main := GetMainModule () ;
   IF (Main = sym) OR NeedToParseImplementation (sym)
   THEN
      (* Only need to read implementation module if hidden types are
         declared or it is the main module.  *)
      IF Main = sym
      THEN
         FileName := Dup (PPSource) ;
         LibName := InitStringCharStar (GetM2Prefix ()) ;
         PutLibName (sym, makekey (string (LibName)))
      ELSE
         IF FindSourceModFile (SymName, FileName, LibName)
         THEN
            ChildDep := MakeSaveTempsFileNameExt (CreateFileStem (SymName, LibName), InitString ('.mod.d')) ;
            FileName := PreprocessModule (FileName, FALSE, TRUE, ChildDep) ;
            PutLibName (sym, makekey (string (LibName))) ;
            MergeDeps (DepContent, ChildDep, LibName)
         ELSE
            qprintf1 ('   Module %-20s : implementation source file not found\n', SymName)
         END
      END ;

      IF FileName # NIL
      THEN
         IF OpenSource (AssociateModule (Dup (FileName), sym))
         THEN
            IF NOT P0SyntaxCheck.CompilationUnit ()
            THEN
               WriteFormat0 ('compilation failed') ;
               CloseSource ;
               SymName := KillString (SymName) ;
               FileName := KillString (FileName) ;
               LibName := KillString (LibName) ;
               RETURN FALSE
            END ;
            qprintf2 ('   Module %-20s : %s', SymName, FileName) ;
            qprintLibName (LibName) ;
            IF IsModLink (sym)
            THEN
               qprintf0 (' (linking)')
            END ;
            qprintf0 ('\n') ;
            CloseSource
         ELSE
            (* It is quite legitimate to implement a module in C (and pretend it was a M2
               implementation) providing that it is not the main program module and the
               definition module does not declare a hidden type when -fextended-opaque
               is used.  *)
            IF (NOT WholeProgram) OR (sym = Main) OR IsHiddenTypeDeclared (sym)
            THEN
               (* Unrecoverable error.  *)
               MetaErrorString1 (Sprintf1 (InitString ('file {%%1EUAF%s} containing module {%%1a} cannot be found'),
                                           FileName), sym) ;
            END
         END
      END
   ELSIF GenModuleList
   THEN
      IF IsDefImp (sym) AND (NOT IsDefinitionForC (sym))
      THEN
         (* The implementation module is only useful if -fgen-module-list= is
            used (to gather all dependencies).  Note that we do not insist
            upon finding the implementation module.  *)
         LibName := NIL ;
         IF FindSourceModFile (SymName, FileName, LibName)
         THEN
            PutLibName (sym, makekey (string (LibName))) ;
            qprintf2 ('   Module %-20s : %s' , SymName, FileName) ;
            qprintLibName (LibName) ;
            qprintf0 (' (linking)\n') ;
            ChildDep := MakeSaveTempsFileNameExt (CreateFileStem (SymName, LibName), InitString ('.mod.d')) ;
            IF OpenSource (AssociateModule (PreprocessModule (FileName, FALSE, TRUE, ChildDep), sym))
            THEN
               PutModLink (sym, TRUE) ;   (* This source is only used to determine link time info.  *)
               IF NOT P0SyntaxCheck.CompilationUnit ()
               THEN
                  WriteFormat0 ('compilation failed') ;
                  CloseSource ;
                  SymName := KillString (SymName) ;
                  FileName := KillString (FileName) ;
                  LibName := KillString (LibName) ;
                  RETURN FALSE
               END ;
               CloseSource ;
               MergeDeps (DepContent, ChildDep, LibName)
            END
         END
      END
   END ;
   SymName := KillString (SymName) ;
   FileName := KillString (FileName) ;
   LibName := KillString (LibName) ;
   RETURN TRUE
END Pass0CheckMod ;


(*
   DoPass0 -
*)

PROCEDURE DoPass0 (filename: String) ;
VAR
   sym       : CARDINAL ;
   i         : CARDINAL ;
   PPSource  : String ;
BEGIN
   P0Init ;
   SetPassToPass0 ;
   (* Maybe preprocess the main file.  *)
   DepOutput := CreateDepFilename (filename) ;
   PPSource := PreprocessModule (filename, TRUE, FALSE, DepOutput) ;
   DepContent := ReadDepContents (filename, DepOutput) ;
   PeepInto (PPSource) ;
   i := 1 ;
   sym := GetModuleNo (i) ;
   qprintf1 ('Compiling: %s\n', PPSource) ;
   IF Debugging
   THEN
      DumpPathName ('DoPass0')
   END ;
   IF Verbose
   THEN
      fprintf1 (StdOut, 'Compiling: %s\n', PPSource)
   END ;
   qprintf0 ('Pass 0: lexical analysis, parsing, modules and associated filenames\n') ;
   WHILE sym # NulSym DO
      IF NOT Pass0CheckDef (sym)
      THEN
         RETURN
      END ;
      IF NOT Pass0CheckMod (sym, PPSource)
      THEN
         RETURN
      END ;
      INC (i) ;
      sym := GetModuleNo (i)
   END ;
   SetPassToNoPass
END DoPass0 ;


(*
   DoPass1 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass1 ;
VAR
   name    : Name ;
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   FileName: String ;
BEGIN
   P1Init ;
   SetPassToPass1 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT P1Build.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            IF NOT P1Build.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass1 ;


(*
   DoPass2 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass2 ;
VAR
   name    : Name ;
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   FileName: String ;
BEGIN
   SetPassToPass2 ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT P2Build.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            IF NOT P2Build.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass2 ;


(*
   DoPassC - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPassC ;
VAR
   name    : Name ;
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   FileName: String ;
BEGIN
   SetPassToPassC ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT PCBuild.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF Debugging
         THEN
            name := GetSymName(Sym) ;
            qprintf1('   Module %a\n', name)
         END ;
         IF OpenSource(FileName)
         THEN
            IF NOT PCBuild.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   PCSymBuild.ResolveConstTypes ;
   ResolveConstructorTypes ;
   SanityCheckConstants ;
   SetPassToNoPass
END DoPassC ;


(*
   DoPass3 - parses the sources of all modules necessary to compile
             the required module, Main.
*)

PROCEDURE DoPass3 ;
VAR
   Main,
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   FileName: String ;
BEGIN
   SetPassToPass3 ;
   Main := GetMainModule() ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   WHILE Sym#NulSym DO
      FileName := GetDefinitionModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF OpenSource(FileName)
         THEN
            ModuleType := Definition ;
            IF NOT P3Build.CompilationUnit()
            THEN
               MetaError0('compilation failed') ;
               CloseSource ;
               RETURN
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      FileName := GetModuleFile(Sym) ;
      IF FileName#NIL
      THEN
         IF OpenSource(FileName)
         THEN
            IF (Main=Sym) OR WholeProgram
            THEN
               IF NOT P3Build.CompilationUnit()
               THEN
                  MetaError0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END
            ELSE
               (*
                  not the main module .mod therefore must be implementing
                  a hidden type - we dont want to generate any
                  StatementSequence quadrupes but we do want to build TYPEs
                  and ConstExpressions.
               *)
               SetPassToNoPass ;
               SetPassToPassHidden ;
               IF NOT PHBuild.CompilationUnit()
               THEN
                  MetaError0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END ;
               SetPassToNoPass ;
               SetPassToPass3
            END ;
            CloseSource
         ELSE
            fprintf1(StdErr, 'failed to open %s\n', FileName) ;
            exit(1)
         END
      END ;
      INC(i) ;
      Sym := GetModuleNo(i)
   END ;
   SetPassToNoPass
END DoPass3 ;


BEGIN
   ModuleType := None ;
   DepContent := NIL ;
   DepOutput := NIL
END M2Comp.
