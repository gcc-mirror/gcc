(* M2Comp.mod continually calls the compiler for every source file.

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

IMPLEMENTATION MODULE M2Comp ;


FROM M2Options IMPORT Statistics, Quiet, WholeProgram, ExtendedOpaque ;

FROM M2Pass IMPORT SetPassToPass0, SetPassToPass1, SetPassToPass2, SetPassToPassC, SetPassToPass3,
                   SetPassToNoPass, SetPassToPassHidden ;

FROM M2Reserved IMPORT toktype ;
FROM M2Search IMPORT FindSourceDefFile, FindSourceModFile ;
FROM M2Code IMPORT Code ;
FROM M2LexBuf IMPORT OpenSource, CloseSource, ResetForNewPass, currenttoken, GetToken, ReInitialize, currentstring, GetTokenNo ;
FROM M2FileName IMPORT CalculateFileName ;
FROM M2Preprocess IMPORT PreprocessModule ;
FROM libc IMPORT exit ;

FROM M2Error IMPORT ErrorStringAt, ErrorStringAt2, ErrorStringsAt2, WriteFormat0, FlushErrors, FlushWarnings ;
FROM M2MetaError IMPORT MetaErrorString1, MetaError0, MetaError1 ;
FROM FormatStrings IMPORT Sprintf1 ;
FROM P0SymBuild IMPORT P0Init, P1Init ;

IMPORT m2flex ;
IMPORT P0SyntaxCheck ;
IMPORT P1Build ;
IMPORT P2Build ;
IMPORT PCBuild ;
IMPORT P3Build ;
IMPORT PHBuild ;
IMPORT PCSymBuild ;

FROM M2Batch IMPORT GetSource, GetModuleNo, GetDefinitionModuleFile, GetModuleFile,
                    AssociateModule, AssociateDefinition, MakeImplementationSource,
                    MakeProgramSource ;

FROM SymbolTable IMPORT GetSymName, IsDefImp, NulSym,
                        IsHiddenTypeDeclared, GetFirstUsed, GetMainModule, SetMainModule,
                        ResolveConstructorTypes, SanityCheckConstants, IsDefinitionForC,
                        IsBuiltinInModule ;

FROM FIO IMPORT StdErr ;
FROM NameKey IMPORT Name, GetKey, KeyToCharStar, makekey ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Quiet IMPORT qprintf0, qprintf1, qprintf2 ;
FROM DynamicStrings IMPORT String, InitString, KillString, InitStringCharStar, Dup, Mark, string ;

CONST
   Debugging = FALSE ;

VAR
   ModuleType : (None, Definition, Implementation, Program) ;


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
   Compile - compile file, s, using a 6 pass technique.
*)

PROCEDURE Compile (s: String) ;
BEGIN
   DoPass0(s) ;
   FlushWarnings ; FlushErrors ;
   ResetForNewPass ;
   qprintf0('Pass 1: scopes, enumerated types, imports and exports\n') ;
   DoPass1 ;
   FlushWarnings ; FlushErrors ;
   qprintf0('Pass 2: constants and types\n') ;
   ResetForNewPass ;
   DoPass2 ;
   FlushWarnings ; FlushErrors ;
   qprintf0('Pass C: aggregate constants\n') ;
   ResetForNewPass ;
   DoPassC ;
   FlushWarnings ; FlushErrors ;
   qprintf0('Pass 3: quadruple generation\n') ;
   ResetForNewPass ;
   DoPass3 ;
   FlushWarnings ; FlushErrors ;
   qprintf0('Pass 4: gcc tree generation\n') ;
   Code ;
   FlushWarnings ; FlushErrors
END Compile ;


(*
   compile - compile the filename.
*)

PROCEDURE compile (filename: ADDRESS) ;
VAR
   f: String ;
BEGIN
   f := InitStringCharStar(filename) ;
   Compile(f) ;
   f := KillString(f) ;
END compile ;


(*
   ExamineCompilationUnit - opens the source file to obtain the module name and kind of module.
*)

PROCEDURE ExamineCompilationUnit (VAR name: ADDRESS; VAR isdefimp: BOOLEAN) ;
BEGIN
   isdefimp := FALSE ;   (* default to program module *)
   (* stop if we see eof, ';' or '[' *)
   WHILE (currenttoken#eoftok) AND (currenttoken#semicolontok) AND (currenttoken#lsbratok) DO
      IF (currenttoken=implementationtok) OR (currenttoken=definitiontok)
      THEN
         isdefimp := TRUE ;
         GetToken
      END ;
      IF currenttoken=identtok
      THEN
         name := currentstring ;
         RETURN
      END ;
      GetToken
   END ;
   m2flex.M2Error(string(InitString('failed to find module name'))) ;
   exit(1)
END ExamineCompilationUnit ;


(*
   PeepInto - peeps into source, s, and initializes a definition/implementation or
              program module accordingly.
*)

PROCEDURE PeepInto (s: String) ;
VAR
   name    : ADDRESS ;
   isdefimp: BOOLEAN ;
BEGIN
   IF OpenSource(PreprocessModule(s))
   THEN
      ExamineCompilationUnit(name, isdefimp) ;
      IF isdefimp
      THEN
         SetMainModule(MakeImplementationSource(GetTokenNo(), makekey(name)))
      ELSE
         SetMainModule(MakeProgramSource(GetTokenNo(), makekey(name)))
      END ;
      CloseSource ;
      ReInitialize
   ELSE
      fprintf1(StdErr, 'failed to open %s\n', s) ;
      exit(1)
   END
END PeepInto ;


(*
   DoPass0 -
*)

PROCEDURE DoPass0 (s: String) ;
VAR
   Main,
   Sym     : CARDINAL ;
   i       : CARDINAL ;
   SymName,
   FileName: String ;
BEGIN
   P0Init ;
   SetPassToPass0 ;
   PeepInto(s) ;
   Main := GetMainModule() ;
   i := 1 ;
   Sym := GetModuleNo(i) ;
   qprintf1('Compiling: %s\n', s) ;
   qprintf0('Pass 0: lexical analysis, parsing, modules and associated filenames\n') ;
   WHILE Sym#NulSym DO
      SymName := InitStringCharStar(KeyToCharStar(GetSymName(Sym))) ;
      IF IsDefImp(Sym)
      THEN
         IF FindSourceDefFile(SymName, FileName)
         THEN
            qprintf2('   Module %-20s : %s\n', SymName, FileName) ;
            ModuleType := Definition ;
            IF OpenSource(AssociateDefinition(PreprocessModule(FileName), Sym))
            THEN
               IF NOT P0SyntaxCheck.CompilationUnit()
               THEN
                  WriteFormat0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END ;
               CloseSource
            ELSE
               MetaErrorString1 (Sprintf1 (InitString ('file {%%1EUF%s} containing module {%%1a} cannot be found'), FileName), Sym) ;
               FlushWarnings ; FlushErrors ;
               fprintf1(StdErr, 'failed to open %s\n', FileName) ;
               exit(1)
            END
         ELSE
            MetaError1 ('the file containing the definition module {%1EUa} cannot be found', Sym) ;
            FlushWarnings ; FlushErrors ;
            fprintf1(StdErr, 'failed to find definition module %s.def\n', SymName) ;
            exit(1)
         END ;
         ModuleType := Implementation
      ELSE
         ModuleType := Program
      END ;
      IF (Main=Sym) OR NeedToParseImplementation(Sym)
      THEN
         (* only need to read implementation module if hidden types are declared or it is the main module *)
         IF Main=Sym
         THEN
            FileName := Dup(s)
         ELSE
            IF FindSourceModFile(SymName, FileName)
            THEN
            END
         END ;
         IF FileName#NIL
         THEN
            qprintf2('   Module %-20s : %s\n', SymName, FileName) ;
            IF OpenSource(AssociateModule(PreprocessModule(FileName), Sym))
            THEN
               IF NOT P0SyntaxCheck.CompilationUnit()
               THEN
                  WriteFormat0('compilation failed') ;
                  CloseSource ;
                  RETURN
               END ;
               CloseSource
            ELSE
               (* quite legitimate to implement a module in C (and pretend it was a M2 implementation
                  providing that it is not the main program module and the definition module does not
                 imply that the implementation defines hidden types.  *)
               IF (NOT WholeProgram) OR (Sym=Main) OR IsHiddenTypeDeclared(Sym)
               THEN
                  MetaErrorString1 (Sprintf1 (InitString ('file {%%1EUF%s} containing module {%%1a} cannot be found'), FileName), Sym) ;
                  FlushWarnings ; FlushErrors ;
                  fprintf1(StdErr, 'file %s cannot be opened\n', FileName)
               END
            END
         END
      END ;
      SymName := KillString(SymName) ;
      FileName := KillString(FileName) ;
      INC(i) ;
      Sym := GetModuleNo(i)
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
   ModuleType := None
END M2Comp.
