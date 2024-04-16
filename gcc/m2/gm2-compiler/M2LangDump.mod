(* M2LangDump.mod provides support routines for the -flang-dump.

Copyright (C) 2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE M2LangDump ;

FROM SYSTEM IMPORT ADDRESS ;

FROM DynamicStrings IMPORT String, Length, InitString, Mark, Slice, EqualArray,
                           InitStringCharStar, ConCatChar, ConCat, KillString,
                           Dup, string, char, Index, ReverseIndex, RIndex, Equal,
                           PushAllocation, PopAllocationExemption ;

FROM SymbolTable IMPORT NulSym,
                        GetSymName, GetLibName,
                        GetScope, GetModuleScope, GetMainModule, GetDeclaredMod,
                        GetLocalSym,
                        IsInnerModule,
                        IsVar,
                        IsProcedure,
                        IsModule, IsDefImp,
                        IsExportQualified, IsExportUnQualified,
                        IsExported, IsPublic, IsExtern, IsMonoName,
                        IsDefinitionForC ;

FROM M2Options IMPORT GetM2DumpFilter, GetDumpDir, GetDumpQuadFilename,
                      GetDumpDeclFilename, GetDumpGimpleFilename ;

FROM M2GCCDeclare IMPORT IncludeDumpSymbol ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1 ;
FROM NameKey IMPORT Name, GetKey, MakeKey, makekey, KeyToCharStar, NulName ;
FROM SymbolConversion IMPORT Gcc2Mod, Mod2Gcc ;
FROM M2AsmUtil IMPORT GetFullScopeAsmName ;
FROM M2LexBuf IMPORT FindFileNameFromToken ;
FROM M2Printf IMPORT fprintf0, fprintf1, printf0, printf1, printf2 ;
FROM M2Error IMPORT InternalError ;
FROM M2Batch IMPORT Get ;
FROM StrLib IMPORT StrLen ;
FROM libc IMPORT printf ;

IMPORT FIO, SFIO, DynamicStrings, StdIO ;


CONST
   Debugging = FALSE ;

VAR
   outputFile   : FIO.File ;
   declActive,
   quadActive,
   mustClose    : BOOLEAN ;
   NoOfQuadDumps,
   NoOfDeclDumps: CARDINAL ;


(*
   Assert - call InternalError is NOT value.
*)

PROCEDURE Assert (value: BOOLEAN) ;
BEGIN
   IF NOT value
   THEN
      InternalError ('assert failed in M2LangDump')
   END
END Assert ;


(*
   DumpWrite - writes a single ch to the dump output.
*)

PROCEDURE DumpWrite (ch: CHAR) ;
BEGIN
   FIO.WriteChar (outputFile, ch)
END DumpWrite ;


(*
   CloseDump - close the dump file and pop the default write procedure.
*)

PROCEDURE CloseDump ;
BEGIN
   IF mustClose
   THEN
      FIO.Close (outputFile) ;
      mustClose := FALSE
   ELSE
      FIO.FlushBuffer (outputFile)
   END ;
   StdIO.PopOutput ;
   outputFile := FIO.StdOut ;
END CloseDump ;


(*
   OpenDump - open filename as a dump file.  The filename '-' is treated as stdout.
              It pushes a write procedure to StdIO.
*)

PROCEDURE OpenDump (filename: String; no: CARDINAL) ;
BEGIN
   IF DynamicStrings.EqualArray (filename, '-')
   THEN
      mustClose := FALSE ;
      outputFile := FIO.StdOut
   ELSE
      filename := Sprintf1 (filename, no) ;
      outputFile := SFIO.OpenToWrite (filename) ;
      mustClose := FIO.IsNoError (outputFile)
   END ;
   filename := KillString (filename) ;
   StdIO.PushOutput (DumpWrite)
END OpenDump ;


(*
   CloseDumpDecl - close the dump output file.
*)

PROCEDURE CloseDumpDecl ;
BEGIN
   IF declActive
   THEN
      CloseDump ;
      declActive := FALSE
   END
END CloseDumpDecl ;


(*
   AddRuleTextDump -
*)

PROCEDURE AddRuleTextDump (rule: String) ;
BEGIN

END AddRuleTextDump ;


(*
   AddRuleScopeQualidentDump -
*)

PROCEDURE AddRuleScopeQualidentDump (rule: String; dot: INTEGER; modsym: CARDINAL) ;
VAR
   modstr,
   idstr : String ;
   start : INTEGER ;
   sym   : CARDINAL ;
BEGIN
   start := dot + 1 ;
   dot := Index (rule, '.', start) ;
   WHILE dot > 0 DO
      modstr := Slice (rule, start, dot) ;
      modsym := GetLocalSym (modsym, makekey (string (modstr))) ;
      IF (modsym # NulSym) AND IsModule (modsym)
      THEN
         start := dot + 1 ;
         dot := Index (rule, '.', start)
      ELSE
         modstr := KillString (modstr) ;
         RETURN
      END
   END ;
   idstr := Slice (rule, start, 0) ;
   sym := GetLocalSym (modsym, makekey (string (idstr))) ;
   IF sym # NulSym
   THEN
      IncludeDumpSymbol (sym)
   END
END AddRuleScopeQualidentDump ;


(*
   AddRuleScopeDump -
*)

PROCEDURE AddRuleScopeDump (rule: String) ;
VAR
   modsym: CARDINAL ;
   libstr,
   modstr: String ;
   start,
   dot   : INTEGER ;
BEGIN
   dot := Index (rule, '.', 0) ;
   Assert (dot # -1) ;
   libstr := NIL ;
   modstr := Slice (rule, 0, dot) ;
   modsym := Get (makekey (string (modstr))) ;
   IF modsym = NulSym
   THEN
      libstr := modstr ;
      start := dot + 1 ;
      dot := Index (rule, '.', start) ;
      IF dot > 0
      THEN
         modstr := Slice (rule, start, dot) ;
         modsym := Get (makekey (string (modstr))) ;
         IF (modsym # NulSym) AND (makekey (string (libstr)) = GetLibName (modsym))
         THEN
            AddRuleScopeQualidentDump (rule, dot, modsym)
         END
      END
   ELSE
      AddRuleScopeQualidentDump (rule, dot, modsym)
   END
END AddRuleScopeDump ;


(*
   AddRuleFilenameDump -
*)

PROCEDURE AddRuleFilenameDump (rule: String) ;
BEGIN

END AddRuleFilenameDump ;


(*
   AddRuleSymToDump -
*)

PROCEDURE AddRuleSymToDump (rule: String) ;
BEGIN
   IF Index (rule, ':', 0) # -1
   THEN
      (* Filename and scope rule.  *)
      AddRuleFilenameDump (rule)
   ELSIF Index (rule, '.', 0) # -1
   THEN
      (* Modula-2 scoping tests.  *)
      AddRuleScopeDump (rule)
   ELSE
      (* Text decl tests.  *)
      AddRuleTextDump (rule)
   END
END AddRuleSymToDump ;


(*
   AddFilterListToDumpWatch -
*)

PROCEDURE AddFilterListToDumpWatch (filter: ADDRESS) ;
VAR
   rule,
   full  : String ;
   start,
   i     : INTEGER ;
BEGIN
   full := InitStringCharStar (filter) ;
   start := 0 ;
   REPEAT
      i := Index (full, ',', start) ;
      IF i = -1
      THEN
         rule := Slice (full, start, 0)
      ELSE
         rule := Slice (full, start, i)
      END ;
      AddRuleSymToDump (rule) ;
      rule := KillString (rule) ;
      start := i+1 ;
   UNTIL i = -1 ;
   full := KillString (full) ;
END AddFilterListToDumpWatch ;


(*
   CreateDumpTitle - creates the underlined title.
*)

PROCEDURE CreateDumpTitle (title: ARRAY OF CHAR) ;
VAR
   len,
   text,
   i   : CARDINAL ;
   s   : String ;
BEGIN
   s := Sprintf0 (Mark (InitString (title))) ;
   s := KillString (SFIO.WriteS (GetDumpFile (), s)) ;
   len := StrLen (title) ;
   i := 0 ;
   text := 0 ;
   WHILE i < len DO
      IF title[i] = '\'
      THEN
         INC (i, 2)
      ELSE
         INC (i) ;
         INC (text)
      END
   END ;
   s := DynamicStrings.Mult (Mark (InitString ('=')), text) ;
   s := KillString (SFIO.WriteS (GetDumpFile (), s)) ;
   fprintf0 (GetDumpFile (), '\n');
END CreateDumpTitle ;


(*
   CreateDumpDecl - create the dump file for a decl dump.
*)

PROCEDURE CreateDumpDecl (title: ARRAY OF CHAR) ;
BEGIN
   IF GetM2DumpFilter () # NIL
   THEN
      Assert (NOT declActive) ;
      Assert (NOT quadActive) ;
      declActive := TRUE ;
      INC (NoOfDeclDumps) ;
      OpenDump (MakeDeclTemplate (), NoOfDeclDumps) ;
      CreateDumpTitle (title) ;
      AddFilterListToDumpWatch (GetM2DumpFilter ())
   END
END CreateDumpDecl ;


(*
   CloseDumpQuad - close the dump output file.
*)

PROCEDURE CloseDumpQuad ;
BEGIN
   CloseDump ;
   quadActive := FALSE
END CloseDumpQuad ;


(*
   CreateDumpQuad - create the dump file for a quad dump.
*)

PROCEDURE CreateDumpQuad (title: ARRAY OF CHAR) ;
BEGIN
   Assert (NOT declActive) ;
   Assert (NOT quadActive) ;
   quadActive := TRUE ;
   INC (NoOfQuadDumps) ;
   OpenDump (MakeQuadTemplate (), NoOfQuadDumps) ;
   CreateDumpTitle (title)
END CreateDumpQuad ;


(*
   GetDumpFile - return the dump output file.
*)

PROCEDURE GetDumpFile () : File ;
BEGIN
   RETURN outputFile
END GetDumpFile ;


(*
   IsDumpRequiredTree - return TRUE if the gcc tree should be dumped.
*)

PROCEDURE IsDumpRequiredTree (tree: Tree; default: BOOLEAN) : BOOLEAN ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := Gcc2Mod (tree) ;
   IF sym = NulSym
   THEN
      RETURN default
   ELSE
      RETURN IsDumpRequired (sym, default)
   END
END IsDumpRequiredTree ;


(*
   IsDumpRequired - return TRUE if symbol sym should be dumped
                    according to the rules of the filter.
                    No filter specified will always return default.
                    The filter is a comma separated list.  Each element
                    of the list can specify a symbol three ways.
                    Firstly by DECL name for example: m2pim_NumberIO_HexToStr
                    Secondly by qualified scope: [pathname.]NumberIO.HexToStr
                    Thirdly by filename and scope: NumberIO.mod:HexToStr
*)

PROCEDURE IsDumpRequired (sym: CARDINAL; default: BOOLEAN) : BOOLEAN ;
VAR
   filter: String ;
BEGIN
   filter := GetM2DumpFilter () ;
   IF filter = NIL
   THEN
      RETURN default
   ELSE
      RETURN Match (filter, sym)
   END
END IsDumpRequired ;


(*
   Match - return TRUE if sym matches any of the filter rules.
*)

PROCEDURE Match (filter: ADDRESS; sym: CARDINAL) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   rule,
   full  : String ;
   start,
   i     : INTEGER ;
BEGIN
   full := InitStringCharStar (filter) ;
   start := 0 ;
   REPEAT
      i := Index (full, ',', start) ;
      IF i = -1
      THEN
         rule := Slice (full, start, 0)
      ELSE
         rule := Slice (full, start, i)
      END ;
      result := MatchRule (rule, sym) ;
      rule := KillString (rule) ;
      IF result
      THEN
         full := KillString (full) ;
         RETURN TRUE
      END ;
      start := i+1 ;
   UNTIL i = -1 ;
   full := KillString (full) ;
   RETURN FALSE
END Match ;


(*
   MatchRule - return TRUE if rule matches sym.
*)

PROCEDURE MatchRule (rule: String; sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF Index (rule, ':', 0) # -1
   THEN
      (* Filename and scope qualification tests.  *)
      RETURN MatchRuleFilenameScope (rule, sym)
   ELSIF Index (rule, '.', 0) # -1
   THEN
      (* Modula-2 scoping tests.  *)
      RETURN MatchRuleScope (rule, sym)
   ELSE
      (* Text decl tests.  *)
      RETURN MatchRuleText (rule, sym)
   END
END MatchRule ;


(*
   MatchRuleFilenameScope - returns TRUE if rule contains filename.ext:qualident
                            and it matches sym.
*)

PROCEDURE MatchRuleFilenameScope (rule: String; sym: CARDINAL) : BOOLEAN ;
VAR
   rulefile,
   symfile,
   subrule : String ;
BEGIN
   rulefile := Slice (rule, 0, Index (rule, ':', 0)) ;
   (* Do not deallocate symfile.  *)
   symfile := FindFileNameFromToken (GetDeclaredMod (sym), 0) ;
   IF TextMatch (rulefile, symfile)
   THEN
      subrule := Slice (rule, Index (rule, ':', 0) + 1, 0) ;
      IF MatchRuleScope (subrule, sym)
      THEN
         subrule := KillString (subrule) ;
         RETURN TRUE
      END
   END ;
   rulefile := KillString (rulefile) ;
   RETURN FALSE
END MatchRuleFilenameScope ;


(*
   MatchRuleScope - returns TRUE if rule contains a [libname.]qualified.ident
                    and it matches sym.
*)

PROCEDURE MatchRuleScope (rule: String; sym: CARDINAL) : BOOLEAN ;
VAR
   i   : INTEGER ;
   name: Name ;
BEGIN
   IF Debugging
   THEN
      name := GetSymName (sym) ;
      printf2 ("MatchRuleScope (%s, %a)\n", rule, name)
   END ;
   (* Compare qualident right to left.  *)
   i := RIndex (rule, '.', 0) ;
   IF i = -1
   THEN
      (* No qualification, just the ident.  *)
      RETURN MatchRuleIdent (rule, sym)
   ELSE
      RETURN MatchRuleQualident (rule, Slice (rule, i+1, 0), i, sym)
   END
END MatchRuleScope ;


(*
   MatchRuleQualident - returns TRUE if rule matches qualified sym.
                        PostCondition:  subrule will be deallocated upon exit.
                                        TRUE is returned if rule matches qualified sym.
*)

PROCEDURE MatchRuleQualident (rule, subrule: String; i: INTEGER; sym: CARDINAL) : BOOLEAN ;
VAR
   scope: CARDINAL ;
BEGIN
   IF TextCompareName (subrule, GetSymName (sym))
   THEN
      IF NOT QualifiedScope (rule, sym, i, scope)
      THEN
         RETURN FALSE
      END ;
      IF OptionalLibname (rule, sym, i, scope)
      THEN
         RETURN TRUE
      END
   END ;
   subrule := KillString (subrule) ;
   IF Debugging
   THEN
      printf0 ("MatchRuleQualident FALSE\n")
   END ;
   RETURN FALSE
END MatchRuleQualident ;


(*
   QualifiedScope - PostCondition: true is returned is rule matches a qualified sym.
                                   i is -1 if no more qualifications or libname is found.
                                   scope will be the set to the last outer scope seen.
*)

PROCEDURE QualifiedScope (rule: String; sym: CARDINAL; VAR i: INTEGER; VAR scope: CARDINAL) : BOOLEAN ;
VAR
   subrule: String ;
   j      : INTEGER ;
   name   : Name ;
BEGIN
   IF Debugging
   THEN
      name := GetSymName (sym) ;
      printf2 ("seen ident name, QualifiedScope (rule = %s, %a)\n", rule, name)
   END ;
   scope := sym ;
   subrule := NIL ;
   REPEAT
      j := i ;
      scope := GetScope (scope) ;
      i := ReverseIndex (rule, '.', j - 1) ;
      IF Debugging
      THEN
         printf2 (" reverseindex (rule = %s, '.', j = %d)\n", rule, j);
         printf1 ("    returns i = %d\n", i)
      END ;
      IF scope # NulSym
      THEN
         subrule := KillString (subrule) ;
         subrule := Slice (rule, i + 1, j) ;
         IF Debugging
         THEN
            name := GetSymName (scope) ;
            printf2 ("QualifiedScope (subrule = %s, %a)\n", subrule, name)
         END ;
         IF NOT TextCompareName (subrule, GetSymName (scope))
         THEN
            subrule := KillString (subrule) ;
            IF Debugging
            THEN
               printf0 ("QualifiedScope FALSE\n")
            END ;
            RETURN FALSE
         END
      END
   UNTIL (i <= 0) OR IsDefImp (scope) OR IsModule (scope) ;
   subrule := KillString (subrule) ;
   RETURN TRUE
END QualifiedScope ;


(*
   OptionalLibname - returns TRUE if rule[0..dot] matches syms libname or
                     if there is no libname the scope is a module or defimp
                     symbol.
*)

PROCEDURE OptionalLibname (rule: String; sym: CARDINAL;
                           dot: INTEGER; scope: CARDINAL) : BOOLEAN ;
VAR
   subrule: String ;
BEGIN
   IF dot > 0
   THEN
      (* Check for optional libname.  *)
      subrule := Slice (rule, 0, dot) ;
      IF Debugging
      THEN
         printf2 ("checking for optional libname (subrule = %s, '.', dot = %d)\n",
                  rule, dot)
      END ;
      IF TextCompareName (subrule, GetLibName (GetModuleScope (sym)))
      THEN
         subrule := KillString (subrule) ;
         IF Debugging
         THEN
            printf0 ("OptionalLibname TRUE\n")
         END ;
         RETURN TRUE
      END ;
      subrule := KillString (subrule)
   ELSIF (scope # NulSym) AND (IsModule (scope) OR IsDefImp (scope))
   THEN
      IF Debugging
      THEN
         printf0 ("OptionalLibname TRUE\n")
      END ;
      RETURN TRUE
   END ;
   RETURN FALSE
END OptionalLibname ;


(*
   MatchRuleIdent - return TRUE if ident sym matches rule.
                    The ident must be in a module or defimp scope.
*)

PROCEDURE MatchRuleIdent (rule: String; sym: CARDINAL) : BOOLEAN ;
VAR
   scope: CARDINAL ;
BEGIN
   IF TextCompareName (rule, GetSymName (sym))
   THEN
      scope := GetScope (sym) ;
      RETURN IsModule (scope) OR IsDefImp (scope)
   END ;
   RETURN FALSE
END MatchRuleIdent ;


(*
   MatchRuleText - returns TRUE if rule matches sym.
*)

PROCEDURE MatchRuleText (rule: String; sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN TextCompareName (rule, GetFullScopeAsmName (sym))
END MatchRuleText ;


(*
   TextCompareName - return TRUE if rule matches name.
*)

PROCEDURE TextCompareName (rule: String; name: Name) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   text  : String ;
BEGIN
   text := InitStringCharStar (KeyToCharStar (name)) ;
   result := TextMatch (rule, text) ;
   text := KillString (text) ;
   RETURN result
END TextCompareName ;


(*
   TextMatch - returns TRUE if rule matches text.  Currently this
               is a simple string compare, but could be extended
               to implement regexp (seen in the rule).
*)

PROCEDURE TextMatch (rule, text: String) : BOOLEAN ;
BEGIN
   IF Debugging
   THEN
      printf2 ("TextMatch (%s, %s)\n", rule, text)
   END ;
   RETURN Equal (rule, text)
END TextMatch ;


(*
   CreateTemplate - create and return a template filename with extension.
                    If the user has specified "-" then "-" is returned otherwise
                    a template is formed from "dumpdir + filename + .%03dl.extension".
*)

PROCEDURE CreateTemplate (filename, extension: String) : String ;
BEGIN
   IF filename = NIL
   THEN
      (* User has not specified a file.  *)
      IF GetDumpDir () = NIL
      THEN
         filename := InitStringCharStar (KeyToCharStar (GetSymName (GetMainModule ())))
      ELSE
         filename := Dup (GetDumpDir ()) ;
         filename := ConCat (filename, Mark (InitStringCharStar (KeyToCharStar (GetSymName (GetMainModule ())))))
      END ;
      filename := ConCat (filename, Mark (InitString ('.mod')))
   ELSE
      (* We need to duplicate the filename to create a new string before ConCat
         is used later on.  *)
      filename := Dup (filename)
   END ;
   IF NOT EqualArray (filename, '-')
   THEN
      filename := ConCat (ConCat (filename, InitString ('.%03dl.')), extension)
   END ;
   RETURN filename
END CreateTemplate ;


(*
   MakeQuadTemplate - return a template for the quad dump file.
*)

PROCEDURE MakeQuadTemplate () : String ;
BEGIN
   RETURN CreateTemplate (GetDumpQuadFilename (), InitString ('quad'))
END MakeQuadTemplate ;


(*
   MakeDeclTemplate - return a template for the decl dump file.
*)

PROCEDURE MakeDeclTemplate () : String ;
BEGIN
   RETURN CreateTemplate (GetDumpDeclFilename (), InitString ('decl'))
END MakeDeclTemplate ;


(*
   MakeGimpleTemplate - return a template for the gimple dump file and assign
                        len to the max number of characters required to complete
                        a template (including a nul terminator).
*)

PROCEDURE MakeGimpleTemplate (VAR len: CARDINAL) : String ;
VAR
   filename: String ;
BEGIN
   filename := CreateTemplate (GetDumpGimpleFilename (), InitString ('gimple')) ;
   len := Length (filename) ;  (* This is a short cut based on '%03d' format
                                  specifier used above.  *)
   RETURN filename
END MakeGimpleTemplate ;


(*
   Init - initialize the module global variables.
*)

PROCEDURE Init ;
BEGIN
   NoOfQuadDumps := 0 ;
   NoOfDeclDumps := 0 ;
   declActive := FALSE ;
   quadActive := FALSE ;
   mustClose := FALSE ;
   outputFile := FIO.StdOut
END Init ;


BEGIN
   Init
END M2LangDump.
