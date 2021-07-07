(* M2AsmUtil.mod provides utilities relating symbols in the SymbolTable.

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

IMPLEMENTATION MODULE M2AsmUtil ;


FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM DynamicStrings IMPORT String, string, ConCat, KillString, InitString, Mark, InitStringCharStar, ConCatChar ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString ;
FROM NameKey IMPORT WriteKey, GetKey, MakeKey, makekey, KeyToCharStar ;
FROM M2Options IMPORT WholeProgram ;

FROM SymbolTable IMPORT NulSym,
                        GetSymName,
                        GetScope,
                        GetBaseModule,
                        IsInnerModule,
                        IsVar,
                        IsProcedure,
                        IsModule,
                        IsDefImp,
                        IsExportQualified,
                        IsExported,
                        IsDefinitionForC ;

FROM M2Error IMPORT InternalError ;
FROM M2Configure IMPORT UseUnderscoreForC, UseDotForJumpLabels ;


(*
   StringToKey - returns a Name, from a string and destroys the string.
*)

PROCEDURE StringToKey (s: String) : Name ;
VAR
   k: Name ;
BEGIN
   k := makekey(string(s)) ;
   s := KillString(s) ;
   RETURN( k )
END StringToKey ;


(*
   DotifyLabel - place a dot infront of the label if necessary.
                 The string, s, should no longer be used after
                 this function.
*)

PROCEDURE DotifyLabel (s: String) : String ;
BEGIN
   IF UseDotForJumpLabels
   THEN
      RETURN( ConCat(InitString('.'), Mark(s)) )
   ELSE
      RETURN( s )
   END
END DotifyLabel ;


(*
   GetFullScopeAsmName - returns the fully qualified name for the symbol.
                         This will take the format
                         [DefImpModule|Module]_{InnerModule}_{Procedure}_SymbolName
*)

PROCEDURE GetFullScopeAsmName (Sym: CARDINAL) : Name ;
VAR
   Module: String ;
   Scope : CARDINAL ;
BEGIN
   Scope := GetScope(Sym) ;
   IF UseUnderscoreForC
   THEN
      Module := InitString('_')
   ELSE
      Module := InitString('')
   END ;
   Module := ConCat(GetFullScopePrefix(Module, Scope, Sym),
                    InitStringCharStar(KeyToCharStar(GetSymName(Sym)))) ;
   RETURN( StringToKey(Module) )
END GetFullScopeAsmName ;


(*
   GetAsmName - returns the NameKey for the assembler string of a symbol.
*)

PROCEDURE GetAsmName (Sym: CARDINAL) : Name ;
VAR
   Module: String ;
   Scope : CARDINAL ;
BEGIN
   Scope := GetScope(Sym) ;
   IF UseUnderscoreForC
   THEN
      Module := InitString('_')
   ELSE
      Module := InitString('')
   END ;
   Module := ConCat(GetModulePrefix(Module, Sym, Scope),
                    InitStringCharStar(KeyToCharStar(GetSymName(Sym)))) ;
   RETURN( StringToKey(Module) )
END GetAsmName ;


(*
   GetFullSymName - returns the NameKey for the symbol name (which also
                    may contain the module name).
*)

PROCEDURE GetFullSymName (Sym: CARDINAL) : Name ;
VAR
   Module  : String ;
   Scope   : CARDINAL ;
BEGIN
   Scope := GetScope(Sym) ;
   Module := GetModulePrefix(InitString(''), Sym, Scope) ;
   RETURN( StringToKey(ConCat(Module, InitStringCharStar(KeyToCharStar(GetSymName(Sym))))) )
END GetFullSymName ;


(*
   WriteAsmName - displays the symbol, Sym, name using module prefixes
                  if it is EXPORT QUALIFIED.
*)

PROCEDURE WriteAsmName (Sym: CARDINAL) ;
VAR
   Scope: CARDINAL ;
BEGIN
   IF UseUnderscoreForC
   THEN
      Write('_')
   END ;
   Scope := GetScope(Sym) ;
   WriteModulePrefix(Sym, Scope) ;
   WriteKey(GetSymName(Sym))
END WriteAsmName ;


(*
   WriteName - displays the symbol, Sym, name using module prefixes
               if it is EXPORT QUALIFIED.
               This procedure differs from the above procedure because
               it does not generate any _ prefix.
*)

PROCEDURE WriteName (Sym: CARDINAL) ;
VAR
   Scope: CARDINAL ;
BEGIN
   IF IsVar(Sym) OR IsProcedure(Sym)
   THEN
      Scope := GetScope(Sym) ;
      WriteModulePrefix(Sym, Scope) ;
      WriteKey(GetSymName(Sym))
   ELSE
      InternalError ('Expecting Var or Procedure symbol')
   END
END WriteName ;


(*
   SymNeedsModulePrefix -
*)

PROCEDURE SymNeedsModulePrefix (sym, mod: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsDefImp(mod)
   THEN
      IF WholeProgram
      THEN
         IF NOT IsDefinitionForC(mod)
         THEN
            RETURN( TRUE )
         END
      ELSIF IsExportQualified(sym)
      THEN
         RETURN( TRUE )
      END
   ELSIF IsModule(mod)
   THEN
      RETURN( WholeProgram )
   END ;
   RETURN( FALSE )
END SymNeedsModulePrefix ;


(*
   GetModulePrefix - returns a String containing the module prefix
                     for module, ModSym, providing symbol, Sym, is exported.
                     Name is marked if it is appended onto the new string.
*)

PROCEDURE GetModulePrefix (Name: String; Sym, ModSym: CARDINAL) : String ;
BEGIN
   IF (ModSym#NulSym) AND (ModSym#GetBaseModule())
   THEN
      IF IsInnerModule(Sym) OR IsInnerModule(ModSym)
      THEN
         RETURN( ConCat(ConCatChar(InitStringCharStar(KeyToCharStar(GetSymName(ModSym))), '_'),
                        GetModulePrefix(Name, ModSym, GetScope(ModSym))) )
      ELSIF SymNeedsModulePrefix(Sym, ModSym)
      THEN
         RETURN( ConCatChar(ConCat(InitStringCharStar(KeyToCharStar(GetSymName(ModSym))), Mark(Name)), '_') )
      END
   END ;
   RETURN( Name )
END GetModulePrefix ;


(*
   GetFullScopePrefix - returns a String containing the full scope prefix
                        for symbol, Sym.  It honours IsExportQualified.
                        Name is marked if it is appended onto the new string.
*)

PROCEDURE GetFullScopePrefix (Name: String; Scope, Sym: CARDINAL) : String ;
BEGIN
   IF Sym#NulSym
   THEN
      IF IsInnerModule(Scope)
      THEN
         RETURN( ConCat(ConCatChar(InitStringCharStar(KeyToCharStar(GetSymName(Scope))), '_'),
                        GetFullScopePrefix(Name, GetScope(Scope), Sym)) )
      ELSIF IsDefImp(Scope) AND IsExportQualified(Sym)
      THEN
         RETURN( ConCatChar(ConCat(InitStringCharStar(KeyToCharStar(GetSymName(Scope))), Mark(Name)), '_') )
      ELSIF IsProcedure(Scope)
      THEN
         RETURN( ConCatChar(ConCat(InitStringCharStar(KeyToCharStar(GetSymName(Scope))), Mark(Name)), '_') )
      END
   END ;
   RETURN( Name )
END GetFullScopePrefix ;


(*
   WriteModulePrefix - writes the module prefix for module, ModSym,
                       providing symbol, Sym, is exported.
*)

PROCEDURE WriteModulePrefix (Sym, ModSym: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := GetModulePrefix(InitString(''), Sym, ModSym) ;
   s := KillString(WriteS(StdOut, s))
END WriteModulePrefix ;


(*
   UnderScoreString - emits a string with a leading underscore if the C compiler
                      uses _ prefixes. The string without the underscore is returned.
*)

PROCEDURE UnderScoreString (s: String) : String ;
BEGIN
   IF UseUnderscoreForC
   THEN
      Write('_')
   END ;
   RETURN( WriteS(StdOut, s) )
END UnderScoreString ;


(*
   GetModuleInitName - returns the name of the initialization section of a module.
*)

PROCEDURE GetModuleInitName (Sym: CARDINAL) : Name ;
VAR
   s: String ;
BEGIN
   s := ConCat(ConCat(InitString('_M2_'), Mark(GetModulePrefix(InitStringCharStar(KeyToCharStar(GetSymName(Sym))),
                                                               Sym, GetScope(Sym)))),
               Mark(InitString('_init'))) ;
   IF UseUnderscoreForC
   THEN
      s := ConCat(InitString('_'), Mark(s))
   END ;
   RETURN( StringToKey(s) )
END GetModuleInitName ;


(*
   GetModuleFinallyName - returns the name of the finalization section of a module.
*)

PROCEDURE GetModuleFinallyName (Sym: CARDINAL) : Name ;
VAR
   s: String ;
BEGIN
   s := ConCat(ConCat(InitString('_M2_'), Mark(GetModulePrefix(InitStringCharStar(KeyToCharStar(GetSymName(Sym))),
                                                               Sym, GetScope(Sym)))),
               Mark(InitString('_finish'))) ;
   IF UseUnderscoreForC
   THEN
      s := ConCat(InitString('_'), Mark(s))
   END ;
   RETURN( StringToKey(s) )
END GetModuleFinallyName ;


END M2AsmUtil.
