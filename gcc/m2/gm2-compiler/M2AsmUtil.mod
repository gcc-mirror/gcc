(* M2AsmUtil.mod provides utilities relating symbols in the SymbolTable.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
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
FROM DynamicStrings IMPORT String, string, ConCat, KillString, InitString, Mark, InitStringCharStar, ConCatChar, EqualArray ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString ;
FROM NameKey IMPORT WriteKey, GetKey, MakeKey, makekey, KeyToCharStar, NulName ;
FROM M2Options IMPORT WholeProgram ;
FROM M2Printf IMPORT printf1 ;

FROM SymbolTable IMPORT NulSym,
                        GetSymName, GetLibName,
                        GetScope,
                        GetBaseModule,
                        IsInnerModule,
                        IsVar,
                        IsProcedure,
                        IsModule,
                        IsDefImp,
                        IsExportQualified, IsExportUnQualified,
                        IsExported, IsPublic, IsExtern, IsMonoName,
                        IsDefinitionForC ;

FROM M2Error IMPORT InternalError ;
FROM m2configure IMPORT UseUnderscoreForC ;


CONST
   Debugging = FALSE ;


(*
   StringToKey - returns a Name, from a string and destroys the string.
*)

PROCEDURE StringToKey (s: String) : Name ;
VAR
   k: Name ;
BEGIN
   k := makekey (string (s)) ;
   s := KillString (s) ;
   RETURN k
END StringToKey ;


(*
   GetFullScopeAsmName - returns the fully qualified name for the symbol.
                         This will take the format
                         [DefImpModule|Module]_{InnerModule}_{Procedure}_SymbolName
*)

PROCEDURE GetFullScopeAsmName (sym: CARDINAL) : Name ;
VAR
   leader: String ;
   scope : CARDINAL ;
BEGIN
   scope := GetScope (sym) ;
   IF UseUnderscoreForC
   THEN
      leader := InitString ('_')
   ELSE
      leader := InitString ('')
   END ;
   IF IsProcedure (sym) AND IsMonoName (sym)
   THEN
      RETURN StringToKey (ConCat (leader, InitStringCharStar (KeyToCharStar (GetSymName (sym)))))

   ELSE
      RETURN StringToKey (ConCat (GetFullScopePrefix (leader, scope, sym),
                                  InitStringCharStar (KeyToCharStar (GetSymName (sym)))))
   END
END GetFullScopeAsmName ;


(*
   GetFullSymName - returns the NameKey for the symbol name (which also
                    may contain the module name).
*)

PROCEDURE GetFullSymName (sym: CARDINAL) : Name ;
VAR
   libname,
   fullsymname,
   module     : String ;
   scope      : CARDINAL ;
BEGIN
   IF IsProcedure (sym) AND IsMonoName (sym)
   THEN
      RETURN GetSymName (sym)
   ELSE
      scope := GetScope (sym) ;
      module := GetModulePrefix (InitString (''), sym, scope) ;
      fullsymname := ConCat (module, InitStringCharStar (KeyToCharStar (GetSymName (sym)))) ;
      IF (IsVar (sym) OR IsProcedure (sym)) AND IsExportQualified (sym)
      THEN
         WHILE NOT IsDefImp (scope) DO
            scope := GetScope (scope)
         END ;
         IF GetLibName (scope) # NulName
         THEN
            IF Debugging
            THEN
               printf1 ("before sym = %s  , ", fullsymname)
            END ;
            libname := InitStringCharStar (KeyToCharStar (GetLibName (scope))) ;
            IF NOT EqualArray (libname, '')
            THEN
               IF Debugging
               THEN
                  printf1 ("libname = %s  , ", libname)
               END ;
               fullsymname := ConCat (ConCatChar (libname, '_'), fullsymname) ;
            END ;
            IF Debugging
            THEN
               printf1 ("after sym = %s\n", fullsymname)
            END
         END
      END ;
      RETURN StringToKey (fullsymname)
   END
END GetFullSymName ;


(*
   SymNeedsModulePrefix - return TRUE if symbol mod is required to have a prefix.
*)

PROCEDURE SymNeedsModulePrefix (sym, mod: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsDefImp (mod)
   THEN
      IF IsExportUnQualified (sym)
      THEN
         RETURN FALSE
      ELSE
         (* We need to force the prefix if whole program is used otherwise
            local symbols from multipl modules might conflict.  *)
         RETURN WholeProgram OR IsExportQualified (sym)
      END
   ELSIF IsModule (mod)
   THEN
      RETURN WholeProgram
   END ;
   RETURN FALSE
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
      ELSIF SymNeedsModulePrefix (Sym, ModSym)
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


END M2AsmUtil.
