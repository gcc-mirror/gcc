(* SymbolConversion.mod mapping between m2 symbols and gcc symbols.

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

IMPLEMENTATION MODULE SymbolConversion ;

FROM NameKey IMPORT Name ;

FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, InBounds,
                     DebugIndex, InitIndexTuned, HighIndice ;

FROM SymbolTable IMPORT IsConst, PopValue, IsValueSolved, GetSymName,
                        GetType, SkipType, NulSym ;

FROM M2Error IMPORT InternalError ;
FROM M2ALU IMPORT PushTypeOfTree ;
FROM m2block IMPORT GetErrorNode, RememberConstant ;
FROM gcctypes IMPORT tree ;
FROM M2Printf IMPORT printf1 ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADDRESS ;

CONST
   USEPOISON = TRUE ;
   GGCPOISON = 0A5A5A5A5H ;   (* poisoned memory contains this code *)

TYPE
   PtrToCardinal = POINTER TO CARDINAL ;

VAR
   mod2gcc       : Index ;
   PoisonedSymbol: ADDRESS ;


(*
   Mod2Gcc - given a modula-2 symbol, sym, return the gcc equivalent.
*)

PROCEDURE Mod2Gcc (sym: CARDINAL) : tree ;
VAR
   n : Name ;
   t : PtrToCardinal ;
   tr: tree ;
BEGIN
   IF USEPOISON
   THEN
      IF InBounds(mod2gcc, sym)
      THEN
         t := PtrToCardinal(GetIndice(mod2gcc, sym)) ;
         IF (t#NIL) AND (t^=GGCPOISON)
         THEN
            InternalError ('gcc symbol has been poisoned')
         END
      END
   END ;
   IF InBounds(mod2gcc, sym)
   THEN
      tr := tree(GetIndice(mod2gcc, sym)) ;
      IF tr=PoisonedSymbol
      THEN
         n := GetSymName(sym) ;
         (* not poisoned by the garbage collector, but by the gm2 front end *)
         printf1('the gm2 front end poisoned this symbol (%a)\n', n) ;
         InternalError ('attempting to use a gcc symbol which is no longer in scope')
      END ;
      RETURN( tr )
   ELSE
      RETURN( NIL )
   END
END Mod2Gcc ;


(*
   Gcc2Mod - given a gcc tree return the modula-2 symbol.
*)

PROCEDURE Gcc2Mod (tree: tree) : CARDINAL ;
VAR
   high, i: CARDINAL ;
BEGIN
   i := 1 ;
   high := HighIndice (mod2gcc) ;
   WHILE i <= high DO
      IF GetIndice (mod2gcc, i) = tree
      THEN
         RETURN i
      END ;
      INC (i)
   END ;
   RETURN NulSym
END Gcc2Mod ;


(*
   AddModGcc - adds the tuple [ sym, gcc ] into the database.
*)

PROCEDURE AddModGcc (sym: CARDINAL; gcc: tree) ;
VAR
   old: tree ;
   t  : PtrToCardinal ;
BEGIN
   IF gcc=GetErrorNode()
   THEN
      InternalError ('error node generated during symbol conversion')
   END ;

   IF USEPOISON
   THEN
      t := PtrToCardinal(gcc) ;
      IF (gcc#tree(NIL)) AND (t^=GGCPOISON)
      THEN
         InternalError ('gcc symbol has been poisoned')
      END
   END ;

   old := Mod2Gcc(sym) ;
   IF old=tree(NIL)
   THEN
      (* absent - add it *)
      PutIndice(mod2gcc, sym, gcc) ;
      IF GetIndice(mod2gcc, sym)#gcc
      THEN
         InternalError ('failed to add gcc <-> mod2 symbol')
      END ;
      gcc := RememberConstant(gcc)
   ELSIF old=gcc
   THEN
      (* do nothing, as it is already stored *)
   ELSIF old=GetErrorNode()
   THEN
      InternalError ('replacing a temporary symbol (currently unexpected)')
   ELSE
      InternalError ('should not be replacing a symbol')
   END ;

   IF IsConst(sym) AND (NOT IsValueSolved(sym))
   THEN
      PushTypeOfTree(sym, gcc) ;
      PopValue(sym)
   END
END AddModGcc ;


(*
   RemoveMod2Gcc - removes the gcc symbol from the lookup table.
*)

PROCEDURE RemoveMod2Gcc (sym: CARDINAL) ;
BEGIN
   PutIndice(mod2gcc, sym, NIL)
END RemoveMod2Gcc ;


(*
   GccKnowsAbout - returns TRUE if gcc knows about the symbol, sym.
*)

PROCEDURE GccKnowsAbout (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( InBounds(mod2gcc, sym) AND (GetIndice(mod2gcc, sym)#NIL) )
END GccKnowsAbout ;


(*
   AddTemporaryKnown - adds a temporary gcc symbol against the modula-2 sym.
*)

PROCEDURE AddTemporaryKnown (sym: CARDINAL) ;
BEGIN
   (* we add the error node against symbol, sym.  We expect it to be retacted later. *)
   PutIndice (mod2gcc, sym, GetErrorNode ())
END AddTemporaryKnown ;


(*
   RemoveTemporaryKnown - removes the temporary symbol.
*)

PROCEDURE RemoveTemporaryKnown (sym: CARDINAL) ;
BEGIN
   IF Mod2Gcc(sym)=GetErrorNode()
   THEN
      PutIndice(mod2gcc, sym, NIL)
   ELSE
      InternalError ('attempting to remove a symbol which is not present in the tree')
   END
END RemoveTemporaryKnown ;


(*
   Mod2GccWithoutGCCPoison - given a modula-2 symbol, sym, return
                             the gcc equivalent, it does not check to see
                             whether the gcc symbol has been poisoned.
*)

PROCEDURE Mod2GccWithoutGCCPoison (sym: CARDINAL) : tree ;
VAR
   n : Name ;
   tr: tree ;
BEGIN
   IF InBounds(mod2gcc, sym)
   THEN
      tr := tree(GetIndice(mod2gcc, sym)) ;
      IF tr=PoisonedSymbol
      THEN
         n := GetSymName(sym) ;
         (* not poisoned by the garbage collector, but by the gm2 front end.  *)
         printf1 ('the gm2 front end poisoned this symbol (%a)\n', n) ;
         InternalError ('attempting to use a gcc symbol which is no longer in scope')
      END ;
      RETURN tr
   ELSE
      RETURN NIL
   END
END Mod2GccWithoutGCCPoison ;


(*
   Poison - poisons a symbol.
*)

PROCEDURE Poison (sym: WORD) ;
VAR
   a: ADDRESS ;
BEGIN
   IF NOT IsConst(sym)
   THEN
      a := Mod2GccWithoutGCCPoison(sym) ;
      IF a#NIL
      THEN
         PutIndice(mod2gcc, sym, PoisonedSymbol)
      END
   END
END Poison ;


(*
   Init - create both binary trees.
*)

PROCEDURE Init ;
BEGIN
   mod2gcc := InitIndexTuned (1, 1024*1024 DIV 16, 16) ;
   ALLOCATE (PoisonedSymbol, 1)
END Init ;


BEGIN
   Init
END SymbolConversion.
