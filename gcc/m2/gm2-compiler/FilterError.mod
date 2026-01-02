(* FilterError.mod implements a filter for token and symbol.

Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE FilterError ;

(* The purpose of this module is to be able to filter out multiple error
   reports referring to the same symbol and token.  This is achieved by
   maintaining a dictionary of symbols each pointing to a dictionary of
   tokens.  *)

FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Storage IMPORT DEALLOCATE, ALLOCATE ;
FROM BinDict IMPORT Node ;
FROM Assertion IMPORT Assert ;
FROM libc IMPORT printf ;

IMPORT BinDict ;

CONST
   Debugging = FALSE ;

TYPE
   Filter = POINTER TO RECORD
                          Sym2Dict: BinDict.Dictionary ;
                       END ;

   PtrToCardinal = POINTER TO CARDINAL ;
   PtrToBoolean = POINTER TO BOOLEAN ;


(*
   Init - return a new empty Filter.
*)

PROCEDURE Init () : Filter ;
VAR
   filter: Filter ;
BEGIN
   NEW (filter) ;
   WITH filter^ DO
      Sym2Dict := BinDict.Init (CompareCardinal, DeleteCardinal, DeleteTree) ;
   END ;
   RETURN filter
END Init ;


(*
   Kill - deletes the entire filter tree and all contents.
*)

PROCEDURE Kill (VAR filter: Filter) ;
BEGIN
   BinDict.Kill (filter^.Sym2Dict) ;
   DISPOSE (filter)
END Kill ;


(*
   CompareCardinal - return an INTEGER representing the comparison
                     between left and right.
                     0 if left == right, -1 if left < right,
                    +1 if left > right.
*)

PROCEDURE CompareCardinal (left, right: PtrToCardinal) : INTEGER ;
BEGIN
   IF left^ = right^
   THEN
      RETURN 0
   ELSIF left^ < right^
   THEN
      RETURN -1
   ELSE
      RETURN 1
   END
END CompareCardinal ;


(*
   DeleteCardinal - deallocate the cardinal key.
*)

PROCEDURE DeleteCardinal (card: PtrToCardinal) ;
BEGIN
   DISPOSE (card)
END DeleteCardinal ;


(*
   DeleteBoolean - deallocate the boolean value.
*)

PROCEDURE DeleteBoolean (boolean: PtrToBoolean) ;
BEGIN
   DISPOSE (boolean)
END DeleteBoolean ;


(*
   DeleteTree - delete tree and all its contents.
*)

PROCEDURE DeleteTree (ErrorTree: BinDict.Dictionary) ;
BEGIN
   BinDict.Kill (ErrorTree)
END DeleteTree ;


(*
   AddSymError - adds the pair sym token to the filter.
*)

PROCEDURE AddSymError (filter: Filter;
                       sym: CARDINAL; token: CARDINAL) ;
BEGIN
   IF NOT IsSymError (filter, sym, token)
   THEN
      AddNewEntry (filter, sym, token, TRUE)
   END
END AddSymError ;


(*
   AddNewEntry - adds a new value to the sym token pair.
*)

PROCEDURE AddNewEntry (filter: Filter; sym: CARDINAL;
                       token: CARDINAL; value: BOOLEAN) ;
VAR
   TokenTree : BinDict.Dictionary ;
   ptrToToken,
   ptrToCard : PtrToCardinal ;
   ptrToBool : PtrToBoolean ;
BEGIN
   TokenTree := BinDict.Get (filter^.Sym2Dict, ADR (sym)) ;
   IF TokenTree = NIL
   THEN
      TokenTree := BinDict.Init (CompareCardinal, DeleteCardinal, DeleteBoolean) ;
      NEW (ptrToCard) ;
      ptrToCard^ := sym ;
      BinDict.Insert (filter^.Sym2Dict, ptrToCard, TokenTree) ;
      Assert (BinDict.Get (filter^.Sym2Dict, ptrToCard) = TokenTree)
   END ;
   NEW (ptrToBool) ;
   ptrToBool^ := value ;
   NEW (ptrToToken) ;
   ptrToToken^ := token ;
   IF Debugging
   THEN
      printf ("adding sym %d: key = 0x%x, value = 0x%x  (%d, %d)\n",
              sym, ptrToToken, ptrToBool, ptrToToken^, ptrToBool^)
   END ;
   BinDict.Insert (TokenTree, ptrToToken, ptrToBool) ;
   Assert (BinDict.Get (TokenTree, ptrToToken) = ptrToBool) ;
   IF Debugging
   THEN
      BinDict.PostOrder (TokenTree, PrintNode)
   END
END AddNewEntry ;


(*
   PrintNode -
*)

PROCEDURE PrintNode (node: Node) ;
VAR
   ptrToCard : PtrToCardinal ;
   ptrToBool : PtrToBoolean ;
BEGIN
   ptrToCard := BinDict.Key (node) ;
   ptrToBool := BinDict.Value (node) ;
   printf ("key = 0x%x, value = 0x%x  (%d, %d)\n",
            ptrToCard, ptrToBool, ptrToCard^, ptrToBool^)
END PrintNode ;


(*
   IsSymError - return TRUE if the pair sym token have been
                entered in the filter.
*)

PROCEDURE IsSymError (filter: Filter; sym: CARDINAL; token: CARDINAL) : BOOLEAN ;
VAR
   ptb      : PtrToBoolean ;
   TokenTree: BinDict.Dictionary ;
BEGIN
   TokenTree := BinDict.Get (filter^.Sym2Dict, ADR (sym)) ;
   (* RETURN (TokenTree # NIL) ; *)
   IF TokenTree = NIL
   THEN
      (* No symbol registered, therefore FALSE.  *)
      RETURN FALSE
   END ;
   ptb := BinDict.Get (TokenTree, ADR (token)) ;
   IF ptb = NIL
   THEN
      (* The symbol was registered, but no entry for token, therefore FALSE.  *)
      RETURN FALSE
   END ;
   (* Found symbol and token so we return the result.  *)
   RETURN ptb^
END IsSymError ;


END FilterError.
