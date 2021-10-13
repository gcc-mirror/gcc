(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE testcse43 ;


CONST
   MaxSyms  = 100 ;
   MaxAlias = 100 ;

TYPE
   AliasList = POINTER TO AList ;

   (*
      AList has two main components: List and Syms.

            List is contains the alias through its pairing of indices in
                 the To and From fields.

            Syms contains the actual symbols that the List indices reference
                 together with their mode.
   *)

   SymIndex  = [0..MaxSyms] ;

   Alias     = RECORD
                  From, To: SymIndex ;    (* index into syms, showing the alias        *)
               END ;

   Symbol    = RECORD
                  addr    : CARDINAL ;
                  Id      : CARDINAL ;    (* symbol value from SymbolTable.mod         *)
                  Dirty   : BOOLEAN ;     (* have we written to this symbol yet?       *)
                  Count   : CARDINAL ;    (* number of times used (read and write)     *)
               END ;

   AList     = RECORD
                  List    : ARRAY [1..MaxAlias] OF Alias ;
                  AliasPtr: CARDINAL ;    (* points to the top of the List array       *)
                  Syms    : ARRAY [1..MaxSyms] OF Symbol ;
                  SymPtr  : SymIndex ;    (* points to the top of the Syms array       *)
                  Next    : AliasList ;   (* used to store old lists on the free queue *)
               END ;


PROCEDURE New () : AliasList ;
BEGIN
   RETURN( NIL )
END New ;


PROCEDURE DuplicateAlias (a: AliasList) : AliasList ;
VAR
   b: AliasList ;
   i: CARDINAL ;
   j: CARDINAL ;
BEGIN
   b := New() ;
   (* it may well be faster simply to perform b^ := a^ ?? *)
   i := a^.AliasPtr ;
   b^.AliasPtr := i ;
   WHILE i>0 DO
      b^.List[i] := a^.List[i] ;
      DEC(i)
   END ;
   j := a^.SymPtr ;
   b^.SymPtr := j ;
   WHILE j>0 DO
      b^.Syms[j] := a^.Syms[j] ;
      DEC(j)
   END ;
   RETURN( b )
END DuplicateAlias ;


VAR
   a, b: AliasList ;
   j, i: CARDINAL ;
BEGIN
   b := DuplicateAlias(a)
END testcse43.
