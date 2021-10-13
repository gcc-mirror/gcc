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
MODULE testcse16 ;

CONST
   MaxScopes  =  20 ; (* Maximum number of scopes at any one time.           *)
   NulSym     =   0 ;

TYPE
   CallFrame = RECORD
                  Main  : CARDINAL ;  (* Main scope for insertions        *)
                  Search: CARDINAL ;  (* Search scope for symbol searches *)
                  Start : CARDINAL ;  (* ScopePtr value before StartScope *)
                                      (* was called.                      *)
               END ;

VAR
   ScopeCallFrame: ARRAY [1..MaxScopes] OF CallFrame ;
   ScopePtr      : CARDINAL ;


PROCEDURE foo (Sym: CARDINAL) ;
BEGIN
   INC(ScopePtr) ;
   WITH ScopeCallFrame[ScopePtr] DO
      Main := ScopeCallFrame[ScopePtr-1].Main ;
(*
      Start := ScopeCallFrame[ScopePtr-1].Start ;
      Search := Sym
*)
   END
END foo ;


PROCEDURE IsAlreadyDeclaredSym (Name: CARDINAL) : BOOLEAN ;
BEGIN
   WITH ScopeCallFrame[ScopePtr] DO
      RETURN( GetLocalSym(ScopeCallFrame[ScopePtr].Main, Name)#NulSym )
   END
END IsAlreadyDeclaredSym ;


PROCEDURE GetLocalSym (Sym: CARDINAL; Name: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( 0 )
END GetLocalSym ;


BEGIN
   ScopePtr := 1 ;
   WITH ScopeCallFrame[ScopePtr] DO
      Main := 1 ;
      Start := 1 ;
      Search := 1
   END ;
   foo(1000) ;
   IF ScopeCallFrame[ScopePtr].Main#ScopeCallFrame[ScopePtr-1].Main
   THEN
      HALT
   END ;
(*
   IF IsAlreadyDeclaredSym(1234)
   THEN
   END
*)
END testcse16.
