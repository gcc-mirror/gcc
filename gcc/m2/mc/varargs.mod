(* varargs.mod provides a basic vararg facility for GNU Modula-2.

Copyright (C) 2015-2024 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE varargs ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM libc IMPORT memcpy ;
FROM SYSTEM IMPORT ADDRESS, TSIZE, ADR, BYTE ;


CONST
   MaxArg = 4 ;

TYPE
   vararg = POINTER TO RECORD
                          nArgs   : CARDINAL ;
			  i       : CARDINAL ;
			  contents: ADDRESS ;
			  size    : CARDINAL ;
			  arg     : ARRAY [0..MaxArg] OF argDesc ;
                       END ;

   argDesc = RECORD
                ptr: ADDRESS ;
                len: CARDINAL ;
             END ;

   ptrToByte = POINTER TO BYTE ;


(*
   arg - fills in, a, with the next argument.  The size of, a, must be an exact
         match with the original vararg parameter.
*)

PROCEDURE arg (v: vararg; VAR a: ARRAY OF BYTE) ;
VAR
   p: POINTER TO BYTE ;
   j: CARDINAL ;
BEGIN
   WITH v^ DO
      IF i=nArgs
      THEN
         HALT  (* too many calls to arg.  *)
      ELSE
         IF HIGH(a)+1=arg[i].len
         THEN
            p := arg[i].ptr ;
            j := 0 ;
            WHILE j<=HIGH (a) DO
               a[j] := p^ ;
               INC (p) ;
               INC (j)
            END
         ELSE
            HALT  (* parameter mismatch.  *)
         END ;
         INC (i)
      END
   END
END arg ;


(*
   nargs - returns the number of arguments wrapped in, v.
*)

PROCEDURE nargs (v: vararg) : CARDINAL ;
BEGIN
   RETURN v^.nArgs
END nargs ;


(*
   copy - returns a copy of, v.
*)

PROCEDURE copy (v: vararg) : vararg ;
VAR
   c     : vararg ;
   j,
   offset: CARDINAL ;
BEGIN
   NEW (c) ;
   WITH c^ DO
      i := v^.i ;
      nArgs := v^.nArgs ;
      size := v^.size ;
      ALLOCATE (contents, size) ;
      contents := memcpy (contents, v^.contents, size) ;
      FOR j := 0 TO nArgs DO
         offset := VAL (CARDINAL, VAL (ptrToByte, v^.contents) - VAL (ptrToByte, v^.arg[j].ptr)) ;
         arg[j].ptr := VAL (ptrToByte, contents) ;
         INC (arg[j].ptr, offset) ;
         arg[j].len := v^.arg[j].len ;
      END
   END ;
   RETURN c
END copy ;


(*
   replace - fills the next argument with, a.  The size of, a,
             must be an exact match with the original vararg
             parameter.
*)

PROCEDURE replace (v: vararg; VAR a: ARRAY OF BYTE) ;
VAR
   p: POINTER TO BYTE ;
   j: CARDINAL ;
BEGIN
   WITH v^ DO
      IF i=nArgs
      THEN
         HALT  (* too many calls to arg.  *)
      ELSE
         IF HIGH(a)+1=arg[i].len
         THEN
            p := arg[i].ptr ;
            j := 0 ;
            WHILE j<=HIGH (a) DO
               p^ := a[j] ;
               INC (p) ;
               INC (j)
            END
         ELSE
            HALT  (* parameter mismatch.  *)
         END
      END
   END
END replace ;


(*
   next - assigns the next arg to be collected as, i.
*)

PROCEDURE next (v: vararg; i: CARDINAL) ;
BEGIN
   v^.i := i
END next ;


(*
   end - destructor for vararg, v.
*)

PROCEDURE end (VAR v: vararg) ;
BEGIN
   IF v#NIL
   THEN
      DEALLOCATE (v^.contents, TSIZE (vararg)) ;
      DISPOSE (v)
   END
END end ;


(*
   start1 - wraps up argument, a, into a vararg.
*)

PROCEDURE start1 (a: ARRAY OF BYTE) : vararg ;
VAR
   v: vararg ;
BEGIN
   NEW (v) ;
   WITH v^ DO
      i := 0 ;
      nArgs := 1 ;
      size := HIGH (a) + 1;
      ALLOCATE (contents, size) ;
      contents := memcpy (contents, ADR (a), size) ;
      arg[0].ptr := contents ;
      arg[0].len := size
   END ;
   RETURN v
END start1 ;


(*
   start2 - wraps up arguments, a, b, into a vararg.
*)

PROCEDURE start2 (a, b: ARRAY OF BYTE) : vararg ;
VAR
   v: vararg ;
   p: POINTER TO BYTE ;
BEGIN
   NEW (v) ;
   WITH v^ DO
      i := 0 ;
      nArgs := 2 ;
      size := HIGH (a) + HIGH (b) + 2 ;
      ALLOCATE (contents, size) ;
      p := memcpy (contents, ADR (a), HIGH (a) + 1) ;
      arg[0].ptr := p ;
      arg[0].len := HIGH (a) + 1 ;
      INC (p, arg[0].len) ;
      p := memcpy (p, ADR (b), HIGH (b) + 1) ;
      arg[1].ptr := p ;
      arg[1].len := HIGH (b) + 1
   END ;
   RETURN v
END start2 ;


(*
   start3 - wraps up arguments, a, b, c, into a vararg.
*)

PROCEDURE start3 (a, b, c: ARRAY OF BYTE) : vararg ;
VAR
   v: vararg ;
   p: POINTER TO BYTE ;
BEGIN
   NEW (v) ;
   WITH v^ DO
      i := 0 ;
      nArgs := 3 ;
      size := HIGH (a) + HIGH (b) + HIGH (c) + 3 ;
      ALLOCATE (contents, size) ;
      p := memcpy (contents, ADR (a), HIGH (a) + 1) ;
      arg[0].ptr := p ;
      arg[0].len := HIGH (a) + 1 ;
      INC (p, arg[0].len) ;
      p := memcpy (p, ADR (b), HIGH (b) + 1) ;
      arg[1].ptr := p ;
      arg[1].len := HIGH (b) + 1 ;
      INC (p, arg[1].len) ;
      p := memcpy (p, ADR (c), HIGH (c) + 1) ;
      arg[2].ptr := p ;
      arg[2].len := HIGH (c) + 1
   END ;
   RETURN v
END start3 ;


(*
   start4 - wraps up arguments, a, b, c, d, into a vararg.
*)

PROCEDURE start4 (a, b, c, d: ARRAY OF BYTE) : vararg ;
VAR
   v: vararg ;
   p: POINTER TO BYTE ;
BEGIN
   NEW (v) ;
   WITH v^ DO
      i := 0 ;
      nArgs := 4 ;
      size := HIGH (a) + HIGH (b) + HIGH (c) + HIGH (d) + 4 ;
      ALLOCATE (contents, size) ;
      p := memcpy (contents, ADR (a), HIGH (a) + 1) ;
      arg[0].len := HIGH (a) + 1 ;
      INC (p, arg[0].len) ;
      p := memcpy (p, ADR (b), HIGH (b) + 1) ;
      arg[1].ptr := p ;
      arg[1].len := HIGH (b) + 1 ;
      INC (p, arg[1].len) ;
      p := memcpy (p, ADR (c), HIGH (c) + 1) ;
      arg[2].ptr := p ;
      arg[2].len := HIGH (c) + 1 ;
      INC (p, arg[2].len) ;
      p := memcpy (p, ADR (c), HIGH (c) + 1) ;
      arg[3].ptr := p ;
      arg[3].len := HIGH (c) + 1
   END ;
   RETURN v
END start4 ;


END varargs.
