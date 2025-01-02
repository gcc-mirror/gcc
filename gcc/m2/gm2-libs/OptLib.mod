(* OptLib.mod allows users to manipulate Argv/Argc.

Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE OptLib ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM libc IMPORT memcpy ;

IMPORT DynamicStrings ;


TYPE
   Option = POINTER TO RECORD
                          argc: INTEGER ;
                          argv: ADDRESS ;
                          next: Option ;
                       END ;

VAR
   freeList: Option ;


(*
   InitOption - constructor for Option.
*)

PROCEDURE InitOption (argc: INTEGER; argv: ADDRESS) : Option ;
VAR
   o: Option ;
BEGIN
   o := newOption () ;
   o^.argc := argc ;
   o^.argv := argv ;
   o^.next := NIL ;
   RETURN o
END InitOption ;


(*
   newOption - returns an option
*)

PROCEDURE newOption () : Option ;
VAR
   o: Option ;
BEGIN
   IF freeList = NIL
   THEN
      NEW (o)
   ELSE
      o := freeList ;
      freeList := freeList^.next
   END ;
   RETURN o
END newOption ;


(*
   KillOption - deconstructor for Option.
*)

PROCEDURE KillOption (o: Option) : Option ;
BEGIN
   o^.next := freeList ;
   freeList := o ;
   RETURN NIL
END KillOption ;


(*
   Min - returns the lowest value of a and b.
*)

PROCEDURE Min (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END Min ;


(*
   dupArgv - return an array which is a duplicate as defined
             by argc and argv.
*)

PROCEDURE dupArgv (argc: INTEGER; argv: ADDRESS) : ADDRESS ;
VAR
   nargv: ADDRESS ;
BEGIN
   ALLOCATE (nargv, VAL (CARDINAL, argc) * SIZE (ADDRESS)) ;
   nargv := memcpy (nargv, argv, VAL (CARDINAL, argc) * SIZE (ADDRESS)) ;
   RETURN nargv
END dupArgv ;


(*
   Dup - duplicate the option array inside, o.
         Notice that this does not duplicate all the contents
         (strings) of argv.
         Shallow copy of the top level indices.
*)

PROCEDURE Dup (o: Option) : Option ;
VAR
   n: Option ;
BEGIN
   n := newOption () ;
   n^.argc := o^.argc ;
   n^.argv := dupArgv (o^.argc, o^.argv) ;
   n^.next := NIL ;
   RETURN n
END Dup ;


(*
   Slice - return a new option which has elements [low:high] from the
           options, o.
*)

PROCEDURE Slice (o: Option; low, high: INTEGER) : Option ;
VAR
   n: Option ;
   p: POINTER TO CHAR ;
   a: ADDRESS ;
BEGIN
   n := newOption () ;
   IF low < 0
   THEN
      low := o^.argc + low
   END ;
   IF high <= 0
   THEN
      high := o^.argc + high
   ELSE
      high := Min (o^.argc, high)
   END ;
   n^.argc := high-low+1 ;
   p := o^.argv ;
   INC (p, VAL (INTEGER, SIZE (ADDRESS)) * low) ;
   ALLOCATE (a, VAL (INTEGER, SIZE (ADDRESS)) * n^.argc) ;
   n^.argv := memcpy (a, p, VAL (INTEGER, SIZE (ADDRESS)) * n^.argc) ;
   n^.next := NIL ;
   RETURN n
END Slice ;


(*
   IndexStrCmp - returns the index in the argv array which matches
                 string, s.  -1 is returned if the string is not found.
*)

PROCEDURE IndexStrCmp (o: Option; s: String) : INTEGER ;
VAR
   i        : INTEGER ;
   p        : POINTER TO POINTER TO CHAR ;
   optString: String ;
BEGIN
   i := 0 ;
   p := o^.argv ;
   WHILE i < o^.argc DO
      optString := DynamicStrings.InitStringCharStar (p^) ;
      IF DynamicStrings.Equal (s, optString)
      THEN
         optString := DynamicStrings.KillString (optString) ;
         RETURN i
      END ;
      optString := DynamicStrings.KillString (optString) ;
      INC (p, SIZE (ADDRESS)) ;
      INC (i)
   END ;
   RETURN -1
END IndexStrCmp ;


(*
   IndexStrNCmp - returns the index in the argv array where the first
                  characters are matched by string, s.
                  -1 is returned if the string is not found.
*)

PROCEDURE IndexStrNCmp (o: Option; s: String) : INTEGER ;
VAR
   len      : CARDINAL ;
   i        : INTEGER ;
   p        : POINTER TO POINTER TO CHAR ;
   optString: String ;
BEGIN
   i := 0 ;
   p := o^.argv ;
   len := DynamicStrings.Length (s) ;
   WHILE i < o^.argc DO
      optString := DynamicStrings.InitStringCharStar (p^) ;
      IF DynamicStrings.Length (optString) >= len
      THEN
         optString := DynamicStrings.Slice (DynamicStrings.Mark (optString), 0, len) ;
         IF DynamicStrings.Equal (s, optString)
         THEN
            optString := DynamicStrings.KillString (optString) ;
            RETURN i
         END
      END ;
      optString := DynamicStrings.KillString (optString) ;
      INC (p, SIZE (ADDRESS)) ;
      INC (i)
   END ;
   RETURN -1
END IndexStrNCmp ;


(*
   ConCat - returns the concatenation of a and b.
*)

PROCEDURE ConCat (a, b: Option) : Option ;
VAR
   result: Option ;
BEGIN
   result := newOption () ;
   result^.argc := a^.argc + b^.argc ;
   ALLOCATE (result^.argv, result^.argc * VAL (INTEGER, SIZE (ADDRESS))) ;
   result^.argv := memcpy (result^.argv, a^.argv, a^.argc * VAL (INTEGER, SIZE (ADDRESS))) ;
   result^.argv := memcpy (result^.argv + VAL (ADDRESS, a^.argc * VAL (INTEGER, SIZE (ADDRESS))),
                           b^.argv, b^.argc * VAL (INTEGER, SIZE (ADDRESS))) ;
   result^.next := NIL ;
   RETURN result
END ConCat ;


(*
   GetArgv - return the argv component of option.
*)

PROCEDURE GetArgv (o: Option) : ADDRESS ;
BEGIN
   RETURN o^.argv
END GetArgv ;


(*
   GetArgc - return the argc component of option.
*)

PROCEDURE GetArgc (o: Option) : INTEGER ;
BEGIN
   RETURN o^.argc
END GetArgc ;


BEGIN
   freeList := NIL
END OptLib.
