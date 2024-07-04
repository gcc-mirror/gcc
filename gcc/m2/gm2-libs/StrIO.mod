(* StrIO.mod provides simple string input output routines.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StrIO ;


FROM ASCII IMPORT cr, nul, lf, bel, del, bs, nak, etb, ff, eof ;
FROM StdIO IMPORT Read, Write ;
FROM libc IMPORT isatty ;


VAR
   IsATTY: BOOLEAN ;   (* Is default input from the keyboard? *)


(*
   WriteLn - writes a carriage return and a newline
             character.
*)

PROCEDURE WriteLn ;
BEGIN
   Echo(cr) ;
   Write(lf)
END WriteLn ;


(*
   ReadString - reads a sequence of characters into a string.
                Line editing accepts Del, Ctrl H, Ctrl W and
                Ctrl U.
*)

PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
   ch   : CHAR ;
BEGIN
   high := HIGH(a) ;
   n := 0 ;
   REPEAT
      Read(ch) ;
      IF (ch=del) OR (ch=bs)
      THEN
         IF n=0
         THEN
            Write(bel)
         ELSE
            Erase ;
            DEC(n)
         END
      ELSIF ch=nak    (* Ctrl U *)
      THEN
         WHILE n>0 DO
            Erase ;
            DEC(n)
         END
      ELSIF ch=etb    (* Ctrl W *)
      THEN
         IF n=0
         THEN
            Echo(bel)
         ELSIF AlphaNum(a[n-1])
         THEN
            REPEAT
               Erase ;
               DEC(n)
            UNTIL (n=0) OR (NOT AlphaNum(a[n-1]))
         ELSE
            Erase ;
            DEC(n)
         END
      ELSIF n<=high
      THEN
         IF (ch=cr) OR (ch=lf)
         THEN
            a[n] := nul ;
            INC(n)
         ELSIF ch=ff
         THEN
            a[0] := ch ;
            IF high>0
            THEN
               a[1] := nul
            END ;
            ch := cr
         ELSIF ch>=' '
         THEN
            Echo(ch) ;
            a[n] := ch ;
            INC(n)
         ELSIF ch=eof
         THEN
            a[n] := ch ;
            INC(n) ;
            ch := cr;
            IF n<=high
            THEN
               a[n] := nul
            END
         END
      ELSIF ch#cr
      THEN
         Echo(bel)
      END
   UNTIL (ch=cr) OR (ch=lf)
END ReadString ;


(*
   WriteString - writes a string to the default output.
*)

PROCEDURE WriteString (a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
BEGIN
   high := HIGH(a) ;
   n := 0 ;
   WHILE (n <= high) AND (a[n] # nul) DO
      Write(a[n]) ;
      INC(n)
   END
END WriteString ;


(*
   Erase - writes a backspace, space and backspace to remove the
           last character displayed.
*)

PROCEDURE Erase ;
BEGIN
   Echo(bs) ;
   Echo(' ') ;
   Echo(bs)
END Erase ;


(*
   Echo - echos the character, ch, onto the output channel if IsATTY
          is true.
*)

PROCEDURE Echo (ch: CHAR) ;
BEGIN
   IF IsATTY
   THEN
      Write(ch)
   END
END Echo ;


(*
   AlphaNum- returns true if character, ch, is an alphanumeric character.
*)

PROCEDURE AlphaNum (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ((ch>='a') AND (ch<='z')) OR
          ((ch>='A') AND (ch<='Z')) OR
          ((ch>='0') AND (ch<='9'))
END AlphaNum ;


BEGIN
(*   IsATTY := isatty() *)
   IsATTY := FALSE
END StrIO.
