(* Terminal.mod provides a Logitech 3.0 compatible and PIM [234] compatible.

Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Terminal ;

IMPORT Termbase ;
FROM ASCII IMPORT nul, cr, tab, lf ;


(*
   Read - reads a single character, ch, from the underlying Termbase
          module.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   Termbase.Read(ch)
END Read ;


(*
   ReadAgain - makes the last character readable again.
*)

PROCEDURE ReadAgain ;
BEGIN
END ReadAgain ;


(*
   KeyPressed - returns TRUE if a character can be read without blocking
                the caller.
*)

PROCEDURE KeyPressed () : BOOLEAN ;
BEGIN
   RETURN( Termbase.KeyPressed() )
END KeyPressed ;


(*
   Write - writes a single character to the Termbase module.
*)

PROCEDURE Write (ch: CHAR) ;
BEGIN
   Termbase.Write(ch)
END Write ;


(*
   ReadString - reads a sequence of characters.
                Tabs are expanded into 8 spaces and <cr> or <lf> terminates
                the string.
*)

PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;
VAR
   t, h, i: CARDINAL ;
BEGIN
   i := 0 ;
   h := HIGH(s) ;
   IF i<=h
   THEN
      REPEAT
         Read(s[i]) ;
         IF (s[i]=cr) OR (s[i]=lf)
         THEN
            s[i] := nul ;
            (* successful *)
            RETURN
         ELSIF s[i]=tab
         THEN
            t := 0 ;
            REPEAT
               s[i] := ' ' ;
               INC(i) ;
               IF i>h
               THEN
                  RETURN
               END ;
               INC(t)
            UNTIL t=8
         END ;
         INC(i)
      UNTIL i>h
   END
END ReadString ;


(*
   WriteString - writes out a string which is terminated by a <nul>
                 character or the end of string HIGH(s).
*)

PROCEDURE WriteString (s: ARRAY OF CHAR) ;
VAR
   i, h: CARDINAL ;
BEGIN
   h := HIGH(s) ;
   i := 0 ;
   WHILE (i<=h) AND (s[i]#nul) DO
      Write(s[i]) ;
      INC(i)
   END
END WriteString ;


(*
   WriteLn - writes a lf character.
*)

PROCEDURE WriteLn ;
BEGIN
   Write(lf)
END WriteLn ;


END Terminal.
