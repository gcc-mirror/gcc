(* CardinalIO.mod provides a PIM and Logitech compatible module.

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

IMPLEMENTATION MODULE CardinalIO ;


FROM DynamicStrings IMPORT String, InitString, KillString, RemoveWhitePrefix ;
FROM SYSTEM IMPORT ADR, BYTE ;
IMPORT InOut ;

FROM StringConvert IMPORT StringToCardinal, CardinalToString,
                          StringToLongCardinal, LongCardinalToString,
                          StringToShortCardinal, ShortCardinalToString ;


(*
   ReadCardinal - read an unsigned decimal number from the terminal.
                  The read continues until a space, newline, esc or
                  end of file is reached.
*)

PROCEDURE ReadCardinal (VAR c: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToCardinal(s, 10, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadCardinal ;


(*
   WriteCardinal - writes the value, c, to the terminal and ensures
                   that at least, n, characters are written. The number
                   will be padded out by preceeding spaces if necessary.
*)

PROCEDURE WriteCardinal (c: CARDINAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(CardinalToString(c, n, ' ', 10, FALSE))) ;
   Done := TRUE
END WriteCardinal ;


(*
   ReadHex - reads in an unsigned hexadecimal number from the terminal.
             The read continues until a space, newline, esc or
             end of file is reached.
*)

PROCEDURE ReadHex (VAR c: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToCardinal(s, 16, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadHex ;


(*
   WriteHex - writes out a CARDINAL, c, in hexadecimal format padding
              with, n, characters (leading with '0')
*)

PROCEDURE WriteHex (c: CARDINAL; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(CardinalToString(c, n, '0', 16, TRUE))) ;
   Done := TRUE
END WriteHex ;


(*
   ReadLongCardinal - read an unsigned decimal number from the terminal.
                      The read continues until a space, newline, esc or
                      end of file is reached.
*)

PROCEDURE ReadLongCardinal (VAR c: LONGCARD) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToLongCardinal(s, 10, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadLongCardinal ;


(*
   WriteLongCardinal - writes the value, c, to the terminal and ensures
                       that at least, n, characters are written. The number
                       will be padded out by preceeding spaces if necessary.
*)

PROCEDURE WriteLongCardinal (c: LONGCARD; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(LongCardinalToString(c, n, ' ', 10, FALSE))) ;
   Done := TRUE
END WriteLongCardinal ;


(*
   ReadLongHex - reads in an unsigned hexadecimal number from the terminal.
                 The read continues until a space, newline, esc or
                 end of file is reached.
*)

PROCEDURE ReadLongHex (VAR c: LONGCARD) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToLongCardinal(s, 16, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadLongHex ;


(*
   WriteLongHex - writes out a LONGCARD, c, in hexadecimal format padding
                  with, n, characters (leading with '0')
*)

PROCEDURE WriteLongHex (c: LONGCARD; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(LongCardinalToString(c, n, '0', 16, TRUE))) ;
   Done := TRUE
END WriteLongHex ;


(*
   ReadShortCardinal - read an unsigned decimal number from the terminal.
                       The read continues until a space, newline, esc or
                       end of file is reached.
*)

PROCEDURE ReadShortCardinal (VAR c: SHORTCARD) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToShortCardinal(s, 10, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadShortCardinal ;


(*
   WriteShortCardinal - writes the value, c, to the terminal and ensures
                        that at least, n, characters are written. The number
                        will be padded out by preceeding spaces if necessary.
*)

PROCEDURE WriteShortCardinal (c: SHORTCARD; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(ShortCardinalToString(c, n, ' ', 10, FALSE))) ;
   Done := TRUE
END WriteShortCardinal ;


(*
   ReadShortHex - reads in an unsigned hexadecimal number from the terminal.
                  The read continues until a space, newline, esc or
                  end of file is reached.
*)

PROCEDURE ReadShortHex (VAR c: SHORTCARD) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(InOut.ReadS()) ;
   IF InOut.Done
   THEN
      c := StringToShortCardinal(s, 16, Done)
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadShortHex ;


(*
   WriteShortHex - writes out a SHORTCARD, c, in hexadecimal format padding
                   with, n, characters (leading with '0')
*)

PROCEDURE WriteShortHex (c: SHORTCARD; n: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := KillString(InOut.WriteS(ShortCardinalToString(c, n, '0', 16, TRUE))) ;
   Done := TRUE
END WriteShortHex ;


END CardinalIO.
(*
 * Local variables:
 *  compile-command: "gm2 -I.:../gm2-libs -g -c -Wsources CardinalIO.mod"
 * End:
 *)
