(* StringChan.mod implements String input/output over channels.

Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE StringChan ;


FROM DynamicStrings IMPORT Length, char ;
FROM TextIO IMPORT WriteChar ;


(*
   writeString - writes a string, s, to ChanId, cid.
                 The string, s, is not destroyed.
*)

PROCEDURE writeString (cid: IOChan.ChanId; s: String) ;
VAR
   i, h: CARDINAL ;
BEGIN
   h := Length(s) ;
   i := 0 ;
   WHILE i<h DO
      WriteChar(cid, char(s, i)) ;
      INC(i)
   END
END writeString ;


(*
   writeFieldWidth - writes a string, s, to ChanId, cid.
                     The string, s, is not destroyed and it
                     is prefixed by spaces so that at least,
                     width, characters are written.  If the
                     string, s, is longer than width then
                     no spaces are prefixed to the output
                     and the entire string is written.
*)

PROCEDURE writeFieldWidth (cid: IOChan.ChanId;
                           s: String; width: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := Length(s) ;
   WHILE i<width DO
      WriteChar(cid, ' ') ;
      INC(i)
   END ;
   writeString(cid, s)
END writeFieldWidth ;


END StringChan.
