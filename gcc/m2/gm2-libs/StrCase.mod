(* StrCase.mod provides procedure to convert between text case.

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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE StrCase ;


FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen ;


(*
   StrToUpperCase - converts string, a, to uppercase returning the
                    result in, b.
*)

PROCEDURE StrToUpperCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
VAR
   higha,
   highb,
   i    : CARDINAL ;
BEGIN
   higha := StrLen(a) ;
   highb := HIGH(b) ;
   i := 0 ;
   WHILE (i<higha) AND (a[i]#nul) AND (i<highb) DO
      b[i] := Cap(a[i]) ;
      INC(i)
   END ;
   IF i<highb
   THEN
      b[i] := nul
   END
END StrToUpperCase ;


(*
   StrToLowerCase - converts string, a, to lowercase returning the
                    result in, b.
*)

PROCEDURE StrToLowerCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
VAR
   higha,
   highb,
   i    : CARDINAL ;
BEGIN
   higha := StrLen(a) ;
   highb := HIGH(b) ;
   i := 0 ;
   WHILE (i<higha) AND (a[i]#nul) AND (i<highb) DO
      b[i] := Lower(a[i]) ;
      INC(i)
   END ;
   IF i<highb
   THEN
      b[i] := nul
   END
END StrToLowerCase ;


(*
   Cap - converts a lower case character into a capital character.
         If the character is not a lower case character 'a'..'z'
         then the character is simply returned unaltered.
*)

PROCEDURE Cap (ch: CHAR) : CHAR ;
BEGIN
   IF (ch>='a') AND (ch<='z')
   THEN
      ch := CHR( ORD(ch)-ORD('a')+ORD('A') )
   END ;
   RETURN( ch )
END Cap ;


(*
   Lower - converts an upper case character into a lower case character.
           If the character is not an upper case character 'A'..'Z'
           then the character is simply returned unaltered.
*)

PROCEDURE Lower (ch: CHAR) : CHAR ;
BEGIN
   IF (ch>='A') AND (ch<='Z')
   THEN
      ch := CHR( ORD(ch)-ORD('A')+ORD('a') )
   END ;
   RETURN( ch )
END Lower ;


END StrCase.
