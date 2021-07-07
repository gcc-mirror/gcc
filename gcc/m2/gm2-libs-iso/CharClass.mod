(* CharClass.mod implement the ISO CharClass specification.

Copyright (C) 2002-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE CharClass ;

FROM ASCII IMPORT lf, cr, tab ;

(* Classification of values of the type CHAR *)

(* Returns TRUE if and only if ch is classified as a numeric character *)

PROCEDURE IsNumeric (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN (ch>='0') AND (ch<='9')
END IsNumeric ;


(* Returns TRUE if and only if ch is classified as a letter *)

PROCEDURE IsLetter (ch: CHAR): BOOLEAN;
BEGIN
   RETURN ((ch>='a') AND (ch<='z')) OR ((ch>='A') AND (ch<='Z'))
END IsLetter ;


(* Returns TRUE if and only if ch is classified as an upper case letter *)

PROCEDURE IsUpper (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch>='A') AND (ch<='Z')
END IsUpper ;


(* Returns TRUE if and only if ch is classified as a lower case letter *)

PROCEDURE IsLower (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch>='a') AND (ch<='z')
END IsLower ;


(* Returns TRUE if and only if ch represents a control function *)

PROCEDURE IsControl (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch<' ')
END IsControl ;


(* Returns TRUE if and only if ch represents a space character or a format effector *)

PROCEDURE IsWhiteSpace (ch: CHAR): BOOLEAN;
BEGIN
   RETURN (ch=' ') OR (ch=cr) OR (ch=lf) OR (ch=tab)
END IsWhiteSpace ;


END CharClass.
