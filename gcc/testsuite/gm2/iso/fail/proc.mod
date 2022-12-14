(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE proc ;

IMPORT ConvTypes ;

TYPE
   type = ConvTypes.ScanState ;


PROCEDURE ScanInt (inputCh: CARDINAL;   (* this first param should be a CHAR *)
                   VAR chClass: ConvTypes.ScanClass;
                   VAR nextState: ConvTypes.ScanState) ;
BEGIN
END ScanInt ;

PROCEDURE test ;
BEGIN
END test ;

PROCEDURE foo () : type ;
BEGIN
   RETURN ScanInt
END foo ;

PROCEDURE bar () : PROC ;
BEGIN
   RETURN test
END bar ;


VAR
   p : ConvTypes.ScanState ;
   ch: CHAR ;
   c : ConvTypes.ScanClass ;
   n : ConvTypes.ScanState ;
BEGIN
   p := foo() ;
   p(ch, c, n)
END proc.
