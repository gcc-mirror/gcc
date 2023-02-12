(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE testconv ;   (*!m2pim*)

FROM ConvTypes IMPORT ConvResults, ScanClass ;
IMPORT ConvTypes ;


PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                    VAR nextState: ConvTypes.ScanState) ;
BEGIN

END ScanReal ;


PROCEDURE FormatReal (str: ARRAY OF CHAR) : ConvResults ;
VAR
   proc   : ConvTypes.ScanState ;
   chClass: ConvTypes.ScanClass ;
   i, h   : CARDINAL ;
BEGIN
   i := 1 ;
   h := LENGTH(str) ;
   ScanReal(str[0], chClass, proc) ;
   WHILE (i<h) AND (chClass=padding) DO
      proc(str[i], chClass, proc) ;
      INC(i)
   END ;
   RETURN strAllRight
END FormatReal ;


END testconv.
