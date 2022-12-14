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

MODULE tinyrecord ;   (*!m2pim*)

FROM SYSTEM IMPORT ADDRESS ;

CONST
   MaxBufferLength = 100 ;

TYPE
   Buffer = POINTER TO RECORD
                          valid   : BOOLEAN ;   (* are the field valid?             *)
                          bufstart: LONGINT ;   (* the position of buffer in file   *)
                          position: CARDINAL ;  (* where are we through this buffer *)
                          address : ADDRESS ;   (* dynamic buffer address           *)
                          filled  : CARDINAL ;  (* length of the buffer filled      *)
                          size    : CARDINAL ;  (* maximum space in this buffer     *)
                          left    : CARDINAL ;  (* number of bytes left to read     *)
                          contents: POINTER TO ARRAY [0..MaxBufferLength] OF CHAR ;
                       END ;

VAR
   b: Buffer ;
   a: ADDRESS ;
BEGIN
   b^.contents := a
END tinyrecord.
