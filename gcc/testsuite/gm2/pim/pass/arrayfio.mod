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

MODULE arrayfio ;

FROM Storage IMPORT ALLOCATE ;

TYPE
   FileRec = POINTER TO RECORD
                           state: (successful, not) ;
                        END ;

TYPE
   File = CARDINAL ;

CONST
   MaxNoOfFiles = 10 ;

VAR
   FileInfo: ARRAY [0..MaxNoOfFiles] OF FileRec ;


PROCEDURE IsNoError (f: File) : BOOLEAN ;
BEGIN
   RETURN( (f<MaxNoOfFiles) AND (FileInfo[f]#NIL) AND (FileInfo[f]^.state=successful) )
END IsNoError ;


BEGIN
   NEW(FileInfo[0]) ;
   FileInfo[0]^.state := successful ;
   IF IsNoError(0)
   THEN
   END
END arrayfio.
