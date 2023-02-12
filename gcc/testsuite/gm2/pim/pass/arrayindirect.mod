(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE arrayindirect ;

FROM SYSTEM IMPORT ADR, TSIZE, SIZE, WORD, ADDRESS ;

CONST
   UNIXREADONLY        =       0 ;
   CreatePermissions   =     666B;
   MaxNoOfFiles        =     100 ;
   MaxBufferLength     = 1024*16 ;
   MaxErrorString      = 1024* 8 ;

TYPE
   FileUsage         = (unused, openedforread, openedforwrite, openedforrandom) ;
   FileStatus        = (successful, outofmemory, toomanyfilesopen, failed, connectionfailure) ;

   NameInfo          = RECORD
                          address: ADDRESS ;
                          size   : CARDINAL ;
                       END ;

   Buffer            = POINTER TO buf ;
   buf               =            RECORD
                                     position: CARDINAL ;  (* where are we through this buffer *)
                                     address : ADDRESS ;   (* dynamic buffer address           *)
                                     filled  : CARDINAL ;  (* length of the buffer filled      *)
                                     size    : CARDINAL ;  (* maximum space in this buffer     *)
                                     left    : CARDINAL ;  (* number of bytes left to read     *)
                                     contents: POINTER TO ARRAY [0..MaxBufferLength] OF CHAR ;
                                  END ;

   FileDescriptors   = POINTER TO fds ;
   fds               =            RECORD
                                     unixfd: INTEGER ;
                                     name  : NameInfo ;
                                     state : FileStatus ;
                                     usage : FileUsage ;
                                     buffer: Buffer ;
                                  END ;
   File = CARDINAL ;

VAR
   FileInfo: ARRAY [0..MaxNoOfFiles] OF FileDescriptors ;


(*
   GetUnixFileDescriptor - returns the UNIX file descriptor of a file.
*)

PROCEDURE GetUnixFileDescriptor (f: File) : INTEGER ;
BEGIN
   IF (f<MaxNoOfFiles) AND (FileInfo[f]#NIL)
   THEN
      RETURN( FileInfo[f]^.unixfd )
   ELSE
      HALT
   END
END GetUnixFileDescriptor ;

BEGIN
   IF GetUnixFileDescriptor(File(1))=0
   THEN
      HALT
   END
END arrayindirect.