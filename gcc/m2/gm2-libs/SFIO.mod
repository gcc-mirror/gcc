(* SFIO.mod provides a String interface to the opening routines of FIO.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE SFIO ;

FROM ASCII IMPORT nul ;

FROM DynamicStrings IMPORT string, Length, InitString, ConCatChar,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM FIO IMPORT exists, openToRead, openToWrite, openForRandom, WriteNBytes, ReadChar,
                EOLN, EOF, IsNoError ;

(*
#undef GM2_DEBUG_SFIO
#if defined(GM2_DEBUG_SFIO)
#  define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#  define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#  define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#  define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#  define Dup(X) DupDB(X, __FILE__, __LINE__)
#  define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
#endif
*)


(*
   Exists - returns TRUE if a file named, fname exists for reading.
*)

PROCEDURE Exists (fname: String) : BOOLEAN ;
BEGIN
   RETURN exists (string (fname), Length (fname))
END Exists ;


(*
   OpenToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*)

PROCEDURE OpenToRead (fname: String) : File ;
BEGIN
   RETURN openToRead (string (fname), Length (fname))
END OpenToRead ;


(*
   OpenToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*)

PROCEDURE OpenToWrite (fname: String) : File ;
BEGIN
   RETURN openToWrite (string (fname), Length (fname))
END OpenToWrite ;


(*
   OpenForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
                   if towrite is TRUE or whether the previous file should
                   be left alone, allowing this descriptor to seek
                   and modify an existing file.
*)

PROCEDURE OpenForRandom (fname: String; towrite, newfile: BOOLEAN) : File ;
BEGIN
   RETURN openForRandom (string (fname), Length (fname), towrite, newfile)
END OpenForRandom ;


(*
   WriteS - writes a string, s, to, file. It returns the String, s.
*)

PROCEDURE WriteS (file: File; s: String) : String ;
VAR
   nBytes: CARDINAL ;
BEGIN
   IF s#NIL
   THEN
      nBytes := WriteNBytes(file, Length(s), string(s))
   END ;
   RETURN( s )
END WriteS ;


(*
   ReadS - reads and returns a string from, file.
           It stops reading the string at the end of line or end of file.
           It consumes the newline at the end of line but does not place
           this into the returned string.
*)

PROCEDURE ReadS (file: File) : String ;
VAR
   s: String ;
BEGIN
   s := InitString ('') ;
   WHILE (NOT EOLN (file)) AND (NOT EOF (file)) AND IsNoError (file) DO
      s := ConCatChar (s, ReadChar (file))
   END ;
   IF EOLN (file)
   THEN
      (* consume nl *)
      IF ReadChar (file) = nul
      THEN
      END
   END ;
   RETURN s
END ReadS ;


END SFIO.
