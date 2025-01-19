(* mcStream.mod provides an interface to create a file from fragments.

Copyright (C) 2015-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE mcStream ;


FROM FIO IMPORT File, OpenToWrite, OpenToRead, EOF, ReadNBytes, WriteNBytes, Close, getFileName, IsNoError ;
FROM libc IMPORT unlink, printf, getpid, exit ;
FROM Indexing IMPORT InitIndex, InBounds, HighIndice, LowIndice, PutIndice, GetIndice, Index, ForeachIndiceInIndexDo ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, string ;
FROM FormatStrings IMPORT Sprintf2 ;
FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;

IMPORT alists ;
IMPORT SFIO ;


TYPE
   ptrToFile = POINTER TO File ;

VAR
   listOfFiles: alists.alist ;
   frag       : Index ;
   destFile   : File ;
   seenDest   : BOOLEAN ;


(*
   removeLater -
*)

PROCEDURE removeLater (filename: String) : String ;
BEGIN
   alists.includeItemIntoList (listOfFiles, filename) ;
   RETURN filename
END removeLater ;


(*
   removeNow - removes a single file, s.
*)

PROCEDURE removeNow (s: String) ;
BEGIN
   IF unlink (string (s)) # 0
   THEN
   END
END removeNow ;


(*
   removeFiles - remove any fragment.
*)

PROCEDURE removeFiles ;
BEGIN
   alists.foreachItemInListDo (listOfFiles, removeNow) ;
   alists.killList (listOfFiles) ;
   listOfFiles := alists.initList ()
END removeFiles ;


(*
   createTemporaryFile -
*)

PROCEDURE createTemporaryFile (id: CARDINAL) : File ;
VAR
   s: String ;
   f: File ;
   p: INTEGER ;
BEGIN
   s := InitString ('/tmp/frag-%d-%d.frag') ;
   p := getpid () ;
   s := removeLater (Sprintf2 (s, p, id)) ;
   f := SFIO.OpenToWrite (s) ;
   RETURN f
END createTemporaryFile ;


(*
   openFrag - create and open fragment, id, and return the file.
              The file should not be closed by the user.
*)

PROCEDURE openFrag (id: CARDINAL) : File ;
VAR
   f: File ;
   p: ptrToFile ;
BEGIN
   f := createTemporaryFile (id) ;
   NEW (p) ;
   p^ := f ;
   PutIndice (frag, id, p) ;
   RETURN f
END openFrag ;


(*
   copy - copies contents of f to the destination file.
*)

PROCEDURE copy (p: ptrToFile) ;
CONST
   maxBuffer = 4096 ;
VAR
   buffer: ARRAY [0..maxBuffer] OF CHAR ;
   b     : CARDINAL ;
   s     : String ;
   f     : File ;
BEGIN
   IF p # NIL
   THEN
      f := p^ ;
      s := InitStringCharStar(getFileName (f)) ;
      Close (f) ;
      f := SFIO.OpenToRead (s) ;
      WHILE (NOT EOF (f)) AND IsNoError (f) DO
         b := ReadNBytes (f, HIGH (buffer), ADR (buffer)) ;
         IF IsNoError (f)
         THEN
            b := WriteNBytes (destFile, b, ADR (buffer))
         ELSIF NOT EOF (f)
         THEN
            printf ("mcStream.mod:copy: error seen when reading file fragment: %s\n",
                    string (s)) ;
            exit (1)
         END
      END ;
      Close (f)
   END
END copy ;


(*
   setDest - informs the stream module and all fragments must be copied
             info, f.
*)

PROCEDURE setDest (f: File) ;
BEGIN
   seenDest := TRUE ;
   destFile := f
END setDest ;


(*
   combine - closes all fragments and then writes them in
             order to the destination file.  The dest file
             is returned.
*)

PROCEDURE combine () : File ;
BEGIN
   IF NOT seenDest
   THEN
      HALT
   END ;
   ForeachIndiceInIndexDo (frag, copy) ;
   removeFiles ;
   RETURN destFile
END combine ;


BEGIN
   listOfFiles := alists.initList () ;
   seenDest := FALSE ;
   frag := InitIndex (1)
END mcStream.
