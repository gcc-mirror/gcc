(* FIO.mod provides a simple buffered file input/output library.

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

IMPLEMENTATION MODULE FIO ;

(*
    Title      : FIO
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Sep  2 22:07:21 1999
    Last edit  : Thu Sep  2 22:07:21 1999
    Description: a complete reimplememtation of FIO.mod
                 provides a simple buffered file input/output library.
*)

FROM SYSTEM IMPORT ADR, TSIZE, WORD, CSSIZE_T ;
FROM ASCII IMPORT nl, nul, tab ;
FROM StrLib IMPORT StrLen, StrConCat, StrCopy ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM NumberIO IMPORT CardToStr ;
FROM Indexing IMPORT Index, InitIndex, InBounds, HighIndice, PutIndice, GetIndice ;
FROM M2RTS IMPORT InstallTerminationProcedure ;
FROM libc IMPORT exit, open, creat, read, write, close, lseek, strncpy, memcpy ;
FROM wrapc IMPORT SeekSet, SeekEnd, ReadOnly, WriteOnly ;


CONST
   MaxBufferLength     = 1024*16 ;
   MaxErrorString      = 1024* 8 ;
   CreatePermissions   =     666B;

TYPE
   FileUsage         = (unused, openedforread, openedforwrite, openedforrandom) ;
   FileStatus        = (successful, outofmemory, toomanyfilesopen, failed, connectionfailure, endofline, endoffile) ;

   NameInfo          = RECORD
                          address: ADDRESS ;
                          size   : CARDINAL ;
                       END ;

   Buffer            = POINTER TO buf ;
   buf               =            RECORD
                                     valid   : BOOLEAN ;   (* are the field valid?             *)
                                     bufstart: LONGINT ;   (* the position of buffer in file   *)
                                     position: CARDINAL ;  (* where are we through this buffer *)
                                     address : ADDRESS ;   (* dynamic buffer address           *)
                                     filled  : CARDINAL ;  (* length of the buffer filled      *)
                                     size    : CARDINAL ;  (* maximum space in this buffer     *)
                                     left    : CARDINAL ;  (* number of bytes left to read     *)
                                     contents: POINTER TO ARRAY [0..MaxBufferLength] OF CHAR ;
                                  END ;

   FileDescriptor   = POINTER TO fds ;
   fds               =            RECORD
                                     unixfd: INTEGER ;
                                     name  : NameInfo ;
                                     state : FileStatus ;
                                     usage : FileUsage ;
                                     output: BOOLEAN ;     (* is this file going to write data *)
                                     buffer: Buffer ;
                                     abspos: LONGINT ;     (* absolute position into file.     *)
                                  END ;                    (* reflects low level reads which   *)
                                                           (* means this value will normally   *)
                                                           (* be further through the file than *)
                                                           (* bufstart above.                  *)
   PtrToChar         = POINTER TO CHAR ;


VAR
   FileInfo: Index ;
   Error   : File ;   (* not stderr, this is an unused file handle
                         which only serves to hold status values
                         when we cannot create a new file handle *)


(*
   GetUnixFileDescriptor - returns the UNIX file descriptor of a file.
*)

PROCEDURE GetUnixFileDescriptor (f: File) : INTEGER ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         RETURN( fd^.unixfd )
      END
   END ;
   FormatError1('file %d has not been opened or is out of range\n', f) ;
   RETURN( -1 )
END GetUnixFileDescriptor ;


(*
   WriteString - writes a string to file, f.
*)

PROCEDURE WriteString (f: File; a: ARRAY OF CHAR) ;
VAR
   l: CARDINAL ;
BEGIN
   l := StrLen(a) ;
   IF WriteNBytes(f, l, ADR(a))#l
   THEN
   END
END WriteString ;


(*
   Max - returns the maximum of two values.
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - returns the minimum of two values.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   GetNextFreeDescriptor - returns the index to the FileInfo array indicating
                           the next free slot.
*)

PROCEDURE GetNextFreeDescriptor () : File ;
VAR
   f, h: File ;
   fd  : FileDescriptor ;
BEGIN
   f := Error+1 ;
   h := HighIndice(FileInfo) ;
   LOOP
      IF f<=h
      THEN
         fd := GetIndice(FileInfo, f) ;
         IF fd=NIL
         THEN
            RETURN( f )
         END
      END ;
      INC(f) ;
      IF f>h
      THEN
         PutIndice(FileInfo, f, NIL) ;  (* create new slot *)
         RETURN( f )
      END
   END
END GetNextFreeDescriptor ;


(*
   IsNoError - returns a TRUE if no error has occured on file, f.
*)

PROCEDURE IsNoError (f: File) : BOOLEAN ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f=Error
   THEN
      RETURN( FALSE )
   ELSE
      fd := GetIndice(FileInfo, f) ;
      RETURN( (fd#NIL) AND ((fd^.state=successful) OR (fd^.state=endoffile) OR (fd^.state=endofline)) )
   END
END IsNoError ;


(*
   IsActive - returns TRUE if the file, f, is still active.
*)

PROCEDURE IsActive (f: File) : BOOLEAN ;
BEGIN
   IF f=Error
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( GetIndice(FileInfo, f)#NIL )
   END
END IsActive ;


(*
   openToRead - attempts to open a file, fname, for reading and
                it returns this file.
                The success of this operation can be checked by
                calling IsNoError.
*)

PROCEDURE openToRead (fname: ADDRESS; flength: CARDINAL) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f=Error
   THEN
      SetState(f, toomanyfilesopen)
   ELSE
      f := InitializeFile(f, fname, flength, successful, openedforread, FALSE, MaxBufferLength) ;
      ConnectToUnix(f, FALSE, FALSE)
   END ;
   RETURN( f )
END openToRead ;


(*
   openToWrite - attempts to open a file, fname, for write and
                 it returns this file.
                 The success of this operation can be checked by
                 calling IsNoError.
*)

PROCEDURE openToWrite (fname: ADDRESS; flength: CARDINAL) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f=Error
   THEN
      SetState(f, toomanyfilesopen)
   ELSE
      f := InitializeFile(f, fname, flength, successful, openedforwrite, TRUE, MaxBufferLength) ;
      ConnectToUnix(f, TRUE, TRUE)
   END ;
   RETURN( f )
END openToWrite ;


(*
   openForRandom - attempts to open a file, fname, for random access
                   read or write and it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
                   towrite, determines whether the file should be
                   opened for writing or reading.
*)

PROCEDURE openForRandom (fname: ADDRESS; flength: CARDINAL;
                         towrite, newfile: BOOLEAN) : File ;
VAR
   f: File ;
BEGIN
   f := GetNextFreeDescriptor() ;
   IF f=Error
   THEN
      SetState(f, toomanyfilesopen)
   ELSE
      f := InitializeFile(f, fname, flength, successful, openedforrandom, towrite, MaxBufferLength) ;
      ConnectToUnix(f, towrite, newfile)
   END ;
   RETURN( f )
END openForRandom ;


(*
   exists - returns TRUE if a file named, fname exists for reading.
*)

PROCEDURE exists (fname: ADDRESS; flength: CARDINAL) : BOOLEAN ;
VAR
   f: File ;
BEGIN
   f := openToRead(fname, flength) ;
   IF IsNoError(f)
   THEN
      Close(f) ;
      RETURN( TRUE )
   ELSE
      Close(f) ;
      RETURN( FALSE )
   END
END exists ;


(*
   SetState - sets the field, state, of file, f, to, s.
*)

PROCEDURE SetState (f: File; s: FileStatus) ;
VAR
   fd: FileDescriptor ;
BEGIN
   fd := GetIndice(FileInfo, f) ;
   fd^.state := s
END SetState ;


(*
   InitializeFile - initialize a file descriptor
*)

PROCEDURE InitializeFile (f: File; fname: ADDRESS;
                          flength: CARDINAL; fstate: FileStatus;
                          use: FileUsage;
                          towrite: BOOLEAN; buflength: CARDINAL) : File ;
VAR
   p : PtrToChar ;
   fd: FileDescriptor ;
BEGIN
   NEW(fd) ;
   IF fd=NIL
   THEN
      SetState(Error, outofmemory) ;
      RETURN( Error )
   ELSE
      PutIndice(FileInfo, f, fd) ;
      WITH fd^ DO
         name.size := flength+1 ;  (* need to guarantee the nul for C *)
         usage     := use ;
         output    := towrite ;
         ALLOCATE(name.address, name.size) ;
         IF name.address=NIL
         THEN
            state := outofmemory ;
            RETURN( f )
         END ;
         name.address := strncpy(name.address, fname, flength) ;
         (* and assign nul to the last byte *)
         p := name.address ;
         INC(p, flength) ;
         p^ := nul ;
         abspos := 0 ;
         (* now for the buffer *)
         NEW(buffer) ;
         IF buffer=NIL
         THEN
            SetState(Error, outofmemory) ;
            RETURN( Error )
         ELSE
            WITH buffer^ DO
               valid    := FALSE ;
               bufstart := 0 ;
               size     := buflength ;
               position := 0 ;
               filled   := 0 ;
               IF size=0
               THEN
                  address := NIL
               ELSE
                  ALLOCATE(address, size) ;
                  IF address=NIL
                  THEN
                     state := outofmemory ;
                     RETURN( f )
                  END
               END ;
               IF towrite
               THEN
                  left := size
               ELSE
                  left := 0
               END ;
               contents := address ;  (* provides easy access for reading characters *)
            END ;
            state := fstate
         END
      END
   END ;
   RETURN( f )
END InitializeFile ;


(*
   ConnectToUnix - connects a FIO file to a UNIX file descriptor.
*)

PROCEDURE ConnectToUnix (f: File; towrite, newfile: BOOLEAN) ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            IF towrite
            THEN
               IF newfile
               THEN
                  unixfd := creat(name.address, CreatePermissions)
               ELSE
                  unixfd := open(name.address, INTEGER (WriteOnly ()), 0)
               END
            ELSE
               unixfd := open(name.address, INTEGER (ReadOnly ()), 0)
            END ;
            IF unixfd<0
            THEN
               state := connectionfailure
            END
         END
      END
   END
END ConnectToUnix ;


(*
   The following functions are wrappers for the above.
*)

PROCEDURE Exists (fname: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN( exists(ADR(fname), StrLen(fname)) )
END Exists ;


PROCEDURE OpenToRead (fname: ARRAY OF CHAR) : File ;
BEGIN
   RETURN( openToRead(ADR(fname), StrLen(fname)) )
END OpenToRead ;


PROCEDURE OpenToWrite (fname: ARRAY OF CHAR) : File ;
BEGIN
   RETURN( openToWrite(ADR(fname), StrLen(fname)) )
END OpenToWrite ;


PROCEDURE OpenForRandom (fname: ARRAY OF CHAR;
                         towrite: BOOLEAN; newfile: BOOLEAN) : File ;
BEGIN
   RETURN( openForRandom(ADR(fname), StrLen(fname), towrite, newfile) )
END OpenForRandom ;


(*
   Close - close a file which has been previously opened using:
           OpenToRead, OpenToWrite, OpenForRandom.
           It is correct to close a file which has an error status.
*)

PROCEDURE Close (f: File) ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      (*
         we allow users to close files which have an error status
      *)
      IF fd#NIL
      THEN
         FlushBuffer(f) ;
         WITH fd^ DO
            IF unixfd>=0
            THEN
               IF close(unixfd)#0
               THEN
                  FormatError1('failed to close file (%s)\n', name.address) ;
                  state := failed   (* --fixme-- too late to notify user (unless we return a BOOLEAN) *)
               END
            END ;
            IF name.address#NIL
            THEN
               DEALLOCATE(name.address, name.size)
            END ;
            IF buffer#NIL
            THEN
               WITH buffer^ DO
                  IF address#NIL
                  THEN
                     DEALLOCATE(address, size)
                  END
               END ;
               DISPOSE(buffer) ;
               buffer := NIL
            END
         END ;
         DISPOSE(fd) ;
         PutIndice(FileInfo, f, NIL)
      END
   END
END Close ;


(*
   ReadFromBuffer - attempts to read, nBytes, from file, f.
                    It firstly consumes the buffer and then performs
                    direct unbuffered reads. This should only be used
                    when wishing to read large files.

                    The actual number of bytes read is returned.
                    -1 is returned if EOF is reached.
*)

PROCEDURE ReadFromBuffer (f: File; a: ADDRESS; nBytes: CARDINAL) : INTEGER ;
VAR
   t     : ADDRESS ;
   result: INTEGER ;
   total,
   n     : CARDINAL ;
   p     : POINTER TO BYTE ;
   fd    : FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      total := 0 ;   (* how many bytes have we read *)
      fd := GetIndice(FileInfo, f) ;
      WITH fd^ DO
         (* extract from the buffer first *)
         IF (buffer#NIL) AND (buffer^.valid)
         THEN
            WITH buffer^ DO
               IF left>0
               THEN
                  IF nBytes=1
                  THEN
                     (* too expensive to call memcpy for 1 character *)
                     p := a ;
                     p^ := contents^[position] ;
                     DEC(left) ;         (* remove consumed bytes               *)
                     INC(position) ;     (* move onwards n bytes                *)
                     nBytes := 0 ;       (* reduce the amount for future direct *)
                                         (* read                                *)
                     RETURN( 1 )
                  ELSE
                     n := Min(left, nBytes) ;
                     t := address ;
                     INC(t, position) ;
                     p := memcpy(a, t, n) ;
                     DEC(left, n) ;      (* remove consumed bytes               *)
                     INC(position, n) ;  (* move onwards n bytes                *)
                                         (* move onwards ready for direct reads *)
                     INC(a, n) ;
                     DEC(nBytes, n) ;    (* reduce the amount for future direct *)
                                         (* read                                *)
                     INC(total, n) ;
                     RETURN( total )     (* much cleaner to return now,         *)
                  END                    (* difficult to record an error if     *)
               END                       (* the read below returns -1           *)
            END
         END ;
         IF nBytes>0
         THEN
            (* still more to read *)
            result := read(unixfd, a, INTEGER(nBytes)) ;
            IF result>0
            THEN
               INC(total, result) ;
               INC(abspos, result) ;
               (* now disable the buffer as we read directly into, a. *)
               IF buffer#NIL
               THEN
                  buffer^.valid := FALSE
               END ;
            ELSE
               IF result=0
               THEN
                  (* eof reached *)
                  state := endoffile
               ELSE
                  state := failed
               END ;
               (* indicate buffer is empty *)
               IF buffer#NIL
               THEN
                  WITH buffer^ DO
                     valid    := FALSE ;
                     left     := 0 ;
                     position := 0 ;
                     IF address#NIL
                     THEN
                        contents^[position] := nul
                     END
                  END
               END ;
               RETURN( -1 )
            END
         END
      END ;
      RETURN( total )
   ELSE
      RETURN( -1 )
   END
END ReadFromBuffer ;


(*
   ReadNBytes - reads nBytes of a file into memory area, dest, returning
                the number of bytes actually read.
                This function will consume from the buffer and then
                perform direct libc reads. It is ideal for large reads.
*)

PROCEDURE ReadNBytes (f: File; nBytes: CARDINAL; dest: ADDRESS) : CARDINAL ;
VAR
   n: INTEGER ;
   p: POINTER TO CHAR ;
BEGIN
   IF f # Error
   THEN
      CheckAccess (f, openedforread, FALSE) ;
      n := ReadFromBuffer (f, dest, nBytes) ;
      IF n <= 0
      THEN
         RETURN 0
      ELSE
         p := dest ;
         INC (p, n-1) ;
         SetEndOfLine (f, p^) ;
         RETURN n
      END
   ELSE
      RETURN 0
   END
END ReadNBytes ;


(*
   BufferedRead - will read, nBytes, through the buffer.
                  Similar to ReadFromBuffer, but this function will always
                  read into the buffer before copying into memory.

                  Useful when performing small reads.
*)

PROCEDURE BufferedRead (f: File; nBytes: CARDINAL; dest: ADDRESS) : INTEGER ;
VAR
   src   : ADDRESS ;
   total,
   n     : INTEGER ;
   p     : POINTER TO BYTE ;
   fd    : FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice (FileInfo, f) ;
      total := 0 ;   (* how many bytes have we read *)
      IF fd#NIL
      THEN
         WITH fd^ DO
            (* extract from the buffer first *)
            IF buffer # NIL
            THEN
               WITH buffer^ DO
                  WHILE nBytes > 0 DO
                     IF (left > 0) AND valid
                     THEN
                        IF nBytes = 1
                        THEN
                           (* too expensive to call memcpy for 1 character *)
                           p := dest ;
                           p^ := contents^[position] ;
                           DEC (left) ;         (* remove consumed byte                *)
                           INC (position) ;     (* move onwards n byte                 *)
                           INC (total) ;
                           RETURN( total )
                        ELSE
                           n := Min (left, nBytes) ;
                           src := address ;
                           INC (src, position) ;
                           p := memcpy (dest, src, n) ;
                           DEC (left, n) ;      (* remove consumed bytes               *)
                           INC (position, n) ;  (* move onwards n bytes                *)
                                               (* move onwards ready for direct reads *)
                           INC (dest, n) ;
                           DEC (nBytes, n) ;    (* reduce the amount for future direct *)
                                               (* read                                *)
                           INC (total, n)
                        END
                     ELSE
                        (* refill buffer *)
                        n := read (unixfd, address, size) ;
                        IF n >= 0
                        THEN
                           valid    := TRUE ;
                           position := 0 ;
                           left     := n ;
                           filled   := n ;
                           bufstart := abspos ;
                           INC (abspos, n) ;
                           IF n = 0
                           THEN
                              (* eof reached *)
                              state := endoffile ;
                              RETURN( -1 )
                           END
                        ELSE
                           valid    := FALSE ;
                           position := 0 ;
                           left     := 0 ;
                           filled   := 0 ;
                           state    := failed ;
                           RETURN( total )
                        END
                     END
                  END
               END ;
               RETURN( total )
            END
         END
      END
   END ;
   RETURN( -1 )
END BufferedRead ;


(*
   HandleEscape - translates \n and \t into their respective ascii codes.
*)

PROCEDURE HandleEscape (VAR dest: ARRAY OF CHAR; src: ARRAY OF CHAR;
                        VAR i, j: CARDINAL; HighSrc, HighDest: CARDINAL) ;
BEGIN
   IF (i+1<HighSrc) AND (src[i]='\') AND (j<HighDest)
   THEN
      IF src[i+1]='n'
      THEN
         (* requires a newline *)
         dest[j] := nl ;
         INC(j) ;
         INC(i, 2)
      ELSIF src[i+1]='t'
      THEN
         (* requires a tab (yuck) tempted to fake this but I better not.. *)
         dest[j] := tab ;
         INC(j) ;
         INC(i, 2)
      ELSE
         (* copy escaped character *)
         INC(i) ;
         dest[j] := src[i] ;
         INC(j) ;
         INC(i)
      END
   END
END HandleEscape ;


(*
   Cast - casts a := b
*)

PROCEDURE Cast (VAR a: ARRAY OF BYTE; b: ARRAY OF BYTE) ;
VAR
   i: CARDINAL ;
BEGIN
   IF HIGH(a)=HIGH(b)
   THEN
      FOR i := 0 TO HIGH(a) DO
         a[i] := b[i]
      END
   ELSE
      FormatError('cast failed')
   END
END Cast ;


(*
   StringFormat1 - converts string, src, into, dest, together with encapsulated
                   entity, w. It only formats the first %s or %d with n.
*)

PROCEDURE StringFormat1 (VAR dest: ARRAY OF CHAR; src: ARRAY OF CHAR;
                         w: ARRAY OF BYTE) ;
VAR
   HighSrc,
   HighDest,
   c, i, j : CARDINAL ;
   str     : ARRAY [0..MaxErrorString] OF CHAR ;
   p       : POINTER TO CHAR ;
BEGIN
   HighSrc := StrLen(src) ;
   HighDest := HIGH(dest) ;
   p := NIL ;
   c := 0 ;
   i := 0 ;
   j := 0 ;
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) AND (src[i]#'%') DO
      IF src[i]='\'
      THEN
         HandleEscape(dest, src, i, j, HighSrc, HighDest)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;

   IF (i+1<HighSrc) AND (src[i]='%') AND (j<HighDest)
   THEN
      IF src[i+1]='s'
      THEN
         Cast(p, w) ;
         WHILE (j<HighDest) AND (p^#nul) DO
            dest[j] := p^ ;
            INC(j) ;
            INC(p)
         END ;
         IF j<HighDest
         THEN
            dest[j] := nul
         END ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSIF src[i+1]='d'
      THEN
         dest[j] := nul ;
         Cast(c, w) ;
         CardToStr(c, 0, str) ;
         StrConCat(dest, str, dest) ;
         j := StrLen(dest) ;
         INC(i, 2)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   (* and finish off copying src into dest *)
   WHILE (i<HighSrc) AND (src[i]#nul) AND (j<HighDest) DO
      IF src[i]='\'
      THEN
         HandleEscape(dest, src, i, j, HighSrc, HighDest)
      ELSE
         dest[j] := src[i] ;
         INC(i) ;
         INC(j)
      END
   END ;
   IF j<HighDest
   THEN
      dest[j] := nul
   END ;
END StringFormat1 ;


(*
   FormatError - provides a orthoganal counterpart to the procedure below.
*)

PROCEDURE FormatError (a: ARRAY OF CHAR) ;
BEGIN
   WriteString (StdErr, a)
END FormatError ;


(*
   FormatError1 - generic error procedure taking standard format string
                  and single parameter.
*)

PROCEDURE FormatError1 (a: ARRAY OF CHAR; w: ARRAY OF BYTE) ;
VAR
   s: ARRAY [0..MaxErrorString] OF CHAR ;
BEGIN
   StringFormat1 (s, a, w) ;
   FormatError (s)
END FormatError1 ;


(*
   FormatError2 - generic error procedure taking standard format string
                  and two parameters.
*)

PROCEDURE FormatError2 (a: ARRAY OF CHAR;
                        w1, w2: ARRAY OF BYTE) ;
VAR
   s: ARRAY [0..MaxErrorString] OF CHAR ;
BEGIN
   StringFormat1 (s, a, w1) ;
   FormatError1 (s, w2)
END FormatError2 ;


(*
   CheckAccess - checks to see whether a file f has been
                 opened for read/write.
*)

PROCEDURE CheckAccess (f: File; use: FileUsage; towrite: BOOLEAN) ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice (FileInfo, f) ;
      IF fd=NIL
      THEN
         IF f#StdErr
         THEN
            FormatError ('this file has probably been closed and not reopened successfully or alternatively never opened\n')
         END ;
         HALT
      ELSE
         WITH fd^ DO
            IF (use=openedforwrite) AND (usage=openedforread)
            THEN
               FormatError1 ('this file (%s) has been opened for reading but is now being written\n',
                             name.address) ;
               HALT
            ELSIF (use=openedforread) AND (usage=openedforwrite)
            THEN
               FormatError1('this file (%s) has been opened for writing but is now being read\n',
                            name.address) ;
               HALT
            ELSIF state=connectionfailure
            THEN
               FormatError1('this file (%s) was not successfully opened\n',
                            name.address) ;
               HALT
            ELSIF towrite#output
            THEN
               IF output
               THEN
                  FormatError1('this file (%s) was opened for writing but is now being read\n',
                               name.address) ;
                  HALT
               ELSE
                  FormatError1('this file (%s) was opened for reading but is now being written\n',
                               name.address) ;
                  HALT
               END
            END
         END
      END
   ELSE
      FormatError('this file has not been opened successfully\n') ;
      HALT
   END
END CheckAccess ;


(*
   ReadChar - returns a character read from file f.
              Sensible to check with IsNoError or EOF after calling
              this function.
*)

PROCEDURE ReadChar (f: File) : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   CheckAccess (f, openedforread, FALSE) ;
   IF BufferedRead (f, SIZE (ch), ADR (ch)) = VAL (INTEGER, SIZE (ch))
   THEN
      SetEndOfLine (f, ch) ;
      RETURN ch
   ELSE
      RETURN nul
   END
END ReadChar ;


(*
   SetEndOfLine -
*)

PROCEDURE SetEndOfLine (f: File; ch: CHAR) ;
VAR
   fd: FileDescriptor ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      WITH fd^ DO
         IF ch=nl
         THEN
            state := endofline
         ELSE
            state := successful
         END
      END
   END
END SetEndOfLine ;


(*
   UnReadChar - replaces a character, ch, back into file f.
                This character must have been read by ReadChar
                and it does not allow successive calls.  It may
                only be called if the previous read was successful
                or end of file was seen.
                If the state was previously endoffile then it
                is altered to successful.
                Otherwise it is left alone.
*)

PROCEDURE UnReadChar (f: File; ch: CHAR) ;
VAR
   fd  : FileDescriptor ;
   n   : CARDINAL ;
   a, b: ADDRESS ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      WITH fd^ DO
         IF (state=successful) OR (state=endoffile) OR (state=endofline)
         THEN
            IF (buffer#NIL) AND (buffer^.valid)
            THEN
               WITH buffer^ DO
                  (* we assume that a ReadChar has occurred, we will check just in case. *)
                  IF state=endoffile
                  THEN
                     position := MaxBufferLength ;
                     left := 0 ;
                     filled := 0 ;
                     state := successful
                  END ;
                  IF position>0
                  THEN
                     DEC(position) ;
                     INC(left) ;
                     contents^[position] := ch ;
                  ELSE
                     (* position=0 *)
                     (* if possible make room and store ch *)
                     IF filled=size
                     THEN
                        FormatError1('performing too many UnReadChar calls on file (%d)\n', f)
                     ELSE
                        n := filled-position ;
                        b := ADR(contents^[position]) ;
                        a := ADR(contents^[position+1]) ;
                        a := memcpy(a, b, n) ;
                        INC(filled) ;
                        contents^[position] := ch ;
                     END
                  END
               END
            END
         ELSE
            FormatError1('UnReadChar can only be called if the previous read was successful or end of file, error on file (%d)\n', f)
         END
      END
   END
END UnReadChar ;


(*
   ReadAny - reads HIGH (a) + 1 bytes into, a.  All input
             is fully buffered, unlike ReadNBytes and thus is more
             suited to small reads.
*)

PROCEDURE ReadAny (f: File; VAR a: ARRAY OF BYTE) ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF BufferedRead (f, HIGH (a) + 1, ADR (a)) = VAL (INTEGER, HIGH (a) + 1)
   THEN
      SetEndOfLine (f, a[HIGH(a)])
   END
END ReadAny ;


(*
   EOF - tests to see whether a file, f, has reached end of file.
*)

PROCEDURE EOF (f: File) : BOOLEAN ;
VAR
   fd: FileDescriptor ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         RETURN( fd^.state=endoffile )
      END
   END ;
   RETURN( TRUE )
END EOF ;


(*
   EOLN - tests to see whether a file, f, is upon a newline.
          It does NOT consume the newline.
*)

PROCEDURE EOLN (f: File) : BOOLEAN ;
VAR
   ch: CHAR ;
   fd: FileDescriptor ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   (*
      we will read a character and then push it back onto the input stream,
      having noted the file status, we also reset the status.
   *)
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         IF (fd^.state=successful) OR (fd^.state=endofline)
         THEN
            ch := ReadChar(f) ;
            IF (fd^.state=successful) OR (fd^.state=endofline)
            THEN
               UnReadChar(f, ch)
            END ;
            RETURN( ch=nl )
         END
      END
   END ;
   RETURN( FALSE )
END EOLN ;


(*
   WasEOLN - tests to see whether a file, f, has just seen a newline.
*)

PROCEDURE WasEOLN (f: File) : BOOLEAN ;
VAR
   fd: FileDescriptor ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   IF f=Error
   THEN
      RETURN FALSE
   ELSE
      fd := GetIndice(FileInfo, f) ;
      RETURN( (fd#NIL) AND (fd^.state=endofline) )
   END
END WasEOLN ;


(*
   WriteLine - writes out a linefeed to file, f.
*)

PROCEDURE WriteLine (f: File) ;
BEGIN
   WriteChar(f, nl)
END WriteLine ;


(*
   WriteNBytes - writes nBytes from memory area src to a file
                 returning the number of bytes actually written.
                 This function will flush the buffer and then
                 write the nBytes using a direct write from libc.
                 It is ideal for large writes.
*)

PROCEDURE WriteNBytes (f: File; nBytes: CARDINAL; src: ADDRESS) : CARDINAL ;
VAR
   total: INTEGER ;
   fd   : FileDescriptor ;
BEGIN
   CheckAccess(f, openedforwrite, TRUE) ;
   FlushBuffer(f) ;
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            total := write(unixfd, src, INTEGER(nBytes)) ;
            IF total<0
            THEN
               state := failed ;
               RETURN( 0 )
            ELSE
               INC(abspos, CARDINAL(total)) ;
               IF buffer#NIL
               THEN
                  buffer^.bufstart := abspos
               END ;
               RETURN( CARDINAL(total) )
            END
         END
      END
   END ;
   RETURN( 0 )
END WriteNBytes ;


(*
   BufferedWrite - will write, nBytes, through the buffer.
                   Similar to WriteNBytes, but this function will always
                   write into the buffer before copying into memory.

                   Useful when performing small writes.
*)

PROCEDURE BufferedWrite (f: File; nBytes: CARDINAL; src: ADDRESS) : INTEGER ;
VAR
   dest  : ADDRESS ;
   total,
   n     : INTEGER ;
   p     : POINTER TO BYTE ;
   fd    : FileDescriptor ;
BEGIN
   IF f # Error
   THEN
      fd := GetIndice (FileInfo, f) ;
      IF fd#NIL
      THEN
         total := 0 ;   (* how many bytes have we read *)
         WITH fd^ DO
            IF buffer # NIL
            THEN
               WITH buffer^ DO
                  WHILE nBytes > 0 DO
                     (* place into the buffer first *)
                     IF left > 0
                     THEN
                        IF nBytes = 1
                        THEN
                           (* too expensive to call memcpy for 1 character *)
                           p := src ;
                           contents^[position] := p^ ;
                           DEC (left) ;         (* reduce space                        *)
                           INC (position) ;     (* move onwards n byte                 *)
                           INC (total) ;
                           RETURN( total )
                        ELSE
                           n := Min (left, nBytes) ;
                           dest := address ;
                           INC (dest, position) ;
                           p := memcpy (dest, src, CARDINAL (n)) ;
                           DEC (left, n) ;      (* remove consumed bytes               *)
                           INC (position, n) ;  (* move onwards n bytes                *)
                                                (* move ready for further writes       *)
                           INC (src, n) ;
                           DEC (nBytes, n) ;    (* reduce the amount for future writes *)
                           INC (total, n)
                        END
                     ELSE
                        FlushBuffer (f) ;
                        IF (state#successful) AND (state#endofline)
                        THEN
                           nBytes := 0
                        END
                     END
                  END
               END ;
               RETURN( total )
            END
         END
      END
   END ;
   RETURN( -1 )
END BufferedWrite ;


(*
   FlushBuffer - flush contents of file, f.
*)

PROCEDURE FlushBuffer (f: File) ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            IF output AND (buffer#NIL)
            THEN
               WITH buffer^ DO
                  IF (position=0) OR (write(unixfd, address, position)=VAL(INTEGER, position))
                  THEN
                     INC(abspos, position) ;
                     bufstart := abspos ;
                     position := 0 ;
                     filled   := 0 ;
                     left     := size
                  ELSE
                     state := failed
                  END
               END
            END
         END
      END
   END
END FlushBuffer ;


(*
   WriteAny - writes HIGH (a) + 1 bytes onto, file, f.  All output
              is fully buffered, unlike WriteNBytes and thus is more
              suited to small writes.
*)

PROCEDURE WriteAny (f: File; VAR a: ARRAY OF BYTE) ;
BEGIN
   CheckAccess (f, openedforwrite, TRUE) ;
   IF BufferedWrite (f, HIGH (a) + 1, ADR (a)) = VAL (INTEGER, HIGH (a) + 1)
   THEN
   END
END WriteAny ;


(*
   WriteChar - writes a single character to file, f.
*)

PROCEDURE WriteChar (f: File; ch: CHAR) ;
BEGIN
   CheckAccess (f, openedforwrite, TRUE) ;
   IF BufferedWrite (f, SIZE (ch), ADR (ch)) = VAL (INTEGER, SIZE (ch))
   THEN
   END
END WriteChar ;


(*
   WriteCardinal - writes a CARDINAL to file, f.
                   It writes the binary image of the cardinal
                   to file, f.
*)

PROCEDURE WriteCardinal (f: File; c: CARDINAL) ;
BEGIN
   WriteAny(f, c)
END WriteCardinal ;


(*
   ReadCardinal - reads a CARDINAL from file, f.
                  It reads a binary image of a CARDINAL
                  from a file, f.
*)

PROCEDURE ReadCardinal (f: File) : CARDINAL ;
VAR
   c: CARDINAL ;
BEGIN
   ReadAny(f, c) ;
   RETURN( c )
END ReadCardinal ;


(*
   ReadString - reads a string from file, f, into string, a.
                It terminates the string if HIGH is reached or
                if a newline is seen or an error occurs.
*)

PROCEDURE ReadString (f: File; VAR a: ARRAY OF CHAR) ;
VAR
   high,
   i   : CARDINAL ;
   ch  : CHAR ;
BEGIN
   CheckAccess(f, openedforread, FALSE) ;
   high := HIGH(a) ;
   i := 0 ;
   REPEAT
      ch := ReadChar(f) ;
      IF i<=high
      THEN
         IF (ch=nl) OR (NOT IsNoError(f)) OR EOF(f)
         THEN
            a[i] := nul ;
            INC(i)
         ELSE
            a[i] := ch ;
            INC(i)
         END
      END
   UNTIL (ch=nl) OR (i>high) OR (NOT IsNoError(f)) OR EOF(f)
END ReadString ;


(*
   SetPositionFromBeginning - sets the position from the beginning of the file.
*)

PROCEDURE SetPositionFromBeginning (f: File; pos: LONGINT) ;
VAR
   offset: LONGINT ;
   fd    : FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            (* always force the lseek, until we are confident that abspos is always correct,
               basically it needs some hard testing before we should remove the OR TRUE. *)
            IF (abspos#pos) OR TRUE
            THEN
               FlushBuffer(f) ;
               IF buffer#NIL
               THEN
                  WITH buffer^ DO
                     IF output
                     THEN
                        left := size
                     ELSE
                        left := 0
                     END ;
                     position := 0 ;
                     filled   := 0
                  END
               END ;
               offset := lseek (unixfd, VAL (CSSIZE_T, pos), SeekSet ()) ;
               IF (offset>=0) AND (pos=offset)
               THEN
                  abspos := pos
               ELSE
                  state  := failed ;
                  abspos := 0
               END ;
               IF buffer#NIL
               THEN
                  buffer^.valid := FALSE ;
                  buffer^.bufstart := abspos
               END
            END
         END
      END
   END
END SetPositionFromBeginning ;


(*
   SetPositionFromEnd - sets the position from the end of the file.
*)

PROCEDURE SetPositionFromEnd (f: File; pos: LONGINT) ;
VAR
   offset: LONGINT ;
   fd    : FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            FlushBuffer(f) ;
            IF buffer#NIL
            THEN
               WITH buffer^ DO
                  IF output
                  THEN
                     left := size
                  ELSE
                     left := 0
                  END ;
                  position := 0 ;
                  filled   := 0
               END
            END ;
            offset := lseek (unixfd, VAL (CSSIZE_T, pos), SeekEnd ()) ;
            IF offset>=0
            THEN
               abspos := offset ;
            ELSE
               state  := failed ;
               abspos := 0 ;
               offset := 0
            END ;
            IF buffer#NIL
            THEN
               buffer^.valid := FALSE ;
               buffer^.bufstart := offset
            END
         END
      END
   END
END SetPositionFromEnd ;


(*
   FindPosition - returns the current absolute position in file, f.
*)

PROCEDURE FindPosition (f: File) : LONGINT ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd#NIL
      THEN
         WITH fd^ DO
            IF (buffer=NIL) OR (NOT buffer^.valid)
            THEN
               RETURN( abspos )
            ELSE
               WITH buffer^ DO
                  RETURN( bufstart+VAL(LONGINT, position) )
               END
            END
         END
      END
   END ;
   RETURN( 0 )
END FindPosition ;


(*
   GetFileName - assigns, a, with the filename associated with, f.
*)

PROCEDURE GetFileName (f: File; VAR a: ARRAY OF CHAR) ;
VAR
   i : CARDINAL ;
   p : POINTER TO CHAR ;
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd=NIL
      THEN
         FormatError('this file has probably been closed and not reopened successfully or alternatively never opened\n') ;
         HALT
      ELSE
         WITH fd^.name DO
            IF address=NIL
            THEN
               StrCopy('', a)
            ELSE
               p := address ;
               i := 0 ;
               WHILE (p^#nul) AND (i<=HIGH(a)) DO
                  a[i] := p^ ;
                  INC(p) ;
                  INC(i)
               END
            END
         END
      END
   END
END GetFileName ;


(*
   getFileName - returns the address of the filename associated with, f.
*)

PROCEDURE getFileName (f: File) : ADDRESS ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd=NIL
      THEN
         FormatError('this file has probably been closed and not reopened successfully or alternatively never opened\n') ;
         HALT
      ELSE
         RETURN fd^.name.address
      END
   END ;
   RETURN NIL
END getFileName ;


(*
   getFileNameLength - returns the number of characters associated with filename, f.
*)

PROCEDURE getFileNameLength (f: File) : CARDINAL ;
VAR
   fd: FileDescriptor ;
BEGIN
   IF f#Error
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF fd=NIL
      THEN
         FormatError('this file has probably been closed and not reopened successfully or alternatively never opened\n') ;
         HALT
      ELSE
         RETURN fd^.name.size
      END
   END ;
   RETURN 0
END getFileNameLength ;


(*
   PreInitialize - preinitialize the file descriptor.
*)

PROCEDURE PreInitialize (f: File; fname: ARRAY OF CHAR;
                         state: FileStatus; use: FileUsage;
                         towrite: BOOLEAN; osfd: INTEGER; bufsize: CARDINAL) ;
VAR
   fd, fe: FileDescriptor ;
BEGIN
   IF InitializeFile(f, ADR(fname), StrLen(fname), state, use, towrite, bufsize)=f
   THEN
      fd := GetIndice(FileInfo, f) ;
      IF f=Error
      THEN
         fe := GetIndice(FileInfo, StdErr) ;
         IF fe=NIL
         THEN
            HALT
         ELSE
            fd^.unixfd := fe^.unixfd    (* the error channel *)
         END
      ELSE
         fd^.unixfd := osfd
      END
   ELSE
      HALT
   END
END PreInitialize ;


(*
   FlushOutErr - flushes, StdOut, and, StdErr.
                 It is also called when the application calls M2RTS.Terminate.
                 (which is automatically placed in program modules by the GM2
                 scaffold).
*)

PROCEDURE FlushOutErr ;
BEGIN
   IF IsNoError(StdOut)
   THEN
      FlushBuffer(StdOut)
   END ;
   IF IsNoError(StdErr)
   THEN
      FlushBuffer(StdErr)
   END
END FlushOutErr ;


(*
   Init - initialize the modules, global variables.
*)

PROCEDURE Init ;
BEGIN
   FileInfo := InitIndex(0) ;
   Error := 0 ;
   PreInitialize(Error       , 'error'   , toomanyfilesopen, unused        , FALSE, -1, 0) ;
   StdIn := 1 ;
   PreInitialize(StdIn       , '<stdin>' , successful      , openedforread , FALSE, 0, MaxBufferLength) ;
   StdOut := 2 ;
   PreInitialize(StdOut      , '<stdout>', successful      , openedforwrite,  TRUE, 1, MaxBufferLength) ;
   StdErr := 3 ;
   PreInitialize(StdErr      , '<stderr>', successful      , openedforwrite,  TRUE, 2, MaxBufferLength) ;
   IF NOT InstallTerminationProcedure(FlushOutErr)
   THEN
      HALT
   END
END Init ;


BEGIN
   Init
FINALLY
   FlushOutErr
END FIO.
