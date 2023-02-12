(* FileSystem.mod provides a PIM [234] FileSystem module.

Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE FileSystem ;

FROM M2RTS IMPORT InstallTerminationProcedure ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT WORD, BYTE, ADDRESS, ADR ;
IMPORT FIO, SFIO, libc, wrapc ;
FROM DynamicStrings IMPORT String, InitString, ConCat, ConCatChar, KillString, string ;
FROM FormatStrings IMPORT Sprintf2 ;

CONST
   TMPDIR   = '/tmp' ;
   DIRSEP   = '/' ;
   SEEK_SET = 0 ;   (* seek relative to from beginning of the file *)

TYPE
   FileList = POINTER TO RECORD
                            next     : FileList ;
                            n        : String ;
                            stillTemp: BOOLEAN ;
                         END ;

VAR
   HeadOfTemp: FileList ;
   tempNo    : CARDINAL ;


(*
   Create - creates a temporary file. To make the file perminant
            the file must be renamed.
*)

PROCEDURE Create (VAR f: File) ;
BEGIN
   WITH f DO
      flags := FlagSet{write, temporary} ;
      eof := FALSE ;
      lastWord := WORD(0) ;
      lastByte := CHAR(0) ;
      name := MakeTemporary() ;
      fio := SFIO.OpenToWrite(name) ;
      IF FIO.IsNoError(fio)
      THEN
         res := done
      ELSE
         res := notdone
      END ;
      highpos := 0 ;
      lowpos := 0
   END
END Create ;


(*
   Close - closes an open file.
*)

PROCEDURE Close (f: File) ;
BEGIN
   WITH f DO
      eof := TRUE ;
      FIO.Close(fio) ;
      IF FIO.IsNoError(fio)
      THEN
         res := done
      ELSE
         res := notdone
      END ;
      IF temporary IN flags
      THEN
         deleteFile(name, f)
      END ;
      name := KillString(name)
   END
END Close ;


(*
   Lookup - looks for a file, filename. If the file is found
            then, f, is opened. If it is not found and, newFile,
            is TRUE then a new file is created and attached to, f.
            If, newFile, is FALSE and no file was found then f.res
            is set to notdone.
*)

PROCEDURE Lookup (VAR f: File; filename: ARRAY OF CHAR; newFile: BOOLEAN) ;
BEGIN
   WITH f DO
      flags := FlagSet{} ;
      IF FIO.Exists(filename)
      THEN
         fio := FIO.OpenToRead(filename) ;
         INCL(flags, read) ;
         res := done
      ELSIF newFile
      THEN
         fio := FIO.OpenToWrite(filename) ;
         INCL(flags, write) ;
         res := done
      ELSE
         res := notdone
      END ;
      name := InitString(filename) ;
      eof := FALSE ;
      highpos := 0 ;
      lowpos := 0
   END
END Lookup ;


(*
   Rename - rename a file and change a temporary file to a permanent
            file. f.res is set appropriately.
*)

PROCEDURE Rename (VAR f: File; newname: ARRAY OF CHAR) ;
VAR
   s: String ;
   r: INTEGER ;
BEGIN
   s := InitString(newname) ;
   WITH f DO
      r := libc.rename(string(name), string(s)) ;
      IF r=0
      THEN
         res := done
      ELSE
         res := notdone
      END ;
      EXCL(flags, temporary) ;
      name := KillString(name) ;
      name := s
   END
END Rename ;


(*
   deleteFile - deletes file, name. It also kills the string, name.
*)

PROCEDURE deleteFile (VAR name: String; VAR f: File) ;
VAR
   r: INTEGER ;
BEGIN
   r := libc.unlink(string(name)) ;
   IF r=0
   THEN
      f.res := done
   ELSE
      f.res := notdone
   END ;
   name := KillString(name) ;
   name := NIL
END deleteFile ;


(*
   Delete - deletes a file, name, and sets the f.res field.
            f.res is set appropriately.
*)

PROCEDURE Delete (name: ARRAY OF CHAR; VAR f: File) ;
VAR
   s: String ;
BEGIN
   s := InitString(name) ;
   deleteFile(s, f) ;
   s := KillString(s)
END Delete ;


(*
   ReadWord - reads a WORD, w, from file, f.
              f.res is set appropriately.
*)

PROCEDURE ReadWord (VAR f: File; VAR w: WORD) ;
VAR
   n: CARDINAL ;
BEGIN
   WITH f DO
      IF again IN flags
      THEN
         w := lastWord ;
         EXCL(flags, again)
      ELSE
         ReadNBytes(f, ADR(w), SIZE(w), n) ;
         IF n=SIZE(w)
         THEN
            res := done
         ELSE
            res := notdone ;
            eof := TRUE
         END
      END
   END
END ReadWord ;


(*
   WriteWord - writes one word to a file, f.
               f.res is set appropriately.
*)

PROCEDURE WriteWord (VAR f: File; w: WORD) ;
VAR
   n: CARDINAL ;
BEGIN
   WriteNBytes(f, ADR(w), SIZE(w), n) ;
   WITH f DO
      IF n=SIZE(w)
      THEN
         res := done
      ELSE
         res := notdone
      END
   END
END WriteWord ;


(*
   ReadChar - reads one character from a file, f.
*)

PROCEDURE ReadChar (VAR f: File; VAR ch: CHAR) ;
VAR
   n: CARDINAL ;
BEGIN
   WITH f DO
      IF again IN flags
      THEN
         ch := CHAR(lastByte) ;
         EXCL(flags, again)
      ELSE
         ReadNBytes(f, ADR(ch), SIZE(ch), n) ;
         IF n=SIZE(ch)
         THEN
            res := done ;
            lastByte := BYTE(ch)
         ELSE
            res := notdone ;
            eof := TRUE
         END
      END
   END
END ReadChar ;


(*
   WriteChar - writes a character, ch, to a file, f.
               f.res is set appropriately.
*)

PROCEDURE WriteChar (VAR f: File; ch: CHAR) ;
VAR
   n: CARDINAL ;
BEGIN
   WriteNBytes(f, ADR(ch), SIZE(ch), n) ;
   WITH f DO
      IF n=SIZE(ch)
      THEN
         res := done
      ELSE
         res := notdone
      END
   END
END WriteChar ;


(*
   ReadByte - reads a BYTE, b, from file, f.
              f.res is set appropriately.
*)

PROCEDURE ReadByte (VAR f: File; VAR b: BYTE) ;
VAR
   n: CARDINAL ;
BEGIN
   WITH f DO
      IF again IN flags
      THEN
         b := lastByte ;
         EXCL(flags, again)
      ELSE
         ReadNBytes(f, ADR(b), SIZE(b), n) ;
         IF n=SIZE(b)
         THEN
            res := done ;
            lastByte := b
         ELSE
            res := notdone ;
            eof := TRUE
         END
      END
   END
END ReadByte ;


(*
   WriteByte - writes one BYTE, b, to a file, f.
               f.res is set appropriately.
*)

PROCEDURE WriteByte (VAR f: File; b: BYTE) ;
VAR
   n: CARDINAL ;
BEGIN
   WriteNBytes(f, ADR(b), SIZE(b), n) ;
   WITH f DO
      IF n=SIZE(b)
      THEN
         res := done
      ELSE
         res := notdone
      END
   END
END WriteByte ;


(*
   ReadNBytes - reads a sequence of bytes from a file, f.
*)

PROCEDURE ReadNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                      VAR actuallyRead: CARDINAL) ;
BEGIN
   WITH f DO
      IF amount>0
      THEN
         actuallyRead := FIO.ReadNBytes(fio, amount, a) ;
         IF FIO.IsNoError(fio)
         THEN
            res := done ;
            IF MAX(CARDINAL)-lowpos<actuallyRead
            THEN
               INC(highpos)
            END ;
            INC(lowpos, actuallyRead)
         ELSE
            res := notdone ;
            eof := TRUE
         END
      END
   END
END ReadNBytes ;


(*
   WriteNBytes - writes a sequence of bytes to file, f.
*)

PROCEDURE WriteNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                       VAR actuallyWritten: CARDINAL) ;
BEGIN
   actuallyWritten := 0 ;
   WITH f DO
      IF amount>0
      THEN
         actuallyWritten := FIO.WriteNBytes(fio, amount, a) ;
         IF FIO.IsNoError(fio)
         THEN
            res := done ;
            IF MAX(CARDINAL)-lowpos<actuallyWritten
            THEN
               INC(highpos)
            END ;
            INC(lowpos, actuallyWritten)
         ELSE
            res := notdone
         END
      END
   END
END WriteNBytes ;


(*
   Again - returns the last character read to the internal buffer
           so that it can be read again.
*)

PROCEDURE Again (VAR f: File) ;
BEGIN
   INCL(f.flags, again)
END Again ;


(*
   doModeChange -
*)

PROCEDURE doModeChange (VAR f: File; mode: Flag) ;
VAR
   r: INTEGER ;
BEGIN
   WITH f DO
      IF NOT (mode IN flags)
      THEN
         INCL(flags, mode) ;
         IF mode=read
         THEN
            EXCL(flags, write)
         ELSIF mode=write
         THEN
            EXCL(flags, read)
         END ;
         IF opened IN flags
         THEN
            FIO.Close(fio)
         END ;
         IF read IN flags
         THEN
            fio := SFIO.OpenToRead(name)
         ELSIF write IN flags
         THEN
            fio := SFIO.OpenToWrite(name)
         END ;
         INCL (flags, opened) ;
         r := libc.lseek (fio,
                          VAL (LONGINT, lowpos) + VAL (LONGINT, highpos) * VAL (LONGINT, MAX (CARDINAL)),
                          SEEK_SET)
      END
   END
END doModeChange ;


(*
   SetRead - puts the file, f, into the read state.
             The file position is unchanged.
*)

PROCEDURE SetRead (VAR f: File) ;
BEGIN
   doModeChange(f, read)
END SetRead ;


(*
   SetWrite - puts the file, f, into the write state.
              The file position is unchanged.
*)

PROCEDURE SetWrite (VAR f: File) ;
BEGIN
   doModeChange(f, write)
END SetWrite ;


(*
   SetModify - puts the file, f, into the modify state.
               The file position is unchanged but the file can be
               read and written.
*)

PROCEDURE SetModify (VAR f: File) ;
BEGIN
   doModeChange(f, modify)
END SetModify ;


(*
   SetOpen - places a file, f, into the open state. The file may
             have been in the read/write/modify state before and
             in which case the previous buffer contents are flushed
             and the file state is reset to open. The position is
             unaltered.
*)

PROCEDURE SetOpen (VAR f: File) ;
BEGIN
   doModeChange(f, opened)
END SetOpen ;


(*
   Reset - places a file, f, into the open state and reset the
           position to the start of the file.
*)

PROCEDURE Reset (VAR f: File) ;
BEGIN
   SetOpen(f) ;
   SetPos(f, 0, 0)
END Reset ;


(*
   SetPos - lseek to a position within a file.
*)

PROCEDURE SetPos (VAR f: File; high, low: CARDINAL) ;
VAR
   r: INTEGER ;
BEGIN
   WITH f DO
      r := libc.lseek(fio, VAL(LONGCARD, low) +
                      (VAL(LONGCARD, MAX(CARDINAL)) * VAL(LONGCARD, high)),
                      SEEK_SET) ;
      highpos := high ;
      lowpos := low ;
   END
END SetPos ;


(*
   GetPos - return the position within a file.
*)

PROCEDURE GetPos (VAR f: File; VAR high, low: CARDINAL) ;
BEGIN
   WITH f DO
      high := highpos ;
      low := lowpos
   END
END GetPos ;


(*
   Length - returns the length of file, in, high, and, low.
*)

PROCEDURE Length (VAR f: File; VAR high, low: CARDINAL) ;
VAR
   i: INTEGER ;
BEGIN
   WITH f DO
      i := wrapc.filesize(FIO.GetUnixFileDescriptor(fio), high, low)
   END
END Length ;


(*
   Doio - effectively flushes a file in write mode, rereads the
          current buffer from disk if in read mode and writes
          and rereads the buffer if in modify mode.
*)

PROCEDURE Doio (VAR f: File) ;
BEGIN
   WITH f DO
      IF opened IN flags
      THEN
         FIO.Close(fio) ;
         EXCL(flags, opened)
      END ;
      IF read IN flags
      THEN
         fio := SFIO.OpenToRead(name) ;
         INCL(flags, opened) ;
         SetPos(f, lowpos, highpos)
      ELSIF write IN flags
      THEN
         fio := SFIO.OpenToWrite(name) ;
         INCL(flags, opened) ;
         SetPos(f, lowpos, highpos)
      END
   END
END Doio ;


(*
   FileNameChar - checks to see whether the character, ch, is
                  legal in a filename. nul is returned if the
                  character was illegal.
*)

PROCEDURE FileNameChar (ch: CHAR) : CHAR ;
BEGIN
   RETURN ch
END FileNameChar ;


(*
   MakeTemporary - creates a temporary file and returns its name.
*)

PROCEDURE MakeTemporary () : String ;
VAR
   p: FileList ;
   i: INTEGER ;
BEGIN
   NEW(p) ;
   INC(tempNo) ;
   i := libc.getpid() ;
   WITH p^ DO
      next := HeadOfTemp ;
      n    := Sprintf2(InitString('fs-%d-%d'), i, tempNo) ;
      n    := ConCat(ConCatChar(InitString(TMPDIR), DIRSEP), n) ;
      RETURN n
   END
END MakeTemporary ;


(*
   CleanUp - deletes all temporary files.
*)

PROCEDURE CleanUp ;
VAR
   p: FileList ;
   r: INTEGER ;
BEGIN
   p := HeadOfTemp ;
   WHILE p#NIL DO
      WITH p^ DO
         IF stillTemp
         THEN
            stillTemp := FALSE ;
            r := libc.unlink(string(n))
         END
      END ;
      p := p^.next
   END
END CleanUp ;


(*
   Init - installs the termination procedure to tidy up any temporary files.
*)

PROCEDURE Init ;
BEGIN
   tempNo := 0 ;
   HeadOfTemp := NIL ;
   IF NOT InstallTerminationProcedure(CleanUp)
   THEN
      HALT
   END
END Init ;


BEGIN
   Init
END FileSystem.
