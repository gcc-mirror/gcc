(* InOut.mod provides a compatible PIM [234] InOut module.

Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE InOut ;

IMPORT FIO, SFIO, Terminal ;
FROM FIO IMPORT File, StdIn, StdOut ;

FROM DynamicStrings IMPORT InitString, Mark, KillString, ConCat,
                           RemoveWhitePrefix, char, ConCatChar, Length ;

FROM StringConvert IMPORT CardinalToString, stoc, stoi, ctos, itos ;
FROM ASCII IMPORT nul ;
FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT read, write ;
FROM Termbase IMPORT AssignRead, AssignWrite ;
IMPORT Keyboard ;


CONST
   stdin = 0 ;
   stdout = 1 ;

TYPE
   CharSet = SET OF CHAR ;

VAR
   inFile, outFile: File ;
   inUsed, outUsed: BOOLEAN ;


(*
   OpenInput - reads a string from stdin as the filename for reading.
               If the filename ends with `.' then it appends the defext
               extension. The global variable Done is set if all
               was successful.
*)

PROCEDURE OpenInput (defext: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := ReadS() ;
   IF char(s, -1)='.'
   THEN
      s := ConCat(s, Mark(InitString(defext)))
   END ;
   IF SFIO.Exists(s)
   THEN
      inFile := SFIO.OpenToRead(s) ;
      Done := FIO.IsNoError(inFile) ;
      inUsed := TRUE
   ELSE
      Done := FALSE ;
      inUsed := FALSE
   END ;
   s := KillString(s)
END OpenInput ;


(*
   CloseInput - closes an opened input file and returns input back to
                StdIn.
*)

PROCEDURE CloseInput ;
BEGIN
   IF inUsed
   THEN
      FIO.Close(inFile) ;
      inFile := StdIn ;
      inUsed := FALSE
   END
END CloseInput ;


(*
   OpenOutput - reads a string from stdin as the filename for writing.
                If the filename ends with `.' then it appends the defext
                extension. The global variable Done is set if all
                was successful.
*)

PROCEDURE OpenOutput (defext: ARRAY OF CHAR) ;
VAR
   s: String ;
BEGIN
   s := ReadS() ;
   IF char(s, -1)='.'
   THEN
      s := ConCat(s, Mark(InitString(defext)))
   END ;
   IF SFIO.Exists(s)
   THEN
      outFile := SFIO.OpenToWrite(s) ;
      Done := FIO.IsNoError(outFile) ;
      outUsed := TRUE
   ELSE
      Done := FALSE ;
      outUsed := FALSE
   END ;
   s := KillString(s)
END OpenOutput ;


(*
   CloseOutput - closes an opened output file and returns output back to
                 StdOut.
*)

PROCEDURE CloseOutput ;
BEGIN
   IF outUsed
   THEN
      FIO.Close(outFile) ;
      outFile := StdOut ;
      outUsed := FALSE
   END
END CloseOutput ;


(*
   LocalRead -
*)

PROCEDURE LocalRead (VAR ch: CHAR) ;
BEGIN
   ch := FIO.ReadChar(inFile) ;
   Done := FIO.IsNoError(inFile) AND (NOT FIO.EOF(inFile))
END LocalRead ;


(*
   LocalStatus - returns TRUE if more characters may be read.
*)

PROCEDURE LocalStatus () : BOOLEAN ;
BEGIN
   IF inUsed
   THEN
      RETURN Done
   ELSE
      RETURN Keyboard.KeyPressed ()
   END
END LocalStatus ;


(*
   ReadS - returns a string which has is a sequence of characters.
           Leading white space is ignored and string is terminated
           with a character <= ' '.
*)

PROCEDURE ReadS () : String ;
VAR
   s : String ;
   ch: CHAR ;
BEGIN
   s := InitString('') ;
   REPEAT
      Read(ch)
   UNTIL ch>' ' ;
   WHILE ch>' ' DO
      s := ConCatChar(s, ch) ;
      Read(ch)
   END ;
   (* successful *)
   RETURN( s )
END ReadS ;


(*
   Read - reads a single character from the current input file.
          Done is set to FALSE if end of file is reached or an
          error occurs.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   Terminal.Read(ch)
END Read ;


(*
   ReadString - reads a sequence of characters. Leading white space
                is ignored and the string is terminated with a character
                <= ' '
*)

PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;
VAR
   h, i: CARDINAL ;
BEGIN
   (* skip leading spaces *)
   REPEAT
      Read(termCH)
   UNTIL termCH>' ' ;
   s[0] := termCH ;
   i := 1 ;
   h := HIGH(s) ;
   IF i<=h
   THEN
      REPEAT
         Read(termCH) ;
         IF termCH<=' '
         THEN
            s[i] := nul ;
            (* successful *)
            RETURN
         END ;
         s[i] := termCH ;
         INC(i)
      UNTIL i>h ;
   END ;
   Done := FALSE   (* out of space *)
END ReadString ;


(*
   WriteString - writes a string to the output file.
*)

PROCEDURE WriteString (s: ARRAY OF CHAR) ;
BEGIN
   FIO.WriteString(outFile, s) ;
   Done := FIO.IsNoError(outFile)
END WriteString ;


(*
   LocalWrite -
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   FIO.WriteChar(outFile, ch) ;
   Done := FIO.IsNoError(outFile) ;
   FIO.FlushBuffer (outFile)
END LocalWrite ;


(*
   Write - writes out a single character, ch, to the current output file.
*)

PROCEDURE Write (ch: CHAR) ;
BEGIN
   Terminal.Write(ch)
END Write ;


(*
   WriteS - writes a String to the output device.
            It returns the string, s.
*)

PROCEDURE WriteS (s: String) : String ;
VAR
   i, h: CARDINAL ;
BEGIN
   i := 0 ;
   h := Length(s) ;
   WHILE i<h DO
      Write(char(s, i)) ;
      INC(i)
   END ;
   RETURN( s )
END WriteS ;


(*
   WriteLn - writes a newline to the output file.
*)

PROCEDURE WriteLn ;
BEGIN
   IF outUsed
   THEN
      FIO.WriteLine(outFile) ;
      Done := FIO.IsNoError(outFile)
   ELSE
      Terminal.WriteLn
   END
END WriteLn ;


(*
   ReadInt - reads a string and converts it into an INTEGER, x.
             Done is set if an INTEGER is read.
*)

PROCEDURE ReadInt (VAR x: INTEGER) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(ReadS()) ;
   IF char(s, 0) IN CharSet{'-', '+', '0'..'9'}
   THEN
      x := stoi(s) ;
      Done := TRUE
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadInt ;


(*
   ReadInt - reads a string and converts it into an INTEGER, x.
             Done is set if an INTEGER is read.
*)

PROCEDURE ReadCard (VAR x: CARDINAL) ;
VAR
   s: String ;
BEGIN
   s := RemoveWhitePrefix(ReadS()) ;
   IF char(s, 0) IN CharSet{'+', '0'..'9'}
   THEN
      x := stoc(s) ;
      Done := TRUE
   ELSE
      Done := FALSE
   END ;
   s := KillString(s)
END ReadCard ;


(*
   WriteCard - writes the CARDINAL, x, to the output file. It ensures
               that the number occupies, n, characters. Leading spaces
               are added if required.
*)

PROCEDURE WriteCard (x, n: CARDINAL) ;
BEGIN
   IF KillString(SFIO.WriteS(outFile, ctos(x, n, ' ')))=NIL
   THEN
   END
END WriteCard ;


(*
   WriteInt - writes the INTEGER, x, to the output file. It ensures
              that the number occupies, n, characters. Leading spaces
              are added if required.
*)

PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) ;
BEGIN
   IF KillString(SFIO.WriteS(outFile, itos(x, n, ' ', FALSE)))=NIL
   THEN
   END
END WriteInt ;


(*
   WriteOct - writes the CARDINAL, x, to the output file in octal.
              It ensures that the number occupies, n, characters.
              Leading spaces are added if required.
*)

PROCEDURE WriteOct (x, n: CARDINAL) ;
BEGIN
   IF KillString(SFIO.WriteS(outFile, CardinalToString(x, n, ' ', 8, FALSE)))=NIL
   THEN
   END
END WriteOct ;


(*
   WriteHex - writes the CARDINAL, x, to the output file in hexadecimal.
              It ensures that the number occupies, n, characters.
              Leading spaces are added if required.
*)

PROCEDURE WriteHex (x, n: CARDINAL) ;
BEGIN
   IF KillString(SFIO.WriteS(outFile, CardinalToString(x, n, ' ', 16, TRUE)))=NIL
   THEN
   END
END WriteHex ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   inFile := FIO.StdIn ;
   outFile := FIO.StdOut ;
   inUsed := FALSE ;
   outUsed := FALSE ;
   AssignRead(LocalRead, LocalStatus, Done) ;
   AssignWrite(LocalWrite, Done)
END Init ;


BEGIN
   Init
END InOut.
