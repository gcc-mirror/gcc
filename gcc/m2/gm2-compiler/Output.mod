(* Output.mod redirect output.

Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE Output ;


IMPORT FIO, SFIO ;
FROM StrLib IMPORT StrEqual ;
FROM NameKey IMPORT KeyToCharStar, Name ;
FROM NumberIO IMPORT CardToStr ;
FROM ASCII IMPORT nl ;

FROM DynamicStrings IMPORT KillString, InitStringCharStar, ConCatChar,
                           ConCat, InitString, Mark ;


VAR
   stdout: BOOLEAN ;
   outputFile: FIO.File ;
   buffer    : String ;


(*
   Open - attempt to open filename as the output file.
          TRUE is returned if success, FALSE otherwise.
*)

PROCEDURE Open (filename: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF StrEqual (filename, "<stdout>") OR StrEqual (filename, "-")
   THEN
      outputFile := FIO.StdOut ;
      stdout := TRUE ;
      RETURN TRUE
   ELSE
      outputFile := FIO.OpenToWrite (filename) ;
      stdout := FALSE ;
      RETURN FIO.IsNoError (outputFile)
   END
END Open ;


(*
   Close - close the output file.
*)

PROCEDURE Close ;
BEGIN
   FIO.Close (outputFile)
END Close ;


(*
   Write - write a single character to the output file.
*)

PROCEDURE Write (ch: CHAR) ;
BEGIN
   IF buffer = NIL
   THEN
      FIO.WriteChar (outputFile, ch)
   ELSE
      buffer := ConCatChar (buffer, ch)
   END
END Write ;


(*
   WriteString - write an unformatted string to the output.
*)

PROCEDURE WriteString (s: ARRAY OF CHAR) ;
BEGIN
   IF buffer = NIL
   THEN
      FIO.WriteString (outputFile, s)
   ELSE
      buffer := ConCat (buffer, Mark (InitString (s)))
   END
END WriteString ;


(*
   KillWriteS - write a string to the output and free the string afterwards.
*)

PROCEDURE KillWriteS (s: String) ;
BEGIN
   IF KillString (SFIO.WriteS (outputFile, s)) = NIL
   THEN
   END
END KillWriteS ;


(*
   WriteS - write a string to the output.  The string is not freed.
*)

PROCEDURE WriteS (s: String) ;
BEGIN
   IF SFIO.WriteS (outputFile, s) = s
   THEN
   END
END WriteS ;


(*
   WriteKey - write a key to the output.
*)

PROCEDURE WriteKey (key: Name) ;
BEGIN
   IF buffer = NIL
   THEN
      KillWriteS (InitStringCharStar (KeyToCharStar (key)))
   ELSE
      buffer := ConCat (buffer, Mark (InitStringCharStar (KeyToCharStar (key))))
   END
END WriteKey ;


(*
   WriteLn - write a newline to the output.
*)

PROCEDURE WriteLn ;
BEGIN
   IF buffer = NIL
   THEN
      FIO.WriteLine (outputFile)
   ELSE
      Write (nl)
   END
END WriteLn ;


(*
   WriteCard - write a cardinal using fieldlength characters.
*)

PROCEDURE WriteCard (card, fieldlength: CARDINAL) ;
VAR
   s: ARRAY [0..20] OF CHAR ;
BEGIN
   CardToStr (card, fieldlength, s) ;
   WriteString (s)
END WriteCard ;


(*
   StartBuffer - create a buffer into which any output is redirected.
*)

PROCEDURE StartBuffer ;
BEGIN
   IF buffer # NIL
   THEN
      buffer := KillString (buffer)
   END ;
   buffer := InitString ('')
END StartBuffer ;


(*
   EndBuffer - end the redirection and return the contents of the buffer.
*)

PROCEDURE EndBuffer () : String ;
VAR
   s: String ;
BEGIN
   s := buffer ;
   buffer := NIL ;
   RETURN s
END EndBuffer ;


BEGIN
   stdout := TRUE ;
   buffer := NIL ;
   outputFile := FIO.StdOut ;
END Output.
