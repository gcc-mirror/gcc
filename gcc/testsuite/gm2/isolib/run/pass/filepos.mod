(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE filepos;

FROM SYSTEM IMPORT ADR;
IMPORT RndFile, IOChan, ChanConsts, SWholeIO, STextIO;
FROM libc IMPORT exit ;

(*
   TestSeek - 
*)

PROCEDURE TestSeek (a: ARRAY OF CHAR; n: CARDINAL) ;
VAR
   pos: CARDINAL;
BEGIN
   RndFile.IOChan.RawWrite(c, ADR(a), LENGTH(a));
   pos := RndFile.CurrentPos(c);
   SWholeIO.WriteCard(pos,1); STextIO.WriteLn;
   IF pos#n
   THEN
      exit(1)
   END
END TestSeek ;


VAR
  c   : IOChan.ChanId;
  res : ChanConsts.OpenResults;
BEGIN
   RndFile.OpenClean(c, "test.txt", RndFile.write, res);
   IF res=ChanConsts.opened
   THEN
      TestSeek('a', 1) ;
      TestSeek('bc', 3) ;
      TestSeek('def', 6) ;
      TestSeek('ghijklmnopqrstuvwxyz', 26)
   END ;
   RndFile.Close(c)
END filepos.
