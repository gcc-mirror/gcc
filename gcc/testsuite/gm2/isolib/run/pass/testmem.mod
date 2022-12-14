(* Copyright (C) 2011 Free Software Foundation, Inc. *)
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
Boston, MA 02110-1301, USA. *)

MODULE testmem ;

FROM MemStream IMPORT OpenRead, OpenWrite, Close ;
FROM ChanConsts IMPORT FlagSet, OpenResults, raw, write, read ;
FROM IOChan IMPORT ChanId ;
FROM SYSTEM IMPORT ADDRESS ;
FROM libc IMPORT printf ;

IMPORT RawIO ;

CONST
   Amount = 1000 ;

VAR
   fd    : ChanId ;
   res   : OpenResults ;
   start : ADDRESS ;
   length: CARDINAL ;
   used  : CARDINAL ;
   i,
   value : CARDINAL ;
BEGIN
   OpenWrite (fd, raw+write, res, start, length, used, FALSE) ;
   IF res=opened
   THEN
      FOR i := 1 TO Amount DO
         RawIO.Write (fd, i)
      END ;
      Close(fd) ;
      printf ("buffer at %p has length 0x%x bytes and 0x%x are used\n",
              start, length, used);
      IF used#SIZE(CARDINAL)*Amount
      THEN
         HALT
      END ;
      OpenRead (fd, raw+read, res, start, length, TRUE) ;
      FOR i := 1 TO Amount DO
         RawIO.Read (fd, value) ;
         IF i#value
         THEN
            HALT
         END
      END ;
      printf ("read the contents of the complete buffer successfully");
   ELSE
      HALT
   END
END testmem.
