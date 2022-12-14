(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE varparam3 ;

   TYPE
      InputModes = BITSET;
      OutputModes = BITSET;
      ControlModes = BITSET;
      LineModes = BITSET;
      TermIO =
	 RECORD
	    inputmodes: InputModes;
	    outputmodes: OutputModes;
	    controlmodes: ControlModes;
	    linemodes: LineModes;
	    linedisc: CHAR;
	 END;

      CTermIO =
	 RECORD
	    iflag1, iflag2: CHAR;
	    oflag1, oflag2: CHAR;
	    cflag1, cflag2: CHAR;
	    lflag1, lflag2: CHAR;
	    line: CHAR;
	    c1, c2, c3, c4, c5, c6, c7, c8: CHAR;
	 END;

   PROCEDURE GetTermIO(fd: CARDINAL; VAR termio: TermIO) : BOOLEAN;
      VAR
	 ctermio: CTermIO;

      PROCEDURE Convert(flag1, flag2: CHAR; VAR bs: BITSET);
      BEGIN
	 bs := BITSET( ORD(flag1)*100H + ORD(flag2) );
      END Convert;

   BEGIN
      WITH termio DO 
         WITH ctermio DO
	    Convert(iflag1, iflag2, inputmodes);
	 END;
      END;
      RETURN TRUE
   END GetTermIO;

VAR
   t: TermIO ;
BEGIN
   IF GetTermIO(0, t)
   THEN
   END
END varparam3.
