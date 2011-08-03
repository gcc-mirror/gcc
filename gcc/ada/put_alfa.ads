------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P U T _ A L F A                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the function used to read ALFA information from the
--  internal tables defined in package ALFA, and output text information for
--  the ALI file. The interface allows control over the destination of the
--  output, so that this routine can also be used for debugging purposes.

with Types; use Types;

generic
   --  The following procedures are used to output text information. The
   --  destination of the text information is thus under control of the
   --  particular instantiation. In particular, this procedure is used to
   --  write output to the ALI file, and also for debugging output.

   with function Write_Info_Col return Positive is <>;
   --  Return the column in which the next character will be written

   with procedure Write_Info_Char (C : Character) is <>;
   --  Output one character

   with procedure Write_Info_Initiate (Key : Character) is <>;
   --  Initiate write of new line to output file, the parameter is the
   --  keyword character for the line.

   with procedure Write_Info_Nat (N : Nat) is <>;
   --  Write image of N to output file with no leading or trailing blanks

   with procedure Write_Info_Terminate is <>;
   --  Terminate current info line and output lines built in Info_Buffer

procedure Put_ALFA;
--  Read information from ALFA tables (ALFA.ALFA_Xref_Table,
--  ALFA.ALFA_Scope_Table and ALFA.ALFA_File_Table) and output corresponding
--  information in ALI format using the Write_Info procedures.
