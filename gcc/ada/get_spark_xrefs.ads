------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G E T _ S P A R K _ X R E F S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2013, Free Software Foundation, Inc.         --
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

--  This package contains the function used to read SPARK cross-reference
--  information from an ALI file and populate the tables defined in package
--  SPARK_Xrefs with the result.

generic
   --  These subprograms provide access to the ALI file. Locating, opening and
   --  providing access to the ALI file is the callers' responsibility.

   with function Getc return Character is <>;
   --  Get next character, positioning the ALI file ready to read the following
   --  character (equivalent to calling Nextc, then Skipc). If the end of file
   --  is encountered, the value Types.EOF is returned.

   with function Nextc return Character is <>;
   --  Look at the next character, and return it, leaving the position of the
   --  file unchanged, so that a subsequent call to Getc or Nextc will return
   --  this same character. If the file is positioned at the end of file, then
   --  Types.EOF is returned.

   with procedure Skipc is <>;
   --  Skip past the current character (which typically was read with Nextc),
   --  and position to the next character, which will be returned by the next
   --  call to Getc or Nextc.

procedure Get_SPARK_Xrefs;
--  Load SPARK cross-reference information from ALI file text format into
--  internal SPARK tables (SPARK_Xrefs.SPARK_Xref_Table,
--  SPARK_Xrefs.SPARK_Scope_Table and SPARK_Xrefs.SPARK_File_Table). On entry
--  the input file is positioned to the initial 'F' of the first SPARK specific
--  line in the ALI file. On return, the file is positioned either to the end
--  of file, or to the first character of the line following the SPARK specific
--  information (which will never start with an 'F').
--
--  If a format error is detected in the input, then an exception is raised
--  (Ada.IO_Exceptions.Data_Error), with the file positioned to the error.
