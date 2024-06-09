------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2024, Free Software Foundation, Inc.         --
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

--  This package contains the low level, operating system routines used only
--  in gnatmake for command line processing and file input output.

package Osint.M is

   function More_Source_Files return Boolean;
   --  Indicates whether more source file remain to be processed. Returns
   --  False right away if no source files, or if all source files have
   --  been processed.

   function Next_Main_Source return File_Name_Type;
   --  This function returns the name of the next main source file specified
   --  on the command line. It is an error to call Next_Main_Source if no more
   --  source files exist (i.e. Next_Main_Source may be called only if a
   --  previous call to More_Source_Files returned True). This name is the
   --  simple file name (without any directory information).

   function Object_File_Name (N : File_Name_Type) return File_Name_Type;
   --  Constructs the name of the object file corresponding to library
   --  file N. If N is a full file name than the returned file name will
   --  also be a full file name. Note that no lookup in the library file
   --  directories is done for this file. This routine merely constructs
   --  the name.

end Osint.M;
