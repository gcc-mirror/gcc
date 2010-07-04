------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - B                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2010, Free Software Foundation, Inc.         --
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
--  in the GNAT binder for command line processing and file input output.

package Osint.B is

   function More_Lib_Files return Boolean;
   --  Indicates whether more library information files remain to be processed.
   --  Returns False right away if no source files, or if all source files
   --  have been processed.

   function Next_Main_Lib_File return File_Name_Type;
   --  This function returns the name of the next library info file specified
   --  on the command line. It is an error to call Next_Main_Lib_File if no
   --  more library information files exist (i.e. Next_Main_Lib_File may be
   --  called only if a previous call to More_Lib_Files returned True). This
   --  name is the simple name, excluding any directory information.

   -------------------
   -- Binder Output --
   -------------------

   --  These routines are used by the binder to generate the C or Ada source
   --  files containing the binder output. The format of these files is
   --  described in package Bindgen.

   procedure Create_Binder_Output
     (Output_File_Name : String;
      Typ              : Character;
      Bfile            : out Name_Id);
   --  Creates the binder output file. Typ is one of
   --
   --    'c'   create output file for case of generating C
   --    'b'   create body file for case of generating Ada
   --    's'   create spec file for case of generating Ada
   --
   --  If Output_File_Name is null, then a default name is used based on
   --  the name of the most recently accessed main source file name. If
   --  Output_File_Name is non-null then it is the full path name of the
   --  file to be output (in the case of Ada, it must have an extension
   --  of adb, and the spec file is created by changing the last character
   --  from b to s. On return, Bfile also contains the Name_Id for the
   --  generated file name.

   procedure Write_Binder_Info (Info : String);
   --  Writes the contents of the referenced string to the binder output file
   --  created by a previous call to Create_Binder_Output. Info represents a
   --  single line in the file, but does not contain any line termination
   --  characters. The implementation of Write_Binder_Info is responsible
   --  for adding necessary end of line and end of file control characters
   --  as required by the operating system.

   procedure Close_Binder_Output;
   --  Closes the file created by Create_Binder_Output, flushing any
   --  buffers etc. from writes by Write_Binder_Info.

   procedure Set_Current_File_Name_Index (To : Int);
   --  Set value of Current_File_Name_Index (in private part of Osint) to To

   ----------------------------------
   -- Other binder-generated files --
   ----------------------------------

   procedure Set_List_File (Filename : String);
   --  Create Filename as a text output file and set it as the current output
   --  (see Output.Set_Output).

   procedure Close_List_File;
   --  If a specific output file was created by Set_List_File, close it and
   --  reset the current output file to standard output.

end Osint.B;
