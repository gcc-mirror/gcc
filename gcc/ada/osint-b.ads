------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - B                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001 Free Software Foundation, Inc.               --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the low level, operating system routines used only
--  in the GNAT binder for command line processing and file input output.

package Osint.B is

   procedure Record_Time_From_Last_Bind;
   --  Trigger the computing of the time from the last bind of the same
   --  program.

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

   function Time_From_Last_Bind return Nat;
   --  This function give an approximate number of minute from the last bind.
   --  It bases its computation on file stamp and therefore does gibe not
   --  any meaningful result before the new output binder file is written.
   --  So it returns Nat'last if:
   --
   --   - it is the first bind of this  specific program
   --   - Record_Time_From_Last_Bind was not Called first
   --   - Close_Binder_Output was not called first
   --
   --  otherwise it returns the number of minutes from the last bind. The
   --  computation does not try to be completely accurate and in particular
   --  does not take leap years into account.

   -------------------
   -- Binder Output --
   -------------------

   --  These routines are used by the binder to generate the C source file
   --  containing the binder output. The format of this file is described
   --  in the package Bindfmt.

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
   --  buffers etc from writes by Write_Binder_Info.

end Osint.B;
