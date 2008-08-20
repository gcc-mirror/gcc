------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              O S I N T - C                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2008, Free Software Foundation, Inc.         --
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
--  in the GNAT compiler for command line processing and file input output.

package Osint.C is

   procedure Set_Output_Object_File_Name (Name : String);
   --  Called by the subprogram processing the command line when an
   --  output object file name is found.

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

   ------------------------------
   -- Debug Source File Output --
   ------------------------------

   --  These routines are used by the compiler to generate the debug source
   --  file for the Debug_Generated_Code (-gnatD switch) option. Note that
   --  debug source file writing occurs at a completely different point in
   --  the processing from library information output, or representation
   --  output, so the code in the body can assume that no two of these
   --  functions are ever used at the same time.

   function Create_Debug_File (Src : File_Name_Type) return File_Name_Type;
   --  Given the simple name of a source file, this routine creates the
   --  corresponding debug file, and returns its full name.

   procedure Write_Debug_Info (Info : String);
   --  Writes contents of given string as next line of the current debug
   --  source file created by the most recent call to Create_Debug_File.
   --  Info does not contain end of line or other formatting characters.

   procedure Close_Debug_File;
   --  Close current debug file created by the most recent call to
   --  Create_Debug_File.

   function Debug_File_Eol_Length return Nat;
   --  Returns the number of characters (1 for NL, 2 for CR/LF) written
   --  at the end of each line by Write_Debug_Info.

   --------------------------------
   -- Representation File Output --
   --------------------------------

   --  These routines are used by the compiler to generate the representation
   --  information to a file if this option is specified (-gnatR?s switch).
   --  Note that the writing of this file occurs at a completely different
   --  point in the processing from library information output, or from
   --  debug file output, so the code in the body can assume that no two
   --  of these functions are ever used at the same time.

   --  Note: these routines are called from Repinfo, but are not called
   --  directly, since we do not want Repinfo to depend on Osint. That
   --  would cause a lot of unwanted junk to be dragged into ASIS. So
   --  what we do is we have Initialize set the addresses of these three
   --  procedures in appropriate variables in Repinfo, so that they can
   --  be called indirectly without creating a dependence.

   procedure Create_Repinfo_File (Src : String);
   --  Given the simple name of a source file, this routine creates the
   --  corresponding file to hold representation information. Note that the
   --  call destroys the contents of Name_Buffer and Name_Len.

   procedure Write_Repinfo_Line (Info : String);
   --  Writes contents of given string as next line of the current debug
   --  source file created by the most recent call to Create_Repinfo_File.
   --  Info does not contain end of line or other formatting characters.

   procedure Close_Repinfo_File;
   --  Close current debug file created by the most recent call to
   --  Create_Repinfo_File.

   --------------------------------
   -- Library Information Output --
   --------------------------------

   --  These routines are used by the compiler to generate the library
   --  information file for the main source file being compiled. See section
   --  above for a discussion of how library information files are stored.

   procedure Create_Output_Library_Info;
   --  Creates the output library information file for the source file which
   --  is currently being compiled (i.e. the file which was most recently
   --  returned by Next_Main_Source).

   procedure Write_Library_Info (Info : String);
   --  Writes the contents of the referenced string to the library information
   --  file for the main source file currently being compiled (i.e. the file
   --  which was most recently opened with a call to Read_Next_File). Info
   --  represents a single line in the file, but does not contain any line
   --  termination characters. The implementation of Write_Library_Info is
   --  responsible for adding necessary end of line and end of file control
   --  characters to the generated file.

   procedure Close_Output_Library_Info;
   --  Closes the file created by Create_Output_Library_Info, flushing any
   --  buffers etc. from writes by Write_Library_Info.

   procedure Read_Library_Info
     (Name : out File_Name_Type;
      Text : out Text_Buffer_Ptr);
   --  The procedure version of Read_Library_Info is used from the compiler
   --  to read an existing ali file associated with the main unit. If the
   --  ALI file exists, then its file name is returned in Name, and its
   --  text is returned in Text. If the file does not exist, then Text is
   --  set to null.

   ----------------------
   -- List File Output --
   ----------------------

   procedure Create_List_File (S : String);
   --  Creates the file whose name is given by S. If the name starts with a
   --  period, then the name is xxx & S, where xxx is the name of the main
   --  source file without the extension stripped. Information is written to
   --  this file using Write_List_File.

   procedure Write_List_Info (S : String);
   --  Writes given string to the list file created by Create_List_File

   procedure Close_List_File;
   --  Close file previously opened by Create_List_File

   --------------------------------
   -- Semantic Tree Input-Output --
   --------------------------------

   procedure Tree_Create;
   --  Creates the tree output file for the source file which is currently
   --  being compiled (i.e. the file which was most recently returned by
   --  Next_Main_Source), and initializes Tree_IO.Tree_Write for output.

   procedure Tree_Close;
   --  Closes the file previously opened by Tree_Create

end Osint.C;
