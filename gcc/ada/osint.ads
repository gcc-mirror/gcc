------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the low level, operating system routines used in
--  the GNAT compiler and binder for command line processing and file input
--  output. The specification is suitable for use with MS-DOS, Unix, and
--  similar systems. Note that for input source and library information
--  files, the line terminator may be either CR/LF or LF alone, and the
--  DOS-style EOF (16#1A#) character marking the end of the text in a
--  file may be used in all systems including Unix. This allows for more
--  convenient processing of DOS files in a Unix environment.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with System;      use System;
with Types;       use Types;

package Osint is

   procedure Set_Main_File_Name (Name : String);
   --  Set the main file name for Gnatmake.

   function Normalize_Directory_Name (Directory : String) return String_Ptr;
   --  Verify and normalize a directory name. If directory name is invalid,
   --  this will return an empty string. Otherwise it will insure a trailing
   --  slash and make other normalizations.

   type File_Type is (Source, Library, Config);

   function Find_File
     (N :    File_Name_Type;
      T :    File_Type)
      return File_Name_Type;
   --  Finds a source or library file depending on the value of T following
   --  the directory search order rules unless N is the name of the file
   --  just read with Next_Main_File and already contains directiory
   --  information, in which case just look in the Primary_Directory.
   --  Returns File_Name_Type of the full file name if found, No_File if
   --  file not found. Note that for the special case of gnat.adc, only the
   --  compilation environment directory is searched, i.e. the directory
   --  where the ali and object files are written. Another special case is
   --  when Debug_Generated_Code is set and the file name ends on ".dg",
   --  in which case we look for the generated file only in the current
   --  directory, since that is where it is always built.

   function Get_Switch_Character return Character;
   pragma Import (C, Get_Switch_Character, "__gnat_get_switch_character");
   Switch_Character : constant Character := Get_Switch_Character;
   --  Set to the default switch character (note that minus is always an
   --  acceptable alternative switch character)

   function Get_File_Names_Case_Sensitive return Int;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                    "__gnat_get_file_names_case_sensitive");
   File_Names_Case_Sensitive : constant Boolean :=
                               Get_File_Names_Case_Sensitive /= 0;
   --  Set to indicate whether the operating system convention is for file
   --  names to be case sensitive (e.g., in Unix, set True), or non case
   --  sensitive (e.g., in OS/2, set False).

   procedure Canonical_Case_File_Name (S : in out String);
   --  Given a file name, converts it to canonical case form. For systems
   --  where file names are case sensitive, this procedure has no effect.
   --  If file names are not case sensitive (i.e. for example if you have
   --  the file "xyz.adb", you can refer to it as XYZ.adb or XyZ.AdB), then
   --  this call converts the given string to canonical all lower case form,
   --  so that two file names compare equal if they refer to the same file.

   function Number_Of_Files return Int;
   --  gives the total number of filenames found on the command line.

   procedure Add_File (File_Name : String);
   --  Called by the subprogram processing the command line for each
   --  file name found.

   procedure Set_Output_Object_File_Name (Name : String);
   --  Called by the subprogram processing the command line when an
   --  output object file name is found.

   type Program_Type is (Compiler, Binder, Make);
   Program : Program_Type;
   --  Program currently running (set by Initialize below)

   procedure Initialize (P : Program_Type);
   --  This routine scans parameters and initializes for the first call to
   --  Next_Main_Source (Compiler or Make) or Next_Main_Lib_File (Binder).
   --  It also resets any of the variables in package Opt in response to
   --  command switch settings.
   --
   --  Initialize may terminate execution if the parameters are invalid or some
   --  other fatal error is encountered. The interface is set up to
   --  accommodate scanning a series of files (e.g. as the result of
   --  wild card references in DOS, or an expanded list of source files
   --  in Unix). Of course it is perfectly possible to ignore this in
   --  the implementation and provide for opening only one file.
   --  The parameter P is the program (Compiler, Binder or Make) that is
   --  actually running.

   procedure Find_Program_Name;
   --  Put simple name of current program being run (excluding the directory
   --  path) in Name_Buffer, with the length in Name_Len.

   function Program_Name (Nam : String) return String_Access;
   --  In the native compilation case, Create a string containing Nam. In
   --  the cross compilation case, looks at the prefix of the current
   --  program being run and prepend it to Nam. For instance if the program
   --  being run is <target>-gnatmake and Nam is "gcc", the returned value
   --  will be a pointer to "<target>-gcc". This function clobbers
   --  Name_Buffer and Name_Len.

   procedure Write_Program_Name;
   --  Writes name of program as invoked to standard output

   procedure Fail (S1 : String; S2 : String := ""; S3 : String := "");
   --  Outputs error messages S1 & S2 & S3 preceded by the name of the
   --  executing program and exits with E_Fatal.

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   function Get_Directory (Name : File_Name_Type) return File_Name_Type;
   --  Get the prefix directory name (if any) from Name. The last separator
   --  is preserved. Return No_File if there is no directory part in the
   --  name.

   function Is_Readonly_Library (File : File_Name_Type) return Boolean;
   --  Check if this library file is a read-only file.

   function Strip_Directory (Name : File_Name_Type) return File_Name_Type;
   --  Strips the prefix directory name (if any) from Name. Returns the
   --  stripped name.

   function Strip_Suffix (Name : File_Name_Type) return File_Name_Type;
   --  Strips the suffix (the '.' and whatever comes after it) from Name.
   --  Returns the stripped name.

   function Executable_Name (Name : File_Name_Type) return File_Name_Type;
   --  Given a file name it adds the appropriate suffix at the end so that
   --  it becomes the name of the executable on the system at end. For
   --  instance under DOS it adds the ".exe" suffix, whereas under UNIX no
   --  suffix is added.

   function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type;
   --  Returns the time stamp of file Name. Name should include relative
   --  path information in order to locate it. If the source file cannot be
   --  opened, or Name = No_File, and all blank time stamp is returned (this is
   --  not an error situation).

   procedure Record_Time_From_Last_Bind;
   --  Trigger the computing of the time from the last bind of the same
   --  program.

   function Time_From_Last_Bind return Nat;
   --  This function give an approximate number of minute from the last bind.
   --  It bases its computation on file stamp and therefore does gibe not
   --  any meaningful result before the new output binder file is written.
   --  So it returns Nat'last if
   --   - it is the first bind of this  specific program
   --   - Record_Time_From_Last_Bind was not Called first
   --   - Close_Binder_Output was not called first
   --  otherwise returns the number of minutes
   --  till the last bind. The computation does not try to be completely
   --  accurate and in particular does not take leap years into account.

   type String_Access_List is array (Positive range <>) of String_Access;
   --  Deferenced type used to return a list of file specs in
   --  To_Canonical_File_List.

   type String_Access_List_Access is access all String_Access_List;
   --  Type used to return a String_Access_List  without dragging in secondary
   --  stack.

   function To_Canonical_File_List
     (Wildcard_Host_File : String; Only_Dirs : Boolean)
   return String_Access_List_Access;
   --  Expand a wildcard host syntax file or directory specification (e.g. on
   --  a VMS host, any file or directory spec that contains:
   --  "*", or "%", or "...")
   --  and return a list of valid Unix syntax file or directory specs.
   --  If Only_Dirs is True, then only return directories.

   function To_Canonical_Dir_Spec
     (Host_Dir     : String;
      Prefix_Style : Boolean)
      return String_Access;
   --  Convert a host syntax directory specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[DIR]") to canonical (Unix) syntax (e.g. "/sys$device/dir").
   --  If Prefix_Style then make it a valid file specification prefix.
   --  A file specification prefix is a directory specification that
   --  can be appended with a simple file specification to yield a valid
   --  absolute or relative path to a file. On a conversion to Unix syntax
   --  this simply means the spec has a trailing slash ("/").

   function To_Canonical_File_Spec
     (Host_File : String)
      return String_Access;
   --  Convert a host syntax file specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[DIR]FILE.EXT;69 to canonical (Unix) syntax (e.g.
   --  "/sys$device/dir/file.ext.69").

   function To_Canonical_Path_Spec
     (Host_Path : String)
      return String_Access;
   --  Convert a host syntax Path specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[BAR],DISK$USER:[FOO] to canonical (Unix) syntax (e.g.
   --  "/sys$device/foo:disk$user/foo").

   function To_Host_Dir_Spec
     (Canonical_Dir : String;
      Prefix_Style  : Boolean)
      return String_Access;
   --  Convert a canonical syntax directory specification to host syntax.
   --  The Prefix_Style flag is currently ignored but should be set to
   --  False.

   function To_Host_File_Spec
     (Canonical_File : String)
      return String_Access;
   --  Convert a canonical syntax file specification to host syntax.

   -------------------------
   -- Search Dir Routines --
   -------------------------

   procedure Add_Default_Search_Dirs;
   --  This routine adds the default search dirs indicated by the
   --  environment variables and sdefault package.

   procedure Add_Lib_Search_Dir (Dir : String);
   --  Add Dir at the end of the library file search path

   procedure Add_Src_Search_Dir (Dir : String);
   --  Add Dir at the end of the source file search path

   procedure Get_Next_Dir_In_Path_Init
     (Search_Path : String_Access);
   function  Get_Next_Dir_In_Path
     (Search_Path : String_Access)
      return        String_Access;
   --  These subprograms are used to parse out the directory names in a
   --  search path specified by a Search_Path argument. The procedure
   --  initializes an internal pointer to point to the initial directory
   --  name, and calls to the function return successive directory names,
   --  with a null pointer marking the end of the list.

   function Get_Primary_Src_Search_Directory return String_Ptr;
   --  Retrieved the primary directory (directory containing the main source
   --   file for Gnatmake.

   function Nb_Dir_In_Src_Search_Path return Natural;
   function Dir_In_Src_Search_Path (Position : Natural) return String_Ptr;
   --  Functions to access the directory names in the source search path

   function Nb_Dir_In_Obj_Search_Path return Natural;
   function Dir_In_Obj_Search_Path (Position : Natural) return String_Ptr;
   --  Functions to access the directory names in the Object search path

   Include_Search_File : constant String_Access
     := new String'("ada_source_path");
   Objects_Search_File : constant String_Access
     := new String'("ada_object_path");

   --  Files containg the default include or objects search directories.

   function Read_Default_Search_Dirs
     (Search_Dir_Prefix : String_Access;
      Search_File : String_Access;
      Search_Dir_Default_Name : String_Access)
     return String_Access;
   --  Read and return the default search directories from the file located
   --  in Search_Dir_Prefix (as modified by update_path) and named Search_File.
   --  If no such file exists or an error occurs then instead return the
   --  Search_Dir_Default_Name (as modified by update_path).

   -----------------------
   -- Source File Input --
   -----------------------

   --  Source file input routines are used by the compiler to read the main
   --  source files and the subsidiary source files (e.g. with'ed units), and
   --  also by the binder to check presence/time stamps of sources.

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

   procedure Read_Source_File
     (N   : File_Name_Type;
      Lo  : Source_Ptr;
      Hi  : out Source_Ptr;
      Src : out Source_Buffer_Ptr;
      T   : File_Type := Source);
   --  Allocates a Source_Buffer of appropriate length and then reads the
   --  entire contents of the source file N into the buffer. The address of
   --  the allocated buffer is returned in Src.
   --
   --  Each line of text is terminated by one of the sequences:
   --
   --    CR
   --    CR/LF
   --    LF/CR
   --    LF

   --  The source is terminated by an EOF (16#1A#) character, which is
   --  the last charcater of the returned source bufer (note that any
   --  EOF characters in positions other than the last source character
   --  are treated as representing blanks).
   --
   --  The logical lower bound of the source buffer is the input value of Lo,
   --  and on exit Hi is set to the logical upper bound of the source buffer.
   --  Note that the returned value in Src points to an array with a physical
   --  lower bound of zero. This virtual origin addressing approach means that
   --  a constrained array pointer can be used with a low bound of zero which
   --  results in more efficient code.
   --
   --  If the given file cannot be opened, then the action depends on whether
   --  this file is the current main unit (i.e. its name matches the name
   --  returned by the most recent call to Next_Main_Source). If so, then the
   --  failure to find the file is a fatal error, an error message is output,
   --  and program execution is terminated. Otherwise (for the case of a
   --  subsidiary source loaded directly or indirectly using with), a file
   --  not found condition causes null to be set as the result value.
   --
   --  Note that the name passed to this function is the simple file name,
   --  without any directory information. The implementation is responsible
   --  for searching for the file in the appropriate directories.
   --
   --  Note the special case that if the file name is gnat.adc, then the
   --  search for the file is done ONLY in the directory corresponding to
   --  the current compilation environment, i.e. in the same directory
   --  where the ali and object files will be written.

   function Full_Source_Name return File_Name_Type;
   function Current_Source_File_Stamp return Time_Stamp_Type;
   --  Returns the full name/time stamp of the source file most recently read
   --  using Read_Source_File. Calling this routine entails no source file
   --  directory lookup penalty.

   function Full_Source_Name (N : File_Name_Type) return File_Name_Type;
   function Source_File_Stamp (N : File_Name_Type) return Time_Stamp_Type;
   --  Returns the full name/time stamp of the source file whose simple name
   --  is N which should not include path information. Note that if the file
   --  cannot be located No_File is returned for the first routine and an
   --  all blank time stamp is returned for the second (this is not an error
   --  situation). The full name includes the appropriate directory
   --  information. The source file directory lookup penalty is incurred
   --  every single time the routines are called unless you have previously
   --  called Source_File_Data (Cache => True). See below.

   function Matching_Full_Source_Name
     (N    : File_Name_Type;
      T    : Time_Stamp_Type)
      return File_Name_Type;
   --  Same semantics than Full_Source_Name but will search on the source
   --  path until a source file with time stamp matching T is found. If
   --  none is found returns No_File.

   procedure Source_File_Data (Cache : Boolean);
   --  By default source file data (full source file name and time stamp)
   --  are looked up every time a call to Full_Source_Name (N) or
   --  Source_File_Stamp (N) is made. This may be undesirable in certain
   --  applications as this is uselessly slow if source file data does not
   --  change during program execution. When this procedure is called with
   --  Cache => True access to source file data does not encurr a penalty if
   --  this data was previously retrieved.

   -------------------------------------------
   -- Representation of Library Information --
   -------------------------------------------

   --  Associated with each compiled source file is library information,
   --  a string of bytes whose exact format is described in the body of
   --  Lib.Writ. Compiling a source file generates this library information
   --  for the compiled unit, and access the library information for units
   --  that were compiled previously on which the unit being compiled depends.

   --  How this information is stored is up to the implementation of this
   --  package. At the interface level, this information is simply associated
   --  with its corresponding source.

   --  Several different implementations are possible:

   --    1. The information could be directly associated with the source file,
   --       e.g. placed in a resource fork of this file on the Mac, or on
   --       MS-DOS, written to the source file after the end of file mark.

   --    2. The information could be written into the generated object module
   --       if the system supports the inclusion of arbitrary informational
   --       byte streams into object files. In this case there must be a naming
   --       convention that allows object files to be located given the name of
   --       the corresponding source file.

   --    3. The information could be written to a separate file, whose name is
   --       related to the name of the source file by a fixed convention.

   --  Which of these three methods is chosen depends on the constraints of the
   --  host operating system. The interface described here is independent of
   --  which of these approaches is used.

   -------------------------------
   -- Library Information Input --
   -------------------------------

   --  These subprograms are used by the binder to read library information
   --  files, see section above for representation of these files.

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

   function Read_Library_Info
     (Lib_File  : File_Name_Type;
      Fatal_Err : Boolean := False)
      return      Text_Buffer_Ptr;
   --  Allocates a Text_Buffer of appropriate length and reads in the entire
   --  source of the library information from the library information file
   --  whose name is given by the parameter Name.
   --
   --  See description of Read_Source_File for details on the format of the
   --  returned text buffer (the format is identical). THe lower bound of
   --  the Text_Buffer is always zero
   --
   --  If the specified file cannot be opened, then the action depends on
   --  Fatal_Err. If Fatal_Err is True, an error message is given and the
   --  compilation is abandoned. Otherwise if Fatal_Err is False, then null
   --  is returned. Note that the Lib_File is a simple name which does not
   --  include any directory information. The implementation is responsible
   --  for searching for the file in appropriate directories.
   --
   --  If Opt.Check_Object_Consistency is set to True then this routine
   --  checks whether the object file corresponding to the Lib_File is
   --  consistent with it. The object file is inconsistent if the object
   --  does not exist or if it has an older time stamp than Lib_File.
   --  This check is not performed when the Lib_File is "locked" (i.e.
   --  read/only) because in this case the object file may be buried
   --  in a library. In case of inconsistencies Read_Library_Info
   --  behaves as if it did not find Lib_File (namely if Fatal_Err is
   --  False, null is returned).

   procedure Read_Library_Info
     (Name : out File_Name_Type;
      Text : out Text_Buffer_Ptr);
   --  The procedure version of Read_Library_Info is used from the compiler
   --  to read an existing ali file associated with the main unit. If the
   --  ALI file exists, then its file name is returned in Name, and its
   --  text is returned in Text. If the file does not exist, then Text is
   --  set to null.

   function Full_Library_Info_Name return File_Name_Type;
   function Full_Object_File_Name return File_Name_Type;
   --  Returns the full name of the library/object file most recently read
   --  using Read_Library_Info, including appropriate directory information.
   --  Calling this routine entails no library file directory lookup
   --  penalty. Note that the object file corresponding to a library file
   --  is not actually read. Its time stamp is fected when the flag
   --  Opt.Check_Object_Consistency is set.

   function Current_Library_File_Stamp return Time_Stamp_Type;
   function Current_Object_File_Stamp return Time_Stamp_Type;
   --  The time stamps of the files returned by the previous two routines.
   --  It is an error to call Current_Object_File_Stamp if
   --  Opt.Check_Object_Consistency is set to False.

   function Full_Lib_File_Name (N : File_Name_Type) return File_Name_Type;
   function Library_File_Stamp (N : File_Name_Type) return Time_Stamp_Type;
   --  Returns the full name/time stamp of library file N. N should not
   --  include path information. Note that if the file cannot be located
   --  No_File is returned for the first routine and an all blank time stamp
   --  is returned for the second (this is not an error situation). The
   --  full name includes the appropriate directory information. The library
   --  file directory lookup penalty is incurred every single time this
   --  routine is called.

   function Object_File_Name (N : File_Name_Type) return File_Name_Type;
   --  Constructs the name of the object file corresponding to library
   --  file N. If N is a full file name than the returned file name will
   --  also be a full file name. Note that no lookup in the library file
   --  directories is done for this file. This routine merely constructs
   --  the name.

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
   --  buffers etc from writes by Write_Library_Info.

   function Lib_File_Name (Source_File : File_Name_Type) return File_Name_Type;
   --  Given the name of a source file, returns the name of the corresponding
   --  library information file. This may be the name of the object file, or
   --  of a separate file used to store the library information. In either case
   --  the returned result is suitable for use in a call to Read_Library_Info.
   --  Note: this subprogram is in this section because it is used by the
   --  compiler to determine the proper library information names to be placed
   --  in the generated library information file.

   ------------------------------
   -- Debug Source File Output --
   ------------------------------

   --  These routines are used by the compiler to generate the debug source
   --  file for the Debug_Generated_Code (-gnatD switch) option. Note that
   --  debug source file writing occurs at a completely different point in
   --  the processing from library information output, so the code in the
   --  body can assume these functions are never used at the same time.

   function Create_Debug_File (Src : File_Name_Type) return File_Name_Type;
   --  Given the simple name of a source file, this routine creates the
   --  corresponding debug file, and returns its full name.

   procedure Write_Debug_Info (Info : String);
   --  Writes contents of given string as next line of the current debug
   --  source file created by the most recent call to Get_Debug_Name. Info
   --  does not contain any end of line or other formatting characters.

   procedure Close_Debug_File;
   --  Close current debug file created by the most recent call to
   --  Get_Debug_Name.

   function Debug_File_Eol_Length return Nat;
   --  Returns the number of characters (1 for NL, 2 for CR/LF) written
   --  at the end of each line by Write_Debug_Info.

   --------------------------------
   -- Semantic Tree Input-Output --
   --------------------------------

   procedure Tree_Create;
   --  Creates the tree output file for the source file which is currently
   --  being compiled (i.e. the file which was most recently returned by
   --  Next_Main_Source), and initializes Tree_IO.Tree_Write for output.

   procedure Tree_Close;
   --  Closes the file previously opened by Tree_Create

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

   -----------------
   -- Termination --
   -----------------

   type Exit_Code_Type is (
      E_Success,    -- No warnings or errors
      E_Warnings,   -- Compiler warnings generated
      E_No_Code,    -- No code generated
      E_No_Compile, -- Compilation not needed (smart recompilation)
      E_Errors,     -- Compiler error messages generated
      E_Fatal,      -- Fatal (serious) error, e.g. source file not found
      E_Abort);     -- Internally detected compiler error

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   --  A call to Exit_Program terminates execution with the given status.
   --  A status of zero indicates normal completion, a non-zero status
   --  indicates abnormal termination.

   -------------------------
   -- Command Line Access --
   -------------------------

   --  Direct interface to command line parameters. (We don't want to use
   --  the predefined command line package because it defines functions
   --  returning string)

   function Arg_Count return Natural;
   pragma Import (C, Arg_Count, "__gnat_arg_count");
   --  Get number of arguments (note: optional globbing may be enabled)

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Import (C, Fill_Arg, "__gnat_fill_arg");
   --  Store one argument

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Import (C, Len_Arg, "__gnat_len_arg");
   --  Get length of argument

end Osint;
