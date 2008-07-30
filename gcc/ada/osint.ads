------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
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

--  This package contains the low level, operating system routines used in the
--  compiler and binder for command line processing and file input output.

with Namet; use Namet;
with Types; use Types;

with System.OS_Lib; use System.OS_Lib;
with System;        use System;

pragma Elaborate_All (System.OS_Lib);
--  For the call to function Get_Target_Object_Suffix in the private part

package Osint is

   Multi_Unit_Index_Character : Character := '~';
   --  The character before the index of the unit in a multi-unit source, in
   --  ALI and object file names. This is not a constant, because it is changed
   --  to '$' on VMS.

   Ada_Include_Path          : constant String := "ADA_INCLUDE_PATH";
   Ada_Objects_Path          : constant String := "ADA_OBJECTS_PATH";
   Project_Include_Path_File : constant String := "ADA_PRJ_INCLUDE_FILE";
   Project_Objects_Path_File : constant String := "ADA_PRJ_OBJECTS_FILE";

   procedure Initialize;
   --  Initialize internal tables

   function Normalize_Directory_Name (Directory : String) return String_Ptr;
   --  Verify and normalize a directory name. If directory name is invalid,
   --  this will return an empty string. Otherwise it will insure a trailing
   --  slash and make other normalizations.

   type File_Type is (Source, Library, Config, Definition, Preprocessing_Data);

   function Find_File
     (N : File_Name_Type;
      T : File_Type) return File_Name_Type;
   --  Finds a source, library or config file depending on the value of T
   --  following the directory search order rules unless N is the name of the
   --  file just read with Next_Main_File and already contains directory
   --  information, in which case just look in the Primary_Directory. Returns
   --  File_Name_Type of the full file name if found, No_File if file not
   --  found. Note that for the special case of gnat.adc, only the compilation
   --  environment directory is searched, i.e. the directory where the ali and
   --  object files are written. Another special case is Debug_Generated_Code
   --  set and the file name ends on ".dg", in which case we look for the
   --  generated file only in the current directory, since that is where it is
   --  always built.

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
   --  Gives the total number of filenames found on the command line

   No_Index : constant := -1;
   --  Value used in Add_File to indicate no index is specified for main

   procedure Add_File (File_Name : String; Index : Int := No_Index);
   --  Called by the subprogram processing the command line for each file name
   --  found. The index, when not defaulted to No_Index is the index of the
   --  subprogram in its source, zero indicating that the source is not
   --  multi-unit.

   procedure Find_Program_Name;
   --  Put simple name of current program being run (excluding the directory
   --  path) in Name_Buffer, with the length in Name_Len.

   function Program_Name (Nam : String; Prog : String) return String_Access;
   --  In the native compilation case, Create a string containing Nam. In the
   --  cross compilation case, looks at the prefix of the current program being
   --  run and prepend it to Nam. For instance if the program being run is
   --  <target>-gnatmake and Nam is "gcc", the returned value will be a pointer
   --  to "<target>-gcc". In the specific case where AAMP_On_Target is set, the
   --  name "gcc" is mapped to "gnaamp", and names of the form "gnat*" are
   --  mapped to "gnaamp*". This function clobbers Name_Buffer and Name_Len.
   --  Also look at any suffix, e.g. gnatmake-4.1 -> "gcc-4.1". Prog is the
   --  default name of the current program being executed, e.g. "gnatmake",
   --  "gnatlink".

   procedure Write_Program_Name;
   --  Writes name of program as invoked to the current output (normally
   --  standard output).

   procedure Fail (S1 : String; S2 : String := ""; S3 : String := "");
   pragma No_Return (Fail);
   --  Outputs error messages S1 & S2 & S3 preceded by the name of the
   --  executing program and exits with E_Fatal. The output goes to standard
   --  error, except if special output is in effect (see Output).

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   function Get_Directory (Name : File_Name_Type) return File_Name_Type;
   --  Get the prefix directory name (if any) from Name. The last separator
   --  is preserved. Return the normalized current directory if there is no
   --  directory part in the name.

   function Is_Readonly_Library (File : File_Name_Type) return Boolean;
   --  Check if this library file is a read-only file

   function Strip_Directory (Name : File_Name_Type) return File_Name_Type;
   --  Strips the prefix directory name (if any) from Name. Returns the
   --  stripped name. Name cannot end with a directory separator.

   function Strip_Suffix (Name : File_Name_Type) return File_Name_Type;
   --  Strips the suffix (the last '.' and whatever comes after it) from Name.
   --  Returns the stripped name.

   function Executable_Name (Name : File_Name_Type) return File_Name_Type;
   --  Given a file name it adds the appropriate suffix at the end so that
   --  it becomes the name of the executable on the system at end. For
   --  instance under DOS it adds the ".exe" suffix, whereas under UNIX no
   --  suffix is added.

   function Executable_Name (Name : String) return String;
   --  Same as above, with String parameters

   function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type;
   --  Returns the time stamp of file Name. Name should include relative path
   --  information in order to locate it. If the source file cannot be opened,
   --  or Name = No_File, and all blank time stamp is returned (this is not an
   --  error situation).

   function File_Stamp (Name : Path_Name_Type) return Time_Stamp_Type;
   --  Same as above for a path name

   type String_Access_List is array (Positive range <>) of String_Access;
   --  Dereferenced type used to return a list of file specs in
   --  To_Canonical_File_List.

   type String_Access_List_Access is access all String_Access_List;
   --  Type used to return a String_Access_List without dragging in secondary
   --  stack.

   function To_Canonical_File_List
     (Wildcard_Host_File : String;
      Only_Dirs          : Boolean) return String_Access_List_Access;
   --  Expand a wildcard host syntax file or directory specification (e.g. on
   --  a VMS host, any file or directory spec that contains: "*", or "%", or
   --  "...") and return a list of valid Unix syntax file or directory specs.
   --  If Only_Dirs is True, then only return directories.

   function To_Canonical_Dir_Spec
     (Host_Dir     : String;
      Prefix_Style : Boolean) return String_Access;
   --  Convert a host syntax directory specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[DIR]") to canonical (Unix) syntax (e.g. "/sys$device/dir").
   --  If Prefix_Style then make it a valid file specification prefix. A file
   --  specification prefix is a directory specification that can be appended
   --  with a simple file specification to yield a valid absolute or relative
   --  path to a file. On a conversion to Unix syntax this simply means the
   --  spec has a trailing slash ("/").

   function To_Canonical_File_Spec
     (Host_File : String) return String_Access;
   --  Convert a host syntax file specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[DIR]FILE.EXT;69 to canonical (Unix) syntax (e.g.
   --  "/sys$device/dir/file.ext.69").

   function To_Canonical_Path_Spec
     (Host_Path : String) return String_Access;
   --  Convert a host syntax Path specification (e.g. on a VMS host:
   --  "SYS$DEVICE:[BAR],DISK$USER:[FOO] to canonical (Unix) syntax (e.g.
   --  "/sys$device/foo:disk$user/foo").

   function To_Host_Dir_Spec
     (Canonical_Dir : String;
      Prefix_Style  : Boolean) return String_Access;
   --  Convert a canonical syntax directory specification to host syntax.
   --  The Prefix_Style flag is currently ignored but should be set to
   --  False.

   function To_Host_File_Spec
     (Canonical_File : String) return String_Access;
   --  Convert a canonical syntax file specification to host syntax

   function Relocate_Path
     (Prefix : String;
      Path   : String) return String_Ptr;
   --  Given an absolute path and a prefix, if Path starts with Prefix,
   --  replace the Prefix substring with the root installation directory.
   --  By default, try to compute the root installation directory by looking
   --  at the executable name as it was typed on the command line and, if
   --  needed, use the PATH environment variable. If the above computation
   --  fails, return Path. This function assumes Prefix'First = Path'First.

   function Shared_Lib (Name : String) return String;
   --  Returns the runtime shared library in the form -l<name>-<version> where
   --  version is the GNAT runtime library option for the platform. For example
   --  this routine called with Name set to "gnat" will return "-lgnat-5.02"
   --  on UNIX and Windows and -lgnat_5_02 on VMS.

   -------------------------
   -- Search Dir Routines --
   -------------------------

   function Include_Dir_Default_Prefix return String;
   --  Return the directory of the run-time library sources, as modified
   --  by update_path.

   function Object_Dir_Default_Prefix return String;
   --  Return the directory of the run-time library ALI and object files, as
   --  modified by update_path.

   procedure Add_Default_Search_Dirs;
   --  This routine adds the default search dirs indicated by the environment
   --  variables and sdefault package.

   procedure Add_Lib_Search_Dir (Dir : String);
   --  Add Dir at the end of the library file search path

   procedure Add_Src_Search_Dir (Dir : String);
   --  Add Dir at the end of the source file search path

   procedure Get_Next_Dir_In_Path_Init
     (Search_Path : String_Access);
   function Get_Next_Dir_In_Path
     (Search_Path : String_Access) return String_Access;
   --  These subprograms are used to parse out the directory names in a search
   --  path specified by a Search_Path argument. The procedure initializes an
   --  internal pointer to point to the initial directory name, and calls to
   --  the function return successive directory names, with a null pointer
   --  marking the end of the list.

   type Search_File_Type is (Include, Objects);

   procedure Add_Search_Dirs
     (Search_Path : String_Ptr;
      Path_Type   : Search_File_Type);
   --  These procedure adds all the search directories that are in Search_Path
   --  in the proper file search path (library or source)

   function Get_Primary_Src_Search_Directory return String_Ptr;
   --  Retrieved the primary directory (directory containing the main source
   --  file for Gnatmake.

   function Nb_Dir_In_Src_Search_Path return Natural;
   function Dir_In_Src_Search_Path (Position : Natural) return String_Ptr;
   --  Functions to access the directory names in the source search path

   function Nb_Dir_In_Obj_Search_Path return Natural;
   function Dir_In_Obj_Search_Path (Position : Natural) return String_Ptr;
   --  Functions to access the directory names in the Object search path

   Include_Search_File : constant String_Access :=
                           new String'("ada_source_path");
   Objects_Search_File : constant String_Access :=
                           new String'("ada_object_path");
   --  Names of the files containing the default include or objects search
   --  directories. These files, located in Sdefault.Search_Dir_Prefix, do
   --  not necessarily exist.

   Exec_Name : String_Ptr;
   --  Executable name as typed by the user (used to compute the
   --  executable prefix).

   function Read_Default_Search_Dirs
     (Search_Dir_Prefix       : String_Access;
      Search_File             : String_Access;
      Search_Dir_Default_Name : String_Access) return String_Access;
   --  Read and return the default search directories from the file located
   --  in Search_Dir_Prefix (as modified by update_path) and named Search_File.
   --  If no such file exists or an error occurs then instead return the
   --  Search_Dir_Default_Name (as modified by update_path).

   function Get_RTS_Search_Dir
     (Search_Dir : String;
      File_Type  : Search_File_Type) return String_Ptr;
   --  This function retrieves the paths to the search (resp. lib) dirs and
   --  return them. The search dir can be absolute or relative. If the search
   --  dir contains Include_Search_File (resp. Object_Search_File), then this
   --  function reads and returns the default search directories from the file.
   --  Otherwise, if the directory is absolute, it will try to find 'adalib'
   --  (resp. 'adainclude'). If found, null is returned. If the directory is
   --  relative, the following directories for the directories 'adalib' and
   --  'adainclude' will be scanned:
   --
   --   - current directory (from which the tool has been spawned)
   --   - $GNAT_ROOT/gcc/gcc-lib/$targ/$vers/
   --   - $GNAT_ROOT/gcc/gcc-lib/$targ/$vers/rts-
   --
   --  The scan will stop as soon as the directory being searched for (adalib
   --  or adainclude) is found. If the scan fails, null is returned.

   -----------------------
   -- Source File Input --
   -----------------------

   --  Source file input routines are used by the compiler to read the main
   --  source files and the subsidiary source files (e.g. with'ed units), and
   --  also by the binder to check presence/time stamps of sources.

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

   --  The source is terminated by an EOF (16#1A#) character, which is the last
   --  character of the returned source buffer (note that any EOF characters in
   --  positions other than the last source character are treated as blanks).
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
   --  Note the special case that if the file name is gnat.adc, then the search
   --  for the file is done ONLY in the directory corresponding to the current
   --  compilation environment, i.e. in the same directory where the ali and
   --  object files will be written.

   function Full_Source_Name return File_Name_Type;
   function Current_Source_File_Stamp return Time_Stamp_Type;
   --  Returns the full name/time stamp of the source file most recently read
   --  using Read_Source_File. Calling this routine entails no source file
   --  directory lookup penalty.

   function Full_Source_Name (N : File_Name_Type) return File_Name_Type;
   function Source_File_Stamp (N : File_Name_Type) return Time_Stamp_Type;
   --  Returns the full name/time stamp of the source file whose simple name
   --  is N which should not include path information. Note that if the file
   --  cannot be located No_File is returned for the first routine and an all
   --  blank time stamp is returned for the second (this is not an error
   --  situation). The full name includes appropriate directory information.
   --  The source file directory lookup penalty is incurred every single time
   --  the routines are called unless you have previously called
   --  Source_File_Data (Cache => True). See below.

   function Current_File_Index return Int;
   --  Return the index in its source file of the current main unit

   function Matching_Full_Source_Name
     (N : File_Name_Type;
      T : Time_Stamp_Type) return File_Name_Type;
   --  Same semantics than Full_Source_Name but will search on the source path
   --  until a source file with time stamp matching T is found. If none is
   --  found returns No_File.

   procedure Source_File_Data (Cache : Boolean);
   --  By default source file data (full source file name and time stamp)
   --  are looked up every time a call to Full_Source_Name (N) or
   --  Source_File_Stamp (N) is made. This may be undesirable in certain
   --  applications as this is uselessly slow if source file data does not
   --  change during program execution. When this procedure is called with
   --  Cache => True access to source file data does not incur a penalty if
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
   --  which of these approaches is used. Currently all versions of GNAT use
   --  the third approach with a file name of xxx.ali where xxx is the source
   --  file name.

   -------------------------------
   -- Library Information Input --
   -------------------------------

   --  These subprograms are used by the binder to read library information
   --  files, see section above for representation of these files.

   function Read_Library_Info
     (Lib_File  : File_Name_Type;
      Fatal_Err : Boolean := False) return Text_Buffer_Ptr;
   --  Allocates a Text_Buffer of appropriate length and reads in the entire
   --  source of the library information from the library information file
   --  whose name is given by the parameter Name.
   --
   --  See description of Read_Source_File for details on the format of the
   --  returned text buffer (the format is identical). The lower bound of
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

   function Full_Library_Info_Name return File_Name_Type;
   function Full_Object_File_Name return File_Name_Type;
   --  Returns the full name of the library/object file most recently read
   --  using Read_Library_Info, including appropriate directory information.
   --  Calling this routine entails no library file directory lookup
   --  penalty. Note that the object file corresponding to a library file
   --  is not actually read. Its time stamp is affected when the flag
   --  Opt.Check_Object_Consistency is set.

   function Current_Library_File_Stamp return Time_Stamp_Type;
   function Current_Object_File_Stamp return Time_Stamp_Type;
   --  The time stamps of the files returned by the previous two routines.
   --  It is an error to call Current_Object_File_Stamp if
   --  Opt.Check_Object_Consistency is set to False.

   function Full_Lib_File_Name (N : File_Name_Type) return File_Name_Type;
   function Library_File_Stamp (N : File_Name_Type) return Time_Stamp_Type;
   --  Returns the full name/time stamp of library file N. N should not include
   --  path information. Note that if the file cannot be located No_File is
   --  returned for the first routine and an all blank time stamp is returned
   --  for the second (this is not an error situation). The full name includes
   --  the appropriate directory information. The library file directory lookup
   --  penalty is incurred every single time this routine is called.

   function Lib_File_Name
     (Source_File : File_Name_Type;
      Munit_Index : Nat := 0) return File_Name_Type;
   --  Given the name of a source file, returns the name of the corresponding
   --  library information file. This may be the name of the object file or of
   --  a separate file used to store the library information. In the current
   --  implementation, a separate file (the ALI file) is always used. In either
   --  case the returned result is suitable for calling Read_Library_Info. The
   --  Munit_Index is the unit index in multiple unit per file mode, or zero in
   --  normal single unit per file mode (used to add ~nnn suffix). Note: this
   --  subprogram is in this section because it is used by the compiler to
   --  determine the proper library information names to be placed in the
   --  generated library information file.

   -----------------
   -- Termination --
   -----------------

   Current_Exit_Status : Integer := 0;
   --  Exit status that is set with procedure OS_Exit_Through_Exception below
   --  and can be used in exception handler for Types.Terminate_Program to call
   --  Set_Exit_Status as the last action of the program.

   procedure OS_Exit_Through_Exception (Status : Integer);
   --  Set the Current_Exit_Status, then raise Types.Terminate_Program

   type Exit_Code_Type is (
      E_Success,    -- No warnings or errors
      E_Warnings,   -- Compiler warnings generated
      E_No_Code,    -- No code generated
      E_No_Compile, -- Compilation not needed (smart recompilation)
      E_Errors,     -- Compiler error messages generated
      E_Fatal,      -- Fatal (serious) error, e.g. source file not found
      E_Abort);     -- Internally detected compiler error

   procedure Exit_Program (Exit_Code : Exit_Code_Type);
   pragma No_Return (Exit_Program);
   --  A call to Exit_Program terminates execution with the given status. A
   --  status of zero indicates normal completion, a non-zero status indicates
   --  abnormal termination.

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

private

   ALI_Suffix : constant String_Ptr := new String'("ali");
   --  The suffix used for the library files (also known as ALI files)

   Current_Main : File_Name_Type := No_File;
   --  Used to save a simple file name between calls to Next_Main_Source and
   --  Read_Source_File. If the file name argument to Read_Source_File is
   --  No_File, that indicates that the file whose name was returned by the
   --  last call to Next_Main_Source (and stored here) is to be read.

   Target_Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix used for the target object files

   Output_FD : File_Descriptor;
   --  File descriptor for current library info, list, tree, or binder output

   Output_File_Name : File_Name_Type;
   --  File_Name_Type for name of open file whose FD is in Output_FD, the name
   --  stored does not include the trailing NUL character.

   Argument_Count : constant Integer := Arg_Count - 1;
   --  Number of arguments (excluding program name)

   type File_Name_Array is array (Int range <>) of String_Ptr;
   type File_Name_Array_Ptr is access File_Name_Array;
   File_Names : File_Name_Array_Ptr :=
                  new File_Name_Array (1 .. Int (Argument_Count) + 2);
   --  As arguments are scanned, file names are stored in this array. The
   --  strings do not have terminating NUL files. The array is extensible,
   --  because when using project files, there may be more files than
   --  arguments on the command line.

   type File_Index_Array is array (Int range <>) of Int;
   type File_Index_Array_Ptr is access File_Index_Array;
   File_Indexes : File_Index_Array_Ptr :=
                    new File_Index_Array (1 .. Int (Argument_Count) + 2);

   Current_File_Name_Index : Int := 0;
   --  The index in File_Names of the last file opened by Next_Main_Source
   --  or Next_Main_Lib_File. The value 0 indicates that no files have been
   --  opened yet.

   procedure Create_File_And_Check
     (Fdesc : out File_Descriptor;
      Fmode : Mode);
   --  Create file whose name (NUL terminated) is in Name_Buffer (with the
   --  length in Name_Len), and place the resulting descriptor in Fdesc. Issue
   --  message and exit with fatal error if file cannot be created. The Fmode
   --  parameter is set to either Text or Binary (for details see description
   --  of System.OS_Lib.Create_File).

   type Program_Type is (Compiler, Binder, Make, Gnatls, Unspecified);
   --  Program currently running
   procedure Set_Program (P : Program_Type);
   --  Indicates to the body of Osint the program currently running. This
   --  procedure is called by the child packages of Osint. A check is made
   --  that this procedure is not called more than once.

   function More_Files return Boolean;
   --  Implements More_Source_Files and More_Lib_Files

   function Next_Main_File return File_Name_Type;
   --  Implements Next_Main_Source and Next_Main_Lib_File

   function Object_File_Name (N : File_Name_Type) return File_Name_Type;
   --  Constructs the name of the object file corresponding to library file N.
   --  If N is a full file name than the returned file name will also be a full
   --  file name. Note that no lookup in the library file directories is done
   --  for this file. This routine merely constructs the name.

   procedure Write_Info (Info : String);
   --  Implementation of Write_Binder_Info, Write_Debug_Info and
   --  Write_Library_Info (identical)

   procedure Write_With_Check (A : Address; N  : Integer);
   --  Writes N bytes from buffer starting at address A to file whose FD is
   --  stored in Output_FD, and whose file name is stored as a File_Name_Type
   --  in Output_File_Name. A check is made for disk full, and if this is
   --  detected, the file being written is deleted, and a fatal error is
   --  signalled.

end Osint;
