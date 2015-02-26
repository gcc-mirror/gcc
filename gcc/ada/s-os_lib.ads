------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . O S _ L I B                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Operating system interface facilities

--  This package contains types and procedures for interfacing to the
--  underlying OS. It is used by the GNAT compiler and by tools associated
--  with the GNAT compiler, and therefore works for the various operating
--  systems to which GNAT has been ported. This package will undoubtedly grow
--  as new services are needed by various tools.

--  This package tends to use fairly low-level Ada in order to not bring in
--  large portions of the RTL. For example, functions return access to string
--  as part of avoiding functions returning unconstrained types.

--  Except where specifically noted, these routines are portable across all
--  GNAT implementations on all supported operating systems.

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.OS_Lib (file g-os_lib.ads).

--  Note: a distinct body for this spec is included in the .NET runtime library
--  and must be kept in sync with changes made in this file.

pragma Compiler_Unit_Warning;

with System;
with System.Strings;

package System.OS_Lib is
   pragma Preelaborate;

   -----------------------
   -- String Operations --
   -----------------------

   --  These are reexported from package Strings (which was introduced to
   --  avoid different packages declaring different types unnecessarily).
   --  See package System.Strings for details.

   subtype String_Access is Strings.String_Access;

   function "=" (Left, Right : String_Access) return Boolean
     renames Strings."=";

   procedure Free (X : in out String_Access) renames Strings.Free;

   subtype String_List is Strings.String_List;

   function "=" (Left, Right : String_List) return Boolean
     renames Strings."=";

   function "&" (Left : String_Access; Right : String_Access)
     return String_List renames Strings."&";
   function "&" (Left : String_Access; Right : String_List)
     return String_List renames Strings."&";
   function "&" (Left : String_List; Right : String_Access)
     return String_List renames Strings."&";
   function "&" (Left : String_List; Right : String_List)
     return String_List renames Strings."&";

   subtype String_List_Access is Strings.String_List_Access;

   function "=" (Left, Right : String_List_Access) return Boolean
     renames Strings."=";

   procedure Free (Arg : in out String_List_Access)
     renames Strings.Free;

   ---------------------
   -- Time/Date Stuff --
   ---------------------

   type OS_Time is private;
   --  The OS's notion of time is represented by the private type OS_Time. This
   --  is the type returned by the File_Time_Stamp functions to obtain the time
   --  stamp of a specified file. Functions and a procedure (modeled after the
   --  similar subprograms in package Calendar) are provided for extracting
   --  information from a value of this type. Although these are called GM, the
   --  intention in the case of time stamps is not that they provide GMT times
   --  in all cases but rather the actual (time-zone independent) time stamp of
   --  the file (of course in Unix systems, this *is* in GMT form).

   Invalid_Time : constant OS_Time;
   --  A special unique value used to flag an invalid time stamp value

   subtype Year_Type   is Integer range 1900 .. 2099;
   subtype Month_Type  is Integer range    1 ..   12;
   subtype Day_Type    is Integer range    1 ..   31;
   subtype Hour_Type   is Integer range    0 ..   23;
   subtype Minute_Type is Integer range    0 ..   59;
   subtype Second_Type is Integer range    0 ..   59;
   --  Declarations similar to those in Calendar, breaking down the time

   function Current_Time return OS_Time;
   --  Return the system clock value as OS_Time

   function GM_Year    (Date : OS_Time) return Year_Type;
   function GM_Month   (Date : OS_Time) return Month_Type;
   function GM_Day     (Date : OS_Time) return Day_Type;
   function GM_Hour    (Date : OS_Time) return Hour_Type;
   function GM_Minute  (Date : OS_Time) return Minute_Type;
   function GM_Second  (Date : OS_Time) return Second_Type;
   --  Functions to extract information from OS_Time value in GMT form

   function "<"  (X, Y : OS_Time) return Boolean;
   function ">"  (X, Y : OS_Time) return Boolean;
   function ">=" (X, Y : OS_Time) return Boolean;
   function "<=" (X, Y : OS_Time) return Boolean;
   --  Basic comparison operators on OS_Time with obvious meanings. Note that
   --  these have Intrinsic convention, so for example it is not permissible
   --  to create accesses to any of these functions.

   procedure GM_Split
     (Date   : OS_Time;
      Year   : out Year_Type;
      Month  : out Month_Type;
      Day    : out Day_Type;
      Hour   : out Hour_Type;
      Minute : out Minute_Type;
      Second : out Second_Type);
   --  Analogous to the Split routine in Ada.Calendar, takes an OS_Time and
   --  provides a representation of it as a set of component parts, to be
   --  interpreted as a date point in UTC.

   function GM_Time_Of
     (Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type) return OS_Time;
   --  Analogous to the Time_Of routine in Ada.Calendar, takes a set of time
   --  component parts to be interpreted in the local time zone, and returns
   --  an OS_Time. Returns Invalid_Time if the creation fails.

   function Current_Time_String return String;
   --  Returns current local time in the form YYYY-MM-DD HH:MM:SS. The result
   --  has bounds 1 .. 19.

   ----------------
   -- File Stuff --
   ----------------

   --  These routines give access to the open/creat/close/read/write level of
   --  I/O routines in the typical C library (these functions are not part of
   --  the ANSI C standard, but are typically available in all systems). See
   --  also package Interfaces.C_Streams for access to the stream level
   --  routines.

   --  Note on file names. If a file name is passed as type String in any of
   --  the following specifications, then the name is a normal Ada string and
   --  need not be NUL-terminated. However, a trailing NUL character is
   --  permitted, and will be ignored (more accurately, the NUL and any
   --  characters that follow it will be ignored).

   type File_Descriptor is new Integer;
   --  Corresponds to the int file handle values used in the C routines

   Standin  : constant File_Descriptor := 0;
   Standout : constant File_Descriptor := 1;
   Standerr : constant File_Descriptor := 2;
   --  File descriptors for standard input output files

   Invalid_FD : constant File_Descriptor := -1;
   --  File descriptor returned when error in opening/creating file

   type Mode is (Binary, Text);
   for Mode'Size use Integer'Size;
   for Mode use (Binary => 0, Text => 1);
   --  Used in all the Open and Create calls to specify if the file is to be
   --  opened in binary mode or text mode. In systems like Unix, this has no
   --  effect, but in systems capable of text mode translation, the use of
   --  Text as the mode parameter causes the system to do CR/LF translation
   --  and also to recognize the DOS end of file character on input. The use
   --  of Text where appropriate allows programs to take a portable Unix view
   --  of DOS-format files and process them appropriately.

   function Open_Read
     (Name  : String;
      Fmode : Mode) return File_Descriptor;
   --  Open file Name for reading, returning its file descriptor. File
   --  descriptor returned is Invalid_FD if the file cannot be opened.

   function Open_Read_Write
     (Name  : String;
      Fmode : Mode) return File_Descriptor;
   --  Open file Name for both reading and writing, returning its file
   --  descriptor. File descriptor returned is Invalid_FD if the file
   --  cannot be opened.

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor;
   --  Opens file Name for appending, returning its file descriptor. File
   --  descriptor returned is Invalid_FD if the file cannot be successfully
   --  opened.

   function Create_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor;
   --  Creates new file with given name for writing, returning file descriptor
   --  for subsequent use in Write calls. If the file already exists, it is
   --  overwritten. File descriptor returned is Invalid_FD if file cannot be
   --  successfully created.

   function Create_Output_Text_File (Name : String) return File_Descriptor;
   --  Creates new text file with given name suitable to redirect standard
   --  output, returning file descriptor. File descriptor returned is
   --  Invalid_FD if file cannot be successfully created.

   function Create_New_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor;
   --  Create new file with given name for writing, returning file descriptor
   --  for subsequent use in Write calls. This differs from Create_File in
   --  that it fails if the file already exists. File descriptor returned is
   --  Invalid_FD if the file exists or cannot be created.

   Temp_File_Len : constant Integer := 12;
   --  Length of name returned by Create_Temp_File call (GNAT-XXXXXX & NUL)

   subtype Temp_File_Name is String (1 .. Temp_File_Len);
   --  String subtype set by Create_Temp_File

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Temp_File_Name);
   --  Create and open for writing a temporary file in the current working
   --  directory. The name of the file and the File Descriptor are returned.
   --  The File Descriptor returned is Invalid_FD in the case of failure. No
   --  mode parameter is provided. Since this is a temporary file, there is no
   --  point in doing text translation on it.
   --
   --  On some operating systems, the maximum number of temp files that can be
   --  created with this procedure may be limited. When the maximum is reached,
   --  this procedure returns Invalid_FD. On some operating systems, there may
   --  be a race condition between processes trying to create temp files at the
   --  same time in the same directory using this procedure.

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out String_Access);
   --  Create and open for writing a temporary file in the current working
   --  directory. The name of the file and the File Descriptor are returned.
   --  It is the responsibility of the caller to deallocate the access value
   --  returned in Name.
   --
   --  The file is opened in binary mode (no text translation).
   --
   --  This procedure will always succeed if the current working directory is
   --  writable. If the current working directory is not writable, then
   --  Invalid_FD is returned for the file descriptor and null for the Name.
   --  There is no race condition problem between processes trying to create
   --  temp files at the same time in the same directory.

   procedure Create_Temp_Output_File
     (FD   : out File_Descriptor;
      Name : out String_Access);
   --  Create and open for writing a temporary file in the current working
   --  directory suitable to redirect standard output. The name of the file and
   --  the File Descriptor are returned. It is the responsibility of the caller
   --  to deallocate the access value returned in Name.
   --
   --  The file is opened in text mode
   --
   --  This procedure will always succeed if the current working directory is
   --  writable. If the current working directory is not writable, then
   --  Invalid_FD is returned for the file descriptor and null for the Name.
   --  There is no race condition problem between processes trying to create
   --  temp files at the same time in the same directory.

   procedure Close (FD : File_Descriptor; Status : out Boolean);
   --  Close file referenced by FD. Status is False if the underlying service
   --  failed. Reasons for failure include: disk full, disk quotas exceeded
   --  and invalid file descriptor (the file may have been closed twice).

   procedure Close (FD : File_Descriptor);
   --  Close file referenced by FD. This form is used when the caller wants to
   --  ignore any possible error (see above for error cases).

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean;
      Status        : out Boolean);
   --  When Close_On_Exec is True, mark FD to be closed automatically when new
   --  program is executed by the calling process (i.e. prevent FD from being
   --  inherited by child processes). When Close_On_Exec is False, mark FD to
   --  not be closed on exec (i.e. allow it to be inherited). Status is False
   --  if the operation could not be performed.

   procedure Delete_File (Name : String; Success : out Boolean);
   --  Deletes file. Success is set True or False indicating if the delete is
   --  successful.

   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean);
   --  Rename a file. Success is set True or False indicating if the rename is
   --  successful or not.
   --
   --  WARNING: In one very important respect, this function is significantly
   --  non-portable. If New_Name already exists then on Unix systems, the call
   --  deletes the existing file, and the call signals success. On Windows, the
   --  call fails, without doing the rename operation. See also the procedure
   --  Ada.Directories.Rename, which portably provides the windows semantics,
   --  i.e. fails if the output file already exists.

   --  The following defines the mode for the Copy_File procedure below. Note
   --  that "time stamps and other file attributes" in the descriptions below
   --  refers to the creation and last modification times, and also the file
   --  access (read/write/execute) status flags.

   type Copy_Mode is
     (Copy,
      --  Copy the file. It is an error if the target file already exists. The
      --  time stamps and other file attributes are preserved in the copy.

      Overwrite,
      --  If the target file exists, the file is replaced otherwise the file
      --  is just copied. The time stamps and other file attributes are
      --  preserved in the copy.

      Append);
      --  If the target file exists, the contents of the source file is
      --  appended at the end. Otherwise the source file is just copied. The
      --  time stamps and other file attributes are preserved if the
      --  destination file does not exist.

   type Attribute is
     (Time_Stamps,
      --  Copy time stamps from source file to target file. All other
      --  attributes are set to normal default values for file creation.

      Full,
      --  All attributes are copied from the source file to the target file.
      --  This includes the timestamps, and for example also includes
      --  read/write/execute attributes in Unix systems.

      None);
      --  No attributes are copied. All attributes including the time stamp
      --  values are set to normal default values for file creation.

   --  Note: The default is Time_Stamps, which corresponds to the normal
   --  default on Windows style systems. Full corresponds to the typical
   --  effect of "cp -p" on Unix systems, and None corresponds to the typical
   --  effect of "cp" on Unix systems.

   --  Note: Time_Stamps and Full are not supported on VxWorks 5

   procedure Copy_File
     (Name     : String;
      Pathname : String;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps);
   --  Copy a file. Name must designate a single file (no wild cards allowed).
   --  Pathname can be a filename or directory name. In the latter case Name
   --  is copied into the directory preserving the same file name. Mode
   --  defines the kind of copy, see above with the default being a normal
   --  copy in which the target file must not already exist. Success is set to
   --  True or False indicating if the copy is successful (depending on the
   --  specified Mode).
   --
   procedure Copy_Time_Stamps (Source, Dest : String; Success : out Boolean);
   --  Copy Source file time stamps (last modification and last access time
   --  stamps) to Dest file. Source and Dest must be valid filenames,
   --  furthermore Dest must be writable. Success will be set to True if the
   --  operation was successful and False otherwise.
   --
   --  Note: this procedure is not supported on VxWorks 5. On this platform,
   --  Success is always set to False.

   procedure Set_File_Last_Modify_Time_Stamp (Name : String; Time : OS_Time);
   --  Given the name of a file or directory, Name, set the last modification
   --  time stamp. This function must be used for an unopened file.

   function Read
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer;
   --  Read N bytes to address A from file referenced by FD. Returned value is
   --  count of bytes actually read, which can be less than N at EOF.

   function Write
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer;
   --  Write N bytes from address A to file referenced by FD. The returned
   --  value is the number of bytes written, which can be less than N if a
   --  disk full condition was detected.

   Seek_Cur : constant := 1;
   Seek_End : constant := 2;
   Seek_Set : constant := 0;
   --  Used to indicate origin for Lseek call

   procedure Lseek
     (FD     : File_Descriptor;
      offset : Long_Integer;
      origin : Integer);
   pragma Import (C, Lseek, "__gnat_lseek");
   --  Sets the current file pointer to the indicated offset value, relative
   --  to the current position (origin = SEEK_CUR), end of file (origin =
   --  SEEK_END), or start of file (origin = SEEK_SET).

   type Large_File_Size is range -2**63 .. 2**63 - 1;
   --  Maximum supported size for a file (8 exabytes = 8 million terabytes,
   --  should be enough to accomodate all possible needs for quite a while).

   function File_Length (FD : File_Descriptor) return Long_Integer;
   pragma Import (C, File_Length, "__gnat_file_length_long");

   function File_Length64 (FD : File_Descriptor) return Large_File_Size;
   pragma Import (C, File_Length64, "__gnat_file_length");
   --  Get length of file from file descriptor FD

   function File_Time_Stamp (Name : String) return OS_Time;
   --  Given the name of a file or directory, Name, obtains and returns the
   --  time stamp. This function can be used for an unopened file. Returns
   --  Invalid_Time is Name doesn't correspond to an existing file.

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time;
   --  Get time stamp of file from file descriptor FD Returns Invalid_Time is
   --  FD doesn't correspond to an existing file.

   function Normalize_Pathname
     (Name           : String;
      Directory      : String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return String;
   --  Returns a file name as an absolute path name, resolving all relative
   --  directories, and symbolic links. The parameter Directory is a fully
   --  resolved path name for a directory, or the empty string (the default).
   --  Name is the name of a file, which is either relative to the given
   --  directory name, if Directory is non-null, or to the current working
   --  directory if Directory is null. The result returned is the normalized
   --  name of the file. For most cases, if two file names designate the same
   --  file through different paths, Normalize_Pathname will return the same
   --  canonical name in both cases. However, there are cases when this is not
   --  true; for example, this is not true in Unix for two hard links
   --  designating the same file.
   --
   --  On Windows, the returned path will start with a drive letter except
   --  when Directory is not empty and does not include a drive letter. If
   --  Directory is empty (the default) and Name is a relative path or an
   --  absolute path without drive letter, the letter of the current drive
   --  will start the returned path. If Case_Sensitive is True (the default),
   --  then this drive letter will be forced to upper case ("C:\...").
   --
   --  If Resolve_Links is set to True, then the symbolic links, on systems
   --  that support them, will be fully converted to the name of the file or
   --  directory pointed to. This is slightly less efficient, since it
   --  requires system calls.
   --
   --  If Name cannot be resolved, is invalid (for example if it is too big) or
   --  is null on entry (for example if there is symbolic link circularity,
   --  e.g. A is a symbolic link for B, and B is a symbolic link for A), then
   --  Normalize_Pathname returns an empty string.
   --
   --  For case-sensitive file systems, the value of Case_Sensitive parameter
   --  is ignored. For file systems that are not case-sensitive, such as
   --  Windows, if this parameter is set to False, then the file and directory
   --  names are folded to lower case. This allows checking whether two files
   --  are the same by applying this function to their names and comparing the
   --  results. If Case_Sensitive is set to True, this function does not change
   --  the casing of file and directory names.

   function Is_Absolute_Path (Name : String) return Boolean;
   --  Returns True if Name is an absolute path name, i.e. it designates a
   --  file or directory absolutely rather than relative to another directory.

   function Is_Regular_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing
   --  regular file. Returns True if so, False otherwise. Name may be an
   --  absolute path name or a relative path name, including a simple file
   --  name. If it is a relative path name, it is relative to the current
   --  working directory.

   function Is_Directory (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of a directory.
   --  Returns True if so, False otherwise. Name may be an absolute path
   --  name or a relative path name, including a simple file name. If it is
   --  a relative path name, it is relative to the current working directory.

   function Is_Readable_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing file
   --  that is readable. Returns True if so, False otherwise. Note that this
   --  function simply interrogates the file attributes (e.g. using the C
   --  function stat), so it does not indicate a situation in which a file may
   --  not actually be readable due to some other process having exclusive
   --  access.

   function Is_Executable_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing file
   --  that is executable. Returns True if so, False otherwise. Note that this
   --  function simply interrogates the file attributes (e.g. using the C
   --  function stat), so it does not indicate a situation in which a file may
   --  not actually be readable due to some other process having exclusive
   --  access.

   function Is_Writable_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing file
   --  that is writable. Returns True if so, False otherwise. Note that this
   --  function simply interrogates the file attributes (e.g. using the C
   --  function stat), so it does not indicate a situation in which a file may
   --  not actually be writeable due to some other process having exclusive
   --  access.

   function Is_Symbolic_Link (Name : String) return Boolean;
   --  Determines if the given string, Name, is the path of a symbolic link on
   --  systems that support it. Returns True if so, False if the path is not a
   --  symbolic link or if the system does not support symbolic links.
   --
   --  A symbolic link is an indirect pointer to a file; its directory entry
   --  contains the name of the file to which it is linked. Symbolic links may
   --  span file systems and may refer to directories.

   procedure Set_Writable (Name : String);
   --  Change permissions on the named file to make it writable for its owner

   procedure Set_Non_Writable (Name : String);
   --  Change permissions on the named file to make it non-writable for its
   --  owner. The readable and executable permissions are not modified.

   procedure Set_Read_Only (Name : String) renames Set_Non_Writable;
   --  This renaming is provided for backwards compatibility with previous
   --  versions. The use of Set_Non_Writable is preferred (clearer name).

   S_Owner  : constant := 1;
   S_Group  : constant := 2;
   S_Others : constant := 4;
   --  Constants for use in Mode parameter to Set_Executable

   procedure Set_Executable (Name : String; Mode : Positive := S_Owner);
   --  Change permissions on the file given by Name to make it executable
   --  for its owner, group or others, according to the setting of Mode.
   --  As indicated, the default if no Mode parameter is given is owner.

   procedure Set_Readable (Name : String);
   --  Change permissions on the named file to make it readable for its
   --  owner.

   procedure Set_Non_Readable (Name : String);
   --  Change permissions on the named file to make it non-readable for
   --  its owner. The writable and executable permissions are not
   --  modified.

   function Locate_Exec_On_Path
     (Exec_Name : String) return String_Access;
   --  Try to locate an executable whose name is given by Exec_Name in the
   --  directories listed in the environment Path. If the Exec_Name does not
   --  have the executable suffix, it will be appended before the search.
   --  Otherwise works like Locate_Regular_File below. If the executable is
   --  not found, null is returned.
   --
   --  Note that this function allocates memory for the returned value. This
   --  memory needs to be deallocated after use.

   function Locate_Regular_File
     (File_Name : String;
      Path      : String) return String_Access;
   --  Try to locate a regular file whose name is given by File_Name in the
   --  directories listed in Path. If a file is found, its full pathname is
   --  returned; otherwise, a null pointer is returned. If the File_Name given
   --  is an absolute pathname, then Locate_Regular_File just checks that the
   --  file exists and is a regular file. Otherwise, if the File_Name given
   --  includes directory information, Locate_Regular_File first checks if the
   --  file exists relative to the current directory. If it does not, or if
   --  the File_Name given is a simple file name, the Path argument is parsed
   --  according to OS conventions, and for each directory in the Path a check
   --  is made if File_Name is a relative pathname of a regular file from that
   --  directory.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   function Get_Debuggable_Suffix return String_Access;
   --  Return the debuggable suffix convention. Usually this is the same as
   --  the convention for Get_Executable_Suffix. The result is allocated on
   --  the heap and should be freed after use to avoid storage leaks.

   function Get_Target_Debuggable_Suffix return String_Access;
   --  Return the target debuggable suffix convention. Usually this is the same
   --  as the convention for Get_Executable_Suffix. The result is allocated on
   --  the heap and should be freed after use to avoid storage leaks.

   function Get_Executable_Suffix return String_Access;
   --  Return the executable suffix convention. The result is allocated on the
   --  heap and should be freed after use to avoid storage leaks.

   function Get_Object_Suffix return String_Access;
   --  Return the object suffix convention. The result is allocated on the heap
   --  and should be freed after use to avoid storage leaks.

   function Get_Target_Executable_Suffix return String_Access;
   --  Return the target executable suffix convention. The result is allocated
   --  on the heap and should be freed after use to avoid storage leaks.

   function Get_Target_Object_Suffix return String_Access;
   --  Return the target object suffix convention. The result is allocated on
   --  the heap and should be freed after use to avoid storage leaks.

   --  The following section contains low-level routines using addresses to
   --  pass file name and executable name. In each routine the name must be
   --  Nul-Terminated. For complete documentation refer to the equivalent
   --  routine (using String in place of C_File_Name) defined above.

   subtype C_File_Name is System.Address;
   --  This subtype is used to document that a parameter is the address of a
   --  null-terminated string containing the name of a file.

   --  All the following functions need comments ???

   function Open_Read
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Open_Read_Write
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Create_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   function Create_New_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor;

   procedure Delete_File (Name : C_File_Name; Success : out Boolean);

   procedure Rename_File
     (Old_Name : C_File_Name;
      New_Name : C_File_Name;
      Success  : out Boolean);

   procedure Copy_File
     (Name     : C_File_Name;
      Pathname : C_File_Name;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps);

   procedure Copy_Time_Stamps
     (Source, Dest : C_File_Name;
      Success      : out Boolean);

   function File_Time_Stamp (Name : C_File_Name) return OS_Time;
   --  Returns Invalid_Time is Name doesn't correspond to an existing file

   function Is_Regular_File (Name : C_File_Name) return Boolean;
   function Is_Directory (Name : C_File_Name) return Boolean;
   function Is_Readable_File (Name : C_File_Name) return Boolean;
   function Is_Executable_File (Name : C_File_Name) return Boolean;
   function Is_Writable_File (Name : C_File_Name) return Boolean;
   function Is_Symbolic_Link (Name : C_File_Name) return Boolean;

   function Locate_Regular_File
     (File_Name : C_File_Name;
      Path      : C_File_Name) return String_Access;

   ------------------
   -- Subprocesses --
   ------------------

   subtype Argument_List is String_List;
   --  Type used for argument list in call to Spawn. The lower bound of the
   --  array should be 1, and the length of the array indicates the number of
   --  arguments.

   subtype Argument_List_Access is String_List_Access;
   --  Type used to return Argument_List without dragging in secondary stack.
   --  Note that there is a Free procedure declared for this subtype which
   --  frees the array and all referenced strings.

   procedure Normalize_Arguments (Args : in out Argument_List);
   --  Normalize all arguments in the list. This ensure that the argument list
   --  is compatible with the running OS and will works fine with Spawn and
   --  Non_Blocking_Spawn for example. If Normalize_Arguments is called twice
   --  on the same list it will do nothing the second time. Note that Spawn
   --  and Non_Blocking_Spawn call Normalize_Arguments automatically, but
   --  since there is a guarantee that a second call does nothing, this
   --  internal call will have no effect if Normalize_Arguments is called
   --  before calling Spawn. The call to Normalize_Arguments assumes that the
   --  individual referenced arguments in Argument_List are on the heap, and
   --  may free them and reallocate if they are modified.

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean);
   --  This procedure spawns a program with a given list of arguments. The
   --  first parameter of is the name of the executable. The second parameter
   --  contains the arguments to be passed to this program. Success is False
   --  if the named program could not be spawned or its execution completed
   --  unsuccessfully. Note that the caller will be blocked until the
   --  execution of the spawned program is complete. For maximum portability,
   --  use a full path name for the Program_Name argument. On some systems
   --  (notably Unix systems) a simple file name may also work (if the
   --  executable can be located in the path).
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.
   --
   --  Note: Arguments in Args that contain spaces and/or quotes such as
   --  "--GCC=gcc -v" or "--GCC=""gcc -v""" are not portable across all
   --  operating systems, and would not have the desired effect if they were
   --  passed directly to the operating system. To avoid this problem, Spawn
   --  makes an internal call to Normalize_Arguments, which ensures that such
   --  arguments are modified in a manner that ensures that the desired effect
   --  is obtained on all operating systems. The caller may call
   --  Normalize_Arguments explicitly before the call (e.g. to print out the
   --  exact form of arguments passed to the operating system). In this case
   --  the guarantee a second call to Normalize_Arguments has no effect
   --  ensures that the internal call will not affect the result. Note that
   --  the implicit call to Normalize_Arguments may free and reallocate some
   --  of the individual arguments.
   --
   --  This function will always set Success to False under VxWorks and other
   --  similar operating systems which have no notion of the concept of
   --  dynamically executable file. Otherwise Success is set True if the exit
   --  status of the spawned process is zero.

   function Spawn
     (Program_Name : String;
      Args         : Argument_List) return Integer;
   --  Similar to the above procedure, but returns the actual status returned
   --  by the operating system, or -1 under VxWorks and any other similar
   --  operating systems which have no notion of separately spawnable programs.
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.

   procedure Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Return_Code            : out Integer;
      Err_To_Out             : Boolean := True);
   --  Similar to the procedure above, but redirects the output to the file
   --  designated by Output_File_Descriptor. If Err_To_Out is True, then the
   --  Standard Error output is also redirected.
   --  Return_Code is set to the status code returned by the operating system
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Success      : out Boolean;
      Return_Code  : out Integer;
      Err_To_Out   : Boolean := True);
   --  Similar to the procedure above, but saves the output of the command to
   --  a file with the name Output_File.
   --
   --  Success is set to True if the command is executed and its output
   --  successfully written to the file. If Success is True, then Return_Code
   --  will be set to the status code returned by the operating system.
   --  Otherwise, Return_Code is undefined.
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.

   type Process_Id is private;
   --  A private type used to identify a process activated by the following
   --  non-blocking calls. The only meaningful operation on this type is a
   --  comparison for equality.

   Invalid_Pid : constant Process_Id;
   --  A special value used to indicate errors, as described below

   function Pid_To_Integer (Pid : Process_Id) return Integer;
   --  Convert a process id to an Integer. Useful for writing hash functions
   --  for type Process_Id or to compare two Process_Id (e.g. for sorting).

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id;
   --  This is a non blocking call. The Process_Id of the spawned process is
   --  returned. Parameters are to be used as in Spawn. If Invalid_Pid is
   --  returned the program could not be spawned.
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.
   --
   --  This function will always return Invalid_Pid under VxWorks, since there
   --  is no notion of executables under this OS.

   function Non_Blocking_Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Err_To_Out             : Boolean := True) return Process_Id;
   --  Similar to the procedure above, but redirects the output to the file
   --  designated by Output_File_Descriptor. If Err_To_Out is True, then the
   --  Standard Error output is also redirected. Invalid_Pid is returned
   --  if the program could not be spawned successfully.
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.
   --
   --  This function will always return Invalid_Pid under VxWorks, since there
   --  is no notion of executables under this OS.

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Err_To_Out   : Boolean := True) return Process_Id;
   --  Similar to the procedure above, but saves the output of the command to
   --  a file with the name Output_File.
   --
   --  Invalid_Pid is returned if the output file could not be created or if
   --  the program could not be spawned successfully.
   --
   --  Spawning processes from tasking programs is not recommended. See
   --  "NOTE: Spawn in tasking programs" below.
   --
   --  This function will always return Invalid_Pid under VxWorks, since there
   --  is no notion of executables under this OS.

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Stdout_File  : String;
      Stderr_File  : String) return Process_Id;
   --  Similar to the procedure above, but saves the standard output of the
   --  command to a file with the name Stdout_File and the standard output
   --  of the command to a file with the name Stderr_File.

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean);
   --  Wait for the completion of any of the processes created by previous
   --  calls to Non_Blocking_Spawn. The caller will be suspended until one of
   --  these processes terminates (normally or abnormally). If any of these
   --  subprocesses terminates prior to the call to Wait_Process (and has not
   --  been returned by a previous call to Wait_Process), then the call to
   --  Wait_Process is immediate. Pid identifies the process that has
   --  terminated (matching the value returned from Non_Blocking_Spawn).
   --  Success is set to True if this sub-process terminated successfully. If
   --  Pid = Invalid_Pid, there were no subprocesses left to wait on.
   --
   --  This function will always set success to False under VxWorks, since
   --  there is no notion of executables under this OS.

   function Argument_String_To_List
     (Arg_String : String) return Argument_List_Access;
   --  Take a string that is a program and its arguments and parse it into an
   --  Argument_List. Note that the result is allocated on the heap, and must
   --  be freed by the programmer (when it is no longer needed) to avoid
   --  memory leaks.

   -------------------------------------
   -- NOTE: Spawn in Tasking Programs --
   -------------------------------------

   --  Spawning processes in tasking programs using the above Spawn and
   --  Non_Blocking_Spawn subprograms is not recommended, because there are
   --  subtle interactions between creating a process and signals/locks that
   --  can cause trouble. These issues are not specific to Ada; they depend
   --  primarily on the operating system.

   --  If you need to spawn processes in a tasking program, you will need to
   --  understand the semantics of your operating system, and you are likely to
   --  write non-portable code, because operating systems differ in this area.

   --  The Spawn and Non_Blocking_Spawn subprograms call the following
   --  operating system functions:

   --     On Windows: spawnvp (blocking) or CreateProcess (non-blocking)

   --     On Solaris: fork1, followed in the child process by execv

   --     On other Unix-like systems: fork, followed in the child
   --     process by execv.

   --     On vxworks, spawning of processes is not supported

   --  For details, look at the functions __gnat_portable_spawn and
   --  __gnat_portable_no_block_spawn in adaint.c.

   --  You should read the operating-system-specific documentation for the
   --  above functions, paying special attention to subtle interactions with
   --  threading, signals, locks, and file descriptors. Most of the issues are
   --  related to the fact that on Unix, there is a window of time between fork
   --  and execv; Windows does not have this problem, because spawning is done
   --  in a single operation.

   --  On Posix-compliant systems, such as Linux, fork duplicates just the
   --  calling thread. (On Solaris, fork1 is the Posix-compliant version of
   --  fork.)

   --  You should avoid using signals while spawning. This includes signals
   --  used internally by the Ada run-time system, such as timer signals used
   --  to implement delay statements.

   --  It is best to spawn any subprocesses very early, before the parent
   --  process creates tasks, locks, or installs signal handlers. Certainly
   --  avoid doing simultaneous spawns from multiple threads of the same
   --  process.

   --  There is no problem spawning a subprocess that uses tasking: the
   --  problems are caused only by tasking in the parent.

   --  If the parent is using tasking, and needs to spawn subprocesses at
   --  arbitrary times, one technique is for the parent to spawn (very early)
   --  a particular spawn-manager subprocess whose job is to spawn other
   --  processes. The spawn-manager must avoid tasking. The parent sends
   --  messages to the spawn-manager requesting it to spawn processes, using
   --  whatever inter-process communication mechanism you like, such as
   --  sockets.

   --  In short, mixing spawning of subprocesses with tasking is a tricky
   --  business, and should be avoided if possible, but if it is necessary,
   --  the above guidelines should be followed, and you should beware of
   --  portability problems.

   -------------------
   -- Miscellaneous --
   -------------------

   function Getenv (Name : String) return String_Access;
   --  Get the value of the environment variable. Returns an access to the
   --  empty string if the environment variable does not exist or has an
   --  explicit null value (in some operating systems these are distinct
   --  cases, in others they are not; this interface abstracts away that
   --  difference. The argument is allocated on the heap (even in the null
   --  case), and needs to be freed explicitly when no longer needed to avoid
   --  memory leaks.

   procedure Setenv (Name : String; Value : String);
   --  Set the value of the environment variable Name to Value. This call
   --  modifies the current environment, but does not modify the parent
   --  process environment. After a call to Setenv, Getenv (Name) will always
   --  return a String_Access referencing the same String as Value. This is
   --  true also for the null string case (the actual effect may be to either
   --  set an explicit null as the value, or to remove the entry, this is
   --  operating system dependent). Note that any following calls to Spawn
   --  will pass an environment to the spawned process that includes the
   --  changes made by Setenv calls.

   procedure OS_Exit (Status : Integer);
   pragma No_Return (OS_Exit);
   --  Exit to OS with given status code (program is terminated). Note that
   --  this is abrupt termination. All tasks are immediately terminated. There
   --  are no finalization or other Ada-specific cleanup actions performed. On
   --  systems with atexit handlers (such as Unix and Windows), atexit handlers
   --  are called.

   type OS_Exit_Subprogram is access procedure (Status : Integer);

   procedure OS_Exit_Default (Status : Integer);
   pragma No_Return (OS_Exit_Default);
   --  Default implementation of procedure OS_Exit

   OS_Exit_Ptr : OS_Exit_Subprogram := OS_Exit_Default'Access;
   --  OS_Exit is implemented through this access value. It it then possible to
   --  change the implementation of OS_Exit by redirecting OS_Exit_Ptr to an
   --  other implementation.

   procedure OS_Abort;
   pragma Import (C, OS_Abort, "abort");
   pragma No_Return (OS_Abort);
   --  Exit to OS signalling an abort (traceback or other appropriate
   --  diagnostic information should be given if possible, or entry made to
   --  the debugger if that is possible).

   function Errno return Integer;
   pragma Import (C, Errno, "__get_errno");
   --  Return the task-safe last error number

   procedure Set_Errno (Errno : Integer);
   pragma Import (C, Set_Errno, "__set_errno");
   --  Set the task-safe error number

   function Errno_Message
     (Err     : Integer := Errno;
      Default : String  := "") return String;
   --  Return a message describing the given Errno value. If none is provided
   --  by the system, return Default if not empty, else return a generic
   --  message indicating the numeric errno value.

   Directory_Separator : constant Character;
   --  The character that is used to separate parts of a pathname

   Path_Separator : constant Character;
   --  The character to separate paths in an environment variable value

private
   pragma Import (C, Path_Separator, "__gnat_path_separator");
   pragma Import (C, Directory_Separator, "__gnat_dir_separator");
   pragma Import (C, Current_Time, "__gnat_current_time");

   type OS_Time is
     range -(2 ** (Standard'Address_Size - Integer'(1))) ..
           +(2 ** (Standard'Address_Size - Integer'(1)) - 1);
   --  Type used for timestamps in the compiler. This type is used to hold
   --  time stamps, but may have a different representation than C's time_t.
   --  This type needs to match the declaration of OS_Time in adaint.h.

   --  Add pragma Inline statements for comparison operations on OS_Time. It
   --  would actually be nice to use pragma Import (Intrinsic) here, but this
   --  was not properly supported till GNAT 3.15a, so that would cause
   --  bootstrap path problems. To be changed later ???

   Invalid_Time : constant OS_Time := -1;
   --  This value should match the return value from __gnat_file_time_*

   pragma Inline ("<");
   pragma Inline (">");
   pragma Inline ("<=");
   pragma Inline (">=");

   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;

end System.OS_Lib;
