------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . O S _ L I B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1995-2001 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- Extensive contributions were provided by Ada Core Technologies Inc.   --
--                                                                          --
------------------------------------------------------------------------------

--  Operating system interface facilities

--  This package contains types and procedures for interfacing to the
--  underlying OS. It is used by the GNAT compiler and by tools associated
--  with the GNAT compiler, and therefore works for the various operating
--  systems to which GNAT has been ported. This package will undoubtedly
--  grow as new services are needed by various tools.

--  This package tends to use fairly low-level Ada in order to not bring
--  in large portions of the RTL. For example, functions return access
--  to string as part of avoiding functions returning unconstrained types;
--  types related to dates are defined here instead of using the types
--  from Calendar, since use of Calendar forces linking in of tasking code.

--  Except where specifically noted, these routines are portable across
--  all GNAT implementations on all supported operating systems.

with System;
with Unchecked_Deallocation;

package GNAT.OS_Lib is
pragma Elaborate_Body (OS_Lib);

   type String_Access is access all String;
   --  General purpose string access type

   procedure Free is new Unchecked_Deallocation
     (Object => String, Name => String_Access);

   type String_List is array (Positive range <>) of String_Access;
   type String_List_Access is access all String_List;
   --  General purpose array and pointer for list of string accesses

   ---------------------
   -- Time/Date Stuff --
   ---------------------

   --  The OS's notion of time is represented by the private type OS_Time.
   --  This is the type returned by the File_Time_Stamp functions to obtain
   --  the time stamp of a specified file. Functions and a procedure (modeled
   --  after the similar subprograms in package Calendar) are provided for
   --  extracting information from a value of this type. Although these are
   --  called GM, the intention is not that they provide GMT times in all
   --  cases but rather the actual (time-zone independent) time stamp of the
   --  file (of course in Unix systems, this *is* in GMT form).

   type OS_Time is private;

   subtype Year_Type   is Integer range 1900 .. 2099;
   subtype Month_Type  is Integer range    1 ..   12;
   subtype Day_Type    is Integer range    1 ..   31;
   subtype Hour_Type   is Integer range    0 ..   23;
   subtype Minute_Type is Integer range    0 ..   59;
   subtype Second_Type is Integer range    0 ..   59;

   function GM_Year    (Date : OS_Time) return Year_Type;
   function GM_Month   (Date : OS_Time) return Month_Type;
   function GM_Day     (Date : OS_Time) return Day_Type;
   function GM_Hour    (Date : OS_Time) return Hour_Type;
   function GM_Minute  (Date : OS_Time) return Minute_Type;
   function GM_Second  (Date : OS_Time) return Second_Type;

   procedure GM_Split
     (Date    : OS_Time;
      Year    : out Year_Type;
      Month   : out Month_Type;
      Day     : out Day_Type;
      Hour    : out Hour_Type;
      Minute  : out Minute_Type;
      Second  : out Second_Type);

   ----------------
   -- File Stuff --
   ----------------

   --  These routines give access to the open/creat/close/read/write level
   --  of I/O routines in the typical C library (these functions are not
   --  part of the ANSI C standard, but are typically available in all
   --  systems). See also package Interfaces.C_Streams for access to the
   --  stream level routines.

   --  Note on file names. If a file name is passed as type String in any
   --  of the following specifications, then the name is a normal Ada string
   --  and need not be NUL-terminated. However, a trailing NUL character is
   --  permitted, and will be ignored (more accurately, the NUL and any
   --  characters that follow it will be ignored).

   type File_Descriptor is private;
   --  Corresponds to the int file handle values used in the C routines,

   Standin  : constant File_Descriptor;
   Standout : constant File_Descriptor;
   Standerr : constant File_Descriptor;
   --  File descriptors for standard input output files

   Invalid_FD : constant File_Descriptor;
   --  File descriptor returned when error in opening/creating file;

   type Mode is (Binary, Text);
   for Mode'Size use Integer'Size;
   for Mode use (Binary => 0, Text => 1);
   --  Used in all the Open and Create calls to specify if the file is to be
   --  opened in binary mode or text mode. In systems like Unix, this has no
   --  effect, but in systems capable of text mode translation, the use of
   --  Text as the mode parameter causes the system to do CR/LF translation
   --  and also to recognize the DOS end of file character on input. The use
   --  of Text where appropriate allows programs to take a portable Unix view
   --  of DOs-format files and process them appropriately.

   function Open_Read
     (Name  : String;
      Fmode : Mode)
      return  File_Descriptor;
   --  Open file Name for reading, returning file descriptor File descriptor
   --  returned is Invalid_FD if file cannot be opened.

   function Open_Read_Write
     (Name  : String;
      Fmode : Mode)
      return  File_Descriptor;
   --  Open file Name for both reading and writing, returning file
   --  descriptor. File descriptor returned is Invalid_FD if file cannot be
   --  opened.

   function Create_File
     (Name  : String;
      Fmode : Mode)
      return  File_Descriptor;
   --  Creates new file with given name for writing, returning file descriptor
   --  for subsequent use in Write calls. File descriptor returned is
   --  Invalid_FD if file cannot be successfully created

   function Create_New_File
     (Name  : String;
      Fmode : Mode)
      return  File_Descriptor;
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
   --  Create and open for writing a temporary file. The name of the
   --  file and the File Descriptor are returned. The File Descriptor
   --  returned is Invalid_FD in the case of failure. No mode parameter
   --  is provided. Since this is a temporary file, there is no point in
   --  doing text translation on it.

   procedure Close (FD : File_Descriptor);
   pragma Import (C, Close, "close");
   --  Close file referenced by FD

   procedure Delete_File (Name : String; Success : out Boolean);
   --  Deletes file. Success is set True or False indicating if the delete is
   --  successful.

   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean);
   --  Rename a file. Successis set True or False indicating if the rename is
   --  successful.

   function Read
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer)
      return Integer;
   pragma Import (C, Read, "read");
   --  Read N bytes to address A from file referenced by FD. Returned value
   --  is count of bytes actually read, which can be less than N at EOF.

   function Write
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer)
      return Integer;
   pragma Import (C, Write, "write");
   --  Write N bytes from address A to file referenced by FD. The returned
   --  value is the number of bytes written, which can be less than N if
   --  a disk full condition was detected.

   Seek_Cur : constant := 1;
   Seek_End : constant := 2;
   Seek_Set : constant := 0;
   --  Used to indicate origin for Lseek call

   procedure Lseek
     (FD     : File_Descriptor;
      offset : Long_Integer;
      origin : Integer);
   pragma Import (C, Lseek, "lseek");
   --  Sets the current file pointer to the indicated offset value,
   --  relative to the current position (origin = SEEK_CUR), end of
   --  file (origin = SEEK_END), or start of file (origin = SEEK_SET).

   function File_Length (FD : File_Descriptor) return Long_Integer;
   pragma Import (C, File_Length, "__gnat_file_length");
   --  Get length of file from file descriptor FD

   function File_Time_Stamp (Name : String) return OS_Time;
   --  Given the name of a file or directory, Name, obtains and returns the
   --  time stamp. This function can be used for an unopend file.

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time;
   --  Get time stamp of file from file descriptor FD

   function Normalize_Pathname
     (Name      : String;
      Directory : String := "")
      return      String;
   --  Returns a file name as an absolute path name, resolving all relative
   --  directories, and symbolic links. The parameter Directory is a fully
   --  resolved path name for a directory, or the empty string (the default).
   --  Name is the name of a file, which is either relative to the given
   --  directory name, if Directory is non-null, or to the current working
   --  directory if Directory is null. The result returned is the normalized
   --  name of the file. For most cases, if two file names designate the same
   --  file through different paths, Normalize_Pathname will return the same
   --  canonical name in both cases. However, there are cases when this is
   --  not true; for example, this is not true in Unix for two hard links
   --  designating the same file.
   --
   --  If Name cannot be resolved or is null on entry (for example if there is
   --  a circularity in symbolic links: A is a symbolic link for B, while B is
   --  a symbolic link for A), then Normalize_Pathname returns an empty string.
   --
   --  In VMS, if Name follows the VMS syntax file specification, it is first
   --  converted into Unix syntax. If the conversion fails, Normalize_Pathname
   --  returns an empty string.

   function Is_Absolute_Path (Name : String) return Boolean;
   --  Returns True if Name is an absolute path name, i.e. it designates
   --  a directory absolutely, rather than relative to another directory.

   function Is_Regular_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing
   --  regular file. Returns True if so, False otherwise.

   function Is_Directory (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of a directory.
   --  Returns True if so, False otherwise.

   function Is_Writable_File (Name : String) return Boolean;
   --  Determines if the given string, Name, is the name of an existing
   --  file that is writable. Returns True if so, False otherwise.

   function Locate_Exec_On_Path
     (Exec_Name : String)
      return      String_Access;
   --  Try to locate an executable whose name is given by Exec_Name in the
   --  directories listed in the environment Path. If the Exec_Name doesn't
   --  have the executable suffix, it will be appended before the search.
   --  Otherwise works like Locate_Regular_File below.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   function Locate_Regular_File
     (File_Name : String;
      Path      : String)
      return      String_Access;
   --  Try to locate a regular file whose name is given by File_Name in the
   --  directories listed in  Path. If a file is found, its full pathname is
   --  returned; otherwise, a null pointer is returned. If the File_Name given
   --  is an absolute pathname, then Locate_Regular_File just checks that the
   --  file exists and is a regular file. Otherwise, the Path argument is
   --  parsed according to OS conventions, and for each directory in the Path
   --  a check is made if File_Name is a relative pathname of a regular file
   --  from that directory.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   function Get_Debuggable_Suffix return String_Access;
   --  Return the debuggable suffix convention. Usually this is the same as
   --  the convention for Get_Executable_Suffix.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   function Get_Executable_Suffix return String_Access;
   --  Return the executable suffix convention.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   function Get_Object_Suffix return String_Access;
   --  Return the object suffix convention.
   --
   --  Note that this function allocates some memory for the returned value.
   --  This memory needs to be deallocated after use.

   --  The following section contains low-level routines using addresses to
   --  pass file name and executable name. In each routine the name must be
   --  Nul-Terminated. For complete documentation refer to the equivalent
   --  routine (but using string) defined above.

   subtype C_File_Name is System.Address;
   --  This subtype is used to document that a parameter is the address
   --  of a null-terminated string containing the name of a file.

   function Open_Read
     (Name  : C_File_Name;
      Fmode : Mode)
      return  File_Descriptor;

   function Open_Read_Write
     (Name  : C_File_Name;
      Fmode : Mode)
      return  File_Descriptor;

   function Create_File
     (Name  : C_File_Name;
      Fmode : Mode)
      return  File_Descriptor;

   function Create_New_File
     (Name  : C_File_Name;
      Fmode : Mode)
      return  File_Descriptor;

   procedure Delete_File (Name : C_File_Name; Success : out Boolean);

   procedure Rename_File
     (Old_Name : C_File_Name;
      New_Name : C_File_Name;
      Success  : out Boolean);

   function File_Time_Stamp (Name : C_File_Name) return OS_Time;

   function Is_Regular_File (Name : C_File_Name) return Boolean;

   function Is_Directory (Name : C_File_Name) return Boolean;

   function Is_Writable_File (Name : C_File_Name) return Boolean;

   function Locate_Regular_File
     (File_Name : C_File_Name;
      Path      : C_File_Name)
      return      String_Access;

   ------------------
   -- Subprocesses --
   ------------------

   subtype Argument_List is String_List;
   --  Type used for argument list in call to Spawn. The lower bound
   --  of the array should be 1, and the length of the array indicates
   --  the number of arguments.

   subtype Argument_List_Access is String_List_Access;
   --  Type used to return an Argument_List without dragging in secondary
   --  stack.

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean);
   --  The first parameter of function Spawn is the name of the executable.
   --  The second parameter contains the arguments to be passed to the
   --  program. Success is False if the named program could not be spawned
   --  or its execution completed unsuccessfully. Note that the caller will
   --  be blocked until the execution of the spawned program is complete.
   --  For maximum portability, use a full path name for the Program_Name
   --  argument. On some systems (notably Unix systems) a simple file
   --  name may also work (if the executable can be located in the path).
   --
   --  Note: Arguments that contain spaces and/or quotes such as
   --        "--GCC=gcc -v" or "--GCC=""gcc-v""" are not portable
   --        across OSes. They may or may not have the desired effect.

   function Spawn
     (Program_Name : String;
      Args         : Argument_List)
      return         Integer;
   --  Like above, but as function returning the exact exit status

   type Process_Id is private;
   --  A private type used to identify a process activated by the following
   --  non-blocking call. The only meaningful operation on this type is a
   --  comparison for equality.

   Invalid_Pid : constant Process_Id;
   --  A special value used to indicate errors, as described below.

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List)
      return         Process_Id;
   --  This is a non blocking call. The Process_Id of the spawned process
   --  is returned. Parameters are to be used as in Spawn. If Invalid_Id
   --  is returned the program could not be spawned.

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean);
   --  Wait for the completion of any of the processes created by previous
   --  calls to Non_Blocking_Spawn. The caller will be suspended until one
   --  of these processes terminates (normally or abnormally). If any of
   --  these subprocesses terminates prior to the call to Wait_Process (and
   --  has not been returned by a previous call to Wait_Process), then the
   --  call to Wait_Process is immediate. Pid identifies the process that
   --  has terminated (matching the value returned from Non_Blocking_Spawn).
   --  Success is set to True if this sub-process terminated successfully.
   --  If Pid = Invalid_Id, there were no subprocesses left to wait on.

   function Argument_String_To_List
     (Arg_String : String)
      return       Argument_List_Access;
   --  Take a string that is a program and it's arguments and parse it into
   --  an Argument_List.

   -------------------
   -- Miscellaneous --
   -------------------

   function Getenv (Name : String) return String_Access;
   --  Get the value of the environment variable. Returns an access
   --  to the empty string if the environment variable does not exist
   --  or has an explicit null value (in some operating systems these
   --  are distinct cases, in others they are not; this interface
   --  abstracts away that difference.

   procedure Setenv (Name : String; Value : String);
   --  Set the value of the environment variable Name to Value. This call
   --  modifies the current environment, but does not modify the parent
   --  process environment. After a call to Setenv, Getenv (Name) will
   --  always return a String_Access referencing the same String as Value.
   --  This is true also for the null string case (the actual effect may
   --  be to either set an explicit null as the value, or to remove the
   --  entry, this is operating system dependent). Note that any following
   --  calls to Spawn will pass an environment to the spawned process that
   --  includes the changes made by Setenv calls. This procedure is not
   --  available under VMS.

   procedure OS_Exit (Status : Integer);
   pragma Import (C, OS_Exit, "__gnat_os_exit");
   --  Exit to OS with given status code (program is terminated)

   procedure OS_Abort;
   pragma Import (C, OS_Abort, "abort");
   --  Exit to OS signalling an abort (traceback or other appropriate
   --  diagnostic information should be given if possible, or entry made
   --  to the debugger if that is possible).

   function Errno return Integer;
   pragma Import (C, Errno, "__get_errno");
   --  Return the task-safe last error number.

   procedure Set_Errno (Errno : Integer);
   pragma Import (C, Set_Errno, "__set_errno");
   --  Set the task-safe error number.

   Directory_Separator : constant Character;
   --  The character that is used to separate parts of a pathname.

   Path_Separator : constant Character;
   --  The character to separate paths in an environment variable value.

private
   pragma Import (C, Path_Separator, "__gnat_path_separator");
   pragma Import (C, Directory_Separator, "__gnat_dir_separator");

   type OS_Time is new Integer;

   type File_Descriptor is new Integer;

   Standin    : constant File_Descriptor :=  0;
   Standout   : constant File_Descriptor :=  1;
   Standerr   : constant File_Descriptor :=  2;
   Invalid_FD : constant File_Descriptor := -1;

   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;

end GNAT.OS_Lib;
