------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1998-2003 Ada Core Technologies, Inc.           --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Directory operations

--  This package provides routines for manipulating directories. A directory
--  can be treated as a file, using open and close routines, and a scanning
--  routine is provided for iterating through the entries in a directory.

--  See also child package GNAT.Directory_Operations.Iteration

--  Note: support on OpenVMS is limited to the support of Unix-style
--  directory names (OpenVMS native directory format is not supported).
--  Read individual entries for more specific notes on OpenVMS support.

with Ada.Strings.Maps;

package GNAT.Directory_Operations is

   subtype Dir_Name_Str is String;
   --  A subtype used in this package to represent string values that are
   --  directory names. A directory name is a prefix for files that appear
   --  with in the directory. This means that for UNIX systems, the string
   --  includes a final '/', and for DOS-like systems, it includes a final
   --  '\' character. It can also include drive letters if the operating
   --  system provides for this. The final '/' or '\' in a Dir_Name_Str is
   --  optional when passed as a procedure or function in parameter.
   --  On OpenVMS, only Unix style path names are supported, not VMS style,
   --  but the directory and file names are not case sensitive.

   type Dir_Type is limited private;
   --  A value used to reference a directory. Conceptually this value includes
   --  the identity of the directory, and a sequential position within it.

   Null_Dir : constant Dir_Type;
   --  Represent the value for an uninitialized or closed directory

   Directory_Error : exception;
   --  Exception raised if the directory cannot be opened, read, closed,
   --  created or if it is not possible to change the current execution
   --  environment directory.

   Dir_Separator : constant Character;
   --  Running system default directory separator

   --------------------------------
   -- Basic Directory operations --
   --------------------------------

   procedure Change_Dir (Dir_Name : Dir_Name_Str);
   --  Changes the working directory of the current execution environment
   --  to the directory named by Dir_Name. Raises Directory_Error if Dir_Name
   --  does not exist.

   procedure Make_Dir (Dir_Name : Dir_Name_Str);
   --  Create a new directory named Dir_Name. Raises Directory_Error if
   --  Dir_Name cannot be created.

   procedure Remove_Dir
     (Dir_Name  : Dir_Name_Str;
      Recursive : Boolean := False);
   --  Remove the directory named Dir_Name. If Recursive is set to True, then
   --  Remove_Dir removes all the subdirectories and files that are in
   --  Dir_Name. Raises Directory_Error if Dir_Name cannot be removed.

   function Get_Current_Dir return Dir_Name_Str;
   --  Returns the current working directory for the execution environment.

   procedure Get_Current_Dir (Dir : out Dir_Name_Str; Last : out Natural);
   --  Returns the current working directory for the execution environment
   --  The name is returned in Dir_Name. Last is the index in Dir_Name such
   --  that Dir_Name (Last) is the last character written. If Dir_Name is
   --  too small for the directory name, the name will be truncated before
   --  being copied to Dir_Name.

   -------------------------
   -- Pathname Operations --
   -------------------------

   subtype Path_Name is String;
   --  All routines using Path_Name handle both styles (UNIX and DOS) of
   --  directory separators (either slash or back slash).

   function Dir_Name (Path : Path_Name) return Dir_Name_Str;
   --  Returns directory name for Path. This is similar to the UNIX dirname
   --  command. Everything after the last directory separator is removed. If
   --  there is no directory separator the current working directory is
   --  returned. Note that the contents of Path is case-sensitive on
   --  systems that have case-sensitive file names (like Unix), and
   --  non-case-sensitive on systems where the file system is also non-
   --  case-sensitive (such as Windows, and OpenVMS).

   function Base_Name
     (Path   : Path_Name;
      Suffix : String := "") return String;
   --  Any directory prefix is removed. If Suffix is non-empty and is a
   --  suffix of Path, it is removed. This is equivalent to the UNIX basename
   --  command. The following rule is always true:
   --
   --    'Path' and 'Dir_Name (Path) & Directory_Separator & Base_Name (Path)'
   --    represent the same file.
   --
   --  This function is not case-sensitive on systems that have a non
   --  case-sensitive file system like Windows and OpenVMS.

   function File_Extension (Path : Path_Name) return String;
   --  Return the file extension. This is defined as the string after the
   --  last dot, including the dot itself. For example, if the file name
   --  is "file1.xyz.adq", then the returned value would be ".adq". If no
   --  dot is present in the file name, or the last character of the file
   --  name is a dot, then the null string is returned.

   function File_Name (Path : Path_Name) return String;
   --  Returns the file name and the file extension if present. It removes all
   --  path information. This is equivalent to Base_Name with default Extension
   --  value.

   type Path_Style is
     (UNIX,
      --  Use '/' as the directory separator. The default on Unix systems
      --  and on OpenVMS.

      DOS,
      --  Use '\' as the directory separator. The default on Windows.

      System_Default);

   function Format_Pathname
     (Path  : Path_Name;
      Style : Path_Style := System_Default) return Path_Name;
   --  Removes all double directory separator and converts all '\' to '/' if
   --  Style is UNIX and converts all '/' to '\' if Style is set to DOS. This
   --  function will help to provide a consistent naming scheme running for
   --  different environments. If style is set to System_Default the routine
   --  will use the default directory separator on the running environment.

   type Environment_Style is
     (UNIX,
      --  Environment variables and OpenVMS logical names use $ as prefix and
      --  can use curly brackets as in ${HOME}/mydir. If there is no closing
      --  curly bracket for an opening one then translation is done, so for
      --  example ${VAR/toto is returned as ${VAR/toto.

      DOS,
      --  Environment variables uses % as prefix and suffix
      --  (e.g. %HOME%/mydir). The name DOS refer to "DOS-like" environment.
      --  This includes al Windows systems.

      Both,
      --  Recognize both forms described above.

      System_Default);
      --  Uses either UNIX on Unix and OpenVMS systems, or DOS on Windows and
      --  OS/2 depending on the running environment.

   function Expand_Path
     (Path : Path_Name;
      Mode : Environment_Style := System_Default) return Path_Name;
   --  Returns Path with environment variables (or logical names on OpenVMS)
   --  replaced by the current environment variable value. For example,
   --  $HOME/mydir will be replaced by /home/joe/mydir if $HOME environment
   --  variable is set to /home/joe and Mode is UNIX. If an environment
   --  variable does not exists the variable will be replaced by the empty
   --  string. Two dollar or percent signs are replaced by a single
   --  dollar/percent sign. Note that a variable must start with a letter.

   ---------------
   -- Iterators --
   ---------------

   procedure Open (Dir : out Dir_Type; Dir_Name : Dir_Name_Str);
   --  Opens the directory named by Dir_Name and returns a Dir_Type value
   --  that refers to this directory, and is positioned at the first entry.
   --  Raises Directory_Error if Dir_Name cannot be accessed. In that case
   --  Dir will be set to Null_Dir.

   procedure Close (Dir : in out Dir_Type);
   --  Closes the directory stream refered to by Dir. After calling Close
   --  Is_Open will return False. Dir will be set to Null_Dir.
   --  Raises Directory_Error if Dir has not be opened (Dir = Null_Dir).

   function Is_Open (Dir : Dir_Type) return Boolean;
   --  Returns True if Dir is open, or False otherwise.

   procedure Read
     (Dir  : in out Dir_Type;
      Str  : out String;
      Last : out Natural);
   --  Reads the next entry from the directory and sets Str to the name
   --  of that entry. Last is the index in Str such that Str (Last) is the
   --  last character written. Last is 0 when there are no more files in the
   --  directory. If Str is too small for the file name, the file name will
   --  be truncated before being copied to Str. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries. The directory is
   --  returned in target-OS form. Raises Directory_Error if Dir has not
   --  be opened (Dir = Null_Dir).

   function Read_Is_Thread_Safe return Boolean;
   --  Indicates if procedure Read is thread safe. On systems where the
   --  target system supports this functionality, Read is thread safe,
   --  and this function returns True (e.g. this will be the case on any
   --  UNIX or UNIX-like system providing a correct implementation of the
   --  function readdir_r). If the system cannot provide a thread safe
   --  implementation of Read, then this function returns False.

private

   type Dir_Type_Value;
   type Dir_Type is access Dir_Type_Value;

   Null_Dir : constant Dir_Type := null;

   pragma Import (C, Dir_Separator, "__gnat_dir_separator");

   Dir_Seps : constant Ada.Strings.Maps.Character_Set :=
                Ada.Strings.Maps.To_Set ("/\");
   --  UNIX and DOS style directory separators.

end GNAT.Directory_Operations;
