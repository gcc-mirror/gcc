------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2020, AdaCore                     --
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

--  Directory operations

--  This package provides routines for manipulating directories. A directory
--  can be treated as a file, using open and close routines, and a scanning
--  routine is provided for iterating through the entries in a directory.

--  See also child package GNAT.Directory_Operations.Iteration

with System;
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
   --  Returns the current working directory for the execution environment

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
   --  case-sensitive (such as Windows).

   function Base_Name
     (Path   : Path_Name;
      Suffix : String := "") return String;
   --  Any directory prefix is removed. A directory prefix is defined as
   --  text up to and including the last directory separator character in
   --  the input string. In addition if Path ends with the string given for
   --  Suffix, then it is also removed. Note that Suffix here can be an
   --  arbitrary string (it is not required to be a file extension). This
   --  is equivalent to the UNIX basename command. The following rule is
   --  always true:
   --
   --    'Path' and 'Dir_Name (Path) & Dir_Separator & Base_Name (Path)'
   --    represent the same file.
   --
   --  The comparison of Suffix is case-insensitive on systems like Windows
   --  where the file search is case-insensitive (e.g. on such systems,
   --  Base_Name ("/Users/AdaCore/BB12.patch", ".Patch") returns "BB12").
   --
   --  Note that the index bounds of the result match the corresponding indexes
   --  in the Path string (you cannot assume that the lower bound of the
   --  returned string is one).

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

   type Path_Style is (UNIX, DOS, System_Default);
   function Format_Pathname
     (Path  : Path_Name;
      Style : Path_Style := System_Default) return Path_Name;
   --  Removes all double directory separator and converts all '\' to '/' if
   --  Style is UNIX and converts all '/' to '\' if Style is set to DOS. This
   --  function will help to provide a consistent naming scheme running for
   --  different environments. If style is set to System_Default the routine
   --  will use the default directory separator on the running environment.
   --
   --  The Style argument indicates the syntax to be used for path names:
   --
   --    DOS
   --      Use '\' as the directory separator (default on Windows)
   --
   --    UNIX
   --      Use '/' as the directory separator (default on all other systems)
   --
   --    System_Default
   --      Use the default style for the current system

   type Environment_Style is (UNIX, DOS, Both, System_Default);
   function Expand_Path
     (Path : Path_Name;
      Mode : Environment_Style := System_Default) return Path_Name;
   --  Returns Path with environment variables replaced by the current
   --  environment variable value. For example, $HOME/mydir will be replaced
   --  by /home/joe/mydir if $HOME environment variable is set to /home/joe and
   --  Mode is UNIX. If an environment variable does not exist the variable
   --  will be replaced by the empty string. Two dollar or percent signs are
   --  replaced by a single dollar/percent sign. Note that a variable must
   --  start with a letter.
   --
   --  The Mode argument indicates the recognized syntax for environment
   --  variables as follows:
   --
   --    UNIX
   --      Environment variables use $ as prefix and can use curly brackets
   --      as in ${HOME}/mydir. If there is no closing curly bracket for an
   --      opening one then no translation is done, so for example ${VAR/toto
   --      is returned as ${VAR/toto. The use of {} brackets is required if
   --      the environment variable name contains other than alphanumeric
   --      characters.
   --
   --    DOS
   --      Environment variables uses % as prefix and suffix (e.g. %HOME%/dir).
   --      The name DOS refer to "DOS-like" environment. This includes all
   --      Windows systems.
   --
   --    Both
   --      Recognize both forms described above.
   --
   --    System_Default
   --      Uses either DOS on Windows, and UNIX on all other systems, depending
   --      on the running environment.

   ---------------
   -- Iterators --
   ---------------

   procedure Open (Dir : out Dir_Type; Dir_Name : Dir_Name_Str);
   --  Opens the directory named by Dir_Name and returns a Dir_Type value
   --  that refers to this directory, and is positioned at the first entry.
   --  Raises Directory_Error if Dir_Name cannot be accessed. In that case
   --  Dir will be set to Null_Dir.

   procedure Close (Dir : in out Dir_Type);
   --  Closes the directory stream referred to by Dir. After calling Close
   --  Is_Open will return False. Dir will be set to Null_Dir.
   --  Raises Directory_Error if Dir has not be opened (Dir = Null_Dir).

   function Is_Open (Dir : Dir_Type) return Boolean;
   --  Returns True if Dir is open, or False otherwise

   procedure Read
     (Dir  : Dir_Type;
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

   type Dir_Type_Value is new System.Address;
   --  Low-level address directory structure as returned by opendir in C

   type Dir_Type is access Dir_Type_Value;

   Null_Dir : constant Dir_Type := null;

   pragma Import (C, Dir_Separator, "__gnat_dir_separator");

   Dir_Seps : constant Ada.Strings.Maps.Character_Set :=
                Ada.Strings.Maps.To_Set ("/\");
   --  UNIX and DOS style directory separators

end GNAT.Directory_Operations;
