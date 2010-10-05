------------------------------------------------------------------------------
--                                                                        --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      A D A . D I R E C T O R I E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived for use with GNAT from AI-00248,  which is --
-- expected to be a part of a future expected revised Ada Reference Manual. --
-- The copyright notice above, and the license provisions that follow apply --
-- solely to the  contents of the part following the private keyword.       --
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

--  Ada 2005: Implementation of Ada.Directories (AI95-00248). Note that this
--  unit is available without -gnat05. That seems reasonable, since you only
--  get it if you explicitly ask for it.

--  External files may be classified as directories, special files, or ordinary
--  files. A directory is an external file that is a container for files on
--  the target system. A special file is an external file that cannot be
--  created or read by a predefined Ada Input-Output package. External files
--  that are not special files or directories are called ordinary files.

--  A file name is a string identifying an external file. Similarly, a
--  directory name is a string identifying a directory. The interpretation of
--  file names and directory names is implementation-defined.

--  The full name of an external file is a full specification of the name of
--  the file. If the external environment allows alternative specifications of
--  the name (for example, abbreviations), the full name should not use such
--  alternatives. A full name typically will include the names of all of
--  directories that contain the item. The simple name of an external file is
--  the name of the item, not including any containing directory names. Unless
--  otherwise specified, a file name or directory name parameter to a
--  predefined Ada input-output subprogram can be a full name, a simple name,
--  or any other form of name supported by the implementation.

--  The default directory is the directory that is used if a directory or
--  file name is not a full name (that is, when the name does not fully
--  identify all of the containing directories).

--  A directory entry is a single item in a directory, identifying a single
--  external file (including directories and special files).

--  For each function that returns a string, the lower bound of the returned
--  value is 1.

with Ada.Calendar;
with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;

package Ada.Directories is

   -----------------------------------
   -- Directory and File Operations --
   -----------------------------------

   function Current_Directory return String;
   --  Returns the full directory name for the current default directory. The
   --  name returned shall be suitable for a future call to Set_Directory.
   --  The exception Use_Error is propagated if a default directory is not
   --  supported by the external environment.

   procedure Set_Directory (Directory : String);
   --  Sets the current default directory. The exception Name_Error is
   --  propagated if the string given as Directory does not identify an
   --  existing directory. The exception Use_Error is propagated if the
   --  external environment does not support making Directory (in the absence
   --  of Name_Error) a default directory.

   procedure Create_Directory
     (New_Directory : String;
      Form          : String := "");
   --  Creates a directory with name New_Directory. The Form parameter can be
   --  used to give system-dependent characteristics of the directory; the
   --  interpretation of the Form parameter is implementation-defined. A null
   --  string for Form specifies the use of the default options of the
   --  implementation of the new directory. The exception Name_Error is
   --  propagated if the string given as New_Directory does not allow the
   --  identification of a directory. The exception Use_Error is propagated if
   --  the external environment does not support the creation of a directory
   --  with the given name (in the absence of Name_Error) and form.
   --
   --  The Form parameter is ignored

   procedure Delete_Directory (Directory : String);
   --  Deletes an existing empty directory with name Directory. The exception
   --  Name_Error is propagated if the string given as Directory does not
   --  identify an existing directory. The exception Use_Error is propagated
   --  if the external environment does not support the deletion of the
   --  directory (or some portion of its contents) with the given name (in the
   --  absence of Name_Error).

   procedure Create_Path
     (New_Directory : String;
      Form          : String := "");
   --  Creates zero or more directories with name New_Directory. Each
   --  non-existent directory named by New_Directory is created. For example,
   --  on a typical Unix system, Create_Path ("/usr/me/my"); would create
   --  directory "me" in directory "usr", then create directory "my" in
   --  directory "me". The Form can be used to give system-dependent
   --  characteristics of the directory; the interpretation of the Form
   --  parameter is implementation-defined. A null string for Form specifies
   --  the use of the default options of the implementation of the new
   --  directory. The exception Name_Error is propagated if the string given
   --  as New_Directory does not allow the identification of any directory.
   --  The exception Use_Error is propagated if the external environment does
   --  not support the creation of any directories with the given name (in the
   --  absence of Name_Error) and form.
   --
   --  The Form parameter is ignored

   procedure Delete_Tree (Directory : String);
   --  Deletes an existing directory with name Directory. The directory and
   --  all of its contents (possibly including other directories) are deleted.
   --  The exception Name_Error is propagated if the string given as Directory
   --  does not identify an existing directory. The exception Use_Error is
   --  propagated if the external environment does not support the deletion of
   --  the directory or some portion of its contents with the given name (in
   --  the absence of Name_Error). If Use_Error is propagated, it is
   --  unspecified if a portion of the contents of the directory are deleted.

   procedure Delete_File (Name : String);
   --  Deletes an existing ordinary or special file with Name. The exception
   --  Name_Error is propagated if the string given as Name does not identify
   --  an existing ordinary or special external file. The exception Use_Error
   --  is propagated if the external environment does not support the deletion
   --  of the file with the given name (in the absence of Name_Error).

   procedure Rename (Old_Name, New_Name : String);
   --  Renames an existing external file (including directories) with Old_Name
   --  to New_Name. The exception Name_Error is propagated if the string given
   --  as Old_Name does not identify an existing external file. The exception
   --  Use_Error is propagated if the external environment does not support the
   --  renaming of the file with the given name (in the absence of Name_Error).
   --  In particular, Use_Error is propagated if a file or directory already
   --  exists with New_Name.

   procedure Copy_File
     (Source_Name   : String;
      Target_Name   : String;
      Form          : String := "");
   --  Copies the contents of the existing external file with Source_Name to
   --  Target_Name. The resulting external file is a duplicate of the source
   --  external file. The Form argument can be used to give system-dependent
   --  characteristics of the resulting external file; the interpretation of
   --  the Form parameter is implementation-defined. Exception Name_Error is
   --  propagated if the string given as Source_Name does not identify an
   --  existing external ordinary or special file or if the string given as
   --  Target_Name does not allow the identification of an external file. The
   --  exception Use_Error is propagated if the external environment does not
   --  support the creating of the file with the name given by Target_Name and
   --  form given by Form, or copying of the file with the name given by
   --  Source_Name (in the absence of Name_Error).
   --
   --  Interpretation of the Form parameter:
   --
   --    The Form parameter is case-insensitive
   --
   --    Two fields are recognized in the Form parameter:
   --      preserve=<value>
   --      mode=<value>
   --
   --      <value> starts immediately after the character '=' and ends with the
   --      character immediately preceding the next comma (',') or with the
   --      last character of the parameter.
   --
   --      The allowed values for preserve= are:
   --
   --        no_attributes:  Do not try to preserve any file attributes. This
   --                        is the default if no preserve= is found in Form.
   --
   --        all_attributes: Try to preserve all file attributes (timestamps,
   --                        access rights).
   --
   --        timestamps:     Preserve the timestamp of the copied file, but not
   --                        the other file attributes.
   --
   --      The allowed values for mode= are:
   --
   --        copy:           Only copy if the destination file does not already
   --                        exist. If it already exists, Copy_File will fail.
   --
   --        overwrite:      Copy the file in all cases. Overwite an already
   --                        existing destination file. This is the default if
   --                        no mode= is found in Form.
   --
   --        append:         Append the original file to the destination file.
   --                        If the destination file does not exist, the
   --                        destination file is a copy of the source file.
   --                        When mode=append, the field preserve=, if it
   --                        exists, is not taken into account.
   --
   --    If the Form parameter includes one or both of the fields and the value
   --    or values are incorrect, Copy_File fails with Use_Error.
   --
   --    Examples of correct Forms:
   --       Form => "preserve=no_attributes,mode=overwrite" (the default)
   --       Form => "mode=append"
   --       Form => "mode=copy,preserve=all_attributes"
   --
   --    Examples of incorrect Forms:
   --       Form => "preserve=junk"
   --       Form => "mode=internal,preserve=timestamps"

   ----------------------------------------
   -- File and directory name operations --
   ----------------------------------------

   function Full_Name (Name : String) return String;
   --  Returns the full name corresponding to the file name specified by Name.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file (including directories
   --  and special files).

   function Simple_Name (Name : String) return String;
   --  Returns the simple name portion of the file name specified by Name. The
   --  exception Name_Error is propagated if the string given as Name does not
   --  allow the identification of an external file (including directories and
   --  special files).

   function Containing_Directory (Name : String) return String;
   --  Returns the name of the containing directory of the external file
   --  (including directories) identified by Name. If more than one directory
   --  can contain Name, the directory name returned is implementation-defined.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file. The exception
   --  Use_Error is propagated if the external file does not have a containing
   --  directory.

   function Extension (Name : String) return String;
   --  Returns the extension name corresponding to Name. The extension name is
   --  a portion of a simple name (not including any separator characters),
   --  typically used to identify the file class. If the external environment
   --  does not have extension names, then the null string is returned.
   --  The exception Name_Error is propagated if the string given as Name does
   --  not allow the identification of an external file.

   function Base_Name (Name : String) return String;
   --  Returns the base name corresponding to Name. The base name is the
   --  remainder of a simple name after removing any extension and extension
   --  separators. The exception Name_Error is propagated if the string given
   --  as Name does not allow the identification of an external file
   --  (including directories and special files).

   function Compose
     (Containing_Directory : String := "";
      Name                 : String;
      Extension            : String := "") return String;
   --  Returns the name of the external file with the specified
   --  Containing_Directory, Name, and Extension. If Extension is the null
   --  string, then Name is interpreted as a simple name; otherwise Name is
   --  interpreted as a base name. The exception Name_Error is propagated if
   --  the string given as Containing_Directory is not null and does not allow
   --  the identification of a directory, or if the string given as Extension
   --  is not null and is not a possible extension, or if the string given as
   --  Name is not a possible simple name (if Extension is null) or base name
   --  (if Extension is non-null).

   --------------------------------
   -- File and directory queries --
   --------------------------------

   type File_Kind is (Directory, Ordinary_File, Special_File);
   --  The type File_Kind represents the kind of file represented by an
   --  external file or directory.

   type File_Size is range 0 .. Long_Long_Integer'Last;
   --  The type File_Size represents the size of an external file

   function Exists (Name : String) return Boolean;
   --  Returns True if external file represented by Name exists, and False
   --  otherwise. The exception Name_Error is propagated if the string given as
   --  Name does not allow the identification of an external file (including
   --  directories and special files).

   function Kind (Name : String) return File_Kind;
   --  Returns the kind of external file represented by Name. The exception
   --  Name_Error is propagated if the string given as Name does not allow the
   --  identification of an existing external file.

   function Size (Name : String) return File_Size;
   --  Returns the size of the external file represented by Name. The size of
   --  an external file is the number of stream elements contained in the file.
   --  If the external file is discontiguous (not all elements exist), the
   --  result is implementation-defined. If the external file is not an
   --  ordinary file, the result is implementation-defined. The exception
   --  Name_Error is propagated if the string given as Name does not allow the
   --  identification of an existing external file. The exception
   --  Constraint_Error is propagated if the file size is not a value of type
   --  File_Size.

   function Modification_Time (Name : String) return Ada.Calendar.Time;
   --  Returns the time that the external file represented by Name was most
   --  recently modified. If the external file is not an ordinary file, the
   --  result is implementation-defined. The exception Name_Error is propagated
   --  if the string given as Name does not allow the identification of an
   --  existing external file. The exception Use_Error is propagated if the
   --  external environment does not support the reading the modification time
   --  of the file with the name given by Name (in the absence of Name_Error).

   -------------------------
   -- Directory Searching --
   -------------------------

   type Directory_Entry_Type is limited private;
   --  The type Directory_Entry_Type represents a single item in a directory.
   --  These items can only be created by the Get_Next_Entry procedure in this
   --  package. Information about the item can be obtained from the functions
   --  declared in this package. A default initialized object of this type is
   --  invalid; objects returned from Get_Next_Entry are valid.

   type Filter_Type is array (File_Kind) of Boolean;
   --  The type Filter_Type specifies which directory entries are provided from
   --  a search operation. If the Directory component is True, directory
   --  entries representing directories are provided. If the Ordinary_File
   --  component is True, directory entries representing ordinary files are
   --  provided. If the Special_File component is True, directory entries
   --  representing special files are provided.

   type Search_Type is limited private;
   --  The type Search_Type contains the state of a directory search. A
   --  default-initialized Search_Type object has no entries available
   --  (More_Entries returns False).

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True));
   --  Starts a search in the directory entry in the directory named by
   --  Directory for entries matching Pattern. Pattern represents a file name
   --  matching pattern. If Pattern is null, all items in the directory are
   --  matched; otherwise, the interpretation of Pattern is implementation-
   --  defined. Only items which match Filter will be returned. After a
   --  successful call on Start_Search, the object Search may have entries
   --  available, but it may have no entries available if no files or
   --  directories match Pattern and Filter. The exception Name_Error is
   --  propagated if the string given by Directory does not identify an
   --  existing directory, or if Pattern does not allow the identification of
   --  any possible external file or directory. The exception Use_Error is
   --  propagated if the external environment does not support the searching
   --  of the directory with the given name (in the absence of Name_Error).

   procedure End_Search (Search : in out Search_Type);
   --  Ends the search represented by Search. After a successful call on
   --  End_Search, the object Search will have no entries available. Note
   --  that it is not necessary to call End_Search if the call to Start_Search
   --  was unsuccessful and raised an exception (but it is harmless to make
   --  the call in this case).

   function More_Entries (Search : Search_Type) return Boolean;
   --  Returns True if more entries are available to be returned by a call
   --  to Get_Next_Entry for the specified search object, and False otherwise.

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type);
   --  Returns the next Directory_Entry for the search described by Search that
   --  matches the pattern and filter. If no further matches are available,
   --  Status_Error is raised. It is implementation-defined as to whether the
   --  results returned by this routine are altered if the contents of the
   --  directory are altered while the Search object is valid (for example, by
   --  another program). The exception Use_Error is propagated if the external
   --  environment does not support continued searching of the directory
   --  represented by Search.

   procedure Search
     (Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True);
      Process   : not null access procedure
                                    (Directory_Entry : Directory_Entry_Type));
   --  Searches in the directory named by Directory for entries matching
   --  Pattern. The subprogram designated by Process is called with each
   --  matching entry in turn. Pattern represents a pattern for matching file
   --  names. If Pattern is null, all items in the directory are matched;
   --  otherwise, the interpretation of Pattern is implementation-defined.
   --  Only items that match Filter will be returned. The exception Name_Error
   --  is propagated if the string given by Directory does not identify
   --  an existing directory, or if Pattern does not allow the identification
   --  of any possible external file or directory. The exception Use_Error is
   --  propagated if the external environment does not support the searching
   --  of the directory with the given name (in the absence of Name_Error).

   -------------------------------------
   -- Operations on Directory Entries --
   -------------------------------------

   function Simple_Name (Directory_Entry : Directory_Entry_Type) return String;
   --  Returns the simple external name of the external file (including
   --  directories) represented by Directory_Entry. The format of the name
   --  returned is implementation-defined. The exception Status_Error is
   --  propagated if Directory_Entry is invalid.

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String;
   --  Returns the full external name of the external file (including
   --  directories) represented by Directory_Entry. The format of the name
   --  returned is implementation-defined. The exception Status_Error is
   --  propagated if Directory_Entry is invalid.

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind;
   --  Returns the kind of external file represented by Directory_Entry. The
   --  exception Status_Error is propagated if Directory_Entry is invalid.

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size;
   --  Returns the size of the external file represented by Directory_Entry.
   --  The size of an external file is the number of stream elements contained
   --  in the file. If the external file is discontiguous (not all elements
   --  exist), the result is implementation-defined. If the external file
   --  represented by Directory_Entry is not an ordinary file, the result is
   --  implementation-defined. The exception Status_Error is propagated if
   --  Directory_Entry is invalid. The exception Constraint_Error is propagated
   --  if the file size is not a value of type File_Size.

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time;
   --  Returns the time that the external file represented by Directory_Entry
   --  was most recently modified. If the external file represented by
   --  Directory_Entry is not an ordinary file, the result is
   --  implementation-defined. The exception Status_Error is propagated if
   --  Directory_Entry is invalid. The exception Use_Error is propagated if
   --  the external environment does not support the reading the modification
   --  time of the file represented by Directory_Entry.

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

private
   type Directory_Entry_Type is record
      Is_Valid : Boolean := False;
      Simple   : Ada.Strings.Unbounded.Unbounded_String;
      Full     : Ada.Strings.Unbounded.Unbounded_String;
      Kind     : File_Kind := Ordinary_File;
   end record;

   --  The type Search_Data is defined in the body, so that the spec does not
   --  depend on packages of the GNAT hierarchy.

   type Search_Data;
   type Search_Ptr is access Search_Data;

   --  Search_Type need to be a controlled type, because it includes component
   --  of type Dir_Type (in GNAT.Directory_Operations) that need to be closed
   --  (if opened) during finalization. The component need to be an access
   --  value, because Search_Data is not fully defined in the spec.

   type Search_Type is new Ada.Finalization.Controlled with record
      Value : Search_Ptr;
   end record;

   procedure Finalize (Search : in out Search_Type);
   --  Close the directory, if opened, and deallocate Value

   procedure End_Search (Search : in out Search_Type) renames Finalize;

end Ada.Directories;
