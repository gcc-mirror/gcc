------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . U T I L                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2001-2005 Free Software Foundation, Inc.       --
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

--  Utilities for use in processing project files

with Types; use Types;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Prj.Util is

   function Executable_Of
     (Project  : Project_Id;
      In_Tree  : Project_Tree_Ref;
      Main     : Name_Id;
      Index    : Int;
      Ada_Main : Boolean := True) return Name_Id;
   --  Return the value of the attribute Builder'Executable for file Main in
   --  the project Project, if it exists. If there is no attribute Executable
   --  for Main, remove the suffix from Main; then, if the attribute
   --  Executable_Suffix is specified, add this suffix, otherwise add the
   --  standard executable suffix for the platform.

   function Value_Of
     (Variable : Variable_Value;
      Default  : String) return String;
   --  Get the value of a single string variable. If Variable is
   --  Nil_Variable_Value, is a string list or is defaulted, return Default.

   function Value_Of
     (Index    : Name_Id;
      In_Array : Array_Element_Id;
      In_Tree  : Project_Tree_Ref) return Name_Id;
   --  Get a single string array component. Returns No_Name if there is no
   --  component Index, if In_Array is null, or if the component is a String
   --  list. Depending on the attribute (only attributes may be associative
   --  arrays) the index may or may not be case sensitive. If the index is not
   --  case sensitive, it is first set to lower case before the search in the
   --  associative array.

   function Value_Of
     (Index     : Name_Id;
      Src_Index : Int := 0;
      In_Array  : Array_Element_Id;
      In_Tree   : Project_Tree_Ref) return Variable_Value;
   --  Get a string array component (single String or String list).
   --  Returns Nil_Variable_Value if there is no component Index
   --  or if In_Array is null.
   --
   --  Depending on the attribute (only attributes may be associative arrays)
   --  the index may or may not be case sensitive. If the index is not
   --  case sensitive, it is first set to lower case before the search
   --  in the associative array.

   function Value_Of
     (Name                    : Name_Id;
      Index                   : Int := 0;
      Attribute_Or_Array_Name : Name_Id;
      In_Package              : Package_Id;
      In_Tree                 : Project_Tree_Ref) return Variable_Value;
   --  In a specific package,
   --   - if there exists an array Attribute_Or_Array_Name with an index
   --     Name, returns the corresponding component (depending on the
   --     attribute, the index may or may not be case sensitive, see previous
   --     function),
   --   - otherwise if there is a single attribute Attribute_Or_Array_Name,
   --     returns this attribute,
   --   - otherwise, returns Nil_Variable_Value.
   --  If In_Package is null, returns Nil_Variable_Value.

   function Value_Of
     (Index     : Name_Id;
      In_Array  : Name_Id;
      In_Arrays : Array_Id;
      In_Tree   : Project_Tree_Ref) return Name_Id;
   --  Get a string array component in an array of an array list.
   --  Returns No_Name if there is no component Index, if In_Arrays is null, if
   --  In_Array is not found in In_Arrays or if the component is a String list.

   function Value_Of
     (Name      : Name_Id;
      In_Arrays : Array_Id;
      In_Tree   : Project_Tree_Ref) return Array_Element_Id;
   --  Returns a specified array in an array list. Returns No_Array_Element
   --  if In_Arrays is null or if Name is not the name of an array in
   --  In_Arrays. The caller must ensure that Name is in lower case.

   function Value_Of
     (Name        : Name_Id;
      In_Packages : Package_Id;
      In_Tree     : Project_Tree_Ref) return Package_Id;
   --  Returns a specified package in a package list. Returns No_Package
   --  if In_Packages is null or if Name is not the name of a package in
   --  Package_List. The caller must ensure that Name is in lower case.

   function Value_Of
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id;
      In_Tree       : Project_Tree_Ref) return Variable_Value;
   --  Returns a specified variable in a variable list. Returns null if
   --  In_Variables is null or if Variable_Name is not the name of a
   --  variable in In_Variables. Caller must ensure that Name is lower case.

   procedure Write_Str
     (S          : String;
      Max_Length : Positive;
      Separator  : Character);
   --  Output string S using Output.Write_Str. If S is too long to fit in
   --  one line of Max_Length, cut it in several lines, using Separator as
   --  the last character of each line, if possible.

   type Text_File is limited private;
   --  Represents a text file. Default is invalid text file.

   function Is_Valid (File : Text_File) return Boolean;
   --  Returns True if File designates an open text file that
   --  has not yet been closed.

   procedure Open (File : out Text_File; Name : String);
   --  Open a text file. If this procedure fails, File is invalid.

   function End_Of_File (File : Text_File) return Boolean;
   --  Returns True if the end of the text file File has been
   --  reached. Fails if File is invalid.

   procedure Get_Line
     (File : Text_File;
      Line : out String;
      Last : out Natural);
   --  Reads a line from an open text file. Fails if File is invalid.

   procedure Close (File : in out Text_File);
   --  Close an open text file. File becomes invalid.
   --  Fails if File is already invalid.

private

   type Text_File_Data is record
      FD                  : File_Descriptor := Invalid_FD;
      Buffer              : String (1 .. 1_000);
      Buffer_Len          : Natural;
      Cursor              : Natural := 0;
      End_Of_File_Reached : Boolean := False;
   end record;

   type Text_File is access Text_File_Data;

end Prj.Util;
