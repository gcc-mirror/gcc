------------------------------------------------------------------------------
--                                                                          --
--                        GNAAMP COMPILER COMPONENTS                        --
--                                                                          --
--                              A A _ U T I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2001-2011, AdaCore                     --
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
------------------------------------------------------------------------------

--  This package provides various utility operations used by GNAT back-ends
--  (e.g. AAMP).

--  This package is a messy grab bag of stuff. These routines should be moved
--  to appropriate units (sem_util,sem_aux,exp_util,namet,uintp,urealp). ???

with Namet;  use Namet;
with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package AA_Util is

   function Is_Global_Entity (E : Entity_Id) return Boolean;
   --  Returns true if and only if E is a library-level entity (excludes
   --  entities declared within blocks at the outer level of library packages).

   function New_Name_Id (Name : String) return Name_Id;
   --  Returns a Name_Id corresponding to the given name string

   function Name_String (Name : Name_Id) return String;
   --  Returns the name string associated with Name

   function New_String_Id (S : String) return String_Id;
   --  Returns a String_Id corresponding to the given string

   function String_Value (Str_Id : String_Id) return String;
   --  Returns the string associated with Str_Id

   --  Name-generation utilities

   type Name_Sequencer is private;
   --  This type is used to support back-end generation of unique symbol
   --  (e.g., for string literal objects or labels). By declaring an
   --  aliased object of type Name_Sequence and passing that object
   --  to the function Next_Name, a series of names with suffixes
   --  of the form "__n" will be produced, where n is a string denoting
   --  a positive integer.  The sequence starts with "__1", and increases
   --  by one on each successive call to Next_Name for a given Name_Sequencer.

   function Next_Name
     (Name_Seq    : not null access Name_Sequencer;
      Name_Prefix : String) return Name_Id;
   --  Returns the Name_Id for a name composed of the given Name_Prefix
   --  concatentated with a unique number suffix of the form "__n",
   --  as detemined by the current state of Name_Seq.

   function Elab_Spec_Name (Module_Name : Name_Id) return Name_Id;
   --  Returns a name id for the elaboration subprogram to be associated with
   --  the specification of the named module. The denoted name is of the form
   --  "modulename___elabs".

   function Elab_Body_Name (Module_Name : Name_Id) return Name_Id;
   --  Returns a name id for the elaboration subprogram to be associated
   --  with the body of the named module. The denoted name is of the form
   --  "modulename___elabb".

   function File_Name_Without_Suffix (File_Name : String) return String;
   --  Removes the suffix ('.' followed by other characters), if present, from
   --  the end of File_Name and returns the shortened name (otherwise simply
   --  returns File_Name).

   function Source_Name (Sloc : Source_Ptr) return File_Name_Type;
   --  Returns file name corresponding to the source file name associated with
   --  the given source position Sloc.

   function Source_Name_Without_Suffix (Sloc : Source_Ptr) return String;
   --  Returns a string corresponding to the source file name associated with
   --  the given source position Sloc, with its dot-preceded suffix, if any,
   --  removed. As examples, the name "main.adb" is mapped to "main" and the
   --  name "main.2.ada" is mapped to "main.2". As a special case, file names
   --  with a ".dg" suffix will also strip off the ".dg", so "main.adb.dg"
   --  becomes simply "main".

   function Source_Id_String (Unit_Name : Name_Id) return String;
   --  Returns a string that uniquely identifies the unit with the given
   --  Unit_Name. This string is derived from Unit_Name by replacing any
   --  multiple underscores with dot ('.') characters and normalizing the
   --  casing to mixed case (e.g., "ada__strings" is mapped to ("Ada.Strings").

   function Source_Id (Unit_Name : Name_Id) return String_Id;
   --  Returns a String_Id reference to a string that uniquely identifies
   --  the program unit having the given name (as defined for function
   --  Source_Id_String).

   function Source_Id_String (Sloc : Source_Ptr) return String;
   --  Returns a string that uniquely identifies the source file containing
   --  the given source location.  This string is constructed from the
   --  concatentation of the date and time stamp of the file with a
   --  hexadecimal check sum (e.g., "020425143059ABCDEF01").

   function Source_Id (Sloc : Source_Ptr) return String_Id;
   --  Returns a String_Id reference to a string that uniquely identifies the
   --  source file containing the given source location (as defined for
   --  function Source_Id_String).

   function Image (I : Int) return String;
   --  Returns Int'Image (I), but without a leading space in the case where
   --  I is nonnegative. Useful for concatenating integers onto other names.

   type Integer_Image_Format is (Decimal, Ada_Hex, AAMP_Hex);

   function UI_Image (I : Uint; Format : Integer_Image_Format) return String;
   --  Returns the image of the universal integer I, with no leading spaces
   --  and in the format specified. The Format parameter specifies whether
   --  the integer representation should be decimal (the default), or Ada
   --  hexadecimal (Ada_Hex => "16#xxxxx#" format), or AAMP hexadecimal.
   --  In the latter case, the integer will have the form of a sequence of
   --  hexadecimal digits bracketed by '^' characters, and will contain '_'
   --  characters as separators for groups of four hexadecimal digits
   --  (e.g., ^1C_A3CD^). If the format AAMP_Hex is selected, the universal
   --  integer must have a nonnegative value.

   function UR_Image (R : Ureal) return String;
   --  Returns a decimal image of the universal real value R

private

   type Name_Sequencer is record
      Sequence_Number : Natural := 0;
   end record;

end AA_Util;
