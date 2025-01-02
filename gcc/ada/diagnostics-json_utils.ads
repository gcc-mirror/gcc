------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               D I A G N O S T I C S . J S O N _ U T I L S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

package Diagnostics.JSON_Utils is

   JSON_FORMATTING : constant Boolean := True;
   --  Adds newlines and indentation to the output JSON.
   --
   --  NOTE: This flag could be associated with the gcc switch:
   --  '-fno-diagnostics-json-formatting'

   INDENT_SIZE : constant := 2;
   --  The number of spaces to indent each level of the JSON output.

   Indent_Level : Natural := 0;
   --  The current indentation level.

   procedure Begin_Block;
   --  Increase the indentation level by one

   procedure End_Block;
   --  Decrease the indentation level by one

   procedure Indent;
   --  Print the indentation for the line

   procedure NL_And_Indent;
   --  Print a new line

   procedure Write_Boolean_Attribute (Name : String; Value : Boolean);
   --  Write a JSON attribute with a boolean value.
   --
   --  The value is either 'true' or 'false' without any quotes

   procedure Write_Int_Attribute (Name : String; Value : Int);

   procedure Write_JSON_Escaped_String (Str : String);
   --  Write each character of Str, taking care of preceding each quote and
   --  backslash with a backslash. Note that this escaping differs from what
   --  GCC does.
   --
   --  Indeed, the JSON specification mandates encoding wide characters
   --  either as their direct UTF-8 representation or as their escaped
   --  UTF-16 surrogate pairs representation. GCC seems to prefer escaping -
   --  we choose to use the UTF-8 representation instead.

   procedure Write_String_Attribute (Name : String; Value : String);
   --  Write a JSON attribute with a string value.
   --
   --  The Value is surrounded by double quotes ("") and the special characters
   --  within the string are escaped.

end Diagnostics.JSON_Utils;
