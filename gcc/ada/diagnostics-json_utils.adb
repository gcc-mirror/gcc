------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               D I A G N O S T I C S . J S O N _ U T I L S                --
--                                                                          --
--                                 B o d y                                  --
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
with Output; use Output;

package body Diagnostics.JSON_Utils is

   -----------------
   -- Begin_Block --
   -----------------

   procedure Begin_Block is
   begin
      Indent_Level := Indent_Level + 1;
   end Begin_Block;

   ---------------
   -- End_Block --
   ---------------

   procedure End_Block is
   begin
      Indent_Level := Indent_Level - 1;
   end End_Block;

   procedure Indent is begin
      if JSON_FORMATTING then
         for I in 1 .. INDENT_SIZE * Indent_Level loop
            Write_Char (' ');
         end loop;
      end if;
   end Indent;

   -------------------
   -- NL_And_Indent --
   -------------------

   procedure NL_And_Indent is
   begin
      if JSON_FORMATTING then
         Write_Eol;
         Indent;
      end if;
   end NL_And_Indent;

   -----------------------------
   -- Write_Boolean_Attribute --
   -----------------------------

   procedure Write_Boolean_Attribute (Name : String; Value : Boolean) is

   begin
      Write_Str ("""" & Name & """" & ": ");
      Write_Str (if Value then "true" else "false");
   end Write_Boolean_Attribute;

   -------------------------
   -- Write_Int_Attribute --
   -------------------------

   procedure Write_Int_Attribute (Name : String; Value : Int) is
   begin
      Write_Str ("""" & Name & """" & ": ");
      Write_Int (Value);
   end Write_Int_Attribute;

   -------------------------------
   -- Write_JSON_Escaped_String --
   -------------------------------

   procedure Write_JSON_Escaped_String (Str : String) is
   begin
      for C of Str loop
         if C = '"' or else C = '\' then
            Write_Char ('\');
         end if;

         Write_Char (C);
      end loop;
   end Write_JSON_Escaped_String;

   ----------------------------
   -- Write_String_Attribute --
   ----------------------------

   procedure Write_String_Attribute (Name : String; Value : String) is
   begin
      Write_Str ("""" & Name & """" & ": ");
      Write_Char ('"');
      Write_JSON_Escaped_String (Value);
      Write_Char ('"');
   end Write_String_Attribute;

end Diagnostics.JSON_Utils;
