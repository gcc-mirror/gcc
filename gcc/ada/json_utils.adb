------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           J S O N _ U T I L S                            --
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

with Namet; use Namet;
with Osint;
with Output; use Output;
with System.OS_Lib;

package body JSON_Utils is

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

   -----------------
   -- To_File_Uri --
   -----------------

   function To_File_Uri (Path : String) return String is

      function Normalize_Uri (Path : String) return String;
      --  Construct a normalized URI from the path name by replacing reserved
      --  URI characters that can appear in paths with their escape character
      --  combinations.
      --
      --  According to the URI standard reserved charcthers within the paths
      --  should be percent encoded:
      --
      --  https://www.rfc-editor.org/info/rfc3986
      --
      --  Reserved charcters are defined as:
      --
      --  reserved = gen-delims / sub-delims
      --  gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@"
      --  sub-delims = "!" / "$" / "&" / "’" / "(" / ")"
      --  / "*" / "+" / "," / ";" / "="

      -------------------
      -- Normalize_Uri --
      -------------------

      function Normalize_Uri (Path : String) return String is
         Buf : Bounded_String;
      begin
         for C of Path loop
            case C is
               when '\' =>

                  --  Use forward slashes instead of backward slashes as
                  --  separators on Windows and on Linux simply encode the
                  --  symbol if part of a directory name.

                  if Osint.On_Windows then
                     Append (Buf, '/');
                  else
                     Append (Buf, "%5C");
                  end if;

               when ' ' =>
                  Append (Buf, "%20");

               when '!' =>
                  Append (Buf, "%21");

               when '#' =>
                  Append (Buf, "%23");

               when '$' =>
                  Append (Buf, "%24");

               when '&' =>
                  Append (Buf, "%26");

               when ''' =>
                  Append (Buf, "%27");

               when '(' =>
                  Append (Buf, "%28");

               when ')' =>
                  Append (Buf, "%29");

               when '*' =>
                  Append (Buf, "%2A");

               when '+' =>
                  Append (Buf, "%2A");

               when ',' =>
                  Append (Buf, "%2A");

               when '/' =>
                  --  Forward slash is a valid file separator on both Unix and
                  --  Windows based machines and should be treated as such
                  --  within a path.
                  Append (Buf, '/');

               when ':' =>
                  Append (Buf, "%3A");

               when ';' =>
                  Append (Buf, "%3B");

               when '=' =>
                  Append (Buf, "%3D");

               when '?' =>
                  Append (Buf, "%3F");

               when '@' =>
                  Append (Buf, "%40");

               when '[' =>
                  Append (Buf, "%5B");

               when ']' =>
                  Append (Buf, "%5D");

               when others =>
                  Append (Buf, C);
            end case;
         end loop;

         return To_String (Buf);
      end Normalize_Uri;

      Norm_Uri : constant String := Normalize_Uri (Path);

   --  Start of processing for To_File_Uri

   begin
      if System.OS_Lib.Is_Absolute_Path (Path) then
         --  URI-s using the file scheme should start with the following
         --  prefix:
         --
         --  "file:///"

         if Osint.On_Windows then
            return "file:///" & Norm_Uri;
         else
            --  Full paths on linux based systems already start with '/'

            return "file://" & Norm_Uri;
         end if;
      else
         return Norm_Uri;
      end if;
   end To_File_Uri;

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

end JSON_Utils;
