------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        G N A T . C G I . D E B U G                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Strings.Unbounded;

package body GNAT.CGI.Debug is

   use Ada.Strings.Unbounded;

   --  Define the abstract type which act as a template for all debug IO modes.
   --  To create a new IO mode you must:
   --     1. create a new package spec
   --     2. create a new type derived from IO.Format
   --     3. implement all the abstract routines in IO

   package IO is

      type Format is abstract tagged null record;

      function Output (Mode : Format'Class) return String;

      function Variable
        (Mode  : Format;
         Name  : String;
         Value : String) return String is abstract;
      --  Returns variable Name and its associated value

      function New_Line (Mode : Format) return String is abstract;
      --  Returns a new line such as this concatenated between two strings
      --  will display the strings on two lines.

      function Title (Mode : Format; Str : String) return String is abstract;
      --  Returns Str as a Title. A title must be alone and centered on a
      --  line. Next output will be on the following line.

      function Header
        (Mode : Format;
         Str  : String) return String is abstract;
      --  Returns Str as an Header. An header must be alone on its line. Next
      --  output will be on the following line.

   end IO;

   ----------------------
   -- IO for HTML Mode --
   ----------------------

   package HTML_IO is

      --  See IO for comments about these routines

      type Format is new IO.Format with null record;

      function Variable
        (IO    : Format;
         Name  : String;
         Value : String) return String;

      function New_Line (IO : Format) return String;

      function Title (IO : Format; Str : String) return String;

      function Header (IO : Format; Str : String) return String;

   end HTML_IO;

   ----------------------------
   -- IO for Plain Text Mode --
   ----------------------------

   package Text_IO is

      --  See IO for comments about these routines

      type Format is new IO.Format with null record;

      function Variable
        (IO    : Format;
         Name  : String;
         Value : String) return String;

      function New_Line (IO : Format) return String;

      function Title (IO : Format; Str : String) return String;

      function Header (IO : Format; Str : String) return String;

   end Text_IO;

   --------------
   -- Debug_IO --
   --------------

   package body IO is

      ------------
      -- Output --
      ------------

      function Output (Mode : Format'Class) return String is
         Result : Unbounded_String;

      begin
         Result :=
           To_Unbounded_String
             (Title (Mode, "CGI complete runtime environment")
              & Header (Mode, "CGI parameters:")
              & New_Line (Mode));

         for K in 1 .. Argument_Count loop
            Result := Result
              & Variable (Mode, Key (K), Value (K))
              & New_Line (Mode);
         end loop;

         Result := Result
           & New_Line (Mode)
           & Header (Mode, "CGI environment variables (Metavariables):")
           & New_Line (Mode);

         for P in Metavariable_Name'Range loop
            if Metavariable_Exists (P) then
               Result := Result
                 & Variable (Mode,
                             Metavariable_Name'Image (P),
                             Metavariable (P))
                 & New_Line (Mode);
            end if;
         end loop;

         return To_String (Result);
      end Output;

   end IO;

   -------------
   -- HTML_IO --
   -------------

   package body HTML_IO is

      NL : constant String := (1 => ASCII.LF);

      function Bold (S : String) return String;
      --  Returns S as an HTML bold string

      function Italic (S : String) return String;
      --  Returns S as an HTML italic string

      ----------
      -- Bold --
      ----------

      function Bold (S : String) return String is
      begin
         return "<b>" & S & "</b>";
      end Bold;

      ------------
      -- Header --
      ------------

      function Header (IO : Format; Str : String) return String is
         pragma Unreferenced (IO);
      begin
         return "<h2>" & Str & "</h2>" & NL;
      end Header;

      ------------
      -- Italic --
      ------------

      function Italic (S : String) return String is
      begin
         return "<i>" & S & "</i>";
      end Italic;

      --------------
      -- New_Line --
      --------------

      function New_Line (IO : Format) return String is
         pragma Unreferenced (IO);
      begin
         return "<br>" & NL;
      end New_Line;

      -----------
      -- Title --
      -----------

      function Title (IO : Format; Str : String) return String is
         pragma Unreferenced (IO);
      begin
         return "<p align=center><font size=+2>" & Str & "</font></p>" & NL;
      end Title;

      --------------
      -- Variable --
      --------------

      function Variable
        (IO    : Format;
         Name  : String;
         Value : String) return String
      is
         pragma Unreferenced (IO);
      begin
         return Bold (Name) & " = " & Italic (Value);
      end Variable;

   end HTML_IO;

   -------------
   -- Text_IO --
   -------------

   package body Text_IO is

      ------------
      -- Header --
      ------------

      function Header (IO : Format; Str : String) return String is
      begin
         return "*** " & Str & New_Line (IO);
      end Header;

      --------------
      -- New_Line --
      --------------

      function New_Line (IO : Format) return String is
         pragma Unreferenced (IO);
      begin
         return String'(1 => ASCII.LF);
      end New_Line;

      -----------
      -- Title --
      -----------

      function Title (IO : Format; Str : String) return String is
         Spaces : constant Natural := (80 - Str'Length) / 2;
         Indent : constant String (1 .. Spaces) := (others => ' ');
      begin
         return Indent & Str & New_Line (IO);
      end Title;

      --------------
      -- Variable --
      --------------

      function Variable
        (IO    : Format;
         Name  : String;
         Value : String) return String
      is
         pragma Unreferenced (IO);
      begin
         return "   " & Name & " = " & Value;
      end Variable;

   end Text_IO;

   -----------------
   -- HTML_Output --
   -----------------

   function HTML_Output return String is
      HTML : HTML_IO.Format;
   begin
      return IO.Output (Mode => HTML);
   end HTML_Output;

   -----------------
   -- Text_Output --
   -----------------

   function Text_Output return String is
      Text : Text_IO.Format;
   begin
      return IO.Output (Mode => Text);
   end Text_Output;

end GNAT.CGI.Debug;
