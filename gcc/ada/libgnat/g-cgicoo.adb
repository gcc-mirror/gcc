------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T . C G I . C O O K I E                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2025, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

with GNAT.Table;

package body GNAT.CGI.Cookie is

   use Ada;

   Valid_Environment : Boolean := False;
   --  This boolean will be set to True if the initialization was fine

   Header_Sent : Boolean := False;
   --  Will be set to True when the header will be sent

   --  Cookie data that has been added

   type String_Access is access String;

   type Cookie_Data is record
      Key     : String_Access;
      Value   : String_Access;
      Comment : String_Access;
      Domain  : String_Access;
      Max_Age : Natural;
      Path    : String_Access;
      Secure  : Boolean := False;
   end record;

   type Key_Value is record
      Key, Value : String_Access;
   end record;

   package Cookie_Table is new Table (Cookie_Data, Positive, 1, 5, 50);
   --  This is the table to keep all cookies to be sent back to the server

   package Key_Value_Table is new Table (Key_Value, Positive, 1, 1, 50);
   --  This is the table to keep all cookies received from the server

   procedure Check_Environment;
   pragma Inline (Check_Environment);
   --  This procedure will raise Data_Error if Valid_Environment is False

   procedure Initialize;
   --  Initialize CGI package by reading the runtime environment. This
   --  procedure is called during elaboration. All exceptions raised during
   --  this procedure are deferred.

   -----------------------
   -- Check_Environment --
   -----------------------

   procedure Check_Environment is
   begin
      if not Valid_Environment then
         raise Data_Error;
      end if;
   end Check_Environment;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Key_Value_Table.Last;
   end Count;

   ------------
   -- Exists --
   ------------

   function Exists (Key : String) return Boolean is
   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop
         if Key_Value_Table.Table (K).Key.all = Key then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------------------
   -- For_Every_Cookie --
   ----------------------

   procedure For_Every_Cookie is
      Quit : Boolean;

   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop
         Quit := False;

         Action (Key_Value_Table.Table (K).Key.all,
                 Key_Value_Table.Table (K).Value.all,
                 K,
                 Quit);

         exit when Quit;
      end loop;
   end For_Every_Cookie;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      HTTP_COOKIE : constant String := Metavariable (CGI.HTTP_Cookie);

      procedure Set_Parameter_Table (Data : String);
      --  Parse Data and insert information in Key_Value_Table

      -------------------------
      -- Set_Parameter_Table --
      -------------------------

      procedure Set_Parameter_Table (Data : String) is

         procedure Add_Parameter (K : Positive; P : String);
         --  Add a single parameter into the table at index K. The parameter
         --  format is "key=value".

         Count : constant Positive :=
                   1 + Strings.Fixed.Count (Data, Strings.Maps.To_Set (";"));
         --  Count is the number of parameters in the string. Parameters are
         --  separated by ampersand character.

         Index : Positive := Data'First;
         Sep   : Natural;

         -------------------
         -- Add_Parameter --
         -------------------

         procedure Add_Parameter (K : Positive; P : String) is
            Equal : constant Natural := Strings.Fixed.Index (P, "=");
         begin
            if Equal = 0 then
               raise Data_Error;
            else
               Key_Value_Table.Table (K) :=
                 Key_Value'(new String'(Decode (P (P'First .. Equal - 1))),
                            new String'(Decode (P (Equal + 1 .. P'Last))));
            end if;
         end Add_Parameter;

      --  Start of processing for Set_Parameter_Table

      begin
         Key_Value_Table.Set_Last (Count);

         for K in 1 .. Count - 1 loop
            Sep := Strings.Fixed.Index (Data (Index .. Data'Last), ";");

            Add_Parameter (K, Data (Index .. Sep - 1));

            Index := Sep + 2;
         end loop;

         --  Add last parameter

         Add_Parameter (Count, Data (Index .. Data'Last));
      end Set_Parameter_Table;

   --  Start of processing for Initialize

   begin
      if HTTP_COOKIE /= "" then
         Set_Parameter_Table (HTTP_COOKIE);
      end if;

      Valid_Environment := True;

   exception
      when others =>
         Valid_Environment := False;
   end Initialize;

   ---------
   -- Key --
   ---------

   function Key (Position : Positive) return String is
   begin
      Check_Environment;

      if Position <= Key_Value_Table.Last then
         return Key_Value_Table.Table (Position).Key.all;
      else
         raise Cookie_Not_Found;
      end if;
   end Key;

   --------
   -- Ok --
   --------

   function Ok return Boolean is
   begin
      return Valid_Environment;
   end Ok;

   ----------------
   -- Put_Header --
   ----------------

   procedure Put_Header
     (Header : String  := Default_Header;
      Force  : Boolean := False)
   is
      procedure Output_Cookies;
      --  Iterate through the list of cookies to be sent to the server
      --  and output them.

      --------------------
      -- Output_Cookies --
      --------------------

      procedure Output_Cookies is

         procedure Output_One_Cookie
           (Key     : String;
            Value   : String;
            Comment : String;
            Domain  : String;
            Max_Age : Natural;
            Path    : String;
            Secure  : Boolean);
         --  Output one cookie in the CGI header

         -----------------------
         -- Output_One_Cookie --
         -----------------------

         procedure Output_One_Cookie
           (Key     : String;
            Value   : String;
            Comment : String;
            Domain  : String;
            Max_Age : Natural;
            Path    : String;
            Secure  : Boolean)
         is
         begin
            Text_IO.Put ("Set-Cookie: ");
            Text_IO.Put (Key & '=' & Value);

            if Comment /= "" then
               Text_IO.Put ("; Comment=" & Comment);
            end if;

            if Domain /= "" then
               Text_IO.Put ("; Domain=" & Domain);
            end if;

            if Max_Age /= Natural'Last then
               Text_IO.Put ("; Max-Age=");
               Integer_Text_IO.Put (Max_Age, Width => 0);
            end if;

            if Path /= "" then
               Text_IO.Put ("; Path=" & Path);
            end if;

            if Secure then
               Text_IO.Put ("; Secure");
            end if;

            Text_IO.New_Line;
         end Output_One_Cookie;

      --  Start of processing for Output_Cookies

      begin
         for C in 1 .. Cookie_Table.Last loop
            Output_One_Cookie (Cookie_Table.Table (C).Key.all,
                               Cookie_Table.Table (C).Value.all,
                               Cookie_Table.Table (C).Comment.all,
                               Cookie_Table.Table (C).Domain.all,
                               Cookie_Table.Table (C).Max_Age,
                               Cookie_Table.Table (C).Path.all,
                               Cookie_Table.Table (C).Secure);
         end loop;
      end Output_Cookies;

   --  Start of processing for Put_Header

   begin
      if Header_Sent = False or else Force then
         Check_Environment;
         Text_IO.Put_Line (Header);
         Output_Cookies;
         Text_IO.New_Line;
         Header_Sent := True;
      end if;
   end Put_Header;

   ---------
   -- Set --
   ---------

   procedure Set
     (Key     : String;
      Value   : String;
      Comment : String   := "";
      Domain  : String   := "";
      Max_Age : Natural  := Natural'Last;
      Path    : String   := "/";
      Secure  : Boolean  := False)
   is
   begin
      Cookie_Table.Increment_Last;

      Cookie_Table.Table (Cookie_Table.Last) :=
        Cookie_Data'(new String'(Key),
                     new String'(Value),
                     new String'(Comment),
                     new String'(Domain),
                     Max_Age,
                     new String'(Path),
                     Secure);
   end Set;

   -----------
   -- Value --
   -----------

   function Value
     (Key      : String;
      Required : Boolean := False) return String
   is
   begin
      Check_Environment;

      for K in 1 .. Key_Value_Table.Last loop
         if Key_Value_Table.Table (K).Key.all = Key then
            return Key_Value_Table.Table (K).Value.all;
         end if;
      end loop;

      if Required then
         raise Cookie_Not_Found;
      else
         return "";
      end if;
   end Value;

   function Value (Position : Positive) return String is
   begin
      Check_Environment;

      if Position <= Key_Value_Table.Last then
         return Key_Value_Table.Table (Position).Value.all;
      else
         raise Cookie_Not_Found;
      end if;
   end Value;

--  Elaboration code for package

begin
   --  Initialize unit by reading the HTTP_COOKIE metavariable and fill
   --  Key_Value_Table structure.

   Initialize;
end GNAT.CGI.Cookie;
