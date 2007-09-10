------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

with Hostparm;
with Osint.C;  use Osint.C;

package body Lib.Util is

   Max_Line   : constant Natural := 2 * Hostparm.Max_Name_Length + 64;
   Max_Buffer : constant Natural := 1000 * Max_Line;

   Info_Buffer : String (1 .. Max_Buffer);
   --  Info_Buffer used to prepare lines of library output

   Info_Buffer_Len : Natural := 0;
   --  Number of characters stored in Info_Buffer

   Info_Buffer_Col : Natural := 1;
   --  Column number of next character to be written.
   --  Can be different from Info_Buffer_Len + 1
   --  because of tab characters written by Write_Info_Tab.

   ---------------------
   -- Write_Info_Char --
   ---------------------

   procedure Write_Info_Char (C : Character) is
   begin
      Info_Buffer_Len := Info_Buffer_Len + 1;
      Info_Buffer (Info_Buffer_Len) := C;
      Info_Buffer_Col := Info_Buffer_Col + 1;
   end Write_Info_Char;

   --------------------------
   -- Write_Info_Char_Code --
   --------------------------

   procedure Write_Info_Char_Code (Code : Char_Code) is

      procedure Write_Info_Hex_Byte (J : Natural);
      --  Write single hex digit

      procedure Write_Info_Hex_Byte (J : Natural) is
         Hexd : constant String := "0123456789abcdef";

      begin
         Write_Info_Char (Hexd (J / 16 + 1));
         Write_Info_Char (Hexd (J mod 16 + 1));
      end Write_Info_Hex_Byte;

   --  Start of processing for Write_Info_Char_Code

   begin
      --  00 .. 7F

      if Code <= 16#7F# then
         Write_Info_Char (Character'Val (Code));

      --  80 .. FF

      elsif Code <= 16#FF# then
         Write_Info_Char ('U');
         Write_Info_Hex_Byte (Natural (Code));

      --  0100 .. FFFF

      else
         Write_Info_Char ('W');
         Write_Info_Hex_Byte (Natural (Code / 256));
         Write_Info_Hex_Byte (Natural (Code mod 256));
      end if;
   end Write_Info_Char_Code;

   --------------------
   -- Write_Info_Col --
   --------------------

   function Write_Info_Col return Positive is
   begin
      return Info_Buffer_Col;
   end Write_Info_Col;

   --------------------
   -- Write_Info_EOL --
   --------------------

   procedure Write_Info_EOL is
   begin
      if Hostparm.OpenVMS
        or else Info_Buffer_Len + Max_Line + 1 > Max_Buffer
      then
         Write_Info_Terminate;
      else
         --  Delete any trailing blanks

         while Info_Buffer_Len > 0
           and then Info_Buffer (Info_Buffer_Len) = ' '
         loop
            Info_Buffer_Len := Info_Buffer_Len - 1;
         end loop;

         Info_Buffer_Len := Info_Buffer_Len + 1;
         Info_Buffer (Info_Buffer_Len) := ASCII.LF;
         Info_Buffer_Col := 1;
      end if;
   end Write_Info_EOL;

   -------------------------
   -- Write_Info_Initiate --
   -------------------------

   procedure Write_Info_Initiate (Key : Character) renames Write_Info_Char;

   ---------------------
   -- Write_Info_Name --
   ---------------------

   procedure Write_Info_Name (Name : Name_Id) is
   begin
      Get_Name_String (Name);
      Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Name_Len) :=
        Name_Buffer (1 .. Name_Len);
      Info_Buffer_Len := Info_Buffer_Len + Name_Len;
      Info_Buffer_Col := Info_Buffer_Col + Name_Len;
   end Write_Info_Name;

   procedure Write_Info_Name (Name : File_Name_Type) is
   begin
      Write_Info_Name (Name_Id (Name));
   end Write_Info_Name;

   procedure Write_Info_Name (Name : Unit_Name_Type) is
   begin
      Write_Info_Name (Name_Id (Name));
   end Write_Info_Name;

   --------------------
   -- Write_Info_Nat --
   --------------------

   procedure Write_Info_Nat (N : Nat) is
   begin
      if N > 9 then
         Write_Info_Nat (N / 10);
      end if;

      Write_Info_Char (Character'Val (N mod 10 + Character'Pos ('0')));
   end Write_Info_Nat;

   --------------------
   -- Write_Info_Str --
   --------------------

   procedure Write_Info_Str (Val : String) is
   begin
      Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Val'Length)
                                                                  := Val;
      Info_Buffer_Len := Info_Buffer_Len + Val'Length;
      Info_Buffer_Col := Info_Buffer_Col + Val'Length;
   end Write_Info_Str;

   --------------------
   -- Write_Info_Tab --
   --------------------

   procedure Write_Info_Tab (Col : Positive) is
      Next_Tab : Positive;

   begin
      if Col <= Info_Buffer_Col then
         Write_Info_Str ("  ");
      else
         loop
            Next_Tab := 8 * ((Info_Buffer_Col - 1) / 8) + 8 + 1;
            exit when Col < Next_Tab;
            Write_Info_Char (ASCII.HT);
            Info_Buffer_Col := Next_Tab;
         end loop;

         while Info_Buffer_Col < Col loop
            Write_Info_Char (' ');
         end loop;
      end if;
   end Write_Info_Tab;

   --------------------------
   -- Write_Info_Terminate --
   --------------------------

   procedure Write_Info_Terminate is
   begin
      --  Delete any trailing blanks

      while Info_Buffer_Len > 0
        and then Info_Buffer (Info_Buffer_Len) = ' '
      loop
         Info_Buffer_Len := Info_Buffer_Len - 1;
      end loop;

      --  Write_Library_Info adds the EOL

      Write_Library_Info (Info_Buffer (1 .. Info_Buffer_Len));

      Info_Buffer_Len := 0;
      Info_Buffer_Col := 1;

   end Write_Info_Terminate;

end Lib.Util;
