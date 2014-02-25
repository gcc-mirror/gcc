------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T R I N G T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Alloc;
with Namet;  use Namet;
with Output; use Output;
with Table;

package body Stringt is

   --  The following table stores the sequence of character codes for the
   --  stored string constants. The entries are referenced from the
   --  separate Strings table.

   package String_Chars is new Table.Table (
     Table_Component_Type => Char_Code,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.String_Chars_Initial,
     Table_Increment      => Alloc.String_Chars_Increment,
     Table_Name           => "String_Chars");

   --  The String_Id values reference entries in the Strings table, which
   --  contains String_Entry records that record the length of each stored
   --  string and its starting location in the String_Chars table.

   type String_Entry is record
      String_Index : Int;
      Length       : Nat;
   end record;

   package Strings is new Table.Table (
     Table_Component_Type => String_Entry,
     Table_Index_Type     => String_Id'Base,
     Table_Low_Bound      => First_String_Id,
     Table_Initial        => Alloc.Strings_Initial,
     Table_Increment      => Alloc.Strings_Increment,
     Table_Name           => "Strings");

   --  Note: it is possible that two entries in the Strings table can share
   --  string data in the String_Chars table, and in particular this happens
   --  when Start_String is called with a parameter that is the last string
   --  currently allocated in the table.

   Strings_Last      : String_Id := First_String_Id;
   String_Chars_Last : Int := 0;
   --  Strings_Last and String_Chars_Last are used by procedure Mark and
   --  Release to get a snapshot of the tables and to restore them to their
   --  previous situation.

   -------------------------------
   -- Add_String_To_Name_Buffer --
   -------------------------------

   procedure Add_String_To_Name_Buffer (S : String_Id) is
      Len : constant Natural := Natural (String_Length (S));

   begin
      for J in 1 .. Len loop
         Name_Buffer (Name_Len + J) :=
           Get_Character (Get_String_Char (S, Int (J)));
      end loop;

      Name_Len := Name_Len + Len;
   end Add_String_To_Name_Buffer;

   ----------------
   -- End_String --
   ----------------

   function End_String return String_Id is
   begin
      return Strings.Last;
   end End_String;

   ---------------------
   -- Get_String_Char --
   ---------------------

   function Get_String_Char (Id : String_Id; Index : Int) return Char_Code is
   begin
      pragma Assert (Id in First_String_Id .. Strings.Last
                       and then Index in 1 .. Strings.Table (Id).Length);

      return String_Chars.Table (Strings.Table (Id).String_Index + Index - 1);
   end Get_String_Char;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      String_Chars.Init;
      Strings.Init;

      --  Set up the null string

      Start_String;
      Null_String_Id := End_String;
   end Initialize;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      String_Chars.Locked := True;
      Strings.Locked := True;
      String_Chars.Release;
      Strings.Release;
   end Lock;

   ----------
   -- Mark --
   ----------

   procedure Mark is
   begin
      Strings_Last := Strings.Last;
      String_Chars_Last := String_Chars.Last;
   end Mark;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Strings.Set_Last (Strings_Last);
      String_Chars.Set_Last (String_Chars_Last);
   end Release;

   ------------------
   -- Start_String --
   ------------------

   --  Version to start completely new string

   procedure Start_String is
   begin
      Strings.Append ((String_Index => String_Chars.Last + 1, Length => 0));
   end Start_String;

   --  Version to start from initially stored string

   procedure Start_String (S : String_Id) is
   begin
      Strings.Increment_Last;

      --  Case of initial string value is at the end of the string characters
      --  table, so it does not need copying, instead it can be shared.

      if Strings.Table (S).String_Index + Strings.Table (S).Length =
                                                    String_Chars.Last + 1
      then
         Strings.Table (Strings.Last).String_Index :=
           Strings.Table (S).String_Index;

      --  Case of initial string value must be copied to new string

      else
         Strings.Table (Strings.Last).String_Index :=
           String_Chars.Last + 1;

         for J in 1 .. Strings.Table (S).Length loop
            String_Chars.Append
              (String_Chars.Table (Strings.Table (S).String_Index + (J - 1)));
         end loop;
      end if;

      --  In either case the result string length is copied from the argument

      Strings.Table (Strings.Last).Length := Strings.Table (S).Length;
   end Start_String;

   -----------------------
   -- Store_String_Char --
   -----------------------

   procedure Store_String_Char (C : Char_Code) is
   begin
      String_Chars.Append (C);
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length + 1;
   end Store_String_Char;

   procedure Store_String_Char (C : Character) is
   begin
      Store_String_Char (Get_Char_Code (C));
   end Store_String_Char;

   ------------------------
   -- Store_String_Chars --
   ------------------------

   procedure Store_String_Chars (S : String) is
   begin
      for J in S'First .. S'Last loop
         Store_String_Char (Get_Char_Code (S (J)));
      end loop;
   end Store_String_Chars;

   procedure Store_String_Chars (S : String_Id) is

      --  We are essentially doing this:

      --   for J in 1 .. String_Length (S) loop
      --      Store_String_Char (Get_String_Char (S, J));
      --   end loop;

      --  but when the string is long it's more efficient to grow the
      --  String_Chars table all at once.

      S_First  : constant Int := Strings.Table (S).String_Index;
      S_Len    : constant Int := String_Length (S);
      Old_Last : constant Int := String_Chars.Last;
      New_Last : constant Int := Old_Last + S_Len;

   begin
      String_Chars.Set_Last (New_Last);
      String_Chars.Table (Old_Last + 1 .. New_Last) :=
        String_Chars.Table (S_First .. S_First + S_Len - 1);
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length + S_Len;
   end Store_String_Chars;

   ----------------------
   -- Store_String_Int --
   ----------------------

   procedure Store_String_Int (N : Int) is
   begin
      if N < 0 then
         Store_String_Char ('-');
         Store_String_Int (-N);

      else
         if N > 9 then
            Store_String_Int (N / 10);
         end if;

         Store_String_Char (Character'Val (Character'Pos ('0') + N mod 10));
      end if;
   end Store_String_Int;

   --------------------------
   -- String_Chars_Address --
   --------------------------

   function String_Chars_Address return System.Address is
   begin
      return String_Chars.Table (0)'Address;
   end String_Chars_Address;

   ------------------
   -- String_Equal --
   ------------------

   function String_Equal (L, R : String_Id) return Boolean is
      Len : constant Nat := Strings.Table (L).Length;

   begin
      if Len /= Strings.Table (R).Length then
         return False;
      else
         for J in 1 .. Len loop
            if Get_String_Char (L, J) /= Get_String_Char (R, J) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end String_Equal;

   -----------------------------
   -- String_From_Name_Buffer --
   -----------------------------

   function String_From_Name_Buffer return String_Id is
   begin
      Start_String;

      for J in 1 .. Name_Len loop
         Store_String_Char (Get_Char_Code (Name_Buffer (J)));
      end loop;

      return End_String;
   end String_From_Name_Buffer;

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Id : String_Id) return Nat is
   begin
      return Strings.Table (Id).Length;
   end String_Length;

   ---------------------------
   -- String_To_Name_Buffer --
   ---------------------------

   procedure String_To_Name_Buffer (S : String_Id) is
   begin
      Name_Len := Natural (String_Length (S));

      for J in 1 .. Name_Len loop
         Name_Buffer (J) :=
           Get_Character (Get_String_Char (S, Int (J)));
      end loop;
   end String_To_Name_Buffer;

   ---------------------
   -- Strings_Address --
   ---------------------

   function Strings_Address return System.Address is
   begin
      return Strings.Table (First_String_Id)'Address;
   end Strings_Address;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      String_Chars.Tree_Read;
      Strings.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      String_Chars.Tree_Write;
      Strings.Tree_Write;
   end Tree_Write;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      String_Chars.Locked := False;
      Strings.Locked := False;
   end Unlock;

   -------------------------
   -- Unstore_String_Char --
   -------------------------

   procedure Unstore_String_Char is
   begin
      String_Chars.Decrement_Last;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length - 1;
   end Unstore_String_Char;

   ---------------------
   -- Write_Char_Code --
   ---------------------

   procedure Write_Char_Code (Code : Char_Code) is

      procedure Write_Hex_Byte (J : Char_Code);
      --  Write single hex byte (value in range 0 .. 255) as two digits

      --------------------
      -- Write_Hex_Byte --
      --------------------

      procedure Write_Hex_Byte (J : Char_Code) is
         Hexd : constant array (Char_Code range 0 .. 15) of Character :=
                  "0123456789abcdef";
      begin
         Write_Char (Hexd (J / 16));
         Write_Char (Hexd (J mod 16));
      end Write_Hex_Byte;

   --  Start of processing for Write_Char_Code

   begin
      if Code in 16#20# .. 16#7E# then
         Write_Char (Character'Val (Code));

      else
         Write_Char ('[');
         Write_Char ('"');

         if Code > 16#FF_FFFF# then
            Write_Hex_Byte (Code / 2 ** 24);
         end if;

         if Code > 16#FFFF# then
            Write_Hex_Byte ((Code / 2 ** 16) mod 256);
         end if;

         if Code > 16#FF# then
            Write_Hex_Byte ((Code / 256) mod 256);
         end if;

         Write_Hex_Byte (Code mod 256);
         Write_Char ('"');
         Write_Char (']');
      end if;
   end Write_Char_Code;

   ------------------------------
   -- Write_String_Table_Entry --
   ------------------------------

   procedure Write_String_Table_Entry (Id : String_Id) is
      C : Char_Code;

   begin
      if Id = No_String then
         Write_Str ("no string");

      else
         Write_Char ('"');

         for J in 1 .. String_Length (Id) loop
            C := Get_String_Char (Id, J);

            if C = Character'Pos ('"') then
               Write_Str ("""""");
            else
               Write_Char_Code (C);
            end if;

            --  If string is very long, quit

            if J >= 1000 then  --  arbitrary limit
               Write_Str ("""...etc (length = ");
               Write_Int (String_Length (Id));
               Write_Str (")");
               return;
            end if;
         end loop;

         Write_Char ('"');
      end if;
   end Write_String_Table_Entry;

end Stringt;
