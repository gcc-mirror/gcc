------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . B O U N D E D _ I O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2019, Free Software Foundation, Inc.         --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Ada.Text_IO.Bounded_IO is

   type String_Access is access all String;

   procedure Free (SA : in out String_Access);
   --  Perform an unchecked deallocation of a non-null string

   ----------
   -- Free --
   ----------

   procedure Free (SA : in out String_Access) is
      Null_String : constant String := "";

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (String, String_Access);

   begin
      --  Do not try to free statically allocated null string

      if SA.all /= Null_String then
         Deallocate (SA);
      end if;
   end Free;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return Bounded.Bounded_String is
   begin
      return Bounded.To_Bounded_String (Get_Line);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (File : File_Type) return Bounded.Bounded_String
   is
   begin
      return Bounded.To_Bounded_String (Get_Line (File));
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Item : out Bounded.Bounded_String)
   is
      Buffer : String (1 .. 1000);
      Last   : Natural;
      Str1   : String_Access;
      Str2   : String_Access;

   begin
      Get_Line (Buffer, Last);
      Str1 := new String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (Buffer, Last);
         Str2 := new String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      Item := Bounded.To_Bounded_String (Str1.all);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Item : out Bounded.Bounded_String)
   is
      Buffer : String (1 .. 1000);
      Last   : Natural;
      Str1   : String_Access;
      Str2   : String_Access;

   begin
      Get_Line (File, Buffer, Last);
      Str1 := new String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (File, Buffer, Last);
         Str2 := new String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      Item := Bounded.To_Bounded_String (Str1.all);
   end Get_Line;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Bounded.Bounded_String)
   is
   begin
      Put (Bounded.To_String (Item));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Bounded.Bounded_String)
   is
   begin
      Put (File, Bounded.To_String (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Item : Bounded.Bounded_String)
   is
   begin
      Put_Line (Bounded.To_String (Item));
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : Bounded.Bounded_String)
   is
   begin
      Put_Line (File, Bounded.To_String (Item));
   end Put_Line;

end Ada.Text_IO.Bounded_IO;
