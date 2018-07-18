------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . W I D E _ T E X T _ I O . W I D E _ B O U N D E D _ I O      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2018, Free Software Foundation, Inc.         --
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

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Unchecked_Deallocation;

package body Ada.Wide_Text_IO.Wide_Bounded_IO is

   type Wide_String_Access is access all Wide_String;

   procedure Free (WSA : in out Wide_String_Access);
   --  Perform an unchecked deallocation of a non-null string

   ----------
   -- Free --
   ----------

   procedure Free (WSA : in out Wide_String_Access) is
      Null_Wide_String : constant Wide_String := "";

      procedure Deallocate is
        new Ada.Unchecked_Deallocation (Wide_String, Wide_String_Access);

   begin
      --  Do not try to free statically allocated null string

      if WSA.all /= Null_Wide_String then
         Deallocate (WSA);
      end if;
   end Free;

   --------------
   -- Get_Line --
   --------------

   function Get_Line return Wide_Bounded.Bounded_Wide_String is
   begin
      return Wide_Bounded.To_Bounded_Wide_String (Get_Line);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (File : File_Type) return Wide_Bounded.Bounded_Wide_String
   is
   begin
      return Wide_Bounded.To_Bounded_Wide_String (Get_Line (File));
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Item : out Wide_Bounded.Bounded_Wide_String)
   is
      Buffer : Wide_String (1 .. 1000);
      Last   : Natural;
      Str1   : Wide_String_Access;
      Str2   : Wide_String_Access;

   begin
      Get_Line (Buffer, Last);
      Str1 := new Wide_String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (Buffer, Last);
         Str2 := new Wide_String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      Item := Wide_Bounded.To_Bounded_Wide_String (Str1.all);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Bounded.Bounded_Wide_String)
   is
      Buffer : Wide_String (1 .. 1000);
      Last   : Natural;
      Str1   : Wide_String_Access;
      Str2   : Wide_String_Access;

   begin
      Get_Line (File, Buffer, Last);
      Str1 := new Wide_String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (File, Buffer, Last);
         Str2 := new Wide_String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      Item := Wide_Bounded.To_Bounded_Wide_String (Str1.all);
   end Get_Line;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Wide_Bounded.Bounded_Wide_String)
   is
   begin
      Put (Wide_Bounded.To_Wide_String (Item));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Wide_Bounded.Bounded_Wide_String)
   is
   begin
      Put (File, Wide_Bounded.To_Wide_String (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Item : Wide_Bounded.Bounded_Wide_String)
   is
   begin
      Put_Line (Wide_Bounded.To_Wide_String (Item));
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File : File_Type;
      Item : Wide_Bounded.Bounded_Wide_String)
   is
   begin
      Put_Line (File, Wide_Bounded.To_Wide_String (Item));
   end Put_Line;

end Ada.Wide_Text_IO.Wide_Bounded_IO;
