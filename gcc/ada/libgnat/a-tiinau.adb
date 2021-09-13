------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . I N T E G E R _ A U X               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Generic_Aux; use Ada.Text_IO.Generic_Aux;

package body Ada.Text_IO.Integer_Aux is

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer := 1;
      Stop : Integer := 0;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Integer (File, Buf, Stop);
      end if;

      Item := Scan (Buf, Ptr'Access, Stop);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get;

   ----------
   -- Gets --
   ----------

   procedure Gets
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field;
      Base  : Number_Base)
   is
      Buf : String (1 .. Integer'Max (Field'Last, Width));
      Ptr : Natural := 0;

   begin
      if Base = 10 and then Width = 0 then
         Set_Image (Item, Buf, Ptr);
      elsif Base = 10 then
         Set_Image_Width (Item, Width, Buf, Ptr);
      else
         Set_Image_Based (Item, Base, Width, Buf, Ptr);
      end if;

      Put_Item (File, Buf (1 .. Ptr));
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To   : out String;
      Item : Num;
      Base : Number_Base)
   is
      Buf : String (1 .. Integer'Max (Field'Last, To'Length));
      Ptr : Natural := 0;

   begin
      if Base = 10 then
         Set_Image_Width (Item, To'Length, Buf, Ptr);
      else
         Set_Image_Based (Item, Base, To'Length, Buf, Ptr);
      end if;

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'First + Ptr - 1) := Buf (1 .. Ptr);
      end if;
   end Puts;

end Ada.Text_IO.Integer_Aux;
