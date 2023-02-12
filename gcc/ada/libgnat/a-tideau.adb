------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . D E C I M A L _ A U X               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package body Ada.Text_IO.Decimal_Aux is

   ---------
   -- Get --
   ---------

   function Get
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Int
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer;
      Stop : Integer := 0;
      Item : Int;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Real (File, Buf, Stop);
         Ptr := 1;
      end if;

      Item := Scan (Buf, Ptr'Access, Stop, Scale);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
      return Item;
   end Get;

   ----------
   -- Gets --
   ----------

   function Gets
     (From  : String;
      Last  : out Positive;
      Scale : Integer) return Int
   is
      Pos  : aliased Integer;
      Item : Int;

   begin
      String_Skip (From, Pos);
      Item := Scan (From, Pos'Access, From'Last, Scale);
      Last := Pos - 1;
      return Item;

   exception
      when Constraint_Error =>
         Last := Pos - 1;
         raise Data_Error;
   end Gets;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Int;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      Set_Image (Item, Buf, Ptr, Scale, Fore, Aft, Exp);
      Put_Item (File, Buf (1 .. Ptr));
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To    : out String;
      Item  : Int;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf  : String (1 .. Positive'Max (Field'Last, To'Length));
      Fore : Integer;
      Ptr  : Natural := 0;

   begin
      --  Compute Fore, allowing for the decimal dot and Aft digits

      Fore := To'Length - 1 - Field'Max (1, Aft);

      --  Allow for Exp and one more for E if exponent present

      if Exp /= 0 then
         Fore := Fore - 1 - Field'Max (2, Exp);
      end if;

      --  Make sure we have enough room

      if Fore < 1 + Boolean'Pos (Item < 0) then
         raise Layout_Error;
      end if;

      --  Do the conversion and check length of result

      Set_Image (Item, Buf, Ptr, Scale, Fore, Aft, Exp);

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To := Buf (1 .. Ptr);
      end if;
   end Puts;

end Ada.Text_IO.Decimal_Aux;
