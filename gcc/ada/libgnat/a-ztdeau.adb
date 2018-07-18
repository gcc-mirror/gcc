------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ T E X T _ I O . D E C I M A L _ A U X     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Ada.Wide_Wide_Text_IO.Generic_Aux; use Ada.Wide_Wide_Text_IO.Generic_Aux;
with Ada.Wide_Wide_Text_IO.Float_Aux;   use Ada.Wide_Wide_Text_IO.Float_Aux;

with System.Img_Dec; use System.Img_Dec;
with System.Img_LLD; use System.Img_LLD;
with System.Val_Dec; use System.Val_Dec;
with System.Val_LLD; use System.Val_LLD;

package body Ada.Wide_Wide_Text_IO.Decimal_Aux is

   -------------
   -- Get_Dec --
   -------------

   function Get_Dec
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Integer
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer;
      Stop : Integer := 0;
      Item : Integer;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Real (File, Buf, Stop);
         Ptr := 1;
      end if;

      Item := Scan_Decimal (Buf, Ptr'Access, Stop, Scale);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
      return Item;
   end Get_Dec;

   -------------
   -- Get_LLD --
   -------------

   function Get_LLD
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Long_Long_Integer
   is
      Buf  : String (1 .. Field'Last);
      Ptr  : aliased Integer;
      Stop : Integer := 0;
      Item : Long_Long_Integer;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Real (File, Buf, Stop);
         Ptr := 1;
      end if;

      Item := Scan_Long_Long_Decimal (Buf, Ptr'Access, Stop, Scale);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
      return Item;
   end Get_LLD;

   --------------
   -- Gets_Dec --
   --------------

   function Gets_Dec
     (From  : String;
      Last  : not null access Positive;
      Scale : Integer) return Integer
   is
      Pos  : aliased Integer;
      Item : Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Decimal (From, Pos'Access, From'Last, Scale);
      Last.all := Pos - 1;
      return Item;

   exception
      when Constraint_Error =>
         Last.all := Pos - 1;
         raise Data_Error;

   end Gets_Dec;

   --------------
   -- Gets_LLD --
   --------------

   function Gets_LLD
     (From  : String;
      Last  : not null access Positive;
      Scale : Integer) return Long_Long_Integer
   is
      Pos  : aliased Integer;
      Item : Long_Long_Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Long_Long_Decimal (From, Pos'Access, From'Last, Scale);
      Last.all := Pos - 1;
      return Item;

   exception
      when Constraint_Error =>
         Last.all := Pos - 1;
         raise Data_Error;

   end Gets_LLD;

   -------------
   -- Put_Dec --
   -------------

   procedure Put_Dec
     (File  : File_Type;
      Item  : Integer;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      Set_Image_Decimal (Item, Buf, Ptr, Scale, Fore, Aft, Exp);
      Put_Item (File, Buf (1 .. Ptr));
   end Put_Dec;

   -------------
   -- Put_LLD --
   -------------

   procedure Put_LLD
     (File  : File_Type;
      Item  : Long_Long_Integer;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      Set_Image_Long_Long_Decimal (Item, Buf, Ptr, Scale, Fore, Aft, Exp);
      Put_Item (File, Buf (1 .. Ptr));
   end Put_LLD;

   --------------
   -- Puts_Dec --
   --------------

   procedure Puts_Dec
     (To    : out String;
      Item  : Integer;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf  : String (1 .. Field'Last);
      Fore : Integer;
      Ptr  : Natural := 0;

   begin
      --  Compute Fore, allowing for Aft digits and the decimal dot

      Fore := To'Length - Field'Max (1, Aft) - 1;

      --  Allow for Exp and two more for E+ or E- if exponent present

      if Exp /= 0 then
         Fore := Fore - 2 - Exp;
      end if;

      --  Make sure we have enough room

      if Fore < 1 then
         raise Layout_Error;
      end if;

      --  Do the conversion and check length of result

      Set_Image_Decimal (Item, Buf, Ptr, Scale, Fore, Aft, Exp);

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To := Buf (1 .. Ptr);
      end if;
   end Puts_Dec;

   --------------
   -- Puts_LLD --
   --------------

   procedure Puts_LLD
     (To    : out String;
      Item  : Long_Long_Integer;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer)
   is
      Buf  : String (1 .. Field'Last);
      Fore : Integer;
      Ptr  : Natural := 0;

   begin
      Fore :=
        (if Exp = 0 then To'Length - 1 - Aft else To'Length - 2 - Aft - Exp);

      if Fore < 1 then
         raise Layout_Error;
      end if;

      Set_Image_Long_Long_Decimal (Item, Buf, Ptr, Scale, Fore, Aft, Exp);

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To := Buf (1 .. Ptr);
      end if;
   end Puts_LLD;

end Ada.Wide_Wide_Text_IO.Decimal_Aux;
