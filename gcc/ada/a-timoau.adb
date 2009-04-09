------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . M O D U L A R  _ A U X              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with System.Img_BIU; use System.Img_BIU;
with System.Img_Uns; use System.Img_Uns;
with System.Img_LLB; use System.Img_LLB;
with System.Img_LLU; use System.Img_LLU;
with System.Img_LLW; use System.Img_LLW;
with System.Img_WIU; use System.Img_WIU;
with System.Val_Uns; use System.Val_Uns;
with System.Val_LLU; use System.Val_LLU;

package body Ada.Text_IO.Modular_Aux is

   use System.Unsigned_Types;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Load_Modular
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural);
   --  This is an auxiliary routine that is used to load an possibly signed
   --  modular literal value from the input file into Buf, starting at Ptr + 1.
   --  Ptr is left set to the last character stored.

   -------------
   -- Get_LLU --
   -------------

   procedure Get_LLU
     (File  : File_Type;
      Item  : out Long_Long_Unsigned;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Stop : Integer := 0;
      Ptr  : aliased Integer := 1;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Modular (File, Buf, Stop);
      end if;

      Item := Scan_Long_Long_Unsigned (Buf, Ptr'Access, Stop);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get_LLU;

   -------------
   -- Get_Uns --
   -------------

   procedure Get_Uns
     (File  : File_Type;
      Item  : out Unsigned;
      Width : Field)
   is
      Buf  : String (1 .. Field'Last);
      Stop : Integer := 0;
      Ptr  : aliased Integer := 1;

   begin
      if Width /= 0 then
         Load_Width (File, Width, Buf, Stop);
         String_Skip (Buf, Ptr);
      else
         Load_Modular (File, Buf, Stop);
      end if;

      Item := Scan_Unsigned (Buf, Ptr'Access, Stop);
      Check_End_Of_Field (Buf, Stop, Ptr, Width);
   end Get_Uns;

   --------------
   -- Gets_LLU --
   --------------

   procedure Gets_LLU
     (From : String;
      Item : out Long_Long_Unsigned;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Long_Long_Unsigned (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets_LLU;

   --------------
   -- Gets_Uns --
   --------------

   procedure Gets_Uns
     (From : String;
      Item : out Unsigned;
      Last : out Positive)
   is
      Pos : aliased Integer;

   begin
      String_Skip (From, Pos);
      Item := Scan_Unsigned (From, Pos'Access, From'Last);
      Last := Pos - 1;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Gets_Uns;

   ------------------
   -- Load_Modular --
   ------------------

   procedure Load_Modular
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural)
   is
      Hash_Loc : Natural;
      Loaded   : Boolean;

   begin
      Load_Skip (File);

      --  Note: it is a bit strange to allow a minus sign here, but it seems
      --  consistent with the general behavior expected by the ACVC tests
      --  which is to scan past junk and then signal data error, see ACVC
      --  test CE3704F, case (6), which is for signed integer exponents,
      --  which seems a similar case.

      Load (File, Buf, Ptr, '+', '-');
      Load_Digits (File, Buf, Ptr, Loaded);

      if Loaded then
         Load (File, Buf, Ptr, '#', ':', Loaded);

         if Loaded then
            Hash_Loc := Ptr;
            Load_Extended_Digits (File, Buf, Ptr);
            Load (File, Buf, Ptr, Buf (Hash_Loc));
         end if;

         Load (File, Buf, Ptr, 'E', 'e', Loaded);

         if Loaded then

            --  Note: it is strange to allow a minus sign, since the syntax
            --  does not, but that is what ACVC test CE3704F, case (6) wants
            --  for the signed case, and there seems no good reason to treat
            --  exponents differently for the signed and unsigned cases.

            Load (File, Buf, Ptr, '+', '-');
            Load_Digits (File, Buf, Ptr);
         end if;
      end if;
   end Load_Modular;

   -------------
   -- Put_LLU --
   -------------

   procedure Put_LLU
     (File  : File_Type;
      Item  : Long_Long_Unsigned;
      Width : Field;
      Base  : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 and then Width = 0 then
         Set_Image_Long_Long_Unsigned (Item, Buf, Ptr);
      elsif Base = 10 then
         Set_Image_Width_Long_Long_Unsigned (Item, Width, Buf, Ptr);
      else
         Set_Image_Based_Long_Long_Unsigned (Item, Base, Width, Buf, Ptr);
      end if;

      Put_Item (File, Buf (1 .. Ptr));
   end Put_LLU;

   -------------
   -- Put_Uns --
   -------------

   procedure Put_Uns
     (File  : File_Type;
      Item  : Unsigned;
      Width : Field;
      Base  : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 and then Width = 0 then
         Set_Image_Unsigned (Item, Buf, Ptr);
      elsif Base = 10 then
         Set_Image_Width_Unsigned (Item, Width, Buf, Ptr);
      else
         Set_Image_Based_Unsigned (Item, Base, Width, Buf, Ptr);
      end if;

      Put_Item (File, Buf (1 .. Ptr));
   end Put_Uns;

   --------------
   -- Puts_LLU --
   --------------

   procedure Puts_LLU
     (To   : out String;
      Item : Long_Long_Unsigned;
      Base : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 then
         Set_Image_Width_Long_Long_Unsigned (Item, To'Length, Buf, Ptr);
      else
         Set_Image_Based_Long_Long_Unsigned (Item, Base, To'Length, Buf, Ptr);
      end if;

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'First + Ptr - 1) := Buf (1 .. Ptr);
      end if;
   end Puts_LLU;

   --------------
   -- Puts_Uns --
   --------------

   procedure Puts_Uns
     (To   : out String;
      Item : Unsigned;
      Base : Number_Base)
   is
      Buf : String (1 .. Field'Last);
      Ptr : Natural := 0;

   begin
      if Base = 10 then
         Set_Image_Width_Unsigned (Item, To'Length, Buf, Ptr);
      else
         Set_Image_Based_Unsigned (Item, Base, To'Length, Buf, Ptr);
      end if;

      if Ptr > To'Length then
         raise Layout_Error;
      else
         To (To'First .. To'First + Ptr - 1) := Buf (1 .. Ptr);
      end if;
   end Puts_Uns;

end Ada.Text_IO.Modular_Aux;
