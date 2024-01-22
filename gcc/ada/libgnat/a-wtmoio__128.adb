------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . M O D U L A R _ I O          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2024, Free Software Foundation, Inc.        --
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

with Ada.Wide_Text_IO.Integer_Aux;
with System.Img_BIU;  use System.Img_BIU;
with System.Img_Uns;  use System.Img_Uns;
with System.Img_LLB;  use System.Img_LLB;
with System.Img_LLU;  use System.Img_LLU;
with System.Img_LLW;  use System.Img_LLW;
with System.Img_LLLB; use System.Img_LLLB;
with System.Img_LLLU; use System.Img_LLLU;
with System.Img_LLLW; use System.Img_LLLW;
with System.Img_WIU;  use System.Img_WIU;
with System.Val_Uns;  use System.Val_Uns;
with System.Val_LLU;  use System.Val_LLU;
with System.Val_LLLU; use System.Val_LLLU;
with System.WCh_Con;  use System.WCh_Con;
with System.WCh_WtS;  use System.WCh_WtS;

package body Ada.Wide_Text_IO.Modular_IO is

   package Aux_Uns is new
     Ada.Wide_Text_IO.Integer_Aux
       (Unsigned,
        Scan_Unsigned,
        Set_Image_Unsigned,
        Set_Image_Width_Unsigned,
        Set_Image_Based_Unsigned);

   package Aux_LLU is new
     Ada.Wide_Text_IO.Integer_Aux
       (Long_Long_Unsigned,
        Scan_Long_Long_Unsigned,
        Set_Image_Long_Long_Unsigned,
        Set_Image_Width_Long_Long_Unsigned,
        Set_Image_Based_Long_Long_Unsigned);

   package Aux_LLLU is new
     Ada.Wide_Text_IO.Integer_Aux
       (Long_Long_Long_Unsigned,
        Scan_Long_Long_Long_Unsigned,
        Set_Image_Long_Long_Long_Unsigned,
        Set_Image_Width_Long_Long_Long_Unsigned,
        Set_Image_Based_Long_Long_Long_Unsigned);

   Need_LLU  : constant Boolean := Num'Base'Size > Unsigned'Size;
   Need_LLLU : constant Boolean := Num'Base'Size > Long_Long_Unsigned'Size;
   --  Throughout this generic body, we distinguish between cases where type
   --  Unsigned is acceptable, where type Long_Long_Unsigned is acceptable and
   --  where type Long_Long_Long_Unsigned is needed. These boolean constants
   --  are used to test for these cases and since they are constant, only code
   --  for the relevant case will be included in the instance.

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);

   begin
      if Need_LLLU then
         Aux_LLLU.Get (File, Long_Long_Long_Unsigned (Item), Width);
      elsif Need_LLU then
         Aux_LLU.Get (File, Long_Long_Unsigned (Item), Width);
      else
         Aux_Uns.Get (File, Unsigned (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
   begin
      Get (Current_In, Item, Width);
   end Get;

   procedure Get
     (From : Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);

      S : constant String := Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if Need_LLLU then
         Aux_LLLU.Gets (S, Long_Long_Long_Unsigned (Item), Last);
      elsif Need_LLU then
         Aux_LLU.Gets (S, Long_Long_Unsigned (Item), Last);
      else
         Aux_Uns.Gets (S, Unsigned (Item), Last);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      if Need_LLLU then
         Aux_LLLU.Put (File, Long_Long_Long_Unsigned (Item), Width, Base);
      elsif Need_LLU then
         Aux_LLU.Put (File, Long_Long_Unsigned (Item), Width, Base);
      else
         Aux_Uns.Put (File, Unsigned (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      Put (Current_Out, Item, Width, Base);
   end Put;

   procedure Put
     (To   : out Wide_String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
      S : String (To'First .. To'Last);

   begin
      if Need_LLLU then
         Aux_LLLU.Puts (S, Long_Long_Long_Unsigned (Item), Base);
      elsif Need_LLU then
         Aux_LLU.Puts (S, Long_Long_Unsigned (Item), Base);
      else
         Aux_Uns.Puts (S, Unsigned (Item), Base);
      end if;

      for J in S'Range loop
         To (J) := Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Text_IO.Modular_IO;
