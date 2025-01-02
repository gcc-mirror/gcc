------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . I N T E G E R _ I O          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
with System.Img_BIU; use System.Img_BIU;
with System.Img_Int; use System.Img_Int;
with System.Img_LLB; use System.Img_LLB;
with System.Img_LLI; use System.Img_LLI;
with System.Img_LLW; use System.Img_LLW;
with System.Img_WIU; use System.Img_WIU;
with System.Val_Int; use System.Val_Int;
with System.Val_LLI; use System.Val_LLI;
with System.WCh_Con; use System.WCh_Con;
with System.WCh_WtS; use System.WCh_WtS;

package body Ada.Wide_Text_IO.Integer_IO is

   package Aux_Int is new
     Ada.Wide_Text_IO.Integer_Aux
       (Integer,
        Scan_Integer,
        Set_Image_Integer,
        Set_Image_Width_Integer,
        Set_Image_Based_Integer);

   package Aux_LLI is new
     Ada.Wide_Text_IO.Integer_Aux
       (Long_Long_Integer,
        Scan_Long_Long_Integer,
        Set_Image_Long_Long_Integer,
        Set_Image_Width_Long_Long_Integer,
        Set_Image_Based_Long_Long_Integer);

   Need_LLI : constant Boolean := Num'Base'Size > Integer'Size;
   --  Throughout this generic body, we distinguish between the case where type
   --  Integer is acceptable, and where a Long_Long_Integer is needed. This
   --  Boolean is used to test for these cases and since it is a constant, only
   --  code for the relevant case will be included in the instance.

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
      pragma Unsuppress (Overflow_Check);

   begin
      if Need_LLI then
         Aux_LLI.Get (File, Long_Long_Integer (Item), Width);
      else
         Aux_Int.Get (File, Integer (Item), Width);
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
      pragma Unsuppress (Overflow_Check);

      S : constant String := Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if Need_LLI then
         Aux_LLI.Gets (S, Long_Long_Integer (Item), Last);
      else
         Aux_Int.Gets (S, Integer (Item), Last);
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
      if Need_LLI then
         Aux_LLI.Put (File, Long_Long_Integer (Item), Width, Base);
      else
         Aux_Int.Put (File, Integer (Item), Width, Base);
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
      if Need_LLI then
         Aux_LLI.Puts (S, Long_Long_Integer (Item), Base);
      else
         Aux_Int.Puts (S, Integer (Item), Base);
      end if;

      for J in S'Range loop
         To (J) := Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Text_IO.Integer_IO;
