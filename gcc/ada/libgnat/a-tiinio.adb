------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . I N T E G E R _ I O                --
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

with Ada.Text_IO.Integer_Aux;
with System.Img_BIU; use System.Img_BIU;
with System.Img_Int; use System.Img_Int;
with System.Img_LLB; use System.Img_LLB;
with System.Img_LLI; use System.Img_LLI;
with System.Img_LLW; use System.Img_LLW;
with System.Img_WIU; use System.Img_WIU;
with System.Val_Int; use System.Val_Int;
with System.Val_LLI; use System.Val_LLI;

package body Ada.Text_IO.Integer_IO is

   package Aux_Int is new
     Ada.Text_IO.Integer_Aux
       (Integer,
        Scan_Integer,
        Set_Image_Integer,
        Set_Image_Width_Integer,
        Set_Image_Based_Integer);

   package Aux_LLI is new
     Ada.Text_IO.Integer_Aux
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
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);
      pragma Unsuppress (Overflow_Check);

   begin
      if Need_LLI then
         Aux_LLI.Gets (From, Long_Long_Integer (Item), Last);
      else
         Aux_Int.Gets (From, Integer (Item), Last);
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
     (To   : out String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
   begin
      if Need_LLI then
         Aux_LLI.Puts (To, Long_Long_Integer (Item), Base);
      else
         Aux_Int.Puts (To, Integer (Item), Base);
      end if;
   end Put;

end Ada.Text_IO.Integer_IO;
