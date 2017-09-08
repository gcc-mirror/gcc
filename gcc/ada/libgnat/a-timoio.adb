------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . M O D U L A R _ I O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Modular_Aux;

with System.Unsigned_Types; use System.Unsigned_Types;

package body Ada.Text_IO.Modular_IO is

   package Aux renames Ada.Text_IO.Modular_Aux;

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      if Num'Size > Unsigned'Size then
         Aux.Get_LLU (File, Long_Long_Unsigned (Item), Width);
      else
         Aux.Get_Uns (File, Unsigned (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      if Num'Size > Unsigned'Size then
         Aux.Get_LLU (Current_In, Long_Long_Unsigned (Item), Width);
      else
         Aux.Get_Uns (Current_In, Unsigned (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

   begin
      if Num'Size > Unsigned'Size then
         Aux.Gets_LLU (From, Long_Long_Unsigned (Item), Last);
      else
         Aux.Gets_Uns (From, Unsigned (Item), Last);
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
      if Num'Size > Unsigned'Size then
         Aux.Put_LLU (File, Long_Long_Unsigned (Item), Width, Base);
      else
         Aux.Put_Uns (File, Unsigned (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Put_LLU (Current_Out, Long_Long_Unsigned (Item), Width, Base);
      else
         Aux.Put_Uns (Current_Out, Unsigned (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (To   : out String;
      Item : Num;
      Base : Number_Base := Default_Base)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Puts_LLU (To, Long_Long_Unsigned (Item), Base);
      else
         Aux.Puts_Uns (To, Unsigned (Item), Base);
      end if;
   end Put;

end Ada.Text_IO.Modular_IO;
