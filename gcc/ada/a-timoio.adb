------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . M O D U L A R _ I O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
     (File  : in File_Type;
      Item  : out Num;
      Width : in Field := 0)
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
      Width : in Field := 0)
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
     (From : in String;
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
     (File  : in File_Type;
      Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Put_LLU (File, Long_Long_Unsigned (Item), Width, Base);
      else
         Aux.Put_Uns (File, Unsigned (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base)
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
      Item : in Num;
      Base : in Number_Base := Default_Base)
   is
   begin
      if Num'Size > Unsigned'Size then
         Aux.Puts_LLU (To, Long_Long_Unsigned (Item), Base);
      else
         Aux.Puts_Uns (To, Unsigned (Item), Base);
      end if;
   end Put;

end Ada.Text_IO.Modular_IO;
