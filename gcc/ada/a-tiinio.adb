------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--               A D A . T E X T _ I O . I N T E G E R _ I O                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

with Ada.Text_IO.Integer_Aux;

package body Ada.Text_IO.Integer_IO is

   package Aux renames Ada.Text_IO.Integer_Aux;

   Need_LLI : constant Boolean := Num'Base'Size > Integer'Size;
   --  Throughout this generic body, we distinguish between the case
   --  where type Integer is acceptable, and where a Long_Long_Integer
   --  is needed. This constant Boolean is used to test for these cases
   --  and since it is a constant, only the code for the relevant case
   --  will be included in the instance.

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : in File_Type;
      Item  : out Num;
      Width : in Field := 0)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);
      pragma Unsuppress (Overflow_Check);

   begin
      if Need_LLI then
         Aux.Get_LLI (File, Long_Long_Integer (Item), Width);
      else
         Aux.Get_Int (File, Integer (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : in Field := 0)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);
      pragma Unsuppress (Overflow_Check);

   begin
      if Need_LLI then
         Aux.Get_LLI (Current_In, Long_Long_Integer (Item), Width);
      else
         Aux.Get_Int (Current_In, Integer (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (From : in String;
      Item : out Num;
      Last : out Positive)
   is
      --  We depend on a range check to get Data_Error

      pragma Unsuppress (Range_Check);
      pragma Unsuppress (Overflow_Check);

   begin
      if Need_LLI then
         Aux.Gets_LLI (From, Long_Long_Integer (Item), Last);
      else
         Aux.Gets_Int (From, Integer (Item), Last);
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
      if Need_LLI then
         Aux.Put_LLI (File, Long_Long_Integer (Item), Width, Base);
      else
         Aux.Put_Int (File, Integer (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (Item  : in Num;
      Width : in Field := Default_Width;
      Base  : in Number_Base := Default_Base)
   is
   begin
      if Need_LLI then
         Aux.Put_LLI (Current_Out, Long_Long_Integer (Item), Width, Base);
      else
         Aux.Put_Int (Current_Out, Integer (Item), Width, Base);
      end if;
   end Put;

   procedure Put
     (To   : out String;
      Item : in Num;
      Base : in Number_Base := Default_Base)
   is
   begin
      if Need_LLI then
         Aux.Puts_LLI (To, Long_Long_Integer (Item), Base);
      else
         Aux.Puts_Int (To, Integer (Item), Base);
      end if;
   end Put;

end Ada.Text_IO.Integer_IO;
