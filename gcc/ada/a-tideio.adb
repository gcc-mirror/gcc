------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--               A D A . T E X T _ I O . D E C I M A L _ I O                --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Text_IO.Decimal_Aux;

package body Ada.Text_IO.Decimal_IO is

   package Aux renames Ada.Text_IO.Decimal_Aux;

   Scale : constant Integer := Num'Scale;

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
      if Num'Size > Integer'Size then
         Item := Num'Fixed_Value (Aux.Get_LLD (File, Width, Scale));

      else
         Item := Num'Fixed_Value (Aux.Get_Dec (File, Width, Scale));
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : in Field := 0)
   is
   begin
      Get (Current_In, Item, Width);
   end Get;

   procedure Get
     (From : in String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

   begin
      if Num'Size > Integer'Size then
         Item := Num'Fixed_Value
                   (Aux.Gets_LLD (From, Last'Unrestricted_Access, Scale));
      else
         Item := Num'Fixed_Value
                   (Aux.Gets_Dec (From, Last'Unrestricted_Access, Scale));
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : in File_Type;
      Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      if Num'Size > Integer'Size then
         Aux.Put_LLD
           (File, Long_Long_Integer'Integer_Value (Item),
            Fore, Aft, Exp, Scale);
      else
         Aux.Put_Dec
           (File, Integer'Integer_Value (Item), Fore, Aft, Exp, Scale);
      end if;
   end Put;

   procedure Put
     (Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      Put (Current_Out, Item, Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out String;
      Item : in Num;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      if Num'Size > Integer'Size then
         Aux.Puts_LLD
           (To, Long_Long_Integer'Integer_Value (Item), Aft, Exp, Scale);
      else
         Aux.Puts_Dec (To, Integer'Integer_Value (Item), Aft, Exp, Scale);
      end if;
   end Put;

end Ada.Text_IO.Decimal_IO;
