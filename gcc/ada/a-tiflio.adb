------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T E X T _ I O . F L O A T _ I O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Ada.Text_IO.Float_Aux;

package body Ada.Text_IO.Float_IO is

   package Aux renames Ada.Text_IO.Float_Aux;

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
      Aux.Get (File, Long_Long_Float (Item), Width);

      --  In the case where the type is unconstrained (e.g. Standard'Float),
      --  the above conversion may result in an infinite value, which is
      --  normally fine for a conversion, but in this case, we want to treat
      --  that as a data error.

      if not Item'Valid then
         raise Data_Error;
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
      Aux.Get (Current_In, Long_Long_Float (Item), Width);

      --  In the case where the type is unconstrained (e.g. Standard'Float),
      --  the above conversion may result in an infinite value, which is
      --  normally fine for a conversion, but in this case, we want to treat
      --  that as a data error.

      if not Item'Valid then
         raise Data_Error;
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
      Aux.Gets (From, Long_Long_Float (Item), Last);

      --  In the case where the type is unconstrained (e.g. Standard'Float),
      --  the above conversion may result in an infinite value, which is
      --  normally fine for a conversion, but in this case, we want to treat
      --  that as a data error.

      if not Item'Valid then
         raise Data_Error;
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
      Aux.Put (File, Long_Long_Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      Aux.Put (Current_Out, Long_Long_Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out String;
      Item : in Num;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp)
   is
   begin
      Aux.Puts (To, Long_Long_Float (Item), Aft, Exp);
   end Put;

end Ada.Text_IO.Float_IO;
