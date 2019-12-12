------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T E X T _ I O . F L O A T _ I O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Float_Aux;

package body Ada.Text_IO.Float_IO is

   package Aux renames Ada.Text_IO.Float_Aux;

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
      Width : Field := 0)
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
     (From : String;
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
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Aux.Put (File, Long_Long_Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Aux.Put (Current_Out, Long_Long_Float (Item), Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Aux.Puts (To, Long_Long_Float (Item), Aft, Exp);
   end Put;

end Ada.Text_IO.Float_IO;
