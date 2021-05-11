------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . P A C K _ 5 9                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with System.Storage_Elements;
with System.Unsigned_Types;

package body System.Pack_59 is

   subtype Bit_Order is System.Bit_Order;
   Reverse_Bit_Order : constant Bit_Order :=
     Bit_Order'Val (1 - Bit_Order'Pos (System.Default_Bit_Order));

   subtype Ofs is System.Storage_Elements.Storage_Offset;
   subtype Uns is System.Unsigned_Types.Unsigned;
   subtype N07 is System.Unsigned_Types.Unsigned range 0 .. 7;

   use type System.Storage_Elements.Storage_Offset;
   use type System.Unsigned_Types.Unsigned;

   type Cluster is record
      E0, E1, E2, E3, E4, E5, E6, E7 : Bits_59;
   end record;

   for Cluster use record
      E0 at 0 range 0 * Bits .. 0 * Bits + Bits - 1;
      E1 at 0 range 1 * Bits .. 1 * Bits + Bits - 1;
      E2 at 0 range 2 * Bits .. 2 * Bits + Bits - 1;
      E3 at 0 range 3 * Bits .. 3 * Bits + Bits - 1;
      E4 at 0 range 4 * Bits .. 4 * Bits + Bits - 1;
      E5 at 0 range 5 * Bits .. 5 * Bits + Bits - 1;
      E6 at 0 range 6 * Bits .. 6 * Bits + Bits - 1;
      E7 at 0 range 7 * Bits .. 7 * Bits + Bits - 1;
   end record;

   for Cluster'Size use Bits * 8;

   for Cluster'Alignment use Integer'Min (Standard'Maximum_Alignment,
     1 +
     1 * Boolean'Pos (Bits mod 2 = 0) +
     2 * Boolean'Pos (Bits mod 4 = 0));
   --  Use maximum possible alignment, given the bit field size, since this
   --  will result in the most efficient code possible for the field.

   type Cluster_Ref is access Cluster;

   type Rev_Cluster is new Cluster
     with Bit_Order            => Reverse_Bit_Order,
          Scalar_Storage_Order => Reverse_Bit_Order;
   type Rev_Cluster_Ref is access Rev_Cluster;

   ------------
   -- Get_59 --
   ------------

   function Get_59
     (Arr     : System.Address;
      N       : Natural;
      Rev_SSO : Boolean) return Bits_59
   is
      A  : constant System.Address := Arr + Bits * Ofs (Uns (N) / 8);
      C  : Cluster_Ref     with Address => A'Address, Import;
      RC : Rev_Cluster_Ref with Address => A'Address, Import;
   begin
      if Rev_SSO then
         case N07 (Uns (N) mod 8) is
            when 0 => return RC.E0;
            when 1 => return RC.E1;
            when 2 => return RC.E2;
            when 3 => return RC.E3;
            when 4 => return RC.E4;
            when 5 => return RC.E5;
            when 6 => return RC.E6;
            when 7 => return RC.E7;
         end case;

      else
         case N07 (Uns (N) mod 8) is
            when 0 => return C.E0;
            when 1 => return C.E1;
            when 2 => return C.E2;
            when 3 => return C.E3;
            when 4 => return C.E4;
            when 5 => return C.E5;
            when 6 => return C.E6;
            when 7 => return C.E7;
         end case;
      end if;
   end Get_59;

   ------------
   -- Set_59 --
   ------------

   procedure Set_59
     (Arr     : System.Address;
      N       : Natural;
      E       : Bits_59;
      Rev_SSO : Boolean)
   is
      A  : constant System.Address := Arr + Bits * Ofs (Uns (N) / 8);
      C  : Cluster_Ref     with Address => A'Address, Import;
      RC : Rev_Cluster_Ref with Address => A'Address, Import;
   begin
      if Rev_SSO then
         case N07 (Uns (N) mod 8) is
            when 0 => RC.E0 := E;
            when 1 => RC.E1 := E;
            when 2 => RC.E2 := E;
            when 3 => RC.E3 := E;
            when 4 => RC.E4 := E;
            when 5 => RC.E5 := E;
            when 6 => RC.E6 := E;
            when 7 => RC.E7 := E;
         end case;
      else
         case N07 (Uns (N) mod 8) is
            when 0 => C.E0 := E;
            when 1 => C.E1 := E;
            when 2 => C.E2 := E;
            when 3 => C.E3 := E;
            when 4 => C.E4 := E;
            when 5 => C.E5 := E;
            when 6 => C.E6 := E;
            when 7 => C.E7 := E;
         end case;
      end if;
   end Set_59;

end System.Pack_59;
