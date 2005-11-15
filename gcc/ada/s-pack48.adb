------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . P A C K _ 4 8                        --
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

with System.Storage_Elements;
with System.Unsigned_Types;
with Unchecked_Conversion;

package body System.Pack_48 is

   subtype Ofs is System.Storage_Elements.Storage_Offset;
   subtype Uns is System.Unsigned_Types.Unsigned;
   subtype N07 is System.Unsigned_Types.Unsigned range 0 .. 7;

   use type System.Storage_Elements.Storage_Offset;
   use type System.Unsigned_Types.Unsigned;

   type Cluster is record
      E0, E1, E2, E3, E4, E5, E6, E7 : Bits_48;
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

   function To_Ref is new
     Unchecked_Conversion (System.Address, Cluster_Ref);

   --  The following declarations are for the case where the address
   --  passed to GetU_48 or SetU_48 is not guaranteed to be aligned.
   --  These routines are used when the packed array is itself a
   --  component of a packed record, and therefore may not be aligned.

   type ClusterU is new Cluster;
   for ClusterU'Alignment use 1;

   type ClusterU_Ref is access ClusterU;

   function To_Ref is new
     Unchecked_Conversion (System.Address, ClusterU_Ref);

   ------------
   -- Get_48 --
   ------------

   function Get_48 (Arr : System.Address; N : Natural) return Bits_48 is
      C : constant Cluster_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
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
   end Get_48;

   -------------
   -- GetU_48 --
   -------------

   function GetU_48 (Arr : System.Address; N : Natural) return Bits_48 is
      C : constant ClusterU_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
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
   end GetU_48;

   ------------
   -- Set_48 --
   ------------

   procedure Set_48 (Arr : System.Address; N : Natural; E : Bits_48) is
      C : constant Cluster_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
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
   end Set_48;

   -------------
   -- SetU_48 --
   -------------

   procedure SetU_48 (Arr : System.Address; N : Natural; E : Bits_48) is
      C : constant ClusterU_Ref := To_Ref (Arr + Bits * Ofs (Uns (N) / 8));
   begin
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
   end SetU_48;

end System.Pack_48;
