------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . V E C T O R S . B O O L E A N _ O P E R A T I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2018, Free Software Foundation, Inc.         --
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

package body System.Vectors.Boolean_Operations is

   SU : constant := Storage_Unit;
   --  Convenient short hand, used throughout

   --  The coding of this unit depends on the fact that the Component_Size
   --  of a normally declared array of Boolean is equal to Storage_Unit. We
   --  can't use the Component_Size directly since it is non-static. The
   --  following declaration checks that this declaration is correct

   type Boolean_Array is array (Integer range <>) of Boolean;
   pragma Compile_Time_Error
     (Boolean_Array'Component_Size /= SU, "run time compile failure");

   --  NOTE: The boolean literals must be qualified here to avoid visibility
   --  anomalies when this package is compiled through Rtsfind, in a context
   --  that includes a user-defined type derived from boolean.

   True_Val : constant Vector := Standard.True'Enum_Rep
                                   + Standard.True'Enum_Rep * 2**SU
                                   + Standard.True'Enum_Rep * 2**(SU * 2)
                                   + Standard.True'Enum_Rep * 2**(SU * 3)
                                   + Standard.True'Enum_Rep * 2**(SU * 4)
                                   + Standard.True'Enum_Rep * 2**(SU * 5)
                                   + Standard.True'Enum_Rep * 2**(SU * 6)
                                   + Standard.True'Enum_Rep * 2**(SU * 7);
   --  This constant represents the bits to be flipped to perform a logical
   --  "not" on a vector of booleans, independent of the actual
   --  representation of True.

   --  The representations of (False, True) are assumed to be zero/one and
   --  the maximum number of unpacked booleans per Vector is assumed to be 8.

   pragma Assert (Standard.False'Enum_Rep = 0);
   pragma Assert (Standard.True'Enum_Rep = 1);
   pragma Assert (Vector'Size / Storage_Unit <= 8);

   --  The reason we need to do these gymnastics is that no call to
   --  Unchecked_Conversion can be made at the library level since this
   --  unit is pure. Also a conversion from the array type to the Vector type
   --  inside the body of "not" is inefficient because of alignment issues.

   -----------
   -- "not" --
   -----------

   function "not" (Item : Vectors.Vector) return Vectors.Vector is
   begin
      return Item xor True_Val;
   end "not";

   ----------
   -- Nand --
   ----------

   function Nand (Left, Right : Boolean) return Boolean is
   begin
      return not (Left and Right);
   end Nand;

   function Nand (Left, Right : Vectors.Vector) return Vectors.Vector is
   begin
      return not (Left and Right);
   end Nand;

   ---------
   -- Nor --
   ---------

   function Nor (Left, Right : Boolean) return Boolean is
   begin
      return not (Left or Right);
   end Nor;

   function Nor (Left, Right : Vectors.Vector) return Vectors.Vector is
   begin
      return not (Left or Right);
   end Nor;

   ----------
   -- Nxor --
   ----------

   function Nxor (Left, Right : Boolean) return Boolean is
   begin
      return not (Left xor Right);
   end Nxor;

   function Nxor (Left, Right : Vectors.Vector) return Vectors.Vector is
   begin
      return not (Left xor Right);
   end Nxor;

end System.Vectors.Boolean_Operations;
