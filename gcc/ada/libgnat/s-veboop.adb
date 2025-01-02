------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . V E C T O R S . B O O L E A N _ O P E R A T I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
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

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

package body System.Vectors.Boolean_Operations
  with SPARK_Mode
is

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

      procedure Prove_Not (Result : Vectors.Vector)
      with
        Ghost,
        Pre  => Valid (Item)
          and then Result = (Item xor True_Val),
        Post => Valid (Result)
          and then (for all J in 1 .. Vector_Boolean_Size =>
                      Model (Result) (J) = not Model (Item) (J));

      procedure Prove_Not (Result : Vectors.Vector) is
      begin
         for J in 1 .. Vector_Boolean_Size loop
            pragma Assert
              (Element (Result, J) = 1 - Element (Item, J));
         end loop;
      end Prove_Not;

   begin
      Prove_Not (Item xor True_Val);
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

      procedure Prove_And (Result : Vectors.Vector)
      with
        Ghost,
        Pre  => Valid (Left)
          and then Valid (Right)
          and then Result = (Left and Right),
          Post => Valid (Result)
            and then (for all J in 1 .. Vector_Boolean_Size =>
                        Model (Result) (J) =
                          (Model (Left) (J) and Model (Right) (J)));

      procedure Prove_And (Result : Vectors.Vector) is
      begin
         for J in 1 .. Vector_Boolean_Size loop
            pragma Assert
              (Element (Result, J) =
                 (if Element (Left, J) = 1
                    and Element (Right, J) = 1
                  then 1
                  else 0));
         end loop;
      end Prove_And;

   begin
      Prove_And (Left and Right);
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

      procedure Prove_Or (Result : Vectors.Vector)
      with
        Ghost,
        Pre  => Valid (Left)
          and then Valid (Right)
          and then Result = (Left or Right),
          Post => Valid (Result)
            and then (for all J in 1 .. Vector_Boolean_Size =>
                        Model (Result) (J) =
                          (Model (Left) (J) or Model (Right) (J)));

      procedure Prove_Or (Result : Vectors.Vector) is
      begin
         for J in 1 .. Vector_Boolean_Size loop
            pragma Assert
              (Element (Result, J) =
                 (if Element (Left, J) = 1
                    or Element (Right, J) = 1
                  then 1
                  else 0));
         end loop;
      end Prove_Or;

   begin
      Prove_Or (Left or Right);
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

      procedure Prove_Xor (Result : Vectors.Vector)
      with
        Ghost,
        Pre  => Valid (Left)
          and then Valid (Right)
          and then Result = (Left xor Right),
          Post => Valid (Result)
            and then (for all J in 1 .. Vector_Boolean_Size =>
                        Model (Result) (J) =
                          (Model (Left) (J) xor Model (Right) (J)));

      procedure Prove_Xor (Result : Vectors.Vector) is
      begin
         for J in 1 .. Vector_Boolean_Size loop
            pragma Assert
              (Element (Result, J) =
                 (if Element (Left, J) = 1
                    xor Element (Right, J) = 1
                  then 1
                  else 0));
         end loop;
      end Prove_Xor;

   begin
      Prove_Xor (Left xor Right);
      return not (Left xor Right);
   end Nxor;

end System.Vectors.Boolean_Operations;
