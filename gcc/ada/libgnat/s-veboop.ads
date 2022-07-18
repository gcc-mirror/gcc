------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--    S Y S T E M . V E C T O R S . B O O L E A N _ O P E R A T I O N S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2024, Free Software Foundation, Inc.         --
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

--  This package contains functions for runtime operations on boolean vectors

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

package System.Vectors.Boolean_Operations
  with Pure, SPARK_Mode
is
   pragma Warnings (Off, "aspect ""Pre"" not enforced on inlined subprogram",
                    Reason => "Pre only used in proof");
   pragma Warnings (Off, "aspect ""Post"" not enforced on inlined subprogram",
                    Reason => "Post only used in proof");

   --  Type Vectors.Vector represents an array of Boolean, each of which
   --  takes 8 bits of the representation, with the 7 msb set to zero. Express
   --  in contracts the constraint on valid vectors and the model that they
   --  represent, and the relationship between input models and output model.

   Vector_Boolean_Size : constant Positive :=
     System.Word_Size / System.Storage_Unit
   with Ghost;

   type Vector_Element is mod 2 ** System.Storage_Unit with Ghost;

   type Vector_Boolean_Array is array (1 .. Vector_Boolean_Size) of Boolean
   with Ghost;

   function Shift_Right (V : Vectors.Vector; N : Natural) return Vectors.Vector
   with Ghost, Import, Convention => Intrinsic;

   function Element (V : Vectors.Vector; N : Positive) return Vector_Element is
     (Vector_Element (Shift_Right (V, (N - 1) * System.Storage_Unit)
                        and (2 ** System.Storage_Unit - 1)))
   with
     Ghost,
     Pre => N <= Vector_Boolean_Size;
   --  Return the Nth element represented by the vector

   function Valid (V : Vectors.Vector) return Boolean is
     (for all J in 1 .. Vector_Boolean_Size =>
        Element (V, J) in 0 .. 1)
   with Ghost;
   --  A valid vector is one for which all elements are 0 (representing False)
   --  or 1 (representing True).

   function Model (V : Vectors.Vector) return Vector_Boolean_Array
   with
     Ghost,
     Pre => Valid (V);

   function Model (V : Vectors.Vector) return Vector_Boolean_Array is
     (for J in 1 .. Vector_Boolean_Size => Element (V, J) = 1);
   --  The model of a valid vector is the corresponding array of Boolean values

   --  Although in general the boolean operations on arrays of booleans are
   --  identical to operations on arrays of unsigned words of the same size,
   --  for the "not" operator this is not the case as False is typically
   --  represented by 0 and true by 1.

   function "not" (Item : Vectors.Vector) return Vectors.Vector
   with
     Pre  => Valid (Item),
     Post => Valid ("not"'Result)
       and then (for all J in 1 .. Vector_Boolean_Size =>
                   Model ("not"'Result) (J) = not Model (Item) (J));

   function Nand (Left, Right : Boolean) return Boolean
   with
     Post => Nand'Result = not (Left and Right);

   function Nor  (Left, Right : Boolean) return Boolean
   with
     Post => Nor'Result = not (Left or Right);

   function Nxor (Left, Right : Boolean) return Boolean
   with
     Post => Nxor'Result = not (Left xor Right);

   function Nand (Left, Right : Vectors.Vector) return Vectors.Vector
   with
     Pre  => Valid (Left)
       and then Valid (Right),
     Post => Valid (Nand'Result)
       and then (for all J in 1 .. Vector_Boolean_Size =>
                   Model (Nand'Result) (J) =
                     Nand (Model (Left) (J), Model (Right) (J)));

   function Nor (Left, Right : Vectors.Vector) return Vectors.Vector
   with
     Pre  => Valid (Left)
       and then Valid (Right),
     Post => Valid (Nor'Result)
       and then (for all J in 1 .. Vector_Boolean_Size =>
                   Model (Nor'Result) (J) =
                     Nor (Model (Left) (J), Model (Right) (J)));

   function Nxor (Left, Right : Vectors.Vector) return Vectors.Vector
   with
     Pre  => Valid (Left)
       and then Valid (Right),
     Post => Valid (Nxor'Result)
       and then (for all J in 1 .. Vector_Boolean_Size =>
                   Model (Nxor'Result) (J) =
                     Nxor (Model (Left) (J), Model (Right) (J)));
   --  The three boolean operations "nand", "nor" and "nxor" are needed
   --  for cases where the compiler moves boolean array operations into
   --  the body of the loop that iterates over the array elements.
   --
   --  Note the following equivalences:
   --    (not X) or  (not Y)  =  not (X and Y)  =  Nand (X, Y)
   --    (not X) and (not Y)  =  not (X or Y)   =  Nor  (X, Y)
   --    (not X) xor (not Y)  =  X xor Y
   --    X       xor (not Y)  =  not (X xor Y)  =  Nxor (X, Y)

   pragma Inline_Always ("not");
   pragma Inline_Always (Nand);
   pragma Inline_Always (Nor);
   pragma Inline_Always (Nxor);
end System.Vectors.Boolean_Operations;
