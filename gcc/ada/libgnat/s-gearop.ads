------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       S Y S T E M . G E N E R I C _ A R R A Y _ O P E R A T I O N S      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2023, Free Software Foundation, Inc.         --
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

--  Proof of this unit is only done up to silver level, i.e. absence of runtime
--  errors, and only regarding runtime checks that depend on the generic part,
--  ignoring runtime checks related to formal generic subprogram parameters
--  in instantiations. For example, contracts do not protect against scalar
--  overflows in arithmetic operations passed on as formal generic subprogram
--  parameters.

--  Preconditions in this unit are meant mostly for analysis, but will be
--  activated at runtime depending on the assertion policy for preconditions at
--  the program point of instantiation. These preconditions are simply checking
--  bounds, so should not impact running time.

package System.Generic_Array_Operations
  with SPARK_Mode
is

   pragma Pure (Generic_Array_Operations);

   ---------------------
   -- Back_Substitute --
   ---------------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      with function "-" (Left, Right : Scalar) return Scalar is <>;
      with function "*" (Left, Right : Scalar) return Scalar is <>;
      with function "/" (Left, Right : Scalar) return Scalar is <>;
      with function Is_Non_Zero (X : Scalar) return Boolean is <>;
   procedure Back_Substitute (M, N : in out Matrix)
   with
     Pre => M'First (1) = N'First (1)
       and then M'Last (1) = N'Last (1);

   --------------
   -- Diagonal --
   --------------

   generic
      type Scalar is private;
      type Vector is array (Integer range <>) of Scalar;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   function Diagonal (A : Matrix) return Vector
   with
     Pre => A'First (1) < A'Last (1)
       and then A'First (2) < A'Last (2)
       and then (if A'First (1) <= 0 then
                   A'Last (1) < Integer'Last + A'First (1))
       and then (if A'First (2) <= 0 then
                   A'Last (2) < Integer'Last + A'First (2));

   -----------------------
   -- Forward_Eliminate --
   -----------------------

   --  Use elementary row operations to put square matrix M in row echolon
   --  form. Identical row operations are performed on matrix N, must have the
   --  same number of rows as M.

   generic
      type Scalar is private;
      type Real is digits <>;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      with function "abs" (Right : Scalar) return Real'Base is <>;
      with function "-" (Left, Right : Scalar) return Scalar is <>;
      with function "*" (Left, Right : Scalar) return Scalar is <>;
      with function "/" (Left, Right : Scalar) return Scalar is <>;
      Zero : Scalar;
      One  : Scalar;
   procedure Forward_Eliminate
     (M   : in out Matrix;
      N   : in out Matrix;
      Det : out Scalar)
   with
     Pre => M'First (1) = N'First (1)
       and then M'Last (1) = N'Last (1);

   --------------------------
   -- Square_Matrix_Length --
   --------------------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   function Square_Matrix_Length (A : Matrix) return Natural
   with
     Pre => (if A'First (1) <= 0 then
               A'Last (1) < Integer'Last + A'First (1))
       and then (if A'First (2) <= 0 then
                   A'Last (2) < Integer'Last + A'First (2))
       and then A'Length (1) = A'Length (2);
   --  If A is non-square, raise Constraint_Error, else return its dimension

   ----------------------------------
   -- Vector_Elementwise_Operation --
   ----------------------------------

   generic
      type X_Scalar is private;
      type Result_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation (X : X_Scalar) return Result_Scalar;
   function Vector_Elementwise_Operation (X : X_Vector) return Result_Vector;

   ----------------------------------
   -- Matrix_Elementwise_Operation --
   ----------------------------------

   generic
      type X_Scalar is private;
      type Result_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation (X : X_Scalar) return Result_Scalar;
   function Matrix_Elementwise_Operation (X : X_Matrix) return Result_Matrix;

   -----------------------------------------
   -- Vector_Vector_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Vector_Vector_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Vector
   with
     Pre => (if Left'First <= 0 then
               Left'Last < Integer'Last + Left'First)
       and then (if Right'First <= 0 then
                   Right'Last < Integer'Last + Right'First)
       and then Left'Length = Right'Length;

   ------------------------------------------------
   -- Vector_Vector_Scalar_Elementwise_Operation --
   ------------------------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type Z_Scalar is private;
      type Result_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Y_Vector is array (Integer range <>) of Y_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (X : X_Scalar;
              Y : Y_Scalar;
              Z : Z_Scalar) return Result_Scalar;
   function Vector_Vector_Scalar_Elementwise_Operation
     (X : X_Vector;
      Y : Y_Vector;
      Z : Z_Scalar) return Result_Vector
   with
     Pre => (if X'First <= 0 then X'Last < Integer'Last + X'First)
       and then (if Y'First <= 0 then Y'Last < Integer'Last + Y'First)
       and then X'Length = Y'Length;

   -----------------------------------------
   -- Matrix_Matrix_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Matrix_Matrix_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix
   with
     Pre => (if Left'First (1) <= 0 then
               Left'Last (1) < Integer'Last + Left'First (1))
       and then (if Right'First (1) <= 0 then
                   Right'Last (1) < Integer'Last + Right'First (1))
       and then Left'Length (1) = Right'Length (1)
       and then (if Left'First (2) <= 0 then
                   Left'Last (2) < Integer'Last + Left'First (2))
       and then (if Right'First (2) <= 0 then
                   Right'Last (2) < Integer'Last + Right'First (2))
       and then Left'Length (2) = Right'Length (2);

   ------------------------------------------------
   -- Matrix_Matrix_Scalar_Elementwise_Operation --
   ------------------------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type Z_Scalar is private;
      type Result_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Y_Matrix is array (Integer range <>, Integer range <>) of Y_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (X : X_Scalar;
              Y : Y_Scalar;
              Z : Z_Scalar) return Result_Scalar;
   function Matrix_Matrix_Scalar_Elementwise_Operation
     (X : X_Matrix;
      Y : Y_Matrix;
      Z : Z_Scalar) return Result_Matrix
   with
     Pre => (if X'First (1) <= 0 then
               X'Last (1) < Integer'Last + X'First (1))
       and then (if Y'First (1) <= 0 then
                   Y'Last (1) < Integer'Last + Y'First (1))
       and then X'Length (1) = Y'Length (1)
       and then (if X'First (2) <= 0 then
                   X'Last (2) < Integer'Last + X'First (2))
       and then (if Y'First (2) <= 0 then
                   Y'Last (2) < Integer'Last + Y'First (2))
       and then X'Length (2) = Y'Length (2);

   -----------------------------------------
   -- Vector_Scalar_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Vector_Scalar_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Scalar) return Result_Vector;

   -----------------------------------------
   -- Matrix_Scalar_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Matrix_Scalar_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Scalar) return Result_Matrix;

   -----------------------------------------
   -- Scalar_Vector_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Scalar_Vector_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Vector) return Result_Vector;

   -----------------------------------------
   -- Scalar_Matrix_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Scalar_Matrix_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Matrix) return Result_Matrix;

   -------------------
   -- Inner_Product --
   -------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Inner_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Scalar
   with
     Pre => (if Left'First <= 0 then
               Left'Last < Integer'Last + Left'First)
       and then (if Right'First <= 0 then
                   Right'Last < Integer'Last + Right'First)
       and then Left'Length = Right'Length;

   -------------
   -- L2_Norm --
   -------------

   generic
      type X_Scalar is private;
      type Result_Real is digits <>;
      type X_Vector is array (Integer range <>) of X_Scalar;
      with function "abs" (Right : X_Scalar) return Result_Real is <>;
      with function Sqrt (X : Result_Real'Base) return Result_Real'Base is <>;
   function L2_Norm (X : X_Vector) return Result_Real'Base;

   -------------------
   -- Outer_Product --
   -------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
   function Outer_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Matrix;

   ---------------------------
   -- Matrix_Vector_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Matrix_Vector_Product
     (Left  : Matrix;
      Right : Right_Vector) return Result_Vector
   with
     Pre => (if Left'First (2) <= 0 then
               Left'Last (2) < Integer'Last + Left'First (2))
       and then (if Right'First <= 0 then
                   Right'Last < Integer'Last + Right'First)
       and then Left'Length (2) = Right'Length;

   ---------------------------
   -- Vector_Matrix_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Vector_Matrix_Product
     (Left  : Left_Vector;
      Right : Matrix) return Result_Vector
   with
     Pre => (if Left'First <= 0 then
               Left'Last < Integer'Last + Left'First)
       and then (if Right'First (1) <= 0 then
                   Right'Last (1) < Integer'Last + Right'First (1))
       and then Left'Length = Right'Length (1);

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Matrix_Matrix_Product
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix
   with
     Pre => (if Left'First (2) <= 0 then
               Left'Last (2) < Integer'Last + Left'First (2))
       and then (if Right'First (1) <= 0 then
                   Right'Last (1) < Integer'Last + Right'First (1))
       and then Left'Length (2) = Right'Length (1);

   ----------------------------
   -- Matrix_Vector_Solution --
   ----------------------------

   generic
      type Scalar is private;
      Zero : Scalar;
      type Vector is array (Integer range <>) of Scalar;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      with procedure Back_Substitute (M, N : in out Matrix) is <>;
      with procedure Forward_Eliminate
             (M   : in out Matrix;
              N   : in out Matrix;
              Det : out Scalar) is <>;
   function Matrix_Vector_Solution (A : Matrix; X : Vector) return Vector
   with
     Pre => (if A'First (1) <= 0 then
               A'Last (1) < Integer'Last + A'First (1))
       and then (if A'First (2) <= 0 then
                   A'Last (2) < Integer'Last + A'First (2))
       and then A'Length (1) = A'Length (2)
       and then (if X'First <= 0 then
                   X'Last < Integer'Last + X'First)
       and then A'Length (1) = X'Length;

   ----------------------------
   -- Matrix_Matrix_Solution --
   ----------------------------

   generic
      type Scalar is private;
      Zero : Scalar;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      with procedure Back_Substitute (M, N : in out Matrix) is <>;
      with procedure Forward_Eliminate
             (M   : in out Matrix;
              N   : in out Matrix;
              Det : out Scalar) is <>;
   function Matrix_Matrix_Solution (A : Matrix; X : Matrix) return Matrix
   with
     Pre => (if A'First (1) <= 0 then
               A'Last (1) < Integer'Last + A'First (1))
       and then (if A'First (2) <= 0 then
                   A'Last (2) < Integer'Last + A'First (2))
       and then A'Length (1) = A'Length (2)
       and then (if X'First (1) <= 0 then
                   X'Last (1) < Integer'Last + X'First (1))
       and then A'Length (1) = X'Length (1);

   ----------
   -- Sqrt --
   ----------

   generic
      type Real is digits <>;
   function Sqrt (X : Real'Base) return Real'Base;

   -----------------
   -- Swap_Column --
   -----------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   procedure Swap_Column (A : in out Matrix; Left, Right : Integer)
   with
     Pre => Left in A'Range (2)
       and then Right in A'Range (2);

   ---------------
   -- Transpose --
   ---------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   procedure Transpose (A : Matrix; R : out Matrix)
   with
     Relaxed_Initialization => R,
     Pre  => A'First (1) = R'First (2)
       and then A'Last (1) = R'Last (2)
       and then A'First (2) = R'First (1)
       and then A'Last (2) = R'Last (1)
       and then (if A'First (1) < 0 then
                   A'Last (1) <= Integer'Last + A'First (1))
       and then (if A'First (2) < 0 then
                   A'Last (2) <= Integer'Last + A'First (2)),
     Post => R'Initialized;

   -------------------------------
   -- Update_Vector_With_Vector --
   -------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Y_Vector is array (Integer range <>) of Y_Scalar;
      with procedure Update (X : in out X_Scalar; Y : Y_Scalar);
   procedure Update_Vector_With_Vector (X : in out X_Vector; Y : Y_Vector)
   with
     Pre => (if X'First <= 0 then
               X'Last < Integer'Last + X'First)
       and then (if Y'First <= 0 then
                   Y'Last < Integer'Last + Y'First)
       and then X'Length = Y'Length;

   -------------------------------
   -- Update_Matrix_With_Matrix --
   -------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Y_Matrix is array (Integer range <>, Integer range <>) of Y_Scalar;
      with procedure Update (X : in out X_Scalar; Y : Y_Scalar);
   procedure Update_Matrix_With_Matrix (X : in out X_Matrix; Y : Y_Matrix)
   with
     Pre => (if X'First (1) <= 0 then
               X'Last (1) < Integer'Last + X'First (1))
       and then (if Y'First (1) <= 0 then
                   Y'Last (1) < Integer'Last + Y'First (1))
       and then X'Length (1) = Y'Length (1)
       and then (if X'First (2) <= 0 then
                   X'Last (2) < Integer'Last + X'First (2))
       and then (if Y'First (2) <= 0 then
                   Y'Last (2) < Integer'Last + Y'First (2))
       and then X'Length (2) = Y'Length (2);

   -----------------
   -- Unit_Matrix --
   -----------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      Zero : Scalar;
      One  : Scalar;
   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Matrix
   with
     Pre => First_1 <= Integer'Last - Order + 1
       and then First_2 <= Integer'Last - Order + 1;

   -----------------
   -- Unit_Vector --
   -----------------

   generic
      type Scalar is private;
      type Vector is array (Integer range <>) of Scalar;
      Zero : Scalar;
      One  : Scalar;
   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Vector
   with
     Pre => Index >= First
       and then First <= Integer'Last - Order + 1
       and then Index <= First + (Order - 1);

end System.Generic_Array_Operations;
