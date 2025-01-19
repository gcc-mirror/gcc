------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       S Y S T E M . G E N E R I C _ A R R A Y _ O P E R A T I O N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2025, Free Software Foundation, Inc.          --
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

--  Preconditions, postconditions, ghost code, loop invariants and assertions
--  in this unit are meant for analysis only, not for run-time checking, as it
--  would be too costly otherwise. This is enforced by setting the assertion
--  policy to Ignore, here for non-generic code, and inside the generic for
--  generic code.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Numerics; use Ada.Numerics;

package body System.Generic_Array_Operations
  with SPARK_Mode
is
   pragma Warnings
     (Off, "aspect * not enforced on inlined subprogram",
      Reason => "Contracts in this unit are never executed");

   function Check_Unit_Last
     (Index : Integer;
      Order : Positive;
      First : Integer) return Integer
   with
     Pre => Index >= First
       and then First <= Integer'Last - Order + 1
       and then Index <= First + (Order - 1),
     Post => Check_Unit_Last'Result = First + (Order - 1);

   pragma Inline_Always (Check_Unit_Last);
   --  Compute index of last element returned by Unit_Vector or Unit_Matrix.
   --  A separate function is needed to allow raising Constraint_Error before
   --  declaring the function result variable. The result variable needs to be
   --  declared first, to allow front-end inlining.

   pragma Warnings (On, "aspect * not enforced on inlined subprogram");

   --------------
   -- Diagonal --
   --------------

   function Diagonal (A : Matrix) return Vector is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);

      N : constant Natural := Natural'Min (A'Length (1), A'Length (2));
   begin
      return R : Vector (A'First (1) .. A'First (1) + (N - 1))
        with Relaxed_Initialization
      do
         for J in 0 .. N - 1 loop
            R (R'First + J) := A (A'First (1) + J, A'First (2) + J);

            pragma Loop_Invariant
              (for all JJ in R'First .. R'First + J => R (JJ)'Initialized);
         end loop;
      end return;
   end Diagonal;

   --------------------------
   -- Square_Matrix_Length --
   --------------------------

   function Square_Matrix_Length (A : Matrix) return Natural is
   begin
      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "matrix is not square";
      else
         return A'Length (1);
      end if;
   end Square_Matrix_Length;

   ---------------------
   -- Check_Unit_Last --
   ---------------------

   function Check_Unit_Last
      (Index : Integer;
       Order : Positive;
       First : Integer) return Integer
   is
   begin
      --  Order the tests carefully to avoid overflow

      if Index < First
        or else First > Integer'Last - Order + 1
        or else Index > First + (Order - 1)
      then
         raise Constraint_Error;
      end if;

      return First + (Order - 1);
   end Check_Unit_Last;

   ---------------------
   -- Back_Substitute --
   ---------------------

   procedure Back_Substitute (M, N : in out Matrix) is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
      pragma Assert (M'First (1) = N'First (1)
                       and then
                     M'Last  (1) = N'Last (1));

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
      with
        Pre => Target in M'Range (1)
          and then Source in M'Range (1);
      --  Elementary row operation that subtracts Factor * M (Source, <>) from
      --  M (Target, <>)

      -------------
      -- Sub_Row --
      -------------

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
      is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

      --  Local declarations

      Max_Col : Integer := M'Last (2);

   --  Start of processing for Back_Substitute

   begin
      Do_Rows : for Row in reverse M'Range (1) loop

         pragma Loop_Invariant (Max_Col <= M'Last (2));

         Find_Non_Zero : for Col in reverse M'First (2) .. Max_Col loop
            if Is_Non_Zero (M (Row, Col)) then

               --  Found first non-zero element, so subtract a multiple of this
               --  element  from all higher rows, to reduce all other elements
               --  in this column to zero.

               declare
                  --  We can't use a for loop, as we'd need to iterate to
                  --  Row - 1, but that expression will overflow if M'First
                  --  equals Integer'First, which is true for aggregates
                  --  without explicit bounds..

                  J  : Integer := M'First (1);
                  NZ : constant Scalar := M (Row, Col);

               begin
                  while J < Row loop
                     pragma Loop_Invariant (J in M'Range (1));

                     Sub_Row (N, J, Row, (M (J, Col) / NZ));
                     Sub_Row (M, J, Row, (M (J, Col) / NZ));
                     J := J + 1;
                  end loop;
               end;

               --  Avoid potential overflow in the subtraction below

               exit Do_Rows when Col = M'First (2);

               Max_Col := Col - 1;

               exit Find_Non_Zero;
            end if;
         end loop Find_Non_Zero;
      end loop Do_Rows;
   end Back_Substitute;

   -----------------------
   -- Forward_Eliminate --
   -----------------------

   procedure Forward_Eliminate
     (M   : in out Matrix;
      N   : in out Matrix;
      Det : out Scalar)
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
      pragma Assert (M'First (1) = N'First (1)
                       and then
                     M'Last  (1) = N'Last (1));

      --  The following are variations of the elementary matrix row operations:
      --  row switching, row multiplication and row addition. Because in this
      --  algorithm the addition factor is always a negated value, we chose to
      --  use  row subtraction instead. Similarly, instead of multiplying by
      --  a reciprocal, we divide.

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
      with
        Pre => Target in M'Range (1)
          and then Source in M'Range (1);
      --  Subtrace Factor * M (Source, <>) from M (Target, <>)

      procedure Divide_Row
        (M, N  : in out Matrix;
         Row   : Integer;
         Scale : Scalar)
      with
        Pre => Row in M'Range (1)
          and then M'First (1) = N'First (1)
          and then M'Last (1) = N'Last (1)
          and then Scale /= Zero;
      --  Divide M (Row) and N (Row) by Scale, and update Det

      procedure Switch_Row
        (M, N  : in out Matrix;
         Row_1 : Integer;
         Row_2 : Integer)
      with
        Pre  => Row_1 in M'Range (1)
          and then Row_2 in M'Range (1)
          and then M'First (1) = N'First (1)
          and then M'Last (1) = N'Last (1),
        Post => (for all J in M'Range (2) =>
                   M (Row_1, J) = M'Old (Row_2, J)
                     and then M (Row_2, J) = M'Old (Row_1, J))
          and then (for all J in N'Range (2) =>
                      N (Row_1, J) = N'Old (Row_2, J)
                        and then N (Row_2, J) = N'Old (Row_1, J));
      --  Exchange M (Row_1) and N (Row_1) with M (Row_2) and N (Row_2),
      --  negating Det in the process.

      -------------
      -- Sub_Row --
      -------------

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
      is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

      ----------------
      -- Divide_Row --
      ----------------

      procedure Divide_Row
        (M, N  : in out Matrix;
         Row   : Integer;
         Scale : Scalar)
      is
      begin
         Det := Det * Scale;

         for J in M'Range (2) loop
            M (Row, J) := M (Row, J) / Scale;
         end loop;

         for J in N'Range (2) loop
            N (Row, J) := N (Row, J) / Scale;
            pragma Annotate
              (CodePeer, False_Positive, "divide by zero", "Scale /= 0");
         end loop;
      end Divide_Row;

      ----------------
      -- Switch_Row --
      ----------------

      procedure Switch_Row
        (M, N  : in out Matrix;
         Row_1 : Integer;
         Row_2 : Integer)
      is
         procedure Swap (X, Y : in out Scalar)
         with
           Post => X = Y'Old and then Y = X'Old;
         --  Exchange the values of X and Y

         ----------
         -- Swap --
         ----------

         procedure Swap (X, Y : in out Scalar) is
            T : constant Scalar := X;
         begin
            X := Y;
            Y := T;
         end Swap;

      --  Start of processing for Switch_Row

      begin
         if Row_1 /= Row_2 then
            Det := Zero - Det;

            for J in M'Range (2) loop
               Swap (M (Row_1, J), M (Row_2, J));
               pragma Annotate
                 (GNATprove, False_Positive,
                  "formal parameters ""X"" and ""Y"" might be aliased",
                  "Row_1 /= Row_2");

               pragma Loop_Invariant
                 (for all JJ in M'First (2) .. J =>
                    M (Row_1, JJ) = M'Loop_Entry (Row_2, JJ)
                      and then M (Row_2, JJ) = M'Loop_Entry (Row_1, JJ));
            end loop;

            for J in N'Range (2) loop
               Swap (N (Row_1, J), N (Row_2, J));
               pragma Annotate
                 (GNATprove, False_Positive,
                  "formal parameters ""X"" and ""Y"" might be aliased",
                  "Row_1 /= Row_2");

               pragma Loop_Invariant
                 (for all JJ in N'First (2) .. J =>
                    N (Row_1, JJ) = N'Loop_Entry (Row_2, JJ)
                      and then N (Row_2, JJ) = N'Loop_Entry (Row_1, JJ));
            end loop;
         end if;
      end Switch_Row;

      --  Local declarations

      Row : Integer := M'First (1);

   --  Start of processing for Forward_Eliminate

   begin
      Det := One;

      for J in M'Range (2) loop
         pragma Loop_Invariant (Row >= M'First (1));

         declare
            Max_Row : Integer := Row;
            Max_Abs : Real'Base := 0.0;

         begin
            --  Find best pivot in column J, starting in row Row

            for K in Row .. M'Last (1) loop
               pragma Loop_Invariant (Max_Row in M'Range (1));
               pragma Loop_Invariant
                 (if Max_Abs /= 0.0 then Max_Abs = abs M (Max_Row, J));

               declare
                  New_Abs : constant Real'Base := abs M (K, J);
               begin
                  if Max_Abs < New_Abs then
                     Max_Abs := New_Abs;
                     Max_Row := K;
                  end if;
               end;
            end loop;

            if Max_Abs > 0.0 then
               Switch_Row (M, N, Row, Max_Row);

               pragma Assert (Max_Abs = abs M (Row, J));

               --  The temporaries below are necessary to force a copy of the
               --  value and avoid improper aliasing.

               declare
                  Scale : constant Scalar := M (Row, J);
               begin
                  Divide_Row (M, N, Row, Scale);
               end;

               for U in Row .. M'Last (1) when U /= Row loop
                  declare
                     Factor : constant Scalar := M (U, J);
                  begin
                     Sub_Row (N, U, Row, Factor);
                     Sub_Row (M, U, Row, Factor);
                  end;
               end loop;

               exit when Row >= M'Last (1);

               Row := Row + 1;

            else
               --  Set zero (note that we do not have literals)

               Det := Zero;
            end if;
         end;
      end loop;
   end Forward_Eliminate;

   -------------------
   -- Inner_Product --
   -------------------

   function Inner_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return  Result_Scalar
   is
      R : Result_Scalar := Zero;

   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with
            "vectors are of different length in inner product";
      end if;

      for J in Left'Range loop
         R := R + Left (J) * Right (J - Left'First + Right'First);
      end loop;

      return R;
   end Inner_Product;

   -------------
   -- L2_Norm --
   -------------

   function L2_Norm (X : X_Vector) return Result_Real'Base is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
      Sum : Result_Real'Base := 0.0;

   begin
      for J in X'Range loop
         pragma Loop_Invariant (Sum >= 0.0);
         Sum := Sum + Result_Real'Base (abs X (J))**2;
         pragma Annotate
           (GNATprove, Intentional, "float overflow check might fail",
            "Intermediate computation might overflow in L2_Norm");
      end loop;

      return Sqrt (Sum);
   end L2_Norm;

   ----------------------------------
   -- Matrix_Elementwise_Operation --
   ----------------------------------

   function Matrix_Elementwise_Operation (X : X_Matrix) return Result_Matrix is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (X'Range (1), X'Range (2))
        with Relaxed_Initialization
      do
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) := Operation (X (J, K));

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Matrix_Elementwise_Operation;

   ----------------------------------
   -- Vector_Elementwise_Operation --
   ----------------------------------

   function Vector_Elementwise_Operation (X : X_Vector) return Result_Vector is
   begin
      return R : Result_Vector (X'Range) do
         for J in R'Range loop
            R (J) := Operation (X (J));
         end loop;
      end return;
   end Vector_Elementwise_Operation;

   -----------------------------------------
   -- Matrix_Matrix_Elementwise_Operation --
   -----------------------------------------

   function Matrix_Matrix_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (Left'Range (1), Left'Range (2))
        with Relaxed_Initialization
      do
         if Left'Length (1) /= Right'Length (1)
              or else
            Left'Length (2) /= Right'Length (2)
         then
            raise Constraint_Error with
              "matrices are of different dimension in elementwise operation";
         end if;

         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) :=
                 Operation
                   (Left (J, K),
                    Right
                      (J - R'First (1) + Right'First (1),
                       K - R'First (2) + Right'First (2)));

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Matrix_Matrix_Elementwise_Operation;

   ------------------------------------------------
   -- Matrix_Matrix_Scalar_Elementwise_Operation --
   ------------------------------------------------

   function Matrix_Matrix_Scalar_Elementwise_Operation
     (X : X_Matrix;
      Y : Y_Matrix;
      Z : Z_Scalar) return Result_Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (X'Range (1), X'Range (2))
        with Relaxed_Initialization
      do
         if X'Length (1) /= Y'Length (1)
              or else
            X'Length (2) /= Y'Length (2)
         then
            raise Constraint_Error with
              "matrices are of different dimension in elementwise operation";
         end if;

         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) :=
                 Operation
                   (X (J, K),
                    Y (J - R'First (1) + Y'First (1),
                       K - R'First (2) + Y'First (2)),
                    Z);

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Matrix_Matrix_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Vector_Vector_Elementwise_Operation --
   -----------------------------------------

   function Vector_Vector_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Vector
   is
   begin
      return R : Result_Vector (Left'Range) do
         if Left'Length /= Right'Length then
            raise Constraint_Error with
              "vectors are of different length in elementwise operation";
         end if;

         for J in R'Range loop
            R (J) := Operation (Left (J), Right (J - R'First + Right'First));
         end loop;
      end return;
   end Vector_Vector_Elementwise_Operation;

   ------------------------------------------------
   -- Vector_Vector_Scalar_Elementwise_Operation --
   ------------------------------------------------

   function Vector_Vector_Scalar_Elementwise_Operation
     (X : X_Vector;
      Y : Y_Vector;
      Z : Z_Scalar) return Result_Vector is
   begin
      return R : Result_Vector (X'Range) do
         if X'Length /= Y'Length then
            raise Constraint_Error with
              "vectors are of different length in elementwise operation";
         end if;

         for J in R'Range loop
            R (J) := Operation (X (J), Y (J - X'First + Y'First), Z);
         end loop;
      end return;
   end Vector_Vector_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Matrix_Scalar_Elementwise_Operation --
   -----------------------------------------

   function Matrix_Scalar_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Scalar) return Result_Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (Left'Range (1), Left'Range (2))
        with Relaxed_Initialization
      do
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) := Operation (Left (J, K), Right);

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Matrix_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Vector_Scalar_Elementwise_Operation --
   -----------------------------------------

   function Vector_Scalar_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Scalar) return Result_Vector
   is
   begin
      return R : Result_Vector (Left'Range) do
         for J in R'Range loop
            R (J) := Operation (Left (J), Right);
         end loop;
      end return;
   end Vector_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Scalar_Matrix_Elementwise_Operation --
   -----------------------------------------

   function Scalar_Matrix_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Matrix) return Result_Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (Right'Range (1), Right'Range (2))
        with Relaxed_Initialization
      do
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) := Operation (Left, Right (J, K));

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Scalar_Matrix_Elementwise_Operation;

   -----------------------------------------
   -- Scalar_Vector_Elementwise_Operation --
   -----------------------------------------

   function Scalar_Vector_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Vector) return Result_Vector
   is
   begin
      return R : Result_Vector (Right'Range) do
         for J in R'Range loop
            R (J) := Operation (Left, Right (J));
         end loop;
      end return;
   end Scalar_Vector_Elementwise_Operation;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Real'Base) return Real'Base
     with SPARK_Mode => Off  --  Not in SPARK due to use of Real'Exponent
   is
      Root, Next : Real'Base;

   begin
      --  Be defensive: any comparisons with NaN values will yield False.

      if not (X > 0.0) then
         if X = 0.0 then
            return X;
         else
            raise Argument_Error;
         end if;

      elsif X > Real'Base'Last then
         pragma Annotate
           (CodePeer, Intentional,
            "test always false", "test for infinity");

         --  X is infinity, which is its own square root

         return X;
      end if;

      --  Compute an initial estimate based on:

      --     X = M * R**E and Sqrt (X) = Sqrt (M) * R**(E / 2.0),

      --  where M is the mantissa, R is the radix and E the exponent.

      --  By ignoring the mantissa and ignoring the case of an odd
      --  exponent, we get a final error that is at most R. In other words,
      --  the result has about a single bit precision.

      Root := Real'Base (Real'Machine_Radix) ** (Real'Exponent (X) / 2);

      --  Because of the poor initial estimate, use the Babylonian method of
      --  computing the square root, as it is stable for all inputs. Every step
      --  will roughly double the precision of the result. Just a few steps
      --  suffice in most cases. Eight iterations should give about 2**8 bits
      --  of precision.

      for J in 1 .. 8 loop
         pragma Assert (Root /= 0.0);

         Next := (Root + X / Root) / 2.0;
         exit when Root = Next;
         Root := Next;
      end loop;

      return Root;
   end Sqrt;

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   function Matrix_Matrix_Product
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Result_Matrix (Left'Range (1), Right'Range (2))
        with Relaxed_Initialization
      do
         if Left'Length (2) /= Right'Length (1) then
            raise Constraint_Error with
              "incompatible dimensions in matrix multiplication";
         end if;

         for J in R'Range (1) loop
            for K in R'Range (2) loop
               declare
                  S : Result_Scalar := Zero;

               begin
                  for M in Left'Range (2) loop
                     S := S + Left (J, M) *
                                Right
                                  (M - Left'First (2) + Right'First (1), K);
                  end loop;

                  R (J, K) := S;
               end;

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Matrix_Matrix_Product;

   ----------------------------
   -- Matrix_Vector_Solution --
   ----------------------------

   function Matrix_Vector_Solution (A : Matrix; X : Vector) return Vector is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);

      procedure Ignore (M : Matrix)
      with
        Ghost,
        Depends => (null => M);

      procedure Ignore (M : Matrix) is null;
      --  Ghost procedure to document that the value of argument M is ignored,
      --  which prevents a warning being issued about the value not being used
      --  in the rest of the code.

      N   : constant Natural := A'Length (1);
      MA  : Matrix := A;
      MX  : Matrix (A'Range (1), 1 .. 1) with Relaxed_Initialization;
      R   : Vector (A'Range (2)) with Relaxed_Initialization;
      Det : Scalar;

   begin
      if A'Length (2) /= N then
         raise Constraint_Error with "matrix is not square";
      end if;

      if X'Length /= N then
         raise Constraint_Error with "incompatible vector length";
      end if;

      for J in 0 .. MX'Length (1) - 1 loop
         MX (MX'First (1) + J, 1) := X (X'First + J);

         pragma Loop_Invariant
           (for all JJ in MX'First (1) .. MX'First (1) + J =>
              MX (JJ, 1)'Initialized);
      end loop;

      Forward_Eliminate (MA, MX, Det);

      if Det = Zero then
         raise Constraint_Error with "matrix is singular";
         pragma Annotate
           (GNATprove, Intentional, "exception might be raised",
            "An exception should be raised on a singular matrix");
      end if;

      Back_Substitute (MA, MX);
      Ignore (MA);

      for J in 0 .. R'Length - 1 loop
         R (R'First + J) := MX (MX'First (1) + J, 1);

         pragma Loop_Invariant
           (for all JJ in R'First .. R'First + J => R (JJ)'Initialized);
      end loop;

      return R;
   end Matrix_Vector_Solution;

   ----------------------------
   -- Matrix_Matrix_Solution --
   ----------------------------

   function Matrix_Matrix_Solution (A, X : Matrix) return Matrix is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);

      procedure Ignore (M : Matrix)
      with
        Ghost,
        Depends => (null => M);

      procedure Ignore (M : Matrix) is null;
      --  Ghost procedure to document that the value of argument M is ignored,
      --  which prevents a warning being issued about the value not being used
      --  in the rest of the code.

      N   : constant Natural := A'Length (1);
      MA  : Matrix (A'Range (2), A'Range (2)) with Relaxed_Initialization;
      MB  : Matrix (A'Range (2), X'Range (2)) with Relaxed_Initialization;
      Det : Scalar;

   begin
      if A'Length (2) /= N then
         raise Constraint_Error with "matrix is not square";
      end if;

      if X'Length (1) /= N then
         raise Constraint_Error with "matrices have unequal number of rows";
      end if;

      for J in 0 .. A'Length (1) - 1 loop
         for K in MA'Range (2) loop
            MA (MA'First (1) + J, K) := A (A'First (1) + J, K);

            pragma Loop_Invariant
              (for all JJ in MA'First (1) .. MA'First (1) + J
                 when JJ /= MA'First (1) + J
               =>
                 (for all KK in MA'Range (2) =>
                    MA (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in MA'First (2) .. K =>
                 MA (MA'First (1) + J, KK)'Initialized);
         end loop;

         for K in MB'Range (2) loop
            MB (MB'First (1) + J, K) := X (X'First (1) + J, K);

            pragma Loop_Invariant
              (for all JJ in MB'First (1) .. MB'First (1) + J
                 when JJ /= MB'First (1) + J
               =>
                 (for all KK in MB'Range (2) =>
                    MB (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in MB'First (2) .. K =>
                 MB (MB'First (1) + J, KK)'Initialized);
         end loop;

         pragma Loop_Invariant
           (for all JJ in MA'First (1) .. MA'First (1) + J =>
              (for all KK in MA'Range (2) =>
                 MA (JJ, KK)'Initialized));
         pragma Loop_Invariant
           (for all JJ in MB'First (1) .. MB'First (1) + J =>
              (for all KK in MB'Range (2) =>
                 MB (JJ, KK)'Initialized));
      end loop;

      Forward_Eliminate (MA, MB, Det);

      if Det = Zero then
         raise Constraint_Error with "matrix is singular";
         pragma Annotate
           (GNATprove, Intentional, "exception might be raised",
            "An exception should be raised on a singular matrix");
      end if;

      Back_Substitute (MA, MB);
      Ignore (MA);

      return MB;
   end Matrix_Matrix_Solution;

   ---------------------------
   -- Matrix_Vector_Product --
   ---------------------------

   function Matrix_Vector_Product
     (Left  : Matrix;
      Right : Right_Vector) return Result_Vector
   is
   begin
      return R : Result_Vector (Left'Range (1)) do
         if Left'Length (2) /= Right'Length then
            raise Constraint_Error with
              "incompatible dimensions in matrix-vector multiplication";
         end if;

         for J in Left'Range (1) loop
            declare
               S : Result_Scalar := Zero;

            begin
               for K in Left'Range (2) loop
                  S := S + Left (J, K)
                         * Right (K - Left'First (2) + Right'First);
               end loop;

               R (J) := S;
            end;
         end loop;
      end return;
   end Matrix_Vector_Product;

   -------------------
   -- Outer_Product --
   -------------------

   function Outer_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Matrix
   is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      return R : Matrix (Left'Range, Right'Range)
        with Relaxed_Initialization
      do
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               R (J, K) := Left (J) * Right (K);

               pragma Loop_Invariant
                 (for all JJ in R'First (1) .. J when JJ /= J =>
                    (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
               pragma Loop_Invariant
                 (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                 (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      end return;
   end Outer_Product;

   -----------------
   -- Swap_Column --
   -----------------

   procedure Swap_Column (A : in out Matrix; Left, Right : Integer) is
      Temp : Scalar;
   begin
      for J in A'Range (1) loop
         Temp := A (J, Left);
         A (J, Left) := A (J, Right);
         A (J, Right) := Temp;
      end loop;
   end Swap_Column;

   ---------------
   -- Transpose --
   ---------------

   procedure Transpose (A : Matrix; R : out Matrix) is
      pragma Assertion_Policy (Pre            => Ignore,
                               Post           => Ignore,
                               Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);
   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := A (K - R'First (2) + A'First (1),
                           J - R'First (1) + A'First (2));

            pragma Loop_Invariant
              (for all JJ in R'First (1) .. J when JJ /= J =>
                (for all K in R'Range (2) => R (JJ, K)'Initialized));
            pragma Loop_Invariant
              (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
         end loop;

         pragma Loop_Invariant
           (for all JJ in R'First (1) .. J =>
             (for all K in R'Range (2) => R (JJ, K)'Initialized));
      end loop;
   end Transpose;

   -------------------------------
   -- Update_Matrix_With_Matrix --
   -------------------------------

   procedure Update_Matrix_With_Matrix (X : in out X_Matrix; Y : Y_Matrix) is
   begin
      if X'Length (1) /= Y'Length (1)
           or else
         X'Length (2) /= Y'Length (2)
      then
         raise Constraint_Error with
           "matrices are of different dimension in update operation";
      end if;

      for J in X'Range (1) loop
         for K in X'Range (2) loop
            Update (X (J, K), Y (J - X'First (1) + Y'First (1),
                                 K - X'First (2) + Y'First (2)));
         end loop;
      end loop;
   end Update_Matrix_With_Matrix;

   -------------------------------
   -- Update_Vector_With_Vector --
   -------------------------------

   procedure Update_Vector_With_Vector (X : in out X_Vector; Y : Y_Vector) is
   begin
      if X'Length /= Y'Length then
         raise Constraint_Error with
           "vectors are of different length in update operation";
      end if;

      for J in X'Range loop
         Update (X (J), Y (J - X'First + Y'First));
      end loop;
   end Update_Vector_With_Vector;

   -----------------
   -- Unit_Matrix --
   -----------------

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Matrix
   is
   begin
      return R : Matrix (First_1 .. Check_Unit_Last (First_1, Order, First_1),
                         First_2 .. Check_Unit_Last (First_2, Order, First_2))
      do
         R := [others => [others => Zero]];

         for J in 0 .. Order - 1 loop
            R (First_1 + J, First_2 + J) := One;
         end loop;
      end return;
   end Unit_Matrix;

   -----------------
   -- Unit_Vector --
   -----------------

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Vector
   is
   begin
      return R : Vector (First .. Check_Unit_Last (Index, Order, First)) do
         R := [others => Zero];
         R (Index) := One;
      end return;
   end Unit_Vector;

   ---------------------------
   -- Vector_Matrix_Product --
   ---------------------------

   function Vector_Matrix_Product
     (Left  : Left_Vector;
      Right : Matrix) return Result_Vector
   is
   begin
      return R : Result_Vector (Right'Range (2)) do
         if Left'Length /= Right'Length (1) then
            raise Constraint_Error with
              "incompatible dimensions in vector-matrix multiplication";
         end if;

         for J in Right'Range (2) loop
            declare
               S : Result_Scalar := Zero;

            begin
               for K in Right'Range (1) loop
                  S := S + Left (K - Right'First (1)
                                   + Left'First) * Right (K, J);
               end loop;

               R (J) := S;
            end;
         end loop;
      end return;
   end Vector_Matrix_Product;

end System.Generic_Array_Operations;
