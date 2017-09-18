------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.NUMERICS.GENERIC_REAL_ARRAYS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2017, Free Software Foundation, Inc.         --
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

--  This version of Generic_Real_Arrays avoids the use of BLAS and LAPACK. One
--  reason for this is new Ada 2012 requirements that prohibit algorithms such
--  as Strassen's algorithm, which may be used by some BLAS implementations. In
--  addition, some platforms lacked suitable compilers to compile the reference
--  BLAS/LAPACK implementation. Finally, on some platforms there are more
--  floating point types than supported by BLAS/LAPACK.

with Ada.Containers.Generic_Anonymous_Array_Sort; use Ada.Containers;

with System; use System;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body Ada.Numerics.Generic_Real_Arrays is

   package Ops renames System.Generic_Array_Operations;

   function Is_Non_Zero (X : Real'Base) return Boolean is (X /= 0.0);

   procedure Back_Substitute is new Ops.Back_Substitute
     (Scalar        => Real'Base,
      Matrix        => Real_Matrix,
      Is_Non_Zero   => Is_Non_Zero);

   function Diagonal is new Ops.Diagonal
     (Scalar       => Real'Base,
      Vector       => Real_Vector,
      Matrix       => Real_Matrix);

   procedure Forward_Eliminate is new Ops.Forward_Eliminate
    (Scalar        => Real'Base,
     Real          => Real'Base,
     Matrix        => Real_Matrix,
     Zero          => 0.0,
     One           => 1.0);

   procedure Swap_Column is new Ops.Swap_Column
    (Scalar        => Real'Base,
     Matrix        => Real_Matrix);

   procedure Transpose is new  Ops.Transpose
       (Scalar        => Real'Base,
        Matrix        => Real_Matrix);

   function Is_Symmetric (A : Real_Matrix) return Boolean is
     (Transpose (A) = A);
   --  Return True iff A is symmetric, see RM G.3.1 (90).

   function Is_Tiny (Value, Compared_To : Real) return Boolean is
     (abs Compared_To + 100.0 * abs (Value) = abs Compared_To);
   --  Return True iff the Value is much smaller in magnitude than the least
   --  significant digit of Compared_To.

   procedure Jacobi
     (A               : Real_Matrix;
      Values          : out Real_Vector;
      Vectors         : out Real_Matrix;
      Compute_Vectors : Boolean := True);
   --  Perform Jacobi's eigensystem algorithm on real symmetric matrix A

   function Length is new Square_Matrix_Length (Real'Base, Real_Matrix);
   --  Helper function that raises a Constraint_Error is the argument is
   --  not a square matrix, and otherwise returns its length.

   procedure Rotate (X, Y : in out Real; Sin, Tau : Real);
   --  Perform a Givens rotation

   procedure Sort_Eigensystem
     (Values  : in out Real_Vector;
      Vectors : in out Real_Matrix);
   --  Sort Values and associated Vectors by decreasing absolute value

   procedure Swap (Left, Right : in out Real);
   --  Exchange Left and Right

   function Sqrt is new Ops.Sqrt (Real);
   --  Instant a generic square root implementation here, in order to avoid
   --  instantiating a complete copy of Generic_Elementary_Functions.
   --  Speed of the square root is not a big concern here.

   ------------
   -- Rotate --
   ------------

   procedure Rotate (X, Y : in out Real; Sin, Tau : Real) is
      Old_X : constant Real := X;
      Old_Y : constant Real := Y;
   begin
      X := Old_X - Sin * (Old_Y + Old_X * Tau);
      Y := Old_Y + Sin * (Old_X - Old_Y * Tau);
   end Rotate;

   ----------
   -- Swap --
   ----------

   procedure Swap (Left, Right : in out Real) is
      Temp : constant Real := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;

   --  Instantiating the following subprograms directly would lead to
   --  name clashes, so use a local package.

   package Instantiations is

      function "+" is new
        Vector_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Vector      => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "+");

      function "+" is new
        Matrix_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Matrix      => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "+");

      function "+" is new
        Vector_Vector_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Right_Vector  => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "+");

      function "+" is new
        Matrix_Matrix_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Matrix   => Real_Matrix,
           Right_Matrix  => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "+");

      function "-" is new
        Vector_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Vector      => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "-");

      function "-" is new
        Matrix_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Matrix      => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "-");

      function "-" is new
        Vector_Vector_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Right_Vector  => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "-");

      function "-" is new
        Matrix_Matrix_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Matrix   => Real_Matrix,
           Right_Matrix  => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "-");

      function "*" is new
        Scalar_Vector_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Right_Vector  => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "*");

      function "*" is new
        Scalar_Matrix_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Right_Matrix  => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "*");

      function "*" is new
        Vector_Scalar_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "*");

      function "*" is new
        Matrix_Scalar_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Matrix   => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "*");

      function "*" is new
        Outer_Product
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Right_Vector  => Real_Vector,
           Matrix        => Real_Matrix);

      function "*" is new
        Inner_Product
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Right_Vector  => Real_Vector,
           Zero          => 0.0);

      function "*" is new
        Matrix_Vector_Product
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Matrix        => Real_Matrix,
           Right_Vector  => Real_Vector,
           Result_Vector => Real_Vector,
           Zero          => 0.0);

      function "*" is new
        Vector_Matrix_Product
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Matrix        => Real_Matrix,
           Result_Vector => Real_Vector,
           Zero          => 0.0);

      function "*" is new
        Matrix_Matrix_Product
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Matrix   => Real_Matrix,
           Right_Matrix  => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Zero          => 0.0);

      function "/" is new
        Vector_Scalar_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Vector   => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "/");

      function "/" is new
        Matrix_Scalar_Elementwise_Operation
          (Left_Scalar   => Real'Base,
           Right_Scalar  => Real'Base,
           Result_Scalar => Real'Base,
           Left_Matrix   => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "/");

      function "abs" is new
        L2_Norm
          (X_Scalar      => Real'Base,
           Result_Real   => Real'Base,
           X_Vector      => Real_Vector,
           "abs"         => "+");
      --  While the L2_Norm by definition uses the absolute values of the
      --  elements of X_Vector, for real values the subsequent squaring
      --  makes this unnecessary, so we substitute the "+" identity function
      --  instead.

      function "abs" is new
        Vector_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Vector      => Real_Vector,
           Result_Vector => Real_Vector,
           Operation     => "abs");

      function "abs" is new
        Matrix_Elementwise_Operation
          (X_Scalar      => Real'Base,
           Result_Scalar => Real'Base,
           X_Matrix      => Real_Matrix,
           Result_Matrix => Real_Matrix,
           Operation     => "abs");

      function Solve is new
        Matrix_Vector_Solution (Real'Base, 0.0, Real_Vector, Real_Matrix);

      function Solve is new
        Matrix_Matrix_Solution (Real'Base, 0.0, Real_Matrix);

      function Unit_Matrix is new
        Generic_Array_Operations.Unit_Matrix
          (Scalar        => Real'Base,
           Matrix        => Real_Matrix,
           Zero          => 0.0,
           One           => 1.0);

      function Unit_Vector is new
        Generic_Array_Operations.Unit_Vector
          (Scalar        => Real'Base,
           Vector        => Real_Vector,
           Zero          => 0.0,
           One           => 1.0);

   end Instantiations;

   ---------
   -- "+" --
   ---------

   function "+" (Right : Real_Vector) return Real_Vector
     renames Instantiations."+";

   function "+" (Right : Real_Matrix) return Real_Matrix
     renames Instantiations."+";

   function "+" (Left, Right : Real_Vector) return Real_Vector
     renames Instantiations."+";

   function "+" (Left, Right : Real_Matrix) return Real_Matrix
     renames Instantiations."+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Real_Vector) return Real_Vector
     renames Instantiations."-";

   function "-" (Right : Real_Matrix) return Real_Matrix
     renames Instantiations."-";

   function "-" (Left, Right : Real_Vector) return Real_Vector
     renames Instantiations."-";

   function "-" (Left, Right : Real_Matrix) return Real_Matrix
      renames Instantiations."-";

   ---------
   -- "*" --
   ---------

   --  Scalar multiplication

   function "*" (Left : Real'Base; Right : Real_Vector) return Real_Vector
     renames Instantiations."*";

   function "*" (Left : Real_Vector; Right : Real'Base) return Real_Vector
     renames Instantiations."*";

   function "*" (Left : Real'Base; Right : Real_Matrix) return Real_Matrix
     renames Instantiations."*";

   function "*" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix
     renames Instantiations."*";

   --  Vector multiplication

   function "*" (Left, Right : Real_Vector) return Real'Base
     renames Instantiations."*";

   function "*" (Left, Right : Real_Vector) return Real_Matrix
     renames Instantiations."*";

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector
     renames Instantiations."*";

   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector
     renames Instantiations."*";

   --  Matrix Multiplication

   function "*" (Left, Right : Real_Matrix) return Real_Matrix
     renames Instantiations."*";

   ---------
   -- "/" --
   ---------

   function "/" (Left : Real_Vector; Right : Real'Base) return Real_Vector
     renames Instantiations."/";

   function "/" (Left : Real_Matrix; Right : Real'Base) return Real_Matrix
     renames Instantiations."/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Real_Vector) return Real'Base
     renames Instantiations."abs";

   function "abs" (Right : Real_Vector) return Real_Vector
     renames Instantiations."abs";

   function "abs" (Right : Real_Matrix) return Real_Matrix
     renames Instantiations."abs";

   -----------------
   -- Determinant --
   -----------------

   function Determinant (A : Real_Matrix) return Real'Base is
      M : Real_Matrix := A;
      B : Real_Matrix (A'Range (1), 1 .. 0);
      R : Real'Base;
   begin
      Forward_Eliminate (M, B, R);
      return R;
   end Determinant;

   -----------------
   -- Eigensystem --
   -----------------

   procedure Eigensystem
     (A       : Real_Matrix;
      Values  : out Real_Vector;
      Vectors : out Real_Matrix)
   is
   begin
      Jacobi (A, Values, Vectors, Compute_Vectors => True);
      Sort_Eigensystem (Values, Vectors);
   end Eigensystem;

   -----------------
   -- Eigenvalues --
   -----------------

   function Eigenvalues (A : Real_Matrix) return Real_Vector is
   begin
      return Values : Real_Vector (A'Range (1)) do
         declare
            Vectors : Real_Matrix (1 .. 0, 1 .. 0);
         begin
            Jacobi (A, Values, Vectors, Compute_Vectors => False);
            Sort_Eigensystem (Values, Vectors);
         end;
      end return;
   end Eigenvalues;

   -------------
   -- Inverse --
   -------------

   function Inverse (A : Real_Matrix) return Real_Matrix is
     (Solve (A, Unit_Matrix (Length (A),
                             First_1 => A'First (2),
                             First_2 => A'First (1))));

   ------------
   -- Jacobi --
   ------------

   procedure Jacobi
     (A               : Real_Matrix;
      Values          : out Real_Vector;
      Vectors         : out Real_Matrix;
      Compute_Vectors : Boolean := True)
   is
      --  This subprogram uses Carl Gustav Jacob Jacobi's iterative method
      --  for computing eigenvalues and eigenvectors and is based on
      --  Rutishauser's implementation.

      --  The given real symmetric matrix is transformed iteratively to
      --  diagonal form through a sequence of appropriately chosen elementary
      --  orthogonal transformations, called Jacobi rotations here.

      --  The Jacobi method produces a systematic decrease of the sum of the
      --  squares of off-diagonal elements. Convergence to zero is quadratic,
      --  both for this implementation, as for the classic method that doesn't
      --  use row-wise scanning for pivot selection.

      --  The numerical stability and accuracy of Jacobi's method make it the
      --  best choice here, even though for large matrices other methods will
      --  be significantly more efficient in both time and space.

      --  While the eigensystem computations are absolutely foolproof for all
      --  real symmetric matrices, in presence of invalid values, or similar
      --  exceptional situations it might not. In such cases the results cannot
      --  be trusted and Constraint_Error is raised.

      --  Note: this implementation needs temporary storage for 2 * N + N**2
      --        values of type Real.

      Max_Iterations  : constant := 50;
      N               : constant Natural := Length (A);

      subtype Square_Matrix is Real_Matrix (1 .. N, 1 .. N);

      --  In order to annihilate the M (Row, Col) element, the
      --  rotation parameters Cos and Sin are computed as
      --  follows:

      --    Theta = Cot (2.0 * Phi)
      --          = (Diag (Col) - Diag (Row)) / (2.0 * M (Row, Col))

      --  Then Tan (Phi) as the smaller root (in modulus) of

      --    T**2 + 2 * T * Theta = 1 (or 0.5 / Theta, if Theta is large)

      function Compute_Tan (Theta : Real) return Real is
         (Real'Copy_Sign (1.0 / (abs Theta + Sqrt (1.0 + Theta**2)), Theta));

      function Compute_Tan (P, H : Real) return Real is
         (if Is_Tiny (P, Compared_To => H) then P / H
          else Compute_Tan (Theta => H / (2.0 * P)));

      function Sum_Strict_Upper (M : Square_Matrix) return Real;
      --  Return the sum of all elements in the strict upper triangle of M

      ----------------------
      -- Sum_Strict_Upper --
      ----------------------

      function Sum_Strict_Upper (M : Square_Matrix) return Real is
         Sum : Real := 0.0;

      begin
         for Row in 1 .. N - 1 loop
            for Col in Row + 1 .. N loop
               Sum := Sum + abs M (Row, Col);
            end loop;
         end loop;

         return Sum;
      end Sum_Strict_Upper;

      M         : Square_Matrix := A; --  Work space for solving eigensystem
      Threshold : Real;
      Sum       : Real;
      Diag      : Real_Vector (1 .. N);
      Diag_Adj  : Real_Vector (1 .. N);

      --  The vector Diag_Adj indicates the amount of change in each value,
      --  while Diag tracks the value itself and Values holds the values as
      --  they were at the beginning. As the changes typically will be small
      --  compared to the absolute value of Diag, at the end of each iteration
      --  Diag is computed as Diag + Diag_Adj thus avoiding accumulating
      --  rounding errors. This technique is due to Rutishauser.

   begin
      if Compute_Vectors
         and then (Vectors'Length (1) /= N or else Vectors'Length (2) /= N)
      then
         raise Constraint_Error with "incompatible matrix dimensions";

      elsif Values'Length /= N then
         raise Constraint_Error with "incompatible vector length";

      elsif not Is_Symmetric (M) then
         raise Constraint_Error with "matrix not symmetric";
      end if;

      --  Note: Only the locally declared matrix M and vectors (Diag, Diag_Adj)
      --        have lower bound equal to 1. The Vectors matrix may have
      --        different bounds, so take care indexing elements. Assignment
      --        as a whole is fine as sliding is automatic in that case.

      Vectors := (if not Compute_Vectors then (1 .. 0 => (1 .. 0 => 0.0))
                  else Unit_Matrix (Vectors'Length (1), Vectors'Length (2)));
      Values := Diagonal (M);

      Sweep : for Iteration in 1 .. Max_Iterations loop

         --  The first three iterations, perform rotation for any non-zero
         --  element. After this, rotate only for those that are not much
         --  smaller than the average off-diagnal element. After the fifth
         --  iteration, additionally zero out off-diagonal elements that are
         --  very small compared to elements on the diagonal with the same
         --  column or row index.

         Sum := Sum_Strict_Upper (M);

         exit Sweep when Sum = 0.0;

         Threshold := (if Iteration < 4 then 0.2 * Sum / Real (N**2) else 0.0);

         --  Iterate over all off-diagonal elements, rotating any that have
         --  an absolute value that exceeds the threshold.

         Diag := Values;
         Diag_Adj := (others => 0.0); -- Accumulates adjustments to Diag

         for Row in 1 .. N - 1 loop
            for Col in Row + 1 .. N loop

               --  If, before the rotation M (Row, Col) is tiny compared to
               --  Diag (Row) and Diag (Col), rotation is skipped. This is
               --  meaningful, as it produces no larger error than would be
               --  produced anyhow if the rotation had been performed.
               --  Suppress this optimization in the first four sweeps, so
               --  that this procedure can be used for computing eigenvectors
               --  of perturbed diagonal matrices.

               if Iteration > 4
                  and then Is_Tiny (M (Row, Col), Compared_To => Diag (Row))
                  and then Is_Tiny (M (Row, Col), Compared_To => Diag (Col))
               then
                  M (Row, Col) := 0.0;

               elsif abs M (Row, Col) > Threshold then
                  Perform_Rotation : declare
                     Tan : constant Real := Compute_Tan (M (Row, Col),
                                               Diag (Col) - Diag (Row));
                     Cos : constant Real := 1.0 / Sqrt (1.0 + Tan**2);
                     Sin : constant Real := Tan * Cos;
                     Tau : constant Real := Sin / (1.0 + Cos);
                     Adj : constant Real := Tan * M (Row, Col);

                  begin
                     Diag_Adj (Row) := Diag_Adj (Row) - Adj;
                     Diag_Adj (Col) := Diag_Adj (Col) + Adj;
                     Diag (Row) := Diag (Row) - Adj;
                     Diag (Col) := Diag (Col) + Adj;

                     M (Row, Col) := 0.0;

                     for J in 1 .. Row - 1 loop        --  1 <= J < Row
                        Rotate (M (J, Row), M (J, Col), Sin, Tau);
                     end loop;

                     for J in Row + 1 .. Col - 1 loop  --  Row < J < Col
                        Rotate (M (Row, J), M (J, Col), Sin, Tau);
                     end loop;

                     for J in Col + 1 .. N loop        --  Col < J <= N
                        Rotate (M (Row, J), M (Col, J), Sin, Tau);
                     end loop;

                     for J in Vectors'Range (1) loop
                        Rotate (Vectors (J, Row - 1 + Vectors'First (2)),
                                Vectors (J, Col - 1 + Vectors'First (2)),
                                Sin, Tau);
                     end loop;
                  end Perform_Rotation;
               end if;
            end loop;
         end loop;

         Values := Values + Diag_Adj;
      end loop Sweep;

      --  All normal matrices with valid values should converge perfectly.

      if Sum /= 0.0 then
         raise Constraint_Error with "eigensystem solution does not converge";
      end if;
   end Jacobi;

   -----------
   -- Solve --
   -----------

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector
      renames Instantiations.Solve;

   function Solve (A, X : Real_Matrix) return Real_Matrix
      renames Instantiations.Solve;

   ----------------------
   -- Sort_Eigensystem --
   ----------------------

   procedure Sort_Eigensystem
     (Values  : in out Real_Vector;
      Vectors : in out Real_Matrix)
   is
      procedure Swap (Left, Right : Integer);
      --  Swap Values (Left) with Values (Right), and also swap the
      --  corresponding eigenvectors. Note that lowerbounds may differ.

      function Less (Left, Right : Integer) return Boolean is
        (Values (Left) > Values (Right));
      --  Sort by decreasing eigenvalue, see RM G.3.1 (76).

      procedure Sort is new Generic_Anonymous_Array_Sort (Integer);
      --  Sorts eigenvalues and eigenvectors by decreasing value

      procedure Swap (Left, Right : Integer) is
      begin
         Swap (Values (Left), Values (Right));
         Swap_Column (Vectors, Left - Values'First + Vectors'First (2),
                               Right - Values'First + Vectors'First (2));
      end Swap;

   begin
      Sort (Values'First, Values'Last);
   end Sort_Eigensystem;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (X : Real_Matrix) return Real_Matrix is
   begin
      return R : Real_Matrix (X'Range (2), X'Range (1)) do
         Transpose (X, R);
      end return;
   end Transpose;

   -----------------
   -- Unit_Matrix --
   -----------------

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Real_Matrix
     renames Instantiations.Unit_Matrix;

   -----------------
   -- Unit_Vector --
   -----------------

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Real_Vector
     renames Instantiations.Unit_Vector;

end Ada.Numerics.Generic_Real_Arrays;
