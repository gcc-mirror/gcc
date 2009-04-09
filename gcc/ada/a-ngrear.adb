------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.NUMERICS.GENERIC_REAL_ARRAYS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2009, Free Software Foundation, Inc.         --
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

with System; use System;
with System.Generic_Real_BLAS;
with System.Generic_Real_LAPACK;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body Ada.Numerics.Generic_Real_Arrays is

   --  Operations involving inner products use BLAS library implementations.
   --  This allows larger matrices and vectors to be computed efficiently,
   --  taking into account memory hierarchy issues and vector instructions
   --  that vary widely between machines.

   --  Operations that are defined in terms of operations on the type Real,
   --  such as addition, subtraction and scaling, are computed in the canonical
   --  way looping over all elements.

   --  Operations for solving linear systems and computing determinant,
   --  eigenvalues, eigensystem and inverse, are implemented using the
   --  LAPACK library.

   package BLAS is
      new Generic_Real_BLAS (Real'Base, Real_Vector, Real_Matrix);

   package LAPACK is
      new Generic_Real_LAPACK (Real'Base, Real_Vector, Real_Matrix);

   use BLAS, LAPACK;

   --  Procedure versions of functions returning unconstrained values.
   --  This allows for inlining the function wrapper.

   procedure Eigenvalues (A : Real_Matrix; Values : out Real_Vector);
   procedure Inverse   (A : Real_Matrix; R : out Real_Matrix);
   procedure Solve     (A : Real_Matrix; X : Real_Vector; B : out Real_Vector);
   procedure Solve     (A : Real_Matrix; X : Real_Matrix; B : out Real_Matrix);

   procedure Transpose is new
     Generic_Array_Operations.Transpose
       (Scalar        => Real'Base,
        Matrix        => Real_Matrix);

   --  Helper function that raises a Constraint_Error is the argument is
   --  not a square matrix, and otherwise returns its length.

   function Length is new Square_Matrix_Length (Real'Base, Real_Matrix);

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

   function "*" (Left, Right : Real_Vector) return Real'Base is
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with
            "vectors are of different length in inner product";
      end if;

      return dot (Left'Length, X => Left, Y => Right);
   end "*";

   function "*" (Left, Right : Real_Vector) return Real_Matrix
      renames Instantiations."*";

   function "*"
     (Left : Real_Vector;
      Right : Real_Matrix) return Real_Vector
   is
      R : Real_Vector (Right'Range (2));

   begin
      if Left'Length /= Right'Length (1) then
         raise Constraint_Error with
           "incompatible dimensions in vector-matrix multiplication";
      end if;

      gemv (Trans => No_Trans'Access,
            M     => Right'Length (2),
            N     => Right'Length (1),
            A     => Right,
            Ld_A  => Right'Length (2),
            X     => Left,
            Y     => R);

      return R;
   end "*";

   function "*"
     (Left : Real_Matrix;
      Right : Real_Vector) return Real_Vector
   is
      R : Real_Vector (Left'Range (1));

   begin
      if Left'Length (2) /= Right'Length then
         raise Constraint_Error with
            "incompatible dimensions in matrix-vector multiplication";
      end if;

      gemv (Trans => Trans'Access,
            M     => Left'Length (2),
            N     => Left'Length (1),
            A     => Left,
            Ld_A  => Left'Length (2),
            X     => Right,
            Y     => R);

      return R;
   end "*";

   --  Matrix Multiplication

   function "*" (Left, Right : Real_Matrix) return Real_Matrix is
      R : Real_Matrix (Left'Range (1), Right'Range (2));

   begin
      if Left'Length (2) /= Right'Length (1) then
         raise Constraint_Error with
            "incompatible dimensions in matrix-matrix multiplication";
      end if;

      gemm (Trans_A => No_Trans'Access,
            Trans_B => No_Trans'Access,
            M       => Right'Length (2),
            N       => Left'Length (1),
            K       => Right'Length (1),
            A       => Right,
            Ld_A    => Right'Length (2),
            B       => Left,
            Ld_B    => Left'Length (2),
            C       => R,
            Ld_C    => R'Length (2));

      return R;
   end "*";

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

   function "abs" (Right : Real_Vector) return Real'Base is
   begin
      return nrm2 (Right'Length, Right);
   end "abs";

   function "abs" (Right : Real_Vector) return Real_Vector
      renames Instantiations."abs";

   function "abs" (Right : Real_Matrix) return Real_Matrix
      renames Instantiations."abs";

   -----------------
   -- Determinant --
   -----------------

   function Determinant (A : Real_Matrix) return Real'Base is
      N    : constant Integer := Length (A);
      LU   : Real_Matrix (1 .. N, 1 .. N) := A;
      Piv  : Integer_Vector (1 .. N);
      Info : aliased Integer := -1;
      Det  : Real := 1.0;

   begin
      getrf (M     => N,
             N     => N,
             A     => LU,
             Ld_A  => N,
             I_Piv => Piv,
             Info  => Info'Access);

      if Info /= 0 then
         raise Constraint_Error with "ill-conditioned matrix";
      end if;

      for J in 1 .. N loop
         if Piv (J) /= J then
            Det := -Det * LU (J, J);
         else
            Det := Det * LU (J, J);
         end if;
      end loop;

      return Det;
   end Determinant;

   -----------------
   -- Eigensystem --
   -----------------

   procedure Eigensystem
     (A       : Real_Matrix;
      Values  : out Real_Vector;
      Vectors : out Real_Matrix)
   is
      N      : constant Natural := Length (A);
      Tau    : Real_Vector (1 .. N);
      L_Work : Real_Vector (1 .. 1);
      Info   : aliased Integer;

      E : Real_Vector (1 .. N);
      pragma Warnings (Off, E);

   begin
      if Values'Length /= N then
         raise Constraint_Error with "wrong length for output vector";
      end if;

      if N = 0 then
         return;
      end if;

      --  Initialize working matrix and check for symmetric input matrix

      Transpose (A, Vectors);

      if A /= Vectors then
         raise Argument_Error with "matrix not symmetric";
      end if;

      --  Compute size of additional working space

      sytrd (Uplo   => Lower'Access,
             N      => N,
             A      => Vectors,
             Ld_A   => N,
             D      => Values,
             E      => E,
             Tau    => Tau,
             Work   => L_Work,
             L_Work => -1,
             Info   => Info'Access);

      declare
         Work : Real_Vector (1 .. Integer'Max (Integer (L_Work (1)), 2 * N));
         pragma Warnings (Off, Work);

         Comp_Z : aliased constant Character := 'V';

      begin
         --  Reduce matrix to tridiagonal form

         sytrd (Uplo   => Lower'Access,
                N      => N,
                A      => Vectors,
                Ld_A   => A'Length (1),
                D      => Values,
                E      => E,
                Tau    => Tau,
                Work   => Work,
                L_Work => Work'Length,
                Info   => Info'Access);

         if Info /= 0 then
            raise Program_Error;
         end if;

         --  Generate the real orthogonal matrix determined by sytrd

         orgtr (Uplo   => Lower'Access,
                N      => N,
                A      => Vectors,
                Ld_A   => N,
                Tau    => Tau,
                Work   => Work,
                L_Work => Work'Length,
                Info   => Info'Access);

         if Info /= 0 then
            raise Program_Error;
         end if;

         --  Compute all eigenvalues and eigenvectors using QR algorithm

         steqr (Comp_Z => Comp_Z'Access,
                N      => N,
                D      => Values,
                E      => E,
                Z      => Vectors,
                Ld_Z   => N,
                Work   => Work,
                Info   => Info'Access);

         if Info /= 0 then
            raise Constraint_Error with
               "eigensystem computation failed to converge";
         end if;
      end;
   end Eigensystem;

   -----------------
   -- Eigenvalues --
   -----------------

   procedure Eigenvalues
     (A      : Real_Matrix;
      Values : out Real_Vector)
   is
      N      : constant Natural := Length (A);
      L_Work : Real_Vector (1 .. 1);
      Info   : aliased Integer;

      B   : Real_Matrix (1 .. N, 1 .. N);
      Tau : Real_Vector (1 .. N);
      E   : Real_Vector (1 .. N);
      pragma Warnings (Off, B);
      pragma Warnings (Off, Tau);
      pragma Warnings (Off, E);

   begin
      if Values'Length /= N then
         raise Constraint_Error with "wrong length for output vector";
      end if;

      if N = 0 then
         return;
      end if;

      --  Initialize working matrix and check for symmetric input matrix

      Transpose (A, B);

      if A /= B then
         raise Argument_Error with "matrix not symmetric";
      end if;

      --  Find size of work area

      sytrd (Uplo   => Lower'Access,
             N      => N,
             A      => B,
             Ld_A   => N,
             D      => Values,
             E      => E,
             Tau    => Tau,
             Work   => L_Work,
             L_Work => -1,
             Info   => Info'Access);

      declare
         Work : Real_Vector (1 .. Integer'Min (Integer (L_Work (1)), 4 * N));
         pragma Warnings (Off, Work);

      begin
         --  Reduce matrix to tridiagonal form

         sytrd (Uplo   => Lower'Access,
                N      => N,
                A      => B,
                Ld_A   => A'Length (1),
                D      => Values,
                E      => E,
                Tau    => Tau,
                Work   => Work,
                L_Work => Work'Length,
                Info   => Info'Access);

         if Info /= 0 then
            raise Constraint_Error;
         end if;

         --  Compute all eigenvalues using QR algorithm

         sterf (N      => N,
                D      => Values,
                E      => E,
                Info   => Info'Access);

         if Info /= 0 then
            raise Constraint_Error with
               "eigenvalues computation failed to converge";
         end if;
      end;
   end Eigenvalues;

   function Eigenvalues (A : Real_Matrix) return Real_Vector is
      R : Real_Vector (A'Range (1));
   begin
      Eigenvalues (A, R);
      return R;
   end Eigenvalues;

   -------------
   -- Inverse --
   -------------

   procedure Inverse (A : Real_Matrix; R : out Real_Matrix) is
      N      : constant Integer := Length (A);
      Piv    : Integer_Vector (1 .. N);
      L_Work : Real_Vector (1 .. 1);
      Info   : aliased Integer := -1;

   begin
      --  All computations are done using column-major order, but this works
      --  out fine, because Transpose (Inverse (Transpose (A))) = Inverse (A).

      R := A;

      --  Compute LU decomposition

      getrf (M      => N,
             N      => N,
             A      => R,
             Ld_A   => N,
             I_Piv  => Piv,
             Info   => Info'Access);

      if Info /= 0 then
         raise Constraint_Error with "inverting singular matrix";
      end if;

      --  Determine size of work area

      getri (N      => N,
             A      => R,
             Ld_A   => N,
             I_Piv  => Piv,
             Work   => L_Work,
             L_Work => -1,
             Info   => Info'Access);

      if Info /= 0 then
         raise Constraint_Error;
      end if;

      declare
         Work : Real_Vector (1 .. Integer (L_Work (1)));
         pragma Warnings (Off, Work);

      begin
         --  Compute inverse from LU decomposition

         getri (N      => N,
                A      => R,
                Ld_A   => N,
                I_Piv  => Piv,
                Work   => Work,
                L_Work => Work'Length,
                Info   => Info'Access);

         if Info /= 0 then
            raise Constraint_Error with "inverting singular matrix";
         end if;

         --  ??? Should iterate with gerfs, based on implementation advice
      end;
   end Inverse;

   function Inverse (A : Real_Matrix) return Real_Matrix is
      R : Real_Matrix (A'Range (2), A'Range (1));
   begin
      Inverse (A, R);
      return R;
   end Inverse;

   -----------
   -- Solve --
   -----------

   procedure Solve (A : Real_Matrix; X : Real_Vector; B : out Real_Vector) is
   begin
      if Length (A) /= X'Length then
         raise Constraint_Error with
           "incompatible matrix and vector dimensions";
      end if;

      --  ??? Should solve directly, is faster and more accurate

      B := Inverse (A) * X;
   end Solve;

   procedure Solve (A : Real_Matrix; X : Real_Matrix; B : out Real_Matrix) is
   begin
      if Length (A) /= X'Length (1) then
         raise Constraint_Error with "incompatible matrix dimensions";
      end if;

      --  ??? Should solve directly, is faster and more accurate

      B := Inverse (A) * X;
   end Solve;

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector is
      B : Real_Vector (A'Range (2));
   begin
      Solve (A, X, B);
      return B;
   end Solve;

   function Solve (A, X : Real_Matrix) return Real_Matrix is
      B : Real_Matrix (A'Range (2), X'Range (2));
   begin
      Solve (A, X, B);
      return B;
   end Solve;

   ---------------
   -- Transpose --
   ---------------

   function Transpose (X : Real_Matrix) return Real_Matrix is
      R : Real_Matrix (X'Range (2), X'Range (1));
   begin
      Transpose (X, R);

      return R;
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
