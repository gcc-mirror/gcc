------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.GENERIC_COMPLEX_ARRAYS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2007, Free Software Foundation, Inc.       --
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

with System.Generic_Array_Operations; use System.Generic_Array_Operations;
with System.Generic_Complex_BLAS;
with System.Generic_Complex_LAPACK;

package body Ada.Numerics.Generic_Complex_Arrays is

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

   type BLAS_Real_Vector is array (Integer range <>) of Real;

   package BLAS is new System.Generic_Complex_BLAS
     (Real           => Real,
      Complex_Types  => Complex_Types,
      Complex_Vector => Complex_Vector,
      Complex_Matrix => Complex_Matrix);

   package LAPACK is new System.Generic_Complex_LAPACK
     (Real           => Real,
      Real_Vector    => BLAS_Real_Vector,
      Complex_Types  => Complex_Types,
      Complex_Vector => Complex_Vector,
      Complex_Matrix => Complex_Matrix);

   subtype Real is Real_Arrays.Real;
   --  Work around visibility bug ???

   use BLAS, LAPACK;

   --  Procedure versions of functions returning unconstrained values.
   --  This allows for inlining the function wrapper.

   procedure Eigenvalues
     (A      : Complex_Matrix;
      Values : out Real_Vector);

   procedure Inverse
     (A      : Complex_Matrix;
      R      : out Complex_Matrix);

   procedure Solve
     (A      : Complex_Matrix;
      X      : Complex_Vector;
      B      : out Complex_Vector);

   procedure Solve
     (A      : Complex_Matrix;
      X      : Complex_Matrix;
      B      : out Complex_Matrix);

   procedure Transpose is new System.Generic_Array_Operations.Transpose
                                (Scalar => Complex,
                                 Matrix => Complex_Matrix);

   --  Helper function that raises a Constraint_Error is the argument is
   --  not a square matrix, and otherwise returns its length.

   function Length is new Square_Matrix_Length (Complex, Complex_Matrix);

   --  Instantiating the following subprograms directly would lead to
   --  name clashes, so use a local package.

   package Instantiations is

      ---------
      -- "*" --
      ---------

      function "*" is new Vector_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "*");

      function "*" is new Vector_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "*");

      function "*" is new Scalar_Vector_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "*");

      function "*" is new Scalar_Vector_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "*");

      function "*" is new Inner_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Real_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Inner_Product
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Outer_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Complex_Vector,
                             Matrix        => Complex_Matrix);

      function "*" is new Outer_Product
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Complex_Vector,
                             Matrix        => Complex_Matrix);

      function "*" is new Outer_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Real_Vector,
                             Matrix        => Complex_Matrix);

      function "*" is new Matrix_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "*");

      function "*" is new Matrix_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "*");

      function "*" is new Scalar_Matrix_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "*");

      function "*" is new Scalar_Matrix_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "*");

      function "*" is new Matrix_Vector_Product
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Matrix        => Real_Matrix,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Matrix_Vector_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Matrix        => Complex_Matrix,
                             Right_Vector  => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Vector_Matrix_Product
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Matrix        => Complex_Matrix,
                             Result_Vector => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Vector_Matrix_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Matrix        => Real_Matrix,
                             Result_Vector => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Matrix_Matrix_Product
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Real_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Zero          => (0.0, 0.0));

      function "*" is new Matrix_Matrix_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Zero          => (0.0, 0.0));

      ---------
      -- "+" --
      ---------

      function "+" is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "+");

      function "+" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "+");

      function "+" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "+");

      function "+" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "+");

      function "+" is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "+");

      function "+" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "+");

      function "+" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Real_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "+");

      function "+" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "+");

      ---------
      -- "-" --
      ---------

      function "-" is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "-");

      function "-" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "-");

      function "-" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "-");

      function "-" is new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Right_Vector  => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "-");

      function "-" is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "-");

      function "-" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "-");

      function "-" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Real_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "-");

      function "-" is new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "-");

      ---------
      -- "/" --
      ---------

      function "/" is new Vector_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "/");

      function "/" is new Vector_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => "/");

      function "/" is new Matrix_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "/");

      function "/" is new Matrix_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => "/");

      --------------
      -- Argument --
      --------------

      function Argument is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Real_Vector,
                             Operation     => Argument);

      function Argument is new Vector_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Real'Base,
                             Left_Vector   => Complex_Vector,
                             Result_Vector => Real_Vector,
                             Operation     => Argument);

      function Argument is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Real_Matrix,
                             Operation     => Argument);

      function Argument is new Matrix_Scalar_Elementwise_Operation
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Real'Base,
                             Left_Matrix   => Complex_Matrix,
                             Result_Matrix => Real_Matrix,
                             Operation     => Argument);

      ----------------------------
      -- Compose_From_Cartesian --
      ----------------------------

      function Compose_From_Cartesian is new Vector_Elementwise_Operation
                            (X_Scalar      => Real'Base,
                             Result_Scalar => Complex,
                             X_Vector      => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => Compose_From_Cartesian);

      function Compose_From_Cartesian is
         new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => Compose_From_Cartesian);

      function Compose_From_Cartesian is new Matrix_Elementwise_Operation
                            (X_Scalar      => Real'Base,
                             Result_Scalar => Complex,
                             X_Matrix      => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => Compose_From_Cartesian);

      function Compose_From_Cartesian is
         new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Real_Matrix,
                             Right_Matrix  => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => Compose_From_Cartesian);

      ------------------------
      -- Compose_From_Polar --
      ------------------------

      function Compose_From_Polar is
        new Vector_Vector_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Vector   => Real_Vector,
                             Right_Vector  => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => Compose_From_Polar);

      function Compose_From_Polar is
        new Vector_Vector_Scalar_Elementwise_Operation
                            (X_Scalar      => Real'Base,
                             Y_Scalar      => Real'Base,
                             Z_Scalar      => Real'Base,
                             Result_Scalar => Complex,
                             X_Vector      => Real_Vector,
                             Y_Vector      => Real_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => Compose_From_Polar);

      function Compose_From_Polar is
        new Matrix_Matrix_Elementwise_Operation
                            (Left_Scalar   => Real'Base,
                             Right_Scalar  => Real'Base,
                             Result_Scalar => Complex,
                             Left_Matrix   => Real_Matrix,
                             Right_Matrix  => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => Compose_From_Polar);

      function Compose_From_Polar is
        new Matrix_Matrix_Scalar_Elementwise_Operation
                            (X_Scalar      => Real'Base,
                             Y_Scalar      => Real'Base,
                             Z_Scalar      => Real'Base,
                             Result_Scalar => Complex,
                             X_Matrix      => Real_Matrix,
                             Y_Matrix      => Real_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => Compose_From_Polar);

      ---------------
      -- Conjugate --
      ---------------

      function Conjugate is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Complex_Vector,
                             Operation     => Conjugate);

      function Conjugate is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Complex,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
                             Operation     => Conjugate);

      --------
      -- Im --
      --------

      function Im is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Real_Vector,
                             Operation     => Im);

      function Im is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Real_Matrix,
                             Operation     => Im);

      -------------
      -- Modulus --
      -------------

      function Modulus is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Real_Vector,
                             Operation     => Modulus);

      function Modulus is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Real_Matrix,
                             Operation     => Modulus);

      --------
      -- Re --
      --------

      function Re is new Vector_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Vector      => Complex_Vector,
                             Result_Vector => Real_Vector,
                             Operation     => Re);

      function Re is new Matrix_Elementwise_Operation
                            (X_Scalar      => Complex,
                             Result_Scalar => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Result_Matrix => Real_Matrix,
                             Operation     => Re);

      ------------
      -- Set_Im --
      ------------

      procedure Set_Im is new Update_Vector_With_Vector
                            (X_Scalar      => Complex,
                             Y_Scalar      => Real'Base,
                             X_Vector      => Complex_Vector,
                             Y_Vector      => Real_Vector,
                             Update        => Set_Im);

      procedure Set_Im is new Update_Matrix_With_Matrix
                            (X_Scalar      => Complex,
                             Y_Scalar      => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Y_Matrix      => Real_Matrix,
                             Update        => Set_Im);

      ------------
      -- Set_Re --
      ------------

      procedure Set_Re is new Update_Vector_With_Vector
                            (X_Scalar      => Complex,
                             Y_Scalar      => Real'Base,
                             X_Vector      => Complex_Vector,
                             Y_Vector      => Real_Vector,
                             Update        => Set_Re);

      procedure Set_Re is new Update_Matrix_With_Matrix
                            (X_Scalar      => Complex,
                             Y_Scalar      => Real'Base,
                             X_Matrix      => Complex_Matrix,
                             Y_Matrix      => Real_Matrix,
                             Update        => Set_Re);

      -----------------
      -- Unit_Matrix --
      -----------------

      function Unit_Matrix is new System.Generic_Array_Operations.Unit_Matrix
                            (Scalar        => Complex,
                             Matrix        => Complex_Matrix,
                             Zero          => (0.0, 0.0),
                             One           => (1.0, 0.0));

      function Unit_Vector is new System.Generic_Array_Operations.Unit_Vector
                            (Scalar        => Complex,
                             Vector        => Complex_Vector,
                             Zero          => (0.0, 0.0),
                             One           => (1.0, 0.0));

   end Instantiations;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Vector) return Complex
   is
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with
            "vectors are of different length in inner product";
      end if;

      return dot (Left'Length, X => Left, Y => Right);
   end "*";

   function "*"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex
     renames Instantiations."*";

   function "*"
     (Left  : Complex;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Complex) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Real'Base;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Real'Base) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Complex_Matrix)
      return  Complex_Matrix
   is
      R : Complex_Matrix (Left'Range (1), Right'Range (2));

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

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Vector) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Matrix) return Complex_Vector
   is
      R : Complex_Vector (Right'Range (2));

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
     (Left  : Complex_Matrix;
      Right : Complex_Vector) return Complex_Vector
   is
      R : Complex_Vector (Left'Range (1));

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

   function "*"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Real_Vector;
      Right : Complex_Matrix) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Real_Matrix) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Real_Matrix;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Real_Vector) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Complex) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Real'Base;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Real'Base) return Complex_Matrix
     renames Instantiations."*";

   ---------
   -- "+" --
   ---------

   function "+" (Right : Complex_Vector) return Complex_Vector
     renames Instantiations."+";

   function "+"
     (Left  : Complex_Vector;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."+";

   function "+"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."+";

   function "+"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Vector
     renames Instantiations."+";

   function "+" (Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."+";

   function "+"
     (Left  : Complex_Matrix;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."+";

   function "+"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."+";

   function "+"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix
     renames Instantiations."+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Right : Complex_Vector) return Complex_Vector
     renames Instantiations."-";

   function "-"
     (Left  : Complex_Vector;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."-";

   function "-"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Vector
      renames Instantiations."-";

   function "-"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Vector
     renames Instantiations."-";

   function "-" (Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."-";

   function "-"
     (Left  : Complex_Matrix;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."-";

   function "-"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix
     renames Instantiations."-";

   function "-"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix
     renames Instantiations."-";

   ---------
   -- "/" --
   ---------

   function "/"
     (Left  : Complex_Vector;
      Right : Complex) return Complex_Vector
     renames Instantiations."/";

   function "/"
     (Left  : Complex_Vector;
      Right : Real'Base) return Complex_Vector
     renames Instantiations."/";

   function "/"
     (Left  : Complex_Matrix;
      Right : Complex) return Complex_Matrix
     renames Instantiations."/";

   function "/"
     (Left  : Complex_Matrix;
      Right : Real'Base) return Complex_Matrix
     renames Instantiations."/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Complex_Vector) return Complex is
   begin
      return (nrm2 (Right'Length, Right), 0.0);
   end "abs";

   --------------
   -- Argument --
   --------------

   function Argument (X : Complex_Vector) return Real_Vector
     renames Instantiations.Argument;

   function Argument
     (X     : Complex_Vector;
      Cycle : Real'Base) return Real_Vector
     renames Instantiations.Argument;

   function Argument (X : Complex_Matrix) return Real_Matrix
     renames Instantiations.Argument;

   function Argument
     (X     : Complex_Matrix;
      Cycle : Real'Base) return Real_Matrix
     renames Instantiations.Argument;

   ----------------------------
   -- Compose_From_Cartesian --
   ----------------------------

   function Compose_From_Cartesian (Re : Real_Vector) return Complex_Vector
     renames Instantiations.Compose_From_Cartesian;

   function Compose_From_Cartesian
     (Re : Real_Vector;
      Im : Real_Vector) return Complex_Vector
     renames Instantiations.Compose_From_Cartesian;

   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix
     renames Instantiations.Compose_From_Cartesian;

   function Compose_From_Cartesian
     (Re : Real_Matrix;
      Im : Real_Matrix) return Complex_Matrix
     renames Instantiations.Compose_From_Cartesian;

   ------------------------
   -- Compose_From_Polar --
   ------------------------

   function Compose_From_Polar
     (Modulus  : Real_Vector;
      Argument : Real_Vector) return Complex_Vector
     renames Instantiations.Compose_From_Polar;

   function Compose_From_Polar
     (Modulus  : Real_Vector;
      Argument : Real_Vector;
      Cycle    : Real'Base) return Complex_Vector
     renames Instantiations.Compose_From_Polar;

   function Compose_From_Polar
     (Modulus  : Real_Matrix;
      Argument : Real_Matrix) return Complex_Matrix
     renames Instantiations.Compose_From_Polar;

   function Compose_From_Polar
     (Modulus  : Real_Matrix;
      Argument : Real_Matrix;
      Cycle    : Real'Base) return Complex_Matrix
     renames Instantiations.Compose_From_Polar;

   ---------------
   -- Conjugate --
   ---------------

   function Conjugate (X : Complex_Vector) return Complex_Vector
     renames Instantiations.Conjugate;

   function Conjugate (X : Complex_Matrix) return Complex_Matrix
     renames Instantiations.Conjugate;

   -----------------
   -- Determinant --
   -----------------

   function Determinant (A : Complex_Matrix) return Complex is
      N    : constant Integer := Length (A);
      LU   : Complex_Matrix (1 .. N, 1 .. N) := A;
      Piv  : Integer_Vector (1 .. N);
      Info : aliased Integer := -1;
      Neg  : Boolean;
      Det  : Complex;

   begin
      if N = 0 then
         return (0.0, 0.0);
      end if;

      getrf (N, N, LU, N, Piv, Info'Access);

      if Info /= 0 then
         raise Constraint_Error with "ill-conditioned matrix";
      end if;

      Det := LU (1, 1);
      Neg := Piv (1) /= 1;

      for J in 2 .. N loop
         Det := Det * LU (J, J);
         Neg := Neg xor (Piv (J) /= J);
      end loop;

      if Neg then
         return -Det;

      else
         return Det;
      end if;
   end Determinant;

   -----------------
   -- Eigensystem --
   -----------------

   procedure Eigensystem
     (A       : Complex_Matrix;
      Values  : out Real_Vector;
      Vectors : out Complex_Matrix)
   is
      Job_Z    : aliased Character := 'V';
      Rng      : aliased Character := 'A';
      Uplo     : aliased Character := 'U';

      N        : constant Natural := Length (A);
      W        : BLAS_Real_Vector (Values'Range);
      M        : Integer;
      B        : Complex_Matrix (1 .. N, 1 .. N);
      L_Work   : Complex_Vector (1 .. 1);
      LR_Work  : BLAS_Real_Vector (1 .. 1);
      LI_Work  : Integer_Vector (1 .. 1);
      I_Supp_Z : Integer_Vector (1 .. 2 * N);
      Info     : aliased Integer;

   begin
      if Values'Length /= N then
         raise Constraint_Error with "wrong length for output vector";
      end if;

      if Vectors'First (1) /= A'First (1)
        or else Vectors'Last (1) /= A'Last (1)
        or else Vectors'First (2) /= A'First (2)
        or else Vectors'Last (2) /= A'Last (2)
      then
         raise Constraint_Error with "wrong dimensions for output matrix";
      end if;

      if N = 0 then
         return;
      end if;

      --  Check for hermitian matrix ???
      --  Copy only required triangle ???

      B := A;

      --  Find size of work area

      heevr
        (Job_Z'Access, Rng'Access, Uplo'Access, N, B, N,
         M        => M,
         W        => W,
         Z        => Vectors,
         Ld_Z     => N,
         I_Supp_Z => I_Supp_Z,
         Work     => L_Work,
         L_Work   => -1,
         R_Work   => LR_Work,
         LR_Work  => -1,
         I_Work   => LI_Work,
         LI_Work  => -1,
         Info     => Info'Access);

      if Info /= 0 then
         raise Constraint_Error;
      end if;

      declare
         Work   : Complex_Vector (1 .. Integer (L_Work (1).Re));
         R_Work : BLAS_Real_Vector (1 .. Integer (LR_Work (1)));
         I_Work : Integer_Vector (1 .. LI_Work (1));

      begin
         heevr
           (Job_Z'Access, Rng'Access, Uplo'Access, N, B, N,
            M        => M,
            W        => W,
            Z        => Vectors,
            Ld_Z     => N,
            I_Supp_Z => I_Supp_Z,
            Work     => Work,
            L_Work   => Work'Length,
            R_Work   => R_Work,
            LR_Work  => LR_Work'Length,
            I_Work   => I_Work,
            LI_Work  => LI_Work'Length,
            Info     => Info'Access);

         if Info /= 0 then
            raise Constraint_Error with "inverting non-Hermetian matrix";
         end if;

         for J in Values'Range loop
            Values (J) := W (J);
         end loop;
      end;
   end Eigensystem;

   -----------------
   -- Eigenvalues --
   -----------------

   procedure Eigenvalues
     (A      : Complex_Matrix;
      Values : out Real_Vector)
   is
      Job_Z    : aliased Character := 'N';
      Rng      : aliased Character := 'A';
      Uplo     : aliased Character := 'U';
      N        : constant Natural := Length (A);
      B        : Complex_Matrix (1 .. N, 1 .. N) := A;
      Z        : Complex_Matrix (1 .. 1, 1 .. 1);
      W        : BLAS_Real_Vector (Values'Range);
      L_Work   : Complex_Vector (1 .. 1);
      LR_Work  : BLAS_Real_Vector (1 .. 1);
      LI_Work  : Integer_Vector (1 .. 1);
      I_Supp_Z : Integer_Vector (1 .. 2 * N);
      M        : Integer;
      Info     : aliased Integer;

   begin
      if Values'Length /= N then
         raise Constraint_Error with "wrong length for output vector";
      end if;

      if N = 0 then
         return;
      end if;

      --  Check for hermitian matrix ???

      --  Find size of work area

      heevr (Job_Z'Access, Rng'Access, Uplo'Access, N, B, N,
             M        => M,
             W        => W,
             Z        => Z,
             Ld_Z     => 1,
             I_Supp_Z => I_Supp_Z,
             Work     => L_Work,  L_Work  => -1,
             R_Work   => LR_Work, LR_Work => -1,
             I_Work   => LI_Work, LI_Work => -1,
             Info     => Info'Access);

      if Info /= 0 then
         raise Constraint_Error;
      end if;

      declare
         Work : Complex_Vector (1 .. Integer (L_Work (1).Re));
         R_Work : BLAS_Real_Vector (1 .. Integer (LR_Work (1)));
         I_Work : Integer_Vector (1 .. LI_Work (1));
      begin
         heevr (Job_Z'Access, Rng'Access, Uplo'Access, N, B, N,
                M        => M,
                W        => W,
                Z        => Z,
                Ld_Z     => 1,
                I_Supp_Z => I_Supp_Z,
                Work     => Work,   L_Work  => Work'Length,
                R_Work   => R_Work, LR_Work => R_Work'Length,
                I_Work   => I_Work, LI_Work => I_Work'Length,
                Info     => Info'Access);

         if Info /= 0 then
            raise Constraint_Error with "inverting singular matrix";
         end if;

         for J in Values'Range loop
            Values (J) := W (J);
         end loop;
      end;
   end Eigenvalues;

   function Eigenvalues (A : Complex_Matrix) return Real_Vector is
      R : Real_Vector (A'Range (1));
   begin
      Eigenvalues (A, R);
      return R;
   end Eigenvalues;

   --------
   -- Im --
   --------

   function Im (X : Complex_Vector) return Real_Vector
     renames Instantiations.Im;

   function Im (X : Complex_Matrix) return Real_Matrix
     renames Instantiations.Im;

   -------------
   -- Inverse --
   -------------

   procedure Inverse (A : Complex_Matrix; R : out Complex_Matrix) is
      N      : constant Integer := Length (A);
      Piv    : Integer_Vector (1 .. N);
      L_Work : Complex_Vector (1 .. 1);
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
         Work : Complex_Vector (1 .. Integer (L_Work (1).Re));

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

   function Inverse (A : Complex_Matrix) return Complex_Matrix is
      R : Complex_Matrix (A'Range (2), A'Range (1));
   begin
      Inverse (A, R);
      return R;
   end Inverse;

   -------------
   -- Modulus --
   -------------

   function Modulus (X : Complex_Vector) return Real_Vector
     renames Instantiations.Modulus;

   function Modulus (X : Complex_Matrix) return Real_Matrix
     renames Instantiations.Modulus;

   --------
   -- Re --
   --------

   function Re (X : Complex_Vector) return Real_Vector
     renames Instantiations.Re;

   function Re (X : Complex_Matrix) return Real_Matrix
     renames Instantiations.Re;

   ------------
   -- Set_Im --
   ------------

   procedure Set_Im
     (X  : in out Complex_Matrix;
      Im : Real_Matrix)
     renames Instantiations.Set_Im;

   procedure Set_Im
     (X  : in out Complex_Vector;
      Im : Real_Vector)
     renames Instantiations.Set_Im;

   ------------
   -- Set_Re --
   ------------

   procedure Set_Re
     (X  : in out Complex_Matrix;
      Re : Real_Matrix)
     renames Instantiations.Set_Re;

   procedure Set_Re
     (X  : in out Complex_Vector;
      Re : Real_Vector)
     renames Instantiations.Set_Re;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (A : Complex_Matrix;
      X : Complex_Vector;
      B : out Complex_Vector)
   is
   begin
      if Length (A) /= X'Length then
         raise Constraint_Error with
           "incompatible matrix and vector dimensions";
      end if;

      --  ??? Should solve directly, is faster and more accurate

      B := Inverse (A) * X;
   end Solve;

   procedure Solve
     (A : Complex_Matrix;
      X : Complex_Matrix;
      B : out Complex_Matrix)
   is
   begin
      if Length (A) /= X'Length (1) then
         raise Constraint_Error with "incompatible matrix dimensions";
      end if;

      --  ??? Should solve directly, is faster and more accurate

      B := Inverse (A) * X;
   end Solve;

   function Solve
     (A : Complex_Matrix;
      X : Complex_Vector) return Complex_Vector
   is
      B : Complex_Vector (A'Range (2));
   begin
      Solve (A, X, B);
      return B;
   end Solve;

   function Solve (A, X : Complex_Matrix) return Complex_Matrix is
      B : Complex_Matrix (A'Range (2), X'Range (2));
   begin
      Solve (A, X, B);
      return B;
   end Solve;

   ---------------
   -- Transpose --
   ---------------

   function Transpose
     (X : Complex_Matrix) return Complex_Matrix
   is
      R : Complex_Matrix (X'Range (2), X'Range (1));
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
      First_2 : Integer := 1) return Complex_Matrix
     renames Instantiations.Unit_Matrix;

   -----------------
   -- Unit_Vector --
   -----------------

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Complex_Vector
     renames Instantiations.Unit_Vector;

end Ada.Numerics.Generic_Complex_Arrays;
