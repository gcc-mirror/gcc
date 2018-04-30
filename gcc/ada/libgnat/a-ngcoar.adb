------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.GENERIC_COMPLEX_ARRAYS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2018, Free Software Foundation, Inc.       --
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

with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body Ada.Numerics.Generic_Complex_Arrays is

   --  Operations that are defined in terms of operations on the type Real,
   --  such as addition, subtraction and scaling, are computed in the canonical
   --  way looping over all elements.

   package Ops renames System.Generic_Array_Operations;

   subtype Real is Real_Arrays.Real;
   --  Work around visibility bug ???

   function Is_Non_Zero (X : Complex) return Boolean is (X /= (0.0, 0.0));
   --  Needed by Back_Substitute

   procedure Back_Substitute is new Ops.Back_Substitute
     (Scalar        => Complex,
      Matrix        => Complex_Matrix,
      Is_Non_Zero   => Is_Non_Zero);

   procedure Forward_Eliminate is new Ops.Forward_Eliminate
    (Scalar        => Complex,
     Real          => Real'Base,
     Matrix        => Complex_Matrix,
     Zero          => (0.0, 0.0),
     One           => (1.0, 0.0));

   procedure Transpose is new Ops.Transpose
                                (Scalar => Complex,
                                 Matrix => Complex_Matrix);

   --  Helper function that raises a Constraint_Error is the argument is
   --  not a square matrix, and otherwise returns its length.

   function Length is new Square_Matrix_Length (Complex, Complex_Matrix);

   --  Instant a generic square root implementation here, in order to avoid
   --  instantiating a complete copy of Generic_Elementary_Functions.
   --  Speed of the square root is not a big concern here.

   function Sqrt is new Ops.Sqrt (Real'Base);

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

      function "*" is new Inner_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
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

      function "*" is new Matrix_Vector_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Matrix        => Complex_Matrix,
                             Right_Vector  => Complex_Vector,
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

      function "*" is new Vector_Matrix_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Vector   => Complex_Vector,
                             Matrix        => Complex_Matrix,
                             Result_Vector => Complex_Vector,
                             Zero          => (0.0, 0.0));

      function "*" is new Matrix_Matrix_Product
                            (Left_Scalar   => Complex,
                             Right_Scalar  => Complex,
                             Result_Scalar => Complex,
                             Left_Matrix   => Complex_Matrix,
                             Right_Matrix  => Complex_Matrix,
                             Result_Matrix => Complex_Matrix,
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

      -----------
      -- "abs" --
      -----------

      function "abs" is new L2_Norm
                              (X_Scalar      => Complex,
                               Result_Real   => Real'Base,
                               X_Vector      => Complex_Vector);

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

      -----------
      -- Solve --
      -----------

      function Solve is new Matrix_Vector_Solution
        (Complex, (0.0, 0.0), Complex_Vector, Complex_Matrix);

      function Solve is new Matrix_Matrix_Solution
        (Complex, (0.0, 0.0), Complex_Matrix);

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
     renames Instantiations."*";

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
      Right : Complex_Matrix) return  Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Vector) return Complex_Matrix
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Matrix) return Complex_Vector
     renames Instantiations."*";

   function "*"
     (Left  : Complex_Matrix;
      Right : Complex_Vector) return Complex_Vector
     renames Instantiations."*";

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

   function "abs" (Right : Complex_Vector) return Real'Base
      renames Instantiations."abs";

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
      M : Complex_Matrix := A;
      B : Complex_Matrix (A'Range (1), 1 .. 0);
      R : Complex;
   begin
      Forward_Eliminate (M, B, R);
      return R;
   end Determinant;

   -----------------
   -- Eigensystem --
   -----------------

   procedure Eigensystem
     (A       : Complex_Matrix;
      Values  : out Real_Vector;
      Vectors : out Complex_Matrix)
   is
      N : constant Natural := Length (A);

      --  For a Hermitian matrix C, we convert the eigenvalue problem to a
      --  real symmetric one: if C = A + i * B, then the (N, N) complex
      --  eigenvalue problem:
      --     (A + i * B) * (u + i * v) = Lambda * (u + i * v)
      --
      --  is equivalent to the (2 * N, 2 * N) real eigenvalue problem:
      --     [  A, B ] [ u ] = Lambda * [ u ]
      --     [ -B, A ] [ v ]            [ v ]
      --
      --  Note that the (2 * N, 2 * N) matrix above is symmetric, as
      --  Transpose (A) = A and Transpose (B) = -B if C is Hermitian.

      --  We solve this eigensystem using the real-valued algorithms. The final
      --  result will have every eigenvalue twice, so in the sorted output we
      --  just pick every second value, with associated eigenvector u + i * v.

      M    : Real_Matrix (1 .. 2 * N, 1 .. 2 * N);
      Vals : Real_Vector (1 .. 2 * N);
      Vecs : Real_Matrix (1 .. 2 * N, 1 .. 2 * N);

   begin
      for J in 1 .. N loop
         for K in 1 .. N loop
            declare
               C : constant Complex :=
                 (A (A'First (1) + (J - 1), A'First (2) + (K - 1)));
            begin
               M (J, K) := Re (C);
               M (J + N, K + N) := Re (C);
               M (J + N, K) := Im (C);
               M (J, K + N) := -Im (C);
            end;
         end loop;
      end loop;

      Eigensystem (M, Vals, Vecs);

      for J in 1 .. N loop
         declare
            Col : constant Integer := Values'First + (J - 1);
         begin
            Values (Col) := Vals (2 * J);

            for K in 1 .. N loop
               declare
                  Row : constant Integer := Vectors'First (2) + (K - 1);
               begin
                  Vectors (Row, Col)
                     := (Vecs (J * 2, Col), Vecs (J * 2, Col + N));
               end;
            end loop;
         end;
      end loop;
   end Eigensystem;

   -----------------
   -- Eigenvalues --
   -----------------

   function Eigenvalues (A : Complex_Matrix) return Real_Vector is
      --  See Eigensystem for a description of the algorithm

      N : constant Natural := Length (A);
      R : Real_Vector (A'Range (1));

      M    : Real_Matrix (1 .. 2 * N, 1 .. 2 * N);
      Vals : Real_Vector (1 .. 2 * N);
   begin
      for J in 1 .. N loop
         for K in 1 .. N loop
            declare
               C : constant Complex :=
                 (A (A'First (1) + (J - 1), A'First (2) + (K - 1)));
            begin
               M (J, K) := Re (C);
               M (J + N, K + N) := Re (C);
               M (J + N, K) := Im (C);
               M (J, K + N) := -Im (C);
            end;
         end loop;
      end loop;

      Vals := Eigenvalues (M);

      for J in 1 .. N loop
         R (A'First (1) + (J - 1)) := Vals (2 * J);
      end loop;

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

   function Inverse (A : Complex_Matrix) return Complex_Matrix is
     (Solve (A, Unit_Matrix (Length (A),
                             First_1 => A'First (2),
                             First_2 => A'First (1))));

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

   function Solve
     (A : Complex_Matrix;
      X : Complex_Vector) return Complex_Vector
     renames Instantiations.Solve;

   function Solve
     (A : Complex_Matrix;
      X : Complex_Matrix) return Complex_Matrix
     renames Instantiations.Solve;

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
