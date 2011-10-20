------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.GENERIC_COMPLEX_ARRAYS                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays, Ada.Numerics.Generic_Complex_Types;

generic
   with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (<>);
   use Real_Arrays;
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
   use Complex_Types;
package Ada.Numerics.Generic_Complex_Arrays is
   pragma Pure (Generic_Complex_Arrays);

   --  Types

   type Complex_Vector is array (Integer range <>) of Complex;
   type Complex_Matrix is
     array (Integer range <>, Integer range <>) of Complex;

   --  Subprograms for Complex_Vector types
   --  Complex_Vector selection, conversion and composition operations

   function Re (X : Complex_Vector) return Real_Vector;
   function Im (X : Complex_Vector) return Real_Vector;

   procedure Set_Re (X : in out Complex_Vector; Re : Real_Vector);
   procedure Set_Im (X : in out Complex_Vector; Im : Real_Vector);

   function Compose_From_Cartesian
     (Re : Real_Vector) return Complex_Vector;
   function Compose_From_Cartesian
     (Re, Im : Real_Vector) return Complex_Vector;

   function Modulus (X : Complex_Vector) return Real_Vector;
   function "abs" (Right : Complex_Vector) return Real_Vector renames Modulus;
   function Argument (X : Complex_Vector) return Real_Vector;

   function Argument
     (X     : Complex_Vector;
      Cycle : Real'Base) return Real_Vector;

   function Compose_From_Polar
     (Modulus, Argument : Real_Vector) return Complex_Vector;

   function Compose_From_Polar
     (Modulus, Argument : Real_Vector;
      Cycle             : Real'Base) return Complex_Vector;

   --  Complex_Vector arithmetic operations

   function "+" (Right : Complex_Vector) return Complex_Vector;
   function "-" (Right : Complex_Vector) return Complex_Vector;
   function Conjugate (X : Complex_Vector) return Complex_Vector;
   function "+" (Left, Right : Complex_Vector) return Complex_Vector;
   function "-" (Left, Right : Complex_Vector) return Complex_Vector;
   function "*" (Left, Right : Complex_Vector) return Complex;
   function "abs" (Right : Complex_Vector) return Real'Base;

   --  Mixed Real_Vector and Complex_Vector arithmetic operations

   function "+"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Vector;

   function "+"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Vector;

   function "-"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Vector;

   function "-"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Vector;

   function "*" (Left : Real_Vector; Right : Complex_Vector) return Complex;
   function "*" (Left : Complex_Vector; Right : Real_Vector) return Complex;

   --  Complex_Vector scaling operations

   function "*"
     (Left  : Complex;
      Right : Complex_Vector) return Complex_Vector;

   function "*"
     (Left  : Complex_Vector;
      Right : Complex) return Complex_Vector;

   function "/"
     (Left  : Complex_Vector;
      Right : Complex) return Complex_Vector;

   function "*"
     (Left  : Real'Base;
      Right : Complex_Vector) return Complex_Vector;

   function "*"
     (Left  : Complex_Vector;
      Right : Real'Base) return Complex_Vector;

   function "/"
     (Left  : Complex_Vector;
      Right : Real'Base) return Complex_Vector;

   --  Other Complex_Vector operations

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Complex_Vector;

   --  Subprograms for Complex_Matrix types

   --  Complex_Matrix selection, conversion and composition operations

   function Re (X : Complex_Matrix) return Real_Matrix;
   function Im (X : Complex_Matrix) return Real_Matrix;

   procedure Set_Re (X : in out Complex_Matrix; Re : Real_Matrix);
   procedure Set_Im (X : in out Complex_Matrix; Im : Real_Matrix);

   function Compose_From_Cartesian (Re : Real_Matrix) return Complex_Matrix;

   function Compose_From_Cartesian
     (Re, Im : Real_Matrix) return  Complex_Matrix;

   function Modulus (X : Complex_Matrix) return Real_Matrix;
   function "abs" (Right : Complex_Matrix) return Real_Matrix renames Modulus;

   function Argument (X : Complex_Matrix) return Real_Matrix;

   function Argument
     (X     : Complex_Matrix;
      Cycle : Real'Base) return Real_Matrix;

   function Compose_From_Polar
     (Modulus, Argument : Real_Matrix) return Complex_Matrix;

   function Compose_From_Polar
     (Modulus  : Real_Matrix;
      Argument : Real_Matrix;
      Cycle    : Real'Base) return Complex_Matrix;

   --  Complex_Matrix arithmetic operations

   function "+" (Right : Complex_Matrix) return Complex_Matrix;
   function "-" (Right : Complex_Matrix) return Complex_Matrix;

   function Conjugate (X : Complex_Matrix) return Complex_Matrix;
   function Transpose (X : Complex_Matrix) return Complex_Matrix;

   function "+" (Left, Right : Complex_Matrix) return Complex_Matrix;
   function "-" (Left, Right : Complex_Matrix) return Complex_Matrix;
   function "*" (Left, Right : Complex_Matrix) return Complex_Matrix;
   function "*" (Left, Right : Complex_Vector) return Complex_Matrix;

   function "*"
     (Left  : Complex_Vector;
      Right : Complex_Matrix) return Complex_Vector;

   function "*"
     (Left  : Complex_Matrix;
      Right : Complex_Vector) return Complex_Vector;

   --  Mixed Real_Matrix and Complex_Matrix arithmetic operations

   function "+"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix;

   function "+"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix;

   function "-"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix;

   function "-"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix;

   function "*"
     (Left  : Real_Matrix;
      Right : Complex_Matrix) return Complex_Matrix;

   function "*"
     (Left  : Complex_Matrix;
      Right : Real_Matrix) return Complex_Matrix;

   function "*"
     (Left  : Real_Vector;
      Right : Complex_Vector) return Complex_Matrix;

   function "*"
     (Left  : Complex_Vector;
      Right : Real_Vector) return Complex_Matrix;

   function "*"
     (Left  : Real_Vector;
      Right : Complex_Matrix) return Complex_Vector;

   function "*"
     (Left  : Complex_Vector;
      Right : Real_Matrix) return Complex_Vector;

   function "*"
     (Left  : Real_Matrix;
      Right : Complex_Vector) return Complex_Vector;

   function "*"
     (Left  : Complex_Matrix;
      Right : Real_Vector) return Complex_Vector;

   --  Complex_Matrix scaling operations

   function "*"
     (Left  : Complex;
      Right : Complex_Matrix) return  Complex_Matrix;

   function "*"
     (Left  : Complex_Matrix;
      Right : Complex) return Complex_Matrix;

   function "/"
     (Left  : Complex_Matrix;
      Right : Complex) return Complex_Matrix;

   function "*"
     (Left  : Real'Base;
      Right : Complex_Matrix) return Complex_Matrix;

   function "*"
     (Left  : Complex_Matrix;
      Right : Real'Base) return Complex_Matrix;

   function "/"
     (Left  : Complex_Matrix;
      Right : Real'Base) return Complex_Matrix;

   --  Complex_Matrix inversion and related operations

   function Solve
     (A : Complex_Matrix;
      X : Complex_Vector) return Complex_Vector;

   function Solve (A, X : Complex_Matrix) return Complex_Matrix;

   function Inverse (A : Complex_Matrix) return Complex_Matrix;

   function Determinant (A : Complex_Matrix) return Complex;

   --  Eigenvalues and vectors of a Hermitian matrix

   function Eigenvalues (A : Complex_Matrix) return Real_Vector;

   procedure Eigensystem
     (A       : Complex_Matrix;
      Values  : out Real_Vector;
      Vectors : out Complex_Matrix);

   --  Other Complex_Matrix operations

   function Unit_Matrix
     (Order            : Positive;
      First_1, First_2 : Integer := 1) return Complex_Matrix;

end Ada.Numerics.Generic_Complex_Arrays;
