------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--            ADA.NUMERICS.GENERIC_COMPLEX_ELEMENTARY_FUNCTIONS             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (<>);
   use Complex_Types;

package Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Pure (Ada.Numerics.Generic_Complex_Elementary_Functions);

   function Sqrt (X : Complex)   return Complex;

   function Log  (X : Complex)   return Complex;

   function Exp  (X : Complex)   return Complex;
   function Exp  (X : Imaginary) return Complex;

   function "**" (Left : Complex;   Right : Complex)   return Complex;
   function "**" (Left : Complex;   Right : Real'Base) return Complex;
   function "**" (Left : Real'Base; Right : Complex)   return Complex;

   function Sin (X : Complex) return Complex;
   function Cos (X : Complex) return Complex;
   function Tan (X : Complex) return Complex;
   function Cot (X : Complex) return Complex;

   function Arcsin (X : Complex) return Complex;
   function Arccos (X : Complex) return Complex;
   function Arctan (X : Complex) return Complex;
   function Arccot (X : Complex) return Complex;

   function Sinh (X : Complex) return Complex;
   function Cosh (X : Complex) return Complex;
   function Tanh (X : Complex) return Complex;
   function Coth (X : Complex) return Complex;

   function Arcsinh (X : Complex) return Complex;
   function Arccosh (X : Complex) return Complex;
   function Arctanh (X : Complex) return Complex;
   function Arccoth (X : Complex) return Complex;

end Ada.Numerics.Generic_Complex_Elementary_Functions;
