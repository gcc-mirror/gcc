-- FC70A00.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- FOUNDATION DESCRIPTION:
--      This file simulates a generic complex integer support package, to be
--      used for tests covering generic formal packages.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

generic               -- Complex integer abstraction.
   type Int_Type is range <>;
package FC70A00 is

   -- Simulate a generic complex integer support package. Complex integers
   -- are treated as coordinates in the Cartesian plane.


   type Complex_Type is private;

   Zero : constant Complex_Type;                      -- (0,0).
   One  : constant Complex_Type;                      -- (1,0).


   function "-" (Right : Complex_Type)                -- Invert a complex
     return Complex_Type;                             -- integer.

   function "+" (Left, Right : Complex_Type)          -- Add two complex
     return Complex_Type;                             -- integers.

   function "*" (Left, Right : Complex_Type)          -- Multiply two complex
     return Complex_Type;                             -- integers.

   function Reciprocal (Right : Complex_Type)         -- Return the reciprocal
     return Complex_Type;                             -- of a complex integer.

   function Complex (Real, Imag : Int_Type)           -- Create a complex
     return Complex_Type;                             -- integer.

private
   
   type Complex_Type is record
      Real : Int_Type;
      Imag : Int_Type;
   end record;

   Zero : constant Complex_Type := (Real => 0, Imag => 0);
   One  : constant Complex_Type := (Real => 1, Imag => 0);

end FC70A00;


     --==================================================================--


package body FC70A00 is  -- Complex integer abstraction.

   function Complex (Real, Imag : Int_Type) return Complex_Type is
   begin
      return ( (Real, Imag) );
   end Complex;

               --==============================================--

   function "-" (Right : Complex_Type) return Complex_Type is
   begin
      return ( (-Right.Real, -Right.Imag) );
   end "-";

               --==============================================--

   function "+" (Left, Right : Complex_Type) return Complex_Type is
   begin
      return ( (Left.Real + Right.Real, Left.Imag + Right.Imag) );
   end "+";

               --==============================================--

   function "*" (Left, Right : Complex_Type) return Complex_Type is
   begin
      return ( (Real => (Left.Real * Right.Real) - (Left.Imag * Right.Imag),
                Imag => (Left.Imag * Right.Real) + (Left.Real * Right.Imag)) );
   end "*";

               --==============================================--

   function Reciprocal (Right : Complex_Type) return Complex_Type is
      Denominator : Int_Type := Right.Real**2 + Right.Imag**2;
   begin  -- NOTE: Results are truncated.
      return ( (Right.Real/Denominator, -Right.Imag/Denominator) );
   end Reciprocal;

end FC70A00;
