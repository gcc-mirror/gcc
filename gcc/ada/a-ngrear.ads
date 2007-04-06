------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     ADA.NUMERICS.GENERIC_REAL_ARRAYS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

generic
   type Real is digits <>;
package Ada.Numerics.Generic_Real_Arrays is
   pragma Pure (Generic_Real_Arrays);

   --  Types

   type Real_Vector is array (Integer range <>) of Real'Base;
   type Real_Matrix is array (Integer range <>, Integer range <>) of Real'Base;

   --  Subprograms for Real_Vector types

   --  Real_Vector arithmetic operations

   function "+"   (Right : Real_Vector)       return Real_Vector;
   function "-"   (Right : Real_Vector)       return Real_Vector;
   function "abs" (Right : Real_Vector)       return Real_Vector;

   function "+"   (Left, Right : Real_Vector) return Real_Vector;
   function "-"   (Left, Right : Real_Vector) return Real_Vector;

   function "*"   (Left, Right : Real_Vector) return Real'Base;

   function "abs" (Right : Real_Vector)       return Real'Base;

   --  Real_Vector scaling operations

   function "*" (Left : Real'Base;   Right : Real_Vector) return Real_Vector;
   function "*" (Left : Real_Vector; Right : Real'Base)   return Real_Vector;
   function "/" (Left : Real_Vector; Right : Real'Base)   return Real_Vector;

   --  Other Real_Vector operations

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Real_Vector;

   --  Subprograms for Real_Matrix types

   --  Real_Matrix arithmetic operations

   function "+"       (Right : Real_Matrix) return Real_Matrix;
   function "-"       (Right : Real_Matrix) return Real_Matrix;
   function "abs"     (Right : Real_Matrix) return Real_Matrix;
   function Transpose (X     : Real_Matrix) return Real_Matrix;

   function "+" (Left, Right : Real_Matrix) return Real_Matrix;
   function "-" (Left, Right : Real_Matrix) return Real_Matrix;
   function "*" (Left, Right : Real_Matrix) return Real_Matrix;

   function "*" (Left, Right : Real_Vector) return Real_Matrix;

   function "*" (Left : Real_Vector; Right : Real_Matrix) return Real_Vector;
   function "*" (Left : Real_Matrix; Right : Real_Vector) return Real_Vector;

   --  Real_Matrix scaling operations

   function "*" (Left : Real'Base;   Right : Real_Matrix) return Real_Matrix;
   function "*" (Left : Real_Matrix; Right : Real'Base)   return Real_Matrix;
   function "/" (Left : Real_Matrix; Right : Real'Base)   return Real_Matrix;

   --  Real_Matrix inversion and related operations

   function Solve (A : Real_Matrix; X : Real_Vector) return Real_Vector;
   function Solve (A, X : Real_Matrix) return Real_Matrix;
   function Inverse (A : Real_Matrix) return Real_Matrix;
   function Determinant (A : Real_Matrix) return Real'Base;

   --  Eigenvalues and vectors of a real symmetric matrix

   function Eigenvalues (A : Real_Matrix) return Real_Vector;

   procedure Eigensystem
     (A       : Real_Matrix;
      Values  : out Real_Vector;
      Vectors : out Real_Matrix);

   --  Other Real_Matrix operations

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Real_Matrix;

private
   --  The following operations are either relatively simple compared to the
   --  expense of returning unconstrained arrays, or are just function wrappers
   --  calling procedures implementing the actual operation. By having the
   --  front end always inline these, the expense of the unconstrained returns
   --  can be avoided.

   pragma Inline_Always ("+");
   pragma Inline_Always ("-");
   pragma Inline_Always ("*");
   pragma Inline_Always ("/");
   pragma Inline_Always ("abs");
   pragma Inline_Always (Eigenvalues);
   pragma Inline_Always (Inverse);
   pragma Inline_Always (Solve);
   pragma Inline_Always (Transpose);
   pragma Inline_Always (Unit_Matrix);
   pragma Inline_Always (Unit_Vector);
end Ada.Numerics.Generic_Real_Arrays;
