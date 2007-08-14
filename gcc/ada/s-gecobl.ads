------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       SYSTEM.GENERIC_COMPLEX_BLAS                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2007, Free Software Foundation, Inc.          --
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

--  Package comment required ???

with Ada.Numerics.Generic_Complex_Types;

generic
   type Real is digits <>;
   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
   use Complex_Types;

   type Complex_Vector is array (Integer range <>) of Complex;
   type Complex_Matrix is array (Integer range <>, Integer range <>)
      of Complex;
package System.Generic_Complex_BLAS is
   pragma Pure;

   --  Although BLAS support is only available for IEEE single and double
   --  compatible floating-point types, this unit will accept any type
   --  and apply conversions as necessary, with possible loss of
   --  precision and range.

   No_Trans   : aliased constant Character := 'N';
   Trans      : aliased constant Character := 'T';
   Conj_Trans : aliased constant Character := 'C';

   --  BLAS Level 1 Subprograms and Types

   function dot
     (N     : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;
      Y     : Complex_Vector;
      Inc_Y : Integer := 1) return Complex;

   function nrm2
     (N     : Natural;
      X     : Complex_Vector;
      Inc_X : Integer := 1) return Real;

   procedure gemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Complex := (1.0, 0.0);
      A     : Complex_Matrix;
      Ld_A  : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;  -- must be non-zero
      Beta  : Complex := (0.0, 0.0);
      Y     : in out Complex_Vector;
      Inc_Y : Integer := 1); -- must be non-zero

   --  BLAS Level 3

   --  gemm   s, d, c, z  Matrix-matrix product of general matrices

   procedure gemm
     (Trans_A : access constant Character;
      Trans_B : access constant Character;
      M       : Positive;
      N       : Positive;
      K       : Positive;
      Alpha   : Complex := (1.0, 0.0);
      A       : Complex_Matrix;
      Ld_A    : Integer;
      B       : Complex_Matrix;
      Ld_B    : Integer;
      Beta    : Complex := (0.0, 0.0);
      C       : in out Complex_Matrix;
      Ld_C    : Integer);

end System.Generic_Complex_BLAS;
