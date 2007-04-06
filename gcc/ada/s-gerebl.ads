------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         SYSTEM.GENERIC_REAL_BLAS                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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

generic
   type Real is digits <>;
   type Real_Vector is array (Integer range <>) of Real;
   type Real_Matrix is array (Integer range <>, Integer range <>) of Real;
package System.Generic_Real_BLAS is
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
      X     : Real_Vector;
      Inc_X : Integer := 1;
      Y     : Real_Vector;
      Inc_Y : Integer := 1) return Real;

   function nrm2
     (N     : Natural;
      X     : Real_Vector;
      Inc_X : Integer := 1) return Real;

   procedure gemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Real := 1.0;
      A     : Real_Matrix;
      Ld_A  : Positive;
      X     : Real_Vector;
      Inc_X : Integer := 1;  -- must be non-zero
      Beta  : Real := 0.0;
      Y     : in out Real_Vector;
      Inc_Y : Integer := 1); -- must be non-zero

   --  BLAS Level 3

   --  gemm   s, d, c, z  Matrix-matrix product of general matrices

   procedure gemm
     (Trans_A : access constant Character;
      Trans_B : access constant Character;
      M       : Positive;
      N       : Positive;
      K       : Positive;
      Alpha   : Real := 1.0;
      A       : Real_Matrix;
      Ld_A    : Integer;
      B       : Real_Matrix;
      Ld_B    : Integer;
      Beta    : Real := 0.0;
      C       : in out Real_Matrix;
      Ld_C    : Integer);

end System.Generic_Real_BLAS;
