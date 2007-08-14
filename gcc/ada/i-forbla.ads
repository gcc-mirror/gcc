------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         INTERFACES.FORTRAN.BLAS                          --
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

--  This package provides a thin binding to the standard Fortran BLAS library.
--  Documentation and a reference BLAS implementation is available from
--  ftp://ftp.netlib.org. The main purpose of this package is to facilitate
--  implementation of the Ada 2005 Ada.Numerics.Generic_Real_Arrays and
--  Ada.Numerics.Generic_Complex_Arrays packages. Bindings to other BLAS
--  routines may be added over time.

--  As actual linker arguments to link with the BLAS implementation differs
--  according to platform and chosen BLAS implementation, the linker arguments
--  are given in the body of this package. The body may need to be modified in
--  order to link with different BLAS implementations tuned to the specific
--  target.

package Interfaces.Fortran.BLAS is
   pragma Pure;
   pragma Elaborate_Body;

   No_Trans   : aliased constant Character := 'N';
   Trans      : aliased constant Character := 'T';
   Conj_Trans : aliased constant Character := 'C';

   --  Vector types

   type Real_Vector is array (Integer range <>) of Real;

   type Complex_Vector is array (Integer range <>) of Complex;

   type Double_Precision_Vector is array (Integer range <>)
     of Double_Precision;

   type Double_Complex_Vector is array (Integer range <>) of Double_Complex;

   --  Matrix types

   type Real_Matrix is array (Integer range <>, Integer range <>)
     of Real;

   type Double_Precision_Matrix is array (Integer range <>, Integer range <>)
     of Double_Precision;

   type Complex_Matrix is array (Integer range <>, Integer range <>)
     of Complex;

   type Double_Complex_Matrix is array (Integer range <>, Integer range <>)
     of Double_Complex;

   --  BLAS Level 1

   function sdot
     (N     : Positive;
      X     : Real_Vector;
      Inc_X : Integer := 1;
      Y     : Real_Vector;
      Inc_Y : Integer := 1) return Real;

   function ddot
     (N     : Positive;
      X     : Double_Precision_Vector;
      Inc_X : Integer := 1;
      Y     : Double_Precision_Vector;
      Inc_Y : Integer := 1) return Double_Precision;

   function cdotu
     (N     : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;
      Y     : Complex_Vector;
      Inc_Y : Integer := 1) return Complex;

   function zdotu
     (N     : Positive;
      X     : Double_Complex_Vector;
      Inc_X : Integer := 1;
      Y     : Double_Complex_Vector;
      Inc_Y : Integer := 1) return Double_Complex;

   function snrm2
     (N     : Natural;
      X     : Real_Vector;
      Inc_X : Integer := 1) return Real;

   function dnrm2
     (N     : Natural;
      X     : Double_Precision_Vector;
      Inc_X : Integer := 1) return Double_Precision;

   function scnrm2
     (N     : Natural;
      X     : Complex_Vector;
      Inc_X : Integer := 1) return Real;

   function dznrm2
     (N     : Natural;
      X     : Double_Complex_Vector;
      Inc_X : Integer := 1) return Double_Precision;

   --  BLAS Level 2

   procedure sgemv
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

   procedure dgemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Double_Precision := 1.0;
      A     : Double_Precision_Matrix;
      Ld_A  : Positive;
      X     : Double_Precision_Vector;
      Inc_X : Integer := 1;  -- must be non-zero
      Beta  : Double_Precision := 0.0;
      Y     : in out Double_Precision_Vector;
      Inc_Y : Integer := 1); -- must be non-zero

   procedure cgemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Complex := (1.0, 1.0);
      A     : Complex_Matrix;
      Ld_A  : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;  -- must be non-zero
      Beta  : Complex := (0.0, 0.0);
      Y     : in out Complex_Vector;
      Inc_Y : Integer := 1); -- must be non-zero

   procedure zgemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Double_Complex := (1.0, 1.0);
      A     : Double_Complex_Matrix;
      Ld_A  : Positive;
      X     : Double_Complex_Vector;
      Inc_X : Integer := 1;  -- must be non-zero
      Beta  : Double_Complex := (0.0, 0.0);
      Y     : in out Double_Complex_Vector;
      Inc_Y : Integer := 1); -- must be non-zero

   --  BLAS Level 3

   procedure sgemm
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

   procedure dgemm
     (Trans_A : access constant Character;
      Trans_B : access constant Character;
      M       : Positive;
      N       : Positive;
      K       : Positive;
      Alpha   : Double_Precision := 1.0;
      A       : Double_Precision_Matrix;
      Ld_A    : Integer;
      B       : Double_Precision_Matrix;
      Ld_B    : Integer;
      Beta    : Double_Precision := 0.0;
      C       : in out Double_Precision_Matrix;
      Ld_C    : Integer);

   procedure cgemm
     (Trans_A : access constant Character;
      Trans_B : access constant Character;
      M       : Positive;
      N       : Positive;
      K       : Positive;
      Alpha   : Complex := (1.0, 1.0);
      A       : Complex_Matrix;
      Ld_A    : Integer;
      B       : Complex_Matrix;
      Ld_B    : Integer;
      Beta    : Complex := (0.0, 0.0);
      C       : in out Complex_Matrix;
      Ld_C    : Integer);

   procedure zgemm
     (Trans_A : access constant Character;
      Trans_B : access constant Character;
      M       : Positive;
      N       : Positive;
      K       : Positive;
      Alpha   : Double_Complex := (1.0, 1.0);
      A       : Double_Complex_Matrix;
      Ld_A    : Integer;
      B       : Double_Complex_Matrix;
      Ld_B    : Integer;
      Beta    : Double_Complex := (0.0, 0.0);
      C       : in out Double_Complex_Matrix;
      Ld_C    : Integer);

private
   pragma Import (Fortran, cdotu,  "cdotu_");
   pragma Import (Fortran, cgemm,  "cgemm_");
   pragma Import (Fortran, cgemv,  "cgemv_");
   pragma Import (Fortran, ddot,   "ddot_");
   pragma Import (Fortran, dgemm,  "dgemm_");
   pragma Import (Fortran, dgemv,  "dgemv_");
   pragma Import (Fortran, dnrm2,  "dnrm2_");
   pragma Import (Fortran, dznrm2, "dznrm2_");
   pragma Import (Fortran, scnrm2, "scnrm2_");
   pragma Import (Fortran, sdot,   "sdot_");
   pragma Import (Fortran, sgemm,  "sgemm_");
   pragma Import (Fortran, sgemv,  "sgemv_");
   pragma Import (Fortran, snrm2,  "snrm2_");
   pragma Import (Fortran, zdotu,  "zdotu_");
   pragma Import (Fortran, zgemm,  "zgemm_");
   pragma Import (Fortran, zgemv,  "zgemv_");
end Interfaces.Fortran.BLAS;
