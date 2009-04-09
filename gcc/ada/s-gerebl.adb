------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . G E N E R I C _ R E A L _ B L A S              --
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

with Ada.Unchecked_Conversion;        use Ada;
with Interfaces;                      use Interfaces;
with Interfaces.Fortran;              use Interfaces.Fortran;
with Interfaces.Fortran.BLAS;         use Interfaces.Fortran.BLAS;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body System.Generic_Real_BLAS is

   Is_Single : constant Boolean :=
                 Real'Machine_Mantissa = Fortran.Real'Machine_Mantissa
                  and then Fortran.Real (Real'First) = Fortran.Real'First
                  and then Fortran.Real (Real'Last) = Fortran.Real'Last;

   Is_Double : constant Boolean :=
                 Real'Machine_Mantissa = Double_Precision'Machine_Mantissa
                  and then
                    Double_Precision (Real'First) = Double_Precision'First
                  and then
                    Double_Precision (Real'Last) = Double_Precision'Last;

   --  Local subprograms

   function To_Double_Precision (X : Real) return Double_Precision;
   pragma Inline_Always (To_Double_Precision);

   function To_Real (X : Double_Precision) return Real;
   pragma Inline_Always (To_Real);

   --  Instantiations

   function To_Double_Precision is new
     Vector_Elementwise_Operation
       (X_Scalar      => Real,
        Result_Scalar => Double_Precision,
        X_Vector      => Real_Vector,
        Result_Vector => Double_Precision_Vector,
        Operation     => To_Double_Precision);

   function To_Real is new
     Vector_Elementwise_Operation
       (X_Scalar      => Double_Precision,
        Result_Scalar => Real,
        X_Vector      => Double_Precision_Vector,
        Result_Vector => Real_Vector,
        Operation     => To_Real);

   function To_Double_Precision is new
     Matrix_Elementwise_Operation
       (X_Scalar      => Real,
        Result_Scalar => Double_Precision,
        X_Matrix      => Real_Matrix,
        Result_Matrix => Double_Precision_Matrix,
        Operation     => To_Double_Precision);

   function To_Real is new
     Matrix_Elementwise_Operation
       (X_Scalar      => Double_Precision,
        Result_Scalar => Real,
        X_Matrix      => Double_Precision_Matrix,
        Result_Matrix => Real_Matrix,
        Operation     => To_Real);

   function To_Double_Precision (X : Real) return Double_Precision is
   begin
      return Double_Precision (X);
   end To_Double_Precision;

   function To_Real (X : Double_Precision) return Real is
   begin
      return Real (X);
   end To_Real;

   ---------
   -- dot --
   ---------

   function dot
     (N     : Positive;
      X     : Real_Vector;
      Inc_X : Integer := 1;
      Y     : Real_Vector;
      Inc_Y : Integer := 1) return Real
   is
   begin
      if Is_Single then
         declare
            type X_Ptr is access all BLAS.Real_Vector (X'Range);
            type Y_Ptr is access all BLAS.Real_Vector (Y'Range);
            function Conv_X is new Unchecked_Conversion (Address, X_Ptr);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            return Real (sdot (N, Conv_X (X'Address).all, Inc_X,
                                  Conv_Y (Y'Address).all, Inc_Y));
         end;

      elsif Is_Double then
         declare
            type X_Ptr is access all BLAS.Double_Precision_Vector (X'Range);
            type Y_Ptr is access all BLAS.Double_Precision_Vector (Y'Range);
            function Conv_X is new Unchecked_Conversion (Address, X_Ptr);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            return Real (ddot (N, Conv_X (X'Address).all, Inc_X,
                                  Conv_Y (Y'Address).all, Inc_Y));
         end;

      else
         return Real (ddot (N, To_Double_Precision (X), Inc_X,
                               To_Double_Precision (Y), Inc_Y));
      end if;
   end dot;

   ----------
   -- gemm --
   ----------

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
      Ld_C    : Integer)
   is
   begin
      if Is_Single then
         declare
            subtype A_Type is BLAS.Real_Matrix (A'Range (1), A'Range (2));
            subtype B_Type is BLAS.Real_Matrix (B'Range (1), B'Range (2));
            type C_Ptr is
              access all BLAS.Real_Matrix (C'Range (1), C'Range (2));
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Real_Matrix, B_Type);
            function Conv_C is new Unchecked_Conversion (Address, C_Ptr);
         begin
            sgemm (Trans_A, Trans_B, M, N, K, Fortran.Real (Alpha),
                   Conv_A (A), Ld_A, Conv_B (B), Ld_B, Fortran.Real (Beta),
                   Conv_C (C'Address).all, Ld_C);
         end;

      elsif Is_Double then
         declare
            subtype A_Type is
               Double_Precision_Matrix (A'Range (1), A'Range (2));
            subtype B_Type is
               Double_Precision_Matrix (B'Range (1), B'Range (2));
            type C_Ptr is
              access all Double_Precision_Matrix (C'Range (1), C'Range (2));
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Real_Matrix, B_Type);
            function Conv_C is new Unchecked_Conversion (Address, C_Ptr);
         begin
            dgemm (Trans_A, Trans_B, M, N, K, Double_Precision (Alpha),
                   Conv_A (A), Ld_A, Conv_B (B), Ld_B, Double_Precision (Beta),
                   Conv_C (C'Address).all, Ld_C);
         end;

      else
         declare
            DP_C : Double_Precision_Matrix (C'Range (1), C'Range (2));
         begin
            if Beta /= 0.0 then
               DP_C := To_Double_Precision (C);
            end if;

            dgemm (Trans_A, Trans_B, M, N, K, Double_Precision (Alpha),
                   To_Double_Precision (A), Ld_A,
                   To_Double_Precision (B), Ld_B, Double_Precision (Beta),
                   DP_C, Ld_C);

            C := To_Real (DP_C);
         end;
      end if;
   end gemm;

   ----------
   -- gemv --
   ----------

   procedure gemv
     (Trans : access constant Character;
      M     : Natural := 0;
      N     : Natural := 0;
      Alpha : Real := 1.0;
      A     : Real_Matrix;
      Ld_A  : Positive;
      X     : Real_Vector;
      Inc_X : Integer := 1;
      Beta  : Real := 0.0;
      Y     : in out Real_Vector;
      Inc_Y : Integer := 1)
   is
   begin
      if Is_Single then
         declare
            subtype A_Type is BLAS.Real_Matrix (A'Range (1), A'Range (2));
            subtype X_Type is BLAS.Real_Vector (X'Range);
            type Y_Ptr is access all BLAS.Real_Vector (Y'Range);
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_X is new Unchecked_Conversion (Real_Vector, X_Type);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            sgemv (Trans, M, N, Fortran.Real (Alpha),
                   Conv_A (A), Ld_A, Conv_X (X), Inc_X, Fortran.Real (Beta),
                   Conv_Y (Y'Address).all, Inc_Y);
         end;

      elsif Is_Double then
         declare
            subtype A_Type is
               Double_Precision_Matrix (A'Range (1), A'Range (2));
            subtype X_Type is Double_Precision_Vector (X'Range);
            type Y_Ptr is access all Double_Precision_Vector (Y'Range);
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_X is new Unchecked_Conversion (Real_Vector, X_Type);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            dgemv (Trans, M, N, Double_Precision (Alpha),
                   Conv_A (A), Ld_A, Conv_X (X), Inc_X,
                   Double_Precision (Beta),
                   Conv_Y (Y'Address).all, Inc_Y);
         end;

      else
         declare
            DP_Y : Double_Precision_Vector (Y'Range);
         begin
            if Beta /= 0.0 then
               DP_Y := To_Double_Precision (Y);
            end if;

            dgemv (Trans, M, N, Double_Precision (Alpha),
                   To_Double_Precision (A), Ld_A,
                   To_Double_Precision (X), Inc_X, Double_Precision (Beta),
                   DP_Y, Inc_Y);

            Y := To_Real (DP_Y);
         end;
      end if;
   end gemv;

   ----------
   -- nrm2 --
   ----------

   function nrm2
     (N     : Natural;
      X     : Real_Vector;
      Inc_X : Integer := 1) return Real
   is
   begin
      if Is_Single then
         declare
            subtype X_Type is BLAS.Real_Vector (X'Range);
            function Conv_X is new Unchecked_Conversion (Real_Vector, X_Type);
         begin
            return Real (snrm2 (N, Conv_X (X), Inc_X));
         end;

      elsif Is_Double then
         declare
            subtype X_Type is Double_Precision_Vector (X'Range);
            function Conv_X is new Unchecked_Conversion (Real_Vector, X_Type);
         begin
            return Real (dnrm2 (N, Conv_X (X), Inc_X));
         end;

      else
         return Real (dnrm2 (N, To_Double_Precision (X), Inc_X));
      end if;
   end nrm2;

end System.Generic_Real_BLAS;
