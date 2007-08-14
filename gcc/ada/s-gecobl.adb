------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       SYSTEM.GENERIC_COMPLEX_BLAS                        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;        use Ada;
with Interfaces;                      use Interfaces;
with Interfaces.Fortran;              use Interfaces.Fortran;
with Interfaces.Fortran.BLAS;         use Interfaces.Fortran.BLAS;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body System.Generic_Complex_BLAS is

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

   subtype Complex is Complex_Types.Complex;

   --  Local subprograms

   function To_Double_Precision (X : Real) return Double_Precision;
   pragma Inline (To_Double_Precision);

   function To_Double_Complex (X : Complex) return Double_Complex;
   pragma Inline (To_Double_Complex);

   function To_Complex (X : Double_Complex) return Complex;
   function To_Complex (X : Fortran.Complex) return Complex;
   pragma Inline (To_Complex);

   function To_Fortran (X : Complex) return Fortran.Complex;
   pragma Inline (To_Fortran);

   --  Instantiations

   function To_Double_Complex is new
      Vector_Elementwise_Operation
       (X_Scalar      => Complex_Types.Complex,
        Result_Scalar => Fortran.Double_Complex,
        X_Vector      => Complex_Vector,
        Result_Vector => BLAS.Double_Complex_Vector,
        Operation     => To_Double_Complex);

   function To_Complex is new
      Vector_Elementwise_Operation
       (X_Scalar      => Fortran.Double_Complex,
        Result_Scalar => Complex,
        X_Vector      => BLAS.Double_Complex_Vector,
        Result_Vector => Complex_Vector,
        Operation     => To_Complex);

   function To_Double_Complex is new
      Matrix_Elementwise_Operation
       (X_Scalar      => Complex,
        Result_Scalar => Double_Complex,
        X_Matrix      => Complex_Matrix,
        Result_Matrix => BLAS.Double_Complex_Matrix,
        Operation     => To_Double_Complex);

   function To_Complex is new
     Matrix_Elementwise_Operation
       (X_Scalar      => Double_Complex,
        Result_Scalar => Complex,
        X_Matrix      => BLAS.Double_Complex_Matrix,
        Result_Matrix => Complex_Matrix,
        Operation     => To_Complex);

   function To_Double_Precision (X : Real) return Double_Precision is
   begin
      return Double_Precision (X);
   end To_Double_Precision;

   function To_Double_Complex (X : Complex) return Double_Complex is
   begin
      return (To_Double_Precision (X.Re), To_Double_Precision (X.Im));
   end To_Double_Complex;

   function To_Complex (X : Double_Complex) return Complex is
   begin
      return (Real (X.Re), Real (X.Im));
   end To_Complex;

   function To_Complex (X : Fortran.Complex) return Complex is
   begin
      return (Real (X.Re), Real (X.Im));
   end To_Complex;

   function To_Fortran (X : Complex) return Fortran.Complex is
   begin
      return (Fortran.Real (X.Re), Fortran.Real (X.Im));
   end To_Fortran;

   ---------
   -- dot --
   ---------

   function dot
     (N     : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;
      Y     : Complex_Vector;
      Inc_Y : Integer := 1) return Complex
   is
   begin
      if Is_Single then
         declare
            type X_Ptr is access all BLAS.Complex_Vector (X'Range);
            type Y_Ptr is access all BLAS.Complex_Vector (Y'Range);
            function Conv_X is new Unchecked_Conversion (Address, X_Ptr);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            return To_Complex (BLAS.cdotu (N, Conv_X (X'Address).all, Inc_X,
                                  Conv_Y (Y'Address).all, Inc_Y));
         end;

      elsif Is_Double then
         declare
            type X_Ptr is access all BLAS.Double_Complex_Vector (X'Range);
            type Y_Ptr is access all BLAS.Double_Complex_Vector (Y'Range);
            function Conv_X is new Unchecked_Conversion (Address, X_Ptr);
            function Conv_Y is new Unchecked_Conversion (Address, Y_Ptr);
         begin
            return To_Complex (BLAS.zdotu (N, Conv_X (X'Address).all, Inc_X,
                                     Conv_Y (Y'Address).all, Inc_Y));
         end;

      else
         return To_Complex (BLAS.zdotu (N, To_Double_Complex (X), Inc_X,
                                  To_Double_Complex (Y), Inc_Y));
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
      Alpha   : Complex := (1.0, 0.0);
      A       : Complex_Matrix;
      Ld_A    : Integer;
      B       : Complex_Matrix;
      Ld_B    : Integer;
      Beta    : Complex := (0.0, 0.0);
      C       : in out Complex_Matrix;
      Ld_C    : Integer)
   is
   begin
      if Is_Single then
         declare
            subtype A_Type is BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            subtype B_Type is BLAS.Complex_Matrix (B'Range (1), B'Range (2));
            type C_Ptr is
              access all BLAS.Complex_Matrix (C'Range (1), C'Range (2));
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_B is
               new Unchecked_Conversion (Complex_Matrix, B_Type);
            function Conv_C is
               new Unchecked_Conversion (Address, C_Ptr);
         begin
            BLAS.cgemm (Trans_A, Trans_B, M, N, K, To_Fortran (Alpha),
                   Conv_A (A), Ld_A, Conv_B (B), Ld_B, To_Fortran (Beta),
                   Conv_C (C'Address).all, Ld_C);
         end;

      elsif Is_Double then
         declare
            subtype A_Type is
               BLAS.Double_Complex_Matrix (A'Range (1), A'Range (2));
            subtype B_Type is
               BLAS.Double_Complex_Matrix (B'Range (1), B'Range (2));
            type C_Ptr is access all
               BLAS.Double_Complex_Matrix (C'Range (1), C'Range (2));
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_B is
               new Unchecked_Conversion (Complex_Matrix, B_Type);
            function Conv_C is new Unchecked_Conversion (Address, C_Ptr);
         begin
            BLAS.zgemm (Trans_A, Trans_B, M, N, K, To_Double_Complex (Alpha),
                   Conv_A (A), Ld_A, Conv_B (B), Ld_B,
                   To_Double_Complex (Beta),
                   Conv_C (C'Address).all, Ld_C);
         end;

      else
         declare
            DP_C : BLAS.Double_Complex_Matrix (C'Range (1), C'Range (2));
         begin
            if Beta.Re /= 0.0 or else Beta.Im /= 0.0 then
               DP_C := To_Double_Complex (C);
            end if;

            BLAS.zgemm (Trans_A, Trans_B, M, N, K, To_Double_Complex (Alpha),
                   To_Double_Complex (A), Ld_A,
                   To_Double_Complex (B), Ld_B, To_Double_Complex (Beta),
                   DP_C, Ld_C);

            C := To_Complex (DP_C);
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
      Alpha : Complex := (1.0, 0.0);
      A     : Complex_Matrix;
      Ld_A  : Positive;
      X     : Complex_Vector;
      Inc_X : Integer := 1;
      Beta  : Complex := (0.0, 0.0);
      Y     : in out Complex_Vector;
      Inc_Y : Integer := 1)
   is
   begin
      if Is_Single then
         declare
            subtype A_Type is BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            subtype X_Type is BLAS.Complex_Vector (X'Range);
            type Y_Ptr is access all BLAS.Complex_Vector (Y'Range);
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_X is
               new Unchecked_Conversion (Complex_Vector, X_Type);
            function Conv_Y is
               new Unchecked_Conversion (Address, Y_Ptr);
         begin
            BLAS.cgemv (Trans, M, N, To_Fortran (Alpha),
                   Conv_A (A), Ld_A, Conv_X (X), Inc_X, To_Fortran (Beta),
                   Conv_Y (Y'Address).all, Inc_Y);
         end;

      elsif Is_Double then
         declare
            subtype A_Type is
               BLAS.Double_Complex_Matrix (A'Range (1), A'Range (2));
            subtype X_Type is
               BLAS.Double_Complex_Vector (X'Range);
            type Y_Ptr is access all BLAS.Double_Complex_Vector (Y'Range);
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_X is
               new Unchecked_Conversion (Complex_Vector, X_Type);
            function Conv_Y is
               new Unchecked_Conversion (Address, Y_Ptr);
         begin
            BLAS.zgemv (Trans, M, N, To_Double_Complex (Alpha),
                   Conv_A (A), Ld_A, Conv_X (X), Inc_X,
                   To_Double_Complex (Beta),
                   Conv_Y (Y'Address).all, Inc_Y);
         end;

      else
         declare
            DP_Y : BLAS.Double_Complex_Vector (Y'Range);
         begin
            if Beta.Re /= 0.0 or else Beta.Im /= 0.0 then
               DP_Y := To_Double_Complex (Y);
            end if;

            BLAS.zgemv (Trans, M, N, To_Double_Complex (Alpha),
                   To_Double_Complex (A), Ld_A,
                   To_Double_Complex (X), Inc_X, To_Double_Complex (Beta),
                   DP_Y, Inc_Y);

            Y := To_Complex (DP_Y);
         end;
      end if;
   end gemv;

   ----------
   -- nrm2 --
   ----------

   function nrm2
     (N     : Natural;
      X     : Complex_Vector;
      Inc_X : Integer := 1) return Real
   is
   begin
      if Is_Single then
         declare
            subtype X_Type is BLAS.Complex_Vector (X'Range);
            function Conv_X is
               new Unchecked_Conversion (Complex_Vector, X_Type);
         begin
            return Real (BLAS.scnrm2 (N, Conv_X (X), Inc_X));
         end;

      elsif Is_Double then
         declare
            subtype X_Type is BLAS.Double_Complex_Vector (X'Range);
            function Conv_X is
               new Unchecked_Conversion (Complex_Vector, X_Type);
         begin
            return Real (BLAS.dznrm2 (N, Conv_X (X), Inc_X));
         end;

      else
         return Real (BLAS.dznrm2 (N, To_Double_Complex (X), Inc_X));
      end if;
   end nrm2;

end System.Generic_Complex_BLAS;
