------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         S Y S T E M . G E N E R I C _ C O M P L E X _ L A P A C K        --
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
with Interfaces.Fortran.LAPACK;       use Interfaces.Fortran.LAPACK;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body System.Generic_Complex_LAPACK is

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

   function To_Real (X : Double_Precision) return Real;
   pragma Inline (To_Real);

   function To_Double_Complex (X : Complex) return Double_Complex;
   pragma Inline (To_Double_Complex);

   function To_Complex (X : Double_Complex) return Complex;
   pragma Inline (To_Complex);

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

   function To_Double_Complex is new
     Matrix_Elementwise_Operation
       (X_Scalar      => Complex,
        Result_Scalar => Double_Complex,
        X_Matrix      => Complex_Matrix,
        Result_Matrix => Double_Complex_Matrix,
        Operation     => To_Double_Complex);

   function To_Complex is new
     Matrix_Elementwise_Operation
       (X_Scalar      => Double_Complex,
        Result_Scalar => Complex,
        X_Matrix      => Double_Complex_Matrix,
        Result_Matrix => Complex_Matrix,
        Operation     => To_Complex);

   function To_Double_Precision (X : Real) return Double_Precision is
   begin
      return Double_Precision (X);
   end To_Double_Precision;

   function To_Real (X : Double_Precision) return Real is
   begin
      return Real (X);
   end To_Real;

   function To_Double_Complex (X : Complex) return Double_Complex is
   begin
      return (To_Double_Precision (X.Re), To_Double_Precision (X.Im));
   end To_Double_Complex;

   function To_Complex (X : Double_Complex) return Complex is
   begin
      return (Real (X.Re), Real (X.Im));
   end To_Complex;

   -----------
   -- getrf --
   -----------

   procedure getrf
     (M     : Natural;
      N     : Natural;
      A     : in out Complex_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer)
   is
   begin
      if Is_Single then
         declare
            type A_Ptr is
               access all BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
         begin
            cgetrf (M, N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv), Info);
         end;

      elsif Is_Double then
         declare
            type A_Ptr is
               access all Double_Complex_Matrix (A'Range (1), A'Range (2));
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
         begin
            zgetrf (M, N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv), Info);
         end;

      else
         declare
            DP_A : Double_Complex_Matrix (A'Range (1), A'Range (2));
         begin
            DP_A := To_Double_Complex (A);
            zgetrf (M, N, DP_A, Ld_A, LAPACK.Integer_Vector (I_Piv), Info);
            A := To_Complex (DP_A);
         end;
      end if;
   end getrf;

   -----------
   -- getri --
   -----------

   procedure getri
     (N      : Natural;
      A      : in out Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Complex_Vector;
      L_Work : Integer;
      Info   : access Integer)
   is
   begin
      if Is_Single then
         declare
            type A_Ptr is
               access all BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            type Work_Ptr is
               access all BLAS.Complex_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            cgetri (N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      elsif Is_Double then
         declare
            type A_Ptr is
               access all Double_Complex_Matrix (A'Range (1), A'Range (2));
            type Work_Ptr is
               access all Double_Complex_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            zgetri (N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      else
         declare
            DP_A : Double_Complex_Matrix (A'Range (1), A'Range (2));
            DP_Work : Double_Complex_Vector (Work'Range);
         begin
            DP_A := To_Double_Complex (A);
            zgetri (N, DP_A, Ld_A, LAPACK.Integer_Vector (I_Piv),
                    DP_Work, L_Work, Info);
            A := To_Complex (DP_A);
            Work (1) := To_Complex (DP_Work (1));
         end;
      end if;
   end getri;

   -----------
   -- getrs --
   -----------

   procedure getrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Complex_Matrix;
      Ld_B   : Positive;
      Info   : access Integer)
   is
   begin
      if Is_Single then
         declare
            subtype A_Type is BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            type B_Ptr is
               access all BLAS.Complex_Matrix (B'Range (1), B'Range (2));
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Address, B_Ptr);
         begin
            cgetrs (Trans, N, N_Rhs,
                    Conv_A (A), Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_B (B'Address).all, Ld_B,
                    Info);
         end;

      elsif Is_Double then
         declare
            subtype A_Type is
               Double_Complex_Matrix (A'Range (1), A'Range (2));
            type B_Ptr is
               access all Double_Complex_Matrix (B'Range (1), B'Range (2));
            function Conv_A is
               new Unchecked_Conversion (Complex_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Address, B_Ptr);
         begin
            zgetrs (Trans, N, N_Rhs,
                    Conv_A (A), Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_B (B'Address).all, Ld_B,
                    Info);
         end;

      else
         declare
            DP_A : Double_Complex_Matrix (A'Range (1), A'Range (2));
            DP_B : Double_Complex_Matrix (B'Range (1), B'Range (2));
         begin
            DP_A := To_Double_Complex (A);
            DP_B := To_Double_Complex (B);
            zgetrs (Trans, N, N_Rhs,
                    DP_A, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    DP_B, Ld_B,
                    Info);
            B := To_Complex (DP_B);
         end;
      end if;
   end getrs;

   procedure heevr
     (Job_Z    : access constant Character;
      Rng      : access constant Character;
      Uplo     : access constant Character;
      N        : Natural;
      A        : in out Complex_Matrix;
      Ld_A     : Positive;
      Vl, Vu   : Real := 0.0;
      Il, Iu   : Integer := 1;
      Abs_Tol  : Real := 0.0;
      M        : out Integer;
      W        : out Real_Vector;
      Z        : out Complex_Matrix;
      Ld_Z     : Positive;
      I_Supp_Z : out Integer_Vector;
      Work     : out Complex_Vector;
      L_Work   : Integer;
      R_Work   : out Real_Vector;
      LR_Work  : Integer;
      I_Work   : out Integer_Vector;
      LI_Work  : Integer;
      Info     : access Integer)
   is
   begin
      if Is_Single then
         declare
            type A_Ptr is
               access all BLAS.Complex_Matrix (A'Range (1), A'Range (2));
            type W_Ptr is
               access all BLAS.Real_Vector (W'Range);
            type Z_Ptr is
               access all BLAS.Complex_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is access all  BLAS.Complex_Vector (Work'Range);
            type R_Work_Ptr is access all BLAS.Real_Vector (R_Work'Range);

            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_W is new Unchecked_Conversion (Address, W_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
            function Conv_R_Work is
               new Unchecked_Conversion (Address, R_Work_Ptr);
         begin
            cheevr (Job_Z, Rng, Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Fortran.Real (Vl), Fortran.Real (Vu),
                    Il, Iu, Fortran.Real (Abs_Tol), M,
                    Conv_W (W'Address).all,
                    Conv_Z (Z'Address).all, Ld_Z,
                    LAPACK.Integer_Vector (I_Supp_Z),
                    Conv_Work (Work'Address).all, L_Work,
                    Conv_R_Work (R_Work'Address).all, LR_Work,
                    LAPACK.Integer_Vector (I_Work), LI_Work, Info);
         end;

      elsif Is_Double then
         declare
            type A_Ptr is
              access all BLAS.Double_Complex_Matrix (A'Range (1), A'Range (2));
            type W_Ptr is
              access all BLAS.Double_Precision_Vector (W'Range);
            type Z_Ptr is
              access all BLAS.Double_Complex_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is
               access all BLAS.Double_Complex_Vector (Work'Range);
            type R_Work_Ptr is
               access all BLAS.Double_Precision_Vector (R_Work'Range);

            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_W is new Unchecked_Conversion (Address, W_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
            function Conv_R_Work is
               new Unchecked_Conversion (Address, R_Work_Ptr);
         begin
            zheevr (Job_Z, Rng, Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Double_Precision (Vl), Double_Precision (Vu),
                    Il, Iu, Double_Precision (Abs_Tol), M,
                    Conv_W (W'Address).all,
                    Conv_Z (Z'Address).all, Ld_Z,
                    LAPACK.Integer_Vector (I_Supp_Z),
                    Conv_Work (Work'Address).all, L_Work,
                    Conv_R_Work (R_Work'Address).all, LR_Work,
                    LAPACK.Integer_Vector (I_Work), LI_Work, Info);
         end;

      else
         declare
            DP_A : Double_Complex_Matrix (A'Range (1), A'Range (2));
            DP_W : Double_Precision_Vector (W'Range);
            DP_Z : Double_Complex_Matrix (Z'Range (1), Z'Range (2));
            DP_Work : Double_Complex_Vector (Work'Range);
            DP_R_Work : Double_Precision_Vector (R_Work'Range);

         begin
            DP_A := To_Double_Complex (A);

            zheevr (Job_Z, Rng, Uplo, N,
                    DP_A, Ld_A,
                    Double_Precision (Vl), Double_Precision (Vu),
                    Il, Iu, Double_Precision (Abs_Tol), M,
                    DP_W, DP_Z, Ld_Z,
                    LAPACK.Integer_Vector (I_Supp_Z),
                    DP_Work, L_Work,
                    DP_R_Work, LR_Work,
                    LAPACK.Integer_Vector (I_Work), LI_Work, Info);

            A := To_Complex (DP_A);
            W := To_Real (DP_W);
            Z := To_Complex (DP_Z);

            Work (1) := To_Complex (DP_Work (1));
            R_Work (1) := To_Real (DP_R_Work (1));
         end;
      end if;
   end heevr;

   -----------
   -- steqr --
   -----------

   procedure steqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Complex_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer)
   is
   begin
      if Is_Single then
         declare
            type D_Ptr is access all BLAS.Real_Vector (D'Range);
            type E_Ptr is access all BLAS.Real_Vector (E'Range);
            type Z_Ptr is
               access all BLAS.Complex_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is
               access all BLAS.Real_Vector (Work'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            csteqr (Comp_Z, N,
                    Conv_D (D'Address).all,
                    Conv_E (E'Address).all,
                    Conv_Z (Z'Address).all,
                    Ld_Z,
                    Conv_Work (Work'Address).all,
                    Info);
         end;

      elsif Is_Double then
         declare
            type D_Ptr is access all Double_Precision_Vector (D'Range);
            type E_Ptr is access all Double_Precision_Vector (E'Range);
            type Z_Ptr is
               access all Double_Complex_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is
               access all Double_Precision_Vector (Work'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            zsteqr (Comp_Z, N,
                    Conv_D (D'Address).all,
                    Conv_E (E'Address).all,
                    Conv_Z (Z'Address).all,
                    Ld_Z,
                    Conv_Work (Work'Address).all,
                    Info);
         end;

      else
         declare
            DP_D    : Double_Precision_Vector (D'Range);
            DP_E    : Double_Precision_Vector (E'Range);
            DP_Z    : Double_Complex_Matrix (Z'Range (1), Z'Range (2));
            DP_Work : Double_Precision_Vector (Work'Range);
         begin
            DP_D := To_Double_Precision (D);
            DP_E := To_Double_Precision (E);

            if Comp_Z.all = 'V' then
               DP_Z := To_Double_Complex (Z);
            end if;

            zsteqr (Comp_Z, N, DP_D, DP_E, DP_Z, Ld_Z, DP_Work, Info);

            D := To_Real (DP_D);
            E := To_Real (DP_E);

            if Comp_Z.all /= 'N' then
               Z := To_Complex (DP_Z);
            end if;
         end;
      end if;
   end steqr;

end System.Generic_Complex_LAPACK;
