------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        SYSTEM.GENERIC_REAL_LAPACK                        --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;        use Ada;
with Interfaces;                      use Interfaces;
with Interfaces.Fortran;              use Interfaces.Fortran;
with Interfaces.Fortran.BLAS;         use Interfaces.Fortran.BLAS;
with Interfaces.Fortran.LAPACK;       use Interfaces.Fortran.LAPACK;
with System.Generic_Array_Operations; use System.Generic_Array_Operations;

package body System.Generic_Real_LAPACK is

   Is_Real : constant Boolean :=
               Real'Machine_Mantissa = Fortran.Real'Machine_Mantissa
                and then Fortran.Real (Real'First) = Fortran.Real'First
                and then Fortran.Real (Real'Last) = Fortran.Real'Last;

   Is_Double_Precision : constant Boolean :=
                           Real'Machine_Mantissa =
                                            Double_Precision'Machine_Mantissa
                            and then
                              Double_Precision (Real'First) =
                                            Double_Precision'First
                            and then
                              Double_Precision (Real'Last) =
                                            Double_Precision'Last;

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

   -----------
   -- getrf --
   -----------

   procedure getrf
     (M     : Natural;
      N     : Natural;
      A     : in out Real_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer)
   is
   begin
      if Is_Real then
         declare
            type A_Ptr is
               access all BLAS.Real_Matrix (A'Range (1), A'Range (2));
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
         begin
            sgetrf (M, N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv), Info);
         end;

      elsif Is_Double_Precision then
         declare
            type A_Ptr is
               access all Double_Precision_Matrix (A'Range (1), A'Range (2));
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
         begin
            dgetrf (M, N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv), Info);
         end;

      else
         declare
            DP_A : Double_Precision_Matrix (A'Range (1), A'Range (2));
         begin
            DP_A := To_Double_Precision (A);
            dgetrf (M, N, DP_A, Ld_A, LAPACK.Integer_Vector (I_Piv), Info);
            A := To_Real (DP_A);
         end;
      end if;
   end getrf;

   -----------
   -- getri --
   -----------

   procedure getri
     (N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Real_Vector;
      L_Work : Integer;
      Info   : access Integer)
   is
   begin
      if Is_Real then
         declare
            type A_Ptr is
               access all BLAS.Real_Matrix (A'Range (1), A'Range (2));
            type Work_Ptr is
               access all BLAS.Real_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            sgetri (N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      elsif Is_Double_Precision then
         declare
            type A_Ptr is
               access all Double_Precision_Matrix (A'Range (1), A'Range (2));
            type Work_Ptr is
               access all Double_Precision_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            dgetri (N, Conv_A (A'Address).all, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      else
         declare
            DP_A : Double_Precision_Matrix (A'Range (1), A'Range (2));
            DP_Work : Double_Precision_Vector (Work'Range);
         begin
            DP_A := To_Double_Precision (A);
            dgetri (N, DP_A, Ld_A, LAPACK.Integer_Vector (I_Piv),
                    DP_Work, L_Work, Info);
            A := To_Real (DP_A);
            Work (1) := To_Real (DP_Work (1));
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
      A      : Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Real_Matrix;
      Ld_B   : Positive;
      Info   : access Integer)
   is
   begin
      if Is_Real then
         declare
            subtype A_Type is BLAS.Real_Matrix (A'Range (1), A'Range (2));
            type B_Ptr is
               access all BLAS.Real_Matrix (B'Range (1), B'Range (2));
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Address, B_Ptr);
         begin
            sgetrs (Trans, N, N_Rhs,
                    Conv_A (A), Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_B (B'Address).all, Ld_B,
                    Info);
         end;

      elsif Is_Double_Precision then
         declare
            subtype A_Type is
               Double_Precision_Matrix (A'Range (1), A'Range (2));
            type B_Ptr is
               access all Double_Precision_Matrix (B'Range (1), B'Range (2));
            function Conv_A is new Unchecked_Conversion (Real_Matrix, A_Type);
            function Conv_B is new Unchecked_Conversion (Address, B_Ptr);
         begin
            dgetrs (Trans, N, N_Rhs,
                    Conv_A (A), Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    Conv_B (B'Address).all, Ld_B,
                    Info);
         end;

      else
         declare
            DP_A : Double_Precision_Matrix (A'Range (1), A'Range (2));
            DP_B : Double_Precision_Matrix (B'Range (1), B'Range (2));
         begin
            DP_A := To_Double_Precision (A);
            DP_B := To_Double_Precision (B);
            dgetrs (Trans, N, N_Rhs,
                    DP_A, Ld_A,
                    LAPACK.Integer_Vector (I_Piv),
                    DP_B, Ld_B,
                    Info);
            B := To_Real (DP_B);
         end;
      end if;
   end getrs;

   -----------
   -- orgtr --
   -----------

   procedure orgtr
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      Tau    : Real_Vector;
      Work   : out Real_Vector;
      L_Work : Integer;
      Info   : access Integer)
   is
   begin
      if Is_Real then
         declare
            type A_Ptr is
               access all BLAS.Real_Matrix (A'Range (1), A'Range (2));
            subtype Tau_Type is BLAS.Real_Vector (Tau'Range);
            type Work_Ptr is
               access all BLAS.Real_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Tau is
               new Unchecked_Conversion (Real_Vector, Tau_Type);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            sorgtr (Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Conv_Tau (Tau),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      elsif Is_Double_Precision then
         declare
            type A_Ptr is
               access all Double_Precision_Matrix (A'Range (1), A'Range (2));
            subtype Tau_Type is Double_Precision_Vector (Tau'Range);
            type Work_Ptr is
               access all Double_Precision_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_Tau is
               new Unchecked_Conversion (Real_Vector, Tau_Type);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            dorgtr (Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Conv_Tau (Tau),
                    Conv_Work (Work'Address).all, L_Work,
                    Info);
         end;

      else
         declare
            DP_A : Double_Precision_Matrix (A'Range (1), A'Range (2));
            DP_Work : Double_Precision_Vector (Work'Range);
            DP_Tau  : Double_Precision_Vector (Tau'Range);
         begin
            DP_A := To_Double_Precision (A);
            DP_Tau := To_Double_Precision (Tau);
            dorgtr (Uplo, N, DP_A, Ld_A, DP_Tau, DP_Work, L_Work, Info);
            A := To_Real (DP_A);
            Work (1) := To_Real (DP_Work (1));
         end;
      end if;
   end orgtr;

   -----------
   -- steqr --
   -----------

   procedure steqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Real_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer)
   is
   begin
      if Is_Real then
         declare
            type D_Ptr is access all BLAS.Real_Vector (D'Range);
            type E_Ptr is access all BLAS.Real_Vector (E'Range);
            type Z_Ptr is
               access all BLAS.Real_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is
               access all BLAS.Real_Vector (Work'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            ssteqr (Comp_Z, N,
                    Conv_D (D'Address).all,
                    Conv_E (E'Address).all,
                    Conv_Z (Z'Address).all,
                    Ld_Z,
                    Conv_Work (Work'Address).all,
                    Info);
         end;

      elsif Is_Double_Precision then
         declare
            type D_Ptr is access all Double_Precision_Vector (D'Range);
            type E_Ptr is access all Double_Precision_Vector (E'Range);
            type Z_Ptr is
               access all Double_Precision_Matrix (Z'Range (1), Z'Range (2));
            type Work_Ptr is
               access all Double_Precision_Vector (Work'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Z is new Unchecked_Conversion (Address, Z_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            dsteqr (Comp_Z, N,
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
            DP_Z    : Double_Precision_Matrix (Z'Range (1), Z'Range (2));
            DP_Work : Double_Precision_Vector (Work'Range);
         begin
            DP_D := To_Double_Precision (D);
            DP_E := To_Double_Precision (E);

            if Comp_Z.all = 'V' then
               DP_Z := To_Double_Precision (Z);
            end if;

            dsteqr (Comp_Z, N, DP_D, DP_E, DP_Z, Ld_Z, DP_Work, Info);

            D := To_Real (DP_D);
            E := To_Real (DP_E);
            Z := To_Real (DP_Z);
         end;
      end if;
   end steqr;

   -----------
   -- sterf --
   -----------

   procedure sterf
     (N    : Natural;
      D    : in out Real_Vector;
      E    : in out Real_Vector;
      Info : access Integer)
   is
   begin
      if Is_Real then
         declare
            type D_Ptr is access all BLAS.Real_Vector (D'Range);
            type E_Ptr is access all BLAS.Real_Vector (E'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
         begin
            ssterf (N, Conv_D (D'Address).all, Conv_E (E'Address).all, Info);
         end;

      elsif Is_Double_Precision then
         declare
            type D_Ptr is access all Double_Precision_Vector (D'Range);
            type E_Ptr is access all Double_Precision_Vector (E'Range);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
         begin
            dsterf (N, Conv_D (D'Address).all, Conv_E (E'Address).all, Info);
         end;

      else
         declare
            DP_D    : Double_Precision_Vector (D'Range);
            DP_E    : Double_Precision_Vector (E'Range);

         begin
            DP_D := To_Double_Precision (D);
            DP_E := To_Double_Precision (E);

            dsterf (N, DP_D, DP_E, Info);

            D := To_Real (DP_D);
            E := To_Real (DP_E);
         end;
      end if;
   end sterf;

   -----------
   -- sytrd --
   -----------

   procedure sytrd
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      D      : out Real_Vector;
      E      : out Real_Vector;
      Tau    : out Real_Vector;
      Work   : out Real_Vector;
      L_Work : Integer;
      Info   : access Integer)
   is
   begin
      if Is_Real then
         declare
            type A_Ptr is
               access all BLAS.Real_Matrix (A'Range (1), A'Range (2));
            type D_Ptr is access all BLAS.Real_Vector (D'Range);
            type E_Ptr is access all BLAS.Real_Vector (E'Range);
            type Tau_Ptr is access all BLAS.Real_Vector (Tau'Range);
            type Work_Ptr is
               access all BLAS.Real_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Tau is new Unchecked_Conversion (Address, Tau_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            ssytrd (Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Conv_D (D'Address).all,
                    Conv_E (E'Address).all,
                    Conv_Tau (Tau'Address).all,
                    Conv_Work (Work'Address).all,
                    L_Work,
                    Info);
         end;

      elsif Is_Double_Precision then
         declare
            type A_Ptr is
               access all Double_Precision_Matrix (A'Range (1), A'Range (2));
            type D_Ptr is access all Double_Precision_Vector (D'Range);
            type E_Ptr is access all Double_Precision_Vector (E'Range);
            type Tau_Ptr is access all Double_Precision_Vector (Tau'Range);
            type Work_Ptr is
               access all Double_Precision_Vector (Work'Range);
            function Conv_A is new Unchecked_Conversion (Address, A_Ptr);
            function Conv_D is new Unchecked_Conversion (Address, D_Ptr);
            function Conv_E is new Unchecked_Conversion (Address, E_Ptr);
            function Conv_Tau is new Unchecked_Conversion (Address, Tau_Ptr);
            function Conv_Work is new Unchecked_Conversion (Address, Work_Ptr);
         begin
            dsytrd (Uplo, N,
                    Conv_A (A'Address).all, Ld_A,
                    Conv_D (D'Address).all,
                    Conv_E (E'Address).all,
                    Conv_Tau (Tau'Address).all,
                    Conv_Work (Work'Address).all,
                    L_Work,
                    Info);
         end;

      else
         declare
            DP_A    : Double_Precision_Matrix (A'Range (1), A'Range (2));
            DP_D    : Double_Precision_Vector (D'Range);
            DP_E    : Double_Precision_Vector (E'Range);
            DP_Tau  : Double_Precision_Vector (Tau'Range);
            DP_Work : Double_Precision_Vector (Work'Range);
         begin
            DP_A := To_Double_Precision (A);

            dsytrd (Uplo, N, DP_A, Ld_A, DP_D, DP_E, DP_Tau,
                    DP_Work, L_Work, Info);

            if L_Work /= -1 then
               A := To_Real (DP_A);
               D := To_Real (DP_D);
               E := To_Real (DP_E);
               Tau := To_Real (DP_Tau);
            end if;

            Work (1) := To_Real (DP_Work (1));
         end;
      end if;
   end sytrd;

end System.Generic_Real_LAPACK;
