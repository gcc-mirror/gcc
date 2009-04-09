------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             I N T E R F A C E S . F O R T R A N . L A P A C K            --
--                                                                          --
--                                 S p e c                                  --
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

--  Package comment required if non-RM package ???

with Interfaces.Fortran.BLAS;
package Interfaces.Fortran.LAPACK is
   pragma Pure;

   type Integer_Vector is array (Integer range <>) of Integer;

   Upper : aliased constant Character := 'U';
   Lower : aliased constant Character := 'L';

   subtype Real_Vector is BLAS.Real_Vector;
   subtype Real_Matrix is BLAS.Real_Matrix;
   subtype Double_Precision_Vector is BLAS.Double_Precision_Vector;
   subtype Double_Precision_Matrix is BLAS.Double_Precision_Matrix;
   subtype Complex_Vector is BLAS.Complex_Vector;
   subtype Complex_Matrix is BLAS.Complex_Matrix;
   subtype Double_Complex_Vector is BLAS.Double_Complex_Vector;
   subtype Double_Complex_Matrix is BLAS.Double_Complex_Matrix;

   --  LAPACK Computational Routines

   --  gerfs  Refines the solution of a system of linear equations with
   --         a general matrix and estimates its error
   --  getrf  Computes LU factorization of a general m-by-n matrix
   --  getri  Computes inverse of an LU-factored general matrix
   --         square matrix, with multiple right-hand sides
   --  getrs  Solves a system of linear equations with an LU-factored
   --         square matrix, with multiple right-hand sides
   --  hetrd  Reduces a complex Hermitian matrix to tridiagonal form
   --  heevr  Computes selected eigenvalues and, optionally, eigenvectors of
   --         a Hermitian matrix using the Relatively Robust Representations
   --  orgtr  Generates the real orthogonal matrix Q determined by sytrd
   --  steqr  Computes all eigenvalues and eigenvectors of a symmetric or
   --         Hermitian matrix reduced to tridiagonal form (QR algorithm)
   --  sterf  Computes all eigenvalues of a real symmetric
   --         tridiagonal matrix using QR algorithm
   --  sytrd  Reduces a real symmetric matrix to tridiagonal form

   procedure sgetrf
     (M     : Natural;
      N     : Natural;
      A     : in out Real_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   procedure dgetrf
     (M     : Natural;
      N     : Natural;
      A     : in out Double_Precision_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   procedure cgetrf
     (M     : Natural;
      N     : Natural;
      A     : in out Complex_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   procedure zgetrf
     (M     : Natural;
      N     : Natural;
      A     : in out Double_Complex_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   procedure sgetri
     (N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Real_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure dgetri
     (N      : Natural;
      A      : in out Double_Precision_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Double_Precision_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure cgetri
     (N      : Natural;
      A      : in out Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Complex_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure zgetri
     (N      : Natural;
      A      : in out Double_Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Double_Complex_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure sgetrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Real_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   procedure dgetrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Double_Precision_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Double_Precision_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   procedure cgetrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Complex_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   procedure zgetrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Double_Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Double_Complex_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   procedure cheevr
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
      Info     : access Integer);

   procedure zheevr
     (Job_Z    : access constant Character;
      Rng      : access constant Character;
      Uplo     : access constant Character;
      N        : Natural;
      A        : in out Double_Complex_Matrix;
      Ld_A     : Positive;
      Vl, Vu   : Double_Precision := 0.0;
      Il, Iu   : Integer := 1;
      Abs_Tol  : Double_Precision := 0.0;
      M        : out Integer;
      W        : out Double_Precision_Vector;
      Z        : out Double_Complex_Matrix;
      Ld_Z     : Positive;
      I_Supp_Z : out Integer_Vector;
      Work     : out Double_Complex_Vector;
      L_Work   : Integer;
      R_Work   : out Double_Precision_Vector;
      LR_Work  : Integer;
      I_Work   : out Integer_Vector;
      LI_Work  : Integer;
      Info     : access Integer);

   procedure chetrd
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Complex_Matrix;
      Ld_A   : Positive;
      D      : out Real_Vector;
      E      : out Real_Vector;
      Tau    : out Complex_Vector;
      Work   : out Complex_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure zhetrd
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Double_Complex_Matrix;
      Ld_A   : Positive;
      D      : out Double_Precision_Vector;
      E      : out Double_Precision_Vector;
      Tau    : out Double_Complex_Vector;
      Work   : out Double_Complex_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure ssytrd
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      D      : out Real_Vector;
      E      : out Real_Vector;
      Tau    : out Real_Vector;
      Work   : out Real_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure dsytrd
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Double_Precision_Matrix;
      Ld_A   : Positive;
      D      : out Double_Precision_Vector;
      E      : out Double_Precision_Vector;
      Tau    : out Double_Precision_Vector;
      Work   : out Double_Precision_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure ssterf
     (N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Info   : access Integer);

   procedure dsterf
     (N      : Natural;
      D      : in out Double_Precision_Vector;
      E      : in out Double_Precision_Vector;
      Info   : access Integer);

   procedure sorgtr
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      Tau    : Real_Vector;
      Work   : out Real_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure dorgtr
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Double_Precision_Matrix;
      Ld_A   : Positive;
      Tau    : Double_Precision_Vector;
      Work   : out Double_Precision_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure sstebz
     (Rng      : access constant Character;
      Order    : access constant Character;
      N        : Natural;
      Vl, Vu   : Real := 0.0;
      Il, Iu   : Integer := 1;
      Abs_Tol  : Real := 0.0;
      D        : Real_Vector;
      E        : Real_Vector;
      M        : out Natural;
      N_Split  : out Natural;
      W        : out Real_Vector;
      I_Block  : out Integer_Vector;
      I_Split  : out Integer_Vector;
      Work     : out Real_Vector;
      I_Work   : out Integer_Vector;
      Info     : access Integer);

   procedure dstebz
     (Rng      : access constant Character;
      Order    : access constant Character;
      N        : Natural;
      Vl, Vu   : Double_Precision := 0.0;
      Il, Iu   : Integer := 1;
      Abs_Tol  : Double_Precision := 0.0;
      D        : Double_Precision_Vector;
      E        : Double_Precision_Vector;
      M        : out Natural;
      N_Split  : out Natural;
      W        : out Double_Precision_Vector;
      I_Block  : out Integer_Vector;
      I_Split  : out Integer_Vector;
      Work     : out Double_Precision_Vector;
      I_Work   : out Integer_Vector;
      Info     : access Integer);

   procedure ssteqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Real_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer);

   procedure dsteqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Double_Precision_Vector;
      E      : in out Double_Precision_Vector;
      Z      : in out Double_Precision_Matrix;
      Ld_Z   : Positive;
      Work   : out Double_Precision_Vector;
      Info   : access Integer);

   procedure csteqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Complex_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer);

   procedure zsteqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Double_Precision_Vector;
      E      : in out Double_Precision_Vector;
      Z      : in out Double_Complex_Matrix;
      Ld_Z   : Positive;
      Work   : out Double_Precision_Vector;
      Info   : access Integer);

private
   pragma Import (Fortran, csteqr, "csteqr_");
   pragma Import (Fortran, cgetrf, "cgetrf_");
   pragma Import (Fortran, cgetri, "cgetri_");
   pragma Import (Fortran, cgetrs, "cgetrs_");
   pragma Import (Fortran, cheevr, "cheevr_");
   pragma Import (Fortran, chetrd, "chetrd_");
   pragma Import (Fortran, dgetrf, "dgetrf_");
   pragma Import (Fortran, dgetri, "dgetri_");
   pragma Import (Fortran, dgetrs, "dgetrs_");
   pragma Import (Fortran, dsytrd, "dsytrd_");
   pragma Import (Fortran, dstebz, "dstebz_");
   pragma Import (Fortran, dsterf, "dsterf_");
   pragma Import (Fortran, dorgtr, "dorgtr_");
   pragma Import (Fortran, dsteqr, "dsteqr_");
   pragma Import (Fortran, sgetrf, "sgetrf_");
   pragma Import (Fortran, sgetri, "sgetri_");
   pragma Import (Fortran, sgetrs, "sgetrs_");
   pragma Import (Fortran, sorgtr, "sorgtr_");
   pragma Import (Fortran, sstebz, "sstebz_");
   pragma Import (Fortran, ssterf, "ssterf_");
   pragma Import (Fortran, ssteqr, "ssteqr_");
   pragma Import (Fortran, ssytrd, "ssytrd_");
   pragma Import (Fortran, zgetrf, "zgetrf_");
   pragma Import (Fortran, zgetri, "zgetri_");
   pragma Import (Fortran, zgetrs, "zgetrs_");
   pragma Import (Fortran, zheevr, "zheevr_");
   pragma Import (Fortran, zhetrd, "zhetrd_");
   pragma Import (Fortran, zsteqr, "zsteqr_");
end Interfaces.Fortran.LAPACK;
