------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . G E N E R I C _ R E A L _ L A P A C K            --
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

--  Package comment required ???

generic
   type Real is digits <>;
   type Real_Vector is array (Integer range <>) of Real;
   type Real_Matrix is array (Integer range <>, Integer range <>) of Real;
package System.Generic_Real_LAPACK is
   pragma Pure;

   type Integer_Vector is array (Integer range <>) of Integer;

   Upper : aliased constant Character := 'U';
   Lower : aliased constant Character := 'L';

   --  LAPACK Computational Routines

   --  gerfs  Refines the solution of a system of linear equations with
   --         a general matrix and estimates its error
   --  getrf  Computes LU factorization of a general m-by-n matrix
   --  getri  Computes inverse of an LU-factored general matrix
   --         square matrix, with multiple right-hand sides
   --  getrs  Solves a system of linear equations with an LU-factored
   --         square matrix, with multiple right-hand sides
   --  orgtr  Generates the Float orthogonal matrix Q determined by sytrd
   --  steqr  Computes all eigenvalues and eigenvectors of a symmetric or
   --         Hermitian matrix reduced to tridiagonal form (QR algorithm)
   --  sterf  Computes all eigenvalues of a Float symmetric
   --         tridiagonal matrix using QR algorithm
   --  sytrd  Reduces a Float symmetric matrix to tridiagonal form

   procedure getrf
     (M     : Natural;
      N     : Natural;
      A     : in out Real_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   procedure getri
     (N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Real_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure getrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Real_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Real_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   procedure orgtr
     (Uplo   : access constant Character;
      N      : Natural;
      A      : in out Real_Matrix;
      Ld_A   : Positive;
      Tau    : Real_Vector;
      Work   : out Real_Vector;
      L_Work : Integer;
      Info   : access Integer);

   procedure sterf
     (N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Info   : access Integer);

   procedure steqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Real_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer);

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
      Info   : access Integer);

end System.Generic_Real_LAPACK;
