------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         S Y S T E M . G E N E R I C _ C O M P L E X _ L A P A C K        --
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

with Ada.Numerics.Generic_Complex_Types;
generic
   type Real is digits <>;
   type Real_Vector is array (Integer range <>) of Real;

   with package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Real);
   use Complex_Types;

   type Complex_Vector is array (Integer range <>) of Complex;
   type Complex_Matrix is array (Integer range <>, Integer range <>)
      of Complex;
package System.Generic_Complex_LAPACK is
   pragma Pure;

   type Integer_Vector is array (Integer range <>) of Integer;

   Upper : aliased constant Character := 'U';
   Lower : aliased constant Character := 'L';

   --  LAPACK Computational Routines

   --  getrf computes LU factorization of a general m-by-n matrix

   procedure getrf
     (M     : Natural;
      N     : Natural;
      A     : in out Complex_Matrix;
      Ld_A  : Positive;
      I_Piv : out Integer_Vector;
      Info  : access Integer);

   --  getri computes inverse of an LU-factored square matrix,
   --  with multiple right-hand sides

   procedure getri
     (N      : Natural;
      A      : in out Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      Work   : in out Complex_Vector;
      L_Work : Integer;
      Info   : access Integer);

   --  getrs solves a system of linear equations with an LU-factored
   --  square matrix, with multiple right-hand sides

   procedure getrs
     (Trans  : access constant Character;
      N      : Natural;
      N_Rhs  : Natural;
      A      : Complex_Matrix;
      Ld_A   : Positive;
      I_Piv  : Integer_Vector;
      B      : in out Complex_Matrix;
      Ld_B   : Positive;
      Info   : access Integer);

   --  heevr computes selected eigenvalues and, optionally,
   --  eigenvectors of a Hermitian matrix using the Relatively
   --  Robust Representations

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
      Info     : access Integer);

   --  steqr computes all eigenvalues and eigenvectors of a symmetric or
   --  Hermitian matrix reduced to tridiagonal form (QR algorithm)

   procedure steqr
     (Comp_Z : access constant Character;
      N      : Natural;
      D      : in out Real_Vector;
      E      : in out Real_Vector;
      Z      : in out Complex_Matrix;
      Ld_Z   : Positive;
      Work   : out Real_Vector;
      Info   : access Integer);

end System.Generic_Complex_LAPACK;
