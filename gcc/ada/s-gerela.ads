------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        SYSTEM.GENERIC_REAL_LAPACK                        --
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
