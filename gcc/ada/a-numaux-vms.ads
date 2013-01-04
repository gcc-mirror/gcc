------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                             (VMS Version)                                --
--                                                                          --
--          Copyright (C) 2003-2011, Free Software Foundation, Inc.         --
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

--  This package provides the basic computational interface for the generic
--  elementary functions. The C library version interfaces with the routines
--  in the C mathematical library, and is thus quite portable, although it may
--  not necessarily meet the requirements for accuracy in the numerics annex.

--  This is the VMS version

package Ada.Numerics.Aux is
   pragma Pure;

   type Double is digits 15;
   pragma Float_Representation (IEEE_Float, Double);
   --  Type Double is the type used to call the C routines. Note that this
   --  is IEEE format even when running on VMS with VAX_Native representation
   --  since we use the IEEE version of the C library with VMS.

   --  We import these functions directly from C. Note that we label them
   --  all as pure functions, because indeed all of them are in fact pure!

   function Sin (X : Double) return Double;
   pragma Import (C, Sin, "MATH$SIN_T");
   pragma Pure_Function (Sin);

   function Cos (X : Double) return Double;
   pragma Import (C, Cos, "MATH$COS_T");
   pragma Pure_Function (Cos);

   function Tan (X : Double) return Double;
   pragma Import (C, Tan, "MATH$TAN_T");
   pragma Pure_Function (Tan);

   function Exp (X : Double) return Double;
   pragma Import (C, Exp, "MATH$EXP_T");
   pragma Pure_Function (Exp);

   function Sqrt (X : Double) return Double;
   pragma Import (C, Sqrt, "MATH$SQRT_T");
   pragma Pure_Function (Sqrt);

   function Log (X : Double) return Double;
   pragma Import (C, Log, "DECC$TLOG_2");
   pragma Pure_Function (Log);

   function Acos (X : Double) return Double;
   pragma Import (C, Acos, "MATH$ACOS_T");
   pragma Pure_Function (Acos);

   function Asin (X : Double) return Double;
   pragma Import (C, Asin, "MATH$ASIN_T");
   pragma Pure_Function (Asin);

   function Atan (X : Double) return Double;
   pragma Import (C, Atan, "MATH$ATAN_T");
   pragma Pure_Function (Atan);

   function Sinh (X : Double) return Double;
   pragma Import (C, Sinh, "MATH$SINH_T");
   pragma Pure_Function (Sinh);

   function Cosh (X : Double) return Double;
   pragma Import (C, Cosh, "MATH$COSH_T");
   pragma Pure_Function (Cosh);

   function Tanh (X : Double) return Double;
   pragma Import (C, Tanh, "MATH$TANH_T");
   pragma Pure_Function (Tanh);

   function Pow (X, Y : Double) return Double;
   pragma Import (C, Pow, "DECC$TPOW_2");
   pragma Pure_Function (Pow);

end Ada.Numerics.Aux;
