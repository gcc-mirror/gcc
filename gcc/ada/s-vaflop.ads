------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V A X _ F L O A T _ O P E R A T I O N S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2.10.1 $
--                                                                          --
--          Copyright (C) 1997-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  This package contains runtime routines for handling the non-IEEE
--  floating-point formats used on the Vax and the Alpha.

package System.Vax_Float_Operations is

   pragma Warnings (Off);
   --  Suppress warnings if not on Alpha/VAX

   type D is digits 9;
   pragma Float_Representation (VAX_Float, D);
   --  D Float type on Vax

   type G is digits 15;
   pragma Float_Representation (VAX_Float, G);
   --  G Float type on Vax

   type F is digits 6;
   pragma Float_Representation (VAX_Float, F);
   --  F Float type on Vax

   type S is digits 6;
   pragma Float_Representation (IEEE_Float, S);
   --  IEEE short

   type T is digits 15;
   pragma Float_Representation (IEEE_Float, T);
   --  IEEE long

   pragma Warnings (On);

   type Q is range -2 ** 63 .. +(2 ** 63 - 1);
   --  64-bit signed integer

   --------------------------
   -- Conversion Functions --
   --------------------------

   function D_To_G (X : D) return G;
   function G_To_D (X : G) return D;
   --  Conversions between D float and G float

   function G_To_F (X : G) return F;
   function F_To_G (X : F) return G;
   --  Conversions between F float and G float

   function F_To_S (X : F) return S;
   function S_To_F (X : S) return F;
   --  Conversions between F float and IEEE short

   function G_To_T (X : G) return T;
   function T_To_G (X : T) return G;
   --  Conversions between G float and IEEE long

   function F_To_Q (X : F) return Q;
   function Q_To_F (X : Q) return F;
   --  Conversions between F float and 64-bit integer

   function G_To_Q (X : G) return Q;
   function Q_To_G (X : Q) return G;
   --  Conversions between G float and 64-bit integer

   function T_To_D (X : T) return D;
   --  Conversion from IEEE long to D_Float (used for literals)

   --------------------------
   -- Arithmetic Functions --
   --------------------------

   function Abs_F (X : F) return F;
   function Abs_G (X : G) return G;
   --  Absolute value of F/G float

   function Add_F (X, Y : F) return F;
   function Add_G (X, Y : G) return G;
   --  Addition of F/G float

   function Div_F (X, Y : F) return F;
   function Div_G (X, Y : G) return G;
   --  Division of F/G float

   function Mul_F (X, Y : F) return F;
   function Mul_G (X, Y : G) return G;
   --  Multiplication of F/G float

   function Neg_F (X : F) return F;
   function Neg_G (X : G) return G;
   --  Negation of F/G float

   function Sub_F (X, Y : F) return F;
   function Sub_G (X, Y : G) return G;
   --  Subtraction of F/G float

   --------------------------
   -- Comparison Functions --
   --------------------------

   function Eq_F (X, Y : F) return Boolean;
   function Eq_G (X, Y : G) return Boolean;
   --  Compares for X = Y

   function Le_F (X, Y : F) return Boolean;
   function Le_G (X, Y : G) return Boolean;
   --  Compares for X <= Y

   function Lt_F (X, Y : F) return Boolean;
   function Lt_G (X, Y : G) return Boolean;
   --  Compares for X < Y

   ----------------------
   -- Debug Procedures --
   ----------------------

   procedure Debug_Output_D (Arg : D);
   procedure Debug_Output_F (Arg : F);
   procedure Debug_Output_G (Arg : G);
   pragma Export (Ada, Debug_Output_D);
   pragma Export (Ada, Debug_Output_F);
   pragma Export (Ada, Debug_Output_G);
   --  These routines output their argument in decimal string form, with
   --  no terminating line return. They are provided for implicit use by
   --  the pre gnat-3.12w GDB, and are retained for backwards compatibility.

   function Debug_String_D (Arg : D) return System.Address;
   function Debug_String_F (Arg : F) return System.Address;
   function Debug_String_G (Arg : G) return System.Address;
   pragma Export (Ada, Debug_String_D);
   pragma Export (Ada, Debug_String_F);
   pragma Export (Ada, Debug_String_G);
   --  These routines return a decimal C string image of their argument.
   --  They are provided for implicit use by the debugger, in response to
   --  the special encoding used for Vax floating-point types (see Exp_Dbug
   --  for details). They supersede the above Debug_Output_D/F/G routines
   --  which didn't work properly with GDBTK.

   procedure pd (Arg : D);
   procedure pf (Arg : F);
   procedure pg (Arg : G);
   pragma Export (Ada, pd);
   pragma Export (Ada, pf);
   pragma Export (Ada, pg);
   --  These are like the Debug_Output_D/F/G procedures except that they
   --  output a line return after the output. They were originally present
   --  for direct use in GDB before GDB recognized Vax floating-point
   --  types, and are retained for backwards compatibility.

private
   pragma Inline (D_To_G);
   pragma Inline (F_To_G);
   pragma Inline (F_To_Q);
   pragma Inline (F_To_S);
   pragma Inline (G_To_D);
   pragma Inline (G_To_F);
   pragma Inline (G_To_Q);
   pragma Inline (G_To_T);
   pragma Inline (Q_To_F);
   pragma Inline (Q_To_G);
   pragma Inline (S_To_F);
   pragma Inline (T_To_G);

   pragma Inline (Abs_F);
   pragma Inline (Abs_G);
   pragma Inline (Add_F);
   pragma Inline (Add_G);
   pragma Inline (Div_G);
   pragma Inline (Div_F);
   pragma Inline (Mul_F);
   pragma Inline (Mul_G);
   pragma Inline (Neg_G);
   pragma Inline (Neg_F);
   pragma Inline (Sub_F);
   pragma Inline (Sub_G);

   pragma Inline (Eq_F);
   pragma Inline (Eq_G);
   pragma Inline (Le_F);
   pragma Inline (Le_G);
   pragma Inline (Lt_F);
   pragma Inline (Lt_G);

end System.Vax_Float_Operations;
