------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V A X _ F L O A T _ O P E R A T I O N S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1997-2000 Free Software Foundation, Inc.          --
--                       (Version for Alpha OpenVMS)                        --
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

with System.IO;           use System.IO;
with System.Machine_Code; use System.Machine_Code;

package body System.Vax_Float_Operations is

   --  Ensure this gets compiled with -O to avoid extra (and possibly
   --  improper) memory stores.

   pragma Optimize (Time);

   --  Declare the functions that do the conversions between floating-point
   --  formats.  Call the operands IEEE float so they get passed in
   --  FP registers.

   function Cvt_G_T (X : T) return T;
   function Cvt_T_G (X : T) return T;
   function Cvt_T_F (X : T) return S;

   pragma Import (C, Cvt_G_T, "OTS$CVT_FLOAT_G_T");
   pragma Import (C, Cvt_T_G, "OTS$CVT_FLOAT_T_G");
   pragma Import (C, Cvt_T_F, "OTS$CVT_FLOAT_T_F");

   --  In each of the conversion routines that are done with OTS calls,
   --  we define variables of the corresponding IEEE type so that they are
   --  passed and kept in the proper register class.

   ------------
   -- D_To_G --
   ------------

   function D_To_G (X : D) return G is
      A, B : T;
      C : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), D'Asm_Input ("m", X));
      Asm ("cvtdg %1,%0", T'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      Asm ("stg %1,%0", G'Asm_Output ("=m", C), T'Asm_Input ("f", B));
      return C;
   end D_To_G;

   ------------
   -- F_To_G --
   ------------

   function F_To_G (X : F) return G is
      A : T;
      B : G;

   begin
      Asm ("ldf %0,%1", T'Asm_Output ("=f", A), F'Asm_Input ("m", X));
      Asm ("stg %1,%0", G'Asm_Output ("=m", B), T'Asm_Input ("f", A));
      return B;
   end F_To_G;

   ------------
   -- F_To_S --
   ------------

   function F_To_S (X : F) return S is
      A : T;
      B : S;

   begin
      --  Because converting to a wider FP format is a no-op, we say
      --  A is 64-bit even though we are loading 32 bits into it.
      Asm ("ldf %0,%1", T'Asm_Output ("=f", A), F'Asm_Input ("m", X));

      B := S (Cvt_G_T (A));
      return B;
   end F_To_S;

   ------------
   -- G_To_D --
   ------------

   function G_To_D (X : G) return D is
      A, B : T;
      C : D;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      Asm ("cvtgd %1,%0", T'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      Asm ("stg %1,%0", D'Asm_Output ("=m", C), T'Asm_Input ("f", B));
      return C;
   end G_To_D;

   ------------
   -- G_To_F --
   ------------

   function G_To_F (X : G) return F is
      A : T;
      B : S;
      C : F;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      Asm ("cvtgf %1,%0", S'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      Asm ("stf %1,%0", F'Asm_Output ("=m", C), S'Asm_Input ("f", B));
      return C;
   end G_To_F;

   ------------
   -- G_To_Q --
   ------------

   function G_To_Q (X : G) return Q is
      A : T;
      B : Q;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      Asm ("cvtgq %1,%0", Q'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      return B;
   end G_To_Q;

   ------------
   -- G_To_T --
   ------------

   function G_To_T (X : G) return T is
      A, B : T;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      B := Cvt_G_T (A);
      return B;
   end G_To_T;

   ------------
   -- F_To_Q --
   ------------

   function F_To_Q (X : F) return Q is
   begin
      return G_To_Q (F_To_G (X));
   end F_To_Q;

   ------------
   -- Q_To_F --
   ------------

   function Q_To_F (X : Q) return F is
      A : S;
      B : F;

   begin
      Asm ("cvtqf %1,%0", S'Asm_Output ("=f", A), Q'Asm_Input ("f", X));
      Asm ("stf %1,%0", F'Asm_Output ("=m", B), S'Asm_Input ("f", A));
      return B;
   end Q_To_F;

   ------------
   -- Q_To_G --
   ------------

   function Q_To_G (X : Q) return G is
      A : T;
      B : G;

   begin
      Asm ("cvtqg %1,%0", T'Asm_Output ("=f", A), Q'Asm_Input ("f", X));
      Asm ("stg %1,%0", G'Asm_Output ("=m", B), T'Asm_Input ("f", A));
      return B;
   end Q_To_G;

   ------------
   -- S_To_F --
   ------------

   function S_To_F (X : S) return F is
      A : S;
      B : F;

   begin
      A := Cvt_T_F (T (X));
      Asm ("stf %1,%0", F'Asm_Output ("=m", B), S'Asm_Input ("f", A));
      return B;
   end S_To_F;

   ------------
   -- T_To_D --
   ------------

   function T_To_D (X : T) return D is
   begin
      return G_To_D (T_To_G (X));
   end T_To_D;

   ------------
   -- T_To_G --
   ------------

   function T_To_G (X : T) return G is
      A : T;
      B : G;

   begin
      A := Cvt_T_G (X);
      Asm ("stg %1,%0", G'Asm_Output ("=m", B), T'Asm_Input ("f", A));
      return B;
   end T_To_G;

   -----------
   -- Abs_F --
   -----------

   function Abs_F (X : F) return F is
      A, B : S;
      C : F;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", A), F'Asm_Input ("m", X));
      Asm ("cpys $f31,%1,%0", S'Asm_Output ("=f", B), S'Asm_Input ("f", A));
      Asm ("stf %1,%0", F'Asm_Output ("=m", C), S'Asm_Input ("f", B));
      return C;
   end Abs_F;

   -----------
   -- Abs_G --
   -----------

   function Abs_G (X : G) return G is
      A, B : T;
      C : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      Asm ("cpys $f31,%1,%0", T'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      Asm ("stg %1,%0", G'Asm_Output ("=m", C), T'Asm_Input ("f", B));
      return C;
   end Abs_G;

   -----------
   -- Add_F --
   -----------

   function Add_F (X, Y : F) return F is
      X1, Y1, R : S;
      R1 : F;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("addf %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      Asm ("stf %1,%0", F'Asm_Output ("=m", R1), S'Asm_Input ("f", R));
      return R1;
   end Add_F;

   -----------
   -- Add_G --
   -----------

   function Add_G (X, Y : G) return G is
      X1, Y1, R : T;
      R1 : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("addg %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      Asm ("stg %1,%0", G'Asm_Output ("=m", R1), T'Asm_Input ("f", R));
      return R1;
   end Add_G;

   --------------------
   -- Debug_Output_D --
   --------------------

   procedure Debug_Output_D (Arg : D) is
   begin
      Put (D'Image (Arg));
   end Debug_Output_D;

   --------------------
   -- Debug_Output_F --
   --------------------

   procedure Debug_Output_F (Arg : F) is
   begin
      Put (F'Image (Arg));
   end Debug_Output_F;

   --------------------
   -- Debug_Output_G --
   --------------------

   procedure Debug_Output_G (Arg : G) is
   begin
      Put (G'Image (Arg));
   end Debug_Output_G;

   --------------------
   -- Debug_String_D --
   --------------------

   Debug_String_Buffer : String (1 .. 32);
   --  Buffer used by all Debug_String_x routines for returning result

   function Debug_String_D (Arg : D) return System.Address is
      Image_String : constant String := D'Image (Arg) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;

   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_D;

   --------------------
   -- Debug_String_F --
   --------------------

   function Debug_String_F (Arg : F) return System.Address is
      Image_String : constant String := F'Image (Arg) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;

   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_F;

   --------------------
   -- Debug_String_G --
   --------------------

   function Debug_String_G (Arg : G) return System.Address is
      Image_String : constant String := G'Image (Arg) & ASCII.NUL;
      Image_Size   : constant Integer := Image_String'Length;

   begin
      Debug_String_Buffer (1 .. Image_Size) := Image_String;
      return Debug_String_Buffer (1)'Address;
   end Debug_String_G;

   -----------
   -- Div_F --
   -----------

   function Div_F (X, Y : F) return F is
      X1, Y1, R : S;

      R1 : F;
   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("divf %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      Asm ("stf %1,%0", F'Asm_Output ("=m", R1), S'Asm_Input ("f", R));
      return R1;
   end Div_F;

   -----------
   -- Div_G --
   -----------

   function Div_G (X, Y : G) return G is
      X1, Y1, R : T;
      R1 : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("divg %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      Asm ("stg %1,%0", G'Asm_Output ("=m", R1), T'Asm_Input ("f", R));
      return R1;
   end Div_G;

   ----------
   -- Eq_F --
   ----------

   function Eq_F (X, Y : F) return Boolean is
      X1, Y1, R : S;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("cmpgeq %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Eq_F;

   ----------
   -- Eq_G --
   ----------

   function Eq_G (X, Y : G) return Boolean is
      X1, Y1, R : T;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("cmpgeq %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Eq_G;

   ----------
   -- Le_F --
   ----------

   function Le_F (X, Y : F) return Boolean is
      X1, Y1, R : S;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("cmpgle %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Le_F;

   ----------
   -- Le_G --
   ----------

   function Le_G (X, Y : G) return Boolean is
      X1, Y1, R : T;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("cmpgle %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Le_G;

   ----------
   -- Lt_F --
   ----------

   function Lt_F (X, Y : F) return Boolean is
      X1, Y1, R : S;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("cmpglt %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Lt_F;

   ----------
   -- Lt_G --
   ----------

   function Lt_G (X, Y : G) return Boolean is
      X1, Y1, R : T;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("cmpglt %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      return R /= 0.0;
   end Lt_G;

   -----------
   -- Mul_F --
   -----------

   function Mul_F (X, Y : F) return F is
      X1, Y1, R : S;
      R1 : F;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("mulf %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      Asm ("stf %1,%0", F'Asm_Output ("=m", R1), S'Asm_Input ("f", R));
      return R1;
   end Mul_F;

   -----------
   -- Mul_G --
   -----------

   function Mul_G (X, Y : G) return G is
      X1, Y1, R : T;
      R1 : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("mulg %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      Asm ("stg %1,%0", G'Asm_Output ("=m", R1), T'Asm_Input ("f", R));
      return R1;
   end Mul_G;

   -----------
   -- Neg_F --
   -----------

   function Neg_F (X : F) return F is
      A, B : S;
      C : F;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", A), F'Asm_Input ("m", X));
      Asm ("cpysn %1,%1,%0", S'Asm_Output ("=f", B), S'Asm_Input ("f", A));
      Asm ("stf %1,%0", F'Asm_Output ("=m", C), S'Asm_Input ("f", B));
      return C;
   end Neg_F;

   -----------
   -- Neg_G --
   -----------

   function Neg_G (X : G) return G is
      A, B : T;
      C : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", A), G'Asm_Input ("m", X));
      Asm ("cpysn %1,%1,%0", T'Asm_Output ("=f", B), T'Asm_Input ("f", A));
      Asm ("stg %1,%0", G'Asm_Output ("=m", C), T'Asm_Input ("f", B));
      return C;
   end Neg_G;

   --------
   -- pd --
   --------

   procedure pd (Arg : D) is
   begin
      Put_Line (D'Image (Arg));
   end pd;

   --------
   -- pf --
   --------

   procedure pf (Arg : F) is
   begin
      Put_Line (F'Image (Arg));
   end pf;

   --------
   -- pg --
   --------

   procedure pg (Arg : G) is
   begin
      Put_Line (G'Image (Arg));
   end pg;

   -----------
   -- Sub_F --
   -----------

   function Sub_F (X, Y : F) return F is
      X1, Y1, R : S;
      R1 : F;

   begin
      Asm ("ldf %0,%1", S'Asm_Output ("=f", X1), F'Asm_Input ("m", X));
      Asm ("ldf %0,%1", S'Asm_Output ("=f", Y1), F'Asm_Input ("m", Y));
      Asm ("subf %1,%2,%0", S'Asm_Output ("=f", R),
           (S'Asm_Input ("f", X1), S'Asm_Input ("f", Y1)));
      Asm ("stf %1,%0", F'Asm_Output ("=m", R1), S'Asm_Input ("f", R));
      return R1;
   end Sub_F;

   -----------
   -- Sub_G --
   -----------

   function Sub_G (X, Y : G) return G is
      X1, Y1, R : T;
      R1 : G;

   begin
      Asm ("ldg %0,%1", T'Asm_Output ("=f", X1), G'Asm_Input ("m", X));
      Asm ("ldg %0,%1", T'Asm_Output ("=f", Y1), G'Asm_Input ("m", Y));
      Asm ("subg %1,%2,%0", T'Asm_Output ("=f", R),
           (T'Asm_Input ("f", X1), T'Asm_Input ("f", Y1)));
      Asm ("stg %1,%0", G'Asm_Output ("=m", R1), T'Asm_Input ("f", R));
      return R1;
   end Sub_G;

end System.Vax_Float_Operations;
