------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . V A X _ F L O A T _ O P E R A T I O N S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2009, Free Software Foundation, Inc.         --
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

--  This is a dummy body for use on non-Alpha systems so that the library
--  can compile. This dummy version uses ordinary conversions and other
--  arithmetic operations. It is used only for testing purposes in the
--  case where the -gnatdm switch is used to force testing of VMS features
--  on non-VMS systems.

with System.IO;

package body System.Vax_Float_Operations is
   pragma Warnings (Off);
   --  Warnings about infinite recursion when the -gnatdm switch is used

   -----------
   -- Abs_F --
   -----------

   function Abs_F (X : F) return F is
   begin
      return abs X;
   end Abs_F;

   -----------
   -- Abs_G --
   -----------

   function Abs_G (X : G) return G is
   begin
      return abs X;
   end Abs_G;

   -----------
   -- Add_F --
   -----------

   function Add_F (X, Y : F) return F is
   begin
      return X + Y;
   end Add_F;

   -----------
   -- Add_G --
   -----------

   function Add_G (X, Y : G) return G is
   begin
      return X + Y;
   end Add_G;

   ------------
   -- D_To_G --
   ------------

   function D_To_G (X : D) return G is
   begin
      return G (X);
   end D_To_G;

   --------------------
   -- Debug_Output_D --
   --------------------

   procedure Debug_Output_D (Arg : D) is
   begin
      System.IO.Put (D'Image (Arg));
   end Debug_Output_D;

   --------------------
   -- Debug_Output_F --
   --------------------

   procedure Debug_Output_F (Arg : F) is
   begin
      System.IO.Put (F'Image (Arg));
   end Debug_Output_F;

   --------------------
   -- Debug_Output_G --
   --------------------

   procedure Debug_Output_G (Arg : G) is
   begin
      System.IO.Put (G'Image (Arg));
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
   begin
      return X / Y;
   end Div_F;

   -----------
   -- Div_G --
   -----------

   function Div_G (X, Y : G) return G is
   begin
      return X / Y;
   end Div_G;

   ----------
   -- Eq_F --
   ----------

   function Eq_F (X, Y : F) return Boolean is
   begin
      return X = Y;
   end Eq_F;

   ----------
   -- Eq_G --
   ----------

   function Eq_G (X, Y : G) return Boolean is
   begin
      return X = Y;
   end Eq_G;

   ------------
   -- F_To_G --
   ------------

   function F_To_G (X : F) return G is
   begin
      return G (X);
   end F_To_G;

   ------------
   -- F_To_Q --
   ------------

   function F_To_Q (X : F) return Q is
   begin
      return Q (X);
   end F_To_Q;

   ------------
   -- F_To_S --
   ------------

   function F_To_S (X : F) return S is
   begin
      return S (X);
   end F_To_S;

   ------------
   -- G_To_D --
   ------------

   function G_To_D (X : G) return D is
   begin
      return D (X);
   end G_To_D;

   ------------
   -- G_To_F --
   ------------

   function G_To_F (X : G) return F is
   begin
      return F (X);
   end G_To_F;

   ------------
   -- G_To_Q --
   ------------

   function G_To_Q (X : G) return Q is
   begin
      return Q (X);
   end G_To_Q;

   ------------
   -- G_To_T --
   ------------

   function G_To_T (X : G) return T is
   begin
      return T (X);
   end G_To_T;

   ----------
   -- Le_F --
   ----------

   function Le_F (X, Y : F) return Boolean is
   begin
      return X <= Y;
   end Le_F;

   ----------
   -- Le_G --
   ----------

   function Le_G (X, Y : G) return Boolean is
   begin
      return X <= Y;
   end Le_G;

   ----------
   -- Lt_F --
   ----------

   function Lt_F (X, Y : F) return Boolean is
   begin
      return X < Y;
   end Lt_F;

   ----------
   -- Lt_G --
   ----------

   function Lt_G (X, Y : G) return Boolean is
   begin
      return X < Y;
   end Lt_G;

   -----------
   -- Mul_F --
   -----------

   function Mul_F (X, Y : F) return F is
   begin
      return X * Y;
   end Mul_F;

   -----------
   -- Mul_G --
   -----------

   function Mul_G (X, Y : G) return G is
   begin
      return X * Y;
   end Mul_G;

   ----------
   -- Ne_F --
   ----------

   function Ne_F (X, Y : F) return Boolean is
   begin
      return X /= Y;
   end Ne_F;

   ----------
   -- Ne_G --
   ----------

   function Ne_G (X, Y : G) return Boolean is
   begin
      return X /= Y;
   end Ne_G;

   -----------
   -- Neg_F --
   -----------

   function Neg_F (X : F) return F is
   begin
      return -X;
   end Neg_F;

   -----------
   -- Neg_G --
   -----------

   function Neg_G (X : G) return G is
   begin
      return -X;
   end Neg_G;

   --------
   -- pd --
   --------

   procedure pd (Arg : D) is
   begin
      System.IO.Put_Line (D'Image (Arg));
   end pd;

   --------
   -- pf --
   --------

   procedure pf (Arg : F) is
   begin
      System.IO.Put_Line (F'Image (Arg));
   end pf;

   --------
   -- pg --
   --------

   procedure pg (Arg : G) is
   begin
      System.IO.Put_Line (G'Image (Arg));
   end pg;

   ------------
   -- Q_To_F --
   ------------

   function Q_To_F (X : Q) return F is
   begin
      return F (X);
   end Q_To_F;

   ------------
   -- Q_To_G --
   ------------

   function Q_To_G (X : Q) return G is
   begin
      return G (X);
   end Q_To_G;

   ------------
   -- S_To_F --
   ------------

   function S_To_F (X : S) return F is
   begin
      return F (X);
   end S_To_F;

   --------------
   -- Return_D --
   --------------

   function Return_D (X : D) return D is
   begin
      return X;
   end Return_D;

   --------------
   -- Return_F --
   --------------

   function Return_F (X : F) return F is
   begin
      return X;
   end Return_F;

   --------------
   -- Return_G --
   --------------

   function Return_G (X : G) return G is
   begin
      return X;
   end Return_G;

   -----------
   -- Sub_F --
   -----------

   function Sub_F (X, Y : F) return F is
   begin
      return X - Y;
   end Sub_F;

   -----------
   -- Sub_G --
   -----------

   function Sub_G (X, Y : G) return G is
   begin
      return X - Y;
   end Sub_G;

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
   begin
      return G (X);
   end T_To_G;

   -------------
   -- Valid_D --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_D (Arg : D) return Boolean is
      Val : constant T := G_To_T (D_To_G (Arg));
   begin
      return Val'Valid;
   end Valid_D;

   -------------
   -- Valid_F --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_F (Arg : F) return Boolean is
      Val : constant S := F_To_S (Arg);
   begin
      return Val'Valid;
   end Valid_F;

   -------------
   -- Valid_G --
   -------------

   --  For now, convert to IEEE and do Valid test on result. This is not quite
   --  accurate, but is good enough in practice.

   function Valid_G (Arg : G) return Boolean is
      Val : constant T := G_To_T (Arg);
   begin
      return Val'Valid;
   end Valid_G;

end System.Vax_Float_Operations;
