------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W I D T H _ I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
use Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

function System.Width_I (Lo, Hi : Int) return Natural is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost          => Ignore,
                            Loop_Invariant => Ignore,
                            Assert         => Ignore);

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Signed_Conversion is new Signed_Conversions (Int => Int);

   function Big (Arg : Int) return Big_Integer renames
     Signed_Conversion.To_Big_Integer;

   --  Maximum value of exponent for 10 that fits in Uns'Base
   function Max_Log10 return Natural is
     (case Int'Base'Size is
        when 8   => 2,
        when 16  => 4,
        when 32  => 9,
        when 64  => 19,
        when 128 => 38,
        when others => raise Program_Error)
   with Ghost;

   ------------------
   -- Local Lemmas --
   ------------------

   procedure Lemma_Lower_Mult (A, B, C : Big_Natural)
   with
     Ghost,
     Pre  => A <= B,
     Post => A * C <= B * C;

   procedure Lemma_Div_Commutation (X, Y : Int)
   with
     Ghost,
     Pre  => X >= 0 and Y > 0,
     Post => Big (X) / Big (Y) = Big (X / Y);

   procedure Lemma_Div_Twice (X : Big_Natural; Y, Z : Big_Positive)
   with
     Ghost,
     Post => X / Y / Z = X / (Y * Z);

   ----------------------
   -- Lemma_Lower_Mult --
   ----------------------

   procedure Lemma_Lower_Mult (A, B, C : Big_Natural) is null;

   ---------------------------
   -- Lemma_Div_Commutation --
   ---------------------------

   procedure Lemma_Div_Commutation (X, Y : Int) is null;

   ---------------------
   -- Lemma_Div_Twice --
   ---------------------

   procedure Lemma_Div_Twice (X : Big_Natural; Y, Z : Big_Positive) is
      XY  : constant Big_Natural := X / Y;
      YZ  : constant Big_Natural := Y * Z;
      XYZ : constant Big_Natural := X / Y / Z;
      R   : constant Big_Natural := (XY rem Z) * Y + (X rem Y);
   begin
      pragma Assert (X = XY * Y + (X rem Y));
      pragma Assert (XY = XY / Z * Z + (XY rem Z));
      pragma Assert (X = XYZ * YZ + R);
      pragma Assert ((XY rem Z) * Y <= (Z - 1) * Y);
      pragma Assert (R <= YZ - 1);
      pragma Assert (X / YZ = (XYZ * YZ + R) / YZ);
      pragma Assert (X / YZ = XYZ + R / YZ);
   end Lemma_Div_Twice;

   --  Local variables

   W : Natural;
   T : Int;

   --  Local ghost variables

   Max_W  : constant Natural := Max_Log10 with Ghost;
   Big_10 : constant Big_Integer := Big (10) with Ghost;

   Pow    : Big_Integer := 1 with Ghost;
   T_Init : constant Int :=
     Int'Max (abs (Int'Max (Lo, Int'First + 1)),
              abs (Int'Max (Hi, Int'First + 1)))
     with Ghost;

--  Start of processing for System.Width_I

begin
   if Lo > Hi then
      return 0;

   else
      --  Minimum value is 2, one for sign, one for digit

      W := 2;

      --  Get max of absolute values, but avoid bomb if we have the maximum
      --  negative number (note that First + 1 has same digits as First)

      T := Int'Max (
             abs (Int'Max (Lo, Int'First + 1)),
             abs (Int'Max (Hi, Int'First + 1)));

      --  Increase value if more digits required

      while T >= 10 loop
         Lemma_Div_Commutation (T, 10);
         Lemma_Div_Twice (Big (T_Init), Big_10 ** (W - 2), Big_10);

         T := T / 10;
         W := W + 1;
         Pow := Pow * 10;

         pragma Loop_Invariant (T >= 0);
         pragma Loop_Invariant (W in 3 .. Max_W + 3);
         pragma Loop_Invariant (Pow = Big_10 ** (W - 2));
         pragma Loop_Invariant (Big (T) = Big (T_Init) / Pow);
         pragma Loop_Variant (Decreases => T);
      end loop;

      declare
         F : constant Big_Positive := Big_10 ** (W - 2) with Ghost;
         Q : constant Big_Natural := Big (T_Init) / F with Ghost;
         R : constant Big_Natural := Big (T_Init) rem F with Ghost;
      begin
         pragma Assert (Q < Big_10);
         pragma Assert (Big (T_Init) = Q * F + R);
         Lemma_Lower_Mult (Q, Big (9), F);
         pragma Assert (Big (T_Init) <= Big (9) * F + F - 1);
         pragma Assert (Big (T_Init) < Big_10 * F);
         pragma Assert (Big_10 * F = Big_10 ** (W - 1));
      end;

      --  This is an expression of the functional postcondition for Width_I,
      --  which cannot be expressed readily as a postcondition as this would
      --  require making the instantiation Signed_Conversion and function Big
      --  available from the spec.

      pragma Assert (Big (Int'Max (Lo, Int'First + 1)) < Big_10 ** (W - 1));
      pragma Assert (Big (Int'Max (Hi, Int'First + 1)) < Big_10 ** (W - 1));

      return W;
   end if;

end System.Width_I;
