------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W I D T H _ U                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

package body System.Width_U is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Subprogram_Variant => Ignore);

   function Width (Lo, Hi : Uns) return Natural is

      --  Ghost code, loop invariants and assertions in this unit are meant for
      --  analysis only, not for run-time checking, as it would be too costly
      --  otherwise. This is enforced by setting the assertion policy to
      --  Ignore.

      pragma Assertion_Policy (Ghost          => Ignore,
                               Loop_Invariant => Ignore,
                               Assert         => Ignore);

      ------------------
      -- Local Lemmas --
      ------------------

      procedure Lemma_Lower_Mult (A, B, C : Big_Natural)
      with
        Ghost,
        Pre  => A <= B,
        Post => A * C <= B * C;

      procedure Lemma_Div_Commutation (X, Y : Uns)
      with
        Ghost,
        Pre  => Y /= 0,
        Post => Big (X) / Big (Y) = Big (X / Y);

      procedure Lemma_Div_Twice (X : Big_Natural; Y, Z : Big_Positive)
      with
        Ghost,
        Post => X / Y / Z = X / (Y * Z);

      procedure Lemma_Euclidian (V, Q, F, R : Big_Integer)
      with
        Ghost,
        Pre  => F > 0 and then Q = V / F and then R = V rem F,
        Post => V = Q * F + R;
      --  Ghost lemma to prove the relation between the quotient/remainder of
      --  division by F and the value V.

      ----------------------
      -- Lemma_Lower_Mult --
      ----------------------

      procedure Lemma_Lower_Mult (A, B, C : Big_Natural) is null;

      ---------------------------
      -- Lemma_Div_Commutation --
      ---------------------------

      procedure Lemma_Div_Commutation (X, Y : Uns) is null;

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

      ---------------------
      -- Lemma_Euclidian --
      ---------------------

      procedure Lemma_Euclidian (V, Q, F, R : Big_Integer) is null;

      --  Local variables

      W : Natural;
      T : Uns;

      --  Local ghost variables

      Max_W  : constant Natural := Max_Log10 with Ghost;
      Pow    : Big_Integer := 1 with Ghost;
      T_Init : constant Uns := Uns'Max (Lo, Hi) with Ghost;

   --  Start of processing for System.Width_U

   begin
      if Lo > Hi then
         return 0;

      else
         --  Minimum value is 2, one for space, one for digit

         W := 2;

         --  Get max of absolute values

         T := Uns'Max (Lo, Hi);

         --  Increase value if more digits required

         while T >= 10 loop
            Lemma_Div_Commutation (T, 10);
            Lemma_Div_Twice (Big (T_Init), Big_10 ** (W - 2), Big_10);

            T := T / 10;
            W := W + 1;
            Pow := Pow * 10;

            pragma Loop_Invariant (W in 3 .. Max_W + 2);
            pragma Loop_Invariant (Pow = Big_10 ** (W - 2));
            pragma Loop_Invariant (Big (T) = Big (T_Init) / Pow);
            pragma Loop_Variant (Decreases => T);
         end loop;

         declare
            F : constant Big_Integer := Big_10 ** (W - 2) with Ghost;
            Q : constant Big_Integer := Big (T_Init) / F with Ghost;
            R : constant Big_Integer := Big (T_Init) rem F with Ghost;
         begin
            pragma Assert (Q < Big_10);
            Lemma_Euclidian (Big (T_Init), Q, F, R);
            Lemma_Lower_Mult (Q, Big (9), F);
            pragma Assert (Big (T_Init) <= Big (9) * F + F - 1);
            pragma Assert (Big (T_Init) < Big_10 * F);
            pragma Assert (Big_10 * F = Big_10 ** (W - 1));
         end;

         return W;
      end if;
   end Width;

end System.Width_U;
