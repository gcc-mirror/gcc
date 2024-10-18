------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . E X P O N N                         --
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

package body System.Exponn
  with SPARK_Mode
is

   --  Preconditions, postconditions, ghost code, loop invariants and
   --  assertions in this unit are meant for analysis only, not for run-time
   --  checking, as it would be too costly otherwise. This is enforced by
   --  setting the assertion policy to Ignore.

   pragma Assertion_Policy (Pre            => Ignore,
                            Post           => Ignore,
                            Ghost          => Ignore,
                            Loop_Invariant => Ignore,
                            Assert         => Ignore);

   --  Local lemmas

   procedure Lemma_Exp_Expand (A : Big_Integer; Exp : Natural)
   with
     Ghost,
     Pre  => A /= 0,
     Post =>
       (if Exp rem 2 = 0 then
          A ** Exp = A ** (Exp / 2) * A ** (Exp / 2)
        else
          A ** Exp = A ** (Exp / 2) * A ** (Exp / 2) * A);

   procedure Lemma_Exp_In_Range (A : Big_Integer; Exp : Positive)
   with
     Ghost,
     Pre  => In_Int_Range (A ** Exp * A ** Exp),
     Post => In_Int_Range (A * A);

   procedure Lemma_Exp_Not_Zero (A : Big_Integer; Exp : Natural)
   with
     Ghost,
     Pre  => A /= 0,
     Post => A ** Exp /= 0;

   procedure Lemma_Exp_Positive (A : Big_Integer; Exp : Natural)
   with
     Ghost,
     Pre  => A /= 0
       and then Exp rem 2 = 0,
     Post => A ** Exp > 0;

   procedure Lemma_Mult_In_Range (X, Y, Z : Big_Integer)
   with
     Ghost,
     Pre  => Y /= 0
       and then not (X = -Big (Int'First) and Y = -1)
       and then X * Y = Z
       and then In_Int_Range (Z),
     Post => In_Int_Range (X);

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Lemma_Exp_Not_Zero (A : Big_Integer; Exp : Natural) is null;
   procedure Lemma_Mult_In_Range (X, Y, Z : Big_Integer) is null;

   -----------
   -- Expon --
   -----------

   function Expon (Left : Int; Right : Natural) return Int is

      --  Note that negative exponents get a constraint error because the
      --  subtype of the Right argument (the exponent) is Natural.

      Result : Int     := 1;
      Factor : Int     := Left;
      Exp    : Natural := Right;

      Rest : Big_Integer with Ghost;
      --  Ghost variable to hold Factor**Exp between Exp and Factor updates

   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "early returns for performance");

      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: for compilation only, it is not worth special casing base
      --  values -1, 0, +1 since the expander does this when the base is a
      --  literal, and other cases will be extremely rare. But for proof,
      --  special casing zero in both positions makes ghost code and lemmas
      --  simpler, so we do it.

      if Right = 0 then
         return 1;
      elsif Left = 0 then
         return 0;
      end if;

      loop
         pragma Loop_Invariant (Exp > 0);
         pragma Loop_Invariant (Factor /= 0);
         pragma Loop_Invariant
           (Big (Result) * Big (Factor) ** Exp = Big (Left) ** Right);
         pragma Loop_Variant (Decreases => Exp);

         if Exp rem 2 /= 0 then
            declare
               pragma Suppress (Overflow_Check);
            begin
               pragma Assert
                 (Big (Factor) ** Exp
                  = Big (Factor) * Big (Factor) ** (Exp - 1));
               Lemma_Exp_Positive (Big (Factor), Exp - 1);
               Lemma_Mult_In_Range (Big (Result) * Big (Factor),
                                    Big (Factor) ** (Exp - 1),
                                    Big (Left) ** Right);

               Result := Result * Factor;
            end;
         end if;

         Lemma_Exp_Expand (Big (Factor), Exp);

         Exp := Exp / 2;
         exit when Exp = 0;

         Rest := Big (Factor) ** Exp;
         pragma Assert
           (Big (Result) * (Rest * Rest) = Big (Left) ** Right);

         declare
            pragma Suppress (Overflow_Check);
         begin
            Lemma_Mult_In_Range (Rest * Rest,
                                 Big (Result),
                                 Big (Left) ** Right);
            Lemma_Exp_In_Range (Big (Factor), Exp);

            Factor := Factor * Factor;
         end;

         pragma Assert (Big (Factor) ** Exp = Rest * Rest);
      end loop;

      pragma Assert (Big (Result) = Big (Left) ** Right);

      return Result;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Expon;

   ----------------------
   -- Lemma_Exp_Expand --
   ----------------------

   procedure Lemma_Exp_Expand (A : Big_Integer; Exp : Natural) is

      procedure Lemma_Exp_Distribution (Exp_1, Exp_2 : Natural) with
        Pre  => A /= 0 and then Natural'Last - Exp_2 >= Exp_1,
        Post => A ** (Exp_1 + Exp_2) = A ** (Exp_1) * A ** (Exp_2);

      ----------------------------
      -- Lemma_Exp_Distribution --
      ----------------------------

      procedure Lemma_Exp_Distribution (Exp_1, Exp_2 : Natural) is null;

   begin
      if Exp rem 2 = 0 then
         pragma Assert (Exp = Exp / 2 + Exp / 2);
      else
         pragma Assert (Exp = Exp / 2 + Exp / 2 + 1);
         Lemma_Exp_Distribution (Exp / 2, Exp / 2 + 1);
         Lemma_Exp_Distribution (Exp / 2, 1);
      end if;
   end Lemma_Exp_Expand;

   ------------------------
   -- Lemma_Exp_In_Range --
   ------------------------

   procedure Lemma_Exp_In_Range (A : Big_Integer; Exp : Positive) is
   begin
      if A /= 0 and Exp /= 1 then
         pragma Assert (A ** Exp = A * A ** (Exp - 1));
         Lemma_Mult_In_Range
           (A * A, A ** (Exp - 1) * A ** (Exp - 1), A ** Exp * A ** Exp);
      end if;
   end Lemma_Exp_In_Range;

   ------------------------
   -- Lemma_Exp_Positive --
   ------------------------

   procedure Lemma_Exp_Positive (A : Big_Integer; Exp : Natural) is
   begin
      if Exp = 0 then
         pragma Assert (A ** Exp = 1);
      else
         pragma Assert (Exp = 2 * (Exp / 2));
         pragma Assert (A ** Exp = A ** (Exp / 2) * A ** (Exp / 2));
         pragma Assert (A ** Exp = (A ** (Exp / 2)) ** 2);
         Lemma_Exp_Not_Zero (A, Exp / 2);
      end if;
   end Lemma_Exp_Positive;

end System.Exponn;
