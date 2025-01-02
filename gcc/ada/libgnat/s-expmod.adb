------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X P _ M O D                        --
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

--  Preconditions, postconditions, ghost code, loop invariants and assertions
--  in this unit are meant for analysis only, not for run-time checking, as it
--  would be too costly otherwise. This is enforced by setting the assertion
--  policy to Ignore.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
use Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

package body System.Exp_Mod
  with SPARK_Mode
is
   use System.Unsigned_Types;

   --  Local lemmas

   procedure Lemma_Add_Mod (X, Y : Big_Natural; B : Big_Positive)
   with
     Ghost,
     Post => (X + Y) mod B = ((X mod B) + (Y mod B)) mod B;

   procedure Lemma_Exp_Expand (A : Big_Integer; Exp : Natural)
   with
     Ghost,
     Post =>
       (if Exp rem 2 = 0 then
          A ** Exp = A ** (Exp / 2) * A ** (Exp / 2)
        else
          A ** Exp = A ** (Exp / 2) * A ** (Exp / 2) * A);

   procedure Lemma_Exp_Mod (A : Big_Natural; Exp : Natural; B : Big_Positive)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Exp),
     Post => ((A mod B) ** Exp) mod B = (A ** Exp) mod B;

   procedure Lemma_Mod_Ident (A : Big_Natural; B : Big_Positive)
   with
     Ghost,
     Pre => A < B,
     Post => A mod B = A;

   procedure Lemma_Mod_Mod (A : Big_Integer; B : Big_Positive)
   with
     Ghost,
     Post => A mod B mod B = A mod B;

   procedure Lemma_Mult_Div (X : Big_Natural; Y : Big_Positive)
   with
     Ghost,
     Post => X * Y / Y = X;

   procedure Lemma_Mult_Mod (X, Y : Big_Natural; B : Big_Positive)
   with
     Ghost,
     --  The following subprogram variant can be added as soon as supported
     --  Subprogram_Variant => (Decreases => Y),
     Post => (X * Y) mod B = ((X mod B) * (Y mod B)) mod B;

   -----------------------------
   -- Local lemma null bodies --
   -----------------------------

   procedure Lemma_Mod_Ident (A : Big_Natural; B : Big_Positive) is null;
   procedure Lemma_Mod_Mod (A : Big_Integer; B : Big_Positive) is null;
   procedure Lemma_Mult_Div (X : Big_Natural; Y : Big_Positive) is null;

   -------------------
   -- Lemma_Add_Mod --
   -------------------

   procedure Lemma_Add_Mod (X, Y : Big_Natural; B : Big_Positive) is

      procedure Lemma_Euclidean_Mod (Q, F, R : Big_Natural) with
        Pre  => F /= 0,
        Post => (Q * F + R) mod F = R mod F,
        Subprogram_Variant => (Decreases => Q);

      -------------------------
      -- Lemma_Euclidean_Mod --
      -------------------------

      procedure Lemma_Euclidean_Mod (Q, F, R : Big_Natural) is
      begin
         if Q > 0 then
            Lemma_Euclidean_Mod (Q - 1, F, R);
         end if;
      end Lemma_Euclidean_Mod;

      --  Local variables

      Left  : constant Big_Natural := (X + Y) mod B;
      Right : constant Big_Natural := ((X mod B) + (Y mod B)) mod B;
      XQuot : constant Big_Natural := X / B;
      YQuot : constant Big_Natural := Y / B;
      AQuot : constant Big_Natural := (X mod B + Y mod B) / B;
   begin
      if Y /= 0 and B > 1 then
         pragma Assert (X = XQuot * B + X mod B);
         pragma Assert (Y = YQuot * B + Y mod B);
         pragma Assert
           (Left = ((XQuot + YQuot) * B + X mod B + Y mod B) mod B);
         pragma Assert (X mod B + Y mod B = AQuot * B + Right);
         pragma Assert (Left = ((XQuot + YQuot + AQuot) * B + Right) mod B);
         Lemma_Euclidean_Mod (XQuot + YQuot + AQuot, B, Right);
         pragma Assert (Left = (Right mod B));
         pragma Assert (Left = Right);
      end if;
   end Lemma_Add_Mod;

   ----------------------
   -- Lemma_Exp_Expand --
   ----------------------

   procedure Lemma_Exp_Expand (A : Big_Integer; Exp : Natural) is

      procedure Lemma_Exp_Distribution (Exp_1, Exp_2 : Natural) with
        Pre  => Natural'Last - Exp_2 >= Exp_1,
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

   -------------------
   -- Lemma_Exp_Mod --
   -------------------

   procedure Lemma_Exp_Mod (A : Big_Natural; Exp : Natural; B : Big_Positive)
   is
   begin
      if Exp /= 0 then
         declare
            Left  : constant Big_Integer := ((A mod B) ** Exp) mod B;
            Right : constant Big_Integer := (A ** Exp) mod B;
         begin
            Lemma_Mult_Mod (A mod B, (A mod B) ** (Exp - 1), B);
            Lemma_Mod_Mod (A, B);
            Lemma_Exp_Mod (A, Exp - 1, B);
            Lemma_Mult_Mod (A, A ** (Exp - 1), B);
            pragma Assert
              ((A mod B) * (A mod B) ** (Exp - 1) = (A mod B) ** Exp);
            pragma Assert (A * A ** (Exp - 1) = A ** Exp);
            pragma Assert (Left = Right);
         end;
      end if;
   end Lemma_Exp_Mod;

   --------------------
   -- Lemma_Mult_Mod --
   --------------------

   procedure Lemma_Mult_Mod (X, Y : Big_Natural; B : Big_Positive) is
      Left : constant Big_Natural := (X * Y) mod B;
      Right : constant Big_Natural := ((X mod B) * (Y mod B)) mod B;
   begin
      if Y /= 0 and B > 1 then
         Lemma_Add_Mod (X * (Y - 1), X, B);
         Lemma_Mult_Mod (X, Y - 1, B);
         Lemma_Mod_Mod (X, B);
         Lemma_Add_Mod ((X mod B) * ((Y - 1) mod B), X mod B, B);
         Lemma_Add_Mod (Y - 1, 1, B);
         pragma Assert (((Y - 1) mod B + 1) mod B = Y mod B);
         if (Y - 1) mod B + 1 < B then
            Lemma_Mod_Ident ((Y - 1) mod B + 1, B);
            Lemma_Mod_Mod ((X mod B) * (Y mod B), B);
            pragma Assert (Left = Right);
         else
            pragma Assert (Y mod B = 0);
            pragma Assert (Y / B * B = Y);
            pragma Assert ((X * Y) mod B = (X * Y) - (X * Y) / B * B);
            pragma Assert
              ((X * Y) mod B = (X * Y) - (X * (Y / B) * B) / B * B);
            Lemma_Mult_Div (X * (Y / B), B);
            pragma Assert (Left = 0);
            pragma Assert (Left = Right);
         end if;
      end if;
   end Lemma_Mult_Mod;

   -----------------
   -- Exp_Modular --
   -----------------

   function Exp_Modular
     (Left    : Unsigned;
      Modulus : Unsigned;
      Right   : Natural) return Unsigned
   is
      Result : Unsigned := 1;
      Factor : Unsigned := Left;
      Exp    : Natural := Right;

      function Mult (X, Y : Unsigned) return Unsigned is
        (Unsigned (Long_Long_Unsigned (X) * Long_Long_Unsigned (Y)
                    mod Long_Long_Unsigned (Modulus)))
      with
        Pre => Modulus /= 0;
      --  Modular multiplication. Note that we can't take advantage of the
      --  compiler's circuit, because the modulus is not known statically.

      --  Local ghost variables, functions and lemmas

      M : constant Big_Positive := Big (Modulus) with Ghost;

      function Equal_Modulo (X, Y : Big_Integer) return Boolean is
         (X mod M = Y mod M)
      with
        Ghost,
        Pre => Modulus /= 0;

      procedure Lemma_Mult (X, Y : Unsigned)
      with
        Ghost,
        Post => Big (Mult (X, Y)) = (Big (X) * Big (Y)) mod M
          and then Big (Mult (X, Y)) < M;

      procedure Lemma_Mult (X, Y : Unsigned) is
      begin
         pragma Assert (Big (Mult (X, Y)) = (Big (X) * Big (Y)) mod M);
      end Lemma_Mult;

      Rest : Big_Integer with Ghost;
      --  Ghost variable to hold Factor**Exp between Exp and Factor updates

   begin
      pragma Assert (Modulus /= 1);

      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: it is not worth special casing the cases of base values -1,0,+1
      --  since the expander does this when the base is a literal, and other
      --  cases will be extremely rare.

      if Exp /= 0 then
         loop
            pragma Loop_Invariant (Exp > 0);
            pragma Loop_Invariant (Result < Modulus);
            pragma Loop_Invariant (Equal_Modulo
              (Big (Result) * Big (Factor) ** Exp, Big (Left) ** Right));
            pragma Loop_Variant (Decreases => Exp);

            if Exp rem 2 /= 0 then
               pragma Assert
                 (Big (Factor) ** Exp
                  = Big (Factor) * Big (Factor) ** (Exp - 1));
               pragma Assert (Equal_Modulo
                 ((Big (Result) * Big (Factor)) * Big (Factor) ** (Exp - 1),
                  Big (Left) ** Right));
               pragma Assert (Big (Factor) >= 0);
               Lemma_Mult_Mod (Big (Result) * Big (Factor),
                                  Big (Factor) ** (Exp - 1),
                                  Big (Modulus));
               Lemma_Mult (Result, Factor);

               Result := Mult (Result, Factor);

               Lemma_Mod_Ident (Big (Result), Big (Modulus));
               Lemma_Mod_Mod (Big (Factor) ** (Exp - 1), Big (Modulus));
               Lemma_Mult_Mod (Big (Result),
                                  Big (Factor) ** (Exp - 1),
                                  Big (Modulus));
               pragma Assert (Equal_Modulo
                 (Big (Result) * Big (Factor) ** (Exp - 1),
                  Big (Left) ** Right));
               Lemma_Exp_Expand (Big (Factor), Exp - 1);
               pragma Assert (Exp / 2 = (Exp - 1) / 2);
            end if;

            Lemma_Exp_Expand (Big (Factor), Exp);

            Exp := Exp / 2;
            exit when Exp = 0;

            Rest := Big (Factor) ** Exp;
            pragma Assert (Equal_Modulo
              (Big (Result) * (Rest * Rest), Big (Left) ** Right));
            Lemma_Exp_Mod (Big (Factor) * Big (Factor), Exp, Big (Modulus));
            pragma Assert
              ((Big (Factor) * Big (Factor)) ** Exp = Rest * Rest);
            pragma Assert (Equal_Modulo
              ((Big (Factor) * Big (Factor)) ** Exp,
               Rest * Rest));
            Lemma_Mult (Factor, Factor);

            Factor := Mult (Factor, Factor);

            Lemma_Mod_Mod (Rest * Rest, Big (Modulus));
            Lemma_Mod_Ident (Big (Result), Big (Modulus));
            Lemma_Mult_Mod (Big (Result), Rest * Rest, Big (Modulus));
            pragma Assert (Big (Factor) >= 0);
            Lemma_Mult_Mod (Big (Result), Big (Factor) ** Exp,
                               Big (Modulus));
            pragma Assert (Equal_Modulo
              (Big (Result) * Big (Factor) ** Exp, Big (Left) ** Right));
         end loop;

         pragma Assert (Big (Result) = Big (Left) ** Right mod Big (Modulus));
      end if;

      return Result;

   end Exp_Modular;

end System.Exp_Mod;
