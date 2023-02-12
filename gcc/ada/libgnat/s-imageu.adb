------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ U                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package body System.Image_U is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Subprogram_Variant => Ignore);

   package Unsigned_Conversion is new Unsigned_Conversions (Int => Uns);

   function Big (Arg : Uns) return Big_Integer renames
     Unsigned_Conversion.To_Big_Integer;

   function From_Big (Arg : Big_Integer) return Uns renames
     Unsigned_Conversion.From_Big_Integer;

   Big_10 : constant Big_Integer := Big (10) with Ghost;

   --  Maximum value of exponent for 10 that fits in Uns'Base
   function Max_Log10 return Natural is
     (case Uns'Base'Size is
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

   procedure Lemma_Non_Zero (X : Uns)
   with
     Ghost,
     Pre  => X /= 0,
     Post => Big (X) /= 0;

   procedure Lemma_Div_Commutation (X, Y : Uns)
   with
     Ghost,
     Pre  => Y /= 0,
     Post => Big (X) / Big (Y) = Big (X / Y);

   procedure Lemma_Div_Twice (X : Big_Natural; Y, Z : Big_Positive)
   with
     Ghost,
     Post => X / Y / Z = X / (Y * Z);

   procedure Lemma_Unsigned_Width_Ghost
   with
     Ghost,
     Post => Unsigned_Width_Ghost = Max_Log10 + 2;

   ---------------------------
   -- Lemma_Div_Commutation --
   ---------------------------

   procedure Lemma_Non_Zero (X : Uns) is null;
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

   --------------------------------
   -- Lemma_Unsigned_Width_Ghost --
   --------------------------------

   procedure Lemma_Unsigned_Width_Ghost is
   begin
      pragma Assert (Unsigned_Width_Ghost <= Max_Log10 + 2);
      pragma Assert (Big (Uns'Last) > Big_10 ** Max_Log10);
      pragma Assert (Big (Uns'Last) < Big_10 ** (Unsigned_Width_Ghost - 1));
      pragma Assert (Unsigned_Width_Ghost >= Max_Log10 + 2);
   end Lemma_Unsigned_Width_Ghost;

   --------------------
   -- Image_Unsigned --
   --------------------

   procedure Image_Unsigned
     (V : Uns;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);

      procedure Prove_Value_Unsigned
      with
        Ghost,
        Pre => S'First = 1
          and then S'Last < Integer'Last
          and then P in 2 .. S'Last
          and then S (1) = ' '
          and then Uns_Params.Only_Decimal_Ghost (S, From => 2, To => P)
          and then Uns_Params.Scan_Based_Number_Ghost (S, From => 2, To => P)
            = Uns_Params.Wrap_Option (V),
        Post => not System.Val_Util.Only_Space_Ghost (S, 1, P)
          and then Uns_Params.Is_Unsigned_Ghost (S (1 .. P))
          and then Uns_Params.Is_Value_Unsigned_Ghost (S (1 .. P), V);
      --  Ghost lemma to prove the value of Value_Unsigned from the value of
      --  Scan_Based_Number_Ghost on a decimal string.

      --------------------------
      -- Prove_Value_Unsigned --
      --------------------------

      procedure Prove_Value_Unsigned is
         Str : constant String := S (1 .. P);
      begin
         pragma Assert (Str'First = 1);
         pragma Assert (S (2) /= ' ');
         pragma Assert
           (Uns_Params.Only_Decimal_Ghost (Str, From => 2, To => P));
         Uns_Params.Prove_Scan_Based_Number_Ghost_Eq
           (S, Str, From => 2, To => P);
         pragma Assert
           (Uns_Params.Scan_Based_Number_Ghost (Str, From => 2, To => P)
            = Uns_Params.Wrap_Option (V));
         Uns_Params.Prove_Scan_Only_Decimal_Ghost (Str, V);
      end Prove_Value_Unsigned;

   --  Start of processing for Image_Unsigned

   begin
      S (1) := ' ';
      P := 1;
      Set_Image_Unsigned (V, S, P);

      Prove_Value_Unsigned;
   end Image_Unsigned;

   ------------------------
   -- Set_Image_Unsigned --
   ------------------------

   procedure Set_Image_Unsigned
     (V : Uns;
      S : in out String;
      P : in out Natural)
   is
      Nb_Digits : Natural := 0;
      Value     : Uns := V;

      --  Local ghost variables

      Pow        : Big_Positive := 1 with Ghost;
      S_Init     : constant String := S with Ghost;
      Prev_Value : Uns with Ghost;
      Prev_S     : String := S with Ghost;

      --  Local ghost lemmas

      procedure Prove_Character_Val (R : Uns)
      with
        Ghost,
        Post => R rem 10 in 0 .. 9
          and then Character'Val (48 + R rem 10) in '0' .. '9';
      --  Ghost lemma to prove the value of a character corresponding to the
      --  next figure.

      procedure Prove_Euclidian (Val, Quot, Rest : Uns)
      with
        Ghost,
        Pre  => Quot = Val / 10
          and then Rest = Val rem 10,
        Post => Uns'Last - Rest >= 10 * Quot and then Val = 10 * Quot + Rest;
      --  Ghost lemma to prove the relation between the quotient/remainder of
      --  division by 10 and the initial value.

      procedure Prove_Hexa_To_Unsigned_Ghost (R : Uns)
      with
        Ghost,
        Pre  => R in 0 .. 9,
        Post => Uns_Params.Hexa_To_Unsigned_Ghost (Character'Val (48 + R)) = R;
      --  Ghost lemma to prove that Hexa_To_Unsigned_Ghost returns the source
      --  figure when applied to the corresponding character.

      procedure Prove_Scan_Iter
        (S, Prev_S      : String;
         V, Prev_V, Res : Uns;
         P, Max         : Natural)
        with
          Ghost,
          Pre =>
            S'First = Prev_S'First and then S'Last = Prev_S'Last
            and then S'Last < Natural'Last and then
            Max in S'Range and then P in S'First .. Max and then
            (for all I in P + 1 .. Max => Prev_S (I) in '0' .. '9')
            and then (for all I in P + 1 .. Max => Prev_S (I) = S (I))
            and then S (P) in '0' .. '9'
            and then V <= Uns'Last / 10
            and then Uns'Last - Uns_Params.Hexa_To_Unsigned_Ghost (S (P))
              >= 10 * V
            and then Prev_V =
              V * 10 + Uns_Params.Hexa_To_Unsigned_Ghost (S (P))
            and then
              (if P = Max then Prev_V = Res
               else Uns_Params.Scan_Based_Number_Ghost
                 (Str  => Prev_S,
                  From => P + 1,
                  To   => Max,
                  Base => 10,
                  Acc  => Prev_V) = Uns_Params.Wrap_Option (Res)),
          Post =>
            (for all I in P .. Max => S (I) in '0' .. '9')
            and then Uns_Params.Scan_Based_Number_Ghost
              (Str  => S,
               From => P,
               To   => Max,
               Base => 10,
               Acc  => V) = Uns_Params.Wrap_Option (Res);
      --  Ghost lemma to prove that Scan_Based_Number_Ghost is preserved
      --  through an iteration of the loop.

      -----------------------------
      -- Local lemma null bodies --
      -----------------------------

      procedure Prove_Character_Val (R : Uns) is null;
      procedure Prove_Euclidian (Val, Quot, Rest : Uns) is null;
      procedure Prove_Hexa_To_Unsigned_Ghost (R : Uns) is null;

      ---------------------
      -- Prove_Scan_Iter --
      ---------------------

      procedure Prove_Scan_Iter
        (S, Prev_S      : String;
         V, Prev_V, Res : Uns;
         P, Max         : Natural)
      is
         pragma Unreferenced (Res);
      begin
         Uns_Params.Lemma_Scan_Based_Number_Ghost_Step
           (Str  => S,
            From => P,
            To   => Max,
            Base => 10,
            Acc  => V);
         if P < Max then
            Uns_Params.Prove_Scan_Based_Number_Ghost_Eq
              (Prev_S, S, P + 1, Max, 10, Prev_V);
         else
            Uns_Params.Lemma_Scan_Based_Number_Ghost_Base
              (Str  => S,
               From => P + 1,
               To   => Max,
               Base => 10,
               Acc  => Prev_V);
         end if;
      end Prove_Scan_Iter;

   --  Start of processing for Set_Image_Unsigned

   begin
      pragma Assert (P >= S'First - 1 and then P < S'Last and then
                     P < Natural'Last);
      --  No check is done since, as documented in the specification, the
      --  caller guarantees that S is long enough to hold the result.

      Lemma_Unsigned_Width_Ghost;

      --  First we compute the number of characters needed for representing
      --  the number.
      loop
         Lemma_Div_Commutation (Value, 10);
         Lemma_Div_Twice (Big (V), Big_10 ** Nb_Digits, Big_10);

         Value := Value / 10;
         Nb_Digits := Nb_Digits + 1;
         Pow := Pow * 10;

         pragma Loop_Invariant (Nb_Digits in 1 .. Unsigned_Width_Ghost - 1);
         pragma Loop_Invariant (Pow = Big_10 ** Nb_Digits);
         pragma Loop_Invariant (Big (Value) = Big (V) / Pow);
         pragma Loop_Variant (Decreases => Value);

         exit when Value = 0;

         Lemma_Non_Zero (Value);
         pragma Assert (Pow <= Big (Uns'Last));
      end loop;
      pragma Assert (Big (V) / (Big_10 ** Nb_Digits) = 0);

      Value := V;
      Pow := 1;

      pragma Assert (Value = From_Big (Big (V) / Big_10 ** 0));

      --  We now populate digits from the end of the string to the beginning
      for J in reverse 1 .. Nb_Digits loop
         Lemma_Div_Commutation (Value, 10);
         Lemma_Div_Twice (Big (V), Big_10 ** (Nb_Digits - J), Big_10);
         Prove_Character_Val (Value);
         Prove_Hexa_To_Unsigned_Ghost (Value rem 10);

         Prev_Value := Value;
         Prev_S := S;
         Pow := Pow * 10;
         S (P + J) := Character'Val (48 + (Value rem 10));
         Value := Value / 10;

         Prove_Euclidian
           (Val  => Prev_Value,
            Quot => Value,
            Rest => Uns_Params.Hexa_To_Unsigned_Ghost (S (P + J)));

         Prove_Scan_Iter
           (S, Prev_S, Value, Prev_Value, V, P + J, P + Nb_Digits);

         pragma Loop_Invariant (Value <= Uns'Last / 10);
         pragma Loop_Invariant
           (for all K in S'First .. P => S (K) = S_Init (K));
         pragma Loop_Invariant
           (Uns_Params.Only_Decimal_Ghost
              (S, From => P + J, To => P + Nb_Digits));
         pragma Loop_Invariant (Pow = Big_10 ** (Nb_Digits - J + 1));
         pragma Loop_Invariant (Big (Value) = Big (V) / Pow);
         pragma Loop_Invariant
           (Uns_Params.Scan_Based_Number_Ghost
              (Str  => S,
               From => P + J,
               To   => P + Nb_Digits,
               Base => 10,
               Acc  => Value)
              = Uns_Params.Wrap_Option (V));
      end loop;
      pragma Assert (Big (Value) = Big (V) / (Big_10 ** Nb_Digits));
      pragma Assert (Value = 0);

      P := P + Nb_Digits;
   end Set_Image_Unsigned;

end System.Image_U;
