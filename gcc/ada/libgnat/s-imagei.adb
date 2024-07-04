------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ I                        --
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

with System.Val_Spec;

package body System.Image_I is

   --  Ghost code, loop invariants and assertions in this unit are meant for
   --  analysis only, not for run-time checking, as it would be too costly
   --  otherwise. This is enforced by setting the assertion policy to Ignore.

   pragma Assertion_Policy (Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Pre                => Ignore,
                            Post               => Ignore,
                            Subprogram_Variant => Ignore);

   subtype Non_Positive is Int range Int'First .. 0;

   function Uns_Of_Non_Positive (T : Non_Positive) return Uns is
     (if T = Int'First then Uns (Int'Last) + 1 else Uns (-T));

   procedure Set_Digits
     (T : Non_Positive;
      S : in out String;
      P : in out Natural)
   with
     Pre  => P < Integer'Last
       and then S'Last < Integer'Last
       and then S'First <= P + 1
       and then S'First <= S'Last
       and then P <= S'Last - Unsigned_Width_Ghost + 1,
     Post => S (S'First .. P'Old) = S'Old (S'First .. P'Old)
       and then P in P'Old + 1 .. S'Last
       and then UP.Only_Decimal_Ghost (S, From => P'Old + 1, To => P)
       and then UP.Scan_Based_Number_Ghost (S, From => P'Old + 1, To => P)
         = UP.Wrap_Option (Uns_Of_Non_Positive (T));
   --  Set digits of absolute value of T, which is zero or negative. We work
   --  with the negative of the value so that the largest negative number is
   --  not a special case.

   package Unsigned_Conversion is new Unsigned_Conversions (Int => Uns);

   function Big (Arg : Uns) return Big_Integer renames
     Unsigned_Conversion.To_Big_Integer;

   function From_Big (Arg : Big_Integer) return Uns renames
     Unsigned_Conversion.From_Big_Integer;

   Big_10 : constant Big_Integer := Big (10) with Ghost;

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

   -------------------
   -- Image_Integer --
   -------------------

   procedure Image_Integer
     (V : Int;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);

      procedure Prove_Value_Integer
      with
        Ghost,
        Pre => S'First = 1
          and then S'Last < Integer'Last
          and then P in 2 .. S'Last
          and then S (1) in ' ' | '-'
          and then (S (1) = '-') = (V < 0)
          and then UP.Only_Decimal_Ghost (S, From => 2, To => P)
          and then UP.Scan_Based_Number_Ghost (S, From => 2, To => P)
            = UP.Wrap_Option (IP.Abs_Uns_Of_Int (V)),
        Post => not System.Val_Spec.Only_Space_Ghost (S, 1, P)
          and then IP.Is_Integer_Ghost (S (1 .. P))
          and then IP.Is_Value_Integer_Ghost (S (1 .. P), V);
      --  Ghost lemma to prove the value of Value_Integer from the value of
      --  Scan_Based_Number_Ghost and the sign on a decimal string.

      -------------------------
      -- Prove_Value_Integer --
      -------------------------

      procedure Prove_Value_Integer is
         Str : constant String := S (1 .. P);
      begin
         pragma Assert (Str'First = 1);
         pragma Assert (Str (2) /= ' ');
         pragma Assert
           (UP.Only_Decimal_Ghost (Str, From => 2, To => P));
         UP.Prove_Scan_Based_Number_Ghost_Eq (S, Str, From => 2, To => P);
         pragma Assert
           (UP.Scan_Based_Number_Ghost (Str, From => 2, To => P)
            = UP.Wrap_Option (IP.Abs_Uns_Of_Int (V)));
         IP.Prove_Scan_Only_Decimal_Ghost (Str, V);
      end Prove_Value_Integer;

   --  Start of processing for Image_Integer

   begin
      if V >= 0 then
         pragma Annotate (CodePeer, False_Positive, "test always false",
                          "V can be positive");
         S (1) := ' ';
         P := 1;
         pragma Assert (P < S'Last);

      else
         P := 0;
         pragma Assert (P < S'Last - 1);
      end if;

      declare
         P_Prev : constant Integer := P with Ghost;
         Offset : constant Positive := (if V >= 0 then 1 else 2) with Ghost;
      begin
         Set_Image_Integer (V, S, P);

         pragma Assert (P_Prev + Offset = 2);
      end;
      pragma Assert (if V >= 0 then S (1) = ' ');
      pragma Assert (S (1) in ' ' | '-');

      Prove_Value_Integer;
   end Image_Integer;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
     (T : Non_Positive;
      S : in out String;
      P : in out Natural)
   is
      Nb_Digits : Natural := 0;
      Value     : Non_Positive := T;

      --  Local ghost variables

      Pow        : Big_Positive := 1 with Ghost;
      S_Init     : constant String := S with Ghost;
      Uns_T      : constant Uns := Uns_Of_Non_Positive (T) with Ghost;
      Uns_Value  : Uns := Uns_Of_Non_Positive (Value) with Ghost;
      Prev_Value : Uns with Ghost;
      Prev_S     : String := S with Ghost;

      --  Local ghost lemmas

      procedure Prove_Character_Val (RU : Uns; RI : Non_Positive)
      with
        Ghost,
        Post => RU rem 10 in 0 .. 9
          and then -(RI rem 10) in 0 .. 9
          and then Character'Val (48 + RU rem 10) in '0' .. '9'
          and then Character'Val (48 - RI rem 10) in '0' .. '9';
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

      procedure Prove_Hexa_To_Unsigned_Ghost (RU : Uns; RI : Int)
      with
        Ghost,
        Pre  => RU in 0 .. 9
          and then RI in 0 .. 9,
        Post => UP.Hexa_To_Unsigned_Ghost
            (Character'Val (48 + RU)) = RU
          and then UP.Hexa_To_Unsigned_Ghost
            (Character'Val (48 + RI)) = Uns (RI);
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
            and then Uns'Last - UP.Hexa_To_Unsigned_Ghost (S (P))
              >= 10 * V
            and then Prev_V =
              V * 10 + UP.Hexa_To_Unsigned_Ghost (S (P))
            and then
              (if P = Max then Prev_V = Res
               else UP.Scan_Based_Number_Ghost
                 (Str  => Prev_S,
                  From => P + 1,
                  To   => Max,
                  Base => 10,
                  Acc  => Prev_V) = UP.Wrap_Option (Res)),
          Post =>
            (for all I in P .. Max => S (I) in '0' .. '9')
            and then UP.Scan_Based_Number_Ghost
              (Str  => S,
               From => P,
               To   => Max,
               Base => 10,
               Acc  => V) = UP.Wrap_Option (Res);
      --  Ghost lemma to prove that Scan_Based_Number_Ghost is preserved
      --  through an iteration of the loop.

      procedure Prove_Uns_Of_Non_Positive_Value
      with
        Ghost,
        Pre  => Uns_Value = Uns_Of_Non_Positive (Value),
        Post => Uns_Value / 10 = Uns_Of_Non_Positive (Value / 10)
          and then Uns_Value rem 10 = Uns_Of_Non_Positive (Value rem 10);
      --  Ghost lemma to prove that the relation between Value and its unsigned
      --  version is preserved.

      -----------------------------
      -- Local lemma null bodies --
      -----------------------------

      procedure Prove_Character_Val (RU : Uns; RI : Non_Positive) is null;
      procedure Prove_Euclidian (Val, Quot, Rest : Uns) is null;
      procedure Prove_Hexa_To_Unsigned_Ghost (RU : Uns; RI : Int) is null;
      procedure Prove_Uns_Of_Non_Positive_Value is null;

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
         UP.Lemma_Scan_Based_Number_Ghost_Step
           (Str  => S,
            From => P,
            To   => Max,
            Base => 10,
            Acc  => V);
         if P < Max then
            UP.Prove_Scan_Based_Number_Ghost_Eq
              (Prev_S, S, P + 1, Max, 10, Prev_V);
         else
            UP.Lemma_Scan_Based_Number_Ghost_Base
              (Str  => S,
               From => P + 1,
               To   => Max,
               Base => 10,
               Acc  => Prev_V);
         end if;
      end Prove_Scan_Iter;

   --  Start of processing for Set_Digits

   begin
      pragma Assert (P >= S'First - 1 and P < S'Last);
      --  No check is done since, as documented in the Set_Image_Integer
      --  specification, the caller guarantees that S is long enough to
      --  hold the result.

      --  First we compute the number of characters needed for representing
      --  the number.
      loop
         Lemma_Div_Commutation (Uns_Of_Non_Positive (Value), 10);
         Lemma_Div_Twice (Big (Uns_Of_Non_Positive (T)),
                          Big_10 ** Nb_Digits, Big_10);
         Prove_Uns_Of_Non_Positive_Value;

         Value := Value / 10;
         Nb_Digits := Nb_Digits + 1;

         Uns_Value := Uns_Value / 10;
         Pow := Pow * 10;

         pragma Loop_Invariant (Uns_Value = Uns_Of_Non_Positive (Value));
         pragma Loop_Invariant (Nb_Digits in 1 .. Unsigned_Width_Ghost - 1);
         pragma Loop_Invariant (Pow = Big_10 ** Nb_Digits);
         pragma Loop_Invariant (Big (Uns_Value) = Big (Uns_T) / Pow);
         pragma Loop_Variant (Increases => Value);

         exit when Value = 0;

         Lemma_Non_Zero (Uns_Value);
         pragma Assert (Pow <= Big (Uns'Last));
      end loop;

      Value := T;
      Uns_Value := Uns_Of_Non_Positive (T);
      Pow := 1;

      pragma Assert (Uns_Value = From_Big (Big (Uns_T) / Big_10 ** 0));

      --  We now populate digits from the end of the string to the beginning
      for J in reverse  1 .. Nb_Digits loop
         Lemma_Div_Commutation (Uns_Value, 10);
         Lemma_Div_Twice (Big (Uns_T), Big_10 ** (Nb_Digits - J), Big_10);
         Prove_Character_Val (Uns_Value, Value);
         Prove_Hexa_To_Unsigned_Ghost (Uns_Value rem 10, -(Value rem 10));
         Prove_Uns_Of_Non_Positive_Value;

         Prev_Value := Uns_Value;
         Prev_S := S;
         Pow := Pow * 10;
         Uns_Value := Uns_Value / 10;

         S (P + J) := Character'Val (48 - (Value rem 10));
         Value := Value / 10;

         Prove_Euclidian
           (Val  => Prev_Value,
            Quot => Uns_Value,
            Rest => UP.Hexa_To_Unsigned_Ghost (S (P + J)));

         Prove_Scan_Iter
           (S, Prev_S, Uns_Value, Prev_Value, Uns_T, P + J, P + Nb_Digits);

         pragma Loop_Invariant (Uns_Value = Uns_Of_Non_Positive (Value));
         pragma Loop_Invariant (Uns_Value <= Uns'Last / 10);
         pragma Loop_Invariant
           (for all K in S'First .. P => S (K) = S_Init (K));
         pragma Loop_Invariant
           (UP.Only_Decimal_Ghost (S, P + J, P + Nb_Digits));
         pragma Loop_Invariant
           (for all K in P + J .. P + Nb_Digits => S (K) in '0' .. '9');
         pragma Loop_Invariant (Pow = Big_10 ** (Nb_Digits - J + 1));
         pragma Loop_Invariant (Big (Uns_Value) = Big (Uns_T) / Pow);
         pragma Loop_Invariant
           (UP.Scan_Based_Number_Ghost
              (Str  => S,
               From => P + J,
               To   => P + Nb_Digits,
               Base => 10,
               Acc  => Uns_Value)
              = UP.Wrap_Option (Uns_T));
      end loop;

      pragma Assert (Big (Uns_Value) = Big (Uns_T) / Big_10 ** (Nb_Digits));
      pragma Assert (Uns_Value = 0);
      pragma Assert
        (UP.Scan_Based_Number_Ghost
           (Str  => S,
            From => P + 1,
            To   => P + Nb_Digits,
            Base => 10,
            Acc  => Uns_Value)
         = UP.Wrap_Option (Uns_T));

      P := P + Nb_Digits;
   end Set_Digits;

   -----------------------
   -- Set_Image_Integer --
   -----------------------

   procedure Set_Image_Integer
     (V : Int;
      S : in out String;
      P : in out Natural)
   is
   begin
      if V >= 0 then
         Set_Digits (-V, S, P);

      else
         pragma Assert (P >= S'First - 1 and P < S'Last);
         --  No check is done since, as documented in the specification,
         --  the caller guarantees that S is long enough to hold the result.
         P := P + 1;
         S (P) := '-';
         Set_Digits (V, S, P);
      end if;
   end Set_Image_Integer;

end System.Image_I;
