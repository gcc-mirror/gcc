------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

   --  As a use_clause for Int_Params cannot be used for instances of this
   --  generic in System specs, rename all constants and subprograms.

   Unsigned_Width_Ghost : constant Natural := Int_Params.Unsigned_Width_Ghost;

   function Wrap_Option (Value : Uns) return Uns_Option
     renames Int_Params.Wrap_Option;
   function Only_Decimal_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
      renames Int_Params.Only_Decimal_Ghost;
   function Hexa_To_Unsigned_Ghost (X : Character) return Uns
     renames Int_Params.Hexa_To_Unsigned_Ghost;
   function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
      return Uns_Option
      renames Int_Params.Scan_Based_Number_Ghost;
   function Is_Integer_Ghost (Str : String) return Boolean
     renames Int_Params.Is_Integer_Ghost;
   procedure Prove_Iter_Scan_Based_Number_Ghost
     (Str1, Str2 : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
      renames Int_Params.Prove_Iter_Scan_Based_Number_Ghost;
   procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int)
     renames Int_Params.Prove_Scan_Only_Decimal_Ghost;
   function Abs_Uns_Of_Int (Val : Int) return Uns
     renames Int_Params.Abs_Uns_Of_Int;
   function Value_Integer (Str : String) return Int
     renames Int_Params.Value_Integer;

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
       and then Only_Decimal_Ghost (S, From => P'Old + 1, To => P)
       and then Scan_Based_Number_Ghost (S, From => P'Old + 1, To => P)
         = Wrap_Option (Uns_Of_Non_Positive (T));
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
          and then Only_Decimal_Ghost (S, From => 2, To => P)
          and then Scan_Based_Number_Ghost (S, From => 2, To => P)
            = Wrap_Option (Abs_Uns_Of_Int (V)),
        Post => Is_Integer_Ghost (S (1 .. P))
          and then Value_Integer (S (1 .. P)) = V;
      --  Ghost lemma to prove the value of Value_Integer from the value of
      --  Scan_Based_Number_Ghost and the sign on a decimal string.

      -------------------------
      -- Prove_Value_Integer --
      -------------------------

      procedure Prove_Value_Integer is
         Str : constant String := S (1 .. P);
      begin
         pragma Assert (Str'First = 1);
         pragma Assert (Only_Decimal_Ghost (Str, From => 2, To => P));
         Prove_Iter_Scan_Based_Number_Ghost (S, Str, From => 2, To => P);
         pragma Assert (Scan_Based_Number_Ghost (Str, From => 2, To => P)
            = Wrap_Option (Abs_Uns_Of_Int (V)));
         Prove_Scan_Only_Decimal_Ghost (Str, V);
      end Prove_Value_Integer;

   --  Start of processing for Image_Integer

   begin
      if V >= 0 then
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
      Prev, Cur  : Uns_Option with Ghost;
      Prev_Value : Uns with Ghost;
      Prev_S     : String := S with Ghost;

      --  Local ghost lemmas

      procedure Prove_Character_Val (RU : Uns; RI : Int)
      with
        Ghost,
        Pre  => RU in 0 .. 9
          and then RI in 0 .. 9,
        Post => Character'Val (48 + RU) in '0' .. '9'
          and then Character'Val (48 + RI) in '0' .. '9';
      --  Ghost lemma to prove the value of a character corresponding to the
      --  next figure.

      procedure Prove_Hexa_To_Unsigned_Ghost (RU : Uns; RI : Int)
      with
        Ghost,
        Pre  => RU in 0 .. 9
          and then RI in 0 .. 9,
        Post => Hexa_To_Unsigned_Ghost (Character'Val (48 + RU)) = RU
          and then Hexa_To_Unsigned_Ghost (Character'Val (48 + RI)) = Uns (RI);
      --  Ghost lemma to prove that Hexa_To_Unsigned_Ghost returns the source
      --  figure when applied to the corresponding character.

      procedure Prove_Unchanged
      with
        Ghost,
        Pre  => P <= S'Last
          and then S_Init'First = S'First
          and then S_Init'Last = S'Last
          and then (for all K in S'First .. P => S (K) = S_Init (K)),
        Post => S (S'First .. P) = S_Init (S'First .. P);
      --  Ghost lemma to prove that the part of string S before P has not been
      --  modified.

      procedure Prove_Uns_Of_Non_Positive_Value
      with
        Ghost,
        Pre  => Uns_Value = Uns_Of_Non_Positive (Value),
        Post => Uns_Value / 10 = Uns_Of_Non_Positive (Value / 10)
          and then Uns_Value rem 10 = Uns_Of_Non_Positive (Value rem 10);
      --  Ghost lemma to prove that the relation between Value and its unsigned
      --  version is preserved.

      procedure Prove_Iter_Scan
        (Str1, Str2 : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
      with
        Ghost,
        Pre  => Str1'Last /= Positive'Last
          and then
            (From > To or else (From >= Str1'First and then To <= Str1'Last))
          and then Only_Decimal_Ghost (Str1, From, To)
          and then Str1'First = Str2'First
          and then Str1'Last = Str2'Last
          and then (for all J in From .. To => Str1 (J) = Str2 (J)),
        Post =>
          Scan_Based_Number_Ghost (Str1, From, To, Base, Acc)
            = Scan_Based_Number_Ghost (Str2, From, To, Base, Acc);
      --  Ghost lemma to prove that the result of Scan_Based_Number_Ghost only
      --  depends on the value of the argument string in the (From .. To) range
      --  of indexes. This is a wrapper on Prove_Iter_Scan_Based_Number_Ghost
      --  so that we can call it here on ghost arguments.

      -----------------------------
      -- Local lemma null bodies --
      -----------------------------

      procedure Prove_Character_Val (RU : Uns; RI : Int) is null;
      procedure Prove_Hexa_To_Unsigned_Ghost (RU : Uns; RI : Int) is null;
      procedure Prove_Unchanged is null;
      procedure Prove_Uns_Of_Non_Positive_Value is null;

      ---------------------
      -- Prove_Iter_Scan --
      ---------------------

      procedure Prove_Iter_Scan
        (Str1, Str2 : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
      is
      begin
         Prove_Iter_Scan_Based_Number_Ghost (Str1, Str2, From, To, Base, Acc);
      end Prove_Iter_Scan;

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
         Prove_Character_Val (Uns_Value rem 10, -(Value rem 10));
         Prove_Hexa_To_Unsigned_Ghost (Uns_Value rem 10, -(Value rem 10));
         Prove_Uns_Of_Non_Positive_Value;
         pragma Assert (Uns_Value rem 10 = Uns_Of_Non_Positive (Value rem 10));
         pragma Assert (Uns_Value rem 10 = Uns (-(Value rem 10)));
         pragma Assert
           (Uns_Value = From_Big (Big (Uns_T) / Big_10 ** (Nb_Digits - J)));

         Prev_Value := Uns_Value;
         Prev_S := S;
         Pow := Pow * 10;
         Uns_Value := Uns_Value / 10;

         S (P + J) := Character'Val (48 - (Value rem 10));
         Value := Value / 10;

         pragma Assert (S (P + J) in '0' .. '9');
         pragma Assert (Hexa_To_Unsigned_Ghost (S (P + J)) =
           From_Big (Big (Uns_T) / Big_10 ** (Nb_Digits - J)) rem 10);
         pragma Assert
           (for all K in P + J + 1 .. P + Nb_Digits => S (K) in '0' .. '9');

         Prev := Scan_Based_Number_Ghost
           (Str  => S,
            From => P + J + 1,
            To   => P + Nb_Digits,
            Base => 10,
            Acc  => Prev_Value);
         Cur := Scan_Based_Number_Ghost
           (Str  => S,
            From => P + J,
            To   => P + Nb_Digits,
            Base => 10,
            Acc  => Uns_Value);
         pragma Assert (Prev_Value = 10 * Uns_Value + (Prev_Value rem 10));
         pragma Assert
           (Prev_Value rem 10 = Hexa_To_Unsigned_Ghost (S (P + J)));
         pragma Assert
           (Prev_Value = 10 * Uns_Value + Hexa_To_Unsigned_Ghost (S (P + J)));

         if J /= Nb_Digits then
            Prove_Iter_Scan
              (Prev_S, S, P + J + 1, P + Nb_Digits, 10, Prev_Value);
         end if;

         pragma Assert (Prev = Cur);
         pragma Assert (Prev = Wrap_Option (Uns_T));

         pragma Loop_Invariant (Uns_Value = Uns_Of_Non_Positive (Value));
         pragma Loop_Invariant (Uns_Value <= Uns'Last / 10);
         pragma Loop_Invariant
           (for all K in S'First .. P => S (K) = S_Init (K));
         pragma Loop_Invariant (Only_Decimal_Ghost (S, P + J, P + Nb_Digits));
         pragma Loop_Invariant
           (for all K in P + J .. P + Nb_Digits => S (K) in '0' .. '9');
         pragma Loop_Invariant (Pow = Big_10 ** (Nb_Digits - J + 1));
         pragma Loop_Invariant (Big (Uns_Value) = Big (Uns_T) / Pow);
         pragma Loop_Invariant
           (Scan_Based_Number_Ghost
              (Str  => S,
               From => P + J,
               To   => P + Nb_Digits,
               Base => 10,
               Acc  => Uns_Value)
              = Wrap_Option (Uns_T));
      end loop;

      pragma Assert (Big (Uns_Value) = Big (Uns_T) / Big_10 ** (Nb_Digits));
      pragma Assert (Uns_Value = 0);
      Prove_Unchanged;
      pragma Assert
        (Scan_Based_Number_Ghost
           (Str  => S,
            From => P + 1,
            To   => P + Nb_Digits,
            Base => 10,
            Acc  => Uns_Value)
         = Wrap_Option (Uns_T));

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
