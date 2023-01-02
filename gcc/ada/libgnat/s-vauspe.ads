------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . V A L U E _ U _ S P E C                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2022-2023, Free Software Foundation, Inc.         --
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

--  This package contains the specification entities using for the formal
--  verification of the routines for scanning modular Unsigned values.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

with System.Val_Util; use System.Val_Util;

generic

   type Uns is mod <>;

package System.Value_U_Spec with
   Ghost,
   SPARK_Mode,
   Annotate => (GNATprove, Always_Return)
is
   pragma Preelaborate;

   type Uns_Option (Overflow : Boolean := False) is record
      case Overflow is
         when True =>
            null;
         when False =>
            Value : Uns := 0;
      end case;
   end record;

   function Wrap_Option (Value : Uns) return Uns_Option is
     (Overflow => False, Value => Value);

   function Only_Decimal_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
   is
      (for all J in From .. To => Str (J) in '0' .. '9')
   with
     Pre => From > To or else (From >= Str'First and then To <= Str'Last);
   --  Ghost function that returns True if S has only decimal characters
   --  from index From to index To.

   function Only_Hexa_Ghost (Str : String; From, To : Integer) return Boolean
   is
      (for all J in From .. To =>
          Str (J) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_')
   with
     Pre => From > To or else (From >= Str'First and then To <= Str'Last);
   --  Ghost function that returns True if S has only hexadecimal characters
   --  from index From to index To.

   function Last_Hexa_Ghost (Str : String) return Positive
   with
     Pre  => Str /= ""
       and then Str (Str'First) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F',
     Post => Last_Hexa_Ghost'Result in Str'Range
       and then (if Last_Hexa_Ghost'Result < Str'Last then
                   Str (Last_Hexa_Ghost'Result + 1) not in
                     '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_')
       and then Only_Hexa_Ghost (Str, Str'First, Last_Hexa_Ghost'Result);
   --  Ghost function that returns the index of the last character in S that
   --  is either an hexadecimal digit or an underscore, which necessarily
   --  exists given the precondition on Str.

   function Is_Based_Format_Ghost (Str : String) return Boolean
   is
     (Str /= ""
        and then Str (Str'First) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'
        and then
        (declare
           L : constant Positive := Last_Hexa_Ghost (Str);
         begin
           Str (L) /= '_'
             and then (for all J in Str'First .. L =>
                         (if Str (J) = '_' then Str (J + 1) /= '_'))));
   --  Ghost function that determines if Str has the correct format for a
   --  based number, consisting in a sequence of hexadecimal digits possibly
   --  separated by single underscores. It may be followed by other characters.

   function Hexa_To_Unsigned_Ghost (X : Character) return Uns is
     (case X is
         when '0' .. '9' => Character'Pos (X) - Character'Pos ('0'),
         when 'a' .. 'f' => Character'Pos (X) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Character'Pos (X) - Character'Pos ('A') + 10,
         when others     => raise Program_Error)
   with
     Pre => X in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
   --  Ghost function that computes the value corresponding to an hexadecimal
   --  digit.

   function Scan_Overflows_Ghost
     (Digit : Uns;
      Base  : Uns;
      Acc   : Uns) return Boolean
   is
     (Digit >= Base
      or else Acc > Uns'Last / Base
      or else Uns'Last - Digit < Base * Acc);
   --  Ghost function which returns True if Digit + Base * Acc overflows or
   --  Digit is greater than Base, as this is used by the algorithm for the
   --  test of overflow.

   function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0) return Uns_Option
   with
     Subprogram_Variant => (Increases => From),
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To);
   --  Ghost function that recursively computes the based number in Str,
   --  assuming Acc has been scanned already and scanning continues at index
   --  From.

   --  Lemmas unfolding the recursive definition of Scan_Based_Number_Ghost

   procedure Lemma_Scan_Based_Number_Ghost_Base
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   with
     Global => null,
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To),
     Post =>
      (if From > To
       then Scan_Based_Number_Ghost (Str, From, To, Base, Acc) =
         (Overflow => False, Value => Acc));
   --  Base case: Scan_Based_Number_Ghost returns Acc if From is bigger than To

   procedure Lemma_Scan_Based_Number_Ghost_Underscore
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   with
     Global => null,
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To),
     Post =>
      (if From <= To and then Str (From) = '_'
       then Scan_Based_Number_Ghost (Str, From, To, Base, Acc) =
           Scan_Based_Number_Ghost (Str, From + 1, To, Base, Acc));
   --  Underscore case: underscores are ignored while scanning

   procedure Lemma_Scan_Based_Number_Ghost_Overflow
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   with
     Global => null,
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To),
     Post =>
      (if From <= To
         and then Str (From) /= '_'
         and then Scan_Overflows_Ghost
           (Hexa_To_Unsigned_Ghost (Str (From)), Base, Acc)
       then Scan_Based_Number_Ghost (Str, From, To, Base, Acc) =
           (Overflow => True));
   --  Overflow case: scanning a digit which causes an overflow

   procedure Lemma_Scan_Based_Number_Ghost_Step
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   with
     Global => null,
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To),
     Post =>
      (if From <= To
         and then Str (From) /= '_'
         and then not Scan_Overflows_Ghost
           (Hexa_To_Unsigned_Ghost (Str (From)), Base, Acc)
       then Scan_Based_Number_Ghost (Str, From, To, Base, Acc) =
         Scan_Based_Number_Ghost
           (Str, From + 1, To, Base,
            Base * Acc + Hexa_To_Unsigned_Ghost (Str (From))));
   --  Normal case: scanning a digit without overflows

   function Exponent_Unsigned_Ghost
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10) return Uns_Option
   with
     Subprogram_Variant => (Decreases => Exp);
   --  Ghost function that recursively computes Value * Base ** Exp

   --  Lemmas unfolding the recursive definition of Exponent_Unsigned_Ghost

   procedure Lemma_Exponent_Unsigned_Ghost_Base
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   with
     Post =>
       (if Exp = 0 or Value = 0
        then Exponent_Unsigned_Ghost (Value, Exp, Base) =
          (Overflow => False, Value => Value));
   --  Base case: Exponent_Unsigned_Ghost returns 0 if Value or Exp is 0

   procedure Lemma_Exponent_Unsigned_Ghost_Overflow
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   with
     Post =>
       (if Exp /= 0
          and then Value /= 0
          and then Scan_Overflows_Ghost (0, Base, Value)
        then Exponent_Unsigned_Ghost (Value, Exp, Base) = (Overflow => True));
   --  Overflow case: the next multiplication overflows

   procedure Lemma_Exponent_Unsigned_Ghost_Step
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10)
   with
     Post =>
       (if Exp /= 0
          and then Value /= 0
          and then not Scan_Overflows_Ghost (0, Base, Value)
        then Exponent_Unsigned_Ghost (Value, Exp, Base) =
            Exponent_Unsigned_Ghost (Value * Base, Exp - 1, Base));
   --  Normal case: exponentiation without overflows

   function Is_Raw_Unsigned_Format_Ghost (Str : String) return Boolean is
     (Is_Natural_Format_Ghost (Str)
      and then
        (declare
           Last_Num_Init   : constant Integer := Last_Number_Ghost (Str);
           Starts_As_Based : constant Boolean :=
             Last_Num_Init < Str'Last - 1
             and then Str (Last_Num_Init + 1) in '#' | ':'
             and then Str (Last_Num_Init + 2) in
               '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
           Last_Num_Based  : constant Integer :=
             (if Starts_As_Based
              then Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. Str'Last))
              else Last_Num_Init);
           Is_Based        : constant Boolean :=
             Starts_As_Based
             and then Last_Num_Based < Str'Last
             and then Str (Last_Num_Based + 1) = Str (Last_Num_Init + 1);
           First_Exp       : constant Integer :=
             (if Is_Based then Last_Num_Based + 2 else Last_Num_Init + 1);
         begin
           (if Starts_As_Based then
              Is_Based_Format_Ghost (Str (Last_Num_Init + 2 .. Str'Last))
              and then Last_Num_Based < Str'Last)
            and then Is_Opt_Exponent_Format_Ghost
              (Str (First_Exp .. Str'Last))))
   with
     Pre  => Str'Last /= Positive'Last;
   --  Ghost function that determines if Str has the correct format for an
   --  unsigned number without a sign character.
   --  It is a natural number in base 10, optionally followed by a based
   --  number surrounded by delimiters # or :, optionally followed by an
   --  exponent part.

   type Split_Value_Ghost is record
      Value : Uns;
      Base  : Uns;
      Expon : Natural;
   end record;

   function Scan_Split_No_Overflow_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
   is
     (declare
        Last_Num_Init   : constant Integer :=
          Last_Number_Ghost (Str (From .. To));
        Init_Val        : constant Uns_Option :=
          Scan_Based_Number_Ghost (Str, From, Last_Num_Init);
        Starts_As_Based : constant Boolean :=
          Last_Num_Init < To - 1
          and then Str (Last_Num_Init + 1) in '#' | ':'
          and then Str (Last_Num_Init + 2) in
          '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
        Last_Num_Based  : constant Integer :=
          (if Starts_As_Based
           then Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. To))
           else Last_Num_Init);
        Based_Val       : constant Uns_Option :=
          (if Starts_As_Based and then not Init_Val.Overflow
           then Scan_Based_Number_Ghost
             (Str, Last_Num_Init + 2, Last_Num_Based, Init_Val.Value)
           else Init_Val);
      begin
        not Init_Val.Overflow
        and then
          (Last_Num_Init >= To - 1
           or else Str (Last_Num_Init + 1) not in '#' | ':'
           or else Init_Val.Value in 2 .. 16)
        and then
          (not Starts_As_Based
           or else not Based_Val.Overflow))
   with
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9';
   --  Ghost function that determines if an overflow might occur while scanning
   --  the representation of an unsigned number. The computation overflows if
   --  either:
   --    * The computation of the decimal part overflows,
   --    * The decimal part is followed by a valid delimiter for a based
   --      part, and the number corresponding to the base is not a valid base,
   --      or
   --    * The computation of the based part overflows.

   pragma Warnings (Off, "constant * is not referenced");
   function Scan_Split_Value_Ghost
     (Str      : String;
      From, To : Integer)
      return Split_Value_Ghost
   is
     (declare
        Last_Num_Init   : constant Integer :=
          Last_Number_Ghost (Str (From .. To));
        Init_Val        : constant Uns_Option :=
          Scan_Based_Number_Ghost (Str, From, Last_Num_Init);
        Starts_As_Based : constant Boolean :=
          Last_Num_Init < To - 1
          and then Str (Last_Num_Init + 1) in '#' | ':'
          and then Str (Last_Num_Init + 2) in
          '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
        Last_Num_Based  : constant Integer :=
          (if Starts_As_Based
           then Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. To))
           else Last_Num_Init);
        Is_Based        : constant Boolean :=
          Starts_As_Based
          and then Last_Num_Based < To
          and then Str (Last_Num_Based + 1) = Str (Last_Num_Init + 1);
        Based_Val       : constant Uns_Option :=
          (if Starts_As_Based and then not Init_Val.Overflow
           then Scan_Based_Number_Ghost
             (Str, Last_Num_Init + 2, Last_Num_Based, Init_Val.Value)
           else Init_Val);
        First_Exp       : constant Integer :=
          (if Is_Based then Last_Num_Based + 2 else Last_Num_Init + 1);
        Expon           : constant Natural :=
          (if Starts_As_Exponent_Format_Ghost (Str (First_Exp .. To))
           then Scan_Exponent_Ghost (Str (First_Exp .. To))
           else 0);
        Base            : constant Uns :=
          (if Is_Based then Init_Val.Value else 10);
        Value           : constant Uns :=
          (if Is_Based then Based_Val.Value else Init_Val.Value);
      begin
        (Value => Value, Base => Base, Expon => Expon))
   with
     Pre => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9'
       and then Scan_Split_No_Overflow_Ghost (Str, From, To);
   --  Ghost function that scans an unsigned number without a sign character
   --  and return a record containing the values scanned for its value, its
   --  base, and its exponent.
   pragma Warnings (On, "constant * is not referenced");

   function Raw_Unsigned_No_Overflow_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
   is
     (Scan_Split_No_Overflow_Ghost (Str, From, To)
      and then
        (declare
           Val : constant Split_Value_Ghost := Scan_Split_Value_Ghost
             (Str, From, To);
         begin
           not Exponent_Unsigned_Ghost
             (Val.Value, Val.Expon, Val.Base).Overflow))
   with
     Pre => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9';
   --  Ghost function that determines if the computation of the unsigned number
   --  represented by Str will overflow. The computation overflows if either:
   --    * The scan of the string overflows, or
   --    * The computation of the exponentiation overflows.

   function Scan_Raw_Unsigned_Ghost
     (Str      : String;
      From, To : Integer)
      return Uns
   is
     (declare
        Val : constant Split_Value_Ghost := Scan_Split_Value_Ghost
          (Str, From, To);
      begin
        Exponent_Unsigned_Ghost (Val.Value, Val.Expon, Val.Base).Value)
   with
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9'
       and then Raw_Unsigned_No_Overflow_Ghost (Str, From, To);
   --  Ghost function that scans an unsigned number without a sign character

   function Raw_Unsigned_Last_Ghost
     (Str      : String;
      From, To : Integer)
      return Positive
   is
     (declare
        Last_Num_Init   : constant Integer :=
          Last_Number_Ghost (Str (From .. To));
        Starts_As_Based : constant Boolean :=
          Last_Num_Init < To - 1
          and then Str (Last_Num_Init + 1) in '#' | ':'
          and then Str (Last_Num_Init + 2) in
          '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';
        Last_Num_Based  : constant Integer :=
          (if Starts_As_Based
           then Last_Hexa_Ghost (Str (Last_Num_Init + 2 .. To))
           else Last_Num_Init);
        Is_Based        : constant Boolean :=
          Starts_As_Based
          and then Last_Num_Based < To
          and then Str (Last_Num_Based + 1) = Str (Last_Num_Init + 1);
        First_Exp       : constant Integer :=
          (if Is_Based then Last_Num_Based + 2 else Last_Num_Init + 1);
      begin
        (if not Starts_As_Exponent_Format_Ghost (Str (First_Exp .. To))
         then First_Exp
         elsif Str (First_Exp + 1) in '-' | '+' then
           Last_Number_Ghost (Str (First_Exp + 2 .. To)) + 1
         else Last_Number_Ghost (Str (First_Exp + 1 .. To)) + 1))
   with
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9';
   --  Ghost function that returns the position of the cursor once an unsigned
   --  number has been seen.

   function Slide_To_1 (Str : String) return String
   with
     Post =>
       Only_Space_Ghost (Str, Str'First, Str'Last) =
       (for all J in Str'First .. Str'Last =>
          Slide_To_1'Result (J - Str'First + 1) = ' ');
   --  Slides Str so that it starts at 1

   function Slide_If_Necessary (Str : String) return String is
     (if Str'Last = Positive'Last then Slide_To_1 (Str) else Str);
   --  If Str'Last = Positive'Last then slides Str so that it starts at 1

   function Is_Unsigned_Ghost (Str : String) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
      begin
        Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last))
          and then Raw_Unsigned_No_Overflow_Ghost (Str, Fst_Num, Str'Last)
          and then Only_Space_Ghost
             (Str, Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last), Str'Last))
   with
       Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last;
   --  Ghost function that determines if Str has the correct format for an
   --  unsigned number, consisting in some blank characters, an optional
   --  + sign, a raw unsigned number which does not overflow and then some
   --  more blank characters.

   function Is_Value_Unsigned_Ghost (Str : String; Val : Uns) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
      begin
        Val = Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last))
   with
       Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last
       and then Is_Unsigned_Ghost (Str);
   --  Ghost function that returns True if Val is the value corresponding to
   --  the unsigned number represented by Str.

   procedure Prove_Scan_Based_Number_Ghost_Eq
     (Str1, Str2 : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
   with
     Subprogram_Variant => (Increases => From),
     Pre  => Str1'Last /= Positive'Last
       and then Str2'Last /= Positive'Last
       and then
         (From > To or else (From >= Str1'First and then To <= Str1'Last))
       and then
         (From > To or else (From >= Str2'First and then To <= Str2'Last))
       and then Only_Hexa_Ghost (Str1, From, To)
       and then (for all J in From .. To => Str1 (J) = Str2 (J)),
     Post =>
       Scan_Based_Number_Ghost (Str1, From, To, Base, Acc)
         = Scan_Based_Number_Ghost (Str2, From, To, Base, Acc);
   --  Scan_Based_Number_Ghost returns the same value on two slices which are
   --  equal.

   procedure Prove_Scan_Only_Decimal_Ghost
     (Str : String;
      Val : Uns)
   with
     Pre  => Str'Last /= Positive'Last
       and then Str'Length >= 2
       and then Str (Str'First) = ' '
       and then Only_Decimal_Ghost (Str, Str'First + 1, Str'Last)
       and then Scan_Based_Number_Ghost (Str, Str'First + 1, Str'Last)
         = Wrap_Option (Val),
     Post => Is_Unsigned_Ghost (Slide_If_Necessary (Str))
       and then
         Is_Value_Unsigned_Ghost (Slide_If_Necessary (Str), Val);
   --  Ghost lemma used in the proof of 'Image implementation, to prove that
   --  the result of Value_Unsigned on a decimal string is the same as the
   --  result of Scan_Based_Number_Ghost.

   --  Bundle Uns type with other types, constants and subprograms used in
   --  ghost code, so that this package can be instantiated once and used
   --  multiple times as generic formal for a given Int type.

   package Uns_Params is new System.Val_Util.Uns_Params
     (Uns                                        => Uns,
      P_Uns_Option                               => Uns_Option,
      P_Wrap_Option                              => Wrap_Option,
      P_Hexa_To_Unsigned_Ghost                   => Hexa_To_Unsigned_Ghost,
      P_Scan_Overflows_Ghost                     => Scan_Overflows_Ghost,
      P_Is_Raw_Unsigned_Format_Ghost             =>
         Is_Raw_Unsigned_Format_Ghost,
      P_Scan_Split_No_Overflow_Ghost             =>
         Scan_Split_No_Overflow_Ghost,
      P_Raw_Unsigned_No_Overflow_Ghost           =>
         Raw_Unsigned_No_Overflow_Ghost,
      P_Exponent_Unsigned_Ghost                  => Exponent_Unsigned_Ghost,
      P_Lemma_Exponent_Unsigned_Ghost_Base       =>
         Lemma_Exponent_Unsigned_Ghost_Base,
      P_Lemma_Exponent_Unsigned_Ghost_Overflow   =>
         Lemma_Exponent_Unsigned_Ghost_Overflow,
      P_Lemma_Exponent_Unsigned_Ghost_Step       =>
         Lemma_Exponent_Unsigned_Ghost_Step,
      P_Scan_Raw_Unsigned_Ghost                  => Scan_Raw_Unsigned_Ghost,
      P_Lemma_Scan_Based_Number_Ghost_Base       =>
         Lemma_Scan_Based_Number_Ghost_Base,
      P_Lemma_Scan_Based_Number_Ghost_Underscore =>
         Lemma_Scan_Based_Number_Ghost_Underscore,
      P_Lemma_Scan_Based_Number_Ghost_Overflow   =>
         Lemma_Scan_Based_Number_Ghost_Overflow,
      P_Lemma_Scan_Based_Number_Ghost_Step       =>
         Lemma_Scan_Based_Number_Ghost_Step,
      P_Raw_Unsigned_Last_Ghost                  => Raw_Unsigned_Last_Ghost,
      P_Only_Decimal_Ghost                       => Only_Decimal_Ghost,
      P_Scan_Based_Number_Ghost                  => Scan_Based_Number_Ghost,
      P_Is_Unsigned_Ghost                        =>
         Is_Unsigned_Ghost,
      P_Is_Value_Unsigned_Ghost                  =>
         Is_Value_Unsigned_Ghost,
      P_Prove_Scan_Only_Decimal_Ghost            =>
         Prove_Scan_Only_Decimal_Ghost,
      P_Prove_Scan_Based_Number_Ghost_Eq         =>
         Prove_Scan_Based_Number_Ghost_Eq);

private

   ----------------
   -- Slide_To_1 --
   ----------------

   function Slide_To_1 (Str : String) return String is
      (declare
         Res : constant String (1 .. Str'Length) := Str;
       begin
         Res);

end System.Value_U_Spec;
