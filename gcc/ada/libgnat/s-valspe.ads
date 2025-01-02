------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ S P E C                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2023-2025, Free Software Foundation, Inc.         --
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

--  This package is part of a set of Ghost code packages used to proof the
--  implementations of the Image and Value attributes. It provides some common
--  specification functions used by the s-valxxx files.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

package System.Val_Spec with
  SPARK_Mode,
  Pure,
  Ghost
is
   function Only_Space_Ghost (S : String; From, To : Integer) return Boolean is
      (for all J in From .. To => S (J) = ' ')
   with
     Pre  => From > To or else (From >= S'First and then To <= S'Last),
     Post => True;
   --  Ghost function that returns True if S has only space characters from
   --  index From to index To.

   function First_Non_Space_Ghost
     (S        : String;
      From, To : Integer) return Positive
   with
     Pre  => From in S'Range
       and then To in S'Range
       and then not Only_Space_Ghost (S, From, To),
     Post => First_Non_Space_Ghost'Result in From .. To
       and then S (First_Non_Space_Ghost'Result) /= ' '
       and then Only_Space_Ghost
         (S, From, First_Non_Space_Ghost'Result - 1);
   --  Ghost function that returns the index of the first non-space character
   --  in S, which necessarily exists given the precondition on S.

   function Is_Boolean_Image_Ghost
     (Str : String;
      Val : Boolean) return Boolean
   is
     (not Only_Space_Ghost (Str, Str'First, Str'Last)
        and then
      (declare
         F : constant Positive := First_Non_Space_Ghost
           (Str, Str'First, Str'Last);
       begin
         (Val
          and then F <= Str'Last - 3
          and then Str (F)     in 't' | 'T'
          and then Str (F + 1) in 'r' | 'R'
          and then Str (F + 2) in 'u' | 'U'
          and then Str (F + 3) in 'e' | 'E'
          and then
            (if F + 3 < Str'Last then
               Only_Space_Ghost (Str, F + 4, Str'Last)))
           or else
         (not Val
          and then F <= Str'Last - 4
          and then Str (F)     in 'f' | 'F'
          and then Str (F + 1) in 'a' | 'A'
          and then Str (F + 2) in 'l' | 'L'
          and then Str (F + 3) in 's' | 'S'
          and then Str (F + 4) in 'e' | 'E'
          and then
            (if F + 4 < Str'Last then
               Only_Space_Ghost (Str, F + 5, Str'Last)))))
   with
     Ghost;
   --  Ghost function that returns True iff Str is the image of boolean Val,
   --  that is "true" or "false" in any capitalization, possibly surounded by
   --  space characters.

   function Only_Number_Ghost (Str : String; From, To : Integer) return Boolean
   is
      (for all J in From .. To => Str (J) in '0' .. '9' | '_')
   with
     Pre => From > To or else (From >= Str'First and then To <= Str'Last);
   --  Ghost function that returns True if S has only number characters from
   --  index From to index To.

   function Last_Number_Ghost (Str : String) return Positive
   with
     Pre  => Str /= "" and then Str (Str'First) in '0' .. '9',
     Post => Last_Number_Ghost'Result in Str'Range
       and then (if Last_Number_Ghost'Result < Str'Last then
                   Str (Last_Number_Ghost'Result + 1) not in '0' .. '9' | '_')
       and then Only_Number_Ghost (Str, Str'First, Last_Number_Ghost'Result);
   --  Ghost function that returns the index of the last character in S that
   --  is either a figure or underscore, which necessarily exists given the
   --  precondition on Str.

   function Is_Natural_Format_Ghost (Str : String) return Boolean is
     (Str /= ""
        and then Str (Str'First) in '0' .. '9'
        and then
        (declare
           L : constant Positive := Last_Number_Ghost (Str);
         begin
           Str (L) in '0' .. '9'
             and then (for all J in Str'First .. L =>
                         (if Str (J) = '_' then Str (J + 1) /= '_'))));
   --  Ghost function that determines if Str has the correct format for a
   --  natural number, consisting in a sequence of figures possibly separated
   --  by single underscores. It may be followed by other characters.

   function Starts_As_Exponent_Format_Ghost
     (Str  : String;
      Real : Boolean := False) return Boolean
   is
     (Str'Length > 1
      and then Str (Str'First) in 'E' | 'e'
      and then
        (declare
            Plus_Sign  : constant Boolean := Str (Str'First + 1) = '+';
            Minus_Sign : constant Boolean := Str (Str'First + 1) = '-';
            Sign       : constant Boolean := Plus_Sign or Minus_Sign;
         begin
           (if Minus_Sign then Real)
            and then (if Sign then Str'Length > 2)
            and then
              (declare
                 Start : constant Natural :=
                  (if Sign then Str'First + 2 else Str'First + 1);
               begin
                 Str (Start) in '0' .. '9')));
   --  Ghost function that determines if Str is recognized as something which
   --  might be an exponent, ie. it starts with an 'e', capitalized or not,
   --  followed by an optional sign which can only be '-' if we are working on
   --  real numbers (Real is True), and then a digit in decimal notation.

   function Is_Opt_Exponent_Format_Ghost
     (Str  : String;
      Real : Boolean := False) return Boolean
   is
     (not Starts_As_Exponent_Format_Ghost (Str, Real)
      or else
        (declare
           Start : constant Natural :=
             (if Str (Str'First + 1) in '+' | '-' then Str'First + 2
              else Str'First + 1);
         begin Is_Natural_Format_Ghost (Str (Start .. Str'Last))));
   --  Ghost function that determines if Str has the correct format for an
   --  optional exponent, that is, either it does not start as an exponent, or
   --  it is in a correct format for a natural number.

   function Scan_Natural_Ghost
     (Str : String;
      P   : Natural;
      Acc : Natural)
      return Natural
   with
     Subprogram_Variant => (Increases => P),
     Pre => Str /= "" and then Str (Str'First) in '0' .. '9'
       and then Str'Last < Natural'Last
       and then P in Str'First .. Last_Number_Ghost (Str) + 1;
   --  Ghost function that recursively computes the natural number in Str, up
   --  to the first number greater or equal to Natural'Last / 10, assuming Acc
   --  has been scanned already and scanning continues at index P.

   function Scan_Exponent_Ghost
     (Str  : String;
      Real : Boolean := False)
      return Integer
   is
     (declare
        Plus_Sign  : constant Boolean := Str (Str'First + 1) = '+';
        Minus_Sign : constant Boolean := Str (Str'First + 1) = '-';
        Sign       : constant Boolean := Plus_Sign or Minus_Sign;
        Start      : constant Natural :=
          (if Sign then Str'First + 2 else Str'First + 1);
        Value      : constant Natural :=
          Scan_Natural_Ghost (Str (Start .. Str'Last), Start, 0);
      begin
        (if Minus_Sign then -Value else Value))
   with
     Pre  => Str'Last < Natural'Last
       and then Starts_As_Exponent_Format_Ghost (Str, Real),
     Post => (if not Real then Scan_Exponent_Ghost'Result >= 0);
   --  Ghost function that scans an exponent

private

   ------------------------
   -- Scan_Natural_Ghost --
   ------------------------

   function Scan_Natural_Ghost
     (Str : String;
      P   : Natural;
      Acc : Natural)
      return Natural
   is
     (if P > Str'Last
        or else Str (P) not in '0' .. '9' | '_'
        or else Acc >= Integer'Last / 10
      then
        Acc
      elsif Str (P) = '_' then
        Scan_Natural_Ghost (Str, P + 1, Acc)
      else
        (declare
           Shift_Acc : constant Natural :=
             Acc * 10 +
               (Integer'(Character'Pos (Str (P))) -
                  Integer'(Character'Pos ('0')));
         begin
           Scan_Natural_Ghost (Str, P + 1, Shift_Acc)));

end System.Val_Spec;
