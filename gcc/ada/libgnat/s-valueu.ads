------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ U                        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains routines for scanning modular Unsigned
--  values for use in Text_IO.Modular_IO, and the Value attribute.

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
pragma Warnings (Off, "postcondition does not mention function result");
--  True postconditions are used to avoid inlining for GNATprove

with System.Val_Util; use System.Val_Util;

generic

   type Uns is mod <>;

package System.Value_U is
   pragma Preelaborate;

   type Uns_Option (Overflow : Boolean := False) is record
      case Overflow is
         when True =>
            null;
         when False =>
            Value : Uns := 0;
      end case;
   end record with Ghost;

   function Only_Hexa_Ghost (Str : String; From, To : Integer) return Boolean
   is
      (for all J in From .. To =>
          Str (J) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_')
   with
     Ghost,
     Pre => From > To or else (From >= Str'First and then To <= Str'Last);
   --  Ghost function that returns True if S has only hexadecimal characters
   --  from index From to index To.

   function Last_Hexa_Ghost (Str : String) return Positive
   with
     Ghost,
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
                         (if Str (J) = '_' then Str (J + 1) /= '_'))))
   with
     Ghost;
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
     Ghost,
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
      or else Uns'Last - Digit < Base * Acc)
   with Ghost;
   --  Ghost function which returns True if Digit + Base * Acc overflows or
   --  Digit is greater than Base, as this is used by the algorithm for the
   --  test of overflow.

   function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0) return Uns_Option
   with
     Ghost,
     Subprogram_Variant => (Increases => From),
     Pre  => Str'Last /= Positive'Last
         and then
           (From > To or else (From >= Str'First and then To <= Str'Last))
         and then Only_Hexa_Ghost (Str, From, To);
   --  Ghost function that recursively computes the based number in Str,
   --  assuming Acc has been scanned already and scanning continues at index
   --  From.

   function Exponent_Unsigned_Ghost
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10) return Uns_Option
   with
     Ghost,
     Subprogram_Variant => (Decreases => Exp);
   --  Ghost function that recursively computes Value * Base ** Exp

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
     Ghost,
     Pre  => Str'Last /= Positive'Last,
     Post => True;
   --  Ghost function that determines if Str has the correct format for an
   --  unsigned number without a sign character.
   --  It is a natural number in base 10, optionally followed by a based
   --  number surrounded by delimiters # or :, optionally followed by an
   --  exponent part.

   function Raw_Unsigned_Overflows_Ghost
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
      begin
        Init_Val.Overflow
        or else
          (Last_Num_Init < To - 1
           and then Str (Last_Num_Init + 1) in '#' | ':'
           and then Init_Val.Value not in 2 .. 16)
        or else
          (Starts_As_Based
           and then Based_Val.Overflow)
        or else
          (Starts_As_Exponent_Format_Ghost (Str (First_Exp .. To))
           and then
             (declare
                Base  : constant Uns :=
                  (if Is_Based then Init_Val.Value else 10);
                Value : constant Uns :=
                  (if Is_Based then Based_Val.Value else Init_Val.Value);
              begin
                Exponent_Unsigned_Ghost
                  (Value, Expon, Base).Overflow)))
   with
     Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9',
     Post => True;
   --  Ghost function that determines if the computation of the unsigned number
   --  represented by Str will overflow. The computation overflows if either:
   --    * The computation of the decimal part overflows,
   --    * The decimal part is followed by a valid delimiter for a based
   --      part, and the number corresponding to the base is not a valid base,
   --    * The computation of the based part overflows, or
   --    * There is an exponent and the computation of the exponentiation
   --      overflows.

   function Scan_Raw_Unsigned_Ghost
     (Str      : String;
      From, To : Integer)
      return Uns
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
        Exponent_Unsigned_Ghost (Value, Expon, Base).Value)
   with
     Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9'
       and then not Raw_Unsigned_Overflows_Ghost (Str, From, To),
     Post => True;
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
     Ghost,
     Pre  => Str'Last /= Positive'Last
       and then From in Str'Range
       and then To in From .. Str'Last
       and then Str (From) in '0' .. '9',
     Post => Raw_Unsigned_Last_Ghost'Result in From .. To + 1;
   --  Ghost function that returns the position of the cursor once an unsigned
   --  number has been seen.

   procedure Scan_Raw_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Uns)
   with Pre => Str'Last /= Positive'Last
     and then Ptr.all in Str'Range
     and then Max in Ptr.all .. Str'Last
     and then Is_Raw_Unsigned_Format_Ghost (Str (Ptr.all .. Max)),
     Post => not Raw_Unsigned_Overflows_Ghost (Str, Ptr.all'Old, Max)
     and Res = Scan_Raw_Unsigned_Ghost (Str, Ptr.all'Old, Max)
     and Ptr.all = Raw_Unsigned_Last_Ghost (Str, Ptr.all'Old, Max);

   --  This function scans the string starting at Str (Ptr.all) for a valid
   --  integer according to the syntax described in (RM 3.5(43)). The substring
   --  scanned extends no further than Str (Max).  Note: this does not scan
   --  leading or trailing blanks, nor leading sign.
   --
   --  There are three cases for the return:
   --
   --  If a valid integer is found, then Ptr.all is updated past the last
   --  character of the integer.
   --
   --  If no valid integer is found, then Ptr.all points either to an initial
   --  non-digit character, or to Max + 1 if the field is all spaces and the
   --  exception Constraint_Error is raised.
   --
   --  If a syntactically valid integer is scanned, but the value is out of
   --  range, or, in the based case, the base value is out of range or there
   --  is an out of range digit, then Ptr.all points past the integer, and
   --  Constraint_Error is raised.
   --
   --  Note: these rules correspond to the requirements for leaving the pointer
   --  positioned in Text_IO.Get. Note that the rules as stated in the RM would
   --  seem to imply that for a case like:
   --
   --    8#12345670009#
   --
   --  the pointer should be left at the first # having scanned out the longest
   --  valid integer literal (8), but in fact in this case the pointer points
   --  past the final # and Constraint_Error is raised. This is the behavior
   --  expected for Text_IO and enforced by the ACATS tests.
   --
   --  If a based literal is malformed in that a character other than a valid
   --  hexadecimal digit is encountered during scanning out the digits after
   --  the # (this includes the case of using the wrong terminator, : instead
   --  of # or vice versa) there are two cases. If all the digits before the
   --  non-digit are in range of the base, as in
   --
   --    8#100x00#
   --    8#100:
   --
   --  then in this case, the "base" value before the initial # is returned as
   --  the result, and the pointer points to the initial # character on return.
   --
   --  If an out of range digit has been detected before the invalid character,
   --  as in:
   --
   --   8#900x00#
   --   8#900:
   --
   --  then the pointer is also left at the initial # character, but constraint
   --  error is raised reflecting the encounter of an out of range digit.
   --
   --  Finally if we have an unterminated fixed-point constant where the final
   --  # or : character is missing, Constraint_Error is raised and the pointer
   --  is left pointing past the last digit, as in:
   --
   --   8#22
   --
   --  This string results in a Constraint_Error with the pointer pointing
   --  past the second 2.
   --
   --  Note: if Str is empty, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case.
   --  ??? This is not the case. We will read Str (Ptr.all) without checking
   --  and increase Ptr.all by one.
   --
   --  Note: this routine should not be called with Str'Last = Positive'Last.
   --  If this occurs Program_Error is raised with a message noting that this
   --  case is not supported. Most such cases are eliminated by the caller.

   procedure Scan_Unsigned
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Uns)
   with Pre => Str'Last /= Positive'Last
     and then Ptr.all in Str'Range
     and then Max in Ptr.all .. Str'Last
     and then not Only_Space_Ghost (Str, Ptr.all, Max)
     and then
       (declare
          Non_Blank : constant Positive :=
            First_Non_Space_Ghost (Str, Ptr.all, Max);
          Fst_Num   : constant Positive :=
            (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
        begin
          Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Max))),
     Post =>
       (declare
          Non_Blank : constant Positive :=
            First_Non_Space_Ghost (Str, Ptr.all'Old, Max);
          Fst_Num   : constant Positive :=
            (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
        begin
          not Raw_Unsigned_Overflows_Ghost (Str, Fst_Num, Max)
          and then Res = Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Max)
          and then Ptr.all = Raw_Unsigned_Last_Ghost (Str, Fst_Num, Max));

   --  Same as Scan_Raw_Unsigned, except scans optional leading
   --  blanks, and an optional leading plus sign.
   --
   --  Note: if a minus sign is present, Constraint_Error will be raised.
   --  Note: trailing blanks are not scanned.

   function Slide_To_1 (Str : String) return String
   with Ghost,
       Post =>
         Only_Space_Ghost (Str, Str'First, Str'Last) =
         (for all J in Str'First .. Str'Last =>
            Slide_To_1'Result (J - Str'First + 1) = ' ');
   --  Slides Str so that it starts at 1

   function Slide_If_Necessary (Str : String) return String is
     (if Str'Last = Positive'Last then Slide_To_1 (Str) else Str)
   with Ghost,
       Post =>
         Only_Space_Ghost (Str, Str'First, Str'Last) =
         Only_Space_Ghost (Slide_If_Necessary'Result,
                           Slide_If_Necessary'Result'First,
                           Slide_If_Necessary'Result'Last);
   --  If Str'Last = Positive'Last then slides Str so that it starts at 1

   function Is_Unsigned_Ghost (Str : String) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) = '+' then Non_Blank + 1 else Non_Blank);
      begin
        Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last))
          and then not Raw_Unsigned_Overflows_Ghost (Str, Fst_Num, Str'Last)
          and then Only_Space_Ghost
             (Str, Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last), Str'Last))
   with Ghost,
       Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last,
       Post => True;
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
   with Ghost,
       Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last
       and then Is_Unsigned_Ghost (Str),
       Post => True;
   --  Ghost function that returns True if Val is the value corresponding to
   --  the unsigned number represented by Str.

   function Value_Unsigned
     (Str : String) return Uns
   with Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
     and then Str'Length /= Positive'Last
     and then Is_Unsigned_Ghost (Slide_If_Necessary (Str)),
     Post =>
         Is_Value_Unsigned_Ghost
           (Slide_If_Necessary (Str), Value_Unsigned'Result),
     Subprogram_Variant => (Decreases => Str'First);
   --  Used in computing X'Value (Str) where X is a modular integer type whose
   --  modulus does not exceed the range of System.Unsigned_Types.Unsigned. Str
   --  is the string argument of the attribute. Constraint_Error is raised if
   --  the string is malformed, or if the value is out of range.

private

   -----------------------------
   -- Exponent_Unsigned_Ghost --
   -----------------------------

   function Exponent_Unsigned_Ghost
     (Value : Uns;
      Exp   : Natural;
      Base  : Uns := 10) return Uns_Option
   is
      (if Exp = 0 or Value = 0 then (Overflow => False, Value => Value)
       elsif Scan_Overflows_Ghost (0, Base, Value) then (Overflow => True)
       else Exponent_Unsigned_Ghost (Value * Base, Exp - 1, Base));

   -----------------------------
   -- Scan_Based_Number_Ghost --
   -----------------------------

   function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0) return Uns_Option
   is
      (if From > To then (Overflow => False, Value => Acc)
       elsif Str (From) = '_'
       then Scan_Based_Number_Ghost (Str, From + 1, To, Base, Acc)
       elsif Scan_Overflows_Ghost
         (Hexa_To_Unsigned_Ghost (Str (From)), Base, Acc)
       then (Overflow => True)
       else Scan_Based_Number_Ghost
         (Str, From + 1, To, Base,
          Base * Acc + Hexa_To_Unsigned_Ghost (Str (From))));

   ----------------
   -- Slide_To_1 --
   ----------------

   function Slide_To_1 (Str : String) return String is
      (declare
         Res : constant String (1 .. Str'Length) := Str;
       begin
         Res);

end System.Value_U;
