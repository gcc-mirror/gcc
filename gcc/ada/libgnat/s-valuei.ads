------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ I                        --
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

--  This package contains routines for scanning signed integer values for use
--  in Text_IO.Integer_IO, and the Value attribute.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

with System.Val_Util; use System.Val_Util;

generic

   type Int is range <>;

   type Uns is mod <>;

   with procedure Scan_Raw_Unsigned
          (Str : String;
           Ptr : not null access Integer;
           Max : Integer;
           Res : out Uns);

   --  Additional parameters for ghost subprograms used inside contracts

   type Uns_Option is private;
   with function Wrap_Option (Value : Uns) return Uns_Option
      with Ghost;
   with function Is_Raw_Unsigned_Format_Ghost (Str : String) return Boolean
      with Ghost;
   with function Raw_Unsigned_Overflows_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
      with Ghost;
   with function Scan_Raw_Unsigned_Ghost
     (Str      : String;
      From, To : Integer)
      return Uns
      with Ghost;
   with function Raw_Unsigned_Last_Ghost
     (Str      : String;
      From, To : Integer)
      return Positive
      with Ghost;
   with function Only_Decimal_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
      with Ghost;
   with function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
      return Uns_Option
      with Ghost;

package System.Value_I is
   pragma Preelaborate;

   function Uns_Is_Valid_Int (Minus : Boolean; Uval : Uns) return Boolean is
     (if Minus then Uval <= Uns (Int'Last) + 1
      else Uval <= Uns (Int'Last))
   with Ghost,
     Post => True;
   --  Return True if Uval (or -Uval when Minus is True) is a valid number of
   --  type Int.

   function Is_Int_Of_Uns
     (Minus : Boolean;
      Uval  : Uns;
      Val   : Int)
      return Boolean
   is
     (if Minus and then Uval = Uns (Int'Last) + 1 then Val = Int'First
      elsif Minus then Val = -(Int (Uval))
      else Val = Int (Uval))
   with
     Ghost,
     Pre  => Uns_Is_Valid_Int (Minus, Uval),
     Post => True;
   --  Return True if Uval (or -Uval when Minus is True) is equal to Val

   function Abs_Uns_Of_Int (Val : Int) return Uns is
     (if Val = Int'First then Uns (Int'Last) + 1
      elsif Val < 0 then Uns (-Val)
      else Uns (Val))
   with Ghost;
   --  Return the unsigned absolute value of Val

   procedure Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Int)
   with
     Pre  => Str'Last /= Positive'Last
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       and then (Ptr.all > Max
                 or else (Ptr.all >= Str'First and then Max <= Str'Last))
       and then not Only_Space_Ghost (Str, Ptr.all, Max)
       and then
         (declare
            Non_Blank : constant Positive := First_Non_Space_Ghost
              (Str, Ptr.all, Max);
            Fst_Num   : constant Positive :=
              (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1
               else Non_Blank);
          begin
            Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Max))
              and then not Raw_Unsigned_Overflows_Ghost (Str, Fst_Num, Max)
              and then Uns_Is_Valid_Int
                (Minus => Str (Non_Blank) = '-',
                 Uval  => Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Max))),
    Post =>
      (declare
         Non_Blank : constant Positive := First_Non_Space_Ghost
           (Str, Ptr.all'Old, Max);
         Fst_Num   : constant Positive :=
           (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1
            else Non_Blank);
         Uval      : constant Uns :=
            Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Max);
       begin
         Is_Int_Of_Uns (Minus => Str (Non_Blank) = '-',
                        Uval  => Uval,
                        Val   => Res)
           and then Ptr.all = Raw_Unsigned_Last_Ghost (Str, Fst_Num, Max));
   --  This procedure scans the string starting at Str (Ptr.all) for a valid
   --  integer according to the syntax described in (RM 3.5(43)). The substring
   --  scanned extends no further than Str (Max). There are three cases for the
   --  return:
   --
   --  If a valid integer is found after scanning past any initial spaces, then
   --  Ptr.all is updated past the last character of the integer (but trailing
   --  spaces are not scanned out).
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
   --  positioned in Text_Io.Get
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case.

   function Slide_To_1 (Str : String) return String
   with
     Ghost,
     Post =>
       Only_Space_Ghost (Str, Str'First, Str'Last) =
         (for all J in Str'First .. Str'Last =>
            Slide_To_1'Result (J - Str'First + 1) = ' ');
   --  Slides Str so that it starts at 1

   function Slide_If_Necessary (Str : String) return String is
     (if Str'Last = Positive'Last then Slide_To_1 (Str) else Str)
   with
     Ghost,
     Post =>
       Only_Space_Ghost (Str, Str'First, Str'Last) =
       Only_Space_Ghost (Slide_If_Necessary'Result,
                         Slide_If_Necessary'Result'First,
                         Slide_If_Necessary'Result'Last);
   --  If Str'Last = Positive'Last then slides Str so that it starts at 1

   function Is_Integer_Ghost (Str : String) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1 else Non_Blank);
      begin
        Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last))
          and then not Raw_Unsigned_Overflows_Ghost (Str, Fst_Num, Str'Last)
          and then
            Uns_Is_Valid_Int
              (Minus => Str (Non_Blank) = '-',
               Uval  => Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last))
          and then Only_Space_Ghost
            (Str, Raw_Unsigned_Last_Ghost (Str, Fst_Num, Str'Last), Str'Last))
   with
     Ghost,
     Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last,
     Post => True;
   --  Ghost function that determines if Str has the correct format for a
   --  signed number, consisting in some blank characters, an optional
   --  sign, a raw unsigned number which does not overflow and then some
   --  more blank characters.

   function Is_Value_Integer_Ghost (Str : String; Val : Int) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1 else Non_Blank);
        Uval      : constant Uns :=
          Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last);
      begin
        Is_Int_Of_Uns (Minus => Str (Non_Blank) = '-',
                       Uval  => Uval,
                       Val   => Val))
   with
     Ghost,
     Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last
       and then Is_Integer_Ghost (Str),
     Post => True;
   --  Ghost function that returns True if Val is the value corresponding to
   --  the signed number represented by Str.

   function Value_Integer (Str : String) return Int
   with
     Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Length /= Positive'Last
       and then Is_Integer_Ghost (Slide_If_Necessary (Str)),
     Post => Is_Value_Integer_Ghost
       (Slide_If_Necessary (Str), Value_Integer'Result),
     Subprogram_Variant => (Decreases => Str'First);
   --  Used in computing X'Value (Str) where X is a signed integer type whose
   --  base range does not exceed the base range of Integer. Str is the string
   --  argument of the attribute. Constraint_Error is raised if the string is
   --  malformed, or if the value is out of range.

   procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int)
   with
     Ghost,
     Pre  => Str'Last /= Positive'Last
       and then Str'Length >= 2
       and then Str (Str'First) in ' ' | '-'
       and then (Str (Str'First) = '-') = (Val < 0)
       and then Only_Decimal_Ghost (Str, Str'First + 1, Str'Last)
       and then Scan_Based_Number_Ghost (Str, Str'First + 1, Str'Last)
         = Wrap_Option (Abs_Uns_Of_Int (Val)),
     Post => Is_Integer_Ghost (Slide_If_Necessary (Str))
       and then Value_Integer (Str) = Val;
   --  Ghost lemma used in the proof of 'Image implementation, to prove that
   --  the result of Value_Integer on a decimal string is the same as the
   --  signing the result of Scan_Based_Number_Ghost.

private

   ----------------
   -- Slide_To_1 --
   ----------------

   function Slide_To_1 (Str : String) return String is
     (declare
        Res : constant String (1 .. Str'Length) := Str;
      begin
        Res);

end System.Value_I;
