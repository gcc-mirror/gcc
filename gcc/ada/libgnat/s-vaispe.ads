------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . V A L U E _ I _ S P E C                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2022-2024, Free Software Foundation, Inc.         --
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
--  verification of the routines for scanning signed integer values.

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

with System.Value_U_Spec;
with System.Val_Spec; use System.Val_Spec;

generic

   type Int is range <>;

   type Uns is mod <>;

   --  Additional parameters for ghost subprograms used inside contracts

   with package U_Spec is new System.Value_U_Spec (Uns => Uns) with Ghost;

package System.Value_I_Spec with
   Ghost,
   SPARK_Mode,
   Always_Terminates
is
   pragma Preelaborate;
   use all type U_Spec.Uns_Option;

   function Uns_Is_Valid_Int (Minus : Boolean; Uval : Uns) return Boolean is
     (if Minus then Uval <= Uns (Int'Last) + 1
      else Uval <= Uns (Int'Last))
   with Post => True;
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
     Pre  => Uns_Is_Valid_Int (Minus, Uval),
     Post => True;
   --  Return True if Uval (or -Uval when Minus is True) is equal to Val

   function Abs_Uns_Of_Int (Val : Int) return Uns is
     (if Val = Int'First then Uns (Int'Last) + 1
      elsif Val < 0 then Uns (-Val)
      else Uns (Val));
   --  Return the unsigned absolute value of Val

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

   function Is_Integer_Ghost (Str : String) return Boolean is
     (declare
        Non_Blank : constant Positive := First_Non_Space_Ghost
          (Str, Str'First, Str'Last);
        Fst_Num   : constant Positive :=
          (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1 else Non_Blank);
      begin
        U_Spec.Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Str'Last))
          and then U_Spec.Raw_Unsigned_No_Overflow_Ghost
             (Str, Fst_Num, Str'Last)
          and then
            Uns_Is_Valid_Int
              (Minus => Str (Non_Blank) = '-',
               Uval  => U_Spec.Scan_Raw_Unsigned_Ghost
                 (Str, Fst_Num, Str'Last))
          and then Only_Space_Ghost
            (Str, U_Spec.Raw_Unsigned_Last_Ghost
             (Str, Fst_Num, Str'Last), Str'Last))
   with
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
          U_Spec.Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Str'Last);
      begin
        Is_Int_Of_Uns (Minus => Str (Non_Blank) = '-',
                       Uval  => Uval,
                       Val   => Val))
   with
     Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Last /= Positive'Last
       and then Is_Integer_Ghost (Str),
     Post => True;
   --  Ghost function that returns True if Val is the value corresponding to
   --  the signed number represented by Str.

   procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int)
   with
     Ghost,
     Pre  => Str'Last /= Positive'Last
       and then Str'Length >= 2
       and then Str (Str'First) in ' ' | '-'
       and then (Str (Str'First) = '-') = (Val < 0)
       and then U_Spec.Only_Decimal_Ghost (Str, Str'First + 1, Str'Last)
       and then U_Spec.Scan_Based_Number_Ghost
         (Str, Str'First + 1, Str'Last)
         = U_Spec.Wrap_Option (Abs_Uns_Of_Int (Val)),
     Post => Is_Integer_Ghost (Slide_If_Necessary (Str))
       and then Is_Value_Integer_Ghost (Str, Val);
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

end System.Value_I_Spec;
