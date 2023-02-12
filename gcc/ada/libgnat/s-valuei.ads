------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ I                        --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains routines for scanning signed integer values for use
--  in Text_IO.Integer_IO, and the Value attribute.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

with System.Val_Util; use System.Val_Util;
with System.Value_I_Spec;

generic

   type Int is range <>;

   type Uns is mod <>;

   with procedure Scan_Raw_Unsigned
          (Str : String;
           Ptr : not null access Integer;
           Max : Integer;
           Res : out Uns);

   --  Additional parameters for ghost subprograms used inside contracts

   with package Uns_Params is new System.Val_Util.Uns_Params
     (Uns => Uns, others => <>)
   with Ghost;

package System.Value_I is
   pragma Preelaborate;
   use all type Uns_Params.Uns_Option;

   package Spec is new System.Value_I_Spec (Int, Uns, Uns_Params);

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
            Uns_Params.Is_Raw_Unsigned_Format_Ghost (Str (Fst_Num .. Max))
              and then Uns_Params.Raw_Unsigned_No_Overflow_Ghost
                (Str, Fst_Num, Max)
              and then Spec.Uns_Is_Valid_Int
                (Minus => Str (Non_Blank) = '-',
                 Uval  => Uns_Params.Scan_Raw_Unsigned_Ghost
                   (Str, Fst_Num, Max))),
    Post =>
      (declare
         Non_Blank : constant Positive := First_Non_Space_Ghost
           (Str, Ptr.all'Old, Max);
         Fst_Num   : constant Positive :=
           (if Str (Non_Blank) in '+' | '-' then Non_Blank + 1
            else Non_Blank);
         Uval      : constant Uns :=
            Uns_Params.Scan_Raw_Unsigned_Ghost (Str, Fst_Num, Max);
       begin
           Spec.Is_Int_Of_Uns (Minus => Str (Non_Blank) = '-',
                               Uval  => Uval,
                               Val   => Res)
           and then Ptr.all = Uns_Params.Raw_Unsigned_Last_Ghost
             (Str, Fst_Num, Max));
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

   function Value_Integer (Str : String) return Int
   with
     Pre => not Only_Space_Ghost (Str, Str'First, Str'Last)
       and then Str'Length /= Positive'Last
       and then Spec.Is_Integer_Ghost (Spec.Slide_If_Necessary (Str)),
     Post => Spec.Is_Value_Integer_Ghost
       (Spec.Slide_If_Necessary (Str), Value_Integer'Result),
     Subprogram_Variant => (Decreases => Str'First);
   --  Used in computing X'Value (Str) where X is a signed integer type whose
   --  base range does not exceed the base range of Integer. Str is the string
   --  argument of the attribute. Constraint_Error is raised if the string is
   --  malformed, or if the value is out of range.

end System.Value_I;
