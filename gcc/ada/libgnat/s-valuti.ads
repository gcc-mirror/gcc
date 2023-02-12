------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
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

--  This package provides some common utilities used by the s-valxxx files

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with System.Case_Util;

package System.Val_Util
  with SPARK_Mode, Pure
is
   pragma Unevaluated_Use_Of_Old (Allow);

   procedure Bad_Value (S : String)
   with
     Depends => (null => S);
   pragma No_Return (Bad_Value);
   --  Raises constraint error with message: bad input for 'Value: "xxx"

   function Only_Space_Ghost (S : String; From, To : Integer) return Boolean is
      (for all J in From .. To => S (J) = ' ')
   with
     Ghost,
     Pre  => From > To or else (From >= S'First and then To <= S'Last),
     Post => True;
   --  Ghost function that returns True if S has only space characters from
   --  index From to index To.

   function First_Non_Space_Ghost
     (S        : String;
      From, To : Integer) return Positive
   with
     Ghost,
     Pre  => From in S'Range
       and then To in S'Range
       and then not Only_Space_Ghost (S, From, To),
     Post => First_Non_Space_Ghost'Result in From .. To
       and then S (First_Non_Space_Ghost'Result) /= ' '
       and then Only_Space_Ghost
         (S, From, First_Non_Space_Ghost'Result - 1);
   --  Ghost function that returns the index of the first non-space character
   --  in S, which necessarily exists given the precondition on S.

   procedure Normalize_String
     (S    : in out String;
      F, L : out Integer)
   with
     Post => (if Only_Space_Ghost (S'Old, S'First, S'Last) then
                F > L
              else
                F >= S'First
                  and then L <= S'Last
                  and then F <= L
                  and then Only_Space_Ghost (S'Old, S'First, F - 1)
                  and then S'Old (F) /= ' '
                  and then S'Old (L) /= ' '
                  and then
                    (if L < S'Last then
                      Only_Space_Ghost (S'Old, L + 1, S'Last))
                  and then
                    (if S'Old (F) /= ''' then
                      (for all J in S'Range =>
                        (if J in F .. L then
                           S (J) = System.Case_Util.To_Upper (S'Old (J))
                         else
                           S (J) = S'Old (J)))));
   --  This procedure scans the string S setting F to be the index of the first
   --  non-blank character of S and L to be the index of the last non-blank
   --  character of S. Any lower case characters present in S will be folded to
   --  their upper case equivalent except for character literals. If S consists
   --  of entirely blanks (including when S = "") then we return with F > L.

   procedure Scan_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Minus : out Boolean;
      Start : out Positive)
   with
     Pre  =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
       and then not Only_Space_Ghost (Str, Ptr.all, Max)
       and then
         (declare
            F : constant Positive :=
              First_Non_Space_Ghost (Str, Ptr.all, Max);
          begin
            (if Str (F) in '+' | '-' then
               F <= Max - 1 and then Str (F + 1) /= ' ')),
     Post =>
       (declare
          F : constant Positive :=
            First_Non_Space_Ghost (Str, Ptr.all'Old, Max);
        begin
          Minus = (Str (F) = '-')
            and then Ptr.all = (if Str (F) in '+' | '-' then F + 1 else F)
            and then Start = F);
   --  The Str, Ptr, Max parameters are as for the scan routines (Str is the
   --  string to be scanned starting at Ptr.all, and Max is the index of the
   --  last character in the string). Scan_Sign first scans out any initial
   --  blanks, raising Constraint_Error if the field is all blank. It then
   --  checks for and skips an initial plus or minus, requiring a non-blank
   --  character to follow (Constraint_Error is raised if plus or minus appears
   --  at the end of the string or with a following blank). Minus is set True
   --  if a minus sign was skipped, and False otherwise. On exit Ptr.all points
   --  to the character after the sign, or to the first non-blank character
   --  if no sign is present. Start is set to the point to the first non-blank
   --  character (sign or digit after it).
   --
   --  Note: if Str is null, i.e. if Max is less than Ptr, then this is a
   --  special case of an all-blank string, and Ptr is unchanged, and hence
   --  is greater than Max as required in this case. Constraint_Error is also
   --  raised in this case.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.

   procedure Scan_Plus_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Start : out Positive)
   with
     Pre  =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
       and then not Only_Space_Ghost (Str, Ptr.all, Max)
       and then
         (declare
            F : constant Positive :=
              First_Non_Space_Ghost (Str, Ptr.all, Max);
          begin
            (if Str (F) = '+' then
               F <= Max - 1 and then Str (F + 1) /= ' ')),
     Post =>
       (declare
          F : constant Positive :=
            First_Non_Space_Ghost (Str, Ptr.all'Old, Max);
        begin
          Ptr.all = (if Str (F) = '+' then F + 1 else F)
            and then Start = F);
   --  Same as Scan_Sign, but allows only plus, not minus. This is used for
   --  modular types.

   function Only_Number_Ghost (Str : String; From, To : Integer) return Boolean
   is
      (for all J in From .. To => Str (J) in '0' .. '9' | '_')
   with
     Ghost,
     Pre => From > To or else (From >= Str'First and then To <= Str'Last);
   --  Ghost function that returns True if S has only number characters from
   --  index From to index To.

   function Last_Number_Ghost (Str : String) return Positive
   with
     Ghost,
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
                         (if Str (J) = '_' then Str (J + 1) /= '_'))))
   with
     Ghost;
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
                 Str (Start) in '0' .. '9')))
   with
     Ghost;
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
         begin Is_Natural_Format_Ghost (Str (Start .. Str'Last))))
   with
     Ghost;
   --  Ghost function that determines if Str has the correct format for an
   --  optional exponent, that is, either it does not start as an exponent, or
   --  it is in a correct format for a natural number.

   function Scan_Natural_Ghost
     (Str : String;
      P   : Natural;
      Acc : Natural)
      return Natural
   with
     Ghost,
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
     Ghost,
     Pre  => Str'Last < Natural'Last
       and then Starts_As_Exponent_Format_Ghost (Str, Real),
     Post => (if not Real then Scan_Exponent_Ghost'Result >= 0);
   --  Ghost function that scans an exponent

   procedure Scan_Exponent
     (Str  : String;
      Ptr  : not null access Integer;
      Max  : Integer;
      Exp  : out Integer;
      Real : Boolean := False)
   with
     Pre =>
       --  Ptr.all .. Max is either an empty range, or a valid range in Str
       (Ptr.all > Max or else (Ptr.all >= Str'First and then Max <= Str'Last))
         and then Max < Natural'Last
         and then Is_Opt_Exponent_Format_Ghost (Str (Ptr.all .. Max), Real),
     Post =>
       (if Starts_As_Exponent_Format_Ghost (Str (Ptr.all'Old .. Max), Real)
        then Exp = Scan_Exponent_Ghost (Str (Ptr.all'Old .. Max), Real)
          and then
          (if Str (Ptr.all'Old + 1) in '-' | '+' then
             Ptr.all = Last_Number_Ghost (Str (Ptr.all'Old + 2 .. Max)) + 1
           else Ptr.all = Last_Number_Ghost (Str (Ptr.all'Old + 1 .. Max)) + 1)
        else Exp = 0 and Ptr.all = Ptr.all'Old);
   --  Called to scan a possible exponent. Str, Ptr, Max are as described above
   --  for Scan_Sign. If Ptr.all < Max and Str (Ptr.all) = 'E' or 'e', then an
   --  exponent is scanned out, with the exponent value returned in Exp, and
   --  Ptr.all updated to point past the exponent. If the exponent field is
   --  incorrectly formed or not present, then Ptr.all is unchanged, and the
   --  returned exponent value is zero. Real indicates whether a minus sign
   --  is permitted (True = permitted). Very large exponents are handled by
   --  returning a suitable large value. If the base is zero, then any value
   --  is allowed, and otherwise the large value will either cause underflow
   --  or overflow during the scaling process which is fine.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.

   procedure Scan_Trailing_Blanks (Str : String; P : Positive)
   with
     Pre => P >= Str'First
       and then Only_Space_Ghost (Str, P, Str'Last);
   --  Checks that the remainder of the field Str (P .. Str'Last) is all
   --  blanks. Raises Constraint_Error if a non-blank character is found.

   pragma Warnings
     (GNATprove, Off, """Ptr"" is not modified",
      Reason => "Ptr is actually modified when raising an exception");
   procedure Scan_Underscore
     (Str : String;
      P   : in out Natural;
      Ptr : not null access Integer;
      Max : Integer;
      Ext : Boolean)
   with
     Pre  => P in Str'Range
       and then Str (P) = '_'
       and then Max in Str'Range
       and then P < Max
       and then
         (if Ext then
            Str (P + 1) in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
          else
            Str (P + 1) in '0' .. '9'),
     Post =>
       P = P'Old + 1
         and then Ptr.all'Old = Ptr.all;
   --  Called if an underscore is encountered while scanning digits. Str (P)
   --  contains the underscore. Ptr is the pointer to be returned to the
   --  ultimate caller of the scan routine, Max is the maximum subscript in
   --  Str, and Ext indicates if extended digits are allowed. In the case
   --  where the underscore is invalid, Constraint_Error is raised with Ptr
   --  set appropriately, otherwise control returns with P incremented past
   --  the underscore.
   --
   --  This routine must not be called with Str'Last = Positive'Last. There is
   --  no check for this case, the caller must ensure this condition is met.
   pragma Warnings (GNATprove, On, """Ptr"" is not modified");

   --  Bundle Uns type with other types, constants and subprograms used in
   --  ghost code, so that this package can be instantiated once and used
   --  multiple times as generic formal for a given Uns type.
   generic
      type Uns is mod <>;
      type P_Uns_Option is private with Ghost;
      with function P_Wrap_Option (Value : Uns) return P_Uns_Option
        with Ghost;
      with function P_Hexa_To_Unsigned_Ghost (X : Character) return Uns
        with Ghost;
      with function P_Scan_Overflows_Ghost
        (Digit : Uns;
         Base  : Uns;
         Acc   : Uns) return Boolean
        with Ghost;
      with function P_Is_Raw_Unsigned_Format_Ghost
        (Str : String) return Boolean
        with Ghost;
      with function P_Scan_Split_No_Overflow_Ghost
        (Str      : String;
         From, To : Integer)
      return Boolean
        with Ghost;
      with function P_Raw_Unsigned_No_Overflow_Ghost
        (Str      : String;
         From, To : Integer)
      return Boolean
        with Ghost;

      with function P_Exponent_Unsigned_Ghost
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10) return P_Uns_Option
        with Ghost;
      with procedure P_Lemma_Exponent_Unsigned_Ghost_Base
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
        with Ghost;
      with procedure P_Lemma_Exponent_Unsigned_Ghost_Overflow
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
        with Ghost;
      with procedure P_Lemma_Exponent_Unsigned_Ghost_Step
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
        with Ghost;

      with function P_Scan_Raw_Unsigned_Ghost
        (Str      : String;
         From, To : Integer)
      return Uns
        with Ghost;
      with procedure P_Lemma_Scan_Based_Number_Ghost_Base
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
        with Ghost;
      with procedure P_Lemma_Scan_Based_Number_Ghost_Underscore
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
        with Ghost;
      with procedure P_Lemma_Scan_Based_Number_Ghost_Overflow
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
        with Ghost;
      with procedure P_Lemma_Scan_Based_Number_Ghost_Step
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
        with Ghost;

      with function P_Raw_Unsigned_Last_Ghost
        (Str      : String;
         From, To : Integer)
      return Positive
        with Ghost;
      with function P_Only_Decimal_Ghost
        (Str      : String;
         From, To : Integer)
      return Boolean
        with Ghost;
      with function P_Scan_Based_Number_Ghost
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
      return P_Uns_Option
        with Ghost;
      with function P_Is_Unsigned_Ghost (Str : String) return Boolean
        with Ghost;
      with function P_Is_Value_Unsigned_Ghost
        (Str : String;
         Val : Uns) return Boolean
        with Ghost;

      with procedure P_Prove_Scan_Only_Decimal_Ghost
        (Str : String;
         Val : Uns)
        with Ghost;
      with procedure P_Prove_Scan_Based_Number_Ghost_Eq
        (Str1, Str2 : String;
         From, To   : Integer;
         Base       : Uns := 10;
         Acc        : Uns := 0)
        with Ghost;

   package Uns_Params is
      subtype Uns_Option is P_Uns_Option with Ghost;
      function Wrap_Option (Value : Uns) return Uns_Option renames
        P_Wrap_Option;
      function Hexa_To_Unsigned_Ghost
        (X : Character) return Uns
         renames P_Hexa_To_Unsigned_Ghost;
      function Scan_Overflows_Ghost
        (Digit : Uns;
         Base  : Uns;
         Acc   : Uns) return Boolean
         renames P_Scan_Overflows_Ghost;
      function Is_Raw_Unsigned_Format_Ghost
        (Str : String) return Boolean
         renames P_Is_Raw_Unsigned_Format_Ghost;
      function Scan_Split_No_Overflow_Ghost
        (Str      : String;
         From, To : Integer) return Boolean
         renames P_Scan_Split_No_Overflow_Ghost;
      function Raw_Unsigned_No_Overflow_Ghost
        (Str      : String;
         From, To : Integer) return Boolean
         renames P_Raw_Unsigned_No_Overflow_Ghost;

      function Exponent_Unsigned_Ghost
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10) return Uns_Option
         renames P_Exponent_Unsigned_Ghost;
      procedure Lemma_Exponent_Unsigned_Ghost_Base
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
         renames P_Lemma_Exponent_Unsigned_Ghost_Base;
      procedure Lemma_Exponent_Unsigned_Ghost_Overflow
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
         renames P_Lemma_Exponent_Unsigned_Ghost_Overflow;
      procedure Lemma_Exponent_Unsigned_Ghost_Step
        (Value : Uns;
         Exp   : Natural;
         Base  : Uns := 10)
         renames P_Lemma_Exponent_Unsigned_Ghost_Step;

      function Scan_Raw_Unsigned_Ghost
        (Str      : String;
         From, To : Integer) return Uns
         renames P_Scan_Raw_Unsigned_Ghost;
      procedure Lemma_Scan_Based_Number_Ghost_Base
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
         renames P_Lemma_Scan_Based_Number_Ghost_Base;
      procedure Lemma_Scan_Based_Number_Ghost_Underscore
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
         renames P_Lemma_Scan_Based_Number_Ghost_Underscore;
      procedure Lemma_Scan_Based_Number_Ghost_Overflow
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
         renames P_Lemma_Scan_Based_Number_Ghost_Overflow;
      procedure Lemma_Scan_Based_Number_Ghost_Step
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0)
         renames P_Lemma_Scan_Based_Number_Ghost_Step;

      function Raw_Unsigned_Last_Ghost
        (Str      : String;
         From, To : Integer) return Positive
         renames P_Raw_Unsigned_Last_Ghost;
      function Only_Decimal_Ghost
        (Str      : String;
         From, To : Integer) return Boolean
         renames P_Only_Decimal_Ghost;
      function Scan_Based_Number_Ghost
        (Str      : String;
         From, To : Integer;
         Base     : Uns := 10;
         Acc      : Uns := 0) return Uns_Option
         renames P_Scan_Based_Number_Ghost;
      function Is_Unsigned_Ghost (Str : String) return Boolean
         renames P_Is_Unsigned_Ghost;
      function Is_Value_Unsigned_Ghost
        (Str : String;
         Val : Uns) return Boolean
         renames P_Is_Value_Unsigned_Ghost;

      procedure Prove_Scan_Only_Decimal_Ghost
        (Str : String;
         Val : Uns)
         renames P_Prove_Scan_Only_Decimal_Ghost;
      procedure Prove_Scan_Based_Number_Ghost_Eq
        (Str1, Str2 : String;
         From, To   : Integer;
         Base       : Uns := 10;
         Acc        : Uns := 0)
      renames P_Prove_Scan_Based_Number_Ghost_Eq;
   end Uns_Params;

   --  Bundle Int type with other types, constants and subprograms used in
   --  ghost code, so that this package can be instantiated once and used
   --  multiple times as generic formal for a given Int type.
   generic
      type Int is range <>;
      type Uns is mod <>;

      with package P_Uns_Params is new System.Val_Util.Uns_Params
        (Uns => Uns, others => <>)
         with Ghost;

      with function P_Abs_Uns_Of_Int (Val : Int) return Uns
         with Ghost;
      with function P_Is_Int_Of_Uns
        (Minus : Boolean;
         Uval  : Uns;
         Val   : Int)
      return Boolean
         with Ghost;
      with function P_Is_Integer_Ghost (Str : String) return Boolean
         with Ghost;
      with function P_Is_Value_Integer_Ghost
        (Str : String;
         Val : Int) return Boolean
         with Ghost;
      with procedure P_Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int)
         with Ghost;

   package Int_Params is
      package Uns_Params renames P_Uns_Params;
      function Abs_Uns_Of_Int (Val : Int) return Uns renames
        P_Abs_Uns_Of_Int;
      function Is_Int_Of_Uns
        (Minus : Boolean;
         Uval  : Uns;
         Val   : Int)
      return Boolean
         renames P_Is_Int_Of_Uns;
      function Is_Integer_Ghost (Str : String) return Boolean renames
        P_Is_Integer_Ghost;
      function Is_Value_Integer_Ghost
        (Str : String;
         Val : Int) return Boolean
         renames P_Is_Value_Integer_Ghost;
      procedure Prove_Scan_Only_Decimal_Ghost (Str : String; Val : Int) renames
        P_Prove_Scan_Only_Decimal_Ghost;
   end Int_Params;

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

end System.Val_Util;
