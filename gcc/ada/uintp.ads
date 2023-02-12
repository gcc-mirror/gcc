------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Support for universal integer arithmetic

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file uintp.h

with Alloc;
with Table;
pragma Elaborate_All (Table);
with Types; use Types;

package Uintp is

   -------------------------------------------------
   -- Basic Types and Constants for Uintp Package --
   -------------------------------------------------

   type Uint is private;
   --  The basic universal integer type

   No_Uint : constant Uint;
   --  A constant value indicating a missing or unset Uint value

   Uint_0   : constant Uint;
   Uint_1   : constant Uint;
   Uint_2   : constant Uint;
   Uint_3   : constant Uint;
   Uint_4   : constant Uint;
   Uint_5   : constant Uint;
   Uint_6   : constant Uint;
   Uint_7   : constant Uint;
   Uint_8   : constant Uint;
   Uint_9   : constant Uint;
   Uint_10  : constant Uint;
   Uint_11  : constant Uint;
   Uint_12  : constant Uint;
   Uint_13  : constant Uint;
   Uint_14  : constant Uint;
   Uint_15  : constant Uint;
   Uint_16  : constant Uint;
   Uint_24  : constant Uint;
   Uint_31  : constant Uint;
   Uint_32  : constant Uint;
   Uint_63  : constant Uint;
   Uint_64  : constant Uint;
   Uint_80  : constant Uint;
   Uint_127 : constant Uint;
   Uint_128 : constant Uint;
   Uint_256 : constant Uint;

   Uint_Minus_1   : constant Uint;
   Uint_Minus_2   : constant Uint;
   Uint_Minus_3   : constant Uint;
   Uint_Minus_4   : constant Uint;
   Uint_Minus_5   : constant Uint;
   Uint_Minus_6   : constant Uint;
   Uint_Minus_7   : constant Uint;
   Uint_Minus_8   : constant Uint;
   Uint_Minus_9   : constant Uint;
   Uint_Minus_12  : constant Uint;
   Uint_Minus_18  : constant Uint;
   Uint_Minus_31  : constant Uint;
   Uint_Minus_36  : constant Uint;
   Uint_Minus_63  : constant Uint;
   Uint_Minus_76  : constant Uint;
   Uint_Minus_80  : constant Uint;
   Uint_Minus_127 : constant Uint;
   Uint_Minus_128 : constant Uint;

   --  Functions for detecting No_Uint. Note that clients of this package
   --  cannot use "=" and "/=" to compare with No_Uint; they must use No
   --  and Present instead.

   function No (X : Uint) return Boolean is (X = No_Uint);
   --  Note that this is using the predefined "=", not the "=" declared below,
   --  which would blow up on No_Uint.

   function Present (X : Uint) return Boolean is (not No (X));

   subtype Valid_Uint is Uint with Predicate => Present (Valid_Uint);
   subtype Unat is Valid_Uint with Predicate => Unat >= Uint_0; -- natural
   subtype Upos is Valid_Uint with Predicate => Upos >= Uint_1; -- positive
   subtype Nonzero_Uint is Valid_Uint with Predicate => Nonzero_Uint /= Uint_0;
   subtype Unegative is Valid_Uint with Predicate => Unegative < Uint_0;
   subtype Ubool is Valid_Uint with
     Predicate => Ubool = Uint_0 or else Ubool = Uint_1;
   subtype Opt_Ubool is Uint with
     Predicate => No (Opt_Ubool) or else Opt_Ubool in Ubool;

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize Uint tables. Note also that there is no lock routine in this
   --  unit, these are among the few tables that can be expanded during
   --  gigi processing.

   function UI_Abs (Right : Valid_Uint) return Unat;
   pragma Inline (UI_Abs);
   --  Returns abs function of universal integer

   function UI_Add (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint;
   function UI_Add (Left : Int;  Right : Valid_Uint) return Valid_Uint;
   function UI_Add (Left : Valid_Uint; Right : Int)  return Valid_Uint;
   --  Returns sum of two integer values

   function UI_Decimal_Digits_Hi (U : Valid_Uint) return Nat;
   --  Returns an estimate of the number of decimal digits required to
   --  represent the absolute value of U. This estimate is correct or high,
   --  i.e. it never returns a value that is too low. The accuracy of the
   --  estimate affects only the effectiveness of comparison optimizations
   --  in Urealp.

   function UI_Decimal_Digits_Lo (U : Valid_Uint) return Nat;
   --  Returns an estimate of the number of decimal digits required to
   --  represent the absolute value of U. This estimate is correct or low,
   --  i.e. it never returns a value that is too high. The accuracy of the
   --  estimate affects only the effectiveness of comparison optimizations
   --  in Urealp.

   function UI_Div (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint;
   function UI_Div (Left : Int;  Right : Nonzero_Uint) return Valid_Uint;
   function UI_Div (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint;
   --  Returns quotient of two integer values. Fatal error if Right = 0

   function UI_Eq (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Eq (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Eq (Left : Valid_Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Eq);
   --  Compares integer values for equality

   function UI_Expon (Left : Valid_Uint; Right : Unat) return Valid_Uint;
   function UI_Expon (Left : Int;  Right : Unat) return Valid_Uint;
   function UI_Expon (Left : Valid_Uint; Right : Nat)  return Valid_Uint;
   function UI_Expon (Left : Int;  Right : Nat)  return Valid_Uint;
   --  Returns result of exponentiating two integer values.
   --  Fatal error if Right is negative.

   function UI_GCD (Uin, Vin : Valid_Uint) return Valid_Uint;
   --  Computes GCD of input values. Assumes Uin >= Vin >= 0

   function UI_Ge (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Ge (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Ge (Left : Valid_Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Ge);
   --  Compares integer values for greater than or equal

   function UI_Gt (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Gt (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Gt (Left : Valid_Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Gt);
   --  Compares integer values for greater than

   function UI_Is_In_Int_Range (Input : Valid_Uint) return Boolean;
   pragma Inline (UI_Is_In_Int_Range);
   --  Determines if universal integer is in Int range.

   function UI_Le (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Le (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Le (Left : Valid_Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Le);
   --  Compares integer values for less than or equal

   function UI_Lt (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Lt (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Lt (Left : Valid_Uint; Right : Int)  return Boolean;
   --  Compares integer values for less than

   function UI_Max (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint;
   function UI_Max (Left : Int;  Right : Valid_Uint) return Valid_Uint;
   function UI_Max (Left : Valid_Uint; Right : Int)  return Valid_Uint;
   --  Returns maximum of two integer values

   function UI_Min (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint;
   function UI_Min (Left : Int;  Right : Valid_Uint) return Valid_Uint;
   function UI_Min (Left : Valid_Uint; Right : Int)  return Valid_Uint;
   --  Returns minimum of two integer values

   function UI_Mod (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint;
   function UI_Mod (Left : Int;  Right : Nonzero_Uint) return Valid_Uint;
   function UI_Mod (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint;
   pragma Inline (UI_Mod);
   --  Returns mod function of two integer values

   function UI_Mul (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint;
   function UI_Mul (Left : Int;  Right : Valid_Uint) return Valid_Uint;
   function UI_Mul (Left : Valid_Uint; Right : Int)  return Valid_Uint;
   --  Returns product of two integer values

   function UI_Ne (Left : Valid_Uint; Right : Valid_Uint) return Boolean;
   function UI_Ne (Left : Int;  Right : Valid_Uint) return Boolean;
   function UI_Ne (Left : Valid_Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Ne);
   --  Compares integer values for inequality

   function UI_Negate (Right : Valid_Uint) return Valid_Uint;
   pragma Inline (UI_Negate);
   --  Returns negative of universal integer

   function UI_Rem (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint;
   function UI_Rem (Left : Int;  Right : Nonzero_Uint) return Valid_Uint;
   function UI_Rem (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint;
   --  Returns rem of two integer values

   function UI_Sub (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint;
   function UI_Sub (Left : Int;  Right : Valid_Uint) return Valid_Uint;
   function UI_Sub (Left : Valid_Uint; Right : Int)  return Valid_Uint;
   pragma Inline (UI_Sub);
   --  Returns difference of two integer values

   function UI_From_Int (Input : Int) return Valid_Uint;
   --  Converts Int value to universal integer form

   generic
      type In_T is range <>;
   function UI_From_Integral (Input : In_T) return Valid_Uint;
   --  Likewise, but converts from any integer type. Must not be applied to
   --  biased types (instantiation will provide a warning if actual is a biased
   --  type).

   function UI_From_CC (Input : Char_Code) return Valid_Uint;
   --  Converts Char_Code value to universal integer form

   function UI_To_Int (Input : Valid_Uint) return Int;
   --  Converts universal integer value to Int. Constraint_Error if value is
   --  not in appropriate range.

   type Unsigned_64 is mod 2**64;
   function UI_To_Unsigned_64 (Input : Valid_Uint) return Unsigned_64;
   --  Converts universal integer value to Unsigned_64. Constraint_Error if
   --  value is not in appropriate range.

   function UI_To_CC (Input : Valid_Uint) return Char_Code;
   --  Converts universal integer value to Char_Code. Constraint_Error if value
   --  is not in Char_Code range.

   function Num_Bits (Input : Valid_Uint) return Nat;
   --  Approximate number of binary bits in given universal integer. This
   --  function is used for capacity checks, and it can be one bit off
   --  without affecting its usage.

   ---------------------
   -- Output Routines --
   ---------------------

   type UI_Format is (Hex, Decimal, Auto);
   --  Used to determine whether UI_Image/UI_Write output is in hexadecimal
   --  or decimal format. Auto, the default setting, lets the routine make a
   --  decision based on the value.

   UI_Image_Max    : constant := 1024;
   UI_Image_Buffer : String (1 .. UI_Image_Max);
   UI_Image_Length : Natural;
   --  Buffer used for UI_Image as described below

   procedure UI_Image (Input : Uint; Format : UI_Format := Auto);
   --  Places a representation of Uint, consisting of a possible minus sign,
   --  followed by the value in UI_Image_Buffer. The form of the value is an
   --  integer literal in either decimal (no base) or hexadecimal (base 16)
   --  format. If Hex is True on entry, then hex mode is forced, otherwise
   --  UI_Image makes a guess at which output format is more convenient. The
   --  value must fit in UI_Image_Buffer. The actual length of the result is
   --  returned in UI_Image_Length. If necessary to meet this requirement, the
   --  result is an approximation of the proper value, using an exponential
   --  format. The image of No_Uint is output as a single question mark.

   function UI_Image (Input : Uint; Format : UI_Format := Auto) return String;
   --  Functional form, in which the result is returned as a string. This call
   --  also leaves the result in UI_Image_Buffer/Length as described above.

   procedure UI_Write (Input : Uint; Format : UI_Format := Auto);
   --  Writes a representation of Uint, consisting of a possible minus sign,
   --  followed by the value to the output file. The form of the value is an
   --  integer literal in either decimal (no base) or hexadecimal (base 16)
   --  format as appropriate. UI_Format shows which format to use. Auto, the
   --  default, asks UI_Write to make a guess at which output format will be
   --  more convenient to read.

   procedure pid (Input : Uint);
   pragma Export (Ada, pid);
   --  Writes representation of Uint in decimal with a terminating line
   --  return. This is intended for use from the debugger.

   procedure pih (Input : Uint);
   pragma Export (Ada, pih);
   --  Writes representation of Uint in hex with a terminating line return.
   --  This is intended for use from the debugger.

   ------------------------
   -- Operator Renamings --
   ------------------------

   function "+" (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint
     renames UI_Add;
   function "+" (Left : Int;  Right : Valid_Uint) return Valid_Uint
     renames UI_Add;
   function "+" (Left : Valid_Uint; Right : Int)  return Valid_Uint
     renames UI_Add;

   function "/" (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
     renames UI_Div;
   function "/" (Left : Int;  Right : Nonzero_Uint) return Valid_Uint
     renames UI_Div;
   function "/" (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint
     renames UI_Div;

   function "*" (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint
     renames UI_Mul;
   function "*" (Left : Int;  Right : Valid_Uint) return Valid_Uint
     renames UI_Mul;
   function "*" (Left : Valid_Uint; Right : Int)  return Valid_Uint
     renames UI_Mul;

   function "-" (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint
     renames UI_Sub;
   function "-" (Left : Int;  Right : Valid_Uint) return Valid_Uint
     renames UI_Sub;
   function "-" (Left : Valid_Uint; Right : Int)  return Valid_Uint
     renames UI_Sub;

   function "**"  (Left : Valid_Uint; Right : Unat) return Valid_Uint
     renames UI_Expon;
   function "**"  (Left : Valid_Uint; Right : Nat)  return Valid_Uint
     renames UI_Expon;
   function "**"  (Left : Int;  Right : Unat) return Valid_Uint
     renames UI_Expon;
   function "**"  (Left : Int;  Right : Nat)  return Valid_Uint
     renames UI_Expon;

   function "abs" (Real : Valid_Uint) return Unat
     renames UI_Abs;

   function "mod" (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
     renames UI_Mod;
   function "mod" (Left : Int;  Right : Nonzero_Uint) return Valid_Uint
     renames UI_Mod;
   function "mod" (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint
     renames UI_Mod;

   function "rem" (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
     renames UI_Rem;
   function "rem" (Left : Int;  Right : Nonzero_Uint) return Valid_Uint
     renames UI_Rem;
   function "rem" (Left : Valid_Uint; Right : Nonzero_Int)  return Valid_Uint
     renames UI_Rem;

   function "-"   (Real : Valid_Uint) return Valid_Uint
     renames UI_Negate;

   function "="   (Left : Valid_Uint; Right : Valid_Uint) return Boolean
     renames UI_Eq;
   function "="   (Left : Int;  Right : Valid_Uint) return Boolean
     renames UI_Eq;
   function "="   (Left : Valid_Uint; Right : Int)  return Boolean
     renames UI_Eq;

   function ">="  (Left : Valid_Uint; Right : Valid_Uint) return Boolean
     renames UI_Ge;
   function ">="  (Left : Int;  Right : Valid_Uint) return Boolean
     renames UI_Ge;
   function ">="  (Left : Valid_Uint; Right : Int)  return Boolean
     renames UI_Ge;

   function ">"   (Left : Valid_Uint; Right : Valid_Uint) return Boolean
     renames UI_Gt;
   function ">"   (Left : Int;  Right : Valid_Uint) return Boolean
     renames UI_Gt;
   function ">"   (Left : Valid_Uint; Right : Int)  return Boolean
     renames UI_Gt;

   function "<="  (Left : Valid_Uint; Right : Valid_Uint) return Boolean
     renames UI_Le;
   function "<="  (Left : Int;  Right : Valid_Uint) return Boolean
     renames UI_Le;
   function "<="  (Left : Valid_Uint; Right : Int)  return Boolean
     renames UI_Le;

   function "<"   (Left : Valid_Uint; Right : Valid_Uint) return Boolean
     renames UI_Lt;
   function "<"   (Left : Int;  Right : Valid_Uint) return Boolean
     renames UI_Lt;
   function "<"   (Left : Valid_Uint; Right : Int)  return Boolean
     renames UI_Lt;

   -----------------------------
   -- Mark/Release Processing --
   -----------------------------

   --  The space used by Uint data is not automatically reclaimed. However, a
   --  mark-release regime is implemented which allows storage to be released
   --  back to a previously noted mark. This is used for example when doing
   --  comparisons, where only intermediate results get stored that do not
   --  need to be saved for future use.

   type Save_Mark is private;

   function Mark return Save_Mark;
   --  Note mark point for future release

   procedure Release (M : Save_Mark);
   --  Release storage allocated since mark was noted

   procedure Release_And_Save (M : Save_Mark; UI : in out Valid_Uint);
   --  Like Release, except that the given Uint value (which is typically among
   --  the data being released) is recopied after the release, so that it is
   --  the most recent item, and UI is updated to point to its copied location.

   procedure Release_And_Save (M : Save_Mark; UI1, UI2 : in out Valid_Uint);
   --  Like Release, except that the given Uint values (which are typically
   --  among the data being released) are recopied after the release, so that
   --  they are the most recent items, and UI1 and UI2 are updated if necessary
   --  to point to the copied locations. This routine is careful to do things
   --  in the right order, so that the values do not clobber one another.

   -----------------------------------
   -- Representation of Uint Values --
   -----------------------------------

private

   type Uint is new Int range Uint_Low_Bound .. Uint_High_Bound;
   for Uint'Size use 32;

   No_Uint : constant Uint := Uint (Uint_Low_Bound);

   --  Uint values are represented as multiple precision integers stored in
   --  a multi-digit format using Base as the base. This value is chosen so
   --  that the product Base*Base is within the range of allowed Int values.

   --  Base is defined to allow efficient execution of the primitive operations
   --  (a0, b0, c0) defined in the section "The Classical Algorithms"
   --  (sec. 4.3.1) of Donald Knuth's "The Art of Computer Programming",
   --  Vol. 2. These algorithms are used in this package. In particular,
   --  the product of two single digits in this base fits in a 32-bit integer.

   Base_Bits : constant := 15;
   --  Number of bits in base value

   Base : constant Int := 2**Base_Bits;

   --  Values in the range -(Base-1) .. Max_Direct are encoded directly as
   --  Uint values by adding a bias value. The value of Max_Direct is chosen
   --  so that a directly represented number always fits in two digits when
   --  represented in base format.

   Min_Direct : constant Int := -(Base - 1);
   Max_Direct : constant Int := (Base - 1) * (Base - 1);

   --  The following values define the bias used to store Uint values which
   --  are in this range, as well as the biased values for the first and last
   --  values in this range. We use a new derived type for these constants to
   --  avoid accidental use of Uint arithmetic on these values, which is never
   --  correct.

   type Ctrl is new Int;

   Uint_Direct_Bias  : constant Ctrl := Ctrl (Uint_Low_Bound) + Ctrl (Base);
   Uint_Direct_First : constant Ctrl := Uint_Direct_Bias + Ctrl (Min_Direct);
   Uint_Direct_Last  : constant Ctrl := Uint_Direct_Bias + Ctrl (Max_Direct);

   Uint_0   : constant Uint := Uint (Uint_Direct_Bias + 0);
   Uint_1   : constant Uint := Uint (Uint_Direct_Bias + 1);
   Uint_2   : constant Uint := Uint (Uint_Direct_Bias + 2);
   Uint_3   : constant Uint := Uint (Uint_Direct_Bias + 3);
   Uint_4   : constant Uint := Uint (Uint_Direct_Bias + 4);
   Uint_5   : constant Uint := Uint (Uint_Direct_Bias + 5);
   Uint_6   : constant Uint := Uint (Uint_Direct_Bias + 6);
   Uint_7   : constant Uint := Uint (Uint_Direct_Bias + 7);
   Uint_8   : constant Uint := Uint (Uint_Direct_Bias + 8);
   Uint_9   : constant Uint := Uint (Uint_Direct_Bias + 9);
   Uint_10  : constant Uint := Uint (Uint_Direct_Bias + 10);
   Uint_11  : constant Uint := Uint (Uint_Direct_Bias + 11);
   Uint_12  : constant Uint := Uint (Uint_Direct_Bias + 12);
   Uint_13  : constant Uint := Uint (Uint_Direct_Bias + 13);
   Uint_14  : constant Uint := Uint (Uint_Direct_Bias + 14);
   Uint_15  : constant Uint := Uint (Uint_Direct_Bias + 15);
   Uint_16  : constant Uint := Uint (Uint_Direct_Bias + 16);
   Uint_24  : constant Uint := Uint (Uint_Direct_Bias + 24);
   Uint_31  : constant Uint := Uint (Uint_Direct_Bias + 31);
   Uint_32  : constant Uint := Uint (Uint_Direct_Bias + 32);
   Uint_63  : constant Uint := Uint (Uint_Direct_Bias + 63);
   Uint_64  : constant Uint := Uint (Uint_Direct_Bias + 64);
   Uint_80  : constant Uint := Uint (Uint_Direct_Bias + 80);
   Uint_127 : constant Uint := Uint (Uint_Direct_Bias + 127);
   Uint_128 : constant Uint := Uint (Uint_Direct_Bias + 128);
   Uint_256 : constant Uint := Uint (Uint_Direct_Bias + 256);

   Uint_Minus_1   : constant Uint := Uint (Uint_Direct_Bias - 1);
   Uint_Minus_2   : constant Uint := Uint (Uint_Direct_Bias - 2);
   Uint_Minus_3   : constant Uint := Uint (Uint_Direct_Bias - 3);
   Uint_Minus_4   : constant Uint := Uint (Uint_Direct_Bias - 4);
   Uint_Minus_5   : constant Uint := Uint (Uint_Direct_Bias - 5);
   Uint_Minus_6   : constant Uint := Uint (Uint_Direct_Bias - 6);
   Uint_Minus_7   : constant Uint := Uint (Uint_Direct_Bias - 7);
   Uint_Minus_8   : constant Uint := Uint (Uint_Direct_Bias - 8);
   Uint_Minus_9   : constant Uint := Uint (Uint_Direct_Bias - 9);
   Uint_Minus_12  : constant Uint := Uint (Uint_Direct_Bias - 12);
   Uint_Minus_18  : constant Uint := Uint (Uint_Direct_Bias - 18);
   Uint_Minus_31  : constant Uint := Uint (Uint_Direct_Bias - 31);
   Uint_Minus_36  : constant Uint := Uint (Uint_Direct_Bias - 36);
   Uint_Minus_63  : constant Uint := Uint (Uint_Direct_Bias - 63);
   Uint_Minus_76  : constant Uint := Uint (Uint_Direct_Bias - 76);
   Uint_Minus_80  : constant Uint := Uint (Uint_Direct_Bias - 80);
   Uint_Minus_127 : constant Uint := Uint (Uint_Direct_Bias - 127);
   Uint_Minus_128 : constant Uint := Uint (Uint_Direct_Bias - 128);

   Uint_Max_Simple_Mul : constant := Uint_Direct_Bias + 2**15;
   --  If two values are directly represented and less than or equal to this
   --  value, then we know the product fits in a 32-bit integer. This allows
   --  UI_Mul to efficiently compute the product in this case.

   type Save_Mark is record
      Save_Uint   : Valid_Uint;
      Save_Udigit : Int;
   end record;

   --  Values outside the range that is represented directly are stored using
   --  two tables. The secondary table Udigits contains sequences of Int values
   --  consisting of the digits of the number in a radix Base system. The
   --  digits are stored from most significant to least significant with the
   --  first digit only carrying the sign.

   --  There is one entry in the primary Uints table for each distinct Uint
   --  value. This table entry contains the length (number of digits) and
   --  a starting offset of the value in the Udigits table.

   Uint_First_Entry : constant Uint := Uint (Uint_Table_Start);

   --  Some subprograms defined in this package manipulate the Udigits table
   --  directly, while for others it is more convenient to work with locally
   --  defined arrays of the digits of the Universal Integers. The type
   --  UI_Vector is declared in the package body for this purpose and some
   --  internal subprograms used for converting from one to the other are
   --  defined.

   type Uint_Entry is record
      Length : aliased Pos;
      --  Length of entry in Udigits table in digits (i.e. in words)

      Loc : aliased Int;
      --  Starting location in Udigits table of this Uint value
   end record;

   package Uints is new Table.Table (
     Table_Component_Type => Uint_Entry,
     Table_Index_Type     => Uint'Base,
     Table_Low_Bound      => Uint_First_Entry,
     Table_Initial        => Alloc.Uints_Initial,
     Table_Increment      => Alloc.Uints_Increment,
     Table_Name           => "Uints");

   package Udigits is new Table.Table (
     Table_Component_Type => Int,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Udigits_Initial,
     Table_Increment      => Alloc.Udigits_Increment,
     Table_Name           => "Udigits");

   --  Note: the reason these tables are defined here in the private part of
   --  the spec, rather than in the body, is that they are referenced directly
   --  by gigi.

end Uintp;
