------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U R E A L P                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Support for universal real arithmetic

with Types; use Types;
with Uintp; use Uintp;

package Urealp is

   ---------------------------------------
   -- Representation of Universal Reals --
   ---------------------------------------

   --  A universal real value is represented by a single value (which is
   --  an index into an internal table). These values are not hashed, so
   --  the equality operator should not be used on Ureal values (instead
   --  use the UR_Eq function).

   --  A Ureal value represents an arbitrary precision universal real value,
   --  stored internally using four components:

   --    the numerator (Uint, always non-negative)
   --    the denominator (Uint, always non-zero, always positive if base = 0)
   --    a real base (Nat, either zero, or in the range 2 .. 16)
   --    a sign flag (Boolean), set if negative

   --  Negative numbers are represented by the sign flag being True.

   --  If the base is zero, then the absolute value of the Ureal is simply
   --  numerator/denominator, where denominator is positive. If the base is
   --  non-zero, then the absolute value is numerator / (base ** denominator).
   --  In that case, since base is positive, (base ** denominator) is also
   --  positive, even when denominator is negative or null.

   --  A normalized Ureal value has base = 0, and numerator/denominator
   --  reduced to lowest terms, with zero itself being represented as 0/1.
   --  This is a canonical format, so that for normalized Ureal values it
   --  is the case that two equal values always have the same denominator
   --  and numerator values.

   --  Note: a value of minus zero is legitimate, and the operations in
   --  Urealp preserve the handling of signed zeroes in accordance with
   --  the rules of IEEE P754 ("IEEE floating point").

   ------------------------------
   -- Types for Urealp Package --
   ------------------------------

   type Ureal is private;
   --  Type used for representation of universal reals

   No_Ureal : constant Ureal;
   --  Constant used to indicate missing or unset Ureal value

   ---------------------
   -- Ureal Constants --
   ---------------------

   function Ureal_0 return Ureal;
   --  Returns value 0.0

   function Ureal_M_0 return Ureal;
   --  Returns value -0.0

   function Ureal_Tenth return Ureal;
   --  Returns value 0.1

   function Ureal_Half return Ureal;
   --  Returns value 0.5

   function Ureal_1 return Ureal;
   --  Returns value 1.0

   function Ureal_2 return Ureal;
   --  Returns value 2.0

   function Ureal_10 return Ureal;
   --  Returns value 10.0

   function Ureal_100 return Ureal;
   --  Returns value 100.0

   function Ureal_2_80 return Ureal;
   --  Returns value 2.0 ** 80

   function Ureal_2_M_80 return Ureal;
   --  Returns value 2.0 ** (-80)

   function Ureal_2_128 return Ureal;
   --  Returns value 2.0 ** 128

   function Ureal_2_M_128 return Ureal;
   --  Returns value 2.0 ** (-128)

   function Ureal_10_36 return Ureal;
   --  Returns value 10.0 ** 36

   function Ureal_M_10_36 return Ureal;
   --  Returns value -10.0 ** 36

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize Ureal tables. Note that Initialize must not be called if
   --  Tree_Read is used. Note also that there is no Lock routine in this
   --  unit. These tables are among the few tables that can be expanded
   --  during Gigi processing.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using the relevant
   --  Table.Tree_Read routines. Note that Initialize should not be called if
   --  Tree_Read is used. Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using the relevant
   --  Table.Tree_Write routines.

   function Rbase (Real : Ureal) return Nat;
   --  Return the base of the universal real

   function Denominator (Real : Ureal) return Uint;
   --  Return the denominator of the universal real

   function Numerator (Real : Ureal) return Uint;
   --  Return the numerator of the universal real

   function Norm_Den (Real : Ureal) return Uint;
   --  Return the denominator of the universal real after a normalization

   function Norm_Num (Real : Ureal) return Uint;
   --  Return the numerator of the universal real after a normalization

   function UR_From_Uint (UI : Uint) return Ureal;
   --  Returns real corresponding to universal integer value

   function UR_To_Uint (Real : Ureal) return Uint;
   --  Return integer value obtained by accurate rounding of real value.
   --  The rounding of values half way between two integers is away from
   --  zero, as required by normal Ada 95 rounding semantics.

   function UR_Trunc (Real : Ureal) return Uint;
   --  Return integer value obtained by a truncation of real towards zero

   function UR_Ceiling (Real : Ureal) return Uint;
   --  Return value of smallest integer not less than the given value

   function UR_Floor (Real : Ureal) return Uint;
   --  Return value of smallest integer not greater than the given value

   --  Conversion table for above four functions

   --    Input    To_Uint    Trunc    Ceiling    Floor
   --     1.0        1         1         1         1
   --     1.2        1         1         2         1
   --     1.5        2         1         2         1
   --     1.7        2         1         2         1
   --     2.0        2         2         2         2
   --    -1.0       -1        -1        -1        -1
   --    -1.2       -1        -1        -1        -2
   --    -1.5       -2        -1        -1        -2
   --    -1.7       -2        -1        -1        -2
   --    -2.0       -2        -2        -2        -2

   function UR_From_Components
     (Num      : Uint;
      Den      : Uint;
      Rbase    : Nat := 0;
      Negative : Boolean := False)
      return     Ureal;
   --  Builds real value from given numerator, denominator and base. The
   --  value is negative if Negative is set to true, and otherwise is
   --  non-negative.

   function UR_Add (Left : Ureal; Right : Ureal) return Ureal;
   function UR_Add (Left : Ureal; Right : Uint)  return Ureal;
   function UR_Add (Left : Uint;  Right : Ureal) return Ureal;
   --  Returns real sum of operands

   function UR_Div (Left : Ureal; Right : Ureal) return Ureal;
   function UR_Div (Left : Uint;  Right : Ureal) return Ureal;
   function UR_Div (Left : Ureal; Right : Uint)  return Ureal;
   --  Returns real quotient of operands. Fatal error if Right is zero

   function UR_Mul (Left : Ureal; Right : Ureal) return Ureal;
   function UR_Mul (Left : Uint;  Right : Ureal) return Ureal;
   function UR_Mul (Left : Ureal; Right : Uint)  return Ureal;
   --  Returns real product of operands

   function UR_Sub (Left : Ureal; Right : Ureal) return Ureal;
   function UR_Sub (Left : Uint;  Right : Ureal) return Ureal;
   function UR_Sub (Left : Ureal; Right : Uint)  return Ureal;
   --  Returns real difference of operands

   function UR_Exponentiate (Real  : Ureal; N : Uint) return  Ureal;
   --  Returns result of raising Ureal to Uint power.
   --  Fatal error if Left is 0 and Right is negative.

   function UR_Abs (Real : Ureal) return Ureal;
   --  Returns abs function of real

   function UR_Negate (Real : Ureal) return Ureal;
   --  Returns negative of real

   function UR_Eq (Left, Right : Ureal) return Boolean;
   --  Compares reals for equality

   function UR_Max (Left, Right : Ureal) return Ureal;
   --  Returns the maximum of two reals

   function UR_Min (Left, Right : Ureal) return Ureal;
   --  Returns the minimum of two reals

   function UR_Ne (Left, Right : Ureal) return Boolean;
   --  Compares reals for inequality

   function UR_Lt (Left, Right : Ureal) return Boolean;
   --  Compares reals for less than

   function UR_Le (Left, Right : Ureal) return Boolean;
   --  Compares reals for less than or equal

   function UR_Gt (Left, Right : Ureal) return Boolean;
   --  Compares reals for greater than

   function UR_Ge (Left, Right : Ureal) return Boolean;
   --  Compares reals for greater than or equal

   function UR_Is_Zero (Real : Ureal) return Boolean;
   --  Tests if real value is zero

   function UR_Is_Negative (Real : Ureal) return Boolean;
   --  Tests if real value is negative, note that negative zero gives true

   function UR_Is_Positive (Real : Ureal) return Boolean;
   --  Test if real value is greater than zero

   procedure UR_Write (Real : Ureal; Brackets : Boolean := False);
   --  Writes value of Real to standard output. Used for debugging and
   --  tree/source output, and also for -gnatR representation output. If the
   --  result is easily representable as a standard Ada literal, it will be
   --  given that way, but as a result of evaluation of static expressions, it
   --  is possible to generate constants (e.g. 1/13) which have no such
   --  representation. In such cases (and in cases where it is too much work to
   --  figure out the Ada literal), the string that is output is of the form
   --  of some expression such as integer/integer, or integer*integer**integer.
   --  In the case where an expression is output, if Brackets is set to True,
   --  the expression is surrounded by square brackets.

   procedure pr (Real : Ureal);
   pragma Export (Ada, pr);
   --  Writes value of Real to standard output with a terminating line return,
   --  using UR_Write as described above. This is for use from the debugger.

   ------------------------
   -- Operator Renamings --
   ------------------------

   function "+" (Left : Ureal; Right : Ureal) return Ureal renames UR_Add;
   function "+" (Left : Uint;  Right : Ureal) return Ureal renames UR_Add;
   function "+" (Left : Ureal; Right : Uint)  return Ureal renames UR_Add;

   function "/" (Left : Ureal; Right : Ureal) return Ureal renames UR_Div;
   function "/" (Left : Uint;  Right : Ureal) return Ureal renames UR_Div;
   function "/" (Left : Ureal; Right : Uint)  return Ureal renames UR_Div;

   function "*" (Left : Ureal; Right : Ureal) return Ureal renames UR_Mul;
   function "*" (Left : Uint;  Right : Ureal) return Ureal renames UR_Mul;
   function "*" (Left : Ureal; Right : Uint)  return Ureal renames UR_Mul;

   function "-" (Left : Ureal; Right : Ureal) return Ureal renames UR_Sub;
   function "-" (Left : Uint;  Right : Ureal) return Ureal renames UR_Sub;
   function "-" (Left : Ureal; Right : Uint)  return Ureal renames UR_Sub;

   function "**"  (Real  : Ureal; N : Uint) return Ureal
                                                     renames UR_Exponentiate;

   function "abs" (Real : Ureal) return Ureal renames UR_Abs;

   function "-"   (Real : Ureal) return Ureal renames UR_Negate;

   function "="   (Left, Right : Ureal) return Boolean renames UR_Eq;

   function "<"   (Left, Right : Ureal) return Boolean renames UR_Lt;

   function "<="  (Left, Right : Ureal) return Boolean renames UR_Le;

   function ">="  (Left, Right : Ureal) return Boolean renames UR_Ge;

   function ">"   (Left, Right : Ureal) return Boolean renames UR_Gt;

   -----------------------------
   -- Mark/Release Processing --
   -----------------------------

   --  The space used by Ureal data is not automatically reclaimed. However,
   --  a mark-release regime is implemented which allows storage to be
   --  released back to a previously noted mark. This is used for example
   --  when doing comparisons, where only intermediate results get stored
   --  that do not need to be saved for future use.

   type Save_Mark is private;

   function Mark return Save_Mark;
   --  Note mark point for future release

   procedure Release (M : Save_Mark);
   --  Release storage allocated since mark was noted

   ------------------------------------
   -- Representation of Ureal Values --
   ------------------------------------

private

   type Ureal is new Int range Ureal_Low_Bound .. Ureal_High_Bound;
   for Ureal'Size use 32;

   No_Ureal : constant Ureal := Ureal'First;

   type Save_Mark is new Int;

   pragma Inline (Denominator);
   pragma Inline (Mark);
   pragma Inline (Norm_Num);
   pragma Inline (Norm_Den);
   pragma Inline (Numerator);
   pragma Inline (Rbase);
   pragma Inline (Release);
   pragma Inline (Ureal_0);
   pragma Inline (Ureal_M_0);
   pragma Inline (Ureal_Tenth);
   pragma Inline (Ureal_Half);
   pragma Inline (Ureal_1);
   pragma Inline (Ureal_2);
   pragma Inline (Ureal_10);
   pragma Inline (UR_From_Components);

end Urealp;
