------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U R E A L P                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Output;  use Output;
with Table;
with Tree_IO; use Tree_IO;

package body Urealp is

   Ureal_First_Entry : constant Ureal := Ureal'Succ (No_Ureal);
   --  First subscript allocated in Ureal table (note that we can't just
   --  add 1 to No_Ureal, since "+" means something different for Ureals!

   type Ureal_Entry is record
      Num  : Uint;
      --  Numerator (always non-negative)

      Den  : Uint;
      --  Denominator (always non-zero, always positive if base is zero)

      Rbase : Nat;
      --  Base value. If Rbase is zero, then the value is simply Num / Den.
      --  If Rbase is non-zero, then the value is Num / (Rbase ** Den)

      Negative : Boolean;
      --  Flag set if value is negative
   end record;

   package Ureals is new Table.Table (
     Table_Component_Type => Ureal_Entry,
     Table_Index_Type     => Ureal,
     Table_Low_Bound      => Ureal_First_Entry,
     Table_Initial        => Alloc.Ureals_Initial,
     Table_Increment      => Alloc.Ureals_Increment,
     Table_Name           => "Ureals");

   --  The following universal reals are the values returned by the constant
   --  functions. They are initialized by the initialization procedure.

   UR_0          : Ureal;
   UR_M_0        : Ureal;
   UR_Tenth      : Ureal;
   UR_Half       : Ureal;
   UR_1          : Ureal;
   UR_2          : Ureal;
   UR_10         : Ureal;
   UR_10_36      : Ureal;
   UR_M_10_36    : Ureal;
   UR_100        : Ureal;
   UR_2_128      : Ureal;
   UR_2_80       : Ureal;
   UR_2_M_128    : Ureal;
   UR_2_M_80     : Ureal;

   Num_Ureal_Constants : constant := 10;
   --  This is used for an assertion check in Tree_Read and Tree_Write to
   --  help remember to add values to these routines when we add to the list.

   Normalized_Real : Ureal := No_Ureal;
   --  Used to memoize Norm_Num and Norm_Den, if either of these functions
   --  is called, this value is set and Normalized_Entry contains the result
   --  of the normalization. On subsequent calls, this is used to avoid the
   --  call to Normalize if it has already been made.

   Normalized_Entry : Ureal_Entry;
   --  Entry built by most recent call to Normalize

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Decimal_Exponent_Hi (V : Ureal) return Int;
   --  Returns an estimate of the exponent of Val represented as a normalized
   --  decimal number (non-zero digit before decimal point), The estimate is
   --  either correct, or high, but never low. The accuracy of the estimate
   --  affects only the efficiency of the comparison routines.

   function Decimal_Exponent_Lo (V : Ureal) return Int;
   --  Returns an estimate of the exponent of Val represented as a normalized
   --  decimal number (non-zero digit before decimal point), The estimate is
   --  either correct, or low, but never high. The accuracy of the estimate
   --  affects only the efficiency of the comparison routines.

   function Equivalent_Decimal_Exponent (U : Ureal_Entry) return Int;
   --  U is a Ureal entry for which the base value is non-zero, the value
   --  returned is the equivalent decimal exponent value, i.e. the value of
   --  Den, adjusted as though the base were base 10. The value is rounded
   --  to the nearest integer, and so can be one off.

   function Is_Integer (Num, Den : Uint) return Boolean;
   --  Return true if the real quotient of Num / Den is an integer value

   function Normalize (Val : Ureal_Entry) return Ureal_Entry;
   --  Normalizes the Ureal_Entry by reducing it to lowest terms (with a
   --  base value of 0).

   function Same (U1, U2 : Ureal) return Boolean;
   pragma Inline (Same);
   --  Determines if U1 and U2 are the same Ureal. Note that we cannot use
   --  the equals operator for this test, since that tests for equality,
   --  not identity.

   function Store_Ureal (Val : Ureal_Entry) return Ureal;
   --  This store a new entry in the universal reals table and return
   --  its index in the table.

   -------------------------
   -- Decimal_Exponent_Hi --
   -------------------------

   function Decimal_Exponent_Hi (V : Ureal) return Int is
      Val : constant Ureal_Entry := Ureals.Table (V);

   begin
      --  Zero always returns zero

      if UR_Is_Zero (V) then
         return 0;

      --  For numbers in rational form, get the maximum number of digits in the
      --  numerator and the minimum number of digits in the denominator, and
      --  subtract. For example:

      --     1000 / 99 = 1.010E+1
      --     9999 / 10 = 9.999E+2

      --  This estimate may of course be high, but that is acceptable

      elsif Val.Rbase = 0 then
         return UI_Decimal_Digits_Hi (Val.Num) -
                UI_Decimal_Digits_Lo (Val.Den);

      --  For based numbers, just subtract the decimal exponent from the
      --  high estimate of the number of digits in the numerator and add
      --  one to accommodate possible round off errors for non-decimal
      --  bases. For example:

      --     1_500_000 / 10**4 = 1.50E-2

      else -- Val.Rbase /= 0
         return UI_Decimal_Digits_Hi (Val.Num) -
                Equivalent_Decimal_Exponent (Val) + 1;
      end if;
   end Decimal_Exponent_Hi;

   -------------------------
   -- Decimal_Exponent_Lo --
   -------------------------

   function Decimal_Exponent_Lo (V : Ureal) return Int is
      Val : constant Ureal_Entry := Ureals.Table (V);

   begin
      --  Zero always returns zero

      if UR_Is_Zero (V) then
         return 0;

      --  For numbers in rational form, get min digits in numerator, max digits
      --  in denominator, and subtract and subtract one more for possible loss
      --  during the division. For example:

      --     1000 / 99 = 1.010E+1
      --     9999 / 10 = 9.999E+2

      --  This estimate may of course be low, but that is acceptable

      elsif Val.Rbase = 0 then
         return UI_Decimal_Digits_Lo (Val.Num) -
                UI_Decimal_Digits_Hi (Val.Den) - 1;

      --  For based numbers, just subtract the decimal exponent from the
      --  low estimate of the number of digits in the numerator and subtract
      --  one to accommodate possible round off errors for non-decimal
      --  bases. For example:

      --     1_500_000 / 10**4 = 1.50E-2

      else -- Val.Rbase /= 0
         return UI_Decimal_Digits_Lo (Val.Num) -
                Equivalent_Decimal_Exponent (Val) - 1;
      end if;
   end Decimal_Exponent_Lo;

   -----------------
   -- Denominator --
   -----------------

   function Denominator (Real : Ureal) return Uint is
   begin
      return Ureals.Table (Real).Den;
   end Denominator;

   ---------------------------------
   -- Equivalent_Decimal_Exponent --
   ---------------------------------

   function Equivalent_Decimal_Exponent (U : Ureal_Entry) return Int is

      --  The following table is a table of logs to the base 10

      Logs : constant array (Nat range 1 .. 16) of Long_Float := (
                1 => 0.000000000000000,
                2 => 0.301029995663981,
                3 => 0.477121254719662,
                4 => 0.602059991327962,
                5 => 0.698970004336019,
                6 => 0.778151250383644,
                7 => 0.845098040014257,
                8 => 0.903089986991944,
                9 => 0.954242509439325,
               10 => 1.000000000000000,
               11 => 1.041392685158230,
               12 => 1.079181246047620,
               13 => 1.113943352306840,
               14 => 1.146128035678240,
               15 => 1.176091259055680,
               16 => 1.204119982655920);

   begin
      pragma Assert (U.Rbase /= 0);
      return Int (Long_Float (UI_To_Int (U.Den)) * Logs (U.Rbase));
   end Equivalent_Decimal_Exponent;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Ureals.Init;
      UR_0       := UR_From_Components (Uint_0, Uint_1,         0, False);
      UR_M_0     := UR_From_Components (Uint_0, Uint_1,         0, True);
      UR_Half    := UR_From_Components (Uint_1, Uint_1,         2, False);
      UR_Tenth   := UR_From_Components (Uint_1, Uint_1,        10, False);
      UR_1       := UR_From_Components (Uint_1, Uint_1,         0, False);
      UR_2       := UR_From_Components (Uint_1, Uint_Minus_1,   2, False);
      UR_10      := UR_From_Components (Uint_1, Uint_Minus_1,  10, False);
      UR_10_36   := UR_From_Components (Uint_1, Uint_Minus_36, 10, False);
      UR_M_10_36 := UR_From_Components (Uint_1, Uint_Minus_36, 10, True);
      UR_100     := UR_From_Components (Uint_1, Uint_Minus_2,  10, False);
      UR_2_128   := UR_From_Components (Uint_1, Uint_Minus_128, 2, False);
      UR_2_M_128 := UR_From_Components (Uint_1, Uint_128,       2, False);
      UR_2_80    := UR_From_Components (Uint_1, Uint_Minus_80,  2, False);
      UR_2_M_80  := UR_From_Components (Uint_1, Uint_80,        2, False);
   end Initialize;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Num, Den : Uint) return Boolean is
   begin
      return (Num / Den) * Den = Num;
   end Is_Integer;

   ----------
   -- Mark --
   ----------

   function Mark return Save_Mark is
   begin
      return Save_Mark (Ureals.Last);
   end Mark;

   --------------
   -- Norm_Den --
   --------------

   function Norm_Den (Real : Ureal) return Uint is
   begin
      if not Same (Real, Normalized_Real) then
         Normalized_Real  := Real;
         Normalized_Entry := Normalize (Ureals.Table (Real));
      end if;

      return Normalized_Entry.Den;
   end Norm_Den;

   --------------
   -- Norm_Num --
   --------------

   function Norm_Num (Real : Ureal) return Uint is
   begin
      if not Same (Real, Normalized_Real) then
         Normalized_Real  := Real;
         Normalized_Entry := Normalize (Ureals.Table (Real));
      end if;

      return Normalized_Entry.Num;
   end Norm_Num;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Val : Ureal_Entry) return Ureal_Entry is
      J   : Uint;
      K   : Uint;
      Tmp : Uint;
      Num : Uint;
      Den : Uint;
      M   : constant Uintp.Save_Mark := Uintp.Mark;

   begin
      --  Start by setting J to the greatest of the absolute values of the
      --  numerator and the denominator (taking into account the base value),
      --  and K to the lesser of the two absolute values. The gcd of Num and
      --  Den is the gcd of J and K.

      if Val.Rbase = 0 then
         J := Val.Num;
         K := Val.Den;

      elsif Val.Den < 0 then
         J := Val.Num * Val.Rbase ** (-Val.Den);
         K := Uint_1;

      else
         J := Val.Num;
         K := Val.Rbase ** Val.Den;
      end if;

      Num := J;
      Den := K;

      if K > J then
         Tmp := J;
         J := K;
         K := Tmp;
      end if;

      J := UI_GCD (J, K);
      Num := Num / J;
      Den := Den / J;
      Uintp.Release_And_Save (M, Num, Den);

      --  Divide numerator and denominator by gcd and return result

      return (Num      => Num,
              Den      => Den,
              Rbase    => 0,
              Negative => Val.Negative);
   end Normalize;

   ---------------
   -- Numerator --
   ---------------

   function Numerator (Real : Ureal) return Uint is
   begin
      return Ureals.Table (Real).Num;
   end Numerator;

   --------
   -- pr --
   --------

   procedure pr (Real : Ureal) is
   begin
      UR_Write (Real);
      Write_Eol;
   end pr;

   -----------
   -- Rbase --
   -----------

   function Rbase (Real : Ureal) return Nat is
   begin
      return Ureals.Table (Real).Rbase;
   end Rbase;

   -------------
   -- Release --
   -------------

   procedure Release (M : Save_Mark) is
   begin
      Ureals.Set_Last (Ureal (M));
   end Release;

   ----------
   -- Same --
   ----------

   function Same (U1, U2 : Ureal) return Boolean is
   begin
      return Int (U1) = Int (U2);
   end Same;

   -----------------
   -- Store_Ureal --
   -----------------

   function Store_Ureal (Val : Ureal_Entry) return Ureal is
   begin
      Ureals.Increment_Last;
      Ureals.Table (Ureals.Last) := Val;

      --  Normalize representation of signed values

      if Val.Num < 0 then
         Ureals.Table (Ureals.Last).Negative := True;
         Ureals.Table (Ureals.Last).Num := -Val.Num;
      end if;

      return Ureals.Last;
   end Store_Ureal;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      pragma Assert (Num_Ureal_Constants = 10);

      Ureals.Tree_Read;
      Tree_Read_Int (Int (UR_0));
      Tree_Read_Int (Int (UR_M_0));
      Tree_Read_Int (Int (UR_Tenth));
      Tree_Read_Int (Int (UR_Half));
      Tree_Read_Int (Int (UR_1));
      Tree_Read_Int (Int (UR_2));
      Tree_Read_Int (Int (UR_10));
      Tree_Read_Int (Int (UR_100));
      Tree_Read_Int (Int (UR_2_128));
      Tree_Read_Int (Int (UR_2_M_128));

      --  Clear the normalization cache

      Normalized_Real := No_Ureal;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      pragma Assert (Num_Ureal_Constants = 10);

      Ureals.Tree_Write;
      Tree_Write_Int (Int (UR_0));
      Tree_Write_Int (Int (UR_M_0));
      Tree_Write_Int (Int (UR_Tenth));
      Tree_Write_Int (Int (UR_Half));
      Tree_Write_Int (Int (UR_1));
      Tree_Write_Int (Int (UR_2));
      Tree_Write_Int (Int (UR_10));
      Tree_Write_Int (Int (UR_100));
      Tree_Write_Int (Int (UR_2_128));
      Tree_Write_Int (Int (UR_2_M_128));
   end Tree_Write;

   ------------
   -- UR_Abs --
   ------------

   function UR_Abs (Real : Ureal) return Ureal is
      Val : constant Ureal_Entry := Ureals.Table (Real);

   begin
      return Store_Ureal (
               (Num      => Val.Num,
                Den      => Val.Den,
                Rbase    => Val.Rbase,
                Negative => False));
   end UR_Abs;

   ------------
   -- UR_Add --
   ------------

   function UR_Add (Left : Uint; Right : Ureal) return Ureal is
   begin
      return UR_From_Uint (Left) + Right;
   end UR_Add;

   function UR_Add (Left : Ureal; Right : Uint) return Ureal is
   begin
      return Left + UR_From_Uint (Right);
   end UR_Add;

   function UR_Add (Left : Ureal; Right : Ureal) return Ureal is
      Lval : Ureal_Entry := Ureals.Table (Left);
      Rval : Ureal_Entry := Ureals.Table (Right);

      Num  : Uint;

   begin
      --  Note, in the temporary Ureal_Entry values used in this procedure,
      --  we store the sign as the sign of the numerator (i.e. xxx.Num may
      --  be negative, even though in stored entries this can never be so)

      if Lval.Rbase /= 0 and then Lval.Rbase = Rval.Rbase then

         declare
            Opd_Min, Opd_Max   : Ureal_Entry;
            Exp_Min, Exp_Max   : Uint;

         begin
            if Lval.Negative then
               Lval.Num := (-Lval.Num);
            end if;

            if Rval.Negative then
               Rval.Num := (-Rval.Num);
            end if;

            if Lval.Den < Rval.Den then
               Exp_Min := Lval.Den;
               Exp_Max := Rval.Den;
               Opd_Min := Lval;
               Opd_Max := Rval;
            else
               Exp_Min := Rval.Den;
               Exp_Max := Lval.Den;
               Opd_Min := Rval;
               Opd_Max := Lval;
            end if;

            Num :=
              Opd_Min.Num * Lval.Rbase ** (Exp_Max - Exp_Min) + Opd_Max.Num;

            if Num = 0 then
               return Store_Ureal (
                        (Num      => Uint_0,
                         Den      => Uint_1,
                         Rbase    => 0,
                         Negative => Lval.Negative));

            else
               return Store_Ureal (
                        (Num      => abs Num,
                         Den      => Exp_Max,
                         Rbase    => Lval.Rbase,
                         Negative => (Num < 0)));
            end if;
         end;

      else
         declare
            Ln : Ureal_Entry := Normalize (Lval);
            Rn : Ureal_Entry := Normalize (Rval);

         begin
            if Ln.Negative then
               Ln.Num := (-Ln.Num);
            end if;

            if Rn.Negative then
               Rn.Num := (-Rn.Num);
            end if;

            Num := (Ln.Num * Rn.Den) + (Rn.Num * Ln.Den);

            if Num = 0 then
               return Store_Ureal (
                        (Num      => Uint_0,
                         Den      => Uint_1,
                         Rbase    => 0,
                         Negative => Lval.Negative));

            else
               return Store_Ureal (
                        Normalize (
                          (Num      => abs Num,
                           Den      => Ln.Den * Rn.Den,
                           Rbase    => 0,
                           Negative => (Num < 0))));
            end if;
         end;
      end if;
   end UR_Add;

   ----------------
   -- UR_Ceiling --
   ----------------

   function UR_Ceiling (Real : Ureal) return Uint is
      Val : constant Ureal_Entry := Normalize (Ureals.Table (Real));

   begin
      if Val.Negative then
         return UI_Negate (Val.Num / Val.Den);
      else
         return (Val.Num + Val.Den - 1) / Val.Den;
      end if;
   end UR_Ceiling;

   ------------
   -- UR_Div --
   ------------

   function UR_Div (Left : Uint; Right : Ureal) return Ureal is
   begin
      return UR_From_Uint (Left) / Right;
   end UR_Div;

   function UR_Div (Left : Ureal; Right : Uint) return Ureal is
   begin
      return Left / UR_From_Uint (Right);
   end UR_Div;

   function UR_Div (Left, Right : Ureal) return Ureal is
      Lval : constant Ureal_Entry := Ureals.Table (Left);
      Rval : constant Ureal_Entry := Ureals.Table (Right);
      Rneg : constant Boolean     := Rval.Negative xor Lval.Negative;

   begin
      pragma Assert (Rval.Num /= Uint_0);

      if Lval.Rbase = 0 then

         if Rval.Rbase = 0 then
            return Store_Ureal (
                     Normalize (
                       (Num      => Lval.Num * Rval.Den,
                        Den      => Lval.Den * Rval.Num,
                        Rbase    => 0,
                        Negative => Rneg)));

         elsif Is_Integer (Lval.Num, Rval.Num * Lval.Den) then
            return Store_Ureal (
                     (Num      => Lval.Num / (Rval.Num * Lval.Den),
                      Den      => (-Rval.Den),
                      Rbase    => Rval.Rbase,
                      Negative => Rneg));

         elsif Rval.Den < 0 then
            return Store_Ureal (
                     Normalize (
                       (Num      => Lval.Num,
                        Den      => Rval.Rbase ** (-Rval.Den) *
                                    Rval.Num *
                                    Lval.Den,
                        Rbase    => 0,
                        Negative => Rneg)));

         else
            return Store_Ureal (
                     Normalize (
                       (Num      => Lval.Num * Rval.Rbase ** Rval.Den,
                        Den      => Rval.Num * Lval.Den,
                        Rbase    => 0,
                        Negative => Rneg)));
         end if;

      elsif Is_Integer (Lval.Num, Rval.Num) then

         if Rval.Rbase = Lval.Rbase then
            return Store_Ureal (
                     (Num      => Lval.Num / Rval.Num,
                      Den      => Lval.Den - Rval.Den,
                      Rbase    => Lval.Rbase,
                      Negative => Rneg));

         elsif Rval.Rbase = 0 then
            return Store_Ureal (
                     (Num      => (Lval.Num / Rval.Num) * Rval.Den,
                      Den      => Lval.Den,
                      Rbase    => Lval.Rbase,
                      Negative => Rneg));

         elsif Rval.Den < 0 then
            declare
               Num, Den : Uint;

            begin
               if Lval.Den < 0 then
                  Num := (Lval.Num / Rval.Num) * (Lval.Rbase ** (-Lval.Den));
                  Den := Rval.Rbase ** (-Rval.Den);
               else
                  Num := Lval.Num / Rval.Num;
                  Den := (Lval.Rbase ** Lval.Den) *
                         (Rval.Rbase ** (-Rval.Den));
               end if;

               return Store_Ureal (
                        (Num      => Num,
                         Den      => Den,
                         Rbase    => 0,
                         Negative => Rneg));
            end;

         else
            return Store_Ureal (
                     (Num      => (Lval.Num / Rval.Num) *
                                  (Rval.Rbase ** Rval.Den),
                      Den      => Lval.Den,
                      Rbase    => Lval.Rbase,
                      Negative => Rneg));
         end if;

      else
         declare
            Num, Den : Uint;

         begin
            if Lval.Den < 0 then
               Num := Lval.Num * (Lval.Rbase ** (-Lval.Den));
               Den := Rval.Num;

            else
               Num := Lval.Num;
               Den := Rval.Num * (Lval.Rbase ** Lval.Den);
            end if;

            if Rval.Rbase /= 0 then
               if Rval.Den < 0 then
                  Den := Den * (Rval.Rbase ** (-Rval.Den));
               else
                  Num := Num * (Rval.Rbase ** Rval.Den);
               end if;

            else
               Num := Num * Rval.Den;
            end if;

            return Store_Ureal (
                     Normalize (
                       (Num      => Num,
                        Den      => Den,
                        Rbase    => 0,
                        Negative => Rneg)));
         end;
      end if;
   end UR_Div;

   -----------
   -- UR_Eq --
   -----------

   function UR_Eq (Left, Right : Ureal) return Boolean is
   begin
      return not UR_Ne (Left, Right);
   end UR_Eq;

   ---------------------
   -- UR_Exponentiate --
   ---------------------

   function UR_Exponentiate (Real : Ureal; N : Uint) return Ureal is
      X    : constant Uint := abs N;
      Bas  : Ureal;
      Val  : Ureal_Entry;
      Neg  : Boolean;
      IBas : Uint;

   begin
      --  If base is negative, then the resulting sign depends on whether
      --  the exponent is even or odd (even => positive, odd = negative)

      if UR_Is_Negative (Real) then
         Neg := (N mod 2) /= 0;
         Bas := UR_Negate (Real);
      else
         Neg := False;
         Bas := Real;
      end if;

      Val := Ureals.Table (Bas);

      --  If the base is a small integer, then we can return the result in
      --  exponential form, which can save a lot of time for junk exponents.

      IBas := UR_Trunc (Bas);

      if IBas <= 16
        and then UR_From_Uint (IBas) = Bas
      then
         return Store_Ureal (
                 (Num      => Uint_1,
                  Den      => -N,
                  Rbase    => UI_To_Int (UR_Trunc (Bas)),
                  Negative => Neg));

      --  If the exponent is negative then we raise the numerator and the
      --  denominator (after normalization) to the absolute value of the
      --  exponent and we return the reciprocal. An assert error will happen
      --  if the numerator is zero.

      elsif N < 0 then
         pragma Assert (Val.Num /= 0);
         Val := Normalize (Val);

         return Store_Ureal (
                 (Num      => Val.Den ** X,
                  Den      => Val.Num ** X,
                  Rbase    => 0,
                  Negative => Neg));

      --  If positive, we distinguish the case when the base is not zero, in
      --  which case the new denominator is just the product of the old one
      --  with the exponent,

      else
         if Val.Rbase /= 0 then

            return Store_Ureal (
                    (Num      => Val.Num ** X,
                     Den      => Val.Den * X,
                     Rbase    => Val.Rbase,
                     Negative => Neg));

         --  And when the base is zero, in which case we exponentiate
         --  the old denominator.

         else
            return Store_Ureal (
                    (Num      => Val.Num ** X,
                     Den      => Val.Den ** X,
                     Rbase    => 0,
                     Negative => Neg));
         end if;
      end if;
   end UR_Exponentiate;

   --------------
   -- UR_Floor --
   --------------

   function UR_Floor (Real : Ureal) return Uint is
      Val : constant Ureal_Entry := Normalize (Ureals.Table (Real));

   begin
      if Val.Negative then
         return UI_Negate ((Val.Num + Val.Den - 1) / Val.Den);
      else
         return Val.Num / Val.Den;
      end if;
   end UR_Floor;

   ------------------------
   -- UR_From_Components --
   ------------------------

   function UR_From_Components
     (Num      : Uint;
      Den      : Uint;
      Rbase    : Nat := 0;
      Negative : Boolean := False)
      return     Ureal
   is
   begin
      return Store_Ureal (
               (Num      => Num,
                Den      => Den,
                Rbase    => Rbase,
                Negative => Negative));
   end UR_From_Components;

   ------------------
   -- UR_From_Uint --
   ------------------

   function UR_From_Uint (UI : Uint) return Ureal is
   begin
      return UR_From_Components
        (abs UI, Uint_1, Negative => (UI < 0));
   end UR_From_Uint;

   -----------
   -- UR_Ge --
   -----------

   function UR_Ge (Left, Right : Ureal) return Boolean is
   begin
      return not (Left < Right);
   end UR_Ge;

   -----------
   -- UR_Gt --
   -----------

   function UR_Gt (Left, Right : Ureal) return Boolean is
   begin
      return (Right < Left);
   end UR_Gt;

   --------------------
   -- UR_Is_Negative --
   --------------------

   function UR_Is_Negative (Real : Ureal) return Boolean is
   begin
      return Ureals.Table (Real).Negative;
   end UR_Is_Negative;

   --------------------
   -- UR_Is_Positive --
   --------------------

   function UR_Is_Positive (Real : Ureal) return Boolean is
   begin
      return not Ureals.Table (Real).Negative
        and then Ureals.Table (Real).Num /= 0;
   end UR_Is_Positive;

   ----------------
   -- UR_Is_Zero --
   ----------------

   function UR_Is_Zero (Real : Ureal) return Boolean is
   begin
      return Ureals.Table (Real).Num = 0;
   end UR_Is_Zero;

   -----------
   -- UR_Le --
   -----------

   function UR_Le (Left, Right : Ureal) return Boolean is
   begin
      return not (Right < Left);
   end UR_Le;

   -----------
   -- UR_Lt --
   -----------

   function UR_Lt (Left, Right : Ureal) return Boolean is
   begin
      --  An operand is not less than itself

      if Same (Left, Right) then
         return False;

      --  Deal with zero cases

      elsif UR_Is_Zero (Left) then
         return UR_Is_Positive (Right);

      elsif UR_Is_Zero (Right) then
         return Ureals.Table (Left).Negative;

      --  Different signs are decisive (note we dealt with zero cases)

      elsif Ureals.Table (Left).Negative
        and then not Ureals.Table (Right).Negative
      then
         return True;

      elsif not Ureals.Table (Left).Negative
        and then Ureals.Table (Right).Negative
      then
         return False;

      --  Signs are same, do rapid check based on worst case estimates of
      --  decimal exponent, which will often be decisive. Precise test
      --  depends on whether operands are positive or negative.

      elsif Decimal_Exponent_Hi (Left) < Decimal_Exponent_Lo (Right) then
         return UR_Is_Positive (Left);

      elsif Decimal_Exponent_Lo (Left) > Decimal_Exponent_Hi (Right) then
         return UR_Is_Negative (Left);

      --  If we fall through, full gruesome test is required. This happens
      --  if the numbers are close together, or in some weird (/=10) base.

      else
         declare
            Imrk   : constant Uintp.Save_Mark  := Mark;
            Rmrk   : constant Urealp.Save_Mark := Mark;
            Lval   : Ureal_Entry;
            Rval   : Ureal_Entry;
            Result : Boolean;

         begin
            Lval := Ureals.Table (Left);
            Rval := Ureals.Table (Right);

            --  An optimization. If both numbers are based, then subtract
            --  common value of base to avoid unnecessarily giant numbers

            if Lval.Rbase = Rval.Rbase and then Lval.Rbase /= 0 then
               if Lval.Den < Rval.Den then
                  Rval.Den := Rval.Den - Lval.Den;
                  Lval.Den := Uint_0;
               else
                  Lval.Den := Lval.Den - Rval.Den;
                  Rval.Den := Uint_0;
               end if;
            end if;

            Lval := Normalize (Lval);
            Rval := Normalize (Rval);

            if Lval.Negative then
               Result := (Lval.Num * Rval.Den) > (Rval.Num * Lval.Den);
            else
               Result := (Lval.Num * Rval.Den) < (Rval.Num * Lval.Den);
            end if;

            Release (Imrk);
            Release (Rmrk);
            return Result;
         end;
      end if;
   end UR_Lt;

   ------------
   -- UR_Max --
   ------------

   function UR_Max (Left, Right : Ureal) return Ureal is
   begin
      if Left >= Right then
         return Left;
      else
         return Right;
      end if;
   end UR_Max;

   ------------
   -- UR_Min --
   ------------

   function UR_Min (Left, Right : Ureal) return Ureal is
   begin
      if Left <= Right then
         return Left;
      else
         return Right;
      end if;
   end UR_Min;

   ------------
   -- UR_Mul --
   ------------

   function UR_Mul (Left : Uint; Right : Ureal) return Ureal is
   begin
      return UR_From_Uint (Left) * Right;
   end UR_Mul;

   function UR_Mul (Left : Ureal; Right : Uint) return Ureal is
   begin
      return Left * UR_From_Uint (Right);
   end UR_Mul;

   function UR_Mul (Left, Right : Ureal) return Ureal is
      Lval : constant Ureal_Entry := Ureals.Table (Left);
      Rval : constant Ureal_Entry := Ureals.Table (Right);
      Num  : Uint                 := Lval.Num * Rval.Num;
      Den  : Uint;
      Rneg : constant Boolean     := Lval.Negative xor Rval.Negative;

   begin
      if Lval.Rbase = 0 then
         if Rval.Rbase = 0 then
            return Store_Ureal (
                     Normalize (
                        (Num      => Num,
                         Den      => Lval.Den * Rval.Den,
                         Rbase    => 0,
                         Negative => Rneg)));

         elsif Is_Integer (Num, Lval.Den) then
            return Store_Ureal (
                     (Num      => Num / Lval.Den,
                      Den      => Rval.Den,
                      Rbase    => Rval.Rbase,
                      Negative => Rneg));

         elsif Rval.Den < 0 then
            return Store_Ureal (
                     Normalize (
                       (Num      => Num * (Rval.Rbase ** (-Rval.Den)),
                        Den      => Lval.Den,
                        Rbase    => 0,
                        Negative => Rneg)));

         else
            return Store_Ureal (
                     Normalize (
                       (Num      => Num,
                        Den      => Lval.Den * (Rval.Rbase ** Rval.Den),
                        Rbase    => 0,
                        Negative => Rneg)));
         end if;

      elsif Lval.Rbase = Rval.Rbase then
         return Store_Ureal (
                  (Num      => Num,
                   Den      => Lval.Den + Rval.Den,
                   Rbase    => Lval.Rbase,
                   Negative => Rneg));

      elsif Rval.Rbase = 0 then
         if Is_Integer (Num, Rval.Den) then
            return Store_Ureal (
                     (Num      => Num / Rval.Den,
                      Den      => Lval.Den,
                      Rbase    => Lval.Rbase,
                      Negative => Rneg));

         elsif Lval.Den < 0 then
            return Store_Ureal (
                     Normalize (
                       (Num      => Num * (Lval.Rbase ** (-Lval.Den)),
                        Den      => Rval.Den,
                        Rbase    => 0,
                        Negative => Rneg)));

         else
            return Store_Ureal (
                     Normalize (
                       (Num      => Num,
                        Den      => Rval.Den * (Lval.Rbase ** Lval.Den),
                        Rbase    => 0,
                        Negative => Rneg)));
         end if;

      else
         Den := Uint_1;

         if Lval.Den < 0 then
            Num := Num * (Lval.Rbase ** (-Lval.Den));
         else
            Den := Den * (Lval.Rbase ** Lval.Den);
         end if;

         if Rval.Den < 0 then
            Num := Num * (Rval.Rbase ** (-Rval.Den));
         else
            Den := Den * (Rval.Rbase ** Rval.Den);
         end if;

         return Store_Ureal (
                  Normalize (
                    (Num      => Num,
                     Den      => Den,
                     Rbase    => 0,
                     Negative => Rneg)));
      end if;
   end UR_Mul;

   -----------
   -- UR_Ne --
   -----------

   function UR_Ne (Left, Right : Ureal) return Boolean is
   begin
      --  Quick processing for case of identical Ureal values (note that
      --  this also deals with comparing two No_Ureal values).

      if Same (Left, Right) then
         return False;

      --  Deal with case of one or other operand is No_Ureal, but not both

      elsif Same (Left, No_Ureal) or else Same (Right, No_Ureal) then
         return True;

      --  Do quick check based on number of decimal digits

      elsif Decimal_Exponent_Hi (Left) < Decimal_Exponent_Lo (Right) or else
            Decimal_Exponent_Lo (Left) > Decimal_Exponent_Hi (Right)
      then
         return True;

      --  Otherwise full comparison is required

      else
         declare
            Imrk   : constant Uintp.Save_Mark  := Mark;
            Rmrk   : constant Urealp.Save_Mark := Mark;
            Lval   : constant Ureal_Entry := Normalize (Ureals.Table (Left));
            Rval   : constant Ureal_Entry := Normalize (Ureals.Table (Right));
            Result : Boolean;

         begin
            if UR_Is_Zero (Left) then
               return not UR_Is_Zero (Right);

            elsif UR_Is_Zero (Right) then
               return not UR_Is_Zero (Left);

            --  Both operands are non-zero

            else
               Result :=
                  Rval.Negative /= Lval.Negative
                   or else Rval.Num /= Lval.Num
                   or else Rval.Den /= Lval.Den;
               Release (Imrk);
               Release (Rmrk);
               return Result;
            end if;
         end;
      end if;
   end UR_Ne;

   ---------------
   -- UR_Negate --
   ---------------

   function UR_Negate (Real : Ureal) return Ureal is
   begin
      return Store_Ureal (
               (Num      => Ureals.Table (Real).Num,
                Den      => Ureals.Table (Real).Den,
                Rbase    => Ureals.Table (Real).Rbase,
                Negative => not Ureals.Table (Real).Negative));
   end UR_Negate;

   ------------
   -- UR_Sub --
   ------------

   function UR_Sub (Left : Uint; Right : Ureal) return Ureal is
   begin
      return UR_From_Uint (Left) + UR_Negate (Right);
   end UR_Sub;

   function UR_Sub (Left : Ureal; Right : Uint) return Ureal is
   begin
      return Left + UR_From_Uint (-Right);
   end UR_Sub;

   function UR_Sub (Left, Right : Ureal) return Ureal is
   begin
      return Left + UR_Negate (Right);
   end UR_Sub;

   ----------------
   -- UR_To_Uint --
   ----------------

   function UR_To_Uint (Real : Ureal) return Uint is
      Val : constant Ureal_Entry := Normalize (Ureals.Table (Real));
      Res : Uint;

   begin
      Res := (Val.Num + (Val.Den / 2)) / Val.Den;

      if Val.Negative then
         return UI_Negate (Res);
      else
         return Res;
      end if;
   end UR_To_Uint;

   --------------
   -- UR_Trunc --
   --------------

   function UR_Trunc (Real : Ureal) return Uint is
      Val : constant Ureal_Entry := Normalize (Ureals.Table (Real));

   begin
      if Val.Negative then
         return -(Val.Num / Val.Den);
      else
         return Val.Num / Val.Den;
      end if;
   end UR_Trunc;

   --------------
   -- UR_Write --
   --------------

   procedure UR_Write (Real : Ureal) is
      Val : constant Ureal_Entry := Ureals.Table (Real);

   begin
      --  If value is negative, we precede the constant by a minus sign
      --  and add an extra layer of parentheses on the outside since the
      --  minus sign is part of the value, not a negation operator.

      if Val.Negative then
         Write_Str ("(-");
      end if;

      --  Constants in base 10 can be written in normal Ada literal style

      if Val.Rbase = 10 then
         UI_Write (Val.Num / 10);
         Write_Char ('.');
         UI_Write (Val.Num mod 10);

         if Val.Den /= 0 then
            Write_Char ('E');
            UI_Write (1 - Val.Den);
         end if;

      --  Constants in a base other than 10 can still be easily written
      --  in normal Ada literal style if the numerator is one.

      elsif Val.Rbase /= 0 and then Val.Num = 1 then
         Write_Int (Val.Rbase);
         Write_Str ("#1.0#E");
         UI_Write (-Val.Den);

      --  Other constants with a base other than 10 are written using one
      --  of the following forms, depending on the sign of the number
      --  and the sign of the exponent (= minus denominator value)

      --    (numerator.0*base**exponent)
      --    (numerator.0*base**(-exponent))

      elsif Val.Rbase /= 0 then
         Write_Char ('(');
         UI_Write (Val.Num, Decimal);
         Write_Str (".0*");
         Write_Int (Val.Rbase);
         Write_Str ("**");

         if Val.Den <= 0 then
            UI_Write (-Val.Den, Decimal);

         else
            Write_Str ("(-");
            UI_Write (Val.Den, Decimal);
            Write_Char (')');
         end if;

         Write_Char (')');

      --  Rational constants with a denominator of 1 can be written as
      --  a real literal for the numerator integer.

      elsif Val.Den = 1 then
         UI_Write (Val.Num, Decimal);
         Write_Str (".0");

      --  Non-based (rational) constants are written in (num/den) style

      else
         Write_Char ('(');
         UI_Write (Val.Num, Decimal);
         Write_Str (".0/");
         UI_Write (Val.Den, Decimal);
         Write_Str (".0)");
      end if;

      --  Add trailing paren for negative values

      if Val.Negative then
         Write_Char (')');
      end if;
   end UR_Write;

   -------------
   -- Ureal_0 --
   -------------

   function Ureal_0 return Ureal is
   begin
      return UR_0;
   end Ureal_0;

   -------------
   -- Ureal_1 --
   -------------

   function Ureal_1 return Ureal is
   begin
      return UR_1;
   end Ureal_1;

   -------------
   -- Ureal_2 --
   -------------

   function Ureal_2 return Ureal is
   begin
      return UR_2;
   end Ureal_2;

   --------------
   -- Ureal_10 --
   --------------

   function Ureal_10 return Ureal is
   begin
      return UR_10;
   end Ureal_10;

   ---------------
   -- Ureal_100 --
   ---------------

   function Ureal_100 return Ureal is
   begin
      return UR_100;
   end Ureal_100;

   -----------------
   -- Ureal_10_36 --
   -----------------

   function Ureal_10_36 return Ureal is
   begin
      return UR_10_36;
   end Ureal_10_36;

   -------------------
   -- Ureal_M_10_36 --
   -------------------

   function Ureal_M_10_36 return Ureal is
   begin
      return UR_M_10_36;
   end Ureal_M_10_36;

   -----------------
   -- Ureal_2_128 --
   -----------------

   function Ureal_2_128 return Ureal is
   begin
      return UR_2_128;
   end Ureal_2_128;

   ----------------
   -- Ureal_2_80 --
   ----------------

   function Ureal_2_80 return Ureal is
   begin
      return UR_2_80;
   end Ureal_2_80;

   -------------------
   -- Ureal_2_M_128 --
   -------------------

   function Ureal_2_M_128 return Ureal is
   begin
      return UR_2_M_128;
   end Ureal_2_M_128;

   -------------------
   -- Ureal_2_M_80 --
   -------------------

   function Ureal_2_M_80 return Ureal is
   begin
      return UR_2_M_80;
   end Ureal_2_M_80;

   ----------------
   -- Ureal_Half --
   ----------------

   function Ureal_Half return Ureal is
   begin
      return UR_Half;
   end Ureal_Half;

   ---------------
   -- Ureal_M_0 --
   ---------------

   function Ureal_M_0 return Ureal is
   begin
      return UR_M_0;
   end Ureal_M_0;

   -----------------
   -- Ureal_Tenth --
   -----------------

   function Ureal_Tenth return Ureal is
   begin
      return UR_Tenth;
   end Ureal_Tenth;

end Urealp;
