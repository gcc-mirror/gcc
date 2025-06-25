------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U R E A L P                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Alloc;
with Output; use Output;
with Table;

package body Urealp is

   Ureal_First_Entry : constant Ureal := Ureal'Succ (No_Ureal);
   --  First subscript allocated in Ureal table (note that we can't just
   --  add 1 to No_Ureal, since "+" means something different for Ureals).

   type Ureal_Entry is record
      Num : Uint;
      --  Numerator (always non-negative)

      Den : Uint;
      --  Denominator (always non-zero, always positive if base is zero)

      Rbase : Nat;
      --  Base value. If Rbase is zero, then the value is simply Num / Den.
      --  If Rbase is non-zero, then the value is Num / (Rbase ** Den)

      Negative : Boolean;
      --  Flag set if value is negative
   end record;

   package Ureals is new Table.Table (
     Table_Component_Type => Ureal_Entry,
     Table_Index_Type     => Ureal'Base,
     Table_Low_Bound      => Ureal_First_Entry,
     Table_Initial        => Alloc.Ureals_Initial,
     Table_Increment      => Alloc.Ureals_Increment,
     Table_Name           => "Ureals");

   --  The following universal reals are the values returned by the constant
   --  functions. They are initialized by the initialization procedure.

   UR_0         : Ureal;
   UR_M_0       : Ureal;
   UR_Tenth     : Ureal;
   UR_Half      : Ureal;
   UR_1         : Ureal;
   UR_2         : Ureal;
   UR_10        : Ureal;
   UR_2_10_18   : Ureal;
   UR_9_10_36   : Ureal;
   UR_10_76     : Ureal;
   UR_M_2_10_18 : Ureal;
   UR_M_9_10_36 : Ureal;
   UR_M_10_76   : Ureal;
   UR_100       : Ureal;
   UR_2_127     : Ureal;
   UR_2_128     : Ureal;
   UR_2_31      : Ureal;
   UR_2_63      : Ureal;
   UR_2_80      : Ureal;
   UR_2_M_127   : Ureal;
   UR_2_M_128   : Ureal;
   UR_2_M_80    : Ureal;

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
   --  decimal number (non-zero digit before decimal point), the estimate is
   --  either correct, or high, but never low. The accuracy of the estimate
   --  affects only the efficiency of the comparison routines.

   function Decimal_Exponent_Lo (V : Ureal) return Int;
   --  Returns an estimate of the exponent of Val represented as a normalized
   --  decimal number (non-zero digit before decimal point), the estimate is
   --  either correct, or low, but never high. The accuracy of the estimate
   --  affects only the efficiency of the comparison routines.

   function Equivalent_Decimal_Exponent (U : Ureal_Entry) return Int;
   --  U is a Ureal entry for which the base value is non-zero, the value
   --  returned is the equivalent decimal exponent value, i.e. the value of
   --  Den, adjusted as though the base were base 10. The value is rounded
   --  toward zero (truncated), and so its value can be off by one.

   function Is_Integer (Num, Den : Uint) return Boolean;
   --  Return true if the real quotient of Num / Den is an integer value

   function Normalize (Val : Ureal_Entry) return Ureal_Entry;
   --  Normalizes the Ureal_Entry by reducing it to lowest terms (with a base
   --  value of 0).

   function Same (U1, U2 : Ureal) return Boolean;
   pragma Inline (Same);
   --  Determines if U1 and U2 are the same Ureal. Note that we cannot use
   --  the equals operator for this test, since that tests for equality, not
   --  identity.

   function Store_Ureal (Val : Ureal_Entry) return Ureal;
   --  This store a new entry in the universal reals table and return its index
   --  in the table.

   function Store_Ureal_Normalized (Val : Ureal_Entry) return Ureal;
   pragma Inline (Store_Ureal_Normalized);
   --  Like Store_Ureal, but normalizes its operand first

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

      --  For based numbers, get the maximum number of digits in the numerator
      --  minus one and the either exact or floor value of the decimal exponent
      --  of the denominator, and subtract. For example:

      --      321 / 10**3 = 3.21E-1
      --      435 / 5**7  = 5.57E-3

      else
         declare
            E : Int;

         begin
            if Val.Rbase = 10 then
               E := UI_To_Int (Val.Den);

            else
               E := Equivalent_Decimal_Exponent (Val);
               if E < 0 then
                  E := E - 1;
               end if;
            end if;

            return UI_Decimal_Digits_Hi (Val.Num) - 1 - E;
         end;
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

      --  For based numbers, get the minimum number of digits in the numerator
      --  minus one and the either exact or ceil value of the decimal exponent
      --  of the denominator, and subtract. For example:

      --      321 / 10**3 = 3.21E-1
      --      435 / 5**7  = 5.57E-3

      else
         declare
            E : Int;

         begin
            if Val.Rbase = 10 then
               E := UI_To_Int (Val.Den);

            else
               E := Equivalent_Decimal_Exponent (Val);
               if E > 0 then
                  E := E + 1;
               end if;
            end if;

            return UI_Decimal_Digits_Lo (Val.Num) - 1 - E;
         end;
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

      type Ratio is record
         Num : Nat;
         Den : Nat;
      end record;

      --  The following table is a table of logs to the base 10. All values
      --  have at least 15 digits of precision, and do not exceed the true
      --  value. To avoid the use of floating point, and as a result potential
      --  target dependency, each entry is represented as a fraction of two
      --  integers.

      Logs : constant array (Nat range 1 .. 16) of Ratio :=
        (1 => (Num =>           0, Den =>            1),  -- 0
         2 => (Num =>  15_392_313, Den =>   51_132_157),  -- 0.301029995663981
         3 => (Num => 731_111_920, Den => 1532_339_867),  -- 0.477121254719662
         4 => (Num =>  30_784_626, Den =>   51_132_157),  -- 0.602059991327962
         5 => (Num => 111_488_153, Den =>  159_503_487),  -- 0.698970004336018
         6 => (Num =>  84_253_929, Den =>  108_274_489),  -- 0.778151250383643
         7 => (Num =>  35_275_468, Den =>   41_741_273),  -- 0.845098040014256
         8 => (Num =>  46_176_939, Den =>   51_132_157),  -- 0.903089986991943
         9 => (Num => 417_620_173, Den =>  437_645_744),  -- 0.954242509439324
        10 => (Num =>           1, Den =>            1),  -- 1.000000000000000
        11 => (Num => 136_507_510, Den =>  131_081_687),  -- 1.041392685158225
        12 => (Num =>  26_797_783, Den =>   24_831_587),  -- 1.079181246047624
        13 => (Num =>  73_333_297, Den =>   65_832_160),  -- 1.113943352306836
        14 => (Num => 102_941_258, Den =>   89_816_543),  -- 1.146128035678238
        15 => (Num =>  53_385_559, Den =>   45_392_361),  -- 1.176091259055681
        16 => (Num =>  78_897_839, Den =>   65_523_237)); -- 1.204119982655924

      function Scale (X : Uint; R : Ratio) return Int;
      --  Compute the value of X scaled by R

      -----------
      -- Scale --
      -----------

      function Scale (X : Uint; R : Ratio) return Int is
      begin
         return UI_To_Int (X * R.Num / R.Den);
      end Scale;

   begin
      pragma Assert (U.Rbase /= 0);
      return Scale (U.Den, Logs (U.Rbase));
   end Equivalent_Decimal_Exponent;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Ureals.Init;
      UR_0         := UR_From_Components (Uint_0, Uint_1,         0, False);
      UR_M_0       := UR_From_Components (Uint_0, Uint_1,         0, True);
      UR_Half      := UR_From_Components (Uint_1, Uint_1,         2, False);
      UR_Tenth     := UR_From_Components (Uint_1, Uint_1,        10, False);
      UR_1         := UR_From_Components (Uint_1, Uint_1,         0, False);
      UR_2         := UR_From_Components (Uint_1, Uint_Minus_1,   2, False);
      UR_10        := UR_From_Components (Uint_1, Uint_Minus_1,  10, False);
      UR_2_10_18   := UR_From_Components (Uint_2, Uint_Minus_18, 10, False);
      UR_9_10_36   := UR_From_Components (Uint_9, Uint_Minus_36, 10, False);
      UR_10_76     := UR_From_Components (Uint_1, Uint_Minus_76, 10, False);
      UR_M_2_10_18 := UR_From_Components (Uint_2, Uint_Minus_18, 10, True);
      UR_M_9_10_36 := UR_From_Components (Uint_9, Uint_Minus_36, 10, True);
      UR_M_10_76   := UR_From_Components (Uint_1, Uint_Minus_76, 10, True);
      UR_100       := UR_From_Components (Uint_1, Uint_Minus_2,  10, False);
      UR_2_127     := UR_From_Components (Uint_1, Uint_Minus_127, 2, False);
      UR_2_M_127   := UR_From_Components (Uint_1, Uint_127,       2, False);
      UR_2_128     := UR_From_Components (Uint_1, Uint_Minus_128, 2, False);
      UR_2_M_128   := UR_From_Components (Uint_1, Uint_128,       2, False);
      UR_2_31      := UR_From_Components (Uint_1, Uint_Minus_31,  2, False);
      UR_2_63      := UR_From_Components (Uint_1, Uint_Minus_63,  2, False);
      UR_2_80      := UR_From_Components (Uint_1, Uint_Minus_80,  2, False);
      UR_2_M_80    := UR_From_Components (Uint_1, Uint_80,        2, False);
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
      M   : constant Uintp.Save_Mark := Mark;

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
      Ureals.Append (Val);

      --  Normalize representation of signed values

      if Val.Num < 0 then
         Ureals.Table (Ureals.Last).Negative := True;
         Ureals.Table (Ureals.Last).Num := -Val.Num;
      end if;

      return Ureals.Last;
   end Store_Ureal;

   ----------------------------
   -- Store_Ureal_Normalized --
   ----------------------------

   function Store_Ureal_Normalized (Val : Ureal_Entry) return Ureal is
   begin
      return Store_Ureal (Normalize (Val));
   end Store_Ureal_Normalized;

   ------------
   -- UR_Abs --
   ------------

   function UR_Abs (Real : Ureal) return Ureal is
      Val : constant Ureal_Entry := Ureals.Table (Real);

   begin
      return Store_Ureal
               ((Num      => Val.Num,
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
      pragma Annotate (CodePeer, Modified, Lval);
      pragma Annotate (CodePeer, Modified, Rval);

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
               return Store_Ureal
                        ((Num      => Uint_0,
                          Den      => Uint_1,
                          Rbase    => 0,
                          Negative => Lval.Negative));

            else
               return Store_Ureal
                        ((Num      => abs Num,
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
               return Store_Ureal
                        ((Num      => Uint_0,
                          Den      => Uint_1,
                          Rbase    => 0,
                          Negative => Lval.Negative));

            else
               return Store_Ureal_Normalized
                        ((Num      => abs Num,
                          Den      => Ln.Den * Rn.Den,
                          Rbase    => 0,
                          Negative => (Num < 0)));
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
      pragma Annotate (CodePeer, Modified, Lval);
      pragma Annotate (CodePeer, Modified, Rval);
      pragma Assert (Rval.Num /= Uint_0);

      if Lval.Rbase = 0 then
         if Rval.Rbase = 0 then
            return Store_Ureal_Normalized
                     ((Num      => Lval.Num * Rval.Den,
                       Den      => Lval.Den * Rval.Num,
                       Rbase    => 0,
                       Negative => Rneg));

         elsif Is_Integer (Lval.Num, Rval.Num * Lval.Den) then
            return Store_Ureal
                     ((Num      => Lval.Num / (Rval.Num * Lval.Den),
                       Den      => (-Rval.Den),
                       Rbase    => Rval.Rbase,
                       Negative => Rneg));

         elsif Rval.Den < 0 then
            return Store_Ureal_Normalized
                     ((Num      => Lval.Num,
                       Den      => Rval.Rbase ** (-Rval.Den) *
                                   Rval.Num *
                                   Lval.Den,
                       Rbase    => 0,
                       Negative => Rneg));

         else
            return Store_Ureal_Normalized
                     ((Num      => Lval.Num * Rval.Rbase ** Rval.Den,
                       Den      => Rval.Num * Lval.Den,
                       Rbase    => 0,
                       Negative => Rneg));
         end if;

      elsif Is_Integer (Lval.Num, Rval.Num) then
         if Rval.Rbase = Lval.Rbase then
            return Store_Ureal
                     ((Num      => Lval.Num / Rval.Num,
                       Den      => Lval.Den - Rval.Den,
                       Rbase    => Lval.Rbase,
                       Negative => Rneg));

         elsif Rval.Rbase = 0 then
            return Store_Ureal
                     ((Num      => (Lval.Num / Rval.Num) * Rval.Den,
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

               return Store_Ureal
                        ((Num      => Num,
                          Den      => Den,
                          Rbase    => 0,
                          Negative => Rneg));
            end;

         else
            return Store_Ureal
                     ((Num      => (Lval.Num / Rval.Num) *
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

            return Store_Ureal_Normalized
                     ((Num      => Num,
                       Den      => Den,
                       Rbase    => 0,
                       Negative => Rneg));
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
         return Store_Ureal
                  ((Num      => Uint_1,
                    Den      => -N,
                    Rbase    => UI_To_Int (IBas),
                    Negative => Neg));

      --  If the exponent is negative then we raise the numerator and the
      --  denominator (after normalization) to the absolute value of the
      --  exponent and we return the reciprocal. An assert error will happen
      --  if the numerator is zero.

      elsif N < 0 then
         pragma Assert (Val.Num /= 0);
         Val := Normalize (Val);

         return Store_Ureal
                  ((Num      => Val.Den ** X,
                    Den      => Val.Num ** X,
                    Rbase    => 0,
                    Negative => Neg));

      --  If positive, we distinguish the case when the base is not zero, in
      --  which case the new denominator is just the product of the old one
      --  with the exponent,

      else
         if Val.Rbase /= 0 then

            return Store_Ureal
                     ((Num      => Val.Num ** X,
                       Den      => Val.Den * X,
                       Rbase    => Val.Rbase,
                       Negative => Neg));

         --  And when the base is zero, in which case we exponentiate
         --  the old denominator.

         else
            return Store_Ureal
                     ((Num      => Val.Num ** X,
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
      return Store_Ureal
               ((Num      => Num,
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
            return Store_Ureal_Normalized
                     ((Num      => Num,
                       Den      => Lval.Den * Rval.Den,
                       Rbase    => 0,
                       Negative => Rneg));

         elsif Is_Integer (Num, Lval.Den) then
            return Store_Ureal
                     ((Num      => Num / Lval.Den,
                       Den      => Rval.Den,
                       Rbase    => Rval.Rbase,
                       Negative => Rneg));

         elsif Rval.Den < 0 then
            return Store_Ureal_Normalized
                     ((Num      => Num * (Rval.Rbase ** (-Rval.Den)),
                       Den      => Lval.Den,
                       Rbase    => 0,
                       Negative => Rneg));

         else
            return Store_Ureal_Normalized
                     ((Num      => Num,
                       Den      => Lval.Den * (Rval.Rbase ** Rval.Den),
                       Rbase    => 0,
                       Negative => Rneg));
         end if;

      elsif Lval.Rbase = Rval.Rbase then
         return Store_Ureal
                  ((Num      => Num,
                    Den      => Lval.Den + Rval.Den,
                    Rbase    => Lval.Rbase,
                    Negative => Rneg));

      elsif Rval.Rbase = 0 then
         if Is_Integer (Num, Rval.Den) then
            return Store_Ureal
                     ((Num      => Num / Rval.Den,
                       Den      => Lval.Den,
                       Rbase    => Lval.Rbase,
                       Negative => Rneg));

         elsif Lval.Den < 0 then
            return Store_Ureal_Normalized
                     ((Num      => Num * (Lval.Rbase ** (-Lval.Den)),
                       Den      => Rval.Den,
                       Rbase    => 0,
                       Negative => Rneg));

         else
            return Store_Ureal_Normalized
                     ((Num      => Num,
                       Den      => Rval.Den * (Lval.Rbase ** Lval.Den),
                       Rbase    => 0,
                       Negative => Rneg));
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

         return Store_Ureal_Normalized
                  ((Num      => Num,
                    Den      => Den,
                    Rbase    => 0,
                    Negative => Rneg));
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
      Val : constant Ureal_Entry := Ureals.Table (Real);
   begin
      return Store_Ureal
               ((Num      => Val.Num,
                 Den      => Val.Den,
                 Rbase    => Val.Rbase,
                 Negative => not Val.Negative));
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

   procedure UR_Write (Real : Ureal; Brackets : Boolean := False) is
      Val : constant Ureal_Entry := Ureals.Table (Real);
      T   : Uint;

   begin
      --  If value is negative, we precede the constant by a minus sign

      if Val.Negative then
         Write_Char ('-');
      end if;

      --  Zero is zero

      if Val.Num = 0 then
         Write_Str ("0.0");

      --  For constants with a denominator of zero, the value is simply the
      --  numerator value, since we are dividing by base**0, which is 1.

      elsif Val.Den = 0 then
         UI_Write (Val.Num, Decimal);
         Write_Str (".0");

      --  Small powers of 2 get written in decimal fixed-point format

      elsif Val.Rbase = 2
        and then Val.Den <= 3
        and then Val.Den >= -16
      then
         if Val.Den = 1 then
            T := Val.Num * (10 / 2);
            UI_Write (T / 10, Decimal);
            Write_Char ('.');
            UI_Write (T mod 10, Decimal);

         elsif Val.Den = 2 then
            T := Val.Num * (100 / 4);
            UI_Write (T / 100, Decimal);
            Write_Char ('.');
            UI_Write (T mod 100 / 10, Decimal);

            if T mod 10 /= 0 then
               UI_Write (T mod 10, Decimal);
            end if;

         elsif Val.Den = 3 then
            T := Val.Num * (1000 / 8);
            UI_Write (T / 1000, Decimal);
            Write_Char ('.');
            UI_Write (T mod 1000 / 100, Decimal);

            if T mod 100 /= 0 then
               UI_Write (T mod 100 / 10, Decimal);

               if T mod 10 /= 0 then
                  UI_Write (T mod 10, Decimal);
               end if;
            end if;

         else
            UI_Write (Val.Num * (Uint_2 ** (-Val.Den)), Decimal);
            Write_Str (".0");
         end if;

      --  Constants in base 10 or 16 can be written in normal Ada literal
      --  style, as long as they fit in the UI_Image_Buffer. Using hexadecimal
      --  notation, 4 bytes are required for the 16# # part, and every fifth
      --  character is an underscore. So, a buffer of size N has room for
      --     ((N - 4) - (N - 4) / 5) * 4 bits,
      --  or at least
      --     N * 16 / 5 - 12 bits.

      elsif (Val.Rbase = 10 or else Val.Rbase = 16)
        and then Num_Bits (Val.Num) < UI_Image_Buffer'Length * 16 / 5 - 12
      then
         pragma Assert (Val.Den /= 0);

         --  Use fixed-point format for small scaling values

         if (Val.Rbase = 10 and then Val.Den < 0 and then Val.Den > -3)
              or else (Val.Rbase = 16 and then Val.Den = -1)
         then
            UI_Write (Val.Num * Val.Rbase**(-Val.Den), Decimal);
            Write_Str (".0");

         --  Write hexadecimal constants in exponential notation with a zero
         --  unit digit. This matches the Ada canonical form for floating point
         --  numbers, and also ensures that the underscores end up in the
         --  correct place.

         elsif Val.Rbase = 16 then
            UI_Image (Val.Num, Hex);
            pragma Assert (Val.Rbase = 16);

            Write_Str ("16#0.");
            Write_Str (UI_Image_Buffer (4 .. UI_Image_Length));

            --  For exponent, exclude 16# # and underscores from length

            UI_Image_Length := UI_Image_Length - 4;
            UI_Image_Length := UI_Image_Length - UI_Image_Length / 5;

            Write_Char ('E');
            UI_Write (Int (UI_Image_Length) - Val.Den, Decimal);

         elsif Val.Den = 1 then
            UI_Write (Val.Num / 10, Decimal);
            Write_Char ('.');
            UI_Write (Val.Num mod 10, Decimal);

         elsif Val.Den = 2 then
            UI_Write (Val.Num / 100, Decimal);
            Write_Char ('.');
            UI_Write (Val.Num / 10 mod 10, Decimal);
            UI_Write (Val.Num mod 10, Decimal);

         --  Else use decimal exponential format

         else
            --  Write decimal constants with a non-zero unit digit. This
            --  matches usual scientific notation.

            UI_Image (Val.Num, Decimal);
            Write_Char (UI_Image_Buffer (1));
            Write_Char ('.');

            if UI_Image_Length = 1 then
               Write_Char ('0');
            else
               Write_Str (UI_Image_Buffer (2 .. UI_Image_Length));
            end if;

            Write_Char ('E');
            UI_Write (Int (UI_Image_Length - 1) - Val.Den, Decimal);
         end if;

      --  Other constants with a base other than 10 are written using one of
      --  the following forms, depending on the sign of the number and the
      --  sign of the exponent (= minus denominator value). See that we are
      --  replacing the division by a multiplication (updating accordingly the
      --  sign of the exponent) to generate an expression whose computation
      --  does not cause a division by 0 when base**exponent is very small.

      --    numerator.0*base**exponent
      --    numerator.0*base**-exponent

      --  And of course an exponent of 0 can be omitted.

      elsif Val.Rbase /= 0 then
         if Brackets then
            Write_Char ('[');
         end if;

         UI_Write (Val.Num, Decimal);
         Write_Str (".0");

         if Val.Den /= 0 then
            Write_Char ('*');
            Write_Int (Val.Rbase);
            Write_Str ("**");

            if Val.Den <= 0 then
               UI_Write (-Val.Den, Decimal);
            else
               Write_Str ("(-");
               UI_Write (Val.Den, Decimal);
               Write_Char (')');
            end if;
         end if;

         if Brackets then
            Write_Char (']');
         end if;

      --  Rationals where numerator is divisible by denominator can be output
      --  as literals after we do the division. This includes the common case
      --  where the denominator is 1.

      elsif Val.Num mod Val.Den = 0 then
         UI_Write (Val.Num / Val.Den, Decimal);
         Write_Str (".0");

      --  Other non-based (rational) constants are written in num/den style

      else
         if Brackets then
            Write_Char ('[');
         end if;

         UI_Write (Val.Num, Decimal);
         Write_Str (".0/");
         UI_Write (Val.Den, Decimal);
         Write_Str (".0");

         if Brackets then
            Write_Char (']');
         end if;
      end if;
   end UR_Write;

   ----------------------
   -- UR_Write_To_JSON --
   ----------------------

   --  We defer to the implementation of UR_Write for values that are naturally
   --  written in a JSON compatible format and write a fraction for the others.

   procedure UR_Write_To_JSON (Real : Ureal) is
      Val : constant Ureal_Entry := Ureals.Table (Real);

      T : Ureal;

   begin
      --  Zero is zero

      if Val.Num = 0 then
         T := Real;

      --  For constants with a denominator of zero, the value is simply the
      --  numerator value, since we are dividing by base**0, which is 1.

      elsif Val.Den = 0 then
         T := Real;

      --  Small powers of 2 get written in decimal fixed-point format

      elsif Val.Rbase = 2
        and then Val.Den <= 3
        and then Val.Den >= -16
      then
         T := Real;

      --  Constants in base 10 can be written in normal Ada literal style

      elsif Val.Rbase = 10 then
         T := Real;

      --  Rationals where numerator is divisible by denominator can be output
      --  as literals after we do the division. This includes the common case
      --  where the denominator is 1.

      elsif Val.Rbase = 0 and then Val.Num mod Val.Den = 0 then
         T := Real;

      --  Other non-based (rational) constants are written in num/den style

      else
         Write_Str ("{ ""code"": ""/"", ""operands"": [ ");
         if Val.Negative then
            Write_Char ('-');
         end if;
         UI_Write (Val.Num, Decimal);
         Write_Str (".0, ");
         UI_Write (Val.Den, Decimal);
         Write_Str (".0 ] }");
         return;
      end if;

      UR_Write (T);
   end UR_Write_To_JSON;

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

   -------------------
   -- Ureal_2_10_18 --
   -------------------

   function Ureal_2_10_18 return Ureal is
   begin
      return UR_2_10_18;
   end Ureal_2_10_18;

   -------------------
   -- Ureal_9_10_36 --
   -------------------

   function Ureal_9_10_36 return Ureal is
   begin
      return UR_9_10_36;
   end Ureal_9_10_36;

   -----------------
   -- Ureal_10_76 --
   -----------------

   function Ureal_10_76 return Ureal is
   begin
      return UR_10_76;
   end Ureal_10_76;

   ----------------
   -- Ureal_2_31 --
   ----------------

   function Ureal_2_31 return Ureal is
   begin
      return UR_2_31;
   end Ureal_2_31;

   ----------------
   -- Ureal_2_63 --
   ----------------

   function Ureal_2_63 return Ureal is
   begin
      return UR_2_63;
   end Ureal_2_63;

   ----------------
   -- Ureal_2_80 --
   ----------------

   function Ureal_2_80 return Ureal is
   begin
      return UR_2_80;
   end Ureal_2_80;

   -----------------
   -- Ureal_2_127 --
   -----------------

   function Ureal_2_127 return Ureal is
   begin
      return UR_2_127;
   end Ureal_2_127;

   -----------------
   -- Ureal_2_128 --
   -----------------

   function Ureal_2_128 return Ureal is
   begin
      return UR_2_128;
   end Ureal_2_128;

   -------------------
   -- Ureal_2_M_80 --
   -------------------

   function Ureal_2_M_80 return Ureal is
   begin
      return UR_2_M_80;
   end Ureal_2_M_80;

   -------------------
   -- Ureal_2_M_127 --
   -------------------

   function Ureal_2_M_127 return Ureal is
   begin
      return UR_2_M_127;
   end Ureal_2_M_127;

   -------------------
   -- Ureal_2_M_128 --
   -------------------

   function Ureal_2_M_128 return Ureal is
   begin
      return UR_2_M_128;
   end Ureal_2_M_128;

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

   ---------------------
   -- Ureal_M_2_10_18 --
   ---------------------

   function Ureal_M_2_10_18 return Ureal is
   begin
      return UR_M_2_10_18;
   end Ureal_M_2_10_18;

   ---------------------
   -- Ureal_M_9_10_36 --
   ---------------------

   function Ureal_M_9_10_36 return Ureal is
   begin
      return UR_M_9_10_36;
   end Ureal_M_9_10_36;

   -------------------
   -- Ureal_M_10_76 --
   -------------------

   function Ureal_M_10_76 return Ureal is
   begin
      return UR_M_10_76;
   end Ureal_M_10_76;

   -----------------
   -- Ureal_Tenth --
   -----------------

   function Ureal_Tenth return Ureal is
   begin
      return UR_Tenth;
   end Ureal_Tenth;

end Urealp;
