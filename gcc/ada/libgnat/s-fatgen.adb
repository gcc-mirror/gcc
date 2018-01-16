------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F A T _ G E N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

--  The implementation here is portable to any IEEE implementation. It does
--  not handle nonbinary radix, and also assumes that model numbers and
--  machine numbers are basically identical, which is not true of all possible
--  floating-point implementations. On a non-IEEE machine, this body must be
--  specialized appropriately, or better still, its generic instantiations
--  should be replaced by efficient machine-specific code.

with Ada.Unchecked_Conversion;
with System;
package body System.Fat_Gen is

   Float_Radix        : constant T := T (T'Machine_Radix);
   Radix_To_M_Minus_1 : constant T := Float_Radix ** (T'Machine_Mantissa - 1);

   pragma Assert (T'Machine_Radix = 2);
   --  This version does not handle radix 16

   --  Constants for Decompose and Scaling

   Rad    : constant T := T (T'Machine_Radix);
   Invrad : constant T := 1.0 / Rad;

   subtype Expbits is Integer range 0 .. 6;
   --  2 ** (2 ** 7) might overflow.  How big can radix-16 exponents get?

   Log_Power : constant array (Expbits) of Integer := (1, 2, 4, 8, 16, 32, 64);

   R_Power : constant array (Expbits) of T :=
     (Rad **  1,
      Rad **  2,
      Rad **  4,
      Rad **  8,
      Rad ** 16,
      Rad ** 32,
      Rad ** 64);

   R_Neg_Power : constant array (Expbits) of T :=
     (Invrad **  1,
      Invrad **  2,
      Invrad **  4,
      Invrad **  8,
      Invrad ** 16,
      Invrad ** 32,
      Invrad ** 64);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Decompose (XX : T; Frac : out T; Expo : out UI);
   --  Decomposes a floating-point number into fraction and exponent parts.
   --  Both results are signed, with Frac having the sign of XX, and UI has
   --  the sign of the exponent. The absolute value of Frac is in the range
   --  0.0 <= Frac < 1.0. If Frac = 0.0 or -0.0, then Expo is always zero.

   function Gradual_Scaling (Adjustment : UI) return T;
   --  Like Scaling with a first argument of 1.0, but returns the smallest
   --  denormal rather than zero when the adjustment is smaller than
   --  Machine_Emin. Used for Succ and Pred.

   --------------
   -- Adjacent --
   --------------

   function Adjacent (X, Towards : T) return T is
   begin
      if Towards = X then
         return X;
      elsif Towards > X then
         return Succ (X);
      else
         return Pred (X);
      end if;
   end Adjacent;

   -------------
   -- Ceiling --
   -------------

   function Ceiling (X : T) return T is
      XT : constant T := Truncation (X);
   begin
      if X <= 0.0 then
         return XT;
      elsif X = XT then
         return X;
      else
         return XT + 1.0;
      end if;
   end Ceiling;

   -------------
   -- Compose --
   -------------

   function Compose (Fraction : T; Exponent : UI) return T is
      Arg_Frac : T;
      Arg_Exp  : UI;
      pragma Unreferenced (Arg_Exp);
   begin
      Decompose (Fraction, Arg_Frac, Arg_Exp);
      return Scaling (Arg_Frac, Exponent);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (Value, Sign : T) return T is
      Result : T;

      function Is_Negative (V : T) return Boolean;
      pragma Import (Intrinsic, Is_Negative);

   begin
      Result := abs Value;

      if Is_Negative (Sign) then
         return -Result;
      else
         return Result;
      end if;
   end Copy_Sign;

   ---------------
   -- Decompose --
   ---------------

   procedure Decompose (XX : T; Frac : out T; Expo : out UI) is
      X : constant T := T'Machine (XX);

   begin
      if X = 0.0 then

         --  The normalized exponent of zero is zero, see RM A.5.2(15)

         Frac := X;
         Expo := 0;

      --  Check for infinities, transfinites, whatnot

      elsif X > T'Safe_Last then
         Frac := Invrad;
         Expo := T'Machine_Emax + 1;

      elsif X < T'Safe_First then
         Frac := -Invrad;
         Expo := T'Machine_Emax + 2;    -- how many extra negative values?

      else
         --  Case of nonzero finite x. Essentially, we just multiply
         --  by Rad ** (+-2**N) to reduce the range.

         declare
            Ax : T  := abs X;
            Ex : UI := 0;

         --  Ax * Rad ** Ex is invariant

         begin
            if Ax >= 1.0 then
               while Ax >= R_Power (Expbits'Last) loop
                  Ax := Ax * R_Neg_Power (Expbits'Last);
                  Ex := Ex + Log_Power (Expbits'Last);
               end loop;

               --  Ax < Rad ** 64

               for N in reverse Expbits'First .. Expbits'Last - 1 loop
                  if Ax >= R_Power (N) then
                     Ax := Ax * R_Neg_Power (N);
                     Ex := Ex + Log_Power (N);
                  end if;

                  --  Ax < R_Power (N)

               end loop;

               --  1 <= Ax < Rad

               Ax := Ax * Invrad;
               Ex := Ex + 1;

            else
               --  0 < ax < 1

               while Ax < R_Neg_Power (Expbits'Last) loop
                  Ax := Ax * R_Power (Expbits'Last);
                  Ex := Ex - Log_Power (Expbits'Last);
               end loop;

               --  Rad ** -64 <= Ax < 1

               for N in reverse Expbits'First .. Expbits'Last - 1 loop
                  if Ax < R_Neg_Power (N) then
                     Ax := Ax * R_Power (N);
                     Ex := Ex - Log_Power (N);
                  end if;

                  --  R_Neg_Power (N) <= Ax < 1

               end loop;
            end if;

            Frac := (if X > 0.0 then Ax else -Ax);
            Expo := Ex;
         end;
      end if;
   end Decompose;

   --------------
   -- Exponent --
   --------------

   function Exponent (X : T) return UI is
      X_Frac : T;
      X_Exp  : UI;
      pragma Unreferenced (X_Frac);
   begin
      Decompose (X, X_Frac, X_Exp);
      return X_Exp;
   end Exponent;

   -----------
   -- Floor --
   -----------

   function Floor (X : T) return T is
      XT : constant T := Truncation (X);
   begin
      if X >= 0.0 then
         return XT;
      elsif XT = X then
         return X;
      else
         return XT - 1.0;
      end if;
   end Floor;

   --------------
   -- Fraction --
   --------------

   function Fraction (X : T) return T is
      X_Frac : T;
      X_Exp  : UI;
      pragma Unreferenced (X_Exp);
   begin
      Decompose (X, X_Frac, X_Exp);
      return X_Frac;
   end Fraction;

   ---------------------
   -- Gradual_Scaling --
   ---------------------

   function Gradual_Scaling  (Adjustment : UI) return T is
      Y  : T;
      Y1 : T;
      Ex : UI := Adjustment;

   begin
      if Adjustment < T'Machine_Emin - 1 then
         Y  := 2.0 ** T'Machine_Emin;
         Y1 := Y;
         Ex := Ex - T'Machine_Emin;
         while Ex < 0 loop
            Y := T'Machine (Y / 2.0);

            if Y = 0.0 then
               return Y1;
            end if;

            Ex := Ex + 1;
            Y1 := Y;
         end loop;

         return Y1;

      else
         return Scaling (1.0, Adjustment);
      end if;
   end Gradual_Scaling;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (X : T; Radix_Digits : UI) return T is
      L    : UI;
      Y, Z : T;

   begin
      if Radix_Digits >= T'Machine_Mantissa then
         return X;

      elsif Radix_Digits <= 0 then
         raise Constraint_Error;

      else
         L := Exponent (X) - Radix_Digits;
         Y := Truncation (Scaling (X, -L));
         Z := Scaling (Y, L);
         return Z;
      end if;
   end Leading_Part;

   -------------
   -- Machine --
   -------------

   --  The trick with Machine is to force the compiler to store the result
   --  in memory so that we do not have extra precision used. The compiler
   --  is clever, so we have to outwit its possible optimizations. We do
   --  this by using an intermediate pragma Volatile location.

   function Machine (X : T) return T is
      Temp : T;
      pragma Volatile (Temp);
   begin
      Temp := X;
      return Temp;
   end Machine;

   ----------------------
   -- Machine_Rounding --
   ----------------------

   --  For now, the implementation is identical to that of Rounding, which is
   --  a permissible behavior, but is not the most efficient possible approach.

   function Machine_Rounding (X : T) return T is
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (abs X);
      Tail   := abs X - Result;

      if Tail >= 0.5 then
         Result := Result + 1.0;
      end if;

      if X > 0.0 then
         return Result;

      elsif X < 0.0 then
         return -Result;

      --  For zero case, make sure sign of zero is preserved

      else
         return X;
      end if;
   end Machine_Rounding;

   -----------
   -- Model --
   -----------

   --  We treat Model as identical to Machine. This is true of IEEE and other
   --  nice floating-point systems, but not necessarily true of all systems.

   function Model (X : T) return T is
   begin
      return T'Machine (X);
   end Model;

   ----------
   -- Pred --
   ----------

   function Pred (X : T) return T is
      X_Frac : T;
      X_Exp  : UI;

   begin
      --  Zero has to be treated specially, since its exponent is zero

      if X = 0.0 then
         return -Succ (X);

      --  Special treatment for most negative number

      elsif X = T'First then

         --  If not generating infinities, we raise a constraint error

         if T'Machine_Overflows then
            raise Constraint_Error with "Pred of largest negative number";

         --  Otherwise generate a negative infinity

         else
            return X / (X - X);
         end if;

      --  For infinities, return unchanged

      elsif X < T'First or else X > T'Last then
         return X;

      --  Subtract from the given number a number equivalent to the value
      --  of its least significant bit. Given that the most significant bit
      --  represents a value of 1.0 * radix ** (exp - 1), the value we want
      --  is obtained by shifting this by (mantissa-1) bits to the right,
      --  i.e. decreasing the exponent by that amount.

      else
         Decompose (X, X_Frac, X_Exp);

         --  A special case, if the number we had was a positive power of
         --  two, then we want to subtract half of what we would otherwise
         --  subtract, since the exponent is going to be reduced.

         --  Note that X_Frac has the same sign as X, so if X_Frac is 0.5,
         --  then we know that we have a positive number (and hence a
         --  positive power of 2).

         if X_Frac = 0.5 then
            return X - Gradual_Scaling (X_Exp - T'Machine_Mantissa - 1);

         --  Otherwise the exponent is unchanged

         else
            return X - Gradual_Scaling (X_Exp - T'Machine_Mantissa);
         end if;
      end if;
   end Pred;

   ---------------
   -- Remainder --
   ---------------

   function Remainder (X, Y : T) return T is
      A        : T;
      B        : T;
      Arg      : T;
      P        : T;
      P_Frac   : T;
      Sign_X   : T;
      IEEE_Rem : T;
      Arg_Exp  : UI;
      P_Exp    : UI;
      K        : UI;
      P_Even   : Boolean;

      Arg_Frac : T;
      pragma Unreferenced (Arg_Frac);

   begin
      if Y = 0.0 then
         raise Constraint_Error;
      end if;

      if X > 0.0 then
         Sign_X :=  1.0;
         Arg := X;
      else
         Sign_X := -1.0;
         Arg := -X;
      end if;

      P := abs Y;

      if Arg < P then
         P_Even := True;
         IEEE_Rem := Arg;
         P_Exp := Exponent (P);

      else
         Decompose (Arg, Arg_Frac, Arg_Exp);
         Decompose (P,   P_Frac,   P_Exp);

         P := Compose (P_Frac, Arg_Exp);
         K := Arg_Exp - P_Exp;
         P_Even := True;
         IEEE_Rem := Arg;

         for Cnt in reverse 0 .. K loop
            if IEEE_Rem >= P then
               P_Even := False;
               IEEE_Rem := IEEE_Rem - P;
            else
               P_Even := True;
            end if;

            P := P * 0.5;
         end loop;
      end if;

      --  That completes the calculation of modulus remainder. The final
      --  step is get the IEEE remainder. Here we need to compare Rem with
      --  (abs Y) / 2. We must be careful of unrepresentable Y/2 value
      --  caused by subnormal numbers

      if P_Exp >= 0 then
         A := IEEE_Rem;
         B := abs Y * 0.5;

      else
         A := IEEE_Rem * 2.0;
         B := abs Y;
      end if;

      if A > B or else (A = B and then not P_Even) then
         IEEE_Rem := IEEE_Rem - abs Y;
      end if;

      return Sign_X * IEEE_Rem;
   end Remainder;

   --------------
   -- Rounding --
   --------------

   function Rounding (X : T) return T is
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (abs X);
      Tail   := abs X - Result;

      if Tail >= 0.5 then
         Result := Result + 1.0;
      end if;

      if X > 0.0 then
         return Result;

      elsif X < 0.0 then
         return -Result;

      --  For zero case, make sure sign of zero is preserved

      else
         return X;
      end if;
   end Rounding;

   -------------
   -- Scaling --
   -------------

   --  Return x * rad ** adjustment quickly, or quietly underflow to zero,
   --  or overflow naturally.

   function Scaling (X : T; Adjustment : UI) return T is
   begin
      if X = 0.0 or else Adjustment = 0 then
         return X;
      end if;

      --  Nonzero x essentially, just multiply repeatedly by Rad ** (+-2**n)

      declare
         Y  : T  := X;
         Ex : UI := Adjustment;

      --  Y * Rad ** Ex is invariant

      begin
         if Ex < 0 then
            while Ex <= -Log_Power (Expbits'Last) loop
               Y := Y * R_Neg_Power (Expbits'Last);
               Ex := Ex + Log_Power (Expbits'Last);
            end loop;

            --  -64 < Ex <= 0

            for N in reverse Expbits'First .. Expbits'Last - 1 loop
               if Ex <= -Log_Power (N) then
                  Y := Y * R_Neg_Power (N);
                  Ex := Ex + Log_Power (N);
               end if;

               --  -Log_Power (N) < Ex <= 0

            end loop;

            --  Ex = 0

         else
            --  Ex >= 0

            while Ex >= Log_Power (Expbits'Last) loop
               Y := Y * R_Power (Expbits'Last);
               Ex := Ex - Log_Power (Expbits'Last);
            end loop;

            --  0 <= Ex < 64

            for N in reverse Expbits'First .. Expbits'Last - 1 loop
               if Ex >= Log_Power (N) then
                  Y := Y * R_Power (N);
                  Ex := Ex - Log_Power (N);
               end if;

               --  0 <= Ex < Log_Power (N)

            end loop;

            --  Ex = 0

         end if;

         return Y;
      end;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (X : T) return T is
      X_Frac : T;
      X_Exp  : UI;
      X1, X2 : T;

   begin
      --  Treat zero specially since it has a zero exponent

      if X = 0.0 then
         X1 := 2.0 ** T'Machine_Emin;

         --  Following loop generates smallest denormal

         loop
            X2 := T'Machine (X1 / 2.0);
            exit when X2 = 0.0;
            X1 := X2;
         end loop;

         return X1;

      --  Special treatment for largest positive number

      elsif X = T'Last then

         --  If not generating infinities, we raise a constraint error

         if T'Machine_Overflows then
            raise Constraint_Error with "Succ of largest negative number";

         --  Otherwise generate a positive infinity

         else
            return X / (X - X);
         end if;

      --  For infinities, return unchanged

      elsif X < T'First or else X > T'Last then
         return X;

      --  Add to the given number a number equivalent to the value
      --  of its least significant bit. Given that the most significant bit
      --  represents a value of 1.0 * radix ** (exp - 1), the value we want
      --  is obtained by shifting this by (mantissa-1) bits to the right,
      --  i.e. decreasing the exponent by that amount.

      else
         Decompose (X, X_Frac, X_Exp);

         --  A special case, if the number we had was a negative power of two,
         --  then we want to add half of what we would otherwise add, since the
         --  exponent is going to be reduced.

         --  Note that X_Frac has the same sign as X, so if X_Frac is -0.5,
         --  then we know that we have a negative number (and hence a negative
         --  power of 2).

         if X_Frac = -0.5 then
            return X + Gradual_Scaling (X_Exp - T'Machine_Mantissa - 1);

         --  Otherwise the exponent is unchanged

         else
            return X + Gradual_Scaling (X_Exp - T'Machine_Mantissa);
         end if;
      end if;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   --  The basic approach is to compute

   --    T'Machine (RM1 + N) - RM1

   --  where N >= 0.0 and RM1 = radix ** (mantissa - 1)

   --  This works provided that the intermediate result (RM1 + N) does not
   --  have extra precision (which is why we call Machine). When we compute
   --  RM1 + N, the exponent of N will be normalized and the mantissa shifted
   --  appropriately so the lower order bits, which cannot contribute to the
   --  integer part of N, fall off on the right. When we subtract RM1 again,
   --  the significant bits of N are shifted to the left, and what we have is
   --  an integer, because only the first e bits are different from zero
   --  (assuming binary radix here).

   function Truncation (X : T) return T is
      Result : T;

   begin
      Result := abs X;

      if Result >= Radix_To_M_Minus_1 then
         return T'Machine (X);

      else
         Result :=
           T'Machine (Radix_To_M_Minus_1 + Result) - Radix_To_M_Minus_1;

         if Result > abs X then
            Result := Result - 1.0;
         end if;

         if X > 0.0 then
            return  Result;

         elsif X < 0.0 then
            return -Result;

         --  For zero case, make sure sign of zero is preserved

         else
            return X;
         end if;
      end if;
   end Truncation;

   -----------------------
   -- Unbiased_Rounding --
   -----------------------

   function Unbiased_Rounding (X : T) return T is
      Abs_X  : constant T := abs X;
      Result : T;
      Tail   : T;

   begin
      Result := Truncation (Abs_X);
      Tail   := Abs_X - Result;

      if Tail > 0.5 then
         Result := Result + 1.0;

      elsif Tail = 0.5 then
         Result := 2.0 * Truncation ((Result / 2.0) + 0.5);
      end if;

      if X > 0.0 then
         return Result;

      elsif X < 0.0 then
         return -Result;

      --  For zero case, make sure sign of zero is preserved

      else
         return X;
      end if;
   end Unbiased_Rounding;

   -----------
   -- Valid --
   -----------

   function Valid (X : not null access T) return Boolean is
      IEEE_Emin : constant Integer := T'Machine_Emin - 1;
      IEEE_Emax : constant Integer := T'Machine_Emax - 1;

      IEEE_Bias : constant Integer := -(IEEE_Emin - 1);

      subtype IEEE_Exponent_Range is
        Integer range IEEE_Emin - 1 .. IEEE_Emax + 1;

      --  The implementation of this floating point attribute uses a
      --  representation type Float_Rep that allows direct access to the
      --  exponent and mantissa parts of a floating point number.

      --  The Float_Rep type is an array of Float_Word elements. This
      --  representation is chosen to make it possible to size the type based
      --  on a generic parameter. Since the array size is known at compile
      --  time, efficient code can still be generated. The size of Float_Word
      --  elements should be large enough to allow accessing the exponent in
      --  one read, but small enough so that all floating point object sizes
      --  are a multiple of the Float_Word'Size.

      --  The following conditions must be met for all possible instantiations
      --  of the attributes package:

      --    - T'Size is an integral multiple of Float_Word'Size

      --    - The exponent and sign are completely contained in a single
      --      component of Float_Rep, named Most_Significant_Word (MSW).

      --    - The sign occupies the most significant bit of the MSW and the
      --      exponent is in the following bits. Unused bits (if any) are in
      --      the least significant part.

      type Float_Word is mod 2**Positive'Min (System.Word_Size, 32);
      type Rep_Index is range 0 .. 7;

      Rep_Words : constant Positive :=
                    (T'Size + Float_Word'Size - 1) / Float_Word'Size;
      Rep_Last  : constant Rep_Index :=
                    Rep_Index'Min
                      (Rep_Index (Rep_Words - 1),
                       (T'Mantissa + 16) / Float_Word'Size);
      --  Determine the number of Float_Words needed for representing the
      --  entire floating-point value. Do not take into account excessive
      --  padding, as occurs on IA-64 where 80 bits floats get padded to 128
      --  bits. In general, the exponent field cannot be larger than 15 bits,
      --  even for 128-bit floating-point types, so the final format size
      --  won't be larger than T'Mantissa + 16.

      type Float_Rep is
         array (Rep_Index range 0 .. Rep_Index (Rep_Words - 1)) of Float_Word;

      pragma Suppress_Initialization (Float_Rep);
      --  This pragma suppresses the generation of an initialization procedure
      --  for type Float_Rep when operating in Initialize/Normalize_Scalars
      --  mode. This is not just a matter of efficiency, but of functionality,
      --  since Valid has a pragma Inline_Always, which is not permitted if
      --  there are nested subprograms present.

      Most_Significant_Word : constant Rep_Index :=
                                Rep_Last * Standard'Default_Bit_Order;
      --  Finding the location of the Exponent_Word is a bit tricky. In general
      --  we assume Word_Order = Bit_Order.

      Exponent_Factor : constant Float_Word :=
                          2**(Float_Word'Size - 1) /
                            Float_Word (IEEE_Emax - IEEE_Emin + 3) *
                              Boolean'Pos (Most_Significant_Word /= 2) +
                                Boolean'Pos (Most_Significant_Word = 2);
      --  Factor that the extracted exponent needs to be divided by to be in
      --  range 0 .. IEEE_Emax - IEEE_Emin + 2. Special case: Exponent_Factor
      --  is 1 for x86/IA64 double extended (GCC adds unused bits to the type).

      Exponent_Mask : constant Float_Word :=
                        Float_Word (IEEE_Emax - IEEE_Emin + 2) *
                          Exponent_Factor;
      --  Value needed to mask out the exponent field. This assumes that the
      --  range IEEE_Emin - 1 .. IEEE_Emax + contains 2**N values, for some N
      --  in Natural.

      function To_Float is new Ada.Unchecked_Conversion (Float_Rep, T);

      type Float_Access is access all T;
      function To_Address is
         new Ada.Unchecked_Conversion (Float_Access, System.Address);

      XA : constant System.Address := To_Address (Float_Access (X));

      R : Float_Rep;
      pragma Import (Ada, R);
      for R'Address use XA;
      --  R is a view of the input floating-point parameter. Note that we
      --  must avoid copying the actual bits of this parameter in float
      --  form (since it may be a signalling NaN).

      E  : constant IEEE_Exponent_Range :=
             Integer ((R (Most_Significant_Word) and Exponent_Mask) /
                                                        Exponent_Factor)
               - IEEE_Bias;
      --  Mask/Shift T to only get bits from the exponent. Then convert biased
      --  value to integer value.

      SR : Float_Rep;
      --  Float_Rep representation of significant of X.all

   begin
      if T'Denorm then

         --  All denormalized numbers are valid, so the only invalid numbers
         --  are overflows and NaNs, both with exponent = Emax + 1.

         return E /= IEEE_Emax + 1;

      end if;

      --  All denormalized numbers except 0.0 are invalid

      --  Set exponent of X to zero, so we end up with the significand, which
      --  definitely is a valid number and can be converted back to a float.

      SR := R;
      SR (Most_Significant_Word) :=
           (SR (Most_Significant_Word)
             and not Exponent_Mask) + Float_Word (IEEE_Bias) * Exponent_Factor;

      return (E in IEEE_Emin .. IEEE_Emax) or else
         ((E = IEEE_Emin - 1) and then abs To_Float (SR) = 1.0);
   end Valid;

end System.Fat_Gen;
