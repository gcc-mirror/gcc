------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F A T _ G E N                        --
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

--  This implementation is portable to any IEEE implementation. It does not
--  handle nonbinary radix, and also assumes that model numbers and machine
--  numbers are basically identical, which is not true of all possible
--  floating-point implementations.

with Ada.Unchecked_Conversion;
with System.Unsigned_Types;

pragma Warnings (Off, "non-static constant in preelaborated unit");
--  Every constant is static given our instantiation model

package body System.Fat_Gen is
   pragma Assert (T'Machine_Radix = 2);
   --  This version does not handle radix 16

   Rad : constant T := T (T'Machine_Radix);
   --  Renaming for the machine radix

   Mantissa : constant Integer := T'Machine_Mantissa;
   --  Renaming for the machine mantissa

   Invrad : constant T := 1.0 / Rad;
   --  Smallest positive mantissa in the canonical form (RM A.5.3(4))

   --  Small : constant T := Rad ** (T'Machine_Emin - 1);
   --  Smallest positive normalized number

   --  Tiny : constant T := Rad ** (T'Machine_Emin - Mantissa);
   --  Smallest positive denormalized number

   RM1 : constant T := Rad ** (Mantissa - 1);
   --  Smallest positive member of the large consecutive integers. It is equal
   --  to the ratio Small / Tiny, which means that multiplying by it normalizes
   --  any nonzero denormalized number.

   IEEE_Emin : constant Integer := T'Machine_Emin - 1;
   IEEE_Emax : constant Integer := T'Machine_Emax - 1;
   --  The mantissa is a fraction with first digit set in Ada whereas it is
   --  shifted by 1 digit to the left in the IEEE floating-point format.

   subtype IEEE_Erange is Integer range IEEE_Emin - 1 .. IEEE_Emax + 1;
   --  The IEEE floating-point format extends the machine range by 1 to the
   --  left for denormalized numbers and 1 to the right for infinities/NaNs.

   IEEE_Ebias : constant Integer := -(IEEE_Emin - 1);
   --  The exponent is biased such that denormalized numbers have it zero

   --  The implementation uses a representation type Float_Rep that allows
   --  direct access to exponent and mantissa of the floating point number.

   --  The Float_Rep type is a simple array of Float_Word elements. This
   --  representation is chosen to make it possible to size the type based
   --  on a generic parameter. Since the array size is known at compile
   --  time, efficient code can still be generated. The size of Float_Word
   --  elements should be large enough to allow accessing the exponent in
   --  one read, but small enough so that all floating-point object sizes
   --  are a multiple of Float_Word'Size.

   --  The following conditions must be met for all possible instantiations
   --  of the attribute package:

   --    - T'Size is an integral multiple of Float_Word'Size

   --    - The exponent and sign are completely contained in a single
   --      component of Float_Rep, named Most Significant Word (MSW).

   --    - The sign occupies the most significant bit of the MSW and the
   --      exponent is in the following bits.

   --  The low-level primitives Copy_Sign, Decompose, Finite_Succ, Scaling and
   --  Valid are implemented by accessing the bit pattern of the floating-point
   --  number. Only the normalization of denormalized numbers, if any, and the
   --  gradual underflow are left to the hardware, mainly because there is some
   --  leeway for the hardware implementation in this area: for example the MSB
   --  of the mantissa, that is 1 for normalized numbers and 0 for denormalized
   --  numbers, may or may not be stored by the hardware.

   Siz : constant := 16;
   type Float_Word is mod 2**Siz;
   --  We use the GCD of the size of all the supported floating-point formats

   N  : constant Natural := (T'Size + Siz - 1) / Siz;
   NR : constant Natural := (Mantissa + 16 + Siz - 1) / Siz;
   Rep_Last : constant Natural := Natural'Min (N, NR) - 1;
   --  Determine the number of Float_Words needed for representing the
   --  entire floating-point value. Do not take into account excessive
   --  padding, as occurs on IA-64 where 80 bits floats get padded to 128
   --  bits. In general, the exponent field cannot be larger than 15 bits,
   --  even for 128-bit floating-point types, so the final format size
   --  won't be larger than Mantissa + 16.

   type Float_Rep is array (Natural range 0 .. N - 1) of Float_Word;
   pragma Suppress_Initialization (Float_Rep);
   --  This pragma suppresses the generation of an initialization procedure
   --  for type Float_Rep when operating in Initialize/Normalize_Scalars mode.

   MSW : constant Natural := Rep_Last * Standard'Default_Bit_Order;
   --  Finding the location of the Exponent_Word is a bit tricky. In general
   --  we assume Word_Order = Bit_Order.

   Exp_Factor : constant Float_Word :=
                  2**(Siz - 1) / Float_Word (IEEE_Emax - IEEE_Emin + 3);
   --  Factor that the extracted exponent needs to be divided by to be in
   --  range 0 .. IEEE_Emax - IEEE_Emin + 2

   Exp_Mask : constant Float_Word :=
                Float_Word (IEEE_Emax - IEEE_Emin + 2) * Exp_Factor;
   --  Value needed to mask out the exponent field. This assumes that the
   --  range 0 .. IEEE_Emax - IEEE_Emin + 2 contains 2**N values, for some
   --  N in Natural.

   Sign_Mask : constant Float_Word := 2**(Siz - 1);
   --  Value needed to mask out the sign field

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Decompose (XX : T; Frac : out T; Expo : out UI);
   --  Decomposes a floating-point number into fraction and exponent parts.
   --  Both results are signed, with Frac having the sign of XX, and UI has
   --  the sign of the exponent. The absolute value of Frac is in the range
   --  0.0 <= Frac < 1.0. If Frac = 0.0 or -0.0, then Expo is always zero.

   function Finite_Succ (X : T) return T;
   --  Return the successor of X, a finite number not equal to T'Last

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
   begin
      Decompose (Fraction, Arg_Frac, Arg_Exp);
      return Scaling (Arg_Frac, Exponent);
   end Compose;

   ---------------
   -- Copy_Sign --
   ---------------

   function Copy_Sign (Value, Sign : T) return T is
      S : constant T := T'Machine (Sign);

      Rep_S : Float_Rep;
      for Rep_S'Address use S'Address;
      --  Rep_S is a view of the Sign parameter

      V : T := T'Machine (Value);

      Rep_V : Float_Rep;
      for Rep_V'Address use V'Address;
      --  Rep_V is a view of the Value parameter

   begin
      Rep_V (MSW) :=
        (Rep_V (MSW) and not Sign_Mask) or (Rep_S (MSW) and Sign_Mask);
      return V;
   end Copy_Sign;

   ---------------
   -- Decompose --
   ---------------

   procedure Decompose (XX : T; Frac : out T; Expo : out UI) is
      X : T := T'Machine (XX);

      Rep : Float_Rep;
      for Rep'Address use X'Address;
      --  Rep is a view of the input floating-point parameter

      Exp : constant IEEE_Erange :=
              Integer ((Rep (MSW) and Exp_Mask) / Exp_Factor) - IEEE_Ebias;
      --  Mask/Shift X to only get bits from the exponent. Then convert biased
      --  value to final value.

      Minus : constant Boolean := (Rep (MSW) and Sign_Mask) /= 0;
      --  Mask/Shift X to only get bit from the sign

   begin
      --  The normalized exponent of zero is zero, see RM A.5.3(15)

      if X = 0.0 then
         Expo := 0;
         Frac := X;

      --  Check for infinities and NaNs

      elsif Exp = IEEE_Emax + 1 then
         Expo := T'Machine_Emax + 1;
         Frac := (if Minus then -Invrad else Invrad);

      --  Check for nonzero denormalized numbers

      elsif Exp = IEEE_Emin - 1 then
         --  Normalize by multiplying by Radix ** (Mantissa - 1)

         Decompose (X * RM1, Frac, Expo);
         Expo := Expo - (Mantissa - 1);

      --  Case of normalized numbers

      else
         --  The Ada exponent is the IEEE exponent plus 1, see above

         Expo := Exp + 1;

         --  Set Ada exponent of X to zero, so we end up with the fraction

         Rep (MSW) := (Rep (MSW) and not Exp_Mask) +
                        Float_Word (IEEE_Ebias - 1) * Exp_Factor;
         Frac := X;
      end if;
   end Decompose;

   --------------
   -- Exponent --
   --------------

   function Exponent (X : T) return UI is
      X_Frac : T;
      X_Exp  : UI;
   begin
      Decompose (X, X_Frac, X_Exp);
      return X_Exp;
   end Exponent;

   -----------------
   -- Finite_Succ --
   -----------------

   function Finite_Succ (X : T) return T is
      XX : T := T'Machine (X);

      Rep : Float_Rep;
      for Rep'Address use XX'Address;
      --  Rep is a view of the input floating-point parameter

   begin
      --  If the floating-point type does not support denormalized numbers,
      --  there is a couple of problematic values, namely -Small and Zero,
      --  because the increment is equal to Small in these cases.

      if not T'Denorm then
         declare
            Small : constant T := Rad ** (T'Machine_Emin - 1);
            --  Smallest positive normalized number declared here and not at
            --  library level for the sake of the CCG compiler, which cannot
            --  currently compile the constant because the target is C90.

         begin
            if X = -Small then
               XX := 0.0;
               return -XX;
            elsif X = 0.0 then
               return Small;
            end if;
         end;
      end if;

      --  In all the other cases, the increment is equal to 1 in the binary
      --  integer representation of the number if X is nonnegative and equal
      --  to -1 if X is negative.

      if XX >= 0.0 then
         --  First clear the sign of negative Zero

         Rep (MSW) := Rep (MSW) and not Sign_Mask;

         --  Deal with big endian

         if MSW = 0 then
            for J in reverse 0 .. Rep_Last loop
               Rep (J) := Rep (J) + 1;

               --  For 80-bit IEEE Extended, the MSB of the mantissa is stored
               --  so, when it has been flipped, its status must be reanalyzed.

               if Mantissa = 64 and then J = 1 then

                  --  If the MSB changed from denormalized to normalized, then
                  --  keep it normalized since the exponent will be bumped.

                  if Rep (J) = 2**(Siz - 1) then
                     null;

                  --  If the MSB changed from normalized, restore it since we
                  --  cannot denormalize in this context.

                  elsif Rep (J) = 0 then
                     Rep (J) := 2**(Siz - 1);

                  else
                     exit;
                  end if;

               --  In other cases, stop if there is no carry

               else
                  exit when Rep (J) > 0;
               end if;
            end loop;

         --  Deal with little endian

         else
            for J in 0 .. Rep_Last loop
               Rep (J) := Rep (J) + 1;

               --  For 80-bit IEEE Extended, the MSB of the mantissa is stored
               --  so, when it has been flipped, its status must be reanalyzed.

               if Mantissa = 64 and then J = Rep_Last - 1 then

                  --  If the MSB changed from denormalized to normalized, then
                  --  keep it normalized since the exponent will be bumped.

                  if Rep (J) = 2**(Siz - 1) then
                     null;

                  --  If the MSB changed from normalized, restore it since we
                  --  cannot denormalize in this context.

                  elsif Rep (J) = 0 then
                     Rep (J) := 2**(Siz - 1);

                  else
                     exit;
                  end if;

               --  In other cases, stop if there is no carry

               else
                  exit when Rep (J) > 0;
               end if;
            end loop;
         end if;

      else
         if MSW = 0 then
            for J in reverse 0 .. Rep_Last loop
               Rep (J) := Rep (J) - 1;

               --  For 80-bit IEEE Extended, the MSB of the mantissa is stored
               --  so, when it has been flipped, its status must be reanalyzed.

               if Mantissa = 64 and then J = 1 then

                  --  If the MSB changed from normalized to denormalized, then
                  --  keep it normalized if the exponent is not 1.

                  if Rep (J) = 2**(Siz - 1) - 1 then
                     if Rep (0) /= 2**(Siz - 1) + 1 then
                        Rep (J) := 2**Siz - 1;
                     end if;

                  else
                     exit;
                  end if;

               --  In other cases, stop if there is no borrow

               else
                  exit when Rep (J) < 2**Siz - 1;
               end if;
            end loop;

         else
            for J in 0 .. Rep_Last loop
               Rep (J) := Rep (J) - 1;

               --  For 80-bit IEEE Extended, the MSB of the mantissa is stored
               --  so, when it has been flipped, its status must be reanalyzed.

               if Mantissa = 64 and then J = Rep_Last - 1 then

                  --  If the MSB changed from normalized to denormalized, then
                  --  keep it normalized if the exponent is not 1.

                  if Rep (J) = 2**(Siz - 1) - 1 then
                     if Rep (Rep_Last) /= 2**(Siz - 1) + 1 then
                        Rep (J) := 2**Siz - 1;
                     end if;

                  else
                     exit;
                  end if;

               --  In other cases, stop if there is no borrow

               else
                  exit when Rep (J) < 2**Siz - 1;
               end if;
            end loop;
         end if;
      end if;

      return XX;
   end Finite_Succ;

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
   begin
      Decompose (X, X_Frac, X_Exp);
      return X_Frac;
   end Fraction;

   ------------------
   -- Leading_Part --
   ------------------

   function Leading_Part (X : T; Radix_Digits : UI) return T is
      L    : UI;
      Y, Z : T;

   begin
      if Radix_Digits >= Mantissa then
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
   begin
      --  Special treatment for largest negative number: raise Constraint_Error

      if X = T'First then
         raise Constraint_Error with "Pred of largest negative number";

      --  For finite numbers, use the symmetry around zero of floating point

      elsif X > T'First and then X <= T'Last then
         pragma Annotate (CodePeer, Intentional, "test always true",
                          "Check for invalid float");
         pragma Annotate (CodePeer, Intentional, "condition predetermined",
                          "Check for invalid float");
         return -Finite_Succ (-X);

      --  For infinities and NaNs, return unchanged

      else
         return X;
         pragma Annotate (CodePeer, Intentional, "dead code",
                          "Check float range.");
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

   function Scaling (X : T; Adjustment : UI) return T is
      pragma Assert (Mantissa <= 64);
      --  This implementation handles only 80-bit IEEE Extended or smaller

      package UST renames System.Unsigned_Types;
      use type UST.Long_Long_Unsigned;

      XX : T := T'Machine (X);

      Rep : Float_Rep;
      for Rep'Address use XX'Address;
      --  Rep is a view of the input floating-point parameter

      Exp : constant IEEE_Erange :=
              Integer ((Rep (MSW) and Exp_Mask) / Exp_Factor) - IEEE_Ebias;
      --  Mask/Shift X to only get bits from the exponent. Then convert biased
      --  value to final value.

      Minus : constant Boolean := (Rep (MSW) and Sign_Mask) /= 0;
      --  Mask/Shift X to only get bit from the sign

      Expi, Expf : IEEE_Erange;

   begin
      --  Check for zero, infinities, NaNs as well as no adjustment

      if X = 0.0 or else Exp = IEEE_Emax + 1 or else Adjustment = 0 then
         return X;

      --  Check for nonzero denormalized numbers

      elsif Exp = IEEE_Emin - 1 then
         --  Check for zero result to protect the subtraction below

         if Adjustment < -(Mantissa - 1) then
            XX := 0.0;
            return (if Minus then -XX else XX);

         --  Normalize by multiplying by Radix ** (Mantissa - 1)

         else
            return Scaling (XX * RM1, Adjustment - (Mantissa - 1));
         end if;

      --  Case of normalized numbers

      else
         --  Check for overflow

         if Adjustment > IEEE_Emax - Exp then
            --  Optionally raise Constraint_Error as per RM A.5.3(29)

            if T'Machine_Overflows then
               raise Constraint_Error with "Too large exponent";

            else
               XX := 0.0;
               return (if Minus then -1.0 / XX else 1.0 / XX);
               pragma Annotate (CodePeer, Intentional, "overflow check",
                                "Infinity produced");
               pragma Annotate (CodePeer, Intentional, "divide by zero",
                                "Infinity produced");
            end if;

         --  Check for underflow

         elsif Adjustment < IEEE_Emin - Exp then
            --  Check for possibly gradual underflow (up to the hardware)

            if Adjustment >= IEEE_Emin - Mantissa - Exp then
               Expf := IEEE_Emin;
               Expi := Exp + Adjustment - Expf;

            --  Case of zero result

            else
               XX := 0.0;
               return (if Minus then -XX else XX);
            end if;

         --  Case of normalized results

         else
            Expf := Exp + Adjustment;
            Expi := 0;
         end if;

         Rep (MSW) := (Rep (MSW) and not Exp_Mask) +
                        Float_Word (IEEE_Ebias + Expf) * Exp_Factor;

         if Expi < 0 then
            --  Given that Expi >= -Mantissa, only -64 is problematic

            if Expi = -64 then
               pragma Annotate
                 (CodePeer, Intentional, "test always false",
                  "test always false in some instantiations");
               XX := XX / 2.0;
               Expi := -63;
            end if;

            XX := XX / T (UST.Long_Long_Unsigned (2) ** (-Expi));
         end if;

         return XX;
      end if;
   end Scaling;

   ----------
   -- Succ --
   ----------

   function Succ (X : T) return T is
   begin
      --  Special treatment for largest positive number: raise Constraint_Error

      if X = T'Last then
         raise Constraint_Error with "Succ of largest positive number";

      --  For finite numbers, call the specific routine

      elsif X >= T'First and then X < T'Last then
         pragma Annotate (CodePeer, Intentional, "test always true",
                          "Check for invalid float");
         pragma Annotate (CodePeer, Intentional, "condition predetermined",
                          "Check for invalid float");
         return Finite_Succ (X);

      --  For infinities and NaNs, return unchanged

      else
         return X;
         pragma Annotate (CodePeer, Intentional, "dead code",
                          "Check float range.");
      end if;
   end Succ;

   ----------------
   -- Truncation --
   ----------------

   --  The basic approach is to compute

   --    T'Machine (RM1 + N) - RM1

   --  where N >= 0.0 and RM1 = Radix ** (Mantissa - 1)

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

      if Result >= RM1 then
         return T'Machine (X);

      else
         Result := T'Machine (RM1 + Result) - RM1;

         if Result > abs X then
            Result := Result - 1.0;
         end if;

         if X > 0.0 then
            return Result;

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
      type Access_T is access all T;
      function To_Address is
        new Ada.Unchecked_Conversion (Access_T, System.Address);

      Rep : Float_Rep;
      for Rep'Address use To_Address (Access_T (X));
      --  Rep is a view of the input floating-point parameter. Note that we
      --  must avoid reading the actual bits of this parameter in float form
      --  since it may be a signalling NaN.

      Exp : constant IEEE_Erange :=
              Integer ((Rep (MSW) and Exp_Mask) / Exp_Factor) - IEEE_Ebias;
      --  Mask/Shift X to only get bits from the exponent. Then convert biased
      --  value to final value.

   begin
      if Exp = IEEE_Emax + 1 then
         --  This is an infinity or a NaN, i.e. always invalid

         return False;

      elsif Exp in IEEE_Emin .. IEEE_Emax then
         --  This is a normalized number, i.e. always valid

         return True;

      else pragma Assert (Exp = IEEE_Emin - 1);
         --  This is a denormalized number, valid if T'Denorm is True or 0.0

         if T'Denorm then
            return True;

         --  Note that we cannot do a direct comparison with 0.0 because the
         --  hardware may evaluate it to True for all denormalized numbers.

         else
            --  First clear the sign bit (the exponent is already zero)

            Rep (MSW) := Rep (MSW) and not Sign_Mask;

            return (for all J in 0 .. Rep_Last => Rep (J) = 0);
         end if;
      end if;
   end Valid;

end System.Fat_Gen;
