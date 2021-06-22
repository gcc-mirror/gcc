------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.BIG_NUMBERS.BIG_REALS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019-2021, Free Software Foundation, Inc.      --
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

with System.Unsigned_Types; use System.Unsigned_Types;

package body Ada.Numerics.Big_Numbers.Big_Reals is

   use Big_Integers;

   procedure Normalize (Arg : in out Big_Real);
   --  Normalize Arg by ensuring that Arg.Den is always positive and that
   --  Arg.Num and Arg.Den always have a GCD of 1.

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Arg : Big_Real) return Boolean is
     (Is_Valid (Arg.Num) and Is_Valid (Arg.Den));

   ---------
   -- "/" --
   ---------

   function "/" (Num, Den : Valid_Big_Integer) return Valid_Big_Real is
      Result : Big_Real;
   begin
      if Den = To_Big_Integer (0) then
         raise Constraint_Error with "divide by zero";
      end if;

      Result.Num := Num;
      Result.Den := Den;
      Normalize (Result);
      return Result;
   end "/";

   ---------------
   -- Numerator --
   ---------------

   function Numerator (Arg : Valid_Big_Real) return Valid_Big_Integer is
     (Arg.Num);

   -----------------
   -- Denominator --
   -----------------

   function Denominator (Arg : Valid_Big_Real) return Big_Positive is
     (Arg.Den);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Valid_Big_Real) return Boolean is
     (L.Num = R.Num and then L.Den = R.Den);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Valid_Big_Real) return Boolean is
     (L.Num * R.Den < R.Num * L.Den);
   --  The denominator is guaranteed to be positive since Normalized is
   --  always called when constructing a Valid_Big_Real

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Valid_Big_Real) return Boolean is (not (R < L));

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Valid_Big_Real) return Boolean is (R < L);

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Valid_Big_Real) return Boolean is (not (L < R));

   -----------------------
   -- Float_Conversions --
   -----------------------

   package body Float_Conversions is

      package Conv is new
        Big_Integers.Unsigned_Conversions (Long_Long_Unsigned);

      -----------------
      -- To_Big_Real --
      -----------------

      --  We get the fractional representation of the floating-point number by
      --  multiplying Num'Fraction by 2.0**M, with M the size of the mantissa,
      --  which gives zero or a number in the range [2.0**(M-1)..2.0**M), which
      --  means that it is an integer N of M bits. The floating-point number is
      --  thus equal to N / 2**(M-E) where E is its Num'Exponent.

      function To_Big_Real (Arg : Num) return Valid_Big_Real is

         A : constant Num'Base := abs (Arg);
         E : constant Integer  := Num'Exponent (A);
         F : constant Num'Base := Num'Fraction (A);
         M : constant Natural  := Num'Machine_Mantissa;

         N, D : Big_Integer;

      begin
         pragma Assert (Num'Machine_Radix = 2);
         --  This implementation does not handle radix 16

         pragma Assert (M <= 64);
         --  This implementation handles only 80-bit IEEE Extended or smaller

         N := Conv.To_Big_Integer (Long_Long_Unsigned (F * 2.0**M));

         --  If E is smaller than M, the denominator is 2**(M-E)

         if E < M then
            D := To_Big_Integer (2) ** (M - E);

         --  Or else, if E is larger than M, multiply the numerator by 2**(E-M)

         elsif E > M then
            N := N * To_Big_Integer (2) ** (E - M);
            D := To_Big_Integer (1);

         --  Otherwise E is equal to M and the result is just N

         else
            D := To_Big_Integer (1);
         end if;

         return (if Arg >= 0.0 then N / D else -N / D);
      end To_Big_Real;

      -------------------
      -- From_Big_Real --
      -------------------

      --  We get the (Frac, Exp) representation of the real number by finding
      --  the exponent E such that it lies in the range [2.0**(E-1)..2.0**E),
      --  multiplying the number by 2.0**(M-E) with M the size of the mantissa,
      --  and converting the result to integer N in the range [2**(M-1)..2**M)
      --  with rounding to nearest, ties to even, and finally call Num'Compose.
      --  This does not apply to the zero, for which we return 0.0 early.

      function From_Big_Real (Arg : Big_Real) return Num is

         M    : constant Natural     := Num'Machine_Mantissa;
         One  : constant Big_Real    := To_Real (1);
         Two  : constant Big_Real    := To_Real (2);
         Half : constant Big_Real    := One / Two;
         TwoI : constant Big_Integer := To_Big_Integer (2);

         function Log2_Estimate (V : Big_Real) return Natural;
         --  Return an integer not larger than Log2 (V) for V >= 1.0

         function Minus_Log2_Estimate (V : Big_Real) return Natural;
         --  Return an integer not larger than -Log2 (V) for V < 1.0

         -------------------
         -- Log2_Estimate --
         -------------------

         function Log2_Estimate (V : Big_Real) return Natural is
            Log : Natural  := 1;
            Pow : Big_Real := Two;

         begin
            while V >= Pow loop
               Pow := Pow * Pow;
               Log := Log + Log;
            end loop;

            return Log / 2;
         end Log2_Estimate;

         -------------------------
         -- Minus_Log2_Estimate --
         -------------------------

         function Minus_Log2_Estimate (V : Big_Real) return Natural is
            Log : Natural  := 1;
            Pow : Big_Real := Half;

         begin
            while V <= Pow loop
               Pow := Pow * Pow;
               Log := Log + Log;
            end loop;

            return Log / 2;
         end Minus_Log2_Estimate;

         --  Local variables

         V : Big_Real := abs (Arg);
         E : Integer  := 0;
         L : Integer;

         A, B, Q, X : Big_Integer;
         N          : Long_Long_Unsigned;
         R          : Num'Base;

      begin
         pragma Assert (Num'Machine_Radix = 2);
         --  This implementation does not handle radix 16

         pragma Assert (M <= 64);
         --  This implementation handles only 80-bit IEEE Extended or smaller

         --  Protect from degenerate case

         if Numerator (V) = To_Big_Integer (0) then
            return 0.0;
         end if;

         --  Use a binary search to compute exponent E

         while V < Half loop
            L := Minus_Log2_Estimate (V);
            V := V * (Two ** L);
            E := E - L;
         end loop;

         --  The dissymetry with above is expected since we go below 2

         while V >= One loop
            L := Log2_Estimate (V) + 1;
            V := V / (Two ** L);
            E := E + L;
         end loop;

         --  The multiplication by 2.0**(-E) has already been done in the loops

         V := V * To_Big_Real (TwoI ** M);

         --  Now go into the integer domain and divide

         A := Numerator (V);
         B := Denominator (V);

         Q := A / B;
         N := Conv.From_Big_Integer (Q);

         --  Round to nearest, ties to even, by comparing twice the remainder

         X := (A - Q * B) * TwoI;

         if X > B or else (X = B and then (N mod 2) = 1) then
            N := N + 1;

            --  If the adjusted quotient overflows the mantissa, scale up

            if N = 2**M then
               N := 1;
               E := E + 1;
            end if;
         end if;

         R := Num'Compose (Num'Base (N), E);

         return (if Numerator (Arg) >= To_Big_Integer (0) then R else -R);
      end From_Big_Real;

   end Float_Conversions;

   -----------------------
   -- Fixed_Conversions --
   -----------------------

   package body Fixed_Conversions is

      package Float_Aux is new Float_Conversions (Long_Float);

      subtype LLLI is Long_Long_Long_Integer;
      subtype LLLU is Long_Long_Long_Unsigned;

      Too_Large : constant Boolean :=
                    Num'Small_Numerator > LLLU'Last
                      or else Num'Small_Denominator > LLLU'Last;
      --  True if the Small is too large for Long_Long_Long_Unsigned, in which
      --  case we convert to/from Long_Float as an intermediate step.

      package Conv_I is new Big_Integers.Signed_Conversions (LLLI);
      package Conv_U is new Big_Integers.Unsigned_Conversions (LLLU);

      -----------------
      -- To_Big_Real --
      -----------------

      --  We just compute V * N / D where V is the mantissa value of the fixed
      --  point number, and N resp. D is the numerator resp. the denominator of
      --  the Small of the fixed-point type.

      function To_Big_Real (Arg : Num) return Valid_Big_Real is
         N, D, V : Big_Integer;

      begin
         if Too_Large then
            return Float_Aux.To_Big_Real (Long_Float (Arg));
         end if;

         N := Conv_U.To_Big_Integer (Num'Small_Numerator);
         D := Conv_U.To_Big_Integer (Num'Small_Denominator);
         V := Conv_I.To_Big_Integer (LLLI'Integer_Value (Arg));

         return V * N / D;
      end To_Big_Real;

      -------------------
      -- From_Big_Real --
      -------------------

      --  We first compute A / B = Arg * D / N where N resp. D is the numerator
      --  resp. the denominator of the Small of the fixed-point type. Then we
      --  divide A by B and convert the result to the mantissa value.

      function From_Big_Real (Arg : Big_Real) return Num is
         N, D, A, B, Q, X : Big_Integer;

      begin
         if Too_Large then
            return Num (Float_Aux.From_Big_Real (Arg));
         end if;

         N := Conv_U.To_Big_Integer (Num'Small_Numerator);
         D := Conv_U.To_Big_Integer (Num'Small_Denominator);
         A := Numerator (Arg) * D;
         B := Denominator (Arg) * N;

         Q := A / B;

         --  Round to nearest, ties to away, by comparing twice the remainder

         X := (A - Q * B) * To_Big_Integer (2);

         if X >= B then
            Q := Q + To_Big_Integer (1);

         elsif X <= -B then
            Q := Q - To_Big_Integer (1);
         end if;

         return Num'Fixed_Value (Conv_I.From_Big_Integer (Q));
      end From_Big_Real;

   end Fixed_Conversions;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Arg  : Valid_Big_Real;
      Fore : Field := 2;
      Aft  : Field := 3;
      Exp  : Field := 0) return String
   is
      Zero : constant Big_Integer := To_Big_Integer (0);
      Ten  : constant Big_Integer := To_Big_Integer (10);

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String;
      --  Return padding of Char concatenated with Str so that the resulting
      --  string is at least Min_Length long.

      function Trailing_Padding
        (Str    : String;
         Length : Field;
         Char   : Character := '0') return String;
      --  Return Str with trailing Char removed, and if needed either
      --  truncated or concatenated with padding of Char so that the resulting
      --  string is Length long.

      function Image (N : Natural) return String;
      --  Return image of N, with no leading space.

      function Numerator_Image
        (Num   : Big_Integer;
         After : Natural) return String;
      --  Return image of Num as a float value with After digits after the "."
      --  and taking Fore, Aft, Exp into account.

      -----------
      -- Image --
      -----------

      function Image (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (2 .. S'Last);
      end Image;

      ---------------------
      -- Leading_Padding --
      ---------------------

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String is
      begin
         if Str = "" then
            return Leading_Padding ("0", Min_Length, Char);
         else
            return (1 .. Integer'Max (Integer (Min_Length) - Str'Length, 0)
                           => Char) & Str;
         end if;
      end Leading_Padding;

      ----------------------
      -- Trailing_Padding --
      ----------------------

      function Trailing_Padding
        (Str    : String;
         Length : Field;
         Char   : Character := '0') return String is
      begin
         if Str'Length > 0 and then Str (Str'Last) = Char then
            for J in reverse Str'Range loop
               if Str (J) /= '0' then
                  return Trailing_Padding
                    (Str (Str'First .. J), Length, Char);
               end if;
            end loop;
         end if;

         if Str'Length >= Length then
            return Str (Str'First .. Str'First + Length - 1);
         else
            return Str &
              (1 .. Integer'Max (Integer (Length) - Str'Length, 0)
                      => Char);
         end if;
      end Trailing_Padding;

      ---------------------
      -- Numerator_Image --
      ---------------------

      function Numerator_Image
        (Num   : Big_Integer;
         After : Natural) return String
      is
         Tmp   : constant String := To_String (Num);
         Str   : constant String (1 .. Tmp'Last - 1) := Tmp (2 .. Tmp'Last);
         Index : Integer;

      begin
         if After = 0 then
            return Leading_Padding (Str, Fore) & "."
                   & Trailing_Padding ("0", Aft);
         else
            Index := Str'Last - After;

            if Index < 0 then
               return Leading_Padding ("0", Fore)
                 & "."
                 & Trailing_Padding ((1 .. -Index => '0') & Str, Aft)
                 & (if Exp = 0 then "" else "E+" & Image (Natural (Exp)));
            else
               return Leading_Padding (Str (Str'First .. Index), Fore)
                 & "."
                 & Trailing_Padding (Str (Index + 1 .. Str'Last), Aft)
                 & (if Exp = 0 then "" else "E+" & Image (Natural (Exp)));
            end if;
         end if;
      end Numerator_Image;

   begin
      if Arg.Num < Zero then
         declare
            Str : String := To_String (-Arg, Fore, Aft, Exp);
         begin
            if Str (1) = ' ' then
               for J in 1 .. Str'Last - 1 loop
                  if Str (J + 1) /= ' ' then
                     Str (J) := '-';
                     exit;
                  end if;
               end loop;

               return Str;
            else
               return '-' & Str;
            end if;
         end;
      else
         --  Compute Num * 10^Aft so that we get Aft significant digits
         --  in the integer part (rounded) to display.

         return Numerator_Image
           ((Arg.Num * Ten ** Aft) / Arg.Den, After => Exp + Aft);
      end if;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Valid_Big_Real is
      Ten   : constant Big_Integer := To_Big_Integer (10);
      Frac  : Big_Integer;
      Exp   : Integer := 0;
      Pow   : Natural := 0;
      Index : Natural := 0;
      Last  : Natural := Arg'Last;

   begin
      for J in reverse Arg'Range loop
         if Arg (J) in 'e' | 'E' then
            if Last /= Arg'Last then
               raise Constraint_Error with "multiple exponents specified";
            end if;

            Last := J - 1;
            Exp := Integer'Value (Arg (J + 1 .. Arg'Last));
            Pow := 0;

         elsif Arg (J) = '.' then
            Index := J - 1;
            exit;
         elsif Arg (J) /= '_' then
            Pow := Pow + 1;
         end if;
      end loop;

      if Index = 0 then
         raise Constraint_Error with "invalid real value";
      end if;

      declare
         Result : Big_Real;
      begin
         Result.Den := Ten ** Pow;
         Result.Num := From_String (Arg (Arg'First .. Index)) * Result.Den;
         Frac := From_String (Arg (Index + 2 .. Last));

         if Result.Num < To_Big_Integer (0) then
            Result.Num := Result.Num - Frac;
         else
            Result.Num := Result.Num + Frac;
         end if;

         if Exp > 0 then
            Result.Num := Result.Num * Ten ** Exp;
         elsif Exp < 0 then
            Result.Den := Result.Den * Ten ** (-Exp);
         end if;

         Normalize (Result);
         return Result;
      end;
   end From_String;

   --------------------------
   -- From_Quotient_String --
   --------------------------

   function From_Quotient_String (Arg : String) return Valid_Big_Real is
      Index : Natural := 0;
   begin
      for J in Arg'First + 1 .. Arg'Last - 1 loop
         if Arg (J) = '/' then
            Index := J;
            exit;
         end if;
      end loop;

      if Index = 0 then
         raise Constraint_Error with "no quotient found";
      end if;

      return Big_Integers.From_String (Arg (Arg'First .. Index - 1)) /
        Big_Integers.From_String (Arg (Index + 1 .. Arg'Last));
   end From_Quotient_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image (S : in out Root_Buffer_Type'Class; V : Big_Real) is
      --  This is implemented in terms of To_String. It might be more elegant
      --  and more efficient to do it the other way around, but this is the
      --  most expedient implementation for now.
   begin
      Strings.Text_Buffers.Put_UTF_8 (S, To_String (V));
   end Put_Image;

   ---------
   -- "+" --
   ---------

   function "+" (L : Valid_Big_Real) return Valid_Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num;
      Result.Den := L.Den;
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Valid_Big_Real) return Valid_Big_Real is
     (Num => -L.Num, Den => L.Den);

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Valid_Big_Real) return Valid_Big_Real is
     (Num => abs L.Num, Den => L.Den);

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Valid_Big_Real) return Valid_Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den + R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Valid_Big_Real) return Valid_Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den - R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Valid_Big_Real) return Valid_Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Num;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Valid_Big_Real) return Valid_Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den;
      Result.Den := L.Den * R.Num;
      Normalize (Result);
      return Result;
   end "/";

   ----------
   -- "**" --
   ----------

   function "**" (L : Valid_Big_Real; R : Integer) return Valid_Big_Real is
      Result : Big_Real;
   begin
      if R = 0 then
         Result.Num := To_Big_Integer (1);
         Result.Den := To_Big_Integer (1);
      else
         if R < 0 then
            Result.Num := L.Den ** (-R);
            Result.Den := L.Num ** (-R);
         else
            Result.Num := L.Num ** R;
            Result.Den := L.Den ** R;
         end if;

         Normalize (Result);
      end if;

      return Result;
   end "**";

   ---------
   -- Min --
   ---------

   function Min (L, R : Valid_Big_Real) return Valid_Big_Real is
     (if L < R then L else R);

   ---------
   -- Max --
   ---------

   function Max (L, R : Valid_Big_Real) return Valid_Big_Real is
     (if L > R then L else R);

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Arg : in out Big_Real) is
      Zero : constant Big_Integer := To_Big_Integer (0);
   begin
      if Arg.Den < Zero then
         Arg.Num := -Arg.Num;
         Arg.Den := -Arg.Den;
      end if;

      if Arg.Num = Zero then
         Arg.Den := To_Big_Integer (1);
      else
         declare
            GCD : constant Big_Integer :=
              Greatest_Common_Divisor (Arg.Num, Arg.Den);
         begin
            Arg.Num := Arg.Num / GCD;
            Arg.Den := Arg.Den / GCD;
         end;
      end if;
   end Normalize;

end Ada.Numerics.Big_Numbers.Big_Reals;
