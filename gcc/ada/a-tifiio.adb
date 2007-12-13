------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 A D A . T E X T _ I O . F I X E D _ I O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  Fixed point I/O
--  ---------------

--  The following documents implementation details of the fixed point
--  input/output routines in the GNAT run time. The first part describes
--  general properties of fixed point types as defined by the Ada 95 standard,
--  including the Information Systems Annex.

--  Subsequently these are reduced to implementation constraints and the impact
--  of these constraints on a few possible approaches to I/O are given.
--  Based on this analysis, a specific implementation is selected for use in
--  the GNAT run time. Finally, the chosen algorithm is analyzed numerically in
--  order to provide user-level documentation on limits for range and precision
--  of fixed point types as well as accuracy of input/output conversions.

--  -------------------------------------------
--  - General Properties of Fixed Point Types -
--  -------------------------------------------

--  Operations on fixed point values, other than input and output, are not
--  important for the purposes of this document. Only the set of values that a
--  fixed point type can represent and the input and output operations are
--  significant.

--  Values
--  ------

--  Set set of values of a fixed point type comprise the integral
--  multiples of a number called the small of the type. The small can
--  either be a power of ten, a power of two or (if the implementation
--  allows) an arbitrary strictly positive real value.

--  Implementations need to support fixed-point types with a precision
--  of at least 24 bits, and (in order to comply with the Information
--  Systems Annex) decimal types need to support at least digits 18.
--  For the rest, however, no requirements exist for the minimal small
--  and range that need to be supported.

--  Operations
--  ----------

--  'Image and 'Wide_Image (see RM 3.5(34))

--          These attributes return a decimal real literal best approximating
--          the value (rounded away from zero if halfway between) with a
--          single leading character that is either a minus sign or a space,
--          one or more digits before the decimal point (with no redundant
--          leading zeros), a decimal point, and N digits after the decimal
--          point. For a subtype S, the value of N is S'Aft, the smallest
--          positive integer such that (10**N)*S'Delta is greater or equal to
--          one, see RM 3.5.10(5).

--          For an arbitrary small, this means large number arithmetic needs
--          to be performed.

--  Put (see RM A.10.9(22-26))

--          The requirements for Put add no extra constraints over the image
--          attributes, although it would be nice to be able to output more
--          than S'Aft digits after the decimal point for values of subtype S.

--  'Value and 'Wide_Value attribute (RM 3.5(40-55))

--          Since the input can be given in any base in the range 2..16,
--          accurate conversion to a fixed point number may require
--          arbitrary precision arithmetic if there is no limit on the
--          magnitude of the small of the fixed point type.

--  Get (see RM A.10.9(12-21))

--          The requirements for Get are identical to those of the Value
--          attribute.

--  ------------------------------
--  - Implementation Constraints -
--  ------------------------------

--  The requirements listed above for the input/output operations lead to
--  significant complexity, if no constraints are put on supported smalls.

--  Implementation Strategies
--  -------------------------

--  * Float arithmetic
--  * Arbitrary-precision integer arithmetic
--  * Fixed-precision integer arithmetic

--  Although it seems convenient to convert fixed point numbers to floating-
--  point and then print them, this leads to a number of restrictions.
--  The first one is precision. The widest floating-point type generally
--  available has 53 bits of mantissa. This means that Fine_Delta cannot
--  be less than 2.0**(-53).

--  In GNAT, Fine_Delta is 2.0**(-63), and Duration for example is a
--  64-bit type. It would still be possible to use multi-precision
--  floating-point to perform calculations using longer mantissas,
--  but this is a much harder approach.

--  The base conversions needed for input and output of (non-decimal)
--  fixed point types can be seen as pairs of integer multiplications
--  and divisions.

--  Arbitrary-precision integer arithmetic would be suitable for the job
--  at hand, but has the draw-back that it is very heavy implementation-wise.
--  Especially in embedded systems, where fixed point types are often used,
--  it may not be desirable to require large amounts of storage and time
--  for fixed I/O operations.

--  Fixed-precision integer arithmetic has the advantage of simplicity and
--  speed. For the most common fixed point types this would be a perfect
--  solution. The downside however may be a too limited set of acceptable
--  fixed point types.

--  Extra Precision
--  ---------------

--  Using a scaled divide which truncates and returns a remainder R,
--  another E trailing digits can be calculated by computing the value
--  (R * (10.0**E)) / Z using another scaled divide. This procedure
--  can be repeated to compute an arbitrary number of digits in linear
--  time and storage. The last scaled divide should be rounded, with
--  a possible carry propagating to the more significant digits, to
--  ensure correct rounding of the unit in the last place.

--  An extension of this technique is to limit the value of Q to 9 decimal
--  digits, since 32-bit integers can be much more efficient than 64-bit
--  integers to output.

with Interfaces;                        use Interfaces;
with System.Arith_64;                   use System.Arith_64;
with System.Img_Real;                   use System.Img_Real;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Text_IO.Float_Aux;
with Ada.Text_IO.Generic_Aux;

package body Ada.Text_IO.Fixed_IO is

   --  Note: we still use the floating-point I/O routines for input of
   --  ordinary fixed-point and output using exponent format. This will
   --  result in inaccuracies for fixed point types with a small that is
   --  not a power of two, and for types that require more precision than
   --  is available in Long_Long_Float.

   package Aux renames Ada.Text_IO.Float_Aux;

   Extra_Layout_Space : constant Field := 5 + Num'Fore;
   --  Extra space that may be needed for output of sign, decimal point,
   --  exponent indication and mandatory decimals after and before the
   --  decimal point. A string with length

   --    Fore + Aft + Exp + Extra_Layout_Space

   --  is always long enough for formatting any fixed point number

   --  Implementation of Put routines

   --  The following section describes a specific implementation choice for
   --  performing base conversions needed for output of values of a fixed
   --  point type T with small T'Small. The goal is to be able to output
   --  all values of types with a precision of 64 bits and a delta of at
   --  least 2.0**(-63), as these are current GNAT limitations already.

   --  The chosen algorithm uses fixed precision integer arithmetic for
   --  reasons of simplicity and efficiency. It is important to understand
   --  in what ways the most simple and accurate approach to fixed point I/O
   --  is limiting, before considering more complicated schemes.

   --  Without loss of generality assume T has a range (-2.0**63) * T'Small
   --  .. (2.0**63 - 1) * T'Small, and is output with Aft digits after the
   --  decimal point and T'Fore - 1 before. If T'Small is integer, or
   --  1.0 / T'Small is integer, let S = T'Small and E = 0. For other T'Small,
   --  let S and E be integers such that S / 10**E best approximates T'Small
   --  and S is in the range 10**17 .. 10**18 - 1. The extra decimal scaling
   --  factor 10**E can be trivially handled during final output, by adjusting
   --  the decimal point or exponent.

   --  Convert a value X * S of type T to a 64-bit integer value Q equal
   --  to 10.0**D * (X * S) rounded to the nearest integer.
   --  This conversion is a scaled integer divide of the form

   --     Q := (X * Y) / Z,

   --  where all variables are 64-bit signed integers using 2's complement,
   --  and both the multiplication and division are done using full
   --  intermediate precision. The final decimal value to be output is

   --     Q * 10**(E-D)

   --  This value can be written to the output file or to the result string
   --  according to the format described in RM A.3.10. The details of this
   --  operation are omitted here.

   --  A 64-bit value can contain all integers with 18 decimal digits, but
   --  not all with 19 decimal digits. If the total number of requested output
   --  digits (Fore - 1) + Aft is greater than 18, for purposes of the
   --  conversion Aft is adjusted to 18 - (Fore - 1). In that case, or
   --  when Fore > 19, trailing zeros can complete the output after writing
   --  the first 18 significant digits, or the technique described in the
   --  next section can be used.

   --  The final expression for D is

   --     D :=  Integer'Max (-18, Integer'Min (Aft, 18 - (Fore - 1)));

   --  For Y and Z the following expressions can be derived:

   --     Q / (10.0**D) = X * S

   --     Q = X * S * (10.0**D) = (X * Y) / Z

   --     S * 10.0**D = Y / Z;

   --  If S is an integer greater than or equal to one, then Fore must be at
   --  least 20 in order to print T'First, which is at most -2.0**63.
   --  This means D < 0, so use

   --    (1)   Y = -S and Z = -10**(-D)

   --  If 1.0 / S is an integer greater than one, use

   --    (2)   Y = -10**D and Z = -(1.0 / S), for D >= 0

   --  or

   --    (3)   Y = 1 and Z = (1.0 / S) * 10**(-D), for D < 0

   --  Negative values are used for nominator Y and denominator Z, so that S
   --  can have a maximum value of 2.0**63 and a minimum of 2.0**(-63).
   --  For Z in -1 .. -9, Fore will still be 20, and D will be negative, as
   --  (-2.0**63) / -9 is greater than 10**18. In these cases there is room
   --  in the denominator for the extra decimal scaling required, so case (3)
   --  will not overflow.

   pragma Assert (System.Fine_Delta >= 2.0**(-63));
   pragma Assert (Num'Small in 2.0**(-63) .. 2.0**63);
   pragma Assert (Num'Fore <= 37);
   --  These assertions need to be relaxed to allow for a Small of
   --  2.0**(-64) at least, since there is an ACATS test for this ???

   Max_Digits : constant := 18;
   --  Maximum number of decimal digits that can be represented in a
   --  64-bit signed number, see above

   --  The constants E0 .. E5 implement a binary search for the appropriate
   --  power of ten to scale the small so that it has one digit before the
   --  decimal point.

   subtype Int is Integer;
   E0 : constant Int := -(20 * Boolean'Pos (Num'Small >= 1.0E1));
   E1 : constant Int := E0 + 10 * Boolean'Pos (Num'Small * 10.0**E0 < 1.0E-10);
   E2 : constant Int := E1 +  5 * Boolean'Pos (Num'Small * 10.0**E1 < 1.0E-5);
   E3 : constant Int := E2 +  3 * Boolean'Pos (Num'Small * 10.0**E2 < 1.0E-3);
   E4 : constant Int := E3 +  2 * Boolean'Pos (Num'Small * 10.0**E3 < 1.0E-1);
   E5 : constant Int := E4 +  1 * Boolean'Pos (Num'Small * 10.0**E4 < 1.0E-0);

   Scale : constant Integer := E5;

   pragma Assert (Num'Small * 10.0**Scale >= 1.0
                   and then Num'Small * 10.0**Scale < 10.0);

   Exact : constant Boolean :=
                Float'Floor (Num'Small) = Float'Ceiling (Num'Small)
            or Float'Floor (1.0 / Num'Small) = Float'Ceiling (1.0 / Num'Small)
            or Num'Small >= 10.0**Max_Digits;
   --  True iff a numerator and denominator can be calculated such that
   --  their ratio exactly represents the small of Num

   --  Local Subprograms

   procedure Put
     (To   : out String;
      Last : out Natural;
      Item : Num;
      Fore : Field;
      Aft  : Field;
      Exp  : Field);
   --  Actual output function, used internally by all other Put routines

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      Aux.Get (File, Long_Long_Float (Item), Width);

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      Aux.Get (Current_In, Long_Long_Float (Item), Width);

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (From : String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

   begin
      Aux.Gets (From, Long_Long_Float (Item), Last);

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      S    : String (1 .. Fore + Aft + Exp + Extra_Layout_Space);
      Last : Natural;
   begin
      Put (S, Last, Item, Fore, Aft, Exp);
      Generic_Aux.Put_Item (File, S (1 .. Last));
   end Put;

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      S    : String (1 .. Fore + Aft + Exp + Extra_Layout_Space);
      Last : Natural;
   begin
      Put (S, Last, Item, Fore, Aft, Exp);
      Generic_Aux.Put_Item (Text_IO.Current_Out, S (1 .. Last));
   end Put;

   procedure Put
     (To   : out String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      Fore : constant Integer := To'Length
                                - 1                      -- Decimal point
                                - Field'Max (1, Aft)     -- Decimal part
                                - Boolean'Pos (Exp /= 0) -- Exponent indicator
                                - Exp;                   -- Exponent
      Last : Natural;

   begin
      if Fore - Boolean'Pos (Item < 0.0) < 1 or else Fore > Field'Last then
         raise Layout_Error;
      end if;

      Put (To, Last, Item, Fore, Aft, Exp);

      if Last /= To'Last then
         raise Layout_Error;
      end if;
   end Put;

   procedure Put
     (To   : out String;
      Last : out Natural;
      Item : Num;
      Fore : Field;
      Aft  : Field;
      Exp  : Field)
   is
      subtype Digit is Int64 range 0 .. 9;

      X   : constant Int64   := Int64'Integer_Value (Item);
      A   : constant Field   := Field'Max (Aft, 1);
      Neg : constant Boolean := (Item < 0.0);
      Pos : Integer := 0;  -- Next digit X has value X * 10.0**Pos;

      Y, Z : Int64;
      E : constant Integer := Boolean'Pos (not Exact)
                                *  (Max_Digits - 1 + Scale);
      D : constant Integer := Boolean'Pos (Exact)
                                * Integer'Min (A, Max_Digits - (Num'Fore - 1))
                            + Boolean'Pos (not Exact)
                                * (Scale - 1);

      procedure Put_Character (C : Character);
      pragma Inline (Put_Character);
      --  Add C to the output string To, updating Last

      procedure Put_Digit (X : Digit);
      --  Add digit X to the output string (going from left to right),
      --  updating Last and Pos, and inserting the sign, leading zeros
      --  or a decimal point when necessary. After outputting the first
      --  digit, Pos must not be changed outside Put_Digit anymore

      procedure Put_Int64 (X : Int64; Scale : Integer);
      --  Output the decimal number X * 10**Scale

      procedure Put_Scaled
        (X, Y, Z : Int64;
         A       : Field;
         E       : Integer);
      --  Output the decimal number (X * Y / Z) * 10**E, producing A digits
      --  after the decimal point and rounding the final digit. The value
      --  X * Y / Z is computed with full precision, but must be in the
      --  range of Int64.

      -------------------
      -- Put_Character --
      -------------------

      procedure Put_Character (C : Character) is
      begin
         Last := Last + 1;

         --  Never put a character outside of string To. Exception Layout_Error
         --  will be raised later if Last is greater than To'Last.

         if Last <= To'Last then
            To (Last) := C;
         end if;
      end Put_Character;

      ---------------
      -- Put_Digit --
      ---------------

      procedure Put_Digit (X : Digit) is
         Digs : constant array (Digit) of Character := "0123456789";

      begin
         if Last = To'First - 1 then
            if X /= 0 or Pos <= 0 then
               --  Before outputting first digit, include leading space,
               --  possible minus sign and, if the first digit is fractional,
               --  decimal seperator and leading zeros.

               --  The Fore part has Pos + 1 + Boolean'Pos (Neg) characters,
               --  if Pos >= 0 and otherwise has a single zero digit plus minus
               --  sign if negative. Add leading space if necessary.

               for J in Integer'Max (0, Pos) + 2 + Boolean'Pos (Neg) .. Fore
               loop
                  Put_Character (' ');
               end loop;

               --  Output minus sign, if number is negative

               if Neg then
                  Put_Character ('-');
               end if;

               --  If starting with fractional digit, output leading zeros

               if Pos < 0 then
                  Put_Character ('0');
                  Put_Character ('.');

                  for J in Pos .. -2 loop
                     Put_Character ('0');
                  end loop;
               end if;

               Put_Character (Digs (X));
            end if;

         else
            --  This is not the first digit to be output, so the only
            --  special handling is that for the decimal point

            if Pos = -1 then
               Put_Character ('.');
            end if;

            Put_Character (Digs (X));
         end if;

         Pos := Pos - 1;
      end Put_Digit;

      ---------------
      -- Put_Int64 --
      ---------------

      procedure Put_Int64 (X : Int64; Scale : Integer) is
      begin
         if X = 0 then
            return;
         end if;

         if X not in -9 .. 9 then
            Put_Int64 (X / 10, Scale + 1);
         end if;

         --  Use Put_Digit to advance Pos. This fixes a case where the second
         --  or later Scaled_Divide would omit leading zeroes, resulting in
         --  too few digits produced and a Layout_Error as result.

         while Pos > Scale loop
            Put_Digit (0);
         end loop;

         --  If Pos is less than Scale now, reset to equal Scale

         Pos := Scale;

         Put_Digit (abs (X rem 10));
      end Put_Int64;

      ----------------
      -- Put_Scaled --
      ----------------

      procedure Put_Scaled
        (X, Y, Z : Int64;
         A       : Field;
         E       : Integer)
      is
         N  : constant Natural := (A + Max_Digits - 1) / Max_Digits + 1;
         Q  : array (1 .. N) of Int64 := (others => 0);

         XX : Int64 := X;
         YY : Int64 := Y;
         AA : Field := A;

      begin
         for J in Q'Range loop
            exit when XX = 0;

            Scaled_Divide (XX, YY, Z, Q (J), XX, Round => AA = 0);

            --  As the last block of digits is rounded, a carry may have to
            --  be propagated to the more significant digits. Since the last
            --  block may have less than Max_Digits, the test for this block
            --  is specialized.

            --  The absolute value of the left-most digit block may equal
            --  10*Max_Digits, as no carry can be propagated from there.
            --  The final output routines need to be prepared to handle
            --  this specific case.

            if (Q (J) = YY or -Q (J) = YY) and then J > Q'First then
               if Q (J) < 0 then
                  Q (J - 1) := Q (J - 1) + 1;
               else
                  Q (J - 1) := Q (J - 1) - 1;
               end if;

               Q (J) := 0;

               Propagate_Carry :
               for J in reverse Q'First + 1 .. Q'Last loop
                  if Q (J) >= 10**Max_Digits then
                     Q (J - 1) := Q (J - 1) + 1;
                     Q (J) := Q (J) - 10**Max_Digits;

                  elsif Q (J) <= -10**Max_Digits then
                     Q (J - 1) := Q (J - 1) - 1;
                     Q (J) := Q (J) + 10**Max_Digits;
                  end if;
               end loop Propagate_Carry;
            end if;

            YY := -10**Integer'Min (Max_Digits, AA);
            AA := AA - Integer'Min (Max_Digits, AA);
         end loop;

         for J in Q'First .. Q'Last - 1 loop
            Put_Int64 (Q (J), E - (J - Q'First) * Max_Digits);
         end loop;

         Put_Int64 (Q (Q'Last), E - A);
      end Put_Scaled;

   --  Start of processing for Put

   begin
      Last := To'First - 1;

      if Exp /= 0 then

         --  With the Exp format, it is not known how many output digits to
         --  generate, as leading zeros must be ignored. Computing too many
         --  digits and then truncating the output will not give the closest
         --  output, it is necessary to round at the correct digit.

         --  The general approach is as follows: as long as no digits have
         --  been generated, compute the Aft next digits (without rounding).
         --  Once a non-zero digit is generated, determine the exact number
         --  of digits remaining and compute them with rounding.
         --  Since a large number of iterations might be necessary in case
         --  of Aft = 1, the following optimization would be desirable.
         --  Count the number Z of leading zero bits in the integer
         --  representation of X, and start with producing
         --  Aft + Z * 1000 / 3322 digits in the first scaled division.

         --  However, the floating-point routines are still used now ???

         System.Img_Real.Set_Image_Real (Long_Long_Float (Item), To, Last,
            Fore, Aft, Exp);
         return;
      end if;

      if Exact then
         Y := Int64'Min (Int64 (-Num'Small), -1) * 10**Integer'Max (0, D);
         Z := Int64'Min (Int64 (-(1.0 / Num'Small)), -1)
                                                 * 10**Integer'Max (0, -D);
      else
         Y := Int64 (-(Num'Small * 10.0**E));
         Z := -10**Max_Digits;
      end if;

      Put_Scaled (X, Y, Z, A - D, -D);

      --  If only zero digits encountered, unit digit has not been output yet

      if Last < To'First then
         Pos := 0;
      end if;

      --  Always output digits up to the first one after the decimal point

      while Pos >= -A loop
         Put_Digit (0);
      end loop;
   end Put;

end Ada.Text_IO.Fixed_IO;
