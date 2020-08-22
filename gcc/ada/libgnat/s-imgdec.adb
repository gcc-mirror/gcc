------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . I M G _ D E C                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

with System.Img_Int; use System.Img_Int;

package body System.Img_Dec is

   -------------------
   -- Image_Decimal --
   -------------------

   procedure Image_Decimal
     (V     : Integer;
      S     : in out String;
      P     : out Natural;
      Scale : Integer)
   is
      pragma Assert (S'First = 1);

   begin
      --  Add space at start for non-negative numbers

      if V >= 0 then
         S (1) := ' ';
         P := 1;
      else
         P := 0;
      end if;

      Set_Image_Decimal (V, S, P, Scale, 1, Integer'Max (1, Scale), 0);
   end Image_Decimal;

   ------------------------
   -- Set_Decimal_Digits --
   ------------------------

   procedure Set_Decimal_Digits
     (Digs  : in out String;
      NDigs : Natural;
      S     : out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
   is
      Minus : constant Boolean := (Digs (Digs'First) = '-');
      --  Set True if input is negative

      Zero : Boolean := (Digs (Digs'First + 1) = '0');
      --  Set True if input is exactly zero (only case when a leading zero
      --  is permitted in the input string given to this procedure). This
      --  flag can get set later if rounding causes the value to become zero.

      FD : Natural := 2;
      --  First digit position of digits remaining to be processed

      LD : Natural := NDigs;
      --  Last digit position of digits remaining to be processed

      ND : Natural := NDigs - 1;
      --  Number of digits remaining to be processed (LD - FD + 1)

      Digits_Before_Point : Integer := ND - Scale;
      --  Number of digits before decimal point in the input value. This
      --  value can be negative if the input value is less than 0.1, so
      --  it is an indication of the current exponent. Digits_Before_Point
      --  is adjusted if the rounding step generates an extra digit.

      Digits_After_Point : constant Natural := Integer'Max (1, Aft);
      --  Digit positions after decimal point in result string

      Expon : Integer;
      --  Integer value of exponent

      procedure Round (N : Integer);
      --  Round the number in Digs. N is the position of the last digit to be
      --  retained in the rounded position (rounding is based on Digs (N + 1)
      --  FD, LD, ND are reset as necessary if required. Note that if the
      --  result value rounds up (e.g. 9.99 => 10.0), an extra digit can be
      --  placed in the sign position as a result of the rounding, this is
      --  the case in which FD is adjusted. The call to Round has no effect
      --  if N is outside the range FD .. LD.

      procedure Set (C : Character);
      pragma Inline (Set);
      --  Sets character C in output buffer

      procedure Set_Blanks_And_Sign (N : Integer);
      --  Sets leading blanks and minus sign if needed. N is the number of
      --  positions to be filled (a minus sign is output even if N is zero
      --  or negative, For a positive value, if N is non-positive, then
      --  a leading blank is filled.

      procedure Set_Digits (S, E : Natural);
      pragma Inline (Set_Digits);
      --  Set digits S through E from Digs, no effect if S > E

      procedure Set_Zeroes (N : Integer);
      pragma Inline (Set_Zeroes);
      --  Set N zeroes, no effect if N is negative

      -----------
      -- Round --
      -----------

      procedure Round (N : Integer) is
         D : Character;

      begin
         --  Nothing to do if rounding past the last digit we have

         if N >= LD then
            return;

         --  Cases of rounding before the initial digit

         elsif N < FD then

            --  The result is zero, unless we are rounding just before
            --  the first digit, and the first digit is five or more.

            if N = 1 and then Digs (Digs'First + 1) >= '5' then
               Digs (Digs'First) := '1';
            else
               Digs (Digs'First) := '0';
               Zero := True;
            end if;

            Digits_Before_Point := Digits_Before_Point + 1;
            FD := 1;
            LD := 1;
            ND := 1;

         --  Normal case of rounding an existing digit

         else
            LD := N;
            ND := LD - 1;

            if Digs (N + 1) >= '5' then
               for J in reverse 2 .. N loop
                  D := Character'Succ (Digs (J));

                  if D <= '9' then
                     Digs (J) := D;
                     return;
                  else
                     Digs (J) := '0';
                  end if;
               end loop;

               --  Here the rounding overflows into the sign position. That's
               --  OK, because we already captured the value of the sign and
               --  we are in any case destroying the value in the Digs buffer

               Digs (Digs'First) := '1';
               FD := 1;
               ND := ND + 1;
               Digits_Before_Point := Digits_Before_Point + 1;
            end if;
         end if;
      end Round;

      ---------
      -- Set --
      ---------

      procedure Set (C : Character) is
      begin
         P := P + 1;
         S (P) := C;
      end Set;

      -------------------------
      -- Set_Blanks_And_Sign --
      -------------------------

      procedure Set_Blanks_And_Sign (N : Integer) is
         W : Integer := N;

      begin
         if Minus then
            W := W - 1;

            for J in 1 .. W loop
               Set (' ');
            end loop;

            Set ('-');

         else
            for J in 1 .. W loop
               Set (' ');
            end loop;
         end if;
      end Set_Blanks_And_Sign;

      ----------------
      -- Set_Digits --
      ----------------

      procedure Set_Digits (S, E : Natural) is
      begin
         for J in S .. E loop
            Set (Digs (J));
         end loop;
      end Set_Digits;

      ----------------
      -- Set_Zeroes --
      ----------------

      procedure Set_Zeroes (N : Integer) is
      begin
         for J in 1 .. N loop
            Set ('0');
         end loop;
      end Set_Zeroes;

   --  Start of processing for Set_Decimal_Digits

   begin
      --  Case of exponent given

      if Exp > 0 then
         Set_Blanks_And_Sign (Fore - 1);
         Round (Digits_After_Point + 2);
         Set (Digs (FD));
         FD := FD + 1;
         ND := ND - 1;
         Set ('.');

         if ND >= Digits_After_Point then
            Set_Digits (FD, FD + Digits_After_Point - 1);
         else
            Set_Digits (FD, LD);
            Set_Zeroes (Digits_After_Point - ND);
         end if;

         --  Calculate exponent. The number of digits before the decimal point
         --  in the input is Digits_Before_Point, and the number of digits
         --  before the decimal point in the output is 1, so we can get the
         --  exponent as the difference between these two values. The one
         --  exception is for the value zero, which by convention has an
         --  exponent of +0.

         Expon := (if Zero then 0 else Digits_Before_Point - 1);
         Set ('E');
         ND := 0;

         if Expon >= 0 then
            Set ('+');
            Set_Image_Integer (Expon, Digs, ND);
         else
            Set ('-');
            Set_Image_Integer (-Expon, Digs, ND);
         end if;

         Set_Zeroes (Exp - ND - 1);
         Set_Digits (1, ND);
         return;

      --  Case of no exponent given. To make these cases clear, we use
      --  examples. For all the examples, we assume Fore = 2, Aft = 3.
      --  A P in the example input string is an implied zero position,
      --  not included in the input string.

      else
         --  Round at correct position
         --    Input: 4PP      => unchanged
         --    Input: 400.03   => unchanged
         --    Input  3.4567   => 3.457
         --    Input: 9.9999   => 10.000
         --    Input: 0.PPP5   => 0.001
         --    Input: 0.PPP4   => 0
         --    Input: 0.00003  => 0

         Round (LD - (Scale - Digits_After_Point));

         --  No digits before point in input
         --    Input: .123   Output: 0.123
         --    Input: .PP3   Output: 0.003

         if Digits_Before_Point <= 0 then
            Set_Blanks_And_Sign (Fore - 1);
            Set ('0');
            Set ('.');

            declare
               DA : Natural := Digits_After_Point;
               --  Digits remaining to output after point

               LZ : constant Integer := Integer'Min (DA, -Digits_Before_Point);
               --  Number of leading zeroes after point. Note: there used to be
               --  a Max of this result with zero, but that's redundant, since
               --  we know DA is positive, and because of the test above, we
               --  know that -Digits_Before_Point >= 0.

            begin
               Set_Zeroes (LZ);
               DA := DA - LZ;

               if DA < ND then

                  --  Note: it is definitely possible for the above condition
                  --  to be True, for example:

                  --    V => 1234, Scale => 5, Fore => 0, After => 1, Exp => 0

                  --  but in this case DA = 0, ND = 1, FD = 1, FD + DA-1 = 0
                  --  so the arguments in the call are (1, 0) meaning that no
                  --  digits are output.

                  --  No obvious example exists where the following call to
                  --  Set_Digits actually outputs some digits, but we lack a
                  --  proof that no such example exists.

                  --  So it is safer to retain this call, even though as a
                  --  result it is hard (or perhaps impossible) to create a
                  --  coverage test for the inlined code of the call.

                  Set_Digits (FD, FD + DA - 1);

               else
                  Set_Digits (FD, LD);
                  Set_Zeroes (DA - ND);
               end if;
            end;

         --  At least one digit before point in input

         else
            --  Less digits in input than are needed before point
            --    Input: 1PP  Output: 100.000

            if ND < Digits_Before_Point then

               --  Special case, if the input is the single digit 0, then we
               --  do not want 000.000, but instead 0.000.

               if ND = 1 and then Digs (FD) = '0' then
                  Set_Blanks_And_Sign (Fore - 1);
                  Set ('0');

               --  Normal case where we need to output scaling zeroes

               else
                  Set_Blanks_And_Sign (Fore - Digits_Before_Point);
                  Set_Digits (FD, LD);
                  Set_Zeroes (Digits_Before_Point - ND);
               end if;

               --  Set period and zeroes after the period

               Set ('.');
               Set_Zeroes (Digits_After_Point);

            --  Input has full amount of digits before decimal point

            else
               Set_Blanks_And_Sign (Fore - Digits_Before_Point);
               Set_Digits (FD, FD + Digits_Before_Point - 1);
               Set ('.');
               Set_Digits (FD + Digits_Before_Point, LD);
               Set_Zeroes (Digits_After_Point - (ND - Digits_Before_Point));
            end if;
         end if;
      end if;
   end Set_Decimal_Digits;

   -----------------------
   -- Set_Image_Decimal --
   -----------------------

   procedure Set_Image_Decimal
     (V     : Integer;
      S     : in out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural)
   is
      Digs : String := Integer'Image (V);
      --  Sign and digits of decimal value

   begin
      Set_Decimal_Digits (Digs, Digs'Length, S, P, Scale, Fore, Aft, Exp);
   end Set_Image_Decimal;

end System.Img_Dec;
