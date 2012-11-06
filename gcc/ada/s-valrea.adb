------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

with System.Powten_Table;  use System.Powten_Table;
with System.Val_Util;      use System.Val_Util;
with System.Float_Control;

package body System.Val_Real is

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Long_Long_Float
   is
      P : Integer;
      --  Local copy of string pointer

      Base : Long_Long_Float;
      --  Base value

      Uval : Long_Long_Float;
      --  Accumulated float result

      subtype Digs is Character range '0' .. '9';
      --  Used to check for decimal digit

      Scale : Integer := 0;
      --  Power of Base to multiply result by

      Start : Positive;
      --  Position of starting non-blank character

      Minus : Boolean;
      --  Set to True if minus sign is present, otherwise to False

      Bad_Base : Boolean := False;
      --  Set True if Base out of range or if out of range digit

      After_Point : Natural := 0;
      --  Set to 1 after the point

      Num_Saved_Zeroes : Natural := 0;
      --  This counts zeroes after the decimal point. A non-zero value means
      --  that this number of previously scanned digits are zero. If the end
      --  of the number is reached, these zeroes are simply discarded, which
      --  ensures that trailing zeroes after the point never affect the value
      --  (which might otherwise happen as a result of rounding). With this
      --  processing in place, we can ensure that, for example, we get the
      --  same exact result from 1.0E+49 and 1.0000000E+49. This is not
      --  necessarily required in a case like this where the result is not
      --  a machine number, but it is certainly a desirable behavior.

      procedure Scanf;
      --  Scans integer literal value starting at current character position.
      --  For each digit encountered, Uval is multiplied by 10.0, and the new
      --  digit value is incremented. In addition Scale is decremented for each
      --  digit encountered if we are after the point (After_Point = 1). The
      --  longest possible syntactically valid numeral is scanned out, and on
      --  return P points past the last character. On entry, the current
      --  character is known to be a digit, so a numeral is definitely present.

      -----------
      -- Scanf --
      -----------

      procedure Scanf is
         Digit : Natural;

      begin
         loop
            Digit := Character'Pos (Str (P)) - Character'Pos ('0');
            P := P + 1;

            --  Save up trailing zeroes after the decimal point

            if Digit = 0 and then After_Point = 1 then
               Num_Saved_Zeroes := Num_Saved_Zeroes + 1;

            --  Here for a non-zero digit

            else
               --  First deal with any previously saved zeroes

               if Num_Saved_Zeroes /= 0 then
                  while Num_Saved_Zeroes > Maxpow loop
                     Uval := Uval * Powten (Maxpow);
                     Num_Saved_Zeroes := Num_Saved_Zeroes - Maxpow;
                     Scale := Scale - Maxpow;
                  end loop;

                  Uval := Uval * Powten (Num_Saved_Zeroes);
                  Scale := Scale - Num_Saved_Zeroes;

                  Num_Saved_Zeroes := 0;
               end if;

               --  Accumulate new digit

               Uval := Uval * 10.0 + Long_Long_Float (Digit);
               Scale := Scale - After_Point;
            end if;

            --  Done if end of input field

            if P > Max then
               return;

            --  Check next character

            elsif Str (P) not in Digs then
               if Str (P) = '_' then
                  Scan_Underscore (Str, P, Ptr, Max, False);
               else
                  return;
               end if;
            end if;
         end loop;
      end Scanf;

   --  Start of processing for System.Scan_Real

   begin
      --  We call the floating-point processor reset routine so that we can
      --  be sure the floating-point processor is properly set for conversion
      --  calls. This is notably need on Windows, where calls to the operating
      --  system randomly reset the processor into 64-bit mode.

      System.Float_Control.Reset;

      Scan_Sign (Str, Ptr, Max, Minus, Start);
      P := Ptr.all;
      Ptr.all := Start;

      --  If digit, scan numeral before point

      if Str (P) in Digs then
         Uval := 0.0;
         Scanf;

      --  Initial point, allowed only if followed by digit (RM 3.5(47))

      elsif Str (P) = '.'
        and then P < Max
        and then Str (P + 1) in Digs
      then
         Uval := 0.0;

      --  Any other initial character is an error

      else
         Bad_Value (Str);
      end if;

      --  Deal with based case

      if P < Max and then (Str (P) = ':' or else Str (P) = '#') then
         declare
            Base_Char : constant Character := Str (P);
            Digit     : Natural;
            Fdigit    : Long_Long_Float;

         begin
            --  Set bad base if out of range, and use safe base of 16.0,
            --  to guard against division by zero in the loop below.

            if Uval < 2.0 or else Uval > 16.0 then
               Bad_Base := True;
               Uval := 16.0;
            end if;

            Base := Uval;
            Uval := 0.0;
            P := P + 1;

            --  Special check to allow initial point (RM 3.5(49))

            if Str (P) = '.' then
               After_Point := 1;
               P := P + 1;
            end if;

            --  Loop to scan digits of based number. On entry to the loop we
            --  must have a valid digit. If we don't, then we have an illegal
            --  floating-point value, and we raise Constraint_Error, note that
            --  Ptr at this stage was reset to the proper (Start) value.

            loop
               if P > Max then
                  Bad_Value (Str);

               elsif Str (P) in Digs then
                  Digit := Character'Pos (Str (P)) - Character'Pos ('0');

               elsif Str (P) in 'A' .. 'F' then
                  Digit :=
                    Character'Pos (Str (P)) - (Character'Pos ('A') - 10);

               elsif Str (P) in 'a' .. 'f' then
                  Digit :=
                    Character'Pos (Str (P)) - (Character'Pos ('a') - 10);

               else
                  Bad_Value (Str);
               end if;

               --  Save up trailing zeroes after the decimal point

               if Digit = 0 and then After_Point = 1 then
                  Num_Saved_Zeroes := Num_Saved_Zeroes + 1;

               --  Here for a non-zero digit

               else
                  --  First deal with any previously saved zeroes

                  if Num_Saved_Zeroes /= 0 then
                     Uval := Uval * Base ** Num_Saved_Zeroes;
                     Scale := Scale - Num_Saved_Zeroes;
                     Num_Saved_Zeroes := 0;
                  end if;

                  --  Now accumulate the new digit

                  Fdigit := Long_Long_Float (Digit);

                  if Fdigit >= Base then
                     Bad_Base := True;
                  else
                     Scale := Scale - After_Point;
                     Uval := Uval * Base + Fdigit;
                  end if;
               end if;

               P := P + 1;

               if P > Max then
                  Bad_Value (Str);

               elsif Str (P) = '_' then
                  Scan_Underscore (Str, P, Ptr, Max, True);

               else
                  --  Skip past period after digit. Note that the processing
                  --  here will permit either a digit after the period, or the
                  --  terminating base character, as allowed in (RM 3.5(48))

                  if Str (P) = '.' and then After_Point = 0 then
                     P := P + 1;
                     After_Point := 1;

                     if P > Max then
                        Bad_Value (Str);
                     end if;
                  end if;

                  exit when Str (P) = Base_Char;
               end if;
            end loop;

            --  Based number successfully scanned out (point was found)

            Ptr.all := P + 1;
         end;

      --  Non-based case, check for being at decimal point now. Note that
      --  in Ada 95, we do not insist on a decimal point being present

      else
         Base := 10.0;
         After_Point := 1;

         if P <= Max and then Str (P) = '.' then
            P := P + 1;

            --  Scan digits after point if any are present (RM 3.5(46))

            if P <= Max and then Str (P) in Digs then
               Scanf;
            end if;
         end if;

         Ptr.all := P;
      end if;

      --  At this point, we have Uval containing the digits of the value as
      --  an integer, and Scale indicates the negative of the number of digits
      --  after the point. Base contains the base value (an integral value in
      --  the range 2.0 .. 16.0). Test for exponent, must be at least one
      --  character after the E for the exponent to be valid.

      Scale := Scale + Scan_Exponent (Str, Ptr, Max, Real => True);

      --  At this point the exponent has been scanned if one is present and
      --  Scale is adjusted to include the exponent value. Uval contains the
      --  the integral value which is to be multiplied by Base ** Scale.

      --  If base is not 10, use exponentiation for scaling

      if Base /= 10.0 then
         Uval := Uval * Base ** Scale;

      --  For base 10, use power of ten table, repeatedly if necessary

      elsif Scale > 0 then
         while Scale > Maxpow loop
            Uval := Uval * Powten (Maxpow);
            Scale := Scale - Maxpow;
         end loop;

         if Scale > 0 then
            Uval := Uval * Powten (Scale);
         end if;

      elsif Scale < 0 then
         while (-Scale) > Maxpow loop
            Uval := Uval / Powten (Maxpow);
            Scale := Scale + Maxpow;
         end loop;

         if Scale < 0 then
            Uval := Uval / Powten (-Scale);
         end if;
      end if;

      --  Here is where we check for a bad based number

      if Bad_Base then
         Bad_Value (Str);

      --  If OK, then deal with initial minus sign, note that this processing
      --  is done even if Uval is zero, so that -0.0 is correctly interpreted.

      else
         if Minus then
            return -Uval;
         else
            return Uval;
         end if;
      end if;
   end Scan_Real;

   ----------------
   -- Value_Real --
   ----------------

   function Value_Real (Str : String) return Long_Long_Float is
      V : Long_Long_Float;
      P : aliased Integer := Str'First;
   begin
      V := Scan_Real (Str, P'Access, Str'Last);
      Scan_Trailing_Blanks (Str, P);
      return V;
   end Value_Real;

end System.Val_Real;
