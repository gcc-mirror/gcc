------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
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

with System.Val_Util;      use System.Val_Util;
with System.Float_Control;

package body System.Val_Real is

   procedure Scan_Integral_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : out Long_Long_Integer;
       Scale          : out Integer;
       Base_Violation : in out Boolean;
       Base           : Long_Long_Integer := 10;
       Base_Specified : Boolean := False);
   --  Scan the integral part of a real (i.e: before decimal separator)
   --
   --  The string parsed is Str (Index .. Max), and after the call Index will
   --  point to the first non parsed character.
   --
   --  For each digit parsed either value := value * base + digit, or scale
   --  is incremented by 1.
   --
   --  Base_Violation will be set to True a digit found is not part of the Base

   procedure Scan_Decimal_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : in out Long_Long_Integer;
       Scale          : in out Integer;
       Base_Violation : in out Boolean;
       Base           : Long_Long_Integer := 10;
       Base_Specified : Boolean := False);
   --  Scan the decimal part of a real (i.e: after decimal separator)
   --
   --  The string parsed is Str (Index .. Max), and after the call Index will
   --  point to the first non parsed character.
   --
   --  For each digit parsed value = value * base + digit and scale is
   --  decremented by 1. If precision limit is reached remaining digits are
   --  still parsed but ignored.
   --
   --  Base_Violation will be set to True a digit found is not part of the Base

   subtype Char_As_Digit is Long_Long_Integer range -2 .. 15;
   subtype Valid_Digit is Char_As_Digit range 0 .. Char_As_Digit'Last;
   Underscore : constant Char_As_Digit := -2;
   E_Digit : constant Char_As_Digit := 14;

   function As_Digit (C : Character) return Char_As_Digit;
   --  Given a character return the digit it represent. If the character is
   --  not a digit then a negative value is returned, -2 for underscore and
   --  -1 for any other character.

   Precision_Limit : constant Long_Long_Integer :=
      2 ** (Long_Long_Float'Machine_Mantissa - 1) - 1;
   --  This is an upper bound for the number of bits used to represent the
   --  mantissa. Beyond that number, any digits parsed are useless.

   --------------
   -- As_Digit --
   --------------

   function As_Digit (C : Character) return Char_As_Digit
   is
   begin
      case C is
         when '0' .. '9' =>
            return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' =>
            return Character'Pos (C) - (Character'Pos ('a') - 10);
         when 'A' .. 'F' =>
            return Character'Pos (C) - (Character'Pos ('A') - 10);
         when '_' =>
            return Underscore;
         when others =>
            return -1;
      end case;
   end As_Digit;

   -------------------------
   -- Scan_Decimal_Digits --
   -------------------------

   procedure Scan_Decimal_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : in out Long_Long_Integer;
       Scale          : in out Integer;
       Base_Violation : in out Boolean;
       Base           : Long_Long_Integer := 10;
       Base_Specified : Boolean := False)

   is
      Precision_Limit_Reached : Boolean := False;
      --  Set to True if addition of a digit will cause Value to be superior
      --  to Precision_Limit.

      Digit : Char_As_Digit;
      --  The current digit.

      Trailing_Zeros : Natural := 0;
      --  Number of trailing zeros at a given point.
   begin

      --  If initial Scale is not 0 then it means that Precision_Limit was
      --  reached during integral part scanning.
      if Scale > 0 then
         Precision_Limit_Reached := True;
      end if;

      --  The function precondition is that the first character is a valid
      --  digit.
      Digit := As_Digit (Str (Index));

      loop
         --  Check if base is correct. If the base is not specified the digit
         --  E or e cannot be considered as a base violation as it can be used
         --  for exponentiation.
         if Digit >= Base then
            if Base_Specified then
               Base_Violation := True;
            elsif Digit = E_Digit then
               return;
            else
               Base_Violation := True;
            end if;
         end if;

         --  If precision limit has been reached just ignore any remaining
         --  digits for the computation of Value and Scale. The scanning
         --  should continue only to assess the validity of the string
         if not Precision_Limit_Reached then
            if Digit = 0 then
               --  Trailing '0' digits are ignored unless a non-zero digit is
               --  found.
               Trailing_Zeros := Trailing_Zeros + 1;
            else

               --  Handle accumulated zeros.
               for J in 1 .. Trailing_Zeros loop
                  if Value > Precision_Limit / Base then
                     Precision_Limit_Reached := True;
                     exit;
                  else
                     Value := Value * Base;
                     Scale := Scale - 1;
                  end if;
               end loop;

               --  Reset trailing zero counter
               Trailing_Zeros := 0;

               --  Handle current non zero digit
               if Value > (Precision_Limit - Digit) / Base then
                  Precision_Limit_Reached := True;
               else
                  Value := Value * Base + Digit;
                  Scale := Scale - 1;
               end if;
            end if;
         end if;

         --  Check next character
         Index := Index + 1;

         if Index > Max then
            return;
         end if;

         Digit := As_Digit (Str (Index));

         if Digit < 0 then
            if Digit = Underscore and Index + 1 <= Max then
               --  Underscore is only alllowed if followed by a digit
               Digit := As_Digit (Str (Index + 1));
               if Digit in Valid_Digit then
                  Index := Index + 1;
               else
                  return;
               end if;
            else
               --  Neither a valid underscore nor a digit.
               return;
            end if;
         end if;
      end loop;

   end Scan_Decimal_Digits;

   --------------------------
   -- Scan_Integral_Digits --
   --------------------------

   procedure Scan_Integral_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : out Long_Long_Integer;
       Scale          : out Integer;
       Base_Violation : in out Boolean;
       Base           : Long_Long_Integer := 10;
       Base_Specified : Boolean := False)
   is
      Precision_Limit_Reached : Boolean := False;
      --  Set to True if addition of a digit will cause Value to be superior
      --  to Precision_Limit.

      Digit : Char_As_Digit;
      --  The current digit
   begin

      --  Initialize Scale and Value
      Value := 0;
      Scale := 0;

      --  The function precondition is that the first character is a valid
      --  digit.
      Digit := As_Digit (Str (Index));

      loop
         --  Check if base is correct. If the base is not specified the digit
         --  E or e cannot be considered as a base violation as it can be used
         --  for exponentiation.
         if Digit >= Base then
            if Base_Specified then
               Base_Violation := True;
            elsif Digit = E_Digit then
               return;
            else
               Base_Violation := True;
            end if;
         end if;

         if Precision_Limit_Reached then
            --  Precision limit has been reached so just update the exponent
            Scale := Scale + 1;
         else
            if Value > (Precision_Limit - Digit) / Base then
               --  Updating Value will overflow so ignore this digit and any
               --  following ones. Only update the scale
               Precision_Limit_Reached := True;
               Scale := Scale + 1;
            else
               Value := Value * Base + Digit;
            end if;
         end if;

         --  Look for the next character
         Index := Index + 1;
         if Index > Max then
            return;
         end if;

         Digit := As_Digit (Str (Index));

         if Digit not in Valid_Digit then
            --  Next character is not a digit. In that case stop scanning
            --  unless the next chracter is an underscore followed by a digit.
            if Digit = Underscore and Index + 1 <= Max then
               Digit := As_Digit (Str (Index + 1));
               if Digit in Valid_Digit then
                  Index := Index + 1;
               else
                  return;
               end if;
            else
               return;
            end if;
         end if;
      end loop;

   end Scan_Integral_Digits;

   ---------------
   -- Scan_Real --
   ---------------

   function Scan_Real
      (Str : String;
       Ptr : not null access Integer;
       Max : Integer)
      return Long_Long_Float

   is
      Start : Positive;
      --  Position of starting non-blank character

      Minus : Boolean;
      --  Set to True if minus sign is present, otherwise to False

      Index : Integer;
      --  Local copy of string pointer

      Int_Value : Long_Long_Integer := -1;
      --  Mantissa as an Integer

      Int_Scale : Integer := 0;
      --  Exponent value

      Base_Violation : Boolean := False;
      --  If True some digits where not in the base. The float is still scan
      --  till the end even if an error will be raised.

      Uval : Long_Long_Float := 0.0;
      --  Contain the final value at the end of the function

      After_Point : Boolean := False;
      --  True if a decimal should be parsed

      Base : Long_Long_Integer := 10;
      --  Current base (default: 10)

      Base_Char : Character := ASCII.NUL;
      --  Character used to set the base. If Nul this means that default
      --  base is used.

   begin
      --  We do not tolerate strings with Str'Last = Positive'Last

      if Str'Last = Positive'Last then
         raise Program_Error with
           "string upper bound is Positive'Last, not supported";
      end if;

      --  We call the floating-point processor reset routine so that we can
      --  be sure the floating-point processor is properly set for conversion
      --  calls. This is notably need on Windows, where calls to the operating
      --  system randomly reset the processor into 64-bit mode.

      System.Float_Control.Reset;

      --  Scan the optional sign
      Scan_Sign (Str, Ptr, Max, Minus, Start);
      Index := Ptr.all;
      Ptr.all := Start;

      --  First character can be either a decimal digit or a dot.
      if Str (Index) in '0' .. '9' then
         --  If this is a digit it can indicates either the float decimal
         --  part or the base to use
         Scan_Integral_Digits
            (Str,
             Index,
             Max            => Max,
             Value          => Int_Value,
             Scale          => Int_Scale,
             Base_Violation => Base_Violation,
             Base           => 10);
      elsif Str (Index) = '.' and then
         --  A dot is only allowed if followed by a digit.
         Index < Max and then
         Str (Index + 1) in '0' .. '9'
      then
         --  Initial point, allowed only if followed by digit (RM 3.5(47))
         After_Point := True;
         Index := Index + 1;
         Int_Value := 0;
      else
         Bad_Value (Str);
      end if;

      --  Check if the first number encountered is a base
      if Index < Max and then
         (Str (Index) = '#' or else Str (Index) = ':')
      then
         Base_Char := Str (Index);
         Base := Int_Value;

         --  Reset Int_Value to indicate that parsing of integral value should
         --  be done
         Int_Value := -1;
         if Base < 2 or else Base > 16 then
            Base_Violation := True;
            Base := 16;
         end if;

         Index := Index + 1;

         if Str (Index) = '.' and then
            Index < Max and then
            As_Digit (Str (Index + 1)) in Valid_Digit
         then
            After_Point := True;
            Index := Index + 1;
            Int_Value := 0;
         end if;
      end if;

      --  Does scanning of integral part needed
      if Int_Value < 0 then
         if Index > Max or else As_Digit (Str (Index)) not in Valid_Digit then
            Bad_Value (Str);
         end if;

         Scan_Integral_Digits
            (Str,
             Index,
             Max => Max,
             Value => Int_Value,
             Scale => Int_Scale,
             Base_Violation => Base_Violation,
             Base => Base,
             Base_Specified => Base_Char /= ASCII.NUL);
      end if;

      --  Do we have a dot ?
      if not After_Point and then
         Index <= Max and then
         Str (Index) = '.'
      then
         --  At this stage if After_Point was not set, this means that an
         --  integral part has been found. Thus the dot is valid even if not
         --  followed by a digit.
         if Index < Max and then As_Digit (Str (Index + 1)) in Valid_Digit then
            After_Point := True;
         end if;

         Index := Index + 1;
      end if;

      if After_Point then
         --  Parse decimal part
         Scan_Decimal_Digits
            (Str,
             Index,
             Max => Max,
             Value => Int_Value,
             Scale => Int_Scale,
             Base_Violation => Base_Violation,
             Base => Base,
             Base_Specified => Base_Char /= ASCII.NUL);
      end if;

      --  If an explicit base was specified ensure that the delimiter is found
      if Base_Char /= ASCII.NUL then
         if Index > Max or else Str (Index) /= Base_Char then
            Bad_Value (Str);
         else
            Index := Index + 1;
         end if;
      end if;

      --  Compute the final value
      Uval := Long_Long_Float (Int_Value);

      --  Update pointer and scan exponent.
      Ptr.all := Index;

      Int_Scale := Int_Scale + Scan_Exponent (Str,
                                              Ptr,
                                              Max,
                                              Real => True);

      Uval := Uval * Long_Long_Float (Base) ** Int_Scale;

      --  Here is where we check for a bad based number
      if Base_Violation then
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
   begin
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      if Str'Last = Positive'Last then
         declare
            subtype NT is String (1 .. Str'Length);
         begin
            return Value_Real (NT (Str));
         end;

      --  Normal case where Str'Last < Positive'Last

      else
         declare
            V : Long_Long_Float;
            P : aliased Integer := Str'First;
         begin
            V := Scan_Real (Str, P'Access, Str'Last);
            Scan_Trailing_Blanks (Str, P);
            return V;
         end;
      end if;
   end Value_Real;

end System.Val_Real;
