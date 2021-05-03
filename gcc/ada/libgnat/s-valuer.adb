------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L U E _ R                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
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

with System.Val_Util; use System.Val_Util;

package body System.Value_R is

   subtype Char_As_Digit is Unsigned range 0 .. 17;
   subtype Valid_Digit is Char_As_Digit range 0 .. 15;
   E_Digit     : constant Char_As_Digit := 14;
   Underscore  : constant Char_As_Digit := 16;
   Not_A_Digit : constant Char_As_Digit := 17;

   function As_Digit (C : Character) return Char_As_Digit;
   --  Given a character return the digit it represents

   procedure Round_Extra
     (Digit : Char_As_Digit;
      Value : in out Uns;
      Scale : in out Integer;
      Extra : in out Char_As_Digit;
      Base  : Unsigned);
   --  Round the triplet (Value, Scale, Extra) according to Digit in Base

   procedure Scan_Decimal_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : in out Uns;
       Scale          : in out Integer;
       Extra          : in out Char_As_Digit;
       Base_Violation : in out Boolean;
       Base           : Unsigned;
       Base_Specified : Boolean);
   --  Scan the decimal part of a real (i.e. after decimal separator)
   --
   --  The string parsed is Str (Index .. Max) and after the call Index will
   --  point to the first non-parsed character.
   --
   --  For each digit parsed, Value = Value * Base + Digit and Scale is
   --  decremented by 1. If precision limit is reached, remaining digits are
   --  still parsed but ignored, except for the first which is stored in Extra.
   --
   --  Base_Violation is set to True if a digit found is not part of the Base
   --
   --  If Base_Specified is set, then the base was specified in the real

   procedure Scan_Integral_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : out Uns;
       Scale          : out Integer;
       Extra          : out Char_As_Digit;
       Base_Violation : in out Boolean;
       Base           : Unsigned;
       Base_Specified : Boolean);
   --  Scan the integral part of a real (i.e. before decimal separator)
   --
   --  The string parsed is Str (Index .. Max) and after the call Index will
   --  point to the first non-parsed character.
   --
   --  For each digit parsed, either Value := Value * Base + Digit or Scale
   --  is incremented by 1 if precision limit is reached, in which case the
   --  remaining digits are still parsed but ignored, except for the first
   --  which is stored in Extra.
   --
   --  Base_Violation is set to True if a digit found is not part of the Base
   --
   --  If Base_Specified is set, then the base was specified in the real

   --------------
   -- As_Digit --
   --------------

   function As_Digit (C : Character) return Char_As_Digit is
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
            return Not_A_Digit;
      end case;
   end As_Digit;

   -----------------
   -- Round_Extra --
   -----------------

   procedure Round_Extra
     (Digit : Char_As_Digit;
      Value : in out Uns;
      Scale : in out Integer;
      Extra : in out Char_As_Digit;
      Base  : Unsigned)
   is
      pragma Assert (Base in 2 .. 16);

      B : constant Uns := Uns (Base);

   begin
      if Digit >= Base / 2 then

         --  If Extra is maximum, round Value

         if Extra = Base - 1 then

            --  If Value is maximum, scale it up

            if Value = Precision_Limit then
               Extra := Char_As_Digit (Value mod B);
               Value := Value / B;
               Scale := Scale + 1;
               Round_Extra (Digit, Value, Scale, Extra, Base);

            else
               Extra := 0;
               Value := Value + 1;
            end if;

         else
            Extra := Extra + 1;
         end if;
      end if;
   end Round_Extra;

   -------------------------
   -- Scan_Decimal_Digits --
   -------------------------

   procedure Scan_Decimal_Digits
      (Str            : String;
       Index          : in out Integer;
       Max            : Integer;
       Value          : in out Uns;
       Scale          : in out Integer;
       Extra          : in out Char_As_Digit;
       Base_Violation : in out Boolean;
       Base           : Unsigned;
       Base_Specified : Boolean)

   is
      pragma Assert (Base in 2 .. 16);
      pragma Assert (Index in Str'Range);
      pragma Assert (Max <= Str'Last);

      Umax : constant Uns := (Precision_Limit - Uns (Base) + 1) / Uns (Base);
      --  Max value which cannot overflow on accumulating next digit

      UmaxB : constant Uns := Precision_Limit / Uns (Base);
      --  Numbers bigger than UmaxB overflow if multiplied by base

      Precision_Limit_Reached : Boolean := False;
      --  Set to True if addition of a digit will cause Value to be superior
      --  to Precision_Limit.

      Precision_Limit_Just_Reached : Boolean;
      --  Set to True if Precision_Limit_Reached was just set to True, but only
      --  used when Round is True.

      Digit : Char_As_Digit;
      --  The current digit

      Temp : Uns;
      --  Temporary

      Trailing_Zeros : Natural := 0;
      --  Number of trailing zeros at a given point

   begin
      --  If initial Scale is not 0 then it means that Precision_Limit was
      --  reached during scanning of the integral part.

      if Scale > 0 then
         Precision_Limit_Reached := True;
      else
         Extra := 0;
      end if;

      if Round then
         Precision_Limit_Just_Reached := False;
      end if;

      --  The function precondition is that the first character is a valid
      --  digit.

      Digit := As_Digit (Str (Index));

      loop
         --  Check if base is correct. If the base is not specified, the digit
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

         --  If precision limit has been reached, just ignore any remaining
         --  digits for the computation of Value and Scale, but store the
         --  first in Extra and use the second to round Extra. The scanning
         --  should continue only to assess the validity of the string.

         if Precision_Limit_Reached then
            if Round and then Precision_Limit_Just_Reached then
               Round_Extra (Digit, Value, Scale, Extra, Base);
               Precision_Limit_Just_Reached := False;
            end if;

         else
            --  Trailing '0' digits are ignored until a non-zero digit is found

            if Digit = 0 then
               Trailing_Zeros := Trailing_Zeros + 1;

            else
               --  Handle accumulated zeros.

               for J in 1 .. Trailing_Zeros loop
                  if Value <= UmaxB then
                     Value := Value * Uns (Base);
                     Scale := Scale - 1;

                  else
                     Extra := 0;
                     Precision_Limit_Reached := True;
                     if Round and then J = Trailing_Zeros then
                        Round_Extra (Digit, Value, Scale, Extra, Base);
                     end if;
                     exit;
                  end if;
               end loop;

               --  Reset trailing zero counter

               Trailing_Zeros := 0;

               --  Handle current non zero digit

               Temp := Value * Uns (Base) + Uns (Digit);

               --  Precision_Limit_Reached may have been set above

               if Precision_Limit_Reached then
                  null;

               --  Check if Temp is larger than Precision_Limit, taking into
               --  account that Temp may wrap around when Precision_Limit is
               --  equal to the largest integer.

               elsif Value <= Umax
                 or else (Value <= UmaxB
                           and then ((Precision_Limit < Uns'Last
                                       and then Temp <= Precision_Limit)
                                     or else (Precision_Limit = Uns'Last
                                               and then Temp >= Uns (Base))))
               then
                  Value := Temp;
                  Scale := Scale - 1;

               else
                  Extra := Digit;
                  Precision_Limit_Reached := True;
                  if Round then
                     Precision_Limit_Just_Reached := True;
                  end if;
               end if;
            end if;
         end if;

         --  Check next character

         Index := Index + 1;

         if Index > Max then
            return;
         end if;

         Digit := As_Digit (Str (Index));

         if Digit not in Valid_Digit then

            --  Underscore is only allowed if followed by a digit

            if Digit = Underscore and Index + 1 <= Max then

               Digit := As_Digit (Str (Index + 1));
               if Digit in Valid_Digit then
                  Index := Index + 1;
               else
                  return;
               end if;

            --  Neither a valid underscore nor a digit

            else
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
       Value          : out Uns;
       Scale          : out Integer;
       Extra          : out Char_As_Digit;
       Base_Violation : in out Boolean;
       Base           : Unsigned;
       Base_Specified : Boolean)
   is
      pragma Assert (Base in 2 .. 16);

      Umax : constant Uns := (Precision_Limit - Uns (Base) + 1) / Uns (Base);
      --  Max value which cannot overflow on accumulating next digit

      UmaxB : constant Uns := Precision_Limit / Uns (Base);
      --  Numbers bigger than UmaxB overflow if multiplied by base

      Precision_Limit_Reached : Boolean := False;
      --  Set to True if addition of a digit will cause Value to be superior
      --  to Precision_Limit.

      Precision_Limit_Just_Reached : Boolean;
      --  Set to True if Precision_Limit_Reached was just set to True, but only
      --  used when Round is True.

      Digit : Char_As_Digit;
      --  The current digit

      Temp : Uns;
      --  Temporary

   begin
      --  Initialize Value, Scale and Extra

      Value := 0;
      Scale := 0;
      Extra := 0;

      if Round then
         Precision_Limit_Just_Reached := False;
      end if;

      pragma Assert (Max <= Str'Last);

      --  The function precondition is that the first character is a valid
      --  digit.

      Digit := As_Digit (Str (Index));

      loop
         --  Check if base is correct. If the base is not specified, the digit
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

         --  If precision limit has been reached, just ignore any remaining
         --  digits for the computation of Value and Scale, but store the
         --  first in Extra and use the second to round Extra. The scanning
         --  should continue only to assess the validity of the string.

         if Precision_Limit_Reached then
            Scale := Scale + 1;

            if Round and then Precision_Limit_Just_Reached then
               Round_Extra (Digit, Value, Scale, Extra, Base);
               Precision_Limit_Just_Reached := False;
            end if;

         else
            Temp := Value * Uns (Base) + Uns (Digit);

            --  Check if Temp is larger than Precision_Limit, taking into
            --  account that Temp may wrap around when Precision_Limit is
            --  equal to the largest integer.

            if Value <= Umax
              or else (Value <= UmaxB
                        and then ((Precision_Limit < Uns'Last
                                    and then Temp <= Precision_Limit)
                                  or else (Precision_Limit = Uns'Last
                                            and then Temp >= Uns (Base))))
            then
               Value := Temp;

            else
               Extra := Digit;
               Precision_Limit_Reached := True;
               if Round then
                  Precision_Limit_Just_Reached := True;
               end if;
               Scale := Scale + 1;
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

   -------------------
   -- Scan_Raw_Real --
   -------------------

   function Scan_Raw_Real
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Base  : out Unsigned;
      Scale : out Integer;
      Extra : out Unsigned;
      Minus : out Boolean) return Uns
   is
      pragma Assert (Max <= Str'Last);

      After_Point : Boolean;
      --  True if a decimal should be parsed

      Base_Char : Character := ASCII.NUL;
      --  Character used to set the base. If Nul this means that default
      --  base is used.

      Base_Violation : Boolean := False;
      --  If True some digits where not in the base. The real is still scanned
      --  till the end even if an error will be raised.

      Index : Integer;
      --  Local copy of string pointer

      Start : Positive;
      pragma Unreferenced (Start);

      Value : Uns;
      --  Mantissa as an Integer

   begin
      --  The default base is 10

      Base := 10;

      --  We do not tolerate strings with Str'Last = Positive'Last

      if Str'Last = Positive'Last then
         raise Program_Error with
           "string upper bound is Positive'Last, not supported";
      end if;

      --  Scan the optional sign

      Scan_Sign (Str, Ptr, Max, Minus, Start);
      Index := Ptr.all;

      pragma Assert (Index >= Str'First);

      pragma Annotate (CodePeer, Modified, Str (Index));

      --  First character can be either a decimal digit or a dot and for some
      --  reason CodePeer incorrectly thinks it is always a digit.

      if Str (Index) in '0' .. '9' then
         After_Point := False;

         --  If this is a digit it can indicates either the float decimal
         --  part or the base to use.

         Scan_Integral_Digits
           (Str, Index, Max, Value, Scale, Char_As_Digit (Extra),
            Base_Violation, Base, Base_Specified => False);

      --  A dot is allowed only if followed by a digit (RM 3.5(47))

      elsif Str (Index) = '.'
        and then Index < Max
        and then Str (Index + 1) in '0' .. '9'
      then
         After_Point := True;
         Index := Index + 1;
         Value := 0;
         Scale := 0;
         Extra := 0;

      else
         Bad_Value (Str);
      end if;

      --  Check if the first number encountered is a base

      pragma Assert (Index >= Str'First);

      if Index < Max
        and then (Str (Index) = '#' or else Str (Index) = ':')
      then
         Base_Char := Str (Index);

         if Value in 2 .. 16 then
            Base := Unsigned (Value);
         else
            Base_Violation := True;
            Base := 16;
         end if;

         Index := Index + 1;

         if Str (Index) = '.'
           and then Index < Max
           and then As_Digit (Str (Index + 1)) in Valid_Digit
         then
            After_Point := True;
            Index := Index + 1;
            Value := 0;
         end if;
      end if;

      --  Scan the integral part if still necessary

      if Base_Char /= ASCII.NUL and then not After_Point then
         if Index > Max or else As_Digit (Str (Index)) not in Valid_Digit then
            Bad_Value (Str);
         end if;

         Scan_Integral_Digits
           (Str, Index, Max, Value, Scale, Char_As_Digit (Extra),
            Base_Violation, Base, Base_Specified => Base_Char /= ASCII.NUL);
      end if;

      --  Do we have a dot?

      pragma Assert (Index >= Str'First);

      if not After_Point and then Index <= Max and then Str (Index) = '.' then

         --  At this stage if After_Point was not set, this means that an
         --  integral part has been found. Thus the dot is valid even if not
         --  followed by a digit.

         if Index < Max and then As_Digit (Str (Index + 1)) in Valid_Digit then
            After_Point := True;
         end if;

         Index := Index + 1;
      end if;

      --  Scan the decimal part

      if After_Point then
         pragma Assert (Index <= Max);

         Scan_Decimal_Digits
           (Str, Index, Max, Value, Scale, Char_As_Digit (Extra),
            Base_Violation, Base, Base_Specified => Base_Char /= ASCII.NUL);
      end if;

      --  If an explicit base was specified ensure that the delimiter is found

      if Base_Char /= ASCII.NUL then
         pragma Assert (Index > Max or else Index in Str'Range);

         if Index > Max or else Str (Index) /= Base_Char then
            Bad_Value (Str);
         else
            Index := Index + 1;
         end if;
      end if;

      --  Update pointer and scan exponent

      Ptr.all := Index;
      Scale := Scale + Scan_Exponent (Str, Ptr, Max, Real => True);

      --  Here is where we check for a bad based number

      if Base_Violation then
         Bad_Value (Str);
      else
         return Value;
      end if;

   end Scan_Raw_Real;

   --------------------
   -- Value_Raw_Real --
   --------------------

   function Value_Raw_Real
     (Str   : String;
      Base  : out Unsigned;
      Scale : out Integer;
      Extra : out Unsigned;
      Minus : out Boolean) return Uns
   is
   begin
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      if Str'Last = Positive'Last then
         declare
            subtype NT is String (1 .. Str'Length);
         begin
            return Value_Raw_Real (NT (Str), Base, Scale, Extra, Minus);
         end;

      --  Normal case where Str'Last < Positive'Last

      else
         declare
            V : Uns;
            P : aliased Integer := Str'First;
         begin
            V := Scan_Raw_Real
                   (Str, P'Access, Str'Last, Base, Scale, Extra, Minus);
            Scan_Trailing_Blanks (Str, P);
            return V;
         end;
      end if;
   end Value_Raw_Real;

end System.Value_R;
