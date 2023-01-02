------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ D                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2023, Free Software Foundation, Inc.       --
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
with System.Val_Util;       use System.Val_Util;
with System.Value_R;

package body System.Value_D is

   pragma Assert (Int'Size <= Uns'Size);
   --  We need an unsigned type large enough to represent the mantissa

   package Impl is new Value_R (Uns, 1, 2**(Int'Size - 1), Round => False);
   --  We do not use the Extra digit for decimal fixed-point types

   function Integer_to_Decimal
     (Str    : String;
      Val    : Uns;
      Base   : Unsigned;
      ScaleB : Integer;
      Minus  : Boolean;
      Scale  : Integer) return Int;
   --  Convert the real value from integer to decimal representation

   ------------------------
   -- Integer_to_Decimal --
   ------------------------

   function Integer_to_Decimal
     (Str    : String;
      Val    : Uns;
      Base   : Unsigned;
      ScaleB : Integer;
      Minus  : Boolean;
      Scale  : Integer) return Int
   is
      function Safe_Expont
        (Base   : Int;
         Exp    : in out Natural;
         Factor : Int) return Int;
      --  Return (Base ** Exp) * Factor if the computation does not overflow,
      --  or else the number of the form (Base ** K) * Factor with the largest
      --  magnitude if the former computation overflows. In both cases, Exp is
      --  updated to contain the remaining power in the computation. Note that
      --  Factor is expected to be positive in this context.

      function Unsigned_To_Signed (Val : Uns) return Int;
      --  Convert an integer value from unsigned to signed representation

      -----------------
      -- Safe_Expont --
      -----------------

      function Safe_Expont
        (Base   : Int;
         Exp    : in out Natural;
         Factor : Int) return Int
      is
         pragma Assert (Base /= 0 and then Factor > 0);

         Max : constant Int := Int'Last / Base;

         Result : Int := Factor;

      begin
         while Exp > 0 and then Result <= Max loop
            Result := Result * Base;
            Exp    := Exp - 1;
         end loop;

         return Result;
      end Safe_Expont;

      ------------------------
      -- Unsigned_To_Signed --
      ------------------------

      function Unsigned_To_Signed (Val : Uns) return Int is
      begin
         --  Deal with overflow cases, and also with largest negative number

         if Val > Uns (Int'Last) then
            if Minus and then Val = Uns (-(Int'First)) then
               return Int'First;
            else
               Bad_Value (Str);
            end if;

         --  Negative values

         elsif Minus then
            return -(Int (Val));

         --  Positive values

         else
            return Int (Val);
         end if;
      end Unsigned_To_Signed;

   begin
      --  If the base of the value is 10 or its scaling factor is zero, then
      --  add the scales (they are defined in the opposite sense) and apply
      --  the result to the value, checking for overflow in the process.

      if Base = 10 or else ScaleB = 0 then
         declare
            S : Integer := ScaleB + Scale;
            V : Uns     := Val;

         begin
            while S < 0 loop
               V := V / 10;
               S := S + 1;
            end loop;

            while S > 0 loop
               if V <= Uns'Last / 10 then
                  V := V * 10;
                  S := S - 1;
               else
                  Bad_Value (Str);
               end if;
            end loop;

            return Unsigned_To_Signed (V);
         end;

      --  If the base of the value is not 10, use a scaled divide operation
      --  to compute Val * (Base ** ScaleB) * (10 ** Scale).

      else
         declare
            B : constant Int     := Int (Base);
            S : constant Integer := ScaleB;

            V : Uns := Val;

            Y, Z, Q, R : Int;

         begin
            --  If S is too negative, then drop trailing digits

            if S < 0 then
               declare
                  LS : Integer := -S;

               begin
                  Y := 10 ** Integer'Max (0, Scale);
                  Z := Safe_Expont (B, LS, 10 ** Integer'Max (0, -Scale));

                  for J in 1 .. LS loop
                     V := V / Uns (B);
                  end loop;
               end;

            --  If S is too positive, then scale V up, which may then overflow

            elsif S > 0 then
               declare
                  LS : Integer := S;

               begin
                  Y := Safe_Expont (B, LS, 10 ** Integer'Max (0, Scale));
                  Z := 10 ** Integer'Max (0, -Scale);

                  for J in 1 .. LS loop
                     if V <= Uns'Last / Uns (B) then
                        V := V * Uns (B);
                     else
                        Bad_Value (Str);
                     end if;
                  end loop;
               end;

            --  The case S equal to zero should have been handled earlier

            else
               raise Program_Error;
            end if;

            --  Perform a scale divide operation with rounding to match 'Image

            Scaled_Divide (Unsigned_To_Signed (V), Y, Z, Q, R, Round => True);

            return Q;
         end;
      end if;

   exception
      when Constraint_Error => Bad_Value (Str);
   end Integer_to_Decimal;

   ------------------
   -- Scan_Decimal --
   ------------------

   function Scan_Decimal
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Int
   is
      Base  : Unsigned;
      Scl   : Impl.Scale_Array;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Impl.Value_Array;

   begin
      Val := Impl.Scan_Raw_Real (Str, Ptr, Max, Base, Scl, Extra, Minus);

      return Integer_to_Decimal (Str, Val (1), Base, Scl (1), Minus, Scale);
   end Scan_Decimal;

   -------------------
   -- Value_Decimal --
   -------------------

   function Value_Decimal (Str : String; Scale : Integer) return Int is
      Base  : Unsigned;
      Scl   : Impl.Scale_Array;
      Extra : Unsigned;
      Minus : Boolean;
      Val   : Impl.Value_Array;

   begin
      Val := Impl.Value_Raw_Real (Str, Base, Scl, Extra, Minus);

      return Integer_to_Decimal (Str, Val (1), Base, Scl (1), Minus, Scale);
   end Value_Decimal;

end System.Value_D;
