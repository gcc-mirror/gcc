------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . A R I T H _ 3 2                       --
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

with Ada.Unchecked_Conversion;

package body System.Arith_32 is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   subtype Uns32 is Interfaces.Unsigned_32;
   subtype Uns64 is Interfaces.Unsigned_64;

   use Interfaces;

   function To_Int is new Ada.Unchecked_Conversion (Uns32, Int32);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "abs" (X : Int32) return Uns32 is
     (if X = Int32'First
      then 2**31
      else Uns32 (Int32'(abs X)));
   --  Convert absolute value of X to unsigned. Note that we can't just use
   --  the expression of the Else since it overflows for X = Int32'First.

   function Hi (A : Uns64) return Uns32 is (Uns32 (Shift_Right (A, 32)));
   --  High order half of 64-bit value

   function To_Neg_Int (A : Uns32) return Int32;
   --  Convert to negative integer equivalent. If the input is in the range
   --  0 .. 2**31, then the corresponding nonpositive signed integer (obtained
   --  by negating the given value) is returned, otherwise constraint error is
   --  raised.

   function To_Pos_Int (A : Uns32) return Int32;
   --  Convert to positive integer equivalent. If the input is in the range
   --  0 .. 2**31 - 1, then the corresponding nonnegative signed integer is
   --  returned, otherwise constraint error is raised.

   procedure Raise_Error;
   pragma No_Return (Raise_Error);
   --  Raise constraint error with appropriate message

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error is
   begin
      raise Constraint_Error with "32-bit arithmetic overflow";
   end Raise_Error;

   -------------------
   -- Scaled_Divide --
   -------------------

   procedure Scaled_Divide32
     (X, Y, Z : Int32;
      Q, R    : out Int32;
      Round   : Boolean)
   is
      Xu  : constant Uns32 := abs X;
      Yu  : constant Uns32 := abs Y;
      Zu  : constant Uns32 := abs Z;

      D   : Uns64;
      --  The dividend

      Qu : Uns32;
      Ru : Uns32;
      --  Unsigned quotient and remainder

   begin
      --  First do the 64-bit multiplication

      D := Uns64 (Xu) * Uns64 (Yu);

      --  If dividend is too large, raise error

      if Hi (D) >= Zu then
         Raise_Error;

      --  Then do the 64-bit division

      else
         Qu := Uns32 (D / Uns64 (Zu));
         Ru := Uns32 (D rem Uns64 (Zu));
      end if;

      --  Deal with rounding case

      if Round and then Ru > (Zu - Uns32'(1)) / Uns32'(2) then

         --  Protect against wrapping around when rounding, by signaling
         --  an overflow when the quotient is too large.

         if Qu = Uns32'Last then
            Raise_Error;
         end if;

         Qu := Qu + Uns32'(1);
      end if;

      --  Set final signs (RM 4.5.5(27-30))

      --  Case of dividend (X * Y) sign positive

      if (X >= 0 and then Y >= 0) or else (X < 0 and then Y < 0) then
         R := To_Pos_Int (Ru);
         Q := (if Z > 0 then To_Pos_Int (Qu) else To_Neg_Int (Qu));

      --  Case of dividend (X * Y) sign negative

      else
         R := To_Neg_Int (Ru);
         Q := (if Z > 0 then To_Neg_Int (Qu) else To_Pos_Int (Qu));
      end if;
   end Scaled_Divide32;

   ----------------
   -- To_Neg_Int --
   ----------------

   function To_Neg_Int (A : Uns32) return Int32 is
      R : constant Int32 :=
        (if A = 2**31 then Int32'First else -To_Int (A));
      --  Note that we can't just use the expression of the Else, because it
      --  overflows for A = 2**31.
   begin
      if R <= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Neg_Int;

   ----------------
   -- To_Pos_Int --
   ----------------

   function To_Pos_Int (A : Uns32) return Int32 is
      R : constant Int32 := To_Int (A);
   begin
      if R >= 0 then
         return R;
      else
         Raise_Error;
      end if;
   end To_Pos_Int;

end System.Arith_32;
