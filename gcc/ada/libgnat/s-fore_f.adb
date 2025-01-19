------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . F O R E _ F                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2025, Free Software Foundation, Inc.       --
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

package body System.Fore_F is

   Maxdigs : constant Natural := Int'Width - 2;
   --  Maximum number of decimal digits that can be represented in an Int.
   --  The "-2" accounts for the sign and one extra digit, since we need the
   --  maximum number of 9's that can be represented, e.g. for the 64-bit case,
   --  Integer_64'Width is 20 since the maximum value is approximately 9.2E+18
   --  and has 19 digits, but the maximum number of 9's that can be represented
   --  in Integer_64 is only 18.

   --  The first prerequisite of the implementation is that the scaled divide
   --  does not overflow, which means that the absolute value of the bounds of
   --  the subtype must be smaller than 10**Maxdigs * 2**(Int'Size - 1).
   --  Otherwise Constraint_Error is raised by the scaled divide operation.

   --  The second prerequisite is that the computation of the operands does not
   --  overflow, which means that, if the small is larger than 1, it is either
   --  an integer or its numerator and denominator must be both smaller than
   --  the power 10**(Maxdigs - 1).

   ----------------
   -- Fore_Fixed --
   ----------------

   function Fore_Fixed (Lo, Hi, Num, Den : Int; Scale : Integer) return Natural
   is
      pragma Assert (Num < 0 and then Den < 0);
      --  Accept only negative numbers to allow -2**(Int'Size - 1)

      function Negative_Abs (Val : Int) return Int is
        (if Val <= 0 then Val else -Val);
      --  Return the opposite of the absolute value of Val

      T : Int := Int'Min (Negative_Abs (Lo), Negative_Abs (Hi));
      F : Natural;

      Q, R : Int;

   begin
      --  Initial value of 2 allows for sign and mandatory single digit

      F := 2;

      --  The easy case is when Num is not larger than Den in magnitude,
      --  i.e. if S = Num / Den, then S <= 1, in which case we can just
      --  compute the product Q = T * S.

      if Num >= Den then
         Scaled_Divide (T, Num, Den, Q, R, Round => False);
         T := Q;

      --  Otherwise S > 1 and thus Scale <= 0, compute Q and R such that

      --    T * Num = Q * (Den * 10**(-D)) + R

      --  with

      --    D = Integer'Max (-Maxdigs, Scale - 1)

      --  then reason on Q if it is non-zero or else on R / Den.

      --  This works only if Den * 10**(-D) does not overflow, which is true
      --  if Den = 1. Suppose that Num corresponds to the maximum value of -D,
      --  i.e. Maxdigs and 10**(-D) = 10**Maxdigs. If you change Den into 10,
      --  then S becomes 10 times smaller and, therefore, Scale is incremented
      --  by 1, which means that -D is decremented by 1 provided that Scale was
      --  initially not smaller than 1 - Maxdigs, so the multiplication still
      --  does not overflow. But you need to reach 10 to trigger this effect,
      --  which means that a leeway of 10 is required, so let's restrict this
      --  to a Num for which 10**(-D) <= 10**(Maxdigs - 1). To sum up, if S is
      --  the ratio of two integers with

      --    1 < Den < Num <= B

      --  where B is a fixed limit, then the multiplication does not overflow.
      --  B can be taken as the largest integer Small such that D = 1 - Maxdigs
      --  i.e. such that Scale = 2 - Maxdigs, which is 10**(Maxdigs - 1) - 1.

      else
         declare
            D : constant Integer := Integer'Max (-Maxdigs, Scale - 1);

         begin
            Scaled_Divide (T, Num, Den * 10**(-D), Q, R, Round => False);

            if Q /= 0 then
               T := Q;
               F := F - D;
            else
               T := R / Den;
            end if;
         end;
      end if;

      --  Loop to increase Fore as needed to include full range of values

      while T <= -10 or else T >= 10 loop
         T := T / 10;
         F := F + 1;
      end loop;

      return F;
   end Fore_Fixed;

end System.Fore_F;
