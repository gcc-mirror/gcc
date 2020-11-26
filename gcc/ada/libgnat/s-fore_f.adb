------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . F O R E _ F                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

   --  The prerequisite of the implementation is that the scaled divide does
   --  not overflow, which means that the absolute value of the bounds of
   --  the subtype must be smaller than 10**Maxdigs * 2**(Int'Size - 1).
   --  Otherwise Constraint_Error is raised by the scaled divide operation.

   ----------------
   -- Fore_Fixed --
   ----------------

   function Fore_Fixed (Lo, Hi, Num, Den : Int) return Natural is
      pragma Assert (Num < 0 and then Den < 0);
      --  Accept only negative numbers to allow -2**(Int'Size - 1)

      function Negative_Abs (Val : Int) return Int is
        (if Val <= 0 then Val else -Val);
      --  Return the opposite of the absolute value of Val

      T : Int := Int'Min (Negative_Abs (Lo), Negative_Abs (Hi));
      F : Natural;

   begin
      --  Initial value of 2 allows for sign and mandatory single digit

      F := 2;

      --  If the Small is 1, then no scaling is needed

      if Num = -1 and then Den = -1 then
         null;

      --  The easy case is when the Small is the reciprocal of an integer

      elsif Num = -1 then
         T := T / Den;

      --  If the Small is an integer, compute Q and R such that

      --    T * Small = Q * 10**Maxdigs - R

      --  then reason on Q if it is non-zero or else on R.

      else pragma Assert (Den = -1);
         declare
            Q, R : Int;

         begin
            Scaled_Divide (T, Num, -10**Maxdigs, Q, R, Round => False);

            if Q /= 0 then
               T := Q;
               F := F + Maxdigs;
            else
               T := R;
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
