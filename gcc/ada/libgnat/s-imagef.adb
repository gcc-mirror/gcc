------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ F                        --
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

with System.Image_I;
with System.Img_Util; use System.Img_Util;
with System.Value_I_Spec;
with System.Value_U_Spec;

package body System.Image_F is

   --  Contracts, ghost code, loop invariants and assertions in this unit are
   --  meant for analysis only, not for run-time checking, as it would be too
   --  costly otherwise. This is enforced by setting the assertion policy to
   --  Ignore.

   pragma Assertion_Policy (Assert             => Ignore,
                            Assert_And_Cut     => Ignore,
                            Contract_Cases     => Ignore,
                            Ghost              => Ignore,
                            Loop_Invariant     => Ignore,
                            Pre                => Ignore,
                            Post               => Ignore,
                            Subprogram_Variant => Ignore);

   Maxdigs : constant Natural := Int'Width - 2;
   --  Maximum number of decimal digits that can be represented in an Int.
   --  The "-2" accounts for the sign and one extra digit, since we need the
   --  maximum number of 9's that can be represented, e.g. for the 64-bit case,
   --  Integer_64'Width is 20 since the maximum value is approximately 9.2E+18
   --  and has 19 digits, but the maximum number of 9's that can be represented
   --  in Integer_64 is only 18.

   --  The first prerequisite of the implementation is that the first scaled
   --  divide does not overflow, which means that the absolute value of the
   --  input X must always be smaller than 10**Maxdigs * 2**(Int'Size - 1).
   --  Otherwise Constraint_Error is raised by the scaled divide operation.

   --  The second prerequisite of the implementation is that the computation
   --  of the operands does not overflow when the small is neither an integer
   --  nor the reciprocal of an integer, which means that its numerator and
   --  denominator must be smaller than 10**(2*Maxdigs-1) / 2**(Int'Size - 1)
   --  if the small is larger than 1, and smaller than 2**(Int'Size - 1) / 10
   --  if the small is smaller than 1.

   Unsigned_Width_Ghost : constant Natural := Int'Width;

   package Uns_Spec is new System.Value_U_Spec (Uns);
   package Int_Spec is new System.Value_I_Spec (Int, Uns, Uns_Spec.Uns_Params);

   package Image_I is new System.Image_I
     (Int                  => Int,
      Uns                  => Uns,
      Unsigned_Width_Ghost => Unsigned_Width_Ghost,
      Int_Params           => Int_Spec.Int_Params);

   procedure Set_Image_Integer
     (V : Int;
      S : in out String;
      P : in out Natural)
     renames Image_I.Set_Image_Integer;

   --  The following section describes a specific implementation choice for
   --  performing base conversions needed for output of values of a fixed
   --  point type T with small T'Small. The goal is to be able to output
   --  all values of fixed point types with a precision of 64 bits and a
   --  small in the range 2.0**(-63) .. 2.0**63. The reasoning can easily
   --  be adapted to fixed point types with a precision of 32 or 128 bits.

   --  The chosen algorithm uses fixed precision integer arithmetic for
   --  reasons of simplicity and efficiency. It is important to understand
   --  in what ways the most simple and accurate approach to fixed point I/O
   --  is limiting, before considering more complicated schemes.

   --  Without loss of generality assume T has a range (-2.0**63) * T'Small
   --  .. (2.0**63 - 1) * T'Small, and is output with Aft digits after the
   --  decimal point and T'Fore - 1 before. If T'Small is integer, or
   --  1.0 / T'Small is integer, let S = T'Small.

   --  The idea is to convert a value X * S of type T to a 64-bit integer value
   --  Q equal to 10.0**D * (X * S) rounded to the nearest integer, using only
   --  a scaled integer divide of the form

   --     Q = (X * Y) / Z,

   --  where the variables X, Y, Z are 64-bit integers, and both multiplication
   --  and division are done using full intermediate precision. Then the final
   --  decimal value to be output is

   --     Q * 10**(-D)

   --  This value can be written to the output file or to the result string
   --  according to the format described in RM A.3.10. The details of this
   --  operation are omitted here.

   --  A 64-bit value can represent all integers with 18 decimal digits, but
   --  not all with 19 decimal digits. If the total number of requested output
   --  digits (Fore - 1) + Aft is greater than 18 then, for purposes of the
   --  conversion, Aft is adjusted to 18 - (Fore - 1). In that case, trailing
   --  zeros can complete the output after writing the first 18 significant
   --  digits, or the technique described in the next section can be used.
   --  In addition, D cannot be smaller than -18, in order for 10.0**(-D) to
   --  fit in a 64-bit integer.

   --  The final expression for D is

   --     D = Integer'Max (-18, Integer'Min (Aft, 18 - (Fore - 1)));

   --  For Y and Z the following expressions can be derived:

   --     Q = X * S * (10.0**D) = (X * Y) / Z

   --  If S is an integer greater than or equal to one, then Fore must be at
   --  least 20 in order to print T'First, which is at most -2.0**63. This
   --  means that D < 0, so use

   --    (1)   Y = -S and Z = -10**(-D)

   --  If 1.0 / S is an integer greater than one, use

   --    (2)   Y = -10**D and Z = -(1.0 / S), for D >= 0

   --  or

   --    (3)   Y = -1 and Z = -(1.0 / S) * 10**(-D), for D < 0

   --  Negative values are used for nominator Y and denominator Z, so that S
   --  can have a maximum value of 2.0**63 and a minimum of 2.0**(-63). For
   --  -(1.0 / S) in -1 .. -9, Fore will still be 20, and D will be negative,
   --  as (-2.0**63) / -9 is greater than 10**18. In these cases there is room
   --  in the denominator for the extra decimal scaling required, so case (3)
   --  will not overflow.

   --  In fact this reasoning can be generalized to most S which are the ratio
   --  of two integers with bounded magnitude. Let S = Num / Den and rewrite
   --  case (1) above where Den = 1 into

   --    (1b)   Y = -Num and Z = -Den * 10**(-D)

   --  Suppose that Num corresponds to the maximum value of -D, i.e. 18 and
   --  10**(-D) = 10**18. If you change Den into 10, then S becomes 10 times
   --  smaller and, therefore, Fore is decremented by 1, which means that -D
   --  is as well, provided that Fore was initially not larger than 37, so the
   --  multiplication for Z still does not overflow. But you need to reach 10
   --  to trigger this effect, which means that a leeway of 10 is required, so
   --  let's restrict this to a Num for which 10**(-D) <= 10**17. To summarize
   --  this case, if S is the ratio of two integers with

   --    Den < Num <= B1

   --  where B1 is a fixed limit, then case (1b) does not overflow. B1 can be
   --  taken as the largest integer Small such that D = -17, i.e. Fore = 36,
   --  which means that B1 * 2.0**63 must be smaller than 10**35.

   --  Let's continue and rewrite case (2) above when Num = 1 into

   --    (2b)   Y = -Num * 10**D and Z = -Den, for D >= 0

   --  Note that D <= 18 - (Fore - 1) and Fore >= 2 so D <= 17, thus you can
   --  safely change Num into 10 in the product, but then S becomes 10 times
   --  larger and, therefore, Fore is incremented by 1, which means that D is
   --  decremented by 1 so you again have a product lesser or equal to 10**17.
   --  To sum up, if S is the ratio of two integers with

   --    Num <= Den * S0

   --  where S0 is the largest Small such that D >= 0, then case (2b) does not
   --  overflow.

   --  Let's conclude and rewrite case (3) above when Num = 1 into

   --    (3b)   Y = -Num and Z = -Den * 10**(-D), for D < 0

   --  As explained above, this occurs only if both S0 < S < 1 and D = -1 and
   --  is preserved if you scale up Num and Den simultaneously, what you can
   --  do until Den * 10 tops the upper bound. To sum up, if S is the ratio of
   --  two integers with

   --    Den * S0 < Num < Den <= B2

   --  where B2 is a fixed limit, then case (3b) does not overflow. B2 can be
   --  taken as the largest integer such that B2 * 10 is smaller than 2.0**63.

   --  The conclusion is that the algorithm works if the small is the ratio of
   --  two integers in the range 1 .. 2**63 if either is equal to 1, or of two
   --  integers in the range 1 .. B1 if the small is larger than 1, or of two
   --  integers in the range 1 .. B2 if the small is smaller than 1.

   --  Using a scaled divide which truncates and returns a remainder R,
   --  another K trailing digits can be calculated by computing the value
   --  (R * (10.0**K)) / Z using another scaled divide. This procedure
   --  can be repeated to compute an arbitrary number of digits in linear
   --  time and storage. The last scaled divide should be rounded, with
   --  a possible carry propagating to the more significant digits, to
   --  ensure correct rounding of the unit in the last place.

   -----------------
   -- Image_Fixed --
   -----------------

   procedure Image_Fixed
     (V    : Int;
      S    : in out String;
      P    : out Natural;
      Num  : Int;
      Den  : Int;
      For0 : Natural;
      Aft0 : Natural)
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

      Set_Image_Fixed (V, S, P, Num, Den, For0, Aft0, 1, Aft0, 0);
   end Image_Fixed;

   ---------------------
   -- Set_Image_Fixed --
   ---------------------

   procedure Set_Image_Fixed
     (V    : Int;
      S    : in out String;
      P    : in out Natural;
      Num  : Int;
      Den  : Int;
      For0 : Natural;
      Aft0 : Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
   is
      pragma Assert (Num < 0 and then Den < 0);
      --  Accept only negative numbers to allow -2**(Int'Size - 1)

      A : constant Natural :=
            Boolean'Pos (Exp > 0) * Aft0 + Natural'Max (Aft, 1) + 1;
      --  Number of digits after the decimal point to be computed. If Exp is
      --  positive, we need to compute Aft decimal digits after the first non
      --  zero digit and we are guaranteed there is at least one in the first
      --  Aft0 digits (unless V is zero). In both cases, we compute one more
      --  digit than requested so that Set_Decimal_Digits can round at Aft.

      D : constant Integer :=
            Integer'Max (-Maxdigs, Integer'Min (A, Maxdigs - (For0 - 1)));
      Y : constant Int     := Num * 10**Integer'Max (0, D);
      Z : constant Int     := Den * 10**Integer'Max (0, -D);
      --  See the description of the algorithm above

      AF : constant Natural := A - D;
      --  Number of remaining digits to be computed after the first round. It
      --  is larger than A if the first round does not compute all the digits
      --  before the decimal point, i.e. (For0 - 1) larger than Maxdigs.

      N : constant Natural := 1 + (AF + Maxdigs - 1) / Maxdigs;
      --  Number of rounds of scaled divide to be performed

      Q : Int;
      --  Quotient of the scaled divide in this round. Only the first round may
      --  yield more than Maxdigs digits and has a significant sign.

      Buf : String (1 .. Maxdigs);
      Len : Natural;
      --  Buffer for the image of the quotient

      Digs  : String (1 .. 2 + N * Maxdigs);
      Ndigs : Natural;
      --  Concatenated image of the successive quotients

      Scale : Integer := 0;
      --  Exponent such that the result is Digs (1 .. NDigs) * 10**(-Scale)

      XX : Int := V;
      YY : Int := Y;
      --  First two operands of the scaled divide

   begin
      --  Set the first character like Image

      if V >= 0 then
         Digs (1) := ' ';
         Ndigs := 1;
      else
         Ndigs := 0;
      end if;

      for J in 1 .. N loop
         exit when XX = 0;

         Scaled_Divide (XX, YY, Z, Q, R => XX, Round => False);

         if J = 1 then
            if Q /= 0 then
               Set_Image_Integer (Q, Digs, Ndigs);
            end if;

            Scale := Scale + D;

            --  Prepare for next round, if any

            YY := 10**Maxdigs;

         else
            pragma Assert (-10**Maxdigs < Q and then Q < 10**Maxdigs);

            Len := 0;
            Set_Image_Integer (abs Q, Buf, Len);

            pragma Assert (1 <= Len and then Len <= Maxdigs);

            --  If no character but the space has been written, write the
            --  minus if need be, since Set_Image_Integer did not do it.

            if Ndigs <= 1 then
               if Q /= 0 then
                  if Ndigs = 0 then
                     Digs (1) := '-';
                  end if;

                  Digs (2 .. Len + 1) := Buf (1 .. Len);
                  Ndigs := Len + 1;
               end if;

            --  Or else pad the output with zeroes up to Maxdigs

            else
               for K in 1 .. Maxdigs - Len loop
                  Digs (Ndigs + K) := '0';
               end loop;

               for K in 1 .. Len loop
                  Digs (Ndigs + Maxdigs - Len + K) := Buf (K);
               end loop;

               Ndigs := Ndigs + Maxdigs;
            end if;

            Scale := Scale + Maxdigs;
         end if;
      end loop;

      --  If no digit was output, this is zero

      if Ndigs <= 1 then
         Digs (1 .. 2) := " 0";
         Ndigs := 2;
      end if;
      pragma Annotate (CodePeer, False_Positive, "test always true",
                       "no digits were output for zero");

      Set_Decimal_Digits (Digs, Ndigs, S, P, Scale, Fore, Aft, Exp);
   end Set_Image_Fixed;

end System.Image_F;
