------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 B o d y                                  --
--                          (Apple OS X Version)                            --
--                                                                          --
--          Copyright (C) 1998-2009, Free Software Foundation, Inc.         --
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

--  File a-numaux.adb <- a-numaux-darwin.adb

package body Ada.Numerics.Aux is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Reduce (X : in out Double; Q : out Natural);
   --  Implements reduction of X by Pi/2. Q is the quadrant of the final
   --  result in the range 0 .. 3. The absolute value of X is at most Pi/4.

   --  The following three functions implement Chebishev approximations
   --  of the trigonometric functions in their reduced domain.
   --  These approximations have been computed using Maple.

   function Sine_Approx (X : Double) return Double;
   function Cosine_Approx (X : Double) return Double;

   pragma Inline (Reduce);
   pragma Inline (Sine_Approx);
   pragma Inline (Cosine_Approx);

   function Cosine_Approx (X : Double) return Double is
      XX : constant Double := X * X;
   begin
      return (((((16#8.DC57FBD05F640#E-08 * XX
              - 16#4.9F7D00BF25D80#E-06) * XX
              + 16#1.A019F7FDEFCC2#E-04) * XX
              - 16#5.B05B058F18B20#E-03) * XX
              + 16#A.AAAAAAAA73FA8#E-02) * XX
              - 16#7.FFFFFFFFFFDE4#E-01) * XX
              - 16#3.655E64869ECCE#E-14 + 1.0;
   end Cosine_Approx;

   function Sine_Approx (X : Double) return Double is
      XX : constant Double := X * X;
   begin
      return (((((16#A.EA2D4ABE41808#E-09 * XX
              - 16#6.B974C10F9D078#E-07) * XX
              + 16#2.E3BC673425B0E#E-05) * XX
              - 16#D.00D00CCA7AF00#E-04) * XX
              + 16#2.222222221B190#E-02) * XX
              - 16#2.AAAAAAAAAAA44#E-01) * (XX * X) + X;
   end Sine_Approx;

   ------------
   -- Reduce --
   ------------

   procedure Reduce (X : in out Double; Q : out Natural) is
      Half_Pi     : constant := Pi / 2.0;
      Two_Over_Pi : constant := 2.0 / Pi;

      HM : constant := Integer'Min (Double'Machine_Mantissa / 2, Natural'Size);
      M  : constant Double := 0.5 + 2.0**(1 - HM); -- Splitting constant
      P1 : constant Double := Double'Leading_Part (Half_Pi, HM);
      P2 : constant Double := Double'Leading_Part (Half_Pi - P1, HM);
      P3 : constant Double := Double'Leading_Part (Half_Pi - P1 - P2, HM);
      P4 : constant Double := Double'Leading_Part (Half_Pi - P1 - P2 - P3, HM);
      P5 : constant Double := Double'Leading_Part (Half_Pi - P1 - P2 - P3
                                                                 - P4, HM);
      P6 : constant Double := Double'Model (Half_Pi - P1 - P2 - P3 - P4 - P5);
      K  : Double;

   begin
      --  For X < 2.0**HM, all products below are computed exactly.
      --  Due to cancellation effects all subtractions are exact as well.
      --  As no double extended floating-point number has more than 75
      --  zeros after the binary point, the result will be the correctly
      --  rounded result of X - K * (Pi / 2.0).

      K := X * Two_Over_Pi;
      while abs K >= 2.0 ** HM loop
         K := K * M - (K * M - K);
         X :=
           (((((X - K * P1) - K * P2) - K * P3) - K * P4) - K * P5) - K * P6;
         K := X * Two_Over_Pi;
      end loop;

      --  If K is not a number (because X was not finite) raise exception

      if K /= K then
         raise Constraint_Error;
      end if;

      K := Double'Rounding (K);
      Q := Integer (K) mod 4;
      X := (((((X - K * P1) - K * P2) - K * P3)
                  - K * P4) - K * P5) - K * P6;
   end Reduce;

   ---------
   -- Cos --
   ---------

   function Cos (X : Double) return Double is
      Reduced_X : Double := abs X;
      Quadrant  : Natural range 0 .. 3;

   begin
      if Reduced_X > Pi / 4.0 then
         Reduce (Reduced_X, Quadrant);

         case Quadrant is
            when 0 =>
               return Cosine_Approx (Reduced_X);

            when 1 =>
               return Sine_Approx (-Reduced_X);

            when 2 =>
               return -Cosine_Approx (Reduced_X);

            when 3 =>
               return Sine_Approx (Reduced_X);
         end case;
      end if;

      return Cosine_Approx (Reduced_X);
   end Cos;

   ---------
   -- Sin --
   ---------

   function Sin (X : Double) return Double is
      Reduced_X : Double := X;
      Quadrant  : Natural range 0 .. 3;

   begin
      if abs X > Pi / 4.0 then
         Reduce (Reduced_X, Quadrant);

         case Quadrant is
            when 0 =>
               return Sine_Approx (Reduced_X);

            when 1 =>
               return Cosine_Approx (Reduced_X);

            when 2 =>
               return Sine_Approx (-Reduced_X);

            when 3 =>
               return -Cosine_Approx (Reduced_X);
         end case;
      end if;

      return Sine_Approx (Reduced_X);
   end Sin;

end Ada.Numerics.Aux;
