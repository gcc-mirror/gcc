------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . E X P O N R                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2021-2024, Free Software Foundation, Inc.       --
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

--  Note that the reason for treating exponents in the range 0 .. 4 specially
--  is to ensure identical results with the static expansion in the case of a
--  compile-time known exponent in this range; similarly, the use 'Machine is
--  to avoid unwanted extra precision in the results.

--  For a negative exponent, we compute the result as per RM 4.5.6(11/3):

--     Left ** Right = 1.0 / (Left ** (-Right))

--  Note that the case of Left being zero is not special, it will simply result
--  in a division by zero at the end, yielding a correctly signed infinity, or
--  possibly raising an overflow exception.

--  Note on overflow: this coding assumes that the target generates infinities
--  with standard IEEE semantics. If this is not the case, then the code for
--  negative exponents may raise Constraint_Error, which is in keeping with the
--  implementation permission given in RM 4.5.6(12).

with System.Double_Real;

function System.Exponr (Left : Num; Right : Integer) return Num is

   package Double_Real is new System.Double_Real (Num);
   use type Double_Real.Double_T;

   subtype Double_T is Double_Real.Double_T;
   --  The double floating-point type

   subtype Safe_Negative is Integer range Integer'First + 1 .. -1;
   --  The range of safe negative exponents

   function Expon (Left : Num; Right : Natural) return Num;
   --  Routine used if Right is greater than 4

   -----------
   -- Expon --
   -----------

   function Expon (Left : Num; Right : Natural) return Num is
      Result : Double_T := Double_Real.To_Double (1.0);
      Factor : Double_T := Double_Real.To_Double (Left);
      Exp    : Natural  := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. If the low order bit or Exp
      --  is set, multiply the result by this factor.

      loop
         if Exp rem 2 /= 0 then
            Result := Result * Factor;
            exit when Exp = 1;
         end if;

         Exp := Exp / 2;
         Factor := Double_Real.Sqr (Factor);
      end loop;

      return Double_Real.To_Single (Result);
   end Expon;

begin
   case Right is
      when 0 =>
         return 1.0;

      when 1 =>
         return Left;

      when 2 =>
         return Num'Machine (Left * Left);

      when 3 =>
         return Num'Machine (Left * Left * Left);

      when 4 =>
         declare
            Sqr : constant Num := Num'Machine (Left * Left);

         begin
            return Num'Machine (Sqr * Sqr);
         end;

      when Safe_Negative =>
         return Num'Machine (1.0 / Exponr (Left, -Right));

      when Integer'First =>
         return Num'Machine (1.0 / (Exponr (Left, Integer'Last) * Left));

      when others =>
         return Num'Machine (Expon (Left, Right));
   end case;
end System.Exponr;
