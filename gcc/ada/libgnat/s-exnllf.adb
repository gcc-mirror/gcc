------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X N _ L L F                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  Note: the reason for treating exponents in the range 0 .. 4 specially is
--  to ensure identical results to the static inline expansion in the case of
--  a compile time known exponent in this range. The use of Float'Machine and
--  Long_Float'Machine is to avoid unwanted extra precision in the results.

--  Note that for a negative exponent in Left ** Right, we compute the result
--  as:

--     1.0 / (Left ** (-Right))

--  Note that the case of Left being zero is not special, it will simply result
--  in a division by zero at the end, yielding a correctly signed infinity, or
--  possibly generating an overflow.

--  Note on overflow: This coding assumes that the target generates infinities
--  with standard IEEE semantics. If this is not the case, then the code
--  for negative exponent may raise Constraint_Error. This follows the
--  implementation permission given in RM 4.5.6(12).

package body System.Exn_LLF is

   subtype Negative is Integer range Integer'First .. -1;

   function Exp
     (Left  : Long_Long_Float;
      Right : Natural) return Long_Long_Float;
   --  Common routine used if Right is greater or equal to 5

   ---------------
   -- Exn_Float --
   ---------------

   function Exn_Float
     (Left  : Float;
      Right : Integer) return Float
   is
      Temp : Float;
   begin
      case Right is
         when 0 =>
            return 1.0;
         when 1 =>
            return Left;
         when 2 =>
            return Float'Machine (Left * Left);
         when 3 =>
            return Float'Machine (Left * Left * Left);
         when 4 =>
            Temp := Float'Machine (Left * Left);
            return Float'Machine (Temp * Temp);
         when Negative =>
            return Float'Machine (1.0 / Exn_Float (Left, -Right));
         when others =>
            return
              Float'Machine
                (Float (Exp (Long_Long_Float (Left), Right)));
      end case;
   end Exn_Float;

   --------------------
   -- Exn_Long_Float --
   --------------------

   function Exn_Long_Float
     (Left  : Long_Float;
      Right : Integer) return Long_Float
   is
      Temp : Long_Float;
   begin
      case Right is
         when 0 =>
            return 1.0;
         when 1 =>
            return Left;
         when 2 =>
            return Long_Float'Machine (Left * Left);
         when 3 =>
            return Long_Float'Machine (Left * Left * Left);
         when 4 =>
            Temp := Long_Float'Machine (Left * Left);
            return Long_Float'Machine (Temp * Temp);
         when Negative =>
            return Long_Float'Machine (1.0 / Exn_Long_Float (Left, -Right));
         when others =>
            return
              Long_Float'Machine
                (Long_Float (Exp (Long_Long_Float (Left), Right)));
      end case;
   end Exn_Long_Float;

   -------------------------
   -- Exn_Long_Long_Float --
   -------------------------

   function Exn_Long_Long_Float
     (Left  : Long_Long_Float;
      Right : Integer) return Long_Long_Float
   is
      Temp : Long_Long_Float;
   begin
      case Right is
         when 0 =>
            return 1.0;
         when 1 =>
            return Left;
         when 2 =>
            return Left * Left;
         when 3 =>
            return Left * Left * Left;
         when 4 =>
            Temp := Left * Left;
            return Temp * Temp;
         when Negative =>
            return 1.0 / Exn_Long_Long_Float (Left, -Right);
         when others =>
            return Exp (Left, Right);
      end case;
   end Exn_Long_Long_Float;

   ---------
   -- Exp --
   ---------

   function Exp
     (Left  : Long_Long_Float;
      Right : Natural) return Long_Long_Float
   is
      Result : Long_Long_Float := 1.0;
      Factor : Long_Long_Float := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. If the low order bit or Exp is
      --  set, multiply the result by this factor.

      loop
         if Exp rem 2 /= 0 then
            Result := Result * Factor;
         end if;

         Exp := Exp / 2;
         exit when Exp = 0;
         Factor := Factor * Factor;
      end loop;

      return Result;
   end Exp;

end System.Exn_LLF;
