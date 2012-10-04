------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X N _ L L F                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

package body System.Exn_LLF is

   -------------------------
   -- Exn_Long_Long_Float --
   -------------------------

   function Exn_Long_Long_Float
     (Left  : Long_Long_Float;
      Right : Integer) return Long_Long_Float
   is
      Result : Long_Long_Float := 1.0;
      Factor : Long_Long_Float := Left;
      Exp    : Integer := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. If the low order bit or Exp is
      --  set, multiply the result by this factor. For negative exponents,
      --  invert result upon return.

      if Exp >= 0 then
         loop
            if Exp rem 2 /= 0 then
               Result := Result * Factor;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;
            Factor := Factor * Factor;
         end loop;

         return Result;

      --  Here we have a negative exponent, and we compute the result as:

      --     1.0 / (Left ** (-Right))

      --  Note that the case of Left being zero is not special, it will
      --  simply result in a division by zero at the end, yielding a
      --  correctly signed infinity, or possibly generating an overflow.

      --  Note on overflow: The coding of this routine assumes that the
      --  target generates infinities with standard IEEE semantics. If this
      --  is not the case, then the code below may raise Constraint_Error.
      --  This follows the implementation permission given in RM 4.5.6(12).

      else
         begin
            loop
               if Exp rem 2 /= 0 then
                  Result := Result * Factor;
               end if;

               Exp := Exp / 2;
               exit when Exp = 0;
               Factor := Factor * Factor;
            end loop;

            return 1.0 / Result;
         end;
      end if;
   end Exn_Long_Long_Float;

end System.Exn_LLF;
