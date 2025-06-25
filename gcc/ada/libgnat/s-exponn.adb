------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . E X P O N N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

package body System.Exponn
  with SPARK_Mode
is
   -----------
   -- Expon --
   -----------

   function Expon (Left : Int; Right : Natural) return Int is

      --  Note that negative exponents get a constraint error because the
      --  subtype of the Right argument (the exponent) is Natural.

      Result : Int     := 1;
      Factor : Int     := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: for compilation only, it is not worth special casing base
      --  values -1, 0, +1 since the expander does this when the base is a
      --  literal, and other cases will be extremely rare. But for proof,
      --  special casing zero in both positions makes ghost code and lemmas
      --  simpler, so we do it.

      if Right = 0 then
         Result := 1;
      elsif Left = 0 then
         Result := 0;
      else
         loop
            if Exp rem 2 /= 0 then
               declare
                  pragma Suppress (Overflow_Check);
               begin
                  Result := Result * Factor;
               end;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;

            declare
               pragma Suppress (Overflow_Check);
            begin
               Factor := Factor * Factor;
            end;
         end loop;
      end if;

      return Result;
   end Expon;

end System.Exponn;
