------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ S P E C                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2023-2025, Free Software Foundation, Inc.         --
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

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

package body System.Val_Spec
  with SPARK_Mode
is

   ---------------------------
   -- First_Non_Space_Ghost --
   ---------------------------

   function First_Non_Space_Ghost
     (S        : String;
      From, To : Integer) return Positive
   is
   begin
      for J in From .. To loop
         if S (J) /= ' ' then
            return J;
         end if;

         pragma Loop_Invariant (for all K in From .. J => S (K) = ' ');
      end loop;

      raise Program_Error;
   end First_Non_Space_Ghost;

   -----------------------
   -- Last_Number_Ghost --
   -----------------------

   function Last_Number_Ghost (Str : String) return Positive is
   begin
      pragma Annotate (Gnatcheck, Exempt_On, "Improper_Returns",
                       "occurs in ghost code, not executable");

      for J in Str'Range loop
         if Str (J) not in '0' .. '9' | '_' then
            return J - 1;
         end if;

         pragma Loop_Invariant
           (for all K in Str'First .. J => Str (K) in '0' .. '9' | '_');
      end loop;

      return Str'Last;

      pragma Annotate (Gnatcheck, Exempt_Off, "Improper_Returns");
   end Last_Number_Ghost;

end System.Val_Spec;
