------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ B O O L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with System.Val_Util; use System.Val_Util;

package body System.Val_Bool
  with SPARK_Mode
is

   -------------------
   -- Value_Boolean --
   -------------------

   function Value_Boolean (Str : String) return Boolean is
      F : Integer;
      L : Integer;
      S : String (Str'Range) := Str;

   begin
      Normalize_String (S, F, L);

      pragma Assert (F = System.Val_Spec.First_Non_Space_Ghost
                     (S, Str'First, Str'Last));

      if S (F .. L) = "TRUE" then
         return True;

      elsif S (F .. L) = "FALSE" then
         return False;

      else
         Bad_Value (Str);
      end if;
   end Value_Boolean;

end System.Val_Bool;
