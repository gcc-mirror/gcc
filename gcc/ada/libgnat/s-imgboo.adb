------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . I M G _ B O O L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2024, Free Software Foundation, Inc.        --
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

package body System.Img_Bool
  with SPARK_Mode
is

   --  Local lemmas

   procedure Lemma_Is_First_Non_Space_Ghost (S : String; R : Positive) with
     Ghost,
     Pre => R in S'Range and then S (R) /= ' '
       and then System.Val_Spec.Only_Space_Ghost (S, S'First, R - 1),
     Post => System.Val_Spec.First_Non_Space_Ghost (S, S'First, S'Last) = R;

   ------------------------------------
   -- Lemma_Is_First_Non_Space_Ghost --
   ------------------------------------

   procedure Lemma_Is_First_Non_Space_Ghost (S : String; R : Positive) is null;

   -------------------
   -- Image_Boolean --
   -------------------

   procedure Image_Boolean
     (V : Boolean;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);
   begin
      if V then
         S (1 .. 4) := "TRUE";
         P := 4;
         Lemma_Is_First_Non_Space_Ghost (S, 1);
      else
         S (1 .. 5) := "FALSE";
         P := 5;
         Lemma_Is_First_Non_Space_Ghost (S, 1);
      end if;
   end Image_Boolean;

end System.Img_Bool;
