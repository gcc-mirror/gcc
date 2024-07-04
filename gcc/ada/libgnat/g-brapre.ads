------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                G N A T . B R A N C H _ P R E D I C T I O N               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                         Copyright (C) 2019-2024, AdaCore                 --
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

--  This package provides routines giving hints to the branch predictor of the
--  code generator. These hints are useful when optimization is enabled and the
--  branch probability heuristics are used (which is the default), but they are
--  overridden when profile feedback-directed optimization is used instead.

--  The canonical pattern is to use them as the condition of an If statement:
--
--     if Likely (X > 0) then
--        Do_Something;
--     end if;
--
--  when it is not obvious that one outcome of the condition is more likely
--  than the other, or else to reverse the prediction made by the heuristics
--  in very peculiar cases. In the other cases, it is better not to use them,
--  because predicting how programs actually perform is notoriously hard.

package GNAT.Branch_Prediction is
   pragma Pure;

   function Expect (Condition : Boolean; Outcome : Boolean) return Boolean;
   pragma Import (Intrinsic, Expect, "__builtin_expect");
   --  This function returns the value of its first parameter Condition and
   --  tells the branch predictor that this value is expected to be Outcome.

   function Likely (Condition : Boolean) return Boolean;
   pragma Import (Intrinsic, Likely, "__builtin_likely");
   --  This function returns the value of its parameter Condition and tells
   --  the branch predictor that this value is expected to be True. Calling
   --  it is strictly equivalent to calling Expect with Outcome set to True.

   function Unlikely (Condition : Boolean) return Boolean;
   pragma Import (Intrinsic, Unlikely, "__builtin_unlikely");
   --  This function returns the value of its parameter Condition and tells
   --  the branch predictor that this value is expected to be False. Calling
   --  it is strictly equivalent to calling Expect with Outcome set to False.

end GNAT.Branch_Prediction;
