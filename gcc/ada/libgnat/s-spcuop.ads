------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . S P A R K . C U T _ O P E R A T I O N S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2022-2025, Free Software Foundation, Inc.      --
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

--  This package provides connectors used to manually help the proof of
--  assertions by introducing intermediate steps. They can only be used inside
--  pragmas Assert or Assert_And_Cut.

package System.SPARK.Cut_Operations with
  SPARK_Mode,
  Pure,
  Always_Terminates
is

   function By (Consequence, Premise : Boolean) return Boolean with
     Ghost,
     Global => null;
   --  If A and B are two boolean expressions, proving By (A, B) requires
   --  proving B, the premise, and then A assuming B, the side-condition. When
   --  By (A, B) is assumed on the other hand, we only assume A. B is used
   --  for the proof, but is not visible afterward.

   function So (Premise, Consequence : Boolean) return Boolean with
     Ghost,
     Global => null;
   --  If A and B are two boolean expressions, proving So (A, B) requires
   --  proving A, the premise, and then B assuming A, the side-condition. When
   --  So (A, B) is assumed both A and B are assumed to be true.

end System.SPARK.Cut_Operations;
