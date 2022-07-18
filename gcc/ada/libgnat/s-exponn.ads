------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . E X P O N N                         --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides functions for signed integer exponentiation. This
--  is the version of the package with checks disabled.

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

generic

   type Int is range <>;

package System.Exponn
  with Pure, SPARK_Mode
is
   --  Preconditions in this unit are meant for analysis only, not for run-time
   --  checking, so that the expected exceptions are raised. This is enforced
   --  by setting the corresponding assertion policy to Ignore. Postconditions
   --  and contract cases should not be executed at runtime as well, in order
   --  not to slow down the execution of these functions.

   pragma Assertion_Policy (Pre            => Ignore,
                            Post           => Ignore,
                            Contract_Cases => Ignore,
                            Ghost          => Ignore);

   package BI_Ghost renames Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
   subtype Big_Integer is BI_Ghost.Big_Integer with Ghost;
   use type BI_Ghost.Big_Integer;

   package Signed_Conversion is new BI_Ghost.Signed_Conversions (Int => Int);

   function Big (Arg : Int) return Big_Integer is
     (Signed_Conversion.To_Big_Integer (Arg))
   with Ghost;

   function In_Int_Range (Arg : Big_Integer) return Boolean is
     (BI_Ghost.In_Range (Arg, Big (Int'First), Big (Int'Last)))
   with Ghost;

   function Expon (Left : Int; Right : Natural) return Int
   with
     Pre  => In_Int_Range (Big (Left) ** Right),
     Post => Expon'Result = Left ** Right;
   --  Calculate ``Left`` ** ``Right``. If ``Left`` is 0 then 0 is returned
   --  and if ``Right`` is 0 then 1 is returned. In all other cases the result
   --  is set to 1 and then computed in a loop as follows:
   --  If ``Right`` is a multiple of 2 then multiply the result with ``Left``.
   --  Divide ``Right`` by 2.
   --  If ``Right is 0, return.
   --  Multiply ``Left`` with itself.

end System.Exponn;
