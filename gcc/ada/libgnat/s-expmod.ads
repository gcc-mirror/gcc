------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X P _ M O D                        --
--                                                                          --
--                                 S p e c                                  --
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

--  This function performs exponentiation of a modular type with nonbinary
--  modulus values. Arithmetic is done in Long_Long_Unsigned, with explicit
--  accounting for the modulus value which is passed as the second argument.
--
--  Note that 1 is a binary modulus (2**0), so the compiler should not (and
--  will not) call this function with Modulus equal to 1.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre            => Ignore,
                         Post           => Ignore,
                         Contract_Cases => Ignore,
                         Ghost          => Ignore);

with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

with System.Unsigned_Types;

package System.Exp_Mod
  with Pure, SPARK_Mode
is
   use type System.Unsigned_Types.Unsigned;
   subtype Unsigned is System.Unsigned_Types.Unsigned;

   use type Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Big_Integer;
   subtype Big_Integer is
     Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Big_Integer
   with Ghost;

   package Unsigned_Conversion is
     new Ada.Numerics.Big_Numbers.Big_Integers_Ghost.Unsigned_Conversions
       (Int => Unsigned);

   function Big (Arg : Unsigned) return Big_Integer is
     (Unsigned_Conversion.To_Big_Integer (Arg))
   with Ghost;

   subtype Power_Of_2 is Unsigned with
     Dynamic_Predicate =>
        Power_Of_2 /= 0 and then (Power_Of_2 and (Power_Of_2 - 1)) = 0;

   function Exp_Modular
     (Left    : Unsigned;
      Modulus : Unsigned;
      Right   : Natural) return Unsigned
   with
     Pre  => Modulus /= 0 and then Modulus not in Power_Of_2,
     Post => Big (Exp_Modular'Result) = Big (Left) ** Right mod Big (Modulus);
   --  Return the power of ``Left`` by ``Right` modulo ``Modulus``.
   --
   --  This function is implemented using the standard logarithmic approach:
   --  ``Right`` gets shifted right testing successive low order bits, and
   --  ``Left`` is raised to the next power of 2. The multiplications are
   --  performed using modular multiplications.

end System.Exp_Mod;
