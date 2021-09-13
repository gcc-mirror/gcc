------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X P _ M O D                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1992-2021, Free Software Foundation, Inc.        --
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
--  Note that 1 is a binary modulus (2**0), so the compiler should not (and
--  will not) call this function with Modulus equal to 1.

with System.Unsigned_Types;

package System.Exp_Mod is
   pragma Pure;
   use type System.Unsigned_Types.Unsigned;

   subtype Power_Of_2 is System.Unsigned_Types.Unsigned with
     Dynamic_Predicate =>
        Power_Of_2 /= 0 and then (Power_Of_2 and (Power_Of_2 - 1)) = 0;

   function Exp_Modular
     (Left    : System.Unsigned_Types.Unsigned;
      Modulus : System.Unsigned_Types.Unsigned;
      Right   : Natural) return System.Unsigned_Types.Unsigned
   with
       Pre  => Modulus /= 0 and then Modulus not in Power_Of_2,
       Post => Exp_Modular'Result = Left ** Right mod Modulus;

end System.Exp_Mod;
