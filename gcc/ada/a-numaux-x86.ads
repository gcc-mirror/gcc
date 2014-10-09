------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                        (Machine Version for x86)                         --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

--  Version for the x86, using 64-bit IEEE format with inline asm statements

package Ada.Numerics.Aux is
   pragma Pure;

   type Double is new Long_Long_Float;

   function Sin (X : Double) return Double;

   function Cos (X : Double) return Double;

   function Tan (X : Double) return Double;

   function Exp (X : Double) return Double;

   function Sqrt (X : Double) return Double;

   function Log (X : Double) return Double;

   function Atan (X : Double) return Double;

   function Acos (X : Double) return Double;

   function Asin (X : Double) return Double;

   function Sinh (X : Double) return Double;

   function Cosh (X : Double) return Double;

   function Tanh (X : Double) return Double;

   function Pow (X, Y : Double) return Double;

private
   pragma Inline (Atan);
   pragma Inline (Cos);
   pragma Inline (Tan);
   pragma Inline (Exp);
   pragma Inline (Log);
   pragma Inline (Sin);
   pragma Inline (Sqrt);

end Ada.Numerics.Aux;
