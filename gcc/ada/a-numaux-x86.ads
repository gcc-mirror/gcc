------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                        (Machine Version for x86)                         --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the basic computational interface for the generic
--  elementary functions. This implementation is based on the glibc assembly
--  sources for the x86 glibc math library.

--  Note: there are two versions of this package. One using the 80-bit x86
--  long double format (which is this version), and one using 64-bit IEEE
--  double (see file a-numaux.ads). The latter version imports the C
--  routines directly.

package Ada.Numerics.Aux is
pragma Pure (Aux);

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
