------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                       (C Library Version for x86)                        --
--                                                                          --
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
--  elementary functions. The C library version interfaces with the routines
--  in the C mathematical library, and is thus quite portable, although it may
--  not necessarily meet the requirements for accuracy in the numerics annex.
--  One advantage of using this package is that it will interface directly to
--  hardware instructions, such as the those provided on the Intel x86.

--  Note: there are two versions of this package. One using the 80-bit x86
--  long double format (which is this version), and one using 64-bit IEEE
--  double (see file a-numaux.ads).

package Ada.Numerics.Aux is
pragma Pure (Aux);

   pragma Linker_Options ("-lm");

   type Double is digits 18;

   function Sin (X : Double) return Double;
   pragma Import (C, Sin, "sinl");

   function Cos (X : Double) return Double;
   pragma Import (C, Cos, "cosl");

   function Tan (X : Double) return Double;
   pragma Import (C, Tan, "tanl");

   function Exp (X : Double) return Double;
   pragma Import (C, Exp, "expl");

   function Sqrt (X : Double) return Double;
   pragma Import (C, Sqrt, "sqrtl");

   function Log (X : Double) return Double;
   pragma Import (C, Log, "logl");

   function Acos (X : Double) return Double;
   pragma Import (C, Acos, "acosl");

   function Asin (X : Double) return Double;
   pragma Import (C, Asin, "asinl");

   function Atan (X : Double) return Double;
   pragma Import (C, Atan, "atanl");

   function Sinh (X : Double) return Double;
   pragma Import (C, Sinh, "sinhl");

   function Cosh (X : Double) return Double;
   pragma Import (C, Cosh, "coshl");

   function Tanh (X : Double) return Double;
   pragma Import (C, Tanh, "tanhl");

   function Pow (X, Y : Double) return Double;
   pragma Import (C, Pow, "powl");

end Ada.Numerics.Aux;
