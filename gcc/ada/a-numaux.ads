------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                       (C Library Version, non-x86)                       --
--                                                                          --
--                            $Revision: 1.1.16.1 $
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

--  Note: there are two versions of this package. One using the normal IEEE
--  64-bit double format (which is this version), and one using 80-bit x86
--  long double (see file 4onumaux.ads).

package Ada.Numerics.Aux is
pragma Pure (Aux);

   pragma Linker_Options ("-lm");

   type Double is digits 15;
   pragma Float_Representation (IEEE_Float, Double);
   --  Type Double is the type used to call the C routines. Note that this
   --  is IEEE format even when running on VMS with Vax_Float representation
   --  since we use the IEEE version of the C library with VMS.

   function Sin (X : Double) return Double;
   pragma Import (C, Sin, "sin");

   function Cos (X : Double) return Double;
   pragma Import (C, Cos, "cos");

   function Tan (X : Double) return Double;
   pragma Import (C, Tan, "tan");

   function Exp (X : Double) return Double;
   pragma Import (C, Exp, "exp");

   function Sqrt (X : Double) return Double;
   pragma Import (C, Sqrt, "sqrt");

   function Log (X : Double) return Double;
   pragma Import (C, Log, "log");

   function Acos (X : Double) return Double;
   pragma Import (C, Acos, "acos");

   function Asin (X : Double) return Double;
   pragma Import (C, Asin, "asin");

   function Atan (X : Double) return Double;
   pragma Import (C, Atan, "atan");

   function Sinh (X : Double) return Double;
   pragma Import (C, Sinh, "sinh");

   function Cosh (X : Double) return Double;
   pragma Import (C, Cosh, "cosh");

   function Tanh (X : Double) return Double;
   pragma Import (C, Tanh, "tanh");

   function Pow (X, Y : Double) return Double;
   pragma Import (C, Pow, "pow");

end Ada.Numerics.Aux;
