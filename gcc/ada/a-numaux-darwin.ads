------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . A U X                      --
--                                                                          --
--                                 S p e c                                  --
--                          (Apple OS X Version)                            --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  This version is for use with normal Unix math functions, except for
--  sine/cosine which have been implemented directly in Ada to get
--  the required accuracy in OS X.  Alternative packages are used
--  on OpenVMS (different import names), VxWorks (no need for the
--  -lm Linker_Options), and on the x86 (where we have two
--  versions one using inline ASM, and one importing from the C long
--  routines that take 80-bit arguments).

package Ada.Numerics.Aux is
pragma Pure (Aux);

   pragma Linker_Options ("-lm");

   type Double is digits 15;
   --  Type Double is the type used to call the C routines

   --  The following functions have been implemented in Ada, since
   --  the OS X math library didn't meet accuracy requirements for
   --  argument reduction. The implementation here has been tailored
   --  to match Ada strict mode Numerics requirements while maintaining
   --  maximum efficiency.
   function Sin (X : Double) return Double;
   pragma Inline (Sin);

   function Cos (X : Double) return Double;
   pragma Inline (Cos);

   --  We import these functions directly from C. Note that we label them
   --  all as pure functions, because indeed all of them are in fact pure!

   function Tan (X : Double) return Double;
   pragma Import (C, Tan, "tan");
   pragma Pure_Function (Tan);

   function Exp (X : Double) return Double;
   pragma Import (C, Exp, "exp");
   pragma Pure_Function (Exp);

   function Sqrt (X : Double) return Double;
   pragma Import (C, Sqrt, "sqrt");
   pragma Pure_Function (Sqrt);

   function Log (X : Double) return Double;
   pragma Import (C, Log, "log");
   pragma Pure_Function (Log);

   function Acos (X : Double) return Double;
   pragma Import (C, Acos, "acos");
   pragma Pure_Function (Acos);

   function Asin (X : Double) return Double;
   pragma Import (C, Asin, "asin");
   pragma Pure_Function (Asin);

   function Atan (X : Double) return Double;
   pragma Import (C, Atan, "atan");
   pragma Pure_Function (Atan);

   function Sinh (X : Double) return Double;
   pragma Import (C, Sinh, "sinh");
   pragma Pure_Function (Sinh);

   function Cosh (X : Double) return Double;
   pragma Import (C, Cosh, "cosh");
   pragma Pure_Function (Cosh);

   function Tanh (X : Double) return Double;
   pragma Import (C, Tanh, "tanh");
   pragma Pure_Function (Tanh);

   function Pow (X, Y : Double) return Double;
   pragma Import (C, Pow, "pow");
   pragma Pure_Function (Pow);

end Ada.Numerics.Aux;
