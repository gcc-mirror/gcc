------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E V A L _ F A T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides for compile-time evaluation of static calls to the
--  floating-point attribute functions. It is the compile-time equivalent of
--  the System.Fat_Gen runtime package. The coding is quite similar, as are
--  the subprogram specs, except that the type is passed as an explicit
--  first parameter (and used via ttypes, to obtain the necessary information
--  about the characteristics of the type for computing the results.

with Types;  use Types;
with Uintp;  use Uintp;
with Urealp; use Urealp;

package Eval_Fat is

   subtype UI is Uint;
   --  The compile time representation of universal integer

   subtype T is Ureal;
   --  The compile time representation of floating-point values

   subtype R is Entity_Id;
   --  The compile time representation of the floating-point root type

   type Rounding_Mode is (Floor, Ceiling, Round, Round_Even);
   for Rounding_Mode use (0, 1, 2, 3);
   --  Used to indicate rounding mode for Machine attribute
   --  Note that C code in gigi knows that Round_Even is 3

   Rounding_Was_Biased : Boolean;
   --  Set if last use of Machine rounded a halfway case away from zero

   function Adjacent          (RT : R; X, Towards : T)              return T;

   function Ceiling           (RT : R; X : T)                       return T;

   function Compose           (RT : R; Fraction : T; Exponent : UI) return T;

   function Copy_Sign         (RT : R; Value, Sign : T)             return T;

   function Exponent          (RT : R; X : T)                       return UI;

   function Floor             (RT : R; X : T)                       return T;

   function Fraction          (RT : R; X : T)                       return T;

   function Leading_Part      (RT : R; X : T; Radix_Digits : UI)    return T;

   function Machine           (RT : R; X : T; Mode : Rounding_Mode) return T;

   function Model             (RT : R; X : T)                       return T;

   function Pred              (RT : R; X : T)                       return T;

   function Remainder         (RT : R; X, Y : T)                    return T;

   function Rounding          (RT : R; X : T)                       return T;

   function Scaling           (RT : R; X : T; Adjustment : UI)      return T;

   function Succ              (RT : R; X : T)                       return T;

   function Truncation        (RT : R; X : T)                       return T;

   function Unbiased_Rounding (RT : R; X : T)                       return T;

end Eval_Fat;
