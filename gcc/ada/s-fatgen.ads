------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F A T _ G E N                        --
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

--  This generic package provides a target independent implementation of the
--  floating-point attributes that denote functions. The implementations here
--  are portable, but very slow. The runtime contains a set of instantiations
--  of this package for all predefined floating-point types, and these should
--  be replaced by efficient assembly language code where possible.

generic
    type T is digits <>;

package System.Fat_Gen is
pragma Pure (Fat_Gen);

   subtype UI is Integer;
   --  The runtime representation of universal integer for the purposes of
   --  this package is integer. The expander generates conversions for the
   --  actual type used. For functions returning universal integer, there
   --  is no problem, since the result always is in range of integer. For
   --  input arguments, the expander has to do some special casing to deal
   --  with the (very annoying!) cases of out of range values. If we used
   --  Long_Long_Integer to represent universal, then there would be no
   --  problem, but the resulting inefficiency would be annoying.

   function Adjacent          (X, Towards : T)              return T;

   function Ceiling           (X : T)                       return T;

   function Compose           (Fraction : T; Exponent : UI) return T;

   function Copy_Sign         (Value, Sign : T)             return T;

   function Exponent          (X : T)                       return UI;

   function Floor             (X : T)                       return T;

   function Fraction          (X : T)                       return T;

   function Leading_Part      (X : T; Radix_Digits : UI)    return T;

   function Machine           (X : T)                       return T;

   function Model             (X : T)                       return T;

   function Pred              (X : T)                       return T;

   function Remainder         (X, Y : T)                    return T;

   function Rounding          (X : T)                       return T;

   function Scaling           (X : T; Adjustment : UI)      return T;

   function Succ              (X : T)                       return T;

   function Truncation        (X : T)                       return T;

   function Unbiased_Rounding (X : T)                       return T;

   function Valid (X : access T) return Boolean;
   --  This function checks if the object of type T referenced by X
   --  is valid, and returns True/False accordingly. The parameter is
   --  passed by reference (access) here, as the object of type T may
   --  be an abnormal value that cannot be passed in a floating-point
   --  register, and the whole point of 'Valid is to prevent exceptions.

private
   pragma Inline (Machine);
   pragma Inline (Model);
   pragma Inline_Always (Valid);

end System.Fat_Gen;
