------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F A T _ G E N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This generic package provides a target independent implementation of the
--  floating-point attributes that denote functions. The implementations here
--  are portable, but very slow. The runtime contains a set of instantiations
--  of this package for all predefined floating-point types, and these should
--  be replaced by efficient assembly language code where possible.

generic
    type T is digits <>;

package System.Fat_Gen is
   pragma Pure;

   subtype UI is Integer;
   --  The runtime representation of universal integer for the purposes of
   --  this package is integer. The expander generates conversions for the
   --  actual type used. For functions returning universal integer, there
   --  is no problem, since the result always is in range of integer. For
   --  input arguments, the expander has to do some special casing to deal
   --  with the (very annoying) cases of out of range values. If we used
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

   function Machine_Rounding  (X : T)                       return T;

   function Model             (X : T)                       return T;

   function Pred              (X : T)                       return T;

   function Remainder         (X, Y : T)                    return T;

   function Rounding          (X : T)                       return T;

   function Scaling           (X : T; Adjustment : UI)      return T;

   function Succ              (X : T)                       return T;

   function Truncation        (X : T)                       return T;

   function Unbiased_Rounding (X : T)                       return T;

   function Valid (X : not null access T) return Boolean;
   --  This function checks if the object of type T referenced by X
   --  is valid, and returns True/False accordingly. The parameter is
   --  passed by reference (access) here, as the object of type T may
   --  be an abnormal value that cannot be passed in a floating-point
   --  register, and the whole point of 'Valid is to prevent exceptions.
   --  Note that the object of type T must have the natural alignment
   --  for type T. See Unaligned_Valid for further discussion.
   --
   --  Note: this routine does not work for Vax_Float ???

   function Unaligned_Valid (A : System.Address) return Boolean;
   --  This version of Valid is used if the floating-point value to
   --  be checked is not known to be aligned (for example it appears
   --  in a packed record). In this case, we cannot call Valid since
   --  Valid assumes proper full alignment. Instead Unaligned_Valid
   --  performs the same processing for a possibly unaligned float,
   --  by first doing a copy and then calling Valid. One might think
   --  that the front end could simply do a copy to an aligned temp,
   --  but remember that we may have an abnormal value that cannot
   --  be copied into a floating-point register, so things are a bit
   --  trickier than one might expect.
   --
   --  Note: Unaligned_Valid is never called for a target which does
   --  not require strict alignment (e.g. the ia32/x86), since on a
   --  target not requiring strict alignment, it is fine to pass a
   --  non-aligned value to the standard Valid routine.
   --
   --  Note: this routine does not work for Vax_Float ???

private
   pragma Inline (Machine);
   pragma Inline (Model);

   --  Note: previously the validity checking subprograms (Unaligned_Valid and
   --  Valid) were also inlined, but this was changed since there were some
   --  problems with this inlining in optimized mode, and in any case it seems
   --  better to avoid this inlining (space and robustness considerations).

end System.Fat_Gen;
