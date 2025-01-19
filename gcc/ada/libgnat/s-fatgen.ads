------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . F A T _ G E N                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
   --  If ``Towards`` = ``X``, the function returns ``X``; oterwise, it yields
   --  the machien number of the type *T* adjacent to ``X`` in the direction
   --  ``Towards``, if that machine number exists.

   function Ceiling           (X : T)                       return T;
   --  Truncate ``X``. If the truncation is equal to ``X`` return ``X``. If
   --  ``X`` is less than zero, return the truncation, otherwise add one
   --  to the truncation.

   function Compose           (Fraction : T; Exponent : UI) return T;
   --  Decompose the ``Fraction`` into its fraction and exponent parts. Call
   --  *Scaling* with the returned fraction part and ``Exponent``.

   function Copy_Sign         (Value, Sign : T)             return T;
   --  Take the absolute value of ``Value``. Negate the result if ``Sign`` is
   --  less than zero.

   function Exponent          (X : T)                       return UI;
   --  Decompose `X`` and return the exponent part.

   function Floor             (X : T)                       return T;
   --  Truncate ``X``. If the truncation is equal to ``X`` return ``X``. If
   --  ``X`` is greater than zero, return the truncation, otherwise subtract
   --  one from the truncation.

   function Fraction          (X : T)                       return T;
   --  Decompose `X`` and return the fraction part

   function Leading_Part      (X : T; Radix_Digits : UI)    return T;
   --  Return ``X`` if the ``Radix_Digits`` is larger than the type's machine
   --  mantissa. Otherwise scale down and truncate ``X`` by the difference
   --  between the exponent of ``X`` and ``Radix_Digits``, then scale the
   --  result back up.

   function Machine           (X : T)                       return T;
   --  Force ``X`` to be stored in memory and retrieve the result

   function Machine_Rounding  (X : T)                       return T;
   --  Truncate the absolute value of ``X`` + 0.5. If ``X`` is negative, negate
   --  the result.

   function Model             (X : T)                       return T;
   --  If ``X`` is a model number of *T*, the function returns ``X``;
   --  otherwise it yields the value obtained by rounding or truncating ``X``
   --  to either one of the adjacent model numbers of *T*.
   --
   --  We treat *Model* as identical to *Machine*. This is true of IEEE and
   --  other nice floating-point systems, but not necessarily true of all
   --  systems.

   function Pred              (X : T)                       return T;
   --  Return the machine number immediately below the value of ``X``.
   --
   --  If zero, return the negative of *Succ* (``X``).
   --
   --  If ``X`` = *T*'First, return negative infinity.
   --
   --  If ``X`` is already infinity, return ``X``.
   --
   --  Otherwise, subtract from ``X`` a number equivalent to the value of its
   --  least significant bit.

   function Remainder         (X, Y : T)                    return T;
   --  Return the remainder (n) of ``X`` divided by ``Y``.
   --  If abs(n - ``X`` / ``Y``) = 1/2 then n is chosen to be even.
   --
   --  Calculate the modulus remainder: if abs(``X``) < abs(``Y``) then the
   --  remainder is abs(``X``). Otherwise, decompose abs(``X``) and abs(``Y``).
   --  Then:
   --
   --  .. code-block:: ada
   --
   --    P := Compose (Y_Frac, X_Exp);
   --    K := X_Exp - Y_Exp;
   --    Rem := |X|;
   --    for J in reverse 0 .. K loop
   --       if Rem >= P then
   --          Rem := Rem - P;
   --       end if;
   --       P := P * 0.5;
   --    end loop;
   --
   --  Return the IEEE remainder by adjusting result such that if
   --  abs(n - X/Y) = 1/2 then n is even.

   function Rounding          (X : T)                       return T;
   --  The function yields the integral value nearest to ``X``, rounding away
   --  from zero if ``X`` lies exactly halfway between two integers.
   --
   --  The function truncates the absolute value of ``X`` + 0.5. If ``X`` is
   --  negative, negate the result.

   function Scaling           (X : T; Adjustment : UI)      return T;
   --  Let v be the value ``X`` * *T*'Machine_RadixAdjustment. If v is a
   --  machine number of the type *T*, or if abs(v) >= *T*'Model_Small, the
   --  function yields v; otherwise, it yields either one of the machine
   --  numbers of the type *T* adjacent to v.
   --
   --  If ``X`` or ``Adjustment`` equal zero, return ``X``. Otherwise, return
   --  ``X`` * Machine_Radix ** ``Adjustment``.

   function Succ              (X : T)                       return T;
   --  Returns the machine number immediately above the value of X.
   --
   --  If zero, return the smallest denormal.
   --
   --  If ``X`` = *T*'Last, return infinity.
   --
   --  If ``X`` is already infinity, return ``X``.
   --
   --  Otherwise, add to X a number equivalent to the value of its least
   --  significant bit.

   function Truncation        (X : T)                       return T;
   --  The function yields the value *Ceiling* (``X``) when ``X`` is negative,
   --  and *Floor* (``X``) otherwise.
   --
   --  Return *T*'Machine (RM1 + N) - RM1 where N is abs(``X``) and
   --  RM1 = radix ** (mantissa - 1). Negate the result where ``X`` is
   --  negative.

   function Unbiased_Rounding (X : T)                       return T;
   --  The integral value nearest to ``X``, rounding toward the even integer
   --  if ``X`` lies exactly halfway between two integers.
   --
   --  This function truncates abs(``X``). If the tail of the result is greater
   --  than 0.5 add one to the result. If the tail equals 0.5, round to the
   --  nearest even integer. Negate the result if ``X`` is negative.

   function Valid (X : not null access T) return Boolean;
   --  This function checks if the object of type *T* referenced by ``X`` is
   --  valid, and returns True/False accordingly. The parameter is passed by
   --  reference (access) here, as the object of type T may be an abnormal
   --  value that cannot be passed in a floating-point register, and the whole
   --  point of 'Valid is to prevent exceptions. Note that the object of
   --  type *T* must have the natural alignment for type *T*.
   --
   --  If denormalized numbers are valid: return True unless ``X`` is infinity
   --  or NaN. If denormalized numbers are not valid, return False if ``X`` is
   --  a denormal number.

   type S is new String (1 .. T'Size / Character'Size);
   type P is access all S with Storage_Size => 0;
   --  Buffer and access types used to initialize temporaries for validity
   --  checks, if the value to be checked has reverse scalar storage order, or
   --  is not known to be properly aligned (for example it appears in a packed
   --  record). In this case, we cannot call Valid since Valid assumes proper
   --  full alignment. Instead, we copy the value to a temporary location using
   --  type S (we cannot simply do a copy of a T value, because the value might
   --  be invalid, in which case it might not be possible to copy it through a
   --  floating point register).

private
   pragma Inline (Compose);
   pragma Inline (Copy_Sign);
   pragma Inline (Exponent);
   pragma Inline (Fraction);
   pragma Inline (Machine);
   pragma Inline (Model);
   pragma Inline (Valid);

end System.Fat_Gen;
