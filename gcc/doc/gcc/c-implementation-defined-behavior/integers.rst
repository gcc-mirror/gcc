..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _integers-implementation:

Integers
********

* Any extended integer types that exist in the implementation (C99
  and C11 6.2.5).

  GCC does not support any extended integer types.

  .. The __mode__ attribute might create types of precisions not

  .. otherwise supported, but the syntax isn't right for use everywhere

  .. the standard type names might be used.  Predefined typedefs should

  .. be used if any extended integer types are to be defined.  The __int128_t and __uint128_t

  .. typedefs are not extended integer types

  .. as they are generally longer than the ABI-specified intmax_t.

* Whether signed integer types are represented using sign and magnitude,
  two's complement, or one's complement, and whether the extraordinary value
  is a trap representation or an ordinary value (C99 and C11 6.2.6.2).

  GCC supports only two's complement integer types, and all bit patterns
  are ordinary values.

* The rank of any extended integer type relative to another extended
  integer type with the same precision (C99 and C11 6.3.1.1).

  GCC does not support any extended integer types.

  .. If it did, there would only be one of each precision and signedness.

* The result of, or the signal raised by, converting an integer to a
  signed integer type when the value cannot be represented in an object of
  that type (C90 6.2.1.2, C99 and C11 6.3.1.3).

  For conversion to a type of width N, the value is reduced
  modulo 2^N to be within range of the type; no signal is raised.

* The results of some bitwise operations on signed integers (C90
  6.3, C99 and C11 6.5).

  Bitwise operators act on the representation of the value including
  both the sign and value bits, where the sign bit is considered
  immediately above the highest-value value bit.  Signed :samp:`>>` acts
  on negative numbers by sign extension.

  As an extension to the C language, GCC does not use the latitude given in
  C99 and C11 only to treat certain aspects of signed :samp:`<<` as undefined.
  However, :option:`-fsanitize=shift` (and :option:`-fsanitize=undefined`) will
  diagnose such cases.  They are also diagnosed where constant
  expressions are required.

* The sign of the remainder on integer division (C90 6.3.5).

  GCC always follows the C99 and C11 requirement that the result of division is
  truncated towards zero.
