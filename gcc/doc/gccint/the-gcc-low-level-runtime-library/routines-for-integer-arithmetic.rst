..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _integer-library-routines:

Routines for integer arithmetic
*******************************

The integer arithmetic routines are used on platforms that don't provide
hardware support for arithmetic operations on some modes.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: int __ashlsi3 (int a, int b)
              long __ashldi3 (long a, int b)
              long long __ashlti3 (long long a, int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits.

.. function:: int __ashrsi3 (int a, int b)
              long __ashrdi3 (long a, int b)
              long long __ashrti3 (long long a, int b)

  These functions return the result of arithmetically shifting :samp:`{a}` right
  by :samp:`{b}` bits.

.. function:: int __divsi3 (int a, int b)
              long __divdi3 (long a, long b)
              long long __divti3 (long long a, long long b)

  These functions return the quotient of the signed division of :samp:`{a}` and
  :samp:`{b}`.

.. function:: int __lshrsi3 (int a, int b)
              long __lshrdi3 (long a, int b)
              long long __lshrti3 (long long a, int b)

  These functions return the result of logically shifting :samp:`{a}` right by
  :samp:`{b}` bits.

.. function:: int __modsi3 (int a, int b)
              long __moddi3 (long a, long b)
              long long __modti3 (long long a, long long b)

  These functions return the remainder of the signed division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: int __mulsi3 (int a, int b)
              long __muldi3 (long a, long b)
              long long __multi3 (long long a, long long b)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: long __negdi2 (long a)
              long long __negti2 (long long a)

  These functions return the negation of :samp:`{a}`.

.. function:: unsigned int __udivsi3 (unsigned int a, unsigned int b)
              unsigned long __udivdi3 (unsigned long a, unsigned long b)
              unsigned long long __udivti3 (unsigned long long a, unsigned long long b)

  These functions return the quotient of the unsigned division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: unsigned long __udivmoddi4 (unsigned long a, unsigned long b, unsigned long *c)
              unsigned long long __udivmodti4 (unsigned long long a, unsigned long long b, unsigned long long *c)

  These functions calculate both the quotient and remainder of the unsigned
  division of :samp:`{a}` and :samp:`{b}`.  The return value is the quotient, and
  the remainder is placed in variable pointed to by :samp:`{c}`.

.. function:: unsigned int __umodsi3 (unsigned int a, unsigned int b)
              unsigned long __umoddi3 (unsigned long a, unsigned long b)
              unsigned long long __umodti3 (unsigned long long a, unsigned long long b)

  These functions return the remainder of the unsigned division of :samp:`{a}`
  and :samp:`{b}`.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

The following functions implement integral comparisons.  These functions
implement a low-level compare, upon which the higher level comparison
operators (such as less than and greater than or equal to) can be
constructed.  The returned values lie in the range zero to two, to allow
the high-level operators to be implemented by testing the returned
result using either signed or unsigned comparison.

.. function:: int __cmpdi2 (long a, long b)
              int __cmpti2 (long long a, long long b)

  These functions perform a signed comparison of :samp:`{a}` and :samp:`{b}`.  If
  :samp:`{a}` is less than :samp:`{b}`, they return 0; if :samp:`{a}` is greater than
  :samp:`{b}`, they return 2; and if :samp:`{a}` and :samp:`{b}` are equal they return 1.

.. function:: int __ucmpdi2 (unsigned long a, unsigned long b)
              int __ucmpti2 (unsigned long long a, unsigned long long b)

  These functions perform an unsigned comparison of :samp:`{a}` and :samp:`{b}`.
  If :samp:`{a}` is less than :samp:`{b}`, they return 0; if :samp:`{a}` is greater than
  :samp:`{b}`, they return 2; and if :samp:`{a}` and :samp:`{b}` are equal they return 1.

Trapping arithmetic functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following functions implement trapping arithmetic.  These functions
call the libc function ``abort`` upon signed arithmetic overflow.

.. function:: int __absvsi2 (int a)
              long __absvdi2 (long a)

  These functions return the absolute value of :samp:`{a}`.

.. function:: int __addvsi3 (int a, int b)
              long __addvdi3 (long a, long b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}` ; that is
  ``a + b``.

.. function:: int __mulvsi3 (int a, int b)
              long __mulvdi3 (long a, long b)

  The functions return the product of :samp:`{a}` and :samp:`{b}` ; that is
  ``a * b``.

.. function:: int __negvsi2 (int a)
              long __negvdi2 (long a)

  These functions return the negation of :samp:`{a}` ; that is ``-a``.

.. function:: int __subvsi3 (int a, int b)
              long __subvdi3 (long a, long b)

  These functions return the difference between :samp:`{b}` and :samp:`{a}` ;
  that is ``a - b``.

Bit operations
^^^^^^^^^^^^^^

.. function:: int __clzsi2 (unsigned int a)
              int __clzdi2 (unsigned long a)
              int __clzti2 (unsigned long long a)

  These functions return the number of leading 0-bits in :samp:`{a}`, starting
  at the most significant bit position.  If :samp:`{a}` is zero, the result is
  undefined.

.. function:: int __ctzsi2 (unsigned int a)
              int __ctzdi2 (unsigned long a)
              int __ctzti2 (unsigned long long a)

  These functions return the number of trailing 0-bits in :samp:`{a}`, starting
  at the least significant bit position.  If :samp:`{a}` is zero, the result is
  undefined.

.. function:: int __ffsdi2 (unsigned long a)
              int __ffsti2 (unsigned long long a)

  These functions return the index of the least significant 1-bit in :samp:`{a}`,
  or the value zero if :samp:`{a}` is zero.  The least significant bit is index
  one.

.. function:: int __paritysi2 (unsigned int a)
              int __paritydi2 (unsigned long a)
              int __parityti2 (unsigned long long a)

  These functions return the value zero if the number of bits set in
  :samp:`{a}` is even, and the value one otherwise.

.. function:: int __popcountsi2 (unsigned int a)
              int __popcountdi2 (unsigned long a)
              int __popcountti2 (unsigned long long a)

  These functions return the number of bits set in :samp:`{a}`.

.. function:: int32_t __bswapsi2 (int32_t a)
              int64_t __bswapdi2 (int64_t a)

  These functions return the :samp:`{a}` byteswapped.
