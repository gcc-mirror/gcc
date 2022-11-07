..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Arithmetic on poly_ints
***********************

Addition, subtraction, negation and bit inversion all work normally for
``poly_int`` s.  Multiplication by a constant multiplier and left
shifting by a constant shift amount also work normally.  General
multiplication of two ``poly_int`` s is not supported and is not
useful in practice.

Other operations are only conditionally supported: the operation
might succeed or might fail, depending on the inputs.

This section describes both types of operation.

.. toctree::
  :maxdepth: 2


Using poly_int with C++ arithmetic operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following C++ expressions are supported, where :samp:`{p1}` and :samp:`{p2}`
are ``poly_int`` s and where :samp:`{c1}` and :samp:`{c2}` are scalars:

.. code-block:: c++

  -p1
  ~p1

  p1 + p2
  p1 + c2
  c1 + p2

  p1 - p2
  p1 - c2
  c1 - p2

  c1 * p2
  p1 * c2

  p1 << c2

  p1 += p2
  p1 += c2

  p1 -= p2
  p1 -= c2

  p1 *= c2
  p1 <<= c2

These arithmetic operations handle integer ranks in a similar way
to C++.  The main difference is that every coefficient narrower than
``HOST_WIDE_INT`` promotes to ``HOST_WIDE_INT``, whereas in
C++ everything narrower than ``int`` promotes to ``int``.
For example:

.. code-block:: c++

  poly_uint16     + int          -> poly_int64
  unsigned int    + poly_uint16  -> poly_int64
  poly_int64      + int          -> poly_int64
  poly_int32      + poly_uint64  -> poly_uint64
  uint64          + poly_int64   -> poly_uint64
  poly_offset_int + int32        -> poly_offset_int
  offset_int      + poly_uint16  -> poly_offset_int

In the first two examples, both coefficients are narrower than
``HOST_WIDE_INT``, so the result has coefficients of type
``HOST_WIDE_INT``.  In the other examples, the coefficient
with the highest rank 'wins'.

If one of the operands is ``wide_int`` or ``poly_wide_int``,
the rules are the same as for ``wide_int`` arithmetic.

wi arithmetic on poly_ints
^^^^^^^^^^^^^^^^^^^^^^^^^^

As well as the C++ operators, ``poly_int`` supports the following
``wi`` routines:

.. code-block:: c++

  wi::neg (p1, &overflow)

  wi::add (p1, p2)
  wi::add (p1, c2)
  wi::add (c1, p1)
  wi::add (p1, p2, sign, &overflow)

  wi::sub (p1, p2)
  wi::sub (p1, c2)
  wi::sub (c1, p1)
  wi::sub (p1, p2, sign, &overflow)

  wi::mul (p1, c2)
  wi::mul (c1, p1)
  wi::mul (p1, c2, sign, &overflow)

  wi::lshift (p1, c2)

These routines just check whether overflow occurs on any individual
coefficient; it is not possible to know at compile time whether the
final runtime value would overflow.

Division of poly_ints
^^^^^^^^^^^^^^^^^^^^^

Division of ``poly_int`` s is possible for certain inputs.  The functions
for division return true if the operation is possible and in most cases
return the results by pointer.  The routines are:

:samp:`multiple_p ({a}, {b})` :samp:`multiple_p ({a}, {b}, &{quotient})`
  Return true if :samp:`{a}` is an exact multiple of :samp:`{b}`, storing the result
  in :samp:`{quotient}` if so.  There are overloads for various combinations
  of polynomial and constant :samp:`{a}`, :samp:`{b}` and :samp:`{quotient}`.

:samp:`constant_multiple_p ({a}, {b})` :samp:`constant_multiple_p ({a}, {b}, &{quotient})`
  Like ``multiple_p``, but also test whether the multiple is a
  compile-time constant.

:samp:`can_div_trunc_p ({a}, {b}, &{quotient})` :samp:`can_div_trunc_p ({a}, {b}, &{quotient}, &{remainder})`
  Return true if we can calculate :samp:`trunc ({a} / {b})` at compile
  time, storing the result in :samp:`{quotient}` and :samp:`{remainder}` if so.

:samp:`can_div_away_from_zero_p ({a}, {b}, &{quotient})`
  Return true if we can calculate :samp:`{a} / {b}` at compile time,
  rounding away from zero.  Store the result in :samp:`{quotient}` if so.

  Note that this is true if and only if ``can_div_trunc_p`` is true.
  The only difference is in the rounding of the result.

  There is also an asserting form of division:

:samp:`exact_div ({a}, {b})`
  Assert that :samp:`{a}` is a multiple of :samp:`{b}` and return
  :samp:`{a} / {b}`.  The result is a ``poly_int`` if :samp:`{a}`
  is a ``poly_int``.

Other poly_int arithmetic
^^^^^^^^^^^^^^^^^^^^^^^^^

There are tentative routines for other operations besides division:

:samp:`can_ior_p ({a}, {b}, &{result})`
  Return true if we can calculate :samp:`{a} | {b}` at compile time,
  storing the result in :samp:`{result}` if so.

  Also, ANDs with a value :samp:`(1 << {y}) - 1` or its inverse can be
  treated as alignment operations.  See :ref:`alignment-of-poly_ints`.

In addition, the following miscellaneous routines are available:

:samp:`coeff_gcd ({a})`
  Return the greatest common divisor of all nonzero coefficients in
  :samp:`{a}`, or zero if :samp:`{a}` is known to be zero.

:samp:`common_multiple ({a}, {b})`
  Return a value that is a multiple of both :samp:`{a}` and :samp:`{b}`, where
  one value is a ``poly_int`` and the other is a scalar.  The result
  will be the least common multiple for some indeterminate values but
  not necessarily for all.

:samp:`force_common_multiple ({a}, {b})`
  Return a value that is a multiple of both ``poly_int`` :samp:`{a}` and
  ``poly_int`` :samp:`{b}`, asserting that such a value exists.  The
  result will be the least common multiple for some indeterminate values
  but not necessarily for all.

  When using this routine, please add a comment explaining why the
  assertion is known to hold.

  Please add any other operations that you find to be useful.