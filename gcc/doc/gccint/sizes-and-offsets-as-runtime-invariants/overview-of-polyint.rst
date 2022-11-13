..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: poly_int, runtime value

Overview of poly_int
********************

We define indeterminates :samp:`{x1}`, ..., :samp:`{xn}` whose values are
only known at runtime and use polynomials of the form:

.. code-block:: c++

  c0 + c1 * x1 + ... + cn * xn

to represent a size or offset whose value might depend on some
of these indeterminates.  The coefficients :samp:`{c0}`, ..., :samp:`{cn}`
are always known at compile time, with the :samp:`{c0}` term being the
'constant' part that does not depend on any runtime value.

GCC uses the ``poly_int`` class to represent these coefficients.
The class has two template parameters: the first specifies the number of
coefficients (:samp:`{n}` + 1) and the second specifies the type of the
coefficients.  For example, :samp:`poly_int<2, unsigned short>` represents
a polynomial with two coefficients (and thus one indeterminate), with each
coefficient having type ``unsigned short``.  When :samp:`{n}` is 0,
the class degenerates to a single compile-time constant :samp:`{c0}`.

.. index:: poly_int, template parameters, NUM_POLY_INT_COEFFS

The number of coefficients needed for compilation is a fixed
property of each target and is specified by the configuration macro
``NUM_POLY_INT_COEFFS``.  The default value is 1, since most targets
do not have such runtime invariants.  Targets that need a different
value should ``#define`` the macro in their :samp:`{cpu}-modes.def`
file.  See :ref:`back-end`.

.. index:: poly_int, invariant range

``poly_int`` makes the simplifying requirement that each indeterminate
must be a nonnegative integer.  An indeterminate value of 0 should usually
represent the minimum possible runtime value, with :samp:`{c0}` specifying
the value in that case.

For example, when targetting the Arm SVE ISA, the single indeterminate
represents the number of 128-bit blocks in a vector *beyond the minimum
length of 128 bits*.  Thus the number of 64-bit doublewords in a vector
is 2 + 2 \* :samp:`{x1}`.  If an aggregate has a single SVE vector and 16
additional bytes, its total size is 32 + 16 \* :samp:`{x1}` bytes.

The header file :samp:`poly-int-types.h` provides typedefs for the
most common forms of ``poly_int``, all having
``NUM_POLY_INT_COEFFS`` coefficients:

.. index:: poly_int, main typedefs

``poly_uint16``
  a :samp:`poly_int` with ``unsigned short`` coefficients.

``poly_int64``
  a :samp:`poly_int` with ``HOST_WIDE_INT`` coefficients.

``poly_uint64``
  a :samp:`poly_int` with ``unsigned HOST_WIDE_INT`` coefficients.

``poly_offset_int``
  a :samp:`poly_int` with ``offset_int`` coefficients.

``poly_wide_int``
  a :samp:`poly_int` with ``wide_int`` coefficients.

``poly_widest_int``
  a :samp:`poly_int` with ``widest_int`` coefficients.

  Since the main purpose of ``poly_int`` is to represent sizes and
  offsets, the last two typedefs are only rarely used.