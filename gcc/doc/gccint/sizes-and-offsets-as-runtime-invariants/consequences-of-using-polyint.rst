..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Consequences of using poly_int
******************************

The two main consequences of using polynomial sizes and offsets are that:

* there is no total ordering between the values at compile time, and

* some operations might yield results that cannot be expressed as a
  ``poly_int``.

For example, if :samp:`{x}` is a runtime invariant, we cannot tell at
compile time whether:

.. code-block:: c++

  3 + 4x <= 1 + 5x

since the condition is false when :samp:`{x}` <= 1 and true when :samp:`{x}` >= 2.

Similarly, ``poly_int`` cannot represent the result of:

.. code-block:: c++

  (3 + 4x) * (1 + 5x)

since it cannot (and in practice does not need to) store powers greater
than one.  It also cannot represent the result of:

.. code-block:: c++

  (3 + 4x) / (1 + 5x)

The following sections describe how we deal with these restrictions.

.. index:: poly_int, use in target-independent code

As described earlier, a ``poly_int<1, T>`` has no indeterminates
and so degenerates to a compile-time constant of type :samp:`{T}`.  It would
be possible in that case to do all normal arithmetic on the :samp:`{T}`,
and to compare the :samp:`{T}` using the normal C++ operators.  We deliberately
prevent target-independent code from doing this, since the compiler needs
to support other ``poly_int<n, T>`` as well, regardless of
the current target's ``NUM_POLY_INT_COEFFS``.

.. index:: poly_int, use in target-specific code

However, it would be very artificial to force target-specific code
to follow these restrictions if the target has no runtime indeterminates.
There is therefore an implicit conversion from ``poly_int<1, T>``
to :samp:`{T}` when compiling target-specific translation units.
