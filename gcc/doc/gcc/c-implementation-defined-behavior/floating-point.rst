..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _floating-point-implementation:

Floating Point
**************

* The accuracy of the floating-point operations and of the library
  functions in ``<math.h>`` and ``<complex.h>`` that return floating-point
  results (C90, C99 and C11 5.2.4.2.2).

  The accuracy is unknown.

* The rounding behaviors characterized by non-standard values
  of ``FLT_ROUNDS``
  (C90, C99 and C11 5.2.4.2.2).

  GCC does not use such values.

* The evaluation methods characterized by non-standard negative
  values of ``FLT_EVAL_METHOD`` (C99 and C11 5.2.4.2.2).

  GCC does not use such values.

* The direction of rounding when an integer is converted to a
  floating-point number that cannot exactly represent the original
  value (C90 6.2.1.3, C99 and C11 6.3.1.4).

  C99 Annex F is followed.

* The direction of rounding when a floating-point number is
  converted to a narrower floating-point number (C90 6.2.1.4, C99 and C11
  6.3.1.5).

  C99 Annex F is followed.

* How the nearest representable value or the larger or smaller
  representable value immediately adjacent to the nearest representable
  value is chosen for certain floating constants (C90 6.1.3.1, C99 and C11
  6.4.4.2).

  C99 Annex F is followed.

* Whether and how floating expressions are contracted when not
  disallowed by the ``FP_CONTRACT`` pragma (C99 and C11 6.5).

  Expressions are currently only contracted if :option:`-ffp-contract=fast`,
  :option:`-funsafe-math-optimizations` or :option:`-ffast-math` are used.
  This is subject to change.

* The default state for the ``FENV_ACCESS`` pragma (C99 and C11
  7.6.1).

  This pragma is not implemented, but the default is to 'off' unless
  :option:`-frounding-math` is used and :option:`-fno-trapping-math` is not
  in which case it is 'on'.

* Additional floating-point exceptions, rounding modes, environments,
  and classifications, and their macro names (C99 and C11 7.6, C99 and
  C11 7.12).

  This is dependent on the implementation of the C library, and is not
  defined by GCC itself.

* The default state for the ``FP_CONTRACT`` pragma (C99 and C11
  7.12.2).

  This pragma is not implemented.  Expressions are currently only
  contracted if :option:`-ffp-contract=fast`,
  :option:`-funsafe-math-optimizations` or :option:`-ffast-math` are used.
  This is subject to change.

* Whether the 'inexact' floating-point exception can be raised
  when the rounded result actually does equal the mathematical result
  in an IEC 60559 conformant implementation (C99 F.9).

  This is dependent on the implementation of the C library, and is not
  defined by GCC itself.

* Whether the 'underflow' (and 'inexact') floating-point
  exception can be raised when a result is tiny but not inexact in an
  IEC 60559 conformant implementation (C99 F.9).

  This is dependent on the implementation of the C library, and is not
  defined by GCC itself.
