..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RRSPACING, real number, relative spacing, floating point, relative spacing

.. _rrspacing:

RRSPACING --- Reciprocal of the relative spacing
************************************************

.. function:: RRSPACING(X)

  ``RRSPACING(X)`` returns the  reciprocal of the relative spacing of
  model numbers near :samp:`{X}`.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The value returned is equal to
    ``ABS(FRACTION(X)) * FLOAT(RADIX(X))**DIGITS(X)``.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = RRSPACING(X)

  See also:
    :ref:`SPACING`