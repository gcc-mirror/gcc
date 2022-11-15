..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FRACTION, real number, fraction, floating point, fraction

.. _fraction:

FRACTION --- Fractional part of the model representation
********************************************************

.. function:: FRACTION(X)

  ``FRACTION(X)`` returns the fractional part of the model
  representation of ``X``.

  :param X:
    The type of the argument shall be a ``REAL``.

  :return:
    The return value is of the same type and kind as the argument.
    The fractional part of the model representation of ``X`` is returned;
    it is ``X * RADIX(X)**(-EXPONENT(X))``.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      Y = FRACTION(X)

  Example:
    .. code-block:: fortran

      program test_fraction
        real :: x
        x = 178.1387e-4
        print *, fraction(x), x * radix(x)**(-exponent(x))
      end program test_fraction
