..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SET_EXPONENT, real number, set exponent, floating point, set exponent

.. _set_exponent:

SET_EXPONENT --- Set the exponent of the model
**********************************************

.. function:: SET_EXPONENT(X, I)

  ``SET_EXPONENT(X, I)`` returns the real number whose fractional part
  is that of :samp:`{X}` and whose exponent part is :samp:`{I}`.

  :param X:
    Shall be of type ``REAL``.

  :param I:
    Shall be of type ``INTEGER``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The real number whose fractional part
    is that of :samp:`{X}` and whose exponent part if :samp:`{I}` is returned;
    it is ``FRACTION(X) * RADIX(X)**I``.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SET_EXPONENT(X, I)

  Example:
    .. code-block:: fortran

      PROGRAM test_setexp
        REAL :: x = 178.1387e-4
        INTEGER :: i = 17
        PRINT *, SET_EXPONENT(x, i), FRACTION(x) * RADIX(x)**i
      END PROGRAM