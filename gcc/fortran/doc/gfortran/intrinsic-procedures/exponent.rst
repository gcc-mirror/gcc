..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: EXPONENT, real number, exponent, floating point, exponent

.. _exponent:

EXPONENT --- Exponent function
*******************************

.. function:: EXPONENT(X)

  ``EXPONENT(X)`` returns the value of the exponent part of :samp:`{X}`. If :samp:`{X}`
  is zero the value returned is zero.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type default ``INTEGER``.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = EXPONENT(X)

  Example:
    .. code-block:: fortran

      program test_exponent
        real :: x = 1.0
        integer :: i
        i = exponent(x)
        print *, i
        print *, exponent(0.0)
      end program test_exponent