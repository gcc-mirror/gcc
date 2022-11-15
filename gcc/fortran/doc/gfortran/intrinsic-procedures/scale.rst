..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SCALE, real number, scale, floating point, scale

.. _scale:

SCALE --- Scale a real value
****************************

.. function:: SCALE(X,I)

  ``SCALE(X,I)`` returns ``X * RADIX(X)**I``.

  :param X:
    The type of the argument shall be a ``REAL``.

  :param I:
    The type of the argument shall be a ``INTEGER``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    Its value is ``X * RADIX(X)**I``.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SCALE(X, I)

  Example:
    .. code-block:: fortran

      program test_scale
        real :: x = 178.1387e-4
        integer :: i = 5
        print *, scale(x,i), x*radix(x)**i
      end program test_scale
