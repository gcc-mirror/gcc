..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DIGITS, model representation, significant digits

.. _digits:

DIGITS --- Significant binary digits function
*********************************************

.. function:: DIGITS(X)

  ``DIGITS(X)`` returns the number of significant binary digits of the internal
  model representation of :samp:`{X}`.  For example, on a system using a 32-bit
  floating point representation, a default real number would likely return 24.

  :param X:
    The type may be ``INTEGER`` or ``REAL``.

  :return:
    The return value is of type ``INTEGER``.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = DIGITS(X)

  Example:
    .. code-block:: fortran

      program test_digits
          integer :: i = 12345
          real :: x = 3.143
          real(8) :: y = 2.33
          print *, digits(i)
          print *, digits(x)
          print *, digits(y)
      end program test_digits