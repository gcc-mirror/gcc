..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: EPSILON, model representation, epsilon

.. _epsilon:

EPSILON --- Epsilon function
****************************

.. function:: EPSILON(X)

  ``EPSILON(X)`` returns the smallest number :samp:`{E}` of the same kind
  as :samp:`{X}` such that 1 + E > 1.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of same type as the argument.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = EPSILON(X)

  Example:
    .. code-block:: fortran

      program test_epsilon
          real :: x = 3.143
          real(8) :: y = 2.33
          print *, EPSILON(x)
          print *, EPSILON(y)
      end program test_epsilon