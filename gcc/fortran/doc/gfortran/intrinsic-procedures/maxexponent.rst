..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MAXEXPONENT, model representation, maximum exponent

.. _maxexponent:

MAXEXPONENT --- Maximum exponent of a real kind
***********************************************

.. function:: MAXEXPONENT(X)

  ``MAXEXPONENT(X)`` returns the maximum exponent in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = MAXEXPONENT(X)

  Example:
    .. code-block:: fortran

      program exponents
        real(kind=4) :: x
        real(kind=8) :: y

        print *, minexponent(x), maxexponent(x)
        print *, minexponent(y), maxexponent(y)
      end program exponents