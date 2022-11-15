..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: NORM2, Euclidean vector norm, L2 vector norm, norm, Euclidean

.. _norm2:

NORM2 --- Euclidean vector norms
********************************

.. function:: NORM2(ARRAY, DIM)

  Calculates the Euclidean vector norm (L_2 norm)
  of :samp:`{ARRAY}` along dimension :samp:`{DIM}`.

  :param ARRAY:
    Shall be an array of type ``REAL``

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{ARRAY}`.

  :return:
    The result is of the same type as :samp:`{ARRAY}`.

  Standard:
    Fortran 2008 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = NORM2(ARRAY[, DIM])

  Example:
    .. code-block:: fortran

      PROGRAM test_sum
        REAL :: x(5) = [ real :: 1, 2, 3, 4, 5 ]
        print *, NORM2(x)  ! = sqrt(55.) ~ 7.416
      END PROGRAM
