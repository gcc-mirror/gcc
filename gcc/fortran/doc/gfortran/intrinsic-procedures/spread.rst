..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SPREAD, array, increase dimension, array, duplicate elements, array, duplicate dimensions

.. _spread:

SPREAD --- Add a dimension to an array
**************************************

.. function:: SPREAD(SOURCE, DIM, NCOPIES)

  Replicates a :samp:`{SOURCE}` array :samp:`{NCOPIES}` times along a specified
  dimension :samp:`{DIM}`.

  :param SOURCE:
    Shall be a scalar or an array of any type and
    a rank less than seven.

  :param DIM:
    Shall be a scalar of type ``INTEGER`` with a
    value in the range from 1 to n+1, where n equals the rank of :samp:`{SOURCE}`.

  :param NCOPIES:
    Shall be a scalar of type ``INTEGER``.

  :return:
    The result is an array of the same type as :samp:`{SOURCE}` and has rank n+1
    where n equals the rank of :samp:`{SOURCE}`.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = SPREAD(SOURCE, DIM, NCOPIES)

  Example:
    .. code-block:: fortran

      PROGRAM test_spread
        INTEGER :: a = 1, b(2) = (/ 1, 2 /)
        WRITE(*,*) SPREAD(A, 1, 2)            ! "1 1"
        WRITE(*,*) SPREAD(B, 1, 2)            ! "1 1 2 2"
      END PROGRAM

  See also:
    :ref:`UNPACK`