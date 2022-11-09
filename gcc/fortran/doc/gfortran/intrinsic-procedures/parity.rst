..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: PARITY, Parity, Reduction, XOR, XOR reduction

.. _parity:

PARITY --- Reduction with exclusive OR
**************************************

.. function:: PARITY(MASK, DIM)

  Calculates the parity, i.e. the reduction using ``.XOR.``,
  of :samp:`{MASK}` along dimension :samp:`{DIM}`.

  :param MASK:
    Shall be an array of type ``LOGICAL``

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{MASK}`.

  :return:
    The result is of the same type as :samp:`{MASK}`.

  Standard:
    Fortran 2008 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = PARITY(MASK[, DIM])

  Example:
    .. code-block:: fortran

      PROGRAM test_sum
        LOGICAL :: x(2) = [ .true., .false. ]
        print *, PARITY(x) ! prints "T" (true).
      END PROGRAM
