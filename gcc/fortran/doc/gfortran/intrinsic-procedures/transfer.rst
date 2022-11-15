..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TRANSFER, bits, move, type cast

.. _transfer:

TRANSFER --- Transfer bit patterns
**********************************

.. function:: TRANSFER(SOURCE, MOLD, SIZE)

  Interprets the bitwise representation of :samp:`{SOURCE}` in memory as if it
  is the representation of a variable or array of the same type and type
  parameters as :samp:`{MOLD}`.

  :param SOURCE:
    Shall be a scalar or an array of any type.

  :param MOLD:
    Shall be a scalar or an array of any type.

  :param SIZE:
    (Optional) shall be a scalar of type
    ``INTEGER``.

  :return:
    The result has the same type as :samp:`{MOLD}`, with the bit level
    representation of :samp:`{SOURCE}`.  If :samp:`{SIZE}` is present, the result is
    a one-dimensional array of length :samp:`{SIZE}`.  If :samp:`{SIZE}` is absent
    but :samp:`{MOLD}` is an array (of any size or shape), the result is a one-
    dimensional array of the minimum length needed to contain the entirety
    of the bitwise representation of :samp:`{SOURCE}`.   If :samp:`{SIZE}` is absent
    and :samp:`{MOLD}` is a scalar, the result is a scalar.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = TRANSFER(SOURCE, MOLD[, SIZE])

  Example:
    .. code-block:: fortran

      PROGRAM test_transfer
        integer :: x = 2143289344
        print *, transfer(x, 1.0)    ! prints "NaN" on i686
      END PROGRAM
