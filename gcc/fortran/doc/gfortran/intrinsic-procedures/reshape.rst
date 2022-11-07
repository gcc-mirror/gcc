..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RESHAPE, array, change dimensions, array, transmogrify

.. _reshape:

RESHAPE --- Function to reshape an array
****************************************

.. function:: RESHAPE(SOURCE, SHAPE, PAD, ORDER)

  Reshapes :samp:`{SOURCE}` to correspond to :samp:`{SHAPE}`. If necessary,
  the new array may be padded with elements from :samp:`{PAD}` or permuted
  as defined by :samp:`{ORDER}`.

  :param SOURCE:
    Shall be an array of any type.

  :param SHAPE:
    Shall be of type ``INTEGER`` and an
    array of rank one. Its values must be positive or zero.

  :param PAD:
    (Optional) shall be an array of the same
    type as :samp:`{SOURCE}`.

  :param ORDER:
    (Optional) shall be of type ``INTEGER``
    and an array of the same shape as :samp:`{SHAPE}`. Its values shall
    be a permutation of the numbers from 1 to n, where n is the size of
    :samp:`{SHAPE}`. If :samp:`{ORDER}` is absent, the natural ordering shall
    be assumed.

  :return:
    The result is an array of shape :samp:`{SHAPE}` with the same type as
    :samp:`{SOURCE}`.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = RESHAPE(SOURCE, SHAPE[, PAD, ORDER])

  Example:
    .. code-block:: fortran

      PROGRAM test_reshape
        INTEGER, DIMENSION(4) :: x
        WRITE(*,*) SHAPE(x)                       ! prints "4"
        WRITE(*,*) SHAPE(RESHAPE(x, (/2, 2/)))    ! prints "2 2"
      END PROGRAM

  See also:
    :ref:`SHAPE`