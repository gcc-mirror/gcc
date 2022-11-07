..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IANY, array, OR, bits, OR of array elements

.. _iany:

IANY --- Bitwise OR of array elements
*************************************

.. function:: IANY(ARRAY, DIM, MASK)

  Reduces with bitwise OR (inclusive or) the elements of :samp:`{ARRAY}` along
  dimension :samp:`{DIM}` if the corresponding element in :samp:`{MASK}` is ``TRUE``.

  :param ARRAY:
    Shall be an array of type ``INTEGER``

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{ARRAY}`.

  :param MASK:
    (Optional) shall be of type ``LOGICAL``
    and either be a scalar or an array of the same shape as :samp:`{ARRAY}`.

  :return:
    The result is of the same type as :samp:`{ARRAY}`.

  Standard:
    Fortran 2008 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = IANY(ARRAY[, MASK])
      RESULT = IANY(ARRAY, DIM[, MASK])

  Example:
    .. code-block:: fortran

      PROGRAM test_iany
        INTEGER(1) :: a(2)

        a(1) = b'00100100'
        a(2) = b'01101010'

        ! prints 01101110
        PRINT '(b8.8)', IANY(a)
      END PROGRAM

  See also:
    :ref:`IPARITY`,
    :ref:`IALL`,
    :ref:`IOR`