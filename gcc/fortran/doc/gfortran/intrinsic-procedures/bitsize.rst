..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: BIT_SIZE, bits, number of, size of a variable, in bits

.. _bit_size:

BIT_SIZE --- Bit size inquiry function
**************************************

.. function:: BIT_SIZE(I)

  ``BIT_SIZE(I)`` returns the number of bits (integer precision plus sign bit)
  represented by the type of :samp:`{I}`.  The result of ``BIT_SIZE(I)`` is
  independent of the actual value of :samp:`{I}`.

  :param I:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``

  Standard:
    Fortran 90 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = BIT_SIZE(I)

  Example:
    .. code-block:: fortran

      program test_bit_size
          integer :: i = 123
          integer :: size
          size = bit_size(i)
          print *, size
      end program test_bit_size