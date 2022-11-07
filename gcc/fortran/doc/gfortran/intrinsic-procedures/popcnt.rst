..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: POPCNT, binary representation, bits set

.. _popcnt:

POPCNT --- Number of bits set
*****************************

.. function:: POPCNT(I)

  ``POPCNT(I)`` returns the number of bits set ('1' bits) in the binary
  representation of ``I``.

  :param I:
    Shall be of type ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = POPCNT(I)

  Example:
    .. code-block:: fortran

      program test_population
        print *, popcnt(127),       poppar(127)
        print *, popcnt(huge(0_4)), poppar(huge(0_4))
        print *, popcnt(huge(0_8)), poppar(huge(0_8))
      end program test_population

  See also:
    :ref:`POPPAR`,
    :ref:`LEADZ`,
    :ref:`TRAILZ`