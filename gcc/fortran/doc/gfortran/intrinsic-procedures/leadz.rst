..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _leadz:

LEADZ --- Number of leading zero bits of an integer
***************************************************

.. index:: LEADZ, zero bits

.. function:: LEADZ(I)

  ``LEADZ`` returns the number of leading zero bits of an integer.

  :param I:
    Shall be of type ``INTEGER``.

  :return:
    The type of the return value is the default ``INTEGER``.
    If all the bits of ``I`` are zero, the result value is ``BIT_SIZE(I)``.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = LEADZ(I)

  Example:
    .. code-block:: fortran

      PROGRAM test_leadz
        WRITE (*,*) BIT_SIZE(1)  ! prints 32
        WRITE (*,*) LEADZ(1)     ! prints 31
      END PROGRAM

  See also:
    :ref:`BIT_SIZE`,
    :ref:`TRAILZ`,
    :ref:`POPCNT`,
    :ref:`POPPAR`