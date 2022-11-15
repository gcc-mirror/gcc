..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: BLT, bitwise comparison

.. _blt:

BLT --- Bitwise less than
*************************

.. function:: BLT(I, J)

  Determines whether an integral is a bitwise less than another.

  :param I:
    Shall be of ``INTEGER`` type.

  :param J:
    Shall be of ``INTEGER`` type, and of the same kind
    as :samp:`{I}`.

  :return:
    The return value is of type ``LOGICAL`` and of the default kind.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = BLT(I, J)

  See also:
    :ref:`BGE`,
    :ref:`BGT`,
    :ref:`BLE`
