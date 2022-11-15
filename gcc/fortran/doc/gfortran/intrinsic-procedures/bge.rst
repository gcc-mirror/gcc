..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: BGE, bitwise comparison

.. _bge:

BGE --- Bitwise greater than or equal to
****************************************

.. function:: BGE(I, J)

  Determines whether an integral is a bitwise greater than or equal to
  another.

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

      RESULT = BGE(I, J)

  See also:
    :ref:`BGT`,
    :ref:`BLE`,
    :ref:`BLT`
