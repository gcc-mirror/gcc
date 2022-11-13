..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FTELL, file operation, position

.. _ftell:

FTELL --- Current stream position
*********************************

.. function:: FTELL(UNIT)

  Retrieves the current position within an open file.

  :param OFFSET:
    Shall of type ``INTEGER``.

  :param UNIT:
    Shall of type ``INTEGER``.

  :return:
    In either syntax, :samp:`{OFFSET}` is set to the current offset of unit
    number :samp:`{UNIT}`, or to -1 if the unit is not currently open.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL FTELL(UNIT, OFFSET)
      OFFSET = FTELL(UNIT)

  Example:
    .. code-block:: fortran

      PROGRAM test_ftell
        INTEGER :: i
        OPEN(10, FILE="temp.dat")
        CALL ftell(10,i)
        WRITE(*,*) i
      END PROGRAM

  See also:
    :ref:`FSEEK`