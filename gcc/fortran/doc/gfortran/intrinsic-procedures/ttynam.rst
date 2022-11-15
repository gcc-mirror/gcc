..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TTYNAM, system, terminal

.. _ttynam:

TTYNAM --- Get the name of a terminal device
********************************************

.. function:: TTYNAM(UNIT)

  Get the name of a terminal device. For more information,
  see ``ttyname(3)``.

  :param UNIT:
    Shall be a scalar ``INTEGER``.

  :param NAME:
    Shall be of type ``CHARACTER``.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL TTYNAM(UNIT, NAME)
      NAME = TTYNAM(UNIT)

  Example:
    .. code-block:: fortran

      PROGRAM test_ttynam
        INTEGER :: unit
        DO unit = 1, 10
          IF (isatty(unit=unit)) write(*,*) ttynam(unit)
        END DO
      END PROGRAM

  See also:
    :ref:`ISATTY`
