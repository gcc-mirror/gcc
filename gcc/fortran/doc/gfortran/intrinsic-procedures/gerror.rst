..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GERROR, system, error handling

.. _gerror:

GERROR --- Get last system error message
****************************************

.. function:: GERROR(RESULT)

  Returns the system error message corresponding to the last system error.
  This resembles the functionality of ``strerror(3)`` in C.

  :param RESULT:
    Shall be of type ``CHARACTER`` and of default kind.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GERROR(RESULT)

  Example:
    .. code-block:: fortran

      PROGRAM test_gerror
        CHARACTER(len=100) :: msg
        CALL gerror(msg)
        WRITE(*,*) msg
      END PROGRAM

  See also:
    :ref:`IERRNO`,
    :ref:`PERROR`