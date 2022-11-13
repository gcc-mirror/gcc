..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _abort:

ABORT --- Abort the program
***************************

.. index:: ABORT, program termination, with core dump, terminate program, with core dump, core, dump

.. function:: ABORT()

  ``ABORT`` causes immediate termination of the program.  On operating
  systems that support a core dump, ``ABORT`` will produce a core dump.
  It will also print a backtrace, unless ``-fno-backtrace`` is given.

  :return:
    Does not return.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL ABORT

  Example:
    .. code-block:: fortran

      program test_abort
        integer :: i = 1, j = 2
        if (i /= j) call abort
      end program test_abort

  See also:
    :ref:`EXIT`,
    :ref:`KILL`,
    :ref:`BACKTRACE`