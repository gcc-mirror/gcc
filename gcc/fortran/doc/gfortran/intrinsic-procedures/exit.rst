..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exit:

EXIT --- Exit the program with status.
***************************************

.. index:: EXIT, program termination, terminate program

.. function:: EXIT(STATUS)

  ``EXIT`` causes immediate termination of the program with status.  If status
  is omitted it returns the canonical *success* for the system.  All Fortran
  I/O units are closed.

  :param STATUS:
    Shall be an ``INTEGER`` of the default kind.

  :return:
    ``STATUS`` is passed to the parent process on exit.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL EXIT([STATUS])

  Example:
    .. code-block:: fortran

      program test_exit
        integer :: STATUS = 0
        print *, 'This program is going to exit.'
        call EXIT(STATUS)
      end program test_exit

  See also:
    :ref:`ABORT`,
    :ref:`KILL`
