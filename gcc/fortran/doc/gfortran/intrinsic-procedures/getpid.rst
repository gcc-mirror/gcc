..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETPID, system, process ID, process ID

.. _getpid:

GETPID --- Process ID function
******************************

.. function:: GETPID()

  Returns the numerical process identifier of the current process.

  :return:
    The return value of ``GETPID`` is an ``INTEGER`` of the default
    kind.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = GETPID()

  Example:
    .. code-block:: fortran

      program info
        print *, "The current process ID is ", getpid()
        print *, "Your numerical user ID is ", getuid()
        print *, "Your numerical group ID is ", getgid()
      end program info

  See also:
    :ref:`GETGID`,
    :ref:`GETUID`
