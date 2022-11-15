..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETLOG, system, login name, login name

.. _getlog:

GETLOG --- Get login name
*************************

.. function:: GETLOG(C)

  Gets the username under which the program is running.

  :param C:
    Shall be of type ``CHARACTER`` and of default kind.

  :return:
    Stores the current user name in :samp:`{C}`.  (On systems where POSIX
    functions ``geteuid`` and ``getpwuid`` are not available, and
    the ``getlogin`` function is not implemented either, this will
    return a blank string.)

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GETLOG(C)

  Example:
    .. code-block:: fortran

      PROGRAM TEST_GETLOG
        CHARACTER(32) :: login
        CALL GETLOG(login)
        WRITE(*,*) login
      END PROGRAM

  See also:
    :ref:`GETUID`
