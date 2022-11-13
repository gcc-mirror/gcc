..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SLEEP, delayed execution

.. _sleep:

SLEEP --- Sleep for the specified number of seconds
***************************************************

.. function:: SLEEP(SECONDS)

  Calling this subroutine causes the process to pause for :samp:`{SECONDS}` seconds.

  :param SECONDS:
    The type shall be of default ``INTEGER``.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL SLEEP(SECONDS)

  Example:
    .. code-block:: fortran

      program test_sleep
        call sleep(5)
      end