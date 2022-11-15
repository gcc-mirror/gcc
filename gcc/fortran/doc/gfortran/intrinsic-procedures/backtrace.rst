..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _backtrace:

BACKTRACE --- Show a backtrace
******************************

.. index:: BACKTRACE, backtrace

.. function:: BACKTRACE()

  ``BACKTRACE`` shows a backtrace at an arbitrary place in user code. Program
  execution continues normally afterwards. The backtrace information is printed
  to the unit corresponding to ``ERROR_UNIT`` in ``ISO_FORTRAN_ENV``.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL BACKTRACE

  Arguments:
    None

  See also:
    :ref:`ABORT`
