..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MCLOCK, time, clock ticks, clock ticks

.. _mclock:

MCLOCK --- Time function
************************

.. function:: MCLOCK()

  Returns the number of clock ticks since the start of the process, based
  on the function ``clock(3)`` in the C standard library.

  :return:
    The return value is a scalar of type ``INTEGER(4)``, equal to the
    number of clock ticks since the start of the process, or ``-1`` if
    the system does not support ``clock(3)``.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = MCLOCK()

  See also:
    :ref:`CTIME`,
    :ref:`GMTIME`,
    :ref:`LTIME`,
    :ref:`MCLOCK`,
    :ref:`TIME`
