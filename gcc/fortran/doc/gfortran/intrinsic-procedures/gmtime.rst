..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GMTIME, time, conversion to GMT info

.. _gmtime:

GMTIME --- Convert time to GMT info
***********************************

.. function:: GMTIME(TIME, VALUES)

  Given a system time value :samp:`{TIME}` (as provided by the :ref:`TIME`
  intrinsic), fills :samp:`{VALUES}` with values extracted from it appropriate
  to the UTC time zone (Universal Coordinated Time, also known in some
  countries as GMT, Greenwich Mean Time), using ``gmtime(3)``.

  :param TIME:
    An ``INTEGER`` scalar expression
    corresponding to a system time, with ``INTENT(IN)``.

  :param VALUES:
    A default ``INTEGER`` array with 9 elements,
    with ``INTENT(OUT)``.

  :return:
    The elements of :samp:`{VALUES}` are assigned as follows:

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GMTIME(TIME, VALUES)

  See also:
    :ref:`DATE_AND_TIME`,
    :ref:`CTIME`,
    :ref:`LTIME`,
    :ref:`TIME`,
    :ref:`TIME8`