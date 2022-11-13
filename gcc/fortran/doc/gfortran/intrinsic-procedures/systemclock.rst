..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SYSTEM_CLOCK, time, clock ticks, clock ticks

.. _system_clock:

SYSTEM_CLOCK --- Time function
******************************

.. function:: SYSTEM_CLOCK(COUNT, COUNT_RATE, COUNT_MAX)

  Determines the :samp:`{COUNT}` of a processor clock since an unspecified
  time in the past modulo :samp:`{COUNT_MAX}`, :samp:`{COUNT_RATE}` determines
  the number of clock ticks per second.  If the platform supports a
  monotonic clock, that clock is used and can, depending on the platform
  clock implementation, provide up to nanosecond resolution.  If a
  monotonic clock is not available, the implementation falls back to a
  realtime clock.

  :param COUNT:
    (Optional) shall be a scalar of type
    ``INTEGER`` with ``INTENT(OUT)``.

  :param COUNT_RATE:
    (Optional) shall be a scalar of type
    ``INTEGER`` or ``REAL``, with ``INTENT(OUT)``.

  :param COUNT_MAX:
    (Optional) shall be a scalar of type
    ``INTEGER`` with ``INTENT(OUT)``.

  Standard:
    Fortran 90 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL SYSTEM_CLOCK([COUNT, COUNT_RATE, COUNT_MAX])

  Example:
    .. code-block:: fortran

      PROGRAM test_system_clock
        INTEGER :: count, count_rate, count_max
        CALL SYSTEM_CLOCK(count, count_rate, count_max)
        WRITE(*,*) count, count_rate, count_max
      END PROGRAM

  See also:
    :ref:`DATE_AND_TIME`,
    :ref:`CPU_TIME`