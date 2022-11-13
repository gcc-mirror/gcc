..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ITIME, time, current, current time

.. _itime:

ITIME --- Get current local time subroutine (hour/minutes/seconds)
*******************************************************************

.. function:: ITIME(VALUES)

  ``ITIME(VALUES)`` Fills :samp:`{VALUES}` with the numerical values at the
  current local time. The hour (in the range 1-24), minute (in the range 1-60),
  and seconds (in the range 1-60) appear in elements 1, 2, and 3 of :samp:`{VALUES}`,
  respectively.

  :param VALUES:
    The type shall be ``INTEGER, DIMENSION(3)``
    and the kind shall be the default integer kind.

  :return:
    Does not return anything.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL ITIME(VALUES)

  Example:
    .. code-block:: fortran

      program test_itime
        integer, dimension(3) :: tarray
        call itime(tarray)
        print *, tarray(1)
        print *, tarray(2)
        print *, tarray(3)
      end program test_itime

  See also:
    :ref:`DATE_AND_TIME`