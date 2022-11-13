..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IDATE, date, current, current date

.. _idate:

IDATE --- Get current local time subroutine (day/month/year)
*************************************************************

.. function:: IDATE(VALUES)

  ``IDATE(VALUES)`` Fills :samp:`{VALUES}` with the numerical values at the
  current local time. The day (in the range 1-31), month (in the range 1-12),
  and year appear in elements 1, 2, and 3 of :samp:`{VALUES}`, respectively.
  The year has four significant digits.

  :param VALUES:
    The type shall be ``INTEGER, DIMENSION(3)`` and
    the kind shall be the default integer kind.

  :return:
    Does not return anything.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL IDATE(VALUES)

  Example:
    .. code-block:: fortran

      program test_idate
        integer, dimension(3) :: tarray
        call idate(tarray)
        print *, tarray(1)
        print *, tarray(2)
        print *, tarray(3)
      end program test_idate

  See also:
    :ref:`DATE_AND_TIME`