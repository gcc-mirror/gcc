..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _ctime:

CTIME --- Convert a time into a string
**************************************

.. index:: CTIME, time, conversion to string, conversion, to string

.. function:: CTIME(TIME, RESULT)

  ``CTIME`` converts a system time value, such as returned by
  :ref:`TIME8`, to a string. The output will be of the form :samp:`Sat
  Aug 19 18:13:14 1995`.

  :param TIME:
    The type shall be of type ``INTEGER``.

  :param RESULT:
    The type shall be of type ``CHARACTER`` and
    of default kind. It is an ``INTENT(OUT)`` argument. If the length
    of this variable is too short for the time and date string to fit
    completely, it will be blank on procedure return.

  :return:
    The converted date and time as a string.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL CTIME(TIME, RESULT).
      RESULT = CTIME(TIME).

  Example:
    .. code-block:: fortran

      program test_ctime
          integer(8) :: i
          character(len=30) :: date
          i = time8()

          ! Do something, main part of the program

          call ctime(i,date)
          print *, 'Program was started on ', date
      end program test_ctime

  See Also:
    :ref:`DATE_AND_TIME`,
    :ref:`GMTIME`,
    :ref:`LTIME`,
    :ref:`TIME`,
    :ref:`TIME8`
