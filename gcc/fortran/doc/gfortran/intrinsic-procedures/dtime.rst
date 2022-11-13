..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DTIME, time, elapsed, elapsed time

.. _dtime:

DTIME --- Execution time subroutine (or function)
*************************************************

.. function:: DTIME(VALUES, TIME)

  ``DTIME(VALUES, TIME)`` initially returns the number of seconds of runtime
  since the start of the process's execution in :samp:`{TIME}`.  :samp:`{VALUES}`
  returns the user and system components of this time in ``VALUES(1)`` and
  ``VALUES(2)`` respectively. :samp:`{TIME}` is equal to ``VALUES(1) +
  VALUES(2)``.

  :param VALUES:
    The type shall be ``REAL(4), DIMENSION(2)``.

  :param TIME:
    The type shall be ``REAL(4)``.

  :return:
    Elapsed time in seconds since the last invocation or since the start of program
    execution if not called before.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL DTIME(VALUES, TIME).
      TIME = DTIME(VALUES), (not recommended).

  Example:
    .. code-block:: fortran

      program test_dtime
          integer(8) :: i, j
          real, dimension(2) :: tarray
          real :: result
          call dtime(tarray, result)
          print *, result
          print *, tarray(1)
          print *, tarray(2)
          do i=1,100000000    ! Just a delay
              j = i * i - i
          end do
          call dtime(tarray, result)
          print *, result
          print *, tarray(1)
          print *, tarray(2)
      end program test_dtime

  See also:
    :ref:`CPU_TIME`