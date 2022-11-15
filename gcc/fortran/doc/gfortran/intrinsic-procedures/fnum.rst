..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FNUM, file operation, file number

.. _fnum:

FNUM --- File number function
*****************************

.. function:: FNUM(UNIT)

  ``FNUM(UNIT)`` returns the POSIX file descriptor number corresponding to the
  open Fortran I/O unit ``UNIT``.

  :param UNIT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = FNUM(UNIT)

  Example:
    .. code-block:: fortran

      program test_fnum
        integer :: i
        open (unit=10, status = "scratch")
        i = fnum(10)
        print *, i
        close (10)
      end program test_fnum
