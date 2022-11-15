..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SCAN, string, find subset

.. _scan:

SCAN --- Scan a string for the presence of a set of characters
**************************************************************

.. function:: SCAN(STRING, SET, BACK , KIND)

  Scans a :samp:`{STRING}` for any of the characters in a :samp:`{SET}`
  of characters.

  :param STRING:
    Shall be of type ``CHARACTER``.

  :param SET:
    Shall be of type ``CHARACTER``.

  :param BACK:
    (Optional) shall be of type ``LOGICAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  Standard:
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SCAN(STRING, SET[, BACK [, KIND]])

  Example:
    .. code-block:: fortran

      PROGRAM test_scan
        WRITE(*,*) SCAN("FORTRAN", "AO")          ! 2, found 'O'
        WRITE(*,*) SCAN("FORTRAN", "AO", .TRUE.)  ! 6, found 'A'
        WRITE(*,*) SCAN("FORTRAN", "C++")         ! 0, found none
      END PROGRAM

  See also:
    :ref:`index-intrinsic`,
    :ref:`VERIFY`
