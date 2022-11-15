..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: VERIFY, string, find missing set

.. _verify:

VERIFY --- Scan a string for characters not a given set
*******************************************************

.. function:: VERIFY(STRING, SET, BACK , KIND)

  Verifies that all the characters in :samp:`{STRING}` belong to the set of
  characters in :samp:`{SET}`.

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

      RESULT = VERIFY(STRING, SET[, BACK [, KIND]])

  Example:
    .. code-block:: fortran

      PROGRAM test_verify
        WRITE(*,*) VERIFY("FORTRAN", "AO")           ! 1, found 'F'
        WRITE(*,*) VERIFY("FORTRAN", "FOO")          ! 3, found 'R'
        WRITE(*,*) VERIFY("FORTRAN", "C++")          ! 1, found 'F'
        WRITE(*,*) VERIFY("FORTRAN", "C++", .TRUE.)  ! 7, found 'N'
        WRITE(*,*) VERIFY("FORTRAN", "FORTRAN")      ! 0' found none
      END PROGRAM

  See also:
    :ref:`SCAN`,
    :ref:`index-intrinsic`
