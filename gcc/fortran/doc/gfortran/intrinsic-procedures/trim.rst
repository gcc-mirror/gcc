..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TRIM, string, remove trailing whitespace

.. _trim:

TRIM --- Remove trailing blank characters of a string
*****************************************************

.. function:: TRIM(STRING)

  Removes trailing blank characters of a string.

  :param STRING:
    Shall be a scalar of type ``CHARACTER``.

  :return:
    A scalar of type ``CHARACTER`` which length is that of :samp:`{STRING}`
    less the number of trailing blanks.

  Standard:
    Fortran 90 and later

  Class:
    Transformational function

  Syntax:
    .. code-block:: fortran

      RESULT = TRIM(STRING)

  Example:
    .. code-block:: fortran

      PROGRAM test_trim
        CHARACTER(len=10), PARAMETER :: s = "GFORTRAN  "
        WRITE(*,*) LEN(s), LEN(TRIM(s))  ! "10 8", with/without trailing blanks
      END PROGRAM

  See also:
    :ref:`ADJUSTL`,
    :ref:`ADJUSTR`
