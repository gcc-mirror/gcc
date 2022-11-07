..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ADJUSTR, string, adjust right, adjust string

.. _adjustr:

ADJUSTR --- Right adjust a string
**********************************

.. function:: ADJUSTR(STRING)

  ``ADJUSTR(STRING)`` will right adjust a string by removing trailing spaces.
  Spaces are inserted at the start of the string as needed.

  :param STR:
    The type shall be ``CHARACTER``.

  :return:
    The return value is of type ``CHARACTER`` and of the same kind as
    :samp:`{STRING}` where trailing spaces are removed and the same number of
    spaces are inserted at the start of :samp:`{STRING}`.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ADJUSTR(STRING)

  Example:
    .. code-block:: fortran

      program test_adjustr
        character(len=20) :: str = 'gfortran'
        str = adjustr(str)
        print *, str
      end program test_adjustr

  See also:
    :ref:`ADJUSTL`,
    :ref:`TRIM`