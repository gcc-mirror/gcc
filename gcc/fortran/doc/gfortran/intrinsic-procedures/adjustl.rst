..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ADJUSTL, string, adjust left, adjust string

.. _adjustl:

ADJUSTL --- Left adjust a string
*********************************

.. function:: ADJUSTL(STRING)

  ``ADJUSTL(STRING)`` will left adjust a string by removing leading spaces.
  Spaces are inserted at the end of the string as needed.

  :param STRING:
    The type shall be ``CHARACTER``.

  :return:
    The return value is of type ``CHARACTER`` and of the same kind as
    :samp:`{STRING}` where leading spaces are removed and the same number of
    spaces are inserted on the end of :samp:`{STRING}`.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ADJUSTL(STRING)

  Example:
    .. code-block:: fortran

      program test_adjustl
        character(len=20) :: str = '   gfortran'
        str = adjustl(str)
        print *, str
      end program test_adjustl

  See also:
    :ref:`ADJUSTR`,
    :ref:`TRIM`