..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SPACING, real number, relative spacing, floating point, relative spacing

.. _spacing:

SPACING --- Smallest distance between two numbers of a given type
*****************************************************************

.. function:: SPACING(X)

  Determines the distance between the argument :samp:`{X}` and the nearest
  adjacent number of the same type.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The result is of the same type as the input argument :samp:`{X}`.

  Standard:
    Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SPACING(X)

  Example:
    .. code-block:: fortran

      PROGRAM test_spacing
        INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6, r=37)
        INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13, r=200)

        WRITE(*,*) spacing(1.0_SGL)      ! "1.1920929E-07"          on i686
        WRITE(*,*) spacing(1.0_DBL)      ! "2.220446049250313E-016" on i686
      END PROGRAM

  See also:
    :ref:`RRSPACING`