..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DBLE, conversion, to real

.. _dble:

DBLE --- Double conversion function
***********************************

.. function:: DBLE(A)

  ``DBLE(A)`` Converts :samp:`{A}` to double precision real type.

  :param A:
    The type shall be ``INTEGER``, ``REAL``,
    or ``COMPLEX``.

  :return:
    The return value is of type double precision real.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = DBLE(A)

  Example:
    .. code-block:: fortran

      program test_dble
          real    :: x = 2.18
          integer :: i = 5
          complex :: z = (2.3,1.14)
          print *, dble(x), dble(i), dble(z)
      end program test_dble

  See also:
    :ref:`REAL`
