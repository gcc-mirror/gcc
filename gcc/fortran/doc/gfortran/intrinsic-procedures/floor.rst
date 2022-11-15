..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FLOOR, floor, rounding, floor

.. _floor:

FLOOR --- Integer floor function
********************************

.. function:: FLOOR(A)

  ``FLOOR(A)`` returns the greatest integer less than or equal to :samp:`{X}`.

  :param A:
    The type shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
    and of default-kind ``INTEGER`` otherwise.

  Standard:
    Fortran 95 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = FLOOR(A [, KIND])

  Example:
    .. code-block:: fortran

      program test_floor
          real :: x = 63.29
          real :: y = -63.59
          print *, floor(x) ! returns 63
          print *, floor(y) ! returns -64
      end program test_floor

  See also:
    :ref:`CEILING`,
    :ref:`NINT`
