..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: CEILING, ceiling, rounding, ceiling

.. _ceiling:

CEILING --- Integer ceiling function
************************************

.. function:: CEILING(A)

  ``CEILING(A)`` returns the least integer greater than or equal to :samp:`{A}`.

  :param A:
    The type shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
    and a default-kind ``INTEGER`` otherwise.

  Standard:
    Fortran 95 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = CEILING(A [, KIND])

  Example:
    .. code-block:: fortran

      program test_ceiling
          real :: x = 63.29
          real :: y = -63.59
          print *, ceiling(x) ! returns 64
          print *, ceiling(y) ! returns -63
      end program test_ceiling

  See also:
    :ref:`FLOOR`,
    :ref:`NINT`
