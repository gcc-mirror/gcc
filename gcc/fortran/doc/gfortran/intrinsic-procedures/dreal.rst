..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: DREAL, complex numbers, real part

.. _dreal:

DREAL --- Double real part function
***********************************

.. function:: DREAL(Z)

  ``DREAL(Z)`` returns the real part of complex variable :samp:`{Z}`.

  :param A:
    The type shall be ``COMPLEX(8)``.

  :return:
    The return value is of type ``REAL(8)``.

  Standard:
    GNU extension

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = DREAL(A)

  Example:
    .. code-block:: fortran

      program test_dreal
          complex(8) :: z = (1.3_8,7.2_8)
          print *, dreal(z)
      end program test_dreal

  See also:
    :ref:`AIMAG`
