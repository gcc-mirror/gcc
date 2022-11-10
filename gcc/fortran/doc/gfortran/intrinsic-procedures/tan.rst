..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TAN

.. index:: DTAN

.. index:: trigonometric function, tangent

.. index:: tangent

.. _tan:

TAN --- Tangent function
************************

.. function:: TAN(X)

  ``TAN(X)`` computes the tangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in radians.

  Standard:
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = TAN(X)

  Example:
    .. code-block:: fortran

      program test_tan
        real(8) :: x = 0.165_8
        x = tan(x)
      end program test_tan

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``TAN(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DTAN(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Inverse function:
    :ref:`ATAN`
    Degrees function:
    :ref:`TAND`
