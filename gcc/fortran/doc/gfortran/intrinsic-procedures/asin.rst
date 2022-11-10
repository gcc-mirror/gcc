..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ASIN

.. index:: DASIN

.. index:: trigonometric function, sine, inverse

.. index:: sine, inverse

.. _asin:

ASIN --- Arcsine function
**************************

.. function:: ASIN(X)

  ``ASIN(X)`` computes the arcsine of its :samp:`{X}` (inverse of ``SIN(X)``).

  :param X:
    The type shall be either ``REAL`` and a magnitude that is
    less than or equal to one - or be ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The real part of the result is in radians and lies in the range
    -\pi/2 \leq \Re \asin(x) \leq \pi/2.

  Standard:
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ASIN(X)

  Example:
    .. code-block:: fortran

      program test_asin
        real(8) :: x = 0.866_8
        x = asin(x)
      end program test_asin

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ASIN(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DASIN(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Inverse function:
    :ref:`SIN`
    Degrees function:
    :ref:`ASIND`
