..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acos:

.. index:: ACOS

.. index:: DACOS

.. index:: trigonometric function, cosine, inverse

.. index:: cosine, inverse

ACOS --- Arccosine function
****************************

.. function:: ACOS(X)

  ``ACOS(X)`` computes the arccosine of :samp:`{X}` (inverse of ``COS(X)``).

  :param X:
    The type shall either be ``REAL`` with a magnitude that is
    less than or equal to one - or the type shall be ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The real part of the result is in radians and lies in the range
    0 \leq \Re \acos(x) \leq \pi.

  Standard:
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ACOS(X)

  Example:
    .. code-block:: fortran

      program test_acos
        real(8) :: x = 0.866_8
        x = acos(x)
      end program test_acos

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ACOS(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DACOS(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Inverse function:
    :ref:`COS`
    Degrees function:
    :ref:`ACOSD`
