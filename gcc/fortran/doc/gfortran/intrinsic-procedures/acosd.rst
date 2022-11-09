..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acosd:

.. index:: ACOSD

.. index:: DACOSD

.. index:: trigonometric function, cosine, inverse, degrees

.. index:: cosine, inverse, degrees

ACOSD --- Arccosine function, degrees
*************************************

.. function:: ACOSD(X)

  ``ACOSD(X)`` computes the arccosine of :samp:`{X}` in degrees (inverse of
  ``COSD(X)``).

  :param X:
    The type shall either be ``REAL`` with a magnitude that is
    less than or equal to one - or the type shall be ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The real part of the result is in degrees and lies in the range
    0 \leq \Re \acos(x) \leq 180.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ACOSD(X)

  Example:
    .. code-block:: fortran

      program test_acosd
        real(8) :: x = 0.866_8
        x = acosd(x)
      end program test_acosd

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ACOSD(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DACOSD(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`COSD`
    Radians function:
    :ref:`ACOS`
