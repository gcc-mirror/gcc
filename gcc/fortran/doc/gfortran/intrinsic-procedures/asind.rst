..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _asind:

.. index:: ASIND

.. index:: DASIND

.. index:: trigonometric function, sine, inverse, degrees

.. index:: sine, inverse, degrees

ASIND --- Arcsine function, degrees
***********************************

.. function:: ASIND(X)

  ``ASIND(X)`` computes the arcsine of its :samp:`{X}` in degrees (inverse of
  ``SIND(X)``).

  :param X:
    The type shall be either ``REAL`` and a magnitude that is
    less than or equal to one - or be ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    The real part of the result is in degrees and lies in the range
    -90 \leq \Re \asin(x) \leq 90.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ASIND(X)

  Example:
    .. code-block:: fortran

      program test_asind
        real(8) :: x = 0.866_8
        x = asind(x)
      end program test_asind

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ASIND(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DASIND(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`SIND`
    Radians function:
    :ref:`ASIN`
