..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _bessel_j0:

.. index:: BESSEL_J0

.. index:: BESJ0

.. index:: DBESJ0

.. index:: Bessel function, first kind

BESSEL_J0 --- Bessel function of the first kind of order 0
**********************************************************

.. function:: BESSEL_J0(X)

  ``BESSEL_J0(X)`` computes the Bessel function of the first kind of
  order 0 of :samp:`{X}`. This function is available under the name
  ``BESJ0`` as a GNU extension.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` and lies in the
    range - 0.4027... \leq Bessel (0,x) \leq 1. It has the same
    kind as :samp:`{X}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = BESSEL_J0(X)

  Example:
    .. code-block:: fortran

      program test_besj0
        real(8) :: x = 0.0_8
        x = bessel_j0(x)
      end program test_besj0

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DBESJ0(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension