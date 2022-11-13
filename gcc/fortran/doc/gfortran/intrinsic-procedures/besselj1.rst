..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _bessel_j1:

.. index:: BESSEL_J1

.. index:: BESJ1

.. index:: DBESJ1

.. index:: Bessel function, first kind

BESSEL_J1 --- Bessel function of the first kind of order 1
**********************************************************

.. function:: BESSEL_J1(X)

  ``BESSEL_J1(X)`` computes the Bessel function of the first kind of
  order 1 of :samp:`{X}`. This function is available under the name
  ``BESJ1`` as a GNU extension.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` and lies in the
    range - 0.5818... \leq Bessel (0,x) \leq 0.5818 . It has the same
    kind as :samp:`{X}`.

  Standard:
    Fortran 2008

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = BESSEL_J1(X)

  Example:
    .. code-block:: fortran

      program test_besj1
        real(8) :: x = 1.0_8
        x = bessel_j1(x)
      end program test_besj1

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DBESJ1(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension