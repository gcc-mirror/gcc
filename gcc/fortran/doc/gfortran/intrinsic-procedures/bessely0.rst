..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: BESSEL_Y0

.. index:: BESY0

.. index:: DBESY0

.. index:: Bessel function, second kind

.. _bessel_y0:

BESSEL_Y0 --- Bessel function of the second kind of order 0
***********************************************************

.. function:: BESSEL_Y0(X)

  ``BESSEL_Y0(X)`` computes the Bessel function of the second kind of
  order 0 of :samp:`{X}`. This function is available under the name
  ``BESY0`` as a GNU extension.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL``. It has the same kind as :samp:`{X}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = BESSEL_Y0(X)

  Example:
    .. code-block:: fortran

      program test_besy0
        real(8) :: x = 0.0_8
        x = bessel_y0(x)
      end program test_besy0

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DBESY0(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension
