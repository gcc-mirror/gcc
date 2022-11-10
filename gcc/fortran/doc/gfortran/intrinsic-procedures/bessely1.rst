..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: BESSEL_Y1

.. index:: BESY1

.. index:: DBESY1

.. index:: Bessel function, second kind

.. _bessel_y1:

BESSEL_Y1 --- Bessel function of the second kind of order 1
***********************************************************

.. function:: BESSEL_Y1(X)

  ``BESSEL_Y1(X)`` computes the Bessel function of the second kind of
  order 1 of :samp:`{X}`. This function is available under the name
  ``BESY1`` as a GNU extension.

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

      RESULT = BESSEL_Y1(X)

  Example:
    .. code-block:: fortran

      program test_besy1
        real(8) :: x = 1.0_8
        x = bessel_y1(x)
      end program test_besy1

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DBESY1(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension
