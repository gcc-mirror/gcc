..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ERFC_SCALED, error function, complementary, exponentially-scaled

.. _erfc_scaled:

ERFC_SCALED --- Error function
*******************************

.. function:: ERFC_SCALED(X)

  ``ERFC_SCALED(X)`` computes the exponentially-scaled complementary
  error function of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` and of the same kind as :samp:`{X}`.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ERFC_SCALED(X)

  Example:
    .. code-block:: fortran

      program test_erfc_scaled
        real(8) :: x = 0.17_8
        x = erfc_scaled(x)
      end program test_erfc_scaled