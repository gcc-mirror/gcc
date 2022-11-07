..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _erfc:

.. index:: ERFC

.. index:: error function, complementary

ERFC --- Error function
************************

.. function:: ERFC(X)

  ``ERFC(X)`` computes the complementary error function of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` and of the same kind as :samp:`{X}`.
    It lies in the range 0 \leq erfc (x) \leq 2 .

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ERFC(X)

  Example:
    .. code-block:: fortran

      program test_erfc
        real(8) :: x = 0.17_8
        x = erfc(x)
      end program test_erfc

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DERFC(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension