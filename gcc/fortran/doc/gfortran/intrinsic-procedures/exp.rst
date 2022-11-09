..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exp:

.. index:: EXP

.. index:: DEXP

.. index:: CEXP

.. index:: ZEXP

.. index:: CDEXP

.. index:: exponential function

.. index:: logarithm function, inverse

EXP --- Exponential function
*****************************

.. function:: EXP(X)

  ``EXP(X)`` computes the base e exponential of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`.

  Standard:
    Fortran 77 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = EXP(X)

  Example:
    .. code-block:: fortran

      program test_exp
        real :: x = 1.0
        x = exp(x)
      end program test_exp

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``EXP(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DEXP(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later
       * - ``CEXP(X)``
         - ``COMPLEX(4) X``
         - ``COMPLEX(4)``
         - Fortran 77 and later
       * - ``ZEXP(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
       * - ``CDEXP(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
