..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _sin:

.. index:: SIN

.. index:: DSIN

.. index:: CSIN

.. index:: ZSIN

.. index:: CDSIN

.. index:: trigonometric function, sine

.. index:: sine

SIN --- Sine function
**********************

.. function:: SIN(X)

  ``SIN(X)`` computes the sine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SIN(X)

  Example:
    .. code-block:: fortran

      program test_sin
        real :: x = 0.0
        x = sin(x)
      end program test_sin

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``SIN(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DSIN(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later
       * - ``CSIN(X)``
         - ``COMPLEX(4) X``
         - ``COMPLEX(4)``
         - Fortran 77 and later
       * - ``ZSIN(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
       * - ``CDSIN(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`ASIN`
    Degrees function:
    :ref:`SIND`