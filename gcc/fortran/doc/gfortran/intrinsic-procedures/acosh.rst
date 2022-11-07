..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acosh:

.. index:: ACOSH

.. index:: DACOSH

.. index:: area hyperbolic cosine

.. index:: inverse hyperbolic cosine

.. index:: hyperbolic function, cosine, inverse

.. index:: cosine, hyperbolic, inverse

ACOSH --- Inverse hyperbolic cosine function
********************************************

.. function:: ACOSH(X)

  ``ACOSH(X)`` computes the inverse hyperbolic cosine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has the same type and kind as :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians and lies between
    0 \leq \Im \acosh(x) \leq \pi.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ACOSH(X)

  Example:
    .. code-block:: fortran

      PROGRAM test_acosh
        REAL(8), DIMENSION(3) :: x = (/ 1.0, 2.0, 3.0 /)
        WRITE (*,*) ACOSH(x)
      END PROGRAM

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DACOSH(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`COSH`