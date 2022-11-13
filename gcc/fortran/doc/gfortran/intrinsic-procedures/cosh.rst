..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cosh:

.. index:: COSH

.. index:: DCOSH

.. index:: hyperbolic cosine

.. index:: hyperbolic function, cosine

.. index:: cosine, hyperbolic

COSH --- Hyperbolic cosine function
***********************************

.. function:: COSH(X)

  ``COSH(X)`` computes the hyperbolic cosine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians. If :samp:`{X}`
    is ``REAL``, the return value has a lower bound of one,
    \cosh (x) \geq 1.

  Standard:
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      X = COSH(X)

  Example:
    .. code-block:: fortran

      program test_cosh
        real(8) :: x = 1.0_8
        x = cosh(x)
      end program test_cosh

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``COSH(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DCOSH(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Inverse function:
    :ref:`ACOSH`