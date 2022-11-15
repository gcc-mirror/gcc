..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _asinh:

.. index:: ASINH

.. index:: DASINH

.. index:: area hyperbolic sine

.. index:: inverse hyperbolic sine

.. index:: hyperbolic function, sine, inverse

.. index:: sine, hyperbolic, inverse

ASINH --- Inverse hyperbolic sine function
******************************************

.. function:: ASINH(X)

  ``ASINH(X)`` computes the inverse hyperbolic sine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value is of the same type and kind as  :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians and lies between
    -\pi/2 \leq \Im \asinh(x) \leq \pi/2.

  Standard:
    Fortran 2008 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ASINH(X)

  Example:
    .. code-block:: fortran

      PROGRAM test_asinh
        REAL(8), DIMENSION(3) :: x = (/ -1.0, 0.0, 1.0 /)
        WRITE (*,*) ASINH(x)
      END PROGRAM

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``DASINH(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension.

  See also:
    Inverse function:
    :ref:`SINH`
