..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ATAN

.. index:: DATAN

.. index:: trigonometric function, tangent, inverse

.. index:: tangent, inverse

.. _atan:

ATAN --- Arctangent function
*****************************

.. function:: ATAN(X)

  ``ATAN(X)`` computes the arctangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX`` ;
    if :samp:`{Y}` is present, :samp:`{X}` shall be REAL.

  :param Y:
    The type and kind type parameter shall be the same as :samp:`{X}`.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    If :samp:`{Y}` is present, the result is identical to ``ATAN2(Y,X)``.
    Otherwise, it the arcus tangent of :samp:`{X}`, where the real part of
    the result is in radians and lies in the range
    -\pi/2 \leq \Re \atan(x) \leq \pi/2.

  Standard:
    Fortran 77 and later, for a complex argument and for two arguments
    Fortran 2008 or later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ATAN(X)
      RESULT = ATAN(Y, X)

  Example:
    .. code-block:: fortran

      program test_atan
        real(8) :: x = 2.866_8
        x = atan(x)
      end program test_atan

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ATAN(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DATAN(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Inverse function:
    :ref:`TAN`
    Degrees function:
    :ref:`ATAND`
