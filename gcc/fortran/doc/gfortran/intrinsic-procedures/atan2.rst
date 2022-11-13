..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _atan2:

.. index:: ATAN2

.. index:: DATAN2

.. index:: trigonometric function, tangent, inverse

.. index:: tangent, inverse

ATAN2 --- Arctangent function
******************************

.. function:: ATAN2(Y, X)

  ``ATAN2(Y, X)`` computes the principal value of the argument
  function of the complex number X + i Y.  This function can
  be used to transform from Cartesian into polar coordinates and
  allows to determine the angle in the correct quadrant.

  :param Y:
    The type shall be ``REAL``.

  :param X:
    The type and kind type parameter shall be the same as :samp:`{Y}`.
    If :samp:`{Y}` is zero, then :samp:`{X}` must be nonzero.

  :return:
    The return value has the same type and kind type parameter as :samp:`{Y}`. It
    is the principal value of the complex number X + i Y.  If :samp:`{X}`
    is nonzero, then it lies in the range -\pi \le \atan (x) \leq \pi.
    The sign is positive if :samp:`{Y}` is positive.  If :samp:`{Y}` is zero, then
    the return value is zero if :samp:`{X}` is strictly positive, \pi if
    :samp:`{X}` is negative and :samp:`{Y}` is positive zero (or the processor does
    not handle signed zeros), and -\pi if :samp:`{X}` is negative and
    :samp:`{Y}` is negative zero.  Finally, if :samp:`{X}` is zero, then the
    magnitude of the result is \pi/2.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ATAN2(Y, X)

  Example:
    .. code-block:: fortran

      program test_atan2
        real(4) :: x = 1.e0_4, y = 0.5e0_4
        x = atan2(y,x)
      end program test_atan2

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ATAN2(X, Y)``
         - ``REAL(4) X, Y``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DATAN2(X, Y)``
         - ``REAL(8) X, Y``
         - ``REAL(8)``
         - Fortran 77 and later

  See also:
    Alias:
    :ref:`ATAN`
    Degrees function:
    :ref:`ATAN2D`
