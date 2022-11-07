..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _atand:

.. index:: ATAND

.. index:: DATAND

.. index:: trigonometric function, tangent, inverse, degrees

.. index:: tangent, inverse, degrees

ATAND --- Arctangent function, degrees
**************************************

.. function:: ATAND(X)

  ``ATAND(X)`` computes the arctangent of :samp:`{X}` in degrees (inverse of
  :ref:`TAND`).

  :param X:
    The type shall be ``REAL`` or ``COMPLEX`` ;
    if :samp:`{Y}` is present, :samp:`{X}` shall be REAL.

  :param Y:
    The type and kind type parameter shall be the same as :samp:`{X}`.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    If :samp:`{Y}` is present, the result is identical to ``ATAND2(Y,X)``.
    Otherwise, it is the arcus tangent of :samp:`{X}`, where the real part of
    the result is in degrees and lies in the range
    -90 \leq \Re \atand(x) \leq 90.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ATAND(X)
      RESULT = ATAND(Y, X)

  Example:
    .. code-block:: fortran

      program test_atand
        real(8) :: x = 2.866_8
        x = atand(x)
      end program test_atand

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ATAND(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DATAND(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`TAND`
    Radians function:
    :ref:`ATAN`