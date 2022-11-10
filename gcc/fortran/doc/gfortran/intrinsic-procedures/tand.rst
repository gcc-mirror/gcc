..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: TAND

.. index:: DTAND

.. index:: trigonometric function, tangent, degrees

.. index:: tangent, degrees

.. _tand:

TAND --- Tangent function, degrees
**********************************

.. function:: TAND(X)

  ``TAND(X)`` computes the tangent of :samp:`{X}` in degrees.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in degrees.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = TAND(X)

  Example:
    .. code-block:: fortran

      program test_tand
        real(8) :: x = 0.165_8
        x = tand(x)
      end program test_tand

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``TAND(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DTAND(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`ATAND`
    Radians function:
    :ref:`TAN`
