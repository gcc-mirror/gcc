..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cotan:

.. index:: COTAN

.. index:: DCOTAN

.. index:: trigonometric function, cotangent

.. index:: cotangent

COTAN --- Cotangent function
****************************

.. function:: COTAN(X)

  ``COTAN(X)`` computes the cotangent of :samp:`{X}`. Equivalent to ``COS(x)``
  divided by ``SIN(x)``, or ``1 / TAN(x)``.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in radians.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = COTAN(X)

  Example:
    .. code-block:: fortran

      program test_cotan
        real(8) :: x = 0.165_8
        x = cotan(x)
      end program test_cotan

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``COTAN(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DCOTAN(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Converse function:
    :ref:`TAN`
    Degrees function:
    :ref:`COTAND`
