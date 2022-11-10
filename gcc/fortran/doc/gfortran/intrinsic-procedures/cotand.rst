..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: COTAND

.. index:: DCOTAND

.. index:: trigonometric function, cotangent, degrees

.. index:: cotangent, degrees

.. _cotand:

COTAND --- Cotangent function, degrees
**************************************

.. function:: COTAND(X)

  ``COTAND(X)`` computes the cotangent of :samp:`{X}` in degrees.  Equivalent to
  ``COSD(x)`` divided by ``SIND(x)``, or ``1 / TAND(x)``.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in degrees.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

    This function is for compatibility only and should be avoided in favor of
    standard constructs wherever possible.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = COTAND(X)

  Example:
    .. code-block:: fortran

      program test_cotand
        real(8) :: x = 0.165_8
        x = cotand(x)
      end program test_cotand

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``COTAND(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DCOTAND(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension

  See also:
    Converse function:
    :ref:`TAND`
    Radians function:
    :ref:`COTAN`
