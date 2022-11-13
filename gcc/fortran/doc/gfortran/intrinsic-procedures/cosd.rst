..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cosd:

.. index:: COSD

.. index:: DCOSD

.. index:: CCOSD

.. index:: ZCOSD

.. index:: CDCOSD

.. index:: trigonometric function, cosine, degrees

.. index:: cosine, degrees

COSD --- Cosine function, degrees
*********************************

.. function:: COSD(X)

  ``COSD(X)`` computes the cosine of :samp:`{X}` in degrees.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`. The real part
    of the result is in degrees.  If :samp:`{X}` is of the type ``REAL``,
    the return value lies in the range -1 \leq \cosd (x) \leq 1.

  Standard:
    GNU extension, enabled with :option:`-fdec-math`.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = COSD(X)

  Example:
    .. code-block:: fortran

      program test_cosd
        real :: x = 0.0
        x = cosd(x)
      end program test_cosd

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``COSD(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - GNU extension
       * - ``DCOSD(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - GNU extension
       * - ``CCOSD(X)``
         - ``COMPLEX(4) X``
         - ``COMPLEX(4)``
         - GNU extension
       * - ``ZCOSD(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
       * - ``CDCOSD(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`ACOSD`
    Radians function:
    :ref:`COS`