..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _cos:

.. index:: COS

.. index:: DCOS

.. index:: CCOS

.. index:: ZCOS

.. index:: CDCOS

.. index:: trigonometric function, cosine

.. index:: cosine

COS --- Cosine function
***********************

.. function:: COS(X)

  ``COS(X)`` computes the cosine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`. The real part
    of the result is in radians. If :samp:`{X}` is of the type ``REAL``,
    the return value lies in the range -1 \leq \cos (x) \leq 1.

  Standard:
    Fortran 77 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = COS(X)

  Example:
    .. code-block:: fortran

      program test_cos
        real :: x = 0.0
        x = cos(x)
      end program test_cos

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``COS(X)``
         - ``REAL(4) X``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DCOS(X)``
         - ``REAL(8) X``
         - ``REAL(8)``
         - Fortran 77 and later
       * - ``CCOS(X)``
         - ``COMPLEX(4) X``
         - ``COMPLEX(4)``
         - Fortran 77 and later
       * - ``ZCOS(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension
       * - ``CDCOS(X)``
         - ``COMPLEX(8) X``
         - ``COMPLEX(8)``
         - GNU extension

  See also:
    Inverse function:
    :ref:`ACOS`
    Degrees function:
    :ref:`COSD`