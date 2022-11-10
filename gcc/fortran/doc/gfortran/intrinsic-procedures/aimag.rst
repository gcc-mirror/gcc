..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: AIMAG

.. index:: DIMAG

.. index:: IMAG

.. index:: IMAGPART

.. index:: complex numbers, imaginary part

.. _aimag:

AIMAG --- Imaginary part of complex number
********************************************

.. function:: AIMAG(Z)

  ``AIMAG(Z)`` yields the imaginary part of complex argument ``Z``.
  The ``IMAG(Z)`` and ``IMAGPART(Z)`` intrinsic functions are provided
  for compatibility with :command:`g77`, and their use in new code is
  strongly discouraged.

  :param Z:
    The type of the argument shall be ``COMPLEX``.

  :return:
    The return value is of type ``REAL`` with the
    kind type parameter of the argument.

  Standard:
    Fortran 77 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = AIMAG(Z)

  Example:
    .. code-block:: fortran

      program test_aimag
        complex(4) z4
        complex(8) z8
        z4 = cmplx(1.e0_4, 0.e0_4)
        z8 = cmplx(0.e0_8, 1.e0_8)
        print *, aimag(z4), dimag(z8)
      end program test_aimag

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``AIMAG(Z)``
         - ``COMPLEX Z``
         - ``REAL``
         - Fortran 77 and later
       * - ``DIMAG(Z)``
         - ``COMPLEX(8) Z``
         - ``REAL(8)``
         - GNU extension
       * - ``IMAG(Z)``
         - ``COMPLEX Z``
         - ``REAL``
         - GNU extension
       * - ``IMAGPART(Z)``
         - ``COMPLEX Z``
         - ``REAL``
         - GNU extension
