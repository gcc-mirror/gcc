..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MOD

.. index:: AMOD

.. index:: DMOD

.. index:: BMOD

.. index:: IMOD

.. index:: JMOD

.. index:: KMOD

.. index:: remainder

.. index:: division, remainder

.. _mod:

MOD --- Remainder function
**************************

.. function:: MOD(A,P)

  ``MOD(A,P)`` computes the remainder of the division of A by P.

  :param A:
    Shall be a scalar of type ``INTEGER`` or ``REAL``.

  :param P:
    Shall be a scalar of the same type and kind as :samp:`{A}`
    and not equal to zero.  (As a GNU extension, arguments of different kinds are
    permitted.)

  :return:
    The return value is the result of ``A - (INT(A/P) * P)``. The type
    and kind of the return value is the same as that of the arguments. The
    returned value has the same sign as A and a magnitude less than the
    magnitude of P.  (As a GNU extension, kind is the largest kind of the actual
    arguments.)

  Standard:
    Fortran 77 and later, has overloads that are GNU extensions

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = MOD(A, P)

  Example:
    .. code-block:: fortran

      program test_mod
        print *, mod(17,3)
        print *, mod(17.5,5.5)
        print *, mod(17.5d0,5.5)
        print *, mod(17.5,5.5d0)

        print *, mod(-17,3)
        print *, mod(-17.5,5.5)
        print *, mod(-17.5d0,5.5)
        print *, mod(-17.5,5.5d0)

        print *, mod(17,-3)
        print *, mod(17.5,-5.5)
        print *, mod(17.5d0,-5.5)
        print *, mod(17.5,-5.5d0)
      end program test_mod

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Arguments
         - Return type
         - Standard

       * - ``MOD(A,P)``
         - ``INTEGER A,P``
         - ``INTEGER``
         - Fortran 77 and later
       * - ``AMOD(A,P)``
         - ``REAL(4) A,P``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DMOD(A,P)``
         - ``REAL(8) A,P``
         - ``REAL(8)``
         - Fortran 77 and later
       * - ``BMOD(A,P)``
         - ``INTEGER(1) A,P``
         - ``INTEGER(1)``
         - GNU extension
       * - ``IMOD(A,P)``
         - ``INTEGER(2) A,P``
         - ``INTEGER(2)``
         - GNU extension
       * - ``JMOD(A,P)``
         - ``INTEGER(4) A,P``
         - ``INTEGER(4)``
         - GNU extension
       * - ``KMOD(A,P)``
         - ``INTEGER(8) A,P``
         - ``INTEGER(8)``
         - GNU extension

  See also:
    :ref:`MODULO`
