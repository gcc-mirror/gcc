..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _anint:

.. index:: ANINT

.. index:: DNINT

.. index:: ceiling

.. index:: rounding, ceiling

ANINT --- Nearest whole number
******************************

.. function:: ANINT(A, KIND)

  ``ANINT(A [, KIND])`` rounds its argument to the nearest whole number.

  :param A:
    The type of the argument shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type real with the kind type parameter of the
    argument if the optional :samp:`{KIND}` is absent; otherwise, the kind
    type parameter will be given by :samp:`{KIND}`.  If :samp:`{A}` is greater than
    zero, ``ANINT(A)`` returns ``AINT(X+0.5)``.  If :samp:`{A}` is
    less than or equal to zero then it returns ``AINT(X-0.5)``.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ANINT(A [, KIND])

  Example:
    .. code-block:: fortran

      program test_anint
        real(4) x4
        real(8) x8
        x4 = 1.234E0_4
        x8 = 4.321_8
        print *, anint(x4), dnint(x8)
        x8 = anint(x4,8)
      end program test_anint

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ANINT(A)``
         - ``REAL(4) A``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DNINT(A)``
         - ``REAL(8) A``
         - ``REAL(8)``
         - Fortran 77 and later
