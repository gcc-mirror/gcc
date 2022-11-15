..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _nint:

.. index:: NINT

.. index:: IDNINT

.. index:: rounding, nearest whole number

NINT --- Nearest whole number
*****************************

.. function:: NINT(A)

  ``NINT(A)`` rounds its argument to the nearest whole number.

  :param A:
    The type of the argument shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    Returns :samp:`{A}` with the fractional portion of its magnitude eliminated by
    rounding to the nearest whole number and with its sign preserved,
    converted to an ``INTEGER`` of the default kind.

  Standard:
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 90 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = NINT(A [, KIND])

  Example:
    .. code-block:: fortran

      program test_nint
        real(4) x4
        real(8) x8
        x4 = 1.234E0_4
        x8 = 4.321_8
        print *, nint(x4), idnint(x8)
      end program test_nint

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return Type
         - Standard

       * - ``NINT(A)``
         - ``REAL(4) A``
         - ``INTEGER``
         - Fortran 77 and later
       * - ``IDNINT(A)``
         - ``REAL(8) A``
         - ``INTEGER``
         - Fortran 77 and later

  See also:
    :ref:`CEILING`,
    :ref:`FLOOR`
