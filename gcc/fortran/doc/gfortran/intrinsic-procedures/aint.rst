..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: AINT

.. index:: DINT

.. index:: floor

.. index:: rounding, floor

.. _aint:

AINT --- Truncate to a whole number
***********************************

.. function:: AINT(A, KIND)

  ``AINT(A [, KIND])`` truncates its argument to a whole number.

  :param A:
    The type of the argument shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``REAL`` with the kind type parameter of the
    argument if the optional :samp:`{KIND}` is absent; otherwise, the kind
    type parameter will be given by :samp:`{KIND}`.  If the magnitude of
    :samp:`{X}` is less than one, ``AINT(X)`` returns zero.  If the
    magnitude is equal to or greater than one then it returns the largest
    whole number that does not exceed its magnitude.  The sign is the same
    as the sign of :samp:`{X}`.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = AINT(A [, KIND])

  Example:
    .. code-block:: fortran

      program test_aint
        real(4) x4
        real(8) x8
        x4 = 1.234E0_4
        x8 = 4.321_8
        print *, aint(x4), dint(x8)
        x8 = aint(x4,8)
      end program test_aint

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``AINT(A)``
         - ``REAL(4) A``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``DINT(A)``
         - ``REAL(8) A``
         - ``REAL(8)``
         - Fortran 77 and later
