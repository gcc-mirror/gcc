..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _sign:

.. index:: SIGN

.. index:: ISIGN

.. index:: DSIGN

.. index:: sign copying

SIGN --- Sign copying function
******************************

.. function:: SIGN(A,B)

  ``SIGN(A,B)`` returns the value of :samp:`{A}` with the sign of :samp:`{B}`.

  :param A:
    Shall be of type ``INTEGER`` or ``REAL``

  :param B:
    Shall be of the same type and kind as :samp:`{A}`.

  :return:
    The kind of the return value is that of :samp:`{A}` and :samp:`{B}`.
    If B \ge 0 then the result is ``ABS(A)``, else
    it is ``-ABS(A)``.

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = SIGN(A, B)

  Example:
    .. code-block:: fortran

      program test_sign
        print *, sign(-12,1)
        print *, sign(-12,0)
        print *, sign(-12,-1)

        print *, sign(-12.,1.)
        print *, sign(-12.,0.)
        print *, sign(-12.,-1.)
      end program test_sign

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Arguments
         - Return type
         - Standard

       * - ``SIGN(A,B)``
         - ``REAL(4) A, B``
         - ``REAL(4)``
         - Fortran 77 and later
       * - ``ISIGN(A,B)``
         - ``INTEGER(4) A, B``
         - ``INTEGER(4)``
         - Fortran 77 and later
       * - ``DSIGN(A,B)``
         - ``REAL(8) A, B``
         - ``REAL(8)``
         - Fortran 77 and later
