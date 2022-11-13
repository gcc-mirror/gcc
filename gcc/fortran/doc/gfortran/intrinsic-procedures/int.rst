..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _int:

.. index:: INT

.. index:: IFIX

.. index:: IDINT

.. index:: conversion, to integer

INT --- Convert to integer type
*******************************

.. function:: INT(A , KIND))

  Convert to integer type

  :param A:
    Shall be of type ``INTEGER``,
    ``REAL``, or ``COMPLEX`` or a boz-literal-constant.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    These functions return a ``INTEGER`` variable or array under
    the following rules:

  Standard:
    Fortran 77 and later, with boz-literal-constant Fortran 2008 and later.

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = INT(A [, KIND))

  Example:
    .. code-block:: fortran

      program test_int
        integer :: i = 42
        complex :: z = (-3.7, 1.0)
        print *, int(i)
        print *, int(z), int(z,8)
      end program

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``INT(A)``
         - ``REAL(4) A``
         - ``INTEGER``
         - Fortran 77 and later
       * - ``IFIX(A)``
         - ``REAL(4) A``
         - ``INTEGER``
         - Fortran 77 and later
       * - ``IDINT(A)``
         - ``REAL(8) A``
         - ``INTEGER``
         - Fortran 77 and later