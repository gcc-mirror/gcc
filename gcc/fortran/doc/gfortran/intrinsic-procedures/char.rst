..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _char:

.. index:: CHAR

.. index:: conversion, to character

CHAR --- Character conversion function
**************************************

.. function:: CHAR(I, KIND)

  ``CHAR(I [, KIND])`` returns the character represented by the integer :samp:`{I}`.

  :param I:
    The type shall be ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``CHARACTER(1)``

  Standard:
    Fortran 77 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = CHAR(I [, KIND])

  Example:
    .. code-block:: fortran

      program test_char
          integer :: i = 74
          character(1) :: c
          c = char(i)
          print *, i, c ! returns 'J'
      end program test_char

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``CHAR(I)``
         - ``INTEGER I``
         - ``CHARACTER(LEN=1)``
         - Fortran 77 and later

  Note:
    See :ref:`ICHAR` for a discussion of converting between numerical values
    and formatted string representations.

  See also:
    :ref:`ACHAR`,
    :ref:`IACHAR`,
    :ref:`ICHAR`
