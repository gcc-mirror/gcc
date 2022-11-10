..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ICHAR

.. index:: conversion, to integer

.. _ichar:

ICHAR --- Character-to-integer conversion function
**************************************************

.. function:: ICHAR(C)

  ``ICHAR(C)`` returns the code for the character in the first character
  position of ``C`` in the system's native character set.
  The correspondence between characters and their codes is not necessarily
  the same across different GNU Fortran implementations.

  :param C:
    Shall be a scalar ``CHARACTER``, with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  Standard:
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ICHAR(C [, KIND])

  Example:
    .. code-block:: fortran

      program test_ichar
        integer i
        i = ichar(' ')
      end program test_ichar

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``ICHAR(C)``
         - ``CHARACTER C``
         - ``INTEGER(4)``
         - Fortran 77 and later

  Note:
    No intrinsic exists to convert between a numeric value and a formatted
    character string representation -- for instance, given the
    ``CHARACTER`` value ``'154'``, obtaining an ``INTEGER`` or
    ``REAL`` value with the value 154, or vice versa. Instead, this
    functionality is provided by internal-file I/O, as in the following
    example:

    .. code-block:: fortran

      program read_val
        integer value
        character(len=10) string, string2
        string = '154'

        ! Convert a string to a numeric value
        read (string,'(I10)') value
        print *, value

        ! Convert a value to a formatted string
        write (string2,'(I10)') value
        print *, string2
      end program read_val

  See also:
    :ref:`ACHAR`,
    :ref:`CHAR`,
    :ref:`IACHAR`
