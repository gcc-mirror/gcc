..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IACHAR, ASCII collating sequence, collating sequence, ASCII, conversion, to integer

.. _iachar:

IACHAR --- Code in ASCII collating sequence
********************************************

.. function:: IACHAR(C)

  ``IACHAR(C)`` returns the code for the ASCII character
  in the first character position of ``C``.

  :param C:
    Shall be a scalar ``CHARACTER``, with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  Standard:
    Fortran 95 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = IACHAR(C [, KIND])

  Example:
    .. code-block:: fortran

      program test_iachar
        integer i
        i = iachar(' ')
      end program test_iachar

  Note:
    See :ref:`ICHAR` for a discussion of converting between numerical values
    and formatted string representations.

  See also:
    :ref:`ACHAR`,
    :ref:`CHAR`,
    :ref:`ICHAR`
