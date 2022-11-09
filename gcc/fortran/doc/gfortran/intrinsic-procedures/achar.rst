..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: ACHAR, ASCII collating sequence, collating sequence, ASCII

.. _achar:

ACHAR --- Character in ASCII collating sequence
************************************************

.. function:: ACHAR(I)

  ``ACHAR(I)`` returns the character located at position ``I``
  in the ASCII collating sequence.

  :param I:
    The type shall be ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``CHARACTER`` with a length of one.
    If the :samp:`{KIND}` argument is present, the return value is of the
    specified kind and of the default kind otherwise.

  Standard:
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = ACHAR(I [, KIND])

  Example:
    .. code-block:: fortran

      program test_achar
        character c
        c = achar(32)
      end program test_achar

  Note:
    See :ref:`ICHAR` for a discussion of converting between numerical values
    and formatted string representations.

  See also:
    :ref:`CHAR`,
    :ref:`IACHAR`,
    :ref:`ICHAR`
