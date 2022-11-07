..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: LEN_TRIM, string, length, without trailing whitespace

.. _len_trim:

LEN_TRIM --- Length of a character entity without trailing blank characters
***************************************************************************

.. function:: LEN_TRIM(STRING , KIND)

  Returns the length of a character string, ignoring any trailing blanks.

  :param STRING:
    Shall be a scalar of type ``CHARACTER``,
    with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  Standard:
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = LEN_TRIM(STRING [, KIND])

  See also:
    :ref:`LEN`,
    :ref:`ADJUSTL`,
    :ref:`ADJUSTR`