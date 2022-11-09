..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _iargc:

IARGC --- Get the number of command line arguments
**************************************************

.. index:: IARGC, command-line arguments, command-line arguments, number of, arguments, to program

.. function:: IARGC()

  ``IARGC`` returns the number of arguments passed on the
  command line when the containing program was invoked.

  :return:
    The number of command line arguments, type ``INTEGER(4)``.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = IARGC()

  Arguments:
    None

  Example:
    See :ref:`GETARG`

  See also:
    GNU Fortran 77 compatibility subroutine:
    :ref:`GETARG`
    Fortran 2003 functions and subroutines:
    :ref:`GET_COMMAND`,
    :ref:`GET_COMMAND_ARGUMENT`,
    :ref:`COMMAND_ARGUMENT_COUNT`
