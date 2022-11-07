..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _command_argument_count:

COMMAND_ARGUMENT_COUNT --- Get number of command line arguments
***************************************************************

.. index:: COMMAND_ARGUMENT_COUNT, command-line arguments, command-line arguments, number of, arguments, to program

.. function:: COMMAND_ARGUMENT_COUNT()

  ``COMMAND_ARGUMENT_COUNT`` returns the number of arguments passed on the
  command line when the containing program was invoked.

  :return:
    The return value is an ``INTEGER`` of default kind.

  Standard:
    Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = COMMAND_ARGUMENT_COUNT()

  Example:
    .. code-block:: fortran

      program test_command_argument_count
          integer :: count
          count = command_argument_count()
          print *, count
      end program test_command_argument_count

  See also:
    :ref:`GET_COMMAND`,
    :ref:`GET_COMMAND_ARGUMENT`