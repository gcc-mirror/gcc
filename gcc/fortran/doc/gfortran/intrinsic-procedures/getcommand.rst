..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GET_COMMAND, command-line arguments, arguments, to program

.. _get_command:

GET_COMMAND --- Get the entire command line
*******************************************

.. function:: GET_COMMAND(COMMAND, LENGTH, STATUS)

  Retrieve the entire command line that was used to invoke the program.

  :param COMMAND:
    (Optional) shall be of type ``CHARACTER`` and
    of default kind.

  :param LENGTH:
    (Optional) Shall be of type ``INTEGER`` and of
    default kind.

  :param STATUS:
    (Optional) Shall be of type ``INTEGER`` and of
    default kind.

  :return:
    If :samp:`{COMMAND}` is present, stores the entire command line that was used
    to invoke the program in :samp:`{COMMAND}`. If :samp:`{LENGTH}` is present, it is
    assigned the length of the command line. If :samp:`{STATUS}` is present, it
    is assigned 0 upon success of the command, -1 if :samp:`{COMMAND}` is too
    short to store the command line, or a positive value in case of an error.

  Standard:
    Fortran 2003 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GET_COMMAND([COMMAND, LENGTH, STATUS])

  Example:
    .. code-block:: fortran

      PROGRAM test_get_command
        CHARACTER(len=255) :: cmd
        CALL get_command(cmd)
        WRITE (*,*) TRIM(cmd)
      END PROGRAM

  See also:
    :ref:`GET_COMMAND_ARGUMENT`,
    :ref:`COMMAND_ARGUMENT_COUNT`