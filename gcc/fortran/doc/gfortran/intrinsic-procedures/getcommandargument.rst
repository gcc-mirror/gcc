..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GET_COMMAND_ARGUMENT, command-line arguments, arguments, to program

.. _get_command_argument:

GET_COMMAND_ARGUMENT --- Get command line arguments
***************************************************

.. function:: GET_COMMAND_ARGUMENT(NUMBER , VALUE, LENGTH, STATUS)

  Retrieve the :samp:`{NUMBER}` -th argument that was passed on the
  command line when the containing program was invoked.

  :param NUMBER:
    Shall be a scalar of type ``INTEGER`` and of
    default kind, :samp:`{NUMBER}` \geq 0

  :param VALUE:
    (Optional) Shall be a scalar of type ``CHARACTER``
    and of default kind.

  :param LENGTH:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :param STATUS:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :return:
    After ``GET_COMMAND_ARGUMENT`` returns, the :samp:`{VALUE}` argument holds the
    :samp:`{NUMBER}` -th command line argument. If :samp:`{VALUE}` cannot hold the argument, it is
    truncated to fit the length of :samp:`{VALUE}`. If there are less than :samp:`{NUMBER}`
    arguments specified at the command line, :samp:`{VALUE}` will be filled with blanks.
    If :samp:`{NUMBER}` = 0, :samp:`{VALUE}` is set to the name of the program (on
    systems that support this feature). The :samp:`{LENGTH}` argument contains the
    length of the :samp:`{NUMBER}` -th command line argument. If the argument retrieval
    fails, :samp:`{STATUS}` is a positive number; if :samp:`{VALUE}` contains a truncated
    command line argument, :samp:`{STATUS}` is -1; and otherwise the :samp:`{STATUS}` is
    zero.

  Standard:
    Fortran 2003 and later

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GET_COMMAND_ARGUMENT(NUMBER [, VALUE, LENGTH, STATUS])

  Example:
    .. code-block:: fortran

      PROGRAM test_get_command_argument
        INTEGER :: i
        CHARACTER(len=32) :: arg

        i = 0
        DO
          CALL get_command_argument(i, arg)
          IF (LEN_TRIM(arg) == 0) EXIT

          WRITE (*,*) TRIM(arg)
          i = i+1
        END DO
      END PROGRAM

  See also:
    :ref:`GET_COMMAND`,
    :ref:`COMMAND_ARGUMENT_COUNT`