..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETARG, command-line arguments, arguments, to program

.. _getarg:

GETARG --- Get command line arguments
*************************************

.. function:: GETARG(POS, VALUE)

  Retrieve the :samp:`{POS}` -th argument that was passed on the
  command line when the containing program was invoked.

  :param POS:
    Shall be of type ``INTEGER`` and not wider than
    the default integer kind; :samp:`{POS}` \geq 0

  :param VALUE:
    Shall be of type ``CHARACTER`` and of default
    kind.

  :return:
    After ``GETARG`` returns, the :samp:`{VALUE}` argument holds the
    :samp:`{POS}` th command line argument. If :samp:`{VALUE}` cannot hold the
    argument, it is truncated to fit the length of :samp:`{VALUE}`. If there are
    less than :samp:`{POS}` arguments specified at the command line, :samp:`{VALUE}`
    will be filled with blanks. If :samp:`{POS}` = 0, :samp:`{VALUE}` is set
    to the name of the program (on systems that support this feature).

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL GETARG(POS, VALUE)

  Example:
    .. code-block:: fortran

      PROGRAM test_getarg
        INTEGER :: i
        CHARACTER(len=32) :: arg

        DO i = 1, iargc()
          CALL getarg(i, arg)
          WRITE (*,*) arg
        END DO
      END PROGRAM

  See also:
    GNU Fortran 77 compatibility function:
    :ref:`IARGC`
    Fortran 2003 functions and subroutines:
    :ref:`GET_COMMAND`,
    :ref:`GET_COMMAND_ARGUMENT`,
    :ref:`COMMAND_ARGUMENT_COUNT`