..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SYSTEM, system, system call

.. _system:

SYSTEM --- Execute a shell command
**********************************

.. function:: SYSTEM(COMMAND)

  Passes the command :samp:`{COMMAND}` to a shell (see ``system(3)``). If
  argument :samp:`{STATUS}` is present, it contains the value returned by
  ``system(3)``, which is presumably 0 if the shell command succeeded.
  Note that which shell is used to invoke the command is system-dependent
  and environment-dependent.

  :param COMMAND:
    Shall be of default ``CHARACTER`` type.

  :param STATUS:
    (Optional) Shall be of default ``INTEGER`` type.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL SYSTEM(COMMAND [, STATUS])
      STATUS = SYSTEM(COMMAND)

  See also:
    :ref:`EXECUTE_COMMAND_LINE`, which is part of the Fortran 2008 standard
    and should considered in new code for future portability.
