..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: PERROR, system, error handling

.. _perror:

PERROR --- Print system error message
*************************************

.. function:: PERROR(STRING)

  Prints (on the C ``stderr`` stream) a newline-terminated error
  message corresponding to the last system error. This is prefixed by
  :samp:`{STRING}`, a colon and a space. See ``perror(3)``.

  :param STRING:
    A scalar of type ``CHARACTER`` and of the
    default kind.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL PERROR(STRING)

  See also:
    :ref:`IERRNO`
