..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETGID, system, group ID

.. _getgid:

GETGID --- Group ID function
****************************

.. function:: GETGID()

  Returns the numerical group ID of the current process.

  :return:
    The return value of ``GETGID`` is an ``INTEGER`` of the default
    kind.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = GETGID()

  Example:
    See ``GETPID`` for an example.

  See also:
    :ref:`GETPID`,
    :ref:`GETUID`