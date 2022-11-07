..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETUID, system, user ID, user id

.. _getuid:

GETUID --- User ID function
***************************

.. function:: GETUID()

  Returns the numerical user ID of the current process.

  :return:
    The return value of ``GETUID`` is an ``INTEGER`` of the default
    kind.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = GETUID()

  Example:
    See ``GETPID`` for an example.

  See also:
    :ref:`GETPID`,
    :ref:`GETLOG`