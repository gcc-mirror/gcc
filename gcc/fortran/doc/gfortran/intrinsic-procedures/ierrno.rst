..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: IERRNO, system, error handling

.. _ierrno:

IERRNO --- Get the last system error number
*******************************************

.. function:: IERRNO()

  Returns the last system error number, as given by the C ``errno``
  variable.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  Standard:
    GNU extension

  Class:
    Function

  Syntax:
    .. code-block:: fortran

      RESULT = IERRNO()

  Arguments:
    None

  See also:
    :ref:`PERROR`