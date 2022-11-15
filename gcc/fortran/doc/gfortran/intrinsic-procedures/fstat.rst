..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _fstat:

FSTAT --- Get file status
*************************

.. index:: FSTAT, file system, file status

.. function:: FSTAT(UNIT, VALUES, STATUS)

  ``FSTAT`` is identical to :ref:`STAT`, except that information about an
  already opened file is obtained.

  :param UNIT:
    An open I/O unit number of type ``INTEGER``.

  :param VALUES:
    The type shall be ``INTEGER(4), DIMENSION(13)``.

  :param STATUS:
    (Optional) status flag of type ``INTEGER(4)``. Returns 0
    on success and a system specific error code otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL FSTAT(UNIT, VALUES [, STATUS])
      STATUS = FSTAT(UNIT, VALUES)

  Example:
    See :ref:`STAT` for an example.

  See also:
    To stat a link:
    :ref:`LSTAT`
    To stat a file:
    :ref:`STAT`
