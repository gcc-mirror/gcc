..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FSEEK, file operation, seek, file operation, position

.. _fseek:

FSEEK --- Low level file positioning subroutine
***********************************************

.. function:: FSEEK(UNIT, OFFSET, WHENCE, STATUS)

  Moves :samp:`{UNIT}` to the specified :samp:`{OFFSET}`. If :samp:`{WHENCE}`
  is set to 0, the :samp:`{OFFSET}` is taken as an absolute value ``SEEK_SET``,
  if set to 1, :samp:`{OFFSET}` is taken to be relative to the current position
  ``SEEK_CUR``, and if set to 2 relative to the end of the file ``SEEK_END``.
  On error, :samp:`{STATUS}` is set to a nonzero value. If :samp:`{STATUS}` the seek
  fails silently.

  :param UNIT:
    Shall be a scalar of type ``INTEGER``.

  :param OFFSET:
    Shall be a scalar of type ``INTEGER``.

  :param WHENCE:
    Shall be a scalar of type ``INTEGER``.
    Its value shall be either 0, 1 or 2.

  :param STATUS:
    (Optional) shall be a scalar of type
    ``INTEGER(4)``.

  Standard:
    GNU extension

  Class:
    Subroutine

  Syntax:
    .. code-block:: fortran

      CALL FSEEK(UNIT, OFFSET, WHENCE[, STATUS])

  Example:
    .. code-block:: fortran

      PROGRAM test_fseek
        INTEGER, PARAMETER :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
        INTEGER :: fd, offset, ierr

        ierr   = 0
        offset = 5
        fd     = 10

        OPEN(UNIT=fd, FILE="fseek.test")
        CALL FSEEK(fd, offset, SEEK_SET, ierr)  ! move to OFFSET
        print *, FTELL(fd), ierr

        CALL FSEEK(fd, 0, SEEK_END, ierr)       ! move to end
        print *, FTELL(fd), ierr

        CALL FSEEK(fd, 0, SEEK_SET, ierr)       ! move to beginning
        print *, FTELL(fd), ierr

        CLOSE(UNIT=fd)
      END PROGRAM

  See also:
    :ref:`FTELL`
