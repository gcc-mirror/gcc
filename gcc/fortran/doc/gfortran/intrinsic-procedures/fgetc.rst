..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FGETC, read character, stream mode, stream mode, read character, file operation, read character

.. _fgetc:

FGETC --- Read a single character in stream mode
************************************************

.. function:: FGETC(UNIT, C)

  Read a single character in stream mode by bypassing normal formatted output.
  Stream I/O should not be mixed with normal record-oriented (formatted or
  unformatted) I/O on the same unit; the results are unpredictable.

  :param UNIT:
    The type shall be ``INTEGER``.

  :param C:
    The type shall be ``CHARACTER`` and of default
    kind.

  :param STATUS:
    (Optional) status flag of type ``INTEGER``.
    Returns 0 on success, -1 on end-of-file and a system specific positive
    error code otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL FGETC(UNIT, C [, STATUS])
      STATUS = FGETC(UNIT, C)

  Example:
    .. code-block:: fortran

      PROGRAM test_fgetc
        INTEGER :: fd = 42, status
        CHARACTER :: c

        OPEN(UNIT=fd, FILE="/etc/passwd", ACTION="READ", STATUS = "OLD")
        DO
          CALL fgetc(fd, c, status)
          IF (status /= 0) EXIT
          call fput(c)
        END DO
        CLOSE(UNIT=fd)
      END PROGRAM

  See also:
    :ref:`FGET`,
    :ref:`FPUT`,
    :ref:`FPUTC`
