..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FPUTC, write character, stream mode, stream mode, write character, file operation, write character

.. _fputc:

FPUTC --- Write a single character in stream mode
*************************************************

.. function:: FPUTC(UNIT, C)

  Write a single character in stream mode by bypassing normal formatted
  output. Stream I/O should not be mixed with normal record-oriented
  (formatted or unformatted) I/O on the same unit; the results are unpredictable.

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

      CALL FPUTC(UNIT, C [, STATUS])
      STATUS = FPUTC(UNIT, C)

  Example:
    .. code-block:: fortran

      PROGRAM test_fputc
        CHARACTER(len=10) :: str = "gfortran"
        INTEGER :: fd = 42, i

        OPEN(UNIT = fd, FILE = "out", ACTION = "WRITE", STATUS="NEW")
        DO i = 1, len_trim(str)
          CALL fputc(fd, str(i:i))
        END DO
        CLOSE(fd)
      END PROGRAM

  See also:
    :ref:`FPUT`,
    :ref:`FGET`,
    :ref:`FGETC`
