..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: FGET, read character, stream mode, stream mode, read character, file operation, read character

.. _fget:

FGET --- Read a single character in stream mode from stdin
***********************************************************

.. function:: FGET(C)

  Read a single character in stream mode from stdin by bypassing normal
  formatted output. Stream I/O should not be mixed with normal record-oriented
  (formatted or unformatted) I/O on the same unit; the results are unpredictable.

  :param C:
    The type shall be ``CHARACTER`` and of default
    kind.

  :param STATUS:
    (Optional) status flag of type ``INTEGER``.
    Returns 0 on success, -1 on end-of-file, and a system specific positive
    error code otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL FGET(C [, STATUS])
      STATUS = FGET(C)

  Example:
    .. code-block:: fortran

      PROGRAM test_fget
        INTEGER, PARAMETER :: strlen = 100
        INTEGER :: status, i = 1
        CHARACTER(len=strlen) :: str = ""

        WRITE (*,*) 'Enter text:'
        DO
          CALL fget(str(i:i), status)
          if (status /= 0 .OR. i > strlen) exit
          i = i + 1
        END DO
        WRITE (*,*) TRIM(str)
      END PROGRAM

  See also:
    :ref:`FGETC`,
    :ref:`FPUT`,
    :ref:`FPUTC`
