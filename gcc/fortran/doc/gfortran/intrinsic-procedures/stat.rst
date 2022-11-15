..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: STAT, file system, file status

.. _stat:

STAT --- Get file status
************************

.. function:: STAT(NAME, VALUES)

  This function returns information about a file. No permissions are required on
  the file itself, but execute (search) permission is required on all of the
  directories in path that lead to the file.

  :param NAME:
    The type shall be ``CHARACTER``, of the
    default kind and a valid path within the file system.

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

      CALL STAT(NAME, VALUES [, STATUS])
      STATUS = STAT(NAME, VALUES)

  Example:
    .. code-block:: fortran

      PROGRAM test_stat
        INTEGER, DIMENSION(13) :: buff
        INTEGER :: status

        CALL STAT("/etc/passwd", buff, status)

        IF (status == 0) THEN
          WRITE (*, FMT="('Device ID:',               T30, I19)") buff(1)
          WRITE (*, FMT="('Inode number:',            T30, I19)") buff(2)
          WRITE (*, FMT="('File mode (octal):',       T30, O19)") buff(3)
          WRITE (*, FMT="('Number of links:',         T30, I19)") buff(4)
          WRITE (*, FMT="('Owner''s uid:',            T30, I19)") buff(5)
          WRITE (*, FMT="('Owner''s gid:',            T30, I19)") buff(6)
          WRITE (*, FMT="('Device where located:',    T30, I19)") buff(7)
          WRITE (*, FMT="('File size:',               T30, I19)") buff(8)
          WRITE (*, FMT="('Last access time:',        T30, A19)") CTIME(buff(9))
          WRITE (*, FMT="('Last modification time',   T30, A19)") CTIME(buff(10))
          WRITE (*, FMT="('Last status change time:', T30, A19)") CTIME(buff(11))
          WRITE (*, FMT="('Preferred block size:',    T30, I19)") buff(12)
          WRITE (*, FMT="('No. of blocks allocated:', T30, I19)") buff(13)
        END IF
      END PROGRAM

  See also:
    To stat an open file:
    :ref:`FSTAT`
    To stat a link:
    :ref:`LSTAT`
