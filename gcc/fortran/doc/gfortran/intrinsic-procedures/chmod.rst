..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _chmod:

CHMOD --- Change access permissions of files
********************************************

.. index:: CHMOD(NAME, MODE, STATUS), file system, change access mode

.. function:: CHMOD(NAME, MODE, STATUS)

  ``CHMOD`` changes the permissions of a file.

  :param NAME:
    Scalar ``CHARACTER`` of default kind with the
    file name. Trailing blanks are ignored unless the character
    ``achar(0)`` is present, then all characters up to and excluding
    ``achar(0)`` are used as the file name.

  :param MODE:
    Scalar ``CHARACTER`` of default kind giving the
    file permission. :samp:`{MODE}` uses the same syntax as the ``chmod`` utility
    as defined by the POSIX standard. The argument shall either be a string of
    a nonnegative octal number or a symbolic mode.

  :param STATUS:
    (optional) scalar ``INTEGER``, which is
    ``0`` on success and nonzero otherwise.

  :return:
    In either syntax, :samp:`{STATUS}` is set to ``0`` on success and nonzero
    otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL CHMOD(NAME, MODE[, STATUS])
      STATUS = CHMOD(NAME, MODE)

  Example:
    ``CHMOD`` as subroutine

    .. code-block:: fortran

      program chmod_test
        implicit none
        integer :: status
        call chmod('test.dat','u+x',status)
        print *, 'Status: ', status
      end program chmod_test

    ``CHMOD`` as function:

    .. code-block:: fortran

      program chmod_test
        implicit none
        integer :: status
        status = chmod('test.dat','u+x')
        print *, 'Status: ', status
      end program chmod_test
