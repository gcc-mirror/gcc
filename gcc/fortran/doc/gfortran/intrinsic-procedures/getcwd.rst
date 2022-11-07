..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GETCWD, system, working directory

.. _getcwd:

GETCWD --- Get current working directory
****************************************

.. function:: GETCWD(C)

  Get current working directory.

  :param C:
    The type shall be ``CHARACTER`` and of default kind.

  :param STATUS:
    (Optional) status flag. Returns 0 on success,
    a system specific and nonzero error code otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL GETCWD(C [, STATUS])
      STATUS = GETCWD(C)

  Example:
    .. code-block:: fortran

      PROGRAM test_getcwd
        CHARACTER(len=255) :: cwd
        CALL getcwd(cwd)
        WRITE(*,*) TRIM(cwd)
      END PROGRAM

  See also:
    :ref:`CHDIR`