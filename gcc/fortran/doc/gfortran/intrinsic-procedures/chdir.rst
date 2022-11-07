..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: CHDIR, system, working directory

.. _chdir:

CHDIR --- Change working directory
**********************************

.. function:: CHDIR(NAME)

  Change current working directory to a specified path.

  :param NAME:
    The type shall be ``CHARACTER`` of default
    kind and shall specify a valid path within the file system.

  :param STATUS:
    (Optional) ``INTEGER`` status flag of the default
    kind.  Returns 0 on success, and a system specific and nonzero error code
    otherwise.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL CHDIR(NAME [, STATUS])
      STATUS = CHDIR(NAME)

  Example:
    .. code-block:: fortran

      PROGRAM test_chdir
        CHARACTER(len=255) :: path
        CALL getcwd(path)
        WRITE(*,*) TRIM(path)
        CALL chdir("/tmp")
        CALL getcwd(path)
        WRITE(*,*) TRIM(path)
      END PROGRAM

  See also:
    :ref:`GETCWD`