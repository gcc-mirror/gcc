..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: UNLINK, file system, remove file

.. _unlink:

UNLINK --- Remove a file from the file system
*********************************************

.. function:: UNLINK(PATH)

  Unlinks the file :samp:`{PATH}`. A null character (``CHAR(0)``) can be
  used to mark the end of the name in :samp:`{PATH}` ; otherwise, trailing
  blanks in the file name are ignored.  If the :samp:`{STATUS}` argument is
  supplied, it contains 0 on success or a nonzero error code upon return;
  see ``unlink(2)``.

  :param PATH:
    Shall be of default ``CHARACTER`` type.

  :param STATUS:
    (Optional) Shall be of default ``INTEGER`` type.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL UNLINK(PATH [, STATUS])
      STATUS = UNLINK(PATH)

  See also:
    :ref:`LINK`,
    :ref:`SYMLNK`