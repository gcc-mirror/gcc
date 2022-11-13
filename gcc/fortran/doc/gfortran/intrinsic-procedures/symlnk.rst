..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SYMLNK, file system, create link, file system, soft link

.. _symlnk:

SYMLNK --- Create a symbolic link
*********************************

.. function:: SYMLNK(PATH1, PATH2)

  Makes a symbolic link from file :samp:`{PATH1}` to :samp:`{PATH2}`. A null
  character (``CHAR(0)``) can be used to mark the end of the names in
  :samp:`{PATH1}` and :samp:`{PATH2}` ; otherwise, trailing blanks in the file
  names are ignored.  If the :samp:`{STATUS}` argument is supplied, it
  contains 0 on success or a nonzero error code upon return; see
  ``symlink(2)``.  If the system does not supply ``symlink(2)``,
  ``ENOSYS`` is returned.

  :param PATH1:
    Shall be of default ``CHARACTER`` type.

  :param PATH2:
    Shall be of default ``CHARACTER`` type.

  :param STATUS:
    (Optional) Shall be of default ``INTEGER`` type.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL SYMLNK(PATH1, PATH2 [, STATUS])
      STATUS = SYMLNK(PATH1, PATH2)

  See also:
    :ref:`LINK`,
    :ref:`UNLINK`