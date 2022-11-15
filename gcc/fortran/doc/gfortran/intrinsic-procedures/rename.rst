..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: RENAME, file system, rename file

.. _rename:

RENAME --- Rename a file
************************

.. function:: RENAME(PATH1, PATH2)

  Renames a file from file :samp:`{PATH1}` to :samp:`{PATH2}`. A null
  character (``CHAR(0)``) can be used to mark the end of the names in
  :samp:`{PATH1}` and :samp:`{PATH2}` ; otherwise, trailing blanks in the file
  names are ignored.  If the :samp:`{STATUS}` argument is supplied, it
  contains 0 on success or a nonzero error code upon return; see
  ``rename(2)``.

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

      CALL RENAME(PATH1, PATH2 [, STATUS])
      STATUS = RENAME(PATH1, PATH2)

  See also:
    :ref:`LINK`
