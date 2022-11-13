..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: file, symbolic link

.. _file-operations-on-symbolic-links:

File operations on symbolic links
*********************************

This section documents the behavior of GNU Fortran for file operations on
symbolic links, on systems that support them.

* Results of INQUIRE statements of the 'inquire by file' form will
  relate to the target of the symbolic link. For example,
  ``INQUIRE(FILE="foo",EXIST=ex)`` will set :samp:`{ex}` to :samp:`{.true.}` if
  :samp:`{foo}` is a symbolic link pointing to an existing file, and :samp:`{.false.}`
  if :samp:`{foo}` points to an non-existing file ('dangling' symbolic link).

* Using the ``OPEN`` statement with a ``STATUS="NEW"`` specifier
  on a symbolic link will result in an error condition, whether the symbolic
  link points to an existing target or is dangling.

* If a symbolic link was connected, using the ``CLOSE`` statement
  with a ``STATUS="DELETE"`` specifier will cause the symbolic link itself
  to be deleted, not its target.