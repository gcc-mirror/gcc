..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: configuration file, xm-machine.h

.. _filesystem:

Host Filesystem
***************

GCC needs to know a number of things about the semantics of the host
machine's filesystem.  Filesystems with Unix and MS-DOS semantics are
automatically detected.  For other systems, you can define the
following macros in :samp:`xm-{machine}.h`.

.. envvar:: HAVE_DOS_BASED_FILE_SYSTEM

  This macro is automatically defined by :samp:`system.h` if the host
  file system obeys the semantics defined by MS-DOS instead of Unix.
  DOS file systems are case insensitive, file specifications may begin
  with a drive letter, and both forward slash and backslash (``/``
  and ``\``) are directory separators.

.. envvar:: DIR_SEPARATOR

  If defined, these macros expand to character constants specifying
  separators for directory names within a file specification.
  :samp:`system.h` will automatically give them appropriate values on
  Unix and MS-DOS file systems.  If your file system is neither of
  these, define one or both appropriately in :samp:`xm-{machine}.h`.

  However, operating systems like VMS, where constructing a pathname is
  more complicated than just stringing together directory names
  separated by a special character, should not define either of these
  macros.

.. envvar:: PATH_SEPARATOR

  If defined, this macro should expand to a character constant
  specifying the separator for elements of search paths.  The default
  value is a colon (:samp:`:`).  DOS-based systems usually, but not
  always, use semicolon (:samp:`;`).

``VMS``
  Define this macro if the host system is VMS.

.. envvar:: HOST_OBJECT_SUFFIX

  Define this macro to be a C string representing the suffix for object
  files on your host machine.  If you do not define this macro, GCC will
  use :samp:`.o` as the suffix for object files.

.. envvar:: HOST_EXECUTABLE_SUFFIX

  Define this macro to be a C string representing the suffix for
  executable files on your host machine.  If you do not define this macro,
  GCC will use the null string as the suffix for executable files.

.. envvar:: HOST_BIT_BUCKET

  A pathname defined by the host operating system, which can be opened as
  a file and written to, but all the information written is discarded.
  This is commonly known as a :dfn:`bit bucket` or :dfn:`null device`.  If
  you do not define this macro, GCC will use :samp:`/dev/null` as the bit
  bucket.  If the host does not support a bit bucket, define this macro to
  an invalid filename.

:samp:`UPDATE_PATH_HOST_CANONICALIZE ({path})`
  If defined, a C statement (sans semicolon) that performs host-dependent
  canonicalization when a path used in a compilation driver or
  preprocessor is canonicalized.  :samp:`{path}` is a malloc-ed path to be
  canonicalized.  If the C statement does canonicalize :samp:`{path}` into a
  different buffer, the old path should be freed and the new buffer should
  have been allocated with malloc.

.. envvar:: DUMPFILE_FORMAT

  Define this macro to be a C string representing the format to use for
  constructing the index part of debugging dump file names.  The resultant
  string must fit in fifteen bytes.  The full filename will be the
  concatenation of: the prefix of the assembler file name, the string
  resulting from applying this format to an index number, and a string
  unique to each dump file kind, e.g. :samp:`rtl`.

  If you do not define this macro, GCC will use :samp:`.%02d.`.  You should
  define this macro if using the default will create an invalid file name.

.. envvar:: DELETE_IF_ORDINARY

  Define this macro to be a C statement (sans semicolon) that performs
  host-dependent removal of ordinary temp files in the compilation driver.

  If you do not define this macro, GCC will use the default version.  You
  should define this macro if the default version does not reliably remove
  the temp file as, for example, on VMS which allows multiple versions
  of a file.

.. envvar:: HOST_LACKS_INODE_NUMBERS

  Define this macro if the host filesystem does not report meaningful inode
  numbers in struct stat.
