..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: directory options, options, directory search, search path

.. _directory-options:

Options for Directory Search
****************************

These options specify directories to search for header files, for
libraries and for parts of the compiler:

.. include:: ../../../../doc/cppdiropts.rst

.. option:: -iplugindir={dir}

  Set the directory to search for plugins that are passed
  by :option:`-fplugin=name` instead of
  :option:`-fplugin=path/name.so`.  This option is not meant
  to be used by the user, but only passed by the driver.

.. option:: -Ldir

  Add directory :samp:`{dir}` to the list of directories to be searched
  for :option:`-l`.

.. option:: -Bprefix

  This option specifies where to find the executables, libraries,
  include files, and data files of the compiler itself.

  The compiler driver program runs one or more of the subprograms
  :command:`cpp`, :command:`cc1`, :command:`as` and :command:`ld`.  It tries
  :samp:`{prefix}` as a prefix for each program it tries to run, both with and
  without :samp:`{machine}/{version}/` for the corresponding target
  machine and compiler version.

  For each subprogram to be run, the compiler driver first tries the
  :option:`-B` prefix, if any.  If that name is not found, or if :option:`-B`
  is not specified, the driver tries two standard prefixes,
  :samp:`/usr/lib/gcc/` and :samp:`/usr/local/lib/gcc/`.  If neither of
  those results in a file name that is found, the unmodified program
  name is searched for using the directories specified in your
  :envvar:`PATH` environment variable.

  The compiler checks to see if the path provided by :option:`-B`
  refers to a directory, and if necessary it adds a directory
  separator character at the end of the path.

  :option:`-B` prefixes that effectively specify directory names also apply
  to libraries in the linker, because the compiler translates these
  options into :option:`-L` options for the linker.  They also apply to
  include files in the preprocessor, because the compiler translates these
  options into :option:`-isystem` options for the preprocessor.  In this case,
  the compiler appends :samp:`include` to the prefix.

  The runtime support file :samp:`libgcc.a` can also be searched for using
  the :option:`-B` prefix, if needed.  If it is not found there, the two
  standard prefixes above are tried, and that is all.  The file is left
  out of the link if it is not found by those means.

  Another way to specify a prefix much like the :option:`-B` prefix is to use
  the environment variable :envvar:`GCC_EXEC_PREFIX`.  See :ref:`environment-variables`.

  As a special kludge, if the path provided by :option:`-B` is
  :samp:`[dir/]stage{N}/`, where :samp:`{N}` is a number in the range 0 to
  9, then it is replaced by :samp:`[dir/]include`.  This is to help
  with boot-strapping the compiler.

.. option:: -no-canonical-prefixes

  Do not expand any symbolic links, resolve references to :samp:`/../`
  or :samp:`/./`, or make the path absolute when generating a relative
  prefix.

.. option:: --sysroot={dir}

  Use :samp:`{dir}` as the logical root directory for headers and libraries.
  For example, if the compiler normally searches for headers in
  :samp:`/usr/include` and libraries in :samp:`/usr/lib`, it instead
  searches :samp:`{dir}/usr/include` and :samp:`{dir}/usr/lib`.

  If you use both this option and the :option:`-isysroot` option, then
  the :option:`--sysroot` option applies to libraries, but the
  :option:`-isysroot` option applies to header files.

  The GNU linker (beginning with version 2.16) has the necessary support
  for this option.  If your linker does not support this option, the
  header file aspect of :option:`--sysroot` still works, but the
  library aspect does not.

.. option:: --no-sysroot-suffix

  For some targets, a suffix is added to the root directory specified
  with :option:`--sysroot`, depending on the other options used, so that
  headers may for example be found in
  :samp:`{dir}/{suffix}/usr/include` instead of
  :samp:`{dir}/usr/include`.  This option disables the addition of
  such a suffix.