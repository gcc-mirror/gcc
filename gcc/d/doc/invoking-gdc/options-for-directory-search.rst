..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: directory options, options, directory search, search path

.. _directory-options:

Options for Directory Search
****************************

These options specify directories to search for files, libraries, and
other parts of the compiler:

.. option:: -Idir

  .. index:: -I

  Specify a directory to use when searching for imported modules at
  compile time.  Multiple :option:`-I` options can be used, and the
  paths are searched in the same order.

.. option:: -Jdir

  .. index:: -J

  Specify a directory to use when searching for files in string imports
  at compile time.  This switch is required in order to use
  ``import(file)`` expressions.  Multiple :option:`-J` options can be
  used, and the paths are searched in the same order.

.. option:: -Ldir

  .. index:: -L

  When linking, specify a library search directory, as with :command:`gcc`.

.. option:: -Bdir

  .. index:: -B

  This option specifies where to find the executables, libraries,
  source files, and data files of the compiler itself, as with :command:`gcc`.

.. option:: -fmodule-file=module=spec

  .. index:: -fmodule-file

  This option manipulates file paths of imported modules, such that if an
  imported module matches all or the leftmost part of :samp:`{module}`, the file
  path in :samp:`{spec}` is used as the location to search for D sources.
  This is used when the source file path and names are not the same as the
  package and module hierarchy.  Consider the following examples:

  .. code-block:: c++

    gdc test.d -fmodule-file=A.B=foo.d -fmodule-file=C=bar

  This will tell the compiler to search in all import paths for the source
  file :samp:`{foo.d}` when importing :samp:`{A.B}`, and the directory :samp:`{bar/}`
  when importing :samp:`{C}`, as annotated in the following D code:

  .. code-block:: c++

    module test;
    import A.B;     // Matches A.B, searches for foo.d
    import C.D.E;   // Matches C, searches for bar/D/E.d
    import A.B.C;   // No match, searches for A/B/C.d

.. option:: -imultilib dir

  .. index:: -imultilib

  Use :samp:`{dir}` as a subdirectory of the gcc directory containing
  target-specific D sources and interfaces.

.. option:: -iprefix prefix

  .. index:: -iprefix

  Specify :samp:`{prefix}` as the prefix for the gcc directory containing
  target-specific D sources and interfaces.  If the :samp:`{prefix}` represents
  a directory, you should include the final ``'/'``.

.. option:: -nostdinc

  .. index:: -nostdinc

  Do not search the standard system directories for D source and interface
  files.  Only the directories that have been specified with :option:`-I` options
  (and the directory of the current file, if appropriate) are searched.
