..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: directory, options, options, directory search, search path, INCLUDE directive, directive, INCLUDE

.. _directory-options:

Options for directory search
****************************

These options affect how GNU Fortran searches
for files specified by the ``INCLUDE`` directive and where it searches
for previously compiled modules.

It also affects the search paths used by :command:`cpp` when used to preprocess
Fortran source.

.. index:: Idir, directory, search paths for inclusion, inclusion, directory search paths for, search paths, for included files, paths, search, module search path

.. option:: -Idir

  These affect interpretation of the ``INCLUDE`` directive
  (as well as of the ``#include`` directive of the :command:`cpp`
  preprocessor).

  Also note that the general behavior of :option:`-I` and
  ``INCLUDE`` is pretty much the same as of :option:`-I` with
  ``#include`` in the :command:`cpp` preprocessor, with regard to
  looking for :samp:`header.gcc` files and other such things.

  This path is also used to search for :samp:`.mod` files when previously
  compiled modules are required by a ``USE`` statement.

  See :ref:`gcc:directory-options`, for information on the
  :option:`-I` option.

.. index:: Jdir, Mdir, paths, search, module search path

.. option:: -Jdir

  This option specifies where to put :samp:`.mod` files for compiled modules.
  It is also added to the list of directories to searched by an ``USE``
  statement.

  The default is the current directory.

.. index:: fintrinsic-modules-pathdir, paths, search, module search path

.. option:: -fintrinsic-modules-path {dir}

  This option specifies the location of pre-compiled intrinsic modules, if
  they are not in the default location expected by the compiler.
