..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _invoking-gccgo:

Invoking gccgo
--------------

.. only:: man

  Synopsis
  ^^^^^^^^

  gccgo [ :option:`-c` | :option:`-S` ]
        [ :option:`-g` ] [ :option:`-pg` ] [ :option:`-O`:samp:`{level}` ]
        [ :option:`-I dir...` ] [ :option:`-L dir...` ]
        [ :option:`-o` :samp:`{outfile}` ] :samp:`{infile}`...

Description
^^^^^^^^^^^

Only the most useful options are listed here; see below for the
remainder.

The :command:`gccgo` command is a frontend to :command:`gcc` and
supports many of the same options.  See :ref:`gcc:option-summary`.  This manual
only documents the options specific to :command:`gccgo`.

The :command:`gccgo` command may be used to compile Go source code into
an object file, link a collection of object files together, or do both
in sequence.

Go source code is compiled as packages.  A package consists of one or
more Go source files.  All the files in a single package must be
compiled together, by passing all the files as arguments to
:command:`gccgo`.  A single invocation of :command:`gccgo` may only
compile a single package.

One Go package may ``import`` a different Go package.  The imported
package must have already been compiled; :command:`gccgo` will read
the import data directly from the compiled package.  When this package
is later linked, the compiled form of the package must be included in
the link command.

Go programs must generally be compiled with debugging information, and
:option:`-g1` is the default as described below.  Stripping a Go
program will generally cause it to misbehave or fail.

Options
^^^^^^^

.. option:: -Idir

  .. index:: -I

  Specify a directory to use when searching for an import package at
  compile time.

.. option:: -Ldir

  .. index:: -L

  When linking, specify a library search directory, as with
  :command:`gcc`.

.. option:: -fgo-pkgpath=string

  .. index:: -fgo-pkgpath

  Set the package path to use.  This sets the value returned by the
  PkgPath method of reflect.Type objects.  It is also used for the names
  of globally visible symbols.  The argument to this option should
  normally be the string that will be used to import this package after
  it has been installed; in other words, a pathname within the
  directories specified by the :option:`-I` option.

.. option:: -fgo-prefix=string

  .. index:: -fgo-prefix

  An alternative to :option:`-fgo-pkgpath`.  The argument will be
  combined with the package name from the source file to produce the
  package path.  If :option:`-fgo-pkgpath` is used, :option:`-fgo-prefix`
  will be ignored.

  Go permits a single program to include more than one package with the
  same name in the ``package`` clause in the source file, though
  obviously the two packages must be imported using different pathnames.
  In order for this to work with :command:`gccgo`, either
  :option:`-fgo-pkgpath` or :option:`-fgo-prefix` must be specified when
  compiling a package.

  Using either :option:`-fgo-pkgpath` or :option:`-fgo-prefix` disables
  the special treatment of the ``main`` package and permits that
  package to be imported like any other.

.. option:: -fgo-relative-import-path=dir

  .. index:: -fgo-relative-import-path

  A relative import is an import that starts with :samp:`./` or
  :samp:`../`.  If this option is used, :command:`gccgo` will use
  :samp:`{dir}` as a prefix for the relative import when searching for it.

.. option:: -frequire-return-statement
.. option:: -fno-require-return-statement

  .. index:: -frequire-return-statement, -fno-require-return-statement

  By default :command:`gccgo` will warn about functions which have one or
  more return parameters but lack an explicit ``return`` statement.
  This warning may be disabled using
  :option:`-fno-require-return-statement`.

.. option:: -fgo-check-divide-zero

  .. index:: -fgo-check-divide-zero, -fno-go-check-divide-zero

  Add explicit checks for division by zero.  In Go a division (or
  modulos) by zero causes a panic.  On Unix systems this is detected in
  the runtime by catching the ``SIGFPE`` signal.  Some processors,
  such as PowerPC, do not generate a SIGFPE on division by zero.  Some
  runtimes do not generate a signal that can be caught.  On those
  systems, this option may be used.  Or the checks may be removed via
  :option:`-fno-go-check-divide-zero`.  This option is currently on by
  default, but in the future may be off by default on systems that do
  not require it.

.. option:: -fgo-check-divide-overflow

  .. index:: -fgo-check-divide-overflow, -fno-go-check-divide-overflow

  Add explicit checks for division overflow.  For example, division
  overflow occurs when computing ``INT_MIN / -1``.  In Go this should
  be wrapped, to produce ``INT_MIN``.  Some processors, such as x86,
  generate a trap on division overflow.  On those systems, this option
  may be used.  Or the checks may be removed via
  :option:`-fno-go-check-divide-overflow`.  This option is currently on
  by default, but in the future may be off by default on systems that do
  not require it.

.. option:: -fno-go-optimize-allocs

  .. index:: -fno-go-optimize-allocs

  Disable escape analysis, which tries to allocate objects on the stack
  rather than the heap.

.. option:: -fgo-debug-escapen

  .. index:: -fgo-debug-escape

  Output escape analysis debugging information.  Larger values of
  :samp:`{n}` generate more information.

.. option:: -fgo-debug-escape-hash=n

  .. index:: -fgo-debug-escape-hash

  A hash value to debug escape analysis.  :samp:`{n}` is a binary string.
  This runs escape analysis only on functions whose names hash to values
  that match the given suffix :samp:`{n}`.  This can be used to binary
  search across functions to uncover escape analysis bugs.

.. option:: -fgo-debug-optimization

  .. index:: -fgo-debug-optimization, -fno-go-debug-optimization

  Output optimization diagnostics.

.. option:: -fgo-c-header=file

  .. index:: -fgo-c-header

  Write top-level named Go struct definitions to :samp:`{file}` as C code.
  This is used when compiling the runtime package.

.. option:: -fgo-compiling-runtime

  .. index:: -fgo-compiling-runtime

  Apply special rules for compiling the runtime package.  Implicit
  memory allocation is forbidden.  Some additional compiler directives
  are supported.

.. option:: -fgo-embedcfg=file

  .. index:: -fgo-embedcfg

  Identify a JSON file used to map patterns used with special
  ``//go:embed`` comments to the files named by the patterns.  The
  JSON file should have two components: ``Patterns`` maps each
  pattern to a list of file names, and ``Files`` maps each file name
  to a full path to the file.  This option is intended for use by the
  :command:`go` command to implement ``//go:embed``.

.. option:: -g

  .. index:: -g for gccgo

  This is the standard :command:`gcc` option (see :ref:`gcc:debugging-options`).  It
  is mentioned here because by default :command:`gccgo` turns on
  debugging information generation with the equivalent of the standard
  option :option:`-g1`.  This is because Go programs require debugging
  information to be available in order to get backtrace information.  An
  explicit :option:`-g0` may be used to disable the generation of
  debugging information, in which case certain standard library
  functions, such as ``runtime.Callers``, will not operate correctly.

.. only:: man

  .. include:: copyright.rst