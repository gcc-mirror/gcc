..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: lto-dump

.. _lto-dump:

lto-dump---Tool for dumping LTO object files.
---------------------------------------------

.. only:: man

  Synopsis
  ^^^^^^^^

  lto-dump
      [ :option:`-list` ]
      [ :option:`-demangle` ]
      [ :option:`-defined-only` ]
      [ :option:`-print-value` ]
      [ :option:`-name-sort` ]
      [ :option:`-size-sort` ]
      [ :option:`-reverse-sort` ]
      [ :option:`-no-sort` ]
      [ :option:`-symbol=` ]
      [ :option:`-objects` ]
      [ :option:`-type-stats` ]
      [ :option:`-tree-stats` ]
      [ :option:`-gimple-stats` ]
      [ :option:`-dump-level=` ]
      [ :option:`-dump-body=` ]
      [ :option:`-help` ] :samp:`{lto-dump}`

.. only:: not man

  .. code-block::

    Usage: lto-dump [OPTION] ... objfiles

Description
^^^^^^^^^^^

:command:`lto-dump` is a tool you can use in conjunction with GCC to
dump link time optimization object files.

Options
^^^^^^^

.. option:: -list

  Dumps list of details of functions and variables.

.. option:: -demangle

  Dump the demangled output.

.. option:: -defined-only

  Dump only the defined symbols.

.. option:: -print-value

  Dump initial values of the variables.

.. option:: -name-sort

  Sort the symbols alphabetically.

.. option:: -size-sort

  Sort the symbols according to size.

.. option:: -reverse-sort

  Dump the symbols in reverse order.

.. option:: -no-sort

  Dump the symbols in order of occurrence.

.. option:: -symbol=

  Dump the details of specific symbol.

.. option:: -objects

  Dump the details of LTO objects.

.. option:: -type-stats

  Dump the statistics of tree types.

.. option:: -tree-stats

  Dump the statistics of trees.

.. option:: -gimple-stats

  Dump the statistics of gimple statements.

.. option:: -dump-level=

  For deciding the optimization level of body.

.. option:: -dump-body=

  Dump the specific gimple body.

.. option:: -help

  Display the dump tool help.

.. only:: man

  .. include:: copyright.rst
