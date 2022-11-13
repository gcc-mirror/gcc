..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: gcov-dump

.. _gcov-dump:

gcov-dump---an Offline Gcda and Gcno Profile Dump Tool
------------------------------------------------------

.. only:: man

  Synopsis
  ^^^^^^^^

  gcov-dump
       [ :option:`-v` | :option:`--version` ]
       [ :option:`-h` | :option:`--help` ]
       [ :option:`-l` | :option:`--long` ]
       [ :option:`-p` | :option:`--positions` ]
       [ :option:`-r` | :option:`--raw` ]
       [ :option:`-s` | :option:`--stable` ]
       [ :samp:`{gcovfiles}` ]

.. only:: not man

  .. code-block::

    gcov-dump [OPTION] ... gcovfiles

Description
^^^^^^^^^^^

:command:`gcov-dump` is a tool you can use in conjunction with GCC to
dump content of gcda and gcno profile files offline.

Options
^^^^^^^

.. option:: -h, --help

  Display help about using :command:`gcov-dump` (on the standard output), and
  exit without doing any further processing.

.. option:: -l, --long

  Dump content of records.

.. option:: -p, --positions

  Dump positions of records.

.. option:: -r, --raw

  Print content records in raw format.

.. option:: -s, --stable

  Print content in stable format usable for comparison.

.. option:: -v, --version

  Display the :command:`gcov-dump` version number (on the standard output),
  and exit without doing any further processing.

.. only:: man

  .. include:: copyright.rst