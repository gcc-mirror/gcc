..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _gcov:

gcov---a Test Coverage Program
------------------------------

:command:`gcov` is a tool you can use in conjunction with GCC to
test code coverage in your programs.

.. only:: man

  Synopsis
  ^^^^^^^^

  gcov [ :option:`-v` | :option:`--version` ] [ :option:`-h` | :option:`--help` ]
      [ :option:`-a` | :option:`--all-blocks` ]
      [ :option:`-b` | :option:`--branch-probabilities` ]
      [ :option:`-c` | :option:`--branch-counts` ]
      [ :option:`-d` | :option:`--display-progress` ]
      [ :option:`-f` | :option:`--function-summaries` ]
      [ :option:`-j` | :option:`--json-format` ]
      [ :option:`-H` | :option:`--human-readable` ]
      [ :option:`-k` | :option:`--use-colors` ]
      [ :option:`-l` | :option:`--long-file-names` ]
      [ :option:`-m` | :option:`--demangled-names` ]
      [ :option:`-n` | :option:`--no-output` ]
      [ :option:`-o` | :option:`--object-directory` :samp:`{directory|file}` ]
      [ :option:`-p` | :option:`--preserve-paths` ]
      [ :option:`-q` | :option:`--use-hotness-colors` ]
      [ :option:`-r` | :option:`--relative-only` ]
      [ :option:`-s` | :option:`--source-prefix` :samp:`{directory}` ]
      [ :option:`-t` | :option:`--stdout` ]
      [ :option:`-u` | :option:`--unconditional-branches` ]
      [ :option:`-x` | :option:`--hash-filenames` ]
      :samp:`{files}`

.. toctree::
  :maxdepth: 2

  gcov/introduction-to-gcov
  gcov/invoking-gcov
  gcov/using-gcov-with-gcc-optimization
  gcov/brief-description-of-gcov-data-files
  gcov/data-file-relocation-to-support-cross-profiling
  gcov/profiling-and-test-coverage-in-freestanding-environments

.. only:: man

  .. include:: copyright.rst
