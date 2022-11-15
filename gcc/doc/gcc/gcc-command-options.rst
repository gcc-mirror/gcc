..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GCC command options, command options, options, GCC command

.. _invoking-gcc:

GCC Command Options
--------------------

.. only:: man

  Synopsis
  ^^^^^^^^

  gcc [ :option:`-c` | :option:`-S` | :option:`-E` ] [ :option:`-std`:samp:`={standard}` ]
      [ :option:`-g` ] [ :option:`-pg` ] [ :option:`-O`:samp:`{level}` ]
      [ :option:`-W`:samp:`{warn}`...] [ :option:`-Wpedantic` ]
      [ :option:`-I`:samp:`{dir}`...] [ :option:`-L`:samp:`{dir}`...]
      [ :option:`-D`:samp:`{macro}` [= :samp:`{defn}` ]...] [ :option:`-U`:samp:`{macro}` ]
      [ :option:`-f`:samp:`{option}`...] [ :option:`-m`:samp:`{machine-option}`...]
      [ :option:`-o` :samp:`{outfile}` ] [@ :samp:`{file}` ] :samp:`{infile}`...

  Only the most useful options are listed here; see below for the
  remainder.  :command:`g++` accepts mostly the same options as :command:`gcc`.

  For instructions on reporting bugs, see
  |bugurl|.

  See the Info entry for :command:`gcc`, or
  https://gcc.gnu.org/onlinedocs/gcc/Contributors.html,
  for contributors to GCC.


.. toctree::
  :maxdepth: 2

  gcc-command-options/description
  gcc-command-options/option-summary
  gcc-command-options/options-controlling-the-kind-of-output
  gcc-command-options/compiling-c++-programs
  gcc-command-options/options-controlling-c-dialect
  gcc-command-options/options-controlling-c++-dialect
  gcc-command-options/options-controlling-objective-c-and-objective-c++-dialects
  gcc-command-options/options-to-control-diagnostic-messages-formatting
  gcc-command-options/options-to-request-or-suppress-warnings
  gcc-command-options/options-that-control-static-analysis
  gcc-command-options/options-for-debugging-your-program
  gcc-command-options/options-that-control-optimization
  gcc-command-options/program-instrumentation-options
  gcc-command-options/options-controlling-the-preprocessor
  gcc-command-options/passing-options-to-the-assembler
  gcc-command-options/options-for-linking
  gcc-command-options/options-for-directory-search
  gcc-command-options/options-for-code-generation-conventions
  gcc-command-options/gcc-developer-options
  gcc-command-options/machine-dependent-options
  gcc-command-options/specifying-subprocesses-and-the-switches-to-pass-to-them
  gcc-command-options/environment-variables-affecting-gcc
  gcc-command-options/using-precompiled-headers
  gcc-command-options/c++-modules

.. only:: man

  .. include:: copyright.rst
