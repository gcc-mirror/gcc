..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _invoking-gdc:

Invoking gdc
------------

.. only:: man

  Synopsis
  ^^^^^^^^

  gdc [ :option:`-c` | :option:`-S` ] [ :option:`-g` ] [ :option:`-pg` ]
      [ :option:`-O`:samp:`{level}` ] [ :option:`-W`:samp:`{warn}`...]
      [ :option:`-I`:samp:`{dir}`...] [ :option:`-L`:samp:`{dir}`...]
      [ :option:`-f`:samp:`{option}`...] [ :option:`-m`:samp:`{machine-option}`...]
      [ :option:`-o` :samp:`{outfile}` ] [@ :samp:`{file}` ] :samp:`{infile}`...

  Only the most useful options are listed here; see below for the
  remainder.

Description
^^^^^^^^^^^

The :command:`gdc` command is the GNU compiler for the D language and
supports many of the same options as :command:`gcc`.  See :ref:`gcc:option-summary`.
This manual only documents the options specific to :command:`gdc`.

Options
^^^^^^^

.. toctree::
  :maxdepth: 2

  invoking-gdc/input-and-output-files
  invoking-gdc/runtime-options
  invoking-gdc/options-for-directory-search
  invoking-gdc/code-generation
  invoking-gdc/warnings
  invoking-gdc/options-for-linking
  invoking-gdc/developer-options

.. only:: man

  .. include:: copyright.rst