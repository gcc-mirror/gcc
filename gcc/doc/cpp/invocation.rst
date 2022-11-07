..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: invocation, command line

.. _invocation:

Invocation
----------

Most often when you use the C preprocessor you do not have to invoke it
explicitly: the C compiler does so automatically.  However, the
preprocessor is sometimes useful on its own.  You can invoke the
preprocessor either with the :command:`cpp` command, or via :command:`gcc -E`.
In GCC, the preprocessor is actually integrated with the compiler
rather than a separate program, and both of these commands invoke
GCC and tell it to stop after the preprocessing phase.

The :command:`cpp` options listed here are also accepted by
:command:`gcc` and have the same meaning.  Likewise the :command:`cpp`
command accepts all the usual :command:`gcc` driver options, although those
pertaining to compilation phases after preprocessing are ignored.

Only options specific to preprocessing behavior are documented here.
Refer to the GCC manual for full documentation of other driver options.

.. only:: man

  Synopsis
  ^^^^^^^^

  cpp [ :option:`-D`:samp:`{macro}` [= :samp:`{defn}` ]...] [ :option:`-U`:samp:`{macro}` ]
      [ :option:`-I`:samp:`{dir}`...] [ :option:`-iquote`:samp:`{dir}`...]
      [ :option:`-M` | :option:`-MM` ] [ :option:`-MG` ] [ :option:`-MF` :samp:`{filename}` ]
      [ :option:`-MP` ] [ :option:`-MQ` :samp:`{target}`...]
      [ :option:`-MT` :samp:`{target}`...]
      :samp:`{infile}` [[ :option:`-o` ] :samp:`{outfile}` ]

  Only the most useful options are given above; see below for a more
  complete list of preprocessor-specific options.
  In addition, :command:`cpp` accepts most :command:`gcc` driver options, which
  are not listed here.  Refer to the GCC documentation for details.

Options
^^^^^^^

The :command:`cpp` command expects two file names as arguments, :samp:`{infile}` and
:samp:`{outfile}`.  The preprocessor reads :samp:`{infile}` together with any
other files it specifies with :samp:`#include`.  All the output generated
by the combined input files is written in :samp:`{outfile}`.

Either :samp:`{infile}` or :samp:`{outfile}` may be :option:`-`, which as
:samp:`{infile}` means to read from standard input and as :samp:`{outfile}`
means to write to standard output.  If either file is omitted, it
means the same as if :option:`-` had been specified for that file.
You can also use the :option:`-o outfile` option to specify the
output file.

Unless otherwise noted, or the option ends in :samp:`=`, all options
which take an argument may have that argument appear either immediately
after the option, or with a space between option and argument:
:option:`-Ifoo` and :option:`-I foo` have the same effect.

.. index:: grouping options, options, grouping

Many options have multi-letter names; therefore multiple single-letter
options may *not* be grouped: :option:`-dM` is very different from
:samp:`-d -M`.

.. index:: options

.. include:: ../../../doc/cppopts.rst


.. include:: ../../../doc/cppdiropts.rst

.. only:: man

  .. include:: copyright.rst