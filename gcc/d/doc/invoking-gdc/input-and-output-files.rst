..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: suffixes for D source, D source file suffixes

.. _input-and-output-files:

Input and Output files
**********************

For any given input file, the file name suffix determines what kind of
compilation is done.  The following kinds of input file names are supported:

:samp:`{file}.d`
  D source files.

:samp:`{file}.dd`
  Ddoc source files.

:samp:`{file}.di`
  D interface files.

You can specify more than one input file on the :command:`gdc` command line,
each being compiled separately in the compilation process.  If you specify a
``-o file`` option, all the input files are compiled together,
producing a single output file, named :samp:`{file}`.  This is allowed even
when using ``-S`` or ``-c``.

.. index:: D interface files.

A D interface file contains only what an import of the module needs,
rather than the whole implementation of that module.  They can be created
by :command:`gdc` from a D source file by using the ``-H`` option.
When the compiler resolves an import declaration, it searches for matching
:samp:`.di` files first, then for :samp:`.d`.

.. index:: Ddoc source files.

A Ddoc source file contains code in the D macro processor language.  It is
primarily designed for use in producing user documentation from embedded
comments, with a slight affinity towards HTML generation.  If a :samp:`.d`
source file starts with the string ``Ddoc`` then it is treated as general
purpose documentation, not as a D source file.