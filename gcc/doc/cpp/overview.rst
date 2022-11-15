..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _top:

.. _overview:

Overview
--------

The C preprocessor, often known as :dfn:`cpp`, is a :dfn:`macro processor`
that is used automatically by the C compiler to transform your program
before compilation.  It is called a macro processor because it allows
you to define :dfn:`macros`, which are brief abbreviations for longer
constructs.

The C preprocessor is intended to be used only with C, C++, and
Objective-C source code.  In the past, it has been abused as a general
text processor.  It will choke on input which does not obey C's lexical
rules.  For example, apostrophes will be interpreted as the beginning of
character constants, and cause errors.  Also, you cannot rely on it
preserving characteristics of the input which are not significant to
C-family languages.  If a Makefile is preprocessed, all the hard tabs
will be removed, and the Makefile will not work.

Having said that, you can often get away with using cpp on things which
are not C.  Other Algol-ish programming languages are often safe
(Ada, etc.) So is assembly, with caution.  :option:`-traditional-cpp`
mode preserves more white space, and is otherwise more permissive.  Many
of the problems can be avoided by writing C or C++ style comments
instead of native language comments, and keeping macros simple.

Wherever possible, you should use a preprocessor geared to the language
you are writing in.  Modern versions of the GNU assembler have macro
facilities.  Most high level programming languages have their own
conditional compilation and inclusion mechanism.  If all else fails,
try a true general text processor, such as GNU M4.

C preprocessors vary in some details.  This manual discusses the GNU C
preprocessor, which provides a small superset of the features of ISO
Standard C.  In its default mode, the GNU C preprocessor does not do a
few things required by the standard.  These are features which are
rarely, if ever, used, and may cause surprising changes to the meaning
of a program which does not expect them.  To get strict ISO Standard C,
you should use the :option:`-std=c90`, :option:`-std=c99`,
:option:`-std=c11` or :option:`-std=c17` options, depending
on which version of the standard you want.  To get all the mandatory
diagnostics, you must also use :option:`-pedantic`.  See :ref:`invocation`.

This manual describes the behavior of the ISO preprocessor.  To
minimize gratuitous differences, where the ISO preprocessor's
behavior does not conflict with traditional semantics, the
traditional preprocessor should behave the same way.  The various
differences that do exist are detailed in the section :ref:`traditional-mode`.

For clarity, unless noted otherwise, references to :samp:`CPP` in this
manual refer to GNU CPP.

.. toctree::
  :maxdepth: 2

  character-sets
  initial-processing
  tokenization
  the-preprocessing-language
