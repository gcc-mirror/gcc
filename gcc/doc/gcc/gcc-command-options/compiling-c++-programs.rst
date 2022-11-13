..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: suffixes for C++ source, C++ source file suffixes

.. _invoking-g++:

Compiling C++ Programs
**********************

C++ source files conventionally use one of the suffixes :samp:`.C`,
:samp:`.cc`, :samp:`.cpp`, :samp:`.CPP`, :samp:`.c++`, :samp:`.cp`, or
:samp:`.cxx`; C++ header files often use :samp:`.hh`, :samp:`.hpp`,
:samp:`.H`, or (for shared template code) :samp:`.tcc`; and
preprocessed C++ files use the suffix :samp:`.ii`.  GCC recognizes
files with these names and compiles them as C++ programs even if you
call the compiler the same way as for compiling C programs (usually
with the name :command:`gcc`).

.. index:: g++, c++

However, the use of :command:`gcc` does not add the C++ library.
:command:`g++` is a program that calls GCC and automatically specifies linking
against the C++ library.  It treats :samp:`.c`,
:samp:`.h` and :samp:`.i` files as C++ source files instead of C source
files unless :option:`-x` is used.  This program is also useful when
precompiling a C header file with a :samp:`.h` extension for use in C++
compilations.  On many systems, :command:`g++` is also installed with
the name :command:`c++`.

.. index:: invoking g++

When you compile C++ programs, you may specify many of the same
command-line options that you use for compiling programs in any
language; or command-line options meaningful for C and related
languages; or options that are meaningful only for C++ programs.
See :ref:`c-dialect-options`, for
explanations of options for languages related to C.
See :ref:`c++-dialect-options`, for
explanations of options that are meaningful only for C++ programs.