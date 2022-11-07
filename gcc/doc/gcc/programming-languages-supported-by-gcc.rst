..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GCC, GNU Compiler Collection, GNU C Compiler, Ada, D, Fortran, Go, Objective-C, Objective-C++

.. _g++-and-gcc:

Programming Languages Supported by GCC
--------------------------------------

GCC stands for 'GNU Compiler Collection'.  GCC is an integrated
distribution of compilers for several major programming languages.  These
languages currently include C, C++, Objective-C, Objective-C++,
Fortran, Ada, D, and Go.

The abbreviation :dfn:`GCC` has multiple meanings in common use.  The
current official meaning is 'GNU Compiler Collection', which refers
generically to the complete suite of tools.  The name historically stood
for 'GNU C Compiler', and this usage is still common when the emphasis
is on compiling C programs.  Finally, the name is also used when speaking
of the :dfn:`language-independent` component of GCC: code shared among the
compilers for all supported languages.

The language-independent component of GCC includes the majority of the
optimizers, as well as the 'back ends' that generate machine code for
various processors.

.. index:: COBOL, Mercury

The part of a compiler that is specific to a particular language is
called the 'front end'.  In addition to the front ends that are
integrated components of GCC, there are several other front ends that
are maintained separately.  These support languages such as
Mercury, and COBOL.  To use these, they must be built together with
GCC proper.

.. index:: C++, G++, Ada, GNAT

Most of the compilers for languages other than C have their own names.
The C++ compiler is G++, the Ada compiler is GNAT, and so on.  When we
talk about compiling one of those languages, we might refer to that
compiler by its own name, or as GCC.  Either is correct.

.. index:: compiler compared to C++ preprocessor, intermediate C version, nonexistent, C intermediate output, nonexistent

Historically, compilers for many languages, including C++ and Fortran,
have been implemented as 'preprocessors' which emit another high
level language such as C.  None of the compilers included in GCC are
implemented this way; they all generate machine code directly.  This
sort of preprocessor should not be confused with the :dfn:`C
preprocessor`, which is an integral feature of the C, C++, Objective-C
and Objective-C++ languages.