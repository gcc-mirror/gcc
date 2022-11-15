..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: implementation-defined behavior, C language

.. _c-implementation:

C Implementation-Defined Behavior
---------------------------------

A conforming implementation of ISO C is required to document its
choice of behavior in each of the areas that are designated
'implementation defined'.  The following lists all such areas,
along with the section numbers from the ISO/IEC 9899:1990, ISO/IEC
9899:1999 and ISO/IEC 9899:2011 standards.  Some areas are only
implementation-defined in one version of the standard.

Some choices depend on the externally determined ABI for the platform
(including standard character encodings) which GCC follows; these are
listed as 'determined by ABI' below.  See :ref:`compatibility`, and https://gcc.gnu.org/readings.html.  Some
choices are documented in the preprocessor manual.
See :ref:`cpp:implementation-defined-behavior`.  Some choices are made by the
library and operating system (or other environment when compiling for
a freestanding environment); refer to their documentation for details.

.. toctree::
  :maxdepth: 2

  c-implementation-defined-behavior/translation
  c-implementation-defined-behavior/environment
  c-implementation-defined-behavior/identifiers
  c-implementation-defined-behavior/characters
  c-implementation-defined-behavior/integers
  c-implementation-defined-behavior/floating-point
  c-implementation-defined-behavior/arrays-and-pointers
  c-implementation-defined-behavior/hints
  c-implementation-defined-behavior/structures-unions-enumerations-and-bit-fields
  c-implementation-defined-behavior/qualifiers
  c-implementation-defined-behavior/declarators
  c-implementation-defined-behavior/statements
  c-implementation-defined-behavior/preprocessing-directives
  c-implementation-defined-behavior/library-functions
  c-implementation-defined-behavior/architecture
  c-implementation-defined-behavior/locale-specific-behavior
