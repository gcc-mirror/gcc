..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: implementation-defined behavior, C++ language

.. _c++-implementation:

C++ Implementation-Defined Behavior
-----------------------------------

A conforming implementation of ISO C++ is required to document its
choice of behavior in each of the areas that are designated
'implementation defined'.  The following lists all such areas,
along with the section numbers from the ISO/IEC 14882:1998 and ISO/IEC
14882:2003 standards.  Some areas are only implementation-defined in
one version of the standard.

Some choices depend on the externally determined ABI for the platform
(including standard character encodings) which GCC follows; these are
listed as 'determined by ABI' below.  See :ref:`compatibility`, and https://gcc.gnu.org/readings.html.  Some
choices are documented in the preprocessor manual.
See :ref:`cpp:implementation-defined-behavior`.  Some choices are documented in
the corresponding document for the C language.  See :ref:`c-implementation`.  Some choices are made by the library and operating
system (or other environment when compiling for a freestanding
environment); refer to their documentation for details.

.. toctree::
  :maxdepth: 2

  conditionally-supported-behavior
  exception-handling
