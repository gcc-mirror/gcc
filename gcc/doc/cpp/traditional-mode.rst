..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _traditional-mode:

Traditional Mode
----------------

Traditional (pre-standard) C preprocessing is rather different from
the preprocessing specified by the standard.  When the preprocessor
is invoked with the
:option:`-traditional-cpp` option, it attempts to emulate a traditional
preprocessor.

This mode is not useful for compiling C code with GCC,
but is intended for use with non-C preprocessing applications.  Thus
traditional mode semantics are supported only when invoking
the preprocessor explicitly, and not in the compiler front ends.

The implementation does not correspond precisely to the behavior of
early pre-standard versions of GCC, nor to any true traditional preprocessor.
After all, inconsistencies among traditional implementations were a
major motivation for C standardization.  However, we intend that it
should be compatible with true traditional preprocessors in all ways
that actually matter.

.. toctree::
  :maxdepth: 2

  traditional-lexical-analysis
  traditional-macros
  traditional-miscellany
  traditional-warnings