..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _translating-to-generic:

Generating the intermediate language for later stages.
------------------------------------------------------

This chapter deals with the transformation of gfortran's frontend data
structures to the intermediate language used by the later stages of
the compiler, the so-called middle end.

Data structures relating to this are found in the source files
:samp:`trans*.h` and :samp:`trans-*.c`.

.. toctree::
  :maxdepth: 2

  generating-the-intermediate-language-for-later-stages/basic-data-structures
  generating-the-intermediate-language-for-later-stages/converting-expressions-to-tree
  generating-the-intermediate-language-for-later-stages/translating-statements
  generating-the-intermediate-language-for-later-stages/accessing-declarations
