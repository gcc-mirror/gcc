..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _blackfin-built-in-functions:

Blackfin Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, there are two Blackfin-specific built-in functions.  These are
used for generating ``CSYNC`` and ``SSYNC`` machine insns without
using inline assembly; by using these built-in functions the compiler can
automatically add workarounds for hardware errata involving these
instructions.  These functions are named as follows:

.. code-block:: c++

  void __builtin_bfin_csync (void);
  void __builtin_bfin_ssync (void);
