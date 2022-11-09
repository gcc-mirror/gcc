..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: M32C

.. index:: M32C options

.. _m32c-options:

M32C Options
^^^^^^^^^^^^

.. option:: -mcpu={name}

  Select the CPU for which code is generated.  :samp:`{name}` may be one of
  :samp:`r8c` for the R8C/Tiny series, :samp:`m16c` for the M16C (up to
  /60) series, :samp:`m32cm` for the M16C/80 series, or :samp:`m32c` for
  the M32C/80 series.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  an alternate runtime library to be linked in which supports, for
  example, file I/O.  You must not use this option when generating
  programs that will run on real hardware; you must provide your own
  runtime library for whatever I/O functions are needed.

.. option:: -memregs={number}

  Specifies the number of memory-based pseudo-registers GCC uses
  during code generation.  These pseudo-registers are used like real
  registers, so there is a tradeoff between GCC's ability to fit the
  code into available registers, and the performance penalty of using
  memory instead of registers.  Note that all modules in a program must
  be compiled with the same value for this option.  Because of that, you
  must not use this option with GCC's default runtime libraries.
