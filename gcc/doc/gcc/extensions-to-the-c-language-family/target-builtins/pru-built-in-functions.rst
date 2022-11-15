..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _pru-built-in-functions:

PRU Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

GCC provides a couple of special builtin functions to aid in utilizing
special PRU instructions.

The built-in functions supported are:

.. function:: __delay_cycles (long long cycles)

  This inserts an instruction sequence that takes exactly :samp:`{cycles}`
  cycles (between 0 and 0xffffffff) to complete.  The inserted sequence
  may use jumps, loops, or no-ops, and does not interfere with any other
  instructions.  Note that :samp:`{cycles}` must be a compile-time constant
  integer - that is, you must pass a number, not a variable that may be
  optimized to a constant later.  The number of cycles delayed by this
  builtin is exact.

.. function:: __halt (void)

  This inserts a HALT instruction to stop processor execution.

.. function:: unsigned int __lmbd (unsigned int wordval, unsigned int bitval)

  This inserts LMBD instruction to calculate the left-most bit with value
  :samp:`{bitval}` in value :samp:`{wordval}`.  Only the least significant bit
  of :samp:`{bitval}` is taken into account.
