..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _msp430-built-in-functions:

MSP430 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides a couple of special builtin functions to aid in the
writing of interrupt handlers in C.

.. function:: __bic_SR_register_on_exit (int mask)

  This clears the indicated bits in the saved copy of the status register
  currently residing on the stack.  This only works inside interrupt
  handlers and the changes to the status register will only take affect
  once the handler returns.

.. function:: __bis_SR_register_on_exit (int mask)

  This sets the indicated bits in the saved copy of the status register
  currently residing on the stack.  This only works inside interrupt
  handlers and the changes to the status register will only take affect
  once the handler returns.

.. function:: __delay_cycles (long long cycles)

  This inserts an instruction sequence that takes exactly :samp:`{cycles}`
  cycles (between 0 and about 17E9) to complete.  The inserted sequence
  may use jumps, loops, or no-ops, and does not interfere with any other
  instructions.  Note that :samp:`{cycles}` must be a compile-time constant
  integer - that is, you must pass a number, not a variable that may be
  optimized to a constant later.  The number of cycles delayed by this
  builtin is exact.