..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _rx-built-in-functions:

RX Built-in Functions
^^^^^^^^^^^^^^^^^^^^^

GCC supports some of the RX instructions which cannot be expressed in
the C programming language via the use of built-in functions.  The
following functions are supported:

.. function:: void __builtin_rx_brk (void)

  Generates the ``brk`` machine instruction.

.. function:: void __builtin_rx_clrpsw (int)

  Generates the ``clrpsw`` machine instruction to clear the specified
  bit in the processor status word.

.. function:: void __builtin_rx_int (int)

  Generates the ``int`` machine instruction to generate an interrupt
  with the specified value.

.. function:: void __builtin_rx_machi (int, int)

  Generates the ``machi`` machine instruction to add the result of
  multiplying the top 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_maclo (int, int)

  Generates the ``maclo`` machine instruction to add the result of
  multiplying the bottom 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_mulhi (int, int)

  Generates the ``mulhi`` machine instruction to place the result of
  multiplying the top 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_mullo (int, int)

  Generates the ``mullo`` machine instruction to place the result of
  multiplying the bottom 16 bits of the two arguments into the
  accumulator.

.. function:: int  __builtin_rx_mvfachi (void)

  Generates the ``mvfachi`` machine instruction to read the top
  32 bits of the accumulator.

.. function:: int  __builtin_rx_mvfacmi (void)

  Generates the ``mvfacmi`` machine instruction to read the middle
  32 bits of the accumulator.

.. function:: int __builtin_rx_mvfc (int)

  Generates the ``mvfc`` machine instruction which reads the control
  register specified in its argument and returns its value.

.. function:: void __builtin_rx_mvtachi (int)

  Generates the ``mvtachi`` machine instruction to set the top
  32 bits of the accumulator.

.. function:: void __builtin_rx_mvtaclo (int)

  Generates the ``mvtaclo`` machine instruction to set the bottom
  32 bits of the accumulator.

.. function:: void __builtin_rx_mvtc (int reg, int val)

  Generates the ``mvtc`` machine instruction which sets control
  register number ``reg`` to ``val``.

.. function:: void __builtin_rx_mvtipl (int)

  Generates the ``mvtipl`` machine instruction set the interrupt
  priority level.

.. function:: void __builtin_rx_racw (int)

  Generates the ``racw`` machine instruction to round the accumulator
  according to the specified mode.

.. function:: int __builtin_rx_revw (int)

  Generates the ``revw`` machine instruction which swaps the bytes in
  the argument so that bits 0--7 now occupy bits 8--15 and vice versa,
  and also bits 16--23 occupy bits 24--31 and vice versa.

.. function:: void __builtin_rx_rmpa (void)

  Generates the ``rmpa`` machine instruction which initiates a
  repeated multiply and accumulate sequence.

.. function:: void __builtin_rx_round (float)

  Generates the ``round`` machine instruction which returns the
  floating-point argument rounded according to the current rounding mode
  set in the floating-point status word register.

.. function:: int __builtin_rx_sat (int)

  Generates the ``sat`` machine instruction which returns the
  saturated value of the argument.

.. function:: void __builtin_rx_setpsw (int)

  Generates the ``setpsw`` machine instruction to set the specified
  bit in the processor status word.

.. function:: void __builtin_rx_wait (void)

  Generates the ``wait`` machine instruction.