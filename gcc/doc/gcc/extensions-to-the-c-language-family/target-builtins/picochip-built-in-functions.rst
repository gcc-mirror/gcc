..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _picochip-built-in-functions:

picoChip Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides an interface to selected machine instructions from the
picoChip instruction set.

.. function:: int __builtin_sbc (int value)

  Sign bit count.  Return the number of consecutive bits in :samp:`{value}`
  that have the same value as the sign bit.  The result is the number of
  leading sign bits minus one, giving the number of redundant sign bits in
  :samp:`{value}`.

.. function:: int __builtin_byteswap (int value)

  Byte swap.  Return the result of swapping the upper and lower bytes of
  :samp:`{value}`.

.. function:: int __builtin_brev (int value)

  Bit reversal.  Return the result of reversing the bits in
  :samp:`{value}`.  Bit 15 is swapped with bit 0, bit 14 is swapped with bit 1,
  and so on.

.. function:: int __builtin_adds (int x, int y)

  Saturating addition.  Return the result of adding :samp:`{x}` and :samp:`{y}`,
  storing the value 32767 if the result overflows.

.. function:: int __builtin_subs (int x, int y)

  Saturating subtraction.  Return the result of subtracting :samp:`{y}` from
  :samp:`{x}`, storing the value -32768 if the result overflows.

.. function:: void __builtin_halt (void)

  Halt.  The processor stops execution.  This built-in is useful for
  implementing assertions.