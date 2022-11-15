..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _avr-built-in-functions:

AVR Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

For each built-in function for AVR, there is an equally named,
uppercase built-in macro defined. That way users can easily query if
or if not a specific built-in is implemented or not. For example, if
``__builtin_avr_nop`` is available the macro
``__BUILTIN_AVR_NOP`` is defined to ``1`` and undefined otherwise.

.. code-block:: c++

  void __builtin_avr_nop (void);
  void __builtin_avr_sei (void);
  void __builtin_avr_cli (void);
  void __builtin_avr_sleep (void);
  void __builtin_avr_wdr (void);
  unsigned char __builtin_avr_swap (unsigned char);
  unsigned int __builtin_avr_fmul (unsigned char, unsigned char);
  int __builtin_avr_fmuls (char, char);
  int __builtin_avr_fmulsu (char, unsigned char);

These built-in functions map to the respective machine
instruction, i.e. ``nop``, ``sei``, ``cli``, ``sleep``,
``wdr``, ``swap``, ``fmul``, ``fmuls``
resp. ``fmulsu``. The three ``fmul*`` built-ins are implemented
as library call if no hardware multiplier is available.

.. function:: void __builtin_avr_delay_cycles (unsigned long ticks)

  Delay execution for :samp:`{ticks}` cycles. Note that this
  built-in does not take into account the effect of interrupts that
  might increase delay time. :samp:`{ticks}` must be a compile-time
  integer constant; delays with a variable number of cycles are not supported.

.. function:: char __builtin_avr_flash_segment (const __memx void*)

  This built-in takes a byte address to the 24-bit
  :ref:`avr-named-address-spaces` ``__memx`` and returns
  the number of the flash segment (the 64 KiB chunk) where the address
  points to.  Counting starts at ``0``.
  If the address does not point to flash memory, return ``-1``.

.. function:: uint8_t __builtin_avr_insert_bits (uint32_t map, uint8_t bits, uint8_t val)

  Insert bits from :samp:`{bits}` into :samp:`{val}` and return the resulting
  value. The nibbles of :samp:`{map}` determine how the insertion is
  performed: Let :samp:`{X}` be the :samp:`{n}` -th nibble of :samp:`{map}`

  * If :samp:`{X}` is ``0xf``,
    then the :samp:`{n}` -th bit of :samp:`{val}` is returned unaltered.

  * If X is in the range 0...7,
    then the :samp:`{n}` -th result bit is set to the :samp:`{X}` -th bit of :samp:`{bits}`

  * If X is in the range 8... ``0xe``,
    then the :samp:`{n}` -th result bit is undefined.

  One typical use case for this built-in is adjusting input and
  output values to non-contiguous port layouts. Some examples:

  .. code-block:: c++

    // same as val, bits is unused
    __builtin_avr_insert_bits (0xffffffff, bits, val);

  .. code-block:: c++

    // same as bits, val is unused
    __builtin_avr_insert_bits (0x76543210, bits, val);

  .. code-block:: c++

    // same as rotating bits by 4
    __builtin_avr_insert_bits (0x32107654, bits, 0);

  .. code-block:: c++

    // high nibble of result is the high nibble of val
    // low nibble of result is the low nibble of bits
    __builtin_avr_insert_bits (0xffff3210, bits, val);

  .. code-block:: c++

    // reverse the bit order of bits
    __builtin_avr_insert_bits (0x01234567, bits, 0);

.. function:: void __builtin_avr_nops (unsigned count)

  Insert :samp:`{count}` ``NOP`` instructions.
  The number of instructions must be a compile-time integer constant.

There are many more AVR-specific built-in functions that are used to
implement the ISO/IEC TR 18037 'Embedded C' fixed-point functions of
section 7.18a.6.  You don't need to use these built-ins directly.
Instead, use the declarations as supplied by the ``stdfix.h`` header
with GNU-C99:

.. code-block:: c++

  #include <stdfix.h>

  // Re-interpret the bit representation of unsigned 16-bit
  // integer uval as Q-format 0.16 value.
  unsigned fract get_bits (uint_ur_t uval)
  {
      return urbits (uval);
  }
