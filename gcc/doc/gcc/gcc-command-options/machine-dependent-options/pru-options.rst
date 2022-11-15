..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: PRU

.. index:: PRU Options

.. _pru-options:

PRU Options
^^^^^^^^^^^

These command-line options are defined for PRU target:

.. option:: -minrt

  Link with a minimum runtime environment, with no support for static
  initializers and constructors.  Using this option can significantly reduce
  the size of the final ELF binary.  Beware that the compiler could still
  generate code with static initializers and constructors.  It is up to the
  programmer to ensure that the source program will not use those features.

.. option:: -mmcu={mcu}

  Specify the PRU MCU variant to use.  Check Newlib for the exact list of
  supported MCUs.

.. option:: -mno-relax

  Make GCC pass the :option:`--no-relax` command-line option to the linker
  instead of the :option:`--relax` option.

.. option:: -mloop

  Allow (or do not allow) GCC to use the LOOP instruction.

.. option:: -mabi={variant}

  Specify the ABI variant to output code for.  :option:`-mabi=ti` selects the
  unmodified TI ABI while :option:`-mabi=gnu` selects a GNU variant that copes
  more naturally with certain GCC assumptions.  These are the differences:

  :samp:`Function Pointer Size`
    TI ABI specifies that function (code) pointers are 16-bit, whereas GNU
    supports only 32-bit data and code pointers.

  :samp:`Optional Return Value Pointer`
    Function return values larger than 64 bits are passed by using a hidden
    pointer as the first argument of the function.  TI ABI, though, mandates that
    the pointer can be NULL in case the caller is not using the returned value.
    GNU always passes and expects a valid return value pointer.

  The current :option:`-mabi=ti` implementation simply raises a compile error
  when any of the above code constructs is detected.  As a consequence
  the standard C library cannot be built and it is omitted when linking with
  :option:`-mabi=ti`.

  Relaxation is a GNU feature and for safety reasons is disabled when using
  :option:`-mabi=ti`.  The TI toolchain does not emit relocations for QBBx
  instructions, so the GNU linker cannot adjust them when shortening adjacent
  LDI32 pseudo instructions.
