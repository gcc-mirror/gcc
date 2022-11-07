..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: NDS32

.. index:: NDS32 Options

.. _nds32-options:

NDS32 Options
^^^^^^^^^^^^^

These options are defined for NDS32 implementations:

.. option:: -mbig-endian

  Generate code in big-endian mode.

.. option:: -mlittle-endian

  Generate code in little-endian mode.

.. option:: -mreduced-regs

  Use reduced-set registers for register allocation.

.. option:: -mfull-regs

  Use full-set registers for register allocation.

.. option:: -mcmov

  Generate conditional move instructions.

.. option:: -mno-cmov

  Do not generate conditional move instructions.

.. option:: -mext-perf

  Generate performance extension instructions.

.. option:: -mno-ext-perf

  Do not generate performance extension instructions.

.. option:: -mext-perf2

  Generate performance extension 2 instructions.

.. option:: -mno-ext-perf2

  Do not generate performance extension 2 instructions.

.. option:: -mext-string

  Generate string extension instructions.

.. option:: -mno-ext-string

  Do not generate string extension instructions.

.. option:: -mv3push

  Generate v3 push25/pop25 instructions.

.. option:: -mno-v3push

  Do not generate v3 push25/pop25 instructions.

.. option:: -m16-bit

  Generate 16-bit instructions.

.. option:: -mno-16-bit

  Do not generate 16-bit instructions.

.. option:: -misr-vector-size={num}

  Specify the size of each interrupt vector, which must be 4 or 16.

.. option:: -mcache-block-size={num}

  Specify the size of each cache block,
  which must be a power of 2 between 4 and 512.

.. option:: -march={arch}

  Specify the name of the target architecture.

.. option:: -mcmodel={code-model}

  Set the code model to one of

  small
    All the data and read-only data segments must be within 512KB addressing space.
    The text segment must be within 16MB addressing space.

  medium
    The data segment must be within 512KB while the read-only data segment can be
    within 4GB addressing space.  The text segment should be still within 16MB
    addressing space.

  large
    All the text and data segments can be within 4GB addressing space.

.. option:: -mctor-dtor

  Enable constructor/destructor feature.

.. option:: -mrelax

  Guide linker to relax instructions.