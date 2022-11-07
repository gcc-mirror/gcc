..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: PDP-11

.. index:: PDP-11 Options

.. _pdp-11-options:

PDP-11 Options
^^^^^^^^^^^^^^

These options are defined for the PDP-11:

.. option:: -mfpu

  Use hardware FPP floating point.  This is the default.  (FIS floating
  point on the PDP-11/40 is not supported.)  Implies :option:`-m45`.

.. option:: -msoft-float

  Do not use hardware floating point.

.. option:: -mac0

  Return floating-point results in ac0 (fr0 in Unix assembler syntax).

.. option:: -mno-ac0

  Return floating-point results in memory.  This is the default.

.. option:: -m40

  Generate code for a PDP-11/40.  Implies -msoft-float -mno-split.

.. option:: -m45

  Generate code for a PDP-11/45.  This is the default.

.. option:: -m10

  Generate code for a PDP-11/10.  Implies -msoft-float -mno-split.

.. option:: -mint16, -mno-int32

  Use 16-bit ``int``.  This is the default.

.. option:: -mint32, -mno-int16

  Use 32-bit ``int``.

.. option:: -msplit

  Target has split instruction and data space.  Implies -m45.

.. option:: -munix-asm

  Use Unix assembler syntax.

.. option:: -mdec-asm

  Use DEC assembler syntax.

.. option:: -mgnu-asm

  Use GNU assembler syntax.  This is the default.

.. option:: -mlra

  Use the new LRA register allocator.  By default, the old 'reload'
  allocator is used.