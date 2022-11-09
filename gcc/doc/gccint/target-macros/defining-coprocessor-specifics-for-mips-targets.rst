..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: MIPS coprocessor-definition macros

.. _mips-coprocessors:

Defining coprocessor specifics for MIPS targets.
************************************************

The MIPS specification allows MIPS implementations to have as many as 4
coprocessors, each with as many as 32 private registers.  GCC supports
accessing these registers and transferring values between the registers
and memory using asm-ized variables.  For example:

.. code-block:: c++

    register unsigned int cp0count asm ("c0r1");
    unsigned int d;

    d = cp0count + 3;

('c0r1' is the default name of register 1 in coprocessor 0; alternate
names may be added as described below, or the default names may be
overridden entirely in ``SUBTARGET_CONDITIONAL_REGISTER_USAGE``.)

Coprocessor registers are assumed to be epilogue-used; sets to them will
be preserved even if it does not appear that the register is used again
later in the function.

Another note: according to the MIPS spec, coprocessor 1 (if present) is
the FPU.  One accesses COP1 registers through standard mips
floating-point support; they are not included in this mechanism.
