..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: jump instruction patterns, defining jump instruction patterns

.. _jump-patterns:

Defining Jump Instruction Patterns
**********************************

GCC does not assume anything about how the machine realizes jumps.
The machine description should define a single pattern, usually
a ``define_expand``, which expands to all the required insns.

Usually, this would be a comparison insn to set the condition code
and a separate branch insn testing the condition code and branching
or not according to its value.  For many machines, however,
separating compares and branches is limiting, which is why the
more flexible approach with one ``define_expand`` is used in GCC.
The machine description becomes clearer for architectures that
have compare-and-branch instructions but no condition code.  It also
works better when different sets of comparison operators are supported
by different kinds of conditional branches (e.g. integer vs.
floating-point), or by conditional branches with respect to conditional stores.

Two separate insns are always used on most machines that use a separate
condition code register (see :ref:`condition-code`).

Even in this case having a single entry point for conditional branches
is advantageous, because it handles equally well the case where a single
comparison instruction records the results of both signed and unsigned
comparison of the given operands (with the branch insns coming in distinct
signed and unsigned flavors) as in the x86 or SPARC, and the case where
there are distinct signed and unsigned compare instructions and only
one set of conditional branch instructions as in the PowerPC.
