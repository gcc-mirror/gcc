..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: assembler instructions in RTL, asm_operands, usage

.. _assembler:

Assembler Instructions as Expressions
*************************************

The RTX code ``asm_operands`` represents a value produced by a
user-specified assembler instruction.  It is used to represent
an ``asm`` statement with arguments.  An ``asm`` statement with
a single output operand, like this:

.. code-block:: c++

  asm ("foo %1,%2,%0" : "=a" (outputvar) : "g" (x + y), "di" (*z));

is represented using a single ``asm_operands`` RTX which represents
the value that is stored in ``outputvar`` :

.. code-block:: c++

  (set rtx-for-outputvar
       (asm_operands "foo %1,%2,%0" "a" 0
                     [rtx-for-addition-result rtx-for-*z]
                     [(asm_input:m1 "g")
                      (asm_input:m2 "di")]))

Here the operands of the ``asm_operands`` RTX are the assembler
template string, the output-operand's constraint, the index-number of the
output operand among the output operands specified, a vector of input
operand RTX's, and a vector of input-operand modes and constraints.  The
mode :samp:`{m1}` is the mode of the sum ``x+y`` ; :samp:`{m2}` is that of
``*z``.

When an ``asm`` statement has multiple output values, its insn has
several such ``set`` RTX's inside of a ``parallel``.  Each ``set``
contains an ``asm_operands`` ; all of these share the same assembler
template and vectors, but each contains the constraint for the respective
output operand.  They are also distinguished by the output-operand index
number, which is 0, 1, ... for successive output operands.