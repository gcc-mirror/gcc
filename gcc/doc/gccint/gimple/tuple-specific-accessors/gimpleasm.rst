..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_ASM

GIMPLE_ASM
^^^^^^^^^^

.. function:: gasm *gimple_build_asm_vec ( const char *string, vec<tree, va_gc> *inputs, vec<tree, va_gc> *outputs, vec<tree, va_gc> *clobbers, vec<tree, va_gc> *labels)

  Build a ``GIMPLE_ASM`` statement.  This statement is used for
  building in-line assembly constructs.  ``STRING`` is the assembly
  code.  ``INPUTS``, ``OUTPUTS``, ``CLOBBERS``  and ``LABELS``
  are the inputs, outputs, clobbered registers and labels.

.. function:: unsigned gimple_asm_ninputs (const gasm *g)

  Return the number of input operands for ``GIMPLE_ASM`` ``G``.

.. function:: unsigned gimple_asm_noutputs (const gasm *g)

  Return the number of output operands for ``GIMPLE_ASM`` ``G``.

.. function:: unsigned gimple_asm_nclobbers (const gasm *g)

  Return the number of clobber operands for ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_input_op (const gasm *g, unsigned index)

  Return input operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_input_op (gasm *g, unsigned index, tree in_op)

  Set ``IN_OP`` to be input operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_output_op (const gasm *g, unsigned index)

  Return output operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_output_op (gasm *g, unsigned index, tree out_op)

  Set ``OUT_OP`` to be output operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_clobber_op (const gasm *g, unsigned index)

  Return clobber operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_clobber_op (gasm *g, unsigned index, tree clobber_op)

  Set ``CLOBBER_OP`` to be clobber operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: const char * gimple_asm_string (const gasm *g)

  Return the string representing the assembly instruction in
  ``GIMPLE_ASM`` ``G``.

.. function:: bool gimple_asm_volatile_p (const gasm *g)

  Return true if ``G`` is an asm statement marked volatile.

.. function:: void gimple_asm_set_volatile (gasm *g, bool volatile_p)

  Mark asm statement ``G`` as volatile or non-volatile based on
  ``VOLATILE_P``.