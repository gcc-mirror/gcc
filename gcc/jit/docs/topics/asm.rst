.. Copyright (C) 2020-2023 Free Software Foundation, Inc.
   Originally contributed by David Malcolm <dmalcolm@redhat.com>

   This is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see
   <https://www.gnu.org/licenses/>.

.. default-domain:: c

Using Assembly Language with libgccjit
======================================

libgccjit has some support for directly embedding assembler instructions.
This is based on GCC's support for inline ``asm`` in C code, and the
following assumes a familiarity with that functionality.  See
`How to Use Inline Assembly Language in C Code <https://gcc.gnu.org/onlinedocs/gcc/Using-Assembly-Language-with-C.html>`_
in GCC's documentation, the "Extended Asm" section in particular.

These entrypoints were added in :ref:`LIBGCCJIT_ABI_15`; you can test
for their presence using

   .. code-block:: c

      #ifdef LIBGCCJIT_HAVE_ASM_STATEMENTS

Adding assembler instructions within a function
***********************************************

.. type:: gcc_jit_extended_asm

   A `gcc_jit_extended_asm` represents an extended ``asm`` statement: a
   series of low-level instructions inside a function that convert inputs
   to outputs.

   To avoid having an API entrypoint with a very large number of
   parameters, an extended ``asm`` statement is made in stages:
   an initial call to create the :type:`gcc_jit_extended_asm`,
   followed by calls to add operands and set other properties of the
   statement.

   There are two API entrypoints for creating a :type:`gcc_jit_extended_asm`:

   * :func:`gcc_jit_block_add_extended_asm` for an ``asm`` statement with
     no control flow, and

   * :func:`gcc_jit_block_end_with_extended_asm_goto` for an ``asm goto``.

   For example, to create the equivalent of:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: // Quote from here in docs/topics/asm.rst: example 1: C
      :end-before: // Quote up to here in docs/topics/asm.rst: example 1: C
      :language: c

   the following API calls could be used:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: /* Quote from here in docs/topics/asm.rst: example 1: jit.  */
      :end-before: /* Quote up to here in docs/topics/asm.rst: example 1: jit.  */
      :language: c

   .. warning::  When considering the numbering of operands within an
		 extended ``asm`` statement (e.g. the ``%0`` and ``%1``
		 above), the equivalent to the C syntax is followed i.e. all
		 output operands, then all input operands, regardless of
		 what order the calls to
		 :func:`gcc_jit_extended_asm_add_output_operand` and
		 :func:`gcc_jit_extended_asm_add_input_operand` were made in.

   As in the C syntax, operands can be given symbolic names to avoid having
   to number them.  For example, to create the equivalent of:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: // Quote from here in docs/topics/asm.rst: example 2: C
      :end-before: // Quote up to here in docs/topics/asm.rst: example 2: C
      :language: c

   the following API calls could be used:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: /* Quote from here in docs/topics/asm.rst: example 2: jit.  */
      :end-before: /* Quote up to here in docs/topics/asm.rst: example 2: jit.  */
      :language: c

.. function:: gcc_jit_extended_asm *\
              gcc_jit_block_add_extended_asm (gcc_jit_block *block,\
                                              gcc_jit_location *loc,\
                                              const char *asm_template)

   Create a :type:`gcc_jit_extended_asm` for an extended ``asm`` statement
   with no control flow (i.e. without the ``goto`` qualifier).

   The parameter ``asm_template`` corresponds to the `AssemblerTemplate`
   within C's extended ``asm`` syntax.  It must be non-NULL.  The call takes
   a copy of the underlying string, so it is valid to pass in a pointer to
   an on-stack buffer.

.. function:: gcc_jit_extended_asm *\
              gcc_jit_block_end_with_extended_asm_goto (gcc_jit_block *block,\
                                                        gcc_jit_location *loc,\
                                                        const char *asm_template,\
                                                        int num_goto_blocks,\
                                                        gcc_jit_block **goto_blocks,\
                                                        gcc_jit_block *fallthrough_block)

   Create a :type:`gcc_jit_extended_asm` for an extended ``asm`` statement
   that may perform jumps, and use it to terminate the given block.
   This is equivalent to the ``goto`` qualifier in C's extended ``asm``
   syntax.

   For example, to create the equivalent of:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: // Quote from here in docs/topics/asm.rst: example 3b: C
      :end-before: // Quote up to here in docs/topics/asm.rst: example 3b: C
      :language: c

   the following API calls could be used:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: /* Quote from here in docs/topics/asm.rst: example 3: jit.  */
      :end-before: /* Quote up to here in docs/topics/asm.rst: example 3: jit.  */
      :language: c

   here referencing a :type:`gcc_jit_block` named "carry".

   ``num_goto_blocks`` must be >= 0.

   ``goto_blocks`` must be non-NULL.  This corresponds to the ``GotoLabels``
   parameter within C's extended ``asm`` syntax.  The block names can be
   referenced within the assembler template.

   ``fallthrough_block`` can be NULL.  If non-NULL, it specifies the block
   to fall through to after the statement.

   .. note:: This is needed since each :type:`gcc_jit_block` must have a
	     single exit point, as a basic block: you can't jump from the
	     middle of a block.  A "goto" is implicitly added after the
	     asm to handle the fallthrough case, which is equivalent to what
	     would have happened in the C case.

.. function:: void\
              gcc_jit_extended_asm_set_volatile_flag (gcc_jit_extended_asm *ext_asm,\
                                                      int flag)

   Set whether the :type:`gcc_jit_extended_asm` has side-effects, equivalent to the
   `volatile <https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#Volatile>`_
   qualifier in C's extended asm syntax.

   For example, to create the equivalent of:

   .. code-block:: c

      asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
                     "shl $32, %%rdx\n\t"  // Shift the upper bits left.
                     "or %%rdx, %0"        // 'Or' in the lower bits.
                     : "=a" (msr)
                     :
                     : "rdx");

   the following API calls could be used:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: /* Quote from here in docs/topics/asm.rst: example 4: jit.  */
      :end-before: /* Quote up to here in docs/topics/asm.rst: example 4: jit.  */
      :language: c

   where the :type:`gcc_jit_extended_asm` is flagged as volatile.

.. function:: void\
              gcc_jit_extended_asm_set_inline_flag (gcc_jit_extended_asm *ext_asm,\
                                                    int flag)

   Set the equivalent of the
   `inline <https://gcc.gnu.org/onlinedocs/gcc/Size-of-an-asm.html#Size-of-an-asm>`_
   qualifier in C's extended ``asm`` syntax.

.. function:: void\
              gcc_jit_extended_asm_add_output_operand (gcc_jit_extended_asm *ext_asm,\
                                                       const char *asm_symbolic_name,\
                                                       const char *constraint,\
                                                       gcc_jit_lvalue *dest)

   Add an output operand to the extended ``asm`` statement.  See the
   `Output Operands <https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#OutputOperands>`_
   section of the documentation of the C syntax.

   ``asm_symbolic_name`` corresponds to the ``asmSymbolicName`` component of C's
   extended ``asm`` syntax.  It can be NULL.  If non-NULL it specifies the
   symbolic name for the operand.

   ``constraint`` corresponds to the ``constraint`` component of C's extended
   ``asm`` syntax.  It must be non-NULL.

   ``dest`` corresponds to the ``cvariablename`` component of C's extended
   ``asm`` syntax.  It must be non-NULL.

   .. code-block:: c

      // Example with a NULL symbolic name, the equivalent of:
      //   : "=r" (dst)
      gcc_jit_extended_asm_add_output_operand (ext_asm, NULL, "=r", dst);

      // Example with a symbolic name ("aIndex"), the equivalent of:
      //   : [aIndex] "=r" (index)
      gcc_jit_extended_asm_add_output_operand (ext_asm, "aIndex", "=r", index);

   This function can't be called on an ``asm goto`` as such instructions can't
   have outputs; see the
   `Goto Labels <https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#GotoLabels>`_
   section of GCC's "Extended Asm" documentation.

.. function:: void\
              gcc_jit_extended_asm_add_input_operand (gcc_jit_extended_asm *ext_asm,\
                                                      const char *asm_symbolic_name,\
                                                      const char *constraint,\
                                                      gcc_jit_rvalue *src)

   Add an input operand to the extended ``asm`` statement.  See the
   `Input Operands <https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#InputOperands>`_
   section of the documentation of the C syntax.

   ``asm_symbolic_name`` corresponds to the ``asmSymbolicName`` component of C's
   extended ``asm`` syntax.  It can be NULL.  If non-NULL it specifies the
   symbolic name for the operand.

   ``constraint`` corresponds to the ``constraint`` component of C's extended
   ``asm`` syntax.  It must be non-NULL.

   ``src`` corresponds to the ``cexpression`` component of C's extended
   ``asm`` syntax.  It must be non-NULL.

   .. code-block:: c

      // Example with a NULL symbolic name, the equivalent of:
      //   : "r" (src)
      gcc_jit_extended_asm_add_input_operand (ext_asm, NULL, "r",
                                              gcc_jit_lvalue_as_rvalue (src));

      // Example with a symbolic name ("aMask"), the equivalent of:
      //   : [aMask] "r" (Mask)
      gcc_jit_extended_asm_add_input_operand (ext_asm, "aMask", "r",
                                              gcc_jit_lvalue_as_rvalue (mask));

.. function:: void\
              gcc_jit_extended_asm_add_clobber (gcc_jit_extended_asm *ext_asm,\
                                                const char *victim)

   Add `victim` to the list of registers clobbered by the extended ``asm``
   statement.  It must be non-NULL.  See the
   `Clobbers and Scratch Registers <https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html#Clobbers-and-Scratch-Registers#>`_
   section of the documentation of the C syntax.

   Statements with multiple clobbers will require multiple calls, one per
   clobber.

   For example:

   .. code-block:: c

     gcc_jit_extended_asm_add_clobber (ext_asm, "r0");
     gcc_jit_extended_asm_add_clobber (ext_asm, "cc");
     gcc_jit_extended_asm_add_clobber (ext_asm, "memory");

A :type:`gcc_jit_extended_asm` is a :type:`gcc_jit_object` "owned" by
the block's context.  The following upcast is available:

.. function:: gcc_jit_object *\
              gcc_jit_extended_asm_as_object (gcc_jit_extended_asm *ext_asm)

   Upcast from extended ``asm`` to object.


Adding top-level assembler statements
*************************************

In addition to creating extended ``asm`` instructions within a function,
there is support for creating "top-level" assembler statements, outside
of any function.

.. function:: void \
              gcc_jit_context_add_top_level_asm (gcc_jit_context *ctxt,\
                                                 gcc_jit_location *loc,\
                                                 const char *asm_stmts)

   Create a set of top-level asm statements, analogous to those created
   by GCC's "basic" ``asm`` syntax in C at file scope.

   For example, to create the equivalent of:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: // Quote from here in docs/topics/asm.rst: example 5: C
      :end-before: // Quote up to here in docs/topics/asm.rst: example 5: C
      :language: c

   the following API calls could be used:

   .. literalinclude:: ../../../testsuite/jit.dg/test-asm.c
      :start-after: /* Quote from here in docs/topics/asm.rst: example 5: jit.  */
      :end-before: /* Quote up to here in docs/topics/asm.rst: example 5: jit.  */
      :language: c
