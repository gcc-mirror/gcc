..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _dispatch-tables:

Output of Dispatch Tables
^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This concerns dispatch tables.

.. index:: dispatch table

.. c:macro:: ASM_OUTPUT_ADDR_DIFF_ELT (stream, body, value, rel)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  pseudo-instruction to generate a difference between two labels.
  :samp:`{value}` and :samp:`{rel}` are the numbers of two internal labels.  The
  definitions of these labels are output using
  ``(*targetm.asm_out.internal_label)``, and they must be printed in the same
  way here.  For example,

  .. code-block:: c++

    fprintf (stream, "\t.word L%d-L%d\n",
             value, rel)

  You must provide this macro on machines where the addresses in a
  dispatch table are relative to the table's own address.  If defined, GCC
  will also use this macro on all machines when producing PIC.
  :samp:`{body}` is the body of the ``ADDR_DIFF_VEC`` ; it is provided so that the
  mode and flags can be read.

.. c:macro:: ASM_OUTPUT_ADDR_VEC_ELT (stream, value)

  This macro should be provided on machines where the addresses
  in a dispatch table are absolute.

  The definition should be a C statement to output to the stdio stream
  :samp:`{stream}` an assembler pseudo-instruction to generate a reference to
  a label.  :samp:`{value}` is the number of an internal label whose
  definition is output using ``(*targetm.asm_out.internal_label)``.
  For example,

  .. code-block:: c++

    fprintf (stream, "\t.word L%d\n", value)

.. c:macro:: ASM_OUTPUT_CASE_LABEL (stream, prefix, num, table)

  Define this if the label before a jump-table needs to be output
  specially.  The first three arguments are the same as for
  ``(*targetm.asm_out.internal_label)`` ; the fourth argument is the
  jump-table which follows (a ``jump_table_data`` containing an
  ``addr_vec`` or ``addr_diff_vec``).

  This feature is used on system V to output a ``swbeg`` statement
  for the table.

  If this macro is not defined, these labels are output with
  ``(*targetm.asm_out.internal_label)``.

.. c:macro:: ASM_OUTPUT_CASE_END (stream, num, table)

  Define this if something special must be output at the end of a
  jump-table.  The definition should be a C statement to be executed
  after the assembler code for the table is written.  It should write
  the appropriate code to stdio stream :samp:`{stream}`.  The argument
  :samp:`{table}` is the jump-table insn, and :samp:`{num}` is the label-number
  of the preceding label.

  If this macro is not defined, nothing special is output at the end of
  the jump-table.

.. function:: void TARGET_ASM_POST_CFI_STARTPROC (FILE *, tree)

  .. hook-start:TARGET_ASM_POST_CFI_STARTPROC

  This target hook is used to emit assembly strings required by the target
  after the .cfi_startproc directive.  The first argument is the file stream to
  write the strings to and the second argument is the function's declaration.  The
  expected use is to add more .cfi_\* directives.

  The default is to not output any assembly strings.

.. hook-end

.. function:: void TARGET_ASM_EMIT_UNWIND_LABEL (FILE *stream, tree decl, int for_eh, int empty)

  .. hook-start:TARGET_ASM_EMIT_UNWIND_LABEL

  This target hook emits a label at the beginning of each FDE.  It
  should be defined on targets where FDEs need special labels, and it
  should write the appropriate label, for the FDE associated with the
  function declaration :samp:`{decl}`, to the stdio stream :samp:`{stream}`.
  The third argument, :samp:`{for_eh}`, is a boolean: true if this is for an
  exception table.  The fourth argument, :samp:`{empty}`, is a boolean:
  true if this is a placeholder label for an omitted FDE.

  The default is that FDEs are not given nonlocal labels.

.. hook-end

.. function:: void TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL (FILE *stream)

  .. hook-start:TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL

  This target hook emits a label at the beginning of the exception table.
  It should be defined on targets where it is desirable for the table
  to be broken up according to function.

  The default is that no label is emitted.

.. hook-end

.. function:: void TARGET_ASM_EMIT_EXCEPT_PERSONALITY (rtx personality)

  .. hook-start:TARGET_ASM_EMIT_EXCEPT_PERSONALITY

  If the target implements ``TARGET_ASM_UNWIND_EMIT``, this hook may be
  used to emit a directive to install a personality hook into the unwind
  info.  This hook should not be used if dwarf2 unwind info is used.

.. hook-end

.. function:: void TARGET_ASM_UNWIND_EMIT (FILE *stream, rtx_insn *insn)

  .. hook-start:TARGET_ASM_UNWIND_EMIT

  This target hook emits assembly directives required to unwind the
  given instruction.  This is only used when ``TARGET_EXCEPT_UNWIND_INFO``
  returns ``UI_TARGET``.

.. hook-end

.. function:: rtx TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT (rtx origsymbol, bool pubvis)

  .. hook-start:TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT

  If necessary, modify personality and LSDA references to handle indirection.
  The original symbol is in ``origsymbol`` and if ``pubvis`` is true
  the symbol is visible outside the TU.

.. hook-end

.. c:var:: bool TARGET_ASM_UNWIND_EMIT_BEFORE_INSN

  .. hook-start:TARGET_ASM_UNWIND_EMIT_BEFORE_INSN

  True if the ``TARGET_ASM_UNWIND_EMIT`` hook should be called before
  the assembly for :samp:`{insn}` has been emitted, false if the hook should
  be called afterward.

.. hook-end

.. function:: bool TARGET_ASM_SHOULD_RESTORE_CFA_STATE (void)

  .. hook-start:TARGET_ASM_SHOULD_RESTORE_CFA_STATE

  For DWARF-based unwind frames, two CFI instructions provide for save and
  restore of register state.  GCC maintains the current frame address (CFA)
  separately from the register bank but the unwinder in libgcc preserves this
  state along with the registers (and this is expected by the code that writes
  the unwind frames).  This hook allows the target to specify that the CFA data
  is not saved/restored along with the registers by the target unwinder so that
  suitable additional instructions should be emitted to restore it.

.. hook-end