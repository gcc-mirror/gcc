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

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_POST_CFI_STARTPROC]
  :end-before: [TARGET_ASM_POST_CFI_STARTPROC]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_EMIT_UNWIND_LABEL]
  :end-before: [TARGET_ASM_EMIT_UNWIND_LABEL]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL]
  :end-before: [TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_EMIT_EXCEPT_PERSONALITY]
  :end-before: [TARGET_ASM_EMIT_EXCEPT_PERSONALITY]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_UNWIND_EMIT]
  :end-before: [TARGET_ASM_UNWIND_EMIT]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT]
  :end-before: [TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_UNWIND_EMIT_BEFORE_INSN]
  :end-before: [TARGET_ASM_UNWIND_EMIT_BEFORE_INSN]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_SHOULD_RESTORE_CFA_STATE]
  :end-before: [TARGET_ASM_SHOULD_RESTORE_CFA_STATE]
