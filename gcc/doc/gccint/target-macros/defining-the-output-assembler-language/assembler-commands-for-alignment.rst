..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _alignment-output:

Assembler Commands for Alignment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes commands for alignment.

.. c:macro:: JUMP_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}`, which is
  a common destination of jumps and has no fallthru incoming edge.

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable :samp:`{align_jumps}` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in :samp:`{align_jumps}` in a ``JUMP_ALIGN`` implementation.

.. c:macro:: LABEL_ALIGN_AFTER_BARRIER (label)

  The alignment (log base 2) to put in front of :samp:`{label}`, which follows
  a ``BARRIER``.

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

.. c:macro:: LOOP_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}` that heads
  a frequently executed basic block (usually the header of a loop).

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable ``align_loops`` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in ``align_loops`` in a ``LOOP_ALIGN`` implementation.

.. c:macro:: LABEL_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}`.
  If ``LABEL_ALIGN_AFTER_BARRIER`` / ``LOOP_ALIGN`` specify a different alignment,
  the maximum of the specified values is used.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable ``align_labels`` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in ``align_labels`` in a ``LABEL_ALIGN`` implementation.

.. c:macro:: ASM_OUTPUT_SKIP (stream, nbytes)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  instruction to advance the location counter by :samp:`{nbytes}` bytes.
  Those bytes should be zero when loaded.  :samp:`{nbytes}` will be a C
  expression of type ``unsigned HOST_WIDE_INT``.

.. c:macro:: ASM_NO_SKIP_IN_TEXT

  Define this macro if ``ASM_OUTPUT_SKIP`` should not be used in the
  text section because it fails to put zeros in the bytes that are skipped.
  This is true on many Unix systems, where the pseudo--op to skip bytes
  produces no-op instructions rather than zeros when used in the text
  section.

.. c:macro:: ASM_OUTPUT_ALIGN (stream, power)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  command to advance the location counter to a multiple of 2 to the
  :samp:`{power}` bytes.  :samp:`{power}` will be a C expression of type ``int``.

.. c:macro:: ASM_OUTPUT_ALIGN_WITH_NOP (stream, power)

  Like ``ASM_OUTPUT_ALIGN``, except that the 'nop' instruction is used
  for padding, if necessary.

.. c:macro:: ASM_OUTPUT_MAX_SKIP_ALIGN (stream, power, max_skip)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  command to advance the location counter to a multiple of 2 to the
  :samp:`{power}` bytes, but only if :samp:`{max_skip}` or fewer bytes are needed to
  satisfy the alignment request.  :samp:`{power}` and :samp:`{max_skip}` will be
  a C expression of type ``int``.
