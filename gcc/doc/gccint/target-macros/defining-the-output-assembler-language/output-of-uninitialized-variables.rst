..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _uninitialized-data:

Output of Uninitialized Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each of the macros in this section is used to do the whole job of
outputting a single uninitialized variable.

.. c:macro:: ASM_OUTPUT_COMMON (stream, name, size, rounded)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a common-label named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{rounded}`
  is the size rounded up to whatever alignment the caller wants.  It is
  possible that :samp:`{size}` may be zero, for instance if a struct with no
  other member than a zero-length array is defined.  In this case, the
  backend must output a symbol definition that allocates at least one
  byte, both so that the address of the resulting object does not compare
  equal to any other, and because some object formats cannot even express
  the concept of a zero-sized common symbol, as that is how they represent
  an ordinary undefined external.

  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.

  This macro controls how the assembler definitions of uninitialized
  common global variables are output.

.. c:macro:: ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, alignment)

  Like ``ASM_OUTPUT_COMMON`` except takes the required alignment as a
  separate, explicit argument.  If you define this macro, it is used in
  place of ``ASM_OUTPUT_COMMON``, and gives you more flexibility in
  handling the required alignment of the variable.  The alignment is specified
  as the number of bits.

.. c:macro:: ASM_OUTPUT_ALIGNED_DECL_COMMON (stream, decl, name, size, alignment)

  Like ``ASM_OUTPUT_ALIGNED_COMMON`` except that :samp:`{decl}` of the
  variable to be output, if there is one, or ``NULL_TREE`` if there
  is no corresponding variable.  If you define this macro, GCC will use it
  in place of both ``ASM_OUTPUT_COMMON`` and
  ``ASM_OUTPUT_ALIGNED_COMMON``.  Define this macro when you need to see
  the variable's decl in order to chose what to output.

.. c:macro:: ASM_OUTPUT_ALIGNED_BSS (stream, decl, name, size, alignment)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of uninitialized global :samp:`{decl}` named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{alignment}`
  is the alignment specified as the number of bits.

  Try to use function ``asm_output_aligned_bss`` defined in file
  :samp:`varasm.cc` when defining this macro.  If unable, use the expression
  ``assemble_name (stream, name)`` to output the name itself;
  before and after that, output the additional assembler syntax for defining
  the name, and a newline.

  There are two ways of handling global BSS.  One is to define this macro.
  The other is to have ``TARGET_ASM_SELECT_SECTION`` return a
  switchable BSS section (see :ref:`target_have_switchable_bss_sections`).
  You do not need to do both.

  Some languages do not have ``common`` data, and require a
  non-common form of global BSS in order to handle uninitialized globals
  efficiently.  C++ is one example of this.  However, if the target does
  not support global BSS, the front end may choose to make globals
  common in order to save space in the object file.

.. c:macro:: ASM_OUTPUT_LOCAL (stream, name, size, rounded)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a local-common-label named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{rounded}`
  is the size rounded up to whatever alignment the caller wants.

  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.

  This macro controls how the assembler definitions of uninitialized
  static variables are output.

.. c:macro:: ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, alignment)

  Like ``ASM_OUTPUT_LOCAL`` except takes the required alignment as a
  separate, explicit argument.  If you define this macro, it is used in
  place of ``ASM_OUTPUT_LOCAL``, and gives you more flexibility in
  handling the required alignment of the variable.  The alignment is specified
  as the number of bits.

.. c:macro:: ASM_OUTPUT_ALIGNED_DECL_LOCAL (stream, decl, name, size, alignment)

  Like ``ASM_OUTPUT_ALIGNED_LOCAL`` except that :samp:`{decl}` of the
  variable to be output, if there is one, or ``NULL_TREE`` if there
  is no corresponding variable.  If you define this macro, GCC will use it
  in place of both ``ASM_OUTPUT_LOCAL`` and
  ``ASM_OUTPUT_ALIGNED_LOCAL``.  Define this macro when you need to see
  the variable's decl in order to chose what to output.