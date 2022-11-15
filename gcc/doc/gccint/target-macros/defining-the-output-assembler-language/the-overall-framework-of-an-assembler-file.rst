..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: assembler format, output of assembler code

.. _file-framework:

The Overall Framework of an Assembler File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes the overall framework of an assembly file.

.. index:: default_file_start

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FILE_START]
  :end-before: [TARGET_ASM_FILE_START]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FILE_START_APP_OFF]
  :end-before: [TARGET_ASM_FILE_START_APP_OFF]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FILE_START_FILE_DIRECTIVE]
  :end-before: [TARGET_ASM_FILE_START_FILE_DIRECTIVE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FILE_END]
  :end-before: [TARGET_ASM_FILE_END]


.. function:: void file_end_indicate_exec_stack ()

  Some systems use a common convention, the :samp:`.note.GNU-stack`
  special section, to indicate whether or not an object file relies on
  the stack being executable.  If your system uses this convention, you
  should define ``TARGET_ASM_FILE_END`` to this function.  If you
  need to do other things in that hook, have your hook function call
  this function.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_LTO_START]
  :end-before: [TARGET_ASM_LTO_START]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_LTO_END]
  :end-before: [TARGET_ASM_LTO_END]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_CODE_END]
  :end-before: [TARGET_ASM_CODE_END]


.. c:macro:: ASM_COMMENT_START

  A C string constant describing how to begin a comment in the target
  assembler language.  The compiler assumes that the comment will end at
  the end of the line.

.. c:macro:: ASM_APP_ON

  A C string constant for text to be output before each ``asm``
  statement or group of consecutive ones.  Normally this is
  ``"#APP"``, which is a comment that has no effect on most
  assemblers but tells the GNU assembler that it must check the lines
  that follow for all valid assembler constructs.

.. c:macro:: ASM_APP_OFF

  A C string constant for text to be output after each ``asm``
  statement or group of consecutive ones.  Normally this is
  ``"#NO_APP"``, which tells the GNU assembler to resume making the
  time-saving assumptions that are valid for ordinary compiler output.

.. c:macro:: ASM_OUTPUT_SOURCE_FILENAME (stream, name)

  A C statement to output COFF information or DWARF debugging information
  which indicates that filename :samp:`{name}` is the current source file to
  the stdio stream :samp:`{stream}`.

  This macro need not be defined if the standard form of output
  for the file format in use is appropriate.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_OUTPUT_SOURCE_FILENAME]
  :end-before: [TARGET_ASM_OUTPUT_SOURCE_FILENAME]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_OUTPUT_IDENT]
  :end-before: [TARGET_ASM_OUTPUT_IDENT]


.. c:macro:: OUTPUT_QUOTED_STRING (stream, string)

  A C statement to output the string :samp:`{string}` to the stdio stream
  :samp:`{stream}`.  If you do not call the function ``output_quoted_string``
  in your config files, GCC will only call it to output filenames to
  the assembler source.  So you can use it to canonicalize the format
  of the filename using this macro.

.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_NAMED_SECTION]
  :end-before: [TARGET_ASM_NAMED_SECTION]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_ELF_FLAGS_NUMERIC]
  :end-before: [TARGET_ASM_ELF_FLAGS_NUMERIC]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_SECTION]
  :end-before: [TARGET_ASM_FUNCTION_SECTION]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS]
  :end-before: [TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS]


.. c:var:: bool TARGET_HAVE_NAMED_SECTIONS

  This flag is true if the target supports ``TARGET_ASM_NAMED_SECTION``.
  It must not be modified by command-line option processing.

.. _target_have_switchable_bss_sections:

.. include:: ../tm.rst.in
  :start-after: [TARGET_HAVE_SWITCHABLE_BSS_SECTIONS]
  :end-before: [TARGET_HAVE_SWITCHABLE_BSS_SECTIONS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_SECTION_TYPE_FLAGS]
  :end-before: [TARGET_SECTION_TYPE_FLAGS]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_RECORD_GCC_SWITCHES]
  :end-before: [TARGET_ASM_RECORD_GCC_SWITCHES]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_RECORD_GCC_SWITCHES_SECTION]
  :end-before: [TARGET_ASM_RECORD_GCC_SWITCHES_SECTION]
