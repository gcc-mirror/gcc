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

.. function:: void TARGET_ASM_FILE_START (void)

  .. hook-start:TARGET_ASM_FILE_START

  Output to ``asm_out_file`` any text which the assembler expects to
  find at the beginning of a file.  The default behavior is controlled
  by two flags, documented below.  Unless your target's assembler is
  quite unusual, if you override the default, you should call
  ``default_file_start`` at some point in your target hook.  This
  lets other target files rely on these variables.

.. hook-end

.. c:var:: bool TARGET_ASM_FILE_START_APP_OFF

  .. hook-start:TARGET_ASM_FILE_START_APP_OFF

  If this flag is true, the text of the macro ``ASM_APP_OFF`` will be
  printed as the very first line in the assembly file, unless
  :option:`-fverbose-asm` is in effect.  (If that macro has been defined
  to the empty string, this variable has no effect.)  With the normal
  definition of ``ASM_APP_OFF``, the effect is to notify the GNU
  assembler that it need not bother stripping comments or extra
  whitespace from its input.  This allows it to work a bit faster.

  The default is false.  You should not set it to true unless you have
  verified that your port does not generate any extra whitespace or
  comments that will cause GAS to issue errors in NO_APP mode.

.. hook-end

.. c:var:: bool TARGET_ASM_FILE_START_FILE_DIRECTIVE

  .. hook-start:TARGET_ASM_FILE_START_FILE_DIRECTIVE

  If this flag is true, ``output_file_directive`` will be called
  for the primary source file, immediately after printing
  ``ASM_APP_OFF`` (if that is enabled).  Most ELF assemblers expect
  this to be done.  The default is false.

.. hook-end

.. function:: void TARGET_ASM_FILE_END (void)

  .. hook-start:TARGET_ASM_FILE_END

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the end of a file.  The default is to output nothing.

.. hook-end

.. function:: void file_end_indicate_exec_stack ()

  Some systems use a common convention, the :samp:`.note.GNU-stack`
  special section, to indicate whether or not an object file relies on
  the stack being executable.  If your system uses this convention, you
  should define ``TARGET_ASM_FILE_END`` to this function.  If you
  need to do other things in that hook, have your hook function call
  this function.

.. function:: void TARGET_ASM_LTO_START (void)

  .. hook-start:TARGET_ASM_LTO_START

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the start of an LTO section.  The default is to output
  nothing.

.. hook-end

.. function:: void TARGET_ASM_LTO_END (void)

  .. hook-start:TARGET_ASM_LTO_END

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the end of an LTO section.  The default is to output
  nothing.

.. hook-end

.. function:: void TARGET_ASM_CODE_END (void)

  .. hook-start:TARGET_ASM_CODE_END

  Output to ``asm_out_file`` any text which is needed before emitting
  unwind info and debug info at the end of a file.  Some targets emit
  here PIC setup thunks that cannot be emitted at the end of file,
  because they couldn't have unwind info then.  The default is to output
  nothing.

.. hook-end

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

.. function:: void TARGET_ASM_OUTPUT_SOURCE_FILENAME (FILE *file, const char *name)

  .. hook-start:TARGET_ASM_OUTPUT_SOURCE_FILENAME

  Output DWARF debugging information which indicates that filename
  :samp:`{name}` is the current source file to the stdio stream :samp:`{file}`.

  This target hook need not be defined if the standard form of output
  for the file format in use is appropriate.

.. hook-end

.. function:: void TARGET_ASM_OUTPUT_IDENT (const char *name)

  .. hook-start:TARGET_ASM_OUTPUT_IDENT

  Output a string based on :samp:`{name}`, suitable for the :samp:`#ident`
  directive, or the equivalent directive or pragma in non-C-family languages.
  If this hook is not defined, nothing is output for the :samp:`#ident`
  directive.

.. hook-end

.. c:macro:: OUTPUT_QUOTED_STRING (stream, string)

  A C statement to output the string :samp:`{string}` to the stdio stream
  :samp:`{stream}`.  If you do not call the function ``output_quoted_string``
  in your config files, GCC will only call it to output filenames to
  the assembler source.  So you can use it to canonicalize the format
  of the filename using this macro.

.. function:: void TARGET_ASM_NAMED_SECTION (const char *name, unsigned int flags, tree decl)

  .. hook-start:TARGET_ASM_NAMED_SECTION

  Output assembly directives to switch to section :samp:`{name}`.  The section
  should have attributes as specified by :samp:`{flags}`, which is a bit mask
  of the ``SECTION_*`` flags defined in :samp:`output.h`.  If :samp:`{decl}`
  is non-NULL, it is the ``VAR_DECL`` or ``FUNCTION_DECL`` with which
  this section is associated.

.. hook-end

.. function:: bool TARGET_ASM_ELF_FLAGS_NUMERIC (unsigned int flags, unsigned int *num)

  .. hook-start:TARGET_ASM_ELF_FLAGS_NUMERIC

  This hook can be used to encode ELF section flags for which no letter
  code has been defined in the assembler.  It is called by
  ``default_asm_named_section`` whenever the section flags need to be
  emitted in the assembler output.  If the hook returns true, then the
  numerical value for ELF section flags should be calculated from
  :samp:`{flags}` and saved in :samp:`{*num}` ; the value is printed out instead of the
  normal sequence of letter codes.  If the hook is not defined, or if it
  returns false, then :samp:`{num}` is ignored and the traditional letter sequence
  is emitted.

.. hook-end

.. function:: section * TARGET_ASM_FUNCTION_SECTION (tree decl, enum node_frequency freq, bool startup, bool exit)

  .. hook-start:TARGET_ASM_FUNCTION_SECTION

  Return preferred text (sub)section for function :samp:`{decl}`.
  Main purpose of this function is to separate cold, normal and hot
  functions. :samp:`{startup}` is true when function is known to be used only
  at startup (from static constructors or it is ``main()``).
  :samp:`{exit}` is true when function is known to be used only at exit
  (from static destructors).
  Return NULL if function should go to default text section.

.. hook-end

.. function:: void TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS (FILE *file, tree decl, bool new_is_cold)

  .. hook-start:TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS

  Used by the target to emit any assembler directives or additional
  labels needed when a function is partitioned between different
  sections.  Output should be written to :samp:`{file}`.  The function
  decl is available as :samp:`{decl}` and the new section is 'cold' if
  :samp:`{new_is_cold}` is ``true``.

.. hook-end

.. c:var:: bool TARGET_HAVE_NAMED_SECTIONS

  .. hook-start:TARGET_HAVE_NAMED_SECTIONS

  .. hook-end

  This flag is true if the target supports ``TARGET_ASM_NAMED_SECTION``.
  It must not be modified by command-line option processing.

.. _target_have_switchable_bss_sections:

.. c:var:: bool TARGET_HAVE_SWITCHABLE_BSS_SECTIONS

  .. hook-start:TARGET_HAVE_SWITCHABLE_BSS_SECTIONS

  This flag is true if we can create zeroed data by switching to a BSS
  section and then using ``ASM_OUTPUT_SKIP`` to allocate the space.
  This is true on most ELF targets.

.. hook-end

.. function:: unsigned int TARGET_SECTION_TYPE_FLAGS (tree decl, const char *name, int reloc)

  .. hook-start:TARGET_SECTION_TYPE_FLAGS

  Choose a set of section attributes for use by ``TARGET_ASM_NAMED_SECTION``
  based on a variable or function decl, a section name, and whether or not the
  declaration's initializer may contain runtime relocations.  :samp:`{decl}` may be
  null, in which case read-write data should be assumed.

  The default version of this function handles choosing code vs data,
  read-only vs read-write data, and ``flag_pic``.  You should only
  need to override this if your target has special flags that might be
  set via ``__attribute__``.

.. hook-end

.. function:: void TARGET_ASM_RECORD_GCC_SWITCHES (const char *)

  .. hook-start:TARGET_ASM_RECORD_GCC_SWITCHES

  Provides the target with the ability to record the gcc command line
  switches provided as argument.

  By default this hook is set to NULL, but an example implementation is
  provided for ELF based targets.  Called :samp:`{elf_record_gcc_switches}`,
  it records the switches as ASCII text inside a new, string mergeable
  section in the assembler output file.  The name of the new section is
  provided by the ``TARGET_ASM_RECORD_GCC_SWITCHES_SECTION`` target
  hook.

.. hook-end

.. c:var:: const char * TARGET_ASM_RECORD_GCC_SWITCHES_SECTION

  .. hook-start:TARGET_ASM_RECORD_GCC_SWITCHES_SECTION

  This is the name of the section that will be created by the example
  ELF implementation of the ``TARGET_ASM_RECORD_GCC_SWITCHES`` target
  hook.

.. hook-end