..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _sections:

Dividing the Output into Sections (Texts, Data, ...)
****************************************************

.. the above section title is WAY too long.  maybe cut the part between
   the (...)?  -mew 10feb93

.. the (...)?  -mew 10feb93

An object file is divided into sections containing different types of
data.  In the most common case, there are three sections: the :dfn:`text
section`, which holds instructions and read-only data; the :dfn:`data
section`, which holds initialized writable data; and the :dfn:`bss
section`, which holds uninitialized data.  Some systems have other kinds
of sections.

:samp:`varasm.cc` provides several well-known sections, such as
``text_section``, ``data_section`` and ``bss_section``.
The normal way of controlling a ``foo_section`` variable
is to define the associated ``FOO_SECTION_ASM_OP`` macro,
as described below.  The macros are only read once, when :samp:`varasm.cc`
initializes itself, so their values must be run-time constants.
They may however depend on command-line flags.

.. note::

  Some run-time files, such :samp:`crtstuff.c`, also make
  use of the ``FOO_SECTION_ASM_OP`` macros, and expect them
  to be string literals.

Some assemblers require a different string to be written every time a
section is selected.  If your assembler falls into this category, you
should define the ``TARGET_ASM_INIT_SECTIONS`` hook and use
``get_unnamed_section`` to set up the sections.

You must always create a ``text_section``, either by defining
``TEXT_SECTION_ASM_OP`` or by initializing ``text_section``
in ``TARGET_ASM_INIT_SECTIONS``.  The same is true of
``data_section`` and ``DATA_SECTION_ASM_OP``.  If you do not
create a distinct ``readonly_data_section``, the default is to
reuse ``text_section``.

All the other :samp:`varasm.cc` sections are optional, and are null
if the target does not provide them.

.. c:macro:: TEXT_SECTION_ASM_OP

  A C expression whose value is a string, including spacing, containing the
  assembler operation that should precede instructions and read-only data.
  Normally ``"\t.text"`` is right.

.. c:macro:: HOT_TEXT_SECTION_NAME

  If defined, a C string constant for the name of the section containing most
  frequently executed functions of the program.  If not defined, GCC will provide
  a default definition if the target supports named sections.

.. c:macro:: UNLIKELY_EXECUTED_TEXT_SECTION_NAME

  If defined, a C string constant for the name of the section containing unlikely
  executed functions in the program.

.. c:macro:: DATA_SECTION_ASM_OP

  A C expression whose value is a string, including spacing, containing the
  assembler operation to identify the following data as writable initialized
  data.  Normally ``"\t.data"`` is right.

.. c:macro:: SDATA_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  initialized, writable small data.

.. c:macro:: READONLY_DATA_SECTION_ASM_OP

  A C expression whose value is a string, including spacing, containing the
  assembler operation to identify the following data as read-only initialized
  data.

.. c:macro:: BSS_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  uninitialized global data.  If not defined, and
  ``ASM_OUTPUT_ALIGNED_BSS`` not defined,
  uninitialized global data will be output in the data section if
  :option:`-fno-common` is passed, otherwise ``ASM_OUTPUT_COMMON`` will be
  used.

.. c:macro:: SBSS_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  uninitialized, writable small data.

.. c:macro:: TLS_COMMON_ASM_OP

  If defined, a C expression whose value is a string containing the
  assembler operation to identify the following data as thread-local
  common data.  The default is ``".tls_common"``.

.. c:macro:: TLS_SECTION_ASM_FLAG

  If defined, a C expression whose value is a character constant
  containing the flag used to mark a section as a TLS section.  The
  default is ``'T'``.

.. c:macro:: INIT_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  initialization code.  If not defined, GCC will assume such a section does
  not exist.  This section has no corresponding ``init_section``
  variable; it is used entirely in runtime code.

.. c:macro:: FINI_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  finalization code.  If not defined, GCC will assume such a section does
  not exist.  This section has no corresponding ``fini_section``
  variable; it is used entirely in runtime code.

.. c:macro:: INIT_ARRAY_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  part of the ``.init_array`` (or equivalent) section.  If not
  defined, GCC will assume such a section does not exist.  Do not define
  both this macro and ``INIT_SECTION_ASM_OP``.

.. c:macro:: FINI_ARRAY_SECTION_ASM_OP

  If defined, a C expression whose value is a string, including spacing,
  containing the assembler operation to identify the following data as
  part of the ``.fini_array`` (or equivalent) section.  If not
  defined, GCC will assume such a section does not exist.  Do not define
  both this macro and ``FINI_SECTION_ASM_OP``.

.. c:macro:: MACH_DEP_SECTION_ASM_FLAG

  If defined, a C expression whose value is a character constant
  containing the flag used to mark a machine-dependent section.  This
  corresponds to the ``SECTION_MACH_DEP`` section flag.

.. c:macro:: CRT_CALL_STATIC_FUNCTION (section_op, function)

  If defined, an ASM statement that switches to a different section
  via :samp:`{section_op}`, calls :samp:`{function}`, and switches back to
  the text section.  This is used in :samp:`crtstuff.c` if
  ``INIT_SECTION_ASM_OP`` or ``FINI_SECTION_ASM_OP`` to calls
  to initialization and finalization functions from the init and fini
  sections.  By default, this macro uses a simple function call.  Some
  ports need hand-crafted assembly code to avoid dependencies on
  registers initialized in the function prologue or to ensure that
  constant pools don't end up too far way in the text section.

.. c:macro:: TARGET_LIBGCC_SDATA_SECTION

  If defined, a string which names the section into which small
  variables defined in crtstuff and libgcc should go.  This is useful
  when the target has options for optimizing access to small data, and
  you want the crtstuff and libgcc routines to be conservative in what
  they expect of your application yet liberal in what your application
  expects.  For example, for targets with a ``.sdata`` section (like
  MIPS), you could compile crtstuff with ``-G 0`` so that it doesn't
  require small data support from your application, but use this macro
  to put small data into ``.sdata`` so that your application can
  access these variables whether it uses small data or not.

.. c:macro:: FORCE_CODE_SECTION_ALIGN

  If defined, an ASM statement that aligns a code section to some
  arbitrary boundary.  This is used to force all fragments of the
  ``.init`` and ``.fini`` sections to have to same alignment
  and thus prevent the linker from having to add any padding.

.. c:macro:: JUMP_TABLES_IN_TEXT_SECTION

  Define this macro to be an expression with a nonzero value if jump
  tables (for ``tablejump`` insns) should be output in the text
  section, along with the assembler instructions.  Otherwise, the
  readonly data section is used.

  This macro is irrelevant if there is no separate readonly data section.

.. include:: tm.rst.in
  :start-after: [TARGET_ASM_INIT_SECTIONS]
  :end-before: [TARGET_ASM_INIT_SECTIONS]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_RELOC_RW_MASK]
  :end-before: [TARGET_ASM_RELOC_RW_MASK]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_GENERATE_PIC_ADDR_DIFF_VEC]
  :end-before: [TARGET_ASM_GENERATE_PIC_ADDR_DIFF_VEC]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_SELECT_SECTION]
  :end-before: [TARGET_ASM_SELECT_SECTION]


.. c:macro:: USE_SELECT_SECTION_FOR_FUNCTIONS

  Define this macro if you wish TARGET_ASM_SELECT_SECTION to be called
  for ``FUNCTION_DECL`` s as well as for variables and constants.

  In the case of a ``FUNCTION_DECL``, :samp:`{reloc}` will be zero if the
  function has been determined to be likely to be called, and nonzero if
  it is unlikely to be called.

.. include:: tm.rst.in
  :start-after: [TARGET_ASM_UNIQUE_SECTION]
  :end-before: [TARGET_ASM_UNIQUE_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_FUNCTION_RODATA_SECTION]
  :end-before: [TARGET_ASM_FUNCTION_RODATA_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_MERGEABLE_RODATA_PREFIX]
  :end-before: [TARGET_ASM_MERGEABLE_RODATA_PREFIX]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_TM_CLONE_TABLE_SECTION]
  :end-before: [TARGET_ASM_TM_CLONE_TABLE_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_SELECT_RTX_SECTION]
  :end-before: [TARGET_ASM_SELECT_RTX_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_MANGLE_DECL_ASSEMBLER_NAME]
  :end-before: [TARGET_MANGLE_DECL_ASSEMBLER_NAME]


.. include:: tm.rst.in
  :start-after: [TARGET_ENCODE_SECTION_INFO]
  :end-before: [TARGET_ENCODE_SECTION_INFO]


.. include:: tm.rst.in
  :start-after: [TARGET_STRIP_NAME_ENCODING]
  :end-before: [TARGET_STRIP_NAME_ENCODING]


.. include:: tm.rst.in
  :start-after: [TARGET_IN_SMALL_DATA_P]
  :end-before: [TARGET_IN_SMALL_DATA_P]


.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_SRODATA_SECTION]
  :end-before: [TARGET_HAVE_SRODATA_SECTION]


.. include:: tm.rst.in
  :start-after: [TARGET_PROFILE_BEFORE_PROLOGUE]
  :end-before: [TARGET_PROFILE_BEFORE_PROLOGUE]


.. include:: tm.rst.in
  :start-after: [TARGET_BINDS_LOCAL_P]
  :end-before: [TARGET_BINDS_LOCAL_P]


.. include:: tm.rst.in
  :start-after: [TARGET_HAVE_TLS]
  :end-before: [TARGET_HAVE_TLS]
