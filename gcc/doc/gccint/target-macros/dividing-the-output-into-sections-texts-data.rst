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

.. function:: void TARGET_ASM_INIT_SECTIONS (void)

  .. hook-start:TARGET_ASM_INIT_SECTIONS

  Define this hook if you need to do something special to set up the
  :samp:`varasm.cc` sections, or if your target has some special sections
  of its own that you need to create.

  GCC calls this hook after processing the command line, but before writing
  any assembly code, and before calling any of the section-returning hooks
  described below.

.. hook-end

.. function:: int TARGET_ASM_RELOC_RW_MASK (void)

  .. hook-start:TARGET_ASM_RELOC_RW_MASK

  Return a mask describing how relocations should be treated when
  selecting sections.  Bit 1 should be set if global relocations
  should be placed in a read-write section; bit 0 should be set if
  local relocations should be placed in a read-write section.

  The default version of this function returns 3 when :option:`-fpic`
  is in effect, and 0 otherwise.  The hook is typically redefined
  when the target cannot support (some kinds of) dynamic relocations
  in read-only sections even in executables.

.. hook-end

.. function:: bool TARGET_ASM_GENERATE_PIC_ADDR_DIFF_VEC (void)

  .. hook-start:TARGET_ASM_GENERATE_PIC_ADDR_DIFF_VEC

  Return true to generate ADDR_DIF_VEC table
  or false to generate ADDR_VEC table for jumps in case of -fPIC.

  The default version of this function returns true if flag_pic
  equals true and false otherwise

.. hook-end

.. function:: section * TARGET_ASM_SELECT_SECTION (tree exp, int reloc, unsigned HOST_WIDE_INT align)

  .. hook-start:TARGET_ASM_SELECT_SECTION

  Return the section into which :samp:`{exp}` should be placed.  You can
  assume that :samp:`{exp}` is either a ``VAR_DECL`` node or a constant of
  some sort.  :samp:`{reloc}` indicates whether the initial value of :samp:`{exp}`
  requires link-time relocations.  Bit 0 is set when variable contains
  local relocations only, while bit 1 is set for global relocations.
  :samp:`{align}` is the constant alignment in bits.

  The default version of this function takes care of putting read-only
  variables in ``readonly_data_section``.

  See also :samp:`{USE_SELECT_SECTION_FOR_FUNCTIONS}`.

.. hook-end

.. c:macro:: USE_SELECT_SECTION_FOR_FUNCTIONS

  Define this macro if you wish TARGET_ASM_SELECT_SECTION to be called
  for ``FUNCTION_DECL`` s as well as for variables and constants.

  In the case of a ``FUNCTION_DECL``, :samp:`{reloc}` will be zero if the
  function has been determined to be likely to be called, and nonzero if
  it is unlikely to be called.

.. function:: void TARGET_ASM_UNIQUE_SECTION (tree decl, int reloc)

  .. hook-start:TARGET_ASM_UNIQUE_SECTION

  Build up a unique section name, expressed as a ``STRING_CST`` node,
  and assign it to :samp:`DECL_SECTION_NAME ({decl})`.
  As with ``TARGET_ASM_SELECT_SECTION``, :samp:`{reloc}` indicates whether
  the initial value of :samp:`{exp}` requires link-time relocations.

  The default version of this function appends the symbol name to the
  ELF section name that would normally be used for the symbol.  For
  example, the function ``foo`` would be placed in ``.text.foo``.
  Whatever the actual target object format, this is often good enough.

.. hook-end

.. function:: section * TARGET_ASM_FUNCTION_RODATA_SECTION (tree decl, bool relocatable)

  .. hook-start:TARGET_ASM_FUNCTION_RODATA_SECTION

  Return the readonly data or reloc readonly data section associated with
  :samp:`DECL_SECTION_NAME ({decl})`. :samp:`{relocatable}` selects the latter
  over the former.
  The default version of this function selects ``.gnu.linkonce.r.name`` if
  the function's section is ``.gnu.linkonce.t.name``, ``.rodata.name``
  or ``.data.rel.ro.name`` if function is in ``.text.name``, and
  the normal readonly-data or reloc readonly data section otherwise.

.. hook-end

.. c:var:: const char * TARGET_ASM_MERGEABLE_RODATA_PREFIX

  .. hook-start:TARGET_ASM_MERGEABLE_RODATA_PREFIX

  Usually, the compiler uses the prefix ``".rodata"`` to construct
  section names for mergeable constant data.  Define this macro to override
  the string if a different section name should be used.

.. hook-end

.. function:: section * TARGET_ASM_TM_CLONE_TABLE_SECTION (void)

  .. hook-start:TARGET_ASM_TM_CLONE_TABLE_SECTION

  Return the section that should be used for transactional memory clone
  tables.

.. hook-end

.. function:: section * TARGET_ASM_SELECT_RTX_SECTION (machine_mode mode, rtx x, unsigned HOST_WIDE_INT align)

  .. hook-start:TARGET_ASM_SELECT_RTX_SECTION

  Return the section into which a constant :samp:`{x}`, of mode :samp:`{mode}`,
  should be placed.  You can assume that :samp:`{x}` is some kind of
  constant in RTL.  The argument :samp:`{mode}` is redundant except in the
  case of a ``const_int`` rtx.  :samp:`{align}` is the constant alignment
  in bits.

  The default version of this function takes care of putting symbolic
  constants in ``flag_pic`` mode in ``data_section`` and everything
  else in ``readonly_data_section``.

.. hook-end

.. function:: tree TARGET_MANGLE_DECL_ASSEMBLER_NAME (tree decl, tree id)

  .. hook-start:TARGET_MANGLE_DECL_ASSEMBLER_NAME

  Define this hook if you need to postprocess the assembler name generated
  by target-independent code.  The :samp:`{id}` provided to this hook will be
  the computed name (e.g., the macro ``DECL_NAME`` of the :samp:`{decl}` in C,
  or the mangled name of the :samp:`{decl}` in C++).  The return value of the
  hook is an ``IDENTIFIER_NODE`` for the appropriate mangled name on
  your target system.  The default implementation of this hook just
  returns the :samp:`{id}` provided.

.. hook-end

.. function:: void TARGET_ENCODE_SECTION_INFO (tree decl, rtx rtl, int new_decl_p)

  .. hook-start:TARGET_ENCODE_SECTION_INFO

  Define this hook if references to a symbol or a constant must be
  treated differently depending on something about the variable or
  function named by the symbol (such as what section it is in).

  The hook is executed immediately after rtl has been created for
  :samp:`{decl}`, which may be a variable or function declaration or
  an entry in the constant pool.  In either case, :samp:`{rtl}` is the
  rtl in question.  Do *not* use ``DECL_RTL (decl)``
  in this hook; that field may not have been initialized yet.

  In the case of a constant, it is safe to assume that the rtl is
  a ``mem`` whose address is a ``symbol_ref``.  Most decls
  will also have this form, but that is not guaranteed.  Global
  register variables, for instance, will have a ``reg`` for their
  rtl.  (Normally the right thing to do with such unusual rtl is
  leave it alone.)

  The :samp:`{new_decl_p}` argument will be true if this is the first time
  that ``TARGET_ENCODE_SECTION_INFO`` has been invoked on this decl.  It will
  be false for subsequent invocations, which will happen for duplicate
  declarations.  Whether or not anything must be done for the duplicate
  declaration depends on whether the hook examines ``DECL_ATTRIBUTES``.
  :samp:`{new_decl_p}` is always true when the hook is called for a constant.

  .. index:: SYMBOL_REF_FLAG, in TARGET_ENCODE_SECTION_INFO

  The usual thing for this hook to do is to record flags in the
  ``symbol_ref``, using ``SYMBOL_REF_FLAG`` or ``SYMBOL_REF_FLAGS``.
  Historically, the name string was modified if it was necessary to
  encode more than one bit of information, but this practice is now
  discouraged; use ``SYMBOL_REF_FLAGS``.

  The default definition of this hook, ``default_encode_section_info``
  in :samp:`varasm.cc`, sets a number of commonly-useful bits in
  ``SYMBOL_REF_FLAGS``.  Check whether the default does what you need
  before overriding it.

.. hook-end

.. function:: const char * TARGET_STRIP_NAME_ENCODING (const char *name)

  .. hook-start:TARGET_STRIP_NAME_ENCODING

  Decode :samp:`{name}` and return the real name part, sans
  the characters that ``TARGET_ENCODE_SECTION_INFO``
  may have added.

.. hook-end

.. function:: bool TARGET_IN_SMALL_DATA_P (const_tree exp)

  .. hook-start:TARGET_IN_SMALL_DATA_P

  Returns true if :samp:`{exp}` should be placed into a 'small data' section.
  The default version of this hook always returns false.

.. hook-end

.. c:var:: bool TARGET_HAVE_SRODATA_SECTION

  .. hook-start:TARGET_HAVE_SRODATA_SECTION

  Contains the value true if the target places read-only
  'small data' into a separate section.  The default value is false.

.. hook-end

.. function:: bool TARGET_PROFILE_BEFORE_PROLOGUE (void)

  .. hook-start:TARGET_PROFILE_BEFORE_PROLOGUE

  It returns true if target wants profile code emitted before prologue.

  The default version of this hook use the target macro
  ``PROFILE_BEFORE_PROLOGUE``.

.. hook-end

.. function:: bool TARGET_BINDS_LOCAL_P (const_tree exp)

  .. hook-start:TARGET_BINDS_LOCAL_P

  Returns true if :samp:`{exp}` names an object for which name resolution
  rules must resolve to the current 'module' (dynamic shared library
  or executable image).

  The default version of this hook implements the name resolution rules
  for ELF, which has a looser model of global name binding than other
  currently supported object file formats.

.. hook-end

.. c:var:: bool TARGET_HAVE_TLS

  .. hook-start:TARGET_HAVE_TLS

  Contains the value true if the target supports thread-local storage.
  The default value is false.

.. hook-end