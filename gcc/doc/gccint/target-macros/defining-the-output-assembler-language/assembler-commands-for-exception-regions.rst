..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _exception-region-output:

Assembler Commands for Exception Regions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes commands marking the start and the end of an exception
region.

.. c:macro:: EH_FRAME_SECTION_NAME

  If defined, a C string constant for the name of the section containing
  exception handling frame unwind information.  If not defined, GCC will
  provide a default definition if the target supports named sections.
  :samp:`crtstuff.c` uses this macro to switch to the appropriate section.

  You should define this symbol if your target supports DWARF 2 frame
  unwind information and the default definition does not work.

.. c:macro:: EH_FRAME_THROUGH_COLLECT2

  If defined, DWARF 2 frame unwind information will identified by
  specially named labels.  The collect2 process will locate these
  labels and generate code to register the frames.

  This might be necessary, for instance, if the system linker will not
  place the eh_frames in-between the sentinals from :samp:`crtstuff.c`,
  or if the system linker does garbage collection and sections cannot
  be marked as not to be collected.

.. c:macro:: EH_TABLES_CAN_BE_READ_ONLY

  Define this macro to 1 if your target is such that no frame unwind
  information encoding used with non-PIC code will ever require a
  runtime relocation, but the linker may not support merging read-only
  and read-write sections into a single read-write section.

.. c:macro:: MASK_RETURN_ADDR

  An rtx used to mask the return address found via ``RETURN_ADDR_RTX``, so
  that it does not contain any extraneous set bits in it.

.. c:macro:: DWARF2_UNWIND_INFO

  Define this macro to 0 if your target supports DWARF 2 frame unwind
  information, but it does not yet work with exception handling.
  Otherwise, if your target supports this information (if it defines
  ``INCOMING_RETURN_ADDR_RTX`` and ``OBJECT_FORMAT_ELF``),
  GCC will provide a default definition of 1.

.. function:: enum unwind_info_type TARGET_EXCEPT_UNWIND_INFO (struct gcc_options *opts)

  .. hook-start:TARGET_EXCEPT_UNWIND_INFO

  .. hook-end

  This hook defines the mechanism that will be used for exception handling
  by the target.  If the target has ABI specified unwind tables, the hook
  should return ``UI_TARGET``.  If the target is to use the
  ``setjmp`` / ``longjmp`` -based exception handling scheme, the hook
  should return ``UI_SJLJ``.  If the target supports DWARF 2 frame unwind
  information, the hook should return ``UI_DWARF2``.

  A target may, if exceptions are disabled, choose to return ``UI_NONE``.
  This may end up simplifying other parts of target-specific code.  The
  default implementation of this hook never returns ``UI_NONE``.

  Note that the value returned by this hook should be constant.  It should
  not depend on anything except the command-line switches described by
  :samp:`{opts}`.  In particular, the
  setting ``UI_SJLJ`` must be fixed at compiler start-up as C pre-processor
  macros and builtin functions related to exception handling are set up
  depending on this setting.

  The default implementation of the hook first honors the
  :option:`--enable-sjlj-exceptions` configure option, then
  ``DWARF2_UNWIND_INFO``, and finally defaults to ``UI_SJLJ``.  If
  ``DWARF2_UNWIND_INFO`` depends on command-line options, the target
  must define this hook so that :samp:`{opts}` is used correctly.

.. c:var:: bool TARGET_UNWIND_TABLES_DEFAULT

  .. hook-start:TARGET_UNWIND_TABLES_DEFAULT

  .. hook-end

  This variable should be set to ``true`` if the target ABI requires unwinding
  tables even when exceptions are not used.  It must not be modified by
  command-line option processing.

.. c:macro:: DONT_USE_BUILTIN_SETJMP

  Define this macro to 1 if the ``setjmp`` / ``longjmp`` -based scheme
  should use the ``setjmp`` / ``longjmp`` functions from the C library
  instead of the ``__builtin_setjmp`` / ``__builtin_longjmp`` machinery.

.. c:macro:: JMP_BUF_SIZE

  This macro has no effect unless ``DONT_USE_BUILTIN_SETJMP`` is also
  defined.  Define this macro if the default size of ``jmp_buf`` buffer
  for the ``setjmp`` / ``longjmp`` -based exception handling mechanism
  is not large enough, or if it is much too large.
  The default size is ``FIRST_PSEUDO_REGISTER * sizeof(void *)``.

.. c:macro:: DWARF_CIE_DATA_ALIGNMENT

  This macro need only be defined if the target might save registers in the
  function prologue at an offset to the stack pointer that is not aligned to
  ``UNITS_PER_WORD``.  The definition should be the negative minimum
  alignment if ``STACK_GROWS_DOWNWARD`` is true, and the positive
  minimum alignment otherwise.  See :ref:`dwarf`.  Only applicable if
  the target supports DWARF 2 frame unwind information.

.. c:var:: bool TARGET_TERMINATE_DW2_EH_FRAME_INFO

  .. hook-start:TARGET_TERMINATE_DW2_EH_FRAME_INFO

  Contains the value true if the target should add a zero word onto the
  end of a Dwarf-2 frame info section when used for exception handling.
  Default value is false if ``EH_FRAME_SECTION_NAME`` is defined, and
  true otherwise.

.. hook-end

.. function:: rtx TARGET_DWARF_REGISTER_SPAN (rtx reg)

  .. hook-start:TARGET_DWARF_REGISTER_SPAN

  Given a register, this hook should return a parallel of registers to
  represent where to find the register pieces.  Define this hook if the
  register and its mode are represented in Dwarf in non-contiguous
  locations, or if the register should be represented in more than one
  register in Dwarf.  Otherwise, this hook should return ``NULL_RTX``.
  If not defined, the default is to return ``NULL_RTX``.

.. hook-end

.. function:: machine_mode TARGET_DWARF_FRAME_REG_MODE (int regno)

  .. hook-start:TARGET_DWARF_FRAME_REG_MODE

  Given a register, this hook should return the mode which the
  corresponding Dwarf frame register should have.  This is normally
  used to return a smaller mode than the raw mode to prevent call
  clobbered parts of a register altering the frame register size

.. hook-end

.. function:: void TARGET_INIT_DWARF_REG_SIZES_EXTRA (tree address)

  .. hook-start:TARGET_INIT_DWARF_REG_SIZES_EXTRA

  If some registers are represented in Dwarf-2 unwind information in
  multiple pieces, define this hook to fill in information about the
  sizes of those pieces in the table used by the unwinder at runtime.
  It will be called by ``expand_builtin_init_dwarf_reg_sizes`` after
  filling in a single size corresponding to each hard register;
  :samp:`{address}` is the address of the table.

.. hook-end

.. function:: bool TARGET_ASM_TTYPE (rtx sym)

  .. hook-start:TARGET_ASM_TTYPE

  This hook is used to output a reference from a frame unwinding table to
  the type_info object identified by :samp:`{sym}`.  It should return ``true``
  if the reference was output.  Returning ``false`` will cause the
  reference to be output using the normal Dwarf2 routines.

.. hook-end

.. c:var:: bool TARGET_ARM_EABI_UNWINDER

  .. hook-start:TARGET_ARM_EABI_UNWINDER

  This flag should be set to ``true`` on targets that use an ARM EABI
  based unwinding library, and ``false`` on other targets.  This effects
  the format of unwinding tables, and how the unwinder in entered after
  running a cleanup.  The default is ``false``.

.. hook-end