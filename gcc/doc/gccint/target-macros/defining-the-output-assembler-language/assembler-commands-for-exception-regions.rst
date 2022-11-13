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

.. include:: ../tm.rst.in
  :start-after: [TARGET_EXCEPT_UNWIND_INFO]
  :end-before: [TARGET_EXCEPT_UNWIND_INFO]


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

.. include:: ../tm.rst.in
  :start-after: [TARGET_UNWIND_TABLES_DEFAULT]
  :end-before: [TARGET_UNWIND_TABLES_DEFAULT]


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

.. include:: ../tm.rst.in
  :start-after: [TARGET_TERMINATE_DW2_EH_FRAME_INFO]
  :end-before: [TARGET_TERMINATE_DW2_EH_FRAME_INFO]


.. include:: ../tm.rst.in
  :start-after: [TARGET_DWARF_REGISTER_SPAN]
  :end-before: [TARGET_DWARF_REGISTER_SPAN]


.. include:: ../tm.rst.in
  :start-after: [TARGET_DWARF_FRAME_REG_MODE]
  :end-before: [TARGET_DWARF_FRAME_REG_MODE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_INIT_DWARF_REG_SIZES_EXTRA]
  :end-before: [TARGET_INIT_DWARF_REG_SIZES_EXTRA]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ASM_TTYPE]
  :end-before: [TARGET_ASM_TTYPE]


.. include:: ../tm.rst.in
  :start-after: [TARGET_ARM_EABI_UNWINDER]
  :end-before: [TARGET_ARM_EABI_UNWINDER]
