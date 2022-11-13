..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _debugging-info:

Controlling Debugging Information Format
****************************************

.. prevent bad page break with this line

This describes how to specify debugging information.

.. toctree::
  :maxdepth: 2

.. _all-debuggers:

Macros Affecting All Debugging Formats
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

These macros affect all debugging formats.

.. c:macro:: DEBUGGER_REGNO (regno)

  A C expression that returns the debugger register number for the compiler
  register number :samp:`{regno}`.  In the default macro provided, the value
  of this expression will be :samp:`{regno}` itself.  But sometimes there are
  some registers that the compiler knows about and debugger does not, or vice
  versa.  In such cases, some register may need to have one number in the
  compiler and another for debugger.

  If two registers have consecutive numbers inside GCC, and they can be
  used as a pair to hold a multiword value, then they *must* have
  consecutive numbers after renumbering with ``DEBUGGER_REGNO``.
  Otherwise, debuggers will be unable to access such a pair, because they
  expect register pairs to be consecutive in their own numbering scheme.

  If you find yourself defining ``DEBUGGER_REGNO`` in way that
  does not preserve register pairs, then what you must do instead is
  redefine the actual register numbering scheme.

.. c:macro:: DEBUGGER_AUTO_OFFSET (x)

  A C expression that returns the integer offset value for an automatic
  variable having address :samp:`{x}` (an RTL expression).  The default
  computation assumes that :samp:`{x}` is based on the frame-pointer and
  gives the offset from the frame-pointer.  This is required for targets
  that produce debugging output for debugger and allow the frame-pointer to be
  eliminated when the :option:`-g` option is used.

.. c:macro:: DEBUGGER_ARG_OFFSET (offset, x)

  A C expression that returns the integer offset value for an argument
  having address :samp:`{x}` (an RTL expression).  The nominal offset is
  :samp:`{offset}`.

.. c:macro:: PREFERRED_DEBUGGING_TYPE

  A C expression that returns the type of debugging output GCC should
  produce when the user specifies just :option:`-g`.  Define
  this if you have arranged for GCC to support more than one format of
  debugging output.  Currently, the allowable values are
  ``DWARF2_DEBUG``, ``VMS_DEBUG``,
  and ``VMS_AND_DWARF2_DEBUG``.

  When the user specifies :option:`-ggdb`, GCC normally also uses the
  value of this macro to select the debugging output format, but with two
  exceptions.  If ``DWARF2_DEBUGGING_INFO`` is defined, GCC uses the
  value ``DWARF2_DEBUG``.

  The value of this macro only affects the default debugging output; the
  user can always get a specific type of output by using  :option:`-gdwarf-2`,
  or :option:`-gvms`.

.. c:macro:: DEFAULT_GDB_EXTENSIONS

  Define this macro to control whether GCC should by default generate
  GDB's extended version of debugging information.  If you don't define the
  macro, the default is 1: always generate the extended information
  if there is any occasion to.

.. _dwarf:

Macros for DWARF Output
^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for DWARF output.

.. c:macro:: DWARF2_DEBUGGING_INFO

  Define this macro if GCC should produce dwarf version 2 format
  debugging output in response to the :option:`-g` option.

  To support optional call frame debugging information, you must also
  define ``INCOMING_RETURN_ADDR_RTX`` and either set
  ``RTX_FRAME_RELATED_P`` on the prologue insns if you use RTL for the
  prologue, or call ``dwarf2out_def_cfa`` and ``dwarf2out_reg_save``
  as appropriate from ``TARGET_ASM_FUNCTION_PROLOGUE`` if you don't.

.. function:: int TARGET_DWARF_CALLING_CONVENTION (const_tree function)

    .. hook-start:TARGET_DWARF_CALLING_CONVENTION

    Define this to enable the dwarf attribute ``DW_AT_calling_convention`` to
    be emitted for each function.  Instead of an integer return the enum
    value for the ``DW_CC_`` tag.

.. hook-end

.. function:: int TARGET_DWARF_CALLING_CONVENTION (const_tree function)

  Define this to enable the dwarf attribute ``DW_AT_calling_convention`` to
  be emitted for each function.  Instead of an integer return the enum
  value for the ``DW_CC_`` tag.

.. c:macro:: DWARF2_FRAME_INFO

  Define this macro to a nonzero value if GCC should always output
  Dwarf 2 frame information.  If ``TARGET_EXCEPT_UNWIND_INFO``
  (see :ref:`exception-region-output`) returns ``UI_DWARF2``, and
  exceptions are enabled, GCC will output this information not matter
  how you define ``DWARF2_FRAME_INFO``.

.. function:: enum unwind_info_type TARGET_DEBUG_UNWIND_INFO (void)

  .. hook-start:TARGET_DEBUG_UNWIND_INFO

  This hook defines the mechanism that will be used for describing frame
  unwind information to the debugger.  Normally the hook will return
  ``UI_DWARF2`` if DWARF 2 debug information is enabled, and
  return ``UI_NONE`` otherwise.

  A target may return ``UI_DWARF2`` even when DWARF 2 debug information
  is disabled in order to always output DWARF 2 frame information.

  A target may return ``UI_TARGET`` if it has ABI specified unwind tables.
  This will suppress generation of the normal debug frame unwind information.

.. hook-end

.. c:macro:: DWARF2_ASM_LINE_DEBUG_INFO

  Define this macro to be a nonzero value if the assembler can generate Dwarf 2
  line debug info sections.  This will result in much more compact line number
  tables, and hence is desirable if it works.

.. c:macro:: DWARF2_ASM_VIEW_DEBUG_INFO

  Define this macro to be a nonzero value if the assembler supports view
  assignment and verification in ``.loc``.  If it does not, but the
  user enables location views, the compiler may have to fallback to
  internal line number tables.

.. function:: int TARGET_RESET_LOCATION_VIEW (rtx_insn *)

  .. hook-start:TARGET_RESET_LOCATION_VIEW

  This hook, if defined, enables -ginternal-reset-location-views, and
  uses its result to override cases in which the estimated min insn
  length might be nonzero even when a PC advance (i.e., a view reset)
  cannot be taken for granted.

  If the hook is defined, it must return a positive value to indicate
  the insn definitely advances the PC, and so the view number can be
  safely assumed to be reset; a negative value to mean the insn
  definitely does not advance the PC, and os the view number must not
  be reset; or zero to decide based on the estimated insn length.

  If insn length is to be regarded as reliable, set the hook to
  ``hook_int_rtx_insn_0``.

.. hook-end

.. c:var:: bool TARGET_WANT_DEBUG_PUB_SECTIONS

  .. hook-start:TARGET_WANT_DEBUG_PUB_SECTIONS

  True if the ``.debug_pubtypes`` and ``.debug_pubnames`` sections
  should be emitted.  These sections are not used on most platforms, and
  in particular GDB does not use them.

.. hook-end

.. c:var:: bool TARGET_DELAY_SCHED2

  .. hook-start:TARGET_DELAY_SCHED2

  True if sched2 is not to be run at its normal place.
  This usually means it will be run as part of machine-specific reorg.

.. hook-end

.. c:var:: bool TARGET_DELAY_VARTRACK

  .. hook-start:TARGET_DELAY_VARTRACK

  True if vartrack is not to be run at its normal place.
  This usually means it will be run as part of machine-specific reorg.

.. hook-end

.. c:var:: bool TARGET_NO_REGISTER_ALLOCATION

  .. hook-start:TARGET_NO_REGISTER_ALLOCATION

  True if register allocation and the passes
  following it should not be run.  Usually true only for virtual assembler
  targets.

.. hook-end

.. c:macro:: ASM_OUTPUT_DWARF_DELTA (stream, size, label1, label2)

  A C statement to issue assembly directives that create a difference
  :samp:`{lab1}` minus :samp:`{lab2}`, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_VMS_DELTA (stream, size, label1, label2)

  A C statement to issue assembly directives that create a difference
  between the two given labels in system defined units, e.g. instruction
  slots on IA64 VMS, using an integer of the given size.

.. c:macro:: ASM_OUTPUT_DWARF_OFFSET (stream, size, label, offset, section)

  A C statement to issue assembly directives that create a
  section-relative reference to the given :samp:`{label}` plus :samp:`{offset}`, using
  an integer of the given :samp:`{size}`.  The label is known to be defined in the
  given :samp:`{section}`.

.. c:macro:: ASM_OUTPUT_DWARF_PCREL (stream, size, label)

  A C statement to issue assembly directives that create a self-relative
  reference to the given :samp:`{label}`, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_DATAREL (stream, size, label)

  A C statement to issue assembly directives that create a reference to the
  given :samp:`{label}` relative to the dbase, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_TABLE_REF (label)

  A C statement to issue assembly directives that create a reference to
  the DWARF table identifier :samp:`{label}` from the current section.  This
  is used on some systems to avoid garbage collecting a DWARF table which
  is referenced by a function.

.. function:: void TARGET_ASM_OUTPUT_DWARF_DTPREL (FILE *file, int size, rtx x)

  .. hook-start:TARGET_ASM_OUTPUT_DWARF_DTPREL

  If defined, this target hook is a function which outputs a DTP-relative
  reference to the given TLS symbol of the specified size.

.. hook-end

.. _vms-debug:

Macros for VMS Debug Format
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for VMS debug format.

.. c:macro:: VMS_DEBUGGING_INFO

  Define this macro if GCC should produce debugging output for VMS
  in response to the :option:`-g` option.  The default behavior for VMS
  is to generate minimal debug info for a traceback in the absence of
  :option:`-g` unless explicitly overridden with :option:`-g0`.  This
  behavior is controlled by ``TARGET_OPTION_OPTIMIZATION`` and
  ``TARGET_OPTION_OVERRIDE``.

.. _ctf-debug:

Macros for CTF Debug Format
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for CTF debug format.

.. c:macro:: CTF_DEBUGGING_INFO

  Define this macro if GCC should produce debugging output in CTF debug
  format in response to the :option:`-gctf` option.

.. _btf-debug:

Macros for BTF Debug Format
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for BTF debug format.

.. c:macro:: BTF_DEBUGGING_INFO

  Define this macro if GCC should produce debugging output in BTF debug
  format in response to the :option:`-gbtf` option.