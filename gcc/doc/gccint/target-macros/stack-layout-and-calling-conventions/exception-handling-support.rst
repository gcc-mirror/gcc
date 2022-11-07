..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: exception handling

.. _exception-handling:

Exception Handling Support
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. c:macro:: EH_RETURN_DATA_REGNO (N)

  A C expression whose value is the :samp:`{N}` th register number used for
  data by exception handlers, or ``INVALID_REGNUM`` if fewer than
  :samp:`{N}` registers are usable.

  The exception handling library routines communicate with the exception
  handlers via a set of agreed upon registers.  Ideally these registers
  should be call-clobbered; it is possible to use call-saved registers,
  but may negatively impact code size.  The target must support at least
  2 data registers, but should define 4 if there are enough free registers.

  You must define this macro if you want to support call frame exception
  handling like that provided by DWARF 2.

.. c:macro:: EH_RETURN_STACKADJ_RTX

  A C expression whose value is RTL representing a location in which
  to store a stack adjustment to be applied before function return.
  This is used to unwind the stack to an exception handler's call frame.
  It will be assigned zero on code paths that return normally.

  Typically this is a call-clobbered hard register that is otherwise
  untouched by the epilogue, but could also be a stack slot.

  Do not define this macro if the stack pointer is saved and restored
  by the regular prolog and epilog code in the call frame itself; in
  this case, the exception handling library routines will update the
  stack location to be restored in place.  Otherwise, you must define
  this macro if you want to support call frame exception handling like
  that provided by DWARF 2.

.. c:macro:: EH_RETURN_HANDLER_RTX

  A C expression whose value is RTL representing a location in which
  to store the address of an exception handler to which we should
  return.  It will not be assigned on code paths that return normally.

  Typically this is the location in the call frame at which the normal
  return address is stored.  For targets that return by popping an
  address off the stack, this might be a memory address just below
  the *target* call frame rather than inside the current call
  frame.  If defined, ``EH_RETURN_STACKADJ_RTX`` will have already
  been assigned, so it may be used to calculate the location of the
  target call frame.

  Some targets have more complex requirements than storing to an
  address calculable during initial code generation.  In that case
  the ``eh_return`` instruction pattern should be used instead.

  If you want to support call frame exception handling, you must
  define either this macro or the ``eh_return`` instruction pattern.

.. c:macro:: RETURN_ADDR_OFFSET

  If defined, an integer-valued C expression for which rtl will be generated
  to add it to the exception handler address before it is searched in the
  exception handling tables, and to subtract it again from the address before
  using it to return to the exception handler.

.. c:macro:: ASM_PREFERRED_EH_DATA_FORMAT (code, global)

  This macro chooses the encoding of pointers embedded in the exception
  handling sections.  If at all possible, this should be defined such
  that the exception handling section will not require dynamic relocations,
  and so may be read-only.

  :samp:`{code}` is 0 for data, 1 for code labels, 2 for function pointers.
  :samp:`{global}` is true if the symbol may be affected by dynamic relocations.
  The macro should return a combination of the ``DW_EH_PE_*`` defines
  as found in :samp:`dwarf2.h`.

  If this macro is not defined, pointers will not be encoded but
  represented directly.

.. c:macro:: ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX (file, encoding, size, addr, done)

  This macro allows the target to emit whatever special magic is required
  to represent the encoding chosen by ``ASM_PREFERRED_EH_DATA_FORMAT``.
  Generic code takes care of pc-relative and indirect encodings; this must
  be defined if the target uses text-relative or data-relative encodings.

  This is a C statement that branches to :samp:`{done}` if the format was
  handled.  :samp:`{encoding}` is the format chosen, :samp:`{size}` is the number
  of bytes that the format occupies, :samp:`{addr}` is the ``SYMBOL_REF``
  to be emitted.

.. c:macro:: MD_FALLBACK_FRAME_STATE_FOR (context, fs)

  This macro allows the target to add CPU and operating system specific
  code to the call-frame unwinder for use when there is no unwind data
  available.  The most common reason to implement this macro is to unwind
  through signal frames.

  This macro is called from ``uw_frame_state_for`` in
  :samp:`unwind-dw2.c`, :samp:`unwind-dw2-xtensa.c` and
  :samp:`unwind-ia64.c`.  :samp:`{context}` is an ``_Unwind_Context`` ;
  :samp:`{fs}` is an ``_Unwind_FrameState``.  Examine ``context->ra``
  for the address of the code being executed and ``context->cfa`` for
  the stack pointer value.  If the frame can be decoded, the register
  save addresses should be updated in :samp:`{fs}` and the macro should
  evaluate to ``_URC_NO_REASON``.  If the frame cannot be decoded,
  the macro should evaluate to ``_URC_END_OF_STACK``.

  For proper signal handling in Java this macro is accompanied by
  ``MAKE_THROW_FRAME``, defined in :samp:`libjava/include/*-signal.h` headers.

.. c:macro:: MD_HANDLE_UNWABI (context, fs)

  This macro allows the target to add operating system specific code to the
  call-frame unwinder to handle the IA-64 ``.unwabi`` unwinding directive,
  usually used for signal or interrupt frames.

  This macro is called from ``uw_update_context`` in libgcc's
  :samp:`unwind-ia64.c`.  :samp:`{context}` is an ``_Unwind_Context`` ;
  :samp:`{fs}` is an ``_Unwind_FrameState``.  Examine ``fs->unwabi``
  for the abi and context in the ``.unwabi`` directive.  If the
  ``.unwabi`` directive can be handled, the register save addresses should
  be updated in :samp:`{fs}`.

.. c:macro:: TARGET_USES_WEAK_UNWIND_INFO

  A C expression that evaluates to true if the target requires unwind
  info to be given comdat linkage.  Define it to be ``1`` if comdat
  linkage is necessary.  The default is ``0``.