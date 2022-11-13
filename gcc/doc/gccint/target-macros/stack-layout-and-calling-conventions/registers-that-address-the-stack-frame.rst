..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _frame-registers:

Registers That Address the Stack Frame
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This discusses registers that address the stack frame.

.. c:macro:: STACK_POINTER_REGNUM

  The register number of the stack pointer register, which must also be a
  fixed register according to ``FIXED_REGISTERS``.  On most machines,
  the hardware determines which register this is.

.. c:macro:: FRAME_POINTER_REGNUM

  The register number of the frame pointer register, which is used to
  access automatic variables in the stack frame.  On some machines, the
  hardware determines which register this is.  On other machines, you can
  choose any register you wish for this purpose.

.. c:macro:: HARD_FRAME_POINTER_REGNUM

  On some machines the offset between the frame pointer and starting
  offset of the automatic variables is not known until after register
  allocation has been done (for example, because the saved registers are
  between these two locations).  On those machines, define
  ``FRAME_POINTER_REGNUM`` the number of a special, fixed register to
  be used internally until the offset is known, and define
  ``HARD_FRAME_POINTER_REGNUM`` to be the actual hard register number
  used for the frame pointer.

  You should define this macro only in the very rare circumstances when it
  is not possible to calculate the offset between the frame pointer and
  the automatic variables until after register allocation has been
  completed.  When this macro is defined, you must also indicate in your
  definition of ``ELIMINABLE_REGS`` how to eliminate
  ``FRAME_POINTER_REGNUM`` into either ``HARD_FRAME_POINTER_REGNUM``
  or ``STACK_POINTER_REGNUM``.

  Do not define this macro if it would be the same as
  ``FRAME_POINTER_REGNUM``.

.. c:macro:: ARG_POINTER_REGNUM

  The register number of the arg pointer register, which is used to access
  the function's argument list.  On some machines, this is the same as the
  frame pointer register.  On some machines, the hardware determines which
  register this is.  On other machines, you can choose any register you
  wish for this purpose.  If this is not the same register as the frame
  pointer register, then you must mark it as a fixed register according to
  ``FIXED_REGISTERS``, or arrange to be able to eliminate it
  (see :ref:`elimination`).

.. c:macro:: HARD_FRAME_POINTER_IS_FRAME_POINTER

  Define this to a preprocessor constant that is nonzero if
  ``hard_frame_pointer_rtx`` and ``frame_pointer_rtx`` should be
  the same.  The default definition is :samp:`(HARD_FRAME_POINTER_REGNUM
  == FRAME_POINTER_REGNUM)`; you only need to define this macro if that
  definition is not suitable for use in preprocessor conditionals.

.. c:macro:: HARD_FRAME_POINTER_IS_ARG_POINTER

  Define this to a preprocessor constant that is nonzero if
  ``hard_frame_pointer_rtx`` and ``arg_pointer_rtx`` should be the
  same.  The default definition is :samp:`(HARD_FRAME_POINTER_REGNUM ==
  ARG_POINTER_REGNUM)`; you only need to define this macro if that
  definition is not suitable for use in preprocessor conditionals.

.. c:macro:: RETURN_ADDRESS_POINTER_REGNUM

  The register number of the return address pointer register, which is used to
  access the current function's return address from the stack.  On some
  machines, the return address is not at a fixed offset from the frame
  pointer or stack pointer or argument pointer.  This register can be defined
  to point to the return address on the stack, and then be converted by
  ``ELIMINABLE_REGS`` into either the frame pointer or stack pointer.

  Do not define this macro unless there is no other way to get the return
  address from the stack.

.. c:macro:: STATIC_CHAIN_REGNUM

.. c:macro:: STATIC_CHAIN_INCOMING_REGNUM

  Register numbers used for passing a function's static chain pointer.  If
  register windows are used, the register number as seen by the called
  function is ``STATIC_CHAIN_INCOMING_REGNUM``, while the register
  number as seen by the calling function is ``STATIC_CHAIN_REGNUM``.  If
  these registers are the same, ``STATIC_CHAIN_INCOMING_REGNUM`` need
  not be defined.

  The static chain register need not be a fixed register.

  If the static chain is passed in memory, these macros should not be
  defined; instead, the ``TARGET_STATIC_CHAIN`` hook should be used.

.. function:: rtx TARGET_STATIC_CHAIN (const_tree fndecl_or_type, bool incoming_p)

  .. hook-start:TARGET_STATIC_CHAIN

  This hook replaces the use of ``STATIC_CHAIN_REGNUM`` et al for
  targets that may use different static chain locations for different
  nested functions.  This may be required if the target has function
  attributes that affect the calling conventions of the function and
  those calling conventions use different static chain locations.

  The default version of this hook uses ``STATIC_CHAIN_REGNUM`` et al.

  If the static chain is passed in memory, this hook should be used to
  provide rtx giving ``mem`` expressions that denote where they are stored.
  Often the ``mem`` expression as seen by the caller will be at an offset
  from the stack pointer and the ``mem`` expression as seen by the callee
  will be at an offset from the frame pointer.

  .. index:: stack_pointer_rtx, frame_pointer_rtx, arg_pointer_rtx

  The variables ``stack_pointer_rtx``, ``frame_pointer_rtx``, and
  ``arg_pointer_rtx`` will have been initialized and should be used
  to refer to those items.

.. hook-end

.. c:macro:: DWARF_FRAME_REGISTERS

  This macro specifies the maximum number of hard registers that can be
  saved in a call frame.  This is used to size data structures used in
  DWARF2 exception handling.

  Prior to GCC 3.0, this macro was needed in order to establish a stable
  exception handling ABI in the face of adding new hard registers for ISA
  extensions.  In GCC 3.0 and later, the EH ABI is insulated from changes
  in the number of hard registers.  Nevertheless, this macro can still be
  used to reduce the runtime memory requirements of the exception handling
  routines, which can be substantial if the ISA contains a lot of
  registers that are not call-saved.

  If this macro is not defined, it defaults to
  ``FIRST_PSEUDO_REGISTER``.

.. c:macro:: PRE_GCC3_DWARF_FRAME_REGISTERS

  This macro is similar to ``DWARF_FRAME_REGISTERS``, but is provided
  for backward compatibility in pre GCC 3.0 compiled code.

  If this macro is not defined, it defaults to
  ``DWARF_FRAME_REGISTERS``.

.. c:macro:: DWARF_REG_TO_UNWIND_COLUMN (regno)

  Define this macro if the target's representation for dwarf registers
  is different than the internal representation for unwind column.
  Given a dwarf register, this macro should return the internal unwind
  column number to use instead.

.. c:macro:: DWARF_FRAME_REGNUM (regno)

  Define this macro if the target's representation for dwarf registers
  used in .eh_frame or .debug_frame is different from that used in other
  debug info sections.  Given a GCC hard register number, this macro
  should return the .eh_frame register number.  The default is
  ``DEBUGGER_REGNO (regno)``.

.. c:macro:: DWARF2_FRAME_REG_OUT (regno, for_eh)

  Define this macro to map register numbers held in the call frame info
  that GCC has collected using ``DWARF_FRAME_REGNUM`` to those that
  should be output in .debug_frame (``for_eh`` is zero) and
  .eh_frame (``for_eh`` is nonzero).  The default is to
  return ``regno``.

.. c:macro:: REG_VALUE_IN_UNWIND_CONTEXT

  Define this macro if the target stores register values as
  ``_Unwind_Word`` type in unwind context.  It should be defined if
  target register size is larger than the size of ``void *``.  The
  default is to store register values as ``void *`` type.

.. c:macro:: ASSUME_EXTENDED_UNWIND_CONTEXT

  Define this macro to be 1 if the target always uses extended unwind
  context with version, args_size and by_value fields.  If it is undefined,
  it will be defined to 1 when ``REG_VALUE_IN_UNWIND_CONTEXT`` is
  defined and 0 otherwise.

.. c:macro:: DWARF_LAZY_REGISTER_VALUE (regno, value)

  Define this macro if the target has pseudo DWARF registers whose
  values need to be computed lazily on demand by the unwinder (such as when
  referenced in a CFA expression).  The macro returns true if :samp:`{regno}`
  is such a register and stores its value in :samp:`*{value}` if so.