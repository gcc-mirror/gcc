..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: stack frame layout, frame layout

.. _frame-layout:

Basic Stack Layout
^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here is the basic stack layout.

.. c:macro:: STACK_GROWS_DOWNWARD

  Define this macro to be true if pushing a word onto the stack moves the stack
  pointer to a smaller address, and false otherwise.

.. c:macro:: STACK_PUSH_CODE

  This macro defines the operation used when something is pushed
  on the stack.  In RTL, a push operation will be
  ``(set (mem (STACK_PUSH_CODE (reg sp))) ...)``

  The choices are ``PRE_DEC``, ``POST_DEC``, ``PRE_INC``,
  and ``POST_INC``.  Which of these is correct depends on
  the stack direction and on whether the stack pointer points
  to the last item on the stack or whether it points to the
  space for the next item on the stack.

  The default is ``PRE_DEC`` when ``STACK_GROWS_DOWNWARD`` is
  true, which is almost always right, and ``PRE_INC`` otherwise,
  which is often wrong.

.. c:macro:: FRAME_GROWS_DOWNWARD

  Define this macro to nonzero value if the addresses of local variable slots
  are at negative offsets from the frame pointer.

.. c:macro:: ARGS_GROW_DOWNWARD

  Define this macro if successive arguments to a function occupy decreasing
  addresses on the stack.

.. function:: HOST_WIDE_INT TARGET_STARTING_FRAME_OFFSET (void)

  .. hook-start:TARGET_STARTING_FRAME_OFFSET

  This hook returns the offset from the frame pointer to the first local
  variable slot to be allocated.  If ``FRAME_GROWS_DOWNWARD``, it is the
  offset to *end* of the first slot allocated, otherwise it is the
  offset to *beginning* of the first slot allocated.  The default
  implementation returns 0.

.. hook-end

.. c:macro:: STACK_ALIGNMENT_NEEDED

  Define to zero to disable final alignment of the stack during reload.
  The nonzero default for this macro is suitable for most ports.

  On ports where ``TARGET_STARTING_FRAME_OFFSET`` is nonzero or where there
  is a register save block following the local block that doesn't require
  alignment to ``STACK_BOUNDARY``, it may be beneficial to disable
  stack alignment and do it in the backend.

.. c:macro:: STACK_POINTER_OFFSET

  Offset from the stack pointer register to the first location at which
  outgoing arguments are placed.  If not specified, the default value of
  zero is used.  This is the proper value for most machines.

  If ``ARGS_GROW_DOWNWARD``, this is the offset to the location above
  the first location at which outgoing arguments are placed.

.. c:macro:: FIRST_PARM_OFFSET (fundecl)

  Offset from the argument pointer register to the first argument's
  address.  On some machines it may depend on the data type of the
  function.

  If ``ARGS_GROW_DOWNWARD``, this is the offset to the location above
  the first argument's address.

.. c:macro:: STACK_DYNAMIC_OFFSET (fundecl)

  Offset from the stack pointer register to an item dynamically allocated
  on the stack, e.g., by ``alloca``.

  The default value for this macro is ``STACK_POINTER_OFFSET`` plus the
  length of the outgoing arguments.  The default is correct for most
  machines.  See :samp:`function.cc` for details.

.. c:macro:: INITIAL_FRAME_ADDRESS_RTX

  A C expression whose value is RTL representing the address of the initial
  stack frame. This address is passed to ``RETURN_ADDR_RTX`` and
  ``DYNAMIC_CHAIN_ADDRESS``.  If you don't define this macro, a reasonable
  default value will be used.  Define this macro in order to make frame pointer
  elimination work in the presence of ``__builtin_frame_address (count)`` and
  ``__builtin_return_address (count)`` for ``count`` not equal to zero.

.. c:macro:: DYNAMIC_CHAIN_ADDRESS (frameaddr)

  A C expression whose value is RTL representing the address in a stack
  frame where the pointer to the caller's frame is stored.  Assume that
  :samp:`{frameaddr}` is an RTL expression for the address of the stack frame
  itself.

  If you don't define this macro, the default is to return the value
  of :samp:`{frameaddr}` ---that is, the stack frame address is also the
  address of the stack word that points to the previous frame.

.. c:macro:: SETUP_FRAME_ADDRESSES

  A C expression that produces the machine-specific code to
  setup the stack so that arbitrary frames can be accessed.  For example,
  on the SPARC, we must flush all of the register windows to the stack
  before we can access arbitrary stack frames.  You will seldom need to
  define this macro.  The default is to do nothing.

.. function:: rtx TARGET_BUILTIN_SETJMP_FRAME_VALUE (void)

  .. hook-start:TARGET_BUILTIN_SETJMP_FRAME_VALUE

  This target hook should return an rtx that is used to store
  the address of the current frame into the built in ``setjmp`` buffer.
  The default value, ``virtual_stack_vars_rtx``, is correct for most
  machines.  One reason you may need to define this target hook is if
  ``hard_frame_pointer_rtx`` is the appropriate value on your machine.

.. hook-end

.. c:macro:: FRAME_ADDR_RTX (frameaddr)

  A C expression whose value is RTL representing the value of the frame
  address for the current frame.  :samp:`{frameaddr}` is the frame pointer
  of the current frame.  This is used for __builtin_frame_address.
  You need only define this macro if the frame address is not the same
  as the frame pointer.  Most machines do not need to define it.

.. c:macro:: RETURN_ADDR_RTX (count, frameaddr)

  A C expression whose value is RTL representing the value of the return
  address for the frame :samp:`{count}` steps up from the current frame, after
  the prologue.  :samp:`{frameaddr}` is the frame pointer of the :samp:`{count}`
  frame, or the frame pointer of the :samp:`{count}` - 1 frame if
  ``RETURN_ADDR_IN_PREVIOUS_FRAME`` is nonzero.

  The value of the expression must always be the correct address when
  :samp:`{count}` is zero, but may be ``NULL_RTX`` if there is no way to
  determine the return address of other frames.

.. c:macro:: RETURN_ADDR_IN_PREVIOUS_FRAME

  Define this macro to nonzero value if the return address of a particular
  stack frame is accessed from the frame pointer of the previous stack
  frame.  The zero default for this macro is suitable for most ports.

.. c:macro:: INCOMING_RETURN_ADDR_RTX

  A C expression whose value is RTL representing the location of the
  incoming return address at the beginning of any function, before the
  prologue.  This RTL is either a ``REG``, indicating that the return
  value is saved in :samp:`REG`, or a ``MEM`` representing a location in
  the stack.

  You only need to define this macro if you want to support call frame
  debugging information like that provided by DWARF 2.

  If this RTL is a ``REG``, you should also define
  ``DWARF_FRAME_RETURN_COLUMN`` to ``DWARF_FRAME_REGNUM (REGNO)``.

.. c:macro:: DWARF_ALT_FRAME_RETURN_COLUMN

  A C expression whose value is an integer giving a DWARF 2 column
  number that may be used as an alternative return column.  The column
  must not correspond to any gcc hard register (that is, it must not
  be in the range of ``DWARF_FRAME_REGNUM``).

  This macro can be useful if ``DWARF_FRAME_RETURN_COLUMN`` is set to a
  general register, but an alternative column needs to be used for signal
  frames.  Some targets have also used different frame return columns
  over time.

.. c:macro:: DWARF_ZERO_REG

  A C expression whose value is an integer giving a DWARF 2 register
  number that is considered to always have the value zero.  This should
  only be defined if the target has an architected zero register, and
  someone decided it was a good idea to use that register number to
  terminate the stack backtrace.  New ports should avoid this.

.. c:macro:: DWARF_VERSION_DEFAULT

  A C expression whose value is the default dwarf standard version we'll honor
  and advertise when generating dwarf debug information, in absence of
  an explicit :option:`-gdwarf-version` option on the command line.

.. function:: void TARGET_DWARF_HANDLE_FRAME_UNSPEC (const char *label, rtx pattern, int index)

  .. hook-start:TARGET_DWARF_HANDLE_FRAME_UNSPEC

  This target hook allows the backend to emit frame-related insns that
  contain UNSPECs or UNSPEC_VOLATILEs.  The DWARF 2 call frame debugging
  info engine will invoke it on insns of the form

  .. code-block:: c++

    (set (reg) (unspec [...] UNSPEC_INDEX))

  and

  .. code-block:: c++

    (set (reg) (unspec_volatile [...] UNSPECV_INDEX)).

  to let the backend emit the call frame instructions.  :samp:`{label}` is
  the CFI label attached to the insn, :samp:`{pattern}` is the pattern of
  the insn and :samp:`{index}` is ``UNSPEC_INDEX`` or ``UNSPECV_INDEX``.

.. hook-end

.. function:: unsigned int TARGET_DWARF_POLY_INDETERMINATE_VALUE (unsigned int i, unsigned int *factor, int *offset)

  .. hook-start:TARGET_DWARF_POLY_INDETERMINATE_VALUE

  Express the value of ``poly_int`` indeterminate :samp:`{i}` as a DWARF
  expression, with :samp:`{i}` counting from 1.  Return the number of a DWARF
  register :samp:`{R}` and set :samp:`*{factor}` and :samp:`*{offset}` such
  that the value of the indeterminate is:

  .. code-block:: c++

    value_of(R) / factor - offset

  A target only needs to define this hook if it sets
  :samp:`NUM_POLY_INT_COEFFS` to a value greater than 1.

.. hook-end

.. c:macro:: INCOMING_FRAME_SP_OFFSET

  A C expression whose value is an integer giving the offset, in bytes,
  from the value of the stack pointer register to the top of the stack
  frame at the beginning of any function, before the prologue.  The top of
  the frame is defined to be the value of the stack pointer in the
  previous frame, just before the call instruction.

  You only need to define this macro if you want to support call frame
  debugging information like that provided by DWARF 2.

.. c:macro:: DEFAULT_INCOMING_FRAME_SP_OFFSET

  Like ``INCOMING_FRAME_SP_OFFSET``, but must be the same for all
  functions of the same ABI, and when using GAS ``.cfi_*`` directives
  must also agree with the default CFI GAS emits.  Define this macro
  only if ``INCOMING_FRAME_SP_OFFSET`` can have different values
  between different functions of the same ABI or when
  ``INCOMING_FRAME_SP_OFFSET`` does not agree with GAS default CFI.

.. c:macro:: ARG_POINTER_CFA_OFFSET (fundecl)

  A C expression whose value is an integer giving the offset, in bytes,
  from the argument pointer to the canonical frame address (cfa).  The
  final value should coincide with that calculated by
  ``INCOMING_FRAME_SP_OFFSET``.  Which is unfortunately not usable
  during virtual register instantiation.

  The default value for this macro is
  ``FIRST_PARM_OFFSET (fundecl) + crtl->args.pretend_args_size``,
  which is correct for most machines; in general, the arguments are found
  immediately before the stack frame.  Note that this is not the case on
  some targets that save registers into the caller's frame, such as SPARC
  and rs6000, and so such targets need to define this macro.

  You only need to define this macro if the default is incorrect, and you
  want to support call frame debugging information like that provided by
  DWARF 2.

.. c:macro:: FRAME_POINTER_CFA_OFFSET (fundecl)

  If defined, a C expression whose value is an integer giving the offset
  in bytes from the frame pointer to the canonical frame address (cfa).
  The final value should coincide with that calculated by
  ``INCOMING_FRAME_SP_OFFSET``.

  Normally the CFA is calculated as an offset from the argument pointer,
  via ``ARG_POINTER_CFA_OFFSET``, but if the argument pointer is
  variable due to the ABI, this may not be possible.  If this macro is
  defined, it implies that the virtual register instantiation should be
  based on the frame pointer instead of the argument pointer.  Only one
  of ``FRAME_POINTER_CFA_OFFSET`` and ``ARG_POINTER_CFA_OFFSET``
  should be defined.

.. c:macro:: CFA_FRAME_BASE_OFFSET (fundecl)

  If defined, a C expression whose value is an integer giving the offset
  in bytes from the canonical frame address (cfa) to the frame base used
  in DWARF 2 debug information.  The default is zero.  A different value
  may reduce the size of debug information on some ports.