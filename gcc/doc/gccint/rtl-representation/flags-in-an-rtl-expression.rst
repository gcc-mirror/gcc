..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: flags in RTL expression

.. _flags:

Flags in an RTL Expression
**************************

RTL expressions contain several flags (one-bit bit-fields)
that are used in certain types of expression.  Most often they
are accessed with the following macros, which expand into lvalues.

.. index:: CROSSING_JUMP_P, jump_insn and /j

:samp:`CROSSING_JUMP_P ({x})`
  Nonzero in a ``jump_insn`` if it crosses between hot and cold sections,
  which could potentially be very far apart in the executable.  The presence
  of this flag indicates to other optimizations that this branching instruction
  should not be 'collapsed' into a simpler branching construct.  It is used
  when the optimization to partition basic blocks into hot and cold sections
  is turned on.

  .. index:: CONSTANT_POOL_ADDRESS_P, symbol_ref and /u, unchanging, in symbol_ref

:samp:`CONSTANT_POOL_ADDRESS_P ({x})`
  Nonzero in a ``symbol_ref`` if it refers to part of the current
  function's constant pool.  For most targets these addresses are in a
  ``.rodata`` section entirely separate from the function, but for
  some targets the addresses are close to the beginning of the function.
  In either case GCC assumes these addresses can be addressed directly,
  perhaps with the help of base registers.
  Stored in the ``unchanging`` field and printed as :samp:`/u`.

  .. index:: INSN_ANNULLED_BRANCH_P, jump_insn and /u, call_insn and /u, insn and /u, unchanging, in jump_insn, call_insn and insn

:samp:`INSN_ANNULLED_BRANCH_P ({x})`
  In a ``jump_insn``, ``call_insn``, or ``insn`` indicates
  that the branch is an annulling one.  See the discussion under
  ``sequence`` below.  Stored in the ``unchanging`` field and
  printed as :samp:`/u`.

  .. index:: INSN_DELETED_P, insn and /v, call_insn and /v, jump_insn and /v, code_label and /v, jump_table_data and /v, barrier and /v, note and /v, volatil, in insn, call_insn, jump_insn, code_label, jump_table_data, barrier, and note

:samp:`INSN_DELETED_P ({x})`
  In an ``insn``, ``call_insn``, ``jump_insn``, ``code_label``,
  ``jump_table_data``, ``barrier``, or ``note``,
  nonzero if the insn has been deleted.  Stored in the
  ``volatil`` field and printed as :samp:`/v`.

  .. index:: INSN_FROM_TARGET_P, insn and /s, jump_insn and /s, call_insn and /s, in_struct, in insn and jump_insn and call_insn

:samp:`INSN_FROM_TARGET_P ({x})`
  In an ``insn`` or ``jump_insn`` or ``call_insn`` in a delay
  slot of a branch, indicates that the insn
  is from the target of the branch.  If the branch insn has
  ``INSN_ANNULLED_BRANCH_P`` set, this insn will only be executed if
  the branch is taken.  For annulled branches with
  ``INSN_FROM_TARGET_P`` clear, the insn will be executed only if the
  branch is not taken.  When ``INSN_ANNULLED_BRANCH_P`` is not set,
  this insn will always be executed.  Stored in the ``in_struct``
  field and printed as :samp:`/s`.

  .. index:: LABEL_PRESERVE_P, code_label and /i, note and /i, in_struct, in code_label and note

:samp:`LABEL_PRESERVE_P ({x})`
  In a ``code_label`` or ``note``, indicates that the label is referenced by
  code or data not visible to the RTL of a given function.
  Labels referenced by a non-local goto will have this bit set.  Stored
  in the ``in_struct`` field and printed as :samp:`/s`.

  .. index:: LABEL_REF_NONLOCAL_P, label_ref and /v, reg_label and /v, volatil, in label_ref and reg_label

:samp:`LABEL_REF_NONLOCAL_P ({x})`
  In ``label_ref`` and ``reg_label`` expressions, nonzero if this is
  a reference to a non-local label.
  Stored in the ``volatil`` field and printed as :samp:`/v`.

  .. index:: MEM_KEEP_ALIAS_SET_P, mem and /j, jump, in mem

:samp:`MEM_KEEP_ALIAS_SET_P ({x})`
  In ``mem`` expressions, 1 if we should keep the alias set for this
  mem unchanged when we access a component.  Set to 1, for example, when we
  are already in a non-addressable component of an aggregate.
  Stored in the ``jump`` field and printed as :samp:`/j`.

  .. index:: MEM_VOLATILE_P, mem and /v, asm_input and /v, asm_operands and /v, volatil, in mem, asm_operands, and asm_input

:samp:`MEM_VOLATILE_P ({x})`
  In ``mem``, ``asm_operands``, and ``asm_input`` expressions,
  nonzero for volatile memory references.
  Stored in the ``volatil`` field and printed as :samp:`/v`.

  .. index:: MEM_NOTRAP_P, mem and /c, call, in mem

:samp:`MEM_NOTRAP_P ({x})`
  In ``mem``, nonzero for memory references that will not trap.
  Stored in the ``call`` field and printed as :samp:`/c`.

  .. index:: MEM_POINTER, mem and /f, frame_related, in mem

:samp:`MEM_POINTER ({x})`
  Nonzero in a ``mem`` if the memory reference holds a pointer.
  Stored in the ``frame_related`` field and printed as :samp:`/f`.

  .. index:: MEM_READONLY_P, mem and /u, unchanging, in mem

:samp:`MEM_READONLY_P ({x})`
  Nonzero in a ``mem``, if the memory is statically allocated and read-only.

  Read-only in this context means never modified during the lifetime of the
  program, not necessarily in ROM or in write-disabled pages.  A common
  example of the later is a shared library's global offset table.  This
  table is initialized by the runtime loader, so the memory is technically
  writable, but after control is transferred from the runtime loader to the
  application, this memory will never be subsequently modified.

  Stored in the ``unchanging`` field and printed as :samp:`/u`.

  .. index:: PREFETCH_SCHEDULE_BARRIER_P, prefetch and /v, volatile, in prefetch

:samp:`PREFETCH_SCHEDULE_BARRIER_P ({x})`
  In a ``prefetch``, indicates that the prefetch is a scheduling barrier.
  No other INSNs will be moved over it.
  Stored in the ``volatil`` field and printed as :samp:`/v`.

  .. index:: REG_FUNCTION_VALUE_P, reg and /i, return_val, in reg

:samp:`REG_FUNCTION_VALUE_P ({x})`
  Nonzero in a ``reg`` if it is the place in which this function's
  value is going to be returned.  (This happens only in a hard
  register.)  Stored in the ``return_val`` field and printed as
  :samp:`/i`.

  .. index:: REG_POINTER, reg and /f, frame_related, in reg

:samp:`REG_POINTER ({x})`
  Nonzero in a ``reg`` if the register holds a pointer.  Stored in the
  ``frame_related`` field and printed as :samp:`/f`.

  .. index:: REG_USERVAR_P, reg and /v, volatil, in reg

:samp:`REG_USERVAR_P ({x})`
  In a ``reg``, nonzero if it corresponds to a variable present in
  the user's source code.  Zero for temporaries generated internally by
  the compiler.  Stored in the ``volatil`` field and printed as
  :samp:`/v`.

  The same hard register may be used also for collecting the values of
  functions called by this one, but ``REG_FUNCTION_VALUE_P`` is zero
  in this kind of use.

  .. index:: RTL_CONST_CALL_P, call_insn and /u, unchanging, in call_insn

:samp:`RTL_CONST_CALL_P ({x})`
  In a ``call_insn`` indicates that the insn represents a call to a
  const function.  Stored in the ``unchanging`` field and printed as
  :samp:`/u`.

  .. index:: RTL_PURE_CALL_P, call_insn and /i, return_val, in call_insn

:samp:`RTL_PURE_CALL_P ({x})`
  In a ``call_insn`` indicates that the insn represents a call to a
  pure function.  Stored in the ``return_val`` field and printed as
  :samp:`/i`.

  .. index:: RTL_CONST_OR_PURE_CALL_P, call_insn and /u or /i

:samp:`RTL_CONST_OR_PURE_CALL_P ({x})`
  In a ``call_insn``, true if ``RTL_CONST_CALL_P`` or
  ``RTL_PURE_CALL_P`` is true.

  .. index:: RTL_LOOPING_CONST_OR_PURE_CALL_P, call_insn and /c, call, in call_insn

:samp:`RTL_LOOPING_CONST_OR_PURE_CALL_P ({x})`
  In a ``call_insn`` indicates that the insn represents a possibly
  infinite looping call to a const or pure function.  Stored in the
  ``call`` field and printed as :samp:`/c`.  Only true if one of
  ``RTL_CONST_CALL_P`` or ``RTL_PURE_CALL_P`` is true.

  .. index:: RTX_FRAME_RELATED_P, insn and /f, call_insn and /f, jump_insn and /f, barrier and /f, set and /f, frame_related, in insn, call_insn, jump_insn, barrier, and set

:samp:`RTX_FRAME_RELATED_P ({x})`
  Nonzero in an ``insn``, ``call_insn``, ``jump_insn``,
  ``barrier``, or ``set`` which is part of a function prologue
  and sets the stack pointer, sets the frame pointer, or saves a register.
  This flag should also be set on an instruction that sets up a temporary
  register to use in place of the frame pointer.
  Stored in the ``frame_related`` field and printed as :samp:`/f`.

  In particular, on RISC targets where there are limits on the sizes of
  immediate constants, it is sometimes impossible to reach the register
  save area directly from the stack pointer.  In that case, a temporary
  register is used that is near enough to the register save area, and the
  Canonical Frame Address, i.e., DWARF2's logical frame pointer, register
  must (temporarily) be changed to be this temporary register.  So, the
  instruction that sets this temporary register must be marked as
  ``RTX_FRAME_RELATED_P``.

  If the marked instruction is overly complex (defined in terms of what
  ``dwarf2out_frame_debug_expr`` can handle), you will also have to
  create a ``REG_FRAME_RELATED_EXPR`` note and attach it to the
  instruction.  This note should contain a simple expression of the
  computation performed by this instruction, i.e., one that
  ``dwarf2out_frame_debug_expr`` can handle.

  This flag is required for exception handling support on targets with RTL
  prologues.

  .. index:: SCHED_GROUP_P, insn and /s, call_insn and /s, jump_insn and /s, jump_table_data and /s, in_struct, in insn, call_insn, jump_insn and jump_table_data

:samp:`SCHED_GROUP_P ({x})`
  During instruction scheduling, in an ``insn``, ``call_insn``,
  ``jump_insn`` or ``jump_table_data``, indicates that the
  previous insn must be scheduled together with this insn.  This is used to
  ensure that certain groups of instructions will not be split up by the
  instruction scheduling pass, for example, ``use`` insns before
  a ``call_insn`` may not be separated from the ``call_insn``.
  Stored in the ``in_struct`` field and printed as :samp:`/s`.

  .. index:: SET_IS_RETURN_P, insn and /j, jump, in insn

:samp:`SET_IS_RETURN_P ({x})`
  For a ``set``, nonzero if it is for a return.
  Stored in the ``jump`` field and printed as :samp:`/j`.

  .. index:: SIBLING_CALL_P, call_insn and /j, jump, in call_insn

:samp:`SIBLING_CALL_P ({x})`
  For a ``call_insn``, nonzero if the insn is a sibling call.
  Stored in the ``jump`` field and printed as :samp:`/j`.

  .. index:: STRING_POOL_ADDRESS_P, symbol_ref and /f, frame_related, in symbol_ref

:samp:`STRING_POOL_ADDRESS_P ({x})`
  For a ``symbol_ref`` expression, nonzero if it addresses this function's
  string constant pool.
  Stored in the ``frame_related`` field and printed as :samp:`/f`.

  .. index:: SUBREG_PROMOTED_UNSIGNED_P, subreg and /u and /v, unchanging, in subreg, volatil, in subreg

:samp:`SUBREG_PROMOTED_UNSIGNED_P ({x})`
  Returns a value greater then zero for a ``subreg`` that has
  ``SUBREG_PROMOTED_VAR_P`` nonzero if the object being referenced is kept
  zero-extended, zero if it is kept sign-extended, and less then zero if it is
  extended some other way via the ``ptr_extend`` instruction.
  Stored in the ``unchanging``
  field and ``volatil`` field, printed as :samp:`/u` and :samp:`/v`.
  This macro may only be used to get the value it may not be used to change
  the value.  Use ``SUBREG_PROMOTED_UNSIGNED_SET`` to change the value.

  .. index:: SUBREG_PROMOTED_UNSIGNED_SET, subreg and /u, unchanging, in subreg, volatil, in subreg

:samp:`SUBREG_PROMOTED_UNSIGNED_SET ({x})`
  Set the ``unchanging`` and ``volatil`` fields in a ``subreg``
  to reflect zero, sign, or other extension.  If ``volatil`` is
  zero, then ``unchanging`` as nonzero means zero extension and as
  zero means sign extension.  If ``volatil`` is nonzero then some
  other type of extension was done via the ``ptr_extend`` instruction.

  .. index:: SUBREG_PROMOTED_VAR_P, subreg and /s, in_struct, in subreg

:samp:`SUBREG_PROMOTED_VAR_P ({x})`
  Nonzero in a ``subreg`` if it was made when accessing an object that
  was promoted to a wider mode in accord with the ``PROMOTED_MODE`` machine
  description macro (see :ref:`storage-layout`).  In this case, the mode of
  the ``subreg`` is the declared mode of the object and the mode of
  ``SUBREG_REG`` is the mode of the register that holds the object.
  Promoted variables are always either sign- or zero-extended to the wider
  mode on every assignment.  Stored in the ``in_struct`` field and
  printed as :samp:`/s`.

  .. index:: SYMBOL_REF_USED, used, in symbol_ref

:samp:`SYMBOL_REF_USED ({x})`
  In a ``symbol_ref``, indicates that :samp:`{x}` has been used.  This is
  normally only used to ensure that :samp:`{x}` is only declared external
  once.  Stored in the ``used`` field.

  .. index:: SYMBOL_REF_WEAK, symbol_ref and /i, return_val, in symbol_ref

:samp:`SYMBOL_REF_WEAK ({x})`
  In a ``symbol_ref``, indicates that :samp:`{x}` has been declared weak.
  Stored in the ``return_val`` field and printed as :samp:`/i`.

  .. index:: SYMBOL_REF_FLAG, symbol_ref and /v, volatil, in symbol_ref

:samp:`SYMBOL_REF_FLAG ({x})`
  In a ``symbol_ref``, this is used as a flag for machine-specific purposes.
  Stored in the ``volatil`` field and printed as :samp:`/v`.

  Most uses of ``SYMBOL_REF_FLAG`` are historic and may be subsumed
  by ``SYMBOL_REF_FLAGS``.  Certainly use of ``SYMBOL_REF_FLAGS``
  is mandatory if the target requires more than one bit of storage.

  These are the fields to which the above macros refer:

.. index:: call, /c in RTL dump

``call``
  In a ``mem``, 1 means that the memory reference will not trap.

  In a ``call``, 1 means that this pure or const call may possibly
  infinite loop.

  In an RTL dump, this flag is represented as :samp:`/c`.

  .. index:: frame_related, /f in RTL dump

``frame_related``
  In an ``insn`` or ``set`` expression, 1 means that it is part of
  a function prologue and sets the stack pointer, sets the frame pointer,
  saves a register, or sets up a temporary register to use in place of the
  frame pointer.

  In ``reg`` expressions, 1 means that the register holds a pointer.

  In ``mem`` expressions, 1 means that the memory reference holds a pointer.

  In ``symbol_ref`` expressions, 1 means that the reference addresses
  this function's string constant pool.

  In an RTL dump, this flag is represented as :samp:`/f`.

  .. index:: in_struct, /s in RTL dump

``in_struct``
  In ``reg`` expressions, it is 1 if the register has its entire life
  contained within the test expression of some loop.

  In ``subreg`` expressions, 1 means that the ``subreg`` is accessing
  an object that has had its mode promoted from a wider mode.

  In ``label_ref`` expressions, 1 means that the referenced label is
  outside the innermost loop containing the insn in which the ``label_ref``
  was found.

  In ``code_label`` expressions, it is 1 if the label may never be deleted.
  This is used for labels which are the target of non-local gotos.  Such a
  label that would have been deleted is replaced with a ``note`` of type
  ``NOTE_INSN_DELETED_LABEL``.

  In an ``insn`` during dead-code elimination, 1 means that the insn is
  dead code.

  In an ``insn`` or ``jump_insn`` during reorg for an insn in the
  delay slot of a branch,
  1 means that this insn is from the target of the branch.

  In an ``insn`` during instruction scheduling, 1 means that this insn
  must be scheduled as part of a group together with the previous insn.

  In an RTL dump, this flag is represented as :samp:`/s`.

  .. index:: return_val, /i in RTL dump

``return_val``
  In ``reg`` expressions, 1 means the register contains
  the value to be returned by the current function.  On
  machines that pass parameters in registers, the same register number
  may be used for parameters as well, but this flag is not set on such
  uses.

  In ``symbol_ref`` expressions, 1 means the referenced symbol is weak.

  In ``call`` expressions, 1 means the call is pure.

  In an RTL dump, this flag is represented as :samp:`/i`.

  .. index:: jump, /j in RTL dump

``jump``
  In a ``mem`` expression, 1 means we should keep the alias set for this
  mem unchanged when we access a component.

  In a ``set``, 1 means it is for a return.

  In a ``call_insn``, 1 means it is a sibling call.

  In a ``jump_insn``, 1 means it is a crossing jump.

  In an RTL dump, this flag is represented as :samp:`/j`.

  .. index:: unchanging, /u in RTL dump

``unchanging``
  In ``reg`` and ``mem`` expressions, 1 means
  that the value of the expression never changes.

  In ``subreg`` expressions, it is 1 if the ``subreg`` references an
  unsigned object whose mode has been promoted to a wider mode.

  In an ``insn`` or ``jump_insn`` in the delay slot of a branch
  instruction, 1 means an annulling branch should be used.

  In a ``symbol_ref`` expression, 1 means that this symbol addresses
  something in the per-function constant pool.

  In a ``call_insn`` 1 means that this instruction is a call to a const
  function.

  In an RTL dump, this flag is represented as :samp:`/u`.

  .. index:: used

``used``
  This flag is used directly (without an access macro) at the end of RTL
  generation for a function, to count the number of times an expression
  appears in insns.  Expressions that appear more than once are copied,
  according to the rules for shared structure (see :ref:`sharing`).

  For a ``reg``, it is used directly (without an access macro) by the
  leaf register renumbering code to ensure that each register is only
  renumbered once.

  In a ``symbol_ref``, it indicates that an external declaration for
  the symbol has already been written.

  .. index:: volatil, /v in RTL dump

``volatil``

  .. index:: volatile memory references

  In a ``mem``, ``asm_operands``, or ``asm_input``
  expression, it is 1 if the memory
  reference is volatile.  Volatile memory references may not be deleted,
  reordered or combined.

  In a ``symbol_ref`` expression, it is used for machine-specific
  purposes.

  In a ``reg`` expression, it is 1 if the value is a user-level variable.
  0 indicates an internal compiler temporary.

  In an ``insn``, 1 means the insn has been deleted.

  In ``label_ref`` and ``reg_label`` expressions, 1 means a reference
  to a non-local label.

  In ``prefetch`` expressions, 1 means that the containing insn is a
  scheduling barrier.

  In an RTL dump, this flag is represented as :samp:`/v`.
