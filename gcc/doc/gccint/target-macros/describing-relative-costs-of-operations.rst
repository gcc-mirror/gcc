..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: costs of instructions, relative costs, speed of instructions

.. _costs:

Describing Relative Costs of Operations
***************************************

These macros let you describe the relative speed of various operations
on the target machine.

.. c:macro:: REGISTER_MOVE_COST (mode, from, to)

  A C expression for the cost of moving data of mode :samp:`{mode}` from a
  register in class :samp:`{from}` to one in class :samp:`{to}`.  The classes are
  expressed using the enumeration values such as ``GENERAL_REGS``.  A
  value of 2 is the default; other values are interpreted relative to
  that.

  It is not required that the cost always equal 2 when :samp:`{from}` is the
  same as :samp:`{to}` ; on some machines it is expensive to move between
  registers if they are not general registers.

  If reload sees an insn consisting of a single ``set`` between two
  hard registers, and if ``REGISTER_MOVE_COST`` applied to their
  classes returns a value of 2, reload does not check to ensure that the
  constraints of the insn are met.  Setting a cost of other than 2 will
  allow reload to verify that the constraints are met.  You should do this
  if the :samp:`mov{m}` pattern's constraints do not allow such copying.

  These macros are obsolete, new ports should use the target hook
  ``TARGET_REGISTER_MOVE_COST`` instead.

.. function:: int TARGET_REGISTER_MOVE_COST (machine_mode mode, reg_class_t from, reg_class_t to)

  .. hook-start:TARGET_REGISTER_MOVE_COST

  This target hook should return the cost of moving data of mode :samp:`{mode}`
  from a register in class :samp:`{from}` to one in class :samp:`{to}`.  The classes
  are expressed using the enumeration values such as ``GENERAL_REGS``.
  A value of 2 is the default; other values are interpreted relative to
  that.

  It is not required that the cost always equal 2 when :samp:`{from}` is the
  same as :samp:`{to}` ; on some machines it is expensive to move between
  registers if they are not general registers.

  If reload sees an insn consisting of a single ``set`` between two
  hard registers, and if ``TARGET_REGISTER_MOVE_COST`` applied to their
  classes returns a value of 2, reload does not check to ensure that the
  constraints of the insn are met.  Setting a cost of other than 2 will
  allow reload to verify that the constraints are met.  You should do this
  if the :samp:`mov{m}` pattern's constraints do not allow such copying.

  The default version of this function returns 2.

.. hook-end

.. c:macro:: MEMORY_MOVE_COST (mode, class, in)

  A C expression for the cost of moving data of mode :samp:`{mode}` between a
  register of class :samp:`{class}` and memory; :samp:`{in}` is zero if the value
  is to be written to memory, nonzero if it is to be read in.  This cost
  is relative to those in ``REGISTER_MOVE_COST``.  If moving between
  registers and memory is more expensive than between two registers, you
  should define this macro to express the relative cost.

  If you do not define this macro, GCC uses a default cost of 4 plus
  the cost of copying via a secondary reload register, if one is
  needed.  If your machine requires a secondary reload register to copy
  between memory and a register of :samp:`{class}` but the reload mechanism is
  more complex than copying via an intermediate, define this macro to
  reflect the actual cost of the move.

  GCC defines the function ``memory_move_secondary_cost`` if
  secondary reloads are needed.  It computes the costs due to copying via
  a secondary register.  If your machine copies from memory using a
  secondary register in the conventional way but the default base value of
  4 is not correct for your machine, define this macro to add some other
  value to the result of that function.  The arguments to that function
  are the same as to this macro.

  These macros are obsolete, new ports should use the target hook
  ``TARGET_MEMORY_MOVE_COST`` instead.

.. function:: int TARGET_MEMORY_MOVE_COST (machine_mode mode, reg_class_t rclass, bool in)

  .. hook-start:TARGET_MEMORY_MOVE_COST

  This target hook should return the cost of moving data of mode :samp:`{mode}`
  between a register of class :samp:`{rclass}` and memory; :samp:`{in}` is ``false``
  if the value is to be written to memory, ``true`` if it is to be read in.
  This cost is relative to those in ``TARGET_REGISTER_MOVE_COST``.
  If moving between registers and memory is more expensive than between two
  registers, you should add this target hook to express the relative cost.

  If you do not add this target hook, GCC uses a default cost of 4 plus
  the cost of copying via a secondary reload register, if one is
  needed.  If your machine requires a secondary reload register to copy
  between memory and a register of :samp:`{rclass}` but the reload mechanism is
  more complex than copying via an intermediate, use this target hook to
  reflect the actual cost of the move.

  GCC defines the function ``memory_move_secondary_cost`` if
  secondary reloads are needed.  It computes the costs due to copying via
  a secondary register.  If your machine copies from memory using a
  secondary register in the conventional way but the default base value of
  4 is not correct for your machine, use this target hook to add some other
  value to the result of that function.  The arguments to that function
  are the same as to this target hook.

.. hook-end

.. c:macro:: BRANCH_COST (speed_p, predictable_p)

  A C expression for the cost of a branch instruction.  A value of 1 is
  the default; other values are interpreted relative to that. Parameter
  :samp:`{speed_p}` is true when the branch in question should be optimized
  for speed.  When it is false, ``BRANCH_COST`` should return a value
  optimal for code size rather than performance.  :samp:`{predictable_p}` is
  true for well-predicted branches. On many architectures the
  ``BRANCH_COST`` can be reduced then.

Here are additional macros which do not specify precise relative costs,
but only that certain actions are more expensive than GCC would
ordinarily expect.

.. c:macro:: SLOW_BYTE_ACCESS

  Define this macro as a C expression which is nonzero if accessing less
  than a word of memory (i.e. a ``char`` or a ``short``) is no
  faster than accessing a word of memory, i.e., if such access
  require more than one instruction or if there is no difference in cost
  between byte and (aligned) word loads.

  When this macro is not defined, the compiler will access a field by
  finding the smallest containing object; when it is defined, a fullword
  load will be used if alignment permits.  Unless bytes accesses are
  faster than word accesses, using word accesses is preferable since it
  may eliminate subsequent memory access if subsequent accesses occur to
  other fields in the same word of the structure, but to different bytes.

.. function:: bool TARGET_SLOW_UNALIGNED_ACCESS (machine_mode mode, unsigned int align)

  .. hook-start:TARGET_SLOW_UNALIGNED_ACCESS

  This hook returns true if memory accesses described by the
  :samp:`{mode}` and :samp:`{alignment}` parameters have a cost many times greater
  than aligned accesses, for example if they are emulated in a trap handler.
  This hook is invoked only for unaligned accesses, i.e. when
  ``alignment < GET_MODE_ALIGNMENT (mode)``.

  When this hook returns true, the compiler will act as if
  ``STRICT_ALIGNMENT`` were true when generating code for block
  moves.  This can cause significantly more instructions to be produced.
  Therefore, do not make this hook return true if unaligned accesses only
  add a cycle or two to the time for a memory access.

  The hook must return true whenever ``STRICT_ALIGNMENT`` is true.
  The default implementation returns ``STRICT_ALIGNMENT``.

.. hook-end

.. c:macro:: MOVE_RATIO (speed)

  The threshold of number of scalar memory-to-memory move insns, *below*
  which a sequence of insns should be generated instead of a
  string move insn or a library call.  Increasing the value will always
  make code faster, but eventually incurs high cost in increased code size.

  Note that on machines where the corresponding move insn is a
  ``define_expand`` that emits a sequence of insns, this macro counts
  the number of such sequences.

  The parameter :samp:`{speed}` is true if the code is currently being
  optimized for speed rather than size.

  If you don't define this, a reasonable default is used.

.. function:: bool TARGET_USE_BY_PIECES_INFRASTRUCTURE_P (unsigned HOST_WIDE_INT size, unsigned int alignment, enum by_pieces_operation op, bool speed_p)

  .. hook-start:TARGET_USE_BY_PIECES_INFRASTRUCTURE_P

  GCC will attempt several strategies when asked to copy between
  two areas of memory, or to set, clear or store to memory, for example
  when copying a ``struct``. The ``by_pieces`` infrastructure
  implements such memory operations as a sequence of load, store or move
  insns.  Alternate strategies are to expand the
  ``cpymem`` or ``setmem`` optabs, to emit a library call, or to emit
  unit-by-unit, loop-based operations.

  This target hook should return true if, for a memory operation with a
  given :samp:`{size}` and :samp:`{alignment}`, using the ``by_pieces``
  infrastructure is expected to result in better code generation.
  Both :samp:`{size}` and :samp:`{alignment}` are measured in terms of storage
  units.

  The parameter :samp:`{op}` is one of: ``CLEAR_BY_PIECES``,
  ``MOVE_BY_PIECES``, ``SET_BY_PIECES``, ``STORE_BY_PIECES`` or
  ``COMPARE_BY_PIECES``.  These describe the type of memory operation
  under consideration.

  The parameter :samp:`{speed_p}` is true if the code is currently being
  optimized for speed rather than size.

  Returning true for higher values of :samp:`{size}` can improve code generation
  for speed if the target does not provide an implementation of the
  ``cpymem`` or ``setmem`` standard names, if the ``cpymem`` or
  ``setmem`` implementation would be more expensive than a sequence of
  insns, or if the overhead of a library call would dominate that of
  the body of the memory operation.

  Returning true for higher values of ``size`` may also cause an increase
  in code size, for example where the number of insns emitted to perform a
  move would be greater than that of a library call.

.. hook-end

.. function:: bool TARGET_OVERLAP_OP_BY_PIECES_P (void)

  .. hook-start:TARGET_OVERLAP_OP_BY_PIECES_P

  This target hook should return true if when the ``by_pieces``
  infrastructure is used, an offset adjusted unaligned memory operation
  in the smallest integer mode for the last piece operation of a memory
  region can be generated to avoid doing more than one smaller operations.

.. hook-end

.. function:: int TARGET_COMPARE_BY_PIECES_BRANCH_RATIO (machine_mode mode)

  .. hook-start:TARGET_COMPARE_BY_PIECES_BRANCH_RATIO

  When expanding a block comparison in MODE, gcc can try to reduce the
  number of branches at the expense of more memory operations.  This hook
  allows the target to override the default choice.  It should return the
  factor by which branches should be reduced over the plain expansion with
  one comparison per :samp:`{mode}` -sized piece.  A port can also prevent a
  particular mode from being used for block comparisons by returning a
  negative number from this hook.

.. hook-end

.. c:macro:: MOVE_MAX_PIECES

  A C expression used by ``move_by_pieces`` to determine the largest unit
  a load or store used to copy memory is.  Defaults to ``MOVE_MAX``.

.. c:macro:: STORE_MAX_PIECES

  A C expression used by ``store_by_pieces`` to determine the largest unit
  a store used to memory is.  Defaults to ``MOVE_MAX_PIECES``, or two times
  the size of ``HOST_WIDE_INT``, whichever is smaller.

.. c:macro:: COMPARE_MAX_PIECES

  A C expression used by ``compare_by_pieces`` to determine the largest unit
  a load or store used to compare memory is.  Defaults to
  ``MOVE_MAX_PIECES``.

.. c:macro:: CLEAR_RATIO (speed)

  The threshold of number of scalar move insns, *below* which a sequence
  of insns should be generated to clear memory instead of a string clear insn
  or a library call.  Increasing the value will always make code faster, but
  eventually incurs high cost in increased code size.

  The parameter :samp:`{speed}` is true if the code is currently being
  optimized for speed rather than size.

  If you don't define this, a reasonable default is used.

.. c:macro:: SET_RATIO (speed)

  The threshold of number of scalar move insns, *below* which a sequence
  of insns should be generated to set memory to a constant value, instead of
  a block set insn or a library call.
  Increasing the value will always make code faster, but
  eventually incurs high cost in increased code size.

  The parameter :samp:`{speed}` is true if the code is currently being
  optimized for speed rather than size.

  If you don't define this, it defaults to the value of ``MOVE_RATIO``.

.. c:macro:: USE_LOAD_POST_INCREMENT (mode)

  A C expression used to determine whether a load postincrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_POST_INCREMENT``.

.. c:macro:: USE_LOAD_POST_DECREMENT (mode)

  A C expression used to determine whether a load postdecrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_POST_DECREMENT``.

.. c:macro:: USE_LOAD_PRE_INCREMENT (mode)

  A C expression used to determine whether a load preincrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_PRE_INCREMENT``.

.. c:macro:: USE_LOAD_PRE_DECREMENT (mode)

  A C expression used to determine whether a load predecrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_PRE_DECREMENT``.

.. c:macro:: USE_STORE_POST_INCREMENT (mode)

  A C expression used to determine whether a store postincrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_POST_INCREMENT``.

.. c:macro:: USE_STORE_POST_DECREMENT (mode)

  A C expression used to determine whether a store postdecrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_POST_DECREMENT``.

.. c:macro:: USE_STORE_PRE_INCREMENT (mode)

  This macro is used to determine whether a store preincrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_PRE_INCREMENT``.

.. c:macro:: USE_STORE_PRE_DECREMENT (mode)

  This macro is used to determine whether a store predecrement is a good
  thing to use for a given mode.  Defaults to the value of
  ``HAVE_PRE_DECREMENT``.

.. c:macro:: NO_FUNCTION_CSE

  Define this macro to be true if it is as good or better to call a constant
  function address than to call an address kept in a register.

.. c:macro:: LOGICAL_OP_NON_SHORT_CIRCUIT

  Define this macro if a non-short-circuit operation produced by
  :samp:`fold_range_test ()` is optimal.  This macro defaults to true if
  ``BRANCH_COST`` is greater than or equal to the value 2.

.. function:: bool TARGET_OPTAB_SUPPORTED_P (int op, machine_mode mode1, machine_mode mode2, optimization_type opt_type)

  .. hook-start:TARGET_OPTAB_SUPPORTED_P

  Return true if the optimizers should use optab :samp:`{op}` with
  modes :samp:`{mode1}` and :samp:`{mode2}` for optimization type :samp:`{opt_type}`.
  The optab is known to have an associated :samp:`.md` instruction
  whose C condition is true.  :samp:`{mode2}` is only meaningful for conversion
  optabs; for direct optabs it is a copy of :samp:`{mode1}`.

  For example, when called with :samp:`{op}` equal to ``rint_optab`` and
  :samp:`{mode1}` equal to ``DFmode``, the hook should say whether the
  optimizers should use optab ``rintdf2``.

  The default hook returns true for all inputs.

.. hook-end

.. function:: bool TARGET_RTX_COSTS (rtx x, machine_mode mode, int outer_code, int opno, int *total, bool speed)

  .. hook-start:TARGET_RTX_COSTS

  This target hook describes the relative costs of RTL expressions.

  The cost may depend on the precise form of the expression, which is
  available for examination in :samp:`{x}`, and the fact that :samp:`{x}` appears
  as operand :samp:`{opno}` of an expression with rtx code :samp:`{outer_code}`.
  That is, the hook can assume that there is some rtx :samp:`{y}` such
  that :samp:`GET_CODE ({y}) == {outer_code}` and such that
  either (a) :samp:`XEXP ({y}, {opno}) == {x}` or
  (b) :samp:`XVEC ({y}, {opno})` contains :samp:`{x}`.

  :samp:`{mode}` is :samp:`{x}` 's machine mode, or for cases like ``const_int`` that
  do not have a mode, the mode in which :samp:`{x}` is used.

  In implementing this hook, you can use the construct
  ``COSTS_N_INSNS (n)`` to specify a cost equal to :samp:`{n}` fast
  instructions.

  On entry to the hook, ``*total`` contains a default estimate
  for the cost of the expression.  The hook should modify this value as
  necessary.  Traditionally, the default costs are ``COSTS_N_INSNS (5)``
  for multiplications, ``COSTS_N_INSNS (7)`` for division and modulus
  operations, and ``COSTS_N_INSNS (1)`` for all other operations.

  When optimizing for code size, i.e. when ``speed`` is
  false, this target hook should be used to estimate the relative
  size cost of an expression, again relative to ``COSTS_N_INSNS``.

  The hook returns true when all subexpressions of :samp:`{x}` have been
  processed, and false when ``rtx_cost`` should recurse.

.. hook-end

.. function:: int TARGET_ADDRESS_COST (rtx address, machine_mode mode, addr_space_t as, bool speed)

  .. hook-start:TARGET_ADDRESS_COST

  This hook computes the cost of an addressing mode that contains
  :samp:`{address}`.  If not defined, the cost is computed from
  the :samp:`{address}` expression and the ``TARGET_RTX_COST`` hook.

  For most CISC machines, the default cost is a good approximation of the
  true cost of the addressing mode.  However, on RISC machines, all
  instructions normally have the same length and execution time.  Hence
  all addresses will have equal costs.

  In cases where more than one form of an address is known, the form with
  the lowest cost will be used.  If multiple forms have the same, lowest,
  cost, the one that is the most complex will be used.

  For example, suppose an address that is equal to the sum of a register
  and a constant is used twice in the same basic block.  When this macro
  is not defined, the address will be computed in a register and memory
  references will be indirect through that register.  On machines where
  the cost of the addressing mode containing the sum is no higher than
  that of a simple indirect reference, this will produce an additional
  instruction and possibly require an additional register.  Proper
  specification of this macro eliminates this overhead for such machines.

  This hook is never called with an invalid address.

  On machines where an address involving more than one register is as
  cheap as an address computation involving only one register, defining
  ``TARGET_ADDRESS_COST`` to reflect this can cause two registers to
  be live over a region of code where only one would have been if
  ``TARGET_ADDRESS_COST`` were not defined in that manner.  This effect
  should be considered in the definition of this macro.  Equivalent costs
  should probably only be given to addresses with different numbers of
  registers on machines with lots of registers.

.. hook-end

.. function:: int TARGET_INSN_COST (rtx_insn *insn, bool speed)

  .. hook-start:TARGET_INSN_COST

  This target hook describes the relative costs of RTL instructions.

  In implementing this hook, you can use the construct
  ``COSTS_N_INSNS (n)`` to specify a cost equal to :samp:`{n}` fast
  instructions.

  When optimizing for code size, i.e. when ``speed`` is
  false, this target hook should be used to estimate the relative
  size cost of an expression, again relative to ``COSTS_N_INSNS``.

.. hook-end

.. function:: unsigned int TARGET_MAX_NOCE_IFCVT_SEQ_COST (edge e)

  .. hook-start:TARGET_MAX_NOCE_IFCVT_SEQ_COST

  This hook returns a value in the same units as ``TARGET_RTX_COSTS``,
  giving the maximum acceptable cost for a sequence generated by the RTL
  if-conversion pass when conditional execution is not available.
  The RTL if-conversion pass attempts to convert conditional operations
  that would require a branch to a series of unconditional operations and
  ``movmodecc`` insns.  This hook returns the maximum cost of the
  unconditional instructions and the ``movmodecc`` insns.
  RTL if-conversion is cancelled if the cost of the converted sequence
  is greater than the value returned by this hook.

  ``e`` is the edge between the basic block containing the conditional
  branch to the basic block which would be executed if the condition
  were true.

  The default implementation of this hook uses the
  ``max-rtl-if-conversion-[un]predictable`` parameters if they are set,
  and uses a multiple of ``BRANCH_COST`` otherwise.

.. hook-end

.. function:: bool TARGET_NOCE_CONVERSION_PROFITABLE_P (rtx_insn *seq, struct noce_if_info *if_info)

  .. hook-start:TARGET_NOCE_CONVERSION_PROFITABLE_P

  This hook returns true if the instruction sequence ``seq`` is a good
  candidate as a replacement for the if-convertible sequence described in
  ``if_info``.

.. hook-end

.. function:: bool TARGET_NEW_ADDRESS_PROFITABLE_P (rtx memref, rtx_insn * insn, rtx new_addr)

  .. hook-start:TARGET_NEW_ADDRESS_PROFITABLE_P

  Return ``true`` if it is profitable to replace the address in
  :samp:`{memref}` with :samp:`{new_addr}`.  This allows targets to prevent the
  scheduler from undoing address optimizations.  The instruction containing the
  memref is :samp:`{insn}`.  The default implementation returns ``true``.

.. hook-end

.. function:: bool TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P (void)

  .. hook-start:TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P

  This predicate controls the use of the eager delay slot filler to disallow
  speculatively executed instructions being placed in delay slots.  Targets
  such as certain MIPS architectures possess both branches with and without
  delay slots.  As the eager delay slot filler can decrease performance,
  disabling it is beneficial when ordinary branches are available.  Use of
  delay slot branches filled using the basic filler is often still desirable
  as the delay slot can hide a pipeline bubble.

.. hook-end

.. function:: HOST_WIDE_INT TARGET_ESTIMATED_POLY_VALUE (poly_int64 val, poly_value_estimate_kind kind)

  .. hook-start:TARGET_ESTIMATED_POLY_VALUE

  Return an estimate of the runtime value of :samp:`{val}`, for use in
  things like cost calculations or profiling frequencies.  :samp:`{kind}` is used
  to ask for the minimum, maximum, and likely estimates of the value through
  the ``POLY_VALUE_MIN``, ``POLY_VALUE_MAX`` and
  ``POLY_VALUE_LIKELY`` values.  The default
  implementation returns the lowest possible value of :samp:`{val}`.

.. hook-end