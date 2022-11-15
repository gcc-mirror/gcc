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

.. include:: tm.rst.in
  :start-after: [TARGET_REGISTER_MOVE_COST]
  :end-before: [TARGET_REGISTER_MOVE_COST]


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

.. include:: tm.rst.in
  :start-after: [TARGET_MEMORY_MOVE_COST]
  :end-before: [TARGET_MEMORY_MOVE_COST]


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

.. include:: tm.rst.in
  :start-after: [TARGET_SLOW_UNALIGNED_ACCESS]
  :end-before: [TARGET_SLOW_UNALIGNED_ACCESS]


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

.. include:: tm.rst.in
  :start-after: [TARGET_USE_BY_PIECES_INFRASTRUCTURE_P]
  :end-before: [TARGET_USE_BY_PIECES_INFRASTRUCTURE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_OVERLAP_OP_BY_PIECES_P]
  :end-before: [TARGET_OVERLAP_OP_BY_PIECES_P]


.. include:: tm.rst.in
  :start-after: [TARGET_COMPARE_BY_PIECES_BRANCH_RATIO]
  :end-before: [TARGET_COMPARE_BY_PIECES_BRANCH_RATIO]


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

.. include:: tm.rst.in
  :start-after: [TARGET_OPTAB_SUPPORTED_P]
  :end-before: [TARGET_OPTAB_SUPPORTED_P]


.. include:: tm.rst.in
  :start-after: [TARGET_RTX_COSTS]
  :end-before: [TARGET_RTX_COSTS]


.. include:: tm.rst.in
  :start-after: [TARGET_ADDRESS_COST]
  :end-before: [TARGET_ADDRESS_COST]


.. include:: tm.rst.in
  :start-after: [TARGET_INSN_COST]
  :end-before: [TARGET_INSN_COST]


.. include:: tm.rst.in
  :start-after: [TARGET_MAX_NOCE_IFCVT_SEQ_COST]
  :end-before: [TARGET_MAX_NOCE_IFCVT_SEQ_COST]


.. include:: tm.rst.in
  :start-after: [TARGET_NOCE_CONVERSION_PROFITABLE_P]
  :end-before: [TARGET_NOCE_CONVERSION_PROFITABLE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_NEW_ADDRESS_PROFITABLE_P]
  :end-before: [TARGET_NEW_ADDRESS_PROFITABLE_P]


.. include:: tm.rst.in
  :start-after: [TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P]
  :end-before: [TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P]


.. include:: tm.rst.in
  :start-after: [TARGET_ESTIMATED_POLY_VALUE]
  :end-before: [TARGET_ESTIMATED_POLY_VALUE]
