..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: condition code status

.. _condition-code:

Condition Code Status
*********************

Condition codes in GCC are represented as registers,
which provides better schedulability for
architectures that do have a condition code register, but on which
most instructions do not affect it.  The latter category includes
most RISC machines.

Implicit clobbering would pose a strong restriction on the placement of
the definition and use of the condition code.  In the past the definition
and use were always adjacent.  However, recent changes to support trapping
arithmetic may result in the definition and user being in different blocks.
Thus, there may be a ``NOTE_INSN_BASIC_BLOCK`` between them.  Additionally,
the definition may be the source of exception handling edges.

These restrictions can prevent important
optimizations on some machines.  For example, on the IBM RS/6000, there
is a delay for taken branches unless the condition code register is set
three instructions earlier than the conditional branch.  The instruction
scheduler cannot perform this optimization if it is not permitted to
separate the definition and use of the condition code register.

If there is a specific
condition code register in the machine, use a hard register.  If the
condition code or comparison result can be placed in any general register,
or if there are multiple condition registers, use a pseudo register.
Registers used to store the condition code value will usually have a mode
that is in class ``MODE_CC``.

Alternatively, you can use ``BImode`` if the comparison operator is
specified already in the compare instruction.  In this case, you are not
interested in most macros in this section.

.. toctree::
  :maxdepth: 2


.. index:: CCmode, MODE_CC

.. _mode_cc-condition-codes:

Representation of condition codes using registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. c:macro:: SELECT_CC_MODE (op, x, y)

  On many machines, the condition code may be produced by other instructions
  than compares, for example the branch can use directly the condition
  code set by a subtract instruction.  However, on some machines
  when the condition code is set this way some bits (such as the overflow
  bit) are not set in the same way as a test instruction, so that a different
  branch instruction must be used for some conditional branches.  When
  this happens, use the machine mode of the condition code register to
  record different formats of the condition code register.  Modes can
  also be used to record which compare instruction (e.g. a signed or an
  unsigned comparison) produced the condition codes.

  If other modes than ``CCmode`` are required, add them to
  :samp:`{machine}-modes.def` and define ``SELECT_CC_MODE`` to choose
  a mode given an operand of a compare.  This is needed because the modes
  have to be chosen not only during RTL generation but also, for example,
  by instruction combination.  The result of ``SELECT_CC_MODE`` should
  be consistent with the mode used in the patterns; for example to support
  the case of the add on the SPARC discussed above, we have the pattern

  .. code-block:: c++

    (define_insn ""
      [(set (reg:CCNZ 0)
            (compare:CCNZ
              (plus:SI (match_operand:SI 0 "register_operand" "%r")
                       (match_operand:SI 1 "arith_operand" "rI"))
              (const_int 0)))]
      ""
      "...")

  together with a ``SELECT_CC_MODE`` that returns ``CCNZmode``
  for comparisons whose argument is a ``plus`` :

  .. code-block:: c++

    #define SELECT_CC_MODE(OP,X,Y) \
      (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT           \
       ? ((OP == LT || OP == LE || OP == GT || OP == GE)     \
          ? CCFPEmode : CCFPmode)                            \
       : ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS     \
           || GET_CODE (X) == NEG || GET_CODE (x) == ASHIFT) \
          ? CCNZmode : CCmode))

  Another reason to use modes is to retain information on which operands
  were used by the comparison; see ``REVERSIBLE_CC_MODE`` later in
  this section.

  You should define this macro if and only if you define extra CC modes
  in :samp:`{machine}-modes.def`.

.. include:: tm.rst.in
  :start-after: [TARGET_CANONICALIZE_COMPARISON]
  :end-before: [TARGET_CANONICALIZE_COMPARISON]


.. c:macro:: REVERSIBLE_CC_MODE (mode)

  A C expression whose value is one if it is always safe to reverse a
  comparison whose mode is :samp:`{mode}`.  If ``SELECT_CC_MODE``
  can ever return :samp:`{mode}` for a floating-point inequality comparison,
  then ``REVERSIBLE_CC_MODE (mode)`` must be zero.

  You need not define this macro if it would always returns zero or if the
  floating-point format is anything other than ``IEEE_FLOAT_FORMAT``.
  For example, here is the definition used on the SPARC, where floating-point
  inequality comparisons are given either ``CCFPEmode`` or ``CCFPmode`` :

  .. code-block:: c++

    #define REVERSIBLE_CC_MODE(MODE) \
       ((MODE) != CCFPEmode && (MODE) != CCFPmode)

.. c:macro:: REVERSE_CONDITION (code, mode)

  A C expression whose value is reversed condition code of the :samp:`{code}` for
  comparison done in CC_MODE :samp:`{mode}`.  The macro is used only in case
  ``REVERSIBLE_CC_MODE (mode)`` is nonzero.  Define this macro in case
  machine has some non-standard way how to reverse certain conditionals.  For
  instance in case all floating point conditions are non-trapping, compiler may
  freely convert unordered compares to ordered ones.  Then definition may look
  like:

  .. code-block:: c++

    #define REVERSE_CONDITION(CODE, MODE) \
       ((MODE) != CCFPmode ? reverse_condition (CODE) \
        : reverse_condition_maybe_unordered (CODE))

.. include:: tm.rst.in
  :start-after: [TARGET_FIXED_CONDITION_CODE_REGS]
  :end-before: [TARGET_FIXED_CONDITION_CODE_REGS]


.. include:: tm.rst.in
  :start-after: [TARGET_CC_MODES_COMPATIBLE]
  :end-before: [TARGET_CC_MODES_COMPATIBLE]


.. include:: tm.rst.in
  :start-after: [TARGET_FLAGS_REGNUM]
  :end-before: [TARGET_FLAGS_REGNUM]
