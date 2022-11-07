..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _scheduling:

Adjusting the Instruction Scheduler
***********************************

The instruction scheduler may need a fair amount of machine-specific
adjustment in order to produce good code.  GCC provides several target
hooks for this purpose.  It is usually enough to define just a few of
them: try the first ones in this list first.

.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_ISSUE_RATE]
  :end-before: [TARGET_SCHED_ISSUE_RATE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_VARIABLE_ISSUE]
  :end-before: [TARGET_SCHED_VARIABLE_ISSUE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_ADJUST_COST]
  :end-before: [TARGET_SCHED_ADJUST_COST]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_ADJUST_PRIORITY]
  :end-before: [TARGET_SCHED_ADJUST_PRIORITY]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_REORDER]
  :end-before: [TARGET_SCHED_REORDER]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_REORDER2]
  :end-before: [TARGET_SCHED_REORDER2]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_MACRO_FUSION_P]
  :end-before: [TARGET_SCHED_MACRO_FUSION_P]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_MACRO_FUSION_PAIR_P]
  :end-before: [TARGET_SCHED_MACRO_FUSION_PAIR_P]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK]
  :end-before: [TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_INIT]
  :end-before: [TARGET_SCHED_INIT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FINISH]
  :end-before: [TARGET_SCHED_FINISH]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_INIT_GLOBAL]
  :end-before: [TARGET_SCHED_INIT_GLOBAL]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FINISH_GLOBAL]
  :end-before: [TARGET_SCHED_FINISH_GLOBAL]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DFA_PRE_CYCLE_INSN]
  :end-before: [TARGET_SCHED_DFA_PRE_CYCLE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN]
  :end-before: [TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DFA_POST_CYCLE_INSN]
  :end-before: [TARGET_SCHED_DFA_POST_CYCLE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN]
  :end-before: [TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE]
  :end-before: [TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DFA_POST_ADVANCE_CYCLE]
  :end-before: [TARGET_SCHED_DFA_POST_ADVANCE_CYCLE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BEGIN]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BEGIN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_ISSUE]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_ISSUE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BACKTRACK]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BACKTRACK]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_END]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_END]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_INIT]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_INIT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_FINI]
  :end-before: [TARGET_SCHED_FIRST_CYCLE_MULTIPASS_FINI]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DFA_NEW_CYCLE]
  :end-before: [TARGET_SCHED_DFA_NEW_CYCLE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_IS_COSTLY_DEPENDENCE]
  :end-before: [TARGET_SCHED_IS_COSTLY_DEPENDENCE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_H_I_D_EXTENDED]
  :end-before: [TARGET_SCHED_H_I_D_EXTENDED]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_ALLOC_SCHED_CONTEXT]
  :end-before: [TARGET_SCHED_ALLOC_SCHED_CONTEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_INIT_SCHED_CONTEXT]
  :end-before: [TARGET_SCHED_INIT_SCHED_CONTEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_SET_SCHED_CONTEXT]
  :end-before: [TARGET_SCHED_SET_SCHED_CONTEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_CLEAR_SCHED_CONTEXT]
  :end-before: [TARGET_SCHED_CLEAR_SCHED_CONTEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FREE_SCHED_CONTEXT]
  :end-before: [TARGET_SCHED_FREE_SCHED_CONTEXT]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_SPECULATE_INSN]
  :end-before: [TARGET_SCHED_SPECULATE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_NEEDS_BLOCK_P]
  :end-before: [TARGET_SCHED_NEEDS_BLOCK_P]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_GEN_SPEC_CHECK]
  :end-before: [TARGET_SCHED_GEN_SPEC_CHECK]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_SET_SCHED_FLAGS]
  :end-before: [TARGET_SCHED_SET_SCHED_FLAGS]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_CAN_SPECULATE_INSN]
  :end-before: [TARGET_SCHED_CAN_SPECULATE_INSN]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_SMS_RES_MII]
  :end-before: [TARGET_SCHED_SMS_RES_MII]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DISPATCH]
  :end-before: [TARGET_SCHED_DISPATCH]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_DISPATCH_DO]
  :end-before: [TARGET_SCHED_DISPATCH_DO]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_EXPOSED_PIPELINE]
  :end-before: [TARGET_SCHED_EXPOSED_PIPELINE]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_REASSOCIATION_WIDTH]
  :end-before: [TARGET_SCHED_REASSOCIATION_WIDTH]


.. include:: tm.rst.in
  :start-after: [TARGET_SCHED_FUSION_PRIORITY]
  :end-before: [TARGET_SCHED_FUSION_PRIORITY]


.. include:: tm.rst.in
  :start-after: [TARGET_EXPAND_DIVMOD_LIBFUNC]
  :end-before: [TARGET_EXPAND_DIVMOD_LIBFUNC]
