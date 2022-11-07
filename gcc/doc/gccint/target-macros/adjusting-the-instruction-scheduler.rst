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

.. function:: int TARGET_SCHED_ISSUE_RATE (void)

  .. hook-start:TARGET_SCHED_ISSUE_RATE

  This hook returns the maximum number of instructions that can ever
  issue at the same time on the target machine.  The default is one.
  Although the insn scheduler can define itself the possibility of issue
  an insn on the same cycle, the value can serve as an additional
  constraint to issue insns on the same simulated processor cycle (see
  hooks :samp:`TARGET_SCHED_REORDER` and :samp:`TARGET_SCHED_REORDER2`).
  This value must be constant over the entire compilation.  If you need
  it to vary depending on what the instructions are, you must use
  :samp:`TARGET_SCHED_VARIABLE_ISSUE`.

.. hook-end

.. function:: int TARGET_SCHED_VARIABLE_ISSUE (FILE *file, int verbose, rtx_insn *insn, int more)

  .. hook-start:TARGET_SCHED_VARIABLE_ISSUE

  This hook is executed by the scheduler after it has scheduled an insn
  from the ready list.  It should return the number of insns which can
  still be issued in the current cycle.  The default is
  :samp:`{more} - 1` for insns other than ``CLOBBER`` and
  ``USE``, which normally are not counted against the issue rate.
  You should define this hook if some insns take more machine resources
  than others, so that fewer insns can follow them in the same cycle.
  :samp:`{file}` is either a null pointer, or a stdio stream to write any
  debug output to.  :samp:`{verbose}` is the verbose level provided by
  :option:`-fsched-verbose-n`.  :samp:`{insn}` is the instruction that
  was scheduled.

.. hook-end

.. function:: int TARGET_SCHED_ADJUST_COST (rtx_insn *insn, int dep_type1, rtx_insn *dep_insn, int cost, unsigned int dw)

  .. hook-start:TARGET_SCHED_ADJUST_COST

  This function corrects the value of :samp:`{cost}` based on the
  relationship between :samp:`{insn}` and :samp:`{dep_insn}` through a
  dependence of type dep_type, and strength :samp:`{dw}`.  It should return the new
  value.  The default is to make no adjustment to :samp:`{cost}`.  This can be
  used for example to specify to the scheduler using the traditional pipeline
  description that an output- or anti-dependence does not incur the same cost
  as a data-dependence.  If the scheduler using the automaton based pipeline
  description, the cost of anti-dependence is zero and the cost of
  output-dependence is maximum of one and the difference of latency
  times of the first and the second insns.  If these values are not
  acceptable, you could use the hook to modify them too.  See also
  see :ref:`processor-pipeline-description`.

.. hook-end

.. function:: int TARGET_SCHED_ADJUST_PRIORITY (rtx_insn *insn, int priority)

  .. hook-start:TARGET_SCHED_ADJUST_PRIORITY

  This hook adjusts the integer scheduling priority :samp:`{priority}` of
  :samp:`{insn}`.  It should return the new priority.  Increase the priority to
  execute :samp:`{insn}` earlier, reduce the priority to execute :samp:`{insn}`
  later.  Do not define this hook if you do not need to adjust the
  scheduling priorities of insns.

.. hook-end

.. function:: int TARGET_SCHED_REORDER (FILE *file, int verbose, rtx_insn **ready, int *n_readyp, int clock)

  .. hook-start:TARGET_SCHED_REORDER

  This hook is executed by the scheduler after it has scheduled the ready
  list, to allow the machine description to reorder it (for example to
  combine two small instructions together on :samp:`VLIW` machines).
  :samp:`{file}` is either a null pointer, or a stdio stream to write any
  debug output to.  :samp:`{verbose}` is the verbose level provided by
  :option:`-fsched-verbose-n`.  :samp:`{ready}` is a pointer to the ready
  list of instructions that are ready to be scheduled.  :samp:`{n_readyp}` is
  a pointer to the number of elements in the ready list.  The scheduler
  reads the ready list in reverse order, starting with
  :samp:`{ready}` [ :samp:`{*n_readyp}` - 1] and going to :samp:`{ready}` [0].  :samp:`{clock}`
  is the timer tick of the scheduler.  You may modify the ready list and
  the number of ready insns.  The return value is the number of insns that
  can issue this cycle; normally this is just ``issue_rate``.  See also
  :samp:`TARGET_SCHED_REORDER2`.

.. hook-end

.. function:: int TARGET_SCHED_REORDER2 (FILE *file, int verbose, rtx_insn **ready, int *n_readyp, int clock)

  .. hook-start:TARGET_SCHED_REORDER2

  Like :samp:`TARGET_SCHED_REORDER`, but called at a different time.  That
  function is called whenever the scheduler starts a new cycle.  This one
  is called once per iteration over a cycle, immediately after
  :samp:`TARGET_SCHED_VARIABLE_ISSUE`; it can reorder the ready list and
  return the number of insns to be scheduled in the same cycle.  Defining
  this hook can be useful if there are frequent situations where
  scheduling one insn causes other insns to become ready in the same
  cycle.  These other insns can then be taken into account properly.

.. hook-end

.. function:: bool TARGET_SCHED_MACRO_FUSION_P (void)

  .. hook-start:TARGET_SCHED_MACRO_FUSION_P

  This hook is used to check whether target platform supports macro fusion.

.. hook-end

.. function:: bool TARGET_SCHED_MACRO_FUSION_PAIR_P (rtx_insn *prev, rtx_insn *curr)

  .. hook-start:TARGET_SCHED_MACRO_FUSION_PAIR_P

  This hook is used to check whether two insns should be macro fused for
  a target microarchitecture. If this hook returns true for the given insn pair
  (:samp:`{prev}` and :samp:`{curr}`), the scheduler will put them into a sched
  group, and they will not be scheduled apart.  The two insns will be either
  two SET insns or a compare and a conditional jump and this hook should
  validate any dependencies needed to fuse the two insns together.

.. hook-end

.. function:: void TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK (rtx_insn *head, rtx_insn *tail)

  .. hook-start:TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK

  This hook is called after evaluation forward dependencies of insns in
  chain given by two parameter values (:samp:`{head}` and :samp:`{tail}`
  correspondingly) but before insns scheduling of the insn chain.  For
  example, it can be used for better insn classification if it requires
  analysis of dependencies.  This hook can use backward and forward
  dependencies of the insn scheduler because they are already
  calculated.

.. hook-end

.. function:: void TARGET_SCHED_INIT (FILE *file, int verbose, int max_ready)

  .. hook-start:TARGET_SCHED_INIT

  This hook is executed by the scheduler at the beginning of each block of
  instructions that are to be scheduled.  :samp:`{file}` is either a null
  pointer, or a stdio stream to write any debug output to.  :samp:`{verbose}`
  is the verbose level provided by :option:`-fsched-verbose-n`.
  :samp:`{max_ready}` is the maximum number of insns in the current scheduling
  region that can be live at the same time.  This can be used to allocate
  scratch space if it is needed, e.g. by :samp:`TARGET_SCHED_REORDER`.

.. hook-end

.. function:: void TARGET_SCHED_FINISH (FILE *file, int verbose)

  .. hook-start:TARGET_SCHED_FINISH

  This hook is executed by the scheduler at the end of each block of
  instructions that are to be scheduled.  It can be used to perform
  cleanup of any actions done by the other scheduling hooks.  :samp:`{file}`
  is either a null pointer, or a stdio stream to write any debug output
  to.  :samp:`{verbose}` is the verbose level provided by
  :option:`-fsched-verbose-n`.

.. hook-end

.. function:: void TARGET_SCHED_INIT_GLOBAL (FILE *file, int verbose, int old_max_uid)

  .. hook-start:TARGET_SCHED_INIT_GLOBAL

  This hook is executed by the scheduler after function level initializations.
  :samp:`{file}` is either a null pointer, or a stdio stream to write any debug output to.
  :samp:`{verbose}` is the verbose level provided by :option:`-fsched-verbose-n`.
  :samp:`{old_max_uid}` is the maximum insn uid when scheduling begins.

.. hook-end

.. function:: void TARGET_SCHED_FINISH_GLOBAL (FILE *file, int verbose)

  .. hook-start:TARGET_SCHED_FINISH_GLOBAL

  This is the cleanup hook corresponding to ``TARGET_SCHED_INIT_GLOBAL``.
  :samp:`{file}` is either a null pointer, or a stdio stream to write any debug output to.
  :samp:`{verbose}` is the verbose level provided by :option:`-fsched-verbose-n`.

.. hook-end

.. function:: rtx TARGET_SCHED_DFA_PRE_CYCLE_INSN (void)

  .. hook-start:TARGET_SCHED_DFA_PRE_CYCLE_INSN

  The hook returns an RTL insn.  The automaton state used in the
  pipeline hazard recognizer is changed as if the insn were scheduled
  when the new simulated processor cycle starts.  Usage of the hook may
  simplify the automaton pipeline description for some VLIW
  processors.  If the hook is defined, it is used only for the automaton
  based pipeline description.  The default is not to change the state
  when the new simulated processor cycle starts.

.. hook-end

.. function:: void TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN (void)

  .. hook-start:TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN

  The hook can be used to initialize data used by the previous hook.

.. hook-end

.. function:: rtx_insn * TARGET_SCHED_DFA_POST_CYCLE_INSN (void)

  .. hook-start:TARGET_SCHED_DFA_POST_CYCLE_INSN

  The hook is analogous to :samp:`TARGET_SCHED_DFA_PRE_CYCLE_INSN` but used
  to changed the state as if the insn were scheduled when the new
  simulated processor cycle finishes.

.. hook-end

.. function:: void TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN (void)

  .. hook-start:TARGET_SCHED_INIT_DFA_POST_CYCLE_INSN

  The hook is analogous to :samp:`TARGET_SCHED_INIT_DFA_PRE_CYCLE_INSN` but
  used to initialize data used by the previous hook.

.. hook-end

.. function:: void TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE (void)

  .. hook-start:TARGET_SCHED_DFA_PRE_ADVANCE_CYCLE

  The hook to notify target that the current simulated cycle is about to finish.
  The hook is analogous to :samp:`TARGET_SCHED_DFA_PRE_CYCLE_INSN` but used
  to change the state in more complicated situations - e.g., when advancing
  state on a single insn is not enough.

.. hook-end

.. function:: void TARGET_SCHED_DFA_POST_ADVANCE_CYCLE (void)

  .. hook-start:TARGET_SCHED_DFA_POST_ADVANCE_CYCLE

  The hook to notify target that new simulated cycle has just started.
  The hook is analogous to :samp:`TARGET_SCHED_DFA_POST_CYCLE_INSN` but used
  to change the state in more complicated situations - e.g., when advancing
  state on a single insn is not enough.

.. hook-end

.. function:: int TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD (void)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD

  This hook controls better choosing an insn from the ready insn queue
  for the DFA-based insn scheduler.  Usually the scheduler
  chooses the first insn from the queue.  If the hook returns a positive
  value, an additional scheduler code tries all permutations of
  :samp:`TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD ()`
  subsequent ready insns to choose an insn whose issue will result in
  maximal number of issued insns on the same cycle.  For the
  VLIW processor, the code could actually solve the problem of
  packing simple insns into the VLIW insn.  Of course, if the
  rules of VLIW packing are described in the automaton.

  This code also could be used for superscalar RISC
  processors.  Let us consider a superscalar RISC processor
  with 3 pipelines.  Some insns can be executed in pipelines :samp:`{A}` or
  :samp:`{B}`, some insns can be executed only in pipelines :samp:`{B}` or
  :samp:`{C}`, and one insn can be executed in pipeline :samp:`{B}`.  The
  processor may issue the 1st insn into :samp:`{A}` and the 2nd one into
  :samp:`{B}`.  In this case, the 3rd insn will wait for freeing :samp:`{B}`
  until the next cycle.  If the scheduler issues the 3rd insn the first,
  the processor could issue all 3 insns per cycle.

  Actually this code demonstrates advantages of the automaton based
  pipeline hazard recognizer.  We try quickly and easy many insn
  schedules to choose the best one.

  The default is no multipass scheduling.

.. hook-end

.. function:: int TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD (rtx_insn *insn, int ready_index)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD

  This hook controls what insns from the ready insn queue will be
  considered for the multipass insn scheduling.  If the hook returns
  zero for :samp:`{insn}`, the insn will be considered in multipass scheduling.
  Positive return values will remove :samp:`{insn}` from consideration on
  the current round of multipass scheduling.
  Negative return values will remove :samp:`{insn}` from consideration for given
  number of cycles.
  Backends should be careful about returning non-zero for highest priority
  instruction at position 0 in the ready list.  :samp:`{ready_index}` is passed
  to allow backends make correct judgements.

  The default is that any ready insns can be chosen to be issued.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BEGIN (void *data, signed char *ready_try, int n_ready, bool first_cycle_insn_p)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BEGIN

  This hook prepares the target backend for a new round of multipass
  scheduling.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_ISSUE (void *data, signed char *ready_try, int n_ready, rtx_insn *insn, const void *prev_data)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_ISSUE

  This hook is called when multipass scheduling evaluates instruction INSN.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BACKTRACK (const void *data, signed char *ready_try, int n_ready)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_BACKTRACK

  This is called when multipass scheduling backtracks from evaluation of
  an instruction.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_END (const void *data)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_END

  This hook notifies the target about the result of the concluded current
  round of multipass scheduling.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_INIT (void *data)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_INIT

  This hook initializes target-specific data used in multipass scheduling.

.. hook-end

.. function:: void TARGET_SCHED_FIRST_CYCLE_MULTIPASS_FINI (void *data)

  .. hook-start:TARGET_SCHED_FIRST_CYCLE_MULTIPASS_FINI

  This hook finalizes target-specific data used in multipass scheduling.

.. hook-end

.. function:: int TARGET_SCHED_DFA_NEW_CYCLE (FILE *dump, int verbose, rtx_insn *insn, int last_clock, int clock, int *sort_p)

  .. hook-start:TARGET_SCHED_DFA_NEW_CYCLE

  This hook is called by the insn scheduler before issuing :samp:`{insn}`
  on cycle :samp:`{clock}`.  If the hook returns nonzero,
  :samp:`{insn}` is not issued on this processor cycle.  Instead,
  the processor cycle is advanced.  If \* :samp:`{sort_p}`
  is zero, the insn ready queue is not sorted on the new cycle
  start as usually.  :samp:`{dump}` and :samp:`{verbose}` specify the file and
  verbosity level to use for debugging output.
  :samp:`{last_clock}` and :samp:`{clock}` are, respectively, the
  processor cycle on which the previous insn has been issued,
  and the current processor cycle.

.. hook-end

.. function:: bool TARGET_SCHED_IS_COSTLY_DEPENDENCE (struct _dep *_dep, int cost, int distance)

  .. hook-start:TARGET_SCHED_IS_COSTLY_DEPENDENCE

  This hook is used to define which dependences are considered costly by
  the target, so costly that it is not advisable to schedule the insns that
  are involved in the dependence too close to one another.  The parameters
  to this hook are as follows:  The first parameter :samp:`{_dep}` is the dependence
  being evaluated.  The second parameter :samp:`{cost}` is the cost of the
  dependence as estimated by the scheduler, and the third
  parameter :samp:`{distance}` is the distance in cycles between the two insns.
  The hook returns ``true`` if considering the distance between the two
  insns the dependence between them is considered costly by the target,
  and ``false`` otherwise.

  Defining this hook can be useful in multiple-issue out-of-order machines,
  where (a) it's practically hopeless to predict the actual data/resource
  delays, however: (b) there's a better chance to predict the actual grouping
  that will be formed, and (c) correctly emulating the grouping can be very
  important.  In such targets one may want to allow issuing dependent insns
  closer to one another---i.e., closer than the dependence distance;  however,
  not in cases of 'costly dependences', which this hooks allows to define.

.. hook-end

.. function:: void TARGET_SCHED_H_I_D_EXTENDED (void)

  .. hook-start:TARGET_SCHED_H_I_D_EXTENDED

  This hook is called by the insn scheduler after emitting a new instruction to
  the instruction stream.  The hook notifies a target backend to extend its
  per instruction data structures.

.. hook-end

.. function:: void * TARGET_SCHED_ALLOC_SCHED_CONTEXT (void)

  .. hook-start:TARGET_SCHED_ALLOC_SCHED_CONTEXT

  Return a pointer to a store large enough to hold target scheduling context.

.. hook-end

.. function:: void TARGET_SCHED_INIT_SCHED_CONTEXT (void *tc, bool clean_p)

  .. hook-start:TARGET_SCHED_INIT_SCHED_CONTEXT

  Initialize store pointed to by :samp:`{tc}` to hold target scheduling context.
  It :samp:`{clean_p}` is true then initialize :samp:`{tc}` as if scheduler is at the
  beginning of the block.  Otherwise, copy the current context into :samp:`{tc}`.

.. hook-end

.. function:: void TARGET_SCHED_SET_SCHED_CONTEXT (void *tc)

  .. hook-start:TARGET_SCHED_SET_SCHED_CONTEXT

  Copy target scheduling context pointed to by :samp:`{tc}` to the current context.

.. hook-end

.. function:: void TARGET_SCHED_CLEAR_SCHED_CONTEXT (void *tc)

  .. hook-start:TARGET_SCHED_CLEAR_SCHED_CONTEXT

  Deallocate internal data in target scheduling context pointed to by :samp:`{tc}`.

.. hook-end

.. function:: void TARGET_SCHED_FREE_SCHED_CONTEXT (void *tc)

  .. hook-start:TARGET_SCHED_FREE_SCHED_CONTEXT

  Deallocate a store for target scheduling context pointed to by :samp:`{tc}`.

.. hook-end

.. function:: int TARGET_SCHED_SPECULATE_INSN (rtx_insn *insn, unsigned int dep_status, rtx *new_pat)

  .. hook-start:TARGET_SCHED_SPECULATE_INSN

  This hook is called by the insn scheduler when :samp:`{insn}` has only
  speculative dependencies and therefore can be scheduled speculatively.
  The hook is used to check if the pattern of :samp:`{insn}` has a speculative
  version and, in case of successful check, to generate that speculative
  pattern.  The hook should return 1, if the instruction has a speculative form,
  or -1, if it doesn't.  :samp:`{request}` describes the type of requested
  speculation.  If the return value equals 1 then :samp:`{new_pat}` is assigned
  the generated speculative pattern.

.. hook-end

.. function:: bool TARGET_SCHED_NEEDS_BLOCK_P (unsigned int dep_status)

  .. hook-start:TARGET_SCHED_NEEDS_BLOCK_P

  This hook is called by the insn scheduler during generation of recovery code
  for :samp:`{insn}`.  It should return ``true``, if the corresponding check
  instruction should branch to recovery code, or ``false`` otherwise.

.. hook-end

.. function:: rtx TARGET_SCHED_GEN_SPEC_CHECK (rtx_insn *insn, rtx_insn *label, unsigned int ds)

  .. hook-start:TARGET_SCHED_GEN_SPEC_CHECK

  This hook is called by the insn scheduler to generate a pattern for recovery
  check instruction.  If :samp:`{mutate_p}` is zero, then :samp:`{insn}` is a
  speculative instruction for which the check should be generated.
  :samp:`{label}` is either a label of a basic block, where recovery code should
  be emitted, or a null pointer, when requested check doesn't branch to
  recovery code (a simple check).  If :samp:`{mutate_p}` is nonzero, then
  a pattern for a branchy check corresponding to a simple check denoted by
  :samp:`{insn}` should be generated.  In this case :samp:`{label}` can't be null.

.. hook-end

.. function:: void TARGET_SCHED_SET_SCHED_FLAGS (struct spec_info_def *spec_info)

  .. hook-start:TARGET_SCHED_SET_SCHED_FLAGS

  This hook is used by the insn scheduler to find out what features should be
  enabled/used.
  The structure \* :samp:`{spec_info}` should be filled in by the target.
  The structure describes speculation types that can be used in the scheduler.

.. hook-end

.. function:: bool TARGET_SCHED_CAN_SPECULATE_INSN (rtx_insn *insn)

  .. hook-start:TARGET_SCHED_CAN_SPECULATE_INSN

  Some instructions should never be speculated by the schedulers, usually
  because the instruction is too expensive to get this wrong.  Often such
  instructions have long latency, and often they are not fully modeled in the
  pipeline descriptions.  This hook should return ``false`` if :samp:`{insn}`
  should not be speculated.

.. hook-end

.. function:: int TARGET_SCHED_SMS_RES_MII (struct ddg *g)

  .. hook-start:TARGET_SCHED_SMS_RES_MII

  This hook is called by the swing modulo scheduler to calculate a
  resource-based lower bound which is based on the resources available in
  the machine and the resources required by each instruction.  The target
  backend can use :samp:`{g}` to calculate such bound.  A very simple lower
  bound will be used in case this hook is not implemented: the total number
  of instructions divided by the issue rate.

.. hook-end

.. function:: bool TARGET_SCHED_DISPATCH (rtx_insn *insn, int x)

  .. hook-start:TARGET_SCHED_DISPATCH

  This hook is called by Haifa Scheduler.  It returns true if dispatch scheduling
  is supported in hardware and the condition specified in the parameter is true.

.. hook-end

.. function:: void TARGET_SCHED_DISPATCH_DO (rtx_insn *insn, int x)

  .. hook-start:TARGET_SCHED_DISPATCH_DO

  This hook is called by Haifa Scheduler.  It performs the operation specified
  in its second parameter.

.. hook-end

.. c:var:: bool TARGET_SCHED_EXPOSED_PIPELINE

  .. hook-start:TARGET_SCHED_EXPOSED_PIPELINE

  True if the processor has an exposed pipeline, which means that not just
  the order of instructions is important for correctness when scheduling, but
  also the latencies of operations.

.. hook-end

.. function:: int TARGET_SCHED_REASSOCIATION_WIDTH (unsigned int opc, machine_mode mode)

  .. hook-start:TARGET_SCHED_REASSOCIATION_WIDTH

  This hook is called by tree reassociator to determine a level of
  parallelism required in output calculations chain.

.. hook-end

.. function:: void TARGET_SCHED_FUSION_PRIORITY (rtx_insn *insn, int max_pri, int *fusion_pri, int *pri)

  .. hook-start:TARGET_SCHED_FUSION_PRIORITY

  This hook is called by scheduling fusion pass.  It calculates fusion
  priorities for each instruction passed in by parameter.  The priorities
  are returned via pointer parameters.

  :samp:`{insn}` is the instruction whose priorities need to be calculated.
  :samp:`{max_pri}` is the maximum priority can be returned in any cases.
  :samp:`{fusion_pri}` is the pointer parameter through which :samp:`{insn}` 's
  fusion priority should be calculated and returned.
  :samp:`{pri}` is the pointer parameter through which :samp:`{insn}` 's priority
  should be calculated and returned.

  Same :samp:`{fusion_pri}` should be returned for instructions which should
  be scheduled together.  Different :samp:`{pri}` should be returned for
  instructions with same :samp:`{fusion_pri}`.  :samp:`{fusion_pri}` is the major
  sort key, :samp:`{pri}` is the minor sort key.  All instructions will be
  scheduled according to the two priorities.  All priorities calculated
  should be between 0 (exclusive) and :samp:`{max_pri}` (inclusive).  To avoid
  false dependencies, :samp:`{fusion_pri}` of instructions which need to be
  scheduled together should be smaller than :samp:`{fusion_pri}` of irrelevant
  instructions.

  Given below example:

  .. code-block:: c++

        ldr r10, [r1, 4]
        add r4, r4, r10
        ldr r15, [r2, 8]
        sub r5, r5, r15
        ldr r11, [r1, 0]
        add r4, r4, r11
        ldr r16, [r2, 12]
        sub r5, r5, r16

  On targets like ARM/AArch64, the two pairs of consecutive loads should be
  merged.  Since peephole2 pass can't help in this case unless consecutive
  loads are actually next to each other in instruction flow.  That's where
  this scheduling fusion pass works.  This hook calculates priority for each
  instruction based on its fustion type, like:

  .. code-block:: c++

        ldr r10, [r1, 4]  ; fusion_pri=99,  pri=96
        add r4, r4, r10   ; fusion_pri=100, pri=100
        ldr r15, [r2, 8]  ; fusion_pri=98,  pri=92
        sub r5, r5, r15   ; fusion_pri=100, pri=100
        ldr r11, [r1, 0]  ; fusion_pri=99,  pri=100
        add r4, r4, r11   ; fusion_pri=100, pri=100
        ldr r16, [r2, 12] ; fusion_pri=98,  pri=88
        sub r5, r5, r16   ; fusion_pri=100, pri=100

  Scheduling fusion pass then sorts all ready to issue instructions according
  to the priorities.  As a result, instructions of same fusion type will be
  pushed together in instruction flow, like:

  .. code-block:: c++

        ldr r11, [r1, 0]
        ldr r10, [r1, 4]
        ldr r15, [r2, 8]
        ldr r16, [r2, 12]
        add r4, r4, r10
        sub r5, r5, r15
        add r4, r4, r11
        sub r5, r5, r16

  Now peephole2 pass can simply merge the two pairs of loads.

  Since scheduling fusion pass relies on peephole2 to do real fusion
  work, it is only enabled by default when peephole2 is in effect.

  This is firstly introduced on ARM/AArch64 targets, please refer to
  the hook implementation for how different fusion types are supported.

.. hook-end

.. function:: void TARGET_EXPAND_DIVMOD_LIBFUNC (rtx libfunc, machine_mode mode, rtx op0, rtx op1, rtx *quot, rtx *rem)

  .. hook-start:TARGET_EXPAND_DIVMOD_LIBFUNC

  Define this hook for enabling divmod transform if the port does not have
  hardware divmod insn but defines target-specific divmod libfuncs.

.. hook-end