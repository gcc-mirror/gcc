/* Instruction scheduling pass.  This file contains definitions used
   internally in the scheduler.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Pointer to data describing the current DFA state.  */
extern state_t curr_state;

/* Forward declaration.  */
struct ready_list;

/* Describe state of dependencies used during sched_analyze phase.  */
struct deps
{
  /* The *_insns and *_mems are paired lists.  Each pending memory operation
     will have a pointer to the MEM rtx on one list and a pointer to the
     containing insn on the other list in the same place in the list.  */

  /* We can't use add_dependence like the old code did, because a single insn
     may have multiple memory accesses, and hence needs to be on the list
     once for each memory access.  Add_dependence won't let you add an insn
     to a list more than once.  */

  /* An INSN_LIST containing all insns with pending read operations.  */
  rtx pending_read_insns;

  /* An EXPR_LIST containing all MEM rtx's which are pending reads.  */
  rtx pending_read_mems;

  /* An INSN_LIST containing all insns with pending write operations.  */
  rtx pending_write_insns;

  /* An EXPR_LIST containing all MEM rtx's which are pending writes.  */
  rtx pending_write_mems;

  /* Indicates the combined length of the two pending lists.  We must prevent
     these lists from ever growing too large since the number of dependencies
     produced is at least O(N*N), and execution time is at least O(4*N*N), as
     a function of the length of these pending lists.  */
  int pending_lists_length;

  /* Length of the pending memory flush list. Large functions with no
     calls may build up extremely large lists.  */
  int pending_flush_length;

  /* The last insn upon which all memory references must depend.
     This is an insn which flushed the pending lists, creating a dependency
     between it and all previously pending memory references.  This creates
     a barrier (or a checkpoint) which no memory reference is allowed to cross.

     This includes all non constant CALL_INSNs.  When we do interprocedural
     alias analysis, this restriction can be relaxed.
     This may also be an INSN that writes memory if the pending lists grow
     too large.  */
  rtx last_pending_memory_flush;

  /* A list of the last function calls we have seen.  We use a list to
     represent last function calls from multiple predecessor blocks.
     Used to prevent register lifetimes from expanding unnecessarily.  */
  rtx last_function_call;

  /* A list of insns which use a pseudo register that does not already
     cross a call.  We create dependencies between each of those insn
     and the next call insn, to ensure that they won't cross a call after
     scheduling is done.  */
  rtx sched_before_next_call;

  /* Used to keep post-call psuedo/hard reg movements together with
     the call.  */
  bool in_post_call_group_p;

  /* Set to the tail insn of the outermost libcall block.

     When nonzero, we will mark each insn processed by sched_analyze_insn
     with SCHED_GROUP_P to ensure libcalls are scheduled as a unit.  */
  rtx libcall_block_tail_insn;

  /* The maximum register number for the following arrays.  Before reload
     this is max_reg_num; after reload it is FIRST_PSEUDO_REGISTER.  */
  int max_reg;

  /* Element N is the next insn that sets (hard or pseudo) register
     N within the current basic block; or zero, if there is no
     such insn.  Needed for new registers which may be introduced
     by splitting insns.  */
  struct deps_reg
    {
      rtx uses;
      rtx sets;
      rtx clobbers;
      int uses_length;
      int clobbers_length;
    } *reg_last;

  /* Element N is set for each register that has any nonzero element
     in reg_last[N].{uses,sets,clobbers}.  */
  regset_head reg_last_in_use;
};

/* This structure holds some state of the current scheduling pass, and
   contains some function pointers that abstract out some of the non-generic
   functionality from functions such as schedule_block or schedule_insn.
   There is one global variable, current_sched_info, which points to the
   sched_info structure currently in use.  */
struct sched_info
{
  /* Add all insns that are initially ready to the ready list.  Called once
     before scheduling a set of insns.  */
  void (*init_ready_list) PARAMS ((struct ready_list *));
  /* Called after taking an insn from the ready list.  Returns nonzero if
     this insn can be scheduled, nonzero if we should silently discard it.  */
  int (*can_schedule_ready_p) PARAMS ((rtx));
  /* Return nonzero if there are more insns that should be scheduled.  */
  int (*schedule_more_p) PARAMS ((void));
  /* Called after an insn has all its dependencies resolved.  Return nonzero
     if it should be moved to the ready list or the queue, or zero if we
     should silently discard it.  */
  int (*new_ready) PARAMS ((rtx));
  /* Compare priority of two insns.  Return a positive number if the second
     insn is to be preferred for scheduling, and a negative one if the first
     is to be preferred.  Zero if they are equally good.  */
  int (*rank) PARAMS ((rtx, rtx));
  /* Return a string that contains the insn uid and optionally anything else
     necessary to identify this insn in an output.  It's valid to use a
     static buffer for this.  The ALIGNED parameter should cause the string
     to be formatted so that multiple output lines will line up nicely.  */
  const char *(*print_insn) PARAMS ((rtx, int));
  /* Return nonzero if an insn should be included in priority
     calculations.  */
  int (*contributes_to_priority) PARAMS ((rtx, rtx));
  /* Called when computing dependencies for a JUMP_INSN.  This function
     should store the set of registers that must be considered as set by
     the jump in the regset.  */
  void (*compute_jump_reg_dependencies) PARAMS ((rtx, regset));

  /* The boundaries of the set of insns to be scheduled.  */
  rtx prev_head, next_tail;

  /* Filled in after the schedule is finished; the first and last scheduled
     insns.  */
  rtx head, tail;

  /* If nonzero, enables an additional sanity check in schedule_block.  */
  unsigned int queue_must_finish_empty:1;
  /* Nonzero if we should use cselib for better alias analysis.  This
     must be 0 if the dependency information is used after sched_analyze
     has completed, e.g. if we're using it to initialize state for successor
     blocks in region scheduling.  */
  unsigned int use_cselib:1;
};

extern struct sched_info *current_sched_info;

/* Indexed by INSN_UID, the collection of all data associated with
   a single instruction.  */

struct haifa_insn_data
{
  /* A list of insns which depend on the instruction.  Unlike LOG_LINKS,
     it represents forward dependencies.  */
  rtx depend;

  /* The line number note in effect for each insn.  For line number
     notes, this indicates whether the note may be reused.  */
  rtx line_note;

  /* Logical uid gives the original ordering of the insns.  */
  int luid;

  /* A priority for each insn.  */
  int priority;

  /* The number of incoming edges in the forward dependency graph.
     As scheduling proceds, counts are decreased.  An insn moves to
     the ready queue when its counter reaches zero.  */
  int dep_count;

  /* An encoding of the blockage range function.  Both unit and range
     are coded.  This member is used only for old pipeline interface.  */
  unsigned int blockage;

  /* Number of instructions referring to this insn.  */
  int ref_count;

  /* The minimum clock tick at which the insn becomes ready.  This is
     used to note timing constraints for the insns in the pending list.  */
  int tick;

  short cost;

  /* An encoding of the function units used.  This member is used only
     for old pipeline interface.  */
  short units;

  /* This weight is an estimation of the insn's contribution to
     register pressure.  */
  short reg_weight;

  /* Some insns (e.g. call) are not allowed to move across blocks.  */
  unsigned int cant_move : 1;

  /* Set if there's DEF-USE dependence between some speculatively
     moved load insn and this one.  */
  unsigned int fed_by_spec_load : 1;
  unsigned int is_load_insn : 1;

  /* Nonzero if priority has been computed already.  */
  unsigned int priority_known : 1;
};

extern struct haifa_insn_data *h_i_d;

/* Accessor macros for h_i_d.  There are more in haifa-sched.c and
   sched-rgn.c.  */
#define INSN_DEPEND(INSN)	(h_i_d[INSN_UID (INSN)].depend)
#define INSN_LUID(INSN)		(h_i_d[INSN_UID (INSN)].luid)
#define CANT_MOVE(insn)		(h_i_d[INSN_UID (insn)].cant_move)
#define INSN_DEP_COUNT(INSN)	(h_i_d[INSN_UID (INSN)].dep_count)
#define INSN_PRIORITY(INSN)	(h_i_d[INSN_UID (INSN)].priority)
#define INSN_PRIORITY_KNOWN(INSN) (h_i_d[INSN_UID (INSN)].priority_known)
#define INSN_COST(INSN)		(h_i_d[INSN_UID (INSN)].cost)
#define INSN_UNIT(INSN)		(h_i_d[INSN_UID (INSN)].units)
#define INSN_REG_WEIGHT(INSN)	(h_i_d[INSN_UID (INSN)].reg_weight)

#define INSN_BLOCKAGE(INSN)	(h_i_d[INSN_UID (INSN)].blockage)
#define UNIT_BITS		5
#define BLOCKAGE_MASK		((1 << BLOCKAGE_BITS) - 1)
#define ENCODE_BLOCKAGE(U, R)			\
  (((U) << BLOCKAGE_BITS			\
    | MIN_BLOCKAGE_COST (R)) << BLOCKAGE_BITS	\
   | MAX_BLOCKAGE_COST (R))
#define UNIT_BLOCKED(B)		((B) >> (2 * BLOCKAGE_BITS))
#define BLOCKAGE_RANGE(B)                                                \
  (((((B) >> BLOCKAGE_BITS) & BLOCKAGE_MASK) << (HOST_BITS_PER_INT / 2)) \
   | ((B) & BLOCKAGE_MASK))

/* Encodings of the `<name>_unit_blockage_range' function.  */
#define MIN_BLOCKAGE_COST(R) ((R) >> (HOST_BITS_PER_INT / 2))
#define MAX_BLOCKAGE_COST(R) ((R) & ((1 << (HOST_BITS_PER_INT / 2)) - 1))

extern FILE *sched_dump;
extern int sched_verbose;

#ifndef __GNUC__
#define __inline
#endif

#ifndef HAIFA_INLINE
#define HAIFA_INLINE __inline
#endif

/* Functions in sched-vis.c.  */
extern void init_target_units PARAMS ((void));
extern void insn_print_units PARAMS ((rtx));
extern void init_block_visualization PARAMS ((void));
extern void print_block_visualization PARAMS ((const char *));
extern void visualize_scheduled_insns PARAMS ((int));
extern void visualize_no_unit PARAMS ((rtx));
extern void visualize_stall_cycles PARAMS ((int));
extern void visualize_alloc PARAMS ((void));
extern void visualize_free PARAMS ((void));

/* Functions in sched-deps.c.  */
extern void add_dependence PARAMS ((rtx, rtx, enum reg_note));
extern void add_insn_mem_dependence PARAMS ((struct deps *, rtx *, rtx *, rtx,
					     rtx));
extern void sched_analyze PARAMS ((struct deps *, rtx, rtx));
extern void init_deps PARAMS ((struct deps *));
extern void free_deps PARAMS ((struct deps *));
extern void init_deps_global PARAMS ((void));
extern void finish_deps_global PARAMS ((void));
extern void compute_forward_dependences PARAMS ((rtx, rtx));
extern rtx find_insn_list PARAMS ((rtx, rtx));
extern void init_dependency_caches PARAMS ((int));
extern void free_dependency_caches PARAMS ((void));

/* Functions in haifa-sched.c.  */
extern void get_block_head_tail PARAMS ((int, rtx *, rtx *));
extern int no_real_insns_p PARAMS ((rtx, rtx));

extern void rm_line_notes PARAMS ((rtx, rtx));
extern void save_line_notes PARAMS ((int, rtx, rtx));
extern void restore_line_notes PARAMS ((rtx, rtx));
extern void rm_redundant_line_notes PARAMS ((void));
extern void rm_other_notes PARAMS ((rtx, rtx));

extern int insn_issue_delay PARAMS ((rtx));
extern int set_priorities PARAMS ((rtx, rtx));

extern rtx sched_emit_insn PARAMS ((rtx));
extern void schedule_block PARAMS ((int, int));
extern void sched_init PARAMS ((FILE *));
extern void sched_finish PARAMS ((void));

extern void ready_add PARAMS ((struct ready_list *, rtx));

/* The following are exported for the benefit of debugging functions.  It
   would be nicer to keep them private to haifa-sched.c.  */
extern int insn_unit PARAMS ((rtx));
extern int insn_cost PARAMS ((rtx, rtx, rtx));
extern rtx get_unit_last_insn PARAMS ((int));
extern int actual_hazard_this_instance PARAMS ((int, int, rtx, int, int));
extern void print_insn PARAMS ((char *, rtx, int));
