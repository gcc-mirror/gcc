/* Data flow analysis for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file contains the data flow analysis pass of the compiler.  It
   computes data flow information which tells combine_instructions
   which insns to consider combining and controls register allocation.

   Additional data flow information that is too bulky to record is
   generated during the analysis, and is used at that time to create
   autoincrement and autodecrement addressing.

   The first step is dividing the function into basic blocks.
   find_basic_blocks does this.  Then life_analysis determines
   where each register is live and where it is dead.

   ** find_basic_blocks **

   find_basic_blocks divides the current function's rtl into basic
   blocks and constructs the CFG.  The blocks are recorded in the
   basic_block_info array; the CFG exists in the edge structures
   referenced by the blocks.

   find_basic_blocks also finds any unreachable loops and deletes them.

   ** life_analysis **

   life_analysis is called immediately after find_basic_blocks.
   It uses the basic block information to determine where each
   hard or pseudo register is live.

   ** live-register info **

   The information about where each register is live is in two parts:
   the REG_NOTES of insns, and the vector basic_block->global_live_at_start.

   basic_block->global_live_at_start has an element for each basic
   block, and the element is a bit-vector with a bit for each hard or
   pseudo register.  The bit is 1 if the register is live at the
   beginning of the basic block.

   Two types of elements can be added to an insn's REG_NOTES.
   A REG_DEAD note is added to an insn's REG_NOTES for any register
   that meets both of two conditions:  The value in the register is not
   needed in subsequent insns and the insn does not replace the value in
   the register (in the case of multi-word hard registers, the value in
   each register must be replaced by the insn to avoid a REG_DEAD note).

   In the vast majority of cases, an object in a REG_DEAD note will be
   used somewhere in the insn.  The (rare) exception to this is if an
   insn uses a multi-word hard register and only some of the registers are
   needed in subsequent insns.  In that case, REG_DEAD notes will be
   provided for those hard registers that are not subsequently needed.
   Partial REG_DEAD notes of this type do not occur when an insn sets
   only some of the hard registers used in such a multi-word operand;
   omitting REG_DEAD notes for objects stored in an insn is optional and
   the desire to do so does not justify the complexity of the partial
   REG_DEAD notes.

   REG_UNUSED notes are added for each register that is set by the insn
   but is unused subsequently (if every register set by the insn is unused
   and the insn does not reference memory or have some other side-effect,
   the insn is deleted instead).  If only part of a multi-word hard
   register is used in a subsequent insn, REG_UNUSED notes are made for
   the parts that will not be used.

   To determine which registers are live after any insn, one can
   start from the beginning of the basic block and scan insns, noting
   which registers are set by each insn and which die there.

   ** Other actions of life_analysis **

   life_analysis sets up the LOG_LINKS fields of insns because the
   information needed to do so is readily available.

   life_analysis deletes insns whose only effect is to store a value
   that is never used.

   life_analysis notices cases where a reference to a register as
   a memory address can be combined with a preceding or following
   incrementation or decrementation of the register.  The separate
   instruction to increment or decrement is deleted and the address
   is changed to a POST_INC or similar rtx.

   Each time an incrementing or decrementing address is created,
   a REG_INC element is added to the insn's REG_NOTES list.

   life_analysis fills in certain vectors containing information about
   register usage: REG_N_REFS, REG_N_DEATHS, REG_N_SETS, REG_LIVE_LENGTH,
   REG_N_CALLS_CROSSED and REG_BASIC_BLOCK.

   life_analysis sets current_function_sp_is_unchanging if the function
   doesn't modify the stack pointer.  */

/* TODO:

   Split out from life_analysis:
	- local property discovery (bb->local_live, bb->local_set)
	- global property computation
	- log links creation
	- pre/post modify transformation
*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "expr.h"
#include "ssa.h"
#include "timevar.h"

#include "obstack.h"
#include "splay-tree.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#ifndef EXIT_IGNORE_STACK
#define EXIT_IGNORE_STACK 0
#endif

#ifndef HAVE_epilogue
#define HAVE_epilogue 0
#endif
#ifndef HAVE_prologue
#define HAVE_prologue 0
#endif
#ifndef HAVE_sibcall_epilogue
#define HAVE_sibcall_epilogue 0
#endif

#ifndef LOCAL_REGNO
#define LOCAL_REGNO(REGNO)  0
#endif
#ifndef EPILOGUE_USES
#define EPILOGUE_USES(REGNO)  0
#endif

#ifdef HAVE_conditional_execution
#ifndef REVERSE_CONDEXEC_PREDICATES_P
#define REVERSE_CONDEXEC_PREDICATES_P(x, y) ((x) == reverse_condition (y))
#endif
#endif

/* The obstack on which the flow graph components are allocated.  */

struct obstack flow_obstack;
static char *flow_firstobj;

/* Number of basic blocks in the current function.  */

int n_basic_blocks;

/* Number of edges in the current function.  */

int n_edges;

/* The basic block array.  */

varray_type basic_block_info;

/* The special entry and exit blocks.  */

struct basic_block_def entry_exit_blocks[2]
= {{NULL,			/* head */
    NULL,			/* end */
    NULL,			/* head_tree */
    NULL,			/* end_tree */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* cond_local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    ENTRY_BLOCK,		/* index */
    0,				/* loop_depth */
    0,				/* count */
    0				/* frequency */
  },
  {
    NULL,			/* head */
    NULL,			/* end */
    NULL,			/* head_tree */
    NULL,			/* end_tree */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* cond_local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    EXIT_BLOCK,			/* index */
    0,				/* loop_depth */
    0,				/* count */
    0				/* frequency */
  }
};

/* Nonzero if the second flow pass has completed.  */
int flow2_completed;

/* Maximum register number used in this function, plus one.  */

int max_regno;

/* Indexed by n, giving various register information */

varray_type reg_n_info;

/* Size of a regset for the current function,
   in (1) bytes and (2) elements.  */

int regset_bytes;
int regset_size;

/* Regset of regs live when calls to `setjmp'-like functions happen.  */
/* ??? Does this exist only for the setjmp-clobbered warning message?  */

regset regs_live_at_setjmp;

/* List made of EXPR_LIST rtx's which gives pairs of pseudo registers
   that have to go in the same hard reg.
   The first two regs in the list are a pair, and the next two
   are another pair, etc.  */
rtx regs_may_share;

/* Callback that determines if it's ok for a function to have no
   noreturn attribute.  */
int (*lang_missing_noreturn_ok_p) PARAMS ((tree));

/* Set of registers that may be eliminable.  These are handled specially
   in updating regs_ever_live.  */

static HARD_REG_SET elim_reg_set;

/* The basic block structure for every insn, indexed by uid.  */

varray_type basic_block_for_insn;

/* The labels mentioned in non-jump rtl.  Valid during find_basic_blocks.  */
/* ??? Should probably be using LABEL_NUSES instead.  It would take a
   bit of surgery to be able to use or co-opt the routines in jump.  */

static rtx label_value_list;
static rtx tail_recursion_label_list;

/* Holds information for tracking conditional register life information.  */
struct reg_cond_life_info
{
  /* A boolean expression of conditions under which a register is dead.  */
  rtx condition;
  /* Conditions under which a register is dead at the basic block end.  */
  rtx orig_condition;

  /* A boolean expression of conditions under which a register has been
     stored into.  */
  rtx stores;

  /* ??? Could store mask of bytes that are dead, so that we could finally
     track lifetimes of multi-word registers accessed via subregs.  */
};

/* For use in communicating between propagate_block and its subroutines.
   Holds all information needed to compute life and def-use information.  */

struct propagate_block_info
{
  /* The basic block we're considering.  */
  basic_block bb;

  /* Bit N is set if register N is conditionally or unconditionally live.  */
  regset reg_live;

  /* Bit N is set if register N is set this insn.  */
  regset new_set;

  /* Element N is the next insn that uses (hard or pseudo) register N
     within the current basic block; or zero, if there is no such insn.  */
  rtx *reg_next_use;

  /* Contains a list of all the MEMs we are tracking for dead store
     elimination.  */
  rtx mem_set_list;

  /* If non-null, record the set of registers set unconditionally in the
     basic block.  */
  regset local_set;

  /* If non-null, record the set of registers set conditionally in the
     basic block.  */
  regset cond_local_set;

#ifdef HAVE_conditional_execution
  /* Indexed by register number, holds a reg_cond_life_info for each
     register that is not unconditionally live or dead.  */
  splay_tree reg_cond_dead;

  /* Bit N is set if register N is in an expression in reg_cond_dead.  */
  regset reg_cond_reg;
#endif

  /* The length of mem_set_list.  */
  int mem_set_list_len;

  /* Non-zero if the value of CC0 is live.  */
  int cc0_live;

  /* Flags controling the set of information propagate_block collects.  */
  int flags;
};

/* Maximum length of pbi->mem_set_list before we start dropping
   new elements on the floor.  */
#define MAX_MEM_SET_LIST_LEN	100

/* Store the data structures necessary for depth-first search.  */
struct depth_first_search_dsS {
  /* stack for backtracking during the algorithm */
  basic_block *stack;

  /* number of edges in the stack.  That is, positions 0, ..., sp-1
     have edges.  */
  unsigned int sp;

  /* record of basic blocks already seen by depth-first search */
  sbitmap visited_blocks;
};
typedef struct depth_first_search_dsS *depth_first_search_ds;

/* Have print_rtl_and_abort give the same information that fancy_abort
   does.  */
#define print_rtl_and_abort() \
  print_rtl_and_abort_fcn (__FILE__, __LINE__, __FUNCTION__)

/* Forward declarations */
static bool try_crossjump_to_edge	PARAMS ((int, edge, edge));
static bool try_crossjump_bb		PARAMS ((int, basic_block));
static bool outgoing_edges_match	PARAMS ((basic_block, basic_block));
static int flow_find_cross_jump		PARAMS ((int, basic_block, basic_block,
						 rtx *, rtx *));
static int count_basic_blocks		PARAMS ((rtx));
static void find_basic_blocks_1		PARAMS ((rtx));
static rtx find_label_refs		PARAMS ((rtx, rtx));
static void make_edges			PARAMS ((rtx, int, int, int));
static void make_label_edge		PARAMS ((sbitmap *, basic_block,
						 rtx, int));
static void make_eh_edge		PARAMS ((sbitmap *, basic_block, rtx));

static void commit_one_edge_insertion	PARAMS ((edge));

static void delete_unreachable_blocks	PARAMS ((void));
static int can_delete_note_p		PARAMS ((rtx));
static void expunge_block		PARAMS ((basic_block));
static int can_delete_label_p		PARAMS ((rtx));
static int tail_recursion_label_p	PARAMS ((rtx));
static int merge_blocks_move_predecessor_nojumps PARAMS ((basic_block,
							  basic_block));
static int merge_blocks_move_successor_nojumps PARAMS ((basic_block,
							basic_block));
static int merge_blocks			PARAMS ((edge,basic_block,basic_block,
						 int));
static bool try_optimize_cfg		PARAMS ((int));
static bool can_fallthru		PARAMS ((basic_block, basic_block));
static bool try_redirect_by_replacing_jump PARAMS ((edge, basic_block));
static bool try_simplify_condjump	PARAMS ((basic_block));
static bool try_forward_edges		PARAMS ((int, basic_block));
static void tidy_fallthru_edges		PARAMS ((void));
static int verify_wide_reg_1		PARAMS ((rtx *, void *));
static void verify_wide_reg		PARAMS ((int, rtx, rtx));
static void verify_local_live_at_start	PARAMS ((regset, basic_block));
static void notice_stack_pointer_modification_1 PARAMS ((rtx, rtx, void *));
static void notice_stack_pointer_modification PARAMS ((rtx));
static void mark_reg			PARAMS ((rtx, void *));
static void mark_regs_live_at_end	PARAMS ((regset));
static int set_phi_alternative_reg      PARAMS ((rtx, int, int, void *));
static void calculate_global_regs_live	PARAMS ((sbitmap, sbitmap, int));
static void propagate_block_delete_insn PARAMS ((basic_block, rtx));
static rtx propagate_block_delete_libcall PARAMS ((basic_block, rtx, rtx));
static int insn_dead_p			PARAMS ((struct propagate_block_info *,
						 rtx, int, rtx));
static int libcall_dead_p		PARAMS ((struct propagate_block_info *,
						 rtx, rtx));
static void mark_set_regs		PARAMS ((struct propagate_block_info *,
						 rtx, rtx));
static void mark_set_1			PARAMS ((struct propagate_block_info *,
						 enum rtx_code, rtx, rtx,
						 rtx, int));
#ifdef HAVE_conditional_execution
static int mark_regno_cond_dead		PARAMS ((struct propagate_block_info *,
						 int, rtx));
static void free_reg_cond_life_info	PARAMS ((splay_tree_value));
static int flush_reg_cond_reg_1		PARAMS ((splay_tree_node, void *));
static void flush_reg_cond_reg		PARAMS ((struct propagate_block_info *,
						 int));
static rtx elim_reg_cond		PARAMS ((rtx, unsigned int));
static rtx ior_reg_cond			PARAMS ((rtx, rtx, int));
static rtx not_reg_cond			PARAMS ((rtx));
static rtx and_reg_cond			PARAMS ((rtx, rtx, int));
#endif
#ifdef AUTO_INC_DEC
static void attempt_auto_inc		PARAMS ((struct propagate_block_info *,
						 rtx, rtx, rtx, rtx, rtx));
static void find_auto_inc		PARAMS ((struct propagate_block_info *,
						 rtx, rtx));
static int try_pre_increment_1		PARAMS ((struct propagate_block_info *,
						 rtx));
static int try_pre_increment		PARAMS ((rtx, rtx, HOST_WIDE_INT));
#endif
static void mark_used_reg		PARAMS ((struct propagate_block_info *,
						 rtx, rtx, rtx));
static void mark_used_regs		PARAMS ((struct propagate_block_info *,
						 rtx, rtx, rtx));
void dump_flow_info			PARAMS ((FILE *));
void debug_flow_info			PARAMS ((void));
static void print_rtl_and_abort_fcn	PARAMS ((const char *, int,
						 const char *))
					ATTRIBUTE_NORETURN;

static void add_to_mem_set_list		PARAMS ((struct propagate_block_info *,
						 rtx));
static void invalidate_mems_from_autoinc PARAMS ((struct propagate_block_info *,
						  rtx));
static void invalidate_mems_from_set	PARAMS ((struct propagate_block_info *,
						 rtx));
static void remove_fake_successors	PARAMS ((basic_block));
static void flow_nodes_print		PARAMS ((const char *, const sbitmap,
						 FILE *));
static void flow_edge_list_print	PARAMS ((const char *, const edge *,
						 int, FILE *));
static void flow_loops_cfg_dump		PARAMS ((const struct loops *,
						 FILE *));
static int flow_loop_nested_p		PARAMS ((struct loop *,
						 struct loop *));
static int flow_loop_entry_edges_find	PARAMS ((basic_block, const sbitmap,
						 edge **));
static int flow_loop_exit_edges_find	PARAMS ((const sbitmap, edge **));
static int flow_loop_nodes_find	PARAMS ((basic_block, basic_block, sbitmap));
static void flow_dfs_compute_reverse_init
  PARAMS ((depth_first_search_ds));
static void flow_dfs_compute_reverse_add_bb
  PARAMS ((depth_first_search_ds, basic_block));
static basic_block flow_dfs_compute_reverse_execute
  PARAMS ((depth_first_search_ds));
static void flow_dfs_compute_reverse_finish
  PARAMS ((depth_first_search_ds));
static void flow_loop_pre_header_scan PARAMS ((struct loop *));
static basic_block flow_loop_pre_header_find PARAMS ((basic_block,
						      const sbitmap *));
static void flow_loop_tree_node_add	PARAMS ((struct loop *, struct loop *));
static void flow_loops_tree_build	PARAMS ((struct loops *));
static int flow_loop_level_compute	PARAMS ((struct loop *, int));
static int flow_loops_level_compute	PARAMS ((struct loops *));
static void delete_dead_jumptables	PARAMS ((void));
static bool back_edge_of_syntactic_loop_p PARAMS ((basic_block, basic_block));
static bool need_fake_edge_p		PARAMS ((rtx));

/* Find basic blocks of the current function.
   F is the first insn of the function and NREGS the number of register
   numbers in use.  */

void
find_basic_blocks (f, nregs, file)
     rtx f;
     int nregs ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
{
  int max_uid;
  timevar_push (TV_CFG);

  /* Flush out existing data.  */
  if (basic_block_info != NULL)
    {
      int i;

      clear_edges ();

      /* Clear bb->aux on all extant basic blocks.  We'll use this as a
	 tag for reuse during create_basic_block, just in case some pass
	 copies around basic block notes improperly.  */
      for (i = 0; i < n_basic_blocks; ++i)
	BASIC_BLOCK (i)->aux = NULL;

      VARRAY_FREE (basic_block_info);
    }

  n_basic_blocks = count_basic_blocks (f);

  /* Size the basic block table.  The actual structures will be allocated
     by find_basic_blocks_1, since we want to keep the structure pointers
     stable across calls to find_basic_blocks.  */
  /* ??? This whole issue would be much simpler if we called find_basic_blocks
     exactly once, and thereafter we don't have a single long chain of
     instructions at all until close to the end of compilation when we
     actually lay them out.  */

  VARRAY_BB_INIT (basic_block_info, n_basic_blocks, "basic_block_info");

  find_basic_blocks_1 (f);

  /* Record the block to which an insn belongs.  */
  /* ??? This should be done another way, by which (perhaps) a label is
     tagged directly with the basic block that it starts.  It is used for
     more than that currently, but IMO that is the only valid use.  */

  max_uid = get_max_uid ();
#ifdef AUTO_INC_DEC
  /* Leave space for insns life_analysis makes in some cases for auto-inc.
     These cases are rare, so we don't need too much space.  */
  max_uid += max_uid / 10;
#endif

  compute_bb_for_insn (max_uid);

  /* Discover the edges of our cfg.  */
  make_edges (label_value_list, 0, n_basic_blocks - 1, 0);

  /* Do very simple cleanup now, for the benefit of code that runs between
     here and cleanup_cfg, e.g. thread_prologue_and_epilogue_insns.  */
  tidy_fallthru_edges ();

  mark_critical_edges ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  timevar_pop (TV_CFG);
}

void
check_function_return_warnings ()
{
  if (warn_missing_noreturn
      && !TREE_THIS_VOLATILE (cfun->decl)
      && EXIT_BLOCK_PTR->pred == NULL
      && (lang_missing_noreturn_ok_p
	  && !lang_missing_noreturn_ok_p (cfun->decl)))
    warning ("function might be possible candidate for attribute `noreturn'");

  /* If we have a path to EXIT, then we do return.  */
  if (TREE_THIS_VOLATILE (cfun->decl)
      && EXIT_BLOCK_PTR->pred != NULL)
    warning ("`noreturn' function does return");

  /* If the clobber_return_insn appears in some basic block, then we
     do reach the end without returning a value.  */
  else if (warn_return_type
	   && cfun->x_clobber_return_insn != NULL
	   && EXIT_BLOCK_PTR->pred != NULL)
    {
      int max_uid = get_max_uid ();

      /* If clobber_return_insn was excised by jump1, then renumber_insns
	 can make max_uid smaller than the number still recorded in our rtx.
	 That's fine, since this is a quick way of verifying that the insn
	 is no longer in the chain.  */
      if (INSN_UID (cfun->x_clobber_return_insn) < max_uid)
	{
	  /* Recompute insn->block mapping, since the initial mapping is
	     set before we delete unreachable blocks.  */
	  compute_bb_for_insn (max_uid);

	  if (BLOCK_FOR_INSN (cfun->x_clobber_return_insn) != NULL)
	    warning ("control reaches end of non-void function");
	}
    }
}

/* Count the basic blocks of the function.  */

static int
count_basic_blocks (f)
     rtx f;
{
  register rtx insn;
  register RTX_CODE prev_code;
  register int count = 0;
  int saw_abnormal_edge = 0;

  prev_code = JUMP_INSN;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      enum rtx_code code = GET_CODE (insn);

      if (code == CODE_LABEL
	  || (GET_RTX_CLASS (code) == 'i'
	      && (prev_code == JUMP_INSN
		  || prev_code == BARRIER
		  || saw_abnormal_edge)))
	{
	  saw_abnormal_edge = 0;
	  count++;
	}

      /* Record whether this insn created an edge.  */
      if (code == CALL_INSN)
	{
	  rtx note;

	  /* If there is a nonlocal goto label and the specified
	     region number isn't -1, we have an edge.  */
	  if (nonlocal_goto_handler_labels
	      && ((note = find_reg_note (insn, REG_EH_REGION, NULL_RTX)) == 0
		  || INTVAL (XEXP (note, 0)) >= 0))
	    saw_abnormal_edge = 1;

	  else if (can_throw_internal (insn))
	    saw_abnormal_edge = 1;
	}
      else if (flag_non_call_exceptions
	       && code == INSN
	       && can_throw_internal (insn))
	saw_abnormal_edge = 1;

      if (code != NOTE)
	prev_code = code;
    }

  /* The rest of the compiler works a bit smoother when we don't have to
     check for the edge case of do-nothing functions with no basic blocks.  */
  if (count == 0)
    {
      emit_insn (gen_rtx_USE (VOIDmode, const0_rtx));
      count = 1;
    }

  return count;
}

/* Scan a list of insns for labels referred to other than by jumps.
   This is used to scan the alternatives of a call placeholder.  */
static rtx
find_label_refs (f, lvl)
     rtx f;
     rtx lvl;
{
  rtx insn;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && GET_CODE (insn) != JUMP_INSN)
      {
	rtx note;

	/* Make a list of all labels referred to other than by jumps
	   (which just don't have the REG_LABEL notes).

	   Make a special exception for labels followed by an ADDR*VEC,
	   as this would be a part of the tablejump setup code.

	   Make a special exception to registers loaded with label
	   values just before jump insns that use them.  */

	for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	  if (REG_NOTE_KIND (note) == REG_LABEL)
	    {
	      rtx lab = XEXP (note, 0), next;

	      if ((next = next_nonnote_insn (lab)) != NULL
		       && GET_CODE (next) == JUMP_INSN
		       && (GET_CODE (PATTERN (next)) == ADDR_VEC
			   || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
		;
	      else if (GET_CODE (lab) == NOTE)
		;
	      else if (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
		       && find_reg_note (NEXT_INSN (insn), REG_LABEL, lab))
		;
	      else
		lvl = alloc_EXPR_LIST (0, XEXP (note, 0), lvl);
	    }
      }

  return lvl;
}

/* Assume that someone emitted code with control flow instructions to the
   basic block.  Update the data structure.  */
void
find_sub_basic_blocks (bb)
     basic_block bb;
{
  rtx insn = bb->head;
  rtx end = bb->end;
  rtx jump_insn = NULL_RTX;
  edge falltru = 0;
  basic_block first_bb = bb;
  int i;

  if (insn == bb->end)
    return;

  if (GET_CODE (insn) == CODE_LABEL)
    insn = NEXT_INSN (insn);

  /* Scan insn chain and try to find new basic block boundaries.  */
  while (1)
    {
      enum rtx_code code = GET_CODE (insn);
      switch (code)
	{
	case BARRIER:
	  if (!jump_insn)
	    abort ();
	  break;
	/* On code label, split current basic block.  */
	case CODE_LABEL:
	  falltru = split_block (bb, PREV_INSN (insn));
	  if (jump_insn)
	    bb->end = jump_insn;
	  bb = falltru->dest;
	  remove_edge (falltru);
	  jump_insn = 0;
	  if (LABEL_ALTERNATE_NAME (insn))
	    make_edge (NULL, ENTRY_BLOCK_PTR, bb, 0);
	  break;
	case INSN:
	case JUMP_INSN:
	  /* In case we've previously split insn on the JUMP_INSN, move the
	     block header to proper place.  */
	  if (jump_insn)
	    {
	      falltru = split_block (bb, PREV_INSN (insn));
	      bb->end = jump_insn;
	      bb = falltru->dest;
	      remove_edge (falltru);
	      jump_insn = 0;
	    }
	  /* We need some special care for those expressions.  */
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (GET_CODE (PATTERN (insn)) == ADDR_VEC
		  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
		abort();
	      jump_insn = insn;
	    }
	  break;
	default:
	  break;
	}
      if (insn == end)
	break;
      insn = NEXT_INSN (insn);
    }

  /* In case expander replaced normal insn by sequence terminating by
     return and barrier, or possibly other sequence not behaving like
     ordinary jump, we need to take care and move basic block boundary.  */
  if (jump_insn && GET_CODE (bb->end) != JUMP_INSN)
    bb->end = jump_insn;

  /* We've possibly replaced the conditional jump by conditional jump
     followed by cleanup at fallthru edge, so the outgoing edges may
     be dead.  */
  purge_dead_edges (bb);

  /* Now re-scan and wire in all edges.  This expect simple (conditional)
     jumps at the end of each new basic blocks.  */
  make_edges (NULL, first_bb->index, bb->index, 1);

  /* Update branch probabilities.  Expect only (un)conditional jumps
     to be created with only the forward edges.  */
  for (i = first_bb->index; i <= bb->index; i++)
    {
      edge e,f;
      basic_block b = BASIC_BLOCK (i);
      if (b != first_bb)
	{
	  b->count = 0;
	  b->frequency = 0;
	  for (e = b->pred; e; e=e->pred_next)
	    {
	      b->count += e->count;
	      b->frequency += EDGE_FREQUENCY (e);
	    }
	}
      if (b->succ && b->succ->succ_next && !b->succ->succ_next->succ_next)
	{
	  rtx note = find_reg_note (b->end, REG_BR_PROB, NULL);
	  int probability;

	  if (!note)
	    continue;
	  probability = INTVAL (XEXP (find_reg_note (b->end,
						     REG_BR_PROB,
						     NULL), 0));
	  e = BRANCH_EDGE (b);
	  e->probability = probability;
	  e->count = ((b->count * probability + REG_BR_PROB_BASE / 2)
		      / REG_BR_PROB_BASE);
	  f = FALLTHRU_EDGE (b);
	  f->probability = REG_BR_PROB_BASE - probability;
	  f->count = b->count - e->count;
	}
      if (b->succ && !b->succ->succ_next)
	{
	  e = b->succ;
	  e->probability = REG_BR_PROB_BASE;
	  e->count = b->count;
	}
    }
}

/* Find all basic blocks of the function whose first insn is F.

   Collect and return a list of labels whose addresses are taken.  This
   will be used in make_edges for use with computed gotos.  */

static void
find_basic_blocks_1 (f)
     rtx f;
{
  register rtx insn, next;
  int i = 0;
  rtx bb_note = NULL_RTX;
  rtx lvl = NULL_RTX;
  rtx trll = NULL_RTX;
  rtx head = NULL_RTX;
  rtx end = NULL_RTX;

  /* We process the instructions in a slightly different way than we did
     previously.  This is so that we see a NOTE_BASIC_BLOCK after we have
     closed out the previous block, so that it gets attached at the proper
     place.  Since this form should be equivalent to the previous,
     count_basic_blocks continues to use the old form as a check.  */

  for (insn = f; insn; insn = next)
    {
      enum rtx_code code = GET_CODE (insn);

      next = NEXT_INSN (insn);

      switch (code)
	{
	case NOTE:
	  {
	    int kind = NOTE_LINE_NUMBER (insn);

	    /* Look for basic block notes with which to keep the
	       basic_block_info pointers stable.  Unthread the note now;
	       we'll put it back at the right place in create_basic_block.
	       Or not at all if we've already found a note in this block.  */
	    if (kind == NOTE_INSN_BASIC_BLOCK)
	      {
		if (bb_note == NULL_RTX)
		  bb_note = insn;
		else
		  next = flow_delete_insn (insn);
	      }
	    break;
	  }

	case CODE_LABEL:
	  /* A basic block starts at a label.  If we've closed one off due
	     to a barrier or some such, no need to do it again.  */
	  if (head != NULL_RTX)
	    {
	      create_basic_block (i++, head, end, bb_note);
	      bb_note = NULL_RTX;
	    }

	  head = end = insn;
	  break;

	case JUMP_INSN:
	  /* A basic block ends at a jump.  */
	  if (head == NULL_RTX)
	    head = insn;
	  else
	    {
	      /* ??? Make a special check for table jumps.  The way this
		 happens is truly and amazingly gross.  We are about to
		 create a basic block that contains just a code label and
		 an addr*vec jump insn.  Worse, an addr_diff_vec creates
		 its own natural loop.

		 Prevent this bit of brain damage, pasting things together
		 correctly in make_edges.

		 The correct solution involves emitting the table directly
		 on the tablejump instruction as a note, or JUMP_LABEL.  */

	      if (GET_CODE (PATTERN (insn)) == ADDR_VEC
		  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
		{
		  head = end = NULL;
		  n_basic_blocks--;
		  break;
		}
	    }
	  end = insn;
	  goto new_bb_inclusive;

	case BARRIER:
	  /* A basic block ends at a barrier.  It may be that an unconditional
	     jump already closed the basic block -- no need to do it again.  */
	  if (head == NULL_RTX)
	    break;
	  goto new_bb_exclusive;

	case CALL_INSN:
	  {
	    /* Record whether this call created an edge.  */
	    rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	    int region = (note ? INTVAL (XEXP (note, 0)) : 0);

	    if (GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	      {
		/* Scan each of the alternatives for label refs.  */
		lvl = find_label_refs (XEXP (PATTERN (insn), 0), lvl);
		lvl = find_label_refs (XEXP (PATTERN (insn), 1), lvl);
		lvl = find_label_refs (XEXP (PATTERN (insn), 2), lvl);
		/* Record its tail recursion label, if any.  */
		if (XEXP (PATTERN (insn), 3) != NULL_RTX)
		  trll = alloc_EXPR_LIST (0, XEXP (PATTERN (insn), 3), trll);
	      }

	    /* A basic block ends at a call that can either throw or
	       do a non-local goto.  */
	    if ((nonlocal_goto_handler_labels && region >= 0)
		|| can_throw_internal (insn))
	      {
	      new_bb_inclusive:
		if (head == NULL_RTX)
		  head = insn;
		end = insn;

	      new_bb_exclusive:
		create_basic_block (i++, head, end, bb_note);
		head = end = NULL_RTX;
		bb_note = NULL_RTX;
		break;
	      }
	  }
	  /* Fall through.  */

	case INSN:
	  /* Non-call exceptions generate new blocks just like calls.  */
	  if (flag_non_call_exceptions && can_throw_internal (insn))
	    goto new_bb_inclusive;

	  if (head == NULL_RTX)
	    head = insn;
	  end = insn;
	  break;

	default:
	  abort ();
	}

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN)
	{
	  rtx note;

	  /* Make a list of all labels referred to other than by jumps.

	     Make a special exception for labels followed by an ADDR*VEC,
	     as this would be a part of the tablejump setup code.

	     Make a special exception to registers loaded with label
	     values just before jump insns that use them.  */

	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_LABEL)
	      {
		rtx lab = XEXP (note, 0), next;

		if ((next = next_nonnote_insn (lab)) != NULL
			 && GET_CODE (next) == JUMP_INSN
			 && (GET_CODE (PATTERN (next)) == ADDR_VEC
			     || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
		  ;
		else if (GET_CODE (lab) == NOTE)
		  ;
		else if (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
			 && find_reg_note (NEXT_INSN (insn), REG_LABEL, lab))
		  ;
		else
		  lvl = alloc_EXPR_LIST (0, XEXP (note, 0), lvl);
	      }
	}
    }

  if (head != NULL_RTX)
    create_basic_block (i++, head, end, bb_note);
  else if (bb_note)
    flow_delete_insn (bb_note);

  if (i != n_basic_blocks)
    abort ();

  label_value_list = lvl;
  tail_recursion_label_list = trll;
}

/* Tidy the CFG by deleting unreachable code and whatnot.  */

void
cleanup_cfg (mode)
     int mode;
{
  timevar_push (TV_CLEANUP_CFG);
  delete_unreachable_blocks ();
  if (try_optimize_cfg (mode))
    delete_unreachable_blocks ();
  mark_critical_edges ();

  /* Kill the data we won't maintain.  */
  free_EXPR_LIST_list (&label_value_list);
  free_EXPR_LIST_list (&tail_recursion_label_list);
  timevar_pop (TV_CLEANUP_CFG);
}

/* Create a new basic block consisting of the instructions between
   HEAD and END inclusive.  Reuses the note and basic block struct
   in BB_NOTE, if any.  */

void
create_basic_block (index, head, end, bb_note)
     int index;
     rtx head, end, bb_note;
{
  basic_block bb;

  if (bb_note
      && ! RTX_INTEGRATED_P (bb_note)
      && (bb = NOTE_BASIC_BLOCK (bb_note)) != NULL
      && bb->aux == NULL)
    {
      /* If we found an existing note, thread it back onto the chain.  */

      rtx after;

      if (GET_CODE (head) == CODE_LABEL)
	after = head;
      else
	{
	  after = PREV_INSN (head);
	  head = bb_note;
	}

      if (after != bb_note && NEXT_INSN (after) != bb_note)
	reorder_insns (bb_note, bb_note, after);
    }
  else
    {
      /* Otherwise we must create a note and a basic block structure.
	 Since we allow basic block structs in rtl, give the struct
	 the same lifetime by allocating it off the function obstack
	 rather than using malloc.  */

      bb = (basic_block) obstack_alloc (&flow_obstack, sizeof (*bb));
      memset (bb, 0, sizeof (*bb));

      if (GET_CODE (head) == CODE_LABEL)
	bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK, head);
      else
	{
	  bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK, head);
	  head = bb_note;
	}
      NOTE_BASIC_BLOCK (bb_note) = bb;
    }

  /* Always include the bb note in the block.  */
  if (NEXT_INSN (end) == bb_note)
    end = bb_note;

  bb->head = head;
  bb->end = end;
  bb->index = index;
  BASIC_BLOCK (index) = bb;

  /* Tag the block so that we know it has been used when considering
     other basic block notes.  */
  bb->aux = bb;
}

/* Return the INSN immediately following the NOTE_INSN_BASIC_BLOCK
   note associated with the BLOCK.  */

rtx
first_insn_after_basic_block_note (block)
     basic_block block;
{
  rtx insn;

  /* Get the first instruction in the block.  */
  insn = block->head;

  if (insn == NULL_RTX)
    return NULL_RTX;
  if (GET_CODE (insn) == CODE_LABEL)
    insn = NEXT_INSN (insn);
  if (!NOTE_INSN_BASIC_BLOCK_P (insn))
    abort ();

  return NEXT_INSN (insn);
}

/* Records the basic block struct in BB_FOR_INSN, for every instruction
   indexed by INSN_UID.  MAX is the size of the array.  */

void
compute_bb_for_insn (max)
     int max;
{
  int i;

  if (basic_block_for_insn)
    VARRAY_FREE (basic_block_for_insn);
  VARRAY_BB_INIT (basic_block_for_insn, max, "basic_block_for_insn");

  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, end;

      end = bb->end;
      insn = bb->head;
      while (1)
	{
	  int uid = INSN_UID (insn);
	  if (uid < max)
	    VARRAY_BB (basic_block_for_insn, uid) = bb;
	  if (insn == end)
	    break;
	  insn = NEXT_INSN (insn);
	}
    }
}

/* Free the memory associated with the edge structures.  */

void
clear_edges ()
{
  int i;
  edge n, e;

  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);

      for (e = bb->succ; e; e = n)
	{
	  n = e->succ_next;
	  free (e);
	}

      bb->succ = 0;
      bb->pred = 0;
    }

  for (e = ENTRY_BLOCK_PTR->succ; e; e = n)
    {
      n = e->succ_next;
      free (e);
    }

  ENTRY_BLOCK_PTR->succ = 0;
  EXIT_BLOCK_PTR->pred = 0;

  n_edges = 0;
}

/* Identify the edges between basic blocks MIN to MAX.

   NONLOCAL_LABEL_LIST is a list of non-local labels in the function.  Blocks
   that are otherwise unreachable may be reachable with a non-local goto.

   BB_EH_END is an array indexed by basic block number in which we record
   the list of exception regions active at the end of the basic block.  */

static void
make_edges (label_value_list, min, max, update_p)
     rtx label_value_list;
     int min, max, update_p;
{
  int i;
  sbitmap *edge_cache = NULL;

  /* Assume no computed jump; revise as we create edges.  */
  current_function_has_computed_jump = 0;

  /* Heavy use of computed goto in machine-generated code can lead to
     nearly fully-connected CFGs.  In that case we spend a significant
     amount of time searching the edge lists for duplicates.  */
  if (forced_labels || label_value_list)
    {
      edge_cache = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
      sbitmap_vector_zero (edge_cache, n_basic_blocks);

      if (update_p)
	for (i = min; i <= max; ++i)
	  {
	    edge e;
	    for (e = BASIC_BLOCK (i)->succ; e ; e = e->succ_next)
	      if (e->dest != EXIT_BLOCK_PTR)
	        SET_BIT (edge_cache[i], e->dest->index);
	  }
    }

  /* By nature of the way these get numbered, block 0 is always the entry.  */
  make_edge (edge_cache, ENTRY_BLOCK_PTR, BASIC_BLOCK (0), EDGE_FALLTHRU);

  for (i = min; i <= max; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, x;
      enum rtx_code code;
      int force_fallthru = 0;

      if (GET_CODE (bb->head) == CODE_LABEL
	  && LABEL_ALTERNATE_NAME (bb->head))
	make_edge (NULL, ENTRY_BLOCK_PTR, bb, 0);

      /* Examine the last instruction of the block, and discover the
	 ways we can leave the block.  */

      insn = bb->end;
      code = GET_CODE (insn);

      /* A branch.  */
      if (code == JUMP_INSN)
	{
	  rtx tmp;

	  /* Recognize exception handling placeholders.  */
	  if (GET_CODE (PATTERN (insn)) == RESX)
	    make_eh_edge (edge_cache, bb, insn);

	  /* Recognize a non-local goto as a branch outside the
	     current function.  */
	  else if (find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	    ;

	  /* ??? Recognize a tablejump and do the right thing.  */
	  else if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
		   && (tmp = NEXT_INSN (tmp)) != NULL_RTX
		   && GET_CODE (tmp) == JUMP_INSN
		   && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
		       || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	    {
	      rtvec vec;
	      int j;

	      if (GET_CODE (PATTERN (tmp)) == ADDR_VEC)
		vec = XVEC (PATTERN (tmp), 0);
	      else
		vec = XVEC (PATTERN (tmp), 1);

	      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
		make_label_edge (edge_cache, bb,
				 XEXP (RTVEC_ELT (vec, j), 0), 0);

	      /* Some targets (eg, ARM) emit a conditional jump that also
		 contains the out-of-range target.  Scan for these and
		 add an edge if necessary.  */
	      if ((tmp = single_set (insn)) != NULL
		  && SET_DEST (tmp) == pc_rtx
		  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF)
		make_label_edge (edge_cache, bb,
				 XEXP (XEXP (SET_SRC (tmp), 2), 0), 0);

#ifdef CASE_DROPS_THROUGH
	      /* Silly VAXen.  The ADDR_VEC is going to be in the way of
		 us naturally detecting fallthru into the next block.  */
	      force_fallthru = 1;
#endif
	    }

	  /* If this is a computed jump, then mark it as reaching
	     everything on the label_value_list and forced_labels list.  */
	  else if (computed_jump_p (insn))
	    {
	      current_function_has_computed_jump = 1;

	      for (x = label_value_list; x; x = XEXP (x, 1))
		make_label_edge (edge_cache, bb, XEXP (x, 0), EDGE_ABNORMAL);

	      for (x = forced_labels; x; x = XEXP (x, 1))
		make_label_edge (edge_cache, bb, XEXP (x, 0), EDGE_ABNORMAL);
	    }

	  /* Returns create an exit out.  */
	  else if (returnjump_p (insn))
	    make_edge (edge_cache, bb, EXIT_BLOCK_PTR, 0);

	  /* Otherwise, we have a plain conditional or unconditional jump.  */
	  else
	    {
	      if (! JUMP_LABEL (insn))
		abort ();
	      make_label_edge (edge_cache, bb, JUMP_LABEL (insn), 0);
	    }
	}

      /* If this is a sibling call insn, then this is in effect a
	 combined call and return, and so we need an edge to the
	 exit block.  No need to worry about EH edges, since we
	 wouldn't have created the sibling call in the first place.  */

      if (code == CALL_INSN && SIBLING_CALL_P (insn))
	make_edge (edge_cache, bb, EXIT_BLOCK_PTR,
		   EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);

      /* If this is a CALL_INSN, then mark it as reaching the active EH
	 handler for this CALL_INSN.  If we're handling non-call
	 exceptions then any insn can reach any of the active handlers.

	 Also mark the CALL_INSN as reaching any nonlocal goto handler.  */

      else if (code == CALL_INSN || flag_non_call_exceptions)
	{
	  /* Add any appropriate EH edges.  */
	  make_eh_edge (edge_cache, bb, insn);

	  if (code == CALL_INSN && nonlocal_goto_handler_labels)
	    {
	      /* ??? This could be made smarter: in some cases it's possible
		 to tell that certain calls will not do a nonlocal goto.

		 For example, if the nested functions that do the nonlocal
		 gotos do not have their addresses taken, then only calls to
		 those functions or to other nested functions that use them
		 could possibly do nonlocal gotos.  */
	      /* We do know that a REG_EH_REGION note with a value less
		 than 0 is guaranteed not to perform a non-local goto.  */
	      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	      if (!note || INTVAL (XEXP (note, 0)) >=  0)
		for (x = nonlocal_goto_handler_labels; x; x = XEXP (x, 1))
		  make_label_edge (edge_cache, bb, XEXP (x, 0),
				   EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);
	    }
	}

      /* Find out if we can drop through to the next block.  */
      insn = next_nonnote_insn (insn);
      if (!insn || (i + 1 == n_basic_blocks && force_fallthru))
	make_edge (edge_cache, bb, EXIT_BLOCK_PTR, EDGE_FALLTHRU);
      else if (i + 1 < n_basic_blocks)
	{
	  rtx tmp = BLOCK_HEAD (i + 1);
	  if (GET_CODE (tmp) == NOTE)
	    tmp = next_nonnote_insn (tmp);
	  if (force_fallthru || insn == tmp)
	    make_edge (edge_cache, bb, BASIC_BLOCK (i + 1), EDGE_FALLTHRU);
	}
    }

  if (edge_cache)
    sbitmap_vector_free (edge_cache);
}

/* Create an edge between two basic blocks.  FLAGS are auxiliary information
   about the edge that is accumulated between calls.  */

void
make_edge (edge_cache, src, dst, flags)
     sbitmap *edge_cache;
     basic_block src, dst;
     int flags;
{
  int use_edge_cache;
  edge e;

  /* Don't bother with edge cache for ENTRY or EXIT; there aren't that
     many edges to them, and we didn't allocate memory for it.  */
  use_edge_cache = (edge_cache
		    && src != ENTRY_BLOCK_PTR
		    && dst != EXIT_BLOCK_PTR);

  /* Make sure we don't add duplicate edges.  */
  switch (use_edge_cache)
    {
    default:
      /* Quick test for non-existance of the edge.  */
      if (! TEST_BIT (edge_cache[src->index], dst->index))
	break;

      /* The edge exists; early exit if no work to do.  */
      if (flags == 0)
	return;

      /* FALLTHRU */
    case 0:
      for (e = src->succ; e; e = e->succ_next)
	if (e->dest == dst)
	  {
	    e->flags |= flags;
	    return;
	  }
      break;
    }

  e = (edge) xcalloc (1, sizeof (*e));
  n_edges++;

  e->succ_next = src->succ;
  e->pred_next = dst->pred;
  e->src = src;
  e->dest = dst;
  e->flags = flags;

  src->succ = e;
  dst->pred = e;

  if (use_edge_cache)
    SET_BIT (edge_cache[src->index], dst->index);
}

/* Create an edge from a basic block to a label.  */

static void
make_label_edge (edge_cache, src, label, flags)
     sbitmap *edge_cache;
     basic_block src;
     rtx label;
     int flags;
{
  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  /* If the label was never emitted, this insn is junk, but avoid a
     crash trying to refer to BLOCK_FOR_INSN (label).  This can happen
     as a result of a syntax error and a diagnostic has already been
     printed.  */

  if (INSN_UID (label) == 0)
    return;

  make_edge (edge_cache, src, BLOCK_FOR_INSN (label), flags);
}

/* Create the edges generated by INSN in REGION.  */

static void
make_eh_edge (edge_cache, src, insn)
     sbitmap *edge_cache;
     basic_block src;
     rtx insn;
{
  int is_call = (GET_CODE (insn) == CALL_INSN ? EDGE_ABNORMAL_CALL : 0);
  rtx handlers, i;

  handlers = reachable_handlers (insn);

  for (i = handlers; i; i = XEXP (i, 1))
    make_label_edge (edge_cache, src, XEXP (i, 0),
		     EDGE_ABNORMAL | EDGE_EH | is_call);

  free_INSN_LIST_list (&handlers);
}

/* Identify critical edges and set the bits appropriately.  */

void
mark_critical_edges ()
{
  int i, n = n_basic_blocks;
  basic_block bb;

  /* We begin with the entry block.  This is not terribly important now,
     but could be if a front end (Fortran) implemented alternate entry
     points.  */
  bb = ENTRY_BLOCK_PTR;
  i = -1;

  while (1)
    {
      edge e;

      /* (1) Critical edges must have a source with multiple successors.  */
      if (bb->succ && bb->succ->succ_next)
	{
	  for (e = bb->succ; e; e = e->succ_next)
	    {
	      /* (2) Critical edges must have a destination with multiple
		 predecessors.  Note that we know there is at least one
		 predecessor -- the edge we followed to get here.  */
	      if (e->dest->pred->pred_next)
		e->flags |= EDGE_CRITICAL;
	      else
		e->flags &= ~EDGE_CRITICAL;
	    }
	}
      else
	{
	  for (e = bb->succ; e; e = e->succ_next)
	    e->flags &= ~EDGE_CRITICAL;
	}

      if (++i >= n)
	break;
      bb = BASIC_BLOCK (i);
    }
}

/* Mark the back edges in DFS traversal.
   Return non-zero if a loop (natural or otherwise) is present.
   Inspired by Depth_First_Search_PP described in:

     Advanced Compiler Design and Implementation
     Steven Muchnick
     Morgan Kaufmann, 1997

   and heavily borrowed from flow_depth_first_order_compute.  */

bool
mark_dfs_back_edges ()
{
  edge *stack;
  int *pre;
  int *post;
  int sp;
  int prenum = 1;
  int postnum = 1;
  sbitmap visited;
  bool found = false;

  /* Allocate the preorder and postorder number arrays.  */
  pre = (int *) xcalloc (n_basic_blocks, sizeof (int));
  post = (int *) xcalloc (n_basic_blocks, sizeof (int));

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      edge e;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;
      e->flags &= ~EDGE_DFS_BACK;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  pre[dest->index] = prenum++;

	  if (dest->succ)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      stack[sp++] = dest->succ;
	    }
	  else
	    post[dest->index] = postnum++;
	}
      else
	{
	  if (dest != EXIT_BLOCK_PTR && src != ENTRY_BLOCK_PTR
	      && pre[src->index] >= pre[dest->index]
	      && post[dest->index] == 0)
	    e->flags |= EDGE_DFS_BACK, found = true;

	  if (! e->succ_next && src != ENTRY_BLOCK_PTR)
	    post[src->index] = postnum++;

	  if (e->succ_next)
	    stack[sp - 1] = e->succ_next;
	  else
	    sp--;
	}
    }

  free (pre);
  free (post);
  free (stack);
  sbitmap_free (visited);

  return found;
}

/* Split a block BB after insn INSN creating a new fallthru edge.
   Return the new edge.  Note that to keep other parts of the compiler happy,
   this function renumbers all the basic blocks so that the new
   one has a number one greater than the block split.  */

edge
split_block (bb, insn)
     basic_block bb;
     rtx insn;
{
  basic_block new_bb;
  edge new_edge;
  edge e;
  rtx bb_note;
  int i, j;

  /* There is no point splitting the block after its end.  */
  if (bb->end == insn)
    return 0;

  /* Create the new structures.  */
  new_bb = (basic_block) obstack_alloc (&flow_obstack, sizeof (*new_bb));
  new_edge = (edge) xcalloc (1, sizeof (*new_edge));
  n_edges++;

  memset (new_bb, 0, sizeof (*new_bb));

  new_bb->head = NEXT_INSN (insn);
  new_bb->end = bb->end;
  bb->end = insn;

  new_bb->succ = bb->succ;
  bb->succ = new_edge;
  new_bb->pred = new_edge;
  new_bb->count = bb->count;
  new_bb->frequency = bb->frequency;
  new_bb->loop_depth = bb->loop_depth;

  new_edge->src = bb;
  new_edge->dest = new_bb;
  new_edge->flags = EDGE_FALLTHRU;
  new_edge->probability = REG_BR_PROB_BASE;
  new_edge->count = bb->count;

  /* Redirect the src of the successor edges of bb to point to new_bb.  */
  for (e = new_bb->succ; e; e = e->succ_next)
    e->src = new_bb;

  /* Place the new block just after the block being split.  */
  VARRAY_GROW (basic_block_info, ++n_basic_blocks);

  /* Some parts of the compiler expect blocks to be number in
     sequential order so insert the new block immediately after the
     block being split..  */
  j = bb->index;
  for (i = n_basic_blocks - 1; i > j + 1; --i)
    {
      basic_block tmp = BASIC_BLOCK (i - 1);
      BASIC_BLOCK (i) = tmp;
      tmp->index = i;
    }

  BASIC_BLOCK (i) = new_bb;
  new_bb->index = i;

  if (GET_CODE (new_bb->head) == CODE_LABEL)
    {
      /* Create the basic block note.  */
      bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK,
				 new_bb->head);
      NOTE_BASIC_BLOCK (bb_note) = new_bb;

      /* If the only thing in this new block was the label, make sure
	 the block note gets included.  */
      if (new_bb->head == new_bb->end)
	new_bb->end = bb_note;
    }
  else
    {
      /* Create the basic block note.  */
      bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK,
				  new_bb->head);
      NOTE_BASIC_BLOCK (bb_note) = new_bb;
      new_bb->head = bb_note;
    }

  update_bb_for_insn (new_bb);

  if (bb->global_live_at_start)
    {
      new_bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      new_bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (new_bb->global_live_at_end, bb->global_live_at_end);

      /* We now have to calculate which registers are live at the end
	 of the split basic block and at the start of the new basic
	 block.  Start with those registers that are known to be live
	 at the end of the original basic block and get
	 propagate_block to determine which registers are live.  */
      COPY_REG_SET (new_bb->global_live_at_start, bb->global_live_at_end);
      propagate_block (new_bb, new_bb->global_live_at_start, NULL, NULL, 0);
      COPY_REG_SET (bb->global_live_at_end,
		    new_bb->global_live_at_start);
    }

  return new_edge;
}

/* Return label in the head of basic block.  Create one if it doesn't exist.  */
rtx
block_label (block)
     basic_block block;
{
  if (block == EXIT_BLOCK_PTR)
    return NULL_RTX;
  if (GET_CODE (block->head) != CODE_LABEL)
    {
      block->head = emit_label_before (gen_label_rtx (), block->head);
      if (basic_block_for_insn)
	set_block_for_insn (block->head, block);
    }
  return block->head;
}

/* Return true if the block has no effect and only forwards control flow to
   its single destination.  */
bool
forwarder_block_p (bb)
     basic_block bb;
{
  rtx insn = bb->head;
  if (bb == EXIT_BLOCK_PTR || bb == ENTRY_BLOCK_PTR
      || !bb->succ || bb->succ->succ_next)
    return false;

  while (insn != bb->end)
    {
      if (active_insn_p (insn))
	return false;
      insn = NEXT_INSN (insn);
    }
  return (!active_insn_p (insn)
	  || (GET_CODE (insn) == JUMP_INSN && onlyjump_p (insn)));
}

/* Return nonzero if we can reach target from src by falling trought.  */
static bool
can_fallthru (src, target)
     basic_block src, target;
{
  rtx insn = src->end;
  rtx insn2 = target->head;

  if (src->index + 1 == target->index && !active_insn_p (insn2))
    insn2 = next_active_insn (insn2);
  /* ??? Later we may add code to move jump tables offline.  */
  return next_active_insn (insn) == insn2;
}

/* Attempt to perform edge redirection by replacing possibly complex jump
   instruction by unconditional jump or removing jump completely.
   This can apply only if all edges now point to the same block.

   The parameters and return values are equivalent to redirect_edge_and_branch.
 */
static bool
try_redirect_by_replacing_jump (e, target)
     edge e;
     basic_block target;
{
  basic_block src = e->src;
  rtx insn = src->end, kill_from;
  edge tmp;
  rtx set;
  int fallthru = 0;

  /* Verify that all targets will be TARGET.  */
  for (tmp = src->succ; tmp; tmp = tmp->succ_next)
    if (tmp->dest != target && tmp != e)
      break;
  if (tmp || !onlyjump_p (insn))
    return false;

  /* Avoid removing branch with side effects.  */
  set = single_set (insn);
  if (!set || side_effects_p (set))
    return false;

  /* In case we zap a conditional jump, we'll need to kill
     the cc0 setter too.  */
  kill_from = insn;
#ifdef HAVE_cc0
  if (reg_mentioned_p (cc0_rtx, PATTERN (insn)))
    kill_from = PREV_INSN (insn);
#endif

  /* See if we can create the fallthru edge.  */
  if (can_fallthru (src, target))
    {
      src->end = PREV_INSN (kill_from);
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Removing jump %i.\n", INSN_UID (insn));
      fallthru = 1;

      /* Selectivly unlink whole insn chain.  */
      flow_delete_insn_chain (kill_from, PREV_INSN (target->head));
    }
  /* If this already is simplejump, redirect it.  */
  else if (simplejump_p (insn))
    {
      if (e->dest == target)
	return false;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Redirecting jump %i from %i to %i.\n",
		 INSN_UID (insn), e->dest->index, target->index);
      redirect_jump (insn, block_label (target), 0);
    }
  /* Or replace possibly complicated jump insn by simple jump insn.  */
  else
    {
      rtx target_label = block_label (target);
      rtx barrier;

      src->end = emit_jump_insn_before (gen_jump (target_label), kill_from);
      JUMP_LABEL (src->end) = target_label;
      LABEL_NUSES (target_label)++;
      if (basic_block_for_insn)
	set_block_for_new_insns (src->end, src);
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Replacing insn %i by jump %i\n",
		 INSN_UID (insn), INSN_UID (src->end));

      flow_delete_insn_chain (kill_from, insn);

      barrier = next_nonnote_insn (src->end);
      if (!barrier || GET_CODE (barrier) != BARRIER)
	emit_barrier_after (src->end);
    }

  /* Keep only one edge out and set proper flags.  */
  while (src->succ->succ_next)
    remove_edge (src->succ);
  e = src->succ;
  if (fallthru)
    e->flags = EDGE_FALLTHRU;
  else
    e->flags = 0;
  e->probability = REG_BR_PROB_BASE;
  e->count = src->count;

  /* We don't want a block to end on a line-number note since that has
     the potential of changing the code between -g and not -g.  */
  while (GET_CODE (e->src->end) == NOTE
	 && NOTE_LINE_NUMBER (e->src->end) >= 0)
    {
      rtx prev = PREV_INSN (e->src->end);
      flow_delete_insn (e->src->end);
      e->src->end = prev;
    }

  if (e->dest != target)
    redirect_edge_succ (e, target);
  return true;
}

/* Return last loop_beg note appearing after INSN, before start of next
   basic block.  Return INSN if there are no such notes.

   When emmiting jump to redirect an fallthru edge, it should always
   appear after the LOOP_BEG notes, as loop optimizer expect loop to
   eighter start by fallthru edge or jump following the LOOP_BEG note
   jumping to the loop exit test.  */
rtx
last_loop_beg_note (insn)
     rtx insn;
{
  rtx last = insn;
  insn = NEXT_INSN (insn);
  while (GET_CODE (insn) == NOTE
	 && NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK)
    {
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	last = insn;
      insn = NEXT_INSN (insn);
    }
  return last;
}

/* Attempt to change code to redirect edge E to TARGET.
   Don't do that on expense of adding new instructions or reordering
   basic blocks.

   Function can be also called with edge destionation equivalent to the
   TARGET.  Then it should try the simplifications and do nothing if
   none is possible.

   Return true if transformation suceeded.  We still return flase in case
   E already destinated TARGET and we didn't managed to simplify instruction
   stream.  */
bool
redirect_edge_and_branch (e, target)
     edge e;
     basic_block target;
{
  rtx tmp;
  rtx old_label = e->dest->head;
  basic_block src = e->src;
  rtx insn = src->end;

  if (e->flags & EDGE_COMPLEX)
    return false;

  if (try_redirect_by_replacing_jump (e, target))
    return true;
  /* Do this fast path late, as we want above code to simplify for cases
     where called on single edge leaving basic block containing nontrivial
     jump insn.  */
  else if (e->dest == target)
    return false;

  /* We can only redirect non-fallthru edges of jump insn.  */
  if (e->flags & EDGE_FALLTHRU)
    return false;
  if (GET_CODE (insn) != JUMP_INSN)
    return false;

  /* Recognize a tablejump and adjust all matching cases.  */
  if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
      && (tmp = NEXT_INSN (tmp)) != NULL_RTX
      && GET_CODE (tmp) == JUMP_INSN
      && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	  || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
    {
      rtvec vec;
      int j;
      rtx new_label = block_label (target);

      if (GET_CODE (PATTERN (tmp)) == ADDR_VEC)
	vec = XVEC (PATTERN (tmp), 0);
      else
	vec = XVEC (PATTERN (tmp), 1);

      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
	if (XEXP (RTVEC_ELT (vec, j), 0) == old_label)
	  {
	    RTVEC_ELT (vec, j) = gen_rtx_LABEL_REF (Pmode, new_label);
	    --LABEL_NUSES (old_label);
	    ++LABEL_NUSES (new_label);
	  }

      /* Handle casesi dispatch insns */
      if ((tmp = single_set (insn)) != NULL
	  && SET_DEST (tmp) == pc_rtx
	  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
	  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF
	  && XEXP (XEXP (SET_SRC (tmp), 2), 0) == old_label)
	{
	  XEXP (SET_SRC (tmp), 2) = gen_rtx_LABEL_REF (VOIDmode,
						       new_label);
	  --LABEL_NUSES (old_label);
	  ++LABEL_NUSES (new_label);
	}
    }
  else
    {
      /* ?? We may play the games with moving the named labels from
	 one basic block to the other in case only one computed_jump is
	 available.  */
      if (computed_jump_p (insn))
	return false;

      /* A return instruction can't be redirected.  */
      if (returnjump_p (insn))
	return false;

      /* If the insn doesn't go where we think, we're confused.  */
      if (JUMP_LABEL (insn) != old_label)
	abort ();
      redirect_jump (insn, block_label (target), 0);
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Edge %i->%i redirected to %i\n",
	     e->src->index, e->dest->index, target->index);
  if (e->dest != target)
    redirect_edge_succ_nodup (e, target);
  return true;
}

/* Redirect edge even at the expense of creating new jump insn or
   basic block.  Return new basic block if created, NULL otherwise.
   Abort if converison is impossible.  */
basic_block
redirect_edge_and_branch_force (e, target)
     edge e;
     basic_block target;
{
  basic_block new_bb;
  edge new_edge;
  rtx label;
  rtx bb_note;
  int i, j;

  if (redirect_edge_and_branch (e, target))
    return NULL;
  if (e->dest == target)
    return NULL;
  if (e->flags & EDGE_ABNORMAL)
    abort ();
  if (!(e->flags & EDGE_FALLTHRU))
    abort ();

  e->flags &= ~EDGE_FALLTHRU;
  label = block_label (target);
  /* Case of the fallthru block.  */
  if (!e->src->succ->succ_next)
    {
      e->src->end = emit_jump_insn_after (gen_jump (label),
					  last_loop_beg_note (e->src->end));
      JUMP_LABEL (e->src->end) = label;
      LABEL_NUSES (label)++;
      if (basic_block_for_insn)
	set_block_for_new_insns (e->src->end, e->src);
      emit_barrier_after (e->src->end);
      if (rtl_dump_file)
	fprintf (rtl_dump_file,
		 "Emitting jump insn %i to redirect edge %i->%i to %i\n",
		 INSN_UID (e->src->end), e->src->index, e->dest->index,
		 target->index);
      redirect_edge_succ (e, target);
      return NULL;
    }
  /* Redirecting fallthru edge of the conditional needs extra work.  */

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "Emitting jump insn %i in new BB to redirect edge %i->%i to %i\n",
	     INSN_UID (e->src->end), e->src->index, e->dest->index,
	     target->index);

  /* Create the new structures.  */
  new_bb = (basic_block) obstack_alloc (&flow_obstack, sizeof (*new_bb));
  new_edge = (edge) xcalloc (1, sizeof (*new_edge));
  n_edges++;

  memset (new_bb, 0, sizeof (*new_bb));

  new_bb->end = new_bb->head = last_loop_beg_note (e->src->end);
  new_bb->succ = NULL;
  new_bb->pred = new_edge;
  new_bb->count = e->count;
  new_bb->frequency = EDGE_FREQUENCY (e);
  new_bb->loop_depth = e->dest->loop_depth;

  new_edge->flags = EDGE_FALLTHRU;
  new_edge->probability = e->probability;
  new_edge->count = e->count;

  if (target->global_live_at_start)
    {
      new_bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      new_bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (new_bb->global_live_at_start,
		    target->global_live_at_start);
      COPY_REG_SET (new_bb->global_live_at_end, new_bb->global_live_at_start);
    }

  /* Wire edge in.  */
  new_edge->src = e->src;
  new_edge->dest = new_bb;
  new_edge->succ_next = e->src->succ;
  e->src->succ = new_edge;
  new_edge->pred_next = NULL;

  /* Redirect old edge.  */
  redirect_edge_succ (e, target);
  redirect_edge_pred (e, new_bb);
  e->probability = REG_BR_PROB_BASE;

  /* Place the new block just after the block being split.  */
  VARRAY_GROW (basic_block_info, ++n_basic_blocks);

  /* Some parts of the compiler expect blocks to be number in
     sequential order so insert the new block immediately after the
     block being split..  */
  j = new_edge->src->index;
  for (i = n_basic_blocks - 1; i > j + 1; --i)
    {
      basic_block tmp = BASIC_BLOCK (i - 1);
      BASIC_BLOCK (i) = tmp;
      tmp->index = i;
    }

  BASIC_BLOCK (i) = new_bb;
  new_bb->index = i;

  /* Create the basic block note.  */
  bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK, new_bb->head);
  NOTE_BASIC_BLOCK (bb_note) = new_bb;
  new_bb->head = bb_note;

  new_bb->end = emit_jump_insn_after (gen_jump (label), new_bb->head);
  JUMP_LABEL (new_bb->end) = label;
  LABEL_NUSES (label)++;
  if (basic_block_for_insn)
    set_block_for_new_insns (new_bb->end, new_bb);
  emit_barrier_after (new_bb->end);
  return new_bb;
}

/* Helper function for split_edge.  Return true in case edge BB2 to BB1
   is back edge of syntactic loop.  */
static bool
back_edge_of_syntactic_loop_p (bb1, bb2)
	basic_block bb1, bb2;
{
  rtx insn;
  int count = 0;

  if (bb1->index > bb2->index)
    return false;

  if (bb1->index == bb2->index)
    return true;

  for (insn = bb1->end; insn != bb2->head && count >= 0;
       insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	  count++;
	if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	  count--;
      }

  return count >= 0;
}

/* Split a (typically critical) edge.  Return the new block.
   Abort on abnormal edges.

   ??? The code generally expects to be called on critical edges.
   The case of a block ending in an unconditional jump to a
   block with multiple predecessors is not handled optimally.  */

basic_block
split_edge (edge_in)
     edge edge_in;
{
  basic_block old_pred, bb, old_succ;
  edge edge_out;
  rtx bb_note;
  int i, j;

  /* Abnormal edges cannot be split.  */
  if ((edge_in->flags & EDGE_ABNORMAL) != 0)
    abort ();

  old_pred = edge_in->src;
  old_succ = edge_in->dest;

  /* Create the new structures.  */
  bb = (basic_block) obstack_alloc (&flow_obstack, sizeof (*bb));
  edge_out = (edge) xcalloc (1, sizeof (*edge_out));
  n_edges++;

  memset (bb, 0, sizeof (*bb));

  /* ??? This info is likely going to be out of date very soon.  */
  if (old_succ->global_live_at_start)
    {
      bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      COPY_REG_SET (bb->global_live_at_start, old_succ->global_live_at_start);
      COPY_REG_SET (bb->global_live_at_end, old_succ->global_live_at_start);
    }

  /* Wire them up.  */
  bb->succ = edge_out;
  bb->count = edge_in->count;
  bb->frequency = EDGE_FREQUENCY (edge_in);

  edge_in->flags &= ~EDGE_CRITICAL;

  edge_out->pred_next = old_succ->pred;
  edge_out->succ_next = NULL;
  edge_out->src = bb;
  edge_out->dest = old_succ;
  edge_out->flags = EDGE_FALLTHRU;
  edge_out->probability = REG_BR_PROB_BASE;
  edge_out->count = edge_in->count;

  old_succ->pred = edge_out;

  /* Tricky case -- if there existed a fallthru into the successor
     (and we're not it) we must add a new unconditional jump around
     the new block we're actually interested in.

     Further, if that edge is critical, this means a second new basic
     block must be created to hold it.  In order to simplify correct
     insn placement, do this before we touch the existing basic block
     ordering for the block we were really wanting.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      edge e;
      for (e = edge_out->pred_next; e; e = e->pred_next)
	if (e->flags & EDGE_FALLTHRU)
	  break;

      if (e)
	{
	  basic_block jump_block;
	  rtx pos;

	  if ((e->flags & EDGE_CRITICAL) == 0
	      && e->src != ENTRY_BLOCK_PTR)
	    {
	      /* Non critical -- we can simply add a jump to the end
		 of the existing predecessor.  */
	      jump_block = e->src;
	    }
	  else
	    {
	      /* We need a new block to hold the jump.  The simplest
	         way to do the bulk of the work here is to recursively
	         call ourselves.  */
	      jump_block = split_edge (e);
	      e = jump_block->succ;
	    }

	  /* Now add the jump insn ...  */
	  pos = emit_jump_insn_after (gen_jump (old_succ->head),
				      last_loop_beg_note (jump_block->end));
	  jump_block->end = pos;
	  if (basic_block_for_insn)
	    set_block_for_new_insns (pos, jump_block);
	  emit_barrier_after (pos);

	  /* ... let jump know that label is in use, ...  */
	  JUMP_LABEL (pos) = old_succ->head;
	  ++LABEL_NUSES (old_succ->head);

	  /* ... and clear fallthru on the outgoing edge.  */
	  e->flags &= ~EDGE_FALLTHRU;

	  /* Continue splitting the interesting edge.  */
	}
    }

  /* Place the new block just in front of the successor.  */
  VARRAY_GROW (basic_block_info, ++n_basic_blocks);
  if (old_succ == EXIT_BLOCK_PTR)
    j = n_basic_blocks - 1;
  else
    j = old_succ->index;
  for (i = n_basic_blocks - 1; i > j; --i)
    {
      basic_block tmp = BASIC_BLOCK (i - 1);
      BASIC_BLOCK (i) = tmp;
      tmp->index = i;
    }
  BASIC_BLOCK (i) = bb;
  bb->index = i;

  /* Create the basic block note.

     Where we place the note can have a noticable impact on the generated
     code.  Consider this cfg:

		        E
			|
			0
		       / \
		   +->1-->2--->E
                   |  |
		   +--+

      If we need to insert an insn on the edge from block 0 to block 1,
      we want to ensure the instructions we insert are outside of any
      loop notes that physically sit between block 0 and block 1.  Otherwise
      we confuse the loop optimizer into thinking the loop is a phony.  */
  if (old_succ != EXIT_BLOCK_PTR
      && PREV_INSN (old_succ->head)
      && GET_CODE (PREV_INSN (old_succ->head)) == NOTE
      && NOTE_LINE_NUMBER (PREV_INSN (old_succ->head)) == NOTE_INSN_LOOP_BEG
      && !back_edge_of_syntactic_loop_p (old_succ, old_pred))
    bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK,
				PREV_INSN (old_succ->head));
  else if (old_succ != EXIT_BLOCK_PTR)
    bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK, old_succ->head);
  else
    bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK, get_last_insn ());
  NOTE_BASIC_BLOCK (bb_note) = bb;
  bb->head = bb->end = bb_note;

  /* For non-fallthry edges, we must adjust the predecessor's
     jump instruction to target our new block.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      if (!redirect_edge_and_branch (edge_in, bb))
	abort ();
    }
  else
    redirect_edge_succ (edge_in, bb);

  return bb;
}

/* Queue instructions for insertion on an edge between two basic blocks.
   The new instructions and basic blocks (if any) will not appear in the
   CFG until commit_edge_insertions is called.  */

void
insert_insn_on_edge (pattern, e)
     rtx pattern;
     edge e;
{
  /* We cannot insert instructions on an abnormal critical edge.
     It will be easier to find the culprit if we die now.  */
  if ((e->flags & (EDGE_ABNORMAL|EDGE_CRITICAL))
      == (EDGE_ABNORMAL|EDGE_CRITICAL))
    abort ();

  if (e->insns == NULL_RTX)
    start_sequence ();
  else
    push_to_sequence (e->insns);

  emit_insn (pattern);

  e->insns = get_insns ();
  end_sequence ();
}

/* Update the CFG for the instructions queued on edge E.  */

static void
commit_one_edge_insertion (e)
     edge e;
{
  rtx before = NULL_RTX, after = NULL_RTX, insns, tmp, last;
  basic_block bb;

  /* Pull the insns off the edge now since the edge might go away.  */
  insns = e->insns;
  e->insns = NULL_RTX;

  /* Figure out where to put these things.  If the destination has
     one predecessor, insert there.  Except for the exit block.  */
  if (e->dest->pred->pred_next == NULL
      && e->dest != EXIT_BLOCK_PTR)
    {
      bb = e->dest;

      /* Get the location correct wrt a code label, and "nice" wrt
	 a basic block note, and before everything else.  */
      tmp = bb->head;
      if (GET_CODE (tmp) == CODE_LABEL)
	tmp = NEXT_INSN (tmp);
      if (NOTE_INSN_BASIC_BLOCK_P (tmp))
	tmp = NEXT_INSN (tmp);
      if (tmp == bb->head)
	before = tmp;
      else
	after = PREV_INSN (tmp);
    }

  /* If the source has one successor and the edge is not abnormal,
     insert there.  Except for the entry block.  */
  else if ((e->flags & EDGE_ABNORMAL) == 0
	   && e->src->succ->succ_next == NULL
	   && e->src != ENTRY_BLOCK_PTR)
    {
      bb = e->src;
      /* It is possible to have a non-simple jump here.  Consider a target
	 where some forms of unconditional jumps clobber a register.  This
	 happens on the fr30 for example.

	 We know this block has a single successor, so we can just emit
	 the queued insns before the jump.  */
      if (GET_CODE (bb->end) == JUMP_INSN)
	{
	  before = bb->end;
	}
      else
	{
	  /* We'd better be fallthru, or we've lost track of what's what.  */
	  if ((e->flags & EDGE_FALLTHRU) == 0)
	    abort ();

	  after = bb->end;
	}
    }

  /* Otherwise we must split the edge.  */
  else
    {
      bb = split_edge (e);
      after = bb->end;
    }

  /* Now that we've found the spot, do the insertion.  */

  /* Set the new block number for these insns, if structure is allocated.  */
  if (basic_block_for_insn)
    {
      rtx i;
      for (i = insns; i != NULL_RTX; i = NEXT_INSN (i))
	set_block_for_insn (i, bb);
    }

  if (before)
    {
      emit_insns_before (insns, before);
      if (before == bb->head)
	bb->head = insns;

      last = prev_nonnote_insn (before);
    }
  else
    {
      last = emit_insns_after (insns, after);
      if (after == bb->end)
	bb->end = last;
    }

  if (returnjump_p (last))
    {
      /* ??? Remove all outgoing edges from BB and add one for EXIT.
         This is not currently a problem because this only happens
	 for the (single) epilogue, which already has a fallthru edge
	 to EXIT.  */

      e = bb->succ;
      if (e->dest != EXIT_BLOCK_PTR
	  || e->succ_next != NULL
	  || (e->flags & EDGE_FALLTHRU) == 0)
	abort ();
      e->flags &= ~EDGE_FALLTHRU;

      emit_barrier_after (last);
      bb->end = last;

      if (before)
	flow_delete_insn (before);
    }
  else if (GET_CODE (last) == JUMP_INSN)
    abort ();
  find_sub_basic_blocks (bb);
}

/* Update the CFG for all queued instructions.  */

void
commit_edge_insertions ()
{
  int i;
  basic_block bb;
  compute_bb_for_insn (get_max_uid ());

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  i = -1;
  bb = ENTRY_BLOCK_PTR;
  while (1)
    {
      edge e, next;

      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;
	  if (e->insns)
	    commit_one_edge_insertion (e);
	}

      if (++i >= n_basic_blocks)
	break;
      bb = BASIC_BLOCK (i);
    }
}

/* Return true if we need to add fake edge to exit.
   Helper function for the flow_call_edges_add.  */
static bool
need_fake_edge_p (insn)
     rtx insn;
{
  if (!INSN_P (insn))
    return false;

  if ((GET_CODE (insn) == CALL_INSN
       && !SIBLING_CALL_P (insn)
       && !find_reg_note (insn, REG_NORETURN, NULL)
       && !find_reg_note (insn, REG_ALWAYS_RETURN, NULL)
       && !CONST_OR_PURE_CALL_P (insn)))
    return true;

  return ((GET_CODE (PATTERN (insn)) == ASM_OPERANDS
	   && MEM_VOLATILE_P (PATTERN (insn)))
	  || (GET_CODE (PATTERN (insn)) == PARALLEL
	      && asm_noperands (insn) != -1
	      && MEM_VOLATILE_P (XVECEXP (PATTERN (insn), 0, 0)))
	  || GET_CODE (PATTERN (insn)) == ASM_INPUT);
}

/* Add fake edges to the function exit for any non constant and non noreturn
   calls, volatile inline assembly in the bitmap of blocks specified by
   BLOCKS or to the whole CFG if BLOCKS is zero.  Return the nuber of blocks
   that were split.

   The goal is to expose cases in which entering a basic block does not imply
   that all subsequent instructions must be executed.  */

int
flow_call_edges_add (blocks)
     sbitmap blocks;
{
  int i;
  int blocks_split = 0;
  int bb_num = 0;
  basic_block *bbs;
  bool check_last_block = false;

  /* Map bb indicies into basic block pointers since split_block
     will renumber the basic blocks.  */

  bbs = xmalloc (n_basic_blocks * sizeof (*bbs));

  if (! blocks)
    {
      for (i = 0; i < n_basic_blocks; i++)
	bbs[bb_num++] = BASIC_BLOCK (i);
      check_last_block = true;
    }
  else
    {
      EXECUTE_IF_SET_IN_SBITMAP (blocks, 0, i,
      {
	bbs[bb_num++] = BASIC_BLOCK (i);
	if (i == n_basic_blocks - 1)
	  check_last_block = true;
      });
    }

  /* In the last basic block, before epilogue generation, there will be
     a fallthru edge to EXIT.  Special care is required if the last insn
     of the last basic block is a call because make_edge folds duplicate
     edges, which would result in the fallthru edge also being marked
     fake, which would result in the fallthru edge being removed by
     remove_fake_edges, which would result in an invalid CFG.

     Moreover, we can't elide the outgoing fake edge, since the block
     profiler needs to take this into account in order to solve the minimal
     spanning tree in the case that the call doesn't return.

     Handle this by adding a dummy instruction in a new last basic block.  */
  if (check_last_block
      && need_fake_edge_p (BASIC_BLOCK (n_basic_blocks - 1)->end))
    {
       edge e;
       for (e = BASIC_BLOCK (n_basic_blocks - 1)->succ; e; e = e->succ_next)
	 if (e->dest == EXIT_BLOCK_PTR)
	    break;
       insert_insn_on_edge (gen_rtx_USE (VOIDmode, const0_rtx), e);
       commit_edge_insertions ();
    }


  /* Now add fake edges to the function exit for any non constant
     calls since there is no way that we can determine if they will
     return or not...  */

  for (i = 0; i < bb_num; i++)
    {
      basic_block bb = bbs[i];
      rtx insn;
      rtx prev_insn;

      for (insn = bb->end; ; insn = prev_insn)
	{
	  prev_insn = PREV_INSN (insn);
	  if (need_fake_edge_p (insn))
	    {
	      edge e;

	      /* The above condition should be enought to verify that there is
		 no edge to the exit block in CFG already.  Calling make_edge in
		 such case would make us to mark that edge as fake and remove it
		 later.  */
#ifdef ENABLE_CHECKING
	      if (insn == bb->end)
		for (e = bb->succ; e; e = e->succ_next)
		  if (e->dest == EXIT_BLOCK_PTR)
		    abort ();
#endif

	      /* Note that the following may create a new basic block
		 and renumber the existing basic blocks.  */
	      e = split_block (bb, insn);
	      if (e)
		blocks_split++;

	      make_edge (NULL, bb, EXIT_BLOCK_PTR, EDGE_FAKE);
	    }
	  if (insn == bb->head)
	    break;
	}
    }

  if (blocks_split)
    verify_flow_info ();

  free (bbs);
  return blocks_split;
}

/* Find unreachable blocks.  An unreachable block will have NULL in
   block->aux, a non-NULL value indicates the block is reachable.  */

void
find_unreachable_blocks ()
{
  edge e;
  int i, n;
  basic_block *tos, *worklist;

  n = n_basic_blocks;
  tos = worklist = (basic_block *) xmalloc (sizeof (basic_block) * n);

  /* Use basic_block->aux as a marker.  Clear them all.  */

  for (i = 0; i < n; ++i)
    BASIC_BLOCK (i)->aux = NULL;

  /* Add our starting points to the worklist.  Almost always there will
     be only one.  It isn't inconcievable that we might one day directly
     support Fortran alternate entry points.  */

  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      *tos++ = e->dest;

      /* Mark the block with a handy non-null value.  */
      e->dest->aux = e;
    }

  /* Iterate: find everything reachable from what we've already seen.  */

  while (tos != worklist)
    {
      basic_block b = *--tos;

      for (e = b->succ; e; e = e->succ_next)
	if (!e->dest->aux)
	  {
	    *tos++ = e->dest;
	    e->dest->aux = e;
	  }
    }

  free (worklist);
}

/* Delete all unreachable basic blocks.   */
static void
delete_unreachable_blocks ()
{
  int i;

  find_unreachable_blocks ();

  /* Delete all unreachable basic blocks.  Count down so that we
     don't interfere with the block renumbering that happens in
     flow_delete_block.  */

  for (i = n_basic_blocks - 1; i >= 0; --i)
    {
      basic_block b = BASIC_BLOCK (i);

      if (b->aux != NULL)
	/* This block was found.  Tidy up the mark.  */
	b->aux = NULL;
      else
	flow_delete_block (b);
    }

  tidy_fallthru_edges ();
}

/* Return true if NOTE is not one of the ones that must be kept paired,
   so that we may simply delete them.  */

static int
can_delete_note_p (note)
     rtx note;
{
  return (NOTE_LINE_NUMBER (note) == NOTE_INSN_DELETED
	  || NOTE_LINE_NUMBER (note) == NOTE_INSN_BASIC_BLOCK);
}

/* Unlink a chain of insns between START and FINISH, leaving notes
   that must be paired.  */

void
flow_delete_insn_chain (start, finish)
     rtx start, finish;
{
  /* Unchain the insns one by one.  It would be quicker to delete all
     of these with a single unchaining, rather than one at a time, but
     we need to keep the NOTE's.  */

  rtx next;

  while (1)
    {
      next = NEXT_INSN (start);
      if (GET_CODE (start) == NOTE && !can_delete_note_p (start))
	;
      else if (GET_CODE (start) == CODE_LABEL
	       && ! can_delete_label_p (start))
	{
	  const char *name = LABEL_NAME (start);
	  PUT_CODE (start, NOTE);
	  NOTE_LINE_NUMBER (start) = NOTE_INSN_DELETED_LABEL;
	  NOTE_SOURCE_FILE (start) = name;
	}
      else
	next = flow_delete_insn (start);

      if (start == finish)
	break;
      start = next;
    }
}

/* Delete the insns in a (non-live) block.  We physically delete every
   non-deleted-note insn, and update the flow graph appropriately.

   Return nonzero if we deleted an exception handler.  */

/* ??? Preserving all such notes strikes me as wrong.  It would be nice
   to post-process the stream to remove empty blocks, loops, ranges, etc.  */

int
flow_delete_block (b)
     basic_block b;
{
  int deleted_handler = 0;
  rtx insn, end, tmp;

  /* If the head of this block is a CODE_LABEL, then it might be the
     label for an exception handler which can't be reached.

     We need to remove the label from the exception_handler_label list
     and remove the associated NOTE_INSN_EH_REGION_BEG and
     NOTE_INSN_EH_REGION_END notes.  */

  insn = b->head;

  never_reached_warning (insn);

  if (GET_CODE (insn) == CODE_LABEL)
    maybe_remove_eh_handler (insn);

  /* Include any jump table following the basic block.  */
  end = b->end;
  if (GET_CODE (end) == JUMP_INSN
      && (tmp = JUMP_LABEL (end)) != NULL_RTX
      && (tmp = NEXT_INSN (tmp)) != NULL_RTX
      && GET_CODE (tmp) == JUMP_INSN
      && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	  || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
    end = tmp;

  /* Include any barrier that may follow the basic block.  */
  tmp = next_nonnote_insn (end);
  if (tmp && GET_CODE (tmp) == BARRIER)
    end = tmp;

  /* Selectively delete the entire chain.  */
  flow_delete_insn_chain (insn, end);

  /* Remove the edges into and out of this block.  Note that there may
     indeed be edges in, if we are removing an unreachable loop.  */
  {
    edge e, next, *q;

    for (e = b->pred; e; e = next)
      {
	for (q = &e->src->succ; *q != e; q = &(*q)->succ_next)
	  continue;
	*q = e->succ_next;
	next = e->pred_next;
	n_edges--;
	free (e);
      }
    for (e = b->succ; e; e = next)
      {
	for (q = &e->dest->pred; *q != e; q = &(*q)->pred_next)
	  continue;
	*q = e->pred_next;
	next = e->succ_next;
	n_edges--;
	free (e);
      }

    b->pred = NULL;
    b->succ = NULL;
  }

  /* Remove the basic block from the array, and compact behind it.  */
  expunge_block (b);

  return deleted_handler;
}

/* Remove block B from the basic block array and compact behind it.  */

static void
expunge_block (b)
     basic_block b;
{
  int i, n = n_basic_blocks;

  for (i = b->index; i + 1 < n; ++i)
    {
      basic_block x = BASIC_BLOCK (i + 1);
      BASIC_BLOCK (i) = x;
      x->index = i;
    }

  basic_block_info->num_elements--;
  n_basic_blocks--;
}

/* Delete INSN by patching it out.  Return the next insn.  */

rtx
flow_delete_insn (insn)
     rtx insn;
{
  rtx prev = PREV_INSN (insn);
  rtx next = NEXT_INSN (insn);
  rtx note;

  PREV_INSN (insn) = NULL_RTX;
  NEXT_INSN (insn) = NULL_RTX;
  INSN_DELETED_P (insn) = 1;

  if (prev)
    NEXT_INSN (prev) = next;
  if (next)
    PREV_INSN (next) = prev;
  else
    set_last_insn (prev);

  if (GET_CODE (insn) == CODE_LABEL)
    remove_node_from_expr_list (insn, &nonlocal_goto_handler_labels);

  /* If deleting a jump, decrement the use count of the label.  Deleting
     the label itself should happen in the normal course of block merging.  */
  if (GET_CODE (insn) == JUMP_INSN
      && JUMP_LABEL (insn)
      && GET_CODE (JUMP_LABEL (insn)) == CODE_LABEL)
    LABEL_NUSES (JUMP_LABEL (insn))--;

  /* Also if deleting an insn that references a label.  */
  else if ((note = find_reg_note (insn, REG_LABEL, NULL_RTX)) != NULL_RTX
	   && GET_CODE (XEXP (note, 0)) == CODE_LABEL)
    LABEL_NUSES (XEXP (note, 0))--;

  if (GET_CODE (insn) == JUMP_INSN
      && (GET_CODE (PATTERN (insn)) == ADDR_VEC
	  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC))
    {
      rtx pat = PATTERN (insn);
      int diff_vec_p = GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC;
      int len = XVECLEN (pat, diff_vec_p);
      int i;

      for (i = 0; i < len; i++)
	LABEL_NUSES (XEXP (XVECEXP (pat, diff_vec_p, i), 0))--;
    }

  return next;
}

/* True if a given label can be deleted.  */

static int
can_delete_label_p (label)
     rtx label;
{
  rtx x;

  if (LABEL_PRESERVE_P (label))
    return 0;

  for (x = forced_labels; x; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;
  for (x = label_value_list; x; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;
  for (x = exception_handler_labels; x; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;

  /* User declared labels must be preserved.  */
  if (LABEL_NAME (label) != 0)
    return 0;

  return 1;
}

static int
tail_recursion_label_p (label)
     rtx label;
{
  rtx x;

  for (x = tail_recursion_label_list; x; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 1;

  return 0;
}

/* Blocks A and B are to be merged into a single block A.  The insns
   are already contiguous, hence `nomove'.  */

void
merge_blocks_nomove (a, b)
     basic_block a, b;
{
  edge e;
  rtx b_head, b_end, a_end;
  rtx del_first = NULL_RTX, del_last = NULL_RTX;
  int b_empty = 0;

  /* If there was a CODE_LABEL beginning B, delete it.  */
  b_head = b->head;
  b_end = b->end;
  if (GET_CODE (b_head) == CODE_LABEL)
    {
      /* Detect basic blocks with nothing but a label.  This can happen
	 in particular at the end of a function.  */
      if (b_head == b_end)
	b_empty = 1;
      del_first = del_last = b_head;
      b_head = NEXT_INSN (b_head);
    }

  /* Delete the basic block note.  */
  if (NOTE_INSN_BASIC_BLOCK_P (b_head))
    {
      if (b_head == b_end)
	b_empty = 1;
      if (! del_last)
	del_first = b_head;
      del_last = b_head;
      b_head = NEXT_INSN (b_head);
    }

  /* If there was a jump out of A, delete it.  */
  a_end = a->end;
  if (GET_CODE (a_end) == JUMP_INSN)
    {
      rtx prev;

      for (prev = PREV_INSN (a_end); ; prev = PREV_INSN (prev))
	if (GET_CODE (prev) != NOTE
	    || NOTE_LINE_NUMBER (prev) == NOTE_INSN_BASIC_BLOCK
	    || prev == a->head)
	  break;

      del_first = a_end;

#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (prev && sets_cc0_p (prev))
	{
	  rtx tmp = prev;
	  prev = prev_nonnote_insn (prev);
	  if (!prev)
	    prev = a->head;
	  del_first = tmp;
	}
#endif

      a_end = prev;
    }
  else if (GET_CODE (NEXT_INSN (a_end)) == BARRIER)
    del_first = NEXT_INSN (a_end);

  /* Delete everything marked above as well as crap that might be
     hanging out between the two blocks.  */
  flow_delete_insn_chain (del_first, del_last);

  /* Normally there should only be one successor of A and that is B, but
     partway though the merge of blocks for conditional_execution we'll
     be merging a TEST block with THEN and ELSE successors.  Free the
     whole lot of them and hope the caller knows what they're doing.  */
  while (a->succ)
    remove_edge (a->succ);

  /* Adjust the edges out of B for the new owner.  */
  for (e = b->succ; e; e = e->succ_next)
    e->src = a;
  a->succ = b->succ;

  /* B hasn't quite yet ceased to exist.  Attempt to prevent mishap.  */
  b->pred = b->succ = NULL;

  /* Reassociate the insns of B with A.  */
  if (!b_empty)
    {
      if (basic_block_for_insn)
	{
	  BLOCK_FOR_INSN (b_head) = a;
	  while (b_head != b_end)
	    {
	      b_head = NEXT_INSN (b_head);
	      BLOCK_FOR_INSN (b_head) = a;
	    }
	}
      a_end = b_end;
    }
  a->end = a_end;

  expunge_block (b);
}

/* Blocks A and B are to be merged into a single block.  A has no incoming
   fallthru edge, so it can be moved before B without adding or modifying
   any jumps (aside from the jump from A to B).  */

static int
merge_blocks_move_predecessor_nojumps (a, b)
     basic_block a, b;
{
  rtx start, end, barrier;
  int index;

  start = a->head;
  end = a->end;

  barrier = next_nonnote_insn (end);
  if (GET_CODE (barrier) != BARRIER)
    abort ();
  flow_delete_insn (barrier);

  /* Move block and loop notes out of the chain so that we do not
     disturb their order.

     ??? A better solution would be to squeeze out all the non-nested notes
     and adjust the block trees appropriately.   Even better would be to have
     a tighter connection between block trees and rtl so that this is not
     necessary.  */
  start = squeeze_notes (start, end);

  /* Scramble the insn chain.  */
  if (end != PREV_INSN (b->head))
    reorder_insns (start, end, PREV_INSN (b->head));

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "Moved block %d before %d and merged.\n",
	       a->index, b->index);
    }

  /* Swap the records for the two blocks around.  Although we are deleting B,
     A is now where B was and we want to compact the BB array from where
     A used to be.  */
  BASIC_BLOCK (a->index) = b;
  BASIC_BLOCK (b->index) = a;
  index = a->index;
  a->index = b->index;
  b->index = index;

  /* Now blocks A and B are contiguous.  Merge them.  */
  merge_blocks_nomove (a, b);

  return 1;
}

/* Blocks A and B are to be merged into a single block.  B has no outgoing
   fallthru edge, so it can be moved after A without adding or modifying
   any jumps (aside from the jump from A to B).  */

static int
merge_blocks_move_successor_nojumps (a, b)
     basic_block a, b;
{
  rtx start, end, barrier;

  start = b->head;
  end = b->end;
  barrier = NEXT_INSN (end);

  /* Recognize a jump table following block B.  */
  if (barrier
      && GET_CODE (barrier) == CODE_LABEL
      && NEXT_INSN (barrier)
      && GET_CODE (NEXT_INSN (barrier)) == JUMP_INSN
      && (GET_CODE (PATTERN (NEXT_INSN (barrier))) == ADDR_VEC
	  || GET_CODE (PATTERN (NEXT_INSN (barrier))) == ADDR_DIFF_VEC))
    {
      end = NEXT_INSN (barrier);
      barrier = NEXT_INSN (end);
    }

  /* There had better have been a barrier there.  Delete it.  */
  if (barrier && GET_CODE (barrier) == BARRIER)
    flow_delete_insn (barrier);

  /* Move block and loop notes out of the chain so that we do not
     disturb their order.

     ??? A better solution would be to squeeze out all the non-nested notes
     and adjust the block trees appropriately.   Even better would be to have
     a tighter connection between block trees and rtl so that this is not
     necessary.  */
  start = squeeze_notes (start, end);

  /* Scramble the insn chain.  */
  reorder_insns (start, end, a->end);

  /* Now blocks A and B are contiguous.  Merge them.  */
  merge_blocks_nomove (a, b);

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file, "Moved block %d after %d and merged.\n",
	       b->index, a->index);
    }

  return 1;
}

/* Attempt to merge basic blocks that are potentially non-adjacent.
   Return true iff the attempt succeeded.  */

static int
merge_blocks (e, b, c, mode)
     edge e;
     basic_block b, c;
     int mode;
{
  /* If C has a tail recursion label, do not merge.  There is no
     edge recorded from the call_placeholder back to this label, as
     that would make optimize_sibling_and_tail_recursive_calls more
     complex for no gain.  */
  if (GET_CODE (c->head) == CODE_LABEL
      && tail_recursion_label_p (c->head))
    return 0;

  /* If B has a fallthru edge to C, no need to move anything.  */
  if (e->flags & EDGE_FALLTHRU)
    {
      merge_blocks_nomove (b, c);

      if (rtl_dump_file)
	{
	  fprintf (rtl_dump_file, "Merged %d and %d without moving.\n",
		   b->index, c->index);
	}

      return 1;
    }
  /* Otherwise we will need to move code around.  Do that only if expensive
     transformations are allowed.  */
  else if (mode & CLEANUP_EXPENSIVE)
    {
      edge tmp_edge, c_fallthru_edge;
      int c_has_outgoing_fallthru;
      int b_has_incoming_fallthru;

      /* Avoid overactive code motion, as the forwarder blocks should be
         eliminated by edge redirection instead.  One exception might have
	 been if B is a forwarder block and C has no fallthru edge, but
	 that should be cleaned up by bb-reorder instead.  */
      if (forwarder_block_p (b) || forwarder_block_p (c))
	return 0;

      /* We must make sure to not munge nesting of lexical blocks,
	 and loop notes.  This is done by squeezing out all the notes
	 and leaving them there to lie.  Not ideal, but functional.  */

      for (tmp_edge = c->succ; tmp_edge; tmp_edge = tmp_edge->succ_next)
	if (tmp_edge->flags & EDGE_FALLTHRU)
	  break;
      c_has_outgoing_fallthru = (tmp_edge != NULL);
      c_fallthru_edge = tmp_edge;

      for (tmp_edge = b->pred; tmp_edge; tmp_edge = tmp_edge->pred_next)
	if (tmp_edge->flags & EDGE_FALLTHRU)
	  break;
      b_has_incoming_fallthru = (tmp_edge != NULL);

      /* If B does not have an incoming fallthru, then it can be moved
	 immediately before C without introducing or modifying jumps.
	 C cannot be the first block, so we do not have to worry about
	 accessing a non-existent block.  */
      if (! b_has_incoming_fallthru)
	return merge_blocks_move_predecessor_nojumps (b, c);

      /* Otherwise, we're going to try to move C after B.  If C does
	 not have an outgoing fallthru, then it can be moved
	 immediately after B without introducing or modifying jumps.  */
      if (! c_has_outgoing_fallthru)
	return merge_blocks_move_successor_nojumps (b, c);

      /* Otherwise, we'll need to insert an extra jump, and possibly
	 a new block to contain it.  We can't redirect to EXIT_BLOCK_PTR,
	 as we don't have explicit return instructions before epilogues
	 are generated, so give up on that case.  */

      if (c_fallthru_edge->dest != EXIT_BLOCK_PTR
	  && merge_blocks_move_successor_nojumps (b, c))
        {
	  basic_block target = c_fallthru_edge->dest;
	  rtx barrier;
	  basic_block new;

	  /* This is a dirty hack to avoid code duplication.

	     Set edge to point to wrong basic block, so
	     redirect_edge_and_branch_force will do the trick
	     and rewire edge back to the original location.  */
	  redirect_edge_succ (c_fallthru_edge, ENTRY_BLOCK_PTR);
	  new = redirect_edge_and_branch_force (c_fallthru_edge, target);

	  /* We've just created barrier, but another barrier is
	     already present in the stream.  Avoid the duplicate.  */
	  barrier = next_nonnote_insn (new ? new->end : b->end);
	  if (GET_CODE (barrier) != BARRIER)
	    abort ();
	  flow_delete_insn (barrier);

	  return 1;
        }

      return 0;
    }
  return 0;
}

/* Simplify a conditional jump around an unconditional jump.
   Return true if something changed.  */

static bool
try_simplify_condjump (cbranch_block)
     basic_block cbranch_block;
{
  basic_block jump_block, jump_dest_block, cbranch_dest_block;
  edge cbranch_jump_edge, cbranch_fallthru_edge;
  rtx cbranch_insn;

  /* Verify that there are exactly two successors.  */
  if (!cbranch_block->succ
      || !cbranch_block->succ->succ_next
      || cbranch_block->succ->succ_next->succ_next)
    return false;

  /* Verify that we've got a normal conditional branch at the end
     of the block.  */
  cbranch_insn = cbranch_block->end;
  if (!any_condjump_p (cbranch_insn))
    return false;

  cbranch_fallthru_edge = FALLTHRU_EDGE (cbranch_block);
  cbranch_jump_edge = BRANCH_EDGE (cbranch_block);

  /* The next block must not have multiple predecessors, must not
     be the last block in the function, and must contain just the
     unconditional jump.  */
  jump_block = cbranch_fallthru_edge->dest;
  if (jump_block->pred->pred_next
      || jump_block->index == n_basic_blocks - 1
      || !forwarder_block_p (jump_block))
    return false;
  jump_dest_block = jump_block->succ->dest;

  /* The conditional branch must target the block after the
     unconditional branch.  */
  cbranch_dest_block = cbranch_jump_edge->dest;

  if (!can_fallthru (jump_block, cbranch_dest_block))
    return false;

  /* Invert the conditional branch.  Prevent jump.c from deleting
     "unreachable" instructions.  */
  LABEL_NUSES (JUMP_LABEL (cbranch_insn))++;
  if (!invert_jump (cbranch_insn, block_label (jump_dest_block), 1))
    {
      LABEL_NUSES (JUMP_LABEL (cbranch_insn))--;
      return false;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Simplifying condjump %i around jump %i\n",
	     INSN_UID (cbranch_insn), INSN_UID (jump_block->end));

  /* Success.  Update the CFG to match.  Note that after this point
     the edge variable names appear backwards; the redirection is done
     this way to preserve edge profile data.  */
  redirect_edge_succ_nodup (cbranch_jump_edge, cbranch_dest_block);
  redirect_edge_succ_nodup (cbranch_fallthru_edge, jump_dest_block);
  cbranch_jump_edge->flags |= EDGE_FALLTHRU;
  cbranch_fallthru_edge->flags &= ~EDGE_FALLTHRU;

  /* Delete the block with the unconditional jump, and clean up the mess.  */
  flow_delete_block (jump_block);
  tidy_fallthru_edge (cbranch_jump_edge, cbranch_block, cbranch_dest_block);

  return true;
}

/* Attempt to forward edges leaving basic block B.
   Return true if sucessful.  */

static bool
try_forward_edges (mode, b)
     basic_block b;
     int mode;
{
  bool changed = false;
  edge e, next;

  for (e = b->succ; e ; e = next)
    {
      basic_block target, first;
      int counter;

      next = e->succ_next;

      /* Skip complex edges because we don't know how to update them.

         Still handle fallthru edges, as we can suceed to forward fallthru
         edge to the same place as the branch edge of conditional branch
         and turn conditional branch to an unconditonal branch.  */
      if (e->flags & EDGE_COMPLEX)
	continue;

      target = first = e->dest;
      counter = 0;

      /* Look for the real destination of the jump.
         Avoid inifinite loop in the infinite empty loop by counting
         up to n_basic_blocks.  */
      while (forwarder_block_p (target)
	     && target->succ->dest != EXIT_BLOCK_PTR
	     && counter < n_basic_blocks)
	{
	  /* Bypass trivial infinite loops.  */
	  if (target == target->succ->dest)
	    counter = n_basic_blocks;

	  /* Avoid killing of loop pre-headers, as it is the place loop
	     optimizer wants to hoist code to.

	     For fallthru forwarders, the LOOP_BEG note must appear between
	     the header of block and CODE_LABEL of the loop, for non forwarders
	     it must appear before the JUMP_INSN.  */
	  if (mode & CLEANUP_PRE_LOOP)
	    {
	      rtx insn = (target->succ->flags & EDGE_FALLTHRU
		          ? target->head : prev_nonnote_insn (target->end));

	      if (GET_CODE (insn) != NOTE)
		insn = NEXT_INSN (insn);

	      for (;insn && GET_CODE (insn) != CODE_LABEL && !INSN_P (insn);
		   insn = NEXT_INSN (insn))
		if (GET_CODE (insn) == NOTE
		    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
		  break;

	      if (GET_CODE (insn) == NOTE)
		break;
	    }
	  target = target->succ->dest, counter++;
	}

      if (counter >= n_basic_blocks)
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Infinite loop in BB %i.\n",
		     target->index);
	}
      else if (target == first)
	; /* We didn't do anything.  */
      else
	{
	  /* Save the values now, as the edge may get removed.  */
	  gcov_type edge_count = e->count;
	  int edge_probability = e->probability;

	  if (redirect_edge_and_branch (e, target))
	    {
	      /* We successfully forwarded the edge.  Now update profile
		 data: for each edge we traversed in the chain, remove
		 the original edge's execution count.  */
	      int edge_frequency = ((edge_probability * b->frequency
				     + REG_BR_PROB_BASE / 2)
				    / REG_BR_PROB_BASE);

	      do
		{
		  first->count -= edge_count;
		  first->succ->count -= edge_count;
		  first->frequency -= edge_frequency;
		  first = first->succ->dest;
		}
	      while (first != target);

	      changed = true;
	    }
	  else
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Forwarding edge %i->%i to %i failed.\n",
			 b->index, e->dest->index, target->index);
	    }
	}
    }

  return changed;
}

/* Look through the insns at the end of BB1 and BB2 and find the longest
   sequence that are equivalent.  Store the first insns for that sequence
   in *F1 and *F2 and return the sequence length.

   To simplify callers of this function, if the blocks match exactly,
   store the head of the blocks in *F1 and *F2.  */

static int
flow_find_cross_jump (mode, bb1, bb2, f1, f2)
     int mode ATTRIBUTE_UNUSED;
     basic_block bb1, bb2;
     rtx *f1, *f2;
{
  rtx i1, i2, p1, p2, last1, last2, afterlast1, afterlast2;
  int ninsns = 0;

  /* Skip simple jumps at the end of the blocks.  Complex jumps still
     need to be compared for equivalence, which we'll do below.  */

  i1 = bb1->end;
  if (onlyjump_p (i1))
    i1 = PREV_INSN (i1);
  i2 = bb2->end;
  if (onlyjump_p (i2))
    i2 = PREV_INSN (i2);

  last1 = afterlast1 = last2 = afterlast2 = NULL_RTX;
  while (true)
    {
      /* Ignore notes.  */
      while ((GET_CODE (i1) == NOTE && i1 != bb1->head))
	i1 = PREV_INSN (i1);
      while ((GET_CODE (i2) == NOTE && i2 != bb2->head))
	i2 = PREV_INSN (i2);

      if (i1 == bb1->head || i2 == bb2->head)
	break;

      /* Verify that I1 and I2 are equivalent.  */

      if (GET_CODE (i1) != GET_CODE (i2))
	break;

      p1 = PATTERN (i1);
      p2 = PATTERN (i2);

      /* If this is a CALL_INSN, compare register usage information.
	 If we don't check this on stack register machines, the two
	 CALL_INSNs might be merged leaving reg-stack.c with mismatching
	 numbers of stack registers in the same basic block.
	 If we don't check this on machines with delay slots, a delay slot may
	 be filled that clobbers a parameter expected by the subroutine.

	 ??? We take the simple route for now and assume that if they're
	 equal, they were constructed identically.  */

      if (GET_CODE (i1) == CALL_INSN
	  && ! rtx_equal_p (CALL_INSN_FUNCTION_USAGE (i1),
			    CALL_INSN_FUNCTION_USAGE (i2)))
	break;

#ifdef STACK_REGS
      /* If cross_jump_death_matters is not 0, the insn's mode
	 indicates whether or not the insn contains any stack-like
	 regs.  */

      if ((mode & CLEANUP_POST_REGSTACK) && stack_regs_mentioned (i1))
	{
	  /* If register stack conversion has already been done, then
	     death notes must also be compared before it is certain that
	     the two instruction streams match.  */

	  rtx note;
	  HARD_REG_SET i1_regset, i2_regset;

	  CLEAR_HARD_REG_SET (i1_regset);
	  CLEAR_HARD_REG_SET (i2_regset);

	  for (note = REG_NOTES (i1); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& STACK_REG_P (XEXP (note, 0)))
	      SET_HARD_REG_BIT (i1_regset, REGNO (XEXP (note, 0)));

	  for (note = REG_NOTES (i2); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD
		&& STACK_REG_P (XEXP (note, 0)))
	      SET_HARD_REG_BIT (i2_regset, REGNO (XEXP (note, 0)));

	  GO_IF_HARD_REG_EQUAL (i1_regset, i2_regset, done);

	  break;

	done:
	  ;
	}
#endif

      if (GET_CODE (p1) != GET_CODE (p2))
	break;

      if (! rtx_renumbered_equal_p (p1, p2))
	{
	  /* The following code helps take care of G++ cleanups.  */
	  rtx equiv1 = find_reg_equal_equiv_note (i1);
	  rtx equiv2 = find_reg_equal_equiv_note (i2);

	  if (equiv1 && equiv2
	      /* If the equivalences are not to a constant, they may
		 reference pseudos that no longer exist, so we can't
		 use them.  */
	      && CONSTANT_P (XEXP (equiv1, 0))
	      && rtx_equal_p (XEXP (equiv1, 0), XEXP (equiv2, 0)))
	    {
	      rtx s1 = single_set (i1);
	      rtx s2 = single_set (i2);
	      if (s1 != 0 && s2 != 0
		  && rtx_renumbered_equal_p (SET_DEST (s1), SET_DEST (s2)))
		{
		  validate_change (i1, &SET_SRC (s1), XEXP (equiv1, 0), 1);
		  validate_change (i2, &SET_SRC (s2), XEXP (equiv2, 0), 1);
		  if (! rtx_renumbered_equal_p (p1, p2))
		    cancel_changes (0);
		  else if (apply_change_group ())
		    goto win;
		}
	    }
	  break;
	}

    win:
      /* Don't begin a cross-jump with a USE or CLOBBER insn.  */
      if (GET_CODE (p1) != USE && GET_CODE (p1) != CLOBBER)
	{
	  afterlast1 = last1, afterlast2 = last2;
	  last1 = i1, last2 = i2;
          ninsns++;
	}
      i1 = PREV_INSN (i1);
      i2 = PREV_INSN (i2);
    }

#ifdef HAVE_cc0
  if (ninsns)
    {
      /* Don't allow the insn after a compare to be shared by
	 cross-jumping unless the compare is also shared.  */
      if (reg_mentioned_p (cc0_rtx, last1) && ! sets_cc0_p (last1))
	last1 = afterlast1, last2 = afterlast2, ninsns--;
    }
#endif

  /* Include preceeding notes and labels in the cross-jump.  One,
     this may bring us to the head of the blocks as requested above.
     Two, it keeps line number notes as matched as may be.  */
  if (ninsns)
    {
      while (last1 != bb1->head && GET_CODE (PREV_INSN (last1)) == NOTE)
	last1 = PREV_INSN (last1);
      if (last1 != bb1->head && GET_CODE (PREV_INSN (last1)) == CODE_LABEL)
	last1 = PREV_INSN (last1);
      while (last2 != bb2->head && GET_CODE (PREV_INSN (last2)) == NOTE)
	last2 = PREV_INSN (last2);
      if (last2 != bb2->head && GET_CODE (PREV_INSN (last2)) == CODE_LABEL)
	last2 = PREV_INSN (last2);

      *f1 = last1;
      *f2 = last2;
    }

  return ninsns;
}

/* Return true iff outgoing edges of BB1 and BB2 match, together with
   the branch instruction.  This means that if we commonize the control
   flow before end of the basic block, the semantic remains unchanged.

   We may assume that there exists one edge with a common destination.  */

static bool
outgoing_edges_match (bb1, bb2)
     basic_block bb1;
     basic_block bb2;
{
  /* If BB1 has only one successor, we must be looking at an unconditional
     jump.  Which, by the assumption above, means that we only need to check
     that BB2 has one successor.  */
  if (bb1->succ && !bb1->succ->succ_next)
    return (bb2->succ && !bb2->succ->succ_next);

  /* Match conditional jumps - this may get tricky when fallthru and branch
     edges are crossed.  */
  if (bb1->succ
      && bb1->succ->succ_next
      && !bb1->succ->succ_next->succ_next
      && any_condjump_p (bb1->end))
    {
      edge b1, f1, b2, f2;
      bool reverse, match;
      rtx set1, set2, cond1, cond2;
      enum rtx_code code1, code2;

      if (!bb2->succ
          || !bb2->succ->succ_next
	  || bb1->succ->succ_next->succ_next
	  || !any_condjump_p (bb2->end))
	return false;

      b1 = BRANCH_EDGE (bb1);
      b2 = BRANCH_EDGE (bb2);
      f1 = FALLTHRU_EDGE (bb1);
      f2 = FALLTHRU_EDGE (bb2);

      /* Get around possible forwarders on fallthru edges.  Other cases
         should be optimized out already.  */
      if (forwarder_block_p (f1->dest))
	f1 = f1->dest->succ;
      if (forwarder_block_p (f2->dest))
	f2 = f2->dest->succ;

      /* To simplify use of this function, return false if there are
	 unneeded forwarder blocks.  These will get eliminated later
	 during cleanup_cfg.  */
      if (forwarder_block_p (f1->dest)
	  || forwarder_block_p (f2->dest)
	  || forwarder_block_p (b1->dest)
	  || forwarder_block_p (b2->dest))
	return false;

      if (f1->dest == f2->dest && b1->dest == b2->dest)
	reverse = false;
      else if (f1->dest == b2->dest && b1->dest == f2->dest)
	reverse = true;
      else
	return false;

      set1 = pc_set (bb1->end);
      set2 = pc_set (bb2->end);
      if ((XEXP (SET_SRC (set1), 1) == pc_rtx)
	  != (XEXP (SET_SRC (set2), 1) == pc_rtx))
	reverse = !reverse;

      cond1 = XEXP (SET_SRC (set1), 0);
      cond2 = XEXP (SET_SRC (set2), 0);
      code1 = GET_CODE (cond1);
      if (reverse)
	code2 = reversed_comparison_code (cond2, bb2->end);
      else
	code2 = GET_CODE (cond2);
      if (code2 == UNKNOWN)
	return false;

      /* Verify codes and operands match.  */
      match = ((code1 == code2
		&& rtx_renumbered_equal_p (XEXP (cond1, 0), XEXP (cond2, 0))
		&& rtx_renumbered_equal_p (XEXP (cond1, 1), XEXP (cond2, 1)))
	       || (code1 == swap_condition (code2)
		   && rtx_renumbered_equal_p (XEXP (cond1, 1),
					      XEXP (cond2, 0))
		   && rtx_renumbered_equal_p (XEXP (cond1, 0),
					      XEXP (cond2, 1))));

      /* If we return true, we will join the blocks.  Which means that
	 we will only have one branch prediction bit to work with.  Thus
	 we require the existing branches to have probabilities that are
	 roughly similar.  */
      /* ??? We should use bb->frequency to allow merging in infrequently
	 executed blocks, but at the moment it is not available when
	 cleanup_cfg is run.  */
      if (match && !optimize_size)
	{
	  rtx note1, note2;
	  int prob1, prob2;
	  note1 = find_reg_note (bb1->end, REG_BR_PROB, 0);
	  note2 = find_reg_note (bb2->end, REG_BR_PROB, 0);

	  if (note1 && note2)
	    {
	      prob1 = INTVAL (XEXP (note1, 0));
	      prob2 = INTVAL (XEXP (note2, 0));
	      if (reverse)
		prob2 = REG_BR_PROB_BASE - prob2;

	      /* Fail if the difference in probabilities is
		 greater than 5%.  */
	      if (abs (prob1 - prob2) > REG_BR_PROB_BASE / 20)
		return false;
	    }
	  else if (note1 || note2)
	    return false;
	}

      if (rtl_dump_file && match)
	fprintf (rtl_dump_file, "Conditionals in bb %i and %i match.\n",
		 bb1->index, bb2->index);

      return match;
    }

  /* ??? We can handle computed jumps too.  This may be important for
     inlined functions containing switch statements.  Also jumps w/o
     fallthru edges can be handled by simply matching whole insn.  */
  return false;
}

/* E1 and E2 are edges with the same destination block.  Search their
   predecessors for common code.  If found, redirect control flow from
   (maybe the middle of) E1->SRC to (maybe the middle of) E2->SRC.  */

static bool
try_crossjump_to_edge (mode, e1, e2)
     int mode;
     edge e1, e2;
{
  int nmatch;
  basic_block src1 = e1->src, src2 = e2->src;
  basic_block redirect_to;
  rtx newpos1, newpos2;
  edge s;
  rtx last;
  rtx label;
  rtx note;

  /* Search backward through forwarder blocks.  We don't need to worry
     about multiple entry or chained forwarders, as they will be optimized
     away.  We do this to look past the unconditional jump following a
     conditional jump that is required due to the current CFG shape.  */
  if (src1->pred
      && !src1->pred->pred_next
      && forwarder_block_p (src1))
    {
      e1 = src1->pred;
      src1 = e1->src;
    }
  if (src2->pred
      && !src2->pred->pred_next
      && forwarder_block_p (src2))
    {
      e2 = src2->pred;
      src2 = e2->src;
    }

  /* Nothing to do if we reach ENTRY, or a common source block.  */
  if (src1 == ENTRY_BLOCK_PTR || src2 == ENTRY_BLOCK_PTR)
    return false;
  if (src1 == src2)
    return false;

  /* Seeing more than 1 forwarder blocks would confuse us later...  */
  if (forwarder_block_p (e1->dest)
      && forwarder_block_p (e1->dest->succ->dest))
    return false;
  if (forwarder_block_p (e2->dest)
      && forwarder_block_p (e2->dest->succ->dest))
    return false;

  /* Likewise with dead code (possibly newly created by the other optimizations
     of cfg_cleanup).  */
  if (!src1->pred || !src2->pred)
    return false;

  /* Likewise with complex edges.
     ??? We should be able to handle most complex edges later with some
     care.  */
  if (e1->flags & EDGE_COMPLEX)
    return false;

  /* Look for the common insn sequence, part the first ...  */
  if (!outgoing_edges_match (src1, src2))
    return false;

  /* ... and part the second.  */
  nmatch = flow_find_cross_jump (mode, src1, src2, &newpos1, &newpos2);
  if (!nmatch)
    return false;

  /* Avoid splitting if possible.  */
  if (newpos2 == src2->head)
    redirect_to = src2;
  else
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Splitting bb %i before %i insns\n",
		 src2->index, nmatch);
      redirect_to = split_block (src2, PREV_INSN (newpos2))->dest;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "Cross jumping from bb %i to bb %i; %i common insns\n",
	     src1->index, src2->index, nmatch);

  redirect_to->count += src1->count;
  redirect_to->frequency += src1->frequency;

  /* Recompute the frequencies and counts of outgoing edges.  */
  for (s = redirect_to->succ; s; s = s->succ_next)
    {
      edge s2;
      basic_block d = s->dest;

      if (forwarder_block_p (d))
	d = d->succ->dest;
      for (s2 = src1->succ; ; s2 = s2->succ_next)
	{
	  basic_block d2 = s2->dest;
	  if (forwarder_block_p (d2))
	    d2 = d2->succ->dest;
	  if (d == d2)
	    break;
	}
      s->count += s2->count;

      /* Take care to update possible forwarder blocks.  We verified
         that there is no more than one in the chain, so we can't run
         into infinite loop.  */
      if (forwarder_block_p (s->dest))
	{
	  s->dest->succ->count += s2->count;
	  s->dest->count += s2->count;
	  s->dest->frequency += EDGE_FREQUENCY (s);
	}
      if (forwarder_block_p (s2->dest))
	{
	  s2->dest->succ->count -= s2->count;
	  s2->dest->count -= s2->count;
	  s2->dest->frequency -= EDGE_FREQUENCY (s);
	}
      if (!redirect_to->frequency && !src1->frequency)
	s->probability = (s->probability + s2->probability) / 2;
      else
	s->probability =
	  ((s->probability * redirect_to->frequency +
	    s2->probability * src1->frequency)
	   / (redirect_to->frequency + src1->frequency));
    }

  note = find_reg_note (redirect_to->end, REG_BR_PROB, 0);
  if (note)
    XEXP (note, 0) = GEN_INT (BRANCH_EDGE (redirect_to)->probability);

  /* Edit SRC1 to go to REDIRECT_TO at NEWPOS1.  */

  /* Skip possible basic block header.  */
  if (GET_CODE (newpos1) == CODE_LABEL)
    newpos1 = NEXT_INSN (newpos1);
  if (GET_CODE (newpos1) == NOTE)
    newpos1 = NEXT_INSN (newpos1);
  last = src1->end;

  /* Emit the jump insn.   */
  label = block_label (redirect_to);
  src1->end = emit_jump_insn_before (gen_jump (label), newpos1);
  JUMP_LABEL (src1->end) = label;
  LABEL_NUSES (label)++;
  if (basic_block_for_insn)
    set_block_for_new_insns (src1->end, src1);

  /* Delete the now unreachable instructions.  */
  flow_delete_insn_chain (newpos1, last);

  /* Make sure there is a barrier after the new jump.  */
  last = next_nonnote_insn (src1->end);
  if (!last || GET_CODE (last) != BARRIER)
    emit_barrier_after (src1->end);

  /* Update CFG.  */
  while (src1->succ)
    remove_edge (src1->succ);
  make_edge (NULL, src1, redirect_to, 0);
  src1->succ->probability = REG_BR_PROB_BASE;
  src1->succ->count = src1->count;

  return true;
}

/* Search the predecessors of BB for common insn sequences.  When found,
   share code between them by redirecting control flow.  Return true if
   any changes made.  */

static bool
try_crossjump_bb (mode, bb)
     int mode;
     basic_block bb;
{
  edge e, e2, nexte2, nexte, fallthru;
  bool changed;

  /* Nothing to do if there is not at least two incomming edges.  */
  if (!bb->pred || !bb->pred->pred_next)
    return false;

  /* It is always cheapest to redirect a block that ends in a branch to
     a block that falls through into BB, as that adds no branches to the
     program.  We'll try that combination first.  */
  for (fallthru = bb->pred; fallthru; fallthru = fallthru->pred_next)
    if (fallthru->flags & EDGE_FALLTHRU)
      break;

  changed = false;
  for (e = bb->pred; e; e = nexte)
    {
      nexte = e->pred_next;

      /* Elide complex edges now, as neither try_crossjump_to_edge
	 nor outgoing_edges_match can handle them.  */
      if (e->flags & EDGE_COMPLEX)
	continue;

      /* As noted above, first try with the fallthru predecessor.  */
      if (fallthru)
	{
	  /* Don't combine the fallthru edge into anything else.
	     If there is a match, we'll do it the other way around.  */
	  if (e == fallthru)
	    continue;

	  if (try_crossjump_to_edge (mode, e, fallthru))
	    {
	      changed = true;
	      nexte = bb->pred;
	      continue;
	    }
	}

      /* Non-obvious work limiting check: Recognize that we're going
	 to call try_crossjump_bb on every basic block.  So if we have
	 two blocks with lots of outgoing edges (a switch) and they
	 share lots of common destinations, then we would do the
	 cross-jump check once for each common destination.

	 Now, if the blocks actually are cross-jump candidates, then
	 all of their destinations will be shared.  Which means that
	 we only need check them for cross-jump candidacy once.  We
	 can eliminate redundant checks of crossjump(A,B) by arbitrarily
	 choosing to do the check from the block for which the edge
	 in question is the first successor of A.  */
      if (e->src->succ != e)
	continue;

      for (e2 = bb->pred; e2; e2 = nexte2)
	{
	  nexte2 = e2->pred_next;

	  if (e2 == e)
	    continue;

	  /* We've already checked the fallthru edge above.  */
	  if (e2 == fallthru)
	    continue;

	  /* Again, neither try_crossjump_to_edge nor outgoing_edges_match
	     can handle complex edges.  */
	  if (e2->flags & EDGE_COMPLEX)
	    continue;

	  /* The "first successor" check above only prevents multiple
	     checks of crossjump(A,B).  In order to prevent redundant
	     checks of crossjump(B,A), require that A be the block
	     with the lowest index.  */
	  if (e->src->index > e2->src->index)
	    continue;

	  if (try_crossjump_to_edge (mode, e, e2))
	    {
	      changed = true;
	      nexte = bb->pred;
	      break;
	    }
	}
    }

  return changed;
}

/* Do simple CFG optimizations - basic block merging, simplifying of jump
   instructions etc.  Return nonzero if changes were made.  */

static bool
try_optimize_cfg (mode)
     int mode;
{
  int i;
  bool changed_overall = false;
  bool changed;
  int iterations = 0;

  /* Attempt to merge blocks as made possible by edge removal.  If a block
     has only one successor, and the successor has only one predecessor,
     they may be combined.  */

  do
    {
      changed = false;
      iterations++;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "\n\ntry_optimize_cfg iteration %i\n\n",
		 iterations);

      for (i = 0; i < n_basic_blocks;)
	{
	  basic_block c, b = BASIC_BLOCK (i);
	  edge s;
	  bool changed_here = false;

	  /* Delete trivially dead basic blocks.  */
	  while (b->pred == NULL)
	    {
	      c = BASIC_BLOCK (b->index - 1);
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Deleting block %i.\n", b->index);
	      flow_delete_block (b);
	      changed = true;
	      b = c;
	    }

	  /* Remove code labels no longer used.  Don't do this before
	     CALL_PLACEHOLDER is removed, as some branches may be hidden
	     within.  */
	  if (b->pred->pred_next == NULL
	      && (b->pred->flags & EDGE_FALLTHRU)
	      && !(b->pred->flags & EDGE_COMPLEX)
	      && GET_CODE (b->head) == CODE_LABEL
	      && (!(mode & CLEANUP_PRE_SIBCALL)
		  || !tail_recursion_label_p (b->head))
	      /* If previous block ends with condjump jumping to next BB,
	         we can't delete the label.  */
	      && (b->pred->src == ENTRY_BLOCK_PTR
		  || !reg_mentioned_p (b->head, b->pred->src->end)))
	    {
	      rtx label = b->head;
	      b->head = NEXT_INSN (b->head);
	      flow_delete_insn_chain (label, label);
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Deleted label in block %i.\n",
			 b->index);
	    }

	  /* If we fall through an empty block, we can remove it.  */
	  if (b->pred->pred_next == NULL
	      && (b->pred->flags & EDGE_FALLTHRU)
	      && GET_CODE (b->head) != CODE_LABEL
	      && forwarder_block_p (b)
	      /* Note that forwarder_block_p true ensures that there
		 is a successor for this block.  */
	      && (b->succ->flags & EDGE_FALLTHRU)
	      && n_basic_blocks > 1)
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file, "Deleting fallthru block %i.\n",
			 b->index);
	      c = BASIC_BLOCK (b->index ? b->index - 1 : 1);
	      redirect_edge_succ_nodup (b->pred, b->succ->dest);
	      flow_delete_block (b);
	      changed = true;
	      b = c;
	    }

	  /* Merge blocks.  Loop because chains of blocks might be
	     combineable.  */
	  while ((s = b->succ) != NULL
		 && s->succ_next == NULL
	         && !(s->flags & EDGE_COMPLEX)
		 && (c = s->dest) != EXIT_BLOCK_PTR
		 && c->pred->pred_next == NULL
		 /* If the jump insn has side effects,
		    we can't kill the edge.  */
		 && (GET_CODE (b->end) != JUMP_INSN
		     || onlyjump_p (b->end))
		 && merge_blocks (s, b, c, mode))
	    changed_here = true;

	  /* Simplify branch over branch.  */
	  if ((mode & CLEANUP_EXPENSIVE) && try_simplify_condjump (b))
	    changed_here = true;

	  /* If B has a single outgoing edge, but uses a non-trivial jump
	     instruction without side-effects, we can either delete the
	     jump entirely, or replace it with a simple unconditional jump.
	     Use redirect_edge_and_branch to do the dirty work.  */
	  if (b->succ
	      && ! b->succ->succ_next
	      && b->succ->dest != EXIT_BLOCK_PTR
	      && onlyjump_p (b->end)
	      && redirect_edge_and_branch (b->succ, b->succ->dest))
	    changed_here = true;

	  /* Simplify branch to branch.  */
	  if (try_forward_edges (mode, b))
	    changed_here = true;

	  /* Look for shared code between blocks.  */
	  if ((mode & CLEANUP_CROSSJUMP)
	      && try_crossjump_bb (mode, b))
	    changed_here = true;

	  /* Don't get confused by the index shift caused by deleting
	     blocks.  */
	  if (!changed_here)
	    i = b->index + 1;
	  else
	    changed = true;
	}

      if ((mode & CLEANUP_CROSSJUMP)
	  && try_crossjump_bb (mode, EXIT_BLOCK_PTR))
	changed = true;

#ifdef ENABLE_CHECKING
      if (changed)
	verify_flow_info ();
#endif

      changed_overall |= changed;
    }
  while (changed);
  return changed_overall;
}

/* The given edge should potentially be a fallthru edge.  If that is in
   fact true, delete the jump and barriers that are in the way.  */

void
tidy_fallthru_edge (e, b, c)
     edge e;
     basic_block b, c;
{
  rtx q;

  /* ??? In a late-running flow pass, other folks may have deleted basic
     blocks by nopping out blocks, leaving multiple BARRIERs between here
     and the target label. They ought to be chastized and fixed.

     We can also wind up with a sequence of undeletable labels between
     one block and the next.

     So search through a sequence of barriers, labels, and notes for
     the head of block C and assert that we really do fall through.  */

  if (next_real_insn (b->end) != next_real_insn (PREV_INSN (c->head)))
    return;

  /* Remove what will soon cease being the jump insn from the source block.
     If block B consisted only of this single jump, turn it into a deleted
     note.  */
  q = b->end;
  if (GET_CODE (q) == JUMP_INSN
      && onlyjump_p (q)
      && (any_uncondjump_p (q)
	  || (b->succ == e && e->succ_next == NULL)))
    {
#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (any_condjump_p (q) && sets_cc0_p (PREV_INSN (q)))
	q = PREV_INSN (q);
#endif

      if (b->head == q)
	{
	  PUT_CODE (q, NOTE);
	  NOTE_LINE_NUMBER (q) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (q) = 0;
	}
      else
	{
	  q = PREV_INSN (q);

	  /* We don't want a block to end on a line-number note since that has
	     the potential of changing the code between -g and not -g.  */
	  while (GET_CODE (q) == NOTE && NOTE_LINE_NUMBER (q) >= 0)
	    q = PREV_INSN (q);
	}

      b->end = q;
    }

  /* Selectively unlink the sequence.  */
  if (q != PREV_INSN (c->head))
    flow_delete_insn_chain (NEXT_INSN (q), PREV_INSN (c->head));

  e->flags |= EDGE_FALLTHRU;
}

/* Fix up edges that now fall through, or rather should now fall through
   but previously required a jump around now deleted blocks.  Simplify
   the search by only examining blocks numerically adjacent, since this
   is how find_basic_blocks created them.  */

static void
tidy_fallthru_edges ()
{
  int i;

  for (i = 1; i < n_basic_blocks; ++i)
    {
      basic_block b = BASIC_BLOCK (i - 1);
      basic_block c = BASIC_BLOCK (i);
      edge s;

      /* We care about simple conditional or unconditional jumps with
	 a single successor.

	 If we had a conditional branch to the next instruction when
	 find_basic_blocks was called, then there will only be one
	 out edge for the block which ended with the conditional
	 branch (since we do not create duplicate edges).

	 Furthermore, the edge will be marked as a fallthru because we
	 merge the flags for the duplicate edges.  So we do not want to
	 check that the edge is not a FALLTHRU edge.  */
      if ((s = b->succ) != NULL
	  && ! (s->flags & EDGE_COMPLEX)
	  && s->succ_next == NULL
	  && s->dest == c
	  /* If the jump insn has side effects, we can't tidy the edge.  */
	  && (GET_CODE (b->end) != JUMP_INSN
	      || onlyjump_p (b->end)))
	tidy_fallthru_edge (s, b, c);
    }
}

/* Perform data flow analysis.
   F is the first insn of the function; FLAGS is a set of PROP_* flags
   to be used in accumulating flow info.  */

void
life_analysis (f, file, flags)
     rtx f;
     FILE *file;
     int flags;
{
#ifdef ELIMINABLE_REGS
  register int i;
  static struct {int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif

  /* Record which registers will be eliminated.  We use this in
     mark_used_regs.  */

  CLEAR_HARD_REG_SET (elim_reg_set);

#ifdef ELIMINABLE_REGS
  for (i = 0; i < (int) ARRAY_SIZE (eliminables); i++)
    SET_HARD_REG_BIT (elim_reg_set, eliminables[i].from);
#else
  SET_HARD_REG_BIT (elim_reg_set, FRAME_POINTER_REGNUM);
#endif

  if (! optimize)
    flags &= ~(PROP_LOG_LINKS | PROP_AUTOINC | PROP_ALLOW_CFG_CHANGES);

  /* The post-reload life analysis have (on a global basis) the same
     registers live as was computed by reload itself.  elimination
     Otherwise offsets and such may be incorrect.

     Reload will make some registers as live even though they do not
     appear in the rtl.

     We don't want to create new auto-incs after reload, since they
     are unlikely to be useful and can cause problems with shared
     stack slots.  */
  if (reload_completed)
    flags &= ~(PROP_REG_INFO | PROP_AUTOINC);

  /* We want alias analysis information for local dead store elimination.  */
  if (optimize && (flags & PROP_SCAN_DEAD_CODE))
    init_alias_analysis ();

  /* Always remove no-op moves.  Do this before other processing so
     that we don't have to keep re-scanning them.  */
  delete_noop_moves (f);

  /* Some targets can emit simpler epilogues if they know that sp was
     not ever modified during the function.  After reload, of course,
     we've already emitted the epilogue so there's no sense searching.  */
  if (! reload_completed)
    notice_stack_pointer_modification (f);

  /* Allocate and zero out data structures that will record the
     data from lifetime analysis.  */
  allocate_reg_life_data ();
  allocate_bb_life_data ();

  /* Find the set of registers live on function exit.  */
  mark_regs_live_at_end (EXIT_BLOCK_PTR->global_live_at_start);

  /* "Update" life info from zero.  It'd be nice to begin the
     relaxation with just the exit and noreturn blocks, but that set
     is not immediately handy.  */

  if (flags & PROP_REG_INFO)
    memset (regs_ever_live, 0, sizeof (regs_ever_live));
  update_life_info (NULL, UPDATE_LIFE_GLOBAL, flags);

  /* Clean up.  */
  if (optimize && (flags & PROP_SCAN_DEAD_CODE))
    end_alias_analysis ();

  if (file)
    dump_flow_info (file);

  free_basic_block_vars (1);

#ifdef ENABLE_CHECKING
  {
    rtx insn;

    /* Search for any REG_LABEL notes which reference deleted labels.  */
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      {
	rtx inote = find_reg_note (insn, REG_LABEL, NULL_RTX);

	if (inote && GET_CODE (inote) == NOTE_INSN_DELETED_LABEL)
	  abort ();
      }
  }
#endif
  /* Removing dead insns should've made jumptables really dead.  */
  delete_dead_jumptables ();
}

/* A subroutine of verify_wide_reg, called through for_each_rtx.
   Search for REGNO.  If found, abort if it is not wider than word_mode.  */

static int
verify_wide_reg_1 (px, pregno)
     rtx *px;
     void *pregno;
{
  rtx x = *px;
  unsigned int regno = *(int *) pregno;

  if (GET_CODE (x) == REG && REGNO (x) == regno)
    {
      if (GET_MODE_BITSIZE (GET_MODE (x)) <= BITS_PER_WORD)
	abort ();
      return 1;
    }
  return 0;
}

/* A subroutine of verify_local_live_at_start.  Search through insns
   between HEAD and END looking for register REGNO.  */

static void
verify_wide_reg (regno, head, end)
     int regno;
     rtx head, end;
{
  while (1)
    {
      if (INSN_P (head)
	  && for_each_rtx (&PATTERN (head), verify_wide_reg_1, &regno))
	return;
      if (head == end)
	break;
      head = NEXT_INSN (head);
    }

  /* We didn't find the register at all.  Something's way screwy.  */
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Aborting in verify_wide_reg; reg %d\n", regno);
  print_rtl_and_abort ();
}

/* A subroutine of update_life_info.  Verify that there are no untoward
   changes in live_at_start during a local update.  */

static void
verify_local_live_at_start (new_live_at_start, bb)
     regset new_live_at_start;
     basic_block bb;
{
  if (reload_completed)
    {
      /* After reload, there are no pseudos, nor subregs of multi-word
	 registers.  The regsets should exactly match.  */
      if (! REG_SET_EQUAL_P (new_live_at_start, bb->global_live_at_start))
	{
	  if (rtl_dump_file)
	    {
	      fprintf (rtl_dump_file,
		       "live_at_start mismatch in bb %d, aborting\n",
		       bb->index);
	      debug_bitmap_file (rtl_dump_file, bb->global_live_at_start);
	      debug_bitmap_file (rtl_dump_file, new_live_at_start);
	    }
	  print_rtl_and_abort ();
	}
    }
  else
    {
      int i;

      /* Find the set of changed registers.  */
      XOR_REG_SET (new_live_at_start, bb->global_live_at_start);

      EXECUTE_IF_SET_IN_REG_SET (new_live_at_start, 0, i,
	{
          /* No registers should die.  */
	  if (REGNO_REG_SET_P (bb->global_live_at_start, i))
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file,
			 "Register %d died unexpectedly in block %d\n", i,
			 bb->index);
	      print_rtl_and_abort ();
	    }

          /* Verify that the now-live register is wider than word_mode.  */
	  verify_wide_reg (i, bb->head, bb->end);
	});
    }
}

/* Updates life information starting with the basic blocks set in BLOCKS.
   If BLOCKS is null, consider it to be the universal set.

   If EXTENT is UPDATE_LIFE_LOCAL, such as after splitting or peepholeing,
   we are only expecting local modifications to basic blocks.  If we find
   extra registers live at the beginning of a block, then we either killed
   useful data, or we have a broken split that wants data not provided.
   If we find registers removed from live_at_start, that means we have
   a broken peephole that is killing a register it shouldn't.

   ??? This is not true in one situation -- when a pre-reload splitter
   generates subregs of a multi-word pseudo, current life analysis will
   lose the kill.  So we _can_ have a pseudo go live.  How irritating.

   Including PROP_REG_INFO does not properly refresh regs_ever_live
   unless the caller resets it to zero.  */

void
update_life_info (blocks, extent, prop_flags)
     sbitmap blocks;
     enum update_life_extent extent;
     int prop_flags;
{
  regset tmp;
  regset_head tmp_head;
  int i;

  tmp = INITIALIZE_REG_SET (tmp_head);

  /* Changes to the CFG are only allowed when
     doing a global update for the entire CFG.  */
  if ((prop_flags & PROP_ALLOW_CFG_CHANGES)
      && (extent == UPDATE_LIFE_LOCAL || blocks))
    abort ();

  /* For a global update, we go through the relaxation process again.  */
  if (extent != UPDATE_LIFE_LOCAL)
    {
      for ( ; ; )
	{
	  int changed = 0;

	  calculate_global_regs_live (blocks, blocks,
				prop_flags & (PROP_SCAN_DEAD_CODE
					      | PROP_ALLOW_CFG_CHANGES));

	  if ((prop_flags & (PROP_KILL_DEAD_CODE | PROP_ALLOW_CFG_CHANGES))
	      != (PROP_KILL_DEAD_CODE | PROP_ALLOW_CFG_CHANGES))
	    break;

	  /* Removing dead code may allow the CFG to be simplified which
	     in turn may allow for further dead code detection / removal.  */
	  for (i = n_basic_blocks - 1; i >= 0; --i)
	    {
	      basic_block bb = BASIC_BLOCK (i);

	      COPY_REG_SET (tmp, bb->global_live_at_end);
	      changed |= propagate_block (bb, tmp, NULL, NULL,
				prop_flags & (PROP_SCAN_DEAD_CODE
					      | PROP_KILL_DEAD_CODE));
	    }

	  if (! changed || ! try_optimize_cfg (CLEANUP_EXPENSIVE))
	    break;

	  delete_unreachable_blocks ();
	  mark_critical_edges ();
	}

      /* If asked, remove notes from the blocks we'll update.  */
      if (extent == UPDATE_LIFE_GLOBAL_RM_NOTES)
	count_or_remove_death_notes (blocks, 1);
    }

  if (blocks)
    {
      EXECUTE_IF_SET_IN_SBITMAP (blocks, 0, i,
	{
	  basic_block bb = BASIC_BLOCK (i);

	  COPY_REG_SET (tmp, bb->global_live_at_end);
	  propagate_block (bb, tmp, NULL, NULL, prop_flags);

	  if (extent == UPDATE_LIFE_LOCAL)
	    verify_local_live_at_start (tmp, bb);
	});
    }
  else
    {
      for (i = n_basic_blocks - 1; i >= 0; --i)
	{
	  basic_block bb = BASIC_BLOCK (i);

	  COPY_REG_SET (tmp, bb->global_live_at_end);
	  propagate_block (bb, tmp, NULL, NULL, prop_flags);

	  if (extent == UPDATE_LIFE_LOCAL)
	    verify_local_live_at_start (tmp, bb);
	}
    }

  FREE_REG_SET (tmp);

  if (prop_flags & PROP_REG_INFO)
    {
      /* The only pseudos that are live at the beginning of the function
	 are those that were not set anywhere in the function.  local-alloc
	 doesn't know how to handle these correctly, so mark them as not
	 local to any one basic block.  */
      EXECUTE_IF_SET_IN_REG_SET (ENTRY_BLOCK_PTR->global_live_at_end,
				 FIRST_PSEUDO_REGISTER, i,
				 { REG_BASIC_BLOCK (i) = REG_BLOCK_GLOBAL; });

      /* We have a problem with any pseudoreg that lives across the setjmp.
	 ANSI says that if a user variable does not change in value between
	 the setjmp and the longjmp, then the longjmp preserves it.  This
	 includes longjmp from a place where the pseudo appears dead.
	 (In principle, the value still exists if it is in scope.)
	 If the pseudo goes in a hard reg, some other value may occupy
	 that hard reg where this pseudo is dead, thus clobbering the pseudo.
	 Conclusion: such a pseudo must not go in a hard reg.  */
      EXECUTE_IF_SET_IN_REG_SET (regs_live_at_setjmp,
				 FIRST_PSEUDO_REGISTER, i,
				 {
				   if (regno_reg_rtx[i] != 0)
				     {
				       REG_LIVE_LENGTH (i) = -1;
				       REG_BASIC_BLOCK (i) = REG_BLOCK_UNKNOWN;
				     }
				 });
    }
}

/* Free the variables allocated by find_basic_blocks.

   KEEP_HEAD_END_P is non-zero if basic_block_info is not to be freed.  */

void
free_basic_block_vars (keep_head_end_p)
     int keep_head_end_p;
{
  if (basic_block_for_insn)
    {
      VARRAY_FREE (basic_block_for_insn);
      basic_block_for_insn = NULL;
    }

  if (! keep_head_end_p)
    {
      if (basic_block_info)
	{
	  clear_edges ();
	  VARRAY_FREE (basic_block_info);
	}
      n_basic_blocks = 0;

      ENTRY_BLOCK_PTR->aux = NULL;
      ENTRY_BLOCK_PTR->global_live_at_end = NULL;
      EXIT_BLOCK_PTR->aux = NULL;
      EXIT_BLOCK_PTR->global_live_at_start = NULL;
    }
}

/* Delete any insns that copy a register to itself.  */

void
delete_noop_moves (f)
     rtx f ATTRIBUTE_UNUSED;
{
  int i;
  rtx insn, next;
  basic_block bb;

  for (i = 0; i < n_basic_blocks; i++)
    {
      bb = BASIC_BLOCK (i);
      for (insn = bb->head; insn != NEXT_INSN (bb->end); insn = next)
	{
	  next = NEXT_INSN (insn);
	  if (INSN_P (insn) && noop_move_p (insn))
	    {
	      /* Do not call flow_delete_insn here to not confuse backward
	         pointers of LIBCALL block.  */
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;
	    }
	}
    }
}

/* Delete any jump tables never referenced.  We can't delete them at the
   time of removing tablejump insn as they are referenced by the preceeding
   insns computing the destination, so we delay deleting and garbagecollect
   them once life information is computed.  */
static void
delete_dead_jumptables ()
{
  rtx insn, next;
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (GET_CODE (insn) == CODE_LABEL
	  && LABEL_NUSES (insn) == 0
	  && GET_CODE (next) == JUMP_INSN
	  && (GET_CODE (PATTERN (next)) == ADDR_VEC
	      || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
	{
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file, "Dead jumptable %i removed\n", INSN_UID (insn));
	  flow_delete_insn (NEXT_INSN (insn));
	  flow_delete_insn (insn);
	  next = NEXT_INSN (next);
	}
    }
}

/* Determine if the stack pointer is constant over the life of the function.
   Only useful before prologues have been emitted.  */

static void
notice_stack_pointer_modification_1 (x, pat, data)
     rtx x;
     rtx pat ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  if (x == stack_pointer_rtx
      /* The stack pointer is only modified indirectly as the result
	 of a push until later in flow.  See the comments in rtl.texi
	 regarding Embedded Side-Effects on Addresses.  */
      || (GET_CODE (x) == MEM
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == 'a'
	  && XEXP (XEXP (x, 0), 0) == stack_pointer_rtx))
    current_function_sp_is_unchanging = 0;
}

static void
notice_stack_pointer_modification (f)
     rtx f;
{
  rtx insn;

  /* Assume that the stack pointer is unchanging if alloca hasn't
     been used.  */
  current_function_sp_is_unchanging = !current_function_calls_alloca;
  if (! current_function_sp_is_unchanging)
    return;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  /* Check if insn modifies the stack pointer.  */
	  note_stores (PATTERN (insn), notice_stack_pointer_modification_1,
		       NULL);
	  if (! current_function_sp_is_unchanging)
	    return;
	}
    }
}

/* Mark a register in SET.  Hard registers in large modes get all
   of their component registers set as well.  */

static void
mark_reg (reg, xset)
     rtx reg;
     void *xset;
{
  regset set = (regset) xset;
  int regno = REGNO (reg);

  if (GET_MODE (reg) == BLKmode)
    abort ();

  SET_REGNO_REG_SET (set, regno);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (--n > 0)
	SET_REGNO_REG_SET (set, regno + n);
    }
}

/* Mark those regs which are needed at the end of the function as live
   at the end of the last basic block.  */

static void
mark_regs_live_at_end (set)
     regset set;
{
  unsigned int i;

  /* If exiting needs the right stack value, consider the stack pointer
     live at the end of the function.  */
  if ((HAVE_epilogue && reload_completed)
      || ! EXIT_IGNORE_STACK
      || (! FRAME_POINTER_REQUIRED
	  && ! current_function_calls_alloca
	  && flag_omit_frame_pointer)
      || current_function_sp_is_unchanging)
    {
      SET_REGNO_REG_SET (set, STACK_POINTER_REGNUM);
    }

  /* Mark the frame pointer if needed at the end of the function.  If
     we end up eliminating it, it will be removed from the live list
     of each basic block by reload.  */

  if (! reload_completed || frame_pointer_needed)
    {
      SET_REGNO_REG_SET (set, FRAME_POINTER_REGNUM);
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
      /* If they are different, also mark the hard frame pointer as live.  */
      if (! LOCAL_REGNO (HARD_FRAME_POINTER_REGNUM))
        SET_REGNO_REG_SET (set, HARD_FRAME_POINTER_REGNUM);
#endif
    }

#ifndef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
  /* Many architectures have a GP register even without flag_pic.
     Assume the pic register is not in use, or will be handled by
     other means, if it is not fixed.  */
  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
      && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
    SET_REGNO_REG_SET (set, PIC_OFFSET_TABLE_REGNUM);
#endif

  /* Mark all global registers, and all registers used by the epilogue
     as being live at the end of the function since they may be
     referenced by our caller.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i] || EPILOGUE_USES (i))
      SET_REGNO_REG_SET (set, i);

  if (HAVE_epilogue && reload_completed)
    {
      /* Mark all call-saved registers that we actually used.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (regs_ever_live[i] && ! call_used_regs[i] && ! LOCAL_REGNO (i))
	  SET_REGNO_REG_SET (set, i);
    }

#ifdef EH_RETURN_DATA_REGNO
  /* Mark the registers that will contain data for the handler.  */
  if (reload_completed && current_function_calls_eh_return)
    for (i = 0; ; ++i)
      {
	unsigned regno = EH_RETURN_DATA_REGNO(i);
	if (regno == INVALID_REGNUM)
	  break;
	SET_REGNO_REG_SET (set, regno);
      }
#endif
#ifdef EH_RETURN_STACKADJ_RTX
  if ((! HAVE_epilogue || ! reload_completed)
      && current_function_calls_eh_return)
    {
      rtx tmp = EH_RETURN_STACKADJ_RTX;
      if (tmp && REG_P (tmp))
	mark_reg (tmp, set);
    }
#endif
#ifdef EH_RETURN_HANDLER_RTX
  if ((! HAVE_epilogue || ! reload_completed)
      && current_function_calls_eh_return)
    {
      rtx tmp = EH_RETURN_HANDLER_RTX;
      if (tmp && REG_P (tmp))
	mark_reg (tmp, set);
    }
#endif

  /* Mark function return value.  */
  diddle_return_value (mark_reg, set);
}

/* Callback function for for_each_successor_phi.  DATA is a regset.
   Sets the SRC_REGNO, the regno of the phi alternative for phi node
   INSN, in the regset.  */

static int
set_phi_alternative_reg (insn, dest_regno, src_regno, data)
     rtx insn ATTRIBUTE_UNUSED;
     int dest_regno ATTRIBUTE_UNUSED;
     int src_regno;
     void *data;
{
  regset live = (regset) data;
  SET_REGNO_REG_SET (live, src_regno);
  return 0;
}

/* Propagate global life info around the graph of basic blocks.  Begin
   considering blocks with their corresponding bit set in BLOCKS_IN.
   If BLOCKS_IN is null, consider it the universal set.

   BLOCKS_OUT is set for every block that was changed.  */

static void
calculate_global_regs_live (blocks_in, blocks_out, flags)
     sbitmap blocks_in, blocks_out;
     int flags;
{
  basic_block *queue, *qhead, *qtail, *qend;
  regset tmp, new_live_at_end, call_used;
  regset_head tmp_head, call_used_head;
  regset_head new_live_at_end_head;
  int i;

  tmp = INITIALIZE_REG_SET (tmp_head);
  new_live_at_end = INITIALIZE_REG_SET (new_live_at_end_head);
  call_used = INITIALIZE_REG_SET (call_used_head);

  /* Inconveniently, this is only redily available in hard reg set form.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    if (call_used_regs[i])
      SET_REGNO_REG_SET (call_used, i);

  /* Create a worklist.  Allocate an extra slot for ENTRY_BLOCK, and one
     because the `head == tail' style test for an empty queue doesn't
     work with a full queue.  */
  queue = (basic_block *) xmalloc ((n_basic_blocks + 2) * sizeof (*queue));
  qtail = queue;
  qhead = qend = queue + n_basic_blocks + 2;

  /* Queue the blocks set in the initial mask.  Do this in reverse block
     number order so that we are more likely for the first round to do
     useful work.  We use AUX non-null to flag that the block is queued.  */
  if (blocks_in)
    {
      /* Clear out the garbage that might be hanging out in bb->aux.  */
      for (i = n_basic_blocks - 1; i >= 0; --i)
	BASIC_BLOCK (i)->aux = NULL;

      EXECUTE_IF_SET_IN_SBITMAP (blocks_in, 0, i,
	{
	  basic_block bb = BASIC_BLOCK (i);
	  *--qhead = bb;
	  bb->aux = bb;
	});
    }
  else
    {
      for (i = 0; i < n_basic_blocks; ++i)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  *--qhead = bb;
	  bb->aux = bb;
	}
    }

  if (blocks_out)
    sbitmap_zero (blocks_out);

  /* We work through the queue until there are no more blocks.  What
     is live at the end of this block is precisely the union of what
     is live at the beginning of all its successors.  So, we set its
     GLOBAL_LIVE_AT_END field based on the GLOBAL_LIVE_AT_START field
     for its successors.  Then, we compute GLOBAL_LIVE_AT_START for
     this block by walking through the instructions in this block in
     reverse order and updating as we go.  If that changed
     GLOBAL_LIVE_AT_START, we add the predecessors of the block to the
     queue; they will now need to recalculate GLOBAL_LIVE_AT_END.

     We are guaranteed to terminate, because GLOBAL_LIVE_AT_START
     never shrinks.  If a register appears in GLOBAL_LIVE_AT_START, it
     must either be live at the end of the block, or used within the
     block.  In the latter case, it will certainly never disappear
     from GLOBAL_LIVE_AT_START.  In the former case, the register
     could go away only if it disappeared from GLOBAL_LIVE_AT_START
     for one of the successor blocks.  By induction, that cannot
     occur.  */
  while (qhead != qtail)
    {
      int rescan, changed;
      basic_block bb;
      edge e;

      bb = *qhead++;
      if (qhead == qend)
	qhead = queue;
      bb->aux = NULL;

      /* Begin by propagating live_at_start from the successor blocks.  */
      CLEAR_REG_SET (new_live_at_end);
      for (e = bb->succ; e; e = e->succ_next)
	{
	  basic_block sb = e->dest;

	  /* Call-clobbered registers die across exception and call edges.  */
	  /* ??? Abnormal call edges ignored for the moment, as this gets
	     confused by sibling call edges, which crashes reg-stack.  */
	  if (e->flags & EDGE_EH)
	    {
	      bitmap_operation (tmp, sb->global_live_at_start,
				call_used, BITMAP_AND_COMPL);
	      IOR_REG_SET (new_live_at_end, tmp);
	    }
	  else
	    IOR_REG_SET (new_live_at_end, sb->global_live_at_start);
	}

      /* The all-important stack pointer must always be live.  */
      SET_REGNO_REG_SET (new_live_at_end, STACK_POINTER_REGNUM);

      /* Before reload, there are a few registers that must be forced
	 live everywhere -- which might not already be the case for
	 blocks within infinite loops.  */
      if (! reload_completed)
	{
	  /* Any reference to any pseudo before reload is a potential
	     reference of the frame pointer.  */
	  SET_REGNO_REG_SET (new_live_at_end, FRAME_POINTER_REGNUM);

#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  /* Pseudos with argument area equivalences may require
	     reloading via the argument pointer.  */
	  if (fixed_regs[ARG_POINTER_REGNUM])
	    SET_REGNO_REG_SET (new_live_at_end, ARG_POINTER_REGNUM);
#endif

	  /* Any constant, or pseudo with constant equivalences, may
	     require reloading from memory using the pic register.  */
	  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
	      && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
	    SET_REGNO_REG_SET (new_live_at_end, PIC_OFFSET_TABLE_REGNUM);
	}

      /* Regs used in phi nodes are not included in
	 global_live_at_start, since they are live only along a
	 particular edge.  Set those regs that are live because of a
	 phi node alternative corresponding to this particular block.  */
      if (in_ssa_form)
	for_each_successor_phi (bb, &set_phi_alternative_reg,
				new_live_at_end);

      if (bb == ENTRY_BLOCK_PTR)
	{
	  COPY_REG_SET (bb->global_live_at_end, new_live_at_end);
	  continue;
	}

      /* On our first pass through this block, we'll go ahead and continue.
	 Recognize first pass by local_set NULL.  On subsequent passes, we
	 get to skip out early if live_at_end wouldn't have changed.  */

      if (bb->local_set == NULL)
	{
	  bb->local_set = OBSTACK_ALLOC_REG_SET (&flow_obstack);
	  bb->cond_local_set = OBSTACK_ALLOC_REG_SET (&flow_obstack);
	  rescan = 1;
	}
      else
	{
	  /* If any bits were removed from live_at_end, we'll have to
	     rescan the block.  This wouldn't be necessary if we had
	     precalculated local_live, however with PROP_SCAN_DEAD_CODE
	     local_live is really dependent on live_at_end.  */
	  CLEAR_REG_SET (tmp);
	  rescan = bitmap_operation (tmp, bb->global_live_at_end,
				     new_live_at_end, BITMAP_AND_COMPL);

	  if (! rescan)
	    {
	      /* If any of the registers in the new live_at_end set are
		 conditionally set in this basic block, we must rescan.
	         This is because conditional lifetimes at the end of the
		 block do not just take the live_at_end set into account,
		 but also the liveness at the start of each successor
		 block.  We can miss changes in those sets if we only
		 compare the new live_at_end against the previous one.  */
	      CLEAR_REG_SET (tmp);
	      rescan = bitmap_operation (tmp, new_live_at_end,
					 bb->cond_local_set, BITMAP_AND);
	    }

	  if (! rescan)
	    {
	      /* Find the set of changed bits.  Take this opportunity
		 to notice that this set is empty and early out.  */
	      CLEAR_REG_SET (tmp);
	      changed = bitmap_operation (tmp, bb->global_live_at_end,
					  new_live_at_end, BITMAP_XOR);
	      if (! changed)
		continue;

	      /* If any of the changed bits overlap with local_set,
		 we'll have to rescan the block.  Detect overlap by
		 the AND with ~local_set turning off bits.  */
	      rescan = bitmap_operation (tmp, tmp, bb->local_set,
					 BITMAP_AND_COMPL);
	    }
	}

      /* Let our caller know that BB changed enough to require its
	 death notes updated.  */
      if (blocks_out)
	SET_BIT (blocks_out, bb->index);

      if (! rescan)
	{
	  /* Add to live_at_start the set of all registers in
	     new_live_at_end that aren't in the old live_at_end.  */

	  bitmap_operation (tmp, new_live_at_end, bb->global_live_at_end,
			    BITMAP_AND_COMPL);
	  COPY_REG_SET (bb->global_live_at_end, new_live_at_end);

	  changed = bitmap_operation (bb->global_live_at_start,
				      bb->global_live_at_start,
				      tmp, BITMAP_IOR);
	  if (! changed)
	    continue;
	}
      else
	{
	  COPY_REG_SET (bb->global_live_at_end, new_live_at_end);

	  /* Rescan the block insn by insn to turn (a copy of) live_at_end
	     into live_at_start.  */
	  propagate_block (bb, new_live_at_end, bb->local_set,
			   bb->cond_local_set, flags);

	  /* If live_at start didn't change, no need to go farther.  */
	  if (REG_SET_EQUAL_P (bb->global_live_at_start, new_live_at_end))
	    continue;

	  COPY_REG_SET (bb->global_live_at_start, new_live_at_end);
	}

      /* Queue all predecessors of BB so that we may re-examine
	 their live_at_end.  */
      for (e = bb->pred; e; e = e->pred_next)
	{
	  basic_block pb = e->src;
	  if (pb->aux == NULL)
	    {
	      *qtail++ = pb;
	      if (qtail == qend)
		qtail = queue;
	      pb->aux = pb;
	    }
	}
    }

  FREE_REG_SET (tmp);
  FREE_REG_SET (new_live_at_end);
  FREE_REG_SET (call_used);

  if (blocks_out)
    {
      EXECUTE_IF_SET_IN_SBITMAP (blocks_out, 0, i,
	{
	  basic_block bb = BASIC_BLOCK (i);
	  FREE_REG_SET (bb->local_set);
	  FREE_REG_SET (bb->cond_local_set);
	});
    }
  else
    {
      for (i = n_basic_blocks - 1; i >= 0; --i)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  FREE_REG_SET (bb->local_set);
	  FREE_REG_SET (bb->cond_local_set);
	}
    }

  free (queue);
}

/* Subroutines of life analysis.  */

/* Allocate the permanent data structures that represent the results
   of life analysis.  Not static since used also for stupid life analysis.  */

void
allocate_bb_life_data ()
{
  register int i;

  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block bb = BASIC_BLOCK (i);

      bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (&flow_obstack);
      bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (&flow_obstack);
    }

  ENTRY_BLOCK_PTR->global_live_at_end
    = OBSTACK_ALLOC_REG_SET (&flow_obstack);
  EXIT_BLOCK_PTR->global_live_at_start
    = OBSTACK_ALLOC_REG_SET (&flow_obstack);

  regs_live_at_setjmp = OBSTACK_ALLOC_REG_SET (&flow_obstack);
}

void
allocate_reg_life_data ()
{
  int i;

  max_regno = max_reg_num ();

  /* Recalculate the register space, in case it has grown.  Old style
     vector oriented regsets would set regset_{size,bytes} here also.  */
  allocate_reg_info (max_regno, FALSE, FALSE);

  /* Reset all the data we'll collect in propagate_block and its
     subroutines.  */
  for (i = 0; i < max_regno; i++)
    {
      REG_N_SETS (i) = 0;
      REG_N_REFS (i) = 0;
      REG_N_DEATHS (i) = 0;
      REG_N_CALLS_CROSSED (i) = 0;
      REG_LIVE_LENGTH (i) = 0;
      REG_BASIC_BLOCK (i) = REG_BLOCK_UNKNOWN;
    }
}

/* Delete dead instructions for propagate_block.  */

static void
propagate_block_delete_insn (bb, insn)
     basic_block bb;
     rtx insn;
{
  rtx inote = find_reg_note (insn, REG_LABEL, NULL_RTX);

  /* If the insn referred to a label, and that label was attached to
     an ADDR_VEC, it's safe to delete the ADDR_VEC.  In fact, it's
     pretty much mandatory to delete it, because the ADDR_VEC may be
     referencing labels that no longer exist.

     INSN may reference a deleted label, particularly when a jump
     table has been optimized into a direct jump.  There's no
     real good way to fix up the reference to the deleted label
     when the label is deleted, so we just allow it here.

     After dead code elimination is complete, we do search for
     any REG_LABEL notes which reference deleted labels as a
     sanity check.  */

  if (inote && GET_CODE (inote) == CODE_LABEL)
    {
      rtx label = XEXP (inote, 0);
      rtx next;

      /* The label may be forced if it has been put in the constant
	 pool.  If that is the only use we must discard the table
	 jump following it, but not the label itself.  */
      if (LABEL_NUSES (label) == 1 + LABEL_PRESERVE_P (label)
	  && (next = next_nonnote_insn (label)) != NULL
	  && GET_CODE (next) == JUMP_INSN
	  && (GET_CODE (PATTERN (next)) == ADDR_VEC
	      || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
	{
	  rtx pat = PATTERN (next);
	  int diff_vec_p = GET_CODE (pat) == ADDR_DIFF_VEC;
	  int len = XVECLEN (pat, diff_vec_p);
	  int i;

	  for (i = 0; i < len; i++)
	    LABEL_NUSES (XEXP (XVECEXP (pat, diff_vec_p, i), 0))--;

	  flow_delete_insn (next);
	}
    }

  if (bb->end == insn)
    bb->end = PREV_INSN (insn);
  flow_delete_insn (insn);
}

/* Delete dead libcalls for propagate_block.  Return the insn
   before the libcall.  */

static rtx
propagate_block_delete_libcall (bb, insn, note)
     basic_block bb;
     rtx insn, note;
{
  rtx first = XEXP (note, 0);
  rtx before = PREV_INSN (first);

  if (insn == bb->end)
    bb->end = before;

  flow_delete_insn_chain (first, insn);
  return before;
}

/* Update the life-status of regs for one insn.  Return the previous insn.  */

rtx
propagate_one_insn (pbi, insn)
     struct propagate_block_info *pbi;
     rtx insn;
{
  rtx prev = PREV_INSN (insn);
  int flags = pbi->flags;
  int insn_is_dead = 0;
  int libcall_is_dead = 0;
  rtx note;
  int i;

  if (! INSN_P (insn))
    return prev;

  note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
  if (flags & PROP_SCAN_DEAD_CODE)
    {
      insn_is_dead = insn_dead_p (pbi, PATTERN (insn), 0, REG_NOTES (insn));
      libcall_is_dead = (insn_is_dead && note != 0
			 && libcall_dead_p (pbi, note, insn));
    }

  /* If an instruction consists of just dead store(s) on final pass,
     delete it.  */
  if ((flags & PROP_KILL_DEAD_CODE) && insn_is_dead)
    {
      /* If we're trying to delete a prologue or epilogue instruction
	 that isn't flagged as possibly being dead, something is wrong.
	 But if we are keeping the stack pointer depressed, we might well
	 be deleting insns that are used to compute the amount to update
	 it by, so they are fine.  */
      if (reload_completed
	  && !(TREE_CODE (TREE_TYPE (current_function_decl)) == FUNCTION_TYPE
		&& (TYPE_RETURNS_STACK_DEPRESSED
		    (TREE_TYPE (current_function_decl))))
	  && (((HAVE_epilogue || HAVE_prologue)
	       && prologue_epilogue_contains (insn))
	      || (HAVE_sibcall_epilogue
		  && sibcall_epilogue_contains (insn)))
	  && find_reg_note (insn, REG_MAYBE_DEAD, NULL_RTX) == 0)
	abort ();

      /* Record sets.  Do this even for dead instructions, since they
	 would have killed the values if they hadn't been deleted.  */
      mark_set_regs (pbi, PATTERN (insn), insn);

      /* CC0 is now known to be dead.  Either this insn used it,
	 in which case it doesn't anymore, or clobbered it,
	 so the next insn can't use it.  */
      pbi->cc0_live = 0;

      if (libcall_is_dead)
	prev = propagate_block_delete_libcall (pbi->bb, insn, note);
      else
	propagate_block_delete_insn (pbi->bb, insn);

      return prev;
    }

  /* See if this is an increment or decrement that can be merged into
     a following memory address.  */
#ifdef AUTO_INC_DEC
  {
    register rtx x = single_set (insn);

    /* Does this instruction increment or decrement a register?  */
    if ((flags & PROP_AUTOINC)
	&& x != 0
	&& GET_CODE (SET_DEST (x)) == REG
	&& (GET_CODE (SET_SRC (x)) == PLUS
	    || GET_CODE (SET_SRC (x)) == MINUS)
	&& XEXP (SET_SRC (x), 0) == SET_DEST (x)
	&& GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
	/* Ok, look for a following memory ref we can combine with.
	   If one is found, change the memory ref to a PRE_INC
	   or PRE_DEC, cancel this insn, and return 1.
	   Return 0 if nothing has been done.  */
	&& try_pre_increment_1 (pbi, insn))
      return prev;
  }
#endif /* AUTO_INC_DEC */

  CLEAR_REG_SET (pbi->new_set);

  /* If this is not the final pass, and this insn is copying the value of
     a library call and it's dead, don't scan the insns that perform the
     library call, so that the call's arguments are not marked live.  */
  if (libcall_is_dead)
    {
      /* Record the death of the dest reg.  */
      mark_set_regs (pbi, PATTERN (insn), insn);

      insn = XEXP (note, 0);
      return PREV_INSN (insn);
    }
  else if (GET_CODE (PATTERN (insn)) == SET
	   && SET_DEST (PATTERN (insn)) == stack_pointer_rtx
	   && GET_CODE (SET_SRC (PATTERN (insn))) == PLUS
	   && XEXP (SET_SRC (PATTERN (insn)), 0) == stack_pointer_rtx
	   && GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 1)) == CONST_INT)
    /* We have an insn to pop a constant amount off the stack.
       (Such insns use PLUS regardless of the direction of the stack,
       and any insn to adjust the stack by a constant is always a pop.)
       These insns, if not dead stores, have no effect on life.  */
    ;
  else
    {
      /* Any regs live at the time of a call instruction must not go
	 in a register clobbered by calls.  Find all regs now live and
	 record this for them.  */

      if (GET_CODE (insn) == CALL_INSN && (flags & PROP_REG_INFO))
	EXECUTE_IF_SET_IN_REG_SET (pbi->reg_live, 0, i,
				   { REG_N_CALLS_CROSSED (i)++; });

      /* Record sets.  Do this even for dead instructions, since they
	 would have killed the values if they hadn't been deleted.  */
      mark_set_regs (pbi, PATTERN (insn), insn);

      if (GET_CODE (insn) == CALL_INSN)
	{
	  register int i;
	  rtx note, cond;

	  cond = NULL_RTX;
	  if (GET_CODE (PATTERN (insn)) == COND_EXEC)
	    cond = COND_EXEC_TEST (PATTERN (insn));

	  /* Non-constant calls clobber memory.  */
	  if (! CONST_OR_PURE_CALL_P (insn))
	    {
	      free_EXPR_LIST_list (&pbi->mem_set_list);
	      pbi->mem_set_list_len = 0;
	    }

	  /* There may be extra registers to be clobbered.  */
	  for (note = CALL_INSN_FUNCTION_USAGE (insn);
	       note;
	       note = XEXP (note, 1))
	    if (GET_CODE (XEXP (note, 0)) == CLOBBER)
	      mark_set_1 (pbi, CLOBBER, XEXP (XEXP (note, 0), 0),
			  cond, insn, pbi->flags);

	  /* Calls change all call-used and global registers.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (TEST_HARD_REG_BIT (regs_invalidated_by_call, i))
	      {
		/* We do not want REG_UNUSED notes for these registers.  */
		mark_set_1 (pbi, CLOBBER, gen_rtx_REG (reg_raw_mode[i], i),
			    cond, insn,
			    pbi->flags & ~(PROP_DEATH_NOTES | PROP_REG_INFO));
	      }
	}

      /* If an insn doesn't use CC0, it becomes dead since we assume
	 that every insn clobbers it.  So show it dead here;
	 mark_used_regs will set it live if it is referenced.  */
      pbi->cc0_live = 0;

      /* Record uses.  */
      if (! insn_is_dead)
	mark_used_regs (pbi, PATTERN (insn), NULL_RTX, insn);

      /* Sometimes we may have inserted something before INSN (such as a move)
	 when we make an auto-inc.  So ensure we will scan those insns.  */
#ifdef AUTO_INC_DEC
      prev = PREV_INSN (insn);
#endif

      if (! insn_is_dead && GET_CODE (insn) == CALL_INSN)
	{
	  register int i;
	  rtx note, cond;

	  cond = NULL_RTX;
	  if (GET_CODE (PATTERN (insn)) == COND_EXEC)
	    cond = COND_EXEC_TEST (PATTERN (insn));

	  /* Calls use their arguments.  */
	  for (note = CALL_INSN_FUNCTION_USAGE (insn);
	       note;
	       note = XEXP (note, 1))
	    if (GET_CODE (XEXP (note, 0)) == USE)
	      mark_used_regs (pbi, XEXP (XEXP (note, 0), 0),
			      cond, insn);

	  /* The stack ptr is used (honorarily) by a CALL insn.  */
	  SET_REGNO_REG_SET (pbi->reg_live, STACK_POINTER_REGNUM);

	  /* Calls may also reference any of the global registers,
	     so they are made live.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (global_regs[i])
	      mark_used_reg (pbi, gen_rtx_REG (reg_raw_mode[i], i),
			     cond, insn);
	}
    }

  /* On final pass, update counts of how many insns in which each reg
     is live.  */
  if (flags & PROP_REG_INFO)
    EXECUTE_IF_SET_IN_REG_SET (pbi->reg_live, 0, i,
			       { REG_LIVE_LENGTH (i)++; });

  return prev;
}

/* Initialize a propagate_block_info struct for public consumption.
   Note that the structure itself is opaque to this file, but that
   the user can use the regsets provided here.  */

struct propagate_block_info *
init_propagate_block_info (bb, live, local_set, cond_local_set, flags)
     basic_block bb;
     regset live, local_set, cond_local_set;
     int flags;
{
  struct propagate_block_info *pbi = xmalloc (sizeof (*pbi));

  pbi->bb = bb;
  pbi->reg_live = live;
  pbi->mem_set_list = NULL_RTX;
  pbi->mem_set_list_len = 0;
  pbi->local_set = local_set;
  pbi->cond_local_set = cond_local_set;
  pbi->cc0_live = 0;
  pbi->flags = flags;

  if (flags & (PROP_LOG_LINKS | PROP_AUTOINC))
    pbi->reg_next_use = (rtx *) xcalloc (max_reg_num (), sizeof (rtx));
  else
    pbi->reg_next_use = NULL;

  pbi->new_set = BITMAP_XMALLOC ();

#ifdef HAVE_conditional_execution
  pbi->reg_cond_dead = splay_tree_new (splay_tree_compare_ints, NULL,
				       free_reg_cond_life_info);
  pbi->reg_cond_reg = BITMAP_XMALLOC ();

  /* If this block ends in a conditional branch, for each register live
     from one side of the branch and not the other, record the register
     as conditionally dead.  */
  if (GET_CODE (bb->end) == JUMP_INSN
      && any_condjump_p (bb->end))
    {
      regset_head diff_head;
      regset diff = INITIALIZE_REG_SET (diff_head);
      basic_block bb_true, bb_false;
      rtx cond_true, cond_false, set_src;
      int i;

      /* Identify the successor blocks.  */
      bb_true = bb->succ->dest;
      if (bb->succ->succ_next != NULL)
	{
	  bb_false = bb->succ->succ_next->dest;

	  if (bb->succ->flags & EDGE_FALLTHRU)
	    {
	      basic_block t = bb_false;
	      bb_false = bb_true;
	      bb_true = t;
	    }
	  else if (! (bb->succ->succ_next->flags & EDGE_FALLTHRU))
	    abort ();
	}
      else
	{
	  /* This can happen with a conditional jump to the next insn.  */
	  if (JUMP_LABEL (bb->end) != bb_true->head)
	    abort ();

	  /* Simplest way to do nothing.  */
	  bb_false = bb_true;
	}

      /* Extract the condition from the branch.  */
      set_src = SET_SRC (pc_set (bb->end));
      cond_true = XEXP (set_src, 0);
      cond_false = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond_true)),
				   GET_MODE (cond_true), XEXP (cond_true, 0),
				   XEXP (cond_true, 1));
      if (GET_CODE (XEXP (set_src, 1)) == PC)
	{
	  rtx t = cond_false;
	  cond_false = cond_true;
	  cond_true = t;
	}

      /* Compute which register lead different lives in the successors.  */
      if (bitmap_operation (diff, bb_true->global_live_at_start,
			    bb_false->global_live_at_start, BITMAP_XOR))
	{
	  rtx reg = XEXP (cond_true, 0);

	  if (GET_CODE (reg) == SUBREG)
	    reg = SUBREG_REG (reg);

	  if (GET_CODE (reg) != REG)
	    abort ();

	  SET_REGNO_REG_SET (pbi->reg_cond_reg, REGNO (reg));

	  /* For each such register, mark it conditionally dead.  */
	  EXECUTE_IF_SET_IN_REG_SET
	    (diff, 0, i,
	     {
	       struct reg_cond_life_info *rcli;
	       rtx cond;

	       rcli = (struct reg_cond_life_info *) xmalloc (sizeof (*rcli));

	       if (REGNO_REG_SET_P (bb_true->global_live_at_start, i))
		 cond = cond_false;
	       else
		 cond = cond_true;
	       rcli->condition = cond;
	       rcli->stores = const0_rtx;
	       rcli->orig_condition = cond;

	       splay_tree_insert (pbi->reg_cond_dead, i,
				  (splay_tree_value) rcli);
	     });
	}

      FREE_REG_SET (diff);
    }
#endif

  /* If this block has no successors, any stores to the frame that aren't
     used later in the block are dead.  So make a pass over the block
     recording any such that are made and show them dead at the end.  We do
     a very conservative and simple job here.  */
  if (optimize
      && ! (TREE_CODE (TREE_TYPE (current_function_decl)) == FUNCTION_TYPE
	    && (TYPE_RETURNS_STACK_DEPRESSED
		(TREE_TYPE (current_function_decl))))
      && (flags & PROP_SCAN_DEAD_CODE)
      && (bb->succ == NULL
	  || (bb->succ->succ_next == NULL
	      && bb->succ->dest == EXIT_BLOCK_PTR
	      && ! current_function_calls_eh_return)))
    {
      rtx insn, set;
      for (insn = bb->end; insn != bb->head; insn = PREV_INSN (insn))
	if (GET_CODE (insn) == INSN
	    && (set = single_set (insn))
	    && GET_CODE (SET_DEST (set)) == MEM)
	  {
	    rtx mem = SET_DEST (set);
	    rtx canon_mem = canon_rtx (mem);

	    /* This optimization is performed by faking a store to the
	       memory at the end of the block.  This doesn't work for
	       unchanging memories because multiple stores to unchanging
	       memory is illegal and alias analysis doesn't consider it.  */
	    if (RTX_UNCHANGING_P (canon_mem))
	      continue;

	    if (XEXP (canon_mem, 0) == frame_pointer_rtx
		|| (GET_CODE (XEXP (canon_mem, 0)) == PLUS
		    && XEXP (XEXP (canon_mem, 0), 0) == frame_pointer_rtx
		    && GET_CODE (XEXP (XEXP (canon_mem, 0), 1)) == CONST_INT))
	      add_to_mem_set_list (pbi, canon_mem);
	  }
    }

  return pbi;
}

/* Release a propagate_block_info struct.  */

void
free_propagate_block_info (pbi)
     struct propagate_block_info *pbi;
{
  free_EXPR_LIST_list (&pbi->mem_set_list);

  BITMAP_XFREE (pbi->new_set);

#ifdef HAVE_conditional_execution
  splay_tree_delete (pbi->reg_cond_dead);
  BITMAP_XFREE (pbi->reg_cond_reg);
#endif

  if (pbi->reg_next_use)
    free (pbi->reg_next_use);

  free (pbi);
}

/* Compute the registers live at the beginning of a basic block BB from
   those live at the end.

   When called, REG_LIVE contains those live at the end.  On return, it
   contains those live at the beginning.

   LOCAL_SET, if non-null, will be set with all registers killed
   unconditionally by this basic block.
   Likewise, COND_LOCAL_SET, if non-null, will be set with all registers
   killed conditionally by this basic block.  If there is any unconditional
   set of a register, then the corresponding bit will be set in LOCAL_SET
   and cleared in COND_LOCAL_SET.
   It is valid for LOCAL_SET and COND_LOCAL_SET to be the same set.  In this
   case, the resulting set will be equal to the union of the two sets that
   would otherwise be computed.

   Return non-zero if an INSN is deleted (i.e. by dead code removal).  */

int
propagate_block (bb, live, local_set, cond_local_set, flags)
     basic_block bb;
     regset live;
     regset local_set;
     regset cond_local_set;
     int flags;
{
  struct propagate_block_info *pbi;
  rtx insn, prev;
  int changed;

  pbi = init_propagate_block_info (bb, live, local_set, cond_local_set, flags);

  if (flags & PROP_REG_INFO)
    {
      register int i;

      /* Process the regs live at the end of the block.
	 Mark them as not local to any one basic block.  */
      EXECUTE_IF_SET_IN_REG_SET (live, 0, i,
				 { REG_BASIC_BLOCK (i) = REG_BLOCK_GLOBAL; });
    }

  /* Scan the block an insn at a time from end to beginning.  */

  changed = 0;
  for (insn = bb->end;; insn = prev)
    {
      /* If this is a call to `setjmp' et al, warn if any
	 non-volatile datum is live.  */
      if ((flags & PROP_REG_INFO)
	  && GET_CODE (insn) == CALL_INSN
	  && find_reg_note (insn, REG_SETJMP, NULL))
	IOR_REG_SET (regs_live_at_setjmp, pbi->reg_live);

      prev = propagate_one_insn (pbi, insn);
      changed |= NEXT_INSN (prev) != insn;

      if (insn == bb->head)
	break;
    }

  free_propagate_block_info (pbi);

  return changed;
}

/* Return 1 if X (the body of an insn, or part of it) is just dead stores
   (SET expressions whose destinations are registers dead after the insn).
   NEEDED is the regset that says which regs are alive after the insn.

   Unless CALL_OK is non-zero, an insn is needed if it contains a CALL.

   If X is the entire body of an insn, NOTES contains the reg notes
   pertaining to the insn.  */

static int
insn_dead_p (pbi, x, call_ok, notes)
     struct propagate_block_info *pbi;
     rtx x;
     int call_ok;
     rtx notes ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (x);

#ifdef AUTO_INC_DEC
  /* If flow is invoked after reload, we must take existing AUTO_INC
     expresions into account.  */
  if (reload_completed)
    {
      for (; notes; notes = XEXP (notes, 1))
	{
	  if (REG_NOTE_KIND (notes) == REG_INC)
	    {
	      int regno = REGNO (XEXP (notes, 0));

	      /* Don't delete insns to set global regs.  */
	      if ((regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
		  || REGNO_REG_SET_P (pbi->reg_live, regno))
		return 0;
	    }
	}
    }
#endif

  /* If setting something that's a reg or part of one,
     see if that register's altered value will be live.  */

  if (code == SET)
    {
      rtx r = SET_DEST (x);

#ifdef HAVE_cc0
      if (GET_CODE (r) == CC0)
	return ! pbi->cc0_live;
#endif

      /* A SET that is a subroutine call cannot be dead.  */
      if (GET_CODE (SET_SRC (x)) == CALL)
	{
	  if (! call_ok)
	    return 0;
	}

      /* Don't eliminate loads from volatile memory or volatile asms.  */
      else if (volatile_refs_p (SET_SRC (x)))
	return 0;

      if (GET_CODE (r) == MEM)
	{
	  rtx temp, canon_r;

	  if (MEM_VOLATILE_P (r) || GET_MODE (r) == BLKmode)
	    return 0;

	  canon_r = canon_rtx (r);

	  /* Walk the set of memory locations we are currently tracking
	     and see if one is an identical match to this memory location.
	     If so, this memory write is dead (remember, we're walking
	     backwards from the end of the block to the start).  Since
	     rtx_equal_p does not check the alias set or flags, we also
	     must have the potential for them to conflict (anti_dependence).  */
	  for (temp = pbi->mem_set_list; temp != 0; temp = XEXP (temp, 1))
	    if (anti_dependence (r, XEXP (temp, 0)))
	      {
		rtx mem = XEXP (temp, 0);

		if (rtx_equal_p (XEXP (canon_r, 0), XEXP (mem, 0))
		    && (GET_MODE_SIZE (GET_MODE (canon_r))
			<= GET_MODE_SIZE (GET_MODE (mem))))
		  return 1;

#ifdef AUTO_INC_DEC
		/* Check if memory reference matches an auto increment. Only
		   post increment/decrement or modify are valid.  */
		if (GET_MODE (mem) == GET_MODE (r)
		    && (GET_CODE (XEXP (mem, 0)) == POST_DEC
			|| GET_CODE (XEXP (mem, 0)) == POST_INC
			|| GET_CODE (XEXP (mem, 0)) == POST_MODIFY)
		    && GET_MODE (XEXP (mem, 0)) == GET_MODE (r)
		    && rtx_equal_p (XEXP (XEXP (mem, 0), 0), XEXP (r, 0)))
		  return 1;
#endif
	      }
	}
      else
	{
	  while (GET_CODE (r) == SUBREG
		 || GET_CODE (r) == STRICT_LOW_PART
		 || GET_CODE (r) == ZERO_EXTRACT)
	    r = XEXP (r, 0);

	  if (GET_CODE (r) == REG)
	    {
	      int regno = REGNO (r);

	      /* Obvious.  */
	      if (REGNO_REG_SET_P (pbi->reg_live, regno))
		return 0;

	      /* If this is a hard register, verify that subsequent
		 words are not needed.  */
	      if (regno < FIRST_PSEUDO_REGISTER)
		{
		  int n = HARD_REGNO_NREGS (regno, GET_MODE (r));

		  while (--n > 0)
		    if (REGNO_REG_SET_P (pbi->reg_live, regno+n))
		      return 0;
		}

	      /* Don't delete insns to set global regs.  */
	      if (regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
		return 0;

	      /* Make sure insns to set the stack pointer aren't deleted.  */
	      if (regno == STACK_POINTER_REGNUM)
		return 0;

	      /* ??? These bits might be redundant with the force live bits
		 in calculate_global_regs_live.  We would delete from
		 sequential sets; whether this actually affects real code
		 for anything but the stack pointer I don't know.  */
	      /* Make sure insns to set the frame pointer aren't deleted.  */
	      if (regno == FRAME_POINTER_REGNUM
		  && (! reload_completed || frame_pointer_needed))
		return 0;
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	      if (regno == HARD_FRAME_POINTER_REGNUM
		  && (! reload_completed || frame_pointer_needed))
		return 0;
#endif

#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      /* Make sure insns to set arg pointer are never deleted
		 (if the arg pointer isn't fixed, there will be a USE
		 for it, so we can treat it normally).  */
	      if (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
		return 0;
#endif

	      /* Otherwise, the set is dead.  */
	      return 1;
	    }
	}
    }

  /* If performing several activities, insn is dead if each activity
     is individually dead.  Also, CLOBBERs and USEs can be ignored; a
     CLOBBER or USE that's inside a PARALLEL doesn't make the insn
     worth keeping.  */
  else if (code == PARALLEL)
    {
      int i = XVECLEN (x, 0);

      for (i--; i >= 0; i--)
	if (GET_CODE (XVECEXP (x, 0, i)) != CLOBBER
	    && GET_CODE (XVECEXP (x, 0, i)) != USE
	    && ! insn_dead_p (pbi, XVECEXP (x, 0, i), call_ok, NULL_RTX))
	  return 0;

      return 1;
    }

  /* A CLOBBER of a pseudo-register that is dead serves no purpose.  That
     is not necessarily true for hard registers.  */
  else if (code == CLOBBER && GET_CODE (XEXP (x, 0)) == REG
	   && REGNO (XEXP (x, 0)) >= FIRST_PSEUDO_REGISTER
	   && ! REGNO_REG_SET_P (pbi->reg_live, REGNO (XEXP (x, 0))))
    return 1;

  /* We do not check other CLOBBER or USE here.  An insn consisting of just
     a CLOBBER or just a USE should not be deleted.  */
  return 0;
}

/* If INSN is the last insn in a libcall, and assuming INSN is dead,
   return 1 if the entire library call is dead.
   This is true if INSN copies a register (hard or pseudo)
   and if the hard return reg of the call insn is dead.
   (The caller should have tested the destination of the SET inside
   INSN already for death.)

   If this insn doesn't just copy a register, then we don't
   have an ordinary libcall.  In that case, cse could not have
   managed to substitute the source for the dest later on,
   so we can assume the libcall is dead.

   PBI is the block info giving pseudoregs live before this insn.
   NOTE is the REG_RETVAL note of the insn.  */

static int
libcall_dead_p (pbi, note, insn)
     struct propagate_block_info *pbi;
     rtx note;
     rtx insn;
{
  rtx x = single_set (insn);

  if (x)
    {
      register rtx r = SET_SRC (x);

      if (GET_CODE (r) == REG)
	{
	  rtx call = XEXP (note, 0);
	  rtx call_pat;
	  register int i;

	  /* Find the call insn.  */
	  while (call != insn && GET_CODE (call) != CALL_INSN)
	    call = NEXT_INSN (call);

	  /* If there is none, do nothing special,
	     since ordinary death handling can understand these insns.  */
	  if (call == insn)
	    return 0;

	  /* See if the hard reg holding the value is dead.
	     If this is a PARALLEL, find the call within it.  */
	  call_pat = PATTERN (call);
	  if (GET_CODE (call_pat) == PARALLEL)
	    {
	      for (i = XVECLEN (call_pat, 0) - 1; i >= 0; i--)
		if (GET_CODE (XVECEXP (call_pat, 0, i)) == SET
		    && GET_CODE (SET_SRC (XVECEXP (call_pat, 0, i))) == CALL)
		  break;

	      /* This may be a library call that is returning a value
		 via invisible pointer.  Do nothing special, since
		 ordinary death handling can understand these insns.  */
	      if (i < 0)
		return 0;

	      call_pat = XVECEXP (call_pat, 0, i);
	    }

	  return insn_dead_p (pbi, call_pat, 1, REG_NOTES (call));
	}
    }
  return 1;
}

/* Return 1 if register REGNO was used before it was set, i.e. if it is
   live at function entry.  Don't count global register variables, variables
   in registers that can be used for function arg passing, or variables in
   fixed hard registers.  */

int
regno_uninitialized (regno)
     int regno;
{
  if (n_basic_blocks == 0
      || (regno < FIRST_PSEUDO_REGISTER
	  && (global_regs[regno]
	      || fixed_regs[regno]
	      || FUNCTION_ARG_REGNO_P (regno))))
    return 0;

  return REGNO_REG_SET_P (BASIC_BLOCK (0)->global_live_at_start, regno);
}

/* 1 if register REGNO was alive at a place where `setjmp' was called
   and was set more than once or is an argument.
   Such regs may be clobbered by `longjmp'.  */

int
regno_clobbered_at_setjmp (regno)
     int regno;
{
  if (n_basic_blocks == 0)
    return 0;

  return ((REG_N_SETS (regno) > 1
	   || REGNO_REG_SET_P (BASIC_BLOCK (0)->global_live_at_start, regno))
	  && REGNO_REG_SET_P (regs_live_at_setjmp, regno));
}

/* Add MEM to PBI->MEM_SET_LIST.  MEM should be canonical.  Respect the
   maximal list size; look for overlaps in mode and select the largest.  */
static void
add_to_mem_set_list (pbi, mem)
     struct propagate_block_info *pbi;
     rtx mem;
{
  rtx i;

  /* We don't know how large a BLKmode store is, so we must not
     take them into consideration.  */
  if (GET_MODE (mem) == BLKmode)
    return;

  for (i = pbi->mem_set_list; i ; i = XEXP (i, 1))
    {
      rtx e = XEXP (i, 0);
      if (rtx_equal_p (XEXP (mem, 0), XEXP (e, 0)))
	{
	  if (GET_MODE_SIZE (GET_MODE (mem)) > GET_MODE_SIZE (GET_MODE (e)))
	    {
#ifdef AUTO_INC_DEC
	      /* If we must store a copy of the mem, we can just modify
		 the mode of the stored copy.  */
	      if (pbi->flags & PROP_AUTOINC)
	        PUT_MODE (e, GET_MODE (mem));
	      else
#endif
	        XEXP (i, 0) = mem;
	    }
	  return;
	}
    }

  if (pbi->mem_set_list_len < MAX_MEM_SET_LIST_LEN)
    {
#ifdef AUTO_INC_DEC
      /* Store a copy of mem, otherwise the address may be
	 scrogged by find_auto_inc.  */
      if (pbi->flags & PROP_AUTOINC)
	mem = shallow_copy_rtx (mem);
#endif
      pbi->mem_set_list = alloc_EXPR_LIST (0, mem, pbi->mem_set_list);
      pbi->mem_set_list_len++;
    }
}

/* INSN references memory, possibly using autoincrement addressing modes.
   Find any entries on the mem_set_list that need to be invalidated due
   to an address change.  */

static void
invalidate_mems_from_autoinc (pbi, insn)
     struct propagate_block_info *pbi;
     rtx insn;
{
  rtx note = REG_NOTES (insn);
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_INC)
      invalidate_mems_from_set (pbi, XEXP (note, 0));
}

/* EXP is a REG.  Remove any dependant entries from pbi->mem_set_list.  */

static void
invalidate_mems_from_set (pbi, exp)
     struct propagate_block_info *pbi;
     rtx exp;
{
  rtx temp = pbi->mem_set_list;
  rtx prev = NULL_RTX;
  rtx next;

  while (temp)
    {
      next = XEXP (temp, 1);
      if (reg_overlap_mentioned_p (exp, XEXP (temp, 0)))
	{
	  /* Splice this entry out of the list.  */
	  if (prev)
	    XEXP (prev, 1) = next;
	  else
	    pbi->mem_set_list = next;
	  free_EXPR_LIST_node (temp);
	  pbi->mem_set_list_len--;
	}
      else
	prev = temp;
      temp = next;
    }
}

/* Process the registers that are set within X.  Their bits are set to
   1 in the regset DEAD, because they are dead prior to this insn.

   If INSN is nonzero, it is the insn being processed.

   FLAGS is the set of operations to perform.  */

static void
mark_set_regs (pbi, x, insn)
     struct propagate_block_info *pbi;
     rtx x, insn;
{
  rtx cond = NULL_RTX;
  rtx link;
  enum rtx_code code;

  if (insn)
    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
      {
	if (REG_NOTE_KIND (link) == REG_INC)
	  mark_set_1 (pbi, SET, XEXP (link, 0),
		      (GET_CODE (x) == COND_EXEC
		       ? COND_EXEC_TEST (x) : NULL_RTX),
		      insn, pbi->flags);
      }
 retry:
  switch (code = GET_CODE (x))
    {
    case SET:
    case CLOBBER:
      mark_set_1 (pbi, code, SET_DEST (x), cond, insn, pbi->flags);
      return;

    case COND_EXEC:
      cond = COND_EXEC_TEST (x);
      x = COND_EXEC_CODE (x);
      goto retry;

    case PARALLEL:
      {
	register int i;
	for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	  {
	    rtx sub = XVECEXP (x, 0, i);
	    switch (code = GET_CODE (sub))
	      {
	      case COND_EXEC:
		if (cond != NULL_RTX)
		  abort ();

		cond = COND_EXEC_TEST (sub);
		sub = COND_EXEC_CODE (sub);
		if (GET_CODE (sub) != SET && GET_CODE (sub) != CLOBBER)
		  break;
		/* Fall through.  */

	      case SET:
	      case CLOBBER:
		mark_set_1 (pbi, code, SET_DEST (sub), cond, insn, pbi->flags);
		break;

	      default:
		break;
	      }
	  }
	break;
      }

    default:
      break;
    }
}

/* Process a single set, which appears in INSN.  REG (which may not
   actually be a REG, it may also be a SUBREG, PARALLEL, etc.) is
   being set using the CODE (which may be SET, CLOBBER, or COND_EXEC).
   If the set is conditional (because it appear in a COND_EXEC), COND
   will be the condition.  */

static void
mark_set_1 (pbi, code, reg, cond, insn, flags)
     struct propagate_block_info *pbi;
     enum rtx_code code;
     rtx reg, cond, insn;
     int flags;
{
  int regno_first = -1, regno_last = -1;
  unsigned long not_dead = 0;
  int i;

  /* Modifying just one hardware register of a multi-reg value or just a
     byte field of a register does not mean the value from before this insn
     is now dead.  Of course, if it was dead after it's unused now.  */

  switch (GET_CODE (reg))
    {
    case PARALLEL:
      /* Some targets place small structures in registers for return values of
	 functions.  We have to detect this case specially here to get correct
	 flow information.  */
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	if (XEXP (XVECEXP (reg, 0, i), 0) != 0)
	  mark_set_1 (pbi, code, XEXP (XVECEXP (reg, 0, i), 0), cond, insn,
		      flags);
      return;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
    case STRICT_LOW_PART:
      /* ??? Assumes STRICT_LOW_PART not used on multi-word registers.  */
      do
	reg = XEXP (reg, 0);
      while (GET_CODE (reg) == SUBREG
	     || GET_CODE (reg) == ZERO_EXTRACT
	     || GET_CODE (reg) == SIGN_EXTRACT
	     || GET_CODE (reg) == STRICT_LOW_PART);
      if (GET_CODE (reg) == MEM)
	break;
      not_dead = (unsigned long) REGNO_REG_SET_P (pbi->reg_live, REGNO (reg));
      /* Fall through.  */

    case REG:
      regno_last = regno_first = REGNO (reg);
      if (regno_first < FIRST_PSEUDO_REGISTER)
	regno_last += HARD_REGNO_NREGS (regno_first, GET_MODE (reg)) - 1;
      break;

    case SUBREG:
      if (GET_CODE (SUBREG_REG (reg)) == REG)
	{
	  enum machine_mode outer_mode = GET_MODE (reg);
	  enum machine_mode inner_mode = GET_MODE (SUBREG_REG (reg));

	  /* Identify the range of registers affected.  This is moderately
	     tricky for hard registers.  See alter_subreg.  */

	  regno_last = regno_first = REGNO (SUBREG_REG (reg));
	  if (regno_first < FIRST_PSEUDO_REGISTER)
	    {
	      regno_first += subreg_regno_offset (regno_first, inner_mode,
						  SUBREG_BYTE (reg),
						  outer_mode);
	      regno_last = (regno_first
			    + HARD_REGNO_NREGS (regno_first, outer_mode) - 1);

	      /* Since we've just adjusted the register number ranges, make
		 sure REG matches.  Otherwise some_was_live will be clear
		 when it shouldn't have been, and we'll create incorrect
		 REG_UNUSED notes.  */
	      reg = gen_rtx_REG (outer_mode, regno_first);
	    }
	  else
	    {
	      /* If the number of words in the subreg is less than the number
		 of words in the full register, we have a well-defined partial
		 set.  Otherwise the high bits are undefined.

		 This is only really applicable to pseudos, since we just took
		 care of multi-word hard registers.  */
	      if (((GET_MODE_SIZE (outer_mode)
		    + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
		  < ((GET_MODE_SIZE (inner_mode)
		      + UNITS_PER_WORD - 1) / UNITS_PER_WORD))
		not_dead = (unsigned long) REGNO_REG_SET_P (pbi->reg_live,
							    regno_first);

	      reg = SUBREG_REG (reg);
	    }
	}
      else
	reg = SUBREG_REG (reg);
      break;

    default:
      break;
    }

  /* If this set is a MEM, then it kills any aliased writes.
     If this set is a REG, then it kills any MEMs which use the reg.  */
  if (optimize && (flags & PROP_SCAN_DEAD_CODE))
    {
      if (GET_CODE (reg) == REG)
	invalidate_mems_from_set (pbi, reg);

      /* If the memory reference had embedded side effects (autoincrement
	 address modes.  Then we may need to kill some entries on the
	 memory set list.  */
      if (insn && GET_CODE (reg) == MEM)
	invalidate_mems_from_autoinc (pbi, insn);

      if (GET_CODE (reg) == MEM && ! side_effects_p (reg)
	  /* ??? With more effort we could track conditional memory life.  */
	  && ! cond
	  /* There are no REG_INC notes for SP, so we can't assume we'll see
	     everything that invalidates it.  To be safe, don't eliminate any
	     stores though SP; none of them should be redundant anyway.  */
	  && ! reg_mentioned_p (stack_pointer_rtx, reg))
        add_to_mem_set_list (pbi, canon_rtx (reg));
    }

  if (GET_CODE (reg) == REG
      && ! (regno_first == FRAME_POINTER_REGNUM
	    && (! reload_completed || frame_pointer_needed))
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
      && ! (regno_first == HARD_FRAME_POINTER_REGNUM
	    && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      && ! (regno_first == ARG_POINTER_REGNUM && fixed_regs[regno_first])
#endif
      )
    {
      int some_was_live = 0, some_was_dead = 0;

      for (i = regno_first; i <= regno_last; ++i)
	{
	  int needed_regno = REGNO_REG_SET_P (pbi->reg_live, i);
	  if (pbi->local_set)
	    {
	      /* Order of the set operation matters here since both
		 sets may be the same.  */
	      CLEAR_REGNO_REG_SET (pbi->cond_local_set, i);
	      if (cond != NULL_RTX
		  && ! REGNO_REG_SET_P (pbi->local_set, i))
		SET_REGNO_REG_SET (pbi->cond_local_set, i);
	      else
		SET_REGNO_REG_SET (pbi->local_set, i);
	    }
	  if (code != CLOBBER)
	    SET_REGNO_REG_SET (pbi->new_set, i);

	  some_was_live |= needed_regno;
	  some_was_dead |= ! needed_regno;
	}

#ifdef HAVE_conditional_execution
      /* Consider conditional death in deciding that the register needs
	 a death note.  */
      if (some_was_live && ! not_dead
	  /* The stack pointer is never dead.  Well, not strictly true,
	     but it's very difficult to tell from here.  Hopefully
	     combine_stack_adjustments will fix up the most egregious
	     errors.  */
	  && regno_first != STACK_POINTER_REGNUM)
	{
	  for (i = regno_first; i <= regno_last; ++i)
	    if (! mark_regno_cond_dead (pbi, i, cond))
	      not_dead |= ((unsigned long) 1) << (i - regno_first);
	}
#endif

      /* Additional data to record if this is the final pass.  */
      if (flags & (PROP_LOG_LINKS | PROP_REG_INFO
		   | PROP_DEATH_NOTES | PROP_AUTOINC))
	{
	  register rtx y;
	  register int blocknum = pbi->bb->index;

	  y = NULL_RTX;
	  if (flags & (PROP_LOG_LINKS | PROP_AUTOINC))
	    {
	      y = pbi->reg_next_use[regno_first];

	      /* The next use is no longer next, since a store intervenes.  */
	      for (i = regno_first; i <= regno_last; ++i)
		pbi->reg_next_use[i] = 0;
	    }

	  if (flags & PROP_REG_INFO)
	    {
	      for (i = regno_first; i <= regno_last; ++i)
		{
		  /* Count (weighted) references, stores, etc.  This counts a
		     register twice if it is modified, but that is correct.  */
		  REG_N_SETS (i) += 1;
		  REG_N_REFS (i) += 1;
		  REG_FREQ (i) += REG_FREQ_FROM_BB (pbi->bb);

	          /* The insns where a reg is live are normally counted
		     elsewhere, but we want the count to include the insn
		     where the reg is set, and the normal counting mechanism
		     would not count it.  */
		  REG_LIVE_LENGTH (i) += 1;
		}

	      /* If this is a hard reg, record this function uses the reg.  */
	      if (regno_first < FIRST_PSEUDO_REGISTER)
		{
		  for (i = regno_first; i <= regno_last; i++)
		    regs_ever_live[i] = 1;
		}
	      else
		{
		  /* Keep track of which basic blocks each reg appears in.  */
		  if (REG_BASIC_BLOCK (regno_first) == REG_BLOCK_UNKNOWN)
		    REG_BASIC_BLOCK (regno_first) = blocknum;
		  else if (REG_BASIC_BLOCK (regno_first) != blocknum)
		    REG_BASIC_BLOCK (regno_first) = REG_BLOCK_GLOBAL;
		}
	    }

	  if (! some_was_dead)
	    {
	      if (flags & PROP_LOG_LINKS)
		{
		  /* Make a logical link from the next following insn
		     that uses this register, back to this insn.
		     The following insns have already been processed.

		     We don't build a LOG_LINK for hard registers containing
		     in ASM_OPERANDs.  If these registers get replaced,
		     we might wind up changing the semantics of the insn,
		     even if reload can make what appear to be valid
		     assignments later.  */
		  if (y && (BLOCK_NUM (y) == blocknum)
		      && (regno_first >= FIRST_PSEUDO_REGISTER
			  || asm_noperands (PATTERN (y)) < 0))
		    LOG_LINKS (y) = alloc_INSN_LIST (insn, LOG_LINKS (y));
		}
	    }
	  else if (not_dead)
	    ;
	  else if (! some_was_live)
	    {
	      if (flags & PROP_REG_INFO)
		REG_N_DEATHS (regno_first) += 1;

	      if (flags & PROP_DEATH_NOTES)
		{
		  /* Note that dead stores have already been deleted
		     when possible.  If we get here, we have found a
		     dead store that cannot be eliminated (because the
		     same insn does something useful).  Indicate this
		     by marking the reg being set as dying here.  */
		  REG_NOTES (insn)
		    = alloc_EXPR_LIST (REG_UNUSED, reg, REG_NOTES (insn));
		}
	    }
	  else
	    {
	      if (flags & PROP_DEATH_NOTES)
		{
		  /* This is a case where we have a multi-word hard register
		     and some, but not all, of the words of the register are
		     needed in subsequent insns.  Write REG_UNUSED notes
		     for those parts that were not needed.  This case should
		     be rare.  */

		  for (i = regno_first; i <= regno_last; ++i)
		    if (! REGNO_REG_SET_P (pbi->reg_live, i))
		      REG_NOTES (insn)
			= alloc_EXPR_LIST (REG_UNUSED,
					   gen_rtx_REG (reg_raw_mode[i], i),
					   REG_NOTES (insn));
		}
	    }
	}

      /* Mark the register as being dead.  */
      if (some_was_live
	  /* The stack pointer is never dead.  Well, not strictly true,
	     but it's very difficult to tell from here.  Hopefully
	     combine_stack_adjustments will fix up the most egregious
	     errors.  */
	  && regno_first != STACK_POINTER_REGNUM)
	{
	  for (i = regno_first; i <= regno_last; ++i)
	    if (!(not_dead & (((unsigned long) 1) << (i - regno_first))))
	      CLEAR_REGNO_REG_SET (pbi->reg_live, i);
	}
    }
  else if (GET_CODE (reg) == REG)
    {
      if (flags & (PROP_LOG_LINKS | PROP_AUTOINC))
	pbi->reg_next_use[regno_first] = 0;
    }

  /* If this is the last pass and this is a SCRATCH, show it will be dying
     here and count it.  */
  else if (GET_CODE (reg) == SCRATCH)
    {
      if (flags & PROP_DEATH_NOTES)
	REG_NOTES (insn)
	  = alloc_EXPR_LIST (REG_UNUSED, reg, REG_NOTES (insn));
    }
}

#ifdef HAVE_conditional_execution
/* Mark REGNO conditionally dead.
   Return true if the register is now unconditionally dead.  */

static int
mark_regno_cond_dead (pbi, regno, cond)
     struct propagate_block_info *pbi;
     int regno;
     rtx cond;
{
  /* If this is a store to a predicate register, the value of the
     predicate is changing, we don't know that the predicate as seen
     before is the same as that seen after.  Flush all dependent
     conditions from reg_cond_dead.  This will make all such
     conditionally live registers unconditionally live.  */
  if (REGNO_REG_SET_P (pbi->reg_cond_reg, regno))
    flush_reg_cond_reg (pbi, regno);

  /* If this is an unconditional store, remove any conditional
     life that may have existed.  */
  if (cond == NULL_RTX)
    splay_tree_remove (pbi->reg_cond_dead, regno);
  else
    {
      splay_tree_node node;
      struct reg_cond_life_info *rcli;
      rtx ncond;

      /* Otherwise this is a conditional set.  Record that fact.
	 It may have been conditionally used, or there may be a
	 subsequent set with a complimentary condition.  */

      node = splay_tree_lookup (pbi->reg_cond_dead, regno);
      if (node == NULL)
	{
	  /* The register was unconditionally live previously.
	     Record the current condition as the condition under
	     which it is dead.  */
	  rcli = (struct reg_cond_life_info *) xmalloc (sizeof (*rcli));
	  rcli->condition = cond;
	  rcli->stores = cond;
	  rcli->orig_condition = const0_rtx;
	  splay_tree_insert (pbi->reg_cond_dead, regno,
			     (splay_tree_value) rcli);

	  SET_REGNO_REG_SET (pbi->reg_cond_reg, REGNO (XEXP (cond, 0)));

	  /* Not unconditionaly dead.  */
	  return 0;
	}
      else
	{
	  /* The register was conditionally live previously.
	     Add the new condition to the old.  */
	  rcli = (struct reg_cond_life_info *) node->value;
	  ncond = rcli->condition;
	  ncond = ior_reg_cond (ncond, cond, 1);
	  if (rcli->stores == const0_rtx)
	    rcli->stores = cond;
	  else if (rcli->stores != const1_rtx)
	    rcli->stores = ior_reg_cond (rcli->stores, cond, 1);

	  /* If the register is now unconditionally dead, remove the entry
	     in the splay_tree.  A register is unconditionally dead if the
	     dead condition ncond is true.  A register is also unconditionally
	     dead if the sum of all conditional stores is an unconditional
	     store (stores is true), and the dead condition is identically the
	     same as the original dead condition initialized at the end of
	     the block.  This is a pointer compare, not an rtx_equal_p
	     compare.  */
	  if (ncond == const1_rtx
	      || (ncond == rcli->orig_condition && rcli->stores == const1_rtx))
	    splay_tree_remove (pbi->reg_cond_dead, regno);
	  else
	    {
	      rcli->condition = ncond;

	      SET_REGNO_REG_SET (pbi->reg_cond_reg, REGNO (XEXP (cond, 0)));

	      /* Not unconditionaly dead.  */
	      return 0;
	    }
	}
    }

  return 1;
}

/* Called from splay_tree_delete for pbi->reg_cond_life.  */

static void
free_reg_cond_life_info (value)
     splay_tree_value value;
{
  struct reg_cond_life_info *rcli = (struct reg_cond_life_info *) value;
  free (rcli);
}

/* Helper function for flush_reg_cond_reg.  */

static int
flush_reg_cond_reg_1 (node, data)
     splay_tree_node node;
     void *data;
{
  struct reg_cond_life_info *rcli;
  int *xdata = (int *) data;
  unsigned int regno = xdata[0];

  /* Don't need to search if last flushed value was farther on in
     the in-order traversal.  */
  if (xdata[1] >= (int) node->key)
    return 0;

  /* Splice out portions of the expression that refer to regno.  */
  rcli = (struct reg_cond_life_info *) node->value;
  rcli->condition = elim_reg_cond (rcli->condition, regno);
  if (rcli->stores != const0_rtx && rcli->stores != const1_rtx)
    rcli->stores = elim_reg_cond (rcli->stores, regno);

  /* If the entire condition is now false, signal the node to be removed.  */
  if (rcli->condition == const0_rtx)
    {
      xdata[1] = node->key;
      return -1;
    }
  else if (rcli->condition == const1_rtx)
    abort ();

  return 0;
}

/* Flush all (sub) expressions referring to REGNO from REG_COND_LIVE.  */

static void
flush_reg_cond_reg (pbi, regno)
     struct propagate_block_info *pbi;
     int regno;
{
  int pair[2];

  pair[0] = regno;
  pair[1] = -1;
  while (splay_tree_foreach (pbi->reg_cond_dead,
			     flush_reg_cond_reg_1, pair) == -1)
    splay_tree_remove (pbi->reg_cond_dead, pair[1]);

  CLEAR_REGNO_REG_SET (pbi->reg_cond_reg, regno);
}

/* Logical arithmetic on predicate conditions.  IOR, NOT and AND.
   For ior/and, the ADD flag determines whether we want to add the new
   condition X to the old one unconditionally.  If it is zero, we will
   only return a new expression if X allows us to simplify part of
   OLD, otherwise we return OLD unchanged to the caller.
   If ADD is nonzero, we will return a new condition in all cases.  The
   toplevel caller of one of these functions should always pass 1 for
   ADD.  */

static rtx
ior_reg_cond (old, x, add)
     rtx old, x;
     int add;
{
  rtx op0, op1;

  if (GET_RTX_CLASS (GET_CODE (old)) == '<')
    {
      if (GET_RTX_CLASS (GET_CODE (x)) == '<'
	  && REVERSE_CONDEXEC_PREDICATES_P (GET_CODE (x), GET_CODE (old))
	  && REGNO (XEXP (x, 0)) == REGNO (XEXP (old, 0)))
	return const1_rtx;
      if (GET_CODE (x) == GET_CODE (old)
	  && REGNO (XEXP (x, 0)) == REGNO (XEXP (old, 0)))
	return old;
      if (! add)
	return old;
      return gen_rtx_IOR (0, old, x);
    }

  switch (GET_CODE (old))
    {
    case IOR:
      op0 = ior_reg_cond (XEXP (old, 0), x, 0);
      op1 = ior_reg_cond (XEXP (old, 1), x, 0);
      if (op0 != XEXP (old, 0) || op1 != XEXP (old, 1))
	{
	  if (op0 == const0_rtx)
	    return op1;
	  if (op1 == const0_rtx)
	    return op0;
	  if (op0 == const1_rtx || op1 == const1_rtx)
	    return const1_rtx;
	  if (op0 == XEXP (old, 0))
	    op0 = gen_rtx_IOR (0, op0, x);
	  else
	    op1 = gen_rtx_IOR (0, op1, x);
	  return gen_rtx_IOR (0, op0, op1);
	}
      if (! add)
	return old;
      return gen_rtx_IOR (0, old, x);

    case AND:
      op0 = ior_reg_cond (XEXP (old, 0), x, 0);
      op1 = ior_reg_cond (XEXP (old, 1), x, 0);
      if (op0 != XEXP (old, 0) || op1 != XEXP (old, 1))
	{
	  if (op0 == const1_rtx)
	    return op1;
	  if (op1 == const1_rtx)
	    return op0;
	  if (op0 == const0_rtx || op1 == const0_rtx)
	    return const0_rtx;
	  if (op0 == XEXP (old, 0))
	    op0 = gen_rtx_IOR (0, op0, x);
	  else
	    op1 = gen_rtx_IOR (0, op1, x);
	  return gen_rtx_AND (0, op0, op1);
	}
      if (! add)
	return old;
      return gen_rtx_IOR (0, old, x);

    case NOT:
      op0 = and_reg_cond (XEXP (old, 0), not_reg_cond (x), 0);
      if (op0 != XEXP (old, 0))
	return not_reg_cond (op0);
      if (! add)
	return old;
      return gen_rtx_IOR (0, old, x);

    default:
      abort ();
    }
}

static rtx
not_reg_cond (x)
     rtx x;
{
  enum rtx_code x_code;

  if (x == const0_rtx)
    return const1_rtx;
  else if (x == const1_rtx)
    return const0_rtx;
  x_code = GET_CODE (x);
  if (x_code == NOT)
    return XEXP (x, 0);
  if (GET_RTX_CLASS (x_code) == '<'
      && GET_CODE (XEXP (x, 0)) == REG)
    {
      if (XEXP (x, 1) != const0_rtx)
	abort ();

      return gen_rtx_fmt_ee (reverse_condition (x_code),
			     VOIDmode, XEXP (x, 0), const0_rtx);
    }
  return gen_rtx_NOT (0, x);
}

static rtx
and_reg_cond (old, x, add)
     rtx old, x;
     int add;
{
  rtx op0, op1;

  if (GET_RTX_CLASS (GET_CODE (old)) == '<')
    {
      if (GET_RTX_CLASS (GET_CODE (x)) == '<'
	  && GET_CODE (x) == reverse_condition (GET_CODE (old))
	  && REGNO (XEXP (x, 0)) == REGNO (XEXP (old, 0)))
	return const0_rtx;
      if (GET_CODE (x) == GET_CODE (old)
	  && REGNO (XEXP (x, 0)) == REGNO (XEXP (old, 0)))
	return old;
      if (! add)
	return old;
      return gen_rtx_AND (0, old, x);
    }

  switch (GET_CODE (old))
    {
    case IOR:
      op0 = and_reg_cond (XEXP (old, 0), x, 0);
      op1 = and_reg_cond (XEXP (old, 1), x, 0);
      if (op0 != XEXP (old, 0) || op1 != XEXP (old, 1))
	{
	  if (op0 == const0_rtx)
	    return op1;
	  if (op1 == const0_rtx)
	    return op0;
	  if (op0 == const1_rtx || op1 == const1_rtx)
	    return const1_rtx;
	  if (op0 == XEXP (old, 0))
	    op0 = gen_rtx_AND (0, op0, x);
	  else
	    op1 = gen_rtx_AND (0, op1, x);
	  return gen_rtx_IOR (0, op0, op1);
	}
      if (! add)
	return old;
      return gen_rtx_AND (0, old, x);

    case AND:
      op0 = and_reg_cond (XEXP (old, 0), x, 0);
      op1 = and_reg_cond (XEXP (old, 1), x, 0);
      if (op0 != XEXP (old, 0) || op1 != XEXP (old, 1))
	{
	  if (op0 == const1_rtx)
	    return op1;
	  if (op1 == const1_rtx)
	    return op0;
	  if (op0 == const0_rtx || op1 == const0_rtx)
	    return const0_rtx;
	  if (op0 == XEXP (old, 0))
	    op0 = gen_rtx_AND (0, op0, x);
	  else
	    op1 = gen_rtx_AND (0, op1, x);
	  return gen_rtx_AND (0, op0, op1);
	}
      if (! add)
	return old;

      /* If X is identical to one of the existing terms of the AND,
	 then just return what we already have.  */
      /* ??? There really should be some sort of recursive check here in
	 case there are nested ANDs.  */
      if ((GET_CODE (XEXP (old, 0)) == GET_CODE (x)
	   && REGNO (XEXP (XEXP (old, 0), 0)) == REGNO (XEXP (x, 0)))
	  || (GET_CODE (XEXP (old, 1)) == GET_CODE (x)
	      && REGNO (XEXP (XEXP (old, 1), 0)) == REGNO (XEXP (x, 0))))
	return old;

      return gen_rtx_AND (0, old, x);

    case NOT:
      op0 = ior_reg_cond (XEXP (old, 0), not_reg_cond (x), 0);
      if (op0 != XEXP (old, 0))
	return not_reg_cond (op0);
      if (! add)
	return old;
      return gen_rtx_AND (0, old, x);

    default:
      abort ();
    }
}

/* Given a condition X, remove references to reg REGNO and return the
   new condition.  The removal will be done so that all conditions
   involving REGNO are considered to evaluate to false.  This function
   is used when the value of REGNO changes.  */

static rtx
elim_reg_cond (x, regno)
     rtx x;
     unsigned int regno;
{
  rtx op0, op1;

  if (GET_RTX_CLASS (GET_CODE (x)) == '<')
    {
      if (REGNO (XEXP (x, 0)) == regno)
	return const0_rtx;
      return x;
    }

  switch (GET_CODE (x))
    {
    case AND:
      op0 = elim_reg_cond (XEXP (x, 0), regno);
      op1 = elim_reg_cond (XEXP (x, 1), regno);
      if (op0 == const0_rtx || op1 == const0_rtx)
	return const0_rtx;
      if (op0 == const1_rtx)
	return op1;
      if (op1 == const1_rtx)
	return op0;
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return x;
      return gen_rtx_AND (0, op0, op1);

    case IOR:
      op0 = elim_reg_cond (XEXP (x, 0), regno);
      op1 = elim_reg_cond (XEXP (x, 1), regno);
      if (op0 == const1_rtx || op1 == const1_rtx)
	return const1_rtx;
      if (op0 == const0_rtx)
	return op1;
      if (op1 == const0_rtx)
	return op0;
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return x;
      return gen_rtx_IOR (0, op0, op1);

    case NOT:
      op0 = elim_reg_cond (XEXP (x, 0), regno);
      if (op0 == const0_rtx)
	return const1_rtx;
      if (op0 == const1_rtx)
	return const0_rtx;
      if (op0 != XEXP (x, 0))
	return not_reg_cond (op0);
      return x;

    default:
      abort ();
    }
}
#endif /* HAVE_conditional_execution */

#ifdef AUTO_INC_DEC

/* Try to substitute the auto-inc expression INC as the address inside
   MEM which occurs in INSN.  Currently, the address of MEM is an expression
   involving INCR_REG, and INCR is the next use of INCR_REG; it is an insn
   that has a single set whose source is a PLUS of INCR_REG and something
   else.  */

static void
attempt_auto_inc (pbi, inc, insn, mem, incr, incr_reg)
     struct propagate_block_info *pbi;
     rtx inc, insn, mem, incr, incr_reg;
{
  int regno = REGNO (incr_reg);
  rtx set = single_set (incr);
  rtx q = SET_DEST (set);
  rtx y = SET_SRC (set);
  int opnum = XEXP (y, 0) == incr_reg ? 0 : 1;

  /* Make sure this reg appears only once in this insn.  */
  if (count_occurrences (PATTERN (insn), incr_reg, 1) != 1)
    return;

  if (dead_or_set_p (incr, incr_reg)
      /* Mustn't autoinc an eliminable register.  */
      && (regno >= FIRST_PSEUDO_REGISTER
	  || ! TEST_HARD_REG_BIT (elim_reg_set, regno)))
    {
      /* This is the simple case.  Try to make the auto-inc.  If
	 we can't, we are done.  Otherwise, we will do any
	 needed updates below.  */
      if (! validate_change (insn, &XEXP (mem, 0), inc, 0))
	return;
    }
  else if (GET_CODE (q) == REG
	   /* PREV_INSN used here to check the semi-open interval
	      [insn,incr).  */
	   && ! reg_used_between_p (q,  PREV_INSN (insn), incr)
	   /* We must also check for sets of q as q may be
	      a call clobbered hard register and there may
	      be a call between PREV_INSN (insn) and incr.  */
	   && ! reg_set_between_p (q,  PREV_INSN (insn), incr))
    {
      /* We have *p followed sometime later by q = p+size.
	 Both p and q must be live afterward,
	 and q is not used between INSN and its assignment.
	 Change it to q = p, ...*q..., q = q+size.
	 Then fall into the usual case.  */
      rtx insns, temp;

      start_sequence ();
      emit_move_insn (q, incr_reg);
      insns = get_insns ();
      end_sequence ();

      if (basic_block_for_insn)
	for (temp = insns; temp; temp = NEXT_INSN (temp))
	  set_block_for_insn (temp, pbi->bb);

      /* If we can't make the auto-inc, or can't make the
	 replacement into Y, exit.  There's no point in making
	 the change below if we can't do the auto-inc and doing
	 so is not correct in the pre-inc case.  */

      XEXP (inc, 0) = q;
      validate_change (insn, &XEXP (mem, 0), inc, 1);
      validate_change (incr, &XEXP (y, opnum), q, 1);
      if (! apply_change_group ())
	return;

      /* We now know we'll be doing this change, so emit the
	 new insn(s) and do the updates.  */
      emit_insns_before (insns, insn);

      if (pbi->bb->head == insn)
	pbi->bb->head = insns;

      /* INCR will become a NOTE and INSN won't contain a
	 use of INCR_REG.  If a use of INCR_REG was just placed in
	 the insn before INSN, make that the next use.
	 Otherwise, invalidate it.  */
      if (GET_CODE (PREV_INSN (insn)) == INSN
	  && GET_CODE (PATTERN (PREV_INSN (insn))) == SET
	  && SET_SRC (PATTERN (PREV_INSN (insn))) == incr_reg)
	pbi->reg_next_use[regno] = PREV_INSN (insn);
      else
	pbi->reg_next_use[regno] = 0;

      incr_reg = q;
      regno = REGNO (q);

      /* REGNO is now used in INCR which is below INSN, but
	 it previously wasn't live here.  If we don't mark
	 it as live, we'll put a REG_DEAD note for it
	 on this insn, which is incorrect.  */
      SET_REGNO_REG_SET (pbi->reg_live, regno);

      /* If there are any calls between INSN and INCR, show
	 that REGNO now crosses them.  */
      for (temp = insn; temp != incr; temp = NEXT_INSN (temp))
	if (GET_CODE (temp) == CALL_INSN)
	  REG_N_CALLS_CROSSED (regno)++;
    }
  else
    return;

  /* If we haven't returned, it means we were able to make the
     auto-inc, so update the status.  First, record that this insn
     has an implicit side effect.  */

  REG_NOTES (insn) = alloc_EXPR_LIST (REG_INC, incr_reg, REG_NOTES (insn));

  /* Modify the old increment-insn to simply copy
     the already-incremented value of our register.  */
  if (! validate_change (incr, &SET_SRC (set), incr_reg, 0))
    abort ();

  /* If that makes it a no-op (copying the register into itself) delete
     it so it won't appear to be a "use" and a "set" of this
     register.  */
  if (REGNO (SET_DEST (set)) == REGNO (incr_reg))
    {
      /* If the original source was dead, it's dead now.  */
      rtx note;

      while ((note = find_reg_note (incr, REG_DEAD, NULL_RTX)) != NULL_RTX)
	{
	  remove_note (incr, note);
	  if (XEXP (note, 0) != incr_reg)
	    CLEAR_REGNO_REG_SET (pbi->reg_live, REGNO (XEXP (note, 0)));
	}

      PUT_CODE (incr, NOTE);
      NOTE_LINE_NUMBER (incr) = NOTE_INSN_DELETED;
      NOTE_SOURCE_FILE (incr) = 0;
    }

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      /* Count an extra reference to the reg.  When a reg is
	 incremented, spilling it is worse, so we want to make
	 that less likely.  */
      REG_FREQ (regno) += REG_FREQ_FROM_BB (pbi->bb);

      /* Count the increment as a setting of the register,
	 even though it isn't a SET in rtl.  */
      REG_N_SETS (regno)++;
    }
}

/* X is a MEM found in INSN.  See if we can convert it into an auto-increment
   reference.  */

static void
find_auto_inc (pbi, x, insn)
     struct propagate_block_info *pbi;
     rtx x;
     rtx insn;
{
  rtx addr = XEXP (x, 0);
  HOST_WIDE_INT offset = 0;
  rtx set, y, incr, inc_val;
  int regno;
  int size = GET_MODE_SIZE (GET_MODE (x));

  if (GET_CODE (insn) == JUMP_INSN)
    return;

  /* Here we detect use of an index register which might be good for
     postincrement, postdecrement, preincrement, or predecrement.  */

  if (GET_CODE (addr) == PLUS && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    offset = INTVAL (XEXP (addr, 1)), addr = XEXP (addr, 0);

  if (GET_CODE (addr) != REG)
    return;

  regno = REGNO (addr);

  /* Is the next use an increment that might make auto-increment? */
  incr = pbi->reg_next_use[regno];
  if (incr == 0 || BLOCK_NUM (incr) != BLOCK_NUM (insn))
    return;
  set = single_set (incr);
  if (set == 0 || GET_CODE (set) != SET)
    return;
  y = SET_SRC (set);

  if (GET_CODE (y) != PLUS)
    return;

  if (REG_P (XEXP (y, 0)) && REGNO (XEXP (y, 0)) == REGNO (addr))
    inc_val = XEXP (y, 1);
  else if (REG_P (XEXP (y, 1)) && REGNO (XEXP (y, 1)) == REGNO (addr))
    inc_val = XEXP (y, 0);
  else
    return;

  if (GET_CODE (inc_val) == CONST_INT)
    {
      if (HAVE_POST_INCREMENT
	  && (INTVAL (inc_val) == size && offset == 0))
	attempt_auto_inc (pbi, gen_rtx_POST_INC (Pmode, addr), insn, x,
			  incr, addr);
      else if (HAVE_POST_DECREMENT
	       && (INTVAL (inc_val) == -size && offset == 0))
	attempt_auto_inc (pbi, gen_rtx_POST_DEC (Pmode, addr), insn, x,
			  incr, addr);
      else if (HAVE_PRE_INCREMENT
	       && (INTVAL (inc_val) == size && offset == size))
	attempt_auto_inc (pbi, gen_rtx_PRE_INC (Pmode, addr), insn, x,
			  incr, addr);
      else if (HAVE_PRE_DECREMENT
	       && (INTVAL (inc_val) == -size && offset == -size))
	attempt_auto_inc (pbi, gen_rtx_PRE_DEC (Pmode, addr), insn, x,
			  incr, addr);
      else if (HAVE_POST_MODIFY_DISP && offset == 0)
	attempt_auto_inc (pbi, gen_rtx_POST_MODIFY (Pmode, addr,
						    gen_rtx_PLUS (Pmode,
								  addr,
								  inc_val)),
			  insn, x, incr, addr);
    }
  else if (GET_CODE (inc_val) == REG
	   && ! reg_set_between_p (inc_val, PREV_INSN (insn),
				   NEXT_INSN (incr)))

    {
      if (HAVE_POST_MODIFY_REG && offset == 0)
	attempt_auto_inc (pbi, gen_rtx_POST_MODIFY (Pmode, addr,
						    gen_rtx_PLUS (Pmode,
								  addr,
								  inc_val)),
			  insn, x, incr, addr);
    }
}

#endif /* AUTO_INC_DEC */

static void
mark_used_reg (pbi, reg, cond, insn)
     struct propagate_block_info *pbi;
     rtx reg;
     rtx cond ATTRIBUTE_UNUSED;
     rtx insn;
{
  unsigned int regno_first, regno_last, i;
  int some_was_live, some_was_dead, some_not_set;

  regno_last = regno_first = REGNO (reg);
  if (regno_first < FIRST_PSEUDO_REGISTER)
    regno_last += HARD_REGNO_NREGS (regno_first, GET_MODE (reg)) - 1;

  /* Find out if any of this register is live after this instruction.  */
  some_was_live = some_was_dead = 0;
  for (i = regno_first; i <= regno_last; ++i)
    {
      int needed_regno = REGNO_REG_SET_P (pbi->reg_live, i);
      some_was_live |= needed_regno;
      some_was_dead |= ! needed_regno;
    }

  /* Find out if any of the register was set this insn.  */
  some_not_set = 0;
  for (i = regno_first; i <= regno_last; ++i)
    some_not_set |= ! REGNO_REG_SET_P (pbi->new_set, i);

  if (pbi->flags & (PROP_LOG_LINKS | PROP_AUTOINC))
    {
      /* Record where each reg is used, so when the reg is set we know
	 the next insn that uses it.  */
      pbi->reg_next_use[regno_first] = insn;
    }

  if (pbi->flags & PROP_REG_INFO)
    {
      if (regno_first < FIRST_PSEUDO_REGISTER)
	{
	  /* If this is a register we are going to try to eliminate,
	     don't mark it live here.  If we are successful in
	     eliminating it, it need not be live unless it is used for
	     pseudos, in which case it will have been set live when it
	     was allocated to the pseudos.  If the register will not
	     be eliminated, reload will set it live at that point.

	     Otherwise, record that this function uses this register.  */
	  /* ??? The PPC backend tries to "eliminate" on the pic
	     register to itself.  This should be fixed.  In the mean
	     time, hack around it.  */

	  if (! (TEST_HARD_REG_BIT (elim_reg_set, regno_first)
	         && (regno_first == FRAME_POINTER_REGNUM
		     || regno_first == ARG_POINTER_REGNUM)))
	    for (i = regno_first; i <= regno_last; ++i)
	      regs_ever_live[i] = 1;
	}
      else
	{
	  /* Keep track of which basic block each reg appears in.  */

	  register int blocknum = pbi->bb->index;
	  if (REG_BASIC_BLOCK (regno_first) == REG_BLOCK_UNKNOWN)
	    REG_BASIC_BLOCK (regno_first) = blocknum;
	  else if (REG_BASIC_BLOCK (regno_first) != blocknum)
	    REG_BASIC_BLOCK (regno_first) = REG_BLOCK_GLOBAL;

	  /* Count (weighted) number of uses of each reg.  */
	  REG_FREQ (regno_first) += REG_FREQ_FROM_BB (pbi->bb);
	  REG_N_REFS (regno_first)++;
	}
    }

  /* Record and count the insns in which a reg dies.  If it is used in
     this insn and was dead below the insn then it dies in this insn.
     If it was set in this insn, we do not make a REG_DEAD note;
     likewise if we already made such a note.  */
  if ((pbi->flags & (PROP_DEATH_NOTES | PROP_REG_INFO))
      && some_was_dead
      && some_not_set)
    {
      /* Check for the case where the register dying partially
	 overlaps the register set by this insn.  */
      if (regno_first != regno_last)
	for (i = regno_first; i <= regno_last; ++i)
	  some_was_live |= REGNO_REG_SET_P (pbi->new_set, i);

      /* If none of the words in X is needed, make a REG_DEAD note.
	 Otherwise, we must make partial REG_DEAD notes.  */
      if (! some_was_live)
	{
	  if ((pbi->flags & PROP_DEATH_NOTES)
	      && ! find_regno_note (insn, REG_DEAD, regno_first))
	    REG_NOTES (insn)
	      = alloc_EXPR_LIST (REG_DEAD, reg, REG_NOTES (insn));

	  if (pbi->flags & PROP_REG_INFO)
	    REG_N_DEATHS (regno_first)++;
	}
      else
	{
	  /* Don't make a REG_DEAD note for a part of a register
	     that is set in the insn.  */
	  for (i = regno_first; i <= regno_last; ++i)
	    if (! REGNO_REG_SET_P (pbi->reg_live, i)
		&& ! dead_or_set_regno_p (insn, i))
	      REG_NOTES (insn)
		= alloc_EXPR_LIST (REG_DEAD,
				   gen_rtx_REG (reg_raw_mode[i], i),
				   REG_NOTES (insn));
	}
    }

  /* Mark the register as being live.  */
  for (i = regno_first; i <= regno_last; ++i)
    {
      SET_REGNO_REG_SET (pbi->reg_live, i);

#ifdef HAVE_conditional_execution
      /* If this is a conditional use, record that fact.  If it is later
	 conditionally set, we'll know to kill the register.  */
      if (cond != NULL_RTX)
	{
	  splay_tree_node node;
	  struct reg_cond_life_info *rcli;
	  rtx ncond;

	  if (some_was_live)
	    {
	      node = splay_tree_lookup (pbi->reg_cond_dead, i);
	      if (node == NULL)
		{
		  /* The register was unconditionally live previously.
		     No need to do anything.  */
		}
	      else
		{
		  /* The register was conditionally live previously.
		     Subtract the new life cond from the old death cond.  */
		  rcli = (struct reg_cond_life_info *) node->value;
		  ncond = rcli->condition;
		  ncond = and_reg_cond (ncond, not_reg_cond (cond), 1);

		  /* If the register is now unconditionally live,
		     remove the entry in the splay_tree.  */
		  if (ncond == const0_rtx)
		    splay_tree_remove (pbi->reg_cond_dead, i);
		  else
		    {
		      rcli->condition = ncond;
		      SET_REGNO_REG_SET (pbi->reg_cond_reg,
					 REGNO (XEXP (cond, 0)));
		    }
		}
	    }
	  else
	    {
	      /* The register was not previously live at all.  Record
		 the condition under which it is still dead.  */
	      rcli = (struct reg_cond_life_info *) xmalloc (sizeof (*rcli));
	      rcli->condition = not_reg_cond (cond);
	      rcli->stores = const0_rtx;
	      rcli->orig_condition = const0_rtx;
	      splay_tree_insert (pbi->reg_cond_dead, i,
				 (splay_tree_value) rcli);

	      SET_REGNO_REG_SET (pbi->reg_cond_reg, REGNO (XEXP (cond, 0)));
	    }
	}
      else if (some_was_live)
	{
	  /* The register may have been conditionally live previously, but
	     is now unconditionally live.  Remove it from the conditionally
	     dead list, so that a conditional set won't cause us to think
	     it dead.  */
	  splay_tree_remove (pbi->reg_cond_dead, i);
	}
#endif
    }
}

/* Scan expression X and store a 1-bit in NEW_LIVE for each reg it uses.
   This is done assuming the registers needed from X are those that
   have 1-bits in PBI->REG_LIVE.

   INSN is the containing instruction.  If INSN is dead, this function
   is not called.  */

static void
mark_used_regs (pbi, x, cond, insn)
     struct propagate_block_info *pbi;
     rtx x, cond, insn;
{
  register RTX_CODE code;
  register int regno;
  int flags = pbi->flags;

 retry:
  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case PC:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

#ifdef HAVE_cc0
    case CC0:
      pbi->cc0_live = 1;
      return;
#endif

    case CLOBBER:
      /* If we are clobbering a MEM, mark any registers inside the address
	 as being used.  */
      if (GET_CODE (XEXP (x, 0)) == MEM)
	mark_used_regs (pbi, XEXP (XEXP (x, 0), 0), cond, insn);
      return;

    case MEM:
      /* Don't bother watching stores to mems if this is not the
	 final pass.  We'll not be deleting dead stores this round.  */
      if (optimize && (flags & PROP_SCAN_DEAD_CODE))
	{
	  /* Invalidate the data for the last MEM stored, but only if MEM is
	     something that can be stored into.  */
	  if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	    /* Needn't clear the memory set list.  */
	    ;
	  else
	    {
	      rtx temp = pbi->mem_set_list;
	      rtx prev = NULL_RTX;
	      rtx next;

	      while (temp)
		{
		  next = XEXP (temp, 1);
		  if (anti_dependence (XEXP (temp, 0), x))
		    {
		      /* Splice temp out of the list.  */
		      if (prev)
			XEXP (prev, 1) = next;
		      else
			pbi->mem_set_list = next;
		      free_EXPR_LIST_node (temp);
		      pbi->mem_set_list_len--;
		    }
		  else
		    prev = temp;
		  temp = next;
		}
	    }

	  /* If the memory reference had embedded side effects (autoincrement
	     address modes.  Then we may need to kill some entries on the
	     memory set list.  */
	  if (insn)
	    invalidate_mems_from_autoinc (pbi, insn);
	}

#ifdef AUTO_INC_DEC
      if (flags & PROP_AUTOINC)
        find_auto_inc (pbi, x, insn);
#endif
      break;

    case SUBREG:
#ifdef CLASS_CANNOT_CHANGE_MODE
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) >= FIRST_PSEUDO_REGISTER
	  && CLASS_CANNOT_CHANGE_MODE_P (GET_MODE (x),
					 GET_MODE (SUBREG_REG (x))))
	REG_CHANGES_MODE (REGNO (SUBREG_REG (x))) = 1;
#endif

      /* While we're here, optimize this case.  */
      x = SUBREG_REG (x);
      if (GET_CODE (x) != REG)
	goto retry;
      /* Fall through.  */

    case REG:
      /* See a register other than being set => mark it as needed.  */
      mark_used_reg (pbi, x, cond, insn);
      return;

    case SET:
      {
	register rtx testreg = SET_DEST (x);
	int mark_dest = 0;

	/* If storing into MEM, don't show it as being used.  But do
	   show the address as being used.  */
	if (GET_CODE (testreg) == MEM)
	  {
#ifdef AUTO_INC_DEC
	    if (flags & PROP_AUTOINC)
	      find_auto_inc (pbi, testreg, insn);
#endif
	    mark_used_regs (pbi, XEXP (testreg, 0), cond, insn);
	    mark_used_regs (pbi, SET_SRC (x), cond, insn);
	    return;
	  }

	/* Storing in STRICT_LOW_PART is like storing in a reg
	   in that this SET might be dead, so ignore it in TESTREG.
	   but in some other ways it is like using the reg.

	   Storing in a SUBREG or a bit field is like storing the entire
	   register in that if the register's value is not used
	   then this SET is not needed.  */
	while (GET_CODE (testreg) == STRICT_LOW_PART
	       || GET_CODE (testreg) == ZERO_EXTRACT
	       || GET_CODE (testreg) == SIGN_EXTRACT
	       || GET_CODE (testreg) == SUBREG)
	  {
#ifdef CLASS_CANNOT_CHANGE_MODE
	    if (GET_CODE (testreg) == SUBREG
		&& GET_CODE (SUBREG_REG (testreg)) == REG
		&& REGNO (SUBREG_REG (testreg)) >= FIRST_PSEUDO_REGISTER
		&& CLASS_CANNOT_CHANGE_MODE_P (GET_MODE (SUBREG_REG (testreg)),
					       GET_MODE (testreg)))
	      REG_CHANGES_MODE (REGNO (SUBREG_REG (testreg))) = 1;
#endif

	    /* Modifying a single register in an alternate mode
	       does not use any of the old value.  But these other
	       ways of storing in a register do use the old value.  */
	    if (GET_CODE (testreg) == SUBREG
		&& !(REG_SIZE (SUBREG_REG (testreg)) > REG_SIZE (testreg)))
	      ;
	    else
	      mark_dest = 1;

	    testreg = XEXP (testreg, 0);
	  }

	/* If this is a store into a register or group of registers,
	   recursively scan the value being stored.  */

	if ((GET_CODE (testreg) == PARALLEL
	     && GET_MODE (testreg) == BLKmode)
	    || (GET_CODE (testreg) == REG
		&& (regno = REGNO (testreg),
		    ! (regno == FRAME_POINTER_REGNUM
		       && (! reload_completed || frame_pointer_needed)))
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		&& ! (regno == HARD_FRAME_POINTER_REGNUM
		      && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
		&& ! (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
		))
	  {
	    if (mark_dest)
	      mark_used_regs (pbi, SET_DEST (x), cond, insn);
	    mark_used_regs (pbi, SET_SRC (x), cond, insn);
	    return;
	  }
      }
      break;

    case ASM_OPERANDS:
    case UNSPEC_VOLATILE:
    case TRAP_IF:
    case ASM_INPUT:
      {
	/* Traditional and volatile asm instructions must be considered to use
	   and clobber all hard registers, all pseudo-registers and all of
	   memory.  So must TRAP_IF and UNSPEC_VOLATILE operations.

	   Consider for instance a volatile asm that changes the fpu rounding
	   mode.  An insn should not be moved across this even if it only uses
	   pseudo-regs because it might give an incorrectly rounded result.

	   ?!? Unfortunately, marking all hard registers as live causes massive
	   problems for the register allocator and marking all pseudos as live
	   creates mountains of uninitialized variable warnings.

	   So for now, just clear the memory set list and mark any regs
	   we can find in ASM_OPERANDS as used.  */
	if (code != ASM_OPERANDS || MEM_VOLATILE_P (x))
	  {
	    free_EXPR_LIST_list (&pbi->mem_set_list);
	    pbi->mem_set_list_len = 0;
	  }

	/* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */
	if (code == ASM_OPERANDS)
	  {
	    int j;

	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      mark_used_regs (pbi, ASM_OPERANDS_INPUT (x, j), cond, insn);
	  }
	break;
      }

    case COND_EXEC:
      if (cond != NULL_RTX)
	abort ();

      mark_used_regs (pbi, COND_EXEC_TEST (x), NULL_RTX, insn);

      cond = COND_EXEC_TEST (x);
      x = COND_EXEC_CODE (x);
      goto retry;

    case PHI:
      /* We _do_not_ want to scan operands of phi nodes.  Operands of
	 a phi function are evaluated only when control reaches this
	 block along a particular edge.  Therefore, regs that appear
	 as arguments to phi should not be added to the global live at
	 start.  */
      return;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register const char * const fmt = GET_RTX_FORMAT (code);
    register int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    /* Tail recursive case: save a function call level.  */
	    if (i == 0)
	      {
		x = XEXP (x, 0);
		goto retry;
	      }
	    mark_used_regs (pbi, XEXP (x, i), cond, insn);
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      mark_used_regs (pbi, XVECEXP (x, i, j), cond, insn);
	  }
      }
  }
}

#ifdef AUTO_INC_DEC

static int
try_pre_increment_1 (pbi, insn)
     struct propagate_block_info *pbi;
     rtx insn;
{
  /* Find the next use of this reg.  If in same basic block,
     make it do pre-increment or pre-decrement if appropriate.  */
  rtx x = single_set (insn);
  HOST_WIDE_INT amount = ((GET_CODE (SET_SRC (x)) == PLUS ? 1 : -1)
			  * INTVAL (XEXP (SET_SRC (x), 1)));
  int regno = REGNO (SET_DEST (x));
  rtx y = pbi->reg_next_use[regno];
  if (y != 0
      && SET_DEST (x) != stack_pointer_rtx
      && BLOCK_NUM (y) == BLOCK_NUM (insn)
      /* Don't do this if the reg dies, or gets set in y; a standard addressing
	 mode would be better.  */
      && ! dead_or_set_p (y, SET_DEST (x))
      && try_pre_increment (y, SET_DEST (x), amount))
    {
      /* We have found a suitable auto-increment and already changed
	 insn Y to do it.  So flush this increment instruction.  */
      propagate_block_delete_insn (pbi->bb, insn);

      /* Count a reference to this reg for the increment insn we are
	 deleting.  When a reg is incremented, spilling it is worse,
	 so we want to make that less likely.  */
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  REG_FREQ (regno) += REG_FREQ_FROM_BB (pbi->bb);
	  REG_N_SETS (regno)++;
	}

      /* Flush any remembered memories depending on the value of
	 the incremented register.  */
      invalidate_mems_from_set (pbi, SET_DEST (x));

      return 1;
    }
  return 0;
}

/* Try to change INSN so that it does pre-increment or pre-decrement
   addressing on register REG in order to add AMOUNT to REG.
   AMOUNT is negative for pre-decrement.
   Returns 1 if the change could be made.
   This checks all about the validity of the result of modifying INSN.  */

static int
try_pre_increment (insn, reg, amount)
     rtx insn, reg;
     HOST_WIDE_INT amount;
{
  register rtx use;

  /* Nonzero if we can try to make a pre-increment or pre-decrement.
     For example, addl $4,r1; movl (r1),... can become movl +(r1),...  */
  int pre_ok = 0;
  /* Nonzero if we can try to make a post-increment or post-decrement.
     For example, addl $4,r1; movl -4(r1),... can become movl (r1)+,...
     It is possible for both PRE_OK and POST_OK to be nonzero if the machine
     supports both pre-inc and post-inc, or both pre-dec and post-dec.  */
  int post_ok = 0;

  /* Nonzero if the opportunity actually requires post-inc or post-dec.  */
  int do_post = 0;

  /* From the sign of increment, see which possibilities are conceivable
     on this target machine.  */
  if (HAVE_PRE_INCREMENT && amount > 0)
    pre_ok = 1;
  if (HAVE_POST_INCREMENT && amount > 0)
    post_ok = 1;

  if (HAVE_PRE_DECREMENT && amount < 0)
    pre_ok = 1;
  if (HAVE_POST_DECREMENT && amount < 0)
    post_ok = 1;

  if (! (pre_ok || post_ok))
    return 0;

  /* It is not safe to add a side effect to a jump insn
     because if the incremented register is spilled and must be reloaded
     there would be no way to store the incremented value back in memory.  */

  if (GET_CODE (insn) == JUMP_INSN)
    return 0;

  use = 0;
  if (pre_ok)
    use = find_use_as_address (PATTERN (insn), reg, 0);
  if (post_ok && (use == 0 || use == (rtx) 1))
    {
      use = find_use_as_address (PATTERN (insn), reg, -amount);
      do_post = 1;
    }

  if (use == 0 || use == (rtx) 1)
    return 0;

  if (GET_MODE_SIZE (GET_MODE (use)) != (amount > 0 ? amount : - amount))
    return 0;

  /* See if this combination of instruction and addressing mode exists.  */
  if (! validate_change (insn, &XEXP (use, 0),
			 gen_rtx_fmt_e (amount > 0
					? (do_post ? POST_INC : PRE_INC)
					: (do_post ? POST_DEC : PRE_DEC),
					Pmode, reg), 0))
    return 0;

  /* Record that this insn now has an implicit side effect on X.  */
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_INC, reg, REG_NOTES (insn));
  return 1;
}

#endif /* AUTO_INC_DEC */

/* Find the place in the rtx X where REG is used as a memory address.
   Return the MEM rtx that so uses it.
   If PLUSCONST is nonzero, search instead for a memory address equivalent to
   (plus REG (const_int PLUSCONST)).

   If such an address does not appear, return 0.
   If REG appears more than once, or is used other than in such an address,
   return (rtx)1.  */

rtx
find_use_as_address (x, reg, plusconst)
     register rtx x;
     rtx reg;
     HOST_WIDE_INT plusconst;
{
  enum rtx_code code = GET_CODE (x);
  const char * const fmt = GET_RTX_FORMAT (code);
  register int i;
  register rtx value = 0;
  register rtx tem;

  if (code == MEM && XEXP (x, 0) == reg && plusconst == 0)
    return x;

  if (code == MEM && GET_CODE (XEXP (x, 0)) == PLUS
      && XEXP (XEXP (x, 0), 0) == reg
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && INTVAL (XEXP (XEXP (x, 0), 1)) == plusconst)
    return x;

  if (code == SIGN_EXTRACT || code == ZERO_EXTRACT)
    {
      /* If REG occurs inside a MEM used in a bit-field reference,
	 that is unacceptable.  */
      if (find_use_as_address (XEXP (x, 0), reg, 0) != 0)
	return (rtx) (HOST_WIDE_INT) 1;
    }

  if (x == reg)
    return (rtx) (HOST_WIDE_INT) 1;

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  tem = find_use_as_address (XEXP (x, i), reg, plusconst);
	  if (value == 0)
	    value = tem;
	  else if (tem != 0)
	    return (rtx) (HOST_WIDE_INT) 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      tem = find_use_as_address (XVECEXP (x, i, j), reg, plusconst);
	      if (value == 0)
		value = tem;
	      else if (tem != 0)
		return (rtx) (HOST_WIDE_INT) 1;
	    }
	}
    }

  return value;
}

/* Write information about registers and basic blocks into FILE.
   This is part of making a debugging dump.  */

void
dump_regset (r, outf)
     regset r;
     FILE *outf;
{
  int i;
  if (r == NULL)
    {
      fputs (" (nil)", outf);
      return;
    }

  EXECUTE_IF_SET_IN_REG_SET (r, 0, i,
    {
      fprintf (outf, " %d", i);
      if (i < FIRST_PSEUDO_REGISTER)
	fprintf (outf, " [%s]",
		 reg_names[i]);
    });
}

/* Print a human-reaable representation of R on the standard error
   stream.  This function is designed to be used from within the
   debugger.  */

void
debug_regset (r)
     regset r;
{
  dump_regset (r, stderr);
  putc ('\n', stderr);
}

void
dump_flow_info (file)
     FILE *file;
{
  register int i;
  static const char * const reg_class_names[] = REG_CLASS_NAMES;

  fprintf (file, "%d registers.\n", max_regno);
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (REG_N_REFS (i))
      {
	enum reg_class class, altclass;
	fprintf (file, "\nRegister %d used %d times across %d insns",
		 i, REG_N_REFS (i), REG_LIVE_LENGTH (i));
	if (REG_BASIC_BLOCK (i) >= 0)
	  fprintf (file, " in block %d", REG_BASIC_BLOCK (i));
	if (REG_N_SETS (i))
	  fprintf (file, "; set %d time%s", REG_N_SETS (i),
		   (REG_N_SETS (i) == 1) ? "" : "s");
	if (REG_USERVAR_P (regno_reg_rtx[i]))
	  fprintf (file, "; user var");
	if (REG_N_DEATHS (i) != 1)
	  fprintf (file, "; dies in %d places", REG_N_DEATHS (i));
	if (REG_N_CALLS_CROSSED (i) == 1)
	  fprintf (file, "; crosses 1 call");
	else if (REG_N_CALLS_CROSSED (i))
	  fprintf (file, "; crosses %d calls", REG_N_CALLS_CROSSED (i));
	if (PSEUDO_REGNO_BYTES (i) != UNITS_PER_WORD)
	  fprintf (file, "; %d bytes", PSEUDO_REGNO_BYTES (i));
	class = reg_preferred_class (i);
	altclass = reg_alternate_class (i);
	if (class != GENERAL_REGS || altclass != ALL_REGS)
	  {
	    if (altclass == ALL_REGS || class == ALL_REGS)
	      fprintf (file, "; pref %s", reg_class_names[(int) class]);
	    else if (altclass == NO_REGS)
	      fprintf (file, "; %s or none", reg_class_names[(int) class]);
	    else
	      fprintf (file, "; pref %s, else %s",
		       reg_class_names[(int) class],
		       reg_class_names[(int) altclass]);
	  }
	if (REG_POINTER (regno_reg_rtx[i]))
	  fprintf (file, "; pointer");
	fprintf (file, ".\n");
      }

  fprintf (file, "\n%d basic blocks, %d edges.\n", n_basic_blocks, n_edges);
  for (i = 0; i < n_basic_blocks; i++)
    {
      register basic_block bb = BASIC_BLOCK (i);
      register edge e;

      fprintf (file, "\nBasic block %d: first insn %d, last %d, loop_depth %d, count ",
	       i, INSN_UID (bb->head), INSN_UID (bb->end), bb->loop_depth);
      fprintf (file, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) bb->count);
      fprintf (file, ", freq %i.\n", bb->frequency);

      fprintf (file, "Predecessors: ");
      for (e = bb->pred; e; e = e->pred_next)
	dump_edge_info (file, e, 0);

      fprintf (file, "\nSuccessors: ");
      for (e = bb->succ; e; e = e->succ_next)
	dump_edge_info (file, e, 1);

      fprintf (file, "\nRegisters live at start:");
      dump_regset (bb->global_live_at_start, file);

      fprintf (file, "\nRegisters live at end:");
      dump_regset (bb->global_live_at_end, file);

      putc ('\n', file);
    }

  putc ('\n', file);
}

void
debug_flow_info ()
{
  dump_flow_info (stderr);
}

void
dump_edge_info (file, e, do_succ)
     FILE *file;
     edge e;
     int do_succ;
{
  basic_block side = (do_succ ? e->dest : e->src);

  if (side == ENTRY_BLOCK_PTR)
    fputs (" ENTRY", file);
  else if (side == EXIT_BLOCK_PTR)
    fputs (" EXIT", file);
  else
    fprintf (file, " %d", side->index);

  if (e->probability)
    fprintf (file, " [%.1f%%] ", e->probability * 100.0 / REG_BR_PROB_BASE);

  if (e->count)
    {
      fprintf (file, " count:");
      fprintf (file, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) e->count);
    }

  if (e->flags)
    {
      static const char * const bitnames[] = {
	"fallthru", "crit", "ab", "abcall", "eh", "fake", "dfs_back"
      };
      int comma = 0;
      int i, flags = e->flags;

      fputc (' ', file);
      fputc ('(', file);
      for (i = 0; flags; i++)
	if (flags & (1 << i))
	  {
	    flags &= ~(1 << i);

	    if (comma)
	      fputc (',', file);
	    if (i < (int) ARRAY_SIZE (bitnames))
	      fputs (bitnames[i], file);
	    else
	      fprintf (file, "%d", i);
	    comma = 1;
	  }
      fputc (')', file);
    }
}

/* Print out one basic block with live information at start and end.  */

void
dump_bb (bb, outf)
     basic_block bb;
     FILE *outf;
{
  rtx insn;
  rtx last;
  edge e;

  fprintf (outf, ";; Basic block %d, loop depth %d, count ",
	   bb->index, bb->loop_depth);
  fprintf (outf, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT) bb->count);
  putc ('\n', outf);

  fputs (";; Predecessors: ", outf);
  for (e = bb->pred; e; e = e->pred_next)
    dump_edge_info (outf, e, 0);
  putc ('\n', outf);

  fputs (";; Registers live at start:", outf);
  dump_regset (bb->global_live_at_start, outf);
  putc ('\n', outf);

  for (insn = bb->head, last = NEXT_INSN (bb->end);
       insn != last;
       insn = NEXT_INSN (insn))
    print_rtl_single (outf, insn);

  fputs (";; Registers live at end:", outf);
  dump_regset (bb->global_live_at_end, outf);
  putc ('\n', outf);

  fputs (";; Successors: ", outf);
  for (e = bb->succ; e; e = e->succ_next)
    dump_edge_info (outf, e, 1);
  putc ('\n', outf);
}

void
debug_bb (bb)
     basic_block bb;
{
  dump_bb (bb, stderr);
}

void
debug_bb_n (n)
     int n;
{
  dump_bb (BASIC_BLOCK (n), stderr);
}

/* Like print_rtl, but also print out live information for the start of each
   basic block.  */

void
print_rtl_with_bb (outf, rtx_first)
     FILE *outf;
     rtx rtx_first;
{
  register rtx tmp_rtx;

  if (rtx_first == 0)
    fprintf (outf, "(nil)\n");
  else
    {
      int i;
      enum bb_state { NOT_IN_BB, IN_ONE_BB, IN_MULTIPLE_BB };
      int max_uid = get_max_uid ();
      basic_block *start = (basic_block *)
	xcalloc (max_uid, sizeof (basic_block));
      basic_block *end = (basic_block *)
	xcalloc (max_uid, sizeof (basic_block));
      enum bb_state *in_bb_p = (enum bb_state *)
	xcalloc (max_uid, sizeof (enum bb_state));

      for (i = n_basic_blocks - 1; i >= 0; i--)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  rtx x;

	  start[INSN_UID (bb->head)] = bb;
	  end[INSN_UID (bb->end)] = bb;
	  for (x = bb->head; x != NULL_RTX; x = NEXT_INSN (x))
	    {
	      enum bb_state state = IN_MULTIPLE_BB;
	      if (in_bb_p[INSN_UID (x)] == NOT_IN_BB)
		state = IN_ONE_BB;
	      in_bb_p[INSN_UID (x)] = state;

	      if (x == bb->end)
		break;
	    }
	}

      for (tmp_rtx = rtx_first; NULL != tmp_rtx; tmp_rtx = NEXT_INSN (tmp_rtx))
	{
	  int did_output;
	  basic_block bb;

	  if ((bb = start[INSN_UID (tmp_rtx)]) != NULL)
	    {
	      fprintf (outf, ";; Start of basic block %d, registers live:",
		       bb->index);
	      dump_regset (bb->global_live_at_start, outf);
	      putc ('\n', outf);
	    }

	  if (in_bb_p[INSN_UID (tmp_rtx)] == NOT_IN_BB
	      && GET_CODE (tmp_rtx) != NOTE
	      && GET_CODE (tmp_rtx) != BARRIER)
	    fprintf (outf, ";; Insn is not within a basic block\n");
	  else if (in_bb_p[INSN_UID (tmp_rtx)] == IN_MULTIPLE_BB)
	    fprintf (outf, ";; Insn is in multiple basic blocks\n");

	  did_output = print_rtl_single (outf, tmp_rtx);

	  if ((bb = end[INSN_UID (tmp_rtx)]) != NULL)
	    {
	      fprintf (outf, ";; End of basic block %d, registers live:\n",
		       bb->index);
	      dump_regset (bb->global_live_at_end, outf);
	      putc ('\n', outf);
	    }

	  if (did_output)
	    putc ('\n', outf);
	}

      free (start);
      free (end);
      free (in_bb_p);
    }

  if (current_function_epilogue_delay_list != 0)
    {
      fprintf (outf, "\n;; Insns in epilogue delay list:\n\n");
      for (tmp_rtx = current_function_epilogue_delay_list; tmp_rtx != 0;
	   tmp_rtx = XEXP (tmp_rtx, 1))
	print_rtl_single (outf, XEXP (tmp_rtx, 0));
    }
}

/* Dump the rtl into the current debugging dump file, then abort.  */

static void
print_rtl_and_abort_fcn (file, line, function)
     const char *file;
     int line;
     const char *function;
{
  if (rtl_dump_file)
    {
      print_rtl_with_bb (rtl_dump_file, get_insns ());
      fclose (rtl_dump_file);
    }

  fancy_abort (file, line, function);
}

/* Recompute register set/reference counts immediately prior to register
   allocation.

   This avoids problems with set/reference counts changing to/from values
   which have special meanings to the register allocators.

   Additionally, the reference counts are the primary component used by the
   register allocators to prioritize pseudos for allocation to hard regs.
   More accurate reference counts generally lead to better register allocation.

   F is the first insn to be scanned.

   LOOP_STEP denotes how much loop_depth should be incremented per
   loop nesting level in order to increase the ref count more for
   references in a loop.

   It might be worthwhile to update REG_LIVE_LENGTH, REG_BASIC_BLOCK and
   possibly other information which is used by the register allocators.  */

void
recompute_reg_usage (f, loop_step)
     rtx f ATTRIBUTE_UNUSED;
     int loop_step ATTRIBUTE_UNUSED;
{
  allocate_reg_life_data ();
  update_life_info (NULL, UPDATE_LIFE_LOCAL, PROP_REG_INFO);
}

/* Optionally removes all the REG_DEAD and REG_UNUSED notes from a set of
   blocks.  If BLOCKS is NULL, assume the universal set.  Returns a count
   of the number of registers that died.  */

int
count_or_remove_death_notes (blocks, kill)
     sbitmap blocks;
     int kill;
{
  int i, count = 0;

  for (i = n_basic_blocks - 1; i >= 0; --i)
    {
      basic_block bb;
      rtx insn;

      if (blocks && ! TEST_BIT (blocks, i))
	continue;

      bb = BASIC_BLOCK (i);

      for (insn = bb->head;; insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      rtx *pprev = &REG_NOTES (insn);
	      rtx link = *pprev;

	      while (link)
		{
		  switch (REG_NOTE_KIND (link))
		    {
		    case REG_DEAD:
		      if (GET_CODE (XEXP (link, 0)) == REG)
			{
			  rtx reg = XEXP (link, 0);
			  int n;

			  if (REGNO (reg) >= FIRST_PSEUDO_REGISTER)
			    n = 1;
			  else
			    n = HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg));
			  count += n;
			}
		      /* Fall through.  */

		    case REG_UNUSED:
		      if (kill)
			{
			  rtx next = XEXP (link, 1);
			  free_EXPR_LIST_node (link);
			  *pprev = link = next;
			  break;
			}
		      /* Fall through.  */

		    default:
		      pprev = &XEXP (link, 1);
		      link = *pprev;
		      break;
		    }
		}
	    }

	  if (insn == bb->end)
	    break;
	}
    }

  return count;
}


/* Update insns block within BB.  */

void
update_bb_for_insn (bb)
     basic_block bb;
{
  rtx insn;

  if (! basic_block_for_insn)
    return;

  for (insn = bb->head; ; insn = NEXT_INSN (insn))
    {
      set_block_for_insn (insn, bb);

      if (insn == bb->end)
	break;
    }
}


/* Record INSN's block as BB.  */

void
set_block_for_insn (insn, bb)
     rtx insn;
     basic_block bb;
{
  size_t uid = INSN_UID (insn);
  if (uid >= basic_block_for_insn->num_elements)
    {
      int new_size;

      /* Add one-eighth the size so we don't keep calling xrealloc.  */
      new_size = uid + (uid + 7) / 8;

      VARRAY_GROW (basic_block_for_insn, new_size);
    }
  VARRAY_BB (basic_block_for_insn, uid) = bb;
}

/* When a new insn has been inserted into an existing block, it will
   sometimes emit more than a single insn. This routine will set the
   block number for the specified insn, and look backwards in the insn
   chain to see if there are any other uninitialized insns immediately
   previous to this one, and set the block number for them too.  */

void
set_block_for_new_insns (insn, bb)
     rtx insn;
     basic_block bb;
{
  set_block_for_insn (insn, bb);

  /* Scan the previous instructions setting the block number until we find
     an instruction that has the block number set, or we find a note
     of any kind.  */
  for (insn = PREV_INSN (insn); insn != NULL_RTX; insn = PREV_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE)
	break;
      if ((unsigned) INSN_UID (insn) >= basic_block_for_insn->num_elements
	  || BLOCK_FOR_INSN (insn) == 0)
	set_block_for_insn (insn, bb);
      else
	break;
    }
}

/* Verify the CFG consistency.  This function check some CFG invariants and
   aborts when something is wrong.  Hope that this function will help to
   convert many optimization passes to preserve CFG consistent.

   Currently it does following checks:

   - test head/end pointers
   - overlapping of basic blocks
   - edge list correctness
   - headers of basic blocks (the NOTE_INSN_BASIC_BLOCK note)
   - tails of basic blocks (ensure that boundary is necesary)
   - scans body of the basic block for JUMP_INSN, CODE_LABEL
     and NOTE_INSN_BASIC_BLOCK
   - check that all insns are in the basic blocks
   (except the switch handling code, barriers and notes)
   - check that all returns are followed by barriers

   In future it can be extended check a lot of other stuff as well
   (reachability of basic blocks, life information, etc. etc.).  */

void
verify_flow_info ()
{
  const int max_uid = get_max_uid ();
  const rtx rtx_first = get_insns ();
  rtx last_head = get_last_insn ();
  basic_block *bb_info, *last_visited;
  rtx x;
  int i, last_bb_num_seen, num_bb_notes, err = 0;

  bb_info = (basic_block *) xcalloc (max_uid, sizeof (basic_block));
  last_visited = (basic_block *) xcalloc (n_basic_blocks + 2,
					  sizeof (basic_block));

  for (i = n_basic_blocks - 1; i >= 0; i--)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx head = bb->head;
      rtx end = bb->end;

      /* Verify the end of the basic block is in the INSN chain.  */
      for (x = last_head; x != NULL_RTX; x = PREV_INSN (x))
	if (x == end)
	  break;
      if (!x)
	{
	  error ("End insn %d for block %d not found in the insn stream.",
		 INSN_UID (end), bb->index);
	  err = 1;
	}

      /* Work backwards from the end to the head of the basic block
	 to verify the head is in the RTL chain.  */
      for (; x != NULL_RTX; x = PREV_INSN (x))
	{
	  /* While walking over the insn chain, verify insns appear
	     in only one basic block and initialize the BB_INFO array
	     used by other passes.  */
	  if (bb_info[INSN_UID (x)] != NULL)
	    {
	      error ("Insn %d is in multiple basic blocks (%d and %d)",
		     INSN_UID (x), bb->index, bb_info[INSN_UID (x)]->index);
	      err = 1;
	    }
	  bb_info[INSN_UID (x)] = bb;

	  if (x == head)
	    break;
	}
      if (!x)
	{
	  error ("Head insn %d for block %d not found in the insn stream.",
		 INSN_UID (head), bb->index);
	  err = 1;
	}

      last_head = x;
    }

  /* Now check the basic blocks (boundaries etc.) */
  for (i = n_basic_blocks - 1; i >= 0; i--)
    {
      basic_block bb = BASIC_BLOCK (i);
      /* Check correctness of edge lists.  */
      edge e;
      int has_fallthru = 0;

      e = bb->succ;
      while (e)
	{
	  if (last_visited [e->dest->index + 2] == bb)
	    {
	      error ("verify_flow_info: Duplicate edge %i->%i",
		     e->src->index, e->dest->index);
	      err = 1;
	    }
	  last_visited [e->dest->index + 2] = bb;

	  if (e->flags & EDGE_FALLTHRU)
	    has_fallthru = 1;

	  if ((e->flags & EDGE_FALLTHRU)
	      && e->src != ENTRY_BLOCK_PTR
	      && e->dest != EXIT_BLOCK_PTR)
	    {
	      rtx insn;
	      if (e->src->index + 1 != e->dest->index)
		{
		    error ("verify_flow_info: Incorrect blocks for fallthru %i->%i",
			   e->src->index, e->dest->index);
		    err = 1;
		}
	      else
		for (insn = NEXT_INSN (e->src->end); insn != e->dest->head;
		     insn = NEXT_INSN (insn))
		  if (GET_CODE (insn) == BARRIER || INSN_P (insn))
		    {
		      error ("verify_flow_info: Incorrect fallthru %i->%i",
			     e->src->index, e->dest->index);
		      fatal_insn ("Wrong insn in the fallthru edge", insn);
		      err = 1;
		    }
	    }
	  if (e->src != bb)
	    {
	      error ("verify_flow_info: Basic block %d succ edge is corrupted",
		     bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fprintf (stderr, "\n");
	      err = 1;
	    }
	  if (e->dest != EXIT_BLOCK_PTR)
	    {
	      edge e2 = e->dest->pred;
	      while (e2 && e2 != e)
		e2 = e2->pred_next;
	      if (!e2)
		{
		  error ("Basic block %i edge lists are corrupted", bb->index);
		  err = 1;
		}
	    }
	  e = e->succ_next;
	}
      if (!has_fallthru)
	{
	  rtx insn = bb->end;

	  /* Ensure existence of barrier in BB with no fallthru edges.  */
	  for (insn = bb->end; GET_CODE (insn) != BARRIER;
	       insn = NEXT_INSN (insn))
	    if (!insn
		|| (GET_CODE (insn) == NOTE
		    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BASIC_BLOCK))
		{
		  error ("Missing barrier after block %i", bb->index);
		  err = 1;
		}
	}

      e = bb->pred;
      while (e)
	{
	  if (e->dest != bb)
	    {
	      error ("Basic block %d pred edge is corrupted", bb->index);
	      fputs ("Predecessor: ", stderr);
	      dump_edge_info (stderr, e, 0);
	      fputs ("\nSuccessor: ", stderr);
	      dump_edge_info (stderr, e, 1);
	      fputc ('\n', stderr);
	      err = 1;
	    }
	  if (e->src != ENTRY_BLOCK_PTR)
	    {
	      edge e2 = e->src->succ;
	      while (e2 && e2 != e)
		e2 = e2->succ_next;
	      if (!e2)
		{
		  error ("Basic block %i edge lists are corrupted", bb->index);
		  err = 1;
		}
	    }
	  e = e->pred_next;
	}

      /* OK pointers are correct.  Now check the header of basic
         block.  It ought to contain optional CODE_LABEL followed
	 by NOTE_BASIC_BLOCK.  */
      x = bb->head;
      if (GET_CODE (x) == CODE_LABEL)
	{
	  if (bb->end == x)
	    {
	      error ("NOTE_INSN_BASIC_BLOCK is missing for block %d",
		     bb->index);
	      err = 1;
	    }
	  x = NEXT_INSN (x);
	}
      if (!NOTE_INSN_BASIC_BLOCK_P (x) || NOTE_BASIC_BLOCK (x) != bb)
	{
	  error ("NOTE_INSN_BASIC_BLOCK is missing for block %d\n",
		 bb->index);
	  err = 1;
	}

      if (bb->end == x)
	{
	  /* Do checks for empty blocks here */
	}
      else
	{
	  x = NEXT_INSN (x);
	  while (x)
	    {
	      if (NOTE_INSN_BASIC_BLOCK_P (x))
		{
		  error ("NOTE_INSN_BASIC_BLOCK %d in the middle of basic block %d",
			 INSN_UID (x), bb->index);
		  err = 1;
		}

	      if (x == bb->end)
		break;

	      if (GET_CODE (x) == JUMP_INSN
		  || GET_CODE (x) == CODE_LABEL
		  || GET_CODE (x) == BARRIER)
		{
		  error ("In basic block %d:", bb->index);
		  fatal_insn ("Flow control insn inside a basic block", x);
		}

	      x = NEXT_INSN (x);
	    }
	}
    }

  last_bb_num_seen = -1;
  num_bb_notes = 0;
  x = rtx_first;
  while (x)
    {
      if (NOTE_INSN_BASIC_BLOCK_P (x))
	{
	  basic_block bb = NOTE_BASIC_BLOCK (x);
	  num_bb_notes++;
	  if (bb->index != last_bb_num_seen + 1)
	    internal_error ("Basic blocks not numbered consecutively.");

	  last_bb_num_seen = bb->index;
	}

      if (!bb_info[INSN_UID (x)])
	{
	  switch (GET_CODE (x))
	    {
	    case BARRIER:
	    case NOTE:
	      break;

	    case CODE_LABEL:
	      /* An addr_vec is placed outside any block block.  */
	      if (NEXT_INSN (x)
		  && GET_CODE (NEXT_INSN (x)) == JUMP_INSN
		  && (GET_CODE (PATTERN (NEXT_INSN (x))) == ADDR_DIFF_VEC
		      || GET_CODE (PATTERN (NEXT_INSN (x))) == ADDR_VEC))
		{
		  x = NEXT_INSN (x);
		}

	      /* But in any case, non-deletable labels can appear anywhere.  */
	      break;

	    default:
	      fatal_insn ("Insn outside basic block", x);
	    }
	}

      if (INSN_P (x)
	  && GET_CODE (x) == JUMP_INSN
	  && returnjump_p (x) && ! condjump_p (x)
	  && ! (NEXT_INSN (x) && GET_CODE (NEXT_INSN (x)) == BARRIER))
	    fatal_insn ("Return not followed by barrier", x);

      x = NEXT_INSN (x);
    }

  if (num_bb_notes != n_basic_blocks)
    internal_error
      ("number of bb notes in insn chain (%d) != n_basic_blocks (%d)",
       num_bb_notes, n_basic_blocks);

  if (err)
    internal_error ("verify_flow_info failed.");

  /* Clean up.  */
  free (bb_info);
  free (last_visited);
}

/* Functions to access an edge list with a vector representation.
   Enough data is kept such that given an index number, the
   pred and succ that edge represents can be determined, or
   given a pred and a succ, its index number can be returned.
   This allows algorithms which consume a lot of memory to
   represent the normally full matrix of edge (pred,succ) with a
   single indexed vector,  edge (EDGE_INDEX (pred, succ)), with no
   wasted space in the client code due to sparse flow graphs.  */

/* This functions initializes the edge list. Basically the entire
   flowgraph is processed, and all edges are assigned a number,
   and the data structure is filled in.  */

struct edge_list *
create_edge_list ()
{
  struct edge_list *elist;
  edge e;
  int num_edges;
  int x;
  int block_count;

  block_count = n_basic_blocks + 2;   /* Include the entry and exit blocks.  */

  num_edges = 0;

  /* Determine the number of edges in the flow graph by counting successor
     edges on each basic block.  */
  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      for (e = bb->succ; e; e = e->succ_next)
	num_edges++;
    }
  /* Don't forget successors of the entry block.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    num_edges++;

  elist = (struct edge_list *) xmalloc (sizeof (struct edge_list));
  elist->num_blocks = block_count;
  elist->num_edges = num_edges;
  elist->index_to_edge = (edge *) xmalloc (sizeof (edge) * num_edges);

  num_edges = 0;

  /* Follow successors of the entry block, and register these edges.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      elist->index_to_edge[num_edges] = e;
      num_edges++;
    }

  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      /* Follow all successors of blocks, and register these edges.  */
      for (e = bb->succ; e; e = e->succ_next)
	{
	  elist->index_to_edge[num_edges] = e;
	  num_edges++;
	}
    }
  return elist;
}

/* This function free's memory associated with an edge list.  */

void
free_edge_list (elist)
     struct edge_list *elist;
{
  if (elist)
    {
      free (elist->index_to_edge);
      free (elist);
    }
}

/* This function provides debug output showing an edge list.  */

void
print_edge_list (f, elist)
     FILE *f;
     struct edge_list *elist;
{
  int x;
  fprintf (f, "Compressed edge list, %d BBs + entry & exit, and %d edges\n",
	   elist->num_blocks - 2, elist->num_edges);

  for (x = 0; x < elist->num_edges; x++)
    {
      fprintf (f, " %-4d - edge(", x);
      if (INDEX_EDGE_PRED_BB (elist, x) == ENTRY_BLOCK_PTR)
	fprintf (f, "entry,");
      else
	fprintf (f, "%d,", INDEX_EDGE_PRED_BB (elist, x)->index);

      if (INDEX_EDGE_SUCC_BB (elist, x) == EXIT_BLOCK_PTR)
	fprintf (f, "exit)\n");
      else
	fprintf (f, "%d)\n", INDEX_EDGE_SUCC_BB (elist, x)->index);
    }
}

/* This function provides an internal consistency check of an edge list,
   verifying that all edges are present, and that there are no
   extra edges.  */

void
verify_edge_list (f, elist)
     FILE *f;
     struct edge_list *elist;
{
  int x, pred, succ, index;
  edge e;

  for (x = 0; x < n_basic_blocks; x++)
    {
      basic_block bb = BASIC_BLOCK (x);

      for (e = bb->succ; e; e = e->succ_next)
	{
	  pred = e->src->index;
	  succ = e->dest->index;
	  index = EDGE_INDEX (elist, e->src, e->dest);
	  if (index == EDGE_INDEX_NO_EDGE)
	    {
	      fprintf (f, "*p* No index for edge from %d to %d\n", pred, succ);
	      continue;
	    }
	  if (INDEX_EDGE_PRED_BB (elist, index)->index != pred)
	    fprintf (f, "*p* Pred for index %d should be %d not %d\n",
		     index, pred, INDEX_EDGE_PRED_BB (elist, index)->index);
	  if (INDEX_EDGE_SUCC_BB (elist, index)->index != succ)
	    fprintf (f, "*p* Succ for index %d should be %d not %d\n",
		     index, succ, INDEX_EDGE_SUCC_BB (elist, index)->index);
	}
    }
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      pred = e->src->index;
      succ = e->dest->index;
      index = EDGE_INDEX (elist, e->src, e->dest);
      if (index == EDGE_INDEX_NO_EDGE)
	{
	  fprintf (f, "*p* No index for edge from %d to %d\n", pred, succ);
	  continue;
	}
      if (INDEX_EDGE_PRED_BB (elist, index)->index != pred)
	fprintf (f, "*p* Pred for index %d should be %d not %d\n",
		 index, pred, INDEX_EDGE_PRED_BB (elist, index)->index);
      if (INDEX_EDGE_SUCC_BB (elist, index)->index != succ)
	fprintf (f, "*p* Succ for index %d should be %d not %d\n",
		 index, succ, INDEX_EDGE_SUCC_BB (elist, index)->index);
    }
  /* We've verified that all the edges are in the list, no lets make sure
     there are no spurious edges in the list.  */

  for (pred = 0; pred < n_basic_blocks; pred++)
    for (succ = 0; succ < n_basic_blocks; succ++)
      {
	basic_block p = BASIC_BLOCK (pred);
	basic_block s = BASIC_BLOCK (succ);

	int found_edge = 0;

	for (e = p->succ; e; e = e->succ_next)
	  if (e->dest == s)
	    {
	      found_edge = 1;
	      break;
	    }
	for (e = s->pred; e; e = e->pred_next)
	  if (e->src == p)
	    {
	      found_edge = 1;
	      break;
	    }
	if (EDGE_INDEX (elist, BASIC_BLOCK (pred), BASIC_BLOCK (succ))
	    == EDGE_INDEX_NO_EDGE && found_edge != 0)
	  fprintf (f, "*** Edge (%d, %d) appears to not have an index\n",
		   pred, succ);
	if (EDGE_INDEX (elist, BASIC_BLOCK (pred), BASIC_BLOCK (succ))
	    != EDGE_INDEX_NO_EDGE && found_edge == 0)
	  fprintf (f, "*** Edge (%d, %d) has index %d, but there is no edge\n",
		   pred, succ, EDGE_INDEX (elist, BASIC_BLOCK (pred),
					   BASIC_BLOCK (succ)));
      }
  for (succ = 0; succ < n_basic_blocks; succ++)
    {
      basic_block p = ENTRY_BLOCK_PTR;
      basic_block s = BASIC_BLOCK (succ);

      int found_edge = 0;

      for (e = p->succ; e; e = e->succ_next)
	if (e->dest == s)
	  {
	    found_edge = 1;
	    break;
	  }
      for (e = s->pred; e; e = e->pred_next)
	if (e->src == p)
	  {
	    found_edge = 1;
	    break;
	  }
      if (EDGE_INDEX (elist, ENTRY_BLOCK_PTR, BASIC_BLOCK (succ))
	  == EDGE_INDEX_NO_EDGE && found_edge != 0)
	fprintf (f, "*** Edge (entry, %d) appears to not have an index\n",
		 succ);
      if (EDGE_INDEX (elist, ENTRY_BLOCK_PTR, BASIC_BLOCK (succ))
	  != EDGE_INDEX_NO_EDGE && found_edge == 0)
	fprintf (f, "*** Edge (entry, %d) has index %d, but no edge exists\n",
		 succ, EDGE_INDEX (elist, ENTRY_BLOCK_PTR,
				   BASIC_BLOCK (succ)));
    }
  for (pred = 0; pred < n_basic_blocks; pred++)
    {
      basic_block p = BASIC_BLOCK (pred);
      basic_block s = EXIT_BLOCK_PTR;

      int found_edge = 0;

      for (e = p->succ; e; e = e->succ_next)
	if (e->dest == s)
	  {
	    found_edge = 1;
	    break;
	  }
      for (e = s->pred; e; e = e->pred_next)
	if (e->src == p)
	  {
	    found_edge = 1;
	    break;
	  }
      if (EDGE_INDEX (elist, BASIC_BLOCK (pred), EXIT_BLOCK_PTR)
	  == EDGE_INDEX_NO_EDGE && found_edge != 0)
	fprintf (f, "*** Edge (%d, exit) appears to not have an index\n",
		 pred);
      if (EDGE_INDEX (elist, BASIC_BLOCK (pred), EXIT_BLOCK_PTR)
	  != EDGE_INDEX_NO_EDGE && found_edge == 0)
	fprintf (f, "*** Edge (%d, exit) has index %d, but no edge exists\n",
		 pred, EDGE_INDEX (elist, BASIC_BLOCK (pred),
				   EXIT_BLOCK_PTR));
    }
}

/* This routine will determine what, if any, edge there is between
   a specified predecessor and successor.  */

int
find_edge_index (edge_list, pred, succ)
     struct edge_list *edge_list;
     basic_block pred, succ;
{
  int x;
  for (x = 0; x < NUM_EDGES (edge_list); x++)
    {
      if (INDEX_EDGE_PRED_BB (edge_list, x) == pred
	  && INDEX_EDGE_SUCC_BB (edge_list, x) == succ)
	return x;
    }
  return (EDGE_INDEX_NO_EDGE);
}

/* This function will remove an edge from the flow graph.  */

void
remove_edge (e)
     edge e;
{
  edge last_pred = NULL;
  edge last_succ = NULL;
  edge tmp;
  basic_block src, dest;
  src = e->src;
  dest = e->dest;
  for (tmp = src->succ; tmp && tmp != e; tmp = tmp->succ_next)
    last_succ = tmp;

  if (!tmp)
    abort ();
  if (last_succ)
    last_succ->succ_next = e->succ_next;
  else
    src->succ = e->succ_next;

  for (tmp = dest->pred; tmp && tmp != e; tmp = tmp->pred_next)
    last_pred = tmp;

  if (!tmp)
    abort ();
  if (last_pred)
    last_pred->pred_next = e->pred_next;
  else
    dest->pred = e->pred_next;

  n_edges--;
  free (e);
}

/* This routine will remove any fake successor edges for a basic block.
   When the edge is removed, it is also removed from whatever predecessor
   list it is in.  */

static void
remove_fake_successors (bb)
     basic_block bb;
{
  edge e;
  for (e = bb->succ; e;)
    {
      edge tmp = e;
      e = e->succ_next;
      if ((tmp->flags & EDGE_FAKE) == EDGE_FAKE)
	remove_edge (tmp);
    }
}

/* This routine will remove all fake edges from the flow graph.  If
   we remove all fake successors, it will automatically remove all
   fake predecessors.  */

void
remove_fake_edges ()
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    remove_fake_successors (BASIC_BLOCK (x));

  /* We've handled all successors except the entry block's.  */
  remove_fake_successors (ENTRY_BLOCK_PTR);
}

/* This function will add a fake edge between any block which has no
   successors, and the exit block. Some data flow equations require these
   edges to exist.  */

void
add_noreturn_fake_exit_edges ()
{
  int x;

  for (x = 0; x < n_basic_blocks; x++)
    if (BASIC_BLOCK (x)->succ == NULL)
      make_edge (NULL, BASIC_BLOCK (x), EXIT_BLOCK_PTR, EDGE_FAKE);
}

/* This function adds a fake edge between any infinite loops to the
   exit block.  Some optimizations require a path from each node to
   the exit node.

   See also Morgan, Figure 3.10, pp. 82-83.

   The current implementation is ugly, not attempting to minimize the
   number of inserted fake edges.  To reduce the number of fake edges
   to insert, add fake edges from _innermost_ loops containing only
   nodes not reachable from the exit block.  */

void
connect_infinite_loops_to_exit ()
{
  basic_block unvisited_block;

  /* Perform depth-first search in the reverse graph to find nodes
     reachable from the exit block.  */
  struct depth_first_search_dsS dfs_ds;

  flow_dfs_compute_reverse_init (&dfs_ds);
  flow_dfs_compute_reverse_add_bb (&dfs_ds, EXIT_BLOCK_PTR);

  /* Repeatedly add fake edges, updating the unreachable nodes.  */
  while (1)
    {
      unvisited_block = flow_dfs_compute_reverse_execute (&dfs_ds);
      if (!unvisited_block)
	break;
      make_edge (NULL, unvisited_block, EXIT_BLOCK_PTR, EDGE_FAKE);
      flow_dfs_compute_reverse_add_bb (&dfs_ds, unvisited_block);
    }

  flow_dfs_compute_reverse_finish (&dfs_ds);

  return;
}

/* Redirect an edge's successor from one block to another.  */

void
redirect_edge_succ (e, new_succ)
     edge e;
     basic_block new_succ;
{
  edge *pe;

  /* Disconnect the edge from the old successor block.  */
  for (pe = &e->dest->pred; *pe != e; pe = &(*pe)->pred_next)
    continue;
  *pe = (*pe)->pred_next;

  /* Reconnect the edge to the new successor block.  */
  e->pred_next = new_succ->pred;
  new_succ->pred = e;
  e->dest = new_succ;
}

/* Like previous but avoid possible dupplicate edge.  */

void
redirect_edge_succ_nodup (e, new_succ)
     edge e;
     basic_block new_succ;
{
  edge s;
  /* Check whether the edge is already present.  */
  for (s = e->src->succ; s; s = s->succ_next)
    if (s->dest == new_succ && s != e)
      break;
  if (s)
    {
      s->flags |= e->flags;
      s->probability += e->probability;
      s->count += e->count;
      remove_edge (e);
    }
  else
    redirect_edge_succ (e, new_succ);
}

/* Redirect an edge's predecessor from one block to another.  */

void
redirect_edge_pred (e, new_pred)
     edge e;
     basic_block new_pred;
{
  edge *pe;

  /* Disconnect the edge from the old predecessor block.  */
  for (pe = &e->src->succ; *pe != e; pe = &(*pe)->succ_next)
    continue;
  *pe = (*pe)->succ_next;

  /* Reconnect the edge to the new predecessor block.  */
  e->succ_next = new_pred->succ;
  new_pred->succ = e;
  e->src = new_pred;
}

/* Dump the list of basic blocks in the bitmap NODES.  */

static void
flow_nodes_print (str, nodes, file)
     const char *str;
     const sbitmap nodes;
     FILE *file;
{
  int node;

  if (! nodes)
    return;

  fprintf (file, "%s { ", str);
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {fprintf (file, "%d ", node);});
  fputs ("}\n", file);
}


/* Dump the list of edges in the array EDGE_LIST.  */

static void
flow_edge_list_print (str, edge_list, num_edges, file)
     const char *str;
     const edge *edge_list;
     int num_edges;
     FILE *file;
{
  int i;

  if (! edge_list)
    return;

  fprintf (file, "%s { ", str);
  for (i = 0; i < num_edges; i++)
    fprintf (file, "%d->%d ", edge_list[i]->src->index,
	     edge_list[i]->dest->index);
  fputs ("}\n", file);
}


/* Dump loop related CFG information.  */

static void
flow_loops_cfg_dump (loops, file)
     const struct loops *loops;
     FILE *file;
{
  int i;

  if (! loops->num || ! file || ! loops->cfg.dom)
    return;

  for (i = 0; i < n_basic_blocks; i++)
    {
      edge succ;

      fprintf (file, ";; %d succs { ", i);
      for (succ = BASIC_BLOCK (i)->succ; succ; succ = succ->succ_next)
	fprintf (file, "%d ", succ->dest->index);
      flow_nodes_print ("} dom", loops->cfg.dom[i], file);
    }

  /* Dump the DFS node order.  */
  if (loops->cfg.dfs_order)
    {
      fputs (";; DFS order: ", file);
      for (i = 0; i < n_basic_blocks; i++)
	fprintf (file, "%d ", loops->cfg.dfs_order[i]);
      fputs ("\n", file);
    }
  /* Dump the reverse completion node order.  */
  if (loops->cfg.rc_order)
    {
      fputs (";; RC order: ", file);
      for (i = 0; i < n_basic_blocks; i++)
	fprintf (file, "%d ", loops->cfg.rc_order[i]);
      fputs ("\n", file);
    }
}

/* Return non-zero if the nodes of LOOP are a subset of OUTER.  */

static int
flow_loop_nested_p (outer, loop)
     struct loop *outer;
     struct loop *loop;
{
  return sbitmap_a_subset_b_p (loop->nodes, outer->nodes);
}


/* Dump the loop information specified by LOOP to the stream FILE
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */
void
flow_loop_dump (loop, file, loop_dump_aux, verbose)
     const struct loop *loop;
     FILE *file;
     void (*loop_dump_aux) PARAMS((const struct loop *, FILE *, int));
     int verbose;
{
  if (! loop || ! loop->header)
    return;

  fprintf (file, ";;\n;; Loop %d (%d to %d):%s%s\n",
	   loop->num, INSN_UID (loop->first->head),
	   INSN_UID (loop->last->end),
	   loop->shared ? " shared" : "",
	   loop->invalid ? " invalid" : "");
  fprintf (file, ";;  header %d, latch %d, pre-header %d, first %d, last %d\n",
	   loop->header->index, loop->latch->index,
	   loop->pre_header ? loop->pre_header->index : -1,
	   loop->first->index, loop->last->index);
  fprintf (file, ";;  depth %d, level %d, outer %ld\n",
	   loop->depth, loop->level,
	   (long) (loop->outer ? loop->outer->num : -1));

  if (loop->pre_header_edges)
    flow_edge_list_print (";;  pre-header edges", loop->pre_header_edges,
			  loop->num_pre_header_edges, file);
  flow_edge_list_print (";;  entry edges", loop->entry_edges,
			loop->num_entries, file);
  fprintf (file, ";;  %d", loop->num_nodes);
  flow_nodes_print (" nodes", loop->nodes, file);
  flow_edge_list_print (";;  exit edges", loop->exit_edges,
			loop->num_exits, file);
  if (loop->exits_doms)
    flow_nodes_print (";;  exit doms", loop->exits_doms, file);
  if (loop_dump_aux)
    loop_dump_aux (loop, file, verbose);
}


/* Dump the loop information specified by LOOPS to the stream FILE,
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */
void
flow_loops_dump (loops, file, loop_dump_aux, verbose)
     const struct loops *loops;
     FILE *file;
     void (*loop_dump_aux) PARAMS((const struct loop *, FILE *, int));
     int verbose;
{
  int i;
  int num_loops;

  num_loops = loops->num;
  if (! num_loops || ! file)
    return;

  fprintf (file, ";; %d loops found, %d levels\n",
	   num_loops, loops->levels);

  for (i = 0; i < num_loops; i++)
    {
      struct loop *loop = &loops->array[i];

      flow_loop_dump (loop, file, loop_dump_aux, verbose);

      if (loop->shared)
	{
	  int j;

	  for (j = 0; j < i; j++)
	    {
	      struct loop *oloop = &loops->array[j];

	      if (loop->header == oloop->header)
		{
		  int disjoint;
		  int smaller;

		  smaller = loop->num_nodes < oloop->num_nodes;

		  /* If the union of LOOP and OLOOP is different than
		     the larger of LOOP and OLOOP then LOOP and OLOOP
		     must be disjoint.  */
		  disjoint = ! flow_loop_nested_p (smaller ? loop : oloop,
						   smaller ? oloop : loop);
		  fprintf (file,
			   ";; loop header %d shared by loops %d, %d %s\n",
			   loop->header->index, i, j,
			   disjoint ? "disjoint" : "nested");
		}
	    }
	}
    }

  if (verbose)
    flow_loops_cfg_dump (loops, file);
}


/* Free all the memory allocated for LOOPS.  */

void
flow_loops_free (loops)
     struct loops *loops;
{
  if (loops->array)
    {
      int i;

      if (! loops->num)
	abort ();

      /* Free the loop descriptors.  */
      for (i = 0; i < loops->num; i++)
	{
	  struct loop *loop = &loops->array[i];

	  if (loop->pre_header_edges)
	    free (loop->pre_header_edges);
	  if (loop->nodes)
	    sbitmap_free (loop->nodes);
	  if (loop->entry_edges)
	    free (loop->entry_edges);
	  if (loop->exit_edges)
	    free (loop->exit_edges);
	  if (loop->exits_doms)
	    sbitmap_free (loop->exits_doms);
	}
      free (loops->array);
      loops->array = NULL;

      if (loops->cfg.dom)
	sbitmap_vector_free (loops->cfg.dom);
      if (loops->cfg.dfs_order)
	free (loops->cfg.dfs_order);

      if (loops->shared_headers)
	sbitmap_free (loops->shared_headers);
    }
}


/* Find the entry edges into the loop with header HEADER and nodes
   NODES and store in ENTRY_EDGES array.  Return the number of entry
   edges from the loop.  */

static int
flow_loop_entry_edges_find (header, nodes, entry_edges)
     basic_block header;
     const sbitmap nodes;
     edge **entry_edges;
{
  edge e;
  int num_entries;

  *entry_edges = NULL;

  num_entries = 0;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block src = e->src;

      if (src == ENTRY_BLOCK_PTR || ! TEST_BIT (nodes, src->index))
	num_entries++;
    }

  if (! num_entries)
    abort ();

  *entry_edges = (edge *) xmalloc (num_entries * sizeof (edge *));

  num_entries = 0;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block src = e->src;

      if (src == ENTRY_BLOCK_PTR || ! TEST_BIT (nodes, src->index))
	(*entry_edges)[num_entries++] = e;
    }

  return num_entries;
}


/* Find the exit edges from the loop using the bitmap of loop nodes
   NODES and store in EXIT_EDGES array.  Return the number of
   exit edges from the loop.  */

static int
flow_loop_exit_edges_find (nodes, exit_edges)
     const sbitmap nodes;
     edge **exit_edges;
{
  edge e;
  int node;
  int num_exits;

  *exit_edges = NULL;

  /* Check all nodes within the loop to see if there are any
     successors not in the loop.  Note that a node may have multiple
     exiting edges ?????  A node can have one jumping edge and one fallthru
     edge so only one of these can exit the loop.  */
  num_exits = 0;
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {
    for (e = BASIC_BLOCK (node)->succ; e; e = e->succ_next)
      {
	basic_block dest = e->dest;

	if (dest == EXIT_BLOCK_PTR || ! TEST_BIT (nodes, dest->index))
	    num_exits++;
      }
  });

  if (! num_exits)
    return 0;

  *exit_edges = (edge *) xmalloc (num_exits * sizeof (edge *));

  /* Store all exiting edges into an array.  */
  num_exits = 0;
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {
    for (e = BASIC_BLOCK (node)->succ; e; e = e->succ_next)
      {
	basic_block dest = e->dest;

	if (dest == EXIT_BLOCK_PTR || ! TEST_BIT (nodes, dest->index))
	  (*exit_edges)[num_exits++] = e;
      }
  });

  return num_exits;
}


/* Find the nodes contained within the loop with header HEADER and
   latch LATCH and store in NODES.  Return the number of nodes within
   the loop.  */

static int
flow_loop_nodes_find (header, latch, nodes)
     basic_block header;
     basic_block latch;
     sbitmap nodes;
{
  basic_block *stack;
  int sp;
  int num_nodes = 0;

  stack = (basic_block *) xmalloc (n_basic_blocks * sizeof (basic_block));
  sp = 0;

  /* Start with only the loop header in the set of loop nodes.  */
  sbitmap_zero (nodes);
  SET_BIT (nodes, header->index);
  num_nodes++;
  header->loop_depth++;

  /* Push the loop latch on to the stack.  */
  if (! TEST_BIT (nodes, latch->index))
    {
      SET_BIT (nodes, latch->index);
      latch->loop_depth++;
      num_nodes++;
      stack[sp++] = latch;
    }

  while (sp)
    {
      basic_block node;
      edge e;

      node = stack[--sp];
      for (e = node->pred; e; e = e->pred_next)
	{
	  basic_block ancestor = e->src;

	  /* If each ancestor not marked as part of loop, add to set of
	     loop nodes and push on to stack.  */
	  if (ancestor != ENTRY_BLOCK_PTR
	      && ! TEST_BIT (nodes, ancestor->index))
	    {
	      SET_BIT (nodes, ancestor->index);
	      ancestor->loop_depth++;
	      num_nodes++;
	      stack[sp++] = ancestor;
	    }
	}
    }
  free (stack);
  return num_nodes;
}

/* Compute the depth first search order and store in the array
  DFS_ORDER if non-zero, marking the nodes visited in VISITED.  If
  RC_ORDER is non-zero, return the reverse completion number for each
  node.  Returns the number of nodes visited.  A depth first search
  tries to get as far away from the starting point as quickly as
  possible.  */

int
flow_depth_first_order_compute (dfs_order, rc_order)
     int *dfs_order;
     int *rc_order;
{
  edge *stack;
  int sp;
  int dfsnum = 0;
  int rcnum = n_basic_blocks - 1;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc ((n_basic_blocks + 1) * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);

  /* Push the first edge on to the stack.  */
  stack[sp++] = ENTRY_BLOCK_PTR->succ;

  while (sp)
    {
      edge e;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      src = e->src;
      dest = e->dest;

      /* Check if the edge destination has been visited yet.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index))
	{
	  /* Mark that we have visited the destination.  */
	  SET_BIT (visited, dest->index);

	  if (dfs_order)
	    dfs_order[dfsnum++] = dest->index;

	  if (dest->succ)
	    {
	      /* Since the DEST node has been visited for the first
		 time, check its successors.  */
	      stack[sp++] = dest->succ;
	    }
	  else
	    {
	      /* There are no successors for the DEST node so assign
		 its reverse completion number.  */
	      if (rc_order)
		rc_order[rcnum--] = dest->index;
	    }
	}
      else
	{
	  if (! e->succ_next && src != ENTRY_BLOCK_PTR)
	    {
	      /* There are no more successors for the SRC node
		 so assign its reverse completion number.  */
	      if (rc_order)
		rc_order[rcnum--] = src->index;
	    }

	  if (e->succ_next)
	    stack[sp - 1] = e->succ_next;
	  else
	    sp--;
	}
    }

  free (stack);
  sbitmap_free (visited);

  /* The number of nodes visited should not be greater than
     n_basic_blocks.  */
  if (dfsnum > n_basic_blocks)
    abort ();

  /* There are some nodes left in the CFG that are unreachable.  */
  if (dfsnum < n_basic_blocks)
    abort ();
  return dfsnum;
}

/* Compute the depth first search order on the _reverse_ graph and
   store in the array DFS_ORDER, marking the nodes visited in VISITED.
   Returns the number of nodes visited.

   The computation is split into three pieces:

   flow_dfs_compute_reverse_init () creates the necessary data
   structures.

   flow_dfs_compute_reverse_add_bb () adds a basic block to the data
   structures.  The block will start the search.

   flow_dfs_compute_reverse_execute () continues (or starts) the
   search using the block on the top of the stack, stopping when the
   stack is empty.

   flow_dfs_compute_reverse_finish () destroys the necessary data
   structures.

   Thus, the user will probably call ..._init(), call ..._add_bb() to
   add a beginning basic block to the stack, call ..._execute(),
   possibly add another bb to the stack and again call ..._execute(),
   ..., and finally call _finish().  */

/* Initialize the data structures used for depth-first search on the
   reverse graph.  If INITIALIZE_STACK is nonzero, the exit block is
   added to the basic block stack.  DATA is the current depth-first
   search context.  If INITIALIZE_STACK is non-zero, there is an
   element on the stack.  */

static void
flow_dfs_compute_reverse_init (data)
     depth_first_search_ds data;
{
  /* Allocate stack for back-tracking up CFG.  */
  data->stack =
    (basic_block *) xmalloc ((n_basic_blocks - (INVALID_BLOCK + 1))
			     * sizeof (basic_block));
  data->sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  data->visited_blocks = sbitmap_alloc (n_basic_blocks - (INVALID_BLOCK + 1));

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (data->visited_blocks);

  return;
}

/* Add the specified basic block to the top of the dfs data
   structures.  When the search continues, it will start at the
   block.  */

static void
flow_dfs_compute_reverse_add_bb (data, bb)
     depth_first_search_ds data;
     basic_block bb;
{
  data->stack[data->sp++] = bb;
  return;
}

/* Continue the depth-first search through the reverse graph starting
   with the block at the stack's top and ending when the stack is
   empty.  Visited nodes are marked.  Returns an unvisited basic
   block, or NULL if there is none available.  */

static basic_block
flow_dfs_compute_reverse_execute (data)
     depth_first_search_ds data;
{
  basic_block bb;
  edge e;
  int i;

  while (data->sp > 0)
    {
      bb = data->stack[--data->sp];

      /* Mark that we have visited this node.  */
      if (!TEST_BIT (data->visited_blocks, bb->index - (INVALID_BLOCK + 1)))
	{
	  SET_BIT (data->visited_blocks, bb->index - (INVALID_BLOCK + 1));

	  /* Perform depth-first search on adjacent vertices.  */
	  for (e = bb->pred; e; e = e->pred_next)
	    flow_dfs_compute_reverse_add_bb (data, e->src);
	}
    }

  /* Determine if there are unvisited basic blocks.  */
  for (i = n_basic_blocks - (INVALID_BLOCK + 1); --i >= 0;)
    if (!TEST_BIT (data->visited_blocks, i))
      return BASIC_BLOCK (i + (INVALID_BLOCK + 1));
  return NULL;
}

/* Destroy the data structures needed for depth-first search on the
   reverse graph.  */

static void
flow_dfs_compute_reverse_finish (data)
     depth_first_search_ds data;
{
  free (data->stack);
  sbitmap_free (data->visited_blocks);
  return;
}


/* Find the root node of the loop pre-header extended basic block and
   the edges along the trace from the root node to the loop header.  */

static void
flow_loop_pre_header_scan (loop)
     struct loop *loop;
{
  int num = 0;
  basic_block ebb;

  loop->num_pre_header_edges = 0;

  if (loop->num_entries != 1)
     return;

  ebb = loop->entry_edges[0]->src;

  if (ebb != ENTRY_BLOCK_PTR)
    {
      edge e;

      /* Count number of edges along trace from loop header to
	 root of pre-header extended basic block.  Usually this is
	 only one or two edges.  */
      num++;
      while (ebb->pred->src != ENTRY_BLOCK_PTR && ! ebb->pred->pred_next)
	{
	  ebb = ebb->pred->src;
	  num++;
	}

      loop->pre_header_edges = (edge *) xmalloc (num * sizeof (edge *));
      loop->num_pre_header_edges = num;

      /* Store edges in order that they are followed.   The source
	 of the first edge is the root node of the pre-header extended
	 basic block and the destination of the last last edge is
	 the loop header.  */
      for (e = loop->entry_edges[0]; num; e = e->src->pred)
	{
	  loop->pre_header_edges[--num] = e;
	}
    }
}


/* Return the block for the pre-header of the loop with header
   HEADER where DOM specifies the dominator information.  Return NULL if
   there is no pre-header.  */

static basic_block
flow_loop_pre_header_find (header, dom)
     basic_block header;
     const sbitmap *dom;
{
  basic_block pre_header;
  edge e;

  /* If block p is a predecessor of the header and is the only block
     that the header does not dominate, then it is the pre-header.  */
  pre_header = NULL;
  for (e = header->pred; e; e = e->pred_next)
    {
      basic_block node = e->src;

      if (node != ENTRY_BLOCK_PTR
	  && ! TEST_BIT (dom[node->index], header->index))
	{
	  if (pre_header == NULL)
	    pre_header = node;
	  else
	    {
	      /* There are multiple edges into the header from outside
		 the loop so there is no pre-header block.  */
	      pre_header = NULL;
	      break;
	    }
	}
    }
  return pre_header;
}

/* Add LOOP to the loop hierarchy tree where PREVLOOP was the loop
   previously added.  The insertion algorithm assumes that the loops
   are added in the order found by a depth first search of the CFG.  */

static void
flow_loop_tree_node_add (prevloop, loop)
     struct loop *prevloop;
     struct loop *loop;
{

  if (flow_loop_nested_p (prevloop, loop))
    {
      prevloop->inner = loop;
      loop->outer = prevloop;
      return;
    }

  while (prevloop->outer)
    {
      if (flow_loop_nested_p (prevloop->outer, loop))
	{
	  prevloop->next = loop;
	  loop->outer = prevloop->outer;
	  return;
	}
      prevloop = prevloop->outer;
    }

  prevloop->next = loop;
  loop->outer = NULL;
}

/* Build the loop hierarchy tree for LOOPS.  */

static void
flow_loops_tree_build (loops)
     struct loops *loops;
{
  int i;
  int num_loops;

  num_loops = loops->num;
  if (! num_loops)
    return;

  /* Root the loop hierarchy tree with the first loop found.
     Since we used a depth first search this should be the
     outermost loop.  */
  loops->tree_root = &loops->array[0];
  loops->tree_root->outer = loops->tree_root->inner = loops->tree_root->next = NULL;

  /* Add the remaining loops to the tree.  */
  for (i = 1; i < num_loops; i++)
    flow_loop_tree_node_add (&loops->array[i - 1], &loops->array[i]);
}

/* Helper function to compute loop nesting depth and enclosed loop level
   for the natural loop specified by LOOP at the loop depth DEPTH.
   Returns the loop level.  */

static int
flow_loop_level_compute (loop, depth)
     struct loop *loop;
     int depth;
{
  struct loop *inner;
  int level = 1;

  if (! loop)
    return 0;

  /* Traverse loop tree assigning depth and computing level as the
     maximum level of all the inner loops of this loop.  The loop
     level is equivalent to the height of the loop in the loop tree
     and corresponds to the number of enclosed loop levels (including
     itself).  */
  for (inner = loop->inner; inner; inner = inner->next)
    {
      int ilevel;

      ilevel = flow_loop_level_compute (inner, depth + 1) + 1;

      if (ilevel > level)
	level = ilevel;
    }
  loop->level = level;
  loop->depth = depth;
  return level;
}

/* Compute the loop nesting depth and enclosed loop level for the loop
   hierarchy tree specfied by LOOPS.  Return the maximum enclosed loop
   level.  */

static int
flow_loops_level_compute (loops)
     struct loops *loops;
{
  struct loop *loop;
  int level;
  int levels = 0;

  /* Traverse all the outer level loops.  */
  for (loop = loops->tree_root; loop; loop = loop->next)
    {
      level = flow_loop_level_compute (loop, 1);
      if (level > levels)
	levels = level;
    }
  return levels;
}


/* Scan a single natural loop specified by LOOP collecting information
   about it specified by FLAGS.  */

int
flow_loop_scan (loops, loop, flags)
     struct loops *loops;
     struct loop *loop;
     int flags;
{
  /* Determine prerequisites.  */
  if ((flags & LOOP_EXITS_DOMS) && ! loop->exit_edges)
    flags |= LOOP_EXIT_EDGES;

  if (flags & LOOP_ENTRY_EDGES)
    {
      /* Find edges which enter the loop header.
	 Note that the entry edges should only
	 enter the header of a natural loop.  */
      loop->num_entries
	= flow_loop_entry_edges_find (loop->header,
				      loop->nodes,
				      &loop->entry_edges);
    }

  if (flags & LOOP_EXIT_EDGES)
    {
      /* Find edges which exit the loop.  */
      loop->num_exits
	= flow_loop_exit_edges_find (loop->nodes,
				     &loop->exit_edges);
    }

  if (flags & LOOP_EXITS_DOMS)
    {
      int j;

      /* Determine which loop nodes dominate all the exits
	 of the loop.  */
      loop->exits_doms = sbitmap_alloc (n_basic_blocks);
      sbitmap_copy (loop->exits_doms, loop->nodes);
      for (j = 0; j < loop->num_exits; j++)
	sbitmap_a_and_b (loop->exits_doms, loop->exits_doms,
			 loops->cfg.dom[loop->exit_edges[j]->src->index]);

      /* The header of a natural loop must dominate
	 all exits.  */
      if (! TEST_BIT (loop->exits_doms, loop->header->index))
	abort ();
    }

  if (flags & LOOP_PRE_HEADER)
    {
      /* Look to see if the loop has a pre-header node.  */
      loop->pre_header
	= flow_loop_pre_header_find (loop->header, loops->cfg.dom);

      /* Find the blocks within the extended basic block of
	 the loop pre-header.  */
      flow_loop_pre_header_scan (loop);
    }
  return 1;
}


/* Find all the natural loops in the function and save in LOOPS structure
   and recalculate loop_depth information in basic block structures.
   FLAGS controls which loop information is collected.
   Return the number of natural loops found.  */

int
flow_loops_find (loops, flags)
     struct loops *loops;
     int flags;
{
  int i;
  int b;
  int num_loops;
  edge e;
  sbitmap headers;
  sbitmap *dom;
  int *dfs_order;
  int *rc_order;

  /* This function cannot be repeatedly called with different
     flags to build up the loop information.  The loop tree
     must always be built if this function is called.  */
  if (! (flags & LOOP_TREE))
    abort ();

  memset (loops, 0, sizeof (*loops));

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return 0;

  dfs_order = NULL;
  rc_order = NULL;

  /* Compute the dominators.  */
  dom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  calculate_dominance_info (NULL, dom, CDI_DOMINATORS);

  /* Count the number of loop edges (back edges).  This should be the
     same as the number of natural loops.  */

  num_loops = 0;
  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block header;

      header = BASIC_BLOCK (b);
      header->loop_depth = 0;

      for (e = header->pred; e; e = e->pred_next)
	{
	  basic_block latch = e->src;

	  /* Look for back edges where a predecessor is dominated
	     by this block.  A natural loop has a single entry
	     node (header) that dominates all the nodes in the
	     loop.  It also has single back edge to the header
	     from a latch node.  Note that multiple natural loops
	     may share the same header.  */
	  if (b != header->index)
	    abort ();

	  if (latch != ENTRY_BLOCK_PTR && TEST_BIT (dom[latch->index], b))
	    num_loops++;
	}
    }

  if (num_loops)
    {
      /* Compute depth first search order of the CFG so that outer
	 natural loops will be found before inner natural loops.  */
      dfs_order = (int *) xmalloc (n_basic_blocks * sizeof (int));
      rc_order = (int *) xmalloc (n_basic_blocks * sizeof (int));
      flow_depth_first_order_compute (dfs_order, rc_order);

      /* Save CFG derived information to avoid recomputing it.  */
      loops->cfg.dom = dom;
      loops->cfg.dfs_order = dfs_order;
      loops->cfg.rc_order = rc_order;

      /* Allocate loop structures.  */
      loops->array
	= (struct loop *) xcalloc (num_loops, sizeof (struct loop));

      headers = sbitmap_alloc (n_basic_blocks);
      sbitmap_zero (headers);

      loops->shared_headers = sbitmap_alloc (n_basic_blocks);
      sbitmap_zero (loops->shared_headers);

      /* Find and record information about all the natural loops
	 in the CFG.  */
      num_loops = 0;
      for (b = 0; b < n_basic_blocks; b++)
	{
	  basic_block header;

	  /* Search the nodes of the CFG in reverse completion order
	     so that we can find outer loops first.  */
	  header = BASIC_BLOCK (rc_order[b]);

	  /* Look for all the possible latch blocks for this header.  */
	  for (e = header->pred; e; e = e->pred_next)
	    {
	      basic_block latch = e->src;

	      /* Look for back edges where a predecessor is dominated
		 by this block.  A natural loop has a single entry
		 node (header) that dominates all the nodes in the
		 loop.  It also has single back edge to the header
		 from a latch node.  Note that multiple natural loops
		 may share the same header.  */
	      if (latch != ENTRY_BLOCK_PTR
		  && TEST_BIT (dom[latch->index], header->index))
		{
		  struct loop *loop;

		  loop = loops->array + num_loops;

		  loop->header = header;
		  loop->latch = latch;
		  loop->num = num_loops;

		  num_loops++;
		}
	    }
	}

      for (i = 0; i < num_loops; i++)
	{
	  struct loop *loop = &loops->array[i];

	  /* Keep track of blocks that are loop headers so
	     that we can tell which loops should be merged.  */
	  if (TEST_BIT (headers, loop->header->index))
	    SET_BIT (loops->shared_headers, loop->header->index);
	  SET_BIT (headers, loop->header->index);

	  /* Find nodes contained within the loop.  */
	  loop->nodes = sbitmap_alloc (n_basic_blocks);
	  loop->num_nodes
	    = flow_loop_nodes_find (loop->header, loop->latch, loop->nodes);

	  /* Compute first and last blocks within the loop.
	     These are often the same as the loop header and
	     loop latch respectively, but this is not always
	     the case.  */
	  loop->first
	    = BASIC_BLOCK (sbitmap_first_set_bit (loop->nodes));
	  loop->last
	    = BASIC_BLOCK (sbitmap_last_set_bit (loop->nodes));

	  flow_loop_scan (loops, loop, flags);
	}

      /* Natural loops with shared headers may either be disjoint or
	 nested.  Disjoint loops with shared headers cannot be inner
	 loops and should be merged.  For now just mark loops that share
	 headers.  */
      for (i = 0; i < num_loops; i++)
	if (TEST_BIT (loops->shared_headers, loops->array[i].header->index))
	  loops->array[i].shared = 1;

      sbitmap_free (headers);
    }
  else
    {
      sbitmap_vector_free (dom);
    }

  loops->num = num_loops;

  /* Build the loop hierarchy tree.  */
  flow_loops_tree_build (loops);

  /* Assign the loop nesting depth and enclosed loop level for each
     loop.  */
  loops->levels = flow_loops_level_compute (loops);

  return num_loops;
}


/* Update the information regarding the loops in the CFG
   specified by LOOPS.  */
int
flow_loops_update (loops, flags)
     struct loops *loops;
     int flags;
{
  /* One day we may want to update the current loop data.  For now
     throw away the old stuff and rebuild what we need.  */
  if (loops->array)
    flow_loops_free (loops);

  return flow_loops_find (loops, flags);
}


/* Return non-zero if edge E enters header of LOOP from outside of LOOP.  */

int
flow_loop_outside_edge_p (loop, e)
     const struct loop *loop;
     edge e;
{
  if (e->dest != loop->header)
    abort ();
  return (e->src == ENTRY_BLOCK_PTR)
    || ! TEST_BIT (loop->nodes, e->src->index);
}

/* Clear LOG_LINKS fields of insns in a chain.
   Also clear the global_live_at_{start,end} fields of the basic block
   structures.  */

void
clear_log_links (insns)
     rtx insns;
{
  rtx i;
  int b;

  for (i = insns; i; i = NEXT_INSN (i))
    if (INSN_P (i))
      LOG_LINKS (i) = 0;

  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block bb = BASIC_BLOCK (b);

      bb->global_live_at_start = NULL;
      bb->global_live_at_end = NULL;
    }

  ENTRY_BLOCK_PTR->global_live_at_end = NULL;
  EXIT_BLOCK_PTR->global_live_at_start = NULL;
}

/* Given a register bitmap, turn on the bits in a HARD_REG_SET that
   correspond to the hard registers, if any, set in that map.  This
   could be done far more efficiently by having all sorts of special-cases
   with moving single words, but probably isn't worth the trouble.  */

void
reg_set_to_hard_reg_set (to, from)
     HARD_REG_SET *to;
     bitmap from;
{
  int i;

  EXECUTE_IF_SET_IN_BITMAP
    (from, 0, i,
     {
       if (i >= FIRST_PSEUDO_REGISTER)
	 return;
       SET_HARD_REG_BIT (*to, i);
     });
}

/* Called once at intialization time.  */

void
init_flow ()
{
  static int initialized;

  if (!initialized)
    {
      gcc_obstack_init (&flow_obstack);
      flow_firstobj = (char *) obstack_alloc (&flow_obstack, 0);
      initialized = 1;
    }
  else
    {
      obstack_free (&flow_obstack, flow_firstobj);
      flow_firstobj = (char *) obstack_alloc (&flow_obstack, 0);
    }
}

/* Assume that the preceeding pass has possibly eliminated jump instructions
   or converted the unconditional jumps.  Eliminate the edges from CFG.
   Return true if any edges are eliminated.  */

bool
purge_dead_edges (bb)
     basic_block bb;
{
  edge e, next;
  rtx insn = bb->end;
  bool purged = false;

  if (GET_CODE (insn) == JUMP_INSN && !simplejump_p (insn))
    return false;
  if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx note;
      edge b,f;
      /* We do care only about conditional jumps and simplejumps.  */
      if (!any_condjump_p (insn)
	  && !returnjump_p (insn)
	  && !simplejump_p (insn))
	return false;
      for (e = bb->succ; e; e = next)
	{
	  next = e->succ_next;

	  /* Check purposes we can have edge.  */
	  if ((e->flags & EDGE_FALLTHRU)
	      && any_condjump_p (insn))
	    continue;
	  if (e->dest != EXIT_BLOCK_PTR
	      && e->dest->head == JUMP_LABEL (insn))
	    continue;
	  if (e->dest == EXIT_BLOCK_PTR
	      && returnjump_p (insn))
	    continue;
	  purged = true;
	  remove_edge (e);
	}
      if (!bb->succ || !purged)
	return false;
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Purged edges from bb %i\n", bb->index);
      if (!optimize)
	return purged;

      /* Redistribute probabilities.  */
      if (!bb->succ->succ_next)
	{
	  bb->succ->probability = REG_BR_PROB_BASE;
	  bb->succ->count = bb->count;
        }
      else
	{
	  note = find_reg_note (insn, REG_BR_PROB, NULL);
	  if (!note)
	    return purged;
	  b = BRANCH_EDGE (bb);
	  f = FALLTHRU_EDGE (bb);
	  b->probability = INTVAL (XEXP (note, 0));
	  f->probability = REG_BR_PROB_BASE - b->probability;
	  b->count = bb->count * b->probability / REG_BR_PROB_BASE;
	  f->count = bb->count * f->probability / REG_BR_PROB_BASE;
	}
      return purged;
    }

  /* Cleanup abnormal edges caused by throwing insns that have been
     eliminated.  */
  if (! can_throw_internal (bb->end))
    for (e = bb->succ; e; e = next)
      {
	next = e->succ_next;
	if (e->flags & EDGE_EH)
	  {
	    remove_edge (e);
	    purged = true;
	  }
      }

  /* If we don't see a jump insn, we don't know exactly why the block would
     have been broken at this point.  Look for a simple, non-fallthru edge,
     as these are only created by conditional branches.  If we find such an
     edge we know that there used to be a jump here and can then safely
     remove all non-fallthru edges.  */
  for (e = bb->succ; e && (e->flags & (EDGE_COMPLEX | EDGE_FALLTHRU));
       e = e->succ_next);
  if (!e)
    return purged;
  for (e = bb->succ; e; e = next)
    {
      next = e->succ_next;
      if (!(e->flags & EDGE_FALLTHRU))
	remove_edge (e), purged = true;
    }
  if (!bb->succ || bb->succ->succ_next)
    abort ();
  bb->succ->probability = REG_BR_PROB_BASE;
  bb->succ->count = bb->count;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Purged non-fallthru edges from bb %i\n",
	     bb->index);
  return purged;
}

/* Search all basic blocks for potentionally dead edges and purge them.

   Return true ifif some edge has been elliminated.
 */

bool
purge_all_dead_edges ()
{
  int i, purged = false;
  for (i = 0; i < n_basic_blocks; i++)
    purged |= purge_dead_edges (BASIC_BLOCK (i));
  return purged;
}
