/* Data flow analysis for GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 
   1999, 2000 Free Software Foundation, Inc.

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
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "insn-flags.h"
#include "expr.h"

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

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.
   For top-level functions, this is temporary_obstack.
   Separate obstacks are made for nested functions.  */

extern struct obstack *function_obstack;

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
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    ENTRY_BLOCK,		/* index */
    0,				/* loop_depth */
    -1, -1			/* eh_beg, eh_end */
  },
  {
    NULL,			/* head */
    NULL,			/* end */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    EXIT_BLOCK,			/* index */
    0,				/* loop_depth */
    -1, -1			/* eh_beg, eh_end */
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

/* Set of registers that may be eliminable.  These are handled specially
   in updating regs_ever_live.  */

static HARD_REG_SET elim_reg_set;

/* The basic block structure for every insn, indexed by uid.  */

varray_type basic_block_for_insn;

/* The labels mentioned in non-jump rtl.  Valid during find_basic_blocks.  */
/* ??? Should probably be using LABEL_NUSES instead.  It would take a 
   bit of surgery to be able to use or co-opt the routines in jump.  */

static rtx label_value_list;

/* Holds information for tracking conditional register life information.  */
struct reg_cond_life_info
{
  /* An EXPR_LIST of conditions under which a register is dead.  */
  rtx condition;

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

  /* If non-null, record the set of registers set in the basic block.  */
  regset local_set;

#ifdef HAVE_conditional_execution
  /* Indexed by register number, holds a reg_cond_life_info for each
     register that is not unconditionally live or dead.  */
  splay_tree reg_cond_dead;

  /* Bit N is set if register N is in an expression in reg_cond_dead.  */
  regset reg_cond_reg;
#endif

  /* Non-zero if the value of CC0 is live.  */
  int cc0_live;

  /* Flags controling the set of information propagate_block collects.  */
  int flags;
};

/* Forward declarations */
static int count_basic_blocks		PARAMS ((rtx));
static rtx find_basic_blocks_1		PARAMS ((rtx));
static void clear_edges			PARAMS ((void));
static void make_edges			PARAMS ((rtx));
static void make_label_edge		PARAMS ((sbitmap *, basic_block,
						 rtx, int));
static void make_eh_edge		PARAMS ((sbitmap *, eh_nesting_info *,
						 basic_block, rtx, int));
static void mark_critical_edges		PARAMS ((void));
static void move_stray_eh_region_notes	PARAMS ((void));
static void record_active_eh_regions	PARAMS ((rtx));

static void commit_one_edge_insertion	PARAMS ((edge));

static void delete_unreachable_blocks	PARAMS ((void));
static void delete_eh_regions		PARAMS ((void));
static int can_delete_note_p		PARAMS ((rtx));
static void expunge_block		PARAMS ((basic_block));
static int can_delete_label_p		PARAMS ((rtx));
static int merge_blocks_move_predecessor_nojumps PARAMS ((basic_block,
							  basic_block));
static int merge_blocks_move_successor_nojumps PARAMS ((basic_block,
							basic_block));
static int merge_blocks			PARAMS ((edge,basic_block,basic_block));
static void try_merge_blocks		PARAMS ((void));
static void tidy_fallthru_edges		PARAMS ((void));
static int verify_wide_reg_1		PARAMS ((rtx *, void *));
static void verify_wide_reg		PARAMS ((int, rtx, rtx));
static void verify_local_live_at_start	PARAMS ((regset, basic_block));
static int set_noop_p			PARAMS ((rtx));
static int noop_move_p			PARAMS ((rtx));
static void delete_noop_moves		PARAMS ((rtx));
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
						 rtx, rtx, rtx));
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
static rtx ior_reg_cond			PARAMS ((rtx, rtx));
static rtx not_reg_cond			PARAMS ((rtx));
static rtx nand_reg_cond		PARAMS ((rtx, rtx));
#endif
#ifdef AUTO_INC_DEC
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
static void dump_edge_info		PARAMS ((FILE *, edge, int));

static void invalidate_mems_from_autoinc PARAMS ((struct propagate_block_info *,
						  rtx));
static void remove_fake_successors	PARAMS ((basic_block));
static void flow_nodes_print	PARAMS ((const char *, const sbitmap, FILE *));
static void flow_exits_print PARAMS ((const char *, const edge *, int, FILE *));
static void flow_loops_cfg_dump		PARAMS ((const struct loops *, FILE *));
static int flow_loop_nested_p		PARAMS ((struct loop *, struct loop *));
static int flow_loop_exits_find		PARAMS ((const sbitmap, edge **));
static int flow_loop_nodes_find	PARAMS ((basic_block, basic_block, sbitmap));
static int flow_depth_first_order_compute PARAMS ((int *));
static basic_block flow_loop_pre_header_find PARAMS ((basic_block, const sbitmap *));
static void flow_loop_tree_node_add	PARAMS ((struct loop *, struct loop *));
static void flow_loops_tree_build	PARAMS ((struct loops *));
static int flow_loop_level_compute	PARAMS ((struct loop *, int));
static int flow_loops_level_compute	PARAMS ((struct loops *));

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

  label_value_list = find_basic_blocks_1 (f);
  
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
  record_active_eh_regions (f);
  make_edges (label_value_list);

  /* Do very simple cleanup now, for the benefit of code that runs between
     here and cleanup_cfg, e.g. thread_prologue_and_epilogue_insns.  */
  tidy_fallthru_edges ();

  mark_critical_edges ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Count the basic blocks of the function.  */

static int 
count_basic_blocks (f)
     rtx f;
{
  register rtx insn;
  register RTX_CODE prev_code;
  register int count = 0;
  int eh_region = 0;
  int call_had_abnormal_edge = 0;

  prev_code = JUMP_INSN;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      register RTX_CODE code = GET_CODE (insn);

      if (code == CODE_LABEL
	  || (GET_RTX_CLASS (code) == 'i'
	      && (prev_code == JUMP_INSN
		  || prev_code == BARRIER
		  || (prev_code == CALL_INSN && call_had_abnormal_edge))))
	count++;

      /* Record whether this call created an edge.  */
      if (code == CALL_INSN)
	{
	  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	  int region = (note ? INTVAL (XEXP (note, 0)) : 1);

	  call_had_abnormal_edge = 0;

	  /* If there is an EH region or rethrow, we have an edge.  */
	  if ((eh_region && region > 0)
	      || find_reg_note (insn, REG_EH_RETHROW, NULL_RTX))
	    call_had_abnormal_edge = 1;
	  else if (nonlocal_goto_handler_labels && region >= 0)
	    /* If there is a nonlocal goto label and the specified
	       region number isn't -1, we have an edge. (0 means
	       no throw, but might have a nonlocal goto).  */
	    call_had_abnormal_edge = 1;
	}

      if (code != NOTE)
	prev_code = code;
      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
	++eh_region;
      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
	--eh_region;
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

/* Find all basic blocks of the function whose first insn is F.

   Collect and return a list of labels whose addresses are taken.  This
   will be used in make_edges for use with computed gotos.  */

static rtx
find_basic_blocks_1 (f)
     rtx f;
{
  register rtx insn, next;
  int i = 0;
  rtx bb_note = NULL_RTX;
  rtx eh_list = NULL_RTX;
  rtx label_value_list = NULL_RTX;
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

	    /* Keep a LIFO list of the currently active exception notes.  */
	    if (kind == NOTE_INSN_EH_REGION_BEG)
	      eh_list = alloc_INSN_LIST (insn, eh_list);
	    else if (kind == NOTE_INSN_EH_REGION_END)
	      {
		rtx t = eh_list;

		eh_list = XEXP (eh_list, 1);
		free_INSN_LIST_node (t);
	      }

	    /* Look for basic block notes with which to keep the 
	       basic_block_info pointers stable.  Unthread the note now;
	       we'll put it back at the right place in create_basic_block.
	       Or not at all if we've already found a note in this block.  */
	    else if (kind == NOTE_INSN_BASIC_BLOCK)
	      {
		if (bb_note == NULL_RTX)
		  bb_note = insn;

		next = flow_delete_insn (insn);
	      }
	    break;
	  }

	case CODE_LABEL:
	  /* A basic block starts at a label.  If we've closed one off due 
	     to a barrier or some such, no need to do it again.  */
	  if (head != NULL_RTX)
	    {
	      /* While we now have edge lists with which other portions of
		 the compiler might determine a call ending a basic block
		 does not imply an abnormal edge, it will be a bit before
		 everything can be updated.  So continue to emit a noop at
		 the end of such a block.  */
	      if (GET_CODE (end) == CALL_INSN && ! SIBLING_CALL_P (end))
		{
		  rtx nop = gen_rtx_USE (VOIDmode, const0_rtx);
		  end = emit_insn_after (nop, end);
		}

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

	  /* While we now have edge lists with which other portions of the
	     compiler might determine a call ending a basic block does not
	     imply an abnormal edge, it will be a bit before everything can
	     be updated.  So continue to emit a noop at the end of such a
	     block.  */
	  if (GET_CODE (end) == CALL_INSN && ! SIBLING_CALL_P (end))
	    {
	      rtx nop = gen_rtx_USE (VOIDmode, const0_rtx);
	      end = emit_insn_after (nop, end);
	    }
	  goto new_bb_exclusive;

	case CALL_INSN:
	  {
	    /* Record whether this call created an edge.  */
	    rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	    int region = (note ? INTVAL (XEXP (note, 0)) : 1);
	    int call_has_abnormal_edge = 0;

	    /* If there is an EH region or rethrow, we have an edge.  */
	    if ((eh_list && region > 0)
		|| find_reg_note (insn, REG_EH_RETHROW, NULL_RTX))
	      call_has_abnormal_edge = 1;
	    else if (nonlocal_goto_handler_labels && region >= 0)
	      /* If there is a nonlocal goto label and the specified
		 region number isn't -1, we have an edge. (0 means
		 no throw, but might have a nonlocal goto).  */
	      call_has_abnormal_edge = 1;

	    /* A basic block ends at a call that can either throw or
	       do a non-local goto.  */
	    if (call_has_abnormal_edge)
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
	  /* FALLTHRU */

	default:
	  if (GET_RTX_CLASS (code) == 'i')
	    {
	      if (head == NULL_RTX)
		head = insn;
	      end = insn;
	    }
	  break;
	}

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx note;

	  /* Make a list of all labels referred to other than by jumps
	     (which just don't have the REG_LABEL notes). 

	     Make a special exception for labels followed by an ADDR*VEC,
	     as this would be a part of the tablejump setup code. 

	     Make a special exception for the eh_return_stub_label, which
	     we know isn't part of any otherwise visible control flow.  */
	     
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_LABEL)
	      {
		rtx lab = XEXP (note, 0), next;

		if (lab == eh_return_stub_label)
		    ;
		else if ((next = next_nonnote_insn (lab)) != NULL
			 && GET_CODE (next) == JUMP_INSN
			 && (GET_CODE (PATTERN (next)) == ADDR_VEC
			     || GET_CODE (PATTERN (next)) == ADDR_DIFF_VEC))
		  ;
		else
		  label_value_list
		    = alloc_EXPR_LIST (0, XEXP (note, 0), label_value_list);
	      }
	}
    }

  if (head != NULL_RTX)
    create_basic_block (i++, head, end, bb_note);

  if (i != n_basic_blocks)
    abort ();

  return label_value_list;
}

/* Tidy the CFG by deleting unreachable code and whatnot.  */

void
cleanup_cfg (f)
     rtx f;
{
  delete_unreachable_blocks ();
  move_stray_eh_region_notes ();
  record_active_eh_regions (f);
  try_merge_blocks ();
  mark_critical_edges ();

  /* Kill the data we won't maintain.  */
  label_value_list = NULL_RTX;
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

      if (GET_CODE (head) == CODE_LABEL)
	add_insn_after (bb_note, head);
      else
	{
	  add_insn_before (bb_note, head);
	  head = bb_note;
	}
    }
  else
    {
      /* Otherwise we must create a note and a basic block structure.
	 Since we allow basic block structs in rtl, give the struct
	 the same lifetime by allocating it off the function obstack
	 rather than using malloc.  */

      bb = (basic_block) obstack_alloc (function_obstack, sizeof (*bb));
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

static void
clear_edges ()
{
  int i;
  edge n, e;

  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);

      for (e = bb->succ; e ; e = n)
	{
	  n = e->succ_next;
	  free (e);
	}

      bb->succ = 0;
      bb->pred = 0;
    }

  for (e = ENTRY_BLOCK_PTR->succ; e ; e = n)
    {
      n = e->succ_next;
      free (e);
    }

  ENTRY_BLOCK_PTR->succ = 0;
  EXIT_BLOCK_PTR->pred = 0;

  n_edges = 0;
}

/* Identify the edges between basic blocks.

   NONLOCAL_LABEL_LIST is a list of non-local labels in the function.  Blocks
   that are otherwise unreachable may be reachable with a non-local goto.

   BB_EH_END is an array indexed by basic block number in which we record 
   the list of exception regions active at the end of the basic block.  */

static void
make_edges (label_value_list)
     rtx label_value_list;
{
  int i;
  eh_nesting_info *eh_nest_info = init_eh_nesting_info ();
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
    }

  /* By nature of the way these get numbered, block 0 is always the entry.  */
  make_edge (edge_cache, ENTRY_BLOCK_PTR, BASIC_BLOCK (0), EDGE_FALLTHRU);

  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, x;
      enum rtx_code code;
      int force_fallthru = 0;

      /* Examine the last instruction of the block, and discover the
	 ways we can leave the block.  */

      insn = bb->end;
      code = GET_CODE (insn);

      /* A branch.  */
      if (code == JUMP_INSN)
	{
	  rtx tmp;

	  /* ??? Recognize a tablejump and do the right thing.  */
	  if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
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
	make_edge (edge_cache, bb, EXIT_BLOCK_PTR, 0);
      else

      /* If this is a CALL_INSN, then mark it as reaching the active EH
	 handler for this CALL_INSN.  If we're handling asynchronous
	 exceptions then any insn can reach any of the active handlers.

	 Also mark the CALL_INSN as reaching any nonlocal goto handler.  */

      if (code == CALL_INSN || asynchronous_exceptions)
	{
	  /* Add any appropriate EH edges.  We do this unconditionally
	     since there may be a REG_EH_REGION or REG_EH_RETHROW note
	     on the call, and this needn't be within an EH region.  */
	  make_eh_edge (edge_cache, eh_nest_info, bb, insn, bb->eh_end);

	  /* If we have asynchronous exceptions, do the same for *all*
	     exception regions active in the block.  */
	  if (asynchronous_exceptions
	      && bb->eh_beg != bb->eh_end)
	    {
	      if (bb->eh_beg >= 0)
		make_eh_edge (edge_cache, eh_nest_info, bb,
			      NULL_RTX, bb->eh_beg);

	      for (x = bb->head; x != bb->end; x = NEXT_INSN (x))
		if (GET_CODE (x) == NOTE
		    && (NOTE_LINE_NUMBER (x) == NOTE_INSN_EH_REGION_BEG
		        || NOTE_LINE_NUMBER (x) == NOTE_INSN_EH_REGION_END))
		  {
		    int region = NOTE_EH_HANDLER (x);
		    make_eh_edge (edge_cache, eh_nest_info, bb,
				  NULL_RTX, region);
		  }
	    }

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
		for (x = nonlocal_goto_handler_labels; x ; x = XEXP (x, 1))
		  make_label_edge (edge_cache, bb, XEXP (x, 0),
				   EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);
	    }
	}

      /* We know something about the structure of the function __throw in
	 libgcc2.c.  It is the only function that ever contains eh_stub
	 labels.  It modifies its return address so that the last block
	 returns to one of the eh_stub labels within it.  So we have to
	 make additional edges in the flow graph.  */
      if (i + 1 == n_basic_blocks && eh_return_stub_label != 0)
	make_label_edge (edge_cache, bb, eh_return_stub_label, EDGE_EH);

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

  free_eh_nesting_info (eh_nest_info);
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
  if (! use_edge_cache || TEST_BIT (edge_cache[src->index], dst->index))
    for (e = src->succ; e ; e = e->succ_next)
      if (e->dest == dst)
	{
	  e->flags |= flags;
	  return;
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
make_eh_edge (edge_cache, eh_nest_info, src, insn, region)
     sbitmap *edge_cache;
     eh_nesting_info *eh_nest_info;
     basic_block src;
     rtx insn;
     int region;
{
  handler_info **handler_list;
  int num, is_call;

  is_call = (insn && GET_CODE (insn) == CALL_INSN ? EDGE_ABNORMAL_CALL : 0);
  num = reachable_handlers (region, eh_nest_info, insn, &handler_list);
  while (--num >= 0)
    {
      make_label_edge (edge_cache, src, handler_list[num]->handler_label,
		       EDGE_ABNORMAL | EDGE_EH | is_call);
    }
}

/* EH_REGION notes appearing between basic blocks is ambiguous, and even
   dangerous if we intend to move basic blocks around.  Move such notes
   into the following block.  */

static void
move_stray_eh_region_notes ()
{
  int i;
  basic_block b1, b2;

  if (n_basic_blocks < 2)
    return;

  b2 = BASIC_BLOCK (n_basic_blocks - 1);
  for (i = n_basic_blocks - 2; i >= 0; --i, b2 = b1)
    {
      rtx insn, next, list = NULL_RTX;

      b1 = BASIC_BLOCK (i);
      for (insn = NEXT_INSN (b1->end); insn != b2->head; insn = next)
	{
	  next = NEXT_INSN (insn);
	  if (GET_CODE (insn) == NOTE
	      && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG
	          || NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END))
	    {
	      /* Unlink from the insn chain.  */
	      NEXT_INSN (PREV_INSN (insn)) = next;
	      PREV_INSN (next) = PREV_INSN (insn);

	      /* Queue it.  */
	      NEXT_INSN (insn) = list;
	      list = insn;
	    }
	}

      if (list == NULL_RTX)
	continue;

      /* Find where to insert these things.  */
      insn = b2->head;
      if (GET_CODE (insn) == CODE_LABEL)
	insn = NEXT_INSN (insn);

      while (list)
	{
	  next = NEXT_INSN (list);
	  add_insn_after (list, insn);
	  list = next;
	}
    }
}

/* Recompute eh_beg/eh_end for each basic block.  */

static void
record_active_eh_regions (f)
     rtx f;
{
  rtx insn, eh_list = NULL_RTX;
  int i = 0;
  basic_block bb = BASIC_BLOCK (0);

  for (insn = f; insn ; insn = NEXT_INSN (insn))
    {
      if (bb->head == insn)
	bb->eh_beg = (eh_list ? NOTE_EH_HANDLER (XEXP (eh_list, 0)) : -1);

      if (GET_CODE (insn) == NOTE)
	{
	  int kind = NOTE_LINE_NUMBER (insn);
	  if (kind == NOTE_INSN_EH_REGION_BEG)
	    eh_list = alloc_INSN_LIST (insn, eh_list);
	  else if (kind == NOTE_INSN_EH_REGION_END)
	    {
	      rtx t = XEXP (eh_list, 1);
	      free_INSN_LIST_node (eh_list);
	      eh_list = t;
	    }
	}

      if (bb->end == insn)
	{
	  bb->eh_end = (eh_list ? NOTE_EH_HANDLER (XEXP (eh_list, 0)) : -1);
	  i += 1;
	  if (i == n_basic_blocks)
	    break;
	  bb = BASIC_BLOCK (i);
	}
    }
}

/* Identify critical edges and set the bits appropriately.  */

static void
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
	  for (e = bb->succ; e ; e = e->succ_next)
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
	  for (e = bb->succ; e ; e = e->succ_next)
	    e->flags &= ~EDGE_CRITICAL;
	}

      if (++i >= n)
	break;
      bb = BASIC_BLOCK (i);
    }
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

  /* Remove the existing edge from the destination's pred list.  */
  {
    edge *pp;
    for (pp = &old_succ->pred; *pp != edge_in; pp = &(*pp)->pred_next)
      continue;
    *pp = edge_in->pred_next;
    edge_in->pred_next = NULL;
  }

  /* Create the new structures.  */
  bb = (basic_block) obstack_alloc (function_obstack, sizeof (*bb));
  edge_out = (edge) xcalloc (1, sizeof (*edge_out));
  n_edges++;

  memset (bb, 0, sizeof (*bb));

  /* ??? This info is likely going to be out of date very soon.  */
  if (old_succ->global_live_at_start)
    {
      bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (function_obstack);
      bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (function_obstack);
      COPY_REG_SET (bb->global_live_at_start, old_succ->global_live_at_start);
      COPY_REG_SET (bb->global_live_at_end, old_succ->global_live_at_start);
    }

  /* Wire them up.  */
  bb->pred = edge_in;
  bb->succ = edge_out;

  edge_in->dest = bb;
  edge_in->flags &= ~EDGE_CRITICAL;

  edge_out->pred_next = old_succ->pred;
  edge_out->succ_next = NULL;
  edge_out->src = bb;
  edge_out->dest = old_succ;
  edge_out->flags = EDGE_FALLTHRU;
  edge_out->probability = REG_BR_PROB_BASE;

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
      for (e = edge_out->pred_next; e ; e = e->pred_next)
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
				      jump_block->end);
	  jump_block->end = pos;
	  if (basic_block_for_insn)
	    set_block_for_insn (pos, jump_block);
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
      && NOTE_LINE_NUMBER (PREV_INSN (old_succ->head)) == NOTE_INSN_LOOP_BEG)
    bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK,
				PREV_INSN (old_succ->head));
  else if (old_succ != EXIT_BLOCK_PTR)
    bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK, old_succ->head);
  else
    bb_note = emit_note_after (NOTE_INSN_BASIC_BLOCK, get_last_insn ());
  NOTE_BASIC_BLOCK (bb_note) = bb;
  bb->head = bb->end = bb_note;

  /* Not quite simple -- for non-fallthru edges, we must adjust the
     predecessor's jump instruction to target our new block.  */
  if ((edge_in->flags & EDGE_FALLTHRU) == 0)
    {
      rtx tmp, insn = old_pred->end;
      rtx old_label = old_succ->head;
      rtx new_label = gen_label_rtx ();

      if (GET_CODE (insn) != JUMP_INSN)
	abort ();

      /* ??? Recognize a tablejump and adjust all matching cases.  */
      if ((tmp = JUMP_LABEL (insn)) != NULL_RTX
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
	    if (XEXP (RTVEC_ELT (vec, j), 0) == old_label)
	      {
	        RTVEC_ELT (vec, j) = gen_rtx_LABEL_REF (VOIDmode, new_label);
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
	  /* This would have indicated an abnormal edge.  */
	  if (computed_jump_p (insn))
	    abort ();

	  /* A return instruction can't be redirected.  */
	  if (returnjump_p (insn))
	    abort ();

	  /* If the insn doesn't go where we think, we're confused.  */
	  if (JUMP_LABEL (insn) != old_label)
	    abort ();

	  redirect_jump (insn, new_label);
	}

      emit_label_before (new_label, bb_note);
      bb->head = new_label;
    }

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
  end_sequence();
}

/* Update the CFG for the instructions queued on edge E.  */

static void
commit_one_edge_insertion (e)
     edge e;
{
  rtx before = NULL_RTX, after = NULL_RTX, insns, tmp;
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
      if (GET_CODE (tmp) == NOTE
	  && NOTE_LINE_NUMBER (tmp) == NOTE_INSN_BASIC_BLOCK)
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
    }
  else
    {
      rtx last = emit_insns_after (insns, after);
      if (after == bb->end)
	{
	  bb->end = last;

	  if (GET_CODE (last) == JUMP_INSN)
	    {
	      if (returnjump_p (last))
		{
		  /* ??? Remove all outgoing edges from BB and add one
		     for EXIT.  This is not currently a problem because
		     this only happens for the (single) epilogue, which
		     already has a fallthru edge to EXIT.  */

		  e = bb->succ;
		  if (e->dest != EXIT_BLOCK_PTR
		      || e->succ_next != NULL
		      || (e->flags & EDGE_FALLTHRU) == 0)
		    abort ();
		  e->flags &= ~EDGE_FALLTHRU;

		  emit_barrier_after (last);
		}
	      else
		abort ();
	    }
	}
    }
}

/* Update the CFG for all queued instructions.  */

void
commit_edge_insertions ()
{
  int i;
  basic_block bb;

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
 
  i = -1;
  bb = ENTRY_BLOCK_PTR;
  while (1)
    {
      edge e, next;

      for (e = bb->succ; e ; e = next)
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

/* Delete all unreachable basic blocks.   */

static void
delete_unreachable_blocks ()
{
  basic_block *worklist, *tos;
  int deleted_handler;
  edge e;
  int i, n;

  n = n_basic_blocks;
  tos = worklist = (basic_block *) xmalloc (sizeof (basic_block) * n);

  /* Use basic_block->aux as a marker.  Clear them all.  */

  for (i = 0; i < n; ++i)
    BASIC_BLOCK (i)->aux = NULL;

  /* Add our starting points to the worklist.  Almost always there will
     be only one.  It isn't inconcievable that we might one day directly
     support Fortran alternate entry points.  */

  for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
    {
      *tos++ = e->dest;

      /* Mark the block with a handy non-null value.  */
      e->dest->aux = e;
    }
      
  /* Iterate: find everything reachable from what we've already seen.  */

  while (tos != worklist)
    {
      basic_block b = *--tos;

      for (e = b->succ; e ; e = e->succ_next)
	if (!e->dest->aux)
	  {
	    *tos++ = e->dest;
	    e->dest->aux = e;
	  }
    }

  /* Delete all unreachable basic blocks.  Count down so that we don't
     interfere with the block renumbering that happens in flow_delete_block. */

  deleted_handler = 0;

  for (i = n - 1; i >= 0; --i)
    {
      basic_block b = BASIC_BLOCK (i);

      if (b->aux != NULL)
	/* This block was found.  Tidy up the mark.  */
	b->aux = NULL;
      else
	deleted_handler |= flow_delete_block (b);
    }

  tidy_fallthru_edges ();

  /* If we deleted an exception handler, we may have EH region begin/end
     blocks to remove as well. */
  if (deleted_handler)
    delete_eh_regions ();

  free (worklist);
}

/* Find EH regions for which there is no longer a handler, and delete them.  */

static void
delete_eh_regions ()
{
  rtx insn;

  update_rethrow_references ();

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if ((NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG) ||
	    (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)) 
	  {
	    int num = NOTE_EH_HANDLER (insn);
	    /* A NULL handler indicates a region is no longer needed,
	       as long as its rethrow label isn't used.  */
	    if (get_first_handler (num) == NULL && ! rethrow_used (num))
	      {
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
	      }
	  }
      }
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
      else if (GET_CODE (start) == CODE_LABEL && !can_delete_label_p (start))
	;
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
    {
      rtx x, *prev = &exception_handler_labels;

      for (x = exception_handler_labels; x; x = XEXP (x, 1))
	{
	  if (XEXP (x, 0) == insn)
	    {
	      /* Found a match, splice this label out of the EH label list.  */
	      *prev = XEXP (x, 1);
	      XEXP (x, 1) = NULL_RTX;
	      XEXP (x, 0) = NULL_RTX;

	      /* Remove the handler from all regions */
	      remove_handler (insn);
	      deleted_handler = 1;
	      break;
	    }
	  prev = &XEXP (x, 1);
	}

      /* This label may be referenced by code solely for its value, or
	 referenced by static data, or something.  We have determined
	 that it is not reachable, but cannot delete the label itself.
	 Save code space and continue to delete the balance of the block,
	 along with properly updating the cfg.  */
      if (!can_delete_label_p (insn))
	{
	  /* If we've only got one of these, skip the whole deleting
	     insns thing.  */
	  if (insn == b->end)
	    goto no_delete_insns;
	  insn = NEXT_INSN (insn);
	}
    }

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

 no_delete_insns:

  /* Remove the edges into and out of this block.  Note that there may 
     indeed be edges in, if we are removing an unreachable loop.  */
  {
    edge e, next, *q;

    for (e = b->pred; e ; e = next)
      {
	for (q = &e->src->succ; *q != e; q = &(*q)->succ_next)
	  continue;
	*q = e->succ_next;
	next = e->pred_next;
	n_edges--;
	free (e);
      }
    for (e = b->succ; e ; e = next)
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
  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
    LABEL_NUSES (JUMP_LABEL (insn))--;

  /* Also if deleting an insn that references a label.  */
  else if ((note = find_reg_note (insn, REG_LABEL, NULL_RTX)) != NULL_RTX)
    LABEL_NUSES (XEXP (note, 0))--;

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

  for (x = forced_labels; x ; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;
  for (x = label_value_list; x ; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;
  for (x = exception_handler_labels; x ; x = XEXP (x, 1))
    if (label == XEXP (x, 0))
      return 0;

  /* User declared labels must be preserved.  */
  if (LABEL_NAME (label) != 0)
    return 0;
  
  return 1;
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
  if (GET_CODE (b_head) == NOTE 
      && NOTE_LINE_NUMBER (b_head) == NOTE_INSN_BASIC_BLOCK)
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

      prev = prev_nonnote_insn (a_end);
      if (!prev) 
	prev = a->head;

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
  for (e = b->succ; e ; e = e->succ_next)
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

  /* We want to delete the BARRIER after the end of the insns we are
     going to move.  If we don't find a BARRIER, then do nothing.  This
     can happen in some cases if we have labels we can not delete. 

     Similarly, do nothing if we can not delete the label at the start
     of the target block.  */
  barrier = next_nonnote_insn (end);
  if (GET_CODE (barrier) != BARRIER
      || (GET_CODE (b->head) == CODE_LABEL
	  && ! can_delete_label_p (b->head)))
    return 0;
  else
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
  BASIC_BLOCK(a->index) = b;
  BASIC_BLOCK(b->index) = a;
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

  /* We want to delete the BARRIER after the end of the insns we are
     going to move.  If we don't find a BARRIER, then do nothing.  This
     can happen in some cases if we have labels we can not delete. 

     Similarly, do nothing if we can not delete the label at the start
     of the target block.  */
  barrier = next_nonnote_insn (end);
  if (GET_CODE (barrier) != BARRIER
      || (GET_CODE (b->head) == CODE_LABEL
	  && ! can_delete_label_p (b->head)))
    return 0;
  else
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
merge_blocks (e, b, c)
     edge e;
     basic_block b, c;
{
  /* If B has a fallthru edge to C, no need to move anything.  */
  if (e->flags & EDGE_FALLTHRU)
    {
      /* If a label still appears somewhere and we cannot delete the label,
	 then we cannot merge the blocks.  The edge was tidied already.  */

      rtx insn, stop = NEXT_INSN (c->head);
      for (insn = NEXT_INSN (b->end); insn != stop; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == CODE_LABEL && !can_delete_label_p (insn))
	  return 0;

      merge_blocks_nomove (b, c);

      if (rtl_dump_file)
	{
	  fprintf (rtl_dump_file, "Merged %d and %d without moving.\n",
		   b->index, c->index);
	}

      return 1;
    }
  else
    {
      edge tmp_edge;
      basic_block d;
      int c_has_outgoing_fallthru;
      int b_has_incoming_fallthru;

      /* We must make sure to not munge nesting of exception regions,
	 lexical blocks, and loop notes.
  
	 The first is taken care of by requiring that the active eh
	 region at the end of one block always matches the active eh
	 region at the beginning of the next block.
  
	 The later two are taken care of by squeezing out all the notes.  */
  
      /* ???  A throw/catch edge (or any abnormal edge) should be rarely
	 executed and we may want to treat blocks which have two out
	 edges, one normal, one abnormal as only having one edge for
	 block merging purposes.  */

      for (tmp_edge = c->succ; tmp_edge ; tmp_edge = tmp_edge->succ_next)
	if (tmp_edge->flags & EDGE_FALLTHRU)
	  break;
      c_has_outgoing_fallthru = (tmp_edge != NULL);

      for (tmp_edge = b->pred; tmp_edge ; tmp_edge = tmp_edge->pred_next)
	if (tmp_edge->flags & EDGE_FALLTHRU)
	  break;
      b_has_incoming_fallthru = (tmp_edge != NULL);

      /* If B does not have an incoming fallthru, and the exception regions
	 match, then it can be moved immediately before C without introducing
	 or modifying jumps.

	 C can not be the first block, so we do not have to worry about
	 accessing a non-existent block.  */
      d = BASIC_BLOCK (c->index - 1);
      if (! b_has_incoming_fallthru
	  && d->eh_end == b->eh_beg
	  && b->eh_end == c->eh_beg)
	return merge_blocks_move_predecessor_nojumps (b, c);

      /* Otherwise, we're going to try to move C after B.  Make sure the
	 exception regions match.

	 If B is the last basic block, then we must not try to access the
	 block structure for block B + 1.  Luckily in that case we do not
	 need to worry about matching exception regions.  */
      d = (b->index + 1 < n_basic_blocks ? BASIC_BLOCK (b->index + 1) : NULL);
      if (b->eh_end == c->eh_beg
	  && (d == NULL || c->eh_end == d->eh_beg))
	{
	  /* If C does not have an outgoing fallthru, then it can be moved
	     immediately after B without introducing or modifying jumps.  */
	  if (! c_has_outgoing_fallthru)
	    return merge_blocks_move_successor_nojumps (b, c);

	  /* Otherwise, we'll need to insert an extra jump, and possibly
	     a new block to contain it.  */
	  /* ??? Not implemented yet.  */
	}

      return 0;
    }
}

/* Top level driver for merge_blocks.  */

static void
try_merge_blocks ()
{
  int i;

  /* Attempt to merge blocks as made possible by edge removal.  If a block
     has only one successor, and the successor has only one predecessor, 
     they may be combined.  */

  for (i = 0; i < n_basic_blocks; )
    {
      basic_block c, b = BASIC_BLOCK (i);
      edge s;

      /* A loop because chains of blocks might be combineable.  */
      while ((s = b->succ) != NULL
	     && s->succ_next == NULL
	     && (s->flags & EDGE_EH) == 0
	     && (c = s->dest) != EXIT_BLOCK_PTR
	     && c->pred->pred_next == NULL
	     /* If the jump insn has side effects, we can't kill the edge.  */
	     && (GET_CODE (b->end) != JUMP_INSN
		 || onlyjump_p (b->end))
	     && merge_blocks (s, b, c))
	continue;

      /* Don't get confused by the index shift caused by deleting blocks.  */
      i = b->index + 1;
    }
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
      && (simplejump_p (q)
	  || (b->succ == e && e->succ_next == NULL)))
    {
#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (! simplejump_p (q) && condjump_p (q) && sets_cc0_p (PREV_INSN (q)))
	q = PREV_INSN (q);
#endif

      if (b->head == q)
	{
	  PUT_CODE (q, NOTE);
	  NOTE_LINE_NUMBER (q) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (q) = 0;
	}
      else
	b->end = q = PREV_INSN (q);
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
  for (i = 0; i < (int) (sizeof eliminables / sizeof eliminables[0]); i++)
    SET_HARD_REG_BIT (elim_reg_set, eliminables[i].from);
#else
  SET_HARD_REG_BIT (elim_reg_set, FRAME_POINTER_REGNUM);
#endif

  if (! optimize)
    flags &= PROP_DEATH_NOTES | PROP_REG_INFO;

  /* The post-reload life analysis have (on a global basis) the same
     registers live as was computed by reload itself.  elimination
     Otherwise offsets and such may be incorrect.

     Reload will make some registers as live even though they do not
     appear in the rtl.  */
  if (reload_completed)
    flags &= ~PROP_REG_INFO;

  /* We want alias analysis information for local dead store elimination.  */
  if (flags & PROP_SCAN_DEAD_CODE)
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
    memset (regs_ever_live, 0, sizeof(regs_ever_live));
  update_life_info (NULL, UPDATE_LIFE_GLOBAL, flags);

  /* Clean up.  */
  if (flags & PROP_SCAN_DEAD_CODE)
    end_alias_analysis ();

  if (file)
    dump_flow_info (file);

  free_basic_block_vars (1);
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
      if (GET_RTX_CLASS (GET_CODE (head)) == 'i'
	  && for_each_rtx (&PATTERN (head), verify_wide_reg_1, &regno))
	return;
      if (head == end)
	break;
      head = NEXT_INSN (head);
    }

  /* We didn't find the register at all.  Something's way screwy.  */
  abort ();
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
        abort ();
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
	    abort ();
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

  /* For a global update, we go through the relaxation process again.  */
  if (extent != UPDATE_LIFE_LOCAL)
    {
      calculate_global_regs_live (blocks, blocks,
				  prop_flags & PROP_SCAN_DEAD_CODE);

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
	  propagate_block (bb, tmp, (regset) NULL, prop_flags);

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
	  propagate_block (bb, tmp, (regset) NULL, prop_flags);

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
      clear_edges ();
      VARRAY_FREE (basic_block_info);
      n_basic_blocks = 0;

      ENTRY_BLOCK_PTR->aux = NULL;
      ENTRY_BLOCK_PTR->global_live_at_end = NULL;
      EXIT_BLOCK_PTR->aux = NULL;
      EXIT_BLOCK_PTR->global_live_at_start = NULL;
    }
}

/* Return nonzero if the destination of SET equals the source.  */
static int
set_noop_p (set)
     rtx set;
{
  rtx src = SET_SRC (set);
  rtx dst = SET_DEST (set);

  if (GET_CODE (src) == SUBREG && GET_CODE (dst) == SUBREG)
    {
      if (SUBREG_WORD (src) != SUBREG_WORD (dst))
	return 0;
      src = SUBREG_REG (src);
      dst = SUBREG_REG (dst);
    }

  return (GET_CODE (src) == REG && GET_CODE (dst) == REG
	  && REGNO (src) == REGNO (dst));
}

/* Return nonzero if an insn consists only of SETs, each of which only sets a
   value to itself.  */
static int
noop_move_p (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);

  /* Insns carrying these notes are useful later on.  */
  if (find_reg_note (insn, REG_EQUAL, NULL_RTX))
    return 0;

  if (GET_CODE (pat) == SET && set_noop_p (pat))
    return 1;

  if (GET_CODE (pat) == PARALLEL)
    {
      int i;
      /* If nothing but SETs of registers to themselves,
	 this insn can also be deleted.  */
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx tem = XVECEXP (pat, 0, i);

	  if (GET_CODE (tem) == USE
	      || GET_CODE (tem) == CLOBBER)
	    continue;

	  if (GET_CODE (tem) != SET || ! set_noop_p (tem))
	    return 0;
	}

      return 1;
    }
  return 0;
}

/* Delete any insns that copy a register to itself.  */

static void
delete_noop_moves (f)
     rtx f;
{
  rtx insn;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN && noop_move_p (insn))
	{
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (insn) = 0;
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
	  && (GET_CODE (XEXP (x, 0)) == PRE_DEC
	      || GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == POST_DEC
	      || GET_CODE (XEXP (x, 0)) == POST_INC)
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
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
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
  int i;

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
      /* If they are different, also mark the hard frame pointer as live */
      SET_REGNO_REG_SET (set, HARD_FRAME_POINTER_REGNUM);
#endif      
    }

#ifdef PIC_OFFSET_TABLE_REGNUM
#ifndef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
  /* Many architectures have a GP register even without flag_pic.
     Assume the pic register is not in use, or will be handled by
     other means, if it is not fixed.  */
  if (fixed_regs[PIC_OFFSET_TABLE_REGNUM])
    SET_REGNO_REG_SET (set, PIC_OFFSET_TABLE_REGNUM);
#endif
#endif

  /* Mark all global registers, and all registers used by the epilogue
     as being live at the end of the function since they may be
     referenced by our caller.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i]
#ifdef EPILOGUE_USES
	|| EPILOGUE_USES (i)
#endif
	)
      SET_REGNO_REG_SET (set, i);

  /* Mark all call-saved registers that we actaully used.  */
  if (HAVE_epilogue && reload_completed)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (! call_used_regs[i] && regs_ever_live[i])
	  SET_REGNO_REG_SET (set, i);
    }

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
  regset tmp, new_live_at_end;
  regset_head tmp_head;
  regset_head new_live_at_end_head;
  int i;

  tmp = INITIALIZE_REG_SET (tmp_head);
  new_live_at_end = INITIALIZE_REG_SET (new_live_at_end_head);

  /* Create a worklist.  Allocate an extra slot for ENTRY_BLOCK, and one
     because the `head == tail' style test for an empty queue doesn't 
     work with a full queue.  */
  queue = (basic_block *) xmalloc ((n_basic_blocks + 2) * sizeof (*queue));
  qtail = queue;
  qhead = qend = queue + n_basic_blocks + 2;

  /* Clear out the garbage that might be hanging out in bb->aux.  */
  for (i = n_basic_blocks - 1; i >= 0; --i)
    BASIC_BLOCK (i)->aux = NULL;

  /* Queue the blocks set in the initial mask.  Do this in reverse block
     number order so that we are more likely for the first round to do 
     useful work.  We use AUX non-null to flag that the block is queued.  */
  if (blocks_in)
    {
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

  while (qhead != qtail)
    {
      int rescan, changed;
      basic_block bb;
      edge e;

      bb = *qhead++;
      if (qhead == qend)
	qhead = queue;
      bb->aux = NULL;

      /* Begin by propogating live_at_start from the successor blocks.  */
      CLEAR_REG_SET (new_live_at_end);
      for (e = bb->succ; e ; e = e->succ_next)
	{
	  basic_block sb = e->dest;
	  IOR_REG_SET (new_live_at_end, sb->global_live_at_start);
	}

      /* Force the stack pointer to be live -- which might not already be 
	 the case for blocks within infinite loops.  */
      SET_REGNO_REG_SET (new_live_at_end, STACK_POINTER_REGNUM);

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
	  bb->local_set = OBSTACK_ALLOC_REG_SET (function_obstack);
	  rescan = 1;
	}
      else
	{
	  /* If any bits were removed from live_at_end, we'll have to
	     rescan the block.  This wouldn't be necessary if we had
	     precalculated local_live, however with PROP_SCAN_DEAD_CODE
	     local_live is really dependant on live_at_end.  */
	  CLEAR_REG_SET (tmp);
	  rescan = bitmap_operation (tmp, bb->global_live_at_end,
				     new_live_at_end, BITMAP_AND_COMPL);

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
	  propagate_block (bb, new_live_at_end, bb->local_set, flags);

	  /* If live_at start didn't change, no need to go farther.  */
	  if (REG_SET_EQUAL_P (bb->global_live_at_start, new_live_at_end))
	    continue;

	  COPY_REG_SET (bb->global_live_at_start, new_live_at_end);
	}

      /* Queue all predecessors of BB so that we may re-examine
	 their live_at_end.  */
      for (e = bb->pred; e ; e = e->pred_next)
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

  if (blocks_out)
    {
      EXECUTE_IF_SET_IN_SBITMAP (blocks_out, 0, i,
	{
	  basic_block bb = BASIC_BLOCK (i);
	  FREE_REG_SET (bb->local_set);
	});
    }
  else
    {
      for (i = n_basic_blocks - 1; i >= 0; --i)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  FREE_REG_SET (bb->local_set);
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

      bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (function_obstack);
      bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (function_obstack);
    }

  ENTRY_BLOCK_PTR->global_live_at_end
    = OBSTACK_ALLOC_REG_SET (function_obstack);
  EXIT_BLOCK_PTR->global_live_at_start
    = OBSTACK_ALLOC_REG_SET (function_obstack);

  regs_live_at_setjmp = OBSTACK_ALLOC_REG_SET (function_obstack);
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
     referencing labels that no longer exist.  */

  if (inote)
    {
      rtx label = XEXP (inote, 0);
      rtx next;

      if (LABEL_NUSES (label) == 1
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
      insn_is_dead = insn_dead_p (pbi, PATTERN (insn), 0,
				  REG_NOTES (insn));
      libcall_is_dead = (insn_is_dead && note != 0
			 && libcall_dead_p (pbi, PATTERN (insn),
					    note, insn));
    }

  /* We almost certainly don't want to delete prologue or epilogue
     instructions.  Warn about probable compiler losage.  */
  if (insn_is_dead
      && reload_completed
      && (((HAVE_epilogue || HAVE_prologue)
	   && prologue_epilogue_contains (insn))
	  || (HAVE_sibcall_epilogue
	      && sibcall_epilogue_contains (insn))))
    {
      if (flags & PROP_KILL_DEAD_CODE)
	{ 
	  warning ("ICE: would have deleted prologue/epilogue insn");
	  if (!inhibit_warnings)
	    debug_rtx (insn);
	}
      libcall_is_dead = insn_is_dead = 0;
    }

  /* If an instruction consists of just dead store(s) on final pass,
     delete it.  */
  if ((flags & PROP_KILL_DEAD_CODE) && insn_is_dead)
    {
      /* Record sets.  Do this even for dead instructions, since they
	 would have killed the values if they hadn't been deleted.  */
      mark_set_regs (pbi, PATTERN (insn), insn);

      /* CC0 is now known to be dead.  Either this insn used it,
	 in which case it doesn't anymore, or clobbered it,
	 so the next insn can't use it.  */
      pbi->cc0_live = 0;

      if (libcall_is_dead)
	{
	  prev = propagate_block_delete_libcall (pbi->bb, insn, note);
	  insn = NEXT_INSN (prev);
	}
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
    if (!reload_completed
	&& (flags & PROP_AUTOINC)
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
	  if (! CONST_CALL_P (insn))
	    free_EXPR_LIST_list (&pbi->mem_set_list);

	  /* There may be extra registers to be clobbered.  */
	  for (note = CALL_INSN_FUNCTION_USAGE (insn);
	       note;
	       note = XEXP (note, 1))
	    if (GET_CODE (XEXP (note, 0)) == CLOBBER)
	      mark_set_1 (pbi, CLOBBER, XEXP (XEXP (note, 0), 0),
			  cond, insn, pbi->flags);

	  /* Calls change all call-used and global registers.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (call_used_regs[i] && ! global_regs[i]
		&& ! fixed_regs[i])
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
init_propagate_block_info (bb, live, local_set, flags)
     basic_block bb;
     regset live;
     regset local_set;
     int flags;
{
  struct propagate_block_info *pbi = xmalloc (sizeof(*pbi));

  pbi->bb = bb;
  pbi->reg_live = live;
  pbi->mem_set_list = NULL_RTX;
  pbi->local_set = local_set;
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
      && condjump_p (bb->end)
      && ! simplejump_p (bb->end))
    {
      regset_head diff_head;
      regset diff = INITIALIZE_REG_SET (diff_head);
      basic_block bb_true, bb_false;
      rtx cond_true, cond_false;
      int i;

      /* Identify the successor blocks.  */
      bb_false = bb->succ->succ_next->dest;
      bb_true = bb->succ->dest;
      if (bb->succ->flags & EDGE_FALLTHRU)
	{
	  basic_block t = bb_false;
	  bb_false = bb_true;
	  bb_true = t;
	}
      else if (! (bb->succ->succ_next->flags & EDGE_FALLTHRU))
	abort ();
     
      /* Extract the condition from the branch.  */
      cond_true = XEXP (SET_SRC (PATTERN (bb->end)), 0);
      cond_false = gen_rtx_fmt_ee (reverse_condition (GET_CODE (cond_true)),
				   GET_MODE (cond_true), XEXP (cond_true, 0),
				   XEXP (cond_true, 1));
      if (GET_CODE (XEXP (SET_SRC (PATTERN (bb->end)), 1)) == PC)
	{
	  rtx t = cond_false;
	  cond_false = cond_true;
	  cond_true = t;
	}

      /* Compute which register lead different lives in the successors.  */
      if (bitmap_operation (diff, bb_true->global_live_at_start,
			    bb_false->global_live_at_start, BITMAP_XOR))
	{
	  if (GET_CODE (XEXP (cond_true, 0)) != REG)
	    abort ();
	  SET_REGNO_REG_SET (pbi->reg_cond_reg, REGNO (XEXP (cond_true, 0)));

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
	       rcli->condition = alloc_EXPR_LIST (0, cond, NULL_RTX);

	       splay_tree_insert (pbi->reg_cond_dead, i,
				  (splay_tree_value) rcli);
	     });
	}

      FREE_REG_SET (diff);
    }
#endif

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

   LOCAL_SET, if non-null, will be set with all registers killed by 
   this basic block.  */

void
propagate_block (bb, live, local_set, flags)
     basic_block bb;
     regset live;
     regset local_set;
     int flags;
{
  struct propagate_block_info *pbi;
  rtx insn, prev;
  
  pbi = init_propagate_block_info (bb, live, local_set, flags);

  if (flags & PROP_REG_INFO)
    {
      register int i;

      /* Process the regs live at the end of the block.
	 Mark them as not local to any one basic block. */
      EXECUTE_IF_SET_IN_REG_SET (live, 0, i,
				 { REG_BASIC_BLOCK (i) = REG_BLOCK_GLOBAL; });
    }

  /* Scan the block an insn at a time from end to beginning.  */

  for (insn = bb->end; ; insn = prev)
    {
      /* If this is a call to `setjmp' et al, warn if any
	 non-volatile datum is live.  */
      if ((flags & PROP_REG_INFO)
	  && GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	IOR_REG_SET (regs_live_at_setjmp, pbi->reg_live);

      prev = propagate_one_insn (pbi, insn);

      if (insn == bb->head)
	break;
    }

  free_propagate_block_info (pbi);
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
      for ( ; notes; notes = XEXP (notes, 1))
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
	  rtx temp;

	  if (MEM_VOLATILE_P (r))
	    return 0;

	  /* Walk the set of memory locations we are currently tracking
	     and see if one is an identical match to this memory location.
	     If so, this memory write is dead (remember, we're walking
	     backwards from the end of the block to the start.  */
	  temp = pbi->mem_set_list;
	  while (temp)
	    {
	      if (rtx_equal_p (XEXP (temp, 0), r))
		return 1;
	      temp = XEXP (temp, 1);
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

/* If X is the pattern of the last insn in a libcall, and assuming X is dead,
   return 1 if the entire library call is dead.
   This is true if X copies a register (hard or pseudo)
   and if the hard return  reg of the call insn is dead.
   (The caller should have tested the destination of X already for death.)

   If this insn doesn't just copy a register, then we don't
   have an ordinary libcall.  In that case, cse could not have
   managed to substitute the source for the dest later on,
   so we can assume the libcall is dead.

   NEEDED is the bit vector of pseudoregs live before this insn.
   NOTE is the REG_RETVAL note of the insn.  INSN is the insn itself.  */

static int
libcall_dead_p (pbi, x, note, insn)
     struct propagate_block_info *pbi;
     rtx x;
     rtx note;
     rtx insn;
{
  register RTX_CODE code = GET_CODE (x);

  if (code == SET)
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
    {
      if (REG_NOTE_KIND (note) == REG_INC)
        {
          rtx temp = pbi->mem_set_list;
          rtx prev = NULL_RTX;
	  rtx next;

          while (temp)
	    {
	      next = XEXP (temp, 1);
	      if (reg_overlap_mentioned_p (XEXP (note, 0), XEXP (temp, 0)))
	        {
	          /* Splice temp out of list.  */
	          if (prev)
	            XEXP (prev, 1) = next;
	          else
	            pbi->mem_set_list = next;
		  free_EXPR_LIST_node (temp);
	        }
	      else
	        prev = temp;
              temp = next;
	    }
	}
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
  enum rtx_code code;

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
		/* FALLTHRU */

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

/* Process a single SET rtx, X.  */

static void
mark_set_1 (pbi, code, reg, cond, insn, flags)
     struct propagate_block_info *pbi;
     enum rtx_code code;
     rtx reg, cond, insn;
     int flags;
{
  int regno_first = -1, regno_last = -1;
  int not_dead = 0;
  int i;

  /* Some targets place small structures in registers for
     return values of functions.  We have to detect this
     case specially here to get correct flow information.  */
  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	mark_set_1 (pbi, code, XVECEXP (reg, 0, i), cond, insn, flags);
      return;
    }

  /* Modifying just one hardware register of a multi-reg value or just a
     byte field of a register does not mean the value from before this insn
     is now dead.  Of course, if it was dead after it's unused now.  */

  switch (GET_CODE (reg))
    {
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
      not_dead = REGNO_REG_SET_P (pbi->reg_live, REGNO (reg));
      /* FALLTHRU */

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
#ifdef ALTER_HARD_SUBREG
	      regno_first = ALTER_HARD_SUBREG (outer_mode, SUBREG_WORD (reg),
					       inner_mode, regno_first);
#else
	      regno_first += SUBREG_WORD (reg);
#endif
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
		not_dead = REGNO_REG_SET_P (pbi->reg_live, regno_first);

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
  if (flags & PROP_SCAN_DEAD_CODE)
    {
      if (GET_CODE (reg) == MEM || GET_CODE (reg) == REG)
	{
	  rtx temp = pbi->mem_set_list;
	  rtx prev = NULL_RTX;
	  rtx next;

	  while (temp)
	    {
	      next = XEXP (temp, 1);
	      if ((GET_CODE (reg) == MEM
		   && output_dependence (XEXP (temp, 0), reg))
		  || (GET_CODE (reg) == REG
		      && reg_overlap_mentioned_p (reg, XEXP (temp, 0))))
		{
		  /* Splice this entry out of the list.  */
		  if (prev)
		    XEXP (prev, 1) = next;
		  else
		    pbi->mem_set_list = next;
		  free_EXPR_LIST_node (temp);
		}
	      else
		prev = temp;
	      temp = next;
	    }
	}

      /* If the memory reference had embedded side effects (autoincrement
	 address modes.  Then we may need to kill some entries on the
	 memory set list.  */
      if (insn && GET_CODE (reg) == MEM)
	invalidate_mems_from_autoinc (pbi, insn);

      if (GET_CODE (reg) == MEM && ! side_effects_p (reg)
	  /* We do not know the size of a BLKmode store, so we do not track
	     them for redundant store elimination.  */
	  && GET_MODE (reg) != BLKmode
	  /* There are no REG_INC notes for SP, so we can't assume we'll see 
	     everything that invalidates it.  To be safe, don't eliminate any
	     stores though SP; none of them should be redundant anyway.  */
	  && ! reg_mentioned_p (stack_pointer_rtx, reg))
	pbi->mem_set_list = alloc_EXPR_LIST (0, reg, pbi->mem_set_list);
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
	    SET_REGNO_REG_SET (pbi->local_set, i);
	  if (code != CLOBBER)
	    SET_REGNO_REG_SET (pbi->new_set, i);

	  some_was_live |= needed_regno;
	  some_was_dead |= ! needed_regno;
	}

#ifdef HAVE_conditional_execution
      /* Consider conditional death in deciding that the register needs
	 a death note.  */
      if (some_was_live
	  /* The stack pointer is never dead.  Well, not strictly true,
	     but it's very difficult to tell from here.  Hopefully
	     combine_stack_adjustments will fix up the most egregious
	     errors.  */
	  && regno_first != STACK_POINTER_REGNUM)
	{
	  for (i = regno_first; i <= regno_last; ++i)
	    if (! mark_regno_cond_dead (pbi, i, cond))
	      not_dead = 1;
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
		  REG_N_REFS (i) += (optimize_size ? 1
				     : pbi->bb->loop_depth + 1);

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
	  && ! not_dead
	  /* The stack pointer is never dead.  Well, not strictly true,
	     but it's very difficult to tell from here.  Hopefully
	     combine_stack_adjustments will fix up the most egregious
	     errors.  */
	  && regno_first != STACK_POINTER_REGNUM)
	{
	  for (i = regno_first; i <= regno_last; ++i)
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
/* Mark REGNO conditionally dead.  Return true if the register is
   now unconditionally dead.  */

static int
mark_regno_cond_dead (pbi, regno, cond)
     struct propagate_block_info *pbi;
     int regno;
     rtx cond;
{
  /* If this is a store to a predicate register, the value of the
     predicate is changing, we don't know that the predicate as seen
     before is the same as that seen after.  Flush all dependant
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
	  rcli = (struct reg_cond_life_info *)
	    xmalloc (sizeof (*rcli));
	  rcli->condition = alloc_EXPR_LIST (0, cond, NULL_RTX);
	  splay_tree_insert (pbi->reg_cond_dead, regno,
			     (splay_tree_value) rcli);

	  SET_REGNO_REG_SET (pbi->reg_cond_reg,
			     REGNO (XEXP (cond, 0)));

	  /* Not unconditionaly dead.  */
	  return 0;
	}
      else
	{
	  /* The register was conditionally live previously. 
	     Add the new condition to the old.  */
	  rcli = (struct reg_cond_life_info *) node->value;
	  ncond = rcli->condition;
	  ncond = ior_reg_cond (ncond, cond);

	  /* If the register is now unconditionally dead,
	     remove the entry in the splay_tree.  */
	  if (ncond == const1_rtx)
	    splay_tree_remove (pbi->reg_cond_dead, regno);
	  else
	    {
	      rcli->condition = ncond;

	      SET_REGNO_REG_SET (pbi->reg_cond_reg,
				 REGNO (XEXP (cond, 0)));

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
  free_EXPR_LIST_list (&rcli->condition);
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
  rtx c, *prev;

  /* Don't need to search if last flushed value was farther on in
     the in-order traversal.  */
  if (xdata[1] >= (int) node->key)
    return 0;

  /* Splice out portions of the expression that refer to regno.  */
  rcli = (struct reg_cond_life_info *) node->value;
  c = *(prev = &rcli->condition);
  while (c)
    {
      if (regno == REGNO (XEXP (XEXP (c, 0), 0)))
	{
	  rtx next = XEXP (c, 1);
	  free_EXPR_LIST_node (c);
	  c = *prev = next;
	}
      else
	c = *(prev = &XEXP (c, 1));
    }

  /* If the entire condition is now NULL, signal the node to be removed.  */
  if (! rcli->condition)
    {
      xdata[1] = node->key;
      return -1;
    }
  else
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

/* Logical arithmetic on predicate conditions.  IOR, NOT and NAND.
   We actually use EXPR_LIST to chain the sub-expressions together
   instead of IOR because it's easier to manipulate and we have 
   the lists.c functions to reuse nodes.
   
   Return a new rtl expression as appropriate.  */

static rtx
ior_reg_cond (old, x)
     rtx old, x;
{
  enum rtx_code x_code;
  rtx x_reg;
  rtx c;

  /* We expect these conditions to be of the form (eq reg 0).  */
  x_code = GET_CODE (x);
  if (GET_RTX_CLASS (x_code) != '<'
      || GET_CODE (x_reg = XEXP (x, 0)) != REG
      || XEXP (x, 1) != const0_rtx)
    abort ();

  /* Search the expression for an existing sub-expression of X_REG.  */
  for (c = old; c ; c = XEXP (c, 1))
    {
      rtx y = XEXP (c, 0);
      if (REGNO (XEXP (y, 0)) == REGNO (x_reg))
	{
	  /* If we find X already present in OLD, we need do nothing.  */
	  if (GET_CODE (y) == x_code)
	    return old;

	  /* If we find X being a compliment of a condition in OLD, 
	     then the entire condition is true.  */
	  if (GET_CODE (y) == reverse_condition (x_code))
	    return const1_rtx;
	}
    }

  /* Otherwise just add to the chain.  */
  return alloc_EXPR_LIST (0, x, old);
}

static rtx
not_reg_cond (x)
     rtx x;
{
  enum rtx_code x_code;
  rtx x_reg;

  /* We expect these conditions to be of the form (eq reg 0).  */
  x_code = GET_CODE (x);
  if (GET_RTX_CLASS (x_code) != '<'
      || GET_CODE (x_reg = XEXP (x, 0)) != REG
      || XEXP (x, 1) != const0_rtx)
    abort ();

  return alloc_EXPR_LIST (0, gen_rtx_fmt_ee (reverse_condition (x_code),
					     VOIDmode, x_reg, const0_rtx),
			  NULL_RTX);
}

static rtx
nand_reg_cond (old, x)
     rtx old, x;
{
  enum rtx_code x_code;
  rtx x_reg;
  rtx c, *prev;

  /* We expect these conditions to be of the form (eq reg 0).  */
  x_code = GET_CODE (x);
  if (GET_RTX_CLASS (x_code) != '<'
      || GET_CODE (x_reg = XEXP (x, 0)) != REG
      || XEXP (x, 1) != const0_rtx)
    abort ();

  /* Search the expression for an existing sub-expression of X_REG.  */

  for (c = *(prev = &old); c ; c = *(prev = &XEXP (c, 1)))
    {
      rtx y = XEXP (c, 0);
      if (REGNO (XEXP (y, 0)) == REGNO (x_reg))
	{
	  /* If we find X already present in OLD, then we need to 
	     splice it out.  */
	  if (GET_CODE (y) == x_code)
	    {
	      *prev = XEXP (c, 1);
	      free_EXPR_LIST_node (c);
	      return old ? old : const0_rtx;
	    }

	  /* If we find X being a compliment of a condition in OLD, 
	     then we need do nothing.  */
	  if (GET_CODE (y) == reverse_condition (x_code))
	    return old;
	}
    }

  /* Otherwise, by implication, the register in question is now live for
     the inverse of the condition X.  */
  return alloc_EXPR_LIST (0, gen_rtx_fmt_ee (reverse_condition (x_code),
					     VOIDmode, x_reg, const0_rtx),
			  old);
}
#endif /* HAVE_conditional_execution */

#ifdef AUTO_INC_DEC

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
  rtx set;

  /* Here we detect use of an index register which might be good for
     postincrement, postdecrement, preincrement, or predecrement.  */

  if (GET_CODE (addr) == PLUS && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    offset = INTVAL (XEXP (addr, 1)), addr = XEXP (addr, 0);

  if (GET_CODE (addr) == REG)
    {
      register rtx y;
      register int size = GET_MODE_SIZE (GET_MODE (x));
      rtx use;
      rtx incr;
      int regno = REGNO (addr);

      /* Is the next use an increment that might make auto-increment? */
      if ((incr = pbi->reg_next_use[regno]) != 0
	  && (set = single_set (incr)) != 0
	  && GET_CODE (set) == SET
	  && BLOCK_NUM (incr) == BLOCK_NUM (insn)
	  /* Can't add side effects to jumps; if reg is spilled and
	     reloaded, there's no way to store back the altered value.  */
	  && GET_CODE (insn) != JUMP_INSN
	  && (y = SET_SRC (set), GET_CODE (y) == PLUS)
	  && XEXP (y, 0) == addr
	  && GET_CODE (XEXP (y, 1)) == CONST_INT
	  && ((HAVE_POST_INCREMENT
	       && (INTVAL (XEXP (y, 1)) == size && offset == 0))
	      || (HAVE_POST_DECREMENT
		  && (INTVAL (XEXP (y, 1)) == - size && offset == 0))
	      || (HAVE_PRE_INCREMENT
		  && (INTVAL (XEXP (y, 1)) == size && offset == size))
	      || (HAVE_PRE_DECREMENT
		  && (INTVAL (XEXP (y, 1)) == - size && offset == - size)))
	  /* Make sure this reg appears only once in this insn.  */
	  && (use = find_use_as_address (PATTERN (insn), addr, offset),
	      use != 0 && use != (rtx) 1))
	{
	  rtx q = SET_DEST (set);
	  enum rtx_code inc_code = (INTVAL (XEXP (y, 1)) == size
				    ? (offset ? PRE_INC : POST_INC)
				    : (offset ? PRE_DEC : POST_DEC));

	  if (dead_or_set_p (incr, addr)
	      /* Mustn't autoinc an eliminable register.  */
	      && (regno >= FIRST_PSEUDO_REGISTER
	          || ! TEST_HARD_REG_BIT (elim_reg_set, regno)))
	    {
	      /* This is the simple case.  Try to make the auto-inc.  If
		 we can't, we are done.  Otherwise, we will do any
		 needed updates below.  */
	      if (! validate_change (insn, &XEXP (x, 0),
				     gen_rtx_fmt_e (inc_code, Pmode, addr),
				     0))
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
	      emit_move_insn (q, addr);
	      insns = get_insns ();
	      end_sequence ();

	      if (basic_block_for_insn)
		for (temp = insns; temp; temp = NEXT_INSN (temp))
		  set_block_for_insn (temp, pbi->bb);

	      /* If we can't make the auto-inc, or can't make the
		 replacement into Y, exit.  There's no point in making
		 the change below if we can't do the auto-inc and doing
		 so is not correct in the pre-inc case.  */

	      validate_change (insn, &XEXP (x, 0),
			       gen_rtx_fmt_e (inc_code, Pmode, q),
			       1);
	      validate_change (incr, &XEXP (y, 0), q, 1);
	      if (! apply_change_group ())
		return;

	      /* We now know we'll be doing this change, so emit the
		 new insn(s) and do the updates.  */
	      emit_insns_before (insns, insn);

	      if (pbi->bb->head == insn)
		pbi->bb->head = insns;

	      /* INCR will become a NOTE and INSN won't contain a
		 use of ADDR.  If a use of ADDR was just placed in
		 the insn before INSN, make that the next use. 
		 Otherwise, invalidate it.  */
	      if (GET_CODE (PREV_INSN (insn)) == INSN
		  && GET_CODE (PATTERN (PREV_INSN (insn))) == SET
		  && SET_SRC (PATTERN (PREV_INSN (insn))) == addr)
		pbi->reg_next_use[regno] = PREV_INSN (insn);
	      else
		pbi->reg_next_use[regno] = 0;

	      addr = q;
	      regno = REGNO (q);

	      /* REGNO is now used in INCR which is below INSN, but it
		 previously wasn't live here.  If we don't mark it as
		 live, we'll put a REG_DEAD note for it on this insn,
		 which is incorrect.  */
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

	  REG_NOTES (insn)
	    = alloc_EXPR_LIST (REG_INC, addr, REG_NOTES (insn));

	  /* Modify the old increment-insn to simply copy
	     the already-incremented value of our register.  */
	  if (! validate_change (incr, &SET_SRC (set), addr, 0))
	    abort ();

	  /* If that makes it a no-op (copying the register into itself) delete
	     it so it won't appear to be a "use" and a "set" of this
	     register.  */
	  if (SET_DEST (set) == addr)
	    {
	      /* If the original source was dead, it's dead now.  */
	      rtx note = find_reg_note (incr, REG_DEAD, NULL_RTX);
	      if (note && XEXP (note, 0) != addr)
		CLEAR_REGNO_REG_SET (pbi->reg_live, REGNO (XEXP (note, 0)));
	      
	      PUT_CODE (incr, NOTE);
	      NOTE_LINE_NUMBER (incr) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (incr) = 0;
	    }

	  if (regno >= FIRST_PSEUDO_REGISTER)
	    {
	      /* Count an extra reference to the reg.  When a reg is
		 incremented, spilling it is worse, so we want to make
		 that less likely.  */
	      REG_N_REFS (regno) += pbi->bb->loop_depth + 1;

	      /* Count the increment as a setting of the register,
		 even though it isn't a SET in rtl.  */
	      REG_N_SETS (regno)++;
	    }
	}
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
  int regno = REGNO (reg);
  int some_was_live = REGNO_REG_SET_P (pbi->reg_live, regno);
  int some_was_dead = ! some_was_live;
  int some_not_set;
  int n;

  /* A hard reg in a wide mode may really be multiple registers.
     If so, mark all of them just like the first.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (--n > 0)
	{
	  int needed_regno = REGNO_REG_SET_P (pbi->reg_live, regno + n);
	  some_was_live |= needed_regno;
	  some_was_dead |= ! needed_regno;
	}
    }

  if (pbi->flags & (PROP_LOG_LINKS | PROP_AUTOINC))
    {
      /* Record where each reg is used, so when the reg is set we know
	 the next insn that uses it.  */
      pbi->reg_next_use[regno] = insn;
    }

  if (pbi->flags & PROP_REG_INFO)
    {
      if (regno < FIRST_PSEUDO_REGISTER)
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

	  if (! (TEST_HARD_REG_BIT (elim_reg_set, regno)
	         && (regno == FRAME_POINTER_REGNUM
		     || regno == ARG_POINTER_REGNUM)))
	    {
	      int n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	      do
		regs_ever_live[regno + --n] = 1;
	      while (n > 0);
	    }
	}
      else
	{
	  /* Keep track of which basic block each reg appears in.  */

	  register int blocknum = pbi->bb->index;
	  if (REG_BASIC_BLOCK (regno) == REG_BLOCK_UNKNOWN)
	    REG_BASIC_BLOCK (regno) = blocknum;
	  else if (REG_BASIC_BLOCK (regno) != blocknum)
	    REG_BASIC_BLOCK (regno) = REG_BLOCK_GLOBAL;

	  /* Count (weighted) number of uses of each reg.  */
	  REG_N_REFS (regno) += pbi->bb->loop_depth + 1;
	}
    }

  /* Find out if any of the register was set this insn.  */
  some_not_set = ! REGNO_REG_SET_P (pbi->new_set, regno);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (--n > 0)
	some_not_set |= ! REGNO_REG_SET_P (pbi->new_set, regno + n);
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
      if (regno < FIRST_PSEUDO_REGISTER
	  && HARD_REGNO_NREGS (regno, GET_MODE (reg)) > 1)
	{
	  n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  while (--n >= 0)
	    some_was_live |= REGNO_REG_SET_P (pbi->new_set, regno + n);
	}

      /* If none of the words in X is needed, make a REG_DEAD note.
	 Otherwise, we must make partial REG_DEAD notes.  */
      if (! some_was_live)
	{
	  if ((pbi->flags & PROP_DEATH_NOTES)
	      && ! find_regno_note (insn, REG_DEAD, regno))
	    REG_NOTES (insn)
	      = alloc_EXPR_LIST (REG_DEAD, reg, REG_NOTES (insn));

	  if (pbi->flags & PROP_REG_INFO)
	    REG_N_DEATHS (regno)++;
	}
      else
	{
	  /* Don't make a REG_DEAD note for a part of a register
	     that is set in the insn.  */

	  n = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg)) - 1;
	  for (; n >= regno; n--)
	    if (! REGNO_REG_SET_P (pbi->reg_live, n)
		&& ! dead_or_set_regno_p (insn, n))
	      REG_NOTES (insn)
		= alloc_EXPR_LIST (REG_DEAD,
				   gen_rtx_REG (reg_raw_mode[n], n),
				   REG_NOTES (insn));
	}
    }

  SET_REGNO_REG_SET (pbi->reg_live, regno);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (--n > 0)
	SET_REGNO_REG_SET (pbi->reg_live, regno + n);
    }

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
	  node = splay_tree_lookup (pbi->reg_cond_dead, regno);
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
	      ncond = nand_reg_cond (ncond, cond);

	      /* If the register is now unconditionally live, remove the
		 entry in the splay_tree.  */
	      if (ncond == const0_rtx)
		{
		  rcli->condition = NULL_RTX;
		  splay_tree_remove (pbi->reg_cond_dead, regno);
		}
	      else
		rcli->condition = ncond;
	    }
	}
      else
	{
	  /* The register was not previously live at all.  Record
	     the condition under which it is still dead.  */
	  rcli = (struct reg_cond_life_info *) xmalloc (sizeof (*rcli));
	  rcli->condition = not_reg_cond (cond);
	  splay_tree_insert (pbi->reg_cond_dead, regno,
			     (splay_tree_value) rcli);
	}
    }
#endif
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
      if (flags & PROP_SCAN_DEAD_CODE)
	{
          /* Invalidate the data for the last MEM stored, but only if MEM is
	     something that can be stored into.  */
          if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	      && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	    ; /* needn't clear the memory set list */
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
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) >= FIRST_PSEUDO_REGISTER
	  && (GET_MODE_SIZE (GET_MODE (x))
	      != GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
	REG_CHANGES_SIZE (REGNO (SUBREG_REG (x))) = 1;

      /* While we're here, optimize this case.  */
      x = SUBREG_REG (x);
      if (GET_CODE (x) != REG)
	goto retry;
      /* FALLTHRU */

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
	    if (GET_CODE (testreg) == SUBREG
		&& GET_CODE (SUBREG_REG (testreg)) == REG
		&& REGNO (SUBREG_REG (testreg)) >= FIRST_PSEUDO_REGISTER
		&& (GET_MODE_SIZE (GET_MODE (testreg))
		    != GET_MODE_SIZE (GET_MODE (SUBREG_REG (testreg)))))
	      REG_CHANGES_SIZE (REGNO (SUBREG_REG (testreg))) = 1;

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

	/* If this is a store into a register, recursively scan the
	   value being stored.  */

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
	  free_EXPR_LIST_list (&pbi->mem_set_list);

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
    register const char *fmt = GET_RTX_FORMAT (code);
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
      && BLOCK_NUM (y) == BLOCK_NUM (insn)
      /* Don't do this if the reg dies, or gets set in y; a standard addressing
	 mode would be better.  */
      && ! dead_or_set_p (y, SET_DEST (x))
      && try_pre_increment (y, SET_DEST (x), amount))
    {
      /* We have found a suitable auto-increment
	 and already changed insn Y to do it.
	 So flush this increment-instruction.  */
      PUT_CODE (insn, NOTE);
      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
      NOTE_SOURCE_FILE (insn) = 0;
      /* Count a reference to this reg for the increment
	 insn we are deleting.  When a reg is incremented.
	 spilling it is worse, so we want to make that
	 less likely.  */
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  REG_N_REFS (regno) += pbi->bb->loop_depth + 1;
	  REG_N_SETS (regno)++;
	}
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
  const char *fmt = GET_RTX_FORMAT (code);
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
	if (REGNO_POINTER_FLAG (i))
	  fprintf (file, "; pointer");
	fprintf (file, ".\n");
      }

  fprintf (file, "\n%d basic blocks, %d edges.\n", n_basic_blocks, n_edges);
  for (i = 0; i < n_basic_blocks; i++)
    {
      register basic_block bb = BASIC_BLOCK (i);
      register edge e;

      fprintf (file, "\nBasic block %d: first insn %d, last %d, loop_depth %d.\n",
	       i, INSN_UID (bb->head), INSN_UID (bb->end), bb->loop_depth);

      fprintf (file, "Predecessors: ");
      for (e = bb->pred; e ; e = e->pred_next)
	dump_edge_info (file, e, 0);

      fprintf (file, "\nSuccessors: ");
      for (e = bb->succ; e ; e = e->succ_next)
	dump_edge_info (file, e, 1);

      fprintf (file, "\nRegisters live at start:");
      dump_regset (bb->global_live_at_start, file);

      fprintf (file, "\nRegisters live at end:");
      dump_regset (bb->global_live_at_end, file);

      putc('\n', file);
    }

  putc('\n', file);
}

void
debug_flow_info ()
{
  dump_flow_info (stderr);
}

static void
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

  if (e->flags)
    {
      static const char * const bitnames[] = {
	"fallthru", "crit", "ab", "abcall", "eh", "fake"
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
	    if (i < (int)(sizeof (bitnames) / sizeof (*bitnames)))
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

  fprintf (outf, ";; Basic block %d, loop depth %d",
	   bb->index, bb->loop_depth);
  if (bb->eh_beg != -1 || bb->eh_end != -1)
    fprintf (outf, ", eh regions %d/%d", bb->eh_beg, bb->eh_end);
  putc ('\n', outf);

  fputs (";; Predecessors: ", outf);
  for (e = bb->pred; e ; e = e->pred_next)
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
  dump_bb (BASIC_BLOCK(n), stderr);
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
	      if (in_bb_p[INSN_UID(x)] == NOT_IN_BB)
		state = IN_ONE_BB;
	      in_bb_p[INSN_UID(x)] = state;

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

	  if (in_bb_p[INSN_UID(tmp_rtx)] == NOT_IN_BB
	      && GET_CODE (tmp_rtx) != NOTE
	      && GET_CODE (tmp_rtx) != BARRIER)
	    fprintf (outf, ";; Insn is not within a basic block\n");
	  else if (in_bb_p[INSN_UID(tmp_rtx)] == IN_MULTIPLE_BB)
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

/* Compute dominator relationships using new flow graph structures.  */
void
compute_flow_dominators (dominators, post_dominators)
     sbitmap *dominators;
     sbitmap *post_dominators;
{
  int bb;
  sbitmap *temp_bitmap;
  edge e;
  basic_block *worklist, *workend, *qin, *qout;
  int qlen;

  /* Allocate a worklist array/queue.  Entries are only added to the
     list if they were not already on the list.  So the size is
     bounded by the number of basic blocks.  */
  worklist = (basic_block *) xmalloc (sizeof (basic_block) * n_basic_blocks);
  workend = &worklist[n_basic_blocks];

  temp_bitmap = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  sbitmap_vector_zero (temp_bitmap, n_basic_blocks);

  if (dominators)
    {
      /* The optimistic setting of dominators requires us to put every
	 block on the work list initially.  */
      qin = qout = worklist;
      for (bb = 0; bb < n_basic_blocks; bb++)
	{
	  *qin++ = BASIC_BLOCK (bb);
	  BASIC_BLOCK (bb)->aux = BASIC_BLOCK (bb);
	}
      qlen = n_basic_blocks;
      qin = worklist;

      /* We want a maximal solution, so initially assume everything dominates
	 everything else.  */
      sbitmap_vector_ones (dominators, n_basic_blocks);

      /* Mark successors of the entry block so we can identify them below.  */
      for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
	e->dest->aux = ENTRY_BLOCK_PTR;

      /* Iterate until the worklist is empty.  */
      while (qlen)
	{
	  /* Take the first entry off the worklist.  */
	  basic_block b = *qout++;
	  if (qout >= workend)
	    qout = worklist;
	  qlen--;

	  bb = b->index;

	  /* Compute the intersection of the dominators of all the
	     predecessor blocks.

	     If one of the predecessor blocks is the ENTRY block, then the
	     intersection of the dominators of the predecessor blocks is
	     defined as the null set.  We can identify such blocks by the
	     special value in the AUX field in the block structure.  */
	  if (b->aux == ENTRY_BLOCK_PTR)
	    {
	      /* Do not clear the aux field for blocks which are
		 successors of the ENTRY block.  That way we we never
		 add them to the worklist again.

		 The intersect of dominators of the preds of this block is
		 defined as the null set.  */
	      sbitmap_zero (temp_bitmap[bb]);
	    }
	  else
	    {
	      /* Clear the aux field of this block so it can be added to
		 the worklist again if necessary.  */
	      b->aux = NULL;
	      sbitmap_intersection_of_preds (temp_bitmap[bb], dominators, bb);
	    }

	  /* Make sure each block always dominates itself.  */
	  SET_BIT (temp_bitmap[bb], bb);

	  /* If the out state of this block changed, then we need to
	     add the successors of this block to the worklist if they
	     are not already on the worklist.  */
	  if (sbitmap_a_and_b (dominators[bb], dominators[bb], temp_bitmap[bb]))
	    {
	      for (e = b->succ; e; e = e->succ_next)
		{
		  if (!e->dest->aux && e->dest != EXIT_BLOCK_PTR)
		    {
		      *qin++ = e->dest;
		      if (qin >= workend)
			qin = worklist;
		      qlen++;

		      e->dest->aux = e;
		    }
		}
	    }
	}
    }

  if (post_dominators)
    {
      /* The optimistic setting of dominators requires us to put every
	 block on the work list initially.  */
      qin = qout = worklist;
      for (bb = 0; bb < n_basic_blocks; bb++)
	{
	  *qin++ = BASIC_BLOCK (bb);
	  BASIC_BLOCK (bb)->aux = BASIC_BLOCK (bb);
	}
      qlen = n_basic_blocks;
      qin = worklist;

      /* We want a maximal solution, so initially assume everything post
	 dominates everything else.  */
      sbitmap_vector_ones (post_dominators, n_basic_blocks);

      /* Mark predecessors of the exit block so we can identify them below.  */
      for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
	e->src->aux = EXIT_BLOCK_PTR;

      /* Iterate until the worklist is empty.  */
      while (qlen)
	{
	  /* Take the first entry off the worklist.  */
	  basic_block b = *qout++;
	  if (qout >= workend)
	    qout = worklist;
	  qlen--;

	  bb = b->index;

	  /* Compute the intersection of the post dominators of all the
	     successor blocks.

	     If one of the successor blocks is the EXIT block, then the
	     intersection of the dominators of the successor blocks is
	     defined as the null set.  We can identify such blocks by the
	     special value in the AUX field in the block structure.  */
	  if (b->aux == EXIT_BLOCK_PTR)
	    {
	      /* Do not clear the aux field for blocks which are
		 predecessors of the EXIT block.  That way we we never
		 add them to the worklist again.

		 The intersect of dominators of the succs of this block is
		 defined as the null set.  */
	      sbitmap_zero (temp_bitmap[bb]);
	    }
	  else
	    {
	      /* Clear the aux field of this block so it can be added to
		 the worklist again if necessary.  */
	      b->aux = NULL;
	      sbitmap_intersection_of_succs (temp_bitmap[bb],
					     post_dominators, bb);
	    }

	  /* Make sure each block always post dominates itself.  */
	  SET_BIT (temp_bitmap[bb], bb);

	  /* If the out state of this block changed, then we need to
	     add the successors of this block to the worklist if they
	     are not already on the worklist.  */
	  if (sbitmap_a_and_b (post_dominators[bb],
			       post_dominators[bb],
			       temp_bitmap[bb]))
	    {
	      for (e = b->pred; e; e = e->pred_next)
		{
		  if (!e->src->aux && e->src != ENTRY_BLOCK_PTR)
		    {
		      *qin++ = e->src;
		      if (qin >= workend)
			qin = worklist;
		      qlen++;

		      e->src->aux = e;
		    }
		}
	    }
	}
    }

  free (worklist);
  free (temp_bitmap);
}

/* Given DOMINATORS, compute the immediate dominators into IDOM.  */

void
compute_immediate_dominators (idom, dominators)
     int *idom;
     sbitmap *dominators;
{
  sbitmap *tmp;
  int b;

  tmp = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);

  /* Begin with tmp(n) = dom(n) - { n }.  */
  for (b = n_basic_blocks; --b >= 0; )
    {
      sbitmap_copy (tmp[b], dominators[b]);
      RESET_BIT (tmp[b], b);
    }

  /* Subtract out all of our dominator's dominators.  */
  for (b = n_basic_blocks; --b >= 0; )
    {
      sbitmap tmp_b = tmp[b];
      int s;

      for (s = n_basic_blocks; --s >= 0; )
	if (TEST_BIT (tmp_b, s))
	  sbitmap_difference (tmp_b, tmp_b, tmp[s]);
    }

  /* Find the one bit set in the bitmap and put it in the output array.  */
  for (b = n_basic_blocks; --b >= 0; )
    {
      int t;
      EXECUTE_IF_SET_IN_SBITMAP (tmp[b], 0, t, { idom[b] = t; });
    }

  sbitmap_vector_free (tmp);
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

      for (insn = bb->head; ; insn = NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
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
		      /* FALLTHRU */

		    case REG_UNUSED:
		      if (kill)
			{
			  rtx next = XEXP (link, 1);
			  free_EXPR_LIST_node (link);
		          *pprev = link = next;
		          break;
			}
		      /* FALLTHRU */

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

/* Record INSN's block number as BB.  */
/* ??? This has got to go.  */

void
set_block_num (insn, bb)
     rtx insn;
     int bb;
{
  set_block_for_insn (insn, BASIC_BLOCK (bb));
}

/* Verify the CFG consistency.  This function check some CFG invariants and
   aborts when something is wrong.  Hope that this function will help to
   convert many optimization passes to preserve CFG consistent.

   Currently it does following checks: 

   - test head/end pointers
   - overlapping of basic blocks
   - edge list corectness
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
  basic_block *bb_info;
  rtx x;
  int i, last_bb_num_seen, num_bb_notes, err = 0;

  bb_info = (basic_block *) xcalloc (max_uid, sizeof (basic_block));

  /* First pass check head/end pointers and set bb_info array used by
     later passes.  */
  for (i = n_basic_blocks - 1; i >= 0; i--)
    {
      basic_block bb = BASIC_BLOCK (i);

      /* Check the head pointer and make sure that it is pointing into
         insn list.  */
      for (x = rtx_first; x != NULL_RTX; x = NEXT_INSN (x))
	if (x == bb->head)
	  break;
      if (!x)
	{
	  error ("Head insn %d for block %d not found in the insn stream.",
		 INSN_UID (bb->head), bb->index);
	  err = 1;
	}

      /* Check the end pointer and make sure that it is pointing into
         insn list.  */
      for (x = bb->head; x != NULL_RTX; x = NEXT_INSN (x))
	{
	  if (bb_info[INSN_UID (x)] != NULL)
	    {
	      error ("Insn %d is in multiple basic blocks (%d and %d)",
		     INSN_UID (x), bb->index, bb_info[INSN_UID (x)]->index);
	      err = 1;
	    }
	  bb_info[INSN_UID (x)] = bb;

	  if (x == bb->end)
	    break;
	}
      if (!x)
	{
	  error ("End insn %d for block %d not found in the insn stream.",
		 INSN_UID (bb->end), bb->index);
	  err = 1;
	}
    }

  /* Now check the basic blocks (boundaries etc.) */
  for (i = n_basic_blocks - 1; i >= 0; i--)
    {
      basic_block bb = BASIC_BLOCK (i);
      /* Check corectness of edge lists */
      edge e;

      e = bb->succ;
      while (e)
	{
	  if (e->src != bb)
	    {
	      fprintf (stderr, "verify_flow_info: Basic block %d succ edge is corrupted\n",
		       bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fflush (stderr);
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
      if (GET_CODE (x) != NOTE
	  || NOTE_LINE_NUMBER (x) != NOTE_INSN_BASIC_BLOCK
	  || NOTE_BASIC_BLOCK (x) != bb)
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
	      if (GET_CODE (x) == NOTE
		  && NOTE_LINE_NUMBER (x) == NOTE_INSN_BASIC_BLOCK)
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
      if (GET_CODE (x) == NOTE
	  && NOTE_LINE_NUMBER (x) == NOTE_INSN_BASIC_BLOCK)
	{
	  basic_block bb = NOTE_BASIC_BLOCK (x);
	  num_bb_notes++;
	  if (bb->index != last_bb_num_seen + 1)
	    fatal ("Basic blocks not numbered consecutively");
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

      if (GET_RTX_CLASS (GET_CODE (x)) == 'i'
	  && GET_CODE (x) == JUMP_INSN
	  && returnjump_p (x) && ! condjump_p (x)
	  && ! (NEXT_INSN (x) && GET_CODE (NEXT_INSN (x)) == BARRIER))
	    fatal_insn ("Return not followed by barrier", x);

      x = NEXT_INSN (x);
    }

  if (num_bb_notes != n_basic_blocks)
    fatal ("number of bb notes in insn chain (%d) != n_basic_blocks (%d)",
	   num_bb_notes, n_basic_blocks);

  if (err)
    abort ();

  /* Clean up.  */
  free (bb_info);
}

/* Functions to access an edge list with a vector representation.
   Enough data is kept such that given an index number, the 
   pred and succ that edge reprsents can be determined, or
   given a pred and a succ, it's index number can be returned.
   This allows algorithms which comsume a lot of memory to 
   represent the normally full matrix of edge (pred,succ) with a
   single indexed vector,  edge (EDGE_INDEX (pred, succ)), with no
   wasted space in the client code due to sparse flow graphs.  */

/* This functions initializes the edge list. Basically the entire 
   flowgraph is processed, and all edges are assigned a number,
   and the data structure is filed in.  */
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
  fprintf(f, "Compressed edge list, %d BBs + entry & exit, and %d edges\n",
	  elist->num_blocks - 2, elist->num_edges);

  for (x = 0; x < elist->num_edges; x++)
    {
      fprintf (f, " %-4d - edge(", x);
      if (INDEX_EDGE_PRED_BB (elist, x) == ENTRY_BLOCK_PTR)
        fprintf (f,"entry,");
      else
        fprintf (f,"%d,", INDEX_EDGE_PRED_BB (elist, x)->index);

      if (INDEX_EDGE_SUCC_BB (elist, x) == EXIT_BLOCK_PTR)
        fprintf (f,"exit)\n");
      else
        fprintf (f,"%d)\n", INDEX_EDGE_SUCC_BB (elist, x)->index);
    }
}

/* This function provides an internal consistancy check of an edge list,
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
	      fprintf (f, "*p* No index for edge from %d to %d\n",pred, succ);
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
	  fprintf (f, "*p* No index for edge from %d to %d\n",pred, succ);
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
  
  for (pred = 0 ; pred < n_basic_blocks; pred++)
    for (succ = 0 ; succ < n_basic_blocks; succ++)
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
    for (succ = 0 ; succ < n_basic_blocks; succ++)
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
    for (pred = 0 ; pred < n_basic_blocks; pred++)
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
  for (e = bb->succ; e ; )
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

/* This functions will add a fake edge between any block which has no
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

/* Dump the list of basic blocks in the bitmap NODES.  */
static void 
flow_nodes_print (str, nodes, file)
     const char *str;
     const sbitmap nodes;
     FILE *file;
{
  int node;

  fprintf (file, "%s { ", str);
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {fprintf (file, "%d ", node);});
  fputs ("}\n", file);
}


/* Dump the list of exiting edges in the array EDGES.  */
static void 
flow_exits_print (str, edges, num_edges, file)
     const char *str;
     const edge *edges;
     int num_edges;
     FILE *file;
{
  int i;

  fprintf (file, "%s { ", str);
  for (i = 0; i < num_edges; i++)
    fprintf (file, "%d->%d ", edges[i]->src->index, edges[i]->dest->index);
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
}


/* Return non-zero if the nodes of LOOP are a subset of OUTER.  */
static int
flow_loop_nested_p (outer, loop)
     struct loop *outer;
     struct loop *loop;
{
  return sbitmap_a_subset_b_p (loop->nodes, outer->nodes);
}


/* Dump the loop information specified by LOOPS to the stream FILE.  */
void 
flow_loops_dump (loops, file, verbose)
     const struct loops *loops;
     FILE *file;
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

      fprintf (file, ";; loop %d (%d to %d):\n;;   header %d, latch %d, pre-header %d, depth %d, level %d, outer %ld\n",
	       i, INSN_UID (loop->header->head), INSN_UID (loop->latch->end),
	       loop->header->index, loop->latch->index,
	       loop->pre_header ? loop->pre_header->index : -1, 
	       loop->depth, loop->level,
	       (long) (loop->outer ? (loop->outer - loops->array) : -1));
      fprintf (file, ";;   %d", loop->num_nodes);
      flow_nodes_print (" nodes", loop->nodes, file);
      fprintf (file, ";;   %d", loop->num_exits);
      flow_exits_print (" exits", loop->exits, loop->num_exits, file);

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
		  fprintf (file, ";; loop header %d shared by loops %d, %d %s\n",
			   loop->header->index, i, j,
			   disjoint ? "disjoint" : "nested");
		}
	    }
	}

      if (verbose)
	{
	  /* Print diagnostics to compare our concept of a loop with
	     what the loop notes say.  */
	  if (GET_CODE (PREV_INSN (loop->first->head)) != NOTE
	      || NOTE_LINE_NUMBER (PREV_INSN (loop->first->head))
	      != NOTE_INSN_LOOP_BEG)
	    fprintf (file, ";; No NOTE_INSN_LOOP_BEG at %d\n", 
		     INSN_UID (PREV_INSN (loop->first->head)));
	  if (GET_CODE (NEXT_INSN (loop->last->end)) != NOTE
	      || NOTE_LINE_NUMBER (NEXT_INSN (loop->last->end))
	      != NOTE_INSN_LOOP_END)
	    fprintf (file, ";; No NOTE_INSN_LOOP_END at %d\n",
		     INSN_UID (NEXT_INSN (loop->last->end)));
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
	  
	  if (loop->nodes)
	    sbitmap_free (loop->nodes);
	  if (loop->exits)
	    free (loop->exits);
	}
      free (loops->array);
      loops->array = NULL;
      
      if (loops->cfg.dom)
	sbitmap_vector_free (loops->cfg.dom);
      if (loops->cfg.dfs_order)
	free (loops->cfg.dfs_order);

      sbitmap_free (loops->shared_headers);
    }
}


/* Find the exits from the loop using the bitmap of loop nodes NODES
   and store in EXITS array.  Return the number of exits from the
   loop.  */
static int
flow_loop_exits_find (nodes, exits)
     const sbitmap nodes;
     edge **exits;
{
  edge e;
  int node;
  int num_exits;

  *exits = NULL;

  /* Check all nodes within the loop to see if there are any
     successors not in the loop.  Note that a node may have multiple
     exiting edges.  */
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

  *exits = (edge *) xmalloc (num_exits * sizeof (edge *));

  /* Store all exiting edges into an array.  */
  num_exits = 0;
  EXECUTE_IF_SET_IN_SBITMAP (nodes, 0, node, {
    for (e = BASIC_BLOCK (node)->succ; e; e = e->succ_next)
      {
	basic_block dest = e->dest;	  

	if (dest == EXIT_BLOCK_PTR || ! TEST_BIT (nodes, dest->index))
	  (*exits)[num_exits++] = e;
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
   DFS_ORDER, marking the nodes visited in VISITED.  Returns the
   number of nodes visited.  */
static int
flow_depth_first_order_compute (dfs_order)
     int *dfs_order;
{
  edge e;
  edge *stack;
  int sp;
  int dfsnum = 0;
  sbitmap visited;

  /* Allocate stack for back-tracking up CFG.  */
  stack = (edge *) xmalloc (n_basic_blocks * sizeof (edge));
  sp = 0;

  /* Allocate bitmap to track nodes that have been visited.  */
  visited = sbitmap_alloc (n_basic_blocks);

  /* None of the nodes in the CFG have been visited yet.  */
  sbitmap_zero (visited);
  
  /* Start with the first successor edge from the entry block.  */
  e = ENTRY_BLOCK_PTR->succ;
  while (e)
    {
      basic_block src = e->src;
      basic_block dest = e->dest;
      
      /* Mark that we have visited this node.  */
      if (src != ENTRY_BLOCK_PTR)
	SET_BIT (visited, src->index);

      /* If this node has not been visited before, push the current
	 edge on to the stack and proceed with the first successor
	 edge of this node.  */
      if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index)
	  && dest->succ)
	{
	  stack[sp++] = e;
	  e = dest->succ;
	}
      else
	{
	  if (dest != EXIT_BLOCK_PTR && ! TEST_BIT (visited, dest->index)
	      && ! dest->succ)
	    {
	      /* DEST has no successors (for example, a non-returning
                 function is called) so do not push the current edge
                 but carry on with its next successor.  */
	      dfs_order[dest->index] = n_basic_blocks - ++dfsnum;
	      SET_BIT (visited, dest->index);
	    }

	  while (! e->succ_next && src != ENTRY_BLOCK_PTR)
	    {
	      dfs_order[src->index] = n_basic_blocks - ++dfsnum;

	      /* Pop edge off stack.  */
	      e = stack[--sp];
	      src = e->src;
	    }
	  e = e->succ_next;
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
  loops->tree = &loops->array[0];
  loops->tree->outer = loops->tree->inner = loops->tree->next = NULL;

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
  for (loop = loops->tree; loop; loop = loop->next)
    {
      level = flow_loop_level_compute (loop, 1);
      if (level > levels)
	levels = level;
    }
  return levels;
}


/* Find all the natural loops in the function and save in LOOPS structure
   and recalculate loop_depth information in basic block structures.
   Return the number of natural loops found.  */

int 
flow_loops_find (loops)
       struct loops *loops;
{
  int i;
  int b;
  int num_loops;
  edge e;
  sbitmap headers;
  sbitmap *dom;
  int *dfs_order;
  
  loops->num = 0;
  loops->array = NULL;
  loops->tree = NULL;
  dfs_order = NULL;

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks == 0)
    return 0;

  /* Compute the dominators.  */
  dom = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  compute_flow_dominators (dom, NULL);

  /* Count the number of loop edges (back edges).  This should be the
     same as the number of natural loops.  Also clear the loop_depth
     and as we work from inner->outer in a loop nest we call
     find_loop_nodes_find which will increment loop_depth for nodes
     within the current loop, which happens to enclose inner loops.  */

  num_loops = 0;
  for (b = 0; b < n_basic_blocks; b++)
    {
      BASIC_BLOCK (b)->loop_depth = 0;
      for (e = BASIC_BLOCK (b)->pred; e; e = e->pred_next)
	{
	  basic_block latch = e->src;
	  
	  /* Look for back edges where a predecessor is dominated
	     by this block.  A natural loop has a single entry
	     node (header) that dominates all the nodes in the
	     loop.  It also has single back edge to the header
	     from a latch node.  Note that multiple natural loops
	     may share the same header.  */
	  if (latch != ENTRY_BLOCK_PTR && TEST_BIT (dom[latch->index], b))
	    num_loops++;
	}
    }
  
  if (num_loops)
    {
      /* Compute depth first search order of the CFG so that outer
	 natural loops will be found before inner natural loops.  */
      dfs_order = (int *) xmalloc (n_basic_blocks * sizeof (int));
      flow_depth_first_order_compute (dfs_order);

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

	  /* Search the nodes of the CFG in DFS order that we can find
	     outer loops first.  */
	  header = BASIC_BLOCK (dfs_order[b]);
	  
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
		  
		  /* Keep track of blocks that are loop headers so
		     that we can tell which loops should be merged.  */
		  if (TEST_BIT (headers, header->index))
		    SET_BIT (loops->shared_headers, header->index);
		  SET_BIT (headers, header->index);
		  
		  /* Find nodes contained within the loop.  */
		  loop->nodes = sbitmap_alloc (n_basic_blocks);
		  loop->num_nodes
		    = flow_loop_nodes_find (header, latch, loop->nodes);

		  /* Compute first and last blocks within the loop.
		     These are often the same as the loop header and
		     loop latch respectively, but this is not always
		     the case.  */
		  loop->first
		    = BASIC_BLOCK (sbitmap_first_set_bit (loop->nodes));
		  loop->last
		    = BASIC_BLOCK (sbitmap_last_set_bit (loop->nodes));	
	  
		  /* Find edges which exit the loop.  Note that a node
		     may have several exit edges.  */
		  loop->num_exits
		    = flow_loop_exits_find (loop->nodes, &loop->exits);

		  /* Look to see if the loop has a pre-header node.  */
		  loop->pre_header 
		    = flow_loop_pre_header_find (header, dom);

		  num_loops++;
		}
	    }
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

  loops->num = num_loops;

  /* Save CFG derived information to avoid recomputing it.  */
  loops->cfg.dom = dom;
  loops->cfg.dfs_order = dfs_order;

  /* Build the loop hierarchy tree.  */
  flow_loops_tree_build (loops);

  /* Assign the loop nesting depth and enclosed loop level for each
     loop.  */
  loops->levels = flow_loops_level_compute (loops);

  return num_loops;
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


/* Clear LOG_LINKS fields of insns in a chain.  */
void
clear_log_links (insns)
     rtx insns;
{
  rtx i;
  for (i = insns; i; i = NEXT_INSN (i))
    if (GET_RTX_CLASS (GET_CODE (i)) == 'i')
      LOG_LINKS (i) = 0;
}
