/* Data flow analysis for GNU compiler.
   Copyright (C) 1987, 88, 92-98, 1999 Free Software Foundation, Inc.

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
   register usage: reg_n_refs, reg_n_deaths, reg_n_sets, reg_live_length,
   reg_n_calls_crosses and reg_basic_block.

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
#include "rtl.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "except.h"
#include "toplev.h"
#include "recog.h"
#include "insn-flags.h"

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free


/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#ifndef EXIT_IGNORE_STACK
#define EXIT_IGNORE_STACK 0
#endif


/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.
   For top-level functions, this is temporary_obstack.
   Separate obstacks are made for nested functions.  */

extern struct obstack *function_obstack;

/* List of labels that must never be deleted.  */
extern rtx forced_labels;

/* Number of basic blocks in the current function.  */

int n_basic_blocks;

/* The basic block array.  */

varray_type basic_block_info;

/* The special entry and exit blocks.  */

struct basic_block_def entry_exit_blocks[2] = 
{
  {
    NULL,			/* head */
    NULL,			/* end */
    NULL,			/* pred */
    NULL,			/* succ */
    NULL,			/* local_set */
    NULL,			/* global_live_at_start */
    NULL,			/* global_live_at_end */
    NULL,			/* aux */
    ENTRY_BLOCK,		/* index */
    0				/* loop_depth */
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
    0				/* loop_depth */
  }
};

/* Nonzero if the second flow pass has completed.  */
int flow2_completed;

/* Maximum register number used in this function, plus one.  */

int max_regno;

/* Indexed by n, giving various register information */

varray_type reg_n_info;

/* Size of the reg_n_info table.  */

unsigned int reg_n_max;

/* Element N is the next insn that uses (hard or pseudo) register number N
   within the current basic block; or zero, if there is no such insn.
   This is valid only during the final backward scan in propagate_block.  */

static rtx *reg_next_use;

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

/* Depth within loops of basic block being scanned for lifetime analysis,
   plus one.  This is the weight attached to references to registers.  */

static int loop_depth;

/* During propagate_block, this is non-zero if the value of CC0 is live.  */

static int cc0_live;

/* During propagate_block, this contains a list of all the MEMs we are
   tracking for dead store elimination. 

   ?!? Note we leak memory by not free-ing items on this list.  We need to
   write some generic routines to operate on memory lists since cse, gcse,
   loop, sched, flow and possibly other passes all need to do basically the
   same operations on these lists.  */

static rtx mem_set_list;

/* Set of registers that may be eliminable.  These are handled specially
   in updating regs_ever_live.  */

static HARD_REG_SET elim_reg_set;

/* The basic block structure for every insn, indexed by uid.  */

varray_type basic_block_for_insn;

/* The labels mentioned in non-jump rtl.  Valid during find_basic_blocks.  */
/* ??? Should probably be using LABEL_NUSES instead.  It would take a 
   bit of surgery to be able to use or co-opt the routines in jump.  */

static rtx label_value_list;

/* INSN_VOLATILE (insn) is 1 if the insn refers to anything volatile.  */

#define INSN_VOLATILE(INSN) bitmap_bit_p (uid_volatile, INSN_UID (INSN))
#define SET_INSN_VOLATILE(INSN) bitmap_set_bit (uid_volatile, INSN_UID (INSN))
static bitmap uid_volatile;

/* Forward declarations */
static int count_basic_blocks		PROTO((rtx));
static rtx find_basic_blocks_1		PROTO((rtx, rtx*));
static void create_basic_block		PROTO((int, rtx, rtx, rtx));
static void compute_bb_for_insn		PROTO((varray_type, int));
static void clear_edges			PROTO((void));
static void make_edges			PROTO((rtx, rtx*));
static void make_edge			PROTO((basic_block, basic_block, int));
static void make_label_edge		PROTO((basic_block, rtx, int));
static void mark_critical_edges		PROTO((void));

static void commit_one_edge_insertion	PROTO((edge));

static void delete_unreachable_blocks	PROTO((void));
static void delete_eh_regions		PROTO((void));
static int can_delete_note_p		PROTO((rtx));
static void delete_insn_chain		PROTO((rtx, rtx));
static int delete_block			PROTO((basic_block));
static void expunge_block		PROTO((basic_block));
static rtx flow_delete_insn		PROTO((rtx));
static int can_delete_label_p		PROTO((rtx));
static void merge_blocks_nomove		PROTO((basic_block, basic_block));
static int merge_blocks			PROTO((edge,basic_block,basic_block));
static void tidy_fallthru_edge		PROTO((edge,basic_block,basic_block));
static void calculate_loop_depth	PROTO((rtx));

static int set_noop_p			PROTO((rtx));
static int noop_move_p			PROTO((rtx));
static void notice_stack_pointer_modification PROTO ((rtx, rtx));
static void record_volatile_insns	PROTO((rtx));
static void mark_regs_live_at_end	PROTO((regset));
static void life_analysis_1		PROTO((rtx, int, int));
static void init_regset_vector		PROTO ((regset *, int,
						struct obstack *));
static void propagate_block		PROTO((regset, rtx, rtx, int, 
					       regset, int, int));
static int insn_dead_p			PROTO((rtx, regset, int, rtx));
static int libcall_dead_p		PROTO((rtx, regset, rtx, rtx));
static void mark_set_regs		PROTO((regset, regset, rtx,
					       rtx, regset));
static void mark_set_1			PROTO((regset, regset, rtx,
					       rtx, regset));
#ifdef AUTO_INC_DEC
static void find_auto_inc		PROTO((regset, rtx, rtx));
static int try_pre_increment_1		PROTO((rtx));
static int try_pre_increment		PROTO((rtx, rtx, HOST_WIDE_INT));
#endif
static void mark_used_regs		PROTO((regset, regset, rtx, int, rtx));
void dump_flow_info			PROTO((FILE *));
static void dump_edge_info		PROTO((FILE *, edge, int));

static int_list_ptr alloc_int_list_node PROTO ((int_list_block **));
static int_list_ptr add_int_list_node   PROTO ((int_list_block **,
						int_list **, int));

static void add_pred_succ		PROTO ((int, int, int_list_ptr *,
						int_list_ptr *, int *, int *));

static void count_reg_sets_1		PROTO ((rtx));
static void count_reg_sets		PROTO ((rtx));
static void count_reg_references	PROTO ((rtx));
static void notice_stack_pointer_modification PROTO ((rtx, rtx));
static void invalidate_mems_from_autoinc	PROTO ((rtx));
void verify_flow_info			PROTO ((void));

/* Find basic blocks of the current function.
   F is the first insn of the function and NREGS the number of register
   numbers in use.  */

void
find_basic_blocks (f, nregs, file, do_cleanup)
     rtx f;
     int nregs ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     int do_cleanup;
{
  rtx *bb_eh_end;
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

  /* An array to record the active exception region at the end of each
     basic block.  It is filled in by find_basic_blocks_1 for make_edges.  */
  bb_eh_end = (rtx *) alloca (n_basic_blocks * sizeof (rtx));

  label_value_list = find_basic_blocks_1 (f, bb_eh_end);
  
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

  VARRAY_BB_INIT (basic_block_for_insn, max_uid, "basic_block_for_insn");
  compute_bb_for_insn (basic_block_for_insn, max_uid);

  /* Discover the edges of our cfg.  */

  make_edges (label_value_list, bb_eh_end);

  /* Delete unreachable blocks.  */

  if (do_cleanup)
    delete_unreachable_blocks ();

  /* Mark critical edges.  */

  mark_critical_edges ();

  /* Discover the loop depth at the start of each basic block to aid
     register allocation.  */
  calculate_loop_depth (f);

  /* Kill the data we won't maintain.  */
  label_value_list = 0;

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
  rtx prev_call = NULL_RTX;

  prev_code = JUMP_INSN;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      register RTX_CODE code = GET_CODE (insn);

      if (code == CODE_LABEL
	  || (GET_RTX_CLASS (code) == 'i'
	      && (prev_code == JUMP_INSN
		  || prev_code == BARRIER
		  || (prev_code == CALL_INSN && call_had_abnormal_edge))))
	{
	  count++;

	  /* If the previous insn was a call that did not create an
	     abnormal edge, we want to add a nop so that the CALL_INSN
	     itself is not at basic_block_end.  This allows us to
	     easily distinguish between normal calls and those which
	     create abnormal edges in the flow graph.  */

	  if (count > 0 && prev_call != 0 && !call_had_abnormal_edge)
	    {
	      rtx nop = gen_rtx_USE (VOIDmode, const0_rtx);
	      emit_insn_after (nop, prev_call);
	    }
	}

      /* Record whether this call created an edge.  */
      if (code == CALL_INSN)
	{
	  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	  int region = (note ? XINT (XEXP (note, 0), 0) : 1);
	  prev_call = insn;
	  call_had_abnormal_edge = 0;

	  /* If there is a specified EH region, we have an edge.  */
	  if (eh_region && region > 0)
	    call_had_abnormal_edge = 1;
	  else
	    {
	      /* If there is a nonlocal goto label and the specified
		 region number isn't -1, we have an edge. (0 means
		 no throw, but might have a nonlocal goto).  */
	      if (nonlocal_goto_handler_labels && region >= 0)
		call_had_abnormal_edge = 1;
	    }
	}
      else if (code != NOTE)
	prev_call = NULL_RTX;

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
   Store the correct data in the tables that describe the basic blocks,
   set up the chains of references for each CODE_LABEL, and
   delete any entire basic blocks that cannot be reached.

   NONLOCAL_LABEL_LIST is a list of non-local labels in the function.  Blocks
   that are otherwise unreachable may be reachable with a non-local goto.

   BB_EH_END is an array in which we record the list of exception regions
   active at the end of every basic block.  */

static rtx
find_basic_blocks_1 (f, bb_eh_end)
     rtx f;
     rtx *bb_eh_end;
{
  register rtx insn, next;
  int call_has_abnormal_edge = 0;
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
     find_basic_blocks_0 continues to use the old form as a check.  */

  for (insn = f; insn; insn = next)
    {
      enum rtx_code code = GET_CODE (insn);

      next = NEXT_INSN (insn);

      if (code == CALL_INSN)
	{
	  /* Record whether this call created an edge.  */
	  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	  int region = (note ? XINT (XEXP (note, 0), 0) : 1);
	  call_has_abnormal_edge = 0;

	  /* If there is an EH region, we have an edge.  */
	  if (eh_list && region > 0)
	    call_has_abnormal_edge = 1;
	  else
	    {
	      /* If there is a nonlocal goto label and the specified
		 region number isn't -1, we have an edge. (0 means
		 no throw, but might have a nonlocal goto).  */
	      if (nonlocal_goto_handler_labels && region >= 0)
		call_has_abnormal_edge = 1;
	    }
	}

      switch (code)
	{
	case NOTE:
	  {
	    int kind = NOTE_LINE_NUMBER (insn);

	    /* Keep a LIFO list of the currently active exception notes.  */
	    if (kind == NOTE_INSN_EH_REGION_BEG)
	      eh_list = gen_rtx_INSN_LIST (VOIDmode, insn, eh_list);
	    else if (kind == NOTE_INSN_EH_REGION_END)
	      eh_list = XEXP (eh_list, 1);

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
	      if (GET_CODE (end) == CALL_INSN)
		{
		  rtx nop = gen_rtx_USE (VOIDmode, const0_rtx);
		  end = emit_insn_after (nop, end);
		}

	      bb_eh_end[i] = eh_list;
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
		 happens is truely and amazingly gross.  We are about to
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
	  if (GET_CODE (end) == CALL_INSN)
	    {
	      rtx nop = gen_rtx_USE (VOIDmode, const0_rtx);
	      end = emit_insn_after (nop, end);
	    }
	  goto new_bb_exclusive;

	case CALL_INSN:
	  /* A basic block ends at a call that can either throw or
	     do a non-local goto.  */
	  if (call_has_abnormal_edge)
	    {
	    new_bb_inclusive:
	      if (head == NULL_RTX)
		head = insn;
	      end = insn;

	    new_bb_exclusive:
	      bb_eh_end[i] = eh_list;
	      create_basic_block (i++, head, end, bb_note);
	      head = end = NULL_RTX;
	      bb_note = NULL_RTX;
	      break;
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
		    = gen_rtx_EXPR_LIST (VOIDmode, XEXP (note, 0),
				         label_value_list);
	      }
	}
    }

  if (head != NULL_RTX)
    {
      bb_eh_end[i] = eh_list;
      create_basic_block (i++, head, end, bb_note);
    }

  if (i != n_basic_blocks)
    abort ();

  return label_value_list;
}

/* Create a new basic block consisting of the instructions between
   HEAD and END inclusive.  Reuses the note and basic block struct
   in BB_NOTE, if any.  */

static void
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

static void
compute_bb_for_insn (bb_for_insn, max)
     varray_type bb_for_insn;
     int max;
{
  int i;

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
	    VARRAY_BB (bb_for_insn, uid) = bb;
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
}

/* Identify the edges between basic blocks.

   NONLOCAL_LABEL_LIST is a list of non-local labels in the function.  Blocks
   that are otherwise unreachable may be reachable with a non-local goto.

   BB_EH_END is an array indexed by basic block number in which we record 
   the list of exception regions active at the end of the basic block.  */

static void
make_edges (label_value_list, bb_eh_end)
     rtx label_value_list;
     rtx *bb_eh_end;
{
  int i;

  /* Assume no computed jump; revise as we create edges.  */
  current_function_has_computed_jump = 0;

  /* By nature of the way these get numbered, block 0 is always the entry.  */
  make_edge (ENTRY_BLOCK_PTR, BASIC_BLOCK (0), EDGE_FALLTHRU);

  for (i = 0; i < n_basic_blocks; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      rtx insn, x, eh_list;
      enum rtx_code code;
      int force_fallthru = 0;

      /* If we have asynchronous exceptions, scan the notes for all exception
	 regions active in the block.  In the normal case, we only need the
	 one active at the end of the block, which is bb_eh_end[i].  */

      eh_list = bb_eh_end[i];
      if (asynchronous_exceptions)
	{
	  for (insn = bb->end; insn != bb->head; insn = PREV_INSN (insn))
	    {
	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
		eh_list = gen_rtx_INSN_LIST (VOIDmode, insn, eh_list);
	    }
	}

      /* Now examine the last instruction of the block, and discover the
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
		make_label_edge (bb, XEXP (RTVEC_ELT (vec, j), 0), 0);

	      /* Some targets (eg, ARM) emit a conditional jump that also
		 contains the out-of-range target.  Scan for these and
		 add an edge if necessary.  */
	      if ((tmp = single_set (insn)) != NULL
		  && SET_DEST (tmp) == pc_rtx
		  && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC (tmp), 2)) == LABEL_REF)
		make_label_edge (bb, XEXP (XEXP (SET_SRC (tmp), 2), 0), 0);

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
		make_label_edge (bb, XEXP (x, 0), EDGE_ABNORMAL);
	      
	      for (x = forced_labels; x; x = XEXP (x, 1))
		make_label_edge (bb, XEXP (x, 0), EDGE_ABNORMAL);
	    }

	  /* Returns create an exit out.  */
	  else if (returnjump_p (insn))
	    make_edge (bb, EXIT_BLOCK_PTR, 0);

	  /* Otherwise, we have a plain conditional or unconditional jump.  */
	  else
	    {
	      if (! JUMP_LABEL (insn))
		abort ();
	      make_label_edge (bb, JUMP_LABEL (insn), 0);
	    }
	}

      /* If this is a CALL_INSN, then mark it as reaching the active EH
	 handler for this CALL_INSN.  If we're handling asynchronous
	 exceptions then any insn can reach any of the active handlers.

	 Also mark the CALL_INSN as reaching any nonlocal goto handler.  */

      if (code == CALL_INSN || asynchronous_exceptions)
	{
	  int is_call = (code == CALL_INSN ? EDGE_ABNORMAL_CALL : 0);
	  handler_info *ptr;

	  /* Use REG_EH_RETHROW and REG_EH_REGION if available.  */
	  /* ??? REG_EH_REGION is not generated presently.  Is it
	     inteded that there be multiple notes for the regions?
	     or is my eh_list collection redundant with handler linking?  */

	  x = find_reg_note (insn, REG_EH_RETHROW, 0);
	  if (!x)
	    x = find_reg_note (insn, REG_EH_REGION, 0);
	  if (x)
	    {
	      if (XINT (XEXP (x, 0), 0) > 0)
		{
		  ptr = get_first_handler (XINT (XEXP (x, 0), 0));
		  while (ptr)
		    {
		      make_label_edge (bb, ptr->handler_label,
				       EDGE_ABNORMAL | EDGE_EH | is_call);
		      ptr = ptr->next;
		    }
		}
	    }
	  else
	    {
	      for (x = eh_list; x; x = XEXP (x, 1))
		{
		  ptr = get_first_handler (NOTE_BLOCK_NUMBER (XEXP (x, 0)));
		  while (ptr)
		    {
		      make_label_edge (bb, ptr->handler_label,
				       EDGE_ABNORMAL | EDGE_EH | is_call);
		      ptr = ptr->next;
		    }
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

	      for (x = nonlocal_goto_handler_labels; x ; x = XEXP (x, 1))
	        make_label_edge (bb, XEXP (x, 0),
			         EDGE_ABNORMAL | EDGE_ABNORMAL_CALL);
	    }
	}

      /* We know something about the structure of the function __throw in
	 libgcc2.c.  It is the only function that ever contains eh_stub
	 labels.  It modifies its return address so that the last block
	 returns to one of the eh_stub labels within it.  So we have to
	 make additional edges in the flow graph.  */
      if (i + 1 == n_basic_blocks && eh_return_stub_label != 0)
	make_label_edge (bb, eh_return_stub_label, EDGE_EH);

      /* Find out if we can drop through to the next block.  */
      insn = next_nonnote_insn (insn);
      if (!insn || (i + 1 == n_basic_blocks && force_fallthru))
	make_edge (bb, EXIT_BLOCK_PTR, EDGE_FALLTHRU);
      else if (i + 1 < n_basic_blocks)
	{
	  rtx tmp = BLOCK_HEAD (i + 1);
	  if (GET_CODE (tmp) == NOTE)
	    tmp = next_nonnote_insn (tmp);
	  if (force_fallthru || insn == tmp)
	    make_edge (bb, BASIC_BLOCK (i + 1), EDGE_FALLTHRU);
	}
    }
}

/* Create an edge between two basic blocks.  FLAGS are auxiliary information
   about the edge that is accumulated between calls.  */

static void
make_edge (src, dst, flags)
     basic_block src, dst;
     int flags;
{
  edge e;

  /* Make sure we don't add duplicate edges.  */

  for (e = src->succ; e ; e = e->succ_next)
    if (e->dest == dst)
      {
	e->flags |= flags;
	return;
      }

  e = (edge) xcalloc (1, sizeof (*e));

  e->succ_next = src->succ;
  e->pred_next = dst->pred;
  e->src = src;
  e->dest = dst;
  e->flags = flags;

  src->succ = e;
  dst->pred = e;
}

/* Create an edge from a basic block to a label.  */

static void
make_label_edge (src, label, flags)
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

  make_edge (src, BLOCK_FOR_INSN (label), flags);
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
  int i;
 
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

  memset (bb, 0, sizeof (*bb));
  bb->local_set = OBSTACK_ALLOC_REG_SET (function_obstack);
  bb->global_live_at_start = OBSTACK_ALLOC_REG_SET (function_obstack);
  bb->global_live_at_end = OBSTACK_ALLOC_REG_SET (function_obstack);

  /* ??? This info is likely going to be out of date very soon.  */
  CLEAR_REG_SET (bb->local_set);
  if (old_succ->global_live_at_start)
    {
      COPY_REG_SET (bb->global_live_at_start, old_succ->global_live_at_start);
      COPY_REG_SET (bb->global_live_at_end, old_succ->global_live_at_start);
    }
  else
    {
      CLEAR_REG_SET (bb->global_live_at_start);
      CLEAR_REG_SET (bb->global_live_at_end);
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

	  if ((e->flags & EDGE_CRITICAL) == 0)
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
	  emit_barrier_after (pos);

	  /* ... let jump know that label is in use, ...  */
	  ++LABEL_NUSES (old_succ->head);
	  
	  /* ... and clear fallthru on the outgoing edge.  */
	  e->flags &= ~EDGE_FALLTHRU;

	  /* Continue splitting the interesting edge.  */
	}
    }

  /* Place the new block just in front of the successor.  */
  VARRAY_GROW (basic_block_info, ++n_basic_blocks);
  for (i = n_basic_blocks - 1; i > old_succ->index; --i)
    {
      basic_block tmp = BASIC_BLOCK (i - 1);
      BASIC_BLOCK (i) = tmp;
      tmp->index = i;
    }
  BASIC_BLOCK (i) = bb;
  bb->index = i;

  /* Create the basic block note.  */
  bb_note = emit_note_before (NOTE_INSN_BASIC_BLOCK, old_succ->head);
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
  rtx before = NULL_RTX, after = NULL_RTX, tmp;
  basic_block bb;

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
      if (GET_CODE (bb->end) == JUMP_INSN)
	{
	  /* ??? Is it possible to wind up with non-simple jumps?  Perhaps
	     a jump with delay slots already filled?  */
	  if (! simplejump_p (bb->end))
	    abort ();

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
  tmp = e->insns;
  e->insns = NULL_RTX;
  if (before)
    {
      emit_insns_before (tmp, before);
      if (before == bb->head)
	bb->head = before;
    }
  else
    {
      tmp = emit_insns_after (tmp, after);
      if (after == bb->end)
	bb->end = tmp;
    }
}

/* Update the CFG for all queued instructions.  */

void
commit_edge_insertions ()
{
  int i;
  basic_block bb;

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
  tos = worklist = (basic_block *) alloca (sizeof (basic_block) * n);

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
     interfere with the block renumbering that happens in delete_block.  */

  deleted_handler = 0;

  for (i = n - 1; i >= 0; --i)
    {
      basic_block b = BASIC_BLOCK (i);

      if (b->aux != NULL)
	/* This block was found.  Tidy up the mark.  */
	b->aux = NULL;
      else
	deleted_handler |= delete_block (b);
    }

  /* Fix up edges that now fall through, or rather should now fall through
     but previously required a jump around now deleted blocks.  Simplify
     the search by only examining blocks numerically adjacent, since this
     is how find_basic_blocks created them.  */

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
	  /* If the last insn is not a normal conditional jump
	     (or an unconditional jump), then we can not tidy the
	     fallthru edge because we can not delete the jump.  */
	  && GET_CODE (b->end) == JUMP_INSN
	  && condjump_p (b->end))
	tidy_fallthru_edge (s, b, c);
    }

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
	     /* If the last insn is not a normal conditional jump
		(or an unconditional jump), then we can not merge
		the blocks because we can not delete the jump.  */
	     && GET_CODE (b->end) == JUMP_INSN
	     && condjump_p (b->end)
	     && merge_blocks (s, b, c))
	continue;

      /* Don't get confused by the index shift caused by deleting blocks.  */
      i = b->index + 1;
    }

  /* If we deleted an exception handler, we may have EH region begin/end
     blocks to remove as well. */
  if (deleted_handler)
    delete_eh_regions ();
}

/* Find EH regions for which there is no longer a handler, and delete them.  */

static void
delete_eh_regions ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE)
      {
	if ((NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG) ||
	    (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)) 
	  {
	    int num = CODE_LABEL_NUMBER (insn);
	    /* A NULL handler indicates a region is no longer needed */
	    if (get_first_handler (num) == NULL)
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

static void
delete_insn_chain (start, finish)
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

static int
delete_block (b)
     basic_block b;
{
  int deleted_handler = 0;
  rtx insn, end;

  /* If the head of this block is a CODE_LABEL, then it might be the
     label for an exception handler which can't be reached.

     We need to remove the label from the exception_handler_label list
     and remove the associated NOTE_EH_REGION_BEG and NOTE_EH_REGION_END
     notes.  */

  insn = b->head;
  
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

  /* Selectively unlink the insn chain.  Include any BARRIER that may
     follow the basic block.  */
  end = next_nonnote_insn (b->end);
  if (!end || GET_CODE (end) != BARRIER)
    end = b->end;
  delete_insn_chain (insn, end);

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
	free (e);
      }
    for (e = b->succ; e ; e = next)
      {
	for (q = &e->dest->pred; *q != e; q = &(*q)->pred_next)
	  continue;
	*q = e->pred_next;
	next = e->succ_next;
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

static rtx
flow_delete_insn (insn)
     rtx insn;
{
  rtx prev = PREV_INSN (insn);
  rtx next = NEXT_INSN (insn);

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

/* Blocks A and B are to be merged into a single block.  The insns
   are already contiguous, hence `nomove'.  */

static void
merge_blocks_nomove (a, b)
     basic_block a, b;
{
  edge e;
  rtx b_head, b_end, a_end;
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
      b_head = flow_delete_insn (b_head);
    }

  /* Delete the basic block note.  */
  if (GET_CODE (b_head) == NOTE 
      && NOTE_LINE_NUMBER (b_head) == NOTE_INSN_BASIC_BLOCK)
    {
      if (b_head == b_end)
	b_empty = 1;
      b_head = flow_delete_insn (b_head);
    }

  /* If there was a jump out of A, delete it.  */
  a_end = a->end;
  if (GET_CODE (a_end) == JUMP_INSN)
    {
      rtx prev;

      prev = prev_nonnote_insn (a_end);
      if (!prev) 
	prev = a->head;

#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */

      if (prev && sets_cc0_p (prev))
	{
          rtx tmp = prev;
	  prev = prev_nonnote_insn (prev);
	  if (!prev)
	    prev = a->head;
	  flow_delete_insn (tmp);
	}
#endif

      /* Note that a->head != a->end, since we should have at least a
	 bb note plus the jump, so prev != insn.  */
      flow_delete_insn (a_end);
      a_end = prev;
    }

  /* By definition, there should only be one successor of A, and that is
     B.  Free that edge struct.  */
  free (a->succ);

  /* Adjust the edges out of B for the new owner.  */
  for (e = b->succ; e ; e = e->succ_next)
    e->src = a;
  a->succ = b->succ;

  /* Reassociate the insns of B with A.  */
  if (!b_empty)
    {
      BLOCK_FOR_INSN (b_head) = a;
      while (b_head != b_end)
	{
	  b_head = NEXT_INSN (b_head);
	  BLOCK_FOR_INSN (b_head) = a;
	}
      a_end = b_head;
    }
  a->end = a_end;
  
  /* Compact the basic block array.  */
  expunge_block (b);
}

/* Attempt to merge basic blocks that are potentially non-adjacent.  
   Return true iff the attempt succeeded.  */

static int
merge_blocks (e, b, c)
     edge e;
     basic_block b, c;
{
  /* If B has a fallthru edge to C, no need to move anything.  */
  if (!(e->flags & EDGE_FALLTHRU))
    {
      /* ??? From here on out we must make sure to not munge nesting
	 of exception regions and lexical blocks.  Need to think about
	 these cases before this gets implemented.  */
      return 0;

      /* If C has an outgoing fallthru, and B does not have an incoming
	 fallthru, move B before C.  The later clause is somewhat arbitrary,
	 but avoids modifying blocks other than the two we've been given.  */

      /* Otherwise, move C after B.  If C had a fallthru, which doesn't
	 happen to be the physical successor to B, insert an unconditional
	 branch.  If C already ended with a conditional branch, the new
	 jump must go in a new basic block D.  */
    }

  /* If a label still appears somewhere and we cannot delete the label,
     then we cannot merge the blocks.  The edge was tidied already.  */
  {
    rtx insn, stop = NEXT_INSN (c->head);
    for (insn = NEXT_INSN (b->end); insn != stop; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == CODE_LABEL && !can_delete_label_p (insn))
	return 0;
  }

  merge_blocks_nomove (b, c);
  return 1;
}

/* The given edge should potentially a fallthru edge.  If that is in
   fact true, delete the unconditional jump and barriers that are in
   the way.  */

static void
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
  if (GET_CODE (q) == JUMP_INSN)
    {
#ifdef HAVE_cc0
      /* If this was a conditional jump, we need to also delete
	 the insn that set cc0.  */
      if (! simplejump_p (q) && condjump_p (q))
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
    delete_insn_chain (NEXT_INSN (q), PREV_INSN (c->head));

  e->flags |= EDGE_FALLTHRU;
}

/* Discover and record the loop depth at the head of each basic block.  */

static void
calculate_loop_depth (insns)
     rtx insns;
{
  basic_block bb;
  rtx insn;
  int i = 0, depth = 1;

  bb = BASIC_BLOCK (i);
  for (insn = insns; insn ; insn = NEXT_INSN (insn))
    {
      if (insn == bb->head)
	{
	  bb->loop_depth = depth;
	  if (++i >= n_basic_blocks)
	    break;
	  bb = BASIC_BLOCK (i);
	}

      if (GET_CODE (insn) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    depth++;
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    depth--;

	  /* If we have LOOP_DEPTH == 0, there has been a bookkeeping error. */
	  if (depth == 0)
	    abort ();
	}
    }
}

/* Perform data flow analysis.
   F is the first insn of the function and NREGS the number of register numbers
   in use.  */

void
life_analysis (f, nregs, file, remove_dead_code)
     rtx f;
     int nregs;
     FILE *file;
     int remove_dead_code;
{
#ifdef ELIMINABLE_REGS
  register size_t i;
  static struct {int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif

  /* Record which registers will be eliminated.  We use this in
     mark_used_regs.  */

  CLEAR_HARD_REG_SET (elim_reg_set);

#ifdef ELIMINABLE_REGS
  for (i = 0; i < sizeof eliminables / sizeof eliminables[0]; i++)
    SET_HARD_REG_BIT (elim_reg_set, eliminables[i].from);
#else
  SET_HARD_REG_BIT (elim_reg_set, FRAME_POINTER_REGNUM);
#endif

  /* Allocate a bitmap to be filled in by record_volatile_insns.  */
  uid_volatile = BITMAP_ALLOCA ();

  /* We want alias analysis information for local dead store elimination.  */
  init_alias_analysis ();
  life_analysis_1 (f, nregs, remove_dead_code);
  end_alias_analysis ();

  if (file)
    dump_flow_info (file);

  BITMAP_FREE (uid_volatile);
  free_basic_block_vars (1);
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
  if (GET_CODE (src) == REG && GET_CODE (dst) == REG
      && REGNO (src) == REGNO (dst))
    return 1;
  if (GET_CODE (src) != SUBREG || GET_CODE (dst) != SUBREG
      || SUBREG_WORD (src) != SUBREG_WORD (dst))
    return 0;
  src = SUBREG_REG (src);
  dst = SUBREG_REG (dst);
  if (GET_CODE (src) == REG && GET_CODE (dst) == REG
      && REGNO (src) == REGNO (dst))
    return 1;
  return 0;
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

static void
notice_stack_pointer_modification (x, pat)
     rtx x;
     rtx pat ATTRIBUTE_UNUSED;
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

/* Record which insns refer to any volatile memory
   or for any reason can't be deleted just because they are dead stores.
   Also, delete any insns that copy a register to itself.
   And see if the stack pointer is modified.  */
static void
record_volatile_insns (f)
     rtx f;
{
  rtx insn;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      enum rtx_code code1 = GET_CODE (insn);
      if (code1 == CALL_INSN)
	SET_INSN_VOLATILE (insn);
      else if (code1 == INSN || code1 == JUMP_INSN)
	{
	  if (GET_CODE (PATTERN (insn)) != USE
	      && volatile_refs_p (PATTERN (insn)))
	    SET_INSN_VOLATILE (insn);

	  /* A SET that makes space on the stack cannot be dead.
	     (Such SETs occur only for allocating variable-size data,
	     so they will always have a PLUS or MINUS according to the
	     direction of stack growth.)
	     Even if this function never uses this stack pointer value,
	     signal handlers do!  */
	  else if (code1 == INSN && GET_CODE (PATTERN (insn)) == SET
		   && SET_DEST (PATTERN (insn)) == stack_pointer_rtx
#ifdef STACK_GROWS_DOWNWARD
		   && GET_CODE (SET_SRC (PATTERN (insn))) == MINUS
#else
		   && GET_CODE (SET_SRC (PATTERN (insn))) == PLUS
#endif
		   && XEXP (SET_SRC (PATTERN (insn)), 0) == stack_pointer_rtx)
	    SET_INSN_VOLATILE (insn);

	  /* Delete (in effect) any obvious no-op moves.  */
	  else if (noop_move_p (insn))
	    {
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;
	    }
	}

      /* Check if insn modifies the stack pointer.  */
      if ( current_function_sp_is_unchanging
	   && GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	note_stores (PATTERN (insn), notice_stack_pointer_modification);
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
  if (! EXIT_IGNORE_STACK
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

  /* ??? Mark function return value here rather than as uses.  */
}

/* Determine which registers are live at the start of each
   basic block of the function whose first insn is F.
   NREGS is the number of registers used in F.
   We allocate the vector basic_block_live_at_start
   and the regsets that it points to, and fill them with the data.
   regset_size and regset_bytes are also set here.  */

static void
life_analysis_1 (f, nregs, remove_dead_code)
     rtx f;
     int nregs;
     int remove_dead_code;
{
  int first_pass;
  int changed;
  register int i;
  char save_regs_ever_live[FIRST_PSEUDO_REGISTER];
  regset *new_live_at_end;

  struct obstack flow_obstack;

  gcc_obstack_init (&flow_obstack);

  max_regno = nregs;

  /* Allocate and zero out many data structures
     that will record the data from lifetime analysis.  */

  allocate_reg_life_data ();
  allocate_bb_life_data ();

  reg_next_use = (rtx *) alloca (nregs * sizeof (rtx));
  memset (reg_next_use, 0, nregs * sizeof (rtx));

  /* Set up regset-vectors used internally within this function.
     Their meanings are documented above, with their declarations.  */

  new_live_at_end = (regset *) alloca ((n_basic_blocks + 1) * sizeof (regset));
  init_regset_vector (new_live_at_end, n_basic_blocks + 1, &flow_obstack);

  /* Stick these vectors into the AUX field of the basic block, so that
     we don't have to keep going through the index.  */

  for (i = 0; i < n_basic_blocks; ++i)
    BASIC_BLOCK (i)->aux = new_live_at_end[i];
  ENTRY_BLOCK_PTR->aux = new_live_at_end[i];

  /* Assume that the stack pointer is unchanging if alloca hasn't been used.
     This will be cleared by record_volatile_insns if it encounters an insn
     which modifies the stack pointer.  */
  current_function_sp_is_unchanging = !current_function_calls_alloca;

  record_volatile_insns (f);

  if (n_basic_blocks > 0)
    {
      regset theend;
      register edge e;

      theend = EXIT_BLOCK_PTR->global_live_at_start;
      mark_regs_live_at_end (theend);

      /* Propogate this exit data to each of EXIT's predecessors.  */
      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	{
	  COPY_REG_SET (e->src->global_live_at_end, theend);
	  COPY_REG_SET ((regset) e->src->aux, theend);
	}
    }

  /* The post-reload life analysis have (on a global basis) the same registers
     live as was computed by reload itself.

     Otherwise elimination offsets and such may be incorrect.

     Reload will make some registers as live even though they do not appear
     in the rtl.  */
  if (reload_completed)
    memcpy (save_regs_ever_live, regs_ever_live, sizeof (regs_ever_live));
  memset (regs_ever_live, 0, sizeof regs_ever_live);

  /* Propagate life info through the basic blocks
     around the graph of basic blocks.

     This is a relaxation process: each time a new register
     is live at the end of the basic block, we must scan the block
     to determine which registers are, as a consequence, live at the beginning
     of that block.  These registers must then be marked live at the ends
     of all the blocks that can transfer control to that block.
     The process continues until it reaches a fixed point.  */

  first_pass = 1;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (i = n_basic_blocks - 1; i >= 0; i--)
	{
	  basic_block bb = BASIC_BLOCK (i);
	  int consider = first_pass;
	  int must_rescan = first_pass;
	  register int j;

	  if (!first_pass)
	    {
	      /* Set CONSIDER if this block needs thinking about at all
		 (that is, if the regs live now at the end of it
		 are not the same as were live at the end of it when
		 we last thought about it).
		 Set must_rescan if it needs to be thought about
		 instruction by instruction (that is, if any additional
		 reg that is live at the end now but was not live there before
		 is one of the significant regs of this basic block).  */

	      EXECUTE_IF_AND_COMPL_IN_REG_SET
		((regset) bb->aux, bb->global_live_at_end, 0, j,
		 {
		   consider = 1;
		   if (REGNO_REG_SET_P (bb->local_set, j))
		     {
		       must_rescan = 1;
		       goto done;
		     }
		 });
	    done:
	      if (! consider)
		continue;
	    }

	  /* The live_at_start of this block may be changing,
	     so another pass will be required after this one.  */
	  changed = 1;

	  if (! must_rescan)
	    {
	      /* No complete rescan needed;
		 just record those variables newly known live at end
		 as live at start as well.  */
	      IOR_AND_COMPL_REG_SET (bb->global_live_at_start,
				     (regset) bb->aux,
				     bb->global_live_at_end);

	      IOR_AND_COMPL_REG_SET (bb->global_live_at_end,
				     (regset) bb->aux,
				     bb->global_live_at_end);
	    }
	  else
	    {
	      /* Update the basic_block_live_at_start
		 by propagation backwards through the block.  */
	      COPY_REG_SET (bb->global_live_at_end, (regset) bb->aux);
	      COPY_REG_SET (bb->global_live_at_start,
			    bb->global_live_at_end);
	      propagate_block (bb->global_live_at_start,
			       bb->head, bb->end, 0,
			       first_pass ? bb->local_set : (regset) 0,
			       i, remove_dead_code);
	    }

	  /* Update the new_live_at_end's of the block's predecessors.  */
	  {
	    register edge e;

	    for (e = bb->pred; e ; e = e->pred_next)
	      IOR_REG_SET ((regset) e->src->aux, bb->global_live_at_start);
	  }

#ifdef USE_C_ALLOCA
	  alloca (0);
#endif
	}
      first_pass = 0;
    }

  /* The only pseudos that are live at the beginning of the function are
     those that were not set anywhere in the function.  local-alloc doesn't
     know how to handle these correctly, so mark them as not local to any
     one basic block.  */

  if (n_basic_blocks > 0)
    EXECUTE_IF_SET_IN_REG_SET (BASIC_BLOCK (0)->global_live_at_start,
			       FIRST_PSEUDO_REGISTER, i,
			       {
				 REG_BASIC_BLOCK (i) = REG_BLOCK_GLOBAL;
			       });

  /* Now the life information is accurate.  Make one more pass over each
     basic block to delete dead stores, create autoincrement addressing
     and record how many times each register is used, is set, or dies.  */

  for (i = 0; i < n_basic_blocks; i++)
    {
      basic_block bb = BASIC_BLOCK (i);

      /* We start with global_live_at_end to determine which stores are
	 dead.  This process is destructive, and we wish to preserve the
	 contents of global_live_at_end for posterity.  Fortunately,
	 new_live_at_end, due to the way we converged on a solution,
	 contains a duplicate of global_live_at_end that we can kill.  */
      propagate_block ((regset) bb->aux, bb->head, bb->end, 1, (regset) 0, i, remove_dead_code);

#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }

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
				   REG_BASIC_BLOCK (i) = -1;
				 }
			     });

  /* Restore regs_ever_live that was provided by reload.  */
  if (reload_completed)
    memcpy (regs_ever_live, save_regs_ever_live, sizeof (regs_ever_live));

  free_regset_vector (new_live_at_end, n_basic_blocks);
  obstack_free (&flow_obstack, NULL_PTR);

  for (i = 0; i < n_basic_blocks; ++i)
    BASIC_BLOCK (i)->aux = NULL;
  ENTRY_BLOCK_PTR->aux = NULL;
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

      bb->local_set = OBSTACK_ALLOC_REG_SET (function_obstack);
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

  /* Recalculate the register space, in case it has grown.  Old style
     vector oriented regsets would set regset_{size,bytes} here also.  */
  allocate_reg_info (max_regno, FALSE, FALSE);

  /* Because both reg_scan and flow_analysis want to set up the REG_N_SETS
     information, explicitly reset it here.  The allocation should have
     already happened on the previous reg_scan pass.  Make sure in case
     some more registers were allocated.  */
  for (i = 0; i < max_regno; i++)
    REG_N_SETS (i) = 0;
}

/* Make each element of VECTOR point at a regset.  The vector has
   NELTS elements, and space is allocated from the ALLOC_OBSTACK
   obstack.  */

static void
init_regset_vector (vector, nelts, alloc_obstack)
     regset *vector;
     int nelts;
     struct obstack *alloc_obstack;
{
  register int i;

  for (i = 0; i < nelts; i++)
    {
      vector[i] = OBSTACK_ALLOC_REG_SET (alloc_obstack);
      CLEAR_REG_SET (vector[i]);
    }
}

/* Release any additional space allocated for each element of VECTOR point
   other than the regset header itself.  The vector has NELTS elements.  */

void
free_regset_vector (vector, nelts)
     regset *vector;
     int nelts;
{
  register int i;

  for (i = 0; i < nelts; i++)
    FREE_REG_SET (vector[i]);
}

/* Compute the registers live at the beginning of a basic block
   from those live at the end.

   When called, OLD contains those live at the end.
   On return, it contains those live at the beginning.
   FIRST and LAST are the first and last insns of the basic block.

   FINAL is nonzero if we are doing the final pass which is not
   for computing the life info (since that has already been done)
   but for acting on it.  On this pass, we delete dead stores,
   set up the logical links and dead-variables lists of instructions,
   and merge instructions for autoincrement and autodecrement addresses.

   SIGNIFICANT is nonzero only the first time for each basic block.
   If it is nonzero, it points to a regset in which we store
   a 1 for each register that is set within the block.

   BNUM is the number of the basic block.  */

static void
propagate_block (old, first, last, final, significant, bnum, remove_dead_code)
     register regset old;
     rtx first;
     rtx last;
     int final;
     regset significant;
     int bnum;
     int remove_dead_code;
{
  register rtx insn;
  rtx prev;
  regset live;
  regset dead;

  /* Find the loop depth for this block.  Ignore loop level changes in the
     middle of the basic block -- for register allocation purposes, the 
     important uses will be in the blocks wholely contained within the loop
     not in the loop pre-header or post-trailer.  */
  loop_depth = BASIC_BLOCK (bnum)->loop_depth;

  dead = ALLOCA_REG_SET ();
  live = ALLOCA_REG_SET ();

  cc0_live = 0;
  mem_set_list = NULL_RTX;

  if (final)
    {
      register int i;

      /* Process the regs live at the end of the block.
	 Mark them as not local to any one basic block. */
      EXECUTE_IF_SET_IN_REG_SET (old, 0, i,
				 {
				   REG_BASIC_BLOCK (i) = REG_BLOCK_GLOBAL;
				 });
    }

  /* Scan the block an insn at a time from end to beginning.  */

  for (insn = last; ; insn = prev)
    {
      prev = PREV_INSN (insn);

      if (GET_CODE (insn) == NOTE)
	{
	  /* If this is a call to `setjmp' et al,
	     warn if any non-volatile datum is live.  */

	  if (final && NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	    IOR_REG_SET (regs_live_at_setjmp, old);
	}

      /* Update the life-status of regs for this insn.
	 First DEAD gets which regs are set in this insn
	 then LIVE gets which regs are used in this insn.
	 Then the regs live before the insn
	 are those live after, with DEAD regs turned off,
	 and then LIVE regs turned on.  */

      else if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  register int i;
	  rtx note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
	  int insn_is_dead = 0;
	  int libcall_is_dead = 0;

	  if (remove_dead_code)
	    {
	      insn_is_dead = (insn_dead_p (PATTERN (insn), old, 0, REG_NOTES (insn))
	                      /* Don't delete something that refers to volatile storage!  */
	                      && ! INSN_VOLATILE (insn));
	      libcall_is_dead = (insn_is_dead && note != 0
	                         && libcall_dead_p (PATTERN (insn), old, note, insn));
	    }

	  /* If an instruction consists of just dead store(s) on final pass,
	     "delete" it by turning it into a NOTE of type NOTE_INSN_DELETED.
	     We could really delete it with delete_insn, but that
	     can cause trouble for first or last insn in a basic block.  */
	  if (final && insn_is_dead)
	    {
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;

	      /* CC0 is now known to be dead.  Either this insn used it,
		 in which case it doesn't anymore, or clobbered it,
		 so the next insn can't use it.  */
	      cc0_live = 0;

	      /* If this insn is copying the return value from a library call,
		 delete the entire library call.  */
	      if (libcall_is_dead)
		{
		  rtx first = XEXP (note, 0);
		  rtx p = insn;
		  while (INSN_DELETED_P (first))
		    first = NEXT_INSN (first);
		  while (p != first)
		    {
		      p = PREV_INSN (p);
		      PUT_CODE (p, NOTE);
		      NOTE_LINE_NUMBER (p) = NOTE_INSN_DELETED;
		      NOTE_SOURCE_FILE (p) = 0;
		    }
		}
	      goto flushed;
	    }

	  CLEAR_REG_SET (dead);
	  CLEAR_REG_SET (live);

	  /* See if this is an increment or decrement that can be
	     merged into a following memory address.  */
#ifdef AUTO_INC_DEC
	  {
	    register rtx x = single_set (insn);

	    /* Does this instruction increment or decrement a register?  */
	    if (!reload_completed
		&& final && x != 0
		&& GET_CODE (SET_DEST (x)) == REG
		&& (GET_CODE (SET_SRC (x)) == PLUS
		    || GET_CODE (SET_SRC (x)) == MINUS)
		&& XEXP (SET_SRC (x), 0) == SET_DEST (x)
		&& GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
		/* Ok, look for a following memory ref we can combine with.
		   If one is found, change the memory ref to a PRE_INC
		   or PRE_DEC, cancel this insn, and return 1.
		   Return 0 if nothing has been done.  */
		&& try_pre_increment_1 (insn))
	      goto flushed;
	  }
#endif /* AUTO_INC_DEC */

	  /* If this is not the final pass, and this insn is copying the
	     value of a library call and it's dead, don't scan the
	     insns that perform the library call, so that the call's
	     arguments are not marked live.  */
	  if (libcall_is_dead)
	    {
	      /* Mark the dest reg as `significant'.  */
	      mark_set_regs (old, dead, PATTERN (insn), NULL_RTX, significant);

	      insn = XEXP (note, 0);
	      prev = PREV_INSN (insn);
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
	      /* Any regs live at the time of a call instruction
		 must not go in a register clobbered by calls.
		 Find all regs now live and record this for them.  */

	      if (GET_CODE (insn) == CALL_INSN && final)
		EXECUTE_IF_SET_IN_REG_SET (old, 0, i,
					   {
					     REG_N_CALLS_CROSSED (i)++;
					   });

	      /* LIVE gets the regs used in INSN;
		 DEAD gets those set by it.  Dead insns don't make anything
		 live.  */

	      mark_set_regs (old, dead, PATTERN (insn),
			     final ? insn : NULL_RTX, significant);

	      /* If an insn doesn't use CC0, it becomes dead since we 
		 assume that every insn clobbers it.  So show it dead here;
		 mark_used_regs will set it live if it is referenced.  */
	      cc0_live = 0;

	      if (! insn_is_dead)
		mark_used_regs (old, live, PATTERN (insn), final, insn);

	      /* Sometimes we may have inserted something before INSN (such as
		 a move) when we make an auto-inc.  So ensure we will scan
		 those insns.  */
#ifdef AUTO_INC_DEC
	      prev = PREV_INSN (insn);
#endif

	      if (! insn_is_dead && GET_CODE (insn) == CALL_INSN)
		{
		  register int i;

		  rtx note;

	          for (note = CALL_INSN_FUNCTION_USAGE (insn);
		       note;
		       note = XEXP (note, 1))
		    if (GET_CODE (XEXP (note, 0)) == USE)
		      mark_used_regs (old, live, SET_DEST (XEXP (note, 0)),
				      final, insn);

		  /* Each call clobbers all call-clobbered regs that are not
		     global or fixed.  Note that the function-value reg is a
		     call-clobbered reg, and mark_set_regs has already had
		     a chance to handle it.  */

		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    if (call_used_regs[i] && ! global_regs[i]
			&& ! fixed_regs[i])
		      SET_REGNO_REG_SET (dead, i);

		  /* The stack ptr is used (honorarily) by a CALL insn.  */
		  SET_REGNO_REG_SET (live, STACK_POINTER_REGNUM);

		  /* Calls may also reference any of the global registers,
		     so they are made live.  */
		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    if (global_regs[i])
		      mark_used_regs (old, live,
				      gen_rtx_REG (reg_raw_mode[i], i),
				      final, insn);

		  /* Calls also clobber memory.  */
		  mem_set_list = NULL_RTX;
		}

	      /* Update OLD for the registers used or set.  */
	      AND_COMPL_REG_SET (old, dead);
	      IOR_REG_SET (old, live);

	    }

	  /* On final pass, update counts of how many insns each reg is live
	     at.  */
	  if (final)
	    EXECUTE_IF_SET_IN_REG_SET (old, 0, i,
				       { REG_LIVE_LENGTH (i)++; });
	}
    flushed: ;
      if (insn == first)
	break;
    }

  FREE_REG_SET (dead);
  FREE_REG_SET (live);
}

/* Return 1 if X (the body of an insn, or part of it) is just dead stores
   (SET expressions whose destinations are registers dead after the insn).
   NEEDED is the regset that says which regs are alive after the insn.

   Unless CALL_OK is non-zero, an insn is needed if it contains a CALL.

   If X is the entire body of an insn, NOTES contains the reg notes
   pertaining to the insn.  */

static int
insn_dead_p (x, needed, call_ok, notes)
     rtx x;
     regset needed;
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
		  || REGNO_REG_SET_P (needed, regno))
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

      /* A SET that is a subroutine call cannot be dead.  */
      if (! call_ok && GET_CODE (SET_SRC (x)) == CALL)
	return 0;

#ifdef HAVE_cc0
      if (GET_CODE (r) == CC0)
	return ! cc0_live;
#endif
      
      if (GET_CODE (r) == MEM && ! MEM_VOLATILE_P (r))
	{
	  rtx temp;
	  /* Walk the set of memory locations we are currently tracking
	     and see if one is an identical match to this memory location.
	     If so, this memory write is dead (remember, we're walking
	     backwards from the end of the block to the start.  */
	  temp = mem_set_list;
	  while (temp)
	    {
	      if (rtx_equal_p (XEXP (temp, 0), r))
		return 1;
	      temp = XEXP (temp, 1);
	    }
	}

      while (GET_CODE (r) == SUBREG || GET_CODE (r) == STRICT_LOW_PART
	     || GET_CODE (r) == ZERO_EXTRACT)
	r = SUBREG_REG (r);

      if (GET_CODE (r) == REG)
	{
	  int regno = REGNO (r);

	  /* Don't delete insns to set global regs.  */
	  if ((regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
	      /* Make sure insns to set frame pointer aren't deleted.  */
	      || (regno == FRAME_POINTER_REGNUM
		  && (! reload_completed || frame_pointer_needed))
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	      || (regno == HARD_FRAME_POINTER_REGNUM
		  && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	      /* Make sure insns to set arg pointer are never deleted
		 (if the arg pointer isn't fixed, there will be a USE for
		 it, so we can treat it normally).  */
	      || (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
	      || REGNO_REG_SET_P (needed, regno))
	    return 0;

	  /* If this is a hard register, verify that subsequent words are
	     not needed.  */
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      int n = HARD_REGNO_NREGS (regno, GET_MODE (r));

	      while (--n > 0)
		if (REGNO_REG_SET_P (needed, regno+n))
		  return 0;
	    }

	  return 1;
	}
    }

  /* If performing several activities,
     insn is dead if each activity is individually dead.
     Also, CLOBBERs and USEs can be ignored; a CLOBBER or USE
     that's inside a PARALLEL doesn't make the insn worth keeping.  */
  else if (code == PARALLEL)
    {
      int i = XVECLEN (x, 0);

      for (i--; i >= 0; i--)
	if (GET_CODE (XVECEXP (x, 0, i)) != CLOBBER
	    && GET_CODE (XVECEXP (x, 0, i)) != USE
	    && ! insn_dead_p (XVECEXP (x, 0, i), needed, call_ok, NULL_RTX))
	  return 0;

      return 1;
    }

  /* A CLOBBER of a pseudo-register that is dead serves no purpose.  That
     is not necessarily true for hard registers.  */
  else if (code == CLOBBER && GET_CODE (XEXP (x, 0)) == REG
	   && REGNO (XEXP (x, 0)) >= FIRST_PSEUDO_REGISTER
	   && ! REGNO_REG_SET_P (needed, REGNO (XEXP (x, 0))))
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
libcall_dead_p (x, needed, note, insn)
     rtx x;
     regset needed;
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

	  return insn_dead_p (call_pat, needed, 1, REG_NOTES (call));
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
invalidate_mems_from_autoinc (insn)
     rtx insn;
{
  rtx note = REG_NOTES (insn);
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    {
      if (REG_NOTE_KIND (note) == REG_INC)
        {
          rtx temp = mem_set_list;
          rtx prev = NULL_RTX;

          while (temp)
	    {
	      if (reg_overlap_mentioned_p (XEXP (note, 0), XEXP (temp, 0)))
	        {
	          /* Splice temp out of list.  */
	          if (prev)
	            XEXP (prev, 1) = XEXP (temp, 1);
	          else
	            mem_set_list = XEXP (temp, 1);
	        }
	      else
	        prev = temp;
              temp = XEXP (temp, 1);
	    }
	}
    }
}

/* Process the registers that are set within X.
   Their bits are set to 1 in the regset DEAD,
   because they are dead prior to this insn.

   If INSN is nonzero, it is the insn being processed
   and the fact that it is nonzero implies this is the FINAL pass
   in propagate_block.  In this case, various info about register
   usage is stored, LOG_LINKS fields of insns are set up.  */

static void
mark_set_regs (needed, dead, x, insn, significant)
     regset needed;
     regset dead;
     rtx x;
     rtx insn;
     regset significant;
{
  register RTX_CODE code = GET_CODE (x);

  if (code == SET || code == CLOBBER)
    mark_set_1 (needed, dead, x, insn, significant);
  else if (code == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET || code == CLOBBER)
	    mark_set_1 (needed, dead, XVECEXP (x, 0, i), insn, significant);
	}
    }
}

/* Process a single SET rtx, X.  */

static void
mark_set_1 (needed, dead, x, insn, significant)
     regset needed;
     regset dead;
     rtx x;
     rtx insn;
     regset significant;
{
  register int regno;
  register rtx reg = SET_DEST (x);

  /* Some targets place small structures in registers for
     return values of functions.  We have to detect this
     case specially here to get correct flow information.  */
  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      register int i;

      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	  mark_set_1 (needed, dead, XVECEXP (reg, 0, i), insn, significant);
      return;
    }

  /* Modifying just one hardware register of a multi-reg value
     or just a byte field of a register
     does not mean the value from before this insn is now dead.
     But it does mean liveness of that register at the end of the block
     is significant.

     Within mark_set_1, however, we treat it as if the register is
     indeed modified.  mark_used_regs will, however, also treat this
     register as being used.  Thus, we treat these insns as setting a
     new value for the register as a function of its old value.  This
     cases LOG_LINKS to be made appropriately and this will help combine.  */

  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  /* If this set is a MEM, then it kills any aliased writes. 
     If this set is a REG, then it kills any MEMs which use the reg.  */
  if (GET_CODE (reg) == MEM
      || GET_CODE (reg) == REG)
    {
      rtx temp = mem_set_list;
      rtx prev = NULL_RTX;

      while (temp)
	{
	  if ((GET_CODE (reg) == MEM
	       && output_dependence (XEXP (temp, 0), reg))
	      || (GET_CODE (reg) == REG
		  && reg_overlap_mentioned_p (reg, XEXP (temp, 0))))
	    {
	      /* Splice this entry out of the list.  */
	      if (prev)
		XEXP (prev, 1) = XEXP (temp, 1);
	      else
		mem_set_list = XEXP (temp, 1);
	    }
	  else
	    prev = temp;
	  temp = XEXP (temp, 1);
	}
    }

  /* If the memory reference had embedded side effects (autoincrement
     address modes.  Then we may need to kill some entries on the
     memory set list.  */
  if (insn && GET_CODE (reg) == MEM)
    invalidate_mems_from_autoinc (insn);

  if (GET_CODE (reg) == MEM && ! side_effects_p (reg)
      /* We do not know the size of a BLKmode store, so we do not track
	 them for redundant store elimination.  */
      && GET_MODE (reg) != BLKmode
      /* There are no REG_INC notes for SP, so we can't assume we'll see 
	 everything that invalidates it.  To be safe, don't eliminate any
	 stores though SP; none of them should be redundant anyway.  */
      && ! reg_mentioned_p (stack_pointer_rtx, reg))
    mem_set_list = gen_rtx_EXPR_LIST (VOIDmode, reg, mem_set_list);

  if (GET_CODE (reg) == REG
      && (regno = REGNO (reg), ! (regno == FRAME_POINTER_REGNUM
				  && (! reload_completed || frame_pointer_needed)))
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
      && ! (regno == HARD_FRAME_POINTER_REGNUM
	    && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      && ! (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
      && ! (regno < FIRST_PSEUDO_REGISTER && global_regs[regno]))
    /* && regno != STACK_POINTER_REGNUM) -- let's try without this.  */
    {
      int some_needed = REGNO_REG_SET_P (needed, regno);
      int some_not_needed = ! some_needed;

      /* Mark it as a significant register for this basic block.  */
      if (significant)
	SET_REGNO_REG_SET (significant, regno);

      /* Mark it as dead before this insn.  */
      SET_REGNO_REG_SET (dead, regno);

      /* A hard reg in a wide mode may really be multiple registers.
	 If so, mark all of them just like the first.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int n;

	  /* Nothing below is needed for the stack pointer; get out asap.
	     Eg, log links aren't needed, since combine won't use them.  */
	  if (regno == STACK_POINTER_REGNUM)
	    return;

	  n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  while (--n > 0)
	    {
	      int regno_n = regno + n;
	      int needed_regno = REGNO_REG_SET_P (needed, regno_n);
	      if (significant)
		SET_REGNO_REG_SET (significant, regno_n);

	      SET_REGNO_REG_SET (dead, regno_n);
	      some_needed |= needed_regno;
	      some_not_needed |= ! needed_regno;
	    }
	}
      /* Additional data to record if this is the final pass.  */
      if (insn)
	{
	  register rtx y = reg_next_use[regno];
	  register int blocknum = BLOCK_NUM (insn);

	  /* If this is a hard reg, record this function uses the reg.  */

	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      register int i;
	      int endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));

	      for (i = regno; i < endregno; i++)
		{
		  /* The next use is no longer "next", since a store
		     intervenes.  */
		  reg_next_use[i] = 0;

		  regs_ever_live[i] = 1;
		  REG_N_SETS (i)++;
		}
	    }
	  else
	    {
	      /* The next use is no longer "next", since a store
		 intervenes.  */
	      reg_next_use[regno] = 0;

	      /* Keep track of which basic blocks each reg appears in.  */

	      if (REG_BASIC_BLOCK (regno) == REG_BLOCK_UNKNOWN)
		REG_BASIC_BLOCK (regno) = blocknum;
	      else if (REG_BASIC_BLOCK (regno) != blocknum)
		REG_BASIC_BLOCK (regno) = REG_BLOCK_GLOBAL;

	      /* Count (weighted) references, stores, etc.  This counts a
		 register twice if it is modified, but that is correct.  */
	      REG_N_SETS (regno)++;

	      REG_N_REFS (regno) += loop_depth;
		  
	      /* The insns where a reg is live are normally counted
		 elsewhere, but we want the count to include the insn
		 where the reg is set, and the normal counting mechanism
		 would not count it.  */
	      REG_LIVE_LENGTH (regno)++;
	    }

	  if (! some_not_needed)
	    {
	      /* Make a logical link from the next following insn
		 that uses this register, back to this insn.
		 The following insns have already been processed.

		 We don't build a LOG_LINK for hard registers containing
		 in ASM_OPERANDs.  If these registers get replaced,
		 we might wind up changing the semantics of the insn,
		 even if reload can make what appear to be valid assignments
		 later.  */
	      if (y && (BLOCK_NUM (y) == blocknum)
		  && (regno >= FIRST_PSEUDO_REGISTER
		      || asm_noperands (PATTERN (y)) < 0))
		LOG_LINKS (y)
		  = gen_rtx_INSN_LIST (VOIDmode, insn, LOG_LINKS (y));
	    }
	  else if (! some_needed)
	    {
	      /* Note that dead stores have already been deleted when possible
		 If we get here, we have found a dead store that cannot
		 be eliminated (because the same insn does something useful).
		 Indicate this by marking the reg being set as dying here.  */
	      REG_NOTES (insn)
		= gen_rtx_EXPR_LIST (REG_UNUSED, reg, REG_NOTES (insn));
	      REG_N_DEATHS (REGNO (reg))++;
	    }
	  else
	    {
	      /* This is a case where we have a multi-word hard register
		 and some, but not all, of the words of the register are
		 needed in subsequent insns.  Write REG_UNUSED notes
		 for those parts that were not needed.  This case should
		 be rare.  */

	      int i;

	      for (i = HARD_REGNO_NREGS (regno, GET_MODE (reg)) - 1;
		   i >= 0; i--)
		if (!REGNO_REG_SET_P (needed, regno + i))
		  REG_NOTES (insn)
		    = gen_rtx_EXPR_LIST (REG_UNUSED,
					 gen_rtx_REG (reg_raw_mode[regno + i],
						      regno + i),
					 REG_NOTES (insn));
	    }
	}
    }
  else if (GET_CODE (reg) == REG)
    reg_next_use[regno] = 0;

  /* If this is the last pass and this is a SCRATCH, show it will be dying
     here and count it.  */
  else if (GET_CODE (reg) == SCRATCH && insn != 0)
    {
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_UNUSED, reg, REG_NOTES (insn));
    }
}

#ifdef AUTO_INC_DEC

/* X is a MEM found in INSN.  See if we can convert it into an auto-increment
   reference.  */

static void
find_auto_inc (needed, x, insn)
     regset needed;
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
      if ((incr = reg_next_use[regno]) != 0
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

	  if (dead_or_set_p (incr, addr))
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
	      basic_block bb;

	      start_sequence ();
	      emit_move_insn (q, addr);
	      insns = get_insns ();
	      end_sequence ();

	      bb = BLOCK_FOR_INSN (insn);
	      for (temp = insns; temp; temp = NEXT_INSN (temp))
		set_block_for_insn (temp, bb);

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

	      if (BLOCK_FOR_INSN (insn)->head == insn)
		BLOCK_FOR_INSN (insn)->head = insns;

	      /* INCR will become a NOTE and INSN won't contain a
		 use of ADDR.  If a use of ADDR was just placed in
		 the insn before INSN, make that the next use. 
		 Otherwise, invalidate it.  */
	      if (GET_CODE (PREV_INSN (insn)) == INSN
		  && GET_CODE (PATTERN (PREV_INSN (insn))) == SET
		  && SET_SRC (PATTERN (PREV_INSN (insn))) == addr)
		reg_next_use[regno] = PREV_INSN (insn);
	      else
		reg_next_use[regno] = 0;

	      addr = q;
	      regno = REGNO (q);

	      /* REGNO is now used in INCR which is below INSN, but
		 it previously wasn't live here.  If we don't mark
		 it as needed, we'll put a REG_DEAD note for it
		 on this insn, which is incorrect.  */
	      SET_REGNO_REG_SET (needed, regno);

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
	    = gen_rtx_EXPR_LIST (REG_INC, addr, REG_NOTES (insn));

	  /* Modify the old increment-insn to simply copy
	     the already-incremented value of our register.  */
	  if (! validate_change (incr, &SET_SRC (set), addr, 0))
	    abort ();

	  /* If that makes it a no-op (copying the register into itself) delete
	     it so it won't appear to be a "use" and a "set" of this
	     register.  */
	  if (SET_DEST (set) == addr)
	    {
	      PUT_CODE (incr, NOTE);
	      NOTE_LINE_NUMBER (incr) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (incr) = 0;
	    }

	  if (regno >= FIRST_PSEUDO_REGISTER)
	    {
	      /* Count an extra reference to the reg.  When a reg is
		 incremented, spilling it is worse, so we want to make
		 that less likely.  */
	      REG_N_REFS (regno) += loop_depth;

	      /* Count the increment as a setting of the register,
		 even though it isn't a SET in rtl.  */
	      REG_N_SETS (regno)++;
	    }
	}
    }
}
#endif /* AUTO_INC_DEC */

/* Scan expression X and store a 1-bit in LIVE for each reg it uses.
   This is done assuming the registers needed from X
   are those that have 1-bits in NEEDED.

   On the final pass, FINAL is 1.  This means try for autoincrement
   and count the uses and deaths of each pseudo-reg.

   INSN is the containing instruction.  If INSN is dead, this function is not
   called.  */

static void
mark_used_regs (needed, live, x, final, insn)
     regset needed;
     regset live;
     rtx x;
     int final;
     rtx insn;
{
  register RTX_CODE code;
  register int regno;
  int i;

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
      cc0_live = 1;
      return;
#endif

    case CLOBBER:
      /* If we are clobbering a MEM, mark any registers inside the address
	 as being used.  */
      if (GET_CODE (XEXP (x, 0)) == MEM)
	mark_used_regs (needed, live, XEXP (XEXP (x, 0), 0), final, insn);
      return;

    case MEM:
      /* Invalidate the data for the last MEM stored, but only if MEM is
	 something that can be stored into.  */
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	; /* needn't clear the memory set list */
      else
	{
	  rtx temp = mem_set_list;
	  rtx prev = NULL_RTX;

	  while (temp)
	    {
	      if (anti_dependence (XEXP (temp, 0), x))
		{
		  /* Splice temp out of the list.  */
		  if (prev)
		    XEXP (prev, 1) = XEXP (temp, 1);
		  else
		    mem_set_list = XEXP (temp, 1);
		}
	      else
		prev = temp;
	      temp = XEXP (temp, 1);
	    }
	}

      /* If the memory reference had embedded side effects (autoincrement
	 address modes.  Then we may need to kill some entries on the
	 memory set list.  */
      if (insn)
	invalidate_mems_from_autoinc (insn);

#ifdef AUTO_INC_DEC
      if (final)
	find_auto_inc (needed, x, insn);
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

      /* In case the SUBREG is not of a register, don't optimize */
      if (GET_CODE (x) != REG)
	{
	  mark_used_regs (needed, live, x, final, insn);
	  return;
	}

      /* ... fall through ...  */

    case REG:
      /* See a register other than being set
	 => mark it as needed.  */

      regno = REGNO (x);
      {
	int some_needed = REGNO_REG_SET_P (needed, regno);
	int some_not_needed = ! some_needed;

	SET_REGNO_REG_SET (live, regno);

	/* A hard reg in a wide mode may really be multiple registers.
	   If so, mark all of them just like the first.  */
	if (regno < FIRST_PSEUDO_REGISTER)
	  {
	    int n;

	    /* For stack ptr or fixed arg pointer,
	       nothing below can be necessary, so waste no more time.  */
	    if (regno == STACK_POINTER_REGNUM
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		|| (regno == HARD_FRAME_POINTER_REGNUM
		    && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
		|| (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
		|| (regno == FRAME_POINTER_REGNUM
		    && (! reload_completed || frame_pointer_needed)))
	      {
		/* If this is a register we are going to try to eliminate,
		   don't mark it live here.  If we are successful in
		   eliminating it, it need not be live unless it is used for
		   pseudos, in which case it will have been set live when
		   it was allocated to the pseudos.  If the register will not
		   be eliminated, reload will set it live at that point.  */

		if (! TEST_HARD_REG_BIT (elim_reg_set, regno))
		  regs_ever_live[regno] = 1;
		return;
	      }
	    /* No death notes for global register variables;
	       their values are live after this function exits.  */
	    if (global_regs[regno])
	      {
		if (final)
		  reg_next_use[regno] = insn;
		return;
	      }

	    n = HARD_REGNO_NREGS (regno, GET_MODE (x));
	    while (--n > 0)
	      {
		int regno_n = regno + n;
		int needed_regno = REGNO_REG_SET_P (needed, regno_n);

		SET_REGNO_REG_SET (live, regno_n);
		some_needed |= needed_regno;
		some_not_needed |= ! needed_regno;
	      }
	  }
	if (final)
	  {
	    /* Record where each reg is used, so when the reg
	       is set we know the next insn that uses it.  */

	    reg_next_use[regno] = insn;

	    if (regno < FIRST_PSEUDO_REGISTER)
	      {
		/* If a hard reg is being used,
		   record that this function does use it.  */

		i = HARD_REGNO_NREGS (regno, GET_MODE (x));
		if (i == 0)
		  i = 1;
		do
		  regs_ever_live[regno + --i] = 1;
		while (i > 0);
	      }
	    else
	      {
		/* Keep track of which basic block each reg appears in.  */

		register int blocknum = BLOCK_NUM (insn);

		if (REG_BASIC_BLOCK (regno) == REG_BLOCK_UNKNOWN)
		  REG_BASIC_BLOCK (regno) = blocknum;
		else if (REG_BASIC_BLOCK (regno) != blocknum)
		  REG_BASIC_BLOCK (regno) = REG_BLOCK_GLOBAL;

		/* Count (weighted) number of uses of each reg.  */

		REG_N_REFS (regno) += loop_depth;
	      }

	    /* Record and count the insns in which a reg dies.
	       If it is used in this insn and was dead below the insn
	       then it dies in this insn.  If it was set in this insn,
	       we do not make a REG_DEAD note; likewise if we already
	       made such a note.  */

	    if (some_not_needed
		&& ! dead_or_set_p (insn, x)
#if 0
		&& (regno >= FIRST_PSEUDO_REGISTER || ! fixed_regs[regno])
#endif
		)
	      {
		/* Check for the case where the register dying partially
		   overlaps the register set by this insn.  */
		if (regno < FIRST_PSEUDO_REGISTER
		    && HARD_REGNO_NREGS (regno, GET_MODE (x)) > 1)
		  {
		    int n = HARD_REGNO_NREGS (regno, GET_MODE (x));
		    while (--n >= 0)
		      some_needed |= dead_or_set_regno_p (insn, regno + n);
		  }

		/* If none of the words in X is needed, make a REG_DEAD
		   note.  Otherwise, we must make partial REG_DEAD notes.  */
		if (! some_needed)
		  {
		    REG_NOTES (insn)
		      = gen_rtx_EXPR_LIST (REG_DEAD, x, REG_NOTES (insn));
		    REG_N_DEATHS (regno)++;
		  }
		else
		  {
		    int i;

		    /* Don't make a REG_DEAD note for a part of a register
		       that is set in the insn.  */

		    for (i = HARD_REGNO_NREGS (regno, GET_MODE (x)) - 1;
			 i >= 0; i--)
		      if (!REGNO_REG_SET_P (needed, regno + i)
			  && ! dead_or_set_regno_p (insn, regno + i))
			REG_NOTES (insn)
			  = gen_rtx_EXPR_LIST (REG_DEAD,
					       gen_rtx_REG (reg_raw_mode[regno + i],
							    regno + i),
					       REG_NOTES (insn));
		  }
	      }
	  }
      }
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
	    if (final)
	      find_auto_inc (needed, testreg, insn);
#endif
	    mark_used_regs (needed, live, XEXP (testreg, 0), final, insn);
	    mark_used_regs (needed, live, SET_SRC (x), final, insn);
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

	/* If this is a store into a register,
	   recursively scan the value being stored.  */

	if ((GET_CODE (testreg) == PARALLEL
	     && GET_MODE (testreg) == BLKmode)
	    || (GET_CODE (testreg) == REG
		&& (regno = REGNO (testreg), ! (regno == FRAME_POINTER_REGNUM
						&& (! reload_completed || frame_pointer_needed)))
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
		&& ! (regno == HARD_FRAME_POINTER_REGNUM
		      && (! reload_completed || frame_pointer_needed))
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
		&& ! (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
		))
	  /* We used to exclude global_regs here, but that seems wrong.
	     Storing in them is like storing in mem.  */
	  {
	    mark_used_regs (needed, live, SET_SRC (x), final, insn);
	    if (mark_dest)
	      mark_used_regs (needed, live, SET_DEST (x), final, insn);
	    return;
	  }
      }
      break;

    case RETURN:
      /* If exiting needs the right stack value, consider this insn as
	 using the stack pointer.  In any event, consider it as using
	 all global registers and all registers used by return.  */
      if (! EXIT_IGNORE_STACK
	  || (! FRAME_POINTER_REQUIRED
	      && ! current_function_calls_alloca
	      && flag_omit_frame_pointer)
	  || current_function_sp_is_unchanging)
	SET_REGNO_REG_SET (live, STACK_POINTER_REGNUM);

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (global_regs[i]
#ifdef EPILOGUE_USES
	    || EPILOGUE_USES (i)
#endif
	    )
	  SET_REGNO_REG_SET (live, i);
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
	  mem_set_list = NULL_RTX;

        /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */
	if (code == ASM_OPERANDS)
	  {
	    int j;

	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      mark_used_regs (needed, live, ASM_OPERANDS_INPUT (x, j),
			      final, insn);
	  }
	break;
      }


    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register char *fmt = GET_RTX_FORMAT (code);
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
	    mark_used_regs (needed, live, XEXP (x, i), final, insn);
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      mark_used_regs (needed, live, XVECEXP (x, i, j), final, insn);
	  }
      }
  }
}

#ifdef AUTO_INC_DEC

static int
try_pre_increment_1 (insn)
     rtx insn;
{
  /* Find the next use of this reg.  If in same basic block,
     make it do pre-increment or pre-decrement if appropriate.  */
  rtx x = single_set (insn);
  HOST_WIDE_INT amount = ((GET_CODE (SET_SRC (x)) == PLUS ? 1 : -1)
		* INTVAL (XEXP (SET_SRC (x), 1)));
  int regno = REGNO (SET_DEST (x));
  rtx y = reg_next_use[regno];
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
	  REG_N_REFS (regno) += loop_depth;
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
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_INC, reg, REG_NOTES (insn));
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
  char *fmt = GET_RTX_FORMAT (code);
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
      if (fmt[i] == 'E')
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
dump_flow_info (file)
     FILE *file;
{
  register int i;
  static char *reg_class_names[] = REG_CLASS_NAMES;

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

  fprintf (file, "\n%d basic blocks.\n", n_basic_blocks);
  for (i = 0; i < n_basic_blocks; i++)
    {
      register basic_block bb = BASIC_BLOCK (i);
      register int regno;
      register edge e;

      fprintf (file, "\nBasic block %d: first insn %d, last %d.\n",
	       i, INSN_UID (bb->head), INSN_UID (bb->end));

      fprintf (file, "Predecessors: ");
      for (e = bb->pred; e ; e = e->pred_next)
	dump_edge_info (file, e, 0);

      fprintf (file, "\nSuccessors: ");
      for (e = bb->succ; e ; e = e->succ_next)
	dump_edge_info (file, e, 1);

      fprintf (file, "\nRegisters live at start:");
      if (bb->global_live_at_start)
	{
          for (regno = 0; regno < max_regno; regno++)
	    if (REGNO_REG_SET_P (bb->global_live_at_start, regno))
	      fprintf (file, " %d", regno);
	}
      else
	fprintf (file, " n/a");

      fprintf (file, "\nRegisters live at end:");
      if (bb->global_live_at_end)
	{
          for (regno = 0; regno < max_regno; regno++)
	    if (REGNO_REG_SET_P (bb->global_live_at_end, regno))
	      fprintf (file, " %d", regno);
	}
      else
	fprintf (file, " n/a");

      putc('\n', file);
    }

  putc('\n', file);
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
      static char * bitnames[] = {
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
	alloca (max_uid * sizeof (basic_block));
      basic_block *end = (basic_block *)
	alloca (max_uid * sizeof (basic_block));
      enum bb_state *in_bb_p = (enum bb_state *)
	alloca (max_uid * sizeof (enum bb_state));

      memset (start, 0, max_uid * sizeof (basic_block));
      memset (end, 0, max_uid * sizeof (basic_block));
      memset (in_bb_p, 0, max_uid * sizeof (enum bb_state));

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

	      EXECUTE_IF_SET_IN_REG_SET (bb->global_live_at_start, 0, i,
					 {
					   fprintf (outf, " %d", i);
					   if (i < FIRST_PSEUDO_REGISTER)
					     fprintf (outf, " [%s]",
						      reg_names[i]);
					 });
	      putc ('\n', outf);
	    }

	  if (in_bb_p[INSN_UID(tmp_rtx)] == NOT_IN_BB
	      && GET_CODE (tmp_rtx) != NOTE
	      && GET_CODE (tmp_rtx) != BARRIER
	      && ! obey_regdecls)
	    fprintf (outf, ";; Insn is not within a basic block\n");
	  else if (in_bb_p[INSN_UID(tmp_rtx)] == IN_MULTIPLE_BB)
	    fprintf (outf, ";; Insn is in multiple basic blocks\n");

	  did_output = print_rtl_single (outf, tmp_rtx);

	  if ((bb = end[INSN_UID (tmp_rtx)]) != NULL)
	    fprintf (outf, ";; End of basic block %d\n", bb->index);

	  if (did_output)
	    putc ('\n', outf);
	}
    }
}


/* Integer list support.  */

/* Allocate a node from list *HEAD_PTR.  */

static int_list_ptr
alloc_int_list_node (head_ptr)
     int_list_block **head_ptr;
{
  struct int_list_block *first_blk = *head_ptr;

  if (first_blk == NULL || first_blk->nodes_left <= 0)
    {
      first_blk = (struct int_list_block *) xmalloc (sizeof (struct int_list_block));
      first_blk->nodes_left = INT_LIST_NODES_IN_BLK;
      first_blk->next = *head_ptr;
      *head_ptr = first_blk;
    }

  first_blk->nodes_left--;
  return &first_blk->nodes[first_blk->nodes_left];
}

/* Pointer to head of predecessor/successor block list.  */
static int_list_block *pred_int_list_blocks;

/* Add a new node to integer list LIST with value VAL.
   LIST is a pointer to a list object to allow for different implementations.
   If *LIST is initially NULL, the list is empty.
   The caller must not care whether the element is added to the front or
   to the end of the list (to allow for different implementations).  */

static int_list_ptr
add_int_list_node (blk_list, list, val)
     int_list_block **blk_list;
     int_list **list;
     int val;
{
  int_list_ptr p = alloc_int_list_node (blk_list);

  p->val = val;
  p->next = *list;
  *list = p;
  return p;
}

/* Free the blocks of lists at BLK_LIST.  */

void
free_int_list (blk_list)
     int_list_block **blk_list;
{
  int_list_block *p, *next;

  for (p = *blk_list; p != NULL; p = next)
    {
      next = p->next;
      free (p);
    }

  /* Mark list as empty for the next function we compile.  */
  *blk_list = NULL;
}

/* Predecessor/successor computation.  */

/* Mark PRED_BB a precessor of SUCC_BB,
   and conversely SUCC_BB a successor of PRED_BB.  */

static void
add_pred_succ (pred_bb, succ_bb, s_preds, s_succs, num_preds, num_succs)
     int pred_bb;
     int succ_bb;
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     int *num_preds;
     int *num_succs;
{
  if (succ_bb != EXIT_BLOCK)
    {
      add_int_list_node (&pred_int_list_blocks, &s_preds[succ_bb], pred_bb);
      num_preds[succ_bb]++;
    }
  if (pred_bb != ENTRY_BLOCK)
    {
      add_int_list_node (&pred_int_list_blocks, &s_succs[pred_bb], succ_bb);
      num_succs[pred_bb]++;
    }
}

/* Convert edge lists into pred/succ lists for backward compatibility.  */

void
compute_preds_succs (s_preds, s_succs, num_preds, num_succs)
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
     int *num_preds;
     int *num_succs;
{
  int i, n = n_basic_blocks;
  edge e;

  memset (s_preds, 0, n_basic_blocks * sizeof (int_list_ptr));
  memset (s_succs, 0, n_basic_blocks * sizeof (int_list_ptr));
  memset (num_preds, 0, n_basic_blocks * sizeof (int));
  memset (num_succs, 0, n_basic_blocks * sizeof (int));

  for (i = 0; i < n; ++i)
    {
      basic_block bb = BASIC_BLOCK (i);
      
      for (e = bb->succ; e ; e = e->succ_next)
	add_pred_succ (i, e->dest->index, s_preds, s_succs,
		       num_preds, num_succs);
    }

  for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
    add_pred_succ (ENTRY_BLOCK, e->dest->index, s_preds, s_succs,
		   num_preds, num_succs);
}

void
dump_bb_data (file, preds, succs, live_info)
     FILE *file;
     int_list_ptr *preds;
     int_list_ptr *succs;
     int live_info;
{
  int bb;
  int_list_ptr p;

  fprintf (file, "BB data\n\n");
  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      fprintf (file, "BB %d, start %d, end %d\n", bb,
	       INSN_UID (BLOCK_HEAD (bb)), INSN_UID (BLOCK_END (bb)));
      fprintf (file, "  preds:");
      for (p = preds[bb]; p != NULL; p = p->next)
	{
	  int pred_bb = INT_LIST_VAL (p);
	  if (pred_bb == ENTRY_BLOCK)
	    fprintf (file, " entry");
	  else
	    fprintf (file, " %d", pred_bb);
	}
      fprintf (file, "\n");
      fprintf (file, "  succs:");
      for (p = succs[bb]; p != NULL; p = p->next)
	{
	  int succ_bb = INT_LIST_VAL (p);
	  if (succ_bb == EXIT_BLOCK)
	    fprintf (file, " exit");
	  else
	    fprintf (file, " %d", succ_bb);
	}
      if (live_info)
	{
	  int regno;
	  fprintf (file, "\nRegisters live at start:");
	  for (regno = 0; regno < max_regno; regno++)
	    if (REGNO_REG_SET_P (BASIC_BLOCK (bb)->global_live_at_start, regno))
	      fprintf (file, " %d", regno);
	  fprintf (file, "\n");
	}
      fprintf (file, "\n");
    }
  fprintf (file, "\n");
}

/* Free basic block data storage.  */

void
free_bb_mem ()
{
  free_int_list (&pred_int_list_blocks);
}

/* Compute dominator relationships.  */
void
compute_dominators (dominators, post_dominators, s_preds, s_succs)
     sbitmap *dominators;
     sbitmap *post_dominators;
     int_list_ptr *s_preds;
     int_list_ptr *s_succs;
{
  int bb, changed, passes;
  sbitmap *temp_bitmap;

  temp_bitmap = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  sbitmap_vector_ones (dominators, n_basic_blocks);
  sbitmap_vector_ones (post_dominators, n_basic_blocks);
  sbitmap_vector_zero (temp_bitmap, n_basic_blocks);

  sbitmap_zero (dominators[0]);
  SET_BIT (dominators[0], 0);

  sbitmap_zero (post_dominators[n_basic_blocks - 1]);
  SET_BIT (post_dominators[n_basic_blocks - 1], 0);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 1; bb < n_basic_blocks; bb++)
	{
	  sbitmap_intersect_of_predecessors (temp_bitmap[bb], dominators,
					     bb, s_preds);
	  SET_BIT (temp_bitmap[bb], bb);
	  changed |= sbitmap_a_and_b (dominators[bb],
				      dominators[bb],
				      temp_bitmap[bb]);
	  sbitmap_intersect_of_successors (temp_bitmap[bb], post_dominators,
					   bb, s_succs);
	  SET_BIT (temp_bitmap[bb], bb);
	  changed |= sbitmap_a_and_b (post_dominators[bb],
				      post_dominators[bb],
				      temp_bitmap[bb]);
	}
      passes++;
    }

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

/* Count for a single SET rtx, X.  */

static void
count_reg_sets_1 (x)
     rtx x;
{
  register int regno;
  register rtx reg = SET_DEST (x);

  /* Find the register that's set/clobbered.  */
  while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == SIGN_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART)
    reg = XEXP (reg, 0);

  if (GET_CODE (reg) == PARALLEL
      && GET_MODE (reg) == BLKmode)
    {
      register int i;
      for (i = XVECLEN (reg, 0) - 1; i >= 0; i--)
	count_reg_sets_1 (XVECEXP (reg, 0, i));
      return;
    }

  if (GET_CODE (reg) == REG)
    {
      regno = REGNO (reg);
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  /* Count (weighted) references, stores, etc.  This counts a
	     register twice if it is modified, but that is correct.  */
	  REG_N_SETS (regno)++;

	  REG_N_REFS (regno) += loop_depth;
	}
    }
}

/* Increment REG_N_SETS for each SET or CLOBBER found in X; also increment
   REG_N_REFS by the current loop depth for each SET or CLOBBER found.  */

static void
count_reg_sets  (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);

  if (code == SET || code == CLOBBER)
    count_reg_sets_1 (x);
  else if (code == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET || code == CLOBBER)
	    count_reg_sets_1 (XVECEXP (x, 0, i));
	}
    }
}

/* Increment REG_N_REFS by the current loop depth each register reference
   found in X.  */

static void
count_reg_references (x)
     rtx x;
{
  register RTX_CODE code;

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
    case ASM_INPUT:
      return;

#ifdef HAVE_cc0
    case CC0:
      return;
#endif

    case CLOBBER:
      /* If we are clobbering a MEM, mark any registers inside the address
	 as being used.  */
      if (GET_CODE (XEXP (x, 0)) == MEM)
	count_reg_references (XEXP (XEXP (x, 0), 0));
      return;

    case SUBREG:
      /* While we're here, optimize this case.  */
      x = SUBREG_REG (x);

      /* In case the SUBREG is not of a register, don't optimize */
      if (GET_CODE (x) != REG)
	{
	  count_reg_references (x);
	  return;
	}

      /* ... fall through ...  */

    case REG:
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER)
	REG_N_REFS (REGNO (x)) += loop_depth;
      return;

    case SET:
      {
	register rtx testreg = SET_DEST (x);
	int mark_dest = 0;

	/* If storing into MEM, don't show it as being used.  But do
	   show the address as being used.  */
	if (GET_CODE (testreg) == MEM)
	  {
	    count_reg_references (XEXP (testreg, 0));
	    count_reg_references (SET_SRC (x));
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

	/* If this is a store into a register,
	   recursively scan the value being stored.  */

	if ((GET_CODE (testreg) == PARALLEL
	     && GET_MODE (testreg) == BLKmode)
	    || GET_CODE (testreg) == REG)
	  {
	    count_reg_references (SET_SRC (x));
	    if (mark_dest)
	      count_reg_references (SET_DEST (x));
	    return;
	  }
      }
      break;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register char *fmt = GET_RTX_FORMAT (code);
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
	    count_reg_references (XEXP (x, i));
	  }
	else if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      count_reg_references (XVECEXP (x, i, j));
	  }
      }
  }
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
   loop nesting level in order to increase the ref count more for references
   in a loop.

   It might be worthwhile to update REG_LIVE_LENGTH, REG_BASIC_BLOCK and
   possibly other information which is used by the register allocators.  */

void
recompute_reg_usage (f, loop_step)
     rtx f;
     int loop_step;
{
  rtx insn;
  int i, max_reg;

  /* Clear out the old data.  */
  max_reg = max_reg_num ();
  for (i = FIRST_PSEUDO_REGISTER; i < max_reg; i++)
    {
      REG_N_SETS (i) = 0;
      REG_N_REFS (i) = 0;
    }

  /* Scan each insn in the chain and count how many times each register is
     set/used.  */
  loop_depth = 1;
  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      /* Keep track of loop depth.  */
      if (GET_CODE (insn) == NOTE)
	{
	  /* Look for loop boundaries.  */
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    loop_depth -= loop_step;
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    loop_depth += loop_step;

	  /* If we have LOOP_DEPTH == 0, there has been a bookkeeping error. 
	     Abort now rather than setting register status incorrectly.  */
	  if (loop_depth == 0)
	    abort ();
	}
      else if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  rtx links;

	  /* This call will increment REG_N_SETS for each SET or CLOBBER
	     of a register in INSN.  It will also increment REG_N_REFS
	     by the loop depth for each set of a register in INSN.  */
	  count_reg_sets (PATTERN (insn));

	  /* count_reg_sets does not detect autoincrement address modes, so
	     detect them here by looking at the notes attached to INSN.  */
	  for (links = REG_NOTES (insn); links; links = XEXP (links, 1))
	    {
	      if (REG_NOTE_KIND (links) == REG_INC)
		/* Count (weighted) references, stores, etc.  This counts a
		   register twice if it is modified, but that is correct.  */
		REG_N_SETS (REGNO (XEXP (links, 0)))++;
	    }

	  /* This call will increment REG_N_REFS by the current loop depth for
	     each reference to a register in INSN.  */
	  count_reg_references (PATTERN (insn));

	  /* count_reg_references will not include counts for arguments to
	     function calls, so detect them here by examining the
	     CALL_INSN_FUNCTION_USAGE data.  */
	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      rtx note;

	      for (note = CALL_INSN_FUNCTION_USAGE (insn);
		   note;
		   note = XEXP (note, 1))
		if (GET_CODE (XEXP (note, 0)) == USE)
		  count_reg_references (SET_DEST (XEXP (note, 0)));
	    }
	}
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

   In future it can be extended check a lot of other stuff as well
   (reachability of basic blocks, life information, etc. etc.).  */

void
verify_flow_info ()
{
  const int max_uid = get_max_uid ();
  const rtx rtx_first = get_insns ();
  basic_block *bb_info;
  rtx x;
  int i;

  bb_info = (basic_block *) alloca (max_uid * sizeof (basic_block));
  memset (bb_info, 0, max_uid * sizeof (basic_block));

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
	  fatal ("verify_flow_info: Head insn %d for block %d not found in the insn stream.\n",
		 INSN_UID (bb->head), bb->index);
	}

      /* Check the end pointer and make sure that it is pointing into
         insn list.  */
      for (x = bb->head; x != NULL_RTX; x = NEXT_INSN (x))
	{
	  if (bb_info[INSN_UID (x)] != NULL)
	    {
	      fatal ("verify_flow_info: Insn %d is in multiple basic blocks (%d and %d)",
		     INSN_UID (x), bb->index, bb_info[INSN_UID (x)]->index);
	    }
	  bb_info[INSN_UID (x)] = bb;

	  if (x == bb->end)
	    break;
	}
      if (!x)
	{
	  fatal ("verify_flow_info: End insn %d for block %d not found in the insn stream.\n",
		 INSN_UID (bb->end), bb->index);
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
	      abort ();
	    }
	  if (e->dest != EXIT_BLOCK_PTR)
	    {
	      edge e2 = e->dest->pred;
	      while (e2 && e2 != e)
		e2 = e2->pred_next;
	      if (!e2)
		{
		  fatal ("verify_flow_info: Basic block %i edge lists are corrupted\n",
			 bb->index);
		}
	    }
	  e = e->succ_next;
	}

      e = bb->pred;
      while (e)
	{
	  if (e->dest != bb)
	    {
	      fprintf (stderr, "verify_flow_info: Basic block %d pred edge is corrupted\n",
		       bb->index);
	      fprintf (stderr, "Predecessor: ");
	      dump_edge_info (stderr, e, 0);
	      fprintf (stderr, "\nSuccessor: ");
	      dump_edge_info (stderr, e, 1);
	      fflush (stderr);
	      abort ();
	    }
	  if (e->src != ENTRY_BLOCK_PTR)
	    {
	      edge e2 = e->src->succ;
	      while (e2 && e2 != e)
		e2 = e2->succ_next;
	      if (!e2)
		{
		  fatal ("verify_flow_info: Basic block %i edge lists are corrupted\n",
			 bb->index);
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
	      fatal ("verify_flow_info: Basic block contains only CODE_LABEL and no NOTE_INSN_BASIC_BLOCK note\n");
	    }
	  x = NEXT_INSN (x);
	}
      if (GET_CODE (x) != NOTE
	  || NOTE_LINE_NUMBER (x) != NOTE_INSN_BASIC_BLOCK
	  || NOTE_BASIC_BLOCK (x) != bb)
	{
	  fatal ("verify_flow_info: NOTE_INSN_BASIC_BLOCK is missing for block %d\n",
		 bb->index);
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
		  fatal ("verify_flow_info: NOTE_INSN_BASIC_BLOCK %d in the middle of basic block %d\n",
			 INSN_UID (x), bb->index);
		}

	      if (x == bb->end)
		break;

	      if (GET_CODE (x) == JUMP_INSN
		  || GET_CODE (x) == CODE_LABEL
		  || GET_CODE (x) == BARRIER)
		{
		  fatal_insn ("verify_flow_info: Incorrect insn in the middle of basic block %d\n",
			      x, bb->index);
		}

	      x = NEXT_INSN (x);
	    }
	}
    }

  x = rtx_first;
  while (x)
    {
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
	      fatal_insn ("verify_flow_info: Insn outside basic block\n", x);
	    }
	}

      x = NEXT_INSN (x);
    }
}
