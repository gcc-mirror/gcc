/* Dead-code elimination pass for the GNU compiler.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Written by Jeffrey D. Oldham <oldham@codesourcery.com>.

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

/* Dead-code elimination is the removal of instructions which have no
   impact on the program's output.  "Dead instructions" have no impact
   on the program's output, while "necessary instructions" may have
   impact on the output.

   The algorithm consists of three phases:
   1) marking as necessary all instructions known to be necessary,
      e.g., writing a value to memory,
   2) propagating necessary instructions, e.g., the instructions
      giving values to operands in necessary instructions, and
   3) removing dead instructions (except replacing dead conditionals
      with unconditional jumps).

   Side Effects:
   The last step can require adding labels, deleting insns, and
   modifying basic block structures.  Some conditional jumps may be
   converted to unconditional jumps so the control-flow graph may be
   out-of-date.

   Edges from some infinite loops to the exit block can be added to
   the control-flow graph, but will be removed after this pass is
   complete.

   It Does Not Perform:
   We decided to not simultaneously perform jump optimization and dead
   loop removal during dead-code elimination.  Thus, all jump
   instructions originally present remain after dead-code elimination
   but 1) unnecessary conditional jump instructions are changed to
   unconditional jump instructions and 2) all unconditional jump
   instructions remain.

   Assumptions:
   1) SSA has been performed.
   2) The basic block and control-flow graph structures are accurate.
   3) The flow graph permits constructing an edge_list.
   4) note rtxes should be saved.

   Unfinished:
   When replacing unnecessary conditional jumps with unconditional
   jumps, the control-flow graph is not updated.  It should be.

   References:
   Building an Optimizing Compiler
   Robert Morgan
   Butterworth-Heinemann, 1998
   Section 8.9
*/

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "ssa.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"


/* A map from blocks to the edges on which they are control dependent.  */
typedef struct {
  /* An dynamically allocated array.  The Nth element corresponds to
     the block with index N + 2.  The Ith bit in the bitmap is set if
     that block is dependent on the Ith edge.  */
  bitmap *data;
  /* The number of elements in the array.  */
  int length;
} control_dependent_block_to_edge_map_s, *control_dependent_block_to_edge_map;

/* Local function prototypes.  */
static control_dependent_block_to_edge_map control_dependent_block_to_edge_map_create
  PARAMS((size_t num_basic_blocks));
static void set_control_dependent_block_to_edge_map_bit
  PARAMS ((control_dependent_block_to_edge_map c, basic_block bb,
	   int edge_index));
static void control_dependent_block_to_edge_map_free
  PARAMS ((control_dependent_block_to_edge_map c));
static void find_all_control_dependences
  PARAMS ((struct edge_list *el, dominance_info pdom,
	   control_dependent_block_to_edge_map cdbte));
static void find_control_dependence
  PARAMS ((struct edge_list *el, int edge_index, dominance_info pdom,
	   control_dependent_block_to_edge_map cdbte));
static basic_block find_pdom
  PARAMS ((dominance_info pdom, basic_block block));
static int inherently_necessary_register_1
  PARAMS ((rtx *current_rtx, void *data));
static int inherently_necessary_register
  PARAMS ((rtx current_rtx));
static int find_inherently_necessary
  PARAMS ((rtx current_rtx));
static int propagate_necessity_through_operand
  PARAMS ((rtx *current_rtx, void *data));
static void note_inherently_necessary_set
  PARAMS ((rtx, rtx, void *));

/* Unnecessary insns are indicated using insns' in_struct bit.  */

/* Indicate INSN is dead-code; returns nothing.  */
#define KILL_INSN(INSN)		INSN_DEAD_CODE_P(INSN) = 1
/* Indicate INSN is necessary, i.e., not dead-code; returns nothing.  */
#define RESURRECT_INSN(INSN)	INSN_DEAD_CODE_P(INSN) = 0
/* Return nonzero if INSN is unnecessary.  */
#define UNNECESSARY_P(INSN)	INSN_DEAD_CODE_P(INSN)
static void mark_all_insn_unnecessary
  PARAMS ((void));
/* Execute CODE with free variable INSN for all unnecessary insns in
   an unspecified order, producing no output.  */
#define EXECUTE_IF_UNNECESSARY(INSN, CODE)	\
{								\
  rtx INSN;							\
								\
  for (INSN = get_insns (); INSN != NULL_RTX; INSN = NEXT_INSN (INSN))	\
    if (INSN_DEAD_CODE_P (INSN)) {				\
      CODE;							\
    }								\
}
/* Find the label beginning block BB.  */
static rtx find_block_label
  PARAMS ((basic_block bb));
/* Remove INSN, updating its basic block structure.  */
static void delete_insn_bb
  PARAMS ((rtx insn));

/* Recording which blocks are control dependent on which edges.  We
   expect each block to be control dependent on very few edges so we
   use a bitmap for each block recording its edges.  An array holds
   the bitmap.  Its position 0 entry holds the bitmap for block
   INVALID_BLOCK+1 so that all blocks, including the entry and exit
   blocks can participate in the data structure.  */

/* Create a control_dependent_block_to_edge_map, given the number
   NUM_BASIC_BLOCKS of non-entry, non-exit basic blocks, e.g.,
   n_basic_blocks.  This memory must be released using
   control_dependent_block_to_edge_map_free ().  */

static control_dependent_block_to_edge_map
control_dependent_block_to_edge_map_create (num_basic_blocks)
     size_t num_basic_blocks;
{
  int i;
  control_dependent_block_to_edge_map c
    = xmalloc (sizeof (control_dependent_block_to_edge_map_s));
  c->length = num_basic_blocks - (INVALID_BLOCK+1);
  c->data = xmalloc ((size_t) c->length*sizeof (bitmap));
  for (i = 0; i < c->length; ++i)
    c->data[i] = BITMAP_XMALLOC ();

  return c;
}

/* Indicate block BB is control dependent on an edge with index
   EDGE_INDEX in the mapping C of blocks to edges on which they are
   control-dependent.  */

static void
set_control_dependent_block_to_edge_map_bit (c, bb, edge_index)
     control_dependent_block_to_edge_map c;
     basic_block bb;
     int edge_index;
{
  if (bb->index - (INVALID_BLOCK+1) >= c->length)
    abort ();

  bitmap_set_bit (c->data[bb->index - (INVALID_BLOCK+1)],
		  edge_index);
}

/* Execute CODE for each edge (given number EDGE_NUMBER within the
   CODE) for which the block containing INSN is control dependent,
   returning no output.  CDBTE is the mapping of blocks to edges on
   which they are control-dependent.  */

#define EXECUTE_IF_CONTROL_DEPENDENT(CDBTE, INSN, EDGE_NUMBER, CODE) \
	EXECUTE_IF_SET_IN_BITMAP \
	  (CDBTE->data[BLOCK_NUM (INSN) - (INVALID_BLOCK+1)], 0, \
	  EDGE_NUMBER, CODE)

/* Destroy a control_dependent_block_to_edge_map C.  */

static void
control_dependent_block_to_edge_map_free (c)
     control_dependent_block_to_edge_map c;
{
  int i;
  for (i = 0; i < c->length; ++i)
    BITMAP_XFREE (c->data[i]);
  free ((PTR) c);
}

/* Record all blocks' control dependences on all edges in the edge
   list EL, ala Morgan, Section 3.6.  The mapping PDOM of blocks to
   their postdominators are used, and results are stored in CDBTE,
   which should be empty.  */

static void
find_all_control_dependences (el, pdom, cdbte)
   struct edge_list *el;
   dominance_info pdom;
   control_dependent_block_to_edge_map cdbte;
{
  int i;

  for (i = 0; i < NUM_EDGES (el); ++i)
    find_control_dependence (el, i, pdom, cdbte);
}

/* Determine all blocks' control dependences on the given edge with
   edge_list EL index EDGE_INDEX, ala Morgan, Section 3.6.  The
   mapping PDOM of blocks to their postdominators are used, and
   results are stored in CDBTE, which is assumed to be initialized
   with zeros in each (block b', edge) position.  */

static void
find_control_dependence (el, edge_index, pdom, cdbte)
   struct edge_list *el;
   int edge_index;
   dominance_info pdom;
   control_dependent_block_to_edge_map cdbte;
{
  basic_block current_block;
  basic_block ending_block;

  if (INDEX_EDGE_PRED_BB (el, edge_index) == EXIT_BLOCK_PTR)
    abort ();
  ending_block =
    (INDEX_EDGE_PRED_BB (el, edge_index) == ENTRY_BLOCK_PTR)
    ? ENTRY_BLOCK_PTR->next_bb
    : find_pdom (pdom, INDEX_EDGE_PRED_BB (el, edge_index));

  for (current_block = INDEX_EDGE_SUCC_BB (el, edge_index);
       current_block != ending_block && current_block != EXIT_BLOCK_PTR;
       current_block = find_pdom (pdom, current_block))
    {
      set_control_dependent_block_to_edge_map_bit (cdbte,
						   current_block,
						   edge_index);
    }
}

/* Find the immediate postdominator PDOM of the specified basic block
   BLOCK.  This function is necessary because some blocks have
   negative numbers.  */

static basic_block
find_pdom (pdom, block)
     dominance_info pdom;
     basic_block block;
{
  if (!block)
    abort ();
  if (block->index == INVALID_BLOCK)
    abort ();

  if (block == ENTRY_BLOCK_PTR)
    return ENTRY_BLOCK_PTR->next_bb;
  else if (block == EXIT_BLOCK_PTR)
    return EXIT_BLOCK_PTR;
  else
    {
      basic_block bb = get_immediate_dominator (pdom, block);
      if (!bb)
	return EXIT_BLOCK_PTR;
      return bb;
    }
}

/* Determine if the given CURRENT_RTX uses a hard register not
   converted to SSA.  Returns nonzero only if it uses such a hard
   register.  DATA is not used.

   The program counter (PC) is not considered inherently necessary
   since code should be position-independent and thus not depend on
   particular PC values.  */

static int
inherently_necessary_register_1 (current_rtx, data)
     rtx *current_rtx;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *current_rtx;

  if (x == NULL_RTX)
    return 0;
  switch (GET_CODE (x))
    {
    case CLOBBER:
      /* Do not traverse the rest of the clobber.  */
      return -1;
      break;
    case PC:
      return 0;
      break;
    case REG:
      if (CONVERT_REGISTER_TO_SSA_P (REGNO (x)) || x == pc_rtx)
	return 0;
      else
	return !0;
      break;
    default:
      return 0;
      break;
    }
}

/* Return nonzero if the insn CURRENT_RTX is inherently necessary.  */

static int
inherently_necessary_register (current_rtx)
     rtx current_rtx;
{
  return for_each_rtx (&current_rtx,
		       &inherently_necessary_register_1, NULL);
}


/* Called via note_stores for each store in an insn.  Note whether
   or not a particular store is inherently necessary.  Store a
   nonzero value in inherently_necessary_p if such a store is found.  */

static void
note_inherently_necessary_set (dest, set, data)
     rtx set ATTRIBUTE_UNUSED;
     rtx dest;
     void *data;
{
  int *inherently_necessary_set_p = (int *) data;

  while (GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == STRICT_LOW_PART
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || GET_CODE (dest) == SIGN_EXTRACT)
    dest = XEXP (dest, 0);

  if (GET_CODE (dest) == MEM
      || GET_CODE (dest) == UNSPEC
      || GET_CODE (dest) == UNSPEC_VOLATILE)
    *inherently_necessary_set_p = 1;
}

/* Mark X as inherently necessary if appropriate.  For example,
   function calls and storing values into memory are inherently
   necessary.  This function is to be used with for_each_rtx ().
   Return nonzero iff inherently necessary.  */

static int
find_inherently_necessary (x)
     rtx x;
{
  if (x == NULL_RTX)
    return 0;
  else if (inherently_necessary_register (x))
    return !0;
  else
    switch (GET_CODE (x))
      {
      case CALL_INSN:
      case BARRIER:
      case PREFETCH:
	return !0;
      case CODE_LABEL:
      case NOTE:
	return 0;
      case JUMP_INSN:
	return JUMP_TABLE_DATA_P (x) || computed_jump_p (x) != 0;
      case INSN:
	{
	  int inherently_necessary_set = 0;
	  note_stores (PATTERN (x),
		       note_inherently_necessary_set,
		       &inherently_necessary_set);

	  /* If we found an inherently necessary set or an asm
	     instruction, then we consider this insn inherently
	     necessary.  */
	  return (inherently_necessary_set
		  || GET_CODE (PATTERN (x)) == ASM_INPUT
		  || asm_noperands (PATTERN (x)) >= 0);
	}
      default:
	/* Found an impossible insn type.  */
	abort ();
	break;
      }
}

/* Propagate necessity through REG and SUBREG operands of CURRENT_RTX.
   This function is called with for_each_rtx () on necessary
   instructions.  The DATA must be a varray of unprocessed
   instructions.  */

static int
propagate_necessity_through_operand (current_rtx, data)
     rtx *current_rtx;
     void *data;
{
  rtx x = *current_rtx;
  varray_type *unprocessed_instructions = (varray_type *) data;

  if (x == NULL_RTX)
    return 0;
  switch ( GET_CODE (x))
    {
    case REG:
      if (CONVERT_REGISTER_TO_SSA_P (REGNO (x)))
	{
	  rtx insn = VARRAY_RTX (ssa_definition, REGNO (x));
	  if (insn != NULL_RTX && UNNECESSARY_P (insn))
	    {
	      RESURRECT_INSN (insn);
	      VARRAY_PUSH_RTX (*unprocessed_instructions, insn);
	    }
	}
      return 0;

    default:
      return 0;
    }
}

/* Indicate all insns initially assumed to be unnecessary.  */

static void
mark_all_insn_unnecessary ()
{
  rtx insn;
  for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
    KILL_INSN (insn);
}

/* Find the label beginning block BB, adding one if necessary.  */

static rtx
find_block_label (bb)
     basic_block bb;
{
  rtx insn = bb->head;
  if (LABEL_P (insn))
    return insn;
  else
    {
      rtx new_label = emit_label_before (gen_label_rtx (), insn);
      if (insn == bb->head)
	bb->head = new_label;
      return new_label;
    }
}

/* Remove INSN, updating its basic block structure.  */

static void
delete_insn_bb (insn)
     rtx insn;
{
  if (!insn)
    abort ();

  /* Do not actually delete anything that is not an INSN.

     We can get here because we only consider INSNs as
     potentially necessary.  We leave it to later passes
     to remove unnecessary notes, unused labels, etc.  */
  if (! INSN_P (insn))
    return;

  delete_insn (insn);
}

/* Perform the dead-code elimination.  */

void
ssa_eliminate_dead_code ()
{
  rtx insn;
  basic_block bb;
  /* Necessary instructions with operands to explore.  */
  varray_type unprocessed_instructions;
  /* Map element (b,e) is nonzero if the block is control dependent on
     edge.  "cdbte" abbreviates control dependent block to edge.  */
  control_dependent_block_to_edge_map cdbte;
 /* Element I is the immediate postdominator of block I.  */
  dominance_info pdom;
  struct edge_list *el;

  /* Initialize the data structures.  */
  mark_all_insn_unnecessary ();
  VARRAY_RTX_INIT (unprocessed_instructions, 64,
		   "unprocessed instructions");
  cdbte = control_dependent_block_to_edge_map_create (last_basic_block);

  /* Prepare for use of BLOCK_NUM ().  */
  connect_infinite_loops_to_exit ();

  /* Compute control dependence.  */
  pdom = calculate_dominance_info (CDI_POST_DOMINATORS);
  el = create_edge_list ();
  find_all_control_dependences (el, pdom, cdbte);

  /* Find inherently necessary instructions.  */
  for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
    if (find_inherently_necessary (insn))
      {
	RESURRECT_INSN (insn);
	VARRAY_PUSH_RTX (unprocessed_instructions, insn);
      }

  /* Propagate necessity using the operands of necessary instructions.  */
  while (VARRAY_ACTIVE_SIZE (unprocessed_instructions) > 0)
    {
      rtx current_instruction;
      int edge_number;

      current_instruction = VARRAY_TOP_RTX (unprocessed_instructions);
      VARRAY_POP (unprocessed_instructions);

      /* Make corresponding control dependent edges necessary.  */
      /* Assume the only JUMP_INSN is the block's last insn.  It appears
	 that the last instruction of the program need not be a
	 JUMP_INSN.  */

      if (INSN_P (current_instruction)
	  && !JUMP_TABLE_DATA_P (current_instruction))
	{
	  /* Notes and labels contain no interesting operands.  */
	  EXECUTE_IF_CONTROL_DEPENDENT
	    (cdbte, current_instruction, edge_number,
	    {
	      rtx jump_insn = (INDEX_EDGE_PRED_BB (el, edge_number))->end;
	      if (GET_CODE (jump_insn) == JUMP_INSN
		  && UNNECESSARY_P (jump_insn))
		{
		  RESURRECT_INSN (jump_insn);
		  VARRAY_PUSH_RTX (unprocessed_instructions, jump_insn);
		}
	    });

	  /* Propagate through the operands.  */
	  for_each_rtx (&current_instruction,
			&propagate_necessity_through_operand,
			(PTR) &unprocessed_instructions);

	  /* PHI nodes are somewhat special in that each PHI alternative
	     has data and control dependencies.  The data dependencies
	     are handled via propagate_necessity_through_operand.  We
	     handle the control dependency here.

	     We consider the control dependent edges leading to the
	     predecessor block associated with each PHI alternative
	     as necessary.  */
	  if (PHI_NODE_P (current_instruction))
	    {
	      rtvec phi_vec = XVEC (SET_SRC (PATTERN (current_instruction)), 0);
	      int num_elem = GET_NUM_ELEM (phi_vec);
	      int v;

	      for (v = num_elem - 2; v >= 0; v -= 2)
		{
		  basic_block bb;

		  bb = BASIC_BLOCK (INTVAL (RTVEC_ELT (phi_vec, v + 1)));
		  EXECUTE_IF_CONTROL_DEPENDENT
		    (cdbte, bb->end, edge_number,
		    {
		      rtx jump_insn;

		      jump_insn = (INDEX_EDGE_PRED_BB (el, edge_number))->end;
		      if (((GET_CODE (jump_insn) == JUMP_INSN))
			  && UNNECESSARY_P (jump_insn))
			{
			  RESURRECT_INSN (jump_insn);
			  VARRAY_PUSH_RTX (unprocessed_instructions, jump_insn);
			}
		    });

		}
	    }
	}
    }

  /* Remove the unnecessary instructions.  */
  EXECUTE_IF_UNNECESSARY (insn,
  {
    if (any_condjump_p (insn))
      {
	basic_block bb = BLOCK_FOR_INSN (insn);
	basic_block pdom_bb = find_pdom (pdom, bb);
	rtx lbl;
	edge e;

	/* Egad.  The immediate post dominator is the exit block.  We
	   would like to optimize this conditional jump to jump directly
	   to the exit block.  That can be difficult as we may not have
	   a suitable CODE_LABEL that allows us to fall unmolested into
	   the exit block.

	   So, we just delete the conditional branch by turning it into
	   a deleted note.   That is safe, but just not as optimal as
	   it could be.  */
	if (pdom_bb == EXIT_BLOCK_PTR)
	  {
	    /* Since we're going to just delete the branch, we need
	       look at all the edges and remove all those which are not
	       a fallthru edge.  */
	    e = bb->succ;
	    while (e)
	      {
		edge temp = e;

		e = e->succ_next;
		if ((temp->flags & EDGE_FALLTHRU) == 0)
		  {
		    /* We've found a non-fallthru edge, find any PHI nodes
		       at the target and clean them up.  */
		    if (temp->dest != EXIT_BLOCK_PTR)
		      {
		        rtx insn
			  = first_insn_after_basic_block_note (temp->dest);

		        while (PHI_NODE_P (insn))
			  {
			    remove_phi_alternative (PATTERN (insn), temp->src);
			    insn = NEXT_INSN (insn);
			  }
		      }

		    remove_edge (temp);
		  }
	      }

	    /* Now "delete" the conditional jump.  */
	    PUT_CODE (insn, NOTE);
	    NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	    continue;
	  }

	/* We've found a conditional branch that is unnecessary.

	   First, remove all outgoing edges from this block, updating
	   PHI nodes as appropriate.  */
	e = bb->succ;
	while (e)
	  {
	    edge temp = e;

	    e = e->succ_next;

	    if (temp->flags & EDGE_ABNORMAL)
	      continue;

	    /* We found an edge that is not executable.  First simplify
	       the PHI nodes in the target block.  */
	    if (temp->dest != EXIT_BLOCK_PTR)
	      {
		rtx insn = first_insn_after_basic_block_note (temp->dest);

		while (PHI_NODE_P (insn))
		  {
		    remove_phi_alternative (PATTERN (insn), temp->src);
		    insn = NEXT_INSN (insn);
		  }
	      }

	    remove_edge (temp);
	  }

	/* Create an edge from this block to the post dominator.
	   What about the PHI nodes at the target?  */
	make_edge (bb, pdom_bb, 0);

	/* Third, transform this insn into an unconditional
	   jump to the label for the immediate postdominator.  */
	lbl = find_block_label (pdom_bb);
	SET_SRC (PATTERN (insn)) = gen_rtx_LABEL_REF (VOIDmode, lbl);
	INSN_CODE (insn) = -1;
	JUMP_LABEL (insn) = lbl;
	LABEL_NUSES (lbl)++;

	/* A barrier must follow any unconditional jump.  Barriers
	   are not in basic blocks so this must occur after
	   deleting the conditional jump.  */
	emit_barrier_after (insn);
      }
    else if (!JUMP_P (insn))
      delete_insn_bb (insn);
  });

  /* Remove fake edges from the CFG.  */
  remove_fake_edges ();

  /* Find any blocks with no successors and ensure they are followed
     by a BARRIER.  delete_insn has the nasty habit of deleting barriers
     when deleting insns.  */
  FOR_EACH_BB (bb)
    {
      if (bb->succ == NULL)
	{
	  rtx next = NEXT_INSN (bb->end);

	  if (!next || GET_CODE (next) != BARRIER)
	    emit_barrier_after (bb->end);
	}
    }
  /* Release allocated memory.  */
  for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
    RESURRECT_INSN (insn);
  if (VARRAY_ACTIVE_SIZE (unprocessed_instructions) != 0)
    abort ();
  control_dependent_block_to_edge_map_free (cdbte);
  free ((PTR) pdom);
  free_edge_list (el);
}
