/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Original framework by Daniel Berlin <dan@cgsoftware.com>
   Fleshed out and major cleanups by Jeff Law <law@redhat.com>

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

/* Conditional constant propagation.

   References:

     Constant propagation with conditional branches,
     Wegman and Zadeck, ACM TOPLAS 13(2):181-210.

     Building an Optimizing Compiler,
     Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     Advanced Compiler Design and Implementation,
     Steven Muchnick, Morgan Kaufmann, 1997, Section 12.6

   The overall structure is as follows:

	1. Run a simple SSA based DCE pass to remove any dead code.
	2. Run CCP to compute what registers are known constants
	   and what edges are not executable.  Remove unexecutable
	   edges from the CFG and simplify PHI nodes.
	3. Replace registers with constants where possible.
	4. Remove unreachable blocks computed in step #2.
	5. Another simple SSA DCE pass to remove dead code exposed
	   by CCP.

   When we exit, we are still in SSA form.


   Potential further enhancements:

    1. Handle SUBREGs, STRICT_LOW_PART, etc in destinations more
       gracefully.

    2. Handle insns with multiple outputs more gracefully.

    3. Handle CONST_DOUBLE and symbolic constants.

    4. Fold expressions after performing constant substitutions.  */


#include "config.h"
#include "system.h"

#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "ssa.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "errors.h"
#include "ggc.h"
#include "df.h"
#include "function.h"

/* Possible lattice values.  */

typedef enum
{
  UNDEFINED,
  CONSTANT,
  VARYING
} latticevalue;

/* Main structure for CCP.

   Contains the lattice value and, if it's a constant, the constant
   value.  */
typedef struct
{
  latticevalue lattice_val;
  rtx const_value;
} value;

/* Array of values indexed by register number.  */
static value *values;

/* A bitmap to keep track of executable blocks in the CFG.  */
static sbitmap executable_blocks;

/* A bitmap for all executable edges in the CFG.  */
static sbitmap executable_edges;

/* Array of edges on the work list.  */
static edge *edge_info;

/* We need an edge list to be able to get indexes easily.  */
static struct edge_list *edges;

/* For building/following use-def and def-use chains.  */
static struct df *df_analyzer;

/* Current edge we are operating on, from the worklist */
static edge flow_edges;

/* Bitmap of SSA edges which will need reexamination as their definition
   has changed.  */
static sbitmap ssa_edges;

/* Simple macros to simplify code */
#define SSA_NAME(x) REGNO (SET_DEST (x))
#define EIE(x,y) EDGE_INDEX (edges, x, y)

static void visit_phi_node             PARAMS ((rtx, basic_block));
static void visit_expression           PARAMS ((rtx, basic_block));
static void defs_to_undefined          PARAMS ((rtx));
static void defs_to_varying            PARAMS ((rtx));
static void examine_flow_edges         PARAMS ((void));
static int mark_references             PARAMS ((rtx *, void *));
static void follow_def_use_chains      PARAMS ((void));
static void optimize_unexecutable_edges PARAMS ((struct edge_list *, sbitmap));
static void ssa_ccp_substitute_constants PARAMS ((void));
static void ssa_ccp_df_delete_unreachable_insns PARAMS ((void));
static void ssa_fast_dce PARAMS ((struct df *));

/* Loop through the PHI_NODE's parameters for BLOCK and compare their
   lattice values to determine PHI_NODE's lattice value.  */
static void
visit_phi_node (phi_node, block)
     rtx phi_node;
     basic_block block;
{
  unsigned int i;
  rtx phi_node_expr = NULL;
  unsigned int phi_node_name = SSA_NAME (PATTERN (phi_node));
  latticevalue phi_node_lattice_val = UNDEFINED;
  rtx pat = PATTERN (phi_node);
  rtvec phi_vec = XVEC (SET_SRC (pat), 0);
  unsigned int num_elem = GET_NUM_ELEM (phi_vec);

  for (i = 0; i < num_elem; i += 2)
    {
      if (TEST_BIT (executable_edges,
		    EIE (BASIC_BLOCK (INTVAL (RTVEC_ELT (phi_vec, i + 1))),
			 block)))
	{
	  unsigned int current_parm
	    = REGNO (RTVEC_ELT (phi_vec, i));

	  latticevalue current_parm_lattice_val
	    = values[current_parm].lattice_val;

	  /* If any node is VARYING, then new value of PHI_NODE
	     is VARYING.  */
	  if (current_parm_lattice_val == VARYING)
	    {
	      phi_node_lattice_val = VARYING;
	      phi_node_expr = NULL;
	      break;
	    }

	  /* If we have more than one distinct constant, then the new
	     value of PHI_NODE is VARYING.  */
	  if (current_parm_lattice_val == CONSTANT
	      && phi_node_lattice_val == CONSTANT
	      && values[current_parm].const_value != phi_node_expr)
	    {
	      phi_node_lattice_val = VARYING;
	      phi_node_expr = NULL;
	      break;
	    }

	  /* If the current value of PHI_NODE is UNDEFINED and one
	     node in PHI_NODE is CONSTANT, then the new value of the
	     PHI is that CONSTANT.  Note this can turn into VARYING
	     if we find another distinct constant later.  */
	  if (phi_node_lattice_val == UNDEFINED
	      && phi_node_expr == NULL
	      && current_parm_lattice_val == CONSTANT)
	    {
	      phi_node_expr = values[current_parm].const_value;
	      phi_node_lattice_val = CONSTANT;
	      continue;
	    }
	}
    }

  /* If the value of PHI_NODE changed, then we will need to
     re-execute uses of the output of PHI_NODE.  */
  if (phi_node_lattice_val != values[phi_node_name].lattice_val)
    {
      values[phi_node_name].lattice_val = phi_node_lattice_val;
      values[phi_node_name].const_value = phi_node_expr;
      SET_BIT (ssa_edges, phi_node_name);
    }
}

/* Sets all defs in an insn to UNDEFINED.  */
static void
defs_to_undefined (insn)
     rtx insn;
{
  struct df_link *currdef;
  for (currdef = DF_INSN_DEFS (df_analyzer, insn); currdef;
       currdef = currdef->next)
    {
      if (values[DF_REF_REGNO (currdef->ref)].lattice_val != UNDEFINED)
	SET_BIT (ssa_edges, DF_REF_REGNO (currdef->ref));
      values[DF_REF_REGNO (currdef->ref)].lattice_val = UNDEFINED;
    }
}

/* Sets all defs in an insn to VARYING.  */
static void
defs_to_varying (insn)
     rtx insn;
{
  struct df_link *currdef;
  for (currdef = DF_INSN_DEFS (df_analyzer, insn); currdef;
       currdef = currdef->next)
    {
      if (values[DF_REF_REGNO (currdef->ref)].lattice_val != VARYING)
	SET_BIT (ssa_edges, DF_REF_REGNO (currdef->ref));
      values[DF_REF_REGNO (currdef->ref)].lattice_val = VARYING;
    }
}

/* Go through the expression, call the appropriate evaluation routines
   to attempt cprop */
static void
visit_expression (insn, block)
     rtx insn;
     basic_block block;
{
  rtx src, dest, set;


  /* Ugh.  CALL_INSNs may end a basic block and have multiple edges
     leading out from them.

     Mark all the outgoing edges as executable, then fall into the
     normal processing below.  */
  if (GET_CODE (insn) == CALL_INSN && block->end == insn)
    {
      edge curredge;

      for (curredge = block->succ; curredge;
	   curredge = curredge->succ_next)
	{
	  int index = EIE (curredge->src, curredge->dest);

	  if (TEST_BIT (executable_edges, index))
	    continue;

	  SET_BIT (executable_edges, index);
	  edge_info[index] = flow_edges;
	  flow_edges = curredge;
	}
    }

  set = single_set (insn);
  if (! set)
    {
      defs_to_varying (insn);
      return;
    }

  src = SET_SRC (set);
  dest = SET_DEST (set);

  /* We may want to refine this some day.  */
  if (GET_CODE (dest) != REG && dest != pc_rtx)
    {
      defs_to_varying (insn);
      return;
    }

  /* Hard registers are not put in SSA form and thus we must consider
     them varying.  All the more reason to avoid hard registers in
     RTL until as late as possible in the compilation.  */
  if (GET_CODE (dest) == REG && REGNO (dest) < FIRST_PSEUDO_REGISTER)
    {
      defs_to_varying (insn);
      return;
    }

  /* If this is assigning DEST to a constant, record that fact.  */
  if (GET_CODE (src) == CONST_INT && GET_CODE (insn) == INSN)
    {
      unsigned int resultreg = REGNO (dest);

      values[resultreg].lattice_val = CONSTANT;
      values[resultreg].const_value = SET_SRC (PATTERN (insn));
      SET_BIT (ssa_edges, resultreg);
    }

  /* If this is a copy operation, then we can copy the lattice values.  */
  else if (GET_CODE (src) == REG && GET_CODE (dest) == REG)
    {
      unsigned int old_value = REGNO (src);
      latticevalue old_lattice_value = values[old_value].lattice_val;
      unsigned int new_value = REGNO (dest);

      /* Unless the lattice value is going to change, don't bother
         adding the "new value" into the worklist.  */
      if (values[new_value].lattice_val != old_lattice_value
	  || values[new_value].const_value != values[old_value].const_value)
	SET_BIT (ssa_edges, new_value);

      /* Copy the old lattice node info into the new value lattice node.  */
      values[new_value].lattice_val = old_lattice_value;
      values[new_value].const_value = values[old_value].const_value;
    }

  /* Handle jumps.  */
  else if (GET_CODE (insn) == JUMP_INSN)
    {
      rtx x = pc_set (insn);
      if (GET_CODE (src) != IF_THEN_ELSE)
	{
	  edge curredge;

	  /* This is a computed jump, table jump, or an unconditional
	     jump.  For all these cases we want to mark all successor
	     blocks as executable if they have not already been
	     marked.

	     One day we may try do better with swtich tables and
	     other computed jumps.  */
	  for (curredge = block->succ; curredge;
	       curredge = curredge->succ_next)
	    {
	      int index = EIE (curredge->src, curredge->dest);

	      if (TEST_BIT (executable_edges, index))
		continue;

	      SET_BIT (executable_edges, index);
	      edge_info[index] = flow_edges;
	      flow_edges = curredge;
	    }
	}
      else
	{
	  edge curredge;
	  enum rtx_code comparison_code;
	  rtx comparison_src0;
	  rtx comparison_src1;

	  comparison_code = GET_CODE (XEXP (src, 0));
	  comparison_src0 = XEXP (XEXP (src, 0), 0);
	  comparison_src1 = XEXP (XEXP (src, 0), 1);

	  /* If either operand is undefined, then there is nothing to
	     do right now.  If/when operands are later defined we will
	     revaluate the condition and take the appropriate action.  */
	  if ((GET_CODE (comparison_src0) == REG
	       && values[REGNO (comparison_src0)].lattice_val == UNDEFINED)
	      || (GET_CODE (comparison_src1) == REG
	          && values[REGNO (comparison_src1)].lattice_val == UNDEFINED))
	    return;

	  /* If either operand is varying, then we must consider all
	     paths as executable.  */
	  if ((GET_CODE (comparison_src0) == REG
	       && values[REGNO (comparison_src0)].lattice_val == VARYING)
	      || (GET_CODE (comparison_src1) == REG
	          && values[REGNO (comparison_src1)].lattice_val == VARYING))
	    {
	      for (curredge = block->succ; curredge;
	           curredge = curredge->succ_next)
	        {
	          int index = EIE (curredge->src, curredge->dest);

	          if (TEST_BIT (executable_edges, index))
		    continue;

	          SET_BIT (executable_edges, index);
	          edge_info[index] = flow_edges;
	          flow_edges = curredge;
	        }
	      return;
	    }

	  /* Try to simplify the comparison.  */
	  if (GET_CODE (comparison_src0) == REG
	      && values[REGNO (comparison_src0)].lattice_val == CONSTANT)
	    comparison_src0 = values[REGNO (comparison_src0)].const_value;

	  if (GET_CODE (comparison_src1) == REG
	      && values[REGNO (comparison_src1)].lattice_val == CONSTANT)
	    comparison_src1 = values[REGNO (comparison_src1)].const_value;

	  x = simplify_ternary_operation (IF_THEN_ELSE,
					  VOIDmode,
					  GET_MODE (XEXP (src, 0)),
					  gen_rtx (comparison_code,
						   GET_MODE (XEXP (src, 0)),
						   comparison_src0,
						   comparison_src1),
					  XEXP (src, 1),
					  XEXP (src, 2));

	  /* Walk through all the outgoing edges from this block and see
	     which (if any) we should mark as executable.  */
	  for (curredge = block->succ; curredge;
	       curredge = curredge->succ_next)
	    {
	      int index = EIE (curredge->src, curredge->dest);

	      if (TEST_BIT (executable_edges, index))
		continue;

	      /* If we were unable to simplify the expression at this
		 point, it's highly unlikely we'll be able to simplify
		 it later.  So consider all edges as executable if the
		 expression did not simplify.

		 If the expression simplified to (pc), then we know we
		 will take the fall-thru edge, so mark it.  Similarly,
		 if the expression simplified to (label_ref ...), then
		 we know the branch will be taken and we mark that
		 edge as taken.  */
	      if (!x
		  || (x == pc_rtx
		      && (curredge->flags & EDGE_FALLTHRU))
		  || (GET_CODE (x) == LABEL_REF
		      && ! (curredge->flags & EDGE_FALLTHRU)))
		{
		  SET_BIT (executable_edges, index);
		  edge_info[index] = flow_edges;
		  flow_edges = curredge;
		}
	    }
	}
    }
  else if (!PHI_NODE_P (insn))
    {
      rtx simplified = NULL;

      /* We've got some kind of INSN.  If it's simple, try to evaluate
	 it and record the results.

	 We already know this insn is a single_set and that it sets
	 a pseudo register.   So we just need to extract the source
	 arguments, simplify them to constants if possible, then
	 simplify the expression as a whole if possible.  */
      switch (GET_RTX_CLASS (GET_CODE (src)))
	{
	  case '<':
	    {
	      rtx src0 = XEXP (src, 0);
	      rtx src1 = XEXP (src, 1);
	      enum machine_mode mode;

	      /* If either is undefined, then the result is undefined.  */
	      if ((GET_CODE (src0) == REG
		   && values[REGNO (src0)].lattice_val == UNDEFINED)
		  || (GET_CODE (src1) == REG
		      && values[REGNO (src1)].lattice_val == UNDEFINED))
		{
		  defs_to_undefined (insn);
		  break;
		}

	      /* Determine the mode for the operation before we simplify
		 our arguments to constants.  */
	      mode = GET_MODE (src0);
	      if (mode == VOIDmode)
		mode = GET_MODE (src1);

	      /* Simplify source operands to whatever known values they
		 may have.  */
	      if (GET_CODE (src0) == REG
		  && values[REGNO (src0)].lattice_val == CONSTANT)
		src0 = values[REGNO (src0)].const_value;

	      if (GET_CODE (src1) == REG
		  && values[REGNO (src1)].lattice_val == CONSTANT)
		src1 = values[REGNO (src1)].const_value;

	      /* See if the simplifier can determine if this operation
		 computes a constant value.  */
	      simplified = simplify_relational_operation (GET_CODE (src),
							  mode, src0, src1);
	      break;

	    }

	  case '1':
	    {
	      rtx src0 = XEXP (src, 0);
	      enum machine_mode mode0 = GET_MODE (src0);

	      /* If the operand is undefined, then the result is undefined.  */
	      if (GET_CODE (src0) == REG
		   && values[REGNO (src0)].lattice_val == UNDEFINED)
		{
		  defs_to_undefined (insn);
		  break;
		}

	      /* Simplify source operands to whatever known values they
		 may have.  */
	      if (GET_CODE (src0) == REG
		  && values[REGNO (src0)].lattice_val == CONSTANT)
		src0 = values[REGNO (src0)].const_value;

	      /* See if the simplifier can determine if this operation
		 computes a constant value.  */
	      simplified = simplify_unary_operation (GET_CODE (src),
						     GET_MODE (src),
						     src0,
						     mode0);
	      break;
	    }

	  case '2':
	  case 'c':
	    {
	      rtx src0 = XEXP (src, 0);
	      rtx src1 = XEXP (src, 1);

	      /* If either is undefined, then the result is undefined.  */
	      if ((GET_CODE (src0) == REG
		   && values[REGNO (src0)].lattice_val == UNDEFINED)
		  || (GET_CODE (src1) == REG
		      && values[REGNO (src1)].lattice_val == UNDEFINED))
		{
		  defs_to_undefined (insn);
		  break;
		}

	      /* Simplify source operands to whatever known values they
		 may have.  */
	      if (GET_CODE (src0) == REG
		  && values[REGNO (src0)].lattice_val == CONSTANT)
		src0 = values[REGNO (src0)].const_value;

	      if (GET_CODE (src1) == REG
		  && values[REGNO (src1)].lattice_val == CONSTANT)
		src1 = values[REGNO (src1)].const_value;

	      /* See if the simplifier can determine if this operation
		 computes a constant value.  */
	      simplified = simplify_binary_operation (GET_CODE (src),
						      GET_MODE (src),
						      src0, src1);
	      break;
	    }

	  case '3':
	  case 'b':
	    {
	      rtx src0 = XEXP (src, 0);
	      rtx src1 = XEXP (src, 1);
	      rtx src2 = XEXP (src, 2);

	      /* If either is undefined, then the result is undefined.  */
	      if ((GET_CODE (src0) == REG
		   && values[REGNO (src0)].lattice_val == UNDEFINED)
		  || (GET_CODE (src1) == REG
		      && values[REGNO (src1)].lattice_val == UNDEFINED)
		  || (GET_CODE (src2) == REG
		      && values[REGNO (src2)].lattice_val == UNDEFINED))
		{
		  defs_to_undefined (insn);
		  break;
		}

	      /* Simplify source operands to whatever known values they
		 may have.  */
	      if (GET_CODE (src0) == REG
		  && values[REGNO (src0)].lattice_val == CONSTANT)
		src0 = values[REGNO (src0)].const_value;

	      if (GET_CODE (src1) == REG
		  && values[REGNO (src1)].lattice_val == CONSTANT)
		src1 = values[REGNO (src1)].const_value;

	      if (GET_CODE (src2) == REG
		  && values[REGNO (src2)].lattice_val == CONSTANT)
		src2 = values[REGNO (src2)].const_value;

	      /* See if the simplifier can determine if this operation
		 computes a constant value.  */
	      simplified = simplify_ternary_operation (GET_CODE (src),
						       GET_MODE (src),
						       GET_MODE (src),
						       src0, src1, src2);
	      break;
	    }

	  default:
	    defs_to_varying (insn);
	}

      if (simplified && GET_CODE (simplified) == CONST_INT)
	{
	  if (values[REGNO (dest)].lattice_val != CONSTANT
	      || values[REGNO (dest)].const_value != simplified)
	    SET_BIT (ssa_edges, REGNO (dest));

	  values[REGNO (dest)].lattice_val = CONSTANT;
	  values[REGNO (dest)].const_value = simplified;
	}
      else
	defs_to_varying (insn);
    }
}

/* Iterate over the FLOW_EDGES work list.  Simulate the target block
   for each edge.  */
static void
examine_flow_edges ()
{
  while (flow_edges != NULL)
    {
      basic_block succ_block;
      rtx curr_phi_node;

      /* Pull the next block to simulate off the worklist.  */
      succ_block = flow_edges->dest;
      flow_edges = edge_info[EIE (flow_edges->src, flow_edges->dest)];

      /* There is nothing to do for the exit block.  */
      if (succ_block == EXIT_BLOCK_PTR)
	continue;

      /* Always simulate PHI nodes, even if we have simulated this block
	 before.  Note that all PHI nodes are consecutive within a block.  */
      for (curr_phi_node = first_insn_after_basic_block_note (succ_block);
	   PHI_NODE_P (curr_phi_node);
	   curr_phi_node = NEXT_INSN (curr_phi_node))
	visit_phi_node (curr_phi_node, succ_block);

      /* If this is the first time we've simulated this block, then we
	 must simulate each of its insns.  */
      if (!TEST_BIT (executable_blocks, succ_block->index))
	{
	  rtx currinsn;
	  edge succ_edge = succ_block->succ;

	  /* Note that we have simulated this block.  */
	  SET_BIT (executable_blocks, succ_block->index);

	  /* Simulate each insn within the block.  */
	  currinsn = succ_block->head;
	  while (currinsn != succ_block->end)
	    {
	      if (INSN_P (currinsn))
		visit_expression (currinsn, succ_block);

	      currinsn = NEXT_INSN (currinsn);
	    }

	  /* Don't forget the last insn in the block.  */
	  if (INSN_P (currinsn))
	    visit_expression (currinsn, succ_block);

	  /* If we haven't looked at the next block, and it has a
	     single successor, add it onto the worklist.  This is because
	     if we only have one successor, we know it gets executed,
	     so we don't have to wait for cprop to tell us.  */
	  if (succ_edge != NULL
	      && succ_edge->succ_next == NULL
	      && !TEST_BIT (executable_edges,
			    EIE (succ_edge->src, succ_edge->dest)))
	    {
	      SET_BIT (executable_edges,
		       EIE (succ_edge->src, succ_edge->dest));
	      edge_info[EIE (succ_edge->src, succ_edge->dest)] = flow_edges;
	      flow_edges = succ_edge;
	    }
	}
    }
}

/* Follow the def-use chains for each definition on the worklist and
   simulate the uses of the definition.  */

static void
follow_def_use_chains ()
{
  /* Iterate over all the entries on the SSA_EDGES worklist.  */
  while (sbitmap_first_set_bit (ssa_edges) >= 0)
    {
      int member;
      struct df_link *curruse;

      /* Pick an entry off the worklist (it does not matter which
	 entry we pick).  */
      member = sbitmap_first_set_bit (ssa_edges);
      RESET_BIT (ssa_edges, member);

      /* Iterate through all the uses of this entry.  */
      for (curruse = df_analyzer->regs[member].uses; curruse;
	   curruse = curruse->next)
	{
	  rtx useinsn;

	  useinsn = DF_REF_INSN (curruse->ref);
	  if (PHI_NODE_P (useinsn))
	    {
	      if (TEST_BIT (executable_blocks, BLOCK_NUM (useinsn)))
		visit_phi_node (useinsn, BLOCK_FOR_INSN (useinsn));
	    }
	  else
	    {
	      if (TEST_BIT (executable_blocks, BLOCK_NUM (useinsn)))
		visit_expression (useinsn, BLOCK_FOR_INSN (useinsn));
	    }
	}
    }
}

/* Examine each edge to see if we were able to prove any were
   not executable.

   If an edge is not executable, then we can remove its alternative
   in PHI nodes as the destination of the edge, we can simplify the
   conditional branch at the source of the edge, and we can remove
   the edge from the CFG.  Note we do not delete unreachable blocks
   yet as the DF analyzer can not deal with that yet.  */
static void
optimize_unexecutable_edges (edges, executable_edges)
     struct edge_list *edges;
     sbitmap executable_edges;
{
  int i;
  basic_block bb;

  for (i = 0; i < NUM_EDGES (edges); i++)
    {
      if (!TEST_BIT (executable_edges, i))
	{
	  edge edge = INDEX_EDGE (edges, i);

	  if (edge->flags & EDGE_ABNORMAL)
	    continue;

	  /* We found an edge that is not executable.  First simplify
	     the PHI nodes in the target block.  */
	  if (edge->dest != EXIT_BLOCK_PTR)
	    {
	      rtx insn = first_insn_after_basic_block_note (edge->dest);

	      while (PHI_NODE_P (insn))
		{
		  remove_phi_alternative (PATTERN (insn), edge->src);
		  if (rtl_dump_file)
		    fprintf (rtl_dump_file,
			     "Removing alternative for bb %d of phi %d\n",
			     edge->src->index, SSA_NAME (PATTERN (insn)));
		  insn = NEXT_INSN (insn);
		}
	    }
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "Removing unexecutable edge from %d to %d\n",
		     edge->src->index, edge->dest->index);
	  /* Since the edge was not executable, remove it from the CFG.  */
	  remove_edge (edge);
	}
    }

  /* We have removed all the unexecutable edges from the CFG.  Fix up
     the conditional jumps at the end of any affected block.

     We have three cases to deal with:

       a. Both outgoing edges are not executable.  This happens if the
	  source block is not reachable.  We will deal with this by
	  deleting all the insns in the block later.

       b. The fall-thru edge is not executable.  In this case we
	  change the conditional jump into an unconditional jump and
	  add a BARRIER after the unconditional jump.  Note that since
	  we are working on generic RTL we can change the jump in-place
	  instead of dealing with the headache of reemitting the jump.

       c. The branch taken edge is not executable.  In this case
	  we turn the jump into (set (pc) (pc)) which is a nop-jump
          and we will remove the unrecognizable insn later.

     In cases B & C we are removing uses of registers, so make sure
     to note those changes for the DF analyzer.  */

  FOR_EACH_BB (bb)
    {
      rtx insn = bb->end;
      edge edge = bb->succ;

      /* If we have no predecessors, then this block is unreachable and
	 will be cleaned up when we remove unreachable blocks.  */
      if (bb->pred == NULL || GET_CODE (insn) != JUMP_INSN)
	continue;

      /* If this block ends in a conditional jump, but only has one
	 successor, then the jump needs adjustment.  */
      if (condjump_p (insn) && ! simplejump_p (insn)
	  && bb->succ && bb->succ->succ_next == NULL)
	{
	  /* If the fallthru edge is the executable edge, then turn
	     this jump into a nop jump, otherwise make it an unconditinoal
	     jump to its target.  */
	  if (edge->flags & EDGE_FALLTHRU)
	    {
	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	    }
	  else
	    {
	      SET_SRC (PATTERN (insn)) = gen_rtx_LABEL_REF (Pmode,
							    JUMP_LABEL (insn));
	      emit_barrier_after (insn);
	      INSN_CODE (insn) = -1;
	    }

	  /* Inform the DF analyzer that this insn changed.  */
	  df_insn_modify (df_analyzer, BLOCK_FOR_INSN (insn), insn);
	}
    }
}

/* Perform substitution of known values for pseudo registers.

   ??? Note we do not do simplifications or constant folding here, it
   is unlikely that any significant simplifications can be done here
   anyway.  Consider that if the simplification would result in an
   expression that produces a constant value that the value would
   have been discovered and recorded already.

   We perform two transformations.  First, we initialize pseudos to their
   known constant values at their definition point.  Second, we try to
   replace uses with the known constant value.  */

static void
ssa_ccp_substitute_constants ()
{
  unsigned int i;

  for (i = FIRST_PSEUDO_REGISTER; i < VARRAY_SIZE (ssa_definition); i++)
    {
      if (values[i].lattice_val == CONSTANT)
	{
	  rtx def = VARRAY_RTX (ssa_definition, i);
	  rtx set = single_set (def);
	  struct df_link *curruse;

	  if (! set)
	    continue;

	  /* Do not try to simplify PHI nodes down to a constant load.
	     That will be done later as we translate out of SSA.  Also,
	     doing that here could violate the rule that all PHI nodes
	     are consecutive at the start of the basic block.

	     Don't do anything to nodes that were already sets to
	     constants.	 */
	  if (! PHI_NODE_P (def)
	      && ! ((GET_CODE (def) == INSN
		     && GET_CODE (SET_SRC (set)) == CONST_INT)))
	    {
	      if (rtl_dump_file)
		fprintf (rtl_dump_file,
			 "Register %d is now set to a constant\n",
			 SSA_NAME (PATTERN (def)));
	      SET_SRC (set) = values[i].const_value;
	      INSN_CODE (def) = -1;
	      df_insn_modify (df_analyzer, BLOCK_FOR_INSN (def), def);
	    }

	  /* Iterate through all the uses of this entry and try replacements
	     there too.  Note it is not particularly profitable to try
	     and fold/simplify expressions here as most of the common
	     cases were handled above.  */
	  for (curruse = df_analyzer->regs[i].uses;
	       curruse;
	       curruse = curruse->next)
	    {
	      rtx useinsn;

	      useinsn = DF_REF_INSN (curruse->ref);

	      if (!INSN_DELETED_P (useinsn)
		  && ! (GET_CODE (useinsn) == NOTE
			&& NOTE_LINE_NUMBER (useinsn) == NOTE_INSN_DELETED)
		  && (GET_CODE (useinsn) == INSN
		      || GET_CODE (useinsn) == JUMP_INSN))
		{

		  if (validate_replace_src (regno_reg_rtx [i],
					values[i].const_value,
					    useinsn))
		    {
		      if (rtl_dump_file)
			fprintf (rtl_dump_file,
				 "Register %d in insn %d replaced with constant\n",
				 i, INSN_UID (useinsn));
		      INSN_CODE (useinsn) = -1;
		      df_insn_modify (df_analyzer,
				      BLOCK_FOR_INSN (useinsn),
				      useinsn);
		    }

		}
	    }
	}
    }
}

/* Now find all unreachable basic blocks.  All the insns in those
   blocks are unreachable, so delete them and mark any necessary
   updates for the DF analyzer.  */

static void
ssa_ccp_df_delete_unreachable_insns ()
{
  basic_block b;

  /* Use the CFG to find all the reachable blocks.  */
  find_unreachable_blocks ();

  /* Now we know what blocks are not reachable.  Mark all the insns
     in those blocks as deleted for the DF analyzer.   We'll let the
     normal flow code actually remove the unreachable blocks.  */
  FOR_EACH_BB_REVERSE (b)
    {
      if (!(b->flags & BB_REACHABLE))
	{
	  rtx start = b->head;
	  rtx end = b->end;
	  rtx tmp;

	  /* Include any jump table following the basic block.  */
	  end = b->end;
	  if (GET_CODE (end) == JUMP_INSN
	      && (tmp = JUMP_LABEL (end)) != NULL_RTX
	      && (tmp = NEXT_INSN (tmp)) != NULL_RTX
	      && GET_CODE (tmp) == JUMP_INSN
	      && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	          || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	    end = tmp;

	  while (1)
	    {
	      rtx next = NEXT_INSN (start);

	      if (GET_CODE (start) == INSN
		  || GET_CODE (start) == CALL_INSN
		  || GET_CODE (start) == JUMP_INSN)
		df_insn_delete (df_analyzer, BLOCK_FOR_INSN (start), start);

	      if (start == end)
		break;
	      start = next;
	    }
	}
    }
}


/* Main entry point for SSA Conditional Constant Propagation.

   Long term it should accept as input the specific flow graph to
   operate on so that it can be called for sub-graphs.  */

void
ssa_const_prop ()
{
  unsigned int i;
  edge curredge;

  /* We need alias analysis (for what?) */
  init_alias_analysis ();

  df_analyzer = df_init ();
  df_analyse (df_analyzer, 0,
	      DF_RD_CHAIN | DF_RU_CHAIN | DF_REG_INFO | DF_HARD_REGS);

  /* Perform a quick and dirty dead code elimination pass.  This is not
     as aggressive as it could be, but it's good enough to clean up a
     lot of unwanted junk and it is fast.  */
  ssa_fast_dce (df_analyzer);

  /* Build an edge list from the CFG.  */
  edges = create_edge_list ();

  /* Initialize the values array with everything as undefined.  */
  values = (value *) xmalloc (VARRAY_SIZE (ssa_definition) * sizeof (value));
  for (i = 0; i < VARRAY_SIZE (ssa_definition); i++)
    {
      if (i < FIRST_PSEUDO_REGISTER)
	values[i].lattice_val = VARYING;
      else
	values[i].lattice_val = UNDEFINED;
      values[i].const_value = NULL;
    }

  ssa_edges = sbitmap_alloc (VARRAY_SIZE (ssa_definition));
  sbitmap_zero (ssa_edges);

  executable_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (executable_blocks);

  executable_edges = sbitmap_alloc (NUM_EDGES (edges));
  sbitmap_zero (executable_edges);

  edge_info = (edge *) xmalloc (NUM_EDGES (edges) * sizeof (edge));
  flow_edges = ENTRY_BLOCK_PTR->succ;

  /* Add the successors of the entry block to the edge worklist.  That
     is enough of a seed to get SSA-CCP started.  */
  for (curredge = ENTRY_BLOCK_PTR->succ; curredge;
       curredge = curredge->succ_next)
    {
      int index = EIE (curredge->src, curredge->dest);
      SET_BIT (executable_edges, index);
      edge_info[index] = curredge->succ_next;
    }

  /* Iterate until until the worklists are empty.  */
  do
    {
      examine_flow_edges ();
      follow_def_use_chains ();
    }
  while (flow_edges != NULL);

  /* Now perform substitutions based on the known constant values.  */
  ssa_ccp_substitute_constants ();

  /* Remove unexecutable edges from the CFG and make appropriate
     adjustments to PHI nodes.  */
  optimize_unexecutable_edges (edges, executable_edges);

  /* Now remove all unreachable insns and update the DF information.
     as appropriate.  */
  ssa_ccp_df_delete_unreachable_insns ();

#if 0
  /* The DF analyzer expects the number of blocks to remain constant,
     so we can't remove unreachable blocks.

     Code the DF analyzer calls expects there to be no unreachable
     blocks in the CFG.  So we can't leave unreachable blocks in the
     CFG.

     So, there is no way to do an incremental update of the DF data
     at this point.  */
  df_analyse (df_analyzer, 0,
	      DF_RD_CHAIN | DF_RU_CHAIN | DF_REG_INFO | DF_HARD_REGS);
#endif

  /* Clean up any dead code exposed by SSA-CCP, do this after updating
     the dataflow information!  */
  ssa_fast_dce (df_analyzer);

  free (values);
  values = NULL;

  free (edge_info);
  edge_info = NULL;

  sbitmap_free (executable_blocks);
  executable_blocks = NULL;

  sbitmap_free (ssa_edges);
  ssa_edges = NULL;

  free_edge_list (edges);
  edges = NULL;

  sbitmap_free (executable_edges);
  executable_edges = NULL;

  df_finish (df_analyzer);
  end_alias_analysis ();
}

static int
mark_references (current_rtx, data)
     rtx *current_rtx;
     void *data;
{
  rtx x = *current_rtx;
  sbitmap worklist = (sbitmap) data;

  if (x == NULL_RTX)
    return 0;

  if (GET_CODE (x) == SET)
    {
      rtx dest = SET_DEST (x);

      if (GET_CODE (dest) == STRICT_LOW_PART
	  || GET_CODE (dest) == SUBREG
	  || GET_CODE (dest) == SIGN_EXTRACT
	  || GET_CODE (dest) == ZERO_EXTRACT)
	{
	  rtx reg;

	  reg = dest;

	  while (GET_CODE (reg) == STRICT_LOW_PART
		 || GET_CODE (reg) == SUBREG
		 || GET_CODE (reg) == SIGN_EXTRACT
		 || GET_CODE (reg) == ZERO_EXTRACT)
	    reg = XEXP (reg, 0);

	  if (GET_CODE (reg) == REG)
	    SET_BIT (worklist, REGNO (reg));
	}

      if (GET_CODE (dest) == REG)
	{
	  for_each_rtx (&SET_SRC (x), mark_references, data);
	  return -1;
	}

      return 0;
    }
  else if (GET_CODE (x) == REG)
    {
      SET_BIT (worklist, REGNO (x));
      return -1;
    }
  else if (GET_CODE (x) == CLOBBER)
    return -1;
  else
    return 0;
}

static void
ssa_fast_dce (df)
     struct df *df;
{
  sbitmap worklist = sbitmap_alloc (VARRAY_SIZE (ssa_definition));
  sbitmap_ones (worklist);

  /* Iterate on the worklist until there's no definitions left to
     examine.  */
  while (sbitmap_first_set_bit (worklist) >= 0)
    {
      struct df_link *curruse;
      int reg, found_use;

      /* Remove an item from the worklist.  */
      reg = sbitmap_first_set_bit (worklist);
      RESET_BIT (worklist, reg);

      /* We never consider deleting assignments to hard regs or things
	 which do not have SSA definitions, or things we have already
	 deleted, or things with unusual side effects.  */
      if (reg < FIRST_PSEUDO_REGISTER
	  || ! VARRAY_RTX (ssa_definition, reg)
	  || INSN_DELETED_P (VARRAY_RTX (ssa_definition, reg))
	  || (GET_CODE (VARRAY_RTX (ssa_definition, reg)) == NOTE
	      && (NOTE_LINE_NUMBER (VARRAY_RTX (ssa_definition, reg))
		  == NOTE_INSN_DELETED))
	  || side_effects_p (PATTERN (VARRAY_RTX (ssa_definition, reg))))
	continue;

      /* Iterate over the uses of this register.  If we can not find
	 any uses that have not been deleted, then the definition of
	 this register is dead.  */
      found_use = 0;
      for (curruse = df->regs[reg].uses; curruse; curruse = curruse->next)
	{
	  if (curruse->ref
	      && DF_REF_INSN (curruse->ref)
	      && ! INSN_DELETED_P (DF_REF_INSN (curruse->ref))
	      && ! (GET_CODE (DF_REF_INSN (curruse->ref)) == NOTE
		    && (NOTE_LINE_NUMBER (DF_REF_INSN (curruse->ref))
			== NOTE_INSN_DELETED))
	      && DF_REF_INSN (curruse->ref) != VARRAY_RTX (ssa_definition, reg))
	    {
	      found_use = 1;
	      break;
	    }
	}

      /* If we did not find a use of this register, then the definition
	 of this register is dead.  */

      if (! found_use)
	{
	  rtx def = VARRAY_RTX (ssa_definition, reg);

	  /* Add all registers referenced by INSN to the work
	     list.  */
	  for_each_rtx (&PATTERN (def), mark_references, worklist);

	  /* Inform the analyzer that this insn is going to be
	     deleted.  */
	  df_insn_delete (df, BLOCK_FOR_INSN (def), def);

	  VARRAY_RTX (ssa_definition, reg) = NULL;
	}
    }

  sbitmap_free (worklist);

  /* Update the use-def chains in the df_analyzer as needed.  */
  df_analyse (df_analyzer, 0,
	      DF_RD_CHAIN | DF_RU_CHAIN | DF_REG_INFO | DF_HARD_REGS);
}
