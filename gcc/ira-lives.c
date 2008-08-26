/* IRA processing allocno lives to build allocno live ranges.
   Copyright (C) 2006, 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "regs.h"
#include "rtl.h"
#include "tm_p.h"
#include "target.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "recog.h"
#include "toplev.h"
#include "params.h"
#include "df.h"
#include "sparseset.h"
#include "ira-int.h"

/* The code in this file is similar to one in global but the code
   works on the allocno basis and creates live ranges instead of
   pseudo-register conflicts.  */

/* Program points are enumerated by numbers from range
   0..IRA_MAX_POINT-1.  There are approximately two times more program
   points than insns.  Program points are places in the program where
   liveness info can be changed.  In most general case (there are more
   complicated cases too) some program points correspond to places
   where input operand dies and other ones correspond to places where
   output operands are born.  */
int ira_max_point;

/* Arrays of size IRA_MAX_POINT mapping a program point to the allocno
   live ranges with given start/finish point.  */
allocno_live_range_t *ira_start_point_ranges, *ira_finish_point_ranges;

/* Number of the current program point.  */
static int curr_point;

/* Point where register pressure excess started or -1 if there is no
   register pressure excess.  Excess pressure for a register class at
   some point means that there are more allocnos of given register
   class living at the point than number of hard-registers of the
   class available for the allocation.  It is defined only for cover
   classes.  */
static int high_pressure_start_point[N_REG_CLASSES];

/* Allocnos live at current point in the scan.  */
static sparseset allocnos_live;

/* Set of hard regs (except eliminable ones) currently live.  */
static HARD_REG_SET hard_regs_live;

/* The loop tree node corresponding to the current basic block.  */
static ira_loop_tree_node_t curr_bb_node;

/* The function processing birth of register REGNO.  It updates living
   hard regs and conflict hard regs for living allocnos or starts a
   new live range for the allocno corresponding to REGNO if it is
   necessary.  */
static void
make_regno_born (int regno)
{
  unsigned int i;
  ira_allocno_t a;
  allocno_live_range_t p;

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      SET_HARD_REG_BIT (hard_regs_live, regno);
      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
        {
	  SET_HARD_REG_BIT (ALLOCNO_CONFLICT_HARD_REGS (ira_allocnos[i]),
			    regno);
	  SET_HARD_REG_BIT (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (ira_allocnos[i]),
			    regno);
	}
      return;
    }
  a = ira_curr_regno_allocno_map[regno];
  if (a == NULL)
    return;
  if ((p = ALLOCNO_LIVE_RANGES (a)) == NULL
      || (p->finish != curr_point && p->finish + 1 != curr_point))
    ALLOCNO_LIVE_RANGES (a)
      = ira_create_allocno_live_range (a, curr_point, -1,
				       ALLOCNO_LIVE_RANGES (a));
}

/* Update ALLOCNO_EXCESS_PRESSURE_POINTS_NUM for allocno A.  */
static void
update_allocno_pressure_excess_length (ira_allocno_t a)
{
  int start;
  enum reg_class cover_class;
  allocno_live_range_t p;

  cover_class = ALLOCNO_COVER_CLASS (a);
  if (high_pressure_start_point[cover_class] < 0)
    return;
  p = ALLOCNO_LIVE_RANGES (a);
  ira_assert (p != NULL);
  start = (high_pressure_start_point[cover_class] > p->start
	   ? high_pressure_start_point[cover_class] : p->start);
  ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a) += curr_point - start + 1;
}

/* Process the death of register REGNO.  This updates hard_regs_live
   or finishes the current live range for the allocno corresponding to
   REGNO.  */
static void
make_regno_dead (int regno)
{
  ira_allocno_t a;
  allocno_live_range_t p;

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      CLEAR_HARD_REG_BIT (hard_regs_live, regno);
      return;
    }
  a = ira_curr_regno_allocno_map[regno];
  if (a == NULL)
    return;
  p = ALLOCNO_LIVE_RANGES (a);
  ira_assert (p != NULL);
  p->finish = curr_point;
  update_allocno_pressure_excess_length (a);
}

/* Process the birth and, right after then, death of register
   REGNO.  */
static void
make_regno_born_and_dead (int regno)
{
  make_regno_born (regno);
  make_regno_dead (regno);
}

/* The current register pressures for each cover class for the current
   basic block.  */
static int curr_reg_pressure[N_REG_CLASSES];

/* Mark allocno A as currently living and update current register
   pressure, maximal register pressure for the current BB, start point
   of the register pressure excess, and conflicting hard registers of
   A.  */
static void
set_allocno_live (ira_allocno_t a)
{
  int nregs;
  enum reg_class cover_class;

  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
    return;
  sparseset_set_bit (allocnos_live, ALLOCNO_NUM (a));
  IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a), hard_regs_live);
  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a), hard_regs_live);
  cover_class = ALLOCNO_COVER_CLASS (a);
  nregs = ira_reg_class_nregs[cover_class][ALLOCNO_MODE (a)];
  curr_reg_pressure[cover_class] += nregs;
  if (high_pressure_start_point[cover_class] < 0
      && (curr_reg_pressure[cover_class]
	  > ira_available_class_regs[cover_class]))
    high_pressure_start_point[cover_class] = curr_point;
  if (curr_bb_node->reg_pressure[cover_class]
      < curr_reg_pressure[cover_class])
    curr_bb_node->reg_pressure[cover_class] = curr_reg_pressure[cover_class];
}

/* Mark allocno A as currently not living and update current register
   pressure, start point of the register pressure excess, and register
   pressure excess length for living allocnos.  */
static void
clear_allocno_live (ira_allocno_t a)
{
  unsigned int i;
  enum reg_class cover_class;

  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
    {
      cover_class = ALLOCNO_COVER_CLASS (a);
      curr_reg_pressure[cover_class]
	-= ira_reg_class_nregs[cover_class][ALLOCNO_MODE (a)];
      ira_assert (curr_reg_pressure[cover_class] >= 0);
      if (high_pressure_start_point[cover_class] >= 0
	  && (curr_reg_pressure[cover_class]
	      <= ira_available_class_regs[cover_class]))
	{
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	    {
	      update_allocno_pressure_excess_length (ira_allocnos[i]);
	    }
	  high_pressure_start_point[cover_class] = -1;
	}
    }
  sparseset_clear_bit (allocnos_live, ALLOCNO_NUM (a));
}

/* Record all regs that are set in any one insn.  Communication from
   mark_reg_{store,clobber}.  */
static VEC(rtx, heap) *regs_set;

/* Handle the case where REG is set by the insn being scanned, during
   the scan to build live ranges and calculate reg pressure info.
   Store a 1 in hard_regs_live or allocnos_live for this register or
   the corresponding allocno, record how many consecutive hardware
   registers it actually needs.

   Note that even if REG does not remain alive after this insn, we
   must mark it here as live, to ensure a conflict between REG and any
   other reg allocnos set in this insn that really do live.  This is
   because those other allocnos could be considered after this.

   REG might actually be something other than a register; if so, we do
   nothing.

   SETTER is 0 if this register was modified by an auto-increment
   (i.e., a REG_INC note was found for it).  */
static void
mark_reg_store (rtx reg, const_rtx setter ATTRIBUTE_UNUSED,
		void *data ATTRIBUTE_UNUSED)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg))
    return;

  VEC_safe_push (rtx, heap, regs_set, reg);

  regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      ira_allocno_t a = ira_curr_regno_allocno_map[regno];

      if (a != NULL)
	{
	  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
	    return;
	  set_allocno_live (a);
	}
      make_regno_born (regno);
    }
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class cover_class;

      while (regno < last)
	{
	  if (! TEST_HARD_REG_BIT (hard_regs_live, regno)
	      && ! TEST_HARD_REG_BIT (eliminable_regset, regno))
	    {
	      cover_class = ira_class_translate[REGNO_REG_CLASS (regno)];
	      if (cover_class != NO_REGS)
		{
		  curr_reg_pressure[cover_class]++;
		  if (high_pressure_start_point[cover_class] < 0
		      && (curr_reg_pressure[cover_class]
			  > ira_available_class_regs[cover_class]))
		    high_pressure_start_point[cover_class] = curr_point;
		}
	      make_regno_born (regno);
	      if (cover_class != NO_REGS
		  && (curr_bb_node->reg_pressure[cover_class]
		      < curr_reg_pressure[cover_class]))
		curr_bb_node->reg_pressure[cover_class]
		  = curr_reg_pressure[cover_class];
	    }
	  regno++;
	}
    }
}

/* Like mark_reg_store except notice just CLOBBERs; ignore SETs.  */
static void
mark_reg_clobber (rtx reg, const_rtx setter, void *data)
{
  if (GET_CODE (setter) == CLOBBER)
    mark_reg_store (reg, setter, data);
}

/* Record that hard register REG (if it is a hard register) has
   conflicts with all the allocno currently live or the corresponding
   allocno lives at just the current program point.  Do not mark REG
   (or the allocno) itself as live.  */
static void
mark_reg_conflicts (rtx reg)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg))
    return;

  regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    make_regno_born_and_dead (regno);
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];

      while (regno < last)
	{
	  make_regno_born_and_dead (regno);
	  regno++;
	}
    }
}

/* Mark REG (or the corresponding allocno) as being dead (following
   the insn being scanned now).  Store a 0 in hard_regs_live or
   allocnos_live for the register.  */
static void
mark_reg_death (rtx reg)
{
  unsigned int i;
  int regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      ira_allocno_t a = ira_curr_regno_allocno_map[regno];

      if (a != NULL)
	{
	  if (! sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
	    return;
	  clear_allocno_live (a);
	}
      make_regno_dead (regno);
    }
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class cover_class;

      while (regno < last)
	{
	  if (TEST_HARD_REG_BIT (hard_regs_live, regno))
	    {
	      cover_class = ira_class_translate[REGNO_REG_CLASS (regno)];
	      if (cover_class != NO_REGS)
		{
		  curr_reg_pressure[cover_class]--;
		  if (high_pressure_start_point[cover_class] >= 0
		      && (curr_reg_pressure[cover_class]
			  <= ira_available_class_regs[cover_class]))
		    {
		      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
			{
			  update_allocno_pressure_excess_length
			    (ira_allocnos[i]);
			}
		      high_pressure_start_point[cover_class] = -1;
		    }
		  ira_assert (curr_reg_pressure[cover_class] >= 0);
		}
	      make_regno_dead (regno);
	    }
	  regno++;
	}
    }
}

/* Checks that CONSTRAINTS permits to use only one hard register.  If
   it is so, the function returns the class of the hard register.
   Otherwise it returns NO_REGS.  */
static enum reg_class
single_reg_class (const char *constraints, rtx op, rtx equiv_const)
{
  int ignore_p;
  enum reg_class cl, next_cl;
  int c;

  cl = NO_REGS;
  for (ignore_p = false;
       (c = *constraints);
       constraints += CONSTRAINT_LEN (c, constraints))
    if (c == '#')
      ignore_p = true;
    else if (c == ',')
      ignore_p = false;
    else if (! ignore_p)
      switch (c)
	{
	case ' ':
	case '\t':
	case '=':
	case '+':
	case '*':
	case '&':
	case '%':
	case '!':
	case '?':
	  break;
	case 'i':
	  if (CONSTANT_P (op)
	      || (equiv_const != NULL_RTX && CONSTANT_P (equiv_const)))
	    return NO_REGS;
	  break;

	case 'n':
	  if (GET_CODE (op) == CONST_INT
	      || (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == VOIDmode)
	      || (equiv_const != NULL_RTX
		  && (GET_CODE (equiv_const) == CONST_INT
		      || (GET_CODE (equiv_const) == CONST_DOUBLE
			  && GET_MODE (equiv_const) == VOIDmode))))
	    return NO_REGS;
	  break;
	  
	case 's':
	  if ((CONSTANT_P (op) && GET_CODE (op) != CONST_INT
	       && (GET_CODE (op) != CONST_DOUBLE || GET_MODE (op) != VOIDmode))
	      || (equiv_const != NULL_RTX
		  && CONSTANT_P (equiv_const)
		  && GET_CODE (equiv_const) != CONST_INT
		  && (GET_CODE (equiv_const) != CONST_DOUBLE
		      || GET_MODE (equiv_const) != VOIDmode)))
	    return NO_REGS;
	  break;
	  
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	  if ((GET_CODE (op) == CONST_INT
	       && CONST_OK_FOR_CONSTRAINT_P (INTVAL (op), c, constraints))
	      || (equiv_const != NULL_RTX
		  && GET_CODE (equiv_const) == CONST_INT
		  && CONST_OK_FOR_CONSTRAINT_P (INTVAL (equiv_const),
						c, constraints)))
	    return NO_REGS;
	  break;
	  
	case 'E':
	case 'F':
	  if (GET_CODE (op) == CONST_DOUBLE
	      || (GET_CODE (op) == CONST_VECTOR
		  && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT)
	      || (equiv_const != NULL_RTX
		  && (GET_CODE (equiv_const) == CONST_DOUBLE
		      || (GET_CODE (equiv_const) == CONST_VECTOR
			  && (GET_MODE_CLASS (GET_MODE (equiv_const))
			      == MODE_VECTOR_FLOAT)))))
	    return NO_REGS;
	  break;
	  
	case 'G':
	case 'H':
	  if ((GET_CODE (op) == CONST_DOUBLE
	       && CONST_DOUBLE_OK_FOR_CONSTRAINT_P (op, c, constraints))
	      || (equiv_const != NULL_RTX
		  && GET_CODE (equiv_const) == CONST_DOUBLE
		  && CONST_DOUBLE_OK_FOR_CONSTRAINT_P (equiv_const,
						       c, constraints)))
	    return NO_REGS;
	  /* ??? what about memory */
	case 'r':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'h': case 'j': case 'k': case 'l':
	case 'q': case 't': case 'u':
	case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D':
	case 'Q': case 'R': case 'S': case 'T': case 'U':
	case 'W': case 'Y': case 'Z':
	  next_cl = (c == 'r'
		     ? GENERAL_REGS
		     : REG_CLASS_FROM_CONSTRAINT (c, constraints));
	  if ((cl != NO_REGS && next_cl != cl)
	      || ira_available_class_regs[next_cl] > 1)
	    return NO_REGS;
	  cl = next_cl;
	  break;
	  
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  next_cl
	    = single_reg_class (recog_data.constraints[c - '0'],
				recog_data.operand[c - '0'], NULL_RTX);
	  if ((cl != NO_REGS && next_cl != cl) || next_cl == NO_REGS
	      || ira_available_class_regs[next_cl] > 1)
	    return NO_REGS;
	  cl = next_cl;
	  break;
	  
	default:
	  return NO_REGS;
	}
  return cl;
}

/* The function checks that operand OP_NUM of the current insn can use
   only one hard register.  If it is so, the function returns the
   class of the hard register.  Otherwise it returns NO_REGS.  */
static enum reg_class
single_reg_operand_class (int op_num)
{
  if (op_num < 0 || recog_data.n_alternatives == 0)
    return NO_REGS;
  return single_reg_class (recog_data.constraints[op_num],
			   recog_data.operand[op_num], NULL_RTX);
}

/* Processes input operands, if IN_P, or output operands otherwise of
   the current insn with FREQ to find allocno which can use only one
   hard register and makes other currently living allocnos conflicting
   with the hard register.  */
static void
process_single_reg_class_operands (bool in_p, int freq)
{
  int i, regno, cost;
  unsigned int px;
  enum reg_class cl, cover_class;
  rtx operand;
  ira_allocno_t operand_a, a;

  for (i = 0; i < recog_data.n_operands; i++)
    {
      operand = recog_data.operand[i];
      if (in_p && recog_data.operand_type[i] != OP_IN
	  && recog_data.operand_type[i] != OP_INOUT)
	continue;
      if (! in_p && recog_data.operand_type[i] != OP_OUT
	  && recog_data.operand_type[i] != OP_INOUT)
	continue;
      cl = single_reg_operand_class (i);
      if (cl == NO_REGS)
	continue;

      operand_a = NULL;

      if (GET_CODE (operand) == SUBREG)
	operand = SUBREG_REG (operand);
      
      if (REG_P (operand)
	  && (regno = REGNO (operand)) >= FIRST_PSEUDO_REGISTER)
	{
	  enum machine_mode mode;
	  enum reg_class cover_class;

	  operand_a = ira_curr_regno_allocno_map[regno];
	  mode = ALLOCNO_MODE (operand_a);
	  cover_class = ALLOCNO_COVER_CLASS (operand_a);
	  if (ira_class_subset_p[cl][cover_class]
	      && ira_class_hard_regs_num[cl] != 0
	      && (ira_class_hard_reg_index[cover_class]
		  [ira_class_hard_regs[cl][0]]) >= 0
	      && reg_class_size[cl] <= (unsigned) CLASS_MAX_NREGS (cl, mode))
	    {
	      /* ??? FREQ */
	      cost = freq * (in_p
			     ? ira_register_move_cost[mode][cover_class][cl]
			     : ira_register_move_cost[mode][cl][cover_class]);
	      ira_allocate_and_set_costs
		(&ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a), cover_class, 0);
	      ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a)
		[ira_class_hard_reg_index
		 [cover_class][ira_class_hard_regs[cl][0]]]
		-= cost;
	    }
	}

      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, px)
        {
	  a = ira_allocnos[px];
	  cover_class = ALLOCNO_COVER_CLASS (a);
	  if (a != operand_a)
	    {
	      /* We could increase costs of A instead of making it
		 conflicting with the hard register.  But it works worse
		 because it will be spilled in reload in anyway.  */
	      IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a),
				reg_class_contents[cl]);
	      IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a),
				reg_class_contents[cl]);
	    }
	}
    }
}

/* Process insns of the basic block given by its LOOP_TREE_NODE to
   update allocno live ranges, allocno hard register conflicts,
   intersected calls, and register pressure info for allocnos for the
   basic block for and regions containing the basic block.  */
static void
process_bb_node_lives (ira_loop_tree_node_t loop_tree_node)
{
  int i;
  unsigned int j;
  basic_block bb;
  rtx insn;
  edge e;
  edge_iterator ei;
  bitmap_iterator bi;
  bitmap reg_live_in;
  unsigned int px;

  bb = loop_tree_node->bb;
  if (bb != NULL)
    {
      for (i = 0; i < ira_reg_class_cover_size; i++)
	{
	  curr_reg_pressure[ira_reg_class_cover[i]] = 0;
	  high_pressure_start_point[ira_reg_class_cover[i]] = -1;
	}
      curr_bb_node = loop_tree_node;
      reg_live_in = DF_LR_IN (bb);
      sparseset_clear (allocnos_live);
      REG_SET_TO_HARD_REG_SET (hard_regs_live, reg_live_in);
      AND_COMPL_HARD_REG_SET (hard_regs_live, eliminable_regset);
      AND_COMPL_HARD_REG_SET (hard_regs_live, ira_no_alloc_regs);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (hard_regs_live, i))
	  {
	    enum reg_class cover_class;
	    
	    cover_class = REGNO_REG_CLASS (i);
	    if (cover_class == NO_REGS)
	      continue;
	    cover_class = ira_class_translate[cover_class];
	    curr_reg_pressure[cover_class]++;
	    if (curr_bb_node->reg_pressure[cover_class]
		< curr_reg_pressure[cover_class])
	      curr_bb_node->reg_pressure[cover_class]
		= curr_reg_pressure[cover_class];
	    ira_assert (curr_reg_pressure[cover_class]
			<= ira_available_class_regs[cover_class]);
	  }
      EXECUTE_IF_SET_IN_BITMAP (reg_live_in, FIRST_PSEUDO_REGISTER, j, bi)
	{
	  ira_allocno_t a = ira_curr_regno_allocno_map[j];
	  
	  if (a == NULL)
	    continue;
	  ira_assert (! sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)));
	  set_allocno_live (a);
	  make_regno_born (j);
	}
      
#ifdef EH_RETURN_DATA_REGNO
      if (bb_has_eh_pred (bb))
	{
	  for (j = 0; ; ++j)
	    {
	      unsigned int regno = EH_RETURN_DATA_REGNO (j);
	      
	      if (regno == INVALID_REGNUM)
		break;
	      make_regno_born_and_dead (regno);
	    }
	}
#endif
      
      /* Allocnos can't go in stack regs at the start of a basic block
	 that is reached by an abnormal edge. Likewise for call
	 clobbered regs, because caller-save, fixup_abnormal_edges and
	 possibly the table driven EH machinery are not quite ready to
	 handle such allocnos live across such edges.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->flags & EDGE_ABNORMAL)
	  break;
      
      if (e != NULL)
	{
#ifdef STACK_REGS
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, px)
	    {
	      ALLOCNO_NO_STACK_REG_P (ira_allocnos[px]) = true;
	      ALLOCNO_TOTAL_NO_STACK_REG_P (ira_allocnos[px]) = true;
	    }
	  for (px = FIRST_STACK_REG; px <= LAST_STACK_REG; px++)
	    make_regno_born_and_dead (px);
#endif
	  /* No need to record conflicts for call clobbered regs if we
	     have nonlocal labels around, as we don't ever try to
	     allocate such regs in this case.  */
	  if (!cfun->has_nonlocal_label)
	    for (px = 0; px < FIRST_PSEUDO_REGISTER; px++)
	      if (call_used_regs[px])
		make_regno_born_and_dead (px);
	}
  
      /* Scan the code of this basic block, noting which allocnos and
	 hard regs are born or die.  */
      FOR_BB_INSNS (bb, insn)
	{
	  rtx link;
	  int freq;
	  
	  if (! INSN_P (insn))
	    continue;
	  
	  freq = REG_FREQ_FROM_BB (BLOCK_FOR_INSN (insn));
	  if (freq == 0)
	    freq = 1;

	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "   Insn %u(l%d): point = %d\n",
		     INSN_UID (insn), loop_tree_node->parent->loop->num,
		     curr_point);

	  /* Check regs_set is an empty set.  */
	  gcc_assert (VEC_empty (rtx, regs_set));
      
	  /* Mark any allocnos clobbered by INSN as live, so they
	     conflict with the inputs.  */
	  note_stores (PATTERN (insn), mark_reg_clobber, NULL);
	  
	  extract_insn (insn);
	  process_single_reg_class_operands (true, freq);
	  
	  /* Mark any allocnos dead after INSN as dead now.  */
	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_DEAD)
	      mark_reg_death (XEXP (link, 0));
	  
	  curr_point++;

	  if (CALL_P (insn))
	    {
	      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	        {
		  ira_allocno_t a = ira_allocnos[i];
		  
		  ALLOCNO_CALL_FREQ (a) += freq;
		  ALLOCNO_CALLS_CROSSED_NUM (a)++;
		  /* Don't allocate allocnos that cross calls, if this
		     function receives a nonlocal goto.  */
		  if (cfun->has_nonlocal_label)
		    {
		      SET_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a));
		      SET_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
		    }
		}
	    }
	  
	  /* Mark any allocnos set in INSN as live.  Clobbers are
	     processed again, so they will conflict with the reg
	     allocnos that are set.  */
	  note_stores (PATTERN (insn), mark_reg_store, NULL);
	  
#ifdef AUTO_INC_DEC
	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_INC)
	      mark_reg_store (XEXP (link, 0), NULL_RTX, NULL);
#endif
	  
	  /* If INSN has multiple outputs, then any allocno that dies
	     here and is used inside of an output must conflict with
	     the other outputs.
	     
	     It is unsafe to use !single_set here since it will ignore
	     an unused output.  Just because an output is unused does
	     not mean the compiler can assume the side effect will not
	     occur.  Consider if ALLOCNO appears in the address of an
	     output and we reload the output.  If we allocate ALLOCNO
	     to the same hard register as an unused output we could
	     set the hard register before the output reload insn.  */
	  if (GET_CODE (PATTERN (insn)) == PARALLEL && multiple_sets (insn))
	    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	      if (REG_NOTE_KIND (link) == REG_DEAD)
		{
		  int i;
		  int used_in_output = 0;
		  rtx reg = XEXP (link, 0);
		  
		  for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		    {
		      rtx set = XVECEXP (PATTERN (insn), 0, i);
		      
		      if (GET_CODE (set) == SET
			  && ! REG_P (SET_DEST (set))
			  && ! rtx_equal_p (reg, SET_DEST (set))
			  && reg_overlap_mentioned_p (reg, SET_DEST (set)))
			used_in_output = 1;
		    }
		  if (used_in_output)
		    mark_reg_conflicts (reg);
		}
	  
	  process_single_reg_class_operands (false, freq);
	  
	  /* Mark any allocnos set in INSN and then never used.  */
	  while (! VEC_empty (rtx, regs_set))
	    {
	      rtx reg = VEC_pop (rtx, regs_set);
	      rtx note = find_regno_note (insn, REG_UNUSED, REGNO (reg));

	      if (note)
		mark_reg_death (XEXP (note, 0));
	    }
	  curr_point++;
	}
      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
       {
	 make_regno_dead (ALLOCNO_REGNO (ira_allocnos[i]));
       }

      curr_point++;

    }
  /* Propagate register pressure to upper loop tree nodes: */
  if (loop_tree_node != ira_loop_tree_root)
    for (i = 0; i < ira_reg_class_cover_size; i++)
      {
	enum reg_class cover_class;

	cover_class = ira_reg_class_cover[i];
	if (loop_tree_node->reg_pressure[cover_class]
	    > loop_tree_node->parent->reg_pressure[cover_class])
	  loop_tree_node->parent->reg_pressure[cover_class]
	    = loop_tree_node->reg_pressure[cover_class];
      }
}

/* Create and set up IRA_START_POINT_RANGES and
   IRA_FINISH_POINT_RANGES.  */
static void
create_start_finish_chains (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  allocno_live_range_t r;

  ira_start_point_ranges
    = (allocno_live_range_t *) ira_allocate (ira_max_point
					     * sizeof (allocno_live_range_t));
  memset (ira_start_point_ranges, 0,
	  ira_max_point * sizeof (allocno_live_range_t));
  ira_finish_point_ranges
    = (allocno_live_range_t *) ira_allocate (ira_max_point
					     * sizeof (allocno_live_range_t));
  memset (ira_finish_point_ranges, 0,
	  ira_max_point * sizeof (allocno_live_range_t));
  FOR_EACH_ALLOCNO (a, ai)
    {
      for (r = ALLOCNO_LIVE_RANGES (a); r != NULL; r = r->next)
	{
	  r->start_next = ira_start_point_ranges[r->start];
	  ira_start_point_ranges[r->start] = r;
	  r->finish_next = ira_finish_point_ranges[r->finish];
 	  ira_finish_point_ranges[r->finish] = r;
	}
    }
}

/* Rebuild IRA_START_POINT_RANGES and IRA_FINISH_POINT_RANGES after
   new live ranges and program points were added as a result if new
   insn generation.  */
void
ira_rebuild_start_finish_chains (void)
{
  ira_free (ira_finish_point_ranges);
  ira_free (ira_start_point_ranges);
  create_start_finish_chains ();
}

/* Print live ranges R to file F.  */
void
ira_print_live_range_list (FILE *f, allocno_live_range_t r)
{
  for (; r != NULL; r = r->next)
    fprintf (f, " [%d..%d]", r->start, r->finish);
  fprintf (f, "\n");
}

/* Print live ranges R to stderr.  */
void
ira_debug_live_range_list (allocno_live_range_t r)
{
  ira_print_live_range_list (stderr, r);
}

/* Print live ranges of allocno A to file F.  */
static void
print_allocno_live_ranges (FILE *f, ira_allocno_t a)
{
  fprintf (f, " a%d(r%d):", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
  ira_print_live_range_list (f, ALLOCNO_LIVE_RANGES (a));
}

/* Print live ranges of allocno A to stderr.  */
void
ira_debug_allocno_live_ranges (ira_allocno_t a)
{
  print_allocno_live_ranges (stderr, a);
}

/* Print live ranges of all allocnos to file F.  */
static void
print_live_ranges (FILE *f)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    print_allocno_live_ranges (f, a);
}

/* Print live ranges of all allocnos to stderr.  */
void
ira_debug_live_ranges (void)
{
  print_live_ranges (stderr);
}

/* The main entry function creates live ranges, set up
   CONFLICT_HARD_REGS and TOTAL_CONFLICT_HARD_REGS for allocnos, and
   calculate register pressure info.  */
void
ira_create_allocno_live_ranges (void)
{
  allocnos_live = sparseset_alloc (ira_allocnos_num);
  /* Make a vector that mark_reg_{store,clobber} will store in.  */
  if (!regs_set)
    regs_set = VEC_alloc (rtx, heap, 10);
  curr_point = 0;
  ira_traverse_loop_tree (true, ira_loop_tree_root, NULL,
			  process_bb_node_lives);
  ira_max_point = curr_point;
  create_start_finish_chains ();
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_live_ranges (ira_dump_file);
  /* Clean up.  */
  sparseset_free (allocnos_live);
}

/* Free arrays IRA_START_POINT_RANGES and IRA_FINISH_POINT_RANGES.  */
void
ira_finish_allocno_live_ranges (void)
{
  ira_free (ira_finish_point_ranges);
  ira_free (ira_start_point_ranges);
}
