/* IRA processing allocno lives to build allocno live ranges.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
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
#include "except.h"
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

/* The number of the last processed call.  */
static int last_call_num;
/* The number of last call at which given allocno was saved.  */
static int *allocno_saved_at_call;

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
  int start, i;
  enum reg_class cover_class, cl;
  allocno_live_range_t p;

  cover_class = ALLOCNO_COVER_CLASS (a);
  for (i = 0;
       (cl = ira_reg_class_super_classes[cover_class][i]) != LIM_REG_CLASSES;
       i++)
    {
      if (high_pressure_start_point[cl] < 0)
	continue;
      p = ALLOCNO_LIVE_RANGES (a);
      ira_assert (p != NULL);
      start = (high_pressure_start_point[cl] > p->start
	       ? high_pressure_start_point[cl] : p->start);
      ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a) += curr_point - start + 1;
    }
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
  int i;
  enum reg_class cover_class, cl;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;
  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
    return;
  sparseset_set_bit (allocnos_live, ALLOCNO_NUM (a));
  IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a), hard_regs_live);
  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a), hard_regs_live);
  cover_class = ALLOCNO_COVER_CLASS (a);
  for (i = 0;
       (cl = ira_reg_class_super_classes[cover_class][i]) != LIM_REG_CLASSES;
       i++)
    {
      curr_reg_pressure[cl] += ira_reg_class_nregs[cl][ALLOCNO_MODE (a)];
      if (high_pressure_start_point[cl] < 0
	  && (curr_reg_pressure[cl] > ira_available_class_regs[cl]))
	high_pressure_start_point[cl] = curr_point;
      if (curr_bb_node->reg_pressure[cl] < curr_reg_pressure[cl])
	curr_bb_node->reg_pressure[cl] = curr_reg_pressure[cl];
    }
}

/* Mark allocno A as currently not living and update current register
   pressure, start point of the register pressure excess, and register
   pressure excess length for living allocnos.  */
static void
clear_allocno_live (ira_allocno_t a)
{
  int i;
  unsigned int j;
  enum reg_class cover_class, cl;
  bool set_p;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;
  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
    {
      cover_class = ALLOCNO_COVER_CLASS (a);
      set_p = false;
      for (i = 0;
	   (cl = ira_reg_class_super_classes[cover_class][i])
	     != LIM_REG_CLASSES;
	   i++)
	{
	  curr_reg_pressure[cl] -= ira_reg_class_nregs[cl][ALLOCNO_MODE (a)];
	  ira_assert (curr_reg_pressure[cl] >= 0);
	  if (high_pressure_start_point[cl] >= 0
	      && curr_reg_pressure[cl] <= ira_available_class_regs[cl])
	    set_p = true;
	}
      if (set_p)
	{
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, j)
	    update_allocno_pressure_excess_length (ira_allocnos[j]);
	  for (i = 0;
	       (cl = ira_reg_class_super_classes[cover_class][i])
		 != LIM_REG_CLASSES;
	       i++)
	    if (high_pressure_start_point[cl] >= 0
		&& curr_reg_pressure[cl] <= ira_available_class_regs[cl])
	      high_pressure_start_point[cl] = -1;

	}
    }
  sparseset_clear_bit (allocnos_live, ALLOCNO_NUM (a));
}

/* Mark the register REG as live.  Store a 1 in hard_regs_live or
   allocnos_live for this register or the corresponding allocno,
   record how many consecutive hardware registers it actually
   needs.  */
static void
mark_reg_live (rtx reg)
{
  int i, regno;

  gcc_assert (REG_P (reg));
  regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      ira_allocno_t a = ira_curr_regno_allocno_map[regno];

      if (a != NULL)
	{
	  if (sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
	    {
	      /* Invalidate because it is referenced.  */
	      allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;
	      return;
	    }
	  set_allocno_live (a);
	}
      make_regno_born (regno);
    }
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class cover_class, cl;

      while (regno < last)
	{
	  if (! TEST_HARD_REG_BIT (hard_regs_live, regno)
	      && ! TEST_HARD_REG_BIT (eliminable_regset, regno))
	    {
	      cover_class = ira_hard_regno_cover_class[regno];
	      for (i = 0;
		   (cl = ira_reg_class_super_classes[cover_class][i])
		     != LIM_REG_CLASSES;
		   i++)
		{
		  curr_reg_pressure[cl]++;
		  if (high_pressure_start_point[cl] < 0
		      && (curr_reg_pressure[cl]
			  > ira_available_class_regs[cl]))
		    high_pressure_start_point[cl] = curr_point;
		}
	      make_regno_born (regno);
	      for (i = 0;
		   (cl = ira_reg_class_super_classes[cover_class][i])
		     != LIM_REG_CLASSES;
		   i++)
		{
		  if (curr_bb_node->reg_pressure[cl] < curr_reg_pressure[cl])
		    curr_bb_node->reg_pressure[cl] = curr_reg_pressure[cl];
		}
	    }
	  regno++;
	}
    }
}

/* Mark the register referenced by use or def REF as live.  */
static void
mark_ref_live (df_ref ref)
{
  rtx reg;

  reg = DF_REF_REG (ref);
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  mark_reg_live (reg);
}

/* Mark the register REG as dead.  Store a 0 in hard_regs_live or
   allocnos_live for the register.  */
static void
mark_reg_dead (rtx reg)
{
  int regno;

  gcc_assert (REG_P (reg));
  regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      ira_allocno_t a = ira_curr_regno_allocno_map[regno];

      if (a != NULL)
	{
	  if (! sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)))
	    {
	      /* Invalidate because it is referenced.  */
	      allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;
	      return;
	    }
	  clear_allocno_live (a);
	}
      make_regno_dead (regno);
    }
  else if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int i;
      unsigned int j;
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class cover_class, cl;
      bool set_p;

      while (regno < last)
	{
	  if (TEST_HARD_REG_BIT (hard_regs_live, regno))
	    {
	      set_p = false;
	      cover_class = ira_hard_regno_cover_class[regno];
	      for (i = 0;
		   (cl = ira_reg_class_super_classes[cover_class][i])
		     != LIM_REG_CLASSES;
		   i++)
  		{
		  curr_reg_pressure[cl]--;
		  if (high_pressure_start_point[cl] >= 0
		      && curr_reg_pressure[cl] <= ira_available_class_regs[cl])
		    set_p = true;
		  ira_assert (curr_reg_pressure[cl] >= 0);
		}
	      if (set_p)
		{
		  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, j)
		    update_allocno_pressure_excess_length (ira_allocnos[j]);
		  for (i = 0;
		       (cl = ira_reg_class_super_classes[cover_class][i])
			 != LIM_REG_CLASSES;
		       i++)
		    if (high_pressure_start_point[cl] >= 0
			&& (curr_reg_pressure[cl]
			    <= ira_available_class_regs[cl]))
		      high_pressure_start_point[cl] = -1;
		}
	      make_regno_dead (regno);
	    }
	  regno++;
	}
    }
}

/* Mark the register referenced by definition DEF as dead, if the
   definition is a total one.  */
static void
mark_ref_dead (df_ref def)
{
  rtx reg;

  if (DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL)
      || DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL))
    return;

  reg = DF_REF_REG (def);
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  mark_reg_dead (reg);
}

/* Make pseudo REG conflicting with pseudo DREG, if the 1st pseudo
   class is intersected with class CL.  Advance the current program
   point before making the conflict if ADVANCE_P.  Return TRUE if we
   will need to advance the current program point.  */
static bool
make_pseudo_conflict (rtx reg, enum reg_class cl, rtx dreg, bool advance_p)
{
  ira_allocno_t a;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg) || REGNO (reg) < FIRST_PSEUDO_REGISTER)
    return advance_p;

  a = ira_curr_regno_allocno_map[REGNO (reg)];
  if (! reg_classes_intersect_p (cl, ALLOCNO_COVER_CLASS (a)))
    return advance_p;

  if (advance_p)
    curr_point++;

  mark_reg_live (reg);
  mark_reg_live (dreg);
  mark_reg_dead (reg);
  mark_reg_dead (dreg);

  return false;
}

/* Check and make if necessary conflicts for pseudo DREG of class
   DEF_CL of the current insn with input operand USE of class USE_CL.
   Advance the current program point before making the conflict if
   ADVANCE_P.  Return TRUE if we will need to advance the current
   program point.  */
static bool
check_and_make_def_use_conflict (rtx dreg, enum reg_class def_cl,
				 int use, enum reg_class use_cl,
				 bool advance_p)
{
  if (! reg_classes_intersect_p (def_cl, use_cl))
    return advance_p;

  advance_p = make_pseudo_conflict (recog_data.operand[use],
				    use_cl, dreg, advance_p);
  /* Reload may end up swapping commutative operands, so you
     have to take both orderings into account.  The
     constraints for the two operands can be completely
     different.  (Indeed, if the constraints for the two
     operands are the same for all alternatives, there's no
     point marking them as commutative.)  */
  if (use < recog_data.n_operands - 1
      && recog_data.constraints[use][0] == '%')
    advance_p
      = make_pseudo_conflict (recog_data.operand[use + 1],
			      use_cl, dreg, advance_p);
  if (use >= 1
      && recog_data.constraints[use - 1][0] == '%')
    advance_p
      = make_pseudo_conflict (recog_data.operand[use - 1],
			      use_cl, dreg, advance_p);
  return advance_p;
}

/* Check and make if necessary conflicts for definition DEF of class
   DEF_CL of the current insn with input operands.  Process only
   constraints of alternative ALT.  */
static void
check_and_make_def_conflict (int alt, int def, enum reg_class def_cl)
{
  int use, use_match;
  ira_allocno_t a;
  enum reg_class use_cl, acl;
  bool advance_p;
  rtx dreg = recog_data.operand[def];

  if (def_cl == NO_REGS)
    return;

  if (GET_CODE (dreg) == SUBREG)
    dreg = SUBREG_REG (dreg);

  if (! REG_P (dreg) || REGNO (dreg) < FIRST_PSEUDO_REGISTER)
    return;

  a = ira_curr_regno_allocno_map[REGNO (dreg)];
  acl = ALLOCNO_COVER_CLASS (a);
  if (! reg_classes_intersect_p (acl, def_cl))
    return;

  advance_p = true;

  for (use = 0; use < recog_data.n_operands; use++)
    {
      int alt1;

      if (use == def || recog_data.operand_type[use] == OP_OUT)
	continue;

      if (recog_op_alt[use][alt].anything_ok)
	use_cl = ALL_REGS;
      else
	use_cl = recog_op_alt[use][alt].cl;

      /* If there's any alternative that allows USE to match DEF, do not
	 record a conflict.  If that causes us to create an invalid
	 instruction due to the earlyclobber, reload must fix it up.  */	 
      for (alt1 = 0; alt1 < recog_data.n_alternatives; alt1++)
	if (recog_op_alt[use][alt1].matches == def
	    || (use < recog_data.n_operands - 1
		&& recog_data.constraints[use][0] == '%'
		&& recog_op_alt[use + 1][alt1].matches == def)
	    || (use >= 1
		&& recog_data.constraints[use - 1][0] == '%'
		&& recog_op_alt[use - 1][alt1].matches == def))
	  break;

      if (alt1 < recog_data.n_alternatives)
	continue;

      advance_p = check_and_make_def_use_conflict (dreg, def_cl, use,
						   use_cl, advance_p);

      if ((use_match = recog_op_alt[use][alt].matches) >= 0)
	{
	  if (use_match == def)
	    continue;

	  if (recog_op_alt[use_match][alt].anything_ok)
	    use_cl = ALL_REGS;
	  else
	    use_cl = recog_op_alt[use_match][alt].cl;
	  advance_p = check_and_make_def_use_conflict (dreg, def_cl, use,
						       use_cl, advance_p);
	}
    }
}

/* Make conflicts of early clobber pseudo registers of the current
   insn with its inputs.  Avoid introducing unnecessary conflicts by
   checking classes of the constraints and pseudos because otherwise
   significant code degradation is possible for some targets.  */
static void
make_early_clobber_and_input_conflicts (void)
{
  int alt;
  int def, def_match;
  enum reg_class def_cl;

  for (alt = 0; alt < recog_data.n_alternatives; alt++)
    for (def = 0; def < recog_data.n_operands; def++)
      {
	def_cl = NO_REGS;
	if (recog_op_alt[def][alt].earlyclobber)
	  {
	    if (recog_op_alt[def][alt].anything_ok)
	      def_cl = ALL_REGS;
	    else
	      def_cl = recog_op_alt[def][alt].cl;
	    check_and_make_def_conflict (alt, def, def_cl);
	  }
	if ((def_match = recog_op_alt[def][alt].matches) >= 0
	    && (recog_op_alt[def_match][alt].earlyclobber
		|| recog_op_alt[def][alt].earlyclobber))
	  {
	    if (recog_op_alt[def_match][alt].anything_ok)
	      def_cl = ALL_REGS;
	    else
	      def_cl = recog_op_alt[def_match][alt].cl;
	    check_and_make_def_conflict (alt, def, def_cl);
	  }
      }
}

/* Mark early clobber hard registers of the current INSN as live (if
   LIVE_P) or dead.  Return true if there are such registers.  */
static bool
mark_hard_reg_early_clobbers (rtx insn, bool live_p)
{
  df_ref *def_rec;
  bool set_p = false;

  for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
    if (DF_REF_FLAGS_IS_SET (*def_rec, DF_REF_MUST_CLOBBER))
      {
	rtx dreg = DF_REF_REG (*def_rec);

	if (GET_CODE (dreg) == SUBREG)
	  dreg = SUBREG_REG (dreg);
	if (! REG_P (dreg) || REGNO (dreg) >= FIRST_PSEUDO_REGISTER)
	  continue;

	/* Hard register clobbers are believed to be early clobber
	   because there is no way to say that non-operand hard
	   register clobbers are not early ones.  */
	if (live_p)
	  mark_ref_live (*def_rec);
	else
	  mark_ref_dead (*def_rec);
	set_p = true;
      }

  return set_p;
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
	  if (CONST_INT_P (op)
	      || (GET_CODE (op) == CONST_DOUBLE && GET_MODE (op) == VOIDmode)
	      || (equiv_const != NULL_RTX
		  && (CONST_INT_P (equiv_const)
		      || (GET_CODE (equiv_const) == CONST_DOUBLE
			  && GET_MODE (equiv_const) == VOIDmode))))
	    return NO_REGS;
	  break;

	case 's':
	  if ((CONSTANT_P (op) && !CONST_INT_P (op)
	       && (GET_CODE (op) != CONST_DOUBLE || GET_MODE (op) != VOIDmode))
	      || (equiv_const != NULL_RTX
		  && CONSTANT_P (equiv_const)
		  && !CONST_INT_P (equiv_const)
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
	  if ((CONST_INT_P (op)
	       && CONST_OK_FOR_CONSTRAINT_P (INTVAL (op), c, constraints))
	      || (equiv_const != NULL_RTX
		  && CONST_INT_P (equiv_const)
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
	      || (ira_available_class_regs[next_cl]
		  > ira_reg_class_nregs[next_cl][GET_MODE (op)]))
	    return NO_REGS;
	  cl = next_cl;
	  break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  next_cl
	    = single_reg_class (recog_data.constraints[c - '0'],
				recog_data.operand[c - '0'], NULL_RTX);
	  if ((cl != NO_REGS && next_cl != cl)
	      || next_cl == NO_REGS
	      || (ira_available_class_regs[next_cl]
		  > ira_reg_class_nregs[next_cl][GET_MODE (op)]))
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

/* The function sets up hard register set *SET to hard registers which
   might be used by insn reloads because the constraints are too
   strict.  */
void
ira_implicitly_set_insn_hard_regs (HARD_REG_SET *set)
{
  int i, c, regno = 0;
  bool ignore_p;
  enum reg_class cl;
  rtx op;
  enum machine_mode mode;

  CLEAR_HARD_REG_SET (*set);
  for (i = 0; i < recog_data.n_operands; i++)
    {
      op = recog_data.operand[i];

      if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);

      if (GET_CODE (op) == SCRATCH
	  || (REG_P (op) && (regno = REGNO (op)) >= FIRST_PSEUDO_REGISTER))
	{
	  const char *p = recog_data.constraints[i];

	  mode = (GET_CODE (op) == SCRATCH
		  ? GET_MODE (op) : PSEUDO_REGNO_MODE (regno));
	  cl = NO_REGS;
	  for (ignore_p = false; (c = *p); p += CONSTRAINT_LEN (c, p))
	    if (c == '#')
	      ignore_p = true;
	    else if (c == ',')
	      ignore_p = false;
	    else if (! ignore_p)
	      switch (c)
		{
		case 'r':
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		case 'h': case 'j': case 'k': case 'l':
		case 'q': case 't': case 'u':
		case 'v': case 'w': case 'x': case 'y': case 'z':
		case 'A': case 'B': case 'C': case 'D':
		case 'Q': case 'R': case 'S': case 'T': case 'U':
		case 'W': case 'Y': case 'Z':
		  cl = (c == 'r'
			? GENERAL_REGS
			: REG_CLASS_FROM_CONSTRAINT (c, p));
		  if (cl != NO_REGS
		      /* There is no register pressure problem if all of the
			 regs in this class are fixed.  */
		      && ira_available_class_regs[cl] != 0
		      && (ira_available_class_regs[cl]
			  <= ira_reg_class_nregs[cl][mode]))
		    IOR_HARD_REG_SET (*set, reg_class_contents[cl]);
		  break;
		}
	}
    }
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
  enum reg_class cl;
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
	      int i, size;
	      cost
		= (freq
		   * (in_p
		      ? ira_get_register_move_cost (mode, cover_class, cl)
		      : ira_get_register_move_cost (mode, cl, cover_class)));
	      ira_allocate_and_set_costs
		(&ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a), cover_class, 0);
	      size = ira_reg_class_nregs[cover_class][mode];
	      for (i = 0; i < size; i++)
	        ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a)
		  [ira_class_hard_reg_index
		   [cover_class][ira_class_hard_regs[cl][i]]]
		  -= cost;
	    }
	}

      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, px)
        {
	  a = ira_allocnos[px];
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

/* Return true when one of the predecessor edges of BB is marked with
   EDGE_ABNORMAL_CALL or EDGE_EH.  */
static bool
bb_has_abnormal_call_pred (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (e->flags & (EDGE_ABNORMAL_CALL | EDGE_EH))
	return true;
    }
  return false;
}

/* Process insns of the basic block given by its LOOP_TREE_NODE to
   update allocno live ranges, allocno hard register conflicts,
   intersected calls, and register pressure info for allocnos for the
   basic block for and regions containing the basic block.  */
static void
process_bb_node_lives (ira_loop_tree_node_t loop_tree_node)
{
  int i, freq;
  unsigned int j;
  basic_block bb;
  rtx insn;
  bitmap_iterator bi;
  bitmap reg_live_out;
  unsigned int px;
  bool set_p;

  bb = loop_tree_node->bb;
  if (bb != NULL)
    {
      for (i = 0; i < ira_reg_class_cover_size; i++)
	{
	  curr_reg_pressure[ira_reg_class_cover[i]] = 0;
	  high_pressure_start_point[ira_reg_class_cover[i]] = -1;
	}
      curr_bb_node = loop_tree_node;
      reg_live_out = DF_LR_OUT (bb);
      sparseset_clear (allocnos_live);
      REG_SET_TO_HARD_REG_SET (hard_regs_live, reg_live_out);
      AND_COMPL_HARD_REG_SET (hard_regs_live, eliminable_regset);
      AND_COMPL_HARD_REG_SET (hard_regs_live, ira_no_alloc_regs);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (hard_regs_live, i))
	  {
	    enum reg_class cover_class, cl;

	    cover_class = ira_class_translate[REGNO_REG_CLASS (i)];
	    for (j = 0;
		 (cl = ira_reg_class_super_classes[cover_class][j])
		   != LIM_REG_CLASSES;
		 j++)
	      {
		curr_reg_pressure[cl]++;
		if (curr_bb_node->reg_pressure[cl] < curr_reg_pressure[cl])
		  curr_bb_node->reg_pressure[cl] = curr_reg_pressure[cl];
		ira_assert (curr_reg_pressure[cl]
			    <= ira_available_class_regs[cl]);
	      }
	  }
      EXECUTE_IF_SET_IN_BITMAP (reg_live_out, FIRST_PSEUDO_REGISTER, j, bi)
	{
	  ira_allocno_t a = ira_curr_regno_allocno_map[j];

	  if (a == NULL)
	    continue;
	  ira_assert (! sparseset_bit_p (allocnos_live, ALLOCNO_NUM (a)));
	  set_allocno_live (a);
	  make_regno_born (j);
	}

      freq = REG_FREQ_FROM_BB (bb);
      if (freq == 0)
	freq = 1;

      /* Invalidate all allocno_saved_at_call entries.  */
      last_call_num++;

      /* Scan the code of this basic block, noting which allocnos and
	 hard regs are born or die.

	 Note that this loop treats uninitialized values as live until
	 the beginning of the block.  For example, if an instruction
	 uses (reg:DI foo), and only (subreg:SI (reg:DI foo) 0) is ever
	 set, FOO will remain live until the beginning of the block.
	 Likewise if FOO is not set at all.  This is unnecessarily
	 pessimistic, but it probably doesn't matter much in practice.  */
      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  df_ref *def_rec, *use_rec;
	  bool call_p;

	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	    fprintf (ira_dump_file, "   Insn %u(l%d): point = %d\n",
		     INSN_UID (insn), loop_tree_node->parent->loop->num,
		     curr_point);

	  /* Mark each defined value as live.  We need to do this for
	     unused values because they still conflict with quantities
	     that are live at the time of the definition.

	     Ignore DF_REF_MAY_CLOBBERs on a call instruction.  Such
	     references represent the effect of the called function
	     on a call-clobbered register.  Marking the register as
	     live would stop us from allocating it to a call-crossing
	     allocno.  */
	  call_p = CALL_P (insn);
	  for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	    if (!call_p || !DF_REF_FLAGS_IS_SET (*def_rec, DF_REF_MAY_CLOBBER))
	      mark_ref_live (*def_rec);

	  /* If INSN has multiple outputs, then any value used in one
	     of the outputs conflicts with the other outputs.  Model this
	     by making the used value live during the output phase.

	     It is unsafe to use !single_set here since it will ignore
	     an unused output.  Just because an output is unused does
	     not mean the compiler can assume the side effect will not
	     occur.  Consider if ALLOCNO appears in the address of an
	     output and we reload the output.  If we allocate ALLOCNO
	     to the same hard register as an unused output we could
	     set the hard register before the output reload insn.  */
	  if (GET_CODE (PATTERN (insn)) == PARALLEL && multiple_sets (insn))
	    for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
	      {
		int i;
		rtx reg;

		reg = DF_REF_REG (*use_rec);
		for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		  {
		    rtx set;

		    set = XVECEXP (PATTERN (insn), 0, i);
		    if (GET_CODE (set) == SET
			&& reg_overlap_mentioned_p (reg, SET_DEST (set)))
		      {
			/* After the previous loop, this is a no-op if
			   REG is contained within SET_DEST (SET).  */
			mark_ref_live (*use_rec);
			break;
		      }
		  }
	      }

	  extract_insn (insn);
	  preprocess_constraints ();
	  process_single_reg_class_operands (false, freq);

	  /* See which defined values die here.  */
	  for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	    if (!call_p || !DF_REF_FLAGS_IS_SET (*def_rec, DF_REF_MAY_CLOBBER))
	      mark_ref_dead (*def_rec);

	  if (call_p)
	    {
	      last_call_num++;
	      /* The current set of live allocnos are live across the call.  */
	      EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, i)
	        {
		  ira_allocno_t a = ira_allocnos[i];

		  if (allocno_saved_at_call[i] != last_call_num)
		    /* Here we are mimicking caller-save.c behaviour
		       which does not save hard register at a call if
		       it was saved on previous call in the same basic
		       block and the hard register was not mentioned
		       between the two calls.  */
		    ALLOCNO_CALL_FREQ (a) += freq;
		  /* Mark it as saved at the next call.  */
		  allocno_saved_at_call[i] = last_call_num + 1;
		  ALLOCNO_CALLS_CROSSED_NUM (a)++;
		  /* Don't allocate allocnos that cross setjmps or any
		     call, if this function receives a nonlocal
		     goto.  */
		  if (cfun->has_nonlocal_label
		      || find_reg_note (insn, REG_SETJMP,
					NULL_RTX) != NULL_RTX)
		    {
		      SET_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a));
		      SET_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
		    }
		  if (can_throw_internal (insn))
		    {
		      IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a),
					call_used_reg_set);
		      IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a),
					call_used_reg_set);
		    }
		}
	    }

	  make_early_clobber_and_input_conflicts ();

	  curr_point++;

	  /* Mark each used value as live.  */
	  for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
	    mark_ref_live (*use_rec);

	  process_single_reg_class_operands (true, freq);

	  set_p = mark_hard_reg_early_clobbers (insn, true);

	  if (set_p)
	    {
	      mark_hard_reg_early_clobbers (insn, false);

	      /* Mark each hard reg as live again.  For example, a
		 hard register can be in clobber and in an insn
		 input.  */
	      for (use_rec = DF_INSN_USES (insn); *use_rec; use_rec++)
		{
		  rtx ureg = DF_REF_REG (*use_rec);

		  if (GET_CODE (ureg) == SUBREG)
		    ureg = SUBREG_REG (ureg);
		  if (! REG_P (ureg) || REGNO (ureg) >= FIRST_PSEUDO_REGISTER)
		    continue;

		  mark_ref_live (*use_rec);
		}
	    }

	  curr_point++;
	}

#ifdef EH_RETURN_DATA_REGNO
      if (bb_has_eh_pred (bb))
	for (j = 0; ; ++j)
	  {
	    unsigned int regno = EH_RETURN_DATA_REGNO (j);
	    if (regno == INVALID_REGNUM)
	      break;
	    make_regno_born (regno);
	  }
#endif

      /* Allocnos can't go in stack regs at the start of a basic block
	 that is reached by an abnormal edge. Likewise for call
	 clobbered regs, because caller-save, fixup_abnormal_edges and
	 possibly the table driven EH machinery are not quite ready to
	 handle such allocnos live across such edges.  */
      if (bb_has_abnormal_pred (bb))
	{
#ifdef STACK_REGS
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, px)
	    {
	      ALLOCNO_NO_STACK_REG_P (ira_allocnos[px]) = true;
	      ALLOCNO_TOTAL_NO_STACK_REG_P (ira_allocnos[px]) = true;
	    }
	  for (px = FIRST_STACK_REG; px <= LAST_STACK_REG; px++)
	    make_regno_born (px);
#endif
	  /* No need to record conflicts for call clobbered regs if we
	     have nonlocal labels around, as we don't ever try to
	     allocate such regs in this case.  */
	  if (!cfun->has_nonlocal_label && bb_has_abnormal_call_pred (bb))
	    for (px = 0; px < FIRST_PSEUDO_REGISTER; px++)
	      if (call_used_regs[px])
		make_regno_born (px);
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

/* Compress allocno live ranges by removing program points where
   nothing happens.  */
static void
remove_some_program_points_and_update_live_ranges (void)
{
  unsigned i;
  int n;
  int *map;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  allocno_live_range_t r;
  bitmap born_or_died;
  bitmap_iterator bi;

  born_or_died = ira_allocate_bitmap ();
  FOR_EACH_ALLOCNO (a, ai)
    {
      for (r = ALLOCNO_LIVE_RANGES (a); r != NULL; r = r->next)
	{
	  ira_assert (r->start <= r->finish);
	  bitmap_set_bit (born_or_died, r->start);
	  bitmap_set_bit (born_or_died, r->finish);
	}
    }
  map = (int *) ira_allocate (sizeof (int) * ira_max_point);
  n = 0;
  EXECUTE_IF_SET_IN_BITMAP(born_or_died, 0, i, bi)
    {
      map[i] = n++;
    }
  ira_free_bitmap (born_or_died);
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "Compressing live ranges: from %d to %d - %d%%\n",
	     ira_max_point, n, 100 * n / ira_max_point);
  ira_max_point = n;
  FOR_EACH_ALLOCNO (a, ai)
    {
      for (r = ALLOCNO_LIVE_RANGES (a); r != NULL; r = r->next)
	{
	  r->start = map[r->start];
	  r->finish = map[r->finish];
	}
    }
  ira_free (map);
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
  curr_point = 0;
  last_call_num = 0;
  allocno_saved_at_call
    = (int *) ira_allocate (ira_allocnos_num * sizeof (int));
  memset (allocno_saved_at_call, 0, ira_allocnos_num * sizeof (int));
  ira_traverse_loop_tree (true, ira_loop_tree_root, NULL,
			  process_bb_node_lives);
  ira_max_point = curr_point;
  create_start_finish_chains ();
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_live_ranges (ira_dump_file);
  /* Clean up.  */
  ira_free (allocno_saved_at_call);
  sparseset_free (allocnos_live);
}

/* Compress allocno live ranges.  */
void
ira_compress_allocno_live_ranges (void)
{
  remove_some_program_points_and_update_live_ranges ();
  ira_rebuild_start_finish_chains ();
  if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file, "Ranges after the compression:\n");
      print_live_ranges (ira_dump_file);
    }
}

/* Free arrays IRA_START_POINT_RANGES and IRA_FINISH_POINT_RANGES.  */
void
ira_finish_allocno_live_ranges (void)
{
  ira_free (ira_finish_point_ranges);
  ira_free (ira_start_point_ranges);
}
