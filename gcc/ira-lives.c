/* IRA processing allocno lives to build allocno live ranges.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.
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
#include "diagnostic-core.h"
#include "params.h"
#include "df.h"
#include "sbitmap.h"
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
live_range_t *ira_start_point_ranges, *ira_finish_point_ranges;

/* Number of the current program point.  */
static int curr_point;

/* Point where register pressure excess started or -1 if there is no
   register pressure excess.  Excess pressure for a register class at
   some point means that there are more allocnos of given register
   class living at the point than number of hard-registers of the
   class available for the allocation.  It is defined only for
   pressure classes.  */
static int high_pressure_start_point[N_REG_CLASSES];

/* Objects live at current point in the scan.  */
static sparseset objects_live;

/* A temporary bitmap used in functions that wish to avoid visiting an allocno
   multiple times.  */
static sparseset allocnos_processed;

/* Set of hard regs (except eliminable ones) currently live.  */
static HARD_REG_SET hard_regs_live;

/* The loop tree node corresponding to the current basic block.  */
static ira_loop_tree_node_t curr_bb_node;

/* The number of the last processed call.  */
static int last_call_num;
/* The number of last call at which given allocno was saved.  */
static int *allocno_saved_at_call;

/* Record the birth of hard register REGNO, updating hard_regs_live and
   hard reg conflict information for living allocnos.  */
static void
make_hard_regno_born (int regno)
{
  unsigned int i;

  SET_HARD_REG_BIT (hard_regs_live, regno);
  EXECUTE_IF_SET_IN_SPARSESET (objects_live, i)
    {
      ira_object_t obj = ira_object_id_map[i];

      SET_HARD_REG_BIT (OBJECT_CONFLICT_HARD_REGS (obj), regno);
      SET_HARD_REG_BIT (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), regno);
    }
}

/* Process the death of hard register REGNO.  This updates
   hard_regs_live.  */
static void
make_hard_regno_dead (int regno)
{
  CLEAR_HARD_REG_BIT (hard_regs_live, regno);
}

/* Record the birth of object OBJ.  Set a bit for it in objects_live,
   start a new live range for it if necessary and update hard register
   conflicts.  */
static void
make_object_born (ira_object_t obj)
{
  live_range_t lr = OBJECT_LIVE_RANGES (obj);

  sparseset_set_bit (objects_live, OBJECT_CONFLICT_ID (obj));
  IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj), hard_regs_live);
  IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), hard_regs_live);

  if (lr == NULL
      || (lr->finish != curr_point && lr->finish + 1 != curr_point))
    ira_add_live_range_to_object (obj, curr_point, -1);
}

/* Update ALLOCNO_EXCESS_PRESSURE_POINTS_NUM for the allocno
   associated with object OBJ.  */
static void
update_allocno_pressure_excess_length (ira_object_t obj)
{
  ira_allocno_t a = OBJECT_ALLOCNO (obj);
  int start, i;
  enum reg_class aclass, pclass, cl;
  live_range_t p;

  aclass = ALLOCNO_CLASS (a);
  pclass = ira_pressure_class_translate[aclass];
  for (i = 0;
       (cl = ira_reg_class_super_classes[pclass][i]) != LIM_REG_CLASSES;
       i++)
    {
      if (! ira_reg_pressure_class_p[cl])
	continue;
      if (high_pressure_start_point[cl] < 0)
	continue;
      p = OBJECT_LIVE_RANGES (obj);
      ira_assert (p != NULL);
      start = (high_pressure_start_point[cl] > p->start
	       ? high_pressure_start_point[cl] : p->start);
      ALLOCNO_EXCESS_PRESSURE_POINTS_NUM (a) += curr_point - start + 1;
    }
}

/* Process the death of object OBJ, which is associated with allocno
   A.  This finishes the current live range for it.  */
static void
make_object_dead (ira_object_t obj)
{
  live_range_t lr;

  sparseset_clear_bit (objects_live, OBJECT_CONFLICT_ID (obj));
  lr = OBJECT_LIVE_RANGES (obj);
  ira_assert (lr != NULL);
  lr->finish = curr_point;
  update_allocno_pressure_excess_length (obj);
}

/* The current register pressures for each pressure class for the current
   basic block.  */
static int curr_reg_pressure[N_REG_CLASSES];

/* Record that register pressure for PCLASS increased by N registers.
   Update the current register pressure, maximal register pressure for
   the current BB and the start point of the register pressure
   excess.  */
static void
inc_register_pressure (enum reg_class pclass, int n)
{
  int i;
  enum reg_class cl;

  for (i = 0;
       (cl = ira_reg_class_super_classes[pclass][i]) != LIM_REG_CLASSES;
       i++)
    {
      if (! ira_reg_pressure_class_p[cl])
	continue;
      curr_reg_pressure[cl] += n;
      if (high_pressure_start_point[cl] < 0
	  && (curr_reg_pressure[cl] > ira_class_hard_regs_num[cl]))
	high_pressure_start_point[cl] = curr_point;
      if (curr_bb_node->reg_pressure[cl] < curr_reg_pressure[cl])
	curr_bb_node->reg_pressure[cl] = curr_reg_pressure[cl];
    }
}

/* Record that register pressure for PCLASS has decreased by NREGS
   registers; update current register pressure, start point of the
   register pressure excess, and register pressure excess length for
   living allocnos.  */

static void
dec_register_pressure (enum reg_class pclass, int nregs)
{
  int i;
  unsigned int j;
  enum reg_class cl;
  bool set_p = false;

  for (i = 0;
       (cl = ira_reg_class_super_classes[pclass][i]) != LIM_REG_CLASSES;
       i++)
    {
      if (! ira_reg_pressure_class_p[cl])
	continue;
      curr_reg_pressure[cl] -= nregs;
      ira_assert (curr_reg_pressure[cl] >= 0);
      if (high_pressure_start_point[cl] >= 0
	  && curr_reg_pressure[cl] <= ira_class_hard_regs_num[cl])
	set_p = true;
    }
  if (set_p)
    {
      EXECUTE_IF_SET_IN_SPARSESET (objects_live, j)
	update_allocno_pressure_excess_length (ira_object_id_map[j]);
      for (i = 0;
	   (cl = ira_reg_class_super_classes[pclass][i]) != LIM_REG_CLASSES;
	   i++)
	{
	  if (! ira_reg_pressure_class_p[cl])
	    continue;
	  if (high_pressure_start_point[cl] >= 0
	      && curr_reg_pressure[cl] <= ira_class_hard_regs_num[cl])
	    high_pressure_start_point[cl] = -1;
	}
    }
}

/* Determine from the objects_live bitmap whether REGNO is currently live,
   and occupies only one object.  Return false if we have no information.  */
static bool
pseudo_regno_single_word_and_live_p (int regno)
{
  ira_allocno_t a = ira_curr_regno_allocno_map[regno];
  ira_object_t obj;

  if (a == NULL)
    return false;
  if (ALLOCNO_NUM_OBJECTS (a) > 1)
    return false;

  obj = ALLOCNO_OBJECT (a, 0);

  return sparseset_bit_p (objects_live, OBJECT_CONFLICT_ID (obj));
}

/* Mark the pseudo register REGNO as live.  Update all information about
   live ranges and register pressure.  */
static void
mark_pseudo_regno_live (int regno)
{
  ira_allocno_t a = ira_curr_regno_allocno_map[regno];
  enum reg_class pclass;
  int i, n, nregs;

  if (a == NULL)
    return;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;

  n = ALLOCNO_NUM_OBJECTS (a);
  pclass = ira_pressure_class_translate[ALLOCNO_CLASS (a)];
  nregs = ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)];
  if (n > 1)
    {
      /* We track every subobject separately.  */
      gcc_assert (nregs == n);
      nregs = 1;
    }

  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);

      if (sparseset_bit_p (objects_live, OBJECT_CONFLICT_ID (obj)))
	continue;

      inc_register_pressure (pclass, nregs);
      make_object_born (obj);
    }
}

/* Like mark_pseudo_regno_live, but try to only mark one subword of
   the pseudo as live.  SUBWORD indicates which; a value of 0
   indicates the low part.  */
static void
mark_pseudo_regno_subword_live (int regno, int subword)
{
  ira_allocno_t a = ira_curr_regno_allocno_map[regno];
  int n;
  enum reg_class pclass;
  ira_object_t obj;

  if (a == NULL)
    return;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;

  n = ALLOCNO_NUM_OBJECTS (a);
  if (n == 1)
    {
      mark_pseudo_regno_live (regno);
      return;
    }

  pclass = ira_pressure_class_translate[ALLOCNO_CLASS (a)];
  gcc_assert
    (n == ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]);
  obj = ALLOCNO_OBJECT (a, subword);

  if (sparseset_bit_p (objects_live, OBJECT_CONFLICT_ID (obj)))
    return;

  inc_register_pressure (pclass, 1);
  make_object_born (obj);
}

/* Mark the register REG as live.  Store a 1 in hard_regs_live for
   this register, record how many consecutive hardware registers it
   actually needs.  */
static void
mark_hard_reg_live (rtx reg)
{
  int regno = REGNO (reg);

  if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class aclass, pclass;

      while (regno < last)
	{
	  if (! TEST_HARD_REG_BIT (hard_regs_live, regno)
	      && ! TEST_HARD_REG_BIT (eliminable_regset, regno))
	    {
	      aclass = ira_hard_regno_allocno_class[regno];
	      pclass = ira_pressure_class_translate[aclass];
	      inc_register_pressure (pclass, 1);
	      make_hard_regno_born (regno);
	    }
	  regno++;
	}
    }
}

/* Mark a pseudo, or one of its subwords, as live.  REGNO is the pseudo's
   register number; ORIG_REG is the access in the insn, which may be a
   subreg.  */
static void
mark_pseudo_reg_live (rtx orig_reg, unsigned regno)
{
  if (df_read_modify_subreg_p (orig_reg))
    {
      mark_pseudo_regno_subword_live (regno,
				      subreg_lowpart_p (orig_reg) ? 0 : 1);
    }
  else
    mark_pseudo_regno_live (regno);
}

/* Mark the register referenced by use or def REF as live.  */
static void
mark_ref_live (df_ref ref)
{
  rtx reg = DF_REF_REG (ref);
  rtx orig_reg = reg;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    mark_pseudo_reg_live (orig_reg, REGNO (reg));
  else
    mark_hard_reg_live (reg);
}

/* Mark the pseudo register REGNO as dead.  Update all information about
   live ranges and register pressure.  */
static void
mark_pseudo_regno_dead (int regno)
{
  ira_allocno_t a = ira_curr_regno_allocno_map[regno];
  int n, i, nregs;
  enum reg_class cl;

  if (a == NULL)
    return;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;

  n = ALLOCNO_NUM_OBJECTS (a);
  cl = ira_pressure_class_translate[ALLOCNO_CLASS (a)];
  nregs = ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)];
  if (n > 1)
    {
      /* We track every subobject separately.  */
      gcc_assert (nregs == n);
      nregs = 1;
    }
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      if (!sparseset_bit_p (objects_live, OBJECT_CONFLICT_ID (obj)))
	continue;

      dec_register_pressure (cl, nregs);
      make_object_dead (obj);
    }
}

/* Like mark_pseudo_regno_dead, but called when we know that only part of the
   register dies.  SUBWORD indicates which; a value of 0 indicates the low part.  */
static void
mark_pseudo_regno_subword_dead (int regno, int subword)
{
  ira_allocno_t a = ira_curr_regno_allocno_map[regno];
  int n;
  enum reg_class cl;
  ira_object_t obj;

  if (a == NULL)
    return;

  /* Invalidate because it is referenced.  */
  allocno_saved_at_call[ALLOCNO_NUM (a)] = 0;

  n = ALLOCNO_NUM_OBJECTS (a);
  if (n == 1)
    /* The allocno as a whole doesn't die in this case.  */
    return;

  cl = ira_pressure_class_translate[ALLOCNO_CLASS (a)];
  gcc_assert
    (n == ira_reg_class_max_nregs[ALLOCNO_CLASS (a)][ALLOCNO_MODE (a)]);

  obj = ALLOCNO_OBJECT (a, subword);
  if (!sparseset_bit_p (objects_live, OBJECT_CONFLICT_ID (obj)))
    return;

  dec_register_pressure (cl, 1);
  make_object_dead (obj);
}

/* Mark the hard register REG as dead.  Store a 0 in hard_regs_live for the
   register.  */
static void
mark_hard_reg_dead (rtx reg)
{
  int regno = REGNO (reg);

  if (! TEST_HARD_REG_BIT (ira_no_alloc_regs, regno))
    {
      int last = regno + hard_regno_nregs[regno][GET_MODE (reg)];
      enum reg_class aclass, pclass;

      while (regno < last)
	{
	  if (TEST_HARD_REG_BIT (hard_regs_live, regno))
	    {
	      aclass = ira_hard_regno_allocno_class[regno];
	      pclass = ira_pressure_class_translate[aclass];
	      dec_register_pressure (pclass, 1);
	      make_hard_regno_dead (regno);
	    }
	  regno++;
	}
    }
}

/* Mark a pseudo, or one of its subwords, as dead.  REGNO is the pseudo's
   register number; ORIG_REG is the access in the insn, which may be a
   subreg.  */
static void
mark_pseudo_reg_dead (rtx orig_reg, unsigned regno)
{
  if (df_read_modify_subreg_p (orig_reg))
    {
      mark_pseudo_regno_subword_dead (regno,
				      subreg_lowpart_p (orig_reg) ? 0 : 1);
    }
  else
    mark_pseudo_regno_dead (regno);
}

/* Mark the register referenced by definition DEF as dead, if the
   definition is a total one.  */
static void
mark_ref_dead (df_ref def)
{
  rtx reg = DF_REF_REG (def);
  rtx orig_reg = reg;

  if (DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL))
    return;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL)
      && (GET_CODE (orig_reg) != SUBREG
	  || REGNO (reg) < FIRST_PSEUDO_REGISTER
	  || !df_read_modify_subreg_p (orig_reg)))
    return;

  if (REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    mark_pseudo_reg_dead (orig_reg, REGNO (reg));
  else
    mark_hard_reg_dead (reg);
}

/* If REG is a pseudo or a subreg of it, and the class of its allocno
   intersects CL, make a conflict with pseudo DREG.  ORIG_DREG is the
   rtx actually accessed, it may be identical to DREG or a subreg of it.
   Advance the current program point before making the conflict if
   ADVANCE_P.  Return TRUE if we will need to advance the current
   program point.  */
static bool
make_pseudo_conflict (rtx reg, enum reg_class cl, rtx dreg, rtx orig_dreg,
		      bool advance_p)
{
  rtx orig_reg = reg;
  ira_allocno_t a;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (! REG_P (reg) || REGNO (reg) < FIRST_PSEUDO_REGISTER)
    return advance_p;

  a = ira_curr_regno_allocno_map[REGNO (reg)];
  if (! reg_classes_intersect_p (cl, ALLOCNO_CLASS (a)))
    return advance_p;

  if (advance_p)
    curr_point++;

  mark_pseudo_reg_live (orig_reg, REGNO (reg));
  mark_pseudo_reg_live (orig_dreg, REGNO (dreg));
  mark_pseudo_reg_dead (orig_reg, REGNO (reg));
  mark_pseudo_reg_dead (orig_dreg, REGNO (dreg));

  return false;
}

/* Check and make if necessary conflicts for pseudo DREG of class
   DEF_CL of the current insn with input operand USE of class USE_CL.
   ORIG_DREG is the rtx actually accessed, it may be identical to
   DREG or a subreg of it.  Advance the current program point before
   making the conflict if ADVANCE_P.  Return TRUE if we will need to
   advance the current program point.  */
static bool
check_and_make_def_use_conflict (rtx dreg, rtx orig_dreg,
				 enum reg_class def_cl, int use,
				 enum reg_class use_cl, bool advance_p)
{
  if (! reg_classes_intersect_p (def_cl, use_cl))
    return advance_p;

  advance_p = make_pseudo_conflict (recog_data.operand[use],
				    use_cl, dreg, orig_dreg, advance_p);

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
			      use_cl, dreg, orig_dreg, advance_p);
  if (use >= 1
      && recog_data.constraints[use - 1][0] == '%')
    advance_p
      = make_pseudo_conflict (recog_data.operand[use - 1],
			      use_cl, dreg, orig_dreg, advance_p);
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
  rtx orig_dreg = dreg;

  if (def_cl == NO_REGS)
    return;

  if (GET_CODE (dreg) == SUBREG)
    dreg = SUBREG_REG (dreg);

  if (! REG_P (dreg) || REGNO (dreg) < FIRST_PSEUDO_REGISTER)
    return;

  a = ira_curr_regno_allocno_map[REGNO (dreg)];
  acl = ALLOCNO_CLASS (a);
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

      advance_p = check_and_make_def_use_conflict (dreg, orig_dreg, def_cl,
						   use, use_cl, advance_p);

      if ((use_match = recog_op_alt[use][alt].matches) >= 0)
	{
	  if (use_match == def)
	    continue;

	  if (recog_op_alt[use_match][alt].anything_ok)
	    use_cl = ALL_REGS;
	  else
	    use_cl = recog_op_alt[use_match][alt].cl;
	  advance_p = check_and_make_def_use_conflict (dreg, orig_dreg, def_cl,
						       use, use_cl, advance_p);
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
  int curr_alt, c;
  bool ignore_p;
  enum reg_class cl, next_cl;

  cl = NO_REGS;
  for (ignore_p = false, curr_alt = 0;
       (c = *constraints);
       constraints += CONSTRAINT_LEN (c, constraints))
    if (c == '#' || !recog_data.alternative_enabled_p[curr_alt])
      ignore_p = true;
    else if (c == ',')
      {
	curr_alt++;
	ignore_p = false;
      }
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
	  if (CONST_SCALAR_INT_P (op)
	      || (equiv_const != NULL_RTX && CONST_SCALAR_INT_P (equiv_const)))
	    return NO_REGS;
	  break;

	case 's':
	  if ((CONSTANT_P (op) && !CONST_SCALAR_INT_P (op))
	      || (equiv_const != NULL_RTX
		  && CONSTANT_P (equiv_const)
		  && !CONST_SCALAR_INT_P (equiv_const)))
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
	  if (CONST_DOUBLE_AS_FLOAT_P (op) 
	      || (GET_CODE (op) == CONST_VECTOR
		  && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT)
	      || (equiv_const != NULL_RTX
		  && (CONST_DOUBLE_AS_FLOAT_P (equiv_const)
		      || (GET_CODE (equiv_const) == CONST_VECTOR
			  && (GET_MODE_CLASS (GET_MODE (equiv_const))
			      == MODE_VECTOR_FLOAT)))))
	    return NO_REGS;
	  break;

	case 'G':
	case 'H':
	  if ((CONST_DOUBLE_AS_FLOAT_P (op) 
	       && CONST_DOUBLE_OK_FOR_CONSTRAINT_P (op, c, constraints))
	      || (equiv_const != NULL_RTX
		  && CONST_DOUBLE_AS_FLOAT_P (equiv_const) 
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
	  if (cl == NO_REGS
	      ? ira_class_singleton[next_cl][GET_MODE (op)] < 0
	      : (ira_class_singleton[cl][GET_MODE (op)]
		 != ira_class_singleton[next_cl][GET_MODE (op)]))
	    return NO_REGS;
	  cl = next_cl;
	  break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  next_cl
	    = single_reg_class (recog_data.constraints[c - '0'],
				recog_data.operand[c - '0'], NULL_RTX);
	  if (cl == NO_REGS
	      ? ira_class_singleton[next_cl][GET_MODE (op)] < 0
	      : (ira_class_singleton[cl][GET_MODE (op)]
		 != ira_class_singleton[next_cl][GET_MODE (op)]))
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
  int i, curr_alt, c, regno = 0;
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
	  for (ignore_p = false, curr_alt = 0;
	       (c = *p);
	       p += CONSTRAINT_LEN (c, p))
	    if (c == '#' || !recog_data.alternative_enabled_p[curr_alt])
	      ignore_p = true;
	    else if (c == ',')
	      {
		curr_alt++;
		ignore_p = false;
	      }
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
		  if (cl != NO_REGS)
		    {
		      /* There is no register pressure problem if all of the
			 regs in this class are fixed.  */
		      int regno = ira_class_singleton[cl][mode];
		      if (regno >= 0)
			add_to_hard_reg_set (set, mode, regno);
		    }
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
  int i, regno;
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
	  enum reg_class aclass;

	  operand_a = ira_curr_regno_allocno_map[regno];
	  aclass = ALLOCNO_CLASS (operand_a);
	  if (ira_class_subset_p[cl][aclass])
	    {
	      /* View the desired allocation of OPERAND as:

		    (REG:YMODE YREGNO),

		 a simplification of:

		    (subreg:YMODE (reg:XMODE XREGNO) OFFSET).  */
	      enum machine_mode ymode, xmode;
	      int xregno, yregno;
	      HOST_WIDE_INT offset;

	      xmode = recog_data.operand_mode[i];
	      xregno = ira_class_singleton[cl][xmode];
	      gcc_assert (xregno >= 0);
	      ymode = ALLOCNO_MODE (operand_a);
	      offset = subreg_lowpart_offset (ymode, xmode);
	      yregno = simplify_subreg_regno (xregno, xmode, offset, ymode);
	      if (yregno >= 0
		  && ira_class_hard_reg_index[aclass][yregno] >= 0)
		{
		  int cost;

		  ira_allocate_and_set_costs
		    (&ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a),
		     aclass, 0);
		  ira_init_register_move_cost_if_necessary (xmode);
		  cost = freq * (in_p
				 ? ira_register_move_cost[xmode][aclass][cl]
				 : ira_register_move_cost[xmode][cl][aclass]);
		  ALLOCNO_CONFLICT_HARD_REG_COSTS (operand_a)
		    [ira_class_hard_reg_index[aclass][yregno]] -= cost;
		}
	    }
	}

      EXECUTE_IF_SET_IN_SPARSESET (objects_live, px)
        {
	  ira_object_t obj = ira_object_id_map[px];
	  a = OBJECT_ALLOCNO (obj);
	  if (a != operand_a)
	    {
	      /* We could increase costs of A instead of making it
		 conflicting with the hard register.  But it works worse
		 because it will be spilled in reload in anyway.  */
	      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
				reg_class_contents[cl]);
	      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
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

/* Look through the CALL_INSN_FUNCTION_USAGE of a call insn INSN, and see if
   we find a SET rtx that we can use to deduce that a register can be cheaply
   caller-saved.  Return such a register, or NULL_RTX if none is found.  */
static rtx
find_call_crossed_cheap_reg (rtx insn)
{
  rtx cheap_reg = NULL_RTX;
  rtx exp = CALL_INSN_FUNCTION_USAGE (insn);

  while (exp != NULL)
    {
      rtx x = XEXP (exp, 0);
      if (GET_CODE (x) == SET)
	{
	  exp = x;
	  break;
	}
      exp = XEXP (exp, 1);
    }
  if (exp != NULL)
    {
      basic_block bb = BLOCK_FOR_INSN (insn);
      rtx reg = SET_SRC (exp);
      rtx prev = PREV_INSN (insn);
      while (prev && !(INSN_P (prev)
		       && BLOCK_FOR_INSN (prev) != bb))
	{
	  if (NONDEBUG_INSN_P (prev))
	    {
	      rtx set = single_set (prev);

	      if (set && rtx_equal_p (SET_DEST (set), reg))
		{
		  rtx src = SET_SRC (set);
		  if (!REG_P (src) || HARD_REGISTER_P (src)
		      || !pseudo_regno_single_word_and_live_p (REGNO (src)))
		    break;
		  if (!modified_between_p (src, prev, insn))
		    cheap_reg = src;
		  break;
		}
	      if (set && rtx_equal_p (SET_SRC (set), reg))
		{
		  rtx dest = SET_DEST (set);
		  if (!REG_P (dest) || HARD_REGISTER_P (dest)
		      || !pseudo_regno_single_word_and_live_p (REGNO (dest)))
		    break;
		  if (!modified_between_p (dest, prev, insn))
		    cheap_reg = dest;
		  break;
		}

	      if (reg_overlap_mentioned_p (reg, PATTERN (prev)))
		break;
	    }
	  prev = PREV_INSN (prev);
	}
    }
  return cheap_reg;
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
      for (i = 0; i < ira_pressure_classes_num; i++)
	{
	  curr_reg_pressure[ira_pressure_classes[i]] = 0;
	  high_pressure_start_point[ira_pressure_classes[i]] = -1;
	}
      curr_bb_node = loop_tree_node;
      reg_live_out = df_get_live_out (bb);
      sparseset_clear (objects_live);
      REG_SET_TO_HARD_REG_SET (hard_regs_live, reg_live_out);
      AND_COMPL_HARD_REG_SET (hard_regs_live, eliminable_regset);
      AND_COMPL_HARD_REG_SET (hard_regs_live, ira_no_alloc_regs);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (hard_regs_live, i))
	  {
	    enum reg_class aclass, pclass, cl;

	    aclass = ira_allocno_class_translate[REGNO_REG_CLASS (i)];
	    pclass = ira_pressure_class_translate[aclass];
	    for (j = 0;
		 (cl = ira_reg_class_super_classes[pclass][j])
		   != LIM_REG_CLASSES;
		 j++)
	      {
		if (! ira_reg_pressure_class_p[cl])
		  continue;
		curr_reg_pressure[cl]++;
		if (curr_bb_node->reg_pressure[cl] < curr_reg_pressure[cl])
		  curr_bb_node->reg_pressure[cl] = curr_reg_pressure[cl];
		ira_assert (curr_reg_pressure[cl]
			    <= ira_class_hard_regs_num[cl]);
	      }
	  }
      EXECUTE_IF_SET_IN_BITMAP (reg_live_out, FIRST_PSEUDO_REGISTER, j, bi)
	mark_pseudo_regno_live (j);

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
		     INSN_UID (insn), loop_tree_node->parent->loop_num,
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
	      /* Try to find a SET in the CALL_INSN_FUNCTION_USAGE, and from
		 there, try to find a pseudo that is live across the call but
		 can be cheaply reconstructed from the return value.  */
	      rtx cheap_reg = find_call_crossed_cheap_reg (insn);
	      if (cheap_reg != NULL_RTX)
		add_reg_note (insn, REG_RETURNED, cheap_reg);

	      last_call_num++;
	      sparseset_clear (allocnos_processed);
	      /* The current set of live allocnos are live across the call.  */
	      EXECUTE_IF_SET_IN_SPARSESET (objects_live, i)
	        {
		  ira_object_t obj = ira_object_id_map[i];
		  ira_allocno_t a = OBJECT_ALLOCNO (obj);
		  int num = ALLOCNO_NUM (a);

		  /* Don't allocate allocnos that cross setjmps or any
		     call, if this function receives a nonlocal
		     goto.  */
		  if (cfun->has_nonlocal_label
		      || find_reg_note (insn, REG_SETJMP,
					NULL_RTX) != NULL_RTX)
		    {
		      SET_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj));
		      SET_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
		    }
		  if (can_throw_internal (insn))
		    {
		      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
					call_used_reg_set);
		      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
					call_used_reg_set);
		    }

		  if (sparseset_bit_p (allocnos_processed, num))
		    continue;
		  sparseset_set_bit (allocnos_processed, num);

		  if (allocno_saved_at_call[num] != last_call_num)
		    /* Here we are mimicking caller-save.c behaviour
		       which does not save hard register at a call if
		       it was saved on previous call in the same basic
		       block and the hard register was not mentioned
		       between the two calls.  */
		    ALLOCNO_CALL_FREQ (a) += freq;
		  /* Mark it as saved at the next call.  */
		  allocno_saved_at_call[num] = last_call_num + 1;
		  ALLOCNO_CALLS_CROSSED_NUM (a)++;
		  if (cheap_reg != NULL_RTX
		      && ALLOCNO_REGNO (a) == (int) REGNO (cheap_reg))
		    ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a)++;
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
	    make_hard_regno_born (regno);
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
	  EXECUTE_IF_SET_IN_SPARSESET (objects_live, px)
	    {
	      ira_allocno_t a = OBJECT_ALLOCNO (ira_object_id_map[px]);

	      ALLOCNO_NO_STACK_REG_P (a) = true;
	      ALLOCNO_TOTAL_NO_STACK_REG_P (a) = true;
	    }
	  for (px = FIRST_STACK_REG; px <= LAST_STACK_REG; px++)
	    make_hard_regno_born (px);
#endif
	  /* No need to record conflicts for call clobbered regs if we
	     have nonlocal labels around, as we don't ever try to
	     allocate such regs in this case.  */
	  if (!cfun->has_nonlocal_label && bb_has_abnormal_call_pred (bb))
	    for (px = 0; px < FIRST_PSEUDO_REGISTER; px++)
	      if (call_used_regs[px])
		make_hard_regno_born (px);
	}

      EXECUTE_IF_SET_IN_SPARSESET (objects_live, i)
	make_object_dead (ira_object_id_map[i]);

      curr_point++;

    }
  /* Propagate register pressure to upper loop tree nodes: */
  if (loop_tree_node != ira_loop_tree_root)
    for (i = 0; i < ira_pressure_classes_num; i++)
      {
	enum reg_class pclass;

	pclass = ira_pressure_classes[i];
	if (loop_tree_node->reg_pressure[pclass]
	    > loop_tree_node->parent->reg_pressure[pclass])
	  loop_tree_node->parent->reg_pressure[pclass]
	    = loop_tree_node->reg_pressure[pclass];
      }
}

/* Create and set up IRA_START_POINT_RANGES and
   IRA_FINISH_POINT_RANGES.  */
static void
create_start_finish_chains (void)
{
  ira_object_t obj;
  ira_object_iterator oi;
  live_range_t r;

  ira_start_point_ranges
    = (live_range_t *) ira_allocate (ira_max_point * sizeof (live_range_t));
  memset (ira_start_point_ranges, 0, ira_max_point * sizeof (live_range_t));
  ira_finish_point_ranges
    = (live_range_t *) ira_allocate (ira_max_point * sizeof (live_range_t));
  memset (ira_finish_point_ranges, 0, ira_max_point * sizeof (live_range_t));
  FOR_EACH_OBJECT (obj, oi)
    for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
      {
	r->start_next = ira_start_point_ranges[r->start];
	ira_start_point_ranges[r->start] = r;
	r->finish_next = ira_finish_point_ranges[r->finish];
 	  ira_finish_point_ranges[r->finish] = r;
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
  ira_object_t obj;
  ira_object_iterator oi;
  live_range_t r, prev_r, next_r;
  sbitmap born_or_dead, born, dead;
  sbitmap_iterator sbi;
  bool born_p, dead_p, prev_born_p, prev_dead_p;
  
  born = sbitmap_alloc (ira_max_point);
  dead = sbitmap_alloc (ira_max_point);
  bitmap_clear (born);
  bitmap_clear (dead);
  FOR_EACH_OBJECT (obj, oi)
    for (r = OBJECT_LIVE_RANGES (obj); r != NULL; r = r->next)
      {
	ira_assert (r->start <= r->finish);
	bitmap_set_bit (born, r->start);
	bitmap_set_bit (dead, r->finish);
      }

  born_or_dead = sbitmap_alloc (ira_max_point);
  bitmap_ior (born_or_dead, born, dead);
  map = (int *) ira_allocate (sizeof (int) * ira_max_point);
  n = -1;
  prev_born_p = prev_dead_p = false;
  EXECUTE_IF_SET_IN_BITMAP (born_or_dead, 0, i, sbi)
    {
      born_p = bitmap_bit_p (born, i);
      dead_p = bitmap_bit_p (dead, i);
      if ((prev_born_p && ! prev_dead_p && born_p && ! dead_p)
	  || (prev_dead_p && ! prev_born_p && dead_p && ! born_p))
	map[i] = n;
      else
	map[i] = ++n;
      prev_born_p = born_p;
      prev_dead_p = dead_p;
    }
  sbitmap_free (born_or_dead);
  sbitmap_free (born);
  sbitmap_free (dead);
  n++;
  if (internal_flag_ira_verbose > 1 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "Compressing live ranges: from %d to %d - %d%%\n",
	     ira_max_point, n, 100 * n / ira_max_point);
  ira_max_point = n;

  FOR_EACH_OBJECT (obj, oi)
    for (r = OBJECT_LIVE_RANGES (obj), prev_r = NULL; r != NULL; r = next_r)
      {
	next_r = r->next;
	r->start = map[r->start];
	r->finish = map[r->finish];
	if (prev_r == NULL || prev_r->start > r->finish + 1)
	  {
	    prev_r = r;
	    continue;
	  }
	prev_r->start = r->start;
	prev_r->next = next_r;
	ira_finish_live_range (r);
      }

  ira_free (map);
}

/* Print live ranges R to file F.  */
void
ira_print_live_range_list (FILE *f, live_range_t r)
{
  for (; r != NULL; r = r->next)
    fprintf (f, " [%d..%d]", r->start, r->finish);
  fprintf (f, "\n");
}

/* Print live ranges R to stderr.  */
void
ira_debug_live_range_list (live_range_t r)
{
  ira_print_live_range_list (stderr, r);
}

/* Print live ranges of object OBJ to file F.  */
static void
print_object_live_ranges (FILE *f, ira_object_t obj)
{
  ira_print_live_range_list (f, OBJECT_LIVE_RANGES (obj));
}

/* Print live ranges of allocno A to file F.  */
static void
print_allocno_live_ranges (FILE *f, ira_allocno_t a)
{
  int n = ALLOCNO_NUM_OBJECTS (a);
  int i;

  for (i = 0; i < n; i++)
    {
      fprintf (f, " a%d(r%d", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
      if (n > 1)
	fprintf (f, " [%d]", i);
      fprintf (f, "):");
      print_object_live_ranges (f, ALLOCNO_OBJECT (a, i));
    }
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
   CONFLICT_HARD_REGS and TOTAL_CONFLICT_HARD_REGS for objects, and
   calculate register pressure info.  */
void
ira_create_allocno_live_ranges (void)
{
  objects_live = sparseset_alloc (ira_objects_num);
  allocnos_processed = sparseset_alloc (ira_allocnos_num);
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
  sparseset_free (objects_live);
  sparseset_free (allocnos_processed);
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
