/* IRA conflict builder.
   Copyright (C) 2006, 2007, 2008, 2009
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
#include "addresses.h"

/* This file contains code responsible for allocno conflict creation,
   allocno copy creation and allocno info accumulation on upper level
   regions.  */

/* ira_allocnos_num array of arrays of bits, recording whether two
   allocno's conflict (can't go in the same hardware register).

   Some arrays will be used as conflict bit vector of the
   corresponding allocnos see function build_allocno_conflicts.  */
static IRA_INT_TYPE **conflicts;

/* Macro to test a conflict of A1 and A2 in `conflicts'.  */
#define CONFLICT_ALLOCNO_P(A1, A2)					\
  (ALLOCNO_MIN (A1) <= ALLOCNO_CONFLICT_ID (A2)				\
   && ALLOCNO_CONFLICT_ID (A2) <= ALLOCNO_MAX (A1)			\
   && TEST_ALLOCNO_SET_BIT (conflicts[ALLOCNO_NUM (A1)],		\
	  		    ALLOCNO_CONFLICT_ID (A2),			\
			    ALLOCNO_MIN (A1),				\
			    ALLOCNO_MAX (A1)))



/* Build allocno conflict table by processing allocno live ranges.
   Return true if the table was built.  The table is not built if it
   is too big.  */
static bool
build_conflict_bit_table (void)
{
  int i, num, id, allocated_words_num, conflict_bit_vec_words_num;
  unsigned int j;
  enum reg_class cover_class;
  ira_allocno_t allocno, live_a;
  allocno_live_range_t r;
  ira_allocno_iterator ai;
  sparseset allocnos_live;
  int allocno_set_words;

  allocno_set_words = (ira_allocnos_num + IRA_INT_BITS - 1) / IRA_INT_BITS;
  allocated_words_num = 0;
  FOR_EACH_ALLOCNO (allocno, ai)
    {
      if (ALLOCNO_MAX (allocno) < ALLOCNO_MIN (allocno))
	  continue;
      conflict_bit_vec_words_num
	= ((ALLOCNO_MAX (allocno) - ALLOCNO_MIN (allocno) + IRA_INT_BITS)
	   / IRA_INT_BITS);
      allocated_words_num += conflict_bit_vec_words_num;
      if ((unsigned long long) allocated_words_num * sizeof (IRA_INT_TYPE)
	  > (unsigned long long) IRA_MAX_CONFLICT_TABLE_SIZE * 1024 * 1024)
	{
	  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
	    fprintf
	      (ira_dump_file,
	       "+++Conflict table will be too big(>%dMB) -- don't use it\n",
	       IRA_MAX_CONFLICT_TABLE_SIZE);
	  return false;
	}
    }
  allocnos_live = sparseset_alloc (ira_allocnos_num);
  conflicts = (IRA_INT_TYPE **) ira_allocate (sizeof (IRA_INT_TYPE *)
					      * ira_allocnos_num);
  allocated_words_num = 0;
  FOR_EACH_ALLOCNO (allocno, ai)
    {
      num = ALLOCNO_NUM (allocno);
      if (ALLOCNO_MAX (allocno) < ALLOCNO_MIN (allocno))
	{
	  conflicts[num] = NULL;
	  continue;
	}
      conflict_bit_vec_words_num
	= ((ALLOCNO_MAX (allocno) - ALLOCNO_MIN (allocno) + IRA_INT_BITS)
	   / IRA_INT_BITS);
      allocated_words_num += conflict_bit_vec_words_num;
      conflicts[num]
	= (IRA_INT_TYPE *) ira_allocate (sizeof (IRA_INT_TYPE)
					 * conflict_bit_vec_words_num);
      memset (conflicts[num], 0,
	      sizeof (IRA_INT_TYPE) * conflict_bit_vec_words_num);
    }
  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    fprintf
      (ira_dump_file,
       "+++Allocating %ld bytes for conflict table (uncompressed size %ld)\n",
       (long) allocated_words_num * sizeof (IRA_INT_TYPE),
       (long) allocno_set_words * ira_allocnos_num * sizeof (IRA_INT_TYPE));
  for (i = 0; i < ira_max_point; i++)
    {
      for (r = ira_start_point_ranges[i]; r != NULL; r = r->start_next)
	{
	  allocno = r->allocno;
	  num = ALLOCNO_NUM (allocno);
	  id = ALLOCNO_CONFLICT_ID (allocno);
	  cover_class = ALLOCNO_COVER_CLASS (allocno);
	  sparseset_set_bit (allocnos_live, num);
	  EXECUTE_IF_SET_IN_SPARSESET (allocnos_live, j)
	    {
	      live_a = ira_allocnos[j];
	      if (ira_reg_classes_intersect_p
		  [cover_class][ALLOCNO_COVER_CLASS (live_a)]
		  /* Don't set up conflict for the allocno with itself.  */
		  && num != (int) j)
		{
		  SET_ALLOCNO_SET_BIT (conflicts[num],
				       ALLOCNO_CONFLICT_ID (live_a),
				       ALLOCNO_MIN (allocno),
				       ALLOCNO_MAX (allocno));
		  SET_ALLOCNO_SET_BIT (conflicts[j], id,
				       ALLOCNO_MIN (live_a),
				       ALLOCNO_MAX (live_a));
		}
	    }
	}

      for (r = ira_finish_point_ranges[i]; r != NULL; r = r->finish_next)
	sparseset_clear_bit (allocnos_live, ALLOCNO_NUM (r->allocno));
    }
  sparseset_free (allocnos_live);
  return true;
}



/* Return TRUE if the operand constraint STR is commutative.  */
static bool
commutative_constraint_p (const char *str)
{
  bool ignore_p;
  int c;

  for (ignore_p = false;;)
    {
      c = *str;
      if (c == '\0')
	break;
      str += CONSTRAINT_LEN (c, str);
      if (c == '#')
	ignore_p = true;
      else if (c == ',')
	ignore_p = false;
      else if (! ignore_p)
	{
	  /* Usually `%' is the first constraint character but the
	     documentation does not require this.  */
	  if (c == '%')
	    return true;
	}
    }
  return false;
}

/* Return the number of the operand which should be the same in any
   case as operand with number OP_NUM (or negative value if there is
   no such operand).  If USE_COMMUT_OP_P is TRUE, the function makes
   temporarily commutative operand exchange before this.  The function
   takes only really possible alternatives into consideration.  */
static int
get_dup_num (int op_num, bool use_commut_op_p)
{
  int curr_alt, c, original, dup;
  bool ignore_p, commut_op_used_p;
  const char *str;
  rtx op;

  if (op_num < 0 || recog_data.n_alternatives == 0)
    return -1;
  op = recog_data.operand[op_num];
  commut_op_used_p = true;
  if (use_commut_op_p)
    {
      if (commutative_constraint_p (recog_data.constraints[op_num]))
	op_num++;
      else if (op_num > 0 && commutative_constraint_p (recog_data.constraints
						       [op_num - 1]))
	op_num--;
      else
	commut_op_used_p = false;
    }
  str = recog_data.constraints[op_num];
  for (ignore_p = false, original = -1, curr_alt = 0;;)
    {
      c = *str;
      if (c == '\0')
	break;
      if (c == '#')
	ignore_p = true;
      else if (c == ',')
	{
	  curr_alt++;
	  ignore_p = false;
	}
      else if (! ignore_p)
	switch (c)
	  {
	  case 'X':
	    return -1;

	  case 'm':
	  case 'o':
	    /* Accept a register which might be placed in memory.  */
	    return -1;
	    break;

	  case 'V':
	  case '<':
	  case '>':
	    break;

	  case 'p':
	    if (address_operand (op, VOIDmode))
	      return -1;
	    break;

	  case 'g':
	    return -1;

	  case 'r':
	  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	  case 'h': case 'j': case 'k': case 'l':
	  case 'q': case 't': case 'u':
	  case 'v': case 'w': case 'x': case 'y': case 'z':
	  case 'A': case 'B': case 'C': case 'D':
	  case 'Q': case 'R': case 'S': case 'T': case 'U':
	  case 'W': case 'Y': case 'Z':
	    {
	      enum reg_class cl;

	      cl = (c == 'r'
		    ? GENERAL_REGS : REG_CLASS_FROM_CONSTRAINT (c, str));
	      if (cl != NO_REGS)
		return -1;
#ifdef EXTRA_CONSTRAINT_STR
	      else if (EXTRA_CONSTRAINT_STR (op, c, str))
		return -1;
#endif
	      break;
	    }

	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    if (original != -1 && original != c)
	      return -1;
	    original = c;
	    break;
	  }
      str += CONSTRAINT_LEN (c, str);
    }
  if (original == -1)
    return -1;
  dup = original - '0';
  if (use_commut_op_p)
    {
      if (commutative_constraint_p (recog_data.constraints[dup]))
	dup++;
      else if (dup > 0
	       && commutative_constraint_p (recog_data.constraints[dup -1]))
	dup--;
      else if (! commut_op_used_p)
	return -1;
    }
  return dup;
}

/* Check that X is REG or SUBREG of REG.  */
#define REG_SUBREG_P(x)							\
   (REG_P (x) || (GET_CODE (x) == SUBREG && REG_P (SUBREG_REG (x))))

/* Return X if X is a REG, otherwise it should be SUBREG of REG and
   the function returns the reg in this case.  *OFFSET will be set to
   0 in the first case or the regno offset in the first case.  */
static rtx
go_through_subreg (rtx x, int *offset)
{
  rtx reg;

  *offset = 0;
  if (REG_P (x))
    return x;
  ira_assert (GET_CODE (x) == SUBREG);
  reg = SUBREG_REG (x);
  ira_assert (REG_P (reg));
  if (REGNO (reg) < FIRST_PSEUDO_REGISTER)
    *offset = subreg_regno_offset (REGNO (reg), GET_MODE (reg),
				   SUBREG_BYTE (x), GET_MODE (x));
  else
    *offset = (SUBREG_BYTE (x) / REGMODE_NATURAL_SIZE (GET_MODE (x)));
  return reg;
}

/* Process registers REG1 and REG2 in move INSN with execution
   frequency FREQ.  The function also processes the registers in a
   potential move insn (INSN == NULL in this case) with frequency
   FREQ.  The function can modify hard register costs of the
   corresponding allocnos or create a copy involving the corresponding
   allocnos.  The function does nothing if the both registers are hard
   registers.  When nothing is changed, the function returns
   FALSE.  */
static bool
process_regs_for_copy (rtx reg1, rtx reg2, bool constraint_p,
		       rtx insn, int freq)
{
  int allocno_preferenced_hard_regno, cost, index, offset1, offset2;
  bool only_regs_p;
  ira_allocno_t a;
  enum reg_class rclass, cover_class;
  enum machine_mode mode;
  ira_copy_t cp;
  ira_loop_tree_node_t parent;

  gcc_assert (REG_SUBREG_P (reg1) && REG_SUBREG_P (reg2));
  only_regs_p = REG_P (reg1) && REG_P (reg2);
  reg1 = go_through_subreg (reg1, &offset1);
  reg2 = go_through_subreg (reg2, &offset2);
  /* Set up hard regno preferenced by allocno.  If allocno gets the
     hard regno the copy (or potential move) insn will be removed.  */
  if (HARD_REGISTER_P (reg1))
    {
      if (HARD_REGISTER_P (reg2))
	return false;
      allocno_preferenced_hard_regno = REGNO (reg1) + offset1 - offset2;
      a = ira_curr_regno_allocno_map[REGNO (reg2)];
    }
  else if (HARD_REGISTER_P (reg2))
    {
      allocno_preferenced_hard_regno = REGNO (reg2) + offset2 - offset1;
      a = ira_curr_regno_allocno_map[REGNO (reg1)];
    }
  else if (!CONFLICT_ALLOCNO_P (ira_curr_regno_allocno_map[REGNO (reg1)],
				ira_curr_regno_allocno_map[REGNO (reg2)])
	   && offset1 == offset2)
    {
      cp = ira_add_allocno_copy (ira_curr_regno_allocno_map[REGNO (reg1)],
				 ira_curr_regno_allocno_map[REGNO (reg2)],
				 freq, constraint_p, insn,
				 ira_curr_loop_tree_node);
      bitmap_set_bit (ira_curr_loop_tree_node->local_copies, cp->num);
      return true;
    }
  else
    return false;
  if (! IN_RANGE (allocno_preferenced_hard_regno, 0, FIRST_PSEUDO_REGISTER - 1))
    /* Can not be tied.  */
    return false;
  rclass = REGNO_REG_CLASS (allocno_preferenced_hard_regno);
  mode = ALLOCNO_MODE (a);
  cover_class = ALLOCNO_COVER_CLASS (a);
  if (only_regs_p && insn != NULL_RTX
      && reg_class_size[rclass] <= (unsigned) CLASS_MAX_NREGS (rclass, mode))
    /* It is already taken into account in ira-costs.c.  */
    return false;
  index = ira_class_hard_reg_index[cover_class][allocno_preferenced_hard_regno];
  if (index < 0)
    /* Can not be tied.  It is not in the cover class.  */
    return false;
  if (HARD_REGISTER_P (reg1))
    cost = ira_get_register_move_cost (mode, cover_class, rclass) * freq;
  else
    cost = ira_get_register_move_cost (mode, rclass, cover_class) * freq;
  for (;;)
    {
      ira_allocate_and_set_costs
	(&ALLOCNO_HARD_REG_COSTS (a), cover_class,
	 ALLOCNO_COVER_CLASS_COST (a));
      ira_allocate_and_set_costs
	(&ALLOCNO_CONFLICT_HARD_REG_COSTS (a), cover_class, 0);
      ALLOCNO_HARD_REG_COSTS (a)[index] -= cost;
      ALLOCNO_CONFLICT_HARD_REG_COSTS (a)[index] -= cost;
      if (ALLOCNO_HARD_REG_COSTS (a)[index] < ALLOCNO_COVER_CLASS_COST (a))
	ALLOCNO_COVER_CLASS_COST (a) = ALLOCNO_HARD_REG_COSTS (a)[index];
      if (ALLOCNO_CAP (a) != NULL)
	a = ALLOCNO_CAP (a);
      else if ((parent = ALLOCNO_LOOP_TREE_NODE (a)->parent) == NULL
	       || (a = parent->regno_allocno_map[ALLOCNO_REGNO (a)]) == NULL)
	break;
    }
  return true;
}

/* Process all of the output registers of the current insn which are
   not bound (BOUND_P) and the input register REG (its operand number
   OP_NUM) which dies in the insn as if there were a move insn between
   them with frequency FREQ.  */
static void
process_reg_shuffles (rtx reg, int op_num, int freq, bool *bound_p)
{
  int i;
  rtx another_reg;

  gcc_assert (REG_SUBREG_P (reg));
  for (i = 0; i < recog_data.n_operands; i++)
    {
      another_reg = recog_data.operand[i];

      if (!REG_SUBREG_P (another_reg) || op_num == i
	  || recog_data.operand_type[i] != OP_OUT
	  || bound_p[i])
	continue;

      process_regs_for_copy (reg, another_reg, false, NULL_RTX, freq);
    }
}

/* Process INSN and create allocno copies if necessary.  For example,
   it might be because INSN is a pseudo-register move or INSN is two
   operand insn.  */
static void
add_insn_allocno_copies (rtx insn)
{
  rtx set, operand, dup;
  const char *str;
  bool commut_p, bound_p[MAX_RECOG_OPERANDS];
  int i, j, n, freq;
  
  freq = REG_FREQ_FROM_BB (BLOCK_FOR_INSN (insn));
  if (freq == 0)
    freq = 1;
  if ((set = single_set (insn)) != NULL_RTX
      && REG_SUBREG_P (SET_DEST (set)) && REG_SUBREG_P (SET_SRC (set))
      && ! side_effects_p (set)
      && find_reg_note (insn, REG_DEAD,
			REG_P (SET_SRC (set))
			? SET_SRC (set)
			: SUBREG_REG (SET_SRC (set))) != NULL_RTX)
    {
      process_regs_for_copy (SET_DEST (set), SET_SRC (set), false, insn, freq);
      return;
    }
  /* Fast check of possibility of constraint or shuffle copies.  If
     there are no dead registers, there will be no such copies.  */
  if (! find_reg_note (insn, REG_DEAD, NULL_RTX))
    return;
  extract_insn (insn);
  for (i = 0; i < recog_data.n_operands; i++)
    bound_p[i] = false;
  for (i = 0; i < recog_data.n_operands; i++)
    {
      operand = recog_data.operand[i];
      if (! REG_SUBREG_P (operand))
	continue;
      str = recog_data.constraints[i];
      while (*str == ' ' || *str == '\t')
	str++;
      for (j = 0, commut_p = false; j < 2; j++, commut_p = true)
	if ((n = get_dup_num (i, commut_p)) >= 0)
	  {
	    bound_p[n] = true;
	    dup = recog_data.operand[n];
	    if (REG_SUBREG_P (dup)
		&& find_reg_note (insn, REG_DEAD,
				  REG_P (operand)
				  ? operand
				  : SUBREG_REG (operand)) != NULL_RTX)
	      process_regs_for_copy (operand, dup, true, NULL_RTX, freq);
	  }
    }
  for (i = 0; i < recog_data.n_operands; i++)
    {
      operand = recog_data.operand[i];
      if (REG_SUBREG_P (operand)
	  && find_reg_note (insn, REG_DEAD,
			    REG_P (operand)
			    ? operand : SUBREG_REG (operand)) != NULL_RTX)
	/* If an operand dies, prefer its hard register for the output
	   operands by decreasing the hard register cost or creating
	   the corresponding allocno copies.  The cost will not
	   correspond to a real move insn cost, so make the frequency
	   smaller.  */
	process_reg_shuffles (operand, i, freq < 8 ? 1 : freq / 8, bound_p);
    }
}

/* Add copies originated from BB given by LOOP_TREE_NODE.  */
static void
add_copies (ira_loop_tree_node_t loop_tree_node)
{
  basic_block bb;
  rtx insn;

  bb = loop_tree_node->bb;
  if (bb == NULL)
    return;
  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      add_insn_allocno_copies (insn);
}

/* Propagate copies the corresponding allocnos on upper loop tree
   level.  */
static void
propagate_copies (void)
{
  ira_copy_t cp;
  ira_copy_iterator ci;
  ira_allocno_t a1, a2, parent_a1, parent_a2;
  ira_loop_tree_node_t parent;

  FOR_EACH_COPY (cp, ci)
    {
      a1 = cp->first;
      a2 = cp->second;
      if (ALLOCNO_LOOP_TREE_NODE (a1) == ira_loop_tree_root)
	continue;
      ira_assert ((ALLOCNO_LOOP_TREE_NODE (a2) != ira_loop_tree_root));
      parent = ALLOCNO_LOOP_TREE_NODE (a1)->parent;
      if ((parent_a1 = ALLOCNO_CAP (a1)) == NULL)
	parent_a1 = parent->regno_allocno_map[ALLOCNO_REGNO (a1)];
      if ((parent_a2 = ALLOCNO_CAP (a2)) == NULL)
	parent_a2 = parent->regno_allocno_map[ALLOCNO_REGNO (a2)];
      ira_assert (parent_a1 != NULL && parent_a2 != NULL);
      if (! CONFLICT_ALLOCNO_P (parent_a1, parent_a2))
	ira_add_allocno_copy (parent_a1, parent_a2, cp->freq,
			      cp->constraint_p, cp->insn, cp->loop_tree_node);
    }
}

/* Array used to collect all conflict allocnos for given allocno.  */
static ira_allocno_t *collected_conflict_allocnos;

/* Build conflict vectors or bit conflict vectors (whatever is more
   profitable) for allocno A from the conflict table and propagate the
   conflicts to upper level allocno.  */
static void
build_allocno_conflicts (ira_allocno_t a)
{
  int i, px, parent_num;
  int conflict_bit_vec_words_num;
  ira_loop_tree_node_t parent;
  ira_allocno_t parent_a, another_a, another_parent_a;
  ira_allocno_t *vec;
  IRA_INT_TYPE *allocno_conflicts;
  ira_allocno_set_iterator asi;

  allocno_conflicts = conflicts[ALLOCNO_NUM (a)];
  px = 0;
  FOR_EACH_ALLOCNO_IN_SET (allocno_conflicts,
			   ALLOCNO_MIN (a), ALLOCNO_MAX (a), i, asi)
    {
      another_a = ira_conflict_id_allocno_map[i];
      ira_assert (ira_reg_classes_intersect_p
		  [ALLOCNO_COVER_CLASS (a)][ALLOCNO_COVER_CLASS (another_a)]);
      collected_conflict_allocnos[px++] = another_a;
    }
  if (ira_conflict_vector_profitable_p (a, px))
    {
      ira_allocate_allocno_conflict_vec (a, px);
      vec = (ira_allocno_t*) ALLOCNO_CONFLICT_ALLOCNO_ARRAY (a);
      memcpy (vec, collected_conflict_allocnos, sizeof (ira_allocno_t) * px);
      vec[px] = NULL;
      ALLOCNO_CONFLICT_ALLOCNOS_NUM (a) = px;
    }
  else
    {
      ALLOCNO_CONFLICT_ALLOCNO_ARRAY (a) = conflicts[ALLOCNO_NUM (a)];
      if (ALLOCNO_MAX (a) < ALLOCNO_MIN (a))
	conflict_bit_vec_words_num = 0;
      else
	conflict_bit_vec_words_num
	  = ((ALLOCNO_MAX (a) - ALLOCNO_MIN (a) + IRA_INT_BITS)
	     / IRA_INT_BITS);
      ALLOCNO_CONFLICT_ALLOCNO_ARRAY_SIZE (a)
	= conflict_bit_vec_words_num * sizeof (IRA_INT_TYPE);
    }
  parent = ALLOCNO_LOOP_TREE_NODE (a)->parent;
  if ((parent_a = ALLOCNO_CAP (a)) == NULL
      && (parent == NULL
	  || (parent_a = parent->regno_allocno_map[ALLOCNO_REGNO (a)])
	  == NULL))
    return;
  ira_assert (parent != NULL);
  ira_assert (ALLOCNO_COVER_CLASS (a) == ALLOCNO_COVER_CLASS (parent_a));
  parent_num = ALLOCNO_NUM (parent_a);
  FOR_EACH_ALLOCNO_IN_SET (allocno_conflicts,
			   ALLOCNO_MIN (a), ALLOCNO_MAX (a), i, asi)
    {
      another_a = ira_conflict_id_allocno_map[i];
      ira_assert (ira_reg_classes_intersect_p
		  [ALLOCNO_COVER_CLASS (a)][ALLOCNO_COVER_CLASS (another_a)]);
      if ((another_parent_a = ALLOCNO_CAP (another_a)) == NULL
	  && (another_parent_a = (parent->regno_allocno_map
				  [ALLOCNO_REGNO (another_a)])) == NULL)
	continue;
      ira_assert (ALLOCNO_NUM (another_parent_a) >= 0);
      ira_assert (ALLOCNO_COVER_CLASS (another_a)
		  == ALLOCNO_COVER_CLASS (another_parent_a));
      SET_ALLOCNO_SET_BIT (conflicts[parent_num],
			   ALLOCNO_CONFLICT_ID (another_parent_a),
			   ALLOCNO_MIN (parent_a),
			   ALLOCNO_MAX (parent_a));
    }
}

/* Build conflict vectors or bit conflict vectors (whatever is more
   profitable) of all allocnos from the conflict table.  */
static void
build_conflicts (void)
{
  int i;
  ira_allocno_t a, cap;

  collected_conflict_allocnos
    = (ira_allocno_t *) ira_allocate (sizeof (ira_allocno_t)
				      * ira_allocnos_num);
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    for (a = ira_regno_allocno_map[i];
	 a != NULL;
	 a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
      {
	build_allocno_conflicts (a);
	for (cap = ALLOCNO_CAP (a); cap != NULL; cap = ALLOCNO_CAP (cap))
	  build_allocno_conflicts (cap);
      }
  ira_free (collected_conflict_allocnos);
}



/* Print hard reg set SET with TITLE to FILE.  */
static void
print_hard_reg_set (FILE *file, const char *title, HARD_REG_SET set)
{
  int i, start;

  fputs (title, file);
  for (start = -1, i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (TEST_HARD_REG_BIT (set, i))
	{
	  if (i == 0 || ! TEST_HARD_REG_BIT (set, i - 1))
	    start = i;
	}
      if (start >= 0
	  && (i == FIRST_PSEUDO_REGISTER - 1 || ! TEST_HARD_REG_BIT (set, i)))
	{
	  if (start == i - 1)
	    fprintf (file, " %d", start);
	  else if (start == i - 2)
	    fprintf (file, " %d %d", start, start + 1);
	  else
	    fprintf (file, " %d-%d", start, i - 1);
	  start = -1;
	}
    }
  putc ('\n', file);
}

/* Print information about allocno or only regno (if REG_P) conflicts
   to FILE.  */
static void
print_conflicts (FILE *file, bool reg_p)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  HARD_REG_SET conflicting_hard_regs;

  FOR_EACH_ALLOCNO (a, ai)
    {
      ira_allocno_t conflict_a;
      ira_allocno_conflict_iterator aci;
      basic_block bb;

      if (reg_p)
	fprintf (file, ";; r%d", ALLOCNO_REGNO (a));
      else
	{
	  fprintf (file, ";; a%d(r%d,", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
	  if ((bb = ALLOCNO_LOOP_TREE_NODE (a)->bb) != NULL)
	    fprintf (file, "b%d", bb->index);
	  else
	    fprintf (file, "l%d", ALLOCNO_LOOP_TREE_NODE (a)->loop->num);
	  putc (')', file);
	}
      fputs (" conflicts:", file);
      if (ALLOCNO_CONFLICT_ALLOCNO_ARRAY (a) != NULL)
	FOR_EACH_ALLOCNO_CONFLICT (a, conflict_a, aci)
	  {
	    if (reg_p)
	      fprintf (file, " r%d,", ALLOCNO_REGNO (conflict_a));
	    else
	      {
		fprintf (file, " a%d(r%d,", ALLOCNO_NUM (conflict_a),
			 ALLOCNO_REGNO (conflict_a));
		if ((bb = ALLOCNO_LOOP_TREE_NODE (conflict_a)->bb) != NULL)
		  fprintf (file, "b%d)", bb->index);
		else
		  fprintf (file, "l%d)",
			   ALLOCNO_LOOP_TREE_NODE (conflict_a)->loop->num);
	      }
	  }
      COPY_HARD_REG_SET (conflicting_hard_regs,
			 ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a));
      AND_COMPL_HARD_REG_SET (conflicting_hard_regs, ira_no_alloc_regs);
      AND_HARD_REG_SET (conflicting_hard_regs,
			reg_class_contents[ALLOCNO_COVER_CLASS (a)]);
      print_hard_reg_set (file, "\n;;     total conflict hard regs:",
			  conflicting_hard_regs);
      COPY_HARD_REG_SET (conflicting_hard_regs,
			 ALLOCNO_CONFLICT_HARD_REGS (a));
      AND_COMPL_HARD_REG_SET (conflicting_hard_regs, ira_no_alloc_regs);
      AND_HARD_REG_SET (conflicting_hard_regs,
			reg_class_contents[ALLOCNO_COVER_CLASS (a)]);
      print_hard_reg_set (file, ";;     conflict hard regs:",
			  conflicting_hard_regs);
    }
  putc ('\n', file);
}

/* Print information about allocno or only regno (if REG_P) conflicts
   to stderr.  */
void
ira_debug_conflicts (bool reg_p)
{
  print_conflicts (stderr, reg_p);
}



/* Entry function which builds allocno conflicts and allocno copies
   and accumulate some allocno info on upper level regions.  */
void
ira_build_conflicts (void)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;
  HARD_REG_SET temp_hard_reg_set;

  if (ira_conflicts_p)
    {
      ira_conflicts_p = build_conflict_bit_table ();
      if (ira_conflicts_p)
	{
	  build_conflicts ();
	  ira_traverse_loop_tree (true, ira_loop_tree_root, NULL, add_copies);
	  /* We need finished conflict table for the subsequent call.  */
	  if (flag_ira_region == IRA_REGION_ALL
	      || flag_ira_region == IRA_REGION_MIXED)
	    propagate_copies ();
	  /* Now we can free memory for the conflict table (see function
	     build_allocno_conflicts for details).  */
	  FOR_EACH_ALLOCNO (a, ai)
	    {
	      if (ALLOCNO_CONFLICT_ALLOCNO_ARRAY (a)
		  != conflicts[ALLOCNO_NUM (a)])
		ira_free (conflicts[ALLOCNO_NUM (a)]);
	    }
	  ira_free (conflicts);
	}
    }
  if (! CLASS_LIKELY_SPILLED_P (base_reg_class (VOIDmode, ADDRESS, SCRATCH)))
    CLEAR_HARD_REG_SET (temp_hard_reg_set);
  else
    {
      COPY_HARD_REG_SET (temp_hard_reg_set,
			 reg_class_contents[base_reg_class (VOIDmode, ADDRESS, SCRATCH)]);
      AND_COMPL_HARD_REG_SET (temp_hard_reg_set, ira_no_alloc_regs);
      AND_HARD_REG_SET (temp_hard_reg_set, call_used_reg_set);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      reg_attrs *attrs;
      tree decl;

      if ((! flag_caller_saves && ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	  /* For debugging purposes don't put user defined variables in
	     callee-clobbered registers.  */
	  || (optimize == 0
	      && (attrs = REG_ATTRS (regno_reg_rtx [ALLOCNO_REGNO (a)])) != NULL
	      && (decl = attrs->decl) != NULL
	      && VAR_OR_FUNCTION_DECL_P (decl)
	      && ! DECL_ARTIFICIAL (decl)))
	{
	  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a),
			    call_used_reg_set);
	  IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a),
			    call_used_reg_set);
	}
      else if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	{
	  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a),
			    no_caller_save_reg_set);
	  IOR_HARD_REG_SET (ALLOCNO_TOTAL_CONFLICT_HARD_REGS (a),
			    temp_hard_reg_set);
	  IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a),
			    no_caller_save_reg_set);
	  IOR_HARD_REG_SET (ALLOCNO_CONFLICT_HARD_REGS (a),
			    temp_hard_reg_set);
	}
    }
  if (optimize && ira_conflicts_p
      && internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_conflicts (ira_dump_file, false);
}
