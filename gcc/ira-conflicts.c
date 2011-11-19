/* IRA conflict builder.
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
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-config.h"
#include "recog.h"
#include "diagnostic-core.h"
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
   corresponding allocnos see function build_object_conflicts.  */
static IRA_INT_TYPE **conflicts;

/* Macro to test a conflict of C1 and C2 in `conflicts'.  */
#define OBJECTS_CONFLICT_P(C1, C2)					\
  (OBJECT_MIN (C1) <= OBJECT_CONFLICT_ID (C2)				\
   && OBJECT_CONFLICT_ID (C2) <= OBJECT_MAX (C1)			\
   && TEST_MINMAX_SET_BIT (conflicts[OBJECT_CONFLICT_ID (C1)],		\
			   OBJECT_CONFLICT_ID (C2),			\
			   OBJECT_MIN (C1), OBJECT_MAX (C1)))


/* Record a conflict between objects OBJ1 and OBJ2.  If necessary,
   canonicalize the conflict by recording it for lower-order subobjects
   of the corresponding allocnos. */
static void
record_object_conflict (ira_object_t obj1, ira_object_t obj2)
{
  ira_allocno_t a1 = OBJECT_ALLOCNO (obj1);
  ira_allocno_t a2 = OBJECT_ALLOCNO (obj2);
  int w1 = OBJECT_SUBWORD (obj1);
  int w2 = OBJECT_SUBWORD (obj2);
  int id1, id2;

  /* Canonicalize the conflict.  If two identically-numbered words
     conflict, always record this as a conflict between words 0.  That
     is the only information we need, and it is easier to test for if
     it is collected in each allocno's lowest-order object.  */
  if (w1 == w2 && w1 > 0)
    {
      obj1 = ALLOCNO_OBJECT (a1, 0);
      obj2 = ALLOCNO_OBJECT (a2, 0);
    }
  id1 = OBJECT_CONFLICT_ID (obj1);
  id2 = OBJECT_CONFLICT_ID (obj2);

  SET_MINMAX_SET_BIT (conflicts[id1], id2, OBJECT_MIN (obj1),
		      OBJECT_MAX (obj1));
  SET_MINMAX_SET_BIT (conflicts[id2], id1, OBJECT_MIN (obj2),
		      OBJECT_MAX (obj2));
}

/* Build allocno conflict table by processing allocno live ranges.
   Return true if the table was built.  The table is not built if it
   is too big.  */
static bool
build_conflict_bit_table (void)
{
  int i;
  unsigned int j;
  enum reg_class aclass;
  int object_set_words, allocated_words_num, conflict_bit_vec_words_num;
  live_range_t r;
  ira_allocno_t allocno;
  ira_allocno_iterator ai;
  sparseset objects_live;
  ira_object_t obj;
  ira_allocno_object_iterator aoi;

  allocated_words_num = 0;
  FOR_EACH_ALLOCNO (allocno, ai)
    FOR_EACH_ALLOCNO_OBJECT (allocno, obj, aoi)
      {
	if (OBJECT_MAX (obj) < OBJECT_MIN (obj))
	  continue;
	conflict_bit_vec_words_num
	  = ((OBJECT_MAX (obj) - OBJECT_MIN (obj) + IRA_INT_BITS)
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

  conflicts = (IRA_INT_TYPE **) ira_allocate (sizeof (IRA_INT_TYPE *)
					      * ira_objects_num);
  allocated_words_num = 0;
  FOR_EACH_ALLOCNO (allocno, ai)
    FOR_EACH_ALLOCNO_OBJECT (allocno, obj, aoi)
      {
	int id = OBJECT_CONFLICT_ID (obj);
	if (OBJECT_MAX (obj) < OBJECT_MIN (obj))
	  {
	    conflicts[id] = NULL;
	    continue;
	  }
	conflict_bit_vec_words_num
	  = ((OBJECT_MAX (obj) - OBJECT_MIN (obj) + IRA_INT_BITS)
	     / IRA_INT_BITS);
	allocated_words_num += conflict_bit_vec_words_num;
	conflicts[id]
	  = (IRA_INT_TYPE *) ira_allocate (sizeof (IRA_INT_TYPE)
					   * conflict_bit_vec_words_num);
	memset (conflicts[id], 0,
		sizeof (IRA_INT_TYPE) * conflict_bit_vec_words_num);
      }

  object_set_words = (ira_objects_num + IRA_INT_BITS - 1) / IRA_INT_BITS;
  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    fprintf
      (ira_dump_file,
       "+++Allocating %ld bytes for conflict table (uncompressed size %ld)\n",
       (long) allocated_words_num * sizeof (IRA_INT_TYPE),
       (long) object_set_words * ira_objects_num * sizeof (IRA_INT_TYPE));

  objects_live = sparseset_alloc (ira_objects_num);
  for (i = 0; i < ira_max_point; i++)
    {
      for (r = ira_start_point_ranges[i]; r != NULL; r = r->start_next)
	{
	  ira_object_t obj = r->object;
	  ira_allocno_t allocno = OBJECT_ALLOCNO (obj);
	  int id = OBJECT_CONFLICT_ID (obj);

	  gcc_assert (id < ira_objects_num);

	  aclass = ALLOCNO_CLASS (allocno);
	  sparseset_set_bit (objects_live, id);
	  EXECUTE_IF_SET_IN_SPARSESET (objects_live, j)
	    {
	      ira_object_t live_obj = ira_object_id_map[j];
	      ira_allocno_t live_a = OBJECT_ALLOCNO (live_obj);
	      enum reg_class live_aclass = ALLOCNO_CLASS (live_a);

	      if (ira_reg_classes_intersect_p[aclass][live_aclass]
		  /* Don't set up conflict for the allocno with itself.  */
		  && live_a != allocno)
		{
		  record_object_conflict (obj, live_obj);
		}
	    }
	}

      for (r = ira_finish_point_ranges[i]; r != NULL; r = r->finish_next)
	sparseset_clear_bit (objects_live, OBJECT_CONFLICT_ID (r->object));
    }
  sparseset_free (objects_live);
  return true;
}

/* Return true iff allocnos A1 and A2 cannot be allocated to the same
   register due to conflicts.  */

static bool
allocnos_conflict_for_copy_p (ira_allocno_t a1, ira_allocno_t a2)
{
  /* Due to the fact that we canonicalize conflicts (see
     record_object_conflict), we only need to test for conflicts of
     the lowest order words.  */
  ira_object_t obj1 = ALLOCNO_OBJECT (a1, 0);
  ira_object_t obj2 = ALLOCNO_OBJECT (a2, 0);

  return OBJECTS_CONFLICT_P (obj1, obj2);
}

/* Return TRUE if the operand constraint STR is commutative.  */
static bool
commutative_constraint_p (const char *str)
{
  int curr_alt, c;
  bool ignore_p;

  for (ignore_p = false, curr_alt = 0;;)
    {
      c = *str;
      if (c == '\0')
	break;
      str += CONSTRAINT_LEN (c, str);
      if (c == '#' || !recog_data.alternative_enabled_p[curr_alt])
	ignore_p = true;
      else if (c == ',')
	{
	  curr_alt++;
	  ignore_p = false;
	}
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
  reg_class_t rclass, aclass;
  enum machine_mode mode;
  ira_copy_t cp;

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
  else
    {
      ira_allocno_t a1 = ira_curr_regno_allocno_map[REGNO (reg1)];
      ira_allocno_t a2 = ira_curr_regno_allocno_map[REGNO (reg2)];
      if (!allocnos_conflict_for_copy_p (a1, a2) && offset1 == offset2)
	{
	  cp = ira_add_allocno_copy (a1, a2, freq, constraint_p, insn,
				     ira_curr_loop_tree_node);
	  bitmap_set_bit (ira_curr_loop_tree_node->local_copies, cp->num);
	  return true;
	}
      else
	return false;
    }

  if (! IN_RANGE (allocno_preferenced_hard_regno,
		  0, FIRST_PSEUDO_REGISTER - 1))
    /* Can not be tied.  */
    return false;
  rclass = REGNO_REG_CLASS (allocno_preferenced_hard_regno);
  mode = ALLOCNO_MODE (a);
  aclass = ALLOCNO_CLASS (a);
  if (only_regs_p && insn != NULL_RTX
      && reg_class_size[rclass] <= ira_reg_class_max_nregs [rclass][mode])
    /* It is already taken into account in ira-costs.c.  */
    return false;
  index = ira_class_hard_reg_index[aclass][allocno_preferenced_hard_regno];
  if (index < 0)
    /* Can not be tied.  It is not in the allocno class.  */
    return false;
  ira_init_register_move_cost_if_necessary (mode);
  if (HARD_REGISTER_P (reg1))
    cost = ira_register_move_cost[mode][aclass][rclass] * freq;
  else
    cost = ira_register_move_cost[mode][rclass][aclass] * freq;
  do
    {
      ira_allocate_and_set_costs
	(&ALLOCNO_HARD_REG_COSTS (a), aclass,
	 ALLOCNO_CLASS_COST (a));
      ira_allocate_and_set_costs
	(&ALLOCNO_CONFLICT_HARD_REG_COSTS (a), aclass, 0);
      ALLOCNO_HARD_REG_COSTS (a)[index] -= cost;
      ALLOCNO_CONFLICT_HARD_REG_COSTS (a)[index] -= cost;
      if (ALLOCNO_HARD_REG_COSTS (a)[index] < ALLOCNO_CLASS_COST (a))
	ALLOCNO_CLASS_COST (a) = ALLOCNO_HARD_REG_COSTS (a)[index];
      a = ira_parent_or_cap_allocno (a);
    }
  while (a != NULL);
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
      process_regs_for_copy (SET_DEST (set), SET_SRC (set),
			     false, insn, freq);
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

  FOR_EACH_COPY (cp, ci)
    {
      a1 = cp->first;
      a2 = cp->second;
      if (ALLOCNO_LOOP_TREE_NODE (a1) == ira_loop_tree_root)
	continue;
      ira_assert ((ALLOCNO_LOOP_TREE_NODE (a2) != ira_loop_tree_root));
      parent_a1 = ira_parent_or_cap_allocno (a1);
      parent_a2 = ira_parent_or_cap_allocno (a2);
      ira_assert (parent_a1 != NULL && parent_a2 != NULL);
      if (! allocnos_conflict_for_copy_p (parent_a1, parent_a2))
	ira_add_allocno_copy (parent_a1, parent_a2, cp->freq,
			      cp->constraint_p, cp->insn, cp->loop_tree_node);
    }
}

/* Array used to collect all conflict allocnos for given allocno.  */
static ira_object_t *collected_conflict_objects;

/* Build conflict vectors or bit conflict vectors (whatever is more
   profitable) for object OBJ from the conflict table.  */
static void
build_object_conflicts (ira_object_t obj)
{
  int i, px, parent_num;
  ira_allocno_t parent_a, another_parent_a;
  ira_object_t parent_obj;
  ira_allocno_t a = OBJECT_ALLOCNO (obj);
  IRA_INT_TYPE *object_conflicts;
  minmax_set_iterator asi;
  int parent_min, parent_max ATTRIBUTE_UNUSED;

  object_conflicts = conflicts[OBJECT_CONFLICT_ID (obj)];
  px = 0;
  FOR_EACH_BIT_IN_MINMAX_SET (object_conflicts,
			      OBJECT_MIN (obj), OBJECT_MAX (obj), i, asi)
    {
      ira_object_t another_obj = ira_object_id_map[i];
      ira_allocno_t another_a = OBJECT_ALLOCNO (obj);

      ira_assert (ira_reg_classes_intersect_p
		  [ALLOCNO_CLASS (a)][ALLOCNO_CLASS (another_a)]);
      collected_conflict_objects[px++] = another_obj;
    }
  if (ira_conflict_vector_profitable_p (obj, px))
    {
      ira_object_t *vec;
      ira_allocate_conflict_vec (obj, px);
      vec = OBJECT_CONFLICT_VEC (obj);
      memcpy (vec, collected_conflict_objects, sizeof (ira_object_t) * px);
      vec[px] = NULL;
      OBJECT_NUM_CONFLICTS (obj) = px;
    }
  else
    {
      int conflict_bit_vec_words_num;

      OBJECT_CONFLICT_ARRAY (obj) = object_conflicts;
      if (OBJECT_MAX (obj) < OBJECT_MIN (obj))
	conflict_bit_vec_words_num = 0;
      else
	conflict_bit_vec_words_num
	  = ((OBJECT_MAX (obj) - OBJECT_MIN (obj) + IRA_INT_BITS)
	     / IRA_INT_BITS);
      OBJECT_CONFLICT_ARRAY_SIZE (obj)
	= conflict_bit_vec_words_num * sizeof (IRA_INT_TYPE);
    }

  parent_a = ira_parent_or_cap_allocno (a);
  if (parent_a == NULL)
    return;
  ira_assert (ALLOCNO_CLASS (a) == ALLOCNO_CLASS (parent_a));
  ira_assert (ALLOCNO_NUM_OBJECTS (a) == ALLOCNO_NUM_OBJECTS (parent_a));
  parent_obj = ALLOCNO_OBJECT (parent_a, OBJECT_SUBWORD (obj));
  parent_num = OBJECT_CONFLICT_ID (parent_obj);
  parent_min = OBJECT_MIN (parent_obj);
  parent_max = OBJECT_MAX (parent_obj);
  FOR_EACH_BIT_IN_MINMAX_SET (object_conflicts,
			      OBJECT_MIN (obj), OBJECT_MAX (obj), i, asi)
    {
      ira_object_t another_obj = ira_object_id_map[i];
      ira_allocno_t another_a = OBJECT_ALLOCNO (another_obj);
      int another_word = OBJECT_SUBWORD (another_obj);

      ira_assert (ira_reg_classes_intersect_p
		  [ALLOCNO_CLASS (a)][ALLOCNO_CLASS (another_a)]);

      another_parent_a = ira_parent_or_cap_allocno (another_a);
      if (another_parent_a == NULL)
	continue;
      ira_assert (ALLOCNO_NUM (another_parent_a) >= 0);
      ira_assert (ALLOCNO_CLASS (another_a)
		  == ALLOCNO_CLASS (another_parent_a));
      ira_assert (ALLOCNO_NUM_OBJECTS (another_a)
		  == ALLOCNO_NUM_OBJECTS (another_parent_a));
      SET_MINMAX_SET_BIT (conflicts[parent_num],
			  OBJECT_CONFLICT_ID (ALLOCNO_OBJECT (another_parent_a,
							      another_word)),
			  parent_min, parent_max);
    }
}

/* Build conflict vectors or bit conflict vectors (whatever is more
   profitable) of all allocnos from the conflict table.  */
static void
build_conflicts (void)
{
  int i;
  ira_allocno_t a, cap;

  collected_conflict_objects
    = (ira_object_t *) ira_allocate (sizeof (ira_object_t)
					  * ira_objects_num);
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    for (a = ira_regno_allocno_map[i];
	 a != NULL;
	 a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
      {
	int j, nregs = ALLOCNO_NUM_OBJECTS (a);
	for (j = 0; j < nregs; j++)
	  {
	    ira_object_t obj = ALLOCNO_OBJECT (a, j);
	    build_object_conflicts (obj);
	    for (cap = ALLOCNO_CAP (a); cap != NULL; cap = ALLOCNO_CAP (cap))
	      {
		ira_object_t cap_obj = ALLOCNO_OBJECT (cap, j);
		gcc_assert (ALLOCNO_NUM_OBJECTS (cap) == ALLOCNO_NUM_OBJECTS (a));
		build_object_conflicts (cap_obj);
	      }
	  }
      }
  ira_free (collected_conflict_objects);
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

static void
print_allocno_conflicts (FILE * file, bool reg_p, ira_allocno_t a)
{
  HARD_REG_SET conflicting_hard_regs;
  basic_block bb;
  int n, i;

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
  n = ALLOCNO_NUM_OBJECTS (a);
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      ira_object_t conflict_obj;
      ira_object_conflict_iterator oci;

      if (OBJECT_CONFLICT_ARRAY (obj) == NULL)
	continue;
      if (n > 1)
	fprintf (file, "\n;;   subobject %d:", i);
      FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	{
	  ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	  if (reg_p)
	    fprintf (file, " r%d,", ALLOCNO_REGNO (conflict_a));
	  else
	    {
	      fprintf (file, " a%d(r%d", ALLOCNO_NUM (conflict_a),
		       ALLOCNO_REGNO (conflict_a));
	      if (ALLOCNO_NUM_OBJECTS (conflict_a) > 1)
		fprintf (file, ",w%d", OBJECT_SUBWORD (conflict_obj));
	      if ((bb = ALLOCNO_LOOP_TREE_NODE (conflict_a)->bb) != NULL)
		fprintf (file, ",b%d", bb->index);
	      else
		fprintf (file, ",l%d",
			 ALLOCNO_LOOP_TREE_NODE (conflict_a)->loop->num);
	      putc (')', file);
	    }
	}
      COPY_HARD_REG_SET (conflicting_hard_regs, OBJECT_TOTAL_CONFLICT_HARD_REGS (obj));
      AND_COMPL_HARD_REG_SET (conflicting_hard_regs, ira_no_alloc_regs);
      AND_HARD_REG_SET (conflicting_hard_regs,
			reg_class_contents[ALLOCNO_CLASS (a)]);
      print_hard_reg_set (file, "\n;;     total conflict hard regs:",
			  conflicting_hard_regs);

      COPY_HARD_REG_SET (conflicting_hard_regs, OBJECT_CONFLICT_HARD_REGS (obj));
      AND_COMPL_HARD_REG_SET (conflicting_hard_regs, ira_no_alloc_regs);
      AND_HARD_REG_SET (conflicting_hard_regs,
			reg_class_contents[ALLOCNO_CLASS (a)]);
      print_hard_reg_set (file, ";;     conflict hard regs:",
			  conflicting_hard_regs);
      putc ('\n', file);
    }

}

/* Print information about allocno or only regno (if REG_P) conflicts
   to FILE.  */
static void
print_conflicts (FILE *file, bool reg_p)
{
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    print_allocno_conflicts (file, reg_p, a);
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
  enum reg_class base;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  HARD_REG_SET temp_hard_reg_set;

  if (ira_conflicts_p)
    {
      ira_conflicts_p = build_conflict_bit_table ();
      if (ira_conflicts_p)
	{
	  ira_object_t obj;
	  ira_object_iterator oi;

	  build_conflicts ();
	  ira_traverse_loop_tree (true, ira_loop_tree_root, NULL, add_copies);
	  /* We need finished conflict table for the subsequent call.  */
	  if (flag_ira_region == IRA_REGION_ALL
	      || flag_ira_region == IRA_REGION_MIXED)
	    propagate_copies ();

	  /* Now we can free memory for the conflict table (see function
	     build_object_conflicts for details).  */
	  FOR_EACH_OBJECT (obj, oi)
	    {
	      if (OBJECT_CONFLICT_ARRAY (obj) != conflicts[OBJECT_CONFLICT_ID (obj)])
		ira_free (conflicts[OBJECT_CONFLICT_ID (obj)]);
	    }
	  ira_free (conflicts);
	}
    }
  base = base_reg_class (VOIDmode, ADDR_SPACE_GENERIC, ADDRESS, SCRATCH);
  if (! targetm.class_likely_spilled_p (base))
    CLEAR_HARD_REG_SET (temp_hard_reg_set);
  else
    {
      COPY_HARD_REG_SET (temp_hard_reg_set, reg_class_contents[base]);
      AND_COMPL_HARD_REG_SET (temp_hard_reg_set, ira_no_alloc_regs);
      AND_HARD_REG_SET (temp_hard_reg_set, call_used_reg_set);
    }
  FOR_EACH_ALLOCNO (a, ai)
    {
      int i, n = ALLOCNO_NUM_OBJECTS (a);

      for (i = 0; i < n; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);
	  reg_attrs *attrs = REG_ATTRS (regno_reg_rtx [ALLOCNO_REGNO (a)]);
	  tree decl;

	  if ((! flag_caller_saves && ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	      /* For debugging purposes don't put user defined variables in
		 callee-clobbered registers.  */
	      || (optimize == 0
		  && attrs != NULL
		  && (decl = attrs->decl) != NULL
		  && VAR_OR_FUNCTION_DECL_P (decl)
		  && ! DECL_ARTIFICIAL (decl)))
	    {
	      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
				call_used_reg_set);
	      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
				call_used_reg_set);
	    }
	  else if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	    {
	      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
				no_caller_save_reg_set);
	      IOR_HARD_REG_SET (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
				temp_hard_reg_set);
	      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
				no_caller_save_reg_set);
	      IOR_HARD_REG_SET (OBJECT_CONFLICT_HARD_REGS (obj),
				temp_hard_reg_set);
	    }

	  if (ALLOCNO_CALLS_CROSSED_NUM (a) != 0)
	    {
	      int regno;

	      /* Allocnos bigger than the saved part of call saved
		 regs must conflict with them.  */
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (!TEST_HARD_REG_BIT (call_used_reg_set, regno)
		    && HARD_REGNO_CALL_PART_CLOBBERED (regno,
						       obj->allocno->mode))
		  {
		    SET_HARD_REG_BIT (OBJECT_CONFLICT_HARD_REGS (obj), regno);
		    SET_HARD_REG_BIT (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj),
				      regno);
		  }
	    }
	}
    }
  if (optimize && ira_conflicts_p
      && internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
    print_conflicts (ira_dump_file, false);
}
