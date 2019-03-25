/* Code for RTL transformations to satisfy insn constraints.
   Copyright (C) 2010-2019 Free Software Foundation, Inc.
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


/* This file contains code for 3 passes: constraint pass,
   inheritance/split pass, and pass for undoing failed inheritance and
   split.

   The major goal of constraint pass is to transform RTL to satisfy
   insn and address constraints by:
     o choosing insn alternatives;
     o generating *reload insns* (or reloads in brief) and *reload
       pseudos* which will get necessary hard registers later;
     o substituting pseudos with equivalent values and removing the
       instructions that initialized those pseudos.

   The constraint pass has biggest and most complicated code in LRA.
   There are a lot of important details like:
     o reuse of input reload pseudos to simplify reload pseudo
       allocations;
     o some heuristics to choose insn alternative to improve the
       inheritance;
     o early clobbers etc.

   The pass is mimicking former reload pass in alternative choosing
   because the reload pass is oriented to current machine description
   model.  It might be changed if the machine description model is
   changed.

   There is special code for preventing all LRA and this pass cycling
   in case of bugs.

   On the first iteration of the pass we process every instruction and
   choose an alternative for each one.  On subsequent iterations we try
   to avoid reprocessing instructions if we can be sure that the old
   choice is still valid.

   The inheritance/spilt pass is to transform code to achieve
   ineheritance and live range splitting.  It is done on backward
   traversal of EBBs.

   The inheritance optimization goal is to reuse values in hard
   registers. There is analogous optimization in old reload pass.  The
   inheritance is achieved by following transformation:

       reload_p1 <- p	     reload_p1 <- p
       ...		     new_p <- reload_p1
       ...		=>   ...
       reload_p2 <- p	     reload_p2 <- new_p

   where p is spilled and not changed between the insns.  Reload_p1 is
   also called *original pseudo* and new_p is called *inheritance
   pseudo*.

   The subsequent assignment pass will try to assign the same (or
   another if it is not possible) hard register to new_p as to
   reload_p1 or reload_p2.

   If the assignment pass fails to assign a hard register to new_p,
   this file will undo the inheritance and restore the original code.
   This is because implementing the above sequence with a spilled
   new_p would make the code much worse.  The inheritance is done in
   EBB scope.  The above is just a simplified example to get an idea
   of the inheritance as the inheritance is also done for non-reload
   insns.

   Splitting (transformation) is also done in EBB scope on the same
   pass as the inheritance:

       r <- ... or ... <- r		 r <- ... or ... <- r
       ...				 s <- r (new insn -- save)
       ...			  =>
       ...				 r <- s (new insn -- restore)
       ... <- r				 ... <- r

    The *split pseudo* s is assigned to the hard register of the
    original pseudo or hard register r.

    Splitting is done:
      o In EBBs with high register pressure for global pseudos (living
	in at least 2 BBs) and assigned to hard registers when there
	are more one reloads needing the hard registers;
      o for pseudos needing save/restore code around calls.

    If the split pseudo still has the same hard register as the
    original pseudo after the subsequent assignment pass or the
    original pseudo was split, the opposite transformation is done on
    the same pass for undoing inheritance.  */

#undef REG_OK_STRICT

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "output.h"
#include "addresses.h"
#include "expr.h"
#include "cfgrtl.h"
#include "rtl-error.h"
#include "params.h"
#include "lra.h"
#include "lra-int.h"
#include "print-rtl.h"

/* Value of LRA_CURR_RELOAD_NUM at the beginning of BB of the current
   insn.  Remember that LRA_CURR_RELOAD_NUM is the number of emitted
   reload insns.  */
static int bb_reload_num;

/* The current insn being processed and corresponding its single set
   (NULL otherwise), its data (basic block, the insn data, the insn
   static data, and the mode of each operand).  */
static rtx_insn *curr_insn;
static rtx curr_insn_set;
static basic_block curr_bb;
static lra_insn_recog_data_t curr_id;
static struct lra_static_insn_data *curr_static_id;
static machine_mode curr_operand_mode[MAX_RECOG_OPERANDS];
/* Mode of the register substituted by its equivalence with VOIDmode
   (e.g. constant) and whose subreg is given operand of the current
   insn.  VOIDmode in all other cases.  */
static machine_mode original_subreg_reg_mode[MAX_RECOG_OPERANDS];



/* Start numbers for new registers and insns at the current constraints
   pass start.	*/
static int new_regno_start;
static int new_insn_uid_start;

/* If LOC is nonnull, strip any outer subreg from it.  */
static inline rtx *
strip_subreg (rtx *loc)
{
  return loc && GET_CODE (*loc) == SUBREG ? &SUBREG_REG (*loc) : loc;
}

/* Return hard regno of REGNO or if it is was not assigned to a hard
   register, use a hard register from its allocno class.  */
static int
get_try_hard_regno (int regno)
{
  int hard_regno;
  enum reg_class rclass;

  if ((hard_regno = regno) >= FIRST_PSEUDO_REGISTER)
    hard_regno = lra_get_regno_hard_regno (regno);
  if (hard_regno >= 0)
    return hard_regno;
  rclass = lra_get_allocno_class (regno);
  if (rclass == NO_REGS)
    return -1;
  return ira_class_hard_regs[rclass][0];
}

/* Return the hard regno of X after removing its subreg.  If X is not
   a register or a subreg of a register, return -1.  If X is a pseudo,
   use its assignment.  If FINAL_P return the final hard regno which will
   be after elimination.  */
static int
get_hard_regno (rtx x, bool final_p)
{
  rtx reg;
  int hard_regno;

  reg = x;
  if (SUBREG_P (x))
    reg = SUBREG_REG (x);
  if (! REG_P (reg))
    return -1;
  if (! HARD_REGISTER_NUM_P (hard_regno = REGNO (reg)))
    hard_regno = lra_get_regno_hard_regno (hard_regno);
  if (hard_regno < 0)
    return -1;
  if (final_p)
    hard_regno = lra_get_elimination_hard_regno (hard_regno);
  if (SUBREG_P (x))
    hard_regno += subreg_regno_offset (hard_regno, GET_MODE (reg),
				       SUBREG_BYTE (x),  GET_MODE (x));
  return hard_regno;
}

/* If REGNO is a hard register or has been allocated a hard register,
   return the class of that register.  If REGNO is a reload pseudo
   created by the current constraints pass, return its allocno class.
   Return NO_REGS otherwise.  */
static enum reg_class
get_reg_class (int regno)
{
  int hard_regno;

  if (! HARD_REGISTER_NUM_P (hard_regno = regno))
    hard_regno = lra_get_regno_hard_regno (regno);
  if (hard_regno >= 0)
    {
      hard_regno = lra_get_elimination_hard_regno (hard_regno);
      return REGNO_REG_CLASS (hard_regno);
    }
  if (regno >= new_regno_start)
    return lra_get_allocno_class (regno);
  return NO_REGS;
}

/* Return true if REG satisfies (or will satisfy) reg class constraint
   CL.  Use elimination first if REG is a hard register.  If REG is a
   reload pseudo created by this constraints pass, assume that it will
   be allocated a hard register from its allocno class, but allow that
   class to be narrowed to CL if it is currently a superset of CL.

   If NEW_CLASS is nonnull, set *NEW_CLASS to the new allocno class of
   REGNO (reg), or NO_REGS if no change in its class was needed.  */
static bool
in_class_p (rtx reg, enum reg_class cl, enum reg_class *new_class)
{
  enum reg_class rclass, common_class;
  machine_mode reg_mode;
  int class_size, hard_regno, nregs, i, j;
  int regno = REGNO (reg);

  if (new_class != NULL)
    *new_class = NO_REGS;
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      rtx final_reg = reg;
      rtx *final_loc = &final_reg;

      lra_eliminate_reg_if_possible (final_loc);
      return TEST_HARD_REG_BIT (reg_class_contents[cl], REGNO (*final_loc));
    }
  reg_mode = GET_MODE (reg);
  rclass = get_reg_class (regno);
  if (regno < new_regno_start
      /* Do not allow the constraints for reload instructions to
	 influence the classes of new pseudos.  These reloads are
	 typically moves that have many alternatives, and restricting
	 reload pseudos for one alternative may lead to situations
	 where other reload pseudos are no longer allocatable.  */
      || (INSN_UID (curr_insn) >= new_insn_uid_start
	  && curr_insn_set != NULL
	  && ((OBJECT_P (SET_SRC (curr_insn_set))
	       && ! CONSTANT_P (SET_SRC (curr_insn_set)))
	      || (GET_CODE (SET_SRC (curr_insn_set)) == SUBREG
		  && OBJECT_P (SUBREG_REG (SET_SRC (curr_insn_set)))
		  && ! CONSTANT_P (SUBREG_REG (SET_SRC (curr_insn_set)))))))
    /* When we don't know what class will be used finally for reload
       pseudos, we use ALL_REGS.  */
    return ((regno >= new_regno_start && rclass == ALL_REGS)
	    || (rclass != NO_REGS && ira_class_subset_p[rclass][cl]
		&& ! hard_reg_set_subset_p (reg_class_contents[cl],
					    lra_no_alloc_regs)));
  else
    {
      common_class = ira_reg_class_subset[rclass][cl];
      if (new_class != NULL)
	*new_class = common_class;
      if (hard_reg_set_subset_p (reg_class_contents[common_class],
				 lra_no_alloc_regs))
	return false;
      /* Check that there are enough allocatable regs.  */
      class_size = ira_class_hard_regs_num[common_class];
      for (i = 0; i < class_size; i++)
	{
	  hard_regno = ira_class_hard_regs[common_class][i];
	  nregs = hard_regno_nregs (hard_regno, reg_mode);
	  if (nregs == 1)
	    return true;
	  for (j = 0; j < nregs; j++)
	    if (TEST_HARD_REG_BIT (lra_no_alloc_regs, hard_regno + j)
		|| ! TEST_HARD_REG_BIT (reg_class_contents[common_class],
					hard_regno + j))
	      break;
	  if (j >= nregs)
	    return true;
	}
      return false;
    }
}

/* Return true if REGNO satisfies a memory constraint.	*/
static bool
in_mem_p (int regno)
{
  return get_reg_class (regno) == NO_REGS;
}

/* Return 1 if ADDR is a valid memory address for mode MODE in address
   space AS, and check that each pseudo has the proper kind of hard
   reg.	 */
static int
valid_address_p (machine_mode mode ATTRIBUTE_UNUSED,
		 rtx addr, addr_space_t as)
{
#ifdef GO_IF_LEGITIMATE_ADDRESS
  lra_assert (ADDR_SPACE_GENERIC_P (as));
  GO_IF_LEGITIMATE_ADDRESS (mode, addr, win);
  return 0;

 win:
  return 1;
#else
  return targetm.addr_space.legitimate_address_p (mode, addr, 0, as);
#endif
}

namespace {
  /* Temporarily eliminates registers in an address (for the lifetime of
     the object).  */
  class address_eliminator {
  public:
    address_eliminator (struct address_info *ad);
    ~address_eliminator ();

  private:
    struct address_info *m_ad;
    rtx *m_base_loc;
    rtx m_base_reg;
    rtx *m_index_loc;
    rtx m_index_reg;
  };
}

address_eliminator::address_eliminator (struct address_info *ad)
  : m_ad (ad),
    m_base_loc (strip_subreg (ad->base_term)),
    m_base_reg (NULL_RTX),
    m_index_loc (strip_subreg (ad->index_term)),
    m_index_reg (NULL_RTX)
{
  if (m_base_loc != NULL)
    {
      m_base_reg = *m_base_loc;
      /* If we have non-legitimate address which is decomposed not in
	 the way we expected, don't do elimination here.  In such case
	 the address will be reloaded and elimination will be done in
	 reload insn finally.  */
      if (REG_P (m_base_reg))
	lra_eliminate_reg_if_possible (m_base_loc);
      if (m_ad->base_term2 != NULL)
	*m_ad->base_term2 = *m_ad->base_term;
    }
  if (m_index_loc != NULL)
    {
      m_index_reg = *m_index_loc;
      if (REG_P (m_index_reg))
	lra_eliminate_reg_if_possible (m_index_loc);
    }
}

address_eliminator::~address_eliminator ()
{
  if (m_base_loc && *m_base_loc != m_base_reg)
    {
      *m_base_loc = m_base_reg;
      if (m_ad->base_term2 != NULL)
	*m_ad->base_term2 = *m_ad->base_term;
    }
  if (m_index_loc && *m_index_loc != m_index_reg)
    *m_index_loc = m_index_reg;
}

/* Return true if the eliminated form of AD is a legitimate target address.  */
static bool
valid_address_p (struct address_info *ad)
{
  address_eliminator eliminator (ad);
  return valid_address_p (ad->mode, *ad->outer, ad->as);
}

/* Return true if the eliminated form of memory reference OP satisfies
   extra (special) memory constraint CONSTRAINT.  */
static bool
satisfies_memory_constraint_p (rtx op, enum constraint_num constraint)
{
  struct address_info ad;

  decompose_mem_address (&ad, op);
  address_eliminator eliminator (&ad);
  return constraint_satisfied_p (op, constraint);
}

/* Return true if the eliminated form of address AD satisfies extra
   address constraint CONSTRAINT.  */
static bool
satisfies_address_constraint_p (struct address_info *ad,
				enum constraint_num constraint)
{
  address_eliminator eliminator (ad);
  return constraint_satisfied_p (*ad->outer, constraint);
}

/* Return true if the eliminated form of address OP satisfies extra
   address constraint CONSTRAINT.  */
static bool
satisfies_address_constraint_p (rtx op, enum constraint_num constraint)
{
  struct address_info ad;

  decompose_lea_address (&ad, &op);
  return satisfies_address_constraint_p (&ad, constraint);
}

/* Initiate equivalences for LRA.  As we keep original equivalences
   before any elimination, we need to make copies otherwise any change
   in insns might change the equivalences.  */
void
lra_init_equiv (void)
{
  ira_expand_reg_equiv ();
  for (int i = FIRST_PSEUDO_REGISTER; i < max_reg_num (); i++)
    {
      rtx res;

      if ((res = ira_reg_equiv[i].memory) != NULL_RTX)
	ira_reg_equiv[i].memory = copy_rtx (res);
      if ((res = ira_reg_equiv[i].invariant) != NULL_RTX)
	ira_reg_equiv[i].invariant = copy_rtx (res);
    }
}

static rtx loc_equivalence_callback (rtx, const_rtx, void *);

/* Update equivalence for REGNO.  We need to this as the equivalence
   might contain other pseudos which are changed by their
   equivalences.  */
static void
update_equiv (int regno)
{
  rtx x;
  
  if ((x = ira_reg_equiv[regno].memory) != NULL_RTX)
    ira_reg_equiv[regno].memory
      = simplify_replace_fn_rtx (x, NULL_RTX, loc_equivalence_callback,
				 NULL_RTX);
  if ((x = ira_reg_equiv[regno].invariant) != NULL_RTX)
    ira_reg_equiv[regno].invariant
      = simplify_replace_fn_rtx (x, NULL_RTX, loc_equivalence_callback,
				 NULL_RTX);
}

/* If we have decided to substitute X with another value, return that
   value, otherwise return X.  */
static rtx
get_equiv (rtx x)
{
  int regno;
  rtx res;

  if (! REG_P (x) || (regno = REGNO (x)) < FIRST_PSEUDO_REGISTER
      || ! ira_reg_equiv[regno].defined_p
      || ! ira_reg_equiv[regno].profitable_p
      || lra_get_regno_hard_regno (regno) >= 0)
    return x;
  if ((res = ira_reg_equiv[regno].memory) != NULL_RTX)
    {
      if (targetm.cannot_substitute_mem_equiv_p (res))
	return x;
      return res;
    }
  if ((res = ira_reg_equiv[regno].constant) != NULL_RTX)
    return res;
  if ((res = ira_reg_equiv[regno].invariant) != NULL_RTX)
    return res;
  gcc_unreachable ();
}

/* If we have decided to substitute X with the equivalent value,
   return that value after elimination for INSN, otherwise return
   X.  */
static rtx
get_equiv_with_elimination (rtx x, rtx_insn *insn)
{
  rtx res = get_equiv (x);

  if (x == res || CONSTANT_P (res))
    return res;
  return lra_eliminate_regs_1 (insn, res, GET_MODE (res),
			       false, false, 0, true);
}

/* Set up curr_operand_mode.  */
static void
init_curr_operand_mode (void)
{
  int nop = curr_static_id->n_operands;
  for (int i = 0; i < nop; i++)
    {
      machine_mode mode = GET_MODE (*curr_id->operand_loc[i]);
      if (mode == VOIDmode)
	{
	  /* The .md mode for address operands is the mode of the
	     addressed value rather than the mode of the address itself.  */
	  if (curr_id->icode >= 0 && curr_static_id->operand[i].is_address)
	    mode = Pmode;
	  else
	    mode = curr_static_id->operand[i].mode;
	}
      curr_operand_mode[i] = mode;
    }
}



/* The page contains code to reuse input reloads.  */

/* Structure describes input reload of the current insns.  */
struct input_reload
{
  /* True for input reload of matched operands.  */
  bool match_p;
  /* Reloaded value.  */
  rtx input;
  /* Reload pseudo used.  */
  rtx reg;
};

/* The number of elements in the following array.  */
static int curr_insn_input_reloads_num;
/* Array containing info about input reloads.  It is used to find the
   same input reload and reuse the reload pseudo in this case.	*/
static struct input_reload curr_insn_input_reloads[LRA_MAX_INSN_RELOADS];

/* Initiate data concerning reuse of input reloads for the current
   insn.  */
static void
init_curr_insn_input_reloads (void)
{
  curr_insn_input_reloads_num = 0;
}

/* Create a new pseudo using MODE, RCLASS, ORIGINAL or reuse already
   created input reload pseudo (only if TYPE is not OP_OUT).  Don't
   reuse pseudo if IN_SUBREG_P is true and the reused pseudo should be
   wrapped up in SUBREG.  The result pseudo is returned through
   RESULT_REG.  Return TRUE if we created a new pseudo, FALSE if we
   reused the already created input reload pseudo.  Use TITLE to
   describe new registers for debug purposes.  */
static bool
get_reload_reg (enum op_type type, machine_mode mode, rtx original,
		enum reg_class rclass, bool in_subreg_p,
		const char *title, rtx *result_reg)
{
  int i, regno;
  enum reg_class new_class;
  bool unique_p = false;

  if (type == OP_OUT)
    {
      *result_reg
	= lra_create_new_reg_with_unique_value (mode, original, rclass, title);
      return true;
    }
  /* Prevent reuse value of expression with side effects,
     e.g. volatile memory.  */
  if (! side_effects_p (original))
    for (i = 0; i < curr_insn_input_reloads_num; i++)
      {
	if (! curr_insn_input_reloads[i].match_p
	    && rtx_equal_p (curr_insn_input_reloads[i].input, original)
	    && in_class_p (curr_insn_input_reloads[i].reg, rclass, &new_class))
	  {
	    rtx reg = curr_insn_input_reloads[i].reg;
	    regno = REGNO (reg);
	    /* If input is equal to original and both are VOIDmode,
	       GET_MODE (reg) might be still different from mode.
	       Ensure we don't return *result_reg with wrong mode.  */
	    if (GET_MODE (reg) != mode)
	      {
		if (in_subreg_p)
		  continue;
		if (maybe_lt (GET_MODE_SIZE (GET_MODE (reg)),
			      GET_MODE_SIZE (mode)))
		  continue;
		reg = lowpart_subreg (mode, reg, GET_MODE (reg));
		if (reg == NULL_RTX || GET_CODE (reg) != SUBREG)
		  continue;
	      }
	    *result_reg = reg;
	    if (lra_dump_file != NULL)
	      {
		fprintf (lra_dump_file, "	 Reuse r%d for reload ", regno);
		dump_value_slim (lra_dump_file, original, 1);
	      }
	    if (new_class != lra_get_allocno_class (regno))
	      lra_change_class (regno, new_class, ", change to", false);
	    if (lra_dump_file != NULL)
	      fprintf (lra_dump_file, "\n");
	    return false;
	  }
	/* If we have an input reload with a different mode, make sure it
	   will get a different hard reg.  */
	else if (REG_P (original)
		 && REG_P (curr_insn_input_reloads[i].input)
		 && REGNO (original) == REGNO (curr_insn_input_reloads[i].input)
		 && (GET_MODE (original)
		     != GET_MODE (curr_insn_input_reloads[i].input)))
	  unique_p = true;
      }
  *result_reg = (unique_p
		 ? lra_create_new_reg_with_unique_value
		 : lra_create_new_reg) (mode, original, rclass, title);
  lra_assert (curr_insn_input_reloads_num < LRA_MAX_INSN_RELOADS);
  curr_insn_input_reloads[curr_insn_input_reloads_num].input = original;
  curr_insn_input_reloads[curr_insn_input_reloads_num].match_p = false;
  curr_insn_input_reloads[curr_insn_input_reloads_num++].reg = *result_reg;
  return true;
}


/* The page contains major code to choose the current insn alternative
   and generate reloads for it.	 */

/* Return the offset from REGNO of the least significant register
   in (reg:MODE REGNO).

   This function is used to tell whether two registers satisfy
   a matching constraint.  (reg:MODE1 REGNO1) matches (reg:MODE2 REGNO2) if:

         REGNO1 + lra_constraint_offset (REGNO1, MODE1)
	 == REGNO2 + lra_constraint_offset (REGNO2, MODE2)  */
int
lra_constraint_offset (int regno, machine_mode mode)
{
  lra_assert (regno < FIRST_PSEUDO_REGISTER);

  scalar_int_mode int_mode;
  if (WORDS_BIG_ENDIAN
      && is_a <scalar_int_mode> (mode, &int_mode)
      && GET_MODE_SIZE (int_mode) > UNITS_PER_WORD)
    return hard_regno_nregs (regno, mode) - 1;
  return 0;
}

/* Like rtx_equal_p except that it allows a REG and a SUBREG to match
   if they are the same hard reg, and has special hacks for
   auto-increment and auto-decrement.  This is specifically intended for
   process_alt_operands to use in determining whether two operands
   match.  X is the operand whose number is the lower of the two.

   It is supposed that X is the output operand and Y is the input
   operand.  Y_HARD_REGNO is the final hard regno of register Y or
   register in subreg Y as we know it now.  Otherwise, it is a
   negative value.  */
static bool
operands_match_p (rtx x, rtx y, int y_hard_regno)
{
  int i;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return true;
  if ((code == REG || (code == SUBREG && REG_P (SUBREG_REG (x))))
      && (REG_P (y) || (GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y)))))
    {
      int j;

      i = get_hard_regno (x, false);
      if (i < 0)
	goto slow;

      if ((j = y_hard_regno) < 0)
	goto slow;

      i += lra_constraint_offset (i, GET_MODE (x));
      j += lra_constraint_offset (j, GET_MODE (y));

      return i == j;
    }

  /* If two operands must match, because they are really a single
     operand of an assembler insn, then two post-increments are invalid
     because the assembler insn would increment only once.  On the
     other hand, a post-increment matches ordinary indexing if the
     post-increment is the output operand.  */
  if (code == POST_DEC || code == POST_INC || code == POST_MODIFY)
    return operands_match_p (XEXP (x, 0), y, y_hard_regno);

  /* Two pre-increments are invalid because the assembler insn would
     increment only once.  On the other hand, a pre-increment matches
     ordinary indexing if the pre-increment is the input operand.  */
  if (GET_CODE (y) == PRE_DEC || GET_CODE (y) == PRE_INC
      || GET_CODE (y) == PRE_MODIFY)
    return operands_match_p (x, XEXP (y, 0), -1);

 slow:

  if (code == REG && REG_P (y))
    return REGNO (x) == REGNO (y);

  if (code == REG && GET_CODE (y) == SUBREG && REG_P (SUBREG_REG (y))
      && x == SUBREG_REG (y))
    return true;
  if (GET_CODE (y) == REG && code == SUBREG && REG_P (SUBREG_REG (x))
      && SUBREG_REG (x) == y)
    return true;

  /* Now we have disposed of all the cases in which different rtx
     codes can match.  */
  if (code != GET_CODE (y))
    return false;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */
  if (GET_MODE (x) != GET_MODE (y))
    return false;

  switch (code)
    {
    CASE_CONST_UNIQUE:
      return false;

    case LABEL_REF:
      return label_ref_label (x) == label_ref_label (y);
    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    default:
      break;
    }

  /* Compare the elements.  If any pair of corresponding elements fail
     to match, return false for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int val, j;
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return false;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return false;
	  break;

	case 'p':
	  if (maybe_ne (SUBREG_BYTE (x), SUBREG_BYTE (y)))
	    return false;
	  break;

	case 'e':
	  val = operands_match_p (XEXP (x, i), XEXP (y, i), -1);
	  if (val == 0)
	    return false;
	  break;

	case '0':
	  break;

	case 'E':
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return false;
	  for (j = XVECLEN (x, i) - 1; j >= 0; --j)
	    {
	      val = operands_match_p (XVECEXP (x, i, j), XVECEXP (y, i, j), -1);
	      if (val == 0)
		return false;
	    }
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's, except for
	     within LABEL_REFs and SYMBOL_REFs.	 */
	default:
	  gcc_unreachable ();
	}
    }
  return true;
}

/* True if X is a constant that can be forced into the constant pool.
   MODE is the mode of the operand, or VOIDmode if not known.  */
#define CONST_POOL_OK_P(MODE, X)		\
  ((MODE) != VOIDmode				\
   && CONSTANT_P (X)				\
   && GET_CODE (X) != HIGH			\
   && GET_MODE_SIZE (MODE).is_constant ()	\
   && !targetm.cannot_force_const_mem (MODE, X))

/* True if C is a non-empty register class that has too few registers
   to be safely used as a reload target class.	*/
#define SMALL_REGISTER_CLASS_P(C)		\
  (ira_class_hard_regs_num [(C)] == 1		\
   || (ira_class_hard_regs_num [(C)] >= 1	\
       && targetm.class_likely_spilled_p (C)))

/* If REG is a reload pseudo, try to make its class satisfying CL.  */
static void
narrow_reload_pseudo_class (rtx reg, enum reg_class cl)
{
  enum reg_class rclass;

  /* Do not make more accurate class from reloads generated.  They are
     mostly moves with a lot of constraints.  Making more accurate
     class may results in very narrow class and impossibility of find
     registers for several reloads of one insn.	 */
  if (INSN_UID (curr_insn) >= new_insn_uid_start)
    return;
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  if (! REG_P (reg) || (int) REGNO (reg) < new_regno_start)
    return;
  if (in_class_p (reg, cl, &rclass) && rclass != cl)
    lra_change_class (REGNO (reg), rclass, "      Change to", true);
}

/* Searches X for any reference to a reg with the same value as REGNO,
   returning the rtx of the reference found if any.  Otherwise,
   returns NULL_RTX.  */
static rtx
regno_val_use_in (unsigned int regno, rtx x)
{
  const char *fmt;
  int i, j;
  rtx tem;

  if (REG_P (x) && lra_reg_info[REGNO (x)].val == lra_reg_info[regno].val)
    return x;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if ((tem = regno_val_use_in (regno, XEXP (x, i))))
	    return tem;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if ((tem = regno_val_use_in (regno , XVECEXP (x, i, j))))
	    return tem;
    }

  return NULL_RTX;
}

/* Return true if all current insn non-output operands except INS (it
   has a negaitve end marker) do not use pseudos with the same value
   as REGNO.  */
static bool
check_conflict_input_operands (int regno, signed char *ins)
{
  int in;
  int n_operands = curr_static_id->n_operands;

  for (int nop = 0; nop < n_operands; nop++)
    if (! curr_static_id->operand[nop].is_operator
	&& curr_static_id->operand[nop].type != OP_OUT)
      {
	for (int i = 0; (in = ins[i]) >= 0; i++)
	  if (in == nop)
	    break;
	if (in < 0
	    && regno_val_use_in (regno, *curr_id->operand_loc[nop]) != NULL_RTX)
	  return false;
      }
  return true;
}

/* Generate reloads for matching OUT and INS (array of input operand
   numbers with end marker -1) with reg class GOAL_CLASS, considering
   output operands OUTS (similar array to INS) needing to be in different
   registers.  Add input and output reloads correspondingly to the lists
   *BEFORE and *AFTER.  OUT might be negative.  In this case we generate
   input reloads for matched input operands INS.  EARLY_CLOBBER_P is a flag
   that the output operand is early clobbered for chosen alternative.  */
static void
match_reload (signed char out, signed char *ins, signed char *outs,
	      enum reg_class goal_class, rtx_insn **before,
	      rtx_insn **after, bool early_clobber_p)
{
  bool out_conflict;
  int i, in;
  rtx new_in_reg, new_out_reg, reg;
  machine_mode inmode, outmode;
  rtx in_rtx = *curr_id->operand_loc[ins[0]];
  rtx out_rtx = out < 0 ? in_rtx : *curr_id->operand_loc[out];

  inmode = curr_operand_mode[ins[0]];
  outmode = out < 0 ? inmode : curr_operand_mode[out];
  push_to_sequence (*before);
  if (inmode != outmode)
    {
      /* process_alt_operands has already checked that the mode sizes
	 are ordered.  */
      if (partial_subreg_p (outmode, inmode))
	{
	  reg = new_in_reg
	    = lra_create_new_reg_with_unique_value (inmode, in_rtx,
						    goal_class, "");
	  new_out_reg = gen_lowpart_SUBREG (outmode, reg);
	  LRA_SUBREG_P (new_out_reg) = 1;
	  /* If the input reg is dying here, we can use the same hard
	     register for REG and IN_RTX.  We do it only for original
	     pseudos as reload pseudos can die although original
	     pseudos still live where reload pseudos dies.  */
	  if (REG_P (in_rtx) && (int) REGNO (in_rtx) < lra_new_regno_start
	      && find_regno_note (curr_insn, REG_DEAD, REGNO (in_rtx))
	      && (!early_clobber_p
		  || check_conflict_input_operands(REGNO (in_rtx), ins)))
	    lra_assign_reg_val (REGNO (in_rtx), REGNO (reg));
	}
      else
	{
	  reg = new_out_reg
	    = lra_create_new_reg_with_unique_value (outmode, out_rtx,
						    goal_class, "");
	  new_in_reg = gen_lowpart_SUBREG (inmode, reg);
	  /* NEW_IN_REG is non-paradoxical subreg.  We don't want
	     NEW_OUT_REG living above.  We add clobber clause for
	     this.  This is just a temporary clobber.  We can remove
	     it at the end of LRA work.  */
	  rtx_insn *clobber = emit_clobber (new_out_reg);
	  LRA_TEMP_CLOBBER_P (PATTERN (clobber)) = 1;
	  LRA_SUBREG_P (new_in_reg) = 1;
	  if (GET_CODE (in_rtx) == SUBREG)
	    {
	      rtx subreg_reg = SUBREG_REG (in_rtx);
	      
	      /* If SUBREG_REG is dying here and sub-registers IN_RTX
		 and NEW_IN_REG are similar, we can use the same hard
		 register for REG and SUBREG_REG.  */
	      if (REG_P (subreg_reg)
		  && (int) REGNO (subreg_reg) < lra_new_regno_start
		  && GET_MODE (subreg_reg) == outmode
		  && known_eq (SUBREG_BYTE (in_rtx), SUBREG_BYTE (new_in_reg))
		  && find_regno_note (curr_insn, REG_DEAD, REGNO (subreg_reg))
		  && (! early_clobber_p
		      || check_conflict_input_operands (REGNO (subreg_reg),
							ins)))
		lra_assign_reg_val (REGNO (subreg_reg), REGNO (reg));
	    }
	}
    }
  else
    {
      /* Pseudos have values -- see comments for lra_reg_info.
	 Different pseudos with the same value do not conflict even if
	 they live in the same place.  When we create a pseudo we
	 assign value of original pseudo (if any) from which we
	 created the new pseudo.  If we create the pseudo from the
	 input pseudo, the new pseudo will have no conflict with the
	 input pseudo which is wrong when the input pseudo lives after
	 the insn and as the new pseudo value is changed by the insn
	 output.  Therefore we create the new pseudo from the output
	 except the case when we have single matched dying input
	 pseudo.

	 We cannot reuse the current output register because we might
	 have a situation like "a <- a op b", where the constraints
	 force the second input operand ("b") to match the output
	 operand ("a").  "b" must then be copied into a new register
	 so that it doesn't clobber the current value of "a".

	 We cannot use the same value if the output pseudo is
	 early clobbered or the input pseudo is mentioned in the
	 output, e.g. as an address part in memory, because
	 output reload will actually extend the pseudo liveness.
	 We don't care about eliminable hard regs here as we are
	 interesting only in pseudos.  */

      /* Matching input's register value is the same as one of the other
	 output operand.  Output operands in a parallel insn must be in
	 different registers.  */
      out_conflict = false;
      if (REG_P (in_rtx))
	{
	  for (i = 0; outs[i] >= 0; i++)
	    {
	      rtx other_out_rtx = *curr_id->operand_loc[outs[i]];
	      if (REG_P (other_out_rtx)
		  && (regno_val_use_in (REGNO (in_rtx), other_out_rtx)
		      != NULL_RTX))
		{
		  out_conflict = true;
		  break;
		}
	    }
	}

      new_in_reg = new_out_reg
	= (! early_clobber_p && ins[1] < 0 && REG_P (in_rtx)
	   && (int) REGNO (in_rtx) < lra_new_regno_start
	   && find_regno_note (curr_insn, REG_DEAD, REGNO (in_rtx))
	   && (! early_clobber_p
	       || check_conflict_input_operands (REGNO (in_rtx), ins))
	   && (out < 0
	       || regno_val_use_in (REGNO (in_rtx), out_rtx) == NULL_RTX)
	   && !out_conflict
	   ? lra_create_new_reg (inmode, in_rtx, goal_class, "")
	   : lra_create_new_reg_with_unique_value (outmode, out_rtx,
						   goal_class, ""));
    }
  /* In operand can be got from transformations before processing insn
     constraints.  One example of such transformations is subreg
     reloading (see function simplify_operand_subreg).  The new
     pseudos created by the transformations might have inaccurate
     class (ALL_REGS) and we should make their classes more
     accurate.  */
  narrow_reload_pseudo_class (in_rtx, goal_class);
  lra_emit_move (copy_rtx (new_in_reg), in_rtx);
  *before = get_insns ();
  end_sequence ();
  /* Add the new pseudo to consider values of subsequent input reload
     pseudos.  */
  lra_assert (curr_insn_input_reloads_num < LRA_MAX_INSN_RELOADS);
  curr_insn_input_reloads[curr_insn_input_reloads_num].input = in_rtx;
  curr_insn_input_reloads[curr_insn_input_reloads_num].match_p = true;
  curr_insn_input_reloads[curr_insn_input_reloads_num++].reg = new_in_reg;
  for (i = 0; (in = ins[i]) >= 0; i++)
    {
      lra_assert
	(GET_MODE (*curr_id->operand_loc[in]) == VOIDmode
	 || GET_MODE (new_in_reg) == GET_MODE (*curr_id->operand_loc[in]));
      *curr_id->operand_loc[in] = new_in_reg;
    }
  lra_update_dups (curr_id, ins);
  if (out < 0)
    return;
  /* See a comment for the input operand above.  */
  narrow_reload_pseudo_class (out_rtx, goal_class);
  if (find_reg_note (curr_insn, REG_UNUSED, out_rtx) == NULL_RTX)
    {
      start_sequence ();
      lra_emit_move (out_rtx, copy_rtx (new_out_reg));
      emit_insn (*after);
      *after = get_insns ();
      end_sequence ();
    }
  *curr_id->operand_loc[out] = new_out_reg;
  lra_update_dup (curr_id, out);
}

/* Return register class which is union of all reg classes in insn
   constraint alternative string starting with P.  */
static enum reg_class
reg_class_from_constraints (const char *p)
{
  int c, len;
  enum reg_class op_class = NO_REGS;

  do
    switch ((c = *p, len = CONSTRAINT_LEN (c, p)), c)
      {
      case '#':
      case ',':
	return op_class;

      case 'g':
	op_class = reg_class_subunion[op_class][GENERAL_REGS];
	break;

      default:
	enum constraint_num cn = lookup_constraint (p);
	enum reg_class cl = reg_class_for_constraint (cn);
	if (cl == NO_REGS)
	  {
	    if (insn_extra_address_constraint (cn))
	      op_class
		= (reg_class_subunion
		   [op_class][base_reg_class (VOIDmode, ADDR_SPACE_GENERIC,
					      ADDRESS, SCRATCH)]);
	    break;
	  }

	op_class = reg_class_subunion[op_class][cl];
 	break;
      }
  while ((p += len), c);
  return op_class;
}

/* If OP is a register, return the class of the register as per
   get_reg_class, otherwise return NO_REGS.  */
static inline enum reg_class
get_op_class (rtx op)
{
  return REG_P (op) ? get_reg_class (REGNO (op)) : NO_REGS;
}

/* Return generated insn mem_pseudo:=val if TO_P or val:=mem_pseudo
   otherwise.  If modes of MEM_PSEUDO and VAL are different, use
   SUBREG for VAL to make them equal.  */
static rtx_insn *
emit_spill_move (bool to_p, rtx mem_pseudo, rtx val)
{
  if (GET_MODE (mem_pseudo) != GET_MODE (val))
    {
      /* Usually size of mem_pseudo is greater than val size but in
	 rare cases it can be less as it can be defined by target
	 dependent macro HARD_REGNO_CALLER_SAVE_MODE.  */
      if (! MEM_P (val))
	{
	  val = gen_lowpart_SUBREG (GET_MODE (mem_pseudo),
				    GET_CODE (val) == SUBREG
				    ? SUBREG_REG (val) : val);
	  LRA_SUBREG_P (val) = 1;
	}
      else
	{
	  mem_pseudo = gen_lowpart_SUBREG (GET_MODE (val), mem_pseudo);
	  LRA_SUBREG_P (mem_pseudo) = 1;
	}
    }
  return to_p ? gen_move_insn (mem_pseudo, val)
	      : gen_move_insn (val, mem_pseudo);
}

/* Process a special case insn (register move), return true if we
   don't need to process it anymore.  INSN should be a single set
   insn.  Set up that RTL was changed through CHANGE_P and that hook
   TARGET_SECONDARY_MEMORY_NEEDED says to use secondary memory through
   SEC_MEM_P.  */
static bool
check_and_process_move (bool *change_p, bool *sec_mem_p ATTRIBUTE_UNUSED)
{
  int sregno, dregno;
  rtx dest, src, dreg, sreg, new_reg, scratch_reg;
  rtx_insn *before;
  enum reg_class dclass, sclass, secondary_class;
  secondary_reload_info sri;

  lra_assert (curr_insn_set != NULL_RTX);
  dreg = dest = SET_DEST (curr_insn_set);
  sreg = src = SET_SRC (curr_insn_set);
  if (GET_CODE (dest) == SUBREG)
    dreg = SUBREG_REG (dest);
  if (GET_CODE (src) == SUBREG)
    sreg = SUBREG_REG (src);
  if (! (REG_P (dreg) || MEM_P (dreg)) || ! (REG_P (sreg) || MEM_P (sreg)))
    return false;
  sclass = dclass = NO_REGS;
  if (REG_P (dreg))
    dclass = get_reg_class (REGNO (dreg));
  gcc_assert (dclass < LIM_REG_CLASSES);
  if (dclass == ALL_REGS)
    /* ALL_REGS is used for new pseudos created by transformations
       like reload of SUBREG_REG (see function
       simplify_operand_subreg).  We don't know their class yet.  We
       should figure out the class from processing the insn
       constraints not in this fast path function.  Even if ALL_REGS
       were a right class for the pseudo, secondary_... hooks usually
       are not define for ALL_REGS.  */
    return false;
  if (REG_P (sreg))
    sclass = get_reg_class (REGNO (sreg));
  gcc_assert (sclass < LIM_REG_CLASSES);
  if (sclass == ALL_REGS)
    /* See comments above.  */
    return false;
  if (sclass == NO_REGS && dclass == NO_REGS)
    return false;
  if (targetm.secondary_memory_needed (GET_MODE (src), sclass, dclass)
      && ((sclass != NO_REGS && dclass != NO_REGS)
	  || (GET_MODE (src)
	      != targetm.secondary_memory_needed_mode (GET_MODE (src)))))
    {
      *sec_mem_p = true;
      return false;
    }
  if (! REG_P (dreg) || ! REG_P (sreg))
    return false;
  sri.prev_sri = NULL;
  sri.icode = CODE_FOR_nothing;
  sri.extra_cost = 0;
  secondary_class = NO_REGS;
  /* Set up hard register for a reload pseudo for hook
     secondary_reload because some targets just ignore unassigned
     pseudos in the hook.  */
  if (dclass != NO_REGS && lra_get_regno_hard_regno (REGNO (dreg)) < 0)
    {
      dregno = REGNO (dreg);
      reg_renumber[dregno] = ira_class_hard_regs[dclass][0];
    }
  else
    dregno = -1;
  if (sclass != NO_REGS && lra_get_regno_hard_regno (REGNO (sreg)) < 0)
    {
      sregno = REGNO (sreg);
      reg_renumber[sregno] = ira_class_hard_regs[sclass][0];
    }
  else
    sregno = -1;
  if (sclass != NO_REGS)
    secondary_class
      = (enum reg_class) targetm.secondary_reload (false, dest,
						   (reg_class_t) sclass,
						   GET_MODE (src), &sri);
  if (sclass == NO_REGS
      || ((secondary_class != NO_REGS || sri.icode != CODE_FOR_nothing)
	  && dclass != NO_REGS))
    {
      enum reg_class old_sclass = secondary_class;
      secondary_reload_info old_sri = sri;

      sri.prev_sri = NULL;
      sri.icode = CODE_FOR_nothing;
      sri.extra_cost = 0;
      secondary_class
	= (enum reg_class) targetm.secondary_reload (true, src,
						     (reg_class_t) dclass,
						     GET_MODE (src), &sri);
      /* Check the target hook consistency.  */
      lra_assert
	((secondary_class == NO_REGS && sri.icode == CODE_FOR_nothing)
	 || (old_sclass == NO_REGS && old_sri.icode == CODE_FOR_nothing)
	 || (secondary_class == old_sclass && sri.icode == old_sri.icode));
    }
  if (sregno >= 0)
    reg_renumber [sregno] = -1;
  if (dregno >= 0)
    reg_renumber [dregno] = -1;
  if (secondary_class == NO_REGS && sri.icode == CODE_FOR_nothing)
    return false;
  *change_p = true;
  new_reg = NULL_RTX;
  if (secondary_class != NO_REGS)
    new_reg = lra_create_new_reg_with_unique_value (GET_MODE (src), NULL_RTX,
						    secondary_class,
						    "secondary");
  start_sequence ();
  if (sri.icode == CODE_FOR_nothing)
    lra_emit_move (new_reg, src);
  else
    {
      enum reg_class scratch_class;

      scratch_class = (reg_class_from_constraints
		       (insn_data[sri.icode].operand[2].constraint));
      scratch_reg = (lra_create_new_reg_with_unique_value
		     (insn_data[sri.icode].operand[2].mode, NULL_RTX,
		      scratch_class, "scratch"));
      emit_insn (GEN_FCN (sri.icode) (new_reg != NULL_RTX ? new_reg : dest,
				      src, scratch_reg));
    }
  before = get_insns ();
  end_sequence ();
  lra_process_new_insns (curr_insn, before, NULL, "Inserting the move");
  if (new_reg != NULL_RTX)
    SET_SRC (curr_insn_set) = new_reg;
  else
    {
      if (lra_dump_file != NULL)
	{
	  fprintf (lra_dump_file, "Deleting move %u\n", INSN_UID (curr_insn));
	  dump_insn_slim (lra_dump_file, curr_insn);
	}
      lra_set_insn_deleted (curr_insn);
      return true;
    }
  return false;
}

/* The following data describe the result of process_alt_operands.
   The data are used in curr_insn_transform to generate reloads.  */

/* The chosen reg classes which should be used for the corresponding
   operands.  */
static enum reg_class goal_alt[MAX_RECOG_OPERANDS];
/* True if the operand should be the same as another operand and that
   other operand does not need a reload.  */
static bool goal_alt_match_win[MAX_RECOG_OPERANDS];
/* True if the operand does not need a reload.	*/
static bool goal_alt_win[MAX_RECOG_OPERANDS];
/* True if the operand can be offsetable memory.  */
static bool goal_alt_offmemok[MAX_RECOG_OPERANDS];
/* The number of an operand to which given operand can be matched to.  */
static int goal_alt_matches[MAX_RECOG_OPERANDS];
/* The number of elements in the following array.  */
static int goal_alt_dont_inherit_ops_num;
/* Numbers of operands whose reload pseudos should not be inherited.  */
static int goal_alt_dont_inherit_ops[MAX_RECOG_OPERANDS];
/* True if the insn commutative operands should be swapped.  */
static bool goal_alt_swapped;
/* The chosen insn alternative.	 */
static int goal_alt_number;

/* True if the corresponding operand is the result of an equivalence
   substitution.  */
static bool equiv_substition_p[MAX_RECOG_OPERANDS];

/* The following five variables are used to choose the best insn
   alternative.	 They reflect final characteristics of the best
   alternative.	 */

/* Number of necessary reloads and overall cost reflecting the
   previous value and other unpleasantness of the best alternative.  */
static int best_losers, best_overall;
/* Overall number hard registers used for reloads.  For example, on
   some targets we need 2 general registers to reload DFmode and only
   one floating point register.	 */
static int best_reload_nregs;
/* Overall number reflecting distances of previous reloading the same
   value.  The distances are counted from the current BB start.  It is
   used to improve inheritance chances.  */
static int best_reload_sum;

/* True if the current insn should have no correspondingly input or
   output reloads.  */
static bool no_input_reloads_p, no_output_reloads_p;

/* True if we swapped the commutative operands in the current
   insn.  */
static int curr_swapped;

/* if CHECK_ONLY_P is false, arrange for address element *LOC to be a
   register of class CL.  Add any input reloads to list BEFORE.  AFTER
   is nonnull if *LOC is an automodified value; handle that case by
   adding the required output reloads to list AFTER.  Return true if
   the RTL was changed.

   if CHECK_ONLY_P is true, check that the *LOC is a correct address
   register.  Return false if the address register is correct.  */
static bool
process_addr_reg (rtx *loc, bool check_only_p, rtx_insn **before, rtx_insn **after,
		  enum reg_class cl)
{
  int regno;
  enum reg_class rclass, new_class;
  rtx reg;
  rtx new_reg;
  machine_mode mode;
  bool subreg_p, before_p = false;

  subreg_p = GET_CODE (*loc) == SUBREG;
  if (subreg_p)
    {
      reg = SUBREG_REG (*loc);
      mode = GET_MODE (reg);

      /* For mode with size bigger than ptr_mode, there unlikely to be "mov"
	 between two registers with different classes, but there normally will
	 be "mov" which transfers element of vector register into the general
	 register, and this normally will be a subreg which should be reloaded
	 as a whole.  This is particularly likely to be triggered when
	 -fno-split-wide-types specified.  */
      if (!REG_P (reg)
	  || in_class_p (reg, cl, &new_class)
	  || known_le (GET_MODE_SIZE (mode), GET_MODE_SIZE (ptr_mode)))
       loc = &SUBREG_REG (*loc);
    }

  reg = *loc;
  mode = GET_MODE (reg);
  if (! REG_P (reg))
    {
      if (check_only_p)
	return true;
      /* Always reload memory in an address even if the target supports
	 such addresses.  */
      new_reg = lra_create_new_reg_with_unique_value (mode, reg, cl, "address");
      before_p = true;
    }
  else
    {
      regno = REGNO (reg);
      rclass = get_reg_class (regno);
      if (! check_only_p
	  && (*loc = get_equiv_with_elimination (reg, curr_insn)) != reg)
	{
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file,
		       "Changing pseudo %d in address of insn %u on equiv ",
		       REGNO (reg), INSN_UID (curr_insn));
	      dump_value_slim (lra_dump_file, *loc, 1);
	      fprintf (lra_dump_file, "\n");
	    }
	  *loc = copy_rtx (*loc);
	}
      if (*loc != reg || ! in_class_p (reg, cl, &new_class))
	{
	  if (check_only_p)
	    return true;
	  reg = *loc;
	  if (get_reload_reg (after == NULL ? OP_IN : OP_INOUT,
			      mode, reg, cl, subreg_p, "address", &new_reg))
	    before_p = true;
	}
      else if (new_class != NO_REGS && rclass != new_class)
	{
	  if (check_only_p)
	    return true;
	  lra_change_class (regno, new_class, "	   Change to", true);
	  return false;
	}
      else
	return false;
    }
  if (before_p)
    {
      push_to_sequence (*before);
      lra_emit_move (new_reg, reg);
      *before = get_insns ();
      end_sequence ();
    }
  *loc = new_reg;
  if (after != NULL)
    {
      start_sequence ();
      lra_emit_move (before_p ? copy_rtx (reg) : reg, new_reg);
      emit_insn (*after);
      *after = get_insns ();
      end_sequence ();
    }
  return true;
}

/* Insert move insn in simplify_operand_subreg. BEFORE returns
   the insn to be inserted before curr insn. AFTER returns the
   the insn to be inserted after curr insn.  ORIGREG and NEWREG
   are the original reg and new reg for reload.  */
static void
insert_move_for_subreg (rtx_insn **before, rtx_insn **after, rtx origreg,
			rtx newreg)
{
  if (before)
    {
      push_to_sequence (*before);
      lra_emit_move (newreg, origreg);
      *before = get_insns ();
      end_sequence ();
    }
  if (after)
    {
      start_sequence ();
      lra_emit_move (origreg, newreg);
      emit_insn (*after);
      *after = get_insns ();
      end_sequence ();
    }
}

static int valid_address_p (machine_mode mode, rtx addr, addr_space_t as);
static bool process_address (int, bool, rtx_insn **, rtx_insn **);

/* Make reloads for subreg in operand NOP with internal subreg mode
   REG_MODE, add new reloads for further processing.  Return true if
   any change was done.  */
static bool
simplify_operand_subreg (int nop, machine_mode reg_mode)
{
  int hard_regno;
  rtx_insn *before, *after;
  machine_mode mode, innermode;
  rtx reg, new_reg;
  rtx operand = *curr_id->operand_loc[nop];
  enum reg_class regclass;
  enum op_type type;

  before = after = NULL;

  if (GET_CODE (operand) != SUBREG)
    return false;

  mode = GET_MODE (operand);
  reg = SUBREG_REG (operand);
  innermode = GET_MODE (reg);
  type = curr_static_id->operand[nop].type;
  if (MEM_P (reg))
    {
      const bool addr_was_valid
	= valid_address_p (innermode, XEXP (reg, 0), MEM_ADDR_SPACE (reg));
      alter_subreg (curr_id->operand_loc[nop], false);
      rtx subst = *curr_id->operand_loc[nop];
      lra_assert (MEM_P (subst));
      const bool addr_is_valid = valid_address_p (GET_MODE (subst),
						  XEXP (subst, 0),
						  MEM_ADDR_SPACE (subst));
      if (!addr_was_valid
	  || addr_is_valid
	  || ((get_constraint_type (lookup_constraint
				    (curr_static_id->operand[nop].constraint))
	       != CT_SPECIAL_MEMORY)
	      /* We still can reload address and if the address is
		 valid, we can remove subreg without reloading its
		 inner memory.  */
	      && valid_address_p (GET_MODE (subst),
				  regno_reg_rtx
				  [ira_class_hard_regs
				   [base_reg_class (GET_MODE (subst),
						    MEM_ADDR_SPACE (subst),
						    ADDRESS, SCRATCH)][0]],
				  MEM_ADDR_SPACE (subst))))
	{
	  /* If we change the address for a paradoxical subreg of memory, the
	     new address might violate the necessary alignment or the access
	     might be slow; take this into consideration.  We need not worry
	     about accesses beyond allocated memory for paradoxical memory
	     subregs as we don't substitute such equiv memory (see processing
	     equivalences in function lra_constraints) and because for spilled
	     pseudos we allocate stack memory enough for the biggest
	     corresponding paradoxical subreg.

	     However, do not blindly simplify a (subreg (mem ...)) for
	     WORD_REGISTER_OPERATIONS targets as this may lead to loading junk
	     data into a register when the inner is narrower than outer or
	     missing important data from memory when the inner is wider than
	     outer.  This rule only applies to modes that are no wider than
	     a word.

	     If valid memory becomes invalid after subreg elimination
	     and address might be different we still have to reload
	     memory.
	  */
	  if ((! addr_was_valid
	       || addr_is_valid
	       || known_eq (GET_MODE_SIZE (mode), GET_MODE_SIZE (innermode)))
	      && !(maybe_ne (GET_MODE_PRECISION (mode),
			     GET_MODE_PRECISION (innermode))
		   && known_le (GET_MODE_SIZE (mode), UNITS_PER_WORD)
		   && known_le (GET_MODE_SIZE (innermode), UNITS_PER_WORD)
		   && WORD_REGISTER_OPERATIONS)
	      && (!(MEM_ALIGN (subst) < GET_MODE_ALIGNMENT (mode)
		    && targetm.slow_unaligned_access (mode, MEM_ALIGN (subst)))
		  || (MEM_ALIGN (reg) < GET_MODE_ALIGNMENT (innermode)
		      && targetm.slow_unaligned_access (innermode,
							MEM_ALIGN (reg)))))
	    return true;

	  *curr_id->operand_loc[nop] = operand;

	  /* But if the address was not valid, we cannot reload the MEM without
	     reloading the address first.  */
	  if (!addr_was_valid)
	    process_address (nop, false, &before, &after);

	  /* INNERMODE is fast, MODE slow.  Reload the mem in INNERMODE.  */
	  enum reg_class rclass
	    = (enum reg_class) targetm.preferred_reload_class (reg, ALL_REGS);
	  if (get_reload_reg (curr_static_id->operand[nop].type, innermode,
			      reg, rclass, TRUE, "slow/invalid mem", &new_reg))
	    {
	      bool insert_before, insert_after;
	      bitmap_set_bit (&lra_subreg_reload_pseudos, REGNO (new_reg));

	      insert_before = (type != OP_OUT
			       || partial_subreg_p (mode, innermode));
	      insert_after = type != OP_IN;
	      insert_move_for_subreg (insert_before ? &before : NULL,
				      insert_after ? &after : NULL,
				      reg, new_reg);
	    }
	  SUBREG_REG (operand) = new_reg;

	  /* Convert to MODE.  */
	  reg = operand;
	  rclass
	    = (enum reg_class) targetm.preferred_reload_class (reg, ALL_REGS);
	  if (get_reload_reg (curr_static_id->operand[nop].type, mode, reg,
			      rclass, TRUE, "slow/invalid mem", &new_reg))
	    {
	      bool insert_before, insert_after;
	      bitmap_set_bit (&lra_subreg_reload_pseudos, REGNO (new_reg));

	      insert_before = type != OP_OUT;
	      insert_after = type != OP_IN;
	      insert_move_for_subreg (insert_before ? &before : NULL,
				      insert_after ? &after : NULL,
				      reg, new_reg);
	    }
	  *curr_id->operand_loc[nop] = new_reg;
	  lra_process_new_insns (curr_insn, before, after,
				 "Inserting slow/invalid mem reload");
	  return true;
	}

      /* If the address was valid and became invalid, prefer to reload
	 the memory.  Typical case is when the index scale should
	 correspond the memory.  */
      *curr_id->operand_loc[nop] = operand;
      /* Do not return false here as the MEM_P (reg) will be processed
	 later in this function.  */
    }
  else if (REG_P (reg) && REGNO (reg) < FIRST_PSEUDO_REGISTER)
    {
      alter_subreg (curr_id->operand_loc[nop], false);
      return true;
    }
  else if (CONSTANT_P (reg))
    {
      /* Try to simplify subreg of constant.  It is usually result of
	 equivalence substitution.  */
      if (innermode == VOIDmode
	  && (innermode = original_subreg_reg_mode[nop]) == VOIDmode)
	innermode = curr_static_id->operand[nop].mode;
      if ((new_reg = simplify_subreg (mode, reg, innermode,
				      SUBREG_BYTE (operand))) != NULL_RTX)
	{
	  *curr_id->operand_loc[nop] = new_reg;
	  return true;
	}
    }
  /* Put constant into memory when we have mixed modes.  It generates
     a better code in most cases as it does not need a secondary
     reload memory.  It also prevents LRA looping when LRA is using
     secondary reload memory again and again.  */
  if (CONSTANT_P (reg) && CONST_POOL_OK_P (reg_mode, reg)
      && SCALAR_INT_MODE_P (reg_mode) != SCALAR_INT_MODE_P (mode))
    {
      SUBREG_REG (operand) = force_const_mem (reg_mode, reg);
      alter_subreg (curr_id->operand_loc[nop], false);
      return true;
    }
  /* Force a reload of the SUBREG_REG if this is a constant or PLUS or
     if there may be a problem accessing OPERAND in the outer
     mode.  */
  if ((REG_P (reg)
       && REGNO (reg) >= FIRST_PSEUDO_REGISTER
       && (hard_regno = lra_get_regno_hard_regno (REGNO (reg))) >= 0
       /* Don't reload paradoxical subregs because we could be looping
	  having repeatedly final regno out of hard regs range.  */
       && (hard_regno_nregs (hard_regno, innermode)
	   >= hard_regno_nregs (hard_regno, mode))
       && simplify_subreg_regno (hard_regno, innermode,
				 SUBREG_BYTE (operand), mode) < 0
       /* Don't reload subreg for matching reload.  It is actually
	  valid subreg in LRA.  */
       && ! LRA_SUBREG_P (operand))
      || CONSTANT_P (reg) || GET_CODE (reg) == PLUS || MEM_P (reg))
    {
      enum reg_class rclass;

      if (REG_P (reg))
	/* There is a big probability that we will get the same class
	   for the new pseudo and we will get the same insn which
	   means infinite looping.  So spill the new pseudo.  */
	rclass = NO_REGS;
      else
	/* The class will be defined later in curr_insn_transform.  */
	rclass
	  = (enum reg_class) targetm.preferred_reload_class (reg, ALL_REGS);

      if (get_reload_reg (curr_static_id->operand[nop].type, reg_mode, reg,
			  rclass, TRUE, "subreg reg", &new_reg))
	{
	  bool insert_before, insert_after;
	  bitmap_set_bit (&lra_subreg_reload_pseudos, REGNO (new_reg));

	  insert_before = (type != OP_OUT
			   || read_modify_subreg_p (operand));
	  insert_after = (type != OP_IN);
	  insert_move_for_subreg (insert_before ? &before : NULL,
				  insert_after ? &after : NULL,
				  reg, new_reg);
	}
      SUBREG_REG (operand) = new_reg;
      lra_process_new_insns (curr_insn, before, after,
			     "Inserting subreg reload");
      return true;
    }
  /* Force a reload for a paradoxical subreg. For paradoxical subreg,
     IRA allocates hardreg to the inner pseudo reg according to its mode
     instead of the outermode, so the size of the hardreg may not be enough
     to contain the outermode operand, in that case we may need to insert
     reload for the reg. For the following two types of paradoxical subreg,
     we need to insert reload:
     1. If the op_type is OP_IN, and the hardreg could not be paired with
        other hardreg to contain the outermode operand
        (checked by in_hard_reg_set_p), we need to insert the reload.
     2. If the op_type is OP_OUT or OP_INOUT.

     Here is a paradoxical subreg example showing how the reload is generated:

     (insn 5 4 7 2 (set (reg:TI 106 [ __comp ])
        (subreg:TI (reg:DI 107 [ __comp ]) 0)) {*movti_internal_rex64}

     In IRA, reg107 is allocated to a DImode hardreg. We use x86-64 as example
     here, if reg107 is assigned to hardreg R15, because R15 is the last
     hardreg, compiler cannot find another hardreg to pair with R15 to
     contain TImode data. So we insert a TImode reload reg180 for it.
     After reload is inserted:

     (insn 283 0 0 (set (subreg:DI (reg:TI 180 [orig:107 __comp ] [107]) 0)
        (reg:DI 107 [ __comp ])) -1
     (insn 5 4 7 2 (set (reg:TI 106 [ __comp ])
        (subreg:TI (reg:TI 180 [orig:107 __comp ] [107]) 0)) {*movti_internal_rex64}

     Two reload hard registers will be allocated to reg180 to save TImode data
     in LRA_assign.

     For LRA pseudos this should normally be handled by the biggest_mode
     mechanism.  However, it's possible for new uses of an LRA pseudo
     to be introduced after we've allocated it, such as when undoing
     inheritance, and the allocated register might not then be appropriate
     for the new uses.  */
  else if (REG_P (reg)
	   && REGNO (reg) >= FIRST_PSEUDO_REGISTER
	   && (hard_regno = lra_get_regno_hard_regno (REGNO (reg))) >= 0
	   && (hard_regno_nregs (hard_regno, innermode)
	       < hard_regno_nregs (hard_regno, mode))
	   && (regclass = lra_get_allocno_class (REGNO (reg)))
	   && (type != OP_IN
	       || !in_hard_reg_set_p (reg_class_contents[regclass],
				      mode, hard_regno)
	       || overlaps_hard_reg_set_p (lra_no_alloc_regs,
					   mode, hard_regno)))
    {
      /* The class will be defined later in curr_insn_transform.  */
      enum reg_class rclass
	= (enum reg_class) targetm.preferred_reload_class (reg, ALL_REGS);

      if (get_reload_reg (curr_static_id->operand[nop].type, mode, reg,
                          rclass, TRUE, "paradoxical subreg", &new_reg))
        {
	  rtx subreg;
	  bool insert_before, insert_after;

	  PUT_MODE (new_reg, mode);
          subreg = gen_lowpart_SUBREG (innermode, new_reg);
	  bitmap_set_bit (&lra_subreg_reload_pseudos, REGNO (new_reg));

	  insert_before = (type != OP_OUT);
	  insert_after = (type != OP_IN);
	  insert_move_for_subreg (insert_before ? &before : NULL,
				  insert_after ? &after : NULL,
				  reg, subreg);
	}
      SUBREG_REG (operand) = new_reg;
      lra_process_new_insns (curr_insn, before, after,
                             "Inserting paradoxical subreg reload");
      return true;
    }
  return false;
}

/* Return TRUE if X refers for a hard register from SET.  */
static bool
uses_hard_regs_p (rtx x, HARD_REG_SET set)
{
  int i, j, x_hard_regno;
  machine_mode mode;
  const char *fmt;
  enum rtx_code code;

  if (x == NULL_RTX)
    return false;
  code = GET_CODE (x);
  mode = GET_MODE (x);

  if (code == SUBREG)
    {
      /* For all SUBREGs we want to check whether the full multi-register
	 overlaps the set.  For normal SUBREGs this means 'get_hard_regno' of
	 the inner register, for paradoxical SUBREGs this means the
	 'get_hard_regno' of the full SUBREG and for complete SUBREGs either is
	 fine.  Use the wider mode for all cases.  */
      rtx subreg = SUBREG_REG (x);
      mode = wider_subreg_mode (x);
      if (mode == GET_MODE (subreg))
	{
	  x = subreg;
	  code = GET_CODE (x);
	}
    }

  if (REG_P (x) || SUBREG_P (x))
    {
      x_hard_regno = get_hard_regno (x, true);
      return (x_hard_regno >= 0
	      && overlaps_hard_reg_set_p (set, mode, x_hard_regno));
    }
  if (MEM_P (x))
    {
      struct address_info ad;

      decompose_mem_address (&ad, x);
      if (ad.base_term != NULL && uses_hard_regs_p (*ad.base_term, set))
	return true;
      if (ad.index_term != NULL && uses_hard_regs_p (*ad.index_term, set))
	return true;
    }
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (uses_hard_regs_p (XEXP (x, i), set))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (uses_hard_regs_p (XVECEXP (x, i, j), set))
	      return true;
	}
    }
  return false;
}

/* Return true if OP is a spilled pseudo. */
static inline bool
spilled_pseudo_p (rtx op)
{
  return (REG_P (op)
	  && REGNO (op) >= FIRST_PSEUDO_REGISTER && in_mem_p (REGNO (op)));
}

/* Return true if X is a general constant.  */
static inline bool
general_constant_p (rtx x)
{
  return CONSTANT_P (x) && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (x));
}

static bool
reg_in_class_p (rtx reg, enum reg_class cl)
{
  if (cl == NO_REGS)
    return get_reg_class (REGNO (reg)) == NO_REGS;
  return in_class_p (reg, cl, NULL);
}

/* Return true if SET of RCLASS contains no hard regs which can be
   used in MODE.  */
static bool
prohibited_class_reg_set_mode_p (enum reg_class rclass,
				 HARD_REG_SET &set,
				 machine_mode mode)
{
  HARD_REG_SET temp;
  
  lra_assert (hard_reg_set_subset_p (reg_class_contents[rclass], set));
  COPY_HARD_REG_SET (temp, set);
  AND_COMPL_HARD_REG_SET (temp, lra_no_alloc_regs);
  return (hard_reg_set_subset_p
	  (temp, ira_prohibited_class_mode_regs[rclass][mode]));
}


/* Used to check validity info about small class input operands.  It
   should be incremented at start of processing an insn
   alternative.  */
static unsigned int curr_small_class_check = 0;

/* Update number of used inputs of class OP_CLASS for operand NOP.
   Return true if we have more such class operands than the number of
   available regs.  */
static bool
update_and_check_small_class_inputs (int nop, enum reg_class op_class)
{
  static unsigned int small_class_check[LIM_REG_CLASSES];
  static int small_class_input_nums[LIM_REG_CLASSES];
  
  if (SMALL_REGISTER_CLASS_P (op_class)
      /* We are interesting in classes became small because of fixing
	 some hard regs, e.g. by an user through GCC options.  */
      && hard_reg_set_intersect_p (reg_class_contents[op_class],
				   ira_no_alloc_regs)
      && (curr_static_id->operand[nop].type != OP_OUT
	  || curr_static_id->operand[nop].early_clobber))
    {
      if (small_class_check[op_class] == curr_small_class_check)
	small_class_input_nums[op_class]++;
      else
	{
	  small_class_check[op_class] = curr_small_class_check;
	  small_class_input_nums[op_class] = 1;
	}
      if (small_class_input_nums[op_class] > ira_class_hard_regs_num[op_class])
	return true;
    }
  return false;
}

/* Major function to choose the current insn alternative and what
   operands should be reloaded and how.	 If ONLY_ALTERNATIVE is not
   negative we should consider only this alternative.  Return false if
   we cannot choose the alternative or find how to reload the
   operands.  */
static bool
process_alt_operands (int only_alternative)
{
  bool ok_p = false;
  int nop, overall, nalt;
  int n_alternatives = curr_static_id->n_alternatives;
  int n_operands = curr_static_id->n_operands;
  /* LOSERS counts the operands that don't fit this alternative and
     would require loading.  */
  int losers;
  int addr_losers;
  /* REJECT is a count of how undesirable this alternative says it is
     if any reloading is required.  If the alternative matches exactly
     then REJECT is ignored, but otherwise it gets this much counted
     against it in addition to the reloading needed.  */
  int reject;
  /* This is defined by '!' or '?' alternative constraint and added to
     reject.  But in some cases it can be ignored.  */
  int static_reject;
  int op_reject;
  /* The number of elements in the following array.  */
  int early_clobbered_regs_num;
  /* Numbers of operands which are early clobber registers.  */
  int early_clobbered_nops[MAX_RECOG_OPERANDS];
  enum reg_class curr_alt[MAX_RECOG_OPERANDS];
  HARD_REG_SET curr_alt_set[MAX_RECOG_OPERANDS];
  bool curr_alt_match_win[MAX_RECOG_OPERANDS];
  bool curr_alt_win[MAX_RECOG_OPERANDS];
  bool curr_alt_offmemok[MAX_RECOG_OPERANDS];
  int curr_alt_matches[MAX_RECOG_OPERANDS];
  /* The number of elements in the following array.  */
  int curr_alt_dont_inherit_ops_num;
  /* Numbers of operands whose reload pseudos should not be inherited.	*/
  int curr_alt_dont_inherit_ops[MAX_RECOG_OPERANDS];
  rtx op;
  /* The register when the operand is a subreg of register, otherwise the
     operand itself.  */
  rtx no_subreg_reg_operand[MAX_RECOG_OPERANDS];
  /* The register if the operand is a register or subreg of register,
     otherwise NULL.  */
  rtx operand_reg[MAX_RECOG_OPERANDS];
  int hard_regno[MAX_RECOG_OPERANDS];
  machine_mode biggest_mode[MAX_RECOG_OPERANDS];
  int reload_nregs, reload_sum;
  bool costly_p;
  enum reg_class cl;

  /* Calculate some data common for all alternatives to speed up the
     function.	*/
  for (nop = 0; nop < n_operands; nop++)
    {
      rtx reg;

      op = no_subreg_reg_operand[nop] = *curr_id->operand_loc[nop];
      /* The real hard regno of the operand after the allocation.  */
      hard_regno[nop] = get_hard_regno (op, true);

      operand_reg[nop] = reg = op;
      biggest_mode[nop] = GET_MODE (op);
      if (GET_CODE (op) == SUBREG)
	{
	  biggest_mode[nop] = wider_subreg_mode (op);
	  operand_reg[nop] = reg = SUBREG_REG (op);
	}
      if (! REG_P (reg))
	operand_reg[nop] = NULL_RTX;
      else if (REGNO (reg) >= FIRST_PSEUDO_REGISTER
	       || ((int) REGNO (reg)
		   == lra_get_elimination_hard_regno (REGNO (reg))))
	no_subreg_reg_operand[nop] = reg;
      else
	operand_reg[nop] = no_subreg_reg_operand[nop]
	  /* Just use natural mode for elimination result.  It should
	     be enough for extra constraints hooks.  */
	  = regno_reg_rtx[hard_regno[nop]];
    }

  /* The constraints are made of several alternatives.	Each operand's
     constraint looks like foo,bar,... with commas separating the
     alternatives.  The first alternatives for all operands go
     together, the second alternatives go together, etc.

     First loop over alternatives.  */
  alternative_mask preferred = curr_id->preferred_alternatives;
  if (only_alternative >= 0)
    preferred &= ALTERNATIVE_BIT (only_alternative);

  for (nalt = 0; nalt < n_alternatives; nalt++)
    {
      /* Loop over operands for one constraint alternative.  */
      if (!TEST_BIT (preferred, nalt))
	continue;

      bool matching_early_clobber[MAX_RECOG_OPERANDS];
      curr_small_class_check++;
      overall = losers = addr_losers = 0;
      static_reject = reject = reload_nregs = reload_sum = 0;
      for (nop = 0; nop < n_operands; nop++)
	{
	  int inc = (curr_static_id
		     ->operand_alternative[nalt * n_operands + nop].reject);
	  if (lra_dump_file != NULL && inc != 0)
	    fprintf (lra_dump_file,
		     "            Staticly defined alt reject+=%d\n", inc);
	  static_reject += inc;
	  matching_early_clobber[nop] = 0;
	}
      reject += static_reject;
      early_clobbered_regs_num = 0;

      for (nop = 0; nop < n_operands; nop++)
	{
	  const char *p;
	  char *end;
	  int len, c, m, i, opalt_num, this_alternative_matches;
	  bool win, did_match, offmemok, early_clobber_p;
	  /* false => this operand can be reloaded somehow for this
	     alternative.  */
	  bool badop;
	  /* true => this operand can be reloaded if the alternative
	     allows regs.  */
	  bool winreg;
	  /* True if a constant forced into memory would be OK for
	     this operand.  */
	  bool constmemok;
	  enum reg_class this_alternative, this_costly_alternative;
	  HARD_REG_SET this_alternative_set, this_costly_alternative_set;
	  bool this_alternative_match_win, this_alternative_win;
	  bool this_alternative_offmemok;
	  bool scratch_p;
	  machine_mode mode;
	  enum constraint_num cn;

	  opalt_num = nalt * n_operands + nop;
	  if (curr_static_id->operand_alternative[opalt_num].anything_ok)
	    {
	      /* Fast track for no constraints at all.	*/
	      curr_alt[nop] = NO_REGS;
	      CLEAR_HARD_REG_SET (curr_alt_set[nop]);
	      curr_alt_win[nop] = true;
	      curr_alt_match_win[nop] = false;
	      curr_alt_offmemok[nop] = false;
	      curr_alt_matches[nop] = -1;
	      continue;
	    }

	  op = no_subreg_reg_operand[nop];
	  mode = curr_operand_mode[nop];

	  win = did_match = winreg = offmemok = constmemok = false;
	  badop = true;

	  early_clobber_p = false;
	  p = curr_static_id->operand_alternative[opalt_num].constraint;

	  this_costly_alternative = this_alternative = NO_REGS;
	  /* We update set of possible hard regs besides its class
	     because reg class might be inaccurate.  For example,
	     union of LO_REGS (l), HI_REGS(h), and STACK_REG(k) in ARM
	     is translated in HI_REGS because classes are merged by
	     pairs and there is no accurate intermediate class.	 */
	  CLEAR_HARD_REG_SET (this_alternative_set);
	  CLEAR_HARD_REG_SET (this_costly_alternative_set);
	  this_alternative_win = false;
	  this_alternative_match_win = false;
	  this_alternative_offmemok = false;
	  this_alternative_matches = -1;

	  /* An empty constraint should be excluded by the fast
	     track.  */
	  lra_assert (*p != 0 && *p != ',');

	  op_reject = 0;
	  /* Scan this alternative's specs for this operand; set WIN
	     if the operand fits any letter in this alternative.
	     Otherwise, clear BADOP if this operand could fit some
	     letter after reloads, or set WINREG if this operand could
	     fit after reloads provided the constraint allows some
	     registers.	 */
	  costly_p = false;
	  do
	    {
	      switch ((c = *p, len = CONSTRAINT_LEN (c, p)), c)
		{
		case '\0':
		  len = 0;
		  break;
		case ',':
		  c = '\0';
		  break;

		case '&':
		  early_clobber_p = true;
		  break;

		case '$':
		  op_reject += LRA_MAX_REJECT;
		  break;
		case '^':
		  op_reject += LRA_LOSER_COST_FACTOR;
		  break;

		case '#':
		  /* Ignore rest of this alternative.  */
		  c = '\0';
		  break;

		case '0':  case '1':  case '2':	 case '3':  case '4':
		case '5':  case '6':  case '7':	 case '8':  case '9':
		  {
		    int m_hregno;
		    bool match_p;

		    m = strtoul (p, &end, 10);
		    p = end;
		    len = 0;
		    lra_assert (nop > m);

		    /* Reject matches if we don't know which operand is
		       bigger.  This situation would arguably be a bug in
		       an .md pattern, but could also occur in a user asm.  */
		    if (!ordered_p (GET_MODE_SIZE (biggest_mode[m]),
				    GET_MODE_SIZE (biggest_mode[nop])))
		      break;

		    /* Don't match wrong asm insn operands for proper
		       diagnostic later.  */
		    if (INSN_CODE (curr_insn) < 0
			&& (curr_operand_mode[m] == BLKmode
			    || curr_operand_mode[nop] == BLKmode)
			&& curr_operand_mode[m] != curr_operand_mode[nop])
		      break;
		    
		    m_hregno = get_hard_regno (*curr_id->operand_loc[m], false);
		    /* We are supposed to match a previous operand.
		       If we do, we win if that one did.  If we do
		       not, count both of the operands as losers.
		       (This is too conservative, since most of the
		       time only a single reload insn will be needed
		       to make the two operands win.  As a result,
		       this alternative may be rejected when it is
		       actually desirable.)  */
		    match_p = false;
		    if (operands_match_p (*curr_id->operand_loc[nop],
					  *curr_id->operand_loc[m], m_hregno))
		      {
			/* We should reject matching of an early
			   clobber operand if the matching operand is
			   not dying in the insn.  */
			if (! curr_static_id->operand[m].early_clobber
			    || operand_reg[nop] == NULL_RTX
			    || (find_regno_note (curr_insn, REG_DEAD,
						 REGNO (op))
				|| REGNO (op) == REGNO (operand_reg[m])))
			  match_p = true;
		      }
		    if (match_p)
		      {
			/* If we are matching a non-offsettable
			   address where an offsettable address was
			   expected, then we must reject this
			   combination, because we can't reload
			   it.	*/
			if (curr_alt_offmemok[m]
			    && MEM_P (*curr_id->operand_loc[m])
			    && curr_alt[m] == NO_REGS && ! curr_alt_win[m])
			  continue;
		      }
		    else
		      {
			/* Operands don't match.  If the operands are
			   different user defined explicit hard registers,
			   then we cannot make them match.  */
			if ((REG_P (*curr_id->operand_loc[nop])
			     || SUBREG_P (*curr_id->operand_loc[nop]))
			    && (REG_P (*curr_id->operand_loc[m])
				|| SUBREG_P (*curr_id->operand_loc[m])))
			  {
			    rtx nop_reg = *curr_id->operand_loc[nop];
			    if (SUBREG_P (nop_reg))
			      nop_reg = SUBREG_REG (nop_reg);
			    rtx m_reg = *curr_id->operand_loc[m];
			    if (SUBREG_P (m_reg))
			      m_reg = SUBREG_REG (m_reg);

			    if (REG_P (nop_reg)
				&& HARD_REGISTER_P (nop_reg)
				&& REG_USERVAR_P (nop_reg)
				&& REG_P (m_reg)
				&& HARD_REGISTER_P (m_reg)
				&& REG_USERVAR_P (m_reg))
			      break;
			  }

			/* Both operands must allow a reload register,
			   otherwise we cannot make them match.  */
			if (curr_alt[m] == NO_REGS)
			  break;
			/* Retroactively mark the operand we had to
			   match as a loser, if it wasn't already and
			   it wasn't matched to a register constraint
			   (e.g it might be matched by memory). */
			if (curr_alt_win[m]
			    && (operand_reg[m] == NULL_RTX
				|| hard_regno[m] < 0))
			  {
			    losers++;
			    reload_nregs
			      += (ira_reg_class_max_nregs[curr_alt[m]]
				  [GET_MODE (*curr_id->operand_loc[m])]);
			  }

			/* Prefer matching earlyclobber alternative as
			   it results in less hard regs required for
			   the insn than a non-matching earlyclobber
			   alternative.  */
			if (curr_static_id->operand[m].early_clobber)
			  {
			    if (lra_dump_file != NULL)
			      fprintf
				(lra_dump_file,
				 "            %d Matching earlyclobber alt:"
				 " reject--\n",
				 nop);
			    if (!matching_early_clobber[m])
			      {
				reject--;
				matching_early_clobber[m] = 1;
			      }
			  }
			/* Otherwise we prefer no matching
			   alternatives because it gives more freedom
			   in RA.  */
			else if (operand_reg[nop] == NULL_RTX
				 || (find_regno_note (curr_insn, REG_DEAD,
						      REGNO (operand_reg[nop]))
				     == NULL_RTX))
			  {
			    if (lra_dump_file != NULL)
			      fprintf
				(lra_dump_file,
				 "            %d Matching alt: reject+=2\n",
				 nop);
			    reject += 2;
			  }
		      }
		    /* If we have to reload this operand and some
		       previous operand also had to match the same
		       thing as this operand, we don't know how to do
		       that.  */
		    if (!match_p || !curr_alt_win[m])
		      {
			for (i = 0; i < nop; i++)
			  if (curr_alt_matches[i] == m)
			    break;
			if (i < nop)
			  break;
		      }
		    else
		      did_match = true;

		    this_alternative_matches = m;
		    /* This can be fixed with reloads if the operand
		       we are supposed to match can be fixed with
		       reloads. */
		    badop = false;
		    this_alternative = curr_alt[m];
		    COPY_HARD_REG_SET (this_alternative_set, curr_alt_set[m]);
		    winreg = this_alternative != NO_REGS;
		    break;
		  }

		case 'g':
		  if (MEM_P (op)
		      || general_constant_p (op)
		      || spilled_pseudo_p (op))
		    win = true;
		  cl = GENERAL_REGS;
		  goto reg;

		default:
		  cn = lookup_constraint (p);
		  switch (get_constraint_type (cn))
		    {
		    case CT_REGISTER:
		      cl = reg_class_for_constraint (cn);
		      if (cl != NO_REGS)
			goto reg;
		      break;

		    case CT_CONST_INT:
		      if (CONST_INT_P (op)
			  && insn_const_int_ok_for_constraint (INTVAL (op), cn))
			win = true;
		      break;

		    case CT_MEMORY:
		      if (MEM_P (op)
			  && satisfies_memory_constraint_p (op, cn))
			win = true;
		      else if (spilled_pseudo_p (op))
			win = true;

		      /* If we didn't already win, we can reload constants
			 via force_const_mem or put the pseudo value into
			 memory, or make other memory by reloading the
			 address like for 'o'.  */
		      if (CONST_POOL_OK_P (mode, op)
			  || MEM_P (op) || REG_P (op)
			  /* We can restore the equiv insn by a
			     reload.  */
			  || equiv_substition_p[nop])
			badop = false;
		      constmemok = true;
		      offmemok = true;
		      break;

		    case CT_ADDRESS:
		      /* An asm operand with an address constraint
			 that doesn't satisfy address_operand has
			 is_address cleared, so that we don't try to
			 make a non-address fit.  */
		      if (!curr_static_id->operand[nop].is_address)
			break;
		      /* If we didn't already win, we can reload the address
			 into a base register.  */
		      if (satisfies_address_constraint_p (op, cn))
			win = true;
		      cl = base_reg_class (VOIDmode, ADDR_SPACE_GENERIC,
					   ADDRESS, SCRATCH);
		      badop = false;
		      goto reg;

		    case CT_FIXED_FORM:
		      if (constraint_satisfied_p (op, cn))
			win = true;
		      break;

		    case CT_SPECIAL_MEMORY:
		      if (MEM_P (op)
			  && satisfies_memory_constraint_p (op, cn))
			win = true;
		      else if (spilled_pseudo_p (op))
			win = true;
		      break;
		    }
		  break;

		reg:
		  if (mode == BLKmode)
		    break;
		  this_alternative = reg_class_subunion[this_alternative][cl];
		  IOR_HARD_REG_SET (this_alternative_set,
				    reg_class_contents[cl]);
		  if (costly_p)
		    {
		      this_costly_alternative
			= reg_class_subunion[this_costly_alternative][cl];
		      IOR_HARD_REG_SET (this_costly_alternative_set,
					reg_class_contents[cl]);
		    }
		  winreg = true;
		  if (REG_P (op))
		    {
		      if (hard_regno[nop] >= 0
			  && in_hard_reg_set_p (this_alternative_set,
						mode, hard_regno[nop]))
			win = true;
		      else if (hard_regno[nop] < 0
			       && in_class_p (op, this_alternative, NULL))
			win = true;
		    }
		  break;
		}
	      if (c != ' ' && c != '\t')
		costly_p = c == '*';
	    }
	  while ((p += len), c);

	  scratch_p = (operand_reg[nop] != NULL_RTX
		       && lra_former_scratch_p (REGNO (operand_reg[nop])));
	  /* Record which operands fit this alternative.  */
	  if (win)
	    {
	      this_alternative_win = true;
	      if (operand_reg[nop] != NULL_RTX)
		{
		  if (hard_regno[nop] >= 0)
		    {
		      if (in_hard_reg_set_p (this_costly_alternative_set,
					     mode, hard_regno[nop]))
			{
			  if (lra_dump_file != NULL)
			    fprintf (lra_dump_file,
				     "            %d Costly set: reject++\n",
				     nop);
			  reject++;
			}
		    }
		  else
		    {
		      /* Prefer won reg to spilled pseudo under other
			 equal conditions for possibe inheritance.  */
		      if (! scratch_p)
			{
			  if (lra_dump_file != NULL)
			    fprintf
			      (lra_dump_file,
			       "            %d Non pseudo reload: reject++\n",
			       nop);
			  reject++;
			}
		      if (in_class_p (operand_reg[nop],
				      this_costly_alternative, NULL))
			{
			  if (lra_dump_file != NULL)
			    fprintf
			      (lra_dump_file,
			       "            %d Non pseudo costly reload:"
			       " reject++\n",
			       nop);
			  reject++;
			}
		    }
		  /* We simulate the behavior of old reload here.
		     Although scratches need hard registers and it
		     might result in spilling other pseudos, no reload
		     insns are generated for the scratches.  So it
		     might cost something but probably less than old
		     reload pass believes.  */
		  if (scratch_p)
		    {
		      if (lra_dump_file != NULL)
			fprintf (lra_dump_file,
				 "            %d Scratch win: reject+=2\n",
				 nop);
		      reject += 2;
		    }
		}
	    }
	  else if (did_match)
	    this_alternative_match_win = true;
	  else
	    {
	      int const_to_mem = 0;
	      bool no_regs_p;

	      reject += op_reject;
	      /* Never do output reload of stack pointer.  It makes
		 impossible to do elimination when SP is changed in
		 RTL.  */
	      if (op == stack_pointer_rtx && ! frame_pointer_needed
		  && curr_static_id->operand[nop].type != OP_IN)
		goto fail;

	      /* If this alternative asks for a specific reg class, see if there
		 is at least one allocatable register in that class.  */
	      no_regs_p
		= (this_alternative == NO_REGS
		   || (hard_reg_set_subset_p
		       (reg_class_contents[this_alternative],
			lra_no_alloc_regs)));

	      /* For asms, verify that the class for this alternative is possible
		 for the mode that is specified.  */
	      if (!no_regs_p && INSN_CODE (curr_insn) < 0)
		{
		  int i;
		  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		    if (targetm.hard_regno_mode_ok (i, mode)
			&& in_hard_reg_set_p (reg_class_contents[this_alternative],
					      mode, i))
		      break;
		  if (i == FIRST_PSEUDO_REGISTER)
		    winreg = false;
		}

	      /* If this operand accepts a register, and if the
		 register class has at least one allocatable register,
		 then this operand can be reloaded.  */
	      if (winreg && !no_regs_p)
		badop = false;

	      if (badop)
		{
		  if (lra_dump_file != NULL)
		    fprintf (lra_dump_file,
			     "            alt=%d: Bad operand -- refuse\n",
			     nalt);
		  goto fail;
		}

	      if (this_alternative != NO_REGS)
		{
		  HARD_REG_SET available_regs;
		  
		  COPY_HARD_REG_SET (available_regs,
				     reg_class_contents[this_alternative]);
		  AND_COMPL_HARD_REG_SET
		    (available_regs,
		     ira_prohibited_class_mode_regs[this_alternative][mode]);
		  AND_COMPL_HARD_REG_SET (available_regs, lra_no_alloc_regs);
		  if (hard_reg_set_empty_p (available_regs))
		    {
		      /* There are no hard regs holding a value of given
			 mode.  */
		      if (offmemok)
			{
			  this_alternative = NO_REGS;
			  if (lra_dump_file != NULL)
			    fprintf (lra_dump_file,
				     "            %d Using memory because of"
				     " a bad mode: reject+=2\n",
				     nop);
			  reject += 2;
			}
		      else
			{
			  if (lra_dump_file != NULL)
			    fprintf (lra_dump_file,
				     "            alt=%d: Wrong mode -- refuse\n",
				     nalt);
			  goto fail;
			}
		    }
		}

	      /* If not assigned pseudo has a class which a subset of
		 required reg class, it is a less costly alternative
		 as the pseudo still can get a hard reg of necessary
		 class.  */
	      if (! no_regs_p && REG_P (op) && hard_regno[nop] < 0
		  && (cl = get_reg_class (REGNO (op))) != NO_REGS
		  && ira_class_subset_p[this_alternative][cl])
		{
		  if (lra_dump_file != NULL)
		    fprintf
		      (lra_dump_file,
		       "            %d Super set class reg: reject-=3\n", nop);
		  reject -= 3;
		}

	      this_alternative_offmemok = offmemok;
	      if (this_costly_alternative != NO_REGS)
		{
		  if (lra_dump_file != NULL)
		    fprintf (lra_dump_file,
			     "            %d Costly loser: reject++\n", nop);
		  reject++;
		}
	      /* If the operand is dying, has a matching constraint,
		 and satisfies constraints of the matched operand
		 which failed to satisfy the own constraints, most probably
		 the reload for this operand will be gone.  */
	      if (this_alternative_matches >= 0
		  && !curr_alt_win[this_alternative_matches]
		  && REG_P (op)
		  && find_regno_note (curr_insn, REG_DEAD, REGNO (op))
		  && (hard_regno[nop] >= 0
		      ? in_hard_reg_set_p (this_alternative_set,
					   mode, hard_regno[nop])
		      : in_class_p (op, this_alternative, NULL)))
		{
		  if (lra_dump_file != NULL)
		    fprintf
		      (lra_dump_file,
		       "            %d Dying matched operand reload: reject++\n",
		       nop);
		  reject++;
		}
	      else
		{
		  /* Strict_low_part requires to reload the register
		     not the sub-register.  In this case we should
		     check that a final reload hard reg can hold the
		     value mode.  */
		  if (curr_static_id->operand[nop].strict_low
		      && REG_P (op)
		      && hard_regno[nop] < 0
		      && GET_CODE (*curr_id->operand_loc[nop]) == SUBREG
		      && ira_class_hard_regs_num[this_alternative] > 0
		      && (!targetm.hard_regno_mode_ok
			  (ira_class_hard_regs[this_alternative][0],
			   GET_MODE (*curr_id->operand_loc[nop]))))
		    {
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            alt=%d: Strict low subreg reload -- refuse\n",
			   nalt);
		      goto fail;
		    }
		  losers++;
		}
	      if (operand_reg[nop] != NULL_RTX
		  /* Output operands and matched input operands are
		     not inherited.  The following conditions do not
		     exactly describe the previous statement but they
		     are pretty close.  */
		  && curr_static_id->operand[nop].type != OP_OUT
		  && (this_alternative_matches < 0
		      || curr_static_id->operand[nop].type != OP_IN))
		{
		  int last_reload = (lra_reg_info[ORIGINAL_REGNO
						  (operand_reg[nop])]
				     .last_reload);

		  /* The value of reload_sum has sense only if we
		     process insns in their order.  It happens only on
		     the first constraints sub-pass when we do most of
		     reload work.  */
		  if (lra_constraint_iter == 1 && last_reload > bb_reload_num)
		    reload_sum += last_reload - bb_reload_num;
		}
	      /* If this is a constant that is reloaded into the
		 desired class by copying it to memory first, count
		 that as another reload.  This is consistent with
		 other code and is required to avoid choosing another
		 alternative when the constant is moved into memory.
		 Note that the test here is precisely the same as in
		 the code below that calls force_const_mem.  */
	      if (CONST_POOL_OK_P (mode, op)
		  && ((targetm.preferred_reload_class
		       (op, this_alternative) == NO_REGS)
		      || no_input_reloads_p))
		{
		  const_to_mem = 1;
		  if (! no_regs_p)
		    losers++;
		}

	      /* Alternative loses if it requires a type of reload not
		 permitted for this insn.  We can always reload
		 objects with a REG_UNUSED note.  */
	      if ((curr_static_id->operand[nop].type != OP_IN
		   && no_output_reloads_p
		   && ! find_reg_note (curr_insn, REG_UNUSED, op))
		  || (curr_static_id->operand[nop].type != OP_OUT
		      && no_input_reloads_p && ! const_to_mem)
		  || (this_alternative_matches >= 0
		      && (no_input_reloads_p
			  || (no_output_reloads_p
			      && (curr_static_id->operand
				  [this_alternative_matches].type != OP_IN)
			      && ! find_reg_note (curr_insn, REG_UNUSED,
						  no_subreg_reg_operand
						  [this_alternative_matches])))))
		{
		  if (lra_dump_file != NULL)
		    fprintf
		      (lra_dump_file,
		       "            alt=%d: No input/otput reload -- refuse\n",
		       nalt);
		  goto fail;
		}

	      /* Alternative loses if it required class pseudo cannot
		 hold value of required mode.  Such insns can be
		 described by insn definitions with mode iterators.  */
	      if (GET_MODE (*curr_id->operand_loc[nop]) != VOIDmode
		  && ! hard_reg_set_empty_p (this_alternative_set)
		  /* It is common practice for constraints to use a
		     class which does not have actually enough regs to
		     hold the value (e.g. x86 AREG for mode requiring
		     more one general reg).  Therefore we have 2
		     conditions to check that the reload pseudo cannot
		     hold the mode value.  */
		  && (!targetm.hard_regno_mode_ok
		      (ira_class_hard_regs[this_alternative][0],
		       GET_MODE (*curr_id->operand_loc[nop])))
		  /* The above condition is not enough as the first
		     reg in ira_class_hard_regs can be not aligned for
		     multi-words mode values.  */
		  && (prohibited_class_reg_set_mode_p
		      (this_alternative, this_alternative_set,
		       GET_MODE (*curr_id->operand_loc[nop]))))
		{
		  if (lra_dump_file != NULL)
		    fprintf (lra_dump_file,
			     "            alt=%d: reload pseudo for op %d "
			     "cannot hold the mode value -- refuse\n",
			     nalt, nop);
		  goto fail;
		}

	      /* Check strong discouragement of reload of non-constant
		 into class THIS_ALTERNATIVE.  */
	      if (! CONSTANT_P (op) && ! no_regs_p
		  && (targetm.preferred_reload_class
		      (op, this_alternative) == NO_REGS
		      || (curr_static_id->operand[nop].type == OP_OUT
			  && (targetm.preferred_output_reload_class
			      (op, this_alternative) == NO_REGS))))
		{
		  if (lra_dump_file != NULL)
		    fprintf (lra_dump_file,
			     "            %d Non-prefered reload: reject+=%d\n",
			     nop, LRA_MAX_REJECT);
		  reject += LRA_MAX_REJECT;
		}

	      if (! (MEM_P (op) && offmemok)
		  && ! (const_to_mem && constmemok))
		{
		  /* We prefer to reload pseudos over reloading other
		     things, since such reloads may be able to be
		     eliminated later.  So bump REJECT in other cases.
		     Don't do this in the case where we are forcing a
		     constant into memory and it will then win since
		     we don't want to have a different alternative
		     match then.  */
		  if (! (REG_P (op) && REGNO (op) >= FIRST_PSEUDO_REGISTER))
		    {
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            %d Non-pseudo reload: reject+=2\n",
			   nop);
		      reject += 2;
		    }

		  if (! no_regs_p)
		    reload_nregs
		      += ira_reg_class_max_nregs[this_alternative][mode];

		  if (SMALL_REGISTER_CLASS_P (this_alternative))
		    {
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            %d Small class reload: reject+=%d\n",
			   nop, LRA_LOSER_COST_FACTOR / 2);
		      reject += LRA_LOSER_COST_FACTOR / 2;
		    }
		}

	      /* We are trying to spill pseudo into memory.  It is
		 usually more costly than moving to a hard register
		 although it might takes the same number of
		 reloads.

		 Non-pseudo spill may happen also.  Suppose a target allows both
		 register and memory in the operand constraint alternatives,
		 then it's typical that an eliminable register has a substition
		 of "base + offset" which can either be reloaded by a simple
		 "new_reg <= base + offset" which will match the register
		 constraint, or a similar reg addition followed by further spill
		 to and reload from memory which will match the memory
		 constraint, but this memory spill will be much more costly
		 usually.

		 Code below increases the reject for both pseudo and non-pseudo
		 spill.  */
	      if (no_regs_p
		  && !(MEM_P (op) && offmemok)
		  && !(REG_P (op) && hard_regno[nop] < 0))
		{
		  if (lra_dump_file != NULL)
		    fprintf
		      (lra_dump_file,
		       "            %d Spill %spseudo into memory: reject+=3\n",
		       nop, REG_P (op) ? "" : "Non-");
		  reject += 3;
		  if (VECTOR_MODE_P (mode))
		    {
		      /* Spilling vectors into memory is usually more
			 costly as they contain big values.  */
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            %d Spill vector pseudo: reject+=2\n",
			   nop);
		      reject += 2;
		    }
		}

	      /* When we use an operand requiring memory in given
		 alternative, the insn should write *and* read the
		 value to/from memory it is costly in comparison with
		 an insn alternative which does not use memory
		 (e.g. register or immediate operand).  We exclude
		 memory operand for such case as we can satisfy the
		 memory constraints by reloading address.  */
	      if (no_regs_p && offmemok && !MEM_P (op))
		{
		  if (lra_dump_file != NULL)
		    fprintf
		      (lra_dump_file,
		       "            Using memory insn operand %d: reject+=3\n",
		       nop);
		  reject += 3;
		}
	      
	      /* If reload requires moving value through secondary
		 memory, it will need one more insn at least.  */
	      if (this_alternative != NO_REGS 
		  && REG_P (op) && (cl = get_reg_class (REGNO (op))) != NO_REGS
		  && ((curr_static_id->operand[nop].type != OP_OUT
		       && targetm.secondary_memory_needed (GET_MODE (op), cl,
							   this_alternative))
		      || (curr_static_id->operand[nop].type != OP_IN
			  && (targetm.secondary_memory_needed
			      (GET_MODE (op), this_alternative, cl)))))
		losers++;

	      if (MEM_P (op) && offmemok)
		addr_losers++;
	      else
		{
		  /* Input reloads can be inherited more often than
		     output reloads can be removed, so penalize output
		     reloads.  */
		  if (!REG_P (op) || curr_static_id->operand[nop].type != OP_IN)
		    {
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            %d Non input pseudo reload: reject++\n",
			   nop);
		      reject++;
		    }

		  if (curr_static_id->operand[nop].type == OP_INOUT)
		    {
		      if (lra_dump_file != NULL)
			fprintf
			  (lra_dump_file,
			   "            %d Input/Output reload: reject+=%d\n",
			   nop, LRA_LOSER_COST_FACTOR);
		      reject += LRA_LOSER_COST_FACTOR;
		    }
		}
	    }

	  if (early_clobber_p && ! scratch_p)
	    {
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file,
			 "            %d Early clobber: reject++\n", nop);
	      reject++;
	    }
	  /* ??? We check early clobbers after processing all operands
	     (see loop below) and there we update the costs more.
	     Should we update the cost (may be approximately) here
	     because of early clobber register reloads or it is a rare
	     or non-important thing to be worth to do it.  */
	  overall = (losers * LRA_LOSER_COST_FACTOR + reject
		     - (addr_losers == losers ? static_reject : 0));
	  if ((best_losers == 0 || losers != 0) && best_overall < overall)
            {
              if (lra_dump_file != NULL)
		fprintf (lra_dump_file,
			 "            alt=%d,overall=%d,losers=%d -- refuse\n",
			 nalt, overall, losers);
              goto fail;
            }

	  if (update_and_check_small_class_inputs (nop, this_alternative))
	    {
	      if (lra_dump_file != NULL)
		fprintf (lra_dump_file,
			 "            alt=%d, not enough small class regs -- refuse\n",
			 nalt);
	      goto fail;
	    }
	  curr_alt[nop] = this_alternative;
	  COPY_HARD_REG_SET (curr_alt_set[nop], this_alternative_set);
	  curr_alt_win[nop] = this_alternative_win;
	  curr_alt_match_win[nop] = this_alternative_match_win;
	  curr_alt_offmemok[nop] = this_alternative_offmemok;
	  curr_alt_matches[nop] = this_alternative_matches;

	  if (this_alternative_matches >= 0
	      && !did_match && !this_alternative_win)
	    curr_alt_win[this_alternative_matches] = false;

	  if (early_clobber_p && operand_reg[nop] != NULL_RTX)
	    early_clobbered_nops[early_clobbered_regs_num++] = nop;
	}

      if (curr_insn_set != NULL_RTX && n_operands == 2
	  /* Prevent processing non-move insns.  */
	  && (GET_CODE (SET_SRC (curr_insn_set)) == SUBREG
	      || SET_SRC (curr_insn_set) == no_subreg_reg_operand[1])
	  && ((! curr_alt_win[0] && ! curr_alt_win[1]
	       && REG_P (no_subreg_reg_operand[0])
	       && REG_P (no_subreg_reg_operand[1])
	       && (reg_in_class_p (no_subreg_reg_operand[0], curr_alt[1])
		   || reg_in_class_p (no_subreg_reg_operand[1], curr_alt[0])))
	      || (! curr_alt_win[0] && curr_alt_win[1]
		  && REG_P (no_subreg_reg_operand[1])
		  /* Check that we reload memory not the memory
		     address.  */
		  && ! (curr_alt_offmemok[0]
			&& MEM_P (no_subreg_reg_operand[0]))
		  && reg_in_class_p (no_subreg_reg_operand[1], curr_alt[0]))
	      || (curr_alt_win[0] && ! curr_alt_win[1]
		  && REG_P (no_subreg_reg_operand[0])
		  /* Check that we reload memory not the memory
		     address.  */
		  && ! (curr_alt_offmemok[1]
			&& MEM_P (no_subreg_reg_operand[1]))
		  && reg_in_class_p (no_subreg_reg_operand[0], curr_alt[1])
		  && (! CONST_POOL_OK_P (curr_operand_mode[1],
					 no_subreg_reg_operand[1])
		      || (targetm.preferred_reload_class
			  (no_subreg_reg_operand[1],
			   (enum reg_class) curr_alt[1]) != NO_REGS))
		  /* If it is a result of recent elimination in move
		     insn we can transform it into an add still by
		     using this alternative.  */
		  && GET_CODE (no_subreg_reg_operand[1]) != PLUS
		  /* Likewise if the source has been replaced with an
		     equivalent value.  This only happens once -- the reload
		     will use the equivalent value instead of the register it
		     replaces -- so there should be no danger of cycling.  */
		  && !equiv_substition_p[1])))
	{
	  /* We have a move insn and a new reload insn will be similar
	     to the current insn.  We should avoid such situation as
	     it results in LRA cycling.  */
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file,
		     "            Cycle danger: overall += LRA_MAX_REJECT\n");
	  overall += LRA_MAX_REJECT;
	}
      ok_p = true;
      curr_alt_dont_inherit_ops_num = 0;
      for (nop = 0; nop < early_clobbered_regs_num; nop++)
	{
	  int i, j, clobbered_hard_regno, first_conflict_j, last_conflict_j;
	  HARD_REG_SET temp_set;

	  i = early_clobbered_nops[nop];
	  if ((! curr_alt_win[i] && ! curr_alt_match_win[i])
	      || hard_regno[i] < 0)
	    continue;
	  lra_assert (operand_reg[i] != NULL_RTX);
	  clobbered_hard_regno = hard_regno[i];
	  CLEAR_HARD_REG_SET (temp_set);
	  add_to_hard_reg_set (&temp_set, biggest_mode[i], clobbered_hard_regno);
	  first_conflict_j = last_conflict_j = -1;
	  for (j = 0; j < n_operands; j++)
	    if (j == i
		/* We don't want process insides of match_operator and
		   match_parallel because otherwise we would process
		   their operands once again generating a wrong
		   code.  */
		|| curr_static_id->operand[j].is_operator)
	      continue;
	    else if ((curr_alt_matches[j] == i && curr_alt_match_win[j])
		     || (curr_alt_matches[i] == j && curr_alt_match_win[i]))
	      continue;
	    /* If we don't reload j-th operand, check conflicts.  */
	    else if ((curr_alt_win[j] || curr_alt_match_win[j])
		     && uses_hard_regs_p (*curr_id->operand_loc[j], temp_set))
	      {
		if (first_conflict_j < 0)
		  first_conflict_j = j;
		last_conflict_j = j;
		/* Both the earlyclobber operand and conflicting operand
		   cannot both be user defined hard registers.  */
		if (HARD_REGISTER_P (operand_reg[i])
		    && REG_USERVAR_P (operand_reg[i])
		    && operand_reg[j] != NULL_RTX
		    && HARD_REGISTER_P (operand_reg[j])
		    && REG_USERVAR_P (operand_reg[j]))
		  fatal_insn ("unable to generate reloads for "
			      "impossible constraints:", curr_insn);
	      }
	  if (last_conflict_j < 0)
	    continue;

	  /* If an earlyclobber operand conflicts with another non-matching
	     operand (ie, they have been assigned the same hard register),
	     then it is better to reload the other operand, as there may
	     exist yet another operand with a matching constraint associated
	     with the earlyclobber operand.  However, if one of the operands
	     is an explicit use of a hard register, then we must reload the
	     other non-hard register operand.  */
	  if (HARD_REGISTER_P (operand_reg[i])
	      || (first_conflict_j == last_conflict_j
		  && operand_reg[last_conflict_j] != NULL_RTX
		  && !curr_alt_match_win[last_conflict_j]
		  && !HARD_REGISTER_P (operand_reg[last_conflict_j])))
	    {
	      curr_alt_win[last_conflict_j] = false;
	      curr_alt_dont_inherit_ops[curr_alt_dont_inherit_ops_num++]
		= last_conflict_j;
	      losers++;
	      if (lra_dump_file != NULL)
		fprintf
		  (lra_dump_file,
		   "            %d Conflict early clobber reload: reject--\n",
		   i);
	    }
	  else
	    {
	      /* We need to reload early clobbered register and the
		 matched registers.  */
	      for (j = 0; j < n_operands; j++)
		if (curr_alt_matches[j] == i)
		  {
		    curr_alt_match_win[j] = false;
		    losers++;
		    overall += LRA_LOSER_COST_FACTOR;
		  }
	      if (! curr_alt_match_win[i])
		curr_alt_dont_inherit_ops[curr_alt_dont_inherit_ops_num++] = i;
	      else
		{
		  /* Remember pseudos used for match reloads are never
		     inherited.  */
		  lra_assert (curr_alt_matches[i] >= 0);
		  curr_alt_win[curr_alt_matches[i]] = false;
		}
	      curr_alt_win[i] = curr_alt_match_win[i] = false;
	      losers++;
	      if (lra_dump_file != NULL)
		fprintf
		  (lra_dump_file,
		   "            %d Matched conflict early clobber reloads: "
		   "reject--\n",
		   i);
	    }
	  /* Early clobber was already reflected in REJECT. */
	  if (!matching_early_clobber[i])
	    {
	      lra_assert (reject > 0);
	      reject--;
	      matching_early_clobber[i] = 1;
	    }
	  overall += LRA_LOSER_COST_FACTOR - 1;
	}
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "          alt=%d,overall=%d,losers=%d,rld_nregs=%d\n",
		 nalt, overall, losers, reload_nregs);

      /* If this alternative can be made to work by reloading, and it
	 needs less reloading than the others checked so far, record
	 it as the chosen goal for reloading.  */
      if ((best_losers != 0 && losers == 0)
	  || (((best_losers == 0 && losers == 0)
	       || (best_losers != 0 && losers != 0))
	      && (best_overall > overall
		  || (best_overall == overall
		      /* If the cost of the reloads is the same,
			 prefer alternative which requires minimal
			 number of reload regs.  */
		      && (reload_nregs < best_reload_nregs
			  || (reload_nregs == best_reload_nregs
			      && (best_reload_sum < reload_sum
				  || (best_reload_sum == reload_sum
				      && nalt < goal_alt_number))))))))
	{
	  for (nop = 0; nop < n_operands; nop++)
	    {
	      goal_alt_win[nop] = curr_alt_win[nop];
	      goal_alt_match_win[nop] = curr_alt_match_win[nop];
	      goal_alt_matches[nop] = curr_alt_matches[nop];
	      goal_alt[nop] = curr_alt[nop];
	      goal_alt_offmemok[nop] = curr_alt_offmemok[nop];
	    }
	  goal_alt_dont_inherit_ops_num = curr_alt_dont_inherit_ops_num;
	  for (nop = 0; nop < curr_alt_dont_inherit_ops_num; nop++)
	    goal_alt_dont_inherit_ops[nop] = curr_alt_dont_inherit_ops[nop];
	  goal_alt_swapped = curr_swapped;
	  best_overall = overall;
	  best_losers = losers;
	  best_reload_nregs = reload_nregs;
	  best_reload_sum = reload_sum;
	  goal_alt_number = nalt;
	}
      if (losers == 0)
	/* Everything is satisfied.  Do not process alternatives
	   anymore.  */
	break;
    fail:
      ;
    }
  return ok_p;
}

/* Make reload base reg from address AD.  */
static rtx
base_to_reg (struct address_info *ad)
{
  enum reg_class cl;
  int code = -1;
  rtx new_inner = NULL_RTX;
  rtx new_reg = NULL_RTX;
  rtx_insn *insn;
  rtx_insn *last_insn = get_last_insn();

  lra_assert (ad->disp == ad->disp_term);
  cl = base_reg_class (ad->mode, ad->as, ad->base_outer_code,
                       get_index_code (ad));
  new_reg = lra_create_new_reg (GET_MODE (*ad->base), NULL_RTX,
                                cl, "base");
  new_inner = simplify_gen_binary (PLUS, GET_MODE (new_reg), new_reg,
                                   ad->disp_term == NULL
                                   ? const0_rtx
                                   : *ad->disp_term);
  if (!valid_address_p (ad->mode, new_inner, ad->as))
    return NULL_RTX;
  insn = emit_insn (gen_rtx_SET (new_reg, *ad->base));
  code = recog_memoized (insn);
  if (code < 0)
    {
      delete_insns_since (last_insn);
      return NULL_RTX;
    }

  return new_inner;
}

/* Make reload base reg + DISP from address AD.  Return the new pseudo.  */
static rtx
base_plus_disp_to_reg (struct address_info *ad, rtx disp)
{
  enum reg_class cl;
  rtx new_reg;

  lra_assert (ad->base == ad->base_term);
  cl = base_reg_class (ad->mode, ad->as, ad->base_outer_code,
		       get_index_code (ad));
  new_reg = lra_create_new_reg (GET_MODE (*ad->base_term), NULL_RTX,
				cl, "base + disp");
  lra_emit_add (new_reg, *ad->base_term, disp);
  return new_reg;
}

/* Make reload of index part of address AD.  Return the new
   pseudo.  */
static rtx
index_part_to_reg (struct address_info *ad)
{
  rtx new_reg;

  new_reg = lra_create_new_reg (GET_MODE (*ad->index), NULL_RTX,
				INDEX_REG_CLASS, "index term");
  expand_mult (GET_MODE (*ad->index), *ad->index_term,
	       GEN_INT (get_index_scale (ad)), new_reg, 1);
  return new_reg;
}

/* Return true if we can add a displacement to address AD, even if that
   makes the address invalid.  The fix-up code requires any new address
   to be the sum of the BASE_TERM, INDEX and DISP_TERM fields.  */
static bool
can_add_disp_p (struct address_info *ad)
{
  return (!ad->autoinc_p
	  && ad->segment == NULL
	  && ad->base == ad->base_term
	  && ad->disp == ad->disp_term);
}

/* Make equiv substitution in address AD.  Return true if a substitution
   was made.  */
static bool
equiv_address_substitution (struct address_info *ad)
{
  rtx base_reg, new_base_reg, index_reg, new_index_reg, *base_term, *index_term;
  poly_int64 disp;
  HOST_WIDE_INT scale;
  bool change_p;

  base_term = strip_subreg (ad->base_term);
  if (base_term == NULL)
    base_reg = new_base_reg = NULL_RTX;
  else
    {
      base_reg = *base_term;
      new_base_reg = get_equiv_with_elimination (base_reg, curr_insn);
    }
  index_term = strip_subreg (ad->index_term);
  if (index_term == NULL)
    index_reg = new_index_reg = NULL_RTX;
  else
    {
      index_reg = *index_term;
      new_index_reg = get_equiv_with_elimination (index_reg, curr_insn);
    }
  if (base_reg == new_base_reg && index_reg == new_index_reg)
    return false;
  disp = 0;
  change_p = false;
  if (lra_dump_file != NULL)
    {
      fprintf (lra_dump_file, "Changing address in insn %d ",
	       INSN_UID (curr_insn));
      dump_value_slim (lra_dump_file, *ad->outer, 1);
    }
  if (base_reg != new_base_reg)
    {
      poly_int64 offset;
      if (REG_P (new_base_reg))
	{
	  *base_term = new_base_reg;
	  change_p = true;
	}
      else if (GET_CODE (new_base_reg) == PLUS
	       && REG_P (XEXP (new_base_reg, 0))
	       && poly_int_rtx_p (XEXP (new_base_reg, 1), &offset)
	       && can_add_disp_p (ad))
	{
	  disp += offset;
	  *base_term = XEXP (new_base_reg, 0);
	  change_p = true;
	}
      if (ad->base_term2 != NULL)
	*ad->base_term2 = *ad->base_term;
    }
  if (index_reg != new_index_reg)
    {
      poly_int64 offset;
      if (REG_P (new_index_reg))
	{
	  *index_term = new_index_reg;
	  change_p = true;
	}
      else if (GET_CODE (new_index_reg) == PLUS
	       && REG_P (XEXP (new_index_reg, 0))
	       && poly_int_rtx_p (XEXP (new_index_reg, 1), &offset)
	       && can_add_disp_p (ad)
	       && (scale = get_index_scale (ad)))
	{
	  disp += offset * scale;
	  *index_term = XEXP (new_index_reg, 0);
	  change_p = true;
	}
    }
  if (maybe_ne (disp, 0))
    {
      if (ad->disp != NULL)
	*ad->disp = plus_constant (GET_MODE (*ad->inner), *ad->disp, disp);
      else
	{
	  *ad->inner = plus_constant (GET_MODE (*ad->inner), *ad->inner, disp);
	  update_address (ad);
	}
      change_p = true;
    }
  if (lra_dump_file != NULL)
    {
      if (! change_p)
	fprintf (lra_dump_file, " -- no change\n");
      else
	{
	  fprintf (lra_dump_file, " on equiv ");
	  dump_value_slim (lra_dump_file, *ad->outer, 1);
	  fprintf (lra_dump_file, "\n");
	}
    }
  return change_p;
}

/* Major function to make reloads for an address in operand NOP or
   check its correctness (If CHECK_ONLY_P is true). The supported
   cases are:

   1) an address that existed before LRA started, at which point it
   must have been valid.  These addresses are subject to elimination
   and may have become invalid due to the elimination offset being out
   of range.

   2) an address created by forcing a constant to memory
   (force_const_to_mem).  The initial form of these addresses might
   not be valid, and it is this function's job to make them valid.

   3) a frame address formed from a register and a (possibly zero)
   constant offset.  As above, these addresses might not be valid and
   this function must make them so.

   Add reloads to the lists *BEFORE and *AFTER.  We might need to add
   reloads to *AFTER because of inc/dec, {pre, post} modify in the
   address.  Return true for any RTL change.

   The function is a helper function which does not produce all
   transformations (when CHECK_ONLY_P is false) which can be
   necessary.  It does just basic steps.  To do all necessary
   transformations use function process_address.  */
static bool
process_address_1 (int nop, bool check_only_p,
		   rtx_insn **before, rtx_insn **after)
{
  struct address_info ad;
  rtx new_reg;
  HOST_WIDE_INT scale;
  rtx op = *curr_id->operand_loc[nop];
  const char *constraint = curr_static_id->operand[nop].constraint;
  enum constraint_num cn = lookup_constraint (constraint);
  bool change_p = false;

  if (MEM_P (op)
      && GET_MODE (op) == BLKmode
      && GET_CODE (XEXP (op, 0)) == SCRATCH)
    return false;

  if (insn_extra_address_constraint (cn)
      /* When we find an asm operand with an address constraint that
	 doesn't satisfy address_operand to begin with, we clear
	 is_address, so that we don't try to make a non-address fit.
	 If the asm statement got this far, it's because other
	 constraints are available, and we'll use them, disregarding
	 the unsatisfiable address ones.  */
      && curr_static_id->operand[nop].is_address)
    decompose_lea_address (&ad, curr_id->operand_loc[nop]);
  /* Do not attempt to decompose arbitrary addresses generated by combine
     for asm operands with loose constraints, e.g 'X'.  */
  else if (MEM_P (op)
	   && !(INSN_CODE (curr_insn) < 0
		&& get_constraint_type (cn) == CT_FIXED_FORM
	        && constraint_satisfied_p (op, cn)))
    decompose_mem_address (&ad, op);
  else if (GET_CODE (op) == SUBREG
	   && MEM_P (SUBREG_REG (op)))
    decompose_mem_address (&ad, SUBREG_REG (op));
  else
    return false;
  /* If INDEX_REG_CLASS is assigned to base_term already and isn't to
     index_term, swap them so to avoid assigning INDEX_REG_CLASS to both
     when INDEX_REG_CLASS is a single register class.  */
  if (ad.base_term != NULL
      && ad.index_term != NULL
      && ira_class_hard_regs_num[INDEX_REG_CLASS] == 1
      && REG_P (*ad.base_term)
      && REG_P (*ad.index_term)
      && in_class_p (*ad.base_term, INDEX_REG_CLASS, NULL)
      && ! in_class_p (*ad.index_term, INDEX_REG_CLASS, NULL))
    {
      std::swap (ad.base, ad.index);
      std::swap (ad.base_term, ad.index_term);
    }
  if (! check_only_p)
    change_p = equiv_address_substitution (&ad);
  if (ad.base_term != NULL
      && (process_addr_reg
	  (ad.base_term, check_only_p, before,
	   (ad.autoinc_p
	    && !(REG_P (*ad.base_term)
		 && find_regno_note (curr_insn, REG_DEAD,
				     REGNO (*ad.base_term)) != NULL_RTX)
	    ? after : NULL),
	   base_reg_class (ad.mode, ad.as, ad.base_outer_code,
			   get_index_code (&ad)))))
    {
      change_p = true;
      if (ad.base_term2 != NULL)
	*ad.base_term2 = *ad.base_term;
    }
  if (ad.index_term != NULL
      && process_addr_reg (ad.index_term, check_only_p,
			   before, NULL, INDEX_REG_CLASS))
    change_p = true;

  /* Target hooks sometimes don't treat extra-constraint addresses as
     legitimate address_operands, so handle them specially.  */
  if (insn_extra_address_constraint (cn)
      && satisfies_address_constraint_p (&ad, cn))
    return change_p;

  if (check_only_p)
    return change_p;

  /* There are three cases where the shape of *AD.INNER may now be invalid:

     1) the original address was valid, but either elimination or
     equiv_address_substitution was applied and that made
     the address invalid.

     2) the address is an invalid symbolic address created by
     force_const_to_mem.

     3) the address is a frame address with an invalid offset.

     4) the address is a frame address with an invalid base.

     All these cases involve a non-autoinc address, so there is no
     point revalidating other types.  */
  if (ad.autoinc_p || valid_address_p (&ad))
    return change_p;

  /* Any index existed before LRA started, so we can assume that the
     presence and shape of the index is valid.  */
  push_to_sequence (*before);
  lra_assert (ad.disp == ad.disp_term);
  if (ad.base == NULL)
    {
      if (ad.index == NULL)
	{
	  rtx_insn *insn;
	  rtx_insn *last = get_last_insn ();
	  int code = -1;
	  enum reg_class cl = base_reg_class (ad.mode, ad.as,
					      SCRATCH, SCRATCH);
	  rtx addr = *ad.inner;

	  new_reg = lra_create_new_reg (Pmode, NULL_RTX, cl, "addr");
	  if (HAVE_lo_sum)
	    {
	      /* addr => lo_sum (new_base, addr), case (2) above.  */
	      insn = emit_insn (gen_rtx_SET
				(new_reg,
				 gen_rtx_HIGH (Pmode, copy_rtx (addr))));
	      code = recog_memoized (insn);
	      if (code >= 0)
		{
		  *ad.inner = gen_rtx_LO_SUM (Pmode, new_reg, addr);
		  if (! valid_address_p (ad.mode, *ad.outer, ad.as))
		    {
		      /* Try to put lo_sum into register.  */
		      insn = emit_insn (gen_rtx_SET
					(new_reg,
					 gen_rtx_LO_SUM (Pmode, new_reg, addr)));
		      code = recog_memoized (insn);
		      if (code >= 0)
			{
			  *ad.inner = new_reg;
			  if (! valid_address_p (ad.mode, *ad.outer, ad.as))
			    {
			      *ad.inner = addr;
			      code = -1;
			    }
			}

		    }
		}
	      if (code < 0)
		delete_insns_since (last);
	    }

	  if (code < 0)
	    {
	      /* addr => new_base, case (2) above.  */
	      lra_emit_move (new_reg, addr);

	      for (insn = last == NULL_RTX ? get_insns () : NEXT_INSN (last);
		   insn != NULL_RTX;
		   insn = NEXT_INSN (insn))
		if (recog_memoized (insn) < 0)
		  break;
	      if (insn != NULL_RTX)
		{
		  /* Do nothing if we cannot generate right insns.
		     This is analogous to reload pass behavior.  */
		  delete_insns_since (last);
		  end_sequence ();
		  return false;
		}
	      *ad.inner = new_reg;
	    }
	}
      else
	{
	  /* index * scale + disp => new base + index * scale,
	     case (1) above.  */
	  enum reg_class cl = base_reg_class (ad.mode, ad.as, PLUS,
					      GET_CODE (*ad.index));

	  lra_assert (INDEX_REG_CLASS != NO_REGS);
	  new_reg = lra_create_new_reg (Pmode, NULL_RTX, cl, "disp");
	  lra_emit_move (new_reg, *ad.disp);
	  *ad.inner = simplify_gen_binary (PLUS, GET_MODE (new_reg),
					   new_reg, *ad.index);
	}
    }
  else if (ad.index == NULL)
    {
      int regno;
      enum reg_class cl;
      rtx set;
      rtx_insn *insns, *last_insn;
      /* Try to reload base into register only if the base is invalid
         for the address but with valid offset, case (4) above.  */
      start_sequence ();
      new_reg = base_to_reg (&ad);

      /* base + disp => new base, cases (1) and (3) above.  */
      /* Another option would be to reload the displacement into an
	 index register.  However, postreload has code to optimize
	 address reloads that have the same base and different
	 displacements, so reloading into an index register would
	 not necessarily be a win.  */
      if (new_reg == NULL_RTX)
	{
	  /* See if the target can split the displacement into a
	     legitimate new displacement from a local anchor.  */
	  gcc_assert (ad.disp == ad.disp_term);
	  poly_int64 orig_offset;
	  rtx offset1, offset2;
	  if (poly_int_rtx_p (*ad.disp, &orig_offset)
	      && targetm.legitimize_address_displacement (&offset1, &offset2,
							  orig_offset,
							  ad.mode))
	    {
	      new_reg = base_plus_disp_to_reg (&ad, offset1);
	      new_reg = gen_rtx_PLUS (GET_MODE (new_reg), new_reg, offset2);
	    }
	  else
	    new_reg = base_plus_disp_to_reg (&ad, *ad.disp);
	}
      insns = get_insns ();
      last_insn = get_last_insn ();
      /* If we generated at least two insns, try last insn source as
	 an address.  If we succeed, we generate one less insn.  */
      if (REG_P (new_reg)
	  && last_insn != insns
	  && (set = single_set (last_insn)) != NULL_RTX
	  && GET_CODE (SET_SRC (set)) == PLUS
	  && REG_P (XEXP (SET_SRC (set), 0))
	  && CONSTANT_P (XEXP (SET_SRC (set), 1)))
	{
	  *ad.inner = SET_SRC (set);
	  if (valid_address_p (ad.mode, *ad.outer, ad.as))
	    {
	      *ad.base_term = XEXP (SET_SRC (set), 0);
	      *ad.disp_term = XEXP (SET_SRC (set), 1);
	      cl = base_reg_class (ad.mode, ad.as, ad.base_outer_code,
				   get_index_code (&ad));
	      regno = REGNO (*ad.base_term);
	      if (regno >= FIRST_PSEUDO_REGISTER
		  && cl != lra_get_allocno_class (regno))
		lra_change_class (regno, cl, "      Change to", true);
	      new_reg = SET_SRC (set);
	      delete_insns_since (PREV_INSN (last_insn));
	    }
	}
      end_sequence ();
      emit_insn (insns);
      *ad.inner = new_reg;
    }
  else if (ad.disp_term != NULL)
    {
      /* base + scale * index + disp => new base + scale * index,
	 case (1) above.  */
      gcc_assert (ad.disp == ad.disp_term);
      new_reg = base_plus_disp_to_reg (&ad, *ad.disp);
      *ad.inner = simplify_gen_binary (PLUS, GET_MODE (new_reg),
				       new_reg, *ad.index);
    }
  else if ((scale = get_index_scale (&ad)) == 1)
    {
      /* The last transformation to one reg will be made in
	 curr_insn_transform function.  */
      end_sequence ();
      return false;
    }
  else if (scale != 0)
    {
      /* base + scale * index => base + new_reg,
	 case (1) above.
      Index part of address may become invalid.  For example, we
      changed pseudo on the equivalent memory and a subreg of the
      pseudo onto the memory of different mode for which the scale is
      prohibitted.  */
      new_reg = index_part_to_reg (&ad);
      *ad.inner = simplify_gen_binary (PLUS, GET_MODE (new_reg),
				       *ad.base_term, new_reg);
    }
  else
    {
      enum reg_class cl = base_reg_class (ad.mode, ad.as,
					  SCRATCH, SCRATCH);
      rtx addr = *ad.inner;
      
      new_reg = lra_create_new_reg (Pmode, NULL_RTX, cl, "addr");
      /* addr => new_base.  */
      lra_emit_move (new_reg, addr);
      *ad.inner = new_reg;
    }
  *before = get_insns ();
  end_sequence ();
  return true;
}

/* If CHECK_ONLY_P is false, do address reloads until it is necessary.
   Use process_address_1 as a helper function.  Return true for any
   RTL changes.

   If CHECK_ONLY_P is true, just check address correctness.  Return
   false if the address correct.  */
static bool
process_address (int nop, bool check_only_p,
		 rtx_insn **before, rtx_insn **after)
{
  bool res = false;

  while (process_address_1 (nop, check_only_p, before, after))
    {
      if (check_only_p)
	return true;
      res = true;
    }
  return res;
}

/* Emit insns to reload VALUE into a new register.  VALUE is an
   auto-increment or auto-decrement RTX whose operand is a register or
   memory location; so reloading involves incrementing that location.
   IN is either identical to VALUE, or some cheaper place to reload
   value being incremented/decremented from.

   INC_AMOUNT is the number to increment or decrement by (always
   positive and ignored for POST_MODIFY/PRE_MODIFY).

   Return pseudo containing the result.	 */
static rtx
emit_inc (enum reg_class new_rclass, rtx in, rtx value, poly_int64 inc_amount)
{
  /* REG or MEM to be copied and incremented.  */
  rtx incloc = XEXP (value, 0);
  /* Nonzero if increment after copying.  */
  int post = (GET_CODE (value) == POST_DEC || GET_CODE (value) == POST_INC
	      || GET_CODE (value) == POST_MODIFY);
  rtx_insn *last;
  rtx inc;
  rtx_insn *add_insn;
  int code;
  rtx real_in = in == value ? incloc : in;
  rtx result;
  bool plus_p = true;

  if (GET_CODE (value) == PRE_MODIFY || GET_CODE (value) == POST_MODIFY)
    {
      lra_assert (GET_CODE (XEXP (value, 1)) == PLUS
		  || GET_CODE (XEXP (value, 1)) == MINUS);
      lra_assert (rtx_equal_p (XEXP (XEXP (value, 1), 0), XEXP (value, 0)));
      plus_p = GET_CODE (XEXP (value, 1)) == PLUS;
      inc = XEXP (XEXP (value, 1), 1);
    }
  else
    {
      if (GET_CODE (value) == PRE_DEC || GET_CODE (value) == POST_DEC)
	inc_amount = -inc_amount;

      inc = gen_int_mode (inc_amount, GET_MODE (value));
    }

  if (! post && REG_P (incloc))
    result = incloc;
  else
    result = lra_create_new_reg (GET_MODE (value), value, new_rclass,
				 "INC/DEC result");

  if (real_in != result)
    {
      /* First copy the location to the result register.  */
      lra_assert (REG_P (result));
      emit_insn (gen_move_insn (result, real_in));
    }

  /* We suppose that there are insns to add/sub with the constant
     increment permitted in {PRE/POST)_{DEC/INC/MODIFY}.  At least the
     old reload worked with this assumption.  If the assumption
     becomes wrong, we should use approach in function
     base_plus_disp_to_reg.  */
  if (in == value)
    {
      /* See if we can directly increment INCLOC.  */
      last = get_last_insn ();
      add_insn = emit_insn (plus_p
			    ? gen_add2_insn (incloc, inc)
			    : gen_sub2_insn (incloc, inc));

      code = recog_memoized (add_insn);
      if (code >= 0)
	{
	  if (! post && result != incloc)
	    emit_insn (gen_move_insn (result, incloc));
	  return result;
	}
      delete_insns_since (last);
    }

  /* If couldn't do the increment directly, must increment in RESULT.
     The way we do this depends on whether this is pre- or
     post-increment.  For pre-increment, copy INCLOC to the reload
     register, increment it there, then save back.  */
  if (! post)
    {
      if (real_in != result)
	emit_insn (gen_move_insn (result, real_in));
      if (plus_p)
	emit_insn (gen_add2_insn (result, inc));
      else
	emit_insn (gen_sub2_insn (result, inc));
      if (result != incloc)
	emit_insn (gen_move_insn (incloc, result));
    }
  else
    {
      /* Post-increment.

	 Because this might be a jump insn or a compare, and because
	 RESULT may not be available after the insn in an input
	 reload, we must do the incrementing before the insn being
	 reloaded for.

	 We have already copied IN to RESULT.  Increment the copy in
	 RESULT, save that back, then decrement RESULT so it has
	 the original value.  */
      if (plus_p)
	emit_insn (gen_add2_insn (result, inc));
      else
	emit_insn (gen_sub2_insn (result, inc));
      emit_insn (gen_move_insn (incloc, result));
      /* Restore non-modified value for the result.  We prefer this
	 way because it does not require an additional hard
	 register.  */
      if (plus_p)
	{
	  poly_int64 offset;
	  if (poly_int_rtx_p (inc, &offset))
	    emit_insn (gen_add2_insn (result,
				      gen_int_mode (-offset,
						    GET_MODE (result))));
	  else
	    emit_insn (gen_sub2_insn (result, inc));
	}
      else
	emit_insn (gen_add2_insn (result, inc));
    }
  return result;
}

/* Return true if the current move insn does not need processing as we
   already know that it satisfies its constraints.  */
static bool
simple_move_p (void)
{
  rtx dest, src;
  enum reg_class dclass, sclass;

  lra_assert (curr_insn_set != NULL_RTX);
  dest = SET_DEST (curr_insn_set);
  src = SET_SRC (curr_insn_set);

  /* If the instruction has multiple sets we need to process it even if it
     is single_set.  This can happen if one or more of the SETs are dead.
     See PR73650.  */
  if (multiple_sets (curr_insn))
    return false;

  return ((dclass = get_op_class (dest)) != NO_REGS
	  && (sclass = get_op_class (src)) != NO_REGS
	  /* The backend guarantees that register moves of cost 2
	     never need reloads.  */
	  && targetm.register_move_cost (GET_MODE (src), sclass, dclass) == 2);
 }

/* Swap operands NOP and NOP + 1. */
static inline void
swap_operands (int nop)
{
  std::swap (curr_operand_mode[nop], curr_operand_mode[nop + 1]);
  std::swap (original_subreg_reg_mode[nop], original_subreg_reg_mode[nop + 1]);
  std::swap (*curr_id->operand_loc[nop], *curr_id->operand_loc[nop + 1]);
  std::swap (equiv_substition_p[nop], equiv_substition_p[nop + 1]);
  /* Swap the duplicates too.  */
  lra_update_dup (curr_id, nop);
  lra_update_dup (curr_id, nop + 1);
}

/* Main entry point of the constraint code: search the body of the
   current insn to choose the best alternative.  It is mimicking insn
   alternative cost calculation model of former reload pass.  That is
   because machine descriptions were written to use this model.  This
   model can be changed in future.  Make commutative operand exchange
   if it is chosen.

   if CHECK_ONLY_P is false, do RTL changes to satisfy the
   constraints.  Return true if any change happened during function
   call.

   If CHECK_ONLY_P is true then don't do any transformation.  Just
   check that the insn satisfies all constraints.  If the insn does
   not satisfy any constraint, return true.  */
static bool
curr_insn_transform (bool check_only_p)
{
  int i, j, k;
  int n_operands;
  int n_alternatives;
  int n_outputs;
  int commutative;
  signed char goal_alt_matched[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  signed char match_inputs[MAX_RECOG_OPERANDS + 1];
  signed char outputs[MAX_RECOG_OPERANDS + 1];
  rtx_insn *before, *after;
  bool alt_p = false;
  /* Flag that the insn has been changed through a transformation.  */
  bool change_p;
  bool sec_mem_p;
  bool use_sec_mem_p;
  int max_regno_before;
  int reused_alternative_num;

  curr_insn_set = single_set (curr_insn);
  if (curr_insn_set != NULL_RTX && simple_move_p ())
    {
      /* We assume that the corresponding insn alternative has no
	 earlier clobbers.  If it is not the case, don't define move
	 cost equal to 2 for the corresponding register classes.  */
      lra_set_used_insn_alternative (curr_insn, LRA_NON_CLOBBERED_ALT);
      return false;
    }

  no_input_reloads_p = no_output_reloads_p = false;
  goal_alt_number = -1;
  change_p = sec_mem_p = false;
  /* JUMP_INSNs and CALL_INSNs are not allowed to have any output
     reloads; neither are insns that SET cc0.  Insns that use CC0 are
     not allowed to have any input reloads.  */
  if (JUMP_P (curr_insn) || CALL_P (curr_insn))
    no_output_reloads_p = true;

  if (HAVE_cc0 && reg_referenced_p (cc0_rtx, PATTERN (curr_insn)))
    no_input_reloads_p = true;
  if (HAVE_cc0 && reg_set_p (cc0_rtx, PATTERN (curr_insn)))
    no_output_reloads_p = true;

  n_operands = curr_static_id->n_operands;
  n_alternatives = curr_static_id->n_alternatives;

  /* Just return "no reloads" if insn has no operands with
     constraints.  */
  if (n_operands == 0 || n_alternatives == 0)
    return false;

  max_regno_before = max_reg_num ();

  for (i = 0; i < n_operands; i++)
    {
      goal_alt_matched[i][0] = -1;
      goal_alt_matches[i] = -1;
    }

  commutative = curr_static_id->commutative;

  /* Now see what we need for pseudos that didn't get hard regs or got
     the wrong kind of hard reg.  For this, we must consider all the
     operands together against the register constraints.  */

  best_losers = best_overall = INT_MAX;
  best_reload_sum = 0;

  curr_swapped = false;
  goal_alt_swapped = false;

  if (! check_only_p)
    /* Make equivalence substitution and memory subreg elimination
       before address processing because an address legitimacy can
       depend on memory mode.  */
    for (i = 0; i < n_operands; i++)
      {
	rtx op, subst, old;
	bool op_change_p = false;

	if (curr_static_id->operand[i].is_operator)
	  continue;
	
	old = op = *curr_id->operand_loc[i];
	if (GET_CODE (old) == SUBREG)
	  old = SUBREG_REG (old);
	subst = get_equiv_with_elimination (old, curr_insn);
	original_subreg_reg_mode[i] = VOIDmode;
	equiv_substition_p[i] = false;
	if (subst != old)
	  {
	    equiv_substition_p[i] = true;
	    subst = copy_rtx (subst);
	    lra_assert (REG_P (old));
	    if (GET_CODE (op) != SUBREG)
	      *curr_id->operand_loc[i] = subst;
	    else
	      {
		SUBREG_REG (op) = subst;
		if (GET_MODE (subst) == VOIDmode)
		  original_subreg_reg_mode[i] = GET_MODE (old);
	      }
	    if (lra_dump_file != NULL)
	      {
		fprintf (lra_dump_file,
			 "Changing pseudo %d in operand %i of insn %u on equiv ",
			 REGNO (old), i, INSN_UID (curr_insn));
		dump_value_slim (lra_dump_file, subst, 1);
		fprintf (lra_dump_file, "\n");
	      }
	    op_change_p = change_p = true;
	  }
	if (simplify_operand_subreg (i, GET_MODE (old)) || op_change_p)
	  {
	    change_p = true;
	    lra_update_dup (curr_id, i);
	  }
      }

  /* Reload address registers and displacements.  We do it before
     finding an alternative because of memory constraints.  */
  before = after = NULL;
  for (i = 0; i < n_operands; i++)
    if (! curr_static_id->operand[i].is_operator
	&& process_address (i, check_only_p, &before, &after))
      {
	if (check_only_p)
	  return true;
	change_p = true;
	lra_update_dup (curr_id, i);
      }
  
  if (change_p)
    /* If we've changed the instruction then any alternative that
       we chose previously may no longer be valid.  */
    lra_set_used_insn_alternative (curr_insn, LRA_UNKNOWN_ALT);

  if (! check_only_p && curr_insn_set != NULL_RTX
      && check_and_process_move (&change_p, &sec_mem_p))
    return change_p;

 try_swapped:

  reused_alternative_num = check_only_p ? LRA_UNKNOWN_ALT : curr_id->used_insn_alternative;
  if (lra_dump_file != NULL && reused_alternative_num >= 0)
    fprintf (lra_dump_file, "Reusing alternative %d for insn #%u\n",
	     reused_alternative_num, INSN_UID (curr_insn));

  if (process_alt_operands (reused_alternative_num))
    alt_p = true;

  if (check_only_p)
    return ! alt_p || best_losers != 0;

  /* If insn is commutative (it's safe to exchange a certain pair of
     operands) then we need to try each alternative twice, the second
     time matching those two operands as if we had exchanged them.  To
     do this, really exchange them in operands.

     If we have just tried the alternatives the second time, return
     operands to normal and drop through.  */

  if (reused_alternative_num < 0 && commutative >= 0)
    {
      curr_swapped = !curr_swapped;
      if (curr_swapped)
	{
	  swap_operands (commutative);
	  goto try_swapped;
	}
      else
	swap_operands (commutative);
    }

  if (! alt_p && ! sec_mem_p)
    {
      /* No alternative works with reloads??  */
      if (INSN_CODE (curr_insn) >= 0)
	fatal_insn ("unable to generate reloads for:", curr_insn);
      error_for_asm (curr_insn,
		     "inconsistent operand constraints in an %<asm%>");
      lra_asm_error_p = true;
      /* Avoid further trouble with this insn.  Don't generate use
	 pattern here as we could use the insn SP offset.  */
      lra_set_insn_deleted (curr_insn);
      return true;
    }

  /* If the best alternative is with operands 1 and 2 swapped, swap
     them.  Update the operand numbers of any reloads already
     pushed.  */

  if (goal_alt_swapped)
    {
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "  Commutative operand exchange in insn %u\n",
		 INSN_UID (curr_insn));

      /* Swap the duplicates too.  */
      swap_operands (commutative);
      change_p = true;
    }

  /* Some targets' TARGET_SECONDARY_MEMORY_NEEDED (e.g. x86) are defined
     too conservatively.  So we use the secondary memory only if there
     is no any alternative without reloads.  */
  use_sec_mem_p = false;
  if (! alt_p)
    use_sec_mem_p = true;
  else if (sec_mem_p)
    {
      for (i = 0; i < n_operands; i++)
	if (! goal_alt_win[i] && ! goal_alt_match_win[i])
	  break;
      use_sec_mem_p = i < n_operands;
    }

  if (use_sec_mem_p)
    {
      int in = -1, out = -1;
      rtx new_reg, src, dest, rld;
      machine_mode sec_mode, rld_mode;

      lra_assert (curr_insn_set != NULL_RTX && sec_mem_p);
      dest = SET_DEST (curr_insn_set);
      src = SET_SRC (curr_insn_set);
      for (i = 0; i < n_operands; i++)
	if (*curr_id->operand_loc[i] == dest)
	  out = i;
	else if (*curr_id->operand_loc[i] == src)
	  in = i;
      for (i = 0; i < curr_static_id->n_dups; i++)
	if (out < 0 && *curr_id->dup_loc[i] == dest)
	  out = curr_static_id->dup_num[i];
	else if (in < 0 && *curr_id->dup_loc[i] == src)
	  in = curr_static_id->dup_num[i];
      lra_assert (out >= 0 && in >= 0
		  && curr_static_id->operand[out].type == OP_OUT
		  && curr_static_id->operand[in].type == OP_IN);
      rld = partial_subreg_p (GET_MODE (src), GET_MODE (dest)) ? src : dest;
      rld_mode = GET_MODE (rld);
      sec_mode = targetm.secondary_memory_needed_mode (rld_mode);
      new_reg = lra_create_new_reg (sec_mode, NULL_RTX,
				    NO_REGS, "secondary");
      /* If the mode is changed, it should be wider.  */
      lra_assert (!partial_subreg_p (sec_mode, rld_mode));
      if (sec_mode != rld_mode)
        {
	  /* If the target says specifically to use another mode for
	     secondary memory moves we cannot reuse the original
	     insn.  */
	  after = emit_spill_move (false, new_reg, dest);
	  lra_process_new_insns (curr_insn, NULL, after,
				 "Inserting the sec. move");
	  /* We may have non null BEFORE here (e.g. after address
	     processing.  */
	  push_to_sequence (before);
	  before = emit_spill_move (true, new_reg, src);
	  emit_insn (before);
	  before = get_insns ();
	  end_sequence ();
	  lra_process_new_insns (curr_insn, before, NULL, "Changing on");
	  lra_set_insn_deleted (curr_insn);
	}
      else if (dest == rld)
        {
	  *curr_id->operand_loc[out] = new_reg;
	  lra_update_dup (curr_id, out);
	  after = emit_spill_move (false, new_reg, dest);
	  lra_process_new_insns (curr_insn, NULL, after,
				 "Inserting the sec. move");
	}
      else
	{
	  *curr_id->operand_loc[in] = new_reg;
	  lra_update_dup (curr_id, in);
	  /* See comments above.  */
	  push_to_sequence (before);
	  before = emit_spill_move (true, new_reg, src);
	  emit_insn (before);
	  before = get_insns ();
	  end_sequence ();
	  lra_process_new_insns (curr_insn, before, NULL,
				 "Inserting the sec. move");
	}
      lra_update_insn_regno_info (curr_insn);
      return true;
    }

  lra_assert (goal_alt_number >= 0);
  lra_set_used_insn_alternative (curr_insn, goal_alt_number);

  if (lra_dump_file != NULL)
    {
      const char *p;

      fprintf (lra_dump_file, "	 Choosing alt %d in insn %u:",
	       goal_alt_number, INSN_UID (curr_insn));
      for (i = 0; i < n_operands; i++)
	{
	  p = (curr_static_id->operand_alternative
	       [goal_alt_number * n_operands + i].constraint);
	  if (*p == '\0')
	    continue;
	  fprintf (lra_dump_file, "  (%d) ", i);
	  for (; *p != '\0' && *p != ',' && *p != '#'; p++)
	    fputc (*p, lra_dump_file);
	}
      if (INSN_CODE (curr_insn) >= 0
          && (p = get_insn_name (INSN_CODE (curr_insn))) != NULL)
        fprintf (lra_dump_file, " {%s}", p);
      if (maybe_ne (curr_id->sp_offset, 0))
	{
	  fprintf (lra_dump_file, " (sp_off=");
	  print_dec (curr_id->sp_offset, lra_dump_file);
	  fprintf (lra_dump_file, ")");
	}
      fprintf (lra_dump_file, "\n");
    }

  /* Right now, for any pair of operands I and J that are required to
     match, with J < I, goal_alt_matches[I] is J.  Add I to
     goal_alt_matched[J].  */

  for (i = 0; i < n_operands; i++)
    if ((j = goal_alt_matches[i]) >= 0)
      {
	for (k = 0; goal_alt_matched[j][k] >= 0; k++)
	  ;
	/* We allow matching one output operand and several input
	   operands.  */
	lra_assert (k == 0
		    || (curr_static_id->operand[j].type == OP_OUT
			&& curr_static_id->operand[i].type == OP_IN
			&& (curr_static_id->operand
			    [goal_alt_matched[j][0]].type == OP_IN)));
	goal_alt_matched[j][k] = i;
	goal_alt_matched[j][k + 1] = -1;
      }

  for (i = 0; i < n_operands; i++)
    goal_alt_win[i] |= goal_alt_match_win[i];

  /* Any constants that aren't allowed and can't be reloaded into
     registers are here changed into memory references.	 */
  for (i = 0; i < n_operands; i++)
    if (goal_alt_win[i])
      {
	int regno;
	enum reg_class new_class;
	rtx reg = *curr_id->operand_loc[i];

	if (GET_CODE (reg) == SUBREG)
	  reg = SUBREG_REG (reg);

	if (REG_P (reg) && (regno = REGNO (reg)) >= FIRST_PSEUDO_REGISTER)
	  {
	    bool ok_p = in_class_p (reg, goal_alt[i], &new_class);

	    if (new_class != NO_REGS && get_reg_class (regno) != new_class)
	      {
		lra_assert (ok_p);
		lra_change_class (regno, new_class, "      Change to", true);
	      }
	  }
      }
    else
      {
	const char *constraint;
	char c;
	rtx op = *curr_id->operand_loc[i];
	rtx subreg = NULL_RTX;
	machine_mode mode = curr_operand_mode[i];

	if (GET_CODE (op) == SUBREG)
	  {
	    subreg = op;
	    op = SUBREG_REG (op);
	    mode = GET_MODE (op);
	  }

	if (CONST_POOL_OK_P (mode, op)
	    && ((targetm.preferred_reload_class
		 (op, (enum reg_class) goal_alt[i]) == NO_REGS)
		|| no_input_reloads_p))
	  {
	    rtx tem = force_const_mem (mode, op);

	    change_p = true;
	    if (subreg != NULL_RTX)
	      tem = gen_rtx_SUBREG (mode, tem, SUBREG_BYTE (subreg));

	    *curr_id->operand_loc[i] = tem;
	    lra_update_dup (curr_id, i);
	    process_address (i, false, &before, &after);

	    /* If the alternative accepts constant pool refs directly
	       there will be no reload needed at all.  */
	    if (subreg != NULL_RTX)
	      continue;
	    /* Skip alternatives before the one requested.  */
	    constraint = (curr_static_id->operand_alternative
			  [goal_alt_number * n_operands + i].constraint);
	    for (;
		 (c = *constraint) && c != ',' && c != '#';
		 constraint += CONSTRAINT_LEN (c, constraint))
	      {
		enum constraint_num cn = lookup_constraint (constraint);
		if ((insn_extra_memory_constraint (cn)
		     || insn_extra_special_memory_constraint (cn))
		    && satisfies_memory_constraint_p (tem, cn))
		  break;
	      }
	    if (c == '\0' || c == ',' || c == '#')
	      continue;

	    goal_alt_win[i] = true;
	  }
      }

  n_outputs = 0;
  outputs[0] = -1;
  for (i = 0; i < n_operands; i++)
    {
      int regno;
      bool optional_p = false;
      rtx old, new_reg;
      rtx op = *curr_id->operand_loc[i];

      if (goal_alt_win[i])
	{
	  if (goal_alt[i] == NO_REGS
	      && REG_P (op)
	      /* When we assign NO_REGS it means that we will not
		 assign a hard register to the scratch pseudo by
		 assigment pass and the scratch pseudo will be
		 spilled.  Spilled scratch pseudos are transformed
		 back to scratches at the LRA end.  */
	      && lra_former_scratch_operand_p (curr_insn, i)
	      && lra_former_scratch_p (REGNO (op)))
	    {
	      int regno = REGNO (op);
	      lra_change_class (regno, NO_REGS, "      Change to", true);
	      if (lra_get_regno_hard_regno (regno) >= 0)
		/* We don't have to mark all insn affected by the
		   spilled pseudo as there is only one such insn, the
		   current one.  */
		reg_renumber[regno] = -1;
	      lra_assert (bitmap_single_bit_set_p
			  (&lra_reg_info[REGNO (op)].insn_bitmap));
	    }
	  /* We can do an optional reload.  If the pseudo got a hard
	     reg, we might improve the code through inheritance.  If
	     it does not get a hard register we coalesce memory/memory
	     moves later.  Ignore move insns to avoid cycling.  */
	  if (! lra_simple_p
	      && lra_undo_inheritance_iter < LRA_MAX_INHERITANCE_PASSES
	      && goal_alt[i] != NO_REGS && REG_P (op)
	      && (regno = REGNO (op)) >= FIRST_PSEUDO_REGISTER
	      && regno < new_regno_start
	      && ! lra_former_scratch_p (regno)
	      && reg_renumber[regno] < 0
	      /* Check that the optional reload pseudo will be able to
		 hold given mode value.  */
	      && ! (prohibited_class_reg_set_mode_p
		    (goal_alt[i], reg_class_contents[goal_alt[i]],
		     PSEUDO_REGNO_MODE (regno)))
	      && (curr_insn_set == NULL_RTX
		  || !((REG_P (SET_SRC (curr_insn_set))
			|| MEM_P (SET_SRC (curr_insn_set))
			|| GET_CODE (SET_SRC (curr_insn_set)) == SUBREG)
		       && (REG_P (SET_DEST (curr_insn_set))
			   || MEM_P (SET_DEST (curr_insn_set))
			   || GET_CODE (SET_DEST (curr_insn_set)) == SUBREG))))
	    optional_p = true;
	  else if (goal_alt_matched[i][0] != -1
		   && curr_static_id->operand[i].type == OP_OUT
		   && (curr_static_id->operand_alternative
		       [goal_alt_number * n_operands + i].earlyclobber)
		   && REG_P (op))
	    {
	      for (j = 0; goal_alt_matched[i][j] != -1; j++)
		{
		  rtx op2 = *curr_id->operand_loc[goal_alt_matched[i][j]];
		  
		  if (REG_P (op2) && REGNO (op) != REGNO (op2))
		    break;
		}
	      if (goal_alt_matched[i][j] != -1)
		{
		  /* Generate reloads for different output and matched
		     input registers.  This is the easiest way to avoid
		     creation of non-existing register conflicts in
		     lra-lives.c.  */
		  match_reload (i, goal_alt_matched[i], outputs, goal_alt[i], &before,
				&after, TRUE);
		  outputs[n_outputs++] = i;
		  outputs[n_outputs] = -1;
		}
	      continue;
	    }
	  else
	    continue;
	}

      /* Operands that match previous ones have already been handled.  */
      if (goal_alt_matches[i] >= 0)
	continue;

      /* We should not have an operand with a non-offsettable address
	 appearing where an offsettable address will do.  It also may
	 be a case when the address should be special in other words
	 not a general one (e.g. it needs no index reg).  */
      if (goal_alt_matched[i][0] == -1 && goal_alt_offmemok[i] && MEM_P (op))
	{
	  enum reg_class rclass;
	  rtx *loc = &XEXP (op, 0);
	  enum rtx_code code = GET_CODE (*loc);

	  push_to_sequence (before);
	  rclass = base_reg_class (GET_MODE (op), MEM_ADDR_SPACE (op),
				   MEM, SCRATCH);
	  if (GET_RTX_CLASS (code) == RTX_AUTOINC)
	    new_reg = emit_inc (rclass, *loc, *loc,
				/* This value does not matter for MODIFY.  */
				GET_MODE_SIZE (GET_MODE (op)));
	  else if (get_reload_reg (OP_IN, Pmode, *loc, rclass, FALSE,
				   "offsetable address", &new_reg))
	    {
	      rtx addr = *loc;
	      enum rtx_code code = GET_CODE (addr);
	      
	      if (code == AND && CONST_INT_P (XEXP (addr, 1)))
		/* (and ... (const_int -X)) is used to align to X bytes.  */
		addr = XEXP (*loc, 0);
	      lra_emit_move (new_reg, addr);
	      if (addr != *loc)
		emit_move_insn (new_reg, gen_rtx_AND (GET_MODE (new_reg), new_reg, XEXP (*loc, 1)));
	    }
	  before = get_insns ();
	  end_sequence ();
	  *loc = new_reg;
	  lra_update_dup (curr_id, i);
	}
      else if (goal_alt_matched[i][0] == -1)
	{
	  machine_mode mode;
	  rtx reg, *loc;
	  int hard_regno;
	  enum op_type type = curr_static_id->operand[i].type;

	  loc = curr_id->operand_loc[i];
	  mode = curr_operand_mode[i];
	  if (GET_CODE (*loc) == SUBREG)
	    {
	      reg = SUBREG_REG (*loc);
	      poly_int64 byte = SUBREG_BYTE (*loc);
	      if (REG_P (reg)
		  /* Strict_low_part requires reloading the register and not
		     just the subreg.  Likewise for a strict subreg no wider
		     than a word for WORD_REGISTER_OPERATIONS targets.  */
		  && (curr_static_id->operand[i].strict_low
		      || (!paradoxical_subreg_p (mode, GET_MODE (reg))
			  && (hard_regno
			      = get_try_hard_regno (REGNO (reg))) >= 0
			  && (simplify_subreg_regno
			      (hard_regno,
			       GET_MODE (reg), byte, mode) < 0)
			  && (goal_alt[i] == NO_REGS
			      || (simplify_subreg_regno
				  (ira_class_hard_regs[goal_alt[i]][0],
				   GET_MODE (reg), byte, mode) >= 0)))
		      || (partial_subreg_p (mode, GET_MODE (reg))
			  && known_le (GET_MODE_SIZE (GET_MODE (reg)),
				       UNITS_PER_WORD)
			  && WORD_REGISTER_OPERATIONS)))
		{
		  /* An OP_INOUT is required when reloading a subreg of a
		     mode wider than a word to ensure that data beyond the
		     word being reloaded is preserved.  Also automatically
		     ensure that strict_low_part reloads are made into
		     OP_INOUT which should already be true from the backend
		     constraints.  */
		  if (type == OP_OUT
		      && (curr_static_id->operand[i].strict_low
			  || read_modify_subreg_p (*loc)))
		    type = OP_INOUT;
		  loc = &SUBREG_REG (*loc);
		  mode = GET_MODE (*loc);
		}
	    }
	  old = *loc;
	  if (get_reload_reg (type, mode, old, goal_alt[i],
			      loc != curr_id->operand_loc[i], "", &new_reg)
	      && type != OP_OUT)
	    {
	      push_to_sequence (before);
	      lra_emit_move (new_reg, old);
	      before = get_insns ();
	      end_sequence ();
	    }
	  *loc = new_reg;
	  if (type != OP_IN
	      && find_reg_note (curr_insn, REG_UNUSED, old) == NULL_RTX)
	    {
	      start_sequence ();
	      lra_emit_move (type == OP_INOUT ? copy_rtx (old) : old, new_reg);
	      emit_insn (after);
	      after = get_insns ();
	      end_sequence ();
	      *loc = new_reg;
	    }
	  for (j = 0; j < goal_alt_dont_inherit_ops_num; j++)
	    if (goal_alt_dont_inherit_ops[j] == i)
	      {
		lra_set_regno_unique_value (REGNO (new_reg));
		break;
	      }
	  lra_update_dup (curr_id, i);
	}
      else if (curr_static_id->operand[i].type == OP_IN
	       && (curr_static_id->operand[goal_alt_matched[i][0]].type
		   == OP_OUT
		   || (curr_static_id->operand[goal_alt_matched[i][0]].type
		       == OP_INOUT
		       && (operands_match_p
			   (*curr_id->operand_loc[i],
			    *curr_id->operand_loc[goal_alt_matched[i][0]],
			    -1)))))
	{
	  /* generate reloads for input and matched outputs.  */
	  match_inputs[0] = i;
	  match_inputs[1] = -1;
	  match_reload (goal_alt_matched[i][0], match_inputs, outputs,
			goal_alt[i], &before, &after,
			curr_static_id->operand_alternative
			[goal_alt_number * n_operands + goal_alt_matched[i][0]]
			.earlyclobber);
	}
      else if ((curr_static_id->operand[i].type == OP_OUT
		|| (curr_static_id->operand[i].type == OP_INOUT
		    && (operands_match_p
			(*curr_id->operand_loc[i],
			 *curr_id->operand_loc[goal_alt_matched[i][0]],
			 -1))))
	       && (curr_static_id->operand[goal_alt_matched[i][0]].type
		    == OP_IN))
	/* Generate reloads for output and matched inputs.  */
	match_reload (i, goal_alt_matched[i], outputs, goal_alt[i], &before,
		      &after, curr_static_id->operand_alternative
			      [goal_alt_number * n_operands + i].earlyclobber);
      else if (curr_static_id->operand[i].type == OP_IN
	       && (curr_static_id->operand[goal_alt_matched[i][0]].type
		   == OP_IN))
	{
	  /* Generate reloads for matched inputs.  */
	  match_inputs[0] = i;
	  for (j = 0; (k = goal_alt_matched[i][j]) >= 0; j++)
	    match_inputs[j + 1] = k;
	  match_inputs[j + 1] = -1;
	  match_reload (-1, match_inputs, outputs, goal_alt[i], &before,
			&after, false);
	}
      else
	/* We must generate code in any case when function
	   process_alt_operands decides that it is possible.  */
	gcc_unreachable ();

      /* Memorise processed outputs so that output remaining to be processed
	 can avoid using the same register value (see match_reload).  */
      if (curr_static_id->operand[i].type == OP_OUT)
	{
	  outputs[n_outputs++] = i;
	  outputs[n_outputs] = -1;
	}

      if (optional_p)
	{
	  rtx reg = op;

	  lra_assert (REG_P (reg));
	  regno = REGNO (reg);
	  op = *curr_id->operand_loc[i]; /* Substitution.  */
	  if (GET_CODE (op) == SUBREG)
	    op = SUBREG_REG (op);
	  gcc_assert (REG_P (op) && (int) REGNO (op) >= new_regno_start);
	  bitmap_set_bit (&lra_optional_reload_pseudos, REGNO (op));
	  lra_reg_info[REGNO (op)].restore_rtx = reg;
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file,
		     "      Making reload reg %d for reg %d optional\n",
		     REGNO (op), regno);
	}
    }
  if (before != NULL_RTX || after != NULL_RTX
      || max_regno_before != max_reg_num ())
    change_p = true;
  if (change_p)
    {
      lra_update_operator_dups (curr_id);
      /* Something changes -- process the insn.	 */
      lra_update_insn_regno_info (curr_insn);
    }
  lra_process_new_insns (curr_insn, before, after, "Inserting insn reload");
  return change_p;
}

/* Return true if INSN satisfies all constraints.  In other words, no
   reload insns are needed.  */
bool
lra_constrain_insn (rtx_insn *insn)
{
  int saved_new_regno_start = new_regno_start;
  int saved_new_insn_uid_start = new_insn_uid_start;
  bool change_p;

  curr_insn = insn;
  curr_id = lra_get_insn_recog_data (curr_insn);
  curr_static_id = curr_id->insn_static_data;
  new_insn_uid_start = get_max_uid ();
  new_regno_start = max_reg_num ();
  change_p = curr_insn_transform (true);
  new_regno_start = saved_new_regno_start;
  new_insn_uid_start = saved_new_insn_uid_start;
  return ! change_p;
}

/* Return true if X is in LIST.	 */
static bool
in_list_p (rtx x, rtx list)
{
  for (; list != NULL_RTX; list = XEXP (list, 1))
    if (XEXP (list, 0) == x)
      return true;
  return false;
}

/* Return true if X contains an allocatable hard register (if
   HARD_REG_P) or a (spilled if SPILLED_P) pseudo.  */
static bool
contains_reg_p (rtx x, bool hard_reg_p, bool spilled_p)
{
  int i, j;
  const char *fmt;
  enum rtx_code code;

  code = GET_CODE (x);
  if (REG_P (x))
    {
      int regno = REGNO (x);
      HARD_REG_SET alloc_regs;

      if (hard_reg_p)
	{
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = lra_get_regno_hard_regno (regno);
	  if (regno < 0)
	    return false;
	  COMPL_HARD_REG_SET (alloc_regs, lra_no_alloc_regs);
	  return overlaps_hard_reg_set_p (alloc_regs, GET_MODE (x), regno);
	}
      else
	{
	  if (regno < FIRST_PSEUDO_REGISTER)
	    return false;
	  if (! spilled_p)
	    return true;
	  return lra_get_regno_hard_regno (regno) < 0;
	}
    }
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (contains_reg_p (XEXP (x, i), hard_reg_p, spilled_p))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (contains_reg_p (XVECEXP (x, i, j), hard_reg_p, spilled_p))
	      return true;
	}
    }
  return false;
}

/* Process all regs in location *LOC and change them on equivalent
   substitution.  Return true if any change was done.  */
static bool
loc_equivalence_change_p (rtx *loc)
{
  rtx subst, reg, x = *loc;
  bool result = false;
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (code == SUBREG)
    {
      reg = SUBREG_REG (x);
      if ((subst = get_equiv_with_elimination (reg, curr_insn)) != reg
	  && GET_MODE (subst) == VOIDmode)
	{
	  /* We cannot reload debug location.  Simplify subreg here
	     while we know the inner mode.  */
	  *loc = simplify_gen_subreg (GET_MODE (x), subst,
				      GET_MODE (reg), SUBREG_BYTE (x));
	  return true;
	}
    }
  if (code == REG && (subst = get_equiv_with_elimination (x, curr_insn)) != x)
    {
      *loc = subst;
      return true;
    }

  /* Scan all the operand sub-expressions.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	result = loc_equivalence_change_p (&XEXP (x, i)) || result;
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  result
	    = loc_equivalence_change_p (&XVECEXP (x, i, j)) || result;
    }
  return result;
}

/* Similar to loc_equivalence_change_p, but for use as
   simplify_replace_fn_rtx callback.  DATA is insn for which the
   elimination is done.  If it null we don't do the elimination.  */
static rtx
loc_equivalence_callback (rtx loc, const_rtx, void *data)
{
  if (!REG_P (loc))
    return NULL_RTX;

  rtx subst = (data == NULL
	       ? get_equiv (loc) : get_equiv_with_elimination (loc, (rtx_insn *) data));
  if (subst != loc)
    return subst;

  return NULL_RTX;
}

/* Maximum number of generated reload insns per an insn.  It is for
   preventing this pass cycling in a bug case.	*/
#define MAX_RELOAD_INSNS_NUMBER LRA_MAX_INSN_RELOADS

/* The current iteration number of this LRA pass.  */
int lra_constraint_iter;

/* True if we substituted equiv which needs checking register
   allocation correctness because the equivalent value contains
   allocatable hard registers or when we restore multi-register
   pseudo.  */
bool lra_risky_transformations_p;

/* Return true if REGNO is referenced in more than one block.  */
static bool
multi_block_pseudo_p (int regno)
{
  basic_block bb = NULL;
  unsigned int uid;
  bitmap_iterator bi;

  if (regno < FIRST_PSEUDO_REGISTER)
    return false;

    EXECUTE_IF_SET_IN_BITMAP (&lra_reg_info[regno].insn_bitmap, 0, uid, bi)
      if (bb == NULL)
	bb = BLOCK_FOR_INSN (lra_insn_recog_data[uid]->insn);
      else if (BLOCK_FOR_INSN (lra_insn_recog_data[uid]->insn) != bb)
	return true;
    return false;
}

/* Return true if LIST contains a deleted insn.  */
static bool
contains_deleted_insn_p (rtx_insn_list *list)
{
  for (; list != NULL_RTX; list = list->next ())
    if (NOTE_P (list->insn ())
	&& NOTE_KIND (list->insn ()) == NOTE_INSN_DELETED)
      return true;
  return false;
}

/* Return true if X contains a pseudo dying in INSN.  */
static bool
dead_pseudo_p (rtx x, rtx_insn *insn)
{
  int i, j;
  const char *fmt;
  enum rtx_code code;

  if (REG_P (x))
    return (insn != NULL_RTX
	    && find_regno_note (insn, REG_DEAD, REGNO (x)) != NULL_RTX);
  code = GET_CODE (x);
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (dead_pseudo_p (XEXP (x, i), insn))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (dead_pseudo_p (XVECEXP (x, i, j), insn))
	      return true;
	}
    }
  return false;
}

/* Return true if INSN contains a dying pseudo in INSN right hand
   side.  */
static bool
insn_rhs_dead_pseudo_p (rtx_insn *insn)
{
  rtx set = single_set (insn);

  gcc_assert (set != NULL);
  return dead_pseudo_p (SET_SRC (set), insn);
}

/* Return true if any init insn of REGNO contains a dying pseudo in
   insn right hand side.  */
static bool
init_insn_rhs_dead_pseudo_p (int regno)
{
  rtx_insn_list *insns = ira_reg_equiv[regno].init_insns;

  if (insns == NULL)
    return false;
  for (; insns != NULL_RTX; insns = insns->next ())
    if (insn_rhs_dead_pseudo_p (insns->insn ()))
      return true;
  return false;
}

/* Return TRUE if REGNO has a reverse equivalence.  The equivalence is
   reverse only if we have one init insn with given REGNO as a
   source.  */
static bool
reverse_equiv_p (int regno)
{
  rtx_insn_list *insns = ira_reg_equiv[regno].init_insns;
  rtx set;

  if (insns == NULL)
    return false;
  if (! INSN_P (insns->insn ())
      || insns->next () != NULL)
    return false;
  if ((set = single_set (insns->insn ())) == NULL_RTX)
    return false;
  return REG_P (SET_SRC (set)) && (int) REGNO (SET_SRC (set)) == regno;
}

/* Return TRUE if REGNO was reloaded in an equivalence init insn.  We
   call this function only for non-reverse equivalence.  */
static bool
contains_reloaded_insn_p (int regno)
{
  rtx set;
  rtx_insn_list *list = ira_reg_equiv[regno].init_insns;

  for (; list != NULL; list = list->next ())
    if ((set = single_set (list->insn ())) == NULL_RTX
	|| ! REG_P (SET_DEST (set))
	|| (int) REGNO (SET_DEST (set)) != regno)
      return true;
  return false;
}

/* Entry function of LRA constraint pass.  Return true if the
   constraint pass did change the code.	 */
bool
lra_constraints (bool first_p)
{
  bool changed_p;
  int i, hard_regno, new_insns_num;
  unsigned int min_len, new_min_len, uid;
  rtx set, x, reg, dest_reg;
  basic_block last_bb;
  bitmap_iterator bi;

  lra_constraint_iter++;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "\n********** Local #%d: **********\n\n",
	     lra_constraint_iter);
  changed_p = false;
  if (pic_offset_table_rtx
      && REGNO (pic_offset_table_rtx) >= FIRST_PSEUDO_REGISTER)
    lra_risky_transformations_p = true;
  else
    /* On the first iteration we should check IRA assignment
       correctness.  In rare cases, the assignments can be wrong as
       early clobbers operands are ignored in IRA or usages of
       paradoxical sub-registers are not taken into account by
       IRA.  */
    lra_risky_transformations_p = first_p;
  new_insn_uid_start = get_max_uid ();
  new_regno_start = first_p ? lra_constraint_new_regno_start : max_reg_num ();
  /* Mark used hard regs for target stack size calulations.  */
  for (i = FIRST_PSEUDO_REGISTER; i < new_regno_start; i++)
    if (lra_reg_info[i].nrefs != 0
	&& (hard_regno = lra_get_regno_hard_regno (i)) >= 0)
      {
	int j, nregs;

	nregs = hard_regno_nregs (hard_regno, lra_reg_info[i].biggest_mode);
	for (j = 0; j < nregs; j++)
	  df_set_regs_ever_live (hard_regno + j, true);
      }
  /* Do elimination before the equivalence processing as we can spill
     some pseudos during elimination.  */
  lra_eliminate (false, first_p);
  auto_bitmap equiv_insn_bitmap (&reg_obstack);
  for (i = FIRST_PSEUDO_REGISTER; i < new_regno_start; i++)
    if (lra_reg_info[i].nrefs != 0)
      {
	ira_reg_equiv[i].profitable_p = true;
	reg = regno_reg_rtx[i];
	if (lra_get_regno_hard_regno (i) < 0 && (x = get_equiv (reg)) != reg)
	  {
	    bool pseudo_p = contains_reg_p (x, false, false);

	    /* After RTL transformation, we cannot guarantee that
	       pseudo in the substitution was not reloaded which might
	       make equivalence invalid.  For example, in reverse
	       equiv of p0

	       p0 <- ...
	       ...
	       equiv_mem <- p0

	       the memory address register was reloaded before the 2nd
	       insn.  */
	    if ((! first_p && pseudo_p)
		/* We don't use DF for compilation speed sake.  So it
		   is problematic to update live info when we use an
		   equivalence containing pseudos in more than one
		   BB.  */
		|| (pseudo_p && multi_block_pseudo_p (i))
		/* If an init insn was deleted for some reason, cancel
		   the equiv.  We could update the equiv insns after
		   transformations including an equiv insn deletion
		   but it is not worthy as such cases are extremely
		   rare.  */ 
		|| contains_deleted_insn_p (ira_reg_equiv[i].init_insns)
		/* If it is not a reverse equivalence, we check that a
		   pseudo in rhs of the init insn is not dying in the
		   insn.  Otherwise, the live info at the beginning of
		   the corresponding BB might be wrong after we
		   removed the insn.  When the equiv can be a
		   constant, the right hand side of the init insn can
		   be a pseudo.  */
		|| (! reverse_equiv_p (i)
		    && (init_insn_rhs_dead_pseudo_p (i)
			/* If we reloaded the pseudo in an equivalence
			   init insn, we cannot remove the equiv init
			   insns and the init insns might write into
			   const memory in this case.  */
			|| contains_reloaded_insn_p (i)))
		/* Prevent access beyond equivalent memory for
		   paradoxical subregs.  */
		|| (MEM_P (x)
		    && maybe_gt (GET_MODE_SIZE (lra_reg_info[i].biggest_mode),
				 GET_MODE_SIZE (GET_MODE (x))))
		|| (pic_offset_table_rtx
		    && ((CONST_POOL_OK_P (PSEUDO_REGNO_MODE (i), x)
			 && (targetm.preferred_reload_class
			     (x, lra_get_allocno_class (i)) == NO_REGS))
			|| contains_symbol_ref_p (x))))
	      ira_reg_equiv[i].defined_p = false;
	    if (contains_reg_p (x, false, true))
	      ira_reg_equiv[i].profitable_p = false;
	    if (get_equiv (reg) != reg)
	      bitmap_ior_into (equiv_insn_bitmap, &lra_reg_info[i].insn_bitmap);
	  }
      }
  for (i = FIRST_PSEUDO_REGISTER; i < new_regno_start; i++)
    update_equiv (i);
  /* We should add all insns containing pseudos which should be
     substituted by their equivalences.  */
  EXECUTE_IF_SET_IN_BITMAP (equiv_insn_bitmap, 0, uid, bi)
    lra_push_insn_by_uid (uid);
  min_len = lra_insn_stack_length ();
  new_insns_num = 0;
  last_bb = NULL;
  changed_p = false;
  while ((new_min_len = lra_insn_stack_length ()) != 0)
    {
      curr_insn = lra_pop_insn ();
      --new_min_len;
      curr_bb = BLOCK_FOR_INSN (curr_insn);
      if (curr_bb != last_bb)
	{
	  last_bb = curr_bb;
	  bb_reload_num = lra_curr_reload_num;
	}
      if (min_len > new_min_len)
	{
	  min_len = new_min_len;
	  new_insns_num = 0;
	}
      if (new_insns_num > MAX_RELOAD_INSNS_NUMBER)
	internal_error
	  ("Max. number of generated reload insns per insn is achieved (%d)\n",
	   MAX_RELOAD_INSNS_NUMBER);
      new_insns_num++;
      if (DEBUG_INSN_P (curr_insn))
	{
	  /* We need to check equivalence in debug insn and change
	     pseudo to the equivalent value if necessary.  */
	  curr_id = lra_get_insn_recog_data (curr_insn);
	  if (bitmap_bit_p (equiv_insn_bitmap, INSN_UID (curr_insn)))
	    {
	      rtx old = *curr_id->operand_loc[0];
	      *curr_id->operand_loc[0]
		= simplify_replace_fn_rtx (old, NULL_RTX,
					   loc_equivalence_callback, curr_insn);
	      if (old != *curr_id->operand_loc[0])
		{
		  lra_update_insn_regno_info (curr_insn);
		  changed_p = true;
		}
	    }
	}
      else if (INSN_P (curr_insn))
	{
	  if ((set = single_set (curr_insn)) != NULL_RTX)
	    {
	      dest_reg = SET_DEST (set);
	      /* The equivalence pseudo could be set up as SUBREG in a
		 case when it is a call restore insn in a mode
		 different from the pseudo mode.  */
	      if (GET_CODE (dest_reg) == SUBREG)
		dest_reg = SUBREG_REG (dest_reg);
	      if ((REG_P (dest_reg)
		   && (x = get_equiv (dest_reg)) != dest_reg
		   /* Remove insns which set up a pseudo whose value
		      cannot be changed.  Such insns might be not in
		      init_insns because we don't update equiv data
		      during insn transformations.
		      
		      As an example, let suppose that a pseudo got
		      hard register and on the 1st pass was not
		      changed to equivalent constant.  We generate an
		      additional insn setting up the pseudo because of
		      secondary memory movement.  Then the pseudo is
		      spilled and we use the equiv constant.  In this
		      case we should remove the additional insn and
		      this insn is not init_insns list.  */
		   && (! MEM_P (x) || MEM_READONLY_P (x)
		       /* Check that this is actually an insn setting
			  up the equivalence.  */
		       || in_list_p (curr_insn,
				     ira_reg_equiv
				     [REGNO (dest_reg)].init_insns)))
		  || (((x = get_equiv (SET_SRC (set))) != SET_SRC (set))
		      && in_list_p (curr_insn,
				    ira_reg_equiv
				    [REGNO (SET_SRC (set))].init_insns)))
		{
		  /* This is equiv init insn of pseudo which did not get a
		     hard register -- remove the insn.	*/
		  if (lra_dump_file != NULL)
		    {
		      fprintf (lra_dump_file,
			       "      Removing equiv init insn %i (freq=%d)\n",
			       INSN_UID (curr_insn),
			       REG_FREQ_FROM_BB (BLOCK_FOR_INSN (curr_insn)));
		      dump_insn_slim (lra_dump_file, curr_insn);
		    }
		  if (contains_reg_p (x, true, false))
		    lra_risky_transformations_p = true;
		  lra_set_insn_deleted (curr_insn);
		  continue;
		}
	    }
	  curr_id = lra_get_insn_recog_data (curr_insn);
	  curr_static_id = curr_id->insn_static_data;
	  init_curr_insn_input_reloads ();
	  init_curr_operand_mode ();
	  if (curr_insn_transform (false))
	    changed_p = true;
	  /* Check non-transformed insns too for equiv change as USE
	     or CLOBBER don't need reloads but can contain pseudos
	     being changed on their equivalences.  */
	  else if (bitmap_bit_p (equiv_insn_bitmap, INSN_UID (curr_insn))
		   && loc_equivalence_change_p (&PATTERN (curr_insn)))
	    {
	      lra_update_insn_regno_info (curr_insn);
	      changed_p = true;
	    }
	}
    }

  /* If we used a new hard regno, changed_p should be true because the
     hard reg is assigned to a new pseudo.  */
  if (flag_checking && !changed_p)
    {
      for (i = FIRST_PSEUDO_REGISTER; i < new_regno_start; i++)
	if (lra_reg_info[i].nrefs != 0
	    && (hard_regno = lra_get_regno_hard_regno (i)) >= 0)
	  {
	    int j, nregs = hard_regno_nregs (hard_regno,
					     PSEUDO_REGNO_MODE (i));

	    for (j = 0; j < nregs; j++)
	      lra_assert (df_regs_ever_live_p (hard_regno + j));
	  }
    }
  return changed_p;
}

static void initiate_invariants (void);
static void finish_invariants (void);

/* Initiate the LRA constraint pass.  It is done once per
   function.  */
void
lra_constraints_init (void)
{
  initiate_invariants ();
}

/* Finalize the LRA constraint pass.  It is done once per
   function.  */
void
lra_constraints_finish (void)
{
  finish_invariants ();
}



/* Structure describes invariants for ineheritance.  */
struct lra_invariant
{
  /* The order number of the invariant.  */
  int num;
  /* The invariant RTX.  */
  rtx invariant_rtx;
  /* The origin insn of the invariant.  */
  rtx_insn *insn;
};

typedef lra_invariant invariant_t;
typedef invariant_t *invariant_ptr_t;
typedef const invariant_t *const_invariant_ptr_t;

/* Pointer to the inheritance invariants.  */
static vec<invariant_ptr_t> invariants;

/* Allocation pool for the invariants.  */
static object_allocator<lra_invariant> *invariants_pool;

/* Hash table for the invariants.  */
static htab_t invariant_table;

/* Hash function for INVARIANT.  */
static hashval_t
invariant_hash (const void *invariant)
{
  rtx inv = ((const_invariant_ptr_t) invariant)->invariant_rtx;
  return lra_rtx_hash (inv);
}

/* Equal function for invariants INVARIANT1 and INVARIANT2.  */
static int
invariant_eq_p (const void *invariant1, const void *invariant2)
{
  rtx inv1 = ((const_invariant_ptr_t) invariant1)->invariant_rtx;
  rtx inv2 = ((const_invariant_ptr_t) invariant2)->invariant_rtx;

  return rtx_equal_p (inv1, inv2);
}

/* Insert INVARIANT_RTX into the table if it is not there yet.  Return
   invariant which is in the table.  */
static invariant_ptr_t
insert_invariant (rtx invariant_rtx)
{
  void **entry_ptr;
  invariant_t invariant;
  invariant_ptr_t invariant_ptr;

  invariant.invariant_rtx = invariant_rtx;
  entry_ptr = htab_find_slot (invariant_table, &invariant, INSERT);
  if (*entry_ptr == NULL)
    {
      invariant_ptr = invariants_pool->allocate ();
      invariant_ptr->invariant_rtx = invariant_rtx;
      invariant_ptr->insn = NULL;
      invariants.safe_push (invariant_ptr);
      *entry_ptr = (void *) invariant_ptr;
    }
  return (invariant_ptr_t) *entry_ptr;
}

/* Initiate the invariant table.  */
static void
initiate_invariants (void)
{
  invariants.create (100);
  invariants_pool
    = new object_allocator<lra_invariant> ("Inheritance invariants");
  invariant_table = htab_create (100, invariant_hash, invariant_eq_p, NULL);
}

/* Finish the invariant table.  */
static void
finish_invariants (void)
{
  htab_delete (invariant_table);
  delete invariants_pool;
  invariants.release ();
}

/* Make the invariant table empty.  */
static void
clear_invariants (void)
{
  htab_empty (invariant_table);
  invariants_pool->release ();
  invariants.truncate (0);
}



/* This page contains code to do inheritance/split
   transformations.  */

/* Number of reloads passed so far in current EBB.  */
static int reloads_num;

/* Number of calls passed so far in current EBB.  */
static int calls_num;

/* Current reload pseudo check for validity of elements in
   USAGE_INSNS.	 */
static int curr_usage_insns_check;

/* Info about last usage of registers in EBB to do inheritance/split
   transformation.  Inheritance transformation is done from a spilled
   pseudo and split transformations from a hard register or a pseudo
   assigned to a hard register.	 */
struct usage_insns
{
  /* If the value is equal to CURR_USAGE_INSNS_CHECK, then the member
     value INSNS is valid.  The insns is chain of optional debug insns
     and a finishing non-debug insn using the corresponding reg.  The
     value is also used to mark the registers which are set up in the
     current insn.  The negated insn uid is used for this.  */
  int check;
  /* Value of global reloads_num at the last insn in INSNS.  */
  int reloads_num;
  /* Value of global reloads_nums at the last insn in INSNS.  */
  int calls_num;
  /* It can be true only for splitting.	 And it means that the restore
     insn should be put after insn given by the following member.  */
  bool after_p;
  /* Next insns in the current EBB which use the original reg and the
     original reg value is not changed between the current insn and
     the next insns.  In order words, e.g. for inheritance, if we need
     to use the original reg value again in the next insns we can try
     to use the value in a hard register from a reload insn of the
     current insn.  */
  rtx insns;
};

/* Map: regno -> corresponding pseudo usage insns.  */
static struct usage_insns *usage_insns;

static void
setup_next_usage_insn (int regno, rtx insn, int reloads_num, bool after_p)
{
  usage_insns[regno].check = curr_usage_insns_check;
  usage_insns[regno].insns = insn;
  usage_insns[regno].reloads_num = reloads_num;
  usage_insns[regno].calls_num = calls_num;
  usage_insns[regno].after_p = after_p;
}

/* The function is used to form list REGNO usages which consists of
   optional debug insns finished by a non-debug insn using REGNO.
   RELOADS_NUM is current number of reload insns processed so far.  */
static void
add_next_usage_insn (int regno, rtx_insn *insn, int reloads_num)
{
  rtx next_usage_insns;

  if (usage_insns[regno].check == curr_usage_insns_check
      && (next_usage_insns = usage_insns[regno].insns) != NULL_RTX
      && DEBUG_INSN_P (insn))
    {
      /* Check that we did not add the debug insn yet.	*/
      if (next_usage_insns != insn
	  && (GET_CODE (next_usage_insns) != INSN_LIST
	      || XEXP (next_usage_insns, 0) != insn))
	usage_insns[regno].insns = gen_rtx_INSN_LIST (VOIDmode, insn,
						      next_usage_insns);
    }
  else if (NONDEBUG_INSN_P (insn))
    setup_next_usage_insn (regno, insn, reloads_num, false);
  else
    usage_insns[regno].check = 0;
}

/* Return first non-debug insn in list USAGE_INSNS.  */
static rtx_insn *
skip_usage_debug_insns (rtx usage_insns)
{
  rtx insn;

  /* Skip debug insns.  */
  for (insn = usage_insns;
       insn != NULL_RTX && GET_CODE (insn) == INSN_LIST;
       insn = XEXP (insn, 1))
    ;
  return safe_as_a <rtx_insn *> (insn);
}

/* Return true if we need secondary memory moves for insn in
   USAGE_INSNS after inserting inherited pseudo of class INHER_CL
   into the insn.  */
static bool
check_secondary_memory_needed_p (enum reg_class inher_cl ATTRIBUTE_UNUSED,
				 rtx usage_insns ATTRIBUTE_UNUSED)
{
  rtx_insn *insn;
  rtx set, dest;
  enum reg_class cl;

  if (inher_cl == ALL_REGS
      || (insn = skip_usage_debug_insns (usage_insns)) == NULL_RTX)
    return false;
  lra_assert (INSN_P (insn));
  if ((set = single_set (insn)) == NULL_RTX || ! REG_P (SET_DEST (set)))
    return false;
  dest = SET_DEST (set);
  if (! REG_P (dest))
    return false;
  lra_assert (inher_cl != NO_REGS);
  cl = get_reg_class (REGNO (dest));
  return (cl != NO_REGS && cl != ALL_REGS
	  && targetm.secondary_memory_needed (GET_MODE (dest), inher_cl, cl));
}

/* Registers involved in inheritance/split in the current EBB
   (inheritance/split pseudos and original registers).	*/
static bitmap_head check_only_regs;

/* Reload pseudos cannot be involded in invariant inheritance in the
   current EBB.  */
static bitmap_head invalid_invariant_regs;

/* Do inheritance transformations for insn INSN, which defines (if
   DEF_P) or uses ORIGINAL_REGNO.  NEXT_USAGE_INSNS specifies which
   instruction in the EBB next uses ORIGINAL_REGNO; it has the same
   form as the "insns" field of usage_insns.  Return true if we
   succeed in such transformation.

   The transformations look like:

     p <- ...		  i <- ...
     ...		  p <- i    (new insn)
     ...	     =>
     <- ... p ...	  <- ... i ...
   or
     ...		  i <- p    (new insn)
     <- ... p ...	  <- ... i ...
     ...	     =>
     <- ... p ...	  <- ... i ...
   where p is a spilled original pseudo and i is a new inheritance pseudo.


   The inheritance pseudo has the smallest class of two classes CL and
   class of ORIGINAL REGNO.  */
static bool
inherit_reload_reg (bool def_p, int original_regno,
		    enum reg_class cl, rtx_insn *insn, rtx next_usage_insns)
{
  if (optimize_function_for_size_p (cfun))
    return false;

  enum reg_class rclass = lra_get_allocno_class (original_regno);
  rtx original_reg = regno_reg_rtx[original_regno];
  rtx new_reg, usage_insn;
  rtx_insn *new_insns;

  lra_assert (! usage_insns[original_regno].after_p);
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n");
  if (! ira_reg_classes_intersect_p[cl][rclass])
    {
      if (lra_dump_file != NULL)
	{
	  fprintf (lra_dump_file,
		   "    Rejecting inheritance for %d "
		   "because of disjoint classes %s and %s\n",
		   original_regno, reg_class_names[cl],
		   reg_class_names[rclass]);
	  fprintf (lra_dump_file,
		   "    >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
	}
      return false;
    }
  if ((ira_class_subset_p[cl][rclass] && cl != rclass)
      /* We don't use a subset of two classes because it can be
	 NO_REGS.  This transformation is still profitable in most
	 cases even if the classes are not intersected as register
	 move is probably cheaper than a memory load.  */
      || ira_class_hard_regs_num[cl] < ira_class_hard_regs_num[rclass])
    {
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "    Use smallest class of %s and %s\n",
		 reg_class_names[cl], reg_class_names[rclass]);

      rclass = cl;
    }
  if (check_secondary_memory_needed_p (rclass, next_usage_insns))
    {
      /* Reject inheritance resulting in secondary memory moves.
	 Otherwise, there is a danger in LRA cycling.  Also such
	 transformation will be unprofitable.  */
      if (lra_dump_file != NULL)
	{
	  rtx_insn *insn = skip_usage_debug_insns (next_usage_insns);
	  rtx set = single_set (insn);

	  lra_assert (set != NULL_RTX);

	  rtx dest = SET_DEST (set);

	  lra_assert (REG_P (dest));
	  fprintf (lra_dump_file,
		   "    Rejecting inheritance for insn %d(%s)<-%d(%s) "
		   "as secondary mem is needed\n",
		   REGNO (dest), reg_class_names[get_reg_class (REGNO (dest))],
		   original_regno, reg_class_names[rclass]);
	  fprintf (lra_dump_file,
		   "    >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
	}
      return false;
    }
  new_reg = lra_create_new_reg (GET_MODE (original_reg), original_reg,
				rclass, "inheritance");
  start_sequence ();
  if (def_p)
    lra_emit_move (original_reg, new_reg);
  else
    lra_emit_move (new_reg, original_reg);
  new_insns = get_insns ();
  end_sequence ();
  if (NEXT_INSN (new_insns) != NULL_RTX)
    {
      if (lra_dump_file != NULL)
	{
	  fprintf (lra_dump_file,
		   "    Rejecting inheritance %d->%d "
		   "as it results in 2 or more insns:\n",
		   original_regno, REGNO (new_reg));
	  dump_rtl_slim (lra_dump_file, new_insns, NULL, -1, 0);
	  fprintf (lra_dump_file,
		   "	>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
	}
      return false;
    }
  lra_substitute_pseudo_within_insn (insn, original_regno, new_reg, false);
  lra_update_insn_regno_info (insn);
  if (! def_p)
    /* We now have a new usage insn for original regno.  */
    setup_next_usage_insn (original_regno, new_insns, reloads_num, false);
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "    Original reg change %d->%d (bb%d):\n",
	     original_regno, REGNO (new_reg), BLOCK_FOR_INSN (insn)->index);
  lra_reg_info[REGNO (new_reg)].restore_rtx = regno_reg_rtx[original_regno];
  bitmap_set_bit (&check_only_regs, REGNO (new_reg));
  bitmap_set_bit (&check_only_regs, original_regno);
  bitmap_set_bit (&lra_inheritance_pseudos, REGNO (new_reg));
  if (def_p)
    lra_process_new_insns (insn, NULL, new_insns,
			   "Add original<-inheritance");
  else
    lra_process_new_insns (insn, new_insns, NULL,
			   "Add inheritance<-original");
  while (next_usage_insns != NULL_RTX)
    {
      if (GET_CODE (next_usage_insns) != INSN_LIST)
	{
	  usage_insn = next_usage_insns;
	  lra_assert (NONDEBUG_INSN_P (usage_insn));
	  next_usage_insns = NULL;
	}
      else
	{
	  usage_insn = XEXP (next_usage_insns, 0);
	  lra_assert (DEBUG_INSN_P (usage_insn));
	  next_usage_insns = XEXP (next_usage_insns, 1);
	}
      lra_substitute_pseudo (&usage_insn, original_regno, new_reg, false,
			     DEBUG_INSN_P (usage_insn));
      lra_update_insn_regno_info (as_a <rtx_insn *> (usage_insn));
      if (lra_dump_file != NULL)
	{
	  basic_block bb = BLOCK_FOR_INSN (usage_insn);
	  fprintf (lra_dump_file,
		   "    Inheritance reuse change %d->%d (bb%d):\n",
		   original_regno, REGNO (new_reg),
		   bb ? bb->index : -1);
	  dump_insn_slim (lra_dump_file, as_a <rtx_insn *> (usage_insn));
	}
    }
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "	  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
  return true;
}

/* Return true if we need a caller save/restore for pseudo REGNO which
   was assigned to a hard register.  */
static inline bool
need_for_call_save_p (int regno)
{
  lra_assert (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] >= 0);
  return (usage_insns[regno].calls_num < calls_num
	  && (overlaps_hard_reg_set_p
	      ((flag_ipa_ra &&
		! hard_reg_set_empty_p (lra_reg_info[regno].actual_call_used_reg_set))
	       ? lra_reg_info[regno].actual_call_used_reg_set
	       : call_used_reg_set,
	       PSEUDO_REGNO_MODE (regno), reg_renumber[regno])
	      || (targetm.hard_regno_call_part_clobbered
		  (lra_reg_info[regno].call_insn,
		   reg_renumber[regno], PSEUDO_REGNO_MODE (regno)))));
}

/* Global registers occurring in the current EBB.  */
static bitmap_head ebb_global_regs;

/* Return true if we need a split for hard register REGNO or pseudo
   REGNO which was assigned to a hard register.
   POTENTIAL_RELOAD_HARD_REGS contains hard registers which might be
   used for reloads since the EBB end.	It is an approximation of the
   used hard registers in the split range.  The exact value would
   require expensive calculations.  If we were aggressive with
   splitting because of the approximation, the split pseudo will save
   the same hard register assignment and will be removed in the undo
   pass.  We still need the approximation because too aggressive
   splitting would result in too inaccurate cost calculation in the
   assignment pass because of too many generated moves which will be
   probably removed in the undo pass.  */
static inline bool
need_for_split_p (HARD_REG_SET potential_reload_hard_regs, int regno)
{
  int hard_regno = regno < FIRST_PSEUDO_REGISTER ? regno : reg_renumber[regno];

  lra_assert (hard_regno >= 0);
  return ((TEST_HARD_REG_BIT (potential_reload_hard_regs, hard_regno)
	   /* Don't split eliminable hard registers, otherwise we can
	      split hard registers like hard frame pointer, which
	      lives on BB start/end according to DF-infrastructure,
	      when there is a pseudo assigned to the register and
	      living in the same BB.  */
	   && (regno >= FIRST_PSEUDO_REGISTER
	       || ! TEST_HARD_REG_BIT (eliminable_regset, hard_regno))
	   && ! TEST_HARD_REG_BIT (lra_no_alloc_regs, hard_regno)
	   /* Don't split call clobbered hard regs living through
	      calls, otherwise we might have a check problem in the
	      assign sub-pass as in the most cases (exception is a
	      situation when lra_risky_transformations_p value is
	      true) the assign pass assumes that all pseudos living
	      through calls are assigned to call saved hard regs.  */
	   && (regno >= FIRST_PSEUDO_REGISTER
	       || ! TEST_HARD_REG_BIT (call_used_reg_set, regno)
	       || usage_insns[regno].calls_num == calls_num)
	   /* We need at least 2 reloads to make pseudo splitting
	      profitable.  We should provide hard regno splitting in
	      any case to solve 1st insn scheduling problem when
	      moving hard register definition up might result in
	      impossibility to find hard register for reload pseudo of
	      small register class.  */
	   && (usage_insns[regno].reloads_num
	       + (regno < FIRST_PSEUDO_REGISTER ? 0 : 3) < reloads_num)
	   && (regno < FIRST_PSEUDO_REGISTER
	       /* For short living pseudos, spilling + inheritance can
		  be considered a substitution for splitting.
		  Therefore we do not splitting for local pseudos.  It
		  decreases also aggressiveness of splitting.  The
		  minimal number of references is chosen taking into
		  account that for 2 references splitting has no sense
		  as we can just spill the pseudo.  */
	       || (regno >= FIRST_PSEUDO_REGISTER
		   && lra_reg_info[regno].nrefs > 3
		   && bitmap_bit_p (&ebb_global_regs, regno))))
	  || (regno >= FIRST_PSEUDO_REGISTER && need_for_call_save_p (regno)));
}

/* Return class for the split pseudo created from original pseudo with
   ALLOCNO_CLASS and MODE which got a hard register HARD_REGNO.	 We
   choose subclass of ALLOCNO_CLASS which contains HARD_REGNO and
   results in no secondary memory movements.  */
static enum reg_class
choose_split_class (enum reg_class allocno_class,
		    int hard_regno ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED)
{
  int i;
  enum reg_class cl, best_cl = NO_REGS;
  enum reg_class hard_reg_class ATTRIBUTE_UNUSED
    = REGNO_REG_CLASS (hard_regno);

  if (! targetm.secondary_memory_needed (mode, allocno_class, allocno_class)
      && TEST_HARD_REG_BIT (reg_class_contents[allocno_class], hard_regno))
    return allocno_class;
  for (i = 0;
       (cl = reg_class_subclasses[allocno_class][i]) != LIM_REG_CLASSES;
       i++)
    if (! targetm.secondary_memory_needed (mode, cl, hard_reg_class)
	&& ! targetm.secondary_memory_needed (mode, hard_reg_class, cl)
	&& TEST_HARD_REG_BIT (reg_class_contents[cl], hard_regno)
	&& (best_cl == NO_REGS
	    || ira_class_hard_regs_num[best_cl] < ira_class_hard_regs_num[cl]))
      best_cl = cl;
  return best_cl;
}

/* Copy any equivalence information from ORIGINAL_REGNO to NEW_REGNO.
   It only makes sense to call this function if NEW_REGNO is always
   equal to ORIGINAL_REGNO.  */

static void
lra_copy_reg_equiv (unsigned int new_regno, unsigned int original_regno)
{
  if (!ira_reg_equiv[original_regno].defined_p)
    return;

  ira_expand_reg_equiv ();
  ira_reg_equiv[new_regno].defined_p = true;
  if (ira_reg_equiv[original_regno].memory)
    ira_reg_equiv[new_regno].memory
      = copy_rtx (ira_reg_equiv[original_regno].memory);
  if (ira_reg_equiv[original_regno].constant)
    ira_reg_equiv[new_regno].constant
      = copy_rtx (ira_reg_equiv[original_regno].constant);
  if (ira_reg_equiv[original_regno].invariant)
    ira_reg_equiv[new_regno].invariant
      = copy_rtx (ira_reg_equiv[original_regno].invariant);
}

/* Do split transformations for insn INSN, which defines or uses
   ORIGINAL_REGNO.  NEXT_USAGE_INSNS specifies which instruction in
   the EBB next uses ORIGINAL_REGNO; it has the same form as the
   "insns" field of usage_insns.  If TO is not NULL, we don't use
   usage_insns, we put restore insns after TO insn.  It is a case when
   we call it from lra_split_hard_reg_for, outside the inheritance
   pass.

   The transformations look like:

     p <- ...		  p <- ...
     ...		  s <- p    (new insn -- save)
     ...	     =>
     ...		  p <- s    (new insn -- restore)
     <- ... p ...	  <- ... p ...
   or
     <- ... p ...	  <- ... p ...
     ...		  s <- p    (new insn -- save)
     ...	     =>
     ...		  p <- s    (new insn -- restore)
     <- ... p ...	  <- ... p ...

   where p is an original pseudo got a hard register or a hard
   register and s is a new split pseudo.  The save is put before INSN
   if BEFORE_P is true.	 Return true if we succeed in such
   transformation.  */
static bool
split_reg (bool before_p, int original_regno, rtx_insn *insn,
	   rtx next_usage_insns, rtx_insn *to)
{
  enum reg_class rclass;
  rtx original_reg;
  int hard_regno, nregs;
  rtx new_reg, usage_insn;
  rtx_insn *restore, *save;
  bool after_p;
  bool call_save_p;
  machine_mode mode;

  if (original_regno < FIRST_PSEUDO_REGISTER)
    {
      rclass = ira_allocno_class_translate[REGNO_REG_CLASS (original_regno)];
      hard_regno = original_regno;
      call_save_p = false;
      nregs = 1;
      mode = lra_reg_info[hard_regno].biggest_mode;
      machine_mode reg_rtx_mode = GET_MODE (regno_reg_rtx[hard_regno]);
      /* A reg can have a biggest_mode of VOIDmode if it was only ever seen
	 as part of a multi-word register.  In that case, or if the biggest
	 mode was larger than a register, just use the reg_rtx.  Otherwise,
	 limit the size to that of the biggest access in the function.  */
      if (mode == VOIDmode
	  || paradoxical_subreg_p (mode, reg_rtx_mode))
	{
	  original_reg = regno_reg_rtx[hard_regno];
	  mode = reg_rtx_mode;
	}
      else
	original_reg = gen_rtx_REG (mode, hard_regno);
    }
  else
    {
      mode = PSEUDO_REGNO_MODE (original_regno);
      hard_regno = reg_renumber[original_regno];
      nregs = hard_regno_nregs (hard_regno, mode);
      rclass = lra_get_allocno_class (original_regno);
      original_reg = regno_reg_rtx[original_regno];
      call_save_p = need_for_call_save_p (original_regno);
    }
  lra_assert (hard_regno >= 0);
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "	  ((((((((((((((((((((((((((((((((((((((((((((((((\n");
	  
  if (call_save_p)
    {
      mode = HARD_REGNO_CALLER_SAVE_MODE (hard_regno,
					  hard_regno_nregs (hard_regno, mode),
					  mode);
      new_reg = lra_create_new_reg (mode, NULL_RTX, NO_REGS, "save");
    }
  else
    {
      rclass = choose_split_class (rclass, hard_regno, mode);
      if (rclass == NO_REGS)
	{
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file,
		       "    Rejecting split of %d(%s): "
		       "no good reg class for %d(%s)\n",
		       original_regno,
		       reg_class_names[lra_get_allocno_class (original_regno)],
		       hard_regno,
		       reg_class_names[REGNO_REG_CLASS (hard_regno)]);
	      fprintf
		(lra_dump_file,
		 "    ))))))))))))))))))))))))))))))))))))))))))))))))\n");
	    }
	  return false;
	}
      /* Split_if_necessary can split hard registers used as part of a
	 multi-register mode but splits each register individually.  The
	 mode used for each independent register may not be supported
	 so reject the split.  Splitting the wider mode should theoretically
	 be possible but is not implemented.  */
      if (!targetm.hard_regno_mode_ok (hard_regno, mode))
	{
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file,
		       "    Rejecting split of %d(%s): unsuitable mode %s\n",
		       original_regno,
		       reg_class_names[lra_get_allocno_class (original_regno)],
		       GET_MODE_NAME (mode));
	      fprintf
		(lra_dump_file,
		 "    ))))))))))))))))))))))))))))))))))))))))))))))))\n");
	    }
	  return false;
	}
      new_reg = lra_create_new_reg (mode, original_reg, rclass, "split");
      reg_renumber[REGNO (new_reg)] = hard_regno;
    }
  int new_regno = REGNO (new_reg);
  save = emit_spill_move (true, new_reg, original_reg);
  if (NEXT_INSN (save) != NULL_RTX && !call_save_p)
    {
      if (lra_dump_file != NULL)
	{
	  fprintf
	    (lra_dump_file,
	     "	  Rejecting split %d->%d resulting in > 2 save insns:\n",
	     original_regno, new_regno);
	  dump_rtl_slim (lra_dump_file, save, NULL, -1, 0);
	  fprintf (lra_dump_file,
		   "	))))))))))))))))))))))))))))))))))))))))))))))))\n");
	}
      return false;
    }
  restore = emit_spill_move (false, new_reg, original_reg);
  if (NEXT_INSN (restore) != NULL_RTX && !call_save_p)
    {
      if (lra_dump_file != NULL)
	{
	  fprintf (lra_dump_file,
		   "	Rejecting split %d->%d "
		   "resulting in > 2 restore insns:\n",
		   original_regno, new_regno);
	  dump_rtl_slim (lra_dump_file, restore, NULL, -1, 0);
	  fprintf (lra_dump_file,
		   "	))))))))))))))))))))))))))))))))))))))))))))))))\n");
	}
      return false;
    }
  /* Transfer equivalence information to the spill register, so that
     if we fail to allocate the spill register, we have the option of
     rematerializing the original value instead of spilling to the stack.  */
  if (!HARD_REGISTER_NUM_P (original_regno)
      && mode == PSEUDO_REGNO_MODE (original_regno))
    lra_copy_reg_equiv (new_regno, original_regno);
  lra_reg_info[new_regno].restore_rtx = regno_reg_rtx[original_regno];
  bitmap_set_bit (&lra_split_regs, new_regno);
  if (to != NULL)
    {
      lra_assert (next_usage_insns == NULL);
      usage_insn = to;
      after_p = TRUE;
    }
  else
    {
      /* We need check_only_regs only inside the inheritance pass.  */
      bitmap_set_bit (&check_only_regs, new_regno);
      bitmap_set_bit (&check_only_regs, original_regno);
      after_p = usage_insns[original_regno].after_p;
      for (;;)
	{
	  if (GET_CODE (next_usage_insns) != INSN_LIST)
	    {
	      usage_insn = next_usage_insns;
	      break;
	    }
	  usage_insn = XEXP (next_usage_insns, 0);
	  lra_assert (DEBUG_INSN_P (usage_insn));
	  next_usage_insns = XEXP (next_usage_insns, 1);
	  lra_substitute_pseudo (&usage_insn, original_regno, new_reg, false,
				 true);
	  lra_update_insn_regno_info (as_a <rtx_insn *> (usage_insn));
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file, "    Split reuse change %d->%d:\n",
		       original_regno, new_regno);
	      dump_insn_slim (lra_dump_file, as_a <rtx_insn *> (usage_insn));
	    }
	}
    }
  lra_assert (NOTE_P (usage_insn) || NONDEBUG_INSN_P (usage_insn));
  lra_assert (usage_insn != insn || (after_p && before_p));
  lra_process_new_insns (as_a <rtx_insn *> (usage_insn),
			 after_p ? NULL : restore,
			 after_p ? restore : NULL,
			 call_save_p
			 ?  "Add reg<-save" : "Add reg<-split");
  lra_process_new_insns (insn, before_p ? save : NULL,
			 before_p ? NULL : save,
			 call_save_p
			 ?  "Add save<-reg" : "Add split<-reg");
  if (nregs > 1)
    /* If we are trying to split multi-register.  We should check
       conflicts on the next assignment sub-pass.  IRA can allocate on
       sub-register levels, LRA do this on pseudos level right now and
       this discrepancy may create allocation conflicts after
       splitting.  */
    lra_risky_transformations_p = true;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "	  ))))))))))))))))))))))))))))))))))))))))))))))))\n");
  return true;
}

/* Split a hard reg for reload pseudo REGNO having RCLASS and living
   in the range [FROM, TO].  Return true if did a split.  Otherwise,
   return false.  */
bool
spill_hard_reg_in_range (int regno, enum reg_class rclass, rtx_insn *from, rtx_insn *to)
{
  int i, hard_regno;
  int rclass_size;
  rtx_insn *insn;
  unsigned int uid;
  bitmap_iterator bi;
  HARD_REG_SET ignore;
  
  lra_assert (from != NULL && to != NULL);
  CLEAR_HARD_REG_SET (ignore);
  EXECUTE_IF_SET_IN_BITMAP (&lra_reg_info[regno].insn_bitmap, 0, uid, bi)
    {
      lra_insn_recog_data_t id = lra_insn_recog_data[uid];
      struct lra_static_insn_data *static_id = id->insn_static_data;
      struct lra_insn_reg *reg;
      
      for (reg = id->regs; reg != NULL; reg = reg->next)
	if (reg->regno < FIRST_PSEUDO_REGISTER)
	  SET_HARD_REG_BIT (ignore, reg->regno);
      for (reg = static_id->hard_regs; reg != NULL; reg = reg->next)
	SET_HARD_REG_BIT (ignore, reg->regno);
    }
  rclass_size = ira_class_hard_regs_num[rclass];
  for (i = 0; i < rclass_size; i++)
    {
      hard_regno = ira_class_hard_regs[rclass][i];
      if (! TEST_HARD_REG_BIT (lra_reg_info[regno].conflict_hard_regs, hard_regno)
	  || TEST_HARD_REG_BIT (ignore, hard_regno))
	continue;
      for (insn = from; insn != NEXT_INSN (to); insn = NEXT_INSN (insn))
	{
	  struct lra_static_insn_data *static_id;
	  struct lra_insn_reg *reg;

	  if (!INSN_P (insn))
	      continue;
	  if (bitmap_bit_p (&lra_reg_info[hard_regno].insn_bitmap,
			    INSN_UID (insn)))
	    break;
	  static_id = lra_get_insn_recog_data (insn)->insn_static_data;
	  for (reg = static_id->hard_regs; reg != NULL; reg = reg->next)
	    if (reg->regno == hard_regno)
	      break;
	  if (reg != NULL)
	    break;
	}
      if (insn != NEXT_INSN (to))
	continue;
      if (split_reg (TRUE, hard_regno, from, NULL, to))
	return true;
    }
  return false;
}

/* Recognize that we need a split transformation for insn INSN, which
   defines or uses REGNO in its insn biggest MODE (we use it only if
   REGNO is a hard register).  POTENTIAL_RELOAD_HARD_REGS contains
   hard registers which might be used for reloads since the EBB end.
   Put the save before INSN if BEFORE_P is true.  MAX_UID is maximla
   uid before starting INSN processing.  Return true if we succeed in
   such transformation.  */
static bool
split_if_necessary (int regno, machine_mode mode,
		    HARD_REG_SET potential_reload_hard_regs,
		    bool before_p, rtx_insn *insn, int max_uid)
{
  bool res = false;
  int i, nregs = 1;
  rtx next_usage_insns;

  if (regno < FIRST_PSEUDO_REGISTER)
    nregs = hard_regno_nregs (regno, mode);
  for (i = 0; i < nregs; i++)
    if (usage_insns[regno + i].check == curr_usage_insns_check
	&& (next_usage_insns = usage_insns[regno + i].insns) != NULL_RTX
	/* To avoid processing the register twice or more.  */
	&& ((GET_CODE (next_usage_insns) != INSN_LIST
	     && INSN_UID (next_usage_insns) < max_uid)
	    || (GET_CODE (next_usage_insns) == INSN_LIST
		&& (INSN_UID (XEXP (next_usage_insns, 0)) < max_uid)))
	&& need_for_split_p (potential_reload_hard_regs, regno + i)
	&& split_reg (before_p, regno + i, insn, next_usage_insns, NULL))
    res = true;
  return res;
}

/* Return TRUE if rtx X is considered as an invariant for
   inheritance.  */
static bool
invariant_p (const_rtx x)
{
  machine_mode mode;
  const char *fmt;
  enum rtx_code code;
  int i, j;

  if (side_effects_p (x))
    return false;

  code = GET_CODE (x);
  mode = GET_MODE (x);
  if (code == SUBREG)
    {
      x = SUBREG_REG (x);
      code = GET_CODE (x);
      mode = wider_subreg_mode (mode, GET_MODE (x));
    }

  if (MEM_P (x))
    return false;

  if (REG_P (x))
    {
      int i, nregs, regno = REGNO (x);

      if (regno >= FIRST_PSEUDO_REGISTER || regno == STACK_POINTER_REGNUM
	  || TEST_HARD_REG_BIT (eliminable_regset, regno)
	  || GET_MODE_CLASS (GET_MODE (x)) == MODE_CC)
	return false;
      nregs = hard_regno_nregs (regno, mode);
      for (i = 0; i < nregs; i++)
	if (! fixed_regs[regno + i]
	    /* A hard register may be clobbered in the current insn
	       but we can ignore this case because if the hard
	       register is used it should be set somewhere after the
	       clobber.  */
	    || bitmap_bit_p (&invalid_invariant_regs, regno + i))
	  return false;
    }
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (! invariant_p (XEXP (x, i)))
	    return false;
	}
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (! invariant_p (XVECEXP (x, i, j)))
	      return false;
	}
    }
  return true;
}

/* We have 'dest_reg <- invariant'.  Let us try to make an invariant
   inheritance transformation (using dest_reg instead invariant in a
   subsequent insn).  */
static bool
process_invariant_for_inheritance (rtx dst_reg, rtx invariant_rtx)
{
  invariant_ptr_t invariant_ptr;
  rtx_insn *insn, *new_insns;
  rtx insn_set, insn_reg, new_reg;
  int insn_regno;
  bool succ_p = false;
  int dst_regno = REGNO (dst_reg);
  machine_mode dst_mode = GET_MODE (dst_reg);
  enum reg_class cl = lra_get_allocno_class (dst_regno), insn_reg_cl;

  invariant_ptr = insert_invariant (invariant_rtx);
  if ((insn = invariant_ptr->insn) != NULL_RTX)
    {
      /* We have a subsequent insn using the invariant.  */
      insn_set = single_set (insn);
      lra_assert (insn_set != NULL);
      insn_reg = SET_DEST (insn_set);
      lra_assert (REG_P (insn_reg));
      insn_regno = REGNO (insn_reg);
      insn_reg_cl = lra_get_allocno_class (insn_regno);

      if (dst_mode == GET_MODE (insn_reg)
	  /* We should consider only result move reg insns which are
	     cheap.  */
	  && targetm.register_move_cost (dst_mode, cl, insn_reg_cl) == 2
	  && targetm.register_move_cost (dst_mode, cl, cl) == 2)
	{
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file,
		     "    [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[\n");
	  new_reg = lra_create_new_reg (dst_mode, dst_reg,
					cl, "invariant inheritance");
	  bitmap_set_bit (&lra_inheritance_pseudos, REGNO (new_reg));
	  bitmap_set_bit (&check_only_regs, REGNO (new_reg));
	  lra_reg_info[REGNO (new_reg)].restore_rtx = PATTERN (insn);
	  start_sequence ();
	  lra_emit_move (new_reg, dst_reg);
	  new_insns = get_insns ();
	  end_sequence ();
	  lra_process_new_insns (curr_insn, NULL, new_insns,
				 "Add invariant inheritance<-original");
	  start_sequence ();
	  lra_emit_move (SET_DEST (insn_set), new_reg);
	  new_insns = get_insns ();
	  end_sequence ();
	  lra_process_new_insns (insn, NULL, new_insns,
				 "Changing reload<-inheritance");
	  lra_set_insn_deleted (insn);
	  succ_p = true;
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file,
		       "    Invariant inheritance reuse change %d (bb%d):\n",
		       REGNO (new_reg), BLOCK_FOR_INSN (insn)->index);
	      dump_insn_slim (lra_dump_file, insn);
	      fprintf (lra_dump_file,
		       "	  ]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]\n");
	    }
	}
    }
  invariant_ptr->insn = curr_insn;
  return succ_p;
}

/* Check only registers living at the current program point in the
   current EBB.	 */
static bitmap_head live_regs;

/* Update live info in EBB given by its HEAD and TAIL insns after
   inheritance/split transformation.  The function removes dead moves
   too.	 */
static void
update_ebb_live_info (rtx_insn *head, rtx_insn *tail)
{
  unsigned int j;
  int i, regno;
  bool live_p;
  rtx_insn *prev_insn;
  rtx set;
  bool remove_p;
  basic_block last_bb, prev_bb, curr_bb;
  bitmap_iterator bi;
  struct lra_insn_reg *reg;
  edge e;
  edge_iterator ei;

  last_bb = BLOCK_FOR_INSN (tail);
  prev_bb = NULL;
  for (curr_insn = tail;
       curr_insn != PREV_INSN (head);
       curr_insn = prev_insn)
    {
      prev_insn = PREV_INSN (curr_insn);
      /* We need to process empty blocks too.  They contain
	 NOTE_INSN_BASIC_BLOCK referring for the basic block.  */
      if (NOTE_P (curr_insn) && NOTE_KIND (curr_insn) != NOTE_INSN_BASIC_BLOCK)
	continue;
      curr_bb = BLOCK_FOR_INSN (curr_insn);
      if (curr_bb != prev_bb)
	{
	  if (prev_bb != NULL)
	    {
	      /* Update df_get_live_in (prev_bb):  */
	      EXECUTE_IF_SET_IN_BITMAP (&check_only_regs, 0, j, bi)
		if (bitmap_bit_p (&live_regs, j))
		  bitmap_set_bit (df_get_live_in (prev_bb), j);
		else
		  bitmap_clear_bit (df_get_live_in (prev_bb), j);
	    }
	  if (curr_bb != last_bb)
	    {
	      /* Update df_get_live_out (curr_bb):  */
	      EXECUTE_IF_SET_IN_BITMAP (&check_only_regs, 0, j, bi)
		{
		  live_p = bitmap_bit_p (&live_regs, j);
		  if (! live_p)
		    FOR_EACH_EDGE (e, ei, curr_bb->succs)
		      if (bitmap_bit_p (df_get_live_in (e->dest), j))
			{
			  live_p = true;
			  break;
			}
		  if (live_p)
		    bitmap_set_bit (df_get_live_out (curr_bb), j);
		  else
		    bitmap_clear_bit (df_get_live_out (curr_bb), j);
		}
	    }
	  prev_bb = curr_bb;
	  bitmap_and (&live_regs, &check_only_regs, df_get_live_out (curr_bb));
	}
      if (! NONDEBUG_INSN_P (curr_insn))
	continue;
      curr_id = lra_get_insn_recog_data (curr_insn);
      curr_static_id = curr_id->insn_static_data;
      remove_p = false;
      if ((set = single_set (curr_insn)) != NULL_RTX
	  && REG_P (SET_DEST (set))
	  && (regno = REGNO (SET_DEST (set))) >= FIRST_PSEUDO_REGISTER
	  && SET_DEST (set) != pic_offset_table_rtx
	  && bitmap_bit_p (&check_only_regs, regno)
	  && ! bitmap_bit_p (&live_regs, regno))
	remove_p = true;
      /* See which defined values die here.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type == OP_OUT && ! reg->subreg_p)
	  bitmap_clear_bit (&live_regs, reg->regno);
      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type == OP_OUT && ! reg->subreg_p)
	  bitmap_clear_bit (&live_regs, reg->regno);
      if (curr_id->arg_hard_regs != NULL)
	/* Make clobbered argument hard registers die.  */
	for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    bitmap_clear_bit (&live_regs, regno - FIRST_PSEUDO_REGISTER);
      /* Mark each used value as live.  */
      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_OUT
	    && bitmap_bit_p (&check_only_regs, reg->regno))
	  bitmap_set_bit (&live_regs, reg->regno);
      for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	if (reg->type != OP_OUT
	    && bitmap_bit_p (&check_only_regs, reg->regno))
	  bitmap_set_bit (&live_regs, reg->regno);
      if (curr_id->arg_hard_regs != NULL)
	/* Make used argument hard registers live.  */
	for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	  if (regno < FIRST_PSEUDO_REGISTER
	      && bitmap_bit_p (&check_only_regs, regno))
	    bitmap_set_bit (&live_regs, regno);
      /* It is quite important to remove dead move insns because it
	 means removing dead store.  We don't need to process them for
	 constraints.  */
      if (remove_p)
	{
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file, "	    Removing dead insn:\n ");
	      dump_insn_slim (lra_dump_file, curr_insn);
	    }
	  lra_set_insn_deleted (curr_insn);
	}
    }
}

/* The structure describes info to do an inheritance for the current
   insn.  We need to collect such info first before doing the
   transformations because the transformations change the insn
   internal representation.  */
struct to_inherit
{
  /* Original regno.  */
  int regno;
  /* Subsequent insns which can inherit original reg value.  */
  rtx insns;
};

/* Array containing all info for doing inheritance from the current
   insn.  */
static struct to_inherit to_inherit[LRA_MAX_INSN_RELOADS];

/* Number elements in the previous array.  */
static int to_inherit_num;

/* Add inheritance info REGNO and INSNS. Their meaning is described in
   structure to_inherit.  */
static void
add_to_inherit (int regno, rtx insns)
{
  int i;

  for (i = 0; i < to_inherit_num; i++)
    if (to_inherit[i].regno == regno)
      return;
  lra_assert (to_inherit_num < LRA_MAX_INSN_RELOADS);
  to_inherit[to_inherit_num].regno = regno;
  to_inherit[to_inherit_num++].insns = insns;
}

/* Return the last non-debug insn in basic block BB, or the block begin
   note if none.  */
static rtx_insn *
get_last_insertion_point (basic_block bb)
{
  rtx_insn *insn;

  FOR_BB_INSNS_REVERSE (bb, insn)
    if (NONDEBUG_INSN_P (insn) || NOTE_INSN_BASIC_BLOCK_P (insn))
      return insn;
  gcc_unreachable ();
}

/* Set up RES by registers living on edges FROM except the edge (FROM,
   TO) or by registers set up in a jump insn in BB FROM.  */
static void
get_live_on_other_edges (basic_block from, basic_block to, bitmap res)
{
  rtx_insn *last;
  struct lra_insn_reg *reg;
  edge e;
  edge_iterator ei;

  lra_assert (to != NULL);
  bitmap_clear (res);
  FOR_EACH_EDGE (e, ei, from->succs)
    if (e->dest != to)
      bitmap_ior_into (res, df_get_live_in (e->dest));
  last = get_last_insertion_point (from);
  if (! JUMP_P (last))
    return;
  curr_id = lra_get_insn_recog_data (last);
  for (reg = curr_id->regs; reg != NULL; reg = reg->next)
    if (reg->type != OP_IN)
      bitmap_set_bit (res, reg->regno);
}

/* Used as a temporary results of some bitmap calculations.  */
static bitmap_head temp_bitmap;

/* We split for reloads of small class of hard regs.  The following
   defines how many hard regs the class should have to be qualified as
   small.  The code is mostly oriented to x86/x86-64 architecture
   where some insns need to use only specific register or pair of
   registers and these register can live in RTL explicitly, e.g. for
   parameter passing.  */
static const int max_small_class_regs_num = 2;

/* Do inheritance/split transformations in EBB starting with HEAD and
   finishing on TAIL.  We process EBB insns in the reverse order.
   Return true if we did any inheritance/split transformation in the
   EBB.

   We should avoid excessive splitting which results in worse code
   because of inaccurate cost calculations for spilling new split
   pseudos in such case.  To achieve this we do splitting only if
   register pressure is high in given basic block and there are reload
   pseudos requiring hard registers.  We could do more register
   pressure calculations at any given program point to avoid necessary
   splitting even more but it is to expensive and the current approach
   works well enough.  */
static bool
inherit_in_ebb (rtx_insn *head, rtx_insn *tail)
{
  int i, src_regno, dst_regno, nregs;
  bool change_p, succ_p, update_reloads_num_p;
  rtx_insn *prev_insn, *last_insn;
  rtx next_usage_insns, curr_set;
  enum reg_class cl;
  struct lra_insn_reg *reg;
  basic_block last_processed_bb, curr_bb = NULL;
  HARD_REG_SET potential_reload_hard_regs, live_hard_regs;
  bitmap to_process;
  unsigned int j;
  bitmap_iterator bi;
  bool head_p, after_p;

  change_p = false;
  curr_usage_insns_check++;
  clear_invariants ();
  reloads_num = calls_num = 0;
  bitmap_clear (&check_only_regs);
  bitmap_clear (&invalid_invariant_regs);
  last_processed_bb = NULL;
  CLEAR_HARD_REG_SET (potential_reload_hard_regs);
  COPY_HARD_REG_SET (live_hard_regs, eliminable_regset);
  IOR_HARD_REG_SET (live_hard_regs, lra_no_alloc_regs);
  /* We don't process new insns generated in the loop.	*/
  for (curr_insn = tail; curr_insn != PREV_INSN (head); curr_insn = prev_insn)
    {
      prev_insn = PREV_INSN (curr_insn);
      if (BLOCK_FOR_INSN (curr_insn) != NULL)
	curr_bb = BLOCK_FOR_INSN (curr_insn);
      if (last_processed_bb != curr_bb)
	{
	  /* We are at the end of BB.  Add qualified living
	     pseudos for potential splitting.  */
	  to_process = df_get_live_out (curr_bb);
	  if (last_processed_bb != NULL)
	    {
	      /* We are somewhere in the middle of EBB.	 */
	      get_live_on_other_edges (curr_bb, last_processed_bb,
				       &temp_bitmap);
	      to_process = &temp_bitmap;
	    }
	  last_processed_bb = curr_bb;
	  last_insn = get_last_insertion_point (curr_bb);
	  after_p = (! JUMP_P (last_insn)
		     && (! CALL_P (last_insn)
			 || (find_reg_note (last_insn,
					   REG_NORETURN, NULL_RTX) == NULL_RTX
			     && ! SIBLING_CALL_P (last_insn))));
	  CLEAR_HARD_REG_SET (potential_reload_hard_regs);
	  EXECUTE_IF_SET_IN_BITMAP (to_process, 0, j, bi)
	    {
	      if ((int) j >= lra_constraint_new_regno_start)
		break;
	      if (j < FIRST_PSEUDO_REGISTER || reg_renumber[j] >= 0)
		{
		  if (j < FIRST_PSEUDO_REGISTER)
		    SET_HARD_REG_BIT (live_hard_regs, j);
		  else
		    add_to_hard_reg_set (&live_hard_regs,
					 PSEUDO_REGNO_MODE (j),
					 reg_renumber[j]);
		  setup_next_usage_insn (j, last_insn, reloads_num, after_p);
		}
	    }
	}
      src_regno = dst_regno = -1;
      curr_set = single_set (curr_insn);
      if (curr_set != NULL_RTX && REG_P (SET_DEST (curr_set)))
	dst_regno = REGNO (SET_DEST (curr_set));
      if (curr_set != NULL_RTX && REG_P (SET_SRC (curr_set)))
	src_regno = REGNO (SET_SRC (curr_set));
      update_reloads_num_p = true;
      if (src_regno < lra_constraint_new_regno_start
	  && src_regno >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[src_regno] < 0
	  && dst_regno >= lra_constraint_new_regno_start
	  && (cl = lra_get_allocno_class (dst_regno)) != NO_REGS)
	{
	  /* 'reload_pseudo <- original_pseudo'.  */
	  if (ira_class_hard_regs_num[cl] <= max_small_class_regs_num)
	    reloads_num++;
	  update_reloads_num_p = false;
	  succ_p = false;
	  if (usage_insns[src_regno].check == curr_usage_insns_check
	      && (next_usage_insns = usage_insns[src_regno].insns) != NULL_RTX)
	    succ_p = inherit_reload_reg (false, src_regno, cl,
					 curr_insn, next_usage_insns);
	  if (succ_p)
	    change_p = true;
	  else
	    setup_next_usage_insn (src_regno, curr_insn, reloads_num, false);
	  if (hard_reg_set_subset_p (reg_class_contents[cl], live_hard_regs))
	    IOR_HARD_REG_SET (potential_reload_hard_regs,
			      reg_class_contents[cl]);
	}
      else if (src_regno < 0
	       && dst_regno >= lra_constraint_new_regno_start
	       && invariant_p (SET_SRC (curr_set))
	       && (cl = lra_get_allocno_class (dst_regno)) != NO_REGS
	       && ! bitmap_bit_p (&invalid_invariant_regs, dst_regno)
	       && ! bitmap_bit_p (&invalid_invariant_regs,
				  ORIGINAL_REGNO(regno_reg_rtx[dst_regno])))
	{
	  /* 'reload_pseudo <- invariant'.  */
	  if (ira_class_hard_regs_num[cl] <= max_small_class_regs_num)
	    reloads_num++;
	  update_reloads_num_p = false;
	  if (process_invariant_for_inheritance (SET_DEST (curr_set), SET_SRC (curr_set)))
	    change_p = true;
	  if (hard_reg_set_subset_p (reg_class_contents[cl], live_hard_regs))
	    IOR_HARD_REG_SET (potential_reload_hard_regs,
			      reg_class_contents[cl]);
	}
      else if (src_regno >= lra_constraint_new_regno_start
	       && dst_regno < lra_constraint_new_regno_start
	       && dst_regno >= FIRST_PSEUDO_REGISTER
	       && reg_renumber[dst_regno] < 0
	       && (cl = lra_get_allocno_class (src_regno)) != NO_REGS
	       && usage_insns[dst_regno].check == curr_usage_insns_check
	       && (next_usage_insns
		   = usage_insns[dst_regno].insns) != NULL_RTX)
	{
	  if (ira_class_hard_regs_num[cl] <= max_small_class_regs_num)
	    reloads_num++;
	  update_reloads_num_p = false;
	  /* 'original_pseudo <- reload_pseudo'.  */
	  if (! JUMP_P (curr_insn)
	      && inherit_reload_reg (true, dst_regno, cl,
				     curr_insn, next_usage_insns))
	    change_p = true;
	  /* Invalidate.  */
	  usage_insns[dst_regno].check = 0;
	  if (hard_reg_set_subset_p (reg_class_contents[cl], live_hard_regs))
	    IOR_HARD_REG_SET (potential_reload_hard_regs,
			      reg_class_contents[cl]);
	}
      else if (INSN_P (curr_insn))
	{
	  int iter;
	  int max_uid = get_max_uid ();

	  curr_id = lra_get_insn_recog_data (curr_insn);
	  curr_static_id = curr_id->insn_static_data;
	  to_inherit_num = 0;
	  /* Process insn definitions.	*/
	  for (iter = 0; iter < 2; iter++)
	    for (reg = iter == 0 ? curr_id->regs : curr_static_id->hard_regs;
		 reg != NULL;
		 reg = reg->next)
	      if (reg->type != OP_IN
		  && (dst_regno = reg->regno) < lra_constraint_new_regno_start)
		{
		  if (dst_regno >= FIRST_PSEUDO_REGISTER && reg->type == OP_OUT
		      && reg_renumber[dst_regno] < 0 && ! reg->subreg_p
		      && usage_insns[dst_regno].check == curr_usage_insns_check
		      && (next_usage_insns
			  = usage_insns[dst_regno].insns) != NULL_RTX)
		    {
		      struct lra_insn_reg *r;

		      for (r = curr_id->regs; r != NULL; r = r->next)
			if (r->type != OP_OUT && r->regno == dst_regno)
			  break;
		      /* Don't do inheritance if the pseudo is also
			 used in the insn.  */
		      if (r == NULL)
			/* We cannot do inheritance right now
			   because the current insn reg info (chain
			   regs) can change after that.  */
			add_to_inherit (dst_regno, next_usage_insns);
		    }
		  /* We cannot process one reg twice here because of
		     usage_insns invalidation.  */
		  if ((dst_regno < FIRST_PSEUDO_REGISTER
		       || reg_renumber[dst_regno] >= 0)
		      && ! reg->subreg_p && reg->type != OP_IN)
		    {
		      HARD_REG_SET s;

		      if (split_if_necessary (dst_regno, reg->biggest_mode,
					      potential_reload_hard_regs,
					      false, curr_insn, max_uid))
			change_p = true;
		      CLEAR_HARD_REG_SET (s);
		      if (dst_regno < FIRST_PSEUDO_REGISTER)
			add_to_hard_reg_set (&s, reg->biggest_mode, dst_regno);
		      else
			add_to_hard_reg_set (&s, PSEUDO_REGNO_MODE (dst_regno),
					     reg_renumber[dst_regno]);
		      AND_COMPL_HARD_REG_SET (live_hard_regs, s);
		      AND_COMPL_HARD_REG_SET (potential_reload_hard_regs, s);
		    }
		  /* We should invalidate potential inheritance or
		     splitting for the current insn usages to the next
		     usage insns (see code below) as the output pseudo
		     prevents this.  */
		  if ((dst_regno >= FIRST_PSEUDO_REGISTER
		       && reg_renumber[dst_regno] < 0)
		      || (reg->type == OP_OUT && ! reg->subreg_p
			  && (dst_regno < FIRST_PSEUDO_REGISTER
			      || reg_renumber[dst_regno] >= 0)))
		    {
		      /* Invalidate and mark definitions.  */
		      if (dst_regno >= FIRST_PSEUDO_REGISTER)
			usage_insns[dst_regno].check = -(int) INSN_UID (curr_insn);
		      else
			{
			  nregs = hard_regno_nregs (dst_regno,
						    reg->biggest_mode);
			  for (i = 0; i < nregs; i++)
			    usage_insns[dst_regno + i].check
			      = -(int) INSN_UID (curr_insn);
			}
		    }
		}
	  /* Process clobbered call regs.  */
	  if (curr_id->arg_hard_regs != NULL)
	    for (i = 0; (dst_regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	      if (dst_regno >= FIRST_PSEUDO_REGISTER)
		usage_insns[dst_regno - FIRST_PSEUDO_REGISTER].check
		  = -(int) INSN_UID (curr_insn);
	  if (! JUMP_P (curr_insn))
	    for (i = 0; i < to_inherit_num; i++)
	      if (inherit_reload_reg (true, to_inherit[i].regno,
				      ALL_REGS, curr_insn,
				      to_inherit[i].insns))
	      change_p = true;
	  if (CALL_P (curr_insn))
	    {
	      rtx cheap, pat, dest;
	      rtx_insn *restore;
	      int regno, hard_regno;

	      calls_num++;
	      if ((cheap = find_reg_note (curr_insn,
					  REG_RETURNED, NULL_RTX)) != NULL_RTX
		  && ((cheap = XEXP (cheap, 0)), true)
		  && (regno = REGNO (cheap)) >= FIRST_PSEUDO_REGISTER
		  && (hard_regno = reg_renumber[regno]) >= 0
		  && usage_insns[regno].check == curr_usage_insns_check
		  /* If there are pending saves/restores, the
		     optimization is not worth.	 */
		  && usage_insns[regno].calls_num == calls_num - 1
		  && TEST_HARD_REG_BIT (call_used_reg_set, hard_regno))
		{
		  /* Restore the pseudo from the call result as
		     REG_RETURNED note says that the pseudo value is
		     in the call result and the pseudo is an argument
		     of the call.  */
		  pat = PATTERN (curr_insn);
		  if (GET_CODE (pat) == PARALLEL)
		    pat = XVECEXP (pat, 0, 0);
		  dest = SET_DEST (pat);
		  /* For multiple return values dest is PARALLEL.
		     Currently we handle only single return value case.  */
		  if (REG_P (dest))
		    {
		      start_sequence ();
		      emit_move_insn (cheap, copy_rtx (dest));
		      restore = get_insns ();
		      end_sequence ();
		      lra_process_new_insns (curr_insn, NULL, restore,
					     "Inserting call parameter restore");
		      /* We don't need to save/restore of the pseudo from
			 this call.	 */
		      usage_insns[regno].calls_num = calls_num;
		      bitmap_set_bit (&check_only_regs, regno);
		    }
		}
	    }
	  to_inherit_num = 0;
	  /* Process insn usages.  */
	  for (iter = 0; iter < 2; iter++)
	    for (reg = iter == 0 ? curr_id->regs : curr_static_id->hard_regs;
		 reg != NULL;
		 reg = reg->next)
	      if ((reg->type != OP_OUT
		   || (reg->type == OP_OUT && reg->subreg_p))
		  && (src_regno = reg->regno) < lra_constraint_new_regno_start)
		{
		  if (src_regno >= FIRST_PSEUDO_REGISTER
		      && reg_renumber[src_regno] < 0 && reg->type == OP_IN)
		    {
		      if (usage_insns[src_regno].check == curr_usage_insns_check
			  && (next_usage_insns
			      = usage_insns[src_regno].insns) != NULL_RTX
			  && NONDEBUG_INSN_P (curr_insn))
			add_to_inherit (src_regno, next_usage_insns);
		      else if (usage_insns[src_regno].check
			       != -(int) INSN_UID (curr_insn))
			/* Add usages but only if the reg is not set up
			   in the same insn.  */
			add_next_usage_insn (src_regno, curr_insn, reloads_num);
		    }
		  else if (src_regno < FIRST_PSEUDO_REGISTER
			   || reg_renumber[src_regno] >= 0)
		    {
		      bool before_p;
		      rtx_insn *use_insn = curr_insn;

		      before_p = (JUMP_P (curr_insn)
				  || (CALL_P (curr_insn) && reg->type == OP_IN));
		      if (NONDEBUG_INSN_P (curr_insn)
			  && (! JUMP_P (curr_insn) || reg->type == OP_IN)
			  && split_if_necessary (src_regno, reg->biggest_mode,
						 potential_reload_hard_regs,
						 before_p, curr_insn, max_uid))
			{
			  if (reg->subreg_p)
			    lra_risky_transformations_p = true;
			  change_p = true;
			  /* Invalidate. */
			  usage_insns[src_regno].check = 0;
			  if (before_p)
			    use_insn = PREV_INSN (curr_insn);
			}
		      if (NONDEBUG_INSN_P (curr_insn))
			{
			  if (src_regno < FIRST_PSEUDO_REGISTER)
			    add_to_hard_reg_set (&live_hard_regs,
						 reg->biggest_mode, src_regno);
			  else
			    add_to_hard_reg_set (&live_hard_regs,
						 PSEUDO_REGNO_MODE (src_regno),
						 reg_renumber[src_regno]);
			}
		      if (src_regno >= FIRST_PSEUDO_REGISTER)
			add_next_usage_insn (src_regno, use_insn, reloads_num);
		      else
			{
			  for (i = 0; i < hard_regno_nregs (src_regno, reg->biggest_mode); i++)
			    add_next_usage_insn (src_regno + i, use_insn, reloads_num);
			}
		    }
		}
	  /* Process used call regs.  */
	  if (curr_id->arg_hard_regs != NULL)
	    for (i = 0; (src_regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	      if (src_regno < FIRST_PSEUDO_REGISTER)
		{
	           SET_HARD_REG_BIT (live_hard_regs, src_regno);
	           add_next_usage_insn (src_regno, curr_insn, reloads_num);
		}
	  for (i = 0; i < to_inherit_num; i++)
	    {
	      src_regno = to_inherit[i].regno;
	      if (inherit_reload_reg (false, src_regno, ALL_REGS,
				      curr_insn, to_inherit[i].insns))
		change_p = true;
	      else
		setup_next_usage_insn (src_regno, curr_insn, reloads_num, false);
	    }
	}
      if (update_reloads_num_p
	  && NONDEBUG_INSN_P (curr_insn) && curr_set != NULL_RTX)
	{
	  int regno = -1;
	  if ((REG_P (SET_DEST (curr_set))
	       && (regno = REGNO (SET_DEST (curr_set))) >= lra_constraint_new_regno_start
	       && reg_renumber[regno] < 0
	       && (cl = lra_get_allocno_class (regno)) != NO_REGS)
	      || (REG_P (SET_SRC (curr_set))
	          && (regno = REGNO (SET_SRC (curr_set))) >= lra_constraint_new_regno_start
	          && reg_renumber[regno] < 0
	          && (cl = lra_get_allocno_class (regno)) != NO_REGS))
	    {
	      if (ira_class_hard_regs_num[cl] <= max_small_class_regs_num)
		reloads_num++;
	      if (hard_reg_set_subset_p (reg_class_contents[cl], live_hard_regs))
		IOR_HARD_REG_SET (potential_reload_hard_regs,
	                          reg_class_contents[cl]);
	    }
	}
      if (NONDEBUG_INSN_P (curr_insn))
	{
	  int regno;

	  /* Invalidate invariants with changed regs.  */
	  curr_id = lra_get_insn_recog_data (curr_insn);
	  for (reg = curr_id->regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN)
	      {
		bitmap_set_bit (&invalid_invariant_regs, reg->regno);
		bitmap_set_bit (&invalid_invariant_regs,
				ORIGINAL_REGNO (regno_reg_rtx[reg->regno]));
	      }
	  curr_static_id = curr_id->insn_static_data;
	  for (reg = curr_static_id->hard_regs; reg != NULL; reg = reg->next)
	    if (reg->type != OP_IN)
	      bitmap_set_bit (&invalid_invariant_regs, reg->regno);
	  if (curr_id->arg_hard_regs != NULL)
	    for (i = 0; (regno = curr_id->arg_hard_regs[i]) >= 0; i++)
	      if (regno >= FIRST_PSEUDO_REGISTER)
		bitmap_set_bit (&invalid_invariant_regs,
				regno - FIRST_PSEUDO_REGISTER);
	}
      /* We reached the start of the current basic block.  */
      if (prev_insn == NULL_RTX || prev_insn == PREV_INSN (head)
	  || BLOCK_FOR_INSN (prev_insn) != curr_bb)
	{
	  /* We reached the beginning of the current block -- do
	     rest of spliting in the current BB.  */
	  to_process = df_get_live_in (curr_bb);
	  if (BLOCK_FOR_INSN (head) != curr_bb)
	    {
	      /* We are somewhere in the middle of EBB.	 */
	      get_live_on_other_edges (EDGE_PRED (curr_bb, 0)->src,
				       curr_bb, &temp_bitmap);
	      to_process = &temp_bitmap;
	    }
	  head_p = true;
	  EXECUTE_IF_SET_IN_BITMAP (to_process, 0, j, bi)
	    {
	      if ((int) j >= lra_constraint_new_regno_start)
		break;
	      if (((int) j < FIRST_PSEUDO_REGISTER || reg_renumber[j] >= 0)
		  && usage_insns[j].check == curr_usage_insns_check
		  && (next_usage_insns = usage_insns[j].insns) != NULL_RTX)
		{
		  if (need_for_split_p (potential_reload_hard_regs, j))
		    {
		      if (lra_dump_file != NULL && head_p)
			{
			  fprintf (lra_dump_file,
				   "  ----------------------------------\n");
			  head_p = false;
			}
		      if (split_reg (false, j, bb_note (curr_bb),
				     next_usage_insns, NULL))
			change_p = true;
		    }
		  usage_insns[j].check = 0;
		}
	    }
	}
    }
  return change_p;
}

/* This value affects EBB forming.  If probability of edge from EBB to
   a BB is not greater than the following value, we don't add the BB
   to EBB.  */
#define EBB_PROBABILITY_CUTOFF \
  ((REG_BR_PROB_BASE * LRA_INHERITANCE_EBB_PROBABILITY_CUTOFF) / 100)

/* Current number of inheritance/split iteration.  */
int lra_inheritance_iter;

/* Entry function for inheritance/split pass.  */
void
lra_inheritance (void)
{
  int i;
  basic_block bb, start_bb;
  edge e;

  lra_inheritance_iter++;
  if (lra_inheritance_iter > LRA_MAX_INHERITANCE_PASSES)
    return;
  timevar_push (TV_LRA_INHERITANCE);
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file, "\n********** Inheritance #%d: **********\n\n",
	     lra_inheritance_iter);
  curr_usage_insns_check = 0;
  usage_insns = XNEWVEC (struct usage_insns, lra_constraint_new_regno_start);
  for (i = 0; i < lra_constraint_new_regno_start; i++)
    usage_insns[i].check = 0;
  bitmap_initialize (&check_only_regs, &reg_obstack);
  bitmap_initialize (&invalid_invariant_regs, &reg_obstack);
  bitmap_initialize (&live_regs, &reg_obstack);
  bitmap_initialize (&temp_bitmap, &reg_obstack);
  bitmap_initialize (&ebb_global_regs, &reg_obstack);
  FOR_EACH_BB_FN (bb, cfun)
    {
      start_bb = bb;
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "EBB");
      /* Form a EBB starting with BB.  */
      bitmap_clear (&ebb_global_regs);
      bitmap_ior_into (&ebb_global_regs, df_get_live_in (bb));
      for (;;)
	{
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, " %d", bb->index);
	  if (bb->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	      || LABEL_P (BB_HEAD (bb->next_bb)))
	    break;
	  e = find_fallthru_edge (bb->succs);
	  if (! e)
	    break;
	  if (e->probability.initialized_p ()
	      && e->probability.to_reg_br_prob_base () < EBB_PROBABILITY_CUTOFF)
	    break;
	  bb = bb->next_bb;
	}
      bitmap_ior_into (&ebb_global_regs, df_get_live_out (bb));
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "\n");
      if (inherit_in_ebb (BB_HEAD (start_bb), BB_END (bb)))
	/* Remember that the EBB head and tail can change in
	   inherit_in_ebb.  */
	update_ebb_live_info (BB_HEAD (start_bb), BB_END (bb));
    }
  bitmap_release (&ebb_global_regs);
  bitmap_release (&temp_bitmap);
  bitmap_release (&live_regs);
  bitmap_release (&invalid_invariant_regs);
  bitmap_release (&check_only_regs);
  free (usage_insns);

  timevar_pop (TV_LRA_INHERITANCE);
}



/* This page contains code to undo failed inheritance/split
   transformations.  */

/* Current number of iteration undoing inheritance/split.  */
int lra_undo_inheritance_iter;

/* Fix BB live info LIVE after removing pseudos created on pass doing
   inheritance/split which are REMOVED_PSEUDOS.	 */
static void
fix_bb_live_info (bitmap live, bitmap removed_pseudos)
{
  unsigned int regno;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (removed_pseudos, 0, regno, bi)
    if (bitmap_clear_bit (live, regno)
	&& REG_P (lra_reg_info[regno].restore_rtx))
      bitmap_set_bit (live, REGNO (lra_reg_info[regno].restore_rtx));
}

/* Return regno of the (subreg of) REG. Otherwise, return a negative
   number.  */
static int
get_regno (rtx reg)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  if (REG_P (reg))
    return REGNO (reg);
  return -1;
}

/* Delete a move INSN with destination reg DREGNO and a previous
   clobber insn with the same regno.  The inheritance/split code can
   generate moves with preceding clobber and when we delete such moves
   we should delete the clobber insn too to keep the correct life
   info.  */
static void
delete_move_and_clobber (rtx_insn *insn, int dregno)
{
  rtx_insn *prev_insn = PREV_INSN (insn);

  lra_set_insn_deleted (insn);
  lra_assert (dregno >= 0);
  if (prev_insn != NULL && NONDEBUG_INSN_P (prev_insn)
      && GET_CODE (PATTERN (prev_insn)) == CLOBBER
      && dregno == get_regno (XEXP (PATTERN (prev_insn), 0)))
    lra_set_insn_deleted (prev_insn);
}

/* Remove inheritance/split pseudos which are in REMOVE_PSEUDOS and
   return true if we did any change.  The undo transformations for
   inheritance looks like
      i <- i2
      p <- i	  =>   p <- i2
   or removing
      p <- i, i <- p, and i <- i3
   where p is original pseudo from which inheritance pseudo i was
   created, i and i3 are removed inheritance pseudos, i2 is another
   not removed inheritance pseudo.  All split pseudos or other
   occurrences of removed inheritance pseudos are changed on the
   corresponding original pseudos.

   The function also schedules insns changed and created during
   inheritance/split pass for processing by the subsequent constraint
   pass.  */
static bool
remove_inheritance_pseudos (bitmap remove_pseudos)
{
  basic_block bb;
  int regno, sregno, prev_sregno, dregno;
  rtx restore_rtx;
  rtx set, prev_set;
  rtx_insn *prev_insn;
  bool change_p, done_p;

  change_p = ! bitmap_empty_p (remove_pseudos);
  /* We cannot finish the function right away if CHANGE_P is true
     because we need to marks insns affected by previous
     inheritance/split pass for processing by the subsequent
     constraint pass.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      fix_bb_live_info (df_get_live_in (bb), remove_pseudos);
      fix_bb_live_info (df_get_live_out (bb), remove_pseudos);
      FOR_BB_INSNS_REVERSE (bb, curr_insn)
	{
	  if (! INSN_P (curr_insn))
	    continue;
	  done_p = false;
	  sregno = dregno = -1;
	  if (change_p && NONDEBUG_INSN_P (curr_insn)
	      && (set = single_set (curr_insn)) != NULL_RTX)
	    {
	      dregno = get_regno (SET_DEST (set));
	      sregno = get_regno (SET_SRC (set));
	    }

	  if (sregno >= 0 && dregno >= 0)
	    {
	      if (bitmap_bit_p (remove_pseudos, dregno)
		  && ! REG_P (lra_reg_info[dregno].restore_rtx))
		{
		  /* invariant inheritance pseudo <- original pseudo */
		  if (lra_dump_file != NULL)
		    {
		      fprintf (lra_dump_file, "	   Removing invariant inheritance:\n");
		      dump_insn_slim (lra_dump_file, curr_insn);
		      fprintf (lra_dump_file, "\n");
		    }
		  delete_move_and_clobber (curr_insn, dregno);
		  done_p = true;
		}
	      else if (bitmap_bit_p (remove_pseudos, sregno)
		       && ! REG_P (lra_reg_info[sregno].restore_rtx))
		{
		  /* reload pseudo <- invariant inheritance pseudo */
		  start_sequence ();
		  /* We cannot just change the source.  It might be
		     an insn different from the move.  */
		  emit_insn (lra_reg_info[sregno].restore_rtx);
		  rtx_insn *new_insns = get_insns ();
		  end_sequence ();
		  lra_assert (single_set (new_insns) != NULL
			      && SET_DEST (set) == SET_DEST (single_set (new_insns)));
		  lra_process_new_insns (curr_insn, NULL, new_insns,
					 "Changing reload<-invariant inheritance");
		  delete_move_and_clobber (curr_insn, dregno);
		  done_p = true;
		}
	      else if ((bitmap_bit_p (remove_pseudos, sregno)
			&& (get_regno (lra_reg_info[sregno].restore_rtx) == dregno
			    || (bitmap_bit_p (remove_pseudos, dregno)
				&& get_regno (lra_reg_info[sregno].restore_rtx) >= 0
				&& (get_regno (lra_reg_info[sregno].restore_rtx)
				    == get_regno (lra_reg_info[dregno].restore_rtx)))))
		       || (bitmap_bit_p (remove_pseudos, dregno)
			   && get_regno (lra_reg_info[dregno].restore_rtx) == sregno))
		/* One of the following cases:
		     original <- removed inheritance pseudo
		     removed inherit pseudo <- another removed inherit pseudo
		     removed inherit pseudo <- original pseudo
		   Or
		     removed_split_pseudo <- original_reg
		     original_reg <- removed_split_pseudo */
		{
		  if (lra_dump_file != NULL)
		    {
		      fprintf (lra_dump_file, "	   Removing %s:\n",
			       bitmap_bit_p (&lra_split_regs, sregno)
			       || bitmap_bit_p (&lra_split_regs, dregno)
			       ? "split" : "inheritance");
		      dump_insn_slim (lra_dump_file, curr_insn);
		    }
		  delete_move_and_clobber (curr_insn, dregno);
		  done_p = true;
		}
	      else if (bitmap_bit_p (remove_pseudos, sregno)
		       && bitmap_bit_p (&lra_inheritance_pseudos, sregno))
		{
		  /* Search the following pattern:
		       inherit_or_split_pseudo1 <- inherit_or_split_pseudo2
		       original_pseudo <- inherit_or_split_pseudo1
		    where the 2nd insn is the current insn and
		    inherit_or_split_pseudo2 is not removed.  If it is found,
		    change the current insn onto:
		       original_pseudo <- inherit_or_split_pseudo2.  */
		  for (prev_insn = PREV_INSN (curr_insn);
		       prev_insn != NULL_RTX && ! NONDEBUG_INSN_P (prev_insn);
		       prev_insn = PREV_INSN (prev_insn))
		    ;
		  if (prev_insn != NULL_RTX && BLOCK_FOR_INSN (prev_insn) == bb
		      && (prev_set = single_set (prev_insn)) != NULL_RTX
		      /* There should be no subregs in insn we are
			 searching because only the original reg might
			 be in subreg when we changed the mode of
			 load/store for splitting.  */
		      && REG_P (SET_DEST (prev_set))
		      && REG_P (SET_SRC (prev_set))
		      && (int) REGNO (SET_DEST (prev_set)) == sregno
		      && ((prev_sregno = REGNO (SET_SRC (prev_set)))
			  >= FIRST_PSEUDO_REGISTER)
		      && (lra_reg_info[prev_sregno].restore_rtx == NULL_RTX
			  ||
			  /* As we consider chain of inheritance or
			     splitting described in above comment we should
			     check that sregno and prev_sregno were
			     inheritance/split pseudos created from the
			     same original regno.  */
			  (get_regno (lra_reg_info[sregno].restore_rtx) >= 0
			   && (get_regno (lra_reg_info[sregno].restore_rtx)
			       == get_regno (lra_reg_info[prev_sregno].restore_rtx))))
		      && ! bitmap_bit_p (remove_pseudos, prev_sregno))
		    {
		      lra_assert (GET_MODE (SET_SRC (prev_set))
				  == GET_MODE (regno_reg_rtx[sregno]));
		      /* Although we have a single set, the insn can
			 contain more one sregno register occurrence
			 as a source.  Change all occurrences.  */
		      lra_substitute_pseudo_within_insn (curr_insn, sregno,
							 SET_SRC (prev_set),
							 false);
		      /* As we are finishing with processing the insn
			 here, check the destination too as it might
			 inheritance pseudo for another pseudo.  */
		      if (bitmap_bit_p (remove_pseudos, dregno)
			  && bitmap_bit_p (&lra_inheritance_pseudos, dregno)
			  && (restore_rtx
			      = lra_reg_info[dregno].restore_rtx) != NULL_RTX)
			{
			  if (GET_CODE (SET_DEST (set)) == SUBREG)
			    SUBREG_REG (SET_DEST (set)) = restore_rtx;
			  else
			    SET_DEST (set) = restore_rtx;
			}
		      lra_push_insn_and_update_insn_regno_info (curr_insn);
		      lra_set_used_insn_alternative_by_uid
			(INSN_UID (curr_insn), LRA_UNKNOWN_ALT);
		      done_p = true;
		      if (lra_dump_file != NULL)
			{
			  fprintf (lra_dump_file, "    Change reload insn:\n");
			  dump_insn_slim (lra_dump_file, curr_insn);
			}
		    }
		}
	    }
	  if (! done_p)
	    {
	      struct lra_insn_reg *reg;
	      bool restored_regs_p = false;
	      bool kept_regs_p = false;

	      curr_id = lra_get_insn_recog_data (curr_insn);
	      for (reg = curr_id->regs; reg != NULL; reg = reg->next)
		{
		  regno = reg->regno;
		  restore_rtx = lra_reg_info[regno].restore_rtx;
		  if (restore_rtx != NULL_RTX)
		    {
		      if (change_p && bitmap_bit_p (remove_pseudos, regno))
			{
			  lra_substitute_pseudo_within_insn
			    (curr_insn, regno, restore_rtx, false);
			  restored_regs_p = true;
			}
		      else
			kept_regs_p = true;
		    }
		}
	      if (NONDEBUG_INSN_P (curr_insn) && kept_regs_p)
		{
		  /* The instruction has changed since the previous
		     constraints pass.  */
		  lra_push_insn_and_update_insn_regno_info (curr_insn);
		  lra_set_used_insn_alternative_by_uid
		    (INSN_UID (curr_insn), LRA_UNKNOWN_ALT);
		}
	      else if (restored_regs_p)
		/* The instruction has been restored to the form that
		   it had during the previous constraints pass.  */
		lra_update_insn_regno_info (curr_insn);
	      if (restored_regs_p && lra_dump_file != NULL)
		{
		  fprintf (lra_dump_file, "   Insn after restoring regs:\n");
		  dump_insn_slim (lra_dump_file, curr_insn);
		}
	    }
	}
    }
  return change_p;
}

/* If optional reload pseudos failed to get a hard register or was not
   inherited, it is better to remove optional reloads.  We do this
   transformation after undoing inheritance to figure out necessity to
   remove optional reloads easier.  Return true if we do any
   change.  */
static bool
undo_optional_reloads (void)
{
  bool change_p, keep_p;
  unsigned int regno, uid;
  bitmap_iterator bi, bi2;
  rtx_insn *insn;
  rtx set, src, dest;
  auto_bitmap removed_optional_reload_pseudos (&reg_obstack);

  bitmap_copy (removed_optional_reload_pseudos, &lra_optional_reload_pseudos);
  EXECUTE_IF_SET_IN_BITMAP (&lra_optional_reload_pseudos, 0, regno, bi)
    {
      keep_p = false;
      /* Keep optional reloads from previous subpasses.  */
      if (lra_reg_info[regno].restore_rtx == NULL_RTX
	  /* If the original pseudo changed its allocation, just
	     removing the optional pseudo is dangerous as the original
	     pseudo will have longer live range.  */
	  || reg_renumber[REGNO (lra_reg_info[regno].restore_rtx)] >= 0)
	keep_p = true;
      else if (reg_renumber[regno] >= 0)
	EXECUTE_IF_SET_IN_BITMAP (&lra_reg_info[regno].insn_bitmap, 0, uid, bi2)
	  {
	    insn = lra_insn_recog_data[uid]->insn;
	    if ((set = single_set (insn)) == NULL_RTX)
	      continue;
	    src = SET_SRC (set);
	    dest = SET_DEST (set);
	    if (! REG_P (src) || ! REG_P (dest))
	      continue;
	    if (REGNO (dest) == regno
		/* Ignore insn for optional reloads itself.  */
		&& REGNO (lra_reg_info[regno].restore_rtx) != REGNO (src)
		/* Check only inheritance on last inheritance pass.  */
		&& (int) REGNO (src) >= new_regno_start
		/* Check that the optional reload was inherited.  */
		&& bitmap_bit_p (&lra_inheritance_pseudos, REGNO (src)))
	      {
		keep_p = true;
		break;
	      }
	  }
      if (keep_p)
	{
	  bitmap_clear_bit (removed_optional_reload_pseudos, regno);
	  if (lra_dump_file != NULL)
	    fprintf (lra_dump_file, "Keep optional reload reg %d\n", regno);
	}
    }
  change_p = ! bitmap_empty_p (removed_optional_reload_pseudos);
  auto_bitmap insn_bitmap (&reg_obstack);
  EXECUTE_IF_SET_IN_BITMAP (removed_optional_reload_pseudos, 0, regno, bi)
    {
      if (lra_dump_file != NULL)
	fprintf (lra_dump_file, "Remove optional reload reg %d\n", regno);
      bitmap_copy (insn_bitmap, &lra_reg_info[regno].insn_bitmap);
      EXECUTE_IF_SET_IN_BITMAP (insn_bitmap, 0, uid, bi2)
	{
	  insn = lra_insn_recog_data[uid]->insn;
	  if ((set = single_set (insn)) != NULL_RTX)
	    {
	      src = SET_SRC (set);
	      dest = SET_DEST (set);
	      if (REG_P (src) && REG_P (dest)
		  && ((REGNO (src) == regno
		       && (REGNO (lra_reg_info[regno].restore_rtx)
			   == REGNO (dest)))
		      || (REGNO (dest) == regno
			  && (REGNO (lra_reg_info[regno].restore_rtx)
			      == REGNO (src)))))
		{
		  if (lra_dump_file != NULL)
		    {
		      fprintf (lra_dump_file, "  Deleting move %u\n",
			       INSN_UID (insn));
		      dump_insn_slim (lra_dump_file, insn);
		    }
		  delete_move_and_clobber (insn, REGNO (dest));
		  continue;
		}
	      /* We should not worry about generation memory-memory
		 moves here as if the corresponding inheritance did
		 not work (inheritance pseudo did not get a hard reg),
		 we remove the inheritance pseudo and the optional
		 reload.  */
	    }
	  lra_substitute_pseudo_within_insn
	    (insn, regno, lra_reg_info[regno].restore_rtx, false);
	  lra_update_insn_regno_info (insn);
	  if (lra_dump_file != NULL)
	    {
	      fprintf (lra_dump_file,
		       "  Restoring original insn:\n");
	      dump_insn_slim (lra_dump_file, insn);
	    }
	}
    }
  /* Clear restore_regnos.  */
  EXECUTE_IF_SET_IN_BITMAP (&lra_optional_reload_pseudos, 0, regno, bi)
    lra_reg_info[regno].restore_rtx = NULL_RTX;
  return change_p;
}

/* Entry function for undoing inheritance/split transformation.	 Return true
   if we did any RTL change in this pass.  */
bool
lra_undo_inheritance (void)
{
  unsigned int regno;
  int hard_regno;
  int n_all_inherit, n_inherit, n_all_split, n_split;
  rtx restore_rtx;
  bitmap_iterator bi;
  bool change_p;

  lra_undo_inheritance_iter++;
  if (lra_undo_inheritance_iter > LRA_MAX_INHERITANCE_PASSES)
    return false;
  if (lra_dump_file != NULL)
    fprintf (lra_dump_file,
	     "\n********** Undoing inheritance #%d: **********\n\n",
	     lra_undo_inheritance_iter);
  auto_bitmap remove_pseudos (&reg_obstack);
  n_inherit = n_all_inherit = 0;
  EXECUTE_IF_SET_IN_BITMAP (&lra_inheritance_pseudos, 0, regno, bi)
    if (lra_reg_info[regno].restore_rtx != NULL_RTX)
      {
	n_all_inherit++;
	if (reg_renumber[regno] < 0
	    /* If the original pseudo changed its allocation, just
	       removing inheritance is dangerous as for changing
	       allocation we used shorter live-ranges.  */
	    && (! REG_P (lra_reg_info[regno].restore_rtx)
		|| reg_renumber[REGNO (lra_reg_info[regno].restore_rtx)] < 0))
	  bitmap_set_bit (remove_pseudos, regno);
	else
	  n_inherit++;
      }
  if (lra_dump_file != NULL && n_all_inherit != 0)
    fprintf (lra_dump_file, "Inherit %d out of %d (%.2f%%)\n",
	     n_inherit, n_all_inherit,
	     (double) n_inherit / n_all_inherit * 100);
  n_split = n_all_split = 0;
  EXECUTE_IF_SET_IN_BITMAP (&lra_split_regs, 0, regno, bi)
    if ((restore_rtx = lra_reg_info[regno].restore_rtx) != NULL_RTX)
      {
	int restore_regno = REGNO (restore_rtx);

	n_all_split++;
	hard_regno = (restore_regno >= FIRST_PSEUDO_REGISTER
		      ? reg_renumber[restore_regno] : restore_regno);
	if (hard_regno < 0 || reg_renumber[regno] == hard_regno)
	  bitmap_set_bit (remove_pseudos, regno);
	else
	  {
	    n_split++;
	    if (lra_dump_file != NULL)
	      fprintf (lra_dump_file, "	     Keep split r%d (orig=r%d)\n",
		       regno, restore_regno);
	  }
      }
  if (lra_dump_file != NULL && n_all_split != 0)
    fprintf (lra_dump_file, "Split %d out of %d (%.2f%%)\n",
	     n_split, n_all_split,
	     (double) n_split / n_all_split * 100);
  change_p = remove_inheritance_pseudos (remove_pseudos);
  /* Clear restore_regnos.  */
  EXECUTE_IF_SET_IN_BITMAP (&lra_inheritance_pseudos, 0, regno, bi)
    lra_reg_info[regno].restore_rtx = NULL_RTX;
  EXECUTE_IF_SET_IN_BITMAP (&lra_split_regs, 0, regno, bi)
    lra_reg_info[regno].restore_rtx = NULL_RTX;
  change_p = undo_optional_reloads () || change_p;
  return change_p;
}
