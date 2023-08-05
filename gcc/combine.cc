/* Optimize by combining instructions for GNU compiler.
   Copyright (C) 1987-2023 Free Software Foundation, Inc.

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

/* This module is essentially the "combiner" phase of the U. of Arizona
   Portable Optimizer, but redone to work on our list-structured
   representation for RTL instead of their string representation.

   The LOG_LINKS of each insn identify the most recent assignment
   to each REG used in the insn.  It is a list of previous insns,
   each of which contains a SET for a REG that is used in this insn
   and not used or set in between.  LOG_LINKs never cross basic blocks.
   They were set up by the preceding pass (lifetime analysis).

   We try to combine each pair of insns joined by a logical link.
   We also try to combine triplets of insns A, B and C when C has
   a link back to B and B has a link back to A.  Likewise for a
   small number of quadruplets of insns A, B, C and D for which
   there's high likelihood of success.

   We check (with modified_between_p) to avoid combining in such a way
   as to move a computation to a place where its value would be different.

   Combination is done by mathematically substituting the previous
   insn(s) values for the regs they set into the expressions in
   the later insns that refer to these regs.  If the result is a valid insn
   for our target machine, according to the machine description,
   we install it, delete the earlier insns, and update the data flow
   information (LOG_LINKS and REG_NOTES) for what we did.

   There are a few exceptions where the dataflow information isn't
   completely updated (however this is only a local issue since it is
   regenerated before the next pass that uses it):

   - reg_live_length is not updated
   - reg_n_refs is not adjusted in the rare case when a register is
     no longer required in a computation
   - there are extremely rare cases (see distribute_notes) when a
     REG_DEAD note is lost
   - a LOG_LINKS entry that refers to an insn with multiple SETs may be
     removed because there is no way to know which register it was
     linking

   To simplify substitution, we combine only when the earlier insn(s)
   consist of only a single assignment.  To simplify updating afterward,
   we never combine when a subroutine call appears in the middle.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "predict.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "stor-layout.h"
#include "cfgrtl.h"
#include "cfgcleanup.h"
/* Include expr.h after insn-config.h so we get HAVE_conditional_move.  */
#include "explow.h"
#include "insn-attr.h"
#include "rtlhooks-def.h"
#include "expr.h"
#include "tree-pass.h"
#include "valtrack.h"
#include "rtl-iter.h"
#include "print-rtl.h"
#include "function-abi.h"
#include "rtlanal.h"

/* Number of attempts to combine instructions in this function.  */

static int combine_attempts;

/* Number of attempts that got as far as substitution in this function.  */

static int combine_merges;

/* Number of instructions combined with added SETs in this function.  */

static int combine_extras;

/* Number of instructions combined in this function.  */

static int combine_successes;

/* combine_instructions may try to replace the right hand side of the
   second instruction with the value of an associated REG_EQUAL note
   before throwing it at try_combine.  That is problematic when there
   is a REG_DEAD note for a register used in the old right hand side
   and can cause distribute_notes to do wrong things.  This is the
   second instruction if it has been so modified, null otherwise.  */

static rtx_insn *i2mod;

/* When I2MOD is nonnull, this is a copy of the old right hand side.  */

static rtx i2mod_old_rhs;

/* When I2MOD is nonnull, this is a copy of the new right hand side.  */

static rtx i2mod_new_rhs;

struct reg_stat_type {
  /* Record last point of death of (hard or pseudo) register n.  */
  rtx_insn			*last_death;

  /* Record last point of modification of (hard or pseudo) register n.  */
  rtx_insn			*last_set;

  /* The next group of fields allows the recording of the last value assigned
     to (hard or pseudo) register n.  We use this information to see if an
     operation being processed is redundant given a prior operation performed
     on the register.  For example, an `and' with a constant is redundant if
     all the zero bits are already known to be turned off.

     We use an approach similar to that used by cse, but change it in the
     following ways:

     (1) We do not want to reinitialize at each label.
     (2) It is useful, but not critical, to know the actual value assigned
	 to a register.  Often just its form is helpful.

     Therefore, we maintain the following fields:

     last_set_value		the last value assigned
     last_set_label		records the value of label_tick when the
				register was assigned
     last_set_table_tick	records the value of label_tick when a
				value using the register is assigned
     last_set_invalid		set to true when it is not valid
				to use the value of this register in some
				register's value

     To understand the usage of these tables, it is important to understand
     the distinction between the value in last_set_value being valid and
     the register being validly contained in some other expression in the
     table.

     (The next two parameters are out of date).

     reg_stat[i].last_set_value is valid if it is nonzero, and either
     reg_n_sets[i] is 1 or reg_stat[i].last_set_label == label_tick.

     Register I may validly appear in any expression returned for the value
     of another register if reg_n_sets[i] is 1.  It may also appear in the
     value for register J if reg_stat[j].last_set_invalid is zero, or
     reg_stat[i].last_set_label < reg_stat[j].last_set_label.

     If an expression is found in the table containing a register which may
     not validly appear in an expression, the register is replaced by
     something that won't match, (clobber (const_int 0)).  */

  /* Record last value assigned to (hard or pseudo) register n.  */

  rtx				last_set_value;

  /* Record the value of label_tick when an expression involving register n
     is placed in last_set_value.  */

  int				last_set_table_tick;

  /* Record the value of label_tick when the value for register n is placed in
     last_set_value.  */

  int				last_set_label;

  /* These fields are maintained in parallel with last_set_value and are
     used to store the mode in which the register was last set, the bits
     that were known to be zero when it was last set, and the number of
     sign bits copies it was known to have when it was last set.  */

  unsigned HOST_WIDE_INT	last_set_nonzero_bits;
  char				last_set_sign_bit_copies;
  ENUM_BITFIELD(machine_mode)	last_set_mode : MACHINE_MODE_BITSIZE;

  /* Set to true if references to register n in expressions should not be
     used.  last_set_invalid is set nonzero when this register is being
     assigned to and last_set_table_tick == label_tick.  */

  bool				last_set_invalid;

  /* Some registers that are set more than once and used in more than one
     basic block are nevertheless always set in similar ways.  For example,
     a QImode register may be loaded from memory in two places on a machine
     where byte loads zero extend.

     We record in the following fields if a register has some leading bits
     that are always equal to the sign bit, and what we know about the
     nonzero bits of a register, specifically which bits are known to be
     zero.

     If an entry is zero, it means that we don't know anything special.  */

  unsigned char			sign_bit_copies;

  unsigned HOST_WIDE_INT	nonzero_bits;

  /* Record the value of the label_tick when the last truncation
     happened.  The field truncated_to_mode is only valid if
     truncation_label == label_tick.  */

  int				truncation_label;

  /* Record the last truncation seen for this register.  If truncation
     is not a nop to this mode we might be able to save an explicit
     truncation if we know that value already contains a truncated
     value.  */

  ENUM_BITFIELD(machine_mode)	truncated_to_mode : MACHINE_MODE_BITSIZE;
};


static vec<reg_stat_type> reg_stat;

/* One plus the highest pseudo for which we track REG_N_SETS.
   regstat_init_n_sets_and_refs allocates the array for REG_N_SETS just once,
   but during combine_split_insns new pseudos can be created.  As we don't have
   updated DF information in that case, it is hard to initialize the array
   after growing.  The combiner only cares about REG_N_SETS (regno) == 1,
   so instead of growing the arrays, just assume all newly created pseudos
   during combine might be set multiple times.  */

static unsigned int reg_n_sets_max;

/* Record the luid of the last insn that invalidated memory
   (anything that writes memory, and subroutine calls, but not pushes).  */

static int mem_last_set;

/* Record the luid of the last CALL_INSN
   so we can tell whether a potential combination crosses any calls.  */

static int last_call_luid;

/* When `subst' is called, this is the insn that is being modified
   (by combining in a previous insn).  The PATTERN of this insn
   is still the old pattern partially modified and it should not be
   looked at, but this may be used to examine the successors of the insn
   to judge whether a simplification is valid.  */

static rtx_insn *subst_insn;

/* This is the lowest LUID that `subst' is currently dealing with.
   get_last_value will not return a value if the register was set at or
   after this LUID.  If not for this mechanism, we could get confused if
   I2 or I1 in try_combine were an insn that used the old value of a register
   to obtain a new value.  In that case, we might erroneously get the
   new value of the register when we wanted the old one.  */

static int subst_low_luid;

/* This contains any hard registers that are used in newpat; reg_dead_at_p
   must consider all these registers to be always live.  */

static HARD_REG_SET newpat_used_regs;

/* This is an insn to which a LOG_LINKS entry has been added.  If this
   insn is the earlier than I2 or I3, combine should rescan starting at
   that location.  */

static rtx_insn *added_links_insn;

/* And similarly, for notes.  */

static rtx_insn *added_notes_insn;

/* Basic block in which we are performing combines.  */
static basic_block this_basic_block;
static bool optimize_this_for_speed_p;


/* Length of the currently allocated uid_insn_cost array.  */

static int max_uid_known;

/* The following array records the insn_cost for every insn
   in the instruction stream.  */

static int *uid_insn_cost;

/* The following array records the LOG_LINKS for every insn in the
   instruction stream as struct insn_link pointers.  */

struct insn_link {
  rtx_insn *insn;
  unsigned int regno;
  struct insn_link *next;
};

static struct insn_link **uid_log_links;

static inline int
insn_uid_check (const_rtx insn)
{
  int uid = INSN_UID (insn);
  gcc_checking_assert (uid <= max_uid_known);
  return uid;
}

#define INSN_COST(INSN)		(uid_insn_cost[insn_uid_check (INSN)])
#define LOG_LINKS(INSN)		(uid_log_links[insn_uid_check (INSN)])

#define FOR_EACH_LOG_LINK(L, INSN)				\
  for ((L) = LOG_LINKS (INSN); (L); (L) = (L)->next)

/* Links for LOG_LINKS are allocated from this obstack.  */

static struct obstack insn_link_obstack;

/* Allocate a link.  */

static inline struct insn_link *
alloc_insn_link (rtx_insn *insn, unsigned int regno, struct insn_link *next)
{
  struct insn_link *l
    = (struct insn_link *) obstack_alloc (&insn_link_obstack,
					  sizeof (struct insn_link));
  l->insn = insn;
  l->regno = regno;
  l->next = next;
  return l;
}

/* Incremented for each basic block.  */

static int label_tick;

/* Reset to label_tick for each extended basic block in scanning order.  */

static int label_tick_ebb_start;

/* Mode used to compute significance in reg_stat[].nonzero_bits.  It is the
   largest integer mode that can fit in HOST_BITS_PER_WIDE_INT.  */

static scalar_int_mode nonzero_bits_mode;

/* Nonzero when reg_stat[].nonzero_bits and reg_stat[].sign_bit_copies can
   be safely used.  It is zero while computing them and after combine has
   completed.  This former test prevents propagating values based on
   previously set values, which can be incorrect if a variable is modified
   in a loop.  */

static int nonzero_sign_valid;


/* Record one modification to rtl structure
   to be undone by storing old_contents into *where.  */

enum undo_kind { UNDO_RTX, UNDO_INT, UNDO_MODE, UNDO_LINKS };

struct undo
{
  struct undo *next;
  enum undo_kind kind;
  union { rtx r; int i; machine_mode m; struct insn_link *l; } old_contents;
  union { rtx *r; int *i; int regno; struct insn_link **l; } where;
};

/* Record a bunch of changes to be undone, up to MAX_UNDO of them.
   num_undo says how many are currently recorded.

   other_insn is nonzero if we have modified some other insn in the process
   of working on subst_insn.  It must be verified too.  */

struct undobuf
{
  struct undo *undos;
  struct undo *frees;
  rtx_insn *other_insn;
};

static struct undobuf undobuf;

/* Number of times the pseudo being substituted for
   was found and replaced.  */

static int n_occurrences;

static rtx reg_nonzero_bits_for_combine (const_rtx, scalar_int_mode,
					 scalar_int_mode,
					 unsigned HOST_WIDE_INT *);
static rtx reg_num_sign_bit_copies_for_combine (const_rtx, scalar_int_mode,
						scalar_int_mode,
						unsigned int *);
static void do_SUBST (rtx *, rtx);
static void do_SUBST_INT (int *, int);
static void init_reg_last (void);
static void setup_incoming_promotions (rtx_insn *);
static void set_nonzero_bits_and_sign_copies (rtx, const_rtx, void *);
static bool cant_combine_insn_p (rtx_insn *);
static bool can_combine_p (rtx_insn *, rtx_insn *, rtx_insn *, rtx_insn *,
			   rtx_insn *, rtx_insn *, rtx *, rtx *);
static bool combinable_i3pat (rtx_insn *, rtx *, rtx, rtx, rtx,
			      bool, bool, rtx *);
static bool contains_muldiv (rtx);
static rtx_insn *try_combine (rtx_insn *, rtx_insn *, rtx_insn *, rtx_insn *,
			      bool *, rtx_insn *);
static void undo_all (void);
static void undo_commit (void);
static rtx *find_split_point (rtx *, rtx_insn *, bool);
static rtx subst (rtx, rtx, rtx, bool, bool, bool);
static rtx combine_simplify_rtx (rtx, machine_mode, bool, bool);
static rtx simplify_if_then_else (rtx);
static rtx simplify_set (rtx);
static rtx simplify_logical (rtx);
static rtx expand_compound_operation (rtx);
static const_rtx expand_field_assignment (const_rtx);
static rtx make_extraction (machine_mode, rtx, HOST_WIDE_INT, rtx,
			    unsigned HOST_WIDE_INT, bool, bool, bool);
static int get_pos_from_mask (unsigned HOST_WIDE_INT,
			      unsigned HOST_WIDE_INT *);
static rtx canon_reg_for_combine (rtx, rtx);
static rtx force_int_to_mode (rtx, scalar_int_mode, scalar_int_mode,
			      scalar_int_mode, unsigned HOST_WIDE_INT, bool);
static rtx force_to_mode (rtx, machine_mode,
			  unsigned HOST_WIDE_INT, bool);
static rtx if_then_else_cond (rtx, rtx *, rtx *);
static rtx known_cond (rtx, enum rtx_code, rtx, rtx);
static bool rtx_equal_for_field_assignment_p (rtx, rtx, bool = false);
static rtx make_field_assignment (rtx);
static rtx apply_distributive_law (rtx);
static rtx distribute_and_simplify_rtx (rtx, int);
static rtx simplify_and_const_int_1 (scalar_int_mode, rtx,
				     unsigned HOST_WIDE_INT);
static rtx simplify_and_const_int (rtx, scalar_int_mode, rtx,
				   unsigned HOST_WIDE_INT);
static bool merge_outer_ops (enum rtx_code *, HOST_WIDE_INT *, enum rtx_code,
			     HOST_WIDE_INT, machine_mode, bool *);
static rtx simplify_shift_const_1 (enum rtx_code, machine_mode, rtx, int);
static rtx simplify_shift_const (rtx, enum rtx_code, machine_mode, rtx,
				 int);
static int recog_for_combine (rtx *, rtx_insn *, rtx *);
static rtx gen_lowpart_for_combine (machine_mode, rtx);
static enum rtx_code simplify_compare_const (enum rtx_code, machine_mode,
					     rtx *, rtx *);
static enum rtx_code simplify_comparison (enum rtx_code, rtx *, rtx *);
static void update_table_tick (rtx);
static void record_value_for_reg (rtx, rtx_insn *, rtx);
static void check_promoted_subreg (rtx_insn *, rtx);
static void record_dead_and_set_regs_1 (rtx, const_rtx, void *);
static void record_dead_and_set_regs (rtx_insn *);
static bool get_last_value_validate (rtx *, rtx_insn *, int, bool);
static rtx get_last_value (const_rtx);
static void reg_dead_at_p_1 (rtx, const_rtx, void *);
static bool reg_dead_at_p (rtx, rtx_insn *);
static void move_deaths (rtx, rtx, int, rtx_insn *, rtx *);
static bool reg_bitfield_target_p (rtx, rtx);
static void distribute_notes (rtx, rtx_insn *, rtx_insn *, rtx_insn *,
			      rtx, rtx, rtx);
static void distribute_links (struct insn_link *);
static void mark_used_regs_combine (rtx);
static void record_promoted_value (rtx_insn *, rtx);
static bool unmentioned_reg_p (rtx, rtx);
static void record_truncated_values (rtx *, void *);
static bool reg_truncated_to_mode (machine_mode, const_rtx);
static rtx gen_lowpart_or_truncate (machine_mode, rtx);


/* It is not safe to use ordinary gen_lowpart in combine.
   See comments in gen_lowpart_for_combine.  */
#undef RTL_HOOKS_GEN_LOWPART
#define RTL_HOOKS_GEN_LOWPART              gen_lowpart_for_combine

/* Our implementation of gen_lowpart never emits a new pseudo.  */
#undef RTL_HOOKS_GEN_LOWPART_NO_EMIT
#define RTL_HOOKS_GEN_LOWPART_NO_EMIT      gen_lowpart_for_combine

#undef RTL_HOOKS_REG_NONZERO_REG_BITS
#define RTL_HOOKS_REG_NONZERO_REG_BITS     reg_nonzero_bits_for_combine

#undef RTL_HOOKS_REG_NUM_SIGN_BIT_COPIES
#define RTL_HOOKS_REG_NUM_SIGN_BIT_COPIES  reg_num_sign_bit_copies_for_combine

#undef RTL_HOOKS_REG_TRUNCATED_TO_MODE
#define RTL_HOOKS_REG_TRUNCATED_TO_MODE    reg_truncated_to_mode

static const struct rtl_hooks combine_rtl_hooks = RTL_HOOKS_INITIALIZER;


/* Convenience wrapper for the canonicalize_comparison target hook.
   Target hooks cannot use enum rtx_code.  */
static inline void
target_canonicalize_comparison (enum rtx_code *code, rtx *op0, rtx *op1,
				bool op0_preserve_value)
{
  int code_int = (int)*code;
  targetm.canonicalize_comparison (&code_int, op0, op1, op0_preserve_value);
  *code = (enum rtx_code)code_int;
}

/* Try to split PATTERN found in INSN.  This returns NULL_RTX if
   PATTERN cannot be split.  Otherwise, it returns an insn sequence.
   This is a wrapper around split_insns which ensures that the
   reg_stat vector is made larger if the splitter creates a new
   register.  */

static rtx_insn *
combine_split_insns (rtx pattern, rtx_insn *insn)
{
  rtx_insn *ret;
  unsigned int nregs;

  ret = split_insns (pattern, insn);
  nregs = max_reg_num ();
  if (nregs > reg_stat.length ())
    reg_stat.safe_grow_cleared (nregs, true);
  return ret;
}

/* This is used by find_single_use to locate an rtx in LOC that
   contains exactly one use of DEST, which is typically a REG.
   It returns a pointer to the innermost rtx expression
   containing DEST.  Appearances of DEST that are being used to
   totally replace it are not counted.  */

static rtx *
find_single_use_1 (rtx dest, rtx *loc)
{
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  rtx *result = NULL;
  rtx *this_result;
  int i;
  const char *fmt;

  switch (code)
    {
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    CASE_CONST_ANY:
    case CLOBBER:
      return 0;

    case SET:
      /* If the destination is anything other than PC, a REG or a SUBREG
	 of a REG that occupies all of the REG, the insn uses DEST if
	 it is mentioned in the destination or the source.  Otherwise, we
	 need just check the source.  */
      if (GET_CODE (SET_DEST (x)) != PC
	  && !REG_P (SET_DEST (x))
	  && ! (GET_CODE (SET_DEST (x)) == SUBREG
		&& REG_P (SUBREG_REG (SET_DEST (x)))
		&& !read_modify_subreg_p (SET_DEST (x))))
	break;

      return find_single_use_1 (dest, &SET_SRC (x));

    case MEM:
    case SUBREG:
      return find_single_use_1 (dest, &XEXP (x, 0));

    default:
      break;
    }

  /* If it wasn't one of the common cases above, check each expression and
     vector of this code.  Look for a unique usage of DEST.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (dest == XEXP (x, i)
	      || (REG_P (dest) && REG_P (XEXP (x, i))
		  && REGNO (dest) == REGNO (XEXP (x, i))))
	    this_result = loc;
	  else
	    this_result = find_single_use_1 (dest, &XEXP (x, i));

	  if (result == NULL)
	    result = this_result;
	  else if (this_result)
	    /* Duplicate usage.  */
	    return NULL;
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      if (XVECEXP (x, i, j) == dest
		  || (REG_P (dest)
		      && REG_P (XVECEXP (x, i, j))
		      && REGNO (XVECEXP (x, i, j)) == REGNO (dest)))
		this_result = loc;
	      else
		this_result = find_single_use_1 (dest, &XVECEXP (x, i, j));

	      if (result == NULL)
		result = this_result;
	      else if (this_result)
		return NULL;
	    }
	}
    }

  return result;
}


/* See if DEST, produced in INSN, is used only a single time in the
   sequel.  If so, return a pointer to the innermost rtx expression in which
   it is used.

   If PLOC is nonzero, *PLOC is set to the insn containing the single use.

   Otherwise, we find the single use by finding an insn that has a
   LOG_LINKS pointing at INSN and has a REG_DEAD note for DEST.  If DEST is
   only referenced once in that insn, we know that it must be the first
   and last insn referencing DEST.  */

static rtx *
find_single_use (rtx dest, rtx_insn *insn, rtx_insn **ploc)
{
  basic_block bb;
  rtx_insn *next;
  rtx *result;
  struct insn_link *link;

  if (!REG_P (dest))
    return 0;

  bb = BLOCK_FOR_INSN (insn);
  for (next = NEXT_INSN (insn);
       next && BLOCK_FOR_INSN (next) == bb;
       next = NEXT_INSN (next))
    if (NONDEBUG_INSN_P (next) && dead_or_set_p (next, dest))
      {
	FOR_EACH_LOG_LINK (link, next)
	  if (link->insn == insn && link->regno == REGNO (dest))
	    break;

	if (link)
	  {
	    result = find_single_use_1 (dest, &PATTERN (next));
	    if (ploc)
	      *ploc = next;
	    return result;
	  }
      }

  return 0;
}

/* Substitute NEWVAL, an rtx expression, into INTO, a place in some
   insn.  The substitution can be undone by undo_all.  If INTO is already
   set to NEWVAL, do not record this change.  Because computing NEWVAL might
   also call SUBST, we have to compute it before we put anything into
   the undo table.  */

static void
do_SUBST (rtx *into, rtx newval)
{
  struct undo *buf;
  rtx oldval = *into;

  if (oldval == newval)
    return;

  /* We'd like to catch as many invalid transformations here as
     possible.  Unfortunately, there are way too many mode changes
     that are perfectly valid, so we'd waste too much effort for
     little gain doing the checks here.  Focus on catching invalid
     transformations involving integer constants.  */
  if (GET_MODE_CLASS (GET_MODE (oldval)) == MODE_INT
      && CONST_INT_P (newval))
    {
      /* Sanity check that we're replacing oldval with a CONST_INT
	 that is a valid sign-extension for the original mode.  */
      gcc_assert (INTVAL (newval)
		  == trunc_int_for_mode (INTVAL (newval), GET_MODE (oldval)));

      /* Replacing the operand of a SUBREG or a ZERO_EXTEND with a
	 CONST_INT is not valid, because after the replacement, the
	 original mode would be gone.  Unfortunately, we can't tell
	 when do_SUBST is called to replace the operand thereof, so we
	 perform this test on oldval instead, checking whether an
	 invalid replacement took place before we got here.  */
      gcc_assert (!(GET_CODE (oldval) == SUBREG
		    && CONST_INT_P (SUBREG_REG (oldval))));
      gcc_assert (!(GET_CODE (oldval) == ZERO_EXTEND
		    && CONST_INT_P (XEXP (oldval, 0))));
    }

  if (undobuf.frees)
    buf = undobuf.frees, undobuf.frees = buf->next;
  else
    buf = XNEW (struct undo);

  buf->kind = UNDO_RTX;
  buf->where.r = into;
  buf->old_contents.r = oldval;
  *into = newval;

  buf->next = undobuf.undos, undobuf.undos = buf;
}

#define SUBST(INTO, NEWVAL)	do_SUBST (&(INTO), (NEWVAL))

/* Similar to SUBST, but NEWVAL is an int expression.  Note that substitution
   for the value of a HOST_WIDE_INT value (including CONST_INT) is
   not safe.  */

static void
do_SUBST_INT (int *into, int newval)
{
  struct undo *buf;
  int oldval = *into;

  if (oldval == newval)
    return;

  if (undobuf.frees)
    buf = undobuf.frees, undobuf.frees = buf->next;
  else
    buf = XNEW (struct undo);

  buf->kind = UNDO_INT;
  buf->where.i = into;
  buf->old_contents.i = oldval;
  *into = newval;

  buf->next = undobuf.undos, undobuf.undos = buf;
}

#define SUBST_INT(INTO, NEWVAL)  do_SUBST_INT (&(INTO), (NEWVAL))

/* Similar to SUBST, but just substitute the mode.  This is used when
   changing the mode of a pseudo-register, so that any other
   references to the entry in the regno_reg_rtx array will change as
   well.  */

static void
subst_mode (int regno, machine_mode newval)
{
  struct undo *buf;
  rtx reg = regno_reg_rtx[regno];
  machine_mode oldval = GET_MODE (reg);

  if (oldval == newval)
    return;

  if (undobuf.frees)
    buf = undobuf.frees, undobuf.frees = buf->next;
  else
    buf = XNEW (struct undo);

  buf->kind = UNDO_MODE;
  buf->where.regno = regno;
  buf->old_contents.m = oldval;
  adjust_reg_mode (reg, newval);

  buf->next = undobuf.undos, undobuf.undos = buf;
}

/* Similar to SUBST, but NEWVAL is a LOG_LINKS expression.  */

static void
do_SUBST_LINK (struct insn_link **into, struct insn_link *newval)
{
  struct undo *buf;
  struct insn_link * oldval = *into;

  if (oldval == newval)
    return;

  if (undobuf.frees)
    buf = undobuf.frees, undobuf.frees = buf->next;
  else
    buf = XNEW (struct undo);

  buf->kind = UNDO_LINKS;
  buf->where.l = into;
  buf->old_contents.l = oldval;
  *into = newval;

  buf->next = undobuf.undos, undobuf.undos = buf;
}

#define SUBST_LINK(oldval, newval) do_SUBST_LINK (&oldval, newval)

/* Subroutine of try_combine.  Determine whether the replacement patterns
   NEWPAT, NEWI2PAT and NEWOTHERPAT are cheaper according to insn_cost
   than the original sequence I0, I1, I2, I3 and undobuf.other_insn.  Note
   that I0, I1 and/or NEWI2PAT may be NULL_RTX.  Similarly, NEWOTHERPAT and
   undobuf.other_insn may also both be NULL_RTX.  Return false if the cost
   of all the instructions can be estimated and the replacements are more
   expensive than the original sequence.  */

static bool
combine_validate_cost (rtx_insn *i0, rtx_insn *i1, rtx_insn *i2, rtx_insn *i3,
		       rtx newpat, rtx newi2pat, rtx newotherpat)
{
  int i0_cost, i1_cost, i2_cost, i3_cost;
  int new_i2_cost, new_i3_cost;
  int old_cost, new_cost;

  /* Lookup the original insn_costs.  */
  i2_cost = INSN_COST (i2);
  i3_cost = INSN_COST (i3);

  if (i1)
    {
      i1_cost = INSN_COST (i1);
      if (i0)
	{
	  i0_cost = INSN_COST (i0);
	  old_cost = (i0_cost > 0 && i1_cost > 0 && i2_cost > 0 && i3_cost > 0
		      ? i0_cost + i1_cost + i2_cost + i3_cost : 0);
	}
      else
	{
	  old_cost = (i1_cost > 0 && i2_cost > 0 && i3_cost > 0
		      ? i1_cost + i2_cost + i3_cost : 0);
	  i0_cost = 0;
	}
    }
  else
    {
      old_cost = (i2_cost > 0 && i3_cost > 0) ? i2_cost + i3_cost : 0;
      i1_cost = i0_cost = 0;
    }

  /* If we have split a PARALLEL I2 to I1,I2, we have counted its cost twice;
     correct that.  */
  if (old_cost && i1 && INSN_UID (i1) == INSN_UID (i2))
    old_cost -= i1_cost;


  /* Calculate the replacement insn_costs.  */
  rtx tmp = PATTERN (i3);
  PATTERN (i3) = newpat;
  int tmpi = INSN_CODE (i3);
  INSN_CODE (i3) = -1;
  new_i3_cost = insn_cost (i3, optimize_this_for_speed_p);
  PATTERN (i3) = tmp;
  INSN_CODE (i3) = tmpi;
  if (newi2pat)
    {
      tmp = PATTERN (i2);
      PATTERN (i2) = newi2pat;
      tmpi = INSN_CODE (i2);
      INSN_CODE (i2) = -1;
      new_i2_cost = insn_cost (i2, optimize_this_for_speed_p);
      PATTERN (i2) = tmp;
      INSN_CODE (i2) = tmpi;
      new_cost = (new_i2_cost > 0 && new_i3_cost > 0)
		 ? new_i2_cost + new_i3_cost : 0;
    }
  else
    {
      new_cost = new_i3_cost;
      new_i2_cost = 0;
    }

  if (undobuf.other_insn)
    {
      int old_other_cost, new_other_cost;

      old_other_cost = INSN_COST (undobuf.other_insn);
      tmp = PATTERN (undobuf.other_insn);
      PATTERN (undobuf.other_insn) = newotherpat;
      tmpi = INSN_CODE (undobuf.other_insn);
      INSN_CODE (undobuf.other_insn) = -1;
      new_other_cost = insn_cost (undobuf.other_insn,
				  optimize_this_for_speed_p);
      PATTERN (undobuf.other_insn) = tmp;
      INSN_CODE (undobuf.other_insn) = tmpi;
      if (old_other_cost > 0 && new_other_cost > 0)
	{
	  old_cost += old_other_cost;
	  new_cost += new_other_cost;
	}
      else
	old_cost = 0;
    }

  /* Disallow this combination if both new_cost and old_cost are greater than
     zero, and new_cost is greater than old cost.  */
  bool reject = old_cost > 0 && new_cost > old_cost;

  if (dump_file)
    {
      fprintf (dump_file, "%s combination of insns ",
	       reject ? "rejecting" : "allowing");
      if (i0)
	fprintf (dump_file, "%d, ", INSN_UID (i0));
      if (i1 && INSN_UID (i1) != INSN_UID (i2))
	fprintf (dump_file, "%d, ", INSN_UID (i1));
      fprintf (dump_file, "%d and %d\n", INSN_UID (i2), INSN_UID (i3));

      fprintf (dump_file, "original costs ");
      if (i0)
	fprintf (dump_file, "%d + ", i0_cost);
      if (i1 && INSN_UID (i1) != INSN_UID (i2))
	fprintf (dump_file, "%d + ", i1_cost);
      fprintf (dump_file, "%d + %d = %d\n", i2_cost, i3_cost, old_cost);

      if (newi2pat)
	fprintf (dump_file, "replacement costs %d + %d = %d\n",
		 new_i2_cost, new_i3_cost, new_cost);
      else
	fprintf (dump_file, "replacement cost %d\n", new_cost);
    }

  if (reject)
    return false;

  /* Update the uid_insn_cost array with the replacement costs.  */
  INSN_COST (i2) = new_i2_cost;
  INSN_COST (i3) = new_i3_cost;
  if (i1)
    {
      INSN_COST (i1) = 0;
      if (i0)
	INSN_COST (i0) = 0;
    }

  return true;
}


/* Delete any insns that copy a register to itself.
   Return true if the CFG was changed.  */

static bool
delete_noop_moves (void)
{
  rtx_insn *insn, *next;
  basic_block bb;

  bool edges_deleted = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb)); insn = next)
	{
	  next = NEXT_INSN (insn);
	  if (INSN_P (insn) && noop_move_p (insn))
	    {
	      if (dump_file)
		fprintf (dump_file, "deleting noop move %d\n", INSN_UID (insn));

	      edges_deleted |= delete_insn_and_edges (insn);
	    }
	}
    }

  return edges_deleted;
}


/* Return false if we do not want to (or cannot) combine DEF.  */
static bool
can_combine_def_p (df_ref def)
{
  /* Do not consider if it is pre/post modification in MEM.  */
  if (DF_REF_FLAGS (def) & DF_REF_PRE_POST_MODIFY)
    return false;

  unsigned int regno = DF_REF_REGNO (def);

  /* Do not combine frame pointer adjustments.  */
  if ((regno == FRAME_POINTER_REGNUM
       && (!reload_completed || frame_pointer_needed))
      || (!HARD_FRAME_POINTER_IS_FRAME_POINTER
	  && regno == HARD_FRAME_POINTER_REGNUM
	  && (!reload_completed || frame_pointer_needed))
      || (FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  && regno == ARG_POINTER_REGNUM && fixed_regs[regno]))
    return false;

  return true;
}

/* Return false if we do not want to (or cannot) combine USE.  */
static bool
can_combine_use_p (df_ref use)
{
  /* Do not consider the usage of the stack pointer by function call.  */
  if (DF_REF_FLAGS (use) & DF_REF_CALL_STACK_USAGE)
    return false;

  return true;
}

/* Fill in log links field for all insns.  */

static void
create_log_links (void)
{
  basic_block bb;
  rtx_insn **next_use;
  rtx_insn *insn;
  df_ref def, use;

  next_use = XCNEWVEC (rtx_insn *, max_reg_num ());

  /* Pass through each block from the end, recording the uses of each
     register and establishing log links when def is encountered.
     Note that we do not clear next_use array in order to save time,
     so we have to test whether the use is in the same basic block as def.

     There are a few cases below when we do not consider the definition or
     usage -- these are taken from original flow.c did. Don't ask me why it is
     done this way; I don't know and if it works, I don't want to know.  */

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS_REVERSE (bb, insn)
        {
          if (!NONDEBUG_INSN_P (insn))
            continue;

	  /* Log links are created only once.  */
	  gcc_assert (!LOG_LINKS (insn));

	  FOR_EACH_INSN_DEF (def, insn)
            {
              unsigned int regno = DF_REF_REGNO (def);
              rtx_insn *use_insn;

              if (!next_use[regno])
                continue;

	      if (!can_combine_def_p (def))
		continue;

	      use_insn = next_use[regno];
	      next_use[regno] = NULL;

	      if (BLOCK_FOR_INSN (use_insn) != bb)
		continue;

	      /* flow.c claimed:

		 We don't build a LOG_LINK for hard registers contained
		 in ASM_OPERANDs.  If these registers get replaced,
		 we might wind up changing the semantics of the insn,
		 even if reload can make what appear to be valid
		 assignments later.  */
	      if (regno < FIRST_PSEUDO_REGISTER
		  && asm_noperands (PATTERN (use_insn)) >= 0)
		continue;

	      /* Don't add duplicate links between instructions.  */
	      struct insn_link *links;
	      FOR_EACH_LOG_LINK (links, use_insn)
	        if (insn == links->insn && regno == links->regno)
		  break;

	      if (!links)
		LOG_LINKS (use_insn)
		  = alloc_insn_link (insn, regno, LOG_LINKS (use_insn));
            }

	  FOR_EACH_INSN_USE (use, insn)
	    if (can_combine_use_p (use))
	      next_use[DF_REF_REGNO (use)] = insn;
        }
    }

  free (next_use);
}

/* Walk the LOG_LINKS of insn B to see if we find a reference to A.  Return
   true if we found a LOG_LINK that proves that A feeds B.  This only works
   if there are no instructions between A and B which could have a link
   depending on A, since in that case we would not record a link for B.  */

static bool
insn_a_feeds_b (rtx_insn *a, rtx_insn *b)
{
  struct insn_link *links;
  FOR_EACH_LOG_LINK (links, b)
    if (links->insn == a)
      return true;
  return false;
}

/* Main entry point for combiner.  F is the first insn of the function.
   NREGS is the first unused pseudo-reg number.

   Return nonzero if the CFG was changed (e.g. if the combiner has
   turned an indirect jump instruction into a direct jump).  */
static bool
combine_instructions (rtx_insn *f, unsigned int nregs)
{
  rtx_insn *insn, *next;
  struct insn_link *links, *nextlinks;
  rtx_insn *first;
  basic_block last_bb;

  bool new_direct_jump_p = false;

  for (first = f; first && !NONDEBUG_INSN_P (first); )
    first = NEXT_INSN (first);
  if (!first)
    return false;

  combine_attempts = 0;
  combine_merges = 0;
  combine_extras = 0;
  combine_successes = 0;

  rtl_hooks = combine_rtl_hooks;

  reg_stat.safe_grow_cleared (nregs, true);

  init_recog_no_volatile ();

  /* Allocate array for insn info.  */
  max_uid_known = get_max_uid ();
  uid_log_links = XCNEWVEC (struct insn_link *, max_uid_known + 1);
  uid_insn_cost = XCNEWVEC (int, max_uid_known + 1);
  gcc_obstack_init (&insn_link_obstack);

  nonzero_bits_mode = int_mode_for_size (HOST_BITS_PER_WIDE_INT, 0).require ();

  /* Don't use reg_stat[].nonzero_bits when computing it.  This can cause
     problems when, for example, we have j <<= 1 in a loop.  */

  nonzero_sign_valid = 0;
  label_tick = label_tick_ebb_start = 1;

  /* Scan all SETs and see if we can deduce anything about what
     bits are known to be zero for some registers and how many copies
     of the sign bit are known to exist for those registers.

     Also set any known values so that we can use it while searching
     for what bits are known to be set.  */

  setup_incoming_promotions (first);
  /* Allow the entry block and the first block to fall into the same EBB.
     Conceptually the incoming promotions are assigned to the entry block.  */
  last_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  create_log_links ();
  FOR_EACH_BB_FN (this_basic_block, cfun)
    {
      optimize_this_for_speed_p = optimize_bb_for_speed_p (this_basic_block);
      last_call_luid = 0;
      mem_last_set = -1;

      label_tick++;
      if (!single_pred_p (this_basic_block)
	  || single_pred (this_basic_block) != last_bb)
	label_tick_ebb_start = label_tick;
      last_bb = this_basic_block;

      FOR_BB_INSNS (this_basic_block, insn)
        if (INSN_P (insn) && BLOCK_FOR_INSN (insn))
	  {
            rtx links;

            subst_low_luid = DF_INSN_LUID (insn);
            subst_insn = insn;

	    note_stores (insn, set_nonzero_bits_and_sign_copies, insn);
	    record_dead_and_set_regs (insn);

	    if (AUTO_INC_DEC)
	      for (links = REG_NOTES (insn); links; links = XEXP (links, 1))
		if (REG_NOTE_KIND (links) == REG_INC)
		  set_nonzero_bits_and_sign_copies (XEXP (links, 0), NULL_RTX,
						    insn);

	    /* Record the current insn_cost of this instruction.  */
	    INSN_COST (insn) = insn_cost (insn, optimize_this_for_speed_p);
	    if (dump_file)
	      {
		fprintf (dump_file, "insn_cost %d for ", INSN_COST (insn));
		dump_insn_slim (dump_file, insn);
	      }
	  }
    }

  nonzero_sign_valid = 1;

  /* Now scan all the insns in forward order.  */
  label_tick = label_tick_ebb_start = 1;
  init_reg_last ();
  setup_incoming_promotions (first);
  last_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  int max_combine = param_max_combine_insns;

  FOR_EACH_BB_FN (this_basic_block, cfun)
    {
      rtx_insn *last_combined_insn = NULL;

      /* Ignore instruction combination in basic blocks that are going to
	 be removed as unreachable anyway.  See PR82386.  */
      if (EDGE_COUNT (this_basic_block->preds) == 0)
	continue;

      optimize_this_for_speed_p = optimize_bb_for_speed_p (this_basic_block);
      last_call_luid = 0;
      mem_last_set = -1;

      label_tick++;
      if (!single_pred_p (this_basic_block)
	  || single_pred (this_basic_block) != last_bb)
	label_tick_ebb_start = label_tick;
      last_bb = this_basic_block;

      rtl_profile_for_bb (this_basic_block);
      for (insn = BB_HEAD (this_basic_block);
	   insn != NEXT_INSN (BB_END (this_basic_block));
	   insn = next ? next : NEXT_INSN (insn))
	{
	  next = 0;
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  while (last_combined_insn
		 && (!NONDEBUG_INSN_P (last_combined_insn)
		     || last_combined_insn->deleted ()))
	    last_combined_insn = PREV_INSN (last_combined_insn);
	  if (last_combined_insn == NULL_RTX
	      || BLOCK_FOR_INSN (last_combined_insn) != this_basic_block
	      || DF_INSN_LUID (last_combined_insn) <= DF_INSN_LUID (insn))
	    last_combined_insn = insn;

	  /* See if we know about function return values before this
	     insn based upon SUBREG flags.  */
	  check_promoted_subreg (insn, PATTERN (insn));

	  /* See if we can find hardregs and subreg of pseudos in
	     narrower modes.  This could help turning TRUNCATEs
	     into SUBREGs.  */
	  note_uses (&PATTERN (insn), record_truncated_values, NULL);

	  /* Try this insn with each insn it links back to.  */

	  FOR_EACH_LOG_LINK (links, insn)
	    if ((next = try_combine (insn, links->insn, NULL,
				     NULL, &new_direct_jump_p,
				     last_combined_insn)) != 0)
	      {
		statistics_counter_event (cfun, "two-insn combine", 1);
		goto retry;
	      }

	  /* Try each sequence of three linked insns ending with this one.  */

	  if (max_combine >= 3)
	    FOR_EACH_LOG_LINK (links, insn)
	      {
		rtx_insn *link = links->insn;

		/* If the linked insn has been replaced by a note, then there
		   is no point in pursuing this chain any further.  */
		if (NOTE_P (link))
		  continue;

		FOR_EACH_LOG_LINK (nextlinks, link)
		  if ((next = try_combine (insn, link, nextlinks->insn,
					   NULL, &new_direct_jump_p,
					   last_combined_insn)) != 0)
		    {
		      statistics_counter_event (cfun, "three-insn combine", 1);
		      goto retry;
		    }
	      }

	  /* Try combining an insn with two different insns whose results it
	     uses.  */
	  if (max_combine >= 3)
	    FOR_EACH_LOG_LINK (links, insn)
	      for (nextlinks = links->next; nextlinks;
		   nextlinks = nextlinks->next)
		if ((next = try_combine (insn, links->insn,
					 nextlinks->insn, NULL,
					 &new_direct_jump_p,
					 last_combined_insn)) != 0)

		  {
		    statistics_counter_event (cfun, "three-insn combine", 1);
		    goto retry;
		  }

	  /* Try four-instruction combinations.  */
	  if (max_combine >= 4)
	    FOR_EACH_LOG_LINK (links, insn)
	      {
		struct insn_link *next1;
		rtx_insn *link = links->insn;

		/* If the linked insn has been replaced by a note, then there
		   is no point in pursuing this chain any further.  */
		if (NOTE_P (link))
		  continue;

		FOR_EACH_LOG_LINK (next1, link)
		  {
		    rtx_insn *link1 = next1->insn;
		    if (NOTE_P (link1))
		      continue;
		    /* I0 -> I1 -> I2 -> I3.  */
		    FOR_EACH_LOG_LINK (nextlinks, link1)
		      if ((next = try_combine (insn, link, link1,
					       nextlinks->insn,
					       &new_direct_jump_p,
					       last_combined_insn)) != 0)
			{
			  statistics_counter_event (cfun, "four-insn combine", 1);
			  goto retry;
			}
		    /* I0, I1 -> I2, I2 -> I3.  */
		    for (nextlinks = next1->next; nextlinks;
			 nextlinks = nextlinks->next)
		      if ((next = try_combine (insn, link, link1,
					       nextlinks->insn,
					       &new_direct_jump_p,
					       last_combined_insn)) != 0)
			{
			  statistics_counter_event (cfun, "four-insn combine", 1);
			  goto retry;
			}
		  }

		for (next1 = links->next; next1; next1 = next1->next)
		  {
		    rtx_insn *link1 = next1->insn;
		    if (NOTE_P (link1))
		      continue;
		    /* I0 -> I2; I1, I2 -> I3.  */
		    FOR_EACH_LOG_LINK (nextlinks, link)
		      if ((next = try_combine (insn, link, link1,
					       nextlinks->insn,
					       &new_direct_jump_p,
					       last_combined_insn)) != 0)
			{
			  statistics_counter_event (cfun, "four-insn combine", 1);
			  goto retry;
			}
		    /* I0 -> I1; I1, I2 -> I3.  */
		    FOR_EACH_LOG_LINK (nextlinks, link1)
		      if ((next = try_combine (insn, link, link1,
					       nextlinks->insn,
					       &new_direct_jump_p,
					       last_combined_insn)) != 0)
			{
			  statistics_counter_event (cfun, "four-insn combine", 1);
			  goto retry;
			}
		  }
	      }

	  /* Try this insn with each REG_EQUAL note it links back to.  */
	  FOR_EACH_LOG_LINK (links, insn)
	    {
	      rtx set, note;
	      rtx_insn *temp = links->insn;
	      if ((set = single_set (temp)) != 0
		  && (note = find_reg_equal_equiv_note (temp)) != 0
		  && (note = XEXP (note, 0), GET_CODE (note)) != EXPR_LIST
		  && ! side_effects_p (SET_SRC (set))
		  /* Avoid using a register that may already been marked
		     dead by an earlier instruction.  */
		  && ! unmentioned_reg_p (note, SET_SRC (set))
		  && (GET_MODE (note) == VOIDmode
		      ? SCALAR_INT_MODE_P (GET_MODE (SET_DEST (set)))
		      : (GET_MODE (SET_DEST (set)) == GET_MODE (note)
			 && (GET_CODE (SET_DEST (set)) != ZERO_EXTRACT
			     || (GET_MODE (XEXP (SET_DEST (set), 0))
				 == GET_MODE (note))))))
		{
		  /* Temporarily replace the set's source with the
		     contents of the REG_EQUAL note.  The insn will
		     be deleted or recognized by try_combine.  */
		  rtx orig_src = SET_SRC (set);
		  rtx orig_dest = SET_DEST (set);
		  if (GET_CODE (SET_DEST (set)) == ZERO_EXTRACT)
		    SET_DEST (set) = XEXP (SET_DEST (set), 0);
		  SET_SRC (set) = note;
		  i2mod = temp;
		  i2mod_old_rhs = copy_rtx (orig_src);
		  i2mod_new_rhs = copy_rtx (note);
		  next = try_combine (insn, i2mod, NULL, NULL,
				      &new_direct_jump_p,
				      last_combined_insn);
		  i2mod = NULL;
		  if (next)
		    {
		      statistics_counter_event (cfun, "insn-with-note combine", 1);
		      goto retry;
		    }
		  INSN_CODE (temp) = -1;
		  SET_SRC (set) = orig_src;
		  SET_DEST (set) = orig_dest;
		}
	    }

	  if (!NOTE_P (insn))
	    record_dead_and_set_regs (insn);

retry:
	  ;
	}
    }

  default_rtl_profile ();
  clear_bb_flags ();

  if (purge_all_dead_edges ())
    new_direct_jump_p = true;
  if (delete_noop_moves ())
    new_direct_jump_p = true;

  /* Clean up.  */
  obstack_free (&insn_link_obstack, NULL);
  free (uid_log_links);
  free (uid_insn_cost);
  reg_stat.release ();

  {
    struct undo *undo, *next;
    for (undo = undobuf.frees; undo; undo = next)
      {
	next = undo->next;
	free (undo);
      }
    undobuf.frees = 0;
  }

  statistics_counter_event (cfun, "attempts", combine_attempts);
  statistics_counter_event (cfun, "merges", combine_merges);
  statistics_counter_event (cfun, "extras", combine_extras);
  statistics_counter_event (cfun, "successes", combine_successes);

  nonzero_sign_valid = 0;
  rtl_hooks = general_rtl_hooks;

  /* Make recognizer allow volatile MEMs again.  */
  init_recog ();

  return new_direct_jump_p;
}

/* Wipe the last_xxx fields of reg_stat in preparation for another pass.  */

static void
init_reg_last (void)
{
  unsigned int i;
  reg_stat_type *p;

  FOR_EACH_VEC_ELT (reg_stat, i, p)
    memset (p, 0, offsetof (reg_stat_type, sign_bit_copies));
}

/* Set up any promoted values for incoming argument registers.  */

static void
setup_incoming_promotions (rtx_insn *first)
{
  tree arg;
  bool strictly_local = false;

  for (arg = DECL_ARGUMENTS (current_function_decl); arg;
       arg = DECL_CHAIN (arg))
    {
      rtx x, reg = DECL_INCOMING_RTL (arg);
      int uns1, uns3;
      machine_mode mode1, mode2, mode3, mode4;

      /* Only continue if the incoming argument is in a register.  */
      if (!REG_P (reg))
	continue;

      /* Determine, if possible, whether all call sites of the current
         function lie within the current compilation unit.  (This does
	 take into account the exporting of a function via taking its
	 address, and so forth.)  */
      strictly_local
	= cgraph_node::local_info_node (current_function_decl)->local;

      /* The mode and signedness of the argument before any promotions happen
         (equal to the mode of the pseudo holding it at that stage).  */
      mode1 = TYPE_MODE (TREE_TYPE (arg));
      uns1 = TYPE_UNSIGNED (TREE_TYPE (arg));

      /* The mode and signedness of the argument after any source language and
         TARGET_PROMOTE_PROTOTYPES-driven promotions.  */
      mode2 = TYPE_MODE (DECL_ARG_TYPE (arg));
      uns3 = TYPE_UNSIGNED (DECL_ARG_TYPE (arg));

      /* The mode and signedness of the argument as it is actually passed,
         see assign_parm_setup_reg in function.cc.  */
      mode3 = promote_function_mode (TREE_TYPE (arg), mode1, &uns3,
				     TREE_TYPE (cfun->decl), 0);

      /* The mode of the register in which the argument is being passed.  */
      mode4 = GET_MODE (reg);

      /* Eliminate sign extensions in the callee when:
	 (a) A mode promotion has occurred;  */
      if (mode1 == mode3)
	continue;
      /* (b) The mode of the register is the same as the mode of
	     the argument as it is passed; */
      if (mode3 != mode4)
	continue;
      /* (c) There's no language level extension;  */
      if (mode1 == mode2)
	;
      /* (c.1) All callers are from the current compilation unit.  If that's
	 the case we don't have to rely on an ABI, we only have to know
	 what we're generating right now, and we know that we will do the
	 mode1 to mode2 promotion with the given sign.  */
      else if (!strictly_local)
	continue;
      /* (c.2) The combination of the two promotions is useful.  This is
	 true when the signs match, or if the first promotion is unsigned.
	 In the later case, (sign_extend (zero_extend x)) is the same as
	 (zero_extend (zero_extend x)), so make sure to force UNS3 true.  */
      else if (uns1)
	uns3 = true;
      else if (uns3)
	continue;

      /* Record that the value was promoted from mode1 to mode3,
	 so that any sign extension at the head of the current
	 function may be eliminated.  */
      x = gen_rtx_CLOBBER (mode1, const0_rtx);
      x = gen_rtx_fmt_e ((uns3 ? ZERO_EXTEND : SIGN_EXTEND), mode3, x);
      record_value_for_reg (reg, first, x);
    }
}

/* If MODE has a precision lower than PREC and SRC is a non-negative constant
   that would appear negative in MODE, sign-extend SRC for use in nonzero_bits
   because some machines (maybe most) will actually do the sign-extension and
   this is the conservative approach.

   ??? For 2.5, try to tighten up the MD files in this regard instead of this
   kludge.  */

static rtx
sign_extend_short_imm (rtx src, machine_mode mode, unsigned int prec)
{
  scalar_int_mode int_mode;
  if (CONST_INT_P (src)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && GET_MODE_PRECISION (int_mode) < prec
      && INTVAL (src) > 0
      && val_signbit_known_set_p (int_mode, INTVAL (src)))
    src = GEN_INT (INTVAL (src) | ~GET_MODE_MASK (int_mode));

  return src;
}

/* Update RSP for pseudo-register X from INSN's REG_EQUAL note (if one exists)
   and SET.  */

static void
update_rsp_from_reg_equal (reg_stat_type *rsp, rtx_insn *insn, const_rtx set,
			   rtx x)
{
  rtx reg_equal_note = insn ? find_reg_equal_equiv_note (insn) : NULL_RTX;
  unsigned HOST_WIDE_INT bits = 0;
  rtx reg_equal = NULL, src = SET_SRC (set);
  unsigned int num = 0;

  if (reg_equal_note)
    reg_equal = XEXP (reg_equal_note, 0);

  if (SHORT_IMMEDIATES_SIGN_EXTEND)
    {
      src = sign_extend_short_imm (src, GET_MODE (x), BITS_PER_WORD);
      if (reg_equal)
	reg_equal = sign_extend_short_imm (reg_equal, GET_MODE (x), BITS_PER_WORD);
    }

  /* Don't call nonzero_bits if it cannot change anything.  */
  if (rsp->nonzero_bits != HOST_WIDE_INT_M1U)
    {
      machine_mode mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && HWI_COMPUTABLE_MODE_P (mode))
	mode = nonzero_bits_mode;
      bits = nonzero_bits (src, mode);
      if (reg_equal && bits)
	bits &= nonzero_bits (reg_equal, mode);
      rsp->nonzero_bits |= bits;
    }

  /* Don't call num_sign_bit_copies if it cannot change anything.  */
  if (rsp->sign_bit_copies != 1)
    {
      num = num_sign_bit_copies (SET_SRC (set), GET_MODE (x));
      if (reg_equal && maybe_ne (num, GET_MODE_PRECISION (GET_MODE (x))))
	{
	  unsigned int numeq = num_sign_bit_copies (reg_equal, GET_MODE (x));
	  if (num == 0 || numeq > num)
	    num = numeq;
	}
      if (rsp->sign_bit_copies == 0 || num < rsp->sign_bit_copies)
	rsp->sign_bit_copies = num;
    }
}

/* Called via note_stores.  If X is a pseudo that is narrower than
   HOST_BITS_PER_WIDE_INT and is being set, record what bits are known zero.

   If we are setting only a portion of X and we can't figure out what
   portion, assume all bits will be used since we don't know what will
   be happening.

   Similarly, set how many bits of X are known to be copies of the sign bit
   at all locations in the function.  This is the smallest number implied
   by any set of X.  */

static void
set_nonzero_bits_and_sign_copies (rtx x, const_rtx set, void *data)
{
  rtx_insn *insn = (rtx_insn *) data;
  scalar_int_mode mode;

  if (REG_P (x)
      && REGNO (x) >= FIRST_PSEUDO_REGISTER
      /* If this register is undefined at the start of the file, we can't
	 say what its contents were.  */
      && ! REGNO_REG_SET_P
	   (DF_LR_IN (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb), REGNO (x))
      && is_a <scalar_int_mode> (GET_MODE (x), &mode)
      && HWI_COMPUTABLE_MODE_P (mode))
    {
      reg_stat_type *rsp = &reg_stat[REGNO (x)];

      if (set == 0 || GET_CODE (set) == CLOBBER)
	{
	  rsp->nonzero_bits = GET_MODE_MASK (mode);
	  rsp->sign_bit_copies = 1;
	  return;
	}

      /* If this register is being initialized using itself, and the
	 register is uninitialized in this basic block, and there are
	 no LOG_LINKS which set the register, then part of the
	 register is uninitialized.  In that case we can't assume
	 anything about the number of nonzero bits.

	 ??? We could do better if we checked this in
	 reg_{nonzero_bits,num_sign_bit_copies}_for_combine.  Then we
	 could avoid making assumptions about the insn which initially
	 sets the register, while still using the information in other
	 insns.  We would have to be careful to check every insn
	 involved in the combination.  */

      if (insn
	  && reg_referenced_p (x, PATTERN (insn))
	  && !REGNO_REG_SET_P (DF_LR_IN (BLOCK_FOR_INSN (insn)),
			       REGNO (x)))
	{
	  struct insn_link *link;

	  FOR_EACH_LOG_LINK (link, insn)
	    if (dead_or_set_p (link->insn, x))
	      break;
	  if (!link)
	    {
	      rsp->nonzero_bits = GET_MODE_MASK (mode);
	      rsp->sign_bit_copies = 1;
	      return;
	    }
	}

      /* If this is a complex assignment, see if we can convert it into a
	 simple assignment.  */
      set = expand_field_assignment (set);

      /* If this is a simple assignment, or we have a paradoxical SUBREG,
	 set what we know about X.  */

      if (SET_DEST (set) == x
	  || (paradoxical_subreg_p (SET_DEST (set))
	      && SUBREG_REG (SET_DEST (set)) == x))
	update_rsp_from_reg_equal (rsp, insn, set, x);
      else
	{
	  rsp->nonzero_bits = GET_MODE_MASK (mode);
	  rsp->sign_bit_copies = 1;
	}
    }
}

/* See if INSN can be combined into I3.  PRED, PRED2, SUCC and SUCC2 are
   optionally insns that were previously combined into I3 or that will be
   combined into the merger of INSN and I3.  The order is PRED, PRED2,
   INSN, SUCC, SUCC2, I3.

   Return false if the combination is not allowed for any reason.

   If the combination is allowed, *PDEST will be set to the single
   destination of INSN and *PSRC to the single source, and this function
   will return true.  */

static bool
can_combine_p (rtx_insn *insn, rtx_insn *i3, rtx_insn *pred ATTRIBUTE_UNUSED,
	       rtx_insn *pred2 ATTRIBUTE_UNUSED, rtx_insn *succ, rtx_insn *succ2,
	       rtx *pdest, rtx *psrc)
{
  int i;
  const_rtx set = 0;
  rtx src, dest;
  rtx_insn *p;
  rtx link;
  bool all_adjacent = true;
  bool (*is_volatile_p) (const_rtx);

  if (succ)
    {
      if (succ2)
	{
	  if (next_active_insn (succ2) != i3)
	    all_adjacent = false;
	  if (next_active_insn (succ) != succ2)
	    all_adjacent = false;
	}
      else if (next_active_insn (succ) != i3)
	all_adjacent = false;
      if (next_active_insn (insn) != succ)
	all_adjacent = false;
    }
  else if (next_active_insn (insn) != i3)
    all_adjacent = false;
    
  /* Can combine only if previous insn is a SET of a REG or a SUBREG,
     or a PARALLEL consisting of such a SET and CLOBBERs.

     If INSN has CLOBBER parallel parts, ignore them for our processing.
     By definition, these happen during the execution of the insn.  When it
     is merged with another insn, all bets are off.  If they are, in fact,
     needed and aren't also supplied in I3, they may be added by
     recog_for_combine.  Otherwise, it won't match.

     We can also ignore a SET whose SET_DEST is mentioned in a REG_UNUSED
     note.

     Get the source and destination of INSN.  If more than one, can't
     combine.  */

  if (GET_CODE (PATTERN (insn)) == SET)
    set = PATTERN (insn);
  else if (GET_CODE (PATTERN (insn)) == PARALLEL
	   && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
    {
      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	{
	  rtx elt = XVECEXP (PATTERN (insn), 0, i);

	  switch (GET_CODE (elt))
	    {
	    /* This is important to combine floating point insns
	       for the SH4 port.  */
	    case USE:
	      /* Combining an isolated USE doesn't make sense.
		 We depend here on combinable_i3pat to reject them.  */
	      /* The code below this loop only verifies that the inputs of
		 the SET in INSN do not change.  We call reg_set_between_p
		 to verify that the REG in the USE does not change between
		 I3 and INSN.
		 If the USE in INSN was for a pseudo register, the matching
		 insn pattern will likely match any register; combining this
		 with any other USE would only be safe if we knew that the
		 used registers have identical values, or if there was
		 something to tell them apart, e.g. different modes.  For
		 now, we forgo such complicated tests and simply disallow
		 combining of USES of pseudo registers with any other USE.  */
	      if (REG_P (XEXP (elt, 0))
		  && GET_CODE (PATTERN (i3)) == PARALLEL)
		{
		  rtx i3pat = PATTERN (i3);
		  int i = XVECLEN (i3pat, 0) - 1;
		  unsigned int regno = REGNO (XEXP (elt, 0));

		  do
		    {
		      rtx i3elt = XVECEXP (i3pat, 0, i);

		      if (GET_CODE (i3elt) == USE
			  && REG_P (XEXP (i3elt, 0))
			  && (REGNO (XEXP (i3elt, 0)) == regno
			      ? reg_set_between_p (XEXP (elt, 0),
						   PREV_INSN (insn), i3)
			      : regno >= FIRST_PSEUDO_REGISTER))
			return false;
		    }
		  while (--i >= 0);
		}
	      break;

	      /* We can ignore CLOBBERs.  */
	    case CLOBBER:
	      break;

	    case SET:
	      /* Ignore SETs whose result isn't used but not those that
		 have side-effects.  */
	      if (find_reg_note (insn, REG_UNUSED, SET_DEST (elt))
		  && insn_nothrow_p (insn)
		  && !side_effects_p (elt))
		break;

	      /* If we have already found a SET, this is a second one and
		 so we cannot combine with this insn.  */
	      if (set)
		return false;

	      set = elt;
	      break;

	    default:
	      /* Anything else means we can't combine.  */
	      return false;
	    }
	}

      if (set == 0
	  /* If SET_SRC is an ASM_OPERANDS we can't throw away these CLOBBERs,
	     so don't do anything with it.  */
	  || GET_CODE (SET_SRC (set)) == ASM_OPERANDS)
	return false;
    }
  else
    return false;

  if (set == 0)
    return false;

  /* The simplification in expand_field_assignment may call back to
     get_last_value, so set safe guard here.  */
  subst_low_luid = DF_INSN_LUID (insn);

  set = expand_field_assignment (set);
  src = SET_SRC (set), dest = SET_DEST (set);

  /* Do not eliminate user-specified register if it is in an
     asm input because we may break the register asm usage defined
     in GCC manual if allow to do so.
     Be aware that this may cover more cases than we expect but this
     should be harmless.  */
  if (REG_P (dest) && REG_USERVAR_P (dest) && HARD_REGISTER_P (dest)
      && extract_asm_operands (PATTERN (i3)))
    return false;

  /* Don't eliminate a store in the stack pointer.  */
  if (dest == stack_pointer_rtx
      /* Don't combine with an insn that sets a register to itself if it has
	 a REG_EQUAL note.  This may be part of a LIBCALL sequence.  */
      || (rtx_equal_p (src, dest) && find_reg_note (insn, REG_EQUAL, NULL_RTX))
      /* Can't merge an ASM_OPERANDS.  */
      || GET_CODE (src) == ASM_OPERANDS
      /* Can't merge a function call.  */
      || GET_CODE (src) == CALL
      /* Don't eliminate a function call argument.  */
      || (CALL_P (i3)
	  && (find_reg_fusage (i3, USE, dest)
	      || (REG_P (dest)
		  && REGNO (dest) < FIRST_PSEUDO_REGISTER
		  && global_regs[REGNO (dest)])))
      /* Don't substitute into an incremented register.  */
      || FIND_REG_INC_NOTE (i3, dest)
      || (succ && FIND_REG_INC_NOTE (succ, dest))
      || (succ2 && FIND_REG_INC_NOTE (succ2, dest))
      /* Don't substitute into a non-local goto, this confuses CFG.  */
      || (JUMP_P (i3) && find_reg_note (i3, REG_NON_LOCAL_GOTO, NULL_RTX))
      /* Make sure that DEST is not used after INSN but before SUCC, or
	 after SUCC and before SUCC2, or after SUCC2 but before I3.  */
      || (!all_adjacent
	  && ((succ2
	       && (reg_used_between_p (dest, succ2, i3)
		   || reg_used_between_p (dest, succ, succ2)))
	      || (!succ2 && succ && reg_used_between_p (dest, succ, i3))
	      || (!succ2 && !succ && reg_used_between_p (dest, insn, i3))
	      || (succ
		  /* SUCC and SUCC2 can be split halves from a PARALLEL; in
		     that case SUCC is not in the insn stream, so use SUCC2
		     instead for this test.  */
		  && reg_used_between_p (dest, insn,
					 succ2
					 && INSN_UID (succ) == INSN_UID (succ2)
					 ? succ2 : succ))))
      /* Make sure that the value that is to be substituted for the register
	 does not use any registers whose values alter in between.  However,
	 If the insns are adjacent, a use can't cross a set even though we
	 think it might (this can happen for a sequence of insns each setting
	 the same destination; last_set of that register might point to
	 a NOTE).  If INSN has a REG_EQUIV note, the register is always
	 equivalent to the memory so the substitution is valid even if there
	 are intervening stores.  Also, don't move a volatile asm or
	 UNSPEC_VOLATILE across any other insns.  */
      || (! all_adjacent
	  && (((!MEM_P (src)
		|| ! find_reg_note (insn, REG_EQUIV, src))
	       && modified_between_p (src, insn, i3))
	      || (GET_CODE (src) == ASM_OPERANDS && MEM_VOLATILE_P (src))
	      || GET_CODE (src) == UNSPEC_VOLATILE))
      /* Don't combine across a CALL_INSN, because that would possibly
	 change whether the life span of some REGs crosses calls or not,
	 and it is a pain to update that information.
	 Exception: if source is a constant, moving it later can't hurt.
	 Accept that as a special case.  */
      || (DF_INSN_LUID (insn) < last_call_luid && ! CONSTANT_P (src)))
    return false;

  /* DEST must be a REG.  */
  if (REG_P (dest))
    {
      /* If register alignment is being enforced for multi-word items in all
	 cases except for parameters, it is possible to have a register copy
	 insn referencing a hard register that is not allowed to contain the
	 mode being copied and which would not be valid as an operand of most
	 insns.  Eliminate this problem by not combining with such an insn.

	 Also, on some machines we don't want to extend the life of a hard
	 register.  */

      if (REG_P (src)
	  && ((REGNO (dest) < FIRST_PSEUDO_REGISTER
	       && !targetm.hard_regno_mode_ok (REGNO (dest), GET_MODE (dest)))
	      /* Don't extend the life of a hard register unless it is
		 user variable (if we have few registers) or it can't
		 fit into the desired register (meaning something special
		 is going on).
		 Also avoid substituting a return register into I3, because
		 reload can't handle a conflict with constraints of other
		 inputs.  */
	      || (REGNO (src) < FIRST_PSEUDO_REGISTER
		  && !targetm.hard_regno_mode_ok (REGNO (src),
						  GET_MODE (src)))))
	return false;
    }
  else
    return false;


  if (GET_CODE (PATTERN (i3)) == PARALLEL)
    for (i = XVECLEN (PATTERN (i3), 0) - 1; i >= 0; i--)
      if (GET_CODE (XVECEXP (PATTERN (i3), 0, i)) == CLOBBER)
	{
	  rtx reg = XEXP (XVECEXP (PATTERN (i3), 0, i), 0);

	  /* If the clobber represents an earlyclobber operand, we must not
	     substitute an expression containing the clobbered register.
	     As we do not analyze the constraint strings here, we have to
	     make the conservative assumption.  However, if the register is
	     a fixed hard reg, the clobber cannot represent any operand;
	     we leave it up to the machine description to either accept or
	     reject use-and-clobber patterns.  */
	  if (!REG_P (reg)
	      || REGNO (reg) >= FIRST_PSEUDO_REGISTER
	      || !fixed_regs[REGNO (reg)])
	    if (reg_overlap_mentioned_p (reg, src))
	      return false;
	}

  /* If INSN contains anything volatile, or is an `asm' (whether volatile
     or not), reject, unless nothing volatile comes between it and I3 */

  if (GET_CODE (src) == ASM_OPERANDS || volatile_refs_p (src))
    {
      /* Make sure neither succ nor succ2 contains a volatile reference.  */
      if (succ2 != 0 && volatile_refs_p (PATTERN (succ2)))
	return false;
      if (succ != 0 && volatile_refs_p (PATTERN (succ)))
	return false;
      /* We'll check insns between INSN and I3 below.  */
    }

  /* If INSN is an asm, and DEST is a hard register, reject, since it has
     to be an explicit register variable, and was chosen for a reason.  */

  if (GET_CODE (src) == ASM_OPERANDS
      && REG_P (dest) && REGNO (dest) < FIRST_PSEUDO_REGISTER)
    return false;

  /* If INSN contains volatile references (specifically volatile MEMs),
     we cannot combine across any other volatile references.
     Even if INSN doesn't contain volatile references, any intervening
     volatile insn might affect machine state.  */

  is_volatile_p = volatile_refs_p (PATTERN (insn))
    ? volatile_refs_p
    : volatile_insn_p;

  for (p = NEXT_INSN (insn); p != i3; p = NEXT_INSN (p))
    if (INSN_P (p) && p != succ && p != succ2 && is_volatile_p (PATTERN (p)))
      return false;

  /* If INSN contains an autoincrement or autodecrement, make sure that
     register is not used between there and I3, and not already used in
     I3 either.  Neither must it be used in PRED or SUCC, if they exist.
     Also insist that I3 not be a jump if using LRA; if it were one
     and the incremented register were spilled, we would lose.
     Reload handles this correctly.  */

  if (AUTO_INC_DEC)
    for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
      if (REG_NOTE_KIND (link) == REG_INC
	  && ((JUMP_P (i3) && targetm.lra_p ())
	      || reg_used_between_p (XEXP (link, 0), insn, i3)
	      || (pred != NULL_RTX
		  && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (pred)))
	      || (pred2 != NULL_RTX
		  && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (pred2)))
	      || (succ != NULL_RTX
		  && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (succ)))
	      || (succ2 != NULL_RTX
		  && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (succ2)))
	      || reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i3))))
	return false;

  /* If we get here, we have passed all the tests and the combination is
     to be allowed.  */

  *pdest = dest;
  *psrc = src;

  return true;
}

/* LOC is the location within I3 that contains its pattern or the component
   of a PARALLEL of the pattern.  We validate that it is valid for combining.

   One problem is if I3 modifies its output, as opposed to replacing it
   entirely, we can't allow the output to contain I2DEST, I1DEST or I0DEST as
   doing so would produce an insn that is not equivalent to the original insns.

   Consider:

	 (set (reg:DI 101) (reg:DI 100))
	 (set (subreg:SI (reg:DI 101) 0) <foo>)

   This is NOT equivalent to:

	 (parallel [(set (subreg:SI (reg:DI 100) 0) <foo>)
		    (set (reg:DI 101) (reg:DI 100))])

   Not only does this modify 100 (in which case it might still be valid
   if 100 were dead in I2), it sets 101 to the ORIGINAL value of 100.

   We can also run into a problem if I2 sets a register that I1
   uses and I1 gets directly substituted into I3 (not via I2).  In that
   case, we would be getting the wrong value of I2DEST into I3, so we
   must reject the combination.  This case occurs when I2 and I1 both
   feed into I3, rather than when I1 feeds into I2, which feeds into I3.
   If I1_NOT_IN_SRC is nonzero, it means that finding I1 in the source
   of a SET must prevent combination from occurring.  The same situation
   can occur for I0, in which case I0_NOT_IN_SRC is set.

   Before doing the above check, we first try to expand a field assignment
   into a set of logical operations.

   If PI3_DEST_KILLED is nonzero, it is a pointer to a location in which
   we place a register that is both set and used within I3.  If more than one
   such register is detected, we fail.

   Return true if the combination is valid, false otherwise.  */

static bool
combinable_i3pat (rtx_insn *i3, rtx *loc, rtx i2dest, rtx i1dest, rtx i0dest,
		  bool i1_not_in_src, bool i0_not_in_src, rtx *pi3dest_killed)
{
  rtx x = *loc;

  if (GET_CODE (x) == SET)
    {
      rtx set = x ;
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);
      rtx inner_dest = dest;
      rtx subdest;

      while (GET_CODE (inner_dest) == STRICT_LOW_PART
	     || GET_CODE (inner_dest) == SUBREG
	     || GET_CODE (inner_dest) == ZERO_EXTRACT)
	inner_dest = XEXP (inner_dest, 0);

      /* Check for the case where I3 modifies its output, as discussed
	 above.  We don't want to prevent pseudos from being combined
	 into the address of a MEM, so only prevent the combination if
	 i1 or i2 set the same MEM.  */
      if ((inner_dest != dest &&
	   (!MEM_P (inner_dest)
	    || rtx_equal_p (i2dest, inner_dest)
	    || (i1dest && rtx_equal_p (i1dest, inner_dest))
	    || (i0dest && rtx_equal_p (i0dest, inner_dest)))
	   && (reg_overlap_mentioned_p (i2dest, inner_dest)
	       || (i1dest && reg_overlap_mentioned_p (i1dest, inner_dest))
	       || (i0dest && reg_overlap_mentioned_p (i0dest, inner_dest))))

	  /* This is the same test done in can_combine_p except we can't test
	     all_adjacent; we don't have to, since this instruction will stay
	     in place, thus we are not considering increasing the lifetime of
	     INNER_DEST.

	     Also, if this insn sets a function argument, combining it with
	     something that might need a spill could clobber a previous
	     function argument; the all_adjacent test in can_combine_p also
	     checks this; here, we do a more specific test for this case.  */

	  || (REG_P (inner_dest)
	      && REGNO (inner_dest) < FIRST_PSEUDO_REGISTER
	      && !targetm.hard_regno_mode_ok (REGNO (inner_dest),
					      GET_MODE (inner_dest)))
	  || (i1_not_in_src && reg_overlap_mentioned_p (i1dest, src))
	  || (i0_not_in_src && reg_overlap_mentioned_p (i0dest, src)))
	return false;

      /* If DEST is used in I3, it is being killed in this insn, so
	 record that for later.  We have to consider paradoxical
	 subregs here, since they kill the whole register, but we
	 ignore partial subregs, STRICT_LOW_PART, etc.
	 Never add REG_DEAD notes for the FRAME_POINTER_REGNUM or the
	 STACK_POINTER_REGNUM, since these are always considered to be
	 live.  Similarly for ARG_POINTER_REGNUM if it is fixed.  */
      subdest = dest;
      if (GET_CODE (subdest) == SUBREG && !partial_subreg_p (subdest))
	subdest = SUBREG_REG (subdest);
      if (pi3dest_killed
	  && REG_P (subdest)
	  && reg_referenced_p (subdest, PATTERN (i3))
	  && REGNO (subdest) != FRAME_POINTER_REGNUM
	  && (HARD_FRAME_POINTER_IS_FRAME_POINTER
	      || REGNO (subdest) != HARD_FRAME_POINTER_REGNUM)
	  && (FRAME_POINTER_REGNUM == ARG_POINTER_REGNUM
	      || (REGNO (subdest) != ARG_POINTER_REGNUM
		  || ! fixed_regs [REGNO (subdest)]))
	  && REGNO (subdest) != STACK_POINTER_REGNUM)
	{
	  if (*pi3dest_killed)
	    return false;

	  *pi3dest_killed = subdest;
	}
    }

  else if (GET_CODE (x) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (x, 0); i++)
	if (! combinable_i3pat (i3, &XVECEXP (x, 0, i), i2dest, i1dest, i0dest,
				i1_not_in_src, i0_not_in_src, pi3dest_killed))
	  return false;
    }

  return true;
}

/* Return true if X is an arithmetic expression that contains a multiplication
   and division.  We don't count multiplications by powers of two here.  */

static bool
contains_muldiv (rtx x)
{
  switch (GET_CODE (x))
    {
    case MOD:  case DIV:  case UMOD:  case UDIV:
      return true;

    case MULT:
      return ! (CONST_INT_P (XEXP (x, 1))
		&& pow2p_hwi (UINTVAL (XEXP (x, 1))));
    default:
      if (BINARY_P (x))
	return contains_muldiv (XEXP (x, 0))
	    || contains_muldiv (XEXP (x, 1));

      if (UNARY_P (x))
	return contains_muldiv (XEXP (x, 0));

      return false;
    }
}

/* Determine whether INSN can be used in a combination.  Return true if
   not.  This is used in try_combine to detect early some cases where we
   can't perform combinations.  */

static bool
cant_combine_insn_p (rtx_insn *insn)
{
  rtx set;
  rtx src, dest;

  /* If this isn't really an insn, we can't do anything.
     This can occur when flow deletes an insn that it has merged into an
     auto-increment address.  */
  if (!NONDEBUG_INSN_P (insn))
    return true;

  /* Never combine loads and stores involving hard regs that are likely
     to be spilled.  The register allocator can usually handle such
     reg-reg moves by tying.  If we allow the combiner to make
     substitutions of likely-spilled regs, reload might die.
     As an exception, we allow combinations involving fixed regs; these are
     not available to the register allocator so there's no risk involved.  */

  set = single_set (insn);
  if (! set)
    return false;
  src = SET_SRC (set);
  dest = SET_DEST (set);
  if (GET_CODE (src) == SUBREG)
    src = SUBREG_REG (src);
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);
  if (REG_P (src) && REG_P (dest)
      && ((HARD_REGISTER_P (src)
	   && ! TEST_HARD_REG_BIT (fixed_reg_set, REGNO (src))
#ifdef LEAF_REGISTERS
	   && ! LEAF_REGISTERS [REGNO (src)])
#else
	   )
#endif
	  || (HARD_REGISTER_P (dest)
	      && ! TEST_HARD_REG_BIT (fixed_reg_set, REGNO (dest))
	      && targetm.class_likely_spilled_p (REGNO_REG_CLASS (REGNO (dest))))))
    return true;

  return false;
}

struct likely_spilled_retval_info
{
  unsigned regno, nregs;
  unsigned mask;
};

/* Called via note_stores by likely_spilled_retval_p.  Remove from info->mask
   hard registers that are known to be written to / clobbered in full.  */
static void
likely_spilled_retval_1 (rtx x, const_rtx set, void *data)
{
  struct likely_spilled_retval_info *const info =
    (struct likely_spilled_retval_info *) data;
  unsigned regno, nregs;
  unsigned new_mask;

  if (!REG_P (XEXP (set, 0)))
    return;
  regno = REGNO (x);
  if (regno >= info->regno + info->nregs)
    return;
  nregs = REG_NREGS (x);
  if (regno + nregs <= info->regno)
    return;
  new_mask = (2U << (nregs - 1)) - 1;
  if (regno < info->regno)
    new_mask >>= info->regno - regno;
  else
    new_mask <<= regno - info->regno;
  info->mask &= ~new_mask;
}

/* Return true iff part of the return value is live during INSN, and
   it is likely spilled.  This can happen when more than one insn is needed
   to copy the return value, e.g. when we consider to combine into the
   second copy insn for a complex value.  */

static bool
likely_spilled_retval_p (rtx_insn *insn)
{
  rtx_insn *use = BB_END (this_basic_block);
  rtx reg;
  rtx_insn *p;
  unsigned regno, nregs;
  /* We assume here that no machine mode needs more than
     32 hard registers when the value overlaps with a register
     for which TARGET_FUNCTION_VALUE_REGNO_P is true.  */
  unsigned mask;
  struct likely_spilled_retval_info info;

  if (!NONJUMP_INSN_P (use) || GET_CODE (PATTERN (use)) != USE || insn == use)
    return false;
  reg = XEXP (PATTERN (use), 0);
  if (!REG_P (reg) || !targetm.calls.function_value_regno_p (REGNO (reg)))
    return false;
  regno = REGNO (reg);
  nregs = REG_NREGS (reg);
  if (nregs == 1)
    return false;
  mask = (2U << (nregs - 1)) - 1;

  /* Disregard parts of the return value that are set later.  */
  info.regno = regno;
  info.nregs = nregs;
  info.mask = mask;
  for (p = PREV_INSN (use); info.mask && p != insn; p = PREV_INSN (p))
    if (INSN_P (p))
      note_stores (p, likely_spilled_retval_1, &info);
  mask = info.mask;

  /* Check if any of the (probably) live return value registers is
     likely spilled.  */
  nregs --;
  do
    {
      if ((mask & 1 << nregs)
	  && targetm.class_likely_spilled_p (REGNO_REG_CLASS (regno + nregs)))
	return true;
    } while (nregs--);
  return false;
}

/* Adjust INSN after we made a change to its destination.

   Changing the destination can invalidate notes that say something about
   the results of the insn and a LOG_LINK pointing to the insn.  */

static void
adjust_for_new_dest (rtx_insn *insn)
{
  /* For notes, be conservative and simply remove them.  */
  remove_reg_equal_equiv_notes (insn, true);

  /* The new insn will have a destination that was previously the destination
     of an insn just above it.  Call distribute_links to make a LOG_LINK from
     the next use of that destination.  */

  rtx set = single_set (insn);
  gcc_assert (set);

  rtx reg = SET_DEST (set);

  while (GET_CODE (reg) == ZERO_EXTRACT
	 || GET_CODE (reg) == STRICT_LOW_PART
	 || GET_CODE (reg) == SUBREG)
    reg = XEXP (reg, 0);
  gcc_assert (REG_P (reg));

  distribute_links (alloc_insn_link (insn, REGNO (reg), NULL));

  df_insn_rescan (insn);
}

/* Return TRUE if combine can reuse reg X in mode MODE.
   ADDED_SETS is trueif the original set is still required.  */
static bool
can_change_dest_mode (rtx x, bool added_sets, machine_mode mode)
{
  unsigned int regno;

  if (!REG_P (x))
    return false;

  /* Don't change between modes with different underlying register sizes,
     since this could lead to invalid subregs.  */
  if (maybe_ne (REGMODE_NATURAL_SIZE (mode),
		REGMODE_NATURAL_SIZE (GET_MODE (x))))
    return false;

  regno = REGNO (x);
  /* Allow hard registers if the new mode is legal, and occupies no more
     registers than the old mode.  */
  if (regno < FIRST_PSEUDO_REGISTER)
    return (targetm.hard_regno_mode_ok (regno, mode)
	    && REG_NREGS (x) >= hard_regno_nregs (regno, mode));

  /* Or a pseudo that is only used once.  */
  return (regno < reg_n_sets_max
	  && REG_N_SETS (regno) == 1
	  && !added_sets
	  && !REG_USERVAR_P (x));
}


/* Check whether X, the destination of a set, refers to part of
   the register specified by REG.  */

static bool
reg_subword_p (rtx x, rtx reg)
{
  /* Check that reg is an integer mode register.  */
  if (!REG_P (reg) || GET_MODE_CLASS (GET_MODE (reg)) != MODE_INT)
    return false;

  if (GET_CODE (x) == STRICT_LOW_PART
      || GET_CODE (x) == ZERO_EXTRACT)
    x = XEXP (x, 0);

  return GET_CODE (x) == SUBREG
	 && !paradoxical_subreg_p (x)
	 && SUBREG_REG (x) == reg
	 && GET_MODE_CLASS (GET_MODE (x)) == MODE_INT;
}

/* Return whether PAT is a PARALLEL of exactly N register SETs followed
   by an arbitrary number of CLOBBERs.  */
static bool
is_parallel_of_n_reg_sets (rtx pat, int n)
{
  if (GET_CODE (pat) != PARALLEL)
    return false;

  int len = XVECLEN (pat, 0);
  if (len < n)
    return false;

  int i;
  for (i = 0; i < n; i++)
    if (GET_CODE (XVECEXP (pat, 0, i)) != SET
	|| !REG_P (SET_DEST (XVECEXP (pat, 0, i))))
      return false;
  for ( ; i < len; i++)
    switch (GET_CODE (XVECEXP (pat, 0, i)))
      {
      case CLOBBER:
	if (XEXP (XVECEXP (pat, 0, i), 0) == const0_rtx)
	  return false;
	break;
      default:
	return false;
      }
  return true;
}

/* Return whether INSN, a PARALLEL of N register SETs (and maybe some
   CLOBBERs), can be split into individual SETs in that order, without
   changing semantics.  */
static bool
can_split_parallel_of_n_reg_sets (rtx_insn *insn, int n)
{
  if (!insn_nothrow_p (insn))
    return false;

  rtx pat = PATTERN (insn);

  int i, j;
  for (i = 0; i < n; i++)
    {
      if (side_effects_p (SET_SRC (XVECEXP (pat, 0, i))))
	return false;

      rtx reg = SET_DEST (XVECEXP (pat, 0, i));

      for (j = i + 1; j < n; j++)
	if (reg_referenced_p (reg, XVECEXP (pat, 0, j)))
	  return false;
    }

  return true;
}

/* Return whether X is just a single_set, with the source
   a general_operand.  */
static bool
is_just_move (rtx_insn *x)
{
  rtx set = single_set (x);
  if (!set)
    return false;

  return general_operand (SET_SRC (set), VOIDmode);
}

/* Callback function to count autoincs.  */

static int
count_auto_inc (rtx, rtx, rtx, rtx, rtx, void *arg)
{
  (*((int *) arg))++;

  return 0;
}

/* Try to combine the insns I0, I1 and I2 into I3.
   Here I0, I1 and I2 appear earlier than I3.
   I0 and I1 can be zero; then we combine just I2 into I3, or I1 and I2 into
   I3.

   If we are combining more than two insns and the resulting insn is not
   recognized, try splitting it into two insns.  If that happens, I2 and I3
   are retained and I1/I0 are pseudo-deleted by turning them into a NOTE.
   Otherwise, I0, I1 and I2 are pseudo-deleted.

   Return 0 if the combination does not work.  Then nothing is changed.
   If we did the combination, return the insn at which combine should
   resume scanning.

   Set NEW_DIRECT_JUMP_P to true if try_combine creates a
   new direct jump instruction.

   LAST_COMBINED_INSN is either I3, or some insn after I3 that has
   been I3 passed to an earlier try_combine within the same basic
   block.  */

static rtx_insn *
try_combine (rtx_insn *i3, rtx_insn *i2, rtx_insn *i1, rtx_insn *i0,
	     bool *new_direct_jump_p, rtx_insn *last_combined_insn)
{
  /* New patterns for I3 and I2, respectively.  */
  rtx newpat, newi2pat = 0;
  rtvec newpat_vec_with_clobbers = 0;
  bool substed_i2 = false, substed_i1 = false, substed_i0 = false;
  /* Indicates need to preserve SET in I0, I1 or I2 in I3 if it is not
     dead.  */
  bool added_sets_0, added_sets_1, added_sets_2;
  /* Total number of SETs to put into I3.  */
  int total_sets;
  /* Nonzero if I2's or I1's body now appears in I3.  */
  int i2_is_used = 0, i1_is_used = 0;
  /* INSN_CODEs for new I3, new I2, and user of condition code.  */
  int insn_code_number, i2_code_number = 0, other_code_number = 0;
  /* Contains I3 if the destination of I3 is used in its source, which means
     that the old life of I3 is being killed.  If that usage is placed into
     I2 and not in I3, a REG_DEAD note must be made.  */
  rtx i3dest_killed = 0;
  /* SET_DEST and SET_SRC of I2, I1 and I0.  */
  rtx i2dest = 0, i2src = 0, i1dest = 0, i1src = 0, i0dest = 0, i0src = 0;
  /* Copy of SET_SRC of I1 and I0, if needed.  */
  rtx i1src_copy = 0, i0src_copy = 0, i0src_copy2 = 0;
  /* Set if I2DEST was reused as a scratch register.  */
  bool i2scratch = false;
  /* The PATTERNs of I0, I1, and I2, or a copy of them in certain cases.  */
  rtx i0pat = 0, i1pat = 0, i2pat = 0;
  /* Indicates if I2DEST or I1DEST is in I2SRC or I1_SRC.  */
  bool i2dest_in_i2src = false, i1dest_in_i1src = false;
  bool i2dest_in_i1src = false, i0dest_in_i0src = false;
  bool i1dest_in_i0src = false, i2dest_in_i0src = false;;
  bool i2dest_killed = false, i1dest_killed = false, i0dest_killed = false;
  bool i1_feeds_i2_n = false, i0_feeds_i2_n = false, i0_feeds_i1_n = false;
  /* Notes that must be added to REG_NOTES in I3 and I2.  */
  rtx new_i3_notes, new_i2_notes;
  /* Notes that we substituted I3 into I2 instead of the normal case.  */
  bool i3_subst_into_i2 = false;
  /* Notes that I1, I2 or I3 is a MULT operation.  */
  bool have_mult = false;
  bool swap_i2i3 = false;
  bool split_i2i3 = false;
  bool changed_i3_dest = false;
  bool i2_was_move = false, i3_was_move = false;
  int n_auto_inc = 0;

  int maxreg;
  rtx_insn *temp_insn;
  rtx temp_expr;
  struct insn_link *link;
  rtx other_pat = 0;
  rtx new_other_notes;
  int i;
  scalar_int_mode dest_mode, temp_mode;
  bool has_non_call_exception = false;

  /* Immediately return if any of I0,I1,I2 are the same insn (I3 can
     never be).  */
  if (i1 == i2 || i0 == i2 || (i0 && i0 == i1))
    return 0;

  /* Only try four-insn combinations when there's high likelihood of
     success.  Look for simple insns, such as loads of constants or
     binary operations involving a constant.  */
  if (i0)
    {
      int i;
      int ngood = 0;
      int nshift = 0;
      rtx set0, set3;

      if (!flag_expensive_optimizations)
	return 0;

      for (i = 0; i < 4; i++)
	{
	  rtx_insn *insn = i == 0 ? i0 : i == 1 ? i1 : i == 2 ? i2 : i3;
	  rtx set = single_set (insn);
	  rtx src;
	  if (!set)
	    continue;
	  src = SET_SRC (set);
	  if (CONSTANT_P (src))
	    {
	      ngood += 2;
	      break;
	    }
	  else if (BINARY_P (src) && CONSTANT_P (XEXP (src, 1)))
	    ngood++;
	  else if (GET_CODE (src) == ASHIFT || GET_CODE (src) == ASHIFTRT
		   || GET_CODE (src) == LSHIFTRT)
	    nshift++;
	}

      /* If I0 loads a memory and I3 sets the same memory, then I1 and I2
	 are likely manipulating its value.  Ideally we'll be able to combine
	 all four insns into a bitfield insertion of some kind. 

	 Note the source in I0 might be inside a sign/zero extension and the
	 memory modes in I0 and I3 might be different.  So extract the address
	 from the destination of I3 and search for it in the source of I0.

	 In the event that there's a match but the source/dest do not actually
	 refer to the same memory, the worst that happens is we try some
	 combinations that we wouldn't have otherwise.  */
      if ((set0 = single_set (i0))
	  /* Ensure the source of SET0 is a MEM, possibly buried inside
	     an extension.  */
	  && (GET_CODE (SET_SRC (set0)) == MEM
	      || ((GET_CODE (SET_SRC (set0)) == ZERO_EXTEND
		   || GET_CODE (SET_SRC (set0)) == SIGN_EXTEND)
		  && GET_CODE (XEXP (SET_SRC (set0), 0)) == MEM))
	  && (set3 = single_set (i3))
	  /* Ensure the destination of SET3 is a MEM.  */
	  && GET_CODE (SET_DEST (set3)) == MEM
	  /* Would it be better to extract the base address for the MEM
	     in SET3 and look for that?  I don't have cases where it matters
	     but I could envision such cases.  */
	  && rtx_referenced_p (XEXP (SET_DEST (set3), 0), SET_SRC (set0)))
	ngood += 2;

      if (ngood < 2 && nshift < 2)
	return 0;
    }

  /* Exit early if one of the insns involved can't be used for
     combinations.  */
  if (CALL_P (i2)
      || (i1 && CALL_P (i1))
      || (i0 && CALL_P (i0))
      || cant_combine_insn_p (i3)
      || cant_combine_insn_p (i2)
      || (i1 && cant_combine_insn_p (i1))
      || (i0 && cant_combine_insn_p (i0))
      || likely_spilled_retval_p (i3))
    return 0;

  combine_attempts++;
  undobuf.other_insn = 0;

  /* Reset the hard register usage information.  */
  CLEAR_HARD_REG_SET (newpat_used_regs);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (i0)
	fprintf (dump_file, "\nTrying %d, %d, %d -> %d:\n",
		 INSN_UID (i0), INSN_UID (i1), INSN_UID (i2), INSN_UID (i3));
      else if (i1)
	fprintf (dump_file, "\nTrying %d, %d -> %d:\n",
		 INSN_UID (i1), INSN_UID (i2), INSN_UID (i3));
      else
	fprintf (dump_file, "\nTrying %d -> %d:\n",
		 INSN_UID (i2), INSN_UID (i3));

      if (i0)
	dump_insn_slim (dump_file, i0);
      if (i1)
	dump_insn_slim (dump_file, i1);
      dump_insn_slim (dump_file, i2);
      dump_insn_slim (dump_file, i3);
    }

  /* If multiple insns feed into one of I2 or I3, they can be in any
     order.  To simplify the code below, reorder them in sequence.  */
  if (i0 && DF_INSN_LUID (i0) > DF_INSN_LUID (i2))
    std::swap (i0, i2);
  if (i0 && DF_INSN_LUID (i0) > DF_INSN_LUID (i1))
    std::swap (i0, i1);
  if (i1 && DF_INSN_LUID (i1) > DF_INSN_LUID (i2))
    std::swap (i1, i2);

  added_links_insn = 0;
  added_notes_insn = 0;

  /* First check for one important special case that the code below will
     not handle.  Namely, the case where I1 is zero, I2 is a PARALLEL
     and I3 is a SET whose SET_SRC is a SET_DEST in I2.  In that case,
     we may be able to replace that destination with the destination of I3.
     This occurs in the common code where we compute both a quotient and
     remainder into a structure, in which case we want to do the computation
     directly into the structure to avoid register-register copies.

     Note that this case handles both multiple sets in I2 and also cases
     where I2 has a number of CLOBBERs inside the PARALLEL.

     We make very conservative checks below and only try to handle the
     most common cases of this.  For example, we only handle the case
     where I2 and I3 are adjacent to avoid making difficult register
     usage tests.  */

  if (i1 == 0 && NONJUMP_INSN_P (i3) && GET_CODE (PATTERN (i3)) == SET
      && REG_P (SET_SRC (PATTERN (i3)))
      && REGNO (SET_SRC (PATTERN (i3))) >= FIRST_PSEUDO_REGISTER
      && find_reg_note (i3, REG_DEAD, SET_SRC (PATTERN (i3)))
      && GET_CODE (PATTERN (i2)) == PARALLEL
      && ! side_effects_p (SET_DEST (PATTERN (i3)))
      /* If the dest of I3 is a ZERO_EXTRACT or STRICT_LOW_PART, the code
	 below would need to check what is inside (and reg_overlap_mentioned_p
	 doesn't support those codes anyway).  Don't allow those destinations;
	 the resulting insn isn't likely to be recognized anyway.  */
      && GET_CODE (SET_DEST (PATTERN (i3))) != ZERO_EXTRACT
      && GET_CODE (SET_DEST (PATTERN (i3))) != STRICT_LOW_PART
      && ! reg_overlap_mentioned_p (SET_SRC (PATTERN (i3)),
				    SET_DEST (PATTERN (i3)))
      && next_active_insn (i2) == i3)
    {
      rtx p2 = PATTERN (i2);

      /* Make sure that the destination of I3,
	 which we are going to substitute into one output of I2,
	 is not used within another output of I2.  We must avoid making this:
	 (parallel [(set (mem (reg 69)) ...)
		    (set (reg 69) ...)])
	 which is not well-defined as to order of actions.
	 (Besides, reload can't handle output reloads for this.)

	 The problem can also happen if the dest of I3 is a memory ref,
	 if another dest in I2 is an indirect memory ref.

	 Neither can this PARALLEL be an asm.  We do not allow combining
	 that usually (see can_combine_p), so do not here either.  */
      bool ok = true;
      for (i = 0; ok && i < XVECLEN (p2, 0); i++)
	{
	  if ((GET_CODE (XVECEXP (p2, 0, i)) == SET
	       || GET_CODE (XVECEXP (p2, 0, i)) == CLOBBER)
	      && reg_overlap_mentioned_p (SET_DEST (PATTERN (i3)),
					  SET_DEST (XVECEXP (p2, 0, i))))
	    ok = false;
	  else if (GET_CODE (XVECEXP (p2, 0, i)) == SET
		   && GET_CODE (SET_SRC (XVECEXP (p2, 0, i))) == ASM_OPERANDS)
	    ok = false;
	}

      if (ok)
	for (i = 0; i < XVECLEN (p2, 0); i++)
	  if (GET_CODE (XVECEXP (p2, 0, i)) == SET
	      && SET_DEST (XVECEXP (p2, 0, i)) == SET_SRC (PATTERN (i3)))
	    {
	      combine_merges++;

	      subst_insn = i3;
	      subst_low_luid = DF_INSN_LUID (i2);

	      added_sets_2 = added_sets_1 = added_sets_0 = false;
	      i2src = SET_SRC (XVECEXP (p2, 0, i));
	      i2dest = SET_DEST (XVECEXP (p2, 0, i));
	      i2dest_killed = dead_or_set_p (i2, i2dest);

	      /* Replace the dest in I2 with our dest and make the resulting
		 insn the new pattern for I3.  Then skip to where we validate
		 the pattern.  Everything was set up above.  */
	      SUBST (SET_DEST (XVECEXP (p2, 0, i)), SET_DEST (PATTERN (i3)));
	      newpat = p2;
	      i3_subst_into_i2 = true;
	      goto validate_replacement;
	    }
    }

  /* If I2 is setting a pseudo to a constant and I3 is setting some
     sub-part of it to another constant, merge them by making a new
     constant.  */
  if (i1 == 0
      && (temp_expr = single_set (i2)) != 0
      && is_a <scalar_int_mode> (GET_MODE (SET_DEST (temp_expr)), &temp_mode)
      && CONST_SCALAR_INT_P (SET_SRC (temp_expr))
      && GET_CODE (PATTERN (i3)) == SET
      && CONST_SCALAR_INT_P (SET_SRC (PATTERN (i3)))
      && reg_subword_p (SET_DEST (PATTERN (i3)), SET_DEST (temp_expr)))
    {
      rtx dest = SET_DEST (PATTERN (i3));
      rtx temp_dest = SET_DEST (temp_expr);
      int offset = -1;
      int width = 0;

      if (GET_CODE (dest) == ZERO_EXTRACT)
	{
	  if (CONST_INT_P (XEXP (dest, 1))
	      && CONST_INT_P (XEXP (dest, 2))
	      && is_a <scalar_int_mode> (GET_MODE (XEXP (dest, 0)),
					 &dest_mode))
	    {
	      width = INTVAL (XEXP (dest, 1));
	      offset = INTVAL (XEXP (dest, 2));
	      dest = XEXP (dest, 0);
	      if (BITS_BIG_ENDIAN)
		offset = GET_MODE_PRECISION (dest_mode) - width - offset;
	    }
	}
      else
	{
	  if (GET_CODE (dest) == STRICT_LOW_PART)
	    dest = XEXP (dest, 0);
	  if (is_a <scalar_int_mode> (GET_MODE (dest), &dest_mode))
	    {
	      width = GET_MODE_PRECISION (dest_mode);
	      offset = 0;
	    }
	}

      if (offset >= 0)
	{
	  /* If this is the low part, we're done.  */
	  if (subreg_lowpart_p (dest))
	    ;
	  /* Handle the case where inner is twice the size of outer.  */
	  else if (GET_MODE_PRECISION (temp_mode)
		   == 2 * GET_MODE_PRECISION (dest_mode))
	    offset += GET_MODE_PRECISION (dest_mode);
	  /* Otherwise give up for now.  */
	  else
	    offset = -1;
	}

      if (offset >= 0)
	{
	  rtx inner = SET_SRC (PATTERN (i3));
	  rtx outer = SET_SRC (temp_expr);

	  wide_int o = wi::insert (rtx_mode_t (outer, temp_mode),
				   rtx_mode_t (inner, dest_mode),
				   offset, width);

	  combine_merges++;
	  subst_insn = i3;
	  subst_low_luid = DF_INSN_LUID (i2);
	  added_sets_2 = added_sets_1 = added_sets_0 = false;
	  i2dest = temp_dest;
	  i2dest_killed = dead_or_set_p (i2, i2dest);

	  /* Replace the source in I2 with the new constant and make the
	     resulting insn the new pattern for I3.  Then skip to where we
	     validate the pattern.  Everything was set up above.  */
	  SUBST (SET_SRC (temp_expr),
		 immed_wide_int_const (o, temp_mode));

	  newpat = PATTERN (i2);

          /* The dest of I3 has been replaced with the dest of I2.  */
	  changed_i3_dest = true;
	  goto validate_replacement;
	}
    }

  /* If we have no I1 and I2 looks like:
	(parallel [(set (reg:CC X) (compare:CC OP (const_int 0)))
		   (set Y OP)])
     make up a dummy I1 that is
	(set Y OP)
     and change I2 to be
	(set (reg:CC X) (compare:CC Y (const_int 0)))

     (We can ignore any trailing CLOBBERs.)

     This undoes a previous combination and allows us to match a branch-and-
     decrement insn.  */

  if (i1 == 0
      && is_parallel_of_n_reg_sets (PATTERN (i2), 2)
      && (GET_MODE_CLASS (GET_MODE (SET_DEST (XVECEXP (PATTERN (i2), 0, 0))))
	  == MODE_CC)
      && GET_CODE (SET_SRC (XVECEXP (PATTERN (i2), 0, 0))) == COMPARE
      && XEXP (SET_SRC (XVECEXP (PATTERN (i2), 0, 0)), 1) == const0_rtx
      && rtx_equal_p (XEXP (SET_SRC (XVECEXP (PATTERN (i2), 0, 0)), 0),
		      SET_SRC (XVECEXP (PATTERN (i2), 0, 1)))
      && !reg_used_between_p (SET_DEST (XVECEXP (PATTERN (i2), 0, 0)), i2, i3)
      && !reg_used_between_p (SET_DEST (XVECEXP (PATTERN (i2), 0, 1)), i2, i3))
    {
      /* We make I1 with the same INSN_UID as I2.  This gives it
	 the same DF_INSN_LUID for value tracking.  Our fake I1 will
	 never appear in the insn stream so giving it the same INSN_UID
	 as I2 will not cause a problem.  */

      i1 = gen_rtx_INSN (VOIDmode, NULL, i2, BLOCK_FOR_INSN (i2),
			 XVECEXP (PATTERN (i2), 0, 1), INSN_LOCATION (i2),
			 -1, NULL_RTX);
      INSN_UID (i1) = INSN_UID (i2);

      SUBST (PATTERN (i2), XVECEXP (PATTERN (i2), 0, 0));
      SUBST (XEXP (SET_SRC (PATTERN (i2)), 0),
	     SET_DEST (PATTERN (i1)));
      unsigned int regno = REGNO (SET_DEST (PATTERN (i1)));
      SUBST_LINK (LOG_LINKS (i2),
		  alloc_insn_link (i1, regno, LOG_LINKS (i2)));
    }

  /* If I2 is a PARALLEL of two SETs of REGs (and perhaps some CLOBBERs),
     make those two SETs separate I1 and I2 insns, and make an I0 that is
     the original I1.  */
  if (i0 == 0
      && is_parallel_of_n_reg_sets (PATTERN (i2), 2)
      && can_split_parallel_of_n_reg_sets (i2, 2)
      && !reg_used_between_p (SET_DEST (XVECEXP (PATTERN (i2), 0, 0)), i2, i3)
      && !reg_used_between_p (SET_DEST (XVECEXP (PATTERN (i2), 0, 1)), i2, i3)
      && !reg_set_between_p  (SET_DEST (XVECEXP (PATTERN (i2), 0, 0)), i2, i3)
      && !reg_set_between_p  (SET_DEST (XVECEXP (PATTERN (i2), 0, 1)), i2, i3))
    {
      /* If there is no I1, there is no I0 either.  */
      i0 = i1;

      /* We make I1 with the same INSN_UID as I2.  This gives it
	 the same DF_INSN_LUID for value tracking.  Our fake I1 will
	 never appear in the insn stream so giving it the same INSN_UID
	 as I2 will not cause a problem.  */

      i1 = gen_rtx_INSN (VOIDmode, NULL, i2, BLOCK_FOR_INSN (i2),
			 XVECEXP (PATTERN (i2), 0, 0), INSN_LOCATION (i2),
			 -1, NULL_RTX);
      INSN_UID (i1) = INSN_UID (i2);

      SUBST (PATTERN (i2), XVECEXP (PATTERN (i2), 0, 1));
    }

  /* Verify that I2 and maybe I1 and I0 can be combined into I3.  */
  if (!can_combine_p (i2, i3, i0, i1, NULL, NULL, &i2dest, &i2src))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Can't combine i2 into i3\n");
      undo_all ();
      return 0;
    }
  if (i1 && !can_combine_p (i1, i3, i0, NULL, i2, NULL, &i1dest, &i1src))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Can't combine i1 into i3\n");
      undo_all ();
      return 0;
    }
  if (i0 && !can_combine_p (i0, i3, NULL, NULL, i1, i2, &i0dest, &i0src))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Can't combine i0 into i3\n");
      undo_all ();
      return 0;
    }

  /* With non-call exceptions we can end up trying to combine multiple
     insns with possible EH side effects.  Make sure we can combine
     that to a single insn which means there must be at most one insn
     in the combination with an EH side effect.  */
  if (cfun->can_throw_non_call_exceptions)
    {
      if (find_reg_note (i3, REG_EH_REGION, NULL_RTX)
	  || find_reg_note (i2, REG_EH_REGION, NULL_RTX)
	  || (i1 && find_reg_note (i1, REG_EH_REGION, NULL_RTX))
	  || (i0 && find_reg_note (i0, REG_EH_REGION, NULL_RTX)))
	{
	  has_non_call_exception = true;
	  if (insn_could_throw_p (i3)
	      + insn_could_throw_p (i2)
	      + (i1 ? insn_could_throw_p (i1) : 0)
	      + (i0 ? insn_could_throw_p (i0) : 0) > 1)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Can't combine multiple insns with EH "
			 "side-effects\n");
	      undo_all ();
	      return 0;
	    }
	}
    }

  /* Record whether i2 and i3 are trivial moves.  */
  i2_was_move = is_just_move (i2);
  i3_was_move = is_just_move (i3);

  /* Record whether I2DEST is used in I2SRC and similarly for the other
     cases.  Knowing this will help in register status updating below.  */
  i2dest_in_i2src = reg_overlap_mentioned_p (i2dest, i2src);
  i1dest_in_i1src = i1 && reg_overlap_mentioned_p (i1dest, i1src);
  i2dest_in_i1src = i1 && reg_overlap_mentioned_p (i2dest, i1src);
  i0dest_in_i0src = i0 && reg_overlap_mentioned_p (i0dest, i0src);
  i1dest_in_i0src = i0 && reg_overlap_mentioned_p (i1dest, i0src);
  i2dest_in_i0src = i0 && reg_overlap_mentioned_p (i2dest, i0src);
  i2dest_killed = dead_or_set_p (i2, i2dest);
  i1dest_killed = i1 && dead_or_set_p (i1, i1dest);
  i0dest_killed = i0 && dead_or_set_p (i0, i0dest);

  /* For the earlier insns, determine which of the subsequent ones they
     feed.  */
  i1_feeds_i2_n = i1 && insn_a_feeds_b (i1, i2);
  i0_feeds_i1_n = i0 && insn_a_feeds_b (i0, i1);
  i0_feeds_i2_n = (i0 && (!i0_feeds_i1_n ? insn_a_feeds_b (i0, i2)
			  : (!reg_overlap_mentioned_p (i1dest, i0dest)
			     && reg_overlap_mentioned_p (i0dest, i2src))));

  /* Ensure that I3's pattern can be the destination of combines.  */
  if (! combinable_i3pat (i3, &PATTERN (i3), i2dest, i1dest, i0dest,
			  i1 && i2dest_in_i1src && !i1_feeds_i2_n,
			  i0 && ((i2dest_in_i0src && !i0_feeds_i2_n)
				 || (i1dest_in_i0src && !i0_feeds_i1_n)),
			  &i3dest_killed))
    {
      undo_all ();
      return 0;
    }

  /* See if any of the insns is a MULT operation.  Unless one is, we will
     reject a combination that is, since it must be slower.  Be conservative
     here.  */
  if (GET_CODE (i2src) == MULT
      || (i1 != 0 && GET_CODE (i1src) == MULT)
      || (i0 != 0 && GET_CODE (i0src) == MULT)
      || (GET_CODE (PATTERN (i3)) == SET
	  && GET_CODE (SET_SRC (PATTERN (i3))) == MULT))
    have_mult = true;

  /* If I3 has an inc, then give up if I1 or I2 uses the reg that is inc'd.
     We used to do this EXCEPT in one case: I3 has a post-inc in an
     output operand.  However, that exception can give rise to insns like
	mov r3,(r3)+
     which is a famous insn on the PDP-11 where the value of r3 used as the
     source was model-dependent.  Avoid this sort of thing.  */

#if 0
  if (!(GET_CODE (PATTERN (i3)) == SET
	&& REG_P (SET_SRC (PATTERN (i3)))
	&& MEM_P (SET_DEST (PATTERN (i3)))
	&& (GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_INC
	    || GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_DEC)))
    /* It's not the exception.  */
#endif
    if (AUTO_INC_DEC)
      {
	rtx link;
	for (link = REG_NOTES (i3); link; link = XEXP (link, 1))
	  if (REG_NOTE_KIND (link) == REG_INC
	      && (reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i2))
		  || (i1 != 0
		      && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i1)))))
	    {
	      undo_all ();
	      return 0;
	    }
      }

  /* See if the SETs in I1 or I2 need to be kept around in the merged
     instruction: whenever the value set there is still needed past I3.
     For the SET in I2, this is easy: we see if I2DEST dies or is set in I3.

     For the SET in I1, we have two cases: if I1 and I2 independently feed
     into I3, the set in I1 needs to be kept around unless I1DEST dies
     or is set in I3.  Otherwise (if I1 feeds I2 which feeds I3), the set
     in I1 needs to be kept around unless I1DEST dies or is set in either
     I2 or I3.  The same considerations apply to I0.  */

  added_sets_2 = !dead_or_set_p (i3, i2dest);

  if (i1)
    added_sets_1 = !(dead_or_set_p (i3, i1dest)
		     || (i1_feeds_i2_n && dead_or_set_p (i2, i1dest)));
  else
    added_sets_1 = false;

  if (i0)
    added_sets_0 =  !(dead_or_set_p (i3, i0dest)
		      || (i0_feeds_i1_n && dead_or_set_p (i1, i0dest))
		      || ((i0_feeds_i2_n || (i0_feeds_i1_n && i1_feeds_i2_n))
			  && dead_or_set_p (i2, i0dest)));
  else
    added_sets_0 = false;

  /* We are about to copy insns for the case where they need to be kept
     around.  Check that they can be copied in the merged instruction.  */

  if (targetm.cannot_copy_insn_p
      && ((added_sets_2 && targetm.cannot_copy_insn_p (i2))
	  || (i1 && added_sets_1 && targetm.cannot_copy_insn_p (i1))
	  || (i0 && added_sets_0 && targetm.cannot_copy_insn_p (i0))))
    {
      undo_all ();
      return 0;
    }

  /* We cannot safely duplicate volatile references in any case.  */

  if ((added_sets_2 && volatile_refs_p (PATTERN (i2)))
      || (added_sets_1 && volatile_refs_p (PATTERN (i1)))
      || (added_sets_0 && volatile_refs_p (PATTERN (i0))))
    {
      undo_all ();
      return 0;
    }

  /* Count how many auto_inc expressions there were in the original insns;
     we need to have the same number in the resulting patterns.  */

  if (i0)
    for_each_inc_dec (PATTERN (i0), count_auto_inc, &n_auto_inc);
  if (i1)
    for_each_inc_dec (PATTERN (i1), count_auto_inc, &n_auto_inc);
  for_each_inc_dec (PATTERN (i2), count_auto_inc, &n_auto_inc);
  for_each_inc_dec (PATTERN (i3), count_auto_inc, &n_auto_inc);

  /* If the set in I2 needs to be kept around, we must make a copy of
     PATTERN (I2), so that when we substitute I1SRC for I1DEST in
     PATTERN (I2), we are only substituting for the original I1DEST, not into
     an already-substituted copy.  This also prevents making self-referential
     rtx.  If I2 is a PARALLEL, we just need the piece that assigns I2SRC to
     I2DEST.  */

  if (added_sets_2)
    {
      if (GET_CODE (PATTERN (i2)) == PARALLEL)
	i2pat = gen_rtx_SET (i2dest, copy_rtx (i2src));
      else
	i2pat = copy_rtx (PATTERN (i2));
    }

  if (added_sets_1)
    {
      if (GET_CODE (PATTERN (i1)) == PARALLEL)
	i1pat = gen_rtx_SET (i1dest, copy_rtx (i1src));
      else
	i1pat = copy_rtx (PATTERN (i1));
    }

  if (added_sets_0)
    {
      if (GET_CODE (PATTERN (i0)) == PARALLEL)
	i0pat = gen_rtx_SET (i0dest, copy_rtx (i0src));
      else
	i0pat = copy_rtx (PATTERN (i0));
    }

  combine_merges++;

  /* Substitute in the latest insn for the regs set by the earlier ones.  */

  maxreg = max_reg_num ();

  subst_insn = i3;

  /* Many machines have insns that can both perform an
     arithmetic operation and set the condition code.  These operations will
     be represented as a PARALLEL with the first element of the vector
     being a COMPARE of an arithmetic operation with the constant zero.
     The second element of the vector will set some pseudo to the result
     of the same arithmetic operation.  If we simplify the COMPARE, we won't
     match such a pattern and so will generate an extra insn.   Here we test
     for this case, where both the comparison and the operation result are
     needed, and make the PARALLEL by just replacing I2DEST in I3SRC with
     I2SRC.  Later we will make the PARALLEL that contains I2.  */

  if (i1 == 0 && added_sets_2 && GET_CODE (PATTERN (i3)) == SET
      && GET_CODE (SET_SRC (PATTERN (i3))) == COMPARE
      && CONST_INT_P (XEXP (SET_SRC (PATTERN (i3)), 1))
      && rtx_equal_p (XEXP (SET_SRC (PATTERN (i3)), 0), i2dest))
    {
      rtx newpat_dest;
      rtx *cc_use_loc = NULL;
      rtx_insn *cc_use_insn = NULL;
      rtx op0 = i2src, op1 = XEXP (SET_SRC (PATTERN (i3)), 1);
      machine_mode compare_mode, orig_compare_mode;
      enum rtx_code compare_code = UNKNOWN, orig_compare_code = UNKNOWN;
      scalar_int_mode mode;

      newpat = PATTERN (i3);
      newpat_dest = SET_DEST (newpat);
      compare_mode = orig_compare_mode = GET_MODE (newpat_dest);

      if (undobuf.other_insn == 0
	  && (cc_use_loc = find_single_use (SET_DEST (newpat), i3,
					    &cc_use_insn)))
	{
	  compare_code = orig_compare_code = GET_CODE (*cc_use_loc);
	  if (is_a <scalar_int_mode> (GET_MODE (i2dest), &mode))
	    compare_code = simplify_compare_const (compare_code, mode,
						   &op0, &op1);
	  target_canonicalize_comparison (&compare_code, &op0, &op1, 1);
	}

      /* Do the rest only if op1 is const0_rtx, which may be the
	 result of simplification.  */
      if (op1 == const0_rtx)
	{
	  /* If a single use of the CC is found, prepare to modify it
	     when SELECT_CC_MODE returns a new CC-class mode, or when
	     the above simplify_compare_const() returned a new comparison
	     operator.  undobuf.other_insn is assigned the CC use insn
	     when modifying it.  */
	  if (cc_use_loc)
	    {
#ifdef SELECT_CC_MODE
	      machine_mode new_mode
		= SELECT_CC_MODE (compare_code, op0, op1);
	      if (new_mode != orig_compare_mode
		  && can_change_dest_mode (SET_DEST (newpat),
					   added_sets_2, new_mode))
		{
		  unsigned int regno = REGNO (newpat_dest);
		  compare_mode = new_mode;
		  if (regno < FIRST_PSEUDO_REGISTER)
		    newpat_dest = gen_rtx_REG (compare_mode, regno);
		  else
		    {
		      subst_mode (regno, compare_mode);
		      newpat_dest = regno_reg_rtx[regno];
		    }
		}
#endif
	      /* Cases for modifying the CC-using comparison.  */
	      if (compare_code != orig_compare_code
		  /* ??? Do we need to verify the zero rtx?  */
		  && XEXP (*cc_use_loc, 1) == const0_rtx)
		{
		  /* Replace cc_use_loc with entire new RTX.  */
		  SUBST (*cc_use_loc,
			 gen_rtx_fmt_ee (compare_code, GET_MODE (*cc_use_loc),
					 newpat_dest, const0_rtx));
		  undobuf.other_insn = cc_use_insn;
		}
	      else if (compare_mode != orig_compare_mode)
		{
		  /* Just replace the CC reg with a new mode.  */
		  SUBST (XEXP (*cc_use_loc, 0), newpat_dest);
		  undobuf.other_insn = cc_use_insn;
		}
	    }

	  /* Now we modify the current newpat:
	     First, SET_DEST(newpat) is updated if the CC mode has been
	     altered. For targets without SELECT_CC_MODE, this should be
	     optimized away.  */
	  if (compare_mode != orig_compare_mode)
	    SUBST (SET_DEST (newpat), newpat_dest);
	  /* This is always done to propagate i2src into newpat.  */
	  SUBST (SET_SRC (newpat),
		 gen_rtx_COMPARE (compare_mode, op0, op1));
	  /* Create new version of i2pat if needed; the below PARALLEL
	     creation needs this to work correctly.  */
	  if (! rtx_equal_p (i2src, op0))
	    i2pat = gen_rtx_SET (i2dest, op0);
	  i2_is_used = 1;
	}
    }

  if (i2_is_used == 0)
    {
      /* It is possible that the source of I2 or I1 may be performing
	 an unneeded operation, such as a ZERO_EXTEND of something
	 that is known to have the high part zero.  Handle that case
	 by letting subst look at the inner insns.

	 Another way to do this would be to have a function that tries
	 to simplify a single insn instead of merging two or more
	 insns.  We don't do this because of the potential of infinite
	 loops and because of the potential extra memory required.
	 However, doing it the way we are is a bit of a kludge and
	 doesn't catch all cases.

	 But only do this if -fexpensive-optimizations since it slows
	 things down and doesn't usually win.

	 This is not done in the COMPARE case above because the
	 unmodified I2PAT is used in the PARALLEL and so a pattern
	 with a modified I2SRC would not match.  */

      if (flag_expensive_optimizations)
	{
	  /* Pass pc_rtx so no substitutions are done, just
	     simplifications.  */
	  if (i1)
	    {
	      subst_low_luid = DF_INSN_LUID (i1);
	      i1src = subst (i1src, pc_rtx, pc_rtx, false, false, false);
	    }

	  subst_low_luid = DF_INSN_LUID (i2);
	  i2src = subst (i2src, pc_rtx, pc_rtx, false, false, false);
	}

      n_occurrences = 0;		/* `subst' counts here */
      subst_low_luid = DF_INSN_LUID (i2);

      /* If I1 feeds into I2 and I1DEST is in I1SRC, we need to make a unique
	 copy of I2SRC each time we substitute it, in order to avoid creating
	 self-referential RTL when we will be substituting I1SRC for I1DEST
	 later.  Likewise if I0 feeds into I2, either directly or indirectly
	 through I1, and I0DEST is in I0SRC.  */
      newpat = subst (PATTERN (i3), i2dest, i2src, false, false,
		      (i1_feeds_i2_n && i1dest_in_i1src)
		      || ((i0_feeds_i2_n || (i0_feeds_i1_n && i1_feeds_i2_n))
			  && i0dest_in_i0src));
      substed_i2 = true;

      /* Record whether I2's body now appears within I3's body.  */
      i2_is_used = n_occurrences;
    }

  /* If we already got a failure, don't try to do more.  Otherwise, try to
     substitute I1 if we have it.  */

  if (i1 && GET_CODE (newpat) != CLOBBER)
    {
      /* Before we can do this substitution, we must redo the test done
	 above (see detailed comments there) that ensures I1DEST isn't
	 mentioned in any SETs in NEWPAT that are field assignments.  */
      if (!combinable_i3pat (NULL, &newpat, i1dest, NULL_RTX, NULL_RTX,
			     false, false, 0))
	{
	  undo_all ();
	  return 0;
	}

      n_occurrences = 0;
      subst_low_luid = DF_INSN_LUID (i1);

      /* If the following substitution will modify I1SRC, make a copy of it
	 for the case where it is substituted for I1DEST in I2PAT later.  */
      if (added_sets_2 && i1_feeds_i2_n)
	i1src_copy = copy_rtx (i1src);

      /* If I0 feeds into I1 and I0DEST is in I0SRC, we need to make a unique
	 copy of I1SRC each time we substitute it, in order to avoid creating
	 self-referential RTL when we will be substituting I0SRC for I0DEST
	 later.  */
      newpat = subst (newpat, i1dest, i1src, false, false,
		      i0_feeds_i1_n && i0dest_in_i0src);
      substed_i1 = true;

      /* Record whether I1's body now appears within I3's body.  */
      i1_is_used = n_occurrences;
    }

  /* Likewise for I0 if we have it.  */

  if (i0 && GET_CODE (newpat) != CLOBBER)
    {
      if (!combinable_i3pat (NULL, &newpat, i0dest, NULL_RTX, NULL_RTX,
			     false, false, 0))
	{
	  undo_all ();
	  return 0;
	}

      /* If the following substitution will modify I0SRC, make a copy of it
	 for the case where it is substituted for I0DEST in I1PAT later.  */
      if (added_sets_1 && i0_feeds_i1_n)
	i0src_copy = copy_rtx (i0src);
      /* And a copy for I0DEST in I2PAT substitution.  */
      if (added_sets_2 && ((i0_feeds_i1_n && i1_feeds_i2_n)
			   || (i0_feeds_i2_n)))
	i0src_copy2 = copy_rtx (i0src);

      n_occurrences = 0;
      subst_low_luid = DF_INSN_LUID (i0);
      newpat = subst (newpat, i0dest, i0src, false, false, false);
      substed_i0 = true;
    }

  if (n_auto_inc)
    {
      int new_n_auto_inc = 0;
      for_each_inc_dec (newpat, count_auto_inc, &new_n_auto_inc);

      if (n_auto_inc != new_n_auto_inc)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Number of auto_inc expressions changed\n");
	  undo_all ();
	  return 0;
	}
    }

  /* Fail if an autoincrement side-effect has been duplicated.  Be careful
     to count all the ways that I2SRC and I1SRC can be used.  */
  if ((FIND_REG_INC_NOTE (i2, NULL_RTX) != 0
       && i2_is_used + added_sets_2 > 1)
      || (i1 != 0 && FIND_REG_INC_NOTE (i1, NULL_RTX) != 0
	  && (i1_is_used + added_sets_1 + (added_sets_2 && i1_feeds_i2_n) > 1))
      || (i0 != 0 && FIND_REG_INC_NOTE (i0, NULL_RTX) != 0
	  && (n_occurrences + added_sets_0
	      + (added_sets_1 && i0_feeds_i1_n)
	      + (added_sets_2 && i0_feeds_i2_n) > 1))
      /* Fail if we tried to make a new register.  */
      || max_reg_num () != maxreg
      /* Fail if we couldn't do something and have a CLOBBER.  */
      || GET_CODE (newpat) == CLOBBER
      /* Fail if this new pattern is a MULT and we didn't have one before
	 at the outer level.  */
      || (GET_CODE (newpat) == SET && GET_CODE (SET_SRC (newpat)) == MULT
	  && ! have_mult))
    {
      undo_all ();
      return 0;
    }

  /* If the actions of the earlier insns must be kept
     in addition to substituting them into the latest one,
     we must make a new PARALLEL for the latest insn
     to hold additional the SETs.  */

  if (added_sets_0 || added_sets_1 || added_sets_2)
    {
      int extra_sets = added_sets_0 + added_sets_1 + added_sets_2;
      combine_extras++;

      if (GET_CODE (newpat) == PARALLEL)
	{
	  rtvec old = XVEC (newpat, 0);
	  total_sets = XVECLEN (newpat, 0) + extra_sets;
	  newpat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total_sets));
	  memcpy (XVEC (newpat, 0)->elem, &old->elem[0],
		  sizeof (old->elem[0]) * old->num_elem);
	}
      else
	{
	  rtx old = newpat;
	  total_sets = 1 + extra_sets;
	  newpat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (total_sets));
	  XVECEXP (newpat, 0, 0) = old;
	}

      if (added_sets_0)
	XVECEXP (newpat, 0, --total_sets) = i0pat;

      if (added_sets_1)
	{
	  rtx t = i1pat;
	  if (i0_feeds_i1_n)
	    t = subst (t, i0dest, i0src_copy ? i0src_copy : i0src,
		       false, false, false);

	  XVECEXP (newpat, 0, --total_sets) = t;
	}
      if (added_sets_2)
	{
	  rtx t = i2pat;
	  if (i1_feeds_i2_n)
	    t = subst (t, i1dest, i1src_copy ? i1src_copy : i1src, false, false,
		       i0_feeds_i1_n && i0dest_in_i0src);
	  if ((i0_feeds_i1_n && i1_feeds_i2_n) || i0_feeds_i2_n)
	    t = subst (t, i0dest, i0src_copy2 ? i0src_copy2 : i0src,
		       false, false, false);

	  XVECEXP (newpat, 0, --total_sets) = t;
	}
    }

 validate_replacement:

  /* Note which hard regs this insn has as inputs.  */
  mark_used_regs_combine (newpat);

  /* If recog_for_combine fails, it strips existing clobbers.  If we'll
     consider splitting this pattern, we might need these clobbers.  */
  if (i1 && GET_CODE (newpat) == PARALLEL
      && GET_CODE (XVECEXP (newpat, 0, XVECLEN (newpat, 0) - 1)) == CLOBBER)
    {
      int len = XVECLEN (newpat, 0);

      newpat_vec_with_clobbers = rtvec_alloc (len);
      for (i = 0; i < len; i++)
	RTVEC_ELT (newpat_vec_with_clobbers, i) = XVECEXP (newpat, 0, i);
    }

  /* We have recognized nothing yet.  */
  insn_code_number = -1;

  /* See if this is a PARALLEL of two SETs where one SET's destination is
     a register that is unused and this isn't marked as an instruction that
     might trap in an EH region.  In that case, we just need the other SET.
     We prefer this over the PARALLEL.

     This can occur when simplifying a divmod insn.  We *must* test for this
     case here because the code below that splits two independent SETs doesn't
     handle this case correctly when it updates the register status.

     It's pointless doing this if we originally had two sets, one from
     i3, and one from i2.  Combining then splitting the parallel results
     in the original i2 again plus an invalid insn (which we delete).
     The net effect is only to move instructions around, which makes
     debug info less accurate.

     If the remaining SET came from I2 its destination should not be used
     between I2 and I3.  See PR82024.  */

  if (!(added_sets_2 && i1 == 0)
      && is_parallel_of_n_reg_sets (newpat, 2)
      && asm_noperands (newpat) < 0)
    {
      rtx set0 = XVECEXP (newpat, 0, 0);
      rtx set1 = XVECEXP (newpat, 0, 1);
      rtx oldpat = newpat;

      if (((REG_P (SET_DEST (set1))
	    && find_reg_note (i3, REG_UNUSED, SET_DEST (set1)))
	   || (GET_CODE (SET_DEST (set1)) == SUBREG
	       && find_reg_note (i3, REG_UNUSED, SUBREG_REG (SET_DEST (set1)))))
	  && insn_nothrow_p (i3)
	  && !side_effects_p (SET_SRC (set1)))
	{
	  newpat = set0;
	  insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
	}

      else if (((REG_P (SET_DEST (set0))
		 && find_reg_note (i3, REG_UNUSED, SET_DEST (set0)))
		|| (GET_CODE (SET_DEST (set0)) == SUBREG
		    && find_reg_note (i3, REG_UNUSED,
				      SUBREG_REG (SET_DEST (set0)))))
	       && insn_nothrow_p (i3)
	       && !side_effects_p (SET_SRC (set0)))
	{
	  rtx dest = SET_DEST (set1);
	  if (GET_CODE (dest) == SUBREG)
	    dest = SUBREG_REG (dest);
	  if (!reg_used_between_p (dest, i2, i3))
	    {
	      newpat = set1;
	      insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

	      if (insn_code_number >= 0)
		changed_i3_dest = true;
	    }
	}

      if (insn_code_number < 0)
	newpat = oldpat;
    }

  /* Is the result of combination a valid instruction?  */
  if (insn_code_number < 0)
    insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

  /* If we were combining three insns and the result is a simple SET
     with no ASM_OPERANDS that wasn't recognized, try to split it into two
     insns.  There are two ways to do this.  It can be split using a
     machine-specific method (like when you have an addition of a large
     constant) or by combine in the function find_split_point.  */

  if (i1 && insn_code_number < 0 && GET_CODE (newpat) == SET
      && asm_noperands (newpat) < 0)
    {
      rtx parallel, *split;
      rtx_insn *m_split_insn;

      /* See if the MD file can split NEWPAT.  If it can't, see if letting it
	 use I2DEST as a scratch register will help.  In the latter case,
	 convert I2DEST to the mode of the source of NEWPAT if we can.  */

      m_split_insn = combine_split_insns (newpat, i3);

      /* We can only use I2DEST as a scratch reg if it doesn't overlap any
	 inputs of NEWPAT.  */

      /* ??? If I2DEST is not safe, and I1DEST exists, then it would be
	 possible to try that as a scratch reg.  This would require adding
	 more code to make it work though.  */

      if (m_split_insn == 0 && ! reg_overlap_mentioned_p (i2dest, newpat))
	{
	  machine_mode new_mode = GET_MODE (SET_DEST (newpat));

	  /* ??? Reusing i2dest without resetting the reg_stat entry for it
	     (temporarily, until we are committed to this instruction
	     combination) does not work: for example, any call to nonzero_bits
	     on the register (from a splitter in the MD file, for example)
	     will get the old information, which is invalid.

	     Since nowadays we can create registers during combine just fine,
	     we should just create a new one here, not reuse i2dest.  */

	  /* First try to split using the original register as a
	     scratch register.  */
	  parallel = gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (2, newpat,
						  gen_rtx_CLOBBER (VOIDmode,
								   i2dest)));
	  m_split_insn = combine_split_insns (parallel, i3);

	  /* If that didn't work, try changing the mode of I2DEST if
	     we can.  */
	  if (m_split_insn == 0
	      && new_mode != GET_MODE (i2dest)
	      && new_mode != VOIDmode
	      && can_change_dest_mode (i2dest, added_sets_2, new_mode))
	    {
	      machine_mode old_mode = GET_MODE (i2dest);
	      rtx ni2dest;

	      if (REGNO (i2dest) < FIRST_PSEUDO_REGISTER)
		ni2dest = gen_rtx_REG (new_mode, REGNO (i2dest));
	      else
		{
		  subst_mode (REGNO (i2dest), new_mode);
		  ni2dest = regno_reg_rtx[REGNO (i2dest)];
		}

	      parallel = (gen_rtx_PARALLEL
			  (VOIDmode,
			   gen_rtvec (2, newpat,
				      gen_rtx_CLOBBER (VOIDmode,
						       ni2dest))));
	      m_split_insn = combine_split_insns (parallel, i3);

	      if (m_split_insn == 0
		  && REGNO (i2dest) >= FIRST_PSEUDO_REGISTER)
		{
		  struct undo *buf;

		  adjust_reg_mode (regno_reg_rtx[REGNO (i2dest)], old_mode);
		  buf = undobuf.undos;
		  undobuf.undos = buf->next;
		  buf->next = undobuf.frees;
		  undobuf.frees = buf;
		}
	    }

	  i2scratch = m_split_insn != 0;
	}

      /* If recog_for_combine has discarded clobbers, try to use them
	 again for the split.  */
      if (m_split_insn == 0 && newpat_vec_with_clobbers)
	{
	  parallel = gen_rtx_PARALLEL (VOIDmode, newpat_vec_with_clobbers);
	  m_split_insn = combine_split_insns (parallel, i3);
	}

      if (m_split_insn && NEXT_INSN (m_split_insn) == NULL_RTX)
	{
	  rtx m_split_pat = PATTERN (m_split_insn);
	  insn_code_number = recog_for_combine (&m_split_pat, i3, &new_i3_notes);
	  if (insn_code_number >= 0)
	    newpat = m_split_pat;
	}
      else if (m_split_insn && NEXT_INSN (NEXT_INSN (m_split_insn)) == NULL_RTX
	       && (next_nonnote_nondebug_insn (i2) == i3
		   || !modified_between_p (PATTERN (m_split_insn), i2, i3)))
	{
	  rtx i2set, i3set;
	  rtx newi3pat = PATTERN (NEXT_INSN (m_split_insn));
	  newi2pat = PATTERN (m_split_insn);

	  i3set = single_set (NEXT_INSN (m_split_insn));
	  i2set = single_set (m_split_insn);

	  i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);

	  /* If I2 or I3 has multiple SETs, we won't know how to track
	     register status, so don't use these insns.  If I2's destination
	     is used between I2 and I3, we also can't use these insns.  */

	  if (i2_code_number >= 0 && i2set && i3set
	      && (next_nonnote_nondebug_insn (i2) == i3
		  || ! reg_used_between_p (SET_DEST (i2set), i2, i3)))
	    insn_code_number = recog_for_combine (&newi3pat, i3,
						  &new_i3_notes);
	  if (insn_code_number >= 0)
	    newpat = newi3pat;

	  /* It is possible that both insns now set the destination of I3.
	     If so, we must show an extra use of it.  */

	  if (insn_code_number >= 0)
	    {
	      rtx new_i3_dest = SET_DEST (i3set);
	      rtx new_i2_dest = SET_DEST (i2set);

	      while (GET_CODE (new_i3_dest) == ZERO_EXTRACT
		     || GET_CODE (new_i3_dest) == STRICT_LOW_PART
		     || GET_CODE (new_i3_dest) == SUBREG)
		new_i3_dest = XEXP (new_i3_dest, 0);

	      while (GET_CODE (new_i2_dest) == ZERO_EXTRACT
		     || GET_CODE (new_i2_dest) == STRICT_LOW_PART
		     || GET_CODE (new_i2_dest) == SUBREG)
		new_i2_dest = XEXP (new_i2_dest, 0);

	      if (REG_P (new_i3_dest)
		  && REG_P (new_i2_dest)
		  && REGNO (new_i3_dest) == REGNO (new_i2_dest)
		  && REGNO (new_i2_dest) < reg_n_sets_max)
		INC_REG_N_SETS (REGNO (new_i2_dest), 1);
	    }
	}

      /* If we can split it and use I2DEST, go ahead and see if that
	 helps things be recognized.  Verify that none of the registers
	 are set between I2 and I3.  */
      if (insn_code_number < 0
          && (split = find_split_point (&newpat, i3, false)) != 0
	  /* We need I2DEST in the proper mode.  If it is a hard register
	     or the only use of a pseudo, we can change its mode.
	     Make sure we don't change a hard register to have a mode that
	     isn't valid for it, or change the number of registers.  */
	  && (GET_MODE (*split) == GET_MODE (i2dest)
	      || GET_MODE (*split) == VOIDmode
	      || can_change_dest_mode (i2dest, added_sets_2,
				       GET_MODE (*split)))
	  && (next_nonnote_nondebug_insn (i2) == i3
	      || !modified_between_p (*split, i2, i3))
	  /* We can't overwrite I2DEST if its value is still used by
	     NEWPAT.  */
	  && ! reg_referenced_p (i2dest, newpat)
	  /* We should not split a possibly trapping part when we
	     care about non-call EH and have REG_EH_REGION notes
	     to distribute.  */
	  && ! (cfun->can_throw_non_call_exceptions
		&& has_non_call_exception
		&& may_trap_p (*split)))
	{
	  rtx newdest = i2dest;
	  enum rtx_code split_code = GET_CODE (*split);
	  machine_mode split_mode = GET_MODE (*split);
	  bool subst_done = false;
	  newi2pat = NULL_RTX;

	  i2scratch = true;

	  /* *SPLIT may be part of I2SRC, so make sure we have the
	     original expression around for later debug processing.
	     We should not need I2SRC any more in other cases.  */
	  if (MAY_HAVE_DEBUG_BIND_INSNS)
	    i2src = copy_rtx (i2src);
	  else
	    i2src = NULL;

	  /* Get NEWDEST as a register in the proper mode.  We have already
	     validated that we can do this.  */
	  if (GET_MODE (i2dest) != split_mode && split_mode != VOIDmode)
	    {
	      if (REGNO (i2dest) < FIRST_PSEUDO_REGISTER)
		newdest = gen_rtx_REG (split_mode, REGNO (i2dest));
	      else
		{
		  subst_mode (REGNO (i2dest), split_mode);
		  newdest = regno_reg_rtx[REGNO (i2dest)];
		}
	    }

	  /* If *SPLIT is a (mult FOO (const_int pow2)), convert it to
	     an ASHIFT.  This can occur if it was inside a PLUS and hence
	     appeared to be a memory address.  This is a kludge.  */
	  if (split_code == MULT
	      && CONST_INT_P (XEXP (*split, 1))
	      && INTVAL (XEXP (*split, 1)) > 0
	      && (i = exact_log2 (UINTVAL (XEXP (*split, 1)))) >= 0)
	    {
	      rtx i_rtx = gen_int_shift_amount (split_mode, i);
	      SUBST (*split, gen_rtx_ASHIFT (split_mode,
					     XEXP (*split, 0), i_rtx));
	      /* Update split_code because we may not have a multiply
		 anymore.  */
	      split_code = GET_CODE (*split);
	    }

	  /* Similarly for (plus (mult FOO (const_int pow2))).  */
	  if (split_code == PLUS
	      && GET_CODE (XEXP (*split, 0)) == MULT
	      && CONST_INT_P (XEXP (XEXP (*split, 0), 1))
	      && INTVAL (XEXP (XEXP (*split, 0), 1)) > 0
	      && (i = exact_log2 (UINTVAL (XEXP (XEXP (*split, 0), 1)))) >= 0)
	    {
	      rtx nsplit = XEXP (*split, 0);
	      rtx i_rtx = gen_int_shift_amount (GET_MODE (nsplit), i);
	      SUBST (XEXP (*split, 0), gen_rtx_ASHIFT (GET_MODE (nsplit),
						       XEXP (nsplit, 0),
						       i_rtx));
	      /* Update split_code because we may not have a multiply
		 anymore.  */
	      split_code = GET_CODE (*split);
	    }

#ifdef INSN_SCHEDULING
	  /* If *SPLIT is a paradoxical SUBREG, when we split it, it should
	     be written as a ZERO_EXTEND.  */
	  if (split_code == SUBREG && MEM_P (SUBREG_REG (*split)))
	    {
	      /* Or as a SIGN_EXTEND if LOAD_EXTEND_OP says that that's
		 what it really is.  */
	      if (load_extend_op (GET_MODE (SUBREG_REG (*split)))
		  == SIGN_EXTEND)
		SUBST (*split, gen_rtx_SIGN_EXTEND (split_mode,
						    SUBREG_REG (*split)));
	      else
		SUBST (*split, gen_rtx_ZERO_EXTEND (split_mode,
						    SUBREG_REG (*split)));
	    }
#endif

	  /* Attempt to split binary operators using arithmetic identities.  */
	  if (BINARY_P (SET_SRC (newpat))
	      && split_mode == GET_MODE (SET_SRC (newpat))
	      && ! side_effects_p (SET_SRC (newpat)))
	    {
	      rtx setsrc = SET_SRC (newpat);
	      machine_mode mode = GET_MODE (setsrc);
	      enum rtx_code code = GET_CODE (setsrc);
	      rtx src_op0 = XEXP (setsrc, 0);
	      rtx src_op1 = XEXP (setsrc, 1);

	      /* Split "X = Y op Y" as "Z = Y; X = Z op Z".  */
	      if (rtx_equal_p (src_op0, src_op1))
		{
		  newi2pat = gen_rtx_SET (newdest, src_op0);
		  SUBST (XEXP (setsrc, 0), newdest);
		  SUBST (XEXP (setsrc, 1), newdest);
		  subst_done = true;
		}
	      /* Split "((P op Q) op R) op S" where op is PLUS or MULT.  */
	      else if ((code == PLUS || code == MULT)
		       && GET_CODE (src_op0) == code
		       && GET_CODE (XEXP (src_op0, 0)) == code
		       && (INTEGRAL_MODE_P (mode)
			   || (FLOAT_MODE_P (mode)
			       && flag_unsafe_math_optimizations)))
		{
		  rtx p = XEXP (XEXP (src_op0, 0), 0);
		  rtx q = XEXP (XEXP (src_op0, 0), 1);
		  rtx r = XEXP (src_op0, 1);
		  rtx s = src_op1;

		  /* Split both "((X op Y) op X) op Y" and
		     "((X op Y) op Y) op X" as "T op T" where T is
		     "X op Y".  */
		  if ((rtx_equal_p (p,r) && rtx_equal_p (q,s))
		       || (rtx_equal_p (p,s) && rtx_equal_p (q,r)))
		    {
		      newi2pat = gen_rtx_SET (newdest, XEXP (src_op0, 0));
		      SUBST (XEXP (setsrc, 0), newdest);
		      SUBST (XEXP (setsrc, 1), newdest);
		      subst_done = true;
		    }
		  /* Split "((X op X) op Y) op Y)" as "T op T" where
		     T is "X op Y".  */
		  else if (rtx_equal_p (p,q) && rtx_equal_p (r,s))
		    {
		      rtx tmp = simplify_gen_binary (code, mode, p, r);
		      newi2pat = gen_rtx_SET (newdest, tmp);
		      SUBST (XEXP (setsrc, 0), newdest);
		      SUBST (XEXP (setsrc, 1), newdest);
		      subst_done = true;
		    }
		}
	    }

	  if (!subst_done)
	    {
	      newi2pat = gen_rtx_SET (newdest, *split);
	      SUBST (*split, newdest);
	    }

	  i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);

	  /* recog_for_combine might have added CLOBBERs to newi2pat.
	     Make sure NEWPAT does not depend on the clobbered regs.  */
	  if (GET_CODE (newi2pat) == PARALLEL)
	    for (i = XVECLEN (newi2pat, 0) - 1; i >= 0; i--)
	      if (GET_CODE (XVECEXP (newi2pat, 0, i)) == CLOBBER)
		{
		  rtx reg = XEXP (XVECEXP (newi2pat, 0, i), 0);
		  if (reg_overlap_mentioned_p (reg, newpat))
		    {
		      undo_all ();
		      return 0;
		    }
		}

	  /* If the split point was a MULT and we didn't have one before,
	     don't use one now.  */
	  if (i2_code_number >= 0 && ! (split_code == MULT && ! have_mult))
	    insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
	}
    }

  /* Check for a case where we loaded from memory in a narrow mode and
     then sign extended it, but we need both registers.  In that case,
     we have a PARALLEL with both loads from the same memory location.
     We can split this into a load from memory followed by a register-register
     copy.  This saves at least one insn, more if register allocation can
     eliminate the copy.

     We cannot do this if the destination of the first assignment is a
     condition code register.  We eliminate this case by making sure
     the SET_DEST and SET_SRC have the same mode.

     We cannot do this if the destination of the second assignment is
     a register that we have already assumed is zero-extended.  Similarly
     for a SUBREG of such a register.  */

  else if (i1 && insn_code_number < 0 && asm_noperands (newpat) < 0
	   && GET_CODE (newpat) == PARALLEL
	   && XVECLEN (newpat, 0) == 2
	   && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
	   && GET_CODE (SET_SRC (XVECEXP (newpat, 0, 0))) == SIGN_EXTEND
	   && (GET_MODE (SET_DEST (XVECEXP (newpat, 0, 0)))
	       == GET_MODE (SET_SRC (XVECEXP (newpat, 0, 0))))
	   && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
	   && rtx_equal_p (SET_SRC (XVECEXP (newpat, 0, 1)),
			   XEXP (SET_SRC (XVECEXP (newpat, 0, 0)), 0))
	   && !modified_between_p (SET_SRC (XVECEXP (newpat, 0, 1)), i2, i3)
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != STRICT_LOW_PART
	   && ! (temp_expr = SET_DEST (XVECEXP (newpat, 0, 1)),
		 (REG_P (temp_expr)
		  && reg_stat[REGNO (temp_expr)].nonzero_bits != 0
		  && known_lt (GET_MODE_PRECISION (GET_MODE (temp_expr)),
			       BITS_PER_WORD)
		  && known_lt (GET_MODE_PRECISION (GET_MODE (temp_expr)),
			       HOST_BITS_PER_INT)
		  && (reg_stat[REGNO (temp_expr)].nonzero_bits
		      != GET_MODE_MASK (word_mode))))
	   && ! (GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) == SUBREG
		 && (temp_expr = SUBREG_REG (SET_DEST (XVECEXP (newpat, 0, 1))),
		     (REG_P (temp_expr)
		      && reg_stat[REGNO (temp_expr)].nonzero_bits != 0
		      && known_lt (GET_MODE_PRECISION (GET_MODE (temp_expr)),
				   BITS_PER_WORD)
		      && known_lt (GET_MODE_PRECISION (GET_MODE (temp_expr)),
				   HOST_BITS_PER_INT)
		      && (reg_stat[REGNO (temp_expr)].nonzero_bits
			  != GET_MODE_MASK (word_mode)))))
	   && ! reg_overlap_mentioned_p (SET_DEST (XVECEXP (newpat, 0, 1)),
					 SET_SRC (XVECEXP (newpat, 0, 1)))
	   && ! find_reg_note (i3, REG_UNUSED,
			       SET_DEST (XVECEXP (newpat, 0, 0))))
    {
      rtx ni2dest;

      newi2pat = XVECEXP (newpat, 0, 0);
      ni2dest = SET_DEST (XVECEXP (newpat, 0, 0));
      newpat = XVECEXP (newpat, 0, 1);
      SUBST (SET_SRC (newpat),
	     gen_lowpart (GET_MODE (SET_SRC (newpat)), ni2dest));
      i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);

      if (i2_code_number >= 0)
	insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

      if (insn_code_number >= 0)
	swap_i2i3 = 1;
    }

  /* Similarly, check for a case where we have a PARALLEL of two independent
     SETs but we started with three insns.  In this case, we can do the sets
     as two separate insns.  This case occurs when some SET allows two
     other insns to combine, but the destination of that SET is still live.

     Also do this if we started with two insns and (at least) one of the
     resulting sets is a noop; this noop will be deleted later.

     Also do this if we started with two insns neither of which was a simple
     move.  */

  else if (insn_code_number < 0 && asm_noperands (newpat) < 0
	   && GET_CODE (newpat) == PARALLEL
	   && XVECLEN (newpat, 0) == 2
	   && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
	   && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
	   && (i1
	       || set_noop_p (XVECEXP (newpat, 0, 0))
	       || set_noop_p (XVECEXP (newpat, 0, 1))
	       || (!i2_was_move && !i3_was_move))
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) != STRICT_LOW_PART
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != STRICT_LOW_PART
	   && ! reg_referenced_p (SET_DEST (XVECEXP (newpat, 0, 1)),
				  XVECEXP (newpat, 0, 0))
	   && ! reg_referenced_p (SET_DEST (XVECEXP (newpat, 0, 0)),
				  XVECEXP (newpat, 0, 1))
	   && ! (contains_muldiv (SET_SRC (XVECEXP (newpat, 0, 0)))
		 && contains_muldiv (SET_SRC (XVECEXP (newpat, 0, 1)))))
    {
      rtx set0 = XVECEXP (newpat, 0, 0);
      rtx set1 = XVECEXP (newpat, 0, 1);

      /* Normally, it doesn't matter which of the two is done first, but
	 one which uses any regs/memory set in between i2 and i3 can't
	 be first.  The PARALLEL might also have been pre-existing in i3,
	 so we need to make sure that we won't wrongly hoist a SET to i2
	 that would conflict with a death note present in there, or would
	 have its dest modified between i2 and i3.  */
      if (!modified_between_p (SET_SRC (set1), i2, i3)
	  && !(REG_P (SET_DEST (set1))
	       && find_reg_note (i2, REG_DEAD, SET_DEST (set1)))
	  && !(GET_CODE (SET_DEST (set1)) == SUBREG
	       && find_reg_note (i2, REG_DEAD,
				 SUBREG_REG (SET_DEST (set1))))
	  && !modified_between_p (SET_DEST (set1), i2, i3)
	  /* If I3 is a jump, ensure that set0 is a jump so that
	     we do not create invalid RTL.  */
	  && (!JUMP_P (i3) || SET_DEST (set0) == pc_rtx)
	 )
	{
	  newi2pat = set1;
	  newpat = set0;
	}
      else if (!modified_between_p (SET_SRC (set0), i2, i3)
	       && !(REG_P (SET_DEST (set0))
		    && find_reg_note (i2, REG_DEAD, SET_DEST (set0)))
	       && !(GET_CODE (SET_DEST (set0)) == SUBREG
		    && find_reg_note (i2, REG_DEAD,
				      SUBREG_REG (SET_DEST (set0))))
	       && !modified_between_p (SET_DEST (set0), i2, i3)
	       /* If I3 is a jump, ensure that set1 is a jump so that
		  we do not create invalid RTL.  */
	       && (!JUMP_P (i3) || SET_DEST (set1) == pc_rtx)
	      )
	{
	  newi2pat = set0;
	  newpat = set1;
	}
      else
	{
	  undo_all ();
	  return 0;
	}

      i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);

      if (i2_code_number >= 0)
	{
	  /* recog_for_combine might have added CLOBBERs to newi2pat.
	     Make sure NEWPAT does not depend on the clobbered regs.  */
	  if (GET_CODE (newi2pat) == PARALLEL)
	    {
	      for (i = XVECLEN (newi2pat, 0) - 1; i >= 0; i--)
		if (GET_CODE (XVECEXP (newi2pat, 0, i)) == CLOBBER)
		  {
		    rtx reg = XEXP (XVECEXP (newi2pat, 0, i), 0);
		    if (reg_overlap_mentioned_p (reg, newpat))
		      {
			undo_all ();
			return 0;
		      }
		  }
	    }

	  insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

	  /* Likewise, recog_for_combine might have added clobbers to NEWPAT.
	     Checking that the SET0's SET_DEST and SET1's SET_DEST aren't
	     mentioned/clobbered, ensures NEWI2PAT's SET_DEST is live.  */
	  if (insn_code_number >= 0 && GET_CODE (newpat) == PARALLEL)
	    {
	      for (i = XVECLEN (newpat, 0) - 1; i >= 0; i--)
		if (GET_CODE (XVECEXP (newpat, 0, i)) == CLOBBER)
		  {
		    rtx reg = XEXP (XVECEXP (newpat, 0, i), 0);
		    if (reg_overlap_mentioned_p (reg, SET_DEST (set0))
			|| reg_overlap_mentioned_p (reg, SET_DEST (set1)))
		      {
			undo_all ();
			return 0;
		      }
		  }
	    }

	  if (insn_code_number >= 0)
	    split_i2i3 = true;
	}
    }

  /* If it still isn't recognized, fail and change things back the way they
     were.  */
  if ((insn_code_number < 0
       /* Is the result a reasonable ASM_OPERANDS?  */
       && (! check_asm_operands (newpat) || added_sets_1 || added_sets_2)))
    {
      undo_all ();
      return 0;
    }

  /* If we had to change another insn, make sure it is valid also.  */
  if (undobuf.other_insn)
    {
      CLEAR_HARD_REG_SET (newpat_used_regs);

      other_pat = PATTERN (undobuf.other_insn);
      other_code_number = recog_for_combine (&other_pat, undobuf.other_insn,
					     &new_other_notes);

      if (other_code_number < 0 && ! check_asm_operands (other_pat))
	{
	  undo_all ();
	  return 0;
	}
    }

  /* Only allow this combination if insn_cost reports that the
     replacement instructions are cheaper than the originals.  */
  if (!combine_validate_cost (i0, i1, i2, i3, newpat, newi2pat, other_pat))
    {
      undo_all ();
      return 0;
    }

  if (MAY_HAVE_DEBUG_BIND_INSNS)
    {
      struct undo *undo;

      for (undo = undobuf.undos; undo; undo = undo->next)
	if (undo->kind == UNDO_MODE)
	  {
	    rtx reg = regno_reg_rtx[undo->where.regno];
	    machine_mode new_mode = GET_MODE (reg);
	    machine_mode old_mode = undo->old_contents.m;

	    /* Temporarily revert mode back.  */
	    adjust_reg_mode (reg, old_mode);

	    if (reg == i2dest && i2scratch)
	      {
		/* If we used i2dest as a scratch register with a
		   different mode, substitute it for the original
		   i2src while its original mode is temporarily
		   restored, and then clear i2scratch so that we don't
		   do it again later.  */
		propagate_for_debug (i2, last_combined_insn, reg, i2src,
				     this_basic_block);
		i2scratch = false;
		/* Put back the new mode.  */
		adjust_reg_mode (reg, new_mode);
	      }
	    else
	      {
		rtx tempreg = gen_raw_REG (old_mode, REGNO (reg));
		rtx_insn *first, *last;

		if (reg == i2dest)
		  {
		    first = i2;
		    last = last_combined_insn;
		  }
		else
		  {
		    first = i3;
		    last = undobuf.other_insn;
		    gcc_assert (last);
		    if (DF_INSN_LUID (last)
			< DF_INSN_LUID (last_combined_insn))
		      last = last_combined_insn;
		  }

		/* We're dealing with a reg that changed mode but not
		   meaning, so we want to turn it into a subreg for
		   the new mode.  However, because of REG sharing and
		   because its mode had already changed, we have to do
		   it in two steps.  First, replace any debug uses of
		   reg, with its original mode temporarily restored,
		   with this copy we have created; then, replace the
		   copy with the SUBREG of the original shared reg,
		   once again changed to the new mode.  */
		propagate_for_debug (first, last, reg, tempreg,
				     this_basic_block);
		adjust_reg_mode (reg, new_mode);
		propagate_for_debug (first, last, tempreg,
				     lowpart_subreg (old_mode, reg, new_mode),
				     this_basic_block);
	      }
	  }
    }

  /* If we will be able to accept this, we have made a
     change to the destination of I3.  This requires us to
     do a few adjustments.  */

  if (changed_i3_dest)
    {
      PATTERN (i3) = newpat;
      adjust_for_new_dest (i3);
    }

  /* We now know that we can do this combination.  Merge the insns and
     update the status of registers and LOG_LINKS.  */

  if (undobuf.other_insn)
    {
      rtx note, next;

      PATTERN (undobuf.other_insn) = other_pat;

      /* If any of the notes in OTHER_INSN were REG_DEAD or REG_UNUSED,
	 ensure that they are still valid.  Then add any non-duplicate
	 notes added by recog_for_combine.  */
      for (note = REG_NOTES (undobuf.other_insn); note; note = next)
	{
	  next = XEXP (note, 1);

	  if ((REG_NOTE_KIND (note) == REG_DEAD
	       && !reg_referenced_p (XEXP (note, 0),
				     PATTERN (undobuf.other_insn)))
	      ||(REG_NOTE_KIND (note) == REG_UNUSED
		 && !reg_set_p (XEXP (note, 0),
				PATTERN (undobuf.other_insn)))
	      /* Simply drop equal note since it may be no longer valid
		 for other_insn.  It may be possible to record that CC
		 register is changed and only discard those notes, but
		 in practice it's unnecessary complication and doesn't
		 give any meaningful improvement.

		 See PR78559.  */
	      || REG_NOTE_KIND (note) == REG_EQUAL
	      || REG_NOTE_KIND (note) == REG_EQUIV)
	    remove_note (undobuf.other_insn, note);
	}

      distribute_notes  (new_other_notes, undobuf.other_insn,
			undobuf.other_insn, NULL, NULL_RTX, NULL_RTX,
			NULL_RTX);
    }

  if (swap_i2i3)
    {
      /* I3 now uses what used to be its destination and which is now
	 I2's destination.  This requires us to do a few adjustments.  */
      PATTERN (i3) = newpat;
      adjust_for_new_dest (i3);
    }

  if (swap_i2i3 || split_i2i3)
    {
      /* We might need a LOG_LINK from I3 to I2.  But then we used to
	 have one, so we still will.

	 However, some later insn might be using I2's dest and have
	 a LOG_LINK pointing at I3.  We should change it to point at
	 I2 instead.  */

      /* newi2pat is usually a SET here; however, recog_for_combine might
	 have added some clobbers.  */
      rtx x = newi2pat;
      if (GET_CODE (x) == PARALLEL)
	x = XVECEXP (newi2pat, 0, 0);

      if (REG_P (SET_DEST (x))
	  || (GET_CODE (SET_DEST (x)) == SUBREG
	      && REG_P (SUBREG_REG (SET_DEST (x)))))
	{
	  unsigned int regno = reg_or_subregno (SET_DEST (x));

	  bool done = false;
	  for (rtx_insn *insn = NEXT_INSN (i3);
	       !done
	       && insn
	       && INSN_P (insn)
	       && BLOCK_FOR_INSN (insn) == this_basic_block;
	       insn = NEXT_INSN (insn))
	    {
	      if (DEBUG_INSN_P (insn))
		continue;
	      struct insn_link *link;
	      FOR_EACH_LOG_LINK (link, insn)
		if (link->insn == i3 && link->regno == regno)
		  {
		    link->insn = i2;
		    done = true;
		    break;
		  }
	    }
	}
    }

  {
    rtx i3notes, i2notes, i1notes = 0, i0notes = 0;
    struct insn_link *i3links, *i2links, *i1links = 0, *i0links = 0;
    rtx midnotes = 0;
    int from_luid;
    /* Compute which registers we expect to eliminate.  newi2pat may be setting
       either i3dest or i2dest, so we must check it.  */
    rtx elim_i2 = ((newi2pat && reg_set_p (i2dest, newi2pat))
		   || i2dest_in_i2src || i2dest_in_i1src || i2dest_in_i0src
		   || !i2dest_killed
		   ? 0 : i2dest);
    /* For i1, we need to compute both local elimination and global
       elimination information with respect to newi2pat because i1dest
       may be the same as i3dest, in which case newi2pat may be setting
       i1dest.  Global information is used when distributing REG_DEAD
       note for i2 and i3, in which case it does matter if newi2pat sets
       i1dest or not.

       Local information is used when distributing REG_DEAD note for i1,
       in which case it doesn't matter if newi2pat sets i1dest or not.
       See PR62151, if we have four insns combination:
	   i0: r0 <- i0src
	   i1: r1 <- i1src (using r0)
		     REG_DEAD (r0)
	   i2: r0 <- i2src (using r1)
	   i3: r3 <- i3src (using r0)
	   ix: using r0
       From i1's point of view, r0 is eliminated, no matter if it is set
       by newi2pat or not.  In other words, REG_DEAD info for r0 in i1
       should be discarded.

       Note local information only affects cases in forms like "I1->I2->I3",
       "I0->I1->I2->I3" or "I0&I1->I2, I2->I3".  For other cases like
       "I0->I1, I1&I2->I3" or "I1&I2->I3", newi2pat won't set i1dest or
       i0dest anyway.  */
    rtx local_elim_i1 = (i1 == 0 || i1dest_in_i1src || i1dest_in_i0src
			 || !i1dest_killed
			 ? 0 : i1dest);
    rtx elim_i1 = (local_elim_i1 == 0
		   || (newi2pat && reg_set_p (i1dest, newi2pat))
		   ? 0 : i1dest);
    /* Same case as i1.  */
    rtx local_elim_i0 = (i0 == 0 || i0dest_in_i0src || !i0dest_killed
			 ? 0 : i0dest);
    rtx elim_i0 = (local_elim_i0 == 0
		   || (newi2pat && reg_set_p (i0dest, newi2pat))
		   ? 0 : i0dest);

    /* Get the old REG_NOTES and LOG_LINKS from all our insns and
       clear them.  */
    i3notes = REG_NOTES (i3), i3links = LOG_LINKS (i3);
    i2notes = REG_NOTES (i2), i2links = LOG_LINKS (i2);
    if (i1)
      i1notes = REG_NOTES (i1), i1links = LOG_LINKS (i1);
    if (i0)
      i0notes = REG_NOTES (i0), i0links = LOG_LINKS (i0);

    /* Ensure that we do not have something that should not be shared but
       occurs multiple times in the new insns.  Check this by first
       resetting all the `used' flags and then copying anything is shared.  */

    reset_used_flags (i3notes);
    reset_used_flags (i2notes);
    reset_used_flags (i1notes);
    reset_used_flags (i0notes);
    reset_used_flags (newpat);
    reset_used_flags (newi2pat);
    if (undobuf.other_insn)
      reset_used_flags (PATTERN (undobuf.other_insn));

    i3notes = copy_rtx_if_shared (i3notes);
    i2notes = copy_rtx_if_shared (i2notes);
    i1notes = copy_rtx_if_shared (i1notes);
    i0notes = copy_rtx_if_shared (i0notes);
    newpat = copy_rtx_if_shared (newpat);
    newi2pat = copy_rtx_if_shared (newi2pat);
    if (undobuf.other_insn)
      reset_used_flags (PATTERN (undobuf.other_insn));

    INSN_CODE (i3) = insn_code_number;
    PATTERN (i3) = newpat;

    if (CALL_P (i3) && CALL_INSN_FUNCTION_USAGE (i3))
      {
	for (rtx link = CALL_INSN_FUNCTION_USAGE (i3); link;
	     link = XEXP (link, 1))
	  {
	    if (substed_i2)
	      {
		/* I2SRC must still be meaningful at this point.  Some
		   splitting operations can invalidate I2SRC, but those
		   operations do not apply to calls.  */
		gcc_assert (i2src);
		XEXP (link, 0) = simplify_replace_rtx (XEXP (link, 0),
						       i2dest, i2src);
	      }
	    if (substed_i1)
	      XEXP (link, 0) = simplify_replace_rtx (XEXP (link, 0),
						     i1dest, i1src);
	    if (substed_i0)
	      XEXP (link, 0) = simplify_replace_rtx (XEXP (link, 0),
						     i0dest, i0src);
	  }
      }

    if (undobuf.other_insn)
      INSN_CODE (undobuf.other_insn) = other_code_number;

    /* We had one special case above where I2 had more than one set and
       we replaced a destination of one of those sets with the destination
       of I3.  In that case, we have to update LOG_LINKS of insns later
       in this basic block.  Note that this (expensive) case is rare.

       Also, in this case, we must pretend that all REG_NOTEs for I2
       actually came from I3, so that REG_UNUSED notes from I2 will be
       properly handled.  */

    if (i3_subst_into_i2)
      {
	for (i = 0; i < XVECLEN (PATTERN (i2), 0); i++)
	  if ((GET_CODE (XVECEXP (PATTERN (i2), 0, i)) == SET
	       || GET_CODE (XVECEXP (PATTERN (i2), 0, i)) == CLOBBER)
	      && REG_P (SET_DEST (XVECEXP (PATTERN (i2), 0, i)))
	      && SET_DEST (XVECEXP (PATTERN (i2), 0, i)) != i2dest
	      && ! find_reg_note (i2, REG_UNUSED,
				  SET_DEST (XVECEXP (PATTERN (i2), 0, i))))
	    for (temp_insn = NEXT_INSN (i2);
		 temp_insn
		 && (this_basic_block->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
		     || BB_HEAD (this_basic_block) != temp_insn);
		 temp_insn = NEXT_INSN (temp_insn))
	      if (temp_insn != i3 && NONDEBUG_INSN_P (temp_insn))
		FOR_EACH_LOG_LINK (link, temp_insn)
		  if (link->insn == i2)
		    link->insn = i3;

	if (i3notes)
	  {
	    rtx link = i3notes;
	    while (XEXP (link, 1))
	      link = XEXP (link, 1);
	    XEXP (link, 1) = i2notes;
	  }
	else
	  i3notes = i2notes;
	i2notes = 0;
      }

    LOG_LINKS (i3) = NULL;
    REG_NOTES (i3) = 0;
    LOG_LINKS (i2) = NULL;
    REG_NOTES (i2) = 0;

    if (newi2pat)
      {
	if (MAY_HAVE_DEBUG_BIND_INSNS && i2scratch)
	  propagate_for_debug (i2, last_combined_insn, i2dest, i2src,
			       this_basic_block);
	INSN_CODE (i2) = i2_code_number;
	PATTERN (i2) = newi2pat;
      }
    else
      {
	if (MAY_HAVE_DEBUG_BIND_INSNS && i2src)
	  propagate_for_debug (i2, last_combined_insn, i2dest, i2src,
			       this_basic_block);
	SET_INSN_DELETED (i2);
      }

    if (i1)
      {
	LOG_LINKS (i1) = NULL;
	REG_NOTES (i1) = 0;
	if (MAY_HAVE_DEBUG_BIND_INSNS)
	  propagate_for_debug (i1, last_combined_insn, i1dest, i1src,
			       this_basic_block);
	SET_INSN_DELETED (i1);
      }

    if (i0)
      {
	LOG_LINKS (i0) = NULL;
	REG_NOTES (i0) = 0;
	if (MAY_HAVE_DEBUG_BIND_INSNS)
	  propagate_for_debug (i0, last_combined_insn, i0dest, i0src,
			       this_basic_block);
	SET_INSN_DELETED (i0);
      }

    /* Get death notes for everything that is now used in either I3 or
       I2 and used to die in a previous insn.  If we built two new
       patterns, move from I1 to I2 then I2 to I3 so that we get the
       proper movement on registers that I2 modifies.  */

    if (i0)
      from_luid = DF_INSN_LUID (i0);
    else if (i1)
      from_luid = DF_INSN_LUID (i1);
    else
      from_luid = DF_INSN_LUID (i2);
    if (newi2pat)
      move_deaths (newi2pat, NULL_RTX, from_luid, i2, &midnotes);
    move_deaths (newpat, newi2pat, from_luid, i3, &midnotes);

    /* Distribute all the LOG_LINKS and REG_NOTES from I1, I2, and I3.  */
    if (i3notes)
      distribute_notes (i3notes, i3, i3, newi2pat ? i2 : NULL,
			elim_i2, elim_i1, elim_i0);
    if (i2notes)
      distribute_notes (i2notes, i2, i3, newi2pat ? i2 : NULL,
			elim_i2, elim_i1, elim_i0);
    if (i1notes)
      distribute_notes (i1notes, i1, i3, newi2pat ? i2 : NULL,
			elim_i2, local_elim_i1, local_elim_i0);
    if (i0notes)
      distribute_notes (i0notes, i0, i3, newi2pat ? i2 : NULL,
			elim_i2, elim_i1, local_elim_i0);
    if (midnotes)
      distribute_notes (midnotes, NULL, i3, newi2pat ? i2 : NULL,
			elim_i2, elim_i1, elim_i0);

    /* Distribute any notes added to I2 or I3 by recog_for_combine.  We
       know these are REG_UNUSED and want them to go to the desired insn,
       so we always pass it as i3.  */

    if (newi2pat && new_i2_notes)
      distribute_notes (new_i2_notes, i2, i2, NULL, NULL_RTX, NULL_RTX,
			NULL_RTX);

    if (new_i3_notes)
      distribute_notes (new_i3_notes, i3, i3, NULL, NULL_RTX, NULL_RTX,
			NULL_RTX);

    /* If I3DEST was used in I3SRC, it really died in I3.  We may need to
       put a REG_DEAD note for it somewhere.  If NEWI2PAT exists and sets
       I3DEST, the death must be somewhere before I2, not I3.  If we passed I3
       in that case, it might delete I2.  Similarly for I2 and I1.
       Show an additional death due to the REG_DEAD note we make here.  If
       we discard it in distribute_notes, we will decrement it again.  */

    if (i3dest_killed)
      {
	rtx new_note = alloc_reg_note (REG_DEAD, i3dest_killed, NULL_RTX);
	if (newi2pat && reg_set_p (i3dest_killed, newi2pat))
	  distribute_notes (new_note, NULL, i2, NULL, elim_i2,
			    elim_i1, elim_i0);
	else
	  distribute_notes (new_note, NULL, i3, newi2pat ? i2 : NULL,
			    elim_i2, elim_i1, elim_i0);
      }

    if (i2dest_in_i2src)
      {
	rtx new_note = alloc_reg_note (REG_DEAD, i2dest, NULL_RTX);
	if (newi2pat && reg_set_p (i2dest, newi2pat))
	  distribute_notes (new_note,  NULL, i2, NULL, NULL_RTX,
			    NULL_RTX, NULL_RTX);
	else
	  distribute_notes (new_note, NULL, i3, newi2pat ? i2 : NULL,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      }

    if (i1dest_in_i1src)
      {
	rtx new_note = alloc_reg_note (REG_DEAD, i1dest, NULL_RTX);
	if (newi2pat && reg_set_p (i1dest, newi2pat))
	  distribute_notes (new_note, NULL, i2, NULL, NULL_RTX,
			    NULL_RTX, NULL_RTX);
	else
	  distribute_notes (new_note, NULL, i3, newi2pat ? i2 : NULL,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      }

    if (i0dest_in_i0src)
      {
	rtx new_note = alloc_reg_note (REG_DEAD, i0dest, NULL_RTX);
	if (newi2pat && reg_set_p (i0dest, newi2pat))
	  distribute_notes (new_note, NULL, i2, NULL, NULL_RTX,
			    NULL_RTX, NULL_RTX);
	else
	  distribute_notes (new_note, NULL, i3, newi2pat ? i2 : NULL,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      }

    distribute_links (i3links);
    distribute_links (i2links);
    distribute_links (i1links);
    distribute_links (i0links);

    if (REG_P (i2dest))
      {
	struct insn_link *link;
	rtx_insn *i2_insn = 0;
	rtx i2_val = 0, set;

	/* The insn that used to set this register doesn't exist, and
	   this life of the register may not exist either.  See if one of
	   I3's links points to an insn that sets I2DEST.  If it does,
	   that is now the last known value for I2DEST. If we don't update
	   this and I2 set the register to a value that depended on its old
	   contents, we will get confused.  If this insn is used, thing
	   will be set correctly in combine_instructions.  */
	FOR_EACH_LOG_LINK (link, i3)
	  if ((set = single_set (link->insn)) != 0
	      && rtx_equal_p (i2dest, SET_DEST (set)))
	    i2_insn = link->insn, i2_val = SET_SRC (set);

	record_value_for_reg (i2dest, i2_insn, i2_val);

	/* If the reg formerly set in I2 died only once and that was in I3,
	   zero its use count so it won't make `reload' do any work.  */
	if (! added_sets_2
	    && (newi2pat == 0 || ! reg_mentioned_p (i2dest, newi2pat))
	    && ! i2dest_in_i2src
	    && REGNO (i2dest) < reg_n_sets_max)
	  INC_REG_N_SETS (REGNO (i2dest), -1);
      }

    if (i1 && REG_P (i1dest))
      {
	struct insn_link *link;
	rtx_insn *i1_insn = 0;
	rtx i1_val = 0, set;

	FOR_EACH_LOG_LINK (link, i3)
	  if ((set = single_set (link->insn)) != 0
	      && rtx_equal_p (i1dest, SET_DEST (set)))
	    i1_insn = link->insn, i1_val = SET_SRC (set);

	record_value_for_reg (i1dest, i1_insn, i1_val);

	if (! added_sets_1
	    && ! i1dest_in_i1src
	    && REGNO (i1dest) < reg_n_sets_max)
	  INC_REG_N_SETS (REGNO (i1dest), -1);
      }

    if (i0 && REG_P (i0dest))
      {
	struct insn_link *link;
	rtx_insn *i0_insn = 0;
	rtx i0_val = 0, set;

	FOR_EACH_LOG_LINK (link, i3)
	  if ((set = single_set (link->insn)) != 0
	      && rtx_equal_p (i0dest, SET_DEST (set)))
	    i0_insn = link->insn, i0_val = SET_SRC (set);

	record_value_for_reg (i0dest, i0_insn, i0_val);

	if (! added_sets_0
	    && ! i0dest_in_i0src
	    && REGNO (i0dest) < reg_n_sets_max)
	  INC_REG_N_SETS (REGNO (i0dest), -1);
      }

    /* Update reg_stat[].nonzero_bits et al for any changes that may have
       been made to this insn.  The order is important, because newi2pat
       can affect nonzero_bits of newpat.  */
    if (newi2pat)
      note_pattern_stores (newi2pat, set_nonzero_bits_and_sign_copies, NULL);
    note_pattern_stores (newpat, set_nonzero_bits_and_sign_copies, NULL);
  }

  if (undobuf.other_insn != NULL_RTX)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "modifying other_insn ");
	  dump_insn_slim (dump_file, undobuf.other_insn);
	}
      df_insn_rescan (undobuf.other_insn);
    }

  if (i0 && !(NOTE_P (i0) && (NOTE_KIND (i0) == NOTE_INSN_DELETED)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "modifying insn i0 ");
	  dump_insn_slim (dump_file, i0);
	}
      df_insn_rescan (i0);
    }

  if (i1 && !(NOTE_P (i1) && (NOTE_KIND (i1) == NOTE_INSN_DELETED)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "modifying insn i1 ");
	  dump_insn_slim (dump_file, i1);
	}
      df_insn_rescan (i1);
    }

  if (i2 && !(NOTE_P (i2) && (NOTE_KIND (i2) == NOTE_INSN_DELETED)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "modifying insn i2 ");
	  dump_insn_slim (dump_file, i2);
	}
      df_insn_rescan (i2);
    }

  if (i3 && !(NOTE_P (i3) && (NOTE_KIND (i3) == NOTE_INSN_DELETED)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "modifying insn i3 ");
	  dump_insn_slim (dump_file, i3);
	}
      df_insn_rescan (i3);
    }

  /* Set new_direct_jump_p if a new return or simple jump instruction
     has been created.  Adjust the CFG accordingly.  */
  if (returnjump_p (i3) || any_uncondjump_p (i3))
    {
      *new_direct_jump_p = 1;
      mark_jump_label (PATTERN (i3), i3, 0);
      update_cfg_for_uncondjump (i3);
    }

  if (undobuf.other_insn != NULL_RTX
      && (returnjump_p (undobuf.other_insn)
	  || any_uncondjump_p (undobuf.other_insn)))
    {
      *new_direct_jump_p = 1;
      update_cfg_for_uncondjump (undobuf.other_insn);
    }

  if (GET_CODE (PATTERN (i3)) == TRAP_IF
      && XEXP (PATTERN (i3), 0) == const1_rtx)
    {
      basic_block bb = BLOCK_FOR_INSN (i3);
      gcc_assert (bb);
      remove_edge (split_block (bb, i3));
      emit_barrier_after_bb (bb);
      *new_direct_jump_p = 1;
    }

  if (undobuf.other_insn
      && GET_CODE (PATTERN (undobuf.other_insn)) == TRAP_IF
      && XEXP (PATTERN (undobuf.other_insn), 0) == const1_rtx)
    {
      basic_block bb = BLOCK_FOR_INSN (undobuf.other_insn);
      gcc_assert (bb);
      remove_edge (split_block (bb, undobuf.other_insn));
      emit_barrier_after_bb (bb);
      *new_direct_jump_p = 1;
    }

  /* A noop might also need cleaning up of CFG, if it comes from the
     simplification of a jump.  */
  if (JUMP_P (i3)
      && GET_CODE (newpat) == SET
      && SET_SRC (newpat) == pc_rtx
      && SET_DEST (newpat) == pc_rtx)
    {
      *new_direct_jump_p = 1;
      update_cfg_for_uncondjump (i3);
    }

  if (undobuf.other_insn != NULL_RTX
      && JUMP_P (undobuf.other_insn)
      && GET_CODE (PATTERN (undobuf.other_insn)) == SET
      && SET_SRC (PATTERN (undobuf.other_insn)) == pc_rtx
      && SET_DEST (PATTERN (undobuf.other_insn)) == pc_rtx)
    {
      *new_direct_jump_p = 1;
      update_cfg_for_uncondjump (undobuf.other_insn);
    }

  combine_successes++;
  undo_commit ();

  rtx_insn *ret = newi2pat ? i2 : i3;
  if (added_links_insn && DF_INSN_LUID (added_links_insn) < DF_INSN_LUID (ret))
    ret = added_links_insn;
  if (added_notes_insn && DF_INSN_LUID (added_notes_insn) < DF_INSN_LUID (ret))
    ret = added_notes_insn;

  return ret;
}

/* Get a marker for undoing to the current state.  */

static void *
get_undo_marker (void)
{
  return undobuf.undos;
}

/* Undo the modifications up to the marker.  */

static void
undo_to_marker (void *marker)
{
  struct undo *undo, *next;

  for (undo = undobuf.undos; undo != marker; undo = next)
    {
      gcc_assert (undo);

      next = undo->next;
      switch (undo->kind)
	{
	case UNDO_RTX:
	  *undo->where.r = undo->old_contents.r;
	  break;
	case UNDO_INT:
	  *undo->where.i = undo->old_contents.i;
	  break;
	case UNDO_MODE:
	  adjust_reg_mode (regno_reg_rtx[undo->where.regno],
			   undo->old_contents.m);
	  break;
	case UNDO_LINKS:
	  *undo->where.l = undo->old_contents.l;
	  break;
	default:
	  gcc_unreachable ();
	}

      undo->next = undobuf.frees;
      undobuf.frees = undo;
    }

  undobuf.undos = (struct undo *) marker;
}

/* Undo all the modifications recorded in undobuf.  */

static void
undo_all (void)
{
  undo_to_marker (0);
}

/* We've committed to accepting the changes we made.  Move all
   of the undos to the free list.  */

static void
undo_commit (void)
{
  struct undo *undo, *next;

  for (undo = undobuf.undos; undo; undo = next)
    {
      next = undo->next;
      undo->next = undobuf.frees;
      undobuf.frees = undo;
    }
  undobuf.undos = 0;
}

/* Find the innermost point within the rtx at LOC, possibly LOC itself,
   where we have an arithmetic expression and return that point.  LOC will
   be inside INSN.

   try_combine will call this function to see if an insn can be split into
   two insns.  */

static rtx *
find_split_point (rtx *loc, rtx_insn *insn, bool set_src)
{
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  rtx *split;
  unsigned HOST_WIDE_INT len = 0;
  HOST_WIDE_INT pos = 0;
  bool unsignedp = false;
  rtx inner = NULL_RTX;
  scalar_int_mode mode, inner_mode;

  /* First special-case some codes.  */
  switch (code)
    {
    case SUBREG:
#ifdef INSN_SCHEDULING
      /* If we are making a paradoxical SUBREG invalid, it becomes a split
	 point.  */
      if (MEM_P (SUBREG_REG (x)))
	return loc;
#endif
      return find_split_point (&SUBREG_REG (x), insn, false);

    case MEM:
      /* If we have (mem (const ..)) or (mem (symbol_ref ...)), split it
	 using LO_SUM and HIGH.  */
      if (HAVE_lo_sum && (GET_CODE (XEXP (x, 0)) == CONST
			  || GET_CODE (XEXP (x, 0)) == SYMBOL_REF))
	{
	  machine_mode address_mode = get_address_mode (x);

	  SUBST (XEXP (x, 0),
		 gen_rtx_LO_SUM (address_mode,
				 gen_rtx_HIGH (address_mode, XEXP (x, 0)),
				 XEXP (x, 0)));
	  return &XEXP (XEXP (x, 0), 0);
	}

      /* If we have a PLUS whose second operand is a constant and the
	 address is not valid, perhaps we can split it up using
	 the machine-specific way to split large constants.  We use
	 the first pseudo-reg (one of the virtual regs) as a placeholder;
	 it will not remain in the result.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && ! memory_address_addr_space_p (GET_MODE (x), XEXP (x, 0),
					    MEM_ADDR_SPACE (x)))
	{
	  rtx reg = regno_reg_rtx[FIRST_PSEUDO_REGISTER];
	  rtx_insn *seq = combine_split_insns (gen_rtx_SET (reg, XEXP (x, 0)),
					       subst_insn);

	  /* This should have produced two insns, each of which sets our
	     placeholder.  If the source of the second is a valid address,
	     we can put both sources together and make a split point
	     in the middle.  */

	  if (seq
	      && NEXT_INSN (seq) != NULL_RTX
	      && NEXT_INSN (NEXT_INSN (seq)) == NULL_RTX
	      && NONJUMP_INSN_P (seq)
	      && GET_CODE (PATTERN (seq)) == SET
	      && SET_DEST (PATTERN (seq)) == reg
	      && ! reg_mentioned_p (reg,
				    SET_SRC (PATTERN (seq)))
	      && NONJUMP_INSN_P (NEXT_INSN (seq))
	      && GET_CODE (PATTERN (NEXT_INSN (seq))) == SET
	      && SET_DEST (PATTERN (NEXT_INSN (seq))) == reg
	      && memory_address_addr_space_p
		   (GET_MODE (x), SET_SRC (PATTERN (NEXT_INSN (seq))),
		    MEM_ADDR_SPACE (x)))
	    {
	      rtx src1 = SET_SRC (PATTERN (seq));
	      rtx src2 = SET_SRC (PATTERN (NEXT_INSN (seq)));

	      /* Replace the placeholder in SRC2 with SRC1.  If we can
		 find where in SRC2 it was placed, that can become our
		 split point and we can replace this address with SRC2.
		 Just try two obvious places.  */

	      src2 = replace_rtx (src2, reg, src1);
	      split = 0;
	      if (XEXP (src2, 0) == src1)
		split = &XEXP (src2, 0);
	      else if (GET_RTX_FORMAT (GET_CODE (XEXP (src2, 0)))[0] == 'e'
		       && XEXP (XEXP (src2, 0), 0) == src1)
		split = &XEXP (XEXP (src2, 0), 0);

	      if (split)
		{
		  SUBST (XEXP (x, 0), src2);
		  return split;
		}
	    }

	  /* If that didn't work and we have a nested plus, like:
	     ((REG1 * CONST1) + REG2) + CONST2 and (REG1 + REG2) + CONST2
	     is valid address, try to split (REG1 * CONST1).  */
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == PLUS
	      && !OBJECT_P (XEXP (XEXP (XEXP (x, 0), 0), 0))
	      && OBJECT_P (XEXP (XEXP (XEXP (x, 0), 0), 1))
	      && ! (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SUBREG
		    && OBJECT_P (SUBREG_REG (XEXP (XEXP (XEXP (x, 0),
							 0), 0)))))
	    {
	      rtx tem = XEXP (XEXP (XEXP (x, 0), 0), 0);
	      XEXP (XEXP (XEXP (x, 0), 0), 0) = reg;
	      if (memory_address_addr_space_p (GET_MODE (x), XEXP (x, 0),
					       MEM_ADDR_SPACE (x)))
		{
		  XEXP (XEXP (XEXP (x, 0), 0), 0) = tem;
		  return &XEXP (XEXP (XEXP (x, 0), 0), 0);
		}
	      XEXP (XEXP (XEXP (x, 0), 0), 0) = tem;
	    }
	  else if (GET_CODE (XEXP (XEXP (x, 0), 0)) == PLUS
		   && OBJECT_P (XEXP (XEXP (XEXP (x, 0), 0), 0))
		   && !OBJECT_P (XEXP (XEXP (XEXP (x, 0), 0), 1))
		   && ! (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SUBREG
			 && OBJECT_P (SUBREG_REG (XEXP (XEXP (XEXP (x, 0),
							      0), 1)))))
	    {
	      rtx tem = XEXP (XEXP (XEXP (x, 0), 0), 1);
	      XEXP (XEXP (XEXP (x, 0), 0), 1) = reg;
	      if (memory_address_addr_space_p (GET_MODE (x), XEXP (x, 0),
					       MEM_ADDR_SPACE (x)))
		{
		  XEXP (XEXP (XEXP (x, 0), 0), 1) = tem;
		  return &XEXP (XEXP (XEXP (x, 0), 0), 1);
		}
	      XEXP (XEXP (XEXP (x, 0), 0), 1) = tem;
	    }

	  /* If that didn't work, perhaps the first operand is complex and
	     needs to be computed separately, so make a split point there.
	     This will occur on machines that just support REG + CONST
	     and have a constant moved through some previous computation.  */
	  if (!OBJECT_P (XEXP (XEXP (x, 0), 0))
	      && ! (GET_CODE (XEXP (XEXP (x, 0), 0)) == SUBREG
		    && OBJECT_P (SUBREG_REG (XEXP (XEXP (x, 0), 0)))))
	    return &XEXP (XEXP (x, 0), 0);
	}

      /* If we have a PLUS whose first operand is complex, try computing it
         separately by making a split there.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
          && ! memory_address_addr_space_p (GET_MODE (x), XEXP (x, 0),
					    MEM_ADDR_SPACE (x))
          && ! OBJECT_P (XEXP (XEXP (x, 0), 0))
          && ! (GET_CODE (XEXP (XEXP (x, 0), 0)) == SUBREG
                && OBJECT_P (SUBREG_REG (XEXP (XEXP (x, 0), 0)))))
        return &XEXP (XEXP (x, 0), 0);
      break;

    case SET:
      /* See if we can split SET_SRC as it stands.  */
      split = find_split_point (&SET_SRC (x), insn, true);
      if (split && split != &SET_SRC (x))
	return split;

      /* See if we can split SET_DEST as it stands.  */
      split = find_split_point (&SET_DEST (x), insn, false);
      if (split && split != &SET_DEST (x))
	return split;

      /* See if this is a bitfield assignment with everything constant.  If
	 so, this is an IOR of an AND, so split it into that.  */
      if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	  && is_a <scalar_int_mode> (GET_MODE (XEXP (SET_DEST (x), 0)),
				     &inner_mode)
	  && HWI_COMPUTABLE_MODE_P (inner_mode)
	  && CONST_INT_P (XEXP (SET_DEST (x), 1))
	  && CONST_INT_P (XEXP (SET_DEST (x), 2))
	  && CONST_INT_P (SET_SRC (x))
	  && ((INTVAL (XEXP (SET_DEST (x), 1))
	       + INTVAL (XEXP (SET_DEST (x), 2)))
	      <= GET_MODE_PRECISION (inner_mode))
	  && ! side_effects_p (XEXP (SET_DEST (x), 0)))
	{
	  HOST_WIDE_INT pos = INTVAL (XEXP (SET_DEST (x), 2));
	  unsigned HOST_WIDE_INT len = INTVAL (XEXP (SET_DEST (x), 1));
	  rtx dest = XEXP (SET_DEST (x), 0);
	  unsigned HOST_WIDE_INT mask = (HOST_WIDE_INT_1U << len) - 1;
	  unsigned HOST_WIDE_INT src = INTVAL (SET_SRC (x)) & mask;
	  rtx or_mask;

	  if (BITS_BIG_ENDIAN)
	    pos = GET_MODE_PRECISION (inner_mode) - len - pos;

	  or_mask = gen_int_mode (src << pos, inner_mode);
	  if (src == mask)
	    SUBST (SET_SRC (x),
		   simplify_gen_binary (IOR, inner_mode, dest, or_mask));
	  else
	    {
	      rtx negmask = gen_int_mode (~(mask << pos), inner_mode);
	      SUBST (SET_SRC (x),
		     simplify_gen_binary (IOR, inner_mode,
					  simplify_gen_binary (AND, inner_mode,
							       dest, negmask),
					  or_mask));
	    }

	  SUBST (SET_DEST (x), dest);

	  split = find_split_point (&SET_SRC (x), insn, true);
	  if (split && split != &SET_SRC (x))
	    return split;
	}

      /* Otherwise, see if this is an operation that we can split into two.
	 If so, try to split that.  */
      code = GET_CODE (SET_SRC (x));

      switch (code)
	{
	case AND:
	  /* If we are AND'ing with a large constant that is only a single
	     bit and the result is only being used in a context where we
	     need to know if it is zero or nonzero, replace it with a bit
	     extraction.  This will avoid the large constant, which might
	     have taken more than one insn to make.  If the constant were
	     not a valid argument to the AND but took only one insn to make,
	     this is no worse, but if it took more than one insn, it will
	     be better.  */

	  if (CONST_INT_P (XEXP (SET_SRC (x), 1))
	      && REG_P (XEXP (SET_SRC (x), 0))
	      && (pos = exact_log2 (UINTVAL (XEXP (SET_SRC (x), 1)))) >= 7
	      && REG_P (SET_DEST (x))
	      && (split = find_single_use (SET_DEST (x), insn, NULL)) != 0
	      && (GET_CODE (*split) == EQ || GET_CODE (*split) == NE)
	      && XEXP (*split, 0) == SET_DEST (x)
	      && XEXP (*split, 1) == const0_rtx)
	    {
	      rtx extraction = make_extraction (GET_MODE (SET_DEST (x)),
						XEXP (SET_SRC (x), 0),
						pos, NULL_RTX, 1,
						true, false, false);
	      if (extraction != 0)
		{
		  SUBST (SET_SRC (x), extraction);
		  return find_split_point (loc, insn, false);
		}
	    }
	  break;

	case NE:
	  /* If STORE_FLAG_VALUE is -1, this is (NE X 0) and only one bit of X
	     is known to be on, this can be converted into a NEG of a shift.  */
	  if (STORE_FLAG_VALUE == -1 && XEXP (SET_SRC (x), 1) == const0_rtx
	      && GET_MODE (SET_SRC (x)) == GET_MODE (XEXP (SET_SRC (x), 0))
	      && ((pos = exact_log2 (nonzero_bits (XEXP (SET_SRC (x), 0),
						   GET_MODE (XEXP (SET_SRC (x),
							     0))))) >= 1))
	    {
	      machine_mode mode = GET_MODE (XEXP (SET_SRC (x), 0));
	      rtx pos_rtx = gen_int_shift_amount (mode, pos);
	      SUBST (SET_SRC (x),
		     gen_rtx_NEG (mode,
				  gen_rtx_LSHIFTRT (mode,
						    XEXP (SET_SRC (x), 0),
						    pos_rtx)));

	      split = find_split_point (&SET_SRC (x), insn, true);
	      if (split && split != &SET_SRC (x))
		return split;
	    }
	  break;

	case SIGN_EXTEND:
	  inner = XEXP (SET_SRC (x), 0);

	  /* We can't optimize if either mode is a partial integer
	     mode as we don't know how many bits are significant
	     in those modes.  */
	  if (!is_int_mode (GET_MODE (inner), &inner_mode)
	      || GET_MODE_CLASS (GET_MODE (SET_SRC (x))) == MODE_PARTIAL_INT)
	    break;

	  pos = 0;
	  len = GET_MODE_PRECISION (inner_mode);
	  unsignedp = false;
	  break;

	case SIGN_EXTRACT:
	case ZERO_EXTRACT:
	  if (is_a <scalar_int_mode> (GET_MODE (XEXP (SET_SRC (x), 0)),
				      &inner_mode)
	      && CONST_INT_P (XEXP (SET_SRC (x), 1))
	      && CONST_INT_P (XEXP (SET_SRC (x), 2)))
	    {
	      inner = XEXP (SET_SRC (x), 0);
	      len = INTVAL (XEXP (SET_SRC (x), 1));
	      pos = INTVAL (XEXP (SET_SRC (x), 2));

	      if (BITS_BIG_ENDIAN)
		pos = GET_MODE_PRECISION (inner_mode) - len - pos;
	      unsignedp = (code == ZERO_EXTRACT);
	    }
	  break;

	default:
	  break;
	}

      if (len
	  && known_subrange_p (pos, len,
			       0, GET_MODE_PRECISION (GET_MODE (inner)))
	  && is_a <scalar_int_mode> (GET_MODE (SET_SRC (x)), &mode))
	{
	  /* For unsigned, we have a choice of a shift followed by an
	     AND or two shifts.  Use two shifts for field sizes where the
	     constant might be too large.  We assume here that we can
	     always at least get 8-bit constants in an AND insn, which is
	     true for every current RISC.  */

	  if (unsignedp && len <= 8)
	    {
	      unsigned HOST_WIDE_INT mask
		= (HOST_WIDE_INT_1U << len) - 1;
	      rtx pos_rtx = gen_int_shift_amount (mode, pos);
	      SUBST (SET_SRC (x),
		     gen_rtx_AND (mode,
				  gen_rtx_LSHIFTRT
				  (mode, gen_lowpart (mode, inner), pos_rtx),
				  gen_int_mode (mask, mode)));

	      split = find_split_point (&SET_SRC (x), insn, true);
	      if (split && split != &SET_SRC (x))
		return split;
	    }
	  else
	    {
	      int left_bits = GET_MODE_PRECISION (mode) - len - pos;
	      int right_bits = GET_MODE_PRECISION (mode) - len;
	      SUBST (SET_SRC (x),
		     gen_rtx_fmt_ee
		     (unsignedp ? LSHIFTRT : ASHIFTRT, mode,
		      gen_rtx_ASHIFT (mode,
				      gen_lowpart (mode, inner),
				      gen_int_shift_amount (mode, left_bits)),
		      gen_int_shift_amount (mode, right_bits)));

	      split = find_split_point (&SET_SRC (x), insn, true);
	      if (split && split != &SET_SRC (x))
		return split;
	    }
	}

      /* See if this is a simple operation with a constant as the second
	 operand.  It might be that this constant is out of range and hence
	 could be used as a split point.  */
      if (BINARY_P (SET_SRC (x))
	  && CONSTANT_P (XEXP (SET_SRC (x), 1))
	  && (OBJECT_P (XEXP (SET_SRC (x), 0))
	      || (GET_CODE (XEXP (SET_SRC (x), 0)) == SUBREG
		  && OBJECT_P (SUBREG_REG (XEXP (SET_SRC (x), 0))))))
	return &XEXP (SET_SRC (x), 1);

      /* Finally, see if this is a simple operation with its first operand
	 not in a register.  The operation might require this operand in a
	 register, so return it as a split point.  We can always do this
	 because if the first operand were another operation, we would have
	 already found it as a split point.  */
      if ((BINARY_P (SET_SRC (x)) || UNARY_P (SET_SRC (x)))
	  && ! register_operand (XEXP (SET_SRC (x), 0), VOIDmode))
	return &XEXP (SET_SRC (x), 0);

      return 0;

    case AND:
    case IOR:
      /* We write NOR as (and (not A) (not B)), but if we don't have a NOR,
	 it is better to write this as (not (ior A B)) so we can split it.
	 Similarly for IOR.  */
      if (GET_CODE (XEXP (x, 0)) == NOT && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  SUBST (*loc,
		 gen_rtx_NOT (GET_MODE (x),
			      gen_rtx_fmt_ee (code == IOR ? AND : IOR,
					      GET_MODE (x),
					      XEXP (XEXP (x, 0), 0),
					      XEXP (XEXP (x, 1), 0))));
	  return find_split_point (loc, insn, set_src);
	}

      /* Many RISC machines have a large set of logical insns.  If the
	 second operand is a NOT, put it first so we will try to split the
	 other operand first.  */
      if (GET_CODE (XEXP (x, 1)) == NOT)
	{
	  rtx tem = XEXP (x, 0);
	  SUBST (XEXP (x, 0), XEXP (x, 1));
	  SUBST (XEXP (x, 1), tem);
	}
      break;

    case PLUS:
    case MINUS:
      /* Canonicalization can produce (minus A (mult B C)), where C is a
	 constant.  It may be better to try splitting (plus (mult B -C) A)
	 instead if this isn't a multiply by a power of two.  */
      if (set_src && code == MINUS && GET_CODE (XEXP (x, 1)) == MULT
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && !pow2p_hwi (INTVAL (XEXP (XEXP (x, 1), 1))))
	{
	  machine_mode mode = GET_MODE (x);
	  unsigned HOST_WIDE_INT this_int = INTVAL (XEXP (XEXP (x, 1), 1));
	  HOST_WIDE_INT other_int = trunc_int_for_mode (-this_int, mode);
	  SUBST (*loc, gen_rtx_PLUS (mode,
				     gen_rtx_MULT (mode,
						   XEXP (XEXP (x, 1), 0),
						   gen_int_mode (other_int,
								 mode)),
				     XEXP (x, 0)));
	  return find_split_point (loc, insn, set_src);
	}

      /* Split at a multiply-accumulate instruction.  However if this is
         the SET_SRC, we likely do not have such an instruction and it's
         worthless to try this split.  */
      if (!set_src
	  && (GET_CODE (XEXP (x, 0)) == MULT
	      || (GET_CODE (XEXP (x, 0)) == ASHIFT
		  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)))
        return loc;

    default:
      break;
    }

  /* Otherwise, select our actions depending on our rtx class.  */
  switch (GET_RTX_CLASS (code))
    {
    case RTX_BITFIELD_OPS:		/* This is ZERO_EXTRACT and SIGN_EXTRACT.  */
    case RTX_TERNARY:
      split = find_split_point (&XEXP (x, 2), insn, false);
      if (split)
	return split;
      /* fall through */
    case RTX_BIN_ARITH:
    case RTX_COMM_ARITH:
    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      split = find_split_point (&XEXP (x, 1), insn, false);
      if (split)
	return split;
      /* fall through */
    case RTX_UNARY:
      /* Some machines have (and (shift ...) ...) insns.  If X is not
	 an AND, but XEXP (X, 0) is, use it as our split point.  */
      if (GET_CODE (x) != AND && GET_CODE (XEXP (x, 0)) == AND)
	return &XEXP (x, 0);

      split = find_split_point (&XEXP (x, 0), insn, false);
      if (split)
	return split;
      return loc;

    default:
      /* Otherwise, we don't have a split point.  */
      return 0;
    }
}

/* Throughout X, replace FROM with TO, and return the result.
   The result is TO if X is FROM;
   otherwise the result is X, but its contents may have been modified.
   If they were modified, a record was made in undobuf so that
   undo_all will (among other things) return X to its original state.

   If the number of changes necessary is too much to record to undo,
   the excess changes are not made, so the result is invalid.
   The changes already made can still be undone.
   undobuf.num_undo is incremented for such changes, so by testing that
   the caller can tell whether the result is valid.

   `n_occurrences' is incremented each time FROM is replaced.

   IN_DEST is true if we are processing the SET_DEST of a SET.

   IN_COND is true if we are at the top level of a condition.

   UNIQUE_COPY is true if each substitution must be unique.  We do this
   by copying if `n_occurrences' is nonzero.  */

static rtx
subst (rtx x, rtx from, rtx to, bool in_dest, bool in_cond, bool unique_copy)
{
  enum rtx_code code = GET_CODE (x);
  machine_mode op0_mode = VOIDmode;
  const char *fmt;
  int len, i;
  rtx new_rtx;

/* Two expressions are equal if they are identical copies of a shared
   RTX or if they are both registers with the same register number
   and mode.  */

#define COMBINE_RTX_EQUAL_P(X,Y)			\
  ((X) == (Y)						\
   || (REG_P (X) && REG_P (Y)	\
       && REGNO (X) == REGNO (Y) && GET_MODE (X) == GET_MODE (Y)))

  /* Do not substitute into clobbers of regs -- this will never result in
     valid RTL.  */
  if (GET_CODE (x) == CLOBBER && REG_P (XEXP (x, 0)))
    return x;

  if (! in_dest && COMBINE_RTX_EQUAL_P (x, from))
    {
      n_occurrences++;
      return (unique_copy && n_occurrences > 1 ? copy_rtx (to) : to);
    }

  /* If X and FROM are the same register but different modes, they
     will not have been seen as equal above.  However, the log links code
     will make a LOG_LINKS entry for that case.  If we do nothing, we
     will try to rerecognize our original insn and, when it succeeds,
     we will delete the feeding insn, which is incorrect.

     So force this insn not to match in this (rare) case.  */
  if (! in_dest && code == REG && REG_P (from)
      && reg_overlap_mentioned_p (x, from))
    return gen_rtx_CLOBBER (GET_MODE (x), const0_rtx);

  /* If this is an object, we are done unless it is a MEM or LO_SUM, both
     of which may contain things that can be combined.  */
  if (code != MEM && code != LO_SUM && OBJECT_P (x))
    return x;

  /* It is possible to have a subexpression appear twice in the insn.
     Suppose that FROM is a register that appears within TO.
     Then, after that subexpression has been scanned once by `subst',
     the second time it is scanned, TO may be found.  If we were
     to scan TO here, we would find FROM within it and create a
     self-referent rtl structure which is completely wrong.  */
  if (COMBINE_RTX_EQUAL_P (x, to))
    return to;

  /* Parallel asm_operands need special attention because all of the
     inputs are shared across the arms.  Furthermore, unsharing the
     rtl results in recognition failures.  Failure to handle this case
     specially can result in circular rtl.

     Solve this by doing a normal pass across the first entry of the
     parallel, and only processing the SET_DESTs of the subsequent
     entries.  Ug.  */

  if (code == PARALLEL
      && GET_CODE (XVECEXP (x, 0, 0)) == SET
      && GET_CODE (SET_SRC (XVECEXP (x, 0, 0))) == ASM_OPERANDS)
    {
      new_rtx = subst (XVECEXP (x, 0, 0), from, to, false, false, unique_copy);

      /* If this substitution failed, this whole thing fails.  */
      if (GET_CODE (new_rtx) == CLOBBER
	  && XEXP (new_rtx, 0) == const0_rtx)
	return new_rtx;

      SUBST (XVECEXP (x, 0, 0), new_rtx);

      for (i = XVECLEN (x, 0) - 1; i >= 1; i--)
	{
	  rtx dest = SET_DEST (XVECEXP (x, 0, i));

	  if (!REG_P (dest) && GET_CODE (dest) != PC)
	    {
	      new_rtx = subst (dest, from, to, false, false, unique_copy);

	      /* If this substitution failed, this whole thing fails.  */
	      if (GET_CODE (new_rtx) == CLOBBER
		  && XEXP (new_rtx, 0) == const0_rtx)
		return new_rtx;

	      SUBST (SET_DEST (XVECEXP (x, 0, i)), new_rtx);
	    }
	}
    }
  else
    {
      len = GET_RTX_LENGTH (code);
      fmt = GET_RTX_FORMAT (code);

      /* We don't need to process a SET_DEST that is a register or PC, so
	 set up to skip this common case.  All other cases where we want
	 to suppress replacing something inside a SET_SRC are handled via
	 the IN_DEST operand.  */
      if (code == SET
	  && (REG_P (SET_DEST (x))
	      || GET_CODE (SET_DEST (x)) == PC))
	fmt = "ie";

      /* Trying to simplify the operands of a widening MULT is not likely
	 to create RTL matching a machine insn.  */
      if (code == MULT
	  && (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	  && (GET_CODE (XEXP (x, 1)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 1)) == SIGN_EXTEND)
	  && REG_P (XEXP (XEXP (x, 0), 0))
	  && REG_P (XEXP (XEXP (x, 1), 0))
	  && from == to)
	return x;


      /* Get the mode of operand 0 in case X is now a SIGN_EXTEND of a
	 constant.  */
      if (fmt[0] == 'e')
	op0_mode = GET_MODE (XEXP (x, 0));

      for (i = 0; i < len; i++)
	{
	  if (fmt[i] == 'E')
	    {
	      int j;
	      for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		{
		  if (COMBINE_RTX_EQUAL_P (XVECEXP (x, i, j), from))
		    {
		      new_rtx = (unique_copy && n_occurrences
			     ? copy_rtx (to) : to);
		      n_occurrences++;
		    }
		  else
		    {
		      new_rtx = subst (XVECEXP (x, i, j), from, to,
				       false, false, unique_copy);

		      /* If this substitution failed, this whole thing
			 fails.  */
		      if (GET_CODE (new_rtx) == CLOBBER
			  && XEXP (new_rtx, 0) == const0_rtx)
			return new_rtx;
		    }

		  SUBST (XVECEXP (x, i, j), new_rtx);
		}
	    }
	  else if (fmt[i] == 'e')
	    {
	      /* If this is a register being set, ignore it.  */
	      new_rtx = XEXP (x, i);
	      if (in_dest
		  && i == 0
		  && (((code == SUBREG || code == ZERO_EXTRACT)
		       && REG_P (new_rtx))
		      || code == STRICT_LOW_PART))
		;

	      else if (COMBINE_RTX_EQUAL_P (XEXP (x, i), from))
		{
		  /* In general, don't install a subreg involving two
		     modes not tieable.  It can worsen register
		     allocation, and can even make invalid reload
		     insns, since the reg inside may need to be copied
		     from in the outside mode, and that may be invalid
		     if it is an fp reg copied in integer mode.

		     We allow an exception to this: It is valid if
		     it is inside another SUBREG and the mode of that
		     SUBREG and the mode of the inside of TO is
		     tieable.  */

		  if (GET_CODE (to) == SUBREG
		      && !targetm.modes_tieable_p (GET_MODE (to),
						   GET_MODE (SUBREG_REG (to)))
		      && ! (code == SUBREG
			    && (targetm.modes_tieable_p
				(GET_MODE (x), GET_MODE (SUBREG_REG (to))))))
		    return gen_rtx_CLOBBER (VOIDmode, const0_rtx);

		  if (code == SUBREG
		      && REG_P (to)
		      && REGNO (to) < FIRST_PSEUDO_REGISTER
		      && simplify_subreg_regno (REGNO (to), GET_MODE (to),
						SUBREG_BYTE (x),
						GET_MODE (x)) < 0)
		    return gen_rtx_CLOBBER (VOIDmode, const0_rtx);

		  new_rtx = (unique_copy && n_occurrences ? copy_rtx (to) : to);
		  n_occurrences++;
		}
	      else
		/* If we are in a SET_DEST, suppress most cases unless we
		   have gone inside a MEM, in which case we want to
		   simplify the address.  We assume here that things that
		   are actually part of the destination have their inner
		   parts in the first expression.  This is true for SUBREG,
		   STRICT_LOW_PART, and ZERO_EXTRACT, which are the only
		   things aside from REG and MEM that should appear in a
		   SET_DEST.  */
		new_rtx = subst (XEXP (x, i), from, to,
			     (((in_dest
				&& (code == SUBREG || code == STRICT_LOW_PART
				    || code == ZERO_EXTRACT))
			       || code == SET)
			      && i == 0),
				 code == IF_THEN_ELSE && i == 0,
				 unique_copy);

	      /* If we found that we will have to reject this combination,
		 indicate that by returning the CLOBBER ourselves, rather than
		 an expression containing it.  This will speed things up as
		 well as prevent accidents where two CLOBBERs are considered
		 to be equal, thus producing an incorrect simplification.  */

	      if (GET_CODE (new_rtx) == CLOBBER && XEXP (new_rtx, 0) == const0_rtx)
		return new_rtx;

	      if (GET_CODE (x) == SUBREG && CONST_SCALAR_INT_P (new_rtx))
		{
		  machine_mode mode = GET_MODE (x);

		  x = simplify_subreg (GET_MODE (x), new_rtx,
				       GET_MODE (SUBREG_REG (x)),
				       SUBREG_BYTE (x));
		  if (! x)
		    x = gen_rtx_CLOBBER (mode, const0_rtx);
		}
	      else if (CONST_SCALAR_INT_P (new_rtx)
		       && (GET_CODE (x) == ZERO_EXTEND
			   || GET_CODE (x) == SIGN_EXTEND
			   || GET_CODE (x) == FLOAT
			   || GET_CODE (x) == UNSIGNED_FLOAT))
		{
		  x = simplify_unary_operation (GET_CODE (x), GET_MODE (x),
						new_rtx,
						GET_MODE (XEXP (x, 0)));
		  if (!x)
		    return gen_rtx_CLOBBER (VOIDmode, const0_rtx);
		}
	      /* CONST_INTs shouldn't be substituted into PRE_DEC, PRE_MODIFY
		 etc. arguments, otherwise we can ICE before trying to recog
		 it.  See PR104446.  */
	      else if (CONST_SCALAR_INT_P (new_rtx)
		       && GET_RTX_CLASS (GET_CODE (x)) == RTX_AUTOINC)
		return gen_rtx_CLOBBER (VOIDmode, const0_rtx);
	      else
		SUBST (XEXP (x, i), new_rtx);
	    }
	}
    }

  /* Check if we are loading something from the constant pool via float
     extension; in this case we would undo compress_float_constant
     optimization and degenerate constant load to an immediate value.  */
  if (GET_CODE (x) == FLOAT_EXTEND
      && MEM_P (XEXP (x, 0))
      && MEM_READONLY_P (XEXP (x, 0)))
    {
      rtx tmp = avoid_constant_pool_reference (x);
      if (x != tmp)
        return x;
    }

  /* Try to simplify X.  If the simplification changed the code, it is likely
     that further simplification will help, so loop, but limit the number
     of repetitions that will be performed.  */

  for (i = 0; i < 4; i++)
    {
      /* If X is sufficiently simple, don't bother trying to do anything
	 with it.  */
      if (code != CONST_INT && code != REG && code != CLOBBER)
	x = combine_simplify_rtx (x, op0_mode, in_dest, in_cond);

      if (GET_CODE (x) == code)
	break;

      code = GET_CODE (x);

      /* We no longer know the original mode of operand 0 since we
	 have changed the form of X)  */
      op0_mode = VOIDmode;
    }

  return x;
}

/* If X is a commutative operation whose operands are not in the canonical
   order, use substitutions to swap them.  */

static void
maybe_swap_commutative_operands (rtx x)
{
  if (COMMUTATIVE_ARITH_P (x)
      && swap_commutative_operands_p (XEXP (x, 0), XEXP (x, 1)))
    {
      rtx temp = XEXP (x, 0);
      SUBST (XEXP (x, 0), XEXP (x, 1));
      SUBST (XEXP (x, 1), temp);
    }

  unsigned n_elts = 0;
  if (GET_CODE (x) == VEC_MERGE
      && CONST_INT_P (XEXP (x, 2))
      && GET_MODE_NUNITS (GET_MODE (x)).is_constant (&n_elts)
      && (swap_commutative_operands_p (XEXP (x, 0), XEXP (x, 1))
	  /* Two operands have same precedence, then
	     first bit of mask select first operand.  */
	  || (!swap_commutative_operands_p (XEXP (x, 1), XEXP (x, 0))
	      && !(UINTVAL (XEXP (x, 2)) & 1))))
    {
      rtx temp = XEXP (x, 0);
      unsigned HOST_WIDE_INT sel = UINTVAL (XEXP (x, 2));
      unsigned HOST_WIDE_INT mask = HOST_WIDE_INT_1U;
      if (n_elts == HOST_BITS_PER_WIDE_INT)
	mask = -1;
      else
	mask = (HOST_WIDE_INT_1U << n_elts) - 1;
      SUBST (XEXP (x, 0), XEXP (x, 1));
      SUBST (XEXP (x, 1), temp);
      SUBST (XEXP (x, 2), GEN_INT (~sel & mask));
    }
}

/* Simplify X, a piece of RTL.  We just operate on the expression at the
   outer level; call `subst' to simplify recursively.  Return the new
   expression.

   OP0_MODE is the original mode of XEXP (x, 0).  IN_DEST is true
   if we are inside a SET_DEST.  IN_COND is true if we are at the top level
   of a condition.  */

static rtx
combine_simplify_rtx (rtx x, machine_mode op0_mode, bool in_dest, bool in_cond)
{
  enum rtx_code code = GET_CODE (x);
  machine_mode mode = GET_MODE (x);
  scalar_int_mode int_mode;
  rtx temp;
  int i;

  /* If this is a commutative operation, put a constant last and a complex
     expression first.  We don't need to do this for comparisons here.  */
  maybe_swap_commutative_operands (x);

  /* Try to fold this expression in case we have constants that weren't
     present before.  */
  temp = 0;
  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      if (op0_mode == VOIDmode)
	op0_mode = GET_MODE (XEXP (x, 0));
      temp = simplify_unary_operation (code, mode, XEXP (x, 0), op0_mode);
      break;
    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      {
	machine_mode cmp_mode = GET_MODE (XEXP (x, 0));
	if (cmp_mode == VOIDmode)
	  {
	    cmp_mode = GET_MODE (XEXP (x, 1));
	    if (cmp_mode == VOIDmode)
	      cmp_mode = op0_mode;
	  }
	temp = simplify_relational_operation (code, mode, cmp_mode,
					      XEXP (x, 0), XEXP (x, 1));
      }
      break;
    case RTX_COMM_ARITH:
    case RTX_BIN_ARITH:
      temp = simplify_binary_operation (code, mode, XEXP (x, 0), XEXP (x, 1));
      break;
    case RTX_BITFIELD_OPS:
    case RTX_TERNARY:
      temp = simplify_ternary_operation (code, mode, op0_mode, XEXP (x, 0),
					 XEXP (x, 1), XEXP (x, 2));
      break;
    default:
      break;
    }

  if (temp)
    {
      x = temp;
      code = GET_CODE (temp);
      op0_mode = VOIDmode;
      mode = GET_MODE (temp);
    }

  /* If this is a simple operation applied to an IF_THEN_ELSE, try
     applying it to the arms of the IF_THEN_ELSE.  This often simplifies
     things.  Check for cases where both arms are testing the same
     condition.

     Don't do anything if all operands are very simple.  */

  if ((BINARY_P (x)
       && ((!OBJECT_P (XEXP (x, 0))
	    && ! (GET_CODE (XEXP (x, 0)) == SUBREG
		  && OBJECT_P (SUBREG_REG (XEXP (x, 0)))))
	   || (!OBJECT_P (XEXP (x, 1))
	       && ! (GET_CODE (XEXP (x, 1)) == SUBREG
		     && OBJECT_P (SUBREG_REG (XEXP (x, 1)))))))
      || (UNARY_P (x)
	  && (!OBJECT_P (XEXP (x, 0))
	       && ! (GET_CODE (XEXP (x, 0)) == SUBREG
		     && OBJECT_P (SUBREG_REG (XEXP (x, 0)))))))
    {
      rtx cond, true_rtx, false_rtx;

      cond = if_then_else_cond (x, &true_rtx, &false_rtx);
      if (cond != 0
	  /* If everything is a comparison, what we have is highly unlikely
	     to be simpler, so don't use it.  */
	  && ! (COMPARISON_P (x)
		&& (COMPARISON_P (true_rtx) || COMPARISON_P (false_rtx)))
	  /* Similarly, if we end up with one of the expressions the same
	     as the original, it is certainly not simpler.  */
	  && ! rtx_equal_p (x, true_rtx)
	  && ! rtx_equal_p (x, false_rtx))
	{
	  rtx cop1 = const0_rtx;
	  enum rtx_code cond_code = simplify_comparison (NE, &cond, &cop1);

	  if (cond_code == NE && COMPARISON_P (cond))
	    return x;

	  /* Simplify the alternative arms; this may collapse the true and
	     false arms to store-flag values.  Be careful to use copy_rtx
	     here since true_rtx or false_rtx might share RTL with x as a
	     result of the if_then_else_cond call above.  */
	  true_rtx = subst (copy_rtx (true_rtx), pc_rtx, pc_rtx,
			    false, false, false);
	  false_rtx = subst (copy_rtx (false_rtx), pc_rtx, pc_rtx,
			     false, false, false);

	  /* If true_rtx and false_rtx are not general_operands, an if_then_else
	     is unlikely to be simpler.  */
	  if (general_operand (true_rtx, VOIDmode)
	      && general_operand (false_rtx, VOIDmode))
	    {
	      enum rtx_code reversed;

	      /* Restarting if we generate a store-flag expression will cause
		 us to loop.  Just drop through in this case.  */

	      /* If the result values are STORE_FLAG_VALUE and zero, we can
		 just make the comparison operation.  */
	      if (true_rtx == const_true_rtx && false_rtx == const0_rtx)
		x = simplify_gen_relational (cond_code, mode, VOIDmode,
					     cond, cop1);
	      else if (true_rtx == const0_rtx && false_rtx == const_true_rtx
		       && ((reversed = reversed_comparison_code_parts
					(cond_code, cond, cop1, NULL))
			   != UNKNOWN))
		x = simplify_gen_relational (reversed, mode, VOIDmode,
					     cond, cop1);

	      /* Likewise, we can make the negate of a comparison operation
		 if the result values are - STORE_FLAG_VALUE and zero.  */
	      else if (CONST_INT_P (true_rtx)
		       && INTVAL (true_rtx) == - STORE_FLAG_VALUE
		       && false_rtx == const0_rtx)
		x = simplify_gen_unary (NEG, mode,
					simplify_gen_relational (cond_code,
								 mode, VOIDmode,
								 cond, cop1),
					mode);
	      else if (CONST_INT_P (false_rtx)
		       && INTVAL (false_rtx) == - STORE_FLAG_VALUE
		       && true_rtx == const0_rtx
		       && ((reversed = reversed_comparison_code_parts
					(cond_code, cond, cop1, NULL))
			   != UNKNOWN))
		x = simplify_gen_unary (NEG, mode,
					simplify_gen_relational (reversed,
								 mode, VOIDmode,
								 cond, cop1),
					mode);

	      code = GET_CODE (x);
	      op0_mode = VOIDmode;
	    }
	}
    }

  /* First see if we can apply the inverse distributive law.  */
  if (code == PLUS || code == MINUS
      || code == AND || code == IOR || code == XOR)
    {
      x = apply_distributive_law (x);
      code = GET_CODE (x);
      op0_mode = VOIDmode;
    }

  /* If CODE is an associative operation not otherwise handled, see if we
     can associate some operands.  This can win if they are constants or
     if they are logically related (i.e. (a & b) & a).  */
  if ((code == PLUS || code == MINUS || code == MULT || code == DIV
       || code == AND || code == IOR || code == XOR
       || code == SMAX || code == SMIN || code == UMAX || code == UMIN)
      && ((INTEGRAL_MODE_P (mode) && code != DIV)
	  || (flag_associative_math && FLOAT_MODE_P (mode))))
    {
      if (GET_CODE (XEXP (x, 0)) == code)
	{
	  rtx other = XEXP (XEXP (x, 0), 0);
	  rtx inner_op0 = XEXP (XEXP (x, 0), 1);
	  rtx inner_op1 = XEXP (x, 1);
	  rtx inner;

	  /* Make sure we pass the constant operand if any as the second
	     one if this is a commutative operation.  */
	  if (CONSTANT_P (inner_op0) && COMMUTATIVE_ARITH_P (x))
	    std::swap (inner_op0, inner_op1);
	  inner = simplify_binary_operation (code == MINUS ? PLUS
					     : code == DIV ? MULT
					     : code,
					     mode, inner_op0, inner_op1);

	  /* For commutative operations, try the other pair if that one
	     didn't simplify.  */
	  if (inner == 0 && COMMUTATIVE_ARITH_P (x))
	    {
	      other = XEXP (XEXP (x, 0), 1);
	      inner = simplify_binary_operation (code, mode,
						 XEXP (XEXP (x, 0), 0),
						 XEXP (x, 1));
	    }

	  if (inner)
	    return simplify_gen_binary (code, mode, other, inner);
	}
    }

  /* A little bit of algebraic simplification here.  */
  switch (code)
    {
    case MEM:
      /* Ensure that our address has any ASHIFTs converted to MULT in case
	 address-recognizing predicates are called later.  */
      temp = make_compound_operation (XEXP (x, 0), MEM);
      SUBST (XEXP (x, 0), temp);
      break;

    case SUBREG:
      if (op0_mode == VOIDmode)
	op0_mode = GET_MODE (SUBREG_REG (x));

      /* See if this can be moved to simplify_subreg.  */
      if (CONSTANT_P (SUBREG_REG (x))
	  && known_eq (subreg_lowpart_offset (mode, op0_mode), SUBREG_BYTE (x))
	     /* Don't call gen_lowpart if the inner mode
		is VOIDmode and we cannot simplify it, as SUBREG without
		inner mode is invalid.  */
	  && (GET_MODE (SUBREG_REG (x)) != VOIDmode
	      || gen_lowpart_common (mode, SUBREG_REG (x))))
	return gen_lowpart (mode, SUBREG_REG (x));

      if (GET_MODE_CLASS (GET_MODE (SUBREG_REG (x))) == MODE_CC)
	break;
      {
	rtx temp;
	temp = simplify_subreg (mode, SUBREG_REG (x), op0_mode,
				SUBREG_BYTE (x));
	if (temp)
	  return temp;

	/* If op is known to have all lower bits zero, the result is zero.  */
	scalar_int_mode int_mode, int_op0_mode;
	if (!in_dest
	    && is_a <scalar_int_mode> (mode, &int_mode)
	    && is_a <scalar_int_mode> (op0_mode, &int_op0_mode)
	    && (GET_MODE_PRECISION (int_mode)
		< GET_MODE_PRECISION (int_op0_mode))
	    && known_eq (subreg_lowpart_offset (int_mode, int_op0_mode),
			 SUBREG_BYTE (x))
	    && HWI_COMPUTABLE_MODE_P (int_op0_mode)
	    && ((nonzero_bits (SUBREG_REG (x), int_op0_mode)
		 & GET_MODE_MASK (int_mode)) == 0)
	    && !side_effects_p (SUBREG_REG (x)))
	  return CONST0_RTX (int_mode);
      }

      /* Don't change the mode of the MEM if that would change the meaning
	 of the address.  */
      if (MEM_P (SUBREG_REG (x))
	  && (MEM_VOLATILE_P (SUBREG_REG (x))
	      || mode_dependent_address_p (XEXP (SUBREG_REG (x), 0),
					   MEM_ADDR_SPACE (SUBREG_REG (x)))))
	return gen_rtx_CLOBBER (mode, const0_rtx);

      /* Note that we cannot do any narrowing for non-constants since
	 we might have been counting on using the fact that some bits were
	 zero.  We now do this in the SET.  */

      break;

    case NEG:
      temp = expand_compound_operation (XEXP (x, 0));

      /* For C equal to the width of MODE minus 1, (neg (ashiftrt X C)) can be
	 replaced by (lshiftrt X C).  This will convert
	 (neg (sign_extract X 1 Y)) to (zero_extract X 1 Y).  */

      if (GET_CODE (temp) == ASHIFTRT
	  && CONST_INT_P (XEXP (temp, 1))
	  && INTVAL (XEXP (temp, 1)) == GET_MODE_UNIT_PRECISION (mode) - 1)
	return simplify_shift_const (NULL_RTX, LSHIFTRT, mode, XEXP (temp, 0),
				     INTVAL (XEXP (temp, 1)));

      /* If X has only a single bit that might be nonzero, say, bit I, convert
	 (neg X) to (ashiftrt (ashift X C-I) C-I) where C is the bitsize of
	 MODE minus 1.  This will convert (neg (zero_extract X 1 Y)) to
	 (sign_extract X 1 Y).  But only do this if TEMP isn't a register
	 or a SUBREG of one since we'd be making the expression more
	 complex if it was just a register.  */

      if (!REG_P (temp)
	  && ! (GET_CODE (temp) == SUBREG
		&& REG_P (SUBREG_REG (temp)))
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && (i = exact_log2 (nonzero_bits (temp, int_mode))) >= 0)
	{
	  rtx temp1 = simplify_shift_const
	    (NULL_RTX, ASHIFTRT, int_mode,
	     simplify_shift_const (NULL_RTX, ASHIFT, int_mode, temp,
				   GET_MODE_PRECISION (int_mode) - 1 - i),
	     GET_MODE_PRECISION (int_mode) - 1 - i);

	  /* If all we did was surround TEMP with the two shifts, we
	     haven't improved anything, so don't use it.  Otherwise,
	     we are better off with TEMP1.  */
	  if (GET_CODE (temp1) != ASHIFTRT
	      || GET_CODE (XEXP (temp1, 0)) != ASHIFT
	      || XEXP (XEXP (temp1, 0), 0) != temp)
	    return temp1;
	}
      break;

    case TRUNCATE:
      /* We can't handle truncation to a partial integer mode here
	 because we don't know the real bitsize of the partial
	 integer mode.  */
      if (GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	break;

      if (HWI_COMPUTABLE_MODE_P (mode))
	SUBST (XEXP (x, 0),
	       force_to_mode (XEXP (x, 0), GET_MODE (XEXP (x, 0)),
			      GET_MODE_MASK (mode), false));

      /* We can truncate a constant value and return it.  */
      {
	poly_int64 c;
	if (poly_int_rtx_p (XEXP (x, 0), &c))
	  return gen_int_mode (c, mode);
      }

      /* Similarly to what we do in simplify-rtx.cc, a truncate of a register
	 whose value is a comparison can be replaced with a subreg if
	 STORE_FLAG_VALUE permits.  */
      if (HWI_COMPUTABLE_MODE_P (mode)
	  && (STORE_FLAG_VALUE & ~GET_MODE_MASK (mode)) == 0
	  && (temp = get_last_value (XEXP (x, 0)))
	  && COMPARISON_P (temp)
	  && TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (XEXP (x, 0))))
	return gen_lowpart (mode, XEXP (x, 0));
      break;

    case CONST:
      /* (const (const X)) can become (const X).  Do it this way rather than
	 returning the inner CONST since CONST can be shared with a
	 REG_EQUAL note.  */
      if (GET_CODE (XEXP (x, 0)) == CONST)
	SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
      break;

    case LO_SUM:
      /* Convert (lo_sum (high FOO) FOO) to FOO.  This is necessary so we
	 can add in an offset.  find_split_point will split this address up
	 again if it doesn't match.  */
      if (HAVE_lo_sum && GET_CODE (XEXP (x, 0)) == HIGH
	  && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1)))
	return XEXP (x, 1);
      break;

    case PLUS:
      /* (plus (xor (and <foo> (const_int pow2 - 1)) <c>) <-c>)
	 when c is (const_int (pow2 + 1) / 2) is a sign extension of a
	 bit-field and can be replaced by either a sign_extend or a
	 sign_extract.  The `and' may be a zero_extend and the two
	 <c>, -<c> constants may be reversed.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && INTVAL (XEXP (x, 1)) == -INTVAL (XEXP (XEXP (x, 0), 1))
	  && ((i = exact_log2 (UINTVAL (XEXP (XEXP (x, 0), 1)))) >= 0
	      || (i = exact_log2 (UINTVAL (XEXP (x, 1)))) >= 0)
	  && HWI_COMPUTABLE_MODE_P (int_mode)
	  && ((GET_CODE (XEXP (XEXP (x, 0), 0)) == AND
	       && CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 0), 1))
	       && (UINTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))
		   == (HOST_WIDE_INT_1U << (i + 1)) - 1))
	      || (GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND
		  && known_eq ((GET_MODE_PRECISION
				(GET_MODE (XEXP (XEXP (XEXP (x, 0), 0), 0)))),
			       (unsigned int) i + 1))))
	return simplify_shift_const
	  (NULL_RTX, ASHIFTRT, int_mode,
	   simplify_shift_const (NULL_RTX, ASHIFT, int_mode,
				 XEXP (XEXP (XEXP (x, 0), 0), 0),
				 GET_MODE_PRECISION (int_mode) - (i + 1)),
	   GET_MODE_PRECISION (int_mode) - (i + 1));

      /* If only the low-order bit of X is possibly nonzero, (plus x -1)
	 can become (ashiftrt (ashift (xor x 1) C) C) where C is
	 the bitsize of the mode - 1.  This allows simplification of
	 "a = (b & 8) == 0;"  */
      if (XEXP (x, 1) == constm1_rtx
	  && !REG_P (XEXP (x, 0))
	  && ! (GET_CODE (XEXP (x, 0)) == SUBREG
		&& REG_P (SUBREG_REG (XEXP (x, 0))))
	  && is_a <scalar_int_mode> (mode, &int_mode)
	  && nonzero_bits (XEXP (x, 0), int_mode) == 1)
	return simplify_shift_const
	  (NULL_RTX, ASHIFTRT, int_mode,
	   simplify_shift_const (NULL_RTX, ASHIFT, int_mode,
				 gen_rtx_XOR (int_mode, XEXP (x, 0),
					      const1_rtx),
				 GET_MODE_PRECISION (int_mode) - 1),
	   GET_MODE_PRECISION (int_mode) - 1);

      /* If we are adding two things that have no bits in common, convert
	 the addition into an IOR.  This will often be further simplified,
	 for example in cases like ((a & 1) + (a & 2)), which can
	 become a & 3.  */

      if (HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (XEXP (x, 0), mode)
	      & nonzero_bits (XEXP (x, 1), mode)) == 0)
	{
	  /* Try to simplify the expression further.  */
	  rtx tor = simplify_gen_binary (IOR, mode, XEXP (x, 0), XEXP (x, 1));
	  temp = combine_simplify_rtx (tor, VOIDmode, in_dest, false);

	  /* If we could, great.  If not, do not go ahead with the IOR
	     replacement, since PLUS appears in many special purpose
	     address arithmetic instructions.  */
	  if (GET_CODE (temp) != CLOBBER
	      && (GET_CODE (temp) != IOR
		  || ((XEXP (temp, 0) != XEXP (x, 0)
		       || XEXP (temp, 1) != XEXP (x, 1))
		      && (XEXP (temp, 0) != XEXP (x, 1)
			  || XEXP (temp, 1) != XEXP (x, 0)))))
	    return temp;
	}

      /* Canonicalize x + x into x << 1.  */
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && rtx_equal_p (XEXP (x, 0), XEXP (x, 1))
	  && !side_effects_p (XEXP (x, 0)))
	return simplify_gen_binary (ASHIFT, mode, XEXP (x, 0), const1_rtx);

      break;

    case MINUS:
      /* (minus <foo> (and <foo> (const_int -pow2))) becomes
	 (and <foo> (const_int pow2-1))  */
      if (is_a <scalar_int_mode> (mode, &int_mode)
	  && GET_CODE (XEXP (x, 1)) == AND
	  && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	  && pow2p_hwi (-UINTVAL (XEXP (XEXP (x, 1), 1)))
	  && rtx_equal_p (XEXP (XEXP (x, 1), 0), XEXP (x, 0)))
	return simplify_and_const_int (NULL_RTX, int_mode, XEXP (x, 0),
				       -INTVAL (XEXP (XEXP (x, 1), 1)) - 1);
      break;

    case MULT:
      /* If we have (mult (plus A B) C), apply the distributive law and then
	 the inverse distributive law to see if things simplify.  This
	 occurs mostly in addresses, often when unrolling loops.  */

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  rtx result = distribute_and_simplify_rtx (x, 0);
	  if (result)
	    return result;
	}

      /* Try simplify a*(b/c) as (a*b)/c.  */
      if (FLOAT_MODE_P (mode) && flag_associative_math
	  && GET_CODE (XEXP (x, 0)) == DIV)
	{
	  rtx tem = simplify_binary_operation (MULT, mode,
					       XEXP (XEXP (x, 0), 0),
					       XEXP (x, 1));
	  if (tem)
	    return simplify_gen_binary (DIV, mode, tem, XEXP (XEXP (x, 0), 1));
	}
      break;

    case UDIV:
      /* If this is a divide by a power of two, treat it as a shift if
	 its first operand is a shift.  */
      if (is_a <scalar_int_mode> (mode, &int_mode)
	  && CONST_INT_P (XEXP (x, 1))
	  && (i = exact_log2 (UINTVAL (XEXP (x, 1)))) >= 0
	  && (GET_CODE (XEXP (x, 0)) == ASHIFT
	      || GET_CODE (XEXP (x, 0)) == LSHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ASHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ROTATE
	      || GET_CODE (XEXP (x, 0)) == ROTATERT))
	return simplify_shift_const (NULL_RTX, LSHIFTRT, int_mode,
				     XEXP (x, 0), i);
      break;

    case EQ:  case NE:
    case GT:  case GTU:  case GE:  case GEU:
    case LT:  case LTU:  case LE:  case LEU:
    case UNEQ:  case LTGT:
    case UNGT:  case UNGE:
    case UNLT:  case UNLE:
    case UNORDERED: case ORDERED:
      /* If the first operand is a condition code, we can't do anything
	 with it.  */
      if (GET_CODE (XEXP (x, 0)) == COMPARE
	  || GET_MODE_CLASS (GET_MODE (XEXP (x, 0))) != MODE_CC)
	{
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = XEXP (x, 1);
	  enum rtx_code new_code;

	  if (GET_CODE (op0) == COMPARE)
	    op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

	  /* Simplify our comparison, if possible.  */
	  new_code = simplify_comparison (code, &op0, &op1);

	  /* If STORE_FLAG_VALUE is 1, we can convert (ne x 0) to simply X
	     if only the low-order bit is possibly nonzero in X (such as when
	     X is a ZERO_EXTRACT of one bit).  Similarly, we can convert EQ to
	     (xor X 1) or (minus 1 X); we use the former.  Finally, if X is
	     known to be either 0 or -1, NE becomes a NEG and EQ becomes
	     (plus X 1).

	     Remove any ZERO_EXTRACT we made when thinking this was a
	     comparison.  It may now be simpler to use, e.g., an AND.  If a
	     ZERO_EXTRACT is indeed appropriate, it will be placed back by
	     the call to make_compound_operation in the SET case.

	     Don't apply these optimizations if the caller would
	     prefer a comparison rather than a value.
	     E.g., for the condition in an IF_THEN_ELSE most targets need
	     an explicit comparison.  */

	  if (in_cond)
	    ;

	  else if (STORE_FLAG_VALUE == 1
		   && new_code == NE
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && nonzero_bits (op0, int_mode) == 1)
	    return gen_lowpart (int_mode,
				expand_compound_operation (op0));

	  else if (STORE_FLAG_VALUE == 1
		   && new_code == NE
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && (num_sign_bit_copies (op0, int_mode)
		       == GET_MODE_PRECISION (int_mode)))
	    {
	      op0 = expand_compound_operation (op0);
	      return simplify_gen_unary (NEG, int_mode,
					 gen_lowpart (int_mode, op0),
					 int_mode);
	    }

	  else if (STORE_FLAG_VALUE == 1
		   && new_code == EQ
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && nonzero_bits (op0, int_mode) == 1)
	    {
	      op0 = expand_compound_operation (op0);
	      return simplify_gen_binary (XOR, int_mode,
					  gen_lowpart (int_mode, op0),
					  const1_rtx);
	    }

	  else if (STORE_FLAG_VALUE == 1
		   && new_code == EQ
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && (num_sign_bit_copies (op0, int_mode)
		       == GET_MODE_PRECISION (int_mode)))
	    {
	      op0 = expand_compound_operation (op0);
	      return plus_constant (int_mode, gen_lowpart (int_mode, op0), 1);
	    }

	  /* If STORE_FLAG_VALUE is -1, we have cases similar to
	     those above.  */
	  if (in_cond)
	    ;

	  else if (STORE_FLAG_VALUE == -1
		   && new_code == NE
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && (num_sign_bit_copies (op0, int_mode)
		       == GET_MODE_PRECISION (int_mode)))
	    return gen_lowpart (int_mode, expand_compound_operation (op0));

	  else if (STORE_FLAG_VALUE == -1
		   && new_code == NE
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && nonzero_bits (op0, int_mode) == 1)
	    {
	      op0 = expand_compound_operation (op0);
	      return simplify_gen_unary (NEG, int_mode,
					 gen_lowpart (int_mode, op0),
					 int_mode);
	    }

	  else if (STORE_FLAG_VALUE == -1
		   && new_code == EQ
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && (num_sign_bit_copies (op0, int_mode)
		       == GET_MODE_PRECISION (int_mode)))
	    {
	      op0 = expand_compound_operation (op0);
	      return simplify_gen_unary (NOT, int_mode,
					 gen_lowpart (int_mode, op0),
					 int_mode);
	    }

	  /* If X is 0/1, (eq X 0) is X-1.  */
	  else if (STORE_FLAG_VALUE == -1
		   && new_code == EQ
		   && is_int_mode (mode, &int_mode)
		   && op1 == const0_rtx
		   && int_mode == GET_MODE (op0)
		   && nonzero_bits (op0, int_mode) == 1)
	    {
	      op0 = expand_compound_operation (op0);
	      return plus_constant (int_mode, gen_lowpart (int_mode, op0), -1);
	    }

	  /* If STORE_FLAG_VALUE says to just test the sign bit and X has just
	     one bit that might be nonzero, we can convert (ne x 0) to
	     (ashift x c) where C puts the bit in the sign bit.  Remove any
	     AND with STORE_FLAG_VALUE when we are done, since we are only
	     going to test the sign bit.  */
	  if (new_code == NE
	      && is_int_mode (mode, &int_mode)
	      && HWI_COMPUTABLE_MODE_P (int_mode)
	      && val_signbit_p (int_mode, STORE_FLAG_VALUE)
	      && op1 == const0_rtx
	      && int_mode == GET_MODE (op0)
	      && (i = exact_log2 (nonzero_bits (op0, int_mode))) >= 0)
	    {
	      x = simplify_shift_const (NULL_RTX, ASHIFT, int_mode,
					expand_compound_operation (op0),
					GET_MODE_PRECISION (int_mode) - 1 - i);
	      if (GET_CODE (x) == AND && XEXP (x, 1) == const_true_rtx)
		return XEXP (x, 0);
	      else
		return x;
	    }

	  /* If the code changed, return a whole new comparison.
	     We also need to avoid using SUBST in cases where
	     simplify_comparison has widened a comparison with a CONST_INT,
	     since in that case the wider CONST_INT may fail the sanity
	     checks in do_SUBST.  */
	  if (new_code != code
	      || (CONST_INT_P (op1)
		  && GET_MODE (op0) != GET_MODE (XEXP (x, 0))
		  && GET_MODE (op0) != GET_MODE (XEXP (x, 1))))
	    return gen_rtx_fmt_ee (new_code, mode, op0, op1);

	  /* Otherwise, keep this operation, but maybe change its operands.
	     This also converts (ne (compare FOO BAR) 0) to (ne FOO BAR).  */
	  SUBST (XEXP (x, 0), op0);
	  SUBST (XEXP (x, 1), op1);
	}
      break;

    case IF_THEN_ELSE:
      return simplify_if_then_else (x);

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
      /* If we are processing SET_DEST, we are done.  */
      if (in_dest)
	return x;

      return expand_compound_operation (x);

    case SET:
      return simplify_set (x);

    case AND:
    case IOR:
      return simplify_logical (x);

    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATE:
    case ROTATERT:
      /* If this is a shift by a constant amount, simplify it.  */
      if (CONST_INT_P (XEXP (x, 1)))
	return simplify_shift_const (x, code, mode, XEXP (x, 0),
				     INTVAL (XEXP (x, 1)));

      else if (SHIFT_COUNT_TRUNCATED && !REG_P (XEXP (x, 1)))
	SUBST (XEXP (x, 1),
	       force_to_mode (XEXP (x, 1), GET_MODE (XEXP (x, 1)),
			      (HOST_WIDE_INT_1U
			       << exact_log2 (GET_MODE_UNIT_BITSIZE
					      (GET_MODE (x)))) - 1, false));
      break;
    case VEC_SELECT:
      {
	rtx trueop0 = XEXP (x, 0);
	mode = GET_MODE (trueop0);
	rtx trueop1 = XEXP (x, 1);
	/* If we select a low-part subreg, return that.  */
	if (vec_series_lowpart_p (GET_MODE (x), mode, trueop1))
	  {
	    rtx new_rtx = lowpart_subreg (GET_MODE (x), trueop0, mode);
	    if (new_rtx != NULL_RTX)
	      return new_rtx;
	  }
      }

    default:
      break;
    }

  return x;
}

/* Simplify X, an IF_THEN_ELSE expression.  Return the new expression.  */

static rtx
simplify_if_then_else (rtx x)
{
  machine_mode mode = GET_MODE (x);
  rtx cond = XEXP (x, 0);
  rtx true_rtx = XEXP (x, 1);
  rtx false_rtx = XEXP (x, 2);
  enum rtx_code true_code = GET_CODE (cond);
  bool comparison_p = COMPARISON_P (cond);
  rtx temp;
  int i;
  enum rtx_code false_code;
  rtx reversed;
  scalar_int_mode int_mode, inner_mode;

  /* Simplify storing of the truth value.  */
  if (comparison_p && true_rtx == const_true_rtx && false_rtx == const0_rtx)
    return simplify_gen_relational (true_code, mode, VOIDmode,
				    XEXP (cond, 0), XEXP (cond, 1));

  /* Also when the truth value has to be reversed.  */
  if (comparison_p
      && true_rtx == const0_rtx && false_rtx == const_true_rtx
      && (reversed = reversed_comparison (cond, mode)))
    return reversed;

  /* Sometimes we can simplify the arm of an IF_THEN_ELSE if a register used
     in it is being compared against certain values.  Get the true and false
     comparisons and see if that says anything about the value of each arm.  */

  if (comparison_p
      && ((false_code = reversed_comparison_code (cond, NULL))
	  != UNKNOWN)
      && REG_P (XEXP (cond, 0)))
    {
      HOST_WIDE_INT nzb;
      rtx from = XEXP (cond, 0);
      rtx true_val = XEXP (cond, 1);
      rtx false_val = true_val;
      bool swapped = false;

      /* If FALSE_CODE is EQ, swap the codes and arms.  */

      if (false_code == EQ)
	{
	  swapped = true, true_code = EQ, false_code = NE;
	  std::swap (true_rtx, false_rtx);
	}

      scalar_int_mode from_mode;
      if (is_a <scalar_int_mode> (GET_MODE (from), &from_mode))
	{
	  /* If we are comparing against zero and the expression being
	     tested has only a single bit that might be nonzero, that is
	     its value when it is not equal to zero.  Similarly if it is
	     known to be -1 or 0.  */
	  if (true_code == EQ
	      && true_val == const0_rtx
	      && pow2p_hwi (nzb = nonzero_bits (from, from_mode)))
	    {
	      false_code = EQ;
	      false_val = gen_int_mode (nzb, from_mode);
	    }
	  else if (true_code == EQ
		   && true_val == const0_rtx
		   && (num_sign_bit_copies (from, from_mode)
		       == GET_MODE_PRECISION (from_mode)))
	    {
	      false_code = EQ;
	      false_val = constm1_rtx;
	    }
	}

      /* Now simplify an arm if we know the value of the register in the
	 branch and it is used in the arm.  Be careful due to the potential
	 of locally-shared RTL.  */

      if (reg_mentioned_p (from, true_rtx))
	true_rtx = subst (known_cond (copy_rtx (true_rtx), true_code,
				      from, true_val),
			  pc_rtx, pc_rtx, false, false, false);
      if (reg_mentioned_p (from, false_rtx))
	false_rtx = subst (known_cond (copy_rtx (false_rtx), false_code,
				       from, false_val),
			   pc_rtx, pc_rtx, false, false, false);

      SUBST (XEXP (x, 1), swapped ? false_rtx : true_rtx);
      SUBST (XEXP (x, 2), swapped ? true_rtx : false_rtx);

      true_rtx = XEXP (x, 1);
      false_rtx = XEXP (x, 2);
      true_code = GET_CODE (cond);
    }

  /* If we have (if_then_else FOO (pc) (label_ref BAR)) and FOO can be
     reversed, do so to avoid needing two sets of patterns for
     subtract-and-branch insns.  Similarly if we have a constant in the true
     arm, the false arm is the same as the first operand of the comparison, or
     the false arm is more complicated than the true arm.  */

  if (comparison_p
      && reversed_comparison_code (cond, NULL) != UNKNOWN
      && (true_rtx == pc_rtx
	  || (CONSTANT_P (true_rtx)
	      && !CONST_INT_P (false_rtx) && false_rtx != pc_rtx)
	  || true_rtx == const0_rtx
	  || (OBJECT_P (true_rtx) && !OBJECT_P (false_rtx))
	  || (GET_CODE (true_rtx) == SUBREG && OBJECT_P (SUBREG_REG (true_rtx))
	      && !OBJECT_P (false_rtx))
	  || reg_mentioned_p (true_rtx, false_rtx)
	  || rtx_equal_p (false_rtx, XEXP (cond, 0))))
    {
      SUBST (XEXP (x, 0), reversed_comparison (cond, GET_MODE (cond)));
      SUBST (XEXP (x, 1), false_rtx);
      SUBST (XEXP (x, 2), true_rtx);

      std::swap (true_rtx, false_rtx);
      cond = XEXP (x, 0);

      /* It is possible that the conditional has been simplified out.  */
      true_code = GET_CODE (cond);
      comparison_p = COMPARISON_P (cond);
    }

  /* If the two arms are identical, we don't need the comparison.  */

  if (rtx_equal_p (true_rtx, false_rtx) && ! side_effects_p (cond))
    return true_rtx;

  /* Convert a == b ? b : a to "a".  */
  if (true_code == EQ && ! side_effects_p (cond)
      && !HONOR_NANS (mode)
      && rtx_equal_p (XEXP (cond, 0), false_rtx)
      && rtx_equal_p (XEXP (cond, 1), true_rtx))
    return false_rtx;
  else if (true_code == NE && ! side_effects_p (cond)
	   && !HONOR_NANS (mode)
	   && rtx_equal_p (XEXP (cond, 0), true_rtx)
	   && rtx_equal_p (XEXP (cond, 1), false_rtx))
    return true_rtx;

  /* Look for cases where we have (abs x) or (neg (abs X)).  */

  if (GET_MODE_CLASS (mode) == MODE_INT
      && comparison_p
      && XEXP (cond, 1) == const0_rtx
      && GET_CODE (false_rtx) == NEG
      && rtx_equal_p (true_rtx, XEXP (false_rtx, 0))
      && rtx_equal_p (true_rtx, XEXP (cond, 0))
      && ! side_effects_p (true_rtx))
    switch (true_code)
      {
      case GT:
      case GE:
	return simplify_gen_unary (ABS, mode, true_rtx, mode);
      case LT:
      case LE:
	return
	  simplify_gen_unary (NEG, mode,
			      simplify_gen_unary (ABS, mode, true_rtx, mode),
			      mode);
      default:
	break;
      }

  /* Look for MIN or MAX.  */

  if ((! FLOAT_MODE_P (mode)
       || (flag_unsafe_math_optimizations
	   && !HONOR_NANS (mode)
	   && !HONOR_SIGNED_ZEROS (mode)))
      && comparison_p
      && rtx_equal_p (XEXP (cond, 0), true_rtx)
      && rtx_equal_p (XEXP (cond, 1), false_rtx)
      && ! side_effects_p (cond))
    switch (true_code)
      {
      case GE:
      case GT:
	return simplify_gen_binary (SMAX, mode, true_rtx, false_rtx);
      case LE:
      case LT:
	return simplify_gen_binary (SMIN, mode, true_rtx, false_rtx);
      case GEU:
      case GTU:
	return simplify_gen_binary (UMAX, mode, true_rtx, false_rtx);
      case LEU:
      case LTU:
	return simplify_gen_binary (UMIN, mode, true_rtx, false_rtx);
      default:
	break;
      }

  /* If we have (if_then_else COND (OP Z C1) Z) and OP is an identity when its
     second operand is zero, this can be done as (OP Z (mult COND C2)) where
     C2 = C1 * STORE_FLAG_VALUE. Similarly if OP has an outer ZERO_EXTEND or
     SIGN_EXTEND as long as Z is already extended (so we don't destroy it).
     We can do this kind of thing in some cases when STORE_FLAG_VALUE is
     neither 1 or -1, but it isn't worth checking for.  */

  if ((STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
      && comparison_p
      && is_int_mode (mode, &int_mode)
      && ! side_effects_p (x))
    {
      rtx t = make_compound_operation (true_rtx, SET);
      rtx f = make_compound_operation (false_rtx, SET);
      rtx cond_op0 = XEXP (cond, 0);
      rtx cond_op1 = XEXP (cond, 1);
      enum rtx_code op = UNKNOWN, extend_op = UNKNOWN;
      scalar_int_mode m = int_mode;
      rtx z = 0, c1 = NULL_RTX;

      if ((GET_CODE (t) == PLUS || GET_CODE (t) == MINUS
	   || GET_CODE (t) == IOR || GET_CODE (t) == XOR
	   || GET_CODE (t) == ASHIFT
	   || GET_CODE (t) == LSHIFTRT || GET_CODE (t) == ASHIFTRT)
	  && rtx_equal_p (XEXP (t, 0), f))
	c1 = XEXP (t, 1), op = GET_CODE (t), z = f;

      /* If an identity-zero op is commutative, check whether there
	 would be a match if we swapped the operands.  */
      else if ((GET_CODE (t) == PLUS || GET_CODE (t) == IOR
		|| GET_CODE (t) == XOR)
	       && rtx_equal_p (XEXP (t, 1), f))
	c1 = XEXP (t, 0), op = GET_CODE (t), z = f;
      else if (GET_CODE (t) == SIGN_EXTEND
	       && is_a <scalar_int_mode> (GET_MODE (XEXP (t, 0)), &inner_mode)
	       && (GET_CODE (XEXP (t, 0)) == PLUS
		   || GET_CODE (XEXP (t, 0)) == MINUS
		   || GET_CODE (XEXP (t, 0)) == IOR
		   || GET_CODE (XEXP (t, 0)) == XOR
		   || GET_CODE (XEXP (t, 0)) == ASHIFT
		   || GET_CODE (XEXP (t, 0)) == LSHIFTRT
		   || GET_CODE (XEXP (t, 0)) == ASHIFTRT)
	       && GET_CODE (XEXP (XEXP (t, 0), 0)) == SUBREG
	       && subreg_lowpart_p (XEXP (XEXP (t, 0), 0))
	       && rtx_equal_p (SUBREG_REG (XEXP (XEXP (t, 0), 0)), f)
	       && (num_sign_bit_copies (f, GET_MODE (f))
		   > (unsigned int)
		     (GET_MODE_PRECISION (int_mode)
		      - GET_MODE_PRECISION (inner_mode))))
	{
	  c1 = XEXP (XEXP (t, 0), 1); z = f; op = GET_CODE (XEXP (t, 0));
	  extend_op = SIGN_EXTEND;
	  m = inner_mode;
	}
      else if (GET_CODE (t) == SIGN_EXTEND
	       && is_a <scalar_int_mode> (GET_MODE (XEXP (t, 0)), &inner_mode)
	       && (GET_CODE (XEXP (t, 0)) == PLUS
		   || GET_CODE (XEXP (t, 0)) == IOR
		   || GET_CODE (XEXP (t, 0)) == XOR)
	       && GET_CODE (XEXP (XEXP (t, 0), 1)) == SUBREG
	       && subreg_lowpart_p (XEXP (XEXP (t, 0), 1))
	       && rtx_equal_p (SUBREG_REG (XEXP (XEXP (t, 0), 1)), f)
	       && (num_sign_bit_copies (f, GET_MODE (f))
		   > (unsigned int)
		     (GET_MODE_PRECISION (int_mode)
		      - GET_MODE_PRECISION (inner_mode))))
	{
	  c1 = XEXP (XEXP (t, 0), 0); z = f; op = GET_CODE (XEXP (t, 0));
	  extend_op = SIGN_EXTEND;
	  m = inner_mode;
	}
      else if (GET_CODE (t) == ZERO_EXTEND
	       && is_a <scalar_int_mode> (GET_MODE (XEXP (t, 0)), &inner_mode)
	       && (GET_CODE (XEXP (t, 0)) == PLUS
		   || GET_CODE (XEXP (t, 0)) == MINUS
		   || GET_CODE (XEXP (t, 0)) == IOR
		   || GET_CODE (XEXP (t, 0)) == XOR
		   || GET_CODE (XEXP (t, 0)) == ASHIFT
		   || GET_CODE (XEXP (t, 0)) == LSHIFTRT
		   || GET_CODE (XEXP (t, 0)) == ASHIFTRT)
	       && GET_CODE (XEXP (XEXP (t, 0), 0)) == SUBREG
	       && HWI_COMPUTABLE_MODE_P (int_mode)
	       && subreg_lowpart_p (XEXP (XEXP (t, 0), 0))
	       && rtx_equal_p (SUBREG_REG (XEXP (XEXP (t, 0), 0)), f)
	       && ((nonzero_bits (f, GET_MODE (f))
		    & ~GET_MODE_MASK (inner_mode))
		   == 0))
	{
	  c1 = XEXP (XEXP (t, 0), 1); z = f; op = GET_CODE (XEXP (t, 0));
	  extend_op = ZERO_EXTEND;
	  m = inner_mode;
	}
      else if (GET_CODE (t) == ZERO_EXTEND
	       && is_a <scalar_int_mode> (GET_MODE (XEXP (t, 0)), &inner_mode)
	       && (GET_CODE (XEXP (t, 0)) == PLUS
		   || GET_CODE (XEXP (t, 0)) == IOR
		   || GET_CODE (XEXP (t, 0)) == XOR)
	       && GET_CODE (XEXP (XEXP (t, 0), 1)) == SUBREG
	       && HWI_COMPUTABLE_MODE_P (int_mode)
	       && subreg_lowpart_p (XEXP (XEXP (t, 0), 1))
	       && rtx_equal_p (SUBREG_REG (XEXP (XEXP (t, 0), 1)), f)
	       && ((nonzero_bits (f, GET_MODE (f))
		    & ~GET_MODE_MASK (inner_mode))
		   == 0))
	{
	  c1 = XEXP (XEXP (t, 0), 0); z = f; op = GET_CODE (XEXP (t, 0));
	  extend_op = ZERO_EXTEND;
	  m = inner_mode;
	}

      if (z)
	{
	  machine_mode cm = m;
	  if ((op == ASHIFT || op == LSHIFTRT || op == ASHIFTRT)
	      && GET_MODE (c1) != VOIDmode)
	    cm = GET_MODE (c1);
	  temp = subst (simplify_gen_relational (true_code, cm, VOIDmode,
						 cond_op0, cond_op1),
			pc_rtx, pc_rtx, false, false, false);
	  temp = simplify_gen_binary (MULT, cm, temp,
				      simplify_gen_binary (MULT, cm, c1,
							   const_true_rtx));
	  temp = subst (temp, pc_rtx, pc_rtx, false, false, false);
	  temp = simplify_gen_binary (op, m, gen_lowpart (m, z), temp);

	  if (extend_op != UNKNOWN)
	    temp = simplify_gen_unary (extend_op, int_mode, temp, m);

	  return temp;
	}
    }

  /* If we have (if_then_else (ne A 0) C1 0) and either A is known to be 0 or
     1 and C1 is a single bit or A is known to be 0 or -1 and C1 is the
     negation of a single bit, we can convert this operation to a shift.  We
     can actually do this more generally, but it doesn't seem worth it.  */

  if (true_code == NE
      && is_a <scalar_int_mode> (mode, &int_mode)
      && XEXP (cond, 1) == const0_rtx
      && false_rtx == const0_rtx
      && CONST_INT_P (true_rtx)
      && ((nonzero_bits (XEXP (cond, 0), int_mode) == 1
	   && (i = exact_log2 (UINTVAL (true_rtx))) >= 0)
	  || ((num_sign_bit_copies (XEXP (cond, 0), int_mode)
	       == GET_MODE_PRECISION (int_mode))
	      && (i = exact_log2 (-UINTVAL (true_rtx))) >= 0)))
    return
      simplify_shift_const (NULL_RTX, ASHIFT, int_mode,
			    gen_lowpart (int_mode, XEXP (cond, 0)), i);

  /* (IF_THEN_ELSE (NE A 0) C1 0) is A or a zero-extend of A if the only
     non-zero bit in A is C1.  */
  if (true_code == NE && XEXP (cond, 1) == const0_rtx
      && false_rtx == const0_rtx && CONST_INT_P (true_rtx)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && is_a <scalar_int_mode> (GET_MODE (XEXP (cond, 0)), &inner_mode)
      && (UINTVAL (true_rtx) & GET_MODE_MASK (int_mode))
	  == nonzero_bits (XEXP (cond, 0), inner_mode)
      && (i = exact_log2 (UINTVAL (true_rtx) & GET_MODE_MASK (int_mode))) >= 0)
    {
      rtx val = XEXP (cond, 0);
      if (inner_mode == int_mode)
        return val;
      else if (GET_MODE_PRECISION (inner_mode) < GET_MODE_PRECISION (int_mode))
        return simplify_gen_unary (ZERO_EXTEND, int_mode, val, inner_mode);
    }

  return x;
}

/* Simplify X, a SET expression.  Return the new expression.  */

static rtx
simplify_set (rtx x)
{
  rtx src = SET_SRC (x);
  rtx dest = SET_DEST (x);
  machine_mode mode
    = GET_MODE (src) != VOIDmode ? GET_MODE (src) : GET_MODE (dest);
  rtx_insn *other_insn;
  rtx *cc_use;
  scalar_int_mode int_mode;

  /* (set (pc) (return)) gets written as (return).  */
  if (GET_CODE (dest) == PC && ANY_RETURN_P (src))
    return src;

  /* Now that we know for sure which bits of SRC we are using, see if we can
     simplify the expression for the object knowing that we only need the
     low-order bits.  */

  if (GET_MODE_CLASS (mode) == MODE_INT && HWI_COMPUTABLE_MODE_P (mode))
    {
      src = force_to_mode (src, mode, HOST_WIDE_INT_M1U, false);
      SUBST (SET_SRC (x), src);
    }

  /* If the source is a COMPARE, look for the use of the comparison result
     and try to simplify it unless we already have used undobuf.other_insn.  */
  if ((GET_MODE_CLASS (mode) == MODE_CC || GET_CODE (src) == COMPARE)
      && (cc_use = find_single_use (dest, subst_insn, &other_insn)) != 0
      && (undobuf.other_insn == 0 || other_insn == undobuf.other_insn)
      && COMPARISON_P (*cc_use)
      && rtx_equal_p (XEXP (*cc_use, 0), dest))
    {
      enum rtx_code old_code = GET_CODE (*cc_use);
      enum rtx_code new_code;
      rtx op0, op1, tmp;
      bool other_changed = false;
      rtx inner_compare = NULL_RTX;
      machine_mode compare_mode = GET_MODE (dest);

      if (GET_CODE (src) == COMPARE)
	{
	  op0 = XEXP (src, 0), op1 = XEXP (src, 1);
	  if (GET_CODE (op0) == COMPARE && op1 == const0_rtx)
	    {
	      inner_compare = op0;
	      op0 = XEXP (inner_compare, 0), op1 = XEXP (inner_compare, 1);
	    }
	}
      else
	op0 = src, op1 = CONST0_RTX (GET_MODE (src));

      tmp = simplify_relational_operation (old_code, compare_mode, VOIDmode,
					   op0, op1);
      if (!tmp)
	new_code = old_code;
      else if (!CONSTANT_P (tmp))
	{
	  new_code = GET_CODE (tmp);
	  op0 = XEXP (tmp, 0);
	  op1 = XEXP (tmp, 1);
	}
      else
	{
	  rtx pat = PATTERN (other_insn);
	  undobuf.other_insn = other_insn;
	  SUBST (*cc_use, tmp);

	  /* Attempt to simplify CC user.  */
	  if (GET_CODE (pat) == SET)
	    {
	      rtx new_rtx = simplify_rtx (SET_SRC (pat));
	      if (new_rtx != NULL_RTX)
		SUBST (SET_SRC (pat), new_rtx);
	    }

	  /* Convert X into a no-op move.  */
	  SUBST (SET_DEST (x), pc_rtx);
	  SUBST (SET_SRC (x), pc_rtx);
	  return x;
	}

      /* Simplify our comparison, if possible.  */
      new_code = simplify_comparison (new_code, &op0, &op1);

#ifdef SELECT_CC_MODE
      /* If this machine has CC modes other than CCmode, check to see if we
	 need to use a different CC mode here.  */
      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC)
	compare_mode = GET_MODE (op0);
      else if (inner_compare
	       && GET_MODE_CLASS (GET_MODE (inner_compare)) == MODE_CC
	       && new_code == old_code
	       && op0 == XEXP (inner_compare, 0)
	       && op1 == XEXP (inner_compare, 1))
	compare_mode = GET_MODE (inner_compare);
      else
	compare_mode = SELECT_CC_MODE (new_code, op0, op1);

      /* If the mode changed, we have to change SET_DEST, the mode in the
	 compare, and the mode in the place SET_DEST is used.  If SET_DEST is
	 a hard register, just build new versions with the proper mode.  If it
	 is a pseudo, we lose unless it is only time we set the pseudo, in
	 which case we can safely change its mode.  */
      if (compare_mode != GET_MODE (dest))
	{
	  if (can_change_dest_mode (dest, 0, compare_mode))
	    {
	      unsigned int regno = REGNO (dest);
	      rtx new_dest;

	      if (regno < FIRST_PSEUDO_REGISTER)
		new_dest = gen_rtx_REG (compare_mode, regno);
	      else
		{
		  subst_mode (regno, compare_mode);
		  new_dest = regno_reg_rtx[regno];
		}

	      SUBST (SET_DEST (x), new_dest);
	      SUBST (XEXP (*cc_use, 0), new_dest);
	      other_changed = true;

	      dest = new_dest;
	    }
	}
#endif  /* SELECT_CC_MODE */

      /* If the code changed, we have to build a new comparison in
	 undobuf.other_insn.  */
      if (new_code != old_code)
	{
	  bool other_changed_previously = other_changed;
	  unsigned HOST_WIDE_INT mask;
	  rtx old_cc_use = *cc_use;

	  SUBST (*cc_use, gen_rtx_fmt_ee (new_code, GET_MODE (*cc_use),
					  dest, const0_rtx));
	  other_changed = true;

	  /* If the only change we made was to change an EQ into an NE or
	     vice versa, OP0 has only one bit that might be nonzero, and OP1
	     is zero, check if changing the user of the condition code will
	     produce a valid insn.  If it won't, we can keep the original code
	     in that insn by surrounding our operation with an XOR.  */

	  if (((old_code == NE && new_code == EQ)
	       || (old_code == EQ && new_code == NE))
	      && ! other_changed_previously && op1 == const0_rtx
	      && HWI_COMPUTABLE_MODE_P (GET_MODE (op0))
	      && pow2p_hwi (mask = nonzero_bits (op0, GET_MODE (op0))))
	    {
	      rtx pat = PATTERN (other_insn), note = 0;

	      if ((recog_for_combine (&pat, other_insn, &note) < 0
		   && ! check_asm_operands (pat)))
		{
		  *cc_use = old_cc_use;
		  other_changed = false;

		  op0 = simplify_gen_binary (XOR, GET_MODE (op0), op0,
					     gen_int_mode (mask,
							   GET_MODE (op0)));
		}
	    }
	}

      if (other_changed)
	undobuf.other_insn = other_insn;

      /* Don't generate a compare of a CC with 0, just use that CC.  */
      if (GET_MODE (op0) == compare_mode && op1 == const0_rtx)
	{
	  SUBST (SET_SRC (x), op0);
	  src = SET_SRC (x);
	}
      /* Otherwise, if we didn't previously have the same COMPARE we
	 want, create it from scratch.  */
      else if (GET_CODE (src) != COMPARE || GET_MODE (src) != compare_mode
	       || XEXP (src, 0) != op0 || XEXP (src, 1) != op1)
	{
	  SUBST (SET_SRC (x), gen_rtx_COMPARE (compare_mode, op0, op1));
	  src = SET_SRC (x);
	}
    }
  else
    {
      /* Get SET_SRC in a form where we have placed back any
	 compound expressions.  Then do the checks below.  */
      src = make_compound_operation (src, SET);
      SUBST (SET_SRC (x), src);
    }

  /* If we have (set x (subreg:m1 (op:m2 ...) 0)) with OP being some operation,
     and X being a REG or (subreg (reg)), we may be able to convert this to
     (set (subreg:m2 x) (op)).

     We can always do this if M1 is narrower than M2 because that means that
     we only care about the low bits of the result.

     However, on machines without WORD_REGISTER_OPERATIONS defined, we cannot
     perform a narrower operation than requested since the high-order bits will
     be undefined.  On machine where it is defined, this transformation is safe
     as long as M1 and M2 have the same number of words.  */

  if (GET_CODE (src) == SUBREG && subreg_lowpart_p (src)
      && !OBJECT_P (SUBREG_REG (src))
      && (known_equal_after_align_up
	  (GET_MODE_SIZE (GET_MODE (src)),
	   GET_MODE_SIZE (GET_MODE (SUBREG_REG (src))),
	   UNITS_PER_WORD))
      && (WORD_REGISTER_OPERATIONS || !paradoxical_subreg_p (src))
      && ! (REG_P (dest) && REGNO (dest) < FIRST_PSEUDO_REGISTER
	    && !REG_CAN_CHANGE_MODE_P (REGNO (dest),
				       GET_MODE (SUBREG_REG (src)),
				       GET_MODE (src)))
      && (REG_P (dest)
	  || (GET_CODE (dest) == SUBREG
	      && REG_P (SUBREG_REG (dest)))))
    {
      SUBST (SET_DEST (x),
	     gen_lowpart (GET_MODE (SUBREG_REG (src)),
				      dest));
      SUBST (SET_SRC (x), SUBREG_REG (src));

      src = SET_SRC (x), dest = SET_DEST (x);
    }

  /* If we have (set FOO (subreg:M (mem:N BAR) 0)) with M wider than N, this
     would require a paradoxical subreg.  Replace the subreg with a
     zero_extend to avoid the reload that would otherwise be required.
     Don't do this unless we have a scalar integer mode, otherwise the
     transformation is incorrect.  */

  enum rtx_code extend_op;
  if (paradoxical_subreg_p (src)
      && MEM_P (SUBREG_REG (src))
      && SCALAR_INT_MODE_P (GET_MODE (src))
      && (extend_op = load_extend_op (GET_MODE (SUBREG_REG (src)))) != UNKNOWN)
    {
      SUBST (SET_SRC (x),
	     gen_rtx_fmt_e (extend_op, GET_MODE (src), SUBREG_REG (src)));

      src = SET_SRC (x);
    }

  /* If we don't have a conditional move, SET_SRC is an IF_THEN_ELSE, and we
     are comparing an item known to be 0 or -1 against 0, use a logical
     operation instead. Check for one of the arms being an IOR of the other
     arm with some value.  We compute three terms to be IOR'ed together.  In
     practice, at most two will be nonzero.  Then we do the IOR's.  */

  if (GET_CODE (dest) != PC
      && GET_CODE (src) == IF_THEN_ELSE
      && is_int_mode (GET_MODE (src), &int_mode)
      && (GET_CODE (XEXP (src, 0)) == EQ || GET_CODE (XEXP (src, 0)) == NE)
      && XEXP (XEXP (src, 0), 1) == const0_rtx
      && int_mode == GET_MODE (XEXP (XEXP (src, 0), 0))
      && (!HAVE_conditional_move
	  || ! can_conditionally_move_p (int_mode))
      && (num_sign_bit_copies (XEXP (XEXP (src, 0), 0), int_mode)
	  == GET_MODE_PRECISION (int_mode))
      && ! side_effects_p (src))
    {
      rtx true_rtx = (GET_CODE (XEXP (src, 0)) == NE
		      ? XEXP (src, 1) : XEXP (src, 2));
      rtx false_rtx = (GET_CODE (XEXP (src, 0)) == NE
		   ? XEXP (src, 2) : XEXP (src, 1));
      rtx term1 = const0_rtx, term2, term3;

      if (GET_CODE (true_rtx) == IOR
	  && rtx_equal_p (XEXP (true_rtx, 0), false_rtx))
	term1 = false_rtx, true_rtx = XEXP (true_rtx, 1), false_rtx = const0_rtx;
      else if (GET_CODE (true_rtx) == IOR
	       && rtx_equal_p (XEXP (true_rtx, 1), false_rtx))
	term1 = false_rtx, true_rtx = XEXP (true_rtx, 0), false_rtx = const0_rtx;
      else if (GET_CODE (false_rtx) == IOR
	       && rtx_equal_p (XEXP (false_rtx, 0), true_rtx))
	term1 = true_rtx, false_rtx = XEXP (false_rtx, 1), true_rtx = const0_rtx;
      else if (GET_CODE (false_rtx) == IOR
	       && rtx_equal_p (XEXP (false_rtx, 1), true_rtx))
	term1 = true_rtx, false_rtx = XEXP (false_rtx, 0), true_rtx = const0_rtx;

      term2 = simplify_gen_binary (AND, int_mode,
				   XEXP (XEXP (src, 0), 0), true_rtx);
      term3 = simplify_gen_binary (AND, int_mode,
				   simplify_gen_unary (NOT, int_mode,
						       XEXP (XEXP (src, 0), 0),
						       int_mode),
				   false_rtx);

      SUBST (SET_SRC (x),
	     simplify_gen_binary (IOR, int_mode,
				  simplify_gen_binary (IOR, int_mode,
						       term1, term2),
				  term3));

      src = SET_SRC (x);
    }

  /* If either SRC or DEST is a CLOBBER of (const_int 0), make this
     whole thing fail.  */
  if (GET_CODE (src) == CLOBBER && XEXP (src, 0) == const0_rtx)
    return src;
  else if (GET_CODE (dest) == CLOBBER && XEXP (dest, 0) == const0_rtx)
    return dest;
  else
    /* Convert this into a field assignment operation, if possible.  */
    return make_field_assignment (x);
}

/* Simplify, X, and AND, IOR, or XOR operation, and return the simplified
   result.  */

static rtx
simplify_logical (rtx x)
{
  rtx op0 = XEXP (x, 0);
  rtx op1 = XEXP (x, 1);
  scalar_int_mode mode;

  switch (GET_CODE (x))
    {
    case AND:
      /* We can call simplify_and_const_int only if we don't lose
	 any (sign) bits when converting INTVAL (op1) to
	 "unsigned HOST_WIDE_INT".  */
      if (is_a <scalar_int_mode> (GET_MODE (x), &mode)
	  && CONST_INT_P (op1)
	  && (HWI_COMPUTABLE_MODE_P (mode)
	      || INTVAL (op1) > 0))
	{
	  x = simplify_and_const_int (x, mode, op0, INTVAL (op1));
	  if (GET_CODE (x) != AND)
	    return x;

	  op0 = XEXP (x, 0);
	  op1 = XEXP (x, 1);
	}

      /* If we have any of (and (ior A B) C) or (and (xor A B) C),
	 apply the distributive law and then the inverse distributive
	 law to see if things simplify.  */
      if (GET_CODE (op0) == IOR || GET_CODE (op0) == XOR)
	{
	  rtx result = distribute_and_simplify_rtx (x, 0);
	  if (result)
	    return result;
	}
      if (GET_CODE (op1) == IOR || GET_CODE (op1) == XOR)
	{
	  rtx result = distribute_and_simplify_rtx (x, 1);
	  if (result)
	    return result;
	}
      break;

    case IOR:
      /* If we have (ior (and A B) C), apply the distributive law and then
	 the inverse distributive law to see if things simplify.  */

      if (GET_CODE (op0) == AND)
	{
	  rtx result = distribute_and_simplify_rtx (x, 0);
	  if (result)
	    return result;
	}

      if (GET_CODE (op1) == AND)
	{
	  rtx result = distribute_and_simplify_rtx (x, 1);
	  if (result)
	    return result;
	}
      break;

    default:
      gcc_unreachable ();
    }

  return x;
}

/* We consider ZERO_EXTRACT, SIGN_EXTRACT, and SIGN_EXTEND as "compound
   operations" because they can be replaced with two more basic operations.
   ZERO_EXTEND is also considered "compound" because it can be replaced with
   an AND operation, which is simpler, though only one operation.

   The function expand_compound_operation is called with an rtx expression
   and will convert it to the appropriate shifts and AND operations,
   simplifying at each stage.

   The function make_compound_operation is called to convert an expression
   consisting of shifts and ANDs into the equivalent compound expression.
   It is the inverse of this function, loosely speaking.  */

static rtx
expand_compound_operation (rtx x)
{
  unsigned HOST_WIDE_INT pos = 0, len;
  bool unsignedp = false;
  unsigned int modewidth;
  rtx tem;
  scalar_int_mode inner_mode;

  switch (GET_CODE (x))
    {
    case ZERO_EXTEND:
      unsignedp = true;
      /* FALLTHRU */
    case SIGN_EXTEND:
      /* We can't necessarily use a const_int for a multiword mode;
	 it depends on implicitly extending the value.
	 Since we don't know the right way to extend it,
	 we can't tell whether the implicit way is right.

	 Even for a mode that is no wider than a const_int,
	 we can't win, because we need to sign extend one of its bits through
	 the rest of it, and we don't know which bit.  */
      if (CONST_INT_P (XEXP (x, 0)))
	return x;

      /* Reject modes that aren't scalar integers because turning vector
	 or complex modes into shifts causes problems.  */
      if (!is_a <scalar_int_mode> (GET_MODE (XEXP (x, 0)), &inner_mode))
	return x;

      /* Return if (subreg:MODE FROM 0) is not a safe replacement for
	 (zero_extend:MODE FROM) or (sign_extend:MODE FROM).  It is for any MEM
	 because (SUBREG (MEM...)) is guaranteed to cause the MEM to be
	 reloaded. If not for that, MEM's would very rarely be safe.

	 Reject modes bigger than a word, because we might not be able
	 to reference a two-register group starting with an arbitrary register
	 (and currently gen_lowpart might crash for a SUBREG).  */

      if (GET_MODE_SIZE (inner_mode) > UNITS_PER_WORD)
	return x;

      len = GET_MODE_PRECISION (inner_mode);
      /* If the inner object has VOIDmode (the only way this can happen
	 is if it is an ASM_OPERANDS), we can't do anything since we don't
	 know how much masking to do.  */
      if (len == 0)
	return x;

      break;

    case ZERO_EXTRACT:
      unsignedp = true;

      /* fall through */

    case SIGN_EXTRACT:
      /* If the operand is a CLOBBER, just return it.  */
      if (GET_CODE (XEXP (x, 0)) == CLOBBER)
	return XEXP (x, 0);

      if (!CONST_INT_P (XEXP (x, 1))
	  || !CONST_INT_P (XEXP (x, 2)))
	return x;

      /* Reject modes that aren't scalar integers because turning vector
	 or complex modes into shifts causes problems.  */
      if (!is_a <scalar_int_mode> (GET_MODE (XEXP (x, 0)), &inner_mode))
	return x;

      len = INTVAL (XEXP (x, 1));
      pos = INTVAL (XEXP (x, 2));

      /* This should stay within the object being extracted, fail otherwise.  */
      if (len + pos > GET_MODE_PRECISION (inner_mode))
	return x;

      if (BITS_BIG_ENDIAN)
	pos = GET_MODE_PRECISION (inner_mode) - len - pos;

      break;

    default:
      return x;
    }

  /* We've rejected non-scalar operations by now.  */
  scalar_int_mode mode = as_a <scalar_int_mode> (GET_MODE (x));

  /* Convert sign extension to zero extension, if we know that the high
     bit is not set, as this is easier to optimize.  It will be converted
     back to cheaper alternative in make_extraction.  */
  if (GET_CODE (x) == SIGN_EXTEND
      && HWI_COMPUTABLE_MODE_P (mode)
      && ((nonzero_bits (XEXP (x, 0), inner_mode)
	   & ~(((unsigned HOST_WIDE_INT) GET_MODE_MASK (inner_mode)) >> 1))
	  == 0))
    {
      rtx temp = gen_rtx_ZERO_EXTEND (mode, XEXP (x, 0));
      rtx temp2 = expand_compound_operation (temp);

      /* Make sure this is a profitable operation.  */
      if (set_src_cost (x, mode, optimize_this_for_speed_p)
          > set_src_cost (temp2, mode, optimize_this_for_speed_p))
       return temp2;
      else if (set_src_cost (x, mode, optimize_this_for_speed_p)
               > set_src_cost (temp, mode, optimize_this_for_speed_p))
       return temp;
      else
       return x;
    }

  /* We can optimize some special cases of ZERO_EXTEND.  */
  if (GET_CODE (x) == ZERO_EXTEND)
    {
      /* (zero_extend:DI (truncate:SI foo:DI)) is just foo:DI if we
	 know that the last value didn't have any inappropriate bits
	 set.  */
      if (GET_CODE (XEXP (x, 0)) == TRUNCATE
	  && GET_MODE (XEXP (XEXP (x, 0), 0)) == mode
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (XEXP (XEXP (x, 0), 0), mode)
	      & ~GET_MODE_MASK (inner_mode)) == 0)
	return XEXP (XEXP (x, 0), 0);

      /* Likewise for (zero_extend:DI (subreg:SI foo:DI 0)).  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && GET_MODE (SUBREG_REG (XEXP (x, 0))) == mode
	  && subreg_lowpart_p (XEXP (x, 0))
	  && HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (SUBREG_REG (XEXP (x, 0)), mode)
	      & ~GET_MODE_MASK (inner_mode)) == 0)
	return SUBREG_REG (XEXP (x, 0));

      /* (zero_extend:DI (truncate:SI foo:DI)) is just foo:DI when foo
	 is a comparison and STORE_FLAG_VALUE permits.  This is like
	 the first case, but it works even when MODE is larger
	 than HOST_WIDE_INT.  */
      if (GET_CODE (XEXP (x, 0)) == TRUNCATE
	  && GET_MODE (XEXP (XEXP (x, 0), 0)) == mode
	  && COMPARISON_P (XEXP (XEXP (x, 0), 0))
	  && GET_MODE_PRECISION (inner_mode) <= HOST_BITS_PER_WIDE_INT
	  && (STORE_FLAG_VALUE & ~GET_MODE_MASK (inner_mode)) == 0)
	return XEXP (XEXP (x, 0), 0);

      /* Likewise for (zero_extend:DI (subreg:SI foo:DI 0)).  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && GET_MODE (SUBREG_REG (XEXP (x, 0))) == mode
	  && subreg_lowpart_p (XEXP (x, 0))
	  && COMPARISON_P (SUBREG_REG (XEXP (x, 0)))
	  && GET_MODE_PRECISION (inner_mode) <= HOST_BITS_PER_WIDE_INT
	  && (STORE_FLAG_VALUE & ~GET_MODE_MASK (inner_mode)) == 0)
	return SUBREG_REG (XEXP (x, 0));

    }

  /* If we reach here, we want to return a pair of shifts.  The inner
     shift is a left shift of BITSIZE - POS - LEN bits.  The outer
     shift is a right shift of BITSIZE - LEN bits.  It is arithmetic or
     logical depending on the value of UNSIGNEDP.

     If this was a ZERO_EXTEND or ZERO_EXTRACT, this pair of shifts will be
     converted into an AND of a shift.

     We must check for the case where the left shift would have a negative
     count.  This can happen in a case like (x >> 31) & 255 on machines
     that can't shift by a constant.  On those machines, we would first
     combine the shift with the AND to produce a variable-position
     extraction.  Then the constant of 31 would be substituted in
     to produce such a position.  */

  modewidth = GET_MODE_PRECISION (mode);
  if (modewidth >= pos + len)
    {
      tem = gen_lowpart (mode, XEXP (x, 0));
      if (!tem || GET_CODE (tem) == CLOBBER)
	return x;
      tem = simplify_shift_const (NULL_RTX, ASHIFT, mode,
				  tem, modewidth - pos - len);
      tem = simplify_shift_const (NULL_RTX, unsignedp ? LSHIFTRT : ASHIFTRT,
				  mode, tem, modewidth - len);
    }
  else if (unsignedp && len < HOST_BITS_PER_WIDE_INT)
    {
      tem = simplify_shift_const (NULL_RTX, LSHIFTRT, inner_mode,
				  XEXP (x, 0), pos);
      tem = gen_lowpart (mode, tem);
      if (!tem || GET_CODE (tem) == CLOBBER)
	return x;
      tem = simplify_and_const_int (NULL_RTX, mode, tem,
				    (HOST_WIDE_INT_1U << len) - 1);
    }
  else
    /* Any other cases we can't handle.  */
    return x;

  /* If we couldn't do this for some reason, return the original
     expression.  */
  if (GET_CODE (tem) == CLOBBER)
    return x;

  return tem;
}

/* X is a SET which contains an assignment of one object into
   a part of another (such as a bit-field assignment, STRICT_LOW_PART,
   or certain SUBREGS). If possible, convert it into a series of
   logical operations.

   We half-heartedly support variable positions, but do not at all
   support variable lengths.  */

static const_rtx
expand_field_assignment (const_rtx x)
{
  rtx inner;
  rtx pos;			/* Always counts from low bit.  */
  int len, inner_len;
  rtx mask, cleared, masked;
  scalar_int_mode compute_mode;

  /* Loop until we find something we can't simplify.  */
  while (1)
    {
      if (GET_CODE (SET_DEST (x)) == STRICT_LOW_PART
	  && GET_CODE (XEXP (SET_DEST (x), 0)) == SUBREG)
	{
	  rtx x0 = XEXP (SET_DEST (x), 0);
	  if (!GET_MODE_PRECISION (GET_MODE (x0)).is_constant (&len))
	    break;
	  inner = SUBREG_REG (XEXP (SET_DEST (x), 0));
	  pos = gen_int_mode (subreg_lsb (XEXP (SET_DEST (x), 0)),
			      MAX_MODE_INT);
	}
      else if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	       && CONST_INT_P (XEXP (SET_DEST (x), 1)))
	{
	  inner = XEXP (SET_DEST (x), 0);
	  if (!GET_MODE_PRECISION (GET_MODE (inner)).is_constant (&inner_len))
	    break;

	  len = INTVAL (XEXP (SET_DEST (x), 1));
	  pos = XEXP (SET_DEST (x), 2);

	  /* A constant position should stay within the width of INNER.  */
	  if (CONST_INT_P (pos) && INTVAL (pos) + len > inner_len)
	    break;

	  if (BITS_BIG_ENDIAN)
	    {
	      if (CONST_INT_P (pos))
		pos = GEN_INT (inner_len - len - INTVAL (pos));
	      else if (GET_CODE (pos) == MINUS
		       && CONST_INT_P (XEXP (pos, 1))
		       && INTVAL (XEXP (pos, 1)) == inner_len - len)
		/* If position is ADJUST - X, new position is X.  */
		pos = XEXP (pos, 0);
	      else
		pos = simplify_gen_binary (MINUS, GET_MODE (pos),
					   gen_int_mode (inner_len - len,
							 GET_MODE (pos)),
					   pos);
	    }
	}

      /* If the destination is a subreg that overwrites the whole of the inner
	 register, we can move the subreg to the source.  */
      else if (GET_CODE (SET_DEST (x)) == SUBREG
	       /* We need SUBREGs to compute nonzero_bits properly.  */
	       && nonzero_sign_valid
	       && !read_modify_subreg_p (SET_DEST (x)))
	{
	  x = gen_rtx_SET (SUBREG_REG (SET_DEST (x)),
			   gen_lowpart
			   (GET_MODE (SUBREG_REG (SET_DEST (x))),
			    SET_SRC (x)));
	  continue;
	}
      else
	break;

      while (GET_CODE (inner) == SUBREG && subreg_lowpart_p (inner))
	inner = SUBREG_REG (inner);

      /* Don't attempt bitwise arithmetic on non scalar integer modes.  */
      if (!is_a <scalar_int_mode> (GET_MODE (inner), &compute_mode))
	{
	  /* Don't do anything for vector or complex integral types.  */
	  if (! FLOAT_MODE_P (GET_MODE (inner)))
	    break;

	  /* Try to find an integral mode to pun with.  */
	  if (!int_mode_for_size (GET_MODE_BITSIZE (GET_MODE (inner)), 0)
	      .exists (&compute_mode))
	    break;

	  inner = gen_lowpart (compute_mode, inner);
	}

      /* Compute a mask of LEN bits, if we can do this on the host machine.  */
      if (len >= HOST_BITS_PER_WIDE_INT)
	break;

      /* Don't try to compute in too wide unsupported modes.  */
      if (!targetm.scalar_mode_supported_p (compute_mode))
	break;

      /* Now compute the equivalent expression.  Make a copy of INNER
	 for the SET_DEST in case it is a MEM into which we will substitute;
	 we don't want shared RTL in that case.  */
      mask = gen_int_mode ((HOST_WIDE_INT_1U << len) - 1,
			   compute_mode);
      cleared = simplify_gen_binary (AND, compute_mode,
				     simplify_gen_unary (NOT, compute_mode,
				       simplify_gen_binary (ASHIFT,
							    compute_mode,
							    mask, pos),
				       compute_mode),
				     inner);
      masked = simplify_gen_binary (ASHIFT, compute_mode,
				    simplify_gen_binary (
				      AND, compute_mode,
				      gen_lowpart (compute_mode, SET_SRC (x)),
				      mask),
				    pos);

      x = gen_rtx_SET (copy_rtx (inner),
		       simplify_gen_binary (IOR, compute_mode,
					    cleared, masked));
    }

  return x;
}

/* Return an RTX for a reference to LEN bits of INNER.  If POS_RTX is nonzero,
   it is an RTX that represents the (variable) starting position; otherwise,
   POS is the (constant) starting bit position.  Both are counted from the LSB.

   UNSIGNEDP is true for an unsigned reference and zero for a signed one.

   IN_DEST is true if this is a reference in the destination of a SET.
   This is used when a ZERO_ or SIGN_EXTRACT isn't needed.  If nonzero,
   a STRICT_LOW_PART will be used, if zero, ZERO_EXTEND or SIGN_EXTEND will
   be used.

   IN_COMPARE is true if we are in a COMPARE.  This means that a
   ZERO_EXTRACT should be built even for bits starting at bit 0.

   MODE is the desired mode of the result (if IN_DEST == 0).

   The result is an RTX for the extraction or NULL_RTX if the target
   can't handle it.  */

static rtx
make_extraction (machine_mode mode, rtx inner, HOST_WIDE_INT pos,
		 rtx pos_rtx, unsigned HOST_WIDE_INT len, bool unsignedp,
		 bool in_dest, bool in_compare)
{
  /* This mode describes the size of the storage area
     to fetch the overall value from.  Within that, we
     ignore the POS lowest bits, etc.  */
  machine_mode is_mode = GET_MODE (inner);
  machine_mode inner_mode;
  scalar_int_mode wanted_inner_mode;
  scalar_int_mode wanted_inner_reg_mode = word_mode;
  scalar_int_mode pos_mode = word_mode;
  machine_mode extraction_mode = word_mode;
  rtx new_rtx = 0;
  rtx orig_pos_rtx = pos_rtx;
  HOST_WIDE_INT orig_pos;

  if (pos_rtx && CONST_INT_P (pos_rtx))
    pos = INTVAL (pos_rtx), pos_rtx = 0;

  if (GET_CODE (inner) == SUBREG
      && subreg_lowpart_p (inner)
      && (paradoxical_subreg_p (inner)
	  /* If trying or potentionally trying to extract
	     bits outside of is_mode, don't look through
	     non-paradoxical SUBREGs.  See PR82192.  */
	  || (pos_rtx == NULL_RTX
	      && known_le (pos + len, GET_MODE_PRECISION (is_mode)))))
    {
      /* If going from (subreg:SI (mem:QI ...)) to (mem:QI ...),
	 consider just the QI as the memory to extract from.
	 The subreg adds or removes high bits; its mode is
	 irrelevant to the meaning of this extraction,
	 since POS and LEN count from the lsb.  */
      if (MEM_P (SUBREG_REG (inner)))
	is_mode = GET_MODE (SUBREG_REG (inner));
      inner = SUBREG_REG (inner);
    }
  else if (GET_CODE (inner) == ASHIFT
	   && CONST_INT_P (XEXP (inner, 1))
	   && pos_rtx == 0 && pos == 0
	   && len > UINTVAL (XEXP (inner, 1)))
    {
      /* We're extracting the least significant bits of an rtx
	 (ashift X (const_int C)), where LEN > C.  Extract the
	 least significant (LEN - C) bits of X, giving an rtx
	 whose mode is MODE, then shift it left C times.  */
      new_rtx = make_extraction (mode, XEXP (inner, 0),
			     0, 0, len - INTVAL (XEXP (inner, 1)),
			     unsignedp, in_dest, in_compare);
      if (new_rtx != 0)
	return gen_rtx_ASHIFT (mode, new_rtx, XEXP (inner, 1));
    }
  else if (GET_CODE (inner) == MULT
	   && CONST_INT_P (XEXP (inner, 1))
	   && pos_rtx == 0 && pos == 0)
    {
      /* We're extracting the least significant bits of an rtx
	 (mult X (const_int 2^C)), where LEN > C.  Extract the
	 least significant (LEN - C) bits of X, giving an rtx
	 whose mode is MODE, then multiply it by 2^C.  */
      const HOST_WIDE_INT shift_amt = exact_log2 (INTVAL (XEXP (inner, 1)));
      if (IN_RANGE (shift_amt, 1, len - 1))
	{
	  new_rtx = make_extraction (mode, XEXP (inner, 0),
				     0, 0, len - shift_amt,
				     unsignedp, in_dest, in_compare);
	  if (new_rtx)
	    return gen_rtx_MULT (mode, new_rtx, XEXP (inner, 1));
	}
    }
  else if (GET_CODE (inner) == TRUNCATE
	   /* If trying or potentionally trying to extract
	      bits outside of is_mode, don't look through
	      TRUNCATE.  See PR82192.  */
	   && pos_rtx == NULL_RTX
	   && known_le (pos + len, GET_MODE_PRECISION (is_mode)))
    inner = XEXP (inner, 0);

  inner_mode = GET_MODE (inner);

  /* See if this can be done without an extraction.  We never can if the
     width of the field is not the same as that of some integer mode. For
     registers, we can only avoid the extraction if the position is at the
     low-order bit and this is either not in the destination or we have the
     appropriate STRICT_LOW_PART operation available.

     For MEM, we can avoid an extract if the field starts on an appropriate
     boundary and we can change the mode of the memory reference.  */

  scalar_int_mode tmode;
  if (int_mode_for_size (len, 1).exists (&tmode)
      && ((pos_rtx == 0 && (pos % BITS_PER_WORD) == 0
	   && !MEM_P (inner)
	   && (pos == 0 || REG_P (inner))
	   && (inner_mode == tmode
	       || !REG_P (inner)
	       || TRULY_NOOP_TRUNCATION_MODES_P (tmode, inner_mode)
	       || reg_truncated_to_mode (tmode, inner))
	   && (! in_dest
	       || (REG_P (inner)
		   && have_insn_for (STRICT_LOW_PART, tmode))))
	  || (MEM_P (inner) && pos_rtx == 0
	      && (pos
		  % (STRICT_ALIGNMENT ? GET_MODE_ALIGNMENT (tmode)
		     : BITS_PER_UNIT)) == 0
	      /* We can't do this if we are widening INNER_MODE (it
		 may not be aligned, for one thing).  */
	      && !paradoxical_subreg_p (tmode, inner_mode)
	      && known_le (pos + len, GET_MODE_PRECISION (is_mode))
	      && (inner_mode == tmode
		  || (! mode_dependent_address_p (XEXP (inner, 0),
						  MEM_ADDR_SPACE (inner))
		      && ! MEM_VOLATILE_P (inner))))))
    {
      /* If INNER is a MEM, make a new MEM that encompasses just the desired
	 field.  If the original and current mode are the same, we need not
	 adjust the offset.  Otherwise, we do if bytes big endian.

	 If INNER is not a MEM, get a piece consisting of just the field
	 of interest (in this case POS % BITS_PER_WORD must be 0).  */

      if (MEM_P (inner))
	{
	  poly_int64 offset;

	  /* POS counts from lsb, but make OFFSET count in memory order.  */
	  if (BYTES_BIG_ENDIAN)
	    offset = bits_to_bytes_round_down (GET_MODE_PRECISION (is_mode)
					       - len - pos);
	  else
	    offset = pos / BITS_PER_UNIT;

	  new_rtx = adjust_address_nv (inner, tmode, offset);
	}
      else if (REG_P (inner))
	{
	  if (tmode != inner_mode)
	    {
	      /* We can't call gen_lowpart in a DEST since we
		 always want a SUBREG (see below) and it would sometimes
		 return a new hard register.  */
	      if (pos || in_dest)
		{
		  poly_uint64 offset
		    = subreg_offset_from_lsb (tmode, inner_mode, pos);

		  /* Avoid creating invalid subregs, for example when
		     simplifying (x>>32)&255.  */
		  if (!validate_subreg (tmode, inner_mode, inner, offset))
		    return NULL_RTX;

		  new_rtx = gen_rtx_SUBREG (tmode, inner, offset);
		}
	      else
		new_rtx = gen_lowpart (tmode, inner);
	    }
	  else
	    new_rtx = inner;
	}
      else
	new_rtx = force_to_mode (inner, tmode,
				 len >= HOST_BITS_PER_WIDE_INT
				 ? HOST_WIDE_INT_M1U
				 : (HOST_WIDE_INT_1U << len) - 1, false);

      /* If this extraction is going into the destination of a SET,
	 make a STRICT_LOW_PART unless we made a MEM.  */

      if (in_dest)
	return (MEM_P (new_rtx) ? new_rtx
		: (GET_CODE (new_rtx) != SUBREG
		   ? gen_rtx_CLOBBER (tmode, const0_rtx)
		   : gen_rtx_STRICT_LOW_PART (VOIDmode, new_rtx)));

      if (mode == tmode)
	return new_rtx;

      if (CONST_SCALAR_INT_P (new_rtx))
	return simplify_unary_operation (unsignedp ? ZERO_EXTEND : SIGN_EXTEND,
					 mode, new_rtx, tmode);

      /* If we know that no extraneous bits are set, and that the high
	 bit is not set, convert the extraction to the cheaper of
	 sign and zero extension, that are equivalent in these cases.  */
      if (flag_expensive_optimizations
	  && (HWI_COMPUTABLE_MODE_P (tmode)
	      && ((nonzero_bits (new_rtx, tmode)
		   & ~(((unsigned HOST_WIDE_INT)GET_MODE_MASK (tmode)) >> 1))
		  == 0)))
	{
	  rtx temp = gen_rtx_ZERO_EXTEND (mode, new_rtx);
	  rtx temp1 = gen_rtx_SIGN_EXTEND (mode, new_rtx);

	  /* Prefer ZERO_EXTENSION, since it gives more information to
	     backends.  */
	  if (set_src_cost (temp, mode, optimize_this_for_speed_p)
	      <= set_src_cost (temp1, mode, optimize_this_for_speed_p))
	    return temp;
	  return temp1;
	}

      /* Otherwise, sign- or zero-extend unless we already are in the
	 proper mode.  */

      return (gen_rtx_fmt_e (unsignedp ? ZERO_EXTEND : SIGN_EXTEND,
			     mode, new_rtx));
    }

  /* Unless this is a COMPARE or we have a funny memory reference,
     don't do anything with zero-extending field extracts starting at
     the low-order bit since they are simple AND operations.  */
  if (pos_rtx == 0 && pos == 0 && ! in_dest
      && ! in_compare && unsignedp)
    return 0;

  /* Unless INNER is not MEM, reject this if we would be spanning bytes or
     if the position is not a constant and the length is not 1.  In all
     other cases, we would only be going outside our object in cases when
     an original shift would have been undefined.  */
  if (MEM_P (inner)
      && ((pos_rtx == 0 && maybe_gt (pos + len, GET_MODE_PRECISION (is_mode)))
	  || (pos_rtx != 0 && len != 1)))
    return 0;

  enum extraction_pattern pattern = (in_dest ? EP_insv
				     : unsignedp ? EP_extzv : EP_extv);

  /* If INNER is not from memory, we want it to have the mode of a register
     extraction pattern's structure operand, or word_mode if there is no
     such pattern.  The same applies to extraction_mode and pos_mode
     and their respective operands.

     For memory, assume that the desired extraction_mode and pos_mode
     are the same as for a register operation, since at present we don't
     have named patterns for aligned memory structures.  */
  class extraction_insn insn;
  unsigned int inner_size;
  if (GET_MODE_BITSIZE (inner_mode).is_constant (&inner_size)
      && get_best_reg_extraction_insn (&insn, pattern, inner_size, mode))
    {
      wanted_inner_reg_mode = insn.struct_mode.require ();
      pos_mode = insn.pos_mode;
      extraction_mode = insn.field_mode;
    }

  /* Never narrow an object, since that might not be safe.  */

  if (mode != VOIDmode
      && partial_subreg_p (extraction_mode, mode))
    extraction_mode = mode;

  /* Punt if len is too large for extraction_mode.  */
  if (maybe_gt (len, GET_MODE_PRECISION (extraction_mode)))
    return NULL_RTX;

  if (!MEM_P (inner))
    wanted_inner_mode = wanted_inner_reg_mode;
  else
    {
      /* Be careful not to go beyond the extracted object and maintain the
	 natural alignment of the memory.  */
      wanted_inner_mode = smallest_int_mode_for_size (len);
      while (pos % GET_MODE_BITSIZE (wanted_inner_mode) + len
	     > GET_MODE_BITSIZE (wanted_inner_mode))
	wanted_inner_mode = GET_MODE_WIDER_MODE (wanted_inner_mode).require ();
    }

  orig_pos = pos;

  if (BITS_BIG_ENDIAN)
    {
      /* POS is passed as if BITS_BIG_ENDIAN == 0, so we need to convert it to
	 BITS_BIG_ENDIAN style.  If position is constant, compute new
	 position.  Otherwise, build subtraction.
	 Note that POS is relative to the mode of the original argument.
	 If it's a MEM we need to recompute POS relative to that.
	 However, if we're extracting from (or inserting into) a register,
	 we want to recompute POS relative to wanted_inner_mode.  */
      int width;
      if (!MEM_P (inner))
	width = GET_MODE_BITSIZE (wanted_inner_mode);
      else if (!GET_MODE_BITSIZE (is_mode).is_constant (&width))
	return NULL_RTX;

      if (pos_rtx == 0)
	pos = width - len - pos;
      else
	pos_rtx
	  = gen_rtx_MINUS (GET_MODE (pos_rtx),
			   gen_int_mode (width - len, GET_MODE (pos_rtx)),
			   pos_rtx);
      /* POS may be less than 0 now, but we check for that below.
	 Note that it can only be less than 0 if !MEM_P (inner).  */
    }

  /* If INNER has a wider mode, and this is a constant extraction, try to
     make it smaller and adjust the byte to point to the byte containing
     the value.  */
  if (wanted_inner_mode != VOIDmode
      && inner_mode != wanted_inner_mode
      && ! pos_rtx
      && partial_subreg_p (wanted_inner_mode, is_mode)
      && MEM_P (inner)
      && ! mode_dependent_address_p (XEXP (inner, 0), MEM_ADDR_SPACE (inner))
      && ! MEM_VOLATILE_P (inner))
    {
      poly_int64 offset = 0;

      /* The computations below will be correct if the machine is big
	 endian in both bits and bytes or little endian in bits and bytes.
	 If it is mixed, we must adjust.  */

      /* If bytes are big endian and we had a paradoxical SUBREG, we must
	 adjust OFFSET to compensate.  */
      if (BYTES_BIG_ENDIAN
	  && paradoxical_subreg_p (is_mode, inner_mode))
	offset -= GET_MODE_SIZE (is_mode) - GET_MODE_SIZE (inner_mode);

      /* We can now move to the desired byte.  */
      offset += (pos / GET_MODE_BITSIZE (wanted_inner_mode))
		* GET_MODE_SIZE (wanted_inner_mode);
      pos %= GET_MODE_BITSIZE (wanted_inner_mode);

      if (BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN
	  && is_mode != wanted_inner_mode)
	offset = (GET_MODE_SIZE (is_mode)
		  - GET_MODE_SIZE (wanted_inner_mode) - offset);

      inner = adjust_address_nv (inner, wanted_inner_mode, offset);
    }

  /* If INNER is not memory, get it into the proper mode.  If we are changing
     its mode, POS must be a constant and smaller than the size of the new
     mode.  */
  else if (!MEM_P (inner))
    {
      /* On the LHS, don't create paradoxical subregs implicitely truncating
	 the register unless TARGET_TRULY_NOOP_TRUNCATION.  */
      if (in_dest
	  && !TRULY_NOOP_TRUNCATION_MODES_P (GET_MODE (inner),
					     wanted_inner_mode))
	return NULL_RTX;

      if (GET_MODE (inner) != wanted_inner_mode
	  && (pos_rtx != 0
	      || orig_pos + len > GET_MODE_BITSIZE (wanted_inner_mode)))
	return NULL_RTX;

      if (orig_pos < 0)
	return NULL_RTX;

      inner = force_to_mode (inner, wanted_inner_mode,
			     pos_rtx
			     || len + orig_pos >= HOST_BITS_PER_WIDE_INT
			     ? HOST_WIDE_INT_M1U
			     : (((HOST_WIDE_INT_1U << len) - 1)
				<< orig_pos), false);
    }

  /* Adjust mode of POS_RTX, if needed.  If we want a wider mode, we
     have to zero extend.  Otherwise, we can just use a SUBREG.

     We dealt with constant rtxes earlier, so pos_rtx cannot
     have VOIDmode at this point.  */
  if (pos_rtx != 0
      && (GET_MODE_SIZE (pos_mode)
	  > GET_MODE_SIZE (as_a <scalar_int_mode> (GET_MODE (pos_rtx)))))
    {
      rtx temp = simplify_gen_unary (ZERO_EXTEND, pos_mode, pos_rtx,
				     GET_MODE (pos_rtx));

      /* If we know that no extraneous bits are set, and that the high
	 bit is not set, convert extraction to cheaper one - either
	 SIGN_EXTENSION or ZERO_EXTENSION, that are equivalent in these
	 cases.  */
      if (flag_expensive_optimizations
	  && (HWI_COMPUTABLE_MODE_P (GET_MODE (pos_rtx))
	      && ((nonzero_bits (pos_rtx, GET_MODE (pos_rtx))
		   & ~(((unsigned HOST_WIDE_INT)
			GET_MODE_MASK (GET_MODE (pos_rtx)))
		       >> 1))
		  == 0)))
	{
	  rtx temp1 = simplify_gen_unary (SIGN_EXTEND, pos_mode, pos_rtx,
					  GET_MODE (pos_rtx));

	  /* Prefer ZERO_EXTENSION, since it gives more information to
	     backends.  */
	  if (set_src_cost (temp1, pos_mode, optimize_this_for_speed_p)
	      < set_src_cost (temp, pos_mode, optimize_this_for_speed_p))
	    temp = temp1;
	}
      pos_rtx = temp;
    }

  /* Make POS_RTX unless we already have it and it is correct.  If we don't
     have a POS_RTX but we do have an ORIG_POS_RTX, the latter must
     be a CONST_INT.  */
  if (pos_rtx == 0 && orig_pos_rtx != 0 && INTVAL (orig_pos_rtx) == pos)
    pos_rtx = orig_pos_rtx;

  else if (pos_rtx == 0)
    pos_rtx = GEN_INT (pos);

  /* Make the required operation.  See if we can use existing rtx.  */
  new_rtx = gen_rtx_fmt_eee (unsignedp ? ZERO_EXTRACT : SIGN_EXTRACT,
			 extraction_mode, inner, GEN_INT (len), pos_rtx);
  if (! in_dest)
    new_rtx = gen_lowpart (mode, new_rtx);

  return new_rtx;
}

/* See if X (of mode MODE) contains an ASHIFT of COUNT or more bits that
   can be commuted with any other operations in X.  Return X without
   that shift if so.  */

static rtx
extract_left_shift (scalar_int_mode mode, rtx x, int count)
{
  enum rtx_code code = GET_CODE (x);
  rtx tem;

  switch (code)
    {
    case ASHIFT:
      /* This is the shift itself.  If it is wide enough, we will return
	 either the value being shifted if the shift count is equal to
	 COUNT or a shift for the difference.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= count)
	return simplify_shift_const (NULL_RTX, ASHIFT, mode, XEXP (x, 0),
				     INTVAL (XEXP (x, 1)) - count);
      break;

    case NEG:  case NOT:
      if ((tem = extract_left_shift (mode, XEXP (x, 0), count)) != 0)
	return simplify_gen_unary (code, mode, tem, mode);

      break;

    case PLUS:  case IOR:  case XOR:  case AND:
      /* If we can safely shift this constant and we find the inner shift,
	 make a new operation.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && (UINTVAL (XEXP (x, 1))
	      & (((HOST_WIDE_INT_1U << count)) - 1)) == 0
	  && (tem = extract_left_shift (mode, XEXP (x, 0), count)) != 0)
	{
	  HOST_WIDE_INT val = INTVAL (XEXP (x, 1)) >> count;
	  return simplify_gen_binary (code, mode, tem,
				      gen_int_mode (val, mode));
	}
      break;

    default:
      break;
    }

  return 0;
}

/* Subroutine of make_compound_operation.  *X_PTR is the rtx at the current
   level of the expression and MODE is its mode.  IN_CODE is as for
   make_compound_operation.  *NEXT_CODE_PTR is the value of IN_CODE
   that should be used when recursing on operands of *X_PTR.

   There are two possible actions:

   - Return null.  This tells the caller to recurse on *X_PTR with IN_CODE
     equal to *NEXT_CODE_PTR, after which *X_PTR holds the final value.

   - Return a new rtx, which the caller returns directly.  */

static rtx
make_compound_operation_int (scalar_int_mode mode, rtx *x_ptr,
			     enum rtx_code in_code,
			     enum rtx_code *next_code_ptr)
{
  rtx x = *x_ptr;
  enum rtx_code next_code = *next_code_ptr;
  enum rtx_code code = GET_CODE (x);
  int mode_width = GET_MODE_PRECISION (mode);
  rtx rhs, lhs;
  rtx new_rtx = 0;
  int i;
  rtx tem;
  scalar_int_mode inner_mode;
  bool equality_comparison = false;

  if (in_code == EQ)
    {
      equality_comparison = true;
      in_code = COMPARE;
    }

  /* Process depending on the code of this operation.  If NEW is set
     nonzero, it will be returned.  */

  switch (code)
    {
    case ASHIFT:
      /* Convert shifts by constants into multiplications if inside
	 an address.  */
      if (in_code == MEM && CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT
	  && INTVAL (XEXP (x, 1)) >= 0)
	{
	  HOST_WIDE_INT count = INTVAL (XEXP (x, 1));
	  HOST_WIDE_INT multval = HOST_WIDE_INT_1 << count;

	  new_rtx = make_compound_operation (XEXP (x, 0), next_code);
	  if (GET_CODE (new_rtx) == NEG)
	    {
	      new_rtx = XEXP (new_rtx, 0);
	      multval = -multval;
	    }
	  multval = trunc_int_for_mode (multval, mode);
	  new_rtx = gen_rtx_MULT (mode, new_rtx, gen_int_mode (multval, mode));
	}
      break;

    case PLUS:
      lhs = XEXP (x, 0);
      rhs = XEXP (x, 1);
      lhs = make_compound_operation (lhs, next_code);
      rhs = make_compound_operation (rhs, next_code);
      if (GET_CODE (lhs) == MULT && GET_CODE (XEXP (lhs, 0)) == NEG)
	{
	  tem = simplify_gen_binary (MULT, mode, XEXP (XEXP (lhs, 0), 0),
				     XEXP (lhs, 1));
	  new_rtx = simplify_gen_binary (MINUS, mode, rhs, tem);
	}
      else if (GET_CODE (lhs) == MULT
	       && (CONST_INT_P (XEXP (lhs, 1)) && INTVAL (XEXP (lhs, 1)) < 0))
	{
	  tem = simplify_gen_binary (MULT, mode, XEXP (lhs, 0),
				     simplify_gen_unary (NEG, mode,
							 XEXP (lhs, 1),
							 mode));
	  new_rtx = simplify_gen_binary (MINUS, mode, rhs, tem);
	}
      else
	{
	  SUBST (XEXP (x, 0), lhs);
	  SUBST (XEXP (x, 1), rhs);
	}
      maybe_swap_commutative_operands (x);
      return x;

    case MINUS:
      lhs = XEXP (x, 0);
      rhs = XEXP (x, 1);
      lhs = make_compound_operation (lhs, next_code);
      rhs = make_compound_operation (rhs, next_code);
      if (GET_CODE (rhs) == MULT && GET_CODE (XEXP (rhs, 0)) == NEG)
	{
	  tem = simplify_gen_binary (MULT, mode, XEXP (XEXP (rhs, 0), 0),
				     XEXP (rhs, 1));
	  return simplify_gen_binary (PLUS, mode, tem, lhs);
	}
      else if (GET_CODE (rhs) == MULT
	       && (CONST_INT_P (XEXP (rhs, 1)) && INTVAL (XEXP (rhs, 1)) < 0))
	{
	  tem = simplify_gen_binary (MULT, mode, XEXP (rhs, 0),
				     simplify_gen_unary (NEG, mode,
							 XEXP (rhs, 1),
							 mode));
	  return simplify_gen_binary (PLUS, mode, tem, lhs);
	}
      else
	{
	  SUBST (XEXP (x, 0), lhs);
	  SUBST (XEXP (x, 1), rhs);
	  return x;
	}

    case AND:
      /* If the second operand is not a constant, we can't do anything
	 with it.  */
      if (!CONST_INT_P (XEXP (x, 1)))
	break;

      /* If the constant is a power of two minus one and the first operand
	 is a logical right shift, make an extraction.  */
      if (GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && (i = exact_log2 (UINTVAL (XEXP (x, 1)) + 1)) >= 0)
	{
	  new_rtx = make_compound_operation (XEXP (XEXP (x, 0), 0), next_code);
	  new_rtx = make_extraction (mode, new_rtx, 0, XEXP (XEXP (x, 0), 1),
				     i, true, false, in_code == COMPARE);
	}

      /* Same as previous, but for (subreg (lshiftrt ...)) in first op.  */
      else if (GET_CODE (XEXP (x, 0)) == SUBREG
	       && subreg_lowpart_p (XEXP (x, 0))
	       && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (XEXP (x, 0))),
					  &inner_mode)
	       && GET_CODE (SUBREG_REG (XEXP (x, 0))) == LSHIFTRT
	       && (i = exact_log2 (UINTVAL (XEXP (x, 1)) + 1)) >= 0)
	{
	  rtx inner_x0 = SUBREG_REG (XEXP (x, 0));
	  new_rtx = make_compound_operation (XEXP (inner_x0, 0), next_code);
	  new_rtx = make_extraction (inner_mode, new_rtx, 0,
				     XEXP (inner_x0, 1),
				     i, true, false, in_code == COMPARE);

	  /* If we narrowed the mode when dropping the subreg, then we lose.  */
	  if (GET_MODE_SIZE (inner_mode) < GET_MODE_SIZE (mode))
	    new_rtx = NULL;

	  /* If that didn't give anything, see if the AND simplifies on
	     its own.  */
	  if (!new_rtx && i >= 0)
	    {
	      new_rtx = make_compound_operation (XEXP (x, 0), next_code);
	      new_rtx = make_extraction (mode, new_rtx, 0, NULL_RTX, i,
					 true, false, in_code == COMPARE);
	    }
	}
      /* Same as previous, but for (xor/ior (lshiftrt...) (lshiftrt...)).  */
      else if ((GET_CODE (XEXP (x, 0)) == XOR
		|| GET_CODE (XEXP (x, 0)) == IOR)
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == LSHIFTRT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == LSHIFTRT
	       && (i = exact_log2 (UINTVAL (XEXP (x, 1)) + 1)) >= 0)
	{
	  /* Apply the distributive law, and then try to make extractions.  */
	  new_rtx = gen_rtx_fmt_ee (GET_CODE (XEXP (x, 0)), mode,
				    gen_rtx_AND (mode, XEXP (XEXP (x, 0), 0),
						 XEXP (x, 1)),
				    gen_rtx_AND (mode, XEXP (XEXP (x, 0), 1),
						 XEXP (x, 1)));
	  new_rtx = make_compound_operation (new_rtx, in_code);
	}

      /* If we are have (and (rotate X C) M) and C is larger than the number
	 of bits in M, this is an extraction.  */

      else if (GET_CODE (XEXP (x, 0)) == ROTATE
	       && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	       && (i = exact_log2 (UINTVAL (XEXP (x, 1)) + 1)) >= 0
	       && i <= INTVAL (XEXP (XEXP (x, 0), 1)))
	{
	  new_rtx = make_compound_operation (XEXP (XEXP (x, 0), 0), next_code);
	  new_rtx = make_extraction (mode, new_rtx,
				     (GET_MODE_PRECISION (mode)
				      - INTVAL (XEXP (XEXP (x, 0), 1))),
				     NULL_RTX, i, true, false,
				     in_code == COMPARE);
	}

      /* On machines without logical shifts, if the operand of the AND is
	 a logical shift and our mask turns off all the propagated sign
	 bits, we can replace the logical shift with an arithmetic shift.  */
      else if (GET_CODE (XEXP (x, 0)) == LSHIFTRT
	       && !have_insn_for (LSHIFTRT, mode)
	       && have_insn_for (ASHIFTRT, mode)
	       && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	       && INTVAL (XEXP (XEXP (x, 0), 1)) >= 0
	       && INTVAL (XEXP (XEXP (x, 0), 1)) < HOST_BITS_PER_WIDE_INT
	       && mode_width <= HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);

	  mask >>= INTVAL (XEXP (XEXP (x, 0), 1));
	  if ((INTVAL (XEXP (x, 1)) & ~mask) == 0)
	    SUBST (XEXP (x, 0),
		   gen_rtx_ASHIFTRT (mode,
				     make_compound_operation (XEXP (XEXP (x,
									  0),
								    0),
							      next_code),
				     XEXP (XEXP (x, 0), 1)));
	}

      /* If the constant is one less than a power of two, this might be
	 representable by an extraction even if no shift is present.
	 If it doesn't end up being a ZERO_EXTEND, we will ignore it unless
	 we are in a COMPARE.  */
      else if ((i = exact_log2 (UINTVAL (XEXP (x, 1)) + 1)) >= 0)
	new_rtx = make_extraction (mode,
				   make_compound_operation (XEXP (x, 0),
							    next_code),
				   0, NULL_RTX, i,
				   true, false, in_code == COMPARE);

      /* If we are in a comparison and this is an AND with a power of two,
	 convert this into the appropriate bit extract.  */
      else if (in_code == COMPARE
	       && (i = exact_log2 (UINTVAL (XEXP (x, 1)))) >= 0
	       && (equality_comparison || i < GET_MODE_PRECISION (mode) - 1))
	new_rtx = make_extraction (mode,
				   make_compound_operation (XEXP (x, 0),
							    next_code),
				   i, NULL_RTX, 1, true, false, true);

      /* If the one operand is a paradoxical subreg of a register or memory and
	 the constant (limited to the smaller mode) has only zero bits where
	 the sub expression has known zero bits, this can be expressed as
	 a zero_extend.  */
      else if (GET_CODE (XEXP (x, 0)) == SUBREG)
	{
	  rtx sub;

	  sub = XEXP (XEXP (x, 0), 0);
	  machine_mode sub_mode = GET_MODE (sub);
	  int sub_width;
	  if ((REG_P (sub) || MEM_P (sub))
	      && GET_MODE_PRECISION (sub_mode).is_constant (&sub_width)
	      && sub_width < mode_width)
	    {
	      unsigned HOST_WIDE_INT mode_mask = GET_MODE_MASK (sub_mode);
	      unsigned HOST_WIDE_INT mask;

	      /* original AND constant with all the known zero bits set */
	      mask = UINTVAL (XEXP (x, 1)) | (~nonzero_bits (sub, sub_mode));
	      if ((mask & mode_mask) == mode_mask)
		{
		  new_rtx = make_compound_operation (sub, next_code);
		  new_rtx = make_extraction (mode, new_rtx, 0, 0, sub_width,
					     true, false, in_code == COMPARE);
		}
	    }
	}

      break;

    case LSHIFTRT:
      /* If the sign bit is known to be zero, replace this with an
	 arithmetic shift.  */
      if (have_insn_for (ASHIFTRT, mode)
	  && ! have_insn_for (LSHIFTRT, mode)
	  && mode_width <= HOST_BITS_PER_WIDE_INT
	  && (nonzero_bits (XEXP (x, 0), mode) & (1 << (mode_width - 1))) == 0)
	{
	  new_rtx = gen_rtx_ASHIFTRT (mode,
				      make_compound_operation (XEXP (x, 0),
							       next_code),
				      XEXP (x, 1));
	  break;
	}

      /* fall through */

    case ASHIFTRT:
      lhs = XEXP (x, 0);
      rhs = XEXP (x, 1);

      /* If we have (ashiftrt (ashift foo C1) C2) with C2 >= C1,
	 this is a SIGN_EXTRACT.  */
      if (CONST_INT_P (rhs)
	  && GET_CODE (lhs) == ASHIFT
	  && CONST_INT_P (XEXP (lhs, 1))
	  && INTVAL (rhs) >= INTVAL (XEXP (lhs, 1))
	  && INTVAL (XEXP (lhs, 1)) >= 0
	  && INTVAL (rhs) < mode_width)
	{
	  new_rtx = make_compound_operation (XEXP (lhs, 0), next_code);
	  new_rtx = make_extraction (mode, new_rtx,
				     INTVAL (rhs) - INTVAL (XEXP (lhs, 1)),
				     NULL_RTX, mode_width - INTVAL (rhs),
				     code == LSHIFTRT, false,
				     in_code == COMPARE);
	  break;
	}

      /* See if we have operations between an ASHIFTRT and an ASHIFT.
	 If so, try to merge the shifts into a SIGN_EXTEND.  We could
	 also do this for some cases of SIGN_EXTRACT, but it doesn't
	 seem worth the effort; the case checked for occurs on Alpha.  */

      if (!OBJECT_P (lhs)
	  && ! (GET_CODE (lhs) == SUBREG
		&& (OBJECT_P (SUBREG_REG (lhs))))
	  && CONST_INT_P (rhs)
	  && INTVAL (rhs) >= 0
	  && INTVAL (rhs) < HOST_BITS_PER_WIDE_INT
	  && INTVAL (rhs) < mode_width
	  && (new_rtx = extract_left_shift (mode, lhs, INTVAL (rhs))) != 0)
	new_rtx = make_extraction (mode, make_compound_operation (new_rtx,
								  next_code),
				   0, NULL_RTX, mode_width - INTVAL (rhs),
				   code == LSHIFTRT, false, in_code == COMPARE);

      break;

    case SUBREG:
      /* Call ourselves recursively on the inner expression.  If we are
	 narrowing the object and it has a different RTL code from
	 what it originally did, do this SUBREG as a force_to_mode.  */
      {
	rtx inner = SUBREG_REG (x), simplified;
	enum rtx_code subreg_code = in_code;

	/* If the SUBREG is masking of a logical right shift,
	   make an extraction.  */
	if (GET_CODE (inner) == LSHIFTRT
	    && is_a <scalar_int_mode> (GET_MODE (inner), &inner_mode)
	    && GET_MODE_SIZE (mode) < GET_MODE_SIZE (inner_mode)
	    && CONST_INT_P (XEXP (inner, 1))
	    && UINTVAL (XEXP (inner, 1)) < GET_MODE_PRECISION (inner_mode)
	    && subreg_lowpart_p (x))
	  {
	    new_rtx = make_compound_operation (XEXP (inner, 0), next_code);
	    int width = GET_MODE_PRECISION (inner_mode)
			- INTVAL (XEXP (inner, 1));
	    if (width > mode_width)
	      width = mode_width;
	    new_rtx = make_extraction (mode, new_rtx, 0, XEXP (inner, 1),
				       width, true, false, in_code == COMPARE);
	    break;
	  }

	/* If in_code is COMPARE, it isn't always safe to pass it through
	   to the recursive make_compound_operation call.  */
	if (subreg_code == COMPARE
	    && (!subreg_lowpart_p (x)
		|| GET_CODE (inner) == SUBREG
		/* (subreg:SI (and:DI (reg:DI) (const_int 0x800000000)) 0)
		   is (const_int 0), rather than
		   (subreg:SI (lshiftrt:DI (reg:DI) (const_int 35)) 0).
		   Similarly (subreg:QI (and:SI (reg:SI) (const_int 0x80)) 0)
		   for non-equality comparisons against 0 is not equivalent
		   to (subreg:QI (lshiftrt:SI (reg:SI) (const_int 7)) 0).  */
		|| (GET_CODE (inner) == AND
		    && CONST_INT_P (XEXP (inner, 1))
		    && partial_subreg_p (x)
		    && exact_log2 (UINTVAL (XEXP (inner, 1)))
		       >= GET_MODE_BITSIZE (mode) - 1)))
	  subreg_code = SET;

	tem = make_compound_operation (inner, subreg_code);

	simplified
	  = simplify_subreg (mode, tem, GET_MODE (inner), SUBREG_BYTE (x));
	if (simplified)
	  tem = simplified;

	if (GET_CODE (tem) != GET_CODE (inner)
	    && partial_subreg_p (x)
	    && subreg_lowpart_p (x))
	  {
	    rtx newer
	      = force_to_mode (tem, mode, HOST_WIDE_INT_M1U, false);

	    /* If we have something other than a SUBREG, we might have
	       done an expansion, so rerun ourselves.  */
	    if (GET_CODE (newer) != SUBREG)
	      newer = make_compound_operation (newer, in_code);

	    /* force_to_mode can expand compounds.  If it just re-expanded
	       the compound, use gen_lowpart to convert to the desired
	       mode.  */
	    if (rtx_equal_p (newer, x)
		/* Likewise if it re-expanded the compound only partially.
		   This happens for SUBREG of ZERO_EXTRACT if they extract
		   the same number of bits.  */
		|| (GET_CODE (newer) == SUBREG
		    && (GET_CODE (SUBREG_REG (newer)) == LSHIFTRT
			|| GET_CODE (SUBREG_REG (newer)) == ASHIFTRT)
		    && GET_CODE (inner) == AND
		    && rtx_equal_p (SUBREG_REG (newer), XEXP (inner, 0))))
	      return gen_lowpart (GET_MODE (x), tem);

	    return newer;
	  }

	if (simplified)
	  return tem;
      }
      break;

    default:
      break;
    }

  if (new_rtx)
    *x_ptr = gen_lowpart (mode, new_rtx);
  *next_code_ptr = next_code;
  return NULL_RTX;
}

/* Look at the expression rooted at X.  Look for expressions
   equivalent to ZERO_EXTRACT, SIGN_EXTRACT, ZERO_EXTEND, SIGN_EXTEND.
   Form these expressions.

   Return the new rtx, usually just X.

   Also, for machines like the VAX that don't have logical shift insns,
   try to convert logical to arithmetic shift operations in cases where
   they are equivalent.  This undoes the canonicalizations to logical
   shifts done elsewhere.

   We try, as much as possible, to re-use rtl expressions to save memory.

   IN_CODE says what kind of expression we are processing.  Normally, it is
   SET.  In a memory address it is MEM.  When processing the arguments of
   a comparison or a COMPARE against zero, it is COMPARE, or EQ if more
   precisely it is an equality comparison against zero.  */

rtx
make_compound_operation (rtx x, enum rtx_code in_code)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;
  enum rtx_code next_code;
  rtx new_rtx, tem;

  /* Select the code to be used in recursive calls.  Once we are inside an
     address, we stay there.  If we have a comparison, set to COMPARE,
     but once inside, go back to our default of SET.  */

  next_code = (code == MEM ? MEM
	       : ((code == COMPARE || COMPARISON_P (x))
		  && XEXP (x, 1) == const0_rtx) ? COMPARE
	       : in_code == COMPARE || in_code == EQ ? SET : in_code);

  scalar_int_mode mode;
  if (is_a <scalar_int_mode> (GET_MODE (x), &mode))
    {
      rtx new_rtx = make_compound_operation_int (mode, &x, in_code,
						 &next_code);
      if (new_rtx)
	return new_rtx;
      code = GET_CODE (x);
    }

  /* Now recursively process each operand of this operation.  We need to
     handle ZERO_EXTEND specially so that we don't lose track of the
     inner mode.  */
  if (code == ZERO_EXTEND)
    {
      new_rtx = make_compound_operation (XEXP (x, 0), next_code);
      tem = simplify_const_unary_operation (ZERO_EXTEND, GET_MODE (x),
					    new_rtx, GET_MODE (XEXP (x, 0)));
      if (tem)
	return tem;
      SUBST (XEXP (x, 0), new_rtx);
      return x;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    if (fmt[i] == 'e')
      {
	new_rtx = make_compound_operation (XEXP (x, i), next_code);
	SUBST (XEXP (x, i), new_rtx);
      }
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	{
	  new_rtx = make_compound_operation (XVECEXP (x, i, j), next_code);
	  SUBST (XVECEXP (x, i, j), new_rtx);
	}

  maybe_swap_commutative_operands (x);
  return x;
}

/* Given M see if it is a value that would select a field of bits
   within an item, but not the entire word.  Return -1 if not.
   Otherwise, return the starting position of the field, where 0 is the
   low-order bit.

   *PLEN is set to the length of the field.  */

static int
get_pos_from_mask (unsigned HOST_WIDE_INT m, unsigned HOST_WIDE_INT *plen)
{
  /* Get the bit number of the first 1 bit from the right, -1 if none.  */
  int pos = m ? ctz_hwi (m) : -1;
  int len = 0;

  if (pos >= 0)
    /* Now shift off the low-order zero bits and see if we have a
       power of two minus 1.  */
    len = exact_log2 ((m >> pos) + 1);

  if (len <= 0)
    pos = -1;

  *plen = len;
  return pos;
}

/* If X refers to a register that equals REG in value, replace these
   references with REG.  */
static rtx
canon_reg_for_combine (rtx x, rtx reg)
{
  rtx op0, op1, op2;
  const char *fmt;
  int i;
  bool copied;

  enum rtx_code code = GET_CODE (x);
  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      op0 = canon_reg_for_combine (XEXP (x, 0), reg);
      if (op0 != XEXP (x, 0))
	return simplify_gen_unary (GET_CODE (x), GET_MODE (x), op0,
				   GET_MODE (reg));
      break;

    case RTX_BIN_ARITH:
    case RTX_COMM_ARITH:
      op0 = canon_reg_for_combine (XEXP (x, 0), reg);
      op1 = canon_reg_for_combine (XEXP (x, 1), reg);
      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	return simplify_gen_binary (GET_CODE (x), GET_MODE (x), op0, op1);
      break;

    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      op0 = canon_reg_for_combine (XEXP (x, 0), reg);
      op1 = canon_reg_for_combine (XEXP (x, 1), reg);
      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	return simplify_gen_relational (GET_CODE (x), GET_MODE (x),
					GET_MODE (op0), op0, op1);
      break;

    case RTX_TERNARY:
    case RTX_BITFIELD_OPS:
      op0 = canon_reg_for_combine (XEXP (x, 0), reg);
      op1 = canon_reg_for_combine (XEXP (x, 1), reg);
      op2 = canon_reg_for_combine (XEXP (x, 2), reg);
      if (op0 != XEXP (x, 0) || op1 != XEXP (x, 1) || op2 != XEXP (x, 2))
	return simplify_gen_ternary (GET_CODE (x), GET_MODE (x),
				     GET_MODE (op0), op0, op1, op2);
      /* FALLTHRU */

    case RTX_OBJ:
      if (REG_P (x))
	{
	  if (rtx_equal_p (get_last_value (reg), x)
	      || rtx_equal_p (reg, get_last_value (x)))
	    return reg;
	  else
	    break;
	}

      /* fall through */

    default:
      fmt = GET_RTX_FORMAT (code);
      copied = false;
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	if (fmt[i] == 'e')
	  {
	    rtx op = canon_reg_for_combine (XEXP (x, i), reg);
	    if (op != XEXP (x, i))
	      {
		if (!copied)
		  {
		    copied = true;
		    x = copy_rtx (x);
		  }
		XEXP (x, i) = op;
	      }
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      {
		rtx op = canon_reg_for_combine (XVECEXP (x, i, j), reg);
		if (op != XVECEXP (x, i, j))
		  {
		    if (!copied)
		      {
			copied = true;
			x = copy_rtx (x);
		      }
		    XVECEXP (x, i, j) = op;
		  }
	      }
	  }

      break;
    }

  return x;
}

/* Return X converted to MODE.  If the value is already truncated to
   MODE we can just return a subreg even though in the general case we
   would need an explicit truncation.  */

static rtx
gen_lowpart_or_truncate (machine_mode mode, rtx x)
{
  if (!CONST_INT_P (x)
      && partial_subreg_p (mode, GET_MODE (x))
      && !TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (x))
      && !(REG_P (x) && reg_truncated_to_mode (mode, x)))
    {
      /* Bit-cast X into an integer mode.  */
      if (!SCALAR_INT_MODE_P (GET_MODE (x)))
	x = gen_lowpart (int_mode_for_mode (GET_MODE (x)).require (), x);
      x = simplify_gen_unary (TRUNCATE, int_mode_for_mode (mode).require (),
			      x, GET_MODE (x));
    }

  return gen_lowpart (mode, x);
}

/* See if X can be simplified knowing that we will only refer to it in
   MODE and will only refer to those bits that are nonzero in MASK.
   If other bits are being computed or if masking operations are done
   that select a superset of the bits in MASK, they can sometimes be
   ignored.

   Return a possibly simplified expression, but always convert X to
   MODE.  If X is a CONST_INT, AND the CONST_INT with MASK.

   If JUST_SELECT is true, don't optimize by noticing that bits in MASK
   are all off in X.  This is used when X will be complemented, by either
   NOT, NEG, or XOR.  */

static rtx
force_to_mode (rtx x, machine_mode mode, unsigned HOST_WIDE_INT mask,
	       bool just_select)
{
  enum rtx_code code = GET_CODE (x);
  bool next_select = just_select || code == XOR || code == NOT || code == NEG;
  machine_mode op_mode;
  unsigned HOST_WIDE_INT nonzero;

  /* If this is a CALL or ASM_OPERANDS, don't do anything.  Some of the
     code below will do the wrong thing since the mode of such an
     expression is VOIDmode.

     Also do nothing if X is a CLOBBER; this can happen if X was
     the return value from a call to gen_lowpart.  */
  if (code == CALL || code == ASM_OPERANDS || code == CLOBBER)
    return x;

  /* We want to perform the operation in its present mode unless we know
     that the operation is valid in MODE, in which case we do the operation
     in MODE.  */
  op_mode = ((GET_MODE_CLASS (mode) == GET_MODE_CLASS (GET_MODE (x))
	      && have_insn_for (code, mode))
	     ? mode : GET_MODE (x));

  /* It is not valid to do a right-shift in a narrower mode
     than the one it came in with.  */
  if ((code == LSHIFTRT || code == ASHIFTRT)
      && partial_subreg_p (mode, GET_MODE (x)))
    op_mode = GET_MODE (x);

  /* Truncate MASK to fit OP_MODE.  */
  if (op_mode)
    mask &= GET_MODE_MASK (op_mode);

  /* Determine what bits of X are guaranteed to be (non)zero.  */
  nonzero = nonzero_bits (x, mode);

  /* If none of the bits in X are needed, return a zero.  */
  if (!just_select && (nonzero & mask) == 0 && !side_effects_p (x))
    x = const0_rtx;

  /* If X is a CONST_INT, return a new one.  Do this here since the
     test below will fail.  */
  if (CONST_INT_P (x))
    {
      if (SCALAR_INT_MODE_P (mode))
	return gen_int_mode (INTVAL (x) & mask, mode);
      else
	{
	  x = GEN_INT (INTVAL (x) & mask);
	  return gen_lowpart_common (mode, x);
	}
    }

  /* If X is narrower than MODE and we want all the bits in X's mode, just
     get X in the proper mode.  */
  if (paradoxical_subreg_p (mode, GET_MODE (x))
      && (GET_MODE_MASK (GET_MODE (x)) & ~mask) == 0)
    return gen_lowpart (mode, x);

  /* We can ignore the effect of a SUBREG if it narrows the mode or
     if the constant masks to zero all the bits the mode doesn't have.  */
  if (GET_CODE (x) == SUBREG
      && subreg_lowpart_p (x)
      && (partial_subreg_p (x)
	  || (mask
	      & GET_MODE_MASK (GET_MODE (x))
	      & ~GET_MODE_MASK (GET_MODE (SUBREG_REG (x)))) == 0))
    return force_to_mode (SUBREG_REG (x), mode, mask, next_select);

  scalar_int_mode int_mode, xmode;
  if (is_a <scalar_int_mode> (mode, &int_mode)
      && is_a <scalar_int_mode> (GET_MODE (x), &xmode))
    /* OP_MODE is either MODE or XMODE, so it must be a scalar
       integer too.  */
    return force_int_to_mode (x, int_mode, xmode,
			      as_a <scalar_int_mode> (op_mode),
			      mask, just_select);

  return gen_lowpart_or_truncate (mode, x);
}

/* Subroutine of force_to_mode that handles cases in which both X and
   the result are scalar integers.  MODE is the mode of the result,
   XMODE is the mode of X, and OP_MODE says which of MODE or XMODE
   is preferred for simplified versions of X.  The other arguments
   are as for force_to_mode.  */

static rtx
force_int_to_mode (rtx x, scalar_int_mode mode, scalar_int_mode xmode,
		   scalar_int_mode op_mode, unsigned HOST_WIDE_INT mask,
		   bool just_select)
{
  enum rtx_code code = GET_CODE (x);
  bool next_select = just_select || code == XOR || code == NOT || code == NEG;
  unsigned HOST_WIDE_INT fuller_mask;
  rtx op0, op1, temp;
  poly_int64 const_op0;

  /* When we have an arithmetic operation, or a shift whose count we
     do not know, we need to assume that all bits up to the highest-order
     bit in MASK will be needed.  This is how we form such a mask.  */
  if (mask & (HOST_WIDE_INT_1U << (HOST_BITS_PER_WIDE_INT - 1)))
    fuller_mask = HOST_WIDE_INT_M1U;
  else
    fuller_mask = ((HOST_WIDE_INT_1U << (floor_log2 (mask) + 1)) - 1);

  switch (code)
    {
    case CLOBBER:
      /* If X is a (clobber (const_int)), return it since we know we are
	 generating something that won't match.  */
      return x;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      x = expand_compound_operation (x);
      if (GET_CODE (x) != code)
	return force_to_mode (x, mode, mask, next_select);
      break;

    case TRUNCATE:
      /* Similarly for a truncate.  */
      return force_to_mode (XEXP (x, 0), mode, mask, next_select);

    case AND:
      /* If this is an AND with a constant, convert it into an AND
	 whose constant is the AND of that constant with MASK.  If it
	 remains an AND of MASK, delete it since it is redundant.  */

      if (CONST_INT_P (XEXP (x, 1)))
	{
	  x = simplify_and_const_int (x, op_mode, XEXP (x, 0),
				      mask & INTVAL (XEXP (x, 1)));
	  xmode = op_mode;

	  /* If X is still an AND, see if it is an AND with a mask that
	     is just some low-order bits.  If so, and it is MASK, we don't
	     need it.  */

	  if (GET_CODE (x) == AND && CONST_INT_P (XEXP (x, 1))
	      && (INTVAL (XEXP (x, 1)) & GET_MODE_MASK (xmode)) == mask)
	    x = XEXP (x, 0);

	  /* If it remains an AND, try making another AND with the bits
	     in the mode mask that aren't in MASK turned on.  If the
	     constant in the AND is wide enough, this might make a
	     cheaper constant.  */

	  if (GET_CODE (x) == AND && CONST_INT_P (XEXP (x, 1))
	      && GET_MODE_MASK (xmode) != mask
	      && HWI_COMPUTABLE_MODE_P (xmode))
	    {
	      unsigned HOST_WIDE_INT cval
		= UINTVAL (XEXP (x, 1)) | (GET_MODE_MASK (xmode) & ~mask);
	      rtx y;

	      y = simplify_gen_binary (AND, xmode, XEXP (x, 0),
				       gen_int_mode (cval, xmode));
	      if (set_src_cost (y, xmode, optimize_this_for_speed_p)
		  < set_src_cost (x, xmode, optimize_this_for_speed_p))
		x = y;
	    }

	  break;
	}

      goto binop;

    case PLUS:
      /* In (and (plus FOO C1) M), if M is a mask that just turns off
	 low-order bits (as in an alignment operation) and FOO is already
	 aligned to that boundary, mask C1 to that boundary as well.
	 This may eliminate that PLUS and, later, the AND.  */

      {
	unsigned int width = GET_MODE_PRECISION (mode);
	unsigned HOST_WIDE_INT smask = mask;

	/* If MODE is narrower than HOST_WIDE_INT and mask is a negative
	   number, sign extend it.  */

	if (width < HOST_BITS_PER_WIDE_INT
	    && (smask & (HOST_WIDE_INT_1U << (width - 1))) != 0)
	  smask |= HOST_WIDE_INT_M1U << width;

	if (CONST_INT_P (XEXP (x, 1))
	    && pow2p_hwi (- smask)
	    && (nonzero_bits (XEXP (x, 0), mode) & ~smask) == 0
	    && (INTVAL (XEXP (x, 1)) & ~smask) != 0)
	  return force_to_mode (plus_constant (xmode, XEXP (x, 0),
					       (INTVAL (XEXP (x, 1)) & smask)),
				mode, smask, next_select);
      }

      /* fall through */

    case MULT:
      /* Substituting into the operands of a widening MULT is not likely to
	 create RTL matching a machine insn.  */
      if (code == MULT
	  && (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	  && (GET_CODE (XEXP (x, 1)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 1)) == SIGN_EXTEND)
	  && REG_P (XEXP (XEXP (x, 0), 0))
	  && REG_P (XEXP (XEXP (x, 1), 0)))
	return gen_lowpart_or_truncate (mode, x);

      /* For PLUS, MINUS and MULT, we need any bits less significant than the
	 most significant bit in MASK since carries from those bits will
	 affect the bits we are interested in.  */
      mask = fuller_mask;
      goto binop;

    case MINUS:
      /* If X is (minus C Y) where C's least set bit is larger than any bit
	 in the mask, then we may replace with (neg Y).  */
      if (poly_int_rtx_p (XEXP (x, 0), &const_op0)
	  && known_alignment (poly_uint64 (const_op0)) > mask)
	{
	  x = simplify_gen_unary (NEG, xmode, XEXP (x, 1), xmode);
	  return force_to_mode (x, mode, mask, next_select);
	}

      /* Similarly, if C contains every bit in the fuller_mask, then we may
	 replace with (not Y).  */
      if (CONST_INT_P (XEXP (x, 0))
	  && ((UINTVAL (XEXP (x, 0)) | fuller_mask) == UINTVAL (XEXP (x, 0))))
	{
	  x = simplify_gen_unary (NOT, xmode, XEXP (x, 1), xmode);
	  return force_to_mode (x, mode, mask, next_select);
	}

      mask = fuller_mask;
      goto binop;

    case IOR:
    case XOR:
      /* If X is (ior (lshiftrt FOO C1) C2), try to commute the IOR and
	 LSHIFTRT so we end up with an (and (lshiftrt (ior ...) ...) ...)
	 operation which may be a bitfield extraction.  Ensure that the
	 constant we form is not wider than the mode of X.  */

      if (GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && INTVAL (XEXP (XEXP (x, 0), 1)) >= 0
	  && INTVAL (XEXP (XEXP (x, 0), 1)) < HOST_BITS_PER_WIDE_INT
	  && CONST_INT_P (XEXP (x, 1))
	  && ((INTVAL (XEXP (XEXP (x, 0), 1))
	       + floor_log2 (INTVAL (XEXP (x, 1))))
	      < GET_MODE_PRECISION (xmode))
	  && (UINTVAL (XEXP (x, 1))
	      & ~nonzero_bits (XEXP (x, 0), xmode)) == 0)
	{
	  temp = gen_int_mode ((INTVAL (XEXP (x, 1)) & mask)
			       << INTVAL (XEXP (XEXP (x, 0), 1)),
			       xmode);
	  temp = simplify_gen_binary (GET_CODE (x), xmode,
				      XEXP (XEXP (x, 0), 0), temp);
	  x = simplify_gen_binary (LSHIFTRT, xmode, temp,
				   XEXP (XEXP (x, 0), 1));
	  return force_to_mode (x, mode, mask, next_select);
	}

    binop:
      /* For most binary operations, just propagate into the operation and
	 change the mode if we have an operation of that mode.  */

      op0 = force_to_mode (XEXP (x, 0), mode, mask, next_select);
      op1 = force_to_mode (XEXP (x, 1), mode, mask, next_select);

      /* If we ended up truncating both operands, truncate the result of the
	 operation instead.  */
      if (GET_CODE (op0) == TRUNCATE
	  && GET_CODE (op1) == TRUNCATE)
	{
	  op0 = XEXP (op0, 0);
	  op1 = XEXP (op1, 0);
	}

      op0 = gen_lowpart_or_truncate (op_mode, op0);
      op1 = gen_lowpart_or_truncate (op_mode, op1);

      if (op_mode != xmode || op0 != XEXP (x, 0) || op1 != XEXP (x, 1))
	{
	  x = simplify_gen_binary (code, op_mode, op0, op1);
	  xmode = op_mode;
	}
      break;

    case ASHIFT:
      /* For left shifts, do the same, but just for the first operand.
	 However, we cannot do anything with shifts where we cannot
	 guarantee that the counts are smaller than the size of the mode
	 because such a count will have a different meaning in a
	 wider mode.  */

      if (! (CONST_INT_P (XEXP (x, 1))
	     && INTVAL (XEXP (x, 1)) >= 0
	     && INTVAL (XEXP (x, 1)) < GET_MODE_PRECISION (mode))
	  && ! (GET_MODE (XEXP (x, 1)) != VOIDmode
		&& (nonzero_bits (XEXP (x, 1), GET_MODE (XEXP (x, 1)))
		    < (unsigned HOST_WIDE_INT) GET_MODE_PRECISION (mode))))
	break;

      /* If the shift count is a constant and we can do arithmetic in
	 the mode of the shift, refine which bits we need.  Otherwise, use the
	 conservative form of the mask.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < GET_MODE_PRECISION (op_mode)
	  && HWI_COMPUTABLE_MODE_P (op_mode))
	mask >>= INTVAL (XEXP (x, 1));
      else
	mask = fuller_mask;

      op0 = gen_lowpart_or_truncate (op_mode,
				     force_to_mode (XEXP (x, 0), mode,
						    mask, next_select));

      if (op_mode != xmode || op0 != XEXP (x, 0))
	{
	  x = simplify_gen_binary (code, op_mode, op0, XEXP (x, 1));
	  xmode = op_mode;
	}
      break;

    case LSHIFTRT:
      /* Here we can only do something if the shift count is a constant,
	 this shift constant is valid for the host, and we can do arithmetic
	 in OP_MODE.  */

      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT
	  && HWI_COMPUTABLE_MODE_P (op_mode))
	{
	  rtx inner = XEXP (x, 0);
	  unsigned HOST_WIDE_INT inner_mask;

	  /* Select the mask of the bits we need for the shift operand.  */
	  inner_mask = mask << INTVAL (XEXP (x, 1));

	  /* We can only change the mode of the shift if we can do arithmetic
	     in the mode of the shift and INNER_MASK is no wider than the
	     width of X's mode.  */
	  if ((inner_mask & ~GET_MODE_MASK (xmode)) != 0)
	    op_mode = xmode;

	  inner = force_to_mode (inner, op_mode, inner_mask, next_select);

	  if (xmode != op_mode || inner != XEXP (x, 0))
	    {
	      x = simplify_gen_binary (LSHIFTRT, op_mode, inner, XEXP (x, 1));
	      xmode = op_mode;
	    }
	}

      /* If we have (and (lshiftrt FOO C1) C2) where the combination of the
	 shift and AND produces only copies of the sign bit (C2 is one less
	 than a power of two), we can do this with just a shift.  */

      if (GET_CODE (x) == LSHIFTRT
	  && CONST_INT_P (XEXP (x, 1))
	  /* The shift puts one of the sign bit copies in the least significant
	     bit.  */
	  && ((INTVAL (XEXP (x, 1))
	       + num_sign_bit_copies (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
	      >= GET_MODE_PRECISION (xmode))
	  && pow2p_hwi (mask + 1)
	  /* Number of bits left after the shift must be more than the mask
	     needs.  */
	  && ((INTVAL (XEXP (x, 1)) + exact_log2 (mask + 1))
	      <= GET_MODE_PRECISION (xmode))
	  /* Must be more sign bit copies than the mask needs.  */
	  && ((int) num_sign_bit_copies (XEXP (x, 0), GET_MODE (XEXP (x, 0)))
	      >= exact_log2 (mask + 1)))
	{
	  int nbits = GET_MODE_PRECISION (xmode) - exact_log2 (mask + 1);
	  x = simplify_gen_binary (LSHIFTRT, xmode, XEXP (x, 0),
				   gen_int_shift_amount (xmode, nbits));
	}
      goto shiftrt;

    case ASHIFTRT:
      /* If we are just looking for the sign bit, we don't need this shift at
	 all, even if it has a variable count.  */
      if (val_signbit_p (xmode, mask))
	return force_to_mode (XEXP (x, 0), mode, mask, next_select);

      /* If this is a shift by a constant, get a mask that contains those bits
	 that are not copies of the sign bit.  We then have two cases:  If
	 MASK only includes those bits, this can be a logical shift, which may
	 allow simplifications.  If MASK is a single-bit field not within
	 those bits, we are requesting a copy of the sign bit and hence can
	 shift the sign bit to the appropriate location.  */

      if (CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT nonzero;
	  int i;

	  /* If the considered data is wider than HOST_WIDE_INT, we can't
	     represent a mask for all its bits in a single scalar.
	     But we only care about the lower bits, so calculate these.  */

	  if (GET_MODE_PRECISION (xmode) > HOST_BITS_PER_WIDE_INT)
	    {
	      nonzero = HOST_WIDE_INT_M1U;

	      /* GET_MODE_PRECISION (GET_MODE (x)) - INTVAL (XEXP (x, 1))
		 is the number of bits a full-width mask would have set.
		 We need only shift if these are fewer than nonzero can
		 hold.  If not, we must keep all bits set in nonzero.  */

	      if (GET_MODE_PRECISION (xmode) - INTVAL (XEXP (x, 1))
		  < HOST_BITS_PER_WIDE_INT)
		nonzero >>= INTVAL (XEXP (x, 1))
			    + HOST_BITS_PER_WIDE_INT
			    - GET_MODE_PRECISION (xmode);
	    }
	  else
	    {
	      nonzero = GET_MODE_MASK (xmode);
	      nonzero >>= INTVAL (XEXP (x, 1));
	    }

	  if ((mask & ~nonzero) == 0)
	    {
	      x = simplify_shift_const (NULL_RTX, LSHIFTRT, xmode,
					XEXP (x, 0), INTVAL (XEXP (x, 1)));
	      if (GET_CODE (x) != ASHIFTRT)
		return force_to_mode (x, mode, mask, next_select);
	    }

	  else if ((i = exact_log2 (mask)) >= 0)
	    {
	      x = simplify_shift_const
		  (NULL_RTX, LSHIFTRT, xmode, XEXP (x, 0),
		   GET_MODE_PRECISION (xmode) - 1 - i);

	      if (GET_CODE (x) != ASHIFTRT)
		return force_to_mode (x, mode, mask, next_select);
	    }
	}

      /* If MASK is 1, convert this to an LSHIFTRT.  This can be done
	 even if the shift count isn't a constant.  */
      if (mask == 1)
	x = simplify_gen_binary (LSHIFTRT, xmode, XEXP (x, 0), XEXP (x, 1));

    shiftrt:

      /* If this is a zero- or sign-extension operation that just affects bits
	 we don't care about, remove it.  Be sure the call above returned
	 something that is still a shift.  */

      if ((GET_CODE (x) == LSHIFTRT || GET_CODE (x) == ASHIFTRT)
	  && CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0
	  && (INTVAL (XEXP (x, 1))
	      <= GET_MODE_PRECISION (xmode) - (floor_log2 (mask) + 1))
	  && GET_CODE (XEXP (x, 0)) == ASHIFT
	  && XEXP (XEXP (x, 0), 1) == XEXP (x, 1))
	return force_to_mode (XEXP (XEXP (x, 0), 0), mode, mask, next_select);

      break;

    case ROTATE:
    case ROTATERT:
      /* If the shift count is constant and we can do computations
	 in the mode of X, compute where the bits we care about are.
	 Otherwise, we can't do anything.  Don't change the mode of
	 the shift or propagate MODE into the shift, though.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) >= 0)
	{
	  temp = simplify_binary_operation (code == ROTATE ? ROTATERT : ROTATE,
					    xmode, gen_int_mode (mask, xmode),
					    XEXP (x, 1));
	  if (temp && CONST_INT_P (temp))
	    x = simplify_gen_binary (code, xmode,
				     force_to_mode (XEXP (x, 0), xmode,
						    INTVAL (temp), next_select),
				     XEXP (x, 1));
	}
      break;

    case NEG:
      /* If we just want the low-order bit, the NEG isn't needed since it
	 won't change the low-order bit.  */
      if (mask == 1)
	return force_to_mode (XEXP (x, 0), mode, mask, just_select);

      /* We need any bits less significant than the most significant bit in
	 MASK since carries from those bits will affect the bits we are
	 interested in.  */
      mask = fuller_mask;
      goto unop;

    case NOT:
      /* (not FOO) is (xor FOO CONST), so if FOO is an LSHIFTRT, we can do the
	 same as the XOR case above.  Ensure that the constant we form is not
	 wider than the mode of X.  */

      if (GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && INTVAL (XEXP (XEXP (x, 0), 1)) >= 0
	  && (INTVAL (XEXP (XEXP (x, 0), 1)) + floor_log2 (mask)
	      < GET_MODE_PRECISION (xmode))
	  && INTVAL (XEXP (XEXP (x, 0), 1)) < HOST_BITS_PER_WIDE_INT)
	{
	  temp = gen_int_mode (mask << INTVAL (XEXP (XEXP (x, 0), 1)), xmode);
	  temp = simplify_gen_binary (XOR, xmode, XEXP (XEXP (x, 0), 0), temp);
	  x = simplify_gen_binary (LSHIFTRT, xmode,
				   temp, XEXP (XEXP (x, 0), 1));

	  return force_to_mode (x, mode, mask, next_select);
	}

      /* (and (not FOO) CONST) is (not (or FOO (not CONST))), so we must
	 use the full mask inside the NOT.  */
      mask = fuller_mask;

    unop:
      op0 = gen_lowpart_or_truncate (op_mode,
				     force_to_mode (XEXP (x, 0), mode, mask,
						    next_select));
      if (op_mode != xmode || op0 != XEXP (x, 0))
	{
	  x = simplify_gen_unary (code, op_mode, op0, op_mode);
	  xmode = op_mode;
	}
      break;

    case NE:
      /* (and (ne FOO 0) CONST) can be (and FOO CONST) if CONST is included
	 in STORE_FLAG_VALUE and FOO has a single bit that might be nonzero,
	 which is equal to STORE_FLAG_VALUE.  */
      if ((mask & ~STORE_FLAG_VALUE) == 0
	  && XEXP (x, 1) == const0_rtx
	  && GET_MODE (XEXP (x, 0)) == mode
	  && pow2p_hwi (nonzero_bits (XEXP (x, 0), mode))
	  && (nonzero_bits (XEXP (x, 0), mode)
	      == (unsigned HOST_WIDE_INT) STORE_FLAG_VALUE))
	return force_to_mode (XEXP (x, 0), mode, mask, next_select);

      break;

    case IF_THEN_ELSE:
      /* We have no way of knowing if the IF_THEN_ELSE can itself be
	 written in a narrower mode.  We play it safe and do not do so.  */

      op0 = gen_lowpart_or_truncate (xmode,
				     force_to_mode (XEXP (x, 1), mode,
						    mask, next_select));
      op1 = gen_lowpart_or_truncate (xmode,
				     force_to_mode (XEXP (x, 2), mode,
						    mask, next_select));
      if (op0 != XEXP (x, 1) || op1 != XEXP (x, 2))
	x = simplify_gen_ternary (IF_THEN_ELSE, xmode,
				  GET_MODE (XEXP (x, 0)), XEXP (x, 0),
				  op0, op1);
      break;

    default:
      break;
    }

  /* Ensure we return a value of the proper mode.  */
  return gen_lowpart_or_truncate (mode, x);
}

/* Return nonzero if X is an expression that has one of two values depending on
   whether some other value is zero or nonzero.  In that case, we return the
   value that is being tested, *PTRUE is set to the value if the rtx being
   returned has a nonzero value, and *PFALSE is set to the other alternative.

   If we return zero, we set *PTRUE and *PFALSE to X.  */

static rtx
if_then_else_cond (rtx x, rtx *ptrue, rtx *pfalse)
{
  machine_mode mode = GET_MODE (x);
  enum rtx_code code = GET_CODE (x);
  rtx cond0, cond1, true0, true1, false0, false1;
  unsigned HOST_WIDE_INT nz;
  scalar_int_mode int_mode;

  /* If we are comparing a value against zero, we are done.  */
  if ((code == NE || code == EQ)
      && XEXP (x, 1) == const0_rtx)
    {
      *ptrue = (code == NE) ? const_true_rtx : const0_rtx;
      *pfalse = (code == NE) ? const0_rtx : const_true_rtx;
      return XEXP (x, 0);
    }

  /* If this is a unary operation whose operand has one of two values, apply
     our opcode to compute those values.  */
  else if (UNARY_P (x)
	   && (cond0 = if_then_else_cond (XEXP (x, 0), &true0, &false0)) != 0)
    {
      *ptrue = simplify_gen_unary (code, mode, true0, GET_MODE (XEXP (x, 0)));
      *pfalse = simplify_gen_unary (code, mode, false0,
				    GET_MODE (XEXP (x, 0)));
      return cond0;
    }

  /* If this is a COMPARE, do nothing, since the IF_THEN_ELSE we would
     make can't possibly match and would suppress other optimizations.  */
  else if (code == COMPARE)
    ;

  /* If this is a binary operation, see if either side has only one of two
     values.  If either one does or if both do and they are conditional on
     the same value, compute the new true and false values.  */
  else if (BINARY_P (x))
    {
      rtx op0 = XEXP (x, 0);
      rtx op1 = XEXP (x, 1);
      cond0 = if_then_else_cond (op0, &true0, &false0);
      cond1 = if_then_else_cond (op1, &true1, &false1);

      if ((cond0 != 0 && cond1 != 0 && !rtx_equal_p (cond0, cond1))
	  && (REG_P (op0) || REG_P (op1)))
	{
	  /* Try to enable a simplification by undoing work done by
	     if_then_else_cond if it converted a REG into something more
	     complex.  */
	  if (REG_P (op0))
	    {
	      cond0 = 0;
	      true0 = false0 = op0;
	    }
	  else
	    {
	      cond1 = 0;
	      true1 = false1 = op1;
	    }
	}

      if ((cond0 != 0 || cond1 != 0)
	  && ! (cond0 != 0 && cond1 != 0 && !rtx_equal_p (cond0, cond1)))
	{
	  /* If if_then_else_cond returned zero, then true/false are the
	     same rtl.  We must copy one of them to prevent invalid rtl
	     sharing.  */
	  if (cond0 == 0)
	    true0 = copy_rtx (true0);
	  else if (cond1 == 0)
	    true1 = copy_rtx (true1);

	  if (COMPARISON_P (x))
	    {
	      *ptrue = simplify_gen_relational (code, mode, VOIDmode,
						true0, true1);
	      *pfalse = simplify_gen_relational (code, mode, VOIDmode,
						 false0, false1);
	     }
	  else
	    {
	      *ptrue = simplify_gen_binary (code, mode, true0, true1);
	      *pfalse = simplify_gen_binary (code, mode, false0, false1);
	    }

	  return cond0 ? cond0 : cond1;
	}

      /* See if we have PLUS, IOR, XOR, MINUS or UMAX, where one of the
	 operands is zero when the other is nonzero, and vice-versa,
	 and STORE_FLAG_VALUE is 1 or -1.  */

      if ((STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	  && (code == PLUS || code == IOR || code == XOR || code == MINUS
	      || code == UMAX)
	  && GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == MULT)
	{
	  rtx op0 = XEXP (XEXP (x, 0), 1);
	  rtx op1 = XEXP (XEXP (x, 1), 1);

	  cond0 = XEXP (XEXP (x, 0), 0);
	  cond1 = XEXP (XEXP (x, 1), 0);

	  if (COMPARISON_P (cond0)
	      && COMPARISON_P (cond1)
	      && SCALAR_INT_MODE_P (mode)
	      && ((GET_CODE (cond0) == reversed_comparison_code (cond1, NULL)
		   && rtx_equal_p (XEXP (cond0, 0), XEXP (cond1, 0))
		   && rtx_equal_p (XEXP (cond0, 1), XEXP (cond1, 1)))
		  || ((swap_condition (GET_CODE (cond0))
		       == reversed_comparison_code (cond1, NULL))
		      && rtx_equal_p (XEXP (cond0, 0), XEXP (cond1, 1))
		      && rtx_equal_p (XEXP (cond0, 1), XEXP (cond1, 0))))
	      && ! side_effects_p (x))
	    {
	      *ptrue = simplify_gen_binary (MULT, mode, op0, const_true_rtx);
	      *pfalse = simplify_gen_binary (MULT, mode,
					     (code == MINUS
					      ? simplify_gen_unary (NEG, mode,
								    op1, mode)
					      : op1),
					      const_true_rtx);
	      return cond0;
	    }
	}

      /* Similarly for MULT, AND and UMIN, except that for these the result
	 is always zero.  */
      if ((STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	  && (code == MULT || code == AND || code == UMIN)
	  && GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == MULT)
	{
	  cond0 = XEXP (XEXP (x, 0), 0);
	  cond1 = XEXP (XEXP (x, 1), 0);

	  if (COMPARISON_P (cond0)
	      && COMPARISON_P (cond1)
	      && ((GET_CODE (cond0) == reversed_comparison_code (cond1, NULL)
		   && rtx_equal_p (XEXP (cond0, 0), XEXP (cond1, 0))
		   && rtx_equal_p (XEXP (cond0, 1), XEXP (cond1, 1)))
		  || ((swap_condition (GET_CODE (cond0))
		       == reversed_comparison_code (cond1, NULL))
		      && rtx_equal_p (XEXP (cond0, 0), XEXP (cond1, 1))
		      && rtx_equal_p (XEXP (cond0, 1), XEXP (cond1, 0))))
	      && ! side_effects_p (x))
	    {
	      *ptrue = *pfalse = const0_rtx;
	      return cond0;
	    }
	}
    }

  else if (code == IF_THEN_ELSE)
    {
      /* If we have IF_THEN_ELSE already, extract the condition and
	 canonicalize it if it is NE or EQ.  */
      cond0 = XEXP (x, 0);
      *ptrue = XEXP (x, 1), *pfalse = XEXP (x, 2);
      if (GET_CODE (cond0) == NE && XEXP (cond0, 1) == const0_rtx)
	return XEXP (cond0, 0);
      else if (GET_CODE (cond0) == EQ && XEXP (cond0, 1) == const0_rtx)
	{
	  *ptrue = XEXP (x, 2), *pfalse = XEXP (x, 1);
	  return XEXP (cond0, 0);
	}
      else
	return cond0;
    }

  /* If X is a SUBREG, we can narrow both the true and false values
     if the inner expression, if there is a condition.  */
  else if (code == SUBREG
	   && (cond0 = if_then_else_cond (SUBREG_REG (x), &true0,
					  &false0)) != 0)
    {
      true0 = simplify_gen_subreg (mode, true0,
				   GET_MODE (SUBREG_REG (x)), SUBREG_BYTE (x));
      false0 = simplify_gen_subreg (mode, false0,
				    GET_MODE (SUBREG_REG (x)), SUBREG_BYTE (x));
      if (true0 && false0)
	{
	  *ptrue = true0;
	  *pfalse = false0;
	  return cond0;
	}
    }

  /* If X is a constant, this isn't special and will cause confusions
     if we treat it as such.  Likewise if it is equivalent to a constant.  */
  else if (CONSTANT_P (x)
	   || ((cond0 = get_last_value (x)) != 0 && CONSTANT_P (cond0)))
    ;

  /* If we're in BImode, canonicalize on 0 and STORE_FLAG_VALUE, as that
     will be least confusing to the rest of the compiler.  */
  else if (mode == BImode)
    {
      *ptrue = GEN_INT (STORE_FLAG_VALUE), *pfalse = const0_rtx;
      return x;
    }

  /* If X is known to be either 0 or -1, those are the true and
     false values when testing X.  */
  else if (x == constm1_rtx || x == const0_rtx
	   || (is_a <scalar_int_mode> (mode, &int_mode)
	       && (num_sign_bit_copies (x, int_mode)
		   == GET_MODE_PRECISION (int_mode))))
    {
      *ptrue = constm1_rtx, *pfalse = const0_rtx;
      return x;
    }

  /* Likewise for 0 or a single bit.  */
  else if (HWI_COMPUTABLE_MODE_P (mode)
	   && pow2p_hwi (nz = nonzero_bits (x, mode)))
    {
      *ptrue = gen_int_mode (nz, mode), *pfalse = const0_rtx;
      return x;
    }

  /* Otherwise fail; show no condition with true and false values the same.  */
  *ptrue = *pfalse = x;
  return 0;
}

/* Return the value of expression X given the fact that condition COND
   is known to be true when applied to REG as its first operand and VAL
   as its second.  X is known to not be shared and so can be modified in
   place.

   We only handle the simplest cases, and specifically those cases that
   arise with IF_THEN_ELSE expressions.  */

static rtx
known_cond (rtx x, enum rtx_code cond, rtx reg, rtx val)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (side_effects_p (x))
    return x;

  /* If either operand of the condition is a floating point value,
     then we have to avoid collapsing an EQ comparison.  */
  if (cond == EQ
      && rtx_equal_p (x, reg)
      && ! FLOAT_MODE_P (GET_MODE (x))
      && ! FLOAT_MODE_P (GET_MODE (val)))
    return val;

  if (cond == UNEQ && rtx_equal_p (x, reg))
    return val;

  /* If X is (abs REG) and we know something about REG's relationship
     with zero, we may be able to simplify this.  */

  if (code == ABS && rtx_equal_p (XEXP (x, 0), reg) && val == const0_rtx)
    switch (cond)
      {
      case GE:  case GT:  case EQ:
	return XEXP (x, 0);
      case LT:  case LE:
	return simplify_gen_unary (NEG, GET_MODE (XEXP (x, 0)),
				   XEXP (x, 0),
				   GET_MODE (XEXP (x, 0)));
      default:
	break;
      }

  /* The only other cases we handle are MIN, MAX, and comparisons if the
     operands are the same as REG and VAL.  */

  else if (COMPARISON_P (x) || COMMUTATIVE_ARITH_P (x))
    {
      if (rtx_equal_p (XEXP (x, 0), val))
        {
	  std::swap (val, reg);
	  cond = swap_condition (cond);
        }

      if (rtx_equal_p (XEXP (x, 0), reg) && rtx_equal_p (XEXP (x, 1), val))
	{
	  if (COMPARISON_P (x))
	    {
	      if (comparison_dominates_p (cond, code))
		return VECTOR_MODE_P (GET_MODE (x)) ? x : const_true_rtx;

	      code = reversed_comparison_code (x, NULL);
	      if (code != UNKNOWN
		  && comparison_dominates_p (cond, code))
		return CONST0_RTX (GET_MODE (x));
	      else
		return x;
	    }
	  else if (code == SMAX || code == SMIN
		   || code == UMIN || code == UMAX)
	    {
	      int unsignedp = (code == UMIN || code == UMAX);

	      /* Do not reverse the condition when it is NE or EQ.
		 This is because we cannot conclude anything about
		 the value of 'SMAX (x, y)' when x is not equal to y,
		 but we can when x equals y.  */
	      if ((code == SMAX || code == UMAX)
		  && ! (cond == EQ || cond == NE))
		cond = reverse_condition (cond);

	      switch (cond)
		{
		case GE:   case GT:
		  return unsignedp ? x : XEXP (x, 1);
		case LE:   case LT:
		  return unsignedp ? x : XEXP (x, 0);
		case GEU:  case GTU:
		  return unsignedp ? XEXP (x, 1) : x;
		case LEU:  case LTU:
		  return unsignedp ? XEXP (x, 0) : x;
		default:
		  break;
		}
	    }
	}
    }
  else if (code == SUBREG)
    {
      machine_mode inner_mode = GET_MODE (SUBREG_REG (x));
      rtx new_rtx, r = known_cond (SUBREG_REG (x), cond, reg, val);

      if (SUBREG_REG (x) != r)
	{
	  /* We must simplify subreg here, before we lose track of the
	     original inner_mode.  */
	  new_rtx = simplify_subreg (GET_MODE (x), r,
				     inner_mode, SUBREG_BYTE (x));
	  if (new_rtx)
	    return new_rtx;
	  else
	    SUBST (SUBREG_REG (x), r);
	}

      return x;
    }
  /* We don't have to handle SIGN_EXTEND here, because even in the
     case of replacing something with a modeless CONST_INT, a
     CONST_INT is already (supposed to be) a valid sign extension for
     its narrower mode, which implies it's already properly
     sign-extended for the wider mode.  Now, for ZERO_EXTEND, the
     story is different.  */
  else if (code == ZERO_EXTEND)
    {
      machine_mode inner_mode = GET_MODE (XEXP (x, 0));
      rtx new_rtx, r = known_cond (XEXP (x, 0), cond, reg, val);

      if (XEXP (x, 0) != r)
	{
	  /* We must simplify the zero_extend here, before we lose
	     track of the original inner_mode.  */
	  new_rtx = simplify_unary_operation (ZERO_EXTEND, GET_MODE (x),
					      r, inner_mode);
	  if (new_rtx)
	    return new_rtx;
	  else
	    SUBST (XEXP (x, 0), r);
	}

      return x;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	SUBST (XEXP (x, i), known_cond (XEXP (x, i), cond, reg, val));
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  SUBST (XVECEXP (x, i, j), known_cond (XVECEXP (x, i, j),
						cond, reg, val));
    }

  return x;
}

/* See if X and Y are equal for the purposes of seeing if we can rewrite an
   assignment as a field assignment.  */

static bool
rtx_equal_for_field_assignment_p (rtx x, rtx y, bool widen_x)
{
  if (widen_x && GET_MODE (x) != GET_MODE (y))
    {
      if (paradoxical_subreg_p (GET_MODE (x), GET_MODE (y)))
	return false;
      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
	return false;
      x = adjust_address_nv (x, GET_MODE (y),
			     byte_lowpart_offset (GET_MODE (y),
						  GET_MODE (x)));
    }

  if (x == y || rtx_equal_p (x, y))
    return true;

  if (x == 0 || y == 0 || GET_MODE (x) != GET_MODE (y))
    return false;

  /* Check for a paradoxical SUBREG of a MEM compared with the MEM.
     Note that all SUBREGs of MEM are paradoxical; otherwise they
     would have been rewritten.  */
  if (MEM_P (x) && GET_CODE (y) == SUBREG
      && MEM_P (SUBREG_REG (y))
      && rtx_equal_p (SUBREG_REG (y),
		      gen_lowpart (GET_MODE (SUBREG_REG (y)), x)))
    return true;

  if (MEM_P (y) && GET_CODE (x) == SUBREG
      && MEM_P (SUBREG_REG (x))
      && rtx_equal_p (SUBREG_REG (x),
		      gen_lowpart (GET_MODE (SUBREG_REG (x)), y)))
    return true;

  /* We used to see if get_last_value of X and Y were the same but that's
     not correct.  In one direction, we'll cause the assignment to have
     the wrong destination and in the case, we'll import a register into this
     insn that might have already have been dead.   So fail if none of the
     above cases are true.  */
  return false;
}

/* See if X, a SET operation, can be rewritten as a bit-field assignment.
   Return that assignment if so.

   We only handle the most common cases.  */

static rtx
make_field_assignment (rtx x)
{
  rtx dest = SET_DEST (x);
  rtx src = SET_SRC (x);
  rtx assign;
  rtx rhs, lhs;
  HOST_WIDE_INT c1;
  HOST_WIDE_INT pos;
  unsigned HOST_WIDE_INT len;
  rtx other;

  /* All the rules in this function are specific to scalar integers.  */
  scalar_int_mode mode;
  if (!is_a <scalar_int_mode> (GET_MODE (dest), &mode))
    return x;

  /* If SRC was (and (not (ashift (const_int 1) POS)) DEST), this is
     a clear of a one-bit field.  We will have changed it to
     (and (rotate (const_int -2) POS) DEST), so check for that.  Also check
     for a SUBREG.  */

  if (GET_CODE (src) == AND && GET_CODE (XEXP (src, 0)) == ROTATE
      && CONST_INT_P (XEXP (XEXP (src, 0), 0))
      && INTVAL (XEXP (XEXP (src, 0), 0)) == -2
      && rtx_equal_for_field_assignment_p (dest, XEXP (src, 1)))
    {
      assign = make_extraction (VOIDmode, dest, 0, XEXP (XEXP (src, 0), 1),
				1, true, true, false);
      if (assign != 0)
	return gen_rtx_SET (assign, const0_rtx);
      return x;
    }

  if (GET_CODE (src) == AND && GET_CODE (XEXP (src, 0)) == SUBREG
      && subreg_lowpart_p (XEXP (src, 0))
      && partial_subreg_p (XEXP (src, 0))
      && GET_CODE (SUBREG_REG (XEXP (src, 0))) == ROTATE
      && CONST_INT_P (XEXP (SUBREG_REG (XEXP (src, 0)), 0))
      && INTVAL (XEXP (SUBREG_REG (XEXP (src, 0)), 0)) == -2
      && rtx_equal_for_field_assignment_p (dest, XEXP (src, 1)))
    {
      assign = make_extraction (VOIDmode, dest, 0,
				XEXP (SUBREG_REG (XEXP (src, 0)), 1),
				1, true, true, false);
      if (assign != 0)
	return gen_rtx_SET (assign, const0_rtx);
      return x;
    }

  /* If SRC is (ior (ashift (const_int 1) POS) DEST), this is a set of a
     one-bit field.  */
  if (GET_CODE (src) == IOR && GET_CODE (XEXP (src, 0)) == ASHIFT
      && XEXP (XEXP (src, 0), 0) == const1_rtx
      && rtx_equal_for_field_assignment_p (dest, XEXP (src, 1)))
    {
      assign = make_extraction (VOIDmode, dest, 0, XEXP (XEXP (src, 0), 1),
				1, true, true, false);
      if (assign != 0)
	return gen_rtx_SET (assign, const1_rtx);
      return x;
    }

  /* If DEST is already a field assignment, i.e. ZERO_EXTRACT, and the
     SRC is an AND with all bits of that field set, then we can discard
     the AND.  */
  if (GET_CODE (dest) == ZERO_EXTRACT
      && CONST_INT_P (XEXP (dest, 1))
      && GET_CODE (src) == AND
      && CONST_INT_P (XEXP (src, 1)))
    {
      HOST_WIDE_INT width = INTVAL (XEXP (dest, 1));
      unsigned HOST_WIDE_INT and_mask = INTVAL (XEXP (src, 1));
      unsigned HOST_WIDE_INT ze_mask;

      if (width >= HOST_BITS_PER_WIDE_INT)
	ze_mask = -1;
      else
	ze_mask = ((unsigned HOST_WIDE_INT)1 << width) - 1;

      /* Complete overlap.  We can remove the source AND.  */
      if ((and_mask & ze_mask) == ze_mask)
	return gen_rtx_SET (dest, XEXP (src, 0));

      /* Partial overlap.  We can reduce the source AND.  */
      if ((and_mask & ze_mask) != and_mask)
	{
	  src = gen_rtx_AND (mode, XEXP (src, 0),
			     gen_int_mode (and_mask & ze_mask, mode));
	  return gen_rtx_SET (dest, src);
	}
    }

  /* The other case we handle is assignments into a constant-position
     field.  They look like (ior/xor (and DEST C1) OTHER).  If C1 represents
     a mask that has all one bits except for a group of zero bits and
     OTHER is known to have zeros where C1 has ones, this is such an
     assignment.  Compute the position and length from C1.  Shift OTHER
     to the appropriate position, force it to the required mode, and
     make the extraction.  Check for the AND in both operands.  */

  /* One or more SUBREGs might obscure the constant-position field
     assignment.  The first one we are likely to encounter is an outer
     narrowing SUBREG, which we can just strip for the purposes of
     identifying the constant-field assignment.  */
  scalar_int_mode src_mode = mode;
  if (GET_CODE (src) == SUBREG
      && subreg_lowpart_p (src)
      && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (src)), &src_mode))
    src = SUBREG_REG (src);

  if (GET_CODE (src) != IOR && GET_CODE (src) != XOR)
    return x;

  rhs = expand_compound_operation (XEXP (src, 0));
  lhs = expand_compound_operation (XEXP (src, 1));

  if (GET_CODE (rhs) == AND
      && CONST_INT_P (XEXP (rhs, 1))
      && rtx_equal_for_field_assignment_p (XEXP (rhs, 0), dest))
    c1 = INTVAL (XEXP (rhs, 1)), other = lhs;
  /* The second SUBREG that might get in the way is a paradoxical
     SUBREG around the first operand of the AND.  We want to 
     pretend the operand is as wide as the destination here.   We
     do this by adjusting the MEM to wider mode for the sole
     purpose of the call to rtx_equal_for_field_assignment_p.   Also
     note this trick only works for MEMs.  */
  else if (GET_CODE (rhs) == AND
	   && paradoxical_subreg_p (XEXP (rhs, 0))
	   && MEM_P (SUBREG_REG (XEXP (rhs, 0)))
	   && CONST_INT_P (XEXP (rhs, 1))
	   && rtx_equal_for_field_assignment_p (SUBREG_REG (XEXP (rhs, 0)),
						dest, true))
    c1 = INTVAL (XEXP (rhs, 1)), other = lhs;
  else if (GET_CODE (lhs) == AND
	   && CONST_INT_P (XEXP (lhs, 1))
	   && rtx_equal_for_field_assignment_p (XEXP (lhs, 0), dest))
    c1 = INTVAL (XEXP (lhs, 1)), other = rhs;
  /* The second SUBREG that might get in the way is a paradoxical
     SUBREG around the first operand of the AND.  We want to 
     pretend the operand is as wide as the destination here.   We
     do this by adjusting the MEM to wider mode for the sole
     purpose of the call to rtx_equal_for_field_assignment_p.   Also
     note this trick only works for MEMs.  */
  else if (GET_CODE (lhs) == AND
	   && paradoxical_subreg_p (XEXP (lhs, 0))
	   && MEM_P (SUBREG_REG (XEXP (lhs, 0)))
	   && CONST_INT_P (XEXP (lhs, 1))
	   && rtx_equal_for_field_assignment_p (SUBREG_REG (XEXP (lhs, 0)),
						dest, true))
    c1 = INTVAL (XEXP (lhs, 1)), other = rhs;
  else
    return x;

  pos = get_pos_from_mask ((~c1) & GET_MODE_MASK (mode), &len);
  if (pos < 0
      || pos + len > GET_MODE_PRECISION (mode)
      || GET_MODE_PRECISION (mode) > HOST_BITS_PER_WIDE_INT
      || (c1 & nonzero_bits (other, mode)) != 0)
    return x;

  assign = make_extraction (VOIDmode, dest, pos, NULL_RTX, len,
			    true, true, false);
  if (assign == 0)
    return x;

  /* The mode to use for the source is the mode of the assignment, or of
     what is inside a possible STRICT_LOW_PART.  */
  machine_mode new_mode = (GET_CODE (assign) == STRICT_LOW_PART
			   ? GET_MODE (XEXP (assign, 0)) : GET_MODE (assign));

  /* Shift OTHER right POS places and make it the source, restricting it
     to the proper length and mode.  */

  src = canon_reg_for_combine (simplify_shift_const (NULL_RTX, LSHIFTRT,
						     src_mode, other, pos),
			       dest);
  src = force_to_mode (src, new_mode,
		       len >= HOST_BITS_PER_WIDE_INT
		       ? HOST_WIDE_INT_M1U
		       : (HOST_WIDE_INT_1U << len) - 1, false);

  /* If SRC is masked by an AND that does not make a difference in
     the value being stored, strip it.  */
  if (GET_CODE (assign) == ZERO_EXTRACT
      && CONST_INT_P (XEXP (assign, 1))
      && INTVAL (XEXP (assign, 1)) < HOST_BITS_PER_WIDE_INT
      && GET_CODE (src) == AND
      && CONST_INT_P (XEXP (src, 1))
      && UINTVAL (XEXP (src, 1))
	 == (HOST_WIDE_INT_1U << INTVAL (XEXP (assign, 1))) - 1)
    src = XEXP (src, 0);

  return gen_rtx_SET (assign, src);
}

/* See if X is of the form (+ (* a c) (* b c)) and convert to (* (+ a b) c)
   if so.  */

static rtx
apply_distributive_law (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  enum rtx_code inner_code;
  rtx lhs, rhs, other;
  rtx tem;

  /* Distributivity is not true for floating point as it can change the
     value.  So we don't do it unless -funsafe-math-optimizations.  */
  if (FLOAT_MODE_P (GET_MODE (x))
      && ! flag_unsafe_math_optimizations)
    return x;

  /* The outer operation can only be one of the following:  */
  if (code != IOR && code != AND && code != XOR
      && code != PLUS && code != MINUS)
    return x;

  lhs = XEXP (x, 0);
  rhs = XEXP (x, 1);

  /* If either operand is a primitive we can't do anything, so get out
     fast.  */
  if (OBJECT_P (lhs) || OBJECT_P (rhs))
    return x;

  lhs = expand_compound_operation (lhs);
  rhs = expand_compound_operation (rhs);
  inner_code = GET_CODE (lhs);
  if (inner_code != GET_CODE (rhs))
    return x;

  /* See if the inner and outer operations distribute.  */
  switch (inner_code)
    {
    case LSHIFTRT:
    case ASHIFTRT:
    case AND:
    case IOR:
      /* These all distribute except over PLUS.  */
      if (code == PLUS || code == MINUS)
	return x;
      break;

    case MULT:
      if (code != PLUS && code != MINUS)
	return x;
      break;

    case ASHIFT:
      /* This is also a multiply, so it distributes over everything.  */
      break;

    /* This used to handle SUBREG, but this turned out to be counter-
       productive, since (subreg (op ...)) usually is not handled by
       insn patterns, and this "optimization" therefore transformed
       recognizable patterns into unrecognizable ones.  Therefore the
       SUBREG case was removed from here.

       It is possible that distributing SUBREG over arithmetic operations
       leads to an intermediate result than can then be optimized further,
       e.g. by moving the outer SUBREG to the other side of a SET as done
       in simplify_set.  This seems to have been the original intent of
       handling SUBREGs here.

       However, with current GCC this does not appear to actually happen,
       at least on major platforms.  If some case is found where removing
       the SUBREG case here prevents follow-on optimizations, distributing
       SUBREGs ought to be re-added at that place, e.g. in simplify_set.  */

    default:
      return x;
    }

  /* Set LHS and RHS to the inner operands (A and B in the example
     above) and set OTHER to the common operand (C in the example).
     There is only one way to do this unless the inner operation is
     commutative.  */
  if (COMMUTATIVE_ARITH_P (lhs)
      && rtx_equal_p (XEXP (lhs, 0), XEXP (rhs, 0)))
    other = XEXP (lhs, 0), lhs = XEXP (lhs, 1), rhs = XEXP (rhs, 1);
  else if (COMMUTATIVE_ARITH_P (lhs)
	   && rtx_equal_p (XEXP (lhs, 0), XEXP (rhs, 1)))
    other = XEXP (lhs, 0), lhs = XEXP (lhs, 1), rhs = XEXP (rhs, 0);
  else if (COMMUTATIVE_ARITH_P (lhs)
	   && rtx_equal_p (XEXP (lhs, 1), XEXP (rhs, 0)))
    other = XEXP (lhs, 1), lhs = XEXP (lhs, 0), rhs = XEXP (rhs, 1);
  else if (rtx_equal_p (XEXP (lhs, 1), XEXP (rhs, 1)))
    other = XEXP (lhs, 1), lhs = XEXP (lhs, 0), rhs = XEXP (rhs, 0);
  else
    return x;

  /* Form the new inner operation, seeing if it simplifies first.  */
  tem = simplify_gen_binary (code, GET_MODE (x), lhs, rhs);

  /* There is one exception to the general way of distributing:
     (a | c) ^ (b | c) -> (a ^ b) & ~c  */
  if (code == XOR && inner_code == IOR)
    {
      inner_code = AND;
      other = simplify_gen_unary (NOT, GET_MODE (x), other, GET_MODE (x));
    }

  /* We may be able to continuing distributing the result, so call
     ourselves recursively on the inner operation before forming the
     outer operation, which we return.  */
  return simplify_gen_binary (inner_code, GET_MODE (x),
			      apply_distributive_law (tem), other);
}

/* See if X is of the form (* (+ A B) C), and if so convert to
   (+ (* A C) (* B C)) and try to simplify.

   Most of the time, this results in no change.  However, if some of
   the operands are the same or inverses of each other, simplifications
   will result.

   For example, (and (ior A B) (not B)) can occur as the result of
   expanding a bit field assignment.  When we apply the distributive
   law to this, we get (ior (and (A (not B))) (and (B (not B)))),
   which then simplifies to (and (A (not B))).

   Note that no checks happen on the validity of applying the inverse
   distributive law.  This is pointless since we can do it in the
   few places where this routine is called.

   N is the index of the term that is decomposed (the arithmetic operation,
   i.e. (+ A B) in the first example above).  !N is the index of the term that
   is distributed, i.e. of C in the first example above.  */
static rtx
distribute_and_simplify_rtx (rtx x, int n)
{
  machine_mode mode;
  enum rtx_code outer_code, inner_code;
  rtx decomposed, distributed, inner_op0, inner_op1, new_op0, new_op1, tmp;

  /* Distributivity is not true for floating point as it can change the
     value.  So we don't do it unless -funsafe-math-optimizations.  */
  if (FLOAT_MODE_P (GET_MODE (x))
      && ! flag_unsafe_math_optimizations)
    return NULL_RTX;

  decomposed = XEXP (x, n);
  if (!ARITHMETIC_P (decomposed))
    return NULL_RTX;

  mode = GET_MODE (x);
  outer_code = GET_CODE (x);
  distributed = XEXP (x, !n);

  inner_code = GET_CODE (decomposed);
  inner_op0 = XEXP (decomposed, 0);
  inner_op1 = XEXP (decomposed, 1);

  /* Special case (and (xor B C) (not A)), which is equivalent to
     (xor (ior A B) (ior A C))  */
  if (outer_code == AND && inner_code == XOR && GET_CODE (distributed) == NOT)
    {
      distributed = XEXP (distributed, 0);
      outer_code = IOR;
    }

  if (n == 0)
    {
      /* Distribute the second term.  */
      new_op0 = simplify_gen_binary (outer_code, mode, inner_op0, distributed);
      new_op1 = simplify_gen_binary (outer_code, mode, inner_op1, distributed);
    }
  else
    {
      /* Distribute the first term.  */
      new_op0 = simplify_gen_binary (outer_code, mode, distributed, inner_op0);
      new_op1 = simplify_gen_binary (outer_code, mode, distributed, inner_op1);
    }

  tmp = apply_distributive_law (simplify_gen_binary (inner_code, mode,
						     new_op0, new_op1));
  if (GET_CODE (tmp) != outer_code
      && (set_src_cost (tmp, mode, optimize_this_for_speed_p)
	  < set_src_cost (x, mode, optimize_this_for_speed_p)))
    return tmp;

  return NULL_RTX;
}

/* Simplify a logical `and' of VAROP with the constant CONSTOP, to be done
   in MODE.  Return an equivalent form, if different from (and VAROP
   (const_int CONSTOP)).  Otherwise, return NULL_RTX.  */

static rtx
simplify_and_const_int_1 (scalar_int_mode mode, rtx varop,
			  unsigned HOST_WIDE_INT constop)
{
  unsigned HOST_WIDE_INT nonzero;
  unsigned HOST_WIDE_INT orig_constop;
  rtx orig_varop;
  int i;

  orig_varop = varop;
  orig_constop = constop;
  if (GET_CODE (varop) == CLOBBER)
    return NULL_RTX;

  /* Simplify VAROP knowing that we will be only looking at some of the
     bits in it.

     Note by passing in CONSTOP, we guarantee that the bits not set in
     CONSTOP are not significant and will never be examined.  We must
     ensure that is the case by explicitly masking out those bits
     before returning.  */
  varop = force_to_mode (varop, mode, constop, false);

  /* If VAROP is a CLOBBER, we will fail so return it.  */
  if (GET_CODE (varop) == CLOBBER)
    return varop;

  /* If VAROP is a CONST_INT, then we need to apply the mask in CONSTOP
     to VAROP and return the new constant.  */
  if (CONST_INT_P (varop))
    return gen_int_mode (INTVAL (varop) & constop, mode);

  /* See what bits may be nonzero in VAROP.  Unlike the general case of
     a call to nonzero_bits, here we don't care about bits outside
     MODE unless WORD_REGISTER_OPERATIONS is true.  */

  scalar_int_mode tmode = mode;
  if (WORD_REGISTER_OPERATIONS && GET_MODE_BITSIZE (mode) < BITS_PER_WORD)
    tmode = word_mode;
  nonzero = nonzero_bits (varop, tmode) & GET_MODE_MASK (tmode);

  /* Turn off all bits in the constant that are known to already be zero.
     Thus, if the AND isn't needed at all, we will have CONSTOP == NONZERO_BITS
     which is tested below.  */

  constop &= nonzero;

  /* If we don't have any bits left, return zero.  */
  if (constop == 0 && !side_effects_p (varop))
    return const0_rtx;

  /* If VAROP is a NEG of something known to be zero or 1 and CONSTOP is
     a power of two, we can replace this with an ASHIFT.  */
  if (GET_CODE (varop) == NEG && nonzero_bits (XEXP (varop, 0), tmode) == 1
      && (i = exact_log2 (constop)) >= 0)
    return simplify_shift_const (NULL_RTX, ASHIFT, mode, XEXP (varop, 0), i);

  /* If VAROP is an IOR or XOR, apply the AND to both branches of the IOR
     or XOR, then try to apply the distributive law.  This may eliminate
     operations if either branch can be simplified because of the AND.
     It may also make some cases more complex, but those cases probably
     won't match a pattern either with or without this.  */

  if (GET_CODE (varop) == IOR || GET_CODE (varop) == XOR)
    {
      scalar_int_mode varop_mode = as_a <scalar_int_mode> (GET_MODE (varop));
      return
	gen_lowpart
	  (mode,
	   apply_distributive_law
	   (simplify_gen_binary (GET_CODE (varop), varop_mode,
				 simplify_and_const_int (NULL_RTX, varop_mode,
							 XEXP (varop, 0),
							 constop),
				 simplify_and_const_int (NULL_RTX, varop_mode,
							 XEXP (varop, 1),
							 constop))));
    }

  /* If VAROP is PLUS, and the constant is a mask of low bits, distribute
     the AND and see if one of the operands simplifies to zero.  If so, we
     may eliminate it.  */

  if (GET_CODE (varop) == PLUS
      && pow2p_hwi (constop + 1))
    {
      rtx o0, o1;

      o0 = simplify_and_const_int (NULL_RTX, mode, XEXP (varop, 0), constop);
      o1 = simplify_and_const_int (NULL_RTX, mode, XEXP (varop, 1), constop);
      if (o0 == const0_rtx)
	return o1;
      if (o1 == const0_rtx)
	return o0;
    }

  /* Make a SUBREG if necessary.  If we can't make it, fail.  */
  varop = gen_lowpart (mode, varop);
  if (varop == NULL_RTX || GET_CODE (varop) == CLOBBER)
    return NULL_RTX;

  /* If we are only masking insignificant bits, return VAROP.  */
  if (constop == nonzero)
    return varop;

  if (varop == orig_varop && constop == orig_constop)
    return NULL_RTX;

  /* Otherwise, return an AND.  */
  return simplify_gen_binary (AND, mode, varop, gen_int_mode (constop, mode));
}


/* We have X, a logical `and' of VAROP with the constant CONSTOP, to be done
   in MODE.

   Return an equivalent form, if different from X.  Otherwise, return X.  If
   X is zero, we are to always construct the equivalent form.  */

static rtx
simplify_and_const_int (rtx x, scalar_int_mode mode, rtx varop,
			unsigned HOST_WIDE_INT constop)
{
  rtx tem = simplify_and_const_int_1 (mode, varop, constop);
  if (tem)
    return tem;

  if (!x)
    x = simplify_gen_binary (AND, GET_MODE (varop), varop,
			     gen_int_mode (constop, mode));
  if (GET_MODE (x) != mode)
    x = gen_lowpart (mode, x);
  return x;
}

/* Given a REG X of mode XMODE, compute which bits in X can be nonzero.
   We don't care about bits outside of those defined in MODE.
   We DO care about all the bits in MODE, even if XMODE is smaller than MODE.

   For most X this is simply GET_MODE_MASK (GET_MODE (MODE)), but if X is
   a shift, AND, or zero_extract, we can do better.  */

static rtx
reg_nonzero_bits_for_combine (const_rtx x, scalar_int_mode xmode,
			      scalar_int_mode mode,
			      unsigned HOST_WIDE_INT *nonzero)
{
  rtx tem;
  reg_stat_type *rsp;

  /* If X is a register whose nonzero bits value is current, use it.
     Otherwise, if X is a register whose value we can find, use that
     value.  Otherwise, use the previously-computed global nonzero bits
     for this register.  */

  rsp = &reg_stat[REGNO (x)];
  if (rsp->last_set_value != 0
      && (rsp->last_set_mode == mode
	  || (REGNO (x) >= FIRST_PSEUDO_REGISTER
	      && GET_MODE_CLASS (rsp->last_set_mode) == MODE_INT
	      && GET_MODE_CLASS (mode) == MODE_INT))
      && ((rsp->last_set_label >= label_tick_ebb_start
	   && rsp->last_set_label < label_tick)
	  || (rsp->last_set_label == label_tick
              && DF_INSN_LUID (rsp->last_set) < subst_low_luid)
	  || (REGNO (x) >= FIRST_PSEUDO_REGISTER
	      && REGNO (x) < reg_n_sets_max
	      && REG_N_SETS (REGNO (x)) == 1
	      && !REGNO_REG_SET_P
		  (DF_LR_IN (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb),
		   REGNO (x)))))
    {
      /* Note that, even if the precision of last_set_mode is lower than that
	 of mode, record_value_for_reg invoked nonzero_bits on the register
	 with nonzero_bits_mode (because last_set_mode is necessarily integral
	 and HWI_COMPUTABLE_MODE_P in this case) so bits in nonzero_bits_mode
	 are all valid, hence in mode too since nonzero_bits_mode is defined
	 to the largest HWI_COMPUTABLE_MODE_P mode.  */
      *nonzero &= rsp->last_set_nonzero_bits;
      return NULL;
    }

  tem = get_last_value (x);
  if (tem)
    {
      if (SHORT_IMMEDIATES_SIGN_EXTEND)
	tem = sign_extend_short_imm (tem, xmode, GET_MODE_PRECISION (mode));

      return tem;
    }

  if (nonzero_sign_valid && rsp->nonzero_bits)
    {
      unsigned HOST_WIDE_INT mask = rsp->nonzero_bits;

      if (GET_MODE_PRECISION (xmode) < GET_MODE_PRECISION (mode))
	/* We don't know anything about the upper bits.  */
	mask |= GET_MODE_MASK (mode) ^ GET_MODE_MASK (xmode);

      *nonzero &= mask;
    }

  return NULL;
}

/* Given a reg X of mode XMODE, return the number of bits at the high-order
   end of X that are known to be equal to the sign bit.  X will be used
   in mode MODE; the returned value will always be between 1 and the
   number of bits in MODE.  */

static rtx
reg_num_sign_bit_copies_for_combine (const_rtx x, scalar_int_mode xmode,
				     scalar_int_mode mode,
				     unsigned int *result)
{
  rtx tem;
  reg_stat_type *rsp;

  rsp = &reg_stat[REGNO (x)];
  if (rsp->last_set_value != 0
      && rsp->last_set_mode == mode
      && ((rsp->last_set_label >= label_tick_ebb_start
	   && rsp->last_set_label < label_tick)
	  || (rsp->last_set_label == label_tick
              && DF_INSN_LUID (rsp->last_set) < subst_low_luid)
	  || (REGNO (x) >= FIRST_PSEUDO_REGISTER
	      && REGNO (x) < reg_n_sets_max
	      && REG_N_SETS (REGNO (x)) == 1
	      && !REGNO_REG_SET_P
		  (DF_LR_IN (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb),
		   REGNO (x)))))
    {
      *result = rsp->last_set_sign_bit_copies;
      return NULL;
    }

  tem = get_last_value (x);
  if (tem != 0)
    return tem;

  if (nonzero_sign_valid && rsp->sign_bit_copies != 0
      && GET_MODE_PRECISION (xmode) == GET_MODE_PRECISION (mode))
    *result = rsp->sign_bit_copies;

  return NULL;
}

/* Return the number of "extended" bits there are in X, when interpreted
   as a quantity in MODE whose signedness is indicated by UNSIGNEDP.  For
   unsigned quantities, this is the number of high-order zero bits.
   For signed quantities, this is the number of copies of the sign bit
   minus 1.  In both case, this function returns the number of "spare"
   bits.  For example, if two quantities for which this function returns
   at least 1 are added, the addition is known not to overflow.

   This function will always return 0 unless called during combine, which
   implies that it must be called from a define_split.  */

unsigned int
extended_count (const_rtx x, machine_mode mode, bool unsignedp)
{
  if (nonzero_sign_valid == 0)
    return 0;

  scalar_int_mode int_mode;
  return (unsignedp
	  ? (is_a <scalar_int_mode> (mode, &int_mode)
	     && HWI_COMPUTABLE_MODE_P (int_mode)
	     ? (unsigned int) (GET_MODE_PRECISION (int_mode) - 1
			       - floor_log2 (nonzero_bits (x, int_mode)))
	     : 0)
	  : num_sign_bit_copies (x, mode) - 1);
}

/* This function is called from `simplify_shift_const' to merge two
   outer operations.  Specifically, we have already found that we need
   to perform operation *POP0 with constant *PCONST0 at the outermost
   position.  We would now like to also perform OP1 with constant CONST1
   (with *POP0 being done last).

   Return true if we can do the operation and update *POP0 and *PCONST0 with
   the resulting operation.  *PCOMP_P is set to true if we would need to
   complement the innermost operand, otherwise it is unchanged.

   MODE is the mode in which the operation will be done.  No bits outside
   the width of this mode matter.  It is assumed that the width of this mode
   is smaller than or equal to HOST_BITS_PER_WIDE_INT.

   If *POP0 or OP1 are UNKNOWN, it means no operation is required.  Only NEG, PLUS,
   IOR, XOR, and AND are supported.  We may set *POP0 to SET if the proper
   result is simply *PCONST0.

   If the resulting operation cannot be expressed as one operation, we
   return false and do not change *POP0, *PCONST0, and *PCOMP_P.  */

static bool
merge_outer_ops (enum rtx_code *pop0, HOST_WIDE_INT *pconst0,
		 enum rtx_code op1, HOST_WIDE_INT const1,
		 machine_mode mode, bool *pcomp_p)
{
  enum rtx_code op0 = *pop0;
  HOST_WIDE_INT const0 = *pconst0;

  const0 &= GET_MODE_MASK (mode);
  const1 &= GET_MODE_MASK (mode);

  /* If OP0 is an AND, clear unimportant bits in CONST1.  */
  if (op0 == AND)
    const1 &= const0;

  /* If OP0 or OP1 is UNKNOWN, this is easy.  Similarly if they are the same or
     if OP0 is SET.  */

  if (op1 == UNKNOWN || op0 == SET)
    return true;

  else if (op0 == UNKNOWN)
    op0 = op1, const0 = const1;

  else if (op0 == op1)
    {
      switch (op0)
	{
	case AND:
	  const0 &= const1;
	  break;
	case IOR:
	  const0 |= const1;
	  break;
	case XOR:
	  const0 ^= const1;
	  break;
	case PLUS:
	  const0 += const1;
	  break;
	case NEG:
	  op0 = UNKNOWN;
	  break;
	default:
	  break;
	}
    }

  /* Otherwise, if either is a PLUS or NEG, we can't do anything.  */
  else if (op0 == PLUS || op1 == PLUS || op0 == NEG || op1 == NEG)
    return false;

  /* If the two constants aren't the same, we can't do anything.  The
     remaining six cases can all be done.  */
  else if (const0 != const1)
    return false;

  else
    switch (op0)
      {
      case IOR:
	if (op1 == AND)
	  /* (a & b) | b == b */
	  op0 = SET;
	else /* op1 == XOR */
	  /* (a ^ b) | b == a | b */
	  {;}
	break;

      case XOR:
	if (op1 == AND)
	  /* (a & b) ^ b == (~a) & b */
	  op0 = AND, *pcomp_p = true;
	else /* op1 == IOR */
	  /* (a | b) ^ b == a & ~b */
	  op0 = AND, const0 = ~const0;
	break;

      case AND:
	if (op1 == IOR)
	  /* (a | b) & b == b */
	op0 = SET;
	else /* op1 == XOR */
	  /* (a ^ b) & b) == (~a) & b */
	  *pcomp_p = true;
	break;
      default:
	break;
      }

  /* Check for NO-OP cases.  */
  const0 &= GET_MODE_MASK (mode);
  if (const0 == 0
      && (op0 == IOR || op0 == XOR || op0 == PLUS))
    op0 = UNKNOWN;
  else if (const0 == 0 && op0 == AND)
    op0 = SET;
  else if ((unsigned HOST_WIDE_INT) const0 == GET_MODE_MASK (mode)
	   && op0 == AND)
    op0 = UNKNOWN;

  *pop0 = op0;

  /* ??? Slightly redundant with the above mask, but not entirely.
     Moving this above means we'd have to sign-extend the mode mask
     for the final test.  */
  if (op0 != UNKNOWN && op0 != NEG)
    *pconst0 = trunc_int_for_mode (const0, mode);

  return true;
}

/* A helper to simplify_shift_const_1 to determine the mode we can perform
   the shift in.  The original shift operation CODE is performed on OP in
   ORIG_MODE.  Return the wider mode MODE if we can perform the operation
   in that mode.  Return ORIG_MODE otherwise.  We can also assume that the
   result of the shift is subject to operation OUTER_CODE with operand
   OUTER_CONST.  */

static scalar_int_mode
try_widen_shift_mode (enum rtx_code code, rtx op, int count,
		      scalar_int_mode orig_mode, scalar_int_mode mode,
		      enum rtx_code outer_code, HOST_WIDE_INT outer_const)
{
  gcc_assert (GET_MODE_PRECISION (mode) > GET_MODE_PRECISION (orig_mode));

  /* In general we can't perform in wider mode for right shift and rotate.  */
  switch (code)
    {
    case ASHIFTRT:
      /* We can still widen if the bits brought in from the left are identical
	 to the sign bit of ORIG_MODE.  */
      if (num_sign_bit_copies (op, mode)
	  > (unsigned) (GET_MODE_PRECISION (mode)
			- GET_MODE_PRECISION (orig_mode)))
	return mode;
      return orig_mode;

    case LSHIFTRT:
      /* Similarly here but with zero bits.  */
      if (HWI_COMPUTABLE_MODE_P (mode)
	  && (nonzero_bits (op, mode) & ~GET_MODE_MASK (orig_mode)) == 0)
	return mode;

      /* We can also widen if the bits brought in will be masked off.  This
	 operation is performed in ORIG_MODE.  */
      if (outer_code == AND)
	{
	  int care_bits = low_bitmask_len (orig_mode, outer_const);

	  if (care_bits >= 0
	      && GET_MODE_PRECISION (orig_mode) - care_bits >= count)
	    return mode;
	}
      /* fall through */

    case ROTATE:
      return orig_mode;

    case ROTATERT:
      gcc_unreachable ();

    default:
      return mode;
    }
}

/* Simplify a shift of VAROP by ORIG_COUNT bits.  CODE says what kind
   of shift.  The result of the shift is RESULT_MODE.  Return NULL_RTX
   if we cannot simplify it.  Otherwise, return a simplified value.

   The shift is normally computed in the widest mode we find in VAROP, as
   long as it isn't a different number of words than RESULT_MODE.  Exceptions
   are ASHIFTRT and ROTATE, which are always done in their original mode.  */

static rtx
simplify_shift_const_1 (enum rtx_code code, machine_mode result_mode,
			rtx varop, int orig_count)
{
  enum rtx_code orig_code = code;
  rtx orig_varop = varop;
  int count, log2;
  machine_mode mode = result_mode;
  machine_mode shift_mode;
  scalar_int_mode tmode, inner_mode, int_mode, int_varop_mode, int_result_mode;
  /* We form (outer_op (code varop count) (outer_const)).  */
  enum rtx_code outer_op = UNKNOWN;
  HOST_WIDE_INT outer_const = 0;
  bool complement_p = false;
  rtx new_rtx, x;

  /* Make sure and truncate the "natural" shift on the way in.  We don't
     want to do this inside the loop as it makes it more difficult to
     combine shifts.  */
  if (SHIFT_COUNT_TRUNCATED)
    orig_count &= GET_MODE_UNIT_BITSIZE (mode) - 1;

  /* If we were given an invalid count, don't do anything except exactly
     what was requested.  */

  if (orig_count < 0 || orig_count >= (int) GET_MODE_UNIT_PRECISION (mode))
    return NULL_RTX;

  count = orig_count;

  /* Unless one of the branches of the `if' in this loop does a `continue',
     we will `break' the loop after the `if'.  */

  while (count != 0)
    {
      /* If we have an operand of (clobber (const_int 0)), fail.  */
      if (GET_CODE (varop) == CLOBBER)
	return NULL_RTX;

      /* Convert ROTATERT to ROTATE.  */
      if (code == ROTATERT)
	{
	  unsigned int bitsize = GET_MODE_UNIT_PRECISION (result_mode);
	  code = ROTATE;
	  count = bitsize - count;
	}

      shift_mode = result_mode;
      if (shift_mode != mode)
	{
	  /* We only change the modes of scalar shifts.  */
	  int_mode = as_a <scalar_int_mode> (mode);
	  int_result_mode = as_a <scalar_int_mode> (result_mode);
	  shift_mode = try_widen_shift_mode (code, varop, count,
					     int_result_mode, int_mode,
					     outer_op, outer_const);
	}

      scalar_int_mode shift_unit_mode
	= as_a <scalar_int_mode> (GET_MODE_INNER (shift_mode));

      /* Handle cases where the count is greater than the size of the mode
	 minus 1.  For ASHIFT, use the size minus one as the count (this can
	 occur when simplifying (lshiftrt (ashiftrt ..))).  For rotates,
	 take the count modulo the size.  For other shifts, the result is
	 zero.

	 Since these shifts are being produced by the compiler by combining
	 multiple operations, each of which are defined, we know what the
	 result is supposed to be.  */

      if (count > (GET_MODE_PRECISION (shift_unit_mode) - 1))
	{
	  if (code == ASHIFTRT)
	    count = GET_MODE_PRECISION (shift_unit_mode) - 1;
	  else if (code == ROTATE || code == ROTATERT)
	    count %= GET_MODE_PRECISION (shift_unit_mode);
	  else
	    {
	      /* We can't simply return zero because there may be an
		 outer op.  */
	      varop = const0_rtx;
	      count = 0;
	      break;
	    }
	}

      /* If we discovered we had to complement VAROP, leave.  Making a NOT
	 here would cause an infinite loop.  */
      if (complement_p)
	break;

      if (shift_mode == shift_unit_mode)
	{
	  /* An arithmetic right shift of a quantity known to be -1 or 0
	     is a no-op.  */
	  if (code == ASHIFTRT
	      && (num_sign_bit_copies (varop, shift_unit_mode)
		  == GET_MODE_PRECISION (shift_unit_mode)))
	    {
	      count = 0;
	      break;
	    }

	  /* If we are doing an arithmetic right shift and discarding all but
	     the sign bit copies, this is equivalent to doing a shift by the
	     bitsize minus one.  Convert it into that shift because it will
	     often allow other simplifications.  */

	  if (code == ASHIFTRT
	      && (count + num_sign_bit_copies (varop, shift_unit_mode)
		  >= GET_MODE_PRECISION (shift_unit_mode)))
	    count = GET_MODE_PRECISION (shift_unit_mode) - 1;

	  /* We simplify the tests below and elsewhere by converting
	     ASHIFTRT to LSHIFTRT if we know the sign bit is clear.
	     `make_compound_operation' will convert it to an ASHIFTRT for
	     those machines (such as VAX) that don't have an LSHIFTRT.  */
	  if (code == ASHIFTRT
	      && HWI_COMPUTABLE_MODE_P (shift_unit_mode)
	      && val_signbit_known_clear_p (shift_unit_mode,
					    nonzero_bits (varop,
							  shift_unit_mode)))
	    code = LSHIFTRT;

	  if (((code == LSHIFTRT
		&& HWI_COMPUTABLE_MODE_P (shift_unit_mode)
		&& !(nonzero_bits (varop, shift_unit_mode) >> count))
	       || (code == ASHIFT
		   && HWI_COMPUTABLE_MODE_P (shift_unit_mode)
		   && !((nonzero_bits (varop, shift_unit_mode) << count)
			& GET_MODE_MASK (shift_unit_mode))))
	      && !side_effects_p (varop))
	    varop = const0_rtx;
	}

      switch (GET_CODE (varop))
	{
	case SIGN_EXTEND:
	case ZERO_EXTEND:
	case SIGN_EXTRACT:
	case ZERO_EXTRACT:
	  new_rtx = expand_compound_operation (varop);
	  if (new_rtx != varop)
	    {
	      varop = new_rtx;
	      continue;
	    }
	  break;

	case MEM:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_mode = as_a <scalar_int_mode> (mode);

	  /* If we have (xshiftrt (mem ...) C) and C is MODE_WIDTH
	     minus the width of a smaller mode, we can do this with a
	     SIGN_EXTEND or ZERO_EXTEND from the narrower memory location.  */
	  if ((code == ASHIFTRT || code == LSHIFTRT)
	      && ! mode_dependent_address_p (XEXP (varop, 0),
					     MEM_ADDR_SPACE (varop))
	      && ! MEM_VOLATILE_P (varop)
	      && (int_mode_for_size (GET_MODE_BITSIZE (int_mode) - count, 1)
		  .exists (&tmode)))
	    {
	      new_rtx = adjust_address_nv (varop, tmode,
					   BYTES_BIG_ENDIAN ? 0
					   : count / BITS_PER_UNIT);

	      varop = gen_rtx_fmt_e (code == ASHIFTRT ? SIGN_EXTEND
				     : ZERO_EXTEND, int_mode, new_rtx);
	      count = 0;
	      continue;
	    }
	  break;

	case SUBREG:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_mode = as_a <scalar_int_mode> (mode);
	  int_varop_mode = as_a <scalar_int_mode> (GET_MODE (varop));

	  /* If VAROP is a SUBREG, strip it as long as the inner operand has
	     the same number of words as what we've seen so far.  Then store
	     the widest mode in MODE.  */
	  if (subreg_lowpart_p (varop)
	      && is_int_mode (GET_MODE (SUBREG_REG (varop)), &inner_mode)
	      && GET_MODE_SIZE (inner_mode) > GET_MODE_SIZE (int_varop_mode)
	      && (CEIL (GET_MODE_SIZE (inner_mode), UNITS_PER_WORD)
		  == CEIL (GET_MODE_SIZE (int_mode), UNITS_PER_WORD))
	      && GET_MODE_CLASS (int_varop_mode) == MODE_INT)
	    {
	      varop = SUBREG_REG (varop);
	      if (GET_MODE_SIZE (inner_mode) > GET_MODE_SIZE (int_mode))
		mode = inner_mode;
	      continue;
	    }
	  break;

	case MULT:
	  /* Some machines use MULT instead of ASHIFT because MULT
	     is cheaper.  But it is still better on those machines to
	     merge two shifts into one.  */
	  if (CONST_INT_P (XEXP (varop, 1))
	      && (log2 = exact_log2 (UINTVAL (XEXP (varop, 1)))) >= 0)
	    {
	      rtx log2_rtx = gen_int_shift_amount (GET_MODE (varop), log2);
	      varop = simplify_gen_binary (ASHIFT, GET_MODE (varop),
					   XEXP (varop, 0), log2_rtx);
	      continue;
	    }
	  break;

	case UDIV:
	  /* Similar, for when divides are cheaper.  */
	  if (CONST_INT_P (XEXP (varop, 1))
	      && (log2 = exact_log2 (UINTVAL (XEXP (varop, 1)))) >= 0)
	    {
	      rtx log2_rtx = gen_int_shift_amount (GET_MODE (varop), log2);
	      varop = simplify_gen_binary (LSHIFTRT, GET_MODE (varop),
					   XEXP (varop, 0), log2_rtx);
	      continue;
	    }
	  break;

	case ASHIFTRT:
	  /* If we are extracting just the sign bit of an arithmetic
	     right shift, that shift is not needed.  However, the sign
	     bit of a wider mode may be different from what would be
	     interpreted as the sign bit in a narrower mode, so, if
	     the result is narrower, don't discard the shift.  */
	  if (code == LSHIFTRT
	      && count == (GET_MODE_UNIT_BITSIZE (result_mode) - 1)
	      && (GET_MODE_UNIT_BITSIZE (result_mode)
		  >= GET_MODE_UNIT_BITSIZE (GET_MODE (varop))))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* fall through */

	case LSHIFTRT:
	case ASHIFT:
	case ROTATE:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_mode = as_a <scalar_int_mode> (mode);
	  int_varop_mode = as_a <scalar_int_mode> (GET_MODE (varop));
	  int_result_mode = as_a <scalar_int_mode> (result_mode);

	  /* Here we have two nested shifts.  The result is usually the
	     AND of a new shift with a mask.  We compute the result below.  */
	  if (CONST_INT_P (XEXP (varop, 1))
	      && INTVAL (XEXP (varop, 1)) >= 0
	      && INTVAL (XEXP (varop, 1)) < GET_MODE_PRECISION (int_varop_mode)
	      && HWI_COMPUTABLE_MODE_P (int_result_mode)
	      && HWI_COMPUTABLE_MODE_P (int_mode))
	    {
	      enum rtx_code first_code = GET_CODE (varop);
	      unsigned int first_count = INTVAL (XEXP (varop, 1));
	      unsigned HOST_WIDE_INT mask;
	      rtx mask_rtx;

	      /* We have one common special case.  We can't do any merging if
		 the inner code is an ASHIFTRT of a smaller mode.  However, if
		 we have (ashift:M1 (subreg:M1 (ashiftrt:M2 FOO C1) 0) C2)
		 with C2 == GET_MODE_BITSIZE (M1) - GET_MODE_BITSIZE (M2),
		 we can convert it to
		 (ashiftrt:M1 (ashift:M1 (and:M1 (subreg:M1 FOO 0) C3) C2) C1).
		 This simplifies certain SIGN_EXTEND operations.  */
	      if (code == ASHIFT && first_code == ASHIFTRT
		  && count == (GET_MODE_PRECISION (int_result_mode)
			       - GET_MODE_PRECISION (int_varop_mode)))
		{
		  /* C3 has the low-order C1 bits zero.  */

		  mask = GET_MODE_MASK (int_mode)
			 & ~((HOST_WIDE_INT_1U << first_count) - 1);

		  varop = simplify_and_const_int (NULL_RTX, int_result_mode,
						  XEXP (varop, 0), mask);
		  varop = simplify_shift_const (NULL_RTX, ASHIFT,
						int_result_mode, varop, count);
		  count = first_count;
		  code = ASHIFTRT;
		  continue;
		}

	      /* If this was (ashiftrt (ashift foo C1) C2) and FOO has more
		 than C1 high-order bits equal to the sign bit, we can convert
		 this to either an ASHIFT or an ASHIFTRT depending on the
		 two counts.

		 We cannot do this if VAROP's mode is not SHIFT_UNIT_MODE.  */

	      if (code == ASHIFTRT && first_code == ASHIFT
		  && int_varop_mode == shift_unit_mode
		  && (num_sign_bit_copies (XEXP (varop, 0), shift_unit_mode)
		      > first_count))
		{
		  varop = XEXP (varop, 0);
		  count -= first_count;
		  if (count < 0)
		    {
		      count = -count;
		      code = ASHIFT;
		    }

		  continue;
		}

	      /* There are some cases we can't do.  If CODE is ASHIFTRT,
		 we can only do this if FIRST_CODE is also ASHIFTRT.

		 We can't do the case when CODE is ROTATE and FIRST_CODE is
		 ASHIFTRT.

		 If the mode of this shift is not the mode of the outer shift,
		 we can't do this if either shift is a right shift or ROTATE.

		 Finally, we can't do any of these if the mode is too wide
		 unless the codes are the same.

		 Handle the case where the shift codes are the same
		 first.  */

	      if (code == first_code)
		{
		  if (int_varop_mode != int_result_mode
		      && (code == ASHIFTRT || code == LSHIFTRT
			  || code == ROTATE))
		    break;

		  count += first_count;
		  varop = XEXP (varop, 0);
		  continue;
		}

	      if (code == ASHIFTRT
		  || (code == ROTATE && first_code == ASHIFTRT)
		  || GET_MODE_PRECISION (int_mode) > HOST_BITS_PER_WIDE_INT
		  || (int_varop_mode != int_result_mode
		      && (first_code == ASHIFTRT || first_code == LSHIFTRT
			  || first_code == ROTATE
			  || code == ROTATE)))
		break;

	      /* To compute the mask to apply after the shift, shift the
		 nonzero bits of the inner shift the same way the
		 outer shift will.  */

	      mask_rtx = gen_int_mode (nonzero_bits (varop, int_varop_mode),
				       int_result_mode);
	      rtx count_rtx = gen_int_shift_amount (int_result_mode, count);
	      mask_rtx
		= simplify_const_binary_operation (code, int_result_mode,
						   mask_rtx, count_rtx);

	      /* Give up if we can't compute an outer operation to use.  */
	      if (mask_rtx == 0
		  || !CONST_INT_P (mask_rtx)
		  || ! merge_outer_ops (&outer_op, &outer_const, AND,
					INTVAL (mask_rtx),
					int_result_mode, &complement_p))
		break;

	      /* If the shifts are in the same direction, we add the
		 counts.  Otherwise, we subtract them.  */
	      if ((code == ASHIFTRT || code == LSHIFTRT)
		  == (first_code == ASHIFTRT || first_code == LSHIFTRT))
		count += first_count;
	      else
		count -= first_count;

	      /* If COUNT is positive, the new shift is usually CODE,
		 except for the two exceptions below, in which case it is
		 FIRST_CODE.  If the count is negative, FIRST_CODE should
		 always be used  */
	      if (count > 0
		  && ((first_code == ROTATE && code == ASHIFT)
		      || (first_code == ASHIFTRT && code == LSHIFTRT)))
		code = first_code;
	      else if (count < 0)
		code = first_code, count = -count;

	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we have (A << B << C) for any shift, we can convert this to
	     (A << C << B).  This wins if A is a constant.  Only try this if
	     B is not a constant.  */

	  else if (GET_CODE (varop) == code
		   && CONST_INT_P (XEXP (varop, 0))
		   && !CONST_INT_P (XEXP (varop, 1)))
	    {
	      /* For ((unsigned) (cstULL >> count)) >> cst2 we have to make
		 sure the result will be masked.  See PR70222.  */
	      if (code == LSHIFTRT
		  && int_mode != int_result_mode
		  && !merge_outer_ops (&outer_op, &outer_const, AND,
				       GET_MODE_MASK (int_result_mode)
				       >> orig_count, int_result_mode,
				       &complement_p))
		break;
	      /* For ((int) (cstLL >> count)) >> cst2 just give up.  Queuing
		 up outer sign extension (often left and right shift) is
		 hardly more efficient than the original.  See PR70429.
		 Similarly punt for rotates with different modes.
		 See PR97386.  */
	      if ((code == ASHIFTRT || code == ROTATE)
		  && int_mode != int_result_mode)
		break;

	      rtx count_rtx = gen_int_shift_amount (int_result_mode, count);
	      rtx new_rtx = simplify_const_binary_operation (code, int_mode,
							     XEXP (varop, 0),
							     count_rtx);
	      varop = gen_rtx_fmt_ee (code, int_mode, new_rtx, XEXP (varop, 1));
	      count = 0;
	      continue;
	    }
	  break;

	case NOT:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;

	  /* Make this fit the case below.  */
	  varop = gen_rtx_XOR (mode, XEXP (varop, 0), constm1_rtx);
	  continue;

	case IOR:
	case AND:
	case XOR:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_varop_mode = as_a <scalar_int_mode> (GET_MODE (varop));
	  int_result_mode = as_a <scalar_int_mode> (result_mode);

	  /* If we have (xshiftrt (ior (plus X (const_int -1)) X) C)
	     with C the size of VAROP - 1 and the shift is logical if
	     STORE_FLAG_VALUE is 1 and arithmetic if STORE_FLAG_VALUE is -1,
	     we have an (le X 0) operation.   If we have an arithmetic shift
	     and STORE_FLAG_VALUE is 1 or we have a logical shift with
	     STORE_FLAG_VALUE of -1, we have a (neg (le X 0)) operation.  */

	  if (GET_CODE (varop) == IOR && GET_CODE (XEXP (varop, 0)) == PLUS
	      && XEXP (XEXP (varop, 0), 1) == constm1_rtx
	      && (STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	      && (code == LSHIFTRT || code == ASHIFTRT)
	      && count == (GET_MODE_PRECISION (int_varop_mode) - 1)
	      && rtx_equal_p (XEXP (XEXP (varop, 0), 0), XEXP (varop, 1)))
	    {
	      count = 0;
	      varop = gen_rtx_LE (int_varop_mode, XEXP (varop, 1),
				  const0_rtx);

	      if (STORE_FLAG_VALUE == 1 ? code == ASHIFTRT : code == LSHIFTRT)
		varop = gen_rtx_NEG (int_varop_mode, varop);

	      continue;
	    }

	  /* If we have (shift (logical)), move the logical to the outside
	     to allow it to possibly combine with another logical and the
	     shift to combine with another shift.  This also canonicalizes to
	     what a ZERO_EXTRACT looks like.  Also, some machines have
	     (and (shift)) insns.  */

	  if (CONST_INT_P (XEXP (varop, 1))
	      /* We can't do this if we have (ashiftrt (xor))  and the
		 constant has its sign bit set in shift_unit_mode with
		 shift_unit_mode wider than result_mode.  */
	      && !(code == ASHIFTRT && GET_CODE (varop) == XOR
		   && int_result_mode != shift_unit_mode
		   && trunc_int_for_mode (INTVAL (XEXP (varop, 1)),
					  shift_unit_mode) < 0)
	      && (new_rtx = simplify_const_binary_operation
		  (code, int_result_mode,
		   gen_int_mode (INTVAL (XEXP (varop, 1)), int_result_mode),
		   gen_int_shift_amount (int_result_mode, count))) != 0
	      && CONST_INT_P (new_rtx)
	      && merge_outer_ops (&outer_op, &outer_const, GET_CODE (varop),
				  INTVAL (new_rtx), int_result_mode,
				  &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we can't do that, try to simplify the shift in each arm of the
	     logical expression, make a new logical expression, and apply
	     the inverse distributive law.  This also can't be done for
	     (ashiftrt (xor)) where we've widened the shift and the constant
	     changes the sign bit.  */
	  if (CONST_INT_P (XEXP (varop, 1))
	      && !(code == ASHIFTRT && GET_CODE (varop) == XOR
		   && int_result_mode != shift_unit_mode
		   && trunc_int_for_mode (INTVAL (XEXP (varop, 1)),
					  shift_unit_mode) < 0))
	    {
	      rtx lhs = simplify_shift_const (NULL_RTX, code, shift_unit_mode,
					      XEXP (varop, 0), count);
	      rtx rhs = simplify_shift_const (NULL_RTX, code, shift_unit_mode,
					      XEXP (varop, 1), count);

	      varop = simplify_gen_binary (GET_CODE (varop), shift_unit_mode,
					   lhs, rhs);
	      varop = apply_distributive_law (varop);

	      count = 0;
	      continue;
	    }
	  break;

	case EQ:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_result_mode = as_a <scalar_int_mode> (result_mode);

	  /* Convert (lshiftrt (eq FOO 0) C) to (xor FOO 1) if STORE_FLAG_VALUE
	     says that the sign bit can be tested, FOO has mode MODE, C is
	     GET_MODE_PRECISION (MODE) - 1, and FOO has only its low-order bit
	     that may be nonzero.  */
	  if (code == LSHIFTRT
	      && XEXP (varop, 1) == const0_rtx
	      && GET_MODE (XEXP (varop, 0)) == int_result_mode
	      && count == (GET_MODE_PRECISION (int_result_mode) - 1)
	      && HWI_COMPUTABLE_MODE_P (int_result_mode)
	      && STORE_FLAG_VALUE == -1
	      && nonzero_bits (XEXP (varop, 0), int_result_mode) == 1
	      && merge_outer_ops (&outer_op, &outer_const, XOR, 1,
				  int_result_mode, &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      count = 0;
	      continue;
	    }
	  break;

	case NEG:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_result_mode = as_a <scalar_int_mode> (result_mode);

	  /* (lshiftrt (neg A) C) where A is either 0 or 1 and C is one less
	     than the number of bits in the mode is equivalent to A.  */
	  if (code == LSHIFTRT
	      && count == (GET_MODE_PRECISION (int_result_mode) - 1)
	      && nonzero_bits (XEXP (varop, 0), int_result_mode) == 1)
	    {
	      varop = XEXP (varop, 0);
	      count = 0;
	      continue;
	    }

	  /* NEG commutes with ASHIFT since it is multiplication.  Move the
	     NEG outside to allow shifts to combine.  */
	  if (code == ASHIFT
	      && merge_outer_ops (&outer_op, &outer_const, NEG, 0,
				  int_result_mode, &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;

	case PLUS:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_result_mode = as_a <scalar_int_mode> (result_mode);

	  /* (lshiftrt (plus A -1) C) where A is either 0 or 1 and C
	     is one less than the number of bits in the mode is
	     equivalent to (xor A 1).  */
	  if (code == LSHIFTRT
	      && count == (GET_MODE_PRECISION (int_result_mode) - 1)
	      && XEXP (varop, 1) == constm1_rtx
	      && nonzero_bits (XEXP (varop, 0), int_result_mode) == 1
	      && merge_outer_ops (&outer_op, &outer_const, XOR, 1,
				  int_result_mode, &complement_p))
	    {
	      count = 0;
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we have (xshiftrt (plus FOO BAR) C), and the only bits
	     that might be nonzero in BAR are those being shifted out and those
	     bits are known zero in FOO, we can replace the PLUS with FOO.
	     Similarly in the other operand order.  This code occurs when
	     we are computing the size of a variable-size array.  */

	  if ((code == ASHIFTRT || code == LSHIFTRT)
	      && count < HOST_BITS_PER_WIDE_INT
	      && nonzero_bits (XEXP (varop, 1), int_result_mode) >> count == 0
	      && (nonzero_bits (XEXP (varop, 1), int_result_mode)
		  & nonzero_bits (XEXP (varop, 0), int_result_mode)) == 0)
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  else if ((code == ASHIFTRT || code == LSHIFTRT)
		   && count < HOST_BITS_PER_WIDE_INT
		   && HWI_COMPUTABLE_MODE_P (int_result_mode)
		   && (nonzero_bits (XEXP (varop, 0), int_result_mode)
		       >> count) == 0
		   && (nonzero_bits (XEXP (varop, 0), int_result_mode)
		       & nonzero_bits (XEXP (varop, 1), int_result_mode)) == 0)
	    {
	      varop = XEXP (varop, 1);
	      continue;
	    }

	  /* (ashift (plus foo C) N) is (plus (ashift foo N) C').  */
	  if (code == ASHIFT
	      && CONST_INT_P (XEXP (varop, 1))
	      && (new_rtx = simplify_const_binary_operation
		  (ASHIFT, int_result_mode,
		   gen_int_mode (INTVAL (XEXP (varop, 1)), int_result_mode),
		   gen_int_shift_amount (int_result_mode, count))) != 0
	      && CONST_INT_P (new_rtx)
	      && merge_outer_ops (&outer_op, &outer_const, PLUS,
				  INTVAL (new_rtx), int_result_mode,
				  &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* Check for 'PLUS signbit', which is the canonical form of 'XOR
	     signbit', and attempt to change the PLUS to an XOR and move it to
	     the outer operation as is done above in the AND/IOR/XOR case
	     leg for shift(logical). See details in logical handling above
	     for reasoning in doing so.  */
	  if (code == LSHIFTRT
	      && CONST_INT_P (XEXP (varop, 1))
	      && mode_signbit_p (int_result_mode, XEXP (varop, 1))
	      && (new_rtx = simplify_const_binary_operation
		  (code, int_result_mode,
		   gen_int_mode (INTVAL (XEXP (varop, 1)), int_result_mode),
		   gen_int_shift_amount (int_result_mode, count))) != 0
	      && CONST_INT_P (new_rtx)
	      && merge_outer_ops (&outer_op, &outer_const, XOR,
				  INTVAL (new_rtx), int_result_mode,
				  &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  break;

	case MINUS:
	  /* The following rules apply only to scalars.  */
	  if (shift_mode != shift_unit_mode)
	    break;
	  int_varop_mode = as_a <scalar_int_mode> (GET_MODE (varop));

	  /* If we have (xshiftrt (minus (ashiftrt X C)) X) C)
	     with C the size of VAROP - 1 and the shift is logical if
	     STORE_FLAG_VALUE is 1 and arithmetic if STORE_FLAG_VALUE is -1,
	     we have a (gt X 0) operation.  If the shift is arithmetic with
	     STORE_FLAG_VALUE of 1 or logical with STORE_FLAG_VALUE == -1,
	     we have a (neg (gt X 0)) operation.  */

	  if ((STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	      && GET_CODE (XEXP (varop, 0)) == ASHIFTRT
	      && count == (GET_MODE_PRECISION (int_varop_mode) - 1)
	      && (code == LSHIFTRT || code == ASHIFTRT)
	      && CONST_INT_P (XEXP (XEXP (varop, 0), 1))
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) == count
	      && rtx_equal_p (XEXP (XEXP (varop, 0), 0), XEXP (varop, 1)))
	    {
	      count = 0;
	      varop = gen_rtx_GT (int_varop_mode, XEXP (varop, 1),
				  const0_rtx);

	      if (STORE_FLAG_VALUE == 1 ? code == ASHIFTRT : code == LSHIFTRT)
		varop = gen_rtx_NEG (int_varop_mode, varop);

	      continue;
	    }
	  break;

	case TRUNCATE:
	  /* Change (lshiftrt (truncate (lshiftrt))) to (truncate (lshiftrt))
	     if the truncate does not affect the value.  */
	  if (code == LSHIFTRT
	      && GET_CODE (XEXP (varop, 0)) == LSHIFTRT
	      && CONST_INT_P (XEXP (XEXP (varop, 0), 1))
	      && (INTVAL (XEXP (XEXP (varop, 0), 1))
		  >= (GET_MODE_UNIT_PRECISION (GET_MODE (XEXP (varop, 0)))
		      - GET_MODE_UNIT_PRECISION (GET_MODE (varop)))))
	    {
	      rtx varop_inner = XEXP (varop, 0);
	      int new_count = count + INTVAL (XEXP (varop_inner, 1));
	      rtx new_count_rtx = gen_int_shift_amount (GET_MODE (varop_inner),
							new_count);
	      varop_inner = gen_rtx_LSHIFTRT (GET_MODE (varop_inner),
					      XEXP (varop_inner, 0),
					      new_count_rtx);
	      varop = gen_rtx_TRUNCATE (GET_MODE (varop), varop_inner);
	      count = 0;
	      continue;
	    }
	  break;

	default:
	  break;
	}

      break;
    }

  shift_mode = result_mode;
  if (shift_mode != mode)
    {
      /* We only change the modes of scalar shifts.  */
      int_mode = as_a <scalar_int_mode> (mode);
      int_result_mode = as_a <scalar_int_mode> (result_mode);
      shift_mode = try_widen_shift_mode (code, varop, count, int_result_mode,
					 int_mode, outer_op, outer_const);
    }

  /* We have now finished analyzing the shift.  The result should be
     a shift of type CODE with SHIFT_MODE shifting VAROP COUNT places.  If
     OUTER_OP is non-UNKNOWN, it is an operation that needs to be applied
     to the result of the shift.  OUTER_CONST is the relevant constant,
     but we must turn off all bits turned off in the shift.  */

  if (outer_op == UNKNOWN
      && orig_code == code && orig_count == count
      && varop == orig_varop
      && shift_mode == GET_MODE (varop))
    return NULL_RTX;

  /* Make a SUBREG if necessary.  If we can't make it, fail.  */
  varop = gen_lowpart (shift_mode, varop);
  if (varop == NULL_RTX || GET_CODE (varop) == CLOBBER)
    return NULL_RTX;

  /* If we have an outer operation and we just made a shift, it is
     possible that we could have simplified the shift were it not
     for the outer operation.  So try to do the simplification
     recursively.  */

  if (outer_op != UNKNOWN)
    x = simplify_shift_const_1 (code, shift_mode, varop, count);
  else
    x = NULL_RTX;

  if (x == NULL_RTX)
    x = simplify_gen_binary (code, shift_mode, varop,
			     gen_int_shift_amount (shift_mode, count));

  /* If we were doing an LSHIFTRT in a wider mode than it was originally,
     turn off all the bits that the shift would have turned off.  */
  if (orig_code == LSHIFTRT && result_mode != shift_mode)
    /* We only change the modes of scalar shifts.  */
    x = simplify_and_const_int (NULL_RTX, as_a <scalar_int_mode> (shift_mode),
				x, GET_MODE_MASK (result_mode) >> orig_count);

  /* Do the remainder of the processing in RESULT_MODE.  */
  x = gen_lowpart_or_truncate (result_mode, x);

  /* If COMPLEMENT_P is set, we have to complement X before doing the outer
     operation.  */
  if (complement_p)
    x = simplify_gen_unary (NOT, result_mode, x, result_mode);

  if (outer_op != UNKNOWN)
    {
      int_result_mode = as_a <scalar_int_mode> (result_mode);

      if (GET_RTX_CLASS (outer_op) != RTX_UNARY
	  && GET_MODE_PRECISION (int_result_mode) < HOST_BITS_PER_WIDE_INT)
	outer_const = trunc_int_for_mode (outer_const, int_result_mode);

      if (outer_op == AND)
	x = simplify_and_const_int (NULL_RTX, int_result_mode, x, outer_const);
      else if (outer_op == SET)
	{
	  /* This means that we have determined that the result is
	     equivalent to a constant.  This should be rare.  */
	  if (!side_effects_p (x))
	    x = GEN_INT (outer_const);
	}
      else if (GET_RTX_CLASS (outer_op) == RTX_UNARY)
	x = simplify_gen_unary (outer_op, int_result_mode, x, int_result_mode);
      else
	x = simplify_gen_binary (outer_op, int_result_mode, x,
				 GEN_INT (outer_const));
    }

  return x;
}

/* Simplify a shift of VAROP by COUNT bits.  CODE says what kind of shift.
   The result of the shift is RESULT_MODE.  If we cannot simplify it,
   return X or, if it is NULL, synthesize the expression with
   simplify_gen_binary.  Otherwise, return a simplified value.

   The shift is normally computed in the widest mode we find in VAROP, as
   long as it isn't a different number of words than RESULT_MODE.  Exceptions
   are ASHIFTRT and ROTATE, which are always done in their original mode.  */

static rtx
simplify_shift_const (rtx x, enum rtx_code code, machine_mode result_mode,
		      rtx varop, int count)
{
  rtx tem = simplify_shift_const_1 (code, result_mode, varop, count);
  if (tem)
    return tem;

  if (!x)
    x = simplify_gen_binary (code, GET_MODE (varop), varop,
			     gen_int_shift_amount (GET_MODE (varop), count));
  if (GET_MODE (x) != result_mode)
    x = gen_lowpart (result_mode, x);
  return x;
}


/* A subroutine of recog_for_combine.  See there for arguments and
   return value.  */

static int
recog_for_combine_1 (rtx *pnewpat, rtx_insn *insn, rtx *pnotes)
{
  rtx pat = *pnewpat;
  rtx pat_without_clobbers;
  int insn_code_number;
  int num_clobbers_to_add = 0;
  int i;
  rtx notes = NULL_RTX;
  rtx old_notes, old_pat;
  int old_icode;

  /* If PAT is a PARALLEL, check to see if it contains the CLOBBER
     we use to indicate that something didn't match.  If we find such a
     thing, force rejection.  */
  if (GET_CODE (pat) == PARALLEL)
    for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
      if (GET_CODE (XVECEXP (pat, 0, i)) == CLOBBER
	  && XEXP (XVECEXP (pat, 0, i), 0) == const0_rtx)
	return -1;

  old_pat = PATTERN (insn);
  old_notes = REG_NOTES (insn);
  PATTERN (insn) = pat;
  REG_NOTES (insn) = NULL_RTX;

  insn_code_number = recog (pat, insn, &num_clobbers_to_add);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (insn_code_number < 0)
	fputs ("Failed to match this instruction:\n", dump_file);
      else
	fputs ("Successfully matched this instruction:\n", dump_file);
      print_rtl_single (dump_file, pat);
    }

  /* If it isn't, there is the possibility that we previously had an insn
     that clobbered some register as a side effect, but the combined
     insn doesn't need to do that.  So try once more without the clobbers
     unless this represents an ASM insn.  */

  if (insn_code_number < 0 && ! check_asm_operands (pat)
      && GET_CODE (pat) == PARALLEL)
    {
      int pos;

      for (pos = 0, i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) != CLOBBER)
	  {
	    if (i != pos)
	      SUBST (XVECEXP (pat, 0, pos), XVECEXP (pat, 0, i));
	    pos++;
	  }

      SUBST_INT (XVECLEN (pat, 0), pos);

      if (pos == 1)
	pat = XVECEXP (pat, 0, 0);

      PATTERN (insn) = pat;
      insn_code_number = recog (pat, insn, &num_clobbers_to_add);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (insn_code_number < 0)
	    fputs ("Failed to match this instruction:\n", dump_file);
	  else
	    fputs ("Successfully matched this instruction:\n", dump_file);
	  print_rtl_single (dump_file, pat);
	}
    }

  pat_without_clobbers = pat;

  PATTERN (insn) = old_pat;
  REG_NOTES (insn) = old_notes;

  /* Recognize all noop sets, these will be killed by followup pass.  */
  if (insn_code_number < 0 && GET_CODE (pat) == SET && set_noop_p (pat))
    insn_code_number = NOOP_MOVE_INSN_CODE, num_clobbers_to_add = 0;

  /* If we had any clobbers to add, make a new pattern than contains
     them.  Then check to make sure that all of them are dead.  */
  if (num_clobbers_to_add)
    {
      rtx newpat = gen_rtx_PARALLEL (VOIDmode,
				     rtvec_alloc (GET_CODE (pat) == PARALLEL
						  ? (XVECLEN (pat, 0)
						     + num_clobbers_to_add)
						  : num_clobbers_to_add + 1));

      if (GET_CODE (pat) == PARALLEL)
	for (i = 0; i < XVECLEN (pat, 0); i++)
	  XVECEXP (newpat, 0, i) = XVECEXP (pat, 0, i);
      else
	XVECEXP (newpat, 0, 0) = pat;

      add_clobbers (newpat, insn_code_number);

      for (i = XVECLEN (newpat, 0) - num_clobbers_to_add;
	   i < XVECLEN (newpat, 0); i++)
	{
	  if (REG_P (XEXP (XVECEXP (newpat, 0, i), 0))
	      && ! reg_dead_at_p (XEXP (XVECEXP (newpat, 0, i), 0), insn))
	    return -1;
	  if (GET_CODE (XEXP (XVECEXP (newpat, 0, i), 0)) != SCRATCH)
	    {
	      gcc_assert (REG_P (XEXP (XVECEXP (newpat, 0, i), 0)));
	      notes = alloc_reg_note (REG_UNUSED,
				      XEXP (XVECEXP (newpat, 0, i), 0), notes);
	    }
	}
      pat = newpat;
    }

  if (insn_code_number >= 0
      && insn_code_number != NOOP_MOVE_INSN_CODE)
    {
      old_pat = PATTERN (insn);
      old_notes = REG_NOTES (insn);
      old_icode = INSN_CODE (insn);
      PATTERN (insn) = pat;
      REG_NOTES (insn) = notes;
      INSN_CODE (insn) = insn_code_number;

      /* Allow targets to reject combined insn.  */
      if (!targetm.legitimate_combined_insn (insn))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fputs ("Instruction not appropriate for target.",
		   dump_file);

	  /* Callers expect recog_for_combine to strip
	     clobbers from the pattern on failure.  */
	  pat = pat_without_clobbers;
	  notes = NULL_RTX;

	  insn_code_number = -1;
	}

      PATTERN (insn) = old_pat;
      REG_NOTES (insn) = old_notes;
      INSN_CODE (insn) = old_icode;
    }

  *pnewpat = pat;
  *pnotes = notes;

  return insn_code_number;
}

/* Change every ZERO_EXTRACT and ZERO_EXTEND of a SUBREG that can be
   expressed as an AND and maybe an LSHIFTRT, to that formulation.
   Return whether anything was so changed.  */

static bool
change_zero_ext (rtx pat)
{
  bool changed = false;
  rtx *src = &SET_SRC (pat);

  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, src, NONCONST)
    {
      rtx x = **iter;
      scalar_int_mode mode, inner_mode;
      if (!is_a <scalar_int_mode> (GET_MODE (x), &mode))
	continue;
      int size;

      if (GET_CODE (x) == ZERO_EXTRACT
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2))
	  && is_a <scalar_int_mode> (GET_MODE (XEXP (x, 0)), &inner_mode)
	  && GET_MODE_PRECISION (inner_mode) <= GET_MODE_PRECISION (mode))
	{
	  size = INTVAL (XEXP (x, 1));

	  int start = INTVAL (XEXP (x, 2));
	  if (BITS_BIG_ENDIAN)
	    start = GET_MODE_PRECISION (inner_mode) - size - start;

	  if (start != 0)
	    x = gen_rtx_LSHIFTRT (inner_mode, XEXP (x, 0),
				  gen_int_shift_amount (inner_mode, start));
	  else
	    x = XEXP (x, 0);

	  if (mode != inner_mode)
	    {
	      if (REG_P (x) && HARD_REGISTER_P (x)
		  && !can_change_dest_mode (x, 0, mode))
		continue;

	      x = gen_lowpart_SUBREG (mode, x);
	    }
	}
      else if (GET_CODE (x) == ZERO_EXTEND
	       && GET_CODE (XEXP (x, 0)) == SUBREG
	       && SCALAR_INT_MODE_P (GET_MODE (SUBREG_REG (XEXP (x, 0))))
	       && !paradoxical_subreg_p (XEXP (x, 0))
	       && subreg_lowpart_p (XEXP (x, 0)))
	{
	  inner_mode = as_a <scalar_int_mode> (GET_MODE (XEXP (x, 0)));
	  size = GET_MODE_PRECISION (inner_mode);
	  x = SUBREG_REG (XEXP (x, 0));
	  if (GET_MODE (x) != mode)
	    {
	      if (REG_P (x) && HARD_REGISTER_P (x)
		  && !can_change_dest_mode (x, 0, mode))
		continue;

	      x = gen_lowpart_SUBREG (mode, x);
	    }
	}
      else if (GET_CODE (x) == ZERO_EXTEND
	       && REG_P (XEXP (x, 0))
	       && HARD_REGISTER_P (XEXP (x, 0))
	       && can_change_dest_mode (XEXP (x, 0), 0, mode))
	{
	  inner_mode = as_a <scalar_int_mode> (GET_MODE (XEXP (x, 0)));
	  size = GET_MODE_PRECISION (inner_mode);
	  x = gen_rtx_REG (mode, REGNO (XEXP (x, 0)));
	}
      else
	continue;

      if (!(GET_CODE (x) == LSHIFTRT
	    && CONST_INT_P (XEXP (x, 1))
	    && size + INTVAL (XEXP (x, 1)) == GET_MODE_PRECISION (mode)))
	{
	  wide_int mask = wi::mask (size, false, GET_MODE_PRECISION (mode));
	  x = gen_rtx_AND (mode, x, immed_wide_int_const (mask, mode));
	}

      SUBST (**iter, x);
      changed = true;
    }

  if (changed)
    FOR_EACH_SUBRTX_PTR (iter, array, src, NONCONST)
      maybe_swap_commutative_operands (**iter);

  rtx *dst = &SET_DEST (pat);
  scalar_int_mode mode;
  if (GET_CODE (*dst) == ZERO_EXTRACT
      && REG_P (XEXP (*dst, 0))
      && is_a <scalar_int_mode> (GET_MODE (XEXP (*dst, 0)), &mode)
      && CONST_INT_P (XEXP (*dst, 1))
      && CONST_INT_P (XEXP (*dst, 2)))
    {
      rtx reg = XEXP (*dst, 0);
      int width = INTVAL (XEXP (*dst, 1));
      int offset = INTVAL (XEXP (*dst, 2));
      int reg_width = GET_MODE_PRECISION (mode);
      if (BITS_BIG_ENDIAN)
	offset = reg_width - width - offset;

      rtx x, y, z, w;
      wide_int mask = wi::shifted_mask (offset, width, true, reg_width);
      wide_int mask2 = wi::shifted_mask (offset, width, false, reg_width);
      x = gen_rtx_AND (mode, reg, immed_wide_int_const (mask, mode));
      if (offset)
	y = gen_rtx_ASHIFT (mode, SET_SRC (pat), GEN_INT (offset));
      else
	y = SET_SRC (pat);
      z = gen_rtx_AND (mode, y, immed_wide_int_const (mask2, mode));
      w = gen_rtx_IOR (mode, x, z);
      SUBST (SET_DEST (pat), reg);
      SUBST (SET_SRC (pat), w);

      changed = true;
    }

  return changed;
}

/* Like recog, but we receive the address of a pointer to a new pattern.
   We try to match the rtx that the pointer points to.
   If that fails, we may try to modify or replace the pattern,
   storing the replacement into the same pointer object.

   Modifications include deletion or addition of CLOBBERs.  If the
   instruction will still not match, we change ZERO_EXTEND and ZERO_EXTRACT
   to the equivalent AND and perhaps LSHIFTRT patterns, and try with that
   (and undo if that fails).

   PNOTES is a pointer to a location where any REG_UNUSED notes added for
   the CLOBBERs are placed.

   The value is the final insn code from the pattern ultimately matched,
   or -1.  */

static int
recog_for_combine (rtx *pnewpat, rtx_insn *insn, rtx *pnotes)
{
  rtx pat = *pnewpat;
  int insn_code_number = recog_for_combine_1 (pnewpat, insn, pnotes);
  if (insn_code_number >= 0 || check_asm_operands (pat))
    return insn_code_number;

  void *marker = get_undo_marker ();
  bool changed = false;

  if (GET_CODE (pat) == SET)
    {
      /* For an unrecognized single set of a constant, try placing it in
	 the constant pool, if this function already uses one.  */
      rtx src = SET_SRC (pat);
      if (CONSTANT_P (src)
	  && !CONST_INT_P (src)
	  && crtl->uses_const_pool)
	{
	  machine_mode mode = GET_MODE (src);
	  if (mode == VOIDmode)
	    mode = GET_MODE (SET_DEST (pat));
	  src = force_const_mem (mode, src);
	  if (src)
	    {
	      SUBST (SET_SRC (pat), src);
	      changed = true;
	    }
	}
      else
	changed = change_zero_ext (pat);
    }
  else if (GET_CODE (pat) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx set = XVECEXP (pat, 0, i);
	  if (GET_CODE (set) == SET)
	    changed |= change_zero_ext (set);
	}
    }

  if (changed)
    {
      insn_code_number = recog_for_combine_1 (pnewpat, insn, pnotes);

      if (insn_code_number < 0)
	undo_to_marker (marker);
    }

  return insn_code_number;
}

/* Like gen_lowpart_general but for use by combine.  In combine it
   is not possible to create any new pseudoregs.  However, it is
   safe to create invalid memory addresses, because combine will
   try to recognize them and all they will do is make the combine
   attempt fail.

   If for some reason this cannot do its job, an rtx
   (clobber (const_int 0)) is returned.
   An insn containing that will not be recognized.  */

static rtx
gen_lowpart_for_combine (machine_mode omode, rtx x)
{
  machine_mode imode = GET_MODE (x);
  rtx result;

  if (omode == imode)
    return x;

  /* We can only support MODE being wider than a word if X is a
     constant integer or has a mode the same size.  */
  if (maybe_gt (GET_MODE_SIZE (omode), UNITS_PER_WORD)
      && ! (CONST_SCALAR_INT_P (x)
	    || known_eq (GET_MODE_SIZE (imode), GET_MODE_SIZE (omode))))
    goto fail;

  /* X might be a paradoxical (subreg (mem)).  In that case, gen_lowpart
     won't know what to do.  So we will strip off the SUBREG here and
     process normally.  */
  if (GET_CODE (x) == SUBREG && MEM_P (SUBREG_REG (x)))
    {
      x = SUBREG_REG (x);

      /* For use in case we fall down into the address adjustments
	 further below, we need to adjust the known mode and size of
	 x; imode and isize, since we just adjusted x.  */
      imode = GET_MODE (x);

      if (imode == omode)
	return x;
    }

  result = gen_lowpart_common (omode, x);

  if (result)
    return result;

  if (MEM_P (x))
    {
      /* Refuse to work on a volatile memory ref or one with a mode-dependent
	 address.  */
      if (MEM_VOLATILE_P (x)
	  || mode_dependent_address_p (XEXP (x, 0), MEM_ADDR_SPACE (x)))
	goto fail;

      /* If we want to refer to something bigger than the original memref,
	 generate a paradoxical subreg instead.  That will force a reload
	 of the original memref X.  */
      if (paradoxical_subreg_p (omode, imode))
	return gen_rtx_SUBREG (omode, x, 0);

      poly_int64 offset = byte_lowpart_offset (omode, imode);
      return adjust_address_nv (x, omode, offset);
    }

  /* If X is a comparison operator, rewrite it in a new mode.  This
     probably won't match, but may allow further simplifications.  */
  else if (COMPARISON_P (x)
	   && SCALAR_INT_MODE_P (imode)
	   && SCALAR_INT_MODE_P (omode))
    return gen_rtx_fmt_ee (GET_CODE (x), omode, XEXP (x, 0), XEXP (x, 1));

  /* If we couldn't simplify X any other way, just enclose it in a
     SUBREG.  Normally, this SUBREG won't match, but some patterns may
     include an explicit SUBREG or we may simplify it further in combine.  */
  else
    {
      rtx res;

      if (imode == VOIDmode)
	{
	  imode = int_mode_for_mode (omode).require ();
	  x = gen_lowpart_common (imode, x);
	  if (x == NULL)
	    goto fail;
	}
      res = lowpart_subreg (omode, x, imode);
      if (res)
	return res;
    }

 fail:
  return gen_rtx_CLOBBER (omode, const0_rtx);
}

/* Try to simplify a comparison between OP0 and a constant OP1,
   where CODE is the comparison code that will be tested, into a
   (CODE OP0 const0_rtx) form.

   The result is a possibly different comparison code to use.
   *POP0 and *POP1 may be updated.  */

static enum rtx_code
simplify_compare_const (enum rtx_code code, machine_mode mode,
			rtx *pop0, rtx *pop1)
{
  scalar_int_mode int_mode;
  rtx op0 = *pop0;
  HOST_WIDE_INT const_op = INTVAL (*pop1);

  /* Get the constant we are comparing against and turn off all bits
     not on in our mode.  */
  if (mode != VOIDmode)
    const_op = trunc_int_for_mode (const_op, mode);

  /* If we are comparing against a constant power of two and the value
     being compared can only have that single bit nonzero (e.g., it was
     `and'ed with that bit), we can replace this with a comparison
     with zero.  */
  if (const_op
      && (code == EQ || code == NE || code == GE || code == GEU
	  || code == LT || code == LTU)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
      && pow2p_hwi (const_op & GET_MODE_MASK (int_mode))
      && (nonzero_bits (op0, int_mode)
	  == (unsigned HOST_WIDE_INT) (const_op & GET_MODE_MASK (int_mode))))
    {
      code = (code == EQ || code == GE || code == GEU ? NE : EQ);
      const_op = 0;
    }

  /* Similarly, if we are comparing a value known to be either -1 or
     0 with -1, change it to the opposite comparison against zero.  */
  if (const_op == -1
      && (code == EQ || code == NE || code == GT || code == LE
	  || code == GEU || code == LTU)
      && is_a <scalar_int_mode> (mode, &int_mode)
      && num_sign_bit_copies (op0, int_mode) == GET_MODE_PRECISION (int_mode))
    {
      code = (code == EQ || code == LE || code == GEU ? NE : EQ);
      const_op = 0;
    }

  /* Do some canonicalizations based on the comparison code.  We prefer
     comparisons against zero and then prefer equality comparisons.
     If we can reduce the size of a constant, we will do that too.  */
  switch (code)
    {
    case LT:
      /* < C is equivalent to <= (C - 1) */
      if (const_op > 0)
	{
	  const_op -= 1;
	  code = LE;
	  /* ... fall through to LE case below.  */
	  gcc_fallthrough ();
	}
      else
	break;

    case LE:
      /* <= C is equivalent to < (C + 1); we do this for C < 0  */
      if (const_op < 0)
	{
	  const_op += 1;
	  code = LT;
	}

      /* If we are doing a <= 0 comparison on a value known to have
	 a zero sign bit, we can replace this with == 0.  */
      else if (const_op == 0
	       && is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && (nonzero_bits (op0, int_mode)
		   & (HOST_WIDE_INT_1U << (GET_MODE_PRECISION (int_mode) - 1)))
	       == 0)
	code = EQ;
      break;

    case GE:
      /* >= C is equivalent to > (C - 1).  */
      if (const_op > 0)
	{
	  const_op -= 1;
	  code = GT;
	  /* ... fall through to GT below.  */
	  gcc_fallthrough ();
	}
      else
	break;

    case GT:
      /* > C is equivalent to >= (C + 1); we do this for C < 0.  */
      if (const_op < 0)
	{
	  const_op += 1;
	  code = GE;
	}

      /* If we are doing a > 0 comparison on a value known to have
	 a zero sign bit, we can replace this with != 0.  */
      else if (const_op == 0
	       && is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && (nonzero_bits (op0, int_mode)
		   & (HOST_WIDE_INT_1U << (GET_MODE_PRECISION (int_mode) - 1)))
	       == 0)
	code = NE;
      break;

    case LTU:
      /* < C is equivalent to <= (C - 1).  */
      if (const_op > 0)
	{
	  const_op -= 1;
	  code = LEU;
	  /* ... fall through ...  */
	  gcc_fallthrough ();
	}
      /* (unsigned) < 0x80000000 is equivalent to >= 0.  */
      else if (is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && ((unsigned HOST_WIDE_INT) const_op
		   == HOST_WIDE_INT_1U << (GET_MODE_PRECISION (int_mode) - 1)))
	{
	  const_op = 0;
	  code = GE;
	  break;
	}
      else
	break;

    case LEU:
      /* unsigned <= 0 is equivalent to == 0 */
      if (const_op == 0)
	code = EQ;
      /* (unsigned) <= 0x7fffffff is equivalent to >= 0.  */
      else if (is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && ((unsigned HOST_WIDE_INT) const_op
		   == ((HOST_WIDE_INT_1U
			<< (GET_MODE_PRECISION (int_mode) - 1)) - 1)))
	{
	  const_op = 0;
	  code = GE;
	}
      break;

    case GEU:
      /* >= C is equivalent to > (C - 1).  */
      if (const_op > 1)
	{
	  const_op -= 1;
	  code = GTU;
	  /* ... fall through ...  */
	  gcc_fallthrough ();
	}

      /* (unsigned) >= 0x80000000 is equivalent to < 0.  */
      else if (is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && ((unsigned HOST_WIDE_INT) const_op
		   == HOST_WIDE_INT_1U << (GET_MODE_PRECISION (int_mode) - 1)))
	{
	  const_op = 0;
	  code = LT;
	  break;
	}
      else
	break;

    case GTU:
      /* unsigned > 0 is equivalent to != 0 */
      if (const_op == 0)
	code = NE;
      /* (unsigned) > 0x7fffffff is equivalent to < 0.  */
      else if (is_a <scalar_int_mode> (mode, &int_mode)
	       && GET_MODE_PRECISION (int_mode) - 1 < HOST_BITS_PER_WIDE_INT
	       && ((unsigned HOST_WIDE_INT) const_op
		   == (HOST_WIDE_INT_1U
		       << (GET_MODE_PRECISION (int_mode) - 1)) - 1))
	{
	  const_op = 0;
	  code = LT;
	}
      break;

    default:
      break;
    }

  /* Narrow non-symmetric comparison of memory and constant as e.g.
     x0...x7 <= 0x3fffffffffffffff into x0 <= 0x3f where x0 is the most
     significant byte.  Likewise, transform x0...x7 >= 0x4000000000000000 into
     x0 >= 0x40.  */
  if ((code == LEU || code == LTU || code == GEU || code == GTU)
      && is_a <scalar_int_mode> (GET_MODE (op0), &int_mode)
      && HWI_COMPUTABLE_MODE_P (int_mode)
      && MEM_P (op0)
      && !MEM_VOLATILE_P (op0)
      /* The optimization makes only sense for constants which are big enough
	 so that we have a chance to chop off something at all.  */
      && (unsigned HOST_WIDE_INT) const_op > 0xff
      /* Bail out, if the constant does not fit into INT_MODE.  */
      && (unsigned HOST_WIDE_INT) const_op
	 < ((HOST_WIDE_INT_1U << (GET_MODE_PRECISION (int_mode) - 1) << 1) - 1)
      /* Ensure that we do not overflow during normalization.  */
      && (code != GTU || (unsigned HOST_WIDE_INT) const_op < HOST_WIDE_INT_M1U))
    {
      unsigned HOST_WIDE_INT n = (unsigned HOST_WIDE_INT) const_op;
      enum rtx_code adjusted_code;

      /* Normalize code to either LEU or GEU.  */
      if (code == LTU)
	{
	  --n;
	  adjusted_code = LEU;
	}
      else if (code == GTU)
	{
	  ++n;
	  adjusted_code = GEU;
	}
      else
	adjusted_code = code;

      scalar_int_mode narrow_mode_iter;
      FOR_EACH_MODE_UNTIL (narrow_mode_iter, int_mode)
	{
	  unsigned nbits = GET_MODE_PRECISION (int_mode)
			   - GET_MODE_PRECISION (narrow_mode_iter);
	  unsigned HOST_WIDE_INT mask = (HOST_WIDE_INT_1U << nbits) - 1;
	  unsigned HOST_WIDE_INT lower_bits = n & mask;
	  if ((adjusted_code == LEU && lower_bits == mask)
	      || (adjusted_code == GEU && lower_bits == 0))
	    {
	      n >>= nbits;
	      break;
	    }
	}

      if (narrow_mode_iter < int_mode)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (
		dump_file, "narrow comparison from mode %s to %s: (MEM %s "
		HOST_WIDE_INT_PRINT_HEX ") to (MEM %s "
		HOST_WIDE_INT_PRINT_HEX ").\n", GET_MODE_NAME (int_mode),
		GET_MODE_NAME (narrow_mode_iter), GET_RTX_NAME (code),
		(unsigned HOST_WIDE_INT)const_op, GET_RTX_NAME (adjusted_code),
		n);
	    }
	  poly_int64 offset = (BYTES_BIG_ENDIAN
			       ? 0
			       : (GET_MODE_SIZE (int_mode)
				  - GET_MODE_SIZE (narrow_mode_iter)));
	  *pop0 = adjust_address_nv (op0, narrow_mode_iter, offset);
	  *pop1 = GEN_INT (n);
	  return adjusted_code;
	}
    }

  *pop1 = GEN_INT (const_op);
  return code;
}

/* Simplify a comparison between *POP0 and *POP1 where CODE is the
   comparison code that will be tested.

   The result is a possibly different comparison code to use.  *POP0 and
   *POP1 may be updated.

   It is possible that we might detect that a comparison is either always
   true or always false.  However, we do not perform general constant
   folding in combine, so this knowledge isn't useful.  Such tautologies
   should have been detected earlier.  Hence we ignore all such cases.  */

static enum rtx_code
simplify_comparison (enum rtx_code code, rtx *pop0, rtx *pop1)
{
  rtx op0 = *pop0;
  rtx op1 = *pop1;
  rtx tem, tem1;
  int i;
  scalar_int_mode mode, inner_mode, tmode;
  opt_scalar_int_mode tmode_iter;

  /* Try a few ways of applying the same transformation to both operands.  */
  while (1)
    {
      /* The test below this one won't handle SIGN_EXTENDs on these machines,
	 so check specially.  */
      if (!WORD_REGISTER_OPERATIONS
	  && code != GTU && code != GEU && code != LTU && code != LEU
	  && GET_CODE (op0) == ASHIFTRT && GET_CODE (op1) == ASHIFTRT
	  && GET_CODE (XEXP (op0, 0)) == ASHIFT
	  && GET_CODE (XEXP (op1, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (op0, 0), 0)) == SUBREG
	  && GET_CODE (XEXP (XEXP (op1, 0), 0)) == SUBREG
	  && is_a <scalar_int_mode> (GET_MODE (op0), &mode)
	  && (is_a <scalar_int_mode>
	      (GET_MODE (SUBREG_REG (XEXP (XEXP (op0, 0), 0))), &inner_mode))
	  && inner_mode == GET_MODE (SUBREG_REG (XEXP (XEXP (op1, 0), 0)))
	  && CONST_INT_P (XEXP (op0, 1))
	  && XEXP (op0, 1) == XEXP (op1, 1)
	  && XEXP (op0, 1) == XEXP (XEXP (op0, 0), 1)
	  && XEXP (op0, 1) == XEXP (XEXP (op1, 0), 1)
	  && (INTVAL (XEXP (op0, 1))
	      == (GET_MODE_PRECISION (mode)
		  - GET_MODE_PRECISION (inner_mode))))
	{
	  op0 = SUBREG_REG (XEXP (XEXP (op0, 0), 0));
	  op1 = SUBREG_REG (XEXP (XEXP (op1, 0), 0));
	}

      /* If both operands are the same constant shift, see if we can ignore the
	 shift.  We can if the shift is a rotate or if the bits shifted out of
	 this shift are known to be zero for both inputs and if the type of
	 comparison is compatible with the shift.  */
      if (GET_CODE (op0) == GET_CODE (op1)
	  && HWI_COMPUTABLE_MODE_P (GET_MODE (op0))
	  && ((GET_CODE (op0) == ROTATE && (code == NE || code == EQ))
	      || ((GET_CODE (op0) == LSHIFTRT || GET_CODE (op0) == ASHIFT)
		  && (code != GT && code != LT && code != GE && code != LE))
	      || (GET_CODE (op0) == ASHIFTRT
		  && (code != GTU && code != LTU
		      && code != GEU && code != LEU)))
	  && CONST_INT_P (XEXP (op0, 1))
	  && INTVAL (XEXP (op0, 1)) >= 0
	  && INTVAL (XEXP (op0, 1)) < HOST_BITS_PER_WIDE_INT
	  && XEXP (op0, 1) == XEXP (op1, 1))
	{
	  machine_mode mode = GET_MODE (op0);
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);
	  int shift_count = INTVAL (XEXP (op0, 1));

	  if (GET_CODE (op0) == LSHIFTRT || GET_CODE (op0) == ASHIFTRT)
	    mask &= (mask >> shift_count) << shift_count;
	  else if (GET_CODE (op0) == ASHIFT)
	    mask = (mask & (mask << shift_count)) >> shift_count;

	  if ((nonzero_bits (XEXP (op0, 0), mode) & ~mask) == 0
	      && (nonzero_bits (XEXP (op1, 0), mode) & ~mask) == 0)
	    op0 = XEXP (op0, 0), op1 = XEXP (op1, 0);
	  else
	    break;
	}

      /* If both operands are AND's of a paradoxical SUBREG by constant, the
	 SUBREGs are of the same mode, and, in both cases, the AND would
	 be redundant if the comparison was done in the narrower mode,
	 do the comparison in the narrower mode (e.g., we are AND'ing with 1
	 and the operand's possibly nonzero bits are 0xffffff01; in that case
	 if we only care about QImode, we don't need the AND).  This case
	 occurs if the output mode of an scc insn is not SImode and
	 STORE_FLAG_VALUE == 1 (e.g., the 386).

	 Similarly, check for a case where the AND's are ZERO_EXTEND
	 operations from some narrower mode even though a SUBREG is not
	 present.  */

      else if (GET_CODE (op0) == AND && GET_CODE (op1) == AND
	       && CONST_INT_P (XEXP (op0, 1))
	       && CONST_INT_P (XEXP (op1, 1)))
	{
	  rtx inner_op0 = XEXP (op0, 0);
	  rtx inner_op1 = XEXP (op1, 0);
	  HOST_WIDE_INT c0 = INTVAL (XEXP (op0, 1));
	  HOST_WIDE_INT c1 = INTVAL (XEXP (op1, 1));
	  bool changed = false;

	  if (paradoxical_subreg_p (inner_op0)
	      && GET_CODE (inner_op1) == SUBREG
	      && HWI_COMPUTABLE_MODE_P (GET_MODE (SUBREG_REG (inner_op0)))
	      && (GET_MODE (SUBREG_REG (inner_op0))
		  == GET_MODE (SUBREG_REG (inner_op1)))
	      && ((~c0) & nonzero_bits (SUBREG_REG (inner_op0),
					GET_MODE (SUBREG_REG (inner_op0)))) == 0
	      && ((~c1) & nonzero_bits (SUBREG_REG (inner_op1),
					GET_MODE (SUBREG_REG (inner_op1)))) == 0)
	    {
	      op0 = SUBREG_REG (inner_op0);
	      op1 = SUBREG_REG (inner_op1);

	      /* The resulting comparison is always unsigned since we masked
		 off the original sign bit.  */
	      code = unsigned_condition (code);

	      changed = true;
	    }

	  else if (c0 == c1)
	    FOR_EACH_MODE_UNTIL (tmode,
				 as_a <scalar_int_mode> (GET_MODE (op0)))
	      if ((unsigned HOST_WIDE_INT) c0 == GET_MODE_MASK (tmode))
		{
		  op0 = gen_lowpart_or_truncate (tmode, inner_op0);
		  op1 = gen_lowpart_or_truncate (tmode, inner_op1);
		  code = unsigned_condition (code);
		  changed = true;
		  break;
		}

	  if (! changed)
	    break;
	}

      /* If both operands are NOT, we can strip off the outer operation
	 and adjust the comparison code for swapped operands; similarly for
	 NEG, except that this must be an equality comparison.  */
      else if ((GET_CODE (op0) == NOT && GET_CODE (op1) == NOT)
	       || (GET_CODE (op0) == NEG && GET_CODE (op1) == NEG
		   && (code == EQ || code == NE)))
	op0 = XEXP (op0, 0), op1 = XEXP (op1, 0), code = swap_condition (code);

      else
	break;
    }

  /* If the first operand is a constant, swap the operands and adjust the
     comparison code appropriately, but don't do this if the second operand
     is already a constant integer.  */
  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  /* We now enter a loop during which we will try to simplify the comparison.
     For the most part, we only are concerned with comparisons with zero,
     but some things may really be comparisons with zero but not start
     out looking that way.  */

  while (CONST_INT_P (op1))
    {
      machine_mode raw_mode = GET_MODE (op0);
      scalar_int_mode int_mode;
      int equality_comparison_p;
      int sign_bit_comparison_p;
      int unsigned_comparison_p;
      HOST_WIDE_INT const_op;

      /* We only want to handle integral modes.  This catches VOIDmode,
	 CCmode, and the floating-point modes.  An exception is that we
	 can handle VOIDmode if OP0 is a COMPARE or a comparison
	 operation.  */

      if (GET_MODE_CLASS (raw_mode) != MODE_INT
	  && ! (raw_mode == VOIDmode
		&& (GET_CODE (op0) == COMPARE || COMPARISON_P (op0))))
	break;

      /* Try to simplify the compare to constant, possibly changing the
	 comparison op, and/or changing op1 to zero.  */
      code = simplify_compare_const (code, raw_mode, &op0, &op1);
      const_op = INTVAL (op1);

      /* Compute some predicates to simplify code below.  */

      equality_comparison_p = (code == EQ || code == NE);
      sign_bit_comparison_p = ((code == LT || code == GE) && const_op == 0);
      unsigned_comparison_p = (code == LTU || code == LEU || code == GTU
			       || code == GEU);

      /* If this is a sign bit comparison and we can do arithmetic in
	 MODE, say that we will only be needing the sign bit of OP0.  */
      if (sign_bit_comparison_p
	  && is_a <scalar_int_mode> (raw_mode, &int_mode)
	  && HWI_COMPUTABLE_MODE_P (int_mode))
	op0 = force_to_mode (op0, int_mode,
			     HOST_WIDE_INT_1U
			     << (GET_MODE_PRECISION (int_mode) - 1), false);

      if (COMPARISON_P (op0))
	{
	  /* We can't do anything if OP0 is a condition code value, rather
	     than an actual data value.  */
	  if (const_op != 0
	      || GET_MODE_CLASS (GET_MODE (XEXP (op0, 0))) == MODE_CC)
	    break;

	  /* Get the two operands being compared.  */
	  if (GET_CODE (XEXP (op0, 0)) == COMPARE)
	    tem = XEXP (XEXP (op0, 0), 0), tem1 = XEXP (XEXP (op0, 0), 1);
	  else
	    tem = XEXP (op0, 0), tem1 = XEXP (op0, 1);

	  /* Check for the cases where we simply want the result of the
	     earlier test or the opposite of that result.  */
	  if (code == NE || code == EQ
	      || (val_signbit_known_set_p (raw_mode, STORE_FLAG_VALUE)
		  && (code == LT || code == GE)))
	    {
	      enum rtx_code new_code;
	      if (code == LT || code == NE)
		new_code = GET_CODE (op0);
	      else
		new_code = reversed_comparison_code (op0, NULL);

	      if (new_code != UNKNOWN)
		{
		  code = new_code;
		  op0 = tem;
		  op1 = tem1;
		  continue;
		}
	    }
	  break;
	}

      if (raw_mode == VOIDmode)
	break;
      scalar_int_mode mode = as_a <scalar_int_mode> (raw_mode);

      /* Now try cases based on the opcode of OP0.  If none of the cases
	 does a "continue", we exit this loop immediately after the
	 switch.  */

      unsigned int mode_width = GET_MODE_PRECISION (mode);
      unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);
      switch (GET_CODE (op0))
	{
	case ZERO_EXTRACT:
	  /* If we are extracting a single bit from a variable position in
	     a constant that has only a single bit set and are comparing it
	     with zero, we can convert this into an equality comparison
	     between the position and the location of the single bit.  */
	  /* Except we can't if SHIFT_COUNT_TRUNCATED is set, since we might
	     have already reduced the shift count modulo the word size.  */
	  if (!SHIFT_COUNT_TRUNCATED
	      && CONST_INT_P (XEXP (op0, 0))
	      && XEXP (op0, 1) == const1_rtx
	      && equality_comparison_p && const_op == 0
	      && (i = exact_log2 (UINTVAL (XEXP (op0, 0)))) >= 0)
	    {
	      if (BITS_BIG_ENDIAN)
		i = BITS_PER_WORD - 1 - i;

	      op0 = XEXP (op0, 2);
	      op1 = GEN_INT (i);
	      const_op = i;

	      /* Result is nonzero iff shift count is equal to I.  */
	      code = reverse_condition (code);
	      continue;
	    }

	  /* fall through */

	case SIGN_EXTRACT:
	  tem = expand_compound_operation (op0);
	  if (tem != op0)
	    {
	      op0 = tem;
	      continue;
	    }
	  break;

	case NOT:
	  /* If testing for equality, we can take the NOT of the constant.  */
	  if (equality_comparison_p
	      && (tem = simplify_unary_operation (NOT, mode, op1, mode)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* If just looking at the sign bit, reverse the sense of the
	     comparison.  */
	  if (sign_bit_comparison_p)
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == GE ? LT : GE);
	      continue;
	    }
	  break;

	case NEG:
	  /* If testing for equality, we can take the NEG of the constant.  */
	  if (equality_comparison_p
	      && (tem = simplify_unary_operation (NEG, mode, op1, mode)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* The remaining cases only apply to comparisons with zero.  */
	  if (const_op != 0)
	    break;

	  /* When X is ABS or is known positive,
	     (neg X) is < 0 if and only if X != 0.  */

	  if (sign_bit_comparison_p
	      && (GET_CODE (XEXP (op0, 0)) == ABS
		  || (mode_width <= HOST_BITS_PER_WIDE_INT
		      && (nonzero_bits (XEXP (op0, 0), mode)
			  & (HOST_WIDE_INT_1U << (mode_width - 1)))
			 == 0)))
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* If we have NEG of something whose two high-order bits are the
	     same, we know that "(-a) < 0" is equivalent to "a > 0".  */
	  if (num_sign_bit_copies (op0, mode) >= 2)
	    {
	      op0 = XEXP (op0, 0);
	      code = swap_condition (code);
	      continue;
	    }
	  break;

	case ROTATE:
	  /* If we are testing equality and our count is a constant, we
	     can perform the inverse operation on our RHS.  */
	  if (equality_comparison_p && CONST_INT_P (XEXP (op0, 1))
	      && (tem = simplify_binary_operation (ROTATERT, mode,
						   op1, XEXP (op0, 1))) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* If we are doing a < 0 or >= 0 comparison, it means we are testing
	     a particular bit.  Convert it to an AND of a constant of that
	     bit.  This will be converted into a ZERO_EXTRACT.  */
	  if (const_op == 0 && sign_bit_comparison_p
	      && CONST_INT_P (XEXP (op0, 1))
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && UINTVAL (XEXP (op0, 1)) < mode_width)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0),
					    (HOST_WIDE_INT_1U
					     << (mode_width - 1
						 - INTVAL (XEXP (op0, 1)))));
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* Fall through.  */

	case ABS:
	  /* ABS is ignorable inside an equality comparison with zero.  */
	  if (const_op == 0 && equality_comparison_p)
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;

	case SIGN_EXTEND:
	  /* Can simplify (compare (zero/sign_extend FOO) CONST) to
	     (compare FOO CONST) if CONST fits in FOO's mode and we
	     are either testing inequality or have an unsigned
	     comparison with ZERO_EXTEND or a signed comparison with
	     SIGN_EXTEND.  But don't do it if we don't have a compare
	     insn of the given mode, since we'd have to revert it
	     later on, and then we wouldn't know whether to sign- or
	     zero-extend.  */
	  if (is_int_mode (GET_MODE (XEXP (op0, 0)), &mode)
	      && ! unsigned_comparison_p
	      && HWI_COMPUTABLE_MODE_P (mode)
	      && trunc_int_for_mode (const_op, mode) == const_op
	      && have_insn_for (COMPARE, mode))
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;

	case SUBREG:
	  /* Check for the case where we are comparing A - C1 with C2, that is

	       (subreg:MODE (plus (A) (-C1))) op (C2)

	     with C1 a constant, and try to lift the SUBREG, i.e. to do the
	     comparison in the wider mode.  One of the following two conditions
	     must be true in order for this to be valid:

	       1. The mode extension results in the same bit pattern being added
		  on both sides and the comparison is equality or unsigned.  As
		  C2 has been truncated to fit in MODE, the pattern can only be
		  all 0s or all 1s.

	       2. The mode extension results in the sign bit being copied on
		  each side.

	     The difficulty here is that we have predicates for A but not for
	     (A - C1) so we need to check that C1 is within proper bounds so
	     as to perturbate A as little as possible.  */

	  if (mode_width <= HOST_BITS_PER_WIDE_INT
	      && subreg_lowpart_p (op0)
	      && is_a <scalar_int_mode> (GET_MODE (SUBREG_REG (op0)),
					 &inner_mode)
	      && GET_MODE_PRECISION (inner_mode) > mode_width
	      && GET_CODE (SUBREG_REG (op0)) == PLUS
	      && CONST_INT_P (XEXP (SUBREG_REG (op0), 1)))
	    {
	      rtx a = XEXP (SUBREG_REG (op0), 0);
	      HOST_WIDE_INT c1 = -INTVAL (XEXP (SUBREG_REG (op0), 1));

	      if ((c1 > 0
		   && (unsigned HOST_WIDE_INT) c1
		       < HOST_WIDE_INT_1U << (mode_width - 1)
		   && (equality_comparison_p || unsigned_comparison_p)
		   /* (A - C1) zero-extends if it is positive and sign-extends
		      if it is negative, C2 both zero- and sign-extends.  */
		   && (((nonzero_bits (a, inner_mode)
			 & ~GET_MODE_MASK (mode)) == 0
			&& const_op >= 0)
		       /* (A - C1) sign-extends if it is positive and 1-extends
			  if it is negative, C2 both sign- and 1-extends.  */
		       || (num_sign_bit_copies (a, inner_mode)
			   > (unsigned int) (GET_MODE_PRECISION (inner_mode)
					     - mode_width)
			   && const_op < 0)))
		  || ((unsigned HOST_WIDE_INT) c1
		       < HOST_WIDE_INT_1U << (mode_width - 2)
		      /* (A - C1) always sign-extends, like C2.  */
		      && num_sign_bit_copies (a, inner_mode)
			 > (unsigned int) (GET_MODE_PRECISION (inner_mode)
					   - (mode_width - 1))))
		{
		  op0 = SUBREG_REG (op0);
		  continue;
		}
	    }

	  /* If the inner mode is narrower and we are extracting the low part,
	     we can treat the SUBREG as if it were a ZERO_EXTEND.  */
	  if (paradoxical_subreg_p (op0))
	    ;
	  else if (subreg_lowpart_p (op0)
		   && GET_MODE_CLASS (mode) == MODE_INT
		   && is_int_mode (GET_MODE (SUBREG_REG (op0)), &inner_mode)
		   && (code == NE || code == EQ)
		   && GET_MODE_PRECISION (inner_mode) <= HOST_BITS_PER_WIDE_INT
		   && !paradoxical_subreg_p (op0)
		   && (nonzero_bits (SUBREG_REG (op0), inner_mode)
		       & ~GET_MODE_MASK (mode)) == 0)
	    {
	      /* Remove outer subregs that don't do anything.  */
	      tem = gen_lowpart (inner_mode, op1);

	      if ((nonzero_bits (tem, inner_mode)
		   & ~GET_MODE_MASK (mode)) == 0)
		{
		  op0 = SUBREG_REG (op0);
		  op1 = tem;
		  continue;
		}
	      break;
	    }
	  else
	    break;

	  /* FALLTHROUGH */

	case ZERO_EXTEND:
	  if (is_int_mode (GET_MODE (XEXP (op0, 0)), &mode)
	      && (unsigned_comparison_p || equality_comparison_p)
	      && HWI_COMPUTABLE_MODE_P (mode)
	      && (unsigned HOST_WIDE_INT) const_op <= GET_MODE_MASK (mode)
	      && const_op >= 0
	      && have_insn_for (COMPARE, mode))
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;

	case PLUS:
	  /* (eq (plus X A) B) -> (eq X (minus B A)).  We can only do
	     this for equality comparisons due to pathological cases involving
	     overflows.  */
	  if (equality_comparison_p
	      && (tem = simplify_binary_operation (MINUS, mode,
						   op1, XEXP (op0, 1))) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* (plus (abs X) (const_int -1)) is < 0 if and only if X == 0.  */
	  if (const_op == 0 && XEXP (op0, 1) == constm1_rtx
	      && GET_CODE (XEXP (op0, 0)) == ABS && sign_bit_comparison_p)
	    {
	      op0 = XEXP (XEXP (op0, 0), 0);
	      code = (code == LT ? EQ : NE);
	      continue;
	    }
	  break;

	case MINUS:
	  /* We used to optimize signed comparisons against zero, but that
	     was incorrect.  Unsigned comparisons against zero (GTU, LEU)
	     arrive here as equality comparisons, or (GEU, LTU) are
	     optimized away.  No need to special-case them.  */

	  /* (eq (minus A B) C) -> (eq A (plus B C)) or
	     (eq B (minus A C)), whichever simplifies.  We can only do
	     this for equality comparisons due to pathological cases involving
	     overflows.  */
	  if (equality_comparison_p
	      && (tem = simplify_binary_operation (PLUS, mode,
						   XEXP (op0, 1), op1)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  if (equality_comparison_p
	      && (tem = simplify_binary_operation (MINUS, mode,
						   XEXP (op0, 0), op1)) != 0)
	    {
	      op0 = XEXP (op0, 1);
	      op1 = tem;
	      continue;
	    }

	  /* The sign bit of (minus (ashiftrt X C) X), where C is the number
	     of bits in X minus 1, is one iff X > 0.  */
	  if (sign_bit_comparison_p && GET_CODE (XEXP (op0, 0)) == ASHIFTRT
	      && CONST_INT_P (XEXP (XEXP (op0, 0), 1))
	      && UINTVAL (XEXP (XEXP (op0, 0), 1)) == mode_width - 1
	      && rtx_equal_p (XEXP (XEXP (op0, 0), 0), XEXP (op0, 1)))
	    {
	      op0 = XEXP (op0, 1);
	      code = (code == GE ? LE : GT);
	      continue;
	    }
	  break;

	case XOR:
	  /* (eq (xor A B) C) -> (eq A (xor B C)).  This is a simplification
	     if C is zero or B is a constant.  */
	  if (equality_comparison_p
	      && (tem = simplify_binary_operation (XOR, mode,
						   XEXP (op0, 1), op1)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }
	  break;


	case IOR:
	  /* The sign bit of (ior (plus X (const_int -1)) X) is nonzero
	     iff X <= 0.  */
	  if (sign_bit_comparison_p && GET_CODE (XEXP (op0, 0)) == PLUS
	      && XEXP (XEXP (op0, 0), 1) == constm1_rtx
	      && rtx_equal_p (XEXP (XEXP (op0, 0), 0), XEXP (op0, 1)))
	    {
	      op0 = XEXP (op0, 1);
	      code = (code == GE ? GT : LE);
	      continue;
	    }
	  break;

	case AND:
	  /* Convert (and (xshift 1 X) Y) to (and (lshiftrt Y X) 1).  This
	     will be converted to a ZERO_EXTRACT later.  */
	  if (const_op == 0 && equality_comparison_p
	      && GET_CODE (XEXP (op0, 0)) == ASHIFT
	      && XEXP (XEXP (op0, 0), 0) == const1_rtx)
	    {
	      op0 = gen_rtx_LSHIFTRT (mode, XEXP (op0, 1),
				      XEXP (XEXP (op0, 0), 1));
	      op0 = simplify_and_const_int (NULL_RTX, mode, op0, 1);
	      continue;
	    }

	  /* If we are comparing (and (lshiftrt X C1) C2) for equality with
	     zero and X is a comparison and C1 and C2 describe only bits set
	     in STORE_FLAG_VALUE, we can compare with X.  */
	  if (const_op == 0 && equality_comparison_p
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && CONST_INT_P (XEXP (op0, 1))
	      && GET_CODE (XEXP (op0, 0)) == LSHIFTRT
	      && CONST_INT_P (XEXP (XEXP (op0, 0), 1))
	      && INTVAL (XEXP (XEXP (op0, 0), 1)) >= 0
	      && INTVAL (XEXP (XEXP (op0, 0), 1)) < HOST_BITS_PER_WIDE_INT)
	    {
	      mask = ((INTVAL (XEXP (op0, 1)) & GET_MODE_MASK (mode))
		      << INTVAL (XEXP (XEXP (op0, 0), 1)));
	      if ((~STORE_FLAG_VALUE & mask) == 0
		  && (COMPARISON_P (XEXP (XEXP (op0, 0), 0))
		      || ((tem = get_last_value (XEXP (XEXP (op0, 0), 0))) != 0
			  && COMPARISON_P (tem))))
		{
		  op0 = XEXP (XEXP (op0, 0), 0);
		  continue;
		}
	    }

	  /* If we are doing an equality comparison of an AND of a bit equal
	     to the sign bit, replace this with a LT or GE comparison of
	     the underlying value.  */
	  if (equality_comparison_p
	      && const_op == 0
	      && CONST_INT_P (XEXP (op0, 1))
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && ((INTVAL (XEXP (op0, 1)) & GET_MODE_MASK (mode))
		  == HOST_WIDE_INT_1U << (mode_width - 1)))
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == EQ ? GE : LT);
	      continue;
	    }

	  /* If this AND operation is really a ZERO_EXTEND from a narrower
	     mode, the constant fits within that mode, and this is either an
	     equality or unsigned comparison, try to do this comparison in
	     the narrower mode.

	     Note that in:

	     (ne:DI (and:DI (reg:DI 4) (const_int 0xffffffff)) (const_int 0))
	     -> (ne:DI (reg:SI 4) (const_int 0))

	     unless TARGET_TRULY_NOOP_TRUNCATION allows it or the register is
	     known to hold a value of the required mode the
	     transformation is invalid.  */
	  if ((equality_comparison_p || unsigned_comparison_p)
	      && CONST_INT_P (XEXP (op0, 1))
	      && (i = exact_log2 ((UINTVAL (XEXP (op0, 1))
				   & GET_MODE_MASK (mode))
				  + 1)) >= 0
	      && const_op >> i == 0
	      && int_mode_for_size (i, 1).exists (&tmode))
	    {
	      op0 = gen_lowpart_or_truncate (tmode, XEXP (op0, 0));
	      continue;
	    }

	  /* If this is (and:M1 (subreg:M1 X:M2 0) (const_int C1)) where C1
	     fits in both M1 and M2 and the SUBREG is either paradoxical
	     or represents the low part, permute the SUBREG and the AND
	     and try again.  */
	  if (GET_CODE (XEXP (op0, 0)) == SUBREG
	      && CONST_INT_P (XEXP (op0, 1)))
	    {
	      unsigned HOST_WIDE_INT c1 = INTVAL (XEXP (op0, 1));
	      /* Require an integral mode, to avoid creating something like
		 (AND:SF ...).  */
	      if ((is_a <scalar_int_mode>
		   (GET_MODE (SUBREG_REG (XEXP (op0, 0))), &tmode))
		  /* It is unsafe to commute the AND into the SUBREG if the
		     SUBREG is paradoxical and WORD_REGISTER_OPERATIONS is
		     not defined.  As originally written the upper bits
		     have a defined value due to the AND operation.
		     However, if we commute the AND inside the SUBREG then
		     they no longer have defined values and the meaning of
		     the code has been changed.
		     Also C1 should not change value in the smaller mode,
		     see PR67028 (a positive C1 can become negative in the
		     smaller mode, so that the AND does no longer mask the
		     upper bits).  */
		  && ((WORD_REGISTER_OPERATIONS
		       && mode_width > GET_MODE_PRECISION (tmode)
		       && mode_width <= BITS_PER_WORD
		       && trunc_int_for_mode (c1, tmode) == (HOST_WIDE_INT) c1)
		      || (mode_width <= GET_MODE_PRECISION (tmode)
			  && subreg_lowpart_p (XEXP (op0, 0))))
		  && mode_width <= HOST_BITS_PER_WIDE_INT
		  && HWI_COMPUTABLE_MODE_P (tmode)
		  && (c1 & ~mask) == 0
		  && (c1 & ~GET_MODE_MASK (tmode)) == 0
		  && c1 != mask
		  && c1 != GET_MODE_MASK (tmode))
		{
		  op0 = simplify_gen_binary (AND, tmode,
					     SUBREG_REG (XEXP (op0, 0)),
					     gen_int_mode (c1, tmode));
		  op0 = gen_lowpart (mode, op0);
		  continue;
		}
	    }

	  /* Convert (ne (and (not X) 1) 0) to (eq (and X 1) 0).  */
	  if (const_op == 0 && equality_comparison_p
	      && XEXP (op0, 1) == const1_rtx
	      && GET_CODE (XEXP (op0, 0)) == NOT)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode,
					    XEXP (XEXP (op0, 0), 0), 1);
	      code = (code == NE ? EQ : NE);
	      continue;
	    }

	  /* Convert (ne (and (lshiftrt (not X)) 1) 0) to
	     (eq (and (lshiftrt X) 1) 0).
	     Also handle the case where (not X) is expressed using xor.  */
	  if (const_op == 0 && equality_comparison_p
	      && XEXP (op0, 1) == const1_rtx
	      && GET_CODE (XEXP (op0, 0)) == LSHIFTRT)
	    {
	      rtx shift_op = XEXP (XEXP (op0, 0), 0);
	      rtx shift_count = XEXP (XEXP (op0, 0), 1);

	      if (GET_CODE (shift_op) == NOT
		  || (GET_CODE (shift_op) == XOR
		      && CONST_INT_P (XEXP (shift_op, 1))
		      && CONST_INT_P (shift_count)
		      && HWI_COMPUTABLE_MODE_P (mode)
		      && (UINTVAL (XEXP (shift_op, 1))
			  == HOST_WIDE_INT_1U
			       << INTVAL (shift_count))))
		{
		  op0
		    = gen_rtx_LSHIFTRT (mode, XEXP (shift_op, 0), shift_count);
		  op0 = simplify_and_const_int (NULL_RTX, mode, op0, 1);
		  code = (code == NE ? EQ : NE);
		  continue;
		}
	    }
	  break;

	case ASHIFT:
	  /* If we have (compare (ashift FOO N) (const_int C)) and
	     the high order N bits of FOO (N+1 if an inequality comparison)
	     are known to be zero, we can do this by comparing FOO with C
	     shifted right N bits so long as the low-order N bits of C are
	     zero.  */
	  if (CONST_INT_P (XEXP (op0, 1))
	      && INTVAL (XEXP (op0, 1)) >= 0
	      && ((INTVAL (XEXP (op0, 1)) + ! equality_comparison_p)
		  < HOST_BITS_PER_WIDE_INT)
	      && (((unsigned HOST_WIDE_INT) const_op
		   & ((HOST_WIDE_INT_1U << INTVAL (XEXP (op0, 1)))
		      - 1)) == 0)
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && (nonzero_bits (XEXP (op0, 0), mode)
		  & ~(mask >> (INTVAL (XEXP (op0, 1))
			       + ! equality_comparison_p))) == 0)
	    {
	      /* We must perform a logical shift, not an arithmetic one,
		 as we want the top N bits of C to be zero.  */
	      unsigned HOST_WIDE_INT temp = const_op & GET_MODE_MASK (mode);

	      temp >>= INTVAL (XEXP (op0, 1));
	      op1 = gen_int_mode (temp, mode);
	      op0 = XEXP (op0, 0);
	      continue;
	    }

	  /* If we are doing a sign bit comparison, it means we are testing
	     a particular bit.  Convert it to the appropriate AND.  */
	  if (sign_bit_comparison_p && CONST_INT_P (XEXP (op0, 1))
	      && mode_width <= HOST_BITS_PER_WIDE_INT)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0),
					    (HOST_WIDE_INT_1U
					     << (mode_width - 1
						 - INTVAL (XEXP (op0, 1)))));
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* If this an equality comparison with zero and we are shifting
	     the low bit to the sign bit, we can convert this to an AND of the
	     low-order bit.  */
	  if (const_op == 0 && equality_comparison_p
	      && CONST_INT_P (XEXP (op0, 1))
	      && UINTVAL (XEXP (op0, 1)) == mode_width - 1)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0), 1);
	      continue;
	    }
	  break;

	case ASHIFTRT:
	  /* If this is an equality comparison with zero, we can do this
	     as a logical shift, which might be much simpler.  */
	  if (equality_comparison_p && const_op == 0
	      && CONST_INT_P (XEXP (op0, 1)))
	    {
	      op0 = simplify_shift_const (NULL_RTX, LSHIFTRT, mode,
					  XEXP (op0, 0),
					  INTVAL (XEXP (op0, 1)));
	      continue;
	    }

	  /* If OP0 is a sign extension and CODE is not an unsigned comparison,
	     do the comparison in a narrower mode.  */
	  if (! unsigned_comparison_p
	      && CONST_INT_P (XEXP (op0, 1))
	      && GET_CODE (XEXP (op0, 0)) == ASHIFT
	      && XEXP (op0, 1) == XEXP (XEXP (op0, 0), 1)
	      && (int_mode_for_size (mode_width - INTVAL (XEXP (op0, 1)), 1)
		  .exists (&tmode))
	      && (((unsigned HOST_WIDE_INT) const_op
		   + (GET_MODE_MASK (tmode) >> 1) + 1)
		  <= GET_MODE_MASK (tmode)))
	    {
	      op0 = gen_lowpart (tmode, XEXP (XEXP (op0, 0), 0));
	      continue;
	    }

	  /* Likewise if OP0 is a PLUS of a sign extension with a
	     constant, which is usually represented with the PLUS
	     between the shifts.  */
	  if (! unsigned_comparison_p
	      && CONST_INT_P (XEXP (op0, 1))
	      && GET_CODE (XEXP (op0, 0)) == PLUS
	      && CONST_INT_P (XEXP (XEXP (op0, 0), 1))
	      && GET_CODE (XEXP (XEXP (op0, 0), 0)) == ASHIFT
	      && XEXP (op0, 1) == XEXP (XEXP (XEXP (op0, 0), 0), 1)
	      && (int_mode_for_size (mode_width - INTVAL (XEXP (op0, 1)), 1)
		  .exists (&tmode))
	      && (((unsigned HOST_WIDE_INT) const_op
		   + (GET_MODE_MASK (tmode) >> 1) + 1)
		  <= GET_MODE_MASK (tmode)))
	    {
	      rtx inner = XEXP (XEXP (XEXP (op0, 0), 0), 0);
	      rtx add_const = XEXP (XEXP (op0, 0), 1);
	      rtx new_const = simplify_gen_binary (ASHIFTRT, mode,
						   add_const, XEXP (op0, 1));

	      op0 = simplify_gen_binary (PLUS, tmode,
					 gen_lowpart (tmode, inner),
					 new_const);
	      continue;
	    }

	  /* FALLTHROUGH */
	case LSHIFTRT:
	  /* If we have (compare (xshiftrt FOO N) (const_int C)) and
	     the low order N bits of FOO are known to be zero, we can do this
	     by comparing FOO with C shifted left N bits so long as no
	     overflow occurs.  Even if the low order N bits of FOO aren't known
	     to be zero, if the comparison is >= or < we can use the same
	     optimization and for > or <= by setting all the low
	     order N bits in the comparison constant.  */
	  if (CONST_INT_P (XEXP (op0, 1))
	      && INTVAL (XEXP (op0, 1)) > 0
	      && INTVAL (XEXP (op0, 1)) < HOST_BITS_PER_WIDE_INT
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && (((unsigned HOST_WIDE_INT) const_op
		   + (GET_CODE (op0) != LSHIFTRT
		      ? ((GET_MODE_MASK (mode) >> INTVAL (XEXP (op0, 1)) >> 1)
			 + 1)
		      : 0))
		  <= GET_MODE_MASK (mode) >> INTVAL (XEXP (op0, 1))))
	    {
	      unsigned HOST_WIDE_INT low_bits
		= (nonzero_bits (XEXP (op0, 0), mode)
		   & ((HOST_WIDE_INT_1U
		       << INTVAL (XEXP (op0, 1))) - 1));
	      if (low_bits == 0 || !equality_comparison_p)
		{
		  /* If the shift was logical, then we must make the condition
		     unsigned.  */
		  if (GET_CODE (op0) == LSHIFTRT)
		    code = unsigned_condition (code);

		  const_op = (unsigned HOST_WIDE_INT) const_op
			      << INTVAL (XEXP (op0, 1));
		  if (low_bits != 0
		      && (code == GT || code == GTU
			  || code == LE || code == LEU))
		    const_op
		      |= ((HOST_WIDE_INT_1 << INTVAL (XEXP (op0, 1))) - 1);
		  op1 = GEN_INT (const_op);
		  op0 = XEXP (op0, 0);
		  continue;
		}
	    }

	  /* If we are using this shift to extract just the sign bit, we
	     can replace this with an LT or GE comparison.  */
	  if (const_op == 0
	      && (equality_comparison_p || sign_bit_comparison_p)
	      && CONST_INT_P (XEXP (op0, 1))
	      && UINTVAL (XEXP (op0, 1)) == mode_width - 1)
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == NE || code == GT ? LT : GE);
	      continue;
	    }
	  break;

	default:
	  break;
	}

      break;
    }

  /* Now make any compound operations involved in this comparison.  Then,
     check for an outmost SUBREG on OP0 that is not doing anything or is
     paradoxical.  The latter transformation must only be performed when
     it is known that the "extra" bits will be the same in op0 and op1 or
     that they don't matter.  There are three cases to consider:

     1. SUBREG_REG (op0) is a register.  In this case the bits are don't
     care bits and we can assume they have any convenient value.  So
     making the transformation is safe.

     2. SUBREG_REG (op0) is a memory and LOAD_EXTEND_OP is UNKNOWN.
     In this case the upper bits of op0 are undefined.  We should not make
     the simplification in that case as we do not know the contents of
     those bits.

     3. SUBREG_REG (op0) is a memory and LOAD_EXTEND_OP is not UNKNOWN.
     In that case we know those bits are zeros or ones.  We must also be
     sure that they are the same as the upper bits of op1.

     We can never remove a SUBREG for a non-equality comparison because
     the sign bit is in a different place in the underlying object.  */

  rtx_code op0_mco_code = SET;
  if (op1 == const0_rtx)
    op0_mco_code = code == NE || code == EQ ? EQ : COMPARE;

  op0 = make_compound_operation (op0, op0_mco_code);
  op1 = make_compound_operation (op1, SET);

  if (GET_CODE (op0) == SUBREG && subreg_lowpart_p (op0)
      && is_int_mode (GET_MODE (op0), &mode)
      && is_int_mode (GET_MODE (SUBREG_REG (op0)), &inner_mode)
      && (code == NE || code == EQ))
    {
      if (paradoxical_subreg_p (op0))
	{
	  /* For paradoxical subregs, allow case 1 as above.  Case 3 isn't
	     implemented.  */
	  if (REG_P (SUBREG_REG (op0)))
	    {
	      op0 = SUBREG_REG (op0);
	      op1 = gen_lowpart (inner_mode, op1);
	    }
	}
      else if (GET_MODE_PRECISION (inner_mode) <= HOST_BITS_PER_WIDE_INT
	       && (nonzero_bits (SUBREG_REG (op0), inner_mode)
		   & ~GET_MODE_MASK (mode)) == 0)
	{
	  tem = gen_lowpart (inner_mode, op1);

	  if ((nonzero_bits (tem, inner_mode) & ~GET_MODE_MASK (mode)) == 0)
	    op0 = SUBREG_REG (op0), op1 = tem;
	}
    }

  /* We now do the opposite procedure: Some machines don't have compare
     insns in all modes.  If OP0's mode is an integer mode smaller than a
     word and we can't do a compare in that mode, see if there is a larger
     mode for which we can do the compare.  There are a number of cases in
     which we can use the wider mode.  */

  if (is_int_mode (GET_MODE (op0), &mode)
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD
      && ! have_insn_for (COMPARE, mode))
    FOR_EACH_WIDER_MODE (tmode_iter, mode)
      {
	tmode = tmode_iter.require ();
	if (!HWI_COMPUTABLE_MODE_P (tmode))
	  break;
	if (have_insn_for (COMPARE, tmode))
	  {
	    int zero_extended;

	    /* If this is a test for negative, we can make an explicit
	       test of the sign bit.  Test this first so we can use
	       a paradoxical subreg to extend OP0.  */

	    if (op1 == const0_rtx && (code == LT || code == GE)
		&& HWI_COMPUTABLE_MODE_P (mode))
	      {
		unsigned HOST_WIDE_INT sign
		  = HOST_WIDE_INT_1U << (GET_MODE_BITSIZE (mode) - 1);
		op0 = simplify_gen_binary (AND, tmode,
					   gen_lowpart (tmode, op0),
					   gen_int_mode (sign, tmode));
		code = (code == LT) ? NE : EQ;
		break;
	      }

	    /* If the only nonzero bits in OP0 and OP1 are those in the
	       narrower mode and this is an equality or unsigned comparison,
	       we can use the wider mode.  Similarly for sign-extended
	       values, in which case it is true for all comparisons.  */
	    zero_extended = ((code == EQ || code == NE
			      || code == GEU || code == GTU
			      || code == LEU || code == LTU)
			     && (nonzero_bits (op0, tmode)
				 & ~GET_MODE_MASK (mode)) == 0
			     && ((CONST_INT_P (op1)
				  || (nonzero_bits (op1, tmode)
				      & ~GET_MODE_MASK (mode)) == 0)));

	    if (zero_extended
		|| ((num_sign_bit_copies (op0, tmode)
		     > (unsigned int) (GET_MODE_PRECISION (tmode)
				       - GET_MODE_PRECISION (mode)))
		    && (num_sign_bit_copies (op1, tmode)
			> (unsigned int) (GET_MODE_PRECISION (tmode)
					  - GET_MODE_PRECISION (mode)))))
	      {
		/* If OP0 is an AND and we don't have an AND in MODE either,
		   make a new AND in the proper mode.  */
		if (GET_CODE (op0) == AND
		    && !have_insn_for (AND, mode))
		  op0 = simplify_gen_binary (AND, tmode,
					     gen_lowpart (tmode,
							  XEXP (op0, 0)),
					     gen_lowpart (tmode,
							  XEXP (op0, 1)));
		else
		  {
		    if (zero_extended)
		      {
			op0 = simplify_gen_unary (ZERO_EXTEND, tmode,
						  op0, mode);
			op1 = simplify_gen_unary (ZERO_EXTEND, tmode,
						  op1, mode);
		      }
		    else
		      {
			op0 = simplify_gen_unary (SIGN_EXTEND, tmode,
						  op0, mode);
			op1 = simplify_gen_unary (SIGN_EXTEND, tmode,
						  op1, mode);
		      }
		    break;
		  }
	      }
	  }
      }

  /* We may have changed the comparison operands.  Re-canonicalize.  */
  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  /* If this machine only supports a subset of valid comparisons, see if we
     can convert an unsupported one into a supported one.  */
  target_canonicalize_comparison (&code, &op0, &op1, 0);

  *pop0 = op0;
  *pop1 = op1;

  return code;
}

/* Utility function for record_value_for_reg.  Count number of
   rtxs in X.  */
static int
count_rtxs (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt;
  int i, j, ret = 1;

  if (GET_RTX_CLASS (code) == RTX_BIN_ARITH
      || GET_RTX_CLASS (code) == RTX_COMM_ARITH)
    {
      rtx x0 = XEXP (x, 0);
      rtx x1 = XEXP (x, 1);

      if (x0 == x1)
	return 1 + 2 * count_rtxs (x0);

      if ((GET_RTX_CLASS (GET_CODE (x1)) == RTX_BIN_ARITH
	   || GET_RTX_CLASS (GET_CODE (x1)) == RTX_COMM_ARITH)
	  && (x0 == XEXP (x1, 0) || x0 == XEXP (x1, 1)))
	return 2 + 2 * count_rtxs (x0)
	       + count_rtxs (x == XEXP (x1, 0)
			     ? XEXP (x1, 1) : XEXP (x1, 0));

      if ((GET_RTX_CLASS (GET_CODE (x0)) == RTX_BIN_ARITH
	   || GET_RTX_CLASS (GET_CODE (x0)) == RTX_COMM_ARITH)
	  && (x1 == XEXP (x0, 0) || x1 == XEXP (x0, 1)))
	return 2 + 2 * count_rtxs (x1)
	       + count_rtxs (x == XEXP (x0, 0)
			     ? XEXP (x0, 1) : XEXP (x0, 0));
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      ret += count_rtxs (XEXP (x, i));
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	ret += count_rtxs (XVECEXP (x, i, j));

  return ret;
}

/* Utility function for following routine.  Called when X is part of a value
   being stored into last_set_value.  Sets last_set_table_tick
   for each register mentioned.  Similar to mention_regs in cse.cc  */

static void
update_table_tick (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  const char *fmt = GET_RTX_FORMAT (code);
  int i, j;

  if (code == REG)
    {
      unsigned int regno = REGNO (x);
      unsigned int endregno = END_REGNO (x);
      unsigned int r;

      for (r = regno; r < endregno; r++)
	{
	  reg_stat_type *rsp = &reg_stat[r];
	  rsp->last_set_table_tick = label_tick;
	}

      return;
    }

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	/* Check for identical subexpressions.  If x contains
	   identical subexpression we only have to traverse one of
	   them.  */
	if (i == 0 && ARITHMETIC_P (x))
	  {
	    /* Note that at this point x1 has already been
	       processed.  */
	    rtx x0 = XEXP (x, 0);
	    rtx x1 = XEXP (x, 1);

	    /* If x0 and x1 are identical then there is no need to
	       process x0.  */
	    if (x0 == x1)
	      break;

	    /* If x0 is identical to a subexpression of x1 then while
	       processing x1, x0 has already been processed.  Thus we
	       are done with x.  */
	    if (ARITHMETIC_P (x1)
		&& (x0 == XEXP (x1, 0) || x0 == XEXP (x1, 1)))
	      break;

	    /* If x1 is identical to a subexpression of x0 then we
	       still have to process the rest of x0.  */
	    if (ARITHMETIC_P (x0)
		&& (x1 == XEXP (x0, 0) || x1 == XEXP (x0, 1)))
	      {
		update_table_tick (XEXP (x0, x1 == XEXP (x0, 0) ? 1 : 0));
		break;
	      }
	  }

	update_table_tick (XEXP (x, i));
      }
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	update_table_tick (XVECEXP (x, i, j));
}

/* Record that REG is set to VALUE in insn INSN.  If VALUE is zero, we
   are saying that the register is clobbered and we no longer know its
   value.  If INSN is zero, don't update reg_stat[].last_set; this is
   only permitted with VALUE also zero and is used to invalidate the
   register.  */

static void
record_value_for_reg (rtx reg, rtx_insn *insn, rtx value)
{
  unsigned int regno = REGNO (reg);
  unsigned int endregno = END_REGNO (reg);
  unsigned int i;
  reg_stat_type *rsp;

  /* If VALUE contains REG and we have a previous value for REG, substitute
     the previous value.  */
  if (value && insn && reg_overlap_mentioned_p (reg, value))
    {
      rtx tem;

      /* Set things up so get_last_value is allowed to see anything set up to
	 our insn.  */
      subst_low_luid = DF_INSN_LUID (insn);
      tem = get_last_value (reg);

      /* If TEM is simply a binary operation with two CLOBBERs as operands,
	 it isn't going to be useful and will take a lot of time to process,
	 so just use the CLOBBER.  */

      if (tem)
	{
	  if (ARITHMETIC_P (tem)
	      && GET_CODE (XEXP (tem, 0)) == CLOBBER
	      && GET_CODE (XEXP (tem, 1)) == CLOBBER)
	    tem = XEXP (tem, 0);
	  else if (count_occurrences (value, reg, 1) >= 2)
	    {
	      /* If there are two or more occurrences of REG in VALUE,
		 prevent the value from growing too much.  */
	      if (count_rtxs (tem) > param_max_last_value_rtl)
		tem = gen_rtx_CLOBBER (GET_MODE (tem), const0_rtx);
	    }

	  value = replace_rtx (copy_rtx (value), reg, tem);
	}
    }

  /* For each register modified, show we don't know its value, that
     we don't know about its bitwise content, that its value has been
     updated, and that we don't know the location of the death of the
     register.  */
  for (i = regno; i < endregno; i++)
    {
      rsp = &reg_stat[i];

      if (insn)
	rsp->last_set = insn;

      rsp->last_set_value = 0;
      rsp->last_set_mode = VOIDmode;
      rsp->last_set_nonzero_bits = 0;
      rsp->last_set_sign_bit_copies = 0;
      rsp->last_death = 0;
      rsp->truncated_to_mode = VOIDmode;
    }

  /* Mark registers that are being referenced in this value.  */
  if (value)
    update_table_tick (value);

  /* Now update the status of each register being set.
     If someone is using this register in this block, set this register
     to invalid since we will get confused between the two lives in this
     basic block.  This makes using this register always invalid.  In cse, we
     scan the table to invalidate all entries using this register, but this
     is too much work for us.  */

  for (i = regno; i < endregno; i++)
    {
      rsp = &reg_stat[i];
      rsp->last_set_label = label_tick;
      if (!insn
	  || (value && rsp->last_set_table_tick >= label_tick_ebb_start))
	rsp->last_set_invalid = true;
      else
	rsp->last_set_invalid = false;
    }

  /* The value being assigned might refer to X (like in "x++;").  In that
     case, we must replace it with (clobber (const_int 0)) to prevent
     infinite loops.  */
  rsp = &reg_stat[regno];
  if (value && !get_last_value_validate (&value, insn, label_tick, false))
    {
      value = copy_rtx (value);
      if (!get_last_value_validate (&value, insn, label_tick, true))
	value = 0;
    }

  /* For the main register being modified, update the value, the mode, the
     nonzero bits, and the number of sign bit copies.  */

  rsp->last_set_value = value;

  if (value)
    {
      machine_mode mode = GET_MODE (reg);
      subst_low_luid = DF_INSN_LUID (insn);
      rsp->last_set_mode = mode;
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && HWI_COMPUTABLE_MODE_P (mode))
	mode = nonzero_bits_mode;
      rsp->last_set_nonzero_bits = nonzero_bits (value, mode);
      rsp->last_set_sign_bit_copies
	= num_sign_bit_copies (value, GET_MODE (reg));
    }
}

/* Called via note_stores from record_dead_and_set_regs to handle one
   SET or CLOBBER in an insn.  DATA is the instruction in which the
   set is occurring.  */

static void
record_dead_and_set_regs_1 (rtx dest, const_rtx setter, void *data)
{
  rtx_insn *record_dead_insn = (rtx_insn *) data;

  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (!record_dead_insn)
    {
      if (REG_P (dest))
	record_value_for_reg (dest, NULL, NULL_RTX);
      return;
    }

  if (REG_P (dest))
    {
      /* If we are setting the whole register, we know its value.  Otherwise
	 show that we don't know the value.  We can handle a SUBREG if it's
	 the low part, but we must be careful with paradoxical SUBREGs on
	 RISC architectures because we cannot strip e.g. an extension around
	 a load and record the naked load since the RTL middle-end considers
	 that the upper bits are defined according to LOAD_EXTEND_OP.  */
      if (GET_CODE (setter) == SET && dest == SET_DEST (setter))
	record_value_for_reg (dest, record_dead_insn, SET_SRC (setter));
      else if (GET_CODE (setter) == SET
	       && GET_CODE (SET_DEST (setter)) == SUBREG
	       && SUBREG_REG (SET_DEST (setter)) == dest
	       && known_le (GET_MODE_PRECISION (GET_MODE (dest)),
			    BITS_PER_WORD)
	       && subreg_lowpart_p (SET_DEST (setter)))
	record_value_for_reg (dest, record_dead_insn,
			      WORD_REGISTER_OPERATIONS
			      && word_register_operation_p (SET_SRC (setter))
			      && paradoxical_subreg_p (SET_DEST (setter))
			      ? SET_SRC (setter)
			      : gen_lowpart (GET_MODE (dest),
					     SET_SRC (setter)));
      else
	record_value_for_reg (dest, record_dead_insn, NULL_RTX);
    }
  else if (MEM_P (dest)
	   /* Ignore pushes, they clobber nothing.  */
	   && ! push_operand (dest, GET_MODE (dest)))
    mem_last_set = DF_INSN_LUID (record_dead_insn);
}

/* Update the records of when each REG was most recently set or killed
   for the things done by INSN.  This is the last thing done in processing
   INSN in the combiner loop.

   We update reg_stat[], in particular fields last_set, last_set_value,
   last_set_mode, last_set_nonzero_bits, last_set_sign_bit_copies,
   last_death, and also the similar information mem_last_set (which insn
   most recently modified memory) and last_call_luid (which insn was the
   most recent subroutine call).  */

static void
record_dead_and_set_regs (rtx_insn *insn)
{
  rtx link;
  unsigned int i;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    {
      if (REG_NOTE_KIND (link) == REG_DEAD
	  && REG_P (XEXP (link, 0)))
	{
	  unsigned int regno = REGNO (XEXP (link, 0));
	  unsigned int endregno = END_REGNO (XEXP (link, 0));

	  for (i = regno; i < endregno; i++)
	    {
	      reg_stat_type *rsp;

	      rsp = &reg_stat[i];
	      rsp->last_death = insn;
	    }
	}
      else if (REG_NOTE_KIND (link) == REG_INC)
	record_value_for_reg (XEXP (link, 0), insn, NULL_RTX);
    }

  if (CALL_P (insn))
    {
      HARD_REG_SET callee_clobbers
	= insn_callee_abi (insn).full_and_partial_reg_clobbers ();
      hard_reg_set_iterator hrsi;
      EXECUTE_IF_SET_IN_HARD_REG_SET (callee_clobbers, 0, i, hrsi)
	{
	  reg_stat_type *rsp;

	  /* ??? We could try to preserve some information from the last
	     set of register I if the call doesn't actually clobber
	     (reg:last_set_mode I), which might be true for ABIs with
	     partial clobbers.  However, it would be difficult to
	     update last_set_nonzero_bits and last_sign_bit_copies
	     to account for the part of I that actually was clobbered.
	     It wouldn't help much anyway, since we rarely see this
	     situation before RA.  */
	  rsp = &reg_stat[i];
	  rsp->last_set_invalid = true;
	  rsp->last_set = insn;
	  rsp->last_set_value = 0;
	  rsp->last_set_mode = VOIDmode;
	  rsp->last_set_nonzero_bits = 0;
	  rsp->last_set_sign_bit_copies = 0;
	  rsp->last_death = 0;
	  rsp->truncated_to_mode = VOIDmode;
	}

      last_call_luid = mem_last_set = DF_INSN_LUID (insn);

      /* We can't combine into a call pattern.  Remember, though, that
	 the return value register is set at this LUID.  We could
	 still replace a register with the return value from the
	 wrong subroutine call!  */
      note_stores (insn, record_dead_and_set_regs_1, NULL_RTX);
    }
  else
    note_stores (insn, record_dead_and_set_regs_1, insn);
}

/* If a SUBREG has the promoted bit set, it is in fact a property of the
   register present in the SUBREG, so for each such SUBREG go back and
   adjust nonzero and sign bit information of the registers that are
   known to have some zero/sign bits set.

   This is needed because when combine blows the SUBREGs away, the
   information on zero/sign bits is lost and further combines can be
   missed because of that.  */

static void
record_promoted_value (rtx_insn *insn, rtx subreg)
{
  struct insn_link *links;
  rtx set;
  unsigned int regno = REGNO (SUBREG_REG (subreg));
  machine_mode mode = GET_MODE (subreg);

  if (!HWI_COMPUTABLE_MODE_P (mode))
    return;

  for (links = LOG_LINKS (insn); links;)
    {
      reg_stat_type *rsp;

      insn = links->insn;
      set = single_set (insn);

      if (! set || !REG_P (SET_DEST (set))
	  || REGNO (SET_DEST (set)) != regno
	  || GET_MODE (SET_DEST (set)) != GET_MODE (SUBREG_REG (subreg)))
	{
	  links = links->next;
	  continue;
	}

      rsp = &reg_stat[regno];
      if (rsp->last_set == insn)
	{
	  if (SUBREG_PROMOTED_UNSIGNED_P (subreg))
	    rsp->last_set_nonzero_bits &= GET_MODE_MASK (mode);
	}

      if (REG_P (SET_SRC (set)))
	{
	  regno = REGNO (SET_SRC (set));
	  links = LOG_LINKS (insn);
	}
      else
	break;
    }
}

/* Check if X, a register, is known to contain a value already
   truncated to MODE.  In this case we can use a subreg to refer to
   the truncated value even though in the generic case we would need
   an explicit truncation.  */

static bool
reg_truncated_to_mode (machine_mode mode, const_rtx x)
{
  reg_stat_type *rsp = &reg_stat[REGNO (x)];
  machine_mode truncated = rsp->truncated_to_mode;

  if (truncated == 0
      || rsp->truncation_label < label_tick_ebb_start)
    return false;
  if (!partial_subreg_p (mode, truncated))
    return true;
  if (TRULY_NOOP_TRUNCATION_MODES_P (mode, truncated))
    return true;
  return false;
}

/* If X is a hard reg or a subreg record the mode that the register is
   accessed in.  For non-TARGET_TRULY_NOOP_TRUNCATION targets we might be
   able to turn a truncate into a subreg using this information.  Return true
   if traversing X is complete.  */

static bool
record_truncated_value (rtx x)
{
  machine_mode truncated_mode;
  reg_stat_type *rsp;

  if (GET_CODE (x) == SUBREG && REG_P (SUBREG_REG (x)))
    {
      machine_mode original_mode = GET_MODE (SUBREG_REG (x));
      truncated_mode = GET_MODE (x);

      if (!partial_subreg_p (truncated_mode, original_mode))
	return true;

      truncated_mode = GET_MODE (x);
      if (TRULY_NOOP_TRUNCATION_MODES_P (truncated_mode, original_mode))
	return true;

      x = SUBREG_REG (x);
    }
  /* ??? For hard-regs we now record everything.  We might be able to
     optimize this using last_set_mode.  */
  else if (REG_P (x) && REGNO (x) < FIRST_PSEUDO_REGISTER)
    truncated_mode = GET_MODE (x);
  else
    return false;

  rsp = &reg_stat[REGNO (x)];
  if (rsp->truncated_to_mode == 0
      || rsp->truncation_label < label_tick_ebb_start
      || partial_subreg_p (truncated_mode, rsp->truncated_to_mode))
    {
      rsp->truncated_to_mode = truncated_mode;
      rsp->truncation_label = label_tick;
    }

  return true;
}

/* Callback for note_uses.  Find hardregs and subregs of pseudos and
   the modes they are used in.  This can help truning TRUNCATEs into
   SUBREGs.  */

static void
record_truncated_values (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, *loc, NONCONST)
    if (record_truncated_value (*iter))
      iter.skip_subrtxes ();
}

/* Scan X for promoted SUBREGs.  For each one found,
   note what it implies to the registers used in it.  */

static void
check_promoted_subreg (rtx_insn *insn, rtx x)
{
  if (GET_CODE (x) == SUBREG
      && SUBREG_PROMOTED_VAR_P (x)
      && REG_P (SUBREG_REG (x)))
    record_promoted_value (insn, x);
  else
    {
      const char *format = GET_RTX_FORMAT (GET_CODE (x));
      int i, j;

      for (i = 0; i < GET_RTX_LENGTH (GET_CODE (x)); i++)
	switch (format[i])
	  {
	  case 'e':
	    check_promoted_subreg (insn, XEXP (x, i));
	    break;
	  case 'V':
	  case 'E':
	    if (XVEC (x, i) != 0)
	      for (j = 0; j < XVECLEN (x, i); j++)
		check_promoted_subreg (insn, XVECEXP (x, i, j));
	    break;
	  }
    }
}

/* Verify that all the registers and memory references mentioned in *LOC are
   still valid.  *LOC was part of a value set in INSN when label_tick was
   equal to TICK.  Return false if some are not.  If REPLACE is true, replace
   the invalid references with (clobber (const_int 0)) and return true.  This
   replacement is useful because we often can get useful information about
   the form of a value (e.g., if it was produced by a shift that always
   produces -1 or 0) even though we don't know exactly what registers it
   was produced from.  */

static bool
get_last_value_validate (rtx *loc, rtx_insn *insn, int tick, bool replace)
{
  rtx x = *loc;
  const char *fmt = GET_RTX_FORMAT (GET_CODE (x));
  int len = GET_RTX_LENGTH (GET_CODE (x));
  int i, j;

  if (REG_P (x))
    {
      unsigned int regno = REGNO (x);
      unsigned int endregno = END_REGNO (x);
      unsigned int j;

      for (j = regno; j < endregno; j++)
	{
	  reg_stat_type *rsp = &reg_stat[j];
	  if (rsp->last_set_invalid
	      /* If this is a pseudo-register that was only set once and not
		 live at the beginning of the function, it is always valid.  */
	      || (! (regno >= FIRST_PSEUDO_REGISTER
		     && regno < reg_n_sets_max
		     && REG_N_SETS (regno) == 1
		     && (!REGNO_REG_SET_P
			 (DF_LR_IN (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb),
			  regno)))
		  && rsp->last_set_label > tick))
	  {
	    if (replace)
	      *loc = gen_rtx_CLOBBER (GET_MODE (x), const0_rtx);
	    return replace;
	  }
	}

      return true;
    }
  /* If this is a memory reference, make sure that there were no stores after
     it that might have clobbered the value.  We don't have alias info, so we
     assume any store invalidates it.  Moreover, we only have local UIDs, so
     we also assume that there were stores in the intervening basic blocks.  */
  else if (MEM_P (x) && !MEM_READONLY_P (x)
	   && (tick != label_tick || DF_INSN_LUID (insn) <= mem_last_set))
    {
      if (replace)
	*loc = gen_rtx_CLOBBER (GET_MODE (x), const0_rtx);
      return replace;
    }

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e')
	{
	  /* Check for identical subexpressions.  If x contains
	     identical subexpression we only have to traverse one of
	     them.  */
	  if (i == 1 && ARITHMETIC_P (x))
	    {
	      /* Note that at this point x0 has already been checked
		 and found valid.  */
	      rtx x0 = XEXP (x, 0);
	      rtx x1 = XEXP (x, 1);

	      /* If x0 and x1 are identical then x is also valid.  */
	      if (x0 == x1)
		return true;

	      /* If x1 is identical to a subexpression of x0 then
		 while checking x0, x1 has already been checked.  Thus
		 it is valid and so as x.  */
	      if (ARITHMETIC_P (x0)
		  && (x1 == XEXP (x0, 0) || x1 == XEXP (x0, 1)))
		return true;

	      /* If x0 is identical to a subexpression of x1 then x is
		 valid iff the rest of x1 is valid.  */
	      if (ARITHMETIC_P (x1)
		  && (x0 == XEXP (x1, 0) || x0 == XEXP (x1, 1)))
		return
		  get_last_value_validate (&XEXP (x1,
						  x0 == XEXP (x1, 0) ? 1 : 0),
					   insn, tick, replace);
	    }

	  if (!get_last_value_validate (&XEXP (x, i), insn, tick, replace))
	    return false;
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (!get_last_value_validate (&XVECEXP (x, i, j),
					insn, tick, replace))
	    return false;
    }

  /* If we haven't found a reason for it to be invalid, it is valid.  */
  return true;
}

/* Get the last value assigned to X, if known.  Some registers
   in the value may be replaced with (clobber (const_int 0)) if their value
   is known longer known reliably.  */

static rtx
get_last_value (const_rtx x)
{
  unsigned int regno;
  rtx value;
  reg_stat_type *rsp;

  /* If this is a non-paradoxical SUBREG, get the value of its operand and
     then convert it to the desired mode.  If this is a paradoxical SUBREG,
     we cannot predict what values the "extra" bits might have.  */
  if (GET_CODE (x) == SUBREG
      && subreg_lowpart_p (x)
      && !paradoxical_subreg_p (x)
      && (value = get_last_value (SUBREG_REG (x))) != 0)
    return gen_lowpart (GET_MODE (x), value);

  if (!REG_P (x))
    return 0;

  regno = REGNO (x);
  rsp = &reg_stat[regno];
  value = rsp->last_set_value;

  /* If we don't have a value, or if it isn't for this basic block and
     it's either a hard register, set more than once, or it's a live
     at the beginning of the function, return 0.

     Because if it's not live at the beginning of the function then the reg
     is always set before being used (is never used without being set).
     And, if it's set only once, and it's always set before use, then all
     uses must have the same last value, even if it's not from this basic
     block.  */

  if (value == 0
      || (rsp->last_set_label < label_tick_ebb_start
	  && (regno < FIRST_PSEUDO_REGISTER
	      || regno >= reg_n_sets_max
	      || REG_N_SETS (regno) != 1
	      || REGNO_REG_SET_P
		 (DF_LR_IN (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb), regno))))
    return 0;

  /* If the value was set in a later insn than the ones we are processing,
     we can't use it even if the register was only set once.  */
  if (rsp->last_set_label == label_tick
      && DF_INSN_LUID (rsp->last_set) >= subst_low_luid)
    return 0;

  /* If fewer bits were set than what we are asked for now, we cannot use
     the value.  */
  if (maybe_lt (GET_MODE_PRECISION (rsp->last_set_mode),
		GET_MODE_PRECISION (GET_MODE (x))))
    return 0;

  /* If the value has all its registers valid, return it.  */
  if (get_last_value_validate (&value, rsp->last_set,
			       rsp->last_set_label, false))
    return value;

  /* Otherwise, make a copy and replace any invalid register with
     (clobber (const_int 0)).  If that fails for some reason, return 0.  */

  value = copy_rtx (value);
  if (get_last_value_validate (&value, rsp->last_set,
			       rsp->last_set_label, true))
    return value;

  return 0;
}

/* Define three variables used for communication between the following
   routines.  */

static unsigned int reg_dead_regno, reg_dead_endregno;
static int reg_dead_flag;
rtx reg_dead_reg;

/* Function called via note_stores from reg_dead_at_p.

   If DEST is within [reg_dead_regno, reg_dead_endregno), set
   reg_dead_flag to 1 if X is a CLOBBER and to -1 it is a SET.  */

static void
reg_dead_at_p_1 (rtx dest, const_rtx x, void *data ATTRIBUTE_UNUSED)
{
  unsigned int regno, endregno;

  if (!REG_P (dest))
    return;

  regno = REGNO (dest);
  endregno = END_REGNO (dest);
  if (reg_dead_endregno > regno && reg_dead_regno < endregno)
    reg_dead_flag = (GET_CODE (x) == CLOBBER) ? 1 : -1;
}

/* Return true if REG is known to be dead at INSN.

   We scan backwards from INSN.  If we hit a REG_DEAD note or a CLOBBER
   referencing REG, it is dead.  If we hit a SET referencing REG, it is
   live.  Otherwise, see if it is live or dead at the start of the basic
   block we are in.  Hard regs marked as being live in NEWPAT_USED_REGS
   must be assumed to be always live.  */

static bool
reg_dead_at_p (rtx reg, rtx_insn *insn)
{
  basic_block block;
  unsigned int i;

  /* Set variables for reg_dead_at_p_1.  */
  reg_dead_regno = REGNO (reg);
  reg_dead_endregno = END_REGNO (reg);
  reg_dead_reg = reg;

  reg_dead_flag = 0;

  /* Check that reg isn't mentioned in NEWPAT_USED_REGS.  For fixed registers
     we allow the machine description to decide whether use-and-clobber
     patterns are OK.  */
  if (reg_dead_regno < FIRST_PSEUDO_REGISTER)
    {
      for (i = reg_dead_regno; i < reg_dead_endregno; i++)
	if (!fixed_regs[i] && TEST_HARD_REG_BIT (newpat_used_regs, i))
	  return false;
    }

  /* Scan backwards until we find a REG_DEAD note, SET, CLOBBER, or
     beginning of basic block.  */
  block = BLOCK_FOR_INSN (insn);
  for (;;)
    {
      if (INSN_P (insn))
        {
	  if (find_regno_note (insn, REG_UNUSED, reg_dead_regno))
	    return true;

	  note_stores (insn, reg_dead_at_p_1, NULL);
	  if (reg_dead_flag)
	    return reg_dead_flag == 1 ? 1 : 0;

	  if (find_regno_note (insn, REG_DEAD, reg_dead_regno))
	    return true;
        }

      if (insn == BB_HEAD (block))
	break;

      insn = PREV_INSN (insn);
    }

  /* Look at live-in sets for the basic block that we were in.  */
  for (i = reg_dead_regno; i < reg_dead_endregno; i++)
    if (REGNO_REG_SET_P (df_get_live_in (block), i))
      return false;

  return true;
}

/* Note hard registers in X that are used.  */

static void
mark_used_regs_combine (rtx x)
{
  RTX_CODE code = GET_CODE (x);
  unsigned int regno;
  int i;

  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    CASE_CONST_ANY:
    case PC:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case ASM_INPUT:
      return;

    case CLOBBER:
      /* If we are clobbering a MEM, mark any hard registers inside the
	 address as used.  */
      if (MEM_P (XEXP (x, 0)))
	mark_used_regs_combine (XEXP (XEXP (x, 0), 0));
      return;

    case REG:
      regno = REGNO (x);
      /* A hard reg in a wide mode may really be multiple registers.
	 If so, mark all of them just like the first.  */
      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  /* None of this applies to the stack, frame or arg pointers.  */
	  if (regno == STACK_POINTER_REGNUM
	      || (!HARD_FRAME_POINTER_IS_FRAME_POINTER
		  && regno == HARD_FRAME_POINTER_REGNUM)
	      || (FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
		  && regno == ARG_POINTER_REGNUM && fixed_regs[regno])
	      || regno == FRAME_POINTER_REGNUM)
	    return;

	  add_to_hard_reg_set (&newpat_used_regs, GET_MODE (x), regno);
	}
      return;

    case SET:
      {
	/* If setting a MEM, or a SUBREG of a MEM, then note any hard regs in
	   the address.  */
	rtx testreg = SET_DEST (x);

	while (GET_CODE (testreg) == SUBREG
	       || GET_CODE (testreg) == ZERO_EXTRACT
	       || GET_CODE (testreg) == STRICT_LOW_PART)
	  testreg = XEXP (testreg, 0);

	if (MEM_P (testreg))
	  mark_used_regs_combine (XEXP (testreg, 0));

	mark_used_regs_combine (SET_SRC (x));
      }
      return;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  {
    const char *fmt = GET_RTX_FORMAT (code);

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  mark_used_regs_combine (XEXP (x, i));
	else if (fmt[i] == 'E')
	  {
	    int j;

	    for (j = 0; j < XVECLEN (x, i); j++)
	      mark_used_regs_combine (XVECEXP (x, i, j));
	  }
      }
  }
}

/* Remove register number REGNO from the dead registers list of INSN.

   Return the note used to record the death, if there was one.  */

rtx
remove_death (unsigned int regno, rtx_insn *insn)
{
  rtx note = find_regno_note (insn, REG_DEAD, regno);

  if (note)
    remove_note (insn, note);

  return note;
}

/* For each register (hardware or pseudo) used within expression X, if its
   death is in an instruction with luid between FROM_LUID (inclusive) and
   TO_INSN (exclusive), put a REG_DEAD note for that register in the
   list headed by PNOTES.

   That said, don't move registers killed by maybe_kill_insn.

   This is done when X is being merged by combination into TO_INSN.  These
   notes will then be distributed as needed.  */

static void
move_deaths (rtx x, rtx maybe_kill_insn, int from_luid, rtx_insn *to_insn,
	     rtx *pnotes)
{
  const char *fmt;
  int len, i;
  enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      unsigned int regno = REGNO (x);
      rtx_insn *where_dead = reg_stat[regno].last_death;

      /* If we do not know where the register died, it may still die between
	 FROM_LUID and TO_INSN.  If so, find it.  This is PR83304.  */
      if (!where_dead || DF_INSN_LUID (where_dead) >= DF_INSN_LUID (to_insn))
	{
	  rtx_insn *insn = prev_real_nondebug_insn (to_insn);
	  while (insn
		 && BLOCK_FOR_INSN (insn) == BLOCK_FOR_INSN (to_insn)
		 && DF_INSN_LUID (insn) >= from_luid)
	    {
	      if (dead_or_set_regno_p (insn, regno))
		{
		  if (find_regno_note (insn, REG_DEAD, regno))
		    where_dead = insn;
		  break;
		}

	      insn = prev_real_nondebug_insn (insn);
	    }
	}

      /* Don't move the register if it gets killed in between from and to.  */
      if (maybe_kill_insn && reg_set_p (x, maybe_kill_insn)
	  && ! reg_referenced_p (x, maybe_kill_insn))
	return;

      if (where_dead
	  && BLOCK_FOR_INSN (where_dead) == BLOCK_FOR_INSN (to_insn)
	  && DF_INSN_LUID (where_dead) >= from_luid
	  && DF_INSN_LUID (where_dead) < DF_INSN_LUID (to_insn))
	{
	  rtx note = remove_death (regno, where_dead);

	  /* It is possible for the call above to return 0.  This can occur
	     when last_death points to I2 or I1 that we combined with.
	     In that case make a new note.

	     We must also check for the case where X is a hard register
	     and NOTE is a death note for a range of hard registers
	     including X.  In that case, we must put REG_DEAD notes for
	     the remaining registers in place of NOTE.  */

	  if (note != 0 && regno < FIRST_PSEUDO_REGISTER
	      && partial_subreg_p (GET_MODE (x), GET_MODE (XEXP (note, 0))))
	    {
	      unsigned int deadregno = REGNO (XEXP (note, 0));
	      unsigned int deadend = END_REGNO (XEXP (note, 0));
	      unsigned int ourend = END_REGNO (x);
	      unsigned int i;

	      for (i = deadregno; i < deadend; i++)
		if (i < regno || i >= ourend)
		  add_reg_note (where_dead, REG_DEAD, regno_reg_rtx[i]);
	    }

	  /* If we didn't find any note, or if we found a REG_DEAD note that
	     covers only part of the given reg, and we have a multi-reg hard
	     register, then to be safe we must check for REG_DEAD notes
	     for each register other than the first.  They could have
	     their own REG_DEAD notes lying around.  */
	  else if ((note == 0
		    || (note != 0
			&& partial_subreg_p (GET_MODE (XEXP (note, 0)),
					     GET_MODE (x))))
		   && regno < FIRST_PSEUDO_REGISTER
		   && REG_NREGS (x) > 1)
	    {
	      unsigned int ourend = END_REGNO (x);
	      unsigned int i, offset;
	      rtx oldnotes = 0;

	      if (note)
		offset = hard_regno_nregs (regno, GET_MODE (XEXP (note, 0)));
	      else
		offset = 1;

	      for (i = regno + offset; i < ourend; i++)
		move_deaths (regno_reg_rtx[i],
			     maybe_kill_insn, from_luid, to_insn, &oldnotes);
	    }

	  if (note != 0 && GET_MODE (XEXP (note, 0)) == GET_MODE (x))
	    {
	      XEXP (note, 1) = *pnotes;
	      *pnotes = note;
	    }
	  else
	    *pnotes = alloc_reg_note (REG_DEAD, x, *pnotes);
	}

      return;
    }

  else if (GET_CODE (x) == SET)
    {
      rtx dest = SET_DEST (x);

      move_deaths (SET_SRC (x), maybe_kill_insn, from_luid, to_insn, pnotes);

      /* In the case of a ZERO_EXTRACT, a STRICT_LOW_PART, or a SUBREG
	 that accesses one word of a multi-word item, some
	 piece of everything register in the expression is used by
	 this insn, so remove any old death.  */
      /* ??? So why do we test for equality of the sizes?  */

      if (GET_CODE (dest) == ZERO_EXTRACT
	  || GET_CODE (dest) == STRICT_LOW_PART
	  || (GET_CODE (dest) == SUBREG
	      && !read_modify_subreg_p (dest)))
	{
	  move_deaths (dest, maybe_kill_insn, from_luid, to_insn, pnotes);
	  return;
	}

      /* If this is some other SUBREG, we know it replaces the entire
	 value, so use that as the destination.  */
      if (GET_CODE (dest) == SUBREG)
	dest = SUBREG_REG (dest);

      /* If this is a MEM, adjust deaths of anything used in the address.
	 For a REG (the only other possibility), the entire value is
	 being replaced so the old value is not used in this insn.  */

      if (MEM_P (dest))
	move_deaths (XEXP (dest, 0), maybe_kill_insn, from_luid,
		     to_insn, pnotes);
      return;
    }

  else if (GET_CODE (x) == CLOBBER)
    return;

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    move_deaths (XVECEXP (x, i, j), maybe_kill_insn, from_luid,
			 to_insn, pnotes);
	}
      else if (fmt[i] == 'e')
	move_deaths (XEXP (x, i), maybe_kill_insn, from_luid, to_insn, pnotes);
    }
}

/* Return true if X is the target of a bit-field assignment in BODY, the
   pattern of an insn.  X must be a REG.  */

static bool
reg_bitfield_target_p (rtx x, rtx body)
{
  int i;

  if (GET_CODE (body) == SET)
    {
      rtx dest = SET_DEST (body);
      rtx target;
      unsigned int regno, tregno, endregno, endtregno;

      if (GET_CODE (dest) == ZERO_EXTRACT)
	target = XEXP (dest, 0);
      else if (GET_CODE (dest) == STRICT_LOW_PART)
	target = SUBREG_REG (XEXP (dest, 0));
      else
	return false;

      if (GET_CODE (target) == SUBREG)
	target = SUBREG_REG (target);

      if (!REG_P (target))
	return false;

      tregno = REGNO (target), regno = REGNO (x);
      if (tregno >= FIRST_PSEUDO_REGISTER || regno >= FIRST_PSEUDO_REGISTER)
	return target == x;

      endtregno = end_hard_regno (GET_MODE (target), tregno);
      endregno = end_hard_regno (GET_MODE (x), regno);

      return endregno > tregno && regno < endtregno;
    }

  else if (GET_CODE (body) == PARALLEL)
    for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
      if (reg_bitfield_target_p (x, XVECEXP (body, 0, i)))
	return true;

  return false;
}

/* Given a chain of REG_NOTES originally from FROM_INSN, try to place them
   as appropriate.  I3 and I2 are the insns resulting from the combination
   insns including FROM (I2 may be zero).

   ELIM_I2 and ELIM_I1 are either zero or registers that we know will
   not need REG_DEAD notes because they are being substituted for.  This
   saves searching in the most common cases.

   Each note in the list is either ignored or placed on some insns, depending
   on the type of note.  */

static void
distribute_notes (rtx notes, rtx_insn *from_insn, rtx_insn *i3, rtx_insn *i2,
		  rtx elim_i2, rtx elim_i1, rtx elim_i0)
{
  rtx note, next_note;
  rtx tem_note;
  rtx_insn *tem_insn;

  for (note = notes; note; note = next_note)
    {
      rtx_insn *place = 0, *place2 = 0;

      next_note = XEXP (note, 1);
      switch (REG_NOTE_KIND (note))
	{
	case REG_BR_PROB:
	case REG_BR_PRED:
	  /* Doesn't matter much where we put this, as long as it's somewhere.
	     It is preferable to keep these notes on branches, which is most
	     likely to be i3.  */
	  place = i3;
	  break;

	case REG_NON_LOCAL_GOTO:
	  if (JUMP_P (i3))
	    place = i3;
	  else
	    {
	      gcc_assert (i2 && JUMP_P (i2));
	      place = i2;
	    }
	  break;

	case REG_EH_REGION:
	  {
	    /* The landing pad handling needs to be kept in sync with the
	       prerequisite checking in try_combine.  */
	    int lp_nr = INTVAL (XEXP (note, 0));
	    /* A REG_EH_REGION note transfering control can only ever come
	       from i3.  */
	    if (lp_nr > 0)
	      gcc_assert (from_insn == i3);
	    /* We are making sure there is a single effective REG_EH_REGION
	       note and it's valid to put it on i3.  */
	    if (!insn_could_throw_p (from_insn)
		&& !(lp_nr == INT_MIN && can_nonlocal_goto (from_insn)))
	      /* Throw away stray notes on insns that can never throw or
		 make a nonlocal goto.  */
	      ;
	    else
	      {
		if (CALL_P (i3))
		  place = i3;
		else
		  {
		    gcc_assert (cfun->can_throw_non_call_exceptions);
		    /* If i3 can still trap preserve the note, otherwise we've
		       combined things such that we can now prove that the
		       instructions can't trap.  Drop the note in this case.  */
		    if (may_trap_p (i3))
		      place = i3;
		  }
	      }
	    break;
	  }

	case REG_ARGS_SIZE:
	  /* ??? How to distribute between i3-i1.  Assume i3 contains the
	     entire adjustment.  Assert i3 contains at least some adjust.  */
	  if (!noop_move_p (i3))
	    {
	      poly_int64 old_size, args_size = get_args_size (note);
	      /* fixup_args_size_notes looks at REG_NORETURN note,
		 so ensure the note is placed there first.  */
	      if (CALL_P (i3))
		{
		  rtx *np;
		  for (np = &next_note; *np; np = &XEXP (*np, 1))
		    if (REG_NOTE_KIND (*np) == REG_NORETURN)
		      {
			rtx n = *np;
			*np = XEXP (n, 1);
			XEXP (n, 1) = REG_NOTES (i3);
			REG_NOTES (i3) = n;
			break;
		      }
		}
	      old_size = fixup_args_size_notes (PREV_INSN (i3), i3, args_size);
	      /* emit_call_1 adds for !ACCUMULATE_OUTGOING_ARGS
		 REG_ARGS_SIZE note to all noreturn calls, allow that here.  */
	      gcc_assert (maybe_ne (old_size, args_size)
			  || (CALL_P (i3)
			      && !ACCUMULATE_OUTGOING_ARGS
			      && find_reg_note (i3, REG_NORETURN, NULL_RTX)));
	    }
	  break;

	case REG_NORETURN:
	case REG_SETJMP:
	case REG_TM:
	case REG_CALL_DECL:
	case REG_UNTYPED_CALL:
	case REG_CALL_NOCF_CHECK:
	  /* These notes must remain with the call.  It should not be
	     possible for both I2 and I3 to be a call.  */
	  if (CALL_P (i3))
	    place = i3;
	  else
	    {
	      gcc_assert (i2 && CALL_P (i2));
	      place = i2;
	    }
	  break;

	case REG_UNUSED:
	  /* Any clobbers for i3 may still exist, and so we must process
	     REG_UNUSED notes from that insn.

	     Any clobbers from i2 or i1 can only exist if they were added by
	     recog_for_combine.  In that case, recog_for_combine created the
	     necessary REG_UNUSED notes.  Trying to keep any original
	     REG_UNUSED notes from these insns can cause incorrect output
	     if it is for the same register as the original i3 dest.
	     In that case, we will notice that the register is set in i3,
	     and then add a REG_UNUSED note for the destination of i3, which
	     is wrong.  However, it is possible to have REG_UNUSED notes from
	     i2 or i1 for register which were both used and clobbered, so
	     we keep notes from i2 or i1 if they will turn into REG_DEAD
	     notes.  */

	  /* If this register is set or clobbered between FROM_INSN and I3,
	     we should not create a note for it.  */
	  if (reg_set_between_p (XEXP (note, 0), from_insn, i3))
	    break;

	  /* If this register is set or clobbered in I3, put the note there
	     unless there is one already.  */
	  if (reg_set_p (XEXP (note, 0), PATTERN (i3)))
	    {
	      if (from_insn != i3)
		break;

	      if (! (REG_P (XEXP (note, 0))
		     ? find_regno_note (i3, REG_UNUSED, REGNO (XEXP (note, 0)))
		     : find_reg_note (i3, REG_UNUSED, XEXP (note, 0))))
		place = i3;
	    }
	  /* Otherwise, if this register is used by I3, then this register
	     now dies here, so we must put a REG_DEAD note here unless there
	     is one already.  */
	  else if (reg_referenced_p (XEXP (note, 0), PATTERN (i3))
		   && ! (REG_P (XEXP (note, 0))
			 ? find_regno_note (i3, REG_DEAD,
					    REGNO (XEXP (note, 0)))
			 : find_reg_note (i3, REG_DEAD, XEXP (note, 0))))
	    {
	      PUT_REG_NOTE_KIND (note, REG_DEAD);
	      place = i3;
	    }

	  /* A SET or CLOBBER of the REG_UNUSED reg has been removed,
	     but we can't tell which at this point.  We must reset any
	     expectations we had about the value that was previously
	     stored in the reg.  ??? Ideally, we'd adjust REG_N_SETS
	     and, if appropriate, restore its previous value, but we
	     don't have enough information for that at this point.  */
	  else
	    {
	      record_value_for_reg (XEXP (note, 0), NULL, NULL_RTX);

	      /* Otherwise, if this register is now referenced in i2
		 then the register used to be modified in one of the
		 original insns.  If it was i3 (say, in an unused
		 parallel), it's now completely gone, so the note can
		 be discarded.  But if it was modified in i2, i1 or i0
		 and we still reference it in i2, then we're
		 referencing the previous value, and since the
		 register was modified and REG_UNUSED, we know that
		 the previous value is now dead.  So, if we only
		 reference the register in i2, we change the note to
		 REG_DEAD, to reflect the previous value.  However, if
		 we're also setting or clobbering the register as
		 scratch, we know (because the register was not
		 referenced in i3) that it's unused, just as it was
		 unused before, and we place the note in i2.  */
	      if (from_insn != i3 && i2 && INSN_P (i2)
		  && reg_referenced_p (XEXP (note, 0), PATTERN (i2)))
		{
		  if (!reg_set_p (XEXP (note, 0), PATTERN (i2)))
		    PUT_REG_NOTE_KIND (note, REG_DEAD);
		  if (! (REG_P (XEXP (note, 0))
			 ? find_regno_note (i2, REG_NOTE_KIND (note),
					    REGNO (XEXP (note, 0)))
			 : find_reg_note (i2, REG_NOTE_KIND (note),
					  XEXP (note, 0))))
		    place = i2;
		}
	    }

	  break;

	case REG_EQUAL:
	case REG_EQUIV:
	case REG_NOALIAS:
	  /* These notes say something about results of an insn.  We can
	     only support them if they used to be on I3 in which case they
	     remain on I3.  Otherwise they are ignored.

	     If the note refers to an expression that is not a constant, we
	     must also ignore the note since we cannot tell whether the
	     equivalence is still true.  It might be possible to do
	     slightly better than this (we only have a problem if I2DEST
	     or I1DEST is present in the expression), but it doesn't
	     seem worth the trouble.  */

	  if (from_insn == i3
	      && (XEXP (note, 0) == 0 || CONSTANT_P (XEXP (note, 0))))
	    place = i3;
	  break;

	case REG_INC:
	  /* These notes say something about how a register is used.  They must
	     be present on any use of the register in I2 or I3.  */
	  if (reg_mentioned_p (XEXP (note, 0), PATTERN (i3)))
	    place = i3;

	  if (i2 && reg_mentioned_p (XEXP (note, 0), PATTERN (i2)))
	    {
	      if (place)
		place2 = i2;
	      else
		place = i2;
	    }
	  break;

	case REG_LABEL_TARGET:
	case REG_LABEL_OPERAND:
	  /* This can show up in several ways -- either directly in the
	     pattern, or hidden off in the constant pool with (or without?)
	     a REG_EQUAL note.  */
	  /* ??? Ignore the without-reg_equal-note problem for now.  */
	  if (reg_mentioned_p (XEXP (note, 0), PATTERN (i3))
	      || ((tem_note = find_reg_note (i3, REG_EQUAL, NULL_RTX))
		  && GET_CODE (XEXP (tem_note, 0)) == LABEL_REF
		  && label_ref_label (XEXP (tem_note, 0)) == XEXP (note, 0)))
	    place = i3;

	  if (i2
	      && (reg_mentioned_p (XEXP (note, 0), PATTERN (i2))
		  || ((tem_note = find_reg_note (i2, REG_EQUAL, NULL_RTX))
		      && GET_CODE (XEXP (tem_note, 0)) == LABEL_REF
		      && label_ref_label (XEXP (tem_note, 0)) == XEXP (note, 0))))
	    {
	      if (place)
		place2 = i2;
	      else
		place = i2;
	    }

	  /* For REG_LABEL_TARGET on a JUMP_P, we prefer to put the note
	     as a JUMP_LABEL or decrement LABEL_NUSES if it's already
	     there.  */
	  if (place && JUMP_P (place)
	      && REG_NOTE_KIND (note) == REG_LABEL_TARGET
	      && (JUMP_LABEL (place) == NULL
		  || JUMP_LABEL (place) == XEXP (note, 0)))
	    {
	      rtx label = JUMP_LABEL (place);

	      if (!label)
		JUMP_LABEL (place) = XEXP (note, 0);
	      else if (LABEL_P (label))
		LABEL_NUSES (label)--;
	    }

	  if (place2 && JUMP_P (place2)
	      && REG_NOTE_KIND (note) == REG_LABEL_TARGET
	      && (JUMP_LABEL (place2) == NULL
		  || JUMP_LABEL (place2) == XEXP (note, 0)))
	    {
	      rtx label = JUMP_LABEL (place2);

	      if (!label)
		JUMP_LABEL (place2) = XEXP (note, 0);
	      else if (LABEL_P (label))
		LABEL_NUSES (label)--;
	      place2 = 0;
	    }
	  break;

	case REG_NONNEG:
	  /* This note says something about the value of a register prior
	     to the execution of an insn.  It is too much trouble to see
	     if the note is still correct in all situations.  It is better
	     to simply delete it.  */
	  break;

	case REG_DEAD:
	  /* If we replaced the right hand side of FROM_INSN with a
	     REG_EQUAL note, the original use of the dying register
	     will not have been combined into I3 and I2.  In such cases,
	     FROM_INSN is guaranteed to be the first of the combined
	     instructions, so we simply need to search back before
	     FROM_INSN for the previous use or set of this register,
	     then alter the notes there appropriately.

	     If the register is used as an input in I3, it dies there.
	     Similarly for I2, if it is nonzero and adjacent to I3.

	     If the register is not used as an input in either I3 or I2
	     and it is not one of the registers we were supposed to eliminate,
	     there are two possibilities.  We might have a non-adjacent I2
	     or we might have somehow eliminated an additional register
	     from a computation.  For example, we might have had A & B where
	     we discover that B will always be zero.  In this case we will
	     eliminate the reference to A.

	     In both cases, we must search to see if we can find a previous
	     use of A and put the death note there.  */

	  if (from_insn
	      && from_insn == i2mod
	      && !reg_overlap_mentioned_p (XEXP (note, 0), i2mod_new_rhs))
	    tem_insn = from_insn;
	  else
	    {
	      if (from_insn
		  && CALL_P (from_insn)
		  && find_reg_fusage (from_insn, USE, XEXP (note, 0)))
		place = from_insn;
	      else if (i2 && reg_set_p (XEXP (note, 0), PATTERN (i2)))
		{
		  /* If the new I2 sets the same register that is marked
		     dead in the note, we do not in general know where to
		     put the note.  One important case we _can_ handle is
		     when the note comes from I3.  */
		  if (from_insn == i3)
		    place = i3;
		  else
		    break;
		}
	      else if (reg_referenced_p (XEXP (note, 0), PATTERN (i3)))
		place = i3;
	      else if (i2 != 0 && next_nonnote_nondebug_insn (i2) == i3
		       && reg_referenced_p (XEXP (note, 0), PATTERN (i2)))
		place = i2;
	      else if ((rtx_equal_p (XEXP (note, 0), elim_i2)
			&& !(i2mod
			     && reg_overlap_mentioned_p (XEXP (note, 0),
							 i2mod_old_rhs)))
		       || rtx_equal_p (XEXP (note, 0), elim_i1)
		       || rtx_equal_p (XEXP (note, 0), elim_i0))
		break;
	      tem_insn = i3;
	    }

	  if (place == 0)
	    {
	      basic_block bb = this_basic_block;

	      for (tem_insn = PREV_INSN (tem_insn); place == 0; tem_insn = PREV_INSN (tem_insn))
		{
		  if (!NONDEBUG_INSN_P (tem_insn))
		    {
		      if (tem_insn == BB_HEAD (bb))
			break;
		      continue;
		    }

		  /* If the register is being set at TEM_INSN, see if that is all
		     TEM_INSN is doing.  If so, delete TEM_INSN.  Otherwise, make this
		     into a REG_UNUSED note instead. Don't delete sets to
		     global register vars.  */
		  if ((REGNO (XEXP (note, 0)) >= FIRST_PSEUDO_REGISTER
		       || !global_regs[REGNO (XEXP (note, 0))])
		      && reg_set_p (XEXP (note, 0), PATTERN (tem_insn)))
		    {
		      rtx set = single_set (tem_insn);
		      rtx inner_dest = 0;

		      if (set != 0)
			for (inner_dest = SET_DEST (set);
			     (GET_CODE (inner_dest) == STRICT_LOW_PART
			      || GET_CODE (inner_dest) == SUBREG
			      || GET_CODE (inner_dest) == ZERO_EXTRACT);
			     inner_dest = XEXP (inner_dest, 0))
			  ;

		      /* Verify that it was the set, and not a clobber that
			 modified the register.

			 If we cannot delete the setter due to side
			 effects, mark the user with an UNUSED note instead
			 of deleting it.  */

		      if (set != 0 && ! side_effects_p (SET_SRC (set))
			  && rtx_equal_p (XEXP (note, 0), inner_dest))
			{
			  /* Move the notes and links of TEM_INSN elsewhere.
			     This might delete other dead insns recursively.
			     First set the pattern to something that won't use
			     any register.  */
			  rtx old_notes = REG_NOTES (tem_insn);

			  PATTERN (tem_insn) = pc_rtx;
			  REG_NOTES (tem_insn) = NULL;

			  distribute_notes (old_notes, tem_insn, tem_insn, NULL,
					    NULL_RTX, NULL_RTX, NULL_RTX);
			  distribute_links (LOG_LINKS (tem_insn));

			  unsigned int regno = REGNO (XEXP (note, 0));
			  reg_stat_type *rsp = &reg_stat[regno];
			  if (rsp->last_set == tem_insn)
			    record_value_for_reg (XEXP (note, 0), NULL, NULL_RTX);

			  SET_INSN_DELETED (tem_insn);
			  if (tem_insn == i2)
			    i2 = NULL;
			}
		      else
			{
			  PUT_REG_NOTE_KIND (note, REG_UNUSED);

			  /*  If there isn't already a REG_UNUSED note, put one
			      here.  Do not place a REG_DEAD note, even if
			      the register is also used here; that would not
			      match the algorithm used in lifetime analysis
			      and can cause the consistency check in the
			      scheduler to fail.  */
			  if (! find_regno_note (tem_insn, REG_UNUSED,
						 REGNO (XEXP (note, 0))))
			    place = tem_insn;
			  break;
			}
		    }
		  else if (reg_referenced_p (XEXP (note, 0), PATTERN (tem_insn))
			   || (CALL_P (tem_insn)
			       && find_reg_fusage (tem_insn, USE, XEXP (note, 0))))
		    {
		      place = tem_insn;

		      /* If we are doing a 3->2 combination, and we have a
			 register which formerly died in i3 and was not used
			 by i2, which now no longer dies in i3 and is used in
			 i2 but does not die in i2, and place is between i2
			 and i3, then we may need to move a link from place to
			 i2.  */
		      if (i2 && DF_INSN_LUID (place) > DF_INSN_LUID (i2)
			  && from_insn
			  && DF_INSN_LUID (from_insn) > DF_INSN_LUID (i2)
			  && reg_referenced_p (XEXP (note, 0), PATTERN (i2)))
			{
			  struct insn_link *links = LOG_LINKS (place);
			  LOG_LINKS (place) = NULL;
			  distribute_links (links);
			}
		      break;
		    }

		  if (tem_insn == BB_HEAD (bb))
		    break;
		}

	    }

	  /* If the register is set or already dead at PLACE, we needn't do
	     anything with this note if it is still a REG_DEAD note.
	     We check here if it is set at all, not if is it totally replaced,
	     which is what `dead_or_set_p' checks, so also check for it being
	     set partially.  */

	  if (place && REG_NOTE_KIND (note) == REG_DEAD)
	    {
	      unsigned int regno = REGNO (XEXP (note, 0));
	      reg_stat_type *rsp = &reg_stat[regno];

	      if (dead_or_set_p (place, XEXP (note, 0))
		  || reg_bitfield_target_p (XEXP (note, 0), PATTERN (place)))
		{
		  /* Unless the register previously died in PLACE, clear
		     last_death.  [I no longer understand why this is
		     being done.] */
		  if (rsp->last_death != place)
		    rsp->last_death = 0;
		  place = 0;
		}
	      else
		rsp->last_death = place;

	      /* If this is a death note for a hard reg that is occupying
		 multiple registers, ensure that we are still using all
		 parts of the object.  If we find a piece of the object
		 that is unused, we must arrange for an appropriate REG_DEAD
		 note to be added for it.  However, we can't just emit a USE
		 and tag the note to it, since the register might actually
		 be dead; so we recourse, and the recursive call then finds
		 the previous insn that used this register.  */

	      if (place && REG_NREGS (XEXP (note, 0)) > 1)
		{
		  unsigned int endregno = END_REGNO (XEXP (note, 0));
		  bool all_used = true;
		  unsigned int i;

		  for (i = regno; i < endregno; i++)
		    if ((! refers_to_regno_p (i, PATTERN (place))
			 && ! find_regno_fusage (place, USE, i))
			|| dead_or_set_regno_p (place, i))
		      {
			all_used = false;
			break;
		      }

		  if (! all_used)
		    {
		      /* Put only REG_DEAD notes for pieces that are
			 not already dead or set.  */

		      for (i = regno; i < endregno;
			   i += hard_regno_nregs (i, reg_raw_mode[i]))
			{
			  rtx piece = regno_reg_rtx[i];
			  basic_block bb = this_basic_block;

			  if (! dead_or_set_p (place, piece)
			      && ! reg_bitfield_target_p (piece,
							  PATTERN (place)))
			    {
			      rtx new_note = alloc_reg_note (REG_DEAD, piece,
							     NULL_RTX);

			      distribute_notes (new_note, place, place,
						NULL, NULL_RTX, NULL_RTX,
						NULL_RTX);
			    }
			  else if (! refers_to_regno_p (i, PATTERN (place))
				   && ! find_regno_fusage (place, USE, i))
			    for (tem_insn = PREV_INSN (place); ;
				 tem_insn = PREV_INSN (tem_insn))
			      {
				if (!NONDEBUG_INSN_P (tem_insn))
				  {
				    if (tem_insn == BB_HEAD (bb))
			 	      break;
				    continue;
				  }
				if (dead_or_set_p (tem_insn, piece)
				    || reg_bitfield_target_p (piece,
							      PATTERN (tem_insn)))
				  {
				    add_reg_note (tem_insn, REG_UNUSED, piece);
				    break;
				  }
			      }
			}

		      place = 0;
		    }
		}
	    }
	  break;

	default:
	  /* Any other notes should not be present at this point in the
	     compilation.  */
	  gcc_unreachable ();
	}

      if (place)
	{
	  XEXP (note, 1) = REG_NOTES (place);
	  REG_NOTES (place) = note;

	  /* Set added_notes_insn to the earliest insn we added a note to.  */
	  if (added_notes_insn == 0
	      || DF_INSN_LUID (added_notes_insn) > DF_INSN_LUID (place))
	    added_notes_insn = place;
	}

      if (place2)
	{
	  add_shallow_copy_of_reg_note (place2, note);

	  /* Set added_notes_insn to the earliest insn we added a note to.  */
	  if (added_notes_insn == 0
	      || DF_INSN_LUID (added_notes_insn) > DF_INSN_LUID (place2))
	    added_notes_insn = place2;
	}
    }
}

/* Similarly to above, distribute the LOG_LINKS that used to be present on
   I3, I2, and I1 to new locations.  This is also called to add a link
   pointing at I3 when I3's destination is changed.  */

static void
distribute_links (struct insn_link *links)
{
  struct insn_link *link, *next_link;

  for (link = links; link; link = next_link)
    {
      rtx_insn *place = 0;
      rtx_insn *insn;
      rtx set, reg;

      next_link = link->next;

      /* If the insn that this link points to is a NOTE, ignore it.  */
      if (NOTE_P (link->insn))
	continue;

      set = 0;
      rtx pat = PATTERN (link->insn);
      if (GET_CODE (pat) == SET)
	set = pat;
      else if (GET_CODE (pat) == PARALLEL)
	{
	  int i;
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    {
	      set = XVECEXP (pat, 0, i);
	      if (GET_CODE (set) != SET)
		continue;

	      reg = SET_DEST (set);
	      while (GET_CODE (reg) == ZERO_EXTRACT
		     || GET_CODE (reg) == STRICT_LOW_PART
		     || GET_CODE (reg) == SUBREG)
		reg = XEXP (reg, 0);

	      if (!REG_P (reg))
		continue;

	      if (REGNO (reg) == link->regno)
		break;
	    }
	  if (i == XVECLEN (pat, 0))
	    continue;
	}
      else
	continue;

      reg = SET_DEST (set);

      while (GET_CODE (reg) == ZERO_EXTRACT
	     || GET_CODE (reg) == STRICT_LOW_PART
	     || GET_CODE (reg) == SUBREG)
	reg = XEXP (reg, 0);

      if (reg == pc_rtx)
	continue;

      /* A LOG_LINK is defined as being placed on the first insn that uses
	 a register and points to the insn that sets the register.  Start
	 searching at the next insn after the target of the link and stop
	 when we reach a set of the register or the end of the basic block.

	 Note that this correctly handles the link that used to point from
	 I3 to I2.  Also note that not much searching is typically done here
	 since most links don't point very far away.  */

      for (insn = NEXT_INSN (link->insn);
	   (insn && (this_basic_block->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
		     || BB_HEAD (this_basic_block->next_bb) != insn));
	   insn = NEXT_INSN (insn))
	if (DEBUG_INSN_P (insn))
	  continue;
	else if (INSN_P (insn) && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	  {
	    if (reg_referenced_p (reg, PATTERN (insn)))
	      place = insn;
	    break;
	  }
	else if (CALL_P (insn)
		 && find_reg_fusage (insn, USE, reg))
	  {
	    place = insn;
	    break;
	  }
	else if (INSN_P (insn) && reg_set_p (reg, insn))
	  break;

      /* If we found a place to put the link, place it there unless there
	 is already a link to the same insn as LINK at that point.  */

      if (place)
	{
	  struct insn_link *link2;

	  FOR_EACH_LOG_LINK (link2, place)
	    if (link2->insn == link->insn && link2->regno == link->regno)
	      break;

	  if (link2 == NULL)
	    {
	      link->next = LOG_LINKS (place);
	      LOG_LINKS (place) = link;

	      /* Set added_links_insn to the earliest insn we added a
		 link to.  */
	      if (added_links_insn == 0
		  || DF_INSN_LUID (added_links_insn) > DF_INSN_LUID (place))
		added_links_insn = place;
	    }
	}
    }
}

/* Check for any register or memory mentioned in EQUIV that is not
   mentioned in EXPR.  This is used to restrict EQUIV to "specializations"
   of EXPR where some registers may have been replaced by constants.  */

static bool
unmentioned_reg_p (rtx equiv, rtx expr)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, equiv, NONCONST)
    {
      const_rtx x = *iter;
      if ((REG_P (x) || MEM_P (x))
	  && !reg_mentioned_p (x, expr))
	return true;
    }
  return false;
}

/* Make pseudo-to-pseudo copies after every hard-reg-to-pseudo-copy, because
   the reg-to-reg copy can usefully combine with later instructions, but we
   do not want to combine the hard reg into later instructions, for that
   restricts register allocation.  */
static void
make_more_copies (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

      FOR_BB_INSNS (bb, insn)
        {
          if (!NONDEBUG_INSN_P (insn))
            continue;

	  rtx set = single_set (insn);
	  if (!set)
	    continue;

	  rtx dest = SET_DEST (set);
	  if (!(REG_P (dest) && !HARD_REGISTER_P (dest)))
	      continue;

	  rtx src = SET_SRC (set);
	  if (!(REG_P (src) && HARD_REGISTER_P (src)))
	    continue;
	  if (TEST_HARD_REG_BIT (fixed_reg_set, REGNO (src)))
	    continue;

	  rtx new_reg = gen_reg_rtx (GET_MODE (dest));
	  rtx_insn *new_insn = gen_move_insn (new_reg, src);
	  SET_SRC (set) = new_reg;
	  emit_insn_before (new_insn, insn);
	  df_insn_rescan (insn);
	}
    }
}

/* Try combining insns through substitution.  */
static void
rest_of_handle_combine (void)
{
  make_more_copies ();

  df_set_flags (DF_LR_RUN_DCE + DF_DEFER_INSN_RESCAN);
  df_note_add_problem ();
  df_analyze ();

  regstat_init_n_sets_and_refs ();
  reg_n_sets_max = max_reg_num ();

  bool rebuild_jump_labels_after_combine
    = combine_instructions (get_insns (), max_reg_num ());

  /* Combining insns may have turned an indirect jump into a
     direct jump.  Rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_jump_labels_after_combine)
    {
      if (dom_info_available_p (CDI_DOMINATORS))
	free_dominance_info (CDI_DOMINATORS);
      timevar_push (TV_JUMP);
      rebuild_jump_labels (get_insns ());
      cleanup_cfg (0);
      timevar_pop (TV_JUMP);
    }

  regstat_free_n_sets_and_refs ();
}

namespace {

const pass_data pass_data_combine =
{
  RTL_PASS, /* type */
  "combine", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_COMBINE, /* tv_id */
  PROP_cfglayout, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_combine : public rtl_opt_pass
{
public:
  pass_combine (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_combine, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return (optimize > 0); }
  unsigned int execute (function *) final override
    {
      rest_of_handle_combine ();
      return 0;
    }

}; // class pass_combine

} // anon namespace

rtl_opt_pass *
make_pass_combine (gcc::context *ctxt)
{
  return new pass_combine (ctxt);
}
