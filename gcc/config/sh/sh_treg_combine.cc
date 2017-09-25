/* An SH specific RTL pass that tries to combine comparisons and redundant
   condition code register stores across multiple basic blocks.
   Copyright (C) 2013-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_ALGORITHM
#define INCLUDE_LIST
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "machmode.h"
#include "predict.h"
#include "vec.h"
#include "hashtab.h"
#include "hash-set.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "basic-block.h"
#include "df.h"
#include "rtl.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "emit-rtl.h"
#include "recog.h"
#include "tree-pass.h"
#include "target.h"
#include "symtab.h"
#include "inchash.h"
#include "tree.h"
#include "optabs.h"
#include "flags.h"
#include "statistics.h"
#include "double-int.h"
#include "real.h"
#include "fixed-value.h"
#include "alias.h"
#include "wide-int.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"

/*
This pass tries to optimize for example this:
	mov.l	@(4,r4),r1
	tst	r1,r1
	movt	r1
	tst	r1,r1
	bt/s	.L5

into something simpler:
	mov.l	@(4,r4),r1
	tst	r1,r1
	bf/s	.L5

Such sequences can be identified by looking for conditional branches and
checking whether the ccreg is set before the conditional branch
by testing another register for != 0, which was set by a ccreg store.
This can be optimized by eliminating the redundant comparison and
inverting the branch condition.  There can be multiple comparisons in
different basic blocks that all end up in the redunant test insn before the
conditional branch.  Some example RTL ...

Example 1)
----------

[bb 3]
(set (reg:SI 147 t) (eq:SI (reg:SI 173) (const_int 0)))
(set (reg:SI 167) (xor:SI (reg:SI 147 t) (const_int 1)))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (eq:SI (reg:SI 177) (const_int 0)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (eq:SI (reg:SI 167) (const_int 0)))
(set (pc) (if_then_else (ne (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 50) (pc)))

In [bb 4] elimination of the comparison would require inversion of the branch
condition and compensation of other BBs.
Instead the comparison in [bb 3] can be replaced with the comparison in [bb 5]
by using a reg-reg move.  In [bb 4] a logical not is used to compensate the
inverted condition.

[bb 3]
(set (reg:SI 167) (reg:SI 173))
-> bb 5

[BB 4]
(set (reg:SI 147 t) (eq:SI (reg:SI 177) (const_int 0)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (eq:SI (reg:SI 167) (const_int 0)))
(set (pc) (if_then_else (ne (reg:SI 147 t) (const_int 0)))
                        (label_ref:SI 50) (pc)))


Example 2)
----------

[bb 3]
(set (reg:SI 147 t) (gt:SI (reg:SI 173) (reg:SI 175)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (gt:SI (reg:SI 177) (reg:SI 179)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (eq:SI (reg:SI 167) (const_int 0)))
(set (pc) (if_then_else (ne (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 51) (pc)))

The common comparison is factored out and the branch condition is inverted:

[bb 3]
(set (reg:SI 167) (reg:SI 173))
(set (reg:SI 200) (reg:SI 175))
-> bb 5

[bb 4]
(set (reg:SI 167) (reg:SI 177))
(set (reg:SI 200) (reg:SI 179))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (gt:SI (reg:SI 167) (reg:SI 200)))
(set (pc) (if_then_else (eq (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 51) (pc)))


Example 3)
----------

[bb 3]
(set (reg:SI 147 t) (gt:SI (reg:SI 173) (reg:SI 175)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (ge:SI (reg:SI 179) (reg:SI 177)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (eq:SI (reg:SI 167) (const_int 0)))
(set (pc) (if_then_else (ne (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 51) (pc)))

The T bit lifetime is extended and the branch condition is inverted:

[bb 3]
(set (reg:SI 147 t) (gt:SI (reg:SI 173) (reg:SI 175)))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (ge:SI (reg:SI 179) (reg:SI 177)))
-> bb 5

[bb 5]
(set (pc) (if_then_else (eq (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 51) (pc)))


Example 4)
----------

[bb 3]
(set (reg:SI 147 t) (eq:SI (reg:SI 173) (const_int 5)))
(set (reg:SI 167) (reg:SI 147 t))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (eq:SI (reg:SI 176) (const_int 5)))
(set (reg:SI 167) (xor:SI (reg:SI 147 t) (const_int 1)))
-> bb 5

[bb 5]
(set (reg:SI 147 t) (eq:SI (reg:SI 167) (const_int 0)))
(set (pc) (if_then_else (ne (reg:SI 147 t) (const_int 0))
                        (label_ref:SI 50) (pc)))

In this case the comparisons are the same and could be combined, but the
branch condition is different for [bb 3] and [bb 5].  Since the comparison
is not a zero comparison, we can't negate one of the operands.  The best thing
we can do here is to eliminate the comparison before the cbranch and invert
the ccreg in one of the BBs.  On SH2A this will utilize the 'nott' instruction.

[bb 3]
(set (reg:SI 147 t) (eq:SI (reg:SI 173) (const_int 5)))
-> bb 5

[bb 4]
(set (reg:SI 147 t) (eq:SI (reg:SI 176) (const_int 5)))
(set (reg:SI 147 t) (xor:SI (reg:SI 147 t) (const_int 1)))
-> bb 5

[bb 5]
(set (pc) (if_then_else (eq (reg:SI 147 t) (const_int 0))  // inverted
                        (label_ref:SI 50) (pc)))


In order to handle cases such as above the RTL pass does the following:

- Find the ccreg sets (comparisons) and ccreg stores
  (inverting and non-inverting) in all related BBs.

- If the comparison types in the BBs are all the same, try to combine the
  comparisons in the BBs and replace the zero comparison before the cbranch
  with the common comparison.

    - If the cstores are the same, move the comparison before the cbranch
      and replace the comparisons in the BBs with reg-reg copies to get the
      operands in place (create new pseudo regs).

    - If the cstores differ and the comparison is a test against zero,
      use reg-reg copies for the dominating cstores and logical not cstores
      for the subordinate cstores.

- If the comparison types in the BBs are not the same, or the first approach
  doesn't work out for some reason, try to eliminate the comparison before the
  cbranch by extending the lifetime of the ccreg by leaving the individual
  comparisons but eliminating the cstores.
  If the cstores are all the same this is straight forward.
  If they're not, try to reverse the ccreg for the subordinate cstore type
  and eliminate the dominating one.
*/

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Helper functions

#define log_msg(...)\
  do { if (dump_file != NULL) fprintf (dump_file, __VA_ARGS__); } while (0)

#define log_insn(i)\
  do { if (dump_file != NULL) print_rtl_single (dump_file, \
						(const_rtx)i); } while (0)

#define log_rtx(r)\
  do { if (dump_file != NULL) print_rtl (dump_file, (const_rtx)r); } while (0)

#define log_return(retval, ...)\
  do { if (dump_file != NULL) fprintf (dump_file, __VA_ARGS__); \
       return retval; } while (0)

#define log_return_void(...)\
  do { if (dump_file != NULL) fprintf (dump_file, __VA_ARGS__); \
       return; } while (0)

struct set_of_reg
{
  // The insn where the search stopped or NULL.
  rtx_insn *insn;

  // The set rtx of the specified reg if found, NULL_RTX otherwise.
  // Notice that the set rtx can also be in a parallel.
  const_rtx set_rtx;

  // The set source operand rtx if found, NULL_RTX otherwise.
  rtx
  set_src (void) const
  {
    return set_rtx == NULL_RTX ? NULL_RTX : XEXP (set_rtx, 1);
  }

  // The set destination operand rtx if found, NULL_RTX otherwise.
  rtx
  set_dst (void) const
  {
    return set_rtx == NULL_RTX ? NULL_RTX : XEXP (set_rtx, 0);
  }

  bool
  empty (void) const
  {
    return insn == NULL_RTX || set_rtx == NULL_RTX;
  }
};

// Given a reg rtx and a start insn find the insn (in the same basic block)
// that sets the reg.
static set_of_reg
find_set_of_reg_bb (rtx reg, rtx_insn *insn)
{
  set_of_reg result = { insn, NULL_RTX };

  if (!REG_P (reg) || insn == NULL)
    return result;

  for (result.insn = insn; result.insn != NULL;
       result.insn = prev_nonnote_insn_bb (result.insn))
    {
      if (BARRIER_P (result.insn))
	return result;
      if (!NONJUMP_INSN_P (result.insn))
	continue;
      if (reg_set_p (reg, result.insn))
	{
	  result.set_rtx = set_of (reg, result.insn);
	  if (result.set_rtx == NULL_RTX || GET_CODE (result.set_rtx) != SET)
	    result.set_rtx = NULL_RTX;
	  return result;
	}
    }

  return result;
}

static bool
reg_dead_after_insn (const_rtx reg, const_rtx insn)
{
  return find_regno_note (insn, REG_DEAD, REGNO (reg)) != NULL_RTX;
}

static bool
reg_unused_after_insn (const_rtx reg, const_rtx insn)
{
  return find_regno_note (insn, REG_UNUSED, REGNO (reg)) != NULL_RTX;
}

// Check whether the two specified basic blocks are adjacent, i.e. there's no
// other basic block in between them.
static bool
is_adjacent_bb (basic_block a, basic_block b)
{
  basic_block bb0[] = { a, b };
  basic_block bb1[] = { b, a };

  for (int i = 0; i < 2; ++i)
    for (edge_iterator ei = ei_start (bb0[i]->succs);
	 !ei_end_p (ei); ei_next (&ei))
      if (ei_edge (ei)->dest == bb1[i])
	return true;

  return false;
}

// Internal function of trace_reg_uses.
static void
trace_reg_uses_1 (rtx reg, rtx_insn *start_insn, basic_block bb, int& count,
		  std::vector<basic_block>& visited_bb, rtx abort_at_insn)
{
  if (bb == NULL)
    return;

  if (std::find (visited_bb.begin (), visited_bb.end (), bb)
      != visited_bb.end ())
    log_return_void ("[bb %d] already visited\n", bb->index);

  visited_bb.push_back (bb);

  if (BB_END (bb) == NULL_RTX)
    log_return_void ("[bb %d] BB_END is null\n", bb->index);

  if (start_insn == NULL_RTX)
    log_return_void ("[bb %d] start_insn is null\n", bb->index);

  rtx end_insn = NEXT_INSN (BB_END (bb));
  if (end_insn == NULL_RTX)
    log_return_void ("[bb %d] end_insn is null\n", bb->index);

  for (rtx_insn *i = NEXT_INSN (start_insn); i != end_insn; i = NEXT_INSN (i))
    {
      if (INSN_P (i))
	{
	  if (NONDEBUG_INSN_P (i)
	      && (reg_overlap_mentioned_p (reg, PATTERN (i))
		  || (CALL_P (i) && find_reg_fusage (i, USE, reg))))
	    {
	      log_msg ("found use in [bb %d] at insn:\n", bb->index);
	      log_insn (i);
	      log_msg ("\n");
	      count += 1;
	    }

	  // Stop following this BB if the reg is set or dies along the way.
	  if (reg_set_p (reg, i) || reg_dead_after_insn (reg, i))
	    return;
	}

      if (abort_at_insn != NULL_RTX && abort_at_insn == i)
	return;
    }

  for (edge_iterator ei = ei_start (bb->succs); !ei_end_p (ei); ei_next (&ei))
    {
      basic_block succ_bb = ei_edge (ei)->dest;
      trace_reg_uses_1 (reg, BB_HEAD (succ_bb), succ_bb, count, visited_bb,
			abort_at_insn);
    }
}

// Trace uses of the specified reg in all basic blocks that are reachable from
// the specified insn.  If 'abort_at_insn' is not null, abort the trace at
// that insn.  If the insn 'abort_at_insn' uses the specified reg, it is also
// counted.
static int
trace_reg_uses (rtx reg, rtx_insn *start_insn, rtx abort_at_insn)
{
  log_msg ("\ntrace_reg_uses\nreg = ");
  log_rtx (reg);
  log_msg ("\nstart_insn = ");
  log_insn (start_insn);

  int count = 0;
  std::vector<basic_block> visited_bb;
  visited_bb.reserve (32);

  trace_reg_uses_1 (reg, start_insn, BLOCK_FOR_INSN (start_insn),
		    count, visited_bb, abort_at_insn);
  return count;
}

static bool
is_conditional_insn (rtx_insn* i)
{
  if (! (INSN_P (i) && NONDEBUG_INSN_P (i)))
    return false;

  rtx p = PATTERN (i);
  return GET_CODE (p) == SET && GET_CODE (XEXP (p, 1)) == IF_THEN_ELSE;
}

// FIXME: Remove dependency on SH predicate function somehow.
extern int t_reg_operand (rtx, machine_mode);
extern int negt_reg_operand (rtx, machine_mode);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// RTL pass class

class sh_treg_combine : public rtl_opt_pass
{
public:
  sh_treg_combine (gcc::context* ctx, bool split_insns, const char* name);
  virtual ~sh_treg_combine (void);
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

private:
  // Type of ccreg store that is supported.
  enum cstore_type_t
  {
    cstore_normal = 0,
    cstore_inverted = 1,
    cstore_unknown = -1
  };

  // Type of branch condition that is supported.
  enum branch_condition_type_t
  {
    branch_if_true = 1,
    branch_if_false = 0,
    unknown_branch_condition = -1
  };

  // For each basic block there can be a trace entry which consists of an
  // insn that sets the ccreg (usually a comparison) and a ccreg store.
  struct bb_entry
  {
    basic_block bb;
    set_of_reg setcc;
    set_of_reg cstore;
    cstore_type_t cstore_type;
    std::vector<set_of_reg> cstore_reg_reg_copies;

    bb_entry (basic_block b)
    : bb (b), setcc (), cstore (), cstore_type (cstore_unknown) { }

    rtx comparison_rtx (void) const { return setcc.set_src (); }
  };

  // A ccreg trace for a conditional branch.
  struct cbranch_trace
  {
    rtx_insn *cbranch_insn;
    rtx* condition_rtx_in_insn;
    branch_condition_type_t cbranch_type;

    // The comparison against zero right before the conditional branch.
    set_of_reg setcc;

    // All BBs that are related to the cbranch.  The last BB in the list is
    // the BB of the cbranch itself and might be empty.
    std::list<bb_entry> bb_entries;

    cbranch_trace (rtx_insn *insn)
    : cbranch_insn (insn),
      condition_rtx_in_insn (NULL),
      cbranch_type (unknown_branch_condition),
      setcc ()
    {
      if (is_conditional_insn (cbranch_insn))
	condition_rtx_in_insn = &XEXP (XEXP (PATTERN (cbranch_insn), 1), 0);
      else if (rtx x = pc_set (cbranch_insn))
	condition_rtx_in_insn = &XEXP (XEXP (x, 1), 0);
    }

    basic_block bb (void) const { return BLOCK_FOR_INSN (cbranch_insn); }

    rtx
    branch_condition_rtx (void) const
    {
      return condition_rtx_in_insn != NULL ? *condition_rtx_in_insn : NULL;
    }
    rtx&
    branch_condition_rtx_ref (void) const
    {
      // Before anything gets to invoke this function, there are other checks
      // in place to make sure that we have a known branch condition and thus
      // the ref to the rtx in the insn.
      gcc_assert (condition_rtx_in_insn != NULL);
      return *condition_rtx_in_insn;
    }

    bool
    can_invert_condition (void) const
    {
      // The branch condition can be inverted safely only if the condition
      // reg is dead after the cbranch.
      return reg_dead_after_insn (XEXP (branch_condition_rtx (), 0),
				  cbranch_insn);
    }
  };

  static const pass_data default_pass_data;

  // Tells whether modified or newly added insns are to be split at the end
  // of the pass.
  const bool m_split_insns;

  // rtx of the ccreg that is obtained from the target.
  rtx m_ccreg;

  // Newly added or modified insns.
  std::vector<rtx> m_touched_insns;

  // Given an rtx determine whether it's a comparison with a constant zero.
  static bool is_cmp_eq_zero (const_rtx i);

  // Update the stored mode of the ccreg from the given branch condition rtx.
  void update_ccreg_mode (const_rtx cond);

  // Given an rtx, figure out the branch condition, assuming that it is
  // in canonical form:
  //   (ne (reg) (const_int 0))
  //   (eq (reg) (const_int 0))
  branch_condition_type_t branch_condition_type (const_rtx cond) const;

  // Return true if the specified rtx is either a normal ccreg or
  // a negated form of the ccreg.
  bool is_normal_ccreg (const_rtx x) const;
  bool is_inverted_ccreg (const_rtx x) const;

  // Given a reg rtx and a start insn rtx, try to find the insn in the same
  // basic block that sets the specified reg.
  // Return how the search ended and the insn where it stopped or NULL_RTX.
  enum record_return_t
  {
    set_found,
    set_not_found,
    other_set_found
  };
  record_return_t record_set_of_reg (rtx reg, rtx_insn *start_insn,
                                     bb_entry& e);

  // Tells whether the cbranch insn of the specified bb_entry can be removed
  // safely without triggering any side effects.
  bool can_remove_cstore (const bb_entry& e,
			  const cbranch_trace& trace) const;

  // Tells whether the setcc insn of the specified bb_entry can be removed
  // safely without triggering any side effects.
  bool can_remove_comparison (const bb_entry& e,
			      const cbranch_trace& trace) const;

  // Tells whether the two specified comparison rtx can be combined into a
  // single comparison.
  bool can_combine_comparisons (const_rtx x, const_rtx y) const;

  // Tells whether the ccreg usage can be extended from the bb_entry on until
  // the final cbranch of the trace.
  bool can_extend_ccreg_usage (const bb_entry& e,
			       const cbranch_trace& trace) const;

  // Create an insn rtx that performs a logical not (test != 0) on the src_reg
  // and stores the result in dst_reg.
  rtx make_not_reg_insn (rtx dst_reg, rtx src_reg) const;

  // Create an insn rtx that inverts the ccreg.
  rtx_insn *make_inv_ccreg_insn (void) const;

  // Adds the specified insn to the set of modified or newly added insns that
  // might need splitting at the end of the pass.
  rtx touched_insn (rtx i);

  // Try to invert the branch condition of the specified trace.
  bool try_invert_branch_condition (cbranch_trace& trace);

  // Try to optimize a cbranch trace by combining comparisons in BBs and
  // eliminate the cstores.
  bool try_combine_comparisons (cbranch_trace& trace,
				int cstore_count, int inv_cstore_count,
				cstore_type_t dominating_cstore);

  // Try to optimize a cbranch trace by eliminating the cstores in BBs only.
  bool try_eliminate_cstores (cbranch_trace& trace,
			      int cstore_count, int inv_cstore_count,
			      cstore_type_t dominating_cstore);

  // Given a branch insn, try to optimize its branch condition.
  // If any insns are modified or added they are added to 'm_touched_insns'.
  void try_optimize_cbranch (rtx_insn *i);
};


const pass_data sh_treg_combine::default_pass_data =
{
  RTL_PASS,		// type
  "",			// name (overwritten by the constructor)
  OPTGROUP_NONE,	// optinfo_flags
  TV_OPTIMIZE,		// tv_id
  0,			// properties_required
  0,			// properties_provided
  0,			// properties_destroyed
  0,			// todo_flags_start
  TODO_df_finish | TODO_df_verify	// todo_flags_finish
};

sh_treg_combine::sh_treg_combine (gcc::context* ctx, bool split_insns,
				  const char* name)
: rtl_opt_pass (default_pass_data, ctx),
  m_split_insns (split_insns),
  m_ccreg (NULL_RTX)
{
  // Overwrite default name in pass_data base class. 
  this->name = name;
}

sh_treg_combine::~sh_treg_combine (void)
{
}

void sh_treg_combine::update_ccreg_mode (const_rtx cond)
{
  if (REG_P (XEXP (cond, 0)) && REGNO (XEXP (cond, 0)) != REGNO (m_ccreg))
    return;

  machine_mode m = GET_MODE (XEXP (cond, 0));
  if (m == GET_MODE (m_ccreg))
    return;

  PUT_MODE (m_ccreg, m);
  log_msg ("updated ccreg mode: ");
  log_rtx (m_ccreg);
  log_msg ("\n");
}

bool
sh_treg_combine::is_cmp_eq_zero (const_rtx i)
{
  return i != NULL_RTX && GET_CODE (i) == EQ
	 && REG_P (XEXP (i, 0)) && XEXP (i, 1) == const0_rtx;
}

sh_treg_combine::branch_condition_type_t
sh_treg_combine::branch_condition_type (const_rtx cond) const
{
  if (cond == NULL_RTX)
    return unknown_branch_condition;

  if (GET_CODE (cond) == NE
      && REG_P (XEXP (cond, 0)) && REGNO (XEXP (cond, 0)) == REGNO (m_ccreg)
      && XEXP (cond, 1) == const0_rtx)
    return branch_if_true;

  else if (GET_CODE (cond) == EQ
      && REG_P (XEXP (cond, 0)) && REGNO (XEXP (cond, 0)) == REGNO (m_ccreg)
      && XEXP (cond, 1) == const0_rtx)
    return branch_if_false;

  else
    return unknown_branch_condition;
}

bool
sh_treg_combine::is_normal_ccreg (const_rtx x) const
{
  return t_reg_operand (const_cast<rtx> (x), VOIDmode);
}

bool
sh_treg_combine::is_inverted_ccreg (const_rtx x) const
{
  return negt_reg_operand (const_cast<rtx> (x), VOIDmode);
}

sh_treg_combine::record_return_t
sh_treg_combine::record_set_of_reg (rtx reg, rtx_insn *start_insn,
				    bb_entry& new_entry)
{
  log_msg ("\n[bb %d]\n", new_entry.bb->index);

  if (start_insn == NULL_RTX)
    log_return (set_not_found, "set of reg not found.  empty BB?\n");

  new_entry.cstore_type = cstore_unknown;

  for (rtx_insn *i = start_insn; i != NULL; )
    {
      new_entry.cstore = find_set_of_reg_bb (reg, i);

      if (new_entry.cstore.set_src () == NULL_RTX)
	log_return (set_not_found, "set of reg not found (cstore)\n");

      log_insn (new_entry.cstore.insn);
      log_msg ("\n");

      if (is_normal_ccreg (new_entry.cstore.set_src ()))
	{
	  log_msg ("normal condition store\n");
	  new_entry.cstore_type = cstore_normal;
	}
      else if (is_inverted_ccreg (new_entry.cstore.set_src ()))
	{
	  log_msg ("inverted condition store\n");
	  new_entry.cstore_type = cstore_inverted;
	}
      else if (REG_P (new_entry.cstore.set_src ()))
	{
	  // If it's a reg-reg copy follow the copied reg.
	  new_entry.cstore_reg_reg_copies.push_back (new_entry.cstore);
	  reg = new_entry.cstore.set_src ();
	  i = new_entry.cstore.insn;

	  log_msg ("reg-reg copy.  tracing ");
	  log_rtx (reg);
	  log_msg ("\n");
	  continue;
	}
      else
	log_return (other_set_found, "not a condition store\n");

      gcc_assert (new_entry.cstore_type != cstore_unknown);

      // Now see how the ccreg was set.
      // For now it must be in the same BB.
      log_msg ("tracing ccreg\n");
      new_entry.setcc =
	  find_set_of_reg_bb (m_ccreg,
			      prev_nonnote_insn_bb (new_entry.cstore.insn));

      // If cstore was found but setcc was not found continue anyway, as
      // for some of the optimization types the setcc is irrelevant.
      if (new_entry.setcc.set_src () == NULL_RTX)
	log_return (set_found, "set of ccreg not found\n");

      else if (GET_CODE (new_entry.setcc.set_rtx) == SET)
	{
	  // Also allow insns that set the ccreg, but are not true comparison
	  // insns, as long as they are sets and not e.g. clobbers.
	  log_insn (new_entry.setcc.insn);
	  log_msg ("\n");
	  return set_found;
	}
      else
	// If cstore was found but setcc was not found continue anyway, as
	// for some of the optimization types the setcc is irrelevant.
 	log_return (set_found, "unknown set of ccreg\n");
    }

  log_return (set_not_found, "set of reg not found\n");
}

bool
sh_treg_combine::can_remove_cstore (const bb_entry& e,
				    const cbranch_trace& trace) const
{
  if (volatile_insn_p (PATTERN (e.cstore.insn)))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause it's volatile\n");
    }

  // On SH there are parallel patterns which store the ccreg multiple times.
  // In this case it's not safe.
  rtx cstore_pat = PATTERN (e.cstore.insn);
  if (GET_CODE (cstore_pat) == PARALLEL)
    for (int i = 0; i < XVECLEN (cstore_pat, 0); ++i)
      {
	rtx x = XVECEXP (cstore_pat, 0, i);

	// It's the cstore set that we're referring to, ignore that one.
	if (x != e.cstore.set_rtx
	    && GET_CODE (x) == SET && reg_referenced_p (m_ccreg, x))
	  {
	    log_msg ("can't remove insn\n");
	    log_insn (e.cstore.insn);
	    log_return (false, "\nbecause it's a multiple ccreg store\n");
	  }
      }

  // If the cstore sets the ccreg (e.g. negc) and the ccreg is used afterwards
  // it's not safe.
  if (modified_in_p (m_ccreg, e.cstore.insn)
      && !(reg_dead_after_insn (m_ccreg, e.cstore.insn)
	   || reg_unused_after_insn (m_ccreg, e.cstore.insn)))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause it sets the ccreg\n");
    }

  // If the cstore destination reg is copied around check the reg-reg
  // copies.  At every reg-reg copy the copied reg must be dead and there
  // must not be a usage of the copied regs between the reg-reg copies.
  // Otherwise we assume that the result of the cstore is used in some
  // other way.
  rtx_insn *prev_insn = e.cstore.insn;
  for (std::vector<set_of_reg>::const_reverse_iterator i =
	   e.cstore_reg_reg_copies.rbegin ();
       i != e.cstore_reg_reg_copies.rend (); ++i)
    {
      if (!reg_dead_after_insn (i->set_src (), i->insn))
	{
	  log_msg ("can't remove insn\n");
	  log_insn (i->insn);
	  log_return (false, "\nbecause source of reg-reg copy doesn't die\n");
	}

     if (reg_used_between_p (i->set_src (), prev_insn, i->insn))
	{
	  log_msg ("can't remove insn\n");
	  log_insn (i->insn);
	  log_return (false, "\nbecause reg %d is otherwise used\n",
			     REGNO (i->set_src ()));
	}

      prev_insn = i->insn;
    }

  // The cstore_dst reg must die after the test before the cbranch, otherwise
  // it's not safe to remove the cstore.
  // If the cstore destination reg is copied around check the effective
  // destination reg of the cstore.  The reg-reg copies are recorded in
  // reverse order, i.e. the most recent reg-reg copy in the insn list
  // comes first.
  rtx cstore_dst = e.cstore_reg_reg_copies.empty ()
		   ? e.cstore.set_dst ()
		   : e.cstore_reg_reg_copies.front ().set_dst ();

  if (!reg_dead_after_insn (cstore_dst, trace.setcc.insn))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause its effective target reg %d doesn't die "
			 "after trace.setcc.insn\n", REGNO (cstore_dst));
    }

  // Also check that the cstore_dst reg is not used in other reachable code
  // paths before it dies.
  // Count the uses of the effective cstore_dst reg (i.e. the last known reg
  // that holds the cstore value after reg-reg copies) in all BBs that can be
  // reached from bb_entry's BB including the BB of the cstore insn.
  // If we get more than 1 uses we assume that it's used somewhere else and is
  // not safe to be removed.
  int cstore_dst_use_count = trace_reg_uses (cstore_dst, e.cstore.insn,
					     trace.setcc.insn);
  if (cstore_dst_use_count > 1)
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause its effective target reg %d is used "
			 "in %d other places\n", REGNO (cstore_dst),
			  cstore_dst_use_count - 1);
    }

  return true;
}

bool
sh_treg_combine::can_remove_comparison (const bb_entry& e,
					const cbranch_trace&/* trace*/) const
{
  // If the ccreg is used otherwise between the comparison and the cstore,
  // it's not safe.
  if (reg_used_between_p (m_ccreg, e.setcc.insn, e.cstore.insn))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.setcc.insn);
      log_return (false, "\nbecause the ccreg is used otherwise\n");
    }

  if (!reg_dead_after_insn (m_ccreg, e.cstore.insn)
      && !reg_unused_after_insn (m_ccreg, e.cstore.insn))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause ccreg is not dead or unused afterwards\n");
    }

  // On SH there are also multiple set patterns that can be used for
  // comparisons, such as "shll".  It's not safe to remove those.
  if (multiple_sets (e.setcc.insn))
    {
      log_msg ("can't remove insn\n");
      log_insn (e.cstore.insn);
      log_return (false, "\nbecause it's a multiple set\n");
    }

  return true;
}

rtx
sh_treg_combine::make_not_reg_insn (rtx dst_reg, rtx src_reg) const
{
  // On SH we can do only SImode and DImode comparisons.
  if (! (GET_MODE (src_reg) == SImode || GET_MODE (src_reg) == DImode))
    return NULL;

  // On SH we can store the ccreg into an SImode or DImode reg only.
  if (! (GET_MODE (dst_reg) == SImode || GET_MODE (dst_reg) == DImode))
    return NULL;

  start_sequence ();

  emit_insn (gen_rtx_SET (VOIDmode, m_ccreg,
			  gen_rtx_fmt_ee (EQ, SImode, src_reg, const0_rtx)));

  if (GET_MODE (dst_reg) == SImode)
    emit_move_insn (dst_reg, m_ccreg);
  else if (GET_MODE (dst_reg) == DImode)
    {
      emit_move_insn (gen_lowpart (SImode, dst_reg), m_ccreg);
      emit_move_insn (gen_highpart (SImode, dst_reg), const0_rtx);
    }
  else
    gcc_unreachable ();

  rtx i = get_insns ();
  end_sequence ();

  return i;
}

rtx_insn *
sh_treg_combine::make_inv_ccreg_insn (void) const
{
  start_sequence ();
  rtx_insn *i = emit_insn (gen_rtx_SET (VOIDmode, m_ccreg,
                                        gen_rtx_fmt_ee (XOR, GET_MODE (m_ccreg),
                                                        m_ccreg, const1_rtx)));
  end_sequence ();
  return i;
}

rtx
sh_treg_combine::touched_insn (rtx i)
{
  m_touched_insns.push_back (i);
  return i;
}

bool
sh_treg_combine::can_combine_comparisons (const_rtx x, const_rtx y) const
{
  if (GET_CODE (x) != GET_CODE (y))
    return false;

  rtx x_op0 = XEXP (x, 0);
  rtx x_op1 = XEXP (x, 1);

  rtx y_op0 = XEXP (y, 0);
  rtx y_op1 = XEXP (y, 1);

  if (!REG_P (x_op0) || !REG_P (y_op0))
    return false;

  if (GET_MODE (x_op0) != GET_MODE (y_op0))
    return false;

  // rtx_equal_p also compares the reg numbers which we do not care about
  // here, as long as both are regs and the modes are the same.
  if (REG_P (x_op1))
    return REG_P (y_op1) && GET_MODE (x_op1) == GET_MODE (y_op1);

  return rtx_equal_p (x_op1, y_op1);
}

bool
sh_treg_combine::can_extend_ccreg_usage (const bb_entry& e,
					 const cbranch_trace& trace) const
{
  // Check if the ccreg is not modified by other insins in the BB path until
  // the final cbranch of the trace.
  // Start checking after the cstore that follows the setcc, assuming that
  // the cstore will be removed.

  // The assumption here is that the specified bb_entry's BB is a direct
  // predecessor of the trace.cbranch_insn's BB.
  if (e.bb != trace.bb () && !is_adjacent_bb (e.bb, trace.bb ()))
    log_return (false,
	"can't extend ccreg usage -- [bb %d] and [bb %d] are not adjacent\n",
	e.bb->index, trace.bb ()->index);

  if (e.cstore.empty ())
    log_return (false, "can't extend ccreg usage -- no cstore\n");

  // The entry's cstore is in the same BB as the final cbranch.
  if (e.bb == trace.bb ())
    {
      if (reg_set_between_p (m_ccreg, e.cstore.insn, trace.setcc.insn))
	log_return (false,
	    "can't extend ccreg usage -- it's modified between e.cstore.insn "
	    "and trace.setcc.insn");
      else
	return true;
    }

  // The entry's cstore and the final cbranch are in different BBs.
  if (reg_set_between_p (m_ccreg, e.cstore.insn, NEXT_INSN (BB_END (e.bb))))
    log_return (false,
	"can't extend ccreg usage -- it's modified in [bb %d]", e.bb->index);

  if (reg_set_between_p (m_ccreg, PREV_INSN (BB_HEAD (trace.bb ())),
			 trace.setcc.insn))
    log_return (false,
	"can't extend ccreg usage -- it's modified in [bb %d]",
	trace.bb ()->index);

  return true;
}

bool
sh_treg_combine::try_invert_branch_condition (cbranch_trace& trace)
{
  log_msg ("inverting branch condition\n");

  rtx& comp = trace.branch_condition_rtx_ref ();

  rtx_code rev_cmp_code = reversed_comparison_code (comp, trace.cbranch_insn);

  if (rev_cmp_code == UNKNOWN)
    log_return (false, "reversed_comparison_code = UNKNOWN\n");

  validate_change (trace.cbranch_insn, &comp,
		   gen_rtx_fmt_ee (rev_cmp_code,
				   GET_MODE (comp), XEXP (comp, 0),
				   XEXP (comp, 1)),
		   1);

  if (verify_changes (num_validated_changes ()))
    confirm_change_group ();
  else
    log_return (false, "verify_changed failed\n");

  touched_insn (trace.cbranch_insn);
  return true;
}

bool
sh_treg_combine::try_combine_comparisons (cbranch_trace& trace,
					  int cstore_count,
					  int inv_cstore_count,
					  cstore_type_t dominating_cstore)
{
  log_msg ("\ntry_combine_comparisons\n");

  // This function will always try to create new pseudos.
  if (!can_create_pseudo_p ())
    log_return (false, "can't create pseudos\n");

  // Check that all ccset insns are comparisons and all comparison types in
  // all BBs are the same and could be combined into one single comparison.
  rtx comp = NULL_RTX;
  rtx comp_insn = NULL_RTX;

  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    {
      int i_empty_count = i->setcc.empty () + i->cstore.empty ();

      // A completly empty entry is OK (could be the BB of the cbranch).
      if (i_empty_count == 2)
	continue;

      // Otherwise we need both, the setcc and the cstore.
      if (i_empty_count != 0)
	log_return (false, "bb entry is not a setcc cstore pair\n");

      rtx other_comp = i->comparison_rtx ();

      if (!COMPARISON_P (other_comp))
	{
	  log_msg ("setcc is not a comparison:\n");
	  log_rtx (other_comp);
	  log_return (false, "\n");
	}

      if (comp_insn == NULL_RTX)
	{
	  comp = other_comp;
	  comp_insn = i->setcc.insn;
	}
      else if (!can_combine_comparisons (comp, other_comp))
	return false;

      // The goal here is to eliminate all cstores and comparisons in the BBs.
      // Thus check if every cstore can actually be removed safely.
      if (!can_remove_cstore (*i, trace) || !can_remove_comparison (*i, trace))
	return false;
    }

  // FIXME: The first operand of the comparison must be a simple reg.
  // This effectively prohibits combining div0s comparisons such as
  //    (lt:SI (xor:SI (reg:SI) (reg:SI)))
  if (!REG_P (XEXP (comp, 0)))
    {
      log_msg ("comparison operand 0\n");
      log_rtx (XEXP (comp, 0));
      log_return (false, "\nis not a reg\n");
    }

  rtx comp_op0 = gen_reg_rtx (GET_MODE (XEXP (comp, 0)));
  rtx comp_op1 = REG_P (XEXP (comp, 1))
		 ? gen_reg_rtx (GET_MODE (XEXP (comp, 1)))
		 : XEXP (comp, 1);

  // If there are both, inverting and non-inverting cstores, they can only
  // be eliminated if the comparison can be inverted.  We assume that the
  // comparison insns that we find are already minimal and canonicalized.
  // There is one special case though, where an integer comparison
  //     (eq (reg) (const_int 0))
  // can be inverted with a sequence
  //     (set (t) (eq (reg) (const_int 0))
  //     (set (reg) (t))
  //     (eq (reg) (const_int 0))
  //
  // FIXME: On SH2A it might be better to use the nott insn in this case,
  // i.e. do the try_eliminate_cstores approach instead.
  if (inv_cstore_count != 0 && cstore_count != 0)
    {
      if (make_not_reg_insn (comp_op0, comp_op0) == NULL_RTX)
	log_return (false, "make_not_reg_insn failed.\n");

      for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
	   i != trace.bb_entries.end (); ++i)
	{
	  if (i->setcc.empty () || i->cstore.empty ())
	    continue;

	  if (i->cstore_type != dominating_cstore
	      && !is_cmp_eq_zero (i->comparison_rtx ()))
	    {
	      log_msg ("can't invert comparison in insn\n");
	      log_insn (i->setcc.insn);
	      log_return (false,
		"\nbecause it's not a (eq (reg) (const_int 0))\n");
	    }
	}
    }

  if (dominating_cstore == cstore_normal
      && !try_invert_branch_condition (trace))
    return false;

  // Replace the test insn before the cbranch with the common comparison.
  // Instead of creating a new insn from scratch we copy the common comparison
  // pattern.  This simplifies handling parallel comparison patterns, such as
  // FP comparisons on SH, which have an extra use on FPSCR.
  log_msg ("installing common comparison in [bb %d]\n", trace.bb ()->index);

  rtx common_comp_pat = copy_rtx (PATTERN (comp_insn));
  rtx common_comp = const_cast<rtx> (set_of (m_ccreg, common_comp_pat));

  gcc_assert (common_comp != NULL_RTX);

  XEXP (XEXP (common_comp, 1), 0) = comp_op0;
  XEXP (XEXP (common_comp, 1), 1) = comp_op1;

  log_rtx (common_comp_pat);
  log_msg ("\n");

  rtx common_comp_insn = touched_insn (emit_insn_after (common_comp_pat,
							trace.setcc.insn));

  if (REG_P (comp_op0))
    add_reg_note (common_comp_insn, REG_DEAD, copy_rtx (comp_op0));
  if (REG_P (comp_op1))
    add_reg_note (common_comp_insn, REG_DEAD, copy_rtx (comp_op1));

  delete_insn (trace.setcc.insn);

  // Replace comparison and cstore insns with reg-reg moves in all BBs.
  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    {
      if (i->setcc.empty () || i->cstore.empty ())
	continue;

      rtx i_comp_op0 = XEXP (i->comparison_rtx (), 0);
      rtx i_comp_op1 = XEXP (i->comparison_rtx (), 1);

      if (i->cstore_type == dominating_cstore)
	{
	  log_msg ("replacing comparison and cstore with reg move "
		   "in [bb %d]\n", i->bb->index);

	  rtx new_i = touched_insn (
		emit_insn_after (gen_move_insn (comp_op0, i_comp_op0),
				 i->setcc.insn));

	  if (REG_P (i_comp_op0)
	      && reg_dead_after_insn (i_comp_op0, i->setcc.insn))
	    add_reg_note (new_i, REG_DEAD, copy_rtx (i_comp_op0));

	  // If the second operand is a reg, have to emit a move insn.
	  // Otherwise assume it's a const_int and just reference it.
	  if (REG_P (comp_op1))
	    {
	      new_i = touched_insn (
		  emit_insn_after (gen_move_insn (comp_op1, i_comp_op1),
				   i->setcc.insn));

	      if (reg_dead_after_insn (i_comp_op1, i->setcc.insn))
		add_reg_note (new_i, REG_DEAD, copy_rtx (i_comp_op1));
	    }
	}
      else
	{
	  log_msg ("replacing comparison and cstore with inverting reg move "
		   "in [bb %d]\n", i->bb->index);

	  rtx new_i = make_not_reg_insn (comp_op0, i_comp_op0);
	  if (REG_P (i_comp_op0)
	      && reg_dead_after_insn (i_comp_op0, i->setcc.insn))
	    add_reg_note (new_i, REG_DEAD, copy_rtx (i_comp_op0));

	  touched_insn (emit_insn_after (new_i, i->setcc.insn));
	}

      delete_insn (i->cstore.insn);
      delete_insn (i->setcc.insn);
    }

  return true;
}

bool
sh_treg_combine::try_eliminate_cstores (cbranch_trace& trace,
					int cstore_count, int inv_cstore_count,
					cstore_type_t dominating_cstore)
{
  log_msg ("\ntry_eliminate_cstores\n");

  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    {
      // A completly empty entry is OK (could be the BB of the cbranch).
      if (i->setcc.empty () && i->cstore.empty ())
	continue;

      // We're going to eliminate cstores, but for that they have to be
      // there.  We don't care about the setcc in this case.
      if (i->cstore.empty ())
	log_return (false, "bb entry cstore empty -- aborting\n");

      // The goal here is to eliminate all cstores in the BBs and extend the
      // ccreg usage.
      if (!can_extend_ccreg_usage (*i, trace))
	return false;

      // If the cstore can't be removed we can keep it around as long as
      // it doesn't modify the ccreg.
      if (!can_remove_cstore (*i, trace)
	  && modified_in_p (m_ccreg, i->cstore.insn))
	log_return (false, "cstore sets ccreg -- aborting\n");
    }

  // If there are both, inverting and non-inverting cstores, we'll have to
  // invert the ccreg as a replacement for one of them.
  if (cstore_count != 0 && inv_cstore_count != 0)
    {
      rtx_insn *i = make_inv_ccreg_insn ();
      if (recog_memoized (i) < 0)
	{
	  log_msg ("failed to match ccreg inversion insn:\n");
	  log_rtx (PATTERN (i));
	  log_return (false, "\naborting\n");
	}
    }

  if (dominating_cstore == cstore_normal
      && !try_invert_branch_condition (trace))
    return false;

  // Eliminate cstores in all BBs.
  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    {
      if (i->cstore.empty ())
	continue;

      if (i->cstore_type == dominating_cstore)
	log_msg ("removing cstore in [bb %d]\n", i->bb->index);
      else
	{
	  log_msg ("replacing cstore with ccreg inversion in [bb %d]\n",
		   i->bb->index);

	  touched_insn (
	    emit_insn_after (make_inv_ccreg_insn (), i->cstore.insn));
	}

      if (can_remove_cstore (*i, trace))
	delete_insn (i->cstore.insn);
    }

  log_msg ("removing test insn before cbranch\n");
  delete_insn (trace.setcc.insn);
  return true;
}

void
sh_treg_combine::try_optimize_cbranch (rtx_insn *insn)
{
  cbranch_trace trace (insn);

  log_msg ("\n\n--------------------------------------\n");
  log_msg ("found cbranch insn in [bb %d]:\n", trace.bb ()->index);
  log_insn (insn);

  trace.cbranch_type = branch_condition_type (trace.branch_condition_rtx ());

  if (trace.cbranch_type == branch_if_true)
    log_msg ("condition: branch if true\n");
  else if (trace.cbranch_type == branch_if_false)
    log_msg ("condition: branch if false\n");
  else
    {
      log_msg ("unknown branch condition\n");
      log_rtx (trace.branch_condition_rtx ());
      log_return_void ("\n");
    }

  update_ccreg_mode (trace.branch_condition_rtx ());

  // Scan the insns backwards for an insn that sets the ccreg by testing a
  // reg against zero like
  //   (set (reg ccreg) (eq (reg) (const_int 0)))
  // The testing insn could also be outside of the current basic block, but
  // for now we limit the search to the current basic block.
  trace.setcc = find_set_of_reg_bb (m_ccreg, prev_nonnote_insn_bb (insn));

  if (trace.setcc.set_src () == NULL_RTX)
    log_return_void ("could not find set of ccreg in current BB\n");

  if (!is_cmp_eq_zero (trace.setcc.set_src ())
      && !is_inverted_ccreg (trace.setcc.set_src ()))
    {
      log_msg ("unsupported set of ccreg in current BB: ");
      log_rtx (trace.setcc.set_src ());
      log_return_void ("\n");
    }

  rtx trace_reg = XEXP (trace.setcc.set_src (), 0);

  log_msg ("set of ccreg:\n");
  log_insn (trace.setcc.insn);

  // See if we can remove the trace.setcc insn safely.
  if (reg_used_between_p (m_ccreg, trace.setcc.insn, trace.cbranch_insn))
    log_return_void ("ccreg used between testing insn and branch insn\n");

  if (volatile_insn_p (PATTERN (trace.setcc.insn)))
    {
      log_msg ("can't remove insn\n");
      log_insn (trace.setcc.insn);
      log_return_void ("\nbecause it's volatile\n");
    }

  // If the ccreg is inverted before cbranch try inverting the branch
  // condition.
  if (is_inverted_ccreg (trace.setcc.set_src ()))
    {
      if (!trace.can_invert_condition ())
	log_return_void ("branch condition can't be inverted - aborting\n");

      if (try_invert_branch_condition (trace))
	delete_insn (trace.setcc.insn);

      return;
    }

  // Now that we have an insn which tests some reg and sets the condition
  // reg before the conditional branch, try to figure out how that tested
  // reg was formed, i.e. find all the insns that set the tested reg in
  // some way.
  // The tested reg might be set in multiple basic blocks so we need to
  // check all basic blocks which can reach this current basic block.
  // If the set of reg is an inverting or non-inverting store of the condition
  // register, check how the ccreg value was obtained.
  log_msg ("\ntracing ");
  log_rtx (trace_reg);
  log_msg ("\n");


  // First check the basic block where the conditional branch is in.
  // If we find it here there's no point in checking other BBs.
  trace.bb_entries.push_front (bb_entry (trace.bb ()));

  record_return_t res =
      record_set_of_reg (trace_reg, prev_nonnote_insn_bb (trace.setcc.insn),
			 trace.bb_entries.front ());

  if (res == other_set_found)
    log_return_void ("other set found - aborting trace\n");
  else if (res == set_not_found)
    {
      // It seems the initial search in the BB of the conditional branch
      // didn't find anything.  Now look in all predecessor BBs.
      for (edge_iterator ei = ei_start (trace.bb ()->preds);
	   !ei_end_p (ei); ei_next (&ei))
	{
	  edge e = ei_edge (ei);
	  trace.bb_entries.push_front (bb_entry (e->src));

	  res = record_set_of_reg (trace_reg, BB_END (e->src),
				   trace.bb_entries.front ());
	  if (res != set_found)
	    log_return_void ("set not found - aborting trace\n");
	}
    }

  if (dump_file != NULL)
    {
      log_msg ("\ncbranch trace summary:\n");
      for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
	   i != trace.bb_entries.end (); ++i)
	{
	  log_msg ("\n[bb %d]\n", i->bb->index);
	  if (!i->setcc.empty ())
	    {
	      log_rtx (i->setcc.set_rtx);
	      log_msg ("\n");
	    }
	  if (!i->cstore.empty ())
	    {
	      log_rtx (i->cstore.set_rtx);
	      log_msg ("\n");
	    }

	  for (std::vector<set_of_reg>::const_reverse_iterator j =
		   i->cstore_reg_reg_copies.rbegin ();
	       j != i->cstore_reg_reg_copies.rend (); ++j)
	    {
	      log_rtx (j->set_rtx);
	      log_msg ("\n");
	    }
	}

      log_rtx (trace.setcc.set_rtx);
      log_msg ("\n");
      log_rtx (PATTERN (trace.cbranch_insn));
      log_msg ("\n");
    }

  // Check that we don't have any empty BBs.
  // Only the BB with the cbranch may be empty.
  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    if (i->setcc.empty () && i->cstore.empty () && i->bb != trace.bb ())
      log_return_void ("\n[bb %d] is empty - aborting.\n", i->bb->index);

  // Determine the dominating cstore type
  // FIXME: Try to take the probabilities of the BBs into account somehow.
  int cstore_count = 0;
  int inv_cstore_count = 0;

  for (std::list<bb_entry>::const_iterator i = trace.bb_entries.begin ();
       i != trace.bb_entries.end (); ++i)
    {
      if (i->cstore_type == cstore_normal)
	cstore_count += 1;
      else if (i->cstore_type == cstore_inverted)
	inv_cstore_count += 1;
    }

  log_msg ("cstore count = %d  inverted cstore count = %d\n",
	   cstore_count, inv_cstore_count);

  // This puts a priority on inverting cstores.
  cstore_type_t dominating_cstore = inv_cstore_count >= cstore_count
				    ? cstore_inverted
				    : cstore_normal;

  if (dominating_cstore == cstore_inverted)
      log_msg ("will try to eliminate inverted cstore\n");
  else if (dominating_cstore == cstore_normal)
    {
      log_msg ("will try to eliminate normal cstore\n");
      if (!trace.can_invert_condition ())
	log_return_void ("branch condition can't be inverted - aborting\n");
    }
  else
    gcc_unreachable ();

  if (try_combine_comparisons (trace, cstore_count, inv_cstore_count,
			       dominating_cstore))
    return;

  try_eliminate_cstores (trace, cstore_count, inv_cstore_count,
			 dominating_cstore);
}

bool
sh_treg_combine::gate (function *)
{
  return optimize > 0;
}

unsigned int
sh_treg_combine::execute (function *fun)
{
  unsigned int ccr0 = INVALID_REGNUM;
  unsigned int ccr1 = INVALID_REGNUM;

  if (targetm.fixed_condition_code_regs (&ccr0, &ccr1)
      && ccr0 != INVALID_REGNUM)
    {
      // Initially create a reg rtx with VOIDmode.
      // When the first conditional branch is discovered, the mode is changed
      // to the mode that is actually used by the target.
      m_ccreg = gen_rtx_REG (VOIDmode, ccr0);
    }

  if (m_ccreg == NULL_RTX)
    log_return (0, "no ccreg.\n\n");

  if (STORE_FLAG_VALUE != 1)
    log_return (0, "unsupported STORE_FLAG_VALUE %d", STORE_FLAG_VALUE);

  log_msg ("ccreg: ");
  log_rtx (m_ccreg);
  log_msg ("  STORE_FLAG_VALUE = %d\n", STORE_FLAG_VALUE);

  // Look for basic blocks that end with a conditional branch or for
  // conditional insns and try to optimize them.
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    {
      rtx_insn* i = BB_END (bb);
      if (i == NULL || i == PREV_INSN (BB_HEAD (bb)))
	continue;

      // A conditional branch is always the last insn of a basic block.
      if (any_condjump_p (i) && onlyjump_p (i))
	{
	  try_optimize_cbranch (i);
	  i = PREV_INSN (i);
	}

      // Check all insns in block for conditional insns.
      for (; i != NULL && i != PREV_INSN (BB_HEAD (bb)); i = PREV_INSN (i))
	if (is_conditional_insn (i))
	  try_optimize_cbranch (i);
    }

  log_msg ("\n\n");

  // If new insns are created and this pass is executed after all insns
  // have been split already, we must split the insns we've changed or added
  // ourselves here.
  // FIXME: Multi-word operations (which emit multiple insns) are not handled
  // properly here, since only one insn will end up in 'm_touched_insns'.
  // On SH this is not a problem though.
  if (m_split_insns)
    for (std::vector<rtx>::const_iterator i = m_touched_insns.begin ();
	 i != m_touched_insns.end (); ++i)
      {
	log_msg ("trying to split insn:\n");
	log_insn (*i);
	log_msg ("\n");
	try_split (PATTERN (*i), *i, 0);
      }

  m_touched_insns.clear ();
  log_return (0, "\n\n");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// This allows instantiating the pass somewhere else without having to pull
// in a header file.
opt_pass*
make_pass_sh_treg_combine (gcc::context* ctx, bool split_insns,
			   const char* name)
{
  return new sh_treg_combine (ctx, split_insns, name);
}
