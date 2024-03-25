/* An SH specific RTL pass that tries to optimize clrt and sett insns.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_ALGORITHM
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "df.h"
#include "cfgrtl.h"
#include "tree-pass.h"

/*
This pass tries to eliminate unnecessary sett or clrt instructions in cases
where the ccreg value is already known to be the same as the constant set
would set it to.  This is done as follows:

Check every BB's insn and see if it's a sett or clrt.
Once a sett or clrt insn is hit, walk insns and predecessor basic blocks
backwards from that insn and determine all possible ccreg values from all
basic block paths.
Insns that set the ccreg value in some way (simple set, clobber etc) are
recorded.  Conditional branches where one edge leads to the sett / clrt insn
are also recorded, since for each edge in the conditional branch the ccreg
value is known constant.
After collecting all possible ccreg values at the sett / clrt insn, check that
all the values are the same.  If that value is the same as the sett / clrt
insn would set the ccreg to, the sett / clrt insn can be eliminated.
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// RTL pass class

class sh_optimize_sett_clrt : public rtl_opt_pass
{
public:
  sh_optimize_sett_clrt (gcc::context* ctx, const char* name);
  virtual ~sh_optimize_sett_clrt (void);
  virtual bool gate (function*);
  virtual unsigned int execute (function* fun);

private:
  static const pass_data default_pass_data;

  struct ccreg_value
  {
    // The insn at which the ccreg value was determined.
    // Might be NULL if e.g. an unknown value is recorded for an
    // empty basic block.
    rtx_insn *insn;

    // The basic block where the insn was discovered.
    basic_block bb;

    // The value of ccreg.  If NULL_RTX, the exact value is not known, but
    // the ccreg is changed in some way (e.g. clobbered).
    rtx value;
  };

  // Update the mode of the captured m_ccreg with the specified mode.
  void update_ccreg_mode (machine_mode m);

  // Given an insn pattern, check if it sets the ccreg to a constant value
  // of either zero or STORE_FLAG_VALUE.  If so, return the value rtx,
  // NULL_RTX otherwise.
  rtx const_setcc_value (rtx pat) const;

  // Given a start insn and its basic block, recursively determine all
  // possible ccreg values in all basic block paths that can lead to the
  // start insn.
  bool find_last_ccreg_values (rtx_insn *start_insn, basic_block bb,
			       std::vector<ccreg_value>& values_out,
			       std::vector<basic_block>& prev_visited_bb) const;

  // Given a cbranch insn, its basic block and another basic block, determine
  // the value to which the ccreg will be set after jumping/falling through to
  // the specified target basic block.
  bool sh_cbranch_ccreg_value (rtx_insn *cbranch_insn,
			       basic_block cbranch_insn_bb,
			       basic_block branch_target_bb) const;

  // Check whether all of the ccreg values are the same.
  static bool all_ccreg_values_equal (const std::vector<ccreg_value>& values);

  // Remove REG_DEAD and REG_UNUSED notes from insns of the specified
  // ccreg_value entries.
  void remove_ccreg_dead_unused_notes (std::vector<ccreg_value>& values) const;

  // rtx of the ccreg that is obtained from the target.
  rtx m_ccreg;
};

const pass_data sh_optimize_sett_clrt::default_pass_data =
{
  RTL_PASS,		// type
  "",			// name (overwritten by the constructor)
  OPTGROUP_NONE,	// optinfo_flags
  TV_OPTIMIZE,		// tv_id
  0,			// properties_required
  0,			// properties_provided
  0,			// properties_destroyed
  0,			// todo_flags_start
  0			// todo_flags_finish
};

sh_optimize_sett_clrt::sh_optimize_sett_clrt (gcc::context* ctx,
					      const char* name)
: rtl_opt_pass (default_pass_data, ctx),
  m_ccreg (NULL_RTX)
{
  // Overwrite default name in pass_data base class.
  this->name = name;
}

sh_optimize_sett_clrt::~sh_optimize_sett_clrt (void)
{
}

bool
sh_optimize_sett_clrt::gate (function*)
{
  return optimize > 0;
}

unsigned int
sh_optimize_sett_clrt::execute (function* fun)
{
  unsigned int ccr0 = INVALID_REGNUM;
  unsigned int ccr1 = INVALID_REGNUM;

  if (targetm.fixed_condition_code_regs (&ccr0, &ccr1)
      && ccr0 != INVALID_REGNUM)
    {
      // Initially create a reg rtx with VOIDmode.
      // When the constant setcc is discovered, the mode is changed
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

  if (!df_regs_ever_live_p (ccr0))
    log_return (0, "ccreg never live\n\n");

  // Output vector for find_known_ccreg_values.
  std::vector<ccreg_value> ccreg_values;
  ccreg_values.reserve (32);

  // Something for recording visited basic blocks to avoid infinite recursion.
  std::vector<basic_block> visited_bbs;
  visited_bbs.reserve (32);

  // Look for insns that set the ccreg to a constant value and see if it can
  // be optimized.
  basic_block bb;
  FOR_EACH_BB_REVERSE_FN (bb, fun)
    for (rtx_insn *next_i, *i = NEXT_INSN (BB_HEAD (bb));
	 i != NULL_RTX && i != BB_END (bb); i = next_i)
      {
	next_i = NEXT_INSN (i);

	if (!INSN_P (i) || !NONDEBUG_INSN_P (i))
	  continue;

	rtx setcc_val = const_setcc_value (PATTERN (i));
	if (setcc_val != NULL_RTX)
	  {
	    update_ccreg_mode (GET_MODE (XEXP (PATTERN (i), 0)));

	    log_msg ("\n\nfound const setcc insn in [bb %d]: \n", bb->index);
	    log_insn (i);
	    log_msg ("\n");

	    ccreg_values.clear ();
	    visited_bbs.clear ();
	    bool ok = find_last_ccreg_values (PREV_INSN (i), bb, ccreg_values,
					      visited_bbs);

	    log_msg ("number of ccreg values collected: %u\n",
		     (unsigned int)ccreg_values.size ());

	    // If all the collected values are equal and are equal to the
	    // constant value of the setcc insn, the setcc insn can be
	    // removed.
	    if (ok && all_ccreg_values_equal (ccreg_values)
		&& rtx_equal_p (ccreg_values.front ().value, setcc_val))
	      {
		log_msg ("all values are ");
		log_rtx (setcc_val);
		log_msg ("\n");

		delete_insn (i);
		remove_ccreg_dead_unused_notes (ccreg_values);
	      }
	  }
      }

  log_return (0, "\n\n");
}

void
sh_optimize_sett_clrt::update_ccreg_mode (machine_mode m)
{
  if (GET_MODE (m_ccreg) == m)
    return;

  PUT_MODE (m_ccreg, m);
  log_msg ("updated ccreg mode: ");
  log_rtx (m_ccreg);
  log_msg ("\n\n");
}

rtx
sh_optimize_sett_clrt::const_setcc_value (rtx pat) const
{
  if (GET_CODE (pat) == SET
      && REG_P (XEXP (pat, 0)) && REGNO (XEXP (pat, 0)) == REGNO (m_ccreg)
      && CONST_INT_P (XEXP (pat, 1))
      && (INTVAL (XEXP (pat, 1)) == 0
	  || INTVAL (XEXP (pat, 1)) == STORE_FLAG_VALUE))
    return XEXP (pat, 1);
  else
    return NULL_RTX;
}

bool
sh_optimize_sett_clrt
::sh_cbranch_ccreg_value (rtx_insn *cbranch_insn, basic_block cbranch_insn_bb,
			  basic_block branch_target_bb) const
{
  rtx pc_set_rtx = pc_set (cbranch_insn);
  gcc_assert (pc_set_rtx != NULL_RTX);
  gcc_assert (branch_target_bb != NULL);

  rtx cond = XEXP (XEXP (pc_set_rtx, 1), 0);
  bool branch_if;

  if (GET_CODE (cond) == NE
      && REG_P (XEXP (cond, 0)) && REGNO (XEXP (cond, 0)) == REGNO (m_ccreg)
      && XEXP (cond, 1) == const0_rtx)
    branch_if = true;

  else if (GET_CODE (cond) == EQ
      && REG_P (XEXP (cond, 0)) && REGNO (XEXP (cond, 0)) == REGNO (m_ccreg)
      && XEXP (cond, 1) == const0_rtx)
    branch_if = false;

  else
    gcc_unreachable ();

  if (branch_target_bb == BRANCH_EDGE (cbranch_insn_bb)->dest)
    return branch_if;
  else if (branch_target_bb == FALLTHRU_EDGE (cbranch_insn_bb)->dest)
    return !branch_if;
  else
    gcc_unreachable ();
}

bool
sh_optimize_sett_clrt
::find_last_ccreg_values (rtx_insn *start_insn, basic_block bb,
			  std::vector<ccreg_value>& values_out,
			  std::vector<basic_block>& prev_visited_bb) const
{
  // FIXME: For larger CFGs this will unnecessarily re-visit basic blocks.
  // Once a basic block has been visited, the result should be stored in
  // some container so that it can be looked up quickly eliminating the
  // re-visits.
  log_msg ("looking for ccreg values in [bb %d] ", bb->index);
  if (!prev_visited_bb.empty ())
    log_msg ("(prev visited [bb %d])", prev_visited_bb.back ()->index);
  log_msg ("\n");

  for (rtx_insn *i = start_insn; i != NULL && i != PREV_INSN (BB_HEAD (bb));
       i = PREV_INSN (i))
    {
      if (!INSN_P (i))
	continue;

      if (reg_set_p (m_ccreg, i))
	{
	  const_rtx set_rtx = set_of (m_ccreg, i);

	  ccreg_value v;
	  v.insn = i;
	  v.bb = bb;
	  v.value = set_rtx != NULL_RTX && GET_CODE (set_rtx) == SET
		    ? XEXP (set_rtx, 1)
		    : NULL_RTX;

	  log_msg ("found setcc in [bb %d] in insn:\n", bb->index);
	  log_insn (i);
	  log_msg ("\nccreg value: ");
	  log_rtx (v.value);
	  log_msg ("\n");

	  values_out.push_back (v);
	  return true;
	}

      if (any_condjump_p (i) && onlyjump_p (i) && !prev_visited_bb.empty ())
	{
	  // For a conditional branch the ccreg value will be a known constant
	  // of either 0 or STORE_FLAG_VALUE after branching/falling through
	  // to one of the two successor BBs.  Record the value for the BB
	  // where we came from.
	  log_msg ("found cbranch in [bb %d]:\n", bb->index);
	  log_insn (i);

	  ccreg_value v;
	  v.insn = i;
	  v.bb = bb;
	  v.value = GEN_INT (sh_cbranch_ccreg_value (i, bb,
						     prev_visited_bb.back ()));

	  log_msg ("    branches to [bb %d] with ccreg value ",
		   prev_visited_bb.back ()->index);
	  log_rtx (v.value);
	  log_msg ("\n");

	  values_out.push_back (v);
	  return true;
	}
    }

  // If here, we've walked up all the insns of the current basic block
  // and none of them seems to modify the ccreg.
  // In this case, check the predecessor basic blocks.
  unsigned int pred_bb_count = 0;

  // If the current basic block is not in the stack of previously visited
  // basic blocks yet, we can recursively check the predecessor basic blocks.
  // Otherwise we have a loop in the CFG and recursing again will result in
  // an infinite loop.
  if (std::find (prev_visited_bb.rbegin (), prev_visited_bb.rend (), bb)
      == prev_visited_bb.rend ())
    {
      prev_visited_bb.push_back (bb);

      for (edge_iterator ei = ei_start (bb->preds); !ei_end_p (ei);
	   ei_next (&ei))
	{
	  if (ei_edge (ei)->flags & EDGE_COMPLEX)
	    log_return (false, "aborting due to complex edge\n");

	  basic_block pred_bb = ei_edge (ei)->src;
	  pred_bb_count += 1;
	  if (!find_last_ccreg_values (BB_END (pred_bb), pred_bb, values_out,
				       prev_visited_bb))
	    return false;
	}

      prev_visited_bb.pop_back ();
    }
  else
    log_msg ("loop detected for [bb %d]\n", bb->index);

  log_msg ("[bb %d] pred_bb_count = %u\n", bb->index, pred_bb_count);

  if (pred_bb_count == 0)
  {
    // If we haven't checked a single predecessor basic block, the current
    // basic block is probably a leaf block and we don't know the ccreg value.
    log_msg ("unknown ccreg value for [bb %d]\n", bb->index);

    ccreg_value v;
    v.insn = BB_END (bb);
    v.bb = bb;
    v.value = NULL_RTX;

    values_out.push_back (v);
  }

  return true;
}

bool
sh_optimize_sett_clrt
::all_ccreg_values_equal (const std::vector<ccreg_value>& values)
{
  if (values.empty ())
    return false;

  rtx last_value = values.front ().value;

  // If the ccreg is modified in the insn but the exact value is not known
  // the value rtx might be null.
  if (last_value == NULL_RTX)
    return false;

  for (std::vector<ccreg_value>::const_iterator i = values.begin ();
       i != values.end (); ++i)
    if (i->value == NULL_RTX || !rtx_equal_p (last_value, i->value))
      return false;

  return true;
}

void
sh_optimize_sett_clrt
::remove_ccreg_dead_unused_notes (std::vector<ccreg_value>& values) const
{
  for (std::vector<ccreg_value>::iterator i = values.begin ();
       i != values.end (); ++i)
    {
      if (i->insn == NULL_RTX)
	continue;

      rtx n = find_regno_note (i->insn, REG_DEAD, REGNO (m_ccreg));
      if (n != NULL_RTX)
	remove_note (i->insn, n);

      n = find_regno_note (i->insn, REG_UNUSED, REGNO (m_ccreg));
      if (n != NULL_RTX)
	remove_note (i->insn, n);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// This allows instantiating the pass somewhere else without having to pull
// in a header file.
opt_pass*
make_pass_sh_optimize_sett_clrt (gcc::context* ctx, const char* name)
{
  return new sh_optimize_sett_clrt (ctx, name);
}
