/* Mode switching cleanup pass for the EPIPHANY cpu.
   Copyright (C) 2000-2019 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cfgrtl.h"
#include "insn-attr-common.h"
#include "tree-pass.h"

namespace {

const pass_data pass_data_resolve_sw_modes =
{
  RTL_PASS, /* type */
  "resolve_sw_modes", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MODE_SWITCH, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_resolve_sw_modes : public rtl_opt_pass
{
public:
  pass_resolve_sw_modes(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_resolve_sw_modes, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize; }
  virtual unsigned int execute (function *);

}; // class pass_resolve_sw_modes

/* Clean-up after mode switching:
   Check for mode setting insns that have FP_MODE_ROUND_UNKNOWN.
   If only one rounding mode is required, select that one.
   Else we have to choose one to use in this mode setting insn and
   insert new mode setting insns on the edges where the other mode
   becomes unambigous.  */

unsigned
pass_resolve_sw_modes::execute (function *fun)
{
  basic_block bb;
  rtx_insn *insn;
  rtx src;
  vec<basic_block> todo;
  sbitmap pushed;
  bool need_commit = false;
  bool finalize_fp_sets = (MACHINE_FUNCTION (cfun)->unknown_mode_sets == 0);

  todo.create (last_basic_block_for_fn (fun));
  pushed = sbitmap_alloc (last_basic_block_for_fn (fun));
  bitmap_clear (pushed);
  if (!finalize_fp_sets)
    {
      df_note_add_problem ();
      df_analyze ();
    }
  FOR_EACH_BB_FN (bb, fun)
    FOR_BB_INSNS (bb, insn)
      {
	enum attr_fp_mode selected_mode;

	if (!NONJUMP_INSN_P (insn)
	    || recog_memoized (insn) != CODE_FOR_set_fp_mode)
	  continue;
	src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
	if (finalize_fp_sets)
	  {
	    SET_SRC (XVECEXP (PATTERN (insn), 0, 2)) = copy_rtx (src);
	    if (REG_P (src))
	      df_insn_rescan (insn);
	    continue;
	  }
	if (REG_P (src)
	    || XINT (XVECEXP (XEXP (src, 0), 0, 0), 0) != FP_MODE_ROUND_UNKNOWN)
	  continue;
	if (find_regno_note (insn, REG_UNUSED, FP_TRUNCATE_REGNUM))
	  selected_mode = FP_MODE_ROUND_NEAREST;
	else if (find_regno_note (insn, REG_UNUSED, FP_NEAREST_REGNUM))
	  selected_mode = FP_MODE_ROUND_TRUNC;
	else
	  {
	    /* We could get more fancy in the selection of the mode by
	       checking the total frequency of the affected edges.  */
	    selected_mode = (enum attr_fp_mode) epiphany_normal_fp_rounding;

	    todo.quick_push (bb);
	    bitmap_set_bit (pushed, bb->index);
	  }
	XVECEXP (XEXP (src, 0), 0, 0) = GEN_INT (selected_mode);
	SET_SRC (XVECEXP (PATTERN (insn), 0, 1)) = copy_rtx (src);
	SET_SRC (XVECEXP (PATTERN (insn), 0, 2)) = copy_rtx (src);
	df_insn_rescan (insn);
      }
  while (todo.length ())
    {
      basic_block bb = todo.pop ();
      int selected_reg, jilted_reg;
      enum attr_fp_mode jilted_mode;
      edge e;
      edge_iterator ei;

      bitmap_set_bit (pushed, bb->index);
      bitmap_set_bit (pushed, bb->index);

      if (epiphany_normal_fp_rounding == FP_MODE_ROUND_NEAREST)
	{
	  selected_reg = FP_NEAREST_REGNUM;
	  jilted_reg = FP_TRUNCATE_REGNUM;
	  jilted_mode = FP_MODE_ROUND_TRUNC;
	}
      else
	{
	  selected_reg = FP_TRUNCATE_REGNUM;
	  jilted_reg = FP_NEAREST_REGNUM;
	  jilted_mode = FP_MODE_ROUND_NEAREST;
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  basic_block succ = e->dest;
	  rtx_insn *seq;

	  if (!REGNO_REG_SET_P (DF_LIVE_IN (succ), jilted_reg))
	    continue;
	  if (REGNO_REG_SET_P (DF_LIVE_IN (succ), selected_reg))
	    {
	      if (bitmap_bit_p (pushed, succ->index))
		continue;
	      todo.quick_push (succ);
	      bitmap_set_bit (pushed, bb->index);
	      continue;
	    }
	  start_sequence ();
	  emit_set_fp_mode (EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN,
			    jilted_mode, FP_MODE_NONE, NULL);
	  seq = get_insns ();
	  end_sequence ();
	  need_commit = true;
	  insert_insn_on_edge (seq, e);
	}
    }
  todo.release ();
  sbitmap_free (pushed);
  if (need_commit)
    commit_edge_insertions ();
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_resolve_sw_modes (gcc::context *ctxt)
{
  return new pass_resolve_sw_modes (ctxt);
}
