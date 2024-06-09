/* Shorten memrefs pass for RISC-V.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "backend.h"
#include "regs.h"
#include "target.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "df.h"
#include "predict.h"
#include "tree-pass.h"

/* Try to make more use of compressed load and store instructions by replacing
   a load/store at address BASE + LARGE_OFFSET with a new load/store at address
   NEW BASE + SMALL OFFSET.  If NEW BASE is stored in a compressed register, the
   load/store can be compressed.  Since creating NEW BASE incurs an overhead,
   the change is only attempted when BASE is referenced by at least four
   load/stores in the same basic block.  */

namespace {

const pass_data pass_data_shorten_memrefs =
{
  RTL_PASS, /* type */
  "shorten_memrefs", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_shorten_memrefs : public rtl_opt_pass
{
public:
  pass_shorten_memrefs (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_shorten_memrefs, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (TARGET_RVC || TARGET_ZCA)
	&& riscv_mshorten_memrefs && optimize > 0;
    }
  virtual unsigned int execute (function *);

private:
  typedef int_hash <HOST_WIDE_INT, 0> regno_hash;
  typedef hash_map <regno_hash, int> regno_map;

  regno_map * analyze (basic_block bb);
  void transform (regno_map *m, basic_block bb);
  bool get_si_mem_base_reg (rtx mem, rtx *addr, bool *extend);
}; // class pass_shorten_memrefs

bool
pass_shorten_memrefs::get_si_mem_base_reg (rtx mem, rtx *addr, bool *extend)
{
  /* Whether it's sign/zero extended.  */
  if (GET_CODE (mem) == ZERO_EXTEND || GET_CODE (mem) == SIGN_EXTEND)
    {
      *extend = true;
      mem = XEXP (mem, 0);
    }

  if (!MEM_P (mem) || GET_MODE (mem) != SImode)
    return false;
  *addr = XEXP (mem, 0);
  return GET_CODE (*addr) == PLUS && REG_P (XEXP (*addr, 0));
}

/* Count how many times each regno is referenced as base address for a memory
   access.  */

pass_shorten_memrefs::regno_map *
pass_shorten_memrefs::analyze (basic_block bb)
{
  regno_map *m = hash_map<regno_hash, int>::create_ggc (10);
  rtx_insn *insn;

  regstat_init_n_sets_and_refs ();

  FOR_BB_INSNS (bb, insn)
    {
      if (!NONJUMP_INSN_P (insn))
	continue;
      rtx pat = PATTERN (insn);
      if (GET_CODE (pat) != SET)
	continue;
      /* Analyze stores first then loads.  */
      for (int i = 0; i < 2; i++)
	{
	  rtx mem = XEXP (pat, i);
	  rtx addr;
	  bool extend = false;
	  if (get_si_mem_base_reg (mem, &addr, &extend))
	    {
	      HOST_WIDE_INT regno = REGNO (XEXP (addr, 0));
	      /* Do not count store zero as these cannot be compressed.  */
	      if (i == 0)
		{
		  if (XEXP (pat, 1) == CONST0_RTX (GET_MODE (XEXP (pat, 1))))
		    continue;
		}
	      if (REG_N_REFS (regno) < 4)
		continue;
	      m->get_or_insert (regno)++;
	    }
	  }
    }
  regstat_free_n_sets_and_refs ();

  return m;
}

/* Convert BASE + LARGE_OFFSET to NEW_BASE + SMALL_OFFSET for each load/store
   with a base reg referenced at least 4 times.  */

void
pass_shorten_memrefs::transform (regno_map *m, basic_block bb)
{
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    {
      if (!NONJUMP_INSN_P (insn))
	continue;
      rtx pat = PATTERN (insn);
      if (GET_CODE (pat) != SET)
	continue;
      start_sequence ();
      /* Transform stores first then loads.  */
      for (int i = 0; i < 2; i++)
	{
	  rtx mem = XEXP (pat, i);
	  rtx addr;
	  bool extend = false;
	  if (get_si_mem_base_reg (mem, &addr, &extend))
	    {
	      HOST_WIDE_INT regno = REGNO (XEXP (addr, 0));
	      /* Do not transform store zero as these cannot be compressed.  */
	      if (i == 0)
		{
		  if (XEXP (pat, 1) == CONST0_RTX (GET_MODE (XEXP (pat, 1))))
		    continue;
		}
	      if (m->get_or_insert (regno) > 3)
		{
		  if (extend)
		    {
		      addr
			= targetm.legitimize_address (addr, addr,
						      GET_MODE (XEXP (mem, 0)));
		      XEXP (XEXP (pat, i), 0)
			= replace_equiv_address (XEXP (mem, 0), addr);
		    }
		  else
		    {
		      addr = targetm.legitimize_address (addr, addr,
							 GET_MODE (mem));
		      XEXP (pat, i) = replace_equiv_address (mem, addr);
		    }
		  df_insn_rescan (insn);
		}
	    }
	}
      rtx_insn *seq = get_insns ();
      end_sequence ();
      emit_insn_before (seq, insn);
    }
}

unsigned int
pass_shorten_memrefs::execute (function *fn)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, fn)
  {
    regno_map *m;
    if (optimize_bb_for_speed_p (bb))
      continue;
    m = analyze (bb);
    transform (m, bb);
  }

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_shorten_memrefs (gcc::context *ctxt)
{
  return new pass_shorten_memrefs (ctxt);
}
