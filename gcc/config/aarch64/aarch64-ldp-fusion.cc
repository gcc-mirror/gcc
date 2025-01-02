// LoadPair fusion optimization pass for AArch64.
// Copyright (C) 2023-2025 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tm_p.h"
#include "rtl-iter.h"
#include "tree-pass.h"
#include "insn-attr.h"
#include "pair-fusion.h"

static constexpr HOST_WIDE_INT LDP_IMM_BITS = 7;
static constexpr HOST_WIDE_INT LDP_IMM_SIGN_BIT = (1 << (LDP_IMM_BITS - 1));
static constexpr HOST_WIDE_INT LDP_MAX_IMM = LDP_IMM_SIGN_BIT - 1;
static constexpr HOST_WIDE_INT LDP_MIN_IMM = -LDP_MAX_IMM - 1;

struct aarch64_pair_fusion : public pair_fusion
{
  bool fpsimd_op_p (rtx reg_op, machine_mode mem_mode,
		    bool load_p) override final
  {
    // Before RA, we use the modes, noting that stores of constant zero
    // operands use GPRs (even in non-integer modes).  After RA, we use
    // the hard register numbers.
    return reload_completed
      ? (REG_P (reg_op) && FP_REGNUM_P (REGNO (reg_op)))
      : (GET_MODE_CLASS (mem_mode) != MODE_INT
	 && (load_p || !aarch64_const_zero_rtx_p (reg_op)));
  }

  bool pair_mem_insn_p (rtx_insn *rti, bool &load_p) override final;

  bool pair_mem_ok_with_policy (rtx base_mem, bool load_p) override final
  {
    return aarch64_mem_ok_with_ldpstp_policy_model (base_mem,
						    load_p,
						    GET_MODE (base_mem));
  }

  bool pair_operand_mode_ok_p (machine_mode mode) override final;

  rtx gen_pair (rtx *pats, rtx writeback, bool load_p) override final;

  bool pair_reg_operand_ok_p (bool load_p, rtx reg_op,
			      machine_mode mode) override final
  {
    return (load_p
	    ? aarch64_ldp_reg_operand (reg_op, mode)
	    : aarch64_stp_reg_operand (reg_op, mode));
  }

  int pair_mem_alias_check_limit () override final
  {
    return aarch64_ldp_alias_check_limit;
  }

  bool should_handle_writeback (writeback_type which) override final
  {
    if (which == writeback_type::ALL)
      return aarch64_ldp_writeback > 1;
    else
      return aarch64_ldp_writeback;
  }

  bool track_loads_p () override final
  {
    return aarch64_tune_params.ldp_policy_model
	   != AARCH64_LDP_STP_POLICY_NEVER;
  }

  bool track_stores_p () override final
  {
    return aarch64_tune_params.stp_policy_model
	   != AARCH64_LDP_STP_POLICY_NEVER;
  }

  bool pair_mem_in_range_p (HOST_WIDE_INT offset) override final
  {
    return (offset >= LDP_MIN_IMM && offset <= LDP_MAX_IMM);
  }

  rtx gen_promote_writeback_pair (rtx wb_effect, rtx mem, rtx regs[2],
				  bool load_p) override final;

  rtx destructure_pair (rtx regs[2], rtx pattern, bool load_p) override final;
};

bool
aarch64_pair_fusion::pair_mem_insn_p (rtx_insn *rti, bool &load_p)
{
  rtx pat = PATTERN (rti);
  if (GET_CODE (pat) == PARALLEL
      && XVECLEN (pat, 0) == 2)
    {
      const auto attr = get_attr_ldpstp (rti);
      if (attr == LDPSTP_NONE)
	return false;

      load_p = (attr == LDPSTP_LDP);
      gcc_checking_assert (load_p || attr == LDPSTP_STP);
      return true;
    }
  return false;
}

rtx
aarch64_pair_fusion::gen_pair (rtx *pats, rtx writeback, bool load_p)
{
  rtx pair_pat;

  if (writeback)
    {
      auto patvec = gen_rtvec (3, writeback, pats[0], pats[1]);
      return gen_rtx_PARALLEL (VOIDmode, patvec);
    }
  else if (load_p)
    return aarch64_gen_load_pair (XEXP (pats[0], 0),
				  XEXP (pats[1], 0),
				  XEXP (pats[0], 1));
  else
    return aarch64_gen_store_pair (XEXP (pats[0], 0),
				   XEXP (pats[0], 1),
				   XEXP (pats[1], 1));
  return pair_pat;
}

// Return true if we should consider forming ldp/stp insns from memory
// accesses with operand mode MODE at this stage in compilation.
bool
aarch64_pair_fusion::pair_operand_mode_ok_p (machine_mode mode)
{
  if (!aarch64_ldpstp_operand_mode_p (mode))
    return false;

  // We don't pair up TImode accesses before RA because TImode is
  // special in that it can be allocated to a pair of GPRs or a single
  // FPR, and the RA is best placed to make that decision.
  return reload_completed || mode != TImode;
}

// Given a pair mode MODE, return a canonical mode to be used for a single
// operand of such a pair.  Currently we only use this when promoting a
// non-writeback pair into a writeback pair, as it isn't otherwise clear
// which mode to use when storing a modeless CONST_INT.
static machine_mode
aarch64_operand_mode_for_pair_mode (machine_mode mode)
{
  switch (mode)
    {
    case E_V2x4QImode:
      return SImode;
    case E_V2x8QImode:
      return DImode;
    case E_V2x16QImode:
      return V16QImode;
    default:
      gcc_unreachable ();
    }
}

// Given a load pair insn in PATTERN, unpack the insn, storing
// the registers in REGS and returning the mem.
static rtx
aarch64_destructure_load_pair (rtx regs[2], rtx pattern)
{
  rtx mem = NULL_RTX;

  for (int i = 0; i < 2; i++)
    {
      rtx pat = XVECEXP (pattern, 0, i);
      regs[i] = XEXP (pat, 0);
      rtx unspec = XEXP (pat, 1);
      gcc_checking_assert (GET_CODE (unspec) == UNSPEC);
      rtx this_mem = XVECEXP (unspec, 0, 0);
      if (mem)
	gcc_checking_assert (rtx_equal_p (mem, this_mem));
      else
	{
	  gcc_checking_assert (MEM_P (this_mem));
	  mem = this_mem;
	}
    }

  return mem;
}

// Given a store pair insn in PATTERN, unpack the insn, storing
// the register operands in REGS, and returning the mem.
static rtx
aarch64_destructure_store_pair (rtx regs[2], rtx pattern)
{
  rtx mem = XEXP (pattern, 0);
  rtx unspec = XEXP (pattern, 1);
  gcc_checking_assert (GET_CODE (unspec) == UNSPEC);
  for (int i = 0; i < 2; i++)
    regs[i] = XVECEXP (unspec, 0, i);
  return mem;
}

rtx
aarch64_pair_fusion::destructure_pair (rtx regs[2], rtx pattern, bool load_p)
{
  if (load_p)
    return aarch64_destructure_load_pair (regs, pattern);
  else
    return aarch64_destructure_store_pair (regs, pattern);
}

rtx
aarch64_pair_fusion::gen_promote_writeback_pair (rtx wb_effect, rtx pair_mem,
						 rtx regs[2],
						 bool load_p)
{
  auto op_mode = aarch64_operand_mode_for_pair_mode (GET_MODE (pair_mem));

  machine_mode modes[2];
  for (int i = 0; i < 2; i++)
    {
      machine_mode mode = GET_MODE (regs[i]);
      if (load_p)
	gcc_checking_assert (mode != VOIDmode);
      else if (mode == VOIDmode)
	mode = op_mode;

      modes[i] = mode;
    }

  const auto op_size = GET_MODE_SIZE (modes[0]);
  gcc_checking_assert (known_eq (op_size, GET_MODE_SIZE (modes[1])));

  rtx pats[2];
  for (int i = 0; i < 2; i++)
    {
      rtx mem = adjust_address_nv (pair_mem, modes[i], op_size * i);
      pats[i] = load_p
	? gen_rtx_SET (regs[i], mem)
	: gen_rtx_SET (mem, regs[i]);
    }

  return gen_rtx_PARALLEL (VOIDmode,
			   gen_rtvec (3, wb_effect, pats[0], pats[1]));
}

namespace {

const pass_data pass_data_ldp_fusion =
{
  RTL_PASS, /* type */
  "ldp_fusion", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_ldp_fusion : public rtl_opt_pass
{
public:
  pass_ldp_fusion (gcc::context *ctx)
    : rtl_opt_pass (pass_data_ldp_fusion, ctx)
    {}

  opt_pass *clone () override { return new pass_ldp_fusion (m_ctxt); }

  bool gate (function *) final override
    {
      if (!optimize || optimize_debug)
	return false;

      if (reload_completed)
	return flag_aarch64_late_ldp_fusion;
      else
	return flag_aarch64_early_ldp_fusion;
    }

  unsigned execute (function *) final override
    {
      aarch64_pair_fusion pass;
      pass.run ();
      return 0;
    }
};

} // anon namespace

rtl_opt_pass *
make_pass_ldp_fusion (gcc::context *ctx)
{
  return new pass_ldp_fusion (ctx);
}
