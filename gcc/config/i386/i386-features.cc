/* Copyright (C) 1988-2025 Free Software Foundation, Inc.

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
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "cfgbuild.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "tm-constrs.h"
#include "cselib.h"
#include "sched-int.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "target-globals.h"
#include "gimple-iterator.h"
#include "shrink-wrap.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tree-iterator.h"
#include "dbgcnt.h"
#include "case-cfn-macros.h"
#include "dojump.h"
#include "fold-const-call.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "print-rtl.h"
#include "intl.h"
#include "ifcvt.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-builtins.h"
#include "i386-features.h"
#include "i386-expand.h"

const char * const xlogue_layout::STUB_BASE_NAMES[XLOGUE_STUB_COUNT] = {
  "savms64",
  "resms64",
  "resms64x",
  "savms64f",
  "resms64f",
  "resms64fx"
};

const unsigned xlogue_layout::REG_ORDER[xlogue_layout::MAX_REGS] = {
/* The below offset values are where each register is stored for the layout
   relative to incoming stack pointer.  The value of each m_regs[].offset will
   be relative to the incoming base pointer (rax or rsi) used by the stub.

    s_instances:   0		1		2		3
    Offset:					realigned or	aligned + 8
    Register	   aligned	aligned + 8	aligned w/HFP	w/HFP	*/
    XMM15_REG,	/* 0x10		0x18		0x10		0x18	*/
    XMM14_REG,	/* 0x20		0x28		0x20		0x28	*/
    XMM13_REG,	/* 0x30		0x38		0x30		0x38	*/
    XMM12_REG,	/* 0x40		0x48		0x40		0x48	*/
    XMM11_REG,	/* 0x50		0x58		0x50		0x58	*/
    XMM10_REG,	/* 0x60		0x68		0x60		0x68	*/
    XMM9_REG,	/* 0x70		0x78		0x70		0x78	*/
    XMM8_REG,	/* 0x80		0x88		0x80		0x88	*/
    XMM7_REG,	/* 0x90		0x98		0x90		0x98	*/
    XMM6_REG,	/* 0xa0		0xa8		0xa0		0xa8	*/
    SI_REG,	/* 0xa8		0xb0		0xa8		0xb0	*/
    DI_REG,	/* 0xb0		0xb8		0xb0		0xb8	*/
    BX_REG,	/* 0xb8		0xc0		0xb8		0xc0	*/
    BP_REG,	/* 0xc0		0xc8		N/A		N/A	*/
    R12_REG,	/* 0xc8		0xd0		0xc0		0xc8	*/
    R13_REG,	/* 0xd0		0xd8		0xc8		0xd0	*/
    R14_REG,	/* 0xd8		0xe0		0xd0		0xd8	*/
    R15_REG,	/* 0xe0		0xe8		0xd8		0xe0	*/
};

/* Instantiate static const values.  */
const HOST_WIDE_INT xlogue_layout::STUB_INDEX_OFFSET;
const unsigned xlogue_layout::MIN_REGS;
const unsigned xlogue_layout::MAX_REGS;
const unsigned xlogue_layout::MAX_EXTRA_REGS;
const unsigned xlogue_layout::VARIANT_COUNT;
const unsigned xlogue_layout::STUB_NAME_MAX_LEN;

/* Initialize xlogue_layout::s_stub_names to zero.  */
char xlogue_layout::s_stub_names[2][XLOGUE_STUB_COUNT][VARIANT_COUNT]
				[STUB_NAME_MAX_LEN];

/* Instantiates all xlogue_layout instances.  */
const xlogue_layout xlogue_layout::s_instances[XLOGUE_SET_COUNT] = {
  xlogue_layout (0, false),
  xlogue_layout (8, false),
  xlogue_layout (0, true),
  xlogue_layout (8, true)
};

/* Return an appropriate const instance of xlogue_layout based upon values
   in cfun->machine and crtl.  */
const class xlogue_layout &
xlogue_layout::get_instance ()
{
  enum xlogue_stub_sets stub_set;
  bool aligned_plus_8 = cfun->machine->call_ms2sysv_pad_in;

  if (stack_realign_fp)
    stub_set = XLOGUE_SET_HFP_ALIGNED_OR_REALIGN;
  else if (frame_pointer_needed)
    stub_set = aligned_plus_8
	      ? XLOGUE_SET_HFP_ALIGNED_PLUS_8
	      : XLOGUE_SET_HFP_ALIGNED_OR_REALIGN;
  else
    stub_set = aligned_plus_8 ? XLOGUE_SET_ALIGNED_PLUS_8 : XLOGUE_SET_ALIGNED;

  return s_instances[stub_set];
}

/* Determine how many clobbered registers can be saved by the stub.
   Returns the count of registers the stub will save and restore.  */
unsigned
xlogue_layout::count_stub_managed_regs ()
{
  bool hfp = frame_pointer_needed || stack_realign_fp;
  unsigned i, count;
  unsigned regno;

  for (count = i = MIN_REGS; i < MAX_REGS; ++i)
    {
      regno = REG_ORDER[i];
      if (regno == BP_REG && hfp)
	continue;
      if (!ix86_save_reg (regno, false, false))
	break;
      ++count;
    }
  return count;
}

/* Determine if register REGNO is a stub managed register given the
   total COUNT of stub managed registers.  */
bool
xlogue_layout::is_stub_managed_reg (unsigned regno, unsigned count)
{
  bool hfp = frame_pointer_needed || stack_realign_fp;
  unsigned i;

  for (i = 0; i < count; ++i)
    {
      gcc_assert (i < MAX_REGS);
      if (REG_ORDER[i] == BP_REG && hfp)
	++count;
      else if (REG_ORDER[i] == regno)
	return true;
    }
  return false;
}

/* Constructor for xlogue_layout.  */
xlogue_layout::xlogue_layout (HOST_WIDE_INT stack_align_off_in, bool hfp)
  : m_hfp (hfp) , m_nregs (hfp ? 17 : 18),
    m_stack_align_off_in (stack_align_off_in)
{
  HOST_WIDE_INT offset = stack_align_off_in;
  unsigned i, j;

  for (i = j = 0; i < MAX_REGS; ++i)
    {
      unsigned regno = REG_ORDER[i];

      if (regno == BP_REG && hfp)
	continue;
      if (SSE_REGNO_P (regno))
	{
	  offset += 16;
	  /* Verify that SSE regs are always aligned.  */
	  gcc_assert (!((stack_align_off_in + offset) & 15));
	}
      else
	offset += 8;

      m_regs[j].regno    = regno;
      m_regs[j++].offset = offset - STUB_INDEX_OFFSET;
    }
  gcc_assert (j == m_nregs);
}

const char *
xlogue_layout::get_stub_name (enum xlogue_stub stub,
			      unsigned n_extra_regs)
{
  const int have_avx = TARGET_AVX;
  char *name = s_stub_names[!!have_avx][stub][n_extra_regs];

  /* Lazy init */
  if (!*name)
    {
      int res = snprintf (name, STUB_NAME_MAX_LEN, "__%s_%s_%u",
			  (have_avx ? "avx" : "sse"),
			  STUB_BASE_NAMES[stub],
			  MIN_REGS + n_extra_regs);
      gcc_checking_assert (res < (int)STUB_NAME_MAX_LEN);
    }

  return name;
}

/* Return rtx of a symbol ref for the entry point (based upon
   cfun->machine->call_ms2sysv_extra_regs) of the specified stub.  */
rtx
xlogue_layout::get_stub_rtx (enum xlogue_stub stub)
{
  const unsigned n_extra_regs = cfun->machine->call_ms2sysv_extra_regs;
  gcc_checking_assert (n_extra_regs <= MAX_EXTRA_REGS);
  gcc_assert (stub < XLOGUE_STUB_COUNT);
  gcc_assert (crtl->stack_realign_finalized);

  return gen_rtx_SYMBOL_REF (Pmode, get_stub_name (stub, n_extra_regs));
}

unsigned scalar_chain::max_id = 0;

namespace {

/* Initialize new chain.  */

scalar_chain::scalar_chain (enum machine_mode smode_, enum machine_mode vmode_)
{
  smode = smode_;
  vmode = vmode_;

  chain_id = ++max_id;

   if (dump_file)
    fprintf (dump_file, "Created a new instruction chain #%d\n", chain_id);

  bitmap_obstack_initialize (NULL);
  insns = BITMAP_ALLOC (NULL);
  defs = BITMAP_ALLOC (NULL);
  defs_conv = BITMAP_ALLOC (NULL);
  insns_conv = BITMAP_ALLOC (NULL);
  queue = NULL;

  cost_sse_integer = 0;
  weighted_cost_sse_integer = 0 ;
  max_visits = x86_stv_max_visits;
}

/* Free chain's data.  */

scalar_chain::~scalar_chain ()
{
  BITMAP_FREE (insns);
  BITMAP_FREE (defs);
  BITMAP_FREE (defs_conv);
  BITMAP_FREE (insns_conv);
  bitmap_obstack_release (NULL);
}

/* Add instruction into chains' queue.  */

void
scalar_chain::add_to_queue (unsigned insn_uid)
{
  if (!bitmap_set_bit (queue, insn_uid))
    return;

  if (dump_file)
    fprintf (dump_file, "  Adding insn %d into chain's #%d queue\n",
	     insn_uid, chain_id);
}

/* For DImode conversion, mark register defined by DEF as requiring
   conversion.  */

void
scalar_chain::mark_dual_mode_def (df_ref def)
{
  gcc_assert (DF_REF_REG_DEF_P (def));

  /* Record the def/insn pair so we can later efficiently iterate over
     the defs to convert on insns not in the chain.  */
  bool reg_new = bitmap_set_bit (defs_conv, DF_REF_REGNO (def));
  basic_block bb = BLOCK_FOR_INSN (DF_REF_INSN (def));
  profile_count entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  bool speed_p = optimize_bb_for_speed_p (bb);
  int cost = 0;

  if (!bitmap_bit_p (insns, DF_REF_INSN_UID (def)))
    {
      if (!bitmap_set_bit (insns_conv, DF_REF_INSN_UID (def))
	  && !reg_new)
	return;

      /* Cost integer to sse moves.  */
      if (speed_p)
	cost = COSTS_N_INSNS (ix86_cost->integer_to_sse) / 2;
      else if (TARGET_64BIT || smode == SImode)
	cost = COSTS_N_BYTES (4);
      /* vmovd (4 bytes) + vpinsrd (6 bytes).  */
      else if (TARGET_SSE4_1)
	cost = COSTS_N_BYTES (10);
      /* movd (4 bytes) + movd (4 bytes) + unpckldq (4 bytes).  */
      else
	cost = COSTS_N_BYTES (12);
    }
  else
    {
      if (!reg_new)
	return;

      /* Cost sse to integer moves.  */
      if (speed_p)
	cost = COSTS_N_INSNS (ix86_cost->sse_to_integer) / 2;
      else if (TARGET_64BIT || smode == SImode)
	cost = COSTS_N_BYTES (4);
      /* vmovd (4 bytes) + vpextrd (6 bytes).  */
      else if (TARGET_SSE4_1)
	cost = COSTS_N_BYTES (10);
      /* movd (4 bytes) + psrlq (5 bytes) + movd (4 bytes).  */
      else
	cost = COSTS_N_BYTES (13);
    }

  if (speed_p)
    weighted_cost_sse_integer += bb->count.to_sreal_scale (entry_count) * cost;

  cost_sse_integer += cost;

  if (dump_file)
    fprintf (dump_file,
	     "  Mark r%d def in insn %d as requiring both modes in chain #%d\n",
	     DF_REF_REGNO (def), DF_REF_INSN_UID (def), chain_id);
}

/* Check REF's chain to add new insns into a queue
   and find registers requiring conversion.  Return true if OK, false
   if the analysis was aborted.  */

bool
scalar_chain::analyze_register_chain (bitmap candidates, df_ref ref,
				      bitmap disallowed)
{
  df_link *chain;
  bool mark_def = false;

  gcc_checking_assert (bitmap_bit_p (insns, DF_REF_INSN_UID (ref)));

  for (chain = DF_REF_CHAIN (ref); chain; chain = chain->next)
    {
      unsigned uid = DF_REF_INSN_UID (chain->ref);

      if (!NONDEBUG_INSN_P (DF_REF_INSN (chain->ref)))
	continue;

      if (--max_visits == 0)
	return false;

      if (!DF_REF_REG_MEM_P (chain->ref))
	{
	  if (bitmap_bit_p (insns, uid))
	    continue;

	  if (bitmap_bit_p (candidates, uid))
	    {
	      add_to_queue (uid);
	      continue;
	    }

	  /* If we run into parts of an aborted chain discovery abort.  */
	  if (bitmap_bit_p (disallowed, uid))
	    return false;
	}

      if (DF_REF_REG_DEF_P (chain->ref))
	{
	  if (dump_file)
	    fprintf (dump_file, "  r%d def in insn %d isn't convertible\n",
		     DF_REF_REGNO (chain->ref), uid);
	  mark_dual_mode_def (chain->ref);
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "  r%d use in insn %d isn't convertible\n",
		     DF_REF_REGNO (chain->ref), uid);
	  mark_def = true;
	}
    }

  if (mark_def)
    mark_dual_mode_def (ref);

  return true;
}

/* Add instruction into a chain.  Return true if OK, false if the search
   was aborted.  */

bool
scalar_chain::add_insn (bitmap candidates, unsigned int insn_uid,
			bitmap disallowed)
{
  if (!bitmap_set_bit (insns, insn_uid))
    return true;

  if (dump_file)
    fprintf (dump_file, "  Adding insn %d to chain #%d\n", insn_uid, chain_id);

  rtx_insn *insn = DF_INSN_UID_GET (insn_uid)->insn;
  rtx def_set = single_set (insn);
  if (def_set && REG_P (SET_DEST (def_set))
      && !HARD_REGISTER_P (SET_DEST (def_set)))
    bitmap_set_bit (defs, REGNO (SET_DEST (def_set)));

  /* ???  The following is quadratic since analyze_register_chain
     iterates over all refs to look for dual-mode regs.  Instead this
     should be done separately for all regs mentioned in the chain once.  */
  df_ref ref;
  for (ref = DF_INSN_UID_DEFS (insn_uid); ref; ref = DF_REF_NEXT_LOC (ref))
    if (!HARD_REGISTER_P (DF_REF_REG (ref)))
      if (!analyze_register_chain (candidates, ref, disallowed))
	return false;

  /* The operand(s) of VEC_SELECT don't need to be converted/convertible.  */
  if (def_set && GET_CODE (SET_SRC (def_set)) == VEC_SELECT)
    return true;

  for (ref = DF_INSN_UID_USES (insn_uid); ref; ref = DF_REF_NEXT_LOC (ref))
    if (!DF_REF_REG_MEM_P (ref))
      if (!analyze_register_chain (candidates, ref, disallowed))
	return false;

  return true;
}

/* Build new chain starting from insn INSN_UID recursively
   adding all dependent uses and definitions.  Return true if OK, false
   if the chain discovery was aborted.  */

bool
scalar_chain::build (bitmap candidates, unsigned insn_uid, bitmap disallowed)
{
  queue = BITMAP_ALLOC (NULL);
  bitmap_set_bit (queue, insn_uid);

  if (dump_file)
    fprintf (dump_file, "Building chain #%d...\n", chain_id);

  while (!bitmap_empty_p (queue))
    {
      insn_uid = bitmap_first_set_bit (queue);
      bitmap_clear_bit (queue, insn_uid);
      bitmap_clear_bit (candidates, insn_uid);
      if (!add_insn (candidates, insn_uid, disallowed))
	{
	  /* If we aborted the search put sofar found insn on the set of
	     disallowed insns so that further searches reaching them also
	     abort and thus we abort the whole but yet undiscovered chain.  */
	  bitmap_ior_into (disallowed, insns);
	  if (dump_file)
	    fprintf (dump_file, "Aborted chain #%d discovery\n", chain_id);
	  BITMAP_FREE (queue);
	  return false;
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "Collected chain #%d...\n", chain_id);
      fprintf (dump_file, "  insns: ");
      dump_bitmap (dump_file, insns);
      if (!bitmap_empty_p (defs_conv))
	{
	  bitmap_iterator bi;
	  unsigned id;
	  const char *comma = "";
	  fprintf (dump_file, "  defs to convert: ");
	  EXECUTE_IF_SET_IN_BITMAP (defs_conv, 0, id, bi)
	    {
	      fprintf (dump_file, "%sr%d", comma, id);
	      comma = ", ";
	    }
	  fprintf (dump_file, "\n");
	}
    }

  BITMAP_FREE (queue);

  return true;
}

/* Return a cost of building a vector costant
   instead of using a scalar one.  */

int
general_scalar_chain::vector_const_cost (rtx exp, basic_block bb)
{
  gcc_assert (CONST_INT_P (exp));

  if (standard_sse_constant_p (exp, vmode))
    return ix86_cost->sse_op;
  if (optimize_bb_for_size_p (bb))
    return COSTS_N_BYTES (8);
  /* We have separate costs for SImode and DImode, use SImode costs
     for smaller modes.  */
  return COSTS_N_INSNS (ix86_cost->sse_load[smode == DImode ? 1 : 0]) / 2;
}

/* Return true if it's cost profitable for chain conversion.  */

bool
general_scalar_chain::compute_convert_gain ()
{
  bitmap_iterator bi;
  unsigned insn_uid;
  int gain = 0;
  sreal weighted_gain = 0;

  if (dump_file)
    fprintf (dump_file, "Computing gain for chain #%d...\n", chain_id);

  /* SSE costs distinguish between SImode and DImode loads/stores, for
     int costs factor in the number of GPRs involved.  When supporting
     smaller modes than SImode the int load/store costs need to be
     adjusted as well.  */
  unsigned sse_cost_idx = smode == DImode ? 1 : 0;
  int m = smode == DImode ? (TARGET_64BIT ? 1 : 2) : 1;

  EXECUTE_IF_SET_IN_BITMAP (insns, 0, insn_uid, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (insn_uid)->insn;
      rtx def_set = single_set (insn);
      rtx src = SET_SRC (def_set);
      rtx dst = SET_DEST (def_set);
      basic_block bb = BLOCK_FOR_INSN (insn);
      int igain = 0;
      profile_count entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      bool speed_p = optimize_bb_for_speed_p (bb);
      sreal bb_freq = bb->count.to_sreal_scale (entry_count);

      if (REG_P (src) && REG_P (dst))
	{
	  if (!speed_p)
	    /* reg-reg move is 2 bytes, while SSE 3.  */
	    igain += COSTS_N_BYTES (2 * m - 3);
	  else
	    /* Move costs are normalized to reg-reg move having cost 2.  */
	    igain += COSTS_N_INSNS (2 * m - ix86_cost->xmm_move) / 2;
	}
      else if (REG_P (src) && MEM_P (dst))
	{
	  if (!speed_p)
	    /* Integer load/store is 3+ bytes and SSE 4+.  */
	    igain += COSTS_N_BYTES (3 * m - 4);
	  else
	    igain
	      += COSTS_N_INSNS (m * ix86_cost->int_store[2]
				- ix86_cost->sse_store[sse_cost_idx]) / 2;
	}
      else if (MEM_P (src) && REG_P (dst))
	{
	  if (!speed_p)
	    igain += COSTS_N_BYTES (3 * m - 4);
	  else
	    igain += COSTS_N_INSNS (m * ix86_cost->int_load[2]
				    - ix86_cost->sse_load[sse_cost_idx]) / 2;
	}
      else
	{
	  /* For operations on memory operands, include the overhead
	     of explicit load and store instructions.  */
	  if (MEM_P (dst))
	    {
	      if (!speed_p)
		/* ??? This probably should account size difference
		   of SSE and integer load rather than full SSE load.  */
		igain -= COSTS_N_BYTES (8);
	      else
		{
		  int cost = (m * (ix86_cost->int_load[2]
				   + ix86_cost->int_store[2])
			     - (ix86_cost->sse_load[sse_cost_idx] +
				ix86_cost->sse_store[sse_cost_idx]));
		  igain += COSTS_N_INSNS (cost) / 2;
		}
	    }

	  switch (GET_CODE (src))
	    {
	    case ASHIFT:
	    case ASHIFTRT:
	    case LSHIFTRT:
	      if (m == 2)
		{
		  if (INTVAL (XEXP (src, 1)) >= 32)
		    igain += ix86_cost->add;
		  /* Gain for extend highpart case.  */
		  else if (GET_CODE (XEXP (src, 0)) == ASHIFT)
		    igain += ix86_cost->shift_const - ix86_cost->sse_op;
		  else
		    igain += ix86_cost->shift_const;
		}

	      igain += ix86_cost->shift_const - ix86_cost->sse_op;

	      if (CONST_INT_P (XEXP (src, 0)))
		igain -= vector_const_cost (XEXP (src, 0), bb);
	      break;

	    case ROTATE:
	    case ROTATERT:
	      igain += m * ix86_cost->shift_const;
	      if (TARGET_AVX512VL)
		igain -= ix86_cost->sse_op;
	      else if (smode == DImode)
		{
		  int bits = INTVAL (XEXP (src, 1));
		  if ((bits & 0x0f) == 0)
		    igain -= ix86_cost->sse_op;
		  else if ((bits & 0x07) == 0)
		    igain -= 2 * ix86_cost->sse_op;
		  else
		    igain -= 3 * ix86_cost->sse_op;
		}
	      else if (INTVAL (XEXP (src, 1)) == 16)
		igain -= ix86_cost->sse_op;
	      else
		igain -= 2 * ix86_cost->sse_op;
	      break;

	    case AND:
	    case IOR:
	    case XOR:
	    case PLUS:
	    case MINUS:
	      igain += m * ix86_cost->add - ix86_cost->sse_op;
	      /* Additional gain for andnot for targets without BMI.  */
	      if (GET_CODE (XEXP (src, 0)) == NOT
		  && !TARGET_BMI)
		igain += m * ix86_cost->add;

	      if (CONST_INT_P (XEXP (src, 0)))
		igain -= vector_const_cost (XEXP (src, 0), bb);
	      if (CONST_INT_P (XEXP (src, 1)))
		igain -= vector_const_cost (XEXP (src, 1), bb);
	      if (MEM_P (XEXP (src, 1)))
		{
		  if (!speed_p)
		    igain -= COSTS_N_BYTES (m == 2 ? 3 : 5);
		  else
		    igain += COSTS_N_INSNS
			       (m * ix86_cost->int_load[2]
				 - ix86_cost->sse_load[sse_cost_idx]) / 2;
		}
	      break;

	    case NEG:
	    case NOT:
	      igain -= ix86_cost->sse_op + COSTS_N_INSNS (1);

	      if (GET_CODE (XEXP (src, 0)) != ABS)
		{
		  igain += m * ix86_cost->add;
		  break;
		}
	      /* FALLTHRU */

	    case ABS:
	    case SMAX:
	    case SMIN:
	    case UMAX:
	    case UMIN:
	      /* We do not have any conditional move cost, estimate it as a
		 reg-reg move.  Comparisons are costed as adds.  */
	      igain += m * (COSTS_N_INSNS (2) + ix86_cost->add);
	      /* Integer SSE ops are all costed the same.  */
	      igain -= ix86_cost->sse_op;
	      break;

	    case COMPARE:
	      if (XEXP (src, 1) != const0_rtx)
		{
		  /* cmp vs. pxor;pshufd;ptest.  */
		  igain += COSTS_N_INSNS (m - 3);
		}
	      else if (GET_CODE (XEXP (src, 0)) != AND)
		{
		  /* test vs. pshufd;ptest.  */
		  igain += COSTS_N_INSNS (m - 2);
		}
	      else if (GET_CODE (XEXP (XEXP (src, 0), 0)) != NOT)
		{
		  /* and;test vs. pshufd;ptest.  */
		  igain += COSTS_N_INSNS (2 * m - 2);
		}
	      else if (TARGET_BMI)
		{
		  /* andn;test vs. pandn;pshufd;ptest.  */
		  igain += COSTS_N_INSNS (2 * m - 3);
		}
	      else
		{
		  /* not;and;test vs. pandn;pshufd;ptest.  */
		  igain += COSTS_N_INSNS (3 * m - 3);
		}
	      break;

	    case CONST_INT:
	      if (REG_P (dst))
		{
		  if (!speed_p)
		    {
		      /* xor (2 bytes) vs. xorps (3 bytes).  */
		      if (src == const0_rtx)
			igain -= COSTS_N_BYTES (1);
		      /* movdi_internal vs. movv2di_internal.  */
		      /* => mov (5 bytes) vs. movaps (7 bytes).  */
		      else if (x86_64_immediate_operand (src, SImode))
			igain -= COSTS_N_BYTES (2);
		      else
			/* ??? Larger immediate constants are placed in the
			   constant pool, where the size benefit/impact of
			   STV conversion is affected by whether and how
			   often each constant pool entry is shared/reused.
			   The value below is empirically derived from the
			   CSiBE benchmark (and the optimal value may drift
			   over time).  */
			igain += COSTS_N_BYTES (0);
		    }
		  else
		    {
		      /* DImode can be immediate for TARGET_64BIT
			 and SImode always.  */
		      igain += m * COSTS_N_INSNS (1);
		      igain -= vector_const_cost (src, bb);
		    }
		}
	      else if (MEM_P (dst))
		{
		  igain += (m * ix86_cost->int_store[2]
			    - ix86_cost->sse_store[sse_cost_idx]);
		  igain -= vector_const_cost (src, bb);
		}
	      break;

	    case VEC_SELECT:
	      if (XVECEXP (XEXP (src, 1), 0, 0) == const0_rtx)
		{
		  // movd (4 bytes) replaced with movdqa (4 bytes).
		  if (!!speed_p)
		    igain += COSTS_N_INSNS (ix86_cost->sse_to_integer
					    - ix86_cost->xmm_move) / 2;
		}
	      else
		{
		  // pshufd; movd replaced with pshufd.
		  if (!speed_p)
		    igain += COSTS_N_BYTES (4);
		  else
		    igain += ix86_cost->sse_to_integer;
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      if (speed_p)
	weighted_gain += bb_freq * igain;
      gain += igain;

      if (igain != 0 && dump_file)
	{
	  fprintf (dump_file, "  Instruction gain %d with bb_freq %.2f for",
		   igain, bb_freq.to_double ());
	  dump_insn_slim (dump_file, insn);
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "  Instruction conversion gain: %d, \n",
	       gain);
      fprintf (dump_file, "  Registers conversion cost: %d\n",
	       cost_sse_integer);
      fprintf (dump_file, "  Weighted instruction conversion gain: %.2f, \n",
	       weighted_gain.to_double ());
      fprintf (dump_file, "  Weighted registers conversion cost: %.2f\n",
	       weighted_cost_sse_integer.to_double ());
    }

  if (weighted_gain != weighted_cost_sse_integer)
    return weighted_gain > weighted_cost_sse_integer;
  else
    return gain > cost_sse_integer;;
}

/* Insert generated conversion instruction sequence INSNS
   after instruction AFTER.  New BB may be required in case
   instruction has EH region attached.  */

void
scalar_chain::emit_conversion_insns (rtx insns, rtx_insn *after)
{
  if (!control_flow_insn_p (after))
    {
      emit_insn_after (insns, after);
      return;
    }

  basic_block bb = BLOCK_FOR_INSN (after);
  edge e = find_fallthru_edge (bb->succs);
  gcc_assert (e);

  basic_block new_bb = split_edge (e);
  emit_insn_after (insns, BB_HEAD (new_bb));
}

} // anon namespace

/* Generate the canonical SET_SRC to move GPR to a VMODE vector register,
   zeroing the upper parts.  */

static rtx
gen_gpr_to_xmm_move_src (enum machine_mode vmode, rtx gpr)
{
  switch (GET_MODE_NUNITS (vmode))
    {
    case 1:
      return gen_rtx_SUBREG (vmode, gpr, 0);
    case 2:
      return gen_rtx_VEC_CONCAT (vmode, gpr,
				 CONST0_RTX (GET_MODE_INNER (vmode)));
    default:
      return gen_rtx_VEC_MERGE (vmode, gen_rtx_VEC_DUPLICATE (vmode, gpr),
				CONST0_RTX (vmode), GEN_INT (HOST_WIDE_INT_1U));
    }
}

/* Make vector copies for all register REGNO definitions
   and replace its uses in a chain.  */

void
scalar_chain::make_vector_copies (rtx_insn *insn, rtx reg)
{
  rtx vreg = *defs_map.get (reg);

  start_sequence ();
  if (!TARGET_INTER_UNIT_MOVES_TO_VEC)
    {
      rtx tmp = assign_386_stack_local (smode, SLOT_STV_TEMP);
      if (smode == DImode && !TARGET_64BIT)
	{
	  emit_move_insn (adjust_address (tmp, SImode, 0),
			  gen_rtx_SUBREG (SImode, reg, 0));
	  emit_move_insn (adjust_address (tmp, SImode, 4),
			  gen_rtx_SUBREG (SImode, reg, 4));
	}
      else
	emit_move_insn (copy_rtx (tmp), reg);
      emit_insn (gen_rtx_SET (gen_rtx_SUBREG (vmode, vreg, 0),
			      gen_gpr_to_xmm_move_src (vmode, tmp)));
    }
  else if (!TARGET_64BIT && smode == DImode)
    {
      if (TARGET_SSE4_1)
	{
	  emit_insn (gen_sse2_loadld (gen_rtx_SUBREG (V4SImode, vreg, 0),
				      CONST0_RTX (V4SImode),
				      gen_rtx_SUBREG (SImode, reg, 0)));
	  emit_insn (gen_sse4_1_pinsrd (gen_rtx_SUBREG (V4SImode, vreg, 0),
					gen_rtx_SUBREG (V4SImode, vreg, 0),
					gen_rtx_SUBREG (SImode, reg, 4),
					GEN_INT (2)));
	}
      else
	{
	  rtx tmp = gen_reg_rtx (DImode);
	  emit_insn (gen_sse2_loadld (gen_rtx_SUBREG (V4SImode, vreg, 0),
				      CONST0_RTX (V4SImode),
				      gen_rtx_SUBREG (SImode, reg, 0)));
	  emit_insn (gen_sse2_loadld (gen_rtx_SUBREG (V4SImode, tmp, 0),
				      CONST0_RTX (V4SImode),
				      gen_rtx_SUBREG (SImode, reg, 4)));
	  emit_insn (gen_vec_interleave_lowv4si
		     (gen_rtx_SUBREG (V4SImode, vreg, 0),
		      gen_rtx_SUBREG (V4SImode, vreg, 0),
		      gen_rtx_SUBREG (V4SImode, tmp, 0)));
	}
    }
  else
    emit_insn (gen_rtx_SET (gen_rtx_SUBREG (vmode, vreg, 0),
			    gen_gpr_to_xmm_move_src (vmode, reg)));
  rtx_insn *seq = end_sequence ();
  emit_conversion_insns (seq, insn);

  if (dump_file)
    fprintf (dump_file,
	     "  Copied r%d to a vector register r%d for insn %d\n",
	     REGNO (reg), REGNO (vreg), INSN_UID (insn));
}

/* Copy the definition SRC of INSN inside the chain to DST for
   scalar uses outside of the chain.  */

void
scalar_chain::convert_reg (rtx_insn *insn, rtx dst, rtx src)
{
  start_sequence ();
  if (!TARGET_INTER_UNIT_MOVES_FROM_VEC)
    {
      rtx tmp = assign_386_stack_local (smode, SLOT_STV_TEMP);
      emit_move_insn (tmp, src);
      if (!TARGET_64BIT && smode == DImode)
	{
	  emit_move_insn (gen_rtx_SUBREG (SImode, dst, 0),
			  adjust_address (tmp, SImode, 0));
	  emit_move_insn (gen_rtx_SUBREG (SImode, dst, 4),
			  adjust_address (tmp, SImode, 4));
	}
      else
	emit_move_insn (dst, copy_rtx (tmp));
    }
  else if (!TARGET_64BIT && smode == DImode)
    {
      if (TARGET_SSE4_1)
	{
	  rtx tmp = gen_rtx_PARALLEL (VOIDmode,
				      gen_rtvec (1, const0_rtx));
	  emit_insn
	      (gen_rtx_SET
	       (gen_rtx_SUBREG (SImode, dst, 0),
		gen_rtx_VEC_SELECT (SImode,
				    gen_rtx_SUBREG (V4SImode, src, 0),
				    tmp)));

	  tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, const1_rtx));
	  emit_insn
	      (gen_rtx_SET
	       (gen_rtx_SUBREG (SImode, dst, 4),
		gen_rtx_VEC_SELECT (SImode,
				    gen_rtx_SUBREG (V4SImode, src, 0),
				    tmp)));
	}
      else
	{
	  rtx vcopy = gen_reg_rtx (V2DImode);
	  emit_move_insn (vcopy, gen_rtx_SUBREG (V2DImode, src, 0));
	  emit_move_insn (gen_rtx_SUBREG (SImode, dst, 0),
			  gen_rtx_SUBREG (SImode, vcopy, 0));
	  emit_move_insn (vcopy,
			  gen_rtx_LSHIFTRT (V2DImode,
					    vcopy, GEN_INT (32)));
	  emit_move_insn (gen_rtx_SUBREG (SImode, dst, 4),
			  gen_rtx_SUBREG (SImode, vcopy, 0));
	}
    }
  else
    emit_move_insn (dst, src);

  rtx_insn *seq = end_sequence ();
  emit_conversion_insns (seq, insn);

  if (dump_file)
    fprintf (dump_file,
	     "  Copied r%d to a scalar register r%d for insn %d\n",
	     REGNO (src), REGNO (dst), INSN_UID (insn));
}

/* Helper function to convert immediate constant X to vmode.  */
static rtx
smode_convert_cst (rtx x, enum machine_mode vmode)
{
  /* Prefer all ones vector in case of -1.  */
  if (constm1_operand (x, GET_MODE (x)))
    return CONSTM1_RTX (vmode);

  unsigned n = GET_MODE_NUNITS (vmode);
  rtx *v = XALLOCAVEC (rtx, n);
  v[0] = x;
  for (unsigned i = 1; i < n; ++i)
    v[i] = const0_rtx;
  return gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (n, v));
}

/* Convert operand OP in INSN.  We should handle
   memory operands and uninitialized registers.
   All other register uses are converted during
   registers conversion.  */

void
scalar_chain::convert_op (rtx *op, rtx_insn *insn)
{
  rtx tmp;

  if (GET_MODE (*op) == V1TImode)
    return;

  *op = copy_rtx_if_shared (*op);

  if (GET_CODE (*op) == NOT
      || GET_CODE (*op) == ASHIFT)
    {
      convert_op (&XEXP (*op, 0), insn);
      PUT_MODE (*op, vmode);
    }
  else if (MEM_P (*op))
    {
      rtx_insn *movabs = NULL;

      /* Emit MOVABS to load from a 64-bit absolute address to a GPR.  */
      if (!memory_operand (*op, GET_MODE (*op)))
	{
	  tmp = gen_reg_rtx (GET_MODE (*op));
	  movabs = emit_insn_before (gen_rtx_SET (tmp, *op), insn);

	  *op = tmp;
	}

      tmp = gen_rtx_SUBREG (vmode, gen_reg_rtx (GET_MODE (*op)), 0);

      rtx_insn *eh_insn
	= emit_insn_before (gen_rtx_SET (copy_rtx (tmp),
					 gen_gpr_to_xmm_move_src (vmode, *op)),
			    insn);

      if (cfun->can_throw_non_call_exceptions)
	{
	  /* Handle REG_EH_REGION note.  */
	  rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	  if (note)
	    {
	      if (movabs)
		eh_insn = movabs;
	      control_flow_insns.safe_push (eh_insn);
	      add_reg_note (eh_insn, REG_EH_REGION, XEXP (note, 0));
	    }
	}

      *op = tmp;

      if (dump_file)
	fprintf (dump_file, "  Preloading operand for insn %d into r%d\n",
		 INSN_UID (insn), reg_or_subregno (tmp));
    }
  else if (REG_P (*op))
    *op = gen_rtx_SUBREG (vmode, *op, 0);
  else if (CONST_SCALAR_INT_P (*op))
    {
      rtx vec_cst = smode_convert_cst (*op, vmode);

      if (!standard_sse_constant_p (vec_cst, vmode))
	{
	  start_sequence ();
	  vec_cst = validize_mem (force_const_mem (vmode, vec_cst));
	  rtx_insn *seq = end_sequence ();
	  emit_insn_before (seq, insn);
	}

      tmp = gen_rtx_SUBREG (vmode, gen_reg_rtx (smode), 0);

      emit_insn_before (gen_move_insn (copy_rtx (tmp), vec_cst), insn);
      *op = tmp;
    }
  else
    {
      gcc_assert (SUBREG_P (*op));
      gcc_assert (GET_MODE (*op) == vmode);
    }
}

/* Convert CCZmode COMPARE to vector mode.  */

rtx
scalar_chain::convert_compare (rtx op1, rtx op2, rtx_insn *insn)
{
  rtx src, tmp;

  /* Handle any REG_EQUAL notes.  */
  tmp = find_reg_equal_equiv_note (insn);
  if (tmp)
    {
      if (GET_CODE (XEXP (tmp, 0)) == COMPARE
	  && GET_MODE (XEXP (tmp, 0)) == CCZmode
	  && REG_P (XEXP (XEXP (tmp, 0), 0)))
	{
	  rtx *op = &XEXP (XEXP (tmp, 0), 1);
	  if (CONST_SCALAR_INT_P (*op))
	    {
	      if (constm1_operand (*op, GET_MODE (*op)))
		*op = CONSTM1_RTX (vmode);
	      else
		{
		  unsigned n = GET_MODE_NUNITS (vmode);
		  rtx *v = XALLOCAVEC (rtx, n);
		  v[0] = *op;
		  for (unsigned i = 1; i < n; ++i)
		    v[i] = const0_rtx;
		  *op = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (n, v));
		}
	      tmp = NULL_RTX;
	    }
	  else if (REG_P (*op))
	    tmp = NULL_RTX;
	}

      if (tmp)
	remove_note (insn, tmp);
    }

  /* Comparison against anything other than zero, requires an XOR.  */
  if (op2 != const0_rtx)
    {
      convert_op (&op1, insn);
      convert_op (&op2, insn);
      /* If both operands are MEMs, explicitly load the OP1 into TMP.  */
      if (MEM_P (op1) && MEM_P (op2))
	{
	  tmp = gen_reg_rtx (vmode);
	  emit_insn_before (gen_rtx_SET (tmp, op1), insn);
	  src = tmp;
	}
      else
	src = op1;
      src = gen_rtx_XOR (vmode, src, op2);
    }
  else if (GET_CODE (op1) == AND
	   && GET_CODE (XEXP (op1, 0)) == NOT)
    {
      rtx op11 = XEXP (XEXP (op1, 0), 0);
      rtx op12 = XEXP (op1, 1);
      convert_op (&op11, insn);
      convert_op (&op12, insn);
      if (!REG_P (op11))
	{
	  tmp = gen_reg_rtx (vmode);
	  emit_insn_before (gen_rtx_SET (tmp, op11), insn);
	  op11 = tmp;
	}
      src = gen_rtx_AND (vmode, gen_rtx_NOT (vmode, op11), op12);
    }
  else if (GET_CODE (op1) == AND)
    {
      rtx op11 = XEXP (op1, 0);
      rtx op12 = XEXP (op1, 1);
      convert_op (&op11, insn);
      convert_op (&op12, insn);
      if (!REG_P (op11))
	{
	  tmp = gen_reg_rtx (vmode);
	  emit_insn_before (gen_rtx_SET (tmp, op11), insn);
	  op11 = tmp;
	}
      return gen_rtx_UNSPEC (CCZmode, gen_rtvec (2, op11, op12),
			     UNSPEC_PTEST);
    }
  else
    {
      convert_op (&op1, insn);
      src = op1;
    }

  if (!REG_P (src))
    {
      tmp = gen_reg_rtx (vmode);
      emit_insn_before (gen_rtx_SET (tmp, src), insn);
      src = tmp;
    }

  if (vmode == V2DImode)
    {
      tmp = gen_reg_rtx (vmode);
      emit_insn_before (gen_vec_interleave_lowv2di (tmp, src, src), insn);
      src = tmp;
    }
  else if (vmode == V4SImode)
    {
      tmp = gen_reg_rtx (vmode);
      emit_insn_before (gen_sse2_pshufd (tmp, src, const0_rtx), insn);
      src = tmp;
    }

  return gen_rtx_UNSPEC (CCZmode, gen_rtvec (2, src, src), UNSPEC_PTEST);
}

/* Helper function for converting INSN to vector mode.  */

void
scalar_chain::convert_insn_common (rtx_insn *insn)
{
  /* Generate copies for out-of-chain uses of defs and adjust debug uses.  */
  for (df_ref ref = DF_INSN_DEFS (insn); ref; ref = DF_REF_NEXT_LOC (ref))
    if (bitmap_bit_p (defs_conv, DF_REF_REGNO (ref)))
      {
	df_link *use;
	for (use = DF_REF_CHAIN (ref); use; use = use->next)
	  if (NONDEBUG_INSN_P (DF_REF_INSN (use->ref))
	      && (DF_REF_REG_MEM_P (use->ref)
		  || !bitmap_bit_p (insns, DF_REF_INSN_UID (use->ref))))
	    break;
	if (use)
	  convert_reg (insn, DF_REF_REG (ref),
		       *defs_map.get (regno_reg_rtx [DF_REF_REGNO (ref)]));
	else if (MAY_HAVE_DEBUG_BIND_INSNS)
	  {
	    /* If we generated a scalar copy we can leave debug-insns
	       as-is, if not, we have to adjust them.  */
	    auto_vec<rtx_insn *, 5> to_reset_debug_insns;
	    for (use = DF_REF_CHAIN (ref); use; use = use->next)
	      if (DEBUG_INSN_P (DF_REF_INSN (use->ref)))
		{
		  rtx_insn *debug_insn = DF_REF_INSN (use->ref);
		  /* If there's a reaching definition outside of the
		     chain we have to reset.  */
		  df_link *def;
		  for (def = DF_REF_CHAIN (use->ref); def; def = def->next)
		    if (!bitmap_bit_p (insns, DF_REF_INSN_UID (def->ref)))
		      break;
		  if (def)
		    to_reset_debug_insns.safe_push (debug_insn);
		  else
		    {
		      *DF_REF_REAL_LOC (use->ref)
			= *defs_map.get (regno_reg_rtx [DF_REF_REGNO (ref)]);
		      df_insn_rescan (debug_insn);
		    }
		}
	    /* Have to do the reset outside of the DF_CHAIN walk to not
	       disrupt it.  */
	    while (!to_reset_debug_insns.is_empty ())
	      {
		rtx_insn *debug_insn = to_reset_debug_insns.pop ();
		INSN_VAR_LOCATION_LOC (debug_insn) = gen_rtx_UNKNOWN_VAR_LOC ();
		df_insn_rescan_debug_internal (debug_insn);
	      }
	  }
      }

  /* Replace uses in this insn with the defs we use in the chain.  */
  for (df_ref ref = DF_INSN_USES (insn); ref; ref = DF_REF_NEXT_LOC (ref))
    if (!DF_REF_REG_MEM_P (ref))
      if (rtx *vreg = defs_map.get (regno_reg_rtx[DF_REF_REGNO (ref)]))
	{
	  /* Also update a corresponding REG_DEAD note.  */
	  rtx note = find_reg_note (insn, REG_DEAD, DF_REF_REG (ref));
	  if (note)
	    XEXP (note, 0) = *vreg;
	  *DF_REF_REAL_LOC (ref) = *vreg;
	}
}

/* Convert INSN which is an SImode or DImode rotation by a constant
   to vector mode.  CODE is either ROTATE or ROTATERT with operands
   OP0 and OP1.  Returns the SET_SRC of the last instruction in the
   resulting sequence, which is emitted before INSN.  */

rtx
general_scalar_chain::convert_rotate (enum rtx_code code, rtx op0, rtx op1,
				      rtx_insn *insn)
{
  int bits = INTVAL (op1);
  rtx pat, result;

  convert_op (&op0, insn);
  if (bits == 0)
    return op0;

  if (smode == DImode)
    {
      if (code == ROTATE)
	bits = 64 - bits;
      if (bits == 32)
	{
	  rtx tmp1 = gen_reg_rtx (V4SImode);
	  pat = gen_sse2_pshufd (tmp1, gen_lowpart (V4SImode, op0),
				 GEN_INT (225));
	  emit_insn_before (pat, insn);
	  result = gen_lowpart (V2DImode, tmp1);
	}
      else if (TARGET_AVX512VL)
	result = simplify_gen_binary (code, V2DImode, op0, op1);
      else if (bits == 16 || bits == 48)
	{
	  rtx tmp1 = gen_reg_rtx (V8HImode);
	  pat = gen_sse2_pshuflw (tmp1, gen_lowpart (V8HImode, op0),
				  GEN_INT (bits == 16 ? 57 : 147));
	  emit_insn_before (pat, insn);
	  result = gen_lowpart (V2DImode, tmp1);
	}
      else if ((bits & 0x07) == 0)
	{
	  rtx tmp1 = gen_reg_rtx (V4SImode);
	  pat = gen_sse2_pshufd (tmp1, gen_lowpart (V4SImode, op0),
				 GEN_INT (68));
	  emit_insn_before (pat, insn);
	  rtx tmp2 = gen_reg_rtx (V1TImode);
	  pat = gen_sse2_lshrv1ti3 (tmp2, gen_lowpart (V1TImode, tmp1),
				    GEN_INT (bits));
	  emit_insn_before (pat, insn);
	  result = gen_lowpart (V2DImode, tmp2);
	}
      else
	{
	  rtx tmp1 = gen_reg_rtx (V4SImode);
	  pat = gen_sse2_pshufd (tmp1, gen_lowpart (V4SImode, op0),
				 GEN_INT (20));
	  emit_insn_before (pat, insn);
	  rtx tmp2 = gen_reg_rtx (V2DImode);
	  pat = gen_lshrv2di3 (tmp2, gen_lowpart (V2DImode, tmp1),
			       GEN_INT (bits & 31));
	  emit_insn_before (pat, insn);
	  rtx tmp3 = gen_reg_rtx (V4SImode);
	  pat = gen_sse2_pshufd (tmp3, gen_lowpart (V4SImode, tmp2),
				 GEN_INT (bits > 32 ? 34 : 136));
	  emit_insn_before (pat, insn);
	  result = gen_lowpart (V2DImode, tmp3);
	}
    }
  else if (bits == 16)
    {
      rtx tmp1 = gen_reg_rtx (V8HImode);
      pat = gen_sse2_pshuflw (tmp1, gen_lowpart (V8HImode, op0), GEN_INT (225));
      emit_insn_before (pat, insn);
      result = gen_lowpart (V4SImode, tmp1);
    }
  else if (TARGET_AVX512VL)
    result = simplify_gen_binary (code, V4SImode, op0, op1);
  else
    {
      if (code == ROTATE)
	bits = 32 - bits;

      rtx tmp1 = gen_reg_rtx (V4SImode);
      emit_insn_before (gen_sse2_pshufd (tmp1, op0, GEN_INT (224)), insn);
      rtx tmp2 = gen_reg_rtx (V2DImode);
      pat = gen_lshrv2di3 (tmp2, gen_lowpart (V2DImode, tmp1),
			   GEN_INT (bits));
      emit_insn_before (pat, insn);
      result = gen_lowpart (V4SImode, tmp2);
    }

  return result;
}

/* Convert INSN to vector mode.  */

void
general_scalar_chain::convert_insn (rtx_insn *insn)
{
  rtx def_set = single_set (insn);
  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);
  rtx subreg;

  if (MEM_P (dst) && !REG_P (src))
    {
      /* There are no scalar integer instructions and therefore
	 temporary register usage is required.  */
      rtx tmp = gen_reg_rtx (smode);
      emit_conversion_insns (gen_move_insn (dst, tmp), insn);
      dst = gen_rtx_SUBREG (vmode, tmp, 0);
    }
  else if (REG_P (dst) && GET_MODE (dst) == smode)
    {
      /* Replace the definition with a SUBREG to the definition we
	 use inside the chain.  */
      rtx *vdef = defs_map.get (dst);
      if (vdef)
	dst = *vdef;
      dst = gen_rtx_SUBREG (vmode, dst, 0);
      /* IRA doesn't like to have REG_EQUAL/EQUIV notes when the SET_DEST
	 is a non-REG_P.  So kill those off.  */
      rtx note = find_reg_equal_equiv_note (insn);
      if (note)
	remove_note (insn, note);
    }

  switch (GET_CODE (src))
    {
    case PLUS:
    case MINUS:
    case IOR:
    case XOR:
    case AND:
    case SMAX:
    case SMIN:
    case UMAX:
    case UMIN:
      convert_op (&XEXP (src, 1), insn);
      /* FALLTHRU */

    case ABS:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      convert_op (&XEXP (src, 0), insn);
      PUT_MODE (src, vmode);
      break;

    case ROTATE:
    case ROTATERT:
      src = convert_rotate (GET_CODE (src), XEXP (src, 0), XEXP (src, 1),
			    insn);
      break;

    case NEG:
      src = XEXP (src, 0);

      if (GET_CODE (src) == ABS)
	{
	  src = XEXP (src, 0);
	  convert_op (&src, insn);
	  subreg = gen_reg_rtx (vmode);
	  emit_insn_before (gen_rtx_SET (subreg,
					 gen_rtx_ABS (vmode, src)), insn);
	  src = subreg;
	}
      else
	convert_op (&src, insn);

      subreg = gen_reg_rtx (vmode);
      emit_insn_before (gen_move_insn (subreg, CONST0_RTX (vmode)), insn);
      src = gen_rtx_MINUS (vmode, subreg, src);
      break;

    case NOT:
      src = XEXP (src, 0);
      convert_op (&src, insn);
      subreg = gen_reg_rtx (vmode);
      emit_insn_before (gen_move_insn (subreg, CONSTM1_RTX (vmode)), insn);
      src = gen_rtx_XOR (vmode, src, subreg);
      break;

    case MEM:
      if (!REG_P (dst))
	convert_op (&src, insn);
      break;

    case REG:
      if (!MEM_P (dst))
	convert_op (&src, insn);
      break;

    case SUBREG:
      gcc_assert (GET_MODE (src) == vmode);
      break;

    case COMPARE:
      dst = gen_rtx_REG (CCZmode, FLAGS_REG);
      src = convert_compare (XEXP (src, 0), XEXP (src, 1), insn);
      break;

    case CONST_INT:
      convert_op (&src, insn);
      break;

    case VEC_SELECT:
      if (XVECEXP (XEXP (src, 1), 0, 0) == const0_rtx)
	src = XEXP (src, 0);
      else if (smode == DImode)
	{
	  rtx tmp = gen_lowpart (V1TImode, XEXP (src, 0));
	  dst = gen_lowpart (V1TImode, dst);
	  src = gen_rtx_LSHIFTRT (V1TImode, tmp, GEN_INT (64));
	}
      else
	{
	  rtx tmp = XVECEXP (XEXP (src, 1), 0, 0);
	  rtvec vec = gen_rtvec (4, tmp, tmp, tmp, tmp);
	  rtx par = gen_rtx_PARALLEL (VOIDmode, vec);
	  src = gen_rtx_VEC_SELECT (vmode, XEXP (src, 0), par);
	}
      break;

    default:
      gcc_unreachable ();
    }

  SET_SRC (def_set) = src;
  SET_DEST (def_set) = dst;

  /* Drop possible dead definitions.  */
  PATTERN (insn) = def_set;

  INSN_CODE (insn) = -1;
  int patt = recog_memoized (insn);
  if  (patt == -1)
    fatal_insn_not_found (insn);
  df_insn_rescan (insn);
}

/* Helper function to compute gain for loading an immediate constant.
   Typically, two movabsq for TImode vs. vmovdqa for V1TImode, but
   with numerous special cases.  */

static int
timode_immed_const_gain (rtx cst, basic_block bb)
{
  /* movabsq vs. movabsq+vmovq+vunpacklqdq.  */
  if (CONST_WIDE_INT_P (cst)
      && CONST_WIDE_INT_NUNITS (cst) == 2
      && CONST_WIDE_INT_ELT (cst, 0) == CONST_WIDE_INT_ELT (cst, 1))
    return optimize_bb_for_size_p (bb) ? -COSTS_N_BYTES (9)
				       : -COSTS_N_INSNS (2);
  /* 2x movabsq ~ vmovdqa.  */
  return 0;
}

/* Return true it's cost profitable for for chain conversion.  */

bool
timode_scalar_chain::compute_convert_gain ()
{
  /* Assume that if we have to move TImode values between units,
     then transforming this chain isn't worth it.  */
  if (cost_sse_integer)
    return false;

  bitmap_iterator bi;
  unsigned insn_uid;

  /* Split ties to prefer V1TImode when not optimizing for size.  */
  int gain = optimize_size ? 0 : 1;
  sreal weighted_gain  = 0;

  if (dump_file)
    fprintf (dump_file, "Computing gain for chain #%d...\n", chain_id);

  EXECUTE_IF_SET_IN_BITMAP (insns, 0, insn_uid, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (insn_uid)->insn;
      rtx def_set = single_set (insn);
      rtx src = SET_SRC (def_set);
      rtx dst = SET_DEST (def_set);
      HOST_WIDE_INT op1val;
      basic_block bb = BLOCK_FOR_INSN (insn);
      int scost, vcost;
      int igain = 0;
      profile_count entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      bool speed_p = optimize_bb_for_speed_p (bb);
      sreal bb_freq = bb->count.to_sreal_scale (entry_count);

      switch (GET_CODE (src))
	{
	case REG:
	  if (!speed_p)
	    igain = MEM_P (dst) ? COSTS_N_BYTES (6) : COSTS_N_BYTES (3);
	  else
	    igain = COSTS_N_INSNS (1);
	  break;

	case MEM:
	  igain = !speed_p ? COSTS_N_BYTES (7) : COSTS_N_INSNS (1);
	  break;

	case CONST_INT:
	  if (MEM_P (dst)
	      && standard_sse_constant_p (src, V1TImode))
	    igain = !speed_p ? COSTS_N_BYTES (11) : 1;
	  break;

	case CONST_WIDE_INT:
	  /* 2 x mov vs. vmovdqa.  */
	  if (MEM_P (dst))
	    igain = !speed_p ? COSTS_N_BYTES (3) : COSTS_N_INSNS (1);
	  break;

	case NOT:
	  if (MEM_P (dst))
	    igain = -COSTS_N_INSNS (1);
	  break;

	case AND:
	case XOR:
	case IOR:
	  if (!MEM_P (dst))
	    igain = COSTS_N_INSNS (1);
	  if (CONST_SCALAR_INT_P (XEXP (src, 1)))
	    igain += timode_immed_const_gain (XEXP (src, 1), bb);
	  break;

	case ASHIFT:
	case LSHIFTRT:
	  /* See ix86_expand_v1ti_shift.  */
	  op1val = INTVAL (XEXP (src, 1));
	  if (!speed_p)
	    {
	      if (op1val == 64 || op1val == 65)
		scost = COSTS_N_BYTES (5);
	      else if (op1val >= 66)
		scost = COSTS_N_BYTES (6);
	      else if (op1val == 1)
		scost = COSTS_N_BYTES (8);
	      else
		scost = COSTS_N_BYTES (9);

	      if ((op1val & 7) == 0)
		vcost = COSTS_N_BYTES (5);
	      else if (op1val > 64)
		vcost = COSTS_N_BYTES (10);
	      else
		vcost = TARGET_AVX ? COSTS_N_BYTES (19) : COSTS_N_BYTES (23);
	    }
	  else
	    {
	      scost = COSTS_N_INSNS (2);
	      if ((op1val & 7) == 0)
		vcost = COSTS_N_INSNS (1);
	      else if (op1val > 64)
		vcost = COSTS_N_INSNS (2);
	      else
		vcost = TARGET_AVX ? COSTS_N_INSNS (4) : COSTS_N_INSNS (5);
	    }
	  igain = scost - vcost;
	  break;

	case ASHIFTRT:
	  /* See ix86_expand_v1ti_ashiftrt.  */
	  op1val = INTVAL (XEXP (src, 1));
	  if (!speed_p)
	    {
	      if (op1val == 64 || op1val == 127)
		scost = COSTS_N_BYTES (7);
	      else if (op1val == 1)
		scost = COSTS_N_BYTES (8);
	      else if (op1val == 65)
		scost = COSTS_N_BYTES (10);
	      else if (op1val >= 66)
		scost = COSTS_N_BYTES (11);
	      else
		scost = COSTS_N_BYTES (9);

	      if (op1val == 127)
		vcost = COSTS_N_BYTES (10);
	      else if (op1val == 64)
		vcost = COSTS_N_BYTES (14);
	      else if (op1val == 96)
		vcost = COSTS_N_BYTES (18);
	      else if (op1val >= 111)
		vcost = COSTS_N_BYTES (15);
	      else if (TARGET_AVX2 && op1val == 32)
		vcost = COSTS_N_BYTES (16);
	      else if (TARGET_SSE4_1 && op1val == 32)
		vcost = COSTS_N_BYTES (20);
	      else if (op1val >= 96)
		vcost = COSTS_N_BYTES (23);
	      else if ((op1val & 7) == 0)
		vcost = COSTS_N_BYTES (28);
	      else if (TARGET_AVX2 && op1val < 32)
		vcost = COSTS_N_BYTES (30);
	      else if (op1val == 1 || op1val >= 64)
		vcost = COSTS_N_BYTES (42);
	      else
		vcost = COSTS_N_BYTES (47);
	    }
	  else
	    {
	      if (op1val >= 65 && op1val <= 126)
		scost = COSTS_N_INSNS (3);
	      else
		scost = COSTS_N_INSNS (2);

	      if (op1val == 127)
		vcost = COSTS_N_INSNS (2);
	      else if (op1val == 64)
		vcost = COSTS_N_INSNS (3);
	      else if (op1val == 96)
		vcost = COSTS_N_INSNS (3);
	      else if (op1val >= 111)
		vcost = COSTS_N_INSNS (3);
	      else if (TARGET_SSE4_1 && op1val == 32)
		vcost = COSTS_N_INSNS (3);
	      else if (TARGET_SSE4_1
		       && (op1val == 8 || op1val == 16 || op1val == 24))
		vcost = COSTS_N_INSNS (3);
	      else if (op1val >= 96)
		vcost = COSTS_N_INSNS (4);
	      else if (TARGET_SSE4_1 && (op1val == 28 || op1val == 80))
		vcost = COSTS_N_INSNS (4);
	      else if ((op1val & 7) == 0)
		vcost = COSTS_N_INSNS (5);
	      else if (TARGET_AVX2 && op1val < 32)
		vcost = COSTS_N_INSNS (6);
	      else if (TARGET_SSE4_1 && op1val < 15)
		vcost = COSTS_N_INSNS (6);
	      else if (op1val == 1 || op1val >= 64)
		vcost = COSTS_N_INSNS (8);
	      else
		vcost = COSTS_N_INSNS (9);
	    }
	  igain = scost - vcost;
	  break;

	case ROTATE:
	case ROTATERT:
	  /* See ix86_expand_v1ti_rotate.  */
	  op1val = INTVAL (XEXP (src, 1));
	  if (!speed_p)
	    {
	      scost = COSTS_N_BYTES (13);
	      if ((op1val & 31) == 0)
		vcost = COSTS_N_BYTES (5);
	      else if ((op1val & 7) == 0)
		vcost = TARGET_AVX ? COSTS_N_BYTES (13) : COSTS_N_BYTES (18);
	      else if (op1val > 32 && op1val < 96)
		vcost = COSTS_N_BYTES (24);
	      else
		vcost = COSTS_N_BYTES (19);
	    }
	  else
	    {
	      scost = COSTS_N_INSNS (3);
	      if ((op1val & 31) == 0)
		vcost = COSTS_N_INSNS (1);
	      else if ((op1val & 7) == 0)
		vcost = TARGET_AVX ? COSTS_N_INSNS (3) : COSTS_N_INSNS (4);
	      else if (op1val > 32 && op1val < 96)
		vcost = COSTS_N_INSNS (5);
	      else
		vcost = COSTS_N_INSNS (1);
	    }
	  igain = scost - vcost;
	  break;

	case COMPARE:
	  if (XEXP (src, 1) == const0_rtx)
	    {
	      if (GET_CODE (XEXP (src, 0)) == AND)
		/* and;and;or (9 bytes) vs. ptest (5 bytes).  */
		igain = !speed_p ? COSTS_N_BYTES (4) : COSTS_N_INSNS (2);
	      /* or (3 bytes) vs. ptest (5 bytes).  */
	      else if (!speed_p)
		igain = -COSTS_N_BYTES (2);
	    }
	  else if (XEXP (src, 1) == const1_rtx)
	    /* and;cmp -1 (7 bytes) vs. pcmpeqd;pxor;ptest (13 bytes).  */
	    igain = !speed_p ? -COSTS_N_BYTES (6) : -COSTS_N_INSNS (1);
	  break;

	default:
	  break;
	}

      gain += igain;
      if (speed_p)
	weighted_gain += bb_freq * igain;

      if (igain != 0 && dump_file)
	{
	  fprintf (dump_file, "  Instruction gain %d with bb_freq %.2f for ",
		   igain, bb_freq.to_double ());
	  dump_insn_slim (dump_file, insn);
	}
    }

  if (dump_file)
    fprintf (dump_file, "  Total gain: %d, weighted gain %.2f\n",
	     gain, weighted_gain.to_double ());

  if (weighted_gain > (sreal) 0)
    return true;
  else
    return gain > 0;
}

/* Fix uses of converted REG in debug insns.  */

void
timode_scalar_chain::fix_debug_reg_uses (rtx reg)
{
  if (!flag_var_tracking)
    return;

  df_ref ref, next;
  for (ref = DF_REG_USE_CHAIN (REGNO (reg)); ref; ref = next)
    {
      rtx_insn *insn = DF_REF_INSN (ref);
      /* Make sure the next ref is for a different instruction,
	 so that we're not affected by the rescan.  */
      next = DF_REF_NEXT_REG (ref);
      while (next && DF_REF_INSN (next) == insn)
	next = DF_REF_NEXT_REG (next);

      if (DEBUG_INSN_P (insn))
	{
	  /* It may be a debug insn with a TImode variable in
	     register.  */
	  bool changed = false;
	  for (; ref != next; ref = DF_REF_NEXT_REG (ref))
	    {
	      rtx *loc = DF_REF_LOC (ref);
	      if (REG_P (*loc) && GET_MODE (*loc) == V1TImode)
		{
		  *loc = gen_rtx_SUBREG (TImode, *loc, 0);
		  changed = true;
		}
	    }
	  if (changed)
	    df_insn_rescan (insn);
	}
    }
}

/* Convert INSN from TImode to V1T1mode.  */

void
timode_scalar_chain::convert_insn (rtx_insn *insn)
{
  rtx def_set = single_set (insn);
  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);
  rtx tmp;

  switch (GET_CODE (dst))
    {
    case REG:
      if (GET_MODE (dst) == TImode)
	{
	  PUT_MODE (dst, V1TImode);
	  fix_debug_reg_uses (dst);
	}
      if (GET_MODE (dst) == V1TImode)
	{
	  /* It might potentially be helpful to convert REG_EQUAL notes,
	     but for now we just remove them.  */
	  rtx note = find_reg_equal_equiv_note (insn);
	  if (note)
	    remove_note (insn, note);
	}
      break;
    case MEM:
      PUT_MODE (dst, V1TImode);
      break;

    default:
      gcc_unreachable ();
    }

  switch (GET_CODE (src))
    {
    case REG:
      if (GET_MODE (src) == TImode)
	{
	  PUT_MODE (src, V1TImode);
	  fix_debug_reg_uses (src);
	}
      break;

    case MEM:
      PUT_MODE (src, V1TImode);
      break;

    case CONST_WIDE_INT:
      if (NONDEBUG_INSN_P (insn))
	{
	  /* Since there are no instructions to store 128-bit constant,
	     temporary register usage is required.  */
	  bool use_move;
	  start_sequence ();
	  tmp = ix86_convert_const_wide_int_to_broadcast (TImode, src);
	  if (tmp)
	    {
	      src = lowpart_subreg (V1TImode, tmp, TImode);
	      use_move = true;
	    }
	  else
	    {
	      src = smode_convert_cst (src, V1TImode);
	      src = validize_mem (force_const_mem (V1TImode, src));
	      use_move = MEM_P (dst);
	    }
	  rtx_insn *seq = end_sequence ();
	  if (seq)
	    emit_insn_before (seq, insn);
	  if (use_move)
	    {
	      tmp = gen_reg_rtx (V1TImode);
	      emit_insn_before (gen_rtx_SET (tmp, src), insn);
	      src = tmp;
	    }
	}
      break;

    case CONST_INT:
      switch (standard_sse_constant_p (src, TImode))
	{
	case 1:
	  src = CONST0_RTX (GET_MODE (dst));
	  break;
	case 2:
	  src = CONSTM1_RTX (GET_MODE (dst));
	  break;
	default:
	  gcc_unreachable ();
	}
      if (MEM_P (dst))
	{
	  tmp = gen_reg_rtx (V1TImode);
	  emit_insn_before (gen_rtx_SET (tmp, src), insn);
	  src = tmp;
	}
      break;

    case AND:
      if (GET_CODE (XEXP (src, 0)) == NOT)
	{
	  convert_op (&XEXP (XEXP (src, 0), 0), insn);
	  convert_op (&XEXP (src, 1), insn);
	  PUT_MODE (XEXP (src, 0), V1TImode);
	  PUT_MODE (src, V1TImode);
	  break;
	}
      /* FALLTHRU */

    case XOR:
    case IOR:
      convert_op (&XEXP (src, 0), insn);
      convert_op (&XEXP (src, 1), insn);
      PUT_MODE (src, V1TImode);
      if (MEM_P (dst))
	{
	  tmp = gen_reg_rtx (V1TImode);
	  emit_insn_before (gen_rtx_SET (tmp, src), insn);
	  src = tmp;
	}
      break;

    case NOT:
      src = XEXP (src, 0);
      convert_op (&src, insn);
      tmp = gen_reg_rtx (V1TImode);
      emit_insn_before (gen_move_insn (tmp, CONSTM1_RTX (V1TImode)), insn);
      src = gen_rtx_XOR (V1TImode, src, tmp);
      if (MEM_P (dst))
	{
	  tmp = gen_reg_rtx (V1TImode);
	  emit_insn_before (gen_rtx_SET (tmp, src), insn);
	  src = tmp;
	}
      break;

    case COMPARE:
      dst = gen_rtx_REG (CCZmode, FLAGS_REG);
      src = convert_compare (XEXP (src, 0), XEXP (src, 1), insn);
      break;

    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATERT:
    case ROTATE:
      convert_op (&XEXP (src, 0), insn);
      PUT_MODE (src, V1TImode);
      break;

    default:
      gcc_unreachable ();
    }

  SET_SRC (def_set) = src;
  SET_DEST (def_set) = dst;

  /* Drop possible dead definitions.  */
  PATTERN (insn) = def_set;

  INSN_CODE (insn) = -1;
  recog_memoized (insn);
  df_insn_rescan (insn);
}

/* Generate copies from defs used by the chain but not defined therein.
   Also populates defs_map which is used later by convert_insn.  */

void
scalar_chain::convert_registers ()
{
  bitmap_iterator bi;
  unsigned id;
  EXECUTE_IF_SET_IN_BITMAP (defs_conv, 0, id, bi)
    {
      rtx chain_reg = gen_reg_rtx (smode);
      defs_map.put (regno_reg_rtx[id], chain_reg);
    }
  EXECUTE_IF_SET_IN_BITMAP (insns_conv, 0, id, bi)
    for (df_ref ref = DF_INSN_UID_DEFS (id); ref; ref = DF_REF_NEXT_LOC (ref))
      if (bitmap_bit_p (defs_conv, DF_REF_REGNO (ref)))
	make_vector_copies (DF_REF_INSN (ref), DF_REF_REAL_REG (ref));
}

/* Convert whole chain creating required register
   conversions and copies.  */

int
scalar_chain::convert ()
{
  bitmap_iterator bi;
  unsigned id;
  int converted_insns = 0;

  if (!dbg_cnt (stv_conversion))
    return 0;

  if (dump_file)
    fprintf (dump_file, "Converting chain #%d...\n", chain_id);

  convert_registers ();

  EXECUTE_IF_SET_IN_BITMAP (insns, 0, id, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (id)->insn;
      convert_insn_common (insn);
      convert_insn (insn);
      converted_insns++;
    }

  return converted_insns;
}

/* Return the SET expression if INSN doesn't reference hard register.
   Return NULL if INSN uses or defines a hard register, excluding
   pseudo register pushes, hard register uses in a memory address,
   clobbers and flags definitions.  */

static rtx
pseudo_reg_set (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return NULL;

  /* Check pseudo register push first. */
  machine_mode mode = TARGET_64BIT ? TImode : DImode;
  if (REG_P (SET_SRC (set))
      && !HARD_REGISTER_P (SET_SRC (set))
      && push_operand (SET_DEST (set), mode))
    return set;

  df_ref ref;
  FOR_EACH_INSN_DEF (ref, insn)
    if (HARD_REGISTER_P (DF_REF_REAL_REG (ref))
	&& !DF_REF_FLAGS_IS_SET (ref, DF_REF_MUST_CLOBBER)
	&& DF_REF_REGNO (ref) != FLAGS_REG)
      return NULL;

  FOR_EACH_INSN_USE (ref, insn)
    if (!DF_REF_REG_MEM_P (ref) && HARD_REGISTER_P (DF_REF_REAL_REG (ref)))
      return NULL;

  return set;
}

/* Return true if the register REG is defined in a single DEF chain.
   If it is defined in more than one DEF chains, we may not be able
   to convert it in all chains.  */

static bool
single_def_chain_p (rtx reg)
{
  df_ref ref = DF_REG_DEF_CHAIN (REGNO (reg));
  if (!ref)
    return false;
  return DF_REF_NEXT_REG (ref) == nullptr;
}

/* Check if comparison INSN may be transformed into vector comparison.
   Currently we transform equality/inequality checks which look like:
   (set (reg:CCZ 17 flags) (compare:CCZ (reg:TI x) (reg:TI y)))  */

static bool
convertible_comparison_p (rtx_insn *insn, enum machine_mode mode)
{
  if (mode != (TARGET_64BIT ? TImode : DImode))
    return false;

  if (!TARGET_SSE4_1)
    return false;

  rtx def_set = single_set (insn);

  gcc_assert (def_set);

  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);

  gcc_assert (GET_CODE (src) == COMPARE);

  if (!REG_P (dst)
      || REGNO (dst) != FLAGS_REG
      || GET_MODE (dst) != CCZmode)
    return false;

  rtx op1 = XEXP (src, 0);
  rtx op2 = XEXP (src, 1);

  /* *cmp<dwi>_doubleword.  */
  if ((CONST_SCALAR_INT_P (op1)
       || ((REG_P (op1) || MEM_P (op1))
	   && GET_MODE (op1) == mode))
      && (CONST_SCALAR_INT_P (op2)
	  || ((REG_P (op2) || MEM_P (op2))
	      && GET_MODE (op2) == mode)))
    return true;

  /* *testti_doubleword.  */
  if (op2 == const0_rtx
      && GET_CODE (op1) == AND
      && REG_P (XEXP (op1, 0)))
    {
      rtx op12 = XEXP (op1, 1);
      return GET_MODE (XEXP (op1, 0)) == TImode
	     && (CONST_SCALAR_INT_P (op12)
		 || ((REG_P (op12) || MEM_P (op12))
		     && GET_MODE (op12) == TImode));
    }

  /* *test<dwi>_not_doubleword.  */
  if (op2 == const0_rtx
      && GET_CODE (op1) == AND
      && GET_CODE (XEXP (op1, 0)) == NOT)
    {
      rtx op11 = XEXP (XEXP (op1, 0), 0);
      rtx op12 = XEXP (op1, 1);
      return (REG_P (op11) || MEM_P (op11))
	     && (REG_P (op12) || MEM_P (op12))
	     && GET_MODE (op11) == mode
	     && GET_MODE (op12) == mode;
    }

  return false;
}

/* The general version of scalar_to_vector_candidate_p.  */

static bool
general_scalar_to_vector_candidate_p (rtx_insn *insn, enum machine_mode mode)
{
  rtx def_set = pseudo_reg_set (insn);

  if (!def_set)
    return false;

  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);

  if (GET_CODE (src) == COMPARE)
    return convertible_comparison_p (insn, mode);

  /* We are interested in "mode" only.  */
  if ((GET_MODE (src) != mode
       && !CONST_INT_P (src))
      || GET_MODE (dst) != mode)
    return false;

  if (!REG_P (dst) && !MEM_P (dst))
    return false;

  switch (GET_CODE (src))
    {
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATE:
    case ROTATERT:
      if (!CONST_INT_P (XEXP (src, 1))
	  || !IN_RANGE (INTVAL (XEXP (src, 1)), 0, GET_MODE_BITSIZE (mode)-1))
	return false;

      /* Check for extend highpart case.  */
      if (mode != DImode
	  || GET_CODE (src) != ASHIFTRT
	  || GET_CODE (XEXP (src, 0)) != ASHIFT)
	break;

      src = XEXP (src, 0);
      break;

    case SMAX:
    case SMIN:
    case UMAX:
    case UMIN:
      if ((mode == DImode && !TARGET_AVX512VL)
	  || (mode == SImode && !TARGET_SSE4_1))
	return false;
      /* Fallthru.  */

    case AND:
    case IOR:
    case XOR:
    case PLUS:
    case MINUS:
      if (!REG_P (XEXP (src, 1))
	  && !MEM_P (XEXP (src, 1))
	  && !CONST_INT_P (XEXP (src, 1)))
	return false;

      if (GET_MODE (XEXP (src, 1)) != mode
	  && !CONST_INT_P (XEXP (src, 1)))
	return false;

      /* Check for andnot case.  */
      if (GET_CODE (src) != AND
	  || GET_CODE (XEXP (src, 0)) != NOT)
	break;

      src = XEXP (src, 0);
      /* FALLTHRU */

    case NOT:
      break;

    case NEG:
      /* Check for nabs case.  */
      if (GET_CODE (XEXP (src, 0)) != ABS)
	break;

      src = XEXP (src, 0);
      /* FALLTHRU */

    case ABS:
      if ((mode == DImode && !TARGET_AVX512VL)
	  || (mode == SImode && !TARGET_SSSE3))
	return false;
      break;

    case REG:
      return true;

    case MEM:
    case CONST_INT:
      return REG_P (dst);

    case VEC_SELECT:
      /* Excluding MEM_P (dst) avoids intefering with vpextr[dq].  */
      return REG_P (dst)
	     && REG_P (XEXP (src, 0))
	     && GET_MODE (XEXP (src, 0)) == (mode == DImode ? V2DImode
							    : V4SImode)
	     && GET_CODE (XEXP (src, 1)) == PARALLEL
	     && XVECLEN (XEXP (src, 1), 0) == 1
	     && CONST_INT_P (XVECEXP (XEXP (src, 1), 0, 0));

    default:
      return false;
    }

  if (!REG_P (XEXP (src, 0))
      && !MEM_P (XEXP (src, 0))
      && !CONST_INT_P (XEXP (src, 0)))
    return false;

  if (GET_MODE (XEXP (src, 0)) != mode
      && !CONST_INT_P (XEXP (src, 0)))
    return false;

  return true;
}

/* Check for a suitable TImode memory operand.  */

static bool
timode_mem_p (rtx x)
{
  return MEM_P (x)
	 && (TARGET_SSE_UNALIGNED_LOAD_OPTIMAL
	     || !misaligned_operand (x, TImode));
}

/* The TImode version of scalar_to_vector_candidate_p.  */

static bool
timode_scalar_to_vector_candidate_p (rtx_insn *insn)
{
  rtx def_set = pseudo_reg_set (insn);

  if (!def_set)
    return false;

  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);

  if (GET_CODE (src) == COMPARE)
    return convertible_comparison_p (insn, TImode);

  if (GET_MODE (dst) != TImode
      || (GET_MODE (src) != TImode
	  && !CONST_SCALAR_INT_P (src)))
    return false;

  if (!REG_P (dst) && !MEM_P (dst))
    return false;

  if (MEM_P (dst)
      && misaligned_operand (dst, TImode)
      && !TARGET_SSE_UNALIGNED_STORE_OPTIMAL)
    return false;

  if (REG_P (dst) && !single_def_chain_p (dst))
    return false;

  switch (GET_CODE (src))
    {
    case REG:
      return single_def_chain_p (src);

    case CONST_WIDE_INT:
      return true;

    case CONST_INT:
      /* ??? Verify performance impact before enabling CONST_INT for
	 __int128 store.  */
      return standard_sse_constant_p (src, TImode);

    case MEM:
      /* Memory must be aligned or unaligned load is optimal.  */
      return (REG_P (dst)
	      && (!misaligned_operand (src, TImode)
		  || TARGET_SSE_UNALIGNED_LOAD_OPTIMAL));

    case AND:
      if (!MEM_P (dst)
	  && GET_CODE (XEXP (src, 0)) == NOT
	  && REG_P (XEXP (XEXP (src, 0), 0))
	  && (REG_P (XEXP (src, 1))
	      || CONST_SCALAR_INT_P (XEXP (src, 1))
	      || timode_mem_p (XEXP (src, 1))))
	return true;
      return (REG_P (XEXP (src, 0))
	      || timode_mem_p (XEXP (src, 0)))
	     && (REG_P (XEXP (src, 1))
		 || CONST_SCALAR_INT_P (XEXP (src, 1))
		 || timode_mem_p (XEXP (src, 1)));

    case IOR:
    case XOR:
      return (REG_P (XEXP (src, 0))
	      || timode_mem_p (XEXP (src, 0)))
	     && (REG_P (XEXP (src, 1))
		 || CONST_SCALAR_INT_P (XEXP (src, 1))
		 || timode_mem_p (XEXP (src, 1)));

    case NOT:
      return REG_P (XEXP (src, 0)) || timode_mem_p (XEXP (src, 0));

    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATERT:
    case ROTATE:
      /* Handle shifts/rotates by integer constants between 0 and 127.  */
      return REG_P (XEXP (src, 0))
	     && CONST_INT_P (XEXP (src, 1))
	     && (INTVAL (XEXP (src, 1)) & ~0x7f) == 0;

    default:
      return false;
    }
}

/* For a register REGNO, scan instructions for its defs and uses.
   Put REGNO in REGS if a def or use isn't in CANDIDATES.  */

static void
timode_check_non_convertible_regs (bitmap candidates, bitmap regs,
				   unsigned int regno)
{
  /* Do nothing if REGNO is already in REGS or is a hard reg.  */
  if (bitmap_bit_p (regs, regno)
      || HARD_REGISTER_NUM_P (regno))
    return;

  for (df_ref def = DF_REG_DEF_CHAIN (regno);
       def;
       def = DF_REF_NEXT_REG (def))
    {
      if (!bitmap_bit_p (candidates, DF_REF_INSN_UID (def)))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "r%d has non convertible def in insn %d\n",
		     regno, DF_REF_INSN_UID (def));

	  bitmap_set_bit (regs, regno);
	  break;
	}
    }

  for (df_ref ref = DF_REG_USE_CHAIN (regno);
       ref;
       ref = DF_REF_NEXT_REG (ref))
    {
      /* Debug instructions are skipped.  */
      if (NONDEBUG_INSN_P (DF_REF_INSN (ref))
	  && !bitmap_bit_p (candidates, DF_REF_INSN_UID (ref)))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "r%d has non convertible use in insn %d\n",
		     regno, DF_REF_INSN_UID (ref));

	  bitmap_set_bit (regs, regno);
	  break;
	}
    }
}

/* For a given bitmap of insn UIDs scans all instructions and
   remove insn from CANDIDATES in case it has both convertible
   and not convertible definitions.

   All insns in a bitmap are conversion candidates according to
   scalar_to_vector_candidate_p.  Currently it implies all insns
   are single_set.  */

static void
timode_remove_non_convertible_regs (bitmap candidates)
{
  bitmap_iterator bi;
  unsigned id;
  bitmap regs = BITMAP_ALLOC (NULL);
  bool changed;

  do {
    changed = false;
    EXECUTE_IF_SET_IN_BITMAP (candidates, 0, id, bi)
      {
	rtx_insn *insn = DF_INSN_UID_GET (id)->insn;
	df_ref ref;

	FOR_EACH_INSN_DEF (ref, insn)
	  if (!DF_REF_REG_MEM_P (ref)
	      && GET_MODE (DF_REF_REG (ref)) == TImode)
	    timode_check_non_convertible_regs (candidates, regs,
					       DF_REF_REGNO (ref));

	FOR_EACH_INSN_USE (ref, insn)
	  if (!DF_REF_REG_MEM_P (ref)
	      && GET_MODE (DF_REF_REG (ref)) == TImode)
	    timode_check_non_convertible_regs (candidates, regs,
					       DF_REF_REGNO (ref));
      }

    EXECUTE_IF_SET_IN_BITMAP (regs, 0, id, bi)
      {
	for (df_ref def = DF_REG_DEF_CHAIN (id);
	     def;
	     def = DF_REF_NEXT_REG (def))
	  if (bitmap_bit_p (candidates, DF_REF_INSN_UID (def)))
	    {
	      if (dump_file)
		fprintf (dump_file, "Removing insn %d from candidates list\n",
			 DF_REF_INSN_UID (def));

	      bitmap_clear_bit (candidates, DF_REF_INSN_UID (def));
	      changed = true;
	    }

	for (df_ref ref = DF_REG_USE_CHAIN (id);
	     ref;
	     ref = DF_REF_NEXT_REG (ref))
	  if (bitmap_bit_p (candidates, DF_REF_INSN_UID (ref)))
	    {
	      if (dump_file)
		fprintf (dump_file, "Removing insn %d from candidates list\n",
			 DF_REF_INSN_UID (ref));

	      bitmap_clear_bit (candidates, DF_REF_INSN_UID (ref));
	      changed = true;
	    }
      }
  } while (changed);

  BITMAP_FREE (regs);
}

/* Main STV pass function.  Find and convert scalar
   instructions into vector mode when profitable.  */

static unsigned int
convert_scalars_to_vector (bool timode_p)
{
  basic_block bb;
  int converted_insns = 0;
  auto_vec<rtx_insn *> control_flow_insns;

  bitmap_obstack_initialize (NULL);
  const machine_mode cand_mode[3] = { SImode, DImode, TImode };
  const machine_mode cand_vmode[3] = { V4SImode, V2DImode, V1TImode };
  bitmap_head candidates[3];  /* { SImode, DImode, TImode } */
  for (unsigned i = 0; i < 3; ++i)
    bitmap_initialize (&candidates[i], &bitmap_default_obstack);

  calculate_dominance_info (CDI_DOMINATORS);
  df_set_flags (DF_DEFER_INSN_RESCAN | DF_RD_PRUNE_DEAD_DEFS);
  df_chain_add_problem (DF_DU_CHAIN | DF_UD_CHAIN);
  df_analyze ();

  /* Find all instructions we want to convert into vector mode.  */
  if (dump_file)
    fprintf (dump_file, "Searching for mode conversion candidates...\n");

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;
      FOR_BB_INSNS (bb, insn)
	if (timode_p
	    && timode_scalar_to_vector_candidate_p (insn))
	  {
	    if (dump_file)
	      fprintf (dump_file, "  insn %d is marked as a TImode candidate\n",
		       INSN_UID (insn));

	    bitmap_set_bit (&candidates[2], INSN_UID (insn));
	  }
	else if (!timode_p)
	  {
	    /* Check {SI,DI}mode.  */
	    for (unsigned i = 0; i <= 1; ++i)
	      if (general_scalar_to_vector_candidate_p (insn, cand_mode[i]))
		{
		  if (dump_file)
		    fprintf (dump_file, "  insn %d is marked as a %s candidate\n",
			     INSN_UID (insn), i == 0 ? "SImode" : "DImode");

		  bitmap_set_bit (&candidates[i], INSN_UID (insn));
		  break;
		}
	  }
    }

  if (timode_p)
    timode_remove_non_convertible_regs (&candidates[2]);

  for (unsigned i = 0; i <= 2; ++i)
    if (!bitmap_empty_p (&candidates[i]))
      break;
    else if (i == 2 && dump_file)
      fprintf (dump_file, "There are no candidates for optimization.\n");

  for (unsigned i = 0; i <= 2; ++i)
    {
      auto_bitmap disallowed;
      bitmap_tree_view (&candidates[i]);
      while (!bitmap_empty_p (&candidates[i]))
	{
	  unsigned uid = bitmap_first_set_bit (&candidates[i]);
	  scalar_chain *chain;

	  if (cand_mode[i] == TImode)
	    chain = new timode_scalar_chain;
	  else
	    chain = new general_scalar_chain (cand_mode[i], cand_vmode[i]);

	  /* Find instructions chain we want to convert to vector mode.
	     Check all uses and definitions to estimate all required
	     conversions.  */
	  if (chain->build (&candidates[i], uid, disallowed))
	    {
	      if (chain->compute_convert_gain ())
		converted_insns += chain->convert ();
	      else if (dump_file)
		fprintf (dump_file, "Chain #%d conversion is not profitable\n",
			 chain->chain_id);
	    }

	  rtx_insn* iter_insn;
	  unsigned int ii;
	  FOR_EACH_VEC_ELT (chain->control_flow_insns, ii, iter_insn)
	    control_flow_insns.safe_push (iter_insn);

	  delete chain;
	}
    }

  if (dump_file)
    fprintf (dump_file, "Total insns converted: %d\n", converted_insns);

  for (unsigned i = 0; i <= 2; ++i)
    bitmap_release (&candidates[i]);
  bitmap_obstack_release (NULL);
  df_process_deferred_rescans ();

  /* Conversion means we may have 128bit register spills/fills
     which require aligned stack.  */
  if (converted_insns)
    {
      if (crtl->stack_alignment_needed < 128)
	crtl->stack_alignment_needed = 128;
      if (crtl->stack_alignment_estimated < 128)
	crtl->stack_alignment_estimated = 128;

      crtl->stack_realign_needed
	= INCOMING_STACK_BOUNDARY < crtl->stack_alignment_estimated;
      crtl->stack_realign_tried = crtl->stack_realign_needed;

      crtl->stack_realign_processed = true;

      if (!crtl->drap_reg)
	{
	  rtx drap_rtx = targetm.calls.get_drap_rtx ();

	  /* stack_realign_drap and drap_rtx must match.  */
	  gcc_assert ((stack_realign_drap != 0) == (drap_rtx != NULL));

	  /* Do nothing if NULL is returned,
	     which means DRAP is not needed.  */
	  if (drap_rtx != NULL)
	    {
	      crtl->args.internal_arg_pointer = drap_rtx;

	      /* Call fixup_tail_calls to clean up
		 REG_EQUIV note if DRAP is needed. */
	      fixup_tail_calls ();
	    }
	}

      /* Fix up DECL_RTL/DECL_INCOMING_RTL of arguments.  */
      if (TARGET_64BIT)
	for (tree parm = DECL_ARGUMENTS (current_function_decl);
	     parm; parm = DECL_CHAIN (parm))
	  {
	    if (TYPE_MODE (TREE_TYPE (parm)) != TImode)
	      continue;
	    if (DECL_RTL_SET_P (parm)
		&& GET_MODE (DECL_RTL (parm)) == V1TImode)
	      {
		rtx r = DECL_RTL (parm);
		if (REG_P (r))
		  SET_DECL_RTL (parm, gen_rtx_SUBREG (TImode, r, 0));
	      }
	    if (DECL_INCOMING_RTL (parm)
		&& GET_MODE (DECL_INCOMING_RTL (parm)) == V1TImode)
	      {
		rtx r = DECL_INCOMING_RTL (parm);
		if (REG_P (r))
		  DECL_INCOMING_RTL (parm) = gen_rtx_SUBREG (TImode, r, 0);
	      }
	  }

      if (!control_flow_insns.is_empty ())
	{
	  free_dominance_info (CDI_DOMINATORS);

	  unsigned int i;
	  rtx_insn* insn;
	  FOR_EACH_VEC_ELT (control_flow_insns, i, insn)
	    if (control_flow_insn_p (insn))
	      {
		/* Split the block after insn.  There will be a fallthru
		   edge, which is OK so we keep it.  We have to create
		   the exception edges ourselves.  */
		bb = BLOCK_FOR_INSN (insn);
		split_block (bb, insn);
		rtl_make_eh_edge (NULL, bb, BB_END (bb));
	      }
	}
    }

  return 0;
}

static unsigned int
rest_of_handle_insert_vzeroupper (void)
{
  /* vzeroupper instructions are inserted immediately after reload and
     postreload_cse to clean up after it a little bit to account for possible
     spills from 256bit or 512bit registers.  The pass reuses mode switching
     infrastructure by re-running mode insertion pass, so disable entities
     that have already been processed.  */
  for (int i = 0; i < MAX_386_ENTITIES; i++)
    ix86_optimize_mode_switching[i] = 0;

  ix86_optimize_mode_switching[AVX_U128] = 1;

  /* Call optimize_mode_switching.  */
  g->get_passes ()->execute_pass_mode_switching ();

  /* LRA removes all REG_DEAD/REG_UNUSED notes and normally they
     reappear in the IL only at the start of pass_rtl_dse2, which does
     df_note_add_problem (); df_analyze ();
     The vzeroupper is scheduled after postreload_cse pass and mode
     switching computes the notes as well, the problem is that e.g.
     pass_gcse2 doesn't maintain the notes, see PR113059 and
     PR112760.  Remove the notes now to restore status quo ante
     until we figure out how to maintain the notes or what else
     to do.  */
  basic_block bb;
  rtx_insn *insn;
  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      if (NONDEBUG_INSN_P (insn))
	{
	  rtx *pnote = &REG_NOTES (insn);
	  while (*pnote != 0)
	    {
	      if (REG_NOTE_KIND (*pnote) == REG_DEAD
		  || REG_NOTE_KIND (*pnote) == REG_UNUSED)
		*pnote = XEXP (*pnote, 1);
	      else
		pnote = &XEXP (*pnote, 1);
	    }
	}

  df_remove_problem (df_note);
  df_analyze ();
  return 0;
}

namespace {

const pass_data pass_data_insert_vzeroupper =
{
  RTL_PASS, /* type */
  "vzeroupper", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_insert_vzeroupper : public rtl_opt_pass
{
public:
  pass_insert_vzeroupper(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_insert_vzeroupper, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return TARGET_AVX && TARGET_VZEROUPPER;
    }

  unsigned int execute (function *) final override
    {
      return rest_of_handle_insert_vzeroupper ();
    }

}; // class pass_insert_vzeroupper

const pass_data pass_data_stv =
{
  RTL_PASS, /* type */
  "stv", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_stv : public rtl_opt_pass
{
public:
  pass_stv (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_stv, ctxt),
      timode_p (false)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return ((!timode_p || TARGET_64BIT)
	      && TARGET_STV && TARGET_SSE2 && optimize > 1);
    }

  unsigned int execute (function *) final override
    {
      return convert_scalars_to_vector (timode_p);
    }

  opt_pass *clone () final override
    {
      return new pass_stv (m_ctxt);
    }

  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      timode_p = param;
    }

private:
  bool timode_p;
}; // class pass_stv

} // anon namespace

rtl_opt_pass *
make_pass_insert_vzeroupper (gcc::context *ctxt)
{
  return new pass_insert_vzeroupper (ctxt);
}

rtl_opt_pass *
make_pass_stv (gcc::context *ctxt)
{
  return new pass_stv (ctxt);
}

/* Inserting ENDBR and pseudo patchable-area instructions.  */

static void
rest_of_insert_endbr_and_patchable_area (bool need_endbr,
					 unsigned int patchable_area_size)
{
  rtx endbr;
  rtx_insn *insn;
  rtx_insn *endbr_insn = NULL;
  basic_block bb;

  if (need_endbr)
    {
      /* Currently emit EB if it's a tracking function, i.e. 'nocf_check'
	 is absent among function attributes.  Later an optimization will
	 be introduced to make analysis if an address of a static function
	 is taken.  A static function whose address is not taken will get
	 a nocf_check attribute.  This will allow to reduce the number of
	 EB.  */
      if (!lookup_attribute ("nocf_check",
			     TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl)))
	  && (!flag_manual_endbr
	      || lookup_attribute ("cf_check",
				   DECL_ATTRIBUTES (cfun->decl)))
	  && (!cgraph_node::get (cfun->decl)->only_called_directly_p ()
	      || ix86_cmodel == CM_LARGE
	      || ix86_cmodel == CM_LARGE_PIC
	      || flag_force_indirect_call
	      || (TARGET_DLLIMPORT_DECL_ATTRIBUTES
		  && DECL_DLLIMPORT_P (cfun->decl))))
	{
	  if (crtl->profile && flag_fentry)
	    {
	      /* Queue ENDBR insertion to x86_function_profiler.
		 NB: Any patchable-area insn will be inserted after
		 ENDBR.  */
	      cfun->machine->insn_queued_at_entrance = TYPE_ENDBR;
	    }
	  else
	    {
	      endbr = gen_nop_endbr ();
	      bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
	      rtx_insn *insn = BB_HEAD (bb);
	      endbr_insn = emit_insn_before (endbr, insn);
	    }
	}
    }

  if (patchable_area_size)
    {
      if (crtl->profile && flag_fentry)
	{
	  /* Queue patchable-area insertion to x86_function_profiler.
	     NB: If there is a queued ENDBR, x86_function_profiler
	     will also handle patchable-area.  */
	  if (!cfun->machine->insn_queued_at_entrance)
	    cfun->machine->insn_queued_at_entrance = TYPE_PATCHABLE_AREA;
	}
      else
	{
	  rtx patchable_area
	    = gen_patchable_area (GEN_INT (patchable_area_size),
				  GEN_INT (crtl->patch_area_entry == 0));
	  if (endbr_insn)
	    emit_insn_after (patchable_area, endbr_insn);
	  else
	    {
	      bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
	      insn = BB_HEAD (bb);
	      emit_insn_before (patchable_area, insn);
	    }
	}
    }

  if (!need_endbr)
    return;

  bb = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (CALL_P (insn))
	    {
	      need_endbr = find_reg_note (insn, REG_SETJMP, NULL) != NULL;
	      if (!need_endbr && !SIBLING_CALL_P (insn))
		{
		  rtx call = get_call_rtx_from (insn);
		  rtx fnaddr = XEXP (call, 0);
		  tree fndecl = NULL_TREE;

		  /* Also generate ENDBRANCH for non-tail call which
		     may return via indirect branch.  */
		  if (SYMBOL_REF_P (XEXP (fnaddr, 0)))
		    fndecl = SYMBOL_REF_DECL (XEXP (fnaddr, 0));
		  if (fndecl == NULL_TREE)
		    fndecl = MEM_EXPR (fnaddr);
		  if (fndecl
		      && TREE_CODE (TREE_TYPE (fndecl)) != FUNCTION_TYPE
		      && TREE_CODE (TREE_TYPE (fndecl)) != METHOD_TYPE)
		    fndecl = NULL_TREE;
		  if (fndecl && TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
		    {
		      tree fntype = TREE_TYPE (fndecl);
		      if (lookup_attribute ("indirect_return",
					    TYPE_ATTRIBUTES (fntype)))
			need_endbr = true;
		    }
		}
	      if (!need_endbr)
		continue;
	      /* Generate ENDBRANCH after CALL, which can return more than
		 twice, setjmp-like functions.  */

	      endbr = gen_nop_endbr ();
	      emit_insn_after_setloc (endbr, insn, INSN_LOCATION (insn));
	      continue;
	    }

	  if (JUMP_P (insn) && flag_cet_switch)
	    {
	      rtx target = JUMP_LABEL (insn);
	      if (target == NULL_RTX || ANY_RETURN_P (target))
		continue;

	      /* Check the jump is a switch table.  */
	      rtx_insn *label = as_a<rtx_insn *> (target);
	      rtx_insn *table = next_insn (label);
	      if (table == NULL_RTX || !JUMP_TABLE_DATA_P (table))
		continue;

	      /* For the indirect jump find out all places it jumps and insert
		 ENDBRANCH there.  It should be done under a special flag to
		 control ENDBRANCH generation for switch stmts.  */
	      edge_iterator ei;
	      edge e;
	      basic_block dest_blk;

	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  rtx_insn *insn;

		  dest_blk = e->dest;
		  insn = BB_HEAD (dest_blk);
		  gcc_assert (LABEL_P (insn));
		  endbr = gen_nop_endbr ();
		  emit_insn_after (endbr, insn);
		}
	      continue;
	    }

	  if (LABEL_P (insn) && LABEL_PRESERVE_P (insn))
	    {
	      endbr = gen_nop_endbr ();
	      emit_insn_after (endbr, insn);
	      continue;
	    }
	}
    }

  return;
}

namespace {

const pass_data pass_data_insert_endbr_and_patchable_area =
{
  RTL_PASS, /* type.  */
  "endbr_and_patchable_area", /* name.  */
  OPTGROUP_NONE, /* optinfo_flags.  */
  TV_MACH_DEP, /* tv_id.  */
  0, /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  0, /* todo_flags_finish.  */
};

class pass_insert_endbr_and_patchable_area : public rtl_opt_pass
{
public:
  pass_insert_endbr_and_patchable_area (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_insert_endbr_and_patchable_area, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      need_endbr = (flag_cf_protection & CF_BRANCH) != 0;
      patchable_area_size = crtl->patch_area_size - crtl->patch_area_entry;
      return need_endbr || patchable_area_size;
    }

  unsigned int execute (function *) final override
    {
      timevar_push (TV_MACH_DEP);
      rest_of_insert_endbr_and_patchable_area (need_endbr,
					       patchable_area_size);
      timevar_pop (TV_MACH_DEP);
      return 0;
    }

private:
  bool need_endbr;
  unsigned int patchable_area_size;
}; // class pass_insert_endbr_and_patchable_area

} // anon namespace

rtl_opt_pass *
make_pass_insert_endbr_and_patchable_area (gcc::context *ctxt)
{
  return new pass_insert_endbr_and_patchable_area (ctxt);
}

bool
ix86_rpad_gate ()
{
  return (TARGET_AVX
	  && TARGET_SSE_PARTIAL_REG_DEPENDENCY
	  && TARGET_SSE_MATH
	  && optimize
	  && optimize_function_for_speed_p (cfun));
}

enum x86_cse_kind
{
  X86_CSE_CONST0_VECTOR,
  X86_CSE_CONSTM1_VECTOR,
  X86_CSE_VEC_DUP,
  X86_CSE_TLS_GD,
  X86_CSE_TLS_LD_BASE,
  X86_CSE_TLSDESC
};

struct redundant_pattern
{
  /* Bitmap of basic blocks with broadcast instructions.  */
  auto_bitmap bbs;
  /* Bitmap of broadcast instructions.  */
  auto_bitmap insns;
  /* The broadcast inner scalar.  */
  rtx val;
  /* The actual redundant source value for UNSPEC_TLSDESC.  */
  rtx tlsdesc_val;
  /* The inner scalar mode.  */
  machine_mode mode;
  /* The instruction which sets the inner scalar.  Nullptr if the inner
     scalar is applied to the whole function, instead of within the same
     block.  */
  rtx_insn *def_insn;
  /* The widest broadcast source.  */
  rtx broadcast_source;
  /* The widest broadcast register.  */
  rtx broadcast_reg;
  /* The basic block of the broadcast instruction.  */
  basic_block bb;
  /* The number of broadcast instructions with the same inner scalar.  */
  unsigned HOST_WIDE_INT count;
  /* The threshold of broadcast instructions with the same inner
     scalar.  */
  unsigned int threshold;
  /* The widest broadcast size in bytes.  */
  unsigned int size;
  /* Load kind.  */
  x86_cse_kind kind;
};

/* Generate a vector set, DEST = SRC, at entry of the nearest dominator
   for basic block map BBS, which is in the fake loop that contains the
   whole function, so that there is only a single vector set in the
   whole function.  If not nullptr, LOAD is a pointer to the load.  */

static void
ix86_place_single_vector_set (rtx dest, rtx src, bitmap bbs,
			      redundant_pattern *load = nullptr)
{
  basic_block bb = nearest_common_dominator_for_set (CDI_DOMINATORS, bbs);
  /* For X86_CSE_VEC_DUP, don't place the vector set outside of the loop
     to avoid extra spills.  */
  if (!load || load->kind != X86_CSE_VEC_DUP)
    {
      while (bb->loop_father->latch
	     != EXIT_BLOCK_PTR_FOR_FN (cfun))
	bb = get_immediate_dominator (CDI_DOMINATORS,
				      bb->loop_father->header);
    }

  rtx set = gen_rtx_SET (dest, src);

  rtx_insn *insn = BB_HEAD (bb);
  while (insn && !NONDEBUG_INSN_P (insn))
    {
      if (insn == BB_END (bb))
	{
	  insn = NULL;
	  break;
	}
      insn = NEXT_INSN (insn);
    }

  rtx_insn *set_insn;
  if (insn == BB_HEAD (bb))
    {
      set_insn = emit_insn_before (set, insn);
      if (dump_file)
	{
	  fprintf (dump_file, "\nPlace:\n\n");
	  print_rtl_single (dump_file, set_insn);
	  fprintf (dump_file, "\nbefore:\n\n");
	  print_rtl_single (dump_file, insn);
	  fprintf (dump_file, "\n");
	}
    }
  else
    {
      rtx_insn *after = insn ? PREV_INSN (insn) : BB_END (bb);
      set_insn = emit_insn_after (set, after);
      if (dump_file)
	{
	  fprintf (dump_file, "\nPlace:\n\n");
	  print_rtl_single (dump_file, set_insn);
	  fprintf (dump_file, "\nafter:\n\n");
	  print_rtl_single (dump_file, after);
	  fprintf (dump_file, "\n");
	}
    }

  if (load && load->kind == X86_CSE_VEC_DUP)
    {
      /* Get the source from LOAD as (reg:SI 99) in

	 (vec_duplicate:V4SI (reg:SI 99))

       */
      rtx inner_scalar = load->val;
      /* Set the source in (vec_duplicate:V4SI (reg:SI 99)).  */
      rtx reg = XEXP (src, 0);
      if ((REG_P (inner_scalar) || MEM_P (inner_scalar))
	  && GET_MODE (reg) != GET_MODE (inner_scalar))
	inner_scalar = gen_rtx_SUBREG (GET_MODE (reg), inner_scalar, 0);
      rtx set = gen_rtx_SET (reg, inner_scalar);
      insn = emit_insn_before (set, set_insn);
      if (dump_file)
	{
	  fprintf (dump_file, "\nAdd:\n\n");
	  print_rtl_single (dump_file, insn);
	  fprintf (dump_file, "\nbefore:\n\n");
	  print_rtl_single (dump_file, set_insn);
	  fprintf (dump_file, "\n");
	}
    }
}

/* At entry of the nearest common dominator for basic blocks with
   conversions/rcp/sqrt/rsqrt/round, generate a single
	vxorps %xmmN, %xmmN, %xmmN
   for all
	vcvtss2sd  op, %xmmN, %xmmX
	vcvtsd2ss  op, %xmmN, %xmmX
	vcvtsi2ss  op, %xmmN, %xmmX
	vcvtsi2sd  op, %xmmN, %xmmX

   NB: We want to generate only a single vxorps to cover the whole
   function.  The LCM algorithm isn't appropriate here since it may
   place a vxorps inside the loop.  */

static unsigned int
remove_partial_avx_dependency (void)
{
  timevar_push (TV_MACH_DEP);

  bitmap_obstack_initialize (NULL);
  bitmap convert_bbs = BITMAP_ALLOC (NULL);

  basic_block bb;
  rtx_insn *insn, *set_insn;
  rtx set;
  rtx v4sf_const0 = NULL_RTX;

  auto_vec<rtx_insn *> control_flow_insns;

  /* We create invalid RTL initially so defer rescans.  */
  df_set_flags (DF_DEFER_INSN_RESCAN);

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  set = single_set (insn);
	  if (!set)
	    continue;

	  if (get_attr_avx_partial_xmm_update (insn)
	      != AVX_PARTIAL_XMM_UPDATE_TRUE)
	    continue;

	  /* Convert PARTIAL_XMM_UPDATE_TRUE insns, DF -> SF, SF -> DF,
	     SI -> SF, SI -> DF, DI -> SF, DI -> DF, sqrt, rsqrt, rcp,
	     round, to vec_dup and vec_merge with subreg.  */
	  rtx src = SET_SRC (set);
	  rtx dest = SET_DEST (set);
	  machine_mode dest_mode = GET_MODE (dest);
	  bool convert_p = false;
	  switch (GET_CODE (src))
	    {
	    case FLOAT:
	    case FLOAT_EXTEND:
	    case FLOAT_TRUNCATE:
	    case UNSIGNED_FLOAT:
	      convert_p = true;
	      break;
	    default:
	      break;
	    }

	  /* Only handle conversion here.  */
	  machine_mode src_mode
	    = convert_p ? GET_MODE (XEXP (src, 0)) : VOIDmode;
	  switch (src_mode)
	    {
	    case E_SFmode:
	    case E_DFmode:
	      if (TARGET_USE_VECTOR_FP_CONVERTS
		  || !TARGET_SSE_PARTIAL_REG_FP_CONVERTS_DEPENDENCY)
		continue;
	      break;
	    case E_SImode:
	    case E_DImode:
	      if (TARGET_USE_VECTOR_CONVERTS
		  || !TARGET_SSE_PARTIAL_REG_CONVERTS_DEPENDENCY)
		continue;
	      break;
	    case E_VOIDmode:
	      gcc_assert (!convert_p);
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  if (!v4sf_const0)
	    v4sf_const0 = gen_reg_rtx (V4SFmode);

	  rtx zero;
	  machine_mode dest_vecmode;
	  switch (dest_mode)
	    {
	    case E_HFmode:
	      dest_vecmode = V8HFmode;
	      zero = gen_rtx_SUBREG (V8HFmode, v4sf_const0, 0);
	      break;
	    case E_SFmode:
	      dest_vecmode = V4SFmode;
	      zero = v4sf_const0;
	      break;
	    case E_DFmode:
	      dest_vecmode = V2DFmode;
	      zero = gen_rtx_SUBREG (V2DFmode, v4sf_const0, 0);
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  /* Change source to vector mode.  */
	  src = gen_rtx_VEC_DUPLICATE (dest_vecmode, src);
	  src = gen_rtx_VEC_MERGE (dest_vecmode, src, zero,
				   GEN_INT (HOST_WIDE_INT_1U));
	  /* Change destination to vector mode.  */
	  rtx vec = gen_reg_rtx (dest_vecmode);
	  /* Generate an XMM vector SET.  */
	  set = gen_rtx_SET (vec, src);
	  set_insn = emit_insn_before (set, insn);

	  if (cfun->can_throw_non_call_exceptions)
	    {
	      /* Handle REG_EH_REGION note.  */
	      rtx note = find_reg_note (insn, REG_EH_REGION, NULL_RTX);
	      if (note)
		{
		  control_flow_insns.safe_push (set_insn);
		  add_reg_note (set_insn, REG_EH_REGION, XEXP (note, 0));
		}
	    }

	  src = gen_rtx_SUBREG (dest_mode, vec, 0);
	  set = gen_rtx_SET (dest, src);

	  /* Drop possible dead definitions.  */
	  PATTERN (insn) = set;

	  INSN_CODE (insn) = -1;
	  recog_memoized (insn);
	  df_insn_rescan (insn);
	  bitmap_set_bit (convert_bbs, bb->index);
	}
    }

  if (v4sf_const0)
    {
      /* (Re-)discover loops so that bb->loop_father can be used in the
	 analysis below.  */
      calculate_dominance_info (CDI_DOMINATORS);
      loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

      ix86_place_single_vector_set (v4sf_const0,
				    CONST0_RTX (V4SFmode),
				    convert_bbs);

      loop_optimizer_finalize ();

      if (!control_flow_insns.is_empty ())
	{
	  free_dominance_info (CDI_DOMINATORS);

	  unsigned int i;
	  FOR_EACH_VEC_ELT (control_flow_insns, i, insn)
	    if (control_flow_insn_p (insn))
	      {
		/* Split the block after insn.  There will be a fallthru
		   edge, which is OK so we keep it.  We have to create
		   the exception edges ourselves.  */
		bb = BLOCK_FOR_INSN (insn);
		split_block (bb, insn);
		rtl_make_eh_edge (NULL, bb, BB_END (bb));
	      }
	}
    }

  df_process_deferred_rescans ();
  df_clear_flags (DF_DEFER_INSN_RESCAN);
  bitmap_obstack_release (NULL);
  BITMAP_FREE (convert_bbs);

  timevar_pop (TV_MACH_DEP);
  return 0;
}

namespace {

const pass_data pass_data_remove_partial_avx_dependency =
{
  RTL_PASS, /* type */
  "rpad", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_remove_partial_avx_dependency : public rtl_opt_pass
{
public:
  pass_remove_partial_avx_dependency (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_remove_partial_avx_dependency, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return ix86_rpad_gate ();
    }

  unsigned int execute (function *) final override
    {
      return remove_partial_avx_dependency ();
    }
}; // class pass_rpad

} // anon namespace

rtl_opt_pass *
make_pass_remove_partial_avx_dependency (gcc::context *ctxt)
{
  return new pass_remove_partial_avx_dependency (ctxt);
}

/* Return a machine mode suitable for vector SIZE with SMODE inner
   mode.  */

static machine_mode
ix86_get_vector_cse_mode (unsigned int size, machine_mode smode)
{
  /* Use the inner scalar mode of vector broadcast source in:

     (set (reg:V8DF 394)
	  (vec_duplicate:V8DF (reg:V2DF 190 [ alpha ])))

     to compute the vector mode for broadcast from vector source.
   */
  if (VECTOR_MODE_P (smode))
    smode = GET_MODE_INNER (smode);
  scalar_mode s_mode = as_a <scalar_mode> (smode);
  poly_uint64 nunits = size / GET_MODE_SIZE (smode);
  machine_mode mode = mode_for_vector (s_mode, nunits).require ();
  return mode;
}

/* Replace the source operand of instructions in VECTOR_INSNS with
   VECTOR_CONST in VECTOR_MODE.  */

static void
replace_vector_const (machine_mode vector_mode, rtx vector_const,
		      auto_bitmap &vector_insns,
		      machine_mode scalar_mode)
{
  bitmap_iterator bi;
  unsigned int id;

  EXECUTE_IF_SET_IN_BITMAP (vector_insns, 0, id, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (id)->insn;

      /* Get the single SET instruction.  */
      rtx set = single_set (insn);
      rtx src = SET_SRC (set);
      rtx dest = SET_DEST (set);
      machine_mode mode = GET_MODE (dest);

      rtx replace;
      /* Replace the source operand with VECTOR_CONST.  */
      if (SUBREG_P (src) || mode == vector_mode)
	replace = vector_const;
      else
	{
	  unsigned int size = GET_MODE_SIZE (mode);
	  if (size < ix86_regmode_natural_size (mode))
	    {
	      /* If the mode size is smaller than its natural size,
		 first insert an extra move with a QI vector SUBREG
		 of the same size to avoid validate_subreg failure.  */
	      machine_mode vmode
		= ix86_get_vector_cse_mode (size, scalar_mode);
	      rtx vreg;
	      if (mode == vmode)
		vreg = vector_const;
	      else
		{
		  vreg = gen_reg_rtx (vmode);
		  rtx vsubreg = gen_rtx_SUBREG (vmode, vector_const, 0);
		  rtx pat = gen_rtx_SET (vreg, vsubreg);
		  rtx_insn *vinsn = emit_insn_before (pat, insn);
		  if (dump_file)
		    {
		      fprintf (dump_file, "\nInsert an extra move:\n\n");
		      print_rtl_single (dump_file, vinsn);
		      fprintf (dump_file, "\nbefore:\n\n");
		      print_rtl_single (dump_file, insn);
		      fprintf (dump_file, "\n");
		    }
		}
	      replace = gen_rtx_SUBREG (mode, vreg, 0);
	    }
	  else
	    replace = gen_rtx_SUBREG (mode, vector_const, 0);
	}

      if (dump_file)
	{
	  fprintf (dump_file, "\nReplace:\n\n");
	  print_rtl_single (dump_file, insn);
	}
      SET_SRC (set) = replace;
      /* Drop possible dead definitions.  */
      PATTERN (insn) = set;
      INSN_CODE (insn) = -1;
      recog_memoized (insn);
      if (dump_file)
	{
	  fprintf (dump_file, "\nwith:\n\n");
	  print_rtl_single (dump_file, insn);
	  fprintf (dump_file, "\n");
	}
      df_insn_rescan (insn);
    }
}

/* Return the inner scalar if OP is a broadcast, else return nullptr.  */

static rtx
ix86_broadcast_inner (rtx op, machine_mode mode,
		      machine_mode *scalar_mode_p,
		      x86_cse_kind *kind_p, rtx_insn **insn_p)
{
  switch (standard_sse_constant_p (op, mode))
    {
    case 1:
      *scalar_mode_p = QImode;
      *kind_p = X86_CSE_CONST0_VECTOR;
      *insn_p = nullptr;
      return const0_rtx;
    case 2:
      *scalar_mode_p = QImode;
      *kind_p = X86_CSE_CONSTM1_VECTOR;
      *insn_p = nullptr;
      return constm1_rtx;
    default:
      break;
    }

  mode = GET_MODE (op);
  int nunits = GET_MODE_NUNITS (mode);
  if (nunits < 2)
    return nullptr;

  *kind_p = X86_CSE_VEC_DUP;

  rtx reg;
  if (GET_CODE (op) == VEC_DUPLICATE)
    {
      /* Only
	  (vec_duplicate:V4SI (reg:SI 99))
	  (vec_duplicate:V2DF (mem/u/c:DF (symbol_ref/u:DI ("*.LC1") [flags 0x2]) [0  S8 A64]))
	 are supported.  Set OP to the broadcast source by default.  */
      op = XEXP (op, 0);
      reg = op;
      if (SUBREG_P (op)
	  && SUBREG_BYTE (op) == 0
	  && !paradoxical_subreg_p (op))
	reg = SUBREG_REG (op);
      if (!REG_P (reg))
	{
	  if (MEM_P (op)
	      && SYMBOL_REF_P (XEXP (op, 0))
	      && CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
	    {
	      /* Handle constant broadcast from memory.  */
	      *scalar_mode_p = GET_MODE_INNER (mode);
	      *insn_p = nullptr;
	      return op;
	    }
	  return nullptr;
	}
    }
  else if (CONST_VECTOR_P (op))
    {
      rtx first = XVECEXP (op, 0, 0);
      for (int i = 1; i < nunits; ++i)
	{
	  rtx tmp = XVECEXP (op, 0, i);
	  /* Vector duplicate value.  */
	  if (!rtx_equal_p (tmp, first))
	    return nullptr;
	}
      *scalar_mode_p = GET_MODE (first);
      *insn_p = nullptr;
      return first;
    }
  else
    return nullptr;

  mode = GET_MODE (op);

  /* Only single def chain is supported.  */
  df_ref ref = DF_REG_DEF_CHAIN (REGNO (reg));
  if (!ref
      || DF_REF_IS_ARTIFICIAL (ref)
      || DF_REF_NEXT_REG (ref) != nullptr)
    return nullptr;

  rtx_insn *insn = DF_REF_INSN (ref);
  rtx set = single_set (insn);
  if (!set)
    return nullptr;

  rtx src = SET_SRC (set);

  if (CONST_INT_P (src))
    {
      /* Handle sequences like

	 (set (reg:SI 99)
	       (const_int 34 [0x22]))
	 (set (reg:V4SI 98)
	       (vec_duplicate:V4SI (reg:SI 99)))

	 Set *INSN_P to nullptr and return SET_SRC if SET_SRC is an
	 integer constant.  */
      op = src;
      if (mode != GET_MODE (reg))
	op = gen_int_mode (INTVAL (src), mode);
      *insn_p = nullptr;
    }
  else
    {
      /* Handle sequences like

	 (set (reg:QI 105 [ c ])
	      (reg:QI 5 di [ c ]))
	 (set (reg:V64QI 102 [ _1 ])
	      (vec_duplicate:V64QI (reg:QI 105 [ c ])))

	 (set (reg/v:SI 116 [ argc ])
	      (mem/c:SI (reg:SI 135) [2 argc+0 S4 A32]))
	 (set (reg:V4SI 119 [ _45 ])
	      (vec_duplicate:V4SI (reg/v:SI 116 [ argc ])))

	 (set (reg:SI 98 [ _1 ])
	      (sign_extend:SI (reg:QI 106 [ c ])))
	 (set (reg:V16SI 103 [ _2 ])
	       (vec_duplicate:V16SI (reg:SI 98 [ _1 ])))

	 (set (reg:SI 102 [ cost ])
	      (mem/c:SI (symbol_ref:DI ("cost") [flags 0x40])))
	 (set (reg:V4HI 103 [ _16 ])
	      (vec_duplicate:V4HI (subreg:HI (reg:SI 102 [ cost ]) 0)))

	 (set (subreg:SI (reg/v:HI 107 [ cr_val ]) 0)
	      (ashift:SI (reg:SI 158)
			 (subreg:QI (reg:SI 156 [ _2 ]) 0)))
	 (set (reg:V16HI 183 [ _61 ])
	      (vec_duplicate:V16HI (reg/v:HI 107 [ cr_val ])))

	 Set *INSN_P to INSN and return the broadcast source otherwise.  */
      *insn_p = insn;
    }

  *scalar_mode_p = mode;
  return op;
}

/* Replace CALL instruction in TLS_CALL_INSNS with SET from SRC and
   put the updated instruction in UPDATED_TLS_INSNS.  */

static void
replace_tls_call (rtx src, auto_bitmap &tls_call_insns,
		  auto_bitmap &updated_tls_insns)
{
  bitmap_iterator bi;
  unsigned int id;

  EXECUTE_IF_SET_IN_BITMAP (tls_call_insns, 0, id, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (id)->insn;

      /* If this isn't a CALL, only GNU2 TLS implicit CALL patterns are
	 allowed.  */
      if (!CALL_P (insn))
	{
	  attr_tls64 tls64 = get_attr_tls64 (insn);
	  if (tls64 != TLS64_CALL && tls64 != TLS64_COMBINE)
	    gcc_unreachable ();
	}

      rtx pat = PATTERN (insn);
      gcc_assert (GET_CODE (pat) == PARALLEL);
      rtx set = XVECEXP (pat, 0, 0);
      gcc_assert (GET_CODE (set) == SET);
      rtx dest = SET_DEST (set);

      set = gen_rtx_SET (dest, src);
      rtx_insn *set_insn = emit_insn_after (set, insn);
      if (recog_memoized (set_insn) < 0)
	gcc_unreachable ();

      /* Put SET_INSN in UPDATED_TLS_INSNS.  */
      bitmap_set_bit (updated_tls_insns, INSN_UID (set_insn));

      if (dump_file)
	{
	  fprintf (dump_file, "\nReplace:\n\n");
	  print_rtl_single (dump_file, insn);
	  fprintf (dump_file, "\nwith:\n\n");
	  print_rtl_single (dump_file, set_insn);
	  fprintf (dump_file, "\n");
	}

      /* Delete the CALL insn.  */
      delete_insn (insn);

      df_insn_rescan (set_insn);
    }
}

/* Return the basic block which dominates all basic blocks which set
   hard register REGNO used in basic block BB.  */

static basic_block
ix86_get_dominator_for_reg (unsigned int regno, basic_block bb)
{
  basic_block set_bb;
  auto_bitmap set_bbs;

  /* Get all BBs which set REGNO and dominate the current BB from all
     DEFs of REGNO.  */
  for (df_ref def = DF_REG_DEF_CHAIN (regno);
       def;
       def = DF_REF_NEXT_REG (def))
    if (!DF_REF_IS_ARTIFICIAL (def)
	&& !DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER)
	&& !DF_REF_FLAGS_IS_SET (def, DF_REF_MUST_CLOBBER))
      {
	set_bb = DF_REF_BB (def);
	if (dominated_by_p (CDI_DOMINATORS, bb, set_bb))
	  bitmap_set_bit (set_bbs, set_bb->index);
      }

  bb = nearest_common_dominator_for_set (CDI_DOMINATORS, set_bbs);
  return bb;
}

/* Mark FLAGS register as live in DATA, a bitmap of live caller-saved
   registers, if DEST is FLAGS register.  */

static void
ix86_check_flags_reg (rtx dest, const_rtx, void *data)
{
  auto_bitmap *live_caller_saved_regs = (auto_bitmap *) data;
  if (REG_P (dest) && REGNO (dest) == FLAGS_REG)
    bitmap_set_bit (*live_caller_saved_regs, FLAGS_REG);
}

/* Emit a TLS_SET instruction of KIND in basic block BB.   Store the
   insertion point in *BEFORE_P for emit_insn_before or in *AFTER_P
   for emit_insn_after.  UPDATED_GNU_TLS_INSNS contains instructions
   which replace the GNU TLS instructions.  UPDATED_GNU2_TLS_INSNS
   contains instructions which replace the GNU2 TLS instructions.  */

static rtx_insn *
ix86_emit_tls_call (rtx tls_set, x86_cse_kind kind, basic_block bb,
		    rtx_insn **before_p, rtx_insn **after_p,
		    auto_bitmap &updated_gnu_tls_insns,
		    auto_bitmap &updated_gnu2_tls_insns)
{
  rtx_insn *tls_insn;

  do
    {
      rtx_insn *insn = BB_HEAD (bb);
      while (insn && !NONDEBUG_INSN_P (insn))
	{
	  if (insn == BB_END (bb))
	    {
	      /* This must be the beginning basic block:

		 (note 4 0 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
		 (note 2 4 26 2 NOTE_INSN_FUNCTION_BEG)

		 or a basic block with only a label:

		 (code_label 78 11 77 3 14 (nil) [1 uses])
		 (note 77 78 54 3 [bb 3] NOTE_INSN_BASIC_BLOCK)

		 or a basic block with only a debug marker:

		 (note 3 0 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
		 (note 2 3 5 2 NOTE_INSN_FUNCTION_BEG)
		 (debug_insn 5 2 16 2 (debug_marker) "x.c":6:3 -1 (nil))

	       */
	      gcc_assert (DEBUG_INSN_P (insn)
			  || (NOTE_P (insn)
			      && ((NOTE_KIND (insn)
				   == NOTE_INSN_FUNCTION_BEG)
				  || (NOTE_KIND (insn)
				      == NOTE_INSN_BASIC_BLOCK))));
	      insn = NULL;
	      break;
	    }
	  insn = NEXT_INSN (insn);
	}

      /* TLS_GD and TLS_LD_BASE instructions are normal functions which
	 clobber caller-saved registers.  TLSDESC instructions only
	 clobber FLAGS.  If any registers clobbered by TLS instructions
	 are live in this basic block, we must insert TLS instructions
	 after all live registers clobbered are dead.  */

      auto_bitmap live_caller_saved_regs;
      bitmap in = df_live ? DF_LIVE_IN (bb) : DF_LR_IN (bb);

      if (bitmap_bit_p (in, FLAGS_REG))
	bitmap_set_bit (live_caller_saved_regs, FLAGS_REG);

      unsigned int i;

      /* Get all live caller-saved registers for TLS_GD and TLS_LD_BASE
	 instructions.  */
      if (kind != X86_CSE_TLSDESC)
	for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	  if (call_used_regs[i]
	      && !fixed_regs[i]
	      && bitmap_bit_p (in, i))
	    bitmap_set_bit (live_caller_saved_regs, i);

      if (bitmap_empty_p (live_caller_saved_regs))
	{
	  if (insn == BB_HEAD (bb))
	    {
	      *before_p = insn;
	      tls_insn = emit_insn_before (tls_set, insn);
	    }
	  else
	    {
	      /* Emit the TLS call after NOTE_INSN_FUNCTION_BEG in the
		 beginning basic block:

		 (note 4 0 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
		 (note 2 4 26 2 NOTE_INSN_FUNCTION_BEG)

		 or after NOTE_INSN_BASIC_BLOCK in a basic block with
		 only a label:

		 (code_label 78 11 77 3 14 (nil) [1 uses])
		 (note 77 78 54 3 [bb 3] NOTE_INSN_BASIC_BLOCK)

		 or after debug marker in a basic block with only a
		 debug marker:

		 (note 3 0 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
		 (note 2 3 5 2 NOTE_INSN_FUNCTION_BEG)
		 (debug_insn 5 2 16 2 (debug_marker) "x.c":6:3 -1 (nil))

	       */
	      insn = insn ? PREV_INSN (insn) : BB_END (bb);
	      *after_p = insn;
	      tls_insn = emit_insn_after (tls_set, insn);
	    }
	  return tls_insn;
	}

      bool repeat = false;

      /* Search for REG_DEAD notes in this basic block.  */
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  /* NB: Conditional jump is the only instruction which reads
	     flags register and changes control flow.  We can never
	     place the TLS call after unconditional jump.  */
	  if (JUMP_P (insn))
	    {
	      /* This must be a conditional jump.  */
	      rtx label = JUMP_LABEL (insn);
	      if (label == nullptr
		  || ANY_RETURN_P (label)
		  || !(LABEL_P (label) || SYMBOL_REF_P (label)))
		gcc_unreachable ();

	      /* Place the call before all FLAGS_REG setting BBs since
		 we can't place a call before nor after a conditional
		 jump.  */
	      bb = ix86_get_dominator_for_reg (FLAGS_REG, bb);

	      /* Start over again.  */
	      repeat = true;
	      break;
	    }

	  if (bitmap_bit_p (updated_gnu_tls_insns, INSN_UID (insn)))
	    {
	      /* Insert the __tls_get_addr call before INSN which
		 replaces a __tls_get_addr call.  */
	      *before_p = insn;
	      tls_insn = emit_insn_before (tls_set, insn);
	      return tls_insn;
	    }

	  if (bitmap_bit_p (updated_gnu2_tls_insns, INSN_UID (insn)))
	    {
	      /* Mark FLAGS register as dead since FLAGS register
		 would be clobbered by the GNU2 TLS instruction.  */
	      bitmap_clear_bit (live_caller_saved_regs, FLAGS_REG);
	      continue;
	    }

	  /* Check if FLAGS register is live.  */
	  note_stores (insn, ix86_check_flags_reg,
		       &live_caller_saved_regs);

	  rtx link;
	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == REG_DEAD
		&& REG_P (XEXP (link, 0)))
	      {
		/* Mark the live caller-saved register as dead.  */
		for (i = REGNO (XEXP (link, 0));
		     i < END_REGNO (XEXP (link, 0));
		     i++)
		  if (i < FIRST_PSEUDO_REGISTER)
		    bitmap_clear_bit (live_caller_saved_regs, i);

		if (bitmap_empty_p (live_caller_saved_regs))
		  {
		    *after_p = insn;
		    tls_insn = emit_insn_after (tls_set, insn);
		    return tls_insn;
		  }
	      }
	}

      /* NB: Start over again for conditional jump.  */
      if (repeat)
	continue;

      gcc_assert (!bitmap_empty_p (live_caller_saved_regs));

      /* If any live caller-saved registers aren't dead at the end of
	 this basic block, get the basic block which dominates all
	 basic blocks which set the remaining live registers.  */
      auto_bitmap set_bbs;
      bitmap_iterator bi;
      unsigned int id;
      EXECUTE_IF_SET_IN_BITMAP (live_caller_saved_regs, 0, id, bi)
	{
	  basic_block set_bb = ix86_get_dominator_for_reg (id, bb);
	  bitmap_set_bit (set_bbs, set_bb->index);
	}
      bb = nearest_common_dominator_for_set (CDI_DOMINATORS, set_bbs);
    }
  while (true);
}

/* Generate a TLS call of KIND with VAL and copy the call result to DEST,
   at entry of the nearest dominator for basic block map BBS, which is in
   the fake loop that contains the whole function, so that there is only
   a single TLS CALL of KIND with VAL in the whole function.
   UPDATED_GNU_TLS_INSNS contains instructions which replace the GNU TLS
   instructions.  UPDATED_GNU2_TLS_INSNS contains instructions which
   replace the GNU2 TLS instructions.  If TLSDESC_SET isn't nullptr,
   insert it before the TLS call.  */

static void
ix86_place_single_tls_call (rtx dest, rtx val, x86_cse_kind kind,
			    auto_bitmap &bbs,
			    auto_bitmap &updated_gnu_tls_insns,
			    auto_bitmap &updated_gnu2_tls_insns,
			    rtx tlsdesc_set = nullptr)
{
  basic_block bb = nearest_common_dominator_for_set (CDI_DOMINATORS, bbs);
  while (bb->loop_father->latch
	 != EXIT_BLOCK_PTR_FOR_FN (cfun))
    bb = get_immediate_dominator (CDI_DOMINATORS,
				  bb->loop_father->header);

  rtx rax = nullptr, rdi;
  rtx eqv = nullptr;
  rtx caddr;
  rtx set;
  rtx clob;
  rtx symbol;
  rtx tls;

  switch (kind)
    {
    case X86_CSE_TLS_GD:
      rax = gen_rtx_REG (Pmode, AX_REG);
      rdi = gen_rtx_REG (Pmode, DI_REG);
      caddr = ix86_tls_get_addr ();

      symbol = XVECEXP (val, 0, 0);
      tls = gen_tls_global_dynamic_64 (Pmode, rax, symbol, caddr, rdi);

      if (GET_MODE (symbol) != Pmode)
	symbol = gen_rtx_ZERO_EXTEND (Pmode, symbol);
      eqv = symbol;
      break;

    case X86_CSE_TLS_LD_BASE:
      rax = gen_rtx_REG (Pmode, AX_REG);
      rdi = gen_rtx_REG (Pmode, DI_REG);
      caddr = ix86_tls_get_addr ();

      tls = gen_tls_local_dynamic_base_64 (Pmode, rax, caddr, rdi);

      /* Attach a unique REG_EQUAL to DEST, to allow the RTL optimizers
	 to share the LD_BASE result with other LD model accesses.  */
      eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
			    UNSPEC_TLS_LD_BASE);

      break;

    case X86_CSE_TLSDESC:
      set = gen_rtx_SET (dest, val);
      clob = gen_rtx_CLOBBER (VOIDmode,
			      gen_rtx_REG (CCmode, FLAGS_REG));
      tls = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clob));
      break;

    default:
      gcc_unreachable ();
    }

  /* Emit the TLS CALL insn.  */
  rtx_insn *before = nullptr;
  rtx_insn *after = nullptr;
  rtx_insn *tls_insn = ix86_emit_tls_call (tls, kind, bb, &before,
					   &after,
					   updated_gnu_tls_insns,
					   updated_gnu2_tls_insns);

  rtx_insn *tlsdesc_insn = nullptr;
  if (tlsdesc_set)
    {
      rtx dest = copy_rtx (SET_DEST (tlsdesc_set));
      rtx src = copy_rtx (SET_SRC (tlsdesc_set));
      tlsdesc_set = gen_rtx_SET (dest, src);
      tlsdesc_insn = emit_insn_before (tlsdesc_set, tls_insn);
    }

  if (kind != X86_CSE_TLSDESC)
    {
      RTL_CONST_CALL_P (tls_insn) = 1;

      /* Indicate that this function can't jump to non-local gotos.  */
      make_reg_eh_region_note_nothrow_nononlocal (tls_insn);
    }

  if (recog_memoized (tls_insn) < 0)
    gcc_unreachable ();

  if (dump_file)
    {
      if (after)
	{
	  fprintf (dump_file, "\nPlace:\n\n");
	  if (tlsdesc_insn)
	    print_rtl_single (dump_file, tlsdesc_insn);
	  print_rtl_single (dump_file, tls_insn);
	  fprintf (dump_file, "\nafter:\n\n");
	  print_rtl_single (dump_file, after);
	  fprintf (dump_file, "\n");
	}
      else
	{
	  fprintf (dump_file, "\nPlace:\n\n");
	  if (tlsdesc_insn)
	    print_rtl_single (dump_file, tlsdesc_insn);
	  print_rtl_single (dump_file, tls_insn);
	  fprintf (dump_file, "\nbefore:\n\n");
	  print_rtl_single (dump_file, before);
	  fprintf (dump_file, "\n");
	}
    }

  if (kind != X86_CSE_TLSDESC)
    {
      /* Copy RAX to DEST.  */
      set = gen_rtx_SET (dest, rax);
      rtx_insn *set_insn = emit_insn_after (set, tls_insn);
      set_dst_reg_note (set_insn, REG_EQUAL, copy_rtx (eqv), dest);
      if (dump_file)
	{
	  fprintf (dump_file, "\nPlace:\n\n");
	  print_rtl_single (dump_file, set_insn);
	  fprintf (dump_file, "\nafter:\n\n");
	  print_rtl_single (dump_file, tls_insn);
	  fprintf (dump_file, "\n");
	}
    }
}

namespace {

const pass_data pass_data_x86_cse =
{
  RTL_PASS, /* type */
  "x86_cse", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_x86_cse : public rtl_opt_pass
{
public:
  pass_x86_cse (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_x86_cse, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
    {
      return (TARGET_SSE2
	      && optimize
	      && optimize_function_for_speed_p (fun));
    }

  unsigned int execute (function *) final override
    {
      return x86_cse ();
    }

private:
  /* The redundant source value.  */
  rtx val;
  /* The actual redundant source value for UNSPEC_TLSDESC.  */
  rtx tlsdesc_val;
  /* The instruction which defines the redundant value.  */
  rtx_insn *def_insn;
  /* Mode of the destination of the candidate redundant instruction.  */
  machine_mode mode;
  /* Mode of the source of the candidate redundant instruction.  */
  machine_mode scalar_mode;
  /* The classification of the candidate redundant instruction.  */
  x86_cse_kind kind;

  unsigned int x86_cse (void);
  bool candidate_gnu_tls_p (rtx_insn *, attr_tls64);
  bool candidate_gnu2_tls_p (rtx, attr_tls64);
  bool candidate_vector_p (rtx);
  rtx_insn *tls_set_insn_from_symbol (const_rtx, const_rtx);
}; // class pass_x86_cse

/* Return the instruction which sets REG from TLS_SYMBOL.  */

rtx_insn *
pass_x86_cse::tls_set_insn_from_symbol (const_rtx reg,
					const_rtx tls_symbol)
{
  rtx_insn *set_insn = nullptr;
  for (df_ref ref = DF_REG_DEF_CHAIN (REGNO (reg));
       ref;
       ref = DF_REF_NEXT_REG (ref))
    {
      if (DF_REF_IS_ARTIFICIAL (ref))
	return nullptr;

      set_insn = DF_REF_INSN (ref);
      if (get_attr_tls64 (set_insn) != TLS64_LEA)
	return nullptr;

      rtx tls_set = PATTERN (set_insn);
      rtx tls_src = XVECEXP (SET_SRC (tls_set), 0, 0);
      if (!rtx_equal_p (tls_symbol, tls_src))
	return nullptr;
    }

  return set_insn;
}

/* Return true and output def_insn, val, mode, scalar_mode and kind if
   INSN is UNSPEC_TLS_GD or UNSPEC_TLS_LD_BASE.  */

bool
pass_x86_cse::candidate_gnu_tls_p (rtx_insn *insn, attr_tls64 tls64)
{
  if (!TARGET_64BIT || !cfun->machine->tls_descriptor_call_multiple_p)
    return false;

  /* Record the redundant TLS CALLs for 64-bit:

     (parallel [
	(set (reg:DI 0 ax)
	     (call:DI (mem:QI (symbol_ref:DI ("__tls_get_addr")))
		      (const_int 0 [0])))
	(unspec:DI [(symbol_ref:DI ("foo") [flags 0x50])
		    (reg/f:DI 7 sp)] UNSPEC_TLS_GD)
	(clobber (reg:DI 5 di))])


     and

     (parallel [
	(set (reg:DI 0 ax)
	     (call:DI (mem:QI (symbol_ref:DI ("__tls_get_addr")))
		      (const_int 0 [0])))
	(unspec:DI [(reg/f:DI 7 sp)] UNSPEC_TLS_LD_BASE)])

   */

  rtx pat = PATTERN (insn);
  rtx set = XVECEXP (pat, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  rtx dest = SET_DEST (set);
  scalar_mode = mode = GET_MODE (dest);
  val = XVECEXP (pat, 0, 1);
  gcc_assert (GET_CODE (val) == UNSPEC);

  if (tls64 == TLS64_GD)
    kind = X86_CSE_TLS_GD;
  else
    kind = X86_CSE_TLS_LD_BASE;

  def_insn = nullptr;
  return true;
}

/* Return true and output def_insn, val, mode, scalar_mode and kind if
   SET is UNSPEC_TLSDESC.  */

bool
pass_x86_cse::candidate_gnu2_tls_p (rtx set, attr_tls64 tls64)
{
  if (!TARGET_64BIT || !cfun->machine->tls_descriptor_call_multiple_p)
    return false;

  rtx tls_symbol;
  rtx_insn *set_insn;
  rtx src = SET_SRC (set);
  val = src;
  tlsdesc_val = src;
  kind = X86_CSE_TLSDESC;

  if (tls64 == TLS64_COMBINE)
    {
      /* Record 64-bit TLS64_COMBINE:

	 (set (reg/f:DI 104)
	      (plus:DI (unspec:DI [
			  (symbol_ref:DI ("_TLS_MODULE_BASE_") [flags 0x10])
			  (reg:DI 114)
			  (reg/f:DI 7 sp)] UNSPEC_TLSDESC)
		       (const:DI (unspec:DI [
				    (symbol_ref:DI ("e") [flags 0x1a])
				  ] UNSPEC_DTPOFF))))

	 (set (reg/f:DI 104)
	      (plus:DI (unspec:DI [
			  (symbol_ref:DI ("_TLS_MODULE_BASE_") [flags 0x10])
			  (unspec:DI [
			     (symbol_ref:DI ("_TLS_MODULE_BASE_") [flags 0x10])
			  ] UNSPEC_TLSDESC)
			  (reg/f:DI 7 sp)] UNSPEC_TLSDESC)
		       (const:DI (unspec:DI [
				    (symbol_ref:DI ("e") [flags 0x1a])
				 ] UNSPEC_DTPOFF))))
     */

      scalar_mode = mode = GET_MODE (src);

      /* Since the first operand of PLUS in the source TLS_COMBINE
	 pattern is unused, use the second operand of PLUS:

	 (const:DI (unspec:DI [
		      (symbol_ref:DI ("e") [flags 0x1a])
		   ] UNSPEC_DTPOFF))

	 as VAL to check if 2 TLS_COMBINE patterns have the same
	 source.  */
      val = XEXP (src, 1);
      gcc_assert (GET_CODE (val) == CONST
		  && GET_CODE (XEXP (val, 0)) == UNSPEC
		      && XINT (XEXP (val, 0), 1) == UNSPEC_DTPOFF
		      && SYMBOL_REF_P (XVECEXP (XEXP (val, 0), 0, 0)));
      def_insn = nullptr;
      return true;
    }

  /* Record 64-bit TLS_CALL:

     (set (reg:DI 101)
	  (unspec:DI [(symbol_ref:DI ("foo") [flags 0x50])
		      (reg:DI 112)
		      (reg/f:DI 7 sp)] UNSPEC_TLSDESC))

   */

  gcc_assert (GET_CODE (src) == UNSPEC);
  tls_symbol = XVECEXP (src, 0, 0);
  src = XVECEXP (src, 0, 1);
  scalar_mode = mode = GET_MODE (src);
  gcc_assert (REG_P (src));

  /* All definitions of reg:DI 129 in

     (set (reg:DI 110)
	  (unspec:DI [(symbol_ref:DI ("foo"))
		      (reg:DI 129)
		      (reg/f:DI 7 sp)] UNSPEC_TLSDESC))

     should have the same source as in

     (set (reg:DI 129)
	  (unspec:DI [(symbol_ref:DI ("foo"))] UNSPEC_TLSDESC))

   */

  set_insn = tls_set_insn_from_symbol (src, tls_symbol);
  if (!set_insn)
    return false;

  /* Use TLS_SYMBOL as VAL to check if 2 patterns have the same source.  */
  val = tls_symbol;
  def_insn = set_insn;
  return true;
}

/* Return true and output def_insn, val, mode, scalar_mode and kind if
  INSN is a vector broadcast instruction.  */

bool
pass_x86_cse::candidate_vector_p (rtx set)
{
  rtx src = SET_SRC (set);
  rtx dest = SET_DEST (set);
  mode = GET_MODE (dest);
  /* Skip non-vector instruction.  */
  if (!VECTOR_MODE_P (mode))
    return false;

  /* Skip non-vector load instruction.  */
  if (!REG_P (dest) && !SUBREG_P (dest))
    return false;

  val = ix86_broadcast_inner (src, mode, &scalar_mode, &kind,
			      &def_insn);
  return val ? true : false;
}

/* At entry of the nearest common dominator for basic blocks with

   1. Vector CONST0_RTX patterns.
   2. Vector CONSTM1_RTX patterns.
   3. Vector broadcast patterns.
   4. UNSPEC_TLS_GD patterns.
   5. UNSPEC_TLS_LD_BASE patterns.
   6. UNSPEC_TLSDESC patterns.

   generate a single pattern whose destination is used to replace the
   source in all identical patterns.

   NB: We want to generate a pattern, which is executed only once, to
   cover the whole function.  The LCM algorithm isn't appropriate here
   since it may place a pattern inside the loop.  */

unsigned int
pass_x86_cse::x86_cse (void)
{
  timevar_push (TV_MACH_DEP);

  auto_vec<redundant_pattern *> loads;
  redundant_pattern *load;
  basic_block bb;
  rtx_insn *insn;
  unsigned int i;
  auto_bitmap updated_gnu_tls_insns;
  auto_bitmap updated_gnu2_tls_insns;

  df_set_flags (DF_DEFER_INSN_RESCAN);

  bool recursive_call_p = cfun->machine->recursive_function;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  bool matched = false;
	  /* Remove redundant pattens if there are more than 2 of
	     them.  */
	  unsigned int threshold = 2;

	  rtx set = single_set (insn);
	  if (!set && !CALL_P (insn))
	    continue;

	  tlsdesc_val = nullptr;

	  attr_tls64 tls64 = get_attr_tls64 (insn);
	  switch (tls64)
	    {
	    case TLS64_GD:
	    case TLS64_LD_BASE:
	      /* Verify UNSPEC_TLS_GD and UNSPEC_TLS_LD_BASE.  */
	      if (candidate_gnu_tls_p (insn, tls64))
		break;
	      continue;

	    case TLS64_CALL:
	    case TLS64_COMBINE:
	      /* Verify UNSPEC_TLSDESC.  */
	      if (candidate_gnu2_tls_p (set, tls64))
		break;
	      continue;

	    case TLS64_LEA:
	      /* Skip TLS64_LEA.  */
	      continue;

	    case TLS64_NONE:
	      if (!set)
		continue;

	      /* Check for vector broadcast.  */
	      if (candidate_vector_p (set))
		break;
	      continue;
	    }

	  /* Check if there is a matching redundant load.   */
	  FOR_EACH_VEC_ELT (loads, i, load)
	    if (load->val
		&& load->kind == kind
		&& load->mode == scalar_mode
		&& (load->bb == bb
		    || kind != X86_CSE_VEC_DUP
		    /* Non all 0s/1s vector load must be in the same
		       basic block if it is in a recursive call.  */
		    || !recursive_call_p)
		&& rtx_equal_p (load->val, val))
	      {
		/* Record instruction.  */
		bitmap_set_bit (load->insns, INSN_UID (insn));

		/* Record the maximum vector size.  */
		if (kind <= X86_CSE_VEC_DUP
		    && load->size < GET_MODE_SIZE (mode))
		  load->size = GET_MODE_SIZE (mode);

		/* Record the basic block.  */
		bitmap_set_bit (load->bbs, bb->index);

		/* Increment the count.  */
		load->count++;

		matched = true;
		break;
	      }

	  if (matched)
	    continue;

	  /* We see this instruction the first time.  Record the
	     redundant source value, its mode, the destination size,
	     instruction which defines the redundant source value,
	     instruction basic block and the instruction kind.  */
	  load = new redundant_pattern;

	  load->val = copy_rtx (val);
	  if (tlsdesc_val)
	    load->tlsdesc_val = copy_rtx (tlsdesc_val);
	  else
	    load->tlsdesc_val = nullptr;
	  load->mode = scalar_mode;
	  load->size = GET_MODE_SIZE (mode);
	  load->def_insn = def_insn;
	  load->count = 1;
	  load->threshold = threshold;
	  load->bb = BLOCK_FOR_INSN (insn);
	  load->kind = kind;

	  bitmap_set_bit (load->insns, INSN_UID (insn));
	  bitmap_set_bit (load->bbs, bb->index);

	  loads.safe_push (load);
	}
    }

  bool replaced = false;
  FOR_EACH_VEC_ELT (loads, i, load)
    if (load->count >= load->threshold)
      {
	machine_mode mode;
	rtx reg, broadcast_source, broadcast_reg;
	replaced = true;
	switch (load->kind)
	  {
	  case X86_CSE_TLS_GD:
	  case X86_CSE_TLS_LD_BASE:
	  case X86_CSE_TLSDESC:
	    broadcast_reg = gen_reg_rtx (load->mode);
	    replace_tls_call (broadcast_reg, load->insns,
			      (load->kind == X86_CSE_TLSDESC
			       ? updated_gnu2_tls_insns
			       : updated_gnu_tls_insns));
	    load->broadcast_reg = broadcast_reg;
	    break;

	  case X86_CSE_CONST0_VECTOR:
	  case X86_CSE_CONSTM1_VECTOR:
	  case X86_CSE_VEC_DUP:
	    mode = ix86_get_vector_cse_mode (load->size, load->mode);
	    broadcast_reg = gen_reg_rtx (mode);
	    if (load->def_insn)
	      {
		/* Replace redundant vector loads with a single vector
		   load in the same basic block.  */
		reg = load->val;
		if (load->mode != GET_MODE (reg))
		  reg = gen_rtx_SUBREG (load->mode, reg, 0);
		broadcast_source = gen_rtx_VEC_DUPLICATE (mode, reg);
	      }
	    else
	      /* This is a constant integer/double vector.  If the
		 inner scalar is 0 or -1, set vector to CONST0_RTX
		 or CONSTM1_RTX directly.  */
	      switch (load->kind)
		{
		case X86_CSE_CONST0_VECTOR:
		  broadcast_source = CONST0_RTX (mode);
		  break;
		case X86_CSE_CONSTM1_VECTOR:
		  broadcast_source = CONSTM1_RTX (mode);
		  break;
		case X86_CSE_VEC_DUP:
		  reg = gen_reg_rtx (load->mode);
		  broadcast_source = gen_rtx_VEC_DUPLICATE (mode, reg);
		  break;
		default:
		  gcc_unreachable ();
		}
	    replace_vector_const (mode, broadcast_reg, load->insns,
				  load->mode);
	    load->broadcast_source = broadcast_source;
	    load->broadcast_reg = broadcast_reg;
	    break;
	  }
      }

  if (replaced)
    {
      auto_vec<rtx_insn *> control_flow_insns;

      /* (Re-)discover loops so that bb->loop_father can be used in the
	 analysis below.  */
      calculate_dominance_info (CDI_DOMINATORS);
      loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

      FOR_EACH_VEC_ELT (loads, i, load)
	if (load->count >= load->threshold)
	  {
	    rtx set;
	    if (load->def_insn)
	      switch (load->kind)
		{
		case X86_CSE_TLSDESC:
		  ix86_place_single_tls_call (load->broadcast_reg,
					      load->tlsdesc_val,
					      load->kind,
					      load->bbs,
					      updated_gnu_tls_insns,
					      updated_gnu2_tls_insns,
					      PATTERN (load->def_insn));
		  break;
		case X86_CSE_VEC_DUP:
		  /* Insert a broadcast after the original scalar
		     definition.  */
		  set = gen_rtx_SET (load->broadcast_reg,
				     load->broadcast_source);
		  insn = emit_insn_after (set, load->def_insn);

		  if (cfun->can_throw_non_call_exceptions)
		    {
		      /* Handle REG_EH_REGION note in DEF_INSN.  */
		      rtx note = find_reg_note (load->def_insn,
						REG_EH_REGION, nullptr);
		      if (note)
			{
			  control_flow_insns.safe_push (load->def_insn);
			  add_reg_note (insn, REG_EH_REGION,
					XEXP (note, 0));
			}
		    }

		  if (dump_file)
		    {
		      fprintf (dump_file, "\nAdd:\n\n");
		      print_rtl_single (dump_file, insn);
		      fprintf (dump_file, "\nafter:\n\n");
		      print_rtl_single (dump_file, load->def_insn);
		      fprintf (dump_file, "\n");
		    }
		  break;
		default:
		  gcc_unreachable ();
		}
	    else
	      switch (load->kind)
		{
		case X86_CSE_TLS_GD:
		case X86_CSE_TLS_LD_BASE:
		case X86_CSE_TLSDESC:
		  ix86_place_single_tls_call (load->broadcast_reg,
					      (load->kind == X86_CSE_TLSDESC
					       ? load->tlsdesc_val
					       : load->val),
					      load->kind,
					      load->bbs,
					      updated_gnu_tls_insns,
					      updated_gnu2_tls_insns);
		  break;
		case X86_CSE_CONST0_VECTOR:
		case X86_CSE_CONSTM1_VECTOR:
		case X86_CSE_VEC_DUP:
		  ix86_place_single_vector_set (load->broadcast_reg,
						load->broadcast_source,
						load->bbs,
						load);
		  break;
		}
	  }

      loop_optimizer_finalize ();

      if (!control_flow_insns.is_empty ())
	{
	  free_dominance_info (CDI_DOMINATORS);

	  FOR_EACH_VEC_ELT (control_flow_insns, i, insn)
	    if (control_flow_insn_p (insn))
	      {
		/* Split the block after insn.  There will be a fallthru
		   edge, which is OK so we keep it.  We have to create
		   the exception edges ourselves.  */
		bb = BLOCK_FOR_INSN (insn);
		split_block (bb, insn);
		rtl_make_eh_edge (NULL, bb, BB_END (bb));
	      }
	}

      df_process_deferred_rescans ();
    }

  df_clear_flags (DF_DEFER_INSN_RESCAN);

  timevar_pop (TV_MACH_DEP);
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_x86_cse (gcc::context *ctxt)
{
  return new pass_x86_cse (ctxt);
}

/* Convert legacy instructions that clobbers EFLAGS to APX_NF
   instructions when there are no flag set between a flag
   producer and user.  */

static unsigned int
ix86_apx_nf_convert (void)
{
  timevar_push (TV_MACH_DEP);

  basic_block bb;
  rtx_insn *insn;
  hash_map <rtx_insn *, rtx> converting_map;
  auto_vec <rtx_insn *> current_convert_list;

  bool converting_seq = false;
  rtx cc = gen_rtx_REG (CCmode, FLAGS_REG);

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Reset conversion for each bb.  */
      converting_seq = false;
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  if (recog_memoized (insn) < 0)
	    continue;

	  /* Convert candidate insns after cstore, which should
	     satisify the two conditions:
	     1. Is not flag user or producer, only clobbers
	     FLAGS_REG.
	     2. Have corresponding nf pattern.  */

	  rtx pat = PATTERN (insn);

	  /* Starting convertion at first cstorecc.  */
	  rtx set = NULL_RTX;
	  if (!converting_seq
	      && (set = single_set (insn))
	      && ix86_comparison_operator (SET_SRC (set), VOIDmode)
	      && reg_overlap_mentioned_p (cc, SET_SRC (set))
	      && !reg_overlap_mentioned_p (cc, SET_DEST (set)))
	    {
	      converting_seq = true;
	      current_convert_list.truncate (0);
	    }
	  /* Terminate at the next explicit flag set.  */
	  else if (reg_set_p (cc, pat)
		   && GET_CODE (set_of (cc, pat)) != CLOBBER)
	    converting_seq = false;

	  if (!converting_seq)
	    continue;

	  if (get_attr_has_nf (insn)
	      && GET_CODE (pat) == PARALLEL)
	    {
	      /* Record the insn to candidate map.  */
	      current_convert_list.safe_push (insn);
	      converting_map.put (insn, pat);
	    }
	  /* If the insn clobbers flags but has no nf_attr,
	     revoke all previous candidates.  */
	  else if (!get_attr_has_nf (insn)
		   && reg_set_p (cc, pat)
		   && GET_CODE (set_of (cc, pat)) == CLOBBER)
	    {
	      for (auto item : current_convert_list)
		converting_map.remove (item);
	      converting_seq = false;
	    }
	}
    }

  if (!converting_map.is_empty ())
    {
      for (auto iter = converting_map.begin ();
	   iter != converting_map.end (); ++iter)
	{
	  rtx_insn *replace = (*iter).first;
	  rtx pat = (*iter).second;
	  int i, n = 0, len = XVECLEN (pat, 0);
	  rtx *new_elems = XALLOCAVEC (rtx, len);
	  rtx new_pat;
	  for (i = 0; i < len; i++)
	    {
	      rtx temp = XVECEXP (pat, 0, i);
	      if (! (GET_CODE (temp) == CLOBBER
		     && reg_overlap_mentioned_p (cc,
						 XEXP (temp, 0))))
		{
		  new_elems[n] = temp;
		  n++;
		}
	    }

	  if (n == 1)
	    new_pat = new_elems[0];
	  else
	    new_pat =
	      gen_rtx_PARALLEL (VOIDmode,
				gen_rtvec_v (n,
					     new_elems));

	  PATTERN (replace) = new_pat;
	  INSN_CODE (replace) = -1;
	  recog_memoized (replace);
	  df_insn_rescan (replace);
	}
    }

  timevar_pop (TV_MACH_DEP);
  return 0;
}


namespace {

const pass_data pass_data_apx_nf_convert =
{
  RTL_PASS, /* type */
  "apx_nfcvt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_apx_nf_convert : public rtl_opt_pass
{
public:
  pass_apx_nf_convert (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_apx_nf_convert, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return (TARGET_APX_NF
	      && optimize
	      && optimize_function_for_speed_p (cfun));
    }

  unsigned int execute (function *) final override
    {
      return ix86_apx_nf_convert ();
    }
}; // class pass_apx_nf_convert

} // anon namespace

rtl_opt_pass *
make_pass_apx_nf_convert (gcc::context *ctxt)
{
  return new pass_apx_nf_convert (ctxt);
}

/* When a hot loop can be fit into one cacheline,
   force align the loop without considering the max skip.  */
static void
ix86_align_loops ()
{
  basic_block bb;

  /* Don't do this when we don't know cache line size.  */
  if (ix86_cost->prefetch_block == 0)
    return;

  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  profile_count count_threshold = cfun->cfg->count_max / param_align_threshold;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *label = BB_HEAD (bb);
      bool has_fallthru = 0;
      edge e;
      edge_iterator ei;

      if (!LABEL_P (label))
	continue;

      profile_count fallthru_count = profile_count::zero ();
      profile_count branch_count = profile_count::zero ();

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    has_fallthru = 1, fallthru_count += e->count ();
	  else
	    branch_count += e->count ();
	}

      if (!fallthru_count.initialized_p () || !branch_count.initialized_p ())
	continue;

      if (bb->loop_father
	  && bb->loop_father->latch != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && (has_fallthru
	      ? (!(single_succ_p (bb)
		   && single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun))
		 && optimize_bb_for_speed_p (bb)
		 && branch_count + fallthru_count > count_threshold
		 && (branch_count > fallthru_count * param_align_loop_iterations))
	      /* In case there'no fallthru for the loop.
		 Nops inserted won't be executed.  */
	      : (branch_count > count_threshold
		 || (bb->count > bb->prev_bb->count * 10
		     && (bb->prev_bb->count
			 <= ENTRY_BLOCK_PTR_FOR_FN (cfun)->count / 2)))))
	{
	  rtx_insn* insn, *end_insn;
	  HOST_WIDE_INT size = 0;
	  bool padding_p = true;
	  basic_block tbb = bb;
	  unsigned cond_branch_num = 0;
	  bool detect_tight_loop_p = false;

	  for (unsigned int i = 0; i != bb->loop_father->num_nodes;
	       i++, tbb = tbb->next_bb)
	    {
	      /* Only handle continuous cfg layout. */
	      if (bb->loop_father != tbb->loop_father)
		{
		  padding_p = false;
		  break;
		}

	      FOR_BB_INSNS (tbb, insn)
		{
		  if (!NONDEBUG_INSN_P (insn))
		    continue;
		  size += ix86_min_insn_size (insn);

		  /* We don't know size of inline asm.
		     Don't align loop for call.  */
		  if (asm_noperands (PATTERN (insn)) >= 0
		      || CALL_P (insn))
		    {
		      size = -1;
		      break;
		    }
		}

	      if (size == -1 || size > ix86_cost->prefetch_block)
		{
		  padding_p = false;
		  break;
		}

	      FOR_EACH_EDGE (e, ei, tbb->succs)
		{
		  /* It could be part of the loop.  */
		  if (e->dest == bb)
		    {
		      detect_tight_loop_p = true;
		      break;
		    }
		}

	      if (detect_tight_loop_p)
		break;

	      end_insn = BB_END (tbb);
	      if (JUMP_P (end_insn))
		{
		  /* For decoded icache:
		     1. Up to two branches are allowed per Way.
		     2. A non-conditional branch is the last micro-op in a Way.
		  */
		  if (onlyjump_p (end_insn)
		      && (any_uncondjump_p (end_insn)
			  || single_succ_p (tbb)))
		    {
		      padding_p = false;
		      break;
		    }
		  else if (++cond_branch_num >= 2)
		    {
		      padding_p = false;
		      break;
		    }
		}

	    }

	  if (padding_p && detect_tight_loop_p)
	    {
	      emit_insn_before (gen_max_skip_align (GEN_INT (ceil_log2 (size)),
						    GEN_INT (0)), label);
	      /* End of function.  */
	      if (!tbb || tbb == EXIT_BLOCK_PTR_FOR_FN (cfun))
		break;
	      /* Skip bb which already fits into one cacheline.  */
	      bb = tbb;
	    }
	}
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
}

namespace {

const pass_data pass_data_align_tight_loops =
{
  RTL_PASS, /* type */
  "align_tight_loops", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_align_tight_loops : public rtl_opt_pass
{
public:
  pass_align_tight_loops (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_align_tight_loops, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return TARGET_ALIGN_TIGHT_LOOPS
	     && optimize
	     && optimize_function_for_speed_p (cfun);
    }

  unsigned int execute (function *) final override
    {
      timevar_push (TV_MACH_DEP);
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
      ix86_align_loops ();
#endif
      timevar_pop (TV_MACH_DEP);
      return 0;
    }
}; // class pass_align_tight_loops

} // anon namespace

rtl_opt_pass *
make_pass_align_tight_loops (gcc::context *ctxt)
{
  return new pass_align_tight_loops (ctxt);
}

/* This compares the priority of target features in function DECL1
   and DECL2.  It returns positive value if DECL1 is higher priority,
   negative value if DECL2 is higher priority and 0 if they are the
   same.  */

int
ix86_compare_version_priority (tree decl1, tree decl2)
{
  unsigned int priority1 = get_builtin_code_for_version (decl1, NULL);
  unsigned int priority2 = get_builtin_code_for_version (decl2, NULL);

  return (int)priority1 - (int)priority2;
}

/* V1 and V2 point to function versions with different priorities
   based on the target ISA.  This function compares their priorities.  */

static int
feature_compare (const void *v1, const void *v2)
{
  typedef struct _function_version_info
    {
      tree version_decl;
      tree predicate_chain;
      unsigned int dispatch_priority;
    } function_version_info;

  const function_version_info c1 = *(const function_version_info *)v1;
  const function_version_info c2 = *(const function_version_info *)v2;
  return (c2.dispatch_priority - c1.dispatch_priority);
}

/* This adds a condition to the basic_block NEW_BB in function FUNCTION_DECL
   to return a pointer to VERSION_DECL if the outcome of the expression
   formed by PREDICATE_CHAIN is true.  This function will be called during
   version dispatch to decide which function version to execute.  It returns
   the basic block at the end, to which more conditions can be added.  */

static basic_block
add_condition_to_bb (tree function_decl, tree version_decl,
		     tree predicate_chain, basic_block new_bb)
{
  gimple *return_stmt;
  tree convert_expr, result_var;
  gimple *convert_stmt;
  gimple *call_cond_stmt;
  gimple *if_else_stmt;

  basic_block bb1, bb2, bb3;
  edge e12, e23;

  tree cond_var, and_expr_var = NULL_TREE;
  gimple_seq gseq;

  tree predicate_decl, predicate_arg;

  push_cfun (DECL_STRUCT_FUNCTION (function_decl));

  gcc_assert (new_bb != NULL);
  gseq = bb_seq (new_bb);


  convert_expr = build1 (CONVERT_EXPR, ptr_type_node,
	     		 build_fold_addr_expr (version_decl));
  result_var = create_tmp_var (ptr_type_node);
  convert_stmt = gimple_build_assign (result_var, convert_expr);
  return_stmt = gimple_build_return (result_var);

  if (predicate_chain == NULL_TREE)
    {
      gimple_seq_add_stmt (&gseq, convert_stmt);
      gimple_seq_add_stmt (&gseq, return_stmt);
      set_bb_seq (new_bb, gseq);
      gimple_set_bb (convert_stmt, new_bb);
      gimple_set_bb (return_stmt, new_bb);
      pop_cfun ();
      return new_bb;
    }

  while (predicate_chain != NULL)
    {
      cond_var = create_tmp_var (integer_type_node);
      predicate_decl = TREE_PURPOSE (predicate_chain);
      predicate_arg = TREE_VALUE (predicate_chain);
      call_cond_stmt = gimple_build_call (predicate_decl, 1, predicate_arg);
      gimple_call_set_lhs (call_cond_stmt, cond_var);

      gimple_set_block (call_cond_stmt, DECL_INITIAL (function_decl));
      gimple_set_bb (call_cond_stmt, new_bb);
      gimple_seq_add_stmt (&gseq, call_cond_stmt);

      predicate_chain = TREE_CHAIN (predicate_chain);

      if (and_expr_var == NULL)
	and_expr_var = cond_var;
      else
	{
	  gimple *assign_stmt;
	  /* Use MIN_EXPR to check if any integer is zero?.
	     and_expr_var = min_expr <cond_var, and_expr_var>  */
	  assign_stmt = gimple_build_assign (and_expr_var,
			  build2 (MIN_EXPR, integer_type_node,
				  cond_var, and_expr_var));

	  gimple_set_block (assign_stmt, DECL_INITIAL (function_decl));
	  gimple_set_bb (assign_stmt, new_bb);
	  gimple_seq_add_stmt (&gseq, assign_stmt);
	}
    }

  if_else_stmt = gimple_build_cond (GT_EXPR, and_expr_var,
				    integer_zero_node,
				    NULL_TREE, NULL_TREE);
  gimple_set_block (if_else_stmt, DECL_INITIAL (function_decl));
  gimple_set_bb (if_else_stmt, new_bb);
  gimple_seq_add_stmt (&gseq, if_else_stmt);

  gimple_seq_add_stmt (&gseq, convert_stmt);
  gimple_seq_add_stmt (&gseq, return_stmt);
  set_bb_seq (new_bb, gseq);

  bb1 = new_bb;
  e12 = split_block (bb1, if_else_stmt);
  bb2 = e12->dest;
  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_TRUE_VALUE;

  e23 = split_block (bb2, return_stmt);

  gimple_set_bb (convert_stmt, bb2);
  gimple_set_bb (return_stmt, bb2);

  bb3 = e23->dest;
  make_edge (bb1, bb3, EDGE_FALSE_VALUE);

  remove_edge (e23);
  make_edge (bb2, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);

  pop_cfun ();

  return bb3;
}

/* This function generates the dispatch function for
   multi-versioned functions.  DISPATCH_DECL is the function which will
   contain the dispatch logic.  FNDECLS are the function choices for
   dispatch, and is a tree chain.  EMPTY_BB is the basic block pointer
   in DISPATCH_DECL in which the dispatch code is generated.  */

static int
dispatch_function_versions (tree dispatch_decl,
			    void *fndecls_p,
			    basic_block *empty_bb)
{
  tree default_decl;
  gimple *ifunc_cpu_init_stmt;
  gimple_seq gseq;
  int ix;
  tree ele;
  vec<tree> *fndecls;
  unsigned int num_versions = 0;
  unsigned int actual_versions = 0;
  unsigned int i;

  struct _function_version_info
    {
      tree version_decl;
      tree predicate_chain;
      unsigned int dispatch_priority;
    }*function_version_info;

  gcc_assert (dispatch_decl != NULL
	      && fndecls_p != NULL
	      && empty_bb != NULL);

  /*fndecls_p is actually a vector.  */
  fndecls = static_cast<vec<tree> *> (fndecls_p);

  /* At least one more version other than the default.  */
  num_versions = fndecls->length ();
  gcc_assert (num_versions >= 2);

  function_version_info = (struct _function_version_info *)
    XNEWVEC (struct _function_version_info, (num_versions - 1));

  /* The first version in the vector is the default decl.  */
  default_decl = (*fndecls)[0];

  push_cfun (DECL_STRUCT_FUNCTION (dispatch_decl));

  gseq = bb_seq (*empty_bb);
  /* Function version dispatch is via IFUNC.  IFUNC resolvers fire before
     constructors, so explicity call __builtin_cpu_init here.  */
  ifunc_cpu_init_stmt
    = gimple_build_call_vec (get_ix86_builtin (IX86_BUILTIN_CPU_INIT), vNULL);
  gimple_seq_add_stmt (&gseq, ifunc_cpu_init_stmt);
  gimple_set_bb (ifunc_cpu_init_stmt, *empty_bb);
  set_bb_seq (*empty_bb, gseq);

  pop_cfun ();


  for (ix = 1; fndecls->iterate (ix, &ele); ++ix)
    {
      tree version_decl = ele;
      tree predicate_chain = NULL_TREE;
      unsigned int priority;
      /* Get attribute string, parse it and find the right predicate decl.
	 The predicate function could be a lengthy combination of many
	 features, like arch-type and various isa-variants.  */
      priority = get_builtin_code_for_version (version_decl,
					       &predicate_chain);

      if (predicate_chain == NULL_TREE)
	continue;

      function_version_info [actual_versions].version_decl = version_decl;
      function_version_info [actual_versions].predicate_chain
	 = predicate_chain;
      function_version_info [actual_versions].dispatch_priority = priority;
      actual_versions++;
    }

  /* Sort the versions according to descending order of dispatch priority.  The
     priority is based on the ISA.  This is not a perfect solution.  There
     could still be ambiguity.  If more than one function version is suitable
     to execute,  which one should be dispatched?  In future, allow the user
     to specify a dispatch  priority next to the version.  */
  qsort (function_version_info, actual_versions,
	 sizeof (struct _function_version_info), feature_compare);

  for  (i = 0; i < actual_versions; ++i)
    *empty_bb = add_condition_to_bb (dispatch_decl,
				     function_version_info[i].version_decl,
				     function_version_info[i].predicate_chain,
				     *empty_bb);

  /* dispatch default version at the end.  */
  *empty_bb = add_condition_to_bb (dispatch_decl, default_decl,
				   NULL, *empty_bb);

  free (function_version_info);
  return 0;
}

/* This function changes the assembler name for functions that are
   versions.  If DECL is a function version and has a "target"
   attribute, it appends the attribute string to its assembler name.  */

static tree
ix86_mangle_function_version_assembler_name (tree decl, tree id)
{
  tree version_attr;
  char *attr_str;

  if (DECL_DECLARED_INLINE_P (decl)
      && lookup_attribute ("gnu_inline",
			   DECL_ATTRIBUTES (decl)))
    error_at (DECL_SOURCE_LOCATION (decl),
	      "function versions cannot be marked as %<gnu_inline%>,"
	      " bodies have to be generated");

  if (DECL_VIRTUAL_P (decl)
      || DECL_VINDEX (decl))
    sorry ("virtual function multiversioning not supported");

  version_attr = lookup_attribute ("target", DECL_ATTRIBUTES (decl));

  /* target attribute string cannot be NULL.  */
  gcc_assert (version_attr != NULL_TREE);

  attr_str = sorted_attr_string (TREE_VALUE (version_attr));

  /* Allow assembler name to be modified if already set.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    SET_DECL_RTL (decl, NULL);

  tree ret = clone_identifier (id, attr_str, true);

  XDELETEVEC (attr_str);

  return ret;
}

tree
ix86_mangle_decl_assembler_name (tree decl, tree id)
{
  /* For function version, add the target suffix to the assembler name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      cgraph_node *node = cgraph_node::get (decl);
      /* Mangle all versions when annotated with target_clones, but only
	 non-default versions when annotated with target attributes.  */
      if (DECL_FUNCTION_VERSIONED (decl)
	  && (node->is_target_clone
	      || !is_function_default_version (node->decl)))
	id = ix86_mangle_function_version_assembler_name (decl, id);
      /* Mangle the dispatched symbol but only in the case of target clones.  */
      else if (node && node->dispatcher_function && !node->is_target_clone)
	id = clone_identifier (id, "ifunc");
      else if (node && node->dispatcher_resolver_function)
	id = clone_identifier (id, "resolver");
    }
#ifdef SUBTARGET_MANGLE_DECL_ASSEMBLER_NAME
  id = SUBTARGET_MANGLE_DECL_ASSEMBLER_NAME (decl, id);
#endif

  return id;
}

/* Make a dispatcher declaration for the multi-versioned function DECL.
   Calls to DECL function will be replaced with calls to the dispatcher
   by the front-end.  Returns the decl of the dispatcher function.  */

tree
ix86_get_function_versions_dispatcher (void *decl)
{
  tree fn = (tree) decl;
  struct cgraph_node *node = NULL;
  struct cgraph_node *default_node = NULL;
  struct cgraph_function_version_info *node_v = NULL;

  tree dispatch_decl = NULL;

  struct cgraph_function_version_info *default_version_info = NULL;

  gcc_assert (fn != NULL && DECL_FUNCTION_VERSIONED (fn));

  node = cgraph_node::get (fn);
  gcc_assert (node != NULL);

  node_v = node->function_version ();
  gcc_assert (node_v != NULL);

  if (node_v->dispatcher_resolver != NULL)
    return node_v->dispatcher_resolver;

  /* The default node is always the beginning of the chain.  */
  default_version_info = node_v;
  while (default_version_info->prev != NULL)
    default_version_info = default_version_info->prev;
  default_node = default_version_info->this_node;

  /* If there is no default node, just return NULL.  */
  if (!is_function_default_version (default_node->decl))
    return NULL;

#if defined (ASM_OUTPUT_TYPE_DIRECTIVE)
  if (targetm.has_ifunc_p ())
    {
      struct cgraph_function_version_info *it_v = NULL;

      /* Right now, the dispatching is done via ifunc.  */
      dispatch_decl = make_dispatcher_decl (default_node->decl);

      /* Set the dispatcher for all the versions.  */
      it_v = default_version_info;
      while (it_v != NULL)
	{
	  it_v->dispatcher_resolver = dispatch_decl;
	  it_v = it_v->next;
	}
    }
  else
#endif
    {
      error_at (DECL_SOURCE_LOCATION (default_node->decl),
		"multiversioning needs %<ifunc%> which is not supported "
		"on this target");
    }

  return dispatch_decl;
}

/* Make the resolver function decl to dispatch the versions of
   a multi-versioned function,  DEFAULT_DECL.  IFUNC_ALIAS_DECL is
   ifunc alias that will point to the created resolver.  Create an
   empty basic block in the resolver and store the pointer in
   EMPTY_BB.  Return the decl of the resolver function.  */

static tree
make_resolver_func (const tree default_decl,
		    const tree ifunc_alias_decl,
		    basic_block *empty_bb)
{
  tree decl, type, t;

  /* The resolver function should return a (void *). */
  type = build_function_type_list (ptr_type_node, NULL_TREE);

  cgraph_node *node = cgraph_node::get (default_decl);
  gcc_assert (node && node->function_version ());

  decl = build_fn_decl (IDENTIFIER_POINTER (DECL_NAME (default_decl)), type);

  /* Set the assembler name to prevent cgraph_node attempting to mangle.  */
  SET_DECL_ASSEMBLER_NAME (decl, DECL_ASSEMBLER_NAME (default_decl));

  cgraph_node *resolver_node = cgraph_node::get_create (decl);
  resolver_node->dispatcher_resolver_function = true;

  if (node->is_target_clone)
    resolver_node->is_target_clone = true;

  tree id = ix86_mangle_decl_assembler_name
    (decl, node->function_version ()->assembler_name);
  SET_DECL_ASSEMBLER_NAME (decl, id);

  DECL_NAME (decl) = DECL_NAME (default_decl);
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;

  /* Resolver is not external, body is generated.  */
  DECL_EXTERNAL (decl) = 0;
  DECL_EXTERNAL (ifunc_alias_decl) = 0;

  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (decl) = 0;

  if (DECL_COMDAT_GROUP (default_decl)
      || TREE_PUBLIC (default_decl))
    {
      /* In this case, each translation unit with a call to this
	 versioned function will put out a resolver.  Ensure it
	 is comdat to keep just one copy.  */
      DECL_COMDAT (decl) = 1;
      make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
    }
  else
    TREE_PUBLIC (ifunc_alias_decl) = 0;

  /* Build result decl and add to function_decl. */
  t = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, ptr_type_node);
  DECL_CONTEXT (t) = decl;
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  gimplify_function_tree (decl);
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  *empty_bb = init_lowered_empty_function (decl, false,
					   profile_count::uninitialized ());

  cgraph_node::add_new_function (decl, true);
  symtab->call_cgraph_insertion_hooks (cgraph_node::get_create (decl));

  pop_cfun ();

  gcc_assert (ifunc_alias_decl != NULL);
  /* Mark ifunc_alias_decl as "ifunc" with resolver as resolver_name.  */
  DECL_ATTRIBUTES (ifunc_alias_decl)
    = make_attribute ("ifunc", IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
		      DECL_ATTRIBUTES (ifunc_alias_decl));

  /* Create the alias for dispatch to resolver here.  */
  cgraph_node::create_same_body_alias (ifunc_alias_decl, decl);
  return decl;
}

/* Generate the dispatching code body to dispatch multi-versioned function
   DECL.  The target hook is called to process the "target" attributes and
   provide the code to dispatch the right function at run-time.  NODE points
   to the dispatcher decl whose body will be created.  */

tree
ix86_generate_version_dispatcher_body (void *node_p)
{
  tree resolver_decl;
  basic_block empty_bb;
  tree default_ver_decl;
  struct cgraph_node *versn;
  struct cgraph_node *node;

  struct cgraph_function_version_info *node_version_info = NULL;
  struct cgraph_function_version_info *versn_info = NULL;

  node = (cgraph_node *)node_p;

  node_version_info = node->function_version ();
  gcc_assert (node->dispatcher_function
	      && node_version_info != NULL);

  if (node_version_info->dispatcher_resolver)
    return node_version_info->dispatcher_resolver;

  /* The first version in the chain corresponds to the default version.  */
  default_ver_decl = node_version_info->next->this_node->decl;

  /* node is going to be an alias, so remove the finalized bit.  */
  node->definition = false;

  resolver_decl = make_resolver_func (default_ver_decl,
				      node->decl, &empty_bb);

  node_version_info->dispatcher_resolver = resolver_decl;

  push_cfun (DECL_STRUCT_FUNCTION (resolver_decl));

  auto_vec<tree, 2> fn_ver_vec;

  for (versn_info = node_version_info->next; versn_info;
       versn_info = versn_info->next)
    {
      versn = versn_info->this_node;
      /* Check for virtual functions here again, as by this time it should
	 have been determined if this function needs a vtable index or
	 not.  This happens for methods in derived classes that override
	 virtual methods in base classes but are not explicitly marked as
	 virtual.  */
      if (DECL_VINDEX (versn->decl))
	sorry ("virtual function multiversioning not supported");

      fn_ver_vec.safe_push (versn->decl);
    }

  dispatch_function_versions (resolver_decl, &fn_ver_vec, &empty_bb);
  cgraph_edge::rebuild_edges ();
  pop_cfun ();
  return resolver_decl;
}


