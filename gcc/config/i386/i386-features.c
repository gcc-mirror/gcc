/* Copyright (C) 1988-2020 Free Software Foundation, Inc.

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
#include "tree-vectorizer.h"
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
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-builtins.h"
#include "i386-features.h"

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
  queue = NULL;
}

/* Free chain's data.  */

scalar_chain::~scalar_chain ()
{
  BITMAP_FREE (insns);
  BITMAP_FREE (defs);
  BITMAP_FREE (defs_conv);
  bitmap_obstack_release (NULL);
}

/* Add instruction into chains' queue.  */

void
scalar_chain::add_to_queue (unsigned insn_uid)
{
  if (bitmap_bit_p (insns, insn_uid)
      || bitmap_bit_p (queue, insn_uid))
    return;

  if (dump_file)
    fprintf (dump_file, "  Adding insn %d into chain's #%d queue\n",
	     insn_uid, chain_id);
  bitmap_set_bit (queue, insn_uid);
}

general_scalar_chain::general_scalar_chain (enum machine_mode smode_,
					    enum machine_mode vmode_)
     : scalar_chain (smode_, vmode_)
{
  insns_conv = BITMAP_ALLOC (NULL);
  n_sse_to_integer = 0;
  n_integer_to_sse = 0;
}

general_scalar_chain::~general_scalar_chain ()
{
  BITMAP_FREE (insns_conv);
}

/* For DImode conversion, mark register defined by DEF as requiring
   conversion.  */

void
general_scalar_chain::mark_dual_mode_def (df_ref def)
{
  gcc_assert (DF_REF_REG_DEF_P (def));

  /* Record the def/insn pair so we can later efficiently iterate over
     the defs to convert on insns not in the chain.  */
  bool reg_new = bitmap_set_bit (defs_conv, DF_REF_REGNO (def));
  if (!bitmap_bit_p (insns, DF_REF_INSN_UID (def)))
    {
      if (!bitmap_set_bit (insns_conv, DF_REF_INSN_UID (def))
	  && !reg_new)
	return;
      n_integer_to_sse++;
    }
  else
    {
      if (!reg_new)
	return;
      n_sse_to_integer++;
    }
 
  if (dump_file)
    fprintf (dump_file,
	     "  Mark r%d def in insn %d as requiring both modes in chain #%d\n",
	     DF_REF_REGNO (def), DF_REF_INSN_UID (def), chain_id);
}

/* For TImode conversion, it is unused.  */

void
timode_scalar_chain::mark_dual_mode_def (df_ref)
{
  gcc_unreachable ();
}

/* Check REF's chain to add new insns into a queue
   and find registers requiring conversion.  */

void
scalar_chain::analyze_register_chain (bitmap candidates, df_ref ref)
{
  df_link *chain;

  gcc_assert (bitmap_bit_p (insns, DF_REF_INSN_UID (ref))
	      || bitmap_bit_p (candidates, DF_REF_INSN_UID (ref)));
  add_to_queue (DF_REF_INSN_UID (ref));

  for (chain = DF_REF_CHAIN (ref); chain; chain = chain->next)
    {
      unsigned uid = DF_REF_INSN_UID (chain->ref);

      if (!NONDEBUG_INSN_P (DF_REF_INSN (chain->ref)))
	continue;

      if (!DF_REF_REG_MEM_P (chain->ref))
	{
	  if (bitmap_bit_p (insns, uid))
	    continue;

	  if (bitmap_bit_p (candidates, uid))
	    {
	      add_to_queue (uid);
	      continue;
	    }
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
	  mark_dual_mode_def (ref);
	}
    }
}

/* Add instruction into a chain.  */

void
scalar_chain::add_insn (bitmap candidates, unsigned int insn_uid)
{
  if (bitmap_bit_p (insns, insn_uid))
    return;

  if (dump_file)
    fprintf (dump_file, "  Adding insn %d to chain #%d\n", insn_uid, chain_id);

  bitmap_set_bit (insns, insn_uid);

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
      analyze_register_chain (candidates, ref);
  for (ref = DF_INSN_UID_USES (insn_uid); ref; ref = DF_REF_NEXT_LOC (ref))
    if (!DF_REF_REG_MEM_P (ref))
      analyze_register_chain (candidates, ref);
}

/* Build new chain starting from insn INSN_UID recursively
   adding all dependent uses and definitions.  */

void
scalar_chain::build (bitmap candidates, unsigned insn_uid)
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
      add_insn (candidates, insn_uid);
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
}

/* Return a cost of building a vector costant
   instead of using a scalar one.  */

int
general_scalar_chain::vector_const_cost (rtx exp)
{
  gcc_assert (CONST_INT_P (exp));

  if (standard_sse_constant_p (exp, vmode))
    return ix86_cost->sse_op;
  /* We have separate costs for SImode and DImode, use SImode costs
     for smaller modes.  */
  return ix86_cost->sse_load[smode == DImode ? 1 : 0];
}

/* Compute a gain for chain conversion.  */

int
general_scalar_chain::compute_convert_gain ()
{
  bitmap_iterator bi;
  unsigned insn_uid;
  int gain = 0;
  int cost = 0;

  if (dump_file)
    fprintf (dump_file, "Computing gain for chain #%d...\n", chain_id);

  /* SSE costs distinguish between SImode and DImode loads/stores, for
     int costs factor in the number of GPRs involved.  When supporting
     smaller modes than SImode the int load/store costs need to be
     adjusted as well.  */
  unsigned sse_cost_idx = smode == DImode ? 1 : 0;
  unsigned m = smode == DImode ? (TARGET_64BIT ? 1 : 2) : 1;

  EXECUTE_IF_SET_IN_BITMAP (insns, 0, insn_uid, bi)
    {
      rtx_insn *insn = DF_INSN_UID_GET (insn_uid)->insn;
      rtx def_set = single_set (insn);
      rtx src = SET_SRC (def_set);
      rtx dst = SET_DEST (def_set);
      int igain = 0;

      if (REG_P (src) && REG_P (dst))
	igain += 2 * m - ix86_cost->xmm_move;
      else if (REG_P (src) && MEM_P (dst))
	igain
	  += m * ix86_cost->int_store[2] - ix86_cost->sse_store[sse_cost_idx];
      else if (MEM_P (src) && REG_P (dst))
	igain += m * ix86_cost->int_load[2] - ix86_cost->sse_load[sse_cost_idx];
      else if (GET_CODE (src) == ASHIFT
	       || GET_CODE (src) == ASHIFTRT
	       || GET_CODE (src) == LSHIFTRT)
	{
	  if (m == 2)
	    {
	      if (INTVAL (XEXP (src, 1)) >= 32)
		igain += ix86_cost->add;
	      else
		igain += ix86_cost->shift_const;
	    }

	  igain += ix86_cost->shift_const - ix86_cost->sse_op;

	  if (CONST_INT_P (XEXP (src, 0)))
	    igain -= vector_const_cost (XEXP (src, 0));
	}
      else if (GET_CODE (src) == PLUS
	       || GET_CODE (src) == MINUS
	       || GET_CODE (src) == IOR
	       || GET_CODE (src) == XOR
	       || GET_CODE (src) == AND)
	{
	  igain += m * ix86_cost->add - ix86_cost->sse_op;
	  /* Additional gain for andnot for targets without BMI.  */
	  if (GET_CODE (XEXP (src, 0)) == NOT
	      && !TARGET_BMI)
	    igain += m * ix86_cost->add;

	  if (CONST_INT_P (XEXP (src, 0)))
	    igain -= vector_const_cost (XEXP (src, 0));
	  if (CONST_INT_P (XEXP (src, 1)))
	    igain -= vector_const_cost (XEXP (src, 1));
	}
      else if (GET_CODE (src) == NEG
	       || GET_CODE (src) == NOT)
	igain += m * ix86_cost->add - ix86_cost->sse_op - COSTS_N_INSNS (1);
      else if (GET_CODE (src) == SMAX
	       || GET_CODE (src) == SMIN
	       || GET_CODE (src) == UMAX
	       || GET_CODE (src) == UMIN)
	{
	  /* We do not have any conditional move cost, estimate it as a
	     reg-reg move.  Comparisons are costed as adds.  */
	  igain += m * (COSTS_N_INSNS (2) + ix86_cost->add);
	  /* Integer SSE ops are all costed the same.  */
	  igain -= ix86_cost->sse_op;
	}
      else if (GET_CODE (src) == COMPARE)
	{
	  /* Assume comparison cost is the same.  */
	}
      else if (CONST_INT_P (src))
	{
	  if (REG_P (dst))
	    /* DImode can be immediate for TARGET_64BIT and SImode always.  */
	    igain += m * COSTS_N_INSNS (1);
	  else if (MEM_P (dst))
	    igain += (m * ix86_cost->int_store[2]
		     - ix86_cost->sse_store[sse_cost_idx]);
	  igain -= vector_const_cost (src);
	}
      else
	gcc_unreachable ();

      if (igain != 0 && dump_file)
	{
	  fprintf (dump_file, "  Instruction gain %d for ", igain);
	  dump_insn_slim (dump_file, insn);
	}
      gain += igain;
    }

  if (dump_file)
    fprintf (dump_file, "  Instruction conversion gain: %d\n", gain);

  /* Cost the integer to sse and sse to integer moves.  */
  cost += n_sse_to_integer * ix86_cost->sse_to_integer;
  /* ???  integer_to_sse but we only have that in the RA cost table.
     Assume sse_to_integer/integer_to_sse are the same which they
     are at the moment.  */
  cost += n_integer_to_sse * ix86_cost->sse_to_integer;

  if (dump_file)
    fprintf (dump_file, "  Registers conversion cost: %d\n", cost);

  gain -= cost;

  if (dump_file)
    fprintf (dump_file, "  Total gain: %d\n", gain);

  return gain;
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
      /* We are not using this case currently.  */
      gcc_unreachable ();
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
general_scalar_chain::make_vector_copies (rtx_insn *insn, rtx reg)
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
  rtx_insn *seq = get_insns ();
  end_sequence ();
  emit_conversion_insns (seq, insn);

  if (dump_file)
    fprintf (dump_file,
	     "  Copied r%d to a vector register r%d for insn %d\n",
	     REGNO (reg), REGNO (vreg), INSN_UID (insn));
}

/* Copy the definition SRC of INSN inside the chain to DST for
   scalar uses outside of the chain.  */

void
general_scalar_chain::convert_reg (rtx_insn *insn, rtx dst, rtx src)
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

  rtx_insn *seq = get_insns ();
  end_sequence ();
  emit_conversion_insns (seq, insn);

  if (dump_file)
    fprintf (dump_file,
	     "  Copied r%d to a scalar register r%d for insn %d\n",
	     REGNO (src), REGNO (dst), INSN_UID (insn));
}

/* Convert operand OP in INSN.  We should handle
   memory operands and uninitialized registers.
   All other register uses are converted during
   registers conversion.  */

void
general_scalar_chain::convert_op (rtx *op, rtx_insn *insn)
{
  *op = copy_rtx_if_shared (*op);

  if (GET_CODE (*op) == NOT)
    {
      convert_op (&XEXP (*op, 0), insn);
      PUT_MODE (*op, vmode);
    }
  else if (MEM_P (*op))
    {
      rtx tmp = gen_reg_rtx (GET_MODE (*op));

      /* Handle movabs.  */
      if (!memory_operand (*op, GET_MODE (*op)))
	{
	  rtx tmp2 = gen_reg_rtx (GET_MODE (*op));

	  emit_insn_before (gen_rtx_SET (tmp2, *op), insn);
	  *op = tmp2;
	}

      emit_insn_before (gen_rtx_SET (gen_rtx_SUBREG (vmode, tmp, 0),
				     gen_gpr_to_xmm_move_src (vmode, *op)),
			insn);
      *op = gen_rtx_SUBREG (vmode, tmp, 0);

      if (dump_file)
	fprintf (dump_file, "  Preloading operand for insn %d into r%d\n",
		 INSN_UID (insn), REGNO (tmp));
    }
  else if (REG_P (*op))
    {
      *op = gen_rtx_SUBREG (vmode, *op, 0);
    }
  else if (CONST_INT_P (*op))
    {
      rtx vec_cst;
      rtx tmp = gen_rtx_SUBREG (vmode, gen_reg_rtx (smode), 0);

      /* Prefer all ones vector in case of -1.  */
      if (constm1_operand (*op, GET_MODE (*op)))
	vec_cst = CONSTM1_RTX (vmode);
      else
	{
	  unsigned n = GET_MODE_NUNITS (vmode);
	  rtx *v = XALLOCAVEC (rtx, n);
	  v[0] = *op;
	  for (unsigned i = 1; i < n; ++i)
	    v[i] = const0_rtx;
	  vec_cst = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (n, v));
	}

      if (!standard_sse_constant_p (vec_cst, vmode))
	{
	  start_sequence ();
	  vec_cst = validize_mem (force_const_mem (vmode, vec_cst));
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  emit_insn_before (seq, insn);
	}

      emit_insn_before (gen_move_insn (copy_rtx (tmp), vec_cst), insn);
      *op = tmp;
    }
  else
    {
      gcc_assert (SUBREG_P (*op));
      gcc_assert (GET_MODE (*op) == vmode);
    }
}

/* Convert INSN to vector mode.  */

void
general_scalar_chain::convert_insn (rtx_insn *insn)
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
  else if (REG_P (dst))
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
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      convert_op (&XEXP (src, 0), insn);
      PUT_MODE (src, vmode);
      break;

    case PLUS:
    case MINUS:
    case IOR:
    case XOR:
    case AND:
    case SMAX:
    case SMIN:
    case UMAX:
    case UMIN:
      convert_op (&XEXP (src, 0), insn);
      convert_op (&XEXP (src, 1), insn);
      PUT_MODE (src, vmode);
      break;

    case NEG:
      src = XEXP (src, 0);
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
      src = SUBREG_REG (XEXP (XEXP (src, 0), 0));

      gcc_assert (REG_P (src) && GET_MODE (src) == DImode);
      subreg = gen_rtx_SUBREG (V2DImode, src, 0);
      emit_insn_before (gen_vec_interleave_lowv2di (copy_rtx_if_shared (subreg),
						    copy_rtx_if_shared (subreg),
						    copy_rtx_if_shared (subreg)),
			insn);
      dst = gen_rtx_REG (CCmode, FLAGS_REG);
      src = gen_rtx_UNSPEC (CCmode, gen_rtvec (2, copy_rtx_if_shared (subreg),
					       copy_rtx_if_shared (subreg)),
			    UNSPEC_PTEST);
      break;

    case CONST_INT:
      convert_op (&src, insn);
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

  switch (GET_CODE (dst))
    {
    case REG:
      {
	rtx tmp = find_reg_equal_equiv_note (insn);
	if (tmp)
	  PUT_MODE (XEXP (tmp, 0), V1TImode);
	PUT_MODE (dst, V1TImode);
	fix_debug_reg_uses (dst);
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
      PUT_MODE (src, V1TImode);
      /* Call fix_debug_reg_uses only if SRC is never defined.  */
      if (!DF_REG_DEF_CHAIN (REGNO (src)))
	fix_debug_reg_uses (src);
      break;

    case MEM:
      PUT_MODE (src, V1TImode);
      break;

    case CONST_WIDE_INT:
      if (NONDEBUG_INSN_P (insn))
	{
	  /* Since there are no instructions to store 128-bit constant,
	     temporary register usage is required.  */
	  rtx tmp = gen_reg_rtx (V1TImode);
	  start_sequence ();
	  src = gen_rtx_CONST_VECTOR (V1TImode, gen_rtvec (1, src));
	  src = validize_mem (force_const_mem (V1TImode, src));
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  if (seq)
	    emit_insn_before (seq, insn);
	  emit_conversion_insns (gen_rtx_SET (dst, tmp), insn);
	  dst = tmp;
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
      if (NONDEBUG_INSN_P (insn))
	{
	  rtx tmp = gen_reg_rtx (V1TImode);
	  /* Since there are no instructions to store standard SSE
	     constant, temporary register usage is required.  */
	  emit_conversion_insns (gen_rtx_SET (dst, tmp), insn);
	  dst = tmp;
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
  recog_memoized (insn);
  df_insn_rescan (insn);
}

/* Generate copies from defs used by the chain but not defined therein.
   Also populates defs_map which is used later by convert_insn.  */

void
general_scalar_chain::convert_registers ()
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
      convert_insn (DF_INSN_UID_GET (id)->insn);
      converted_insns++;
    }

  return converted_insns;
}

/* Return 1 if INSN uses or defines a hard register.
   Hard register uses in a memory address are ignored.
   Clobbers and flags definitions are ignored.  */

static bool
has_non_address_hard_reg (rtx_insn *insn)
{
  df_ref ref;
  FOR_EACH_INSN_DEF (ref, insn)
    if (HARD_REGISTER_P (DF_REF_REAL_REG (ref))
	&& !DF_REF_FLAGS_IS_SET (ref, DF_REF_MUST_CLOBBER)
	&& DF_REF_REGNO (ref) != FLAGS_REG)
      return true;

  FOR_EACH_INSN_USE (ref, insn)
    if (!DF_REF_REG_MEM_P (ref) && HARD_REGISTER_P (DF_REF_REAL_REG (ref)))
      return true;

  return false;
}

/* Check if comparison INSN may be transformed
   into vector comparison.  Currently we transform
   zero checks only which look like:

   (set (reg:CCZ 17 flags)
        (compare:CCZ (ior:SI (subreg:SI (reg:DI x) 4)
                             (subreg:SI (reg:DI x) 0))
		     (const_int 0 [0])))  */

static bool
convertible_comparison_p (rtx_insn *insn, enum machine_mode mode)
{
  /* ??? Currently convertible for double-word DImode chain only.  */
  if (TARGET_64BIT || mode != DImode)
    return false;

  if (!TARGET_SSE4_1)
    return false;

  rtx def_set = single_set (insn);

  gcc_assert (def_set);

  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);

  gcc_assert (GET_CODE (src) == COMPARE);

  if (GET_CODE (dst) != REG
      || REGNO (dst) != FLAGS_REG
      || GET_MODE (dst) != CCZmode)
    return false;

  rtx op1 = XEXP (src, 0);
  rtx op2 = XEXP (src, 1);

  if (op2 != CONST0_RTX (GET_MODE (op2)))
    return false;

  if (GET_CODE (op1) != IOR)
    return false;

  op2 = XEXP (op1, 1);
  op1 = XEXP (op1, 0);

  if (!SUBREG_P (op1)
      || !SUBREG_P (op2)
      || GET_MODE (op1) != SImode
      || GET_MODE (op2) != SImode
      || ((SUBREG_BYTE (op1) != 0
	   || SUBREG_BYTE (op2) != GET_MODE_SIZE (SImode))
	  && (SUBREG_BYTE (op2) != 0
	      || SUBREG_BYTE (op1) != GET_MODE_SIZE (SImode))))
    return false;

  op1 = SUBREG_REG (op1);
  op2 = SUBREG_REG (op2);

  if (op1 != op2
      || !REG_P (op1)
      || GET_MODE (op1) != DImode)
    return false;

  return true;
}

/* The general version of scalar_to_vector_candidate_p.  */

static bool
general_scalar_to_vector_candidate_p (rtx_insn *insn, enum machine_mode mode)
{
  rtx def_set = single_set (insn);

  if (!def_set)
    return false;

  if (has_non_address_hard_reg (insn))
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
    case ASHIFTRT:
      if (!TARGET_AVX512VL)
	return false;
      /* FALLTHRU */

    case ASHIFT:
    case LSHIFTRT:
      if (!CONST_INT_P (XEXP (src, 1))
	  || !IN_RANGE (INTVAL (XEXP (src, 1)), 0, GET_MODE_BITSIZE (mode)-1))
	return false;
      break;

    case SMAX:
    case SMIN:
    case UMAX:
    case UMIN:
      if ((mode == DImode && !TARGET_AVX512VL)
	  || (mode == SImode && !TARGET_SSE4_1))
	return false;
      /* Fallthru.  */

    case PLUS:
    case MINUS:
    case IOR:
    case XOR:
    case AND:
      if (!REG_P (XEXP (src, 1))
	  && !MEM_P (XEXP (src, 1))
	  && !CONST_INT_P (XEXP (src, 1)))
	return false;

      if (GET_MODE (XEXP (src, 1)) != mode
	  && !CONST_INT_P (XEXP (src, 1)))
	return false;
      break;

    case NEG:
    case NOT:
      break;

    case REG:
      return true;

    case MEM:
    case CONST_INT:
      return REG_P (dst);

    default:
      return false;
    }

  if (!REG_P (XEXP (src, 0))
      && !MEM_P (XEXP (src, 0))
      && !CONST_INT_P (XEXP (src, 0))
      /* Check for andnot case.  */
      && (GET_CODE (src) != AND
	  || GET_CODE (XEXP (src, 0)) != NOT
	  || !REG_P (XEXP (XEXP (src, 0), 0))))
      return false;

  if (GET_MODE (XEXP (src, 0)) != mode
      && !CONST_INT_P (XEXP (src, 0)))
    return false;

  return true;
}

/* The TImode version of scalar_to_vector_candidate_p.  */

static bool
timode_scalar_to_vector_candidate_p (rtx_insn *insn)
{
  rtx def_set = single_set (insn);

  if (!def_set)
    return false;

  if (has_non_address_hard_reg (insn))
    return false;

  rtx src = SET_SRC (def_set);
  rtx dst = SET_DEST (def_set);

  /* Only TImode load and store are allowed.  */
  if (GET_MODE (dst) != TImode)
    return false;

  if (MEM_P (dst))
    {
      /* Check for store.  Memory must be aligned or unaligned store
	 is optimal.  Only support store from register, standard SSE
	 constant or CONST_WIDE_INT generated from piecewise store.

	 ??? Verify performance impact before enabling CONST_INT for
	 __int128 store.  */
      if (misaligned_operand (dst, TImode)
	  && !TARGET_SSE_UNALIGNED_STORE_OPTIMAL)
	return false;

      switch (GET_CODE (src))
	{
	default:
	  return false;

	case REG:
	case CONST_WIDE_INT:
	  return true;

	case CONST_INT:
	  return standard_sse_constant_p (src, TImode);
	}
    }
  else if (MEM_P (src))
    {
      /* Check for load.  Memory must be aligned or unaligned load is
	 optimal.  */
      return (REG_P (dst)
	      && (!misaligned_operand (src, TImode)
		  || TARGET_SSE_UNALIGNED_LOAD_OPTIMAL));
    }

  return false;
}

/* For a register REGNO, scan instructions for its defs and uses.
   Put REGNO in REGS if a def or use isn't in CANDIDATES.  */

static void
timode_check_non_convertible_regs (bitmap candidates, bitmap regs,
				   unsigned int regno)
{
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

/* The TImode version of remove_non_convertible_regs.  */

static void
timode_remove_non_convertible_regs (bitmap candidates)
{
  bitmap_iterator bi;
  unsigned id;
  bitmap regs = BITMAP_ALLOC (NULL);

  EXECUTE_IF_SET_IN_BITMAP (candidates, 0, id, bi)
    {
      rtx def_set = single_set (DF_INSN_UID_GET (id)->insn);
      rtx dest = SET_DEST (def_set);
      rtx src = SET_SRC (def_set);

      if ((!REG_P (dest)
	   || bitmap_bit_p (regs, REGNO (dest))
	   || HARD_REGISTER_P (dest))
	  && (!REG_P (src)
	      || bitmap_bit_p (regs, REGNO (src))
	      || HARD_REGISTER_P (src)))
	continue;

      if (REG_P (dest))
	timode_check_non_convertible_regs (candidates, regs,
					   REGNO (dest));

      if (REG_P (src))
	timode_check_non_convertible_regs (candidates, regs,
					   REGNO (src));
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
	  }
    }

  BITMAP_FREE (regs);
}

/* Main STV pass function.  Find and convert scalar
   instructions into vector mode when profitable.  */

static unsigned int
convert_scalars_to_vector (bool timode_p)
{
  basic_block bb;
  int converted_insns = 0;

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
	chain->build (&candidates[i], uid);

	if (chain->compute_convert_gain () > 0)
	  converted_insns += chain->convert ();
	else
	  if (dump_file)
	    fprintf (dump_file, "Chain #%d conversion is not profitable\n",
		     chain->chain_id);

	delete chain;
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
    }

  return 0;
}

/* Modify the vzeroupper pattern in INSN so that it describes the effect
   that the instruction has on the SSE registers.  LIVE_REGS are the set
   of registers that are live across the instruction.

   For a live register R we use:

     (set (reg:V2DF R) (reg:V2DF R))

   which preserves the low 128 bits but clobbers the upper bits.  */

static void
ix86_add_reg_usage_to_vzeroupper (rtx_insn *insn, bitmap live_regs)
{
  rtx pattern = PATTERN (insn);
  unsigned int nregs = TARGET_64BIT ? 16 : 8;
  unsigned int npats = nregs;
  for (unsigned int i = 0; i < nregs; ++i)
    {
      unsigned int regno = GET_SSE_REGNO (i);
      if (!bitmap_bit_p (live_regs, regno))
	npats--;
    }
  if (npats == 0)
    return;
  rtvec vec = rtvec_alloc (npats + 1);
  RTVEC_ELT (vec, 0) = XVECEXP (pattern, 0, 0);
  for (unsigned int i = 0, j = 0; i < nregs; ++i)
    {
      unsigned int regno = GET_SSE_REGNO (i);
      if (!bitmap_bit_p (live_regs, regno))
	continue;
      rtx reg = gen_rtx_REG (V2DImode, regno);
      ++j;
      RTVEC_ELT (vec, j) = gen_rtx_SET (reg, reg);
    }
  XVEC (pattern, 0) = vec;
  INSN_CODE (insn) = -1;
  df_insn_rescan (insn);
}

/* Walk the vzeroupper instructions in the function and annotate them
   with the effect that they have on the SSE registers.  */

static void
ix86_add_reg_usage_to_vzerouppers (void)
{
  basic_block bb;
  rtx_insn *insn;
  auto_bitmap live_regs;

  df_analyze ();
  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_copy (live_regs, df_get_live_out (bb));
      df_simulate_initialize_backwards (bb, live_regs);
      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn))
	    continue;
	  if (vzeroupper_pattern (PATTERN (insn), VOIDmode))
	    ix86_add_reg_usage_to_vzeroupper (insn, live_regs);
	  df_simulate_one_insn_backwards (bb, insn, live_regs);
	}
    }
}

static unsigned int
rest_of_handle_insert_vzeroupper (void)
{
  if (TARGET_VZEROUPPER
      && flag_expensive_optimizations
      && !optimize_size)
    {
      /* vzeroupper instructions are inserted immediately after reload to
	 account for possible spills from 256bit or 512bit registers.  The pass
	 reuses mode switching infrastructure by re-running mode insertion
	 pass, so disable entities that have already been processed.  */
      for (int i = 0; i < MAX_386_ENTITIES; i++)
	ix86_optimize_mode_switching[i] = 0;

      ix86_optimize_mode_switching[AVX_U128] = 1;

      /* Call optimize_mode_switching.  */
      g->get_passes ()->execute_pass_mode_switching ();
    }
  ix86_add_reg_usage_to_vzerouppers ();
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
  virtual bool gate (function *)
    {
      return TARGET_AVX
	     && ((TARGET_VZEROUPPER
		  && flag_expensive_optimizations
		  && !optimize_size)
		 || cfun->machine->has_explicit_vzeroupper);
    }

  virtual unsigned int execute (function *)
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
  virtual bool gate (function *)
    {
      return ((!timode_p || TARGET_64BIT)
	      && TARGET_STV && TARGET_SSE2 && optimize > 1);
    }

  virtual unsigned int execute (function *)
    {
      return convert_scalars_to_vector (timode_p);
    }

  opt_pass *clone ()
    {
      return new pass_stv (m_ctxt);
    }

  void set_pass_param (unsigned int n, bool param)
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

/* Inserting ENDBRANCH instructions.  */

static unsigned int
rest_of_insert_endbranch (void)
{
  timevar_push (TV_MACH_DEP);

  rtx cet_eb;
  rtx_insn *insn;
  basic_block bb;

  /* Currently emit EB if it's a tracking function, i.e. 'nocf_check' is
     absent among function attributes.  Later an optimization will be
     introduced to make analysis if an address of a static function is
     taken.  A static function whose address is not taken will get a
     nocf_check attribute.  This will allow to reduce the number of EB.  */

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
      /* Queue ENDBR insertion to x86_function_profiler.  */
      if (crtl->profile && flag_fentry)
	cfun->machine->endbr_queued_at_entrance = true;
      else
	{
	  cet_eb = gen_nop_endbr ();

	  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
	  insn = BB_HEAD (bb);
	  emit_insn_before (cet_eb, insn);
	}
    }

  bb = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (CALL_P (insn))
	    {
	      bool need_endbr;
	      need_endbr = find_reg_note (insn, REG_SETJMP, NULL) != NULL;
	      if (!need_endbr && !SIBLING_CALL_P (insn))
		{
		  rtx call = get_call_rtx_from (insn);
		  rtx fnaddr = XEXP (call, 0);
		  tree fndecl = NULL_TREE;

		  /* Also generate ENDBRANCH for non-tail call which
		     may return via indirect branch.  */
		  if (GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF)
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

	      cet_eb = gen_nop_endbr ();
	      emit_insn_after_setloc (cet_eb, insn, INSN_LOCATION (insn));
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
		  cet_eb = gen_nop_endbr ();
		  emit_insn_after (cet_eb, insn);
		}
	      continue;
	    }

	  if (LABEL_P (insn) && LABEL_PRESERVE_P (insn))
	    {
	      cet_eb = gen_nop_endbr ();
	      emit_insn_after (cet_eb, insn);
	      continue;
	    }
	}
    }

  timevar_pop (TV_MACH_DEP);
  return 0;
}

namespace {

const pass_data pass_data_insert_endbranch =
{
  RTL_PASS, /* type.  */
  "cet", /* name.  */
  OPTGROUP_NONE, /* optinfo_flags.  */
  TV_MACH_DEP, /* tv_id.  */
  0, /* properties_required.  */
  0, /* properties_provided.  */
  0, /* properties_destroyed.  */
  0, /* todo_flags_start.  */
  0, /* todo_flags_finish.  */
};

class pass_insert_endbranch : public rtl_opt_pass
{
public:
  pass_insert_endbranch (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_insert_endbranch, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return ((flag_cf_protection & CF_BRANCH));
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_insert_endbranch ();
    }

}; // class pass_insert_endbranch

} // anon namespace

rtl_opt_pass *
make_pass_insert_endbranch (gcc::context *ctxt)
{
  return new pass_insert_endbranch (ctxt);
}

/* At entry of the nearest common dominator for basic blocks with
   conversions, generate a single
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

	  if (!v4sf_const0)
	    v4sf_const0 = gen_reg_rtx (V4SFmode);

	  /* Convert PARTIAL_XMM_UPDATE_TRUE insns, DF -> SF, SF -> DF,
	     SI -> SF, SI -> DF, DI -> SF, DI -> DF, to vec_dup and
	     vec_merge with subreg.  */
	  rtx src = SET_SRC (set);
	  rtx dest = SET_DEST (set);
	  machine_mode dest_mode = GET_MODE (dest);

	  rtx zero;
	  machine_mode dest_vecmode;
	  if (dest_mode == E_SFmode)
	    {
	      dest_vecmode = V4SFmode;
	      zero = v4sf_const0;
	    }
	  else
	    {
	      dest_vecmode = V2DFmode;
	      zero = gen_rtx_SUBREG (V2DFmode, v4sf_const0, 0);
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
	  df_insn_rescan (set_insn);

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

      /* Generate a vxorps at entry of the nearest dominator for basic
	 blocks with conversions, which is in the fake loop that
	 contains the whole function, so that there is only a single
	 vxorps in the whole function.   */
      bb = nearest_common_dominator_for_set (CDI_DOMINATORS,
					     convert_bbs);
      while (bb->loop_father->latch
	     != EXIT_BLOCK_PTR_FOR_FN (cfun))
	bb = get_immediate_dominator (CDI_DOMINATORS,
				      bb->loop_father->header);

      set = gen_rtx_SET (v4sf_const0, CONST0_RTX (V4SFmode));

      insn = BB_HEAD (bb);
      while (insn && !NONDEBUG_INSN_P (insn))
	{
	  if (insn == BB_END (bb))
	    {
	      insn = NULL;
	      break;
	    }
	  insn = NEXT_INSN (insn);
	}
      if (insn == BB_HEAD (bb))
        set_insn = emit_insn_before (set, insn);
      else
	set_insn = emit_insn_after (set,
				    insn ? PREV_INSN (insn) : BB_END (bb));
      df_insn_rescan (set_insn);
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
  virtual bool gate (function *)
    {
      return (TARGET_AVX
	      && TARGET_SSE_PARTIAL_REG_DEPENDENCY
	      && TARGET_SSE_MATH
	      && optimize
	      && optimize_function_for_speed_p (cfun));
    }

  virtual unsigned int execute (function *)
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
  const char *orig_name, *version_string;
  char *attr_str, *assembler_name;

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

  orig_name = IDENTIFIER_POINTER (id);
  version_string
    = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (version_attr)));

  if (strcmp (version_string, "default") == 0)
    return id;

  attr_str = sorted_attr_string (TREE_VALUE (version_attr));
  assembler_name = XNEWVEC (char, strlen (orig_name) + strlen (attr_str) + 2);

  sprintf (assembler_name, "%s.%s", orig_name, attr_str);

  /* Allow assembler name to be modified if already set.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    SET_DECL_RTL (decl, NULL);

  tree ret = get_identifier (assembler_name);
  XDELETEVEC (attr_str);
  XDELETEVEC (assembler_name);
  return ret;
}

tree 
ix86_mangle_decl_assembler_name (tree decl, tree id)
{
  /* For function version, add the target suffix to the assembler name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_FUNCTION_VERSIONED (decl))
    id = ix86_mangle_function_version_assembler_name (decl, id);
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
  struct cgraph_function_version_info *first_v = NULL;

  tree dispatch_decl = NULL;

  struct cgraph_function_version_info *default_version_info = NULL;
 
  gcc_assert (fn != NULL && DECL_FUNCTION_VERSIONED (fn));

  node = cgraph_node::get (fn);
  gcc_assert (node != NULL);

  node_v = node->function_version ();
  gcc_assert (node_v != NULL);
 
  if (node_v->dispatcher_resolver != NULL)
    return node_v->dispatcher_resolver;

  /* Find the default version and make it the first node.  */
  first_v = node_v;
  /* Go to the beginning of the chain.  */
  while (first_v->prev != NULL)
    first_v = first_v->prev;
  default_version_info = first_v;
  while (default_version_info != NULL)
    {
      if (is_function_default_version
	    (default_version_info->this_node->decl))
        break;
      default_version_info = default_version_info->next;
    }

  /* If there is no default node, just return NULL.  */
  if (default_version_info == NULL)
    return NULL;

  /* Make default info the first node.  */
  if (first_v != default_version_info)
    {
      default_version_info->prev->next = default_version_info->next;
      if (default_version_info->next)
        default_version_info->next->prev = default_version_info->prev;
      first_v->prev = default_version_info;
      default_version_info->next = first_v;
      default_version_info->prev = NULL;
    }

  default_node = default_version_info->this_node;

#if defined (ASM_OUTPUT_TYPE_DIRECTIVE)
  if (targetm.has_ifunc_p ())
    {
      struct cgraph_function_version_info *it_v = NULL;
      struct cgraph_node *dispatcher_node = NULL;
      struct cgraph_function_version_info *dispatcher_version_info = NULL;

      /* Right now, the dispatching is done via ifunc.  */
      dispatch_decl = make_dispatcher_decl (default_node->decl);

      dispatcher_node = cgraph_node::get_create (dispatch_decl);
      gcc_assert (dispatcher_node != NULL);
      dispatcher_node->dispatcher_function = 1;
      dispatcher_version_info
	= dispatcher_node->insert_new_function_version ();
      dispatcher_version_info->next = default_version_info;
      dispatcher_node->definition = 1;

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

  /* Create resolver function name based on default_decl.  */
  tree decl_name = clone_function_name (default_decl, "resolver");
  const char *resolver_name = IDENTIFIER_POINTER (decl_name);

  /* The resolver function should return a (void *). */
  type = build_function_type_list (ptr_type_node, NULL_TREE);

  decl = build_fn_decl (resolver_name, type);
  SET_DECL_ASSEMBLER_NAME (decl, decl_name);

  DECL_NAME (decl) = decl_name;
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
    = make_attribute ("ifunc", resolver_name,
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


