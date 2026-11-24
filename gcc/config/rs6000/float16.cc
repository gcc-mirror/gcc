/* Subroutines for the C front end on the PowerPC architecture.
   Copyright (C) 2002-2025 Free Software Foundation, Inc.

   Contributed by Zack Weinberg <zack@codesourcery.com>
   and Paolo Bonzini <bonzini@gnu.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* 16-bit floating point support.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "insn-attr.h"
#include "flags.h"
#include "attribs.h"
#include "emit-rtl.h"
#include "explow.h"
#include "expr.h"
#include "common/common-target.h"
#include "rs6000-internal.h"

/* Expand a 16-bit vector operation:

   ICODE:   Operation to perform.
   RESULT:  Result of the operation.
   OP1:     Input operand1.
   OP2:     Input operand2.
   OP3:     Input operand3 or NULL_RTX.
   SUBTYPE: Describe the operation.  */
	
void
fp16_vectorization (enum rtx_code icode,
		    rtx result,
		    rtx op1,
		    rtx op2,
		    rtx op3,
		    enum fp16_operation subtype)
{
  gcc_assert (can_create_pseudo_p ());

  machine_mode result_mode = GET_MODE (result);
  rtx op_orig[3] = { op1, op2, op3 };
  rtx op_hi[3];
  rtx op_lo[3];
  rtx result_hi;
  rtx result_lo;
  size_t n_opts;

  switch (subtype)
    {
    case FP16_BINARY:
      n_opts = 2;
      break;

    case FP16_FMA:
    case FP16_FMS:
    case FP16_NFMA:
    case FP16_NFMS:
      n_opts = 3;
      break;

    default:
      gcc_unreachable ();
    }

  /* Allocate 2 temporaries for the results and the input operands.  */
  result_hi = gen_reg_rtx (V4SFmode);
  result_lo = gen_reg_rtx (V4SFmode);

  for (size_t i = 0; i < n_opts; i++)
    {
      gcc_assert (op_orig[i] != NULL_RTX);
      op_hi[i] = gen_reg_rtx (V4SFmode);	/* high register.  */
      op_lo[i] = gen_reg_rtx (V4SFmode);	/* low register.  */

      rtx interleave_hi = gen_reg_rtx (result_mode);
      rtx interleave_lo = gen_reg_rtx (result_mode);
      rtx orig = op_orig[i];

      rs6000_expand_interleave (interleave_hi, orig, orig, !BYTES_BIG_ENDIAN);
      rs6000_expand_interleave (interleave_lo, orig, orig,  BYTES_BIG_ENDIAN);

      if (result_mode == V8HFmode)
	{
	  emit_insn (gen_xvcvhpsp_v8hf (op_hi[i], interleave_hi));
	  emit_insn (gen_xvcvhpsp_v8hf (op_lo[i], interleave_lo));
	}

      else if (result_mode == V8BFmode)
	{
	  emit_insn (gen_xvcvbf16spn_v8bf (op_hi[i], interleave_hi));
	  emit_insn (gen_xvcvbf16spn_v8bf (op_lo[i], interleave_lo));
	}

      else
	gcc_unreachable ();
    }

  /* Do 2 sets of V4SFmode operations.  */
  switch (subtype)
    {
    case FP16_BINARY:
      emit_insn (gen_rtx_SET (result_hi,
			      gen_rtx_fmt_ee (icode, V4SFmode,
					      op_hi[0],
					      op_hi[1])));

      emit_insn (gen_rtx_SET (result_lo,
			      gen_rtx_fmt_ee (icode, V4SFmode,
					      op_lo[0],
					      op_lo[1])));
      break;

    case FP16_FMA:
    case FP16_FMS:
    case FP16_NFMA:
    case FP16_NFMS:
      {
	rtx op1_hi = op_hi[0];
	rtx op2_hi = op_hi[1];
	rtx op3_hi = op_hi[2];

	rtx op1_lo = op_lo[0];
	rtx op2_lo = op_lo[1];
	rtx op3_lo = op_lo[2];

	if (subtype == FP16_FMS || subtype == FP16_NFMS)
	  {
	    op3_hi = gen_rtx_NEG (V4SFmode, op3_hi);
	    op3_lo = gen_rtx_NEG (V4SFmode, op3_lo);
	  }

	rtx op_fma_hi = gen_rtx_FMA (V4SFmode, op1_hi, op2_hi, op3_hi);
	rtx op_fma_lo = gen_rtx_FMA (V4SFmode, op1_lo, op2_lo, op3_lo);

	if (subtype == FP16_NFMA || subtype == FP16_NFMS)
	  {
	    op_fma_hi = gen_rtx_NEG (V4SFmode, op_fma_hi);
	    op_fma_lo = gen_rtx_NEG (V4SFmode, op_fma_lo);
	  }

	emit_insn (gen_rtx_SET (result_hi, op_fma_hi));
	emit_insn (gen_rtx_SET (result_lo, op_fma_lo));
      }
      break;

    default:
      gcc_unreachable ();
    }

  /* Combine the 2 V4SFmode operations into one V8HFmode/V8BFmode vector.  */
  if (result_mode == V8HFmode)
    emit_insn (gen_vec_pack_trunc_v4sf_v8hf (result, result_hi, result_lo));

  else if (result_mode == V8BFmode)
    emit_insn (gen_vec_pack_trunc_v4sf_v8bf (result, result_hi, result_lo));

  else
    gcc_unreachable ();

  return;
}

/* Decide whether an expression can optimize BFmode to use V4SFmode.

   On Power10, we do not have scalar conversion functions between BFmode and
   SFmode/DFmode.

   To convert from BFmode to the DFmode scalar format we have to do a shift
   left by 48 bits to convert the value into the SFmode format in the top 32
   bits of a VSX register, and then do a XSCVSPDPN instruction to convert it to
   the DFmode used for scalar instructions.

   To convert from DFmode to BFmode, we have to convert the DFmode value into
   the SFmode memory format via the XSCVDPSP instruction, and then use the
   vector XVCVSPBF16 instruction to convert the V4SFmode vector to a V8BFmode
   vector and use the appropriate element.

   This code determines whether a given RTX can be converted to use the V4SF
   vector instructions.

   We allow multiple operations (a + b + c) to be folded together to try to do
   as much possible in the vector unit without having to convert back to scalar
   mode.

   We also fold in loads from memory to simplify the conversion.

   The expression that is combined is split before register allocation.

   For example:

	__bf16 *p, *q, *r;
	*p = *q + *r;

   the old vs. new code would be:

	old code			new code
	========			========

	lhz         9,0(4)		lxsihzx      0,0,4
	lhz         2,0(5)		lxsihzx      12,0,5
	sldi        9,9,48		xxspltw       0,0,1
	sldi        2,2,48		xxspltw      12,12,1
	mtvsrd      12,9		xvcvbf16spn  0,0
	mtvsrd      11,2		xvcvbf16spn  12,12
	xscvspdpn   0,12		xvaddsp      0,0,12
	xscvspdpn   12,11		xvcvspbf16   0,0
	fadds       0,0,12		stxsihx      0,0,3
	xscvdpsp    0,0
	xvcvspbf16  0,0
	stxsihx     0,0,3

   We can combine more operations to do as much of the calculation using
   V4SFmode arithmetic, for example:

	__bf16 *p, *q, *r, *s;
	*p = *q + *r + *s;

   The old vs. new code would be:

	old code			new code
	========			========

	lhz         10,0(5)		lxsihzx      0,0,5
	lhz         9,0(6)		lxsihzx      11,0,4
	lhz         2,0(4)		lxsihzx      12,0,6
	sldi        10,10,48		xxspltw      0,0,1
	sldi        9,9,48		xxspltw      11,11,1
	sldi        2,2,48		xxspltw      12,12,1
	mtvsrd      12,10		xvcvbf16spn  0,0
	mtvsrd      10,2		xvcvbf16spn  11,11
	xscvspdpn   0,12		xvcvbf16spn  12,12
	mtvsrd      12,9		xvaddsp      0,0,11
	xscvspdpn   11,12		xvaddsp      0,0,12
	xscvspdpn   12,10		xvcvspbf     16 0,0
	fadds       0,0,11		stxsihx      0,0,3
	fadds       0,0,12
	xscvdpsp    0,0
	xvcvspbf16  0,0
	stxsihx     0,0,3

   For non-vectorized power10 code, I measured that using the V4SFmode vector
   instructions were 20-45 times faster than using the scalar conversions and
   scalar SFmode instructions.  The HFmode code which has the appropriate
   scalar conversions operates at roughly the same speed as the BFmode code
   using the V4SFmode vector instructions.  */

/* Stats for whole expression.  */

struct bfloat16_to_v4sf_stats
{
  unsigned num_bf_regs;		/* # BF registers used.  */
  unsigned num_bf_loads;	/* # BF values loaded from memory.  */
  unsigned num_sf_regs;		/* # SF registers used.  */
  unsigned num_const;		/* # constants used.  */
  unsigned num_binary;		/* # binary operations done.  */
  unsigned num_fma;		/* # FMA (fused multiply-adds) done.  */
  unsigned num_unary;		/* # unary ops not folded into FMA/ABS.  */
  unsigned num_neg_folded;	/* # negates folded into a FMA/ABS insn.  */
  unsigned num_extend;		/* # FLOAT_EXTENDs.  */
};

/* Recursively evaluate whether a RTX expression can be optimized to use
   V4SFmode operations.  Return true if the expression might be able to be
   optimized, and update the stats.  */

static bool
bfloat16_to_v4sf_inner (rtx op, struct bfloat16_to_v4sf_stats *stats)
{
  machine_mode mode = GET_MODE (op);
  rtx arg;
  rtx_code code;

  if (mode != BFmode && mode != SFmode)
    return false;

  switch (GET_CODE (op))
    {
    default:
      break;

    // Update register counts.
    case REG:
    case SUBREG:
      if (mode == BFmode)
	stats->num_bf_regs++;
      else
	stats->num_sf_regs++;

      return true;

    // Update memory counts.
    case MEM:
      if (mode != BFmode)
	return false;

      stats->num_bf_loads++;
      return true;

    // Update constants
    case CONST_DOUBLE:
      stats->num_const++;
      return true;

    // Process absolute value
    case ABS:
      stats->num_unary++;
      return bfloat16_to_v4sf_inner (XEXP (op, 0), stats);

    // Process negation, deal with negate being folded into FMA or ABS.
    case NEG:
      arg = XEXP (op, 0);
      code = GET_CODE (arg);

      if (code == FMA || code == ABS)
	stats->num_neg_folded++;
      else
	stats->num_unary++;

      return bfloat16_to_v4sf_inner (arg, stats);

    // Process binary operations
    case PLUS:
    case MINUS:
    case MULT:
    case SMIN:
    case SMAX:
      stats->num_binary++;
      return (bfloat16_to_v4sf_inner (XEXP (op, 0), stats)
	      && bfloat16_to_v4sf_inner (XEXP (op, 1), stats));

    // Process fused multiply-adds
    case FMA:
      stats->num_fma++;
      return (bfloat16_to_v4sf_inner (XEXP (op, 0), stats)
	      && bfloat16_to_v4sf_inner (XEXP (op, 1), stats)
	      && bfloat16_to_v4sf_inner (XEXP (op, 2), stats));

    // Process extend BF -> SF
    case FLOAT_EXTEND:
      stats->num_extend++;
      arg = XEXP (op, 0);
      if (mode != SFmode || GET_MODE (arg) != BFmode)
	return false;

      return bfloat16_to_v4sf_inner (arg, stats);
    }

  return false;
}

bool
bfloat16_to_v4sf_p (rtx op, machine_mode mode)
{
  static struct bfloat16_to_v4sf_stats	zero_bfloat16_to_v4sf_stats;

  if (mode != BFmode && mode != SFmode)
    return false;

  rtx orig_op = op;
  struct bfloat16_to_v4sf_stats stats = zero_bfloat16_to_v4sf_stats;

  // Handle top level FLOAT_TRUNCATE
  if (GET_CODE (op) == FLOAT_TRUNCATE
      && mode == BFmode
      && GET_MODE (XEXP (op, 0)) == SFmode)
    op = XEXP (op, 0);

  bool ret = bfloat16_to_v4sf_inner (op, &stats);

  // Make sure we had enough BFmode operations to do the optimization
  if (ret)
    {
      unsigned num_bf = stats.num_bf_regs + stats.num_bf_loads;

      // If we had no BFmode values, don't do the optimization.
      if (!num_bf)
	ret = false;

      // If we had no binary or fma opertors, don't do the optimization.
      else if (!stats.num_binary && !stats.num_fma)
	ret = false;

      // If the expression has much more SFmode elements than BFmode, prefer
      // converting BFmode to SFmode instead of using V4SFmode.
      else if (stats.num_sf_regs > (num_bf + 2))
	ret = false;
    }

  // Debugging.
  if (TARGET_FLOAT16_DEBUG1)
    {
      if (ret)
	{
	  fprintf (stderr, "\n========== bfloat16_to_v4sf_p, return = true");

	  if (stats.num_bf_regs)
	    fprintf (stderr, ", # BF regs = %u", stats.num_bf_regs);

	  if (stats.num_bf_loads)
	    fprintf (stderr, ", # BF values loaded = %u", stats.num_bf_loads);

	  if (stats.num_sf_regs)
	    fprintf (stderr, ", # SF regs = %u", stats.num_sf_regs);

	  if (stats.num_const)
	    fprintf (stderr, ", # constants = %u", stats.num_const);

	  if (stats.num_extend)
	    fprintf (stderr, ", # extends = %u", stats.num_extend);

	  if (stats.num_unary)
	    fprintf (stderr, ", # unary = %u", stats.num_unary);

	  if (stats.num_binary)
	    fprintf (stderr, ", # binary = %u", stats.num_binary);

	  if (stats.num_fma)
	    fprintf (stderr, ", # fma = %u", stats.num_fma);

	  if (stats.num_neg_folded)
	    fprintf (stderr, ", # neg fold  = %u", stats.num_neg_folded);

	  fprintf (stderr, "\n");
	}
      else
	fprintf (stderr, "\n========== bfloat16_to_v4sf_p, return = false\n");

      debug_rtx (orig_op);
    }

  return ret;
}


/* Expand a bfloat16 scalar floating point operation.

   The operation is done as a V4SFmode vector operation.  This is because
   converting BFmode from a scalar BFmode to SFmode to do the operation and
   back again takes quite a bit of time.  GCC will only generate the native
   operation if -Ofast is used.  The float16.md code that calls this function
   adds various combine operations to do the operation in V4SFmode instead of
   SFmode.

   Inner part that does the operation in V4SFmode, and returns the pseudo
   with the results.  */

static rtx
bfloat16_to_v4sf_split_inner (rtx op)
{
  gcc_assert (can_create_pseudo_p ());

  rtx result_v4sf = gen_reg_rtx (V4SFmode);
  enum rtx_code icode = GET_CODE (op);
  size_t n_opts = GET_RTX_LENGTH (icode);
  rtx ops_orig[3];
  rtx ops_v4sf[3];

  switch (n_opts)
    {
    case 1:
      ops_orig[0] = XEXP (op, 0);
      ops_orig[1] = NULL_RTX;
      ops_orig[2] = NULL_RTX;
      break;

    case 2:
      ops_orig[0] = XEXP (op, 0);
      ops_orig[1] = XEXP (op, 1);
      ops_orig[2] = NULL_RTX;
      break;

    case 3:
      ops_orig[0] = XEXP (op, 0);
      ops_orig[1] = XEXP (op, 1);
      ops_orig[2] = XEXP (op, 2);
      break;

    default:
      gcc_unreachable ();
    }

  for (size_t i = 0; i < n_opts; i++)
    {
      rtx op = ops_orig[i];

      gcc_assert (op != NULL_RTX);

      switch (GET_CODE (op))
	{
	  /* Handle chained operations.  */
	case NEG:
	case ABS:
	case PLUS:
	case MINUS:
	case MULT:
	case SMIN:
	case SMAX:
	case FMA:
	  ops_v4sf[i] = bfloat16_to_v4sf_split_inner (op);
	  continue;

	  /* Remove truncation/extend added.  */
	case FLOAT_EXTEND:
	case FLOAT_TRUNCATE:
	  op = XEXP (op, 0);
	  break;

	default:
	  break;
	}

      /* Convert operands to V4SFmode format.  We use SPLAT for registers to
	 get the value into the upper 32-bits.  We can use XXSPLTW to splat
	 words instead of VSPLTIH since the XVCVBF16SPN instruction ignores the
	 odd half-words, and XXSPLTW can operate on all VSX registers instead
	 of just the Altivec registers.  Using SPLAT instead of a shift also
	 insure that other bits are not a signalling NaN.  If we are using
	 XXSPLTIW or XXSPLTIB to load the constant the other bits are
	 duplicated.  */

      rtx tmp = ops_v4sf[i] = gen_reg_rtx (V4SFmode);
      if (op == CONST0_RTX (SFmode) || op == CONST0_RTX (BFmode))
	emit_move_insn (tmp, CONST0_RTX (V4SFmode));

      else if (GET_MODE (op) == BFmode)
	{
	  emit_insn (gen_xxspltw_bf (tmp, force_reg (BFmode, op)));
	  emit_insn (gen_xvcvbf16spn_bf (tmp, tmp));
	}

      else if (GET_MODE (op) == SFmode)
	{
	  if (GET_CODE (op) == CONST_DOUBLE)
	    {
	      rtvec v = gen_rtvec (4, op, op, op, op);
	      emit_move_insn (tmp, gen_rtx_CONST_VECTOR (V4SFmode, v));
	    }

	  else
	    emit_insn (gen_vsx_splat_v4sf (tmp,
					   force_reg (SFmode, op)));
	}

      else
	gcc_unreachable ();
    }

  /* Do the operation in V4SFmode.  */
  switch (n_opts)
    {
    case 1:
      emit_insn (gen_rtx_SET (result_v4sf,
			      gen_rtx_fmt_e (icode, V4SFmode,
					     ops_v4sf[0])));

      break;

    case 2:
      emit_insn (gen_rtx_SET (result_v4sf,
			      gen_rtx_fmt_ee (icode, V4SFmode,
					      ops_v4sf[0],
					      ops_v4sf[1])));
      break;

    case 3:
      emit_insn (gen_rtx_SET (result_v4sf,
			      gen_rtx_fmt_eee (icode, V4SFmode,
					       ops_v4sf[0],
					       ops_v4sf[1],
					       ops_v4sf[2])));
      break;

    default:
      gcc_unreachable ();
    }

  return result_v4sf;
}

/* Split the isnsns tht optimize BFmode to use V4SFmode.  */

void
bfloat16_to_v4sf_split (rtx dest, rtx src)
{
  enum rtx_code code = GET_CODE (src);
  rtx dest_v4sf;

  if (TARGET_FLOAT16_DEBUG2)
    {
      fprintf (stderr, "\n========== bfloat16_to_v4sf_split:\n");
      debug_rtx (gen_rtx_SET (dest, src));
    }

  if (code == FLOAT_EXTEND || code == FLOAT_TRUNCATE)
    {
      src = XEXP (src, 0);
      code = GET_CODE (src);
    }

  switch (code)
    {
    case ABS:
    case NEG:
    case PLUS:
    case MINUS:
    case MULT:
    case SMIN:
    case SMAX:
    case FMA:
      dest_v4sf = bfloat16_to_v4sf_split_inner (src);
      break;

    default:
      gcc_unreachable ();
    }

  /* Convert V4SF result back to scalar mode.  */
  if (GET_MODE (dest) == BFmode)
    emit_insn (gen_xvcvspbf16_bf (dest, dest_v4sf));

  else if (GET_MODE (dest) == SFmode)
    {
      rtx element = GEN_INT (WORDS_BIG_ENDIAN ? 2 : 3);
      emit_insn (gen_vsx_extract_v4sf (dest, dest_v4sf, element));
    }

  else
    gcc_unreachable ();

  return;
}
