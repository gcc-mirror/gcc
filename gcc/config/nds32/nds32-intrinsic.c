/* Intrinsic functions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "expr.h"
#include "langhooks.h"		/* For add_builtin_function().  */
#include "recog.h"
#include "explow.h"

/* ------------------------------------------------------------------------ */

/* Read the requested argument from the EXP given by INDEX.
   Return the value as an rtx.  */
static rtx
nds32_read_argument (tree exp, unsigned int index)
{
  return expand_normal (CALL_EXPR_ARG (exp, index));
}

/* Return a legitimate rtx for instruction ICODE's return value.  Use TARGET
   if it's not null, has the right mode, and satisfies operand 0's
   predicate.  */
static rtx
nds32_legitimize_target (enum insn_code icode, rtx target)
{
  enum machine_mode mode = insn_data[icode].operand[0].mode;

  if (! target
      || GET_MODE (target) != mode
      || ! (*insn_data[icode].operand[0].predicate) (target, mode))
    return gen_reg_rtx (mode);
  else
    return target;
}

/* Given that ARG is being passed as operand OPNUM to instruction ICODE,
   check whether ARG satisfies the operand's constraints.  If it doesn't,
   copy ARG to a temporary register and return that.  Otherwise return ARG
   itself.  */
static rtx
nds32_legitimize_argument (enum insn_code icode, int opnum, rtx arg)
{
  enum machine_mode mode = insn_data[icode].operand[opnum].mode;

  if ((*insn_data[icode].operand[opnum].predicate) (arg, mode))
    return arg;
  else if (VECTOR_MODE_P (mode) && CONST_INT_P (arg))
    {
      /* Handle CONST_INT covert to CONST_VECTOR.  */
      int nunits = GET_MODE_NUNITS (mode);
      int i, shift = 0;
      rtvec v = rtvec_alloc (nunits);
      int val = INTVAL (arg);
      enum machine_mode val_mode = (mode == V4QImode) ? QImode : HImode;
      int shift_acc = (val_mode == QImode) ? 8 : 16;
      int mask = (val_mode == QImode) ? 0xff : 0xffff;
      int tmp_val = val;

      if (TARGET_BIG_ENDIAN)
	for (i = 0; i < nunits; i++)
	  {
	    tmp_val = (val >> shift) & mask;
	    RTVEC_ELT (v, nunits - i - 1) = gen_int_mode (tmp_val, val_mode);
	    shift += shift_acc;
	  }
      else
	for (i = 0; i < nunits; i++)
	  {
	    tmp_val = (val >> shift) & mask;
	    RTVEC_ELT (v, i) = gen_int_mode (tmp_val, val_mode);
	    shift += shift_acc;
	  }

      return copy_to_mode_reg (mode, gen_rtx_CONST_VECTOR (mode, v));
    }
  else
    {
      rtx tmp_rtx = gen_reg_rtx (mode);
      convert_move (tmp_rtx, arg, false);
      return tmp_rtx;
    }
}

/* Return true if OPVAL can be used for operand OPNUM of instruction ICODE.
   The instruction should require a constant operand of some sort.  The
   function prints an error if OPVAL is not valid.  */
static int
nds32_check_constant_argument (enum insn_code icode, int opnum, rtx opval,
			       const char *name)
{
  if (GET_CODE (opval) != CONST_INT)
    {
      error ("invalid argument to built-in function %s", name);
      return false;
    }
  if (! (*insn_data[icode].operand[opnum].predicate) (opval, VOIDmode))
    {
      error ("constant argument out of range for %s", name);

      return false;
    }
  return true;
}

/* Expand builtins that return target.  */
static rtx
nds32_expand_noarg_builtin (enum insn_code icode, rtx target)
{
  rtx pat;

  target = nds32_legitimize_target (icode, target);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (target);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take one operand.  */
static rtx
nds32_expand_unop_builtin (enum insn_code icode, tree exp, rtx target,
			   bool return_p)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  int op0_num = return_p ? 1 : 0;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  op0 = nds32_legitimize_argument (icode, op0_num, op0);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0);
  else
    pat = GEN_FCN (icode) (op0);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take one operands and the first is immediate.  */
static rtx
nds32_expand_unopimm_builtin (enum insn_code icode, tree exp, rtx target,
			      bool return_p, const char *name)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  int op0_num = return_p ? 1 : 0;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  if (!nds32_check_constant_argument (icode, op0_num, op0, name))
    return NULL_RTX;

  op0 = nds32_legitimize_argument (icode, op0_num, op0);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0);
  else
    pat = GEN_FCN (icode) (op0);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take two operands.  */
static rtx
nds32_expand_binop_builtin (enum insn_code icode, tree exp, rtx target,
			    bool return_p)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  int op0_num = return_p ? 1 : 0;
  int op1_num = return_p ? 2 : 1;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  op0 = nds32_legitimize_argument (icode, op0_num, op0);
  op1 = nds32_legitimize_argument (icode, op1_num, op1);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0, op1);
  else
    pat = GEN_FCN (icode) (op0, op1);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take two operands and the second is immediate.  */
static rtx
nds32_expand_binopimm_builtin (enum insn_code icode, tree exp, rtx target,
			       bool return_p, const char *name)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  int op0_num = return_p ? 1 : 0;
  int op1_num = return_p ? 2 : 1;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  if (!nds32_check_constant_argument (icode, op1_num, op1, name))
    return NULL_RTX;

  op0 = nds32_legitimize_argument (icode, op0_num, op0);
  op1 = nds32_legitimize_argument (icode, op1_num, op1);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0, op1);
  else
    pat = GEN_FCN (icode) (op0, op1);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take three operands.  */
static rtx
nds32_expand_triop_builtin (enum insn_code icode, tree exp, rtx target,
			    bool return_p)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  rtx op2 = nds32_read_argument (exp, 2);
  int op0_num = return_p ? 1 : 0;
  int op1_num = return_p ? 2 : 1;
  int op2_num = return_p ? 3 : 2;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  op0 = nds32_legitimize_argument (icode, op0_num, op0);
  op1 = nds32_legitimize_argument (icode, op1_num, op1);
  op2 = nds32_legitimize_argument (icode, op2_num, op2);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0, op1, op2);
  else
    pat = GEN_FCN (icode) (op0, op1, op2);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take three operands and the third is immediate.  */
static rtx
nds32_expand_triopimm_builtin (enum insn_code icode, tree exp, rtx target,
			       bool return_p, const char *name)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  rtx op2 = nds32_read_argument (exp, 2);
  int op0_num = return_p ? 1 : 0;
  int op1_num = return_p ? 2 : 1;
  int op2_num = return_p ? 3 : 2;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  if (!nds32_check_constant_argument (icode, op2_num, op2, name))
    return NULL_RTX;

  op0 = nds32_legitimize_argument (icode, op0_num, op0);
  op1 = nds32_legitimize_argument (icode, op1_num, op1);
  op2 = nds32_legitimize_argument (icode, op2_num, op2);

  /* Emit and return the new instruction. */
  if (return_p)
    pat = GEN_FCN (icode) (target, op0, op1, op2);
  else
    pat = GEN_FCN (icode) (op0, op1, op2);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins for load.  */
static rtx
nds32_expand_builtin_load (enum insn_code icode, tree exp, rtx target)
{
  /* Load address format is [$ra + $rb],
     but input arguments not enough,
     so we need another temp register as $rb.
     Generating assembly code:
       movi $temp, 0
       llw  $rt, [$ra + $temp] */
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx addr_helper = gen_reg_rtx (insn_data[icode].operand[1].mode);

  target = nds32_legitimize_target (icode, target);
  op0 = nds32_legitimize_argument (icode, 1, op0);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (target, op0, addr_helper);
  if (!pat)
    return NULL_RTX;

  emit_move_insn (addr_helper, GEN_INT (0));
  emit_insn (pat);
  return target;
}

/* Expand builtins for store.  */
static rtx
nds32_expand_builtin_store (enum insn_code icode, tree exp, rtx target)
{
  /* Store address format is [$ra + $rb],
     but input arguments not enough,
     so we need another temp register as $rb.
     Generating assembly code:
       movi $temp, 0
       store  $rt, [$ra + $temp] */
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  rtx addr_helper = gen_reg_rtx (insn_data[icode].operand[1].mode);

  op0 = nds32_legitimize_argument (icode, 0, op0);
  op1 = nds32_legitimize_argument (icode, 2, op1);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (op0, addr_helper, op1);
  if (! pat)
    return NULL_RTX;

  emit_move_insn (addr_helper, GEN_INT (0));
  emit_insn (pat);
  return target;
}

/* Expand cctl builtins.  */
static rtx
nds32_expand_cctl_builtin (enum insn_code icode, tree exp, rtx target,
			   bool return_p, const char *name)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  int op0_num = return_p ? 1 : 0;
  int op1_num = return_p ? 2 : 1;

  if (return_p)
    target = nds32_legitimize_target (icode, target);

  if (!nds32_check_constant_argument (icode, op0_num, op0, name))
    return NULL_RTX;

  op0 = nds32_legitimize_argument (icode, op0_num, op0);
  op1 = nds32_legitimize_argument (icode, op1_num, op1);

  /* Emit and return the new instruction. */
  if (icode == CODE_FOR_cctl_idx_write)
    {
      /* cctl_idx_write is three argument,
	 so create operand2 for cctl_idx_write pattern.  */
      rtx op2 = nds32_read_argument (exp, 2);
      op2 = nds32_legitimize_argument (icode, 2, op2);
      pat = GEN_FCN (icode) (op0, op1, op2);
    }
  else if (return_p)
    pat = GEN_FCN (icode) (target, op0, op1);
  else
    pat = GEN_FCN (icode) (op0, op1);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand scw builtins.  */
static rtx
nds32_expand_scw_builtin (enum insn_code icode, tree exp, rtx target)
{
  /* SCW address format is [$ra + $rb], but input arguments not enough,
     so we need another temp register as $rb.
     Generating assembly code:
	movi $temp, 0
	scw  $rt, [$ra + $temp] */
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);
  rtx addr_helper = gen_reg_rtx (insn_data[icode].operand[1].mode);

  target = nds32_legitimize_target (icode, target);
  op0 = nds32_legitimize_argument (icode, 1, op0);
  op1 = nds32_legitimize_argument (icode, 2, op1);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (target, op0, addr_helper, target);

  if (!pat)
    return NULL_RTX;

  emit_move_insn (addr_helper, GEN_INT (0));
  emit_move_insn (target, op1);
  emit_insn (pat);
  return target;
}

/* Expand set int priority builtins. */
static rtx
nds32_expand_priority_builtin (enum insn_code icode, tree exp, rtx target,
			       const char *name)
{
  rtx pat;
  rtx op0 = nds32_read_argument (exp, 0);
  rtx op1 = nds32_read_argument (exp, 1);

  /* set_int_priority intrinsic function that two arguments are immediate,
     so check whether auguments are immedite.  */

  if (!nds32_check_constant_argument (icode, 0, op0, name))
    return NULL_RTX;

  if (!nds32_check_constant_argument (icode, 1, op1, name))
    return NULL_RTX;

  op0 = nds32_legitimize_argument (icode, 0, op0);
  op1 = nds32_legitimize_argument (icode, 1, op1);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (op0, op1);

  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

struct builtin_description
{
  const enum insn_code icode;
  const char *name;
  enum nds32_builtins code;
  bool return_p;
};

#define NDS32_BUILTIN(code, string, builtin) \
  { CODE_FOR_##code, "__nds32__" string, \
    NDS32_BUILTIN_##builtin, true },

#define NDS32_NO_TARGET_BUILTIN(code, string, builtin) \
  { CODE_FOR_##code, "__nds32__" string, \
    NDS32_BUILTIN_##builtin, false },

/* Intrinsics that no argument, and that return value.  */
static struct builtin_description bdesc_noarg[] =
{
  NDS32_BUILTIN(unspec_fmfcfg, "fmfcfg", FMFCFG)
  NDS32_BUILTIN(unspec_fmfcsr, "fmfcsr", FMFCSR)
  NDS32_BUILTIN(unspec_get_current_sp, "get_current_sp", GET_CURRENT_SP)
  NDS32_BUILTIN(unspec_return_address, "return_address", RETURN_ADDRESS)
  NDS32_BUILTIN(unspec_get_all_pending_int, "get_all_pending_int",
		GET_ALL_PENDING_INT)
  NDS32_BUILTIN(unspec_unaligned_feature, "unaligned_feature",
		UNALIGNED_FEATURE)
  NDS32_NO_TARGET_BUILTIN(unspec_enable_unaligned, "enable_unaligned",
			  ENABLE_UNALIGNED)
  NDS32_NO_TARGET_BUILTIN(unspec_disable_unaligned, "disable_unaligned",
			  DISABLE_UNALIGNED)
};

/* Intrinsics that take just one argument.  */
static struct builtin_description bdesc_1arg[] =
{
  NDS32_BUILTIN(unspec_ssabssi2, "abs", ABS)
  NDS32_BUILTIN(clzsi2, "clz", CLZ)
  NDS32_BUILTIN(unspec_clo, "clo", CLO)
  NDS32_BUILTIN(unspec_wsbh, "wsbh", WSBH)
  NDS32_BUILTIN(unspec_tlbop_pb, "tlbop_pb",TLBOP_PB)
  NDS32_BUILTIN(unaligned_load_hw, "unaligned_load_hw", UALOAD_HW)
  NDS32_BUILTIN(unaligned_loadsi, "unaligned_load_w", UALOAD_W)
  NDS32_BUILTIN(unaligned_loaddi, "unaligned_load_dw", UALOAD_DW)
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_isync, "isync", ISYNC)
  NDS32_NO_TARGET_BUILTIN(unspec_fmtcsr, "fmtcsr", FMTCSR)
  NDS32_NO_TARGET_BUILTIN(unspec_jr_itoff, "jr_itoff", JR_ITOFF)
  NDS32_NO_TARGET_BUILTIN(unspec_jr_toff, "jr_toff", JR_TOFF)
  NDS32_NO_TARGET_BUILTIN(unspec_jral_ton, "jral_ton", JRAL_TON)
  NDS32_NO_TARGET_BUILTIN(unspec_ret_toff, "ret_toff", RET_TOFF)
  NDS32_NO_TARGET_BUILTIN(unspec_jral_iton, "jral_iton",JRAL_ITON)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_trd, "tlbop_trd", TLBOP_TRD)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_twr, "tlbop_twr", TLBOP_TWR)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_rwr, "tlbop_rwr", TLBOP_RWR)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_rwlk, "tlbop_rwlk", TLBOP_RWLK)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_unlk, "tlbop_unlk", TLBOP_UNLK)
  NDS32_NO_TARGET_BUILTIN(unspec_tlbop_inv, "tlbop_inv", TLBOP_INV)
  NDS32_NO_TARGET_BUILTIN(unspec_ret_itoff, "ret_itoff", RET_ITOFF)
  NDS32_NO_TARGET_BUILTIN(unspec_set_current_sp,
			  "set_current_sp", SET_CURRENT_SP)
};

/* Intrinsics that take just one argument. and the argument is immediate.  */
static struct builtin_description bdesc_1argimm[] =
{
  NDS32_BUILTIN(unspec_volatile_mfsr, "mfsr", MFSR)
  NDS32_BUILTIN(unspec_volatile_mfusr, "mfsr", MFUSR)
  NDS32_BUILTIN(unspec_get_pending_int, "get_pending_int", GET_PENDING_INT)
  NDS32_BUILTIN(unspec_get_int_priority, "get_int_priority", GET_INT_PRIORITY)
  NDS32_NO_TARGET_BUILTIN(unspec_trap, "trap", TRAP)
  NDS32_NO_TARGET_BUILTIN(unspec_break, "break", BREAK)
  NDS32_NO_TARGET_BUILTIN(unspec_syscall, "syscall", SYSCALL)
  NDS32_NO_TARGET_BUILTIN(unspec_enable_int, "enable_int", ENABLE_INT)
  NDS32_NO_TARGET_BUILTIN(unspec_disable_int, "disable_int", DISABLE_INT)
  NDS32_NO_TARGET_BUILTIN(unspec_clr_pending_hwint, "clr_pending_hwint",
			  CLR_PENDING_HWINT)
  NDS32_NO_TARGET_BUILTIN(unspec_set_trig_level, "set_trig_level",
			  SET_TRIG_LEVEL)
  NDS32_NO_TARGET_BUILTIN(unspec_set_trig_edge, "set_trig_edge",
			  SET_TRIG_EDGE)
  NDS32_BUILTIN(unspec_get_trig_type, "get_trig_type", GET_TRIG_TYPE)
};

/* Intrinsics that take two arguments.  */
static struct builtin_description bdesc_2arg[] =
{
  NDS32_BUILTIN(unspec_fcpynss, "fcpynss", FCPYNSS)
  NDS32_BUILTIN(unspec_fcpyss, "fcpyss", FCPYSS)
  NDS32_BUILTIN(unspec_fcpynsd, "fcpynsd", FCPYNSD)
  NDS32_BUILTIN(unspec_fcpysd, "fcpysd", FCPYSD)
  NDS32_BUILTIN(unspec_ave, "ave", AVE)
  NDS32_BUILTIN(unspec_pbsad, "pbsad", PBSAD)
  NDS32_BUILTIN(unspec_ffb, "ffb", FFB)
  NDS32_BUILTIN(unspec_ffmism, "ffmsim", FFMISM)
  NDS32_BUILTIN(unspec_flmism, "flmism", FLMISM)
  NDS32_BUILTIN(rotrsi3, "rotr", ROTR)
  NDS32_BUILTIN(unspec_sva, "sva", SVA)
  NDS32_BUILTIN(unspec_svs, "svs", SVS)
  NDS32_NO_TARGET_BUILTIN(mtsr_isb, "mtsr_isb", MTSR_ISB)
  NDS32_NO_TARGET_BUILTIN(mtsr_dsb, "mtsr_dsb", MTSR_DSB)
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_mtsr, "mtsr", MTSR)
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_mtusr, "mtusr", MTUSR)
  NDS32_NO_TARGET_BUILTIN(unaligned_store_hw, "unaligned_store_hw", UASTORE_HW)
  NDS32_NO_TARGET_BUILTIN(unaligned_storesi, "unaligned_store_hw", UASTORE_W)
  NDS32_NO_TARGET_BUILTIN(unaligned_storedi, "unaligned_store_hw", UASTORE_DW)

};

/* Two-argument intrinsics with an immediate second argument.  */
static struct builtin_description bdesc_2argimm[] =
{
  NDS32_BUILTIN(unspec_bclr, "bclr", BCLR)
  NDS32_BUILTIN(unspec_bset, "bset", BSET)
  NDS32_BUILTIN(unspec_btgl, "btgl", BTGL)
  NDS32_BUILTIN(unspec_btst, "btst", BTST)
  NDS32_BUILTIN(unspec_clip, "clip", CLIP)
  NDS32_BUILTIN(unspec_clips, "clips", CLIPS)
  NDS32_NO_TARGET_BUILTIN(unspec_teqz, "teqz", TEQZ)
  NDS32_NO_TARGET_BUILTIN(unspec_tnez, "tnez", TNEZ)
};

/* Intrinsics that take three arguments.  */
static struct builtin_description bdesc_3arg[] =
{
  NDS32_BUILTIN(unspec_pbsada, "pbsada", PBSADA)
  NDS32_NO_TARGET_BUILTIN(bse, "bse", BSE)
  NDS32_NO_TARGET_BUILTIN(bsp, "bsp", BSP)
};

/* Three-argument intrinsics with an immediate third argument.  */
static struct builtin_description bdesc_3argimm[] =
{
  NDS32_NO_TARGET_BUILTIN(prefetch_qw, "prefetch_qw", DPREF_QW)
  NDS32_NO_TARGET_BUILTIN(prefetch_hw, "prefetch_hw", DPREF_HW)
  NDS32_NO_TARGET_BUILTIN(prefetch_w, "prefetch_w", DPREF_W)
  NDS32_NO_TARGET_BUILTIN(prefetch_dw, "prefetch_dw", DPREF_DW)
};

/* Intrinsics that load a value.  */
static struct builtin_description bdesc_load[] =
{
  NDS32_BUILTIN(unspec_volatile_llw, "llw", LLW)
  NDS32_BUILTIN(unspec_lwup, "lwup", LWUP)
  NDS32_BUILTIN(unspec_lbup, "lbup", LBUP)
};

/* Intrinsics that store a value.  */
static struct builtin_description bdesc_store[] =
{
  NDS32_BUILTIN(unspec_swup, "swup", SWUP)
  NDS32_BUILTIN(unspec_sbup, "sbup", SBUP)
};

static struct builtin_description bdesc_cctl[] =
{
  NDS32_BUILTIN(cctl_idx_read, "cctl_idx_read", CCTL_IDX_READ)
  NDS32_NO_TARGET_BUILTIN(cctl_idx_write, "cctl_idx_write", CCTL_IDX_WRITE)
  NDS32_NO_TARGET_BUILTIN(cctl_va_lck, "cctl_va_lck", CCTL_VA_LCK)
  NDS32_NO_TARGET_BUILTIN(cctl_idx_wbinval,
			  "cctl_idx_wbinval", CCTL_IDX_WBINVAL)
  NDS32_NO_TARGET_BUILTIN(cctl_va_wbinval_l1,
			  "cctl_va_wbinval_l1", CCTL_VA_WBINVAL_L1)
  NDS32_NO_TARGET_BUILTIN(cctl_va_wbinval_la,
			  "cctl_va_wbinval_la", CCTL_VA_WBINVAL_LA)
};

rtx
nds32_expand_builtin_impl (tree exp,
			   rtx target,
			   rtx subtarget ATTRIBUTE_UNUSED,
			   enum machine_mode mode ATTRIBUTE_UNUSED,
			   int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  unsigned i;
  struct builtin_description *d;

  switch (fcode)
    {
    /* FPU Register Transfer.  */
    case NDS32_BUILTIN_FMFCFG:
    case NDS32_BUILTIN_FMFCSR:
    case NDS32_BUILTIN_FMTCSR:
    case NDS32_BUILTIN_FCPYNSS:
    case NDS32_BUILTIN_FCPYSS:
      /* Both v3s and v3f toolchains define TARGET_FPU_SINGLE.  */
      if (!TARGET_FPU_SINGLE)
	{
	  error ("this builtin function is only available "
		 "on the v3s or v3f toolchain");
	  return NULL_RTX;
	}
      break;

    /* FPU Register Transfer.  */
    case NDS32_BUILTIN_FCPYNSD:
    case NDS32_BUILTIN_FCPYSD:
      /* Only v3f toolchain defines TARGET_FPU_DOUBLE.  */
      if (!TARGET_FPU_DOUBLE)
	{
	  error ("this builtin function is only available "
		 "on the v3f toolchain");
	  return NULL_RTX;
	}
      break;

    /* Load and Store  */
    case NDS32_BUILTIN_LLW:
    case NDS32_BUILTIN_LWUP:
    case NDS32_BUILTIN_LBUP:
    case NDS32_BUILTIN_SCW:
    case NDS32_BUILTIN_SWUP:
    case NDS32_BUILTIN_SBUP:
      if (TARGET_ISA_V3M)
	{
	  error ("this builtin function not support "
		 "on the v3m toolchain");
	  return NULL_RTX;
	}
      break;

    /* Performance Extension  */
    case NDS32_BUILTIN_ABS:
    case NDS32_BUILTIN_AVE:
    case NDS32_BUILTIN_BCLR:
    case NDS32_BUILTIN_BSET:
    case NDS32_BUILTIN_BTGL:
    case NDS32_BUILTIN_BTST:
    case NDS32_BUILTIN_CLIP:
    case NDS32_BUILTIN_CLIPS:
    case NDS32_BUILTIN_CLZ:
    case NDS32_BUILTIN_CLO:
      if (!TARGET_EXT_PERF)
	{
	  error ("don't support performance extension instructions");
	  return NULL_RTX;
	}
      break;

    /* Performance Extension 2  */
    case NDS32_BUILTIN_PBSAD:
    case NDS32_BUILTIN_PBSADA:
    case NDS32_BUILTIN_BSE:
    case NDS32_BUILTIN_BSP:
      if (!TARGET_EXT_PERF2)
	{
	  error ("don't support performance extension "
		 "version 2 instructions");
	  return NULL_RTX;
	}
      break;

    /* String Extension  */
    case NDS32_BUILTIN_FFB:
    case NDS32_BUILTIN_FFMISM:
    case NDS32_BUILTIN_FLMISM:
      if (!TARGET_EXT_STRING)
	{
	  error ("don't support string extension instructions");
	  return NULL_RTX;
	}
      break;

    default:
      break;
    }

  /* Since there are no result and operands, we can simply emit this rtx.  */
  switch (fcode)
    {
    case NDS32_BUILTIN_ISB:
      emit_insn (gen_unspec_volatile_isb ());
      return target;
    case NDS32_BUILTIN_DSB:
      emit_insn (gen_unspec_dsb ());
      return target;
    case NDS32_BUILTIN_MSYNC_ALL:
      emit_insn (gen_unspec_msync_all ());
      return target;
    case NDS32_BUILTIN_MSYNC_STORE:
      emit_insn (gen_unspec_msync_store ());
      return target;
    case NDS32_BUILTIN_SETGIE_EN:
      emit_insn (gen_unspec_volatile_setgie_en ());
      emit_insn (gen_unspec_dsb ());
      return target;
    case NDS32_BUILTIN_SETGIE_DIS:
      emit_insn (gen_unspec_volatile_setgie_dis ());
      emit_insn (gen_unspec_dsb ());
      return target;
    case NDS32_BUILTIN_GIE_DIS:
      emit_insn (gen_unspec_volatile_setgie_dis ());
      emit_insn (gen_unspec_dsb ());
      return target;
    case NDS32_BUILTIN_GIE_EN:
      emit_insn (gen_unspec_volatile_setgie_en ());
      emit_insn (gen_unspec_dsb ());
      return target;
    case NDS32_BUILTIN_SET_PENDING_SWINT:
      emit_insn (gen_unspec_set_pending_swint ());
      return target;
    case NDS32_BUILTIN_CLR_PENDING_SWINT:
      emit_insn (gen_unspec_clr_pending_swint ());
      return target;
    case NDS32_BUILTIN_CCTL_L1D_INVALALL:
      emit_insn (gen_cctl_l1d_invalall());
      return target;
    case NDS32_BUILTIN_CCTL_L1D_WBALL_ALVL:
      emit_insn (gen_cctl_l1d_wball_alvl());
      return target;
    case NDS32_BUILTIN_CCTL_L1D_WBALL_ONE_LVL:
      emit_insn (gen_cctl_l1d_wball_one_lvl());
      return target;
    case NDS32_BUILTIN_STANDBY_NO_WAKE_GRANT:
      emit_insn (gen_unspec_standby_no_wake_grant ());
      return target;
    case NDS32_BUILTIN_STANDBY_WAKE_GRANT:
      emit_insn (gen_unspec_standby_wake_grant ());
      return target;
    case NDS32_BUILTIN_STANDBY_WAKE_DONE:
      emit_insn (gen_unspec_standby_wait_done ());
      return target;
    case NDS32_BUILTIN_SETEND_BIG:
      emit_insn (gen_unspec_setend_big ());
      return target;
    case NDS32_BUILTIN_SETEND_LITTLE:
      emit_insn (gen_unspec_setend_little ());
      return target;
    case NDS32_BUILTIN_NOP:
      emit_insn (gen_unspec_nop ());
      return target;
    case NDS32_BUILTIN_SCHE_BARRIER:
      emit_insn (gen_blockage ());
      return target;
    case NDS32_BUILTIN_TLBOP_FLUA:
      emit_insn (gen_unspec_tlbop_flua ());
      return target;
    case NDS32_BUILTIN_SCW:
      return nds32_expand_scw_builtin (CODE_FOR_unspec_volatile_scw,
				       exp, target);
    case NDS32_BUILTIN_SET_INT_PRIORITY:
      return nds32_expand_priority_builtin (CODE_FOR_unspec_set_int_priority,
					    exp, target,
					    "__nds32__set_int_priority");
      return target;
    default:
      break;
    }

  /* Expand groups of builtins.  */
  for (i = 0, d = bdesc_noarg; i < ARRAY_SIZE (bdesc_noarg); i++, d++)
    if (d->code == fcode)
      return nds32_expand_noarg_builtin (d->icode, target);

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return nds32_expand_unop_builtin (d->icode, exp, target, d->return_p);

  for (i = 0, d = bdesc_1argimm; i < ARRAY_SIZE (bdesc_1argimm); i++, d++)
    if (d->code == fcode)
      return nds32_expand_unopimm_builtin (d->icode, exp, target,
					   d->return_p, d->name);

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return nds32_expand_binop_builtin (d->icode, exp, target, d->return_p);

  for (i = 0, d = bdesc_2argimm; i < ARRAY_SIZE (bdesc_2argimm); i++, d++)
    if (d->code == fcode)
      return nds32_expand_binopimm_builtin (d->icode, exp, target,
					    d->return_p, d->name);

  for (i = 0, d = bdesc_3arg; i < ARRAY_SIZE (bdesc_3arg); i++, d++)
    if (d->code == fcode)
      return nds32_expand_triop_builtin (d->icode, exp, target, d->return_p);

  for (i = 0, d = bdesc_3argimm; i < ARRAY_SIZE (bdesc_3argimm); i++, d++)
    if (d->code == fcode)
      return nds32_expand_triopimm_builtin (d->icode, exp, target,
					    d->return_p, d->name);

  for (i = 0, d = bdesc_load; i < ARRAY_SIZE (bdesc_load); i++, d++)
    if (d->code == fcode)
      return nds32_expand_builtin_load (d->icode, exp, target);

  for (i = 0, d = bdesc_store; i < ARRAY_SIZE (bdesc_store); i++, d++)
    if (d->code == fcode)
      return nds32_expand_builtin_store (d->icode, exp, target);

  for (i = 0, d = bdesc_cctl; i < ARRAY_SIZE (bdesc_cctl); i++, d++)
    if (d->code == fcode)
      return nds32_expand_cctl_builtin (d->icode, exp, target,
					d->return_p, d->name);

  return NULL_RTX;
}

static GTY(()) tree nds32_builtin_decls[NDS32_BUILTIN_COUNT];

/* Return the NDS32 builtin for CODE.  */
tree
nds32_builtin_decl_impl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= NDS32_BUILTIN_COUNT)
    return error_mark_node;

  return nds32_builtin_decls[code];
}

void
nds32_init_builtins_impl (void)
{
#define ADD_NDS32_BUILTIN0(NAME, RET_TYPE, CODE)		\
  nds32_builtin_decls[NDS32_BUILTIN_ ## CODE] =			\
  add_builtin_function ("__builtin_nds32_" NAME,		\
			build_function_type_list (RET_TYPE##_type_node, \
						  NULL_TREE),		\
			NDS32_BUILTIN_ ## CODE, BUILT_IN_MD, NULL, NULL_TREE)

#define ADD_NDS32_BUILTIN1(NAME, RET_TYPE, ARG_TYPE, CODE)	\
  nds32_builtin_decls[NDS32_BUILTIN_ ## CODE] =			\
  add_builtin_function ("__builtin_nds32_" NAME,		\
			build_function_type_list (RET_TYPE##_type_node, \
						  ARG_TYPE##_type_node, \
						  NULL_TREE),		\
			NDS32_BUILTIN_ ## CODE, BUILT_IN_MD, NULL, NULL_TREE)

#define ADD_NDS32_BUILTIN2(NAME, RET_TYPE, ARG_TYPE1, ARG_TYPE2, CODE)	\
  nds32_builtin_decls[NDS32_BUILTIN_ ## CODE] =				\
  add_builtin_function ("__builtin_nds32_" NAME,			\
			build_function_type_list (RET_TYPE##_type_node, \
						  ARG_TYPE1##_type_node,\
						  ARG_TYPE2##_type_node,\
						  NULL_TREE),		\
			NDS32_BUILTIN_ ## CODE, BUILT_IN_MD, NULL, NULL_TREE)

#define ADD_NDS32_BUILTIN3(NAME, RET_TYPE,				\
			   ARG_TYPE1, ARG_TYPE2, ARG_TYPE3, CODE)	\
  nds32_builtin_decls[NDS32_BUILTIN_ ## CODE] =				\
  add_builtin_function ("__builtin_nds32_" NAME,			\
			build_function_type_list (RET_TYPE##_type_node,	\
						  ARG_TYPE1##_type_node,\
						  ARG_TYPE2##_type_node,\
						  ARG_TYPE3##_type_node,\
						  NULL_TREE),		\
			NDS32_BUILTIN_ ## CODE, BUILT_IN_MD, NULL, NULL_TREE)

  /* Looking for return type and argument can be found in tree.h file.  */
  tree ptr_uchar_type_node = build_pointer_type (unsigned_char_type_node);
  tree ptr_ushort_type_node = build_pointer_type (short_unsigned_type_node);
  tree ptr_uint_type_node = build_pointer_type (unsigned_type_node);
  tree ptr_ulong_type_node = build_pointer_type (long_long_unsigned_type_node);

  /* Cache.  */
  ADD_NDS32_BUILTIN1 ("isync", void, ptr_uint, ISYNC);
  ADD_NDS32_BUILTIN0 ("isb", void, ISB);
  ADD_NDS32_BUILTIN0 ("dsb", void, DSB);
  ADD_NDS32_BUILTIN0 ("msync_all", void, MSYNC_ALL);
  ADD_NDS32_BUILTIN0 ("msync_store", void, MSYNC_STORE);

  /* Register Transfer.  */
  ADD_NDS32_BUILTIN1 ("mfsr", unsigned, integer, MFSR);
  ADD_NDS32_BUILTIN1 ("mfusr", unsigned, integer, MFUSR);
  ADD_NDS32_BUILTIN2 ("mtsr", void, unsigned, integer, MTSR);
  ADD_NDS32_BUILTIN2 ("mtsr_isb", void, unsigned, integer, MTSR_ISB);
  ADD_NDS32_BUILTIN2 ("mtsr_dsb", void, unsigned, integer, MTSR_DSB);
  ADD_NDS32_BUILTIN2 ("mtusr", void, unsigned, integer, MTUSR);

  /* FPU Register Transfer.  */
  ADD_NDS32_BUILTIN0 ("fmfcsr", unsigned, FMFCSR);
  ADD_NDS32_BUILTIN1 ("fmtcsr", void, unsigned, FMTCSR);
  ADD_NDS32_BUILTIN0 ("fmfcfg", unsigned, FMFCFG);
  ADD_NDS32_BUILTIN2 ("fcpyss", float, float, float, FCPYSS);
  ADD_NDS32_BUILTIN2 ("fcpynss", float, float, float, FCPYNSS);
  ADD_NDS32_BUILTIN2 ("fcpysd", double, double, double, FCPYSD);
  ADD_NDS32_BUILTIN2 ("fcpynsd", double, double, double, FCPYNSD);

  /* Interrupt.  */
  ADD_NDS32_BUILTIN0 ("setgie_en", void, SETGIE_EN);
  ADD_NDS32_BUILTIN0 ("setgie_dis", void, SETGIE_DIS);
  ADD_NDS32_BUILTIN0 ("gie_en", void, GIE_EN);
  ADD_NDS32_BUILTIN0 ("gie_dis", void, GIE_DIS);
  ADD_NDS32_BUILTIN1 ("enable_int", void, integer, ENABLE_INT);
  ADD_NDS32_BUILTIN1 ("disable_int", void, integer, DISABLE_INT);
  ADD_NDS32_BUILTIN0 ("set_pending_swint", void, SET_PENDING_SWINT);
  ADD_NDS32_BUILTIN0 ("clr_pending_swint", void, CLR_PENDING_SWINT);
  ADD_NDS32_BUILTIN0 ("get_all_pending_int", unsigned, GET_ALL_PENDING_INT);
  ADD_NDS32_BUILTIN1 ("get_pending_int", unsigned, integer, GET_PENDING_INT);
  ADD_NDS32_BUILTIN1 ("get_int_priority", unsigned, integer, GET_INT_PRIORITY);
  ADD_NDS32_BUILTIN2 ("set_int_priority", void, integer, integer,
		      SET_INT_PRIORITY);
  ADD_NDS32_BUILTIN1 ("clr_pending_hwint", void, integer, CLR_PENDING_HWINT);
  ADD_NDS32_BUILTIN1 ("set_trig_level", void, integer, SET_TRIG_LEVEL);
  ADD_NDS32_BUILTIN1 ("set_trig_edge", void, integer, SET_TRIG_EDGE);
  ADD_NDS32_BUILTIN1 ("get_trig_type", unsigned, integer, GET_TRIG_TYPE);

  /* Load and Store  */
  ADD_NDS32_BUILTIN1 ("llw", unsigned, ptr_uint, LLW);
  ADD_NDS32_BUILTIN1 ("lwup", unsigned, ptr_uint, LWUP);
  ADD_NDS32_BUILTIN1 ("lbup", char, ptr_uchar, LBUP);
  ADD_NDS32_BUILTIN2 ("scw", unsigned, ptr_uint, unsigned, SCW);
  ADD_NDS32_BUILTIN2 ("swup", void, ptr_uint, unsigned, SWUP);
  ADD_NDS32_BUILTIN2 ("sbup", void, ptr_uchar, char, SBUP);

  /* CCTL  */
  ADD_NDS32_BUILTIN0 ("cctl_l1d_invalall", void, CCTL_L1D_INVALALL);
  ADD_NDS32_BUILTIN0 ("cctl_l1d_wball_alvl", void, CCTL_L1D_WBALL_ALVL);
  ADD_NDS32_BUILTIN0 ("cctl_l1d_wball_one_lvl", void, CCTL_L1D_WBALL_ONE_LVL);
  ADD_NDS32_BUILTIN2 ("cctl_va_lck", void, integer, ptr_uint, CCTL_VA_LCK);
  ADD_NDS32_BUILTIN2 ("cctl_idx_wbinval", void, integer, unsigned,
		      CCTL_IDX_WBINVAL);
  ADD_NDS32_BUILTIN2 ("cctl_va_wbinval_l1", void, integer, ptr_uint,
		      CCTL_VA_WBINVAL_L1);
  ADD_NDS32_BUILTIN2 ("cctl_va_wbinval_la", void, integer, ptr_uint,
		      CCTL_VA_WBINVAL_LA);
  ADD_NDS32_BUILTIN2 ("cctl_idx_read", unsigned, integer, unsigned,
		      CCTL_IDX_READ);
  ADD_NDS32_BUILTIN3 ("cctl_idx_write", void, integer, unsigned, unsigned,
		      CCTL_IDX_WRITE);

  /* PREFETCH  */
  ADD_NDS32_BUILTIN3 ("dpref_qw", void, ptr_uchar, unsigned, integer, DPREF_QW);
  ADD_NDS32_BUILTIN3 ("dpref_hw", void, ptr_ushort, unsigned, integer,
		      DPREF_HW);
  ADD_NDS32_BUILTIN3 ("dpref_w", void, ptr_uint, unsigned, integer, DPREF_W);
  ADD_NDS32_BUILTIN3 ("dpref_dw", void, ptr_ulong, unsigned, integer, DPREF_DW);

  /* Performance Extension  */
  ADD_NDS32_BUILTIN1 ("pe_abs", integer, integer, ABS);
  ADD_NDS32_BUILTIN2 ("pe_ave", integer, integer, integer, AVE);
  ADD_NDS32_BUILTIN2 ("pe_bclr", unsigned, unsigned, unsigned, BCLR);
  ADD_NDS32_BUILTIN2 ("pe_bset", unsigned, unsigned, unsigned, BSET);
  ADD_NDS32_BUILTIN2 ("pe_btgl", unsigned, unsigned, unsigned, BTGL);
  ADD_NDS32_BUILTIN2 ("pe_btst", unsigned, unsigned, unsigned, BTST);
  ADD_NDS32_BUILTIN2 ("pe_clip", unsigned, integer, unsigned, CLIP);
  ADD_NDS32_BUILTIN2 ("pe_clips", integer, integer, unsigned, CLIPS);
  ADD_NDS32_BUILTIN1 ("pe_clz", unsigned, unsigned, CLZ);
  ADD_NDS32_BUILTIN1 ("pe_clo", unsigned, unsigned, CLO);

  /* Performance Extension 2  */
  ADD_NDS32_BUILTIN3 ("pe2_bse", void, ptr_uint, unsigned, ptr_uint, BSE);
  ADD_NDS32_BUILTIN3 ("pe2_bsp", void, ptr_uint, unsigned, ptr_uint, BSP);
  ADD_NDS32_BUILTIN2 ("pe2_pbsad", unsigned, unsigned, unsigned, PBSAD);
  ADD_NDS32_BUILTIN3 ("pe2_pbsada", unsigned, unsigned, unsigned, unsigned,
		      PBSADA);

  /* String Extension  */
  ADD_NDS32_BUILTIN2 ("se_ffb", integer, unsigned, unsigned, FFB);
  ADD_NDS32_BUILTIN2 ("se_ffmism", integer, unsigned, unsigned, FFMISM);
  ADD_NDS32_BUILTIN2 ("se_flmism", integer, unsigned, unsigned, FLMISM);


  /* ROTR  */
  ADD_NDS32_BUILTIN2 ("rotr", unsigned, unsigned, unsigned, ROTR);

  /* Swap  */
  ADD_NDS32_BUILTIN1 ("wsbh", unsigned, unsigned, WSBH);

  /* System  */
  ADD_NDS32_BUILTIN2 ("svs", unsigned, integer, integer, SVS);
  ADD_NDS32_BUILTIN2 ("sva", unsigned, integer, integer, SVA);
  ADD_NDS32_BUILTIN1 ("jr_itoff", void, unsigned, JR_ITOFF);
  ADD_NDS32_BUILTIN1 ("jr_toff", void, unsigned, JR_TOFF);
  ADD_NDS32_BUILTIN1 ("jral_iton", void, unsigned, JRAL_ITON);
  ADD_NDS32_BUILTIN1 ("jral_ton", void, unsigned, JRAL_TON);
  ADD_NDS32_BUILTIN1 ("ret_itoff", void, unsigned, RET_ITOFF);
  ADD_NDS32_BUILTIN1 ("ret_toff", void, unsigned, RET_TOFF);
  ADD_NDS32_BUILTIN0 ("standby_no_wake_grant", void, STANDBY_NO_WAKE_GRANT);
  ADD_NDS32_BUILTIN0 ("standby_wake_grant", void, STANDBY_WAKE_GRANT);
  ADD_NDS32_BUILTIN0 ("standby_wait_done", void, STANDBY_WAKE_DONE);
  ADD_NDS32_BUILTIN1 ("break", void, unsigned, BREAK);
  ADD_NDS32_BUILTIN1 ("syscall", void, unsigned, SYSCALL);
  ADD_NDS32_BUILTIN0 ("nop", void, NOP);
  ADD_NDS32_BUILTIN0 ("get_current_sp", unsigned, GET_CURRENT_SP);
  ADD_NDS32_BUILTIN1 ("set_current_sp", void, unsigned, SET_CURRENT_SP);
  ADD_NDS32_BUILTIN2 ("teqz", void, unsigned, unsigned, TEQZ);
  ADD_NDS32_BUILTIN2 ("tnez", void, unsigned, unsigned, TNEZ);
  ADD_NDS32_BUILTIN1 ("trap", void, unsigned, TRAP);
  ADD_NDS32_BUILTIN0 ("return_address", unsigned, RETURN_ADDRESS);
  ADD_NDS32_BUILTIN0 ("setend_big", void, SETEND_BIG);
  ADD_NDS32_BUILTIN0 ("setend_little", void, SETEND_LITTLE);

  /* Schedule Barrier */
  ADD_NDS32_BUILTIN0 ("schedule_barrier", void, SCHE_BARRIER);

  /* TLBOP  */
  ADD_NDS32_BUILTIN1 ("tlbop_trd", void, unsigned, TLBOP_TRD);
  ADD_NDS32_BUILTIN1 ("tlbop_twr", void, unsigned, TLBOP_TWR);
  ADD_NDS32_BUILTIN1 ("tlbop_rwr", void, unsigned, TLBOP_RWR);
  ADD_NDS32_BUILTIN1 ("tlbop_rwlk", void, unsigned, TLBOP_RWLK);
  ADD_NDS32_BUILTIN1 ("tlbop_unlk", void, unsigned, TLBOP_UNLK);
  ADD_NDS32_BUILTIN1 ("tlbop_pb", unsigned, unsigned, TLBOP_PB);
  ADD_NDS32_BUILTIN1 ("tlbop_inv", void, unsigned, TLBOP_INV);
  ADD_NDS32_BUILTIN0 ("tlbop_flua", void, TLBOP_FLUA);

  /* Unaligned Load/Store  */
  ADD_NDS32_BUILTIN1 ("unaligned_load_hw", short_unsigned, ptr_ushort,
		      UALOAD_HW);
  ADD_NDS32_BUILTIN1 ("unaligned_load_w", unsigned, ptr_uint, UALOAD_W);
  ADD_NDS32_BUILTIN1 ("unaligned_load_dw", long_long_unsigned, ptr_ulong,
		      UALOAD_DW);
  ADD_NDS32_BUILTIN2 ("unaligned_store_hw", void, ptr_ushort, short_unsigned,
		      UASTORE_HW);
  ADD_NDS32_BUILTIN2 ("unaligned_store_w", void, ptr_uint, unsigned, UASTORE_W);
  ADD_NDS32_BUILTIN2 ("unaligned_store_dw", void, ptr_ulong, long_long_unsigned,
		      UASTORE_DW);
  ADD_NDS32_BUILTIN0 ("unaligned_feature", unsigned, UNALIGNED_FEATURE);
  ADD_NDS32_BUILTIN0 ("enable_unaligned", void, ENABLE_UNALIGNED);
  ADD_NDS32_BUILTIN0 ("disable_unaligned", void, DISABLE_UNALIGNED);

}
