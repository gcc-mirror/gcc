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
  NDS32_BUILTIN(unspec_volatile_rdov, "rdov", RDOV)
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
  NDS32_BUILTIN(kabsv2hi2, "kabs16", KABS16)
  NDS32_BUILTIN(kabsv2hi2, "v_kabs16", V_KABS16)
  NDS32_BUILTIN(kabsv4qi2, "kabs8", KABS8)
  NDS32_BUILTIN(kabsv4qi2, "v_kabs8", V_KABS8)
  NDS32_BUILTIN(sunpkd810, "sunpkd810", SUNPKD810)
  NDS32_BUILTIN(sunpkd810, "v_sunpkd810", V_SUNPKD810)
  NDS32_BUILTIN(sunpkd820, "sunpkd820", SUNPKD820)
  NDS32_BUILTIN(sunpkd820, "v_sunpkd820", V_SUNPKD820)
  NDS32_BUILTIN(sunpkd830, "sunpkd830", SUNPKD830)
  NDS32_BUILTIN(sunpkd830, "v_sunpkd830", V_SUNPKD830)
  NDS32_BUILTIN(sunpkd831, "sunpkd831", SUNPKD831)
  NDS32_BUILTIN(sunpkd831, "v_sunpkd831", V_SUNPKD831)
  NDS32_BUILTIN(zunpkd810, "zunpkd810", ZUNPKD810)
  NDS32_BUILTIN(zunpkd810, "v_zunpkd810", V_ZUNPKD810)
  NDS32_BUILTIN(zunpkd820, "zunpkd820", ZUNPKD820)
  NDS32_BUILTIN(zunpkd820, "v_zunpkd820", V_ZUNPKD820)
  NDS32_BUILTIN(zunpkd830, "zunpkd830", ZUNPKD830)
  NDS32_BUILTIN(zunpkd830, "v_zunpkd830", V_ZUNPKD830)
  NDS32_BUILTIN(zunpkd831, "zunpkd831", ZUNPKD831)
  NDS32_BUILTIN(zunpkd831, "v_zunpkd831", V_ZUNPKD831)
  NDS32_BUILTIN(unspec_kabs, "kabs", KABS)
  NDS32_BUILTIN(unaligned_loadv2hi, "get_unaligned_u16x2", UALOAD_U16)
  NDS32_BUILTIN(unaligned_loadv2hi, "get_unaligned_s16x2", UALOAD_S16)
  NDS32_BUILTIN(unaligned_loadv4qi, "get_unaligned_u8x4", UALOAD_U8)
  NDS32_BUILTIN(unaligned_loadv4qi, "get_unaligned_s8x4", UALOAD_S8)
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
  NDS32_BUILTIN(unspec_kaddw, "kaddw", KADDW)
  NDS32_BUILTIN(unspec_kaddh, "kaddh", KADDH)
  NDS32_BUILTIN(unspec_ksubw, "ksubw", KSUBW)
  NDS32_BUILTIN(unspec_ksubh, "ksubh", KSUBH)
  NDS32_BUILTIN(unspec_kdmbb, "kdmbb", KDMBB)
  NDS32_BUILTIN(unspec_kdmbb, "v_kdmbb", V_KDMBB)
  NDS32_BUILTIN(unspec_kdmbt, "kdmbt", KDMBT)
  NDS32_BUILTIN(unspec_kdmbt, "v_kdmbt", V_KDMBT)
  NDS32_BUILTIN(unspec_kdmtb, "kdmtb", KDMTB)
  NDS32_BUILTIN(unspec_kdmtb, "v_kdmtb", V_KDMTB)
  NDS32_BUILTIN(unspec_kdmtt, "kdmtt", KDMTT)
  NDS32_BUILTIN(unspec_kdmtt, "v_kdmtt", V_KDMTT)
  NDS32_BUILTIN(unspec_khmbb, "khmbb", KHMBB)
  NDS32_BUILTIN(unspec_khmbb, "v_khmbb", V_KHMBB)
  NDS32_BUILTIN(unspec_khmbt, "khmbt", KHMBT)
  NDS32_BUILTIN(unspec_khmbt, "v_khmbt", V_KHMBT)
  NDS32_BUILTIN(unspec_khmtb, "khmtb", KHMTB)
  NDS32_BUILTIN(unspec_khmtb, "v_khmtb", V_KHMTB)
  NDS32_BUILTIN(unspec_khmtt, "khmtt", KHMTT)
  NDS32_BUILTIN(unspec_khmtt, "v_khmtt", V_KHMTT)
  NDS32_BUILTIN(unspec_kslraw, "kslraw", KSLRAW)
  NDS32_BUILTIN(unspec_kslrawu, "kslraw_u", KSLRAW_U)
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
  NDS32_BUILTIN(addv2hi3, "add16", ADD16)
  NDS32_BUILTIN(addv2hi3, "v_uadd16", V_UADD16)
  NDS32_BUILTIN(addv2hi3, "v_sadd16", V_SADD16)
  NDS32_BUILTIN(raddv2hi3, "radd16", RADD16)
  NDS32_BUILTIN(raddv2hi3, "v_radd16", V_RADD16)
  NDS32_BUILTIN(uraddv2hi3, "uradd16", URADD16)
  NDS32_BUILTIN(uraddv2hi3, "v_uradd16", V_URADD16)
  NDS32_BUILTIN(kaddv2hi3, "kadd16", KADD16)
  NDS32_BUILTIN(kaddv2hi3, "v_kadd16", V_KADD16)
  NDS32_BUILTIN(ukaddv2hi3, "ukadd16", UKADD16)
  NDS32_BUILTIN(ukaddv2hi3, "v_ukadd16", V_UKADD16)
  NDS32_BUILTIN(subv2hi3, "sub16", SUB16)
  NDS32_BUILTIN(subv2hi3, "v_usub16", V_USUB16)
  NDS32_BUILTIN(subv2hi3, "v_ssub16", V_SSUB16)
  NDS32_BUILTIN(rsubv2hi3, "rsub16", RSUB16)
  NDS32_BUILTIN(rsubv2hi3, "v_rsub16", V_RSUB16)
  NDS32_BUILTIN(ursubv2hi3, "ursub16", URSUB16)
  NDS32_BUILTIN(ursubv2hi3, "v_ursub16", V_URSUB16)
  NDS32_BUILTIN(ksubv2hi3, "ksub16", KSUB16)
  NDS32_BUILTIN(ksubv2hi3, "v_ksub16", V_KSUB16)
  NDS32_BUILTIN(uksubv2hi3, "uksub16", UKSUB16)
  NDS32_BUILTIN(uksubv2hi3, "v_uksub16", V_UKSUB16)
  NDS32_BUILTIN(cras16_1, "cras16", CRAS16)
  NDS32_BUILTIN(cras16_1, "v_ucras16", V_UCRAS16)
  NDS32_BUILTIN(cras16_1, "v_scras16", V_SCRAS16)
  NDS32_BUILTIN(rcras16_1, "rcras16", RCRAS16)
  NDS32_BUILTIN(rcras16_1, "v_rcras16", V_RCRAS16)
  NDS32_BUILTIN(urcras16_1, "urcras16", URCRAS16)
  NDS32_BUILTIN(urcras16_1, "v_urcras16", V_URCRAS16)
  NDS32_BUILTIN(kcras16_1, "kcras16", KCRAS16)
  NDS32_BUILTIN(kcras16_1, "v_kcras16", V_KCRAS16)
  NDS32_BUILTIN(ukcras16_1, "ukcras16", UKCRAS16)
  NDS32_BUILTIN(ukcras16_1, "v_ukcras16", V_UKCRAS16)
  NDS32_BUILTIN(crsa16_1, "crsa16", CRSA16)
  NDS32_BUILTIN(crsa16_1, "v_ucrsa16", V_UCRSA16)
  NDS32_BUILTIN(crsa16_1, "v_scrsa16", V_SCRSA16)
  NDS32_BUILTIN(rcrsa16_1, "rcrsa16", RCRSA16)
  NDS32_BUILTIN(rcrsa16_1, "v_rcrsa16", V_RCRSA16)
  NDS32_BUILTIN(urcrsa16_1, "urcrsa16", URCRSA16)
  NDS32_BUILTIN(urcrsa16_1, "v_urcrsa16", V_URCRSA16)
  NDS32_BUILTIN(kcrsa16_1, "kcrsa16", KCRSA16)
  NDS32_BUILTIN(kcrsa16_1, "v_kcrsa16", V_KCRSA16)
  NDS32_BUILTIN(ukcrsa16_1, "ukcrsa16", UKCRSA16)
  NDS32_BUILTIN(ukcrsa16_1, "v_ukcrsa16", V_UKCRSA16)
  NDS32_BUILTIN(addv4qi3, "add8", ADD8)
  NDS32_BUILTIN(addv4qi3, "v_uadd8", V_UADD8)
  NDS32_BUILTIN(addv4qi3, "v_sadd8", V_SADD8)
  NDS32_BUILTIN(raddv4qi3, "radd8", RADD8)
  NDS32_BUILTIN(raddv4qi3, "v_radd8", V_RADD8)
  NDS32_BUILTIN(uraddv4qi3, "uradd8", URADD8)
  NDS32_BUILTIN(uraddv4qi3, "v_uradd8", V_URADD8)
  NDS32_BUILTIN(kaddv4qi3, "kadd8", KADD8)
  NDS32_BUILTIN(kaddv4qi3, "v_kadd8", V_KADD8)
  NDS32_BUILTIN(ukaddv4qi3, "ukadd8", UKADD8)
  NDS32_BUILTIN(ukaddv4qi3, "v_ukadd8", V_UKADD8)
  NDS32_BUILTIN(subv4qi3, "sub8", SUB8)
  NDS32_BUILTIN(subv4qi3, "v_usub8", V_USUB8)
  NDS32_BUILTIN(subv4qi3, "v_ssub8", V_SSUB8)
  NDS32_BUILTIN(rsubv4qi3, "rsub8", RSUB8)
  NDS32_BUILTIN(rsubv4qi3, "v_rsub8", V_RSUB8)
  NDS32_BUILTIN(ursubv4qi3, "ursub8", URSUB8)
  NDS32_BUILTIN(ursubv4qi3, "v_ursub8", V_URSUB8)
  NDS32_BUILTIN(ksubv4qi3, "ksub8", KSUB8)
  NDS32_BUILTIN(ksubv4qi3, "v_ksub8", V_KSUB8)
  NDS32_BUILTIN(uksubv4qi3, "uksub8", UKSUB8)
  NDS32_BUILTIN(uksubv4qi3, "v_uksub8", V_UKSUB8)
  NDS32_BUILTIN(ashrv2hi3, "sra16", SRA16)
  NDS32_BUILTIN(ashrv2hi3, "v_sra16", V_SRA16)
  NDS32_BUILTIN(sra16_round, "sra16_u", SRA16_U)
  NDS32_BUILTIN(sra16_round, "v_sra16_u", V_SRA16_U)
  NDS32_BUILTIN(lshrv2hi3, "srl16", SRL16)
  NDS32_BUILTIN(lshrv2hi3, "v_srl16", V_SRL16)
  NDS32_BUILTIN(srl16_round, "srl16_u", SRL16_U)
  NDS32_BUILTIN(srl16_round, "v_srl16_u", V_SRL16_U)
  NDS32_BUILTIN(ashlv2hi3, "sll16", SLL16)
  NDS32_BUILTIN(ashlv2hi3, "v_sll16", V_SLL16)
  NDS32_BUILTIN(kslli16, "ksll16", KSLL16)
  NDS32_BUILTIN(kslli16, "v_ksll16", V_KSLL16)
  NDS32_BUILTIN(kslra16, "kslra16", KSLRA16)
  NDS32_BUILTIN(kslra16, "v_kslra16", V_KSLRA16)
  NDS32_BUILTIN(kslra16_round, "kslra16_u", KSLRA16_U)
  NDS32_BUILTIN(kslra16_round, "v_kslra16_u", V_KSLRA16_U)
  NDS32_BUILTIN(cmpeq16, "cmpeq16", CMPEQ16)
  NDS32_BUILTIN(cmpeq16, "v_scmpeq16", V_SCMPEQ16)
  NDS32_BUILTIN(cmpeq16, "v_ucmpeq16", V_UCMPEQ16)
  NDS32_BUILTIN(scmplt16, "scmplt16", SCMPLT16)
  NDS32_BUILTIN(scmplt16, "v_scmplt16", V_SCMPLT16)
  NDS32_BUILTIN(scmple16, "scmple16", SCMPLE16)
  NDS32_BUILTIN(scmple16, "v_scmple16", V_SCMPLE16)
  NDS32_BUILTIN(ucmplt16, "ucmplt16", UCMPLT16)
  NDS32_BUILTIN(ucmplt16, "v_ucmplt16", V_UCMPLT16)
  NDS32_BUILTIN(ucmplt16, "ucmple16", UCMPLE16)
  NDS32_BUILTIN(ucmplt16, "v_ucmple16", V_UCMPLE16)
  NDS32_BUILTIN(cmpeq8, "cmpeq8", CMPEQ8)
  NDS32_BUILTIN(cmpeq8, "v_scmpeq8", V_SCMPEQ8)
  NDS32_BUILTIN(cmpeq8, "v_ucmpeq8", V_UCMPEQ8)
  NDS32_BUILTIN(scmplt8, "scmplt8", SCMPLT8)
  NDS32_BUILTIN(scmplt8, "v_scmplt8", V_SCMPLT8)
  NDS32_BUILTIN(scmple8, "scmple8", SCMPLE8)
  NDS32_BUILTIN(scmple8, "v_scmple8", V_SCMPLE8)
  NDS32_BUILTIN(ucmplt8, "ucmplt8", UCMPLT8)
  NDS32_BUILTIN(ucmplt8, "v_ucmplt8", V_UCMPLT8)
  NDS32_BUILTIN(ucmplt8, "ucmple8", UCMPLE8)
  NDS32_BUILTIN(ucmplt8, "v_ucmple8", V_UCMPLE8)
  NDS32_BUILTIN(sminv2hi3, "smin16", SMIN16)
  NDS32_BUILTIN(sminv2hi3, "v_smin16", V_SMIN16)
  NDS32_BUILTIN(uminv2hi3, "umin16", UMIN16)
  NDS32_BUILTIN(uminv2hi3, "v_umin16", V_UMIN16)
  NDS32_BUILTIN(smaxv2hi3, "smax16", SMAX16)
  NDS32_BUILTIN(smaxv2hi3, "v_smax16", V_SMAX16)
  NDS32_BUILTIN(umaxv2hi3, "umax16", UMAX16)
  NDS32_BUILTIN(umaxv2hi3, "v_umax16", V_UMAX16)
  NDS32_BUILTIN(khm16, "khm16", KHM16)
  NDS32_BUILTIN(khm16, "v_khm16", V_KHM16)
  NDS32_BUILTIN(khmx16, "khmx16", KHMX16)
  NDS32_BUILTIN(khmx16, "v_khmx16", V_KHMX16)
  NDS32_BUILTIN(sminv4qi3, "smin8", SMIN8)
  NDS32_BUILTIN(sminv4qi3, "v_smin8", V_SMIN8)
  NDS32_BUILTIN(uminv4qi3, "umin8", UMIN8)
  NDS32_BUILTIN(uminv4qi3, "v_umin8", V_UMIN8)
  NDS32_BUILTIN(smaxv4qi3, "smax8", SMAX8)
  NDS32_BUILTIN(smaxv4qi3, "v_smax8", V_SMAX8)
  NDS32_BUILTIN(umaxv4qi3, "umax8", UMAX8)
  NDS32_BUILTIN(umaxv4qi3, "v_umax8", V_UMAX8)
  NDS32_BUILTIN(raddsi3, "raddw", RADDW)
  NDS32_BUILTIN(uraddsi3, "uraddw", URADDW)
  NDS32_BUILTIN(rsubsi3, "rsubw", RSUBW)
  NDS32_BUILTIN(ursubsi3, "ursubw", URSUBW)
  NDS32_BUILTIN(sraiu, "sra_u", SRA_U)
  NDS32_BUILTIN(kssl, "ksll", KSLL)
  NDS32_BUILTIN(pkbb, "pkbb16", PKBB16)
  NDS32_BUILTIN(pkbb, "v_pkbb16", V_PKBB16)
  NDS32_BUILTIN(pkbt, "pkbt16", PKBT16)
  NDS32_BUILTIN(pkbt, "v_pkbt16", V_PKBT16)
  NDS32_BUILTIN(pktb, "pktb16", PKTB16)
  NDS32_BUILTIN(pktb, "v_pktb16", V_PKTB16)
  NDS32_BUILTIN(pktt, "pktt16", PKTT16)
  NDS32_BUILTIN(pktt, "v_pktt16", V_PKTT16)
  NDS32_BUILTIN(smulsi3_highpart, "smmul", SMMUL)
  NDS32_BUILTIN(smmul_round, "smmul_u", SMMUL_U)
  NDS32_BUILTIN(smmwb, "smmwb", SMMWB)
  NDS32_BUILTIN(smmwb, "v_smmwb", V_SMMWB)
  NDS32_BUILTIN(smmwb_round, "smmwb_u", SMMWB_U)
  NDS32_BUILTIN(smmwb_round, "v_smmwb_u", V_SMMWB_U)
  NDS32_BUILTIN(smmwt, "smmwt", SMMWT)
  NDS32_BUILTIN(smmwt, "v_smmwt", V_SMMWT)
  NDS32_BUILTIN(smmwt_round, "smmwt_u", SMMWT_U)
  NDS32_BUILTIN(smmwt_round, "v_smmwt_u", V_SMMWT_U)
  NDS32_BUILTIN(smbb, "smbb", SMBB)
  NDS32_BUILTIN(smbb, "v_smbb", V_SMBB)
  NDS32_BUILTIN(smbt, "smbt", SMBT)
  NDS32_BUILTIN(smbt, "v_smbt", V_SMBT)
  NDS32_BUILTIN(smtt, "smtt", SMTT)
  NDS32_BUILTIN(smtt, "v_smtt", V_SMTT)
  NDS32_BUILTIN(kmda, "kmda", KMDA)
  NDS32_BUILTIN(kmda, "v_kmda", V_KMDA)
  NDS32_BUILTIN(kmxda, "kmxda", KMXDA)
  NDS32_BUILTIN(kmxda, "v_kmxda", V_KMXDA)
  NDS32_BUILTIN(smds, "smds", SMDS)
  NDS32_BUILTIN(smds, "v_smds", V_SMDS)
  NDS32_BUILTIN(smdrs, "smdrs", SMDRS)
  NDS32_BUILTIN(smdrs, "v_smdrs", V_SMDRS)
  NDS32_BUILTIN(smxdsv, "smxds", SMXDS)
  NDS32_BUILTIN(smxdsv, "v_smxds", V_SMXDS)
  NDS32_BUILTIN(smal1, "smal", SMAL)
  NDS32_BUILTIN(smal1, "v_smal", V_SMAL)
  NDS32_BUILTIN(bitrev, "bitrev", BITREV)
  NDS32_BUILTIN(wext, "wext", WEXT)
  NDS32_BUILTIN(adddi3, "sadd64", SADD64)
  NDS32_BUILTIN(adddi3, "uadd64", UADD64)
  NDS32_BUILTIN(radddi3, "radd64", RADD64)
  NDS32_BUILTIN(uradddi3, "uradd64", URADD64)
  NDS32_BUILTIN(kadddi3, "kadd64", KADD64)
  NDS32_BUILTIN(ukadddi3, "ukadd64", UKADD64)
  NDS32_BUILTIN(subdi3, "ssub64", SSUB64)
  NDS32_BUILTIN(subdi3, "usub64", USUB64)
  NDS32_BUILTIN(rsubdi3, "rsub64", RSUB64)
  NDS32_BUILTIN(ursubdi3, "ursub64", URSUB64)
  NDS32_BUILTIN(ksubdi3, "ksub64", KSUB64)
  NDS32_BUILTIN(uksubdi3, "uksub64", UKSUB64)
  NDS32_BUILTIN(smul16, "smul16", SMUL16)
  NDS32_BUILTIN(smul16, "v_smul16", V_SMUL16)
  NDS32_BUILTIN(smulx16, "smulx16", SMULX16)
  NDS32_BUILTIN(smulx16, "v_smulx16", V_SMULX16)
  NDS32_BUILTIN(umul16, "umul16", UMUL16)
  NDS32_BUILTIN(umul16, "v_umul16", V_UMUL16)
  NDS32_BUILTIN(umulx16, "umulx16", UMULX16)
  NDS32_BUILTIN(umulx16, "v_umulx16", V_UMULX16)
  NDS32_BUILTIN(kwmmul, "kwmmul", KWMMUL)
  NDS32_BUILTIN(kwmmul_round, "kwmmul_u", KWMMUL_U)
  NDS32_NO_TARGET_BUILTIN(unaligned_storev2hi,
			  "put_unaligned_u16x2", UASTORE_U16)
  NDS32_NO_TARGET_BUILTIN(unaligned_storev2hi,
			  "put_unaligned_s16x2", UASTORE_S16)
  NDS32_NO_TARGET_BUILTIN(unaligned_storev4qi, "put_unaligned_u8x4", UASTORE_U8)
  NDS32_NO_TARGET_BUILTIN(unaligned_storev4qi, "put_unaligned_s8x4", UASTORE_S8)
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
  NDS32_BUILTIN(ashrv2hi3, "srl16", SRL16)
  NDS32_BUILTIN(ashrv2hi3, "v_srl16", V_SRL16)
  NDS32_BUILTIN(srl16_round, "srl16_u", SRL16_U)
  NDS32_BUILTIN(srl16_round, "v_srl16_u", V_SRL16_U)
  NDS32_BUILTIN(kslli16, "ksll16", KSLL16)
  NDS32_BUILTIN(kslli16, "v_ksll16", V_KSLL16)
  NDS32_BUILTIN(sclip16, "sclip16", SCLIP16)
  NDS32_BUILTIN(sclip16, "v_sclip16", V_SCLIP16)
  NDS32_BUILTIN(uclip16, "uclip16", UCLIP16)
  NDS32_BUILTIN(uclip16, "v_uclip16", V_UCLIP16)
  NDS32_BUILTIN(sraiu, "sra_u", SRA_U)
  NDS32_BUILTIN(kssl, "ksll", KSLL)
  NDS32_BUILTIN(bitrev, "bitrev", BITREV)
  NDS32_BUILTIN(wext, "wext", WEXT)
  NDS32_BUILTIN(uclip32, "uclip32", UCLIP32)
  NDS32_BUILTIN(sclip32, "sclip32", SCLIP32)
};

/* Intrinsics that take three arguments.  */
static struct builtin_description bdesc_3arg[] =
{
  NDS32_BUILTIN(unspec_pbsada, "pbsada", PBSADA)
  NDS32_NO_TARGET_BUILTIN(bse, "bse", BSE)
  NDS32_NO_TARGET_BUILTIN(bsp, "bsp", BSP)
  NDS32_BUILTIN(kmabb, "kmabb", KMABB)
  NDS32_BUILTIN(kmabb, "v_kmabb", V_KMABB)
  NDS32_BUILTIN(kmabt, "kmabt", KMABT)
  NDS32_BUILTIN(kmabt, "v_kmabt", V_KMABT)
  NDS32_BUILTIN(kmatt, "kmatt", KMATT)
  NDS32_BUILTIN(kmatt, "v_kmatt", V_KMATT)
  NDS32_BUILTIN(kmada, "kmada", KMADA)
  NDS32_BUILTIN(kmada, "v_kmada", V_KMADA)
  NDS32_BUILTIN(kmaxda, "kmaxda", KMAXDA)
  NDS32_BUILTIN(kmaxda, "v_kmaxda", V_KMAXDA)
  NDS32_BUILTIN(kmads, "kmads", KMADS)
  NDS32_BUILTIN(kmads, "v_kmads", V_KMADS)
  NDS32_BUILTIN(kmadrs, "kmadrs", KMADRS)
  NDS32_BUILTIN(kmadrs, "v_kmadrs", V_KMADRS)
  NDS32_BUILTIN(kmaxds, "kmaxds", KMAXDS)
  NDS32_BUILTIN(kmaxds, "v_kmaxds", V_KMAXDS)
  NDS32_BUILTIN(kmsda, "kmsda", KMSDA)
  NDS32_BUILTIN(kmsda, "v_kmsda", V_KMSDA)
  NDS32_BUILTIN(kmsxda, "kmsxda", KMSXDA)
  NDS32_BUILTIN(kmsxda, "v_kmsxda", V_KMSXDA)
  NDS32_BUILTIN(bpick1, "bpick", BPICK)
  NDS32_BUILTIN(smar64_1, "smar64", SMAR64)
  NDS32_BUILTIN(smsr64, "smsr64", SMSR64)
  NDS32_BUILTIN(umar64_1, "umar64", UMAR64)
  NDS32_BUILTIN(umsr64, "umsr64", UMSR64)
  NDS32_BUILTIN(kmar64_1, "kmar64", KMAR64)
  NDS32_BUILTIN(kmsr64, "kmsr64", KMSR64)
  NDS32_BUILTIN(ukmar64_1, "ukmar64", UKMAR64)
  NDS32_BUILTIN(ukmsr64, "ukmsr64", UKMSR64)
  NDS32_BUILTIN(smalbb, "smalbb", SMALBB)
  NDS32_BUILTIN(smalbb, "v_smalbb", V_SMALBB)
  NDS32_BUILTIN(smalbt, "smalbt", SMALBT)
  NDS32_BUILTIN(smalbt, "v_smalbt", V_SMALBT)
  NDS32_BUILTIN(smaltt, "smaltt", SMALTT)
  NDS32_BUILTIN(smaltt, "v_smaltt", V_SMALTT)
  NDS32_BUILTIN(smalda1, "smalda", SMALDA)
  NDS32_BUILTIN(smalda1, "v_smalda", V_SMALDA)
  NDS32_BUILTIN(smalxda1, "smalxda", SMALXDA)
  NDS32_BUILTIN(smalxda1, "v_smalxda", V_SMALXDA)
  NDS32_BUILTIN(smalds1, "smalds", SMALDS)
  NDS32_BUILTIN(smalds1, "v_smalds", V_SMALDS)
  NDS32_BUILTIN(smaldrs3, "smaldrs", SMALDRS)
  NDS32_BUILTIN(smaldrs3, "v_smaldrs", V_SMALDRS)
  NDS32_BUILTIN(smalxds1, "smalxds", SMALXDS)
  NDS32_BUILTIN(smalxds1, "v_smalxds", V_SMALXDS)
  NDS32_BUILTIN(smslda1, "smslda", SMSLDA)
  NDS32_BUILTIN(smslda1, "v_smslda", V_SMSLDA)
  NDS32_BUILTIN(smslxda1, "smslxda", SMSLXDA)
  NDS32_BUILTIN(smslxda1, "v_smslxda", V_SMSLXDA)
  NDS32_BUILTIN(kmmawb, "kmmawb", KMMAWB)
  NDS32_BUILTIN(kmmawb, "v_kmmawb", V_KMMAWB)
  NDS32_BUILTIN(kmmawb_round, "kmmawb_u", KMMAWB_U)
  NDS32_BUILTIN(kmmawb_round, "v_kmmawb_u", V_KMMAWB_U)
  NDS32_BUILTIN(kmmawt, "kmmawt", KMMAWT)
  NDS32_BUILTIN(kmmawt, "v_kmmawt", V_KMMAWT)
  NDS32_BUILTIN(kmmawt_round, "kmmawt_u", KMMAWT_U)
  NDS32_BUILTIN(kmmawt_round, "v_kmmawt_u", V_KMMAWT_U)
  NDS32_BUILTIN(kmmac, "kmmac", KMMAC)
  NDS32_BUILTIN(kmmac_round, "kmmac_u", KMMAC_U)
  NDS32_BUILTIN(kmmsb, "kmmsb", KMMSB)
  NDS32_BUILTIN(kmmsb_round, "kmmsb_u", KMMSB_U)
};

/* Three-argument intrinsics with an immediate third argument.  */
static struct builtin_description bdesc_3argimm[] =
{
  NDS32_NO_TARGET_BUILTIN(prefetch_qw, "prefetch_qw", DPREF_QW)
  NDS32_NO_TARGET_BUILTIN(prefetch_hw, "prefetch_hw", DPREF_HW)
  NDS32_NO_TARGET_BUILTIN(prefetch_w, "prefetch_w", DPREF_W)
  NDS32_NO_TARGET_BUILTIN(prefetch_dw, "prefetch_dw", DPREF_DW)
  NDS32_BUILTIN(insb, "insb", INSB)
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

  if (!NDS32_EXT_DSP_P ()
      && fcode > NDS32_BUILTIN_DSP_BEGIN
      && fcode < NDS32_BUILTIN_DSP_END)
    error ("don't support DSP extension instructions");

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
    case NDS32_BUILTIN_CLROV:
      emit_insn (gen_unspec_volatile_clrov ());
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
  tree ptr_char_type_node = build_pointer_type (char_type_node);
  tree ptr_uchar_type_node = build_pointer_type (unsigned_char_type_node);
  tree ptr_ushort_type_node = build_pointer_type (short_unsigned_type_node);
  tree ptr_short_type_node = build_pointer_type (short_integer_type_node);
  tree ptr_uint_type_node = build_pointer_type (unsigned_type_node);
  tree ptr_ulong_type_node = build_pointer_type (long_long_unsigned_type_node);
  tree v4qi_type_node = build_vector_type (intQI_type_node, 4);
  tree u_v4qi_type_node = build_vector_type (unsigned_intQI_type_node, 4);
  tree v2hi_type_node = build_vector_type (intHI_type_node, 2);
  tree u_v2hi_type_node = build_vector_type (unsigned_intHI_type_node, 2);
  tree v2si_type_node = build_vector_type (intSI_type_node, 2);
  tree u_v2si_type_node = build_vector_type (unsigned_intSI_type_node, 2);

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

  /* SATURATION  */
  ADD_NDS32_BUILTIN2 ("kaddw", integer, integer, integer, KADDW);
  ADD_NDS32_BUILTIN2 ("ksubw", integer, integer, integer, KSUBW);
  ADD_NDS32_BUILTIN2 ("kaddh", integer, integer, integer, KADDH);
  ADD_NDS32_BUILTIN2 ("ksubh", integer, integer, integer, KSUBH);
  ADD_NDS32_BUILTIN2 ("kdmbb", integer, unsigned, unsigned, KDMBB);
  ADD_NDS32_BUILTIN2 ("v_kdmbb", integer, v2hi, v2hi, V_KDMBB);
  ADD_NDS32_BUILTIN2 ("kdmbt", integer, unsigned, unsigned, KDMBT);
  ADD_NDS32_BUILTIN2 ("v_kdmbt", integer, v2hi, v2hi, V_KDMBT);
  ADD_NDS32_BUILTIN2 ("kdmtb", integer, unsigned, unsigned, KDMTB);
  ADD_NDS32_BUILTIN2 ("v_kdmtb", integer, v2hi, v2hi, V_KDMTB);
  ADD_NDS32_BUILTIN2 ("kdmtt", integer, unsigned, unsigned, KDMTT);
  ADD_NDS32_BUILTIN2 ("v_kdmtt", integer, v2hi, v2hi, V_KDMTT);
  ADD_NDS32_BUILTIN2 ("khmbb", integer, unsigned, unsigned, KHMBB);
  ADD_NDS32_BUILTIN2 ("v_khmbb", integer, v2hi, v2hi, V_KHMBB);
  ADD_NDS32_BUILTIN2 ("khmbt", integer, unsigned, unsigned, KHMBT);
  ADD_NDS32_BUILTIN2 ("v_khmbt", integer, v2hi, v2hi, V_KHMBT);
  ADD_NDS32_BUILTIN2 ("khmtb", integer, unsigned, unsigned, KHMTB);
  ADD_NDS32_BUILTIN2 ("v_khmtb", integer, v2hi, v2hi, V_KHMTB);
  ADD_NDS32_BUILTIN2 ("khmtt", integer, unsigned, unsigned, KHMTT);
  ADD_NDS32_BUILTIN2 ("v_khmtt", integer, v2hi, v2hi, V_KHMTT);
  ADD_NDS32_BUILTIN2 ("kslraw", integer, integer, integer, KSLRAW);
  ADD_NDS32_BUILTIN2 ("kslraw_u", integer, integer, integer, KSLRAW_U);
  ADD_NDS32_BUILTIN0 ("rdov", unsigned, RDOV);
  ADD_NDS32_BUILTIN0 ("clrov", void, CLROV);

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

  /* DSP Extension: SIMD 16bit Add and Subtract.  */
  ADD_NDS32_BUILTIN2 ("add16", unsigned, unsigned, unsigned, ADD16);
  ADD_NDS32_BUILTIN2 ("v_uadd16", u_v2hi, u_v2hi, u_v2hi, V_UADD16);
  ADD_NDS32_BUILTIN2 ("v_sadd16", v2hi, v2hi, v2hi, V_SADD16);
  ADD_NDS32_BUILTIN2 ("radd16", unsigned, unsigned, unsigned, RADD16);
  ADD_NDS32_BUILTIN2 ("v_radd16", v2hi, v2hi, v2hi, V_RADD16);
  ADD_NDS32_BUILTIN2 ("uradd16", unsigned, unsigned, unsigned, URADD16);
  ADD_NDS32_BUILTIN2 ("v_uradd16", u_v2hi, u_v2hi, u_v2hi, V_URADD16);
  ADD_NDS32_BUILTIN2 ("kadd16", unsigned, unsigned, unsigned, KADD16);
  ADD_NDS32_BUILTIN2 ("v_kadd16", v2hi, v2hi, v2hi, V_KADD16);
  ADD_NDS32_BUILTIN2 ("ukadd16", unsigned, unsigned, unsigned, UKADD16);
  ADD_NDS32_BUILTIN2 ("v_ukadd16", u_v2hi, u_v2hi, u_v2hi, V_UKADD16);
  ADD_NDS32_BUILTIN2 ("sub16", unsigned, unsigned, unsigned, SUB16);
  ADD_NDS32_BUILTIN2 ("v_usub16", u_v2hi, u_v2hi, u_v2hi, V_USUB16);
  ADD_NDS32_BUILTIN2 ("v_ssub16", v2hi, v2hi, v2hi, V_SSUB16);
  ADD_NDS32_BUILTIN2 ("rsub16", unsigned, unsigned, unsigned, RSUB16);
  ADD_NDS32_BUILTIN2 ("v_rsub16", v2hi, v2hi, v2hi, V_RSUB16);
  ADD_NDS32_BUILTIN2 ("ursub16", unsigned, unsigned, unsigned, URSUB16);
  ADD_NDS32_BUILTIN2 ("v_ursub16", u_v2hi, u_v2hi, u_v2hi, V_URSUB16);
  ADD_NDS32_BUILTIN2 ("ksub16", unsigned, unsigned, unsigned, KSUB16);
  ADD_NDS32_BUILTIN2 ("v_ksub16", v2hi, v2hi, v2hi, V_KSUB16);
  ADD_NDS32_BUILTIN2 ("uksub16", unsigned, unsigned, unsigned, UKSUB16);
  ADD_NDS32_BUILTIN2 ("v_uksub16", u_v2hi, u_v2hi, u_v2hi, V_UKSUB16);
  ADD_NDS32_BUILTIN2 ("cras16", unsigned, unsigned, unsigned, CRAS16);
  ADD_NDS32_BUILTIN2 ("v_ucras16", u_v2hi, u_v2hi, u_v2hi, V_UCRAS16);
  ADD_NDS32_BUILTIN2 ("v_scras16", v2hi, v2hi, v2hi, V_SCRAS16);
  ADD_NDS32_BUILTIN2 ("rcras16", unsigned, unsigned, unsigned, RCRAS16);
  ADD_NDS32_BUILTIN2 ("v_rcras16", v2hi, v2hi, v2hi, V_RCRAS16);
  ADD_NDS32_BUILTIN2 ("urcras16", unsigned, unsigned, unsigned, URCRAS16);
  ADD_NDS32_BUILTIN2 ("v_urcras16", u_v2hi, u_v2hi, u_v2hi, V_URCRAS16);
  ADD_NDS32_BUILTIN2 ("kcras16", unsigned, unsigned, unsigned, KCRAS16);
  ADD_NDS32_BUILTIN2 ("v_kcras16", v2hi, v2hi, v2hi, V_KCRAS16);
  ADD_NDS32_BUILTIN2 ("ukcras16", unsigned, unsigned, unsigned, UKCRAS16);
  ADD_NDS32_BUILTIN2 ("v_ukcras16", u_v2hi, u_v2hi, u_v2hi, V_UKCRAS16);
  ADD_NDS32_BUILTIN2 ("crsa16", unsigned, unsigned, unsigned, CRSA16);
  ADD_NDS32_BUILTIN2 ("v_ucrsa16", u_v2hi, u_v2hi, u_v2hi, V_UCRSA16);
  ADD_NDS32_BUILTIN2 ("v_scrsa16", v2hi, v2hi, v2hi, V_SCRSA16);
  ADD_NDS32_BUILTIN2 ("rcrsa16", unsigned, unsigned, unsigned, RCRSA16);
  ADD_NDS32_BUILTIN2 ("v_rcrsa16", v2hi, v2hi, v2hi, V_RCRSA16);
  ADD_NDS32_BUILTIN2 ("urcrsa16", unsigned, unsigned, unsigned, URCRSA16);
  ADD_NDS32_BUILTIN2 ("v_urcrsa16", u_v2hi, u_v2hi, u_v2hi, V_URCRSA16);
  ADD_NDS32_BUILTIN2 ("kcrsa16", unsigned, unsigned, unsigned, KCRSA16);
  ADD_NDS32_BUILTIN2 ("v_kcrsa16", v2hi, v2hi, v2hi, V_KCRSA16);
  ADD_NDS32_BUILTIN2 ("ukcrsa16", unsigned, unsigned, unsigned, UKCRSA16);
  ADD_NDS32_BUILTIN2 ("v_ukcrsa16", u_v2hi, u_v2hi, u_v2hi, V_UKCRSA16);

  /* DSP Extension: SIMD 8bit Add and Subtract.  */
  ADD_NDS32_BUILTIN2 ("add8", integer, integer, integer, ADD8);
  ADD_NDS32_BUILTIN2 ("v_uadd8", u_v4qi, u_v4qi, u_v4qi, V_UADD8);
  ADD_NDS32_BUILTIN2 ("v_sadd8", v4qi, v4qi, v4qi, V_SADD8);
  ADD_NDS32_BUILTIN2 ("radd8", unsigned, unsigned, unsigned, RADD8);
  ADD_NDS32_BUILTIN2 ("v_radd8", v4qi, v4qi, v4qi, V_RADD8);
  ADD_NDS32_BUILTIN2 ("uradd8", unsigned, unsigned, unsigned, URADD8);
  ADD_NDS32_BUILTIN2 ("v_uradd8", u_v4qi, u_v4qi, u_v4qi, V_URADD8);
  ADD_NDS32_BUILTIN2 ("kadd8", unsigned, unsigned, unsigned, KADD8);
  ADD_NDS32_BUILTIN2 ("v_kadd8", v4qi, v4qi, v4qi, V_KADD8);
  ADD_NDS32_BUILTIN2 ("ukadd8", unsigned, unsigned, unsigned, UKADD8);
  ADD_NDS32_BUILTIN2 ("v_ukadd8", u_v4qi, u_v4qi, u_v4qi, V_UKADD8);
  ADD_NDS32_BUILTIN2 ("sub8", integer, integer, integer, SUB8);
  ADD_NDS32_BUILTIN2 ("v_usub8", u_v4qi, u_v4qi, u_v4qi, V_USUB8);
  ADD_NDS32_BUILTIN2 ("v_ssub8", v4qi, v4qi, v4qi, V_SSUB8);
  ADD_NDS32_BUILTIN2 ("rsub8", unsigned, unsigned, unsigned, RSUB8);
  ADD_NDS32_BUILTIN2 ("v_rsub8", v4qi, v4qi, v4qi, V_RSUB8);
  ADD_NDS32_BUILTIN2 ("ursub8", unsigned, unsigned, unsigned, URSUB8);
  ADD_NDS32_BUILTIN2 ("v_ursub8", u_v4qi, u_v4qi, u_v4qi, V_URSUB8);
  ADD_NDS32_BUILTIN2 ("ksub8", unsigned, unsigned, unsigned, KSUB8);
  ADD_NDS32_BUILTIN2 ("v_ksub8", v4qi, v4qi, v4qi, V_KSUB8);
  ADD_NDS32_BUILTIN2 ("uksub8", unsigned, unsigned, unsigned, UKSUB8);
  ADD_NDS32_BUILTIN2 ("v_uksub8", u_v4qi, u_v4qi, u_v4qi, V_UKSUB8);

  /* DSP Extension: SIMD 16bit Shift.  */
  ADD_NDS32_BUILTIN2 ("sra16", unsigned, unsigned, unsigned, SRA16);
  ADD_NDS32_BUILTIN2 ("v_sra16", v2hi, v2hi, unsigned, V_SRA16);
  ADD_NDS32_BUILTIN2 ("sra16_u", unsigned, unsigned, unsigned, SRA16_U);
  ADD_NDS32_BUILTIN2 ("v_sra16_u", v2hi, v2hi, unsigned, V_SRA16_U);
  ADD_NDS32_BUILTIN2 ("srl16", unsigned, unsigned, unsigned, SRL16);
  ADD_NDS32_BUILTIN2 ("v_srl16", u_v2hi, u_v2hi, unsigned, V_SRL16);
  ADD_NDS32_BUILTIN2 ("srl16_u", unsigned, unsigned, unsigned, SRL16_U);
  ADD_NDS32_BUILTIN2 ("v_srl16_u", u_v2hi, u_v2hi, unsigned, V_SRL16_U);
  ADD_NDS32_BUILTIN2 ("sll16", unsigned, unsigned, unsigned, SLL16);
  ADD_NDS32_BUILTIN2 ("v_sll16", u_v2hi, u_v2hi, unsigned, V_SLL16);
  ADD_NDS32_BUILTIN2 ("ksll16", unsigned, unsigned, unsigned, KSLL16);
  ADD_NDS32_BUILTIN2 ("v_ksll16", v2hi, v2hi, unsigned, V_KSLL16);
  ADD_NDS32_BUILTIN2 ("kslra16", unsigned, unsigned, unsigned, KSLRA16);
  ADD_NDS32_BUILTIN2 ("v_kslra16", v2hi, v2hi, unsigned, V_KSLRA16);
  ADD_NDS32_BUILTIN2 ("kslra16_u", unsigned, unsigned, unsigned, KSLRA16_U);
  ADD_NDS32_BUILTIN2 ("v_kslra16_u", v2hi, v2hi, unsigned, V_KSLRA16_U);

  /* DSP Extension: 16bit Compare.  */
  ADD_NDS32_BUILTIN2 ("cmpeq16", unsigned, unsigned, unsigned, CMPEQ16);
  ADD_NDS32_BUILTIN2 ("v_scmpeq16", u_v2hi, v2hi, v2hi, V_SCMPEQ16);
  ADD_NDS32_BUILTIN2 ("v_ucmpeq16", u_v2hi, u_v2hi, u_v2hi, V_UCMPEQ16);
  ADD_NDS32_BUILTIN2 ("scmplt16", unsigned, unsigned, unsigned, SCMPLT16);
  ADD_NDS32_BUILTIN2 ("v_scmplt16", u_v2hi, v2hi, v2hi, V_SCMPLT16);
  ADD_NDS32_BUILTIN2 ("scmple16", unsigned, unsigned, unsigned, SCMPLE16);
  ADD_NDS32_BUILTIN2 ("v_scmple16", u_v2hi, v2hi, v2hi, V_SCMPLE16);
  ADD_NDS32_BUILTIN2 ("ucmplt16", unsigned, unsigned, unsigned, UCMPLT16);
  ADD_NDS32_BUILTIN2 ("v_ucmplt16", u_v2hi, u_v2hi, u_v2hi, V_UCMPLT16);
  ADD_NDS32_BUILTIN2 ("ucmple16", unsigned, unsigned, unsigned, UCMPLE16);
  ADD_NDS32_BUILTIN2 ("v_ucmple16", u_v2hi, u_v2hi, u_v2hi, V_UCMPLE16);

  /* DSP Extension: 8bit Compare.  */
  ADD_NDS32_BUILTIN2 ("cmpeq8", unsigned, unsigned, unsigned, CMPEQ8);
  ADD_NDS32_BUILTIN2 ("v_scmpeq8", u_v4qi, v4qi, v4qi, V_SCMPEQ8);
  ADD_NDS32_BUILTIN2 ("v_ucmpeq8", u_v4qi, u_v4qi, u_v4qi, V_UCMPEQ8);
  ADD_NDS32_BUILTIN2 ("scmplt8", unsigned, unsigned, unsigned, SCMPLT8);
  ADD_NDS32_BUILTIN2 ("v_scmplt8", u_v4qi, v4qi, v4qi, V_SCMPLT8);
  ADD_NDS32_BUILTIN2 ("scmple8", unsigned, unsigned, unsigned, SCMPLE8);
  ADD_NDS32_BUILTIN2 ("v_scmple8", u_v4qi, v4qi, v4qi, V_SCMPLE8);
  ADD_NDS32_BUILTIN2 ("ucmplt8", unsigned, unsigned, unsigned, UCMPLT8);
  ADD_NDS32_BUILTIN2 ("v_ucmplt8", u_v4qi, u_v4qi, u_v4qi, V_UCMPLT8);
  ADD_NDS32_BUILTIN2 ("ucmple8", unsigned, unsigned, unsigned, UCMPLE8);
  ADD_NDS32_BUILTIN2 ("v_ucmple8", u_v4qi, u_v4qi, u_v4qi, V_UCMPLE8);

  /* DSP Extension: SIMD 16bit MISC.  */
  ADD_NDS32_BUILTIN2 ("smin16", unsigned, unsigned, unsigned, SMIN16);
  ADD_NDS32_BUILTIN2 ("v_smin16", v2hi, v2hi, v2hi, V_SMIN16);
  ADD_NDS32_BUILTIN2 ("umin16", unsigned, unsigned, unsigned, UMIN16);
  ADD_NDS32_BUILTIN2 ("v_umin16", u_v2hi, u_v2hi, u_v2hi, V_UMIN16);
  ADD_NDS32_BUILTIN2 ("smax16", unsigned, unsigned, unsigned, SMAX16);
  ADD_NDS32_BUILTIN2 ("v_smax16", v2hi, v2hi, v2hi, V_SMAX16);
  ADD_NDS32_BUILTIN2 ("umax16", unsigned, unsigned, unsigned, UMAX16);
  ADD_NDS32_BUILTIN2 ("v_umax16", u_v2hi, u_v2hi, u_v2hi, V_UMAX16);
  ADD_NDS32_BUILTIN2 ("sclip16", unsigned, unsigned, unsigned, SCLIP16);
  ADD_NDS32_BUILTIN2 ("v_sclip16", v2hi, v2hi, unsigned, V_SCLIP16);
  ADD_NDS32_BUILTIN2 ("uclip16", unsigned, unsigned, unsigned, UCLIP16);
  ADD_NDS32_BUILTIN2 ("v_uclip16", v2hi, v2hi, unsigned, V_UCLIP16);
  ADD_NDS32_BUILTIN2 ("khm16", unsigned, unsigned, unsigned, KHM16);
  ADD_NDS32_BUILTIN2 ("v_khm16", v2hi, v2hi, v2hi, V_KHM16);
  ADD_NDS32_BUILTIN2 ("khmx16", unsigned, unsigned, unsigned, KHMX16);
  ADD_NDS32_BUILTIN2 ("v_khmx16", v2hi, v2hi, v2hi, V_KHMX16);
  ADD_NDS32_BUILTIN1 ("kabs16", unsigned, unsigned, KABS16);
  ADD_NDS32_BUILTIN1 ("v_kabs16", v2hi, v2hi, V_KABS16);
  ADD_NDS32_BUILTIN2 ("smul16", long_long_unsigned, unsigned, unsigned, SMUL16);
  ADD_NDS32_BUILTIN2 ("v_smul16", v2si, v2hi, v2hi, V_SMUL16);
  ADD_NDS32_BUILTIN2 ("smulx16",
		      long_long_unsigned, unsigned, unsigned, SMULX16);
  ADD_NDS32_BUILTIN2 ("v_smulx16", v2si, v2hi, v2hi, V_SMULX16);
  ADD_NDS32_BUILTIN2 ("umul16", long_long_unsigned, unsigned, unsigned, UMUL16);
  ADD_NDS32_BUILTIN2 ("v_umul16", u_v2si, u_v2hi, u_v2hi, V_UMUL16);
  ADD_NDS32_BUILTIN2 ("umulx16",
		      long_long_unsigned, unsigned, unsigned, UMULX16);
  ADD_NDS32_BUILTIN2 ("v_umulx16", u_v2si, u_v2hi, u_v2hi, V_UMULX16);

  /* DSP Extension: SIMD 8bit MISC.  */
  ADD_NDS32_BUILTIN2 ("smin8", unsigned, unsigned, unsigned, SMIN8);
  ADD_NDS32_BUILTIN2 ("v_smin8", v4qi, v4qi, v4qi, V_SMIN8);
  ADD_NDS32_BUILTIN2 ("umin8", unsigned, unsigned, unsigned, UMIN8);
  ADD_NDS32_BUILTIN2 ("v_umin8", u_v4qi, u_v4qi, u_v4qi, V_UMIN8);
  ADD_NDS32_BUILTIN2 ("smax8", unsigned, unsigned, unsigned, SMAX8);
  ADD_NDS32_BUILTIN2 ("v_smax8", v4qi, v4qi, v4qi, V_SMAX8);
  ADD_NDS32_BUILTIN2 ("umax8", unsigned, unsigned, unsigned, UMAX8);
  ADD_NDS32_BUILTIN2 ("v_umax8", u_v4qi, u_v4qi, u_v4qi, V_UMAX8);
  ADD_NDS32_BUILTIN1 ("kabs8", unsigned, unsigned, KABS8);
  ADD_NDS32_BUILTIN1 ("v_kabs8", v4qi, v4qi, V_KABS8);

  /* DSP Extension: 8bit Unpacking.  */
  ADD_NDS32_BUILTIN1 ("sunpkd810", unsigned, unsigned, SUNPKD810);
  ADD_NDS32_BUILTIN1 ("v_sunpkd810", v2hi, v4qi, V_SUNPKD810);
  ADD_NDS32_BUILTIN1 ("sunpkd820", unsigned, unsigned, SUNPKD820);
  ADD_NDS32_BUILTIN1 ("v_sunpkd820", v2hi, v4qi, V_SUNPKD820);
  ADD_NDS32_BUILTIN1 ("sunpkd830", unsigned, unsigned, SUNPKD830);
  ADD_NDS32_BUILTIN1 ("v_sunpkd830", v2hi, v4qi, V_SUNPKD830);
  ADD_NDS32_BUILTIN1 ("sunpkd831", unsigned, unsigned, SUNPKD831);
  ADD_NDS32_BUILTIN1 ("v_sunpkd831", v2hi, v4qi, V_SUNPKD831);
  ADD_NDS32_BUILTIN1 ("zunpkd810", unsigned, unsigned, ZUNPKD810);
  ADD_NDS32_BUILTIN1 ("v_zunpkd810", u_v2hi, u_v4qi, V_ZUNPKD810);
  ADD_NDS32_BUILTIN1 ("zunpkd820", unsigned, unsigned, ZUNPKD820);
  ADD_NDS32_BUILTIN1 ("v_zunpkd820", u_v2hi, u_v4qi, V_ZUNPKD820);
  ADD_NDS32_BUILTIN1 ("zunpkd830", unsigned, unsigned, ZUNPKD830);
  ADD_NDS32_BUILTIN1 ("v_zunpkd830", u_v2hi, u_v4qi, V_ZUNPKD830);
  ADD_NDS32_BUILTIN1 ("zunpkd831", unsigned, unsigned, ZUNPKD831);
  ADD_NDS32_BUILTIN1 ("v_zunpkd831", u_v2hi, u_v4qi, V_ZUNPKD831);

  /* DSP Extension: 32bit Add and Subtract.  */
  ADD_NDS32_BUILTIN2 ("raddw", integer, integer, integer, RADDW);
  ADD_NDS32_BUILTIN2 ("uraddw", unsigned, unsigned, unsigned, URADDW);
  ADD_NDS32_BUILTIN2 ("rsubw", integer, integer, integer, RSUBW);
  ADD_NDS32_BUILTIN2 ("ursubw", unsigned, unsigned, unsigned, URSUBW);

  /* DSP Extension: 32bit Shift.  */
  ADD_NDS32_BUILTIN2 ("sra_u", integer, integer, unsigned, SRA_U);
  ADD_NDS32_BUILTIN2 ("ksll", integer, integer, unsigned, KSLL);

  /* DSP Extension: 16bit Packing.  */
  ADD_NDS32_BUILTIN2 ("pkbb16", unsigned, unsigned, unsigned, PKBB16);
  ADD_NDS32_BUILTIN2 ("v_pkbb16", u_v2hi, u_v2hi, u_v2hi, V_PKBB16);
  ADD_NDS32_BUILTIN2 ("pkbt16", unsigned, unsigned, unsigned, PKBT16);
  ADD_NDS32_BUILTIN2 ("v_pkbt16", u_v2hi, u_v2hi, u_v2hi, V_PKBT16);
  ADD_NDS32_BUILTIN2 ("pktb16", unsigned, unsigned, unsigned, PKTB16);
  ADD_NDS32_BUILTIN2 ("v_pktb16", u_v2hi, u_v2hi, u_v2hi, V_PKTB16);
  ADD_NDS32_BUILTIN2 ("pktt16", unsigned, unsigned, unsigned, PKTT16);
  ADD_NDS32_BUILTIN2 ("v_pktt16", u_v2hi, u_v2hi, u_v2hi, V_PKTT16);

  /* DSP Extension: Signed MSW 32x32 Multiply and ADD.  */
  ADD_NDS32_BUILTIN2 ("smmul", integer, integer, integer, SMMUL);
  ADD_NDS32_BUILTIN2 ("smmul_u", integer, integer, integer, SMMUL_U);
  ADD_NDS32_BUILTIN3 ("kmmac", integer, integer, integer, integer, KMMAC);
  ADD_NDS32_BUILTIN3 ("kmmac_u", integer, integer, integer, integer, KMMAC_U);
  ADD_NDS32_BUILTIN3 ("kmmsb", integer, integer, integer, integer, KMMSB);
  ADD_NDS32_BUILTIN3 ("kmmsb_u", integer, integer, integer, integer, KMMSB_U);
  ADD_NDS32_BUILTIN2 ("kwmmul", integer, integer, integer, KWMMUL);
  ADD_NDS32_BUILTIN2 ("kwmmul_u", integer, integer, integer, KWMMUL_U);

  /* DSP Extension: Most Significant Word 32x16 Multiply and ADD.  */
  ADD_NDS32_BUILTIN2 ("smmwb", integer, integer, unsigned, SMMWB);
  ADD_NDS32_BUILTIN2 ("v_smmwb", integer, integer, v2hi, V_SMMWB);
  ADD_NDS32_BUILTIN2 ("smmwb_u", integer, integer, unsigned, SMMWB_U);
  ADD_NDS32_BUILTIN2 ("v_smmwb_u", integer, integer, v2hi, V_SMMWB_U);
  ADD_NDS32_BUILTIN2 ("smmwt", integer, integer, unsigned, SMMWT);
  ADD_NDS32_BUILTIN2 ("v_smmwt", integer, integer, v2hi, V_SMMWT);
  ADD_NDS32_BUILTIN2 ("smmwt_u", integer, integer, unsigned, SMMWT_U);
  ADD_NDS32_BUILTIN2 ("v_smmwt_u", integer, integer, v2hi, V_SMMWT_U);
  ADD_NDS32_BUILTIN3 ("kmmawb", integer, integer, integer, unsigned, KMMAWB);
  ADD_NDS32_BUILTIN3 ("v_kmmawb", integer, integer, integer, v2hi, V_KMMAWB);
  ADD_NDS32_BUILTIN3 ("kmmawb_u",
		      integer, integer, integer, unsigned, KMMAWB_U);
  ADD_NDS32_BUILTIN3 ("v_kmmawb_u",
		      integer, integer, integer, v2hi, V_KMMAWB_U);
  ADD_NDS32_BUILTIN3 ("kmmawt", integer, integer, integer, unsigned, KMMAWT);
  ADD_NDS32_BUILTIN3 ("v_kmmawt", integer, integer, integer, v2hi, V_KMMAWT);
  ADD_NDS32_BUILTIN3 ("kmmawt_u",
		      integer, integer, integer, unsigned, KMMAWT_U);
  ADD_NDS32_BUILTIN3 ("v_kmmawt_u",
		      integer, integer, integer, v2hi, V_KMMAWT_U);

  /* DSP Extension: Signed 16bit Multiply with ADD/Subtract.  */
  ADD_NDS32_BUILTIN2 ("smbb", integer, unsigned, unsigned, SMBB);
  ADD_NDS32_BUILTIN2 ("v_smbb", integer, v2hi, v2hi, V_SMBB);
  ADD_NDS32_BUILTIN2 ("smbt", integer, unsigned, unsigned, SMBT);
  ADD_NDS32_BUILTIN2 ("v_smbt", integer, v2hi, v2hi, V_SMBT);
  ADD_NDS32_BUILTIN2 ("smtt", integer, unsigned, unsigned, SMTT);
  ADD_NDS32_BUILTIN2 ("v_smtt", integer, v2hi, v2hi, V_SMTT);
  ADD_NDS32_BUILTIN2 ("kmda", integer, unsigned, unsigned, KMDA);
  ADD_NDS32_BUILTIN2 ("v_kmda", integer, v2hi, v2hi, V_KMDA);
  ADD_NDS32_BUILTIN2 ("kmxda", integer, unsigned, unsigned, KMXDA);
  ADD_NDS32_BUILTIN2 ("v_kmxda", integer, v2hi, v2hi, V_KMXDA);
  ADD_NDS32_BUILTIN2 ("smds", integer, unsigned, unsigned, SMDS);
  ADD_NDS32_BUILTIN2 ("v_smds", integer, v2hi, v2hi, V_SMDS);
  ADD_NDS32_BUILTIN2 ("smdrs", integer, unsigned, unsigned, SMDRS);
  ADD_NDS32_BUILTIN2 ("v_smdrs", integer, v2hi, v2hi, V_SMDRS);
  ADD_NDS32_BUILTIN2 ("smxds", integer, unsigned, unsigned, SMXDS);
  ADD_NDS32_BUILTIN2 ("v_smxds", integer, v2hi, v2hi, V_SMXDS);
  ADD_NDS32_BUILTIN3 ("kmabb", integer, integer, unsigned, unsigned, KMABB);
  ADD_NDS32_BUILTIN3 ("v_kmabb", integer, integer, v2hi, v2hi, V_KMABB);
  ADD_NDS32_BUILTIN3 ("kmabt", integer, integer, unsigned, unsigned, KMABT);
  ADD_NDS32_BUILTIN3 ("v_kmabt", integer, integer, v2hi, v2hi, V_KMABT);
  ADD_NDS32_BUILTIN3 ("kmatt", integer, integer, unsigned, unsigned, KMATT);
  ADD_NDS32_BUILTIN3 ("v_kmatt", integer, integer, v2hi, v2hi, V_KMATT);
  ADD_NDS32_BUILTIN3 ("kmada", integer, integer, unsigned, unsigned, KMADA);
  ADD_NDS32_BUILTIN3 ("v_kmada", integer, integer, v2hi, v2hi, V_KMADA);
  ADD_NDS32_BUILTIN3 ("kmaxda", integer, integer, unsigned, unsigned, KMAXDA);
  ADD_NDS32_BUILTIN3 ("v_kmaxda", integer, integer, v2hi, v2hi, V_KMAXDA);
  ADD_NDS32_BUILTIN3 ("kmads", integer, integer, unsigned, unsigned, KMADS);
  ADD_NDS32_BUILTIN3 ("v_kmads", integer, integer, v2hi, v2hi, V_KMADS);
  ADD_NDS32_BUILTIN3 ("kmadrs", integer, integer, unsigned, unsigned, KMADRS);
  ADD_NDS32_BUILTIN3 ("v_kmadrs", integer, integer, v2hi, v2hi, V_KMADRS);
  ADD_NDS32_BUILTIN3 ("kmaxds", integer, integer, unsigned, unsigned, KMAXDS);
  ADD_NDS32_BUILTIN3 ("v_kmaxds", integer, integer, v2hi, v2hi, V_KMAXDS);
  ADD_NDS32_BUILTIN3 ("kmsda", integer, integer, unsigned, unsigned, KMSDA);
  ADD_NDS32_BUILTIN3 ("v_kmsda", integer, integer, v2hi, v2hi, V_KMSDA);
  ADD_NDS32_BUILTIN3 ("kmsxda", integer, integer, unsigned, unsigned, KMSXDA);
  ADD_NDS32_BUILTIN3 ("v_kmsxda", integer, integer, v2hi, v2hi, V_KMSXDA);

  /* DSP Extension: Signed 16bit Multiply with 64bit ADD/Subtract.  */
  ADD_NDS32_BUILTIN2 ("smal", long_long_integer,
		      long_long_integer, unsigned, SMAL);
  ADD_NDS32_BUILTIN2 ("v_smal", long_long_integer,
		      long_long_integer, v2hi, V_SMAL);

  /* DSP Extension: 32bit MISC.  */
  ADD_NDS32_BUILTIN2 ("bitrev", unsigned, unsigned, unsigned, BITREV);
  ADD_NDS32_BUILTIN2 ("wext", unsigned, long_long_integer, unsigned, WEXT);
  ADD_NDS32_BUILTIN3 ("bpick", unsigned, unsigned, unsigned, unsigned, BPICK);
  ADD_NDS32_BUILTIN3 ("insb", unsigned, unsigned, unsigned, unsigned, INSB);

  /* DSP Extension: 64bit Add and Subtract.  */
  ADD_NDS32_BUILTIN2 ("sadd64", long_long_integer,
		      long_long_integer, long_long_integer, SADD64);
  ADD_NDS32_BUILTIN2 ("uadd64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, UADD64);
  ADD_NDS32_BUILTIN2 ("radd64", long_long_integer,
		      long_long_integer, long_long_integer, RADD64);
  ADD_NDS32_BUILTIN2 ("uradd64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, URADD64);
  ADD_NDS32_BUILTIN2 ("kadd64", long_long_integer,
		      long_long_integer, long_long_integer, KADD64);
  ADD_NDS32_BUILTIN2 ("ukadd64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, UKADD64);
  ADD_NDS32_BUILTIN2 ("ssub64", long_long_integer,
		      long_long_integer, long_long_integer, SSUB64);
  ADD_NDS32_BUILTIN2 ("usub64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, USUB64);
  ADD_NDS32_BUILTIN2 ("rsub64", long_long_integer,
		      long_long_integer, long_long_integer, RSUB64);
  ADD_NDS32_BUILTIN2 ("ursub64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, URSUB64);
  ADD_NDS32_BUILTIN2 ("ksub64", long_long_integer,
		      long_long_integer, long_long_integer, KSUB64);
  ADD_NDS32_BUILTIN2 ("uksub64", long_long_unsigned,
		      long_long_unsigned, long_long_unsigned, UKSUB64);

  /* DSP Extension: 32bit Multiply with 64bit Add/Subtract.  */
  ADD_NDS32_BUILTIN3 ("smar64", long_long_integer,
		      long_long_integer, integer, integer, SMAR64);
  ADD_NDS32_BUILTIN3 ("smsr64", long_long_integer,
		      long_long_integer, integer, integer, SMSR64);
  ADD_NDS32_BUILTIN3 ("umar64", long_long_unsigned,
		      long_long_unsigned, unsigned, unsigned, UMAR64);
  ADD_NDS32_BUILTIN3 ("umsr64", long_long_unsigned,
		      long_long_unsigned, unsigned, unsigned, UMSR64);
  ADD_NDS32_BUILTIN3 ("kmar64", long_long_integer,
		      long_long_integer, integer, integer, KMAR64);
  ADD_NDS32_BUILTIN3 ("kmsr64", long_long_integer,
		      long_long_integer, integer, integer, KMSR64);
  ADD_NDS32_BUILTIN3 ("ukmar64", long_long_unsigned,
		      long_long_unsigned, unsigned, unsigned, UKMAR64);
  ADD_NDS32_BUILTIN3 ("ukmsr64", long_long_unsigned,
		      long_long_unsigned, unsigned, unsigned, UKMSR64);

  /* DSP Extension: Signed 16bit Multiply with 64bit Add/Subtract.  */
  ADD_NDS32_BUILTIN3 ("smalbb", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALBB);
  ADD_NDS32_BUILTIN3 ("v_smalbb", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALBB);
  ADD_NDS32_BUILTIN3 ("smalbt", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALBT);
  ADD_NDS32_BUILTIN3 ("v_smalbt", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALBT);
  ADD_NDS32_BUILTIN3 ("smaltt", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALTT);
  ADD_NDS32_BUILTIN3 ("v_smaltt", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALTT);
  ADD_NDS32_BUILTIN3 ("smalda", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALDA);
  ADD_NDS32_BUILTIN3 ("v_smalda", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALDA);
  ADD_NDS32_BUILTIN3 ("smalxda", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALXDA);
  ADD_NDS32_BUILTIN3 ("v_smalxda", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALXDA);
  ADD_NDS32_BUILTIN3 ("smalds", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALDS);
  ADD_NDS32_BUILTIN3 ("v_smalds", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALDS);
  ADD_NDS32_BUILTIN3 ("smaldrs", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALDRS);
  ADD_NDS32_BUILTIN3 ("v_smaldrs", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALDRS);
  ADD_NDS32_BUILTIN3 ("smalxds", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMALXDS);
  ADD_NDS32_BUILTIN3 ("v_smalxds", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMALXDS);
  ADD_NDS32_BUILTIN3 ("smslda", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMSLDA);
  ADD_NDS32_BUILTIN3 ("v_smslda", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMSLDA);
  ADD_NDS32_BUILTIN3 ("smslxda", long_long_integer,
		      long_long_integer, unsigned, unsigned, SMSLXDA);
  ADD_NDS32_BUILTIN3 ("v_smslxda", long_long_integer,
		      long_long_integer, v2hi, v2hi, V_SMSLXDA);

  /* DSP Extension: augmented baseline.  */
  ADD_NDS32_BUILTIN2 ("uclip32", unsigned, integer, unsigned, UCLIP32);
  ADD_NDS32_BUILTIN2 ("sclip32", integer, integer, unsigned, SCLIP32);
  ADD_NDS32_BUILTIN1 ("kabs", integer, integer, KABS);

  /* DSP Extension: vector type unaligned Load/Store  */
  ADD_NDS32_BUILTIN1 ("get_unaligned_u16x2", u_v2hi, ptr_ushort, UALOAD_U16);
  ADD_NDS32_BUILTIN1 ("get_unaligned_s16x2", v2hi, ptr_short, UALOAD_S16);
  ADD_NDS32_BUILTIN1 ("get_unaligned_u8x4", u_v4qi, ptr_uchar, UALOAD_U8);
  ADD_NDS32_BUILTIN1 ("get_unaligned_s8x4", v4qi, ptr_char, UALOAD_S8);
  ADD_NDS32_BUILTIN2 ("put_unaligned_u16x2", void, ptr_ushort,
		      u_v2hi, UASTORE_U16);
  ADD_NDS32_BUILTIN2 ("put_unaligned_s16x2", void, ptr_short,
		      v2hi, UASTORE_S16);
  ADD_NDS32_BUILTIN2 ("put_unaligned_u8x4", void, ptr_uchar,
		      u_v4qi, UASTORE_U8);
  ADD_NDS32_BUILTIN2 ("put_unaligned_s8x4", void, ptr_char,
		      v4qi, UASTORE_S8);
}
