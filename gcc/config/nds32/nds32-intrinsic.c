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

/* Intrinsics that take just one argument.  */
static struct builtin_description bdesc_1arg[] =
{
  NDS32_BUILTIN(unaligned_load_hw, "unaligned_load_hw", UALOAD_HW)
  NDS32_BUILTIN(unaligned_loadsi, "unaligned_load_w", UALOAD_W)
  NDS32_BUILTIN(unaligned_loaddi, "unaligned_load_dw", UALOAD_DW)
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_isync, "isync", ISYNC)
};

/* Intrinsics that take just one argument. and the argument is immediate.  */
static struct builtin_description bdesc_1argimm[] =
{
  NDS32_BUILTIN(unspec_volatile_mfsr, "mfsr", MFSR)
  NDS32_BUILTIN(unspec_volatile_mfusr, "mfsr", MFUSR)
};

/* Intrinsics that take two arguments.  */
static struct builtin_description bdesc_2arg[] =
{
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_mtsr, "mtsr", MTSR)
  NDS32_NO_TARGET_BUILTIN(unspec_volatile_mtusr, "mtusr", MTUSR)
  NDS32_NO_TARGET_BUILTIN(unaligned_store_hw, "unaligned_store_hw", UASTORE_HW)
  NDS32_NO_TARGET_BUILTIN(unaligned_storesi, "unaligned_store_hw", UASTORE_W)
  NDS32_NO_TARGET_BUILTIN(unaligned_storedi, "unaligned_store_hw", UASTORE_DW)

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

  /* Since there are no result and operands, we can simply emit this rtx.  */
  switch (fcode)
    {
    case NDS32_BUILTIN_ISB:
      emit_insn (gen_unspec_volatile_isb ());
      return target;
    case NDS32_BUILTIN_SETGIE_EN:
      emit_insn (gen_unspec_volatile_setgie_en ());
      return target;
    case NDS32_BUILTIN_SETGIE_DIS:
      emit_insn (gen_unspec_volatile_setgie_dis ());
      return target;
    default:
      break;
    }

  /* Expand groups of builtins.  */
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
  tree ptr_ushort_type_node = build_pointer_type (short_unsigned_type_node);
  tree ptr_uint_type_node = build_pointer_type (unsigned_type_node);
  tree ptr_ulong_type_node = build_pointer_type (long_long_unsigned_type_node);

  /* Cache.  */
  ADD_NDS32_BUILTIN1 ("isync", void, ptr_uint, ISYNC);
  ADD_NDS32_BUILTIN0 ("isb", void, ISB);

  /* Register Transfer.  */
  ADD_NDS32_BUILTIN1 ("mfsr", unsigned, integer, MFSR);
  ADD_NDS32_BUILTIN1 ("mfusr", unsigned, integer, MFUSR);
  ADD_NDS32_BUILTIN2 ("mtsr", void, unsigned, integer, MTSR);
  ADD_NDS32_BUILTIN2 ("mtusr", void, unsigned, integer, MTUSR);

  /* Interrupt.  */
  ADD_NDS32_BUILTIN0 ("setgie_en", void, SETGIE_EN);
  ADD_NDS32_BUILTIN0 ("setgie_dis", void, SETGIE_DIS);

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

}
