/* Intrinsic functions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "expr.h"
#include "langhooks.h"		/* For add_builtin_function().  */

/* ------------------------------------------------------------------------ */

/* Function to expand builtin function for
   '[(unspec_volatile [(reg)])]'.  */
static rtx
nds32_expand_builtin_null_ftype_reg (enum insn_code icode,
				     tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> value0 <--> arg0 */
  struct expand_operand ops[1];
  tree arg0;
  rtx value0;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  value0 = expand_normal (arg0);

  /* Create operands.  */
  create_input_operand (&ops[0], value0, TYPE_MODE (TREE_TYPE (arg0)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 1, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* Function to expand builtin function for
   '[(set (reg) (unspec_volatile [(imm)]))]'.  */
static rtx
nds32_expand_builtin_reg_ftype_imm (enum insn_code icode,
				    tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> target <--> exp
       ops[1] <--> value0 <--> arg0 */
  struct expand_operand ops[2];
  tree arg0;
  rtx value0;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  value0 = expand_normal (arg0);

  /* Create operands.  */
  create_output_operand (&ops[0], target, TYPE_MODE (TREE_TYPE (exp)));
  create_input_operand (&ops[1], value0, TYPE_MODE (TREE_TYPE (arg0)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 2, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* Function to expand builtin function for
   '[(unspec_volatile [(reg) (imm)])]' pattern.  */
static rtx
nds32_expand_builtin_null_ftype_reg_imm (enum insn_code icode,
					 tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> value0 <--> arg0
       ops[1] <--> value1 <--> arg1 */
  struct expand_operand ops[2];
  tree arg0, arg1;
  rtx value0, value1;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  value0 = expand_normal (arg0);
  value1 = expand_normal (arg1);

  /* Create operands.  */
  create_input_operand (&ops[0], value0, TYPE_MODE (TREE_TYPE (arg0)));
  create_input_operand (&ops[1], value1, TYPE_MODE (TREE_TYPE (arg1)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 2, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* ------------------------------------------------------------------------ */

void
nds32_init_builtins_impl (void)
{
  tree pointer_type_node  = build_pointer_type (integer_type_node);

  tree void_ftype_void    = build_function_type (void_type_node,
						 void_list_node);

  tree void_ftype_pint    = build_function_type_list (void_type_node,
						      pointer_type_node,
						      NULL_TREE);

  tree int_ftype_int      = build_function_type_list (integer_type_node,
						      integer_type_node,
						      NULL_TREE);

  tree void_ftype_int_int = build_function_type_list (void_type_node,
						      integer_type_node,
						      integer_type_node,
						      NULL_TREE);

  /* Cache.  */
  add_builtin_function ("__builtin_nds32_isync",  void_ftype_pint,
			NDS32_BUILTIN_ISYNC,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_isb",  void_ftype_void,
			NDS32_BUILTIN_ISB,
			BUILT_IN_MD, NULL, NULL_TREE);

  /* Register Transfer.  */
  add_builtin_function ("__builtin_nds32_mfsr",  int_ftype_int,
			NDS32_BUILTIN_MFSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mfusr", int_ftype_int,
			NDS32_BUILTIN_MFUSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mtsr",  void_ftype_int_int,
			NDS32_BUILTIN_MTSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mtusr", void_ftype_int_int,
			NDS32_BUILTIN_MTUSR,
			BUILT_IN_MD, NULL, NULL_TREE);

  /* Interrupt.  */
  add_builtin_function ("__builtin_nds32_setgie_en",  void_ftype_void,
			NDS32_BUILTIN_SETGIE_EN,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_setgie_dis", void_ftype_void,
			NDS32_BUILTIN_SETGIE_DIS,
			BUILT_IN_MD, NULL, NULL_TREE);
}


rtx
nds32_expand_builtin_impl (tree exp,
			   rtx target,
			   rtx subtarget ATTRIBUTE_UNUSED,
			   machine_mode mode ATTRIBUTE_UNUSED,
			   int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);

  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    /* Cache.  */
    case NDS32_BUILTIN_ISYNC:
      return nds32_expand_builtin_null_ftype_reg
	     (CODE_FOR_unspec_volatile_isync, exp, target);
    case NDS32_BUILTIN_ISB:
      /* Since there are no result and operands for isb instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_isb ());
      return target;

    /* Register Transfer.  */
    case NDS32_BUILTIN_MFSR:
      return nds32_expand_builtin_reg_ftype_imm
	     (CODE_FOR_unspec_volatile_mfsr, exp, target);
    case NDS32_BUILTIN_MFUSR:
      return nds32_expand_builtin_reg_ftype_imm
	     (CODE_FOR_unspec_volatile_mfusr, exp, target);
    case NDS32_BUILTIN_MTSR:
      return nds32_expand_builtin_null_ftype_reg_imm
	     (CODE_FOR_unspec_volatile_mtsr, exp, target);
    case NDS32_BUILTIN_MTUSR:
      return nds32_expand_builtin_null_ftype_reg_imm
	     (CODE_FOR_unspec_volatile_mtusr, exp, target);

    /* Interrupt.  */
    case NDS32_BUILTIN_SETGIE_EN:
      /* Since there are no result and operands for setgie.e instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_setgie_en ());
      return target;
    case NDS32_BUILTIN_SETGIE_DIS:
      /* Since there are no result and operands for setgie.d instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_setgie_dis ());
      return target;

    default:
      gcc_unreachable ();
    }

  return NULL_RTX;
}

/* ------------------------------------------------------------------------ */
