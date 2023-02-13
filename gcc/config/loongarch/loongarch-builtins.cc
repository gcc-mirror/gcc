/* Subroutines used for expanding LoongArch builtins.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "expr.h"
#include "langhooks.h"

/* Macros to create an enumeration identifier for a function prototype.  */
#define LARCH_FTYPE_NAME1(A, B) LARCH_##A##_FTYPE_##B
#define LARCH_FTYPE_NAME2(A, B, C) LARCH_##A##_FTYPE_##B##_##C
#define LARCH_FTYPE_NAME3(A, B, C, D) LARCH_##A##_FTYPE_##B##_##C##_##D

/* Classifies the prototype of a built-in function.  */
enum loongarch_function_type
{
#define DEF_LARCH_FTYPE(NARGS, LIST) LARCH_FTYPE_NAME##NARGS LIST,
#include "config/loongarch/loongarch-ftypes.def"
#undef DEF_LARCH_FTYPE
  LARCH_MAX_FTYPE_MAX
};

/* Specifies how a built-in function should be converted into rtl.  */
enum loongarch_builtin_type
{
  /* The function corresponds directly to an .md pattern.  The return
     value is mapped to operand 0 and the arguments are mapped to
     operands 1 and above.  */
  LARCH_BUILTIN_DIRECT,

  /* The function corresponds directly to an .md pattern.  There is no return
     value and the arguments are mapped to operands 0 and above.  */
  LARCH_BUILTIN_DIRECT_NO_TARGET,

};

/* Declare an availability predicate for built-in functions that require
 * COND to be true.  NAME is the main part of the predicate's name.  */
#define AVAIL_ALL(NAME, COND) \
  static unsigned int \
  loongarch_builtin_avail_##NAME (void) \
  { \
    return (COND) ? 1 : 0; \
  }

static unsigned int
loongarch_builtin_avail_default (void)
{
  return 1;
}
/* This structure describes a single built-in function.  */
struct loongarch_builtin_description
{
  /* The code of the main .md file instruction.  See loongarch_builtin_type
     for more information.  */
  enum insn_code icode;

  /* The name of the built-in function.  */
  const char *name;

  /* Specifies how the function should be expanded.  */
  enum loongarch_builtin_type builtin_type;

  /* The function's prototype.  */
  enum loongarch_function_type function_type;

  /* Whether the function is available.  */
  unsigned int (*avail) (void);
};

AVAIL_ALL (hard_float, TARGET_HARD_FLOAT_ABI)

/* Construct a loongarch_builtin_description from the given arguments.

   INSN is the name of the associated instruction pattern, without the
   leading CODE_FOR_loongarch_.

   CODE is the floating-point condition code associated with the
   function.  It can be 'f' if the field is not applicable.

   NAME is the name of the function itself, without the leading
   "__builtin_loongarch_".

   BUILTIN_TYPE and FUNCTION_TYPE are loongarch_builtin_description fields.

   AVAIL is the name of the availability predicate, without the leading
   loongarch_builtin_avail_.  */
#define LARCH_BUILTIN(INSN, NAME, BUILTIN_TYPE, FUNCTION_TYPE, AVAIL) \
  { \
    CODE_FOR_loongarch_##INSN, "__builtin_loongarch_" NAME, \
      BUILTIN_TYPE, FUNCTION_TYPE, \
      loongarch_builtin_avail_##AVAIL \
  }

/* Define __builtin_loongarch_<INSN>, which is a LARCH_BUILTIN_DIRECT function
   mapped to instruction CODE_FOR_loongarch_<INSN>,  FUNCTION_TYPE and AVAIL
   are as for LARCH_BUILTIN.  */
#define DIRECT_BUILTIN(INSN, FUNCTION_TYPE, AVAIL) \
  LARCH_BUILTIN (INSN, #INSN, LARCH_BUILTIN_DIRECT, FUNCTION_TYPE, AVAIL)

/* Define __builtin_loongarch_<INSN>, which is a LARCH_BUILTIN_DIRECT_NO_TARGET
   function mapped to instruction CODE_FOR_loongarch_<INSN>,  FUNCTION_TYPE
   and AVAIL are as for LARCH_BUILTIN.  */
#define DIRECT_NO_TARGET_BUILTIN(INSN, FUNCTION_TYPE, AVAIL) \
  LARCH_BUILTIN (INSN, #INSN, LARCH_BUILTIN_DIRECT_NO_TARGET, \
		 FUNCTION_TYPE, AVAIL)

static const struct loongarch_builtin_description loongarch_builtins[] = {
#define LARCH_MOVFCSR2GR 0
  DIRECT_BUILTIN (movfcsr2gr, LARCH_USI_FTYPE_UQI, hard_float),
#define LARCH_MOVGR2FCSR 1
  DIRECT_NO_TARGET_BUILTIN (movgr2fcsr, LARCH_VOID_FTYPE_UQI_USI, hard_float),

  DIRECT_NO_TARGET_BUILTIN (cacop_w, LARCH_VOID_FTYPE_USI_USI_SI, default),
  DIRECT_NO_TARGET_BUILTIN (cacop_d, LARCH_VOID_FTYPE_USI_UDI_SI, default),
  DIRECT_NO_TARGET_BUILTIN (dbar, LARCH_VOID_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (ibar, LARCH_VOID_FTYPE_USI, default),

  DIRECT_BUILTIN (lddir_d, LARCH_DI_FTYPE_DI_UQI, default),
  DIRECT_BUILTIN (lddir_w, LARCH_SI_FTYPE_SI_UQI, default),
  DIRECT_NO_TARGET_BUILTIN (ldpte_d, LARCH_VOID_FTYPE_DI_UQI, default),
  DIRECT_NO_TARGET_BUILTIN (ldpte_w, LARCH_VOID_FTYPE_SI_UQI, default),

  /* CRC Instrinsic */

  DIRECT_BUILTIN (crc_w_b_w, LARCH_SI_FTYPE_QI_SI, default),
  DIRECT_BUILTIN (crc_w_h_w, LARCH_SI_FTYPE_HI_SI, default),
  DIRECT_BUILTIN (crc_w_w_w, LARCH_SI_FTYPE_SI_SI, default),
  DIRECT_BUILTIN (crc_w_d_w, LARCH_SI_FTYPE_DI_SI, default),
  DIRECT_BUILTIN (crcc_w_b_w, LARCH_SI_FTYPE_QI_SI, default),
  DIRECT_BUILTIN (crcc_w_h_w, LARCH_SI_FTYPE_HI_SI, default),
  DIRECT_BUILTIN (crcc_w_w_w, LARCH_SI_FTYPE_SI_SI, default),
  DIRECT_BUILTIN (crcc_w_d_w, LARCH_SI_FTYPE_DI_SI, default),

  DIRECT_BUILTIN (csrrd_w, LARCH_USI_FTYPE_USI, default),
  DIRECT_BUILTIN (csrrd_d, LARCH_UDI_FTYPE_USI, default),
  DIRECT_BUILTIN (csrwr_w, LARCH_USI_FTYPE_USI_USI, default),
  DIRECT_BUILTIN (csrwr_d, LARCH_UDI_FTYPE_UDI_USI, default),
  DIRECT_BUILTIN (csrxchg_w, LARCH_USI_FTYPE_USI_USI_USI, default),
  DIRECT_BUILTIN (csrxchg_d, LARCH_UDI_FTYPE_UDI_UDI_USI, default),
  DIRECT_BUILTIN (iocsrrd_b, LARCH_UQI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_h, LARCH_UHI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_w, LARCH_USI_FTYPE_USI, default),
  DIRECT_BUILTIN (iocsrrd_d, LARCH_UDI_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_b, LARCH_VOID_FTYPE_UQI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_h, LARCH_VOID_FTYPE_UHI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_w, LARCH_VOID_FTYPE_USI_USI, default),
  DIRECT_NO_TARGET_BUILTIN (iocsrwr_d, LARCH_VOID_FTYPE_UDI_USI, default),

  DIRECT_BUILTIN (cpucfg, LARCH_USI_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (asrtle_d, LARCH_VOID_FTYPE_DI_DI, default),
  DIRECT_NO_TARGET_BUILTIN (asrtgt_d, LARCH_VOID_FTYPE_DI_DI, default),
  DIRECT_NO_TARGET_BUILTIN (syscall, LARCH_VOID_FTYPE_USI, default),
  DIRECT_NO_TARGET_BUILTIN (break, LARCH_VOID_FTYPE_USI, default),
};

/* Index I is the function declaration for loongarch_builtins[I], or null if
   the function isn't defined on this target.  */
static GTY (()) tree loongarch_builtin_decls[ARRAY_SIZE (loongarch_builtins)];
/* Get the index I of the function declaration for loongarch_builtin_decls[I]
   using the instruction code or return null if not defined for the target.  */
static GTY (()) int loongarch_get_builtin_decl_index[NUM_INSN_CODES];

/* Source-level argument types.  */
#define LARCH_ATYPE_VOID void_type_node
#define LARCH_ATYPE_INT integer_type_node
#define LARCH_ATYPE_POINTER ptr_type_node

/* Standard mode-based argument types.  */
#define LARCH_ATYPE_QI intQI_type_node
#define LARCH_ATYPE_UQI unsigned_intQI_type_node
#define LARCH_ATYPE_HI intHI_type_node
#define LARCH_ATYPE_UHI unsigned_intHI_type_node
#define LARCH_ATYPE_SI intSI_type_node
#define LARCH_ATYPE_USI unsigned_intSI_type_node
#define LARCH_ATYPE_DI intDI_type_node
#define LARCH_ATYPE_UDI unsigned_intDI_type_node
#define LARCH_ATYPE_SF float_type_node
#define LARCH_ATYPE_DF double_type_node

/* LARCH_FTYPE_ATYPESN takes N LARCH_FTYPES-like type codes and lists
   their associated LARCH_ATYPEs.  */
#define LARCH_FTYPE_ATYPES1(A, B) LARCH_ATYPE_##A, LARCH_ATYPE_##B

#define LARCH_FTYPE_ATYPES2(A, B, C) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C

#define LARCH_FTYPE_ATYPES3(A, B, C, D) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C, LARCH_ATYPE_##D

#define LARCH_FTYPE_ATYPES4(A, B, C, D, E) \
  LARCH_ATYPE_##A, LARCH_ATYPE_##B, LARCH_ATYPE_##C, LARCH_ATYPE_##D, \
  LARCH_ATYPE_##E

/* Return the function type associated with function prototype TYPE.  */

static tree
loongarch_build_function_type (enum loongarch_function_type type)
{
  static tree types[(int) LARCH_MAX_FTYPE_MAX];

  if (types[(int) type] == NULL_TREE)
    switch (type)
      {
#define DEF_LARCH_FTYPE(NUM, ARGS) \
  case LARCH_FTYPE_NAME##NUM ARGS: \
    types[(int) type] \
      = build_function_type_list (LARCH_FTYPE_ATYPES##NUM ARGS, NULL_TREE); \
    break;
#include "config/loongarch/loongarch-ftypes.def"
#undef DEF_LARCH_FTYPE
      default:
	gcc_unreachable ();
      }

  return types[(int) type];
}

/* Implement TARGET_INIT_BUILTINS.  */

void
loongarch_init_builtins (void)
{
  const struct loongarch_builtin_description *d;
  unsigned int i;
  tree type;

  /* Iterate through all of the bdesc arrays, initializing all of the
     builtin functions.  */
  for (i = 0; i < ARRAY_SIZE (loongarch_builtins); i++)
    {
      d = &loongarch_builtins[i];
      if (d->avail ())
	{
	  type = loongarch_build_function_type (d->function_type);
	  loongarch_builtin_decls[i]
	    = add_builtin_function (d->name, type, i, BUILT_IN_MD, NULL,
				    NULL);
	  loongarch_get_builtin_decl_index[d->icode] = i;
	}
    }
}

/* Implement TARGET_BUILTIN_DECL.  */

tree
loongarch_builtin_decl (unsigned int code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= ARRAY_SIZE (loongarch_builtins))
    return error_mark_node;
  return loongarch_builtin_decls[code];
}

/* Take argument ARGNO from EXP's argument list and convert it into
   an expand operand.  Store the operand in *OP.  */

static void
loongarch_prepare_builtin_arg (struct expand_operand *op, tree exp,
			       unsigned int argno)
{
  tree arg;
  rtx value;

  arg = CALL_EXPR_ARG (exp, argno);
  value = expand_normal (arg);
  create_input_operand (op, value, TYPE_MODE (TREE_TYPE (arg)));
}

/* Expand instruction ICODE as part of a built-in function sequence.
   Use the first NOPS elements of OPS as the instruction's operands.
   HAS_TARGET_P is true if operand 0 is a target; it is false if the
   instruction has no target.

   Return the target rtx if HAS_TARGET_P, otherwise return const0_rtx.  */

static rtx
loongarch_expand_builtin_insn (enum insn_code icode, unsigned int nops,
			       struct expand_operand *ops, bool has_target_p)
{
  if (!maybe_expand_insn (icode, nops, ops))
    {
      error ("invalid argument to built-in function");
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;
    }
  return has_target_p ? ops[0].value : const0_rtx;
}

/* Expand a LARCH_BUILTIN_DIRECT or LARCH_BUILTIN_DIRECT_NO_TARGET function;
   HAS_TARGET_P says which.  EXP is the CALL_EXPR that calls the function
   and ICODE is the code of the associated .md pattern.  TARGET, if nonnull,
   suggests a good place to put the result.  */

static rtx
loongarch_expand_builtin_direct (enum insn_code icode, rtx target, tree exp,
				 bool has_target_p)
{
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  int opno, argno;

  /* Map any target to operand 0.  */
  opno = 0;
  if (has_target_p)
    create_output_operand (&ops[opno++], target, TYPE_MODE (TREE_TYPE (exp)));

  /* Map the arguments to the other operands.  */
  gcc_assert (opno + call_expr_nargs (exp)
	      == insn_data[icode].n_generator_args);
  for (argno = 0; argno < call_expr_nargs (exp); argno++)
    loongarch_prepare_builtin_arg (&ops[opno++], exp, argno);

  return loongarch_expand_builtin_insn (icode, opno, ops, has_target_p);
}

/* Implement TARGET_EXPAND_BUILTIN.  */

rtx
loongarch_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED,
			  int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl;
  unsigned int fcode, avail;
  const struct loongarch_builtin_description *d;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fcode = DECL_MD_FUNCTION_CODE (fndecl);
  gcc_assert (fcode < ARRAY_SIZE (loongarch_builtins));
  d = &loongarch_builtins[fcode];
  avail = d->avail ();
  gcc_assert (avail != 0);
  switch (d->builtin_type)
    {
    case LARCH_BUILTIN_DIRECT:
      return loongarch_expand_builtin_direct (d->icode, target, exp, true);

    case LARCH_BUILTIN_DIRECT_NO_TARGET:
      return loongarch_expand_builtin_direct (d->icode, target, exp, false);
    }
  gcc_unreachable ();
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

void
loongarch_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_HARD_FLOAT_ABI)
    return;
  tree exceptions_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree fcsr_orig_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree fcsr_mod_var = create_tmp_var_raw (LARCH_ATYPE_USI);
  tree const0 = build_int_cst (LARCH_ATYPE_UQI, 0);
  tree get_fcsr = loongarch_builtin_decls[LARCH_MOVFCSR2GR];
  tree set_fcsr = loongarch_builtin_decls[LARCH_MOVGR2FCSR];
  tree get_fcsr_hold_call = build_call_expr (get_fcsr, 1, const0);
  tree hold_assign_orig = build4 (TARGET_EXPR, LARCH_ATYPE_USI,
				  fcsr_orig_var, get_fcsr_hold_call,
				  NULL, NULL);
  tree hold_mod_val = build2 (BIT_AND_EXPR, LARCH_ATYPE_USI, fcsr_orig_var,
			      build_int_cst (LARCH_ATYPE_USI, 0xffe0ffe0));
  tree hold_assign_mod = build4 (TARGET_EXPR, LARCH_ATYPE_USI,
				 fcsr_mod_var, hold_mod_val, NULL, NULL);
  tree set_fcsr_hold_call = build_call_expr (set_fcsr, 2, const0,
					     fcsr_mod_var);
  tree hold_all = build2 (COMPOUND_EXPR, LARCH_ATYPE_USI, hold_assign_orig,
			  hold_assign_mod);
  *hold = build2 (COMPOUND_EXPR, void_type_node, hold_all, set_fcsr_hold_call);

  *clear = build_call_expr (set_fcsr, 2, const0, fcsr_mod_var);

  tree get_fcsr_update_call = build_call_expr (get_fcsr, 1, const0);
  *update = build4 (TARGET_EXPR, LARCH_ATYPE_USI, exceptions_var,
		    get_fcsr_update_call, NULL, NULL);
  tree set_fcsr_update_call = build_call_expr (set_fcsr, 2, const0,
					       fcsr_orig_var);
  *update = build2 (COMPOUND_EXPR, void_type_node, *update,
		    set_fcsr_update_call);
  tree atomic_feraiseexcept
    = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  tree int_exceptions_var = fold_convert (integer_type_node, exceptions_var);
  tree atomic_feraiseexcept_call = build_call_expr (atomic_feraiseexcept, 1,
						    int_exceptions_var);
  *update = build2 (COMPOUND_EXPR, void_type_node, *update,
		    atomic_feraiseexcept_call);
}

/* Implement TARGET_BUILTIN_VA_LIST.  */

tree
loongarch_build_builtin_va_list (void)
{
  return ptr_type_node;
}
