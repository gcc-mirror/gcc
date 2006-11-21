/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cpplib.h"
#include "tree.h"
#include "c-tree.h"
#include "c-pragma.h"
#include "function.h"
#include "rtl.h"
#include "expr.h"
#include "errors.h"
#include "tm_p.h"
#include "langhooks.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "recog.h"
#include "optabs.h"
#include "spu-builtins.h"

static rtx spu_expand_builtin_1 (struct spu_builtin_description *d,
				 tree arglist, rtx target);
static void spu_check_builtin_parm (struct spu_builtin_description *, rtx,
				    int);
static void expand_builtin_args (struct spu_builtin_description *, tree, rtx,
				 rtx[]);
static rtx spu_force_reg (enum machine_mode mode, rtx op);

/* Builtin types, data and prototypes. */
struct spu_builtin_range
{
  int low, high;
};

static struct spu_builtin_range spu_builtin_range[] = {
  {-0x40ll, 0x7fll},		/* SPU_BTI_7     */
  {-0x40ll, 0x3fll},		/* SPU_BTI_S7    */
  {0ll, 0x7fll},		/* SPU_BTI_U7    */
  {-0x200ll, 0x1ffll},		/* SPU_BTI_S10   */
  {-0x2000ll, 0x1fffll},	/* SPU_BTI_S10_4 */
  {0ll, 0x3fffll},		/* SPU_BTI_U14   */
  {-0x8000ll, 0xffffll},	/* SPU_BTI_16    */
  {-0x8000ll, 0x7fffll},	/* SPU_BTI_S16   */
  {-0x20000ll, 0x1ffffll},	/* SPU_BTI_S16_2 */
  {0ll, 0xffffll},		/* SPU_BTI_U16   */
  {0ll, 0x3ffffll},		/* SPU_BTI_U16_2 */
  {0ll, 0x3ffffll},		/* SPU_BTI_U18   */
};


/* Helper for spu_resolve_overloaded_builtin.  */
static tree
spu_build_overload_builtin (tree fndecl, tree fnargs)
{
  tree param, param_type;
  tree ret_type = TREE_TYPE (TREE_TYPE (fndecl));
  tree arg, arglist = NULL_TREE;
  tree val;

  for (param = TYPE_ARG_TYPES (TREE_TYPE (fndecl)), arg = fnargs;
       param != void_list_node;
       param = TREE_CHAIN (param), arg = TREE_CHAIN (arg))
    {
      gcc_assert (arg != NULL_TREE);
      param_type = TREE_VALUE (param);
      val = default_conversion (TREE_VALUE (arg));
      val = fold_convert (param_type, val);

      arglist = tree_cons (NULL_TREE, val, arglist);
    }
  gcc_assert (arg == NULL_TREE);
  arglist = nreverse (arglist);

  return fold_convert (ret_type, build_function_call_expr (fndecl, arglist));
}

/* target hook for resolve_overloaded_builtin(). Returns a function call
   RTX if we can resolve the overloaded builtin */
tree
spu_resolve_overloaded_builtin (tree fndecl, tree fnargs)
{
  spu_function_code new_fcode, fcode =
    DECL_FUNCTION_CODE (fndecl) - END_BUILTINS;
  struct spu_builtin_description *desc;
  tree match = NULL_TREE;

  /* The vector types are not available if the backend is not initalized */
  gcc_assert (!flag_preprocess_only);

  desc = &spu_builtins[fcode];
  if (desc->type != B_OVERLOAD)
    return NULL_TREE;

  /* Compare the signature of each internal builtin function with the
     function arguments until a match is found. */

  for (new_fcode = fcode + 1; spu_builtins[new_fcode].type == B_INTERNAL;
       new_fcode++)
    {
      tree decl = spu_builtins[new_fcode].fndecl;
      tree params = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree arg, param;
      int p;

      for (param = params, arg = fnargs, p = 0;
	   param != void_list_node;
	   param = TREE_CHAIN (param), arg = TREE_CHAIN (arg), p++)
	{
	  tree var, arg_type, param_type = TREE_VALUE (param);

	  if (!arg)
	    {
	      error ("insufficient arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  var = TREE_VALUE (arg);

	  if (TREE_CODE (var) == NON_LVALUE_EXPR)
	    var = TREE_OPERAND (var, 0);

	  if (TREE_CODE (var) == ERROR_MARK)
	    return NULL_TREE;	/* Let somebody else deal with the problem. */

	  arg_type = TREE_TYPE (var);

	  /* The intrinsics spec does not specify precisely how to
	     resolve generic intrinsics.  We require an exact match
	     for vector types and let C do it's usual parameter type
	     checking/promotions for scalar arguments, except for the
	     first argument of intrinsics which don't have a vector
	     parameter. */
	  if ((TREE_CODE (param_type) == VECTOR_TYPE
	       || ((fcode == SPU_SPLATS || fcode == SPU_PROMOTE
		    || fcode == SPU_HCMPEQ || fcode == SPU_HCMPGT
		    || fcode == SPU_MASKB || fcode == SPU_MASKH
		    || fcode == SPU_MASKW) && p == 0))
	      && !comptypes (TYPE_MAIN_VARIANT (param_type),
			     TYPE_MAIN_VARIANT (arg_type)))
	    break;
	}
      if (param == void_list_node)
	{
	  if (arg)
	    {
	      error ("too many arguments to overloaded function %s",
		     desc->name);
	      return error_mark_node;
	    }

	  match = decl;
	  break;
	}
    }

  if (match == NULL_TREE)
    {
      error ("parameter list does not match a valid signature for %s()",
	     desc->name);
      return error_mark_node;
    }

  return spu_build_overload_builtin (match, fnargs);
}

static void
spu_check_builtin_parm (struct spu_builtin_description *d, rtx op, int p)
{
  HOST_WIDE_INT v = 0;
  int lsbits;
  /* Check the range of immediate operands. */
  if (p >= SPU_BTI_7 && p <= SPU_BTI_U18)
    {
      int range = p - SPU_BTI_7;
      if (!CONSTANT_P (op)
	  || (GET_CODE (op) == CONST_INT
	      && (INTVAL (op) < spu_builtin_range[range].low
		  || INTVAL (op) > spu_builtin_range[range].high)))
	error ("%s expects an integer literal in the range [%d, %d].",
	       d->name,
	       spu_builtin_range[range].low, spu_builtin_range[range].high);

      if (GET_CODE (op) == CONST
	  && (GET_CODE (XEXP (op, 0)) == PLUS
	      || GET_CODE (XEXP (op, 0)) == MINUS))
	{
	  v = INTVAL (XEXP (XEXP (op, 0), 1));
	  op = XEXP (XEXP (op, 0), 0);
	}
      else if (GET_CODE (op) == CONST_INT)
	v = INTVAL (op);

      switch (p)
	{
	case SPU_BTI_S10_4:
	  lsbits = 4;
	  break;
	case SPU_BTI_U16_2:
	  /* This is only used in lqa, and stqa.  Even though the insns
	     encode 16 bits of the address (all but the 2 least
	     significant), only 14 bits are used because it is masked to
	     be 16 byte aligned. */
	  lsbits = 4;
	  break;
	case SPU_BTI_S16_2:
	  /* This is used for lqr and stqr. */
	  lsbits = 2;
	  break;
	default:
	  lsbits = 0;
	}

      if (GET_CODE (op) == LABEL_REF
	  || (GET_CODE (op) == SYMBOL_REF
	      && SYMBOL_REF_FUNCTION_P (op))
	  || (INTVAL (op) & ((1 << lsbits) - 1)) != 0)
	warning (0, "%d least significant bits of %s are ignored.", lsbits,
		 d->name);
    }
}

static void
expand_builtin_args (struct spu_builtin_description *d, tree arglist,
		     rtx target, rtx ops[])
{
  enum insn_code icode = d->icode;
  int i = 0;

  /* Expand the arguments into rtl. */

  if (d->parm[0] != SPU_BTI_VOID)
    ops[i++] = target;

  for (; i < insn_data[icode].n_operands; i++)
    {
      tree arg = TREE_VALUE (arglist);
      if (arg == 0)
	abort ();
      ops[i] = expand_expr (arg, NULL_RTX, VOIDmode, 0);
      arglist = TREE_CHAIN (arglist);
    }
}

static rtx
spu_expand_builtin_1 (struct spu_builtin_description *d,
		      tree arglist, rtx target)
{
  rtx pat;
  rtx ops[8];
  enum insn_code icode = d->icode;
  enum machine_mode mode, tmode;
  int i, p;
  tree return_type;

  /* Set up ops[] with values from arglist. */
  expand_builtin_args (d, arglist, target, ops);

  /* Handle the target operand which must be operand 0. */
  i = 0;
  if (d->parm[0] != SPU_BTI_VOID)
    {

      /* We prefer the mode specified for the match_operand otherwise
         use the mode from the builtin function prototype. */
      tmode = insn_data[d->icode].operand[0].mode;
      if (tmode == VOIDmode)
	tmode = TYPE_MODE (spu_builtin_types[d->parm[0]]);

      /* Try to use target because not using it can lead to extra copies
         and when we are using all of the registers extra copies leads
         to extra spills.  */
      if (target && GET_CODE (target) == REG && GET_MODE (target) == tmode)
	ops[0] = target;
      else
	target = ops[0] = gen_reg_rtx (tmode);

      if (!(*insn_data[icode].operand[0].predicate) (ops[0], tmode))
	abort ();

      i++;
    }

  /* Ignore align_hint, but still expand it's args in case they have
     side effects. */
  if (icode == CODE_FOR_spu_align_hint)
    return 0;

  /* Handle the rest of the operands. */
  for (p = 1; i < insn_data[icode].n_operands; i++, p++)
    {
      if (insn_data[d->icode].operand[i].mode != VOIDmode)
	mode = insn_data[d->icode].operand[i].mode;
      else
	mode = TYPE_MODE (spu_builtin_types[d->parm[i]]);

      /* mode can be VOIDmode here for labels */

      /* For specific intrinsics with an immediate operand, e.g.,
         si_ai(), we sometimes need to convert the scalar argument to a
         vector argument by splatting the scalar. */
      if (VECTOR_MODE_P (mode)
	  && (GET_CODE (ops[i]) == CONST_INT
	      || GET_MODE_CLASS (GET_MODE (ops[i])) == MODE_INT
	      || GET_MODE_CLASS (GET_MODE (ops[i])) == MODE_FLOAT))
	{
	  if (GET_CODE (ops[i]) == CONST_INT)
	    ops[i] = spu_const (mode, INTVAL (ops[i]));
	  else
	    {
	      rtx reg = gen_reg_rtx (mode);
	      enum machine_mode imode = GET_MODE_INNER (mode);
	      if (!spu_nonmem_operand (ops[i], GET_MODE (ops[i])))
		ops[i] = force_reg (GET_MODE (ops[i]), ops[i]);
	      if (imode != GET_MODE (ops[i]))
		ops[i] = convert_to_mode (imode, ops[i],
					  TYPE_UNSIGNED (spu_builtin_types
							 [d->parm[i]]));
	      emit_insn (gen_spu_splats (reg, ops[i]));
	      ops[i] = reg;
	    }
	}

      if (!(*insn_data[icode].operand[i].predicate) (ops[i], mode))
	ops[i] = spu_force_reg (mode, ops[i]);

      spu_check_builtin_parm (d, ops[i], d->parm[p]);
    }

  switch (insn_data[icode].n_operands)
    {
    case 0:
      pat = GEN_FCN (icode) (0);
      break;
    case 1:
      pat = GEN_FCN (icode) (ops[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (ops[0], ops[1]);
      break;
    case 3:
      pat = GEN_FCN (icode) (ops[0], ops[1], ops[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3]);
      break;
    case 5:
      pat = GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3], ops[4]);
      break;
    case 6:
      pat = GEN_FCN (icode) (ops[0], ops[1], ops[2], ops[3], ops[4], ops[5]);
      break;
    default:
      abort ();
    }

  if (!pat)
    abort ();

  if (d->type == B_CALL || d->type == B_BISLED)
    emit_call_insn (pat);
  else if (d->type == B_JUMP)
    {
      emit_jump_insn (pat);
      emit_barrier ();
    }
  else
    emit_insn (pat);

  return_type = spu_builtin_types[d->parm[0]];
  if (d->parm[0] != SPU_BTI_VOID
      && GET_MODE (target) != TYPE_MODE (return_type))
    {
      /* target is the return value.  It should always be the mode of
         the builtin function prototype. */
      target = spu_force_reg (TYPE_MODE (return_type), target);
    }

  return target;
}

rtx
spu_expand_builtin (tree exp,
		    rtx target,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl) - END_BUILTINS;
  tree arglist = TREE_OPERAND (exp, 1);
  struct spu_builtin_description *d;

  if (fcode < NUM_SPU_BUILTINS)
    {
      d = &spu_builtins[fcode];

      return spu_expand_builtin_1 (d, arglist, target);
    }
  abort ();
}

static rtx
spu_force_reg (enum machine_mode mode, rtx op)
{
  rtx x, r;
  if (GET_MODE (op) == VOIDmode || GET_MODE (op) == BLKmode)
    {
      if ((SCALAR_INT_MODE_P (mode) && GET_CODE (op) == CONST_INT)
	  || GET_MODE (op) == BLKmode)
	return force_reg (mode, convert_to_mode (mode, op, 0));
      abort ();
    }

  r = force_reg (GET_MODE (op), op);
  if (GET_MODE_SIZE (GET_MODE (op)) == GET_MODE_SIZE (mode))
    {
      x = simplify_gen_subreg (mode, r, GET_MODE (op), 0);
      if (x)
	return x;
    }

  x = gen_reg_rtx (mode);
  emit_insn (gen_spu_convert (x, r));
  return x;
}
