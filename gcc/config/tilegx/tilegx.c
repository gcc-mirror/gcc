/* Subroutines used for code generation on the Tilera TILE-Gx.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic.h"
#include "output.h"
#include "insn-attr.h"
#include "alias.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "expr.h"
#include "langhooks.h"
#include "cfgrtl.h"
#include "tm-constrs.h"
#include "dwarf2.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "tilegx-builtins.h"
#include "tilegx-multiply.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* SYMBOL_REF for GOT */
static GTY(()) rtx g_got_symbol = NULL;

/* Report whether we're printing out the first address fragment of a
   POST_INC or POST_DEC memory reference, from TARGET_PRINT_OPERAND to
   TARGET_PRINT_OPERAND_ADDRESS.  */
static bool output_memory_autoinc_first;



/* Option handling  */

/* Implement TARGET_OPTION_OVERRIDE.  */
static void
tilegx_option_override (void)
{
  if (global_options_set.x_tilegx_cmodel)
    {
      switch (tilegx_cmodel)
	{
	case CM_SMALL:
	case CM_SMALL_PIC:
	  if (flag_pic)
	    tilegx_cmodel = CM_SMALL_PIC;
	  break;

	case CM_LARGE:
	case CM_LARGE_PIC:
	  if (flag_pic)
	    tilegx_cmodel = CM_LARGE_PIC;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    tilegx_cmodel = flag_pic ? CM_SMALL_PIC : CM_SMALL;

  /* When modulo scheduling is enabled, we still rely on regular
     scheduler for bundling.  */
  if (flag_modulo_sched)
    flag_resched_modulo_sched = 1;
}



/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */
static bool
tilegx_scalar_mode_supported_p (scalar_mode mode)
{
  switch (mode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
    case E_DImode:
    case E_TImode:
      return true;

    case E_SFmode:
    case E_DFmode:
      return true;

    default:
      return false;
    }
}


/* Implement TARGET_VECTOR_MODE_SUPPORTED_P.  */
static bool
tilegx_vector_mode_supported_p (machine_mode mode)
{
  return mode == V8QImode || mode == V4HImode || mode == V2SImode;
}


/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */
static bool
tilegx_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED,
			       rtx x ATTRIBUTE_UNUSED)
{
  return true;
}


/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */
static bool
tilegx_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  return (tilegx_cmodel != CM_LARGE && tilegx_cmodel != CM_LARGE_PIC
	  && (decl != NULL));
}


/* Implement TARGET_PASS_BY_REFERENCE.  Variable sized types are
   passed by reference.  */
static bool
tilegx_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  return (type && TYPE_SIZE (type)
	  && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST);
}


/* Implement TARGET_RETURN_IN_MSB.  We return a value in the most
   significant part of a register if:
   - the target is big-endian; and
   - the value has an aggregate type (e.g., structure or union).  */
static bool
tilegx_return_in_msb (const_tree valtype)
{
  return (TARGET_BIG_ENDIAN && AGGREGATE_TYPE_P (valtype));
}


/* Implement TARGET_RETURN_IN_MEMORY.  */
static bool
tilegx_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  return !IN_RANGE (int_size_in_bytes (type),
		    0, TILEGX_NUM_RETURN_REGS * UNITS_PER_WORD);
}


/* Implement TARGET_MODE_REP_EXTENDED.  */
static int
tilegx_mode_rep_extended (scalar_int_mode mode, scalar_int_mode mode_rep)
{
  /* SImode register values are sign-extended to DImode.  */
  if (mode == SImode && mode_rep == DImode)
    return SIGN_EXTEND;

  return UNKNOWN;
}


/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  */
static unsigned int
tilegx_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int alignment;

  alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);
  if (alignment < PARM_BOUNDARY)
    alignment = PARM_BOUNDARY;
  if (alignment > STACK_BOUNDARY)
    alignment = STACK_BOUNDARY;
  return alignment;
}


/* Implement TARGET_FUNCTION_ARG.  */
static rtx
tilegx_function_arg (cumulative_args_t cum_v,
		     machine_mode mode,
		     const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS cum = *get_cumulative_args (cum_v);
  int byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  bool doubleword_aligned_p;

  if (cum >= TILEGX_NUM_ARG_REGS)
    return NULL_RTX;

  /* See whether the argument has doubleword alignment.  */
  doubleword_aligned_p =
    tilegx_function_arg_boundary (mode, type) > BITS_PER_WORD;

  if (doubleword_aligned_p)
    cum += cum & 1;

  /* The ABI does not allow parameters to be passed partially in reg
     and partially in stack.  */
  if ((cum + (byte_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
      > TILEGX_NUM_ARG_REGS)
    return NULL_RTX;

  return gen_rtx_REG (mode, cum);
}


/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */
static void
tilegx_function_arg_advance (cumulative_args_t cum_v,
			     machine_mode mode,
			     const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  int byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  int word_size = (byte_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  bool doubleword_aligned_p;

  /* See whether the argument has doubleword alignment.  */
  doubleword_aligned_p =
    tilegx_function_arg_boundary (mode, type) > BITS_PER_WORD;

  if (doubleword_aligned_p)
    *cum += *cum & 1;

  /* If the current argument does not fit in the pretend_args space,
     skip over it.  */
  if (*cum < TILEGX_NUM_ARG_REGS
      && *cum + word_size > TILEGX_NUM_ARG_REGS)
    *cum = TILEGX_NUM_ARG_REGS;

  *cum += word_size;
}


/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
tilegx_function_value (const_tree valtype, const_tree fn_decl_or_type,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsigned_p;

  mode = TYPE_MODE (valtype);
  unsigned_p = TYPE_UNSIGNED (valtype);

  mode = promote_function_mode (valtype, mode, &unsigned_p,
				fn_decl_or_type, 1);

  return gen_rtx_REG (mode, 0);
}


/* Implement TARGET_LIBCALL_VALUE.  */
static rtx
tilegx_libcall_value (machine_mode mode,
		       const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 0);
}


/* Implement FUNCTION_VALUE_REGNO_P.  */
static bool
tilegx_function_value_regno_p (const unsigned int regno)
{
  return regno < TILEGX_NUM_RETURN_REGS;
}


/* Implement TARGET_BUILD_BUILTIN_VA_LIST.  */
static tree
tilegx_build_builtin_va_list (void)
{
  tree f_args, f_skip, record, type_decl;
  bool owp;

  record = lang_hooks.types.make_type (RECORD_TYPE);

  type_decl = build_decl (BUILTINS_LOCATION, TYPE_DECL,
			  get_identifier ("__va_list_tag"), record);

  f_args = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		       get_identifier ("__args"), ptr_type_node);
  f_skip = build_decl (BUILTINS_LOCATION, FIELD_DECL,
		       get_identifier ("__skip"), ptr_type_node);

  DECL_FIELD_CONTEXT (f_args) = record;

  DECL_FIELD_CONTEXT (f_skip) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_args;
  TREE_CHAIN (f_args) = f_skip;

  /* We know this is being padded and we want it too.  It is an
     internal type so hide the warnings from the user.  */
  owp = warn_padded;
  warn_padded = false;

  layout_type (record);

  warn_padded = owp;

  /* The correct type is an array type of one element.  */
  return record;
}


/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */
static void
tilegx_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  tree f_args, f_skip;
  tree args, skip, t;

  f_args = TYPE_FIELDS (TREE_TYPE (valist));
  f_skip = TREE_CHAIN (f_args);

  args =
    build3 (COMPONENT_REF, TREE_TYPE (f_args), valist, f_args, NULL_TREE);
  skip =
    build3 (COMPONENT_REF, TREE_TYPE (f_skip), valist, f_skip, NULL_TREE);

  /* Find the __args area.  */
  t = make_tree (TREE_TYPE (args), virtual_incoming_args_rtx);
  t = fold_build_pointer_plus_hwi (t,
				   UNITS_PER_WORD *
				   (crtl->args.info - TILEGX_NUM_ARG_REGS));

  if (crtl->args.pretend_args_size > 0)
    t = fold_build_pointer_plus_hwi (t, -STACK_POINTER_OFFSET);

  t = build2 (MODIFY_EXPR, TREE_TYPE (args), args, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the __skip area.  */
  t = make_tree (TREE_TYPE (skip), virtual_incoming_args_rtx);
  t = fold_build_pointer_plus_hwi (t, -STACK_POINTER_OFFSET);
  t = build2 (MODIFY_EXPR, TREE_TYPE (skip), skip, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}


/* Implement TARGET_SETUP_INCOMING_VARARGS.  */
static void
tilegx_setup_incoming_varargs (cumulative_args_t cum,
			       machine_mode mode,
			       tree type, int *pretend_args, int no_rtl)
{
  CUMULATIVE_ARGS local_cum = *get_cumulative_args (cum);
  int first_reg;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  targetm.calls.function_arg_advance (pack_cumulative_args (&local_cum),
				      mode, type, true);
  first_reg = local_cum;

  if (local_cum < TILEGX_NUM_ARG_REGS)
    {
      *pretend_args = UNITS_PER_WORD * (TILEGX_NUM_ARG_REGS - first_reg);

      if (!no_rtl)
	{
	  alias_set_type set = get_varargs_alias_set ();
	  rtx tmp =
	    gen_rtx_MEM (BLKmode, plus_constant (Pmode,
						 virtual_incoming_args_rtx,
						 -STACK_POINTER_OFFSET -
						 UNITS_PER_WORD *
						 (TILEGX_NUM_ARG_REGS -
						  first_reg)));
	  MEM_NOTRAP_P (tmp) = 1;
	  set_mem_alias_set (tmp, set);
	  move_block_from_reg (first_reg, tmp,
			       TILEGX_NUM_ARG_REGS - first_reg);
	}
    }
  else
    *pretend_args = 0;
}


/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  Gimplify va_arg by updating
   the va_list structure VALIST as required to retrieve an argument of
   type TYPE, and returning that argument.
   
   ret = va_arg(VALIST, TYPE);

   generates code equivalent to:
  
    paddedsize = (sizeof(TYPE) + 7) & -8;
    if (  (VALIST.__args + paddedsize > VALIST.__skip)
	& (VALIST.__args <= VALIST.__skip))
      addr = VALIST.__skip + STACK_POINTER_OFFSET;
    else
      addr = VALIST.__args;
    VALIST.__args = addr + paddedsize;
    if (BYTES_BIG_ENDIAN)
      ret = *(TYPE *)(addr + paddedsize - sizeof(TYPE));
    else
      ret = *(TYPE *)addr;
 */
static tree
tilegx_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			     gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree f_args, f_skip;
  tree args, skip;
  HOST_WIDE_INT size, rsize;
  tree addr, tmp;
  bool pass_by_reference_p;

  f_args = TYPE_FIELDS (va_list_type_node);
  f_skip = TREE_CHAIN (f_args);

  args =
    build3 (COMPONENT_REF, TREE_TYPE (f_args), valist, f_args, NULL_TREE);
  skip =
    build3 (COMPONENT_REF, TREE_TYPE (f_skip), valist, f_skip, NULL_TREE);

  addr = create_tmp_var (ptr_type_node, "va_arg");

  /* If an object is dynamically sized, a pointer to it is passed
     instead of the object itself.  */
  pass_by_reference_p = pass_by_reference (NULL, TYPE_MODE (type), type,
					   false);

  if (pass_by_reference_p)
    type = build_pointer_type (type);

  size = int_size_in_bytes (type);
  rsize = ((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD) * UNITS_PER_WORD;

  /* If the alignment of the type is greater than the default for a
     parameter, align to the STACK_BOUNDARY. */
  if (TYPE_ALIGN (type) > PARM_BOUNDARY)
    {
      /* Assert the only case we generate code for: when
	 stack boundary = 2 * parm boundary. */
      gcc_assert (STACK_BOUNDARY == PARM_BOUNDARY * 2);

      tmp = build2 (BIT_AND_EXPR, sizetype,
		    fold_convert (sizetype, unshare_expr (args)),
		    size_int (PARM_BOUNDARY / 8));
      tmp = build2 (POINTER_PLUS_EXPR, ptr_type_node,
		    unshare_expr (args), tmp);

      gimplify_assign (unshare_expr (args), tmp, pre_p);
    }
 
  /* Build conditional expression to calculate addr. The expression
     will be gimplified later.  */
  tmp = fold_build_pointer_plus_hwi (unshare_expr (args), rsize);
  tmp = build2 (TRUTH_AND_EXPR, boolean_type_node,
		build2 (GT_EXPR, boolean_type_node, tmp, unshare_expr (skip)),
		build2 (LE_EXPR, boolean_type_node, unshare_expr (args),
			unshare_expr (skip)));

  tmp = build3 (COND_EXPR, ptr_type_node, tmp,
		build2 (POINTER_PLUS_EXPR, ptr_type_node, unshare_expr (skip),
			size_int (STACK_POINTER_OFFSET)),
		unshare_expr (args));

  /* Adjust the address of va_arg if it is in big endian mode.  */
  if (BYTES_BIG_ENDIAN && rsize > size)
    tmp = fold_build_pointer_plus_hwi (tmp, rsize - size);
  gimplify_assign (addr, tmp, pre_p);

  /* Update VALIST.__args.  */
  
  if (BYTES_BIG_ENDIAN && rsize > size)
    tmp = fold_build_pointer_plus_hwi (addr, size);
  else
    tmp = fold_build_pointer_plus_hwi (addr, rsize);
  gimplify_assign (unshare_expr (args), tmp, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (pass_by_reference_p)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}



/* Implement TARGET_RTX_COSTS.  */
static bool
tilegx_rtx_costs (rtx x, machine_mode mode, int outer_code, int opno,
		  int *total, bool speed)
{
  int code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
      /* If this is an 8-bit constant, return zero since it can be
	 used nearly anywhere with no cost.  If it is a valid operand
	 for an ADD or AND, likewise return 0 if we know it will be
	 used in that context.  Otherwise, return 2 since it might be
	 used there later.  All other constants take at least two
	 insns.  */
      if (satisfies_constraint_I (x))
	{
	  *total = 0;
	  return true;
	}
      else if (outer_code == PLUS && add_operand (x, VOIDmode))
	{
	  /* Slightly penalize large constants even though we can add
	     them in one instruction, because it forces the use of
	     2-wide bundling mode.  */
	  *total = 1;
	  return true;
	}
      else if (move_operand (x, SImode))
	{
	  /* We can materialize in one move.  */
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else
	{
	  /* We can materialize in two moves.  */
	  *total = COSTS_N_INSNS (2);
	  return true;
	}

      return false;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (2);
      return true;

    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (4);
      return true;

    case HIGH:
      *total = 0;
      return true;

    case MEM:
      /* If outer-code was a sign or zero extension, a cost of
	 COSTS_N_INSNS (1) was already added in, so account for
	 that.  */
      if (outer_code == ZERO_EXTEND || outer_code == SIGN_EXTEND)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (2);
      return true;

    case PLUS:
      /* Convey that shl[123]add are efficient.  */
      if (GET_CODE (XEXP (x, 0)) == MULT
	  && cint_248_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
	{
	  *total = (rtx_cost (XEXP (XEXP (x, 0), 0), mode,
			      (enum rtx_code) outer_code, opno, speed)
		    + rtx_cost (XEXP (x, 1), mode,
				(enum rtx_code) outer_code, opno, speed)
		    + COSTS_N_INSNS (1));
	  return true;
	}
      return false;

    case MULT:
      *total = COSTS_N_INSNS (2);
      return false;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      /* These are handled by software and are very expensive.  */
      *total = COSTS_N_INSNS (100);
      return false;

    case UNSPEC:
    case UNSPEC_VOLATILE:
      {
	int num = XINT (x, 1);

	if (num <= TILEGX_LAST_LATENCY_1_INSN)
	  *total = COSTS_N_INSNS (1);
	else if (num <= TILEGX_LAST_LATENCY_2_INSN)
	  *total = COSTS_N_INSNS (2);
	else if (num > TILEGX_LAST_LATENCY_INSN)
	  {
	    if (num == UNSPEC_NON_TEMPORAL)
	      {
		/* These are basically loads.  */
		if (outer_code == ZERO_EXTEND || outer_code == SIGN_EXTEND)
		  *total = COSTS_N_INSNS (1);
		else
		  *total = COSTS_N_INSNS (2);
	      }
	    else
	      {
		if (outer_code == PLUS)
		  *total = 0;
		else
		  *total = COSTS_N_INSNS (1);
	      }
	  }
	else
	  {
	    switch (num)
	      {
	      case UNSPEC_BLOCKAGE:
	      case UNSPEC_NETWORK_BARRIER:
	      case UNSPEC_ATOMIC:
		*total = 0;
		break;

	      case UNSPEC_LNK_AND_LABEL:
	      case UNSPEC_MF:
	      case UNSPEC_MOV_PCREL_STEP3:
	      case UNSPEC_NETWORK_RECEIVE:
	      case UNSPEC_NETWORK_SEND:
	      case UNSPEC_SPR_MOVE:
	      case UNSPEC_TLS_GD_ADD:
		*total = COSTS_N_INSNS (1);
		break;

	      case UNSPEC_TLS_IE_LOAD:
	      case UNSPEC_XCHG:
		*total = COSTS_N_INSNS (2);
		break;

	      case UNSPEC_SP_SET:
		*total = COSTS_N_INSNS (3);
		break;

	      case UNSPEC_SP_TEST:
		*total = COSTS_N_INSNS (4);
		break;

	      case UNSPEC_CMPXCHG:
	      case UNSPEC_INSN_CMPEXCH:
	      case UNSPEC_LATENCY_L2:
		*total = COSTS_N_INSNS (11);
		break;

	      case UNSPEC_TLS_GD_CALL:
		*total = COSTS_N_INSNS (30);
		break;

	      case UNSPEC_LATENCY_MISS:
		*total = COSTS_N_INSNS (80);
		break;

	      default:
		*total = COSTS_N_INSNS (1);
	      }
	  }
	return true;
      }

    default:
      return false;
    }
}



/* Rtl lowering.  */

/* Create a temporary variable to hold a partial result, to enable
   CSE.  */
static rtx
create_temp_reg_if_possible (machine_mode mode, rtx default_reg)
{
  return can_create_pseudo_p () ? gen_reg_rtx (mode) : default_reg;
}


/* Functions to save and restore machine-specific function data.  */
static struct machine_function *
tilegx_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* Do anything needed before RTL is emitted for each function.  */
void
tilegx_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function
     status.  */
  init_machine_status = tilegx_init_machine_status;

  if (cfun && cfun->machine && flag_pic)
    {
      static int label_num = 0;

      char text_label_name[32];

      struct machine_function *machine = cfun->machine;

      ASM_GENERATE_INTERNAL_LABEL (text_label_name, "L_PICLNK", label_num++);

      machine->text_label_symbol =
	gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (text_label_name));

      machine->text_label_rtx =
	gen_rtx_REG (Pmode, TILEGX_PIC_TEXT_LABEL_REGNUM);

      machine->got_rtx = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);

      machine->calls_tls_get_addr = false;
    }
}


/* Implement TARGET_EXPAND_TO_RTL_HOOK.  */
static void
tilegx_expand_to_rtl_hook (void)
{
  /* Exclude earlier sets of crtl->uses_pic_offset_table, because we
     only care about uses actually emitted.  */
  crtl->uses_pic_offset_table = 0;
}


/* Implement TARGET_SHIFT_TRUNCATION_MASK.  DImode shifts use the mode
   matching insns and therefore guarantee that the shift count is
   modulo 64.  SImode shifts sometimes use the 64 bit version so do
   not hold such guarantee.  */
static unsigned HOST_WIDE_INT
tilegx_shift_truncation_mask (machine_mode mode)
{
  return mode == DImode ? 63 : 0;
}


/* Implement TARGET_INIT_LIBFUNCS.  */
static void
tilegx_init_libfuncs (void)
{
  /* We need to explicitly generate these libfunc's to support
     conversion of divide by constant to multiply (the divide stubs in
     tilegx.md exist also for this reason).  Normally we'd expect gcc
     to lazily generate them when they are needed, but for some reason
     it's set up to only generate them if the mode is the word
     mode.  */
  set_optab_libfunc (sdiv_optab, SImode, "__divsi3");
  set_optab_libfunc (udiv_optab, SImode, "__udivsi3");
  set_optab_libfunc (smod_optab, SImode, "__modsi3");
  set_optab_libfunc (umod_optab, SImode, "__umodsi3");
}


/* Return true if X contains a thread-local symbol.  */
static bool
tilegx_tls_referenced_p (rtx x)
{
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
    x = XEXP (XEXP (x, 0), 0);

  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x))
    return true;

  /* That's all we handle in tilegx_legitimize_tls_address for
     now.  */
  return false;
}


/* Return true if X requires a scratch register.  It is given that
   flag_pic is on and that X satisfies CONSTANT_P.  */
static int
tilegx_pic_address_needs_scratch (rtx x)
{
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
      && (CONST_INT_P (XEXP (XEXP (x, 0), 1))))
    return true;

  return false;
}


/* Implement TARGET_LEGITIMATE_CONSTANT_P.  This is all constants for
   which we are willing to load the value into a register via a move
   pattern.  TLS cannot be treated as a constant because it can
   include a function call.  */
static bool
tilegx_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
    case SYMBOL_REF:
      return !tilegx_tls_referenced_p (x);

    default:
      return true;
    }
}


/* Return true if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and that
   X satisfies CONSTANT_P.  */
bool
tilegx_legitimate_pic_operand_p (rtx x)
{
  if (tilegx_pic_address_needs_scratch (x))
    return false;

  if (tilegx_tls_referenced_p (x))
    return false;

  return true;
}


/* Return true if the rtx X can be used as an address operand.  */
static bool
tilegx_legitimate_address_p (machine_mode ARG_UNUSED (mode), rtx x,
			     bool strict)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  switch (GET_CODE (x))
    {
    case POST_INC:
    case POST_DEC:
      if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	return false;

      x = XEXP (x, 0);
      break;

    case POST_MODIFY:
      if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	return false;

      if (GET_CODE (XEXP (x, 1)) != PLUS)
	return false;

      if (!rtx_equal_p (XEXP (x, 0), XEXP (XEXP (x, 1), 0)))
	return false;

      if (!satisfies_constraint_I (XEXP (XEXP (x, 1), 1)))
	return false;

      x = XEXP (x, 0);
      break;

    case REG:
      break;

    default:
      return false;
    }

  /* Check if x is a valid reg.  */
  if (!REG_P (x))
    return false;

  if (strict)
    return REGNO_OK_FOR_BASE_P (REGNO (x));
  else
    return true;
}


/* Return the rtx containing SYMBOL_REF to the text label.  */
static rtx
tilegx_text_label_symbol (void)
{
  return cfun->machine->text_label_symbol;
}


/* Return the register storing the value of the text label.  */
static rtx
tilegx_text_label_rtx (void)
{
  return cfun->machine->text_label_rtx;
}


/* Return the register storing the value of the global offset
   table.  */
static rtx
tilegx_got_rtx (void)
{
  return cfun->machine->got_rtx;
}


/* Return the SYMBOL_REF for _GLOBAL_OFFSET_TABLE_.  */
static rtx
tilegx_got_symbol (void)
{
  if (g_got_symbol == NULL)
    g_got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");

  return g_got_symbol;
}


/* Return a reference to the got to be used by tls references.  */
static rtx
tilegx_tls_got (void)
{
  rtx temp;
  if (flag_pic)
    {
      crtl->uses_pic_offset_table = 1;
      return tilegx_got_rtx ();
    }

  temp = gen_reg_rtx (Pmode);
  emit_move_insn (temp, tilegx_got_symbol ());

  return temp;
}


/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */
static rtx
tilegx_legitimize_tls_address (rtx addr)
{
  rtx ret;

  gcc_assert (can_create_pseudo_p ());

  if (GET_CODE (addr) == SYMBOL_REF)
    switch (SYMBOL_REF_TLS_MODEL (addr))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
      case TLS_MODEL_LOCAL_DYNAMIC:
	{
	  rtx r0, temp, temp2, temp3, got;

	  ret = gen_reg_rtx (Pmode);
	  r0 = gen_rtx_REG (Pmode, 0);
	  temp = gen_reg_rtx (Pmode);
	  temp2 = gen_reg_rtx (Pmode);
	  temp3 = gen_reg_rtx (Pmode);

	  got = tilegx_tls_got ();
	  if (TARGET_32BIT)
	    {
	      emit_insn (gen_mov_tls_gd_step1_32bit (temp, addr));
	      emit_insn (gen_mov_tls_gd_step2_32bit (temp2, temp, addr));
	      emit_insn (gen_tls_add_32bit (temp2, got, temp2, addr));
	    }
	  else
	    {
	      emit_insn (gen_mov_tls_gd_step1 (temp, addr));
	      emit_insn (gen_mov_tls_gd_step2 (temp2, temp, addr));
	      emit_insn (gen_tls_add (temp2, got, temp2, addr));
	    }

	  emit_move_insn (r0, temp2);

	  if (TARGET_32BIT)
	    {
	      emit_insn (gen_tls_gd_call_32bit (addr));
	    }
	  else
	    {
	      emit_insn (gen_tls_gd_call (addr));
	    }

	  emit_move_insn (temp3, r0);

	  rtx_insn *last;
	  if (TARGET_32BIT)
	    last = emit_insn (gen_tls_gd_add_32bit (ret, temp3, addr));
	  else
	    last = emit_insn (gen_tls_gd_add (ret, temp3, addr));

	  set_unique_reg_note (last, REG_EQUAL, copy_rtx (addr));
	  break;
	}
      case TLS_MODEL_INITIAL_EXEC:
	{
	  rtx temp, temp2, temp3, got;
	  rtx_insn *last;

	  ret = gen_reg_rtx (Pmode);
	  temp = gen_reg_rtx (Pmode);
	  temp2 = gen_reg_rtx (Pmode);
	  temp3 = gen_reg_rtx (Pmode);

	  got = tilegx_tls_got ();
	  if (TARGET_32BIT)
	    {
	      emit_insn (gen_mov_tls_ie_step1_32bit (temp, addr));
	      emit_insn (gen_mov_tls_ie_step2_32bit (temp2, temp, addr));
	      emit_insn (gen_tls_add_32bit (temp2, got, temp2, addr));
	      emit_insn (gen_tls_ie_load_32bit (temp3, temp2, addr));
	    }
	  else
	    {
	      emit_insn (gen_mov_tls_ie_step1 (temp, addr));
	      emit_insn (gen_mov_tls_ie_step2 (temp2, temp, addr));
	      emit_insn (gen_tls_add (temp2, got, temp2, addr));
	      emit_insn (gen_tls_ie_load (temp3, temp2, addr));
	    }

	  last =
	    emit_move_insn(ret,
			   gen_rtx_PLUS (Pmode,
					 gen_rtx_REG (Pmode,
						      THREAD_POINTER_REGNUM),
					 temp3));
	  set_unique_reg_note (last, REG_EQUAL, copy_rtx (addr));
	  break;
	}
      case TLS_MODEL_LOCAL_EXEC:
	{
	  rtx temp, temp2;
	  rtx_insn *last;

	  ret = gen_reg_rtx (Pmode);
	  temp = gen_reg_rtx (Pmode);
	  temp2 = gen_reg_rtx (Pmode);

	  if (TARGET_32BIT)
	    {
	      emit_insn (gen_mov_tls_le_step1_32bit (temp, addr));
	      emit_insn (gen_mov_tls_le_step2_32bit (temp2, temp, addr));
	    }
	  else
	    {
	      emit_insn (gen_mov_tls_le_step1 (temp, addr));
	      emit_insn (gen_mov_tls_le_step2 (temp2, temp, addr));
	    }

	  last =
	    emit_move_insn (ret,
			    gen_rtx_PLUS (Pmode,
					  gen_rtx_REG (Pmode,
						       THREAD_POINTER_REGNUM),
					  temp2));
	  set_unique_reg_note (last, REG_EQUAL, copy_rtx (addr));
	  break;
	}
      default:
	gcc_unreachable ();
      }
  else if (GET_CODE (addr) == CONST)
    {
      rtx base, offset;

      gcc_assert (GET_CODE (XEXP (addr, 0)) == PLUS);

      base = tilegx_legitimize_tls_address (XEXP (XEXP (addr, 0), 0));
      offset = XEXP (XEXP (addr, 0), 1);

      base = force_operand (base, NULL_RTX);
      ret = force_reg (Pmode, gen_rtx_PLUS (Pmode, base, offset));
    }
  else
    gcc_unreachable ();

  return ret;
}


/* Returns a register that points to ADDR, a symbolic address, by
   computing its address relative to tilegx_text_label_symbol.  */
void
tilegx_compute_pcrel_address (rtx result, rtx addr)
{
  rtx text_label_symbol = tilegx_text_label_symbol ();
  rtx text_label_rtx = tilegx_text_label_rtx ();
  rtx temp, temp2, temp3;

  temp = create_temp_reg_if_possible (Pmode, result);
  temp2 = create_temp_reg_if_possible (Pmode, result);

  if (TARGET_32BIT)
    {
      emit_insn (gen_mov_pcrel_step1_32bit (temp, addr, text_label_symbol));
      emit_insn (gen_mov_pcrel_step2_32bit (temp2, temp, addr,
					    text_label_symbol));
      emit_insn (gen_mov_pcrel_step3_32bit (result, temp2,
					    text_label_rtx,
					    addr, text_label_symbol));
    }
  else if (tilegx_cmodel == CM_LARGE_PIC)
    {
      temp3 = create_temp_reg_if_possible (Pmode, result);
      emit_insn (gen_mov_large_pcrel_step1 (temp, addr, text_label_symbol));
      emit_insn (gen_mov_large_pcrel_step2 (temp2, temp, addr,
					    text_label_symbol));
      emit_insn (gen_mov_large_pcrel_step3 (temp3, temp2, addr,
					    text_label_symbol));
      emit_insn (gen_mov_large_pcrel_step4 (result, temp3,
					    text_label_rtx,
					    addr, text_label_symbol));
    }
  else
    {
      emit_insn (gen_mov_pcrel_step1 (temp, addr, text_label_symbol));
      emit_insn (gen_mov_pcrel_step2 (temp2, temp, addr, text_label_symbol));
      emit_insn (gen_mov_pcrel_step3 (result, temp2,
				      text_label_rtx,
				      addr, text_label_symbol));
    }
}


/* Returns a register that points to the plt entry of ADDR, a symbolic
   address, by computing its address relative to
   tilegx_text_label_symbol.  */
void
tilegx_compute_pcrel_plt_address (rtx result, rtx addr)
{
  rtx text_label_symbol = tilegx_text_label_symbol ();
  rtx text_label_rtx = tilegx_text_label_rtx ();
  rtx temp, temp2, temp3;

  temp = create_temp_reg_if_possible (Pmode, result);
  temp2 = create_temp_reg_if_possible (Pmode, result);

  if (TARGET_32BIT)
    {
      emit_insn (gen_mov_plt_pcrel_step1_32bit (temp, addr,
						text_label_symbol));
      emit_insn (gen_mov_plt_pcrel_step2_32bit (temp2, temp, addr,
						text_label_symbol));
      emit_move_insn (result, gen_rtx_PLUS (Pmode, temp2, text_label_rtx));
    }
  else
    {
      temp3 = create_temp_reg_if_possible (Pmode, result);

      emit_insn (gen_mov_plt_pcrel_step1 (temp, addr, text_label_symbol));
      emit_insn (gen_mov_plt_pcrel_step2 (temp2, temp, addr,
					  text_label_symbol));
      emit_insn (gen_mov_plt_pcrel_step3 (temp3, temp2, addr,
					  text_label_symbol));
      emit_move_insn (result, gen_rtx_PLUS (Pmode, temp3, text_label_rtx));
    }
}


/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go into a reg.  This is REG if
   nonzero, otherwise we allocate register(s) as necessary.  */
static rtx
tilegx_legitimize_pic_address (rtx orig,
			       machine_mode mode ATTRIBUTE_UNUSED,
			       rtx reg)
{
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      rtx address, pic_ref;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      if (SYMBOL_REF_LOCAL_P (orig))
	{
	  /* If not during reload, allocate another temp reg here for
	     loading in the address, so that these instructions can be
	     optimized properly.  */
	  rtx temp_reg = create_temp_reg_if_possible (Pmode, reg);
	  tilegx_compute_pcrel_address (temp_reg, orig);

	  /* Note: this is conservative.  We use the text_label but we
	     don't use the pic_offset_table.  However, in some cases
	     we may need the pic_offset_table (see
	     tilegx_fixup_pcrel_references).  */
	  crtl->uses_pic_offset_table = 1;

	  address = temp_reg;

	  emit_move_insn (reg, address);
	  return reg;
	}
      else
	{
	  /* If not during reload, allocate another temp reg here for
	     loading in the address, so that these instructions can be
	     optimized properly.  */
	  rtx temp_reg = create_temp_reg_if_possible (Pmode, reg);

	  gcc_assert (flag_pic);
	  if (flag_pic == 1)
	    {
	      if (TARGET_32BIT)
		{
		  emit_insn (gen_add_got16_32bit (temp_reg,
						  tilegx_got_rtx (),
						  orig));
		}
	      else
		{
		  emit_insn (gen_add_got16 (temp_reg,
					    tilegx_got_rtx (), orig));
		}
	    }
	  else
	    {
	      rtx temp_reg2 = create_temp_reg_if_possible (Pmode, reg);
	      rtx temp_reg3 = create_temp_reg_if_possible (Pmode, reg);
	      if (TARGET_32BIT)
		{
		  emit_insn (gen_mov_got32_step1_32bit (temp_reg3, orig));
		  emit_insn (gen_mov_got32_step2_32bit
			     (temp_reg2, temp_reg3, orig));
		}
	      else
		{
		  emit_insn (gen_mov_got32_step1 (temp_reg3, orig));
		  emit_insn (gen_mov_got32_step2 (temp_reg2, temp_reg3,
						  orig));
		}
	      emit_move_insn (temp_reg,
			      gen_rtx_PLUS (Pmode,
					    tilegx_got_rtx (), temp_reg2));
	    }

	  address = temp_reg;

	  pic_ref = gen_const_mem (Pmode, address);
	  crtl->uses_pic_offset_table = 1;
	  emit_move_insn (reg, pic_ref);
	  /* The following put a REG_EQUAL note on this insn, so that
	     it can be optimized by loop.  But it causes the label to
	     be optimized away.  */
	  /* set_unique_reg_note (insn, REG_EQUAL, orig); */
	  return reg;
	}
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == tilegx_got_rtx ())
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);
      base = tilegx_legitimize_pic_address (XEXP (XEXP (orig, 0), 0),
					    Pmode, reg);
      offset = tilegx_legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					      base == reg ? 0 : reg);

      if (CONST_INT_P (offset))
	{
	  if (can_create_pseudo_p ())
	    offset = force_reg (Pmode, offset);
	  else
	    /* If we reach here, then something is seriously wrong.  */
	    gcc_unreachable ();
	}

      if (can_create_pseudo_p ())
	return force_reg (Pmode, gen_rtx_PLUS (Pmode, base, offset));
      else
	gcc_unreachable ();
    }
  else if (GET_CODE (orig) == LABEL_REF)
    {
      rtx address;
      rtx temp_reg;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      /* If not during reload, allocate another temp reg here for
	 loading in the address, so that these instructions can be
	 optimized properly.  */
      temp_reg = create_temp_reg_if_possible (Pmode, reg);
      tilegx_compute_pcrel_address (temp_reg, orig);

      /* Note: this is conservative.  We use the text_label but we
	 don't use the pic_offset_table.  */
      crtl->uses_pic_offset_table = 1;

      address = temp_reg;

      emit_move_insn (reg, address);

      return reg;
    }

  return orig;
}


/* Implement TARGET_LEGITIMIZE_ADDRESS.  */
static rtx
tilegx_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			   machine_mode mode)
{
  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
      && symbolic_operand (x, Pmode) && tilegx_tls_referenced_p (x))
    {
      return tilegx_legitimize_tls_address (x);
    }
  else if (flag_pic)
    {
      return tilegx_legitimize_pic_address (x, mode, 0);
    }
  else
    return x;
}


/* Implement TARGET_DELEGITIMIZE_ADDRESS.  */
static rtx
tilegx_delegitimize_address (rtx x)
{
  x = delegitimize_mem_from_attrs (x);

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == UNSPEC)
    {
      switch (XINT (XEXP (x, 0), 1))
	{
	  case UNSPEC_HW0:
	  case UNSPEC_HW1:
	  case UNSPEC_HW2:
	  case UNSPEC_HW3:
	  case UNSPEC_HW0_LAST:
	  case UNSPEC_HW1_LAST:
	  case UNSPEC_HW2_LAST:
	  case UNSPEC_HW0_PCREL:
	  case UNSPEC_HW1_PCREL:
	  case UNSPEC_HW1_LAST_PCREL:
	  case UNSPEC_HW2_LAST_PCREL:
	  case UNSPEC_HW0_PLT_PCREL:
	  case UNSPEC_HW1_PLT_PCREL:
	  case UNSPEC_HW1_LAST_PLT_PCREL:
	  case UNSPEC_HW2_LAST_PLT_PCREL:
	  case UNSPEC_HW0_GOT:
	  case UNSPEC_HW0_LAST_GOT:
  	  case UNSPEC_HW1_LAST_GOT:
  	  case UNSPEC_HW0_TLS_GD:
  	  case UNSPEC_HW1_LAST_TLS_GD:
  	  case UNSPEC_HW0_TLS_IE:
  	  case UNSPEC_HW1_LAST_TLS_IE:
  	  case UNSPEC_HW0_TLS_LE:
  	  case UNSPEC_HW1_LAST_TLS_LE:
	    x = XVECEXP (XEXP (x, 0), 0, 0);
	  break;
	}
    }

  return x;
}


/* Emit code to load the PIC register.  */
static void
load_pic_register (bool delay_pic_helper ATTRIBUTE_UNUSED)
{
  int orig_flag_pic = flag_pic;

  rtx got_symbol = tilegx_got_symbol ();
  rtx text_label_symbol = tilegx_text_label_symbol ();
  rtx text_label_rtx = tilegx_text_label_rtx ();
  flag_pic = 0;

  if (TARGET_32BIT)
    {
      emit_insn (gen_insn_lnk_and_label_32bit (text_label_rtx,
					       text_label_symbol));
    }
  else
    {
      emit_insn (gen_insn_lnk_and_label (text_label_rtx, text_label_symbol));
    }

  tilegx_compute_pcrel_address (tilegx_got_rtx (), got_symbol);

  flag_pic = orig_flag_pic;

  /* Need to emit this whether or not we obey regdecls, since
     setjmp/longjmp can cause life info to screw up.  ??? In the case
     where we don't obey regdecls, this is not sufficient since we may
     not fall out the bottom.  */
  emit_use (tilegx_got_rtx ());
}


/* Return the simd variant of the constant NUM of mode MODE, by
   replicating it to fill an interger of mode DImode.  NUM is first
   truncated to fit in MODE.  */
rtx
tilegx_simd_int (rtx num, machine_mode mode)
{
  HOST_WIDE_INT n = 0;

  gcc_assert (CONST_INT_P (num));

  n = INTVAL (num);

  switch (mode)
    {
    case E_QImode:
      n = 0x0101010101010101LL * (n & 0x000000FF);
      break;
    case E_HImode:
      n = 0x0001000100010001LL * (n & 0x0000FFFF);
      break;
    case E_SImode:
      n = 0x0000000100000001LL * (n & 0xFFFFFFFF);
      break;
    case E_DImode:
      break;
    default:
      gcc_unreachable ();
    }

  return GEN_INT (n);
}


/* Returns true iff VAL can be moved into a register in one
   instruction.  And if it can, it emits the code to move the constant
   into DEST_REG.

   If THREE_WIDE_ONLY is true, this insists on an instruction that
   works in a bundle containing three instructions.  */
static bool
expand_set_cint64_one_inst (rtx dest_reg,
			    HOST_WIDE_INT val, bool three_wide_only)
{
  if (val == trunc_int_for_mode (val, QImode))
    {
      /* Success! */
      emit_move_insn (dest_reg, GEN_INT (val));
      return true;
    }
  else if (!three_wide_only)
    {
      /* Test for the following constraints: J, K, N, P.  We avoid
	 generating an rtx and using existing predicates because we
	 can be testing and rejecting a lot of constants, and GEN_INT
	 is O(N).  */
      if ((val >= -32768 && val <= 65535)
	  || ((val == (val & 0xFF) * 0x0101010101010101LL))
	  || (val == ((trunc_int_for_mode (val, QImode) & 0xFFFF)
		      * 0x0001000100010001LL)))
	{
	  emit_move_insn (dest_reg, GEN_INT (val));
	  return true;
	}
    }

  return false;
}


/* Implement DImode rotatert.  */
static HOST_WIDE_INT
rotate_right (HOST_WIDE_INT n, int count)
{
  unsigned HOST_WIDE_INT x = n & 0xFFFFFFFFFFFFFFFFULL;
  if (count == 0)
    return x;
  return ((x >> count) | (x << (64 - count))) & 0xFFFFFFFFFFFFFFFFULL;
}


/* Return true iff n contains exactly one contiguous sequence of 1
   bits, possibly wrapping around from high bits to low bits.  */
bool
tilegx_bitfield_operand_p (HOST_WIDE_INT n, int *first_bit, int *last_bit)
{
  int i;

  if (n == 0)
    return false;

  for (i = 0; i < 64; i++)
    {
      unsigned HOST_WIDE_INT x = rotate_right (n, i);
      if (!(x & 1))
	continue;

      /* See if x is a power of two minus one, i.e. only consecutive 1
	 bits starting from bit 0.  */
      if ((x & (x + 1)) == 0)
	{
	  if (first_bit != NULL)
	    *first_bit = i;
	  if (last_bit != NULL)
	    *last_bit = (i + exact_log2 (x ^ (x >> 1))) & 63;

	  return true;
	}
    }

  return false;
}


/* Create code to move the CONST_INT value in src_val to dest_reg.  */
static void
expand_set_cint64 (rtx dest_reg, rtx src_val)
{
  HOST_WIDE_INT val;
  int leading_zeroes, trailing_zeroes;
  int three_wide_only;
  int shift, ins_shift, zero_cluster_shift;
  rtx temp, subreg;

  gcc_assert (CONST_INT_P (src_val));
  val = trunc_int_for_mode (INTVAL (src_val), GET_MODE (dest_reg));

  /* See if we can generate the constant in one instruction.  */
  if (expand_set_cint64_one_inst (dest_reg, val, false))
    return;

  /* Force the destination to DImode so we can use DImode instructions
     to create it.  This both allows instructions like rotl, and
     certain efficient 3-wide instructions.  */
  subreg = simplify_gen_subreg (DImode, dest_reg, GET_MODE (dest_reg), 0);
  gcc_assert (subreg != NULL);
  dest_reg = subreg;

  temp = create_temp_reg_if_possible (DImode, dest_reg);

  leading_zeroes = 63 - floor_log2 (val & 0xFFFFFFFFFFFFFFFFULL);
  trailing_zeroes = exact_log2 (val & -val);

  /* First try all three-wide instructions that generate a constant
     (i.e. movei) followed by various shifts and rotates. If none of
     those work, try various two-wide ways of generating a constant
     followed by various shifts and rotates.  */
  for (three_wide_only = 1; three_wide_only >= 0; three_wide_only--)
    {
      int count;

      if (expand_set_cint64_one_inst (temp, val >> trailing_zeroes,
				      three_wide_only))
	{
	  /* 0xFFFFFFFFFFFFA500 becomes:
	     movei temp, 0xFFFFFFFFFFFFFFA5
	     shli dest, temp, 8  */
	  emit_move_insn (dest_reg,
			  gen_rtx_ASHIFT (DImode, temp,
					  GEN_INT (trailing_zeroes)));
	  return;
	}

      if (expand_set_cint64_one_inst (temp, val << leading_zeroes,
				      three_wide_only))
	{
	  /* 0x7FFFFFFFFFFFFFFF becomes:
	     movei temp, -2
	     shrui dest, temp, 1  */
	  emit_move_insn (dest_reg,
			  gen_rtx_LSHIFTRT (DImode, temp,
					    GEN_INT (leading_zeroes)));
	  return;
	}

      /* Try rotating a one-instruction immediate.  */
      for (count = 1; count < 64; count++)
	{
	  HOST_WIDE_INT r = rotate_right (val, count);
	  if (expand_set_cint64_one_inst (temp, r, three_wide_only))
	    {
	      /* 0xFFFFFFFFFFA5FFFF becomes:
		 movei temp, 0xFFFFFFFFFFFFFFA5
		 rotli dest, temp, 16  */
	      emit_move_insn (dest_reg,
			      gen_rtx_ROTATE (DImode, temp, GEN_INT (count)));
	      return;
	    }
	}
    }

  /* There are two cases here to produce a large constant.
     In the most general case, we do this:

     moveli x, hw3(NUM)
     shl16insli x, x, hw2(NUM)
     shl16insli x, x, hw1(NUM)
     shl16insli x, x, hw0(NUM)

     However, we can sometimes do better.  shl16insli is a poor way to
     insert 16 zero bits, because simply shifting left by 16 has more
     bundling freedom.  So if we see any contiguous aligned sequence
     of 16 or more zero bits (below the highest set bit), it is always
     more efficient to materialize the bits above the zero bits, then
     left shift to put in the zeroes, then insert whatever bits
     remain.  For example, we might end up with:

     movei x, NUM >> (37 + 16)
     shli x, x, 37
     shl16insli x, x, hw0(NUM)      */

  zero_cluster_shift = -1;

  for (shift = 0; shift < 48 - leading_zeroes; shift += 16)
    {
      HOST_WIDE_INT x = val >> shift;

      /* Find the least significant group of 16 aligned zero bits.  */
      if ((x & 0xFFFF) == 0x0000)
	{
	  /* Grab any following zero bits as well.  */
	  zero_cluster_shift = exact_log2 (x & -x);
	  shift += zero_cluster_shift;
	  break;
	}
    }

  if (zero_cluster_shift >= 0)
    {
      unsigned HOST_WIDE_INT leftover;

      /* Recursively create the constant above the lowest 16 zero
	 bits.  */
      expand_set_cint64 (temp, GEN_INT (val >> shift));

      /* See if we can easily insert the remaining bits, or if we need
	 to fall through to the more general case.  */
      leftover = val - ((val >> shift) << shift);
      if (leftover == 0)
	{
	  /* A simple left shift is enough.  */
	  emit_move_insn (dest_reg,
			  gen_rtx_ASHIFT (DImode, temp, GEN_INT (shift)));
	  return;
	}
      else if (leftover <= 32767)
	{
	  /* Left shift into position then add in the leftover.  */
	  rtx temp2 = create_temp_reg_if_possible (DImode, temp);
	  emit_move_insn (temp2,
			  gen_rtx_ASHIFT (DImode, temp, GEN_INT (shift)));
	  emit_move_insn (dest_reg,
			  gen_rtx_PLUS (DImode, temp2, GEN_INT (leftover)));
	  return;
	}
      else
	{
	  /* Shift in the batch of >= 16 zeroes we detected earlier.
	     After this, shift will be aligned mod 16 so the final
	     loop can use shl16insli.  */
	  rtx temp2 = create_temp_reg_if_possible (DImode, temp);
	  rtx shift_count_rtx = GEN_INT (zero_cluster_shift);

	  emit_move_insn (temp2,
			  gen_rtx_ASHIFT (DImode, temp, shift_count_rtx));

	  shift -= zero_cluster_shift;
	  temp = temp2;
	}
    }
  else
    {
      /* Set as many high 16-bit blocks as we can with a single
	 instruction.  We'll insert the remaining 16-bit blocks
	 below.  */
      for (shift = 16;; shift += 16)
	{
	  gcc_assert (shift < 64);
	  if (expand_set_cint64_one_inst (temp, val >> shift, false))
	    break;
	}
    }

  /* At this point, temp == val >> shift, shift % 16 == 0, and we
     still need to insert any bits of 'val' below 'shift'. Those bits
     are guaranteed to not have 16 contiguous zeroes.  */

  gcc_assert ((shift & 15) == 0);

  for (ins_shift = shift - 16; ins_shift >= 0; ins_shift -= 16)
    {
      rtx result;
      HOST_WIDE_INT bits = (val >> ins_shift) & 0xFFFF;
      gcc_assert (bits != 0);

      /* On the last iteration we need to store into dest_reg.  */
      if (ins_shift == 0)
	result = dest_reg;
      else
	result = create_temp_reg_if_possible (DImode, dest_reg);

      emit_insn (gen_insn_shl16insli (result, temp, GEN_INT (bits)));

      temp = result;
    }
}


/* Load OP1, a 64-bit constant, into OP0, a register.  We know it
   can't be done in one insn when we get here, the move expander
   guarantees this.  */
void
tilegx_expand_set_const64 (rtx op0, rtx op1)
{
  if (CONST_INT_P (op1))
    {
      /* TODO: I don't know if we want to split large constants
	 now, or wait until later (with a define_split).

	 Does splitting early help CSE?  Does it harm other
	 optimizations that might fold loads?  */
      expand_set_cint64 (op0, op1);
    }
  else
    {
      rtx temp = create_temp_reg_if_possible (Pmode, op0);

      if (TARGET_32BIT)
	{
	  /* Generate the 2-insn sequence to materialize a symbolic
	     address.  */
	  emit_insn (gen_mov_address_32bit_step1 (temp, op1));
	  emit_insn (gen_mov_address_32bit_step2 (op0, temp, op1));
	}
      else
	{
	  /* Generate the 3-insn sequence to materialize a symbolic
	     address.  Note that this assumes that virtual addresses
	     fit in 48 signed bits, which is currently true.  */
	  rtx temp2 = create_temp_reg_if_possible (Pmode, op0);
	  emit_insn (gen_mov_address_step1 (temp, op1));
	  emit_insn (gen_mov_address_step2 (temp2, temp, op1));
	  emit_insn (gen_mov_address_step3 (op0, temp2, op1));
	}
    }
}


/* Expand a move instruction.  Return true if all work is done.  */
bool
tilegx_expand_mov (machine_mode mode, rtx *operands)
{
  /* Handle sets of MEM first.  */
  if (MEM_P (operands[0]))
    {
      if (can_create_pseudo_p ())
	operands[0] = validize_mem (operands[0]);

      if (reg_or_0_operand (operands[1], mode))
	return false;

      if (!reload_in_progress)
	operands[1] = force_reg (mode, operands[1]);
    }

  /* Fixup TLS cases.  */
  if (CONSTANT_P (operands[1]) && tilegx_tls_referenced_p (operands[1]))
    {
      operands[1] = tilegx_legitimize_tls_address (operands[1]);
      return false;
    }

  /* Fixup PIC cases.  */
  if (flag_pic && CONSTANT_P (operands[1]))
    {
      if (tilegx_pic_address_needs_scratch (operands[1]))
	operands[1] = tilegx_legitimize_pic_address (operands[1], mode, 0);

      if (symbolic_operand (operands[1], mode))
	{
	  operands[1] = tilegx_legitimize_pic_address (operands[1],
						       mode,
						       (reload_in_progress ?
							operands[0] :
							NULL_RTX));
	  return false;
	}
    }

  /* Accept non-constants and valid constants unmodified.  */
  if (!CONSTANT_P (operands[1]) || move_operand (operands[1], mode))
    return false;

  /* Split large integers.  */
  tilegx_expand_set_const64 (operands[0], operands[1]);
  return true;
}


/* Expand unaligned loads.  */
void
tilegx_expand_unaligned_load (rtx dest_reg, rtx mem, HOST_WIDE_INT bitsize,
			      HOST_WIDE_INT bit_offset, bool sign)
{
  machine_mode mode;
  rtx addr_lo, addr_hi;
  rtx mem_lo, mem_hi, hi;
  rtx mema, wide_result;
  int last_byte_offset;
  HOST_WIDE_INT byte_offset = bit_offset / BITS_PER_UNIT;

  mode = GET_MODE (dest_reg);

  if (bitsize == 2 * BITS_PER_UNIT && (bit_offset % BITS_PER_UNIT) == 0)
    {
      rtx mem_left, mem_right;
      rtx left = gen_reg_rtx (mode);

      /* When just loading a two byte value, we can load the two bytes
	 individually and combine them efficiently.  */

      mem_lo = adjust_address (mem, QImode, byte_offset);
      mem_hi = adjust_address (mem, QImode, byte_offset + 1);

      if (BYTES_BIG_ENDIAN)
	{
	  mem_left = mem_lo;
	  mem_right = mem_hi;
	}
      else
	{
	  mem_left = mem_hi;
	  mem_right = mem_lo;
	}

      if (sign)
	{
	  /* Do a signed load of the second byte and use bfins to set
	     the high bits of the result.  */
	  emit_insn (gen_zero_extendqidi2 (gen_lowpart (DImode, dest_reg),
					   mem_right));
	  emit_insn (gen_extendqidi2 (gen_lowpart (DImode, left), mem_left));
	  emit_insn (gen_insv (gen_lowpart (DImode, dest_reg),
			       GEN_INT (64 - 8), GEN_INT (8),
			       gen_lowpart (DImode, left)));
	}
      else
	{
	  /* Do two unsigned loads and use v1int_l to interleave
	     them.  */
	  rtx right = gen_reg_rtx (mode);
	  emit_insn (gen_zero_extendqidi2 (gen_lowpart (DImode, right),
					   mem_right));
	  emit_insn (gen_zero_extendqidi2 (gen_lowpart (DImode, left),
					   mem_left));
	  emit_insn (gen_insn_v1int_l (gen_lowpart (DImode, dest_reg),
				       gen_lowpart (DImode, left),
				       gen_lowpart (DImode, right)));
	}

      return;
    }

  mema = XEXP (mem, 0);

  /* AND addresses cannot be in any alias set, since they may
     implicitly alias surrounding code.  Ideally we'd have some alias
     set that covered all types except those with alignment 8 or
     higher.  */
  addr_lo = force_reg (Pmode, plus_constant (Pmode, mema, byte_offset));
  mem_lo = change_address (mem, mode,
			   gen_rtx_AND (GET_MODE (mema), addr_lo,
					GEN_INT (-8)));
  set_mem_alias_set (mem_lo, 0);

  /* Load the high word at an address that will not fault if the low
     address is aligned and at the very end of a page.  */
  last_byte_offset = (bit_offset + bitsize - 1) / BITS_PER_UNIT;
  addr_hi = force_reg (Pmode, plus_constant (Pmode, mema, last_byte_offset));
  mem_hi = change_address (mem, mode,
			   gen_rtx_AND (GET_MODE (mema), addr_hi,
					GEN_INT (-8)));
  set_mem_alias_set (mem_hi, 0);

  if (bitsize == 64)
    {
      addr_lo = make_safe_from (addr_lo, dest_reg);
      wide_result = dest_reg;
    }
  else
    {
      wide_result = gen_reg_rtx (mode);
    }

  /* Load hi first in case dest_reg is used in mema.  */
  hi = gen_reg_rtx (mode);
  emit_move_insn (hi, mem_hi);
  emit_move_insn (wide_result, mem_lo);

  emit_insn (gen_insn_dblalign (gen_lowpart (DImode, wide_result),
				gen_lowpart (DImode, wide_result),
				gen_lowpart (DImode, hi), addr_lo));

  if (bitsize != 64)
    {
      rtx extracted =
	extract_bit_field (gen_lowpart (DImode, wide_result),
			   bitsize, bit_offset % BITS_PER_UNIT,
			   !sign, gen_lowpart (DImode, dest_reg),
			   DImode, DImode, false, NULL);

      if (extracted != dest_reg)
	emit_move_insn (dest_reg, gen_lowpart (DImode, extracted));
    }
}


/* Expand unaligned stores.  */
static void
tilegx_expand_unaligned_store (rtx mem, rtx src, HOST_WIDE_INT bitsize,
			       HOST_WIDE_INT bit_offset)
{
  HOST_WIDE_INT byte_offset = bit_offset / BITS_PER_UNIT;
  HOST_WIDE_INT bytesize = bitsize / BITS_PER_UNIT;
  HOST_WIDE_INT shift_init, shift_increment, shift_amt;
  HOST_WIDE_INT i;
  rtx mem_addr;
  rtx store_val;

  shift_init = BYTES_BIG_ENDIAN ? (bitsize - BITS_PER_UNIT) : 0;
  shift_increment = BYTES_BIG_ENDIAN ? -BITS_PER_UNIT : BITS_PER_UNIT;

  for (i = 0, shift_amt = shift_init;
       i < bytesize;
       i++, shift_amt += shift_increment)
    {
      mem_addr = adjust_address (mem, QImode, byte_offset + i);

      if (shift_amt)
	{
	  store_val = expand_simple_binop (DImode, LSHIFTRT,
					   gen_lowpart (DImode, src),
					   GEN_INT (shift_amt), NULL, 1,
					   OPTAB_LIB_WIDEN);
	  store_val = gen_lowpart (QImode, store_val);
	}
      else
	{
	  store_val = gen_lowpart (QImode, src);
	}

      emit_move_insn (mem_addr, store_val);
    }
}


/* Implement the movmisalign patterns.  One of the operands is a
   memory that is not naturally aligned.  Emit instructions to load
   it.  */
void
tilegx_expand_movmisalign (machine_mode mode, rtx *operands)
{
  if (MEM_P (operands[1]))
    {
      rtx tmp;

      if (register_operand (operands[0], mode))
	tmp = operands[0];
      else
	tmp = gen_reg_rtx (mode);

      tilegx_expand_unaligned_load (tmp, operands[1], GET_MODE_BITSIZE (mode),
				    0, true);

      if (tmp != operands[0])
	emit_move_insn (operands[0], tmp);
    }
  else if (MEM_P (operands[0]))
    {
      if (!reg_or_0_operand (operands[1], mode))
	operands[1] = force_reg (mode, operands[1]);

      tilegx_expand_unaligned_store (operands[0], operands[1],
				     GET_MODE_BITSIZE (mode), 0);
    }
  else
    gcc_unreachable ();

}


/* Implement the allocate_stack pattern (alloca).  */
void
tilegx_allocate_stack (rtx op0, rtx op1)
{
  /* Technically the correct way to initialize chain_loc is with
   * gen_frame_mem() instead of gen_rtx_MEM(), but gen_frame_mem()
   * sets the alias_set to that of a frame reference.  Some of our
   * tests rely on some unsafe assumption about when the chaining
   * update is done, we need to be conservative about reordering the
   * chaining instructions.
   */
  rtx fp_addr = gen_reg_rtx (Pmode);
  rtx fp_value = gen_reg_rtx (Pmode);
  rtx fp_loc;

  emit_move_insn (fp_addr, gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					 GEN_INT (UNITS_PER_WORD)));

  fp_loc = gen_frame_mem (Pmode, fp_addr);

  emit_move_insn (fp_value, fp_loc);

  op1 = force_reg (Pmode, op1);

  emit_move_insn (stack_pointer_rtx,
		  gen_rtx_MINUS (Pmode, stack_pointer_rtx, op1));

  emit_move_insn (fp_addr, gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					 GEN_INT (UNITS_PER_WORD)));

  fp_loc = gen_frame_mem (Pmode, fp_addr);

  emit_move_insn (fp_loc, fp_value);

  emit_move_insn (op0, virtual_stack_dynamic_rtx);
}



/* Multiplies */


/* Returns the insn_code in ENTRY.  */
static enum insn_code
tilegx_multiply_get_opcode (const struct tilegx_multiply_insn_seq_entry
			    *entry)
{
  return tilegx_multiply_insn_seq_decode_opcode[entry->compressed_opcode];
}


/* Returns the length of the 'op' array.  */
static int
tilegx_multiply_get_num_ops (const struct tilegx_multiply_insn_seq *seq)
{
  /* The array either uses all of its allocated slots or is terminated
     by a bogus opcode. Either way, the array size is the index of the
     last valid opcode plus one.  */
  int i;
  for (i = tilegx_multiply_insn_seq_MAX_OPERATIONS - 1; i >= 0; i--)
    if (tilegx_multiply_get_opcode (&seq->op[i]) != CODE_FOR_nothing)
      return i + 1;

  /* An empty array is not allowed.  */
  gcc_unreachable ();
}


/* We precompute a number of expression trees for multiplying by
   constants.  This generates code for such an expression tree by
   walking through the nodes in the tree (which are conveniently
   pre-linearized) and emitting an instruction for each one.  */
static void
tilegx_expand_constant_multiply_given_sequence (rtx result, rtx src,
						const struct
						tilegx_multiply_insn_seq *seq)
{
  int i;
  int num_ops;

  /* Keep track of the subexpressions computed so far, so later
     instructions can refer to them.  We seed the array with zero and
     the value being multiplied.  */
  int num_subexprs = 2;
  rtx subexprs[tilegx_multiply_insn_seq_MAX_OPERATIONS + 2];
  subexprs[0] = const0_rtx;
  subexprs[1] = src;

  /* Determine how many instructions we are going to generate.  */
  num_ops = tilegx_multiply_get_num_ops (seq);
  gcc_assert (num_ops > 0
	      && num_ops <= tilegx_multiply_insn_seq_MAX_OPERATIONS);

  for (i = 0; i < num_ops; i++)
    {
      const struct tilegx_multiply_insn_seq_entry *entry = &seq->op[i];

      /* Figure out where to store the output of this instruction.  */
      const bool is_last_op = (i + 1 == num_ops);
      rtx out = is_last_op ? result : gen_reg_rtx (DImode);

      enum insn_code opcode = tilegx_multiply_get_opcode (entry);
      if (opcode == CODE_FOR_ashldi3)
	{
	  /* Handle shift by immediate. This is a special case because
	     the meaning of the second operand is a constant shift
	     count rather than an operand index.  */

	  /* Make sure the shift count is in range. Zero should not
	     happen.  */
	  const int shift_count = entry->rhs;
	  gcc_assert (shift_count > 0 && shift_count < 64);

	  /* Emit the actual instruction.  */
	  emit_insn (GEN_FCN (opcode)
		     (out, subexprs[entry->lhs],
		      gen_rtx_CONST_INT (DImode, shift_count)));
	}
      else
	{
	  /* Handle a normal two-operand instruction, such as add or
	     shl1add.  */

	  /* Make sure we are referring to a previously computed
	     subexpression.  */
	  gcc_assert (entry->rhs < num_subexprs);

	  /* Emit the actual instruction.  */
	  emit_insn (GEN_FCN (opcode)
		     (out, subexprs[entry->lhs], subexprs[entry->rhs]));
	}

      /* Record this subexpression for use by later expressions.  */
      subexprs[num_subexprs++] = out;
    }
}


/* bsearch helper function.  */
static int
tilegx_compare_multipliers (const void *key, const void *t)
{
  long long delta =
    (*(const long long *) key
     - ((const struct tilegx_multiply_insn_seq *) t)->multiplier);
  return (delta < 0) ? -1 : (delta > 0);
}


/* Returns the tilegx_multiply_insn_seq for multiplier, or NULL if none
   exists.  */
static const struct tilegx_multiply_insn_seq *
tilegx_find_multiply_insn_seq_for_constant (long long multiplier)
{
  return ((const struct tilegx_multiply_insn_seq *)
	  bsearch (&multiplier, tilegx_multiply_insn_seq_table,
		   tilegx_multiply_insn_seq_table_size,
		   sizeof tilegx_multiply_insn_seq_table[0],
		   tilegx_compare_multipliers));
}


/* Try to a expand constant multiply in DImode by looking it up in a
   precompiled table.  OP0 is the result operand, OP1 is the source
   operand, and MULTIPLIER is the value of the constant.  Return true
   if it succeeds.  */
static bool
tilegx_expand_const_muldi (rtx op0, rtx op1, long long multiplier)
{
  /* See if we have precomputed an efficient way to multiply by this
     constant.  */
  const struct tilegx_multiply_insn_seq *seq =
    tilegx_find_multiply_insn_seq_for_constant (multiplier);
  if (seq != NULL)
    {
      tilegx_expand_constant_multiply_given_sequence (op0, op1, seq);
      return true;
    }
  else
    return false;
}


/* Expand the muldi pattern.  */
bool
tilegx_expand_muldi (rtx op0, rtx op1, rtx op2)
{
  if (CONST_INT_P (op2))
    {
      HOST_WIDE_INT n = trunc_int_for_mode (INTVAL (op2), DImode);
      return tilegx_expand_const_muldi (op0, op1, n);
    }
  return false;
}


/* Expand a high multiply pattern in DImode.  RESULT, OP1, OP2 are the
   operands, and SIGN is true if it's a signed multiply, and false if
   it's an unsigned multiply.  */
static void
tilegx_expand_high_multiply (rtx result, rtx op1, rtx op2, bool sign)
{
  rtx tmp0 = gen_reg_rtx (DImode);
  rtx tmp1 = gen_reg_rtx (DImode);
  rtx tmp2 = gen_reg_rtx (DImode);
  rtx tmp3 = gen_reg_rtx (DImode);
  rtx tmp4 = gen_reg_rtx (DImode);
  rtx tmp5 = gen_reg_rtx (DImode);
  rtx tmp6 = gen_reg_rtx (DImode);
  rtx tmp7 = gen_reg_rtx (DImode);
  rtx tmp8 = gen_reg_rtx (DImode);
  rtx tmp9 = gen_reg_rtx (DImode);
  rtx tmp10 = gen_reg_rtx (DImode);
  rtx tmp11 = gen_reg_rtx (DImode);
  rtx tmp12 = gen_reg_rtx (DImode);
  rtx tmp13 = gen_reg_rtx (DImode);
  rtx result_lo = gen_reg_rtx (DImode);

  if (sign)
    {
      emit_insn (gen_insn_mul_hs_lu (tmp0, op1, op2));
      emit_insn (gen_insn_mul_hs_lu (tmp1, op2, op1));
      emit_insn (gen_insn_mul_lu_lu (tmp2, op1, op2));
      emit_insn (gen_insn_mul_hs_hs (tmp3, op1, op2));
    }
  else
    {
      emit_insn (gen_insn_mul_hu_lu (tmp0, op1, op2));
      emit_insn (gen_insn_mul_hu_lu (tmp1, op2, op1));
      emit_insn (gen_insn_mul_lu_lu (tmp2, op1, op2));
      emit_insn (gen_insn_mul_hu_hu (tmp3, op1, op2));
    }

  emit_move_insn (tmp4, (gen_rtx_ASHIFT (DImode, tmp0, GEN_INT (32))));

  emit_move_insn (tmp5, (gen_rtx_ASHIFT (DImode, tmp1, GEN_INT (32))));

  emit_move_insn (tmp6, (gen_rtx_PLUS (DImode, tmp4, tmp5)));
  emit_move_insn (result_lo, (gen_rtx_PLUS (DImode, tmp2, tmp6)));

  emit_move_insn (tmp7, gen_rtx_LTU (DImode, tmp6, tmp4));
  emit_move_insn (tmp8, gen_rtx_LTU (DImode, result_lo, tmp2));

  if (sign)
    {
      emit_move_insn (tmp9, (gen_rtx_ASHIFTRT (DImode, tmp0, GEN_INT (32))));
      emit_move_insn (tmp10, (gen_rtx_ASHIFTRT (DImode, tmp1, GEN_INT (32))));
    }
  else
    {
      emit_move_insn (tmp9, (gen_rtx_LSHIFTRT (DImode, tmp0, GEN_INT (32))));
      emit_move_insn (tmp10, (gen_rtx_LSHIFTRT (DImode, tmp1, GEN_INT (32))));
    }

  emit_move_insn (tmp11, (gen_rtx_PLUS (DImode, tmp3, tmp7)));
  emit_move_insn (tmp12, (gen_rtx_PLUS (DImode, tmp8, tmp9)));
  emit_move_insn (tmp13, (gen_rtx_PLUS (DImode, tmp11, tmp12)));
  emit_move_insn (result, (gen_rtx_PLUS (DImode, tmp13, tmp10)));
}


/* Implement smuldi3_highpart.  */
void
tilegx_expand_smuldi3_highpart (rtx op0, rtx op1, rtx op2)
{
  tilegx_expand_high_multiply (op0, op1, op2, true);
}


/* Implement umuldi3_highpart.  */
void
tilegx_expand_umuldi3_highpart (rtx op0, rtx op1, rtx op2)
{
  tilegx_expand_high_multiply (op0, op1, op2, false);
}



/* Compare and branches  */

/* Produce the rtx yielding a bool for a floating point
   comparison.  */
static bool
tilegx_emit_fp_setcc (rtx res, enum rtx_code code, machine_mode mode,
		      rtx op0, rtx op1)
{
  /* TODO: Certain compares again constants can be done using entirely
     integer operations. But you have to get the special cases right
     e.g. NaN, +0 == -0, etc.  */

  rtx flags;
  int flag_index;
  rtx a = force_reg (DImode, gen_lowpart (DImode, op0));
  rtx b = force_reg (DImode, gen_lowpart (DImode, op1));

  flags = gen_reg_rtx (DImode);

  if (mode == SFmode)
    {
      emit_insn (gen_insn_fsingle_add1 (flags, a, b));
    }
  else
    {
      gcc_assert (mode == DFmode);
      emit_insn (gen_insn_fdouble_add_flags (flags, a, b));
    }

  switch (code)
    {
    case EQ: flag_index = 30; break;
    case NE: flag_index = 31; break;
    case LE: flag_index = 27; break;
    case LT: flag_index = 26; break;
    case GE: flag_index = 29; break;
    case GT: flag_index = 28; break;
    default: gcc_unreachable ();
    }

  gcc_assert (GET_MODE (res) == DImode);
  emit_move_insn (res, gen_rtx_ZERO_EXTRACT (DImode, flags, GEN_INT (1),
					     GEN_INT (flag_index)));
  return true;
}


/* Certain simplifications can be done to make invalid setcc
   operations valid.  Return the final comparison, or NULL if we can't
   work.  */
static bool
tilegx_emit_setcc_internal (rtx res, enum rtx_code code, rtx op0, rtx op1,
			    machine_mode cmp_mode)
{
  rtx tmp;
  bool swap = false;

  if (cmp_mode == SFmode || cmp_mode == DFmode)
    return tilegx_emit_fp_setcc (res, code, cmp_mode, op0, op1);

  /* The general case: fold the comparison code to the types of
     compares that we have, choosing the branch as necessary.  */

  switch (code)
    {
    case EQ:
    case NE:
    case LE:
    case LT:
    case LEU:
    case LTU:
      /* We have these compares.  */
      break;

    case GE:
    case GT:
    case GEU:
    case GTU:
      /* We do not have these compares, so we reverse the
	 operands.  */
      swap = true;
      break;

    default:
      /* We should not have called this with any other code.  */
      gcc_unreachable ();
    }

  if (swap)
    {
      code = swap_condition (code);
      tmp = op0, op0 = op1, op1 = tmp;
    }

  if (!reg_or_0_operand (op0, cmp_mode))
    op0 = force_reg (cmp_mode, op0);

  if (!CONST_INT_P (op1) && !register_operand (op1, cmp_mode))
    op1 = force_reg (cmp_mode, op1);

  /* Return the setcc comparison.  */
  emit_insn (gen_rtx_SET (res, gen_rtx_fmt_ee (code, DImode, op0, op1)));

  return true;
}


/* Implement cstore patterns.  */
bool
tilegx_emit_setcc (rtx operands[], machine_mode cmp_mode)
{
  return
    tilegx_emit_setcc_internal (operands[0], GET_CODE (operands[1]),
				operands[2], operands[3], cmp_mode);
}


/* Return whether CODE is a signed comparison.  */
static bool
signed_compare_p (enum rtx_code code)
{
  return (code == EQ || code == NE || code == LT || code == LE
	  || code == GT || code == GE);
}


/* Generate the comparison for a DImode conditional branch.  */
static rtx
tilegx_emit_cc_test (enum rtx_code code, rtx op0, rtx op1,
		     machine_mode cmp_mode, bool eq_ne_only)
{
  enum rtx_code branch_code;
  rtx temp;

  if (cmp_mode == SFmode || cmp_mode == DFmode)
    {
      /* Compute a boolean saying whether the comparison is true.  */
      temp = gen_reg_rtx (DImode);
      tilegx_emit_setcc_internal (temp, code, op0, op1, cmp_mode);

      /* Test that flag.  */
      return gen_rtx_fmt_ee (NE, VOIDmode, temp, const0_rtx);
    }

  /* Check for a compare against zero using a comparison we can do
     directly.  */
  if (op1 == const0_rtx
      && (code == EQ || code == NE
	  || (!eq_ne_only && signed_compare_p (code))))
    {
      op0 = force_reg (cmp_mode, op0);
      return gen_rtx_fmt_ee (code, VOIDmode, op0, const0_rtx);
    }

  /* The general case: fold the comparison code to the types of
     compares that we have, choosing the branch as necessary.  */
  switch (code)
    {
    case EQ:
    case LE:
    case LT:
    case LEU:
    case LTU:
      /* We have these compares.  */
      branch_code = NE;
      break;

    case NE:
    case GE:
    case GT:
    case GEU:
    case GTU:
      /* These must be reversed (except NE, but let's
	 canonicalize).  */
      code = reverse_condition (code);
      branch_code = EQ;
      break;

    default:
      gcc_unreachable ();
    }

  if (CONST_INT_P (op1) && (!satisfies_constraint_I (op1) || code == LEU))
    {
      HOST_WIDE_INT n = INTVAL (op1);

      switch (code)
	{
	case EQ:
	  /* Subtract off the value we want to compare against and see
	     if we get zero.  This is cheaper than creating a constant
	     in a register. Except that subtracting -128 is more
	     expensive than seqi to -128, so we leave that alone.  */
	  /* ??? Don't do this when comparing against symbols,
	     otherwise we'll reduce (&x == 0x1234) to (&x-0x1234 ==
	     0), which will be declared false out of hand (at least
	     for non-weak).  */
	  if (n != -128
	      && add_operand (GEN_INT (-n), DImode)
	      && !(symbolic_operand (op0, VOIDmode)
		   || (REG_P (op0) && REG_POINTER (op0))))
	    {
	      /* TODO: Use a SIMD add immediate to hit zero for tiled
		 constants in a single instruction.  */
	      if (GET_MODE (op0) != DImode)
		{
		  /* Convert to DImode so we can use addli.  Note that
		     this will not actually generate any code because
		     sign extension from SI -> DI is a no-op.  I don't
		     know if it's safe just to make a paradoxical
		     subreg here though.  */
		  rtx temp2 = gen_reg_rtx (DImode);
		  emit_insn (gen_extendsidi2 (temp2, op0));
		  op0 = temp2;
		}
	      else
		{
		  op0 = force_reg (DImode, op0);
		}
	      temp = gen_reg_rtx (DImode);
	      emit_move_insn (temp, gen_rtx_PLUS (DImode, op0, GEN_INT (-n)));
	      return gen_rtx_fmt_ee (reverse_condition (branch_code),
				     VOIDmode, temp, const0_rtx);
	    }
	  break;

	case LEU:
	  if (n == -1)
	    break;
	  /* FALLTHRU */

	case LTU:
	  /* Change ((unsigned)x < 0x1000) into !((int)x >> 12), etc.
	     We use arithmetic shift right because it's a 3-wide op,
	     while logical shift right is not.  */
	  {
	    int first = exact_log2 (code == LTU ? n : n + 1);
	    if (first != -1)
	      {
		op0 = force_reg (cmp_mode, op0);
		temp = gen_reg_rtx (cmp_mode);
		emit_move_insn (temp,
				gen_rtx_ASHIFTRT (cmp_mode, op0,
						  GEN_INT (first)));
		return gen_rtx_fmt_ee (reverse_condition (branch_code),
				       VOIDmode, temp, const0_rtx);
	      }
	  }
	  break;

	default:
	  break;
	}
    }

  /* Compute a flag saying whether we should branch.  */
  temp = gen_reg_rtx (DImode);
  tilegx_emit_setcc_internal (temp, code, op0, op1, cmp_mode);

  /* Return the branch comparison.  */
  return gen_rtx_fmt_ee (branch_code, VOIDmode, temp, const0_rtx);
}


/* Generate the comparison for a conditional branch.  */
void
tilegx_emit_conditional_branch (rtx operands[], machine_mode cmp_mode)
{
  rtx cmp_rtx =
    tilegx_emit_cc_test (GET_CODE (operands[0]), operands[1], operands[2],
			 cmp_mode, false);
  rtx branch_rtx = gen_rtx_SET (pc_rtx,
				gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx,
						      gen_rtx_LABEL_REF
						      (VOIDmode,
						       operands[3]),
						      pc_rtx));
  emit_jump_insn (branch_rtx);
}


/* Implement the mov<mode>cc pattern.  */
rtx
tilegx_emit_conditional_move (rtx cmp)
{
  return
    tilegx_emit_cc_test (GET_CODE (cmp), XEXP (cmp, 0), XEXP (cmp, 1),
			 GET_MODE (XEXP (cmp, 0)), true);
}


/* Return true if INSN is annotated with a REG_BR_PROB note that
   indicates it's a branch that's predicted taken.  */
static bool
cbranch_predicted_p (rtx_insn *insn)
{
  rtx x = find_reg_note (insn, REG_BR_PROB, 0);

  if (x)
    {
      return profile_probability::from_reg_br_prob_note (XINT (x, 0))
	     >= profile_probability::even ();
    }

  return false;
}


/* Output assembly code for a specific branch instruction, appending
   the branch prediction flag to the opcode if appropriate.  */
static const char *
tilegx_output_simple_cbranch_with_opcode (rtx_insn *insn, const char *opcode,
					  int regop, bool reverse_predicted)
{
  static char buf[64];
  sprintf (buf, "%s%s\t%%r%d, %%l0", opcode,
	   (cbranch_predicted_p (insn) ^ reverse_predicted) ? "t" : "",
	   regop);
  return buf;
}


/* Output assembly code for a specific branch instruction, appending
   the branch prediction flag to the opcode if appropriate.  */
const char *
tilegx_output_cbranch_with_opcode (rtx_insn *insn, rtx *operands,
				   const char *opcode,
				   const char *rev_opcode, int regop)
{
  const char *branch_if_false;
  rtx taken, not_taken;
  bool is_simple_branch;

  gcc_assert (LABEL_P (operands[0]));

  is_simple_branch = true;
  if (INSN_ADDRESSES_SET_P ())
    {
      int from_addr = INSN_ADDRESSES (INSN_UID (insn));
      int to_addr = INSN_ADDRESSES (INSN_UID (operands[0]));
      int delta = to_addr - from_addr;
      is_simple_branch = IN_RANGE (delta, -524288, 524280);
    }

  if (is_simple_branch)
    {
      /* Just a simple conditional branch.  */
      return
	tilegx_output_simple_cbranch_with_opcode (insn, opcode, regop, false);
    }

  /* Generate a reversed branch around a direct jump.  This fallback
     does not use branch-likely instructions.  */
  not_taken = gen_label_rtx ();
  taken = operands[0];

  /* Generate the reversed branch to NOT_TAKEN.  */
  operands[0] = not_taken;
  branch_if_false =
    tilegx_output_simple_cbranch_with_opcode (insn, rev_opcode, regop, true);
  output_asm_insn (branch_if_false, operands);

  output_asm_insn ("j\t%l0", &taken);

  /* Output NOT_TAKEN.  */
  targetm.asm_out.internal_label (asm_out_file, "L",
				  CODE_LABEL_NUMBER (not_taken));
  return "";
}


/* Output assembly code for a conditional branch instruction.  */
const char *
tilegx_output_cbranch (rtx_insn *insn, rtx *operands, bool reversed)
{
  enum rtx_code code = GET_CODE (operands[1]);
  const char *opcode;
  const char *rev_opcode;

  if (reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case NE:
      opcode = "bnez";
      rev_opcode = "beqz";
      break;
    case EQ:
      opcode = "beqz";
      rev_opcode = "bnez";
      break;
    case GE:
      opcode = "bgez";
      rev_opcode = "bltz";
      break;
    case GT:
      opcode = "bgtz";
      rev_opcode = "blez";
      break;
    case LE:
      opcode = "blez";
      rev_opcode = "bgtz";
      break;
    case LT:
      opcode = "bltz";
      rev_opcode = "bgez";
      break;
    default:
      gcc_unreachable ();
    }

  return tilegx_output_cbranch_with_opcode (insn, operands, opcode,
					    rev_opcode, 2);
}


/* Implement the tablejump pattern.  */
void
tilegx_expand_tablejump (rtx op0, rtx op1)
{
  if (flag_pic)
    {
      rtx temp = gen_reg_rtx (Pmode);
      rtx temp2 = gen_reg_rtx (Pmode);

      tilegx_compute_pcrel_address (temp, gen_rtx_LABEL_REF (Pmode, op1));
      emit_move_insn (temp2,
		      gen_rtx_PLUS (Pmode,
				    convert_to_mode (Pmode, op0, false),
				    temp));
      op0 = temp2;
    }

  emit_jump_insn (gen_tablejump_aux (op0, op1));
}


/* Emit barrier before an atomic, as needed for the memory MODEL.  */
void
tilegx_pre_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, true))
    emit_insn (gen_memory_barrier ());
}


/* Emit barrier after an atomic, as needed for the memory MODEL.  */
void
tilegx_post_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, false))
    emit_insn (gen_memory_barrier ());
}



/* Expand a builtin vector binary op, by calling gen function GEN with
   operands in the proper modes.  DEST is converted to DEST_MODE, and
   src0 and src1 (if DO_SRC1 is true) is converted to SRC_MODE.  */
void
tilegx_expand_builtin_vector_binop (rtx (*gen) (rtx, rtx, rtx),
				    machine_mode dest_mode,
				    rtx dest,
				    machine_mode src_mode,
				    rtx src0, rtx src1, bool do_src1)
{
  dest = gen_lowpart (dest_mode, dest);

  if (src0 == const0_rtx)
    src0 = CONST0_RTX (src_mode);
  else
    src0 = gen_lowpart (src_mode, src0);

  if (do_src1)
    {
      if (src1 == const0_rtx)
	src1 = CONST0_RTX (src_mode);
      else
	src1 = gen_lowpart (src_mode, src1);
    }

  emit_insn ((*gen) (dest, src0, src1));
}



/* Intrinsics  */


struct tile_builtin_info
{
  enum insn_code icode;
  tree fndecl;
};

static struct tile_builtin_info tilegx_builtin_info[TILEGX_BUILTIN_max] = {
  { CODE_FOR_adddi3,                    NULL }, /* add */
  { CODE_FOR_addsi3,                    NULL }, /* addx */
  { CODE_FOR_ssaddsi3,                  NULL }, /* addxsc */
  { CODE_FOR_anddi3,                    NULL }, /* and */
  { CODE_FOR_insn_bfexts,               NULL }, /* bfexts */
  { CODE_FOR_insn_bfextu,               NULL }, /* bfextu */
  { CODE_FOR_insn_bfins,                NULL }, /* bfins */
  { CODE_FOR_clzdi2,                    NULL }, /* clz */
  { CODE_FOR_insn_cmoveqz,              NULL }, /* cmoveqz */
  { CODE_FOR_insn_cmovnez,              NULL }, /* cmovnez */
  { CODE_FOR_insn_cmpeq_didi,           NULL }, /* cmpeq */
  { CODE_FOR_insn_cmpexch,              NULL }, /* cmpexch */
  { CODE_FOR_insn_cmpexch4,             NULL }, /* cmpexch4 */
  { CODE_FOR_insn_cmples_didi,          NULL }, /* cmples */
  { CODE_FOR_insn_cmpleu_didi,          NULL }, /* cmpleu */
  { CODE_FOR_insn_cmplts_didi,          NULL }, /* cmplts */
  { CODE_FOR_insn_cmpltu_didi,          NULL }, /* cmpltu */
  { CODE_FOR_insn_cmpne_didi,           NULL }, /* cmpne */
  { CODE_FOR_insn_cmul,                 NULL }, /* cmul */
  { CODE_FOR_insn_cmula,                NULL }, /* cmula */
  { CODE_FOR_insn_cmulaf,               NULL }, /* cmulaf */
  { CODE_FOR_insn_cmulf,                NULL }, /* cmulf */
  { CODE_FOR_insn_cmulfr,               NULL }, /* cmulfr */
  { CODE_FOR_insn_cmulh,                NULL }, /* cmulh */
  { CODE_FOR_insn_cmulhr,               NULL }, /* cmulhr */
  { CODE_FOR_insn_crc32_32,             NULL }, /* crc32_32 */
  { CODE_FOR_insn_crc32_8,              NULL }, /* crc32_8 */
  { CODE_FOR_ctzdi2,                    NULL }, /* ctz */
  { CODE_FOR_insn_dblalign,             NULL }, /* dblalign */
  { CODE_FOR_insn_dblalign2,            NULL }, /* dblalign2 */
  { CODE_FOR_insn_dblalign4,            NULL }, /* dblalign4 */
  { CODE_FOR_insn_dblalign6,            NULL }, /* dblalign6 */
  { CODE_FOR_insn_drain,                NULL }, /* drain */
  { CODE_FOR_insn_dtlbpr,               NULL }, /* dtlbpr */
  { CODE_FOR_insn_exch,                 NULL }, /* exch */
  { CODE_FOR_insn_exch4,                NULL }, /* exch4 */
  { CODE_FOR_insn_fdouble_add_flags,    NULL }, /* fdouble_add_flags */
  { CODE_FOR_insn_fdouble_addsub,       NULL }, /* fdouble_addsub */
  { CODE_FOR_insn_fdouble_mul_flags,    NULL }, /* fdouble_mul_flags */
  { CODE_FOR_insn_fdouble_pack1,        NULL }, /* fdouble_pack1 */
  { CODE_FOR_insn_fdouble_pack2,        NULL }, /* fdouble_pack2 */
  { CODE_FOR_insn_fdouble_sub_flags,    NULL }, /* fdouble_sub_flags */
  { CODE_FOR_insn_fdouble_unpack_max,   NULL }, /* fdouble_unpack_max */
  { CODE_FOR_insn_fdouble_unpack_min,   NULL }, /* fdouble_unpack_min */
  { CODE_FOR_insn_fetchadd,             NULL }, /* fetchadd */
  { CODE_FOR_insn_fetchadd4,            NULL }, /* fetchadd4 */
  { CODE_FOR_insn_fetchaddgez,          NULL }, /* fetchaddgez */
  { CODE_FOR_insn_fetchaddgez4,         NULL }, /* fetchaddgez4 */
  { CODE_FOR_insn_fetchand,             NULL }, /* fetchand */
  { CODE_FOR_insn_fetchand4,            NULL }, /* fetchand4 */
  { CODE_FOR_insn_fetchor,              NULL }, /* fetchor */
  { CODE_FOR_insn_fetchor4,             NULL }, /* fetchor4 */
  { CODE_FOR_insn_finv,                 NULL }, /* finv */
  { CODE_FOR_insn_flush,                NULL }, /* flush */
  { CODE_FOR_insn_flushwb,              NULL }, /* flushwb */
  { CODE_FOR_insn_fnop,                 NULL }, /* fnop */
  { CODE_FOR_insn_fsingle_add1,         NULL }, /* fsingle_add1 */
  { CODE_FOR_insn_fsingle_addsub2,      NULL }, /* fsingle_addsub2 */
  { CODE_FOR_insn_fsingle_mul1,         NULL }, /* fsingle_mul1 */
  { CODE_FOR_insn_fsingle_mul2,         NULL }, /* fsingle_mul2 */
  { CODE_FOR_insn_fsingle_pack1,        NULL }, /* fsingle_pack1 */
  { CODE_FOR_insn_fsingle_pack2,        NULL }, /* fsingle_pack2 */
  { CODE_FOR_insn_fsingle_sub1,         NULL }, /* fsingle_sub1 */
  { CODE_FOR_insn_icoh,                 NULL }, /* icoh */
  { CODE_FOR_insn_ill,                  NULL }, /* ill */
  { CODE_FOR_insn_info,                 NULL }, /* info */
  { CODE_FOR_insn_infol,                NULL }, /* infol */
  { CODE_FOR_insn_inv,                  NULL }, /* inv */
  { CODE_FOR_insn_ld,                   NULL }, /* ld */
  { CODE_FOR_insn_ld1s,                 NULL }, /* ld1s */
  { CODE_FOR_insn_ld1u,                 NULL }, /* ld1u */
  { CODE_FOR_insn_ld2s,                 NULL }, /* ld2s */
  { CODE_FOR_insn_ld2u,                 NULL }, /* ld2u */
  { CODE_FOR_insn_ld4s,                 NULL }, /* ld4s */
  { CODE_FOR_insn_ld4u,                 NULL }, /* ld4u */
  { CODE_FOR_insn_ldna,                 NULL }, /* ldna */
  { CODE_FOR_insn_ldnt,                 NULL }, /* ldnt */
  { CODE_FOR_insn_ldnt1s,               NULL }, /* ldnt1s */
  { CODE_FOR_insn_ldnt1u,               NULL }, /* ldnt1u */
  { CODE_FOR_insn_ldnt2s,               NULL }, /* ldnt2s */
  { CODE_FOR_insn_ldnt2u,               NULL }, /* ldnt2u */
  { CODE_FOR_insn_ldnt4s,               NULL }, /* ldnt4s */
  { CODE_FOR_insn_ldnt4u,               NULL }, /* ldnt4u */
  { CODE_FOR_insn_ld_L2,                NULL }, /* ld_L2 */
  { CODE_FOR_insn_ld1s_L2,              NULL }, /* ld1s_L2 */
  { CODE_FOR_insn_ld1u_L2,              NULL }, /* ld1u_L2 */
  { CODE_FOR_insn_ld2s_L2,              NULL }, /* ld2s_L2 */
  { CODE_FOR_insn_ld2u_L2,              NULL }, /* ld2u_L2 */
  { CODE_FOR_insn_ld4s_L2,              NULL }, /* ld4s_L2 */
  { CODE_FOR_insn_ld4u_L2,              NULL }, /* ld4u_L2 */
  { CODE_FOR_insn_ldna_L2,              NULL }, /* ldna_L2 */
  { CODE_FOR_insn_ldnt_L2,              NULL }, /* ldnt_L2 */
  { CODE_FOR_insn_ldnt1s_L2,            NULL }, /* ldnt1s_L2 */
  { CODE_FOR_insn_ldnt1u_L2,            NULL }, /* ldnt1u_L2 */
  { CODE_FOR_insn_ldnt2s_L2,            NULL }, /* ldnt2s_L2 */
  { CODE_FOR_insn_ldnt2u_L2,            NULL }, /* ldnt2u_L2 */
  { CODE_FOR_insn_ldnt4s_L2,            NULL }, /* ldnt4s_L2 */
  { CODE_FOR_insn_ldnt4u_L2,            NULL }, /* ldnt4u_L2 */
  { CODE_FOR_insn_ld_miss,              NULL }, /* ld_miss */
  { CODE_FOR_insn_ld1s_miss,            NULL }, /* ld1s_miss */
  { CODE_FOR_insn_ld1u_miss,            NULL }, /* ld1u_miss */
  { CODE_FOR_insn_ld2s_miss,            NULL }, /* ld2s_miss */
  { CODE_FOR_insn_ld2u_miss,            NULL }, /* ld2u_miss */
  { CODE_FOR_insn_ld4s_miss,            NULL }, /* ld4s_miss */
  { CODE_FOR_insn_ld4u_miss,            NULL }, /* ld4u_miss */
  { CODE_FOR_insn_ldna_miss,            NULL }, /* ldna_miss */
  { CODE_FOR_insn_ldnt_miss,            NULL }, /* ldnt_miss */
  { CODE_FOR_insn_ldnt1s_miss,          NULL }, /* ldnt1s_miss */
  { CODE_FOR_insn_ldnt1u_miss,          NULL }, /* ldnt1u_miss */
  { CODE_FOR_insn_ldnt2s_miss,          NULL }, /* ldnt2s_miss */
  { CODE_FOR_insn_ldnt2u_miss,          NULL }, /* ldnt2u_miss */
  { CODE_FOR_insn_ldnt4s_miss,          NULL }, /* ldnt4s_miss */
  { CODE_FOR_insn_ldnt4u_miss,          NULL }, /* ldnt4u_miss */
  { CODE_FOR_insn_lnk,                  NULL }, /* lnk */
  { CODE_FOR_memory_barrier,            NULL }, /* mf */
  { CODE_FOR_insn_mfspr,                NULL }, /* mfspr */
  { CODE_FOR_insn_mm,                   NULL }, /* mm */
  { CODE_FOR_insn_mnz,                  NULL }, /* mnz */
  { CODE_FOR_movdi,                     NULL }, /* move */
  { CODE_FOR_insn_mtspr,                NULL }, /* mtspr */
  { CODE_FOR_insn_mul_hs_hs,            NULL }, /* mul_hs_hs */
  { CODE_FOR_insn_mul_hs_hu,            NULL }, /* mul_hs_hu */
  { CODE_FOR_insn_mul_hs_ls,            NULL }, /* mul_hs_ls */
  { CODE_FOR_insn_mul_hs_lu,            NULL }, /* mul_hs_lu */
  { CODE_FOR_insn_mul_hu_hu,            NULL }, /* mul_hu_hu */
  { CODE_FOR_insn_mul_hu_ls,            NULL }, /* mul_hu_ls */
  { CODE_FOR_insn_mul_hu_lu,            NULL }, /* mul_hu_lu */
  { CODE_FOR_insn_mul_ls_ls,            NULL }, /* mul_ls_ls */
  { CODE_FOR_insn_mul_ls_lu,            NULL }, /* mul_ls_lu */
  { CODE_FOR_insn_mul_lu_lu,            NULL }, /* mul_lu_lu */
  { CODE_FOR_insn_mula_hs_hs,           NULL }, /* mula_hs_hs */
  { CODE_FOR_insn_mula_hs_hu,           NULL }, /* mula_hs_hu */
  { CODE_FOR_insn_mula_hs_ls,           NULL }, /* mula_hs_ls */
  { CODE_FOR_insn_mula_hs_lu,           NULL }, /* mula_hs_lu */
  { CODE_FOR_insn_mula_hu_hu,           NULL }, /* mula_hu_hu */
  { CODE_FOR_insn_mula_hu_ls,           NULL }, /* mula_hu_ls */
  { CODE_FOR_insn_mula_hu_lu,           NULL }, /* mula_hu_lu */
  { CODE_FOR_insn_mula_ls_ls,           NULL }, /* mula_ls_ls */
  { CODE_FOR_insn_mula_ls_lu,           NULL }, /* mula_ls_lu */
  { CODE_FOR_insn_mula_lu_lu,           NULL }, /* mula_lu_lu */
  { CODE_FOR_insn_mulax,                NULL }, /* mulax */
  { CODE_FOR_mulsi3,                    NULL }, /* mulx */
  { CODE_FOR_insn_mz,                   NULL }, /* mz */
  { CODE_FOR_insn_nap,                  NULL }, /* nap */
  { CODE_FOR_nop,                       NULL }, /* nop */
  { CODE_FOR_insn_nor_di,               NULL }, /* nor */
  { CODE_FOR_iordi3,                    NULL }, /* or */
  { CODE_FOR_popcountdi2,               NULL }, /* pcnt */
  { CODE_FOR_insn_prefetch_l1,          NULL }, /* prefetch_l1 */
  { CODE_FOR_insn_prefetch_l1_fault,    NULL }, /* prefetch_l1_fault */
  { CODE_FOR_insn_prefetch_l2,          NULL }, /* prefetch_l2 */
  { CODE_FOR_insn_prefetch_l2_fault,    NULL }, /* prefetch_l2_fault */
  { CODE_FOR_insn_prefetch_l3,          NULL }, /* prefetch_l3 */
  { CODE_FOR_insn_prefetch_l3_fault,    NULL }, /* prefetch_l3_fault */
  { CODE_FOR_insn_revbits,              NULL }, /* revbits */
  { CODE_FOR_bswapdi2,                  NULL }, /* revbytes */
  { CODE_FOR_rotldi3,                   NULL }, /* rotl */
  { CODE_FOR_ashldi3,                   NULL }, /* shl */
  { CODE_FOR_insn_shl16insli,           NULL }, /* shl16insli */
  { CODE_FOR_insn_shl1add,              NULL }, /* shl1add */
  { CODE_FOR_insn_shl1addx,             NULL }, /* shl1addx */
  { CODE_FOR_insn_shl2add,              NULL }, /* shl2add */
  { CODE_FOR_insn_shl2addx,             NULL }, /* shl2addx */
  { CODE_FOR_insn_shl3add,              NULL }, /* shl3add */
  { CODE_FOR_insn_shl3addx,             NULL }, /* shl3addx */
  { CODE_FOR_ashlsi3,                   NULL }, /* shlx */
  { CODE_FOR_ashrdi3,                   NULL }, /* shrs */
  { CODE_FOR_lshrdi3,                   NULL }, /* shru */
  { CODE_FOR_lshrsi3,                   NULL }, /* shrux */
  { CODE_FOR_insn_shufflebytes,         NULL }, /* shufflebytes */
  { CODE_FOR_insn_shufflebytes1,        NULL }, /* shufflebytes1 */
  { CODE_FOR_insn_st,                   NULL }, /* st */
  { CODE_FOR_insn_st1,                  NULL }, /* st1 */
  { CODE_FOR_insn_st2,                  NULL }, /* st2 */
  { CODE_FOR_insn_st4,                  NULL }, /* st4 */
  { CODE_FOR_insn_stnt,                 NULL }, /* stnt */
  { CODE_FOR_insn_stnt1,                NULL }, /* stnt1 */
  { CODE_FOR_insn_stnt2,                NULL }, /* stnt2 */
  { CODE_FOR_insn_stnt4,                NULL }, /* stnt4 */
  { CODE_FOR_subdi3,                    NULL }, /* sub */
  { CODE_FOR_subsi3,                    NULL }, /* subx */
  { CODE_FOR_sssubsi3,                  NULL }, /* subxsc */
  { CODE_FOR_insn_tblidxb0,             NULL }, /* tblidxb0 */
  { CODE_FOR_insn_tblidxb1,             NULL }, /* tblidxb1 */
  { CODE_FOR_insn_tblidxb2,             NULL }, /* tblidxb2 */
  { CODE_FOR_insn_tblidxb3,             NULL }, /* tblidxb3 */
  { CODE_FOR_insn_v1add,                NULL }, /* v1add */
  { CODE_FOR_insn_v1addi,               NULL }, /* v1addi */
  { CODE_FOR_insn_v1adduc,              NULL }, /* v1adduc */
  { CODE_FOR_insn_v1adiffu,             NULL }, /* v1adiffu */
  { CODE_FOR_insn_v1avgu,               NULL }, /* v1avgu */
  { CODE_FOR_insn_v1cmpeq,              NULL }, /* v1cmpeq */
  { CODE_FOR_insn_v1cmpeqi,             NULL }, /* v1cmpeqi */
  { CODE_FOR_insn_v1cmples,             NULL }, /* v1cmples */
  { CODE_FOR_insn_v1cmpleu,             NULL }, /* v1cmpleu */
  { CODE_FOR_insn_v1cmplts,             NULL }, /* v1cmplts */
  { CODE_FOR_insn_v1cmpltsi,            NULL }, /* v1cmpltsi */
  { CODE_FOR_insn_v1cmpltu,             NULL }, /* v1cmpltu */
  { CODE_FOR_insn_v1cmpltui,            NULL }, /* v1cmpltui */
  { CODE_FOR_insn_v1cmpne,              NULL }, /* v1cmpne */
  { CODE_FOR_insn_v1ddotpu,             NULL }, /* v1ddotpu */
  { CODE_FOR_insn_v1ddotpua,            NULL }, /* v1ddotpua */
  { CODE_FOR_insn_v1ddotpus,            NULL }, /* v1ddotpus */
  { CODE_FOR_insn_v1ddotpusa,           NULL }, /* v1ddotpusa */
  { CODE_FOR_insn_v1dotp,               NULL }, /* v1dotp */
  { CODE_FOR_insn_v1dotpa,              NULL }, /* v1dotpa */
  { CODE_FOR_insn_v1dotpu,              NULL }, /* v1dotpu */
  { CODE_FOR_insn_v1dotpua,             NULL }, /* v1dotpua */
  { CODE_FOR_insn_v1dotpus,             NULL }, /* v1dotpus */
  { CODE_FOR_insn_v1dotpusa,            NULL }, /* v1dotpusa */
  { CODE_FOR_insn_v1int_h,              NULL }, /* v1int_h */
  { CODE_FOR_insn_v1int_l,              NULL }, /* v1int_l */
  { CODE_FOR_insn_v1maxu,               NULL }, /* v1maxu */
  { CODE_FOR_insn_v1maxui,              NULL }, /* v1maxui */
  { CODE_FOR_insn_v1minu,               NULL }, /* v1minu */
  { CODE_FOR_insn_v1minui,              NULL }, /* v1minui */
  { CODE_FOR_insn_v1mnz,                NULL }, /* v1mnz */
  { CODE_FOR_insn_v1multu,              NULL }, /* v1multu */
  { CODE_FOR_insn_v1mulu,               NULL }, /* v1mulu */
  { CODE_FOR_insn_v1mulus,              NULL }, /* v1mulus */
  { CODE_FOR_insn_v1mz,                 NULL }, /* v1mz */
  { CODE_FOR_insn_v1sadau,              NULL }, /* v1sadau */
  { CODE_FOR_insn_v1sadu,               NULL }, /* v1sadu */
  { CODE_FOR_insn_v1shl,                NULL }, /* v1shl */
  { CODE_FOR_insn_v1shl,                NULL }, /* v1shli */
  { CODE_FOR_insn_v1shrs,               NULL }, /* v1shrs */
  { CODE_FOR_insn_v1shrs,               NULL }, /* v1shrsi */
  { CODE_FOR_insn_v1shru,               NULL }, /* v1shru */
  { CODE_FOR_insn_v1shru,               NULL }, /* v1shrui */
  { CODE_FOR_insn_v1sub,                NULL }, /* v1sub */
  { CODE_FOR_insn_v1subuc,              NULL }, /* v1subuc */
  { CODE_FOR_insn_v2add,                NULL }, /* v2add */
  { CODE_FOR_insn_v2addi,               NULL }, /* v2addi */
  { CODE_FOR_insn_v2addsc,              NULL }, /* v2addsc */
  { CODE_FOR_insn_v2adiffs,             NULL }, /* v2adiffs */
  { CODE_FOR_insn_v2avgs,               NULL }, /* v2avgs */
  { CODE_FOR_insn_v2cmpeq,              NULL }, /* v2cmpeq */
  { CODE_FOR_insn_v2cmpeqi,             NULL }, /* v2cmpeqi */
  { CODE_FOR_insn_v2cmples,             NULL }, /* v2cmples */
  { CODE_FOR_insn_v2cmpleu,             NULL }, /* v2cmpleu */
  { CODE_FOR_insn_v2cmplts,             NULL }, /* v2cmplts */
  { CODE_FOR_insn_v2cmpltsi,            NULL }, /* v2cmpltsi */
  { CODE_FOR_insn_v2cmpltu,             NULL }, /* v2cmpltu */
  { CODE_FOR_insn_v2cmpltui,            NULL }, /* v2cmpltui */
  { CODE_FOR_insn_v2cmpne,              NULL }, /* v2cmpne */
  { CODE_FOR_insn_v2dotp,               NULL }, /* v2dotp */
  { CODE_FOR_insn_v2dotpa,              NULL }, /* v2dotpa */
  { CODE_FOR_insn_v2int_h,              NULL }, /* v2int_h */
  { CODE_FOR_insn_v2int_l,              NULL }, /* v2int_l */
  { CODE_FOR_insn_v2maxs,               NULL }, /* v2maxs */
  { CODE_FOR_insn_v2maxsi,              NULL }, /* v2maxsi */
  { CODE_FOR_insn_v2mins,               NULL }, /* v2mins */
  { CODE_FOR_insn_v2minsi,              NULL }, /* v2minsi */
  { CODE_FOR_insn_v2mnz,                NULL }, /* v2mnz */
  { CODE_FOR_insn_v2mulfsc,             NULL }, /* v2mulfsc */
  { CODE_FOR_insn_v2muls,               NULL }, /* v2muls */
  { CODE_FOR_insn_v2mults,              NULL }, /* v2mults */
  { CODE_FOR_insn_v2mz,                 NULL }, /* v2mz */
  { CODE_FOR_insn_v2packh,              NULL }, /* v2packh */
  { CODE_FOR_insn_v2packl,              NULL }, /* v2packl */
  { CODE_FOR_insn_v2packuc,             NULL }, /* v2packuc */
  { CODE_FOR_insn_v2sadas,              NULL }, /* v2sadas */
  { CODE_FOR_insn_v2sadau,              NULL }, /* v2sadau */
  { CODE_FOR_insn_v2sads,               NULL }, /* v2sads */
  { CODE_FOR_insn_v2sadu,               NULL }, /* v2sadu */
  { CODE_FOR_insn_v2shl,                NULL }, /* v2shl */
  { CODE_FOR_insn_v2shl,                NULL }, /* v2shli */
  { CODE_FOR_insn_v2shlsc,              NULL }, /* v2shlsc */
  { CODE_FOR_insn_v2shrs,               NULL }, /* v2shrs */
  { CODE_FOR_insn_v2shrs,               NULL }, /* v2shrsi */
  { CODE_FOR_insn_v2shru,               NULL }, /* v2shru */
  { CODE_FOR_insn_v2shru,               NULL }, /* v2shrui */
  { CODE_FOR_insn_v2sub,                NULL }, /* v2sub */
  { CODE_FOR_insn_v2subsc,              NULL }, /* v2subsc */
  { CODE_FOR_insn_v4add,                NULL }, /* v4add */
  { CODE_FOR_insn_v4addsc,              NULL }, /* v4addsc */
  { CODE_FOR_insn_v4int_h,              NULL }, /* v4int_h */
  { CODE_FOR_insn_v4int_l,              NULL }, /* v4int_l */
  { CODE_FOR_insn_v4packsc,             NULL }, /* v4packsc */
  { CODE_FOR_insn_v4shl,                NULL }, /* v4shl */
  { CODE_FOR_insn_v4shlsc,              NULL }, /* v4shlsc */
  { CODE_FOR_insn_v4shrs,               NULL }, /* v4shrs */
  { CODE_FOR_insn_v4shru,               NULL }, /* v4shru */
  { CODE_FOR_insn_v4sub,                NULL }, /* v4sub */
  { CODE_FOR_insn_v4subsc,              NULL }, /* v4subsc */
  { CODE_FOR_insn_wh64,                 NULL }, /* wh64 */
  { CODE_FOR_xordi3,                    NULL }, /* xor */
  { CODE_FOR_tilegx_network_barrier,    NULL }, /* network_barrier */
  { CODE_FOR_tilegx_idn0_receive,       NULL }, /* idn0_receive */
  { CODE_FOR_tilegx_idn1_receive,       NULL }, /* idn1_receive */
  { CODE_FOR_tilegx_idn_send,           NULL }, /* idn_send */
  { CODE_FOR_tilegx_udn0_receive,       NULL }, /* udn0_receive */
  { CODE_FOR_tilegx_udn1_receive,       NULL }, /* udn1_receive */
  { CODE_FOR_tilegx_udn2_receive,       NULL }, /* udn2_receive */
  { CODE_FOR_tilegx_udn3_receive,       NULL }, /* udn3_receive */
  { CODE_FOR_tilegx_udn_send,           NULL }, /* udn_send */
};


struct tilegx_builtin_def
{
  const char *name;
  enum tilegx_builtin code;
  bool is_const;
  /* The first character is the return type.  Subsequent characters
     are the argument types. See char_to_type.  */
  const char *type;
};


static const struct tilegx_builtin_def tilegx_builtins[] = {
  { "__insn_add",                TILEGX_INSN_ADD,                true,  "lll"  },
  { "__insn_addi",               TILEGX_INSN_ADD,                true,  "lll"  },
  { "__insn_addli",              TILEGX_INSN_ADD,                true,  "lll"  },
  { "__insn_addx",               TILEGX_INSN_ADDX,               true,  "iii"  },
  { "__insn_addxi",              TILEGX_INSN_ADDX,               true,  "iii"  },
  { "__insn_addxli",             TILEGX_INSN_ADDX,               true,  "iii"  },
  { "__insn_addxsc",             TILEGX_INSN_ADDXSC,             true,  "iii"  },
  { "__insn_and",                TILEGX_INSN_AND,                true,  "lll"  },
  { "__insn_andi",               TILEGX_INSN_AND,                true,  "lll"  },
  { "__insn_bfexts",             TILEGX_INSN_BFEXTS,             true,  "llll" },
  { "__insn_bfextu",             TILEGX_INSN_BFEXTU,             true,  "llll" },
  { "__insn_bfins",              TILEGX_INSN_BFINS,              true,  "lllll"},
  { "__insn_clz",                TILEGX_INSN_CLZ,                true,  "ll"   },
  { "__insn_cmoveqz",            TILEGX_INSN_CMOVEQZ,            true,  "llll" },
  { "__insn_cmovnez",            TILEGX_INSN_CMOVNEZ,            true,  "llll" },
  { "__insn_cmpeq",              TILEGX_INSN_CMPEQ,              true,  "lll"  },
  { "__insn_cmpeqi",             TILEGX_INSN_CMPEQ,              true,  "lll"  },
  { "__insn_cmpexch",            TILEGX_INSN_CMPEXCH,            false, "lpl"  },
  { "__insn_cmpexch4",           TILEGX_INSN_CMPEXCH4,           false, "ipi"  },
  { "__insn_cmples",             TILEGX_INSN_CMPLES,             true,  "lll"  },
  { "__insn_cmpleu",             TILEGX_INSN_CMPLEU,             true,  "lll"  },
  { "__insn_cmplts",             TILEGX_INSN_CMPLTS,             true,  "lll"  },
  { "__insn_cmpltsi",            TILEGX_INSN_CMPLTS,             true,  "lll"  },
  { "__insn_cmpltu",             TILEGX_INSN_CMPLTU,             true,  "lll"  },
  { "__insn_cmpltui",            TILEGX_INSN_CMPLTU,             true,  "lll"  },
  { "__insn_cmpne",              TILEGX_INSN_CMPNE,              true,  "lll"  },
  { "__insn_cmul",               TILEGX_INSN_CMUL,               true,  "lll"  },
  { "__insn_cmula",              TILEGX_INSN_CMULA,              true,  "llll" },
  { "__insn_cmulaf",             TILEGX_INSN_CMULAF,             true,  "llll" },
  { "__insn_cmulf",              TILEGX_INSN_CMULF,              true,  "lll"  },
  { "__insn_cmulfr",             TILEGX_INSN_CMULFR,             true,  "lll"  },
  { "__insn_cmulh",              TILEGX_INSN_CMULH,              true,  "lll"  },
  { "__insn_cmulhr",             TILEGX_INSN_CMULHR,             true,  "lll"  },
  { "__insn_crc32_32",           TILEGX_INSN_CRC32_32,           true,  "lll"  },
  { "__insn_crc32_8",            TILEGX_INSN_CRC32_8,            true,  "lll"  },
  { "__insn_ctz",                TILEGX_INSN_CTZ,                true,  "ll"   },
  { "__insn_dblalign",           TILEGX_INSN_DBLALIGN,           true,  "lllk" },
  { "__insn_dblalign2",          TILEGX_INSN_DBLALIGN2,          true,  "lll"  },
  { "__insn_dblalign4",          TILEGX_INSN_DBLALIGN4,          true,  "lll"  },
  { "__insn_dblalign6",          TILEGX_INSN_DBLALIGN6,          true,  "lll"  },
  { "__insn_drain",              TILEGX_INSN_DRAIN,              false, "v"    },
  { "__insn_dtlbpr",             TILEGX_INSN_DTLBPR,             false, "vl"   },
  { "__insn_exch",               TILEGX_INSN_EXCH,               false, "lpl"  },
  { "__insn_exch4",              TILEGX_INSN_EXCH4,              false, "ipi"  },
  { "__insn_fdouble_add_flags",  TILEGX_INSN_FDOUBLE_ADD_FLAGS,  true,  "lll"  },
  { "__insn_fdouble_addsub",     TILEGX_INSN_FDOUBLE_ADDSUB,     true,  "llll" },
  { "__insn_fdouble_mul_flags",  TILEGX_INSN_FDOUBLE_MUL_FLAGS,  true,  "lll"  },
  { "__insn_fdouble_pack1",      TILEGX_INSN_FDOUBLE_PACK1,      true,  "lll"  },
  { "__insn_fdouble_pack2",      TILEGX_INSN_FDOUBLE_PACK2,      true,  "llll" },
  { "__insn_fdouble_sub_flags",  TILEGX_INSN_FDOUBLE_SUB_FLAGS,  true,  "lll"  },
  { "__insn_fdouble_unpack_max", TILEGX_INSN_FDOUBLE_UNPACK_MAX, true,  "lll"  },
  { "__insn_fdouble_unpack_min", TILEGX_INSN_FDOUBLE_UNPACK_MIN, true,  "lll"  },
  { "__insn_fetchadd",           TILEGX_INSN_FETCHADD,           false, "lpl"  },
  { "__insn_fetchadd4",          TILEGX_INSN_FETCHADD4,          false, "ipi"  },
  { "__insn_fetchaddgez",        TILEGX_INSN_FETCHADDGEZ,        false, "lpl"  },
  { "__insn_fetchaddgez4",       TILEGX_INSN_FETCHADDGEZ4,       false, "ipi"  },
  { "__insn_fetchand",           TILEGX_INSN_FETCHAND,           false, "lpl"  },
  { "__insn_fetchand4",          TILEGX_INSN_FETCHAND4,          false, "ipi"  },
  { "__insn_fetchor",            TILEGX_INSN_FETCHOR,            false, "lpl"  },
  { "__insn_fetchor4",           TILEGX_INSN_FETCHOR4,           false, "ipi"  },
  { "__insn_finv",               TILEGX_INSN_FINV,               false, "vk"   },
  { "__insn_flush",              TILEGX_INSN_FLUSH,              false, "vk"   },
  { "__insn_flushwb",            TILEGX_INSN_FLUSHWB,            false, "v"    },
  { "__insn_fnop",               TILEGX_INSN_FNOP,               false, "v"    },
  { "__insn_fsingle_add1",       TILEGX_INSN_FSINGLE_ADD1,       true,  "lll"  },
  { "__insn_fsingle_addsub2",    TILEGX_INSN_FSINGLE_ADDSUB2,    true,  "llll" },
  { "__insn_fsingle_mul1",       TILEGX_INSN_FSINGLE_MUL1,       true,  "lll"  },
  { "__insn_fsingle_mul2",       TILEGX_INSN_FSINGLE_MUL2,       true,  "lll"  },
  { "__insn_fsingle_pack1",      TILEGX_INSN_FSINGLE_PACK1,      true,  "ll"   },
  { "__insn_fsingle_pack2",      TILEGX_INSN_FSINGLE_PACK2,      true,  "lll"  },
  { "__insn_fsingle_sub1",       TILEGX_INSN_FSINGLE_SUB1,       true,  "lll"  },
  { "__insn_icoh",               TILEGX_INSN_ICOH,               false, "vk"   },
  { "__insn_ill",                TILEGX_INSN_ILL,                false, "v"    },
  { "__insn_info",               TILEGX_INSN_INFO,               false, "vl"   },
  { "__insn_infol",              TILEGX_INSN_INFOL,              false, "vl"   },
  { "__insn_inv",                TILEGX_INSN_INV,                false, "vp"   },
  { "__insn_ld",                 TILEGX_INSN_LD,                 false, "lk"   },
  { "__insn_ld1s",               TILEGX_INSN_LD1S,               false, "lk"   },
  { "__insn_ld1u",               TILEGX_INSN_LD1U,               false, "lk"   },
  { "__insn_ld2s",               TILEGX_INSN_LD2S,               false, "lk"   },
  { "__insn_ld2u",               TILEGX_INSN_LD2U,               false, "lk"   },
  { "__insn_ld4s",               TILEGX_INSN_LD4S,               false, "lk"   },
  { "__insn_ld4u",               TILEGX_INSN_LD4U,               false, "lk"   },
  { "__insn_ldna",               TILEGX_INSN_LDNA,               false, "lk"   },
  { "__insn_ldnt",               TILEGX_INSN_LDNT,               false, "lk"   },
  { "__insn_ldnt1s",             TILEGX_INSN_LDNT1S,             false, "lk"   },
  { "__insn_ldnt1u",             TILEGX_INSN_LDNT1U,             false, "lk"   },
  { "__insn_ldnt2s",             TILEGX_INSN_LDNT2S,             false, "lk"   },
  { "__insn_ldnt2u",             TILEGX_INSN_LDNT2U,             false, "lk"   },
  { "__insn_ldnt4s",             TILEGX_INSN_LDNT4S,             false, "lk"   },
  { "__insn_ldnt4u",             TILEGX_INSN_LDNT4U,             false, "lk"   },
  { "__insn_ld_L2",              TILEGX_INSN_LD_L2,              false, "lk"   },
  { "__insn_ld1s_L2",            TILEGX_INSN_LD1S_L2,            false, "lk"   },
  { "__insn_ld1u_L2",            TILEGX_INSN_LD1U_L2,            false, "lk"   },
  { "__insn_ld2s_L2",            TILEGX_INSN_LD2S_L2,            false, "lk"   },
  { "__insn_ld2u_L2",            TILEGX_INSN_LD2U_L2,            false, "lk"   },
  { "__insn_ld4s_L2",            TILEGX_INSN_LD4S_L2,            false, "lk"   },
  { "__insn_ld4u_L2",            TILEGX_INSN_LD4U_L2,            false, "lk"   },
  { "__insn_ldna_L2",            TILEGX_INSN_LDNA_L2,            false, "lk"   },
  { "__insn_ldnt_L2",            TILEGX_INSN_LDNT_L2,            false, "lk"   },
  { "__insn_ldnt1s_L2",          TILEGX_INSN_LDNT1S_L2,          false, "lk"   },
  { "__insn_ldnt1u_L2",          TILEGX_INSN_LDNT1U_L2,          false, "lk"   },
  { "__insn_ldnt2s_L2",          TILEGX_INSN_LDNT2S_L2,          false, "lk"   },
  { "__insn_ldnt2u_L2",          TILEGX_INSN_LDNT2U_L2,          false, "lk"   },
  { "__insn_ldnt4s_L2",          TILEGX_INSN_LDNT4S_L2,          false, "lk"   },
  { "__insn_ldnt4u_L2",          TILEGX_INSN_LDNT4U_L2,          false, "lk"   },
  { "__insn_ld_miss",            TILEGX_INSN_LD_MISS,            false, "lk"   },
  { "__insn_ld1s_miss",          TILEGX_INSN_LD1S_MISS,          false, "lk"   },
  { "__insn_ld1u_miss",          TILEGX_INSN_LD1U_MISS,          false, "lk"   },
  { "__insn_ld2s_miss",          TILEGX_INSN_LD2S_MISS,          false, "lk"   },
  { "__insn_ld2u_miss",          TILEGX_INSN_LD2U_MISS,          false, "lk"   },
  { "__insn_ld4s_miss",          TILEGX_INSN_LD4S_MISS,          false, "lk"   },
  { "__insn_ld4u_miss",          TILEGX_INSN_LD4U_MISS,          false, "lk"   },
  { "__insn_ldna_miss",          TILEGX_INSN_LDNA_MISS,          false, "lk"   },
  { "__insn_ldnt_miss",          TILEGX_INSN_LDNT_MISS,          false, "lk"   },
  { "__insn_ldnt1s_miss",        TILEGX_INSN_LDNT1S_MISS,        false, "lk"   },
  { "__insn_ldnt1u_miss",        TILEGX_INSN_LDNT1U_MISS,        false, "lk"   },
  { "__insn_ldnt2s_miss",        TILEGX_INSN_LDNT2S_MISS,        false, "lk"   },
  { "__insn_ldnt2u_miss",        TILEGX_INSN_LDNT2U_MISS,        false, "lk"   },
  { "__insn_ldnt4s_miss",        TILEGX_INSN_LDNT4S_MISS,        false, "lk"   },
  { "__insn_ldnt4u_miss",        TILEGX_INSN_LDNT4U_MISS,        false, "lk"   },
  { "__insn_lnk",                TILEGX_INSN_LNK,                true,  "l"    },
  { "__insn_mf",                 TILEGX_INSN_MF,                 false, "v"    },
  { "__insn_mfspr",              TILEGX_INSN_MFSPR,              false, "ll"   },
  { "__insn_mm",                 TILEGX_INSN_MM,                 true,  "lllll"},
  { "__insn_mnz",                TILEGX_INSN_MNZ,                true,  "lll"  },
  { "__insn_move",               TILEGX_INSN_MOVE,               true,  "ll"   },
  { "__insn_movei",              TILEGX_INSN_MOVE,               true,  "ll"   },
  { "__insn_moveli",             TILEGX_INSN_MOVE,               true,  "ll"   },
  { "__insn_mtspr",              TILEGX_INSN_MTSPR,              false, "vll"  },
  { "__insn_mul_hs_hs",          TILEGX_INSN_MUL_HS_HS,          true,  "lll"  },
  { "__insn_mul_hs_hu",          TILEGX_INSN_MUL_HS_HU,          true,  "lll"  },
  { "__insn_mul_hs_ls",          TILEGX_INSN_MUL_HS_LS,          true,  "lll"  },
  { "__insn_mul_hs_lu",          TILEGX_INSN_MUL_HS_LU,          true,  "lll"  },
  { "__insn_mul_hu_hu",          TILEGX_INSN_MUL_HU_HU,          true,  "lll"  },
  { "__insn_mul_hu_ls",          TILEGX_INSN_MUL_HU_LS,          true,  "lll"  },
  { "__insn_mul_hu_lu",          TILEGX_INSN_MUL_HU_LU,          true,  "lll"  },
  { "__insn_mul_ls_ls",          TILEGX_INSN_MUL_LS_LS,          true,  "lll"  },
  { "__insn_mul_ls_lu",          TILEGX_INSN_MUL_LS_LU,          true,  "lll"  },
  { "__insn_mul_lu_lu",          TILEGX_INSN_MUL_LU_LU,          true,  "lll"  },
  { "__insn_mula_hs_hs",         TILEGX_INSN_MULA_HS_HS,         true,  "llll" },
  { "__insn_mula_hs_hu",         TILEGX_INSN_MULA_HS_HU,         true,  "llll" },
  { "__insn_mula_hs_ls",         TILEGX_INSN_MULA_HS_LS,         true,  "llll" },
  { "__insn_mula_hs_lu",         TILEGX_INSN_MULA_HS_LU,         true,  "llll" },
  { "__insn_mula_hu_hu",         TILEGX_INSN_MULA_HU_HU,         true,  "llll" },
  { "__insn_mula_hu_ls",         TILEGX_INSN_MULA_HU_LS,         true,  "llll" },
  { "__insn_mula_hu_lu",         TILEGX_INSN_MULA_HU_LU,         true,  "llll" },
  { "__insn_mula_ls_ls",         TILEGX_INSN_MULA_LS_LS,         true,  "llll" },
  { "__insn_mula_ls_lu",         TILEGX_INSN_MULA_LS_LU,         true,  "llll" },
  { "__insn_mula_lu_lu",         TILEGX_INSN_MULA_LU_LU,         true,  "llll" },
  { "__insn_mulax",              TILEGX_INSN_MULAX,              true,  "iiii" },
  { "__insn_mulx",               TILEGX_INSN_MULX,               true,  "iii"  },
  { "__insn_mz",                 TILEGX_INSN_MZ,                 true,  "lll"  },
  { "__insn_nap",                TILEGX_INSN_NAP,                false, "v"    },
  { "__insn_nop",                TILEGX_INSN_NOP,                true,  "v"    },
  { "__insn_nor",                TILEGX_INSN_NOR,                true,  "lll"  },
  { "__insn_or",                 TILEGX_INSN_OR,                 true,  "lll"  },
  { "__insn_ori",                TILEGX_INSN_OR,                 true,  "lll"  },
  { "__insn_pcnt",               TILEGX_INSN_PCNT,               true,  "ll"   },
  { "__insn_prefetch",           TILEGX_INSN_PREFETCH_L1,        false, "vk"   },
  { "__insn_prefetch_l1",        TILEGX_INSN_PREFETCH_L1,        false, "vk"   },
  { "__insn_prefetch_l1_fault",  TILEGX_INSN_PREFETCH_L1_FAULT,  false, "vk"   },
  { "__insn_prefetch_l2",        TILEGX_INSN_PREFETCH_L2,        false, "vk"   },
  { "__insn_prefetch_l2_fault",  TILEGX_INSN_PREFETCH_L2_FAULT,  false, "vk"   },
  { "__insn_prefetch_l3",        TILEGX_INSN_PREFETCH_L3,        false, "vk"   },
  { "__insn_prefetch_l3_fault",  TILEGX_INSN_PREFETCH_L3_FAULT,  false, "vk"   },
  { "__insn_revbits",            TILEGX_INSN_REVBITS,            true,  "ll"   },
  { "__insn_revbytes",           TILEGX_INSN_REVBYTES,           true,  "ll"   },
  { "__insn_rotl",               TILEGX_INSN_ROTL,               true,  "lli"  },
  { "__insn_rotli",              TILEGX_INSN_ROTL,               true,  "lli"  },
  { "__insn_shl",                TILEGX_INSN_SHL,                true,  "lli"  },
  { "__insn_shl16insli",         TILEGX_INSN_SHL16INSLI,         true,  "lll"  },
  { "__insn_shl1add",            TILEGX_INSN_SHL1ADD,            true,  "lll"  },
  { "__insn_shl1addx",           TILEGX_INSN_SHL1ADDX,           true,  "iii"  },
  { "__insn_shl2add",            TILEGX_INSN_SHL2ADD,            true,  "lll"  },
  { "__insn_shl2addx",           TILEGX_INSN_SHL2ADDX,           true,  "iii"  },
  { "__insn_shl3add",            TILEGX_INSN_SHL3ADD,            true,  "lll"  },
  { "__insn_shl3addx",           TILEGX_INSN_SHL3ADDX,           true,  "iii"  },
  { "__insn_shli",               TILEGX_INSN_SHL,                true,  "lli"  },
  { "__insn_shlx",               TILEGX_INSN_SHLX,               true,  "iii"  },
  { "__insn_shlxi",              TILEGX_INSN_SHLX,               true,  "iii"  },
  { "__insn_shrs",               TILEGX_INSN_SHRS,               true,  "lli"  },
  { "__insn_shrsi",              TILEGX_INSN_SHRS,               true,  "lli"  },
  { "__insn_shru",               TILEGX_INSN_SHRU,               true,  "lli"  },
  { "__insn_shrui",              TILEGX_INSN_SHRU,               true,  "lli"  },
  { "__insn_shrux",              TILEGX_INSN_SHRUX,              true,  "iii"  },
  { "__insn_shruxi",             TILEGX_INSN_SHRUX,              true,  "iii"  },
  { "__insn_shufflebytes",       TILEGX_INSN_SHUFFLEBYTES,       true,  "llll" },
  { "__insn_shufflebytes1",      TILEGX_INSN_SHUFFLEBYTES1,      true,  "lll"  },
  { "__insn_st",                 TILEGX_INSN_ST,                 false, "vpl"  },
  { "__insn_st1",                TILEGX_INSN_ST1,                false, "vpl"  },
  { "__insn_st2",                TILEGX_INSN_ST2,                false, "vpl"  },
  { "__insn_st4",                TILEGX_INSN_ST4,                false, "vpl"  },
  { "__insn_stnt",               TILEGX_INSN_STNT,               false, "vpl"  },
  { "__insn_stnt1",              TILEGX_INSN_STNT1,              false, "vpl"  },
  { "__insn_stnt2",              TILEGX_INSN_STNT2,              false, "vpl"  },
  { "__insn_stnt4",              TILEGX_INSN_STNT4,              false, "vpl"  },
  { "__insn_sub",                TILEGX_INSN_SUB,                true,  "lll"  },
  { "__insn_subx",               TILEGX_INSN_SUBX,               true,  "iii"  },
  { "__insn_subxsc",             TILEGX_INSN_SUBXSC,             true,  "iii"  },
  { "__insn_tblidxb0",           TILEGX_INSN_TBLIDXB0,           true,  "lll"  },
  { "__insn_tblidxb1",           TILEGX_INSN_TBLIDXB1,           true,  "lll"  },
  { "__insn_tblidxb2",           TILEGX_INSN_TBLIDXB2,           true,  "lll"  },
  { "__insn_tblidxb3",           TILEGX_INSN_TBLIDXB3,           true,  "lll"  },
  { "__insn_v1add",              TILEGX_INSN_V1ADD,              true,  "lll"  },
  { "__insn_v1addi",             TILEGX_INSN_V1ADDI,             true,  "lll"  },
  { "__insn_v1adduc",            TILEGX_INSN_V1ADDUC,            true,  "lll"  },
  { "__insn_v1adiffu",           TILEGX_INSN_V1ADIFFU,           true,  "lll"  },
  { "__insn_v1avgu",             TILEGX_INSN_V1AVGU,             true,  "lll"  },
  { "__insn_v1cmpeq",            TILEGX_INSN_V1CMPEQ,            true,  "lll"  },
  { "__insn_v1cmpeqi",           TILEGX_INSN_V1CMPEQI,           true,  "lll"  },
  { "__insn_v1cmples",           TILEGX_INSN_V1CMPLES,           true,  "lll"  },
  { "__insn_v1cmpleu",           TILEGX_INSN_V1CMPLEU,           true,  "lll"  },
  { "__insn_v1cmplts",           TILEGX_INSN_V1CMPLTS,           true,  "lll"  },
  { "__insn_v1cmpltsi",          TILEGX_INSN_V1CMPLTSI,          true,  "lll"  },
  { "__insn_v1cmpltu",           TILEGX_INSN_V1CMPLTU,           true,  "lll"  },
  { "__insn_v1cmpltui",          TILEGX_INSN_V1CMPLTUI,          true,  "lll"  },
  { "__insn_v1cmpne",            TILEGX_INSN_V1CMPNE,            true,  "lll"  },
  { "__insn_v1ddotpu",           TILEGX_INSN_V1DDOTPU,           true,  "lll"  },
  { "__insn_v1ddotpua",          TILEGX_INSN_V1DDOTPUA,          true,  "llll" },
  { "__insn_v1ddotpus",          TILEGX_INSN_V1DDOTPUS,          true,  "lll"  },
  { "__insn_v1ddotpusa",         TILEGX_INSN_V1DDOTPUSA,         true,  "llll" },
  { "__insn_v1dotp",             TILEGX_INSN_V1DOTP,             true,  "lll"  },
  { "__insn_v1dotpa",            TILEGX_INSN_V1DOTPA,            true,  "llll" },
  { "__insn_v1dotpu",            TILEGX_INSN_V1DOTPU,            true,  "lll"  },
  { "__insn_v1dotpua",           TILEGX_INSN_V1DOTPUA,           true,  "llll" },
  { "__insn_v1dotpus",           TILEGX_INSN_V1DOTPUS,           true,  "lll"  },
  { "__insn_v1dotpusa",          TILEGX_INSN_V1DOTPUSA,          true,  "llll" },
  { "__insn_v1int_h",            TILEGX_INSN_V1INT_H,            true,  "lll"  },
  { "__insn_v1int_l",            TILEGX_INSN_V1INT_L,            true,  "lll"  },
  { "__insn_v1maxu",             TILEGX_INSN_V1MAXU,             true,  "lll"  },
  { "__insn_v1maxui",            TILEGX_INSN_V1MAXUI,            true,  "lll"  },
  { "__insn_v1minu",             TILEGX_INSN_V1MINU,             true,  "lll"  },
  { "__insn_v1minui",            TILEGX_INSN_V1MINUI,            true,  "lll"  },
  { "__insn_v1mnz",              TILEGX_INSN_V1MNZ,              true,  "lll"  },
  { "__insn_v1multu",            TILEGX_INSN_V1MULTU,            true,  "lll"  },
  { "__insn_v1mulu",             TILEGX_INSN_V1MULU,             true,  "lll"  },
  { "__insn_v1mulus",            TILEGX_INSN_V1MULUS,            true,  "lll"  },
  { "__insn_v1mz",               TILEGX_INSN_V1MZ,               true,  "lll"  },
  { "__insn_v1sadau",            TILEGX_INSN_V1SADAU,            true,  "llll" },
  { "__insn_v1sadu",             TILEGX_INSN_V1SADU,             true,  "lll"  },
  { "__insn_v1shl",              TILEGX_INSN_V1SHL,              true,  "lll"  },
  { "__insn_v1shli",             TILEGX_INSN_V1SHLI,             true,  "lll"  },
  { "__insn_v1shrs",             TILEGX_INSN_V1SHRS,             true,  "lll"  },
  { "__insn_v1shrsi",            TILEGX_INSN_V1SHRSI,            true,  "lll"  },
  { "__insn_v1shru",             TILEGX_INSN_V1SHRU,             true,  "lll"  },
  { "__insn_v1shrui",            TILEGX_INSN_V1SHRUI,            true,  "lll"  },
  { "__insn_v1sub",              TILEGX_INSN_V1SUB,              true,  "lll"  },
  { "__insn_v1subuc",            TILEGX_INSN_V1SUBUC,            true,  "lll"  },
  { "__insn_v2add",              TILEGX_INSN_V2ADD,              true,  "lll"  },
  { "__insn_v2addi",             TILEGX_INSN_V2ADDI,             true,  "lll"  },
  { "__insn_v2addsc",            TILEGX_INSN_V2ADDSC,            true,  "lll"  },
  { "__insn_v2adiffs",           TILEGX_INSN_V2ADIFFS,           true,  "lll"  },
  { "__insn_v2avgs",             TILEGX_INSN_V2AVGS,             true,  "lll"  },
  { "__insn_v2cmpeq",            TILEGX_INSN_V2CMPEQ,            true,  "lll"  },
  { "__insn_v2cmpeqi",           TILEGX_INSN_V2CMPEQI,           true,  "lll"  },
  { "__insn_v2cmples",           TILEGX_INSN_V2CMPLES,           true,  "lll"  },
  { "__insn_v2cmpleu",           TILEGX_INSN_V2CMPLEU,           true,  "lll"  },
  { "__insn_v2cmplts",           TILEGX_INSN_V2CMPLTS,           true,  "lll"  },
  { "__insn_v2cmpltsi",          TILEGX_INSN_V2CMPLTSI,          true,  "lll"  },
  { "__insn_v2cmpltu",           TILEGX_INSN_V2CMPLTU,           true,  "lll"  },
  { "__insn_v2cmpltui",          TILEGX_INSN_V2CMPLTUI,          true,  "lll"  },
  { "__insn_v2cmpne",            TILEGX_INSN_V2CMPNE,            true,  "lll"  },
  { "__insn_v2dotp",             TILEGX_INSN_V2DOTP,             true,  "lll"  },
  { "__insn_v2dotpa",            TILEGX_INSN_V2DOTPA,            true,  "llll" },
  { "__insn_v2int_h",            TILEGX_INSN_V2INT_H,            true,  "lll"  },
  { "__insn_v2int_l",            TILEGX_INSN_V2INT_L,            true,  "lll"  },
  { "__insn_v2maxs",             TILEGX_INSN_V2MAXS,             true,  "lll"  },
  { "__insn_v2maxsi",            TILEGX_INSN_V2MAXSI,            true,  "lll"  },
  { "__insn_v2mins",             TILEGX_INSN_V2MINS,             true,  "lll"  },
  { "__insn_v2minsi",            TILEGX_INSN_V2MINSI,            true,  "lll"  },
  { "__insn_v2mnz",              TILEGX_INSN_V2MNZ,              true,  "lll"  },
  { "__insn_v2mulfsc",           TILEGX_INSN_V2MULFSC,           true,  "lll"  },
  { "__insn_v2muls",             TILEGX_INSN_V2MULS,             true,  "lll"  },
  { "__insn_v2mults",            TILEGX_INSN_V2MULTS,            true,  "lll"  },
  { "__insn_v2mz",               TILEGX_INSN_V2MZ,               true,  "lll"  },
  { "__insn_v2packh",            TILEGX_INSN_V2PACKH,            true,  "lll"  },
  { "__insn_v2packl",            TILEGX_INSN_V2PACKL,            true,  "lll"  },
  { "__insn_v2packuc",           TILEGX_INSN_V2PACKUC,           true,  "lll"  },
  { "__insn_v2sadas",            TILEGX_INSN_V2SADAS,            true,  "llll" },
  { "__insn_v2sadau",            TILEGX_INSN_V2SADAU,            true,  "llll" },
  { "__insn_v2sads",             TILEGX_INSN_V2SADS,             true,  "lll"  },
  { "__insn_v2sadu",             TILEGX_INSN_V2SADU,             true,  "lll"  },
  { "__insn_v2shl",              TILEGX_INSN_V2SHL,              true,  "lll"  },
  { "__insn_v2shli",             TILEGX_INSN_V2SHLI,             true,  "lll"  },
  { "__insn_v2shlsc",            TILEGX_INSN_V2SHLSC,            true,  "lll"  },
  { "__insn_v2shrs",             TILEGX_INSN_V2SHRS,             true,  "lll"  },
  { "__insn_v2shrsi",            TILEGX_INSN_V2SHRSI,            true,  "lll"  },
  { "__insn_v2shru",             TILEGX_INSN_V2SHRU,             true,  "lll"  },
  { "__insn_v2shrui",            TILEGX_INSN_V2SHRUI,            true,  "lll"  },
  { "__insn_v2sub",              TILEGX_INSN_V2SUB,              true,  "lll"  },
  { "__insn_v2subsc",            TILEGX_INSN_V2SUBSC,            true,  "lll"  },
  { "__insn_v4add",              TILEGX_INSN_V4ADD,              true,  "lll"  },
  { "__insn_v4addsc",            TILEGX_INSN_V4ADDSC,            true,  "lll"  },
  { "__insn_v4int_h",            TILEGX_INSN_V4INT_H,            true,  "lll"  },
  { "__insn_v4int_l",            TILEGX_INSN_V4INT_L,            true,  "lll"  },
  { "__insn_v4packsc",           TILEGX_INSN_V4PACKSC,           true,  "lll"  },
  { "__insn_v4shl",              TILEGX_INSN_V4SHL,              true,  "lll"  },
  { "__insn_v4shlsc",            TILEGX_INSN_V4SHLSC,            true,  "lll"  },
  { "__insn_v4shrs",             TILEGX_INSN_V4SHRS,             true,  "lll"  },
  { "__insn_v4shru",             TILEGX_INSN_V4SHRU,             true,  "lll"  },
  { "__insn_v4sub",              TILEGX_INSN_V4SUB,              true,  "lll"  },
  { "__insn_v4subsc",            TILEGX_INSN_V4SUBSC,            true,  "lll"  },
  { "__insn_wh64",               TILEGX_INSN_WH64,               false, "vp"   },
  { "__insn_xor",                TILEGX_INSN_XOR,                true,  "lll"  },
  { "__insn_xori",               TILEGX_INSN_XOR,                true,  "lll"  },
  { "__tile_network_barrier",    TILEGX_NETWORK_BARRIER,         false, "v"    },
  { "__tile_idn0_receive",       TILEGX_IDN0_RECEIVE,            false, "l"    },
  { "__tile_idn1_receive",       TILEGX_IDN1_RECEIVE,            false, "l"    },
  { "__tile_idn_send",           TILEGX_IDN_SEND,                false, "vl"   },
  { "__tile_udn0_receive",       TILEGX_UDN0_RECEIVE,            false, "l"    },
  { "__tile_udn1_receive",       TILEGX_UDN1_RECEIVE,            false, "l"    },
  { "__tile_udn2_receive",       TILEGX_UDN2_RECEIVE,            false, "l"    },
  { "__tile_udn3_receive",       TILEGX_UDN3_RECEIVE,            false, "l"    },
  { "__tile_udn_send",           TILEGX_UDN_SEND,                false, "vl"   },
};


/* Convert a character in a builtin type string to a tree type.  */
static tree
char_to_type (char c)
{
  static tree volatile_ptr_type_node = NULL;
  static tree volatile_const_ptr_type_node = NULL;

  if (volatile_ptr_type_node == NULL)
    {
      volatile_ptr_type_node =
	build_pointer_type (build_qualified_type (void_type_node,
						  TYPE_QUAL_VOLATILE));
      volatile_const_ptr_type_node =
	build_pointer_type (build_qualified_type (void_type_node,
						  TYPE_QUAL_CONST
						  | TYPE_QUAL_VOLATILE));
    }

  switch (c)
    {
    case 'v':
      return void_type_node;
    case 'i':
      return unsigned_type_node;
    case 'l':
      return long_long_unsigned_type_node;
    case 'p':
      return volatile_ptr_type_node;
    case 'k':
      return volatile_const_ptr_type_node;
    default:
      gcc_unreachable ();
    }
}


/* Implement TARGET_INIT_BUILTINS.  */
static void
tilegx_init_builtins (void)
{
  size_t i;

  for (i = 0; i < ARRAY_SIZE (tilegx_builtins); i++)
    {
      const struct tilegx_builtin_def *p = &tilegx_builtins[i];
      tree ftype, ret_type, arg_type_list = void_list_node;
      tree decl;
      int j;

      for (j = strlen (p->type) - 1; j > 0; j--)
	{
	  arg_type_list =
	    tree_cons (NULL_TREE, char_to_type (p->type[j]), arg_type_list);
	}

      ret_type = char_to_type (p->type[0]);

      ftype = build_function_type (ret_type, arg_type_list);

      decl = add_builtin_function (p->name, ftype, p->code, BUILT_IN_MD,
				   NULL, NULL);

      if (p->is_const)
	TREE_READONLY (decl) = 1;
      TREE_NOTHROW (decl) = 1;

      if (tilegx_builtin_info[p->code].fndecl == NULL)
	tilegx_builtin_info[p->code].fndecl = decl;
    }
}


/* Implement TARGET_EXPAND_BUILTIN.  */
static rtx
tilegx_expand_builtin (tree exp,
		       rtx target,
		       rtx subtarget ATTRIBUTE_UNUSED,
		       machine_mode mode ATTRIBUTE_UNUSED,
		       int ignore ATTRIBUTE_UNUSED)
{
#define MAX_BUILTIN_ARGS 4

  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arg;
  call_expr_arg_iterator iter;
  enum insn_code icode;
  rtx op[MAX_BUILTIN_ARGS + 1], pat;
  int opnum;
  bool nonvoid;
  insn_gen_fn fn;

  if (fcode >= TILEGX_BUILTIN_max)
    internal_error ("bad builtin fcode");
  icode = tilegx_builtin_info[fcode].icode;
  if (icode == 0)
    internal_error ("bad builtin icode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;

  opnum = nonvoid;
  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      const struct insn_operand_data *insn_op;

      if (arg == error_mark_node)
	return NULL_RTX;
      if (opnum > MAX_BUILTIN_ARGS)
	return NULL_RTX;

      insn_op = &insn_data[icode].operand[opnum];

      op[opnum] = expand_expr (arg, NULL_RTX, insn_op->mode, EXPAND_NORMAL);

      if (!(*insn_op->predicate) (op[opnum], insn_op->mode))
	{
	  machine_mode opmode = insn_op->mode;

	  /* pointer_operand and pmode_register_operand operands do
	     not specify a mode, so use the operand's mode instead
	     (which should always be right by the time we get here,
	     except for constants, which are VOIDmode).  */
	  if (opmode == VOIDmode)
	    {
	      machine_mode m = GET_MODE (op[opnum]);
	      gcc_assert (m == Pmode || m == VOIDmode);
	      opmode = Pmode;
	    }

	  op[opnum] = copy_to_mode_reg (opmode, op[opnum]);
	}

      if (!(*insn_op->predicate) (op[opnum], insn_op->mode))
	{
	  /* We still failed to meet the predicate even after moving
	     into a register. Assume we needed an immediate.  */
	  error_at (EXPR_LOCATION (exp),
		    "operand must be an immediate of the right size");
	  return const0_rtx;
	}

      opnum++;
    }

  if (nonvoid)
    {
      machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	{
	  if (tmode == VOIDmode)
	    {
	      /* get the mode from the return type.  */
	      tmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (fndecl)));
	    }
	  target = gen_reg_rtx (tmode);
	}
      op[0] = target;
    }

  fn = GEN_FCN (icode);
  switch (opnum)
    {
    case 0:
      pat = fn (NULL_RTX);
      break;
    case 1:
      pat = fn (op[0]);
      break;
    case 2:
      pat = fn (op[0], op[1]);
      break;
    case 3:
      pat = fn (op[0], op[1], op[2]);
      break;
    case 4:
      pat = fn (op[0], op[1], op[2], op[3]);
      break;
    case 5:
      pat = fn (op[0], op[1], op[2], op[3], op[4]);
      break;
    default:
      gcc_unreachable ();
    }
  if (!pat)
    return NULL_RTX;

  /* If we are generating a prefetch, tell the scheduler not to move
     it around.  */
  if (GET_CODE (pat) == PREFETCH)
    PREFETCH_SCHEDULE_BARRIER_P (pat) = true;

  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}


/* Implement TARGET_BUILTIN_DECL.  */
static tree
tilegx_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= TILEGX_BUILTIN_max)
    return error_mark_node;

  return tilegx_builtin_info[code].fndecl;
}



/* Stack frames  */

/* Return whether REGNO needs to be saved in the stack frame.  */
static bool
need_to_save_reg (unsigned int regno)
{
  if (!fixed_regs[regno] && !call_used_regs[regno]
      && df_regs_ever_live_p (regno))
    return true;

  if (flag_pic
      && (regno == PIC_OFFSET_TABLE_REGNUM
	  || regno == TILEGX_PIC_TEXT_LABEL_REGNUM)
      && (crtl->uses_pic_offset_table || crtl->saves_all_registers))
    return true;

  if (crtl->calls_eh_return)
    {
      unsigned i;
      for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; i++)
	{
	  if (regno == EH_RETURN_DATA_REGNO (i))
	    return true;
	}
    }

  return false;
}


/* Return the size of the register savev area.  This function is only
   correct starting with local register allocation */
static int
tilegx_saved_regs_size (void)
{
  int reg_save_size = 0;
  int regno;
  int offset_to_frame;
  int align_mask;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (need_to_save_reg (regno))
      reg_save_size += UNITS_PER_WORD;

  /* Pad out the register save area if necessary to make
     frame_pointer_rtx be as aligned as the stack pointer.  */
  offset_to_frame = crtl->args.pretend_args_size + reg_save_size;
  align_mask = (STACK_BOUNDARY / BITS_PER_UNIT) - 1;
  reg_save_size += (-offset_to_frame) & align_mask;

  return reg_save_size;
}


/* Round up frame size SIZE.  */
static int
round_frame_size (int size)
{
  return ((size + STACK_BOUNDARY / BITS_PER_UNIT - 1)
	  & -STACK_BOUNDARY / BITS_PER_UNIT);
}


/* Emit a store in the stack frame to save REGNO at address ADDR, and
   emit the corresponding REG_CFA_OFFSET note described by CFA and
   CFA_OFFSET.  Return the emitted insn.  */
static rtx
frame_emit_store (int regno, int regno_note, rtx addr, rtx cfa,
		  int cfa_offset)
{
  rtx reg = gen_rtx_REG (DImode, regno);
  rtx mem = gen_frame_mem (DImode, addr);
  rtx mov = gen_movdi (mem, reg);

  /* Describe what just happened in a way that dwarf understands.  We
     use temporary registers to hold the address to make scheduling
     easier, and use the REG_CFA_OFFSET to describe the address as an
     offset from the CFA.  */
  rtx reg_note = gen_rtx_REG (DImode, regno_note);
  rtx cfa_relative_addr = gen_rtx_PLUS (Pmode, cfa, GEN_INT (cfa_offset));
  rtx cfa_relative_mem = gen_frame_mem (DImode, cfa_relative_addr);
  rtx real = gen_rtx_SET (cfa_relative_mem, reg_note);
  add_reg_note (mov, REG_CFA_OFFSET, real);

  return emit_insn (mov);
}


/* Emit a load in the stack frame to load REGNO from address ADDR.
   Add a REG_CFA_RESTORE note to CFA_RESTORES if CFA_RESTORES is
   non-null.  Return the emitted insn.  */
static rtx_insn *
frame_emit_load (int regno, rtx addr, rtx *cfa_restores)
{
  rtx reg = gen_rtx_REG (DImode, regno);
  rtx mem = gen_frame_mem (DImode, addr);
  if (cfa_restores)
    *cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, *cfa_restores);
  return emit_insn (gen_movdi (reg, mem));
}


/* Helper function to set RTX_FRAME_RELATED_P on instructions,
   including sequences.  */
static rtx
set_frame_related_p (void)
{
  rtx_insn *seq = get_insns ();
  rtx_insn *insn;

  end_sequence ();

  if (!seq)
    return NULL_RTX;

  if (INSN_P (seq))
    {
      insn = seq;
      while (insn != NULL_RTX)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  insn = NEXT_INSN (insn);
	}
      seq = emit_insn (seq);
    }
  else
    {
      seq = emit_insn (seq);
      RTX_FRAME_RELATED_P (seq) = 1;
    }
  return seq;
}


#define FRP(exp)  (start_sequence (), exp, set_frame_related_p ())

/* This emits code for 'sp += offset'.
   
   The ABI only allows us to modify 'sp' in a single 'addi' or
   'addli', so the backtracer understands it. Larger amounts cannot
   use those instructions, so are added by placing the offset into a
   large register and using 'add'.

   This happens after reload, so we need to expand it ourselves.  */
static rtx_insn *
emit_sp_adjust (int offset, int *next_scratch_regno, bool frame_related,
		rtx reg_notes)
{
  rtx to_add;
  rtx imm_rtx = GEN_INT (offset);
  rtx pat;
  rtx_insn *insn;

  if (satisfies_constraint_J (imm_rtx))
    {
      /* We can add this using a single immediate add.  */
      to_add = imm_rtx;
    }
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, (*next_scratch_regno)--);
      tilegx_expand_set_const64 (tmp, imm_rtx);
      to_add = tmp;
    }

  /* Actually adjust the stack pointer.  */
  if (TARGET_32BIT)
    pat = gen_sp_adjust_32bit (stack_pointer_rtx, stack_pointer_rtx, to_add);
  else
    pat = gen_sp_adjust (stack_pointer_rtx, stack_pointer_rtx, to_add);

  insn = emit_insn (pat);
  REG_NOTES (insn) = reg_notes;

  /* Describe what just happened in a way that dwarf understands.  */
  if (frame_related)
    {
      rtx real = gen_rtx_SET (stack_pointer_rtx,
			      gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					    imm_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_ADJUST_CFA, real);
    }

  return insn;
}


/* Return whether the current function is leaf.  This takes into
   account whether the function calls tls_get_addr.  */
static bool
tilegx_current_function_is_leaf (void)
{
  return crtl->is_leaf && !cfun->machine->calls_tls_get_addr;
}


/* Return the frame size.  */
static int
compute_total_frame_size (void)
{
  int total_size = (get_frame_size () + tilegx_saved_regs_size ()
		    + crtl->outgoing_args_size
		    + crtl->args.pretend_args_size);

  if (!tilegx_current_function_is_leaf () || cfun->calls_alloca)
    {
      /* Make room for save area in callee.  */
      total_size += STACK_POINTER_OFFSET;
    }

  return round_frame_size (total_size);
}


/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack was
   created.  */
bool
tilegx_can_use_return_insn_p (void)
{
  return (reload_completed
	  && !cfun->static_chain_decl
	  && !compute_total_frame_size ()
	  && tilegx_current_function_is_leaf ()
	  && !crtl->profile && !df_regs_ever_live_p (TILEGX_LINK_REGNUM));
}


/* Returns an rtx for a stack slot at 'FP + offset_from_fp'.  If there
   is a frame pointer, it computes the value relative to
   that. Otherwise it uses the stack pointer.  */
static rtx
compute_frame_addr (int offset_from_fp, int *next_scratch_regno)
{
  rtx base_reg_rtx, tmp_reg_rtx, offset_rtx;
  int offset_from_base;

  if (frame_pointer_needed)
    {
      base_reg_rtx = hard_frame_pointer_rtx;
      offset_from_base = offset_from_fp;
    }
  else
    {
      int offset_from_sp = compute_total_frame_size () + offset_from_fp;
      offset_from_base = offset_from_sp;
      base_reg_rtx = stack_pointer_rtx;
    }

  if (offset_from_base == 0)
    return base_reg_rtx;

  /* Compute the new value of the stack pointer.  */
  tmp_reg_rtx = gen_rtx_REG (Pmode, (*next_scratch_regno)--);
  offset_rtx = GEN_INT (offset_from_base);

  if (!add_operand (offset_rtx, Pmode))
    {
      expand_set_cint64 (tmp_reg_rtx, offset_rtx);
      offset_rtx = tmp_reg_rtx;
    }

  emit_insn (gen_rtx_SET (tmp_reg_rtx,
			  gen_rtx_PLUS (Pmode, base_reg_rtx, offset_rtx)));

  return tmp_reg_rtx;
}


/* The stack frame looks like this:
         +-------------+
         |    ...      | 
         |  incoming   | 
         | stack args  | 
   AP -> +-------------+
         | caller's HFP|
         +-------------+
         | lr save     |
  HFP -> +-------------+
         |  var args   | 
         |  reg save   | crtl->args.pretend_args_size bytes
         +-------------+
         |    ...      | 
         | saved regs  | tilegx_saved_regs_size() bytes
   FP -> +-------------+
         |    ...      | 
         |   vars      | get_frame_size() bytes
         +-------------+
         |    ...      | 
         |  outgoing   | 
         |  stack args | crtl->outgoing_args_size bytes
         +-------------+
         | HFP         | ptr_size bytes (only here if nonleaf / alloca)
         +-------------+
         | callee lr   | ptr_size bytes (only here if nonleaf / alloca)
         | save        | 
   SP -> +-------------+

  HFP == incoming SP.

  For functions with a frame larger than 32767 bytes, or which use
  alloca (), r52 is used as a frame pointer.  Otherwise there is no
  frame pointer.

  FP is saved at SP+ptr_size before calling a subroutine so the callee
  can chain.  */
void
tilegx_expand_prologue (void)
{
#define ROUND_ROBIN_SIZE 4
  /* We round-robin through four scratch registers to hold temporary
     addresses for saving registers, to make instruction scheduling
     easier.  */
  rtx reg_save_addr[ROUND_ROBIN_SIZE] = {
    NULL_RTX, NULL_RTX, NULL_RTX, NULL_RTX
  };
  rtx insn, cfa;
  unsigned int which_scratch;
  int offset, start_offset, regno;

  /* A register that holds a copy of the incoming fp.  */
  int fp_copy_regno = -1;

  /* A register that holds a copy of the incoming sp.  */
  int sp_copy_regno = -1;

  /* Next scratch register number to hand out (postdecrementing).  */
  int next_scratch_regno = 29;

  int total_size = compute_total_frame_size ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = total_size;

  /* Save lr first in its special location because code after this
     might use the link register as a scratch register.  */
  if (df_regs_ever_live_p (TILEGX_LINK_REGNUM) || crtl->calls_eh_return)
    {
      FRP (frame_emit_store (TILEGX_LINK_REGNUM, TILEGX_LINK_REGNUM,
			     stack_pointer_rtx, stack_pointer_rtx, 0));
      emit_insn (gen_blockage ());
    }

  if (total_size == 0)
    {
      /* Load the PIC register if needed.  */
      if (flag_pic && crtl->uses_pic_offset_table)
	load_pic_register (false);

      return;
    }

  cfa = stack_pointer_rtx;

  if (frame_pointer_needed)
    {
      fp_copy_regno = next_scratch_regno--;

      /* Copy the old frame pointer aside so we can save it later.  */
      insn =
	FRP (emit_move_insn (gen_rtx_REG (word_mode, fp_copy_regno),
			     gen_lowpart (word_mode, hard_frame_pointer_rtx)));
      add_reg_note (insn, REG_CFA_REGISTER, NULL_RTX);

      /* Set up the frame pointer.  */
      insn = FRP (emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx));
      add_reg_note (insn, REG_CFA_DEF_CFA, hard_frame_pointer_rtx);
      cfa = hard_frame_pointer_rtx;
      REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = STACK_BOUNDARY;

      /* fp holds a copy of the incoming sp, in case we need to store
	 it.  */
      sp_copy_regno = HARD_FRAME_POINTER_REGNUM;
    }
  else if (!tilegx_current_function_is_leaf ())
    {
      /* Copy the old stack pointer aside so we can save it later.  */
      sp_copy_regno = next_scratch_regno--;
      emit_move_insn (gen_rtx_REG (Pmode, sp_copy_regno),
		      stack_pointer_rtx);
    }

  if (tilegx_current_function_is_leaf ())
    {
      /* No need to store chain pointer to caller's frame.  */
      emit_sp_adjust (-total_size, &next_scratch_regno,
		      !frame_pointer_needed, NULL_RTX);
    }
  else
    {
      /* Save the frame pointer (incoming sp value) to support
         backtracing.  First we need to create an rtx with the store
         address.  */
      rtx chain_addr = gen_rtx_REG (Pmode, next_scratch_regno--);
      rtx size_rtx = GEN_INT (-(total_size - UNITS_PER_WORD));

      if (add_operand (size_rtx, Pmode))
	{
	  /* Expose more parallelism by computing this value from the
	     original stack pointer, not the one after we have pushed
	     the frame.  */
	  rtx p = gen_rtx_PLUS (Pmode, stack_pointer_rtx, size_rtx);
	  emit_insn (gen_rtx_SET (chain_addr, p));
	  emit_sp_adjust (-total_size, &next_scratch_regno,
			  !frame_pointer_needed, NULL_RTX);
	}
      else
	{
	  /* The stack frame is large, so just store the incoming sp
	     value at *(new_sp + UNITS_PER_WORD).  */
	  rtx p;
	  emit_sp_adjust (-total_size, &next_scratch_regno,
			  !frame_pointer_needed, NULL_RTX);
	  p = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			    GEN_INT (UNITS_PER_WORD));
	  emit_insn (gen_rtx_SET (chain_addr, p));
	}

      /* Save our frame pointer for backtrace chaining.  */
      emit_insn (gen_movdi (gen_frame_mem (DImode, chain_addr),
			    gen_rtx_REG (DImode, sp_copy_regno)));
    }

  /* Compute where to start storing registers we need to save.  */
  start_offset = -crtl->args.pretend_args_size - UNITS_PER_WORD;
  offset = start_offset;

  /* Store all registers that need saving.  */
  which_scratch = 0;
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (need_to_save_reg (regno))
      {
	rtx r = reg_save_addr[which_scratch];
	int from_regno;
	int cfa_offset = frame_pointer_needed ? offset : total_size + offset;

	if (r == NULL_RTX)
	  {
	    int prev_scratch_regno = next_scratch_regno;
	    r = compute_frame_addr (offset, &next_scratch_regno);
	    if (prev_scratch_regno != next_scratch_regno)
	      reg_save_addr[which_scratch] = r;
	  }
	else
	  {
	    /* Advance to the next stack slot to store this
	       register.  */
	    int stride = ROUND_ROBIN_SIZE * -UNITS_PER_WORD;
	    rtx p = gen_rtx_PLUS (Pmode, r, GEN_INT (stride));
	    emit_insn (gen_rtx_SET (r, p));
	  }

	/* Save this register to the stack (but use the old fp value
	   we copied aside if appropriate).  */
	from_regno =
	  (fp_copy_regno >= 0 && regno == HARD_FRAME_POINTER_REGNUM)
	  ? fp_copy_regno : regno;
	FRP (frame_emit_store (from_regno, regno, r, cfa, cfa_offset));

	offset -= UNITS_PER_WORD;
	which_scratch = (which_scratch + 1) % ROUND_ROBIN_SIZE;
      }

  /* If profiling, force that to happen after the frame is set up.  */
  if (crtl->profile)
    emit_insn (gen_blockage ());

  /* Load the PIC register if needed.  */
  if (flag_pic && crtl->uses_pic_offset_table)
    load_pic_register (false);
}


/* Implement the epilogue and sibcall_epilogue patterns.  SIBCALL_P is
   true for a sibcall_epilogue pattern, and false for an epilogue
   pattern.  */
void
tilegx_expand_epilogue (bool sibcall_p)
{
  /* We round-robin through four scratch registers to hold temporary
     addresses for saving registers, to make instruction scheduling
     easier.  */
  rtx reg_save_addr[ROUND_ROBIN_SIZE] = {
    NULL_RTX, NULL_RTX, NULL_RTX, NULL_RTX
  };
  rtx_insn *last_insn, *insn;
  unsigned int which_scratch;
  int offset, start_offset, regno;
  rtx cfa_restores = NULL_RTX;

  /* A register that holds a copy of the incoming fp.  */
  int fp_copy_regno = -1;

  /* Next scratch register number to hand out (postdecrementing).  */
  int next_scratch_regno = 29;

  int total_size = compute_total_frame_size ();

  last_insn = get_last_insn ();

  /* Load lr first since we are going to need it first.  */
  insn = NULL;
  if (df_regs_ever_live_p (TILEGX_LINK_REGNUM))
    {
      insn = frame_emit_load (TILEGX_LINK_REGNUM,
			      compute_frame_addr (0, &next_scratch_regno),
			      &cfa_restores);
    }

  if (total_size == 0)
    {
      if (insn)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  REG_NOTES (insn) = cfa_restores;
	}
      goto done;
    }

  /* Compute where to start restoring registers.  */
  start_offset = -crtl->args.pretend_args_size - UNITS_PER_WORD;
  offset = start_offset;

  if (frame_pointer_needed)
    fp_copy_regno = next_scratch_regno--;

  /* Restore all callee-saved registers.  */
  which_scratch = 0;
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (need_to_save_reg (regno))
      {
	rtx r = reg_save_addr[which_scratch];
	if (r == NULL_RTX)
	  {
	    r = compute_frame_addr (offset, &next_scratch_regno);
	    reg_save_addr[which_scratch] = r;
	  }
	else
	  {
	    /* Advance to the next stack slot to store this register.  */
	    int stride = ROUND_ROBIN_SIZE * -UNITS_PER_WORD;
	    rtx p = gen_rtx_PLUS (Pmode, r, GEN_INT (stride));
	    emit_insn (gen_rtx_SET (r, p));
	  }

	if (fp_copy_regno >= 0 && regno == HARD_FRAME_POINTER_REGNUM)
	  frame_emit_load (fp_copy_regno, r, NULL);
	else
	  frame_emit_load (regno, r, &cfa_restores);

	offset -= UNITS_PER_WORD;
	which_scratch = (which_scratch + 1) % ROUND_ROBIN_SIZE;
      }

  if (!tilegx_current_function_is_leaf ())
    cfa_restores =
      alloc_reg_note (REG_CFA_RESTORE, stack_pointer_rtx, cfa_restores);

  emit_insn (gen_blockage ());

  if (frame_pointer_needed)
    {
      /* Restore the old stack pointer by copying from the frame
	 pointer.  */
      if (TARGET_32BIT)
	{
	  insn = emit_insn (gen_sp_restore_32bit (stack_pointer_rtx,
						  hard_frame_pointer_rtx));
	}
      else
	{
	  insn = emit_insn (gen_sp_restore (stack_pointer_rtx,
					    hard_frame_pointer_rtx));
	}
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = cfa_restores;
      add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);
    }
  else
    {
      insn = emit_sp_adjust (total_size, &next_scratch_regno, true,
			     cfa_restores);
    }

  if (crtl->calls_eh_return)
    {
      if (TARGET_32BIT)
	emit_insn (gen_sp_adjust_32bit (stack_pointer_rtx, stack_pointer_rtx,
					EH_RETURN_STACKADJ_RTX));
      else
	emit_insn (gen_sp_adjust (stack_pointer_rtx, stack_pointer_rtx,
				  EH_RETURN_STACKADJ_RTX));
    }

  /* Restore the old frame pointer.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (gen_lowpart (DImode, hard_frame_pointer_rtx),
			     gen_rtx_REG (DImode, fp_copy_regno));
      add_reg_note (insn, REG_CFA_RESTORE, hard_frame_pointer_rtx);
    }

  /* Mark the pic registers as live outside of the function.  */
  if (flag_pic)
    {
      emit_use (cfun->machine->text_label_rtx);
      emit_use (cfun->machine->got_rtx);
    }

done:
  if (!sibcall_p)
    {
      emit_jump_insn (gen__return ());
    }
  else
    {
      emit_use (gen_rtx_REG (Pmode, TILEGX_LINK_REGNUM));
    }

  /* Mark all insns we just emitted as frame-related.  */
  for (; last_insn != NULL_RTX; last_insn = next_insn (last_insn))
    RTX_FRAME_RELATED_P (last_insn) = 1;
}

#undef ROUND_ROBIN_SIZE


/* Implement INITIAL_ELIMINATION_OFFSET.  */
int
tilegx_initial_elimination_offset (int from, int to)
{
  int total_size = compute_total_frame_size ();

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return (total_size - crtl->args.pretend_args_size
	      - tilegx_saved_regs_size ());
    }
  else if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    {
      return -(crtl->args.pretend_args_size + tilegx_saved_regs_size ());
    }
  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return STACK_POINTER_OFFSET + total_size;
    }
  else if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    {
      return STACK_POINTER_OFFSET;
    }
  else
    gcc_unreachable ();
}


/* Return an RTX indicating where the return address to the calling
   function can be found.  */
rtx
tilegx_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, TILEGX_LINK_REGNUM);
}


/* Implement EH_RETURN_HANDLER_RTX.  The MEM needs to be volatile to
   prevent it from being deleted.  */
rtx
tilegx_eh_return_handler_rtx (void)
{
  rtx tmp = gen_frame_mem (Pmode, hard_frame_pointer_rtx);
  MEM_VOLATILE_P (tmp) = true;
  return tmp;
}



/* Registers  */

/* Implemnet TARGET_CONDITIONAL_REGISTER_USAGE.  */
static void
tilegx_conditional_register_usage (void)
{
  global_regs[TILEGX_NETORDER_REGNUM] = 1;
  /* TILEGX_PIC_TEXT_LABEL_REGNUM is conditionally used.  It is a
     member of fixed_regs, and therefore must be member of
     call_used_regs, but it is not a member of call_really_used_regs[]
     because it is not clobbered by a call.  */
  if (TILEGX_PIC_TEXT_LABEL_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[TILEGX_PIC_TEXT_LABEL_REGNUM] = 1;
      call_used_regs[TILEGX_PIC_TEXT_LABEL_REGNUM] = 1;
    }
  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
}


/* Implement TARGET_FRAME_POINTER_REQUIRED.  */
static bool
tilegx_frame_pointer_required (void)
{
  return crtl->calls_eh_return || cfun->calls_alloca;
}



/* Scheduling and reorg  */

/* Return the length of INSN.  LENGTH is the initial length computed
   by attributes in the machine-description file.  This is where we
   account for bundles.  */
int
tilegx_adjust_insn_length (rtx_insn *insn, int length)
{
  machine_mode mode = GET_MODE (insn);

  /* A non-termininating instruction in a bundle has length 0.  */
  if (mode == SImode)
    return 0;

  /* By default, there is not length adjustment.  */
  return length;
}


/* Implement TARGET_SCHED_ISSUE_RATE.  */
static int
tilegx_issue_rate (void)
{
  return 3;
}


/* Return the rtx for the jump target.  */
static rtx
get_jump_target (rtx branch)
{
  if (CALL_P (branch))
    {
      rtx call;
      call = PATTERN (branch);

      if (GET_CODE (call) == PARALLEL)
	call = XVECEXP (call, 0, 0);

      if (GET_CODE (call) == SET)
	call = SET_SRC (call);

      if (GET_CODE (call) == CALL)
	return XEXP (XEXP (call, 0), 0);
    }
  return 0;
}


/* Implement TARGET_SCHED_ADJUST_COST.  */
static int
tilegx_sched_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn,
			  int cost, unsigned int)
{
  /* If we have a true dependence, INSN is a call, and DEP_INSN
     defines a register that is needed by the call (argument or stack
     pointer) , set its latency to 0 so that it can be bundled with
     the call.  Explicitly check for and exclude the case when
     DEP_INSN defines the target of the jump.  */
  if (CALL_P (insn) && dep_type == REG_DEP_TRUE)
    {
      rtx target = get_jump_target (insn);
      if (!REG_P (target) || !set_of (target, dep_insn))
	return 0;
    }

  return cost;
}


/* Skip over irrelevant NOTEs and such and look for the next insn we
   would consider bundling.  */
static rtx_insn *
next_insn_to_bundle (rtx_insn *r, rtx_insn *end)
{
  for (; r != end; r = NEXT_INSN (r))
    {
      if (NONDEBUG_INSN_P (r)
	  && GET_CODE (PATTERN (r)) != USE
	  && GET_CODE (PATTERN (r)) != CLOBBER)
	return r;
    }

  return NULL;
}


/* Go through all insns, and use the information generated during
   scheduling to generate SEQUENCEs to represent bundles of
   instructions issued simultaneously.  */
static void
tilegx_gen_bundles (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn, *next, *prev;
      rtx_insn *end = NEXT_INSN (BB_END (bb));

      prev = NULL;
      for (insn = next_insn_to_bundle (BB_HEAD (bb), end); insn; insn = next)
	{
	  next = next_insn_to_bundle (NEXT_INSN (insn), end);

	  /* Never wrap {} around inline asm.  */
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT)
	    {
	      if (next == NULL_RTX || GET_MODE (next) == TImode
		  /* NOTE: The scheduler incorrectly believes a call
		     insn can execute in the same cycle as the insn
		     after the call.  This is of course impossible.
		     Really we need to fix the scheduler somehow, so
		     the code after the call gets scheduled
		     optimally.  */
		  || CALL_P (insn))
		{
		  /* Mark current insn as the end of a bundle.  */
		  PUT_MODE (insn, QImode);
		}
	      else
		{
		  /* Mark it as part of a bundle.  */
		  PUT_MODE (insn, SImode);
		}
	    }

	  /* Delete barrier insns, because they can mess up the
	     emitting of bundle braces.  If it is end-of-bundle, then
	     the previous insn must be marked end-of-bundle.  */
	  if (get_attr_type (insn) == TYPE_NOTHING) {
	    if (GET_MODE (insn) == QImode && prev != NULL
		&& GET_MODE (prev) == SImode)
	      {
		PUT_MODE (prev, QImode);
	      }
	    delete_insn (insn);

            // Note: prev remains the same for next iteration.
	  }
          else
            prev = insn;
	}
    }
}


/* Replace OLD_INSN with NEW_INSN.  */
static void
replace_insns (rtx_insn *old_insn, rtx_insn *new_insns)
{
  if (new_insns)
    emit_insn_before (new_insns, old_insn);

  delete_insn (old_insn);
}


/* Returns true if INSN is the first instruction of a pc-relative
   address compuatation.  */
static bool
match_pcrel_step1 (rtx insn)
{
  rtx pattern = PATTERN (insn);
  rtx src;

  if (GET_CODE (pattern) != SET)
    return false;

  src = SET_SRC (pattern);

  return (GET_CODE (src) == CONST
	  && GET_CODE (XEXP (src, 0)) == UNSPEC
	  && XINT (XEXP (src, 0), 1) == UNSPEC_HW1_LAST_PCREL);
}


/* Do the first replacement step in tilegx_fixup_pcrel_references.  */
static void
replace_mov_pcrel_step1 (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  rtx unspec;
  rtx opnds[2];
  rtx_insn *new_insns;

  gcc_assert (GET_CODE (pattern) == SET);
  opnds[0] = SET_DEST (pattern);

  gcc_assert (GET_CODE (SET_SRC (pattern)) == CONST);

  unspec = XEXP (SET_SRC (pattern), 0);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_HW1_LAST_PCREL);
  opnds[1] = XVECEXP (unspec, 0, 0);

  /* We only need to replace SYMBOL_REFs, not LABEL_REFs.  */
  if (GET_CODE (opnds[1]) != SYMBOL_REF)
    return;

  start_sequence ();

  if (flag_pic != 1)
    {
      if (TARGET_32BIT)
	emit_insn (gen_mov_got32_step1_32bit (opnds[0], opnds[1]));
      else
	emit_insn (gen_mov_got32_step1 (opnds[0], opnds[1]));
    }

  new_insns = get_insns ();
  end_sequence ();

  replace_insns (insn, new_insns);
}


/* Returns true if INSN is the second instruction of a pc-relative
   address compuatation.  */
static bool
match_pcrel_step2 (rtx_insn *insn)
{
  rtx unspec;
  rtx addr;

  if (TARGET_32BIT)
    {
      if (recog_memoized (insn) != CODE_FOR_insn_addr_shl16insli_32bit)
	return false;
    }
  else
    {
      if (recog_memoized (insn) != CODE_FOR_insn_addr_shl16insli)
	return false;
    }

  unspec = SET_SRC (PATTERN (insn));
  addr = XVECEXP (unspec, 0, 1);

  return (GET_CODE (addr) == CONST
	  && GET_CODE (XEXP (addr, 0)) == UNSPEC
	  && XINT (XEXP (addr, 0), 1) == UNSPEC_HW0_PCREL);
}


/* Do the second replacement step in tilegx_fixup_pcrel_references.  */
static void
replace_mov_pcrel_step2 (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  rtx unspec;
  rtx addr;
  rtx opnds[3];
  rtx_insn *new_insns;
  rtx got_rtx = tilegx_got_rtx ();

  gcc_assert (GET_CODE (pattern) == SET);
  opnds[0] = SET_DEST (pattern);

  unspec = SET_SRC (pattern);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_INSN_ADDR_SHL16INSLI);

  opnds[1] = XVECEXP (unspec, 0, 0);

  addr = XVECEXP (unspec, 0, 1);
  gcc_assert (GET_CODE (addr) == CONST);

  unspec = XEXP (addr, 0);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_HW0_PCREL);
  opnds[2] = XVECEXP (unspec, 0, 0);

  /* We only need to replace SYMBOL_REFs, not LABEL_REFs.  */
  if (GET_CODE (opnds[2]) != SYMBOL_REF)
    return;

  start_sequence ();

  if (flag_pic == 1)
    {
      if (TARGET_32BIT)
	emit_insn (gen_add_got16_32bit (opnds[0], got_rtx, opnds[2]));
      else
	emit_insn (gen_add_got16 (opnds[0], got_rtx, opnds[2]));
    }
  else
    {
      if (TARGET_32BIT)
	emit_insn (gen_mov_got32_step2_32bit
		   (opnds[0], opnds[1], opnds[2]));
      else
	emit_insn (gen_mov_got32_step2 (opnds[0], opnds[1], opnds[2]));
    }

  new_insns = get_insns ();
  end_sequence ();

  replace_insns (insn, new_insns);
}


/* Do the third replacement step in tilegx_fixup_pcrel_references.  */
static void
replace_mov_pcrel_step3 (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  rtx unspec;
  rtx opnds[4];
  rtx_insn *new_insns;
  rtx got_rtx = tilegx_got_rtx ();
  rtx text_label_rtx = tilegx_text_label_rtx ();

  gcc_assert (GET_CODE (pattern) == SET);
  opnds[0] = SET_DEST (pattern);

  unspec = SET_SRC (pattern);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_MOV_PCREL_STEP3);

  opnds[1] = got_rtx;

  if (XVECEXP (unspec, 0, 0) == text_label_rtx)
    opnds[2] = XVECEXP (unspec, 0, 1);
  else
    {
      gcc_assert (XVECEXP (unspec, 0, 1) == text_label_rtx);
      opnds[2] = XVECEXP (unspec, 0, 0);
    }

  opnds[3] = XVECEXP (unspec, 0, 2);

  /* We only need to replace SYMBOL_REFs, not LABEL_REFs.  */
  if (GET_CODE (opnds[3]) != SYMBOL_REF)
    return;

  start_sequence ();

  if (flag_pic == 1)
    {
      emit_move_insn (opnds[0], gen_const_mem (Pmode, opnds[2]));
    }
  else
    {
      emit_move_insn (opnds[0], gen_rtx_PLUS (Pmode, opnds[1], opnds[2]));
      emit_move_insn (opnds[0], gen_const_mem (Pmode, opnds[0]));
    }

  new_insns = get_insns ();
  end_sequence ();

  replace_insns (insn, new_insns);
}


/* We generate PC relative SYMBOL_REFs as an optimization, to avoid
   going through the GOT when the symbol is local to the compilation
   unit.  But such a symbol requires that the common text_label that
   we generate at the beginning of the function be in the same section
   as the reference to the SYMBOL_REF.  This may not be true if we
   generate hot/cold sections.  This function looks for such cases and
   replaces such references with the longer sequence going through the
   GOT.

   We expect following instruction sequence:
   moveli      tmp1, hw1_last(x-.L_PICLNK)          [1]
   shl16insli  tmp2, tmp1, hw0(x-.L_PICLNK)         [2]
   add<x>      tmp3, txt_label_reg, tmp2            [3]

   If we're compiling -fpic, we replace with the following sequence
   (the numbers in brackets match the instructions they're replacing
   above).

   add<x>li    tmp2, got_reg, hw0_last_got(x)       [2]
   ld<4>       tmp3, tmp2                           [3]

   If we're compiling -fPIC, we replace the first instruction with:

   moveli      tmp1, hw1_last_got(x)                [1]
   shl16insli  tmp2, tmp1, hw0_got(x)               [2]
   add<x>      tmp3, got_reg, tmp2                  [3]
   ld<4>       tmp3, tmp3                           [3]

   Note that we're careful to disturb the instruction sequence as
   little as possible, since it's very late in the compilation
   process.  */
static void
tilegx_fixup_pcrel_references (void)
{
  rtx_insn *insn, *next_insn;
  bool same_section_as_entry = true;

  for (insn = get_insns (); insn; insn = next_insn)
    {
      next_insn = NEXT_INSN (insn);

      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
	{
	  same_section_as_entry = !same_section_as_entry;
	  continue;
	}

      if (same_section_as_entry)
	continue;

      if (!(INSN_P (insn)
	    && GET_CODE (PATTERN (insn)) != USE
	    && GET_CODE (PATTERN (insn)) != CLOBBER))
	continue;

      if (TARGET_32BIT)
	{
	  if (match_pcrel_step1 (insn))
	    replace_mov_pcrel_step1 (insn);
	  else if (match_pcrel_step2 (insn))
	    replace_mov_pcrel_step2 (insn);
	  else if (recog_memoized (insn) == CODE_FOR_mov_pcrel_step3_32bit)
	    replace_mov_pcrel_step3 (insn);
	}
      else
	{
	  if (match_pcrel_step1 (insn))
	    replace_mov_pcrel_step1 (insn);
	  else if (match_pcrel_step2 (insn))
	    replace_mov_pcrel_step2 (insn);
	  else if (recog_memoized (insn) == CODE_FOR_mov_pcrel_step3)
	    replace_mov_pcrel_step3 (insn);
	}
    }
}


/* Ensure that no var tracking notes are emitted in the middle of a
   three-instruction bundle.  */
static void
reorder_var_tracking_notes (void)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
  {
    rtx_insn *insn, *next;
    rtx_insn *queue = NULL;
    bool in_bundle = false;

    for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = next)
      {
	next = NEXT_INSN (insn);

	if (INSN_P (insn))
	  {
	    /* Emit queued up notes at the last instruction of a
	       bundle.  */
	    if (GET_MODE (insn) == QImode)
	      {
		while (queue)
		  {
		    rtx_insn *next_queue = PREV_INSN (queue);
		    SET_PREV_INSN (NEXT_INSN (insn)) = queue;
		    SET_NEXT_INSN (queue) = NEXT_INSN (insn);
		    SET_NEXT_INSN (insn) = queue;
		    SET_PREV_INSN (queue) = insn;
		    queue = next_queue;
		  }
		in_bundle = false;
	      }
	    else if (GET_MODE (insn) == SImode)
	      in_bundle = true;
	  }
	else if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION)
	  {
	    if (in_bundle)
	      {
		rtx_insn *prev = PREV_INSN (insn);
		SET_PREV_INSN (next) = prev;
		SET_NEXT_INSN (prev) = next;

		SET_PREV_INSN (insn) = queue;
		queue = insn;
	      }
	  }
      }
  }
}


/* Perform machine dependent operations on the rtl chain INSNS.  */
static void
tilegx_reorg (void)
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it
     now.  */
  compute_bb_for_insn ();

  if (flag_reorder_blocks_and_partition)
    {
      tilegx_fixup_pcrel_references ();
    }

  if (flag_schedule_insns_after_reload)
    {
      split_all_insns ();

      timevar_push (TV_SCHED2);
      schedule_insns ();
      timevar_pop (TV_SCHED2);

      /* Examine the schedule to group into bundles.  */
      tilegx_gen_bundles ();
    }

  df_analyze ();

  if (flag_var_tracking)
    {
      timevar_push (TV_VAR_TRACKING);
      variable_tracking_main ();
      reorder_var_tracking_notes ();
      timevar_pop (TV_VAR_TRACKING);
    }

  df_finish_pass (false);
}



/* Assembly  */

/* Select a format to encode pointers in exception handling data.
   CODE is 0 for data, 1 for code labels, 2 for function pointers.
   GLOBAL is true if the symbol may be affected by dynamic
   relocations.  */
int
tilegx_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
  int type = TARGET_32BIT ? DW_EH_PE_sdata4 : DW_EH_PE_sdata8;
  return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
}


/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  */
static void
tilegx_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			tree function)
{
  rtx this_rtx, funexp, addend;
  rtx_insn *insn;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  If the function returns a structure,
     the structure return pointer is in $1.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, 1);
  else
    this_rtx = gen_rtx_REG (Pmode, 0);

  /* Add DELTA to THIS_RTX.  */
  if (!(delta >= -32868 && delta <= 32767))
    {
      addend = gen_rtx_REG (Pmode, 29);
      emit_move_insn (addend, GEN_INT (delta));
    }
  else
    addend = GEN_INT (delta);

  if (TARGET_32BIT)
    emit_insn (gen_addsi3 (this_rtx, this_rtx, addend));
  else
    emit_insn (gen_adddi3 (this_rtx, this_rtx, addend));

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset)
    {
      rtx tmp;

      tmp = gen_rtx_REG (Pmode, 29);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));

      if (!(vcall_offset >= -32868 && vcall_offset <= 32767))
	{
	  addend = gen_rtx_REG (Pmode, 28);
	  emit_move_insn (addend, GEN_INT (vcall_offset));
	}
      else
	addend = GEN_INT (vcall_offset);

      if (TARGET_32BIT)
	emit_insn (gen_addsi3 (tmp, tmp, addend));
      else
	emit_insn (gen_adddi3 (tmp, tmp, addend));

      emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));

      if (TARGET_32BIT)
	emit_insn (gen_addsi3 (this_rtx, this_rtx, tmp));
      else
	emit_insn (gen_adddi3 (this_rtx, this_rtx, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  Note that use_thunk calls
     assemble_start_function and assemble_end_function.

     We don't currently bundle, but the instruciton sequence is all
     serial except for the tail call, so we're only wasting one cycle.
   */
  insn = get_insns ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}


/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.  */
static void
tilegx_asm_trampoline_template (FILE *file)
{
  int ptr_mode_size = GET_MODE_SIZE (ptr_mode);
  if (TARGET_32BIT)
    {
      fprintf (file, "\tlnk      r10\n");
      fprintf (file, "\taddxi    r10, r10, 32\n");
      fprintf (file, "\tld4s_add r11, r10, %d\n", ptr_mode_size);
      fprintf (file, "\tld4s     r10, r10\n");
      fprintf (file, "\tjr       r11\n");
      fprintf (file, "\t.word 0 # <function address>\n");
      fprintf (file, "\t.word 0 # <static chain value>\n");
    }
  else
    {
      fprintf (file, "\tlnk      r10\n");
      fprintf (file, "\taddi     r10, r10, 32\n");
      fprintf (file, "\tld_add   r11, r10, %d\n", ptr_mode_size);
      fprintf (file, "\tld       r10, r10\n");
      fprintf (file, "\tjr       r11\n");
      fprintf (file, "\t.quad 0 # <function address>\n");
      fprintf (file, "\t.quad 0 # <static chain value>\n");
    }
}


/* Implement TARGET_TRAMPOLINE_INIT.  */
static void
tilegx_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx fnaddr, chaddr;
  rtx mem;
  rtx begin_addr, end_addr;
  int ptr_mode_size = GET_MODE_SIZE (ptr_mode);

  fnaddr = copy_to_reg (XEXP (DECL_RTL (fndecl), 0));
  chaddr = copy_to_reg (static_chain);

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, ptr_mode,
			TRAMPOLINE_SIZE - 2 * ptr_mode_size);
  emit_move_insn (mem, fnaddr);
  mem = adjust_address (m_tramp, ptr_mode,
			TRAMPOLINE_SIZE - ptr_mode_size);
  emit_move_insn (mem, chaddr);

  /* Get pointers to the beginning and end of the code block.  */
  begin_addr = force_reg (Pmode, XEXP (m_tramp, 0));
  end_addr = force_reg (Pmode, plus_constant (Pmode, XEXP (m_tramp, 0),
					      TRAMPOLINE_SIZE));

  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
		     LCT_NORMAL, VOIDmode, begin_addr, Pmode,
		     end_addr, Pmode);
}


/* Implement TARGET_PRINT_OPERAND.  */
static void
tilegx_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'c':
      /* Print the compare operator opcode for conditional moves.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("z", file);
	  break;
	case NE:
	  fputs ("nz", file);
	  break;
	default:
	  output_operand_lossage ("invalid %%c operand");
	}
      return;

    case 'C':
      /* Print the compare operator opcode for conditional moves.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("nz", file);
	  break;
	case NE:
	  fputs ("z", file);
	  break;
	default:
	  output_operand_lossage ("invalid %%C operand");
	}
      return;

    case 'd':
      {
	/* Print the compare operator opcode for conditional moves.  */
	switch (GET_CODE (x))
	  {
	  case EQ:
	    fputs ("eq", file);
	    break;
	  case NE:
	    fputs ("ne", file);
	    break;
	  default:
	    output_operand_lossage ("invalid %%d operand");
	  }
	return;
      }

    case 'D':
      {
	/* Print the compare operator opcode for conditional moves.  */
	switch (GET_CODE (x))
	  {
	  case EQ:
	    fputs ("ne", file);
	    break;
	  case NE:
	    fputs ("eq", file);
	    break;
	  default:
	    output_operand_lossage ("invalid %%D operand");
	  }
	return;
      }

    case 'H':
      {
      if (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == UNSPEC)
	{
	  rtx addr = XVECEXP (XEXP (x, 0), 0, 0);
	  int unspec = XINT (XEXP (x, 0), 1);
	  const char *opstr = NULL;
	  switch (unspec)
	    {
	    case UNSPEC_HW0:
	    case UNSPEC_HW0_PCREL:
	      opstr = "hw0";
	      break;
	    case UNSPEC_HW1:
	    case UNSPEC_HW1_PCREL:
	      opstr = "hw1";
	      break;
	    case UNSPEC_HW2:
	      opstr = "hw2";
	      break;
	    case UNSPEC_HW3:
	      opstr = "hw3";
	      break;
	    case UNSPEC_HW0_LAST:
	      opstr = "hw0_last";
	      break;
	    case UNSPEC_HW1_LAST:
	    case UNSPEC_HW1_LAST_PCREL:
	      opstr = "hw1_last";
	      break;
	    case UNSPEC_HW2_LAST:
	    case UNSPEC_HW2_LAST_PCREL:
	      opstr = "hw2_last";
	      break;
	    case UNSPEC_HW0_GOT:
	      opstr = "hw0_got";
	      break;
	    case UNSPEC_HW0_LAST_GOT:
	      opstr = "hw0_last_got";
	      break;
	    case UNSPEC_HW1_LAST_GOT:
	      opstr = "hw1_last_got";
	      break;
	    case UNSPEC_HW0_TLS_GD:
	      opstr = "hw0_tls_gd";
	      break;
	    case UNSPEC_HW1_LAST_TLS_GD:
	      opstr = "hw1_last_tls_gd";
	      break;
	    case UNSPEC_HW0_TLS_IE:
	      opstr = "hw0_tls_ie";
	      break;
	    case UNSPEC_HW1_LAST_TLS_IE:
	      opstr = "hw1_last_tls_ie";
	      break;
	    case UNSPEC_HW0_TLS_LE:
	      opstr = "hw0_tls_le";
	      break;
	    case UNSPEC_HW1_LAST_TLS_LE:
	      opstr = "hw1_last_tls_le";
	      break;
	    case UNSPEC_HW0_PLT_PCREL:
	      opstr = "hw0_plt";
	      break;
	    case UNSPEC_HW1_PLT_PCREL:
	      opstr = "hw1_plt";
	      break;
	    case UNSPEC_HW1_LAST_PLT_PCREL:
	      opstr = "hw1_last_plt";
	      break;
	    case UNSPEC_HW2_LAST_PLT_PCREL:
	      opstr = "hw2_last_plt";
	      break;
	    default:
	      output_operand_lossage ("invalid %%H specifier");
	    }

	  fputs (opstr, file);
	  fputc ('(', file);
	  output_addr_const (file, addr);

	  if (unspec == UNSPEC_HW0_PCREL
	      || unspec == UNSPEC_HW1_PCREL
	      || unspec == UNSPEC_HW1_LAST_PCREL
	      || unspec == UNSPEC_HW2_LAST_PCREL
	      || unspec == UNSPEC_HW0_PLT_PCREL
	      || unspec == UNSPEC_HW1_PLT_PCREL
	      || unspec == UNSPEC_HW1_LAST_PLT_PCREL
	      || unspec == UNSPEC_HW2_LAST_PLT_PCREL)
	    {
	      rtx addr2 = XVECEXP (XEXP (x, 0), 0, 1);
	      fputs (" - " , file);
	      output_addr_const (file, addr2);
	    }

	  fputc (')', file);
	  return;
	}
      else if (symbolic_operand (x, VOIDmode))
	{
	  output_addr_const (file, x);
	  return;
	}
      }
      /* FALLTHRU */

    case 'h':
      {
	/* Print the low 16 bits of a constant.  */
	HOST_WIDE_INT i;
	if (CONST_INT_P (x))
	  i = INTVAL (x);
	else if (GET_CODE (x) == CONST_DOUBLE)
	  i = CONST_DOUBLE_LOW (x);
	else
	  {
	    output_operand_lossage ("invalid %%h operand");
	    return;
	  }
	i = trunc_int_for_mode (i, HImode);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }

    case 'I':
      /* Print an auto-inc memory operand.  */
      if (!MEM_P (x))
	{
	  output_operand_lossage ("invalid %%I operand");
	  return;
	}

      output_memory_autoinc_first = true;
      output_address (GET_MODE (x), XEXP (x, 0));
      return;

    case 'i':
      /* Print an auto-inc memory operand.  */
      if (!MEM_P (x))
	{
	  output_operand_lossage ("invalid %%i operand");
	  return;
	}

      output_memory_autoinc_first = false;
      output_address (GET_MODE (x), XEXP (x, 0));
      return;

    case 'j':
      {
	/* Print the low 8 bits of a constant.  */
	HOST_WIDE_INT i;
	if (CONST_INT_P (x))
	  i = INTVAL (x);
	else if (GET_CODE (x) == CONST_DOUBLE)
	  i = CONST_DOUBLE_LOW (x);
	else if (GET_CODE (x) == CONST_VECTOR
		 && CONST_INT_P (CONST_VECTOR_ELT (x, 0)))
	  i = INTVAL (CONST_VECTOR_ELT (x, 0));
	else
	  {
	    output_operand_lossage ("invalid %%j operand");
	    return;
	  }
	i = trunc_int_for_mode (i, QImode);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }

    case 'P':
      {
	/* Print a constant plus one.  */
	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid %%P operand");
	    return;
	  }
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) + 1);
	return;
      }

    case 'm':
    case 'M':
      {
	/* Print a bfextu-style bit range.  */
	int first_bit, last_bit;
	HOST_WIDE_INT flip = (code == 'm') ? ~0 : 0;

	if (!CONST_INT_P (x)
	    || !tilegx_bitfield_operand_p (INTVAL (x) ^ flip,
					   &first_bit, &last_bit))
	  {
	    output_operand_lossage ("invalid %%%c operand", code);
	    return;
	  }

	fprintf (file, "%d, %d", first_bit, last_bit);
	return;
      }

    case 'N':
      {
	const char *reg = NULL;

	/* Print a network register.  */
	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid %%N operand");
	    return;
	  }

	switch (INTVAL (x))
	  {
	  case TILEGX_NETREG_IDN0: reg = "idn0"; break;
	  case TILEGX_NETREG_IDN1: reg = "idn1"; break;
	  case TILEGX_NETREG_UDN0: reg = "udn0"; break;
	  case TILEGX_NETREG_UDN1: reg = "udn1"; break;
	  case TILEGX_NETREG_UDN2: reg = "udn2"; break;
	  case TILEGX_NETREG_UDN3: reg = "udn3"; break;
	  default:
	    gcc_unreachable ();
	  }

	fprintf (file, reg);
	return;
      }

    case 'p':
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  if (flag_pic && !SYMBOL_REF_LOCAL_P (x))
	    fprintf (file, "plt(");
	  output_addr_const (file, x);
	  if (flag_pic && !SYMBOL_REF_LOCAL_P (x))
	    fprintf (file, ")");
	}
      else
	output_addr_const (file, x);
      return;

    case 'r':
      /* In this case we need a register.  Use 'zero' if the operand
	 is const0_rtx.  */
      if (x == const0_rtx
	  || (GET_MODE (x) != VOIDmode && x == CONST0_RTX (GET_MODE (x))))
	{
	  fputs ("zero", file);
	  return;
	}
      else if (!REG_P (x))
	{
	  output_operand_lossage ("invalid operand for 'r' specifier");
	  return;
	}
      /* FALLTHRU */

    case 0:
      if (REG_P (x))
	{
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  return;
	}
      else if (MEM_P (x))
	{
	  output_address (VOIDmode, XEXP (x, 0));
	  return;
	}
      else
	{
	  output_addr_const (file, x);
	  return;
	}
    }

  debug_rtx (x);
  output_operand_lossage ("unable to print out operand yet; code == %d (%c)",
			  code, code);
}


/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */
static void
tilegx_print_operand_address (FILE *file, machine_mode mode, rtx addr)
{
  if (GET_CODE (addr) == POST_DEC
      || GET_CODE (addr) == POST_INC)
    {
      int offset = GET_MODE_SIZE (mode);

      gcc_assert (mode != VOIDmode);

      if (output_memory_autoinc_first)
	fprintf (file, "%s", reg_names[REGNO (XEXP (addr, 0))]);
      else
	fprintf (file, "%d",
		 GET_CODE (addr) == POST_DEC ? -offset : offset);
    }
  else if (GET_CODE (addr) == POST_MODIFY)
    {
      gcc_assert (mode != VOIDmode);

      gcc_assert (GET_CODE (XEXP (addr, 1)) == PLUS);

      if (output_memory_autoinc_first)
	fprintf (file, "%s", reg_names[REGNO (XEXP (addr, 0))]);
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 INTVAL (XEXP (XEXP (addr, 1), 1)));
    }
  else
    tilegx_print_operand (file, addr, 'r');
}


/* Machine mode of current insn, for determining curly brace
   placement.  */
static machine_mode insn_mode;


/* Implement FINAL_PRESCAN_INSN.  This is used to emit bundles.  */
void
tilegx_final_prescan_insn (rtx_insn *insn)
{
  /* Record this for tilegx_asm_output_opcode to examine.  */
  insn_mode = GET_MODE (insn);
}


/* While emitting asm, are we currently inside '{' for a bundle?  */
static bool tilegx_in_bundle = false;

/* Implement ASM_OUTPUT_OPCODE.  Prepend/append curly braces as
   appropriate given the bundling information recorded by
   tilegx_gen_bundles.  */
const char *
tilegx_asm_output_opcode (FILE *stream, const char *code)
{
  bool pseudo = !strcmp (code, "pseudo");

  if (!tilegx_in_bundle && insn_mode == SImode)
    {
      /* Start a new bundle.  */
      fprintf (stream, "{\n\t");
      tilegx_in_bundle = true;
    }

  if (tilegx_in_bundle && insn_mode == QImode)
    {
      /* Close an existing bundle.  */
      static char buf[100];

      gcc_assert (strlen (code) + 3 + 1 < sizeof (buf));

      strcpy (buf, pseudo ? "" : code);
      strcat (buf, "\n\t}");
      tilegx_in_bundle = false;

      return buf;
    }
  else
    {
      return pseudo ? "" : code;
    }
}


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
void
tilegx_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  if (tilegx_in_bundle)
    {
      fprintf (file, "\t}\n");
    }

  if (cfun->static_chain_decl)
    {
      fprintf (file,
	       "\t{\n"
	       "\taddi\tsp, sp, -16\n"
	       "\tst\tsp, r10\n"
	       "\t}\n");
    }

  if (flag_pic)
    {
      fprintf (file,
	       "\t{\n"
	       "\tmove\tr10, lr\n"
	       "\tjal\tplt(%s)\n"
	       "\t}\n", MCOUNT_NAME);
    }
  else
    {
      fprintf (file,
	       "\t{\n"
	       "\tmove\tr10, lr\n"
	       "\tjal\t%s\n"
	       "\t}\n", MCOUNT_NAME);
    }

  if (cfun->static_chain_decl)
    {
      fprintf (file,
	       "\taddi\tsp, sp, 16\n"
	       "\tld\tr10, sp\n");
    }

  tilegx_in_bundle = false;
}


/* Implement TARGET_ASM_FILE_END.  */
static void
tilegx_file_end (void)
{
  if (NEED_INDICATE_EXEC_STACK)
    file_end_indicate_exec_stack ();
}

/* Implement TARGET_TRULY_NOOP_TRUNCATION.  We represent all SI values
   as sign-extended DI values in registers.  */

static bool
tilegx_truly_noop_truncation (unsigned int outprec, unsigned int inprec)
{
  return inprec <= 32 || outprec > 32;
}

#undef  TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE tilegx_option_override

#ifdef TARGET_THREAD_SSP_OFFSET
#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD hook_tree_void_null
#endif

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P tilegx_scalar_mode_supported_p

#undef  TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P tilegx_vector_mode_supported_p

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM tilegx_cannot_force_const_mem

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL tilegx_function_ok_for_sibcall

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE tilegx_pass_by_reference

#undef  TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB tilegx_return_in_msb

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY tilegx_return_in_memory

#undef  TARGET_MODE_REP_EXTENDED
#define TARGET_MODE_REP_EXTENDED tilegx_mode_rep_extended

#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY tilegx_function_arg_boundary

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG tilegx_function_arg

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE tilegx_function_arg_advance

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE tilegx_function_value

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE tilegx_libcall_value

#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P tilegx_function_value_regno_p

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_false

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST tilegx_build_builtin_va_list

#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START tilegx_va_start

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS tilegx_setup_incoming_varargs

#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR tilegx_gimplify_va_arg_expr

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS tilegx_rtx_costs

#undef  TARGET_EXPAND_TO_RTL_HOOK
#define TARGET_EXPAND_TO_RTL_HOOK tilegx_expand_to_rtl_hook

#undef  TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK tilegx_shift_truncation_mask

#undef  TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS tilegx_init_libfuncs

/* Limit to what we can reach in one addli.  */
#undef  TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -32768
#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 32767

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P tilegx_legitimate_constant_p

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P tilegx_legitimate_address_p

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS tilegx_legitimize_address

#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS tilegx_delegitimize_address

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  tilegx_init_builtins

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL tilegx_builtin_decl

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN tilegx_expand_builtin

#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE tilegx_conditional_register_usage

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED tilegx_frame_pointer_required

#undef  TARGET_DELAY_SCHED2
#define TARGET_DELAY_SCHED2 true

#undef  TARGET_DELAY_VARTRACK
#define TARGET_DELAY_VARTRACK true

#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE tilegx_issue_rate

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST tilegx_sched_adjust_cost

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG tilegx_reorg

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK tilegx_output_mi_thunk

#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE tilegx_asm_trampoline_template

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT tilegx_trampoline_init

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND tilegx_print_operand

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS tilegx_print_operand_address

#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END tilegx_file_end

#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"

#undef  TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef  TARGET_TRULY_NOOP_TRUNCATION
#define TARGET_TRULY_NOOP_TRUNCATION tilegx_truly_noop_truncation

#undef  TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-tilegx.h"
