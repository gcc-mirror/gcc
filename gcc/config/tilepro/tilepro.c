/* Subroutines used for code generation on the Tilera TILEPro.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "output.h"
#include "insn-attr.h"
#include "recog.h"
#include "expr.h"
#include "langhooks.h"
#include "optabs.h"
#include "sched-int.h"
#include "sel-sched.h"
#include "tm_p.h"
#include "tm-constrs.h"
#include "target.h"
#include "target-def.h"
#include "function.h"
#include "dwarf2.h"
#include "timevar.h"
#include "tree.h"
#include "gimple.h"
#include "cfgloop.h"
#include "tilepro-builtins.h"
#include "tilepro-multiply.h"
#include "diagnostic.h"

/* SYMBOL_REF for GOT */
static GTY(()) rtx g_got_symbol = NULL;

/* In case of a POST_INC or POST_DEC memory reference, we must report
   the mode of the memory reference from TARGET_PRINT_OPERAND to
   TARGET_PRINT_OPERAND_ADDRESS.  */
static enum machine_mode output_memory_reference_mode;

/* Report whether we're printing out the first address fragment of a
   POST_INC or POST_DEC memory reference, from TARGET_PRINT_OPERAND to
   TARGET_PRINT_OPERAND_ADDRESS.  */
static bool output_memory_autoinc_first;



/* Option handling  */

/* Implement TARGET_OPTION_OVERRIDE.  */
static void
tilepro_option_override (void)
{
  /* When modulo scheduling is enabled, we still rely on regular
     scheduler for bundling.  */
  if (flag_modulo_sched)
    flag_resched_modulo_sched = 1;
}



/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */
static bool
tilepro_scalar_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
    case DImode:
      return true;

    case SFmode:
    case DFmode:
      return true;

    default:
      return false;
    }
}


/* Implement TARGET_VECTOR_MODE_SUPPORTED_P.  */
static bool
tile_vector_mode_supported_p (enum machine_mode mode)
{
  return mode == V4QImode || mode == V2HImode;
}


/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */
static bool
tilepro_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED,
				rtx x ATTRIBUTE_UNUSED)
{
  return true;
}


/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */
static bool
tilepro_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  return decl != NULL;
}


/* Implement TARGET_PASS_BY_REFERENCE.  Variable sized types are
   passed by reference.  */
static bool
tilepro_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			   enum machine_mode mode ATTRIBUTE_UNUSED,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  return (type && TYPE_SIZE (type)
	  && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST);
}


/* Implement TARGET_RETURN_IN_MEMORY.  */
static bool
tilepro_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  return !IN_RANGE (int_size_in_bytes (type),
		    0, TILEPRO_NUM_RETURN_REGS * UNITS_PER_WORD);
}


/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  */
static unsigned int
tilepro_function_arg_boundary (enum machine_mode mode, const_tree type)
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
tilepro_function_arg (cumulative_args_t cum_v,
		      enum machine_mode mode,
		      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS cum = *get_cumulative_args (cum_v);
  int byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  bool doubleword_aligned_p;

  if (cum >= TILEPRO_NUM_ARG_REGS)
    return NULL_RTX;

  /* See whether the argument has doubleword alignment.  */
  doubleword_aligned_p =
    tilepro_function_arg_boundary (mode, type) > BITS_PER_WORD;

  if (doubleword_aligned_p)
    cum += cum & 1;

  /* The ABI does not allow parameters to be passed partially in reg
     and partially in stack.  */
  if ((cum + (byte_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
      > TILEPRO_NUM_ARG_REGS)
    return NULL_RTX;

  return gen_rtx_REG (mode, cum);
}


/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */
static void
tilepro_function_arg_advance (cumulative_args_t cum_v,
			      enum machine_mode mode,
			      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  int byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  int word_size = (byte_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  bool doubleword_aligned_p;

  /* See whether the argument has doubleword alignment.  */
  doubleword_aligned_p =
    tilepro_function_arg_boundary (mode, type) > BITS_PER_WORD;

  if (doubleword_aligned_p)
    *cum += *cum & 1;

  /* If the current argument does not fit in the pretend_args space,
     skip over it.  */
  if (*cum < TILEPRO_NUM_ARG_REGS
      && *cum + word_size > TILEPRO_NUM_ARG_REGS)
    *cum = TILEPRO_NUM_ARG_REGS;

  *cum += word_size;
}


/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
tilepro_function_value (const_tree valtype, const_tree fn_decl_or_type,
			bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode;
  int unsigned_p;

  mode = TYPE_MODE (valtype);
  unsigned_p = TYPE_UNSIGNED (valtype);

  mode = promote_function_mode (valtype, mode, &unsigned_p,
				fn_decl_or_type, 1);

  return gen_rtx_REG (mode, 0);
}


/* Implement TARGET_LIBCALL_VALUE.  */
static rtx
tilepro_libcall_value (enum machine_mode mode,
		       const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 0);
}


/* Implement FUNCTION_VALUE_REGNO_P.  */
static bool
tilepro_function_value_regno_p (const unsigned int regno)
{
  return regno < TILEPRO_NUM_RETURN_REGS;
}


/* Implement TARGET_BUILD_BUILTIN_VA_LIST.  */
static tree
tilepro_build_builtin_va_list (void)
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
tilepro_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
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
				   (crtl->args.info - TILEPRO_NUM_ARG_REGS));

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
tilepro_setup_incoming_varargs (cumulative_args_t cum,
				enum machine_mode mode,
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

  if (local_cum < TILEPRO_NUM_ARG_REGS)
    {
      *pretend_args = UNITS_PER_WORD * (TILEPRO_NUM_ARG_REGS - first_reg);

      if (!no_rtl)
	{
	  alias_set_type set = get_varargs_alias_set ();
	  rtx tmp =
	    gen_rtx_MEM (BLKmode, plus_constant (Pmode, \
						 virtual_incoming_args_rtx,
						 -STACK_POINTER_OFFSET -
						 UNITS_PER_WORD *
						 (TILEPRO_NUM_ARG_REGS -
						  first_reg)));
	  MEM_NOTRAP_P (tmp) = 1;
	  set_mem_alias_set (tmp, set);
	  move_block_from_reg (first_reg, tmp,
			       TILEPRO_NUM_ARG_REGS - first_reg);
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
  
    paddedsize = (sizeof(TYPE) + 3) & -4;
    if ((VALIST.__args + paddedsize > VALIST.__skip)
	& (VALIST.__args <= VALIST.__skip))
      addr = VALIST.__skip + STACK_POINTER_OFFSET;
    else
      addr = VALIST.__args;
    VALIST.__args = addr + paddedsize;
    ret = *(TYPE *)addr;                                          */
static tree
tilepro_gimplify_va_arg_expr (tree valist, tree type, gimple_seq * pre_p,
			      gimple_seq * post_p ATTRIBUTE_UNUSED)
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

  /* if an object is dynamically sized, a pointer to it is passed
     instead of the object itself.  */
  pass_by_reference_p = pass_by_reference (NULL, TYPE_MODE (type), type,
					   false);

  if (pass_by_reference_p)
    type = build_pointer_type (type);

  size = int_size_in_bytes (type);
  rsize = ((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD) * UNITS_PER_WORD;

  /* If the alignment of the type is greater than the default for a
     parameter, align to STACK_BOUNDARY.  */
  if (TYPE_ALIGN (type) > PARM_BOUNDARY)
    {
      /* Assert the only case we generate code for: when
         stack boundary = 2 * parm boundary.  */
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

  gimplify_assign (addr, tmp, pre_p);

  /* Update VALIST.__args.  */
  tmp = fold_build_pointer_plus_hwi (addr, rsize);
  gimplify_assign (unshare_expr (args), tmp, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (pass_by_reference_p)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}



/* Implement TARGET_RTX_COSTS.  */
static bool
tilepro_rtx_costs (rtx x, int code, int outer_code, int opno, int *total,
		   bool speed)
{
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
      /* Convey that s[123]a are efficient.  */
      if (GET_CODE (XEXP (x, 0)) == MULT
	  && cint_248_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
	{
	  *total = (rtx_cost (XEXP (XEXP (x, 0), 0),
			      (enum rtx_code) outer_code, opno, speed)
		    + rtx_cost (XEXP (x, 1),
				(enum rtx_code) outer_code, opno, speed)
		    + COSTS_N_INSNS (1));
	  return true;
	}
      return false;

    case MULT:
      *total = COSTS_N_INSNS (2);
      return false;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (outer_code == MULT)
	*total = 0;
      else
	*total = COSTS_N_INSNS (1);
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

	if (num <= TILEPRO_LAST_LATENCY_1_INSN)
	  *total = COSTS_N_INSNS (1);
	else if (num <= TILEPRO_LAST_LATENCY_2_INSN)
	  *total = COSTS_N_INSNS (2);
	else if (num > TILEPRO_LAST_LATENCY_INSN)
	  {
	    if (outer_code == PLUS)
	      *total = 0;
	    else
	      *total = COSTS_N_INSNS (1);
	  }
	else
	  {
	    switch (num)
	      {
	      case UNSPEC_BLOCKAGE:
	      case UNSPEC_NETWORK_BARRIER:
		*total = 0;
		break;

	      case UNSPEC_LNK_AND_LABEL:
	      case UNSPEC_MF:
	      case UNSPEC_NETWORK_RECEIVE:
	      case UNSPEC_NETWORK_SEND:
	      case UNSPEC_TLS_GD_ADD:
		*total = COSTS_N_INSNS (1);
		break;

	      case UNSPEC_TLS_IE_LOAD:
		*total = COSTS_N_INSNS (2);
		break;

	      case UNSPEC_SP_SET:
		*total = COSTS_N_INSNS (3);
		break;

	      case UNSPEC_SP_TEST:
		*total = COSTS_N_INSNS (4);
		break;

	      case UNSPEC_LATENCY_L2:
		*total = COSTS_N_INSNS (8);
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



/* Returns an SImode integer rtx with value VAL.  */
static rtx
gen_int_si (HOST_WIDE_INT val)
{
  return gen_int_mode (val, SImode);
}


/* Create a temporary variable to hold a partial result, to enable
   CSE.  */
static rtx
create_temp_reg_if_possible (enum machine_mode mode, rtx default_reg)
{
  return can_create_pseudo_p ()? gen_reg_rtx (mode) : default_reg;
}


/* Functions to save and restore machine-specific function data.  */
static struct machine_function *
tilepro_init_machine_status (void)
{
  return ggc_alloc_cleared_machine_function ();
}


/* Do anything needed before RTL is emitted for each function.  */
void
tilepro_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function
     status.  */
  init_machine_status = tilepro_init_machine_status;

  if (cfun && cfun->machine && flag_pic)
    {
      static int label_num = 0;

      char text_label_name[32];

      struct machine_function *machine = cfun->machine;

      ASM_GENERATE_INTERNAL_LABEL (text_label_name, "L_PICLNK", label_num++);

      machine->text_label_symbol =
	gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (text_label_name));

      machine->text_label_rtx =
	gen_rtx_REG (Pmode, TILEPRO_PIC_TEXT_LABEL_REGNUM);

      machine->got_rtx = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);

      machine->calls_tls_get_addr = false;
    }
}


/* Return true if X contains a thread-local symbol.  */
static bool
tilepro_tls_referenced_p (rtx x)
{
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
    x = XEXP (XEXP (x, 0), 0);

  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x))
    return true;

  /* That's all we handle in tilepro_legitimize_tls_address for
     now.  */
  return false;
}


/* Return true if X requires a scratch register.  It is given that
   flag_pic is on and that X satisfies CONSTANT_P.  */
static int
tilepro_pic_address_needs_scratch (rtx x)
{
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	  || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
      && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    return true;

  return false;
}


/* Implement TARGET_LEGITIMATE_CONSTANT_P.  This is all constants for
   which we are willing to load the value into a register via a move
   pattern.  TLS cannot be treated as a constant because it can
   include a function call.  */
static bool
tilepro_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
    case SYMBOL_REF:
      return !tilepro_tls_referenced_p (x);

    default:
      return true;
    }
}


/* Return true if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and that
   X satisfies CONSTANT_P.  */
bool
tilepro_legitimate_pic_operand_p (rtx x)
{
  if (tilepro_pic_address_needs_scratch (x))
    return false;

  if (tilepro_tls_referenced_p (x))
    return false;

  return true;
}


/* Return true if the rtx X can be used as an address operand.  */
static bool
tilepro_legitimate_address_p (enum machine_mode ARG_UNUSED (mode), rtx x,
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
tilepro_text_label_symbol (void)
{
  return cfun->machine->text_label_symbol;
}


/* Return the register storing the value of the text label.  */
static rtx
tilepro_text_label_rtx (void)
{
  return cfun->machine->text_label_rtx;
}


/* Return the register storing the value of the global offset
   table.  */
static rtx
tilepro_got_rtx (void)
{
  return cfun->machine->got_rtx;
}


/* Return the SYMBOL_REF for _GLOBAL_OFFSET_TABLE_.  */
static rtx
tilepro_got_symbol (void)
{
  if (g_got_symbol == NULL)
    g_got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");

  return g_got_symbol;
}


/* Return a reference to the got to be used by tls references.  */
static rtx
tilepro_tls_got (void)
{
  rtx temp;
  if (flag_pic)
    {
      crtl->uses_pic_offset_table = 1;
      return tilepro_got_rtx ();
    }

  temp = gen_reg_rtx (Pmode);
  emit_move_insn (temp, tilepro_got_symbol ());

  return temp;
}


/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */
static rtx
tilepro_legitimize_tls_address (rtx addr)
{
  rtx ret;

  gcc_assert (can_create_pseudo_p ());

  if (GET_CODE (addr) == SYMBOL_REF)
    switch (SYMBOL_REF_TLS_MODEL (addr))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
      case TLS_MODEL_LOCAL_DYNAMIC:
	{
	  rtx r0, temp1, temp2, temp3, got, last;

	  ret = gen_reg_rtx (Pmode);
	  r0 = gen_rtx_REG (Pmode, 0);
	  temp1 = gen_reg_rtx (Pmode);
	  temp2 = gen_reg_rtx (Pmode);
	  temp3 = gen_reg_rtx (Pmode);

	  got = tilepro_tls_got ();
	  emit_insn (gen_tls_gd_addhi (temp1, got, addr));
	  emit_insn (gen_tls_gd_addlo (temp2, temp1, addr));
	  emit_move_insn (r0, temp2);
	  emit_insn (gen_tls_gd_call (addr));
	  emit_move_insn (temp3, r0);
	  last = emit_insn (gen_tls_gd_add (ret, temp3, addr));
	  set_unique_reg_note (last, REG_EQUAL, copy_rtx (addr));
	  break;
	}
      case TLS_MODEL_INITIAL_EXEC:
	{
	  rtx temp1, temp2, temp3, got, last;

	  ret = gen_reg_rtx (Pmode);
	  temp1 = gen_reg_rtx (Pmode);
	  temp2 = gen_reg_rtx (Pmode);
	  temp3 = gen_reg_rtx (Pmode);

	  got = tilepro_tls_got ();
	  emit_insn (gen_tls_ie_addhi (temp1, got, addr));
	  emit_insn (gen_tls_ie_addlo (temp2, temp1, addr));
	  emit_insn (gen_tls_ie_load (temp3, temp2, addr));
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
	  rtx temp1, last;

	  ret = gen_reg_rtx (Pmode);
	  temp1 = gen_reg_rtx (Pmode);

	  emit_insn (gen_tls_le_addhi (temp1,
				       gen_rtx_REG (Pmode,
						    THREAD_POINTER_REGNUM),
				       addr));
	  last = emit_insn (gen_tls_le_addlo (ret, temp1, addr));
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

      base = tilepro_legitimize_tls_address (XEXP (XEXP (addr, 0), 0));
      offset = XEXP (XEXP (addr, 0), 1);

      base = force_operand (base, NULL_RTX);
      ret = force_reg (Pmode, gen_rtx_PLUS (Pmode, base, offset));
    }
  else
    gcc_unreachable ();

  return ret;
}


/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go into a reg.  This is REG if
   nonzero, otherwise we allocate register(s) as necessary.  */
static rtx
tilepro_legitimize_pic_address (rtx orig,
				enum machine_mode mode ATTRIBUTE_UNUSED,
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
	  rtx text_label_symbol = tilepro_text_label_symbol ();
	  rtx text_label_rtx = tilepro_text_label_rtx ();

	  emit_insn (gen_addli_pcrel (temp_reg, text_label_rtx, orig,
				      text_label_symbol));
	  emit_insn (gen_auli_pcrel (temp_reg, temp_reg, orig,
				     text_label_symbol));

	  /* Note: this is conservative.  We use the text_label but we
	     don't use the pic_offset_table.  However, in some cases
	     we may need the pic_offset_table (see
	     tilepro_fixup_pcrel_references).  */
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
	      emit_insn (gen_add_got16 (temp_reg,
					tilepro_got_rtx (), orig));
	    }
	  else
	    {
	      rtx temp_reg2 = create_temp_reg_if_possible (Pmode, reg);
	      emit_insn (gen_addhi_got32 (temp_reg2,
					  tilepro_got_rtx (), orig));
	      emit_insn (gen_addlo_got32 (temp_reg, temp_reg2, orig));
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
	  && XEXP (XEXP (orig, 0), 0) == tilepro_got_rtx ())
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);
      base = tilepro_legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode,
					     reg);
      offset =
	tilepro_legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					base == reg ? 0 : reg);

      if (CONST_INT_P (offset))
	{
	  if (can_create_pseudo_p ())
	    offset = force_reg (Pmode, offset);
	  else
	    /* If we reach here, then something is seriously
	       wrong.  */
	    gcc_unreachable ();
	}

      if (can_create_pseudo_p ())
	return force_reg (Pmode, gen_rtx_PLUS (Pmode, base, offset));
      else
	gcc_unreachable ();
    }
  else if (GET_CODE (orig) == LABEL_REF)
    {
      rtx address, temp_reg;
      rtx text_label_symbol;
      rtx text_label_rtx;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      /* If not during reload, allocate another temp reg here for
         loading in the address, so that these instructions can be
         optimized properly.  */
      temp_reg = create_temp_reg_if_possible (Pmode, reg);
      text_label_symbol = tilepro_text_label_symbol ();
      text_label_rtx = tilepro_text_label_rtx ();

      emit_insn (gen_addli_pcrel (temp_reg, text_label_rtx, orig,
				  text_label_symbol));
      emit_insn (gen_auli_pcrel (temp_reg, temp_reg, orig,
				 text_label_symbol));

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
tilepro_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			    enum machine_mode mode)
{
  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
      && symbolic_operand (x, Pmode) && tilepro_tls_referenced_p (x))
    {
      return tilepro_legitimize_tls_address (x);
    }
  else if (flag_pic)
    {
      return tilepro_legitimize_pic_address (x, mode, 0);
    }
  else
    return x;
}


/* Implement TARGET_DELEGITIMIZE_ADDRESS.  */
static rtx
tilepro_delegitimize_address (rtx x)
{
  x = delegitimize_mem_from_attrs (x);

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == UNSPEC)
    {
      switch (XINT (XEXP (x, 0), 1))
	{
	case UNSPEC_PCREL_SYM:
	case UNSPEC_GOT16_SYM:
	case UNSPEC_GOT32_SYM:
	case UNSPEC_TLS_GD:
	case UNSPEC_TLS_IE:
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

  rtx got_symbol = tilepro_got_symbol ();
  rtx text_label_symbol = tilepro_text_label_symbol ();
  rtx text_label_rtx = tilepro_text_label_rtx ();
  flag_pic = 0;

  emit_insn (gen_insn_lnk_and_label (text_label_rtx, text_label_symbol));

  emit_insn (gen_addli_pcrel (tilepro_got_rtx (),
			      text_label_rtx, got_symbol, text_label_symbol));

  emit_insn (gen_auli_pcrel (tilepro_got_rtx (),
			     tilepro_got_rtx (),
			     got_symbol, text_label_symbol));

  flag_pic = orig_flag_pic;

  /* Need to emit this whether or not we obey regdecls, since
     setjmp/longjmp can cause life info to screw up.  ??? In the case
     where we don't obey regdecls, this is not sufficient since we may
     not fall out the bottom.  */
  emit_use (tilepro_got_rtx ());
}


/* Return the simd variant of the constant NUM of mode MODE, by
   replicating it to fill an interger of mode SImode.  NUM is first
   truncated to fit in MODE.  */
rtx
tilepro_simd_int (rtx num, enum machine_mode mode)
{
  HOST_WIDE_INT n = 0;

  gcc_assert (CONST_INT_P (num));

  n = INTVAL (num);

  switch (mode)
    {
    case QImode:
      n = 0x01010101 * (n & 0x000000FF);
      break;
    case HImode:
      n = 0x00010001 * (n & 0x0000FFFF);
      break;
    case SImode:
      break;
    case DImode:
      break;
    default:
      gcc_unreachable ();
    }

  return gen_int_si (n);
}


/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant,
   or CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL
   to split and "num" is its length.  lo_half and hi_half are output
   arrays that parallel "operands".  */
void
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses,
         but we still have to handle it.  */
      if (MEM_P (op))
	{
	  lo_half[num] = adjust_address (op, SImode, 0);
	  hi_half[num] = adjust_address (op, SImode, 4);
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 0);
	  hi_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 4);
	}
    }
}


/* Returns true iff val can be moved into a register in one
   instruction.  And if it can, it emits the code to move the
   constant.

   If three_wide_only is true, this insists on an instruction that
   works in a bundle containing three instructions.  */
static bool
expand_set_cint32_one_inst (rtx dest_reg,
			    HOST_WIDE_INT val, bool three_wide_only)
{
  val = trunc_int_for_mode (val, SImode);

  if (val == trunc_int_for_mode (val, QImode))
    {
      /* Success! */
      emit_move_insn (dest_reg, GEN_INT (val));
      return true;
    }
  else if (!three_wide_only)
    {
      rtx imm_op = GEN_INT (val);

      if (satisfies_constraint_J (imm_op)
	  || satisfies_constraint_K (imm_op)
	  || satisfies_constraint_N (imm_op)
	  || satisfies_constraint_P (imm_op))
	{
	  emit_move_insn (dest_reg, imm_op);
	  return true;
	}
    }

  return false;
}


/* Implement SImode rotatert.  */
static HOST_WIDE_INT
rotate_right (HOST_WIDE_INT n, int count)
{
  unsigned HOST_WIDE_INT x = n & 0xFFFFFFFF;
  if (count == 0)
    return x;
  return ((x >> count) | (x << (32 - count))) & 0xFFFFFFFF;
}


/* Return true iff n contains exactly one contiguous sequence of 1
   bits, possibly wrapping around from high bits to low bits.  */
bool
tilepro_bitfield_operand_p (HOST_WIDE_INT n, int *first_bit, int *last_bit)
{
  int i;

  if (n == 0)
    return false;

  for (i = 0; i < 32; i++)
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
	    *last_bit = (i + exact_log2 (x ^ (x >> 1))) & 31;

	  return true;
	}
    }

  return false;
}


/* Create code to move the CONST_INT value in src_val to dest_reg.  */
static void
expand_set_cint32 (rtx dest_reg, rtx src_val)
{
  HOST_WIDE_INT val;
  int leading_zeroes, trailing_zeroes;
  int lower, upper;
  int three_wide_only;
  rtx temp;

  gcc_assert (CONST_INT_P (src_val));
  val = trunc_int_for_mode (INTVAL (src_val), SImode);

  /* See if we can generate the constant in one instruction.  */
  if (expand_set_cint32_one_inst (dest_reg, val, false))
    return;

  /* Create a temporary variable to hold a partial result, to enable
     CSE.  */
  temp = create_temp_reg_if_possible (SImode, dest_reg);

  leading_zeroes = 31 - floor_log2 (val & 0xFFFFFFFF);
  trailing_zeroes = exact_log2 (val & -val);

  lower = trunc_int_for_mode (val, HImode);
  upper = trunc_int_for_mode ((val - lower) >> 16, HImode);

  /* First try all three-wide instructions that generate a constant
     (i.e. movei) followed by various shifts and rotates. If none of
     those work, try various two-wide ways of generating a constant
     followed by various shifts and rotates.  */
  for (three_wide_only = 1; three_wide_only >= 0; three_wide_only--)
    {
      int count;

      if (expand_set_cint32_one_inst (temp, val >> trailing_zeroes,
				      three_wide_only))
	{
	  /* 0xFFFFA500 becomes:
	     movei temp, 0xFFFFFFA5
	     shli dest, temp, 8  */
	  emit_move_insn (dest_reg,
			  gen_rtx_ASHIFT (SImode, temp,
					  GEN_INT (trailing_zeroes)));
	  return;
	}

      if (expand_set_cint32_one_inst (temp, val << leading_zeroes,
				      three_wide_only))
	{
	  /* 0x7FFFFFFF becomes:
	     movei temp, -2
	     shri dest, temp, 1  */
	  emit_move_insn (dest_reg,
			  gen_rtx_LSHIFTRT (SImode, temp,
					    GEN_INT (leading_zeroes)));
	  return;
	}

      /* Try rotating a one-instruction immediate, since rotate is
         3-wide.  */
      for (count = 1; count < 32; count++)
	{
	  HOST_WIDE_INT r = rotate_right (val, count);
	  if (expand_set_cint32_one_inst (temp, r, three_wide_only))
	    {
	      /* 0xFFA5FFFF becomes:
	         movei temp, 0xFFFFFFA5
	         rli dest, temp, 16  */
	      emit_move_insn (dest_reg,
			      gen_rtx_ROTATE (SImode, temp, GEN_INT (count)));
	      return;
	    }
	}

      if (lower == trunc_int_for_mode (lower, QImode))
	{
	  /* We failed to use two 3-wide instructions, but the low 16
	     bits are a small number so just use a 2-wide + 3-wide
	     auli + addi pair rather than anything more exotic.

	     0x12340056 becomes:
	     auli temp, zero, 0x1234
	     addi dest, temp, 0x56  */
	  break;
	}
    }

  /* Fallback case: use a auli + addli/addi pair.  */
  emit_move_insn (temp, GEN_INT (upper << 16));
  emit_move_insn (dest_reg, (gen_rtx_PLUS (SImode, temp, GEN_INT (lower))));
}


/* Load OP1, a 32-bit constant, into OP0, a register.  We know it
   can't be done in one insn when we get here, the move expander
   guarantees this.  */
void
tilepro_expand_set_const32 (rtx op0, rtx op1)
{
  enum machine_mode mode = GET_MODE (op0);
  rtx temp;

  if (CONST_INT_P (op1))
    {
      /* TODO: I don't know if we want to split large constants now,
         or wait until later (with a define_split).

         Does splitting early help CSE?  Does it harm other
         optimizations that might fold loads? */
      expand_set_cint32 (op0, op1);
    }
  else
    {
      temp = create_temp_reg_if_possible (mode, op0);

      /* A symbol, emit in the traditional way.  */
      emit_move_insn (temp, gen_rtx_HIGH (mode, op1));
      emit_move_insn (op0, gen_rtx_LO_SUM (mode, temp, op1));
    }
}


/* Expand a move instruction.  Return true if all work is done.  */
bool
tilepro_expand_mov (enum machine_mode mode, rtx *operands)
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
  if (CONSTANT_P (operands[1]) && tilepro_tls_referenced_p (operands[1]))
    {
      operands[1] = tilepro_legitimize_tls_address (operands[1]);
      return false;
    }

  /* Fixup PIC cases.  */
  if (flag_pic && CONSTANT_P (operands[1]))
    {
      if (tilepro_pic_address_needs_scratch (operands[1]))
	operands[1] = tilepro_legitimize_pic_address (operands[1], mode, 0);

      if (symbolic_operand (operands[1], mode))
	{
	  operands[1] = tilepro_legitimize_pic_address (operands[1],
							mode,
							(reload_in_progress ?
							 operands[0] :
							 NULL_RTX));
	  return false;
	}
    }

  /* Fixup for UNSPEC addresses.  */
  if (flag_pic
      && GET_CODE (operands[1]) == HIGH
      && GET_CODE (XEXP (operands[1], 0)) == CONST
      && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == UNSPEC)
    {
      rtx unspec = XEXP (XEXP (operands[1], 0), 0);
      int unspec_num = XINT (unspec, 1);
      if (unspec_num == UNSPEC_PCREL_SYM)
	{
	  emit_insn (gen_auli_pcrel (operands[0], const0_rtx,
				     XVECEXP (unspec, 0, 0),
				     XVECEXP (unspec, 0, 1)));
	  return true;
	}
      else if (flag_pic == 2 && unspec_num == UNSPEC_GOT32_SYM)
	{
	  emit_insn (gen_addhi_got32 (operands[0], const0_rtx,
				      XVECEXP (unspec, 0, 0)));
	  return true;
	}
      else if (HAVE_AS_TLS && unspec_num == UNSPEC_TLS_GD)
	{
	  emit_insn (gen_tls_gd_addhi (operands[0], const0_rtx,
				       XVECEXP (unspec, 0, 0)));
	  return true;
	}
      else if (HAVE_AS_TLS && unspec_num == UNSPEC_TLS_IE)
	{
	  emit_insn (gen_tls_ie_addhi (operands[0], const0_rtx,
				       XVECEXP (unspec, 0, 0)));
	  return true;
	}
      else if (HAVE_AS_TLS && unspec_num == UNSPEC_TLS_LE)
	{
	  emit_insn (gen_tls_le_addhi (operands[0], const0_rtx,
				       XVECEXP (unspec, 0, 0)));
	  return true;
	}
    }

  /* Accept non-constants and valid constants unmodified.  */
  if (!CONSTANT_P (operands[1])
      || GET_CODE (operands[1]) == HIGH || move_operand (operands[1], mode))
    return false;

  /* Split large integers.  */
  if (GET_MODE_SIZE (mode) <= 4)
    {
      tilepro_expand_set_const32 (operands[0], operands[1]);
      return true;
    }

  return false;
}


/* Expand the "insv" pattern.  */
void
tilepro_expand_insv (rtx operands[4])
{
  rtx first_rtx = operands[2];
  HOST_WIDE_INT first = INTVAL (first_rtx);
  HOST_WIDE_INT width = INTVAL (operands[1]);
  rtx v = operands[3];

  /* Shift the inserted bits into position.  */
  if (first != 0)
    {
      if (CONST_INT_P (v))
	{
	  /* Shift the constant into mm position.  */
	  v = gen_int_si (INTVAL (v) << first);
	}
      else
	{
	  /* Shift over the value to be inserted.  */
	  rtx tmp = gen_reg_rtx (SImode);
	  emit_insn (gen_ashlsi3 (tmp, v, first_rtx));
	  v = tmp;
	}
    }

  /* Insert the shifted bits using an 'mm' insn.  */
  emit_insn (gen_insn_mm (operands[0], v, operands[0], first_rtx,
			  GEN_INT (first + width - 1)));
}


/* Expand unaligned loads.  */
void
tilepro_expand_unaligned_load (rtx dest_reg, rtx mem, HOST_WIDE_INT bitsize,
			       HOST_WIDE_INT bit_offset, bool sign)
{
  enum machine_mode mode;
  rtx addr_lo, addr_hi;
  rtx mem_lo, mem_hi, hi;
  rtx mema, wide_result;
  int last_byte_offset;
  HOST_WIDE_INT byte_offset = bit_offset / BITS_PER_UNIT;

  mode = GET_MODE (dest_reg);

  hi = gen_reg_rtx (mode);

  if (bitsize == 2 * BITS_PER_UNIT && (bit_offset % BITS_PER_UNIT) == 0)
    {
      rtx lo;

      /* When just loading a two byte value, we can load the two bytes
         individually and combine them efficiently.  */

      mem_lo = adjust_address (mem, QImode, byte_offset);
      mem_hi = adjust_address (mem, QImode, byte_offset + 1);

      lo = gen_reg_rtx (mode);
      emit_insn (gen_zero_extendqisi2 (lo, mem_lo));

      if (sign)
	{
	  rtx tmp = gen_reg_rtx (mode);

	  /* Do a signed load of the second byte then shift and OR it
	     in.  */
	  emit_insn (gen_extendqisi2 (gen_lowpart (SImode, hi), mem_hi));
	  emit_insn (gen_ashlsi3 (gen_lowpart (SImode, tmp),
				  gen_lowpart (SImode, hi), GEN_INT (8)));
	  emit_insn (gen_iorsi3 (gen_lowpart (SImode, dest_reg),
				 gen_lowpart (SImode, lo),
				 gen_lowpart (SImode, tmp)));
	}
      else
	{
	  /* Do two unsigned loads and use intlb to interleave
	     them.  */
	  emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, hi), mem_hi));
	  emit_insn (gen_insn_intlb (gen_lowpart (SImode, dest_reg),
				     gen_lowpart (SImode, hi),
				     gen_lowpart (SImode, lo)));
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
			   gen_rtx_AND (Pmode, addr_lo, GEN_INT (-4)));
  set_mem_alias_set (mem_lo, 0);

  /* Load the high word at an address that will not fault if the low
     address is aligned and at the very end of a page.  */
  last_byte_offset = (bit_offset + bitsize - 1) / BITS_PER_UNIT;
  addr_hi = force_reg (Pmode, plus_constant (Pmode, mema, last_byte_offset));
  mem_hi = change_address (mem, mode,
			   gen_rtx_AND (Pmode, addr_hi, GEN_INT (-4)));
  set_mem_alias_set (mem_hi, 0);

  if (bitsize == 32)
    {
      addr_lo = make_safe_from (addr_lo, dest_reg);
      wide_result = dest_reg;
    }
  else
    {
      wide_result = gen_reg_rtx (mode);
    }

  /* Load hi first in case dest_reg is used in mema.  */
  emit_move_insn (hi, mem_hi);
  emit_move_insn (wide_result, mem_lo);

  emit_insn (gen_insn_dword_align (gen_lowpart (SImode, wide_result),
				   gen_lowpart (SImode, wide_result),
				   gen_lowpart (SImode, hi), addr_lo));

  if (bitsize != 32)
    {
      rtx extracted =
	extract_bit_field (gen_lowpart (SImode, wide_result),
			   bitsize, bit_offset % BITS_PER_UNIT,
			   !sign, gen_lowpart (SImode, dest_reg),
			   SImode, SImode);

      if (extracted != dest_reg)
	emit_move_insn (dest_reg, gen_lowpart (SImode, extracted));
    }
}


/* Expand unaligned stores.  */
static void
tilepro_expand_unaligned_store (rtx mem, rtx src, HOST_WIDE_INT bitsize,
				HOST_WIDE_INT bit_offset)
{
  HOST_WIDE_INT byte_offset = bit_offset / BITS_PER_UNIT;
  HOST_WIDE_INT bytesize = bitsize / BITS_PER_UNIT;
  HOST_WIDE_INT shift_amt;
  HOST_WIDE_INT i;
  rtx mem_addr;
  rtx store_val;

  for (i = 0, shift_amt = 0; i < bytesize; i++, shift_amt += BITS_PER_UNIT)
    {
      mem_addr = adjust_address (mem, QImode, byte_offset + i);

      if (shift_amt)
	{
	  store_val = expand_simple_binop (SImode, LSHIFTRT,
					   gen_lowpart (SImode, src),
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
tilepro_expand_movmisalign (enum machine_mode mode, rtx *operands)
{
  if (MEM_P (operands[1]))
    {
      rtx tmp;

      if (register_operand (operands[0], mode))
	tmp = operands[0];
      else
	tmp = gen_reg_rtx (mode);

      tilepro_expand_unaligned_load (tmp, operands[1],
				     GET_MODE_BITSIZE (mode), 0, true);

      if (tmp != operands[0])
	emit_move_insn (operands[0], tmp);
    }
  else if (MEM_P (operands[0]))
    {
      if (!reg_or_0_operand (operands[1], mode))
	operands[1] = force_reg (mode, operands[1]);

      tilepro_expand_unaligned_store (operands[0], operands[1],
				      GET_MODE_BITSIZE (mode), 0);
    }
  else
    gcc_unreachable ();
}


/* Implement the addsi3 pattern.  */
bool
tilepro_expand_addsi (rtx op0, rtx op1, rtx op2)
{
  rtx temp;
  HOST_WIDE_INT n;
  HOST_WIDE_INT high;

  /* Skip anything that only takes one instruction.  */
  if (add_operand (op2, SImode))
    return false;

  /* We can only optimize ints here (it should be impossible to get
     here with any other type, but it is harmless to check.  */
  if (!CONST_INT_P (op2))
    return false;

  temp = create_temp_reg_if_possible (SImode, op0);
  n = INTVAL (op2);
  high = (n + (n & 0x8000)) & ~0xffff;

  emit_move_insn (temp, gen_rtx_PLUS (SImode, op1, gen_int_si (high)));
  emit_move_insn (op0, gen_rtx_PLUS (SImode, temp, gen_int_si (n - high)));

  return true;
}


/* Implement the allocate_stack pattern (alloca).  */
void
tilepro_allocate_stack (rtx op0, rtx op1)
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
tilepro_multiply_get_opcode (const struct tilepro_multiply_insn_seq_entry
			     *entry)
{
  return tilepro_multiply_insn_seq_decode_opcode[entry->compressed_opcode];
}


/* Returns the length of the 'op' array.  */
static int
tilepro_multiply_get_num_ops (const struct tilepro_multiply_insn_seq *seq)
{
  /* The array either uses all of its allocated slots or is terminated
     by a bogus opcode. Either way, the array size is the index of the
     last valid opcode plus one.  */
  int i;
  for (i = tilepro_multiply_insn_seq_MAX_OPERATIONS - 1; i >= 0; i--)
    if (tilepro_multiply_get_opcode (&seq->op[i]) != CODE_FOR_nothing)
      return i + 1;

  /* An empty array is not allowed.  */
  gcc_unreachable ();
}


/* We precompute a number of expression trees for multiplying by
   constants.  This generates code for such an expression tree by
   walking through the nodes in the tree (which are conveniently
   pre-linearized) and emitting an instruction for each one.  */
static void
tilepro_expand_constant_multiply_given_sequence (rtx result, rtx src,
						 const struct
						 tilepro_multiply_insn_seq
						 *seq)
{
  int i;
  int num_ops;

  /* Keep track of the subexpressions computed so far, so later
     instructions can refer to them.  We seed the array with zero and
     the value being multiplied.  */
  int num_subexprs = 2;
  rtx subexprs[tilepro_multiply_insn_seq_MAX_OPERATIONS + 2];
  subexprs[0] = const0_rtx;
  subexprs[1] = src;

  /* Determine how many instructions we are going to generate.  */
  num_ops = tilepro_multiply_get_num_ops (seq);
  gcc_assert (num_ops > 0
	      && num_ops <= tilepro_multiply_insn_seq_MAX_OPERATIONS);

  for (i = 0; i < num_ops; i++)
    {
      const struct tilepro_multiply_insn_seq_entry *entry = &seq->op[i];

      /* Figure out where to store the output of this instruction.  */
      const bool is_last_op = (i + 1 == num_ops);
      rtx out = is_last_op ? result : gen_reg_rtx (SImode);

      enum insn_code opcode = tilepro_multiply_get_opcode (entry);
      if (opcode == CODE_FOR_ashlsi3)
	{
	  /* Handle shift by immediate. This is a special case because
	     the meaning of the second operand is a constant shift
	     count rather than an operand index.  */

	  /* Make sure the shift count is in range. Zero should not
	     happen.  */
	  const int shift_count = entry->rhs;
	  gcc_assert (shift_count > 0 && shift_count < 32);

	  /* Emit the actual instruction.  */
	  emit_insn (GEN_FCN (opcode)
		     (out, subexprs[entry->lhs],
		      gen_rtx_CONST_INT (SImode, shift_count)));
	}
      else
	{
	  /* Handle a normal two-operand instruction, such as add or
	     s1a.  */

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
tilepro_compare_multipliers (const void *key, const void *t)
{
  return *(const int *) key -
    ((const struct tilepro_multiply_insn_seq *) t)->multiplier;
}


/* Returns the tilepro_multiply_insn_seq for multiplier, or NULL if
   none exists.  */
static const struct tilepro_multiply_insn_seq *
tilepro_find_multiply_insn_seq_for_constant (int multiplier)
{
  return ((const struct tilepro_multiply_insn_seq *)
	  bsearch (&multiplier, tilepro_multiply_insn_seq_table,
		   tilepro_multiply_insn_seq_table_size,
		   sizeof tilepro_multiply_insn_seq_table[0],
		   tilepro_compare_multipliers));
}


/* Try to a expand constant multiply in SImode by looking it up in a
   precompiled table.  OP0 is the result operand, OP1 is the source
   operand, and MULTIPLIER is the value of the constant.  Return true
   if it succeeds.  */
static bool
tilepro_expand_const_mulsi (rtx op0, rtx op1, int multiplier)
{
  /* See if we have precomputed an efficient way to multiply by this
     constant.  */
  const struct tilepro_multiply_insn_seq *seq =
    tilepro_find_multiply_insn_seq_for_constant (multiplier);
  if (seq != NULL)
    {
      tilepro_expand_constant_multiply_given_sequence (op0, op1, seq);
      return true;
    }
  else
    return false;
}


/* Expand the mulsi pattern.  */
bool
tilepro_expand_mulsi (rtx op0, rtx op1, rtx op2)
{
  if (CONST_INT_P (op2))
    {
      HOST_WIDE_INT n = trunc_int_for_mode (INTVAL (op2), SImode);
      return tilepro_expand_const_mulsi (op0, op1, n);
    }
  return false;
}


/* Expand a high multiply pattern in SImode.  RESULT, OP1, OP2 are the
   operands, and SIGN is true if it's a signed multiply, and false if
   it's an unsigned multiply.  */
static void
tilepro_expand_high_multiply (rtx result, rtx op1, rtx op2, bool sign)
{
  rtx tmp0 = gen_reg_rtx (SImode);
  rtx tmp1 = gen_reg_rtx (SImode);
  rtx tmp2 = gen_reg_rtx (SImode);
  rtx tmp3 = gen_reg_rtx (SImode);
  rtx tmp4 = gen_reg_rtx (SImode);
  rtx tmp5 = gen_reg_rtx (SImode);
  rtx tmp6 = gen_reg_rtx (SImode);
  rtx tmp7 = gen_reg_rtx (SImode);
  rtx tmp8 = gen_reg_rtx (SImode);
  rtx tmp9 = gen_reg_rtx (SImode);
  rtx tmp10 = gen_reg_rtx (SImode);
  rtx tmp11 = gen_reg_rtx (SImode);
  rtx tmp12 = gen_reg_rtx (SImode);
  rtx tmp13 = gen_reg_rtx (SImode);
  rtx result_lo = gen_reg_rtx (SImode);

  if (sign)
    {
      emit_insn (gen_insn_mulhl_su (tmp0, op1, op2));
      emit_insn (gen_insn_mulhl_su (tmp1, op2, op1));
      emit_insn (gen_insn_mulll_uu (tmp2, op1, op2));
      emit_insn (gen_insn_mulhh_ss (tmp3, op1, op2));
    }
  else
    {
      emit_insn (gen_insn_mulhl_uu (tmp0, op1, op2));
      emit_insn (gen_insn_mulhl_uu (tmp1, op2, op1));
      emit_insn (gen_insn_mulll_uu (tmp2, op1, op2));
      emit_insn (gen_insn_mulhh_uu (tmp3, op1, op2));
    }

  emit_move_insn (tmp4, (gen_rtx_ASHIFT (SImode, tmp0, GEN_INT (16))));

  emit_move_insn (tmp5, (gen_rtx_ASHIFT (SImode, tmp1, GEN_INT (16))));

  emit_move_insn (tmp6, (gen_rtx_PLUS (SImode, tmp4, tmp5)));
  emit_move_insn (result_lo, (gen_rtx_PLUS (SImode, tmp2, tmp6)));

  emit_move_insn (tmp7, gen_rtx_LTU (SImode, tmp6, tmp4));
  emit_move_insn (tmp8, gen_rtx_LTU (SImode, result_lo, tmp2));

  if (sign)
    {
      emit_move_insn (tmp9, (gen_rtx_ASHIFTRT (SImode, tmp0, GEN_INT (16))));
      emit_move_insn (tmp10, (gen_rtx_ASHIFTRT (SImode, tmp1, GEN_INT (16))));
    }
  else
    {
      emit_move_insn (tmp9, (gen_rtx_LSHIFTRT (SImode, tmp0, GEN_INT (16))));
      emit_move_insn (tmp10, (gen_rtx_LSHIFTRT (SImode, tmp1, GEN_INT (16))));
    }

  emit_move_insn (tmp11, (gen_rtx_PLUS (SImode, tmp3, tmp7)));
  emit_move_insn (tmp12, (gen_rtx_PLUS (SImode, tmp8, tmp9)));
  emit_move_insn (tmp13, (gen_rtx_PLUS (SImode, tmp11, tmp12)));
  emit_move_insn (result, (gen_rtx_PLUS (SImode, tmp13, tmp10)));
}


/* Implement smulsi3_highpart.  */
void
tilepro_expand_smulsi3_highpart (rtx op0, rtx op1, rtx op2)
{
  tilepro_expand_high_multiply (op0, op1, op2, true);
}


/* Implement umulsi3_highpart.  */
void
tilepro_expand_umulsi3_highpart (rtx op0, rtx op1, rtx op2)
{
  tilepro_expand_high_multiply (op0, op1, op2, false);
}



/* Compare and branches  */

/* Helper function to handle DImode for tilepro_emit_setcc_internal.  */
static bool
tilepro_emit_setcc_internal_di (rtx res, enum rtx_code code, rtx op0, rtx op1)
{
  rtx operands[2], lo_half[2], hi_half[2];
  rtx tmp, tmp0, tmp1, tmp2;
  bool swap = false;

  /* Reduce the number of cases we need to handle by reversing the
     operands.  */
  switch (code)
    {
    case EQ:
    case NE:
    case LE:
    case LT:
    case LEU:
    case LTU:
      /* We handle these compares directly.  */
      break;

    case GE:
    case GT:
    case GEU:
    case GTU:
      /* Reverse the operands.  */
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

  operands[0] = op0;
  operands[1] = op1;

  split_di (operands, 2, lo_half, hi_half);

  if (!reg_or_0_operand (lo_half[0], SImode))
    lo_half[0] = force_reg (SImode, lo_half[0]);

  if (!reg_or_0_operand (hi_half[0], SImode))
    hi_half[0] = force_reg (SImode, hi_half[0]);

  if (!CONST_INT_P (lo_half[1]) && !register_operand (lo_half[1], SImode))
    lo_half[1] = force_reg (SImode, lo_half[1]);

  if (!CONST_INT_P (hi_half[1]) && !register_operand (hi_half[1], SImode))
    hi_half[1] = force_reg (SImode, hi_half[1]);

  tmp0 = gen_reg_rtx (SImode);
  tmp1 = gen_reg_rtx (SImode);
  tmp2 = gen_reg_rtx (SImode);

  switch (code)
    {
    case EQ:
      emit_insn (gen_insn_seq (tmp0, lo_half[0], lo_half[1]));
      emit_insn (gen_insn_seq (tmp1, hi_half[0], hi_half[1]));
      emit_insn (gen_andsi3 (res, tmp0, tmp1));
      return true;
      break;
    case NE:
      emit_insn (gen_insn_sne (tmp0, lo_half[0], lo_half[1]));
      emit_insn (gen_insn_sne (tmp1, hi_half[0], hi_half[1]));
      emit_insn (gen_iorsi3 (res, tmp0, tmp1));
      return true;
      break;
    case LE:
      emit_insn (gen_insn_slte (tmp0, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_seq (tmp1, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_slte_u (tmp2, lo_half[0], lo_half[1]));
      emit_insn (gen_insn_mvnz (res, tmp0, tmp1, tmp2));
      return true;
    case LT:
      if (operands[1] == const0_rtx)
	{
	  emit_insn (gen_lshrsi3 (res, hi_half[0], GEN_INT (31)));
	  return true;
	}
      else
	{
	  emit_insn (gen_insn_slt (tmp0, hi_half[0], hi_half[1]));
	  emit_insn (gen_insn_seq (tmp1, hi_half[0], hi_half[1]));
	  emit_insn (gen_insn_slt_u (tmp2, lo_half[0], lo_half[1]));
	  emit_insn (gen_insn_mvnz (res, tmp0, tmp1, tmp2));
	}
      return true;
    case LEU:
      emit_insn (gen_insn_slte_u (tmp0, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_seq (tmp1, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_slte_u (tmp2, lo_half[0], lo_half[1]));
      emit_insn (gen_insn_mvnz (res, tmp0, tmp1, tmp2));
      return true;
    case LTU:
      emit_insn (gen_insn_slt_u (tmp0, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_seq (tmp1, hi_half[0], hi_half[1]));
      emit_insn (gen_insn_slt_u (tmp2, lo_half[0], lo_half[1]));
      emit_insn (gen_insn_mvnz (res, tmp0, tmp1, tmp2));
      return true;
    default:
      gcc_unreachable ();
    }

  return false;
}


/* Certain simplifications can be done to make invalid setcc
   operations valid.  Return the final comparison, or NULL if we can't
   work.  */
static bool
tilepro_emit_setcc_internal (rtx res, enum rtx_code code, rtx op0, rtx op1,
			     enum machine_mode cmp_mode)
{
  rtx tmp;
  bool swap = false;

  if (cmp_mode == DImode)
    {
      return tilepro_emit_setcc_internal_di (res, code, op0, op1);
    }

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

  if (!reg_or_0_operand (op0, SImode))
    op0 = force_reg (SImode, op0);

  if (!CONST_INT_P (op1) && !register_operand (op1, SImode))
    op1 = force_reg (SImode, op1);

  /* Return the setcc comparison.  */
  emit_insn (gen_rtx_SET (VOIDmode, res,
			  gen_rtx_fmt_ee (code, SImode, op0, op1)));

  return true;
}


/* Implement cstore patterns.  */
bool
tilepro_emit_setcc (rtx operands[], enum machine_mode cmp_mode)
{
  return
    tilepro_emit_setcc_internal (operands[0], GET_CODE (operands[1]),
				 operands[2], operands[3], cmp_mode);
}


/* Return whether CODE is a signed comparison.  */
static bool
signed_compare_p (enum rtx_code code)
{
  return (code == EQ || code == NE || code == LT || code == LE
	  || code == GT || code == GE);
}


/* Generate the comparison for an SImode conditional branch.  */
static rtx
tilepro_emit_cc_test (enum rtx_code code, rtx op0, rtx op1,
		      enum machine_mode cmp_mode, bool eq_ne_only)
{
  enum rtx_code branch_code;
  rtx temp;

  /* Check for a compare against zero using a comparison we can do
     directly.  */
  if (cmp_mode != DImode
      && op1 == const0_rtx
      && (code == EQ || code == NE
	  || (!eq_ne_only && signed_compare_p (code))))
    {
      op0 = force_reg (SImode, op0);
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

  if (cmp_mode != DImode
      && CONST_INT_P (op1) && (!satisfies_constraint_I (op1) || code == LEU))
    {
      HOST_WIDE_INT n = trunc_int_for_mode (INTVAL (op1), SImode);

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
	  if (!(symbolic_operand (op0, VOIDmode)
		|| (REG_P (op0) && REG_POINTER (op0))))
	    {
	      /* To compare against MIN_INT, we add MIN_INT and check
	         for 0.  */
	      HOST_WIDE_INT add;
	      if (n != -2147483647 - 1)
		add = -n;
	      else
		add = n;

	      op0 = force_reg (SImode, op0);
	      temp = gen_reg_rtx (SImode);
	      emit_insn (gen_addsi3 (temp, op0, gen_int_si (add)));
	      return gen_rtx_fmt_ee (reverse_condition (branch_code),
				     VOIDmode, temp, const0_rtx);
	    }
	  break;

	case LEU:
	  if (n == -1)
	    break;
	  /* FALLTHRU */

	case LTU:
	  /* Change ((unsigned)x < 0x1000) into !((unsigned)x >> 12),
	     etc.  */
	  {
	    int first = exact_log2 (code == LTU ? n : n + 1);
	    if (first != -1)
	      {
		op0 = force_reg (SImode, op0);
		temp = gen_reg_rtx (SImode);
		emit_move_insn (temp,
				gen_rtx_LSHIFTRT (SImode, op0,
						  gen_int_si (first)));
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
  temp = gen_reg_rtx (SImode);
  tilepro_emit_setcc_internal (temp, code, op0, op1, cmp_mode);

  /* Return the branch comparison.  */
  return gen_rtx_fmt_ee (branch_code, VOIDmode, temp, const0_rtx);
}


/* Generate the comparison for a conditional branch.  */
void
tilepro_emit_conditional_branch (rtx operands[], enum machine_mode cmp_mode)
{
  rtx cmp_rtx =
    tilepro_emit_cc_test (GET_CODE (operands[0]), operands[1], operands[2],
			  cmp_mode, false);
  rtx branch_rtx = gen_rtx_SET (VOIDmode, pc_rtx,
				gen_rtx_IF_THEN_ELSE (VOIDmode, cmp_rtx,
						      gen_rtx_LABEL_REF
						      (VOIDmode,
						       operands[3]),
						      pc_rtx));
  emit_jump_insn (branch_rtx);
}


/* Implement the movsicc pattern.  */
rtx
tilepro_emit_conditional_move (rtx cmp)
{
  return
    tilepro_emit_cc_test (GET_CODE (cmp), XEXP (cmp, 0), XEXP (cmp, 1),
			  GET_MODE (XEXP (cmp, 0)), true);
}


/* Return true if INSN is annotated with a REG_BR_PROB note that
   indicates it's a branch that's predicted taken.  */
static bool
cbranch_predicted_p (rtx insn)
{
  rtx x = find_reg_note (insn, REG_BR_PROB, 0);

  if (x)
    {
      int pred_val = XINT (x, 0);

      return pred_val >= REG_BR_PROB_BASE / 2;
    }

  return false;
}


/* Output assembly code for a specific branch instruction, appending
   the branch prediction flag to the opcode if appropriate.  */
static const char *
tilepro_output_simple_cbranch_with_opcode (rtx insn, const char *opcode,
					   int regop, bool netreg_p,
					   bool reverse_predicted)
{
  static char buf[64];
  sprintf (buf, "%s%s\t%%%c%d, %%l0", opcode,
	   (cbranch_predicted_p (insn) ^ reverse_predicted) ? "t" : "",
	   netreg_p ? 'N' : 'r', regop);
  return buf;
}


/* Output assembly code for a specific branch instruction, appending
   the branch prediction flag to the opcode if appropriate.  */
const char *
tilepro_output_cbranch_with_opcode (rtx insn, rtx *operands,
				    const char *opcode,
				    const char *rev_opcode,
				    int regop, bool netreg_p)
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
	tilepro_output_simple_cbranch_with_opcode (insn, opcode, regop,
						   netreg_p, false);
    }

  /* Generate a reversed branch around a direct jump.  This fallback
     does not use branch-likely instructions.  */
  not_taken = gen_label_rtx ();
  taken = operands[0];

  /* Generate the reversed branch to NOT_TAKEN.  */
  operands[0] = not_taken;
  branch_if_false =
    tilepro_output_simple_cbranch_with_opcode (insn, rev_opcode, regop,
					       netreg_p, true);
  output_asm_insn (branch_if_false, operands);

  output_asm_insn ("j\t%l0", &taken);

  /* Output NOT_TAKEN.  */
  targetm.asm_out.internal_label (asm_out_file, "L",
				  CODE_LABEL_NUMBER (not_taken));
  return "";
}


/* Output assembly code for a conditional branch instruction.  */
const char *
tilepro_output_cbranch (rtx insn, rtx *operands, bool reversed)
{
  enum rtx_code code = GET_CODE (operands[1]);
  const char *opcode;
  const char *rev_opcode;

  if (reversed)
    code = reverse_condition (code);

  switch (code)
    {
    case NE:
      opcode = "bnz";
      rev_opcode = "bz";
      break;
    case EQ:
      opcode = "bz";
      rev_opcode = "bnz";
      break;
    case GE:
      opcode = "bgez";
      rev_opcode = "blz";
      break;
    case GT:
      opcode = "bgz";
      rev_opcode = "blez";
      break;
    case LE:
      opcode = "blez";
      rev_opcode = "bgz";
      break;
    case LT:
      opcode = "blz";
      rev_opcode = "bgez";
      break;
    default:
      gcc_unreachable ();
    }

  return
    tilepro_output_cbranch_with_opcode (insn, operands, opcode, rev_opcode,
					2, false);
}


/* Implement the tablejump pattern.  */
void
tilepro_expand_tablejump (rtx op0, rtx op1)
{
  if (flag_pic)
    {
      rtx table = gen_rtx_LABEL_REF (Pmode, op1);
      rtx temp = gen_reg_rtx (Pmode);
      rtx text_label_symbol = tilepro_text_label_symbol ();
      rtx text_label_rtx = tilepro_text_label_rtx ();

      emit_insn (gen_addli_pcrel (temp, text_label_rtx,
				  table, text_label_symbol));
      emit_insn (gen_auli_pcrel (temp, temp, table, text_label_symbol));
      emit_move_insn (temp,
		      gen_rtx_PLUS (Pmode,
				    convert_to_mode (Pmode, op0, false),
				    temp));
      op0 = temp;
    }

  emit_jump_insn (gen_tablejump_aux (op0, op1));
}


/* Expand a builtin vector binary op, by calling gen function GEN with
   operands in the proper modes.  DEST is converted to DEST_MODE, and
   src0 and src1 (if DO_SRC1 is true) is converted to SRC_MODE.  */
void
tilepro_expand_builtin_vector_binop (rtx (*gen) (rtx, rtx, rtx),
				     enum machine_mode dest_mode,
				     rtx dest,
				     enum machine_mode src_mode,
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

static struct tile_builtin_info tilepro_builtin_info[TILEPRO_BUILTIN_max] = {
  { CODE_FOR_addsi3,                    NULL }, /* add */
  { CODE_FOR_insn_addb,                 NULL }, /* addb */
  { CODE_FOR_insn_addbs_u,              NULL }, /* addbs_u */
  { CODE_FOR_insn_addh,                 NULL }, /* addh */
  { CODE_FOR_insn_addhs,                NULL }, /* addhs */
  { CODE_FOR_insn_addib,                NULL }, /* addib */
  { CODE_FOR_insn_addih,                NULL }, /* addih */
  { CODE_FOR_insn_addlis,               NULL }, /* addlis */
  { CODE_FOR_ssaddsi3,                  NULL }, /* adds */
  { CODE_FOR_insn_adiffb_u,             NULL }, /* adiffb_u */
  { CODE_FOR_insn_adiffh,               NULL }, /* adiffh */
  { CODE_FOR_andsi3,                    NULL }, /* and */
  { CODE_FOR_insn_auli,                 NULL }, /* auli */
  { CODE_FOR_insn_avgb_u,               NULL }, /* avgb_u */
  { CODE_FOR_insn_avgh,                 NULL }, /* avgh */
  { CODE_FOR_insn_bitx,                 NULL }, /* bitx */
  { CODE_FOR_bswapsi2,                  NULL }, /* bytex */
  { CODE_FOR_clzsi2,                    NULL }, /* clz */
  { CODE_FOR_insn_crc32_32,             NULL }, /* crc32_32 */
  { CODE_FOR_insn_crc32_8,              NULL }, /* crc32_8 */
  { CODE_FOR_ctzsi2,                    NULL }, /* ctz */
  { CODE_FOR_insn_drain,                NULL }, /* drain */
  { CODE_FOR_insn_dtlbpr,               NULL }, /* dtlbpr */
  { CODE_FOR_insn_dword_align,          NULL }, /* dword_align */
  { CODE_FOR_insn_finv,                 NULL }, /* finv */
  { CODE_FOR_insn_flush,                NULL }, /* flush */
  { CODE_FOR_insn_fnop,                 NULL }, /* fnop */
  { CODE_FOR_insn_icoh,                 NULL }, /* icoh */
  { CODE_FOR_insn_ill,                  NULL }, /* ill */
  { CODE_FOR_insn_info,                 NULL }, /* info */
  { CODE_FOR_insn_infol,                NULL }, /* infol */
  { CODE_FOR_insn_inthb,                NULL }, /* inthb */
  { CODE_FOR_insn_inthh,                NULL }, /* inthh */
  { CODE_FOR_insn_intlb,                NULL }, /* intlb */
  { CODE_FOR_insn_intlh,                NULL }, /* intlh */
  { CODE_FOR_insn_inv,                  NULL }, /* inv */
  { CODE_FOR_insn_lb,                   NULL }, /* lb */
  { CODE_FOR_insn_lb_u,                 NULL }, /* lb_u */
  { CODE_FOR_insn_lh,                   NULL }, /* lh */
  { CODE_FOR_insn_lh_u,                 NULL }, /* lh_u */
  { CODE_FOR_insn_lnk,                  NULL }, /* lnk */
  { CODE_FOR_insn_lw,                   NULL }, /* lw */
  { CODE_FOR_insn_lw_na,                NULL }, /* lw_na */
  { CODE_FOR_insn_lb_L2,                NULL }, /* lb_L2 */
  { CODE_FOR_insn_lb_u_L2,              NULL }, /* lb_u_L2 */
  { CODE_FOR_insn_lh_L2,                NULL }, /* lh_L2 */
  { CODE_FOR_insn_lh_u_L2,              NULL }, /* lh_u_L2 */
  { CODE_FOR_insn_lw_L2,                NULL }, /* lw_L2 */
  { CODE_FOR_insn_lw_na_L2,             NULL }, /* lw_na_L2 */
  { CODE_FOR_insn_lb_miss,              NULL }, /* lb_miss */
  { CODE_FOR_insn_lb_u_miss,            NULL }, /* lb_u_miss */
  { CODE_FOR_insn_lh_miss,              NULL }, /* lh_miss */
  { CODE_FOR_insn_lh_u_miss,            NULL }, /* lh_u_miss */
  { CODE_FOR_insn_lw_miss,              NULL }, /* lw_miss */
  { CODE_FOR_insn_lw_na_miss,           NULL }, /* lw_na_miss */
  { CODE_FOR_insn_maxb_u,               NULL }, /* maxb_u */
  { CODE_FOR_insn_maxh,                 NULL }, /* maxh */
  { CODE_FOR_insn_maxib_u,              NULL }, /* maxib_u */
  { CODE_FOR_insn_maxih,                NULL }, /* maxih */
  { CODE_FOR_memory_barrier,            NULL }, /* mf */
  { CODE_FOR_insn_mfspr,                NULL }, /* mfspr */
  { CODE_FOR_insn_minb_u,               NULL }, /* minb_u */
  { CODE_FOR_insn_minh,                 NULL }, /* minh */
  { CODE_FOR_insn_minib_u,              NULL }, /* minib_u */
  { CODE_FOR_insn_minih,                NULL }, /* minih */
  { CODE_FOR_insn_mm,                   NULL }, /* mm */
  { CODE_FOR_insn_mnz,                  NULL }, /* mnz */
  { CODE_FOR_insn_mnzb,                 NULL }, /* mnzb */
  { CODE_FOR_insn_mnzh,                 NULL }, /* mnzh */
  { CODE_FOR_movsi,                     NULL }, /* move */
  { CODE_FOR_insn_movelis,              NULL }, /* movelis */
  { CODE_FOR_insn_mtspr,                NULL }, /* mtspr */
  { CODE_FOR_insn_mulhh_ss,             NULL }, /* mulhh_ss */
  { CODE_FOR_insn_mulhh_su,             NULL }, /* mulhh_su */
  { CODE_FOR_insn_mulhh_uu,             NULL }, /* mulhh_uu */
  { CODE_FOR_insn_mulhha_ss,            NULL }, /* mulhha_ss */
  { CODE_FOR_insn_mulhha_su,            NULL }, /* mulhha_su */
  { CODE_FOR_insn_mulhha_uu,            NULL }, /* mulhha_uu */
  { CODE_FOR_insn_mulhhsa_uu,           NULL }, /* mulhhsa_uu */
  { CODE_FOR_insn_mulhl_ss,             NULL }, /* mulhl_ss */
  { CODE_FOR_insn_mulhl_su,             NULL }, /* mulhl_su */
  { CODE_FOR_insn_mulhl_us,             NULL }, /* mulhl_us */
  { CODE_FOR_insn_mulhl_uu,             NULL }, /* mulhl_uu */
  { CODE_FOR_insn_mulhla_ss,            NULL }, /* mulhla_ss */
  { CODE_FOR_insn_mulhla_su,            NULL }, /* mulhla_su */
  { CODE_FOR_insn_mulhla_us,            NULL }, /* mulhla_us */
  { CODE_FOR_insn_mulhla_uu,            NULL }, /* mulhla_uu */
  { CODE_FOR_insn_mulhlsa_uu,           NULL }, /* mulhlsa_uu */
  { CODE_FOR_insn_mulll_ss,             NULL }, /* mulll_ss */
  { CODE_FOR_insn_mulll_su,             NULL }, /* mulll_su */
  { CODE_FOR_insn_mulll_uu,             NULL }, /* mulll_uu */
  { CODE_FOR_insn_mullla_ss,            NULL }, /* mullla_ss */
  { CODE_FOR_insn_mullla_su,            NULL }, /* mullla_su */
  { CODE_FOR_insn_mullla_uu,            NULL }, /* mullla_uu */
  { CODE_FOR_insn_mulllsa_uu,           NULL }, /* mulllsa_uu */
  { CODE_FOR_insn_mvnz,                 NULL }, /* mvnz */
  { CODE_FOR_insn_mvz,                  NULL }, /* mvz */
  { CODE_FOR_insn_mz,                   NULL }, /* mz */
  { CODE_FOR_insn_mzb,                  NULL }, /* mzb */
  { CODE_FOR_insn_mzh,                  NULL }, /* mzh */
  { CODE_FOR_insn_nap,                  NULL }, /* nap */
  { CODE_FOR_nop,                       NULL }, /* nop */
  { CODE_FOR_insn_nor,                  NULL }, /* nor */
  { CODE_FOR_iorsi3,                    NULL }, /* or */
  { CODE_FOR_insn_packbs_u,             NULL }, /* packbs_u */
  { CODE_FOR_insn_packhb,               NULL }, /* packhb */
  { CODE_FOR_insn_packhs,               NULL }, /* packhs */
  { CODE_FOR_insn_packlb,               NULL }, /* packlb */
  { CODE_FOR_popcountsi2,               NULL }, /* pcnt */
  { CODE_FOR_insn_prefetch,             NULL }, /* prefetch */
  { CODE_FOR_insn_prefetch_L1,          NULL }, /* prefetch_L1 */
  { CODE_FOR_rotlsi3,                   NULL }, /* rl */
  { CODE_FOR_insn_s1a,                  NULL }, /* s1a */
  { CODE_FOR_insn_s2a,                  NULL }, /* s2a */
  { CODE_FOR_insn_s3a,                  NULL }, /* s3a */
  { CODE_FOR_insn_sadab_u,              NULL }, /* sadab_u */
  { CODE_FOR_insn_sadah,                NULL }, /* sadah */
  { CODE_FOR_insn_sadah_u,              NULL }, /* sadah_u */
  { CODE_FOR_insn_sadb_u,               NULL }, /* sadb_u */
  { CODE_FOR_insn_sadh,                 NULL }, /* sadh */
  { CODE_FOR_insn_sadh_u,               NULL }, /* sadh_u */
  { CODE_FOR_insn_sb,                   NULL }, /* sb */
  { CODE_FOR_insn_seq,                  NULL }, /* seq */
  { CODE_FOR_insn_seqb,                 NULL }, /* seqb */
  { CODE_FOR_insn_seqh,                 NULL }, /* seqh */
  { CODE_FOR_insn_seqib,                NULL }, /* seqib */
  { CODE_FOR_insn_seqih,                NULL }, /* seqih */
  { CODE_FOR_insn_sh,                   NULL }, /* sh */
  { CODE_FOR_ashlsi3,                   NULL }, /* shl */
  { CODE_FOR_insn_shlb,                 NULL }, /* shlb */
  { CODE_FOR_insn_shlh,                 NULL }, /* shlh */
  { CODE_FOR_insn_shlb,                 NULL }, /* shlib */
  { CODE_FOR_insn_shlh,                 NULL }, /* shlih */
  { CODE_FOR_lshrsi3,                   NULL }, /* shr */
  { CODE_FOR_insn_shrb,                 NULL }, /* shrb */
  { CODE_FOR_insn_shrh,                 NULL }, /* shrh */
  { CODE_FOR_insn_shrb,                 NULL }, /* shrib */
  { CODE_FOR_insn_shrh,                 NULL }, /* shrih */
  { CODE_FOR_insn_slt,                  NULL }, /* slt */
  { CODE_FOR_insn_slt_u,                NULL }, /* slt_u */
  { CODE_FOR_insn_sltb,                 NULL }, /* sltb */
  { CODE_FOR_insn_sltb_u,               NULL }, /* sltb_u */
  { CODE_FOR_insn_slte,                 NULL }, /* slte */
  { CODE_FOR_insn_slte_u,               NULL }, /* slte_u */
  { CODE_FOR_insn_slteb,                NULL }, /* slteb */
  { CODE_FOR_insn_slteb_u,              NULL }, /* slteb_u */
  { CODE_FOR_insn_slteh,                NULL }, /* slteh */
  { CODE_FOR_insn_slteh_u,              NULL }, /* slteh_u */
  { CODE_FOR_insn_slth,                 NULL }, /* slth */
  { CODE_FOR_insn_slth_u,               NULL }, /* slth_u */
  { CODE_FOR_insn_sltib,                NULL }, /* sltib */
  { CODE_FOR_insn_sltib_u,              NULL }, /* sltib_u */
  { CODE_FOR_insn_sltih,                NULL }, /* sltih */
  { CODE_FOR_insn_sltih_u,              NULL }, /* sltih_u */
  { CODE_FOR_insn_sne,                  NULL }, /* sne */
  { CODE_FOR_insn_sneb,                 NULL }, /* sneb */
  { CODE_FOR_insn_sneh,                 NULL }, /* sneh */
  { CODE_FOR_ashrsi3,                   NULL }, /* sra */
  { CODE_FOR_insn_srab,                 NULL }, /* srab */
  { CODE_FOR_insn_srah,                 NULL }, /* srah */
  { CODE_FOR_insn_srab,                 NULL }, /* sraib */
  { CODE_FOR_insn_srah,                 NULL }, /* sraih */
  { CODE_FOR_subsi3,                    NULL }, /* sub */
  { CODE_FOR_insn_subb,                 NULL }, /* subb */
  { CODE_FOR_insn_subbs_u,              NULL }, /* subbs_u */
  { CODE_FOR_insn_subh,                 NULL }, /* subh */
  { CODE_FOR_insn_subhs,                NULL }, /* subhs */
  { CODE_FOR_sssubsi3,                  NULL }, /* subs */
  { CODE_FOR_insn_sw,                   NULL }, /* sw */
  { CODE_FOR_insn_tblidxb0,             NULL }, /* tblidxb0 */
  { CODE_FOR_insn_tblidxb1,             NULL }, /* tblidxb1 */
  { CODE_FOR_insn_tblidxb2,             NULL }, /* tblidxb2 */
  { CODE_FOR_insn_tblidxb3,             NULL }, /* tblidxb3 */
  { CODE_FOR_insn_tns,                  NULL }, /* tns */
  { CODE_FOR_insn_wh64,                 NULL }, /* wh64 */
  { CODE_FOR_xorsi3,                    NULL }, /* xor */
  { CODE_FOR_tilepro_network_barrier,   NULL }, /* network_barrier */
  { CODE_FOR_tilepro_idn0_receive,      NULL }, /* idn0_receive */
  { CODE_FOR_tilepro_idn1_receive,      NULL }, /* idn1_receive */
  { CODE_FOR_tilepro_idn_send,          NULL }, /* idn_send */
  { CODE_FOR_tilepro_sn_receive,        NULL }, /* sn_receive */
  { CODE_FOR_tilepro_sn_send,           NULL }, /* sn_send */
  { CODE_FOR_tilepro_udn0_receive,      NULL }, /* udn0_receive */
  { CODE_FOR_tilepro_udn1_receive,      NULL }, /* udn1_receive */
  { CODE_FOR_tilepro_udn2_receive,      NULL }, /* udn2_receive */
  { CODE_FOR_tilepro_udn3_receive,      NULL }, /* udn3_receive */
  { CODE_FOR_tilepro_udn_send,          NULL }, /* udn_send */
};


struct tilepro_builtin_def
{
  const char *name;
  enum tilepro_builtin code;
  bool is_const;
  /* The first character is the return type.  Subsequent characters
     are the argument types. See char_to_type.  */
  const char *type;
};


static const struct tilepro_builtin_def tilepro_builtins[] = {
  { "__insn_add",             TILEPRO_INSN_ADD,         true,  "lll"   },
  { "__insn_addb",            TILEPRO_INSN_ADDB,        true,  "lll"   },
  { "__insn_addbs_u",         TILEPRO_INSN_ADDBS_U,     false, "lll"   },
  { "__insn_addh",            TILEPRO_INSN_ADDH,        true,  "lll"   },
  { "__insn_addhs",           TILEPRO_INSN_ADDHS,       false, "lll"   },
  { "__insn_addi",            TILEPRO_INSN_ADD,         true,  "lll"   },
  { "__insn_addib",           TILEPRO_INSN_ADDIB,       true,  "lll"   },
  { "__insn_addih",           TILEPRO_INSN_ADDIH,       true,  "lll"   },
  { "__insn_addli",           TILEPRO_INSN_ADD,         true,  "lll"   },
  { "__insn_addlis",          TILEPRO_INSN_ADDLIS,      false, "lll"   },
  { "__insn_adds",            TILEPRO_INSN_ADDS,        false, "lll"   },
  { "__insn_adiffb_u",        TILEPRO_INSN_ADIFFB_U,    true,  "lll"   },
  { "__insn_adiffh",          TILEPRO_INSN_ADIFFH,      true,  "lll"   },
  { "__insn_and",             TILEPRO_INSN_AND,         true,  "lll"   },
  { "__insn_andi",            TILEPRO_INSN_AND,         true,  "lll"   },
  { "__insn_auli",            TILEPRO_INSN_AULI,        true,  "lll"   },
  { "__insn_avgb_u",          TILEPRO_INSN_AVGB_U,      true,  "lll"   },
  { "__insn_avgh",            TILEPRO_INSN_AVGH,        true,  "lll"   },
  { "__insn_bitx",            TILEPRO_INSN_BITX,        true,  "ll"    },
  { "__insn_bytex",           TILEPRO_INSN_BYTEX,       true,  "ll"    },
  { "__insn_clz",             TILEPRO_INSN_CLZ,         true,  "ll"    },
  { "__insn_crc32_32",        TILEPRO_INSN_CRC32_32,    true,  "lll"   },
  { "__insn_crc32_8",         TILEPRO_INSN_CRC32_8,     true,  "lll"   },
  { "__insn_ctz",             TILEPRO_INSN_CTZ,         true,  "ll"    },
  { "__insn_drain",           TILEPRO_INSN_DRAIN,       false, "v"     },
  { "__insn_dtlbpr",          TILEPRO_INSN_DTLBPR,      false, "vl"    },
  { "__insn_dword_align",     TILEPRO_INSN_DWORD_ALIGN, true,  "lllk"  },
  { "__insn_finv",            TILEPRO_INSN_FINV,        false, "vk"    },
  { "__insn_flush",           TILEPRO_INSN_FLUSH,       false, "vk"    },
  { "__insn_fnop",            TILEPRO_INSN_FNOP,        false, "v"     },
  { "__insn_icoh",            TILEPRO_INSN_ICOH,        false, "vk"    },
  { "__insn_ill",             TILEPRO_INSN_ILL,         false, "v"     },
  { "__insn_info",            TILEPRO_INSN_INFO,        false, "vl"    },
  { "__insn_infol",           TILEPRO_INSN_INFOL,       false, "vl"    },
  { "__insn_inthb",           TILEPRO_INSN_INTHB,       true,  "lll"   },
  { "__insn_inthh",           TILEPRO_INSN_INTHH,       true,  "lll"   },
  { "__insn_intlb",           TILEPRO_INSN_INTLB,       true,  "lll"   },
  { "__insn_intlh",           TILEPRO_INSN_INTLH,       true,  "lll"   },
  { "__insn_inv",             TILEPRO_INSN_INV,         false, "vp"    },
  { "__insn_lb",              TILEPRO_INSN_LB,          false, "lk"    },
  { "__insn_lb_u",            TILEPRO_INSN_LB_U,        false, "lk"    },
  { "__insn_lh",              TILEPRO_INSN_LH,          false, "lk"    },
  { "__insn_lh_u",            TILEPRO_INSN_LH_U,        false, "lk"    },
  { "__insn_lnk",             TILEPRO_INSN_LNK,         true,  "l"     },
  { "__insn_lw",              TILEPRO_INSN_LW,          false, "lk"    },
  { "__insn_lw_na",           TILEPRO_INSN_LW_NA,       false, "lk"    },
  { "__insn_lb_L2",           TILEPRO_INSN_LB_L2,       false, "lk"    },
  { "__insn_lb_u_L2",         TILEPRO_INSN_LB_U_L2,     false, "lk"    },
  { "__insn_lh_L2",           TILEPRO_INSN_LH_L2,       false, "lk"    },
  { "__insn_lh_u_L2",         TILEPRO_INSN_LH_U_L2,     false, "lk"    },
  { "__insn_lw_L2",           TILEPRO_INSN_LW_L2,       false, "lk"    },
  { "__insn_lw_na_L2",        TILEPRO_INSN_LW_NA_L2,    false, "lk"    },
  { "__insn_lb_miss",         TILEPRO_INSN_LB_MISS,     false, "lk"    },
  { "__insn_lb_u_miss",       TILEPRO_INSN_LB_U_MISS,   false, "lk"    },
  { "__insn_lh_miss",         TILEPRO_INSN_LH_MISS,     false, "lk"    },
  { "__insn_lh_u_miss",       TILEPRO_INSN_LH_U_MISS,   false, "lk"    },
  { "__insn_lw_miss",         TILEPRO_INSN_LW_MISS,     false, "lk"    },
  { "__insn_lw_na_miss",      TILEPRO_INSN_LW_NA_MISS,  false, "lk"    },
  { "__insn_maxb_u",          TILEPRO_INSN_MAXB_U,      true,  "lll"   },
  { "__insn_maxh",            TILEPRO_INSN_MAXH,        true,  "lll"   },
  { "__insn_maxib_u",         TILEPRO_INSN_MAXIB_U,     true,  "lll"   },
  { "__insn_maxih",           TILEPRO_INSN_MAXIH,       true,  "lll"   },
  { "__insn_mf",              TILEPRO_INSN_MF,          false, "v"     },
  { "__insn_mfspr",           TILEPRO_INSN_MFSPR,       false, "ll"    },
  { "__insn_minb_u",          TILEPRO_INSN_MINB_U,      true,  "lll"   },
  { "__insn_minh",            TILEPRO_INSN_MINH,        true,  "lll"   },
  { "__insn_minib_u",         TILEPRO_INSN_MINIB_U,     true,  "lll"   },
  { "__insn_minih",           TILEPRO_INSN_MINIH,       true,  "lll"   },
  { "__insn_mm",              TILEPRO_INSN_MM,          true,  "lllll" },
  { "__insn_mnz",             TILEPRO_INSN_MNZ,         true,  "lll"   },
  { "__insn_mnzb",            TILEPRO_INSN_MNZB,        true,  "lll"   },
  { "__insn_mnzh",            TILEPRO_INSN_MNZH,        true,  "lll"   },
  { "__insn_move",            TILEPRO_INSN_MOVE,        true,  "ll"    },
  { "__insn_movei",           TILEPRO_INSN_MOVE,        true,  "ll"    },
  { "__insn_moveli",          TILEPRO_INSN_MOVE,        true,  "ll"    },
  { "__insn_movelis",         TILEPRO_INSN_MOVELIS,     false, "ll"    },
  { "__insn_mtspr",           TILEPRO_INSN_MTSPR,       false, "vll"   },
  { "__insn_mulhh_ss",        TILEPRO_INSN_MULHH_SS,    true,  "lll"   },
  { "__insn_mulhh_su",        TILEPRO_INSN_MULHH_SU,    true,  "lll"   },
  { "__insn_mulhh_uu",        TILEPRO_INSN_MULHH_UU,    true,  "lll"   },
  { "__insn_mulhha_ss",       TILEPRO_INSN_MULHHA_SS,   true,  "llll"  },
  { "__insn_mulhha_su",       TILEPRO_INSN_MULHHA_SU,   true,  "llll"  },
  { "__insn_mulhha_uu",       TILEPRO_INSN_MULHHA_UU,   true,  "llll"  },
  { "__insn_mulhhsa_uu",      TILEPRO_INSN_MULHHSA_UU,  true,  "llll"  },
  { "__insn_mulhl_ss",        TILEPRO_INSN_MULHL_SS,    true,  "lll"   },
  { "__insn_mulhl_su",        TILEPRO_INSN_MULHL_SU,    true,  "lll"   },
  { "__insn_mulhl_us",        TILEPRO_INSN_MULHL_US,    true,  "lll"   },
  { "__insn_mulhl_uu",        TILEPRO_INSN_MULHL_UU,    true,  "lll"   },
  { "__insn_mulhla_ss",       TILEPRO_INSN_MULHLA_SS,   true,  "llll"  },
  { "__insn_mulhla_su",       TILEPRO_INSN_MULHLA_SU,   true,  "llll"  },
  { "__insn_mulhla_us",       TILEPRO_INSN_MULHLA_US,   true,  "llll"  },
  { "__insn_mulhla_uu",       TILEPRO_INSN_MULHLA_UU,   true,  "llll"  },
  { "__insn_mulhlsa_uu",      TILEPRO_INSN_MULHLSA_UU,  true,  "llll"  },
  { "__insn_mulll_ss",        TILEPRO_INSN_MULLL_SS,    true,  "lll"   },
  { "__insn_mulll_su",        TILEPRO_INSN_MULLL_SU,    true,  "lll"   },
  { "__insn_mulll_uu",        TILEPRO_INSN_MULLL_UU,    true,  "lll"   },
  { "__insn_mullla_ss",       TILEPRO_INSN_MULLLA_SS,   true,  "llll"  },
  { "__insn_mullla_su",       TILEPRO_INSN_MULLLA_SU,   true,  "llll"  },
  { "__insn_mullla_uu",       TILEPRO_INSN_MULLLA_UU,   true,  "llll"  },
  { "__insn_mulllsa_uu",      TILEPRO_INSN_MULLLSA_UU,  true,  "llll"  },
  { "__insn_mvnz",            TILEPRO_INSN_MVNZ,        true,  "llll"  },
  { "__insn_mvz",             TILEPRO_INSN_MVZ,         true,  "llll"  },
  { "__insn_mz",              TILEPRO_INSN_MZ,          true,  "lll"   },
  { "__insn_mzb",             TILEPRO_INSN_MZB,         true,  "lll"   },
  { "__insn_mzh",             TILEPRO_INSN_MZH,         true,  "lll"   },
  { "__insn_nap",             TILEPRO_INSN_NAP,         false, "v"     },
  { "__insn_nop",             TILEPRO_INSN_NOP,         true,  "v"     },
  { "__insn_nor",             TILEPRO_INSN_NOR,         true,  "lll"   },
  { "__insn_or",              TILEPRO_INSN_OR,          true,  "lll"   },
  { "__insn_ori",             TILEPRO_INSN_OR,          true,  "lll"   },
  { "__insn_packbs_u",        TILEPRO_INSN_PACKBS_U,    false, "lll"   },
  { "__insn_packhb",          TILEPRO_INSN_PACKHB,      true,  "lll"   },
  { "__insn_packhs",          TILEPRO_INSN_PACKHS,      false, "lll"   },
  { "__insn_packlb",          TILEPRO_INSN_PACKLB,      true,  "lll"   },
  { "__insn_pcnt",            TILEPRO_INSN_PCNT,        true,  "ll"    },
  { "__insn_prefetch",        TILEPRO_INSN_PREFETCH,    false, "vk"    },
  { "__insn_prefetch_L1",     TILEPRO_INSN_PREFETCH_L1, false, "vk"    },
  { "__insn_rl",              TILEPRO_INSN_RL,          true,  "lll"   },
  { "__insn_rli",             TILEPRO_INSN_RL,          true,  "lll"   },
  { "__insn_s1a",             TILEPRO_INSN_S1A,         true,  "lll"   },
  { "__insn_s2a",             TILEPRO_INSN_S2A,         true,  "lll"   },
  { "__insn_s3a",             TILEPRO_INSN_S3A,         true,  "lll"   },
  { "__insn_sadab_u",         TILEPRO_INSN_SADAB_U,     true,  "llll"  },
  { "__insn_sadah",           TILEPRO_INSN_SADAH,       true,  "llll"  },
  { "__insn_sadah_u",         TILEPRO_INSN_SADAH_U,     true,  "llll"  },
  { "__insn_sadb_u",          TILEPRO_INSN_SADB_U,      true,  "lll"   },
  { "__insn_sadh",            TILEPRO_INSN_SADH,        true,  "lll"   },
  { "__insn_sadh_u",          TILEPRO_INSN_SADH_U,      true,  "lll"   },
  { "__insn_sb",              TILEPRO_INSN_SB,          false, "vpl"   },
  { "__insn_seq",             TILEPRO_INSN_SEQ,         true,  "lll"   },
  { "__insn_seqb",            TILEPRO_INSN_SEQB,        true,  "lll"   },
  { "__insn_seqh",            TILEPRO_INSN_SEQH,        true,  "lll"   },
  { "__insn_seqi",            TILEPRO_INSN_SEQ,         true,  "lll"   },
  { "__insn_seqib",           TILEPRO_INSN_SEQIB,       true,  "lll"   },
  { "__insn_seqih",           TILEPRO_INSN_SEQIH,       true,  "lll"   },
  { "__insn_sh",              TILEPRO_INSN_SH,          false, "vpl"   },
  { "__insn_shl",             TILEPRO_INSN_SHL,         true,  "lll"   },
  { "__insn_shlb",            TILEPRO_INSN_SHLB,        true,  "lll"   },
  { "__insn_shlh",            TILEPRO_INSN_SHLH,        true,  "lll"   },
  { "__insn_shli",            TILEPRO_INSN_SHL,         true,  "lll"   },
  { "__insn_shlib",           TILEPRO_INSN_SHLIB,       true,  "lll"   },
  { "__insn_shlih",           TILEPRO_INSN_SHLIH,       true,  "lll"   },
  { "__insn_shr",             TILEPRO_INSN_SHR,         true,  "lll"   },
  { "__insn_shrb",            TILEPRO_INSN_SHRB,        true,  "lll"   },
  { "__insn_shrh",            TILEPRO_INSN_SHRH,        true,  "lll"   },
  { "__insn_shri",            TILEPRO_INSN_SHR,         true,  "lll"   },
  { "__insn_shrib",           TILEPRO_INSN_SHRIB,       true,  "lll"   },
  { "__insn_shrih",           TILEPRO_INSN_SHRIH,       true,  "lll"   },
  { "__insn_slt",             TILEPRO_INSN_SLT,         true,  "lll"   },
  { "__insn_slt_u",           TILEPRO_INSN_SLT_U,       true,  "lll"   },
  { "__insn_sltb",            TILEPRO_INSN_SLTB,        true,  "lll"   },
  { "__insn_sltb_u",          TILEPRO_INSN_SLTB_U,      true,  "lll"   },
  { "__insn_slte",            TILEPRO_INSN_SLTE,        true,  "lll"   },
  { "__insn_slte_u",          TILEPRO_INSN_SLTE_U,      true,  "lll"   },
  { "__insn_slteb",           TILEPRO_INSN_SLTEB,       true,  "lll"   },
  { "__insn_slteb_u",         TILEPRO_INSN_SLTEB_U,     true,  "lll"   },
  { "__insn_slteh",           TILEPRO_INSN_SLTEH,       true,  "lll"   },
  { "__insn_slteh_u",         TILEPRO_INSN_SLTEH_U,     true,  "lll"   },
  { "__insn_slth",            TILEPRO_INSN_SLTH,        true,  "lll"   },
  { "__insn_slth_u",          TILEPRO_INSN_SLTH_U,      true,  "lll"   },
  { "__insn_slti",            TILEPRO_INSN_SLT,         true,  "lll"   },
  { "__insn_slti_u",          TILEPRO_INSN_SLT_U,       true,  "lll"   },
  { "__insn_sltib",           TILEPRO_INSN_SLTIB,       true,  "lll"   },
  { "__insn_sltib_u",         TILEPRO_INSN_SLTIB_U,     true,  "lll"   },
  { "__insn_sltih",           TILEPRO_INSN_SLTIH,       true,  "lll"   },
  { "__insn_sltih_u",         TILEPRO_INSN_SLTIH_U,     true,  "lll"   },
  { "__insn_sne",             TILEPRO_INSN_SNE,         true,  "lll"   },
  { "__insn_sneb",            TILEPRO_INSN_SNEB,        true,  "lll"   },
  { "__insn_sneh",            TILEPRO_INSN_SNEH,        true,  "lll"   },
  { "__insn_sra",             TILEPRO_INSN_SRA,         true,  "lll"   },
  { "__insn_srab",            TILEPRO_INSN_SRAB,        true,  "lll"   },
  { "__insn_srah",            TILEPRO_INSN_SRAH,        true,  "lll"   },
  { "__insn_srai",            TILEPRO_INSN_SRA,         true,  "lll"   },
  { "__insn_sraib",           TILEPRO_INSN_SRAIB,       true,  "lll"   },
  { "__insn_sraih",           TILEPRO_INSN_SRAIH,       true,  "lll"   },
  { "__insn_sub",             TILEPRO_INSN_SUB,         true,  "lll"   },
  { "__insn_subb",            TILEPRO_INSN_SUBB,        true,  "lll"   },
  { "__insn_subbs_u",         TILEPRO_INSN_SUBBS_U,     false, "lll"   },
  { "__insn_subh",            TILEPRO_INSN_SUBH,        true,  "lll"   },
  { "__insn_subhs",           TILEPRO_INSN_SUBHS,       false, "lll"   },
  { "__insn_subs",            TILEPRO_INSN_SUBS,        false, "lll"   },
  { "__insn_sw",              TILEPRO_INSN_SW,          false, "vpl"   },
  { "__insn_tblidxb0",        TILEPRO_INSN_TBLIDXB0,    true,  "lll"   },
  { "__insn_tblidxb1",        TILEPRO_INSN_TBLIDXB1,    true,  "lll"   },
  { "__insn_tblidxb2",        TILEPRO_INSN_TBLIDXB2,    true,  "lll"   },
  { "__insn_tblidxb3",        TILEPRO_INSN_TBLIDXB3,    true,  "lll"   },
  { "__insn_tns",             TILEPRO_INSN_TNS,         false, "lp"    },
  { "__insn_wh64",            TILEPRO_INSN_WH64,        false, "vp"    },
  { "__insn_xor",             TILEPRO_INSN_XOR,         true,  "lll"   },
  { "__insn_xori",            TILEPRO_INSN_XOR,         true,  "lll"   },
  { "__tile_network_barrier", TILEPRO_NETWORK_BARRIER,  false, "v"     },
  { "__tile_idn0_receive",    TILEPRO_IDN0_RECEIVE,     false, "l"     },
  { "__tile_idn1_receive",    TILEPRO_IDN1_RECEIVE,     false, "l"     },
  { "__tile_idn_send",        TILEPRO_IDN_SEND,         false, "vl"    },
  { "__tile_sn_receive",      TILEPRO_SN_RECEIVE,       false, "l"     },
  { "__tile_sn_send",         TILEPRO_SN_SEND,          false, "vl"    },
  { "__tile_udn0_receive",    TILEPRO_UDN0_RECEIVE,     false, "l"     },
  { "__tile_udn1_receive",    TILEPRO_UDN1_RECEIVE,     false, "l"     },
  { "__tile_udn2_receive",    TILEPRO_UDN2_RECEIVE,     false, "l"     },
  { "__tile_udn3_receive",    TILEPRO_UDN3_RECEIVE,     false, "l"     },
  { "__tile_udn_send",        TILEPRO_UDN_SEND,         false, "vl"    },
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
    case 'l':
      return long_unsigned_type_node;
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
tilepro_init_builtins (void)
{
  size_t i;

  for (i = 0; i < ARRAY_SIZE (tilepro_builtins); i++)
    {
      const struct tilepro_builtin_def *p = &tilepro_builtins[i];
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

      if (tilepro_builtin_info[p->code].fndecl == NULL)
	tilepro_builtin_info[p->code].fndecl = decl;
    }
}


/* Implement TARGET_EXPAND_BUILTIN.  */
static rtx
tilepro_expand_builtin (tree exp,
			rtx target,
			rtx subtarget ATTRIBUTE_UNUSED,
			enum machine_mode mode ATTRIBUTE_UNUSED,
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

  if (fcode >= TILEPRO_BUILTIN_max)
    internal_error ("bad builtin fcode");
  icode = tilepro_builtin_info[fcode].icode;
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
      op[opnum] = copy_to_mode_reg (insn_op->mode, op[opnum]);

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
      enum machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
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
  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}


/* Implement TARGET_BUILTIN_DECL.  */
static tree
tilepro_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= TILEPRO_BUILTIN_max)
    return error_mark_node;

  return tilepro_builtin_info[code].fndecl;
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
	  || regno == TILEPRO_PIC_TEXT_LABEL_REGNUM)
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
tilepro_saved_regs_size (void)
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
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (Pmode, addr);
  rtx mov = gen_movsi (mem, reg);

  /* Describe what just happened in a way that dwarf understands.  We
     use temporary registers to hold the address to make scheduling
     easier, and use the REG_CFA_OFFSET to describe the address as an
     offset from the CFA.  */
  rtx reg_note = gen_rtx_REG (Pmode, regno_note);
  rtx cfa_relative_addr = gen_rtx_PLUS (Pmode, cfa, gen_int_si (cfa_offset));
  rtx cfa_relative_mem = gen_frame_mem (Pmode, cfa_relative_addr);
  rtx real = gen_rtx_SET (VOIDmode, cfa_relative_mem, reg_note);
  add_reg_note (mov, REG_CFA_OFFSET, real);

  return emit_insn (mov);
}


/* Emit a load in the stack frame to load REGNO from address ADDR.
   Add a REG_CFA_RESTORE note to CFA_RESTORES if CFA_RESTORES is
   non-null.  Return the emitted insn.  */
static rtx
frame_emit_load (int regno, rtx addr, rtx *cfa_restores)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (Pmode, addr);
  if (cfa_restores)
    *cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, *cfa_restores);
  return emit_insn (gen_movsi (reg, mem));
}


/* Helper function to set RTX_FRAME_RELATED_P on instructions,
   including sequences.  */
static rtx
set_frame_related_p (void)
{
  rtx seq = get_insns ();
  rtx insn;

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
static rtx
emit_sp_adjust (int offset, int *next_scratch_regno, bool frame_related,
		rtx reg_notes)
{
  rtx to_add;
  rtx imm_rtx = gen_int_si (offset);

  rtx insn;
  if (satisfies_constraint_J (imm_rtx))
    {
      /* We can add this using a single addi or addli.  */
      to_add = imm_rtx;
    }
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, (*next_scratch_regno)--);
      tilepro_expand_set_const32 (tmp, imm_rtx);
      to_add = tmp;
    }

  /* Actually adjust the stack pointer.  */
  insn = emit_insn (gen_sp_adjust (stack_pointer_rtx, stack_pointer_rtx,
				   to_add));
  REG_NOTES (insn) = reg_notes;

  /* Describe what just happened in a way that dwarf understands.  */
  if (frame_related)
    {
      rtx real = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
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
tilepro_current_function_is_leaf (void)
{
  return crtl->is_leaf && !cfun->machine->calls_tls_get_addr;
}


/* Return the frame size.  */
static int
compute_total_frame_size (void)
{
  int total_size = (get_frame_size () + tilepro_saved_regs_size ()
		    + crtl->outgoing_args_size
		    + crtl->args.pretend_args_size);

  if (!tilepro_current_function_is_leaf () || cfun->calls_alloca)
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
tilepro_can_use_return_insn_p (void)
{
  return (reload_completed
	  && cfun->static_chain_decl == 0
	  && compute_total_frame_size () == 0
	  && tilepro_current_function_is_leaf ()
	  && !crtl->profile && !df_regs_ever_live_p (TILEPRO_LINK_REGNUM));
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
      base_reg_rtx = stack_pointer_rtx;
      offset_from_base = offset_from_sp;
    }

  if (offset_from_base == 0)
    return base_reg_rtx;

  /* Compute the new value of the stack pointer.  */
  tmp_reg_rtx = gen_rtx_REG (Pmode, (*next_scratch_regno)--);
  offset_rtx = gen_int_si (offset_from_base);

  if (!tilepro_expand_addsi (tmp_reg_rtx, base_reg_rtx, offset_rtx))
    {
      emit_insn (gen_rtx_SET (VOIDmode, tmp_reg_rtx,
			      gen_rtx_PLUS (Pmode, base_reg_rtx,
					    offset_rtx)));
    }

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
         | saved regs  | tilepro_saved_regs_size() bytes
   FP -> +-------------+
         |    ...      | 
         |   vars      | get_frame_size() bytes
         +-------------+
         |    ...      | 
         |  outgoing   | 
         |  stack args | crtl->outgoing_args_size bytes
         +-------------+
         | HFP         | 4 bytes (only here if nonleaf / alloca)
         +-------------+
         | callee lr   | 4 bytes (only here if nonleaf / alloca)
         | save        | 
   SP -> +-------------+

  HFP == incoming SP.

  For functions with a frame larger than 32767 bytes, or which use
  alloca (), r52 is used as a frame pointer.  Otherwise there is no
  frame pointer.

  FP is saved at SP+4 before calling a subroutine so the
  callee can chain.  */
void
tilepro_expand_prologue (void)
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
  if (df_regs_ever_live_p (TILEPRO_LINK_REGNUM) || crtl->calls_eh_return)
    FRP (frame_emit_store (TILEPRO_LINK_REGNUM, TILEPRO_LINK_REGNUM,
			   stack_pointer_rtx, stack_pointer_rtx, 0));

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
      insn = FRP (emit_move_insn (gen_rtx_REG (word_mode, fp_copy_regno),
				  hard_frame_pointer_rtx));
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
  else if (!tilepro_current_function_is_leaf ())
    {
      /* Copy the old stack pointer aside so we can save it later.  */
      sp_copy_regno = next_scratch_regno--;
      emit_move_insn (gen_rtx_REG (Pmode, sp_copy_regno),
		      stack_pointer_rtx);
    }

  if (tilepro_current_function_is_leaf ())
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
      rtx size_rtx = gen_int_si (-(total_size - UNITS_PER_WORD));

      if (add_operand (size_rtx, Pmode))
	{
	  /* Expose more parallelism by computing this value from the
	     original stack pointer, not the one after we have pushed
	     the frame.  */
	  rtx p = gen_rtx_PLUS (Pmode, stack_pointer_rtx, size_rtx);
	  emit_insn (gen_rtx_SET (VOIDmode, chain_addr, p));
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
	  emit_insn (gen_rtx_SET (VOIDmode, chain_addr, p));
	}

      /* Save our frame pointer for backtrace chaining.  */
      emit_insn (gen_movsi (gen_frame_mem (SImode, chain_addr),
			    gen_rtx_REG (SImode, sp_copy_regno)));
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
	    rtx p = compute_frame_addr (offset, &next_scratch_regno);
	    r = gen_rtx_REG (word_mode, next_scratch_regno--);
	    reg_save_addr[which_scratch] = r;

	    emit_insn (gen_rtx_SET (VOIDmode, r, p));
	  }
	else
	  {
	    /* Advance to the next stack slot to store this register.  */
	    int stride = ROUND_ROBIN_SIZE * -UNITS_PER_WORD;
	    rtx p = gen_rtx_PLUS (Pmode, r, GEN_INT (stride));
	    emit_insn (gen_rtx_SET (VOIDmode, r, p));
	  }

	/* Save this register to the stack (but use the old fp value
	   we copied aside if appropriate).  */
	from_regno = (fp_copy_regno >= 0
		      && regno ==
		      HARD_FRAME_POINTER_REGNUM) ? fp_copy_regno : regno;
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
tilepro_expand_epilogue (bool sibcall_p)
{
  /* We round-robin through four scratch registers to hold temporary
     addresses for saving registers, to make instruction scheduling
     easier.  */
  rtx reg_save_addr[ROUND_ROBIN_SIZE] = {
    NULL_RTX, NULL_RTX, NULL_RTX, NULL_RTX
  };
  rtx last_insn, insn;
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
  if (df_regs_ever_live_p (TILEPRO_LINK_REGNUM))
    {
      insn = frame_emit_load (TILEPRO_LINK_REGNUM,
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
	    /* Advance to the next stack slot to store this
	       register.  */
	    int stride = ROUND_ROBIN_SIZE * -UNITS_PER_WORD;
	    rtx p = gen_rtx_PLUS (Pmode, r, GEN_INT (stride));
	    emit_insn (gen_rtx_SET (VOIDmode, r, p));
	  }

	if (fp_copy_regno >= 0 && regno == HARD_FRAME_POINTER_REGNUM)
	  frame_emit_load (fp_copy_regno, r, NULL);
	else
	  frame_emit_load (regno, r, &cfa_restores);

	offset -= UNITS_PER_WORD;
	which_scratch = (which_scratch + 1) % ROUND_ROBIN_SIZE;
      }

  if (!tilepro_current_function_is_leaf ())
    cfa_restores =
      alloc_reg_note (REG_CFA_RESTORE, stack_pointer_rtx, cfa_restores);

  emit_insn (gen_blockage ());

  if (frame_pointer_needed)
    {
      /* Restore the old stack pointer by copying from the frame
         pointer.  */
      insn = emit_insn (gen_sp_restore (stack_pointer_rtx,
					hard_frame_pointer_rtx));
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
    emit_insn (gen_sp_adjust (stack_pointer_rtx, stack_pointer_rtx,
			      EH_RETURN_STACKADJ_RTX));

  /* Restore the old frame pointer.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx,
			     gen_rtx_REG (Pmode, fp_copy_regno));
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
      /* Emit the actual 'return' instruction.  */
      emit_jump_insn (gen__return ());
    }
  else
    {
      emit_use (gen_rtx_REG (Pmode, TILEPRO_LINK_REGNUM));
    }

  /* Mark all insns we just emitted as frame-related.  */
  for (; last_insn != NULL_RTX; last_insn = next_insn (last_insn))
    RTX_FRAME_RELATED_P (last_insn) = 1;
}

#undef ROUND_ROBIN_SIZE


/* Implement INITIAL_ELIMINATION_OFFSET.  */
int
tilepro_initial_elimination_offset (int from, int to)
{
  int total_size = compute_total_frame_size ();

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return (total_size - crtl->args.pretend_args_size
	      - tilepro_saved_regs_size ());
    }
  else if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    {
      return -(crtl->args.pretend_args_size + tilepro_saved_regs_size ());
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


/* Return an RTX indicating where the return address to the
   calling function can be found.  */
rtx
tilepro_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, TILEPRO_LINK_REGNUM);
}


/* Implement EH_RETURN_HANDLER_RTX.  */
rtx
tilepro_eh_return_handler_rtx (void)
{
  /* The MEM needs to be volatile to prevent it from being
     deleted.  */
  rtx tmp = gen_frame_mem (Pmode, hard_frame_pointer_rtx);
  MEM_VOLATILE_P (tmp) = true;
  return tmp;
}



/* Registers  */

/* Implemnet TARGET_CONDITIONAL_REGISTER_USAGE.  */
static void
tilepro_conditional_register_usage (void)
{
  global_regs[TILEPRO_NETORDER_REGNUM] = 1;
  /* TILEPRO_PIC_TEXT_LABEL_REGNUM is conditionally used.  It is a
     member of fixed_regs, and therefore must be member of
     call_used_regs, but it is not a member of call_really_used_regs[]
     because it is not clobbered by a call.  */
  if (TILEPRO_PIC_TEXT_LABEL_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[TILEPRO_PIC_TEXT_LABEL_REGNUM] = 1;
      call_used_regs[TILEPRO_PIC_TEXT_LABEL_REGNUM] = 1;
    }
  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
}


/* Implement TARGET_FRAME_POINTER_REQUIRED.  */
static bool
tilepro_frame_pointer_required (void)
{
  return crtl->calls_eh_return || cfun->calls_alloca;
}



/* Scheduling and reorg  */

/* Return the length of INSN.  LENGTH is the initial length computed
   by attributes in the machine-description file.  This is where we
   account for bundles.  */
int
tilepro_adjust_insn_length (rtx insn, int length)
{
  enum machine_mode mode = GET_MODE (insn);

  /* A non-termininating instruction in a bundle has length 0.  */
  if (mode == SImode)
    return 0;

  /* By default, there is not length adjustment.  */
  return length;
}


/* Implement TARGET_SCHED_ISSUE_RATE.  */
static int
tilepro_issue_rate (void)
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
tilepro_sched_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  /* If we have a true dependence, INSN is a call, and DEP_INSN
     defines a register that is needed by the call (argument or stack
     pointer), set its latency to 0 so that it can be bundled with
     the call.  Explicitly check for and exclude the case when
     DEP_INSN defines the target of the jump.  */
  if (CALL_P (insn) && REG_NOTE_KIND (link) == REG_DEP_TRUE)
    {
      rtx target = get_jump_target (insn);
      if (!REG_P (target) || !set_of (target, dep_insn))
	return 0;
    }

  return cost;
}


/* Skip over irrelevant NOTEs and such and look for the next insn we
   would consider bundling.  */
static rtx
next_insn_to_bundle (rtx r, rtx end)
{
  for (; r != end; r = NEXT_INSN (r))
    {
      if (NONDEBUG_INSN_P (r)
	  && GET_CODE (PATTERN (r)) != USE
	  && GET_CODE (PATTERN (r)) != CLOBBER)
	return r;
    }

  return NULL_RTX;
}


/* Go through all insns, and use the information generated during
   scheduling to generate SEQUENCEs to represent bundles of
   instructions issued simultaneously.  */
static void
tilepro_gen_bundles (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
  {
    rtx insn, next;
    rtx end = NEXT_INSN (BB_END (bb));

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
      }
  }
}


/* Helper function for tilepro_fixup_pcrel_references.  */
static void
replace_pc_relative_symbol_ref (rtx insn, rtx opnds[4], bool first_insn_p)
{
  rtx new_insns;

  start_sequence ();

  if (flag_pic == 1)
    {
      if (!first_insn_p)
	{
	  emit_insn (gen_add_got16 (opnds[0], tilepro_got_rtx (),
				    opnds[2]));
	  emit_insn (gen_insn_lw (opnds[0], opnds[0]));
	}
    }
  else
    {
      if (first_insn_p)
	{
	  emit_insn (gen_addhi_got32 (opnds[0], tilepro_got_rtx (),
				      opnds[2]));
	}
      else
	{
	  emit_insn (gen_addlo_got32 (opnds[0], opnds[1], opnds[2]));
	  emit_insn (gen_insn_lw (opnds[0], opnds[0]));
	}
    }

  new_insns = get_insns ();
  end_sequence ();

  if (new_insns)
    emit_insn_before (new_insns, insn);

  delete_insn (insn);
}


/* Returns whether INSN is a pc-relative addli insn.   */
static bool
match_addli_pcrel (rtx insn)
{
  rtx pattern = PATTERN (insn);
  rtx unspec;

  if (GET_CODE (pattern) != SET)
    return false;

  if (GET_CODE (SET_SRC (pattern)) != LO_SUM)
    return false;

  if (GET_CODE (XEXP (SET_SRC (pattern), 1)) != CONST)
    return false;

  unspec = XEXP (XEXP (SET_SRC (pattern), 1), 0);

  return (GET_CODE (unspec) == UNSPEC
	  && XINT (unspec, 1) == UNSPEC_PCREL_SYM);
}


/* Helper function for tilepro_fixup_pcrel_references.  */
static void
replace_addli_pcrel (rtx insn)
{
  rtx pattern = PATTERN (insn);
  rtx set_src;
  rtx unspec;
  rtx opnds[4];
  bool first_insn_p;

  gcc_assert (GET_CODE (pattern) == SET);
  opnds[0] = SET_DEST (pattern);

  set_src = SET_SRC (pattern);
  gcc_assert (GET_CODE (set_src) == LO_SUM);
  gcc_assert (GET_CODE (XEXP (set_src, 1)) == CONST);
  opnds[1] = XEXP (set_src, 0);

  unspec = XEXP (XEXP (set_src, 1), 0);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_PCREL_SYM);
  opnds[2] = XVECEXP (unspec, 0, 0);
  opnds[3] = XVECEXP (unspec, 0, 1);

  /* We only need to replace SYMBOL_REFs, not LABEL_REFs.  */
  if (GET_CODE (opnds[2]) != SYMBOL_REF)
    return;

  first_insn_p = (opnds[1] == tilepro_text_label_rtx ());

  replace_pc_relative_symbol_ref (insn, opnds, first_insn_p);
}


/* Returns whether INSN is a pc-relative auli insn.   */
static bool
match_auli_pcrel (rtx insn)
{
  rtx pattern = PATTERN (insn);
  rtx high;
  rtx unspec;

  if (GET_CODE (pattern) != SET)
    return false;

  if (GET_CODE (SET_SRC (pattern)) != PLUS)
    return false;

  high = XEXP (SET_SRC (pattern), 1);

  if (GET_CODE (high) != HIGH
      || GET_CODE (XEXP (high, 0)) != CONST)
    return false;

  unspec = XEXP (XEXP (high, 0), 0);

  return (GET_CODE (unspec) == UNSPEC
	  && XINT (unspec, 1) == UNSPEC_PCREL_SYM);
}


/* Helper function for tilepro_fixup_pcrel_references.  */
static void
replace_auli_pcrel (rtx insn)
{
  rtx pattern = PATTERN (insn);
  rtx set_src;
  rtx high;
  rtx unspec;
  rtx opnds[4];
  bool first_insn_p;

  gcc_assert (GET_CODE (pattern) == SET);
  opnds[0] = SET_DEST (pattern);

  set_src = SET_SRC (pattern);
  gcc_assert (GET_CODE (set_src) == PLUS);
  opnds[1] = XEXP (set_src, 0);

  high = XEXP (set_src, 1);
  gcc_assert (GET_CODE (high) == HIGH);
  gcc_assert (GET_CODE (XEXP (high, 0)) == CONST);

  unspec = XEXP (XEXP (high, 0), 0);
  gcc_assert (GET_CODE (unspec) == UNSPEC);
  gcc_assert (XINT (unspec, 1) == UNSPEC_PCREL_SYM);
  opnds[2] = XVECEXP (unspec, 0, 0);
  opnds[3] = XVECEXP (unspec, 0, 1);

  /* We only need to replace SYMBOL_REFs, not LABEL_REFs.  */
  if (GET_CODE (opnds[2]) != SYMBOL_REF)
    return;

  first_insn_p = (opnds[1] == tilepro_text_label_rtx ());

  replace_pc_relative_symbol_ref (insn, opnds, first_insn_p);
}


/* We generate PC relative SYMBOL_REFs as an optimization, to avoid
   going through the GOT when the symbol is local to the compilation
   unit.  But such a symbol requires that the common text_label that
   we generate at the beginning of the function be in the same section
   as the reference to the SYMBOL_REF.  This may not be true if we
   generate hot/cold sections.  This function looks for such cases and
   replaces such references with the longer sequence going through the
   GOT.

   We expect one of the following two instruction sequences:
   addli tmp1, txt_label_reg, lo16(sym - txt_label)
   auli  tmp2,          tmp1, ha16(sym - txt_label)

   auli  tmp1, txt_label_reg, ha16(sym - txt_label)
   addli tmp2,          tmp1, lo16(sym - txt_label)

   If we're compiling -fpic, we replace the first instruction with
   nothing, and the second instruction with:

   addli tmp2, got_rtx, got(sym)
   lw    tmp2,    tmp2
   
   If we're compiling -fPIC, we replace the first instruction with:

   auli  tmp1, got_rtx, got_ha16(sym)

   and the second instruction with:

   addli tmp2,    tmp1, got_lo16(sym)
   lw    tmp2,    tmp2

   Note that we're careful to disturb the instruction sequence as
   little as possible, since it's very late in the compilation
   process.
*/
static void
tilepro_fixup_pcrel_references (void)
{
  rtx insn, next_insn;
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

      if (match_addli_pcrel (insn))
	replace_addli_pcrel (insn);
      else if (match_auli_pcrel (insn))
	replace_auli_pcrel (insn);
    }
}


/* Ensure that no var tracking notes are emitted in the middle of a
   three-instruction bundle.  */
static void
reorder_var_tracking_notes (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
  {
    rtx insn, next;
    rtx queue = NULL_RTX;
    bool in_bundle = false;

    for (insn = BB_HEAD (bb); insn != BB_END (bb); insn = next)
      {
	next = NEXT_INSN (insn);

	if (INSN_P (insn))
	  {
	    /* Emit queued up notes at the last instruction of a bundle.  */
	    if (GET_MODE (insn) == QImode)
	      {
		while (queue)
		  {
		    rtx next_queue = PREV_INSN (queue);
		    PREV_INSN (NEXT_INSN (insn)) = queue;
		    NEXT_INSN (queue) = NEXT_INSN (insn);
		    NEXT_INSN (insn) = queue;
		    PREV_INSN (queue) = insn;
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
		rtx prev = PREV_INSN (insn);
		PREV_INSN (next) = prev;
		NEXT_INSN (prev) = next;

		PREV_INSN (insn) = queue;
		queue = insn;
	      }
	  }
      }
  }
}


/* Perform machine dependent operations on the rtl chain INSNS.  */
static void
tilepro_reorg (void)
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it
     now.  */
  compute_bb_for_insn ();

  if (flag_reorder_blocks_and_partition)
    {
      tilepro_fixup_pcrel_references ();
    }

  if (flag_schedule_insns_after_reload)
    {
      split_all_insns ();

      timevar_push (TV_SCHED2);
      schedule_insns ();
      timevar_pop (TV_SCHED2);

      /* Examine the schedule to group into bundles.  */
      tilepro_gen_bundles ();
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
tilepro_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
  return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4;
}


/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  */
static void
tilepro_asm_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			     HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			     tree function)
{
  rtx this_rtx, insn, funexp;

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
  emit_insn (gen_addsi3 (this_rtx, this_rtx, GEN_INT (delta)));

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset)
    {
      rtx tmp;

      tmp = gen_rtx_REG (Pmode, 29);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));

      emit_insn (gen_addsi3 (tmp, tmp, GEN_INT (vcall_offset)));

      emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));

      emit_insn (gen_addsi3 (this_rtx, this_rtx, tmp));
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
tilepro_asm_trampoline_template (FILE *file)
{
  fprintf (file, "\tlnk   r10\n");
  fprintf (file, "\taddi  r10, r10, 32\n");
  fprintf (file, "\tlwadd r11, r10, %d\n", GET_MODE_SIZE (ptr_mode));
  fprintf (file, "\tlw    r10, r10\n");
  fprintf (file, "\tjr    r11\n");
  fprintf (file, "\t.word 0 # <function address>\n");
  fprintf (file, "\t.word 0 # <static chain value>\n");
}


/* Implement TARGET_TRAMPOLINE_INIT.  */
static void
tilepro_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
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
		     LCT_NORMAL, VOIDmode, 2, begin_addr, Pmode,
		     end_addr, Pmode);
}


/* Implement TARGET_PRINT_OPERAND.  */
static void
tilepro_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'c':
      /* Print the compare operator opcode for conditional moves. */
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
      /* Print the compare operator opcode for conditional moves. */
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

    case 'h':
      {
	/* Print the high 16 bits of a 32-bit constant.  */
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
	i = trunc_int_for_mode (i >> 16, HImode);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }

    case 'H':
      {
	rtx addr = NULL;
	const char *opstr = NULL;
	bool pcrel = false;
	if (GET_CODE (x) == CONST
	    && GET_CODE (XEXP (x, 0)) == UNSPEC)
	  {
	    addr = XVECEXP (XEXP (x, 0), 0, 0);
	    switch (XINT (XEXP (x, 0), 1))
	    {
	    case UNSPEC_GOT32_SYM:
	      opstr = "got_ha16";
	      break;
	    case UNSPEC_PCREL_SYM:
	      opstr = "ha16";
	      pcrel = true;
	      break;
	    case UNSPEC_TLS_GD:
	      opstr = "tls_gd_ha16";
	      break;
	    case UNSPEC_TLS_IE:
	      opstr = "tls_ie_ha16";
	      break;
	    case UNSPEC_TLS_LE:
	      opstr = "tls_le_ha16";
	      break;
	    default:
	      output_operand_lossage ("invalid %%H operand");
	    }
	  }
	else
	  {
	    addr = x;
	    opstr = "ha16";
	  }

	fputs (opstr, file);
	fputc ('(', file);
	output_addr_const (file, addr);

	if (pcrel)
	  {
	    rtx addr2 = XVECEXP (XEXP (x, 0), 0, 1);
	    fputs (" - " , file);
	    output_addr_const (file, addr2);
	  }

	fputc (')', file);
	return;
      }

    case 'I':
      /* Print an auto-inc memory operand.  */
      if (!MEM_P (x))
	{
	  output_operand_lossage ("invalid %%I operand");
	  return;
	}

      output_memory_reference_mode = GET_MODE (x);
      output_memory_autoinc_first = true;
      output_address (XEXP (x, 0));
      output_memory_reference_mode = VOIDmode;
      return;

    case 'i':
      /* Print an auto-inc memory operand.  */
      if (!MEM_P (x))
	{
	  output_operand_lossage ("invalid %%i operand");
	  return;
	}

      output_memory_reference_mode = GET_MODE (x);
      output_memory_autoinc_first = false;
      output_address (XEXP (x, 0));
      output_memory_reference_mode = VOIDmode;
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

    case 'L':
      {
	rtx addr = NULL;
	const char *opstr = NULL;
	bool pcrel = false;
	if (GET_CODE (x) == CONST
	    && GET_CODE (XEXP (x, 0)) == UNSPEC)
	  {
	    addr = XVECEXP (XEXP (x, 0), 0, 0);
	    switch (XINT (XEXP (x, 0), 1))
	    {
	    case UNSPEC_GOT16_SYM:
	      opstr = "got";
	      break;
	    case UNSPEC_GOT32_SYM:
	      opstr = "got_lo16";
	      break;
	    case UNSPEC_PCREL_SYM:
	      opstr = "lo16";
	      pcrel = true;
	      break;
	    case UNSPEC_TLS_GD:
	      opstr = "tls_gd_lo16";
	      break;
	    case UNSPEC_TLS_IE:
	      opstr = "tls_ie_lo16";
	      break;
	    case UNSPEC_TLS_LE:
	      opstr = "tls_le_lo16";
	      break;
	    default:
	      output_operand_lossage ("invalid %%L operand");
	    }
	  }
	else
	  {
	    addr = x;
	    opstr = "lo16";
	  }

	fputs (opstr, file);
	fputc ('(', file);
	output_addr_const (file, addr);

	if (pcrel)
	  {
	    rtx addr2 = XVECEXP (XEXP (x, 0), 0, 1);
	    fputs (" - " , file);
	    output_addr_const (file, addr2);
	  }

	fputc (')', file);
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

    case 'P':
      {
	/* Print a 32-bit constant plus one.  */
	HOST_WIDE_INT i;
	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid %%P operand");
	    return;
	  }
	i = trunc_int_for_mode (INTVAL (x) + 1, SImode);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }

    case 'M':
      {
	/* Print an mm-style bit range.  */
	int first_bit, last_bit;

	if (!CONST_INT_P (x)
	    || !tilepro_bitfield_operand_p (INTVAL (x), &first_bit,
					    &last_bit))
	  {
	    output_operand_lossage ("invalid %%M operand");
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
	  case TILEPRO_NETREG_IDN0: reg = "idn0"; break;
	  case TILEPRO_NETREG_IDN1: reg = "idn1"; break;
	  case TILEPRO_NETREG_SN:   reg = "sn";   break;
	  case TILEPRO_NETREG_UDN0: reg = "udn0"; break;
	  case TILEPRO_NETREG_UDN1: reg = "udn1"; break;
	  case TILEPRO_NETREG_UDN2: reg = "udn2"; break;
	  case TILEPRO_NETREG_UDN3: reg = "udn3"; break;
	  default: gcc_unreachable ();
	  }

	fprintf (file, reg);
	return;
      }

    case 't':
      {
	/* Log base 2 of a power of two.  */
	HOST_WIDE_INT i;
	HOST_WIDE_INT n;

	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid %%t operand");
	    return;
	  }
	n = trunc_int_for_mode (INTVAL (x), SImode);
	i = exact_log2 (n);
	if (i < 0)
	  {
	    output_operand_lossage ("invalid %%t operand '"
				    HOST_WIDE_INT_PRINT_DEC "'", n);
	    return;
	  }

	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }
      break;

    case 'r':
      /* In this case we need a register.  Use 'zero' if the
         operand is const0_rtx.  */
      if (x == const0_rtx
	  || (GET_MODE (x) != VOIDmode && x == CONST0_RTX (GET_MODE (x))))
	{
	  fputs ("zero", file);
	  return;
	}
      else if (!REG_P (x))
	{
	  output_operand_lossage ("invalid %%r operand");
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
	  output_memory_reference_mode = VOIDmode;
	  output_address (XEXP (x, 0));
	  return;
	}
      else
	{
	  output_addr_const (file, x);
	  return;
	}
      break;
    }

  debug_rtx (x);
  output_operand_lossage ("unable to print out operand yet; code == %d (%c)",
			  code, code);
}


/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */
static void
tilepro_print_operand_address (FILE *file, rtx addr)
{
  if (GET_CODE (addr) == POST_DEC
      || GET_CODE (addr) == POST_INC)
    {
      int offset = GET_MODE_SIZE (output_memory_reference_mode);

      gcc_assert (output_memory_reference_mode != VOIDmode);

      if (output_memory_autoinc_first)
	fprintf (file, "%s", reg_names[REGNO (XEXP (addr, 0))]);
      else
	fprintf (file, "%d",
		 GET_CODE (addr) == POST_DEC ? -offset : offset);
    }
  else if (GET_CODE (addr) == POST_MODIFY)
    {
      gcc_assert (output_memory_reference_mode != VOIDmode);

      gcc_assert (GET_CODE (XEXP (addr, 1)) == PLUS);

      if (output_memory_autoinc_first)
	fprintf (file, "%s", reg_names[REGNO (XEXP (addr, 0))]);
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 INTVAL (XEXP (XEXP (addr, 1), 1)));
    }
  else
    tilepro_print_operand (file, addr, 'r');
}


/* Machine mode of current insn, for determining curly brace
   placement.  */
static enum machine_mode insn_mode;


/* Implement FINAL_PRESCAN_INSN.  This is used to emit bundles.  */
void
tilepro_final_prescan_insn (rtx insn)
{
  /* Record this for tilepro_asm_output_opcode to examine.  */
  insn_mode = GET_MODE (insn);
}


/* While emitting asm, are we currently inside '{' for a bundle? */
static bool tilepro_in_bundle = false;

/* Implement ASM_OUTPUT_OPCODE.  Prepend/append curly braces as
   appropriate given the bundling information recorded by
   tilepro_gen_bundles.  */
const char *
tilepro_asm_output_opcode (FILE *stream, const char *code)
{
  bool pseudo = !strcmp (code, "pseudo");

  if (!tilepro_in_bundle && insn_mode == SImode)
    {
      /* Start a new bundle.  */
      fprintf (stream, "{\n\t");
      tilepro_in_bundle = true;
    }

  if (tilepro_in_bundle && insn_mode == QImode)
    {
      /* Close an existing bundle.  */
      static char buf[100];

      gcc_assert (strlen (code) + 3 + 1 < sizeof (buf));

      strcpy (buf, pseudo ? "" : code);
      strcat (buf, "\n\t}");
      tilepro_in_bundle = false;

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
tilepro_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  if (tilepro_in_bundle)
    {
      fprintf (file, "\t}\n");
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

  tilepro_in_bundle = false;
}


/* Implement TARGET_ASM_FILE_END.  */
static void
tilepro_file_end (void)
{
  if (NEED_INDICATE_EXEC_STACK)
    file_end_indicate_exec_stack ();
}


#undef  TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE tilepro_option_override

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P tilepro_scalar_mode_supported_p

#undef  TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P tile_vector_mode_supported_p

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM tilepro_cannot_force_const_mem

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL tilepro_function_ok_for_sibcall

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE tilepro_pass_by_reference

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY tilepro_return_in_memory

#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY tilepro_function_arg_boundary

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG tilepro_function_arg

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE tilepro_function_arg_advance

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE tilepro_function_value

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE tilepro_libcall_value

#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P tilepro_function_value_regno_p

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE \
  default_promote_function_mode_always_promote

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_false

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST tilepro_build_builtin_va_list

#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START tilepro_va_start

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS tilepro_setup_incoming_varargs

#undef  TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR tilepro_gimplify_va_arg_expr

#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS tilepro_rtx_costs

/* Limit to what we can reach in one addli.  */
#undef  TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -32768
#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 32767

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P tilepro_legitimate_constant_p

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P tilepro_legitimate_address_p

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS tilepro_legitimize_address

#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS tilepro_delegitimize_address

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  tilepro_init_builtins

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL tilepro_builtin_decl

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN tilepro_expand_builtin

#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE tilepro_conditional_register_usage

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED tilepro_frame_pointer_required

#undef  TARGET_DELAY_SCHED2
#define TARGET_DELAY_SCHED2 true

#undef  TARGET_DELAY_VARTRACK
#define TARGET_DELAY_VARTRACK true

#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE tilepro_issue_rate

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST tilepro_sched_adjust_cost

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG tilepro_reorg

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK tilepro_asm_output_mi_thunk

#undef  TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE tilepro_asm_trampoline_template

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT tilepro_trampoline_init

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND tilepro_print_operand

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS tilepro_print_operand_address

#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END tilepro_file_end


struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-tilepro.h"
