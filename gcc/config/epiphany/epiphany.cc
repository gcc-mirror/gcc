/* Subroutines used for code generation on the EPIPHANY cpu.
   Copyright (C) 1994-2024 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "output.h"
#include "insn-attr.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "tree-pass.h"	/* for current_pass */
#include "context.h"
#include "pass_manager.h"
#include "builtins.h"

/* Which cpu we're compiling for.  */
int epiphany_cpu_type;

/* Name of mangle string to add to symbols to separate code compiled for each
   cpu (or NULL).  */
const char *epiphany_mangle_cpu;

/* Array of valid operand punctuation characters.  */
char epiphany_punct_chars[256];

/* The rounding mode that we generally use for floating point.  */
int epiphany_normal_fp_rounding;

/* The pass instance, for use in epiphany_optimize_mode_switching. */
static opt_pass *pass_mode_switch_use;

static void epiphany_init_reg_tables (void);
static int get_epiphany_condition_code (rtx);
static tree epiphany_handle_interrupt_attribute (tree *, tree, tree, int, bool *);
static tree epiphany_handle_forwarder_attribute (tree *, tree, tree, int,
						 bool *);
static bool epiphany_pass_by_reference (cumulative_args_t,
					const function_arg_info &);
static rtx_insn *frame_insn (rtx);

/* defines for the initialization of the GCC target structure.  */
#define TARGET_ATTRIBUTE_TABLE epiphany_attribute_table

#define TARGET_PRINT_OPERAND epiphany_print_operand
#define TARGET_PRINT_OPERAND_ADDRESS epiphany_print_operand_address

#define TARGET_RTX_COSTS epiphany_rtx_costs
#define TARGET_ADDRESS_COST epiphany_address_cost
#define TARGET_MEMORY_MOVE_COST epiphany_memory_move_cost

#define TARGET_PROMOTE_FUNCTION_MODE epiphany_promote_function_mode
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#define TARGET_RETURN_IN_MEMORY epiphany_return_in_memory
#define TARGET_PASS_BY_REFERENCE epiphany_pass_by_reference
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_arg_info_true
#define TARGET_FUNCTION_VALUE epiphany_function_value
#define TARGET_LIBCALL_VALUE epiphany_libcall_value
#define TARGET_FUNCTION_VALUE_REGNO_P epiphany_function_value_regno_p

#define TARGET_SETUP_INCOMING_VARARGS epiphany_setup_incoming_varargs

/* Using the simplistic varags handling forces us to do partial reg/stack
   argument passing for types with larger size (> 4 bytes) than alignment.  */
#define TARGET_ARG_PARTIAL_BYTES epiphany_arg_partial_bytes

#define TARGET_FUNCTION_OK_FOR_SIBCALL epiphany_function_ok_for_sibcall

#define TARGET_SCHED_ISSUE_RATE epiphany_issue_rate
#define TARGET_SCHED_ADJUST_COST epiphany_adjust_cost

#define TARGET_LEGITIMATE_ADDRESS_P epiphany_legitimate_address_p

#define TARGET_SECONDARY_RELOAD epiphany_secondary_reload

#define TARGET_OPTION_OVERRIDE epiphany_override_options

#define TARGET_CONDITIONAL_REGISTER_USAGE epiphany_conditional_register_usage

#define TARGET_FUNCTION_ARG epiphany_function_arg

#define TARGET_FUNCTION_ARG_ADVANCE epiphany_function_arg_advance

#define TARGET_FUNCTION_ARG_BOUNDARY epiphany_function_arg_boundary

#define TARGET_TRAMPOLINE_INIT epiphany_trampoline_init

/* Nonzero if the constant rtx value is a legitimate general operand.
   We can handle any 32- or 64-bit constant.  */
#define TARGET_LEGITIMATE_CONSTANT_P hook_bool_mode_rtx_true

#define TARGET_MIN_DIVISIONS_FOR_RECIP_MUL \
  epiphany_min_divisions_for_recip_mul

#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE epiphany_preferred_simd_mode

#define TARGET_VECTOR_MODE_SUPPORTED_P epiphany_vector_mode_supported_p

#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  epiphany_vector_alignment_reachable

#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  epiphany_support_vector_misalignment

#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true
#define TARGET_ASM_OUTPUT_MI_THUNK epiphany_output_mi_thunk

/* ??? we can use larger offsets for wider-mode sized accesses, but there
   is no concept of anchors being dependent on the modes that they are used
   for, so we can only use an offset range that would suit all modes.  */
#define TARGET_MAX_ANCHOR_OFFSET (optimize_size ? 31 : 2047)
/* We further restrict the minimum to be a multiple of eight.  */
#define TARGET_MIN_ANCHOR_OFFSET (optimize_size ? 0 : -2040)

/* Mode switching hooks.  */

#define TARGET_MODE_EMIT emit_set_fp_mode

#define TARGET_MODE_NEEDED epiphany_mode_needed

#define TARGET_MODE_PRIORITY epiphany_mode_priority

#define TARGET_MODE_ENTRY epiphany_mode_entry

#define TARGET_MODE_EXIT epiphany_mode_exit

#define TARGET_MODE_AFTER epiphany_mode_after

#include "target-def.h"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK epiphany_hard_regno_mode_ok

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT epiphany_constant_alignment

#undef TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET epiphany_starting_frame_offset

bool
epiphany_is_interrupt_p (tree decl)
{
  tree attrs;

  attrs = DECL_ATTRIBUTES (decl);
  if (lookup_attribute ("interrupt", attrs))
    return true;
  else
    return false;
}

/* Called from epiphany_override_options.
   We use this to initialize various things.  */

static void
epiphany_init (void)
{
  /* N.B. this pass must not run before the first optimize_mode_switching
     pass because of the side offect of epiphany_mode_needed on
     MACHINE_FUNCTION(cfun)->unknown_mode_uses.  But it must run before
     pass_resolve_sw_modes.  */
  pass_mode_switch_use = make_pass_mode_switch_use (g);
  struct register_pass_info insert_use_info
    = { pass_mode_switch_use, "mode_sw",
	1, PASS_POS_INSERT_AFTER
      };
  opt_pass *mode_sw2
    = g->get_passes()->get_pass_mode_switching ()->clone ();
  struct register_pass_info mode_sw2_info
    = { mode_sw2, "mode_sw",
	1, PASS_POS_INSERT_AFTER
      };
  opt_pass *mode_sw3 = make_pass_resolve_sw_modes (g);
  struct register_pass_info mode_sw3_info
    = { mode_sw3, "mode_sw",
	1, PASS_POS_INSERT_AFTER
      };
  opt_pass *mode_sw4
    = g->get_passes()->get_pass_split_all_insns ()->clone ();
  struct register_pass_info mode_sw4_info
    = { mode_sw4, "mode_sw",
	1, PASS_POS_INSERT_AFTER
      };
  static const int num_modes[] = NUM_MODES_FOR_MODE_SWITCHING;
#define N_ENTITIES ARRAY_SIZE (num_modes)

  epiphany_init_reg_tables ();

  /* Initialize array for PRINT_OPERAND_PUNCT_VALID_P.  */
  memset (epiphany_punct_chars, 0, sizeof (epiphany_punct_chars));
  epiphany_punct_chars['-'] = 1;

  epiphany_normal_fp_rounding
    = (epiphany_normal_fp_mode == FP_MODE_ROUND_TRUNC
       ? FP_MODE_ROUND_TRUNC : FP_MODE_ROUND_NEAREST);
  register_pass (&mode_sw4_info);
  register_pass (&mode_sw2_info);
  register_pass (&mode_sw3_info);
  register_pass (&insert_use_info);
  register_pass (&mode_sw2_info);
  /* Verify that NUM_MODES_FOR_MODE_SWITCHING has one value per entity.  */
  gcc_assert (N_ENTITIES == EPIPHANY_MSW_ENTITY_NUM);

#if 1 /* As long as peep2_rescan is not implemented,
         (see http://gcc.gnu.org/ml/gcc-patches/2011-10/msg02819.html,)
         we need a second peephole2 pass to get reasonable code.  */
  {
    opt_pass *extra_peephole2
      = g->get_passes ()->get_pass_peephole2 ()->clone ();
    struct register_pass_info peep2_2_info
      = { extra_peephole2, "peephole2",
	  1, PASS_POS_INSERT_AFTER
	};

    register_pass (&peep2_2_info);
  }
#endif
}

/* The condition codes of the EPIPHANY, and the inverse function.  */
static const char *const epiphany_condition_codes[] =
{ /* 0    1      2      3      4      5      6     7      8      9   */
   "eq", "ne", "ltu", "gteu", "gt", "lte", "gte", "lt", "gtu", "lteu",
  /* 10   11    12     13  */
   "beq","bne","blt", "blte",
};

#define EPIPHANY_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Returns the index of the EPIPHANY condition code string in
   `epiphany_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

static int
get_epiphany_condition_code (rtx comparison)
{
  switch (GET_MODE (XEXP (comparison, 0)))
    {
    case E_CCmode:
      switch (GET_CODE (comparison))
	{
	case EQ  : return 0;
	case NE  : return 1;
	case LTU : return 2;
	case GEU : return 3;
	case GT  : return 4;
	case LE  : return 5;
	case GE  : return 6;
	case LT  : return 7;
	case GTU : return 8;
	case LEU : return 9;

	default : gcc_unreachable ();
	}
    case E_CC_N_NEmode:
      switch (GET_CODE (comparison))
	{
	case EQ: return 6;
	case NE: return 7;
	default: gcc_unreachable ();
	}
    case E_CC_C_LTUmode:
      switch (GET_CODE (comparison))
	{
	case GEU: return 2;
	case LTU: return 3;
	default: gcc_unreachable ();
	}
    case E_CC_C_GTUmode:
      switch (GET_CODE (comparison))
	{
	case LEU: return 3;
	case GTU: return 2;
	default: gcc_unreachable ();
	}
    case E_CC_FPmode:
      switch (GET_CODE (comparison))
	{
	case EQ: return 10;
	case NE: return 11;
	case LT: return 12;
	case LE: return 13;
	default: gcc_unreachable ();
	}
    case E_CC_FP_EQmode:
      switch (GET_CODE (comparison))
	{
	case EQ: return 0;
	case NE: return 1;
	default: gcc_unreachable ();
	}
    case E_CC_FP_GTEmode:
      switch (GET_CODE (comparison))
	{
	case EQ: return 0;
	case NE: return 1;
	case GT : return 4;
	case GE : return 6;
	case UNLE : return 5;
	case UNLT : return 7;
	default: gcc_unreachable ();
	}
    case E_CC_FP_ORDmode:
      switch (GET_CODE (comparison))
	{
	case ORDERED: return 9;
	case UNORDERED: return 8;
	default: gcc_unreachable ();
	}
    case E_CC_FP_UNEQmode:
      switch (GET_CODE (comparison))
	{
	case UNEQ: return 9;
	case LTGT: return 8;
	default: gcc_unreachable ();
	}
    default: gcc_unreachable ();
    }
  /*NOTREACHED*/
  return (42);
}


/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
epiphany_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return (regno & 1) == 0 && GPR_P (regno);
  else
    return true;
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

machine_mode
epiphany_select_cc_mode (enum rtx_code op,
			 rtx x ATTRIBUTE_UNUSED,
			 rtx y ATTRIBUTE_UNUSED)
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      if (TARGET_SOFT_CMPSF
	  || op == ORDERED || op == UNORDERED)
	{
	  if (op == EQ || op == NE)
	    return CC_FP_EQmode;
	  if (op == ORDERED || op == UNORDERED)
	    return CC_FP_ORDmode;
	  if (op == UNEQ || op == LTGT)
	    return CC_FP_UNEQmode;
	  return CC_FP_GTEmode;
	}
      return CC_FPmode;
    }
  /* recognize combiner pattern ashlsi_btst:
     (parallel [
	    (set (reg:N_NE 65 cc1)
		(compare:N_NE (zero_extract:SI (reg/v:SI 75 [ a ])
			(const_int 1 [0x1])
			(const_int 0 [0x0]))
		    (const_int 0 [0x0])))
	    (clobber (scratch:SI))  */
  else if ((op == EQ || op == NE)
	   && GET_CODE (x) == ZERO_EXTRACT
	   && XEXP (x, 1) == const1_rtx
	   && CONST_INT_P (XEXP (x, 2)))
    return CC_N_NEmode;
  else if ((op == GEU || op == LTU) && GET_CODE (x) == PLUS)
    return CC_C_LTUmode;
  else if ((op == LEU || op == GTU) && GET_CODE (x) == MINUS)
    return CC_C_GTUmode;
  else
    return CCmode;
}

enum reg_class epiphany_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
epiphany_init_reg_tables (void)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i == GPR_LR)
	epiphany_regno_reg_class[i] = LR_REGS;
      else if (i <= 7 && TARGET_PREFER_SHORT_INSN_REGS)
	epiphany_regno_reg_class[i] = SHORT_INSN_REGS;
      else if (call_used_or_fixed_reg_p (i)
	       && TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], i))
	epiphany_regno_reg_class[i] = SIBCALL_REGS;
      else if (i >= CORE_CONTROL_FIRST && i <= CORE_CONTROL_LAST)
	epiphany_regno_reg_class[i] = CORE_CONTROL_REGS;
      else if (i < (GPR_LAST+1)
	       || i == ARG_POINTER_REGNUM || i == FRAME_POINTER_REGNUM)
	epiphany_regno_reg_class[i] = GENERAL_REGS;
      else if (i == CC_REGNUM)
	epiphany_regno_reg_class[i] = NO_REGS /* CC_REG: must be NO_REGS */;
      else
	epiphany_regno_reg_class[i] = NO_REGS;
    }
}

/* EPIPHANY specific attribute support.

   The EPIPHANY has these attributes:
   interrupt - for interrupt functions.
   short_call - the function is assumed to be reachable with the b / bl
		instructions.
   long_call - the function address is loaded into a register before use.
   disinterrupt - functions which mask interrupts throughout.
                     They unmask them while calling an interruptible
		     function, though.  */

TARGET_GNU_ATTRIBUTES (epiphany_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "interrupt",  0, 9, true,  false, false, true,
    epiphany_handle_interrupt_attribute, NULL },
  { "forwarder_section", 1, 1, true, false, false, false,
    epiphany_handle_forwarder_attribute, NULL },
  { "long_call",  0, 0, false, true, true, false, NULL, NULL },
  { "short_call", 0, 0, false, true, true, false, NULL, NULL },
  { "disinterrupt", 0, 0, false, true, true, true, NULL, NULL }
});

/* Handle an "interrupt" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
epiphany_handle_interrupt_attribute (tree *node, tree name, tree args,
				     int flags ATTRIBUTE_UNUSED,
				     bool *no_add_attrs)
{
  tree value;

  if (!args)
    {
      gcc_assert (DECL_P (*node));
      tree t = TREE_TYPE (*node);
      if (TREE_CODE (t) != FUNCTION_TYPE)
	warning (OPT_Wattributes, "%qE attribute only applies to functions",
		 name);
      /* Argument handling and the stack layout for interrupt handlers
	 don't mix.  It makes no sense in the first place, so emit an
	 error for this.  */
      else if (TYPE_ARG_TYPES (t)
	       && TREE_VALUE (TYPE_ARG_TYPES (t)) != void_type_node)
	error_at (DECL_SOURCE_LOCATION (*node),
		  "interrupt handlers cannot have arguments");
      return NULL_TREE;
    }

  value = TREE_VALUE (args);

  if (TREE_CODE (value) != STRING_CST)
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not a string constant", name);
      *no_add_attrs = true;
    }
  else if (strcmp (TREE_STRING_POINTER (value), "reset")
	   && strcmp (TREE_STRING_POINTER (value), "software_exception")
	   && strcmp (TREE_STRING_POINTER (value), "page_miss")
	   && strcmp (TREE_STRING_POINTER (value), "timer0")
	   && strcmp (TREE_STRING_POINTER (value), "timer1")
	   && strcmp (TREE_STRING_POINTER (value), "message")
	   && strcmp (TREE_STRING_POINTER (value), "dma0")
	   && strcmp (TREE_STRING_POINTER (value), "dma1")
	   && strcmp (TREE_STRING_POINTER (value), "wand")
	   && strcmp (TREE_STRING_POINTER (value), "swi"))
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not %qs, %qs %qs, %qs, %qs, "
	       "%qs, %qs, %qs, %qs or %qs", name,
	       "reset", "software_exception", "page_miss", "timer0", "timer1",
	       "message", "dma0", "dma1", "wand", "swi");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  return epiphany_handle_interrupt_attribute (node, name, TREE_CHAIN (args),
					      flags, no_add_attrs);
}

/* Handle a "forwarder_section" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
epiphany_handle_forwarder_attribute (tree *node ATTRIBUTE_UNUSED,
				     tree name, tree args,
				     int flags ATTRIBUTE_UNUSED,
				     bool *no_add_attrs)
{
  tree value;

  value = TREE_VALUE (args);

  if (TREE_CODE (value) != STRING_CST)
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not a string constant", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}


/* Misc. utilities.  */

/* Generate a SYMBOL_REF for the special function NAME.  When the address
   can't be placed directly into a call instruction, and if possible, copy
   it to a register so that cse / code hoisting is possible.  */
rtx
sfunc_symbol (const char *name)
{
  rtx sym = gen_rtx_SYMBOL_REF (Pmode, name);

  /* These sfuncs should be hidden, and every dso should get a copy.  */
  SYMBOL_REF_FLAGS (sym) = SYMBOL_FLAG_FUNCTION | SYMBOL_FLAG_LOCAL;
  if (TARGET_SHORT_CALLS)
    ; /* Nothing to be done.  */
  else if (can_create_pseudo_p ())
    sym = copy_to_mode_reg (Pmode, sym);
  else /* We rely on reload to fix this up.  */
    gcc_assert (!reload_in_progress || reload_completed);
  return sym;
}

/* X and Y are two things to compare using CODE in IN_MODE.
   Emit the compare insn, construct the proper cc reg in the proper
   mode, and return the rtx for the cc reg comparison in CMODE.  */

rtx
gen_compare_reg (machine_mode cmode, enum rtx_code code,
		 machine_mode in_mode, rtx x, rtx y)
{
  machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg, pat, clob0, clob1, clob2;

  if (in_mode == VOIDmode)
    in_mode = GET_MODE (x);
  if (in_mode == VOIDmode)
    in_mode = GET_MODE (y);

  if (mode == CC_FPmode)
    {
      /* The epiphany has only EQ / NE / LT / LE conditions for
	 hardware floating point.  */
      if (code == GT || code == GE || code == UNLE || code == UNLT)
	{
	  rtx tmp = x; x = y; y = tmp;
	  code = swap_condition (code);
	}
      cc_reg = gen_rtx_REG (mode, CCFP_REGNUM);
      y = force_reg (in_mode, y);
    }
  else
    {
      if (mode == CC_FP_GTEmode
	  && (code == LE || code == LT || code == UNGT || code == UNGE))
	{
	  if (flag_finite_math_only
	      && ((REG_P (x) && REGNO (x) == GPR_0)
		  || (REG_P (y) && REGNO (y) == GPR_1)))
	    switch (code)
	      {
	      case LE: code = UNLE; break;
	      case LT: code = UNLT; break;
	      case UNGT: code = GT; break;
	      case UNGE: code = GE; break;
	      default: gcc_unreachable ();
	      }
	  else
	    {
	      rtx tmp = x; x = y; y = tmp;
	      code = swap_condition (code);
	    }
	}
      cc_reg = gen_rtx_REG (mode, CC_REGNUM);
    }
  if ((mode == CC_FP_EQmode || mode == CC_FP_GTEmode
       || mode == CC_FP_ORDmode || mode == CC_FP_UNEQmode)
      /* mov<mode>cc might want to re-emit a comparison during ifcvt.  */
      && (!REG_P (x) || REGNO (x) != GPR_0
	  || !REG_P (y) || REGNO (y) != GPR_1))
    {
      rtx reg;

#if 0
      /* ??? We should really do the r0/r1 clobber only during rtl expansion,
	 but just like the flag clobber of movsicc, we have to allow
	 this for ifcvt to work, on the assumption that we'll only want
	 to do this if these registers have been used before by the
	 pre-ifcvt  code.  */
      gcc_assert (currently_expanding_to_rtl);
#endif
      reg = gen_rtx_REG (in_mode, GPR_0);
      if (reg_overlap_mentioned_p (reg, y))
	return 0;
      emit_move_insn (reg, x);
      x = reg;
      reg = gen_rtx_REG (in_mode, GPR_1);
      emit_move_insn (reg, y);
      y = reg;
    }
  else
    x = force_reg (in_mode, x);

  pat = gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, x, y));
  if (mode == CC_FP_EQmode || mode == CC_FP_GTEmode)
    {
      const char *name = mode == CC_FP_EQmode ? "__eqsf2" : "__gtesf2";
      rtx use = gen_rtx_USE (VOIDmode, sfunc_symbol (name));

      clob0 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, GPR_IP));
      clob1 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, GPR_LR));
      pat = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (4, pat, use, clob0, clob1));
    }
  else if (mode == CC_FP_ORDmode || mode == CC_FP_UNEQmode)
    {
      const char *name = mode == CC_FP_ORDmode ? "__ordsf2" : "__uneqsf2";
      rtx use = gen_rtx_USE (VOIDmode, sfunc_symbol (name));

      clob0 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, GPR_IP));
      clob1 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, GPR_16));
      clob2 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, GPR_LR));
      pat = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (5, pat, use,
						   clob0, clob1, clob2));
    }
  else
    {
      clob0 = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (in_mode));
      pat = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, pat, clob0));
    }
  emit_insn (pat);
  return gen_rtx_fmt_ee (code, cmode, cc_reg, const0_rtx);
}

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
  ((MODE) == BLKmode \
   ? ROUND_ADVANCE (int_size_in_bytes (TYPE)) \
   : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) \
  (epiphany_function_arg_boundary ((MODE), (TYPE)) > BITS_PER_WORD \
   ? (((CUM) + 1) & ~1)	\
   : (CUM))

static unsigned int
epiphany_function_arg_boundary (machine_mode mode, const_tree type)
{
  if ((type ? TYPE_ALIGN (type) : GET_MODE_BITSIZE (mode)) <= PARM_BOUNDARY)
    return PARM_BOUNDARY;
  return 2 * PARM_BOUNDARY;
}

/* Do any needed setup for a variadic function.  For the EPIPHANY, we
   actually emit the code in epiphany_expand_prologue.

   CUM has not been updated for the last named argument (which is given
   by ARG), and we rely on this fact.  */


static void
epiphany_setup_incoming_varargs (cumulative_args_t cum,
				 const function_arg_info &arg,
				 int *pretend_size, int no_rtl)
{
  int first_anon_arg;
  CUMULATIVE_ARGS next_cum;
  machine_function_t *mf = MACHINE_FUNCTION (cfun);

  /* All BLKmode values are passed by reference.  */
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    gcc_assert (arg.mode != BLKmode);

  next_cum = *get_cumulative_args (cum);
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    next_cum = (ROUND_ADVANCE_CUM (next_cum, arg.mode, arg.type)
		+ ROUND_ADVANCE_ARG (arg.mode, arg.type));
  first_anon_arg = next_cum;

  if (first_anon_arg < MAX_EPIPHANY_PARM_REGS && !no_rtl)
    {
      /* Note that first_reg_offset < MAX_EPIPHANY_PARM_REGS.  */
      int first_reg_offset = first_anon_arg;

      *pretend_size = ((MAX_EPIPHANY_PARM_REGS - first_reg_offset)
		       * UNITS_PER_WORD);
    }
  mf->args_parsed = 1;
  mf->pretend_args_odd = ((*pretend_size & UNITS_PER_WORD) ? 1 : 0);
}

static int
epiphany_arg_partial_bytes (cumulative_args_t cum,
			    const function_arg_info &arg)
{
  int words = 0, rounded_cum;

  gcc_assert (!epiphany_pass_by_reference (cum, arg));

  rounded_cum = ROUND_ADVANCE_CUM (*get_cumulative_args (cum),
				   arg.mode, arg.type);
  if (rounded_cum < MAX_EPIPHANY_PARM_REGS)
    {
      words = MAX_EPIPHANY_PARM_REGS - rounded_cum;
      if (words >= ROUND_ADVANCE_ARG (arg.mode, arg.type))
	words = 0;
    }
  return words * UNITS_PER_WORD;
}

/* Cost functions.  */

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
epiphany_rtx_costs (rtx x, machine_mode mode, int outer_code,
		    int opno ATTRIBUTE_UNUSED,
		    int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  switch (code)
    {
      /* Small integers in the right context are as cheap as registers.  */
    case CONST_INT:
      if ((outer_code == PLUS || outer_code == MINUS)
	  && SIMM11 (INTVAL (x)))
	{
	  *total = 0;
	  return true;
	}
      if (IMM16 (INTVAL (x)))
	{
	  *total = outer_code == SET ? 0 : COSTS_N_INSNS (1);
	  return true;
	}
      /* FALLTHRU */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS ((epiphany_small16 (x) ? 0 : 1)
			      + (outer_code == SET ? 0 : 1));
      return true;

    case CONST_DOUBLE:
      {
	rtx high, low;
	split_double (x, &high, &low);
	*total = COSTS_N_INSNS (!IMM16 (INTVAL (high))
				+ !IMM16 (INTVAL (low)));
	return true;
      }

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      *total = COSTS_N_INSNS (1);
      return true;

    case COMPARE:
      switch (mode)
	{
	/* There are a number of single-insn combiner patterns that use
	   the flag side effects of arithmetic.  */
	case E_CC_N_NEmode:
	case E_CC_C_LTUmode:
	case E_CC_C_GTUmode:
	  return true;
	default:
	  return false;
	}


    case SET:
      {
	rtx src = SET_SRC (x);
	if (BINARY_P (src))
	  *total = 0;
	return false;
      }

    default:
      return false;
    }
}


/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */

static int
epiphany_address_cost (rtx addr, machine_mode mode,
		       addr_space_t as ATTRIBUTE_UNUSED, bool speed)
{
  rtx reg;
  rtx off = const0_rtx;
  int i;

  if (speed)
    return 0;
  /* Return 0 for addresses valid in short insns, 1 for addresses only valid
     in long insns.  */
  switch (GET_CODE (addr))
    {
    case PLUS :
      reg = XEXP (addr, 0);
      off = XEXP (addr, 1);
      break;
    case POST_MODIFY:
      reg = XEXP (addr, 0);
      off = XEXP (addr, 1);
      gcc_assert (GET_CODE (off) == PLUS && rtx_equal_p (reg, XEXP (off, 0)));
      off = XEXP (off, 1);
      if (satisfies_constraint_Rgs (reg) && satisfies_constraint_Rgs (off))
	return 0;
      return 1;
    case REG:
    default:
      reg = addr;
      break;
    }
  if (!satisfies_constraint_Rgs (reg))
    return 1;
  /* The offset range available for short instructions depends on the mode
     of the memory access.  */
  /* First, make sure we have a valid integer.  */
  if (!satisfies_constraint_L (off))
    return 1;
  i = INTVAL (off);
  switch (GET_MODE_SIZE (mode))
    {
      default:
      case 4:
	if (i & 1)
	  return 1;
	i >>= 1;
	/* Fall through.  */
      case 2:
	if (i & 1)
	  return 1;
	i >>= 1;
	/* Fall through.  */
      case 1:
	return i < -7 || i > 7;
    }
}

/* Compute the cost of moving data between registers and memory.
   For integer, load latency is twice as long as register-register moves,
   but issue pich is the same.  For floating point, load latency is three
   times as much as a reg-reg move.  */
static int
epiphany_memory_move_cost (machine_mode mode,
                          reg_class_t rclass ATTRIBUTE_UNUSED,
                          bool in ATTRIBUTE_UNUSED)
{
  return GET_MODE_CLASS (mode) == MODE_INT ? 3 : 4;
}

/* Function prologue/epilogue handlers.  */

/* EPIPHANY stack frames look like:

	     Before call                       After call
	+-----------------------+       +-----------------------+
	|                       |       |                       |
   high |  local variables,     |       |  local variables,     |
   mem  |  reg save area, etc.  |       |  reg save area, etc.  |
	|                       |       |                       |
	+-----------------------+       +-----------------------+
	|                       |       |                       |
	|  arguments on stack.  |       |  arguments on stack.  |
	|                       |       |                       |
  SP+8->+-----------------------+FP+8m->+-----------------------+
	| 2 word save area for  |       |  reg parm save area,  |
	| leaf funcs / flags    |       |  only created for     |
  SP+0->+-----------------------+       |  variable argument    |
					|  functions            |
				 FP+8n->+-----------------------+
					|                       |
					|  register save area   |
					|                       |
					+-----------------------+
					|                       |
					|  local variables      |
					|                       |
				  FP+0->+-----------------------+
					|                       |
					|  alloca allocations   |
					|                       |
					+-----------------------+
					|                       |
					|  arguments on stack   |
					|                       |
				  SP+8->+-----------------------+
   low                                  | 2 word save area for  |
   memory                               | leaf funcs / flags    |
				  SP+0->+-----------------------+

Notes:
1) The "reg parm save area" does not exist for non variable argument fns.
   The "reg parm save area" could be eliminated if we created our
   own TARGET_GIMPLIFY_VA_ARG_EXPR, but that has tradeoffs as well
   (so it's not done).  */

/* Structure to be filled in by epiphany_compute_frame_size with register
   save masks, and offsets for the current function.  */
struct epiphany_frame_info
{
  unsigned int total_size;	/* # bytes that the entire frame takes up.  */
  unsigned int pretend_size;	/* # bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # bytes needed to store regs.  */
  unsigned int var_size;	/* # bytes that variables take up.  */
  HARD_REG_SET gmask;		/* Set of saved gp registers.  */
  int          initialized;	/* Nonzero if frame size already calculated.  */
  int      stld_sz;             /* Current load/store data size for offset
				   adjustment. */
  int      need_fp;             /* value to override "frame_pointer_needed */
  /* FIRST_SLOT is the slot that is saved first, at the very start of
     the frame, with a POST_MODIFY to allocate the frame, if the size fits,
     or at least the parm and register save areas, otherwise.
     In the case of a large frame, LAST_SLOT is the slot that is saved last,
     with a POST_MODIFY to allocate the rest of the frame.  */
  int first_slot, last_slot, first_slot_offset, last_slot_offset;
  int first_slot_size;
  int small_threshold;
};

/* Current frame information calculated by epiphany_compute_frame_size.  */
static struct epiphany_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct epiphany_frame_info zero_frame_info;

/* The usual; we set up our machine_function data.  */
static struct machine_function *
epiphany_init_machine_status (void)
{
  struct machine_function *machine;

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;

  machine = ggc_cleared_alloc<machine_function_t> ();

  return machine;
}

/* Implements INIT_EXPANDERS.  We just set up to call the above
 *    function.  */
void
epiphany_init_expanders (void)
{
  init_machine_status = epiphany_init_machine_status;
}

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

static enum epiphany_function_type
epiphany_compute_function_type (tree decl)
{
  tree a;
  /* Cached value.  */
  static enum epiphany_function_type fn_type = EPIPHANY_FUNCTION_UNKNOWN;
  /* Last function we were called for.  */
  static tree last_fn = NULL_TREE;

  /* Resetting the cached value?  */
  if (decl == NULL_TREE)
    {
      fn_type = EPIPHANY_FUNCTION_UNKNOWN;
      last_fn = NULL_TREE;
      return fn_type;
    }

  if (decl == last_fn && fn_type != EPIPHANY_FUNCTION_UNKNOWN)
    return fn_type;

  /* Assume we have a normal function (not an interrupt handler).  */
  fn_type = EPIPHANY_FUNCTION_NORMAL;

  /* Now see if this is an interrupt handler.  */
  for (a = DECL_ATTRIBUTES (decl);
       a;
       a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);

      if (name == get_identifier ("interrupt"))
	fn_type = EPIPHANY_FUNCTION_INTERRUPT;
    }

  last_fn = decl;
  return fn_type;
}

#define RETURN_ADDR_REGNUM GPR_LR
#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK (1 << (RETURN_ADDR_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.  */
#define MUST_SAVE_REGISTER(regno, interrupt_p) \
  ((df_regs_ever_live_p (regno) \
    || (interrupt_p && !crtl->is_leaf \
	&& call_used_or_fixed_reg_p (regno) && !fixed_regs[regno])) \
   && (!call_used_or_fixed_reg_p (regno) || regno == GPR_LR \
       || (interrupt_p && regno != GPR_SP)))

#define MUST_SAVE_RETURN_ADDR 0

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

static unsigned int
epiphany_compute_frame_size (int size /* # of var. bytes allocated.  */)
{
  int regno;
  unsigned int total_size, var_size, args_size, pretend_size, reg_size;
  HARD_REG_SET gmask;
  enum epiphany_function_type fn_type;
  int interrupt_p;
  int first_slot, last_slot, first_slot_offset, last_slot_offset;
  int first_slot_size;
  int small_slots = 0;

  var_size	= size;
  args_size	= crtl->outgoing_args_size;
  pretend_size	= crtl->args.pretend_args_size;
  total_size	= args_size + var_size;
  reg_size	= 0;
  CLEAR_HARD_REG_SET (gmask);
  first_slot = -1;
  first_slot_offset = 0;
  last_slot = -1;
  last_slot_offset = 0;
  first_slot_size = UNITS_PER_WORD;

  /* See if this is an interrupt handler.  Call used registers must be saved
     for them too.  */
  fn_type = epiphany_compute_function_type (current_function_decl);
  interrupt_p = EPIPHANY_INTERRUPT_P (fn_type);

  /* Calculate space needed for registers.  */

  for (regno = MAX_EPIPHANY_PARM_REGS - 1; pretend_size > reg_size; regno--)
    {
      reg_size += UNITS_PER_WORD;
      SET_HARD_REG_BIT (gmask, regno);
      if (epiphany_stack_offset - reg_size == 0)
	first_slot = regno;
    }

  if (interrupt_p)
    reg_size += 2 * UNITS_PER_WORD;
  else
    small_slots = epiphany_stack_offset / UNITS_PER_WORD;

  if (frame_pointer_needed)
    {
      current_frame_info.need_fp = 1;
      if (!interrupt_p && first_slot < 0)
	first_slot = GPR_FP;
    }
  else
    current_frame_info.need_fp = 0;
  for (regno = 0; regno <= GPR_LAST; regno++)
    {
      if (MUST_SAVE_REGISTER (regno, interrupt_p))
	{
	  gcc_assert (!TEST_HARD_REG_BIT (gmask, regno));
	  reg_size += UNITS_PER_WORD;
	  SET_HARD_REG_BIT (gmask, regno);
	  /* FIXME: when optimizing for speed, take schedling into account
	     when selecting these registers.  */
	  if (regno == first_slot)
	    gcc_assert (regno == GPR_FP && frame_pointer_needed);
	  else if (!interrupt_p && first_slot < 0)
	    first_slot = regno;
	  else if (last_slot < 0
		   && (first_slot ^ regno) != 1
		   && (!interrupt_p || regno > GPR_1))
	    last_slot = regno;
	}
    }
  if (TEST_HARD_REG_BIT (gmask, GPR_LR))
    MACHINE_FUNCTION (cfun)->lr_clobbered = 1;
  /* ??? Could sometimes do better than that.  */
  current_frame_info.small_threshold
    = (optimize >= 3 || interrupt_p ? 0
       : pretend_size ? small_slots
       : 4 + small_slots - (first_slot == GPR_FP));

  /* If there might be variables with 64-bit alignment requirement, align the
     start of the variables.  */
  if (var_size >= 2 * UNITS_PER_WORD
      /* We don't want to split a double reg save/restore across two unpaired
	 stack slots when optimizing.  This rounding could be avoided with
	 more complex reordering of the register saves, but that would seem
	 to be a lot of code complexity for little gain.  */
      || (reg_size > 8 && optimize))
    reg_size = EPIPHANY_STACK_ALIGN (reg_size);
  if (((total_size + reg_size
	/* Reserve space for UNKNOWN_REGNUM.  */
	+ EPIPHANY_STACK_ALIGN (4))
       <= (unsigned) epiphany_stack_offset)
      && !interrupt_p
      && crtl->is_leaf && !frame_pointer_needed)
    {
      first_slot = -1;
      last_slot = -1;
      goto alloc_done;
    }
  else if (reg_size
	   && !interrupt_p
	   && reg_size < (unsigned HOST_WIDE_INT) epiphany_stack_offset)
    reg_size = epiphany_stack_offset;
  if (interrupt_p)
    {
      if (total_size + reg_size < 0x3fc)
	{
	  first_slot_offset = EPIPHANY_STACK_ALIGN (total_size + reg_size);
	  first_slot_offset += EPIPHANY_STACK_ALIGN (epiphany_stack_offset);
	  last_slot = -1;
	}
      else
	{
	  first_slot_offset = EPIPHANY_STACK_ALIGN (reg_size);
	  last_slot_offset = EPIPHANY_STACK_ALIGN (total_size);
	  last_slot_offset += EPIPHANY_STACK_ALIGN (epiphany_stack_offset);
	  if (last_slot >= 0)
	    CLEAR_HARD_REG_BIT (gmask, last_slot);
	}
    }
  else if (total_size + reg_size < 0x1ffc && first_slot >= 0)
    {
      first_slot_offset = EPIPHANY_STACK_ALIGN (total_size + reg_size);
      last_slot = -1;
    }
  else
    {
      if (total_size + reg_size <= (unsigned) epiphany_stack_offset)
	{
	  gcc_assert (first_slot < 0);
	  gcc_assert (reg_size == 0 || (int) reg_size == epiphany_stack_offset);
	  last_slot_offset = EPIPHANY_STACK_ALIGN (total_size + reg_size);
	}
      else
	{
	  first_slot_offset
	    = (reg_size
	       ? EPIPHANY_STACK_ALIGN (reg_size - epiphany_stack_offset) : 0);
	  if (!first_slot_offset)
	    {
	      if (first_slot != GPR_FP || !current_frame_info.need_fp)
		last_slot = first_slot;
	      first_slot = -1;
	    }
	  last_slot_offset = EPIPHANY_STACK_ALIGN (total_size);
	  if (reg_size)
	    last_slot_offset += EPIPHANY_STACK_ALIGN (epiphany_stack_offset);
	}
      if (last_slot >= 0)
	CLEAR_HARD_REG_BIT (gmask, last_slot);
    }
 alloc_done:
  if (first_slot >= 0)
    {
      CLEAR_HARD_REG_BIT (gmask, first_slot);
      if (TEST_HARD_REG_BIT (gmask, first_slot ^ 1)
	  && epiphany_stack_offset - pretend_size >= 2 * UNITS_PER_WORD)
	{
	  CLEAR_HARD_REG_BIT (gmask, first_slot ^ 1);
	  first_slot_size = 2 * UNITS_PER_WORD;
	  first_slot &= ~1;
	}
    }
  total_size = first_slot_offset + last_slot_offset;

  /* Save computed information.  */
  current_frame_info.total_size   = total_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size	  = reg_size;
  current_frame_info.gmask	  = gmask;
  current_frame_info.first_slot		= first_slot;
  current_frame_info.last_slot		= last_slot;
  current_frame_info.first_slot_offset	= first_slot_offset;
  current_frame_info.first_slot_size	= first_slot_size;
  current_frame_info.last_slot_offset	= last_slot_offset;

  current_frame_info.initialized  = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

static void
epiphany_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'd':
      fputs (epiphany_condition_codes[get_epiphany_condition_code (x)], file);
      return;
    case 'D':
     fputs (epiphany_condition_codes[EPIPHANY_INVERSE_CONDITION_CODE
				 (get_epiphany_condition_code (x))],
	     file);
      return;

    case 'X':
      current_frame_info.stld_sz = 8;
      break;

    case 'C' :
      current_frame_info.stld_sz = 4;
      break;

    case 'c' :
      current_frame_info.stld_sz = 2;
      break;

    case 'f':
     fputs (REG_P (x) ? "jalr " : "bl ", file);
     break;

    case '-':
    fprintf (file, "r%d", epiphany_m1reg);
    return;

    case 0 :
      /* Do nothing special.  */
      break;
    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  switch (GET_CODE (x))
    {
      rtx addr;
      rtx offset;

    case REG :
      fputs (reg_names[REGNO (x)], file);
      break;
    case MEM :
      if (code == 0)
	current_frame_info.stld_sz = 1;
      fputc ('[', file);
      addr = XEXP (x, 0);
      switch (GET_CODE (addr))
	{
	  case POST_INC:
	    offset = GEN_INT (GET_MODE_SIZE (GET_MODE (x)));
	    addr = XEXP (addr, 0);
	    break;
	  case POST_DEC:
	    offset = GEN_INT (-GET_MODE_SIZE (GET_MODE (x)));
	    addr = XEXP (addr, 0);
	    break;
	  case POST_MODIFY:
	    offset = XEXP (XEXP (addr, 1), 1);
	    addr = XEXP (addr, 0);
	    break;
	  default:
	    offset = 0;
	    break;
	}
      output_address (GET_MODE (x), addr);
      fputc (']', file);
      if (offset)
	{
	  fputc (',', file);
	  if (CONST_INT_P (offset)) switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	      default:
		gcc_unreachable ();
	      case 8:
		offset = GEN_INT (INTVAL (offset) >> 3);
		break;
	      case 4:
		offset = GEN_INT (INTVAL (offset) >> 2);
		break;
	      case 2:
		offset = GEN_INT (INTVAL (offset) >> 1);
		break;
	      case 1:
		break;
	    }
	  output_address (GET_MODE (x), offset);
	}
      break;
    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  long l;

	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);
	  fprintf (file, "%s0x%08lx", IMMEDIATE_PREFIX, l);
	  break;
	}
      /* FALLTHRU */
      /* Let output_addr_const deal with it.  */
    case CONST_INT:
      fprintf(file,"%s",IMMEDIATE_PREFIX);
      if (code == 'C' || code == 'X')
	{
	  fprintf (file, "%ld",
		   (long) (INTVAL (x) / current_frame_info.stld_sz));
	  break;
	}
      /* Fall through */
    default :
      output_addr_const (file, x);
      break;
    }
}

/* Print a memory address as an operand to reference that memory location.  */

static void
epiphany_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{
  rtx base, index = 0;
  int offset = 0;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;
    case SYMBOL_REF :
      if (/*???*/ 0 && SYMBOL_REF_FUNCTION_P (addr))
	{
	  output_addr_const (file, addr);
	}
      else
	{
	  output_addr_const (file, addr);
	}
      break;
    case PLUS :
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);
      gcc_assert (GET_CODE (base) == REG);
      fputs (reg_names[REGNO (base)], file);
      if (index == 0)
	{
	  /*
	  ** ++rk quirky method to scale offset for ld/str.......
	  */
	  fprintf (file, ",%s%d", IMMEDIATE_PREFIX,
		   offset/current_frame_info.stld_sz);
	}
      else
	{
	  switch (GET_CODE (index))
	    {
	    case REG:
	      fprintf (file, ",%s", reg_names[REGNO (index)]);
	      break;
	    case SYMBOL_REF:
	      fputc (',', file), output_addr_const (file, index);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
      break;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC: case POST_MODIFY:
      /* We shouldn't get here as we've lost the mode of the memory object
	 (which says how much to inc/dec by.
	 FIXME: We have the mode now, address printing can be moved into this
	 function.  */
      gcc_unreachable ();
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}

void
epiphany_final_prescan_insn (rtx_insn *insn ATTRIBUTE_UNUSED,
			     rtx *opvec ATTRIBUTE_UNUSED,
			     int noperands ATTRIBUTE_UNUSED)
{
  int i = epiphany_n_nops;
  rtx pat ATTRIBUTE_UNUSED;

  while (i--)
    fputs ("\tnop\n", asm_out_file);
}


/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
epiphany_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);

  if (AGGREGATE_TYPE_P (type)
      && (TYPE_MODE (type) == BLKmode || TYPE_NEEDS_CONSTRUCTING (type)))
    return true;
  return (size == -1 || size > 8);
}

/* For EPIPHANY, All aggregates and arguments greater than 8 bytes are
   passed by reference.  */

static bool
epiphany_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  if (tree type = arg.type)
    {
      if (AGGREGATE_TYPE_P (type)
	  && (arg.mode == BLKmode || TYPE_NEEDS_CONSTRUCTING (type)))
	return true;
    }
  return false;
}


static rtx
epiphany_function_value (const_tree ret_type,
			 const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
			 bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;

  mode = TYPE_MODE (ret_type);
  /* We must change the mode like PROMOTE_MODE does.
     ??? PROMOTE_MODE is ignored for non-scalar types.
     The set of types tested here has to be kept in sync
     with the one in explow.cc:promote_mode.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < 4
      && (TREE_CODE (ret_type) == INTEGER_TYPE
          || TREE_CODE (ret_type) == ENUMERAL_TYPE
          || TREE_CODE (ret_type) == BOOLEAN_TYPE
          || TREE_CODE (ret_type) == OFFSET_TYPE))
    mode = SImode;
  return gen_rtx_REG (mode, 0);
}

static rtx
epiphany_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 0);
}

static bool
epiphany_function_value_regno_p (const unsigned int regno ATTRIBUTE_UNUSED)
{
  return regno == 0;
}

/* Fix up invalid option settings.  */
static void
epiphany_override_options (void)
{
  if (epiphany_stack_offset < 4)
    error ("%<stack_offset%> must be at least 4");
  if (epiphany_stack_offset & 3)
    error ("%<stack_offset%> must be a multiple of 4");
  epiphany_stack_offset = (epiphany_stack_offset + 3) & -4;
 if (!TARGET_SOFT_CMPSF)
   flag_finite_math_only = 1;

  /* This needs to be done at start up.  It's convenient to do it here.  */
  epiphany_init ();
}

/* For a DImode load / store SET, make a SImode set for a
   REG_FRAME_RELATED_EXPR note, using OFFSET to create a high or lowpart
   subreg.  */
static rtx
frame_subreg_note (rtx set, int offset)
{
  rtx src = simplify_gen_subreg (SImode, SET_SRC (set), DImode, offset);
  rtx dst = simplify_gen_subreg (SImode, SET_DEST (set), DImode, offset);

  set = gen_rtx_SET (dst ,src);
  RTX_FRAME_RELATED_P (set) = 1;
  return set;
}

static rtx_insn *
frame_insn (rtx x)
{
  int i;
  rtx note = NULL_RTX;
  rtx_insn *insn;

  if (GET_CODE (x) == PARALLEL)
    {
      rtx part = XVECEXP (x, 0, 0);

      if (GET_MODE (SET_DEST (part)) == DImode)
	{
	  note = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (XVECLEN (x, 0) + 1));
	  XVECEXP (note, 0, 0) = frame_subreg_note (part, 0);
	  XVECEXP (note, 0, 1) = frame_subreg_note (part, UNITS_PER_WORD);
	  for (i = XVECLEN (x, 0) - 1; i >= 1; i--)
	    {
	      part = copy_rtx (XVECEXP (x, 0, i));

	      if (GET_CODE (part) == SET)
		RTX_FRAME_RELATED_P (part) = 1;
	      XVECEXP (note, 0, i + 1) = part;
	    }
	}
      else
	{
	  for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	    {
	      part = XVECEXP (x, 0, i);

	      if (GET_CODE (part) == SET)
		RTX_FRAME_RELATED_P (part) = 1;
	    }
	}
    }
  else if (GET_CODE (x) == SET && GET_MODE (SET_DEST (x)) == DImode)
    note = gen_rtx_PARALLEL (VOIDmode,
			     gen_rtvec (2, frame_subreg_note (x, 0),
					frame_subreg_note (x, UNITS_PER_WORD)));
  insn = emit_insn (x);
  RTX_FRAME_RELATED_P (insn) = 1;
  if (note)
    add_reg_note (insn, REG_FRAME_RELATED_EXPR, note);
  return insn;
}

static rtx_insn *
frame_move_insn (rtx to, rtx from)
{
  return frame_insn (gen_rtx_SET (to, from));
}

/* Generate a MEM referring to a varargs argument slot.  */

static rtx
gen_varargs_mem (machine_mode mode, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr);
  MEM_NOTRAP_P (mem) = 1;
  set_mem_alias_set (mem, get_varargs_alias_set ());
  return mem;
}

/* Emit instructions to save or restore registers in the range [MIN..LIMIT) .
   If EPILOGUE_P is 0, save; if it is one, restore.
   ADDR is the stack slot to save the first register to; subsequent
   registers are written to lower addresses.
   However, the order of register pairs can be reversed in order to
   use double-word load-store instructions.  Likewise, an unpaired single
   word save slot can be skipped while double saves are carried out, and
   reused when a single register is to be saved.  */

static void
epiphany_emit_save_restore (int min, int limit, rtx addr, int epilogue_p)
{
  int i;
  int stack_offset
    = current_frame_info.first_slot >= 0 ? epiphany_stack_offset : 0;
  rtx skipped_mem = NULL_RTX;
  int last_saved = limit - 1;

  if (!optimize)
    while (last_saved >= 0
	   && !TEST_HARD_REG_BIT (current_frame_info.gmask, last_saved))
      last_saved--;
  for (i = 0; i < limit; i++)
    {
      machine_mode mode = word_mode;
      rtx mem, reg;
      int n = i;
      rtx (*gen_mem) (machine_mode, rtx) = gen_frame_mem;

      /* Make sure we push the arguments in the right order.  */
      if (n < MAX_EPIPHANY_PARM_REGS && crtl->args.pretend_args_size)
	{
	  n = MAX_EPIPHANY_PARM_REGS - 1 - n;
	  gen_mem = gen_varargs_mem;
	}
      if (stack_offset == current_frame_info.first_slot_size
	  && current_frame_info.first_slot >= 0)
	{
	  if (current_frame_info.first_slot_size > UNITS_PER_WORD)
	    {
	      mode = DImode;
	      addr = plus_constant (Pmode, addr,
				    - (HOST_WIDE_INT) UNITS_PER_WORD);
	    }
	  if (i-- < min || !epilogue_p)
	    goto next_slot;
	  n = current_frame_info.first_slot;
	  gen_mem = gen_frame_mem;
	}
      else if (n == UNKNOWN_REGNUM
	       && stack_offset > current_frame_info.first_slot_size)
	{
	  i--;
	  goto next_slot;
	}
      else if (!TEST_HARD_REG_BIT (current_frame_info.gmask, n))
	continue;
      else if (i < min)
	goto next_slot;

      /* Check for a register pair to save.  */
      if (n == i
	  && (n >= MAX_EPIPHANY_PARM_REGS || crtl->args.pretend_args_size == 0)
	  && (n & 1) == 0 && n+1 < limit
	  && TEST_HARD_REG_BIT (current_frame_info.gmask, n+1))
	{
	  /* If it fits in the current stack slot pair, place it there.  */
	  if (GET_CODE (addr) == PLUS && (stack_offset & 7) == 0
	      && stack_offset != 2 * UNITS_PER_WORD
	      && (current_frame_info.last_slot < 0
		  || INTVAL (XEXP (addr, 1)) != UNITS_PER_WORD)
	      && (n+1 != last_saved || !skipped_mem))
	    {
	      mode = DImode;
	      i++;
	      addr = plus_constant (Pmode, addr,
				    - (HOST_WIDE_INT) UNITS_PER_WORD);
	    }
	  /* If it fits in the following stack slot pair, that's fine, too.  */
	  else if (GET_CODE (addr) == PLUS && (stack_offset & 7) == 4
		   && stack_offset != 2 * UNITS_PER_WORD
		   && stack_offset != 3 * UNITS_PER_WORD
		   && (current_frame_info.last_slot < 0
		       || INTVAL (XEXP (addr, 1)) != 2 * UNITS_PER_WORD)
		   && n + 1 != last_saved)
	    {
	      gcc_assert (!skipped_mem);
	      stack_offset -= GET_MODE_SIZE (mode);
	      skipped_mem = gen_mem (mode, addr);
	      mode = DImode;
	      i++;
	      addr = plus_constant (Pmode, addr,
				    - (HOST_WIDE_INT) 2 * UNITS_PER_WORD);
	    }
	}
      reg = gen_rtx_REG (mode, n);
      if (mode != DImode && skipped_mem)
	mem = skipped_mem;
      else
	mem = gen_mem (mode, addr);

      /* If we are loading / storing LR, note the offset that
	 gen_reload_insi_ra requires.  Since GPR_LR is even,
	 we only need to test n, even if mode is DImode.  */
      gcc_assert ((GPR_LR & 1) == 0);
      if (n == GPR_LR)
	{
	  long lr_slot_offset = 0;
	  rtx m_addr = XEXP (mem, 0);

	  if (GET_CODE (m_addr) == PLUS)
	    lr_slot_offset = INTVAL (XEXP (m_addr, 1));
	  if (frame_pointer_needed)
	    lr_slot_offset += (current_frame_info.first_slot_offset
			       - current_frame_info.total_size);
	  if (MACHINE_FUNCTION (cfun)->lr_slot_known)
	    gcc_assert (MACHINE_FUNCTION (cfun)->lr_slot_offset
			== lr_slot_offset);
	  MACHINE_FUNCTION (cfun)->lr_slot_offset = lr_slot_offset;
	  MACHINE_FUNCTION (cfun)->lr_slot_known = 1;
	}

      if (!epilogue_p)
	frame_move_insn (mem, reg);
      else if (n >= MAX_EPIPHANY_PARM_REGS || !crtl->args.pretend_args_size)
	emit_move_insn (reg, mem);
      if (mem == skipped_mem)
	{
	  skipped_mem = NULL_RTX;
	  continue;
	}
    next_slot:
      addr = plus_constant (Pmode, addr, -(HOST_WIDE_INT) UNITS_PER_WORD);
      stack_offset -= GET_MODE_SIZE (mode);
    }
}

void
epiphany_expand_prologue (void)
{
  int interrupt_p;
  enum epiphany_function_type fn_type;
  rtx addr, mem, off, reg;

  if (!current_frame_info.initialized)
    epiphany_compute_frame_size (get_frame_size ());

  /* It is debatable if we should adjust this by epiphany_stack_offset.  */
  if (flag_stack_usage_info)
    current_function_static_stack_size = current_frame_info.total_size;

  fn_type = epiphany_compute_function_type (current_function_decl);
  interrupt_p = EPIPHANY_INTERRUPT_P (fn_type);

  if (interrupt_p)
    {
      addr = plus_constant (Pmode, stack_pointer_rtx,
			    - (HOST_WIDE_INT) 2 * UNITS_PER_WORD);
      if (!lookup_attribute ("forwarder_section",
			    DECL_ATTRIBUTES (current_function_decl))
	  || !epiphany_is_long_call_p (XEXP (DECL_RTL (current_function_decl),
					     0)))
        frame_move_insn (gen_frame_mem (DImode, addr),
			 gen_rtx_REG (DImode, GPR_0));
      frame_move_insn (gen_rtx_REG (SImode, GPR_0),
		       gen_rtx_REG (word_mode, STATUS_REGNUM));
      frame_move_insn (gen_rtx_REG (SImode, GPR_1),
		       gen_rtx_REG (word_mode, IRET_REGNUM));
      mem = gen_frame_mem (BLKmode, stack_pointer_rtx);
      off = GEN_INT (-current_frame_info.first_slot_offset);
      frame_insn (gen_stack_adjust_add (off, mem));
      if (!epiphany_uninterruptible_p (current_function_decl))
	emit_insn (gen_gie ());
      addr = plus_constant (Pmode, stack_pointer_rtx,
			    current_frame_info.first_slot_offset
			    - (HOST_WIDE_INT) 3 * UNITS_PER_WORD);
    }
  else
    {
      addr = plus_constant (Pmode, stack_pointer_rtx,
			    epiphany_stack_offset
			    - (HOST_WIDE_INT) UNITS_PER_WORD);
      epiphany_emit_save_restore (0, current_frame_info.small_threshold,
				  addr, 0);
      /* Allocate register save area; for small to medium size frames,
	 allocate the entire frame; this is joint with one register save.  */
      if (current_frame_info.first_slot >= 0)
	{
	  machine_mode mode
	= (current_frame_info.first_slot_size == UNITS_PER_WORD
	   ? word_mode : DImode);

	  off = GEN_INT (-current_frame_info.first_slot_offset);
	  mem = gen_frame_mem (BLKmode,
			       gen_rtx_PLUS (Pmode, stack_pointer_rtx, off));
	  frame_insn (gen_stack_adjust_str
		       (gen_frame_mem (mode, stack_pointer_rtx),
			gen_rtx_REG (mode, current_frame_info.first_slot),
			off, mem));
	  addr = plus_constant (Pmode, addr,
				current_frame_info.first_slot_offset);
	}
    }
  epiphany_emit_save_restore (current_frame_info.small_threshold,
			      FIRST_PSEUDO_REGISTER, addr, 0);
  if (current_frame_info.need_fp)
    frame_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
  /* For large frames, allocate bulk of frame.  This is usually joint with one
     register save.  */
  if (current_frame_info.last_slot >= 0)
    {
      rtx ip, mem2, note;
      rtx_insn *insn;

      gcc_assert (current_frame_info.last_slot != GPR_FP
		  || (!current_frame_info.need_fp
		      && current_frame_info.first_slot < 0));
      off = GEN_INT (-current_frame_info.last_slot_offset);
      mem = gen_frame_mem (BLKmode,
			   gen_rtx_PLUS (Pmode, stack_pointer_rtx, off));
      ip = gen_rtx_REG (Pmode, GPR_IP);
      frame_move_insn (ip, off);
      reg = gen_rtx_REG (word_mode, current_frame_info.last_slot),
      mem2 = gen_frame_mem (word_mode, stack_pointer_rtx),
      insn = frame_insn (gen_stack_adjust_str (mem2, reg, ip, mem));
      /* Instruction scheduling can separate the instruction setting IP from
	 INSN so that dwarf2out_frame_debug_expr becomes confused what the
	 temporary register is.  Example: _gcov.o  */
      note = gen_rtx_SET (stack_pointer_rtx,
			  gen_rtx_PLUS (Pmode, stack_pointer_rtx, off));
      note = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (2, gen_rtx_SET (mem2, reg), note));
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, note);
    }
  /* If there is only one or no register to save, yet we have a large frame,
     use an add.  */
  else if (current_frame_info.last_slot_offset)
    {
      mem = gen_frame_mem (BLKmode,
			   plus_constant (Pmode, stack_pointer_rtx,
					  current_frame_info.last_slot_offset));
      off = GEN_INT (-current_frame_info.last_slot_offset);
      if (!SIMM11 (INTVAL (off)))
	{
	  reg = gen_rtx_REG (Pmode, GPR_IP);
	  frame_move_insn (reg, off);
	  off = reg;
	}
      frame_insn (gen_stack_adjust_add (off, mem));
    }
}

void
epiphany_expand_epilogue (int sibcall_p)
{
  int interrupt_p;
  enum epiphany_function_type fn_type;
  rtx mem, addr, reg, off;
  HOST_WIDE_INT restore_offset;

  fn_type = epiphany_compute_function_type( current_function_decl);
  interrupt_p = EPIPHANY_INTERRUPT_P (fn_type);

  /* For variable frames, deallocate bulk of frame.  */
  if (current_frame_info.need_fp)
    {
      mem = gen_frame_mem (BLKmode, stack_pointer_rtx);
      emit_insn (gen_stack_adjust_mov (mem));
    }
  /* Else for large static frames, deallocate bulk of frame.  */
  else if (current_frame_info.last_slot_offset)
    {
      mem = gen_frame_mem (BLKmode, stack_pointer_rtx);
      reg = gen_rtx_REG (Pmode, GPR_IP);
      emit_move_insn (reg, GEN_INT (current_frame_info.last_slot_offset));
      emit_insn (gen_stack_adjust_add (reg, mem));
    }
  restore_offset = (interrupt_p
		    ? - 3 * UNITS_PER_WORD
		    : epiphany_stack_offset - (HOST_WIDE_INT) UNITS_PER_WORD);
  addr = plus_constant (Pmode, stack_pointer_rtx,
			(current_frame_info.first_slot_offset
			 + restore_offset));
  epiphany_emit_save_restore (current_frame_info.small_threshold,
			   FIRST_PSEUDO_REGISTER, addr, 1);

  if (interrupt_p && !epiphany_uninterruptible_p (current_function_decl))
    emit_insn (gen_gid ());

  off = GEN_INT (current_frame_info.first_slot_offset);
  mem = gen_frame_mem (BLKmode, stack_pointer_rtx);
  /* For large / variable size frames, deallocating the register save area is
     joint with one register restore; for medium size frames, we use a
     dummy post-increment load to dealloacte the whole frame.  */
  if (!SIMM11 (INTVAL (off)) || current_frame_info.last_slot >= 0)
    {
      emit_insn (gen_stack_adjust_ldr
		  (gen_rtx_REG (word_mode,
				(current_frame_info.last_slot >= 0
				 ? current_frame_info.last_slot : GPR_IP)),
		   gen_frame_mem (word_mode, stack_pointer_rtx),
		   off,
		   mem));
    }
  /* While for small frames, we deallocate the entire frame with one add.  */
  else if (INTVAL (off))
    {
      emit_insn (gen_stack_adjust_add (off, mem));
    }
  if (interrupt_p)
    {
      emit_move_insn (gen_rtx_REG (word_mode, STATUS_REGNUM),
		      gen_rtx_REG (SImode, GPR_0));
      emit_move_insn (gen_rtx_REG (word_mode, IRET_REGNUM),
		      gen_rtx_REG (SImode, GPR_1));
      addr = plus_constant (Pmode, stack_pointer_rtx,
			    - (HOST_WIDE_INT) 2 * UNITS_PER_WORD);
      emit_move_insn (gen_rtx_REG (DImode, GPR_0),
		      gen_frame_mem (DImode, addr));
    }
  addr = plus_constant (Pmode, stack_pointer_rtx,
			epiphany_stack_offset - (HOST_WIDE_INT) UNITS_PER_WORD);
  epiphany_emit_save_restore (0, current_frame_info.small_threshold, addr, 1);
  if (!sibcall_p)
    {
      if (interrupt_p)
	emit_jump_insn (gen_return_internal_interrupt());
      else
	emit_jump_insn (gen_return_i ());
    }
}

int
epiphany_initial_elimination_offset (int from, int to)
{
  epiphany_compute_frame_size (get_frame_size ());
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return current_frame_info.total_size - current_frame_info.reg_size;
  if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return current_frame_info.first_slot_offset - current_frame_info.reg_size;
  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return (current_frame_info.total_size
	    - ((current_frame_info.pretend_size + 4) & -8));
  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return (current_frame_info.first_slot_offset
	    - ((current_frame_info.pretend_size + 4) & -8));
  gcc_unreachable ();
}

bool
epiphany_regno_rename_ok (unsigned, unsigned dst)
{
  enum epiphany_function_type fn_type;

  fn_type = epiphany_compute_function_type (current_function_decl);
  if (!EPIPHANY_INTERRUPT_P (fn_type))
    return true;
  if (df_regs_ever_live_p (dst))
    return true;
  return false;
}

static int
epiphany_issue_rate (void)
{
  return 2;
}

/* Function to update the integer COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.  The return value should be
   the new value for COST.  */
static int
epiphany_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn,
		      int cost, unsigned int)
{
  if (dep_type == 0)
    {
      rtx dep_set;

      if (recog_memoized (insn) < 0
	  || recog_memoized (dep_insn) < 0)
	return cost;

      dep_set = single_set (dep_insn);

      /* The latency that we specify in the scheduling description refers
	 to the actual output, not to an auto-increment register; for that,
	 the latency is one.  */
      if (dep_set && MEM_P (SET_SRC (dep_set)) && cost > 1)
	{
	  rtx set = single_set (insn);

	  if (set
	      && !reg_overlap_mentioned_p (SET_DEST (dep_set), SET_SRC (set))
	      && (!MEM_P (SET_DEST (set))
		  || !reg_overlap_mentioned_p (SET_DEST (dep_set),
					       XEXP (SET_DEST (set), 0))))
	    cost = 1;
	}
    }
  return cost;
}

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#define RTX_OK_FOR_BASE_P(X) \
  (REG_P (X) && REG_OK_FOR_BASE_P (X))

#define RTX_OK_FOR_INDEX_P(MODE, X) \
  ((GET_MODE_CLASS (MODE) != MODE_VECTOR_INT \
    || epiphany_vect_align >= GET_MODE_SIZE (MODE)) \
   && (REG_P (X) && REG_OK_FOR_INDEX_P (X)))

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE, X) \
(GET_CODE (X) == PLUS \
 && RTX_OK_FOR_BASE_P (XEXP (X, 0)) \
 && (RTX_OK_FOR_INDEX_P (MODE, XEXP (X, 1)) \
     || RTX_OK_FOR_OFFSET_P (MODE, XEXP (X, 1))))

static bool
epiphany_legitimate_address_p (machine_mode mode, rtx x, bool strict,
			       code_helper = ERROR_MARK)
{
#define REG_OK_FOR_BASE_P(X) \
  (strict ? GPR_P (REGNO (X)) : GPR_AP_OR_PSEUDO_P (REGNO (X)))
  if (RTX_OK_FOR_BASE_P (x))
    return true;
  if (RTX_FRAME_OFFSET_P (x))
    return true;
  if (LEGITIMATE_OFFSET_ADDRESS_P (mode, x))
    return true;
  /* If this is a misaligned stack access, don't force it to reg+index.  */
  if (GET_MODE_SIZE (mode) == 8
      && GET_CODE (x) == PLUS && XEXP (x, 0) == stack_pointer_rtx
      /* Decomposed to SImode; GET_MODE_SIZE (SImode) == 4 */
      && !(INTVAL (XEXP (x, 1)) & 3)
      && INTVAL (XEXP (x, 1)) >= -2047 * 4
      && INTVAL (XEXP (x, 1)) <=  2046 * 4)
    return true;
  if (TARGET_POST_INC
      && (GET_CODE (x) == POST_DEC || GET_CODE (x) == POST_INC)
      && RTX_OK_FOR_BASE_P (XEXP ((x), 0)))
    return true;
  if ((TARGET_POST_MODIFY || reload_completed)
      && GET_CODE (x) == POST_MODIFY
      && GET_CODE (XEXP ((x), 1)) == PLUS
      && rtx_equal_p (XEXP ((x), 0), XEXP (XEXP ((x), 1), 0))
      && LEGITIMATE_OFFSET_ADDRESS_P (mode, XEXP ((x), 1)))
    return true;
  if (mode == BLKmode)
    return epiphany_legitimate_address_p (SImode, x, strict);
  return false;
}

static reg_class_t
epiphany_secondary_reload (bool in_p, rtx x, reg_class_t rclass,
			machine_mode mode ATTRIBUTE_UNUSED,
			secondary_reload_info *sri)
{
  /* This could give more reload inheritance, but we are missing some
     reload infrastructure.  */
 if (0)
  if (in_p && GET_CODE (x) == UNSPEC
      && satisfies_constraint_Sra (x) && !satisfies_constraint_Rra (x))
    {
      gcc_assert (rclass == GENERAL_REGS);
      sri->icode = CODE_FOR_reload_insi_ra;
      return NO_REGS;
    }
  return NO_REGS;
}

bool
epiphany_is_long_call_p (rtx x)
{
  tree decl = SYMBOL_REF_DECL (x);
  bool ret_val = !TARGET_SHORT_CALLS;
  tree attrs;

  /* ??? Is it safe to default to ret_val if decl is NULL?  We should
     probably encode information via encode_section_info, and also
     have (an) option(s) to take SYMBOL_FLAG_LOCAL and/or SYMBOL_FLAG_EXTERNAL
     into account.  */
  if (decl)
    {
      attrs = TYPE_ATTRIBUTES (TREE_TYPE (decl));
      if (lookup_attribute ("long_call", attrs))
	ret_val = true;
      else if (lookup_attribute ("short_call", attrs))
	ret_val = false;
    }
  return ret_val;
}

bool
epiphany_small16 (rtx x)
{
  rtx base = x;
  rtx offs ATTRIBUTE_UNUSED = const0_rtx;

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
    {
      base = XEXP (XEXP (x, 0), 0);
      offs = XEXP (XEXP (x, 0), 1);
    }
  if (GET_CODE (base) == SYMBOL_REF && SYMBOL_REF_FUNCTION_P (base)
      && epiphany_is_long_call_p (base))
    return false;
  return TARGET_SMALL16 != 0;
}

/* Return nonzero if it is ok to make a tail-call to DECL.  */
static bool
epiphany_function_ok_for_sibcall (tree decl, tree exp)
{
  bool cfun_interrupt_p, call_interrupt_p;

  cfun_interrupt_p = EPIPHANY_INTERRUPT_P (epiphany_compute_function_type
					(current_function_decl));
  if (decl)
    call_interrupt_p = EPIPHANY_INTERRUPT_P (epiphany_compute_function_type (decl));
  else
    {
      tree fn_type = TREE_TYPE (CALL_EXPR_FN (exp));

      gcc_assert (POINTER_TYPE_P (fn_type));
      fn_type = TREE_TYPE (fn_type);
      gcc_assert (FUNC_OR_METHOD_TYPE_P (fn_type));
      call_interrupt_p
	= lookup_attribute ("interrupt", TYPE_ATTRIBUTES (fn_type)) != NULL;
    }

  /* Don't tailcall from or to an ISR routine - although we could in
     principle tailcall from one ISR routine to another, we'd need to
     handle this in sibcall_epilogue to make it work.  */
  if (cfun_interrupt_p || call_interrupt_p)
    return false;

  /* Everything else is ok.  */
  return true;
}

/* T is a function declaration or the MEM_EXPR of a MEM passed to a call
   expander.
   Return true iff the type of T has the uninterruptible attribute.
   If T is NULL, return false.  */
bool
epiphany_uninterruptible_p (tree t)
{
  tree attrs;

  if (t)
    {
      attrs = TYPE_ATTRIBUTES (TREE_TYPE (t));
      if (lookup_attribute ("disinterrupt", attrs))
	return true;
    }
  return false;
}

bool
epiphany_call_uninterruptible_p (rtx mem)
{
  rtx addr = XEXP (mem, 0);
  tree t = NULL_TREE;

  if (GET_CODE (addr) == SYMBOL_REF)
    t = SYMBOL_REF_DECL (addr);
  if (!t)
    t = MEM_EXPR (mem);
  return epiphany_uninterruptible_p (t);
}

static machine_mode
epiphany_promote_function_mode (const_tree type, machine_mode mode,
				int *punsignedp ATTRIBUTE_UNUSED,
				const_tree funtype ATTRIBUTE_UNUSED,
				int for_return ATTRIBUTE_UNUSED)
{
  int dummy;

  return promote_mode (type, mode, &dummy);
}

static void
epiphany_conditional_register_usage (void)
{
  int i;

  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
  if (TARGET_HALF_REG_FILE)
    {
      for (i = 32; i <= 63; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}
    }
  if (epiphany_m1reg >= 0)
    {
      fixed_regs[epiphany_m1reg] = 1;
      call_used_regs[epiphany_m1reg] = 1;
    }
  if (!TARGET_PREFER_SHORT_INSN_REGS)
    CLEAR_HARD_REG_SET (reg_class_contents[SHORT_INSN_REGS]);
  reg_class_contents[SIBCALL_REGS] = reg_class_contents[GENERAL_REGS];
  /* It would be simpler and quicker if we could just use
     &~, alas, call_used_or_fixed_regs is yet uninitialized;
     it is set up later by our caller.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (!call_used_regs[i])
      CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], i);
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   ARG is a description of the argument.  */
/* On the EPIPHANY the first MAX_EPIPHANY_PARM_REGS args are normally in
   registers and the rest are pushed.  */
static rtx
epiphany_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS cum = *get_cumulative_args (cum_v);

  if (PASS_IN_REG_P (cum, arg.mode, arg.type))
    return gen_rtx_REG (arg.mode, ROUND_ADVANCE_CUM (cum, arg.mode, arg.type));
  return 0;
}

/* Update the data in CUM to advance over argument ARG.  */
static void
epiphany_function_arg_advance (cumulative_args_t cum_v,
			       const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum = (ROUND_ADVANCE_CUM (*cum, arg.mode, arg.type)
	  + ROUND_ADVANCE_ARG (arg.mode, arg.type));
}

/* Nested function support.
   An epiphany trampoline looks like this:
   mov r16,%low(fnaddr)
   movt r16,%high(fnaddr)
   mov ip,%low(cxt)
   movt ip,%high(cxt)
   jr r16  */

#define EPIPHANY_LOW_RTX(X) \
  (gen_rtx_IOR (SImode, \
    gen_rtx_ASHIFT (SImode, \
		    gen_rtx_AND (SImode, (X), GEN_INT (0xff)), GEN_INT (5)), \
    gen_rtx_ASHIFT (SImode, \
		    gen_rtx_AND (SImode, (X), GEN_INT (0xff00)), GEN_INT (12))))
#define EPIPHANY_HIGH_RTX(X) \
  EPIPHANY_LOW_RTX (gen_rtx_LSHIFTRT (SImode, (X), GEN_INT (16)))

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
static void
epiphany_trampoline_init (rtx tramp_mem, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx tramp = force_reg (Pmode, XEXP (tramp_mem, 0));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, tramp, 0)),
		  gen_rtx_IOR (SImode, GEN_INT (0x4002000b),
			       EPIPHANY_LOW_RTX (fnaddr)));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, tramp, 4)),
		  gen_rtx_IOR (SImode, GEN_INT (0x5002000b),
			       EPIPHANY_HIGH_RTX (fnaddr)));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, tramp, 8)),
		  gen_rtx_IOR (SImode, GEN_INT (0x2002800b),
			       EPIPHANY_LOW_RTX (cxt)));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, tramp, 12)),
		  gen_rtx_IOR (SImode, GEN_INT (0x3002800b),
			       EPIPHANY_HIGH_RTX (cxt)));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (Pmode, tramp, 16)),
		  GEN_INT (0x0802014f));
}

bool
epiphany_optimize_mode_switching (int entity)
{
  if (MACHINE_FUNCTION (cfun)->sw_entities_processed & (1 << entity))
    return false;
  switch (entity)
    {
    case EPIPHANY_MSW_ENTITY_AND:
    case EPIPHANY_MSW_ENTITY_OR:
    case EPIPHANY_MSW_ENTITY_CONFIG:
      return true;
    case EPIPHANY_MSW_ENTITY_NEAREST:
    case EPIPHANY_MSW_ENTITY_TRUNC:
      return optimize > 0;
    case EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN:
      return MACHINE_FUNCTION (cfun)->unknown_mode_uses != 0;
    case EPIPHANY_MSW_ENTITY_ROUND_KNOWN:
      return (MACHINE_FUNCTION (cfun)->sw_entities_processed
	      & (1 << EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN)) != 0;
    case EPIPHANY_MSW_ENTITY_FPU_OMNIBUS:
      return optimize == 0 || current_pass == pass_mode_switch_use;
    }
  gcc_unreachable ();
}

static int
epiphany_mode_priority (int entity, int priority)
{
  if (entity == EPIPHANY_MSW_ENTITY_AND || entity == EPIPHANY_MSW_ENTITY_OR
      || entity== EPIPHANY_MSW_ENTITY_CONFIG)
    return priority;
  if (priority > 3)
    switch (priority)
      {
      case 4: return FP_MODE_ROUND_UNKNOWN;
      case 5: return FP_MODE_NONE;
      default: gcc_unreachable ();
      }
  switch ((enum attr_fp_mode) epiphany_normal_fp_mode)
    {
      case FP_MODE_INT:
	switch (priority)
	  {
	  case 0: return FP_MODE_INT;
	  case 1: return epiphany_normal_fp_rounding;
	  case 2: return (epiphany_normal_fp_rounding == FP_MODE_ROUND_NEAREST
			  ? FP_MODE_ROUND_TRUNC : FP_MODE_ROUND_NEAREST);
	  case 3: return FP_MODE_CALLER;
	  default: gcc_unreachable ();
	  }
      case FP_MODE_ROUND_NEAREST:
      case FP_MODE_CALLER:
	switch (priority)
	  {
	  case 0: return FP_MODE_ROUND_NEAREST;
	  case 1: return FP_MODE_ROUND_TRUNC;
	  case 2: return FP_MODE_INT;
	  case 3: return FP_MODE_CALLER;
	  default: gcc_unreachable ();
	  }
      case FP_MODE_ROUND_TRUNC:
	switch (priority)
	  {
	  case 0: return FP_MODE_ROUND_TRUNC;
	  case 1: return FP_MODE_ROUND_NEAREST;
	  case 2: return FP_MODE_INT;
	  case 3: return FP_MODE_CALLER;
	  default: gcc_unreachable ();
	  }
      case FP_MODE_ROUND_UNKNOWN:
      case FP_MODE_NONE:
	gcc_unreachable ();
    }
  gcc_unreachable ();
}

int
epiphany_mode_needed (int entity, rtx_insn *insn, HARD_REG_SET)
{
  enum attr_fp_mode mode;

  if (recog_memoized (insn) < 0)
    {
      if (entity == EPIPHANY_MSW_ENTITY_AND
	  || entity == EPIPHANY_MSW_ENTITY_OR
	  || entity == EPIPHANY_MSW_ENTITY_CONFIG)
	return 2;
      return FP_MODE_NONE;
    }
  mode = get_attr_fp_mode (insn);

  switch (entity)
  {
  case EPIPHANY_MSW_ENTITY_AND:
    return mode != FP_MODE_NONE && mode != FP_MODE_INT ? 1 : 2;
  case EPIPHANY_MSW_ENTITY_OR:
    return mode == FP_MODE_INT ? 1 : 2;
  case EPIPHANY_MSW_ENTITY_CONFIG:
    /* We must know/save config before we set it to something else.
       Where we need the original value, we are fine with having it
       just unchanged from the function start.
       Because of the nature of the mode switching optimization,
       a restore will be dominated by a clobber.  */
    if (mode != FP_MODE_NONE && mode != FP_MODE_CALLER)
      return 1;
    /* A cpecial case are abnormal edges, which are deemed to clobber
       the mode as well.  We need to pin this effect on a actually
       dominating insn, and one where the frame can be accessed, too, in
       case the pseudo used to save CONFIG doesn't get a hard register.  */
    if (CALL_P (insn) && find_reg_note (insn, REG_EH_REGION, NULL_RTX))
      return 1;
    return 2;
  case EPIPHANY_MSW_ENTITY_ROUND_KNOWN:
    if (recog_memoized (insn) == CODE_FOR_set_fp_mode)
      mode = (enum attr_fp_mode) epiphany_mode_after (entity, mode, insn, {});
    /* Fall through.  */
  case EPIPHANY_MSW_ENTITY_NEAREST:
  case EPIPHANY_MSW_ENTITY_TRUNC:
    if (mode == FP_MODE_ROUND_UNKNOWN)
      {
	MACHINE_FUNCTION (cfun)->unknown_mode_uses++;
	return FP_MODE_NONE;
      }
    return mode;
  case EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN:
    if (mode == FP_MODE_ROUND_NEAREST || mode == FP_MODE_ROUND_TRUNC)
	return FP_MODE_ROUND_UNKNOWN;
    return mode;
  case EPIPHANY_MSW_ENTITY_FPU_OMNIBUS:
    if (mode == FP_MODE_ROUND_UNKNOWN)
      return epiphany_normal_fp_rounding;
    return mode;
  default:
    gcc_unreachable ();
  }
}

static int
epiphany_mode_entry_exit (int entity, bool exit)
{
  int normal_mode = epiphany_normal_fp_mode ;

  MACHINE_FUNCTION (cfun)->sw_entities_processed |= (1 << entity);
  if (epiphany_is_interrupt_p (current_function_decl))
    normal_mode = FP_MODE_CALLER;
  switch (entity)
    {
    case EPIPHANY_MSW_ENTITY_AND:
      if (exit)
	return normal_mode != FP_MODE_INT ? 1 : 2;
      return 0;
    case EPIPHANY_MSW_ENTITY_OR:
      if (exit)
	return normal_mode == FP_MODE_INT ? 1 : 2;
      return 0;
    case EPIPHANY_MSW_ENTITY_CONFIG:
      if (exit)
	return 2;
      return normal_mode == FP_MODE_CALLER ? 0 : 1;
    case EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN:
      if (normal_mode == FP_MODE_ROUND_NEAREST
	  || normal_mode == FP_MODE_ROUND_TRUNC)
      return FP_MODE_ROUND_UNKNOWN;
      /* Fall through.  */
    case EPIPHANY_MSW_ENTITY_NEAREST:
    case EPIPHANY_MSW_ENTITY_TRUNC:
    case EPIPHANY_MSW_ENTITY_ROUND_KNOWN:
    case EPIPHANY_MSW_ENTITY_FPU_OMNIBUS:
      return normal_mode;
    default:
      gcc_unreachable ();
    }
}

int
epiphany_mode_after (int entity, int last_mode, rtx_insn *insn,
		     HARD_REG_SET)
{
  /* We have too few call-saved registers to hope to keep the masks across
     calls.  */
  if (entity == EPIPHANY_MSW_ENTITY_AND || entity == EPIPHANY_MSW_ENTITY_OR)
    {
      if (CALL_P (insn))
	return 0;
      return last_mode;
    }
  /* If there is an abnormal edge, we don't want the config register to
     be 'saved' again at the destination.
     The frame pointer adjustment is inside a PARALLEL because of the
     flags clobber.  */
  if (entity == EPIPHANY_MSW_ENTITY_CONFIG && NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == PARALLEL
      && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET
      && SET_DEST (XVECEXP (PATTERN (insn), 0, 0)) == frame_pointer_rtx)
    {
      gcc_assert (cfun->has_nonlocal_label);
      return 1;
    }
  if (recog_memoized (insn) < 0)
    return last_mode;
  if (get_attr_fp_mode (insn) == FP_MODE_ROUND_UNKNOWN
      && last_mode != FP_MODE_ROUND_NEAREST && last_mode != FP_MODE_ROUND_TRUNC)
    {
      if (entity == EPIPHANY_MSW_ENTITY_NEAREST)
	return FP_MODE_ROUND_NEAREST;
      if (entity == EPIPHANY_MSW_ENTITY_TRUNC)
	return FP_MODE_ROUND_TRUNC;
    }
  if (recog_memoized (insn) == CODE_FOR_set_fp_mode)
    {
      rtx src = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
      int fp_mode;

      if (REG_P (src))
	return FP_MODE_CALLER;
      fp_mode = INTVAL (XVECEXP (XEXP (src, 0), 0, 0));
      if (entity == EPIPHANY_MSW_ENTITY_ROUND_UNKNOWN
	  && (fp_mode == FP_MODE_ROUND_NEAREST
	      || fp_mode == EPIPHANY_MSW_ENTITY_TRUNC))
	return FP_MODE_ROUND_UNKNOWN;
      return fp_mode;
    }
  return last_mode;
}

static int
epiphany_mode_entry (int entity)
{
  return epiphany_mode_entry_exit (entity, false);
}

static int
epiphany_mode_exit (int entity)
{
  return epiphany_mode_entry_exit (entity, true);
}

void
emit_set_fp_mode (int entity, int mode, int prev_mode ATTRIBUTE_UNUSED,
		  HARD_REG_SET regs_live ATTRIBUTE_UNUSED)
{
  rtx save_cc, cc_reg, mask, src, src2;
  enum attr_fp_mode fp_mode;

  if (!MACHINE_FUNCTION (cfun)->and_mask)
    {
      MACHINE_FUNCTION (cfun)->and_mask = gen_reg_rtx (SImode);
      MACHINE_FUNCTION (cfun)->or_mask = gen_reg_rtx (SImode);
    }
  if (entity == EPIPHANY_MSW_ENTITY_AND)
    {
      gcc_assert (mode >= 0 && mode <= 2);
      if (mode == 1)
	emit_move_insn (MACHINE_FUNCTION (cfun)->and_mask,
			gen_int_mode (0xfff1fffe, SImode));
      return;
    }
  else if (entity == EPIPHANY_MSW_ENTITY_OR)
    {
      gcc_assert (mode >= 0 && mode <= 2);
      if (mode == 1)
	emit_move_insn (MACHINE_FUNCTION (cfun)->or_mask, GEN_INT(0x00080000));
      return;
    }
  else if (entity == EPIPHANY_MSW_ENTITY_CONFIG)
    {
      /* Mode switching optimization is done after emit_initial_value_sets,
	 so we have to take care of CONFIG_REGNUM here.  */
      gcc_assert (mode >= 0 && mode <= 2);
      rtx save = get_hard_reg_initial_val (SImode, CONFIG_REGNUM);
      if (mode == 1)
	emit_insn (gen_save_config (save));
      return;
    }
  fp_mode = (enum attr_fp_mode) mode;
  src = NULL_RTX;

  switch (fp_mode)
    {
      case FP_MODE_CALLER:
	/* The EPIPHANY_MSW_ENTITY_CONFIG processing must come later
	   so that the config save gets inserted before the first use.  */
	gcc_assert (entity > EPIPHANY_MSW_ENTITY_CONFIG);
	src = get_hard_reg_initial_val (SImode, CONFIG_REGNUM);
	mask = MACHINE_FUNCTION (cfun)->and_mask;
	break;
      case FP_MODE_ROUND_UNKNOWN:
	MACHINE_FUNCTION (cfun)->unknown_mode_sets++;
	mask = MACHINE_FUNCTION (cfun)->and_mask;
	break;
      case FP_MODE_ROUND_NEAREST:
	if (entity == EPIPHANY_MSW_ENTITY_TRUNC)
	  return;
	mask = MACHINE_FUNCTION (cfun)->and_mask;
	break;
      case FP_MODE_ROUND_TRUNC:
	if (entity == EPIPHANY_MSW_ENTITY_NEAREST)
	  return;
	mask = MACHINE_FUNCTION (cfun)->and_mask;
	break;
      case FP_MODE_INT:
	mask = MACHINE_FUNCTION (cfun)->or_mask;
	break;
      case FP_MODE_NONE:
      default:
	gcc_unreachable ();
    }
  save_cc = gen_reg_rtx (CCmode);
  cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
  emit_move_insn (save_cc, cc_reg);
  mask = force_reg (SImode, mask);
  if (!src)
    {
      rtvec v = gen_rtvec (1, GEN_INT (fp_mode));

      src = gen_rtx_CONST (SImode, gen_rtx_UNSPEC (SImode, v, UNSPEC_FP_MODE));
    }
  if (entity == EPIPHANY_MSW_ENTITY_ROUND_KNOWN
      || entity == EPIPHANY_MSW_ENTITY_FPU_OMNIBUS)
    src2 = copy_rtx (src);
  else
    {
      rtvec v = gen_rtvec (1, GEN_INT (FP_MODE_ROUND_UNKNOWN));

      src2 = gen_rtx_CONST (SImode, gen_rtx_UNSPEC (SImode, v, UNSPEC_FP_MODE));
    }
  emit_insn (gen_set_fp_mode (src, src2, mask));
  emit_move_insn (cc_reg, save_cc);
}

void
epiphany_expand_set_fp_mode (rtx *operands)
{
  rtx ctrl = gen_rtx_REG (SImode, CONFIG_REGNUM);
  rtx src = operands[0];
  rtx mask_reg = operands[2];
  rtx scratch = operands[3];
  enum attr_fp_mode fp_mode;


  gcc_assert (rtx_equal_p (src, operands[1])
	      /* Sometimes reload gets silly and reloads the same pseudo
		 into different registers.  */
	      || (REG_P (src) && REG_P (operands[1])));

  if (!epiphany_uninterruptible_p (current_function_decl))
    emit_insn (gen_gid ());
  emit_move_insn (scratch, ctrl);

  if (GET_CODE (src) == REG)
    {
      /* FP_MODE_CALLER */
      emit_insn (gen_xorsi3 (scratch, scratch, src));
      emit_insn (gen_andsi3 (scratch, scratch, mask_reg));
      emit_insn (gen_xorsi3 (scratch, scratch, src));
    }
  else
    {
      gcc_assert (GET_CODE (src) == CONST);
      src = XEXP (src, 0);
      fp_mode = (enum attr_fp_mode) INTVAL (XVECEXP (src, 0, 0));
      switch (fp_mode)
	{
	case FP_MODE_ROUND_NEAREST:
	  emit_insn (gen_andsi3 (scratch, scratch, mask_reg));
	  break;
	case FP_MODE_ROUND_TRUNC:
	  emit_insn (gen_andsi3 (scratch, scratch, mask_reg));
	  emit_insn (gen_add2_insn (scratch, const1_rtx));
	  break;
	case FP_MODE_INT:
	  emit_insn (gen_iorsi3 (scratch, scratch, mask_reg));
	  break;
	case FP_MODE_CALLER:
	case FP_MODE_ROUND_UNKNOWN:
	case FP_MODE_NONE:
	  gcc_unreachable ();
	}
    }
  emit_move_insn (ctrl, scratch);
  if (!epiphany_uninterruptible_p (current_function_decl))
    emit_insn (gen_gie ());
}

void
epiphany_insert_mode_switch_use (rtx_insn *insn,
				 int entity ATTRIBUTE_UNUSED,
				 int mode ATTRIBUTE_UNUSED)
{
  rtx pat = PATTERN (insn);
  rtvec v;
  int len, i;
  rtx near = gen_rtx_REG (SImode, FP_NEAREST_REGNUM);
  rtx trunc = gen_rtx_REG (SImode, FP_TRUNCATE_REGNUM);

  if (entity != EPIPHANY_MSW_ENTITY_FPU_OMNIBUS)
    return;
  switch ((enum attr_fp_mode) get_attr_fp_mode (insn))
    {
      case FP_MODE_ROUND_NEAREST:
	near = gen_rtx_USE (VOIDmode, near);
	trunc = gen_rtx_CLOBBER (VOIDmode, trunc);
	break;
      case FP_MODE_ROUND_TRUNC:
	near = gen_rtx_CLOBBER (VOIDmode, near);
	trunc = gen_rtx_USE (VOIDmode, trunc);
	break;
      case FP_MODE_ROUND_UNKNOWN:
	near = gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, FP_ANYFP_REGNUM));
	trunc = copy_rtx (near);
	/* Fall through.  */
      case FP_MODE_INT:
      case FP_MODE_CALLER:
	near = gen_rtx_USE (VOIDmode, near);
	trunc = gen_rtx_USE (VOIDmode, trunc);
	break;
      case FP_MODE_NONE:
	gcc_unreachable ();
    }
  gcc_assert (GET_CODE (pat) == PARALLEL);
  len = XVECLEN (pat, 0);
  v = rtvec_alloc (len + 2);
  for (i = 0; i < len; i++)
    RTVEC_ELT (v, i) = XVECEXP (pat, 0, i);
  RTVEC_ELT (v, len) = near;
  RTVEC_ELT (v, len + 1) = trunc;
  pat = gen_rtx_PARALLEL (VOIDmode, v);
  PATTERN (insn) = pat;
  MACHINE_FUNCTION (cfun)->control_use_inserted = true;
}

bool
epiphany_epilogue_uses (int regno)
{
  if (regno == GPR_LR)
    return true;
  if (reload_completed && epiphany_is_interrupt_p (current_function_decl))
    {
      if (fixed_regs[regno]
	  && regno != STATUS_REGNUM && regno != IRET_REGNUM
	  && regno != FP_NEAREST_REGNUM && regno != FP_TRUNCATE_REGNUM)
	return false;
      return true;
    }
  if (regno == FP_NEAREST_REGNUM
      && epiphany_normal_fp_mode != FP_MODE_ROUND_TRUNC)
    return true;
  if (regno == FP_TRUNCATE_REGNUM
      && epiphany_normal_fp_mode != FP_MODE_ROUND_NEAREST)
    return true;
  return false;
}

static unsigned int
epiphany_min_divisions_for_recip_mul (machine_mode mode)
{
  if (flag_reciprocal_math && mode == SFmode)
    /* We'll expand into a multiply-by-reciprocal anyway, so we might a well do
       it already at the tree level and expose it to further optimizations.  */
    return 1;
  return default_min_divisions_for_recip_mul (mode);
}

static machine_mode
epiphany_preferred_simd_mode (scalar_mode mode ATTRIBUTE_UNUSED)
{
  return TARGET_VECT_DOUBLE ? DImode : SImode;
}

static bool
epiphany_vector_mode_supported_p (machine_mode mode)
{
  if (mode == V2SFmode)
    return true;
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
      && (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8))
    return true;
  return false;
}

static bool
epiphany_vector_alignment_reachable (const_tree type, bool is_packed)
{
  /* Vectors which aren't in packed structures will not be less aligned than
     the natural alignment of their element type, so this is safe.  */
  if (TYPE_ALIGN_UNIT (type) == 4)
    return !is_packed;

  return default_builtin_vector_alignment_reachable (type, is_packed);
}

static bool
epiphany_support_vector_misalignment (machine_mode mode, const_tree type,
				      int misalignment, bool is_packed)
{
  if (GET_MODE_SIZE (mode) == 8 && misalignment % 4 == 0)
    return true;
  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* STRUCTURE_SIZE_BOUNDARY seems a bit crude in how it enlarges small
   structs.  Make structs double-word-aligned it they are a double word or
   (potentially) larger;  failing that, do the same for a size of 32 bits.  */
unsigned
epiphany_special_round_type_align (tree type, unsigned computed,
				   unsigned specified)
{
  unsigned align = MAX (computed, specified);
  tree field;
  HOST_WIDE_INT total, max;
  unsigned try_align = FASTEST_ALIGNMENT;

  if (maximum_field_alignment && try_align > maximum_field_alignment)
    try_align = maximum_field_alignment;
  if (align >= try_align)
    return align;
  for (max = 0, field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      tree offset, size;

      if (TREE_CODE (field) != FIELD_DECL
	  || TREE_TYPE (field) == error_mark_node)
	continue;
      offset = bit_position (field);
      size = DECL_SIZE (field);
      if (!tree_fits_uhwi_p (offset) || !tree_fits_uhwi_p (size)
	  || tree_to_uhwi (offset) >= try_align
	  || tree_to_uhwi (size) >= try_align)
	return try_align;
      total = tree_to_uhwi (offset) + tree_to_uhwi (size);
      if (total > max)
	max = total;
    }
  if (max >= (HOST_WIDE_INT) try_align)
    align = try_align;
  else if (try_align > 32 && max >= 32)
    align = max > 32 ? 64 : 32;
  return align;
}

/* Upping the alignment of arrays in structs is not only a performance
   enhancement, it also helps preserve assumptions about how
   arrays-at-the-end-of-structs work, like for struct gcov_fn_info in
   libgcov.c .  */
unsigned
epiphany_adjust_field_align (tree type, unsigned computed)
{
  if (computed == 32
      && TREE_CODE (type) == ARRAY_TYPE)
    {
      tree elmsz = TYPE_SIZE (TREE_TYPE (type));

      if (!tree_fits_uhwi_p (elmsz) || tree_to_uhwi (elmsz) >= 32)
	return 64;
    }
  return computed;
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
epiphany_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			  HOST_WIDE_INT delta,
			  HOST_WIDE_INT vcall_offset,
			  tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));
  int this_regno
    = aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function) ? 1 : 0;
  const char *this_name = reg_names[this_regno];
  const char *fname;

  assemble_start_function (thunk, fnname);
  /* We use IP and R16 as a scratch registers.  */
  gcc_assert (call_used_or_fixed_reg_p (GPR_IP));
  gcc_assert (call_used_or_fixed_reg_p (GPR_16));

  /* Add DELTA.  When possible use a plain add, otherwise load it into
     a register first. */
  if (delta == 0)
    ; /* Done.  */
  else if (SIMM11 (delta))
    asm_fprintf (file, "\tadd\t%s,%s,%d\n", this_name, this_name, (int) delta);
  else if (delta < 0 && delta >= -0xffff)
    {
      asm_fprintf (file, "\tmov\tip,%d\n", (int) -delta);
      asm_fprintf (file, "\tsub\t%s,%s,ip\n", this_name, this_name);
    }
  else
    {
      asm_fprintf (file, "\tmov\tip,%%low(%ld)\n", (long) delta);
      if (delta & ~0xffff)
	asm_fprintf (file, "\tmovt\tip,%%high(%ld)\n", (long) delta);
      asm_fprintf (file, "\tadd\t%s,%s,ip\n", this_name, this_name);
    }

  /* If needed, add *(*THIS + VCALL_OFFSET) to THIS.  */
  if (vcall_offset != 0)
    {
      /* ldr ip,[this]		--> temp = *this
	 ldr ip,[ip,vcall_offset] > temp = *(*this + vcall_offset)
	 add this,this,ip	--> this+ = *(*this + vcall_offset) */
      asm_fprintf (file, "\tldr\tip, [%s]\n", this_name);
      if (vcall_offset < -0x7ff * 4 || vcall_offset > 0x7ff * 4
	  || (vcall_offset & 3) != 0)
	{
	  asm_fprintf (file, "\tmov\tr16, %%low(%ld)\n", (long) vcall_offset);
	  asm_fprintf (file, "\tmovt\tr16, %%high(%ld)\n", (long) vcall_offset);
	  asm_fprintf (file, "\tldr\tip, [ip,r16]\n");
	}
      else
	asm_fprintf (file, "\tldr\tip, [ip,%d]\n", (int) vcall_offset / 4);
      asm_fprintf (file, "\tadd\t%s, %s, ip\n", this_name, this_name);
    }

  fname = XSTR (XEXP (DECL_RTL (function), 0), 0);
  if (epiphany_is_long_call_p (XEXP (DECL_RTL (function), 0)))
    {
      fputs ("\tmov\tip,%low(", file);
      assemble_name (file, fname);
      fputs (")\n\tmovt\tip,%high(", file);
      assemble_name (file, fname);
      fputs (")\n\tjr ip\n", file);
    }
  else
    {
      fputs ("\tb\t", file);
      assemble_name (file, fname);
      fputc ('\n', file);
    }
  assemble_end_function (thunk, fnname);
}

void
epiphany_start_function (FILE *file, const char *name, tree decl)
{
  /* If the function doesn't fit into the on-chip memory, it will have a
     section attribute - or lack of it - that denotes it goes somewhere else.
     But the architecture spec says that an interrupt vector still has to
     point to on-chip memory.  So we must place a jump there to get to the
     actual function implementation.  The forwarder_section attribute
     specifies the section where this jump goes.
     This mechanism can also be useful to have a shortcall destination for
     a function that is actually placed much farther away.  */
  tree attrs, int_attr, int_names, int_name, forwarder_attr;

  attrs = DECL_ATTRIBUTES (decl);
  int_attr = lookup_attribute ("interrupt", attrs);
  if (int_attr)
    for (int_names = TREE_VALUE (int_attr); int_names;
	 int_names = TREE_CHAIN (int_names))
      {
	char buf[99];

	int_name = TREE_VALUE (int_names);
	sprintf (buf, "ivt_entry_%.80s", TREE_STRING_POINTER (int_name));
	switch_to_section (get_section (buf, SECTION_CODE, decl));
	fputs ("\tb\t", file);
	assemble_name (file, name);
	fputc ('\n', file);
      }
  forwarder_attr = lookup_attribute ("forwarder_section", attrs);
  if (forwarder_attr)
    {
      const char *prefix = "__forwarder_dst_";
      char *dst_name = (char *) alloca (strlen (prefix) + strlen (name) + 1);

      strcpy (dst_name, prefix);
      strcat (dst_name, name);
      forwarder_attr = TREE_VALUE (TREE_VALUE (forwarder_attr));
      switch_to_section (get_section (TREE_STRING_POINTER (forwarder_attr),
			 SECTION_CODE, decl));
      ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);
      if (epiphany_is_long_call_p (XEXP (DECL_RTL (decl), 0)))
	{
	  int tmp = GPR_0;

	  if (int_attr)
	    fputs ("\tstrd r0,[sp,-1]\n", file);
	  else
	    tmp = GPR_16;
	  gcc_assert (call_used_or_fixed_reg_p (tmp));
	  fprintf (file, "\tmov r%d,%%low(", tmp);
	  assemble_name (file, dst_name);
	  fprintf (file, ")\n"
		   "\tmovt r%d,%%high(", tmp);
	  assemble_name (file, dst_name);
	  fprintf (file, ")\n"
		 "\tjr r%d\n", tmp);
	}
      else
	{
	  fputs ("\tb\t", file);
	  assemble_name (file, dst_name);
	  fputc ('\n', file);
	}
      name = dst_name;
    }
  switch_to_section (function_section (decl));
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);
}


/* Implement TARGET_CONSTANT_ALIGNMENT.  */

static HOST_WIDE_INT
epiphany_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST)
    return MAX (align, FASTEST_ALIGNMENT);
  return align;
}

/* Implement TARGET_STARTING_FRAME_OFFSET.  */

static HOST_WIDE_INT
epiphany_starting_frame_offset (void)
{
  return epiphany_stack_offset;
}

struct gcc_target targetm = TARGET_INITIALIZER;
