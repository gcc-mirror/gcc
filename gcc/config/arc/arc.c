/* Subroutines used for code generation on the Synopsys DesignWare ARC cpu.
   Copyright (C) 1994-2017 Free Software Foundation, Inc.

   Sources derived from work done by Sankhya Technologies (www.sankhya.com) on
   behalf of Synopsys Inc.

   Position Independent Code support added,Code cleaned up,
   Comments and Support For ARC700 instructions added by
   Saurabh Verma (saurabh.verma@codito.com)
   Ramana Radhakrishnan(ramana.radhakrishnan@codito.com)

   Fixing ABI inconsistencies, optimizations for ARC600 / ARC700 pipelines,
   profiling support added by Joern Rennecke <joern.rennecke@embecosm.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "tm-constrs.h"
#include "reload.h" /* For operands_match_p */
#include "cfgrtl.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "alias.h"
#include "opts.h"
#include "hw-doloop.h"

/* Which cpu we're compiling for (ARC600, ARC601, ARC700).  */
static char arc_cpu_name[10] = "";
static const char *arc_cpu_string = arc_cpu_name;

/* Maximum size of a loop.  */
#define ARC_MAX_LOOP_LENGTH 4095

/* ??? Loads can handle any constant, stores can only handle small ones.  */
/* OTOH, LIMMs cost extra, so their usefulness is limited.  */
#define RTX_OK_FOR_OFFSET_P(MODE, X) \
(GET_CODE (X) == CONST_INT \
 && SMALL_INT_RANGE (INTVAL (X), (GET_MODE_SIZE (MODE) - 1) & -4, \
		     (INTVAL (X) & (GET_MODE_SIZE (MODE) - 1) & 3 \
		      ? 0 \
		      : -(-GET_MODE_SIZE (MODE) | -4) >> 1)))

#define LEGITIMATE_SMALL_DATA_OFFSET_P(X)				\
  (GET_CODE (X) == CONST						\
   && GET_CODE (XEXP ((X), 0)) == PLUS					\
   && GET_CODE (XEXP (XEXP ((X), 0), 0)) == SYMBOL_REF			\
   && SYMBOL_REF_SMALL_P (XEXP (XEXP ((X), 0), 0))			\
   && GET_CODE (XEXP(XEXP ((X), 0), 1)) == CONST_INT			\
   && INTVAL (XEXP (XEXP ((X), 0), 1)) <= g_switch_value)

#define LEGITIMATE_SMALL_DATA_ADDRESS_P(X)				\
  (GET_CODE (X) == PLUS							\
     && REG_P (XEXP ((X), 0))						\
     && REGNO (XEXP ((X), 0)) == SDATA_BASE_REGNUM			\
     && ((GET_CODE (XEXP ((X), 1)) == SYMBOL_REF			\
	    && SYMBOL_REF_SMALL_P (XEXP ((X), 1)))			\
	 || LEGITIMATE_SMALL_DATA_OFFSET_P (XEXP ((X), 1))))

/* Array of valid operand punctuation characters.  */
char arc_punct_chars[256];

/* State used by arc_ccfsm_advance to implement conditional execution.  */
struct GTY (()) arc_ccfsm
{
  int state;
  int cc;
  rtx cond;
  rtx_insn *target_insn;
  int target_label;
};

/* Status of the IRQ_CTRL_AUX register.  */
typedef struct irq_ctrl_saved_t
{
  /* Last register number used by IRQ_CTRL_SAVED aux_reg.  */
  short irq_save_last_reg;
  /* True if BLINK is automatically saved.  */
  bool  irq_save_blink;
  /* True if LPCOUNT is automatically saved.  */
  bool  irq_save_lpcount;
} irq_ctrl_saved_t;
static irq_ctrl_saved_t irq_ctrl_saved;

#define ARC_AUTOBLINK_IRQ_P(FNTYPE)				\
  ((ARC_INTERRUPT_P (FNTYPE)					\
    && irq_ctrl_saved.irq_save_blink)				\
   || (ARC_FAST_INTERRUPT_P (FNTYPE)				\
       && rgf_banked_register_count > 8))

#define ARC_AUTOFP_IRQ_P(FNTYPE)				\
  ((ARC_INTERRUPT_P (FNTYPE)					\
    && (irq_ctrl_saved.irq_save_last_reg > 26))			\
  || (ARC_FAST_INTERRUPT_P (FNTYPE)				\
      && rgf_banked_register_count > 8))

#define ARC_AUTO_IRQ_P(FNTYPE)					\
  (ARC_INTERRUPT_P (FNTYPE) && !ARC_FAST_INTERRUPT_P (FNTYPE)	\
   && (irq_ctrl_saved.irq_save_blink				\
       || (irq_ctrl_saved.irq_save_last_reg >= 0)))

/* Number of registers in second bank for FIRQ support.  */
static int rgf_banked_register_count;

#define arc_ccfsm_current cfun->machine->ccfsm_current

#define ARC_CCFSM_BRANCH_DELETED_P(STATE) \
  ((STATE)->state == 1 || (STATE)->state == 2)

/* Indicate we're conditionalizing insns now.  */
#define ARC_CCFSM_RECORD_BRANCH_DELETED(STATE) \
  ((STATE)->state += 2)

#define ARC_CCFSM_COND_EXEC_P(STATE) \
  ((STATE)->state == 3 || (STATE)->state == 4 || (STATE)->state == 5 \
   || current_insn_predicate)

/* Check if INSN has a 16 bit opcode considering struct arc_ccfsm *STATE.  */
#define CCFSM_ISCOMPACT(INSN,STATE) \
  (ARC_CCFSM_COND_EXEC_P (STATE) \
   ? (get_attr_iscompact (INSN) == ISCOMPACT_TRUE \
      || get_attr_iscompact (INSN) == ISCOMPACT_TRUE_LIMM) \
   : get_attr_iscompact (INSN) != ISCOMPACT_FALSE)

/* Likewise, but also consider that INSN might be in a delay slot of JUMP.  */
#define CCFSM_DBR_ISCOMPACT(INSN,JUMP,STATE) \
  ((ARC_CCFSM_COND_EXEC_P (STATE) \
    || (JUMP_P (JUMP) \
	&& INSN_ANNULLED_BRANCH_P (JUMP) \
	&& (TARGET_AT_DBR_CONDEXEC || INSN_FROM_TARGET_P (INSN)))) \
   ? (get_attr_iscompact (INSN) == ISCOMPACT_TRUE \
      || get_attr_iscompact (INSN) == ISCOMPACT_TRUE_LIMM) \
   : get_attr_iscompact (INSN) != ISCOMPACT_FALSE)

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
/* When optimizing for speed:
    Let p be the probability that the potentially skipped insns need to
    be executed, pn the cost of a correctly predicted non-taken branch,
    mt the cost of a mis/non-predicted taken branch,
    mn mispredicted non-taken, pt correctly predicted taken ;
    costs expressed in numbers of instructions like the ones considered
    skipping.
    Unfortunately we don't have a measure of predictability - this
    is linked to probability only in that in the no-eviction-scenario
    there is a lower bound 1 - 2 * min (p, 1-p), and a somewhat larger
    value that can be assumed *if* the distribution is perfectly random.
    A predictability of 1 is perfectly plausible not matter what p is,
    because the decision could be dependent on an invocation parameter
    of the program.
    For large p, we want MAX_INSNS_SKIPPED == pn/(1-p) + mt - pn
    For small p, we want MAX_INSNS_SKIPPED == pt

   When optimizing for size:
    We want to skip insn unless we could use 16 opcodes for the
    non-conditionalized insn to balance the branch length or more.
    Performance can be tie-breaker.  */
/* If the potentially-skipped insns are likely to be executed, we'll
   generally save one non-taken branch
   o
   this to be no less than the 1/p  */
#define MAX_INSNS_SKIPPED 3

/* A nop is needed between a 4 byte insn that sets the condition codes and
   a branch that uses them (the same isn't true for an 8 byte insn that sets
   the condition codes).  Set by arc_ccfsm_advance.  Used by
   arc_print_operand.  */

static int get_arc_condition_code (rtx);

static tree arc_handle_interrupt_attribute (tree *, tree, tree, int, bool *);
static tree arc_handle_fndecl_attribute (tree *, tree, tree, int, bool *);

/* Initialized arc_attribute_table to NULL since arc doesnot have any
   machine specific supported attributes.  */
const struct attribute_spec arc_attribute_table[] =
{
 /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
      affects_type_identity } */
  { "interrupt", 1, 1, true, false, false, arc_handle_interrupt_attribute, true },
  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 21/25 bit addressing range of a normal function
     call.  */
  { "long_call",    0, 0, false, true,  true,  NULL, false },
  /* Whereas these functions are always known to reside within the 25 bit
     addressing range of unconditionalized bl.  */
  { "medium_call",   0, 0, false, true,  true,  NULL, false },
  /* And these functions are always known to reside within the 21 bit
     addressing range of blcc.  */
  { "short_call",   0, 0, false, true,  true,  NULL, false },
  /* Function which are not having the prologue and epilogue generated
     by the compiler.  */
  { "naked", 0, 0, true, false, false, arc_handle_fndecl_attribute, false },
  { NULL, 0, 0, false, false, false, NULL, false }
};
static int arc_comp_type_attributes (const_tree, const_tree);
static void arc_file_start (void);
static void arc_internal_label (FILE *, const char *, unsigned long);
static void arc_output_mi_thunk (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT,
				 tree);
static int arc_address_cost (rtx, machine_mode, addr_space_t, bool);
static void arc_encode_section_info (tree decl, rtx rtl, int first);

static void arc_init_builtins (void);
static rtx arc_expand_builtin (tree, rtx, rtx, machine_mode, int);

static int branch_dest (rtx);

static void  arc_output_pic_addr_const (FILE *,  rtx, int);
static bool arc_function_ok_for_sibcall (tree, tree);
static rtx arc_function_value (const_tree, const_tree, bool);
const char * output_shift (rtx *);
static void arc_reorg (void);
static bool arc_in_small_data_p (const_tree);

static void arc_init_reg_tables (void);
static bool arc_return_in_memory (const_tree, const_tree);
static bool arc_vector_mode_supported_p (machine_mode);

static bool arc_can_use_doloop_p (const widest_int &, const widest_int &,
				  unsigned int, bool);
static const char *arc_invalid_within_doloop (const rtx_insn *);

static void output_short_suffix (FILE *file);

static bool arc_frame_pointer_required (void);

static bool arc_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT,
						unsigned int,
						enum by_pieces_operation op,
						bool);

/* Globally visible information about currently selected cpu.  */
const arc_cpu_t *arc_selected_cpu;

static bool
legitimate_scaled_address_p (machine_mode mode, rtx op, bool strict)
{
  if (GET_CODE (op) != PLUS)
    return false;

  if (GET_CODE (XEXP (op, 0)) != MULT)
    return false;

  /* Check multiplication operands.  */
  if (!RTX_OK_FOR_INDEX_P (XEXP (XEXP (op, 0), 0), strict))
    return false;

  if (!CONST_INT_P (XEXP (XEXP (op, 0), 1)))
    return false;

  switch (GET_MODE_SIZE (mode))
    {
    case 2:
      if (INTVAL (XEXP (XEXP (op, 0), 1)) != 2)
	return false;
      break;
    case 8:
      if (!TARGET_LL64)
	return false;
      /*  Fall through. */
    case 4:
      if (INTVAL (XEXP (XEXP (op, 0), 1)) != 4)
	return false;
    default:
      return false;
    }

  /* Check the base.  */
  if (RTX_OK_FOR_BASE_P (XEXP (op, 1), (strict)))
    return true;

  if (flag_pic)
    {
      if (CONST_INT_P (XEXP (op, 1)))
	return true;
      return false;
    }
  if (CONSTANT_P (XEXP (op, 1)))
    {
      /* Scalled addresses for sdata is done other places.  */
      if (GET_CODE (XEXP (op, 1)) == SYMBOL_REF
	  && SYMBOL_REF_SMALL_P (XEXP (op, 1)))
	return false;
      return true;
    }

  return false;
}

/* Check for constructions like REG + OFFS, where OFFS can be a
   register, an immediate or an long immediate. */

static bool
legitimate_offset_address_p (machine_mode mode, rtx x, bool index, bool strict)
{
  if (GET_CODE (x) != PLUS)
    return false;

  if (!RTX_OK_FOR_BASE_P (XEXP (x, 0), (strict)))
    return false;

  /* Check for: [Rx + small offset] or [Rx + Ry].  */
  if (((index && RTX_OK_FOR_INDEX_P (XEXP (x, 1), (strict))
	&& GET_MODE_SIZE ((mode)) <= 4)
       || RTX_OK_FOR_OFFSET_P (mode, XEXP (x, 1))))
    return true;

  /* Check for [Rx + symbol].  */
  if (!flag_pic
      && (GET_CODE (XEXP (x, 1)) == SYMBOL_REF)
      /* Avoid this type of address for double or larger modes.  */
      && (GET_MODE_SIZE (mode) <= 4)
      /* Avoid small data which ends in something like GP +
	 symb@sda.  */
      && (!SYMBOL_REF_SMALL_P (XEXP (x, 1))))
    return true;

  return false;
}

/* Implements target hook vector_mode_supported_p.  */

static bool
arc_vector_mode_supported_p (machine_mode mode)
{
  switch (mode)
    {
    case E_V2HImode:
      return TARGET_PLUS_DMPY;
    case E_V4HImode:
    case E_V2SImode:
      return TARGET_PLUS_QMACW;
    case E_V4SImode:
    case E_V8HImode:
      return TARGET_SIMD_SET;

    default:
      return false;
    }
}

/* Implements target hook TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */

static machine_mode
arc_preferred_simd_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_HImode:
      return TARGET_PLUS_QMACW ? V4HImode : V2HImode;
    case E_SImode:
      return V2SImode;

    default:
      return word_mode;
    }
}

/* Implements target hook
   TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES.  */

static unsigned int
arc_autovectorize_vector_sizes (void)
{
  return TARGET_PLUS_QMACW ? (8 | 4) : 0;
}

/* TARGET_PRESERVE_RELOAD_P is still awaiting patch re-evaluation / review.  */
static bool arc_preserve_reload_p (rtx in) ATTRIBUTE_UNUSED;
static rtx arc_delegitimize_address (rtx);
static bool arc_can_follow_jump (const rtx_insn *follower,
				 const rtx_insn *followee);

static rtx frame_insn (rtx);
static void arc_function_arg_advance (cumulative_args_t, machine_mode,
				      const_tree, bool);
static rtx arc_legitimize_address_0 (rtx, rtx, machine_mode mode);

static void arc_finalize_pic (void);

/* initialize the GCC target structure.  */
#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES arc_comp_type_attributes
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START arc_file_start
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE arc_attribute_table
#undef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL arc_internal_label
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS arc_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST arc_address_cost

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO arc_encode_section_info

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM arc_cannot_force_const_mem

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  arc_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN arc_expand_builtin

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL arc_builtin_decl

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK arc_output_mi_thunk

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL arc_function_ok_for_sibcall

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG arc_reorg

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P arc_in_small_data_p

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE \
  default_promote_function_mode_always_promote

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY arc_return_in_memory
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE arc_pass_by_reference

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS arc_setup_incoming_varargs

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES arc_arg_partial_bytes

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE arc_function_value

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY arc_sched_adjust_priority

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P arc_vector_mode_supported_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE arc_preferred_simd_mode

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES arc_autovectorize_vector_sizes

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P arc_can_use_doloop_p

#undef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP arc_invalid_within_doloop

#undef TARGET_PRESERVE_RELOAD_P
#define TARGET_PRESERVE_RELOAD_P arc_preserve_reload_p

#undef TARGET_CAN_FOLLOW_JUMP
#define TARGET_CAN_FOLLOW_JUMP arc_can_follow_jump

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS arc_delegitimize_address

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  arc_use_by_pieces_infrastructure_p

/* Usually, we will be able to scale anchor offsets.
   When this fails, we want LEGITIMIZE_ADDRESS to kick in.  */
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET (-1024)
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET (1020)

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD arc_secondary_reload

#define TARGET_OPTION_OVERRIDE arc_override_options

#define TARGET_CONDITIONAL_REGISTER_USAGE arc_conditional_register_usage

#define TARGET_TRAMPOLINE_INIT arc_initialize_trampoline

#define TARGET_TRAMPOLINE_ADJUST_ADDRESS arc_trampoline_adjust_address

#define TARGET_CAN_ELIMINATE arc_can_eliminate

#define TARGET_FRAME_POINTER_REQUIRED arc_frame_pointer_required

#define TARGET_FUNCTION_ARG arc_function_arg

#define TARGET_FUNCTION_ARG_ADVANCE arc_function_arg_advance

#define TARGET_LEGITIMATE_CONSTANT_P arc_legitimate_constant_p

#define TARGET_LEGITIMATE_ADDRESS_P arc_legitimate_address_p

#define TARGET_MODE_DEPENDENT_ADDRESS_P arc_mode_dependent_address_p

#define TARGET_LEGITIMIZE_ADDRESS arc_legitimize_address

#define TARGET_ADJUST_INSN_LENGTH arc_adjust_insn_length

#define TARGET_INSN_LENGTH_PARAMETERS arc_insn_length_parameters

#undef TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P
#define TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P	\
  arc_no_speculation_in_delay_slots_p

#undef TARGET_LRA_P
#define TARGET_LRA_P arc_lra_p
#define TARGET_REGISTER_PRIORITY arc_register_priority
/* Stores with scaled offsets have different displacement ranges.  */
#define TARGET_DIFFERENT_ADDR_DISPLACEMENT_P hook_bool_void_true
#define TARGET_SPILL_CLASS arc_spill_class

#undef TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS arc_allocate_stack_slots_for_args

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN arc_warn_func_return

#include "target-def.h"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS
#endif

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN arc_dwarf_register_span

/* Try to keep the (mov:DF _, reg) as early as possible so
   that the d<add/sub/mul>h-lr insns appear together and can
   use the peephole2 pattern.  */

static int
arc_sched_adjust_priority (rtx_insn *insn, int priority)
{
  rtx set = single_set (insn);
  if (set
      && GET_MODE (SET_SRC(set)) == DFmode
      && GET_CODE (SET_SRC(set)) == REG)
    {
      /* Incrementing priority by 20 (empirically derived).  */
      return priority + 20;
    }

  return priority;
}

/* For ARC base register + offset addressing, the validity of the
   address is mode-dependent for most of the offset range, as the
   offset can be scaled by the access size.
   We don't expose these as mode-dependent addresses in the
   mode_dependent_address_p target hook, because that would disable
   lots of optimizations, and most uses of these addresses are for 32
   or 64 bit accesses anyways, which are fine.
   However, that leaves some addresses for 8 / 16 bit values not
   properly reloaded by the generic code, which is why we have to
   schedule secondary reloads for these.  */

static reg_class_t
arc_secondary_reload (bool in_p,
		      rtx x,
		      reg_class_t cl,
		      machine_mode mode,
		      secondary_reload_info *sri)
{
  enum rtx_code code = GET_CODE (x);

  if (cl == DOUBLE_REGS)
    return GENERAL_REGS;

  /* The loop counter register can be stored, but not loaded directly.  */
  if ((cl == LPCOUNT_REG || cl == WRITABLE_CORE_REGS)
      && in_p && MEM_P (x))
    return GENERAL_REGS;

 /* If we have a subreg (reg), where reg is a pseudo (that will end in
    a memory location), then we may need a scratch register to handle
    the fp/sp+largeoffset address.  */
  if (code == SUBREG)
    {
      rtx addr = NULL_RTX;
      x = SUBREG_REG (x);

      if (REG_P (x))
	{
	  int regno = REGNO (x);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = reg_renumber[regno];

	  if (regno != -1)
	    return NO_REGS;

	  /* It is a pseudo that ends in a stack location.  */
	  if (reg_equiv_mem (REGNO (x)))
	    {
	      /* Get the equivalent address and check the range of the
		 offset.  */
	      rtx mem = reg_equiv_mem (REGNO (x));
	      addr = find_replacement (&XEXP (mem, 0));
	    }
	}
      else
	{
	  gcc_assert (MEM_P (x));
	  addr = XEXP (x, 0);
	  addr = simplify_rtx (addr);
	}
      if (addr && GET_CODE (addr) == PLUS
	  && CONST_INT_P (XEXP (addr, 1))
	  && (!RTX_OK_FOR_OFFSET_P (mode, XEXP (addr, 1))))
	{
	  switch (mode)
	    {
	    case E_QImode:
	      sri->icode =
		in_p ? CODE_FOR_reload_qi_load : CODE_FOR_reload_qi_store;
	      break;
	    case E_HImode:
	      sri->icode =
		in_p ? CODE_FOR_reload_hi_load : CODE_FOR_reload_hi_store;
	      break;
	    default:
	      break;
	    }
	}
    }
  return NO_REGS;
}

/* Convert reloads using offsets that are too large to use indirect
   addressing.  */

void
arc_secondary_reload_conv (rtx reg, rtx mem, rtx scratch, bool store_p)
{
  rtx addr;

  gcc_assert (GET_CODE (mem) == MEM);
  addr = XEXP (mem, 0);

  /* Large offset: use a move.  FIXME: ld ops accepts limms as
     offsets.  Hence, the following move insn is not required.  */
  emit_move_insn (scratch, addr);
  mem = replace_equiv_address_nv (mem, scratch);

  /* Now create the move.  */
  if (store_p)
    emit_insn (gen_rtx_SET (mem, reg));
  else
    emit_insn (gen_rtx_SET (reg, mem));

  return;
}

static unsigned arc_ifcvt (void);

namespace {

const pass_data pass_data_arc_ifcvt =
{
  RTL_PASS,
  "arc_ifcvt",				/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  TV_IFCVT2,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_df_finish			/* todo_flags_finish */
};

class pass_arc_ifcvt : public rtl_opt_pass
{
public:
  pass_arc_ifcvt(gcc::context *ctxt)
  : rtl_opt_pass(pass_data_arc_ifcvt, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_arc_ifcvt (m_ctxt); }
  virtual unsigned int execute (function *) { return arc_ifcvt (); }
};

} // anon namespace

rtl_opt_pass *
make_pass_arc_ifcvt (gcc::context *ctxt)
{
  return new pass_arc_ifcvt (ctxt);
}

static unsigned arc_predicate_delay_insns (void);

namespace {

const pass_data pass_data_arc_predicate_delay_insns =
{
  RTL_PASS,
  "arc_predicate_delay_insns",		/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  TV_IFCVT2,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_df_finish			/* todo_flags_finish */
};

class pass_arc_predicate_delay_insns : public rtl_opt_pass
{
public:
  pass_arc_predicate_delay_insns(gcc::context *ctxt)
  : rtl_opt_pass(pass_data_arc_predicate_delay_insns, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return arc_predicate_delay_insns ();
    }
};

} // anon namespace

rtl_opt_pass *
make_pass_arc_predicate_delay_insns (gcc::context *ctxt)
{
  return new pass_arc_predicate_delay_insns (ctxt);
}

/* Called by OVERRIDE_OPTIONS to initialize various things.  */

static void
arc_init (void)
{
  if (TARGET_V2)
    {
      /* I have the multiplier, then use it*/
      if (TARGET_MPYW || TARGET_MULTI)
	  arc_multcost = COSTS_N_INSNS (1);
    }
  /* Note: arc_multcost is only used in rtx_cost if speed is true.  */
  if (arc_multcost < 0)
    switch (arc_tune)
      {
      case TUNE_ARC700_4_2_STD:
	/* latency 7;
	   max throughput (1 multiply + 4 other insns) / 5 cycles.  */
	arc_multcost = COSTS_N_INSNS (4);
	if (TARGET_NOMPY_SET)
	  arc_multcost = COSTS_N_INSNS (30);
	break;
      case TUNE_ARC700_4_2_XMAC:
	/* latency 5;
	   max throughput (1 multiply + 2 other insns) / 3 cycles.  */
	arc_multcost = COSTS_N_INSNS (3);
	if (TARGET_NOMPY_SET)
	  arc_multcost = COSTS_N_INSNS (30);
	break;
      case TUNE_ARC600:
	if (TARGET_MUL64_SET)
	  {
	    arc_multcost = COSTS_N_INSNS (4);
	    break;
	  }
	/* Fall through.  */
      default:
	arc_multcost = COSTS_N_INSNS (30);
	break;
      }

  /* MPY instructions valid only for ARC700 or ARCv2.  */
  if (TARGET_NOMPY_SET && TARGET_ARC600_FAMILY)
      error ("-mno-mpy supported only for ARC700 or ARCv2");

  if (!TARGET_DPFP && TARGET_DPFP_DISABLE_LRSR)
      error ("-mno-dpfp-lrsr supported only with -mdpfp");

  /* FPX-1. No fast and compact together.  */
  if ((TARGET_DPFP_FAST_SET && TARGET_DPFP_COMPACT_SET)
      || (TARGET_SPFP_FAST_SET && TARGET_SPFP_COMPACT_SET))
    error ("FPX fast and compact options cannot be specified together");

  /* FPX-2. No fast-spfp for arc600 or arc601.  */
  if (TARGET_SPFP_FAST_SET && TARGET_ARC600_FAMILY)
    error ("-mspfp_fast not available on ARC600 or ARC601");

  /* FPX-4.  No FPX extensions mixed with FPU extensions.  */
  if ((TARGET_DPFP_FAST_SET || TARGET_DPFP_COMPACT_SET || TARGET_SPFP)
      && TARGET_HARD_FLOAT)
    error ("No FPX/FPU mixing allowed");

  /* Warn for unimplemented PIC in pre-ARC700 cores, and disable flag_pic.  */
  if (flag_pic && TARGET_ARC600_FAMILY)
    {
      warning (DK_WARNING,
	       "PIC is not supported for %s. Generating non-PIC code only..",
	       arc_cpu_string);
      flag_pic = 0;
    }

  arc_init_reg_tables ();

  /* Initialize array for PRINT_OPERAND_PUNCT_VALID_P.  */
  memset (arc_punct_chars, 0, sizeof (arc_punct_chars));
  arc_punct_chars['#'] = 1;
  arc_punct_chars['*'] = 1;
  arc_punct_chars['?'] = 1;
  arc_punct_chars['!'] = 1;
  arc_punct_chars['^'] = 1;
  arc_punct_chars['&'] = 1;
  arc_punct_chars['+'] = 1;
  arc_punct_chars['_'] = 1;

  if (optimize > 1 && !TARGET_NO_COND_EXEC)
    {
      /* There are two target-independent ifcvt passes, and arc_reorg may do
	 one or more arc_ifcvt calls.  */
      opt_pass *pass_arc_ifcvt_4 = make_pass_arc_ifcvt (g);
      struct register_pass_info arc_ifcvt4_info
	= { pass_arc_ifcvt_4, "dbr", 1, PASS_POS_INSERT_AFTER };
      struct register_pass_info arc_ifcvt5_info
	= { pass_arc_ifcvt_4->clone (), "shorten", 1, PASS_POS_INSERT_BEFORE };

      register_pass (&arc_ifcvt4_info);
      register_pass (&arc_ifcvt5_info);
    }

  if (flag_delayed_branch)
    {
      opt_pass *pass_arc_predicate_delay_insns
	= make_pass_arc_predicate_delay_insns (g);
      struct register_pass_info arc_predicate_delay_info
	= { pass_arc_predicate_delay_insns, "dbr", 1, PASS_POS_INSERT_AFTER };

      register_pass (&arc_predicate_delay_info);
    }
}

/* Parse -mirq-ctrl-saved=RegisterRange, blink, lp_copunt.  The
   register range is specified as two registers separated by a dash.
   It always starts with r0, and its upper limit is fp register.
   blink and lp_count registers are optional.  */

static void
irq_range (const char *cstr)
{
  int i, first, last, blink, lpcount, xreg;
  char *str, *dash, *comma;

  i = strlen (cstr);
  str = (char *) alloca (i + 1);
  memcpy (str, cstr, i + 1);
  blink = -1;
  lpcount = -1;

  dash = strchr (str, '-');
  if (!dash)
    {
      warning (0, "value of -mirq-ctrl-saved must have form R0-REGx");
      return;
    }
  *dash = '\0';

  comma = strchr (dash + 1, ',');
  if (comma)
    *comma = '\0';

  first = decode_reg_name (str);
  if (first != 0)
    {
      warning (0, "first register must be R0");
      return;
    }

  /* At this moment we do not have the register names initialized
     accordingly.  */
  if (!strcmp (dash + 1, "ilink"))
    last = 29;
  else
    last = decode_reg_name (dash + 1);

  if (last < 0)
    {
      warning (0, "unknown register name: %s", dash + 1);
      return;
    }

  if (!(last & 0x01))
    {
      warning (0, "last register name %s must be an odd register", dash + 1);
      return;
    }

  *dash = '-';

  if (first > last)
    {
      warning (0, "%s-%s is an empty range", str, dash + 1);
      return;
    }

  while (comma)
    {
      *comma = ',';
      str = comma + 1;

      comma = strchr (str, ',');
      if (comma)
	*comma = '\0';

      xreg = decode_reg_name (str);
      switch (xreg)
	{
	case 31:
	  blink = 31;
	  break;

	case 60:
	  lpcount = 60;
	  break;

	default:
	  warning (0, "unknown register name: %s", str);
	  return;
	}
    }

  irq_ctrl_saved.irq_save_last_reg = last;
  irq_ctrl_saved.irq_save_blink    = (blink == 31) || (last == 31);
  irq_ctrl_saved.irq_save_lpcount  = (lpcount == 60);
}

/* Parse -mrgf-banked-regs=NUM option string.  Valid values for NUM are 4,
   8, 16, or 32.  */

static void
parse_mrgf_banked_regs_option (const char *arg)
{
  long int val;
  char *end_ptr;

  errno = 0;
  val = strtol (arg, &end_ptr, 10);
  if (errno != 0 || *arg == '\0' || *end_ptr != '\0'
      || (val != 0 && val != 4 && val != 8 && val != 16 && val != 32))
    {
      error ("invalid number in -mrgf-banked-regs=%s "
	     "valid values are 0, 4, 8, 16, or 32", arg);
      return;
    }
  rgf_banked_register_count = (int) val;
}

/* Check ARC options, generate derived target attributes.  */

static void
arc_override_options (void)
{
  unsigned int i;
  cl_deferred_option *opt;
  vec<cl_deferred_option> *vopt
    = (vec<cl_deferred_option> *) arc_deferred_options;

  if (arc_cpu == PROCESSOR_NONE)
    arc_cpu = TARGET_CPU_DEFAULT;

  /* Set the default cpu options.  */
  arc_selected_cpu = &arc_cpu_types[(int) arc_cpu];

  /* Set the architectures.  */
  switch (arc_selected_cpu->arch_info->arch_id)
    {
    case BASE_ARCH_em:
      arc_cpu_string = "EM";
      break;
    case BASE_ARCH_hs:
      arc_cpu_string = "HS";
      break;
    case BASE_ARCH_700:
      if (arc_selected_cpu->processor == PROCESSOR_nps400)
	arc_cpu_string = "NPS400";
      else
	arc_cpu_string = "ARC700";
      break;
    case BASE_ARCH_6xx:
      arc_cpu_string = "ARC600";
      break;
    default:
      gcc_unreachable ();
    }

  irq_ctrl_saved.irq_save_last_reg = -1;
  irq_ctrl_saved.irq_save_blink    = false;
  irq_ctrl_saved.irq_save_lpcount  = false;

  rgf_banked_register_count = 0;

  /* Handle the deferred options.  */
  if (vopt)
    FOR_EACH_VEC_ELT (*vopt, i, opt)
      {
	switch (opt->opt_index)
	  {
	  case OPT_mirq_ctrl_saved_:
	    if (TARGET_V2)
	      irq_range (opt->arg);
	    else
	      warning (0, "option -mirq-ctrl-saved valid only for ARC v2 processors");
	    break;

	  case OPT_mrgf_banked_regs_:
	    if (TARGET_V2)
	      parse_mrgf_banked_regs_option (opt->arg);
	    else
	      warning (0, "option -mrgf-banked-regs valid only for ARC v2 processors");
	    break;

	  default:
	    gcc_unreachable();
	  }
      }

  /* Set cpu flags accordingly to architecture/selected cpu.  The cpu
     specific flags are set in arc-common.c.  The architecture forces
     the default hardware configurations in, regardless what command
     line options are saying.  The CPU optional hw options can be
     turned on or off.  */
#define ARC_OPT(NAME, CODE, MASK, DOC)			\
  do {							\
    if ((arc_selected_cpu->flags & CODE)		\
	&& ((target_flags_explicit & MASK) == 0))	\
      target_flags |= MASK;				\
    if (arc_selected_cpu->arch_info->dflags & CODE)	\
      target_flags |= MASK;				\
  } while (0);
#define ARC_OPTX(NAME, CODE, VAR, VAL, DOC)		\
  do {							\
    if ((arc_selected_cpu->flags & CODE)		\
	&& (VAR == DEFAULT_##VAR))			\
      VAR = VAL;					\
    if (arc_selected_cpu->arch_info->dflags & CODE)	\
      VAR = VAL;					\
  } while (0);

#include "arc-options.def"

#undef ARC_OPTX
#undef ARC_OPT

  /* Check options against architecture options.  Throw an error if
     option is not allowed.  */
#define ARC_OPTX(NAME, CODE, VAR, VAL, DOC)			\
  do {								\
    if ((VAR == VAL)						\
	&& (!(arc_selected_cpu->arch_info->flags & CODE)))	\
      {								\
	error ("%s is not available for %s architecture",	\
	       DOC, arc_selected_cpu->arch_info->name);		\
      }								\
  } while (0);
#define ARC_OPT(NAME, CODE, MASK, DOC)				\
  do {								\
    if ((target_flags & MASK)					\
	&& (!(arc_selected_cpu->arch_info->flags & CODE)))	\
      error ("%s is not available for %s architecture",		\
	     DOC, arc_selected_cpu->arch_info->name);		\
  } while (0);

#include "arc-options.def"

#undef ARC_OPTX
#undef ARC_OPT

  /* Set Tune option.  */
  if (arc_tune == TUNE_NONE)
    arc_tune = (enum attr_tune) arc_selected_cpu->tune;

  if (arc_size_opt_level == 3)
    optimize_size = 1;

  /* Compact casesi is not a valid option for ARCv2 family.  */
  if (TARGET_V2)
    {
      if (TARGET_COMPACT_CASESI)
	{
	  warning (0, "compact-casesi is not applicable to ARCv2");
	  TARGET_COMPACT_CASESI = 0;
	}
    }
  else if (optimize_size == 1
	   && !global_options_set.x_TARGET_COMPACT_CASESI)
    TARGET_COMPACT_CASESI = 1;

  if (flag_pic)
    target_flags |= MASK_NO_SDATA_SET;

  if (flag_no_common == 255)
    flag_no_common = !TARGET_NO_SDATA_SET;

  /* TARGET_COMPACT_CASESI needs the "q" register class.  */
  if (TARGET_MIXED_CODE)
    TARGET_Q_CLASS = 1;
  if (!TARGET_Q_CLASS)
    TARGET_COMPACT_CASESI = 0;
  if (TARGET_COMPACT_CASESI)
    TARGET_CASE_VECTOR_PC_RELATIVE = 1;

  /* Check for small data option */
  if (!global_options_set.x_g_switch_value && !TARGET_NO_SDATA_SET)
    g_switch_value = TARGET_LL64 ? 8 : 4;

  /* These need to be done at start up.  It's convenient to do them here.  */
  arc_init ();
}

/* The condition codes of the ARC, and the inverse function.  */
/* For short branches, the "c" / "nc" names are not defined in the ARC
   Programmers manual, so we have to use "lo" / "hs"" instead.  */
static const char *arc_condition_codes[] =
{
  "al", 0, "eq", "ne", "p", "n", "lo", "hs", "v", "nv",
  "gt", "le", "ge", "lt", "hi", "ls", "pnz", 0
};

enum arc_cc_code_index
{
  ARC_CC_AL, ARC_CC_EQ = ARC_CC_AL+2, ARC_CC_NE, ARC_CC_P, ARC_CC_N,
  ARC_CC_C,  ARC_CC_NC, ARC_CC_V, ARC_CC_NV,
  ARC_CC_GT, ARC_CC_LE, ARC_CC_GE, ARC_CC_LT, ARC_CC_HI, ARC_CC_LS, ARC_CC_PNZ,
  ARC_CC_LO = ARC_CC_C, ARC_CC_HS = ARC_CC_NC
};

#define ARC_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Returns the index of the ARC condition code string in
   `arc_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

static int
get_arc_condition_code (rtx comparison)
{
  switch (GET_MODE (XEXP (comparison, 0)))
    {
    case E_CCmode:
    case E_SImode: /* For BRcc.  */
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	case GT : return ARC_CC_GT;
	case LE : return ARC_CC_LE;
	case GE : return ARC_CC_GE;
	case LT : return ARC_CC_LT;
	case GTU : return ARC_CC_HI;
	case LEU : return ARC_CC_LS;
	case LTU : return ARC_CC_LO;
	case GEU : return ARC_CC_HS;
	default : gcc_unreachable ();
	}
    case E_CC_ZNmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	case GE: return ARC_CC_P;
	case LT: return ARC_CC_N;
	case GT : return ARC_CC_PNZ;
	default : gcc_unreachable ();
	}
    case E_CC_Zmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	default : gcc_unreachable ();
	}
    case E_CC_Cmode:
      switch (GET_CODE (comparison))
	{
	case LTU : return ARC_CC_C;
	case GEU : return ARC_CC_NC;
	default : gcc_unreachable ();
	}
    case E_CC_FP_GTmode:
      if (TARGET_ARGONAUT_SET && TARGET_SPFP)
	switch (GET_CODE (comparison))
	  {
	  case GT  : return ARC_CC_N;
	  case UNLE: return ARC_CC_P;
	  default : gcc_unreachable ();
	}
      else
	switch (GET_CODE (comparison))
	  {
	  case GT   : return ARC_CC_HI;
	  case UNLE : return ARC_CC_LS;
	  default : gcc_unreachable ();
	}
    case E_CC_FP_GEmode:
      /* Same for FPX and non-FPX.  */
      switch (GET_CODE (comparison))
	{
	case GE   : return ARC_CC_HS;
	case UNLT : return ARC_CC_LO;
	default : gcc_unreachable ();
	}
    case E_CC_FP_UNEQmode:
      switch (GET_CODE (comparison))
	{
	case UNEQ : return ARC_CC_EQ;
	case LTGT : return ARC_CC_NE;
	default : gcc_unreachable ();
	}
    case E_CC_FP_ORDmode:
      switch (GET_CODE (comparison))
	{
	case UNORDERED : return ARC_CC_C;
	case ORDERED   : return ARC_CC_NC;
	default : gcc_unreachable ();
	}
    case E_CC_FPXmode:
      switch (GET_CODE (comparison))
	{
	case EQ        : return ARC_CC_EQ;
	case NE        : return ARC_CC_NE;
	case UNORDERED : return ARC_CC_C;
	case ORDERED   : return ARC_CC_NC;
	case LTGT      : return ARC_CC_HI;
	case UNEQ      : return ARC_CC_LS;
	default : gcc_unreachable ();
	}
    case E_CC_FPUmode:
      switch (GET_CODE (comparison))
	{
	case EQ	       : return ARC_CC_EQ;
	case NE	       : return ARC_CC_NE;
	case GT	       : return ARC_CC_GT;
	case GE	       : return ARC_CC_GE;
	case LT	       : return ARC_CC_C;
	case LE	       : return ARC_CC_LS;
	case UNORDERED : return ARC_CC_V;
	case ORDERED   : return ARC_CC_NV;
	case UNGT      : return ARC_CC_HI;
	case UNGE      : return ARC_CC_HS;
	case UNLT      : return ARC_CC_LT;
	case UNLE      : return ARC_CC_LE;
	  /* UNEQ and LTGT do not have representation.  */
	case LTGT      : /* Fall through.  */
	case UNEQ      : /* Fall through.  */
	default : gcc_unreachable ();
	}
    case E_CC_FPU_UNEQmode:
      switch (GET_CODE (comparison))
	{
	case LTGT : return ARC_CC_NE;
	case UNEQ : return ARC_CC_EQ;
	default : gcc_unreachable ();
	}
    default : gcc_unreachable ();
    }
  /*NOTREACHED*/
  return (42);
}

/* Return true if COMPARISON has a short form that can accomodate OFFSET.  */

bool
arc_short_comparison_p (rtx comparison, int offset)
{
  gcc_assert (ARC_CC_NC == ARC_CC_HS);
  gcc_assert (ARC_CC_C == ARC_CC_LO);
  switch (get_arc_condition_code (comparison))
    {
    case ARC_CC_EQ: case ARC_CC_NE:
      return offset >= -512 && offset <= 506;
    case ARC_CC_GT: case ARC_CC_LE: case ARC_CC_GE: case ARC_CC_LT:
    case ARC_CC_HI: case ARC_CC_LS: case ARC_CC_LO: case ARC_CC_HS:
      return offset >= -64 && offset <= 58;
    default:
      return false;
    }
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

machine_mode
arc_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  machine_mode mode = GET_MODE (x);
  rtx x1;

  /* For an operation that sets the condition codes as a side-effect, the
     C and V flags is not set as for cmp, so we can only use comparisons where
     this doesn't matter.  (For LT and GE we can use "mi" and "pl"
     instead.)  */
  /* ??? We could use "pnz" for greater than zero, however, we could then
     get into trouble because the comparison could not be reversed.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && y == const0_rtx
      && (op == EQ || op == NE
	  || ((op == LT || op == GE) && GET_MODE_SIZE (GET_MODE (x)) <= 4)))
    return CC_ZNmode;

  /* add.f for if (a+b) */
  if (mode == SImode
      && GET_CODE (y) == NEG
      && (op == EQ || op == NE))
    return CC_ZNmode;

  /* Check if this is a test suitable for bxor.f .  */
  if (mode == SImode && (op == EQ || op == NE) && CONST_INT_P (y)
      && ((INTVAL (y) - 1) & INTVAL (y)) == 0
      && INTVAL (y))
    return CC_Zmode;

  /* Check if this is a test suitable for add / bmsk.f .  */
  if (mode == SImode && (op == EQ || op == NE) && CONST_INT_P (y)
      && GET_CODE (x) == AND && CONST_INT_P ((x1 = XEXP (x, 1)))
      && ((INTVAL (x1) + 1) & INTVAL (x1)) == 0
      && (~INTVAL (x1) | INTVAL (y)) < 0
      && (~INTVAL (x1) | INTVAL (y)) > -0x800)
    return CC_Zmode;

  if (GET_MODE (x) == SImode && (op == LTU || op == GEU)
      && GET_CODE (x) == PLUS
      && (rtx_equal_p (XEXP (x, 0), y) || rtx_equal_p (XEXP (x, 1), y)))
    return CC_Cmode;

  if (TARGET_ARGONAUT_SET
      && ((mode == SFmode && TARGET_SPFP) || (mode == DFmode && TARGET_DPFP)))
    switch (op)
      {
      case EQ: case NE: case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	return CC_FPXmode;
      case LT: case UNGE: case GT: case UNLE:
	return CC_FP_GTmode;
      case LE: case UNGT: case GE: case UNLT:
	return CC_FP_GEmode;
      default: gcc_unreachable ();
      }
  else if (TARGET_HARD_FLOAT
	   && ((mode == SFmode && TARGET_FP_SP_BASE)
	       || (mode == DFmode && TARGET_FP_DP_BASE)))
    switch (op)
      {
      case EQ:
      case NE:
      case UNORDERED:
      case ORDERED:
      case UNLT:
      case UNLE:
      case UNGT:
      case UNGE:
      case LT:
      case LE:
      case GT:
      case GE:
	return CC_FPUmode;

      case LTGT:
      case UNEQ:
	return CC_FPU_UNEQmode;

      default:
	gcc_unreachable ();
      }
  else if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_OPTFPE)
    {
      switch (op)
	{
	case EQ: case NE: return CC_Zmode;
	case LT: case UNGE:
	case GT: case UNLE: return CC_FP_GTmode;
	case LE: case UNGT:
	case GE: case UNLT: return CC_FP_GEmode;
	case UNEQ: case LTGT: return CC_FP_UNEQmode;
	case ORDERED: case UNORDERED: return CC_FP_ORDmode;
	default: gcc_unreachable ();
	}
    }
  return CCmode;
}

/* Vectors to keep interesting information about registers where it can easily
   be got.  We use to use the actual mode value as the bit number, but there
   is (or may be) more than 32 modes now.  Instead we use two tables: one
   indexed by hard register number, and one indexed by mode.  */

/* The purpose of arc_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32-bit word (again).  Each real mode is
   mapped into one arc_mode_class mode.  */

enum arc_mode_class {
  C_MODE,
  S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE,
  V_MODE
};

/* Modes for condition codes.  */
#define C_MODES (1 << (int) C_MODE)

/* Modes for single-word and smaller quantities.  */
#define S_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << DF_MODE))

/* Mode for 8-byte DF values only.  */
#define DF_MODES (1 << DF_MODE)

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Modes for 128-bit vectors.  */
#define V_MODES (1 << (int) V_MODE)

/* Value is 1 if register/mode pair is acceptable on arc.  */

unsigned int arc_hard_regno_mode_ok[] = {
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, D_MODES,
  D_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,

  /* ??? Leave these as S_MODES for now.  */
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  DF_MODES, 0, DF_MODES, 0, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, C_MODES, S_MODES,

  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,

  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,
  V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES, V_MODES,

  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES, S_MODES
};

unsigned int arc_mode_class [NUM_MACHINE_MODES];

enum reg_class arc_regno_reg_class[FIRST_PSEUDO_REGISTER];

enum reg_class
arc_preferred_reload_class (rtx, enum reg_class cl)
{
  if ((cl) == CHEAP_CORE_REGS  || (cl) == WRITABLE_CORE_REGS)
    return GENERAL_REGS;
  return cl;
}

/* Initialize the arc_mode_class array.  */

static void
arc_init_reg_tables (void)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      machine_mode m = (machine_mode) i;

      switch (GET_MODE_CLASS (m))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (GET_MODE_SIZE (m) <= 4)
	    arc_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (m) == 8)
	    arc_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (m) == 16)
	    arc_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (m) == 32)
	    arc_mode_class[i] = 1 << (int) O_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (m) <= 4)
	    arc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (m) == 8)
	    arc_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (m) == 16)
	    arc_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (m) == 32)
	    arc_mode_class[i] = 1 << (int) OF_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	case MODE_VECTOR_INT:
	  if (GET_MODE_SIZE (m) == 4)
	    arc_mode_class[i] = (1 << (int) S_MODE);
	  else if (GET_MODE_SIZE (m) == 8)
	    arc_mode_class[i] = (1 << (int) D_MODE);
	  else
	    arc_mode_class[i] = (1 << (int) V_MODE);
	  break;
	case MODE_CC:
	default:
	  /* mode_class hasn't been initialized yet for EXTRA_CC_MODES, so
	     we must explicitly check for them here.  */
	  if (i == (int) CCmode || i == (int) CC_ZNmode || i == (int) CC_Zmode
	      || i == (int) CC_Cmode
	      || i == CC_FP_GTmode || i == CC_FP_GEmode || i == CC_FP_ORDmode
	      || i == CC_FPUmode || i == CC_FPU_UNEQmode)
	    arc_mode_class[i] = 1 << (int) C_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	}
    }
}

/* Core registers 56..59 are used for multiply extension options.
   The dsp option uses r56 and r57, these are then named acc1 and acc2.
   acc1 is the highpart, and acc2 the lowpart, so which register gets which
   number depends on endianness.
   The mul64 multiplier options use r57 for mlo, r58 for mmid and r59 for mhi.
   Because mlo / mhi form a 64 bit value, we use different gcc internal
   register numbers to make them form a register pair as the gcc internals
   know it.  mmid gets number 57, if still available, and mlo / mhi get
   number 58 and 59, depending on endianness.  We use DBX_REGISTER_NUMBER
   to map this back.  */
  char rname56[5] = "r56";
  char rname57[5] = "r57";
  char rname58[5] = "r58";
  char rname59[5] = "r59";
  char rname29[7] = "ilink1";
  char rname30[7] = "ilink2";

static void
arc_conditional_register_usage (void)
{
  int regno;
  int i;
  int fix_start = 60, fix_end = 55;

  if (TARGET_V2)
    {
      /* For ARCv2 the core register set is changed.  */
      strcpy (rname29, "ilink");
      strcpy (rname30, "r30");
      call_used_regs[30] = 1;
      fixed_regs[30] = 0;

      arc_regno_reg_class[30] = WRITABLE_CORE_REGS;
      SET_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], 30);
      SET_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], 30);
      SET_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], 30);
      SET_HARD_REG_BIT (reg_class_contents[MPY_WRITABLE_CORE_REGS], 30);
   }

  if (TARGET_MUL64_SET)
    {
      fix_start = 57;
      fix_end = 59;

      /* We don't provide a name for mmed.  In rtl / assembly resource lists,
	 you are supposed to refer to it as mlo & mhi, e.g
	 (zero_extract:SI (reg:DI 58) (const_int 32) (16)) .
	 In an actual asm instruction, you are of course use mmed.
	 The point of avoiding having a separate register for mmed is that
	 this way, we don't have to carry clobbers of that reg around in every
	 isntruction that modifies mlo and/or mhi.  */
      strcpy (rname57, "");
      strcpy (rname58, TARGET_BIG_ENDIAN ? "mhi" : "mlo");
      strcpy (rname59, TARGET_BIG_ENDIAN ? "mlo" : "mhi");
    }

  /* The nature of arc_tp_regno is actually something more like a global
     register, however globalize_reg requires a declaration.
     We use EPILOGUE_USES to compensate so that sets from
     __builtin_set_frame_pointer are not deleted.  */
  if (arc_tp_regno != -1)
    fixed_regs[arc_tp_regno] = call_used_regs[arc_tp_regno] = 1;

  if (TARGET_MULMAC_32BY16_SET)
    {
      fix_start = 56;
      fix_end = fix_end > 57 ? fix_end : 57;
      strcpy (rname56, TARGET_BIG_ENDIAN ? "acc1" : "acc2");
      strcpy (rname57, TARGET_BIG_ENDIAN ? "acc2" : "acc1");
    }
  for (regno = fix_start; regno <= fix_end; regno++)
    {
      if (!fixed_regs[regno])
	warning (0, "multiply option implies r%d is fixed", regno);
      fixed_regs [regno] = call_used_regs[regno] = 1;
    }
  if (TARGET_Q_CLASS)
    {
      if (optimize_size)
	{
	  reg_alloc_order[0] = 0;
	  reg_alloc_order[1] = 1;
	  reg_alloc_order[2] = 2;
	  reg_alloc_order[3] = 3;
	  reg_alloc_order[4] = 12;
	  reg_alloc_order[5] = 13;
	  reg_alloc_order[6] = 14;
	  reg_alloc_order[7] = 15;
	  reg_alloc_order[8] = 4;
	  reg_alloc_order[9] = 5;
	  reg_alloc_order[10] = 6;
	  reg_alloc_order[11] = 7;
	  reg_alloc_order[12] = 8;
	  reg_alloc_order[13] = 9;
	  reg_alloc_order[14] = 10;
	  reg_alloc_order[15] = 11;
	}
      else
	{
	  reg_alloc_order[2] = 12;
	  reg_alloc_order[3] = 13;
	  reg_alloc_order[4] = 14;
	  reg_alloc_order[5] = 15;
	  reg_alloc_order[6] = 1;
	  reg_alloc_order[7] = 0;
	  reg_alloc_order[8] = 4;
	  reg_alloc_order[9] = 5;
	  reg_alloc_order[10] = 6;
	  reg_alloc_order[11] = 7;
	  reg_alloc_order[12] = 8;
	  reg_alloc_order[13] = 9;
	  reg_alloc_order[14] = 10;
	  reg_alloc_order[15] = 11;
	}
    }
  if (TARGET_SIMD_SET)
    {
      int i;
      for (i = ARC_FIRST_SIMD_VR_REG; i <= ARC_LAST_SIMD_VR_REG; i++)
	reg_alloc_order [i] = i;
      for (i = ARC_FIRST_SIMD_DMA_CONFIG_REG;
	   i <= ARC_LAST_SIMD_DMA_CONFIG_REG; i++)
	reg_alloc_order [i] = i;
    }

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (!call_used_regs[regno])
      CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno);
  for (regno = 32; regno < 60; regno++)
    if (!fixed_regs[regno])
      SET_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], regno);
  if (!TARGET_ARC600_FAMILY)
    {
      for (regno = 32; regno <= 60; regno++)
	CLEAR_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], regno);

      /* If they have used -ffixed-lp_count, make sure it takes
	 effect.  */
      if (fixed_regs[LP_COUNT])
	{
	  CLEAR_HARD_REG_BIT (reg_class_contents[LPCOUNT_REG], LP_COUNT);
	  CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], LP_COUNT);
	  CLEAR_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], LP_COUNT);

	  /* Instead of taking out SF_MODE like below, forbid it outright.  */
	  arc_hard_regno_mode_ok[60] = 0;
	}
      else
	arc_hard_regno_mode_ok[60] = 1 << (int) S_MODE;
    }

  /* ARCHS has 64-bit data-path which makes use of the even-odd paired
     registers.  */
  if (TARGET_HS)
    {
      for (regno = 1; regno < 32; regno +=2)
	{
	  arc_hard_regno_mode_ok[regno] = S_MODES;
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i < 29)
	{
	  if ((TARGET_Q_CLASS || TARGET_RRQ_CLASS)
	      && ((i <= 3) || ((i >= 12) && (i <= 15))))
	    arc_regno_reg_class[i] = ARCOMPACT16_REGS;
	  else
	    arc_regno_reg_class[i] = GENERAL_REGS;
	}
      else if (i < 60)
	arc_regno_reg_class[i]
	  = (fixed_regs[i]
	     ? (TEST_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], i)
		? CHEAP_CORE_REGS : ALL_CORE_REGS)
	     : (((!TARGET_ARC600_FAMILY)
		 && TEST_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], i))
		? CHEAP_CORE_REGS : WRITABLE_CORE_REGS));
      else
	arc_regno_reg_class[i] = NO_REGS;
    }

  /* ARCOMPACT16_REGS is empty, if TARGET_Q_CLASS / TARGET_RRQ_CLASS
     has not been activated.  */
  if (!TARGET_Q_CLASS && !TARGET_RRQ_CLASS)
    CLEAR_HARD_REG_SET(reg_class_contents [ARCOMPACT16_REGS]);
  if (!TARGET_Q_CLASS)
    CLEAR_HARD_REG_SET(reg_class_contents [AC16_BASE_REGS]);

  gcc_assert (FIRST_PSEUDO_REGISTER >= 144);

  /* Handle Special Registers.  */
  arc_regno_reg_class[29] = LINK_REGS; /* ilink1 register.  */
  if (!TARGET_V2)
    arc_regno_reg_class[30] = LINK_REGS; /* ilink2 register.  */
  arc_regno_reg_class[31] = LINK_REGS; /* blink register.  */
  arc_regno_reg_class[60] = LPCOUNT_REG;
  arc_regno_reg_class[61] = NO_REGS;      /* CC_REG: must be NO_REGS.  */
  arc_regno_reg_class[62] = GENERAL_REGS;

  if (TARGET_DPFP)
    {
      for (i = 40; i < 44; ++i)
	{
	  arc_regno_reg_class[i] = DOUBLE_REGS;

	  /* Unless they want us to do 'mov d1, 0x00000000' make sure
	     no attempt is made to use such a register as a destination
	     operand in *movdf_insn.  */
	  if (!TARGET_ARGONAUT_SET)
	    {
	    /* Make sure no 'c', 'w', 'W', or 'Rac' constraint is
	       interpreted to mean they can use D1 or D2 in their insn.  */
	    CLEAR_HARD_REG_BIT(reg_class_contents[CHEAP_CORE_REGS       ], i);
	    CLEAR_HARD_REG_BIT(reg_class_contents[ALL_CORE_REGS         ], i);
	    CLEAR_HARD_REG_BIT(reg_class_contents[WRITABLE_CORE_REGS    ], i);
	    CLEAR_HARD_REG_BIT(reg_class_contents[MPY_WRITABLE_CORE_REGS], i);
	    }
	}
    }
  else
    {
      /* Disable all DOUBLE_REGISTER settings,
	 if not generating DPFP code.  */
      arc_regno_reg_class[40] = ALL_REGS;
      arc_regno_reg_class[41] = ALL_REGS;
      arc_regno_reg_class[42] = ALL_REGS;
      arc_regno_reg_class[43] = ALL_REGS;

      fixed_regs[40] = 1;
      fixed_regs[41] = 1;
      fixed_regs[42] = 1;
      fixed_regs[43] = 1;

      arc_hard_regno_mode_ok[40] = 0;
      arc_hard_regno_mode_ok[42] = 0;

      CLEAR_HARD_REG_SET(reg_class_contents [DOUBLE_REGS]);
    }

  if (TARGET_SIMD_SET)
    {
      gcc_assert (ARC_FIRST_SIMD_VR_REG == 64);
      gcc_assert (ARC_LAST_SIMD_VR_REG  == 127);

      for (i = ARC_FIRST_SIMD_VR_REG; i <= ARC_LAST_SIMD_VR_REG; i++)
	arc_regno_reg_class [i] =  SIMD_VR_REGS;

      gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_REG == 128);
      gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_IN_REG == 128);
      gcc_assert (ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG == 136);
      gcc_assert (ARC_LAST_SIMD_DMA_CONFIG_REG  == 143);

      for (i = ARC_FIRST_SIMD_DMA_CONFIG_REG;
	   i <= ARC_LAST_SIMD_DMA_CONFIG_REG; i++)
	arc_regno_reg_class [i] =  SIMD_DMA_CONFIG_REGS;
    }

  /* pc : r63 */
  arc_regno_reg_class[PROGRAM_COUNTER_REGNO] = GENERAL_REGS;

  /*ARCV2 Accumulator.  */
  if ((TARGET_V2
       && (TARGET_FP_DP_FUSED || TARGET_FP_SP_FUSED))
      || TARGET_PLUS_DMPY)
  {
    arc_regno_reg_class[ACCL_REGNO] = WRITABLE_CORE_REGS;
    arc_regno_reg_class[ACCH_REGNO] = WRITABLE_CORE_REGS;
    SET_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], ACCL_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], ACCH_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], ACCL_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], ACCH_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], ACCL_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], ACCH_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[MPY_WRITABLE_CORE_REGS], ACCL_REGNO);
    SET_HARD_REG_BIT (reg_class_contents[MPY_WRITABLE_CORE_REGS], ACCH_REGNO);

     /* Allow the compiler to freely use them.  */
    fixed_regs[ACCL_REGNO] = 0;
    fixed_regs[ACCH_REGNO] = 0;

    arc_hard_regno_mode_ok[ACC_REG_FIRST] = D_MODES;
  }
}

/* Handle an "interrupt" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
arc_handle_interrupt_attribute (tree *, tree name, tree args, int,
				bool *no_add_attrs)
{
  gcc_assert (args);

  tree value = TREE_VALUE (args);

  if (TREE_CODE (value) != STRING_CST)
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not a string constant",
	       name);
      *no_add_attrs = true;
    }
  else if (!TARGET_V2
	   && strcmp (TREE_STRING_POINTER (value), "ilink1")
	   && strcmp (TREE_STRING_POINTER (value), "ilink2"))
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not \"ilink1\" or \"ilink2\"",
	       name);
      *no_add_attrs = true;
    }
  else if (TARGET_V2
	   && strcmp (TREE_STRING_POINTER (value), "ilink")
	   && strcmp (TREE_STRING_POINTER (value), "firq"))
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not \"ilink\" or \"firq\"",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
arc_handle_fndecl_attribute (tree *node, tree name, tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Implement `TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS' */

static bool
arc_allocate_stack_slots_for_args (void)
{
  /* Naked functions should not allocate stack slots for arguments.  */
  unsigned int fn_type = arc_compute_function_type (cfun);

  return !ARC_NAKED_P(fn_type);
}

/* Implement `TARGET_WARN_FUNC_RETURN'.  */

static bool
arc_warn_func_return (tree decl)
{
  struct function *func = DECL_STRUCT_FUNCTION (decl);
  unsigned int fn_type = arc_compute_function_type (func);

  return !ARC_NAKED_P (fn_type);
}

/* Return zero if TYPE1 and TYPE are incompatible, one if they are compatible,
   and two if they are nearly compatible (which causes a warning to be
   generated).  */

static int
arc_comp_type_attributes (const_tree type1,
			  const_tree type2)
{
  int l1, l2, m1, m2, s1, s2;

  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2)) != NULL;
  m1 = lookup_attribute ("medium_call", TYPE_ATTRIBUTES (type1)) != NULL;
  m2 = lookup_attribute ("medium_call", TYPE_ATTRIBUTES (type2)) != NULL;
  s1 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1)) != NULL;
  s2 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2)) != NULL;

  /* Only bother to check if an attribute is defined.  */
  if (l1 | l2 | m1 | m2 | s1 | s2)
    {
      /* If one type has an attribute, the other must have the same attribute.  */
      if ((l1 != l2) || (m1 != m2) || (s1 != s2))
	return 0;

      /* Disallow mixed attributes.  */
      if (l1 + m1 + s1 > 1)
	return 0;
    }


  return 1;
}

/* Set the default attributes for TYPE.  */

void
arc_set_default_type_attributes (tree type ATTRIBUTE_UNUSED)
{
  gcc_unreachable();
}

/* Misc. utilities.  */

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for the cc reg in the proper mode.  */

rtx
gen_compare_reg (rtx comparison, machine_mode omode)
{
  enum rtx_code code = GET_CODE (comparison);
  rtx x = XEXP (comparison, 0);
  rtx y = XEXP (comparison, 1);
  rtx tmp, cc_reg;
  machine_mode mode, cmode;


  cmode = GET_MODE (x);
  if (cmode == VOIDmode)
    cmode = GET_MODE (y);
  gcc_assert (cmode == SImode || cmode == SFmode || cmode == DFmode);
  if (cmode == SImode)
    {
      if (!register_operand (x, SImode))
	{
	  if (register_operand (y, SImode))
	    {
	      tmp = x;
	      x = y;
	      y = tmp;
	      code = swap_condition (code);
	    }
	  else
	    x = copy_to_mode_reg (SImode, x);
	}
      if (GET_CODE (y) == SYMBOL_REF && flag_pic)
	y = copy_to_mode_reg (SImode, y);
    }
  else
    {
      x = force_reg (cmode, x);
      y = force_reg (cmode, y);
    }
  mode = SELECT_CC_MODE (code, x, y);

  cc_reg = gen_rtx_REG (mode, CC_REG);

  /* ??? FIXME (x-y)==0, as done by both cmpsfpx_raw and
     cmpdfpx_raw, is not a correct comparison for floats:
        http://www.cygnus-software.com/papers/comparingfloats/comparingfloats.htm
   */
  if (TARGET_ARGONAUT_SET
      && ((cmode == SFmode && TARGET_SPFP) || (cmode == DFmode && TARGET_DPFP)))
    {
      switch (code)
	{
	case NE: case EQ: case LT: case UNGE: case LE: case UNGT:
	case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	  break;
	case GT: case UNLE: case GE: case UNLT:
	  code = swap_condition (code);
	  tmp = x;
	  x = y;
	  y = tmp;
	  break;
	default:
	  gcc_unreachable ();
	}
      if (cmode == SFmode)
      {
	emit_insn (gen_cmpsfpx_raw (x, y));
      }
      else /* DFmode */
      {
	/* Accepts Dx regs directly by insns.  */
	emit_insn (gen_cmpdfpx_raw (x, y));
      }

      if (mode != CC_FPXmode)
	emit_insn (gen_rtx_SET (cc_reg,
				gen_rtx_COMPARE (mode,
						 gen_rtx_REG (CC_FPXmode, 61),
						 const0_rtx)));
    }
  else if (TARGET_FPX_QUARK && (cmode == SFmode))
    {
      switch (code)
	{
	case NE: case EQ: case GT: case UNLE: case GE: case UNLT:
	case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	  break;
	case LT: case UNGE: case LE: case UNGT:
	  code = swap_condition (code);
	  tmp = x;
	  x = y;
	  y = tmp;
	  break;
	default:
	  gcc_unreachable ();
	}

      emit_insn (gen_cmp_quark (cc_reg,
				gen_rtx_COMPARE (mode, x, y)));
    }
  else if (TARGET_HARD_FLOAT
	   && ((cmode == SFmode && TARGET_FP_SP_BASE)
	       || (cmode == DFmode && TARGET_FP_DP_BASE)))
    emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, x, y)));
  else if (GET_MODE_CLASS (cmode) == MODE_FLOAT && TARGET_OPTFPE)
    {
      rtx op0 = gen_rtx_REG (cmode, 0);
      rtx op1 = gen_rtx_REG (cmode, GET_MODE_SIZE (cmode) / UNITS_PER_WORD);
      bool swap = false;

      switch (code)
	{
	case NE: case EQ: case GT: case UNLE: case GE: case UNLT:
	case UNEQ: case LTGT: case ORDERED: case UNORDERED:
	  break;
	case LT: case UNGE: case LE: case UNGT:
	  code = swap_condition (code);
	  swap = true;
	  break;
	default:
	  gcc_unreachable ();
	}
      if (currently_expanding_to_rtl)
	{
	  if (swap)
	    {
	      tmp = x;
	      x = y;
	      y = tmp;
	    }
	  emit_move_insn (op0, x);
	  emit_move_insn (op1, y);
	}
      else
	{
	  gcc_assert (rtx_equal_p (op0, x));
	  gcc_assert (rtx_equal_p (op1, y));
	  if (swap)
	    {
	      op0 = y;
	      op1 = x;
	    }
	}
      emit_insn (gen_cmp_float (cc_reg, gen_rtx_COMPARE (mode, op0, op1)));
    }
  else
    emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, x, y)));
  return gen_rtx_fmt_ee (code, omode, cc_reg, const0_rtx);
}

/* Return true if VALUE, a const_double, will fit in a limm (4 byte number).
   We assume the value can be either signed or unsigned.  */

bool
arc_double_limm_p (rtx value)
{
  HOST_WIDE_INT low, high;

  gcc_assert (GET_CODE (value) == CONST_DOUBLE);

  if (TARGET_DPFP)
    return true;

  low = CONST_DOUBLE_LOW (value);
  high = CONST_DOUBLE_HIGH (value);

  if (low & 0x80000000)
    {
      return (((unsigned HOST_WIDE_INT) low <= 0xffffffff && high == 0)
	      || (((low & - (unsigned HOST_WIDE_INT) 0x80000000)
		   == - (unsigned HOST_WIDE_INT) 0x80000000)
		  && high == -1));
    }
  else
    {
      return (unsigned HOST_WIDE_INT) low <= 0x7fffffff && high == 0;
    }
}

/* Do any needed setup for a variadic function.  For the ARC, we must
   create a register parameter block, and then copy any anonymous arguments
   in registers to memory.

   CUM has not been updated for the last named argument which has type TYPE
   and mode MODE, and we rely on this fact.  */

static void
arc_setup_incoming_varargs (cumulative_args_t args_so_far,
			    machine_mode mode, tree type,
			    int *pretend_size, int no_rtl)
{
  int first_anon_arg;
  CUMULATIVE_ARGS next_cum;

  /* We must treat `__builtin_va_alist' as an anonymous arg.  */

  next_cum = *get_cumulative_args (args_so_far);
  arc_function_arg_advance (pack_cumulative_args (&next_cum),
			    mode, type, true);
  first_anon_arg = next_cum;

  if (FUNCTION_ARG_REGNO_P (first_anon_arg))
    {
      /* First anonymous (unnamed) argument is in a reg.  */

      /* Note that first_reg_offset < MAX_ARC_PARM_REGS.  */
      int first_reg_offset = first_anon_arg;

      if (!no_rtl)
	{
	  rtx regblock
	    = gen_rtx_MEM (BLKmode, plus_constant (Pmode, arg_pointer_rtx,
			   FIRST_PARM_OFFSET (0)));
	  move_block_from_reg (first_reg_offset, regblock,
			       MAX_ARC_PARM_REGS - first_reg_offset);
	}

      *pretend_size
	= ((MAX_ARC_PARM_REGS - first_reg_offset ) * UNITS_PER_WORD);
    }
}

/* Cost functions.  */

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */

int
arc_address_cost (rtx addr, machine_mode, addr_space_t, bool speed)
{
  switch (GET_CODE (addr))
    {
    case REG :
      return speed || satisfies_constraint_Rcq (addr) ? 0 : 1;
    case PRE_INC: case PRE_DEC: case POST_INC: case POST_DEC:
    case PRE_MODIFY: case POST_MODIFY:
      return !speed;

    case LABEL_REF :
    case SYMBOL_REF :
    case CONST :
      if (TARGET_NPS_CMEM && cmem_address (addr, SImode))
	return 0;
      /* Most likely needs a LIMM.  */
      return COSTS_N_INSNS (1);

    case PLUS :
      {
	register rtx plus0 = XEXP (addr, 0);
	register rtx plus1 = XEXP (addr, 1);

	if (GET_CODE (plus0) != REG
	    && (GET_CODE (plus0) != MULT
		|| !CONST_INT_P (XEXP (plus0, 1))
		|| (INTVAL (XEXP (plus0, 1)) != 2
		    && INTVAL (XEXP (plus0, 1)) != 4)))
	  break;

	switch (GET_CODE (plus1))
	  {
	  case CONST_INT :
	    return (!RTX_OK_FOR_OFFSET_P (SImode, plus1)
		    ? COSTS_N_INSNS (1)
		    : speed
		    ? 0
		    : (satisfies_constraint_Rcq (plus0)
		       && satisfies_constraint_O (plus1))
		    ? 0
		    : 1);
	  case REG:
	    return (speed < 1 ? 0
		    : (satisfies_constraint_Rcq (plus0)
		       && satisfies_constraint_Rcq (plus1))
		    ? 0 : 1);
	  case CONST :
	  case SYMBOL_REF :
	  case LABEL_REF :
	    return COSTS_N_INSNS (1);
	  default:
	    break;
	  }
	break;
      }
    default:
      break;
    }

  return 4;
}

/* Emit instruction X with the frame related bit set.  */

static rtx
frame_insn (rtx x)
{
  x = emit_insn (x);
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* Emit a frame insn to move SRC to DST.  */

static rtx
frame_move (rtx dst, rtx src)
{
  rtx tmp = gen_rtx_SET (dst, src);
  RTX_FRAME_RELATED_P (tmp) = 1;
  return frame_insn (tmp);
}

/* Like frame_move, but add a REG_INC note for REG if ADDR contains an
   auto increment address, or is zero.  */

static rtx
frame_move_inc (rtx dst, rtx src, rtx reg, rtx addr)
{
  rtx insn = frame_move (dst, src);

  if (!addr
      || GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == POST_INC
      || GET_CODE (addr) == PRE_MODIFY || GET_CODE (addr) == POST_MODIFY)
    add_reg_note (insn, REG_INC, reg);
  return insn;
}

/* Emit a frame insn which adjusts a frame address register REG by OFFSET.  */

static rtx
frame_add (rtx reg, HOST_WIDE_INT offset)
{
  gcc_assert ((offset & 0x3) == 0);
  if (!offset)
    return NULL_RTX;
  return frame_move (reg, plus_constant (Pmode, reg, offset));
}

/* Emit a frame insn which adjusts stack pointer by OFFSET.  */

static rtx
frame_stack_add (HOST_WIDE_INT offset)
{
  return frame_add (stack_pointer_rtx, offset);
}

/* Traditionally, we push saved registers first in the prologue,
   then we allocate the rest of the frame - and reverse in the epilogue.
   This has still its merits for ease of debugging, or saving code size
   or even execution time if the stack frame is so large that some accesses
   can't be encoded anymore with offsets in the instruction code when using
   a different scheme.
   Also, it would be a good starting point if we got instructions to help
   with register save/restore.

   However, often stack frames are small, and the pushing / popping has
   some costs:
   - the stack modification prevents a lot of scheduling.
   - frame allocation / deallocation needs extra instructions.
   - unless we know that we compile ARC700 user code, we need to put
     a memory barrier after frame allocation / before deallocation to
     prevent interrupts clobbering our data in the frame.
     In particular, we don't have any such guarantees for library functions,
     which tend to, on the other hand, to have small frames.

   Thus, for small frames, we'd like to use a different scheme:
   - The frame is allocated in full with the first prologue instruction,
     and deallocated in full with the last epilogue instruction.
     Thus, the instructions in-betwen can be freely scheduled.
   - If the function has no outgoing arguments on the stack, we can allocate
     one register save slot at the top of the stack.  This register can then
     be saved simultanously with frame allocation, and restored with
     frame deallocation.
     This register can be picked depending on scheduling considerations,
     although same though should go into having some set of registers
     to be potentially lingering after a call, and others to be available
     immediately - i.e. in the absence of interprocedual optimization, we
     can use an ABI-like convention for register allocation to reduce
     stalls after function return.  */
/* Function prologue/epilogue handlers.  */

/* ARCompact stack frames look like:

           Before call                     After call
  high  +-----------------------+       +-----------------------+
  mem   |  reg parm save area   |       | reg parm save area    |
        |  only created for     |       | only created for      |
        |  variable arg fns     |       | variable arg fns      |
    AP  +-----------------------+       +-----------------------+
        |  return addr register |       | return addr register  |
        |  (if required)        |       | (if required)         |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  reg save area        |       | reg save area         |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |  frame pointer        |       | frame pointer         |
        |  (if required)        |       | (if required)         |
    FP  +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  local/temp variables |       | local/temp variables  |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  arguments on stack   |       | arguments on stack    |
        |                       |       |                       |
    SP  +-----------------------+       +-----------------------+
                                        | reg parm save area    |
                                        | only created for      |
                                        | variable arg fns      |
                                    AP  +-----------------------+
                                        | return addr register  |
                                        | (if required)         |
                                        +-----------------------+
                                        |                       |
                                        | reg save area         |
                                        |                       |
                                        +-----------------------+
                                        | frame pointer         |
                                        | (if required)         |
                                    FP  +-----------------------+
                                        |                       |
                                        | local/temp variables  |
                                        |                       |
                                        +-----------------------+
                                        |                       |
                                        | arguments on stack    |
  low                                   |                       |
  mem                               SP  +-----------------------+

Notes:
1) The "reg parm save area" does not exist for non variable argument fns.
   The "reg parm save area" can be eliminated completely if we created our
   own va-arc.h, but that has tradeoffs as well (so it's not done).  */

/* Structure to be filled in by arc_compute_frame_size with register
   save masks, and offsets for the current function.  */
struct GTY (()) arc_frame_info
{
  unsigned int total_size;	/* # bytes that the entire frame takes up.  */
  unsigned int extra_size;	/* # bytes of extra stuff.  */
  unsigned int pretend_size;	/* # bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # bytes needed to store regs.  */
  unsigned int var_size;	/* # bytes that variables take up.  */
  unsigned int reg_offset;	/* Offset from new sp to store regs.  */
  unsigned int gmask;		/* Mask of saved gp registers.  */
  int          initialized;	/* Nonzero if frame size already calculated.  */
  short millicode_start_reg;
  short millicode_end_reg;
  bool save_return_addr;
};

/* Defining data structures for per-function information.  */

typedef struct GTY (()) machine_function
{
  unsigned int fn_type;
  struct arc_frame_info frame_info;
  /* To keep track of unalignment caused by short insns.  */
  int unalign;
  int force_short_suffix; /* Used when disgorging return delay slot insns.  */
  const char *size_reason;
  struct arc_ccfsm ccfsm_current;
  /* Map from uid to ccfsm state during branch shortening.  */
  rtx ccfsm_current_insn;
  char arc_reorg_started;
  char prescan_initialized;
} machine_function;

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

unsigned int
arc_compute_function_type (struct function *fun)
{
  tree attr, decl = fun->decl;
  unsigned int fn_type = fun->machine->fn_type;

  if (fn_type != ARC_FUNCTION_UNKNOWN)
    return fn_type;

  /* Check if it is a naked function.  */
  if (lookup_attribute ("naked", DECL_ATTRIBUTES (decl)) != NULL_TREE)
    fn_type |= ARC_FUNCTION_NAKED;
  else
    fn_type |= ARC_FUNCTION_NORMAL;

  /* Now see if this is an interrupt handler.  */
  attr = lookup_attribute ("interrupt", DECL_ATTRIBUTES (decl));
  if (attr != NULL_TREE)
    {
      tree value, args = TREE_VALUE (attr);

      gcc_assert (list_length (args) == 1);
      value = TREE_VALUE (args);
      gcc_assert (TREE_CODE (value) == STRING_CST);

      if (!strcmp (TREE_STRING_POINTER (value), "ilink1")
	  || !strcmp (TREE_STRING_POINTER (value), "ilink"))
	fn_type |= ARC_FUNCTION_ILINK1;
      else if (!strcmp (TREE_STRING_POINTER (value), "ilink2"))
	fn_type |= ARC_FUNCTION_ILINK2;
      else if (!strcmp (TREE_STRING_POINTER (value), "firq"))
	fn_type |= ARC_FUNCTION_FIRQ;
      else
	gcc_unreachable ();
    }

  return fun->machine->fn_type = fn_type;
}

#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK (1 << (RETURN_ADDR_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.
   Addition for pic: The gp register needs to be saved if the current
   function changes it to access gotoff variables.
   FIXME: This will not be needed if we used some arbitrary register
   instead of r26.
*/

static bool
arc_must_save_register (int regno, struct function *func)
{
  unsigned int fn_type = arc_compute_function_type (func);
  bool irq_auto_save_p = ((irq_ctrl_saved.irq_save_last_reg >= regno)
			  && ARC_AUTO_IRQ_P (fn_type));
  bool firq_auto_save_p = ARC_FAST_INTERRUPT_P (fn_type);

  switch (rgf_banked_register_count)
    {
    case 4:
      firq_auto_save_p &= (regno < 4);
      break;
    case 8:
      firq_auto_save_p &= ((regno < 4) || ((regno > 11) && (regno < 16)));
      break;
    case 16:
      firq_auto_save_p &= ((regno < 4) || ((regno > 9) && (regno < 16))
			   || ((regno > 25) && (regno < 29))
			   || ((regno > 29) && (regno < 32)));
      break;
    case 32:
      firq_auto_save_p &= (regno != 29) && (regno < 32);
      break;
    default:
      firq_auto_save_p = false;
      break;
    }

  if ((regno) != RETURN_ADDR_REGNUM
      && (regno) != FRAME_POINTER_REGNUM
      && df_regs_ever_live_p (regno)
      && (!call_used_regs[regno]
	  || ARC_INTERRUPT_P (fn_type))
      /* Do not emit code for auto saved regs.  */
      && !irq_auto_save_p
      && !firq_auto_save_p)
    return true;

  if (flag_pic && crtl->uses_pic_offset_table
      && regno == PIC_OFFSET_TABLE_REGNUM)
    return true;

  return false;
}

/* Return true if the return address must be saved in the current function,
   otherwise return false.  */

static bool
arc_must_save_return_addr (struct function *func)
{
  if (func->machine->frame_info.save_return_addr)
    return true;

  return false;
}

/* Helper function to wrap FRAME_POINTER_NEEDED.  We do this as
   FRAME_POINTER_NEEDED will not be true until the IRA (Integrated
   Register Allocator) pass, while we want to get the frame size
   correct earlier than the IRA pass.  */
static bool
arc_frame_pointer_needed (void)
{
  return (frame_pointer_needed);
}


/* Return non-zero if there are registers to be saved or loaded using
   millicode thunks.  We can only use consecutive sequences starting
   with r13, and not going beyond r25.
   GMASK is a bitmask of registers to save.  This function sets
   FRAME->millicod_start_reg .. FRAME->millicode_end_reg to the range
   of registers to be saved / restored with a millicode call.  */

static int
arc_compute_millicode_save_restore_regs (unsigned int gmask,
					 struct arc_frame_info *frame)
{
  int regno;

  int start_reg = 13, end_reg = 25;

  for (regno = start_reg; regno <= end_reg && (gmask & (1L << regno));)
    regno++;
  end_reg = regno - 1;
  /* There is no point in using millicode thunks if we don't save/restore
     at least three registers.  For non-leaf functions we also have the
     blink restore.  */
  if (regno - start_reg >= 3 - (crtl->is_leaf == 0))
    {
      frame->millicode_start_reg = 13;
      frame->millicode_end_reg = regno - 1;
      return 1;
    }
  return 0;
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

unsigned int
arc_compute_frame_size (int size)	/* size = # of var. bytes allocated.  */
{
  int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size, reg_offset;
  unsigned int gmask;
  struct arc_frame_info *frame_info = &cfun->machine->frame_info;

  size = ARC_STACK_ALIGN (size);

  /* 1) Size of locals and temporaries */
  var_size	= size;

  /* 2) Size of outgoing arguments */
  args_size	= crtl->outgoing_args_size;

  /* 3) Calculate space needed for saved registers.
     ??? We ignore the extension registers for now.  */

  /* See if this is an interrupt handler.  Call used registers must be saved
     for them too.  */

  reg_size = 0;
  gmask = 0;

  for (regno = 0; regno <= 31; regno++)
    {
      if (arc_must_save_register (regno, cfun))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1L << regno;
	}
    }

  /* 4) Space for back trace data structure.
	<return addr reg size> (if required) + <fp size> (if required).  */
  frame_info->save_return_addr
    = (!crtl->is_leaf || df_regs_ever_live_p (RETURN_ADDR_REGNUM));
  /* Saving blink reg in case of leaf function for millicode thunk calls.  */
  if (optimize_size && !TARGET_NO_MILLICODE_THUNK_SET)
    {
      if (arc_compute_millicode_save_restore_regs (gmask, frame_info))
	frame_info->save_return_addr = true;
    }

  extra_size = 0;
  if (arc_must_save_return_addr (cfun))
    extra_size = 4;
  if (arc_frame_pointer_needed ())
    extra_size += 4;

  /* 5) Space for variable arguments passed in registers */
  pretend_size	= crtl->args.pretend_args_size;

  /* Ensure everything before the locals is aligned appropriately.  */
    {
       unsigned int extra_plus_reg_size;
       unsigned int extra_plus_reg_size_aligned;

       extra_plus_reg_size = extra_size + reg_size;
       extra_plus_reg_size_aligned = ARC_STACK_ALIGN(extra_plus_reg_size);
       reg_size = extra_plus_reg_size_aligned - extra_size;
    }

  /* Compute total frame size.  */
  total_size = var_size + args_size + extra_size + pretend_size + reg_size;

  total_size = ARC_STACK_ALIGN (total_size);

  /* Compute offset of register save area from stack pointer:
     Frame: pretend_size <blink> reg_size <fp> var_size args_size <--sp
  */
  reg_offset = (total_size - (pretend_size + reg_size + extra_size)
		+ (arc_frame_pointer_needed () ? 4 : 0));

  /* Save computed information.  */
  frame_info->total_size   = total_size;
  frame_info->extra_size   = extra_size;
  frame_info->pretend_size = pretend_size;
  frame_info->var_size     = var_size;
  frame_info->args_size    = args_size;
  frame_info->reg_size     = reg_size;
  frame_info->reg_offset   = reg_offset;
  frame_info->gmask        = gmask;
  frame_info->initialized  = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

/* Common code to save/restore registers.  */
/* BASE_REG is the base register to use for addressing and to adjust.
   GMASK is a bitmask of general purpose registers to save/restore.
   epilogue_p 0: prologue 1:epilogue 2:epilogue, sibling thunk
   If *FIRST_OFFSET is non-zero, add it first to BASE_REG - preferably
   using a pre-modify for the first memory access.  *FIRST_OFFSET is then
   zeroed.  */

static void
arc_save_restore (rtx base_reg,
		  unsigned int gmask, int epilogue_p, int *first_offset)
{
  unsigned int offset = 0;
  int regno;
  struct arc_frame_info *frame = &cfun->machine->frame_info;
  rtx sibthunk_insn = NULL_RTX;

  if (gmask)
    {
      /* Millicode thunks implementation:
	 Generates calls to millicodes for registers starting from r13 to r25
	 Present Limitations:
	 - Only one range supported. The remaining regs will have the ordinary
	   st and ld instructions for store and loads. Hence a gmask asking
	   to store r13-14, r16-r25 will only generate calls to store and
	   load r13 to r14 while store and load insns will be generated for
	   r16 to r25 in the prologue and epilogue respectively.

	 - Presently library only supports register ranges starting from r13.
      */
      if (epilogue_p == 2 || frame->millicode_end_reg > 14)
	{
	  int start_call = frame->millicode_start_reg;
	  int end_call = frame->millicode_end_reg;
	  int n_regs = end_call - start_call + 1;
	  int i = 0, r, off = 0;
	  rtx insn;
	  rtx ret_addr = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);

	  if (*first_offset)
	    {
	      /* "reg_size" won't be more than 127 .  */
	      gcc_assert (epilogue_p || abs (*first_offset) <= 127);
	      frame_add (base_reg, *first_offset);
	      *first_offset = 0;
	    }
	  insn = gen_rtx_PARALLEL
		  (VOIDmode, rtvec_alloc ((epilogue_p == 2) + n_regs + 1));
	  if (epilogue_p == 2)
	    i += 2;
	  else
	    XVECEXP (insn, 0, n_regs) = gen_rtx_CLOBBER (VOIDmode, ret_addr);
	  for (r = start_call; r <= end_call; r++, off += UNITS_PER_WORD, i++)
	    {
	      rtx reg = gen_rtx_REG (SImode, r);
	      rtx mem
		= gen_frame_mem (SImode, plus_constant (Pmode, base_reg, off));

	      if (epilogue_p)
		XVECEXP (insn, 0, i) = gen_rtx_SET (reg, mem);
	      else
		XVECEXP (insn, 0, i) = gen_rtx_SET (mem, reg);
	      gmask = gmask & ~(1L << r);
	    }
	  if (epilogue_p == 2)
	    sibthunk_insn = insn;
	  else
	    {
	      insn = frame_insn (insn);
	      if (epilogue_p)
		for (r = start_call; r <= end_call; r++)
		  {
		    rtx reg = gen_rtx_REG (SImode, r);
		    add_reg_note (insn, REG_CFA_RESTORE, reg);
		  }
	    }
	  offset += off;
	}

      for (regno = 0; regno <= 31; regno++)
	{
	  machine_mode mode = SImode;
	  bool found = false;

	  if (TARGET_LL64
	      && (regno % 2 == 0)
	      && ((gmask & (1L << regno)) != 0)
	      && ((gmask & (1L << (regno+1))) != 0))
	    {
	      found = true;
	      mode  = DImode;
	    }
	  else if ((gmask & (1L << regno)) != 0)
	    {
	      found = true;
	      mode  = SImode;
	    }

	  if (found)
	    {
	      rtx reg = gen_rtx_REG (mode, regno);
	      rtx addr, mem;
	      int cfa_adjust = *first_offset;

	      if (*first_offset)
		{
		  gcc_assert (!offset);
		  addr = plus_constant (Pmode, base_reg, *first_offset);
		  addr = gen_rtx_PRE_MODIFY (Pmode, base_reg, addr);
		  *first_offset = 0;
		}
	      else
		{
		  gcc_assert (SMALL_INT (offset));
		  addr = plus_constant (Pmode, base_reg, offset);
		}
	      mem = gen_frame_mem (mode, addr);
	      if (epilogue_p)
		{
		  rtx insn =
		    frame_move_inc (reg, mem, base_reg, addr);
		  add_reg_note (insn, REG_CFA_RESTORE, reg);
		  if (cfa_adjust)
		    {
		      enum reg_note note = REG_CFA_ADJUST_CFA;
		      add_reg_note (insn, note,
				    gen_rtx_SET (stack_pointer_rtx,
						 plus_constant (Pmode,
								stack_pointer_rtx,
								cfa_adjust)));
		    }
		}
	      else
		frame_move_inc (mem, reg, base_reg, addr);
	      offset += UNITS_PER_WORD;
	      if (mode == DImode)
		{
		  offset += UNITS_PER_WORD;
		  ++regno;
		}
	    } /* if */
	} /* for */
    }/* if */
  if (sibthunk_insn)
    {
      int start_call = frame->millicode_start_reg;
      int end_call = frame->millicode_end_reg;
      int r;

      rtx r12 = gen_rtx_REG (Pmode, 12);

      frame_insn (gen_rtx_SET (r12, GEN_INT (offset)));
      XVECEXP (sibthunk_insn, 0, 0) = ret_rtx;
      XVECEXP (sibthunk_insn, 0, 1)
	= gen_rtx_SET (stack_pointer_rtx,
		       gen_rtx_PLUS (Pmode, stack_pointer_rtx, r12));
      sibthunk_insn = emit_jump_insn (sibthunk_insn);
      RTX_FRAME_RELATED_P (sibthunk_insn) = 1;

      /* Would be nice if we could do this earlier, when the PARALLEL
	 is populated, but these need to be attached after the
	 emit.  */
      for (r = start_call; r <= end_call; r++)
	{
	  rtx reg = gen_rtx_REG (SImode, r);
	  add_reg_note (sibthunk_insn, REG_CFA_RESTORE, reg);
	}
    }
} /* arc_save_restore */

/* Build dwarf information when the context is saved via AUX_IRQ_CTRL
   mechanism.  */

static void
arc_dwarf_emit_irq_save_regs (void)
{
  rtx tmp, par, insn, reg;
  int i, offset, j;

  par = gen_rtx_SEQUENCE (VOIDmode,
			  rtvec_alloc (irq_ctrl_saved.irq_save_last_reg + 1
				       + irq_ctrl_saved.irq_save_blink
				       + irq_ctrl_saved.irq_save_lpcount
				       + 1));

  /* Build the stack adjustment note for unwind info.  */
  j = 0;
  offset = UNITS_PER_WORD * (irq_ctrl_saved.irq_save_last_reg + 1
			     + irq_ctrl_saved.irq_save_blink
			     + irq_ctrl_saved.irq_save_lpcount);
  tmp = plus_constant (Pmode, stack_pointer_rtx, -1 * offset);
  tmp = gen_rtx_SET (stack_pointer_rtx, tmp);
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (par, 0, j++) = tmp;

  offset -= UNITS_PER_WORD;

  /* 1st goes LP_COUNT.  */
  if (irq_ctrl_saved.irq_save_lpcount)
    {
      reg = gen_rtx_REG (SImode, 60);
      tmp = plus_constant (Pmode, stack_pointer_rtx, offset);
      tmp = gen_frame_mem (SImode, tmp);
      tmp = gen_rtx_SET (tmp, reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, j++) = tmp;
      offset -= UNITS_PER_WORD;
    }

  /* 2nd goes BLINK.  */
  if (irq_ctrl_saved.irq_save_blink)
    {
      reg = gen_rtx_REG (SImode, 31);
      tmp = plus_constant (Pmode, stack_pointer_rtx, offset);
      tmp = gen_frame_mem (SImode, tmp);
      tmp = gen_rtx_SET (tmp, reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, j++) = tmp;
      offset -= UNITS_PER_WORD;
    }

  /* Build the parallel of the remaining registers recorded as saved
     for unwind.  */
  for (i = irq_ctrl_saved.irq_save_last_reg; i >= 0; i--)
    {
      reg = gen_rtx_REG (SImode, i);
      tmp = plus_constant (Pmode, stack_pointer_rtx, offset);
      tmp = gen_frame_mem (SImode, tmp);
      tmp = gen_rtx_SET (tmp, reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (par, 0, j++) = tmp;
      offset -= UNITS_PER_WORD;
    }

  /* Dummy insn used to anchor the dwarf info.  */
  insn = emit_insn (gen_stack_irq_dwarf());
  add_reg_note (insn, REG_FRAME_RELATED_EXPR, par);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Set up the stack and frame pointer (if desired) for the function.  */

void
arc_expand_prologue (void)
{
  int size = get_frame_size ();
  unsigned int gmask = cfun->machine->frame_info.gmask;
  /*  unsigned int frame_pointer_offset;*/
  unsigned int frame_size_to_allocate;
  /* (FIXME: The first store will use a PRE_MODIFY; this will usually be r13.
     Change the stack layout so that we rather store a high register with the
     PRE_MODIFY, thus enabling more short insn generation.)  */
  int first_offset = 0;
  unsigned int fn_type = arc_compute_function_type (cfun);

  /* Naked functions don't have prologue.  */
  if (ARC_NAKED_P (fn_type))
    return;

  size = ARC_STACK_ALIGN (size);

  /* Compute/get total frame size.  */
  size = (!cfun->machine->frame_info.initialized
	   ? arc_compute_frame_size (size)
	   : cfun->machine->frame_info.total_size);

  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  /* Keep track of frame size to be allocated.  */
  frame_size_to_allocate = size;

  /* These cases shouldn't happen.  Catch them now.  */
  gcc_assert (!(size == 0 && gmask));

  /* Allocate space for register arguments if this is a variadic function.  */
  if (cfun->machine->frame_info.pretend_size != 0)
    {
       /* Ensure pretend_size is maximum of 8 * word_size.  */
      gcc_assert (cfun->machine->frame_info.pretend_size <= 32);

      frame_stack_add (-(HOST_WIDE_INT)cfun->machine->frame_info.pretend_size);
      frame_size_to_allocate -= cfun->machine->frame_info.pretend_size;
    }

  /* IRQ using automatic save mechanism will save the register before
     anything we do.  */
  if (ARC_AUTO_IRQ_P (fn_type)
      && !ARC_FAST_INTERRUPT_P (fn_type))
    {
      arc_dwarf_emit_irq_save_regs ();
    }

  /* The home-grown ABI says link register is saved first.  */
  if (arc_must_save_return_addr (cfun)
      && !ARC_AUTOBLINK_IRQ_P (fn_type))
    {
      rtx ra = gen_rtx_REG (SImode, RETURN_ADDR_REGNUM);
      rtx mem = gen_frame_mem (Pmode,
			       gen_rtx_PRE_DEC (Pmode,
						stack_pointer_rtx));

      frame_move_inc (mem, ra, stack_pointer_rtx, 0);
      frame_size_to_allocate -= UNITS_PER_WORD;
    }

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler) for ARCompact ISA.  */
  if (cfun->machine->frame_info.reg_size)
    {
      first_offset = -cfun->machine->frame_info.reg_size;
      /* N.B. FRAME_POINTER_MASK and RETURN_ADDR_MASK are cleared in gmask.  */
      arc_save_restore (stack_pointer_rtx, gmask, 0, &first_offset);
      frame_size_to_allocate -= cfun->machine->frame_info.reg_size;
    }

  /* Save frame pointer if needed.  First save the FP on stack, if not
     autosaved.  */
  if (arc_frame_pointer_needed ()
      && !ARC_AUTOFP_IRQ_P (fn_type))
    {
      rtx addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			       GEN_INT (-UNITS_PER_WORD + first_offset));
      rtx mem = gen_frame_mem (Pmode, gen_rtx_PRE_MODIFY (Pmode,
							  stack_pointer_rtx,
							  addr));
      frame_move_inc (mem, frame_pointer_rtx, stack_pointer_rtx, 0);
      frame_size_to_allocate -= UNITS_PER_WORD;
      first_offset = 0;
    }

  /* Emit mov fp,sp.  */
  if (arc_frame_pointer_needed ())
    {
      frame_move (frame_pointer_rtx, stack_pointer_rtx);
    }

  /* ??? We don't handle the case where the saved regs are more than 252
     bytes away from sp.  This can be handled by decrementing sp once, saving
     the regs, and then decrementing it again.  The epilogue doesn't have this
     problem as the `ld' insn takes reg+limm values (though it would be more
     efficient to avoid reg+limm).  */

  frame_size_to_allocate -= first_offset;
  /* Allocate the stack frame.  */
  if (frame_size_to_allocate > 0)
    {
      frame_stack_add ((HOST_WIDE_INT) 0 - frame_size_to_allocate);
      /* If the frame pointer is needed, emit a special barrier that
	 will prevent the scheduler from moving stores to the frame
	 before the stack adjustment.  */
      if (arc_frame_pointer_needed ())
	emit_insn (gen_stack_tie (stack_pointer_rtx,
				  hard_frame_pointer_rtx));
    }

  /* Setup the gp register, if needed.  */
  if (crtl->uses_pic_offset_table)
    arc_finalize_pic ();
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */

void
arc_expand_epilogue (int sibcall_p)
{
  int size = get_frame_size ();
  unsigned int fn_type = arc_compute_function_type (cfun);

  size = ARC_STACK_ALIGN (size);
  size = (!cfun->machine->frame_info.initialized
	   ? arc_compute_frame_size (size)
	   : cfun->machine->frame_info.total_size);

  unsigned int pretend_size = cfun->machine->frame_info.pretend_size;
  unsigned int frame_size;
  unsigned int size_to_deallocate;
  int restored;
  int can_trust_sp_p = !cfun->calls_alloca;
  int first_offset = 0;
  int millicode_p = cfun->machine->frame_info.millicode_end_reg > 0;
  rtx insn;

  /* Naked functions don't have epilogue.  */
  if (ARC_NAKED_P (fn_type))
    return;

  size_to_deallocate = size;

  frame_size = size - (pretend_size +
		       cfun->machine->frame_info.reg_size +
		       cfun->machine->frame_info.extra_size);

  /* ??? There are lots of optimizations that can be done here.
     EG: Use fp to restore regs if it's closer.
     Maybe in time we'll do them all.  For now, always restore regs from
     sp, but don't restore sp if we don't have to.  */

  if (!can_trust_sp_p)
    gcc_assert (arc_frame_pointer_needed ());

  /* Restore stack pointer to the beginning of saved register area for
     ARCompact ISA.  */
  if (frame_size)
    {
      if (arc_frame_pointer_needed ())
	frame_move (stack_pointer_rtx, frame_pointer_rtx);
      else
	first_offset = frame_size;
      size_to_deallocate -= frame_size;
    }
  else if (!can_trust_sp_p)
    frame_stack_add (-frame_size);


  /* Restore any saved registers.  */
  if (arc_frame_pointer_needed ()
      && !ARC_AUTOFP_IRQ_P (fn_type))
    {
      rtx addr = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);

      insn = frame_move_inc (frame_pointer_rtx, gen_frame_mem (Pmode, addr),
			     stack_pointer_rtx, 0);
      add_reg_note (insn, REG_CFA_RESTORE, frame_pointer_rtx);
      add_reg_note (insn, REG_CFA_DEF_CFA,
		    plus_constant (SImode, stack_pointer_rtx,
				   4));
      size_to_deallocate -= UNITS_PER_WORD;
    }

  /* Load blink after the calls to thunk calls in case of optimize size.  */
  if (millicode_p)
    {
	  int sibthunk_p = (!sibcall_p
			    && fn_type == ARC_FUNCTION_NORMAL
			    && !cfun->machine->frame_info.pretend_size);

	  gcc_assert (!(cfun->machine->frame_info.gmask
			& (FRAME_POINTER_MASK | RETURN_ADDR_MASK)));
	  arc_save_restore (stack_pointer_rtx,
			    cfun->machine->frame_info.gmask,
			    1 + sibthunk_p, &first_offset);
	  if (sibthunk_p)
	    return;
    }
  /* If we are to restore registers, and first_offset would require
     a limm to be encoded in a PRE_MODIFY, yet we can add it with a
     fast add to the stack pointer, do this now.  */
  if ((!SMALL_INT (first_offset)
       && cfun->machine->frame_info.gmask
       && ((TARGET_ARC700 && !optimize_size)
	    ? first_offset <= 0x800
	    : satisfies_constraint_C2a (GEN_INT (first_offset))))
       /* Also do this if we have both gprs and return
	  address to restore, and they both would need a LIMM.  */
      || (arc_must_save_return_addr (cfun)
	  && !SMALL_INT ((cfun->machine->frame_info.reg_size + first_offset) >> 2)
	  && cfun->machine->frame_info.gmask))
    {
      frame_stack_add (first_offset);
      first_offset = 0;
    }
  if (arc_must_save_return_addr (cfun)
      && !ARC_AUTOBLINK_IRQ_P (fn_type))
    {
      rtx ra = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
      int ra_offs = cfun->machine->frame_info.reg_size + first_offset;
      rtx addr = plus_constant (Pmode, stack_pointer_rtx, ra_offs);
      HOST_WIDE_INT cfa_adjust = 0;

      /* If the load of blink would need a LIMM, but we can add
	 the offset quickly to sp, do the latter.  */
      if (!SMALL_INT (ra_offs >> 2)
	  && !cfun->machine->frame_info.gmask
	  && ((TARGET_ARC700 && !optimize_size)
	       ? ra_offs <= 0x800
	       : satisfies_constraint_C2a (GEN_INT (ra_offs))))
	{
	   size_to_deallocate -= ra_offs - first_offset;
	   first_offset = 0;
	   frame_stack_add (ra_offs);
	   ra_offs = 0;
	   addr = stack_pointer_rtx;
	}
      /* See if we can combine the load of the return address with the
	 final stack adjustment.
	 We need a separate load if there are still registers to
	 restore.  We also want a separate load if the combined insn
	 would need a limm, but a separate load doesn't.  */
      if (ra_offs
	  && !cfun->machine->frame_info.gmask
	  && (SMALL_INT (ra_offs) || !SMALL_INT (ra_offs >> 2)))
	{
	  addr = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx, addr);
	  cfa_adjust = ra_offs;
	  first_offset = 0;
	  size_to_deallocate -= cfun->machine->frame_info.reg_size;
	}
      else if (!ra_offs && size_to_deallocate == UNITS_PER_WORD)
	{
	  addr = gen_rtx_POST_INC (Pmode, addr);
	  cfa_adjust = GET_MODE_SIZE (Pmode);
	  size_to_deallocate = 0;
	}

      insn = frame_move_inc (ra, gen_frame_mem (Pmode, addr),
			     stack_pointer_rtx, addr);
      if (cfa_adjust)
	{
	  enum reg_note note = REG_CFA_ADJUST_CFA;

	  add_reg_note (insn, note,
			gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (SImode, stack_pointer_rtx,
						    cfa_adjust)));
	}
      add_reg_note (insn, REG_CFA_RESTORE, ra);
    }

  if (!millicode_p)
    {
       if (cfun->machine->frame_info.reg_size)
	 arc_save_restore (stack_pointer_rtx,
	   /* The zeroing of these two bits is unnecessary, but leave this in for clarity.  */
			   cfun->machine->frame_info.gmask
			   & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK), 1, &first_offset);
    }

  /* The rest of this function does the following:
     ARCompact    : handle epilogue_delay, restore sp (phase-2), return
  */

  /* Keep track of how much of the stack pointer we've restored.
     It makes the following a lot more readable.  */
  size_to_deallocate += first_offset;
  restored = size - size_to_deallocate;

  if (size > restored)
    frame_stack_add (size - restored);

  /* Emit the return instruction.  */
  if (sibcall_p == FALSE)
    emit_jump_insn (gen_simple_return ());
}

/* Return the offset relative to the stack pointer where the return address
   is stored, or -1 if it is not stored.  */

int
arc_return_slot_offset ()
{
  struct arc_frame_info *afi = &cfun->machine->frame_info;

  return (afi->save_return_addr
	  ? afi->total_size - afi->pretend_size - afi->extra_size : -1);
}

/* PIC */

/* Helper to generate unspec constant.  */

static rtx
arc_unspec_offset (rtx loc, int unspec)
{
  return gen_rtx_CONST (Pmode, gen_rtx_UNSPEC (Pmode, gen_rtvec (1, loc),
					       unspec));
}

/* Emit special PIC prologues and epilogues.  */
/* If the function has any GOTOFF relocations, then the GOTBASE
   register has to be setup in the prologue
   The instruction needed at the function start for setting up the
   GOTBASE register is
      add rdest, pc,
   ----------------------------------------------------------
   The rtl to be emitted for this should be:
     set (reg basereg)
         (plus (reg pc)
               (const (unspec (symref _DYNAMIC) 3)))
   ----------------------------------------------------------  */

static void
arc_finalize_pic (void)
{
  rtx pat;
  rtx baseptr_rtx = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);

  if (crtl->uses_pic_offset_table == 0)
    return;

  gcc_assert (flag_pic != 0);

  pat = gen_rtx_SYMBOL_REF (Pmode, "_DYNAMIC");
  pat = arc_unspec_offset (pat, ARC_UNSPEC_GOT);
  pat = gen_rtx_SET (baseptr_rtx, pat);

  emit_insn (pat);
}

/* !TARGET_BARREL_SHIFTER support.  */
/* Emit a shift insn to set OP0 to OP1 shifted by OP2; CODE specifies what
   kind of shift.  */

void
emit_shift (enum rtx_code code, rtx op0, rtx op1, rtx op2)
{
  rtx shift = gen_rtx_fmt_ee (code, SImode, op1, op2);
  rtx pat
    = ((shift4_operator (shift, SImode) ?  gen_shift_si3 : gen_shift_si3_loop)
	(op0, op1, op2, shift));
  emit_insn (pat);
}

/* Output the assembler code for doing a shift.
   We go to a bit of trouble to generate efficient code as the ARC601 only has
   single bit shifts.  This is taken from the h8300 port.  We only have one
   mode of shifting and can't access individual bytes like the h8300 can, so
   this is greatly simplified (at the expense of not generating hyper-
   efficient code).

   This function is not used if the variable shift insns are present.  */

/* FIXME:  This probably can be done using a define_split in arc.md.
   Alternately, generate rtx rather than output instructions.  */

const char *
output_shift (rtx *operands)
{
  /*  static int loopend_lab;*/
  rtx shift = operands[3];
  machine_mode mode = GET_MODE (shift);
  enum rtx_code code = GET_CODE (shift);
  const char *shift_one;

  gcc_assert (mode == SImode);

  switch (code)
    {
    case ASHIFT:   shift_one = "add %0,%1,%1"; break;
    case ASHIFTRT: shift_one = "asr %0,%1"; break;
    case LSHIFTRT: shift_one = "lsr %0,%1"; break;
    default:       gcc_unreachable ();
    }

  if (GET_CODE (operands[2]) != CONST_INT)
    {
      output_asm_insn ("and.f lp_count,%2, 0x1f", operands);
      goto shiftloop;
    }
  else
    {
      int n;

      n = INTVAL (operands[2]);

      /* Only consider the lower 5 bits of the shift count.  */
      n = n & 0x1f;

      /* First see if we can do them inline.  */
      /* ??? We could get better scheduling & shorter code (using short insns)
	 by using splitters.  Alas, that'd be even more verbose.  */
      if (code == ASHIFT && n <= 9 && n > 2
	  && dest_reg_operand (operands[4], SImode))
	{
	  output_asm_insn ("mov %4,0\n\tadd3 %0,%4,%1", operands);
	  for (n -=3 ; n >= 3; n -= 3)
	    output_asm_insn ("add3 %0,%4,%0", operands);
	  if (n == 2)
	    output_asm_insn ("add2 %0,%4,%0", operands);
	  else if (n)
	    output_asm_insn ("add %0,%0,%0", operands);
	}
      else if (n <= 4)
	{
	  while (--n >= 0)
	    {
	      output_asm_insn (shift_one, operands);
	      operands[1] = operands[0];
	    }
	}
      /* See if we can use a rotate/and.  */
      else if (n == BITS_PER_WORD - 1)
	{
	  switch (code)
	    {
	    case ASHIFT :
	      output_asm_insn ("and %0,%1,1\n\tror %0,%0", operands);
	      break;
	    case ASHIFTRT :
	      /* The ARC doesn't have a rol insn.  Use something else.  */
	      output_asm_insn ("add.f 0,%1,%1\n\tsbc %0,%0,%0", operands);
	      break;
	    case LSHIFTRT :
	      /* The ARC doesn't have a rol insn.  Use something else.  */
	      output_asm_insn ("add.f 0,%1,%1\n\trlc %0,0", operands);
	      break;
	    default:
	      break;
	    }
	}
      else if (n == BITS_PER_WORD - 2 && dest_reg_operand (operands[4], SImode))
	{
	  switch (code)
	    {
	    case ASHIFT :
	      output_asm_insn ("and %0,%1,3\n\tror %0,%0\n\tror %0,%0", operands);
	      break;
	    case ASHIFTRT :
#if 1 /* Need some scheduling comparisons.  */
	      output_asm_insn ("add.f %4,%1,%1\n\tsbc %0,%0,%0\n\t"
			       "add.f 0,%4,%4\n\trlc %0,%0", operands);
#else
	      output_asm_insn ("add.f %4,%1,%1\n\tbxor %0,%4,31\n\t"
			       "sbc.f %0,%0,%4\n\trlc %0,%0", operands);
#endif
	      break;
	    case LSHIFTRT :
#if 1
	      output_asm_insn ("add.f %4,%1,%1\n\trlc %0,0\n\t"
			       "add.f 0,%4,%4\n\trlc %0,%0", operands);
#else
	      output_asm_insn ("add.f %0,%1,%1\n\trlc.f %0,0\n\t"
			       "and %0,%0,1\n\trlc %0,%0", operands);
#endif
	      break;
	    default:
	      break;
	    }
	}
      else if (n == BITS_PER_WORD - 3 && code == ASHIFT)
	output_asm_insn ("and %0,%1,7\n\tror %0,%0\n\tror %0,%0\n\tror %0,%0",
			 operands);
      /* Must loop.  */
      else
	{
	  operands[2] = GEN_INT (n);
	  output_asm_insn ("mov.f lp_count, %2", operands);

	shiftloop:
	    {
	      output_asm_insn ("lpnz\t2f", operands);
	      output_asm_insn (shift_one, operands);
	      output_asm_insn ("nop", operands);
	      fprintf (asm_out_file, "2:\t%s end single insn loop\n",
		       ASM_COMMENT_START);
	    }
	}
    }

  return "";
}

/* Nested function support.  */

/* Directly store VALUE into memory object BLOCK at OFFSET.  */

static void
emit_store_direct (rtx block, int offset, int value)
{
  emit_insn (gen_store_direct (adjust_address (block, SImode, offset),
			       force_reg (SImode,
					  gen_int_mode (value, SImode))));
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
/* With potentially multiple shared objects loaded, and multiple stacks
   present for multiple thereds where trampolines might reside, a simple
   range check will likely not suffice for the profiler to tell if a callee
   is a trampoline.  We a speedier check by making the trampoline start at
   an address that is not 4-byte aligned.
   A trampoline looks like this:

   nop_s	     0x78e0
entry:
   ld_s r12,[pcl,12] 0xd403
   ld   r11,[pcl,12] 0x170c 700b
   j_s [r12]         0x7c00
   nop_s	     0x78e0

   The fastest trampoline to execute for trampolines within +-8KB of CTX
   would be:
   add2 r11,pcl,s12
   j [limm]           0x20200f80 limm
   and that would also be faster to write to the stack by computing the offset
   from CTX to TRAMP at compile time.  However, it would really be better to
   get rid of the high cost of cache invalidation when generating trampolines,
   which requires that the code part of trampolines stays constant, and
   additionally either
   - making sure that no executable code but trampolines is on the stack,
     no icache entries linger for the area of the stack from when before the
     stack was allocated, and allocating trampolines in trampoline-only
     cache lines
  or
   - allocate trampolines fram a special pool of pre-allocated trampolines.  */

static void
arc_initialize_trampoline (rtx tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_store_direct (tramp, 0, TARGET_BIG_ENDIAN ? 0x78e0d403 : 0xd40378e0);
  emit_store_direct (tramp, 4, TARGET_BIG_ENDIAN ? 0x170c700b : 0x700b170c);
  emit_store_direct (tramp, 8, TARGET_BIG_ENDIAN ? 0x7c0078e0 : 0x78e07c00);
  emit_move_insn (adjust_address (tramp, SImode, 12), fnaddr);
  emit_move_insn (adjust_address (tramp, SImode, 16), cxt);
  emit_insn (gen_flush_icache (adjust_address (tramp, SImode, 0)));
}

/* Allow the profiler to easily distinguish trampolines from normal
  functions.  */

static rtx
arc_trampoline_adjust_address (rtx addr)
{
  return plus_constant (Pmode, addr, 2);
}

/* This is set briefly to 1 when we output a ".as" address modifer, and then
   reset when we output the scaled address.  */
static int output_scaled = 0;

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
/* In final.c:output_asm_insn:
    'l' : label
    'a' : address
    'c' : constant address if CONSTANT_ADDRESS_P
    'n' : negative
   Here:
    'Z': log2(x+1)-1
    'z': log2
    'M': log2(~x)
    'p': bit Position of lsb
    's': size of bit field
    '#': condbranch delay slot suffix
    '*': jump delay slot suffix
    '?' : nonjump-insn suffix for conditional execution or short instruction
    '!' : jump / call suffix for conditional execution or short instruction
    '`': fold constant inside unary o-perator, re-recognize, and emit.
    'd'
    'D'
    'R': Second word
    'S'
    'B': Branch comparison operand - suppress sda reference
    'H': Most significant word
    'L': Least significant word
    'A': ASCII decimal representation of floating point value
    'U': Load/store update or scaling indicator
    'V': cache bypass indicator for volatile
    'P'
    'F'
    '^'
    'O': Operator
    'o': original symbol - no @ prepending.  */

void
arc_print_operand (FILE *file, rtx x, int code)
{
  switch (code)
    {
    case 'Z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(INTVAL (x) + 1) - 1 );
      else
	output_operand_lossage ("invalid operand to %%Z code");

      return;

    case 'z':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(INTVAL (x)) );
      else
	output_operand_lossage ("invalid operand to %%z code");

      return;

    case 'c':
      if (GET_CODE (x) == CONST_INT)
        fprintf (file, "%d", INTVAL (x) );
      else
        output_operand_lossage ("invalid operands to %%c code");

      return;

    case 'M':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(~INTVAL (x)) );
      else
	output_operand_lossage ("invalid operand to %%M code");

      return;

    case 'p':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d", exact_log2 (INTVAL (x) & -INTVAL (x)));
      else
	output_operand_lossage ("invalid operand to %%p code");
      return;

    case 's':
      if (GET_CODE (x) == CONST_INT)
	{
	  HOST_WIDE_INT i = INTVAL (x);
	  HOST_WIDE_INT s = exact_log2 (i & -i);
	  fprintf (file, "%d", exact_log2 (((0xffffffffUL & i) >> s) + 1));
	}
      else
	output_operand_lossage ("invalid operand to %%s code");
      return;

    case '#' :
      /* Conditional branches depending on condition codes.
	 Note that this is only for branches that were known to depend on
	 condition codes before delay slot scheduling;
	 out-of-range brcc / bbit expansions should use '*'.
	 This distinction is important because of the different
	 allowable delay slot insns and the output of the delay suffix
	 for TARGET_AT_DBR_COND_EXEC.  */
    case '*' :
      /* Unconditional branches / branches not depending on condition codes.
	 This could also be a CALL_INSN.
	 Output the appropriate delay slot suffix.  */
      if (final_sequence && final_sequence->len () != 1)
	{
	  rtx_insn *jump = final_sequence->insn (0);
	  rtx_insn *delay = final_sequence->insn (1);

	  /* For TARGET_PAD_RETURN we might have grabbed the delay insn.  */
	  if (delay->deleted ())
	    return;
	  if (JUMP_P (jump) && INSN_ANNULLED_BRANCH_P (jump))
	    fputs (INSN_FROM_TARGET_P (delay) ? ".d"
		   : TARGET_AT_DBR_CONDEXEC && code == '#' ? ".d"
		   : get_attr_type (jump) == TYPE_RETURN && code == '#' ? ""
		   : ".nd",
		   file);
	  else
	    fputs (".d", file);
	}
      return;
    case '?' : /* with leading "." */
    case '!' : /* without leading "." */
      /* This insn can be conditionally executed.  See if the ccfsm machinery
	 says it should be conditionalized.
	 If it shouldn't, we'll check the compact attribute if this insn
	 has a short variant, which may be used depending on code size and
	 alignment considerations.  */
      if (current_insn_predicate)
	arc_ccfsm_current.cc
	  = get_arc_condition_code (current_insn_predicate);
      if (ARC_CCFSM_COND_EXEC_P (&arc_ccfsm_current))
	{
	  /* Is this insn in a delay slot sequence?  */
	  if (!final_sequence || XVECLEN (final_sequence, 0) < 2
	      || current_insn_predicate
	      || CALL_P (final_sequence->insn (0))
	      || simplejump_p (final_sequence->insn (0)))
	    {
	      /* This insn isn't in a delay slot sequence, or conditionalized
		 independently of its position in a delay slot.  */
	      fprintf (file, "%s%s",
		       code == '?' ? "." : "",
		       arc_condition_codes[arc_ccfsm_current.cc]);
	      /* If this is a jump, there are still short variants.  However,
		 only beq_s / bne_s have the same offset range as b_s,
		 and the only short conditional returns are jeq_s and jne_s.  */
	      if (code == '!'
		  && (arc_ccfsm_current.cc == ARC_CC_EQ
		      || arc_ccfsm_current.cc == ARC_CC_NE
		      || 0 /* FIXME: check if branch in 7 bit range.  */))
		output_short_suffix (file);
	    }
	  else if (code == '!') /* Jump with delay slot.  */
	    fputs (arc_condition_codes[arc_ccfsm_current.cc], file);
	  else /* An Instruction in a delay slot of a jump or call.  */
	    {
	      rtx jump = XVECEXP (final_sequence, 0, 0);
	      rtx insn = XVECEXP (final_sequence, 0, 1);

	      /* If the insn is annulled and is from the target path, we need
		 to inverse the condition test.  */
	      if (JUMP_P (jump) && INSN_ANNULLED_BRANCH_P (jump))
		{
		  if (INSN_FROM_TARGET_P (insn))
		    fprintf (file, "%s%s",
			     code == '?' ? "." : "",
			     arc_condition_codes[ARC_INVERSE_CONDITION_CODE (arc_ccfsm_current.cc)]);
		  else
		    fprintf (file, "%s%s",
			     code == '?' ? "." : "",
			     arc_condition_codes[arc_ccfsm_current.cc]);
		  if (arc_ccfsm_current.state == 5)
		    arc_ccfsm_current.state = 0;
		}
	      else
		/* This insn is executed for either path, so don't
		   conditionalize it at all.  */
		output_short_suffix (file);

	    }
	}
      else
	output_short_suffix (file);
      return;
    case'`':
      /* FIXME: fold constant inside unary operator, re-recognize, and emit.  */
      gcc_unreachable ();
    case 'd' :
      fputs (arc_condition_codes[get_arc_condition_code (x)], file);
      return;
    case 'D' :
      fputs (arc_condition_codes[ARC_INVERSE_CONDITION_CODE
				 (get_arc_condition_code (x))],
	     file);
      return;
    case 'R' :
      /* Write second word of DImode or DFmode reference,
	 register or memory.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)+1], file);
      else if (GET_CODE (x) == MEM)
	{
	  fputc ('[', file);

	  /* Handle possible auto-increment.  For PRE_INC / PRE_DEC /
	    PRE_MODIFY, we will have handled the first word already;
	    For POST_INC / POST_DEC / POST_MODIFY, the access to the
	    first word will be done later.  In either case, the access
	    to the first word will do the modify, and we only have
	    to add an offset of four here.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC
	      || GET_CODE (XEXP (x, 0)) == PRE_MODIFY
	      || GET_CODE (XEXP (x, 0)) == POST_INC
	      || GET_CODE (XEXP (x, 0)) == POST_DEC
	      || GET_CODE (XEXP (x, 0)) == POST_MODIFY)
	    output_address (VOIDmode,
			    plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 4));
	  else if (output_scaled)
	    {
	      rtx addr = XEXP (x, 0);
	      int size = GET_MODE_SIZE (GET_MODE (x));

	      output_address (VOIDmode,
			      plus_constant (Pmode, XEXP (addr, 0),
					     ((INTVAL (XEXP (addr, 1)) + 4)
					      >> (size == 2 ? 1 : 2))));
	      output_scaled = 0;
	    }
	  else
	    output_address (VOIDmode,
			    plus_constant (Pmode, XEXP (x, 0), 4));
	  fputc (']', file);
	}
      else
	output_operand_lossage ("invalid operand to %%R code");
      return;
    case 'S' :
	/* FIXME: remove %S option.  */
	break;
    case 'B' /* Branch or other LIMM ref - must not use sda references.  */ :
      if (CONSTANT_P (x))
	{
	  output_addr_const (file, x);
	  return;
	}
      break;
    case 'H' :
    case 'L' :
      if (GET_CODE (x) == REG)
	{
	  /* L = least significant word, H = most significant word.  */
	  if ((WORDS_BIG_ENDIAN != 0) ^ (code == 'L'))
	    fputs (reg_names[REGNO (x)], file);
	  else
	    fputs (reg_names[REGNO (x)+1], file);
	}
      else if (GET_CODE (x) == CONST_INT
	       || GET_CODE (x) == CONST_DOUBLE)
	{
	  rtx first, second, word;

	  split_double (x, &first, &second);

	  if((WORDS_BIG_ENDIAN) == 0)
	    word = (code == 'L' ? first : second);
	  else
	    word = (code == 'L' ? second : first);

	  fprintf (file, "0x%08" PRIx32, ((uint32_t) INTVAL (word)));
	}
      else
	output_operand_lossage ("invalid operand to %%H/%%L code");
      return;
    case 'A' :
      {
	char str[30];

	gcc_assert (GET_CODE (x) == CONST_DOUBLE
		    && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT);

	real_to_decimal (str, CONST_DOUBLE_REAL_VALUE (x), sizeof (str), 0, 1);
	fprintf (file, "%s", str);
	return;
      }
    case 'U' :
      /* Output a load/store with update indicator if appropriate.  */
      if (GET_CODE (x) == MEM)
	{
	  rtx addr = XEXP (x, 0);
	  switch (GET_CODE (addr))
	    {
	    case PRE_INC: case PRE_DEC: case PRE_MODIFY:
	      fputs (".a", file); break;
	    case POST_INC: case POST_DEC: case POST_MODIFY:
	      fputs (".ab", file); break;
	    case PLUS:
	      /* Are we using a scaled index?  */
	      if (GET_CODE (XEXP (addr, 0)) == MULT)
		fputs (".as", file);
	      /* Can we use a scaled offset?  */
	      else if (CONST_INT_P (XEXP (addr, 1))
		       && GET_MODE_SIZE (GET_MODE (x)) > 1
		       && (!(INTVAL (XEXP (addr, 1))
			     & (GET_MODE_SIZE (GET_MODE (x)) - 1) & 3))
		       /* Does it make a difference?  */
		       && !SMALL_INT_RANGE(INTVAL (XEXP (addr, 1)),
					   GET_MODE_SIZE (GET_MODE (x)) - 2, 0))
		{
		  fputs (".as", file);
		  output_scaled = 1;
		}
	      else if (LEGITIMATE_SMALL_DATA_ADDRESS_P (addr)
		       && GET_MODE_SIZE (GET_MODE (x)) > 1)
		{
		  tree decl = NULL_TREE;
		  int align = 0;
		  if (GET_CODE (XEXP (addr, 1)) == SYMBOL_REF)
		    decl = SYMBOL_REF_DECL (XEXP (addr, 1));
		  else if (GET_CODE (XEXP (XEXP (XEXP (addr, 1), 0), 0))
			   == SYMBOL_REF)
		    decl = SYMBOL_REF_DECL (XEXP (XEXP (XEXP (addr, 1), 0), 0));
		  if (decl)
		    align = DECL_ALIGN (decl);
		  align = align / BITS_PER_UNIT;
		  if ((GET_MODE_SIZE (GET_MODE (x)) == 2)
		      && align && ((align & 1) == 0))
		    fputs (".as", file);
		  if ((GET_MODE_SIZE (GET_MODE (x)) >= 4)
		      && align && ((align & 3) == 0))
		    fputs (".as", file);
		}
	      break;
	    case REG:
	      break;
	    default:
	      gcc_assert (CONSTANT_P (addr)); break;
	    }
	}
      else
	output_operand_lossage ("invalid operand to %%U code");
      return;
    case 'V' :
      /* Output cache bypass indicator for a load/store insn.  Volatile memory
	 refs are defined to use the cache bypass mechanism.  */
      if (GET_CODE (x) == MEM)
	{
	  if (MEM_VOLATILE_P (x) && !TARGET_VOLATILE_CACHE_SET )
	    fputs (".di", file);
	}
      else
	output_operand_lossage ("invalid operand to %%V code");
      return;
      /* plt code.  */
    case 'P':
    case 0 :
      /* Do nothing special.  */
      break;
    case 'F':
      fputs (reg_names[REGNO (x)]+1, file);
      return;
    case '^':
	/* This punctuation character is needed because label references are
	printed in the output template using %l. This is a front end
	character, and when we want to emit a '@' before it, we have to use
	this '^'.  */

	fputc('@',file);
	return;
    case 'O':
      /* Output an operator.  */
      switch (GET_CODE (x))
	{
	case PLUS:	fputs ("add", file); return;
	case SS_PLUS:	fputs ("adds", file); return;
	case AND:	fputs ("and", file); return;
	case IOR:	fputs ("or", file); return;
	case XOR:	fputs ("xor", file); return;
	case MINUS:	fputs ("sub", file); return;
	case SS_MINUS:	fputs ("subs", file); return;
	case ASHIFT:	fputs ("asl", file); return;
	case ASHIFTRT:	fputs ("asr", file); return;
	case LSHIFTRT:	fputs ("lsr", file); return;
	case ROTATERT:	fputs ("ror", file); return;
	case MULT:	fputs ("mpy", file); return;
	case ABS:	fputs ("abs", file); return; /* Unconditional.  */
	case NEG:	fputs ("neg", file); return;
	case SS_NEG:	fputs ("negs", file); return;
	case NOT:	fputs ("not", file); return; /* Unconditional.  */
	case ZERO_EXTEND:
	  fputs ("ext", file); /* bmsk allows predication.  */
	  goto size_suffix;
	case SIGN_EXTEND: /* Unconditional.  */
	  fputs ("sex", file);
	size_suffix:
	  switch (GET_MODE (XEXP (x, 0)))
	    {
	    case E_QImode: fputs ("b", file); return;
	    case E_HImode: fputs ("w", file); return;
	    default: break;
	    }
	  break;
	case SS_TRUNCATE:
	  if (GET_MODE (x) != HImode)
	    break;
	  fputs ("sat16", file);
	default: break;
	}
      output_operand_lossage ("invalid operand to %%O code"); return;
    case 'o':
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  assemble_name (file, XSTR (x, 0));
	  return;
	}
      break;
    case '&':
      if (TARGET_ANNOTATE_ALIGN && cfun->machine->size_reason)
	fprintf (file, "; unalign: %d", cfun->machine->unalign);
      return;
    case '+':
      if (TARGET_V2)
	fputs ("m", file);
      else
	fputs ("h", file);
      return;
    case '_':
      if (TARGET_V2)
	fputs ("h", file);
      else
	fputs ("w", file);
      return;
    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  switch (GET_CODE (x))
    {
    case REG :
      fputs (reg_names[REGNO (x)], file);
      break;
    case MEM :
      {
	rtx addr = XEXP (x, 0);
	int size = GET_MODE_SIZE (GET_MODE (x));

	fputc ('[', file);

	switch (GET_CODE (addr))
	  {
	  case PRE_INC: case POST_INC:
	    output_address (VOIDmode,
			    plus_constant (Pmode, XEXP (addr, 0), size)); break;
	  case PRE_DEC: case POST_DEC:
	    output_address (VOIDmode,
			    plus_constant (Pmode, XEXP (addr, 0), -size));
	    break;
	  case PRE_MODIFY: case POST_MODIFY:
	    output_address (VOIDmode, XEXP (addr, 1)); break;
	  case PLUS:
	    if (output_scaled)
	      {
		output_address (VOIDmode,
				plus_constant (Pmode, XEXP (addr, 0),
					       (INTVAL (XEXP (addr, 1))
						>> (size == 2 ? 1 : 2))));
		output_scaled = 0;
	      }
	    else
	      output_address (VOIDmode, addr);
	    break;
	  default:
	    if (flag_pic && CONSTANT_ADDRESS_P (addr))
	      arc_output_pic_addr_const (file, addr, code);
	    else
	      output_address (VOIDmode, addr);
	    break;
	  }
	fputc (']', file);
	break;
      }
    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  long l;

	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);
	  fprintf (file, "0x%08lx", l);
	  break;
	}
      /* FALLTHRU */
      /* Let output_addr_const deal with it.  */
    default :
      if (flag_pic
	  || (GET_CODE (x) == CONST
	      && GET_CODE (XEXP (x, 0)) == UNSPEC
	      && (XINT (XEXP (x, 0), 1) == UNSPEC_TLS_OFF
		  || XINT (XEXP (x, 0), 1) == UNSPEC_TLS_GD))
	  || (GET_CODE (x) == CONST
	      && GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == UNSPEC
	      && (XINT (XEXP (XEXP (x, 0), 0), 1) == UNSPEC_TLS_OFF
		  || XINT (XEXP (XEXP (x, 0), 0), 1) == UNSPEC_TLS_GD)))
	arc_output_pic_addr_const (file, x, code);
      else
	{
	  /* FIXME: Dirty way to handle @var@sda+const. Shd be handled
	     with asm_output_symbol_ref */
	  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
	    {
	      x = XEXP (x, 0);
	      output_addr_const (file, XEXP (x, 0));
	      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF && SYMBOL_REF_SMALL_P (XEXP (x, 0)))
		fprintf (file, "@sda");

	      if (GET_CODE (XEXP (x, 1)) != CONST_INT
		  || INTVAL (XEXP (x, 1)) >= 0)
		fprintf (file, "+");
	      output_addr_const (file, XEXP (x, 1));
	    }
	  else
	    output_addr_const (file, x);
	}
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_SMALL_P (x))
	fprintf (file, "@sda");
      break;
    }
}

/* Print a memory address as an operand to reference that memory location.  */

void
arc_print_operand_address (FILE *file , rtx addr)
{
  register rtx base, index = 0;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;
    case SYMBOL_REF :
      output_addr_const (file, addr);
      if (SYMBOL_REF_SMALL_P (addr))
	fprintf (file, "@sda");
      break;
    case PLUS :
      if (GET_CODE (XEXP (addr, 0)) == MULT)
	index = XEXP (XEXP (addr, 0), 0), base = XEXP (addr, 1);
      else if (CONST_INT_P (XEXP (addr, 0)))
	index = XEXP (addr, 0), base = XEXP (addr, 1);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);

      gcc_assert (OBJECT_P (base));
      arc_print_operand_address (file, base);
      if (CONSTANT_P (base) && CONST_INT_P (index))
	fputc ('+', file);
      else
	fputc (',', file);
      gcc_assert (OBJECT_P (index));
      arc_print_operand_address (file, index);
      break;
    case CONST:
      {
	rtx c = XEXP (addr, 0);

	if ((GET_CODE (c) == UNSPEC
	     && (XINT (c, 1) == UNSPEC_TLS_OFF
		 || XINT (c, 1) == UNSPEC_TLS_IE))
	    || (GET_CODE (c) == PLUS
		&& GET_CODE (XEXP (c, 0)) == UNSPEC
		&& (XINT (XEXP (c, 0), 1) == UNSPEC_TLS_OFF
		    || XINT (XEXP (c, 0), 1) == ARC_UNSPEC_GOTOFFPC)))
	  {
	    arc_output_pic_addr_const (file, c, 0);
	    break;
	  }
	gcc_assert (GET_CODE (c) == PLUS);
	gcc_assert (GET_CODE (XEXP (c, 0)) == SYMBOL_REF);
	gcc_assert (GET_CODE (XEXP (c, 1)) == CONST_INT);

	output_address (VOIDmode, XEXP (addr, 0));

	break;
      }
    case PRE_INC :
    case PRE_DEC :
      /* We shouldn't get here as we've lost the mode of the memory object
	 (which says how much to inc/dec by.  */
      gcc_unreachable ();
      break;
    default :
      if (flag_pic)
	arc_output_pic_addr_const (file, addr, 0);
      else
	output_addr_const (file, addr);
      break;
    }
}

/* Conditional execution support.

   This is based on the ARM port but for now is much simpler.

   A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   arc_ccfsm_advance (called by arc_final_prescan_insn), and controls the
   actions of PRINT_OPERAND.  The patterns in the .md file for the branch
   insns also have a hand in this.  */
/* The way we leave dealing with non-anulled or annull-false delay slot
   insns to the consumer is awkward.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: don't output this insn
   2: don't output this insn
   3: make insns conditional
   4: make insns conditional
   5: make insn conditional (only for outputting anulled delay slot insns)

   special value for cfun->machine->uid_ccfsm_state:
   6: return with but one insn before it since function start / call

   State transitions (state->state by whom, under what condition):
   0 -> 1 arc_ccfsm_advance, if insn is a conditional branch skipping over
          some instructions.
   0 -> 2 arc_ccfsm_advance, if insn is a conditional branch followed
          by zero or more non-jump insns and an unconditional branch with
	  the same target label as the condbranch.
   1 -> 3 branch patterns, after having not output the conditional branch
   2 -> 4 branch patterns, after having not output the conditional branch
   0 -> 5 branch patterns, for anulled delay slot insn.
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL, if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to
	  arc_ccfsm_target_label).
   4 -> 0 arc_ccfsm_advance, if `target' unconditional branch is reached
   3 -> 1 arc_ccfsm_advance, finding an 'else' jump skipping over some insns.
   5 -> 0 when outputting the delay slot insn

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   We also handle separating branches from sets of the condition code.
   This is done here because knowledge of the ccfsm state is required,
   we may not be outputting the branch.  */

/* arc_final_prescan_insn calls arc_ccfsm_advance to adjust arc_ccfsm_current,
   before letting final output INSN.  */

static void
arc_ccfsm_advance (rtx_insn *insn, struct arc_ccfsm *state)
{
  /* BODY will hold the body of INSN.  */
  register rtx body;

  /* This will be 1 if trying to repeat the trick (ie: do the `else' part of
     an if/then/else), and things need to be reversed.  */
  int reverse = 0;

  /* If we start with a return insn, we only succeed if we find another one.  */
  int seeking_return = 0;

  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx_insn *start_insn = insn;

  /* Type of the jump_insn. Brcc insns don't affect ccfsm changes,
     since they don't rely on a cmp preceding the.  */
  enum attr_type jump_insn_type;

  /* Allow -mdebug-ccfsm to turn this off so we can see how well it does.
     We can't do this in macro FINAL_PRESCAN_INSN because its called from
     final_scan_insn which has `optimize' as a local.  */
  if (optimize < 2 || TARGET_NO_COND_EXEC)
    return;

  /* Ignore notes and labels.  */
  if (!INSN_P (insn))
    return;
  body = PATTERN (insn);
  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (state->state == 4)
    {
      if (insn == state->target_insn)
	{
	  state->target_insn = NULL;
	  state->state = 0;
	}
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  Or in other words "we've done the
     `then' part, see if we can do the `else' part."  */
  if (state->state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* ??? Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == state->target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == SIMPLE_RETURN)
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == state->target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
	}
      else
	return;
    }

  if (GET_CODE (insn) != JUMP_INSN
      || GET_CODE (PATTERN (insn)) == ADDR_VEC
      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
    return;

 /* We can't predicate BRCC or loop ends.
    Also, when generating PIC code, and considering a medium range call,
    we can't predicate the call.  */
  jump_insn_type = get_attr_type (insn);
  if (jump_insn_type == TYPE_BRCC
      || jump_insn_type == TYPE_BRCC_NO_DELAY_SLOT
      || jump_insn_type == TYPE_LOOP_END
      || (jump_insn_type == TYPE_CALL && !get_attr_predicable (insn)))
    return;

  /* This jump might be paralleled with a clobber of the condition codes,
     the jump should always come first.  */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped = 0, fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      /* Nonzero if next insn must be the target label.  */
      int next_must_be_target_label_p;
      rtx_insn *this_insn = start_insn;
      rtx label = 0;

      /* Register the insn jumped to.  */
      if (reverse)
	{
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == SIMPLE_RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == SIMPLE_RETURN)
	{
	  seeking_return = 1;
	  then_not_else = FALSE;
	}
      else
	gcc_unreachable ();

      /* If this is a non-annulled branch with a delay slot, there is
	 no need to conditionalize the delay slot.  */
      if ((GET_CODE (PATTERN (NEXT_INSN (PREV_INSN (insn)))) == SEQUENCE)
	  && state->state == 0 && !INSN_ANNULLED_BRANCH_P (insn))
	{
	  this_insn = NEXT_INSN (this_insn);
	}
      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0, next_must_be_target_label_p = FALSE;
	   !fail && !succeed && insns_skipped < MAX_INSNS_SKIPPED;
	   insns_skipped++)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  if (next_must_be_target_label_p)
	    {
	      if (GET_CODE (this_insn) == BARRIER)
		continue;
	      if (GET_CODE (this_insn) == CODE_LABEL
		  && this_insn == label)
		{
		  state->state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;
	    }

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  state->state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.
		 If return insns are used then the last insn in a function
		 will be a barrier.  */
	      next_must_be_target_label_p = TRUE;
	      break;

	    case CALL_INSN:
	      /* Can handle a call insn if there are no insns after it.
		 IE: The next "insn" is the target label.  We don't have to
		 worry about delay slots as such insns are SEQUENCE's inside
		 INSN's.  ??? It is possible to handle such insns though.  */
	      if (get_attr_cond (this_insn) == COND_CANUSE)
		next_must_be_target_label_p = TRUE;
	      else
		fail = TRUE;
	      break;

	    case JUMP_INSN:
	      scanbody = PATTERN (this_insn);

	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* ??? Probably, the test for the SET and the PC are
		 unnecessary.  */

	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      state->state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		  else if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      else if (GET_CODE (scanbody) == SIMPLE_RETURN
		       && seeking_return)
		{
		  state->state = 2;
		  succeed = TRUE;
		}
	      else if (GET_CODE (scanbody) == PARALLEL)
		{
		  if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      break;

	    case INSN:
	      scanbody = PATTERN (this_insn);

	      /* We can only do this with insns that can use the condition
		 codes (and don't set them).  */
	      if (GET_CODE (scanbody) == SET
		  || GET_CODE (scanbody) == PARALLEL)
		{
		  if (get_attr_cond (this_insn) != COND_CANUSE)
		    fail = TRUE;
		}
	      /* We can't handle other insns like sequences.  */
	      else
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}

      if (succeed)
	{
	  if ((!seeking_return) && (state->state == 1 || reverse))
	    state->target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || state->state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
		{
		  this_insn = next_nonnote_insn (this_insn);

		  gcc_assert (!this_insn ||
			      (GET_CODE (this_insn) != BARRIER
			       && GET_CODE (this_insn) != CODE_LABEL));
		}
	      if (!this_insn)
		{
		  /* Oh dear! we ran off the end, give up.  */
		  extract_insn_cached (insn);
		  state->state = 0;
		  state->target_insn = NULL;
		  return;
		}
	      state->target_insn = this_insn;
	    }
	  else
	    gcc_unreachable ();

	  /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
	     what it was.  */
	  if (!reverse)
	    {
	      state->cond = XEXP (SET_SRC (body), 0);
	      state->cc = get_arc_condition_code (XEXP (SET_SRC (body), 0));
	    }

	  if (reverse || then_not_else)
	    state->cc = ARC_INVERSE_CONDITION_CODE (state->cc);
	}

      /* Restore recog_operand.  Getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 across this call; since the insn has been recognized already we
	 call insn_extract direct.  */
      extract_insn_cached (insn);
    }
}

/* Record that we are currently outputting label NUM with prefix PREFIX.
   It it's the label we're looking for, reset the ccfsm machinery.

   Called from ASM_OUTPUT_INTERNAL_LABEL.  */

static void
arc_ccfsm_at_label (const char *prefix, int num, struct arc_ccfsm *state)
{
  if (state->state == 3 && state->target_label == num
      && !strcmp (prefix, "L"))
    {
      state->state = 0;
      state->target_insn = NULL;
    }
}

/* We are considering a conditional branch with the condition COND.
   Check if we want to conditionalize a delay slot insn, and if so modify
   the ccfsm state accordingly.
   REVERSE says branch will branch when the condition is false.  */
void
arc_ccfsm_record_condition (rtx cond, bool reverse, rtx_insn *jump,
			    struct arc_ccfsm *state)
{
  rtx_insn *seq_insn = NEXT_INSN (PREV_INSN (jump));
  if (!state)
    state = &arc_ccfsm_current;

  gcc_assert (state->state == 0);
  if (seq_insn != jump)
    {
      rtx insn = XVECEXP (PATTERN (seq_insn), 0, 1);

      if (!as_a<rtx_insn *> (insn)->deleted ()
	  && INSN_ANNULLED_BRANCH_P (jump)
	  && (TARGET_AT_DBR_CONDEXEC || INSN_FROM_TARGET_P (insn)))
	{
	  state->cond = cond;
	  state->cc = get_arc_condition_code (cond);
	  if (!reverse)
	    arc_ccfsm_current.cc
	      = ARC_INVERSE_CONDITION_CODE (state->cc);
	  rtx pat = PATTERN (insn);
	  if (GET_CODE (pat) == COND_EXEC)
	    gcc_assert ((INSN_FROM_TARGET_P (insn)
			 ? ARC_INVERSE_CONDITION_CODE (state->cc) : state->cc)
			== get_arc_condition_code (XEXP (pat, 0)));
	  else
	    state->state = 5;
	}
    }
}

/* Update *STATE as we would when we emit INSN.  */

static void
arc_ccfsm_post_advance (rtx_insn *insn, struct arc_ccfsm *state)
{
  enum attr_type type;

  if (LABEL_P (insn))
    arc_ccfsm_at_label ("L", CODE_LABEL_NUMBER (insn), state);
  else if (JUMP_P (insn)
	   && GET_CODE (PATTERN (insn)) != ADDR_VEC
	   && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
	   && ((type = get_attr_type (insn)) == TYPE_BRANCH
	       || ((type == TYPE_UNCOND_BRANCH
		    || type == TYPE_RETURN)
		   && ARC_CCFSM_BRANCH_DELETED_P (state))))
    {
      if (ARC_CCFSM_BRANCH_DELETED_P (state))
	ARC_CCFSM_RECORD_BRANCH_DELETED (state);
      else
	{
	  rtx src = SET_SRC (PATTERN (insn));
	  arc_ccfsm_record_condition (XEXP (src, 0), XEXP (src, 1) == pc_rtx,
				      insn, state);
	}
    }
  else if (arc_ccfsm_current.state == 5)
    arc_ccfsm_current.state = 0;
}

/* Return true if the current insn, which is a conditional branch, is to be
   deleted.  */

bool
arc_ccfsm_branch_deleted_p (void)
{
  return ARC_CCFSM_BRANCH_DELETED_P (&arc_ccfsm_current);
}

/* Record a branch isn't output because subsequent insns can be
   conditionalized.  */

void
arc_ccfsm_record_branch_deleted (void)
{
  ARC_CCFSM_RECORD_BRANCH_DELETED (&arc_ccfsm_current);
}

/* During insn output, indicate if the current insn is predicated.  */

bool
arc_ccfsm_cond_exec_p (void)
{
  return (cfun->machine->prescan_initialized
	  && ARC_CCFSM_COND_EXEC_P (&arc_ccfsm_current));
}

/* Like next_active_insn, but return NULL if we find an ADDR_(DIFF_)VEC,
   and look inside SEQUENCEs.  */

static rtx_insn *
arc_next_active_insn (rtx_insn *insn, struct arc_ccfsm *statep)
{
  rtx pat;

  do
    {
      if (statep)
	arc_ccfsm_post_advance (insn, statep);
      insn = NEXT_INSN (insn);
      if (!insn || BARRIER_P (insn))
	return NULL;
      if (statep)
	arc_ccfsm_advance (insn, statep);
    }
  while (NOTE_P (insn)
	 || (cfun->machine->arc_reorg_started
	     && LABEL_P (insn) && !label_to_alignment (insn))
	 || (NONJUMP_INSN_P (insn)
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (!LABEL_P (insn))
    {
      gcc_assert (INSN_P (insn));
      pat = PATTERN (insn);
      if (GET_CODE (pat) == ADDR_VEC || GET_CODE (pat) == ADDR_DIFF_VEC)
	return NULL;
      if (GET_CODE (pat) == SEQUENCE)
	return as_a <rtx_insn *> (XVECEXP (pat, 0, 0));
    }
  return insn;
}

/* When deciding if an insn should be output short, we want to know something
   about the following insns:
   - if another insn follows which we know we can output as a short insn
     before an alignment-sensitive point, we can output this insn short:
     the decision about the eventual alignment can be postponed.
   - if a to-be-aligned label comes next, we should output this insn such
     as to get / preserve 4-byte alignment.
   - if a likely branch without delay slot insn, or a call with an immediately
     following short insn comes next, we should out output this insn such as to
     get / preserve 2 mod 4 unalignment.
   - do the same for a not completely unlikely branch with a short insn
     following before any other branch / label.
   - in order to decide if we are actually looking at a branch, we need to
     call arc_ccfsm_advance.
   - in order to decide if we are looking at a short insn, we should know
     if it is conditionalized.  To a first order of approximation this is
     the case if the state from arc_ccfsm_advance from before this insn
     indicates the insn is conditionalized.  However, a further refinement
     could be to not conditionalize an insn if the destination register(s)
     is/are dead in the non-executed case.  */
/* Return non-zero if INSN should be output as a short insn.  UNALIGN is
   zero if the current insn is aligned to a 4-byte-boundary, two otherwise.
   If CHECK_ATTR is greater than 0, check the iscompact attribute first.  */

int
arc_verify_short (rtx_insn *insn, int, int check_attr)
{
  enum attr_iscompact iscompact;
  struct machine_function *machine;

  if (check_attr > 0)
    {
      iscompact = get_attr_iscompact (insn);
      if (iscompact == ISCOMPACT_FALSE)
	return 0;
    }
  machine = cfun->machine;

  if (machine->force_short_suffix >= 0)
    return machine->force_short_suffix;

  return (get_attr_length (insn) & 2) != 0;
}

/* When outputting an instruction (alternative) that can potentially be short,
   output the short suffix if the insn is in fact short, and update
   cfun->machine->unalign accordingly.  */

static void
output_short_suffix (FILE *file)
{
  rtx_insn *insn = current_output_insn;

  if (arc_verify_short (insn, cfun->machine->unalign, 1))
    {
      fprintf (file, "_s");
      cfun->machine->unalign ^= 2;
    }
  /* Restore recog_operand.  */
  extract_insn_cached (insn);
}

/* Implement FINAL_PRESCAN_INSN.  */

void
arc_final_prescan_insn (rtx_insn *insn, rtx *opvec ATTRIBUTE_UNUSED,
			int noperands ATTRIBUTE_UNUSED)
{
  if (TARGET_DUMPISIZE)
    fprintf (asm_out_file, "\n; at %04x\n", INSN_ADDRESSES (INSN_UID (insn)));

  /* Output a nop if necessary to prevent a hazard.
     Don't do this for delay slots: inserting a nop would
     alter semantics, and the only time we would find a hazard is for a
     call function result - and in that case, the hazard is spurious to
     start with.  */
  if (PREV_INSN (insn)
      && PREV_INSN (NEXT_INSN (insn)) == insn
      && arc_hazard (prev_real_insn (insn), insn))
    {
      current_output_insn =
	emit_insn_before (gen_nop (), NEXT_INSN (PREV_INSN (insn)));
      final_scan_insn (current_output_insn, asm_out_file, optimize, 1, NULL);
      current_output_insn = insn;
    }
  /* Restore extraction data which might have been clobbered by arc_hazard.  */
  extract_constrain_insn_cached (insn);

  if (!cfun->machine->prescan_initialized)
    {
      /* Clear lingering state from branch shortening.  */
      memset (&arc_ccfsm_current, 0, sizeof arc_ccfsm_current);
      cfun->machine->prescan_initialized = 1;
    }
  arc_ccfsm_advance (insn, &arc_ccfsm_current);

  cfun->machine->size_reason = 0;
}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   All eliminations are permissible. If we need a frame
   pointer, we must eliminate ARG_POINTER_REGNUM into
   FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM.  */

static bool
arc_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return ((to == FRAME_POINTER_REGNUM) || !arc_frame_pointer_needed ());
}

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */

int
arc_initial_elimination_offset (int from, int to)
{
  if (! cfun->machine->frame_info.initialized)
     arc_compute_frame_size (get_frame_size ());

  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      return (cfun->machine->frame_info.extra_size
	      + cfun->machine->frame_info.reg_size);
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      return (cfun->machine->frame_info.total_size
	      - cfun->machine->frame_info.pretend_size);
    }

  if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
    {
      return (cfun->machine->frame_info.total_size
	      - (cfun->machine->frame_info.pretend_size
	      + cfun->machine->frame_info.extra_size
	      + cfun->machine->frame_info.reg_size));
    }

  gcc_unreachable ();
}

static bool
arc_frame_pointer_required (void)
{
 return cfun->calls_alloca;
}


/* Return the destination address of a branch.  */

int
branch_dest (rtx branch)
{
  rtx pat = PATTERN (branch);
  rtx dest = (GET_CODE (pat) == PARALLEL
	      ? SET_SRC (XVECEXP (pat, 0, 0)) : SET_SRC (pat));
  int dest_uid;

  if (GET_CODE (dest) == IF_THEN_ELSE)
    dest = XEXP (dest, XEXP (dest, 1) == pc_rtx ? 2 : 1);

  dest = XEXP (dest, 0);
  dest_uid = INSN_UID (dest);

  return INSN_ADDRESSES (dest_uid);
}


/* Implement TARGET_ENCODE_SECTION_INFO hook.  */

static void
arc_encode_section_info (tree decl, rtx rtl, int first)
{
  /* For sdata, SYMBOL_FLAG_LOCAL and SYMBOL_FLAG_FUNCTION.
     This clears machine specific flags, so has to come first.  */
  default_encode_section_info (decl, rtl, first);

  /* Check if it is a function, and whether it has the
     [long/medium/short]_call attribute specified.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      rtx symbol = XEXP (rtl, 0);
      int flags = SYMBOL_REF_FLAGS (symbol);

      tree attr = (TREE_TYPE (decl) != error_mark_node
		   ? TYPE_ATTRIBUTES (TREE_TYPE (decl)) : NULL_TREE);
      tree long_call_attr = lookup_attribute ("long_call", attr);
      tree medium_call_attr = lookup_attribute ("medium_call", attr);
      tree short_call_attr = lookup_attribute ("short_call", attr);

      if (long_call_attr != NULL_TREE)
	flags |= SYMBOL_FLAG_LONG_CALL;
      else if (medium_call_attr != NULL_TREE)
	flags |= SYMBOL_FLAG_MEDIUM_CALL;
      else if (short_call_attr != NULL_TREE)
	flags |= SYMBOL_FLAG_SHORT_CALL;

      SYMBOL_REF_FLAGS (symbol) = flags;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      rtx symbol = XEXP (rtl, 0);

      tree attr = (TREE_TYPE (decl) != error_mark_node
		   ? DECL_ATTRIBUTES (decl) : NULL_TREE);

      tree sec_attr = lookup_attribute ("section", attr);
      if (sec_attr)
	{
	  const char *sec_name
	    = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (sec_attr)));
	  if (strcmp (sec_name, ".cmem") == 0
	      || strcmp (sec_name, ".cmem_shared") == 0
	      || strcmp (sec_name, ".cmem_private") == 0)
	    SYMBOL_REF_FLAGS (symbol) |= SYMBOL_FLAG_CMEM;
	}
    }
}

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

static void arc_internal_label (FILE *stream, const char *prefix, unsigned long labelno)
{
  if (cfun)
    arc_ccfsm_at_label (prefix, labelno, &arc_ccfsm_current);
  default_internal_label (stream, prefix, labelno);
}

/* Set the cpu type and print out other fancy things,
   at the top of the file.  */

static void arc_file_start (void)
{
  default_file_start ();
  fprintf (asm_out_file, "\t.cpu %s\n", arc_cpu_string);
}

/* Cost functions.  */

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
arc_rtx_costs (rtx x, machine_mode mode, int outer_code,
	       int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  int code = GET_CODE (x);

  switch (code)
    {
      /* Small integers are as cheap as registers.  */
    case CONST_INT:
      {
	bool nolimm = false; /* Can we do without long immediate?  */
	bool fast = false; /* Is the result available immediately?  */
	bool condexec = false; /* Does this allow conditiobnal execution?  */
	bool compact = false; /* Is a 16 bit opcode available?  */
	/* CONDEXEC also implies that we can have an unconditional
	   3-address operation.  */

	nolimm = compact = condexec = false;
	if (UNSIGNED_INT6 (INTVAL (x)))
	  nolimm = condexec = compact = true;
	else
	  {
	    if (SMALL_INT (INTVAL (x)))
	      nolimm = fast = true;
	    switch (outer_code)
	      {
	      case AND: /* bclr, bmsk, ext[bw] */
		if (satisfies_constraint_Ccp (x) /* bclr */
		    || satisfies_constraint_C1p (x) /* bmsk */)
		  nolimm = fast = condexec = compact = true;
		break;
	      case IOR: /* bset */
		if (satisfies_constraint_C0p (x)) /* bset */
		  nolimm = fast = condexec = compact = true;
		break;
	      case XOR:
		if (satisfies_constraint_C0p (x)) /* bxor */
		  nolimm = fast = condexec = true;
		break;
	      case SET:
		if (satisfies_constraint_Crr (x)) /* ror b,u6 */
		  nolimm = true;
	      default:
		break;
	      }
	  }
	/* FIXME: Add target options to attach a small cost if
	   condexec / compact is not true.  */
	if (nolimm)
	  {
	    *total = 0;
	    return true;
	  }
      }
      /* FALLTHRU */

      /*  4 byte values can be fetched as immediate constants -
	  let's give that the cost of an extra insn.  */
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (1);
      return true;

    case CONST_DOUBLE:
      {
	rtx first, second;

	if (TARGET_DPFP)
	  {
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }
	split_double (x, &first, &second);
	*total = COSTS_N_INSNS (!SMALL_INT (INTVAL (first))
				+ !SMALL_INT (INTVAL (second)));
	return true;
      }

    /* Encourage synth_mult to find a synthetic multiply when reasonable.
       If we need more than 12 insns to do a multiply, then go out-of-line,
       since the call overhead will be < 10% of the cost of the multiply.  */
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (TARGET_BARREL_SHIFTER)
	{
	  /* If we want to shift a constant, we need a LIMM.  */
	  /* ??? when the optimizers want to know if a constant should be
	     hoisted, they ask for the cost of the constant.  OUTER_CODE is
	     insufficient context for shifts since we don't know which operand
	     we are looking at.  */
	  if (CONSTANT_P (XEXP (x, 0)))
	    {
	      *total += (COSTS_N_INSNS (2)
			 + rtx_cost (XEXP (x, 1), mode, (enum rtx_code) code,
				     0, speed));
	      return true;
	    }
	  *total = COSTS_N_INSNS (1);
	}
      else if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	*total = COSTS_N_INSNS (16);
      else
	{
	  *total = COSTS_N_INSNS (INTVAL (XEXP ((x), 1)));
	  /* ??? want_to_gcse_p can throw negative shift counts at us,
	     and then panics when it gets a negative cost as result.
	     Seen for gcc.c-torture/compile/20020710-1.c -Os .  */
	  if (*total < 0)
	    *total = 0;
	}
      return false;

    case DIV:
    case UDIV:
      if (speed)
	*total = COSTS_N_INSNS(30);
      else
	*total = COSTS_N_INSNS(1);
	return false;

    case MULT:
      if ((TARGET_DPFP && GET_MODE (x) == DFmode))
	*total = COSTS_N_INSNS (1);
      else if (speed)
	*total= arc_multcost;
      /* We do not want synth_mult sequences when optimizing
	 for size.  */
      else if (TARGET_MUL64_SET || TARGET_ARC700_MPY)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (2);
      return false;
    case PLUS:
      if ((GET_CODE (XEXP (x, 0)) == ASHIFT
	   && _1_2_3_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
          || (GET_CODE (XEXP (x, 0)) == MULT
              && _2_4_8_operand (XEXP (XEXP (x, 0), 1), VOIDmode)))
	{
	  *total += (rtx_cost (XEXP (x, 1), mode, PLUS, 0, speed)
		     + rtx_cost (XEXP (XEXP (x, 0), 0), mode, PLUS, 1, speed));
	  return true;
	}
      return false;
    case MINUS:
      if ((GET_CODE (XEXP (x, 1)) == ASHIFT
	   && _1_2_3_operand (XEXP (XEXP (x, 1), 1), VOIDmode))
          || (GET_CODE (XEXP (x, 1)) == MULT
              && _2_4_8_operand (XEXP (XEXP (x, 1), 1), VOIDmode)))
	{
	  *total += (rtx_cost (XEXP (x, 0), mode, PLUS, 0, speed)
		     + rtx_cost (XEXP (XEXP (x, 1), 0), mode, PLUS, 1, speed));
	  return true;
	}
      return false;
    case COMPARE:
      {
	rtx op0 = XEXP (x, 0);
	rtx op1 = XEXP (x, 1);

	if (GET_CODE (op0) == ZERO_EXTRACT && op1 == const0_rtx
	    && XEXP (op0, 1) == const1_rtx)
	  {
	    /* btst / bbit0 / bbit1:
	       Small integers and registers are free; everything else can
	       be put in a register.  */
	    mode = GET_MODE (XEXP (op0, 0));
	    *total = (rtx_cost (XEXP (op0, 0), mode, SET, 1, speed)
		      + rtx_cost (XEXP (op0, 2), mode, SET, 1, speed));
	    return true;
	  }
	if (GET_CODE (op0) == AND && op1 == const0_rtx
	    && satisfies_constraint_C1p (XEXP (op0, 1)))
	  {
	    /* bmsk.f */
	    *total = rtx_cost (XEXP (op0, 0), VOIDmode, SET, 1, speed);
	    return true;
	  }
	/* add.f  */
	if (GET_CODE (op1) == NEG)
	  {
	    /* op0 might be constant, the inside of op1 is rather
	       unlikely to be so.  So swapping the operands might lower
	       the cost.  */
	    mode = GET_MODE (op0);
	    *total = (rtx_cost (op0, mode, PLUS, 1, speed)
		      + rtx_cost (XEXP (op1, 0), mode, PLUS, 0, speed));
	  }
	return false;
      }
    case EQ: case NE:
      if (outer_code == IF_THEN_ELSE
	  && GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	  && XEXP (x, 1) == const0_rtx
	  && XEXP (XEXP (x, 0), 1) == const1_rtx)
	{
	  /* btst / bbit0 / bbit1:
	     Small integers and registers are free; everything else can
	     be put in a register.  */
	  rtx op0 = XEXP (x, 0);

	  mode = GET_MODE (XEXP (op0, 0));
	  *total = (rtx_cost (XEXP (op0, 0), mode, SET, 1, speed)
		    + rtx_cost (XEXP (op0, 2), mode, SET, 1, speed));
	  return true;
	}
      /* Fall through.  */
    /* scc_insn expands into two insns.  */
    case GTU: case GEU: case LEU:
      if (mode == SImode)
	*total += COSTS_N_INSNS (1);
      return false;
    case LTU: /* might use adc.  */
      if (mode == SImode)
	*total += COSTS_N_INSNS (1) - 1;
      return false;
    default:
      return false;
    }
}

/* Return true if ADDR is a valid pic address.
   A valid pic address on arc should look like
   const (unspec (SYMBOL_REF/LABEL) (ARC_UNSPEC_GOTOFF/ARC_UNSPEC_GOT))  */

bool
arc_legitimate_pic_addr_p (rtx addr)
{
  if (GET_CODE (addr) != CONST)
    return false;

  addr = XEXP (addr, 0);


  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 1)) != CONST_INT)
	return false;
      addr = XEXP (addr, 0);
    }

  if (GET_CODE (addr) != UNSPEC
      || XVECLEN (addr, 0) != 1)
    return false;

  /* Must be one of @GOT, @GOTOFF, @GOTOFFPC, @tlsgd, tlsie.  */
  if (XINT (addr, 1) != ARC_UNSPEC_GOT
      && XINT (addr, 1) != ARC_UNSPEC_GOTOFF
      && XINT (addr, 1) != ARC_UNSPEC_GOTOFFPC
      && XINT (addr, 1) != UNSPEC_TLS_GD
      && XINT (addr, 1) != UNSPEC_TLS_IE)
    return false;

  if (GET_CODE (XVECEXP (addr, 0, 0)) != SYMBOL_REF
      && GET_CODE (XVECEXP (addr, 0, 0)) != LABEL_REF)
    return false;

  return true;
}



/* Return true if OP contains a symbol reference.  */

static bool
symbolic_reference_mentioned_p (rtx op)
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return true;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return true;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return true;
    }

  return false;
}

/* Return true if OP contains a SYMBOL_REF that is not wrapped in an unspec.
   If SKIP_LOCAL is true, skip symbols that bind locally.
   This is used further down in this file, and, without SKIP_LOCAL,
   in the addsi3 / subsi3 expanders when generating PIC code.  */

bool
arc_raw_symbolic_reference_mentioned_p (rtx op, bool skip_local)
{
  register const char *fmt;
  register int i;

  if (GET_CODE(op) == UNSPEC)
    return false;

  if (GET_CODE (op) == SYMBOL_REF)
    {
      if (SYMBOL_REF_TLS_MODEL (op))
	return true;
      if (!flag_pic)
	return false;
      tree decl = SYMBOL_REF_DECL (op);
      return !skip_local || !decl || !default_binds_local_p (decl);
    }

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (arc_raw_symbolic_reference_mentioned_p (XVECEXP (op, i, j),
							skip_local))
	      return true;
	}

      else if (fmt[i] == 'e'
	       && arc_raw_symbolic_reference_mentioned_p (XEXP (op, i),
							  skip_local))
	return true;
    }

  return false;
}

/* Get the thread pointer.  */

static rtx
arc_get_tp (void)
{
   /* If arc_tp_regno has been set, we can use that hard register
      directly as a base register.  */
  if (arc_tp_regno != -1)
    return gen_rtx_REG (Pmode, arc_tp_regno);

  /* Otherwise, call __read_tp.  Copy the result to a pseudo to avoid
     conflicts with function arguments / results.  */
  rtx reg = gen_reg_rtx (Pmode);
  emit_insn (gen_tls_load_tp_soft ());
  emit_move_insn (reg, gen_rtx_REG (Pmode, R0_REG));
  return reg;
}

/* Helper to be used by TLS Global dynamic model.  */

static rtx
arc_emit_call_tls_get_addr (rtx sym, int reloc, rtx eqv)
{
  rtx r0 = gen_rtx_REG (Pmode, R0_REG);
  rtx call_fusage = NULL_RTX;

  start_sequence ();

  rtx x = arc_unspec_offset (sym, reloc);
  emit_move_insn (r0, x);
  use_reg (&call_fusage, r0);

  gcc_assert (reloc == UNSPEC_TLS_GD);
  rtx call_insn = emit_call_insn (gen_tls_gd_get_addr (sym));
  /* Should we set RTL_CONST_CALL_P?  We read memory, but not in a
     way that the application should care.  */
  RTL_PURE_CALL_P (call_insn) = 1;
  add_function_usage_to (call_insn, call_fusage);

  rtx_insn *insns = get_insns ();
  end_sequence ();

  rtx dest = gen_reg_rtx (Pmode);
  emit_libcall_block (insns, dest, r0, eqv);
  return dest;
}

#define DTPOFF_ZERO_SYM ".tdata"

/* Return a legitimized address for ADDR,
   which is a SYMBOL_REF with tls_model MODEL.  */

static rtx
arc_legitimize_tls_address (rtx addr, enum tls_model model)
{
  if (!flag_pic && model == TLS_MODEL_LOCAL_DYNAMIC)
    model = TLS_MODEL_LOCAL_EXEC;

  switch (model)
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
      rtx base;
      tree decl;
      const char *base_name;
      rtvec v;

      decl = SYMBOL_REF_DECL (addr);
      base_name = DTPOFF_ZERO_SYM;
      if (decl && bss_initializer_p (decl))
	base_name = ".tbss";

      base = gen_rtx_SYMBOL_REF (Pmode, base_name);
      if (strcmp (base_name, DTPOFF_ZERO_SYM) == 0)
	{
	  if (!flag_pic)
	    goto local_exec;
	  v = gen_rtvec (1, addr);
	}
      else
	v = gen_rtvec (2, addr, base);
      addr = gen_rtx_UNSPEC (Pmode, v, UNSPEC_TLS_OFF);
      addr = gen_rtx_CONST (Pmode, addr);
      base = arc_legitimize_tls_address (base, TLS_MODEL_GLOBAL_DYNAMIC);
      return gen_rtx_PLUS (Pmode, force_reg (Pmode, base), addr);

    case TLS_MODEL_GLOBAL_DYNAMIC:
      return arc_emit_call_tls_get_addr (addr, UNSPEC_TLS_GD, addr);

    case TLS_MODEL_INITIAL_EXEC:
      addr = arc_unspec_offset (addr, UNSPEC_TLS_IE);
      addr = copy_to_mode_reg (Pmode, gen_const_mem (Pmode, addr));
      return gen_rtx_PLUS (Pmode, arc_get_tp (), addr);

    case TLS_MODEL_LOCAL_EXEC:
    local_exec:
      addr = arc_unspec_offset (addr, UNSPEC_TLS_OFF);
      return gen_rtx_PLUS (Pmode, arc_get_tp (), addr);
    default:
      gcc_unreachable ();
    }
}

/* Legitimize a pic address reference in ORIG.
   The return value is the legitimated address.
   If OLDX is non-zero, it is the target to assign the address to first.  */

static rtx
arc_legitimize_pic_address (rtx orig, rtx oldx)
{
  rtx addr = orig;
  rtx pat = orig;
  rtx base;

  if (oldx == orig)
    oldx = NULL;

  if (GET_CODE (addr) == LABEL_REF)
    ; /* Do nothing.  */
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      enum tls_model model = SYMBOL_REF_TLS_MODEL (addr);
      if (model != 0)
	return arc_legitimize_tls_address (addr, model);
      else if (!flag_pic)
	return orig;
      else if (CONSTANT_POOL_ADDRESS_P (addr) || SYMBOL_REF_LOCAL_P (addr))
	return arc_unspec_offset (addr, ARC_UNSPEC_GOTOFFPC);

      /* This symbol must be referenced via a load from the Global
	 Offset Table (@GOTPC).  */
      pat = arc_unspec_offset (addr, ARC_UNSPEC_GOT);
      pat = gen_const_mem (Pmode, pat);

      if (oldx == NULL)
	oldx = gen_reg_rtx (Pmode);

      emit_move_insn (oldx, pat);
      pat = oldx;
    }
  else
    {
      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) == UNSPEC)
	    {
	      /* Check that the unspec is one of the ones we generate?  */
	      return orig;
	    }
	  /* fwprop is placing in the REG_EQUIV notes constant pic
	     unspecs expressions.  Then, loop may use these notes for
	     optimizations resulting in complex patterns that are not
	     supported by the current implementation. The following
	     two if-cases are simplifying the complex patters to
	     simpler ones.  */
	  else if (GET_CODE (addr) == MINUS)
	    {
	      rtx op0 = XEXP (addr, 0);
	      rtx op1 = XEXP (addr, 1);
	      gcc_assert (oldx);
	      gcc_assert (GET_CODE (op1) == UNSPEC);

	      emit_move_insn (oldx,
			      gen_rtx_CONST (SImode,
					     arc_legitimize_pic_address (op1,
									 NULL_RTX)));
	      emit_insn (gen_rtx_SET (oldx, gen_rtx_MINUS (SImode, op0, oldx)));
	      return oldx;

	    }
	  else if (GET_CODE (addr) != PLUS)
	    {
	      rtx tmp = XEXP (addr, 0);
	      enum rtx_code code = GET_CODE (addr);

	      /* It only works for UNARY operations.  */
	      gcc_assert (UNARY_P (addr));
	      gcc_assert (GET_CODE (tmp) == UNSPEC);
	      gcc_assert (oldx);

	      emit_move_insn
		(oldx,
		 gen_rtx_CONST (SImode,
				arc_legitimize_pic_address (tmp,
							    NULL_RTX)));

	      emit_insn (gen_rtx_SET (oldx,
				      gen_rtx_fmt_ee (code, SImode,
						      oldx, const0_rtx)));

	      return oldx;
	    }
	  else
	    {
	      gcc_assert (GET_CODE (addr) == PLUS);
	      if (GET_CODE (XEXP (addr, 0)) == UNSPEC)
		return orig;
	    }
	}

      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  base = arc_legitimize_pic_address (op0, oldx);
	  pat  = arc_legitimize_pic_address (op1,
					     base == oldx ? NULL_RTX : oldx);

	  if (base == op0 && pat == op1)
	    return orig;

	  if (GET_CODE (pat) == CONST_INT)
	    pat = plus_constant (Pmode, base, INTVAL (pat));
	  else
	    {
	      if (GET_CODE (pat) == PLUS && CONSTANT_P (XEXP (pat, 1)))
		{
		  base = gen_rtx_PLUS (Pmode, base, XEXP (pat, 0));
		  pat = XEXP (pat, 1);
		}
	      pat = gen_rtx_PLUS (Pmode, base, pat);
	    }
	}
    }

 return pat;
}

/* Output address constant X to FILE, taking PIC into account.  */

static void
arc_output_pic_addr_const (FILE * file, rtx x, int code)
{
  char buf[256];

 restart:
  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	gcc_unreachable ();
      break;

    case SYMBOL_REF:
      output_addr_const (file, x);

      /* Local functions do not get references through the PLT.  */
      if (code == 'P' && ! SYMBOL_REF_LOCAL_P (x))
	fputs ("@plt", file);
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      assemble_name (file, buf);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (file, buf);
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
	  else if  (CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* FIXME: Not needed here.  */
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	  fprintf (file, "+");
	  arc_output_pic_addr_const (file, XEXP (x, 0), code);
	}
      else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  arc_output_pic_addr_const (file, XEXP (x, 0), code);
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else
	gcc_unreachable();
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      fprintf (file, "-");
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  fprintf (file, "(");
	  arc_output_pic_addr_const (file, XEXP (x, 1), code);
	  fprintf (file, ")");
	}
      else
	arc_output_pic_addr_const (file, XEXP (x, 1), code);
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      arc_output_pic_addr_const (file, XEXP (x, 0), code);
      break;


    case UNSPEC:
      const char *suffix;
      bool pcrel; pcrel = false;
      rtx base; base = NULL;
      gcc_assert (XVECLEN (x, 0) >= 1);
      switch (XINT (x, 1))
	{
	case ARC_UNSPEC_GOT:
	  suffix = "@gotpc", pcrel = true;
	  break;
	case ARC_UNSPEC_GOTOFF:
	  suffix = "@gotoff";
	  break;
	case ARC_UNSPEC_GOTOFFPC:
	  suffix = "@pcl",   pcrel = true;
	  break;
	case ARC_UNSPEC_PLT:
	  suffix = "@plt";
	  break;
	case UNSPEC_TLS_GD:
	  suffix = "@tlsgd", pcrel = true;
	  break;
	case UNSPEC_TLS_IE:
	  suffix = "@tlsie", pcrel = true;
	  break;
	case UNSPEC_TLS_OFF:
	  if (XVECLEN (x, 0) == 2)
	    base = XVECEXP (x, 0, 1);
	  if (SYMBOL_REF_TLS_MODEL (XVECEXP (x, 0, 0)) == TLS_MODEL_LOCAL_EXEC
	      || (!flag_pic && !base))
	    suffix = "@tpoff";
	  else
	    suffix = "@dtpoff";
	  break;
	default:
	  suffix = "@invalid";
	  output_operand_lossage ("invalid UNSPEC as operand: %d", XINT (x,1));
	  break;
	}
      if (pcrel)
	fputs ("pcl,", file);
      arc_output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
      fputs (suffix, file);
      if (base)
	arc_output_pic_addr_const (file, base, code);
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

#define SYMBOLIC_CONST(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Emit insns to move operands[1] into operands[0].  */

static void
prepare_pic_move (rtx *operands, machine_mode)
{
  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1])
      && flag_pic)
    operands[1] = force_reg (Pmode, operands[1]);
  else
    {
      rtx temp = (reload_in_progress ? operands[0]
		  : flag_pic? gen_reg_rtx (Pmode) : NULL_RTX);
      operands[1] = arc_legitimize_pic_address (operands[1], temp);
    }
}


/* The function returning the number of words, at the beginning of an
   argument, must be put in registers.  The returned value must be
   zero for arguments that are passed entirely in registers or that
   are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in
   registers and partially in memory.  On these machines, typically
   the first N words of arguments are passed in registers, and the
   rest on the stack.  If a multi-word argument (a `double' or a
   structure) crosses that boundary, its first few words must be
   passed in registers and the rest must be pushed.  This function
   tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register
   to be used by the caller for this argument; likewise
   `FUNCTION_INCOMING_ARG', for the called function.

   The function is used to implement macro FUNCTION_ARG_PARTIAL_NREGS.  */

/* If REGNO is the least arg reg available then what is the total number of arg
   regs available.  */
#define GPR_REST_ARG_REGS(REGNO) \
  ((REGNO) <= MAX_ARC_PARM_REGS ? MAX_ARC_PARM_REGS - (REGNO) : 0 )

/* Since arc parm regs are contiguous.  */
#define ARC_NEXT_ARG_REG(REGNO) ( (REGNO) + 1 )

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
arc_arg_partial_bytes (cumulative_args_t cum_v, machine_mode mode,
		       tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = (mode == BLKmode
	       ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode));
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int arg_num = *cum;
  int ret;

  arg_num = ROUND_ADVANCE_CUM (arg_num, mode, type);
  ret = GPR_REST_ARG_REGS (arg_num);

  /* ICEd at function.c:2361, and ret is copied to data->partial */
    ret = (ret >= words ? 0 : ret * UNITS_PER_WORD);

  return ret;
}

/* This function is used to control a function argument is passed in a
   register, and which register.

   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes
   (in a way defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE)
   all of the previous arguments so far passed in registers; MODE, the
   machine mode of the argument; TYPE, the data type of the argument
   as a tree node or 0 if that is not known (which happens for C
   support library functions); and NAMED, which is 1 for an ordinary
   argument and 0 for nameless arguments that correspond to `...' in
   the called function's prototype.

   The returned value should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the
   argument on the stack.

   For machines like the Vax and 68000, where normally all arguments
   are pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine
   where some arguments are usually passed in registers, is to cause
   nameless arguments to be passed on the stack instead.  This is done
   by making the function return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the
   definition of this function to determine if this argument is of a
   type that must be passed in the stack.  If `REG_PARM_STACK_SPACE'
   is not defined and the function returns non-zero for such an
   argument, the compiler will abort.  If `REG_PARM_STACK_SPACE' is
   defined, the argument will be computed in the stack and then loaded
   into a register.

   The function is used to implement macro FUNCTION_ARG.  */
/* On the ARC the first MAX_ARC_PARM_REGS args are normally in registers
   and the rest are pushed.  */

static rtx
arc_function_arg (cumulative_args_t cum_v,
		  machine_mode mode,
		  const_tree type ATTRIBUTE_UNUSED,
		  bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int arg_num = *cum;
  rtx ret;
  const char *debstr ATTRIBUTE_UNUSED;

  arg_num = ROUND_ADVANCE_CUM (arg_num, mode, type);
  /* Return a marker for use in the call instruction.  */
  if (mode == VOIDmode)
    {
      ret = const0_rtx;
      debstr = "<0>";
    }
  else if (GPR_REST_ARG_REGS (arg_num) > 0)
    {
      ret = gen_rtx_REG (mode, arg_num);
      debstr = reg_names [arg_num];
    }
  else
    {
      ret = NULL_RTX;
      debstr = "memory";
    }
  return ret;
}

/* The function to update the summarizer variable *CUM to advance past
   an argument in the argument list.  The values MODE, TYPE and NAMED
   describe that argument.  Once this is done, the variable *CUM is
   suitable for analyzing the *following* argument with
   `FUNCTION_ARG', etc.

   This function need not do anything if the argument in question was
   passed on the stack.  The compiler knows how to track the amount of
   stack space used for arguments without any special help.

   The function is used to implement macro FUNCTION_ARG_ADVANCE.  */
/* For the ARC: the cum set here is passed on to function_arg where we
   look at its value and say which reg to use. Strategy: advance the
   regnumber here till we run out of arg regs, then set *cum to last
   reg. In function_arg, since *cum > last arg reg we would return 0
   and thus the arg will end up on the stack. For straddling args of
   course function_arg_partial_nregs will come into play.  */

static void
arc_function_arg_advance (cumulative_args_t cum_v,
			  machine_mode mode,
			  const_tree type,
			  bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = (mode == BLKmode
	       ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode));
  int words = (bytes + UNITS_PER_WORD  - 1) / UNITS_PER_WORD;
  int i;

  if (words)
    *cum = ROUND_ADVANCE_CUM (*cum, mode, type);
  for (i = 0; i < words; i++)
    *cum = ARC_NEXT_ARG_REG (*cum);

}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FN_DECL_OR_TYPE is its
   FUNCTION_DECL; otherwise, FN_DECL_OR_TYPE is its type.  */

static rtx
arc_function_value (const_tree valtype,
		    const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode = TYPE_MODE (valtype);
  int unsignedp ATTRIBUTE_UNUSED;

  unsignedp = TYPE_UNSIGNED (valtype);
  if (INTEGRAL_TYPE_P (valtype) || TREE_CODE (valtype) == OFFSET_TYPE)
    PROMOTE_MODE (mode, unsignedp, valtype);
  return gen_rtx_REG (mode, 0);
}

/* Returns the return address that is used by builtin_return_address.  */

rtx
arc_return_addr_rtx (int count, ATTRIBUTE_UNUSED rtx frame)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode , RETURN_ADDR_REGNUM);
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

bool
arc_legitimate_constant_p (machine_mode mode, rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x))
    return false;

  if (!flag_pic && mode != Pmode)
    return true;

  switch (GET_CODE (x))
    {
    case CONST:
      if (flag_pic)
	{
	  if (arc_legitimate_pic_addr_p (x))
	    return true;
	}
      return arc_legitimate_constant_p (mode, XEXP (x, 0));

    case SYMBOL_REF:
      if (SYMBOL_REF_TLS_MODEL (x))
	return false;
      /* Fall through.  */
    case LABEL_REF:
      if (flag_pic)
	return false;
      /* Fall through.  */
    case CONST_INT:
    case CONST_DOUBLE:
      return true;

    case NEG:
      return arc_legitimate_constant_p (mode, XEXP (x, 0));

    case PLUS:
    case MINUS:
      {
	bool t1 = arc_legitimate_constant_p (mode, XEXP (x, 0));
	bool t2 = arc_legitimate_constant_p (mode, XEXP (x, 1));

	return (t1 && t2);
      }

    case CONST_VECTOR:
      switch (mode)
	{
	case E_V2HImode:
	  return TARGET_PLUS_DMPY;
	case E_V2SImode:
	case E_V4HImode:
	  return TARGET_PLUS_QMACW;
	default:
	  return false;
	}

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_TLS_GD:
	case UNSPEC_TLS_OFF:
	case UNSPEC_TLS_IE:
	  return true;
	default:
	  /* Any other unspec ending here are pic related, hence the above
	     constant pic address checking returned false.  */
	  return false;
	}
      /* Fall through.  */

    default:
      fatal_insn ("unrecognized supposed constant", x);
    }

  gcc_unreachable ();
}

static bool
arc_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  if (RTX_OK_FOR_BASE_P (x, strict))
     return true;
  if (legitimate_offset_address_p (mode, x, TARGET_INDEXED_LOADS, strict))
     return true;
  if (legitimate_scaled_address_p (mode, x, strict))
    return true;
  if (LEGITIMATE_SMALL_DATA_ADDRESS_P (x))
     return true;
  if (GET_CODE (x) == CONST_INT && LARGE_INT (INTVAL (x)))
     return true;

  /* When we compile for size avoid const (@sym + offset)
     addresses.  */
  if (!flag_pic && optimize_size && !reload_completed
      && (GET_CODE (x) == CONST)
      && (GET_CODE (XEXP (x, 0)) == PLUS)
      && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
      && SYMBOL_REF_TLS_MODEL (XEXP (XEXP (x, 0), 0)) == 0
      && !SYMBOL_REF_FUNCTION_P (XEXP (XEXP (x, 0), 0)))
    {
      rtx addend = XEXP (XEXP (x, 0), 1);
      gcc_assert (CONST_INT_P (addend));
      HOST_WIDE_INT offset = INTVAL (addend);

      /* Allow addresses having a large offset to pass.  Anyhow they
	 will end in a limm.  */
      return !(offset > -1024 && offset < 1020);
    }

  if ((GET_MODE_SIZE (mode) != 16) && CONSTANT_P (x))
    {
      return arc_legitimate_constant_p (mode, x);
    }
  if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == PRE_INC
       || GET_CODE (x) == POST_DEC || GET_CODE (x) == POST_INC)
      && RTX_OK_FOR_BASE_P (XEXP (x, 0), strict))
    return true;
      /* We're restricted here by the `st' insn.  */
  if ((GET_CODE (x) == PRE_MODIFY || GET_CODE (x) == POST_MODIFY)
      && GET_CODE (XEXP ((x), 1)) == PLUS
      && rtx_equal_p (XEXP ((x), 0), XEXP (XEXP (x, 1), 0))
      && legitimate_offset_address_p (QImode, XEXP (x, 1),
				      TARGET_AUTO_MODIFY_REG, strict))
    return true;
  return false;
}

/* Return true iff ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

static bool
arc_mode_dependent_address_p (const_rtx addr, addr_space_t)
{
  /* SYMBOL_REF is not mode dependent: it is either a small data reference,
     which is valid for loads and stores, or a limm offset, which is valid for
     loads.  Scaled indices are scaled by the access mode.  */
  if (GET_CODE (addr) == PLUS
      && GET_CODE (XEXP ((addr), 0)) == MULT)
    return true;
  return false;
}

/* Determine if it's legal to put X into the constant pool.  */

static bool
arc_cannot_force_const_mem (machine_mode mode, rtx x)
{
  return !arc_legitimate_constant_p (mode, x);
}

/* IDs for all the ARC builtins.  */

enum arc_builtin_id
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)	\
    ARC_BUILTIN_ ## NAME,
#include "builtins.def"
#undef DEF_BUILTIN

    ARC_BUILTIN_COUNT
  };

struct GTY(()) arc_builtin_description
{
  enum insn_code icode;
  int n_args;
  tree fndecl;
};

static GTY(()) struct arc_builtin_description
arc_bdesc[ARC_BUILTIN_COUNT] =
{
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)		\
  { (enum insn_code) CODE_FOR_ ## ICODE, N_ARGS, NULL_TREE },
#include "builtins.def"
#undef DEF_BUILTIN
};

/* Transform UP into lowercase and write the result to LO.
   You must provide enough space for LO.  Return LO.  */

static char*
arc_tolower (char *lo, const char *up)
{
  char *lo0 = lo;

  for (; *up; up++, lo++)
    *lo = TOLOWER (*up);

  *lo = '\0';

  return lo0;
}

/* Implement `TARGET_BUILTIN_DECL'.  */

static tree
arc_builtin_decl (unsigned id, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (id < ARC_BUILTIN_COUNT)
    return arc_bdesc[id].fndecl;

  return error_mark_node;
}

static void
arc_init_builtins (void)
{
  tree V4HI_type_node;
  tree V2SI_type_node;
  tree V2HI_type_node;

  /* Vector types based on HS SIMD elements.  */
  V4HI_type_node = build_vector_type_for_mode (intHI_type_node, V4HImode);
  V2SI_type_node = build_vector_type_for_mode (intSI_type_node, V2SImode);
  V2HI_type_node = build_vector_type_for_mode (intHI_type_node, V2HImode);

  tree pcvoid_type_node
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_CONST));
  tree V8HI_type_node = build_vector_type_for_mode (intHI_type_node,
						    V8HImode);

  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree int_ftype_int
    = build_function_type_list (integer_type_node, integer_type_node,
				NULL_TREE);
  tree int_ftype_pcvoid_int
    = build_function_type_list (integer_type_node, pcvoid_type_node,
				integer_type_node, NULL_TREE);
  tree void_ftype_usint_usint
    = build_function_type_list (void_type_node, long_unsigned_type_node,
				long_unsigned_type_node, NULL_TREE);
  tree int_ftype_int_int
    = build_function_type_list (integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree usint_ftype_usint
    = build_function_type_list  (long_unsigned_type_node,
				 long_unsigned_type_node, NULL_TREE);
  tree void_ftype_usint
    = build_function_type_list (void_type_node, long_unsigned_type_node,
				NULL_TREE);
  tree int_ftype_void
    = build_function_type_list (integer_type_node, void_type_node,
				NULL_TREE);
  tree void_ftype_int
    = build_function_type_list (void_type_node, integer_type_node,
				NULL_TREE);
  tree int_ftype_short
    = build_function_type_list (integer_type_node, short_integer_type_node,
				NULL_TREE);

  /* Old ARC SIMD types.  */
  tree v8hi_ftype_v8hi_v8hi
    = build_function_type_list (V8HI_type_node, V8HI_type_node,
				V8HI_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_int
    = build_function_type_list (V8HI_type_node, V8HI_type_node,
				integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_int_int
    = build_function_type_list (V8HI_type_node, V8HI_type_node,
				integer_type_node, integer_type_node,
				NULL_TREE);
  tree void_ftype_v8hi_int_int
    = build_function_type_list (void_type_node, V8HI_type_node,
				integer_type_node, integer_type_node,
				NULL_TREE);
  tree void_ftype_v8hi_int_int_int
    = build_function_type_list (void_type_node, V8HI_type_node,
				integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree v8hi_ftype_int_int
    = build_function_type_list (V8HI_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree void_ftype_int_int
    = build_function_type_list (void_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi
    = build_function_type_list (V8HI_type_node, V8HI_type_node,
				NULL_TREE);
  /* ARCv2 SIMD types.  */
  tree long_ftype_v4hi_v4hi
    = build_function_type_list (long_long_integer_type_node,
				V4HI_type_node,	V4HI_type_node, NULL_TREE);
  tree int_ftype_v2hi_v2hi
    = build_function_type_list (integer_type_node,
				V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree v2si_ftype_v2hi_v2hi
    = build_function_type_list (V2SI_type_node,
				V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi_v2hi
    = build_function_type_list (V2HI_type_node,
				V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree v2si_ftype_v2si_v2si
    = build_function_type_list (V2SI_type_node,
				V2SI_type_node, V2SI_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi_v4hi
    = build_function_type_list (V4HI_type_node,
				V4HI_type_node, V4HI_type_node, NULL_TREE);
  tree long_ftype_v2si_v2hi
    = build_function_type_list (long_long_integer_type_node,
				V2SI_type_node, V2HI_type_node, NULL_TREE);

  /* Add the builtins.  */
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)			\
  {									\
    int id = ARC_BUILTIN_ ## NAME;					\
    const char *Name = "__builtin_arc_" #NAME;				\
    char *name = (char*) alloca (1 + strlen (Name));			\
									\
    gcc_assert (id < ARC_BUILTIN_COUNT);				\
    if (MASK)								\
      arc_bdesc[id].fndecl						\
	= add_builtin_function (arc_tolower(name, Name), TYPE, id,	\
				BUILT_IN_MD, NULL, NULL_TREE);		\
  }
#include "builtins.def"
#undef DEF_BUILTIN
}

/* Helper to expand __builtin_arc_aligned (void* val, int
  alignval).  */

static rtx
arc_expand_builtin_aligned (tree exp)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  fold (arg1);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  if (!CONST_INT_P (op1))
    {
      /* If we can't fold the alignment to a constant integer
	 whilst optimizing, this is probably a user error.  */
      if (optimize)
	warning (0, "__builtin_arc_aligned with non-constant alignment");
    }
  else
    {
      HOST_WIDE_INT alignTest = INTVAL (op1);
      /* Check alignTest is positive, and a power of two.  */
      if (alignTest <= 0 || alignTest != (alignTest & -alignTest))
	{
	  error ("invalid alignment value for __builtin_arc_aligned");
	  return NULL_RTX;
	}

      if (CONST_INT_P (op0))
	{
	  HOST_WIDE_INT pnt = INTVAL (op0);

	  if ((pnt & (alignTest - 1)) == 0)
	    return const1_rtx;
	}
      else
	{
	  unsigned  align = get_pointer_alignment (arg0);
	  unsigned  numBits = alignTest * BITS_PER_UNIT;

	  if (align && align >= numBits)
	    return const1_rtx;
	  /* Another attempt to ascertain alignment.  Check the type
	     we are pointing to.  */
	  if (POINTER_TYPE_P (TREE_TYPE (arg0))
	      && TYPE_ALIGN (TREE_TYPE (TREE_TYPE (arg0))) >= numBits)
	    return const1_rtx;
	}
    }

  /* Default to false.  */
  return const0_rtx;
}

/* Helper arc_expand_builtin, generates a pattern for the given icode
   and arguments.  */

static rtx_insn *
apply_GEN_FCN (enum insn_code icode, rtx *arg)
{
  switch (insn_data[icode].n_generator_args)
    {
    case 0:
      return GEN_FCN (icode) ();
    case 1:
      return GEN_FCN (icode) (arg[0]);
    case 2:
      return GEN_FCN (icode) (arg[0], arg[1]);
    case 3:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2]);
    case 4:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2], arg[3]);
    case 5:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2], arg[3], arg[4]);
    default:
      gcc_unreachable ();
    }
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
arc_expand_builtin (tree exp,
		    rtx target,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int id = DECL_FUNCTION_CODE (fndecl);
  const struct arc_builtin_description *d = &arc_bdesc[id];
  int i, j, n_args = call_expr_nargs (exp);
  rtx pat = NULL_RTX;
  rtx xop[5];
  enum insn_code icode = d->icode;
  machine_mode tmode = insn_data[icode].operand[0].mode;
  int nonvoid;
  tree arg0;
  tree arg1;
  tree arg2;
  tree arg3;
  rtx op0;
  rtx op1;
  rtx op2;
  rtx op3;
  rtx op4;
  machine_mode mode0;
  machine_mode mode1;
  machine_mode mode2;
  machine_mode mode3;
  machine_mode mode4;

  if (id >= ARC_BUILTIN_COUNT)
    internal_error ("bad builtin fcode");

  /* 1st part: Expand special builtins.  */
  switch (id)
    {
    case ARC_BUILTIN_NOP:
      emit_insn (gen_nopv ());
      return NULL_RTX;

    case ARC_BUILTIN_RTIE:
    case ARC_BUILTIN_SYNC:
    case ARC_BUILTIN_BRK:
    case ARC_BUILTIN_SWI:
    case ARC_BUILTIN_UNIMP_S:
      gcc_assert (icode != 0);
      emit_insn (GEN_FCN (icode) (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_ALIGNED:
      return arc_expand_builtin_aligned (exp);

    case ARC_BUILTIN_CLRI:
      target = gen_reg_rtx (SImode);
      emit_insn (gen_clri (target, const1_rtx));
      return target;

    case ARC_BUILTIN_TRAP_S:
    case ARC_BUILTIN_SLEEP:
      arg0 = CALL_EXPR_ARG (exp, 0);
      fold (arg0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      if  (!CONST_INT_P (op0) || !satisfies_constraint_L (op0))
	{
	  error ("builtin operand should be an unsigned 6-bit value");
	  return NULL_RTX;
	}
      gcc_assert (icode != 0);
      emit_insn (GEN_FCN (icode) (op0));
      return NULL_RTX;

    case ARC_BUILTIN_VDORUN:
    case ARC_BUILTIN_VDIRUN:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      target = gen_rtx_REG (SImode, (id == ARC_BUILTIN_VDIRUN) ? 131 : 139);

      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (!insn_data[icode].operand[1].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (!insn_data[icode].operand[2].predicate (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (target, op0, op1);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return NULL_RTX;

    case ARC_BUILTIN_VDIWR:
    case ARC_BUILTIN_VDOWR:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      if (!CONST_INT_P (op0)
	  || !(UNSIGNED_INT3 (INTVAL (op0))))
	error ("operand 1 should be an unsigned 3-bit immediate");

      mode1 =  insn_data[icode].operand[1].mode;

      if (icode == CODE_FOR_vdiwr_insn)
	target = gen_rtx_REG (SImode,
			      ARC_FIRST_SIMD_DMA_CONFIG_IN_REG + INTVAL (op0));
      else if (icode == CODE_FOR_vdowr_insn)
	target = gen_rtx_REG (SImode,
			      ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG + INTVAL (op0));
      else
	gcc_unreachable ();

      if (!insn_data[icode].operand[2].predicate (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (target, op1);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return NULL_RTX;

    case ARC_BUILTIN_VASRW:
    case ARC_BUILTIN_VSR8:
    case ARC_BUILTIN_VSR8AW:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);
      op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (!insn_data[icode].operand[1].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if ((!insn_data[icode].operand[2].predicate (op1, mode1))
	  || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 2 should be an unsigned 3-bit value (I0-I7)");

      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return target;

    case ARC_BUILTIN_VLD32WH:
    case ARC_BUILTIN_VLD32WL:
    case ARC_BUILTIN_VLD64:
    case ARC_BUILTIN_VLD32:
      rtx src_vreg;
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0); /* source vreg.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7.  */
      arg2 = CALL_EXPR_ARG (exp, 2); /* u8.  */

      src_vreg = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op0 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL);
      op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);

      /* target <- src vreg.  */
      emit_insn (gen_move_insn (target, src_vreg));

      /* target <- vec_concat: target, mem (Ib, u8).  */
      mode0 =  insn_data[icode].operand[3].mode;
      mode1 =  insn_data[icode].operand[1].mode;

      if ((!insn_data[icode].operand[3].predicate (op0, mode0))
	  || !(UNSIGNED_INT3 (INTVAL (op0))))
	error ("operand 1 should be an unsigned 3-bit value (I0-I7)");

      if ((!insn_data[icode].operand[1].predicate (op1, mode1))
	  || !(UNSIGNED_INT8 (INTVAL (op1))))
	error ("operand 2 should be an unsigned 8-bit value");

      pat = GEN_FCN (icode) (target, op1, op2, op0);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return target;

    case ARC_BUILTIN_VLD64W:
    case ARC_BUILTIN_VLD128:
      arg0 = CALL_EXPR_ARG (exp, 0); /* dest vreg.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7.  */

      op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);
      op1 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op2 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      /* target <- src vreg.  */
      target = gen_reg_rtx (V8HImode);

      /* target <- vec_concat: target, mem (Ib, u8).  */
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;
      mode2 =  insn_data[icode].operand[3].mode;

      if ((!insn_data[icode].operand[2].predicate (op1, mode1))
	  || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 1 should be an unsigned 3-bit value (I0-I7)");

      if ((!insn_data[icode].operand[3].predicate (op2, mode2))
	  || !(UNSIGNED_INT8 (INTVAL (op2))))
	error ("operand 2 should be an unsigned 8-bit value");

      pat = GEN_FCN (icode) (target, op0, op1, op2);

      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return target;

    case ARC_BUILTIN_VST128:
    case ARC_BUILTIN_VST64:
      arg0 = CALL_EXPR_ARG (exp, 0); /* src vreg.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7.  */
      arg2 = CALL_EXPR_ARG (exp, 2); /* u8.  */

      op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);
      op2 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL);
      op3 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);

      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;

      if ((!insn_data[icode].operand[1].predicate (op1, mode1))
	  || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 2 should be an unsigned 3-bit value (I0-I7)");

      if ((!insn_data[icode].operand[2].predicate (op2, mode2))
	  || !(UNSIGNED_INT8 (INTVAL (op2))))
	error ("operand 3 should be an unsigned 8-bit value");

      if (!insn_data[icode].operand[3].predicate (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      pat = GEN_FCN (icode) (op0, op1, op2, op3);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return NULL_RTX;

    case ARC_BUILTIN_VST16_N:
    case ARC_BUILTIN_VST32_N:
      arg0 = CALL_EXPR_ARG (exp, 0); /* source vreg.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* u3.  */
      arg2 = CALL_EXPR_ARG (exp, 2); /* [I]0-7.  */
      arg3 = CALL_EXPR_ARG (exp, 3); /* u8.  */

      op0 = expand_expr (arg3, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);
      op2 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL);
      op3 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op4 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      mode0 = insn_data[icode].operand[0].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;
      mode4 = insn_data[icode].operand[4].mode;

      /* Do some correctness checks for the operands.  */
      if ((!insn_data[icode].operand[0].predicate (op0, mode0))
	  || !(UNSIGNED_INT8 (INTVAL (op0))))
	error ("operand 4 should be an unsigned 8-bit value (0-255)");

      if ((!insn_data[icode].operand[2].predicate (op2, mode2))
	  || !(UNSIGNED_INT3 (INTVAL (op2))))
	error ("operand 3 should be an unsigned 3-bit value (I0-I7)");

      if (!insn_data[icode].operand[3].predicate (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      if ((!insn_data[icode].operand[4].predicate (op4, mode4))
	   || !(UNSIGNED_INT3 (INTVAL (op4))))
	error ("operand 2 should be an unsigned 3-bit value (subreg 0-7)");
      else if (icode == CODE_FOR_vst32_n_insn
	       && ((INTVAL (op4) % 2) != 0))
	error ("operand 2 should be an even 3-bit value (subreg 0,2,4,6)");

      pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
      if (!pat)
	return NULL_RTX;

      emit_insn (pat);
      return NULL_RTX;

    default:
      break;
    }

  /* 2nd part: Expand regular builtins.  */
  if (icode == 0)
    internal_error ("bad builtin fcode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;
  j = 0;

  if (nonvoid)
    {
      if (target == NULL_RTX
	  || GET_MODE (target) != tmode
	  || !insn_data[icode].operand[0].predicate (target, tmode))
	{
	  target = gen_reg_rtx (tmode);
	}
      xop[j++] = target;
    }

  gcc_assert (n_args <= 4);
  for (i = 0; i < n_args; i++, j++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      machine_mode mode = insn_data[icode].operand[j].mode;
      rtx op = expand_expr (arg, NULL_RTX, mode, EXPAND_NORMAL);
      machine_mode opmode = GET_MODE (op);
      char c = insn_data[icode].operand[j].constraint[0];

      /* SIMD extension requires exact immediate operand match.  */
      if ((id > ARC_BUILTIN_SIMD_BEGIN)
	  && (id < ARC_BUILTIN_SIMD_END)
	  && (c != 'v')
	  && (c != 'r'))
	{
	  if (!CONST_INT_P (op))
	    error ("builtin requires an immediate for operand %d", j);
	  switch (c)
	    {
	    case 'L':
	      if (!satisfies_constraint_L (op))
		error ("operand %d should be a 6 bit unsigned immediate", j);
	      break;
	    case 'P':
	      if (!satisfies_constraint_P (op))
		error ("operand %d should be a 8 bit unsigned immediate", j);
	      break;
	    case 'K':
	      if (!satisfies_constraint_K (op))
		error ("operand %d should be a 3 bit unsigned immediate", j);
	      break;
	    default:
	      error ("unknown builtin immediate operand type for operand %d",
		     j);
	    }
	}

      if (CONST_INT_P (op))
	opmode = mode;

      if ((opmode == SImode) && (mode == HImode))
	{
	  opmode = HImode;
	  op = gen_lowpart (HImode, op);
	}

      /* In case the insn wants input operands in modes different from
	 the result, abort.  */
      gcc_assert (opmode == mode || opmode == VOIDmode);

      if (!insn_data[icode].operand[i + nonvoid].predicate (op, mode))
	op = copy_to_mode_reg (mode, op);

      xop[j] = op;
    }

  pat = apply_GEN_FCN (icode, xop);
  if (pat == NULL_RTX)
    return NULL_RTX;

  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}

/* Returns true if the operands[opno] is a valid compile-time constant to be
   used as register number in the code for builtins.  Else it flags an error
   and returns false.  */

bool
check_if_valid_regno_const (rtx *operands, int opno)
{

  switch (GET_CODE (operands[opno]))
    {
    case SYMBOL_REF :
    case CONST :
    case CONST_INT :
      return true;
    default:
	error ("register number must be a compile-time constant. Try giving higher optimization levels");
	break;
    }
  return false;
}

/* Check that after all the constant folding, whether the operand to
   __builtin_arc_sleep is an unsigned int of 6 bits.  If not, flag an error.  */

bool
check_if_valid_sleep_operand (rtx *operands, int opno)
{
  switch (GET_CODE (operands[opno]))
    {
    case CONST :
    case CONST_INT :
	if( UNSIGNED_INT6 (INTVAL (operands[opno])))
	    return true;
    /* FALLTHRU */
    default:
	fatal_error (input_location,
		     "operand for sleep instruction must be an unsigned 6 bit compile-time constant");
	break;
    }
  return false;
}

/* Return true if it is ok to make a tail-call to DECL.  */

static bool
arc_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
			     tree exp ATTRIBUTE_UNUSED)
{
  /* Never tailcall from an ISR routine - it needs a special exit sequence.  */
  if (ARC_INTERRUPT_P (arc_compute_function_type (cfun)))
    return false;

  /* Everything else is ok.  */
  return true;
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */

static void
arc_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
		     HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset,
		     tree function)
{
  int mi_delta = delta;
  const char *const mi_op = mi_delta < 0 ? "sub" : "add";
  int shift = 0;
  int this_regno
    = aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function) ? 1 : 0;
  rtx fnaddr;

  if (mi_delta < 0)
    mi_delta = - mi_delta;

  /* Add DELTA.  When possible use a plain add, otherwise load it into
     a register first.  */

  while (mi_delta != 0)
    {
      if ((mi_delta & (3 << shift)) == 0)
	shift += 2;
      else
	{
	  asm_fprintf (file, "\t%s\t%s, %s, %d\n",
		       mi_op, reg_names[this_regno], reg_names[this_regno],
		       mi_delta & (0xff << shift));
	  mi_delta &= ~(0xff << shift);
	  shift += 8;
	}
    }

  /* If needed, add *(*THIS + VCALL_OFFSET) to THIS.  */
  if (vcall_offset != 0)
    {
      /* ld  r12,[this]           --> temp = *this
	 add r12,r12,vcall_offset --> temp = *(*this + vcall_offset)
	 ld r12,[r12]
	 add this,this,r12        --> this+ = *(*this + vcall_offset) */
      asm_fprintf (file, "\tld\t%s, [%s]\n",
		   ARC_TEMP_SCRATCH_REG, reg_names[this_regno]);
      asm_fprintf (file, "\tadd\t%s, %s, " HOST_WIDE_INT_PRINT_DEC "\n",
		   ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG, vcall_offset);
      asm_fprintf (file, "\tld\t%s, [%s]\n",
		   ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG);
      asm_fprintf (file, "\tadd\t%s, %s, %s\n", reg_names[this_regno],
		   reg_names[this_regno], ARC_TEMP_SCRATCH_REG);
    }

  fnaddr = XEXP (DECL_RTL (function), 0);

  if (arc_is_longcall_p (fnaddr))
    {
      if (flag_pic)
	{
	  asm_fprintf (file, "\tld\t%s, [pcl, @",
		       ARC_TEMP_SCRATCH_REG);
	  assemble_name (file, XSTR (fnaddr, 0));
	  fputs ("@gotpc]\n", file);
	  asm_fprintf (file, "\tj\t[%s]", ARC_TEMP_SCRATCH_REG);
	}
      else
	{
	  fputs ("\tj\t@", file);
	  assemble_name (file, XSTR (fnaddr, 0));
	}
    }
  else
    {
      fputs ("\tb\t@", file);
      assemble_name (file, XSTR (fnaddr, 0));
      if (flag_pic)
	fputs ("@plt\n", file);
    }
  fputc ('\n', file);
}

/* Return true if a 32 bit "long_call" should be generated for
   this calling SYM_REF.  We generate a long_call if the function:

        a.  has an __attribute__((long call))
     or b.  the -mlong-calls command line switch has been specified

   However we do not generate a long call if the function has an
   __attribute__ ((short_call)) or __attribute__ ((medium_call))

   This function will be called by C fragments contained in the machine
   description file.  */

bool
arc_is_longcall_p (rtx sym_ref)
{
  if (GET_CODE (sym_ref) != SYMBOL_REF)
    return false;

  return (SYMBOL_REF_LONG_CALL_P (sym_ref)
	  || (TARGET_LONG_CALLS_SET
	      && !SYMBOL_REF_SHORT_CALL_P (sym_ref)
	      && !SYMBOL_REF_MEDIUM_CALL_P (sym_ref)));

}

/* Likewise for short calls.  */

bool
arc_is_shortcall_p (rtx sym_ref)
{
  if (GET_CODE (sym_ref) != SYMBOL_REF)
    return false;

  return (SYMBOL_REF_SHORT_CALL_P (sym_ref)
	  || (!TARGET_LONG_CALLS_SET && !TARGET_MEDIUM_CALLS
	      && !SYMBOL_REF_LONG_CALL_P (sym_ref)
	      && !SYMBOL_REF_MEDIUM_CALL_P (sym_ref)));

}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
arc_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  if (AGGREGATE_TYPE_P (type) || TREE_ADDRESSABLE (type))
    return true;
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      return (size == -1 || size > (TARGET_V2 ? 16 : 8));
    }
}


/* This was in rtlanal.c, and can go in there when we decide we want
   to submit the change for inclusion in the GCC tree.  */
/* Like note_stores, but allow the callback to have side effects on the rtl
   (like the note_stores of yore):
   Call FUN on each register or MEM that is stored into or clobbered by X.
   (X would be the pattern of an insn).  DATA is an arbitrary pointer,
   ignored by note_stores, but passed to FUN.
   FUN may alter parts of the RTL.

   FUN receives three arguments:
   1. the REG, MEM, CC0 or PC being stored in or clobbered,
   2. the SET or CLOBBER rtx that does the store,
   3. the pointer DATA provided to note_stores.

  If the item being stored in or clobbered is a SUBREG of a hard register,
  the SUBREG will be passed.  */

/* For now.  */ static
void
walk_stores (rtx x, void (*fun) (rtx, rtx, void *), void *data)
{
  int i;

  if (GET_CODE (x) == COND_EXEC)
    x = COND_EXEC_CODE (x);

  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      rtx dest = SET_DEST (x);

      while ((GET_CODE (dest) == SUBREG
	      && (!REG_P (SUBREG_REG (dest))
		  || REGNO (SUBREG_REG (dest)) >= FIRST_PSEUDO_REGISTER))
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);

      /* If we have a PARALLEL, SET_DEST is a list of EXPR_LIST expressions,
	 each of whose first operand is a register.  */
      if (GET_CODE (dest) == PARALLEL)
	{
	  for (i = XVECLEN (dest, 0) - 1; i >= 0; i--)
	    if (XEXP (XVECEXP (dest, 0, i), 0) != 0)
	      (*fun) (XEXP (XVECEXP (dest, 0, i), 0), x, data);
	}
      else
	(*fun) (dest, x, data);
    }

  else if (GET_CODE (x) == PARALLEL)
    for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
      walk_stores (XVECEXP (x, 0, i), fun, data);
}

static bool
arc_pass_by_reference (cumulative_args_t ca_v ATTRIBUTE_UNUSED,
		       machine_mode mode ATTRIBUTE_UNUSED,
		       const_tree type,
		       bool named ATTRIBUTE_UNUSED)
{
  return (type != 0
	  && (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      || TREE_ADDRESSABLE (type)));
}

/* Implement TARGET_CAN_USE_DOLOOP_P.  */

static bool
arc_can_use_doloop_p (const widest_int &,
		      const widest_int &iterations_max,
		      unsigned int loop_depth, bool entered_at_top)
{
  /* Considering limitations in the hardware, only use doloop
     for innermost loops which must be entered from the top.  */
  if (loop_depth > 1 || !entered_at_top)
    return false;

  /* Check for lp_count width boundary.  */
  if (arc_lpcwidth != 32
      && (wi::gtu_p (iterations_max, ((1 << arc_lpcwidth) - 1))
	  || wi::eq_p (iterations_max, 0)))
    return false;
  return true;
}

/* NULL if INSN insn is valid within a low-overhead loop.  Otherwise
   return why doloop cannot be applied.  */

static const char *
arc_invalid_within_doloop (const rtx_insn *insn)
{
  if (CALL_P (insn))
    return "Function call in the loop.";

  /* FIXME! add here all the ZOL exceptions.  */
  return NULL;
}

/* Return true if a load instruction (CONSUMER) uses the same address as a
   store instruction (PRODUCER).  This function is used to avoid st/ld
   address hazard in ARC700 cores.  */
bool
arc_store_addr_hazard_p (rtx_insn* producer, rtx_insn* consumer)
{
  rtx in_set, out_set;
  rtx out_addr, in_addr;

  if (!producer)
    return false;

  if (!consumer)
    return false;

  /* Peel the producer and the consumer for the address.  */
  out_set = single_set (producer);
  if (out_set)
    {
      out_addr = SET_DEST (out_set);
      if (!out_addr)
	return false;
      if (GET_CODE (out_addr) == ZERO_EXTEND
	  || GET_CODE (out_addr) == SIGN_EXTEND)
	out_addr = XEXP (out_addr, 0);

      if (!MEM_P (out_addr))
	return false;

      in_set = single_set (consumer);
      if (in_set)
	{
	  in_addr = SET_SRC (in_set);
	  if (!in_addr)
	    return false;
	  if (GET_CODE (in_addr) == ZERO_EXTEND
	      || GET_CODE (in_addr) == SIGN_EXTEND)
	    in_addr = XEXP (in_addr, 0);

	  if (!MEM_P (in_addr))
	    return false;
	  /* Get rid of the MEM and check if the addresses are
	     equivalent.  */
	  in_addr = XEXP (in_addr, 0);
	  out_addr = XEXP (out_addr, 0);

	  return exp_equiv_p (in_addr, out_addr, 0, true);
	}
    }
  return false;
}

/* The same functionality as arc_hazard.  It is called in machine
   reorg before any other optimization.  Hence, the NOP size is taken
   into account when doing branch shortening.  */

static void
workaround_arc_anomaly (void)
{
  rtx_insn *insn, *succ0;

  /* For any architecture: call arc_hazard here.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      succ0 = next_real_insn (insn);
      if (arc_hazard (insn, succ0))
	{
	  emit_insn_before (gen_nopv (), succ0);
	}
    }

  if (TARGET_ARC700)
    {
      rtx_insn *succ1;

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  succ0 = next_real_insn (insn);
	  if (arc_store_addr_hazard_p (insn, succ0))
	    {
	      emit_insn_after (gen_nopv (), insn);
	      emit_insn_after (gen_nopv (), insn);
	      continue;
	    }

	  /* Avoid adding nops if the instruction between the ST and LD is
	     a call or jump.  */
	  succ1 = next_real_insn (succ0);
	  if (succ0 && !JUMP_P (succ0) && !CALL_P (succ0)
	      && arc_store_addr_hazard_p (insn, succ1))
	    emit_insn_after (gen_nopv (), insn);
	}
    }
}

/* A callback for the hw-doloop pass.  Called when a loop we have discovered
   turns out not to be optimizable; we have to split the loop_end pattern into
   a subtract and a test.  */

static void
hwloop_fail (hwloop_info loop)
{
  rtx test;
  rtx insn = loop->loop_end;

  if (TARGET_V2
      && (loop->length && (loop->length <= ARC_MAX_LOOP_LENGTH))
      && REG_P (loop->iter_reg))
    {
      /* TARGET_V2 has dbnz instructions.  */
      test = gen_dbnz (loop->iter_reg, loop->start_label);
      insn = emit_jump_insn_before (test, loop->loop_end);
    }
  else if (REG_P (loop->iter_reg) && (REGNO (loop->iter_reg) == LP_COUNT))
    {
      /* We have the lp_count as loop iterator, try to use it.  */
      emit_insn_before (gen_loop_fail (), loop->loop_end);
      test = gen_rtx_NE (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REG),
			 const0_rtx);
      test = gen_rtx_IF_THEN_ELSE (VOIDmode, test,
				   gen_rtx_LABEL_REF (Pmode, loop->start_label),
				   pc_rtx);
      insn = emit_jump_insn_before (gen_rtx_SET (pc_rtx, test),
				     loop->loop_end);
    }
  else
    {
      emit_insn_before (gen_addsi3 (loop->iter_reg,
				    loop->iter_reg,
				    constm1_rtx),
			loop->loop_end);
      test = gen_rtx_NE (VOIDmode, loop->iter_reg, const0_rtx);
      insn = emit_jump_insn_before (gen_cbranchsi4 (test,
						    loop->iter_reg,
						    const0_rtx,
						    loop->start_label),
				    loop->loop_end);
    }
  JUMP_LABEL (insn) = loop->start_label;
  LABEL_NUSES (loop->start_label)++;
  delete_insn (loop->loop_end);
}

/* Optimize LOOP.  */

static bool
hwloop_optimize (hwloop_info loop)
{
  int i;
  edge entry_edge;
  basic_block entry_bb, bb;
  rtx iter_reg, end_label;
  rtx_insn *insn, *seq, *entry_after, *last_insn;
  unsigned int length;
  bool need_fix = false;
  rtx lp_reg = gen_rtx_REG (SImode, LP_COUNT);

  if (loop->depth > 1)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d is not innermost\n",
                 loop->loop_no);
      return false;
    }

  if (!loop->incoming_dest)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d has more than one entry\n",
                 loop->loop_no);
      return false;
    }

  if (loop->incoming_dest != loop->head)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d is not entered from head\n",
                 loop->loop_no);
      return false;
    }

  if (loop->has_call || loop->has_asm)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d has invalid insn\n",
                 loop->loop_no);
      return false;
    }

  /* Scan all the blocks to make sure they don't use iter_reg.  */
  if (loop->iter_reg_used || loop->iter_reg_used_outside)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d uses iterator\n",
                 loop->loop_no);
      return false;
    }

  /* Check if start_label appears before doloop_end.  */
  length = 0;
  for (insn = loop->start_label;
       insn && insn != loop->loop_end;
       insn = NEXT_INSN (insn))
    length += NONDEBUG_INSN_P (insn) ? get_attr_length (insn) : 0;

  if (!insn)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d start_label not before loop_end\n",
                 loop->loop_no);
      return false;
    }

  loop->length = length;
  if (loop->length > ARC_MAX_LOOP_LENGTH)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d too long\n", loop->loop_no);
      return false;
    }

  /* Check if we use a register or not.  */
  if (!REG_P (loop->iter_reg))
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d iterator is MEM\n",
                 loop->loop_no);
      return false;
    }

  /* Check if loop register is lpcount.  */
  if (REG_P (loop->iter_reg) && (REGNO (loop->iter_reg)) != LP_COUNT)
    {
      if (dump_file)
        fprintf (dump_file, ";; loop %d doesn't use lp_count as loop"
		 " iterator\n",
                 loop->loop_no);
      /* This loop doesn't use the lp_count, check though if we can
	 fix it.  */
      if (TEST_HARD_REG_BIT (loop->regs_set_in_loop, LP_COUNT)
	  /* In very unique cases we may have LP_COUNT alive.  */
	  || (loop->incoming_src
	      && REGNO_REG_SET_P (df_get_live_out (loop->incoming_src),
				  LP_COUNT)))
	return false;
      else
	need_fix = true;
    }

  /* Check for control like instruction as the last instruction of a
     ZOL.  */
  bb = loop->tail;
  last_insn = PREV_INSN (loop->loop_end);

  while (1)
    {
      for (; last_insn != BB_HEAD (bb);
	   last_insn = PREV_INSN (last_insn))
	if (NONDEBUG_INSN_P (last_insn))
	  break;

      if (last_insn != BB_HEAD (bb))
	break;

      if (single_pred_p (bb)
	  && single_pred_edge (bb)->flags & EDGE_FALLTHRU
	  && single_pred (bb) != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  bb = single_pred (bb);
	  last_insn = BB_END (bb);
	  continue;
	}
      else
	{
	  last_insn = NULL;
	  break;
	}
    }

  if (!last_insn)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d has no last instruction\n",
		 loop->loop_no);
      return false;
    }

  if ((TARGET_ARC600_FAMILY || TARGET_HS)
      && INSN_P (last_insn)
      && (JUMP_P (last_insn) || CALL_P (last_insn)
	  || GET_CODE (PATTERN (last_insn)) == SEQUENCE
	  || get_attr_type (last_insn) == TYPE_BRCC
	  || get_attr_type (last_insn) == TYPE_BRCC_NO_DELAY_SLOT))
    {
      if (loop->length + 2 > ARC_MAX_LOOP_LENGTH)
	{
	  if (dump_file)
	    fprintf (dump_file, ";; loop %d too long\n", loop->loop_no);
	  return false;
	}
      if (dump_file)
	fprintf (dump_file, ";; loop %d has a control like last insn;"
		 "add a nop\n",
		 loop->loop_no);

      last_insn = emit_insn_after (gen_nopv (), last_insn);
    }

  if (LABEL_P (last_insn))
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d has a label as last insn;"
		 "add a nop\n",
		 loop->loop_no);
      last_insn = emit_insn_after (gen_nopv (), last_insn);
    }
  loop->last_insn = last_insn;

  /* Get the loop iteration register.  */
  iter_reg = loop->iter_reg;

  gcc_assert (REG_P (iter_reg));

  entry_edge = NULL;

  FOR_EACH_VEC_SAFE_ELT (loop->incoming, i, entry_edge)
    if (entry_edge->flags & EDGE_FALLTHRU)
      break;

  if (entry_edge == NULL)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d has no fallthru edge jumping"
		 "into the loop\n",
		 loop->loop_no);
      return false;
    }
  /* The loop is good.  */
  end_label = gen_label_rtx ();
  loop->end_label = end_label;

  /* Place the zero_cost_loop_start instruction before the loop.  */
  entry_bb = entry_edge->src;

  start_sequence ();

  if (need_fix)
    {
      /* The loop uses a R-register, but the lp_count is free, thus
	 use lp_count.  */
      emit_insn (gen_movsi (lp_reg, iter_reg));
      SET_HARD_REG_BIT (loop->regs_set_in_loop, LP_COUNT);
      iter_reg = lp_reg;
      if (dump_file)
	{
	  fprintf (dump_file, ";; fix loop %d to use lp_count\n",
		   loop->loop_no);
	}
    }

  insn = emit_insn (gen_arc_lp (iter_reg,
				loop->start_label,
				loop->end_label));

  seq = get_insns ();
  end_sequence ();

  entry_after = BB_END (entry_bb);
  if (!single_succ_p (entry_bb) || vec_safe_length (loop->incoming) > 1
      || !entry_after)
    {
      basic_block new_bb;
      edge e;
      edge_iterator ei;

      emit_insn_before (seq, BB_HEAD (loop->head));
      seq = emit_label_before (gen_label_rtx (), seq);
      new_bb = create_basic_block (seq, insn, entry_bb);
      FOR_EACH_EDGE (e, ei, loop->incoming)
        {
          if (!(e->flags & EDGE_FALLTHRU))
            redirect_edge_and_branch_force (e, new_bb);
          else
            redirect_edge_succ (e, new_bb);
        }

      make_edge (new_bb, loop->head, 0);
    }
  else
    {
#if 0
      while (DEBUG_INSN_P (entry_after)
             || (NOTE_P (entry_after)
                 && NOTE_KIND (entry_after) != NOTE_INSN_BASIC_BLOCK
		 /* Make sure we don't split a call and its corresponding
		    CALL_ARG_LOCATION note.  */
                 && NOTE_KIND (entry_after) != NOTE_INSN_CALL_ARG_LOCATION))
        entry_after = NEXT_INSN (entry_after);
#endif
      entry_after = next_nonnote_insn_bb (entry_after);

      gcc_assert (entry_after);
      emit_insn_before (seq, entry_after);
    }

  delete_insn (loop->loop_end);
  /* Insert the loop end label before the last instruction of the
     loop.  */
  emit_label_after (end_label, loop->last_insn);

  return true;
}

/* A callback for the hw-doloop pass.  This function examines INSN; if
   it is a loop_end pattern we recognize, return the reg rtx for the
   loop counter.  Otherwise, return NULL_RTX.  */

static rtx
hwloop_pattern_reg (rtx_insn *insn)
{
  rtx reg;

  if (!JUMP_P (insn) || recog_memoized (insn) != CODE_FOR_loop_end)
    return NULL_RTX;

  reg = SET_DEST (XVECEXP (PATTERN (insn), 0, 1));
  if (!REG_P (reg))
    return NULL_RTX;
  return reg;
}

static struct hw_doloop_hooks arc_doloop_hooks =
{
  hwloop_pattern_reg,
  hwloop_optimize,
  hwloop_fail
};

/* Run from machine_dependent_reorg, this pass looks for doloop_end insns
   and tries to rewrite the RTL of these loops so that proper Blackfin
   hardware loops are generated.  */

static void
arc_reorg_loops (void)
{
  reorg_loops (true, &arc_doloop_hooks);
}

static int arc_reorg_in_progress = 0;

/* ARC's machince specific reorg function.  */

static void
arc_reorg (void)
{
  rtx_insn *insn;
  rtx pattern;
  rtx pc_target;
  long offset;
  int changed;

  cfun->machine->arc_reorg_started = 1;
  arc_reorg_in_progress = 1;

  compute_bb_for_insn ();

  df_analyze ();

  /* Doloop optimization.  */
  arc_reorg_loops ();

  workaround_arc_anomaly ();

/* FIXME: should anticipate ccfsm action, generate special patterns for
   to-be-deleted branches that have no delay slot and have at least the
   length of the size increase forced on other insns that are conditionalized.
   This can also have an insn_list inside that enumerates insns which are
   not actually conditionalized because the destinations are dead in the
   not-execute case.
   Could also tag branches that we want to be unaligned if they get no delay
   slot, or even ones that we don't want to do delay slot sheduling for
   because we can unalign them.

   However, there are cases when conditional execution is only possible after
   delay slot scheduling:

   - If a delay slot is filled with a nocond/set insn from above, the previous
     basic block can become elegible for conditional execution.
   - If a delay slot is filled with a nocond insn from the fall-through path,
     the branch with that delay slot can become eligble for conditional
     execution (however, with the same sort of data flow analysis that dbr
     does, we could have figured out before that we don't need to
     conditionalize this insn.)
     - If a delay slot insn is filled with an insn from the target, the
       target label gets its uses decremented (even deleted if falling to zero),
   thus possibly creating more condexec opportunities there.
   Therefore, we should still be prepared to apply condexec optimization on
   non-prepared branches if the size increase of conditionalized insns is no
   more than the size saved from eliminating the branch.  An invocation option
   could also be used to reserve a bit of extra size for condbranches so that
   this'll work more often (could also test in arc_reorg if the block is
   'close enough' to be eligible for condexec to make this likely, and
   estimate required size increase).  */
  /* Generate BRcc insns, by combining cmp and Bcc insns wherever possible.  */
  if (TARGET_NO_BRCC_SET)
    return;

  do
    {
      init_insn_lengths();
      changed = 0;

      if (optimize > 1 && !TARGET_NO_COND_EXEC)
	{
	  arc_ifcvt ();
	  unsigned int flags = pass_data_arc_ifcvt.todo_flags_finish;
	  df_finish_pass ((flags & TODO_df_verify) != 0);

	  if (dump_file)
	    {
	      fprintf (dump_file, ";; After if conversion:\n\n");
	      print_rtl (dump_file, get_insns ());
	    }
	}

      /* Call shorten_branches to calculate the insn lengths.  */
      shorten_branches (get_insns());
      cfun->machine->ccfsm_current_insn = NULL_RTX;

      if (!INSN_ADDRESSES_SET_P())
	  fatal_error (input_location, "Insn addresses not set after shorten_branches");

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  rtx label;
	  enum attr_type insn_type;

	  /* If a non-jump insn (or a casesi jump table), continue.  */
	  if (GET_CODE (insn) != JUMP_INSN ||
	      GET_CODE (PATTERN (insn)) == ADDR_VEC
	      || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    continue;

	  /* If we already have a brcc, note if it is suitable for brcc_s.
	     Be a bit generous with the brcc_s range so that we can take
	     advantage of any code shortening from delay slot scheduling.  */
	  if (recog_memoized (insn) == CODE_FOR_cbranchsi4_scratch)
	    {
	      rtx pat = PATTERN (insn);
	      rtx op = XEXP (SET_SRC (XVECEXP (pat, 0, 0)), 0);
	      rtx *ccp = &XEXP (XVECEXP (pat, 0, 1), 0);

	      offset = branch_dest (insn) - INSN_ADDRESSES (INSN_UID (insn));
	      if ((offset >= -140 && offset < 140)
		  && rtx_equal_p (XEXP (op, 1), const0_rtx)
		  && compact_register_operand (XEXP (op, 0), VOIDmode)
		  && equality_comparison_operator (op, VOIDmode))
		PUT_MODE (*ccp, CC_Zmode);
	      else if (GET_MODE (*ccp) == CC_Zmode)
		PUT_MODE (*ccp, CC_ZNmode);
	      continue;
	    }
	  if ((insn_type =  get_attr_type (insn)) == TYPE_BRCC
	      || insn_type == TYPE_BRCC_NO_DELAY_SLOT)
	    continue;

	  /* OK. so we have a jump insn.  */
	  /* We need to check that it is a bcc.  */
	  /* Bcc => set (pc) (if_then_else ) */
	  pattern = PATTERN (insn);
	  if (GET_CODE (pattern) != SET
	      || GET_CODE (SET_SRC (pattern)) != IF_THEN_ELSE
	      || ANY_RETURN_P (XEXP (SET_SRC (pattern), 1)))
	    continue;

	  /* Now check if the jump is beyond the s9 range.  */
	  if (CROSSING_JUMP_P (insn))
	    continue;
	  offset = branch_dest (insn) - INSN_ADDRESSES (INSN_UID (insn));

	  if(offset > 253 || offset < -254)
	    continue;

	  pc_target = SET_SRC (pattern);

	  /* Avoid FPU instructions.  */
	  if ((GET_MODE (XEXP (XEXP (pc_target, 0), 0)) == CC_FPUmode)
	      || (GET_MODE (XEXP (XEXP (pc_target, 0), 0)) == CC_FPU_UNEQmode))
	    continue;

	  /* Now go back and search for the set cc insn.  */

	  label = XEXP (pc_target, 1);

	    {
	      rtx pat;
	      rtx_insn *scan, *link_insn = NULL;

	      for (scan = PREV_INSN (insn);
		   scan && GET_CODE (scan) != CODE_LABEL;
		   scan = PREV_INSN (scan))
		{
		  if (! INSN_P (scan))
		    continue;
		  pat = PATTERN (scan);
		  if (GET_CODE (pat) == SET
		      && cc_register (SET_DEST (pat), VOIDmode))
		    {
		      link_insn = scan;
		      break;
		    }
		}
	      if (!link_insn)
		continue;
	      else
		/* Check if this is a data dependency.  */
		{
		  rtx op, cc_clob_rtx, op0, op1, brcc_insn, note;
		  rtx cmp0, cmp1;

		  /* Ok this is the set cc. copy args here.  */
		  op = XEXP (pc_target, 0);

		  op0 = cmp0 = XEXP (SET_SRC (pat), 0);
		  op1 = cmp1 = XEXP (SET_SRC (pat), 1);
		  if (GET_CODE (op0) == ZERO_EXTRACT
		      && XEXP (op0, 1) == const1_rtx
		      && (GET_CODE (op) == EQ
			  || GET_CODE (op) == NE))
		    {
		      /* btst / b{eq,ne} -> bbit{0,1} */
		      op0 = XEXP (cmp0, 0);
		      op1 = XEXP (cmp0, 2);
		    }
		  else if (!register_operand (op0, VOIDmode)
			  || !general_operand (op1, VOIDmode))
		    continue;
		  /* Be careful not to break what cmpsfpx_raw is
		     trying to create for checking equality of
		     single-precision floats.  */
		  else if (TARGET_SPFP
			   && GET_MODE (op0) == SFmode
			   && GET_MODE (op1) == SFmode)
		    continue;

		  /* None of the two cmp operands should be set between the
		     cmp and the branch.  */
		  if (reg_set_between_p (op0, link_insn, insn))
		    continue;

		  if (reg_set_between_p (op1, link_insn, insn))
		    continue;

		  /* Since the MODE check does not work, check that this is
		     CC reg's last set location before insn, and also no
		     instruction between the cmp and branch uses the
		     condition codes.  */
		  if ((reg_set_between_p (SET_DEST (pat), link_insn, insn))
		      || (reg_used_between_p (SET_DEST (pat), link_insn, insn)))
		    continue;

		  /* CC reg should be dead after insn.  */
		  if (!find_regno_note (insn, REG_DEAD, CC_REG))
		    continue;

		  op = gen_rtx_fmt_ee (GET_CODE (op),
				       GET_MODE (op), cmp0, cmp1);
		  /* If we create a LIMM where there was none before,
		     we only benefit if we can avoid a scheduling bubble
		     for the ARC600.  Otherwise, we'd only forgo chances
		     at short insn generation, and risk out-of-range
		     branches.  */
		  if (!brcc_nolimm_operator (op, VOIDmode)
		      && !long_immediate_operand (op1, VOIDmode)
		      && (TARGET_ARC700
			  || next_active_insn (link_insn) != insn))
		    continue;

		  /* Emit bbit / brcc (or brcc_s if possible).
		     CC_Zmode indicates that brcc_s is possible.  */

		  if (op0 != cmp0)
		    cc_clob_rtx = gen_rtx_REG (CC_ZNmode, CC_REG);
		  else if ((offset >= -140 && offset < 140)
			   && rtx_equal_p (op1, const0_rtx)
			   && compact_register_operand (op0, VOIDmode)
			   && (GET_CODE (op) == EQ
			       || GET_CODE (op) == NE))
		    cc_clob_rtx = gen_rtx_REG (CC_Zmode, CC_REG);
		  else
		    cc_clob_rtx = gen_rtx_REG (CCmode, CC_REG);

		  brcc_insn
		    = gen_rtx_IF_THEN_ELSE (VOIDmode, op, label, pc_rtx);
		  brcc_insn = gen_rtx_SET (pc_rtx, brcc_insn);
		  cc_clob_rtx = gen_rtx_CLOBBER (VOIDmode, cc_clob_rtx);
		  brcc_insn
		    = gen_rtx_PARALLEL
			(VOIDmode, gen_rtvec (2, brcc_insn, cc_clob_rtx));
		  brcc_insn = emit_jump_insn_before (brcc_insn, insn);

		  JUMP_LABEL (brcc_insn) = JUMP_LABEL (insn);
		  note = find_reg_note (insn, REG_BR_PROB, 0);
		  if (note)
		    {
		      XEXP (note, 1) = REG_NOTES (brcc_insn);
		      REG_NOTES (brcc_insn) = note;
		    }
		  note = find_reg_note (link_insn, REG_DEAD, op0);
		  if (note)
		    {
		      remove_note (link_insn, note);
		      XEXP (note, 1) = REG_NOTES (brcc_insn);
		      REG_NOTES (brcc_insn) = note;
		    }
		  note = find_reg_note (link_insn, REG_DEAD, op1);
		  if (note)
		    {
		      XEXP (note, 1) = REG_NOTES (brcc_insn);
		      REG_NOTES (brcc_insn) = note;
		    }

		  changed = 1;

		  /* Delete the bcc insn.  */
		  set_insn_deleted (insn);

		  /* Delete the cmp insn.  */
		  set_insn_deleted (link_insn);

		}
	    }
	}
      /* Clear out insn_addresses.  */
      INSN_ADDRESSES_FREE ();

    } while (changed);

  if (INSN_ADDRESSES_SET_P())
    fatal_error (input_location, "insn addresses not freed");

  arc_reorg_in_progress = 0;
}

 /* Check if the operands are valid for BRcc.d generation
    Valid Brcc.d patterns are
        Brcc.d b, c, s9
        Brcc.d b, u6, s9

        For cc={GT, LE, GTU, LEU}, u6=63 can not be allowed,
      since they are encoded by the assembler as {GE, LT, HS, LS} 64, which
      does not have a delay slot

  Assumed precondition: Second operand is either a register or a u6 value.  */

bool
valid_brcc_with_delay_p (rtx *operands)
{
  if (optimize_size && GET_MODE (operands[4]) == CC_Zmode)
    return false;
  return brcc_nolimm_operator (operands[0], VOIDmode);
}

/* Implement TARGET_IN_SMALL_DATA_P.  Return true if it would be safe to
   access DECL using %gp_rel(...)($gp).  */

static bool
arc_in_small_data_p (const_tree decl)
{
  HOST_WIDE_INT size;

  /* Only variables are going into small data area.  */
  if (TREE_CODE (decl) != VAR_DECL)
    return false;

  if (TARGET_NO_SDATA_SET)
    return false;

  /* Disable sdata references to weak variables.  */
  if (DECL_WEAK (decl))
    return false;

  /* Don't put constants into the small data section: we want them to
     be in ROM rather than RAM.  */
  if (TREE_READONLY (decl))
    return false;

  /* To ensure -mvolatile-cache works ld.di does not have a
     gp-relative variant.  */
  if (!TARGET_VOLATILE_CACHE_SET
      && TREE_THIS_VOLATILE (decl))
    return false;

  if (DECL_SECTION_NAME (decl) != 0)
    {
      const char *name = DECL_SECTION_NAME (decl);
      if (strcmp (name, ".sdata") == 0
	  || strcmp (name, ".sbss") == 0)
	return true;
    }
  /* If it's not public, there's no need to put it in the small data
     section.  */
  else if (TREE_PUBLIC (decl))
    {
      size = int_size_in_bytes (TREE_TYPE (decl));
      return (size > 0 && size <= g_switch_value);
    }
  return false;
}

/* Return true if X is a small data address that can be rewritten
   as a gp+symref.  */

static bool
arc_rewrite_small_data_p (const_rtx x)
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	x = XEXP (x, 0);
    }

  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_SMALL_P (x))
    {
      gcc_assert (SYMBOL_REF_TLS_MODEL (x) == 0);
      return true;
    }
  return false;
}

/* If possible, rewrite OP so that it refers to small data using
   explicit relocations.  */

static rtx
arc_rewrite_small_data_1 (rtx op)
{
  rtx rgp = gen_rtx_REG (Pmode, SDATA_BASE_REGNUM);
  op = copy_insn (op);
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, &op, ALL)
    {
      rtx *loc = *iter;
      if (arc_rewrite_small_data_p (*loc))
	{
	  *loc = gen_rtx_PLUS (Pmode, rgp, *loc);
	  iter.skip_subrtxes ();
	}
      else if (GET_CODE (*loc) == PLUS
	       && rtx_equal_p (XEXP (*loc, 0), rgp))
	iter.skip_subrtxes ();
    }
  return op;
}

rtx
arc_rewrite_small_data (rtx op)
{
  op = arc_rewrite_small_data_1 (op);

  /* Check if we fit small data constraints.  */
  if (MEM_P (op)
      && !LEGITIMATE_SMALL_DATA_ADDRESS_P (XEXP (op, 0)))
    {
      rtx addr = XEXP (op, 0);
      rtx tmp = gen_reg_rtx (Pmode);
      emit_move_insn (tmp, addr);
      op = replace_equiv_address_nv (op, tmp);
    }
  return op;
}

/* Return true if OP refers to small data symbols directly, not through
   a PLUS.  */

bool
small_data_pattern (rtx op, machine_mode)
{
  if (GET_CODE (op) == SEQUENCE)
    return false;

  rtx rgp = gen_rtx_REG (Pmode, SDATA_BASE_REGNUM);
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, op, ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == PLUS
	  && rtx_equal_p (XEXP (x, 0), rgp))
	iter.skip_subrtxes ();
      else if (arc_rewrite_small_data_p (x))
	return true;
    }
  return false;
}

/* Return true if OP is an acceptable memory operand for ARCompact
   16-bit gp-relative load instructions.
   op shd look like : [r26, symref@sda]
   i.e. (mem (plus (reg 26) (symref with smalldata flag set))
  */
/* volatile cache option still to be handled.  */

bool
compact_sda_memory_operand (rtx op, machine_mode mode, bool short_p)
{
  rtx addr;
  int size;
  tree decl = NULL_TREE;
  int align = 0;
  int mask = 0;

  /* Eliminate non-memory operations.  */
  if (GET_CODE (op) != MEM)
    return false;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate them.  */
  if (size > UNITS_PER_WORD)
    return false;

  /* Decode the address now.  */
  addr = XEXP (op, 0);

  if (!LEGITIMATE_SMALL_DATA_ADDRESS_P (addr))
    return false;

  if (!short_p || size == 1)
    return true;

  /* Now check for the alignment, the short loads using gp require the
     addresses to be aligned.  */
  if (GET_CODE (XEXP (addr, 1)) == SYMBOL_REF)
    decl = SYMBOL_REF_DECL (XEXP (addr, 1));
  else if (GET_CODE (XEXP (XEXP (XEXP (addr, 1), 0), 0)) == SYMBOL_REF)
    decl = SYMBOL_REF_DECL (XEXP (XEXP (XEXP (addr, 1), 0), 0));
  if (decl)
    align = DECL_ALIGN (decl);
  align = align / BITS_PER_UNIT;

  switch (mode)
    {
    case E_HImode:
      mask = 1;
      break;
    default:
      mask = 3;
      break;
    }

  if (align && ((align & mask) == 0))
    return true;
  return false;
}

/* Implement ASM_OUTPUT_ALIGNED_DECL_LOCAL.  */

void
arc_asm_output_aligned_decl_local (FILE * stream, tree decl, const char * name,
				   unsigned HOST_WIDE_INT size,
				   unsigned HOST_WIDE_INT align,
				   unsigned HOST_WIDE_INT globalize_p)
{
  int in_small_data =   arc_in_small_data_p (decl);

  if (in_small_data)
    switch_to_section (get_named_section (NULL, ".sbss", 0));
  /*    named_section (0,".sbss",0); */
  else
    switch_to_section (bss_section);

  if (globalize_p)
    (*targetm.asm_out.globalize_label) (stream, name);

  ASM_OUTPUT_ALIGN (stream, floor_log2 ((align) / BITS_PER_UNIT));
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "object");
  ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size);
  ASM_OUTPUT_LABEL (stream, name);

  if (size != 0)
    ASM_OUTPUT_SKIP (stream, size);
}

static bool
arc_preserve_reload_p (rtx in)
{
  return (GET_CODE (in) == PLUS
	  && RTX_OK_FOR_BASE_P (XEXP (in, 0), true)
	  && CONST_INT_P (XEXP (in, 1))
	  && !((INTVAL (XEXP (in, 1)) & 511)));
}

int
arc_register_move_cost (machine_mode,
			enum reg_class from_class, enum reg_class to_class)
{
  /* The ARC600 has no bypass for extension registers, hence a nop might be
     needed to be inserted after a write so that reads are safe.  */
  if (TARGET_ARC600)
    {
      if (to_class == MPY_WRITABLE_CORE_REGS)
	return 3;
     /* Instructions modifying LP_COUNT need 4 additional cycles before
	the register will actually contain the value.  */
      else if (to_class == LPCOUNT_REG)
	return 6;
      else if (to_class == WRITABLE_CORE_REGS)
	return 6;
    }

  /* Using lp_count as scratch reg is a VERY bad idea.  */
  if (from_class == LPCOUNT_REG)
    return 1000;
  if (to_class == LPCOUNT_REG)
    return 6;

  /* Force an attempt to 'mov Dy,Dx' to spill.  */
  if ((TARGET_ARC700 || TARGET_EM) && TARGET_DPFP
      && from_class == DOUBLE_REGS && to_class == DOUBLE_REGS)
    return 100;

  return 2;
}

/* Emit code for an addsi3 instruction with OPERANDS.
   COND_P indicates if this will use conditional execution.
   Return the length of the instruction.
   If OUTPUT_P is false, don't actually output the instruction, just return
   its length.  */
int
arc_output_addsi (rtx *operands, bool cond_p, bool output_p)
{
  char format[35];

  int match = operands_match_p (operands[0], operands[1]);
  int match2 = operands_match_p (operands[0], operands[2]);
  int intval = (REG_P (operands[2]) ? 1
		: CONST_INT_P (operands[2]) ? INTVAL (operands[2]) : 0xbadc057);
  int neg_intval = -intval;
  int short_0 = satisfies_constraint_Rcq (operands[0]);
  int short_p = (!cond_p && short_0 && satisfies_constraint_Rcq (operands[1]));
  int ret = 0;

#define REG_H_P(OP) (REG_P (OP) && ((TARGET_V2 && REGNO (OP) <= 31	\
				     && REGNO (OP) != 30)		\
				    || !TARGET_V2))

#define ADDSI_OUTPUT1(FORMAT) do {\
  if (output_p) \
    output_asm_insn (FORMAT, operands);\
  return ret; \
} while (0)
#define ADDSI_OUTPUT(LIST) do {\
  if (output_p) \
    sprintf LIST;\
  ADDSI_OUTPUT1 (format);\
  return ret; \
} while (0)

  /* First try to emit a 16 bit insn.  */
  ret = 2;
  if (!cond_p
      /* If we are actually about to output this insn, don't try a 16 bit
	 variant if we already decided that we don't want that
	 (I.e. we upsized this insn to align some following insn.)
	 E.g. add_s r0,sp,70 is 16 bit, but add r0,sp,70 requires a LIMM -
	 but add1 r0,sp,35 doesn't.  */
      && (!output_p || (get_attr_length (current_output_insn) & 2)))
    {
      /* Generate add_s a,b,c; add_s b,b,u7; add_s c,b,u3; add_s b,b,h
	 patterns.  */
      if (short_p
	  && ((REG_H_P (operands[2])
	       && (match || satisfies_constraint_Rcq (operands[2])))
	      || (CONST_INT_P (operands[2])
		  && ((unsigned) intval <= (match ? 127 : 7)))))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2 ;1");

      /* Generate add_s b,b,h patterns.  */
      if (short_0 && match2 && REG_H_P (operands[1]))
	ADDSI_OUTPUT1 ("add%? %0,%2,%1 ;2");

      /* Generate add_s b,sp,u7; add_s sp,sp,u7 patterns.  */
      if ((short_0 || REGNO (operands[0]) == STACK_POINTER_REGNUM)
	  && REGNO (operands[1]) == STACK_POINTER_REGNUM && !(intval & ~124))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2 ;3");

      if ((short_p && (unsigned) neg_intval <= (match ? 31 : 7))
	  || (REGNO (operands[0]) == STACK_POINTER_REGNUM
	      && match && !(neg_intval & ~124)))
	ADDSI_OUTPUT1 ("sub%? %0,%1,%n2 ;4");

      /* Generate add_s h,h,s3 patterns.  */
      if (REG_H_P (operands[0]) && match && TARGET_V2
	  && CONST_INT_P (operands[2]) && ((intval>= -1) && (intval <= 6)))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2 ;5");

      /* Generate add_s r0,b,u6; add_s r1,b,u6 patterns.  */
      if (TARGET_CODE_DENSITY && REG_P (operands[0]) && REG_P (operands[1])
	  && ((REGNO (operands[0]) == 0) || (REGNO (operands[0]) == 1))
	  && satisfies_constraint_Rcq (operands[1])
	  && satisfies_constraint_L (operands[2]))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2 ;6");
    }

  /* Now try to emit a 32 bit insn without long immediate.  */
  ret = 4;
  if (!match && match2 && REG_P (operands[1]))
    ADDSI_OUTPUT1 ("add%? %0,%2,%1");
  if (match || !cond_p)
    {
      int limit = (match && !cond_p) ? 0x7ff : 0x3f;
      int range_factor = neg_intval & intval;
      int shift;

      if (intval == (HOST_WIDE_INT) (HOST_WIDE_INT_M1U << 31))
	ADDSI_OUTPUT1 ("bxor%? %0,%1,31");

      /* If we can use a straight add / sub instead of a {add,sub}[123] of
	 same size, do, so - the insn latency is lower.  */
      /* -0x800 is a 12-bit constant for add /add3 / sub / sub3, but
	 0x800 is not.  */
      if ((intval >= 0 && intval <= limit)
	       || (intval == -0x800 && limit == 0x7ff))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2");
      else if ((intval < 0 && neg_intval <= limit)
	       || (intval == 0x800 && limit == 0x7ff))
	ADDSI_OUTPUT1 ("sub%? %0,%1,%n2");
      shift = range_factor >= 8 ? 3 : (range_factor >> 1);
      gcc_assert (shift == 0 || shift == 1 || shift == 2 || shift == 3);
      gcc_assert ((((1 << shift) - 1) & intval) == 0);
      if (((intval < 0 && intval != -0x4000)
	   /* sub[123] is slower than add_s / sub, only use it if it
	      avoids a long immediate.  */
	   && neg_intval <= limit << shift)
	  || (intval == 0x4000 && limit == 0x7ff))
	ADDSI_OUTPUT ((format, "sub%d%%? %%0,%%1,%d",
		       shift, neg_intval >> shift));
      else if ((intval >= 0 && intval <= limit << shift)
	       || (intval == -0x4000 && limit == 0x7ff))
	ADDSI_OUTPUT ((format, "add%d%%? %%0,%%1,%d", shift, intval >> shift));
    }
  /* Try to emit a 16 bit opcode with long immediate.  */
  ret = 6;
  if (short_p && match)
    ADDSI_OUTPUT1 ("add%? %0,%1,%S2");

  /* We have to use a 32 bit opcode, and with a long immediate.  */
  ret = 8;
  ADDSI_OUTPUT1 (intval < 0 ? "sub%? %0,%1,%n2" : "add%? %0,%1,%S2");
}

/* Emit code for an commutative_cond_exec instruction with OPERANDS.
   Return the length of the instruction.
   If OUTPUT_P is false, don't actually output the instruction, just return
   its length.  */
int
arc_output_commutative_cond_exec (rtx *operands, bool output_p)
{
  enum rtx_code commutative_op = GET_CODE (operands[3]);
  const char *pat = NULL;

  /* Canonical rtl should not have a constant in the first operand position.  */
  gcc_assert (!CONSTANT_P (operands[1]));

  switch (commutative_op)
    {
      case AND:
	if (satisfies_constraint_C1p (operands[2]))
	  pat = "bmsk%? %0,%1,%Z2";
	else if (satisfies_constraint_C2p (operands[2]))
	  {
	    operands[2] = GEN_INT ((~INTVAL (operands[2])));
	    pat = "bmskn%? %0,%1,%Z2";
	  }
	else if (satisfies_constraint_Ccp (operands[2]))
	  pat = "bclr%? %0,%1,%M2";
	else if (satisfies_constraint_CnL (operands[2]))
	  pat = "bic%? %0,%1,%n2-1";
	break;
      case IOR:
	if (satisfies_constraint_C0p (operands[2]))
	  pat = "bset%? %0,%1,%z2";
	break;
      case XOR:
	if (satisfies_constraint_C0p (operands[2]))
	  pat = "bxor%? %0,%1,%z2";
	break;
      case PLUS:
	return arc_output_addsi (operands, true, output_p);
      default: break;
    }
  if (output_p)
    output_asm_insn (pat ? pat : "%O3.%d5 %0,%1,%2", operands);
  if (pat || REG_P (operands[2]) || satisfies_constraint_L (operands[2]))
    return 4;
  return 8;
}

/* Helper function of arc_expand_movmem.  ADDR points to a chunk of memory.
   Emit code and return an potentially modified address such that offsets
   up to SIZE are can be added to yield a legitimate address.
   if REUSE is set, ADDR is a register that may be modified.  */

static rtx
force_offsettable (rtx addr, HOST_WIDE_INT size, bool reuse)
{
  rtx base = addr;
  rtx offs = const0_rtx;

  if (GET_CODE (base) == PLUS)
    {
      offs = XEXP (base, 1);
      base = XEXP (base, 0);
    }
  if (!REG_P (base)
      || (REGNO (base) != STACK_POINTER_REGNUM
	  && REGNO_PTR_FRAME_P (REGNO (base)))
      || !CONST_INT_P (offs) || !SMALL_INT (INTVAL (offs))
      || !SMALL_INT (INTVAL (offs) + size))
    {
      if (reuse)
	emit_insn (gen_add2_insn (addr, offs));
      else
	addr = copy_to_mode_reg (Pmode, addr);
    }
  return addr;
}

/* Like move_by_pieces, but take account of load latency, and actual
   offset ranges.  Return true on success.  */

bool
arc_expand_movmem (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx dst_addr, src_addr;
  HOST_WIDE_INT size;
  int align = INTVAL (operands[3]);
  unsigned n_pieces;
  int piece = align;
  rtx store[2];
  rtx tmpx[2];
  int i;

  if (!CONST_INT_P (operands[2]))
    return false;
  size = INTVAL (operands[2]);
  /* move_by_pieces_ninsns is static, so we can't use it.  */
  if (align >= 4)
    {
      if (TARGET_LL64)
	n_pieces = (size + 4) / 8U + ((size >> 1) & 1) + (size & 1);
      else
	n_pieces = (size + 2) / 4U + (size & 1);
    }
  else if (align == 2)
    n_pieces = (size + 1) / 2U;
  else
    n_pieces = size;
  if (n_pieces >= (unsigned int) (optimize_size ? 3 : 15))
    return false;
  /* Force 32 bit aligned and larger datum to use 64 bit transfers, if
     possible.  */
  if (TARGET_LL64 && (piece >= 4) && (size >= 8))
    piece = 8;
  else if (piece > 4)
    piece = 4;
  dst_addr = force_offsettable (XEXP (operands[0], 0), size, 0);
  src_addr = force_offsettable (XEXP (operands[1], 0), size, 0);
  store[0] = store[1] = NULL_RTX;
  tmpx[0] = tmpx[1] = NULL_RTX;
  for (i = 0; size > 0; i ^= 1, size -= piece)
    {
      rtx tmp;
      machine_mode mode;

      while (piece > size)
	piece >>= 1;
      mode = smallest_int_mode_for_size (piece * BITS_PER_UNIT);
      /* If we don't re-use temporaries, the scheduler gets carried away,
	 and the register pressure gets unnecessarily high.  */
      if (0 && tmpx[i] && GET_MODE (tmpx[i]) == mode)
	tmp = tmpx[i];
      else
	tmpx[i] = tmp = gen_reg_rtx (mode);
      dst_addr = force_offsettable (dst_addr, piece, 1);
      src_addr = force_offsettable (src_addr, piece, 1);
      if (store[i])
	emit_insn (store[i]);
      emit_move_insn (tmp, change_address (src, mode, src_addr));
      store[i] = gen_move_insn (change_address (dst, mode, dst_addr), tmp);
      dst_addr = plus_constant (Pmode, dst_addr, piece);
      src_addr = plus_constant (Pmode, src_addr, piece);
    }
  if (store[i])
    emit_insn (store[i]);
  if (store[i^1])
    emit_insn (store[i^1]);
  return true;
}

/* Prepare operands for move in MODE.  Return true iff the move has
   been emitted.  */

bool
prepare_move_operands (rtx *operands, machine_mode mode)
{
  /* We used to do this only for MODE_INT Modes, but addresses to floating
     point variables may well be in the small data section.  */
  if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[0], Pmode))
    operands[0] = arc_rewrite_small_data (operands[0]);

  if (mode == SImode && SYMBOLIC_CONST (operands[1]))
    {
      prepare_pic_move (operands, SImode);

      /* Disable any REG_EQUALs associated with the symref
	 otherwise the optimization pass undoes the work done
	 here and references the variable directly.  */
    }

  if (GET_CODE (operands[0]) != MEM
      && !TARGET_NO_SDATA_SET
      && small_data_pattern (operands[1], Pmode))
    {
      /* This is to take care of address calculations involving sdata
	 variables.  */
      operands[1] = arc_rewrite_small_data (operands[1]);

      emit_insn (gen_rtx_SET (operands[0],operands[1]));
      /* ??? This note is useless, since it only restates the set itself.
	 We should rather use the original SYMBOL_REF.  However, there is
	 the problem that we are lying to the compiler about these
	 SYMBOL_REFs to start with.  symbol@sda should be encoded specially
	 so that we can tell it apart from an actual symbol.  */
      set_unique_reg_note (get_last_insn (), REG_EQUAL, operands[1]);

      /* Take care of the REG_EQUAL note that will be attached to mark the
	 output reg equal to the initial symbol_ref after this code is
	 executed.  */
      emit_move_insn (operands[0], operands[0]);
      return true;
    }

  if (MEM_P (operands[0])
      && !(reload_in_progress || reload_completed))
    {
      operands[1] = force_reg (mode, operands[1]);
      if (!move_dest_operand (operands[0], mode))
	{
	  rtx addr = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
	  /* This is like change_address_1 (operands[0], mode, 0, 1) ,
	     except that we can't use that function because it is static.  */
	  rtx pat = change_address (operands[0], mode, addr);
	  MEM_COPY_ATTRIBUTES (pat, operands[0]);
	  operands[0] = pat;
	}
      if (!cse_not_expected)
	{
	  rtx pat = XEXP (operands[0], 0);

	  pat = arc_legitimize_address_0 (pat, pat, mode);
	  if (pat)
	    {
	      pat = change_address (operands[0], mode, pat);
	      MEM_COPY_ATTRIBUTES (pat, operands[0]);
	      operands[0] = pat;
	    }
	}
    }

  if (MEM_P (operands[1]) && !cse_not_expected)
    {
      rtx pat = XEXP (operands[1], 0);

      pat = arc_legitimize_address_0 (pat, pat, mode);
      if (pat)
	{
	  pat = change_address (operands[1], mode, pat);
	  MEM_COPY_ATTRIBUTES (pat, operands[1]);
	  operands[1] = pat;
	}
    }

  return false;
}

/* Prepare OPERANDS for an extension using CODE to OMODE.
   Return true iff the move has been emitted.  */

bool
prepare_extend_operands (rtx *operands, enum rtx_code code,
			 machine_mode omode)
{
  if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[1], Pmode))
    {
      /* This is to take care of address calculations involving sdata
	 variables.  */
      operands[1]
	= gen_rtx_fmt_e (code, omode, arc_rewrite_small_data (operands[1]));
      emit_insn (gen_rtx_SET (operands[0], operands[1]));
      set_unique_reg_note (get_last_insn (), REG_EQUAL, operands[1]);

      /* Take care of the REG_EQUAL note that will be attached to mark the
	 output reg equal to the initial extension after this code is
	 executed.  */
      emit_move_insn (operands[0], operands[0]);
      return true;
    }
  return false;
}

/* Output a library call to a function called FNAME that has been arranged
   to be local to any dso.  */

const char *
arc_output_libcall (const char *fname)
{
  unsigned len = strlen (fname);
  static char buf[64];

  gcc_assert (len < sizeof buf - 35);
  if (TARGET_LONG_CALLS_SET
     || (TARGET_MEDIUM_CALLS && arc_ccfsm_cond_exec_p ()))
    {
      if (flag_pic)
	sprintf (buf, "add r12,pcl,@%s@pcl\n\tjl%%!%%* [r12]", fname);
      else
	sprintf (buf, "jl%%! @%s", fname);
    }
  else
    sprintf (buf, "bl%%!%%* @%s", fname);
  return buf;
}

/* Return the SImode highpart of the DImode value IN.  */

rtx
disi_highpart (rtx in)
{
  return simplify_gen_subreg (SImode, in, DImode, TARGET_BIG_ENDIAN ? 0 : 4);
}

/* Return length adjustment for INSN.
   For ARC600:
   A write to a core reg greater or equal to 32 must not be immediately
   followed by a use.  Anticipate the length requirement to insert a nop
   between PRED and SUCC to prevent a hazard.  */

static int
arc600_corereg_hazard (rtx_insn *pred, rtx_insn *succ)
{
  if (!TARGET_ARC600)
    return 0;
  if (GET_CODE (PATTERN (pred)) == SEQUENCE)
    pred = as_a <rtx_sequence *> (PATTERN (pred))->insn (1);
  if (GET_CODE (PATTERN (succ)) == SEQUENCE)
    succ = as_a <rtx_sequence *> (PATTERN (succ))->insn (0);
  if (recog_memoized (pred) == CODE_FOR_mulsi_600
      || recog_memoized (pred) == CODE_FOR_umul_600
      || recog_memoized (pred) == CODE_FOR_mac_600
      || recog_memoized (pred) == CODE_FOR_mul64_600
      || recog_memoized (pred) == CODE_FOR_mac64_600
      || recog_memoized (pred) == CODE_FOR_umul64_600
      || recog_memoized (pred) == CODE_FOR_umac64_600)
    return 0;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (pred), NONCONST)
    {
      const_rtx x = *iter;
      switch (GET_CODE (x))
	{
	case SET: case POST_INC: case POST_DEC: case PRE_INC: case PRE_DEC:
	  break;
	default:
	  /* This is also fine for PRE/POST_MODIFY, because they
	     contain a SET.  */
	  continue;
	}
      rtx dest = XEXP (x, 0);
      /* Check if this sets a an extension register.  N.B. we use 61 for the
	 condition codes, which is definitely not an extension register.  */
      if (REG_P (dest) && REGNO (dest) >= 32 && REGNO (dest) < 61
	  /* Check if the same register is used by the PAT.  */
	  && (refers_to_regno_p
	      (REGNO (dest),
	       REGNO (dest) + (GET_MODE_SIZE (GET_MODE (dest)) + 3) / 4U,
	       PATTERN (succ), 0)))
	return 4;
    }
  return 0;
}

/* Given a rtx, check if it is an assembly instruction or not.  */

static int
arc_asm_insn_p (rtx x)
{
  int i, j;

  if (x == 0)
    return 0;

  switch (GET_CODE (x))
    {
    case ASM_OPERANDS:
    case ASM_INPUT:
      return 1;

    case SET:
      return arc_asm_insn_p (SET_SRC (x));

    case PARALLEL:
      j = 0;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	j += arc_asm_insn_p (XVECEXP (x, 0, i));
      if ( j > 0)
	return 1;
      break;

    default:
      break;
    }

  return 0;
}

/* For ARC600:
   A write to a core reg greater or equal to 32 must not be immediately
   followed by a use.  Anticipate the length requirement to insert a nop
   between PRED and SUCC to prevent a hazard.  */

int
arc_hazard (rtx_insn *pred, rtx_insn *succ)
{
  if (!pred || !INSN_P (pred) || !succ || !INSN_P (succ))
    return 0;

  if (TARGET_ARC600)
    return arc600_corereg_hazard (pred, succ);

  return 0;
}

/* Return length adjustment for INSN.  */

int
arc_adjust_insn_length (rtx_insn *insn, int len, bool)
{
  if (!INSN_P (insn))
    return len;
  /* We already handle sequences by ignoring the delay sequence flag.  */
  if (GET_CODE (PATTERN (insn)) == SEQUENCE)
    return len;

  /* Check for return with but one preceding insn since function
     start / call.  */
  if (TARGET_PAD_RETURN
      && JUMP_P (insn)
      && GET_CODE (PATTERN (insn)) != ADDR_VEC
      && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
      && get_attr_type (insn) == TYPE_RETURN)
    {
      rtx_insn *prev = prev_active_insn (insn);

      if (!prev || !(prev = prev_active_insn (prev))
	  || ((NONJUMP_INSN_P (prev)
	       && GET_CODE (PATTERN (prev)) == SEQUENCE)
	      ? CALL_ATTR (as_a <rtx_sequence *> (PATTERN (prev))->insn (0),
			   NON_SIBCALL)
	      : CALL_ATTR (prev, NON_SIBCALL)))
	return len + 4;
    }
  if (TARGET_ARC600)
    {
      rtx_insn *succ = next_real_insn (insn);

      /* One the ARC600, a write to an extension register must be separated
	 from a read.  */
      if (succ && INSN_P (succ))
	len += arc600_corereg_hazard (insn, succ);
    }

  /* Restore extracted operands - otherwise splitters like the addsi3_mixed one
     can go awry.  */
  extract_constrain_insn_cached (insn);

  return len;
}

/* Values for length_sensitive.  */
enum
{
  ARC_LS_NONE,// Jcc
  ARC_LS_25, // 25 bit offset, B
  ARC_LS_21, // 21 bit offset, Bcc
  ARC_LS_U13,// 13 bit unsigned offset, LP
  ARC_LS_10, // 10 bit offset, B_s, Beq_s, Bne_s
  ARC_LS_9,  //  9 bit offset, BRcc
  ARC_LS_8,  //  8 bit offset, BRcc_s
  ARC_LS_U7, //  7 bit unsigned offset, LPcc
  ARC_LS_7   //  7 bit offset, Bcc_s
};

/* While the infrastructure patch is waiting for review, duplicate the
   struct definitions, to allow this file to compile.  */
#if 1
typedef struct
{
  unsigned align_set;
  /* Cost as a branch / call target or call return address.  */
  int target_cost;
  int fallthrough_cost;
  int branch_cost;
  int length;
  /* 0 for not length sensitive, 1 for largest offset range,
 *      2 for next smaller etc.  */
  unsigned length_sensitive : 8;
  bool enabled;
} insn_length_variant_t;

typedef struct insn_length_parameters_s
{
  int align_unit_log;
  int align_base_log;
  int max_variants;
  int (*get_variants) (rtx_insn *, int, bool, bool, insn_length_variant_t *);
} insn_length_parameters_t;

static void
arc_insn_length_parameters (insn_length_parameters_t *ilp) ATTRIBUTE_UNUSED;
#endif

static int
arc_get_insn_variants (rtx_insn *insn, int len, bool, bool target_p,
		       insn_length_variant_t *ilv)
{
  if (!NONDEBUG_INSN_P (insn))
    return 0;
  enum attr_type type;
  /* shorten_branches doesn't take optimize_size into account yet for the
     get_variants mechanism, so turn this off for now.  */
  if (optimize_size)
    return 0;
  if (rtx_sequence *pat = dyn_cast <rtx_sequence *> (PATTERN (insn)))
    {
      /* The interaction of a short delay slot insn with a short branch is
	 too weird for shorten_branches to piece together, so describe the
	 entire SEQUENCE.  */
      rtx_insn *inner;
      if (TARGET_UPSIZE_DBR
	  && get_attr_length (pat->insn (1)) <= 2
	  && (((type = get_attr_type (inner = pat->insn (0)))
	       == TYPE_UNCOND_BRANCH)
	      || type == TYPE_BRANCH)
	  && get_attr_delay_slot_filled (inner) == DELAY_SLOT_FILLED_YES)
	{
	  int n_variants
	    = arc_get_insn_variants (inner, get_attr_length (inner), true,
				     target_p, ilv+1);
	  /* The short variant gets split into a higher-cost aligned
	     and a lower cost unaligned variant.  */
	  gcc_assert (n_variants);
	  gcc_assert (ilv[1].length_sensitive == ARC_LS_7
		      || ilv[1].length_sensitive == ARC_LS_10);
	  gcc_assert (ilv[1].align_set == 3);
	  ilv[0] = ilv[1];
	  ilv[0].align_set = 1;
	  ilv[0].branch_cost += 1;
	  ilv[1].align_set = 2;
	  n_variants++;
	  for (int i = 0; i < n_variants; i++)
	    ilv[i].length += 2;
	  /* In case an instruction with aligned size is wanted, and
	     the short variants are unavailable / too expensive, add
	     versions of long branch + long delay slot.  */
	  for (int i = 2, end = n_variants; i < end; i++, n_variants++)
	    {
	      ilv[n_variants] = ilv[i];
	      ilv[n_variants].length += 2;
	    }
	  return n_variants;
	}
      return 0;
    }
  insn_length_variant_t *first_ilv = ilv;
  type = get_attr_type (insn);
  bool delay_filled
    = (get_attr_delay_slot_filled (insn) == DELAY_SLOT_FILLED_YES);
  int branch_align_cost = delay_filled ? 0 : 1;
  int branch_unalign_cost = delay_filled ? 0 : TARGET_UNALIGN_BRANCH ? 0 : 1;
  /* If the previous instruction is an sfunc call, this insn is always
     a target, even though the middle-end is unaware of this.  */
  bool force_target = false;
  rtx_insn *prev = prev_active_insn (insn);
  if (prev && arc_next_active_insn (prev, 0) == insn
      && ((NONJUMP_INSN_P (prev) && GET_CODE (PATTERN (prev)) == SEQUENCE)
	  ? CALL_ATTR (as_a <rtx_sequence *> (PATTERN (prev))->insn (0),
		       NON_SIBCALL)
	  : (CALL_ATTR (prev, NON_SIBCALL)
	     && NEXT_INSN (PREV_INSN (prev)) == prev)))
    force_target = true;

  switch (type)
    {
    case TYPE_BRCC:
      /* Short BRCC only comes in no-delay-slot version, and without limm  */
      if (!delay_filled)
	{
	  ilv->align_set = 3;
	  ilv->length = 2;
	  ilv->branch_cost = 1;
	  ilv->enabled = (len == 2);
	  ilv->length_sensitive = ARC_LS_8;
	  ilv++;
	}
      /* Fall through.  */
    case TYPE_BRCC_NO_DELAY_SLOT:
      /* doloop_fallback* patterns are TYPE_BRCC_NO_DELAY_SLOT for
	 (delay slot) scheduling purposes, but they are longer.  */
      if (GET_CODE (PATTERN (insn)) == PARALLEL
	  && GET_CODE (XVECEXP (PATTERN (insn), 0, 1)) == SET)
	return 0;
      /* Standard BRCC: 4 bytes, or 8 bytes with limm.  */
      ilv->length = ((type == TYPE_BRCC) ? 4 : 8);
      ilv->align_set = 3;
      ilv->branch_cost = branch_align_cost;
      ilv->enabled = (len <= ilv->length);
      ilv->length_sensitive = ARC_LS_9;
      if ((target_p || force_target)
	  || (!delay_filled && TARGET_UNALIGN_BRANCH))
	{
	  ilv[1] = *ilv;
	  ilv->align_set = 1;
	  ilv++;
	  ilv->align_set = 2;
	  ilv->target_cost = 1;
	  ilv->branch_cost = branch_unalign_cost;
	}
      ilv++;

      rtx op, op0;
      op = XEXP (SET_SRC (XVECEXP (PATTERN (insn), 0, 0)), 0);
      op0 = XEXP (op, 0);

      if (GET_CODE (op0) == ZERO_EXTRACT
	  && satisfies_constraint_L (XEXP (op0, 2)))
	op0 = XEXP (op0, 0);
      if (satisfies_constraint_Rcq (op0))
	{
	  ilv->length = ((type == TYPE_BRCC) ? 6 : 10);
	  ilv->align_set = 3;
	  ilv->branch_cost = 1 + branch_align_cost;
	  ilv->fallthrough_cost = 1;
	  ilv->enabled = true;
	  ilv->length_sensitive = ARC_LS_21;
	  if (!delay_filled && TARGET_UNALIGN_BRANCH)
	    {
	      ilv[1] = *ilv;
	      ilv->align_set = 1;
	      ilv++;
	      ilv->align_set = 2;
	      ilv->branch_cost = 1 + branch_unalign_cost;
	    }
	  ilv++;
	}
      ilv->length = ((type == TYPE_BRCC) ? 8 : 12);
      ilv->align_set = 3;
      ilv->branch_cost = 1 + branch_align_cost;
      ilv->fallthrough_cost = 1;
      ilv->enabled = true;
      ilv->length_sensitive = ARC_LS_21;
      if ((target_p || force_target)
	  || (!delay_filled && TARGET_UNALIGN_BRANCH))
	{
	  ilv[1] = *ilv;
	  ilv->align_set = 1;
	  ilv++;
	  ilv->align_set = 2;
	  ilv->target_cost = 1;
	  ilv->branch_cost = 1 + branch_unalign_cost;
	}
      ilv++;
      break;

    case TYPE_SFUNC:
      ilv->length = 12;
      goto do_call;
    case TYPE_CALL_NO_DELAY_SLOT:
      ilv->length = 8;
      goto do_call;
    case TYPE_CALL:
      ilv->length = 4;
      ilv->length_sensitive
	= GET_CODE (PATTERN (insn)) == COND_EXEC ? ARC_LS_21 : ARC_LS_25;
    do_call:
      ilv->align_set = 3;
      ilv->fallthrough_cost = branch_align_cost;
      ilv->enabled = true;
      if ((target_p || force_target)
	  || (!delay_filled && TARGET_UNALIGN_BRANCH))
	{
	  ilv[1] = *ilv;
	  ilv->align_set = 1;
	  ilv++;
	  ilv->align_set = 2;
	  ilv->target_cost = 1;
	  ilv->fallthrough_cost = branch_unalign_cost;
	}
      ilv++;
      break;
    case TYPE_UNCOND_BRANCH:
      /* Strictly speaking, this should be ARC_LS_10 for equality comparisons,
	 but that makes no difference at the moment.  */
      ilv->length_sensitive = ARC_LS_7;
      ilv[1].length_sensitive = ARC_LS_25;
      goto do_branch;
    case TYPE_BRANCH:
      ilv->length_sensitive = ARC_LS_10;
      ilv[1].length_sensitive = ARC_LS_21;
    do_branch:
      ilv->align_set = 3;
      ilv->length = 2;
      ilv->branch_cost = branch_align_cost;
      ilv->enabled = (len == ilv->length);
      ilv++;
      ilv->length = 4;
      ilv->align_set = 3;
      ilv->branch_cost = branch_align_cost;
      ilv->enabled = true;
      if ((target_p || force_target)
	  || (!delay_filled && TARGET_UNALIGN_BRANCH))
	{
	  ilv[1] = *ilv;
	  ilv->align_set = 1;
	  ilv++;
	  ilv->align_set = 2;
	  ilv->target_cost = 1;
	  ilv->branch_cost = branch_unalign_cost;
	}
      ilv++;
      break;
    case TYPE_JUMP:
      return 0;
    default:
      /* For every short insn, there is generally also a long insn.
	 trap_s is an exception.  */
      if ((len & 2) == 0 || recog_memoized (insn) == CODE_FOR_trap_s)
	return 0;
      ilv->align_set = 3;
      ilv->length = len;
      ilv->enabled = 1;
      ilv++;
      ilv->align_set = 3;
      ilv->length = len + 2;
      ilv->enabled = 1;
      if (target_p || force_target)
	{
	  ilv[1] = *ilv;
	  ilv->align_set = 1;
	  ilv++;
	  ilv->align_set = 2;
	  ilv->target_cost = 1;
	}
      ilv++;
    }
  /* If the previous instruction is an sfunc call, this insn is always
     a target, even though the middle-end is unaware of this.
     Therefore, if we have a call predecessor, transfer the target cost
     to the fallthrough and branch costs.  */
  if (force_target)
    {
      for (insn_length_variant_t *p = first_ilv; p < ilv; p++)
	{
	  p->fallthrough_cost += p->target_cost;
	  p->branch_cost += p->target_cost;
	  p->target_cost = 0;
	}
    }

  return ilv - first_ilv;
}

static void
arc_insn_length_parameters (insn_length_parameters_t *ilp)
{
  ilp->align_unit_log = 1;
  ilp->align_base_log = 1;
  ilp->max_variants = 7;
  ilp->get_variants = arc_get_insn_variants;
}

/* Return a copy of COND from *STATEP, inverted if that is indicated by the
   CC field of *STATEP.  */

static rtx
arc_get_ccfsm_cond (struct arc_ccfsm *statep, bool reverse)
{
  rtx cond = statep->cond;
  int raw_cc = get_arc_condition_code (cond);
  if (reverse)
    raw_cc = ARC_INVERSE_CONDITION_CODE (raw_cc);

  if (statep->cc == raw_cc)
    return copy_rtx (cond);

  gcc_assert (ARC_INVERSE_CONDITION_CODE (raw_cc) == statep->cc);

  machine_mode ccm = GET_MODE (XEXP (cond, 0));
  enum rtx_code code = reverse_condition (GET_CODE (cond));
  if (code == UNKNOWN || ccm == CC_FP_GTmode || ccm == CC_FP_GEmode)
    code = reverse_condition_maybe_unordered (GET_CODE (cond));

  return gen_rtx_fmt_ee (code, GET_MODE (cond),
			 copy_rtx (XEXP (cond, 0)), copy_rtx (XEXP (cond, 1)));
}

/* Return version of PAT conditionalized with COND, which is part of INSN.
   ANNULLED indicates if INSN is an annulled delay-slot insn.
   Register further changes if necessary.  */
static rtx
conditionalize_nonjump (rtx pat, rtx cond, rtx insn, bool annulled)
{
  /* For commutative operators, we generally prefer to have
     the first source match the destination.  */
  if (GET_CODE (pat) == SET)
    {
      rtx src = SET_SRC (pat);

      if (COMMUTATIVE_P (src))
	{
	  rtx src0 = XEXP (src, 0);
	  rtx src1 = XEXP (src, 1);
	  rtx dst = SET_DEST (pat);

	  if (rtx_equal_p (src1, dst) && !rtx_equal_p (src0, dst)
	      /* Leave add_n alone - the canonical form is to
		 have the complex summand first.  */
	      && REG_P (src0))
	    pat = gen_rtx_SET (dst,
			       gen_rtx_fmt_ee (GET_CODE (src), GET_MODE (src),
					       src1, src0));
	}
    }

  /* dwarf2out.c:dwarf2out_frame_debug_expr doesn't know
     what to do with COND_EXEC.  */
  if (RTX_FRAME_RELATED_P (insn))
    {
      /* If this is the delay slot insn of an anulled branch,
	 dwarf2out.c:scan_trace understands the anulling semantics
	 without the COND_EXEC.  */
      gcc_assert (annulled);
      rtx note = alloc_reg_note (REG_FRAME_RELATED_EXPR, pat,
				 REG_NOTES (insn));
      validate_change (insn, &REG_NOTES (insn), note, 1);
    }
  pat = gen_rtx_COND_EXEC (VOIDmode, cond, pat);
  return pat;
}

/* Use the ccfsm machinery to do if conversion.  */

static unsigned
arc_ifcvt (void)
{
  struct arc_ccfsm *statep = &cfun->machine->ccfsm_current;

  memset (statep, 0, sizeof *statep);
  for (rtx_insn *insn = get_insns (); insn; insn = next_insn (insn))
    {
      arc_ccfsm_advance (insn, statep);

      switch (statep->state)
	{
	case 0:
	  break;
	case 1: case 2:
	  {
	    /* Deleted branch.  */
	    arc_ccfsm_post_advance (insn, statep);
	    gcc_assert (!IN_RANGE (statep->state, 1, 2));
	    rtx_insn *seq = NEXT_INSN (PREV_INSN (insn));
	    if (GET_CODE (PATTERN (seq)) == SEQUENCE)
	      {
		rtx slot = XVECEXP (PATTERN (seq), 0, 1);
		rtx pat = PATTERN (slot);
		if (INSN_ANNULLED_BRANCH_P (insn))
		  {
		    rtx cond
		      = arc_get_ccfsm_cond (statep, INSN_FROM_TARGET_P (slot));
		    pat = gen_rtx_COND_EXEC (VOIDmode, cond, pat);
		  }
		if (!validate_change (seq, &PATTERN (seq), pat, 0))
		  gcc_unreachable ();
		PUT_CODE (slot, NOTE);
		NOTE_KIND (slot) = NOTE_INSN_DELETED;
	      }
	    else
	      {
		set_insn_deleted (insn);
	      }
	    continue;
	  }
	case 3:
	  if (LABEL_P (insn)
	      && statep->target_label == CODE_LABEL_NUMBER (insn))
	    {
	      arc_ccfsm_post_advance (insn, statep);
	      if (--LABEL_NUSES (insn) == 0)
		delete_insn (insn);
	      continue;
	    }
	  /* Fall through.  */
	case 4: case 5:
	  if (!NONDEBUG_INSN_P (insn))
	    break;

	  /* Conditionalized insn.  */

	  rtx_insn *prev, *pprev;
	  rtx *patp, pat, cond;
	  bool annulled; annulled = false;

	  /* If this is a delay slot insn in a non-annulled branch,
	     don't conditionalize it.  N.B., this should be fine for
	     conditional return too.  However, don't do this for
	     unconditional branches, as these would be encountered when
	     processing an 'else' part.  */
	  prev = PREV_INSN (insn);
	  pprev = PREV_INSN (prev);
	  if (pprev && NEXT_INSN (NEXT_INSN (pprev)) == NEXT_INSN (insn)
	      && JUMP_P (prev) && get_attr_cond (prev) == COND_USE)
	    {
	      if (!INSN_ANNULLED_BRANCH_P (prev))
		break;
	      annulled = true;
	    }

	  patp = &PATTERN (insn);
	  pat = *patp;
	  cond = arc_get_ccfsm_cond (statep, INSN_FROM_TARGET_P (insn));
	  if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	    {
	      /* ??? don't conditionalize if all side effects are dead
		 in the not-execute case.  */

	      pat = conditionalize_nonjump (pat, cond, insn, annulled);
	    }
	  else if (simplejump_p (insn))
	    {
	      patp = &SET_SRC (pat);
	      pat = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, *patp, pc_rtx);
	    }
	  else if (JUMP_P (insn) && ANY_RETURN_P (PATTERN (insn)))
	    {
	      pat = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, pat, pc_rtx);
	      pat = gen_rtx_SET (pc_rtx, pat);
	    }
	  else
	    gcc_unreachable ();
	  validate_change (insn, patp, pat, 1);
	  if (!apply_change_group ())
	    gcc_unreachable ();
	  if (JUMP_P (insn))
	    {
	      rtx_insn *next = next_nonnote_insn (insn);
	      if (GET_CODE (next) == BARRIER)
		delete_insn (next);
	      if (statep->state == 3)
		continue;
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
      arc_ccfsm_post_advance (insn, statep);
    }
  return 0;
}

/* Find annulled delay insns and convert them to use the appropriate predicate.
   This allows branch shortening to size up these insns properly.  */

static unsigned
arc_predicate_delay_insns (void)
{
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat, jump, dlay, src, cond, *patp;
      int reverse;

      if (!NONJUMP_INSN_P (insn)
	  || GET_CODE (pat = PATTERN (insn)) != SEQUENCE)
	continue;
      jump = XVECEXP (pat, 0, 0);
      dlay = XVECEXP (pat, 0, 1);
      if (!JUMP_P (jump) || !INSN_ANNULLED_BRANCH_P (jump))
	continue;
      /* If the branch insn does the annulling, leave the delay insn alone.  */
      if (!TARGET_AT_DBR_CONDEXEC && !INSN_FROM_TARGET_P (dlay))
	continue;
      /* ??? Could also leave DLAY un-conditionalized if its target is dead
	 on the other path.  */
      gcc_assert (GET_CODE (PATTERN (jump)) == SET);
      gcc_assert (SET_DEST (PATTERN (jump)) == pc_rtx);
      src = SET_SRC (PATTERN (jump));
      gcc_assert (GET_CODE (src) == IF_THEN_ELSE);
      cond = XEXP (src, 0);
      if (XEXP (src, 2) == pc_rtx)
	reverse = 0;
      else if (XEXP (src, 1) == pc_rtx)
	reverse = 1;
      else
	gcc_unreachable ();
      if (reverse != !INSN_FROM_TARGET_P (dlay))
	{
	  machine_mode ccm = GET_MODE (XEXP (cond, 0));
	  enum rtx_code code = reverse_condition (GET_CODE (cond));
	  if (code == UNKNOWN || ccm == CC_FP_GTmode || ccm == CC_FP_GEmode)
	    code = reverse_condition_maybe_unordered (GET_CODE (cond));

	  cond = gen_rtx_fmt_ee (code, GET_MODE (cond),
				 copy_rtx (XEXP (cond, 0)),
				 copy_rtx (XEXP (cond, 1)));
	}
      else
	cond = copy_rtx (cond);
      patp = &PATTERN (dlay);
      pat = *patp;
      pat = conditionalize_nonjump (pat, cond, dlay, true);
      validate_change (dlay, patp, pat, 1);
      if (!apply_change_group ())
	gcc_unreachable ();
    }
  return 0;
}

/* For ARC600: If a write to a core reg >=32 appears in a delay slot
  (other than of a forward brcc), it creates a hazard when there is a read
  of the same register at the branch target.  We can't know what is at the
  branch target of calls, and for branches, we don't really know before the
  end of delay slot scheduling, either.  Not only can individual instruction
  be hoisted out into a delay slot, a basic block can also be emptied this
  way, and branch and/or fall through targets be redirected.  Hence we don't
  want such writes in a delay slot.  */

/* Return nonzreo iff INSN writes to an extension core register.  */

int
arc_write_ext_corereg (rtx insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    {
      const_rtx x = *iter;
      switch (GET_CODE (x))
	{
	case SET: case POST_INC: case POST_DEC: case PRE_INC: case PRE_DEC:
	  break;
	default:
	  /* This is also fine for PRE/POST_MODIFY, because they
	     contain a SET.  */
	  continue;
	}
      const_rtx dest = XEXP (x, 0);
      if (REG_P (dest) && REGNO (dest) >= 32 && REGNO (dest) < 61)
	return 1;
    }
  return 0;
}

/* This is like the hook, but returns NULL when it can't / won't generate
   a legitimate address.  */

static rtx
arc_legitimize_address_0 (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  machine_mode mode)
{
  rtx addr, inner;

  if (flag_pic && SYMBOLIC_CONST (x))
     (x) =  arc_legitimize_pic_address (x, 0);
  addr = x;
  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) == PLUS
      && CONST_INT_P (XEXP (addr, 1))
      && ((GET_CODE (XEXP (addr, 0)) == SYMBOL_REF
	   && !SYMBOL_REF_FUNCTION_P (XEXP (addr, 0)))
	  || (REG_P (XEXP (addr, 0))
	      && (INTVAL (XEXP (addr, 1)) & 252))))
    {
      HOST_WIDE_INT offs, upper;
      int size = GET_MODE_SIZE (mode);

      offs = INTVAL (XEXP (addr, 1));
      upper = (offs + 256 * size) & ~511 * size;
      inner = plus_constant (Pmode, XEXP (addr, 0), upper);
#if 0 /* ??? this produces worse code for EEMBC idctrn01  */
      if (GET_CODE (x) == CONST)
	inner = gen_rtx_CONST (Pmode, inner);
#endif
      addr = plus_constant (Pmode, force_reg (Pmode, inner), offs - upper);
      x = addr;
    }
  else if (GET_CODE (addr) == SYMBOL_REF && !SYMBOL_REF_FUNCTION_P (addr))
    x = force_reg (Pmode, x);
  if (memory_address_p ((machine_mode) mode, x))
     return x;
  return NULL_RTX;
}

static rtx
arc_legitimize_address (rtx orig_x, rtx oldx, machine_mode mode)
{
  if (GET_CODE (orig_x) == SYMBOL_REF)
    {
      enum tls_model model = SYMBOL_REF_TLS_MODEL (orig_x);
      if (model != 0)
	return arc_legitimize_tls_address (orig_x, model);
    }

  rtx new_x = arc_legitimize_address_0 (orig_x, oldx, mode);

  if (new_x)
    return new_x;
  return orig_x;
}

static rtx
arc_delegitimize_address_0 (rtx x)
{
  rtx u, gp, p;

  if (GET_CODE (x) == CONST && GET_CODE (u = XEXP (x, 0)) == UNSPEC)
    {
      if (XINT (u, 1) == ARC_UNSPEC_GOT
	  || XINT (u, 1) == ARC_UNSPEC_GOTOFFPC)
	return XVECEXP (u, 0, 0);
    }
  else if (GET_CODE (x) == CONST && GET_CODE (p = XEXP (x, 0)) == PLUS
	   && GET_CODE (u = XEXP (p, 0)) == UNSPEC
	   && (XINT (u, 1) == ARC_UNSPEC_GOT
	       || XINT (u, 1) == ARC_UNSPEC_GOTOFFPC))
    return gen_rtx_CONST
	    (GET_MODE (x),
	     gen_rtx_PLUS (GET_MODE (p), XVECEXP (u, 0, 0), XEXP (p, 1)));
  else if (GET_CODE (x) == PLUS
	   && ((REG_P (gp = XEXP (x, 0))
		&& REGNO (gp) == PIC_OFFSET_TABLE_REGNUM)
	       || (GET_CODE (gp) == CONST
		   && GET_CODE (u = XEXP (gp, 0)) == UNSPEC
		   && XINT (u, 1) == ARC_UNSPEC_GOT
		   && GET_CODE (XVECEXP (u, 0, 0)) == SYMBOL_REF
		   && !strcmp (XSTR (XVECEXP (u, 0, 0), 0), "_DYNAMIC")))
	   && GET_CODE (XEXP (x, 1)) == CONST
	   && GET_CODE (u = XEXP (XEXP (x, 1), 0)) == UNSPEC
	   && XINT (u, 1) == ARC_UNSPEC_GOTOFF)
    return XVECEXP (u, 0, 0);
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
	   && ((REG_P (gp = XEXP (XEXP (x, 0), 1))
		&& REGNO (gp) == PIC_OFFSET_TABLE_REGNUM)
	       || (GET_CODE (gp) == CONST
		   && GET_CODE (u = XEXP (gp, 0)) == UNSPEC
		   && XINT (u, 1) == ARC_UNSPEC_GOT
		   && GET_CODE (XVECEXP (u, 0, 0)) == SYMBOL_REF
		   && !strcmp (XSTR (XVECEXP (u, 0, 0), 0), "_DYNAMIC")))
	   && GET_CODE (XEXP (x, 1)) == CONST
	   && GET_CODE (u = XEXP (XEXP (x, 1), 0)) == UNSPEC
	   && XINT (u, 1) == ARC_UNSPEC_GOTOFF)
    return gen_rtx_PLUS (GET_MODE (x), XEXP (XEXP (x, 0), 0),
			 XVECEXP (u, 0, 0));
  else if (GET_CODE (x) == PLUS
	   && (u = arc_delegitimize_address_0 (XEXP (x, 1))))
    return gen_rtx_PLUS (GET_MODE (x), XEXP (x, 0), u);
  return NULL_RTX;
}

static rtx
arc_delegitimize_address (rtx x)
{
  rtx orig_x = x = delegitimize_mem_from_attrs (x);
  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);
  x = arc_delegitimize_address_0 (x);
  if (x)
    {
      if (MEM_P (orig_x))
	x = replace_equiv_address_nv (orig_x, x);
      return x;
    }
  return orig_x;
}

/* Return a REG rtx for acc1.  N.B. the gcc-internal representation may
   differ from the hardware register number in order to allow the generic
   code to correctly split the concatenation of acc1 and acc2.  */

rtx
gen_acc1 (void)
{
  return gen_rtx_REG (SImode, TARGET_BIG_ENDIAN ? 56: 57);
}

/* Return a REG rtx for acc2.  N.B. the gcc-internal representation may
   differ from the hardware register number in order to allow the generic
   code to correctly split the concatenation of acc1 and acc2.  */

rtx
gen_acc2 (void)
{
  return gen_rtx_REG (SImode, TARGET_BIG_ENDIAN ? 57: 56);
}

/* Return a REG rtx for mlo.  N.B. the gcc-internal representation may
   differ from the hardware register number in order to allow the generic
   code to correctly split the concatenation of mhi and mlo.  */

rtx
gen_mlo (void)
{
  return gen_rtx_REG (SImode, TARGET_BIG_ENDIAN ? 59: 58);
}

/* Return a REG rtx for mhi.  N.B. the gcc-internal representation may
   differ from the hardware register number in order to allow the generic
   code to correctly split the concatenation of mhi and mlo.  */

rtx
gen_mhi (void)
{
  return gen_rtx_REG (SImode, TARGET_BIG_ENDIAN ? 58: 59);
}

/* FIXME: a parameter should be added, and code added to final.c,
   to reproduce this functionality in shorten_branches.  */
#if 0
/* Return nonzero iff BRANCH should be unaligned if possible by upsizing
   a previous instruction.  */
int
arc_unalign_branch_p (rtx branch)
{
  rtx note;

  if (!TARGET_UNALIGN_BRANCH)
    return 0;
  /* Do not do this if we have a filled delay slot.  */
  if (get_attr_delay_slot_filled (branch) == DELAY_SLOT_FILLED_YES
      && !NEXT_INSN (branch)->deleted ())
    return 0;
  note = find_reg_note (branch, REG_BR_PROB, 0);
  return (!note
	  || (arc_unalign_prob_threshold && !br_prob_note_reliable_p (note))
	  || INTVAL (XEXP (note, 0)) < arc_unalign_prob_threshold);
}
#endif

/* When estimating sizes during arc_reorg, when optimizing for speed, there
   are three reasons why we need to consider branches to be length 6:
   - annull-false delay slot insns are implemented using conditional execution,
     thus preventing short insn formation where used.
   - for ARC600: annul-true delay slot insns are implemented where possible
     using conditional execution, preventing short insn formation where used.
   - for ARC700: likely or somewhat likely taken branches are made long and
     unaligned if possible to avoid branch penalty.  */

bool
arc_branch_size_unknown_p (void)
{
  return !optimize_size && arc_reorg_in_progress;
}

/* We are about to output a return insn.  Add padding if necessary to avoid
   a mispredict.  A return could happen immediately after the function
   start, but after a call we know that there will be at least a blink
   restore.  */

void
arc_pad_return (void)
{
  rtx_insn *insn = current_output_insn;
  rtx_insn *prev = prev_active_insn (insn);
  int want_long;

  if (!prev)
    {
      fputs ("\tnop_s\n", asm_out_file);
      cfun->machine->unalign ^= 2;
      want_long = 1;
    }
  /* If PREV is a sequence, we know it must be a branch / jump or a tailcall,
     because after a call, we'd have to restore blink first.  */
  else if (GET_CODE (PATTERN (prev)) == SEQUENCE)
    return;
  else
    {
      want_long = (get_attr_length (prev) == 2);
      prev = prev_active_insn (prev);
    }
  if (!prev
      || ((NONJUMP_INSN_P (prev) && GET_CODE (PATTERN (prev)) == SEQUENCE)
	  ? CALL_ATTR (as_a <rtx_sequence *> (PATTERN (prev))->insn (0),
		       NON_SIBCALL)
	  : CALL_ATTR (prev, NON_SIBCALL)))
    {
      if (want_long)
	cfun->machine->size_reason
	  = "call/return and return/return must be 6 bytes apart to avoid mispredict";
      else if (TARGET_UNALIGN_BRANCH && cfun->machine->unalign)
	{
	  cfun->machine->size_reason
	    = "Long unaligned jump avoids non-delay slot penalty";
	  want_long = 1;
	}
      /* Disgorge delay insn, if there is any, and it may be moved.  */
      if (final_sequence
	  /* ??? Annulled would be OK if we can and do conditionalize
	     the delay slot insn accordingly.  */
	  && !INSN_ANNULLED_BRANCH_P (insn)
	  && (get_attr_cond (insn) != COND_USE
	      || !reg_set_p (gen_rtx_REG (CCmode, CC_REG),
			     XVECEXP (final_sequence, 0, 1))))
	{
	  prev = as_a <rtx_insn *> (XVECEXP (final_sequence, 0, 1));
	  gcc_assert (!prev_real_insn (insn)
		      || !arc_hazard (prev_real_insn (insn), prev));
	  cfun->machine->force_short_suffix = !want_long;
	  rtx save_pred = current_insn_predicate;
	  final_scan_insn (prev, asm_out_file, optimize, 1, NULL);
	  cfun->machine->force_short_suffix = -1;
	  prev->set_deleted ();
	  current_output_insn = insn;
	  current_insn_predicate = save_pred;
	}
      else if (want_long)
	fputs ("\tnop\n", asm_out_file);
      else
	{
	  fputs ("\tnop_s\n", asm_out_file);
	  cfun->machine->unalign ^= 2;
	}
    }
  return;
}

/* The usual; we set up our machine_function data.  */

static struct machine_function *
arc_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  machine->fn_type = ARC_FUNCTION_UNKNOWN;
  machine->force_short_suffix = -1;

  return machine;
}

/* Implements INIT_EXPANDERS.  We just set up to call the above
   function.  */

void
arc_init_expanders (void)
{
  init_machine_status = arc_init_machine_status;
}

/* Check if OP is a proper parallel of a millicode call pattern.  OFFSET
   indicates a number of elements to ignore - that allows to have a
   sibcall pattern that starts with (return).  LOAD_P is zero for store
   multiple (for prologues), and one for load multiples (for epilogues),
   and two for load multiples where no final clobber of blink is required.
   We also skip the first load / store element since this is supposed to
   be checked in the instruction pattern.  */

int
arc_check_millicode (rtx op, int offset, int load_p)
{
  int len = XVECLEN (op, 0) - offset;
  int i;

  if (load_p == 2)
    {
      if (len < 2 || len > 13)
	return 0;
      load_p = 1;
    }
  else
    {
      rtx elt = XVECEXP (op, 0, --len);

      if (GET_CODE (elt) != CLOBBER
	  || !REG_P (XEXP (elt, 0))
	  || REGNO (XEXP (elt, 0)) != RETURN_ADDR_REGNUM
	  || len < 3 || len > 13)
	return 0;
    }
  for (i = 1; i < len; i++)
    {
      rtx elt = XVECEXP (op, 0, i + offset);
      rtx reg, mem, addr;

      if (GET_CODE (elt) != SET)
	return 0;
      mem = XEXP (elt, load_p);
      reg = XEXP (elt, 1-load_p);
      if (!REG_P (reg) || REGNO (reg) != 13U+i || !MEM_P (mem))
	return 0;
      addr = XEXP (mem, 0);
      if (GET_CODE (addr) != PLUS
	  || !rtx_equal_p (stack_pointer_rtx, XEXP (addr, 0))
	  || !CONST_INT_P (XEXP (addr, 1)) || INTVAL (XEXP (addr, 1)) != i*4)
	return 0;
    }
  return 1;
}

/* Accessor functions for cfun->machine->unalign.  */

int
arc_get_unalign (void)
{
  return cfun->machine->unalign;
}

void
arc_clear_unalign (void)
{
  if (cfun)
    cfun->machine->unalign = 0;
}

void
arc_toggle_unalign (void)
{
  cfun->machine->unalign ^= 2;
}

/* Operands 0..2 are the operands of a addsi which uses a 12 bit
   constant in operand 2, but which would require a LIMM because of
   operand mismatch.
   operands 3 and 4 are new SET_SRCs for operands 0.  */

void
split_addsi (rtx *operands)
{
  int val = INTVAL (operands[2]);

  /* Try for two short insns first.  Lengths being equal, we prefer
     expansions with shorter register lifetimes.  */
  if (val > 127 && val <= 255
      && satisfies_constraint_Rcq (operands[0]))
    {
      operands[3] = operands[2];
      operands[4] = gen_rtx_PLUS (SImode, operands[0], operands[1]);
    }
  else
    {
      operands[3] = operands[1];
      operands[4] = gen_rtx_PLUS (SImode, operands[0], operands[2]);
    }
}

/* Operands 0..2 are the operands of a subsi which uses a 12 bit
   constant in operand 1, but which would require a LIMM because of
   operand mismatch.
   operands 3 and 4 are new SET_SRCs for operands 0.  */

void
split_subsi (rtx *operands)
{
  int val = INTVAL (operands[1]);

  /* Try for two short insns first.  Lengths being equal, we prefer
     expansions with shorter register lifetimes.  */
  if (satisfies_constraint_Rcq (operands[0])
      && satisfies_constraint_Rcq (operands[2]))
    {
      if (val >= -31 && val <= 127)
	{
	  operands[3] = gen_rtx_NEG (SImode, operands[2]);
	  operands[4] = gen_rtx_PLUS (SImode, operands[0], operands[1]);
	  return;
	}
      else if (val >= 0 && val < 255)
	{
	  operands[3] = operands[1];
	  operands[4] = gen_rtx_MINUS (SImode, operands[0], operands[2]);
	  return;
	}
    }
  /* If the destination is not an ARCompact16 register, we might
     still have a chance to make a short insn if the source is;
      we need to start with a reg-reg move for this.  */
  operands[3] = operands[2];
  operands[4] = gen_rtx_MINUS (SImode, operands[1], operands[0]);
}

/* Handle DOUBLE_REGS uses.
   Operand 0: destination register
   Operand 1: source register  */

static bool
arc_process_double_reg_moves (rtx *operands)
{
  rtx dest = operands[0];
  rtx src  = operands[1];

  enum usesDxState { none, srcDx, destDx, maxDx };
  enum usesDxState state = none;

  if (refers_to_regno_p (40, 44, src, 0))
    state = srcDx;
  if (refers_to_regno_p (40, 44, dest, 0))
    {
      /* Via arc_register_move_cost, we should never see D,D moves.  */
      gcc_assert (state == none);
      state = destDx;
    }

  if (state == none)
    return false;

  if (state == srcDx)
    {
      /* Without the LR insn, we need to split this into a
	 sequence of insns which will use the DEXCLx and DADDHxy
	 insns to be able to read the Dx register in question.  */
      if (TARGET_DPFP_DISABLE_LRSR)
	{
	  /* gen *movdf_insn_nolrsr */
	  rtx set = gen_rtx_SET (dest, src);
	  rtx use1 = gen_rtx_USE (VOIDmode, const1_rtx);
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, use1)));
	}
      else
	{
	  /* When we have 'mov D, r' or 'mov D, D' then get the target
	     register pair for use with LR insn.  */
	  rtx destHigh = simplify_gen_subreg (SImode, dest, DFmode,
					     TARGET_BIG_ENDIAN ? 0 : 4);
	  rtx destLow  = simplify_gen_subreg (SImode, dest, DFmode,
					     TARGET_BIG_ENDIAN ? 4 : 0);

	  /* Produce the two LR insns to get the high and low parts.  */
	  emit_insn (gen_rtx_SET (destHigh,
				  gen_rtx_UNSPEC_VOLATILE (Pmode,
							   gen_rtvec (1, src),
				  VUNSPEC_ARC_LR_HIGH)));
	  emit_insn (gen_rtx_SET (destLow,
				  gen_rtx_UNSPEC_VOLATILE (Pmode,
							   gen_rtvec (1, src),
				  VUNSPEC_ARC_LR)));
	}
    }
  else if (state == destDx)
    {
      /* When we have 'mov r, D' or 'mov D, D' and we have access to the
	 LR insn get the target register pair.  */
      rtx srcHigh = simplify_gen_subreg (SImode, src, DFmode,
					TARGET_BIG_ENDIAN ? 0 : 4);
      rtx srcLow  = simplify_gen_subreg (SImode, src, DFmode,
					TARGET_BIG_ENDIAN ? 4 : 0);

      emit_insn (gen_dexcl_2op (dest, srcHigh, srcLow));
    }
  else
    gcc_unreachable ();

  return true;
}

/* operands 0..1 are the operands of a 64 bit move instruction.
   split it into two moves with operands 2/3 and 4/5.  */

void
arc_split_move (rtx *operands)
{
  machine_mode mode = GET_MODE (operands[0]);
  int i;
  int swap = 0;
  rtx xop[4];

  if (TARGET_DPFP)
  {
    if (arc_process_double_reg_moves (operands))
      return;
  }

  if (TARGET_LL64
      && ((memory_operand (operands[0], mode)
	   && even_register_operand (operands[1], mode))
	  || (memory_operand (operands[1], mode)
	      && even_register_operand (operands[0], mode))))
    {
      emit_move_insn (operands[0], operands[1]);
      return;
    }

  if (TARGET_PLUS_QMACW
      && GET_CODE (operands[1]) == CONST_VECTOR)
    {
      HOST_WIDE_INT intval0, intval1;
      if (GET_MODE (operands[1]) == V2SImode)
	{
	  intval0 = INTVAL (XVECEXP (operands[1], 0, 0));
	  intval1 = INTVAL (XVECEXP (operands[1], 0, 1));
	}
      else
	{
	  intval1  = INTVAL (XVECEXP (operands[1], 0, 3)) << 16;
	  intval1 |= INTVAL (XVECEXP (operands[1], 0, 2)) & 0xFFFF;
	  intval0  = INTVAL (XVECEXP (operands[1], 0, 1)) << 16;
	  intval0 |= INTVAL (XVECEXP (operands[1], 0, 0)) & 0xFFFF;
	}
      xop[0] = gen_rtx_REG (SImode, REGNO (operands[0]));
      xop[3] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
      xop[2] = GEN_INT (trunc_int_for_mode (intval0, SImode));
      xop[1] = GEN_INT (trunc_int_for_mode (intval1, SImode));
      emit_move_insn (xop[0], xop[2]);
      emit_move_insn (xop[3], xop[1]);
      return;
    }

  for (i = 0; i < 2; i++)
    {
      if (MEM_P (operands[i]) && auto_inc_p (XEXP (operands[i], 0)))
	{
	  rtx addr = XEXP (operands[i], 0);
	  rtx r, o;
	  enum rtx_code code;

	  gcc_assert (!reg_overlap_mentioned_p (operands[0], addr));
	  switch (GET_CODE (addr))
	    {
	    case PRE_DEC: o = GEN_INT (-8); goto pre_modify;
	    case PRE_INC: o = GEN_INT (8); goto pre_modify;
	    case PRE_MODIFY: o = XEXP (XEXP (addr, 1), 1);
	    pre_modify:
	      code = PRE_MODIFY;
	      break;
	    case POST_DEC: o = GEN_INT (-8); goto post_modify;
	    case POST_INC: o = GEN_INT (8); goto post_modify;
	    case POST_MODIFY: o = XEXP (XEXP (addr, 1), 1);
	    post_modify:
	      code = POST_MODIFY;
	      swap = 2;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  r = XEXP (addr, 0);
	  xop[0+i] = adjust_automodify_address_nv
		      (operands[i], SImode,
		       gen_rtx_fmt_ee (code, Pmode, r,
				       gen_rtx_PLUS (Pmode, r, o)),
		       0);
	  xop[2+i] = adjust_automodify_address_nv
		      (operands[i], SImode, plus_constant (Pmode, r, 4), 4);
	}
      else
	{
	  xop[0+i] = operand_subword (operands[i], 0, 0, mode);
	  xop[2+i] = operand_subword (operands[i], 1, 0, mode);
	}
    }
  if (reg_overlap_mentioned_p (xop[0], xop[3]))
    {
      swap = 2;
      gcc_assert (!reg_overlap_mentioned_p (xop[2], xop[1]));
    }

  emit_move_insn (xop[0 + swap], xop[1 + swap]);
  emit_move_insn (xop[2 - swap], xop[3 - swap]);

}

/* Select between the instruction output templates s_tmpl (for short INSNs)
   and l_tmpl (for long INSNs).  */

const char *
arc_short_long (rtx_insn *insn, const char *s_tmpl, const char *l_tmpl)
{
  int is_short = arc_verify_short (insn, cfun->machine->unalign, -1);

  extract_constrain_insn_cached (insn);
  return is_short ? s_tmpl : l_tmpl;
}

/* Searches X for any reference to REGNO, returning the rtx of the
   reference found if any.  Otherwise, returns NULL_RTX.  */

rtx
arc_regno_use_in (unsigned int regno, rtx x)
{
  const char *fmt;
  int i, j;
  rtx tem;

  if (REG_P (x) && refers_to_regno_p (regno, x))
    return x;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if ((tem = regno_use_in (regno, XEXP (x, i))))
	    return tem;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if ((tem = regno_use_in (regno , XVECEXP (x, i, j))))
	    return tem;
    }

  return NULL_RTX;
}

/* Return the integer value of the "type" attribute for INSN, or -1 if
   INSN can't have attributes.  */

int
arc_attr_type (rtx_insn *insn)
{
  if (NONJUMP_INSN_P (insn)
      ? (GET_CODE (PATTERN (insn)) == USE
	 || GET_CODE (PATTERN (insn)) == CLOBBER)
      : JUMP_P (insn)
      ? (GET_CODE (PATTERN (insn)) == ADDR_VEC
	 || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
      : !CALL_P (insn))
    return -1;
  return get_attr_type (insn);
}

/* Return true if insn sets the condition codes.  */

bool
arc_sets_cc_p (rtx_insn *insn)
{
  if (NONJUMP_INSN_P (insn))
    if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (PATTERN (insn)))
      insn = seq->insn (seq->len () - 1);
  return arc_attr_type (insn) == TYPE_COMPARE;
}

/* Return true if INSN is an instruction with a delay slot we may want
   to fill.  */

bool
arc_need_delay (rtx_insn *insn)
{
  rtx_insn *next;

  if (!flag_delayed_branch)
    return false;
  /* The return at the end of a function needs a delay slot.  */
  if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == USE
      && (!(next = next_active_insn (insn))
	  || ((!NONJUMP_INSN_P (next) || GET_CODE (PATTERN (next)) != SEQUENCE)
	      && arc_attr_type (next) == TYPE_RETURN))
      && (!TARGET_PAD_RETURN
	  || (prev_active_insn (insn)
	      && prev_active_insn (prev_active_insn (insn))
	      && prev_active_insn (prev_active_insn (prev_active_insn (insn))))))
    return true;
  if (NONJUMP_INSN_P (insn)
      ? (GET_CODE (PATTERN (insn)) == USE
	 || GET_CODE (PATTERN (insn)) == CLOBBER
	 || GET_CODE (PATTERN (insn)) == SEQUENCE)
      : JUMP_P (insn)
      ? (GET_CODE (PATTERN (insn)) == ADDR_VEC
	 || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
      : !CALL_P (insn))
    return false;
  return num_delay_slots (insn) != 0;
}

/* Return true if the scheduling pass(es) has/have already run,
   i.e. where possible, we should try to mitigate high latencies
   by different instruction selection.  */

bool
arc_scheduling_not_expected (void)
{
  return cfun->machine->arc_reorg_started;
}

int
arc_label_align (rtx_insn *label)
{
  /* Code has a minimum p2 alignment of 1, which we must restore after an
     ADDR_DIFF_VEC.  */
  if (align_labels_log < 1)
    {
      rtx_insn *next = next_nonnote_nondebug_insn (label);
      if (INSN_P (next) && recog_memoized (next) >= 0)
	return 1;
    }
  return align_labels_log;
}

/* Return true if LABEL is in executable code.  */

bool
arc_text_label (rtx_insn *label)
{
  rtx_insn *next;

  /* ??? We use deleted labels like they were still there, see
     gcc.c-torture/compile/20000326-2.c .  */
  gcc_assert (GET_CODE (label) == CODE_LABEL
	      || (GET_CODE (label) == NOTE
		  && NOTE_KIND (label) == NOTE_INSN_DELETED_LABEL));
  next = next_nonnote_insn (label);
  if (next)
    return (!JUMP_TABLE_DATA_P (next)
	    || GET_CODE (PATTERN (next)) != ADDR_VEC);
  else if (!PREV_INSN (label))
    /* ??? sometimes text labels get inserted very late, see
       gcc.dg/torture/stackalign/comp-goto-1.c */
    return true;
  return false;
}

/* Without this, gcc.dg/tree-prof/bb-reorg.c fails to assemble
  when compiling with -O2 -freorder-blocks-and-partition -fprofile-use
  -D_PROFILE_USE; delay branch scheduling then follows a crossing jump
  to redirect two breqs.  */

static bool
arc_can_follow_jump (const rtx_insn *follower, const rtx_insn *followee)
{
  /* ??? get_attr_type is declared to take an rtx.  */
  union { const rtx_insn *c; rtx_insn *r; } u;

  u.c = follower;
  if (CROSSING_JUMP_P (followee))
    switch (get_attr_type (u.r))
      {
      case TYPE_BRANCH:
	if (get_attr_length (u.r) != 2)
	  break;
      case TYPE_BRCC:
      case TYPE_BRCC_NO_DELAY_SLOT:
	return false;
      default:
	return true;
      }
  return true;
}

/* Return the register number of the register holding the return address
   for a function of type TYPE.  */

int
arc_return_address_register (unsigned int fn_type)
{
  int regno = 0;

  if (ARC_INTERRUPT_P (fn_type))
    {
      if (((fn_type & ARC_FUNCTION_ILINK1) | ARC_FUNCTION_FIRQ) != 0)
        regno = ILINK1_REGNUM;
      else if ((fn_type & ARC_FUNCTION_ILINK2) != 0)
        regno = ILINK2_REGNUM;
      else
        gcc_unreachable ();
    }
  else if (ARC_NORMAL_P (fn_type) || ARC_NAKED_P (fn_type))
    regno = RETURN_ADDR_REGNUM;

  gcc_assert (regno != 0);
  return regno;
}

/* Implement EPILOGUE_USES.
   Return true if REGNO should be added to the deemed uses of the epilogue.

   We have to make sure all the register restore instructions are
   known to be live in interrupt functions, plus the blink register if
   it is clobbered by the isr.  */

bool
arc_epilogue_uses (int regno)
{
  unsigned int fn_type;

  if (regno == arc_tp_regno)
    return true;

  fn_type = arc_compute_function_type (cfun);
  if (reload_completed)
    {
      if (ARC_INTERRUPT_P (cfun->machine->fn_type))
	{
	  if (!fixed_regs[regno])
	    return true;
	  return ((regno == arc_return_address_register (fn_type))
		  || (regno == RETURN_ADDR_REGNUM));
	}
      else
	return regno == RETURN_ADDR_REGNUM;
    }
  else
    return regno == arc_return_address_register (fn_type);
}

/* Helper for EH_USES macro.  */

bool
arc_eh_uses (int regno)
{
  if (regno == arc_tp_regno)
    return true;
  return false;
}

#ifndef TARGET_NO_LRA
#define TARGET_NO_LRA !TARGET_LRA
#endif

static bool
arc_lra_p (void)
{
  return !TARGET_NO_LRA;
}

/* ??? Should we define TARGET_REGISTER_PRIORITY?  We might perfer to use
   Rcq registers, because some insn are shorter with them.  OTOH we already
   have separate alternatives for this purpose, and other insns don't
   mind, so maybe we should rather prefer the other registers?
   We need more data, and we can only get that if we allow people to
   try all options.  */
static int
arc_register_priority (int r)
{
  switch (arc_lra_priority_tag)
    {
    case ARC_LRA_PRIORITY_NONE:
      return 0;
    case ARC_LRA_PRIORITY_NONCOMPACT:
      return ((((r & 7) ^ 4) - 4) & 15) != r;
    case ARC_LRA_PRIORITY_COMPACT:
      return ((((r & 7) ^ 4) - 4) & 15) == r;
    default:
      gcc_unreachable ();
    }
}

static reg_class_t
arc_spill_class (reg_class_t /* orig_class */, machine_mode)
{
  return GENERAL_REGS;
}

bool
arc_legitimize_reload_address (rtx *p, machine_mode mode, int opnum,
			       int itype)
{
  rtx x = *p;
  enum reload_type type = (enum reload_type) itype;

  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1))
      && (RTX_OK_FOR_BASE_P (XEXP (x, 0), true)
	  || (REG_P (XEXP (x, 0))
	      && reg_equiv_constant (REGNO (XEXP (x, 0))))))
    {
      int scale = GET_MODE_SIZE (mode);
      int shift;
      rtx index_rtx = XEXP (x, 1);
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;
      rtx reg, sum, sum2;

      if (scale > 4)
	scale = 4;
      if ((scale-1) & offset)
	scale = 1;
      shift = scale >> 1;
      offset_base
	= ((offset + (256 << shift))
	   & ((HOST_WIDE_INT)((unsigned HOST_WIDE_INT) -512 << shift)));
      /* Sometimes the normal form does not suit DImode.  We
	 could avoid that by using smaller ranges, but that
	 would give less optimized code when SImode is
	 prevalent.  */
      if (GET_MODE_SIZE (mode) + offset - offset_base <= (256 << shift))
	{
	  int regno;

	  reg = XEXP (x, 0);
	  regno = REGNO (reg);
	  sum2 = sum = plus_constant (Pmode, reg, offset_base);

	  if (reg_equiv_constant (regno))
	    {
	      sum2 = plus_constant (Pmode, reg_equiv_constant (regno),
				    offset_base);
	      if (GET_CODE (sum2) == PLUS)
		sum2 = gen_rtx_CONST (Pmode, sum2);
	    }
	  *p = gen_rtx_PLUS (Pmode, sum, GEN_INT (offset - offset_base));
	  push_reload (sum2, NULL_RTX, &XEXP (*p, 0), NULL,
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum,
		       type);
	  return true;
	}
    }
  /* We must re-recognize what we created before.  */
  else if (GET_CODE (x) == PLUS
	   && GET_CODE (XEXP (x, 0)) == PLUS
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && REG_P  (XEXP (XEXP (x, 0), 0))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      /* Because this address is so complex, we know it must have
	 been created by LEGITIMIZE_RELOAD_ADDRESS before; thus,
	 it is already unshared, and needs no further unsharing.  */
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, opnum, type);
      return true;
    }
  return false;
}

/* Implement TARGET_USE_BY_PIECES_INFRASTRUCTURE_P.  */

static bool
arc_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				    unsigned int align,
				    enum by_pieces_operation op,
				    bool speed_p)
{
  /* Let the movmem expander handle small block moves.  */
  if (op == MOVE_BY_PIECES)
    return false;

  return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);
}

/* Emit a (pre) memory barrier around an atomic sequence according to
   MODEL.  */

static void
arc_pre_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, true))
    emit_insn (gen_memory_barrier ());
}

/* Emit a (post) memory barrier around an atomic sequence according to
   MODEL.  */

static void
arc_post_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, false))
    emit_insn (gen_memory_barrier ());
}

/* Expand a compare and swap pattern.  */

static void
emit_unlikely_jump (rtx insn)
{
  rtx_insn *jump = emit_jump_insn (insn);
  add_reg_br_prob_note (jump, profile_probability::very_unlikely ());
}

/* Expand code to perform a 8 or 16-bit compare and swap by doing
   32-bit compare and swap on the word containing the byte or
   half-word.  The difference between a weak and a strong CAS is that
   the weak version may simply fail.  The strong version relies on two
   loops, one checks if the SCOND op is succsfully or not, the other
   checks if the 32 bit accessed location which contains the 8 or 16
   bit datum is not changed by other thread.  The first loop is
   implemented by the atomic_compare_and_swapsi_1 pattern.  The second
   loops is implemented by this routine.  */

static void
arc_expand_compare_and_swap_qh (rtx bool_result, rtx result, rtx mem,
				rtx oldval, rtx newval, rtx weak,
				rtx mod_s, rtx mod_f)
{
  rtx addr1 = force_reg (Pmode, XEXP (mem, 0));
  rtx addr = gen_reg_rtx (Pmode);
  rtx off = gen_reg_rtx (SImode);
  rtx oldv = gen_reg_rtx (SImode);
  rtx newv = gen_reg_rtx (SImode);
  rtx oldvalue = gen_reg_rtx (SImode);
  rtx newvalue = gen_reg_rtx (SImode);
  rtx res = gen_reg_rtx (SImode);
  rtx resv = gen_reg_rtx (SImode);
  rtx memsi, val, mask, end_label, loop_label, cc, x;
  machine_mode mode;
  bool is_weak = (weak != const0_rtx);

  /* Truncate the address.  */
  emit_insn (gen_rtx_SET (addr,
			  gen_rtx_AND (Pmode, addr1, GEN_INT (-4))));

  /* Compute the datum offset.  */
  emit_insn (gen_rtx_SET (off,
			  gen_rtx_AND (SImode, addr1, GEN_INT (3))));
  if (TARGET_BIG_ENDIAN)
    emit_insn (gen_rtx_SET (off,
			    gen_rtx_MINUS (SImode,
					   (GET_MODE (mem) == QImode) ?
					   GEN_INT (3) : GEN_INT (2), off)));

  /* Normal read from truncated address.  */
  memsi = gen_rtx_MEM (SImode, addr);
  set_mem_alias_set (memsi, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (memsi) = MEM_VOLATILE_P (mem);

  val = copy_to_reg (memsi);

  /* Convert the offset in bits.  */
  emit_insn (gen_rtx_SET (off,
			  gen_rtx_ASHIFT (SImode, off, GEN_INT (3))));

  /* Get the proper mask.  */
  if (GET_MODE (mem) == QImode)
    mask = force_reg (SImode, GEN_INT (0xff));
  else
    mask = force_reg (SImode, GEN_INT (0xffff));

  emit_insn (gen_rtx_SET (mask,
			  gen_rtx_ASHIFT (SImode, mask, off)));

  /* Prepare the old and new values.  */
  emit_insn (gen_rtx_SET (val,
			  gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
				       val)));

  oldval = gen_lowpart (SImode, oldval);
  emit_insn (gen_rtx_SET (oldv,
			  gen_rtx_ASHIFT (SImode, oldval, off)));

  newval = gen_lowpart_common (SImode, newval);
  emit_insn (gen_rtx_SET (newv,
			  gen_rtx_ASHIFT (SImode, newval, off)));

  emit_insn (gen_rtx_SET (oldv,
			  gen_rtx_AND (SImode, oldv, mask)));

  emit_insn (gen_rtx_SET (newv,
			  gen_rtx_AND (SImode, newv, mask)));

  if (!is_weak)
    {
      end_label = gen_label_rtx ();
      loop_label = gen_label_rtx ();
      emit_label (loop_label);
    }

  /* Make the old and new values.  */
  emit_insn (gen_rtx_SET (oldvalue,
			  gen_rtx_IOR (SImode, oldv, val)));

  emit_insn (gen_rtx_SET (newvalue,
			  gen_rtx_IOR (SImode, newv, val)));

  /* Try an 32bit atomic compare and swap.  It clobbers the CC
     register.  */
  emit_insn (gen_atomic_compare_and_swapsi_1 (res, memsi, oldvalue, newvalue,
					      weak, mod_s, mod_f));

  /* Regardless of the weakness of the operation, a proper boolean
     result needs to be provided.  */
  x = gen_rtx_REG (CC_Zmode, CC_REG);
  x = gen_rtx_EQ (SImode, x, const0_rtx);
  emit_insn (gen_rtx_SET (bool_result, x));

  if (!is_weak)
    {
      /* Check the results: if the atomic op is successfully the goto
	 to end label.  */
      x = gen_rtx_REG (CC_Zmode, CC_REG);
      x = gen_rtx_EQ (VOIDmode, x, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, end_label), pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, x));

      /* Wait for the right moment when the accessed 32-bit location
	 is stable.  */
      emit_insn (gen_rtx_SET (resv,
			      gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
					   res)));
      mode = SELECT_CC_MODE (NE, resv, val);
      cc = gen_rtx_REG (mode, CC_REG);
      emit_insn (gen_rtx_SET (cc, gen_rtx_COMPARE (mode, resv, val)));

      /* Set the new value of the 32 bit location, proper masked.  */
      emit_insn (gen_rtx_SET (val, resv));

      /* Try again if location is unstable.  Fall through if only
	 scond op failed.  */
      x = gen_rtx_NE (VOIDmode, cc, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, loop_label), pc_rtx);
      emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

      emit_label (end_label);
    }

  /* End: proper return the result for the given mode.  */
  emit_insn (gen_rtx_SET (res,
			  gen_rtx_AND (SImode, res, mask)));

  emit_insn (gen_rtx_SET (res,
			  gen_rtx_LSHIFTRT (SImode, res, off)));

  emit_move_insn (result, gen_lowpart (GET_MODE (result), res));
}

/* Helper function used by "atomic_compare_and_swap" expand
   pattern.  */

void
arc_expand_compare_and_swap (rtx operands[])
{
  rtx bval, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x;
  machine_mode mode;

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (rval, oldval))
    oldval = copy_to_reg (oldval);

  if (mode == SImode)
    {
      emit_insn (gen_atomic_compare_and_swapsi_1 (rval, mem, oldval, newval,
						  is_weak, mod_s, mod_f));
      x = gen_rtx_REG (CC_Zmode, CC_REG);
      x = gen_rtx_EQ (SImode, x, const0_rtx);
      emit_insn (gen_rtx_SET (bval, x));
    }
  else
    {
      arc_expand_compare_and_swap_qh (bval, rval, mem, oldval, newval,
				      is_weak, mod_s, mod_f);
    }
}

/* Helper function used by the "atomic_compare_and_swapsi_1"
   pattern.  */

void
arc_split_compare_and_swap (rtx operands[])
{
  rtx rval, mem, oldval, newval;
  machine_mode mode;
  enum memmodel mod_s, mod_f;
  bool is_weak;
  rtx label1, label2, x, cond;

  rval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  is_weak = (operands[4] != const0_rtx);
  mod_s = (enum memmodel) INTVAL (operands[5]);
  mod_f = (enum memmodel) INTVAL (operands[6]);
  mode = GET_MODE (mem);

  /* ARC atomic ops work only with 32-bit aligned memories.  */
  gcc_assert (mode == SImode);

  arc_pre_atomic_barrier (mod_s);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  /* Load exclusive.  */
  emit_insn (gen_arc_load_exclusivesi (rval, mem));

  /* Check if it is oldval.  */
  mode = SELECT_CC_MODE (NE, rval, oldval);
  cond = gen_rtx_REG (mode, CC_REG);
  emit_insn (gen_rtx_SET (cond, gen_rtx_COMPARE (mode, rval, oldval)));

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  /* Exclusively store new item.  Store clobbers CC reg.  */
  emit_insn (gen_arc_store_exclusivesi (mem, newval));

  if (!is_weak)
    {
      /* Check the result of the store.  */
      cond = gen_rtx_REG (CC_Zmode, CC_REG);
      x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, label1), pc_rtx);
      emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
    }

  if (mod_f != MEMMODEL_RELAXED)
    emit_label (label2);

  arc_post_atomic_barrier (mod_s);

  if (mod_f == MEMMODEL_RELAXED)
    emit_label (label2);
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform.  MEM is the memory on which to operate.  VAL is the second
   operand of the binary operator.  BEFORE and AFTER are optional locations to
   return the value of MEM either before of after the operation.  MODEL_RTX
   is a CONST_INT containing the memory model to use.  */

void
arc_expand_atomic_op (enum rtx_code code, rtx mem, rtx val,
			 rtx orig_before, rtx orig_after, rtx model_rtx)
{
  enum memmodel model = (enum memmodel) INTVAL (model_rtx);
  machine_mode mode = GET_MODE (mem);
  rtx label, x, cond;
  rtx before = orig_before, after = orig_after;

  /* ARC atomic ops work only with 32-bit aligned memories.  */
  gcc_assert (mode == SImode);

  arc_pre_atomic_barrier (model);

  label = gen_label_rtx ();
  emit_label (label);
  label = gen_rtx_LABEL_REF (VOIDmode, label);

  if (before == NULL_RTX)
    before = gen_reg_rtx (mode);

  if (after == NULL_RTX)
    after = gen_reg_rtx (mode);

  /* Load exclusive.  */
  emit_insn (gen_arc_load_exclusivesi (before, mem));

  switch (code)
    {
    case NOT:
      x = gen_rtx_AND (mode, before, val);
      emit_insn (gen_rtx_SET (after, x));
      x = gen_rtx_NOT (mode, after);
      emit_insn (gen_rtx_SET (after, x));
      break;

    case MINUS:
      if (CONST_INT_P (val))
	{
	  val = GEN_INT (-INTVAL (val));
	  code = PLUS;
	}

      /* FALLTHRU.  */
    default:
      x = gen_rtx_fmt_ee (code, mode, before, val);
      emit_insn (gen_rtx_SET (after, x));
      break;
   }

  /* Exclusively store new item.  Store clobbers CC reg.  */
  emit_insn (gen_arc_store_exclusivesi (mem, after));

  /* Check the result of the store.  */
  cond = gen_rtx_REG (CC_Zmode, CC_REG);
  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    label, pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  arc_post_atomic_barrier (model);
}

/* Implement TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P.  */

static bool
arc_no_speculation_in_delay_slots_p ()
{
  return true;
}

/* Return a parallel of registers to represent where to find the
   register pieces if required, otherwise NULL_RTX.  */

static rtx
arc_dwarf_register_span (rtx rtl)
{
   machine_mode mode = GET_MODE (rtl);
   unsigned regno;
   rtx p;

   if (GET_MODE_SIZE (mode) != 8)
     return NULL_RTX;

   p = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (2));
   regno = REGNO (rtl);
   XVECEXP (p, 0, 0) = gen_rtx_REG (SImode, regno);
   XVECEXP (p, 0, 1) = gen_rtx_REG (SImode, regno + 1);

   return p;
}

/* Return true if OP is an acceptable memory operand for ARCompact
   16-bit load instructions of MODE.

   AV2SHORT: TRUE if address needs to fit into the new ARCv2 short
   non scaled instructions.

   SCALED: TRUE if address can be scaled.  */

bool
compact_memory_operand_p (rtx op, machine_mode mode,
			  bool av2short, bool scaled)
{
  rtx addr, plus0, plus1;
  int size, off;

  /* Eliminate non-memory operations.  */
  if (GET_CODE (op) != MEM)
    return 0;

  /* .di instructions have no 16-bit form.  */
  if (MEM_VOLATILE_P (op) && !TARGET_VOLATILE_CACHE_SET)
    return false;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  size = GET_MODE_SIZE (mode);

  /* dword operations really put out 2 instructions, so eliminate
     them.  */
  if (size > UNITS_PER_WORD)
    return false;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
      return (REGNO (addr) >= FIRST_PSEUDO_REGISTER
	      || COMPACT_GP_REG_P (REGNO (addr))
	      || (SP_REG_P (REGNO (addr)) && (size != 2)));
    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);

      if ((GET_CODE (plus0) == REG)
	  && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
	      || COMPACT_GP_REG_P (REGNO (plus0)))
	  && ((GET_CODE (plus1) == REG)
	      && ((REGNO (plus1) >= FIRST_PSEUDO_REGISTER)
		  || COMPACT_GP_REG_P (REGNO (plus1)))))
	{
	  return !av2short;
	}

      if ((GET_CODE (plus0) == REG)
	  && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
	      || (COMPACT_GP_REG_P (REGNO (plus0)) && !av2short)
	      || (IN_RANGE (REGNO (plus0), 0, 31) && av2short))
	  && (GET_CODE (plus1) == CONST_INT))
	{
	  bool valid = false;

	  off = INTVAL (plus1);

	  /* Negative offset is not supported in 16-bit load/store insns.  */
	  if (off < 0)
	    return 0;

	  /* Only u5 immediates allowed in code density instructions.  */
	  if (av2short)
	    {
	      switch (size)
		{
		case 1:
		  return false;
		case 2:
		  /* This is an ldh_s.x instruction, check the u6
		     immediate.  */
		  if (COMPACT_GP_REG_P (REGNO (plus0)))
		    valid = true;
		  break;
		case 4:
		  /* Only u5 immediates allowed in 32bit access code
		     density instructions.  */
		  if (REGNO (plus0) <= 31)
		    return ((off < 32) && (off % 4 == 0));
		  break;
		default:
		  return false;
		}
	    }
	  else
	    if (COMPACT_GP_REG_P (REGNO (plus0)))
	      valid = true;

	  if (valid)
	    {

	      switch (size)
		{
		case 1:
		  return (off < 32);
		case 2:
		  /* The 6-bit constant get shifted to fit the real
		     5-bits field.  Check also for the alignment.  */
		  return ((off < 64) && (off % 2 == 0));
		case 4:
		  return ((off < 128) && (off % 4 == 0));
		default:
		  return false;
		}
	    }
	}

      if (REG_P (plus0) && CONST_INT_P (plus1)
	  && ((REGNO (plus0) >= FIRST_PSEUDO_REGISTER)
	      || SP_REG_P (REGNO (plus0)))
	  && !av2short)
	{
	  off = INTVAL (plus1);
	  return ((size != 2) && (off >= 0 && off < 128) && (off % 4 == 0));
	}

      if ((GET_CODE (plus0) == MULT)
	  && (GET_CODE (XEXP (plus0, 0)) == REG)
	  && ((REGNO (XEXP (plus0, 0)) >= FIRST_PSEUDO_REGISTER)
	      || COMPACT_GP_REG_P (REGNO (XEXP (plus0, 0))))
	  && (GET_CODE (plus1) == REG)
	  && ((REGNO (plus1) >= FIRST_PSEUDO_REGISTER)
	      || COMPACT_GP_REG_P (REGNO (plus1))))
	return scaled;
    default:
      break ;
      /* TODO: 'gp' and 'pcl' are to supported as base address operand
	 for 16-bit load instructions.  */
    }
  return false;
}

/* Implement TARGET_USE_ANCHORS_FOR_SYMBOL_P.  We don't want to use
   anchors for small data: the GP register acts as an anchor in that
   case.  We also don't want to use them for PC-relative accesses,
   where the PC acts as an anchor.  Prohibit also TLS symbols to use
   anchors.  */

static bool
arc_use_anchors_for_symbol_p (const_rtx symbol)
{
  if (SYMBOL_REF_TLS_MODEL (symbol))
    return false;

  if (flag_pic)
    return false;

  if (SYMBOL_REF_SMALL_P (symbol))
    return false;

  return default_use_anchors_for_symbol_p (symbol);
}

#undef TARGET_USE_ANCHORS_FOR_SYMBOL_P
#define TARGET_USE_ANCHORS_FOR_SYMBOL_P arc_use_anchors_for_symbol_p

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-arc.h"
