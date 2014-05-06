/* Subroutines used for code generation on picoChip processors.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Picochip Ltd. (http://www.picochip.com)
   Maintained by Daniel Towner (daniel.towner@picochip.com) and
   Hariharan Sandanagobalane (hariharan@picochip.com)

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
along with GCC; see the file COPYING3.  If not, see
<http://www.gnu.org/licenses/>. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "calls.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "varasm.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "ggc.h"
#include "hashtab.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "reload.h"
#include "params.h"

#include "picochip-protos.h"

#include "insn-attr.h"		/* For DFA state_t. */
#include "insn-config.h"	/* Required by recog.h */
#include "insn-codes.h"		/* For CODE_FOR_? */
#include "optabs.h"		/* For GEN_FCN */
#include "basic-block.h"	/* UPDATE_LIFE_GLOBAL* for picochip_reorg. */
#include "timevar.h"		/* For TV_SCHED2, in picochip_reorg. */
#include "libfuncs.h"		/* For memcpy_libfuncs, etc. */
#include "df.h"			/* For df_regs_ever_live_df_regs_ever_live_pp, etc. */
#include "dbxout.h"


/* Target AE ISA information. */
enum picochip_dfa_type picochip_schedule_type;

bool picochip_has_mul_unit = false;
bool picochip_has_mac_unit = false;

/* targetm hook function prototypes. */

void picochip_asm_file_start (void);
void picochip_asm_file_end (void);

void picochip_init_libfuncs (void);
void picochip_reorg (void);

int picochip_arg_partial_bytes (cumulative_args_t p_cum,
				       enum machine_mode mode,
				       tree type, bool named);
rtx picochip_function_arg (cumulative_args_t p_cum,
			   enum machine_mode mode,
			   const_tree type, bool named);
rtx picochip_incoming_function_arg (cumulative_args_t p_cum,
				    enum machine_mode mode,
				    const_tree type, bool named);
void picochip_arg_advance (cumulative_args_t p_cum, enum machine_mode mode,
			   const_tree type, bool named);
unsigned int picochip_function_arg_boundary (enum machine_mode mode,
					     const_tree type);

int picochip_sched_lookahead (void);
int picochip_sched_issue_rate (void);
int picochip_sched_adjust_cost (rtx insn, rtx link,
				       rtx dep_insn, int cost);
int picochip_sched_reorder (FILE * file, int verbose, rtx * ready,
				   int *n_readyp, int clock);

void picochip_init_builtins (void);
rtx picochip_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

bool picochip_rtx_costs (rtx x, int code, int outer_code, int opno,
			 int* total, bool speed);
bool picochip_return_in_memory(const_tree type,
                              const_tree fntype ATTRIBUTE_UNUSED);
bool picochip_legitimate_address_p (enum machine_mode, rtx, bool);
rtx picochip_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
                             enum machine_mode mode);
int picochip_legitimize_reload_address (rtx *x, enum machine_mode mode,
                                        int opnum, int type, int ind_levels);

rtx picochip_struct_value_rtx(tree fntype ATTRIBUTE_UNUSED, int incoming ATTRIBUTE_UNUSED);
rtx picochip_function_value (const_tree valtype, const_tree func ATTRIBUTE_UNUSED,
                         bool outgoing ATTRIBUTE_UNUSED);
static reg_class_t
picochip_secondary_reload (bool in_p,
			   rtx x ATTRIBUTE_UNUSED,
			   reg_class_t cla ATTRIBUTE_UNUSED,
			   enum machine_mode mode,
			   secondary_reload_info *sri);
void
picochip_asm_named_section (const char *name,
			    unsigned int flags ATTRIBUTE_UNUSED,
			    tree decl ATTRIBUTE_UNUSED);

static rtx picochip_static_chain (const_tree, bool);

static void picochip_option_override (void);

/* Lookup table mapping a register number to the earliest containing
   class.  Used by REGNO_REG_CLASS.  */
const enum reg_class picochip_regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  TWIN_REGS, TWIN_REGS, TWIN_REGS, TWIN_REGS,
  TWIN_REGS, TWIN_REGS, TWIN_REGS, TWIN_REGS,
  TWIN_REGS, TWIN_REGS, TWIN_REGS, TWIN_REGS,
  GR_REGS, FRAME_REGS, PTR_REGS, CONST_REGS,
  ACC_REGS, CC_REGS, GR_REGS, GR_REGS
};

/* picoChip register names. */
const char *picochip_regnames[] = REGISTER_NAMES;

/* Define the maximum number of registers which may be used to pass
 * parameters to functions. */
#define MAX_CALL_PARAMETER_REGS 6


/* Target scheduling information. */

/* This flag indicates whether the next instruction to be output is a
   VLIW continuation instruction.  It is used to communicate between
   final_prescan_insn and asm_output_opcode. */
static int picochip_vliw_continuation = 0;

/* This variable is used to communicate the current instruction
   between final_prescan_insn and functions such as asm_output_opcode,
   and picochip_get_vliw_alu_id (which are otherwise unable to determine the
   current instruction. */
static rtx picochip_current_prescan_insn;

static bool picochip_is_delay_slot_pending = 0;

/* When final_prescan_insn is called, it computes information about
   the current VLIW packet, and stores it in this structure. When
   instructions are output, this state is used to make sure that the
   instructions are output in the correct way (e.g., which ALU to use,
   whether a macro branch was ever previously a real branch, etc.). */
struct vliw_state
{
  int contains_pico_alu_insn;
  int contains_non_cc_alu_insn;
  int num_alu_insns_so_far;

  /* Record how many instructions are contained in the packet. */
  int num_insns_in_packet;

  /* There was a case for this to be more than 1 */
  int num_cfi_labels_deferred;
  char cfi_label_name[2][256];	/* Used to record the name of a CFI label
				   emitted inside a VLIW packet. */
  char lm_label_name[256];	/* Used to record the name of an LM label. */
};

struct vliw_state picochip_current_vliw_state;

/* Save/restore recog_data. */
static int picochip_saved_which_alternative;
static struct recog_data_d picochip_saved_recog_data;

/* Determine which ALU to use for the instruction in
   picochip_current_prescan_insn. */
static char picochip_get_vliw_alu_id (void);

/* Initialize the GCC target structure.  */

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE picochip_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE picochip_function_epilogue

#undef TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL picochip_output_internal_label

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL picochip_output_global

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP ".initByte "
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP  ".initWord "
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP  ".unalignedInitWord "
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP ".initLong "
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP ".unalignedInitLong "

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS picochip_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN picochip_expand_builtin

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS picochip_rtx_costs

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE picochip_sched_issue_rate

#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER picochip_sched_reorder

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  picochip_sched_lookahead

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST picochip_sched_adjust_cost

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION picochip_asm_named_section

#undef TARGET_HAVE_SWITCHABLE_BSS_SECTIONS
#define TARGET_HAVE_SWITCHABLE_BSS_SECTIONS 1

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS picochip_init_libfuncs

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START picochip_asm_file_start

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END picochip_asm_file_end

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG picochip_reorg

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES picochip_arg_partial_bytes

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG picochip_function_arg

#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG picochip_incoming_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE picochip_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY picochip_function_arg_boundary

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

/* Target support for Anchored Addresses optimization */
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET 0
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 7
#undef TARGET_ASM_OUTPUT_ANCHOR
#define TARGET_ASM_OUTPUT_ANCHOR  picochip_asm_output_anchor

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE picochip_function_value
/*
#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE picochip_libgcc_cmp_return_mode
*/

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P picochip_legitimate_address_p

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS picochip_legitimize_address

/* Loading and storing QImode values to and from memory
   usually requires a scratch register. */
#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD picochip_secondary_reload

/* How Large Values are Returned  */

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY picochip_return_in_memory

#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN picochip_static_chain

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE picochip_option_override

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE picochip_option_override

/* The 2nd scheduling pass option is switched off, and a machine
   dependent reorganisation ensures that it is run later on, after the
   second jump optimisation.  */
#undef TARGET_DELAY_SCHED2
#define TARGET_DELAY_SCHED2 true

/* Variable tracking should be run after all optimizations which
   change order of insns.  It also needs a valid CFG.  */
#undef TARGET_DELAY_VARTRACK
#define TARGET_DELAY_VARTRACK true

struct gcc_target targetm = TARGET_INITIALIZER;


/* Only return a value in memory if it is greater than 4 bytes.
   int_size_in_bytes returns -1 for variable size objects, which go in
   memory always.  The cast to unsigned makes -1 > 8.  */

bool
picochip_return_in_memory(const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  return ((unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 4);
}

/* Allow some options to be overriden. */

static void
picochip_option_override (void)
{
  /* If we are optimizing for stack, dont let inliner to inline functions
     that could potentially increase stack size.*/
   if (flag_conserve_stack)
     {
       maybe_set_param_value (PARAM_LARGE_STACK_FRAME, 0,
			      global_options.x_param_values,
			      global_options_set.x_param_values);
       maybe_set_param_value (PARAM_STACK_FRAME_GROWTH, 0,
			      global_options.x_param_values,
			      global_options_set.x_param_values);
     }

  /* Turn off the elimination of unused types. The elaborator
     generates various interesting types to represent constants,
     generics, and so on, and it is useful to retain this information
     in the debug output. The increased size of the debug information
     is not really an issue for us. */
  flag_eliminate_unused_debug_types = 0;

  /* Even if the user specifies a -fno-omit-frame-pointer on the
     command line, we still want to go ahead and omit frame pointer
     usages, since we dont really have a frame pointer register.
     So, all accesses to FP need to be converted to accesses off
     stack pointer.*/
  flag_omit_frame_pointer = 1;

  /* Turning on anchored addresses by default. This is an optimization
     that could decrease the code size by placing anchors in data and
     accessing offsets from the anchor for file local data variables.*/
  if (optimize >= 1)
    flag_section_anchors = 1;

  /* The second scheduling pass runs within picochip_reorg, to avoid
     having the second jump optimisation trash the instruction modes
     (e.g., instructions are changed to TImode to mark the beginning
     of cycles).  Two types of DFA scheduling are possible: space and
     speed.  In both cases, instructions are reordered to avoid stalls
     (e.g., memory loads stall for one cycle).  Speed scheduling will
     also enable VLIW instruction packing.  VLIW instructions use more
     code space, so VLIW scheduling is disabled when scheduling for
     size.  */
  if (flag_schedule_insns_after_reload)
    {
      if (optimize_size)
	picochip_schedule_type = DFA_TYPE_SPACE;
      else
	{
	  picochip_schedule_type = DFA_TYPE_SPEED;
	  flag_delayed_branch = 0;
	}
    }
  else
    picochip_schedule_type = DFA_TYPE_NONE;

  /* Ensure that the debug level is always at least -g2. The flow
     analyser works at its best if it always has debug
     information. DWARF is non-intrusive, so it makes no difference to
     code quality if debug is always enabled. */
  if (debug_info_level < DINFO_LEVEL_NORMAL)
  {
    debug_info_level = DINFO_LEVEL_NORMAL;
    write_symbols = DWARF2_DEBUG;
  }

  /* Options of the form -mae=mac, and so on will be substituted by
     the compiler driver for the appropriate byte access and multiply
     unit ISA options. Any unrecognised AE types will end up being
     passed to the compiler, which should reject them as invalid. */
  if (picochip_ae_type_string != NULL)
    error ("invalid AE type specified (%s)", picochip_ae_type_string);

  /* Override any specific capabilities of the instruction set. These
     take precedence over any capabilities inferred from the AE type,
     regardless of where the options appear on the command line. */
  if (picochip_mul_type_string == NULL)
    {
      /* Default to MEM-type multiply, for historical compatibility. */
      picochip_has_mac_unit = false;
      picochip_has_mul_unit = true;
    }
  else
    {
      picochip_has_mac_unit = false;
      picochip_has_mul_unit = false;

      if (strcmp (picochip_mul_type_string, "mul") == 0)
	picochip_has_mul_unit = true;
      else if (strcmp (picochip_mul_type_string, "mac") == 0)
	picochip_has_mac_unit = true;
      else if (strcmp (picochip_mul_type_string, "none") == 0)
	{ /* Do nothing. Unit types already set to false. */ }
      else
	error ("invalid mul type specified (%s) - expected mac, mul or none",
	       picochip_mul_type_string);
    }
}


/* Initialise the library functions to handle arithmetic on some of
   the larger modes. */
void
picochip_init_libfuncs (void)
{
  /* 64-bit shifts */
  set_optab_libfunc (ashr_optab, DImode, "__ashrdi3");
  set_optab_libfunc (ashl_optab, DImode, "__ashldi3");
  set_optab_libfunc (lshr_optab, DImode, "__lshrdi3");

  /* 64-bit signed multiplication. */
  set_optab_libfunc (smul_optab, DImode, "__muldi3");

  /* Signed division */
  set_optab_libfunc (sdiv_optab, HImode, "__divhi3");
  set_optab_libfunc (sdiv_optab, DImode, "__divdi3");

  /* Signed modulus */
  set_optab_libfunc (smod_optab, HImode, "__modhi3");
  set_optab_libfunc (smod_optab, DImode, "__moddi3");

  /* 32-bit count leading Zeros*/
  set_optab_libfunc (clz_optab, SImode, "_clzsi2");

  /* 64-bit comparison */
  set_optab_libfunc (ucmp_optab, DImode, "__ucmpdi2");
  set_optab_libfunc (cmp_optab, DImode, "__cmpdi2");

  /* 64-bit addition and subtraction*/
  set_optab_libfunc (add_optab, DImode, "_adddi3");
  set_optab_libfunc (sub_optab, DImode, "_subdi3");
}

/* Memcpy function */
int
picochip_expand_movmemhi (rtx *operands)
{
  rtx src_addr_reg, dst_addr_reg, count_reg, src_mem, dst_mem, tmp_reg;
  rtx start_label;
  int align, size;
  src_addr_reg = gen_reg_rtx(HImode);
  dst_addr_reg = gen_reg_rtx(HImode);
  count_reg = gen_reg_rtx(HImode);
  emit_insn (gen_movhi (count_reg, operands[2]));
  emit_insn (gen_movqi (src_addr_reg, XEXP(operands[1], 0)));
  emit_insn (gen_movqi (dst_addr_reg, XEXP(operands[0], 0)));
  gcc_assert (GET_CODE(count_reg) == REG);
  start_label = gen_label_rtx ();
  emit_label (start_label);

  /* We can specialise the code for different alignments */
  align = INTVAL(operands[3]);
  size = INTVAL(operands[2]);
  gcc_assert(align >= 0 && size >= 0);
  if (size != 0)
    {
      if (size % 4 == 0 && align % 4 == 0)
        {
          src_mem = gen_rtx_MEM(SImode, src_addr_reg);
          dst_mem = gen_rtx_MEM(SImode, dst_addr_reg);
          tmp_reg = gen_reg_rtx(SImode);
          emit_insn (gen_movsi (tmp_reg, src_mem));
          emit_insn (gen_movsi (dst_mem, tmp_reg));
          emit_insn (gen_addhi3 (dst_addr_reg, dst_addr_reg, GEN_INT(4)));
          emit_insn (gen_addhi3 (src_addr_reg, src_addr_reg, GEN_INT(4)));
          emit_insn (gen_addhi3 (count_reg, count_reg, GEN_INT(-4)));
          /* The sub instruction above generates cc, but we cannot just emit the branch.*/
          emit_cmp_and_jump_insns (count_reg, const0_rtx, GT, 0, HImode, 0, start_label);
        }
      else if (size % 2 == 0 && align % 2 == 0)
        {
          src_mem = gen_rtx_MEM(HImode, src_addr_reg);
          dst_mem = gen_rtx_MEM(HImode, dst_addr_reg);
          tmp_reg = gen_reg_rtx(HImode);
          emit_insn (gen_movhi (tmp_reg, src_mem));
          emit_insn (gen_movhi (dst_mem, tmp_reg));
          emit_insn (gen_addhi3 (dst_addr_reg, dst_addr_reg, const2_rtx));
          emit_insn (gen_addhi3 (src_addr_reg, src_addr_reg, const2_rtx));
          emit_insn (gen_addhi3 (count_reg, count_reg, GEN_INT(-2)));
          /* The sub instruction above generates cc, but we cannot just emit the branch.*/
          emit_cmp_and_jump_insns (count_reg, const0_rtx, GT, 0, HImode, 0, start_label);
        }
      else
        {
          src_mem = gen_rtx_MEM(QImode, src_addr_reg);
          dst_mem = gen_rtx_MEM(QImode, dst_addr_reg);
          tmp_reg = gen_reg_rtx(QImode);
          emit_insn (gen_movqi (tmp_reg, src_mem));
          emit_insn (gen_movqi (dst_mem, tmp_reg));
          emit_insn (gen_addhi3 (dst_addr_reg, dst_addr_reg, const1_rtx));
          emit_insn (gen_addhi3 (src_addr_reg, src_addr_reg, const1_rtx));
          emit_insn (gen_addhi3 (count_reg, count_reg, GEN_INT(-1)));
          /* The sub instruction above generates cc, but we cannot just emit the branch.*/
          emit_cmp_and_jump_insns (count_reg, const0_rtx, GT, 0, HImode, 0, start_label);
        }
    }
  return 1;
}


/* Return the register class for letter C.  */
enum reg_class
picochip_reg_class_from_letter (unsigned c)
{
  switch (c)
    {
    case 'k':
      return FRAME_REGS;
    case 'f':
      return PTR_REGS;
    case 't':
      return TWIN_REGS;
    case 'r':
      return GR_REGS;
    default:
      return NO_REGS;
    }
}

static const int
pico_leaf_reg_alloc_order[] = LEAF_REG_ALLOC_ORDER;
static const int
pico_nonleaf_reg_alloc_order[] = REG_ALLOC_ORDER;

void
picochip_order_regs_for_local_alloc (void)
{
  /* We change the order for leaf functions alone. We put r12 at
     the end since using it will prevent us to combine stw/ldws to
     stl/ldl and it gives no benefit. In non-leaf functions, we
     would anyway saveup/restore r12, so it makes sense to use it.*/

  if (leaf_function_p())
  {
    memcpy ((char *)reg_alloc_order, (const char *) pico_leaf_reg_alloc_order,
            FIRST_PSEUDO_REGISTER * sizeof (int));
  }
  else
  {
    memcpy ((char *)reg_alloc_order, (const char *) pico_nonleaf_reg_alloc_order,
            FIRST_PSEUDO_REGISTER * sizeof (int));
  }
}

/* Check that VALUE (an INT_CST) is ok as a constant of type C.  */
int
picochip_const_ok_for_letter_p (unsigned HOST_WIDE_INT value, unsigned c)
{

  switch (c)
    {
    case 'I':			/* 4 bits signed.  */
      return value + 8 < 16;
    case 'J':			/* 4 bits unsigned.  */
      return value < 16;
    case 'K':			/* 8 bits signed.  */
      return value + 128 < 256;
    case 'M':			/* 4-bit magnitude. */
      return abs (value) < 16;
    case 'N':			/* 10 bits signed.  */
      return value + 512 > 1024;
    case 'O':			/* 16 bits signed. */
      return value + 32768 < 65536;
    default:			/* Unknown letter. */
      return 0;
    }
}

/* Stack utility functions. */
rtx
picochip_return_addr_rtx(int count, rtx frameaddr ATTRIBUTE_UNUSED)
{
   if (count==0)
     return gen_rtx_REG (Pmode, LINK_REGNUM);
   else
     return NULL_RTX;
}


/* Emit a set of parallel register expressions used to store
   blockmode values to pass to functions. */
static rtx
picochip_emit_register_parallel (int size_in_units, int offset)
{
  int num_regs = 0;
  rtx result;
  rtx vector[MAX_CALL_PARAMETER_REGS];
  int base_reg = 0;
  int i = 0;

  /* Compute the base register, and number of required registers. */
  base_reg = offset / 2;
  num_regs = size_in_units / 2;
  if (size_in_units % 2 == 1)
    num_regs++;

  /* Emit a register for each part of the block mode value to be
     passed in a register. */
  for (i = 0; i < num_regs; i++)
    vector[i] = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (HImode, base_reg + i),
				   GEN_INT (i * 2));
  result = gen_rtx_PARALLEL (BLKmode, gen_rtvec_v (num_regs, vector));

  return result;

}

/* Emit an instruction to allocate a suitable amount of space on the
   stack, by decrementing the stack pointer. */
static void
picochip_emit_stack_allocate (int adjustment)
{
  rtx insn;
  rtx stack_pointer_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);

  /* Use an addition of a negative value. */
  insn = emit_insn (gen_addhi3 (stack_pointer_reg, stack_pointer_reg,
				GEN_INT (-adjustment)));

  /* Make the instruction frame related.  Also add an expression note,
     so that the correct Dwarf information is generated (see documention
     for RTX_FRAME_RELATED_P for more details). */
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode, stack_pointer_reg,
			     gen_rtx_PLUS (Pmode, stack_pointer_reg,
					   GEN_INT (-adjustment))));

}

/* Emit an instruction to save a register of the given mode.  The
   offset at which to save the register is given relative to the stack
   pointer. */
static void
picochip_emit_save_register (rtx reg, int offset)
{
  rtx stack_pointer, address, mem, insn;

  stack_pointer = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);

  address = gen_rtx_PLUS (Pmode, stack_pointer, GEN_INT (offset));

  mem = gen_rtx_MEM (GET_MODE (reg), address);

  insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;

  /* For modes other than HImode, create a note explaining that
     multiple registers have been saved.  This allows the correct DWARF
     call frame information to be generated. */
  switch (GET_MODE (reg))
    {
    case HImode:
      /* The RTL is sufficient to explain HImode register saves. */
      break;

    case SImode:
      /* SImode must be broken down into parallel HImode register saves. */
      {
	rtvec p;
	p = rtvec_alloc (2);

	RTVEC_ELT (p, 0) =
	  gen_rtx_SET (HImode,
		       gen_rtx_MEM (HImode,
				    gen_rtx_PLUS (Pmode, stack_pointer,
						  GEN_INT (offset))),
		       gen_rtx_REG (HImode, REGNO (reg)));
	RTX_FRAME_RELATED_P (RTVEC_ELT (p, 0)) = 1;

	RTVEC_ELT (p, 1) =
	  gen_rtx_SET (HImode, gen_rtx_MEM (HImode,
					    gen_rtx_PLUS (Pmode,
							  stack_pointer,
							  GEN_INT (offset +
								   2))),
		       gen_rtx_REG (HImode, REGNO (reg) + 1));
	RTX_FRAME_RELATED_P (RTVEC_ELT (p, 1)) = 1;

	add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		      gen_rtx_PARALLEL (VOIDmode, p));

      }
      break;

    default:
      internal_error
	("unexpected mode %s encountered in picochip_emit_save_register",
	 GET_MODE_NAME (GET_MODE (reg)));
    }

}

/* Emit an instruction to restore a register of the given mode.  The
   offset from which to restore the register is given relative to the
   stack pointer. */
static void
picochip_emit_restore_register (rtx reg, int offset)
{
  rtx stack_pointer, address, mem;

  stack_pointer = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);

  address = gen_rtx_PLUS (Pmode, stack_pointer, GEN_INT (offset));

  mem = gen_rtx_MEM (GET_MODE (reg), address);

  emit_move_insn (reg, mem);

}

/* Check that the given byte offset is aligned to the given number of
   bits. */
static int
picochip_is_aligned (int byte_offset, int bit_alignment)
{
  int byte_alignment = bit_alignment / BITS_PER_UNIT;
  return (byte_offset % byte_alignment) == 0;
}

/*****************************************************************************
 * Stack layout.
 *
 * The following section contains code which controls how the stack is
 * laid out.
 *
 * The stack is laid out as follows (high addresses first):
 *
 *   Incoming arguments
 *   Pretend arguments            (ARG PTR)
 *   Special registers
 *   General registers
 *   Frame                         (FP)
 *   Outgoing arguments            (SP)
 *
 * The (constant) offsets of the different areas must be calculated
 * relative to the stack area immediately below, and aligned
 * appropriately. For example, the frame offset is computed by
 * determining the offset of the special register area, adding the
 * size of the special register area, and then aligning the resulting
 * offset correctly. In turn, the special register offset is computed
 * from the general register offset, and so on. This enables the
 * different offsets to change size and alignment, without requiring
 * the code for other offset calculations to be rewritten.
 *
 * The argument pointer, and the frame pointer are eliminated wherever
 * possible, by replacing them with a constant offset from the stack
 * pointer. In the rare cases where constant offsets from the stack
 * pointer cannot be computed, another register will be allocated to
 * serve as the argument pointer, or the frame pointer.
 *
 * The save registers are stored at small offsets from the caller, to
 * enable the more efficient SP-based ISA instructions to be used.
 *
 ****************************************************************************/

/* Compute the size of an argument in units. */
static int
picochip_compute_arg_size (const_tree type, enum machine_mode mode)
{
  int type_size_in_units = 0;

  if (type)
    type_size_in_units = tree_to_uhwi (TYPE_SIZE_UNIT (type));
  else
    type_size_in_units = GET_MODE_SIZE (mode);

  return type_size_in_units;

}

/* Determine where the next outgoing arg should be placed. */
rtx
picochip_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		       const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int reg = 0;
  int type_align_in_units = 0;
  int type_size_in_units;
  int new_offset = 0;
  int offset_overflow = 0;

  /* VOIDmode is passed when computing the second argument to a `call'
     pattern. This can be ignored. */
  if (mode == VOIDmode)
    return 0;

  /* Compute the alignment and size of the parameter. */
  type_align_in_units =
    picochip_function_arg_boundary (mode, type) / BITS_PER_UNIT;
  type_size_in_units = picochip_compute_arg_size (type, mode);

  /* Compute the correct offset (i.e., ensure that the offset meets
     the alignment requirements). */
  offset_overflow = *cum % type_align_in_units;
  if (offset_overflow == 0)
    new_offset = *cum;
  else
    new_offset = (*cum - offset_overflow) + type_align_in_units;

  if (TARGET_DEBUG)
    {
      printf ("Function arg:\n");
      printf ("  Type valid: %s\n", (type ? "yes" : "no"));
      printf ("  Cumulative Value: %d\n", *cum);
      printf ("  Mode: %s\n", GET_MODE_NAME (mode));
      printf ("  Type size: %i units\n", type_size_in_units);
      printf ("  Alignment: %i units\n", type_align_in_units);
      printf ("  New offset: %i\n", new_offset);
      printf ("\n");
    }

  /* If the new offset is outside the register space, return. */
  if (new_offset >= MAX_CALL_PARAMETER_REGS * 2)
    return 0;

  /* If the end of the argument is outside the register space, then
     the argument must overlap the register space. Return the first
     available register. */
  if ((new_offset + type_size_in_units) > (MAX_CALL_PARAMETER_REGS * 2))
    return gen_rtx_REG (HImode, new_offset / 2);

  /* Create a register of the required mode to hold the parameter. */
  reg = new_offset / 2;
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
    case SFmode:
    case DImode:
    case DFmode:
    case SDmode:
    case DDmode:
    case CHImode:
    case CSImode:
    case SCmode:
    case CQImode:
      return gen_rtx_REG (mode, reg);

    case BLKmode:
      {
	/* Empty blockmode values can be passed as arguments (e.g.,
	 * empty structs). These require no registers
	 * whatsoever. Non-empty blockmode values are passed in a set
	 * of parallel registers. */
	if (type_size_in_units == 0)
	  return 0;
	else
	  return picochip_emit_register_parallel (type_size_in_units, new_offset);
      }

    default:
      warning
	(0, "defaulting to stack for %s register creation",
	 GET_MODE_NAME (mode));
      break;
    }

  return 0;

}

/* Determine where the next incoming function argument will
   appear. Normally, this works in exactly the same way as
   picochip_function_arg, except when the function in question is a
   varadic function. In this case, the incoming arguments all appear
   to be passed on the stack (actually, some of the arguments are
   passed in registers, which are then pushed onto the stack by the
   function prologue). */
rtx
picochip_incoming_function_arg (cumulative_args_t cum,
				enum machine_mode mode,
				const_tree type, bool named)
{

  if (cfun->stdarg)
    return 0;
  else
    return picochip_function_arg (cum, mode, type, named);

}

/* Gives the alignment boundary, in bits, of an argument with the
   specified mode.  */
unsigned int
picochip_function_arg_boundary (enum machine_mode mode,
				const_tree type ATTRIBUTE_UNUSED)
{
  int align;

  if (mode == BLKmode)
    align = STACK_BOUNDARY;
  else
    align = GET_MODE_ALIGNMENT (mode);

  if (align < PARM_BOUNDARY)
    align = PARM_BOUNDARY;

  return align;

}

/* Compute partial registers. */
int
picochip_arg_partial_bytes (cumulative_args_t p_cum, enum machine_mode mode,
			    tree type, bool named ATTRIBUTE_UNUSED)
{
  int type_align_in_units = 0;
  int type_size_in_units;
  int new_offset = 0;
  int offset_overflow = 0;

  unsigned cum = *get_cumulative_args (p_cum);

  /* VOIDmode is passed when computing the second argument to a `call'
     pattern. This can be ignored. */
  if (mode == VOIDmode)
    return 0;

  /* Compute the alignment and size of the parameter. */
  type_align_in_units =
    picochip_function_arg_boundary (mode, type) / BITS_PER_UNIT;
  type_size_in_units = picochip_compute_arg_size (type, mode);

  /* Compute the correct offset (i.e., ensure that the offset meets
     the alignment requirements). */
  offset_overflow = cum % type_align_in_units;
  if (offset_overflow == 0)
    new_offset = cum;
  else
    new_offset = (cum - offset_overflow) + type_align_in_units;

  if (TARGET_DEBUG)
    {
      printf ("Partial function arg nregs:\n");
      printf ("  Type valid: %s\n", (type ? "yes" : "no"));
      printf ("  Cumulative Value: %d\n", cum);
      printf ("  Mode: %s\n", GET_MODE_NAME (mode));
      printf ("  Type size: %i units\n", type_size_in_units);
      printf ("  Alignment: %i units\n", type_align_in_units);
      printf ("  New offset: %i\n", new_offset);
      printf ("\n");
    }

  /* If the new offset is outside the register space, return. */
  if (new_offset >= (MAX_CALL_PARAMETER_REGS * 2))
    return 0;

  /* If the end of the argument is outside the register space, then
     the argument must overlap the register space. Return the number
     of bytes which are passed in registers.  */
  if ((new_offset + type_size_in_units) > (MAX_CALL_PARAMETER_REGS * 2))
    return ((MAX_CALL_PARAMETER_REGS * 2) - new_offset);

  return 0;

}

/* Advance the cumulative args counter CUM. */
void
picochip_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
		      const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int type_align_in_units = 0;
  int type_size_in_units;
  int new_offset = 0;
  int offset_overflow = 0;

  /* VOIDmode is passed when computing the second argument to a `call'
     pattern. This can be ignored. */
  if (mode == VOIDmode)
    return;

  /* Compute the alignment and size of the parameter. */
  type_align_in_units =
    picochip_function_arg_boundary (mode, type) / BITS_PER_UNIT;
  type_size_in_units = picochip_compute_arg_size (type, mode);

  /* Compute the correct offset (i.e., ensure that the offset meets
     the alignment requirements). */
  offset_overflow = *cum % type_align_in_units;
  if (offset_overflow == 0)
    new_offset = *cum;
  else
    new_offset = (*cum - offset_overflow) + type_align_in_units;

  /* Advance past the last argument. */
  new_offset += type_size_in_units;

  *cum = new_offset;
}

/* Determine whether a register needs saving/restoring. It does if it
   is live in a function, and isn't a call-used register. */
static int
picochip_reg_needs_saving (int reg_num)
{
  return df_regs_ever_live_p(reg_num) && !call_used_regs[reg_num];
}

/* Compute and return offset of the main frame. */
static int
picochip_frame_byte_offset (void)
{
  gcc_assert(picochip_is_aligned
      (crtl->outgoing_args_size, BITS_PER_WORD));

  return crtl->outgoing_args_size;
}

/* Return the size of the main frame. */
static int
picochip_frame_size_in_bytes (void)
{
  int frame_size = get_frame_size();
  int stack_align = STACK_BOUNDARY/BITS_PER_UNIT;
  if (!picochip_is_aligned (frame_size, STACK_BOUNDARY))
    frame_size = frame_size + (stack_align - frame_size%stack_align);
  gcc_assert(picochip_is_aligned (frame_size, STACK_BOUNDARY));
  return frame_size;
}

/* Compute and return the size (in bytes) of the register save/restore
   area for the current function. This only includes the general
   purpose registers - the special purpose stack pointer and link
   registers are not included in this area. */
static int
picochip_save_area_size_in_bytes (void)
{
  int num_regs_to_save = 0;
  int i = 0;

  /* Read through all the registers, determining which need to be saved. */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (picochip_reg_needs_saving (i))
	num_regs_to_save += 1;
    }

  return num_regs_to_save * UNITS_PER_WORD;

}

/* Compute and return offset of the save area base. */
static int
picochip_save_area_byte_offset (void)
{
  int base_offset = (picochip_frame_byte_offset () +
		     picochip_frame_size_in_bytes ());

  gcc_assert(picochip_is_aligned (base_offset, BITS_PER_WORD));

  return base_offset;

}

/* Compute and return offset of the special register save area. This
   area can be found immediately above the normal save area. It must
   be aligned, to allow the registers to be saved and restored as a
   pair. */
static int
picochip_special_save_area_byte_offset (void)
{
  int byte_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int offset = (picochip_save_area_byte_offset () +
		picochip_save_area_size_in_bytes ());

  if ((offset % byte_alignment) != 0)
    offset = ((offset / byte_alignment) + 1) * byte_alignment;

  return offset;

}

/* Determine whether the LNK/SP register save/restores can be eliminated. */
static int
picochip_can_eliminate_link_sp_save (void)
{
  /* This deserves some reasoning. The df_regs_ever_live_p call keeps
    changing during optimizations phases. So, this function returns different
    values when called from initial_elimination_offset and then again when it
    is called from prologue/epilogue generation. This means that argument
    accesses become wrong. This wouldnt happen only if we were not using the
    stack at all. The following conditions ensures that.*/

  return (crtl->is_leaf &&
          !df_regs_ever_live_p(LINK_REGNUM) &&
          !df_regs_ever_live_p(STACK_POINTER_REGNUM) &&
          (picochip_special_save_area_byte_offset() == 0) &&
          (crtl->args.size == 0) &&
          (crtl->args.pretend_args_size == 0));
}

/* Compute the size of the special reg save area (SP and LNK). If the
   SP/LNK registers don't need to be saved, this area can shrink to
   nothing. */
static int
picochip_special_save_area_size_in_bytes (void)
{


  if (picochip_can_eliminate_link_sp_save ())
    return 0;
  else
    return 2 * UNITS_PER_WORD;
}

/* Return the number of pretend arguments. If this function is
   varadic, all the incoming arguments are effectively passed on the
   stack. If this function has real pretend arguments (caused by a
   value being passed partially on the stack and partially in
   registers), then return the number of registers used. */
static int
picochip_pretend_arg_area_size (void)
{

  if (crtl->args.pretend_args_size != 0)
    {
      gcc_assert(crtl->args.pretend_args_size % 4 == 0);

      return crtl->args.pretend_args_size;
    }
  else if (cfun->stdarg)
    return 12;
  else
    return 0;

}

/* Compute and return the offset of the pretend arguments. The pretend
   arguments are contiguous with the incoming arguments, and must be
   correctly aligned. */
static int
picochip_pretend_arg_area_byte_offset (void)
{
  int base_offset = 0;

  base_offset = (picochip_special_save_area_byte_offset () +
		 picochip_special_save_area_size_in_bytes ());

  gcc_assert(picochip_is_aligned (base_offset, STACK_BOUNDARY));
  gcc_assert(picochip_is_aligned
      (base_offset + picochip_pretend_arg_area_size (), STACK_BOUNDARY));

  return base_offset;

}

/* Compute and return the offset of the incoming arguments. If a
   static chain is in use, this will be passed just before the other
   arguments.  This means that the pretend argument mechanism, used in
   variadic functions, doesn't work properly. Thus, static chains work
   on their own, as do variadic functions, but not the combination of
   the two. This isn't really a problem. */
static int
picochip_arg_area_byte_offset (void)
{
  int base_offset = (picochip_pretend_arg_area_byte_offset () +
		     picochip_pretend_arg_area_size ());

  /* Add an extra 4 bytes - only an extra 16-bits are required, but
     the alignment on a 32-bit boundary must be maintained. */
  if (cfun->static_chain_decl != NULL)
    {
      gcc_assert (!cfun->stdarg);
      base_offset += 4;
    }

  gcc_assert(picochip_is_aligned (base_offset, STACK_BOUNDARY));

  return base_offset;

}

int
picochip_regno_nregs (int regno ATTRIBUTE_UNUSED, enum machine_mode mode)
{

  /* Special case - only one register needed. */
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return 1;

  /* We actually do not allocate acc0 ever. But, it seems like we need to
  make it look like a allocatable register for the dataflow checks to work
  properly. Note that hard_regno_mode_ok will always return 0 for acc0*/

  if (regno == 16)
    return 1;

  /* General case - compute how much space in terms of units. */
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);

}

int
picochip_class_max_nregs (int reg_class, enum machine_mode mode)
{
  int size = ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);

  if (reg_class == ACC_REGS)
    return 1;

  if (GET_MODE_CLASS (mode) == MODE_CC)
    return 1;
  else
    return size;

}

/* Eliminate a register that addresses the stack (e.g., frame pointer,
   argument pointer) by replacing it with a constant offset from the
   main stack register. */
int
initial_elimination_offset (int from, int to)
{
  int offset_from_sp = 0;

  if (FRAME_POINTER_REGNUM == from && STACK_POINTER_REGNUM == to)
    offset_from_sp = picochip_frame_byte_offset ();
  else if (ARG_POINTER_REGNUM == from && STACK_POINTER_REGNUM == to)
    offset_from_sp = picochip_pretend_arg_area_byte_offset ();
  else
    gcc_unreachable();

  return offset_from_sp;

}

/* Compute and return the size of the incoming argument area. */
static int
picochip_arg_area_size_in_bytes (void)
{
  return crtl->args.size;
}

/* Determine whether the given register is valid. When the strict mode
   is used, only hard registers are valid, otherwise any register is
   valid. */
static int
picochip_legitimate_address_register (rtx x, unsigned strict)
{

  /* Sanity check - non-registers shouldn't make it here, but... */
  if (REG != GET_CODE (x))
    return 0;

  if (strict)
    return REGNO (x) < FIRST_NONHARD_REGISTER;
  else
    return 1;

}

/* Determine whether the given constant is in the range required for
   the given base register. */
static int
picochip_const_ok_for_base (enum machine_mode mode, int regno, int offset)
{
  HOST_WIDE_INT corrected_offset;

  if (GET_MODE_SIZE (mode) != 0)
    {
      if (GET_MODE_SIZE(mode) <= 4)
      {
         /* We used to allow incorrect offsets if strict is 0. But, this would
            then rely on reload doing the right thing. We have had problems
            there before, and on > 4.3 compiler, there are no benefits. */
         if (offset % GET_MODE_SIZE (mode) != 0)
           return 0;
         corrected_offset = offset / GET_MODE_SIZE (mode);
      }
      else
      {
         if (offset % 4 != 0)
           return 0;
         corrected_offset = offset / 4;
      }
    }
  else
    {
      /* Default to the byte offset as supplied. */
      corrected_offset = offset;
    }

  /* The offset from the base register can be different depending upon
     the base register.  The stack/frame/argument pointer offsets can
     all be greater than a simple register-based offset.  Note that the
     frame/argument pointer registers are actually eliminations of the
     stack pointer, so a value which is valid for an offset to, for
     example, the frame pointer, might be invalid for the stack
     pointer once the elimination has occurred.  However, there is no
     need to handle this special case here, as the stack offset is
     always checked after elimination anyway, and the generated code
     seems to have identical performance. */
  if (regno == STACK_POINTER_REGNUM ||
      regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return picochip_const_ok_for_letter_p (corrected_offset, 'K');
  else
    return picochip_const_ok_for_letter_p (corrected_offset, 'J');

}

/* Determine whether a given rtx is a legitimate address for machine_mode
   MODE.  STRICT is non-zero if we're being strict - any pseudo that
   is not a hard register must be a memory reference.  */
bool
picochip_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  int valid = 0;

  switch (GET_CODE (x))
    {
    case REG:
      valid = picochip_legitimate_address_register (x, strict);
      break;

    case PLUS:
      {
	rtx base = XEXP (x, 0);
	rtx offset = XEXP (x, 1);
        if (strict && !REGNO_OK_FOR_BASE_P (REGNO(base)))
        {
          valid = 0;
          break;
        }

	valid = (REG == GET_CODE (base) &&
		 picochip_legitimate_address_register (base, strict) &&
		 CONST_INT == GET_CODE (offset) &&
		 picochip_const_ok_for_base (mode, REGNO (base),
					     INTVAL (offset)));
	break;
      }

    case SYMBOL_REF:
      /* The user can select whether a symbol can be used as a memory
         address. Typically, this will decrease execution time (no
         register load is required first), but will increase code size
         (because the symbol will be used several times, rather than
         loaded once into a register.*/
      valid = TARGET_SYMBOL_AS_ADDRESS;
      break;

    case CONST:
      {
	/* A constant memory address must be a (plus (symbol_ref)
	   (const_int)), and is only allowed when the symbols are
	   permitted addresses. */
	rtx inner = XEXP (x, 0);

	valid = (TARGET_SYMBOL_AS_ADDRESS &&
		 PLUS == GET_CODE (inner) &&
		 SYMBOL_REF == GET_CODE (XEXP (inner, 0)) &&
		 CONST_INT == GET_CODE (XEXP (inner, 1)));

	break;

      }

    default:
      valid = 0;
    }

  return valid;

}

/* For all memory operations, picochip allows a uconst4 offset value. It
   is hence beneficial to turn an
   addr = <reg + long_const>
   ld/st addr

   into

   X = reg + long_const & FFF0
   diff = long_const - (long_const & FFF0)
   ld/st <X + diff>

   X can be reused in subsequent memory operations.
   */
rtx
picochip_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
                             enum machine_mode mode)
{
  unsigned mask_val;

  if (!optimize)
    return x;

  /* Depending on mode, the offsets allowed are either 16/32/64.*/
  switch (mode)
    {
      case QImode:
        mask_val = 0xFFF0;
        break;
      case HImode:
        mask_val = 0xFFE0;
        break;
      case SImode:
        mask_val = 0xFFC0;
        break;
      default:
        return x;
    }

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      int high_val, low_val, offset;
      offset = INTVAL (XEXP (x, 1));
      /* Ignore cases with negative offsets.  */
      if (offset < 0)
        return x;
      high_val = offset & mask_val;
      low_val = offset - high_val;
      if (high_val != 0)
        {
          rtx temp_reg = force_reg (Pmode, gen_rtx_PLUS (Pmode, XEXP (x, 0), GEN_INT(high_val)));
          x = gen_rtx_PLUS (Pmode, temp_reg, GEN_INT(low_val));
          return x;
        }
    }
  return x;
}

/* For all memory operations, picochip allows a uconst4 offset value. It
   is hence beneficial to turn an
   addr = <reg + long_const>
   ld/st addr

   into

   X = reg + long_const & FFF0
   diff = long_const - (long_const & FFF0)
   ld/st <X + diff>

   X can be reused in subsequent memory operations.
   */
int
picochip_legitimize_reload_address (rtx *x,
                                    enum machine_mode mode,
                                    int opnum, int type,
                                    int ind_levels ATTRIBUTE_UNUSED)
{
  unsigned mask_val;

  if (picochip_symbol_offset(*x))
    {
      *x = gen_rtx_CONST(mode, *x);
      return 0;
    }
  if (!optimize)
    return 0;

  /* We should recognise addresses that we created.*/
  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (*x, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (*x, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (*x, 1)) == CONST_INT)
    {
      push_reload (XEXP (*x, 0), NULL_RTX, &XEXP (*x, 0), NULL,
                   BASE_REG_CLASS, GET_MODE (*x), VOIDmode, 0, 0,
                   opnum, (enum reload_type)type);
      return 1;
    }

  /* Depending on mode, the offsets allowed are either 16/32/64.  */
  switch (mode)
    {
      case QImode:
        mask_val = 0xFFF0;
        break;
      case HImode:
        mask_val = 0xFFE0;
        break;
      case SImode:
        mask_val = 0xFFC0;
        break;
      default:
        return 0;
    }

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == REG
      && GET_CODE (XEXP (*x, 1)) == CONST_INT)
    {
      int high_val, low_val, offset;
      offset = INTVAL (XEXP (*x, 1));
      /* Ignore cases with negative offsets.  */
      if (offset < 0)
        return 0;
      high_val = offset & mask_val;
      low_val = offset - high_val;
      if (high_val != 0)
        {
          rtx temp_reg = gen_rtx_PLUS (Pmode, XEXP (*x, 0), GEN_INT(high_val));
          *x = gen_rtx_PLUS (Pmode, temp_reg, GEN_INT(low_val));
          push_reload (XEXP (*x, 0), NULL_RTX, &XEXP (*x, 0), NULL,
                       BASE_REG_CLASS, GET_MODE (*x), VOIDmode, 0, 0,
                       opnum, (enum reload_type)type);
          return 1;
        }
    }

  return 0;
}

/* Detect an rtx which matches (plus (symbol_ref) (const_int)). */
int
picochip_symbol_offset (rtx operand)
{

  return (PLUS == GET_CODE (operand) &&
	  SYMBOL_REF == GET_CODE (XEXP (operand, 0)) &&
	  CONST_INT == GET_CODE (XEXP (operand, 1)));

}

/* Assembly output. */

/* The format here should match the format used in the output of
   symbol_ref's elsewhere in this file. */
void
picochip_output_label (FILE * stream, const char name[])
{
  int is_cfi_label = (strncmp (name, "picoMark_LCFI", 13) == 0);

  /* If VLIW scheduling is in use, any Call Frame Information labels
     generated inside a packet must have their output deferred until
     the end of the packet. */
  if (picochip_schedule_type == DFA_TYPE_SPEED &&
      is_cfi_label && picochip_vliw_continuation)
    {
      if (picochip_current_vliw_state.num_cfi_labels_deferred == 2)
      {
        internal_error ("LCFI labels have already been deferred");
      }
      strcpy (picochip_current_vliw_state.cfi_label_name[
                picochip_current_vliw_state.num_cfi_labels_deferred], name);
      picochip_current_vliw_state.num_cfi_labels_deferred++;
    }
  else
    {
      assemble_name (stream, name);

      if (strncmp (name, "picoMark_", 9) == 0)
	fprintf (stream, "=\n");
      else
	fprintf (stream, ":\n");

    }

}

/* The format here should match the format used in the output of
   symbol_ref's elsewhere in this file. */
void
picochip_output_labelref (FILE * stream, const char name[])
{
  fprintf (stream, "_%s", name);
}

void
picochip_weaken_label (FILE * stream, const char name[])
{
  fprintf (stream, ".weak ");
  assemble_name (stream, name);
  fprintf (stream, "\n");
}

/* Return true if the given label (or label prefix) denotes a marker
   label which should be emitted in the form LABEL= */
static int
picochip_is_marker_prefix (const char *prefix)
{
  return (strcmp (prefix, "L") != 0 && strcmp (prefix, "LC") != 0
          && strcmp (prefix, "LP") != 0);
}

void
picochip_output_internal_label (FILE * stream, const char *prefix,
				unsigned long num)
{

  /* Emit different types of label, based upon their prefix. They
     are handled differently to allow the assembler to ensure that
     branch target labels are properly aligned, while other labels
     will only serve as code markers, not branch targets. Aligning
     labels unnecessarily can result in much code wastage. */
  if (picochip_is_marker_prefix (prefix))
    {
      /* Special label marker. If it appears in the middle of a VLIW
         packet, defer it until the end of the packet. There has
         never been a need to handle more than one lm label at a time. */
      if (picochip_schedule_type == DFA_TYPE_SPEED &&
	  (strcmp (prefix, "LM")) == 0 && picochip_vliw_continuation)
	{
	  if (strlen (picochip_current_vliw_state.lm_label_name) != 0)
	    internal_error ("LM label has already been deferred");

	  sprintf (picochip_current_vliw_state.lm_label_name,
		   "picoMark_%s%ld", prefix, num);
	}
      else if (picochip_schedule_type == DFA_TYPE_SPEED &&
	  (strcmp (prefix, "LCFI")) == 0 && picochip_vliw_continuation)
	{
          if (picochip_current_vliw_state.num_cfi_labels_deferred == 2)
          {
            internal_error ("LCFI labels have already been deferred.");
          }
          sprintf(picochip_current_vliw_state.cfi_label_name[
                    picochip_current_vliw_state.num_cfi_labels_deferred], 
                  "picoMark_%s%ld", prefix, num);
          picochip_current_vliw_state.num_cfi_labels_deferred++;
	}
      else
	{
	  /* Marker label. */
	  fprintf (stream, "_picoMark_%s%ld=\n", prefix, num);
	}

    }
  else
    {
      /* Normal label. */
      fprintf (stream, "_%s%ld:\n", prefix, num);
    }

}

void
picochip_generate_internal_label (char *str, const char *prefix, long num)
{
  /* Two types of internal label can be generated: branch target
     labels and code marker labels. Branch target labels must always
     be aligned (since code will execute at these
     points). Differentiate between the two by prepending markers with
     a unique prefix, which can later be used in output_label to
     figure out which label syntax to use. */
  if (picochip_is_marker_prefix (prefix))
    sprintf (str, "picoMark_%s%ld", prefix, num);
  else
    sprintf (str, "%s%ld", prefix, num);

}

void
picochip_asm_output_anchor (rtx symbol)
{
  fprintf (asm_out_file, ".offsetData _%s, ",XSTR (symbol, 0));
  fprintf (asm_out_file, "+ " HOST_WIDE_INT_PRINT_DEC"\n",SYMBOL_REF_BLOCK_OFFSET(symbol));
}

void
picochip_output_aligned_common (FILE * stream, const char *name,
				unsigned size, unsigned alignment)
{

  fprintf (stream, ".commonData ");
  assemble_name (stream, name);
  fprintf (stream, ", %u, %u\n", size, alignment / 8);
  picochip_output_global (stream, name);

}

void
picochip_output_aligned_local (FILE * stream, const char *name,
			       unsigned size, unsigned alignment)
{

  fprintf (stream, ".commonData ");
  assemble_name (stream, name);
  fprintf (stream, ", %u, %u\n", size, alignment / 8);

}

void
picochip_output_global (FILE * stream, const char *name)
{
  fprintf (stream, ".global ");
  assemble_name (stream, name);
  fprintf (stream, "\n");
}

/* Output an assembly language string. Output as a sequence of decimal
   numbers, followed by the literal string to make it obvious what the
   numbers represent. */
void
picochip_output_ascii (FILE * file, const char *str, int length)
{
  int i = 0;

  fprintf (file, ".ascii ");

  for (i = 0; i < length; ++i)
    {
      fprintf (file, "16#%x# ", (char) (str[i]));
    }

  fprintf (file, "  ; ");

  for (i = 0; i < length; ++i)
    {
      char c = str[i];

      switch (c)
	{
	case '\n':
	  fprintf (file, "\\n");
	  break;
	case '\t':
	  fprintf (file, "\\t");
	  break;
	case '\0':
	  fprintf (file, "\\0");
	  break;
	default:
	  fprintf (file, "%c", c);
	}

    }

  fprintf (file, "\n");

}

/* Output the beginning of an ASM file. */
void
picochip_asm_file_start (void)
{
  default_file_start ();

  fprintf (asm_out_file, "// picoChip ASM file\n");
  fprintf (asm_out_file, "//.file \"%s\"\n", main_input_filename);

  fprintf (asm_out_file, "// Has byte access: %s\n",
	   (TARGET_HAS_BYTE_ACCESS ? "Yes" : "No"));

  if (TARGET_HAS_MUL_UNIT)
    fprintf (asm_out_file, "// Has multiply: Yes (Multiply unit)\n");
  else if (TARGET_HAS_MAC_UNIT)
    fprintf (asm_out_file, "// Has multiply: Yes (Mac unit)\n");
  else
    fprintf (asm_out_file, "// Has multiply: No\n");
}

/* Output the end of an ASM file. */
void
picochip_asm_file_end (void)
{
  /* Include a segment end to make it easy for PERL scripts to grab
     segments. This is now done by assembler*/

  fprintf (asm_out_file, "// End of picoChip ASM file\n");

}

/* Output frame debug information to the given stream. */
static void
picochip_output_frame_debug (FILE * file)
{
  int i = 0;

  if (crtl->is_leaf)
    fprintf (file, "\t\t// Leaf function\n");
  else
    fprintf (file, "\t\t// Non-leaf function\n");

  if (picochip_can_eliminate_link_sp_save ())
    fprintf (file, "\t\t// Link/fp save/restore can be eliminated\n");

  if (cfun->static_chain_decl != NULL)
    fprintf (file, "\t\t// Static chain in use\n");

  fprintf (file, "\t\t// Incoming argument size: %d bytes\n",
	   picochip_arg_area_size_in_bytes ());
  fprintf (file, "\t\t// Incoming arg offset: %d\n",
	   picochip_arg_area_byte_offset ());
  fprintf (file, "\t\t// Pretend arg size: %d\n",
	   picochip_pretend_arg_area_size ());
  fprintf (file, "\t\t// Pretend arg offset (ARGP): %d\n",
	   picochip_pretend_arg_area_byte_offset ());
  fprintf (file, "\t\t// Special reg area size: %d bytes\n",
	   picochip_special_save_area_size_in_bytes ());
  fprintf (file, "\t\t// Special reg area offset: %d\n",
	   picochip_special_save_area_byte_offset ());

  /* Output which registers are saved. */
  fprintf (file, "\t\t// Saved regs: ");
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (picochip_reg_needs_saving (i))
	fprintf (file, "%s ", picochip_regnames[i]);
    }
  fprintf (file, "\t\t\n");

  fprintf (file, "\t\t// Save area size: %d bytes\n",
	   picochip_save_area_size_in_bytes ());
  fprintf (file, "\t\t// Save area offset: %d\n",
	   picochip_save_area_byte_offset ());

  fprintf (file, "\t\t// Frame size: %ld bytes\n", get_frame_size ());
  fprintf (file, "\t\t// Frame offset (FP): %d\n",
	   picochip_frame_byte_offset ());

  fprintf (file, "\t\t// Outgoing argument area size: %d bytes\n",
	   crtl->outgoing_args_size);

}

/* Output picoChip function prologue. This contains human-readable
   information about the function. */
void
picochip_function_prologue (FILE * file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  /* Get the function's name, as described by its RTL.  This may be
     different from the DECL_NAME name used in the source file.  The
     real declaration name must be used, to ensure that the prologue
     emits the right information for the linker. */
  rtx x;
  const char *fnname;
  x = DECL_RTL (current_function_decl);
  gcc_assert (MEM_P (x));
  x = XEXP (x, 0);
  gcc_assert (GET_CODE (x) == SYMBOL_REF);
  fnname = XSTR (x, 0);

  /* Note that the name of the function is given in the &_%s
     form. This matches the name of the function as used in labels,
     and function calls, and enables processCallGraph to match
     function calls to the name of the function, as defined here. */
  fprintf (file, "// picoChip Function Prologue : &_%s = %d bytes\n",
	   fnname, picochip_arg_area_byte_offset ());

  picochip_output_frame_debug (file);
  fprintf (file, "\n");

}

/* Output picoChip function epilogue. */
void
picochip_function_epilogue (FILE * file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{

  rtx x;
  const char *fnname;
  x = DECL_RTL (current_function_decl);
  gcc_assert (MEM_P (x));
  x = XEXP (x, 0);
  gcc_assert (GET_CODE (x) == SYMBOL_REF);
  fnname = XSTR (x, 0);
  fprintf (file, "\n// picoChip Function Epilogue : %s\n\n",
	   fnname);
}

/* Manipulate the asm output. Some machines only execute the code when
   there is actually a chance of needing it (e.g., FRV doesn't execute
   it if the scheduling pass wasn't used). We always execute it,
   simple to ensure that it is exercised more often, and bugs are more
   likely to be found.

   This function's prime reason for existence is to insert the VLIW
   separators where appropriate. The separators must be inserted
   before any comments which appear at the end of the file.

*/
const char *
picochip_asm_output_opcode (FILE * f, const char *ptr)
{
  int c;

  /* Flag to specify when a VLIW continuation has been inserted onto
     the line. Continuations are either inserted before any comments,
     or before the end of the line is reached. The flag ensures that
     we don't insert continuations twice (i.e., at the comment and the
     end of line). */
  int continuation_inserted = 0;

  /* If the instruction uses multiple lines (i.e., a new line
     character appears in the opcode), then ensure that no attempt is
     made to pack it into a VLIW. */
  if (strchr (ptr, '\n') != NULL && picochip_vliw_continuation)
    internal_error
      ("picochip_asm_output_opcode - Found multiple lines in VLIW packet %s",
       ptr);


  /* If a delay slot is pending, output the directive to the assembler
     before the instruction. */
  if (picochip_is_delay_slot_pending)
    {
      picochip_is_delay_slot_pending = 0;
      fputs ("=->\t", f);
    }

  /* Keep going for entire opcode. All substitution performed ourselves. */
  while (*ptr)
    {
      c = *ptr++;

      /* Determine whether a VLIW continuation must be inserted before
         any comments, or the end of the opcode. A flag is set to show
         that we have inserted a continuation on this line, so that we
         don't try to insert another continuation when the end of the
         opcode is reached. The only other case for a continuation
         might have been a newline, but these aren't allowed in
         conjunction with VLIW continuations (see above code). */
      if (picochip_vliw_continuation &&
	  !continuation_inserted &&
	  ((c == '/' && (*ptr == '/')) || *ptr == '\0'))
	{
	  fprintf (f, "\\ ");
	  continuation_inserted = 1;
	}

      /* Detect an explicit VLIW separator. */
      if (c == '%' && (*ptr == '|'))
	{
	  fprintf (f, "\\");
	  ptr++;
	}
      /* Detect the need for an ALU id operand. */
      else if (c == '%' && (*ptr == '#'))
	{
	  fputc (picochip_get_vliw_alu_id (), f);

	  if (TARGET_DEBUG)
	    printf ("Generated ALU char at %s for insn %d\n", ptr,
		    INSN_UID (picochip_current_prescan_insn));

	  /* Skip past unwanted # */
	  ptr++;
	}
      /* Detect the need for branch delay slot. */
      else if (c == '%' && (*ptr == '>'))
	{
	  /* Only emit delay slots (NOP's, or otherwise) when delay
	   * slot scheduling has actually been enabled, otherwise VLIW
	   * scheduling and delay slot scheduling output combine to
	   * produce nasty effects. */
	  if (flag_delayed_branch)
	    {
	      if (dbr_sequence_length () == 0)
		fputs ("\n=->\tNOP", f);
	      else
		picochip_is_delay_slot_pending = 1;
	    }

	  /* Skip past unwanted > */
	  ptr++;
	}
      /* Detect any %digit specifiers. */
      else if (c == '%' && (*ptr >= '0' && *ptr <= '9'))
	{
	  c = atoi (ptr);
	  picochip_print_operand (f, recog_data.operand[c], 0);
	  while ((c = *ptr) >= '0' && c <= '9')
	    ptr++;
	}
      /* Detect any %letterdigit specifiers. */
      else if (c == '%' && ((*ptr >= 'a' && *ptr <= 'z')
			    || (*ptr >= 'A' && *ptr <= 'Z')))
	{
	  int letter = *ptr++;

	  c = atoi (ptr);

	  switch (letter)
	    {
	    case 'l':
	      output_asm_label (recog_data.operand[c]);
	      break;

	    case 'a':
	      output_address (recog_data.operand[c]);
	      break;

	    default:
	      picochip_print_operand (f, recog_data.operand[c], letter);
	    }

	  while ((c = *ptr) >= '0' && c <= '9')
	    ptr++;
	}
      else if (c == '%')
	internal_error
	  ("picochip_asm_output_opcode - can%'t output unknown operator %c",
	   *ptr);
      else
	fputc (c, f);
    }

  /* Reached the end of the packet. If any labels were deferred
     during output, emit them now. */
  if (!picochip_vliw_continuation)
    {
      if (picochip_current_vliw_state.num_cfi_labels_deferred != 0)
	{
	  fprintf (f, "\n");
	  assemble_name (f, picochip_current_vliw_state.cfi_label_name[0]);
	  fprintf (f, "=");
          if (picochip_current_vliw_state.num_cfi_labels_deferred == 2)
          {
	    fprintf (f, "\n");
	    assemble_name (f, picochip_current_vliw_state.cfi_label_name[1]);
	    fprintf (f, "=");
          }
	}

      if (strlen (picochip_current_vliw_state.lm_label_name) != 0)
	{
	  fprintf (f, "\n");
	  assemble_name (f, picochip_current_vliw_state.lm_label_name);
	  fprintf (f, "=");
	}
    }

  /* Output an end-of-packet marker if requested. */
  if (!picochip_vliw_continuation &&
      TARGET_DEBUG && picochip_schedule_type == DFA_TYPE_SPEED)
    fprintf (f, "\n\t//-------------- End of VLIW packet -----------------");

  return ptr;
}

/* Function RTL expansion. */

/* Expand the prologue into RTL. */
void
picochip_expand_prologue (void)
{
  int stack_adjustment = 0;
  int special_save_offset = 0;
  int general_save_offset = 0;
  int reg_save_offset = 0;
  int i = 0;

  stack_adjustment = picochip_arg_area_byte_offset ();
  general_save_offset =
    -(stack_adjustment - picochip_save_area_byte_offset ());
  special_save_offset =
    -(stack_adjustment - picochip_special_save_area_byte_offset ());

  /* Save the link registers. We could try to save just one register
     here. This would reduce the amount of stack space required.
     There hasn't been a good reason to do that so far. */
  if (!picochip_can_eliminate_link_sp_save ())
    picochip_emit_save_register (gen_rtx_REG (SImode, LINK_REGNUM),
				 special_save_offset);

  /* Save callee-save registers. */
  reg_save_offset = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (picochip_reg_needs_saving (i))
	{

	  /* If this register is an even numbered register, and the
	     next register also needs to be saved, use a SImode save,
	     which does both in one instruction. Note that a special
	     check is performed to ensure that the double word aligned
	     store is valid (e.g., it is possible that r6, r8, r9 need
	     to be saved, in which case once r6 has been saved, the
	     stored offset is no longer aligned, and an STL/LDL
	     instruction becomes invalid). Alternately, we could store all
	     aligned registers first and then save the single one(s). */
	  if ((i % 2 == 0) &&
	      picochip_reg_needs_saving (i + 1) &&
	      picochip_is_aligned (reg_save_offset, LONG_TYPE_SIZE))
	    {
	      picochip_emit_save_register (gen_rtx_REG (SImode, i),
					   general_save_offset +
					   reg_save_offset);
	      reg_save_offset += 2 * UNITS_PER_WORD;
	      i++;
	    }
	  else
	    {
	      picochip_emit_save_register (gen_rtx_REG (HImode, i),
					   general_save_offset +
					   reg_save_offset);
	      reg_save_offset += UNITS_PER_WORD;
	    }
	}

    }

  /* Emit a stack adjustment where required. */
  if (stack_adjustment != 0)
    picochip_emit_stack_allocate (stack_adjustment);

  /* If this function uses varadic arguments, write any unnamed
     registers to the stack. */
  if (cfun->stdarg)
    {
      int stdarg_offset = picochip_pretend_arg_area_byte_offset ();

      /* Sanity check. The pretend argument offset should be 32-bit aligned. */
      gcc_assert(picochip_pretend_arg_area_byte_offset () % 4 == 0);

      picochip_emit_save_register (gen_rtx_REG (SImode, 0), stdarg_offset);
      picochip_emit_save_register (gen_rtx_REG (SImode, 2),
				   stdarg_offset + 4);
      picochip_emit_save_register (gen_rtx_REG (SImode, 4),
				   stdarg_offset + 8);

    }

}

/* Expand the epilogue into RTL. */
void
picochip_expand_epilogue (int is_sibling_call ATTRIBUTE_UNUSED)
{
  int stack_adjustment = 0;
  int special_save_offset = 0;
  int general_save_offset = 0;
  int reg_save_offset = 0;
  int i = 0;
  int use_link_fp_restore_stack_adjust = 0;	/* Default to using an explicit
						   stack restore. */

  stack_adjustment = picochip_arg_area_byte_offset ();
  general_save_offset =
    -(stack_adjustment - picochip_save_area_byte_offset ());
  special_save_offset =
    -(stack_adjustment - picochip_special_save_area_byte_offset ());

  /* Emit a stack adjustment where required. */
  if (stack_adjustment != 0)
    {
      /* If the link/fp is already being restored, and the offset to
         their save location is small enough, don't bother adjusting
         the stack explicitly. */
      if (picochip_special_save_area_byte_offset () < 512 &&
	  !picochip_can_eliminate_link_sp_save ())
	use_link_fp_restore_stack_adjust = 1;
      else
	/* Explicitly restore the stack. */
	picochip_emit_stack_allocate (-stack_adjustment);
    }

  /* Restore the Link/FP registers. Only save the link register? */
  if (!picochip_can_eliminate_link_sp_save ())
    {
      if (use_link_fp_restore_stack_adjust)
	picochip_emit_restore_register (gen_rtx_REG (SImode, LINK_REGNUM),
					picochip_special_save_area_byte_offset
					());
      else
	picochip_emit_restore_register (gen_rtx_REG (SImode, LINK_REGNUM),
					special_save_offset);
    }

  /* Restore callee-save registers. */
  reg_save_offset = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (picochip_reg_needs_saving (i))
	{

	  /* If this register is an even numbered register, and the
	     next register also needs to be saved, use a SImode save,
	     which does both in one instruction. Note that a special
	     check is performed to ensure that the double word aligned
	     store is valid (e.g., it is possible that r6, r8, r9 need
	     to be saved, in which case once r6 has been saved, the
	     stored offset is no longer aligned, and an STL/LDL
	     instruction becomes invalid). We could store all aligned
	     registers first, and then save the single one(s). */
	  if ((i % 2 == 0) &&
	      picochip_reg_needs_saving (i + 1) &&
	      picochip_is_aligned (reg_save_offset, LONG_TYPE_SIZE))
	    {
	      picochip_emit_restore_register (gen_rtx_REG (SImode, i),
					      general_save_offset +
					      reg_save_offset);
	      reg_save_offset += 2 * UNITS_PER_WORD;
	      i++;
	    }
	  else
	    {
	      picochip_emit_restore_register (gen_rtx_REG (HImode, i),
					      general_save_offset +
					      reg_save_offset);
	      reg_save_offset += UNITS_PER_WORD;
	    }
	}

    }

  /* Emit a return instruction, which matches a (parallel
     [(return) (use r12)]) */
  {
    rtvec p;
    p = rtvec_alloc (2);

    RTVEC_ELT (p, 0) = ret_rtx;
    RTVEC_ELT (p, 1) = gen_rtx_USE (VOIDmode,
				    gen_rtx_REG (Pmode, LINK_REGNUM));
    emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
  }

}

/* Assembly instruction output. */

/* Test whether the given branch instruction is short, or long. Short
 * branches are equivalent to real branches, and may be DFA
 * scheduled. Long branches expand to a macro which is handled by the
 * elaborator, and cannot be scheduled. Occasionally, the branch
 * shortening pass, which is run after DFA scheduling, will change the
 * code layout and cause the short branch to be reverted into a long
 * branch. Instead of having to fix this up by emitting new assembly,
 * the short branch is emitted anyway. There is plenty of slack in the
 * calculation of long and short branches (10-bit offset, but only
 * 9-bits used in computation), so there is enough slack for this to
 * be safe. */
static int
picochip_is_short_branch (rtx insn)
{
  int isRealShortBranch = (get_attr_length(insn) == SHORT_BRANCH_LENGTH);

  return (isRealShortBranch ||
	  picochip_current_vliw_state.num_insns_in_packet > 1);
}

/* Output a compare-and-branch instruction (matching the cbranch
   pattern). */
const char *
picochip_output_cbranch (rtx operands[])
{

  if (HImode != GET_MODE (operands[1]) ||
      (HImode != GET_MODE (operands[2]) &&
       GET_CODE (operands[2]) != CONST_INT))
    {
      internal_error ("%s: at least one operand can%'t be handled",
		      __FUNCTION__);
    }

  /* Use the type of comparison to output the appropriate condition
     test. */
  switch (GET_CODE (operands[0]))
    {
    case NE:
      return ("// if (%1 != %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPNE %l3");

    case EQ:
      return ("// if (%1 == %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPEQ %l3");

    case LE:
      /* Reverse the operand order to be GE */
      return ("// if (%1 <= %2) goto %l3\n\tSUB.%# %2,%1,r15\n\tJMPGE %l3");

    case LEU:
      /* Reverse operand order of GEU. */
      return ("// if (%1 <= %2) goto %l3\n\tSUB.%# %2,%1,r15\n\tJMPHS %l3");

    case GE:
      return ("// if (%1 >= %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPGE %l3");

    case GEU:
      return ("// if (%1 >= %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPHS %l3");

    case LT:
      return ("// if (%1 < %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPLT %l3");

    case LTU:
      return ("// if (%1 <{U} %2) goto %l3\n\tSUB.%# %1,%2,r15\n\tJMPLO %l3");

    case GT:
      /* Reversed operand version of LT. */
      return ("// if (%1 > %2) goto %l3\n\tSUB.%# %2,%1,r15\n\tJMPLT %l3");

    case GTU:
      /* Reverse an LTU. */
      return ("// if (%1 >{U} %2) goto %l3\n\tSUB.%# %2,%1,r15\n\tJMPLO %l3");

    default:
      gcc_unreachable();
    }
}

/* Output a compare-and-branch instruction (matching the cbranch
   pattern). This function is current unused since the cbranch
   split is disabled. The function is kept around so we can use
   it when we understand how to do cbranch split safely. */
const char *
picochip_output_compare (rtx operands[])
{
  int code;

  if (HImode != GET_MODE (operands[1]) ||
      (HImode != GET_MODE (operands[2]) &&
       GET_CODE (operands[2]) != CONST_INT))
    {
      internal_error ("%s: at least one operand can%'t be handled",
		      __FUNCTION__);
    }

  code = GET_CODE (operands[0]);
  /* Use the type of comparison to output the appropriate condition
     test. */
  switch (code)
    {
    case NE:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case EQ:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case LE:
      /* Reverse the operand order to be GE */
      return ("SUB.%# %2,%1,r15\t// CC := (%0)");

    case LEU:
      /* Reverse operand order of GEU. */
      return ("SUB.%# %2,%1,r15\t// CC := (%0)");

    case GE:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case GEU:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case LT:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case LTU:
      return ("SUB.%# %1,%2,r15\t// CC := (%0)");

    case GT:
      /* Reversed operand version of LT. */
      return ("SUB.%# %2,%1,r15\t// CC := (%0)");

    case GTU:
      /* Reverse an LTU. */
      return ("SUB.%# %2,%1,r15\t// CC := (%0)");

    default:
      gcc_unreachable();
    }
}

/* Output the branch insn part of a compare-and-branch split. */
const char *
picochip_output_branch (rtx operands[], rtx insn)
{

  int code = GET_CODE(operands[2]);
  if (picochip_is_short_branch (insn))
    {
      /* Short branches can be output directly using the
         appropriate instruction. */
      switch (code)
	{
	case NE:
	  return ("BNE %l0 %>");
	case EQ:
	  return ("BEQ %l0 %>");
	case LE:
	  return ("BGE %l0 %>");
	case LEU:
	  return ("BHS %l0 %>");
	case GE:
	  return ("BGE %l0 %>");
	case GEU:
	  return ("BHS %l0 %>");
	case LT:
	  return ("BLT %l0 %>");
	case LTU:
	  return ("BLO %l0 %>");
	case GT:
	  return ("BLT %l0 %>");
	case GTU:
	  return ("BLO %l0 %>");
	default:
	  internal_error ("unknown short branch in %s (type %d)",
			  __FUNCTION__, (int) INTVAL (operands[1]));
	  return "UNKNOWN_BRANCH";
	}
    }
  else
    {
      /* Long branches result in the emission of a special
         instruction, which the assembler expands into a suitable long
         branch. */

      /* Use the type of comparison to output the appropriate condition
         test. */
      switch (code)
	{
	case NE:
	  return ("JMPNE %l0 %>");
	case EQ:
	  return ("JMPEQ %l0 %>");
	case LE:
	  return ("JMPGE %l0 %>");
	case LEU:
	  return ("JMPHS %l0 %>");
	case GE:
	  return ("JMPGE %l0 %>");
	case GEU:
	  return ("JMPHS %l0 %>");
	case LT:
	  return ("JMPLT %l0 %>");
	case LTU:
	  return ("JMPLO %l0 %>");
	case GT:
	  return ("JMPLT %l0 %>");
	case GTU:
	  return ("JMPLO %l0 %>");

	default:
	  internal_error ("unknown long branch in %s (type %d)",
			  __FUNCTION__, (int) INTVAL (operands[1]));
	  return "UNKNOWN_BRANCH";
	}

    }
}

/* Output a jump instruction. */
const char *
picochip_output_jump (rtx insn)
{
  if (picochip_is_short_branch (insn))
    return "BRA %l0%>";
  else
    return "JMPRA %l0%>";
}

const char *
picochip_output_put_array (int alternative, rtx operands[])
{
  /* Local output buffer. */
  char buf[256];

  int portArraySize = INTVAL(operands[1]);
  int portBaseIndex = INTVAL(operands[2]);

  if (alternative == 0)
    {
      sprintf (buf, "// Array put\n\tadd.0 [lsl %%0,2],&__commTable_put_%d_%d,lr\n\tjl (lr)",
	       portArraySize, portBaseIndex);
      output_asm_insn (buf, operands);
    }
  else if (alternative == 1)
    {
      /* Constant port id. Emit a real instruction. */
      int portIndex = INTVAL(operands[0]) + portBaseIndex;
      if (portIndex < portBaseIndex ||
	  portIndex >= (portBaseIndex + portArraySize))
	{
	  error ("PUT uses port array index %d, which is out of range [%d..%d)",
		 portIndex, portBaseIndex, portBaseIndex + portArraySize);
	}
      sprintf(buf, "PUT R[0:1],%d", portIndex);
      output_asm_insn (buf, operands);
    }
  else
    gcc_unreachable();

  /* Both alternatives output the insn directly. */
  return "";
}

const char *picochip_output_get_array (int alternative, rtx operands[])
{
  /* Local output buffer. */
  char buf[256];

  int portArraySize = INTVAL(operands[1]);
  int portBaseIndex = INTVAL(operands[2]);

  if (alternative == 0)
    {
      sprintf (buf, "// Array get\n\tadd.0 [lsl %%0,2],&__commTable_get_%d_%d,lr\n\tjl (lr)",
	       portArraySize, portBaseIndex);
      output_asm_insn (buf, operands);
    }
  else if (alternative == 1)
    {
      /* Constant port id. Emit a real instruction. */
      int portIndex = INTVAL(operands[0]) + portBaseIndex;
      if (portIndex < portBaseIndex ||
	  portIndex >= (portBaseIndex + portArraySize))
	{
	  error ("GET uses port array index %d, which is out of range [%d..%d)",
		 portIndex, portBaseIndex, portBaseIndex + portArraySize);
	}
      sprintf(buf, "GET %d,R[0:1]", portIndex);
      output_asm_insn (buf, operands);
    }
  else
    gcc_unreachable();

  /* Both alternatives output the insn directly. */
  return "";
}

const char *picochip_output_testport_array (int alternative, rtx operands[])
{
  /* Local output buffer. */
  char buf[256];

  int portArraySize = INTVAL(operands[2]);
  int portBaseIndex = INTVAL(operands[3]);

  if (alternative == 0)
    {
      sprintf (buf, "// Array tstport\n\tadd.0 [lsl %%1,2],&__commTable_tstport_%d_%d,lr\n\tjl (lr)\n=->\tcopy.0 0,%%0\n\tcopyeq 1,%%0",
	       portArraySize, portBaseIndex);
      output_asm_insn (buf, operands);
    }
  else if (alternative == 1)
    {
      /* Constant port id. Emit a real instruction. */
      int portIndex = INTVAL(operands[1]) + portBaseIndex;
      if (portIndex < portBaseIndex ||
	  portIndex >= (portBaseIndex + portArraySize))
	{
	  error ("PUT uses port array index %d, which is out of range [%d..%d)",
		 portIndex, portBaseIndex, portBaseIndex + portArraySize);
	}
      sprintf(buf, "copy.1 0,%%0 %%| TSTPORT %d\n\tcopyeq 1,%%0", portIndex);
      output_asm_insn (buf, operands);
    }
  else
    gcc_unreachable();

  /* Both alternatives output the insn directly. */
  return "";
}

/* Output a comparison operand as a symbol (e.g., >). */
static void
picochip_print_comparison (FILE * file, rtx operand, int letter)
{

  if (letter == 'i')
    {
      /* Output just the comparison symbol. */
      switch (GET_CODE (operand))
	{
	case NE:
	  fprintf (file, "!=");
	  break;
	case EQ:
	  fprintf (file, "==");
	  break;
	case GE:
	  fprintf (file, ">=");
	  break;
	case GEU:
	  fprintf (file, ">={U}");
	  break;
	case LT:
	  fprintf (file, "<");
	  break;
	case LTU:
	  fprintf (file, "<{U}");
	  break;
	case LE:
	  fprintf (file, "<=");
	  break;
	case LEU:
	  fprintf (file, "<={U}");
	  break;
	case GT:
	  fprintf (file, ">");
	  break;
	case GTU:
	  fprintf (file, ">{U}");
	  break;
	default:
	  gcc_unreachable();
	}
    }
  else
    {
      /* Output the comparison formatted as operand,symbol,operand */
      rtx op0 = XEXP (operand, 0);
      rtx op1 = XEXP (operand, 1);

      picochip_print_operand (file, op0, 0);
      picochip_print_comparison (file, operand, 'i');
      picochip_print_operand (file, op1, 0);
    }
}

/* This function generates a memory address operand in the given
   mode.  That is, if the address contains a constant offset, then the
   offset is divided by the required mode size to compute the
   mode specific offset.  By default, picochip_print_operand_address calls
   this function using the natural mode of the operand, but special
   operand codes can be used to invoke the computation using an
   unnatural mode (e.g., compute the HI aligned address of an SI mode
   address). */
static void
picochip_print_memory_address (FILE * file, rtx operand,
			       enum machine_mode mode)
{
  rtx address = XEXP (operand, 0);

  /* Sanity check. */
  if (MEM != GET_CODE (operand))
    fatal_insn ("picochip_print_memory_address - Operand isn't memory based",
		operand);

  if (TARGET_DEBUG)
    {
      printf ("picochip_print_memory_address: ");
      print_rtl (stdout, operand);
      printf ("\n");
    }

  switch (GET_CODE (address))
    {
    case PLUS:
      {
	/* Grab the address components. */
	rtx base = XEXP (address, 0);
	rtx offset = XEXP (address, 1);

	/* Only handle reg+const addresses */
	if (REG == GET_CODE (base) && CONST_INT == GET_CODE (offset))
	  {
	    /* Sanity check.  If an FP+offset address is given, ensure
	       that the offset lies within the given frame, or a lower
	       frame. */
	    if (REGNO (base) == STACK_POINTER_REGNUM )
              gcc_assert (INTVAL (offset) <= (picochip_arg_area_byte_offset () +
                          crtl->args.size));

	    /* Print the base register - identical for all modes. */
	    fprintf (file, "(");
	    picochip_print_operand (file, base, 'r');
	    fprintf (file, ")");

	    /* Print the constant offset with compensation for the mode. */
	    switch (mode)
	      {
	      case QImode:
		picochip_print_operand (file, offset, 'Q');
		break;

	      case HImode:
		picochip_print_operand (file, offset, 'H');
		break;

	      case SImode:
	      case SFmode:
		picochip_print_operand (file, offset, 'S');
		break;

	      case DImode:
		picochip_print_operand (file, offset, 'D');
		break;

	      default:
	        gcc_unreachable();
	      }

	  }

      }

      break;

    case SYMBOL_REF:
      picochip_print_operand (file, address, 's');
      break;

    case CONST:
      {
	rtx inner;
	rtx base;
	rtx offset;

	inner = XEXP (address, 0);

	/* Sanity check - the CONST memory address must be a base+offset. */
	gcc_assert (PLUS == GET_CODE (inner));

	base = XEXP (inner, 0);
	offset = XEXP (inner, 1);

	fprintf (file, "&_%s%+d", XSTR (base, 0), XINT (offset, 0));

	break;
      }

    case REG:
      /* Register operand. Provide a zero offset. */
      fprintf (file, "(");
      picochip_print_operand (file, address, 'r');
      fprintf (file, ")0");
      break;

    default:
      gcc_unreachable();
    }

}

/* Output an operand.  Formatting letters allow particular parts of
   the operand to be output. */
void
picochip_print_operand (FILE * file, rtx operand, int letter)
{

  /* Handle special cases. */
  switch (letter)
    {
      /* VLIW continuation, for explicit VLIW sequences. */
    case '|':
      fprintf (file, "\\");
      return;

      /* ALU selector.  */
    case '#':
      fputc (picochip_get_vliw_alu_id (), file);
      return;

      /* Delay slot specifier. */
    case '>':
      /* This should be handled in asm_output_opcode. */
      gcc_unreachable();

      /* Instruction mnemonics (e.g., lshift becomes LSL). */
    case 'I':
      switch (GET_CODE (operand))
	{
	case AND:
	  fprintf (file, "AND");
	  break;
	case IOR:
	  fprintf (file, "OR");
	  break;
	case XOR:
	  fprintf (file, "XOR");
	  break;
	case PLUS:
	  fprintf (file, "ADD");
	  break;
	case MINUS:
	  fprintf (file, "SUB");
	  break;
	default:
	  gcc_unreachable();
	}
      return;

      /* Symbolic instructions (e.g., lshift becomes <<). */
    case 'i':
      switch (GET_CODE (operand))
	{
	case AND:
	  fprintf (file, "&");
	  break;
	case IOR:
	  fprintf (file, "|");
	  break;
	case XOR:
	  fprintf (file, "^");
	  break;
	case PLUS:
	  fprintf (file, "+");
	  break;
	case MINUS:
	  fprintf (file, "-");
	  break;
	default:
	  fprintf (file, "UNKNOWN_INSN");
	  break;
	}
      return;

    default:			/* Not a punctuation character - process as normal. */
      break;
    }

  switch (GET_CODE (operand))
    {
    case REG:
      switch (letter)
	{
	case 'R':
	  /* Write a range of registers. */
	  fprintf (file, "R[%d:%d]", REGNO (operand) + 1, REGNO (operand));
	  break;

	case 'U':
	  /* The upper register of a pair is requested. */
	  fprintf (file, "%s", picochip_regnames[REGNO (operand) + 1]);
	  break;

	case 'L':
	  /* The lower register of a pair is requested. Equivalent to the
	     default, but included for completeness. */
	  fprintf (file, "%s", picochip_regnames[REGNO (operand)]);
	  break;

	case 'X':
	  /* The 3rd register of a DI mode register. */
	  fprintf (file, "%s", picochip_regnames[REGNO (operand) + 2]);
	  break;

	case 'Y':
	  /* The 4th register of a DI mode register. */
	  fprintf (file, "%s", picochip_regnames[REGNO (operand) + 3]);
	  break;

	default:
	  fprintf (file, "%s", picochip_regnames[REGNO (operand)]);
	}
      break;

    case CONST_INT:
      /* A range of letters can be used to format integers.  The
         letters Q/H/S are used to divide the constant by the width of
         QI/HI/SI mode integers in bytes.  The U/L modifiers are used
         to obtain the upper and lower 16-bits of a 32-bit
         constant.  Where possible, signed numbers are used, since
         signed representations of numbers may be more compact (e.g.,
         65535 can be represented as -1, which fits into a small
         constant, whereas 65535 requires a large constant). */
      switch (letter)
	{
	case 'Q':
	  fprintf (file, "%ld", INTVAL (operand));
	  break;

	case 'H':
	  fprintf (file, "%ld", INTVAL (operand) / 2);
	  break;

	case 'S':
	  fprintf (file, "%ld", INTVAL (operand) / 4);
	  break;

	case 'P':
	  fprintf (file, "%d", exact_log2 (INTVAL(operand)));
	  break;

	case 'U':
	  fprintf (file, "%hi", (short) ((INTVAL (operand) >> 16) & 0xFFFF));
	  break;

	case 'L':
	  fprintf (file, "%hi", (short) (INTVAL (operand) & 0xFFFF));
	  break;

	default:
	  fprintf (file, "%ld", INTVAL (operand));
	  break;
	}
      break;

    case CONST_DOUBLE:
      {
	long val;
	REAL_VALUE_TYPE rv;

	if (GET_MODE (operand) != SFmode)
	  fatal_insn ("Unknown mode in print_operand (CONST_DOUBLE) :",
		      operand);
	REAL_VALUE_FROM_CONST_DOUBLE (rv, operand);
	REAL_VALUE_TO_TARGET_SINGLE (rv, val);

	switch (letter)
	  {
	  case 'U':
	    fprintf (file, "%hi", (short) ((val >> 16) & 0xFFFF));
	    break;

	  case 'L':
	    fprintf (file, "%hi", (short) (val & 0xFFFF));
	    break;
	  }

	break;

      }

      /* Output a symbol.  The output format must match that of
         picochip_output_label. */
    case SYMBOL_REF:
      /* Ensure that the symbol is marked as referenced.  Gcc can
         occasionally omit the function bodies when it believes them
         to be unreferenced. */
      if (SYMBOL_REF_DECL (operand))
	mark_decl_referenced (SYMBOL_REF_DECL (operand));
      fprintf (file, "&");
      assemble_name (file, XSTR (operand, 0));
      break;

    case LABEL_REF:
      /* This format must match that of picochip_output_label. */
      fprintf (file, "&");
      output_asm_label (operand);
      break;

    case MEM:
      {
	rtx addr = XEXP (operand, 0);

	switch (letter)
	  {
	  case 'o':
	    if (PLUS != GET_CODE (addr))
	      fatal_insn ("Bad address, not (reg+disp):", addr);
	    else
	      picochip_print_operand (file, XEXP (addr, 1), 0);
	    break;

	  case 'M':
	    /* Output a memory address in byte mode notation (i.e., the
	       constant address (if any) is the actual byte address. */
	    picochip_print_memory_address (file, operand, QImode);
	    break;

	    /* Output a constant offset of the given mode (i.e., divide
	       the constant by the number of units in the mode to get the
	       constant). */
	  case 'Q':
	    picochip_print_memory_address (file, operand, QImode);
	    break;

	  case 'H':
	    picochip_print_memory_address (file, operand, HImode);
	    break;

	  case 'S':
	    picochip_print_memory_address (file, operand, SImode);
	    break;

	  case 'F':
	    picochip_print_memory_address (file, operand, SFmode);
	    break;

	  case 'b':
	    if (PLUS != GET_CODE (addr))
	      fatal_insn ("Bad address, not (reg+disp):", addr);
	    else
	      picochip_print_operand (file, XEXP (addr, 0), 0);
	    break;

          /* When the mem operand is (reg + big offset) which cannot
            be represented in an instruction as operand, the compiler
            automatically generates the instruction to put in (reg +
            big offset) into another register. In such cases, it
            returns '0' as the character. This needs to be handled
            as well. */
	  case 0:
	  case 'r':
	    if (REG != GET_CODE (addr))
	      fatal_insn ("Bad address, not register:", addr);
	    else
	      picochip_print_operand (file, addr, 0);
	    break;

	  default:
	    fprintf (file, "Unknown mem operand - letter %c ",
		     (char) (letter));
	    print_rtl (file, operand);
	  }

	break;
      }

    case CONST:
      {
	rtx const_exp = XEXP (operand, 0);

	/* Handle constant offsets to symbol references. */
	if (PLUS == GET_CODE (const_exp) &&
	    SYMBOL_REF == GET_CODE (XEXP (const_exp, 0)) &&
	    CONST_INT == GET_CODE (XEXP (const_exp, 1)))
	  {

	    picochip_print_operand (file, XEXP (const_exp, 0), 0);
	    if (INTVAL (XEXP (const_exp, 1)) >= 0)
	      fprintf (file, "+");
	    /* else use the - from the operand (i.e., AP-2)) */

	    picochip_print_operand (file, XEXP (const_exp, 1), letter);

	  }
      }
      break;


    case PLUS:
      {
	/* PLUS expressions are of the form (base + offset). Different
	   options (analagous to those of memory PLUS expressions) are used
	   to extract the base and offset components. */

	switch (letter)
	  {
	  case 'b':
	    picochip_print_operand (file, XEXP (operand, 0), 0);
	    break;

	  case 'o':
	    picochip_print_operand (file, XEXP (operand, 1), 0);
	    break;

	  default:

	    /* If the expression is composed entirely of constants,
	       evaluate the result.  This should only occur with the
	       picoChip specific comms instructions, which are emitted as
	       base+offset expressions. */
	    if (CONST_INT == GET_CODE (XEXP (operand, 0)) &&
		CONST_INT == GET_CODE (XEXP (operand, 1)))
	      {
		HOST_WIDE_INT result = (XINT (XEXP (operand, 0), 0) +
					XINT (XEXP (operand, 1), 0));
		fprintf (file, "%ld", result);
	      }
	    else
	      {
		fprintf (file, "(");
		picochip_print_operand (file, XEXP (operand, 0), 0);
		fprintf (file, "+");
		picochip_print_operand (file, XEXP (operand, 1), 0);
		fprintf (file, ")");
	      }
	  }

	break;
      }

      /* Comparison operations. */
    case NE:
    case EQ:
    case GE:
    case GEU:
    case LT:
    case LTU:
    case LE:
    case LEU:
    case GT:
    case GTU:
      picochip_print_comparison (file, operand, letter);
      return;

    default:
      fprintf (stderr, "Unknown operand encountered in %s\n", __FUNCTION__);
      print_rtl (file, operand);
      break;

    }

}

/* Output an operand address */
void
picochip_print_operand_address (FILE * file, rtx operand)
{

  switch (GET_CODE (operand))
    {

    case SYMBOL_REF:
      /* This format must match that of picochip_output_label. */
      assemble_name (file, XSTR (operand, 0));
      break;

    case CODE_LABEL:
      /* Note  this format must match that of picochip_output_label. */
      fprintf (file, "_L%d", XINT (operand, 5));
      break;

    case MEM:
      /* Pass on to a specialised memory address generator. */
      picochip_print_memory_address (file, operand, GET_MODE (operand));
      break;

    default:
      gcc_unreachable();

    }

}


/* Scheduling functions. */

/* Save some of the contents of recog_data. */
static void
picochip_save_recog_data (void)
{
  picochip_saved_which_alternative = which_alternative;
  memcpy (&picochip_saved_recog_data, &recog_data,
	  sizeof (struct recog_data_d));
}

/* Restore some of the contents of global variable recog_data. */
static void
picochip_restore_recog_data (void)
{
  which_alternative = picochip_saved_which_alternative;
  memcpy (&recog_data, &picochip_saved_recog_data,
	  sizeof (struct recog_data_d));
}

/* Ensure that no var tracking notes are emitted in the middle of a
   three-instruction bundle.  */
static void
reorder_var_tracking_notes (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx insn, next, last_insn = NULL_RTX;
      rtx queue = NULL_RTX;

      /* Iterate through the bb and find the last non-debug insn */
      for (insn = BB_HEAD (bb); insn != NEXT_INSN(BB_END (bb)); insn = NEXT_INSN(insn))
        {
          if (NONDEBUG_INSN_P(insn))
            last_insn = insn;
        }

      /* In all normal cases, queue up notes and emit them just before a TImode
         instruction. For the last instruction, emit the queued notes just after
         the last instruction. */
      for (insn = BB_HEAD (bb); insn != NEXT_INSN(BB_END (bb)); insn = next)
        {
          next = NEXT_INSN (insn);

          if (insn == last_insn)
            {
              while (queue)
                {
                  rtx next_queue = PREV_INSN (queue);
                  PREV_INSN (NEXT_INSN(insn)) = queue;
                  NEXT_INSN(queue) = NEXT_INSN(insn);
                  PREV_INSN(queue) = insn;
                  NEXT_INSN(insn) = queue;
                  queue = next_queue;
                }
              /* There is no more to do for this bb. break*/
              break;
            }
          else if (NONDEBUG_INSN_P (insn))
            {
              /* Emit queued up notes before the first instruction of a bundle.  */
              if (GET_MODE (insn) == TImode)
                {
                  while (queue)
                    {
                      rtx next_queue = PREV_INSN (queue);
                      NEXT_INSN (PREV_INSN(insn)) = queue;
                      PREV_INSN (queue) = PREV_INSN(insn);
                      PREV_INSN (insn) = queue;
                      NEXT_INSN (queue) = insn;
                      queue = next_queue;
                    }
                }
            }
          else if (NOTE_P (insn))
            {
               rtx prev = PREV_INSN (insn);
               PREV_INSN (next) = prev;
               NEXT_INSN (prev) = next;
               /* Ignore call_arg notes. They are expected to be just after the
                  call insn. If the call is start of a long VLIW, labels are
                  emitted in the middle of a VLIW, which our assembler can not
                  handle. */
               if (NOTE_KIND (insn) != NOTE_INSN_CALL_ARG_LOCATION)
                 {
                   PREV_INSN (insn) = queue;
                   queue = insn;
                 }
            }
        }
        /* Make sure we are not dropping debug instructions.*/
        gcc_assert (queue == NULL_RTX);
    }
}

/* Perform machine dependent operations on the rtl chain INSNS. */
void
picochip_reorg (void)
{
  rtx insn, insn1, vliw_start = NULL_RTX;
  int vliw_insn_location = 0;

  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  if (optimize == 0)
    split_all_insns ();

  if (picochip_schedule_type != DFA_TYPE_NONE)
    {
      timevar_push (TV_SCHED2);

      /* Process the instruction list, computing the sizes of each
         instruction, and consequently branch distances.  This can
         result in some branches becoming short enough to be treated
         as a real branch instruction, rather than an assembly branch
         macro which may expand into multiple instructions.  The
         benefit of shortening branches is that real branch
         instructions can be properly DFA scheduled, whereas macro
         branches cannot. */
      shorten_branches (get_insns ());

      /* Do control and data sched analysis again,
         and write some more of the results to dump file. */

      split_all_insns ();

      schedule_ebbs ();

      timevar_pop (TV_SCHED2);

      ggc_collect ();

      if (picochip_schedule_type == DFA_TYPE_SPEED)
	{
	  /* Whenever a VLIW packet is generated, all instructions in
	     that packet must appear to come from the same source
	     location.  The following code finds all the VLIW packets,
	     and tags their instructions with the location of the first
	     instruction from the packet.  Clearly this will result in
	     strange behaviour when debugging the code, but since
	     debugging and optimisation are being used in conjunction,
	     strange behaviour is certain to occur anyway. */
          /* Slight bit of change. If the vliw set contains a branch
             or call instruction, we pick its location.*/
	  for (insn = get_insns (); insn; insn = next_real_insn (insn))
	    {

	      /* If this is the first instruction in the VLIW packet,
	         extract its location. */
              if (GET_MODE (insn) == TImode)
              {
                vliw_start = insn;
                vliw_insn_location = INSN_LOCATION (insn);
              }
              if (JUMP_P (insn) || CALL_P(insn))
              {
                vliw_insn_location = INSN_LOCATION (insn);
                for (insn1 = vliw_start; insn1 != insn ; insn1 = next_real_insn (insn1))
                  INSN_LOCATION (insn1) = vliw_insn_location;
              }
              /* Tag subsequent instructions with the same location. */
              INSN_LOCATION (insn) = vliw_insn_location;
	    }
	}

    }

  /* Locate the note marking the end of the function's prologue.  If
     the note appears in the middle of a VLIW packet, move the note to
     the end.  This avoids unpleasant consequences such as trying to
     emit prologue markers (e.g., .loc/.file directives) in the middle
     of VLIW packets. */
  if (picochip_schedule_type == DFA_TYPE_SPEED)
    {
      rtx prologue_end_note = NULL;
      rtx last_insn_in_packet = NULL;

      for (insn = get_insns (); insn; insn = next_insn (insn))
	{
	  /* The prologue end must be moved to the end of the VLIW packet. */
	  if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_PROLOGUE_END)
	    {
	      prologue_end_note = insn;
	      break;
	    }
	}

      /* Find the last instruction in this packet. */
      for (insn = prologue_end_note; insn; insn = next_real_insn (insn))
	{
	  if (GET_MODE (insn) == TImode)
	    break;
	  else
	    last_insn_in_packet = insn;
	}

      if (last_insn_in_packet != NULL)
	{
          rtx tmp_note
	    = emit_note_after ((enum insn_note) NOTE_KIND (prologue_end_note),
			       last_insn_in_packet);
          memcpy(&NOTE_DATA (tmp_note), &NOTE_DATA(prologue_end_note), sizeof(NOTE_DATA(prologue_end_note)));
	  delete_insn (prologue_end_note);
	}
    }

  if (flag_var_tracking)
    {
      timevar_push (TV_VAR_TRACKING);
      variable_tracking_main ();
      /* We also have to deal with variable tracking notes in the
	 middle of VLIW packets. */
      reorder_var_tracking_notes();
      timevar_pop (TV_VAR_TRACKING);
    }
}

/* Return the ALU character identifier for the current
   instruction.  This will be 0 or 1. */
static char
picochip_get_vliw_alu_id (void)
{
  int attr_type = 0;

  /* Always use ALU 0 if VLIW scheduling is disabled. */
  if (picochip_schedule_type != DFA_TYPE_SPEED)
    return '0';

  /* Get the attribute type of the instruction.  Note that this can
     ruin the contents of recog_data, so save/restore around the
     call. */
  picochip_save_recog_data ();
  attr_type = get_attr_type (picochip_current_prescan_insn);
  picochip_restore_recog_data ();

  if (picochip_current_vliw_state.contains_pico_alu_insn)
    {

      /* If this a picoAlu insn? If it is, then stuff it into ALU 0,
         else it must be the other ALU (either basic or nonCc)
         instruction which goes into 1. */
      if (attr_type == TYPE_PICOALU)
	return '0';
      else
	return '1';

    }
  else if (picochip_current_vliw_state.contains_non_cc_alu_insn)
    {
      /* Is this the non CC instruction? If it is, then stuff it into
         ALU 1, else it must be a picoAlu or basicAlu, in which case
         it goes into ALU 0. */
      if (attr_type == TYPE_NONCCALU)
	return '1';
      else
	return '0';
    }
  else
    {
      /* No picoAlu/nonCc instructions in use, so purely dependent upon
         whether an ALU instruction has already been scheduled in this
         cycle. */
      switch (picochip_current_vliw_state.num_alu_insns_so_far)
	{
	case 0:
	  picochip_current_vliw_state.num_alu_insns_so_far++;
	  return '0';

	case 1:
	  picochip_current_vliw_state.num_alu_insns_so_far++;
	  return '1';

	default:
	  internal_error ("too many ALU instructions emitted (%d)",
			  picochip_current_vliw_state.num_alu_insns_so_far);
	  return 'X';
	}
    }

}

/* Reset any information about the current VLIW packing status. */
static void
picochip_reset_vliw (rtx insn)
{
  rtx local_insn = insn;

  /* Nothing to do if VLIW scheduling isn't being used. */
  if (picochip_schedule_type != DFA_TYPE_SPEED)
    return;

  if (TARGET_DEBUG)
    printf ("%s on insn %d\n", __FUNCTION__, INSN_UID (insn));

  /* Reset. */
  picochip_current_vliw_state.contains_pico_alu_insn = 0;
  picochip_current_vliw_state.contains_non_cc_alu_insn = 0;
  picochip_current_vliw_state.num_alu_insns_so_far = 0;
  picochip_current_vliw_state.num_cfi_labels_deferred = 0;
  picochip_current_vliw_state.lm_label_name[0] = 0;
  picochip_current_vliw_state.num_insns_in_packet = 0;

  /* Read through the VLIW packet, classifying the instructions where
     appropriate. */
  local_insn = insn;
  do
    {
      if (NOTE_P (local_insn) || DEBUG_INSN_P(local_insn))
	{
	  local_insn = NEXT_INSN (local_insn);
	  continue;
	}
      else if (!INSN_P (local_insn))
	break;
      else
	{
	  /* It is an instruction, but is it ours? */
	  if (INSN_CODE (local_insn) != -1)
	    {
	      int attr_type = 0;

	      picochip_current_vliw_state.num_insns_in_packet += 1;

	      /* Is it a picoAlu or nonCcAlu instruction? Note that the
	         get_attr_type function can overwrite the values in
	         the recog_data global, hence this is saved and
	         restored around the call.  Not doing so results in
	         asm_output_opcode being called with a different
	         instruction to final_prescan_insn, which is fatal. */
	      picochip_save_recog_data ();
	      attr_type = get_attr_type (local_insn);
	      picochip_restore_recog_data ();

	      if (attr_type == TYPE_PICOALU)
		picochip_current_vliw_state.contains_pico_alu_insn = 1;
	      if (attr_type == TYPE_NONCCALU)
		picochip_current_vliw_state.contains_non_cc_alu_insn = 1;

	    }
	}

      /* Get the next instruction. */
      local_insn = NEXT_INSN (local_insn);

      /* Keep going while the next instruction is part of the same
         VLIW packet (i.e., its a valid instruction and doesn't mark
         the start of a new VLIW packet. */
    }
  while (local_insn &&
	 (GET_MODE (local_insn) != TImode) && (INSN_CODE (local_insn) != -1));

}

int
picochip_sched_reorder (FILE * file, int verbose,
			rtx * ready ATTRIBUTE_UNUSED,
			int *n_readyp ATTRIBUTE_UNUSED, int clock)
{

  if (verbose > 0)
    fprintf (file, ";;\tClock %d\n", clock);

  return picochip_sched_issue_rate ();

}

int
picochip_sched_lookahead (void)
{
  /* It should always be enough to lookahead by 2 insns. Only slot0/1 could
     have a conflict. */
  return 2;
}

int
picochip_sched_issue_rate (void)
{
  return 3;
}

/* Adjust the scheduling cost between the two given instructions,
   which have the given dependency. */
int
picochip_sched_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{

  if (TARGET_DEBUG)
    {
      printf ("Sched Adjust Cost: %d->%d is %d\n",
	      INSN_UID (insn), INSN_UID (dep_insn), cost);

      printf ("  Dependency type:");
      switch (REG_NOTE_KIND (link))
	{
	case 0:
	  printf ("Data\n");
	  break;
	case REG_DEP_ANTI:
	  printf ("ANTI\n");
	  break;
	case REG_DEP_OUTPUT:
	  printf ("OUTPUT\n");
	  break;
	default:
	  printf ("Unknown (%d)\n", REG_NOTE_KIND (link));
	}
    }

  /* Anti-dependencies are used to enforce the ordering between a
   * branch, and any subsequent instructions.  For example:
   *
   *   BNE someLabel
   *   ADD.0 r0,r1,r2
   *
   * The ADD instruction must execute after the branch, and this is
   * enforced using an anti-dependency.  Unfortunately, VLIW machines
   * are happy to execute anti-dependent instructions in the same
   * cycle, which then results in a schedule like the following being
   * created:
   *
   *    BNE someLabel \ ADD.0 r0,r1,r2
   *
   * The instruction which would normally be conditionally executed
   * depending upon the outcome of the branch, is now unconditionally
   * executed every time.  To prevent this happening, any
   * anti-dependencies between a branch and another instruction are
   * promoted to become real dependencies.
   */
  if ((JUMP_P (dep_insn) || CALL_P(dep_insn)) && REG_NOTE_KIND (link) == REG_DEP_ANTI)
    {

      if (TARGET_DEBUG)
	printf ("Promoting anti-dependency %d->%d to a true-dependency\n",
		INSN_UID (insn), INSN_UID (dep_insn));

      return 1;
    }

  return cost;

}

/* Return the minimum of the two values */
static int
minimum (int a, int b)
{
  if (a < b)
    return a;
  if (b < a)
    return b;
  /* I dont expect to get to this function with a==b.*/
  gcc_unreachable();
}


/* This function checks if the memory of the two stores are just off by 2 bytes.
   It returns the lower memory operand's index.*/

static int
memory_just_off (rtx opnd1, rtx opnd2)
{
  int offset1 = 0, offset2 = 0;
  int reg1, reg2;

  if (GET_CODE(XEXP(opnd1, 0)) == PLUS && GET_CODE(XEXP(XEXP(opnd1, 0),1)) == CONST_INT)
  {
    offset1 = INTVAL(XEXP(XEXP(opnd1, 0), 1));
    reg1 = REGNO(XEXP(XEXP(opnd1, 0), 0));
  }
  else
  {
    reg1 = REGNO(XEXP(opnd1, 0));
  }
  if (GET_CODE(XEXP(opnd2, 0)) == PLUS && GET_CODE(XEXP(XEXP(opnd2, 0), 1)) == CONST_INT)
  {
    offset2 = INTVAL(XEXP(XEXP(opnd2, 0), 1));
    reg2 = REGNO(XEXP(XEXP(opnd2, 0), 0));
  }
  else
  {
    reg2 = REGNO(XEXP(opnd2, 0));
  }

  /* Peepholing 2 STW/LDWs has the restriction that the resulting STL/LDL's address
     should be 4 byte aligned. We can currently guarantee that only if the base
     address is FP(R13) and the offset is aligned. */

  if (reg1 == reg2 && reg1 == 13 && abs(offset1-offset2) == 2 && minimum(offset1, offset2) % 4 == 0)
    return (minimum(offset1, offset2) == offset1) ? 1:2;

  return 0;
}

static int
registers_just_off (rtx opnd1, rtx opnd2)
{
  int reg1, reg2;
  reg1 = REGNO(opnd1);
  reg2 = REGNO(opnd2);
  if (abs(reg1-reg2) == 1 && minimum(reg1, reg2) % 2 == 0)
    return (minimum(reg1, reg2) == reg1)?1:2;
  return 0;
}

/* Check to see if the two LDWs can be peepholed together into a LDL
   They can be if the registers getting loaded into are contiguous
   and the memory addresses are contiguous as well.
   for eg.
           LDW r2,[r11]x
           LDW r3,[r11]x+1
   can be merged together into
           LDL r[3:2],[r11]

   NOTE:
   1. The LDWs themselves only guarantee that r11 will be a 2-byte
   aligned address. Only FP can be assumed to be 4 byte aligned.
   2. The progression of addresses and the register numbers should
   be similar. For eg., if you swap r2 and r3 in the above instructions,
   the resultant pair cannot be merged.

*/
bool
ok_to_peephole_ldw(rtx opnd0, rtx opnd1, rtx opnd2, rtx opnd3)
{
  int memtest=0,regtest=0;
  regtest = registers_just_off(opnd1,opnd3);
  if (regtest == 0)
    return false;

  memtest = memory_just_off(opnd0,opnd2);
  if (memtest == 0)
    return false;

  if (regtest == memtest)
  {
    return true;
  }
  return false;
}

/* Similar to LDW peephole */
bool
ok_to_peephole_stw(rtx opnd0, rtx opnd1, rtx opnd2, rtx opnd3)
{
  int memtest=0,regtest=0;
  regtest = registers_just_off(opnd1,opnd3);
  if (regtest == 0)
    return false;

  memtest = memory_just_off(opnd0,opnd2);
  if (memtest == 0)
    return false;

  if (regtest == memtest)
  {
    return true;
  }
  return false;
}


/* Generate a SImode register with the register number that is the smaller of the two */
rtx
gen_min_reg(rtx opnd1,rtx opnd2)
{
  return gen_rtx_REG (SImode, minimum(REGNO(opnd1),REGNO(opnd2)));
}

/* Generate a SImode memory with the address that is the smaller of the two */
rtx
gen_SImode_mem(rtx opnd1,rtx opnd2)
{
  int offset1=0,offset2=0;
  rtx reg;
  rtx address;
  if (GET_CODE(XEXP(opnd1,0)) == PLUS && GET_CODE(XEXP(XEXP(opnd1,0),1)) == CONST_INT)
  {
    offset1 = INTVAL(XEXP(XEXP(opnd1,0),1));
    reg = XEXP(XEXP(opnd1,0),0);
  }
  else
  {
    reg = XEXP(opnd1,0);
  }
  if (GET_CODE(XEXP(opnd2,0)) == PLUS && GET_CODE(XEXP(XEXP(opnd2,0),1)) == CONST_INT)
  {
    offset2 = INTVAL(XEXP(XEXP(opnd2,0),1));
  }
  address = gen_rtx_PLUS (HImode, reg, GEN_INT(minimum(offset1,offset2)));
  return gen_rtx_MEM(SImode,address);
}

bool
picochip_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		    int opno ATTRIBUTE_UNUSED, int* total, bool speed)
{

  int localTotal = 0;

  if (!speed)
  {
    /* Need to penalize immediates that need to be encoded as long constants.*/
    if (code == CONST_INT && !(INTVAL (x) >= 0 && INTVAL (x) < 16))
    {
        *total = COSTS_N_INSNS(1);
        return true;
    }
  }
  switch (code)
  {
  case SYMBOL_REF:
  case LABEL_REF:
    *total = COSTS_N_INSNS (outer_code != MEM);
    return true;
    break;

  case IF_THEN_ELSE:
    /* if_then_else come out of cbranch instructions. It will get split into
       a condition code generating subtraction and a branch */
    *total = COSTS_N_INSNS (2);
    return true;
    break;

  case AND:
  case IOR:
  case XOR:
    if (GET_MODE(x) == SImode)
      *total = COSTS_N_INSNS (2);
    if (GET_MODE(x) == DImode)
      *total = COSTS_N_INSNS (4);
    return false;

  case MEM:
    /* Byte Memory access on a NO_BYTE_ACCESS machine would be expensive */
    if (GET_MODE(x) == QImode && !TARGET_HAS_BYTE_ACCESS)
      *total = COSTS_N_INSNS (10);

    /* 64-bit accesses have to be done through 2 32-bit access */
    if (GET_MODE(x) == DImode)
      *total = COSTS_N_INSNS (2);
    return false;
    break;

  case ASHIFTRT:

    /* SImode shifts are expensive */
    if (GET_MODE(x) == SImode)
      *total = COSTS_N_INSNS (10);

    /* Register shift by constant is cheap. */
    if ((GET_MODE(x) == QImode || GET_MODE(x) == HImode)
        && GET_CODE(XEXP(x, 0)) == REG
        && GET_CODE(XEXP(x, 1)) == CONST_INT)
      *total = COSTS_N_INSNS (1);
    else
      *total = COSTS_N_INSNS (4);
    return false;
    break;

  case DIV:
  case MOD:

    /* Divisions are more expensive than the default 7*/
    if (GET_MODE(x) == SImode)
      *total = COSTS_N_INSNS (20);
    else
      *total = COSTS_N_INSNS (12);
    return false;
    break;

  case MULT:
    /* Look for the simple cases of multiplying register*register or
       register*constant. */
    if ((GET_MODE(x) == QImode || GET_MODE(x) == HImode)
        && ((GET_CODE(XEXP(x, 0)) == REG
           && (GET_CODE(XEXP(x, 1)) == REG || GET_CODE(XEXP(x,1)) == CONST_INT))
           || (GET_CODE(XEXP(x, 0)) == ZERO_EXTEND 
               && GET_CODE(XEXP(XEXP(x, 0),0)) == REG
               && GET_CODE(XEXP(x, 1)) == ZERO_EXTEND 
               && GET_CODE(XEXP(XEXP(x, 1),0)) == REG)))
      {

        /* When optimising for size, multiplication by constant
           should be discouraged slightly over multiplication by a
           register. */
        if (picochip_has_mac_unit)
          {
            /* Single cycle multiplication, but the result must be
               loaded back into a general register afterwards. */
            *total = COSTS_N_INSNS(2);
            return true;
          }
        else if (picochip_has_mul_unit)
          {
            /* Single cycle multiplication. */
            *total = COSTS_N_INSNS(1);
            return true;
          }
        /* Else no multiply available. Use default cost. */

      }
    break;

  default:
    /* Do nothing. */
    break;
  }

  if (localTotal != 0)
    {
      *total = localTotal;
      return true;
    }
  else
    {
      return false;
    }

}

void
picochip_final_prescan_insn (rtx insn, rtx * opvec ATTRIBUTE_UNUSED,
			     int num_operands ATTRIBUTE_UNUSED)
{
  rtx local_insn;

  picochip_current_prescan_insn = insn;

  if (TARGET_DEBUG)
    printf ("Final prescan on INSN %d with mode %s\n",
	    INSN_UID (insn), GET_MODE_NAME (GET_MODE (insn)));

  /* If this is the start of a new instruction cycle, or no scheduling
     is used, then reset the VLIW status. */
  if (GET_MODE (insn) == TImode || !picochip_schedule_type == DFA_TYPE_SPEED)
    picochip_reset_vliw (insn);

  /* No VLIW scheduling occurred, so don't go any further. */
  if (picochip_schedule_type != DFA_TYPE_SPEED)
    return;

  /* Look for the next printable instruction.  This loop terminates on
     any recognisable instruction, and on any unrecognisable
     instruction with TImode. */
  local_insn = insn;
  for (local_insn = NEXT_INSN (local_insn); local_insn;
       local_insn = NEXT_INSN (local_insn))
    {
      if (NOTE_P (local_insn) || DEBUG_INSN_P(local_insn))
	continue;
      else if (!INSN_P (local_insn))
	break;
      else if (GET_MODE (local_insn) == TImode
	       || INSN_CODE (local_insn) != -1)
	break;
    }

  /* Set the continuation flag if the next instruction can be packed
     with the current instruction (i.e., the next instruction is
     valid, and isn't the start of a new cycle). */
  picochip_vliw_continuation = (local_insn && NONDEBUG_INSN_P (local_insn) &&
				(GET_MODE (local_insn) != TImode));

}

/* Builtin functions. */
/* Given a builtin function taking 2 operands (i.e., target + source),
   emit the RTL for the underlying instruction. */
static rtx
picochip_expand_builtin_2op (enum insn_code icode, tree call, rtx target)
{
  tree arg0;
  rtx op0, pat;
  enum machine_mode tmode, mode0;

  /* Grab the incoming argument and emit its RTL. */
  arg0 = CALL_EXPR_ARG (call, 0);
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* Determine the modes of the instruction operands. */
  tmode = insn_data[icode].operand[0].mode;
  mode0 = insn_data[icode].operand[1].mode;

  /* Ensure that the incoming argument RTL is in a register of the
     correct mode. */
  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  /* If there isn't a suitable target, emit a target register. */
  if (target == 0
      || GET_MODE (target) != tmode
      || !(*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (target, op0);
  if (!pat)
    return 0;
  emit_insn (pat);

  return target;

}

/* Given a builtin function taking 3 operands (i.e., target + two
   source), emit the RTL for the underlying instruction. */
static rtx
picochip_expand_builtin_3op (enum insn_code icode, tree call, rtx target)
{
  tree arg0, arg1;
  rtx op0, op1, pat;
  enum machine_mode tmode, mode0, mode1;

  /* Grab the function's arguments. */
  arg0 = CALL_EXPR_ARG (call, 0);
  arg1 = CALL_EXPR_ARG (call, 1);

  /* Emit rtl sequences for the function arguments. */
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* Get the mode's of each of the instruction operands. */
  tmode = insn_data[icode].operand[0].mode;
  mode0 = insn_data[icode].operand[1].mode;
  mode1 = insn_data[icode].operand[2].mode;

  /* Ensure that each of the function argument rtl sequences are in a
     register of the correct mode. */
  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /* If no target has been given, create a register to use as the target. */
  if (target == 0
      || GET_MODE (target) != tmode
      || !(*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (target, op0, op1);
  if (!pat)
    return 0;
  emit_insn (pat);

  return target;

}

/* Expand a builtin function which takes two arguments, and returns a void. */
static rtx
picochip_expand_builtin_2opvoid (enum insn_code icode, tree call)
{
  tree arg0, arg1;
  rtx op0, op1, pat;
  enum machine_mode mode0, mode1;

  /* Grab the function's arguments. */
  arg0 = CALL_EXPR_ARG (call, 0);
  arg1 = CALL_EXPR_ARG (call, 1);

  /* Emit rtl sequences for the function arguments. */
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* Get the mode's of each of the instruction operands. */
  mode0 = insn_data[icode].operand[0].mode;
  mode1 = insn_data[icode].operand[1].mode;

  /* Ensure that each of the function argument rtl sequences are in a
     register of the correct mode. */
  if (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /* Emit and return the new instruction. */
  pat = GEN_FCN (icode) (op0, op1);
  if (!pat)
    return 0;
  emit_insn (pat);

  return NULL_RTX;

}

/* Expand an array get into the corresponding RTL. */
static rtx
picochip_expand_array_get (tree call, rtx target)
{
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, pat;

  /* Grab the function's arguments. */
  arg0 = CALL_EXPR_ARG (call, 0);
  arg1 = CALL_EXPR_ARG (call, 1);
  arg2 = CALL_EXPR_ARG (call, 2) ;

  /* Emit rtl sequences for the function arguments. */
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op2 = expand_expr (arg2, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* The second and third operands must be constant.  Nothing else will
     do. */
  if (CONST_INT != GET_CODE (op1))
    internal_error ("%s: Second source operand is not a constant",
		    __FUNCTION__);
  if (CONST_INT != GET_CODE (op2))
    internal_error ("%s: Third source operand is not a constant",
		    __FUNCTION__);

  /* If no target has been given, create a register to use as the target. */
  if (target == 0 || GET_MODE (target) != SImode)
    target = gen_reg_rtx (SImode);

  /* The first operand must be a HImode register or a constant.  If it
     isn't, force it into a HImode register. */
  if (GET_MODE (op0) != HImode || REG != GET_CODE (op0))
    op0 = copy_to_mode_reg (HImode, op0);


  /* Emit and return the new instruction. */
  pat = gen_commsArrayGet (target, op0, op1, op2);
  emit_insn (pat);

  return target;

}

/* Expand an array put into the corresponding RTL. */
static rtx
picochip_expand_array_put (tree call, rtx target)
{
  tree arg0, arg1, arg2, arg3;
  rtx op0, op1, op2, op3, pat;

  /* Grab the function's arguments. */
  arg0 = CALL_EXPR_ARG (call, 0);
  arg1 = CALL_EXPR_ARG (call, 1);
  arg2 = CALL_EXPR_ARG (call, 2);
  arg3 = CALL_EXPR_ARG (call, 3);

  /* Emit rtl sequences for the function arguments. */
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op2 = expand_expr (arg2, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op3 = expand_expr (arg3, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* The first operand must be an SImode register. */
  if (GET_MODE (op0) != SImode || REG != GET_CODE (op0))
    op0 = copy_to_mode_reg (SImode, op0);

  /* The second (index) operand must be a HImode register, or a
     constant.  If it isn't, force it into a HImode register. */
  if (GET_MODE (op1) != HImode || REG != GET_CODE (op1))
    op1 = copy_to_mode_reg (HImode, op1);

  /* The remaining operands must be constant.  Nothing else will do. */
  if (CONST_INT != GET_CODE (op2))
    internal_error ("%s: Third source operand is not a constant",
		    __FUNCTION__);
  if (CONST_INT != GET_CODE (op3))
    internal_error ("%s: Fourth source operand is not a constant",
		    __FUNCTION__);

  /* Emit and return the new instruction. */
  pat = gen_commsArrayPut (op0, op1, op2, op3);
  emit_insn (pat);

  return target;

}

/* Expand an array testport into the corresponding RTL. */
static rtx
picochip_expand_array_testport (tree call, rtx target)
{
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, pat;

  /* Grab the function's arguments. */
  arg0 = CALL_EXPR_ARG (call, 0);
  arg1 = CALL_EXPR_ARG (call, 1);
  arg2 = CALL_EXPR_ARG (call, 2);

  /* Emit rtl sequences for the function arguments. */
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  op2 = expand_expr (arg2, NULL_RTX, VOIDmode, EXPAND_NORMAL);

  /* The first operand must be a HImode register, or a constant.  If it
     isn't, force it into a HImode register. */
  if (GET_MODE (op0) != HImode || REG != GET_CODE (op0))
    op0 = copy_to_mode_reg (HImode, op0);

  /* The second and third operands must be constant.  Nothing else will
     do. */
  if (CONST_INT != GET_CODE (op1))
    internal_error ("%s: Second source operand is not a constant",
		    __FUNCTION__);
  if (CONST_INT != GET_CODE (op2))
    internal_error ("%s: Third source operand is not a constant",
		    __FUNCTION__);

  /* If no target has been given, create a HImode register to use as
     the target. */
  if (target == 0 || GET_MODE (target) != HImode)
    target = gen_reg_rtx (HImode);

  /* Emit and return the new instruction. */
  pat = gen_commsArrayTestPort (target, op0, op1, op2);
  emit_insn (pat);

  return target;

}

/* Generate a unique HALT instruction by giving the instruction a
   unique integer. This integer makes no difference to the assembly
   output (other than a comment indicating the supplied id), but the
   presence of the unique integer prevents the compiler from combining
   several different halt instructions into one instruction. This
   means that each use of the halt instruction is unique, which in
   turn means that assertions work as expected. */
static rtx
picochip_generate_halt (void)
{
  static int currentId = 0;
  rtx insns;
  rtx id = GEN_INT (currentId);
  currentId += 1;

  start_sequence();
  emit_insn (gen_halt (id));

  /* A barrier is inserted to prevent the compiler from thinking that
     it has to continue execution after the HALT.*/
  emit_barrier ();

  insns = get_insns();
  end_sequence();
  emit_insn (insns);

  return const0_rtx;
}

/* Initialise the builtin functions.  Start by initialising
   descriptions of different types of functions (e.g., void fn(int),
   int fn(void)), and then use these to define the builtins. */
void
picochip_init_builtins (void)
{
  tree noreturn;

  tree int_ftype_int, int_ftype_int_int;
  tree long_ftype_int, long_ftype_int_int_int;
  tree void_ftype_int_long, int_ftype_int_int_int,
    void_ftype_long_int_int_int;
  tree void_ftype_void, unsigned_ftype_unsigned;

  /* void func (void) */
  void_ftype_void = build_function_type_list (void_type_node, NULL_TREE);

  /* int func (int) */
  int_ftype_int = build_function_type_list (integer_type_node,
					    integer_type_node, NULL_TREE);

  /* unsigned int func (unsigned int) */
  unsigned_ftype_unsigned
    = build_function_type_list (unsigned_type_node,
				unsigned_type_node, NULL_TREE);

  /* int func(int, int) */
  int_ftype_int_int
    = build_function_type_list (integer_type_node,
				integer_type_node, integer_type_node,
				NULL_TREE);

  /* long func(int) */
  long_ftype_int = build_function_type_list (long_integer_type_node,
					     integer_type_node, NULL_TREE);

  /* long func(int, int, int) */
  long_ftype_int_int_int
    = build_function_type_list (long_integer_type_node,
				integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);

  /* int func(int, int, int) */
  int_ftype_int_int_int
    = build_function_type_list (integer_type_node,
				integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);

  /* void func(int, long) */
  void_ftype_int_long
    = build_function_type_list (void_type_node,
				integer_type_node, long_integer_type_node,
				NULL_TREE);

  /* void func(long, int, int, int) */
  void_ftype_long_int_int_int
    = build_function_type_list (void_type_node,
				long_integer_type_node, integer_type_node,
				integer_type_node, integer_type_node,
				NULL_TREE);

  /* Initialise the sign-bit-count function. */
  add_builtin_function ("__builtin_sbc", int_ftype_int,
			       PICOCHIP_BUILTIN_SBC, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("picoSbc", int_ftype_int, PICOCHIP_BUILTIN_SBC,
			       BUILT_IN_MD, NULL, NULL_TREE);

  /* Initialise the bit reverse function. */
  add_builtin_function ("__builtin_brev", unsigned_ftype_unsigned,
			       PICOCHIP_BUILTIN_BREV, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("picoBrev", unsigned_ftype_unsigned,
			       PICOCHIP_BUILTIN_BREV, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Initialise the byte swap function. */
  add_builtin_function ("__builtin_byteswap", unsigned_ftype_unsigned,
			       PICOCHIP_BUILTIN_BYTESWAP, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("picoByteSwap", unsigned_ftype_unsigned,
			       PICOCHIP_BUILTIN_BYTESWAP, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Initialise the ASRI function (note that while this can be coded
     using a signed shift in C, extra scratch registers are required,
     which we avoid by having a direct builtin to map to the
     instruction). */
  add_builtin_function ("__builtin_asri", int_ftype_int_int,
			       PICOCHIP_BUILTIN_ASRI, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Initialise saturating addition. */
  add_builtin_function ("__builtin_adds", int_ftype_int_int,
			       PICOCHIP_BUILTIN_ADDS, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("picoAdds", int_ftype_int_int,
			       PICOCHIP_BUILTIN_ADDS, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Initialise saturating subtraction. */
  add_builtin_function ("__builtin_subs", int_ftype_int_int,
			       PICOCHIP_BUILTIN_SUBS, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("picoSubs", int_ftype_int_int,
			       PICOCHIP_BUILTIN_SUBS, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Scalar comms builtins. */
  add_builtin_function ("__builtin_get", long_ftype_int,
			       PICOCHIP_BUILTIN_GET, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("__builtin_put", void_ftype_int_long,
			       PICOCHIP_BUILTIN_PUT, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("__builtin_testport", int_ftype_int,
			       PICOCHIP_BUILTIN_TESTPORT, BUILT_IN_MD, NULL,
			       NULL_TREE);

  /* Array comms builtins. */
  add_builtin_function ("__builtin_put_array",
			       void_ftype_long_int_int_int,
			       PICOCHIP_BUILTIN_PUT_ARRAY, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("__builtin_get_array", long_ftype_int_int_int,
			       PICOCHIP_BUILTIN_GET_ARRAY, BUILT_IN_MD, NULL,
			       NULL_TREE);
  add_builtin_function ("__builtin_testport_array",
			       int_ftype_int_int_int,
			       PICOCHIP_BUILTIN_TESTPORT_ARRAY, BUILT_IN_MD,
			       NULL, NULL_TREE);

  /* Halt instruction. Note that the builtin function is marked as
     having the attribute `noreturn' so that the compiler realises
     that the halt stops the program dead. */
  noreturn = tree_cons (get_identifier ("noreturn"), NULL, NULL);
  add_builtin_function ("__builtin_halt", void_ftype_void,
			       PICOCHIP_BUILTIN_HALT, BUILT_IN_MD, NULL,
			       noreturn);
  add_builtin_function ("picoHalt", void_ftype_void,
			       PICOCHIP_BUILTIN_HALT, BUILT_IN_MD, NULL,
			       noreturn);

}

/* Expand a call to a builtin function. */
rtx
picochip_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case PICOCHIP_BUILTIN_ASRI:
      return picochip_expand_builtin_3op (CODE_FOR_builtin_asri, exp,
					  target);

    case PICOCHIP_BUILTIN_ADDS:
      return picochip_expand_builtin_3op (CODE_FOR_sataddhi3, exp,
					  target);

    case PICOCHIP_BUILTIN_SUBS:
      return picochip_expand_builtin_3op (CODE_FOR_satsubhi3, exp,
					  target);

    case PICOCHIP_BUILTIN_SBC:
      return picochip_expand_builtin_2op (CODE_FOR_sbc, exp, target);

    case PICOCHIP_BUILTIN_BREV:
      return picochip_expand_builtin_2op (CODE_FOR_brev, exp, target);

    case PICOCHIP_BUILTIN_BYTESWAP:
      return picochip_expand_builtin_2op (CODE_FOR_bswaphi2, exp, target);

    case PICOCHIP_BUILTIN_GET:
      return picochip_expand_builtin_2op (CODE_FOR_commsGet, exp, target);

    case PICOCHIP_BUILTIN_PUT:
      return picochip_expand_builtin_2opvoid (CODE_FOR_commsPut, exp);

    case PICOCHIP_BUILTIN_TESTPORT:
      return picochip_expand_builtin_2op (CODE_FOR_commsTestPort, exp,
					  target);

    case PICOCHIP_BUILTIN_PUT_ARRAY:
      return picochip_expand_array_put (exp, target);

    case PICOCHIP_BUILTIN_GET_ARRAY:
      return picochip_expand_array_get (exp, target);

    case PICOCHIP_BUILTIN_TESTPORT_ARRAY:
      return picochip_expand_array_testport (exp, target);

    case PICOCHIP_BUILTIN_HALT:
      return picochip_generate_halt ();

    default:
      gcc_unreachable();

    }

  /* Should really do something sensible here.  */
  return NULL_RTX;
}

/* Emit warnings. */
static void
picochip_warn_inefficient (const char *msg)
{
  if (TARGET_INEFFICIENT_WARNINGS)
    warning (OPT_minefficient_warnings,
	     "%s (disable warning using -mno-inefficient-warnings)", msg);
}

void
warn_of_byte_access (void)
{
  static int warned = 0;

  if (!warned)
    {
      picochip_warn_inefficient
	("byte access is synthesised - consider using MUL AE");
      warned = 1;
    }

}

rtx
picochip_function_value (const_tree valtype, const_tree func,
                         bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = TYPE_MODE (valtype);
  int unsignedp = TYPE_UNSIGNED (valtype);

  /* Since we define PROMOTE_FUNCTION_RETURN, we must promote the mode
     just as PROMOTE_MODE does.  */
  mode = promote_function_mode (valtype, mode, &unsignedp, func, 1);

  return gen_rtx_REG (mode, 0);

}

/* Check that the value of the given mode will fit in the register of
   the given mode. */
int
picochip_hard_regno_mode_ok (int regno, enum machine_mode mode)
{

  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  /* If the CC register is being used, then only CC mode values are
     allowed (which have already been tested). */
  if (regno == CC_REGNUM || regno == ACC_REGNUM)
    return 0;

  /* Must be a valid register. */
  if (regno > 16)
    return 0;

  /* Modes QI and HI may be placed in any register except the CC. */
  if (mode == QImode || mode == HImode)
    return 1;

  /* DI must be in a quad register. */
  if (mode == DImode)
    return (regno % 4 == 0);

  /* All other modes must be placed in a even numbered register. */
  return !(regno & 1);

}

/* Extract the lower and upper components of a constant value. */

rtx
picochip_get_low_const (rtx value)
{
  return gen_int_mode (INTVAL (value) & 0xFFFF, HImode);
}

rtx
picochip_get_high_const (rtx value)
{
  /*return GEN_INT ((((INTVAL (value) >> 16) & 0xFFFF) ^ 0x8000) - 0x8000); */
  return gen_int_mode ((INTVAL (value) >> 16) & 0xFFFF, HImode);
}


/* Loading and storing QImode values to and from memory in a machine
   without byte access requires might require a scratch
   register.  However, the scratch register might correspond to the
   register in which the value is being loaded.  To ensure that a
   scratch register is supplied which is definitely different to the
   output register, request a register pair.  This effectively gives a
   choice of two registers to choose from, so that we a guaranteed to
   get at least one register which is different to the output
   register.  This trick is taken from the alpha implementation. */
static reg_class_t
picochip_secondary_reload (bool in_p,
			   rtx x ATTRIBUTE_UNUSED,
			   reg_class_t cla ATTRIBUTE_UNUSED,
			   enum machine_mode mode,
			   secondary_reload_info *sri)
{
  if (mode == QImode && !TARGET_HAS_BYTE_ACCESS)
  {
    if (in_p == 0)
      sri->icode = CODE_FOR_reload_outqi;
    else
      sri->icode = CODE_FOR_reload_inqi;
  }

  /* We dont need to return a register class type when we need only a
     scratch register. It realizes the scratch register type by looking
     at the instruction definition for sri->icode. We only need to
     return the register type when we need intermediaries for copies.*/
  return NO_REGS;
}

/* Return true if the given memory operand can be aligned to a
   word+offset memory reference (e.g., FP+3 can be converted into the
   memory operand FP+2, with the offset 1). */
int
picochip_alignable_memory_operand (rtx mem_operand,
				   enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx address;

  /* Not a mem operand. Refuse immediately. */
  if (MEM != GET_CODE (mem_operand))
    return 0;

  address = XEXP (mem_operand, 0);

  /* Return true if a PLUS of the SP and a (valid) constant, or SP itself. */
  return ((PLUS == GET_CODE (address) &&
	   REGNO (XEXP (address, 0)) == STACK_POINTER_REGNUM &&
	   CONST_INT == GET_CODE (XEXP (address, 1)) &&
	   picochip_const_ok_for_letter_p (INTVAL (XEXP (address, 1)), 'K'))
	  || (REG == GET_CODE (address)
	      && REGNO (address) == STACK_POINTER_REGNUM));

}

/* Return true if the given memory reference is to a word aligned
   address.  Currently this means it must be either SP, or
   SP+offset.  We could replace this function with alignable
   memory references in the above function?. */
int
picochip_word_aligned_memory_reference (rtx operand)
{


  /* The address must be the SP register, or a constant, aligned
     offset from SP which doesn't exceed the FP+offset
     restrictions. */
  return ((PLUS == GET_CODE (operand)
	   && REGNO (XEXP (operand, 0)) == STACK_POINTER_REGNUM
	   && picochip_is_aligned (INTVAL (XEXP (operand, 1)), 16)
           && picochip_const_ok_for_letter_p (INTVAL (XEXP (operand, 1)),
                                                'K'))
	  || (REG == GET_CODE (operand)
	      && REGNO (operand) == STACK_POINTER_REGNUM));

}

/* Given an alignable memory location, convert the memory location
   into a HI mode access, storing the new memory reference in
   paligned_mem, and the number of bits by which to shift in pbitnum
   (i.e., given a reference to FP+3, this creates an aligned reference
   of FP+2, with an 8-bit shift). This code is a modification of that
   found in the Alpha port. */
void
picochip_get_hi_aligned_mem (rtx ref, rtx * paligned_mem, rtx * pbitnum)
{
  rtx base;
  HOST_WIDE_INT offset = 0;

  gcc_assert (GET_CODE (ref) == MEM);

  if (reload_in_progress && !memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    {
      base = find_replacement (&XEXP (ref, 0));

      gcc_assert(memory_address_p (GET_MODE (ref), base));
    }
  else
    {
      base = XEXP (ref, 0);
    }

  if (GET_CODE (base) == PLUS)
    {
      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);
    }

  *paligned_mem = widen_memory_access (ref, HImode, (offset & ~1) - offset);

  if (offset > 0)
    {
      if (TARGET_DEBUG)
	{
	  printf
	    ("Found non-zero offset in get_hi_aligned_mem - check that the correct value is being used (as this functionality hasn't been exploited yet).\n");
	}
    }

  *pbitnum = GEN_INT ((offset & 1) * 8);

}

/* Return true if the given operand is an absolute address in memory
   (i.e., a symbolic offset). */
int
picochip_absolute_memory_operand (rtx op,
				  enum machine_mode mode ATTRIBUTE_UNUSED)
{

  if (MEM == GET_CODE (op))
    {
      rtx address = XEXP (op, 0);

      /* Symbols are valid absolute addresses. */
      if (SYMBOL_REF == GET_CODE (address))
	return 1;

      /* Constant offsets to symbols are valid absolute addresses. */
      if (CONST == GET_CODE (address) &&
	  PLUS == GET_CODE (XEXP (address, 0)) &&
	  SYMBOL_REF == GET_CODE (XEXP (XEXP (address, 0), 0)) &&
	  CONST_INT == GET_CODE (XEXP (XEXP (address, 0), 1)))
	return 1;

    }
  else
    return 0;

  /* Symbols are valid absolute addresses. */
  if (SYMBOL_REF == GET_CODE (XEXP (op, 0)))
    return 1;


  return 0;

}

void
picochip_asm_named_section (const char *name,
			    unsigned int flags ATTRIBUTE_UNUSED,
			    tree decl ATTRIBUTE_UNUSED)
{
  fprintf (asm_out_file, ".section %s\n", name);
}


/* Check if we can make a conditional copy instruction.  This is emitted as an
   instruction to set the condition register, followed by an instruction which
   uses the condition registers to perform the conditional move. */
int
picochip_check_conditional_copy (rtx * operands)
{

  rtx branch_op_0 = XEXP (operands[1], 0);
  rtx branch_op_1 = XEXP (operands[1], 1);

  /* Only HI mode conditional moves are currently allowed.  Can we add
     SI mode moves? */
  if (GET_CODE (operands[1]) != EQ && GET_CODE (operands[1]) != NE)
    return 0;

  /* Is the comparison valid? Only allow operands which are registers
     if they are HImode.  SI mode comparisons against 0 could be
     handled using logical operations (e.g., SIreg != 0 when low ||
     high). Need to find test cases to provoke this though (fixunssfdi
     in libgcc does, but is complicated). */
  if (register_operand(branch_op_0, GET_MODE(branch_op_0)) &&
      GET_MODE(branch_op_0) != HImode)
    return 0;
  if (register_operand(branch_op_1, GET_MODE(branch_op_1)) &&
      GET_MODE(branch_op_1) != HImode)
    return 0;

  return 1;

}


static rtx
picochip_static_chain (const_tree ARG_UNUSED (fndecl), bool incoming_p)
{
  rtx addr;
  if (incoming_p)
    addr = arg_pointer_rtx;
  else
    addr = plus_constant (Pmode, stack_pointer_rtx, -2 * UNITS_PER_WORD);
  return gen_frame_mem (Pmode, addr);
}
