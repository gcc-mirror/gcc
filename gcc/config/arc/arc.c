/* Subroutines used for code generation on the Synopsys DesignWare ARC cpu.
   Copyright (C) 1994, 1995, 1997, 2004, 2007-2013
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "function.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"
#include "target.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "recog.h"
#include "debug.h"
#include "diagnostic.h"
#include "insn-codes.h"
#include "langhooks.h"
#include "optabs.h"
#include "tm-constrs.h"
#include "reload.h" /* For operands_match_p */
#include "df.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"

/* Which cpu we're compiling for (A5, ARC600, ARC601, ARC700).  */
static const char *arc_cpu_string = "";

/* ??? Loads can handle any constant, stores can only handle small ones.  */
/* OTOH, LIMMs cost extra, so their usefulness is limited.  */
#define RTX_OK_FOR_OFFSET_P(MODE, X) \
(GET_CODE (X) == CONST_INT \
 && SMALL_INT_RANGE (INTVAL (X), (GET_MODE_SIZE (MODE) - 1) & -4, \
		     (INTVAL (X) & (GET_MODE_SIZE (MODE) - 1) & 3 \
		      ? 0 \
		      : -(-GET_MODE_SIZE (MODE) | -4) >> 1)))

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE, X, INDEX, STRICT) \
(GET_CODE (X) == PLUS			     \
  && RTX_OK_FOR_BASE_P (XEXP (X, 0), (STRICT)) \
  && ((INDEX && RTX_OK_FOR_INDEX_P (XEXP (X, 1), (STRICT)) \
       && GET_MODE_SIZE ((MODE)) <= 4) \
      || RTX_OK_FOR_OFFSET_P (MODE, XEXP (X, 1))))

#define LEGITIMATE_SCALED_ADDRESS_P(MODE, X, STRICT) \
(GET_CODE (X) == PLUS \
 && GET_CODE (XEXP (X, 0)) == MULT \
 && RTX_OK_FOR_INDEX_P (XEXP (XEXP (X, 0), 0), (STRICT)) \
 && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT \
 && ((GET_MODE_SIZE (MODE) == 2 && INTVAL (XEXP (XEXP (X, 0), 1)) == 2) \
     || (GET_MODE_SIZE (MODE) == 4 && INTVAL (XEXP (XEXP (X, 0), 1)) == 4)) \
 && (RTX_OK_FOR_BASE_P (XEXP (X, 1), (STRICT)) \
     || (flag_pic ? CONST_INT_P (XEXP (X, 1)) : CONSTANT_P (XEXP (X, 1)))))

#define LEGITIMATE_SMALL_DATA_ADDRESS_P(X) \
  (GET_CODE (X) == PLUS \
   && (REG_P (XEXP ((X), 0)) && REGNO (XEXP ((X), 0)) == SDATA_BASE_REGNUM) \
   && ((GET_CODE (XEXP((X),1)) == SYMBOL_REF \
	&& SYMBOL_REF_SMALL_P (XEXP ((X), 1))) \
       || (GET_CODE (XEXP ((X), 1)) == CONST \
	   && GET_CODE (XEXP (XEXP ((X), 1), 0)) == PLUS \
	   && GET_CODE (XEXP (XEXP (XEXP ((X), 1), 0), 0)) == SYMBOL_REF \
	   && SYMBOL_REF_SMALL_P (XEXP (XEXP (XEXP ((X), 1), 0), 0)) \
	   && GET_CODE (XEXP(XEXP (XEXP ((X), 1), 0), 1)) == CONST_INT)))

/* Array of valid operand punctuation characters.  */
char arc_punct_chars[256];

/* State used by arc_ccfsm_advance to implement conditional execution.  */
struct GTY (()) arc_ccfsm
{
  int state;
  int cc;
  rtx cond;
  rtx target_insn;
  int target_label;
};

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

/* The values of unspec's first field.  */
enum {
  ARC_UNSPEC_PLT = 3,
  ARC_UNSPEC_GOT,
  ARC_UNSPEC_GOTOFF
} ;


enum arc_builtins {
  ARC_BUILTIN_NOP        =    2,
  ARC_BUILTIN_NORM       =    3,
  ARC_BUILTIN_NORMW      =    4,
  ARC_BUILTIN_SWAP       =    5,
  ARC_BUILTIN_BRK        =    6,
  ARC_BUILTIN_DIVAW      =    7,
  ARC_BUILTIN_EX         =    8,
  ARC_BUILTIN_MUL64      =    9,
  ARC_BUILTIN_MULU64     =   10,
  ARC_BUILTIN_RTIE       =   11,
  ARC_BUILTIN_SYNC       =   12,
  ARC_BUILTIN_CORE_READ  =   13,
  ARC_BUILTIN_CORE_WRITE =   14,
  ARC_BUILTIN_FLAG       =   15,
  ARC_BUILTIN_LR         =   16,
  ARC_BUILTIN_SR         =   17,
  ARC_BUILTIN_SLEEP      =   18,
  ARC_BUILTIN_SWI        =   19,
  ARC_BUILTIN_TRAP_S     =   20,
  ARC_BUILTIN_UNIMP_S    =   21,
  ARC_BUILTIN_ALIGNED    =   22,

  /* Sentinel to mark start of simd builtins.  */
  ARC_SIMD_BUILTIN_BEGIN      = 1000,

  ARC_SIMD_BUILTIN_VADDAW     = 1001,
  ARC_SIMD_BUILTIN_VADDW      = 1002,
  ARC_SIMD_BUILTIN_VAVB       = 1003,
  ARC_SIMD_BUILTIN_VAVRB      = 1004,
  ARC_SIMD_BUILTIN_VDIFAW     = 1005,
  ARC_SIMD_BUILTIN_VDIFW      = 1006,
  ARC_SIMD_BUILTIN_VMAXAW     = 1007,
  ARC_SIMD_BUILTIN_VMAXW      = 1008,
  ARC_SIMD_BUILTIN_VMINAW     = 1009,
  ARC_SIMD_BUILTIN_VMINW      = 1010,
  ARC_SIMD_BUILTIN_VMULAW     = 1011,
  ARC_SIMD_BUILTIN_VMULFAW    = 1012,
  ARC_SIMD_BUILTIN_VMULFW     = 1013,
  ARC_SIMD_BUILTIN_VMULW      = 1014,
  ARC_SIMD_BUILTIN_VSUBAW     = 1015,
  ARC_SIMD_BUILTIN_VSUBW      = 1016,
  ARC_SIMD_BUILTIN_VSUMMW     = 1017,
  ARC_SIMD_BUILTIN_VAND       = 1018,
  ARC_SIMD_BUILTIN_VANDAW     = 1019,
  ARC_SIMD_BUILTIN_VBIC       = 1020,
  ARC_SIMD_BUILTIN_VBICAW     = 1021,
  ARC_SIMD_BUILTIN_VOR        = 1022,
  ARC_SIMD_BUILTIN_VXOR       = 1023,
  ARC_SIMD_BUILTIN_VXORAW     = 1024,
  ARC_SIMD_BUILTIN_VEQW       = 1025,
  ARC_SIMD_BUILTIN_VLEW       = 1026,
  ARC_SIMD_BUILTIN_VLTW       = 1027,
  ARC_SIMD_BUILTIN_VNEW       = 1028,
  ARC_SIMD_BUILTIN_VMR1AW     = 1029,
  ARC_SIMD_BUILTIN_VMR1W      = 1030,
  ARC_SIMD_BUILTIN_VMR2AW     = 1031,
  ARC_SIMD_BUILTIN_VMR2W      = 1032,
  ARC_SIMD_BUILTIN_VMR3AW     = 1033,
  ARC_SIMD_BUILTIN_VMR3W      = 1034,
  ARC_SIMD_BUILTIN_VMR4AW     = 1035,
  ARC_SIMD_BUILTIN_VMR4W      = 1036,
  ARC_SIMD_BUILTIN_VMR5AW     = 1037,
  ARC_SIMD_BUILTIN_VMR5W      = 1038,
  ARC_SIMD_BUILTIN_VMR6AW     = 1039,
  ARC_SIMD_BUILTIN_VMR6W      = 1040,
  ARC_SIMD_BUILTIN_VMR7AW     = 1041,
  ARC_SIMD_BUILTIN_VMR7W      = 1042,
  ARC_SIMD_BUILTIN_VMRB       = 1043,
  ARC_SIMD_BUILTIN_VH264F     = 1044,
  ARC_SIMD_BUILTIN_VH264FT    = 1045,
  ARC_SIMD_BUILTIN_VH264FW    = 1046,
  ARC_SIMD_BUILTIN_VVC1F      = 1047,
  ARC_SIMD_BUILTIN_VVC1FT     = 1048,

  /* Va, Vb, rlimm instructions.  */
  ARC_SIMD_BUILTIN_VBADDW     = 1050,
  ARC_SIMD_BUILTIN_VBMAXW     = 1051,
  ARC_SIMD_BUILTIN_VBMINW     = 1052,
  ARC_SIMD_BUILTIN_VBMULAW    = 1053,
  ARC_SIMD_BUILTIN_VBMULFW    = 1054,
  ARC_SIMD_BUILTIN_VBMULW     = 1055,
  ARC_SIMD_BUILTIN_VBRSUBW    = 1056,
  ARC_SIMD_BUILTIN_VBSUBW     = 1057,

  /* Va, Vb, Ic instructions.  */
  ARC_SIMD_BUILTIN_VASRW      = 1060,
  ARC_SIMD_BUILTIN_VSR8       = 1061,
  ARC_SIMD_BUILTIN_VSR8AW     = 1062,

  /* Va, Vb, u6 instructions.  */
  ARC_SIMD_BUILTIN_VASRRWi    = 1065,
  ARC_SIMD_BUILTIN_VASRSRWi   = 1066,
  ARC_SIMD_BUILTIN_VASRWi     = 1067,
  ARC_SIMD_BUILTIN_VASRPWBi   = 1068,
  ARC_SIMD_BUILTIN_VASRRPWBi  = 1069,
  ARC_SIMD_BUILTIN_VSR8AWi    = 1070,
  ARC_SIMD_BUILTIN_VSR8i      = 1071,

  /* Va, Vb, u8 (simm) instructions.  */
  ARC_SIMD_BUILTIN_VMVAW      = 1075,
  ARC_SIMD_BUILTIN_VMVW       = 1076,
  ARC_SIMD_BUILTIN_VMVZW      = 1077,
  ARC_SIMD_BUILTIN_VD6TAPF    = 1078,

  /* Va, rlimm, u8 (simm) instructions.  */
  ARC_SIMD_BUILTIN_VMOVAW     = 1080,
  ARC_SIMD_BUILTIN_VMOVW      = 1081,
  ARC_SIMD_BUILTIN_VMOVZW     = 1082,

  /* Va, Vb instructions.  */
  ARC_SIMD_BUILTIN_VABSAW     = 1085,
  ARC_SIMD_BUILTIN_VABSW      = 1086,
  ARC_SIMD_BUILTIN_VADDSUW    = 1087,
  ARC_SIMD_BUILTIN_VSIGNW     = 1088,
  ARC_SIMD_BUILTIN_VEXCH1     = 1089,
  ARC_SIMD_BUILTIN_VEXCH2     = 1090,
  ARC_SIMD_BUILTIN_VEXCH4     = 1091,
  ARC_SIMD_BUILTIN_VUPBAW     = 1092,
  ARC_SIMD_BUILTIN_VUPBW      = 1093,
  ARC_SIMD_BUILTIN_VUPSBAW    = 1094,
  ARC_SIMD_BUILTIN_VUPSBW     = 1095,

  ARC_SIMD_BUILTIN_VDIRUN     = 1100,
  ARC_SIMD_BUILTIN_VDORUN     = 1101,
  ARC_SIMD_BUILTIN_VDIWR      = 1102,
  ARC_SIMD_BUILTIN_VDOWR      = 1103,

  ARC_SIMD_BUILTIN_VREC       = 1105,
  ARC_SIMD_BUILTIN_VRUN       = 1106,
  ARC_SIMD_BUILTIN_VRECRUN    = 1107,
  ARC_SIMD_BUILTIN_VENDREC    = 1108,

  ARC_SIMD_BUILTIN_VLD32WH    = 1110,
  ARC_SIMD_BUILTIN_VLD32WL    = 1111,
  ARC_SIMD_BUILTIN_VLD64      = 1112,
  ARC_SIMD_BUILTIN_VLD32      = 1113,
  ARC_SIMD_BUILTIN_VLD64W     = 1114,
  ARC_SIMD_BUILTIN_VLD128     = 1115,
  ARC_SIMD_BUILTIN_VST128     = 1116,
  ARC_SIMD_BUILTIN_VST64      = 1117,

  ARC_SIMD_BUILTIN_VST16_N    = 1120,
  ARC_SIMD_BUILTIN_VST32_N    = 1121,

  ARC_SIMD_BUILTIN_VINTI      = 1201,

  ARC_SIMD_BUILTIN_END
};

/* A nop is needed between a 4 byte insn that sets the condition codes and
   a branch that uses them (the same isn't true for an 8 byte insn that sets
   the condition codes).  Set by arc_ccfsm_advance.  Used by
   arc_print_operand.  */

static int get_arc_condition_code (rtx);

static tree arc_handle_interrupt_attribute (tree *, tree, tree, int, bool *);

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
  { NULL, 0, 0, false, false, false, NULL, false }
};
static int arc_comp_type_attributes (const_tree, const_tree);
static void arc_file_start (void);
static void arc_internal_label (FILE *, const char *, unsigned long);
static void arc_output_mi_thunk (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT,
				 tree);
static int arc_address_cost (rtx, enum machine_mode, addr_space_t, bool);
static void arc_encode_section_info (tree decl, rtx rtl, int first);

static void arc_init_builtins (void);
static rtx arc_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

static int branch_dest (rtx);

static void  arc_output_pic_addr_const (FILE *,  rtx, int);
void emit_pic_move (rtx *, enum machine_mode);
bool arc_legitimate_pic_operand_p (rtx);
static bool arc_function_ok_for_sibcall (tree, tree);
static rtx arc_function_value (const_tree, const_tree, bool);
const char * output_shift (rtx *);
static void arc_reorg (void);
static bool arc_in_small_data_p (const_tree);

static void arc_init_reg_tables (void);
static bool arc_return_in_memory (const_tree, const_tree);
static void arc_init_simd_builtins (void);
static bool arc_vector_mode_supported_p (enum machine_mode);

static const char *arc_invalid_within_doloop (const_rtx);

static void output_short_suffix (FILE *file);

static bool arc_frame_pointer_required (void);

/* Implements target hook vector_mode_supported_p.  */

static bool
arc_vector_mode_supported_p (enum machine_mode mode)
{
  if (!TARGET_SIMD_SET)
    return false;

  if ((mode == V4SImode)
      || (mode == V8HImode))
    return true;

  return false;
}


/* TARGET_PRESERVE_RELOAD_P is still awaiting patch re-evaluation / review.  */
static bool arc_preserve_reload_p (rtx in) ATTRIBUTE_UNUSED;
static rtx arc_delegitimize_address (rtx);
static bool arc_can_follow_jump (const_rtx follower, const_rtx followee);

static rtx frame_insn (rtx);
static void arc_function_arg_advance (cumulative_args_t, enum machine_mode,
				      const_tree, bool);
static rtx arc_legitimize_address_0 (rtx, rtx, enum machine_mode mode);

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

#undef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP arc_invalid_within_doloop

#undef TARGET_PRESERVE_RELOAD_P
#define TARGET_PRESERVE_RELOAD_P arc_preserve_reload_p

#undef TARGET_CAN_FOLLOW_JUMP
#define TARGET_CAN_FOLLOW_JUMP arc_can_follow_jump

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS arc_delegitimize_address

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

#define TARGET_LRA_P arc_lra_p
#define TARGET_REGISTER_PRIORITY arc_register_priority
/* Stores with scaled offsets have different displacement ranges.  */
#define TARGET_DIFFERENT_ADDR_DISPLACEMENT_P hook_bool_void_true
#define TARGET_SPILL_CLASS arc_spill_class

#include "target-def.h"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

/* Try to keep the (mov:DF _, reg) as early as possible so
   that the d<add/sub/mul>h-lr insns appear together and can
   use the peephole2 pattern.  */

static int
arc_sched_adjust_priority (rtx insn, int priority)
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

static reg_class_t
arc_secondary_reload (bool in_p, rtx x, reg_class_t cl, enum machine_mode,
		      secondary_reload_info *)
{
  if (cl == DOUBLE_REGS)
    return GENERAL_REGS;

  /* The loop counter register can be stored, but not loaded directly.  */
  if ((cl == LPCOUNT_REG || cl == WRITABLE_CORE_REGS)
      && in_p && MEM_P (x))
    return GENERAL_REGS;
  return NO_REGS;
}

static unsigned arc_ifcvt (void);

namespace {

const pass_data pass_data_arc_ifcvt =
{
  RTL_PASS,
  "arc_ifcvt",				/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  false,				/* has_gate */
  true,					/* has_execute */
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
  opt_pass * clone () { return new pass_arc_ifcvt (ctxt_); }
  unsigned int execute () { return arc_ifcvt (); }
};

} // anon namespace

rtl_opt_pass *
make_pass_arc_ifcvt (gcc::context *ctxt)
{
  return new pass_arc_ifcvt (ctxt);
}

/* Called by OVERRIDE_OPTIONS to initialize various things.  */

void
arc_init (void)
{
  enum attr_tune tune_dflt = TUNE_NONE;

  if (TARGET_A5)
    {
      arc_cpu_string = "A5";
    }
  else if (TARGET_ARC600)
    {
      arc_cpu_string = "ARC600";
      tune_dflt = TUNE_ARC600;
    }
  else if (TARGET_ARC601)
    {
      arc_cpu_string = "ARC601";
      tune_dflt = TUNE_ARC600;
    }
  else if (TARGET_ARC700)
    {
      arc_cpu_string = "ARC700";
      tune_dflt = TUNE_ARC700_4_2_STD;
    }
  else
    gcc_unreachable ();
  if (arc_tune == TUNE_NONE)
    arc_tune = tune_dflt;
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

  /* Support mul64 generation only for A5 and ARC600.  */
  if (TARGET_MUL64_SET && TARGET_ARC700)
      error ("-mmul64 not supported for ARC700");

  /* MPY instructions valid only for ARC700.  */
  if (TARGET_NOMPY_SET && !TARGET_ARC700)
      error ("-mno-mpy supported only for ARC700");

  /* mul/mac instructions only for ARC600.  */
  if (TARGET_MULMAC_32BY16_SET && !(TARGET_ARC600 || TARGET_ARC601))
      error ("-mmul32x16 supported only for ARC600 or ARC601");

  if (!TARGET_DPFP && TARGET_DPFP_DISABLE_LRSR)
      error ("-mno-dpfp-lrsr suppforted only with -mdpfp");

  /* FPX-1. No fast and compact together.  */
  if ((TARGET_DPFP_FAST_SET && TARGET_DPFP_COMPACT_SET)
      || (TARGET_SPFP_FAST_SET && TARGET_SPFP_COMPACT_SET))
    error ("FPX fast and compact options cannot be specified together");

  /* FPX-2. No fast-spfp for arc600 or arc601.  */
  if (TARGET_SPFP_FAST_SET && (TARGET_ARC600 || TARGET_ARC601))
    error ("-mspfp_fast not available on ARC600 or ARC601");

  /* FPX-3. No FPX extensions on pre-ARC600 cores.  */
  if ((TARGET_DPFP || TARGET_SPFP)
      && !(TARGET_ARC600 || TARGET_ARC601 || TARGET_ARC700))
    error ("FPX extensions not available on pre-ARC600 cores");

  /* Warn for unimplemented PIC in pre-ARC700 cores, and disable flag_pic.  */
  if (flag_pic && !TARGET_ARC700)
    {
      warning (DK_WARNING, "PIC is not supported for %s. Generating non-PIC code only..", arc_cpu_string);
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
}

/* Check ARC options, generate derived target attributes.  */

static void
arc_override_options (void)
{
  if (arc_cpu == PROCESSOR_NONE)
    arc_cpu = PROCESSOR_ARC700;

  if (arc_size_opt_level == 3)
    optimize_size = 1;

  if (flag_pic)
    target_flags |= MASK_NO_SDATA_SET;

  if (flag_no_common == 255)
    flag_no_common = !TARGET_NO_SDATA_SET;

  /* TARGET_COMPACT_CASESI needs the "q" register class.  */ \
  if (TARGET_MIXED_CODE)
    TARGET_Q_CLASS = 1;
  if (!TARGET_Q_CLASS)
    TARGET_COMPACT_CASESI = 0;
  if (TARGET_COMPACT_CASESI)
    TARGET_CASE_VECTOR_PC_RELATIVE = 1;

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
    case CCmode:
    case SImode: /* For BRcc.  */
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
    case CC_ZNmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	case GE: return ARC_CC_P;
	case LT: return ARC_CC_N;
	case GT : return ARC_CC_PNZ;
	default : gcc_unreachable ();
	}
    case CC_Zmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	default : gcc_unreachable ();
	}
    case CC_Cmode:
      switch (GET_CODE (comparison))
	{
	case LTU : return ARC_CC_C;
	case GEU : return ARC_CC_NC;
	default : gcc_unreachable ();
	}
    case CC_FP_GTmode:
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
    case CC_FP_GEmode:
      /* Same for FPX and non-FPX.  */
      switch (GET_CODE (comparison))
	{
	case GE   : return ARC_CC_HS;
	case UNLT : return ARC_CC_LO;
	default : gcc_unreachable ();
	}
    case CC_FP_UNEQmode:
      switch (GET_CODE (comparison))
	{
	case UNEQ : return ARC_CC_EQ;
	case LTGT : return ARC_CC_NE;
	default : gcc_unreachable ();
	}
    case CC_FP_ORDmode:
      switch (GET_CODE (comparison))
	{
	case UNORDERED : return ARC_CC_C;
	case ORDERED   : return ARC_CC_NC;
	default : gcc_unreachable ();
	}
    case CC_FPXmode:
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

enum machine_mode
arc_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  enum machine_mode mode = GET_MODE (x);
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
	  || ((op == LT || op == GE) && GET_MODE_SIZE (GET_MODE (x) <= 4))))
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
  else if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_OPTFPE)
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
      switch (GET_MODE_CLASS (i))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (GET_MODE_SIZE (i) <= 4)
	    arc_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    arc_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    arc_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    arc_mode_class[i] = 1 << (int) O_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (i) <= 4)
	    arc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    arc_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    arc_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    arc_mode_class[i] = 1 << (int) OF_MODE;
	  else
	    arc_mode_class[i] = 0;
	  break;
	case MODE_VECTOR_INT:
	  arc_mode_class [i] = (1<< (int) V_MODE);
	  break;
	case MODE_CC:
	default:
	  /* mode_class hasn't been initialized yet for EXTRA_CC_MODES, so
	     we must explicitly check for them here.  */
	  if (i == (int) CCmode || i == (int) CC_ZNmode || i == (int) CC_Zmode
	      || i == (int) CC_Cmode
	      || i == CC_FP_GTmode || i == CC_FP_GEmode || i == CC_FP_ORDmode)
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

static void
arc_conditional_register_usage (void)
{
  int regno;
  int i;
  int fix_start = 60, fix_end = 55;

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
  if (TARGET_SIMD_SET)
    {
      int i;
      for (i = ARC_FIRST_SIMD_VR_REG; i <= ARC_LAST_SIMD_VR_REG; i++)
	reg_alloc_order [i] = i;
      for (i = ARC_FIRST_SIMD_DMA_CONFIG_REG;
	   i <= ARC_LAST_SIMD_DMA_CONFIG_REG; i++)
	reg_alloc_order [i] = i;
    }
  /* For Arctangent-A5 / ARC600, lp_count may not be read in an instruction
     following immediately after another one setting it to a new value.
     There was some discussion on how to enforce scheduling constraints for
     processors with missing interlocks on the gcc mailing list:
     http://gcc.gnu.org/ml/gcc/2008-05/msg00021.html .
     However, we can't actually use this approach, because for ARC the
     delay slot scheduling pass is active, which runs after
     machine_dependent_reorg.  */
  if (TARGET_ARC600)
    CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], LP_COUNT);
  else if (!TARGET_ARC700)
    fixed_regs[LP_COUNT] = 1;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (!call_used_regs[regno])
      CLEAR_HARD_REG_BIT (reg_class_contents[SIBCALL_REGS], regno);
  for (regno = 32; regno < 60; regno++)
    if (!fixed_regs[regno])
      SET_HARD_REG_BIT (reg_class_contents[WRITABLE_CORE_REGS], regno);
  if (TARGET_ARC700)
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

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i < 29)
	{
	  if (TARGET_Q_CLASS && ((i <= 3) || ((i >= 12) && (i <= 15))))
	    arc_regno_reg_class[i] = ARCOMPACT16_REGS;
	  else
	    arc_regno_reg_class[i] = GENERAL_REGS;
	}
      else if (i < 60)
	arc_regno_reg_class[i]
	  = (fixed_regs[i]
	     ? (TEST_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], i)
		? CHEAP_CORE_REGS : ALL_CORE_REGS)
	     : ((TARGET_ARC700
		 && TEST_HARD_REG_BIT (reg_class_contents[CHEAP_CORE_REGS], i))
		? CHEAP_CORE_REGS : WRITABLE_CORE_REGS));
      else
	arc_regno_reg_class[i] = NO_REGS;
    }

  /* ARCOMPACT16_REGS is empty, if TARGET_Q_CLASS has not been activated.  */
  if (!TARGET_Q_CLASS)
    {
      CLEAR_HARD_REG_SET(reg_class_contents [ARCOMPACT16_REGS]);
      CLEAR_HARD_REG_SET(reg_class_contents [AC16_BASE_REGS]);
    }

  gcc_assert (FIRST_PSEUDO_REGISTER >= 144);

  /* Handle Special Registers.  */
  arc_regno_reg_class[29] = LINK_REGS; /* ilink1 register.  */
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
  else if (strcmp (TREE_STRING_POINTER (value), "ilink1")
	   && strcmp (TREE_STRING_POINTER (value), "ilink2"))
    {
      warning (OPT_Wattributes,
	       "argument of %qE attribute is not \"ilink1\" or \"ilink2\"",
	       name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
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
gen_compare_reg (rtx comparison, enum machine_mode omode)
{
  enum rtx_code code = GET_CODE (comparison);
  rtx x = XEXP (comparison, 0);
  rtx y = XEXP (comparison, 1);
  rtx tmp, cc_reg;
  enum machine_mode mode, cmode;


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
	emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
				gen_rtx_COMPARE (mode,
						 gen_rtx_REG (CC_FPXmode, 61),
						 const0_rtx)));
    }
  else if (GET_MODE_CLASS (cmode) == MODE_FLOAT && TARGET_OPTFPE)
    {
      rtx op0 = gen_rtx_REG (cmode, 0);
      rtx op1 = gen_rtx_REG (cmode, GET_MODE_SIZE (cmode) / UNITS_PER_WORD);

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
      if (currently_expanding_to_rtl)
	{
	  emit_move_insn (op0, x);
	  emit_move_insn (op1, y);
	}
      else
	{
	  gcc_assert (rtx_equal_p (op0, x));
	  gcc_assert (rtx_equal_p (op1, y));
	}
      emit_insn (gen_cmp_float (cc_reg, gen_rtx_COMPARE (mode, op0, op1)));
    }
  else
    emit_insn (gen_rtx_SET (omode, cc_reg,
			    gen_rtx_COMPARE (mode, x, y)));
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
			    enum machine_mode mode, tree type,
			    int *pretend_size, int no_rtl)
{
  int first_anon_arg;
  CUMULATIVE_ARGS next_cum;

  /* We must treat `__builtin_va_alist' as an anonymous arg.  */

  next_cum = *get_cumulative_args (args_so_far);
  arc_function_arg_advance (pack_cumulative_args (&next_cum), mode, type, 1);
  first_anon_arg = next_cum;

  if (first_anon_arg < MAX_ARC_PARM_REGS)
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
arc_address_cost (rtx addr, enum machine_mode, addr_space_t, bool speed)
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
  return frame_insn (gen_rtx_SET (VOIDmode, dst, src));
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
struct arc_frame_info
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
  enum arc_function_type fn_type;
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

enum arc_function_type
arc_compute_function_type (struct function *fun)
{
  tree decl = fun->decl;
  tree a;
  enum arc_function_type fn_type = fun->machine->fn_type;

  if (fn_type != ARC_FUNCTION_UNKNOWN)
    return fn_type;

  /* Assume we have a normal function (not an interrupt handler).  */
  fn_type = ARC_FUNCTION_NORMAL;

  /* Now see if this is an interrupt handler.  */
  for (a = DECL_ATTRIBUTES (decl);
       a;
       a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a), args = TREE_VALUE (a);

      if (name == get_identifier ("interrupt")
	  && list_length (args) == 1
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  tree value = TREE_VALUE (args);

	  if (!strcmp (TREE_STRING_POINTER (value), "ilink1"))
	    fn_type = ARC_FUNCTION_ILINK1;
	  else if (!strcmp (TREE_STRING_POINTER (value), "ilink2"))
	    fn_type = ARC_FUNCTION_ILINK2;
	  else
	    gcc_unreachable ();
	  break;
	}
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
#define MUST_SAVE_REGISTER(regno, interrupt_p) \
(((regno) != RETURN_ADDR_REGNUM && (regno) != FRAME_POINTER_REGNUM \
  && (df_regs_ever_live_p (regno) && (!call_used_regs[regno] || interrupt_p))) \
 || (flag_pic && crtl->uses_pic_offset_table \
     && regno == PIC_OFFSET_TABLE_REGNUM) )

#define MUST_SAVE_RETURN_ADDR \
  (cfun->machine->frame_info.save_return_addr)

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
  enum arc_function_type fn_type;
  int interrupt_p;
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
  fn_type = arc_compute_function_type (cfun);
  interrupt_p = ARC_INTERRUPT_P (fn_type);

  for (regno = 0; regno <= 31; regno++)
    {
      if (MUST_SAVE_REGISTER (regno, interrupt_p))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
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
  if (MUST_SAVE_RETURN_ADDR)
    extra_size = 4;
  if (frame_pointer_needed)
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
     A5 Frame: pretend_size <blink> reg_size <fp> var_size args_size <--sp
  */
  reg_offset = (total_size - (pretend_size + reg_size + extra_size)
		+ (frame_pointer_needed ? 4 : 0));

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
	      gcc_assert (epilogue_p || abs (*first_offset <= 127));
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
		XVECEXP (insn, 0, i) = gen_rtx_SET (VOIDmode, reg, mem);
	      else
		XVECEXP (insn, 0, i) = gen_rtx_SET (VOIDmode, mem, reg);
	      gmask = gmask & ~(1L << r);
	    }
	  if (epilogue_p == 2)
	    sibthunk_insn = insn;
	  else
	    frame_insn (insn);
	  offset += off;
	}

      for (regno = 0; regno <= 31; regno++)
	{
	  if ((gmask & (1L << regno)) != 0)
	    {
	      rtx reg = gen_rtx_REG (SImode, regno);
	      rtx addr, mem;

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
	      mem = gen_frame_mem (SImode, addr);
	      if (epilogue_p)
		frame_move_inc (reg, mem, base_reg, addr);
	      else
		frame_move_inc (mem, reg, base_reg, addr);
	      offset += UNITS_PER_WORD;
	    } /* if */
	} /* for */
    }/* if */
  if (sibthunk_insn)
    {
      rtx r12 = gen_rtx_REG (Pmode, 12);

      frame_insn (gen_rtx_SET (VOIDmode, r12, GEN_INT (offset)));
      XVECEXP (sibthunk_insn, 0, 0) = ret_rtx;
      XVECEXP (sibthunk_insn, 0, 1)
	= gen_rtx_SET (VOIDmode, stack_pointer_rtx,
		       gen_rtx_PLUS (Pmode, stack_pointer_rtx, r12));
      sibthunk_insn = emit_jump_insn (sibthunk_insn);
      RTX_FRAME_RELATED_P (sibthunk_insn) = 1;
    }
} /* arc_save_restore */


int arc_return_address_regs[4]
  = {0, RETURN_ADDR_REGNUM, ILINK1_REGNUM, ILINK2_REGNUM};

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

  /* The home-grown ABI says link register is saved first.  */
  if (MUST_SAVE_RETURN_ADDR)
    {
      rtx ra = gen_rtx_REG (SImode, RETURN_ADDR_REGNUM);
      rtx mem = gen_frame_mem (Pmode, gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx));

      frame_move_inc (mem, ra, stack_pointer_rtx, 0);
      frame_size_to_allocate -= UNITS_PER_WORD;

    } /* MUST_SAVE_RETURN_ADDR */

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler) for ARCompact ISA.  */
  if (cfun->machine->frame_info.reg_size)
    {
      first_offset = -cfun->machine->frame_info.reg_size;
      /* N.B. FRAME_POINTER_MASK and RETURN_ADDR_MASK are cleared in gmask.  */
      arc_save_restore (stack_pointer_rtx, gmask, 0, &first_offset);
      frame_size_to_allocate -= cfun->machine->frame_info.reg_size;
    }


  /* Save frame pointer if needed.  */
  if (frame_pointer_needed)
    {
      rtx addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			       GEN_INT (-UNITS_PER_WORD + first_offset));
      rtx mem = gen_frame_mem (Pmode, gen_rtx_PRE_MODIFY (Pmode,
							  stack_pointer_rtx,
							  addr));
      frame_move_inc (mem, frame_pointer_rtx, stack_pointer_rtx, 0);
      frame_size_to_allocate -= UNITS_PER_WORD;
      first_offset = 0;
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
    frame_stack_add ((HOST_WIDE_INT) 0 - frame_size_to_allocate);

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
  enum arc_function_type fn_type = arc_compute_function_type (cfun);

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

  size_to_deallocate = size;

  frame_size = size - (pretend_size +
		       cfun->machine->frame_info.reg_size +
		       cfun->machine->frame_info.extra_size);

  /* ??? There are lots of optimizations that can be done here.
     EG: Use fp to restore regs if it's closer.
     Maybe in time we'll do them all.  For now, always restore regs from
     sp, but don't restore sp if we don't have to.  */

  if (!can_trust_sp_p)
    gcc_assert (frame_pointer_needed);

  /* Restore stack pointer to the beginning of saved register area for
     ARCompact ISA.  */
  if (frame_size)
    {
      if (frame_pointer_needed)
	frame_move (stack_pointer_rtx, frame_pointer_rtx);
      else
	first_offset = frame_size;
      size_to_deallocate -= frame_size;
    }
  else if (!can_trust_sp_p)
    frame_stack_add (-frame_size);


  /* Restore any saved registers.  */
  if (frame_pointer_needed)
    {
	  rtx addr = gen_rtx_POST_INC (Pmode, stack_pointer_rtx);

	  frame_move_inc (frame_pointer_rtx, gen_frame_mem (Pmode, addr),
			  stack_pointer_rtx, 0);
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
	    goto epilogue_done;
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
       || (MUST_SAVE_RETURN_ADDR
	   && !SMALL_INT ((cfun->machine->frame_info.reg_size + first_offset) >> 2)
	   && cfun->machine->frame_info.gmask))
    {
      frame_stack_add (first_offset);
      first_offset = 0;
    }
  if (MUST_SAVE_RETURN_ADDR)
    {
      rtx ra = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
      int ra_offs = cfun->machine->frame_info.reg_size + first_offset;
      rtx addr = plus_constant (Pmode, stack_pointer_rtx, ra_offs);

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
	  first_offset = 0;
	  size_to_deallocate -= cfun->machine->frame_info.reg_size;
	}
      else if (!ra_offs && size_to_deallocate == UNITS_PER_WORD)
	{
	  addr = gen_rtx_POST_INC (Pmode, addr);
	  size_to_deallocate = 0;
	}
      frame_move_inc (ra, gen_frame_mem (Pmode, addr), stack_pointer_rtx, addr);
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
 epilogue_done:
  if (!TARGET_EPILOGUE_CFI)
    {
      rtx insn;

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	RTX_FRAME_RELATED_P (insn) = 0;
    }
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
  pat = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, pat), ARC_UNSPEC_GOT);
  pat = gen_rtx_CONST (Pmode, pat);

  pat = gen_rtx_SET (VOIDmode, baseptr_rtx, pat);

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
  enum machine_mode mode = GET_MODE (shift);
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

    case 'M':
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, "%d",exact_log2(~INTVAL (x)) );
      else
	output_operand_lossage ("invalid operand to %%M code");

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
      if (final_sequence && XVECLEN (final_sequence, 0) != 1)
	{
	  rtx jump = XVECEXP (final_sequence, 0, 0);
	  rtx delay = XVECEXP (final_sequence, 0, 1);

	  /* For TARGET_PAD_RETURN we might have grabbed the delay insn.  */
	  if (INSN_DELETED_P (delay))
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
	      || CALL_P (XVECEXP (final_sequence, 0, 0))
	      || simplejump_p (XVECEXP (final_sequence, 0, 0)))
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
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 4));
	  else if (output_scaled)
	    {
	      rtx addr = XEXP (x, 0);
	      int size = GET_MODE_SIZE (GET_MODE (x));

	      output_address (plus_constant (Pmode, XEXP (addr, 0),
					     ((INTVAL (XEXP (addr, 1)) + 4)
					      >> (size == 2 ? 1 : 2))));
	      output_scaled = 0;
	    }
	  else
	    output_address (plus_constant (Pmode, XEXP (x, 0), 4));
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
	  rtx first, second;

	  split_double (x, &first, &second);

	  if((WORDS_BIG_ENDIAN) == 0)
	      fprintf (file, "0x%08lx",
		       code == 'L' ? INTVAL (first) : INTVAL (second));
	  else
	      fprintf (file, "0x%08lx",
		       code == 'L' ? INTVAL (second) : INTVAL (first));


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
	    case QImode: fputs ("b", file); return;
	    case HImode: fputs ("w", file); return;
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
	    output_address (plus_constant (Pmode, XEXP (addr, 0), size)); break;
	  case PRE_DEC: case POST_DEC:
	    output_address (plus_constant (Pmode, XEXP (addr, 0), -size));
	    break;
	  case PRE_MODIFY: case POST_MODIFY:
	    output_address (XEXP (addr, 1)); break;
	  case PLUS:
	    if (output_scaled)
	      {
		output_address (plus_constant (Pmode, XEXP (addr, 0),
					       (INTVAL (XEXP (addr, 1))
						>> (size == 2 ? 1 : 2))));
		output_scaled = 0;
	      }
	    else
	      output_address (addr);
	    break;
	  default:
	    if (flag_pic && CONSTANT_ADDRESS_P (addr))
	      arc_output_pic_addr_const (file, addr, code);
	    else
	      output_address (addr);
	    break;
	  }
	fputc (']', file);
	break;
      }
    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  REAL_VALUE_TYPE d;
	  long l;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  REAL_VALUE_TO_TARGET_SINGLE (d, l);
	  fprintf (file, "0x%08lx", l);
	  break;
	}
      /* Fall through.  Let output_addr_const deal with it.  */
    default :
      if (flag_pic)
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

	gcc_assert (GET_CODE (XEXP (c, 0)) == SYMBOL_REF);
	gcc_assert (GET_CODE (XEXP (c, 1)) == CONST_INT);

	output_address(XEXP(addr,0));

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

/* Called via walk_stores.  DATA points to a hash table we can use to
   establish a unique SYMBOL_REF for each counter, which corresponds to
   a caller-callee pair.
   X is a store which we want to examine for an UNSPEC_PROF, which
   would be an address loaded into a register, or directly used in a MEM.
   If we found an UNSPEC_PROF, if we encounter a new counter the first time,
   write out a description and a data allocation for a 32 bit counter.
   Also, fill in the appropriate symbol_ref into each UNSPEC_PROF instance.  */

static void
write_profile_sections (rtx dest ATTRIBUTE_UNUSED, rtx x, void *data)
{
  rtx *srcp, src;
  htab_t htab = (htab_t) data;
  rtx *slot;

  if (GET_CODE (x) != SET)
    return;
  srcp = &SET_SRC (x);
  if (MEM_P (*srcp))
    srcp = &XEXP (*srcp, 0);
  else if (MEM_P (SET_DEST (x)))
    srcp = &XEXP (SET_DEST (x), 0);
  src = *srcp;
  if (GET_CODE (src) != CONST)
    return;
  src = XEXP (src, 0);
  if (GET_CODE (src) != UNSPEC || XINT (src, 1) != UNSPEC_PROF)
    return;

  gcc_assert (XVECLEN (src, 0) == 3);
  if (!htab_elements (htab))
    {
      output_asm_insn (".section .__arc_profile_desc, \"a\"\n"
		       "\t.long %0 + 1\n",
		       &XVECEXP (src, 0, 0));
    }
  slot = (rtx *) htab_find_slot (htab, src, INSERT);
  if (*slot == HTAB_EMPTY_ENTRY)
    {
      static int count_nr;
      char buf[24];
      rtx count;

      *slot = src;
      sprintf (buf, "__prof_count%d", count_nr++);
      count = gen_rtx_SYMBOL_REF (Pmode, xstrdup (buf));
      XVECEXP (src, 0, 2) = count;
      output_asm_insn (".section\t.__arc_profile_desc, \"a\"\n"
		       "\t.long\t%1\n"
		       "\t.section\t.__arc_profile_counters, \"aw\"\n"
		       "\t.type\t%o2, @object\n"
		       "\t.size\t%o2, 4\n"
		       "%o2:\t.zero 4",
		       &XVECEXP (src, 0, 0));
      *srcp = count;
    }
  else
    *srcp = XVECEXP (*slot, 0, 2);
}

/* Hash function for UNSPEC_PROF htab.  Use both the caller's name and
   the callee's name (if known).  */

static hashval_t
unspec_prof_hash (const void *x)
{
  const_rtx u = (const_rtx) x;
  const_rtx s1 = XVECEXP (u, 0, 1);

  return (htab_hash_string (XSTR (XVECEXP (u, 0, 0), 0))
	  ^ (s1->code == SYMBOL_REF ? htab_hash_string (XSTR (s1, 0)) : 0));
}

/* Equality function for UNSPEC_PROF htab.  Two pieces of UNSPEC_PROF rtl
   shall refer to the same counter if both caller name and callee rtl
   are identical.  */

static int
unspec_prof_htab_eq (const void *x, const void *y)
{
  const_rtx u0 = (const_rtx) x;
  const_rtx u1 = (const_rtx) y;
  const_rtx s01 = XVECEXP (u0, 0, 1);
  const_rtx s11 = XVECEXP (u1, 0, 1);

  return (!strcmp (XSTR (XVECEXP (u0, 0, 0), 0),
		   XSTR (XVECEXP (u1, 0, 0), 0))
	  && rtx_equal_p (s01, s11));
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
arc_ccfsm_advance (rtx insn, struct arc_ccfsm *state)
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
  rtx start_insn = insn;

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
      rtx this_insn = start_insn, label = 0;

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
      if (NEXT_INSN (PREV_INSN (insn)) != insn
	  && state->state == 0 && !INSN_ANNULLED_BRANCH_P (insn))
	{
	  this_insn = NEXT_INSN (this_insn);
	  gcc_assert (NEXT_INSN (NEXT_INSN (PREV_INSN (start_insn)))
		      == NEXT_INSN (this_insn));
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

	  scanbody = PATTERN (this_insn);

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
      state->target_insn = NULL_RTX;
    }
}

/* We are considering a conditional branch with the condition COND.
   Check if we want to conditionalize a delay slot insn, and if so modify
   the ccfsm state accordingly.
   REVERSE says branch will branch when the condition is false.  */
void
arc_ccfsm_record_condition (rtx cond, bool reverse, rtx jump,
			    struct arc_ccfsm *state)
{
  rtx seq_insn = NEXT_INSN (PREV_INSN (jump));
  if (!state)
    state = &arc_ccfsm_current;

  gcc_assert (state->state == 0);
  if (seq_insn != jump)
    {
      rtx insn = XVECEXP (PATTERN (seq_insn), 0, 1);

      if (!INSN_DELETED_P (insn)
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
arc_ccfsm_post_advance (rtx insn, struct arc_ccfsm *state)
{
  if (LABEL_P (insn))
    arc_ccfsm_at_label ("L", CODE_LABEL_NUMBER (insn), state);
  else if (JUMP_P (insn)
	   && GET_CODE (PATTERN (insn)) != ADDR_VEC
	   && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
	   && get_attr_type (insn) == TYPE_BRANCH)
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

static rtx
arc_next_active_insn (rtx insn, struct arc_ccfsm *statep)
{
  rtx pat;

  do
    {
      if (statep)
	arc_ccfsm_post_advance (insn, statep);
      insn = NEXT_INSN (insn);
      if (!insn || BARRIER_P (insn))
	return NULL_RTX;
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
	return NULL_RTX;
      if (GET_CODE (pat) == SEQUENCE)
	return XVECEXP (pat, 0, 0);
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
arc_verify_short (rtx insn, int, int check_attr)
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
  rtx insn = current_output_insn;

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
arc_final_prescan_insn (rtx insn, rtx *opvec ATTRIBUTE_UNUSED,
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
  return to == FRAME_POINTER_REGNUM || !arc_frame_pointer_required ();
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
arc_rtx_costs (rtx x, int code, int outer_code, int opno ATTRIBUTE_UNUSED,
	       int *total, bool speed)
{
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
	rtx high, low;

	if (TARGET_DPFP)
	  {
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }
	/* FIXME: correct the order of high,low */
	split_double (x, &high, &low);
	*total = COSTS_N_INSNS (!SMALL_INT (INTVAL (high))
				+ !SMALL_INT (INTVAL (low)));
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
			 + rtx_cost (XEXP (x, 1), (enum rtx_code) code, 0, speed));
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
      else if (TARGET_MUL64_SET || (TARGET_ARC700 && !TARGET_NOMPY_SET))
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (2);
      return false;
    case PLUS:
      if (GET_CODE (XEXP (x, 0)) == MULT
	  && _2_4_8_operand (XEXP (XEXP (x, 0), 1), VOIDmode))
	{
	  *total += (rtx_cost (XEXP (x, 1), PLUS, 0, speed)
		     + rtx_cost (XEXP (XEXP (x, 0), 0), PLUS, 1, speed));
	  return true;
	}
      return false;
    case MINUS:
      if (GET_CODE (XEXP (x, 1)) == MULT
	  && _2_4_8_operand (XEXP (XEXP (x, 1), 1), VOIDmode))
	{
	  *total += (rtx_cost (XEXP (x, 0), PLUS, 0, speed)
		     + rtx_cost (XEXP (XEXP (x, 1), 0), PLUS, 1, speed));
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
	    *total = (rtx_cost (XEXP (op0, 0), SET, 1, speed)
		      + rtx_cost (XEXP (op0, 2), SET, 1, speed));
	    return true;
	  }
	if (GET_CODE (op0) == AND && op1 == const0_rtx
	    && satisfies_constraint_C1p (XEXP (op0, 1)))
	  {
	    /* bmsk.f */
	    *total = rtx_cost (XEXP (op0, 0), SET, 1, speed);
	    return true;
	  }
	/* add.f  */
	if (GET_CODE (op1) == NEG)
	  {
	    /* op0 might be constant, the inside of op1 is rather
	       unlikely to be so.  So swapping the operands might lower
	       the cost.  */
	    *total = (rtx_cost (op0, PLUS, 1, speed)
		      + rtx_cost (XEXP (op1, 0), PLUS, 0, speed));
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

	  *total = (rtx_cost (XEXP (op0, 0), SET, 1, speed)
		    + rtx_cost (XEXP (op0, 2), SET, 1, speed));
	  return true;
	}
      /* Fall through.  */
    /* scc_insn expands into two insns.  */
    case GTU: case GEU: case LEU:
      if (GET_MODE (x) == SImode)
	*total += COSTS_N_INSNS (1);
      return false;
    case LTU: /* might use adc.  */
      if (GET_MODE (x) == SImode)
	*total += COSTS_N_INSNS (1) - 1;
      return false;
    default:
      return false;
    }
}

/* Return true if ADDR is an address that needs to be expressed as an
   explicit sum of pcl + offset.  */

bool
arc_legitimate_pc_offset_p (rtx addr)
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
  return (GET_CODE (addr) == UNSPEC
	  && XVECLEN (addr, 0) == 1
	  && XINT (addr, 1) == ARC_UNSPEC_GOT
	  && GET_CODE (XVECEXP (addr, 0, 0)) == SYMBOL_REF);
}

/* Return true if ADDR is a valid pic address.
   A valid pic address on arc should look like
   const (unspec (SYMBOL_REF/LABEL) (ARC_UNSPEC_GOTOFF/ARC_UNSPEC_GOT))  */

bool
arc_legitimate_pic_addr_p (rtx addr)
{
  if (GET_CODE (addr) == LABEL_REF)
    return true;
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

  /* Must be @GOT or @GOTOFF.  */
  if (XINT (addr, 1) != ARC_UNSPEC_GOT
      && XINT (addr, 1) != ARC_UNSPEC_GOTOFF)
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

/* Legitimize a pic address reference in ORIG.
   The return value is the legitimated address.
   If OLDX is non-zero, it is the target to assign the address to first.  */

rtx
arc_legitimize_pic_address (rtx orig, rtx oldx)
{
  rtx addr = orig;
  rtx pat = orig;
  rtx base;

  if (oldx == orig)
    oldx = NULL;

  if (GET_CODE (addr) == LABEL_REF)
    ; /* Do nothing.  */
  else if (GET_CODE (addr) == SYMBOL_REF
	   && (CONSTANT_POOL_ADDRESS_P (addr)
	       || SYMBOL_REF_LOCAL_P (addr)))
    {
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      /* FIXME: if we had a way to emit pc-relative adds that don't
	 create a GOT entry, we could do without the use of the gp register.  */
      crtl->uses_pic_offset_table = 1;
      pat = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), ARC_UNSPEC_GOTOFF);
      pat = gen_rtx_CONST (Pmode, pat);
      pat = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, pat);

      if (oldx == NULL)
	oldx = gen_reg_rtx (Pmode);

      if (oldx != 0)
	{
	  emit_move_insn (oldx, pat);
	  pat = oldx;
	}

    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      /* This symbol must be referenced via a load from the
	 Global Offset Table (@GOTPC).  */

      pat = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), ARC_UNSPEC_GOT);
      pat = gen_rtx_CONST (Pmode, pat);
      pat = gen_const_mem (Pmode, pat);

      if (oldx == 0)
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
	    }
	  else
	    gcc_assert (GET_CODE (addr) == PLUS);
	}

      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant offset from a @GOTOFF
	     symbol reference.  */
	  if ((GET_CODE (op0) == LABEL_REF
	       || (GET_CODE (op0) == SYMBOL_REF
		   && (CONSTANT_POOL_ADDRESS_P (op0)
		       || SYMBOL_REF_LOCAL_P (op0))))
	      && GET_CODE (op1) == CONST_INT)
	    {
	      /* FIXME: like above, could do without gp reference.  */
	      crtl->uses_pic_offset_table = 1;
	      pat
		= gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0), ARC_UNSPEC_GOTOFF);
	      pat = gen_rtx_PLUS (Pmode, pat, op1);
	      pat = gen_rtx_CONST (Pmode, pat);
	      pat = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, pat);

	      if (oldx != 0)
		{
		  emit_move_insn (oldx, pat);
		  pat = oldx;
		}
	    }
	  else
	    {
	      base = arc_legitimize_pic_address (XEXP (addr, 0), oldx);
	      pat  = arc_legitimize_pic_address (XEXP (addr, 1),
					     base == oldx ? NULL_RTX : oldx);

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
    }

 return pat;
}

/* Output address constant X to FILE, taking PIC into account.  */

void
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
      gcc_assert (XVECLEN (x, 0) == 1);
      if (XINT (x, 1) == ARC_UNSPEC_GOT)
	fputs ("pcl,", file);
      arc_output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
      switch (XINT (x, 1))
	{
	case ARC_UNSPEC_GOT:
	  fputs ("@gotpc", file);
	  break;
	case ARC_UNSPEC_GOTOFF:
	  fputs ("@gotoff", file);
	  break;
	case ARC_UNSPEC_PLT:
	  fputs ("@plt", file);
	  break;
	default:
	  output_operand_lossage ("invalid UNSPEC as operand: %d", XINT (x,1));
	  break;
	}
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

void
emit_pic_move (rtx *operands, enum machine_mode)
{
  rtx temp = reload_in_progress ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM && SYMBOLIC_CONST (operands[1]))
    operands[1] = force_reg (Pmode, operands[1]);
  else
    operands[1] = arc_legitimize_pic_address (operands[1], temp);
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
arc_arg_partial_bytes (cumulative_args_t cum_v, enum machine_mode mode,
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
arc_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		  const_tree type ATTRIBUTE_UNUSED, bool named ATTRIBUTE_UNUSED)
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
arc_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			  const_tree type, bool named ATTRIBUTE_UNUSED)
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
  enum machine_mode mode = TYPE_MODE (valtype);
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

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

bool
arc_legitimate_pic_operand_p (rtx x)
{
  return !arc_raw_symbolic_reference_mentioned_p (x, true);
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

bool
arc_legitimate_constant_p (enum machine_mode, rtx x)
{
  if (!flag_pic)
    return true;

  switch (GET_CODE (x))
    {
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    return false;
	  x = XEXP (x, 0);
	}

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (x) == UNSPEC)
	switch (XINT (x, 1))
	  {
	  case ARC_UNSPEC_PLT:
	  case ARC_UNSPEC_GOTOFF:
	  case ARC_UNSPEC_GOT:
	  case UNSPEC_PROF:
	    return true;

	  default:
	    gcc_unreachable ();
	  }

      /* We must have drilled down to a symbol.  */
      if (arc_raw_symbolic_reference_mentioned_p (x, false))
	return false;

      /* Return true.  */
      break;

    case LABEL_REF:
    case SYMBOL_REF:
      return false;

    default:
      break;
    }

  /* Otherwise we handle everything else in the move patterns.  */
  return true;
}

static bool
arc_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  if (RTX_OK_FOR_BASE_P (x, strict))
     return true;
  if (LEGITIMATE_OFFSET_ADDRESS_P (mode, x, TARGET_INDEXED_LOADS, strict))
     return true;
  if (LEGITIMATE_SCALED_ADDRESS_P (mode, x, strict))
    return true;
  if (LEGITIMATE_SMALL_DATA_ADDRESS_P (x))
     return true;
  if (GET_CODE (x) == CONST_INT && LARGE_INT (INTVAL (x)))
     return true;
  if ((GET_MODE_SIZE (mode) != 16)
      && (GET_CODE (x) == SYMBOL_REF
	  || GET_CODE (x) == LABEL_REF
	  || GET_CODE (x) == CONST))
    {
      if (!flag_pic || arc_legitimate_pic_addr_p (x))
	return true;
    }
  if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == PRE_INC
       || GET_CODE (x) == POST_DEC || GET_CODE (x) == POST_INC)
      && RTX_OK_FOR_BASE_P (XEXP (x, 0), strict))
    return true;
      /* We're restricted here by the `st' insn.  */
  if ((GET_CODE (x) == PRE_MODIFY || GET_CODE (x) == POST_MODIFY)
      && GET_CODE (XEXP ((x), 1)) == PLUS
      && rtx_equal_p (XEXP ((x), 0), XEXP (XEXP (x, 1), 0))
      && LEGITIMATE_OFFSET_ADDRESS_P (QImode, XEXP (x, 1),
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
     loads.  */
  /* Scaled indices are scaled by the access mode; likewise for scaled
     offsets, which are needed for maximum offset stores.  */
  if (GET_CODE (addr) == PLUS
      && (GET_CODE (XEXP ((addr), 0)) == MULT
	  || (CONST_INT_P (XEXP ((addr), 1))
	      && !SMALL_INT (INTVAL (XEXP ((addr), 1))))))
    return true;
  return false;
}

/* Determine if it's legal to put X into the constant pool.  */

static bool
arc_cannot_force_const_mem (enum machine_mode mode, rtx x)
{
  return !arc_legitimate_constant_p (mode, x);
}


/* Generic function to define a builtin.  */
#define def_mbuiltin(MASK, NAME, TYPE, CODE)				\
  do									\
    {									\
       if (MASK)							\
	  add_builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD, NULL, NULL_TREE); \
    }									\
  while (0)


static void
arc_init_builtins (void)
{
    tree endlink = void_list_node;

    tree void_ftype_void
	= build_function_type (void_type_node,
			       endlink);

    tree int_ftype_int
	= build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node, endlink));

    tree pcvoid_type_node
	= build_pointer_type (build_qualified_type (void_type_node, TYPE_QUAL_CONST));
    tree int_ftype_pcvoid_int
	= build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, pcvoid_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
				    endlink)));

    tree int_ftype_short_int
	= build_function_type (integer_type_node,
			       tree_cons (NULL_TREE, short_integer_type_node, endlink));

    tree void_ftype_int_int
	= build_function_type (void_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
					  tree_cons (NULL_TREE, integer_type_node, endlink)));
    tree void_ftype_usint_usint
	= build_function_type (void_type_node,
			       tree_cons (NULL_TREE, long_unsigned_type_node,
					  tree_cons (NULL_TREE, long_unsigned_type_node, endlink)));

    tree int_ftype_int_int
	= build_function_type (integer_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
					  tree_cons (NULL_TREE, integer_type_node, endlink)));

    tree usint_ftype_usint
	= build_function_type (long_unsigned_type_node,
			   tree_cons (NULL_TREE, long_unsigned_type_node, endlink));

    tree void_ftype_usint
	= build_function_type (void_type_node,
			   tree_cons (NULL_TREE, long_unsigned_type_node, endlink));

    /* Add the builtins.  */
    def_mbuiltin (1,"__builtin_arc_nop", void_ftype_void, ARC_BUILTIN_NOP);
    def_mbuiltin (TARGET_NORM, "__builtin_arc_norm", int_ftype_int, ARC_BUILTIN_NORM);
    def_mbuiltin (TARGET_NORM, "__builtin_arc_normw", int_ftype_short_int, ARC_BUILTIN_NORMW);
    def_mbuiltin (TARGET_SWAP, "__builtin_arc_swap", int_ftype_int, ARC_BUILTIN_SWAP);
    def_mbuiltin (TARGET_MUL64_SET,"__builtin_arc_mul64", void_ftype_int_int, ARC_BUILTIN_MUL64);
    def_mbuiltin (TARGET_MUL64_SET,"__builtin_arc_mulu64", void_ftype_usint_usint, ARC_BUILTIN_MULU64);
    def_mbuiltin (1,"__builtin_arc_rtie", void_ftype_void, ARC_BUILTIN_RTIE);
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_sync", void_ftype_void, ARC_BUILTIN_SYNC);
    def_mbuiltin ((TARGET_EA_SET),"__builtin_arc_divaw", int_ftype_int_int, ARC_BUILTIN_DIVAW);
    def_mbuiltin (1,"__builtin_arc_brk", void_ftype_void, ARC_BUILTIN_BRK);
    def_mbuiltin (1,"__builtin_arc_flag", void_ftype_usint, ARC_BUILTIN_FLAG);
    def_mbuiltin (1,"__builtin_arc_sleep", void_ftype_usint, ARC_BUILTIN_SLEEP);
    def_mbuiltin (1,"__builtin_arc_swi", void_ftype_void, ARC_BUILTIN_SWI);
    def_mbuiltin (1,"__builtin_arc_core_read", usint_ftype_usint, ARC_BUILTIN_CORE_READ);
    def_mbuiltin (1,"__builtin_arc_core_write", void_ftype_usint_usint, ARC_BUILTIN_CORE_WRITE);
    def_mbuiltin (1,"__builtin_arc_lr", usint_ftype_usint, ARC_BUILTIN_LR);
    def_mbuiltin (1,"__builtin_arc_sr", void_ftype_usint_usint, ARC_BUILTIN_SR);
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_trap_s", void_ftype_usint, ARC_BUILTIN_TRAP_S);
    def_mbuiltin (TARGET_ARC700,"__builtin_arc_unimp_s", void_ftype_void, ARC_BUILTIN_UNIMP_S);
    def_mbuiltin (1,"__builtin_arc_aligned", int_ftype_pcvoid_int, ARC_BUILTIN_ALIGNED);

    if (TARGET_SIMD_SET)
      arc_init_simd_builtins ();
}

static rtx arc_expand_simd_builtin (tree, rtx, rtx, enum machine_mode, int);

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
arc_expand_builtin (tree exp,
		    rtx target,
		    rtx subtarget,
		    enum machine_mode mode,
		    int ignore)
{
  tree              fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree              arg0;
  tree              arg1;
  rtx               op0;
  rtx               op1;
  int               fcode = DECL_FUNCTION_CODE (fndecl);
  int               icode;
  enum machine_mode mode0;
  enum machine_mode mode1;

  if (fcode > ARC_SIMD_BUILTIN_BEGIN && fcode < ARC_SIMD_BUILTIN_END)
    return arc_expand_simd_builtin (exp, target, subtarget, mode, ignore);

  switch (fcode)
    {
    case ARC_BUILTIN_NOP:
      emit_insn (gen_nop ());
      return NULL_RTX;

    case ARC_BUILTIN_NORM:
      icode = CODE_FOR_clrsbsi2;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 =  insn_data[icode].operand[1].mode;
      target = gen_reg_rtx (SImode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_clrsbsi2 (target, op0));
      return target;

    case ARC_BUILTIN_NORMW:

      /* FIXME : This should all be HImode, not SImode.  */
      icode = CODE_FOR_normw;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 =  insn_data[icode].operand[1].mode;
      target = gen_reg_rtx (SImode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, convert_to_mode (mode0, op0,0));

      emit_insn (gen_normw (target, op0));
      return target;

    case ARC_BUILTIN_MUL64:
      icode = CODE_FOR_mul64;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      mode0 =  insn_data[icode].operand[0].mode;
      mode1 =  insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      emit_insn (gen_mul64 (op0,op1));
      return NULL_RTX;

    case ARC_BUILTIN_MULU64:
      icode = CODE_FOR_mulu64;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      mode0 =  insn_data[icode].operand[0].mode;
      mode1 =  insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[0].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      emit_insn (gen_mulu64 (op0,op1));
      return NULL_RTX;

    case ARC_BUILTIN_RTIE:
      icode = CODE_FOR_rtie;
      emit_insn (gen_rtie (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_SYNC:
      icode = CODE_FOR_sync;
      emit_insn (gen_sync (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_SWAP:
      icode = CODE_FOR_swap;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 =  insn_data[icode].operand[1].mode;
      target = gen_reg_rtx (SImode);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_swap (target, op0));
      return target;

    case ARC_BUILTIN_DIVAW:
      icode = CODE_FOR_divaw;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      target = gen_reg_rtx (SImode);

      mode0 =  insn_data[icode].operand[0].mode;
      mode1 =  insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      emit_insn (gen_divaw (target, op0, op1));
      return target;

    case ARC_BUILTIN_BRK:
      icode = CODE_FOR_brk;
      emit_insn (gen_brk (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_SLEEP:
      icode = CODE_FOR_sleep;
      arg0 = CALL_EXPR_ARG (exp, 0);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[1].mode;

      emit_insn (gen_sleep (op0));
      return NULL_RTX;

    case ARC_BUILTIN_SWI:
      icode = CODE_FOR_swi;
      emit_insn (gen_swi (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_FLAG:
      icode = CODE_FOR_flag;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 =  insn_data[icode].operand[0].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_flag (op0));
      return NULL_RTX;

    case ARC_BUILTIN_CORE_READ:
      icode = CODE_FOR_core_read;
      arg0 = CALL_EXPR_ARG (exp, 0);
      target = gen_reg_rtx (SImode);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[1].mode;

      emit_insn (gen_core_read (target, op0));
      return target;

    case ARC_BUILTIN_CORE_WRITE:
      icode = CODE_FOR_core_write;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);

      fold (arg1);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      emit_insn (gen_core_write (op0, op1));
      return NULL_RTX;

    case ARC_BUILTIN_LR:
      icode = CODE_FOR_lr;
      arg0 = CALL_EXPR_ARG (exp, 0);
      target = gen_reg_rtx (SImode);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[1].mode;

      emit_insn (gen_lr (target, op0));
      return target;

    case ARC_BUILTIN_SR:
      icode = CODE_FOR_sr;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);

      fold (arg1);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      emit_insn (gen_sr (op0, op1));
      return NULL_RTX;

    case ARC_BUILTIN_TRAP_S:
      icode = CODE_FOR_trap_s;
      arg0 = CALL_EXPR_ARG (exp, 0);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[1].mode;

      /* We don't give an error for non-cost values here because
	 we still want to allow things to be fixed up by later inlining /
	 constant folding / dead code elimination.  */
      if  (CONST_INT_P (op0) && !satisfies_constraint_L (op0))
	{
	  /* Keep this message in sync with the one in arc.md:trap_s,
	     because *.md files don't get scanned by exgettext.  */
	  error ("operand to trap_s should be an unsigned 6-bit value");
	}
      emit_insn (gen_trap_s (op0));
      return NULL_RTX;

    case ARC_BUILTIN_UNIMP_S:
      icode = CODE_FOR_unimp_s;
      emit_insn (gen_unimp_s (const1_rtx));
      return NULL_RTX;

    case ARC_BUILTIN_ALIGNED:
      /* __builtin_arc_aligned (void* val, int alignval) */
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      fold (arg1);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      target = gen_reg_rtx (SImode);

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

    default:
      break;
    }

  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
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
    default:
	fatal_error("operand for sleep instruction must be an unsigned 6 bit compile-time constant");
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
      asm_fprintf (file, "\tadd\t%s, %s, %ld\n",
		   ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG, vcall_offset);
      asm_fprintf (file, "\tld\t%s, [%s]\n",
		   ARC_TEMP_SCRATCH_REG, ARC_TEMP_SCRATCH_REG);
      asm_fprintf (file, "\tadd\t%s, %s, %s\n", reg_names[this_regno],
		   reg_names[this_regno], ARC_TEMP_SCRATCH_REG);
    }

  fnaddr = XEXP (DECL_RTL (function), 0);

  if (arc_is_longcall_p (fnaddr))
    fputs ("\tj\t", file);
  else
    fputs ("\tb\t", file);
  assemble_name (file, XSTR (fnaddr, 0));
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

/* Emit profiling code for calling CALLEE.  Return true if a special
   call pattern needs to be generated.  */

bool
arc_profile_call (rtx callee)
{
  rtx from = XEXP (DECL_RTL (current_function_decl), 0);

  if (TARGET_UCB_MCOUNT)
    /* Profiling is done by instrumenting the callee.  */
    return false;

  if (CONSTANT_P (callee))
    {
      rtx count_ptr
	= gen_rtx_CONST (Pmode,
			 gen_rtx_UNSPEC (Pmode,
					 gen_rtvec (3, from, callee,
						    CONST0_RTX (Pmode)),
					 UNSPEC_PROF));
      rtx counter = gen_rtx_MEM (SImode, count_ptr);
      /* ??? The increment would better be done atomically, but as there is
	 no proper hardware support, that would be too expensive.  */
      emit_move_insn (counter,
		      force_reg (SImode, plus_constant (SImode, counter, 1)));
      return false;
    }
  else
    {
      rtx count_list_ptr
	= gen_rtx_CONST (Pmode,
			 gen_rtx_UNSPEC (Pmode,
					 gen_rtvec (3, from, CONST0_RTX (Pmode),
						    CONST0_RTX (Pmode)),
					 UNSPEC_PROF));
      emit_move_insn (gen_rtx_REG (Pmode, 8), count_list_ptr);
      emit_move_insn (gen_rtx_REG (Pmode, 9), callee);
      return true;
    }
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
      return (size == -1 || size > 8);
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
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       const_tree type,
		       bool named ATTRIBUTE_UNUSED)
{
  return (type != 0
	  && (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      || TREE_ADDRESSABLE (type)));
}


/* NULL if INSN insn is valid within a low-overhead loop.
   Otherwise return why doloop cannot be applied.  */

static const char *
arc_invalid_within_doloop (const_rtx insn)
{
  if (CALL_P (insn))
    return "Function call in the loop.";
  return NULL;
}

static int arc_reorg_in_progress = 0;

/* ARC's machince specific reorg function.  */

static void
arc_reorg (void)
{
  rtx insn, pattern;
  rtx pc_target;
  long offset;
  int changed;

  cfun->machine->arc_reorg_started = 1;
  arc_reorg_in_progress = 1;

  /* Emit special sections for profiling.  */
  if (crtl->profile)
    {
      section *save_text_section;
      rtx insn;
      int size = get_max_uid () >> 4;
      htab_t htab = htab_create (size, unspec_prof_hash, unspec_prof_htab_eq,
				 NULL);

      save_text_section = in_section;
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (NONJUMP_INSN_P (insn))
	  walk_stores (PATTERN (insn), write_profile_sections, htab);
      if (htab_elements (htab))
	in_section = 0;
      switch_to_section (save_text_section);
      htab_delete (htab);
    }

  /* Link up loop ends with their loop start.  */
  {
    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_CODE (insn) == JUMP_INSN
	  && recog_memoized (insn) == CODE_FOR_doloop_end_i)
	{
	  rtx top_label
	    = XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn), 0, 0)), 1), 0);
	  rtx num = GEN_INT (CODE_LABEL_NUMBER (top_label));
	  rtx lp, prev = prev_nonnote_insn (top_label);
	  rtx lp_simple = NULL_RTX;
	  rtx next = NULL_RTX;
	  rtx op0 = XEXP (XVECEXP (PATTERN (insn), 0, 1), 0);
	  HOST_WIDE_INT loop_end_id
	    = -INTVAL (XEXP (XVECEXP (PATTERN (insn), 0, 4), 0));
	  int seen_label = 0;

	  for (lp = prev;
	       (lp && NONJUMP_INSN_P (lp)
		&& recog_memoized (lp) != CODE_FOR_doloop_begin_i);
	       lp = prev_nonnote_insn (lp))
	    ;
	  if (!lp || !NONJUMP_INSN_P (lp)
	      || dead_or_set_regno_p (lp, LP_COUNT))
	    {
	      for (prev = next = insn, lp = NULL_RTX ; prev || next;)
		{
		  if (prev)
		    {
		      if (NONJUMP_INSN_P (prev)
			  && recog_memoized (prev) == CODE_FOR_doloop_begin_i
			  && (INTVAL (XEXP (XVECEXP (PATTERN (prev), 0, 5), 0))
			      == loop_end_id))
			{
			  lp = prev;
			  break;
			}
		      else if (LABEL_P (prev))
			seen_label = 1;
		      prev = prev_nonnote_insn (prev);
		    }
		  if (next)
		    {
		      if (NONJUMP_INSN_P (next)
			  && recog_memoized (next) == CODE_FOR_doloop_begin_i
			  && (INTVAL (XEXP (XVECEXP (PATTERN (next), 0, 5), 0))
			      == loop_end_id))
			{
			  lp = next;
			  break;
			}
		      next = next_nonnote_insn (next);
		    }
		}
	      prev = NULL_RTX;
	    }
	  else
	    lp_simple = lp;
	  if (lp && !dead_or_set_regno_p (lp, LP_COUNT))
	    {
	      rtx begin_cnt = XEXP (XVECEXP (PATTERN (lp), 0 ,3), 0);
	      if (INTVAL (XEXP (XVECEXP (PATTERN (lp), 0, 4), 0)))
		/* The loop end insn has been duplicated.  That can happen
		   when there is a conditional block at the very end of
		   the loop.  */
		goto failure;
	      /* If Register allocation failed to allocate to the right
		 register, There is no point into teaching reload to
		 fix this up with reloads, as that would cost more
		 than using an ordinary core register with the
		 doloop_fallback pattern.  */
	      if ((true_regnum (op0) != LP_COUNT || !REG_P (begin_cnt))
	      /* Likewise, if the loop setup is evidently inside the loop,
		 we loose.  */
		  || (!lp_simple && lp != next && !seen_label))
		{
		  remove_insn (lp);
		  goto failure;
		}
	      /* It is common that the optimizers copy the loop count from
		 another register, and doloop_begin_i is stuck with the
		 source of the move.  Making doloop_begin_i only accept "l"
		 is nonsentical, as this then makes reload evict the pseudo
		 used for the loop end.  The underlying cause is that the
		 optimizers don't understand that the register allocation for
		 doloop_begin_i should be treated as part of the loop.
		 Try to work around this problem by verifying the previous
		 move exists.  */
	      if (true_regnum (begin_cnt) != LP_COUNT)
		{
		  rtx mov, set, note;

		  for (mov = prev_nonnote_insn (lp); mov;
		       mov = prev_nonnote_insn (mov))
		    {
		      if (!NONJUMP_INSN_P (mov))
			mov = 0;
		      else if ((set = single_set (mov))
			  && rtx_equal_p (SET_SRC (set), begin_cnt)
			  && rtx_equal_p (SET_DEST (set), op0))
			break;
		    }
		  if (mov)
		    {
		      XEXP (XVECEXP (PATTERN (lp), 0 ,3), 0) = op0;
		      note = find_regno_note (lp, REG_DEAD, REGNO (begin_cnt));
		      if (note)
			remove_note (lp, note);
		    }
		  else
		    {
		      remove_insn (lp);
		      goto failure;
		    }
		}
	      XEXP (XVECEXP (PATTERN (insn), 0, 4), 0) = num;
	      XEXP (XVECEXP (PATTERN (lp), 0, 4), 0) = num;
	      if (next == lp)
		XEXP (XVECEXP (PATTERN (lp), 0, 6), 0) = const2_rtx;
	      else if (!lp_simple)
		XEXP (XVECEXP (PATTERN (lp), 0, 6), 0) = const1_rtx;
	      else if (prev != lp)
		{
		  remove_insn (lp);
		  add_insn_after (lp, prev, NULL);
		}
	      if (!lp_simple)
		{
		  XEXP (XVECEXP (PATTERN (lp), 0, 7), 0)
		    = gen_rtx_LABEL_REF (Pmode, top_label);
		  add_reg_note (lp, REG_LABEL_OPERAND, top_label);
		  LABEL_NUSES (top_label)++;
		}
	      /* We can avoid tedious loop start / end setting for empty loops
		 be merely setting the loop count to its final value.  */
	      if (next_active_insn (top_label) == insn)
		{
		  rtx lc_set
		    = gen_rtx_SET (VOIDmode,
				   XEXP (XVECEXP (PATTERN (lp), 0, 3), 0),
				   const0_rtx);

		  lc_set = emit_insn_before (lc_set, insn);
		  delete_insn (lp);
		  delete_insn (insn);
		  insn = lc_set;
		}
	      /* If the loop is non-empty with zero length, we can't make it
		 a zero-overhead loop.  That can happen for empty asms.  */
	      else
		{
		  rtx scan;

		  for (scan = top_label;
		       (scan && scan != insn
			&& (!NONJUMP_INSN_P (scan) || !get_attr_length (scan)));
		       scan = NEXT_INSN (scan));
		  if (scan == insn)
		    {
		      remove_insn (lp);
		      goto failure;
		    }
		}
	    }
	  else
	    {
	      /* Sometimes the loop optimizer makes a complete hash of the
		 loop.  If it were only that the loop is not entered at the
		 top, we could fix this up by setting LP_START with SR .
		 However, if we can't find the loop begin were it should be,
		 chances are that it does not even dominate the loop, but is
		 inside the loop instead.  Using SR there would kill
		 performance.
		 We use the doloop_fallback pattern here, which executes
		 in two cycles on the ARC700 when predicted correctly.  */
	    failure:
	      if (!REG_P (op0))
		{
		  rtx op3 = XEXP (XVECEXP (PATTERN (insn), 0, 5), 0);

		  emit_insn_before (gen_move_insn (op3, op0), insn);
		  PATTERN (insn)
		    = gen_doloop_fallback_m (op3, JUMP_LABEL (insn), op0);
		}
	      else
		XVEC (PATTERN (insn), 0)
		  = gen_rtvec (2, XVECEXP (PATTERN (insn), 0, 0),
			       XVECEXP (PATTERN (insn), 0, 1));
	      INSN_CODE (insn) = -1;
	    }
	}
    }

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
	}

      /* Call shorten_branches to calculate the insn lengths.  */
      shorten_branches (get_insns());
      cfun->machine->ccfsm_current_insn = NULL_RTX;

      if (!INSN_ADDRESSES_SET_P())
	  fatal_error ("Insn addresses not set after shorten_branches");

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
	  if (find_reg_note (insn, REG_CROSSING_JUMP, NULL_RTX))
	    continue;
	  offset = branch_dest (insn) - INSN_ADDRESSES (INSN_UID (insn));

	  if(offset > 253 || offset < -254)
	    continue;

	  pc_target = SET_SRC (pattern);

	  /* Now go back and search for the set cc insn.  */

	  label = XEXP (pc_target, 1);

	    {
	      rtx pat, scan, link_insn = NULL;

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
	      if (! link_insn)
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
		  brcc_insn = gen_rtx_SET (VOIDmode, pc_rtx, brcc_insn);
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
    fatal_error ("insn addresses not freed");

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

/* ??? Hack.  This should no really be here.  See PR32143.  */
static bool
arc_decl_anon_ns_mem_p (const_tree decl)
{
  while (1)
    {
      if (decl == NULL_TREE || decl == error_mark_node)
	return false;
      if (TREE_CODE (decl) == NAMESPACE_DECL
	  && DECL_NAME (decl) == NULL_TREE)
	return true;
      /* Classes and namespaces inside anonymous namespaces have
	 TREE_PUBLIC == 0, so we can shortcut the search.  */
      else if (TYPE_P (decl))
	return (TREE_PUBLIC (TYPE_NAME (decl)) == 0);
      else if (TREE_CODE (decl) == NAMESPACE_DECL)
	return (TREE_PUBLIC (decl) == 0);
      else
	decl = DECL_CONTEXT (decl);
    }
}

/* Implement TARGET_IN_SMALL_DATA_P.  Return true if it would be safe to
   access DECL using %gp_rel(...)($gp).  */

static bool
arc_in_small_data_p (const_tree decl)
{
  HOST_WIDE_INT size;

  if (TREE_CODE (decl) == STRING_CST || TREE_CODE (decl) == FUNCTION_DECL)
    return false;


  /* We don't yet generate small-data references for -mabicalls.  See related
     -G handling in override_options.  */
  if (TARGET_NO_SDATA_SET)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl) != 0)
    {
      const char *name;

      /* Reject anything that isn't in a known small-data section.  */
      name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (strcmp (name, ".sdata") != 0 && strcmp (name, ".sbss") != 0)
	return false;

      /* If a symbol is defined externally, the assembler will use the
	 usual -G rules when deciding how to implement macros.  */
      if (!DECL_EXTERNAL (decl))
	  return true;
    }
  /* Only global variables go into sdata section for now.  */
  else if (1)
    {
      /* Don't put constants into the small data section: we want them
	 to be in ROM rather than RAM.  */
      if (TREE_CODE (decl) != VAR_DECL)
	return false;

      if (TREE_READONLY (decl)
	  && !TREE_SIDE_EFFECTS (decl)
	  && (!DECL_INITIAL (decl) || TREE_CONSTANT (DECL_INITIAL (decl))))
	return false;

      /* TREE_PUBLIC might change after the first call, because of the patch
	 for PR19238.  */
      if (default_binds_local_p_1 (decl, 1)
	  || arc_decl_anon_ns_mem_p (decl))
	return false;

      /* To ensure -mvolatile-cache works
	 ld.di does not have a gp-relative variant.  */
      if (TREE_THIS_VOLATILE (decl))
	return false;
    }

  /* Disable sdata references to weak variables.  */
  if (DECL_WEAK (decl))
    return false;

  size = int_size_in_bytes (TREE_TYPE (decl));

/*   if (AGGREGATE_TYPE_P (TREE_TYPE (decl))) */
/*     return false; */

  /* Allow only <=4B long data types into sdata.  */
  return (size > 0 && size <= 4);
}

/* Return true if X is a small data address that can be rewritten
   as a gp+symref.  */

static bool
arc_rewrite_small_data_p (rtx x)
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	x = XEXP (x, 0);
    }

  return (GET_CODE (x) ==  SYMBOL_REF
	  && SYMBOL_REF_SMALL_P(x));
}

/* A for_each_rtx callback, used by arc_rewrite_small_data.  */

static int
arc_rewrite_small_data_1 (rtx *loc, void *data)
{
  if (arc_rewrite_small_data_p (*loc))
    {
      rtx top;

      gcc_assert (SDATA_BASE_REGNUM == PIC_OFFSET_TABLE_REGNUM);
      *loc = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, *loc);
      if (loc == data)
	return -1;
      top = *(rtx*) data;
      if (GET_CODE (top) == MEM && &XEXP (top, 0) == loc)
	; /* OK.  */
      else if (GET_CODE (top) == MEM
	  && GET_CODE (XEXP (top, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (top, 0), 0)) == MULT)
	*loc = force_reg (Pmode, *loc);
      else
	gcc_unreachable ();
      return -1;
    }

  if (GET_CODE (*loc) == PLUS
      && rtx_equal_p (XEXP (*loc, 0), pic_offset_table_rtx))
    return -1;

  return 0;
}

/* If possible, rewrite OP so that it refers to small data using
   explicit relocations.  */

rtx
arc_rewrite_small_data (rtx op)
{
  op = copy_insn (op);
  for_each_rtx (&op, arc_rewrite_small_data_1, &op);
  return op;
}

/* A for_each_rtx callback for small_data_pattern.  */

static int
small_data_pattern_1 (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (*loc) == PLUS
      && rtx_equal_p (XEXP (*loc, 0), pic_offset_table_rtx))
    return  -1;

  return arc_rewrite_small_data_p (*loc);
}

/* Return true if OP refers to small data symbols directly, not through
   a PLUS.  */

bool
small_data_pattern (rtx op, enum machine_mode)
{
  return (GET_CODE (op) != SEQUENCE
	  && for_each_rtx (&op, small_data_pattern_1, 0));
}

/* Return true if OP is an acceptable memory operand for ARCompact
   16-bit gp-relative load instructions.
   op shd look like : [r26, symref@sda]
   i.e. (mem (plus (reg 26) (symref with smalldata flag set))
  */
/* volatile cache option still to be handled.  */

bool
compact_sda_memory_operand (rtx op, enum machine_mode mode)
{
  rtx addr;
  int size;

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

  return LEGITIMATE_SMALL_DATA_ADDRESS_P  (addr);
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


































/* SIMD builtins support.  */
enum simd_insn_args_type {
  Va_Vb_Vc,
  Va_Vb_rlimm,
  Va_Vb_Ic,
  Va_Vb_u6,
  Va_Vb_u8,
  Va_rlimm_u8,

  Va_Vb,

  void_rlimm,
  void_u6,

  Da_u3_rlimm,
  Da_rlimm_rlimm,

  Va_Ib_u8,
  void_Va_Ib_u8,

  Va_Vb_Ic_u8,
  void_Va_u3_Ib_u8
};

struct builtin_description
{
  enum simd_insn_args_type args_type;
  const enum insn_code     icode;
  const char * const       name;
  const enum arc_builtins  code;
};

static const struct builtin_description arc_simd_builtin_desc_list[] =
{
  /* VVV builtins go first.  */
#define SIMD_BUILTIN(type, code, string, builtin) \
  { type,CODE_FOR_##code, "__builtin_arc_" string, \
    ARC_SIMD_BUILTIN_##builtin },

  SIMD_BUILTIN (Va_Vb_Vc,    vaddaw_insn,   "vaddaw",     VADDAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vaddw_insn,    "vaddw",      VADDW)
  SIMD_BUILTIN (Va_Vb_Vc,      vavb_insn,     "vavb",       VAVB)
  SIMD_BUILTIN (Va_Vb_Vc,     vavrb_insn,    "vavrb",      VAVRB)
  SIMD_BUILTIN (Va_Vb_Vc,    vdifaw_insn,   "vdifaw",     VDIFAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vdifw_insn,    "vdifw",      VDIFW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmaxaw_insn,   "vmaxaw",     VMAXAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmaxw_insn,    "vmaxw",      VMAXW)
  SIMD_BUILTIN (Va_Vb_Vc,    vminaw_insn,   "vminaw",     VMINAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vminw_insn,    "vminw",      VMINW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmulaw_insn,   "vmulaw",     VMULAW)
  SIMD_BUILTIN (Va_Vb_Vc,   vmulfaw_insn,  "vmulfaw",    VMULFAW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmulfw_insn,   "vmulfw",     VMULFW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmulw_insn,    "vmulw",      VMULW)
  SIMD_BUILTIN (Va_Vb_Vc,    vsubaw_insn,   "vsubaw",     VSUBAW)
  SIMD_BUILTIN (Va_Vb_Vc,     vsubw_insn,    "vsubw",      VSUBW)
  SIMD_BUILTIN (Va_Vb_Vc,    vsummw_insn,   "vsummw",     VSUMMW)
  SIMD_BUILTIN (Va_Vb_Vc,      vand_insn,     "vand",       VAND)
  SIMD_BUILTIN (Va_Vb_Vc,    vandaw_insn,   "vandaw",     VANDAW)
  SIMD_BUILTIN (Va_Vb_Vc,      vbic_insn,     "vbic",       VBIC)
  SIMD_BUILTIN (Va_Vb_Vc,    vbicaw_insn,   "vbicaw",     VBICAW)
  SIMD_BUILTIN (Va_Vb_Vc,       vor_insn,      "vor",        VOR)
  SIMD_BUILTIN (Va_Vb_Vc,      vxor_insn,     "vxor",       VXOR)
  SIMD_BUILTIN (Va_Vb_Vc,    vxoraw_insn,   "vxoraw",     VXORAW)
  SIMD_BUILTIN (Va_Vb_Vc,      veqw_insn,     "veqw",       VEQW)
  SIMD_BUILTIN (Va_Vb_Vc,      vlew_insn,     "vlew",       VLEW)
  SIMD_BUILTIN (Va_Vb_Vc,      vltw_insn,     "vltw",       VLTW)
  SIMD_BUILTIN (Va_Vb_Vc,      vnew_insn,     "vnew",       VNEW)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr1aw_insn,   "vmr1aw",     VMR1AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr1w_insn,    "vmr1w",      VMR1W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr2aw_insn,   "vmr2aw",     VMR2AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr2w_insn,    "vmr2w",      VMR2W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr3aw_insn,   "vmr3aw",     VMR3AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr3w_insn,    "vmr3w",      VMR3W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr4aw_insn,   "vmr4aw",     VMR4AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr4w_insn,    "vmr4w",      VMR4W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr5aw_insn,   "vmr5aw",     VMR5AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr5w_insn,    "vmr5w",      VMR5W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr6aw_insn,   "vmr6aw",     VMR6AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr6w_insn,    "vmr6w",      VMR6W)
  SIMD_BUILTIN (Va_Vb_Vc,    vmr7aw_insn,   "vmr7aw",     VMR7AW)
  SIMD_BUILTIN (Va_Vb_Vc,     vmr7w_insn,    "vmr7w",      VMR7W)
  SIMD_BUILTIN (Va_Vb_Vc,      vmrb_insn,     "vmrb",       VMRB)
  SIMD_BUILTIN (Va_Vb_Vc,    vh264f_insn,   "vh264f",     VH264F)
  SIMD_BUILTIN (Va_Vb_Vc,   vh264ft_insn,  "vh264ft",    VH264FT)
  SIMD_BUILTIN (Va_Vb_Vc,   vh264fw_insn,  "vh264fw",    VH264FW)
  SIMD_BUILTIN (Va_Vb_Vc,     vvc1f_insn,    "vvc1f",      VVC1F)
  SIMD_BUILTIN (Va_Vb_Vc,    vvc1ft_insn,   "vvc1ft",     VVC1FT)

  SIMD_BUILTIN (Va_Vb_rlimm,    vbaddw_insn,   "vbaddw",     VBADDW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbmaxw_insn,   "vbmaxw",     VBMAXW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbminw_insn,   "vbminw",     VBMINW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbmulaw_insn,  "vbmulaw",    VBMULAW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbmulfw_insn,  "vbmulfw",    VBMULFW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbmulw_insn,   "vbmulw",     VBMULW)
  SIMD_BUILTIN (Va_Vb_rlimm,   vbrsubw_insn,  "vbrsubw",    VBRSUBW)
  SIMD_BUILTIN (Va_Vb_rlimm,    vbsubw_insn,   "vbsubw",     VBSUBW)

  /* Va, Vb, Ic instructions.  */
  SIMD_BUILTIN (Va_Vb_Ic,        vasrw_insn,    "vasrw",      VASRW)
  SIMD_BUILTIN (Va_Vb_Ic,         vsr8_insn,     "vsr8",       VSR8)
  SIMD_BUILTIN (Va_Vb_Ic,       vsr8aw_insn,   "vsr8aw",     VSR8AW)

  /* Va, Vb, u6 instructions.  */
  SIMD_BUILTIN (Va_Vb_u6,      vasrrwi_insn,  "vasrrwi",    VASRRWi)
  SIMD_BUILTIN (Va_Vb_u6,     vasrsrwi_insn, "vasrsrwi",   VASRSRWi)
  SIMD_BUILTIN (Va_Vb_u6,       vasrwi_insn,   "vasrwi",     VASRWi)
  SIMD_BUILTIN (Va_Vb_u6,     vasrpwbi_insn, "vasrpwbi",   VASRPWBi)
  SIMD_BUILTIN (Va_Vb_u6,    vasrrpwbi_insn,"vasrrpwbi",  VASRRPWBi)
  SIMD_BUILTIN (Va_Vb_u6,      vsr8awi_insn,  "vsr8awi",    VSR8AWi)
  SIMD_BUILTIN (Va_Vb_u6,        vsr8i_insn,    "vsr8i",      VSR8i)

  /* Va, Vb, u8 (simm) instructions.  */
  SIMD_BUILTIN (Va_Vb_u8,        vmvaw_insn,    "vmvaw",      VMVAW)
  SIMD_BUILTIN (Va_Vb_u8,         vmvw_insn,     "vmvw",       VMVW)
  SIMD_BUILTIN (Va_Vb_u8,        vmvzw_insn,    "vmvzw",      VMVZW)
  SIMD_BUILTIN (Va_Vb_u8,      vd6tapf_insn,  "vd6tapf",    VD6TAPF)

  /* Va, rlimm, u8 (simm) instructions.  */
  SIMD_BUILTIN (Va_rlimm_u8,    vmovaw_insn,   "vmovaw",     VMOVAW)
  SIMD_BUILTIN (Va_rlimm_u8,     vmovw_insn,    "vmovw",      VMOVW)
  SIMD_BUILTIN (Va_rlimm_u8,    vmovzw_insn,   "vmovzw",     VMOVZW)

  /* Va, Vb instructions.  */
  SIMD_BUILTIN (Va_Vb,          vabsaw_insn,   "vabsaw",     VABSAW)
  SIMD_BUILTIN (Va_Vb,           vabsw_insn,    "vabsw",      VABSW)
  SIMD_BUILTIN (Va_Vb,         vaddsuw_insn,  "vaddsuw",    VADDSUW)
  SIMD_BUILTIN (Va_Vb,          vsignw_insn,   "vsignw",     VSIGNW)
  SIMD_BUILTIN (Va_Vb,          vexch1_insn,   "vexch1",     VEXCH1)
  SIMD_BUILTIN (Va_Vb,          vexch2_insn,   "vexch2",     VEXCH2)
  SIMD_BUILTIN (Va_Vb,          vexch4_insn,   "vexch4",     VEXCH4)
  SIMD_BUILTIN (Va_Vb,          vupbaw_insn,   "vupbaw",     VUPBAW)
  SIMD_BUILTIN (Va_Vb,           vupbw_insn,    "vupbw",      VUPBW)
  SIMD_BUILTIN (Va_Vb,         vupsbaw_insn,  "vupsbaw",    VUPSBAW)
  SIMD_BUILTIN (Va_Vb,          vupsbw_insn,   "vupsbw",     VUPSBW)

  /* DIb, rlimm, rlimm instructions.  */
  SIMD_BUILTIN (Da_rlimm_rlimm,  vdirun_insn,  "vdirun",     VDIRUN)
  SIMD_BUILTIN (Da_rlimm_rlimm,  vdorun_insn,  "vdorun",     VDORUN)

  /* DIb, limm, rlimm instructions.  */
  SIMD_BUILTIN (Da_u3_rlimm,   vdiwr_insn,    "vdiwr",      VDIWR)
  SIMD_BUILTIN (Da_u3_rlimm,    vdowr_insn,    "vdowr",     VDOWR)

  /* rlimm instructions.  */
  SIMD_BUILTIN (void_rlimm,        vrec_insn,     "vrec",      VREC)
  SIMD_BUILTIN (void_rlimm,        vrun_insn,     "vrun",      VRUN)
  SIMD_BUILTIN (void_rlimm,     vrecrun_insn,  "vrecrun",   VRECRUN)
  SIMD_BUILTIN (void_rlimm,     vendrec_insn,  "vendrec",   VENDREC)

  /* Va, [Ib,u8] instructions.  */
  SIMD_BUILTIN (Va_Vb_Ic_u8,       vld32wh_insn,  "vld32wh",   VLD32WH)
  SIMD_BUILTIN (Va_Vb_Ic_u8,       vld32wl_insn,  "vld32wl",   VLD32WL)
  SIMD_BUILTIN (Va_Vb_Ic_u8,         vld64_insn,    "vld64",     VLD64)
  SIMD_BUILTIN (Va_Vb_Ic_u8,         vld32_insn,    "vld32",     VLD32)

  SIMD_BUILTIN (Va_Ib_u8,           vld64w_insn,   "vld64w",   VLD64W)
  SIMD_BUILTIN (Va_Ib_u8,           vld128_insn,   "vld128",   VLD128)
  SIMD_BUILTIN (void_Va_Ib_u8,      vst128_insn,   "vst128",   VST128)
  SIMD_BUILTIN (void_Va_Ib_u8,       vst64_insn,    "vst64",    VST64)

  /* Va, [Ib, u8] instructions.  */
  SIMD_BUILTIN (void_Va_u3_Ib_u8,  vst16_n_insn,  "vst16_n",   VST16_N)
  SIMD_BUILTIN (void_Va_u3_Ib_u8,  vst32_n_insn,  "vst32_n",   VST32_N)

  SIMD_BUILTIN (void_u6,  vinti_insn,  "vinti",   VINTI)
};

static void
arc_init_simd_builtins (void)
{
  int i;
  tree endlink = void_list_node;
  tree V8HI_type_node = build_vector_type_for_mode (intHI_type_node, V8HImode);

  tree v8hi_ftype_v8hi_v8hi
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, V8HI_type_node,
						 endlink)));
  tree v8hi_ftype_v8hi_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 endlink)));

  tree v8hi_ftype_v8hi_int_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    integer_type_node,
							    endlink))));

  tree void_ftype_v8hi_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, V8HI_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    integer_type_node,
							    endlink))));

  tree void_ftype_v8hi_int_int_int
    = (build_function_type
	(void_type_node,
	 tree_cons (NULL_TREE, V8HI_type_node,
		    tree_cons (NULL_TREE, integer_type_node,
			       tree_cons (NULL_TREE, integer_type_node,
					  tree_cons (NULL_TREE,
						     integer_type_node,
						     endlink))))));

  tree v8hi_ftype_int_int
    = build_function_type (V8HI_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 endlink)));

  tree void_ftype_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 endlink)));

  tree void_ftype_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node, endlink));

  tree v8hi_ftype_v8hi
    = build_function_type (V8HI_type_node, tree_cons (NULL_TREE, V8HI_type_node,
						      endlink));

  /* These asserts have been introduced to ensure that the order of builtins
     does not get messed up, else the initialization goes wrong.  */
  gcc_assert (arc_simd_builtin_desc_list [0].args_type == Va_Vb_Vc);
  for (i=0; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Vc; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_v8hi, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_u6);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_u6; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_rlimm_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_rlimm_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Da_rlimm_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Da_rlimm_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,
		  void_ftype_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Da_u3_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == Da_u3_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  void_ftype_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_rlimm);
  for (; arc_simd_builtin_desc_list [i].args_type == void_rlimm; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  void_ftype_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Vb_Ic_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_v8hi_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == Va_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == Va_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  v8hi_ftype_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_Va_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == void_Va_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list [i].name,
		  void_ftype_v8hi_int_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_Va_u3_Ib_u8);
  for (; arc_simd_builtin_desc_list [i].args_type == void_Va_u3_Ib_u8; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  void_ftype_v8hi_int_int_int,
		  arc_simd_builtin_desc_list[i].code);

  gcc_assert (arc_simd_builtin_desc_list [i].args_type == void_u6);
  for (; arc_simd_builtin_desc_list [i].args_type == void_u6; i++)
    def_mbuiltin (TARGET_SIMD_SET, arc_simd_builtin_desc_list[i].name,
		  void_ftype_int, arc_simd_builtin_desc_list[i].code);

  gcc_assert(i == ARRAY_SIZE (arc_simd_builtin_desc_list));
}

/* Helper function of arc_expand_builtin; has the same parameters,
   except that EXP is now known to be a call to a simd builtin.  */

static rtx
arc_expand_simd_builtin (tree exp,
			 rtx target,
			 rtx subtarget ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 int ignore ATTRIBUTE_UNUSED)
{
  tree              fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree              arg0;
  tree              arg1;
  tree              arg2;
  tree              arg3;
  rtx               op0;
  rtx               op1;
  rtx               op2;
  rtx               op3;
  rtx               op4;
  rtx pat;
  unsigned int         i;
  int               fcode = DECL_FUNCTION_CODE (fndecl);
  int               icode;
  enum machine_mode mode0;
  enum machine_mode mode1;
  enum machine_mode mode2;
  enum machine_mode mode3;
  enum machine_mode mode4;
  const struct builtin_description * d;

  for (i = 0, d = arc_simd_builtin_desc_list;
       i < ARRAY_SIZE (arc_simd_builtin_desc_list); i++, d++)
    if (d->code == (const enum arc_builtins) fcode)
      break;

  /* We must get an entry here.  */
  gcc_assert (i < ARRAY_SIZE (arc_simd_builtin_desc_list));

  switch (d->args_type)
    {
    case Va_Vb_rlimm:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	  op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Va_Vb_u6:
    case Va_Vb_u8:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1)
	  ||  (d->args_type == Va_Vb_u6 && !UNSIGNED_INT6 (INTVAL (op1)))
	  ||  (d->args_type == Va_Vb_u8 && !UNSIGNED_INT8 (INTVAL (op1))))
	error ("operand 2 of %s instruction should be an unsigned %d-bit value",
	       d->name,
	       (d->args_type == Va_Vb_u6)? 6: 8);

      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Va_rlimm_u8:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	   || !(UNSIGNED_INT8 (INTVAL (op1))))
	error ("operand 2 of %s instruction should be an unsigned 8-bit value",
	       d->name);

      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Va_Vb_Ic:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);
      op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	   || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 2 of %s instruction should be an unsigned 3-bit value (I0-I7)",
	       d->name);

      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Va_Vb_Vc:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, V8HImode, EXPAND_NORMAL);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Va_Vb:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);

      target = gen_reg_rtx (V8HImode);
      mode0 =  insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case Da_rlimm_rlimm:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);


      if (icode == CODE_FOR_vdirun_insn)
	target = gen_rtx_REG (SImode, 131);
      else if (icode == CODE_FOR_vdorun_insn)
	target = gen_rtx_REG (SImode, 139);
      else
	  gcc_unreachable ();

      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);


      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    case Da_u3_rlimm:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);


      if (! (GET_CODE (op0) == CONST_INT)
	  || !(UNSIGNED_INT3 (INTVAL (op0))))
	error ("operand 1 of %s instruction should be an unsigned 3-bit value (DR0-DR7)",
	       d->name);

      mode1 =  insn_data[icode].operand[1].mode;

      if (icode == CODE_FOR_vdiwr_insn)
	target = gen_rtx_REG (SImode,
			      ARC_FIRST_SIMD_DMA_CONFIG_IN_REG + INTVAL (op0));
      else if (icode == CODE_FOR_vdowr_insn)
	target = gen_rtx_REG (SImode,
			      ARC_FIRST_SIMD_DMA_CONFIG_OUT_REG + INTVAL (op0));
      else
	gcc_unreachable ();

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (target, op1);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    case void_u6:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[0].mode;

      /* op0 should be u6.  */
      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0)
	  || !(UNSIGNED_INT6 (INTVAL (op0))))
	error ("operand of %s instruction should be an unsigned 6-bit value",
	       d->name);

      pat = GEN_FCN (icode) (op0);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    case void_rlimm:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0);

      fold (arg0);

      op0 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL);
      mode0 = insn_data[icode].operand[0].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (op0);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    case Va_Vb_Ic_u8:
      {
	rtx src_vreg;
	icode = d->icode;
	arg0 = CALL_EXPR_ARG (exp, 0); /* source vreg */
	arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7 */
	arg2 = CALL_EXPR_ARG (exp, 2); /* u8 */

	src_vreg = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);
	op0 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);  /* [I]0-7 */
	op1 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL);  /* u8 */
	op2 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);	    /* VR0 */

	/* target <- src vreg */
	emit_insn (gen_move_insn (target, src_vreg));

	/* target <- vec_concat: target, mem(Ib, u8) */
	mode0 =  insn_data[icode].operand[3].mode;
	mode1 =  insn_data[icode].operand[1].mode;

	if ( (!(*insn_data[icode].operand[3].predicate) (op0, mode0))
	     || !(UNSIGNED_INT3 (INTVAL (op0))))
	  error ("operand 1 of %s instruction should be an unsigned 3-bit value (I0-I7)",
		 d->name);

	if ( (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	     || !(UNSIGNED_INT8 (INTVAL (op1))))
	  error ("operand 2 of %s instruction should be an unsigned 8-bit value",
		 d->name);

	pat = GEN_FCN (icode) (target, op1, op2, op0);
	if (! pat)
	  return 0;

	emit_insn (pat);
	return target;
      }

    case void_Va_Ib_u8:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0); /* src vreg */
      arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7 */
      arg2 = CALL_EXPR_ARG (exp, 2); /* u8 */

      op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);         /* VR0    */
      op1 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);   /* I[0-7] */
      op2 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL);   /* u8     */
      op3 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL); /* Vdest  */

      mode0 =  insn_data[icode].operand[0].mode;
      mode1 =  insn_data[icode].operand[1].mode;
      mode2 =  insn_data[icode].operand[2].mode;
      mode3 =  insn_data[icode].operand[3].mode;

      if ( (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	   || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 2 of %s instruction should be an unsigned 3-bit value (I0-I7)",
	       d->name);

      if ( (!(*insn_data[icode].operand[2].predicate) (op2, mode2))
	   || !(UNSIGNED_INT8 (INTVAL (op2))))
	error ("operand 3 of %s instruction should be an unsigned 8-bit value",
	       d->name);

      if (!(*insn_data[icode].operand[3].predicate) (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      pat = GEN_FCN (icode) (op0, op1, op2, op3);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    case Va_Ib_u8:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0); /* dest vreg */
      arg1 = CALL_EXPR_ARG (exp, 1); /* [I]0-7 */

      op0 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);       /* VR0    */
      op1 = expand_expr (arg0, NULL_RTX, SImode, EXPAND_NORMAL); /* I[0-7] */
      op2 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL); /* u8     */

      /* target <- src vreg */
      target = gen_reg_rtx (V8HImode);

      /* target <- vec_concat: target, mem(Ib, u8) */
      mode0 =  insn_data[icode].operand[1].mode;
      mode1 =  insn_data[icode].operand[2].mode;
      mode2 =  insn_data[icode].operand[3].mode;

      if ( (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	   || !(UNSIGNED_INT3 (INTVAL (op1))))
	error ("operand 1 of %s instruction should be an unsigned 3-bit value (I0-I7)",
	       d->name);

      if ( (!(*insn_data[icode].operand[3].predicate) (op2, mode2))
	   || !(UNSIGNED_INT8 (INTVAL (op2))))
	error ("operand 2 of %s instruction should be an unsigned 8-bit value",
	       d->name);

      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (! pat)
	return 0;

      emit_insn (pat);
      return target;

    case void_Va_u3_Ib_u8:
      icode = d->icode;
      arg0 = CALL_EXPR_ARG (exp, 0); /* source vreg */
      arg1 = CALL_EXPR_ARG (exp, 1); /* u3 */
      arg2 = CALL_EXPR_ARG (exp, 2); /* [I]0-7 */
      arg3 = CALL_EXPR_ARG (exp, 3); /* u8 */

      op0 = expand_expr (arg3, NULL_RTX, SImode, EXPAND_NORMAL); /* u8        */
      op1 = gen_rtx_REG (V8HImode, ARC_FIRST_SIMD_VR_REG);       /* VR        */
      op2 = expand_expr (arg2, NULL_RTX, SImode, EXPAND_NORMAL); /* [I]0-7    */
      op3 = expand_expr (arg0, NULL_RTX, V8HImode, EXPAND_NORMAL);/* vreg to be stored */
      op4 = expand_expr (arg1, NULL_RTX, SImode, EXPAND_NORMAL);  /* vreg 0-7 subreg no. */

      mode0 =  insn_data[icode].operand[0].mode;
      mode2 =  insn_data[icode].operand[2].mode;
      mode3 =  insn_data[icode].operand[3].mode;
      mode4 =  insn_data[icode].operand[4].mode;

      /* Do some correctness checks for the operands.  */
      if ( (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	   || !(UNSIGNED_INT8 (INTVAL (op0))))
	error ("operand 4 of %s instruction should be an unsigned 8-bit value (0-255)",
	       d->name);

      if ( (!(*insn_data[icode].operand[2].predicate) (op2, mode2))
	   || !(UNSIGNED_INT3 (INTVAL (op2))))
	error ("operand 3 of %s instruction should be an unsigned 3-bit value (I0-I7)",
	       d->name);

      if (!(*insn_data[icode].operand[3].predicate) (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      if ( (!(*insn_data[icode].operand[4].predicate) (op4, mode4))
	   || !(UNSIGNED_INT3 (INTVAL (op4))))
	error ("operand 2 of %s instruction should be an unsigned 3-bit value (subreg 0-7)",
	       d->name);
      else if (icode == CODE_FOR_vst32_n_insn
	       && ((INTVAL(op4) % 2 ) != 0))
	error ("operand 2 of %s instruction should be an even 3-bit value (subreg 0,2,4,6)",
	       d->name);

      pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
      if (! pat)
	return 0;

      emit_insn (pat);
      return NULL_RTX;

    default:
      gcc_unreachable ();
    }
  return NULL_RTX;
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
arc_register_move_cost (enum machine_mode,
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

  /* The ARC700 stalls for 3 cycles when *reading* from lp_count.  */
  if (TARGET_ARC700
      && (from_class == LPCOUNT_REG || from_class == ALL_CORE_REGS
	  || from_class == WRITABLE_CORE_REGS))
    return 8;

  /* Force an attempt to 'mov Dy,Dx' to spill.  */
  if (TARGET_ARC700 && TARGET_DPFP
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
  char format[32];

  int match = operands_match_p (operands[0], operands[1]);
  int match2 = operands_match_p (operands[0], operands[2]);
  int intval = (REG_P (operands[2]) ? 1
		: CONST_INT_P (operands[2]) ? INTVAL (operands[2]) : 0xbadc057);
  int neg_intval = -intval;
  int short_0 = satisfies_constraint_Rcq (operands[0]);
  int short_p = (!cond_p && short_0 && satisfies_constraint_Rcq (operands[1]));
  int ret = 0;

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
      if (short_p
	  && (REG_P (operands[2])
	      ? (match || satisfies_constraint_Rcq (operands[2]))
	      : (unsigned) intval <= (match ? 127 : 7)))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2");
      if (short_0 && REG_P (operands[1]) && match2)
	ADDSI_OUTPUT1 ("add%? %0,%2,%1");
      if ((short_0 || REGNO (operands[0]) == STACK_POINTER_REGNUM)
	  && REGNO (operands[1]) == STACK_POINTER_REGNUM && !(intval & ~124))
	ADDSI_OUTPUT1 ("add%? %0,%1,%2");

      if ((short_p && (unsigned) neg_intval <= (match ? 31 : 7))
	  || (REGNO (operands[0]) == STACK_POINTER_REGNUM
	      && match && !(neg_intval & ~124)))
	ADDSI_OUTPUT1 ("sub%? %0,%1,%n2");
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

      if (intval == -1 << 31)
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
	  && REGNO_PTR_FRAME_P (REGNO (addr)))
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

/* Like move_by_pieces, but take account of load latency,
   and actual offset ranges.
   Return true on success.  */

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
    n_pieces = (size + 2) / 4U + (size & 1);
  else if (align == 2)
    n_pieces = (size + 1) / 2U;
  else
    n_pieces = size;
  if (n_pieces >= (unsigned int) (optimize_size ? 3 : 15))
    return false;
  if (piece > 4)
    piece = 4;
  dst_addr = force_offsettable (XEXP (operands[0], 0), size, 0);
  src_addr = force_offsettable (XEXP (operands[1], 0), size, 0);
  store[0] = store[1] = NULL_RTX;
  tmpx[0] = tmpx[1] = NULL_RTX;
  for (i = 0; size > 0; i ^= 1, size -= piece)
    {
      rtx tmp;
      enum machine_mode mode;

      if (piece > size)
	piece = size & -size;
      mode = smallest_mode_for_size (piece * BITS_PER_UNIT, MODE_INT);
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
prepare_move_operands (rtx *operands, enum machine_mode mode)
{
  /* We used to do this only for MODE_INT Modes, but addresses to floating
     point variables may well be in the small data section.  */
  if (1)
    {
      if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[0], Pmode))
	operands[0] = arc_rewrite_small_data (operands[0]);
      else if (mode == SImode && flag_pic && SYMBOLIC_CONST (operands[1]))
	{
	  emit_pic_move (operands, SImode);

	  /* Disable any REG_EQUALs associated with the symref
	     otherwise the optimization pass undoes the work done
	     here and references the variable directly.  */
	}
      else if (GET_CODE (operands[0]) != MEM
	       && !TARGET_NO_SDATA_SET
	       && small_data_pattern (operands[1], Pmode))
       {
	  /* This is to take care of address calculations involving sdata
	     variables.  */
	  operands[1] = arc_rewrite_small_data (operands[1]);

	  emit_insn (gen_rtx_SET (mode, operands[0],operands[1]));
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
			 enum machine_mode omode)
{
  if (!TARGET_NO_SDATA_SET && small_data_pattern (operands[1], Pmode))
    {
      /* This is to take care of address calculations involving sdata
	 variables.  */
      operands[1]
	= gen_rtx_fmt_e (code, omode, arc_rewrite_small_data (operands[1]));
      emit_insn (gen_rtx_SET (omode, operands[0], operands[1]));
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
	sprintf (buf, "add r12,pcl,@%s-(.&-4)\n\tjl%%!%%* [r12]", fname);
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

/* Called by arc600_corereg_hazard via for_each_rtx.
   If a hazard is found, return a conservative estimate of the required
   length adjustment to accomodate a nop.  */

static int
arc600_corereg_hazard_1 (rtx *xp, void *data)
{
  rtx x = *xp;
  rtx dest;
  rtx pat = (rtx) data;

  switch (GET_CODE (x))
    {
    case SET: case POST_INC: case POST_DEC: case PRE_INC: case PRE_DEC:
      break;
    default:
    /* This is also fine for PRE/POST_MODIFY, because they contain a SET.  */
      return 0;
    }
  dest = XEXP (x, 0);
  /* Check if this sets a an extension register.  N.B. we use 61 for the
     condition codes, which is definitely not an extension register.  */
  if (REG_P (dest) && REGNO (dest) >= 32 && REGNO (dest) < 61
      /* Check if the same register is used by the PAT.  */
      && (refers_to_regno_p
	   (REGNO (dest),
	   REGNO (dest) + (GET_MODE_SIZE (GET_MODE (dest)) + 3) / 4U, pat, 0)))
    return 4;

  return 0;
}

/* Return length adjustment for INSN.
   For ARC600:
   A write to a core reg greater or equal to 32 must not be immediately
   followed by a use.  Anticipate the length requirement to insert a nop
   between PRED and SUCC to prevent a hazard.  */

static int
arc600_corereg_hazard (rtx pred, rtx succ)
{
  if (!TARGET_ARC600)
    return 0;
  /* If SUCC is a doloop_end_i with a preceding label, we must output a nop
     in front of SUCC anyway, so there will be separation between PRED and
     SUCC.  */
  if (recog_memoized (succ) == CODE_FOR_doloop_end_i
      && LABEL_P (prev_nonnote_insn (succ)))
    return 0;
  if (recog_memoized (succ) == CODE_FOR_doloop_begin_i)
    return 0;
  if (GET_CODE (PATTERN (pred)) == SEQUENCE)
    pred = XVECEXP (PATTERN (pred), 0, 1);
  if (GET_CODE (PATTERN (succ)) == SEQUENCE)
    succ = XVECEXP (PATTERN (succ), 0, 0);
  if (recog_memoized (pred) == CODE_FOR_mulsi_600
      || recog_memoized (pred) == CODE_FOR_umul_600
      || recog_memoized (pred) == CODE_FOR_mac_600
      || recog_memoized (pred) == CODE_FOR_mul64_600
      || recog_memoized (pred) == CODE_FOR_mac64_600
      || recog_memoized (pred) == CODE_FOR_umul64_600
      || recog_memoized (pred) == CODE_FOR_umac64_600)
    return 0;
  return for_each_rtx (&PATTERN (pred), arc600_corereg_hazard_1,
		       PATTERN (succ));
}

/* For ARC600:
   A write to a core reg greater or equal to 32 must not be immediately
   followed by a use.  Anticipate the length requirement to insert a nop
   between PRED and SUCC to prevent a hazard.  */

int
arc_hazard (rtx pred, rtx succ)
{
  if (!TARGET_ARC600)
    return 0;
  if (!pred || !INSN_P (pred) || !succ || !INSN_P (succ))
    return 0;
  /* We might have a CALL to a non-returning function before a loop end.
     ??? Although the manual says that's OK (the target is outside the loop,
     and the loop counter unused there), the assembler barfs on this, so we
     must instert a nop before such a call too.  */
  if (recog_memoized (succ) == CODE_FOR_doloop_end_i
      && (JUMP_P (pred) || CALL_P (pred)
	  || GET_CODE (PATTERN (pred)) == SEQUENCE))
    return 4;
  return arc600_corereg_hazard (pred, succ);
}

/* Return length adjustment for INSN.  */

int
arc_adjust_insn_length (rtx insn, int len, bool)
{
  if (!INSN_P (insn))
    return len;
  /* We already handle sequences by ignoring the delay sequence flag.  */
  if (GET_CODE (PATTERN (insn)) == SEQUENCE)
    return len;

  /* It is impossible to jump to the very end of a Zero-Overhead Loop, as
     the ZOL mechanism only triggers when advancing to the end address,
     so if there's a label at the end of a ZOL, we need to insert a nop.
     The ARC600 ZOL also has extra restrictions on jumps at the end of a
     loop.  */
  if (recog_memoized (insn) == CODE_FOR_doloop_end_i)
    {
      rtx prev = prev_nonnote_insn (insn);

      return ((LABEL_P (prev)
	       || (TARGET_ARC600
		   && (JUMP_P (prev)
		       || CALL_P (prev) /* Could be a noreturn call.  */
		       || (NONJUMP_INSN_P (prev)
			   && GET_CODE (PATTERN (prev)) == SEQUENCE))))
	      ? len + 4 : len);
    }

  /* Check for return with but one preceding insn since function
     start / call.  */
  if (TARGET_PAD_RETURN
      && JUMP_P (insn)
      && GET_CODE (PATTERN (insn)) != ADDR_VEC
      && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
      && get_attr_type (insn) == TYPE_RETURN)
    {
      rtx prev = prev_active_insn (insn);

      if (!prev || !(prev = prev_active_insn (prev))
	  || ((NONJUMP_INSN_P (prev)
	       && GET_CODE (PATTERN (prev)) == SEQUENCE)
	      ? CALL_ATTR (XVECEXP (PATTERN (prev), 0, 0), NON_SIBCALL)
	      : CALL_ATTR (prev, NON_SIBCALL)))
	return len + 4;
    }
  if (TARGET_ARC600)
    {
      rtx succ = next_real_insn (insn);

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
  int (*get_variants) (rtx, int, bool, bool, insn_length_variant_t *);
} insn_length_parameters_t;

static void
arc_insn_length_parameters (insn_length_parameters_t *ilp) ATTRIBUTE_UNUSED;
#endif

static int
arc_get_insn_variants (rtx insn, int len, bool, bool target_p,
		       insn_length_variant_t *ilv)
{
  if (!NONDEBUG_INSN_P (insn))
    return 0;
  enum attr_type type;
  /* shorten_branches doesn't take optimize_size into account yet for the
     get_variants mechanism, so turn this off for now.  */
  if (optimize_size)
    return 0;
  if (GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      /* The interaction of a short delay slot insn with a short branch is
	 too weird for shorten_branches to piece together, so describe the
	 entire SEQUENCE.  */
      rtx pat, inner;
      if (TARGET_UPSIZE_DBR
	  && get_attr_length (XVECEXP ((pat = PATTERN (insn)), 0, 1)) <= 2
	  && (((type = get_attr_type (inner = XVECEXP (pat, 0, 0)))
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
  rtx prev = prev_active_insn (insn);
  if (prev && arc_next_active_insn (prev, 0) == insn
      && ((NONJUMP_INSN_P (prev) && GET_CODE (PATTERN (prev)) == SEQUENCE)
	  ? CALL_ATTR (XVECEXP (PATTERN (prev), 0, 0), NON_SIBCALL)
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

  enum machine_mode ccm = GET_MODE (XEXP (cond, 0));
  enum rtx_code code = reverse_condition (GET_CODE (cond));
  if (code == UNKNOWN || ccm == CC_FP_GTmode || ccm == CC_FP_GEmode)
    code = reverse_condition_maybe_unordered (GET_CODE (cond));

  return gen_rtx_fmt_ee (code, GET_MODE (cond),
			 copy_rtx (XEXP (cond, 0)), copy_rtx (XEXP (cond, 1)));
}

/* Use the ccfsm machinery to do if conversion.  */

static unsigned
arc_ifcvt (void)
{
  struct arc_ccfsm *statep = &cfun->machine->ccfsm_current;
  basic_block merge_bb = 0;

  memset (statep, 0, sizeof *statep);
  for (rtx insn = get_insns (); insn; insn = next_insn (insn))
    {
      arc_ccfsm_advance (insn, statep);

      switch (statep->state)
	{
	case 0:
	  if (JUMP_P (insn))
	    merge_bb = 0;
	  break;
	case 1: case 2:
	  {
	    /* Deleted branch.  */
	    gcc_assert (!merge_bb);
	    merge_bb = BLOCK_FOR_INSN (insn);
	    basic_block succ_bb
	      = BLOCK_FOR_INSN (NEXT_INSN (NEXT_INSN (PREV_INSN (insn))));
	    arc_ccfsm_post_advance (insn, statep);
	    rtx seq = NEXT_INSN (PREV_INSN (insn));
	    if (seq != insn)
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
		if (merge_bb && succ_bb)
		  merge_blocks (merge_bb, succ_bb);
	      }
	    else if (merge_bb && succ_bb)
	      {
		set_insn_deleted (insn);
		merge_blocks (merge_bb, succ_bb);
	      }
	    else
	      {
		PUT_CODE (insn, NOTE);
		NOTE_KIND (insn) = NOTE_INSN_DELETED;
	      }
	    continue;
	  }
	case 3:
	  if (LABEL_P (insn)
	      && statep->target_label == CODE_LABEL_NUMBER (insn))
	    {
	      arc_ccfsm_post_advance (insn, statep);
	      basic_block succ_bb = BLOCK_FOR_INSN (insn);
	      if (merge_bb && succ_bb)
		merge_blocks (merge_bb, succ_bb);
	      else if (--LABEL_NUSES (insn) == 0)
		{
		  const char *name = LABEL_NAME (insn);
		  PUT_CODE (insn, NOTE);
		  NOTE_KIND (insn) = NOTE_INSN_DELETED_LABEL;
		  NOTE_DELETED_LABEL_NAME (insn) = name;
		}
	      merge_bb = 0;
	      continue;
	    }
	  /* Fall through.  */
	case 4: case 5:
	  if (!NONDEBUG_INSN_P (insn))
	    break;

	  /* Conditionalized insn.  */

	  rtx prev, pprev, *patp, pat, cond;

	  /* If this is a delay slot insn in a non-annulled branch,
	     don't conditionalize it.  N.B., this should be fine for
	     conditional return too.  However, don't do this for
	     unconditional branches, as these would be encountered when
	     processing an 'else' part.  */
	  prev = PREV_INSN (insn);
	  pprev = PREV_INSN (prev);
	  if (pprev && NEXT_INSN (NEXT_INSN (pprev)) == NEXT_INSN (insn)
	      && JUMP_P (prev) && get_attr_cond (prev) == COND_USE
	      && !INSN_ANNULLED_BRANCH_P (prev))
	    break;

	  patp = &PATTERN (insn);
	  pat = *patp;
	  cond = arc_get_ccfsm_cond (statep, INSN_FROM_TARGET_P (insn));
	  if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	    {
	      /* ??? don't conditionalize if all side effects are dead
		 in the not-execute case.  */
	      /* dwarf2out.c:dwarf2out_frame_debug_expr doesn't know
		 what to do with COND_EXEC.  */
	      if (RTX_FRAME_RELATED_P (insn))
		{
		  /* If this is the delay slot insn of an anulled branch,
		     dwarf2out.c:scan_trace understands the anulling semantics
		     without the COND_EXEC.  */
		  gcc_assert
		   (pprev && NEXT_INSN (NEXT_INSN (pprev)) == NEXT_INSN (insn)
		    && JUMP_P (prev) && get_attr_cond (prev) == COND_USE
		    && INSN_ANNULLED_BRANCH_P (prev));
		  rtx note = alloc_reg_note (REG_FRAME_RELATED_EXPR, pat,
					     REG_NOTES (insn));
		  validate_change (insn, &REG_NOTES (insn), note, 1);
		}
	      pat = gen_rtx_COND_EXEC (VOIDmode, cond, pat);
	    }
	  else if (simplejump_p (insn))
	    {
	      patp = &SET_SRC (pat);
	      pat = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, *patp, pc_rtx);
	    }
	  else if (JUMP_P (insn) && ANY_RETURN_P (PATTERN (insn)))
	    {
	      pat = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, pat, pc_rtx);
	      pat = gen_rtx_SET (VOIDmode, pc_rtx, pat);
	    }
	  else
	    gcc_unreachable ();
	  validate_change (insn, patp, pat, 1);
	  if (!apply_change_group ())
	    gcc_unreachable ();
	  if (JUMP_P (insn))
	    {
	      rtx next = next_nonnote_insn (insn);
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

/* For ARC600: If a write to a core reg >=32 appears in a delay slot
  (other than of a forward brcc), it creates a hazard when there is a read
  of the same register at the branch target.  We can't know what is at the
  branch target of calls, and for branches, we don't really know before the
  end of delay slot scheduling, either.  Not only can individual instruction
  be hoisted out into a delay slot, a basic block can also be emptied this
  way, and branch and/or fall through targets be redirected.  Hence we don't
  want such writes in a delay slot.  */
/* Called by arc_write_ext_corereg via for_each_rtx.  */

static int
write_ext_corereg_1 (rtx *xp, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *xp;
  rtx dest;

  switch (GET_CODE (x))
    {
    case SET: case POST_INC: case POST_DEC: case PRE_INC: case PRE_DEC:
      break;
    default:
    /* This is also fine for PRE/POST_MODIFY, because they contain a SET.  */
      return 0;
    }
  dest = XEXP (x, 0);
  if (REG_P (dest) && REGNO (dest) >= 32 && REGNO (dest) < 61)
    return 1;
  return 0;
}

/* Return nonzreo iff INSN writes to an extension core register.  */

int
arc_write_ext_corereg (rtx insn)
{
  return for_each_rtx (&PATTERN (insn), write_ext_corereg_1, 0);
}

/* This is like the hook, but returns NULL when it can't / won't generate
   a legitimate address.  */

static rtx
arc_legitimize_address_0 (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  enum machine_mode mode)
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
  if (memory_address_p ((enum machine_mode) mode, x))
     return x;
  return NULL_RTX;
}

static rtx
arc_legitimize_address (rtx orig_x, rtx oldx, enum machine_mode mode)
{
  rtx new_x = arc_legitimize_address_0 (orig_x, oldx, mode);

  if (new_x)
    return new_x;
  return orig_x;
}

static rtx
arc_delegitimize_address_0 (rtx x)
{
  rtx u, gp;

  if (GET_CODE (x) == CONST && GET_CODE (u = XEXP (x, 0)) == UNSPEC)
    {
      if (XINT (u, 1) == ARC_UNSPEC_GOT)
	return XVECEXP (u, 0, 0);
    }
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
      && !INSN_DELETED_P (NEXT_INSN (branch)))
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
  rtx insn = current_output_insn;
  rtx prev = prev_active_insn (insn);
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
	  ? CALL_ATTR (XVECEXP (PATTERN (prev), 0, 0), NON_SIBCALL)
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
	  prev = XVECEXP (final_sequence, 0, 1);
	  gcc_assert (!prev_real_insn (insn)
		      || !arc_hazard (prev_real_insn (insn), prev));
	  cfun->machine->force_short_suffix = !want_long;
	  rtx save_pred = current_insn_predicate;
	  final_scan_insn (prev, asm_out_file, optimize, 1, NULL);
	  cfun->machine->force_short_suffix = -1;
	  INSN_DELETED_P (prev) = 1;
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
  machine = ggc_alloc_cleared_machine_function ();
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

static rtx
arc_process_double_reg_moves (rtx *operands)
{
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

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
    return NULL_RTX;

  start_sequence ();

  if (state == srcDx)
    {
      /* Without the LR insn, we need to split this into a
	 sequence of insns which will use the DEXCLx and DADDHxy
	 insns to be able to read the Dx register in question.  */
      if (TARGET_DPFP_DISABLE_LRSR)
	{
	  /* gen *movdf_insn_nolrsr */
	  rtx set = gen_rtx_SET (VOIDmode, dest, src);
	  rtx use1 = gen_rtx_USE (VOIDmode, const1_rtx);
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, use1)));
	}
      else
	{
	  /* When we have 'mov D, r' or 'mov D, D' then get the target
	     register pair for use with LR insn.  */
	  rtx destHigh = simplify_gen_subreg(SImode, dest, DFmode, 4);
	  rtx destLow  = simplify_gen_subreg(SImode, dest, DFmode, 0);

	  /* Produce the two LR insns to get the high and low parts.  */
	  emit_insn (gen_rtx_SET (VOIDmode,
				  destHigh,
				  gen_rtx_UNSPEC_VOLATILE (Pmode, gen_rtvec (1, src),
				  VUNSPEC_LR_HIGH)));
	  emit_insn (gen_rtx_SET (VOIDmode,
				  destLow,
				  gen_rtx_UNSPEC_VOLATILE (Pmode, gen_rtvec (1, src),
				  VUNSPEC_LR)));
	}
    }
  else if (state == destDx)
    {
      /* When we have 'mov r, D' or 'mov D, D' and we have access to the
	 LR insn get the target register pair.  */
      rtx srcHigh = simplify_gen_subreg(SImode, src, DFmode, 4);
      rtx srcLow  = simplify_gen_subreg(SImode, src, DFmode, 0);

      emit_insn (gen_rtx_UNSPEC_VOLATILE (Pmode,
					  gen_rtvec (3, dest, srcHigh, srcLow),
					  VUNSPEC_DEXCL_NORES));

    }
  else
    gcc_unreachable ();

  val = get_insns ();
  end_sequence ();
  return val;
}

/* operands 0..1 are the operands of a 64 bit move instruction.
   split it into two moves with operands 2/3 and 4/5.  */

rtx
arc_split_move (rtx *operands)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  int i;
  int swap = 0;
  rtx xop[4];
  rtx val;

  if (TARGET_DPFP)
  {
    val = arc_process_double_reg_moves (operands);
    if (val)
      return val;
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
  operands[2+swap] = xop[0];
  operands[3+swap] = xop[1];
  operands[4-swap] = xop[2];
  operands[5-swap] = xop[3];

  start_sequence ();
  emit_insn (gen_rtx_SET (VOIDmode, operands[2], operands[3]));
  emit_insn (gen_rtx_SET (VOIDmode, operands[4], operands[5]));
  val = get_insns ();
  end_sequence ();

  return val;
}

/* Select between the instruction output templates s_tmpl (for short INSNs)
   and l_tmpl (for long INSNs).  */

const char *
arc_short_long (rtx insn, const char *s_tmpl, const char *l_tmpl)
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

  if (REG_P (x) && refers_to_regno_p (regno, regno+1, x, (rtx *) 0))
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
arc_attr_type (rtx insn)
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
arc_sets_cc_p (rtx insn)
{
  if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, XVECLEN (PATTERN (insn), 0) - 1);
  return arc_attr_type (insn) == TYPE_COMPARE;
}

/* Return true if INSN is an instruction with a delay slot we may want
   to fill.  */

bool
arc_need_delay (rtx insn)
{
  rtx next;

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

/* Oddly enough, sometimes we get a zero overhead loop that branch
   shortening doesn't think is a loop - observed with compile/pr24883.c
   -O3 -fomit-frame-pointer -funroll-loops.  Make sure to include the
   alignment visible for branch shortening  (we actually align the loop
   insn before it, but that is equivalent since the loop insn is 4 byte
   long.)  */

int
arc_label_align (rtx label)
{
  int loop_align = LOOP_ALIGN (LABEL);

  if (loop_align > align_labels_log)
    {
      rtx prev = prev_nonnote_insn (label);

      if (prev && NONJUMP_INSN_P (prev)
	  && GET_CODE (PATTERN (prev)) == PARALLEL
	  && recog_memoized (prev) == CODE_FOR_doloop_begin_i)
	return loop_align;
    }
  /* Code has a minimum p2 alignment of 1, which we must restore after an
     ADDR_DIFF_VEC.  */
  if (align_labels_log < 1)
    {
      rtx next = next_nonnote_nondebug_insn (label);
      if (INSN_P (next) && recog_memoized (next) >= 0)
	return 1;
    }
  return align_labels_log;
}

/* Return true if LABEL is in executable code.  */

bool
arc_text_label (rtx label)
{
  rtx next;

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

/* Return the size of the pretend args for DECL.  */

int
arc_decl_pretend_args (tree decl)
{
  /* struct function is in DECL_STRUCT_FUNCTION (decl), but no
     pretend_args there...  See PR38391.  */
  gcc_assert (decl == current_function_decl);
  return crtl->args.pretend_args_size;
}

/* Without this, gcc.dg/tree-prof/bb-reorg.c fails to assemble
  when compiling with -O2 -freorder-blocks-and-partition -fprofile-use
  -D_PROFILE_USE; delay branch scheduling then follows a REG_CROSSING_JUMP
  to redirect two breqs.  */

static bool
arc_can_follow_jump (const_rtx follower, const_rtx followee)
{
  /* ??? get_attr_type is declared to take an rtx.  */
  union { const_rtx c; rtx r; } u;

  u.c = follower;
  if (find_reg_note (followee, REG_CROSSING_JUMP, NULL_RTX))
    switch (get_attr_type (u.r))
      {
      case TYPE_BRCC:
      case TYPE_BRCC_NO_DELAY_SLOT:
	return false;
      default:
	return true;
      }
  return true;
}

/* Implement EPILOGUE__USES.
   Return true if REGNO should be added to the deemed uses of the epilogue.

   We use the return address
   arc_return_address_regs[arc_compute_function_type (cfun)] .
   But also, we have to make sure all the register restore instructions
   are known to be live in interrupt functions.  */

bool
arc_epilogue_uses (int regno)
{
  if (reload_completed)
    {
      if (ARC_INTERRUPT_P (cfun->machine->fn_type))
	{
	  if (!fixed_regs[regno])
	    return true;
	  return regno == arc_return_address_regs[cfun->machine->fn_type];
	}
      else
	return regno == RETURN_ADDR_REGNUM;
    }
  else
    return regno == arc_return_address_regs[arc_compute_function_type (cfun)];
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
arc_spill_class (reg_class_t /* orig_class */, enum machine_mode)
{
  return GENERAL_REGS;
}

bool
arc_legitimize_reload_address (rtx *p, enum machine_mode mode, int opnum,
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
      offset_base = (offset + (256 << shift)) & (-512 << shift);
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

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-arc.h"
