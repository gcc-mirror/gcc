/* Definitions of target machine for GNU compiler.
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by James E. Wilson <wilson@cygnus.com> and
   		  David Mosberger <davidm@hpl.hp.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "basic-block.h"
#include "toplev.h"
#include "sched-int.h"
#include "timevar.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"

/* This is used for communication between ASM_OUTPUT_LABEL and
   ASM_OUTPUT_LABELREF.  */
int ia64_asm_output_label = 0;

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */
struct rtx_def * ia64_compare_op0;
struct rtx_def * ia64_compare_op1;

/* Register names for ia64_expand_prologue.  */
static const char * const ia64_reg_numbers[96] =
{ "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",
  "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",
  "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",
  "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63",
  "r64", "r65", "r66", "r67", "r68", "r69", "r70", "r71",
  "r72", "r73", "r74", "r75", "r76", "r77", "r78", "r79",
  "r80", "r81", "r82", "r83", "r84", "r85", "r86", "r87",
  "r88", "r89", "r90", "r91", "r92", "r93", "r94", "r95",
  "r96", "r97", "r98", "r99", "r100","r101","r102","r103",
  "r104","r105","r106","r107","r108","r109","r110","r111",
  "r112","r113","r114","r115","r116","r117","r118","r119",
  "r120","r121","r122","r123","r124","r125","r126","r127"};

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_input_reg_names[8] =
{ "in0",  "in1",  "in2",  "in3",  "in4",  "in5",  "in6",  "in7" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_local_reg_names[80] =
{ "loc0", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7",
  "loc8", "loc9", "loc10","loc11","loc12","loc13","loc14","loc15",
  "loc16","loc17","loc18","loc19","loc20","loc21","loc22","loc23",
  "loc24","loc25","loc26","loc27","loc28","loc29","loc30","loc31",
  "loc32","loc33","loc34","loc35","loc36","loc37","loc38","loc39",
  "loc40","loc41","loc42","loc43","loc44","loc45","loc46","loc47",
  "loc48","loc49","loc50","loc51","loc52","loc53","loc54","loc55",
  "loc56","loc57","loc58","loc59","loc60","loc61","loc62","loc63",
  "loc64","loc65","loc66","loc67","loc68","loc69","loc70","loc71",
  "loc72","loc73","loc74","loc75","loc76","loc77","loc78","loc79" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
static const char * const ia64_output_reg_names[8] =
{ "out0", "out1", "out2", "out3", "out4", "out5", "out6", "out7" };

/* String used with the -mfixed-range= option.  */
const char *ia64_fixed_range_string;

/* Determines whether we use adds, addl, or movl to generate our
   TLS immediate offsets.  */
int ia64_tls_size = 22;

/* String used with the -mtls-size= option.  */
const char *ia64_tls_size_string;

/* Determines whether we run our final scheduling pass or not.  We always
   avoid the normal second scheduling pass.  */
static int ia64_flag_schedule_insns2;

/* Variables which are this size or smaller are put in the sdata/sbss
   sections.  */

unsigned int ia64_section_threshold;

/* Structure to be filled in by ia64_compute_frame_size with register
   save masks and offsets for the current function.  */

struct ia64_frame_info
{
  HOST_WIDE_INT total_size;	/* size of the stack frame, not including
				   the caller's scratch area.  */
  HOST_WIDE_INT spill_cfa_off;	/* top of the reg spill area from the cfa.  */
  HOST_WIDE_INT spill_size;	/* size of the gr/br/fr spill area.  */
  HOST_WIDE_INT extra_spill_size;  /* size of spill area for others.  */
  HARD_REG_SET mask;		/* mask of saved registers.  */
  unsigned int gr_used_mask;	/* mask of registers in use as gr spill 
				   registers or long-term scratches.  */
  int n_spilled;		/* number of spilled registers.  */
  int reg_fp;			/* register for fp.  */
  int reg_save_b0;		/* save register for b0.  */
  int reg_save_pr;		/* save register for prs.  */
  int reg_save_ar_pfs;		/* save register for ar.pfs.  */
  int reg_save_ar_unat;		/* save register for ar.unat.  */
  int reg_save_ar_lc;		/* save register for ar.lc.  */
  int reg_save_gp;		/* save register for gp.  */
  int n_input_regs;		/* number of input registers used.  */
  int n_local_regs;		/* number of local registers used.  */
  int n_output_regs;		/* number of output registers used.  */
  int n_rotate_regs;		/* number of rotating registers used.  */

  char need_regstk;		/* true if a .regstk directive needed.  */
  char initialized;		/* true if the data is finalized.  */
};

/* Current frame information calculated by ia64_compute_frame_size.  */
static struct ia64_frame_info current_frame_info;

static rtx gen_tls_get_addr PARAMS ((void));
static rtx gen_thread_pointer PARAMS ((void));
static int find_gr_spill PARAMS ((int));
static int next_scratch_gr_reg PARAMS ((void));
static void mark_reg_gr_used_mask PARAMS ((rtx, void *));
static void ia64_compute_frame_size PARAMS ((HOST_WIDE_INT));
static void setup_spill_pointers PARAMS ((int, rtx, HOST_WIDE_INT));
static void finish_spill_pointers PARAMS ((void));
static rtx spill_restore_mem PARAMS ((rtx, HOST_WIDE_INT));
static void do_spill PARAMS ((rtx (*)(rtx, rtx, rtx), rtx, HOST_WIDE_INT, rtx));
static void do_restore PARAMS ((rtx (*)(rtx, rtx, rtx), rtx, HOST_WIDE_INT));
static rtx gen_movdi_x PARAMS ((rtx, rtx, rtx));
static rtx gen_fr_spill_x PARAMS ((rtx, rtx, rtx));
static rtx gen_fr_restore_x PARAMS ((rtx, rtx, rtx));

static enum machine_mode hfa_element_mode PARAMS ((tree, int));
static void fix_range PARAMS ((const char *));
static struct machine_function * ia64_init_machine_status PARAMS ((void));
static void emit_insn_group_barriers PARAMS ((FILE *, rtx));
static void emit_all_insn_group_barriers PARAMS ((FILE *, rtx));
static void emit_predicate_relation_info PARAMS ((void));
static bool ia64_in_small_data_p PARAMS ((tree));
static void ia64_encode_section_info PARAMS ((tree, int));
static const char *ia64_strip_name_encoding PARAMS ((const char *));
static void process_epilogue PARAMS ((void));
static int process_set PARAMS ((FILE *, rtx));

static rtx ia64_expand_fetch_and_op PARAMS ((optab, enum machine_mode,
					     tree, rtx));
static rtx ia64_expand_op_and_fetch PARAMS ((optab, enum machine_mode,
					     tree, rtx));
static rtx ia64_expand_compare_and_swap PARAMS ((enum machine_mode,
						 enum machine_mode,
						 int, tree, rtx));
static rtx ia64_expand_lock_test_and_set PARAMS ((enum machine_mode,
						  tree, rtx));
static rtx ia64_expand_lock_release PARAMS ((enum machine_mode, tree, rtx));
static bool ia64_assemble_integer PARAMS ((rtx, unsigned int, int));
static void ia64_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void ia64_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static void ia64_output_function_end_prologue PARAMS ((FILE *));

static int ia64_issue_rate PARAMS ((void));
static int ia64_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static void ia64_sched_init PARAMS ((FILE *, int, int));
static void ia64_sched_finish PARAMS ((FILE *, int));
static int ia64_internal_sched_reorder PARAMS ((FILE *, int, rtx *,
						int *, int, int));
static int ia64_sched_reorder PARAMS ((FILE *, int, rtx *, int *, int));
static int ia64_sched_reorder2 PARAMS ((FILE *, int, rtx *, int *, int));
static int ia64_variable_issue PARAMS ((FILE *, int, rtx, int));

static void ia64_output_mi_thunk PARAMS ((FILE *, tree, HOST_WIDE_INT,
					  HOST_WIDE_INT, tree));

static void ia64_select_rtx_section PARAMS ((enum machine_mode, rtx,
					     unsigned HOST_WIDE_INT));
static void ia64_rwreloc_select_section PARAMS ((tree, int,
					         unsigned HOST_WIDE_INT))
     ATTRIBUTE_UNUSED;
static void ia64_rwreloc_unique_section PARAMS ((tree, int))
     ATTRIBUTE_UNUSED;
static void ia64_rwreloc_select_rtx_section PARAMS ((enum machine_mode, rtx,
					             unsigned HOST_WIDE_INT))
     ATTRIBUTE_UNUSED;
static unsigned int ia64_rwreloc_section_type_flags
     PARAMS ((tree, const char *, int))
     ATTRIBUTE_UNUSED;

static void ia64_hpux_add_extern_decl PARAMS ((const char *name))
     ATTRIBUTE_UNUSED;

/* Table of valid machine attributes.  */
static const struct attribute_spec ia64_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "syscall_linkage", 0, 0, false, true,  true,  NULL },
  { NULL,              0, 0, false, false, false, NULL }
};

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ia64_attribute_table

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS ia64_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN ia64_expand_builtin

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tdata1\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tdata2\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tdata4\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\tdata8\t"
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\tdata2.ua\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\tdata4.ua\t"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\tdata8.ua\t"
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER ia64_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE ia64_output_function_prologue
#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE ia64_output_function_end_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE ia64_output_function_epilogue

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P  ia64_in_small_data_p
#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO ia64_encode_section_info
#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING ia64_strip_name_encoding

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST ia64_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE ia64_issue_rate
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE ia64_variable_issue
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT ia64_sched_init
#undef TARGET_SCHED_FINISH
#define TARGET_SCHED_FINISH ia64_sched_finish
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER ia64_sched_reorder
#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 ia64_sched_reorder2

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK ia64_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_tree_hwi_hwi_tree_true

struct gcc_target targetm = TARGET_INITIALIZER;

/* Return 1 if OP is a valid operand for the MEM of a CALL insn.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op) && mode != VOIDmode)
    return 0;

  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG
	  || (GET_CODE (op) == SUBREG && GET_CODE (XEXP (op, 0)) == REG));
}

/* Return 1 if OP refers to a symbol in the sdata section.  */

int
sdata_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
      if (GET_CODE (XEXP (op, 0)) != PLUS
	  || GET_CODE (XEXP (XEXP (op, 0), 0)) != SYMBOL_REF)
	break;
      op = XEXP (XEXP (op, 0), 0);
      /* FALLTHRU */

    case SYMBOL_REF:
      if (CONSTANT_POOL_ADDRESS_P (op))
	return GET_MODE_SIZE (get_pool_mode (op)) <= ia64_section_threshold;
      else
	{
	  const char *str = XSTR (op, 0);
          return (str[0] == ENCODE_SECTION_INFO_CHAR && str[1] == 's');
	}

    default:
      break;
    }

  return 0;
}

/* Return 1 if OP refers to a symbol, and is appropriate for a GOT load.  */

int
got_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS)
	return 0;
      if (GET_CODE (XEXP (op, 0)) != SYMBOL_REF)
	return 0;
      op = XEXP (op, 1);
      if (GET_CODE (op) != CONST_INT)
	return 0;

	return 1;

      /* Ok if we're not using GOT entries at all.  */
      if (TARGET_NO_PIC || TARGET_AUTO_PIC)
	return 1;

      /* "Ok" while emitting rtl, since otherwise we won't be provided
	 with the entire offset during emission, which makes it very
	 hard to split the offset into high and low parts.  */
      if (rtx_equal_function_value_matters)
	return 1;

      /* Force the low 14 bits of the constant to zero so that we do not
	 use up so many GOT entries.  */
      return (INTVAL (op) & 0x3fff) == 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      break;
    }
  return 0;
}

/* Return 1 if OP refers to a symbol.  */

int
symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      break;
    }
  return 0;
}

/* Return tls_model if OP refers to a TLS symbol.  */

int
tls_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  const char *str;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;
  str = XSTR (op, 0);
  if (str[0] != ENCODE_SECTION_INFO_CHAR)
    return 0;
  switch (str[1])
    {
    case 'G':
      return TLS_MODEL_GLOBAL_DYNAMIC;
    case 'L':
      return TLS_MODEL_LOCAL_DYNAMIC;
    case 'i':
      return TLS_MODEL_INITIAL_EXEC;
    case 'l':
      return TLS_MODEL_LOCAL_EXEC;
    }
  return 0;
}


/* Return 1 if OP refers to a function.  */

int
function_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_FLAG (op))
    return 1;
  else
    return 0;
}

/* Return 1 if OP is setjmp or a similar function.  */

/* ??? This is an unsatisfying solution.  Should rethink.  */

int
setjmp_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  const char *name;
  int retval = 0;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  name = XSTR (op, 0);

  /* The following code is borrowed from special_function_p in calls.c.  */

  /* Disregard prefix _, __ or __x.  */
  if (name[0] == '_')
    {
      if (name[1] == '_' && name[2] == 'x')
	name += 3;
      else if (name[1] == '_')
	name += 2;
      else
	name += 1;
    }

  if (name[0] == 's')
    {
      retval
	= ((name[1] == 'e'
	    && (! strcmp (name, "setjmp")
		|| ! strcmp (name, "setjmp_syscall")))
	   || (name[1] == 'i'
	       && ! strcmp (name, "sigsetjmp"))
	   || (name[1] == 'a'
	       && ! strcmp (name, "savectx")));
    }
  else if ((name[0] == 'q' && name[1] == 's'
	    && ! strcmp (name, "qsetjmp"))
	   || (name[0] == 'v' && name[1] == 'f'
	       && ! strcmp (name, "vfork")))
    retval = 1;

  return retval;
}

/* Return 1 if OP is a general operand, but when pic exclude symbolic
   operands.  */

/* ??? If we drop no-pic support, can delete SYMBOL_REF, CONST, and LABEL_REF
   from PREDICATE_CODES.  */

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! TARGET_NO_PIC && symbolic_operand (op, mode))
    return 0;

  return general_operand (op, mode);
}

/* Return 1 if OP is a register operand that is (or could be) a GR reg.  */

int
gr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a register operand that is (or could be) an FR reg.  */

int
fr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a register operand that is (or could be) a GR/FR reg.  */

int
grfr_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno) || FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a nonimmediate operand that is (or could be) a GR reg.  */

int
gr_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a nonimmediate operand that is (or could be) a FR reg.  */

int
fr_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a nonimmediate operand that is a GR/FR reg.  */

int
grfr_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) == REG)
    {
      unsigned int regno = REGNO (op);
      if (regno < FIRST_PSEUDO_REGISTER)
	return GENERAL_REGNO_P (regno) || FR_REGNO_P (regno);
    }
  return 1;
}

/* Return 1 if OP is a GR register operand, or zero.  */

int
gr_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or a 5 bit immediate operand.  */

int
gr_reg_or_5bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && INTVAL (op) >= 0 && INTVAL (op) < 32)
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or a 6 bit immediate operand.  */

int
gr_reg_or_6bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR register operand, or an 8 bit immediate operand.  */

int
gr_reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a GR/FR register operand, or an 8 bit immediate.  */

int
grfr_reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || grfr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or an 8 bit adjusted immediate
   operand.  */

int
gr_reg_or_8bit_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or is valid for both an 8 bit
   immediate and an 8 bit adjusted immediate operand.  This is necessary
   because when we emit a compare, we don't know what the condition will be,
   so we need the union of the immediates accepted by GT and LT.  */

int
gr_reg_or_8bit_and_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op))
	   && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 14 bit immediate operand.  */

int
gr_reg_or_14bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_I (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 22 bit immediate operand.  */

int
gr_reg_or_22bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_J (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || gr_register_operand (op, mode));
}

/* Return 1 if OP is a 6 bit immediate operand.  */

int
shift_count_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 5 bit immediate operand.  */

int
shift_32bit_count_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && (INTVAL (op) >= 0 && INTVAL (op) < 32))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 2, 4, 8, or 16 immediate operand.  */

int
shladd_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 2 || INTVAL (op) == 4
	      || INTVAL (op) == 8 || INTVAL (op) == 16));
}

/* Return 1 if OP is a -16, -8, -4, -1, 1, 4, 8, or 16 immediate operand.  */

int
fetchadd_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
          && (INTVAL (op) == -16 || INTVAL (op) == -8 ||
              INTVAL (op) == -4  || INTVAL (op) == -1 ||
              INTVAL (op) == 1   || INTVAL (op) == 4  ||
              INTVAL (op) == 8   || INTVAL (op) == 16));
}

/* Return 1 if OP is a floating-point constant zero, one, or a register.  */

int
fr_reg_or_fp01_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (op))
	  || fr_register_operand (op, mode));
}

/* Like nonimmediate_operand, but don't allow MEMs that try to use a
   POST_MODIFY with a REG as displacement.  */

int
destination_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! nonimmediate_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM
      && GET_CODE (XEXP (op, 0)) == POST_MODIFY
      && GET_CODE (XEXP (XEXP (XEXP (op, 0), 1), 1)) == REG)
    return 0;
  return 1;
}

/* Like memory_operand, but don't allow post-increments.  */

int
not_postinc_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (memory_operand (op, mode)
	  && GET_RTX_CLASS (GET_CODE (XEXP (op, 0))) != 'a');
}

/* Return 1 if this is a comparison operator, which accepts an normal 8-bit
   signed immediate operand.  */

int
normal_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == EQ || code == NE
	      || code == GT || code == LE || code == GTU || code == LEU));
}

/* Return 1 if this is a comparison operator, which accepts an adjusted 8-bit
   signed immediate operand.  */

int
adjusted_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == LT || code == GE || code == LTU || code == GEU));
}

/* Return 1 if this is a signed inequality operator.  */

int
signed_inequality_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == GE || code == GT
	      || code == LE || code == LT));
}

/* Return 1 if this operator is valid for predication.  */

int
predicate_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && (code == EQ || code == NE));
}

/* Return 1 if this operator can be used in a conditional operation.  */

int
condop_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && (code == PLUS || code == MINUS || code == AND
	      || code == IOR || code == XOR));
}

/* Return 1 if this is the ar.lc register.  */

int
ar_lc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == DImode
	  && (mode == DImode || mode == VOIDmode)
	  && GET_CODE (op) == REG
	  && REGNO (op) == AR_LC_REGNUM);
}

/* Return 1 if this is the ar.ccv register.  */

int
ar_ccv_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && GET_CODE (op) == REG
	  && REGNO (op) == AR_CCV_REGNUM);
}

/* Return 1 if this is the ar.pfs register.  */

int
ar_pfs_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && GET_CODE (op) == REG
	  && REGNO (op) == AR_PFS_REGNUM);
}

/* Like general_operand, but don't allow (mem (addressof)).  */

int
general_tfmode_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! general_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == ADDRESSOF)
    return 0;
  return 1;
}

/* Similarly.  */

int
destination_tfmode_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! destination_operand (op, mode))
    return 0;
  if (GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == ADDRESSOF)
    return 0;
  return 1;
}

/* Similarly.  */

int
tfreg_or_fp01_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    return 0;
  return fr_reg_or_fp01_operand (op, mode);
}

/* Return 1 if OP is valid as a base register in a reg + offset address.  */

int
basereg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* ??? Should I copy the flag_omit_frame_pointer and cse_not_expected
     checks from pa.c basereg_operand as well?  Seems to be OK without them
     in test runs.  */

  return (register_operand (op, mode) &&
	  REG_POINTER ((GET_CODE (op) == SUBREG) ? SUBREG_REG (op) : op));
}

/* Return 1 if the operands of a move are ok.  */

int
ia64_move_ok (dst, src)
     rtx dst, src;
{
  /* If we're under init_recog_no_volatile, we'll not be able to use
     memory_operand.  So check the code directly and don't worry about
     the validity of the underlying address, which should have been
     checked elsewhere anyway.  */
  if (GET_CODE (dst) != MEM)
    return 1;
  if (GET_CODE (src) == MEM)
    return 0;
  if (register_operand (src, VOIDmode))
    return 1;

  /* Otherwise, this must be a constant, and that either 0 or 0.0 or 1.0.  */
  if (INTEGRAL_MODE_P (GET_MODE (dst)))
    return src == const0_rtx;
  else
    return GET_CODE (src) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (src);
}

/* Return 0 if we are doing C++ code.  This optimization fails with
   C++ because of GNAT c++/6685.  */

int
addp4_optimize_ok (op1, op2)
     rtx op1, op2;
{

  if (!strcmp (lang_hooks.name, "GNU C++"))
    return 0;

  return (basereg_operand (op1, GET_MODE(op1)) !=
	  basereg_operand (op2, GET_MODE(op2)));
}

/* Check if OP is a mask suitible for use with SHIFT in a dep.z instruction.
   Return the length of the field, or <= 0 on failure.  */

int
ia64_depz_field_mask (rop, rshift)
     rtx rop, rshift;
{
  unsigned HOST_WIDE_INT op = INTVAL (rop);
  unsigned HOST_WIDE_INT shift = INTVAL (rshift);

  /* Get rid of the zero bits we're shifting in.  */
  op >>= shift;

  /* We must now have a solid block of 1's at bit 0.  */
  return exact_log2 (op + 1);
}

/* Expand a symbolic constant load.  */
/* ??? Should generalize this, so that we can also support 32 bit pointers.  */

void
ia64_expand_load_address (dest, src, scratch)
      rtx dest, src, scratch;
{
  rtx temp;

  /* The destination could be a MEM during initial rtl generation,
     which isn't a valid destination for the PIC load address patterns.  */
  if (! register_operand (dest, DImode))
    if (! scratch || ! register_operand (scratch, DImode))
      temp = gen_reg_rtx (DImode);
    else
      temp = scratch;
  else
    temp = dest;

  if (tls_symbolic_operand (src, Pmode))
    abort ();

  if (TARGET_AUTO_PIC)
    emit_insn (gen_load_gprel64 (temp, src));
  else if (GET_CODE (src) == SYMBOL_REF && SYMBOL_REF_FLAG (src))
    emit_insn (gen_load_fptr (temp, src));
  else if ((GET_MODE (src) == Pmode || GET_MODE (src) == ptr_mode)
           && sdata_symbolic_operand (src, VOIDmode))
    emit_insn (gen_load_gprel (temp, src));
  else if (GET_CODE (src) == CONST
	   && GET_CODE (XEXP (src, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (src, 0), 1)) == CONST_INT
	   && (INTVAL (XEXP (XEXP (src, 0), 1)) & 0x1fff) != 0)
    {
      rtx subtarget = no_new_pseudos ? temp : gen_reg_rtx (DImode);
      rtx sym = XEXP (XEXP (src, 0), 0);
      HOST_WIDE_INT ofs, hi, lo;

      /* Split the offset into a sign extended 14-bit low part
	 and a complementary high part.  */
      ofs = INTVAL (XEXP (XEXP (src, 0), 1));
      lo = ((ofs & 0x3fff) ^ 0x2000) - 0x2000;
      hi = ofs - lo;

      if (! scratch)
	scratch = no_new_pseudos ? subtarget : gen_reg_rtx (DImode);

      ia64_expand_load_address (subtarget, plus_constant (sym, hi), scratch);
      emit_insn (gen_adddi3 (temp, subtarget, GEN_INT (lo)));
    }
  else
    {
      rtx insn;
      if (! scratch)
	scratch = no_new_pseudos ? temp : gen_reg_rtx (DImode);

      insn = emit_insn (gen_load_symptr (temp, src, scratch));
#ifdef POINTERS_EXTEND_UNSIGNED
      if (GET_MODE (temp) != GET_MODE (src))
	src = convert_memory_address (GET_MODE (temp), src);
#endif
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL, src, REG_NOTES (insn));
    }

  if (temp != dest)
    {
      if (GET_MODE (dest) != GET_MODE (temp))
	temp = convert_to_mode (GET_MODE (dest), temp, 0);
      emit_move_insn (dest, temp);
    }
}

static GTY(()) rtx gen_tls_tga;
static rtx
gen_tls_get_addr ()
{
  if (!gen_tls_tga)
    {
      gen_tls_tga = init_one_libfunc ("__tls_get_addr");
     }
  return gen_tls_tga;
}

static GTY(()) rtx thread_pointer_rtx;
static rtx
gen_thread_pointer ()
{
  if (!thread_pointer_rtx)
    {
      thread_pointer_rtx = gen_rtx_REG (Pmode, 13);
      RTX_UNCHANGING_P (thread_pointer_rtx) = 1;
    }
  return thread_pointer_rtx;
}

rtx
ia64_expand_move (op0, op1)
     rtx op0, op1;
{
  enum machine_mode mode = GET_MODE (op0);

  if (!reload_in_progress && !reload_completed && !ia64_move_ok (op0, op1))
    op1 = force_reg (mode, op1);

  if (mode == Pmode || mode == ptr_mode)
    {
      enum tls_model tls_kind;
      if ((tls_kind = tls_symbolic_operand (op1, Pmode)))
	{
	  rtx tga_op1, tga_op2, tga_ret, tga_eqv, tmp, insns;
	  rtx orig_op0 = op0;

	  switch (tls_kind)
	    {
	    case TLS_MODEL_GLOBAL_DYNAMIC:
	      start_sequence ();

	      tga_op1 = gen_reg_rtx (Pmode);
	      emit_insn (gen_load_ltoff_dtpmod (tga_op1, op1));
	      tga_op1 = gen_rtx_MEM (Pmode, tga_op1);
	      RTX_UNCHANGING_P (tga_op1) = 1;

	      tga_op2 = gen_reg_rtx (Pmode);
	      emit_insn (gen_load_ltoff_dtprel (tga_op2, op1));
	      tga_op2 = gen_rtx_MEM (Pmode, tga_op2);
	      RTX_UNCHANGING_P (tga_op2) = 1;
	      
	      tga_ret = emit_library_call_value (gen_tls_get_addr (), NULL_RTX,
						 LCT_CONST, Pmode, 2, tga_op1,
						 Pmode, tga_op2, Pmode);

	      insns = get_insns ();
	      end_sequence ();

	      if (GET_MODE (op0) != Pmode)
		op0 = tga_ret;
	      emit_libcall_block (insns, op0, tga_ret, op1);
	      break;

	    case TLS_MODEL_LOCAL_DYNAMIC:
	      /* ??? This isn't the completely proper way to do local-dynamic
		 If the call to __tls_get_addr is used only by a single symbol,
		 then we should (somehow) move the dtprel to the second arg
		 to avoid the extra add.  */
	      start_sequence ();

	      tga_op1 = gen_reg_rtx (Pmode);
	      emit_insn (gen_load_ltoff_dtpmod (tga_op1, op1));
	      tga_op1 = gen_rtx_MEM (Pmode, tga_op1);
	      RTX_UNCHANGING_P (tga_op1) = 1;

	      tga_op2 = const0_rtx;

	      tga_ret = emit_library_call_value (gen_tls_get_addr (), NULL_RTX,
						 LCT_CONST, Pmode, 2, tga_op1,
						 Pmode, tga_op2, Pmode);

	      insns = get_insns ();
	      end_sequence ();

	      tga_eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
					UNSPEC_LD_BASE);
	      tmp = gen_reg_rtx (Pmode);
	      emit_libcall_block (insns, tmp, tga_ret, tga_eqv);

	      if (!register_operand (op0, Pmode))
		op0 = gen_reg_rtx (Pmode);
	      if (TARGET_TLS64)
		{
		  emit_insn (gen_load_dtprel (op0, op1));
		  emit_insn (gen_adddi3 (op0, tmp, op0));
		}
	      else
		emit_insn (gen_add_dtprel (op0, tmp, op1));
	      break;

	    case TLS_MODEL_INITIAL_EXEC:
	      tmp = gen_reg_rtx (Pmode);
	      emit_insn (gen_load_ltoff_tprel (tmp, op1));
	      tmp = gen_rtx_MEM (Pmode, tmp);
	      RTX_UNCHANGING_P (tmp) = 1;
	      tmp = force_reg (Pmode, tmp);

	      if (!register_operand (op0, Pmode))
		op0 = gen_reg_rtx (Pmode);
	      emit_insn (gen_adddi3 (op0, tmp, gen_thread_pointer ()));
	      break;

	    case TLS_MODEL_LOCAL_EXEC:
	      if (!register_operand (op0, Pmode))
		op0 = gen_reg_rtx (Pmode);
	      if (TARGET_TLS64)
		{
		  emit_insn (gen_load_tprel (op0, op1));
		  emit_insn (gen_adddi3 (op0, gen_thread_pointer (), op0));
		}
	      else
		emit_insn (gen_add_tprel (op0, gen_thread_pointer (), op1));
	      break;

	    default:
	      abort ();
	    }

	  if (orig_op0 == op0)
	    return NULL_RTX;
	  if (GET_MODE (orig_op0) == Pmode)
	    return op0;
	  return gen_lowpart (GET_MODE (orig_op0), op0);
	}
      else if (!TARGET_NO_PIC &&
	       (symbolic_operand (op1, Pmode) ||
		symbolic_operand (op1, ptr_mode)))
	{
	  /* Before optimization starts, delay committing to any particular
	     type of PIC address load.  If this function gets deferred, we
	     may acquire information that changes the value of the
	     sdata_symbolic_operand predicate.

	     But don't delay for function pointers.  Loading a function address
	     actually loads the address of the descriptor not the function.
	     If we represent these as SYMBOL_REFs, then they get cse'd with
	     calls, and we end up with calls to the descriptor address instead
	     of calls to the function address.  Functions are not candidates
	     for sdata anyways.

	     Don't delay for LABEL_REF because the splitter loses REG_LABEL
	     notes.  Don't delay for pool addresses on general principals;
	     they'll never become non-local behind our back.  */

	  if (rtx_equal_function_value_matters
	      && GET_CODE (op1) != LABEL_REF
	      && ! (GET_CODE (op1) == SYMBOL_REF
		    && (SYMBOL_REF_FLAG (op1)
			|| CONSTANT_POOL_ADDRESS_P (op1)
			|| STRING_POOL_ADDRESS_P (op1))))
	    if (GET_MODE (op1) == DImode)
	      emit_insn (gen_movdi_symbolic (op0, op1));
	    else
	      emit_insn (gen_movsi_symbolic (op0, op1));
	  else
	    ia64_expand_load_address (op0, op1, NULL_RTX);
	  return NULL_RTX;
	}
    }

  return op1;
}

/* Split a post-reload TImode reference into two DImode components.  */

rtx
ia64_split_timode (out, in, scratch)
     rtx out[2];
     rtx in, scratch;
{
  switch (GET_CODE (in))
    {
    case REG:
      out[0] = gen_rtx_REG (DImode, REGNO (in));
      out[1] = gen_rtx_REG (DImode, REGNO (in) + 1);
      return NULL_RTX;

    case MEM:
      {
	rtx base = XEXP (in, 0);

	switch (GET_CODE (base))
	  {
	  case REG:
	    out[0] = adjust_address (in, DImode, 0);
	    break;
	  case POST_MODIFY:
	    base = XEXP (base, 0);
	    out[0] = adjust_address (in, DImode, 0);
	    break;

	  /* Since we're changing the mode, we need to change to POST_MODIFY
	     as well to preserve the size of the increment.  Either that or
	     do the update in two steps, but we've already got this scratch
	     register handy so let's use it.  */
	  case POST_INC:
	    base = XEXP (base, 0);
	    out[0]
	      = change_address (in, DImode,
				gen_rtx_POST_MODIFY
				(Pmode, base, plus_constant (base, 16)));
	    break;
	  case POST_DEC:
	    base = XEXP (base, 0);
	    out[0]
	      = change_address (in, DImode,
				gen_rtx_POST_MODIFY
				(Pmode, base, plus_constant (base, -16)));
	    break;
	  default:
	    abort ();
	  }

	if (scratch == NULL_RTX)
	  abort ();
	out[1] = change_address (in, DImode, scratch);
	return gen_adddi3 (scratch, base, GEN_INT (8));
      }

    case CONST_INT:
    case CONST_DOUBLE:
      split_double (in, &out[0], &out[1]);
      return NULL_RTX;

    default:
      abort ();
    }
}

/* ??? Fixing GR->FR TFmode moves during reload is hard.  You need to go
   through memory plus an extra GR scratch register.  Except that you can
   either get the first from SECONDARY_MEMORY_NEEDED or the second from
   SECONDARY_RELOAD_CLASS, but not both.

   We got into problems in the first place by allowing a construct like
   (subreg:TF (reg:TI)), which we got from a union containing a long double.  
   This solution attempts to prevent this situation from occurring.  When
   we see something like the above, we spill the inner register to memory.  */

rtx
spill_tfmode_operand (in, force)
     rtx in;
     int force;
{
  if (GET_CODE (in) == SUBREG
      && GET_MODE (SUBREG_REG (in)) == TImode
      && GET_CODE (SUBREG_REG (in)) == REG)
    {
      rtx mem = gen_mem_addressof (SUBREG_REG (in), NULL_TREE, true);
      return gen_rtx_MEM (TFmode, copy_to_reg (XEXP (mem, 0)));
    }
  else if (force && GET_CODE (in) == REG)
    {
      rtx mem = gen_mem_addressof (in, NULL_TREE, true);
      return gen_rtx_MEM (TFmode, copy_to_reg (XEXP (mem, 0)));
    }
  else if (GET_CODE (in) == MEM
	   && GET_CODE (XEXP (in, 0)) == ADDRESSOF)
    return change_address (in, TFmode, copy_to_reg (XEXP (in, 0)));
  else
    return in;
}

/* Emit comparison instruction if necessary, returning the expression
   that holds the compare result in the proper mode.  */

rtx
ia64_expand_compare (code, mode)
     enum rtx_code code;
     enum machine_mode mode;
{
  rtx op0 = ia64_compare_op0, op1 = ia64_compare_op1;
  rtx cmp;

  /* If we have a BImode input, then we already have a compare result, and
     do not need to emit another comparison.  */
  if (GET_MODE (op0) == BImode)
    {
      if ((code == NE || code == EQ) && op1 == const0_rtx)
	cmp = op0;
      else
	abort ();
    }
  else
    {
      cmp = gen_reg_rtx (BImode);
      emit_insn (gen_rtx_SET (VOIDmode, cmp,
			      gen_rtx_fmt_ee (code, BImode, op0, op1)));
      code = NE;
    }

  return gen_rtx_fmt_ee (code, mode, cmp, const0_rtx);
}

/* Emit the appropriate sequence for a call.  */
void
ia64_expand_call (retval, addr, nextarg, sibcall_p)
     rtx retval;
     rtx addr;
     rtx nextarg ATTRIBUTE_UNUSED;
     int sibcall_p;
{
  rtx insn, b0;

  addr = XEXP (addr, 0);
  addr = convert_memory_address (DImode, addr);
  b0 = gen_rtx_REG (DImode, R_BR (0));

  /* ??? Should do this for functions known to bind local too.  */
  if (TARGET_NO_PIC || TARGET_AUTO_PIC)
    {
      if (sibcall_p)
	insn = gen_sibcall_nogp (addr);
      else if (! retval)
	insn = gen_call_nogp (addr, b0);
      else
	insn = gen_call_value_nogp (retval, addr, b0);
      insn = emit_call_insn (insn);
    }
  else
    {
      if (sibcall_p)
	insn = gen_sibcall_gp (addr);
      else if (! retval)
	insn = gen_call_gp (addr, b0);
      else
	insn = gen_call_value_gp (retval, addr, b0);
      insn = emit_call_insn (insn);

      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), pic_offset_table_rtx);
    }

  if (sibcall_p)
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), b0);
}
void
ia64_reload_gp ()
{
  rtx tmp;

  if (current_frame_info.reg_save_gp)
    tmp = gen_rtx_REG (DImode, current_frame_info.reg_save_gp);
  else
    {
      HOST_WIDE_INT offset;

      offset = (current_frame_info.spill_cfa_off
	        + current_frame_info.spill_size);
      if (frame_pointer_needed)
        {
          tmp = hard_frame_pointer_rtx;
          offset = -offset;
        }
      else
        {
          tmp = stack_pointer_rtx;
          offset = current_frame_info.total_size - offset;
        }

      if (CONST_OK_FOR_I (offset))
        emit_insn (gen_adddi3 (pic_offset_table_rtx,
			       tmp, GEN_INT (offset)));
      else
        {
          emit_move_insn (pic_offset_table_rtx, GEN_INT (offset));
          emit_insn (gen_adddi3 (pic_offset_table_rtx,
			         pic_offset_table_rtx, tmp));
        }

      tmp = gen_rtx_MEM (DImode, pic_offset_table_rtx);
    }

  emit_move_insn (pic_offset_table_rtx, tmp);
}

void
ia64_split_call (retval, addr, retaddr, scratch_r, scratch_b,
		 noreturn_p, sibcall_p)
     rtx retval, addr, retaddr, scratch_r, scratch_b;
     int noreturn_p, sibcall_p;
{
  rtx insn;
  bool is_desc = false;

  /* If we find we're calling through a register, then we're actually
     calling through a descriptor, so load up the values.  */
  if (REG_P (addr) && GR_REGNO_P (REGNO (addr)))
    {
      rtx tmp;
      bool addr_dead_p;

      /* ??? We are currently constrained to *not* use peep2, because
	 we can legitimiately change the global lifetime of the GP
	 (in the form of killing where previously live).  This is 
	 because a call through a descriptor doesn't use the previous
	 value of the GP, while a direct call does, and we do not
	 commit to either form until the split here.

	 That said, this means that we lack precise life info for
	 whether ADDR is dead after this call.  This is not terribly
	 important, since we can fix things up essentially for free
	 with the POST_DEC below, but it's nice to not use it when we
	 can immediately tell it's not necessary.  */
      addr_dead_p = ((noreturn_p || sibcall_p
		      || TEST_HARD_REG_BIT (regs_invalidated_by_call,
					    REGNO (addr)))
		     && !FUNCTION_ARG_REGNO_P (REGNO (addr)));

      /* Load the code address into scratch_b.  */
      tmp = gen_rtx_POST_INC (Pmode, addr);
      tmp = gen_rtx_MEM (Pmode, tmp);
      emit_move_insn (scratch_r, tmp);
      emit_move_insn (scratch_b, scratch_r);

      /* Load the GP address.  If ADDR is not dead here, then we must
	 revert the change made above via the POST_INCREMENT.  */
      if (!addr_dead_p)
	tmp = gen_rtx_POST_DEC (Pmode, addr);
      else
	tmp = addr;
      tmp = gen_rtx_MEM (Pmode, tmp);
      emit_move_insn (pic_offset_table_rtx, tmp);

      is_desc = true;
      addr = scratch_b;
    }

  if (sibcall_p)
    insn = gen_sibcall_nogp (addr);
  else if (retval)
    insn = gen_call_value_nogp (retval, addr, retaddr);
  else
    insn = gen_call_nogp (addr, retaddr);
  emit_call_insn (insn);

  if ((!TARGET_CONST_GP || is_desc) && !noreturn_p && !sibcall_p)
    ia64_reload_gp ();
}

/* Begin the assembly file.  */

void
emit_safe_across_calls (f)
     FILE *f;
{
  unsigned int rs, re;
  int out_state;

  rs = 1;
  out_state = 0;
  while (1)
    {
      while (rs < 64 && call_used_regs[PR_REG (rs)])
	rs++;
      if (rs >= 64)
	break;
      for (re = rs + 1; re < 64 && ! call_used_regs[PR_REG (re)]; re++)
	continue;
      if (out_state == 0)
	{
	  fputs ("\t.pred.safe_across_calls ", f);
	  out_state = 1;
	}
      else
	fputc (',', f);
      if (re == rs + 1)
	fprintf (f, "p%u", rs);
      else
	fprintf (f, "p%u-p%u", rs, re - 1);
      rs = re + 1;
    }
  if (out_state)
    fputc ('\n', f);
}

/* Helper function for ia64_compute_frame_size: find an appropriate general
   register to spill some special register to.  SPECIAL_SPILL_MASK contains
   bits in GR0 to GR31 that have already been allocated by this routine.
   TRY_LOCALS is true if we should attempt to locate a local regnum.  */

static int
find_gr_spill (try_locals)
     int try_locals;
{
  int regno;

  /* If this is a leaf function, first try an otherwise unused
     call-clobbered register.  */
  if (current_function_is_leaf)
    {
      for (regno = GR_REG (1); regno <= GR_REG (31); regno++)
	if (! regs_ever_live[regno]
	    && call_used_regs[regno]
	    && ! fixed_regs[regno]
	    && ! global_regs[regno]
	    && ((current_frame_info.gr_used_mask >> regno) & 1) == 0)
	  {
	    current_frame_info.gr_used_mask |= 1 << regno;
	    return regno;
	  }
    }

  if (try_locals)
    {
      regno = current_frame_info.n_local_regs;
      /* If there is a frame pointer, then we can't use loc79, because
	 that is HARD_FRAME_POINTER_REGNUM.  In particular, see the
	 reg_name switching code in ia64_expand_prologue.  */
      if (regno < (80 - frame_pointer_needed))
	{
	  current_frame_info.n_local_regs = regno + 1;
	  return LOC_REG (0) + regno;
	}
    }

  /* Failed to find a general register to spill to.  Must use stack.  */
  return 0;
}

/* In order to make for nice schedules, we try to allocate every temporary
   to a different register.  We must of course stay away from call-saved,
   fixed, and global registers.  We must also stay away from registers
   allocated in current_frame_info.gr_used_mask, since those include regs
   used all through the prologue.

   Any register allocated here must be used immediately.  The idea is to
   aid scheduling, not to solve data flow problems.  */

static int last_scratch_gr_reg;

static int
next_scratch_gr_reg ()
{
  int i, regno;

  for (i = 0; i < 32; ++i)
    {
      regno = (last_scratch_gr_reg + i + 1) & 31;
      if (call_used_regs[regno]
	  && ! fixed_regs[regno]
	  && ! global_regs[regno]
	  && ((current_frame_info.gr_used_mask >> regno) & 1) == 0)
	{
	  last_scratch_gr_reg = regno;
	  return regno;
	}
    }

  /* There must be _something_ available.  */
  abort ();
}

/* Helper function for ia64_compute_frame_size, called through
   diddle_return_value.  Mark REG in current_frame_info.gr_used_mask.  */

static void
mark_reg_gr_used_mask (reg, data)
     rtx reg;
     void *data ATTRIBUTE_UNUSED;
{
  unsigned int regno = REGNO (reg);
  if (regno < 32)
    {
      unsigned int i, n = HARD_REGNO_NREGS (regno, GET_MODE (reg));
      for (i = 0; i < n; ++i)
	current_frame_info.gr_used_mask |= 1 << (regno + i);
    }
}

/* Returns the number of bytes offset between the frame pointer and the stack
   pointer for the current function.  SIZE is the number of bytes of space
   needed for local variables.  */

static void
ia64_compute_frame_size (size)
     HOST_WIDE_INT size;
{
  HOST_WIDE_INT total_size;
  HOST_WIDE_INT spill_size = 0;
  HOST_WIDE_INT extra_spill_size = 0;
  HOST_WIDE_INT pretend_args_size;
  HARD_REG_SET mask;
  int n_spilled = 0;
  int spilled_gr_p = 0;
  int spilled_fr_p = 0;
  unsigned int regno;
  int i;

  if (current_frame_info.initialized)
    return;

  memset (&current_frame_info, 0, sizeof current_frame_info);
  CLEAR_HARD_REG_SET (mask);

  /* Don't allocate scratches to the return register.  */
  diddle_return_value (mark_reg_gr_used_mask, NULL);

  /* Don't allocate scratches to the EH scratch registers.  */
  if (cfun->machine->ia64_eh_epilogue_sp)
    mark_reg_gr_used_mask (cfun->machine->ia64_eh_epilogue_sp, NULL);
  if (cfun->machine->ia64_eh_epilogue_bsp)
    mark_reg_gr_used_mask (cfun->machine->ia64_eh_epilogue_bsp, NULL);

  /* Find the size of the register stack frame.  We have only 80 local
     registers, because we reserve 8 for the inputs and 8 for the
     outputs.  */

  /* Skip HARD_FRAME_POINTER_REGNUM (loc79) when frame_pointer_needed,
     since we'll be adjusting that down later.  */
  regno = LOC_REG (78) + ! frame_pointer_needed;
  for (; regno >= LOC_REG (0); regno--)
    if (regs_ever_live[regno])
      break;
  current_frame_info.n_local_regs = regno - LOC_REG (0) + 1;

  /* For functions marked with the syscall_linkage attribute, we must mark
     all eight input registers as in use, so that locals aren't visible to
     the caller.  */

  if (cfun->machine->n_varargs > 0
      || lookup_attribute ("syscall_linkage",
			   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    current_frame_info.n_input_regs = 8;
  else
    {
      for (regno = IN_REG (7); regno >= IN_REG (0); regno--)
	if (regs_ever_live[regno])
	  break;
      current_frame_info.n_input_regs = regno - IN_REG (0) + 1;
    }

  for (regno = OUT_REG (7); regno >= OUT_REG (0); regno--)
    if (regs_ever_live[regno])
      break;
  i = regno - OUT_REG (0) + 1;

  /* When -p profiling, we need one output register for the mcount argument.
     Likwise for -a profiling for the bb_init_func argument.  For -ax
     profiling, we need two output registers for the two bb_init_trace_func
     arguments.  */
  if (current_function_profile)
    i = MAX (i, 1);
  current_frame_info.n_output_regs = i;

  /* ??? No rotating register support yet.  */
  current_frame_info.n_rotate_regs = 0;

  /* Discover which registers need spilling, and how much room that
     will take.  Begin with floating point and general registers, 
     which will always wind up on the stack.  */

  for (regno = FR_REG (2); regno <= FR_REG (127); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 16;
	n_spilled += 1;
	spilled_fr_p = 1;
      }

  for (regno = GR_REG (1); regno <= GR_REG (31); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 8;
	n_spilled += 1;
	spilled_gr_p = 1;
      }

  for (regno = BR_REG (1); regno <= BR_REG (7); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	spill_size += 8;
	n_spilled += 1;
      }

  /* Now come all special registers that might get saved in other
     general registers.  */
  
  if (frame_pointer_needed)
    {
      current_frame_info.reg_fp = find_gr_spill (1);
      /* If we did not get a register, then we take LOC79.  This is guaranteed
	 to be free, even if regs_ever_live is already set, because this is
	 HARD_FRAME_POINTER_REGNUM.  This requires incrementing n_local_regs,
	 as we don't count loc79 above.  */
      if (current_frame_info.reg_fp == 0)
	{
	  current_frame_info.reg_fp = LOC_REG (79);
	  current_frame_info.n_local_regs++;
	}
    }

  if (! current_function_is_leaf)
    {
      /* Emit a save of BR0 if we call other functions.  Do this even
	 if this function doesn't return, as EH depends on this to be
	 able to unwind the stack.  */
      SET_HARD_REG_BIT (mask, BR_REG (0));

      current_frame_info.reg_save_b0 = find_gr_spill (1);
      if (current_frame_info.reg_save_b0 == 0)
	{
	  spill_size += 8;
	  n_spilled += 1;
	}

      /* Similarly for ar.pfs.  */
      SET_HARD_REG_BIT (mask, AR_PFS_REGNUM);
      current_frame_info.reg_save_ar_pfs = find_gr_spill (1);
      if (current_frame_info.reg_save_ar_pfs == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}

      /* Similarly for gp.  Note that if we're calling setjmp, the stacked
	 registers are clobbered, so we fall back to the stack.  */
      current_frame_info.reg_save_gp
	= (current_function_calls_setjmp ? 0 : find_gr_spill (1));
      if (current_frame_info.reg_save_gp == 0)
	{
	  SET_HARD_REG_BIT (mask, GR_REG (1));
	  spill_size += 8;
	  n_spilled += 1;
	}
    }
  else
    {
      if (regs_ever_live[BR_REG (0)] && ! call_used_regs[BR_REG (0)])
	{
	  SET_HARD_REG_BIT (mask, BR_REG (0));
	  spill_size += 8;
	  n_spilled += 1;
	}

      if (regs_ever_live[AR_PFS_REGNUM])
	{
	  SET_HARD_REG_BIT (mask, AR_PFS_REGNUM);
	  current_frame_info.reg_save_ar_pfs = find_gr_spill (1);
	  if (current_frame_info.reg_save_ar_pfs == 0)
	    {
	      extra_spill_size += 8;
	      n_spilled += 1;
	    }
	}
    }

  /* Unwind descriptor hackery: things are most efficient if we allocate
     consecutive GR save registers for RP, PFS, FP in that order. However,
     it is absolutely critical that FP get the only hard register that's
     guaranteed to be free, so we allocated it first.  If all three did
     happen to be allocated hard regs, and are consecutive, rearrange them
     into the preferred order now.  */
  if (current_frame_info.reg_fp != 0
      && current_frame_info.reg_save_b0 == current_frame_info.reg_fp + 1
      && current_frame_info.reg_save_ar_pfs == current_frame_info.reg_fp + 2)
    {
      current_frame_info.reg_save_b0 = current_frame_info.reg_fp;
      current_frame_info.reg_save_ar_pfs = current_frame_info.reg_fp + 1;
      current_frame_info.reg_fp = current_frame_info.reg_fp + 2;
    }

  /* See if we need to store the predicate register block.  */
  for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      break;
  if (regno <= PR_REG (63))
    {
      SET_HARD_REG_BIT (mask, PR_REG (0));
      current_frame_info.reg_save_pr = find_gr_spill (1);
      if (current_frame_info.reg_save_pr == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}

      /* ??? Mark them all as used so that register renaming and such
	 are free to use them.  */
      for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
	regs_ever_live[regno] = 1;
    }

  /* If we're forced to use st8.spill, we're forced to save and restore
     ar.unat as well.  The check for existing liveness allows inline asm
     to touch ar.unat.  */
  if (spilled_gr_p || cfun->machine->n_varargs
      || regs_ever_live[AR_UNAT_REGNUM])
    {
      regs_ever_live[AR_UNAT_REGNUM] = 1;
      SET_HARD_REG_BIT (mask, AR_UNAT_REGNUM);
      current_frame_info.reg_save_ar_unat = find_gr_spill (spill_size == 0);
      if (current_frame_info.reg_save_ar_unat == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}
    }

  if (regs_ever_live[AR_LC_REGNUM])
    {
      SET_HARD_REG_BIT (mask, AR_LC_REGNUM);
      current_frame_info.reg_save_ar_lc = find_gr_spill (spill_size == 0);
      if (current_frame_info.reg_save_ar_lc == 0)
	{
	  extra_spill_size += 8;
	  n_spilled += 1;
	}
    }

  /* If we have an odd number of words of pretend arguments written to
     the stack, then the FR save area will be unaligned.  We round the
     size of this area up to keep things 16 byte aligned.  */
  if (spilled_fr_p)
    pretend_args_size = IA64_STACK_ALIGN (current_function_pretend_args_size);
  else
    pretend_args_size = current_function_pretend_args_size;

  total_size = (spill_size + extra_spill_size + size + pretend_args_size
		+ current_function_outgoing_args_size);
  total_size = IA64_STACK_ALIGN (total_size);

  /* We always use the 16-byte scratch area provided by the caller, but
     if we are a leaf function, there's no one to which we need to provide
     a scratch area.  */
  if (current_function_is_leaf)
    total_size = MAX (0, total_size - 16);

  current_frame_info.total_size = total_size;
  current_frame_info.spill_cfa_off = pretend_args_size - 16;
  current_frame_info.spill_size = spill_size;
  current_frame_info.extra_spill_size = extra_spill_size;
  COPY_HARD_REG_SET (current_frame_info.mask, mask);
  current_frame_info.n_spilled = n_spilled;
  current_frame_info.initialized = reload_completed;
}

/* Compute the initial difference between the specified pair of registers.  */

HOST_WIDE_INT
ia64_initial_elimination_offset (from, to)
     int from, to;
{
  HOST_WIDE_INT offset;

  ia64_compute_frame_size (get_frame_size ());
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      if (to == HARD_FRAME_POINTER_REGNUM)
	{
	  if (current_function_is_leaf)
	    offset = -current_frame_info.total_size;
	  else
	    offset = -(current_frame_info.total_size
		       - current_function_outgoing_args_size - 16);
	}
      else if (to == STACK_POINTER_REGNUM)
	{
	  if (current_function_is_leaf)
	    offset = 0;
	  else
	    offset = 16 + current_function_outgoing_args_size;
	}
      else
	abort ();
      break;

    case ARG_POINTER_REGNUM:
      /* Arguments start above the 16 byte save area, unless stdarg
	 in which case we store through the 16 byte save area.  */
      if (to == HARD_FRAME_POINTER_REGNUM)
	offset = 16 - current_function_pretend_args_size;
      else if (to == STACK_POINTER_REGNUM)
	offset = (current_frame_info.total_size
		  + 16 - current_function_pretend_args_size);
      else
	abort ();
      break;

    default:
      abort ();
    }

  return offset;
}

/* If there are more than a trivial number of register spills, we use
   two interleaved iterators so that we can get two memory references
   per insn group.

   In order to simplify things in the prologue and epilogue expanders,
   we use helper functions to fix up the memory references after the
   fact with the appropriate offsets to a POST_MODIFY memory mode.
   The following data structure tracks the state of the two iterators
   while insns are being emitted.  */

struct spill_fill_data
{
  rtx init_after;		/* point at which to emit initializations */
  rtx init_reg[2];		/* initial base register */
  rtx iter_reg[2];		/* the iterator registers */
  rtx *prev_addr[2];		/* address of last memory use */
  rtx prev_insn[2];		/* the insn corresponding to prev_addr */
  HOST_WIDE_INT prev_off[2];	/* last offset */
  int n_iter;			/* number of iterators in use */
  int next_iter;		/* next iterator to use */
  unsigned int save_gr_used_mask;
};

static struct spill_fill_data spill_fill_data;

static void
setup_spill_pointers (n_spills, init_reg, cfa_off)
     int n_spills;
     rtx init_reg;
     HOST_WIDE_INT cfa_off;
{
  int i;

  spill_fill_data.init_after = get_last_insn ();
  spill_fill_data.init_reg[0] = init_reg;
  spill_fill_data.init_reg[1] = init_reg;
  spill_fill_data.prev_addr[0] = NULL;
  spill_fill_data.prev_addr[1] = NULL;
  spill_fill_data.prev_insn[0] = NULL;
  spill_fill_data.prev_insn[1] = NULL;
  spill_fill_data.prev_off[0] = cfa_off;
  spill_fill_data.prev_off[1] = cfa_off;
  spill_fill_data.next_iter = 0;
  spill_fill_data.save_gr_used_mask = current_frame_info.gr_used_mask;

  spill_fill_data.n_iter = 1 + (n_spills > 2);
  for (i = 0; i < spill_fill_data.n_iter; ++i)
    {
      int regno = next_scratch_gr_reg ();
      spill_fill_data.iter_reg[i] = gen_rtx_REG (DImode, regno);
      current_frame_info.gr_used_mask |= 1 << regno;
    }
}

static void
finish_spill_pointers ()
{
  current_frame_info.gr_used_mask = spill_fill_data.save_gr_used_mask;
}

static rtx
spill_restore_mem (reg, cfa_off)
     rtx reg;
     HOST_WIDE_INT cfa_off;
{
  int iter = spill_fill_data.next_iter;
  HOST_WIDE_INT disp = spill_fill_data.prev_off[iter] - cfa_off;
  rtx disp_rtx = GEN_INT (disp);
  rtx mem;

  if (spill_fill_data.prev_addr[iter])
    {
      if (CONST_OK_FOR_N (disp))
	{
	  *spill_fill_data.prev_addr[iter]
	    = gen_rtx_POST_MODIFY (DImode, spill_fill_data.iter_reg[iter],
				   gen_rtx_PLUS (DImode,
						 spill_fill_data.iter_reg[iter],
						 disp_rtx));
	  REG_NOTES (spill_fill_data.prev_insn[iter])
	    = gen_rtx_EXPR_LIST (REG_INC, spill_fill_data.iter_reg[iter],
				 REG_NOTES (spill_fill_data.prev_insn[iter]));
	}
      else
	{
	  /* ??? Could use register post_modify for loads.  */
	  if (! CONST_OK_FOR_I (disp))
	    {
	      rtx tmp = gen_rtx_REG (DImode, next_scratch_gr_reg ());
	      emit_move_insn (tmp, disp_rtx);
	      disp_rtx = tmp;
	    }
	  emit_insn (gen_adddi3 (spill_fill_data.iter_reg[iter],
				 spill_fill_data.iter_reg[iter], disp_rtx));
	}
    }
  /* Micro-optimization: if we've created a frame pointer, it's at
     CFA 0, which may allow the real iterator to be initialized lower,
     slightly increasing parallelism.  Also, if there are few saves
     it may eliminate the iterator entirely.  */
  else if (disp == 0
	   && spill_fill_data.init_reg[iter] == stack_pointer_rtx
	   && frame_pointer_needed)
    {
      mem = gen_rtx_MEM (GET_MODE (reg), hard_frame_pointer_rtx);
      set_mem_alias_set (mem, get_varargs_alias_set ());
      return mem;
    }
  else
    {
      rtx seq, insn;

      if (disp == 0)
	seq = gen_movdi (spill_fill_data.iter_reg[iter],
			 spill_fill_data.init_reg[iter]);
      else
	{
	  start_sequence ();

	  if (! CONST_OK_FOR_I (disp))
	    {
	      rtx tmp = gen_rtx_REG (DImode, next_scratch_gr_reg ());
	      emit_move_insn (tmp, disp_rtx);
	      disp_rtx = tmp;
	    }

	  emit_insn (gen_adddi3 (spill_fill_data.iter_reg[iter],
				 spill_fill_data.init_reg[iter],
				 disp_rtx));

	  seq = get_insns ();
	  end_sequence ();
	}

      /* Careful for being the first insn in a sequence.  */
      if (spill_fill_data.init_after)
	insn = emit_insn_after (seq, spill_fill_data.init_after);
      else
	{
	  rtx first = get_insns ();
	  if (first)
	    insn = emit_insn_before (seq, first);
	  else
	    insn = emit_insn (seq);
	}
      spill_fill_data.init_after = insn;

      /* If DISP is 0, we may or may not have a further adjustment
	 afterward.  If we do, then the load/store insn may be modified
	 to be a post-modify.  If we don't, then this copy may be
	 eliminated by copyprop_hardreg_forward, which makes this
	 insn garbage, which runs afoul of the sanity check in
	 propagate_one_insn.  So mark this insn as legal to delete.  */
      if (disp == 0)
	REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx,
					     REG_NOTES (insn));
    }

  mem = gen_rtx_MEM (GET_MODE (reg), spill_fill_data.iter_reg[iter]);

  /* ??? Not all of the spills are for varargs, but some of them are.
     The rest of the spills belong in an alias set of their own.  But
     it doesn't actually hurt to include them here.  */
  set_mem_alias_set (mem, get_varargs_alias_set ());

  spill_fill_data.prev_addr[iter] = &XEXP (mem, 0);
  spill_fill_data.prev_off[iter] = cfa_off;

  if (++iter >= spill_fill_data.n_iter)
    iter = 0;
  spill_fill_data.next_iter = iter;

  return mem;
}

static void
do_spill (move_fn, reg, cfa_off, frame_reg)
     rtx (*move_fn) PARAMS ((rtx, rtx, rtx));
     rtx reg, frame_reg;
     HOST_WIDE_INT cfa_off;
{
  int iter = spill_fill_data.next_iter;
  rtx mem, insn;

  mem = spill_restore_mem (reg, cfa_off);
  insn = emit_insn ((*move_fn) (mem, reg, GEN_INT (cfa_off)));
  spill_fill_data.prev_insn[iter] = insn;

  if (frame_reg)
    {
      rtx base;
      HOST_WIDE_INT off;

      RTX_FRAME_RELATED_P (insn) = 1;

      /* Don't even pretend that the unwind code can intuit its way 
	 through a pair of interleaved post_modify iterators.  Just
	 provide the correct answer.  */

      if (frame_pointer_needed)
	{
	  base = hard_frame_pointer_rtx;
	  off = - cfa_off;
	}
      else
	{
	  base = stack_pointer_rtx;
	  off = current_frame_info.total_size - cfa_off;
	}

      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode,
			     gen_rtx_MEM (GET_MODE (reg),
					  plus_constant (base, off)),
			     frame_reg),
		REG_NOTES (insn));
    }
}

static void
do_restore (move_fn, reg, cfa_off)
     rtx (*move_fn) PARAMS ((rtx, rtx, rtx));
     rtx reg;
     HOST_WIDE_INT cfa_off;
{
  int iter = spill_fill_data.next_iter;
  rtx insn;

  insn = emit_insn ((*move_fn) (reg, spill_restore_mem (reg, cfa_off),
				GEN_INT (cfa_off)));
  spill_fill_data.prev_insn[iter] = insn;
}

/* Wrapper functions that discards the CONST_INT spill offset.  These
   exist so that we can give gr_spill/gr_fill the offset they need and
   use a consistant function interface.  */

static rtx
gen_movdi_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_movdi (dest, src);
}

static rtx
gen_fr_spill_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_fr_spill (dest, src);
}

static rtx
gen_fr_restore_x (dest, src, offset)
     rtx dest, src;
     rtx offset ATTRIBUTE_UNUSED;
{
  return gen_fr_restore (dest, src);
}

/* Called after register allocation to add any instructions needed for the
   prologue.  Using a prologue insn is favored compared to putting all of the
   instructions in output_function_prologue(), since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.

   The register save area is layed out like so:
   cfa+16
	[ varargs spill area ]
	[ fr register spill area ]
	[ br register spill area ]
	[ ar register spill area ]
	[ pr register spill area ]
	[ gr register spill area ] */

/* ??? Get inefficient code when the frame size is larger than can fit in an
   adds instruction.  */

void
ia64_expand_prologue ()
{
  rtx insn, ar_pfs_save_reg, ar_unat_save_reg;
  int i, epilogue_p, regno, alt_regno, cfa_off, n_varargs;
  rtx reg, alt_reg;

  ia64_compute_frame_size (get_frame_size ());
  last_scratch_gr_reg = 15;

  /* If there is no epilogue, then we don't need some prologue insns.
     We need to avoid emitting the dead prologue insns, because flow
     will complain about them.  */
  if (optimize)
    {
      edge e;

      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	if ((e->flags & EDGE_FAKE) == 0
	    && (e->flags & EDGE_FALLTHRU) != 0)
	  break;
      epilogue_p = (e != NULL);
    }
  else
    epilogue_p = 1;

  /* Set the local, input, and output register names.  We need to do this
     for GNU libc, which creates crti.S/crtn.S by splitting initfini.c in
     half.  If we use in/loc/out register names, then we get assembler errors
     in crtn.S because there is no alloc insn or regstk directive in there.  */
  if (! TARGET_REG_NAMES)
    {
      int inputs = current_frame_info.n_input_regs;
      int locals = current_frame_info.n_local_regs;
      int outputs = current_frame_info.n_output_regs;

      for (i = 0; i < inputs; i++)
	reg_names[IN_REG (i)] = ia64_reg_numbers[i];
      for (i = 0; i < locals; i++)
	reg_names[LOC_REG (i)] = ia64_reg_numbers[inputs + i];
      for (i = 0; i < outputs; i++)
	reg_names[OUT_REG (i)] = ia64_reg_numbers[inputs + locals + i];
    }

  /* Set the frame pointer register name.  The regnum is logically loc79,
     but of course we'll not have allocated that many locals.  Rather than
     worrying about renumbering the existing rtxs, we adjust the name.  */
  /* ??? This code means that we can never use one local register when
     there is a frame pointer.  loc79 gets wasted in this case, as it is
     renamed to a register that will never be used.  See also the try_locals
     code in find_gr_spill.  */
  if (current_frame_info.reg_fp)
    {
      const char *tmp = reg_names[HARD_FRAME_POINTER_REGNUM];
      reg_names[HARD_FRAME_POINTER_REGNUM]
	= reg_names[current_frame_info.reg_fp];
      reg_names[current_frame_info.reg_fp] = tmp;
    }

  /* We don't need an alloc instruction if we've used no outputs or locals.  */
  if (current_frame_info.n_local_regs == 0
      && current_frame_info.n_output_regs == 0
      && current_frame_info.n_input_regs <= current_function_args_info.int_regs
      && !TEST_HARD_REG_BIT (current_frame_info.mask, AR_PFS_REGNUM))
    {
      /* If there is no alloc, but there are input registers used, then we
	 need a .regstk directive.  */
      current_frame_info.need_regstk = (TARGET_REG_NAMES != 0);
      ar_pfs_save_reg = NULL_RTX;
    }
  else
    {
      current_frame_info.need_regstk = 0;

      if (current_frame_info.reg_save_ar_pfs)
	regno = current_frame_info.reg_save_ar_pfs;
      else
	regno = next_scratch_gr_reg ();
      ar_pfs_save_reg = gen_rtx_REG (DImode, regno);

      insn = emit_insn (gen_alloc (ar_pfs_save_reg, 
				   GEN_INT (current_frame_info.n_input_regs),
				   GEN_INT (current_frame_info.n_local_regs),
				   GEN_INT (current_frame_info.n_output_regs),
				   GEN_INT (current_frame_info.n_rotate_regs)));
      RTX_FRAME_RELATED_P (insn) = (current_frame_info.reg_save_ar_pfs != 0);
    }

  /* Set up frame pointer, stack pointer, and spill iterators.  */

  n_varargs = cfun->machine->n_varargs;
  setup_spill_pointers (current_frame_info.n_spilled + n_varargs,
			stack_pointer_rtx, 0);

  if (frame_pointer_needed)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (current_frame_info.total_size != 0)
    {
      rtx frame_size_rtx = GEN_INT (- current_frame_info.total_size);
      rtx offset;

      if (CONST_OK_FOR_I (- current_frame_info.total_size))
	offset = frame_size_rtx;
      else
	{
	  regno = next_scratch_gr_reg ();
 	  offset = gen_rtx_REG (DImode, regno);
	  emit_move_insn (offset, frame_size_rtx);
	}

      insn = emit_insn (gen_adddi3 (stack_pointer_rtx,
				    stack_pointer_rtx, offset));

      if (! frame_pointer_needed)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  if (GET_CODE (offset) != CONST_INT)
	    {
	      REG_NOTES (insn)
		= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     gen_rtx_PLUS (DImode,
						   stack_pointer_rtx,
						   frame_size_rtx)),
			REG_NOTES (insn));
	    }
	}

      /* ??? At this point we must generate a magic insn that appears to
	 modify the stack pointer, the frame pointer, and all spill
	 iterators.  This would allow the most scheduling freedom.  For
	 now, just hard stop.  */
      emit_insn (gen_blockage ());
    }

  /* Must copy out ar.unat before doing any integer spills.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      if (current_frame_info.reg_save_ar_unat)
	ar_unat_save_reg
	  = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_unat);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  ar_unat_save_reg = gen_rtx_REG (DImode, alt_regno);
	  current_frame_info.gr_used_mask |= 1 << alt_regno;
	}

      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      insn = emit_move_insn (ar_unat_save_reg, reg);
      RTX_FRAME_RELATED_P (insn) = (current_frame_info.reg_save_ar_unat != 0);

      /* Even if we're not going to generate an epilogue, we still
	 need to save the register so that EH works.  */
      if (! epilogue_p && current_frame_info.reg_save_ar_unat)
	emit_insn (gen_prologue_use (ar_unat_save_reg));
    }
  else
    ar_unat_save_reg = NULL_RTX;

  /* Spill all varargs registers.  Do this before spilling any GR registers,
     since we want the UNAT bits for the GR registers to override the UNAT
     bits from varargs, which we don't care about.  */

  cfa_off = -16;
  for (regno = GR_ARG_FIRST + 7; n_varargs > 0; --n_varargs, --regno)
    {
      reg = gen_rtx_REG (DImode, regno);
      do_spill (gen_gr_spill, reg, cfa_off += 8, NULL_RTX);
    }

  /* Locate the bottom of the register save area.  */
  cfa_off = (current_frame_info.spill_cfa_off
	     + current_frame_info.spill_size
	     + current_frame_info.extra_spill_size);

  /* Save the predicate register block either in a register or in memory.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, PR_REG (0)))
    {
      reg = gen_rtx_REG (DImode, PR_REG (0));
      if (current_frame_info.reg_save_pr != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_pr);
	  insn = emit_move_insn (alt_reg, reg);

	  /* ??? Denote pr spill/fill by a DImode move that modifies all
	     64 hard registers.  */
	  RTX_FRAME_RELATED_P (insn) = 1;
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode, alt_reg, reg),
			REG_NOTES (insn));

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_prologue_use (alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  insn = emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  /* Handle AR regs in numerical order.  All of them get special handling.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM)
      && current_frame_info.reg_save_ar_unat == 0)
    {
      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      do_spill (gen_movdi_x, ar_unat_save_reg, cfa_off, reg);
      cfa_off -= 8;
    }

  /* The alloc insn already copied ar.pfs into a general register.  The
     only thing we have to do now is copy that register to a stack slot
     if we'd not allocated a local register for the job.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_PFS_REGNUM)
      && current_frame_info.reg_save_ar_pfs == 0)
    {
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      do_spill (gen_movdi_x, ar_pfs_save_reg, cfa_off, reg);
      cfa_off -= 8;
    }

  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_LC_REGNUM))
    {
      reg = gen_rtx_REG (DImode, AR_LC_REGNUM);
      if (current_frame_info.reg_save_ar_lc != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_lc);
	  insn = emit_move_insn (alt_reg, reg);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_prologue_use (alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  if (current_frame_info.reg_save_gp)
    {
      insn = emit_move_insn (gen_rtx_REG (DImode,
					  current_frame_info.reg_save_gp),
			     pic_offset_table_rtx);
      /* We don't know for sure yet if this is actually needed, since
	 we've not split the PIC call patterns.  If all of the calls
	 are indirect, and not followed by any uses of the gp, then
	 this save is dead.  Allow it to go away.  */
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx, REG_NOTES (insn));
    }

  /* We should now be at the base of the gr/br/fr spill area.  */
  if (cfa_off != (current_frame_info.spill_cfa_off
		  + current_frame_info.spill_size))
    abort ();

  /* Spill all general registers.  */
  for (regno = GR_REG (1); regno <= GR_REG (31); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	reg = gen_rtx_REG (DImode, regno);
	do_spill (gen_gr_spill, reg, cfa_off, reg);
	cfa_off -= 8;
      }

  /* Handle BR0 specially -- it may be getting stored permanently in
     some GR register.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, BR_REG (0)))
    {
      reg = gen_rtx_REG (DImode, BR_REG (0));
      if (current_frame_info.reg_save_b0 != 0)
	{
	  alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_b0);
	  insn = emit_move_insn (alt_reg, reg);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* Even if we're not going to generate an epilogue, we still
	     need to save the register so that EH works.  */
	  if (! epilogue_p)
	    emit_insn (gen_prologue_use (alt_reg));
	}
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  emit_move_insn (alt_reg, reg);
	  do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	  cfa_off -= 8;
	}
    }

  /* Spill the rest of the BR registers.  */
  for (regno = BR_REG (1); regno <= BR_REG (7); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	alt_regno = next_scratch_gr_reg ();
	alt_reg = gen_rtx_REG (DImode, alt_regno);
	reg = gen_rtx_REG (DImode, regno);
	emit_move_insn (alt_reg, reg);
	do_spill (gen_movdi_x, alt_reg, cfa_off, reg);
	cfa_off -= 8;
      }

  /* Align the frame and spill all FR registers.  */
  for (regno = FR_REG (2); regno <= FR_REG (127); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
        if (cfa_off & 15)
	  abort ();
	reg = gen_rtx_REG (TFmode, regno);
	do_spill (gen_fr_spill_x, reg, cfa_off, reg);
	cfa_off -= 16;
      }

  if (cfa_off != current_frame_info.spill_cfa_off)
    abort ();

  finish_spill_pointers ();
}

/* Called after register allocation to add any instructions needed for the
   epilogue.  Using an epilogue insn is favored compared to putting all of the
   instructions in output_function_prologue(), since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.  */

void
ia64_expand_epilogue (sibcall_p)
     int sibcall_p;
{
  rtx insn, reg, alt_reg, ar_unat_save_reg;
  int regno, alt_regno, cfa_off;

  ia64_compute_frame_size (get_frame_size ());

  /* If there is a frame pointer, then we use it instead of the stack
     pointer, so that the stack pointer does not need to be valid when
     the epilogue starts.  See EXIT_IGNORE_STACK.  */
  if (frame_pointer_needed)
    setup_spill_pointers (current_frame_info.n_spilled,
			  hard_frame_pointer_rtx, 0);
  else
    setup_spill_pointers (current_frame_info.n_spilled, stack_pointer_rtx, 
			  current_frame_info.total_size);

  if (current_frame_info.total_size != 0)
    {
      /* ??? At this point we must generate a magic insn that appears to
         modify the spill iterators and the frame pointer.  This would
	 allow the most scheduling freedom.  For now, just hard stop.  */
      emit_insn (gen_blockage ());
    }

  /* Locate the bottom of the register save area.  */
  cfa_off = (current_frame_info.spill_cfa_off
	     + current_frame_info.spill_size
	     + current_frame_info.extra_spill_size);

  /* Restore the predicate registers.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, PR_REG (0)))
    {
      if (current_frame_info.reg_save_pr != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_pr);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, PR_REG (0));
      emit_move_insn (reg, alt_reg);
    }

  /* Restore the application registers.  */

  /* Load the saved unat from the stack, but do not restore it until
     after the GRs have been restored.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      if (current_frame_info.reg_save_ar_unat != 0)
        ar_unat_save_reg
	  = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_unat);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  ar_unat_save_reg = gen_rtx_REG (DImode, alt_regno);
	  current_frame_info.gr_used_mask |= 1 << alt_regno;
	  do_restore (gen_movdi_x, ar_unat_save_reg, cfa_off);
	  cfa_off -= 8;
	}
    }
  else
    ar_unat_save_reg = NULL_RTX;
      
  if (current_frame_info.reg_save_ar_pfs != 0)
    {
      alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_pfs);
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      emit_move_insn (reg, alt_reg);
    }
  else if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_PFS_REGNUM))
    {
      alt_regno = next_scratch_gr_reg ();
      alt_reg = gen_rtx_REG (DImode, alt_regno);
      do_restore (gen_movdi_x, alt_reg, cfa_off);
      cfa_off -= 8;
      reg = gen_rtx_REG (DImode, AR_PFS_REGNUM);
      emit_move_insn (reg, alt_reg);
    }

  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_LC_REGNUM))
    {
      if (current_frame_info.reg_save_ar_lc != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_ar_lc);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, AR_LC_REGNUM);
      emit_move_insn (reg, alt_reg);
    }

  /* We should now be at the base of the gr/br/fr spill area.  */
  if (cfa_off != (current_frame_info.spill_cfa_off
		  + current_frame_info.spill_size))
    abort ();

  /* The GP may be stored on the stack in the prologue, but it's
     never restored in the epilogue.  Skip the stack slot.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, GR_REG (1)))
    cfa_off -= 8;

  /* Restore all general registers.  */
  for (regno = GR_REG (2); regno <= GR_REG (31); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	reg = gen_rtx_REG (DImode, regno);
	do_restore (gen_gr_restore, reg, cfa_off);
	cfa_off -= 8;
      }
  
  /* Restore the branch registers.  Handle B0 specially, as it may
     have gotten stored in some GR register.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, BR_REG (0)))
    {
      if (current_frame_info.reg_save_b0 != 0)
	alt_reg = gen_rtx_REG (DImode, current_frame_info.reg_save_b0);
      else
	{
	  alt_regno = next_scratch_gr_reg ();
	  alt_reg = gen_rtx_REG (DImode, alt_regno);
	  do_restore (gen_movdi_x, alt_reg, cfa_off);
	  cfa_off -= 8;
	}
      reg = gen_rtx_REG (DImode, BR_REG (0));
      emit_move_insn (reg, alt_reg);
    }
    
  for (regno = BR_REG (1); regno <= BR_REG (7); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
	alt_regno = next_scratch_gr_reg ();
	alt_reg = gen_rtx_REG (DImode, alt_regno);
	do_restore (gen_movdi_x, alt_reg, cfa_off);
	cfa_off -= 8;
	reg = gen_rtx_REG (DImode, regno);
	emit_move_insn (reg, alt_reg);
      }

  /* Restore floating point registers.  */
  for (regno = FR_REG (2); regno <= FR_REG (127); ++regno)
    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
      {
        if (cfa_off & 15)
	  abort ();
	reg = gen_rtx_REG (TFmode, regno);
	do_restore (gen_fr_restore_x, reg, cfa_off);
	cfa_off -= 16;
      }

  /* Restore ar.unat for real.  */
  if (TEST_HARD_REG_BIT (current_frame_info.mask, AR_UNAT_REGNUM))
    {
      reg = gen_rtx_REG (DImode, AR_UNAT_REGNUM);
      emit_move_insn (reg, ar_unat_save_reg);
    }

  if (cfa_off != current_frame_info.spill_cfa_off)
    abort ();

  finish_spill_pointers ();

  if (current_frame_info.total_size || cfun->machine->ia64_eh_epilogue_sp)
    {
      /* ??? At this point we must generate a magic insn that appears to
         modify the spill iterators, the stack pointer, and the frame
	 pointer.  This would allow the most scheduling freedom.  For now,
	 just hard stop.  */
      emit_insn (gen_blockage ());
    }

  if (cfun->machine->ia64_eh_epilogue_sp)
    emit_move_insn (stack_pointer_rtx, cfun->machine->ia64_eh_epilogue_sp);
  else if (frame_pointer_needed)
    {
      insn = emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (current_frame_info.total_size)
    {
      rtx offset, frame_size_rtx;

      frame_size_rtx = GEN_INT (current_frame_info.total_size);
      if (CONST_OK_FOR_I (current_frame_info.total_size))
	offset = frame_size_rtx;
      else
	{
	  regno = next_scratch_gr_reg ();
	  offset = gen_rtx_REG (DImode, regno);
	  emit_move_insn (offset, frame_size_rtx);
	}

      insn = emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				    offset));

      RTX_FRAME_RELATED_P (insn) = 1;
      if (GET_CODE (offset) != CONST_INT)
	{
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     gen_rtx_PLUS (DImode,
						   stack_pointer_rtx,
						   frame_size_rtx)),
			REG_NOTES (insn));
	}
    }

  if (cfun->machine->ia64_eh_epilogue_bsp)
    emit_insn (gen_set_bsp (cfun->machine->ia64_eh_epilogue_bsp));
 
  if (! sibcall_p)
    emit_jump_insn (gen_return_internal (gen_rtx_REG (DImode, BR_REG (0))));
  else
    {
      int fp = GR_REG (2);
      /* We need a throw away register here, r0 and r1 are reserved, so r2 is the
	 first available call clobbered register.  If there was a frame_pointer 
	 register, we may have swapped the names of r2 and HARD_FRAME_POINTER_REGNUM, 
	 so we have to make sure we're using the string "r2" when emitting
	 the register name for the assmbler.  */
      if (current_frame_info.reg_fp && current_frame_info.reg_fp == GR_REG (2))
	fp = HARD_FRAME_POINTER_REGNUM;

      /* We must emit an alloc to force the input registers to become output
	 registers.  Otherwise, if the callee tries to pass its parameters
	 through to another call without an intervening alloc, then these
	 values get lost.  */
      /* ??? We don't need to preserve all input registers.  We only need to
	 preserve those input registers used as arguments to the sibling call.
	 It is unclear how to compute that number here.  */
      if (current_frame_info.n_input_regs != 0)
	{
	  rtx n_inputs = GEN_INT (current_frame_info.n_input_regs);
	  insn = emit_insn (gen_alloc (gen_rtx_REG (DImode, fp),
				const0_rtx, const0_rtx,
				n_inputs, const0_rtx));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Return 1 if br.ret can do all the work required to return from a
   function.  */

int
ia64_direct_return ()
{
  if (reload_completed && ! frame_pointer_needed)
    {
      ia64_compute_frame_size (get_frame_size ());

      return (current_frame_info.total_size == 0
	      && current_frame_info.n_spilled == 0
	      && current_frame_info.reg_save_b0 == 0
	      && current_frame_info.reg_save_pr == 0
	      && current_frame_info.reg_save_ar_pfs == 0
	      && current_frame_info.reg_save_ar_unat == 0
	      && current_frame_info.reg_save_ar_lc == 0);
    }
  return 0;
}

/* Return the magic cookie that we use to hold the return address
   during early compilation.  */

rtx
ia64_return_addr_rtx (count, frame)
     HOST_WIDE_INT count;
     rtx frame ATTRIBUTE_UNUSED;
{
  if (count != 0)
    return NULL;
  return gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_RET_ADDR);
}

/* Split this value after reload, now that we know where the return
   address is saved.  */

void
ia64_split_return_addr_rtx (dest)
     rtx dest;
{
  rtx src;

  if (TEST_HARD_REG_BIT (current_frame_info.mask, BR_REG (0)))
    {
      if (current_frame_info.reg_save_b0 != 0)
	src = gen_rtx_REG (DImode, current_frame_info.reg_save_b0);
      else
	{
	  HOST_WIDE_INT off;
	  unsigned int regno;

	  /* Compute offset from CFA for BR0.  */
	  /* ??? Must be kept in sync with ia64_expand_prologue.  */
	  off = (current_frame_info.spill_cfa_off
		 + current_frame_info.spill_size);
	  for (regno = GR_REG (1); regno <= GR_REG (31); ++regno)
	    if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
	      off -= 8;

	  /* Convert CFA offset to a register based offset.  */
	  if (frame_pointer_needed)
	    src = hard_frame_pointer_rtx;
	  else
	    {
	      src = stack_pointer_rtx;
	      off += current_frame_info.total_size;
	    }

	  /* Load address into scratch register.  */
	  if (CONST_OK_FOR_I (off))
	    emit_insn (gen_adddi3 (dest, src, GEN_INT (off)));
	  else
	    {
	      emit_move_insn (dest, GEN_INT (off));
	      emit_insn (gen_adddi3 (dest, src, dest));
	    }

	  src = gen_rtx_MEM (Pmode, dest);
	}
    }
  else
    src = gen_rtx_REG (DImode, BR_REG (0));

  emit_move_insn (dest, src);
}

int
ia64_hard_regno_rename_ok (from, to)
     int from;
     int to;
{
  /* Don't clobber any of the registers we reserved for the prologue.  */
  if (to == current_frame_info.reg_fp
      || to == current_frame_info.reg_save_b0
      || to == current_frame_info.reg_save_pr
      || to == current_frame_info.reg_save_ar_pfs
      || to == current_frame_info.reg_save_ar_unat
      || to == current_frame_info.reg_save_ar_lc)
    return 0;

  if (from == current_frame_info.reg_fp
      || from == current_frame_info.reg_save_b0
      || from == current_frame_info.reg_save_pr
      || from == current_frame_info.reg_save_ar_pfs
      || from == current_frame_info.reg_save_ar_unat
      || from == current_frame_info.reg_save_ar_lc)
    return 0;

  /* Don't use output registers outside the register frame.  */
  if (OUT_REGNO_P (to) && to >= OUT_REG (current_frame_info.n_output_regs))
    return 0;

  /* Retain even/oddness on predicate register pairs.  */
  if (PR_REGNO_P (from) && PR_REGNO_P (to))
    return (from & 1) == (to & 1);

  return 1;
}

/* Target hook for assembling integer objects.  Handle word-sized
   aligned objects and detect the cases when @fptr is needed.  */

static bool
ia64_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (size == (TARGET_ILP32 ? 4 : 8)
      && !(TARGET_NO_PIC || TARGET_AUTO_PIC)
      && GET_CODE (x) == SYMBOL_REF
      && SYMBOL_REF_FLAG (x))
    {
      static const char * const directive[2][2] = {
	  /* 64-bit pointer */  /* 32-bit pointer */
	{ "\tdata8.ua\t@fptr(", "\tdata4.ua\t@fptr("},	/* unaligned */
	{ "\tdata8\t@fptr(",    "\tdata4\t@fptr("}	/* aligned */
      };
      fputs (directive[aligned_p != 0][TARGET_ILP32 != 0], asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Emit the function prologue.  */

static void
ia64_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int mask, grsave, grsave_prev;

  if (current_frame_info.need_regstk)
    fprintf (file, "\t.regstk %d, %d, %d, %d\n",
	     current_frame_info.n_input_regs,
	     current_frame_info.n_local_regs,
	     current_frame_info.n_output_regs,
	     current_frame_info.n_rotate_regs);

  if (!flag_unwind_tables && (!flag_exceptions || USING_SJLJ_EXCEPTIONS))
    return;

  /* Emit the .prologue directive.  */

  mask = 0;
  grsave = grsave_prev = 0;
  if (current_frame_info.reg_save_b0 != 0)
    {
      mask |= 8;
      grsave = grsave_prev = current_frame_info.reg_save_b0;
    }
  if (current_frame_info.reg_save_ar_pfs != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_save_ar_pfs == grsave_prev + 1))
    {
      mask |= 4;
      if (grsave_prev == 0)
	grsave = current_frame_info.reg_save_ar_pfs;
      grsave_prev = current_frame_info.reg_save_ar_pfs;
    }
  if (current_frame_info.reg_fp != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_fp == grsave_prev + 1))
    {
      mask |= 2;
      if (grsave_prev == 0)
	grsave = HARD_FRAME_POINTER_REGNUM;
      grsave_prev = current_frame_info.reg_fp;
    }
  if (current_frame_info.reg_save_pr != 0
      && (grsave_prev == 0
	  || current_frame_info.reg_save_pr == grsave_prev + 1))
    {
      mask |= 1;
      if (grsave_prev == 0)
	grsave = current_frame_info.reg_save_pr;
    }

  if (mask)
    fprintf (file, "\t.prologue %d, %d\n", mask,
	     ia64_dbx_register_number (grsave));
  else
    fputs ("\t.prologue\n", file);

  /* Emit a .spill directive, if necessary, to relocate the base of
     the register spill area.  */
  if (current_frame_info.spill_cfa_off != -16)
    fprintf (file, "\t.spill %ld\n",
	     (long) (current_frame_info.spill_cfa_off
		     + current_frame_info.spill_size));
}

/* Emit the .body directive at the scheduled end of the prologue.  */

static void
ia64_output_function_end_prologue (file)
     FILE *file;
{
  if (!flag_unwind_tables && (!flag_exceptions || USING_SJLJ_EXCEPTIONS))
    return;

  fputs ("\t.body\n", file);
}

/* Emit the function epilogue.  */

static void
ia64_output_function_epilogue (file, size)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int i;

  if (current_frame_info.reg_fp)
    {
      const char *tmp = reg_names[HARD_FRAME_POINTER_REGNUM];
      reg_names[HARD_FRAME_POINTER_REGNUM]
	= reg_names[current_frame_info.reg_fp];
      reg_names[current_frame_info.reg_fp] = tmp;
    }
  if (! TARGET_REG_NAMES)
    {
      for (i = 0; i < current_frame_info.n_input_regs; i++)
	reg_names[IN_REG (i)] = ia64_input_reg_names[i];
      for (i = 0; i < current_frame_info.n_local_regs; i++)
	reg_names[LOC_REG (i)] = ia64_local_reg_names[i];
      for (i = 0; i < current_frame_info.n_output_regs; i++)
	reg_names[OUT_REG (i)] = ia64_output_reg_names[i];
    }

  current_frame_info.initialized = 0;
}

int
ia64_dbx_register_number (regno)
     int regno;
{
  /* In ia64_expand_prologue we quite literally renamed the frame pointer
     from its home at loc79 to something inside the register frame.  We
     must perform the same renumbering here for the debug info.  */
  if (current_frame_info.reg_fp)
    {
      if (regno == HARD_FRAME_POINTER_REGNUM)
	regno = current_frame_info.reg_fp;
      else if (regno == current_frame_info.reg_fp)
	regno = HARD_FRAME_POINTER_REGNUM;
    }

  if (IN_REGNO_P (regno))
    return 32 + regno - IN_REG (0);
  else if (LOC_REGNO_P (regno))
    return 32 + current_frame_info.n_input_regs + regno - LOC_REG (0);
  else if (OUT_REGNO_P (regno))
    return (32 + current_frame_info.n_input_regs
	    + current_frame_info.n_local_regs + regno - OUT_REG (0));
  else
    return regno;
}

void
ia64_initialize_trampoline (addr, fnaddr, static_chain)
     rtx addr, fnaddr, static_chain;
{
  rtx addr_reg, eight = GEN_INT (8);

  /* Load up our iterator.  */
  addr_reg = gen_reg_rtx (Pmode);
  emit_move_insn (addr_reg, addr);

  /* The first two words are the fake descriptor:
     __ia64_trampoline, ADDR+16.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg),
		  gen_rtx_SYMBOL_REF (Pmode, "__ia64_trampoline"));
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg),
		  copy_to_reg (plus_constant (addr, 16)));
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  /* The third word is the target descriptor.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg), fnaddr);
  emit_insn (gen_adddi3 (addr_reg, addr_reg, eight));

  /* The fourth word is the static chain.  */
  emit_move_insn (gen_rtx_MEM (Pmode, addr_reg), static_chain);
}

/* Do any needed setup for a variadic function.  CUM has not been updated
   for the last named argument which has type TYPE and mode MODE.

   We generate the actual spill instructions during prologue generation.  */

void
ia64_setup_incoming_varargs (cum, int_mode, type, pretend_size, second_time)
     CUMULATIVE_ARGS cum;
     int             int_mode;
     tree            type;
     int *           pretend_size;
     int	     second_time ATTRIBUTE_UNUSED;
{
  /* Skip the current argument.  */
  ia64_function_arg_advance (&cum, int_mode, type, 1);

  if (cum.words < MAX_ARGUMENT_SLOTS)
    {
      int n = MAX_ARGUMENT_SLOTS - cum.words;
      *pretend_size = n * UNITS_PER_WORD;
      cfun->machine->n_varargs = n;
    }
}

/* Check whether TYPE is a homogeneous floating point aggregate.  If
   it is, return the mode of the floating point type that appears
   in all leafs.  If it is not, return VOIDmode.

   An aggregate is a homogeneous floating point aggregate is if all
   fields/elements in it have the same floating point type (e.g,
   SFmode).  128-bit quad-precision floats are excluded.  */

static enum machine_mode
hfa_element_mode (type, nested)
     tree type;
     int nested;
{
  enum machine_mode element_mode = VOIDmode;
  enum machine_mode mode;
  enum tree_code code = TREE_CODE (type);
  int know_element_mode = 0;
  tree t;

  switch (code)
    {
    case VOID_TYPE:	case INTEGER_TYPE:	case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:	case CHAR_TYPE:		case POINTER_TYPE:
    case OFFSET_TYPE:	case REFERENCE_TYPE:	case METHOD_TYPE:
    case FILE_TYPE:	case SET_TYPE:		case LANG_TYPE:
    case FUNCTION_TYPE:
      return VOIDmode;

      /* Fortran complex types are supposed to be HFAs, so we need to handle
	 gcc's COMPLEX_TYPEs as HFAs.  We need to exclude the integral complex
	 types though.  */
    case COMPLEX_TYPE:
      if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_FLOAT
	  && (TYPE_MODE (type) != TCmode || INTEL_EXTENDED_IEEE_FORMAT))
	return mode_for_size (GET_MODE_UNIT_SIZE (TYPE_MODE (type))
			      * BITS_PER_UNIT, MODE_FLOAT, 0);
      else
	return VOIDmode;

    case REAL_TYPE:
      /* We want to return VOIDmode for raw REAL_TYPEs, but the actual
	 mode if this is contained within an aggregate.  */
      if (nested && (TYPE_MODE (type) != TFmode || INTEL_EXTENDED_IEEE_FORMAT))
	return TYPE_MODE (type);
      else
	return VOIDmode;

    case ARRAY_TYPE:
      return hfa_element_mode (TREE_TYPE (type), 1);

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
	{
	  if (TREE_CODE (t) != FIELD_DECL)
	    continue;

	  mode = hfa_element_mode (TREE_TYPE (t), 1);
	  if (know_element_mode)
	    {
	      if (mode != element_mode)
		return VOIDmode;
	    }
	  else if (GET_MODE_CLASS (mode) != MODE_FLOAT)
	    return VOIDmode;
	  else
	    {
	      know_element_mode = 1;
	      element_mode = mode;
	    }
	}
      return element_mode;

    default:
      /* If we reach here, we probably have some front-end specific type
	 that the backend doesn't know about.  This can happen via the
	 aggregate_value_p call in init_function_start.  All we can do is
	 ignore unknown tree types.  */
      return VOIDmode;
    }

  return VOIDmode;
}

/* Return rtx for register where argument is passed, or zero if it is passed
   on the stack.  */

/* ??? 128-bit quad-precision floats are always passed in general
   registers.  */

rtx
ia64_function_arg (cum, mode, type, named, incoming)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming;
{
  int basereg = (incoming ? GR_ARG_FIRST : AR_ARG_FIRST);
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* Integer and float arguments larger than 8 bytes start at the next even
     boundary.  Aggregates larger than 8 bytes start at the next even boundary
     if the aggregate has 16 byte alignment.  Net effect is that types with
     alignment greater than 8 start at the next even boundary.  */
  /* ??? The ABI does not specify how to handle aggregates with alignment from
     9 to 15 bytes, or greater than 16.   We handle them all as if they had
     16 byte alignment.  Such aggregates can occur only if gcc extensions are
     used.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      rtx loc[16];
      int i = 0;
      int fp_regs = cum->fp_regs;
      int int_regs = cum->words + offset;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD)); i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, (FR_ARG_FIRST
							      + fp_regs)),
				      GEN_INT (offset));
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      /* If no prototype, then the whole thing must go in GR regs.  */
      if (! cum->prototype)
	offset = 0;
      /* If this is an SFmode aggregate, then we might have some left over
	 that needs to go in GR regs.  */
      else if (byte_size != offset)
	int_regs += offset / UNITS_PER_WORD;

      /* Fill in the GR regs.  We must use DImode here, not the hfa mode.  */

      for (; offset < byte_size && int_regs < MAX_ARGUMENT_SLOTS; i++)
	{
	  enum machine_mode gr_mode = DImode;

	  /* If we have an odd 4 byte hunk because we ran out of FR regs,
	     then this goes in a GR reg left adjusted/little endian, right
	     adjusted/big endian.  */
	  /* ??? Currently this is handled wrong, because 4-byte hunks are
	     always right adjusted/little endian.  */
	  if (offset & 0x4)
	    gr_mode = SImode;
	  /* If we have an even 4 byte hunk because the aggregate is a
	     multiple of 4 bytes in size, then this goes in a GR reg right
	     adjusted/little endian.  */
	  else if (byte_size - offset == 4)
	    gr_mode = SImode;
	  /* Complex floats need to have float mode.  */
	  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	    gr_mode = hfa_mode;

	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (gr_mode, (basereg
							     + int_regs)),
				      GEN_INT (offset));
	  offset += GET_MODE_SIZE (gr_mode);
	  int_regs += GET_MODE_SIZE (gr_mode) <= UNITS_PER_WORD
		      ? 1 : GET_MODE_SIZE (gr_mode) / UNITS_PER_WORD;
	}

      /* If we ended up using just one location, just return that one loc, but
	 change the mode back to the argument mode.  */
      if (i == 1)
	return gen_rtx_REG (mode, REGNO (XEXP (loc[0], 0)));
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (((mode == TFmode) && ! INTEL_EXTENDED_IEEE_FORMAT)
          || (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS))
    {
      int byte_size = ((mode == BLKmode)
                       ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      if (BYTES_BIG_ENDIAN
	&& (mode == BLKmode || (type && AGGREGATE_TYPE_P (type)))
	&& byte_size < UNITS_PER_WORD
	&& byte_size > 0)
	{
	  rtx gr_reg = gen_rtx_EXPR_LIST (VOIDmode,
					  gen_rtx_REG (DImode,
						       (basereg + cum->words
							+ offset)),
					  const0_rtx);
	  return gen_rtx_PARALLEL (mode, gen_rtvec (1, gr_reg));
	}
      else
	return gen_rtx_REG (mode, basereg + cum->words + offset);

    }

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	return gen_rtx_REG (mode, basereg + cum->words + offset);
      else
	return gen_rtx_REG (mode, FR_ARG_FIRST + cum->fp_regs);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    {
      rtx fp_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode, (FR_ARG_FIRST
							  + cum->fp_regs)),
				      const0_rtx);
      rtx gr_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode,
						   (basereg + cum->words
						    + offset)),
				      const0_rtx);

      return gen_rtx_PARALLEL (mode, gen_rtvec (2, fp_reg, gr_reg));
    }
}

/* Return number of words, at the beginning of the argument, that must be
   put in registers.  0 is the argument is entirely in registers or entirely
   in memory.  */

int
ia64_function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* It doesn't matter whether the argument goes in FR or GR regs.  If
     it fits within the 8 argument slots, then it goes entirely in
     registers.  If it extends past the last argument slot, then the rest
     goes on the stack.  */

  if (words + cum->words + offset <= MAX_ARGUMENT_SLOTS)
    return 0;

  return MAX_ARGUMENT_SLOTS - cum->words - offset;
}

/* Update CUM to point after this argument.  This is patterned after
   ia64_function_arg.  */

void
ia64_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* If all arg slots are already full, then there is nothing to do.  */
  if (cum->words >= MAX_ARGUMENT_SLOTS)
    return;

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if ((type ? (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
       : (words > 1))
      && (cum->words & 1))
    offset = 1;

  cum->words += words + offset;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      int fp_regs = cum->fp_regs;
      /* This is the original value of cum->words + offset.  */
      int int_regs = cum->words - words;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD));)
	{
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      cum->fp_regs = fp_regs;
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS)
    cum->int_regs = cum->words;

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	cum->int_regs = cum->words;
      else
	/* ??? Complex types should not reach here.  */
	cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    { 
      /* ??? Complex types should not reach here.  */
      cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);
      cum->int_regs = cum->words;
    }
}

/* Variable sized types are passed by reference.  */
/* ??? At present this is a GCC extension to the IA-64 ABI.  */

int
ia64_function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  return type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST;
}


/* Implement va_arg.  */

rtx
ia64_va_arg (valist, type)
     tree valist, type;
{
  tree t;

  /* Variable sized types are passed by reference.  */
  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    {
      rtx addr = force_reg (ptr_mode,
	    std_expand_builtin_va_arg (valist, build_pointer_type (type)));
#ifdef POINTERS_EXTEND_UNSIGNED
      addr = convert_memory_address (Pmode, addr);
#endif
      return gen_rtx_MEM (ptr_mode, addr);
    }

  /* Arguments with alignment larger than 8 bytes start at the next even
     boundary.  */
  if (TYPE_ALIGN (type) > 8 * BITS_PER_UNIT)
    {
      t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
		 build_int_2 (2 * UNITS_PER_WORD - 1, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t,
		 build_int_2 (-2 * UNITS_PER_WORD, -1));
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return std_expand_builtin_va_arg (valist, type);
}

/* Return 1 if function return value returned in memory.  Return 0 if it is
   in a register.  */

int
ia64_return_in_memory (valtype)
     tree valtype;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;
  HOST_WIDE_INT byte_size;

  mode = TYPE_MODE (valtype);
  byte_size = GET_MODE_SIZE (mode);
  if (mode == BLKmode)
    {
      byte_size = int_size_in_bytes (valtype);
      if (byte_size < 0)
	return 1;
    }

  /* Hfa's with up to 8 elements are returned in the FP argument registers.  */

  hfa_mode = hfa_element_mode (valtype, 0);
  if (hfa_mode != VOIDmode)
    {
      int hfa_size = GET_MODE_SIZE (hfa_mode);

      if (byte_size / hfa_size > MAX_ARGUMENT_SLOTS)
	return 1;
      else
	return 0;
    }
  else if (byte_size > UNITS_PER_WORD * MAX_INT_RETURN_SLOTS)
    return 1;
  else
    return 0;
}

/* Return rtx for register that holds the function return value.  */

rtx
ia64_function_value (valtype, func)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;

  mode = TYPE_MODE (valtype);
  hfa_mode = hfa_element_mode (valtype, 0);

  if (hfa_mode != VOIDmode)
    {
      rtx loc[8];
      int i;
      int hfa_size;
      int byte_size;
      int offset;

      hfa_size = GET_MODE_SIZE (hfa_mode);
      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (valtype) : GET_MODE_SIZE (mode));
      offset = 0;
      for (i = 0; offset < byte_size; i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, FR_ARG_FIRST + i),
				      GEN_INT (offset));
	  offset += hfa_size;
	}

      if (i == 1)
	return XEXP (loc[0], 0);
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }
  else if (FLOAT_TYPE_P (valtype) &&
           ((mode != TFmode) || INTEL_EXTENDED_IEEE_FORMAT))
    return gen_rtx_REG (mode, FR_ARG_FIRST);
  else
    {
      if (BYTES_BIG_ENDIAN
	  && (mode == BLKmode || (valtype && AGGREGATE_TYPE_P (valtype))))
	{
	  rtx loc[8];
	  int offset;
	  int bytesize;
	  int i;

	  offset = 0;
	  bytesize = int_size_in_bytes (valtype);
	  for (i = 0; offset < bytesize; i++)
	    {
	      loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
					  gen_rtx_REG (DImode,
						       GR_RET_FIRST + i),
					  GEN_INT (offset));
	      offset += UNITS_PER_WORD;
	    }
	  return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
	}
      else
	return gen_rtx_REG (mode, GR_RET_FIRST);
    }
}

/* Print a memory address as an operand to reference that memory location.  */

/* ??? Do we need this?  It gets used only for 'a' operands.  We could perhaps
   also call this from ia64_print_operand for memory addresses.  */

void
ia64_print_operand_address (stream, address)
     FILE * stream ATTRIBUTE_UNUSED;
     rtx    address ATTRIBUTE_UNUSED;
{
}

/* Print an operand to an assembler instruction.
   C	Swap and print a comparison operator.
   D	Print an FP comparison operator.
   E    Print 32 - constant, for SImode shifts as extract.
   e    Print 64 - constant, for DImode rotates.
   F	A floating point constant 0.0 emitted as f0, or 1.0 emitted as f1, or
        a floating point register emitted normally.
   I	Invert a predicate register by adding 1.
   J    Select the proper predicate register for a condition.
   j    Select the inverse predicate register for a condition.
   O	Append .acq for volatile load.
   P	Postincrement of a MEM.
   Q	Append .rel for volatile store.
   S	Shift amount for shladd instruction.
   T	Print an 8-bit sign extended number (K) as a 32-bit unsigned number
	for Intel assembler.
   U	Print an 8-bit sign extended number (K) as a 64-bit unsigned number
	for Intel assembler.
   r	Print register name, or constant 0 as r0.  HP compatibility for
	Linux kernel.  */
void
ia64_print_operand (file, x, code)
     FILE * file;
     rtx    x;
     int    code;
{
  const char *str;

  switch (code)
    {
    case 0:
      /* Handled below.  */
      break;

    case 'C':
      {
	enum rtx_code c = swap_condition (GET_CODE (x));
	fputs (GET_RTX_NAME (c), file);
	return;
      }

    case 'D':
      switch (GET_CODE (x))
	{
	case NE:
	  str = "neq";
	  break;
	case UNORDERED:
	  str = "unord";
	  break;
	case ORDERED:
	  str = "ord";
	  break;
	default:
	  str = GET_RTX_NAME (GET_CODE (x));
	  break;
	}
      fputs (str, file);
      return;

    case 'E':
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, 32 - INTVAL (x));
      return;

    case 'e':
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, 64 - INTVAL (x));
      return;

    case 'F':
      if (x == CONST0_RTX (GET_MODE (x)))
	str = reg_names [FR_REG (0)];
      else if (x == CONST1_RTX (GET_MODE (x)))
	str = reg_names [FR_REG (1)];
      else if (GET_CODE (x) == REG)
	str = reg_names [REGNO (x)];
      else
	abort ();
      fputs (str, file);
      return;

    case 'I':
      fputs (reg_names [REGNO (x) + 1], file);
      return;

    case 'J':
    case 'j':
      {
	unsigned int regno = REGNO (XEXP (x, 0));
	if (GET_CODE (x) == EQ)
	  regno += 1;
	if (code == 'j')
	  regno ^= 1;
        fputs (reg_names [regno], file);
      }
      return;

    case 'O':
      if (MEM_VOLATILE_P (x))
	fputs(".acq", file);
      return;

    case 'P':
      {
	HOST_WIDE_INT value;

	switch (GET_CODE (XEXP (x, 0)))
	  {
	  default:
	    return;

	  case POST_MODIFY:
	    x = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    if (GET_CODE (x) == CONST_INT)
	      value = INTVAL (x);
	    else if (GET_CODE (x) == REG)
	      {
		fprintf (file, ", %s", reg_names[REGNO (x)]);
		return;
	      }
	    else
	      abort ();
	    break;

	  case POST_INC:
	    value = GET_MODE_SIZE (GET_MODE (x));
	    break;

	  case POST_DEC:
	    value = - (HOST_WIDE_INT) GET_MODE_SIZE (GET_MODE (x));
	    break;
	  }

	putc (',', file);
	putc (' ', file);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, value);
	return;
      }

    case 'Q':
      if (MEM_VOLATILE_P (x))
	fputs(".rel", file);
      return;

    case 'S':
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      return;

    case 'T':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "0x%x", (int) INTVAL (x) & 0xffffffff);
	  return;
	}
      break;

    case 'U':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  const char *prefix = "0x";
	  if (INTVAL (x) & 0x80000000)
	    {
	      fprintf (file, "0xffffffff");
	      prefix = "";
	    }
	  fprintf (file, "%s%x", prefix, (int) INTVAL (x) & 0xffffffff);
	  return;
	}
      break;

    case 'r':
      /* If this operand is the constant zero, write it as register zero.
	 Any register, zero, or CONST_INT value is OK here.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)], file);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fputs ("r0", file);
      else if (GET_CODE (x) == CONST_INT)
	output_addr_const (file, x);
      else
	output_operand_lossage ("invalid %%r value");
      return;

    case '+':
      {
	const char *which;
	
	/* For conditional branches, returns or calls, substitute
	   sptk, dptk, dpnt, or spnt for %s.  */
	x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	if (x)
	  {
	    int pred_val = INTVAL (XEXP (x, 0));

	    /* Guess top and bottom 10% statically predicted.  */
	    if (pred_val < REG_BR_PROB_BASE / 50)
	      which = ".spnt";
	    else if (pred_val < REG_BR_PROB_BASE / 2)
	      which = ".dpnt";
	    else if (pred_val < REG_BR_PROB_BASE / 100 * 98)
	      which = ".dptk";
	    else
	      which = ".sptk";
	  }
	else if (GET_CODE (current_output_insn) == CALL_INSN)
	  which = ".sptk";
	else
	  which = ".dptk";

	fputs (which, file);
	return;
      }

    case ',':
      x = current_insn_predicate;
      if (x)
	{
	  unsigned int regno = REGNO (XEXP (x, 0));
	  if (GET_CODE (x) == EQ)
	    regno += 1;
          fprintf (file, "(%s) ", reg_names [regno]);
	}
      return;

    default:
      output_operand_lossage ("ia64_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
      /* This happens for the spill/restore instructions.  */
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      x = XEXP (x, 0);
      /* ... fall through ...  */

    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      {
	rtx addr = XEXP (x, 0);
	if (GET_RTX_CLASS (GET_CODE (addr)) == 'a')
	  addr = XEXP (addr, 0);
	fprintf (file, "[%s]", reg_names [REGNO (addr)]);
	break;
      }

    default:
      output_addr_const (file, x);
      break;
    }

  return;
}

/* Calulate the cost of moving data from a register in class FROM to
   one in class TO, using MODE.  */

int
ia64_register_move_cost (mode, from, to)
     enum machine_mode mode;
     enum reg_class from, to;
{
  /* ADDL_REGS is the same as GR_REGS for movement purposes.  */
  if (to == ADDL_REGS)
    to = GR_REGS;
  if (from == ADDL_REGS)
    from = GR_REGS;

  /* All costs are symmetric, so reduce cases by putting the
     lower number class as the destination.  */
  if (from < to)
    {
      enum reg_class tmp = to;
      to = from, from = tmp;
    }

  /* Moving from FR<->GR in TFmode must be more expensive than 2,
     so that we get secondary memory reloads.  Between FR_REGS,
     we have to make this at least as expensive as MEMORY_MOVE_COST
     to avoid spectacularly poor register class preferencing.  */
  if (mode == TFmode)
    {
      if (to != GR_REGS || from != GR_REGS)
        return MEMORY_MOVE_COST (mode, to, 0);
      else
	return 3;
    }

  switch (to)
    {
    case PR_REGS:
      /* Moving between PR registers takes two insns.  */
      if (from == PR_REGS)
	return 3;
      /* Moving between PR and anything but GR is impossible.  */
      if (from != GR_REGS)
	return MEMORY_MOVE_COST (mode, to, 0);
      break;

    case BR_REGS:
      /* Moving between BR and anything but GR is impossible.  */
      if (from != GR_REGS && from != GR_AND_BR_REGS)
	return MEMORY_MOVE_COST (mode, to, 0);
      break;

    case AR_I_REGS:
    case AR_M_REGS:
      /* Moving between AR and anything but GR is impossible.  */
      if (from != GR_REGS)
	return MEMORY_MOVE_COST (mode, to, 0);
      break;

    case GR_REGS:
    case FR_REGS:
    case GR_AND_FR_REGS:
    case GR_AND_BR_REGS:
    case ALL_REGS:
      break;

    default:
      abort ();
    }

  return 2;
}

/* This function returns the register class required for a secondary
   register when copying between one of the registers in CLASS, and X,
   using MODE.  A return value of NO_REGS means that no secondary register
   is required.  */

enum reg_class
ia64_secondary_reload_class (class, mode, x)
     enum reg_class class;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x;
{
  int regno = -1;

  if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  switch (class)
    {
    case BR_REGS:
    case AR_M_REGS:
    case AR_I_REGS:
      /* ??? BR<->BR register copies can happen due to a bad gcse/cse/global
	 interaction.  We end up with two pseudos with overlapping lifetimes
	 both of which are equiv to the same constant, and both which need
	 to be in BR_REGS.  This seems to be a cse bug.  cse_basic_block_end
	 changes depending on the path length, which means the qty_first_reg
	 check in make_regs_eqv can give different answers at different times.
	 At some point I'll probably need a reload_indi pattern to handle
	 this.

	 We can also get GR_AND_FR_REGS to BR_REGS/AR_REGS copies, where we
	 wound up with a FP register from GR_AND_FR_REGS.  Extend that to all
	 non-general registers for good measure.  */
      if (regno >= 0 && ! GENERAL_REGNO_P (regno))
	return GR_REGS;

      /* This is needed if a pseudo used as a call_operand gets spilled to a
	 stack slot.  */
      if (GET_CODE (x) == MEM)
	return GR_REGS;
      break;

    case FR_REGS:
      /* Need to go through general regsters to get to other class regs.  */
      if (regno >= 0 && ! (FR_REGNO_P (regno) || GENERAL_REGNO_P (regno)))
	return GR_REGS;
 
      /* This can happen when a paradoxical subreg is an operand to the
	 muldi3 pattern.  */
      /* ??? This shouldn't be necessary after instruction scheduling is
	 enabled, because paradoxical subregs are not accepted by
	 register_operand when INSN_SCHEDULING is defined.  Or alternatively,
	 stop the paradoxical subreg stupidity in the *_operand functions
	 in recog.c.  */
      if (GET_CODE (x) == MEM
	  && (GET_MODE (x) == SImode || GET_MODE (x) == HImode
	      || GET_MODE (x) == QImode))
	return GR_REGS;

      /* This can happen because of the ior/and/etc patterns that accept FP
	 registers as operands.  If the third operand is a constant, then it
	 needs to be reloaded into a FP register.  */
      if (GET_CODE (x) == CONST_INT)
	return GR_REGS;

      /* This can happen because of register elimination in a muldi3 insn.
	 E.g. `26107 * (unsigned long)&u'.  */
      if (GET_CODE (x) == PLUS)
	return GR_REGS;
      break;

    case PR_REGS:
      /* ??? This happens if we cse/gcse a BImode value across a call,
	 and the function has a nonlocal goto.  This is because global
	 does not allocate call crossing pseudos to hard registers when
	 current_function_has_nonlocal_goto is true.  This is relatively
	 common for C++ programs that use exceptions.  To reproduce,
	 return NO_REGS and compile libstdc++.  */
      if (GET_CODE (x) == MEM)
	return GR_REGS;

      /* This can happen when we take a BImode subreg of a DImode value,
	 and that DImode value winds up in some non-GR register.  */
      if (regno >= 0 && ! GENERAL_REGNO_P (regno) && ! PR_REGNO_P (regno))
	return GR_REGS;
      break;

    case GR_REGS:
      /* Since we have no offsettable memory addresses, we need a temporary
	 to hold the address of the second word.  */
      if (mode == TImode)
	return GR_REGS;
      break;

    default:
      break;
    }

  return NO_REGS;
}

/* Emit text to declare externally defined variables and functions, because
   the Intel assembler does not support undefined externals.  */

void
ia64_asm_output_external (file, decl, name)
     FILE *file;
     tree decl;
     const char *name;
{
  int save_referenced;

  /* GNU as does not need anything here, but the HP linker does need
     something for external functions.  */

  if (TARGET_GNU_AS
      && (!TARGET_HPUX_LD
	  || TREE_CODE (decl) != FUNCTION_DECL
	  || strstr(name, "__builtin_") == name))
    return;

  /* ??? The Intel assembler creates a reference that needs to be satisfied by
     the linker when we do this, so we need to be careful not to do this for
     builtin functions which have no library equivalent.  Unfortunately, we
     can't tell here whether or not a function will actually be called by
     expand_expr, so we pull in library functions even if we may not need
     them later.  */
  if (! strcmp (name, "__builtin_next_arg")
      || ! strcmp (name, "alloca")
      || ! strcmp (name, "__builtin_constant_p")
      || ! strcmp (name, "__builtin_args_info"))
    return;

  if (TARGET_HPUX_LD)
    ia64_hpux_add_extern_decl (name);
  else
    {
      /* assemble_name will set TREE_SYMBOL_REFERENCED, so we must save and
         restore it.  */
      save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl));
      if (TREE_CODE (decl) == FUNCTION_DECL)
        ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
      (*targetm.asm_out.globalize_label) (file, name);
      TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)) = save_referenced;
    }
}

/* Parse the -mfixed-range= option string.  */

static void
fix_range (const_str)
     const char *const_str;
{
  int i, first, last;
  char *str, *dash, *comma;

  /* str must be of the form REG1'-'REG2{,REG1'-'REG} where REG1 and
     REG2 are either register names or register numbers.  The effect
     of this option is to mark the registers in the range from REG1 to
     REG2 as ``fixed'' so they won't be used by the compiler.  This is
     used, e.g., to ensure that kernel mode code doesn't use f32-f127.  */

  i = strlen (const_str);
  str = (char *) alloca (i + 1);
  memcpy (str, const_str, i + 1);

  while (1)
    {
      dash = strchr (str, '-');
      if (!dash)
	{
	  warning ("value of -mfixed-range must have form REG1-REG2");
	  return;
	}
      *dash = '\0';

      comma = strchr (dash + 1, ',');
      if (comma)
	*comma = '\0';

      first = decode_reg_name (str);
      if (first < 0)
	{
	  warning ("unknown register name: %s", str);
	  return;
	}

      last = decode_reg_name (dash + 1);
      if (last < 0)
	{
	  warning ("unknown register name: %s", dash + 1);
	  return;
	}

      *dash = '-';

      if (first > last)
	{
	  warning ("%s-%s is an empty range", str, dash + 1);
	  return;
	}

      for (i = first; i <= last; ++i)
	fixed_regs[i] = call_used_regs[i] = 1;

      if (!comma)
	break;

      *comma = ',';
      str = comma + 1;
    }
}

static struct machine_function *
ia64_init_machine_status ()
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

/* Handle TARGET_OPTIONS switches.  */

void
ia64_override_options ()
{
  if (TARGET_AUTO_PIC)
    target_flags |= MASK_CONST_GP;

  if (TARGET_INLINE_FLOAT_DIV_LAT && TARGET_INLINE_FLOAT_DIV_THR)
    {
      warning ("cannot optimize floating point division for both latency and throughput");
      target_flags &= ~MASK_INLINE_FLOAT_DIV_THR;
    }

  if (TARGET_INLINE_INT_DIV_LAT && TARGET_INLINE_INT_DIV_THR)
    {
      warning ("cannot optimize integer division for both latency and throughput");
      target_flags &= ~MASK_INLINE_INT_DIV_THR;
    }

  if (ia64_fixed_range_string)
    fix_range (ia64_fixed_range_string);

  if (ia64_tls_size_string)
    {
      char *end;
      unsigned long tmp = strtoul (ia64_tls_size_string, &end, 10);
      if (*end || (tmp != 14 && tmp != 22 && tmp != 64))
	error ("bad value (%s) for -mtls-size= switch", ia64_tls_size_string);
      else
	ia64_tls_size = tmp;
    }

  ia64_flag_schedule_insns2 = flag_schedule_insns_after_reload;
  flag_schedule_insns_after_reload = 0;

  ia64_section_threshold = g_switch_set ? g_switch_value : IA64_DEFAULT_GVALUE;

  init_machine_status = ia64_init_machine_status;

  /* Tell the compiler which flavor of TFmode we're using.  */
  if (INTEL_EXTENDED_IEEE_FORMAT)
    real_format_for_mode[TFmode - QFmode] = &ieee_extended_intel_128_format;
}

static enum attr_itanium_requires_unit0 ia64_safe_itanium_requires_unit0 PARAMS((rtx));
static enum attr_itanium_class ia64_safe_itanium_class PARAMS((rtx));
static enum attr_type ia64_safe_type PARAMS((rtx));

static enum attr_itanium_requires_unit0
ia64_safe_itanium_requires_unit0 (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_itanium_requires_unit0 (insn);
  else
    return ITANIUM_REQUIRES_UNIT0_NO;
}

static enum attr_itanium_class
ia64_safe_itanium_class (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_itanium_class (insn);
  else
    return ITANIUM_CLASS_UNKNOWN;
}

static enum attr_type
ia64_safe_type (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_type (insn);
  else
    return TYPE_UNKNOWN;
}

/* The following collection of routines emit instruction group stop bits as
   necessary to avoid dependencies.  */

/* Need to track some additional registers as far as serialization is
   concerned so we can properly handle br.call and br.ret.  We could
   make these registers visible to gcc, but since these registers are
   never explicitly used in gcc generated code, it seems wasteful to
   do so (plus it would make the call and return patterns needlessly
   complex).  */
#define REG_GP		(GR_REG (1))
#define REG_RP		(BR_REG (0))
#define REG_AR_CFM	(FIRST_PSEUDO_REGISTER + 1)
/* This is used for volatile asms which may require a stop bit immediately
   before and after them.  */
#define REG_VOLATILE	(FIRST_PSEUDO_REGISTER + 2)
#define AR_UNAT_BIT_0	(FIRST_PSEUDO_REGISTER + 3)
#define NUM_REGS	(AR_UNAT_BIT_0 + 64)

/* For each register, we keep track of how it has been written in the
   current instruction group.

   If a register is written unconditionally (no qualifying predicate),
   WRITE_COUNT is set to 2 and FIRST_PRED is ignored.

   If a register is written if its qualifying predicate P is true, we
   set WRITE_COUNT to 1 and FIRST_PRED to P.  Later on, the same register
   may be written again by the complement of P (P^1) and when this happens,
   WRITE_COUNT gets set to 2.

   The result of this is that whenever an insn attempts to write a register
   whose WRITE_COUNT is two, we need to issue an insn group barrier first.

   If a predicate register is written by a floating-point insn, we set
   WRITTEN_BY_FP to true.

   If a predicate register is written by an AND.ORCM we set WRITTEN_BY_AND
   to true; if it was written by an OR.ANDCM we set WRITTEN_BY_OR to true.  */

struct reg_write_state
{
  unsigned int write_count : 2;
  unsigned int first_pred : 16;
  unsigned int written_by_fp : 1;
  unsigned int written_by_and : 1;
  unsigned int written_by_or : 1;
};

/* Cumulative info for the current instruction group.  */
struct reg_write_state rws_sum[NUM_REGS];
/* Info for the current instruction.  This gets copied to rws_sum after a
   stop bit is emitted.  */
struct reg_write_state rws_insn[NUM_REGS];

/* Indicates whether this is the first instruction after a stop bit,
   in which case we don't need another stop bit.  Without this, we hit
   the abort in ia64_variable_issue when scheduling an alloc.  */
static int first_instruction;

/* Misc flags needed to compute RAW/WAW dependencies while we are traversing
   RTL for one instruction.  */
struct reg_flags
{
  unsigned int is_write : 1;	/* Is register being written?  */
  unsigned int is_fp : 1;	/* Is register used as part of an fp op?  */
  unsigned int is_branch : 1;	/* Is register used as part of a branch?  */
  unsigned int is_and : 1;	/* Is register used as part of and.orcm?  */
  unsigned int is_or : 1;	/* Is register used as part of or.andcm?  */
  unsigned int is_sibcall : 1;	/* Is this a sibling or normal call?  */
};

static void rws_update PARAMS ((struct reg_write_state *, int,
				struct reg_flags, int));
static int rws_access_regno PARAMS ((int, struct reg_flags, int));
static int rws_access_reg PARAMS ((rtx, struct reg_flags, int));
static void update_set_flags PARAMS ((rtx, struct reg_flags *, int *, rtx *));
static int set_src_needs_barrier PARAMS ((rtx, struct reg_flags, int, rtx));
static int rtx_needs_barrier PARAMS ((rtx, struct reg_flags, int));
static void init_insn_group_barriers PARAMS ((void));
static int group_barrier_needed_p PARAMS ((rtx));
static int safe_group_barrier_needed_p PARAMS ((rtx));

/* Update *RWS for REGNO, which is being written by the current instruction,
   with predicate PRED, and associated register flags in FLAGS.  */

static void
rws_update (rws, regno, flags, pred)
     struct reg_write_state *rws;
     int regno;
     struct reg_flags flags;
     int pred;
{
  if (pred)
    rws[regno].write_count++;
  else
    rws[regno].write_count = 2;
  rws[regno].written_by_fp |= flags.is_fp;
  /* ??? Not tracking and/or across differing predicates.  */
  rws[regno].written_by_and = flags.is_and;
  rws[regno].written_by_or = flags.is_or;
  rws[regno].first_pred = pred;
}

/* Handle an access to register REGNO of type FLAGS using predicate register
   PRED.  Update rws_insn and rws_sum arrays.  Return 1 if this access creates
   a dependency with an earlier instruction in the same group.  */

static int
rws_access_regno (regno, flags, pred)
     int regno;
     struct reg_flags flags;
     int pred;
{
  int need_barrier = 0;

  if (regno >= NUM_REGS)
    abort ();

  if (! PR_REGNO_P (regno))
    flags.is_and = flags.is_or = 0;

  if (flags.is_write)
    {
      int write_count;

      /* One insn writes same reg multiple times?  */
      if (rws_insn[regno].write_count > 0)
	abort ();

      /* Update info for current instruction.  */
      rws_update (rws_insn, regno, flags, pred);
      write_count = rws_sum[regno].write_count;

      switch (write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  rws_update (rws_sum, regno, flags, pred);
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if (flags.is_and && rws_sum[regno].written_by_and)
	    ; 
	  else if (flags.is_or && rws_sum[regno].written_by_or)
	    ;
	  else if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  rws_update (rws_sum, regno, flags, pred);
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  if (flags.is_and && rws_sum[regno].written_by_and)
	    ;
	  else if (flags.is_or && rws_sum[regno].written_by_or)
	    ;
	  else
	    need_barrier = 1;
	  rws_sum[regno].written_by_and = flags.is_and;
	  rws_sum[regno].written_by_or = flags.is_or;
	  break;

	default:
	  abort ();
	}
    }
  else
    {
      if (flags.is_branch)
	{
	  /* Branches have several RAW exceptions that allow to avoid
	     barriers.  */

	  if (REGNO_REG_CLASS (regno) == BR_REGS || regno == AR_PFS_REGNUM)
	    /* RAW dependencies on branch regs are permissible as long
	       as the writer is a non-branch instruction.  Since we
	       never generate code that uses a branch register written
	       by a branch instruction, handling this case is
	       easy.  */
	    return 0;

	  if (REGNO_REG_CLASS (regno) == PR_REGS
	      && ! rws_sum[regno].written_by_fp)
	    /* The predicates of a branch are available within the
	       same insn group as long as the predicate was written by
	       something other than a floating-point instruction.  */
	    return 0;
	}

      if (flags.is_and && rws_sum[regno].written_by_and)
	return 0;
      if (flags.is_or && rws_sum[regno].written_by_or)
	return 0;

      switch (rws_sum[regno].write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  need_barrier = 1;
	  break;

	default:
	  abort ();
	}
    }

  return need_barrier;
}

static int
rws_access_reg (reg, flags, pred)
     rtx reg;
     struct reg_flags flags;
     int pred;
{
  int regno = REGNO (reg);
  int n = HARD_REGNO_NREGS (REGNO (reg), GET_MODE (reg));

  if (n == 1)
    return rws_access_regno (regno, flags, pred);
  else
    {
      int need_barrier = 0;
      while (--n >= 0)
	need_barrier |= rws_access_regno (regno + n, flags, pred);
      return need_barrier;
    }
}

/* Examine X, which is a SET rtx, and update the flags, the predicate, and
   the condition, stored in *PFLAGS, *PPRED and *PCOND.  */

static void
update_set_flags (x, pflags, ppred, pcond)
     rtx x;
     struct reg_flags *pflags;
     int *ppred;
     rtx *pcond;
{
  rtx src = SET_SRC (x);

  *pcond = 0;

  switch (GET_CODE (src))
    {
    case CALL:
      return;

    case IF_THEN_ELSE:
      if (SET_DEST (x) == pc_rtx)
	/* X is a conditional branch.  */
	return;	
      else
	{
	  int is_complemented = 0;

	  /* X is a conditional move.  */
	  rtx cond = XEXP (src, 0);
	  if (GET_CODE (cond) == EQ)
	    is_complemented = 1;
	  cond = XEXP (cond, 0);
	  if (GET_CODE (cond) != REG
	      && REGNO_REG_CLASS (REGNO (cond)) != PR_REGS)
	    abort ();
	  *pcond = cond;
	  if (XEXP (src, 1) == SET_DEST (x)
	      || XEXP (src, 2) == SET_DEST (x))
	    {
	      /* X is a conditional move that conditionally writes the
		 destination.  */

	      /* We need another complement in this case.  */
	      if (XEXP (src, 1) == SET_DEST (x))
		is_complemented = ! is_complemented;

	      *ppred = REGNO (cond);
	      if (is_complemented)
		++*ppred;
	    }

	  /* ??? If this is a conditional write to the dest, then this
	     instruction does not actually read one source.  This probably
	     doesn't matter, because that source is also the dest.  */
	  /* ??? Multiple writes to predicate registers are allowed
	     if they are all AND type compares, or if they are all OR
	     type compares.  We do not generate such instructions
	     currently.  */
	}
      /* ... fall through ...  */

    default:
      if (GET_RTX_CLASS (GET_CODE (src)) == '<'
	  && GET_MODE_CLASS (GET_MODE (XEXP (src, 0))) == MODE_FLOAT)
	/* Set pflags->is_fp to 1 so that we know we're dealing
	   with a floating point comparison when processing the
	   destination of the SET.  */
	pflags->is_fp = 1;

      /* Discover if this is a parallel comparison.  We only handle
	 and.orcm and or.andcm at present, since we must retain a
	 strict inverse on the predicate pair.  */
      else if (GET_CODE (src) == AND)
	pflags->is_and = 1;
      else if (GET_CODE (src) == IOR)
	pflags->is_or = 1;

      break;
    }
}

/* Subroutine of rtx_needs_barrier; this function determines whether the
   source of a given SET rtx found in X needs a barrier.  FLAGS and PRED
   are as in rtx_needs_barrier.  COND is an rtx that holds the condition
   for this insn.  */
   
static int
set_src_needs_barrier (x, flags, pred, cond)
     rtx x;
     struct reg_flags flags;
     int pred;
     rtx cond;
{
  int need_barrier = 0;
  rtx dst;
  rtx src = SET_SRC (x);

  if (GET_CODE (src) == CALL)
    /* We don't need to worry about the result registers that
       get written by subroutine call.  */
    return rtx_needs_barrier (src, flags, pred);
  else if (SET_DEST (x) == pc_rtx)
    {
      /* X is a conditional branch.  */
      /* ??? This seems redundant, as the caller sets this bit for
	 all JUMP_INSNs.  */
      flags.is_branch = 1;
      return rtx_needs_barrier (src, flags, pred);
    }

  need_barrier = rtx_needs_barrier (src, flags, pred);

  /* This instruction unconditionally uses a predicate register.  */
  if (cond)
    need_barrier |= rws_access_reg (cond, flags, 0);

  dst = SET_DEST (x);
  if (GET_CODE (dst) == ZERO_EXTRACT)
    {
      need_barrier |= rtx_needs_barrier (XEXP (dst, 1), flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (dst, 2), flags, pred);
      dst = XEXP (dst, 0);
    }
  return need_barrier;
}

/* Handle an access to rtx X of type FLAGS using predicate register PRED.
   Return 1 is this access creates a dependency with an earlier instruction
   in the same group.  */

static int
rtx_needs_barrier (x, flags, pred)
     rtx x;
     struct reg_flags flags;
     int pred;
{
  int i, j;
  int is_complemented = 0;
  int need_barrier = 0;
  const char *format_ptr;
  struct reg_flags new_flags;
  rtx cond = 0;

  if (! x)
    return 0;

  new_flags = flags;

  switch (GET_CODE (x))
    {
    case SET:      
      update_set_flags (x, &new_flags, &pred, &cond);
      need_barrier = set_src_needs_barrier (x, new_flags, pred, cond);
      if (GET_CODE (SET_SRC (x)) != CALL)
	{
	  new_flags.is_write = 1;
	  need_barrier |= rtx_needs_barrier (SET_DEST (x), new_flags, pred);
	}
      break;

    case CALL:
      new_flags.is_write = 0;
      need_barrier |= rws_access_regno (AR_EC_REGNUM, new_flags, pred);

      /* Avoid multiple register writes, in case this is a pattern with
	 multiple CALL rtx.  This avoids an abort in rws_access_reg.  */
      if (! flags.is_sibcall && ! rws_insn[REG_AR_CFM].write_count)
	{
	  new_flags.is_write = 1;
	  need_barrier |= rws_access_regno (REG_RP, new_flags, pred);
	  need_barrier |= rws_access_regno (AR_PFS_REGNUM, new_flags, pred);
	  need_barrier |= rws_access_regno (REG_AR_CFM, new_flags, pred);
	}
      break;

    case COND_EXEC:
      /* X is a predicated instruction.  */

      cond = COND_EXEC_TEST (x);
      if (pred)
	abort ();
      need_barrier = rtx_needs_barrier (cond, flags, 0);

      if (GET_CODE (cond) == EQ)
	is_complemented = 1;
      cond = XEXP (cond, 0);
      if (GET_CODE (cond) != REG
	  && REGNO_REG_CLASS (REGNO (cond)) != PR_REGS)
	abort ();
      pred = REGNO (cond);
      if (is_complemented)
	++pred;

      need_barrier |= rtx_needs_barrier (COND_EXEC_CODE (x), flags, pred);
      return need_barrier;

    case CLOBBER:
    case USE:
      /* Clobber & use are for earlier compiler-phases only.  */
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
      /* We always emit stop bits for traditional asms.  We emit stop bits
	 for volatile extended asms if TARGET_VOL_ASM_STOP is true.  */
      if (GET_CODE (x) != ASM_OPERANDS
	  || (MEM_VOLATILE_P (x) && TARGET_VOL_ASM_STOP))
	{
	  /* Avoid writing the register multiple times if we have multiple
	     asm outputs.  This avoids an abort in rws_access_reg.  */
	  if (! rws_insn[REG_VOLATILE].write_count)
	    {
	      new_flags.is_write = 1;
	      rws_access_regno (REG_VOLATILE, new_flags, pred);
	    }
	  return 1;
	}

      /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	 We can not just fall through here since then we would be confused
	 by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	 traditional asms unlike their normal usage.  */

      for (i = ASM_OPERANDS_INPUT_LENGTH (x) - 1; i >= 0; --i)
	if (rtx_needs_barrier (ASM_OPERANDS_INPUT (x, i), flags, pred))
	  need_barrier = 1;
      break;

    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; --i)
	{
	  rtx pat = XVECEXP (x, 0, i);
	  if (GET_CODE (pat) == SET)
	    {
	      update_set_flags (pat, &new_flags, &pred, &cond);
	      need_barrier |= set_src_needs_barrier (pat, new_flags, pred, cond);
	    }
	  else if (GET_CODE (pat) == USE
		   || GET_CODE (pat) == CALL
		   || GET_CODE (pat) == ASM_OPERANDS)
	    need_barrier |= rtx_needs_barrier (pat, flags, pred);
	  else if (GET_CODE (pat) != CLOBBER && GET_CODE (pat) != RETURN)
	    abort ();
	}
      for (i = XVECLEN (x, 0) - 1; i >= 0; --i)
	{
	  rtx pat = XVECEXP (x, 0, i);
	  if (GET_CODE (pat) == SET)
	    {
	      if (GET_CODE (SET_SRC (pat)) != CALL)
		{
		  new_flags.is_write = 1;
		  need_barrier |= rtx_needs_barrier (SET_DEST (pat), new_flags,
						     pred);
		}
	    }
	  else if (GET_CODE (pat) == CLOBBER || GET_CODE (pat) == RETURN)
	    need_barrier |= rtx_needs_barrier (pat, flags, pred);
	}
      break;

    case SUBREG:
      x = SUBREG_REG (x);
      /* FALLTHRU */
    case REG:
      if (REGNO (x) == AR_UNAT_REGNUM)
	{
	  for (i = 0; i < 64; ++i)
	    need_barrier |= rws_access_regno (AR_UNAT_BIT_0 + i, flags, pred);
	}
      else
	need_barrier = rws_access_reg (x, flags, pred);
      break;

    case MEM:
      /* Find the regs used in memory address computation.  */
      new_flags.is_write = 0;
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      break;

    case CONST_INT:   case CONST_DOUBLE:
    case SYMBOL_REF:  case LABEL_REF:     case CONST:
      break;

      /* Operators with side-effects.  */
    case POST_INC:    case POST_DEC:
      if (GET_CODE (XEXP (x, 0)) != REG)
	abort ();

      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (XEXP (x, 0), new_flags, pred);
      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (XEXP (x, 0), new_flags, pred);
      break;

    case POST_MODIFY:
      if (GET_CODE (XEXP (x, 0)) != REG)
	abort ();

      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (XEXP (x, 0), new_flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (x, 1), new_flags, pred);
      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (XEXP (x, 0), new_flags, pred);
      break;

      /* Handle common unary and binary ops for efficiency.  */
    case COMPARE:  case PLUS:    case MINUS:   case MULT:      case DIV:
    case MOD:      case UDIV:    case UMOD:    case AND:       case IOR:
    case XOR:      case ASHIFT:  case ROTATE:  case ASHIFTRT:  case LSHIFTRT:
    case ROTATERT: case SMIN:    case SMAX:    case UMIN:      case UMAX:
    case NE:       case EQ:      case GE:      case GT:        case LE:
    case LT:       case GEU:     case GTU:     case LEU:       case LTU:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (x, 1), new_flags, pred);
      break;

    case NEG:      case NOT:	        case SIGN_EXTEND:     case ZERO_EXTEND:
    case TRUNCATE: case FLOAT_EXTEND:   case FLOAT_TRUNCATE:  case FLOAT:
    case FIX:      case UNSIGNED_FLOAT: case UNSIGNED_FIX:    case ABS:
    case SQRT:     case FFS:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), flags, pred);
      break;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_LTOFF_DTPMOD:
	case UNSPEC_LTOFF_DTPREL:
	case UNSPEC_DTPREL:
	case UNSPEC_LTOFF_TPREL:
	case UNSPEC_TPREL:
	case UNSPEC_PRED_REL_MUTEX:
	case UNSPEC_PIC_CALL:
        case UNSPEC_MF:
        case UNSPEC_FETCHADD_ACQ:
	case UNSPEC_BSP_VALUE:
	case UNSPEC_FLUSHRS:
	case UNSPEC_BUNDLE_SELECTOR:
          break;

	case UNSPEC_GR_SPILL:
	case UNSPEC_GR_RESTORE:
	  {
	    HOST_WIDE_INT offset = INTVAL (XVECEXP (x, 0, 1));
	    HOST_WIDE_INT bit = (offset >> 3) & 63;

	    need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	    new_flags.is_write = (XINT (x, 1) == 1);
	    need_barrier |= rws_access_regno (AR_UNAT_BIT_0 + bit,
					      new_flags, pred);
	    break;
	  }
	  
	case UNSPEC_FR_SPILL:
	case UNSPEC_FR_RESTORE:
	case UNSPEC_POPCNT:
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	  break;

        case UNSPEC_ADDP4:
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	  break;

	case UNSPEC_FR_RECIP_APPROX:
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	  need_barrier |= rtx_needs_barrier (XVECEXP (x, 0, 1), flags, pred);
	  break;

        case UNSPEC_CMPXCHG_ACQ:
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 1), flags, pred);
	  need_barrier |= rtx_needs_barrier (XVECEXP (x, 0, 2), flags, pred);
	  break;

	default:
	  abort ();
	}
      break;

    case UNSPEC_VOLATILE:
      switch (XINT (x, 1))
	{
	case UNSPECV_ALLOC:
	  /* Alloc must always be the first instruction of a group.
	     We force this by always returning true.  */
	  /* ??? We might get better scheduling if we explicitly check for
	     input/local/output register dependencies, and modify the
	     scheduler so that alloc is always reordered to the start of
	     the current group.  We could then eliminate all of the
	     first_instruction code.  */
	  rws_access_regno (AR_PFS_REGNUM, flags, pred);

	  new_flags.is_write = 1;
	  rws_access_regno (REG_AR_CFM, new_flags, pred);
	  return 1;

	case UNSPECV_SET_BSP:
	  need_barrier = 1;
          break;

	case UNSPECV_BLOCKAGE:
	case UNSPECV_INSN_GROUP_BARRIER:
	case UNSPECV_BREAK:
	case UNSPECV_PSAC_ALL:
	case UNSPECV_PSAC_NORMAL:
	  return 0;

	default:
	  abort ();
	}
      break;

    case RETURN:
      new_flags.is_write = 0;
      need_barrier  = rws_access_regno (REG_RP, flags, pred);
      need_barrier |= rws_access_regno (AR_PFS_REGNUM, flags, pred);

      new_flags.is_write = 1;
      need_barrier |= rws_access_regno (AR_EC_REGNUM, new_flags, pred);
      need_barrier |= rws_access_regno (REG_AR_CFM, new_flags, pred);
      break;

    default:
      format_ptr = GET_RTX_FORMAT (GET_CODE (x));
      for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
	switch (format_ptr[i])
	  {
	  case '0':	/* unused field */
	  case 'i':	/* integer */
	  case 'n':	/* note */
	  case 'w':	/* wide integer */
	  case 's':	/* pointer to string */
	  case 'S':	/* optional pointer to string */
	    break;

	  case 'e':
	    if (rtx_needs_barrier (XEXP (x, i), flags, pred))
	      need_barrier = 1;
	    break;

	  case 'E':
	    for (j = XVECLEN (x, i) - 1; j >= 0; --j)
	      if (rtx_needs_barrier (XVECEXP (x, i, j), flags, pred))
		need_barrier = 1;
	    break;

	  default:
	    abort ();
	  }
      break;
    }
  return need_barrier;
}

/* Clear out the state for group_barrier_needed_p at the start of a
   sequence of insns.  */

static void
init_insn_group_barriers ()
{
  memset (rws_sum, 0, sizeof (rws_sum));
  first_instruction = 1;
}

/* Given the current state, recorded by previous calls to this function,
   determine whether a group barrier (a stop bit) is necessary before INSN.
   Return nonzero if so.  */

static int
group_barrier_needed_p (insn)
     rtx insn;
{
  rtx pat;
  int need_barrier = 0;
  struct reg_flags flags;

  memset (&flags, 0, sizeof (flags));
  switch (GET_CODE (insn))
    {
    case NOTE:
      break;

    case BARRIER:
      /* A barrier doesn't imply an instruction group boundary.  */
      break;

    case CODE_LABEL:
      memset (rws_insn, 0, sizeof (rws_insn));
      return 1;

    case CALL_INSN:
      flags.is_branch = 1;
      flags.is_sibcall = SIBLING_CALL_P (insn);
      memset (rws_insn, 0, sizeof (rws_insn));

      /* Don't bundle a call following another call.  */
      if ((pat = prev_active_insn (insn))
	  && GET_CODE (pat) == CALL_INSN)
	{
	  need_barrier = 1;
	  break;
	}

      need_barrier = rtx_needs_barrier (PATTERN (insn), flags, 0);
      break;

    case JUMP_INSN:
      flags.is_branch = 1;

      /* Don't bundle a jump following a call.  */
      if ((pat = prev_active_insn (insn))
	  && GET_CODE (pat) == CALL_INSN)
	{
	  need_barrier = 1;
	  break;
	}
      /* FALLTHRU */

    case INSN:
      if (GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER)
	/* Don't care about USE and CLOBBER "insns"---those are used to
	   indicate to the optimizer that it shouldn't get rid of
	   certain operations.  */
	break;

      pat = PATTERN (insn);

      /* Ug.  Hack hacks hacked elsewhere.  */
      switch (recog_memoized (insn))
	{
	  /* We play dependency tricks with the epilogue in order
	     to get proper schedules.  Undo this for dv analysis.  */
	case CODE_FOR_epilogue_deallocate_stack:
	case CODE_FOR_prologue_allocate_stack:
	  pat = XVECEXP (pat, 0, 0);
	  break;

	  /* The pattern we use for br.cloop confuses the code above.
	     The second element of the vector is representative.  */
	case CODE_FOR_doloop_end_internal:
	  pat = XVECEXP (pat, 0, 1);
	  break;

	  /* Doesn't generate code.  */
	case CODE_FOR_pred_rel_mutex:
	case CODE_FOR_prologue_use:
	  return 0;

	default:
	  break;
	}

      memset (rws_insn, 0, sizeof (rws_insn));
      need_barrier = rtx_needs_barrier (pat, flags, 0);

      /* Check to see if the previous instruction was a volatile
	 asm.  */
      if (! need_barrier)
	need_barrier = rws_access_regno (REG_VOLATILE, flags, 0);
      break;

    default:
      abort ();
    }

  if (first_instruction)
    {
      need_barrier = 0;
      first_instruction = 0;
    }

  return need_barrier;
}

/* Like group_barrier_needed_p, but do not clobber the current state.  */

static int
safe_group_barrier_needed_p (insn)
     rtx insn;
{
  struct reg_write_state rws_saved[NUM_REGS];
  int saved_first_instruction;
  int t;

  memcpy (rws_saved, rws_sum, NUM_REGS * sizeof *rws_saved);
  saved_first_instruction = first_instruction;

  t = group_barrier_needed_p (insn);

  memcpy (rws_sum, rws_saved, NUM_REGS * sizeof *rws_saved);
  first_instruction = saved_first_instruction;

  return t;
}

/* INSNS is an chain of instructions.  Scan the chain, and insert stop bits
   as necessary to eliminate dependendencies.  This function assumes that
   a final instruction scheduling pass has been run which has already
   inserted most of the necessary stop bits.  This function only inserts
   new ones at basic block boundaries, since these are invisible to the
   scheduler.  */

static void
emit_insn_group_barriers (dump, insns)
     FILE *dump;
     rtx insns;
{
  rtx insn;
  rtx last_label = 0;
  int insns_since_last_label = 0;

  init_insn_group_barriers ();

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	{
	  if (insns_since_last_label)
	    last_label = insn;
	  insns_since_last_label = 0;
	}
      else if (GET_CODE (insn) == NOTE
	       && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BASIC_BLOCK)
	{
	  if (insns_since_last_label)
	    last_label = insn;
	  insns_since_last_label = 0;
	}
      else if (GET_CODE (insn) == INSN
	       && GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
	       && XINT (PATTERN (insn), 1) == UNSPECV_INSN_GROUP_BARRIER)
	{
	  init_insn_group_barriers ();
	  last_label = 0;
	}
      else if (INSN_P (insn))
	{
	  insns_since_last_label = 1;

	  if (group_barrier_needed_p (insn))
	    {
	      if (last_label)
		{
		  if (dump)
		    fprintf (dump, "Emitting stop before label %d\n",
			     INSN_UID (last_label));
		  emit_insn_before (gen_insn_group_barrier (GEN_INT (3)), last_label);
		  insn = last_label;

		  init_insn_group_barriers ();
		  last_label = 0;
		}
	    }
	}
    }
}

/* Like emit_insn_group_barriers, but run if no final scheduling pass was run.
   This function has to emit all necessary group barriers.  */

static void
emit_all_insn_group_barriers (dump, insns)
     FILE *dump ATTRIBUTE_UNUSED;
     rtx insns;
{
  rtx insn;

  init_insn_group_barriers ();

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == BARRIER)
	{
	  rtx last = prev_active_insn (insn);

	  if (! last)
	    continue;
	  if (GET_CODE (last) == JUMP_INSN
	      && GET_CODE (PATTERN (last)) == ADDR_DIFF_VEC)
	    last = prev_active_insn (last);
	  if (recog_memoized (last) != CODE_FOR_insn_group_barrier)
	    emit_insn_after (gen_insn_group_barrier (GEN_INT (3)), last);

	  init_insn_group_barriers ();
	}
      else if (INSN_P (insn))
	{
	  if (recog_memoized (insn) == CODE_FOR_insn_group_barrier)
	    init_insn_group_barriers ();
	  else if (group_barrier_needed_p (insn))
	    {
	      emit_insn_before (gen_insn_group_barrier (GEN_INT (3)), insn);
	      init_insn_group_barriers ();
	      group_barrier_needed_p (insn);
	    }
	}
    }
}

static int errata_find_address_regs PARAMS ((rtx *, void *));
static void errata_emit_nops PARAMS ((rtx));
static void fixup_errata PARAMS ((void));

/* This structure is used to track some details about the previous insns
   groups so we can determine if it may be necessary to insert NOPs to
   workaround hardware errata.  */
static struct group
{
  HARD_REG_SET p_reg_set;
  HARD_REG_SET gr_reg_conditionally_set;
} last_group[2];

/* Index into the last_group array.  */
static int group_idx;

/* Called through for_each_rtx; determines if a hard register that was
   conditionally set in the previous group is used as an address register.
   It ensures that for_each_rtx returns 1 in that case.  */
static int
errata_find_address_regs (xp, data)
     rtx *xp;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *xp;
  if (GET_CODE (x) != MEM)
    return 0;
  x = XEXP (x, 0);
  if (GET_CODE (x) == POST_MODIFY)
    x = XEXP (x, 0);
  if (GET_CODE (x) == REG)
    {
      struct group *prev_group = last_group + (group_idx ^ 1);
      if (TEST_HARD_REG_BIT (prev_group->gr_reg_conditionally_set,
			     REGNO (x)))
	return 1;
      return -1;
    }
  return 0;
}

/* Called for each insn; this function keeps track of the state in
   last_group and emits additional NOPs if necessary to work around
   an Itanium A/B step erratum.  */
static void
errata_emit_nops (insn)
     rtx insn;
{
  struct group *this_group = last_group + group_idx;
  struct group *prev_group = last_group + (group_idx ^ 1);
  rtx pat = PATTERN (insn);
  rtx cond = GET_CODE (pat) == COND_EXEC ? COND_EXEC_TEST (pat) : 0;
  rtx real_pat = cond ? COND_EXEC_CODE (pat) : pat;
  enum attr_type type;
  rtx set = real_pat;

  if (GET_CODE (real_pat) == USE
      || GET_CODE (real_pat) == CLOBBER
      || GET_CODE (real_pat) == ASM_INPUT
      || GET_CODE (real_pat) == ADDR_VEC
      || GET_CODE (real_pat) == ADDR_DIFF_VEC
      || asm_noperands (PATTERN (insn)) >= 0)
    return;

  /* single_set doesn't work for COND_EXEC insns, so we have to duplicate
     parts of it.  */

  if (GET_CODE (set) == PARALLEL)
    {
      int i;
      set = XVECEXP (real_pat, 0, 0);
      for (i = 1; i < XVECLEN (real_pat, 0); i++)
	if (GET_CODE (XVECEXP (real_pat, 0, i)) != USE
	    && GET_CODE (XVECEXP (real_pat, 0, i)) != CLOBBER)
	  {
	    set = 0;
	    break;
	  }
    }

  if (set && GET_CODE (set) != SET)
    set = 0;

  type  = get_attr_type (insn);

  if (type == TYPE_F
      && set && REG_P (SET_DEST (set)) && PR_REGNO_P (REGNO (SET_DEST (set))))
    SET_HARD_REG_BIT (this_group->p_reg_set, REGNO (SET_DEST (set)));

  if ((type == TYPE_M || type == TYPE_A) && cond && set
      && REG_P (SET_DEST (set))
      && GET_CODE (SET_SRC (set)) != PLUS
      && GET_CODE (SET_SRC (set)) != MINUS
      && (GET_CODE (SET_SRC (set)) != ASHIFT
	  || !shladd_operand (XEXP (SET_SRC (set), 1), VOIDmode))
      && (GET_CODE (SET_SRC (set)) != MEM
	  || GET_CODE (XEXP (SET_SRC (set), 0)) != POST_MODIFY)
      && GENERAL_REGNO_P (REGNO (SET_DEST (set))))
    {
      if (GET_RTX_CLASS (GET_CODE (cond)) != '<'
	  || ! REG_P (XEXP (cond, 0)))
	abort ();

      if (TEST_HARD_REG_BIT (prev_group->p_reg_set, REGNO (XEXP (cond, 0))))
	SET_HARD_REG_BIT (this_group->gr_reg_conditionally_set, REGNO (SET_DEST (set)));
    }
  if (for_each_rtx (&real_pat, errata_find_address_regs, NULL))
    {
      emit_insn_before (gen_insn_group_barrier (GEN_INT (3)), insn);
      emit_insn_before (gen_nop (), insn);
      emit_insn_before (gen_insn_group_barrier (GEN_INT (3)), insn);
      group_idx = 0;
      memset (last_group, 0, sizeof last_group);
    }
}

/* Emit extra nops if they are required to work around hardware errata.  */

static void
fixup_errata ()
{
  rtx insn;

  if (! TARGET_B_STEP)
    return;

  group_idx = 0;
  memset (last_group, 0, sizeof last_group);

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (!INSN_P (insn))
	continue;

      if (ia64_safe_type (insn) == TYPE_S)
	{
	  group_idx ^= 1;
	  memset (last_group + group_idx, 0, sizeof last_group[group_idx]);
	}
      else
	errata_emit_nops (insn);
    }
}

/* Instruction scheduling support.  */
/* Describe one bundle.  */

struct bundle
{
  /* Zero if there's no possibility of a stop in this bundle other than
     at the end, otherwise the position of the optional stop bit.  */
  int possible_stop;
  /* The types of the three slots.  */
  enum attr_type t[3];
  /* The pseudo op to be emitted into the assembler output.  */
  const char *name;
};

#define NR_BUNDLES 10

/* A list of all available bundles.  */

static const struct bundle bundle[NR_BUNDLES] =
{
  { 2, { TYPE_M, TYPE_I, TYPE_I }, ".mii" },
  { 1, { TYPE_M, TYPE_M, TYPE_I }, ".mmi" },
  { 0, { TYPE_M, TYPE_F, TYPE_I }, ".mfi" },
  { 0, { TYPE_M, TYPE_M, TYPE_F }, ".mmf" },
#if NR_BUNDLES == 10
  { 0, { TYPE_B, TYPE_B, TYPE_B }, ".bbb" },
  { 0, { TYPE_M, TYPE_B, TYPE_B }, ".mbb" },
#endif
  { 0, { TYPE_M, TYPE_I, TYPE_B }, ".mib" },
  { 0, { TYPE_M, TYPE_M, TYPE_B }, ".mmb" },
  { 0, { TYPE_M, TYPE_F, TYPE_B }, ".mfb" },
  /* .mfi needs to occur earlier than .mlx, so that we only generate it if
     it matches an L type insn.  Otherwise we'll try to generate L type
     nops.  */
  { 0, { TYPE_M, TYPE_L, TYPE_X }, ".mlx" }
};

/* Describe a packet of instructions.  Packets consist of two bundles that
   are visible to the hardware in one scheduling window.  */

struct ia64_packet
{
  const struct bundle *t1, *t2;
  /* Precomputed value of the first split issue in this packet if a cycle
     starts at its beginning.  */
  int first_split;
  /* For convenience, the insn types are replicated here so we don't have
     to go through T1 and T2 all the time.  */
  enum attr_type t[6];
};

/* An array containing all possible packets.  */
#define NR_PACKETS (NR_BUNDLES * NR_BUNDLES)
static struct ia64_packet packets[NR_PACKETS];

/* Map attr_type to a string with the name.  */

static const char *const type_names[] =
{
  "UNKNOWN", "A", "I", "M", "F", "B", "L", "X", "S"
};

/* Nonzero if we should insert stop bits into the schedule.  */
int ia64_final_schedule = 0;

static int itanium_split_issue PARAMS ((const struct ia64_packet *, int));
static rtx ia64_single_set PARAMS ((rtx));
static int insn_matches_slot PARAMS ((const struct ia64_packet *, enum attr_type, int, rtx));
static void ia64_emit_insn_before PARAMS ((rtx, rtx));
static void maybe_rotate PARAMS ((FILE *));
static void finish_last_head PARAMS ((FILE *, int));
static void rotate_one_bundle PARAMS ((FILE *));
static void rotate_two_bundles PARAMS ((FILE *));
static void nop_cycles_until PARAMS ((int, FILE *));
static void cycle_end_fill_slots PARAMS ((FILE *));
static int packet_matches_p PARAMS ((const struct ia64_packet *, int, int *));
static int get_split PARAMS ((const struct ia64_packet *, int));
static int find_best_insn PARAMS ((rtx *, enum attr_type *, int,
				   const struct ia64_packet *, int));
static void find_best_packet PARAMS ((int *, const struct ia64_packet **,
				      rtx *, enum attr_type *, int));
static int itanium_reorder PARAMS ((FILE *, rtx *, rtx *, int));
static void dump_current_packet PARAMS ((FILE *));
static void schedule_stop PARAMS ((FILE *));
static rtx gen_nop_type PARAMS ((enum attr_type));
static void ia64_emit_nops PARAMS ((void));

/* Map a bundle number to its pseudo-op.  */

const char *
get_bundle_name (b)
     int b;
{
  return bundle[b].name;
}

/* Compute the slot which will cause a split issue in packet P if the
   current cycle begins at slot BEGIN.  */

static int
itanium_split_issue (p, begin)
     const struct ia64_packet *p;
     int begin;
{
  int type_count[TYPE_S];
  int i;
  int split = 6;

  if (begin < 3)
    {
      /* Always split before and after MMF.  */
      if (p->t[0] == TYPE_M && p->t[1] == TYPE_M && p->t[2] == TYPE_F)
	return 3;
      if (p->t[3] == TYPE_M && p->t[4] == TYPE_M && p->t[5] == TYPE_F)
	return 3;
      /* Always split after MBB and BBB.  */
      if (p->t[1] == TYPE_B)
	return 3;
      /* Split after first bundle in MIB BBB combination.  */
      if (p->t[2] == TYPE_B && p->t[3] == TYPE_B)
	return 3;
    }

  memset (type_count, 0, sizeof type_count);
  for (i = begin; i < split; i++)
    {
      enum attr_type t0 = p->t[i];
      /* An MLX bundle reserves the same units as an MFI bundle.  */
      enum attr_type t = (t0 == TYPE_L ? TYPE_F
			  : t0 == TYPE_X ? TYPE_I
			  : t0);

      /* Itanium can execute up to 3 branches, 2 floating point, 2 memory, and
	 2 integer per cycle.  */
      int max = (t == TYPE_B ? 3 : 2);
      if (type_count[t] == max)
	return i;

      type_count[t]++;
    }
  return split;
}

/* Return the maximum number of instructions a cpu can issue.  */

static int
ia64_issue_rate ()
{
  return 6;
}

/* Helper function - like single_set, but look inside COND_EXEC.  */

static rtx
ia64_single_set (insn)
     rtx insn;
{
  rtx x = PATTERN (insn), ret;
  if (GET_CODE (x) == COND_EXEC)
    x = COND_EXEC_CODE (x);
  if (GET_CODE (x) == SET)
    return x;

  /* Special case here prologue_allocate_stack and epilogue_deallocate_stack.
     Although they are not classical single set, the second set is there just
     to protect it from moving past FP-relative stack accesses.  */
  switch (recog_memoized (insn))
    {
    case CODE_FOR_prologue_allocate_stack:
    case CODE_FOR_epilogue_deallocate_stack:
      ret = XVECEXP (x, 0, 0);
      break;

    default:
      ret = single_set_2 (insn, x);
      break;
    }

  return ret;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
ia64_adjust_cost (insn, link, dep_insn, cost)
     rtx insn, link, dep_insn;
     int cost;
{
  enum attr_type dep_type;
  enum attr_itanium_class dep_class;
  enum attr_itanium_class insn_class;
  rtx dep_set, set, src, addr;

  if (GET_CODE (PATTERN (insn)) == CLOBBER
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (dep_insn)) == CLOBBER
      || GET_CODE (PATTERN (dep_insn)) == USE
      /* @@@ Not accurate for indirect calls.  */
      || GET_CODE (insn) == CALL_INSN
      || ia64_safe_type (insn) == TYPE_S)
    return 0;

  if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT
      || REG_NOTE_KIND (link) == REG_DEP_ANTI)
    return 0;

  dep_type = ia64_safe_type (dep_insn);
  dep_class = ia64_safe_itanium_class (dep_insn);
  insn_class = ia64_safe_itanium_class (insn);

  /* Compares that feed a conditional branch can execute in the same
     cycle.  */
  dep_set = ia64_single_set (dep_insn);
  set = ia64_single_set (insn);

  if (dep_type != TYPE_F
      && dep_set
      && GET_CODE (SET_DEST (dep_set)) == REG
      && PR_REG (REGNO (SET_DEST (dep_set)))
      && GET_CODE (insn) == JUMP_INSN)
    return 0;

  if (dep_set && GET_CODE (SET_DEST (dep_set)) == MEM)
    {
      /* ??? Can't find any information in the documenation about whether
	 a sequence
	   st [rx] = ra
	   ld rb = [ry]
	 splits issue.  Assume it doesn't.  */
      return 0;
    }

  src = set ? SET_SRC (set) : 0;
  addr = 0;
  if (set)
    {
      if (GET_CODE (SET_DEST (set)) == MEM)
	addr = XEXP (SET_DEST (set), 0);
      else if (GET_CODE (SET_DEST (set)) == SUBREG
	       && GET_CODE (SUBREG_REG (SET_DEST (set))) == MEM)
	addr = XEXP (SUBREG_REG (SET_DEST (set)), 0);
      else
	{
	  addr = src;
	  if (GET_CODE (addr) == UNSPEC && XVECLEN (addr, 0) > 0)
	    addr = XVECEXP (addr, 0, 0);
	  while (GET_CODE (addr) == SUBREG || GET_CODE (addr) == ZERO_EXTEND)
	    addr = XEXP (addr, 0);

	  /* Note that LO_SUM is used for GOT loads.  */
	  if (GET_CODE (addr) == MEM || GET_CODE (addr) == LO_SUM)
	    addr = XEXP (addr, 0);
	  else
	    addr = 0;
	}
    }

  if (addr && GET_CODE (addr) == POST_MODIFY)
    addr = XEXP (addr, 0);

  set = ia64_single_set (dep_insn);

  if ((dep_class == ITANIUM_CLASS_IALU
       || dep_class == ITANIUM_CLASS_ILOG
       || dep_class == ITANIUM_CLASS_LD)
      && (insn_class == ITANIUM_CLASS_LD
	  || insn_class == ITANIUM_CLASS_ST))
    {
      if (! addr || ! set)
	abort ();
      /* This isn't completely correct - an IALU that feeds an address has
	 a latency of 1 cycle if it's issued in an M slot, but 2 cycles
	 otherwise.  Unfortunately there's no good way to describe this.  */
      if (reg_overlap_mentioned_p (SET_DEST (set), addr))
	return cost + 1;
    }

  if ((dep_class == ITANIUM_CLASS_IALU
       || dep_class == ITANIUM_CLASS_ILOG
       || dep_class == ITANIUM_CLASS_LD)
      && (insn_class == ITANIUM_CLASS_MMMUL
	  || insn_class == ITANIUM_CLASS_MMSHF
	  || insn_class == ITANIUM_CLASS_MMSHFI))
    return 3;

  if (dep_class == ITANIUM_CLASS_FMAC
      && (insn_class == ITANIUM_CLASS_FMISC
	  || insn_class == ITANIUM_CLASS_FCVTFX
	  || insn_class == ITANIUM_CLASS_XMPY))
    return 7;

  if ((dep_class == ITANIUM_CLASS_FMAC
       || dep_class == ITANIUM_CLASS_FMISC
       || dep_class == ITANIUM_CLASS_FCVTFX
       || dep_class == ITANIUM_CLASS_XMPY)
      && insn_class == ITANIUM_CLASS_STF)
    return 8;

  /* Intel docs say only LD, ST, IALU, ILOG, ISHF consumers have latency 4,
     but HP engineers say any non-MM operation.  */
  if ((dep_class == ITANIUM_CLASS_MMMUL
       || dep_class == ITANIUM_CLASS_MMSHF
       || dep_class == ITANIUM_CLASS_MMSHFI)
      && insn_class != ITANIUM_CLASS_MMMUL
      && insn_class != ITANIUM_CLASS_MMSHF
      && insn_class != ITANIUM_CLASS_MMSHFI)
    return 4;

  return cost;
}

/* Describe the current state of the Itanium pipeline.  */
static struct
{
  /* The first slot that is used in the current cycle.  */
  int first_slot;
  /* The next slot to fill.  */
  int cur;
  /* The packet we have selected for the current issue window.  */
  const struct ia64_packet *packet;
  /* The position of the split issue that occurs due to issue width
     limitations (6 if there's no split issue).  */
  int split;
  /* Record data about the insns scheduled so far in the same issue
     window.  The elements up to but not including FIRST_SLOT belong
     to the previous cycle, the ones starting with FIRST_SLOT belong
     to the current cycle.  */
  enum attr_type types[6];
  rtx insns[6];
  int stopbit[6];
  /* Nonzero if we decided to schedule a stop bit.  */
  int last_was_stop;
} sched_data;

/* Temporary arrays; they have enough elements to hold all insns that
   can be ready at the same time while scheduling of the current block.
   SCHED_READY can hold ready insns, SCHED_TYPES their types.  */
static rtx *sched_ready;
static enum attr_type *sched_types;

/* Determine whether an insn INSN of type ITYPE can fit into slot SLOT
   of packet P.  */

static int
insn_matches_slot (p, itype, slot, insn)
     const struct ia64_packet *p;
     enum attr_type itype;
     int slot;
     rtx insn;
{
  enum attr_itanium_requires_unit0 u0;
  enum attr_type stype = p->t[slot];

  if (insn)
    {
      u0 = ia64_safe_itanium_requires_unit0 (insn);
      if (u0 == ITANIUM_REQUIRES_UNIT0_YES)
	{
	  int i;
	  for (i = sched_data.first_slot; i < slot; i++)
	    if (p->t[i] == stype
		|| (stype == TYPE_F && p->t[i] == TYPE_L)
		|| (stype == TYPE_I && p->t[i] == TYPE_X))
	      return 0;
	}
      if (GET_CODE (insn) == CALL_INSN)
	{
	  /* Reject calls in multiway branch packets.  We want to limit
	     the number of multiway branches we generate (since the branch
	     predictor is limited), and this seems to work fairly well.
	     (If we didn't do this, we'd have to add another test here to
	     force calls into the third slot of the bundle.)  */
	  if (slot < 3)
	    {
	      if (p->t[1] == TYPE_B)
		return 0;
	    }
	  else
	    {
	      if (p->t[4] == TYPE_B)
		return 0;
	    }
	}
    }

  if (itype == stype)
    return 1;
  if (itype == TYPE_A)
    return stype == TYPE_M || stype == TYPE_I;
  return 0;
}

/* Like emit_insn_before, but skip cycle_display notes.
   ??? When cycle display notes are implemented, update this.  */

static void
ia64_emit_insn_before (insn, before)
     rtx insn, before;
{
  emit_insn_before (insn, before);
}

/* When rotating a bundle out of the issue window, insert a bundle selector
   insn in front of it.  DUMP is the scheduling dump file or NULL.  START
   is either 0 or 3, depending on whether we want to emit a bundle selector
   for the first bundle or the second bundle in the current issue window.

   The selector insns are emitted this late because the selected packet can
   be changed until parts of it get rotated out.  */

static void
finish_last_head (dump, start)
     FILE *dump;
     int start;
{
  const struct ia64_packet *p = sched_data.packet;
  const struct bundle *b = start == 0 ? p->t1 : p->t2;
  int bundle_type = b - bundle;
  rtx insn;
  int i;

  if (! ia64_final_schedule)
    return;

  for (i = start; sched_data.insns[i] == 0; i++)
    if (i == start + 3)
      abort ();
  insn = sched_data.insns[i];

  if (dump)
    fprintf (dump, "//    Emitting template before %d: %s\n",
	     INSN_UID (insn), b->name);

  ia64_emit_insn_before (gen_bundle_selector (GEN_INT (bundle_type)), insn);
}

/* We can't schedule more insns this cycle.  Fix up the scheduling state
   and advance FIRST_SLOT and CUR.
   We have to distribute the insns that are currently found between
   FIRST_SLOT and CUR into the slots of the packet we have selected.  So
   far, they are stored successively in the fields starting at FIRST_SLOT;
   now they must be moved to the correct slots.
   DUMP is the current scheduling dump file, or NULL.  */

static void
cycle_end_fill_slots (dump)
     FILE *dump;
{
  const struct ia64_packet *packet = sched_data.packet;
  int slot, i;
  enum attr_type tmp_types[6];
  rtx tmp_insns[6];

  memcpy (tmp_types, sched_data.types, 6 * sizeof (enum attr_type));
  memcpy (tmp_insns, sched_data.insns, 6 * sizeof (rtx));

  for (i = slot = sched_data.first_slot; i < sched_data.cur; i++)
    {
      enum attr_type t = tmp_types[i];
      if (t != ia64_safe_type (tmp_insns[i]))
	abort ();
      while (! insn_matches_slot (packet, t, slot, tmp_insns[i]))
	{
	  if (slot > sched_data.split)
	    abort ();
	  if (dump)
	    fprintf (dump, "// Packet needs %s, have %s\n",
		     type_names[packet->t[slot]], type_names[t]);
	  sched_data.types[slot] = packet->t[slot];
	  sched_data.insns[slot] = 0;
	  sched_data.stopbit[slot] = 0;

	  /* ??? TYPE_L instructions always fill up two slots, but we don't
	     support TYPE_L nops.  */
	  if (packet->t[slot] == TYPE_L)
	    abort ();

	  slot++;
	}

      /* Do _not_ use T here.  If T == TYPE_A, then we'd risk changing the
	 actual slot type later.  */
      sched_data.types[slot] = packet->t[slot];
      sched_data.insns[slot] = tmp_insns[i];
      sched_data.stopbit[slot] = 0;
      slot++;

      /* TYPE_L instructions always fill up two slots.  */
      if (t == TYPE_L)
	{
	  sched_data.types[slot] = packet->t[slot];
	  sched_data.insns[slot] = 0;
	  sched_data.stopbit[slot] = 0;
	  slot++;
	}
    }

  /* This isn't right - there's no need to pad out until the forced split;
     the CPU will automatically split if an insn isn't ready.  */
#if 0
  while (slot < sched_data.split)
    {
      sched_data.types[slot] = packet->t[slot];
      sched_data.insns[slot] = 0;
      sched_data.stopbit[slot] = 0;
      slot++;
    }
#endif

  sched_data.first_slot = sched_data.cur = slot;
}

/* Bundle rotations, as described in the Itanium optimization manual.
   We can rotate either one or both bundles out of the issue window.
   DUMP is the current scheduling dump file, or NULL.  */

static void
rotate_one_bundle (dump)
     FILE *dump;
{
  if (dump)
    fprintf (dump, "// Rotating one bundle.\n");

  finish_last_head (dump, 0);
  if (sched_data.cur > 3)
    {
      sched_data.cur -= 3;
      sched_data.first_slot -= 3;
      memmove (sched_data.types,
	       sched_data.types + 3,
	       sched_data.cur * sizeof *sched_data.types);
      memmove (sched_data.stopbit,
	       sched_data.stopbit + 3,
	       sched_data.cur * sizeof *sched_data.stopbit);
      memmove (sched_data.insns,
	       sched_data.insns + 3,
	       sched_data.cur * sizeof *sched_data.insns);
      sched_data.packet
	= &packets[(sched_data.packet->t2 - bundle) * NR_BUNDLES];
    }
  else
    {
      sched_data.cur = 0;
      sched_data.first_slot = 0;
    }
}

static void
rotate_two_bundles (dump)
     FILE *dump;
{
  if (dump)
    fprintf (dump, "// Rotating two bundles.\n");

  if (sched_data.cur == 0)
    return;

  finish_last_head (dump, 0);
  if (sched_data.cur > 3)
    finish_last_head (dump, 3);
  sched_data.cur = 0;
  sched_data.first_slot = 0;
}

/* We're beginning a new block.  Initialize data structures as necessary.  */

static void
ia64_sched_init (dump, sched_verbose, max_ready)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     int max_ready;
{
  static int initialized = 0;

  if (! initialized)
    {
      int b1, b2, i;

      initialized = 1;

      for (i = b1 = 0; b1 < NR_BUNDLES; b1++)
	{
	  const struct bundle *t1 = bundle + b1;
	  for (b2 = 0; b2 < NR_BUNDLES; b2++, i++)
	    {
	      const struct bundle *t2 = bundle + b2;

	      packets[i].t1 = t1;
	      packets[i].t2 = t2;
	    }
	}
      for (i = 0; i < NR_PACKETS; i++)
	{
	  int j;
	  for (j = 0; j < 3; j++)
	    packets[i].t[j] = packets[i].t1->t[j];
	  for (j = 0; j < 3; j++)
	    packets[i].t[j + 3] = packets[i].t2->t[j];
	  packets[i].first_split = itanium_split_issue (packets + i, 0);
	}
	
    }

  init_insn_group_barriers ();

  memset (&sched_data, 0, sizeof sched_data);
  sched_types = (enum attr_type *) xmalloc (max_ready
					    * sizeof (enum attr_type));
  sched_ready = (rtx *) xmalloc (max_ready * sizeof (rtx));
}

/* See if the packet P can match the insns we have already scheduled.  Return
   nonzero if so.  In *PSLOT, we store the first slot that is available for
   more instructions if we choose this packet.
   SPLIT holds the last slot we can use, there's a split issue after it so
   scheduling beyond it would cause us to use more than one cycle.  */

static int
packet_matches_p (p, split, pslot)
     const struct ia64_packet *p;
     int split;
     int *pslot;
{
  int filled = sched_data.cur;
  int first = sched_data.first_slot;
  int i, slot;

  /* First, check if the first of the two bundles must be a specific one (due
     to stop bits).  */
  if (first > 0 && sched_data.stopbit[0] && p->t1->possible_stop != 1)
    return 0;
  if (first > 1 && sched_data.stopbit[1] && p->t1->possible_stop != 2)
    return 0;

  for (i = 0; i < first; i++)
    if (! insn_matches_slot (p, sched_data.types[i], i,
			     sched_data.insns[i]))
      return 0;
  for (i = slot = first; i < filled; i++)
    {
      while (slot < split)
	{
	  if (insn_matches_slot (p, sched_data.types[i], slot,
				 sched_data.insns[i]))
	    break;
	  slot++;
	}
      if (slot == split)
	return 0;
      slot++;
    }

  if (pslot)
    *pslot = slot;
  return 1;
}

/* A frontend for itanium_split_issue.  For a packet P and a slot
   number FIRST that describes the start of the current clock cycle,
   return the slot number of the first split issue.  This function
   uses the cached number found in P if possible.  */

static int
get_split (p, first)
     const struct ia64_packet *p;
     int first;
{
  if (first == 0)
    return p->first_split;
  return itanium_split_issue (p, first);
}

/* Given N_READY insns in the array READY, whose types are found in the
   corresponding array TYPES, return the insn that is best suited to be
   scheduled in slot SLOT of packet P.  */

static int
find_best_insn (ready, types, n_ready, p, slot)
     rtx *ready;
     enum attr_type *types;
     int n_ready;
     const struct ia64_packet *p;
     int slot;
{
  int best = -1;
  int best_pri = 0;
  while (n_ready-- > 0)
    {
      rtx insn = ready[n_ready];
      if (! insn)
	continue;
      if (best >= 0 && INSN_PRIORITY (ready[n_ready]) < best_pri)
	break;
      /* If we have equally good insns, one of which has a stricter
	 slot requirement, prefer the one with the stricter requirement.  */
      if (best >= 0 && types[n_ready] == TYPE_A)
	continue;
      if (insn_matches_slot (p, types[n_ready], slot, insn))
	{
	  best = n_ready;
	  best_pri = INSN_PRIORITY (ready[best]);

	  /* If there's no way we could get a stricter requirement, stop
	     looking now.  */
	  if (types[n_ready] != TYPE_A
	      && ia64_safe_itanium_requires_unit0 (ready[n_ready]))
	    break;
	  break;
	}
    }
  return best;
}

/* Select the best packet to use given the current scheduler state and the
   current ready list.
   READY is an array holding N_READY ready insns; TYPES is a corresponding
   array that holds their types.  Store the best packet in *PPACKET and the
   number of insns that can be scheduled in the current cycle in *PBEST.  */

static void
find_best_packet (pbest, ppacket, ready, types, n_ready)
     int *pbest;
     const struct ia64_packet **ppacket;
     rtx *ready;
     enum attr_type *types;
     int n_ready;
{
  int first = sched_data.first_slot;
  int best = 0;
  int lowest_end = 6;
  const struct ia64_packet *best_packet = NULL;
  int i;

  for (i = 0; i < NR_PACKETS; i++)
    {
      const struct ia64_packet *p = packets + i;
      int slot;
      int split = get_split (p, first);
      int win = 0;
      int first_slot, last_slot;
      int b_nops = 0;

      if (! packet_matches_p (p, split, &first_slot))
	continue;

      memcpy (sched_ready, ready, n_ready * sizeof (rtx));

      win = 0;
      last_slot = 6;
      for (slot = first_slot; slot < split; slot++)
	{
	  int insn_nr;

	  /* Disallow a degenerate case where the first bundle doesn't
	     contain anything but NOPs!  */
	  if (first_slot == 0 && win == 0 && slot == 3)
	    {
	      win = -1;
	      break;
	    }

	  insn_nr = find_best_insn (sched_ready, types, n_ready, p, slot);
	  if (insn_nr >= 0)
	    {
	      sched_ready[insn_nr] = 0;
	      last_slot = slot;
	      win++;
	    }
	  else if (p->t[slot] == TYPE_B)
	    b_nops++;
	}
      /* We must disallow MBB/BBB packets if any of their B slots would be
	 filled with nops.  */
      if (last_slot < 3)
	{
	  if (p->t[1] == TYPE_B && (b_nops || last_slot < 2))
	    win = -1;
	}
      else
	{
	  if (p->t[4] == TYPE_B && (b_nops || last_slot < 5))
	    win = -1;
	}

      if (win > best
	  || (win == best && last_slot < lowest_end))
	{
	  best = win;
	  lowest_end = last_slot;
	  best_packet = p;
	}
    }
  *pbest = best;
  *ppacket = best_packet;
}

/* Reorder the ready list so that the insns that can be issued in this cycle
   are found in the correct order at the end of the list.
   DUMP is the scheduling dump file, or NULL.  READY points to the start,
   E_READY to the end of the ready list.  MAY_FAIL determines what should be
   done if no insns can be scheduled in this cycle: if it is zero, we abort,
   otherwise we return 0.
   Return 1 if any insns can be scheduled in this cycle.  */

static int
itanium_reorder (dump, ready, e_ready, may_fail)
     FILE *dump;
     rtx *ready;
     rtx *e_ready;
     int may_fail;
{
  const struct ia64_packet *best_packet;
  int n_ready = e_ready - ready;
  int first = sched_data.first_slot;
  int i, best, best_split, filled;

  for (i = 0; i < n_ready; i++)
    sched_types[i] = ia64_safe_type (ready[i]);

  find_best_packet (&best, &best_packet, ready, sched_types, n_ready);

  if (best == 0)
    {
      if (may_fail)
	return 0;
      abort ();
    }

  if (dump)
    {
      fprintf (dump, "// Selected bundles: %s %s (%d insns)\n",
	       best_packet->t1->name,
	       best_packet->t2 ? best_packet->t2->name : NULL, best);
    }

  best_split = itanium_split_issue (best_packet, first);
  packet_matches_p (best_packet, best_split, &filled);

  for (i = filled; i < best_split; i++)
    {
      int insn_nr;

      insn_nr = find_best_insn (ready, sched_types, n_ready, best_packet, i);
      if (insn_nr >= 0)
	{
	  rtx insn = ready[insn_nr];
	  memmove (ready + insn_nr, ready + insn_nr + 1,
		   (n_ready - insn_nr - 1) * sizeof (rtx));
	  memmove (sched_types + insn_nr, sched_types + insn_nr + 1,
		   (n_ready - insn_nr - 1) * sizeof (enum attr_type));
	  ready[--n_ready] = insn;
	}
    }

  sched_data.packet = best_packet;
  sched_data.split = best_split;
  return 1;
}

/* Dump information about the current scheduling state to file DUMP.  */

static void
dump_current_packet (dump)
     FILE *dump;
{
  int i;
  fprintf (dump, "//    %d slots filled:", sched_data.cur);
  for (i = 0; i < sched_data.first_slot; i++)
    {
      rtx insn = sched_data.insns[i];
      fprintf (dump, " %s", type_names[sched_data.types[i]]);
      if (insn)
	fprintf (dump, "/%s", type_names[ia64_safe_type (insn)]);
      if (sched_data.stopbit[i])
	fprintf (dump, " ;;");
    }
  fprintf (dump, " :::");
  for (i = sched_data.first_slot; i < sched_data.cur; i++)
    {
      rtx insn = sched_data.insns[i];
      enum attr_type t = ia64_safe_type (insn);
      fprintf (dump, " (%d) %s", INSN_UID (insn), type_names[t]);
    }
  fprintf (dump, "\n");
}

/* Schedule a stop bit.  DUMP is the current scheduling dump file, or
   NULL.  */

static void
schedule_stop (dump)
     FILE *dump;
{
  const struct ia64_packet *best = sched_data.packet;
  int i;
  int best_stop = 6;

  if (dump)
    fprintf (dump, "// Stop bit, cur = %d.\n", sched_data.cur);

  if (sched_data.cur == 0)
    {
      if (dump)
	fprintf (dump, "//   At start of bundle, so nothing to do.\n");

      rotate_two_bundles (NULL);
      return;
    }

  for (i = -1; i < NR_PACKETS; i++)
    {
      /* This is a slight hack to give the current packet the first chance.
	 This is done to avoid e.g. switching from MIB to MBB bundles.  */
      const struct ia64_packet *p = (i >= 0 ? packets + i : sched_data.packet);
      int split = get_split (p, sched_data.first_slot);
      const struct bundle *compare;
      int next, stoppos;

      if (! packet_matches_p (p, split, &next))
	continue;

      compare = next > 3 ? p->t2 : p->t1;

      stoppos = 3;
      if (compare->possible_stop)
	stoppos = compare->possible_stop;
      if (next > 3)
	stoppos += 3;

      if (stoppos < next || stoppos >= best_stop)
	{
	  if (compare->possible_stop == 0)
	    continue;
	  stoppos = (next > 3 ? 6 : 3);
	}
      if (stoppos < next || stoppos >= best_stop)
	continue;

      if (dump)
	fprintf (dump, "//   switching from %s %s to %s %s (stop at %d)\n",
		 best->t1->name, best->t2->name, p->t1->name, p->t2->name,
		 stoppos);

      best_stop = stoppos;
      best = p;
    }

  sched_data.packet = best;
  cycle_end_fill_slots (dump);
  while (sched_data.cur < best_stop)
    {
      sched_data.types[sched_data.cur] = best->t[sched_data.cur];
      sched_data.insns[sched_data.cur] = 0;
      sched_data.stopbit[sched_data.cur] = 0;
      sched_data.cur++;
    }
  sched_data.stopbit[sched_data.cur - 1] = 1;
  sched_data.first_slot = best_stop;

  if (dump)
    dump_current_packet (dump);
}

/* If necessary, perform one or two rotations on the scheduling state.  
   This should only be called if we are starting a new cycle.  */

static void
maybe_rotate (dump)
     FILE *dump;
{
  cycle_end_fill_slots (dump);
  if (sched_data.cur == 6)
    rotate_two_bundles (dump);
  else if (sched_data.cur >= 3)
    rotate_one_bundle (dump);
  sched_data.first_slot = sched_data.cur;
}

/* The clock cycle when ia64_sched_reorder was last called.  */
static int prev_cycle;

/* The first insn scheduled in the previous cycle.  This is the saved
   value of sched_data.first_slot.  */
static int prev_first;

/* Emit NOPs to fill the delay between PREV_CYCLE and CLOCK_VAR.  Used to
   pad out the delay between MM (shifts, etc.) and integer operations.  */

static void
nop_cycles_until (clock_var, dump)
     int clock_var;
     FILE *dump;
{
  int prev_clock = prev_cycle;
  int cycles_left = clock_var - prev_clock;
  bool did_stop = false;

  /* Finish the previous cycle; pad it out with NOPs.  */
  if (sched_data.cur == 3)
    {
      sched_emit_insn (gen_insn_group_barrier (GEN_INT (3)));
      did_stop = true;
      maybe_rotate (dump);
    }
  else if (sched_data.cur > 0)
    {
      int need_stop = 0;
      int split = itanium_split_issue (sched_data.packet, prev_first);

      if (sched_data.cur < 3 && split > 3)
	{
	  split = 3;
	  need_stop = 1;
	}

      if (split > sched_data.cur)
	{
	  int i;
	  for (i = sched_data.cur; i < split; i++)
	    {
	      rtx t = sched_emit_insn (gen_nop_type (sched_data.packet->t[i]));
	      sched_data.types[i] = sched_data.packet->t[i];
	      sched_data.insns[i] = t;
	      sched_data.stopbit[i] = 0;
	    }
	  sched_data.cur = split;
	}

      if (! need_stop && sched_data.cur > 0 && sched_data.cur < 6
	  && cycles_left > 1)
	{
	  int i;
	  for (i = sched_data.cur; i < 6; i++)
	    {
	      rtx t = sched_emit_insn (gen_nop_type (sched_data.packet->t[i]));
	      sched_data.types[i] = sched_data.packet->t[i];
	      sched_data.insns[i] = t;
	      sched_data.stopbit[i] = 0;
	    }
	  sched_data.cur = 6;
	  cycles_left--;
	  need_stop = 1;
	}

      if (need_stop || sched_data.cur == 6)
	{
	  sched_emit_insn (gen_insn_group_barrier (GEN_INT (3)));
	  did_stop = true;
	}
      maybe_rotate (dump);
    }

  cycles_left--;
  while (cycles_left > 0)
    {
      sched_emit_insn (gen_bundle_selector (GEN_INT (0)));
      sched_emit_insn (gen_nop_type (TYPE_M));
      sched_emit_insn (gen_nop_type (TYPE_I));
      if (cycles_left > 1)
	{
	  sched_emit_insn (gen_insn_group_barrier (GEN_INT (2)));
	  cycles_left--;
	}
      sched_emit_insn (gen_nop_type (TYPE_I));
      sched_emit_insn (gen_insn_group_barrier (GEN_INT (3)));
      did_stop = true;
      cycles_left--;
    }

  if (did_stop)
    init_insn_group_barriers ();
}

/* We are about to being issuing insns for this clock cycle.
   Override the default sort algorithm to better slot instructions.  */

static int
ia64_internal_sched_reorder (dump, sched_verbose, ready, pn_ready,
		    reorder_type, clock_var)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     rtx *ready;
     int *pn_ready;
     int reorder_type, clock_var;
{
  int n_asms;
  int n_ready = *pn_ready;
  rtx *e_ready = ready + n_ready;
  rtx *insnp;

  if (sched_verbose)
    {
      fprintf (dump, "// ia64_sched_reorder (type %d):\n", reorder_type);
      dump_current_packet (dump);
    }

  /* Work around the pipeline flush that will occurr if the results of
     an MM instruction are accessed before the result is ready.  Intel
     documentation says this only happens with IALU, ISHF, ILOG, LD,
     and ST consumers, but experimental evidence shows that *any* non-MM
     type instruction will incurr the flush.  */
  if (reorder_type == 0 && clock_var > 0 && ia64_final_schedule)
    {
      for (insnp = ready; insnp < e_ready; insnp++)
	{
	  rtx insn = *insnp, link;
	  enum attr_itanium_class t = ia64_safe_itanium_class (insn);

	  if (t == ITANIUM_CLASS_MMMUL
	      || t == ITANIUM_CLASS_MMSHF
	      || t == ITANIUM_CLASS_MMSHFI)
	    continue;

	  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
	    if (REG_NOTE_KIND (link) == 0)
	      {
		rtx other = XEXP (link, 0);
		enum attr_itanium_class t0 = ia64_safe_itanium_class (other);
		if (t0 == ITANIUM_CLASS_MMSHF || t0 == ITANIUM_CLASS_MMMUL)
		  {
		    nop_cycles_until (clock_var, sched_verbose ? dump : NULL);
		    goto out;
		  }
	      }
	}
    }
 out:

  prev_first = sched_data.first_slot;
  prev_cycle = clock_var;

  if (reorder_type == 0)
    maybe_rotate (sched_verbose ? dump : NULL);

  /* First, move all USEs, CLOBBERs and other crud out of the way.  */
  n_asms = 0;
  for (insnp = ready; insnp < e_ready; insnp++)
    if (insnp < e_ready)
      {
	rtx insn = *insnp;
	enum attr_type t = ia64_safe_type (insn);
	if (t == TYPE_UNKNOWN)
	  {
	    if (GET_CODE (PATTERN (insn)) == ASM_INPUT
		|| asm_noperands (PATTERN (insn)) >= 0)
	      {
		rtx lowest = ready[n_asms];
		ready[n_asms] = insn;
		*insnp = lowest;
		n_asms++;
	      }
	    else
	      {
		rtx highest = ready[n_ready - 1];
		ready[n_ready - 1] = insn;
		*insnp = highest;
		if (ia64_final_schedule && group_barrier_needed_p (insn))
		  {
		    schedule_stop (sched_verbose ? dump : NULL);
		    sched_data.last_was_stop = 1;
		    maybe_rotate (sched_verbose ? dump : NULL);
		  }

		return 1;
	      }
	  }
      }
  if (n_asms < n_ready)
    {
      /* Some normal insns to process.  Skip the asms.  */
      ready += n_asms;
      n_ready -= n_asms;
    }
  else if (n_ready > 0)
    {
      /* Only asm insns left.  */
      if (ia64_final_schedule && group_barrier_needed_p (ready[n_ready - 1]))
	{
	  schedule_stop (sched_verbose ? dump : NULL);
	  sched_data.last_was_stop = 1;
	  maybe_rotate (sched_verbose ? dump : NULL);
	}
      cycle_end_fill_slots (sched_verbose ? dump : NULL);
      return 1;
    }

  if (ia64_final_schedule)
    {
      int nr_need_stop = 0;

      for (insnp = ready; insnp < e_ready; insnp++)
	if (safe_group_barrier_needed_p (*insnp))
	  nr_need_stop++;

      /* Schedule a stop bit if
          - all insns require a stop bit, or
          - we are starting a new cycle and _any_ insns require a stop bit.
         The reason for the latter is that if our schedule is accurate, then
         the additional stop won't decrease performance at this point (since
	 there's a split issue at this point anyway), but it gives us more
         freedom when scheduling the currently ready insns.  */
      if ((reorder_type == 0 && nr_need_stop)
	  || (reorder_type == 1 && n_ready == nr_need_stop))
	{
	  schedule_stop (sched_verbose ? dump : NULL);
	  sched_data.last_was_stop = 1;
	  maybe_rotate (sched_verbose ? dump : NULL);
	  if (reorder_type == 1)
	    return 0;
	}
      else
	{
	  int deleted = 0;
	  insnp = e_ready;
	  /* Move down everything that needs a stop bit, preserving relative
	     order.  */
	  while (insnp-- > ready + deleted)
	    while (insnp >= ready + deleted)
	      {
		rtx insn = *insnp;
		if (! safe_group_barrier_needed_p (insn))
		  break;
		memmove (ready + 1, ready, (insnp - ready) * sizeof (rtx));
		*ready = insn;
		deleted++;
	      }
	  n_ready -= deleted;
	  ready += deleted;
	  if (deleted != nr_need_stop)
	    abort ();
	}
    }

  return itanium_reorder (sched_verbose ? dump : NULL,
			  ready, e_ready, reorder_type == 1);
}

static int
ia64_sched_reorder (dump, sched_verbose, ready, pn_ready, clock_var)
     FILE *dump;
     int sched_verbose;
     rtx *ready;
     int *pn_ready;
     int clock_var;
{
  return ia64_internal_sched_reorder (dump, sched_verbose, ready,
				      pn_ready, 0, clock_var);
}

/* Like ia64_sched_reorder, but called after issuing each insn.
   Override the default sort algorithm to better slot instructions.  */

static int
ia64_sched_reorder2 (dump, sched_verbose, ready, pn_ready, clock_var)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     rtx *ready;
     int *pn_ready;
     int clock_var;
{
  if (sched_data.last_was_stop)
    return 0;

  /* Detect one special case and try to optimize it.
     If we have 1.M;;MI 2.MIx, and slots 2.1 (M) and 2.2 (I) are both NOPs,
     then we can get better code by transforming this to 1.MFB;; 2.MIx.  */
  if (sched_data.first_slot == 1
      && sched_data.stopbit[0]
      && ((sched_data.cur == 4
	   && (sched_data.types[1] == TYPE_M || sched_data.types[1] == TYPE_A)
	   && (sched_data.types[2] == TYPE_I || sched_data.types[2] == TYPE_A)
	   && (sched_data.types[3] != TYPE_M && sched_data.types[3] != TYPE_A))
	  || (sched_data.cur == 3
	      && (sched_data.types[1] == TYPE_M
		  || sched_data.types[1] == TYPE_A)
	      && (sched_data.types[2] != TYPE_M
		  && sched_data.types[2] != TYPE_I
		  && sched_data.types[2] != TYPE_A))))
      
    {
      int i, best;
      rtx stop = sched_data.insns[1];

      /* Search backward for the stop bit that must be there.  */
      while (1)
	{
	  int insn_code;

	  stop = PREV_INSN (stop);
	  if (GET_CODE (stop) != INSN)
	    abort ();
	  insn_code = recog_memoized (stop);

	  /* Ignore .pred.rel.mutex.

	     ??? Update this to ignore cycle display notes too
	     ??? once those are implemented  */
	  if (insn_code == CODE_FOR_pred_rel_mutex
	      || insn_code == CODE_FOR_prologue_use)
	    continue;

	  if (insn_code == CODE_FOR_insn_group_barrier)
	    break;
	  abort ();
	}

      /* Adjust the stop bit's slot selector.  */
      if (INTVAL (XVECEXP (PATTERN (stop), 0, 0)) != 1)
	abort ();
      XVECEXP (PATTERN (stop), 0, 0) = GEN_INT (3);

      sched_data.stopbit[0] = 0;
      sched_data.stopbit[2] = 1;

      sched_data.types[5] = sched_data.types[3];
      sched_data.types[4] = sched_data.types[2];
      sched_data.types[3] = sched_data.types[1];
      sched_data.insns[5] = sched_data.insns[3];
      sched_data.insns[4] = sched_data.insns[2];
      sched_data.insns[3] = sched_data.insns[1];
      sched_data.stopbit[5] = sched_data.stopbit[4] = sched_data.stopbit[3] = 0;
      sched_data.cur += 2;
      sched_data.first_slot = 3;
      for (i = 0; i < NR_PACKETS; i++)
	{
	  const struct ia64_packet *p = packets + i;
	  if (p->t[0] == TYPE_M && p->t[1] == TYPE_F && p->t[2] == TYPE_B)
	    {
	      sched_data.packet = p;
	      break;
	    }
	}
      rotate_one_bundle (sched_verbose ? dump : NULL);

      best = 6;
      for (i = 0; i < NR_PACKETS; i++)
	{
	  const struct ia64_packet *p = packets + i;
	  int split = get_split (p, sched_data.first_slot);
	  int next;

	  /* Disallow multiway branches here.  */
	  if (p->t[1] == TYPE_B)
	    continue;

	  if (packet_matches_p (p, split, &next) && next < best)
	    {
	      best = next;
	      sched_data.packet = p;
	      sched_data.split = split;
	    }
	}
      if (best == 6)
	abort ();
    }

  if (*pn_ready > 0)
    {
      int more = ia64_internal_sched_reorder (dump, sched_verbose,
					      ready, pn_ready, 1,
					      clock_var);
      if (more)
	return more;
      /* Did we schedule a stop?  If so, finish this cycle.  */
      if (sched_data.cur == sched_data.first_slot)
	return 0;
    }

  if (sched_verbose)
    fprintf (dump, "//   Can't issue more this cycle; updating type array.\n");

  cycle_end_fill_slots (sched_verbose ? dump : NULL);
  if (sched_verbose)
    dump_current_packet (dump);
  return 0;
}

/* We are about to issue INSN.  Return the number of insns left on the
   ready queue that can be issued this cycle.  */

static int
ia64_variable_issue (dump, sched_verbose, insn, can_issue_more)
     FILE *dump;
     int sched_verbose;
     rtx insn;
     int can_issue_more ATTRIBUTE_UNUSED;
{
  enum attr_type t = ia64_safe_type (insn);

  if (sched_data.last_was_stop)
    {
      int t = sched_data.first_slot;
      if (t == 0)
	t = 3;
      ia64_emit_insn_before (gen_insn_group_barrier (GEN_INT (t)), insn);
      init_insn_group_barriers ();
      sched_data.last_was_stop = 0;
    }

  if (t == TYPE_UNKNOWN)
    {
      if (sched_verbose)
	fprintf (dump, "// Ignoring type %s\n", type_names[t]);
      if (GET_CODE (PATTERN (insn)) == ASM_INPUT
	  || asm_noperands (PATTERN (insn)) >= 0)
	{
	  /* This must be some kind of asm.  Clear the scheduling state.  */
	  rotate_two_bundles (sched_verbose ? dump : NULL);
	  if (ia64_final_schedule)
	    group_barrier_needed_p (insn);
	}
      return 1;
    }

  /* This is _not_ just a sanity check.  group_barrier_needed_p will update
     important state info.  Don't delete this test.  */
  if (ia64_final_schedule
      && group_barrier_needed_p (insn))
    abort ();

  sched_data.stopbit[sched_data.cur] = 0;
  sched_data.insns[sched_data.cur] = insn;
  sched_data.types[sched_data.cur] = t;

  sched_data.cur++;
  if (sched_verbose)
    fprintf (dump, "// Scheduling insn %d of type %s\n",
	     INSN_UID (insn), type_names[t]);

  if (GET_CODE (insn) == CALL_INSN && ia64_final_schedule)
    {
      schedule_stop (sched_verbose ? dump : NULL);
      sched_data.last_was_stop = 1;
    }

  return 1;
}

/* Free data allocated by ia64_sched_init.  */

static void
ia64_sched_finish (dump, sched_verbose)
     FILE *dump;
     int sched_verbose;
{
  if (sched_verbose)
    fprintf (dump, "// Finishing schedule.\n");
  rotate_two_bundles (NULL);
  free (sched_types);
  free (sched_ready);
}

/* Emit pseudo-ops for the assembler to describe predicate relations.
   At present this assumes that we only consider predicate pairs to
   be mutex, and that the assembler can deduce proper values from
   straight-line code.  */

static void
emit_predicate_relation_info ()
{
  basic_block bb;

  FOR_EACH_BB_REVERSE (bb)
    {
      int r;
      rtx head = bb->head;

      /* We only need such notes at code labels.  */
      if (GET_CODE (head) != CODE_LABEL)
	continue;
      if (GET_CODE (NEXT_INSN (head)) == NOTE
	  && NOTE_LINE_NUMBER (NEXT_INSN (head)) == NOTE_INSN_BASIC_BLOCK)
	head = NEXT_INSN (head);

      for (r = PR_REG (0); r < PR_REG (64); r += 2)
	if (REGNO_REG_SET_P (bb->global_live_at_start, r))
	  {
	    rtx p = gen_rtx_REG (BImode, r);
	    rtx n = emit_insn_after (gen_pred_rel_mutex (p), head);
	    if (head == bb->end)
	      bb->end = n;
	    head = n;
	  }
    }

  /* Look for conditional calls that do not return, and protect predicate
     relations around them.  Otherwise the assembler will assume the call
     returns, and complain about uses of call-clobbered predicates after
     the call.  */
  FOR_EACH_BB_REVERSE (bb)
    {
      rtx insn = bb->head;
      
      while (1)
	{
	  if (GET_CODE (insn) == CALL_INSN
	      && GET_CODE (PATTERN (insn)) == COND_EXEC
	      && find_reg_note (insn, REG_NORETURN, NULL_RTX))
	    {
	      rtx b = emit_insn_before (gen_safe_across_calls_all (), insn);
	      rtx a = emit_insn_after (gen_safe_across_calls_normal (), insn);
	      if (bb->head == insn)
		bb->head = b;
	      if (bb->end == insn)
		bb->end = a;
	    }
	  
	  if (insn == bb->end)
	    break;
	  insn = NEXT_INSN (insn);
	}
    }
}

/* Generate a NOP instruction of type T.  We will never generate L type
   nops.  */

static rtx
gen_nop_type (t)
     enum attr_type t;
{
  switch (t)
    {
    case TYPE_M:
      return gen_nop_m ();
    case TYPE_I:
      return gen_nop_i ();
    case TYPE_B:
      return gen_nop_b ();
    case TYPE_F:
      return gen_nop_f ();
    case TYPE_X:
      return gen_nop_x ();
    default:
      abort ();
    }
}

/* After the last scheduling pass, fill in NOPs.  It's easier to do this
   here than while scheduling.  */

static void
ia64_emit_nops ()
{
  rtx insn;
  const struct bundle *b = 0;
  int bundle_pos = 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat;
      enum attr_type t;
      pat = INSN_P (insn) ? PATTERN (insn) : const0_rtx;
      if (GET_CODE (pat) == USE || GET_CODE (pat) == CLOBBER)
	continue;
      if ((GET_CODE (pat) == UNSPEC && XINT (pat, 1) == UNSPEC_BUNDLE_SELECTOR)
	  || GET_CODE (insn) == CODE_LABEL)
	{
	  if (b)
	    while (bundle_pos < 3)
	      {
		emit_insn_before (gen_nop_type (b->t[bundle_pos]), insn);
		bundle_pos++;
	      }
	  if (GET_CODE (insn) != CODE_LABEL)
	    b = bundle + INTVAL (XVECEXP (pat, 0, 0));
	  else
	    b = 0;
	  bundle_pos = 0;
	  continue;
	}
      else if (GET_CODE (pat) == UNSPEC_VOLATILE
	       && XINT (pat, 1) == UNSPECV_INSN_GROUP_BARRIER)
	{
	  int t = INTVAL (XVECEXP (pat, 0, 0));
	  if (b)
	    while (bundle_pos < t)
	      {
		emit_insn_before (gen_nop_type (b->t[bundle_pos]), insn);
		bundle_pos++;
	      }
	  continue;
	}

      if (bundle_pos == 3)
	b = 0;

      if (b && INSN_P (insn))
	{
	  t = ia64_safe_type (insn);
	  if (asm_noperands (PATTERN (insn)) >= 0
	      || GET_CODE (PATTERN (insn)) == ASM_INPUT)
	    {
	      while (bundle_pos < 3)
		{
		  if (b->t[bundle_pos] != TYPE_L)
		    emit_insn_before (gen_nop_type (b->t[bundle_pos]), insn);
		  bundle_pos++;
		}
	      continue;
	    }

	  if (t == TYPE_UNKNOWN)
	    continue;
	  while (bundle_pos < 3)
	    {
	      if (t == b->t[bundle_pos]
		  || (t == TYPE_A && (b->t[bundle_pos] == TYPE_M
				      || b->t[bundle_pos] == TYPE_I)))
		break;

	      emit_insn_before (gen_nop_type (b->t[bundle_pos]), insn);
	      bundle_pos++;
	    }
	  if (bundle_pos < 3)
	    bundle_pos++;
	}
    }
}

/* Perform machine dependent operations on the rtl chain INSNS.  */

void
ia64_reorg (insns)
     rtx insns;
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  /* If optimizing, we'll have split before scheduling.  */
  if (optimize == 0)
    split_all_insns (0);

  /* ??? update_life_info_in_dirty_blocks fails to terminate during
     non-optimizing bootstrap.  */
  update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES, PROP_DEATH_NOTES);

  if (ia64_flag_schedule_insns2)
    {
      timevar_push (TV_SCHED2);
      ia64_final_schedule = 1;
      schedule_ebbs (rtl_dump_file);
      ia64_final_schedule = 0;
      timevar_pop (TV_SCHED2);

      /* This relies on the NOTE_INSN_BASIC_BLOCK notes to be in the same
	 place as they were during scheduling.  */
      emit_insn_group_barriers (rtl_dump_file, insns);
      ia64_emit_nops ();
    }
  else
    emit_all_insn_group_barriers (rtl_dump_file, insns);

  /* A call must not be the last instruction in a function, so that the
     return address is still within the function, so that unwinding works
     properly.  Note that IA-64 differs from dwarf2 on this point.  */
  if (flag_unwind_tables || (flag_exceptions && !USING_SJLJ_EXCEPTIONS))
    {
      rtx insn;
      int saw_stop = 0;

      insn = get_last_insn ();
      if (! INSN_P (insn))
        insn = prev_active_insn (insn);
      /* Skip over insns that expand to nothing.  */
      while (GET_CODE (insn) == INSN && get_attr_empty (insn) == EMPTY_YES)
        {
	  if (GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
	      && XINT (PATTERN (insn), 1) == UNSPECV_INSN_GROUP_BARRIER)
	    saw_stop = 1;
	  insn = prev_active_insn (insn);
	}
      if (GET_CODE (insn) == CALL_INSN)
	{
	  if (! saw_stop)
	    emit_insn (gen_insn_group_barrier (GEN_INT (3)));
	  emit_insn (gen_break_f ());
	  emit_insn (gen_insn_group_barrier (GEN_INT (3)));
	}
    }

  fixup_errata ();
  emit_predicate_relation_info ();
}

/* Return true if REGNO is used by the epilogue.  */

int
ia64_epilogue_uses (regno)
     int regno;
{
  switch (regno)
    {
    case R_GR (1):
      /* With a call to a function in another module, we will write a new
	 value to "gp".  After returning from such a call, we need to make
	 sure the function restores the original gp-value, even if the
	 function itself does not use the gp anymore.  */
      return !(TARGET_AUTO_PIC || TARGET_NO_PIC);

    case IN_REG (0): case IN_REG (1): case IN_REG (2): case IN_REG (3):
    case IN_REG (4): case IN_REG (5): case IN_REG (6): case IN_REG (7):
      /* For functions defined with the syscall_linkage attribute, all
	 input registers are marked as live at all function exits.  This
	 prevents the register allocator from using the input registers,
	 which in turn makes it possible to restart a system call after
	 an interrupt without having to save/restore the input registers.
	 This also prevents kernel data from leaking to application code.  */
      return lookup_attribute ("syscall_linkage",
	   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))) != NULL;

    case R_BR (0):
      /* Conditional return patterns can't represent the use of `b0' as
         the return address, so we force the value live this way.  */
      return 1;

    case AR_PFS_REGNUM:
      /* Likewise for ar.pfs, which is used by br.ret.  */
      return 1;

    default:
      return 0;
    }
}

/* Return true if REGNO is used by the frame unwinder.  */

int
ia64_eh_uses (regno)
     int regno;
{
  if (! reload_completed)
    return 0;

  if (current_frame_info.reg_save_b0
      && regno == current_frame_info.reg_save_b0)
    return 1;
  if (current_frame_info.reg_save_pr
      && regno == current_frame_info.reg_save_pr)
    return 1;
  if (current_frame_info.reg_save_ar_pfs
      && regno == current_frame_info.reg_save_ar_pfs)
    return 1;
  if (current_frame_info.reg_save_ar_unat
      && regno == current_frame_info.reg_save_ar_unat)
    return 1;
  if (current_frame_info.reg_save_ar_lc
      && regno == current_frame_info.reg_save_ar_lc)
    return 1;

  return 0;
}

/* For ia64, SYMBOL_REF_FLAG set means that it is a function.

   We add @ to the name if this goes in small data/bss.  We can only put
   a variable in small data/bss if it is defined in this module or a module
   that we are statically linked with.  We can't check the second condition,
   but TREE_STATIC gives us the first one.  */

/* ??? If we had IPA, we could check the second condition.  We could support
   programmer added section attributes if the variable is not defined in this
   module.  */

/* ??? See the v850 port for a cleaner way to do this.  */

/* ??? We could also support own long data here.  Generating movl/add/ld8
   instead of addl,ld8/ld8.  This makes the code bigger, but should make the
   code faster because there is one less load.  This also includes incomplete
   types which can't go in sdata/sbss.  */

static bool
ia64_in_small_data_p (exp)
     tree exp;
{
  if (TARGET_NO_SDATA)
    return false;

  /* Functions are never small data.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));
      if (strcmp (section, ".sdata") == 0
	  || strcmp (section, ".sbss") == 0)
	return true;
    }
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
	 in sdata because it might be too big when completed.  */
      if (size > 0 && size <= ia64_section_threshold)
	return true;
    }

  return false;
}

static void
ia64_encode_section_info (decl, first)
     tree decl;
     int first ATTRIBUTE_UNUSED;
{
  const char *symbol_str;
  bool is_local;
  rtx symbol;
  char encoding = 0;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
      return;
    }

  /* Careful not to prod global register variables.  */
  if (TREE_CODE (decl) != VAR_DECL
      || GET_CODE (DECL_RTL (decl)) != MEM
      || GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
    return;

  symbol = XEXP (DECL_RTL (decl), 0);
  symbol_str = XSTR (symbol, 0);

  is_local = (*targetm.binds_local_p) (decl);

  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL (decl))
    encoding = " GLil"[decl_tls_model (decl)];
  /* Determine if DECL will wind up in .sdata/.sbss.  */
  else if (is_local && ia64_in_small_data_p (decl))
    encoding = 's';

  /* Finally, encode this into the symbol string.  */
  if (encoding)
    {
      char *newstr;
      size_t len;

      if (symbol_str[0] == ENCODE_SECTION_INFO_CHAR)
	{
	  if (encoding == symbol_str[1])
	    return;
	  /* ??? Sdata became thread or thread becaome not thread.  Lose.  */
	  if (encoding == 's' || symbol_str[1] == 's')
	    abort ();
	}

      len = strlen (symbol_str);
      newstr = alloca (len + 3);
      newstr[0] = ENCODE_SECTION_INFO_CHAR;
      newstr[1] = encoding;
      memcpy (newstr + 2, symbol_str, len + 1);

      XSTR (symbol, 0) = ggc_alloc_string (newstr, len + 2);
    }

  /* This decl is marked as being in small data/bss but it shouldn't be;
     one likely explanation for this is that the decl has been moved into
     a different section from the one it was in when encode_section_info
     was first called.  Remove the encoding.  */
  else if (symbol_str[0] == ENCODE_SECTION_INFO_CHAR)
    XSTR (symbol, 0) = ggc_strdup (symbol_str + 2);
}

static const char *
ia64_strip_name_encoding (str)
     const char *str;
{
  if (str[0] == ENCODE_SECTION_INFO_CHAR)
    str += 2;
  if (str[0] == '*')
    str++;
  return str;
}

/* True if it is OK to do sibling call optimization for the specified
   call expression EXP.  DECL will be the called function, or NULL if
   this is an indirect call.  */
bool
ia64_function_ok_for_sibcall (decl)
     tree decl;
{
  /* We can't perform a sibcall if the current function has the syscall_linkage
     attribute.  */
  if (lookup_attribute ("syscall_linkage",
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return false;

  /* We must always return with our current GP.  This means we can
     only sibcall to functions defined in the current module.  */
  return decl && (*targetm.binds_local_p) (decl);
}

/* Output assembly directives for prologue regions.  */

/* The current basic block number.  */

static bool last_block;

/* True if we need a copy_state command at the start of the next block.  */

static bool need_copy_state;

/* The function emits unwind directives for the start of an epilogue.  */

static void
process_epilogue ()
{
  /* If this isn't the last block of the function, then we need to label the
     current state, and copy it back in at the start of the next block.  */

  if (!last_block)
    {
      fprintf (asm_out_file, "\t.label_state 1\n");
      need_copy_state = true;
    }

  fprintf (asm_out_file, "\t.restore sp\n");
}

/* This function processes a SET pattern looking for specific patterns
   which result in emitting an assembly directive required for unwinding.  */

static int
process_set (asm_out_file, pat)
     FILE *asm_out_file;
     rtx pat;
{
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);
  int src_regno, dest_regno;

  /* Look for the ALLOC insn.  */
  if (GET_CODE (src) == UNSPEC_VOLATILE
      && XINT (src, 1) == UNSPECV_ALLOC
      && GET_CODE (dest) == REG)
    {
      dest_regno = REGNO (dest);

      /* If this is the final destination for ar.pfs, then this must
	 be the alloc in the prologue.  */
      if (dest_regno == current_frame_info.reg_save_ar_pfs)
	fprintf (asm_out_file, "\t.save ar.pfs, r%d\n",
		 ia64_dbx_register_number (dest_regno));
      else
	{
	  /* This must be an alloc before a sibcall.  We must drop the
	     old frame info.  The easiest way to drop the old frame
	     info is to ensure we had a ".restore sp" directive
	     followed by a new prologue.  If the procedure doesn't
	     have a memory-stack frame, we'll issue a dummy ".restore
	     sp" now.  */
	  if (current_frame_info.total_size == 0 && !frame_pointer_needed)
	    /* if haven't done process_epilogue() yet, do it now */
	    process_epilogue ();
	  fprintf (asm_out_file, "\t.prologue\n");
	}
      return 1;
    }

  /* Look for SP = ....  */
  if (GET_CODE (dest) == REG && REGNO (dest) == STACK_POINTER_REGNUM)
    {
      if (GET_CODE (src) == PLUS)
        {
	  rtx op0 = XEXP (src, 0);
	  rtx op1 = XEXP (src, 1);
	  if (op0 == dest && GET_CODE (op1) == CONST_INT)
	    {
	      if (INTVAL (op1) < 0)
		{
		  fputs ("\t.fframe ", asm_out_file);
		  fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC,
			   -INTVAL (op1));
		  fputc ('\n', asm_out_file);
		}
	      else
		process_epilogue ();
	    }
	  else
	    abort ();
	}
      else if (GET_CODE (src) == REG
	       && REGNO (src) == HARD_FRAME_POINTER_REGNUM)
	process_epilogue ();
      else
	abort ();

      return 1;
    }

  /* Register move we need to look at.  */
  if (GET_CODE (dest) == REG && GET_CODE (src) == REG)
    {
      src_regno = REGNO (src);
      dest_regno = REGNO (dest);

      switch (src_regno)
	{
	case BR_REG (0):
	  /* Saving return address pointer.  */
	  if (dest_regno != current_frame_info.reg_save_b0)
	    abort ();
	  fprintf (asm_out_file, "\t.save rp, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case PR_REG (0):
	  if (dest_regno != current_frame_info.reg_save_pr)
	    abort ();
	  fprintf (asm_out_file, "\t.save pr, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case AR_UNAT_REGNUM:
	  if (dest_regno != current_frame_info.reg_save_ar_unat)
	    abort ();
	  fprintf (asm_out_file, "\t.save ar.unat, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case AR_LC_REGNUM:
	  if (dest_regno != current_frame_info.reg_save_ar_lc)
	    abort ();
	  fprintf (asm_out_file, "\t.save ar.lc, r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	case STACK_POINTER_REGNUM:
	  if (dest_regno != HARD_FRAME_POINTER_REGNUM
	      || ! frame_pointer_needed)
	    abort ();
	  fprintf (asm_out_file, "\t.vframe r%d\n",
		   ia64_dbx_register_number (dest_regno));
	  return 1;

	default:
	  /* Everything else should indicate being stored to memory.  */
	  abort ();
	}
    }

  /* Memory store we need to look at.  */
  if (GET_CODE (dest) == MEM && GET_CODE (src) == REG)
    {
      long off;
      rtx base;
      const char *saveop;

      if (GET_CODE (XEXP (dest, 0)) == REG)
	{
	  base = XEXP (dest, 0);
	  off = 0;
	}
      else if (GET_CODE (XEXP (dest, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (dest, 0), 1)) == CONST_INT)
	{
	  base = XEXP (XEXP (dest, 0), 0);
	  off = INTVAL (XEXP (XEXP (dest, 0), 1));
	}
      else
	abort ();

      if (base == hard_frame_pointer_rtx)
	{
	  saveop = ".savepsp";
	  off = - off;
	}
      else if (base == stack_pointer_rtx)
	saveop = ".savesp";
      else
	abort ();

      src_regno = REGNO (src);
      switch (src_regno)
	{
	case BR_REG (0):
	  if (current_frame_info.reg_save_b0 != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s rp, %ld\n", saveop, off);
	  return 1;

	case PR_REG (0):
	  if (current_frame_info.reg_save_pr != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s pr, %ld\n", saveop, off);
	  return 1;

	case AR_LC_REGNUM:
	  if (current_frame_info.reg_save_ar_lc != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.lc, %ld\n", saveop, off);
	  return 1;

	case AR_PFS_REGNUM:
	  if (current_frame_info.reg_save_ar_pfs != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.pfs, %ld\n", saveop, off);
	  return 1;

	case AR_UNAT_REGNUM:
	  if (current_frame_info.reg_save_ar_unat != 0)
	    abort ();
	  fprintf (asm_out_file, "\t%s ar.unat, %ld\n", saveop, off);
	  return 1;

	case GR_REG (4):
	case GR_REG (5):
	case GR_REG (6):
	case GR_REG (7):
	  fprintf (asm_out_file, "\t.save.g 0x%x\n",
		   1 << (src_regno - GR_REG (4)));
	  return 1;

	case BR_REG (1):
	case BR_REG (2):
	case BR_REG (3):
	case BR_REG (4):
	case BR_REG (5):
	  fprintf (asm_out_file, "\t.save.b 0x%x\n",
		   1 << (src_regno - BR_REG (1)));
	  return 1;

	case FR_REG (2):
	case FR_REG (3):
	case FR_REG (4):
	case FR_REG (5):
	  fprintf (asm_out_file, "\t.save.f 0x%x\n",
		   1 << (src_regno - FR_REG (2)));
	  return 1;

	case FR_REG (16): case FR_REG (17): case FR_REG (18): case FR_REG (19):
	case FR_REG (20): case FR_REG (21): case FR_REG (22): case FR_REG (23):
	case FR_REG (24): case FR_REG (25): case FR_REG (26): case FR_REG (27):
	case FR_REG (28): case FR_REG (29): case FR_REG (30): case FR_REG (31):
	  fprintf (asm_out_file, "\t.save.gf 0x0, 0x%x\n",
		   1 << (src_regno - FR_REG (12)));
	  return 1;

	default:
	  return 0;
	}
    }

  return 0;
}


/* This function looks at a single insn and emits any directives
   required to unwind this insn.  */
void
process_for_unwind_directive (asm_out_file, insn)
     FILE *asm_out_file;
     rtx insn;
{
  if (flag_unwind_tables
      || (flag_exceptions && !USING_SJLJ_EXCEPTIONS))
    {
      rtx pat;

      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BASIC_BLOCK)
	{
	  last_block = NOTE_BASIC_BLOCK (insn)->next_bb == EXIT_BLOCK_PTR;

	  /* Restore unwind state from immediately before the epilogue.  */
	  if (need_copy_state)
	    {
	      fprintf (asm_out_file, "\t.body\n");
	      fprintf (asm_out_file, "\t.copy_state 1\n");
	      need_copy_state = false;
	    }
	}

      if (GET_CODE (insn) == NOTE || ! RTX_FRAME_RELATED_P (insn))
	return;

      pat = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
      if (pat)
	pat = XEXP (pat, 0);
      else
	pat = PATTERN (insn);

      switch (GET_CODE (pat))
        {
	case SET:
	  process_set (asm_out_file, pat);
	  break;

	case PARALLEL:
	  {
	    int par_index;
	    int limit = XVECLEN (pat, 0);
	    for (par_index = 0; par_index < limit; par_index++)
	      {
		rtx x = XVECEXP (pat, 0, par_index);
		if (GET_CODE (x) == SET)
		  process_set (asm_out_file, x);
	      }
	    break;
	  }

	default:
	  abort ();
	}
    }
}


void
ia64_init_builtins ()
{
  tree psi_type_node = build_pointer_type (integer_type_node);
  tree pdi_type_node = build_pointer_type (long_integer_type_node);

  /* __sync_val_compare_and_swap_si, __sync_bool_compare_and_swap_si */
  tree si_ftype_psi_si_si
    = build_function_type_list (integer_type_node,
				psi_type_node, integer_type_node,
				integer_type_node, NULL_TREE);

  /* __sync_val_compare_and_swap_di */
  tree di_ftype_pdi_di_di
    = build_function_type_list (long_integer_type_node,
				pdi_type_node, long_integer_type_node,
				long_integer_type_node, NULL_TREE);
  /* __sync_bool_compare_and_swap_di */
  tree si_ftype_pdi_di_di
    = build_function_type_list (integer_type_node,
				pdi_type_node, long_integer_type_node,
				long_integer_type_node, NULL_TREE);
  /* __sync_synchronize */
  tree void_ftype_void
    = build_function_type (void_type_node, void_list_node);

  /* __sync_lock_test_and_set_si */
  tree si_ftype_psi_si
    = build_function_type_list (integer_type_node,
				psi_type_node, integer_type_node, NULL_TREE);

  /* __sync_lock_test_and_set_di */
  tree di_ftype_pdi_di
    = build_function_type_list (long_integer_type_node,
				pdi_type_node, long_integer_type_node,
				NULL_TREE);

  /* __sync_lock_release_si */
  tree void_ftype_psi
    = build_function_type_list (void_type_node, psi_type_node, NULL_TREE);

  /* __sync_lock_release_di */
  tree void_ftype_pdi
    = build_function_type_list (void_type_node, pdi_type_node, NULL_TREE);

#define def_builtin(name, type, code) \
  builtin_function ((name), (type), (code), BUILT_IN_MD, NULL, NULL_TREE)

  def_builtin ("__sync_val_compare_and_swap_si", si_ftype_psi_si_si,
	       IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI);
  def_builtin ("__sync_val_compare_and_swap_di", di_ftype_pdi_di_di,
	       IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI);
  def_builtin ("__sync_bool_compare_and_swap_si", si_ftype_psi_si_si,
	       IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI);
  def_builtin ("__sync_bool_compare_and_swap_di", si_ftype_pdi_di_di,
	       IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI);

  def_builtin ("__sync_synchronize", void_ftype_void,
	       IA64_BUILTIN_SYNCHRONIZE);

  def_builtin ("__sync_lock_test_and_set_si", si_ftype_psi_si,
	       IA64_BUILTIN_LOCK_TEST_AND_SET_SI);
  def_builtin ("__sync_lock_test_and_set_di", di_ftype_pdi_di,
	       IA64_BUILTIN_LOCK_TEST_AND_SET_DI);
  def_builtin ("__sync_lock_release_si", void_ftype_psi,
	       IA64_BUILTIN_LOCK_RELEASE_SI);
  def_builtin ("__sync_lock_release_di", void_ftype_pdi,
	       IA64_BUILTIN_LOCK_RELEASE_DI);

  def_builtin ("__builtin_ia64_bsp",
	       build_function_type (ptr_type_node, void_list_node),
	       IA64_BUILTIN_BSP);

  def_builtin ("__builtin_ia64_flushrs", 
	       build_function_type (void_type_node, void_list_node), 
	       IA64_BUILTIN_FLUSHRS);

  def_builtin ("__sync_fetch_and_add_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_ADD_SI);
  def_builtin ("__sync_fetch_and_sub_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_SUB_SI);
  def_builtin ("__sync_fetch_and_or_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_OR_SI);
  def_builtin ("__sync_fetch_and_and_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_AND_SI);
  def_builtin ("__sync_fetch_and_xor_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_XOR_SI);
  def_builtin ("__sync_fetch_and_nand_si", si_ftype_psi_si,
	       IA64_BUILTIN_FETCH_AND_NAND_SI);

  def_builtin ("__sync_add_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_ADD_AND_FETCH_SI);
  def_builtin ("__sync_sub_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_SUB_AND_FETCH_SI);
  def_builtin ("__sync_or_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_OR_AND_FETCH_SI);
  def_builtin ("__sync_and_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_AND_AND_FETCH_SI);
  def_builtin ("__sync_xor_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_XOR_AND_FETCH_SI);
  def_builtin ("__sync_nand_and_fetch_si", si_ftype_psi_si,
	       IA64_BUILTIN_NAND_AND_FETCH_SI);

  def_builtin ("__sync_fetch_and_add_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_ADD_DI);
  def_builtin ("__sync_fetch_and_sub_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_SUB_DI);
  def_builtin ("__sync_fetch_and_or_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_OR_DI);
  def_builtin ("__sync_fetch_and_and_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_AND_DI);
  def_builtin ("__sync_fetch_and_xor_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_XOR_DI);
  def_builtin ("__sync_fetch_and_nand_di", di_ftype_pdi_di,
	       IA64_BUILTIN_FETCH_AND_NAND_DI);

  def_builtin ("__sync_add_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_ADD_AND_FETCH_DI);
  def_builtin ("__sync_sub_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_SUB_AND_FETCH_DI);
  def_builtin ("__sync_or_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_OR_AND_FETCH_DI);
  def_builtin ("__sync_and_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_AND_AND_FETCH_DI);
  def_builtin ("__sync_xor_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_XOR_AND_FETCH_DI);
  def_builtin ("__sync_nand_and_fetch_di", di_ftype_pdi_di,
	       IA64_BUILTIN_NAND_AND_FETCH_DI);

#undef def_builtin
}

/* Expand fetch_and_op intrinsics.  The basic code sequence is:

     mf
     tmp = [ptr];
     do {
       ret = tmp;
       ar.ccv = tmp;
       tmp <op>= value;
       cmpxchgsz.acq tmp = [ptr], tmp
     } while (tmp != ret)
*/

static rtx
ia64_expand_fetch_and_op (binoptab, mode, arglist, target)
     optab binoptab;
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  rtx ret, label, tmp, ccv, insn, mem, value;
  tree arg0, arg1;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
#ifdef POINTERS_EXTEND_UNSIGNED
  if (GET_MODE(mem) != Pmode)
    mem = convert_memory_address (Pmode, mem);
#endif
  value = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (target && register_operand (target, mode))
    ret = target;
  else
    ret = gen_reg_rtx (mode);

  emit_insn (gen_mf ());

  /* Special case for fetchadd instructions.  */
  if (binoptab == add_optab && fetchadd_operand (value, VOIDmode))
    {
      if (mode == SImode)
        insn = gen_fetchadd_acq_si (ret, mem, value);
      else
        insn = gen_fetchadd_acq_di (ret, mem, value);
      emit_insn (insn);
      return ret;
    }

  tmp = gen_reg_rtx (mode);
  /* ar.ccv must always be loaded with a zero-extended DImode value.  */
  ccv = gen_rtx_REG (DImode, AR_CCV_REGNUM);
  emit_move_insn (tmp, mem);

  label = gen_label_rtx ();
  emit_label (label);
  emit_move_insn (ret, tmp);
  convert_move (ccv, tmp, /*unsignedp=*/1);

  /* Perform the specific operation.  Special case NAND by noticing
     one_cmpl_optab instead.  */
  if (binoptab == one_cmpl_optab)
    {
      tmp = expand_unop (mode, binoptab, tmp, NULL, OPTAB_WIDEN);
      binoptab = and_optab;
    }
  tmp = expand_binop (mode, binoptab, tmp, value, tmp, 1, OPTAB_WIDEN);

  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, tmp, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, tmp, ccv);
  emit_insn (insn);

  emit_cmp_and_jump_insns (tmp, ret, NE, 0, mode, 1, label);

  return ret;
}

/* Expand op_and_fetch intrinsics.  The basic code sequence is:

     mf
     tmp = [ptr];
     do {
       old = tmp;
       ar.ccv = tmp;
       ret = tmp <op> value;
       cmpxchgsz.acq tmp = [ptr], ret
     } while (tmp != old)
*/

static rtx
ia64_expand_op_and_fetch (binoptab, mode, arglist, target)
     optab binoptab;
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  rtx old, label, tmp, ret, ccv, insn, mem, value;
  tree arg0, arg1;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, Pmode, 0);
#ifdef POINTERS_EXTEND_UNSIGNED
  if (GET_MODE(mem) != Pmode)
    mem = convert_memory_address (Pmode, mem);
#endif

  value = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (Pmode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (target && ! register_operand (target, mode))
    target = NULL_RTX;

  emit_insn (gen_mf ());
  tmp = gen_reg_rtx (mode);
  old = gen_reg_rtx (mode);
  /* ar.ccv must always be loaded with a zero-extended DImode value.  */        
  ccv = gen_rtx_REG (DImode, AR_CCV_REGNUM);

  emit_move_insn (tmp, mem);

  label = gen_label_rtx ();
  emit_label (label);
  emit_move_insn (old, tmp);
  convert_move (ccv, tmp, /*unsignedp=*/1);

  /* Perform the specific operation.  Special case NAND by noticing
     one_cmpl_optab instead.  */
  if (binoptab == one_cmpl_optab)
    {
      tmp = expand_unop (mode, binoptab, tmp, NULL, OPTAB_WIDEN);
      binoptab = and_optab;
    }
  ret = expand_binop (mode, binoptab, tmp, value, target, 1, OPTAB_WIDEN);

  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, ret, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, ret, ccv);
  emit_insn (insn);

  emit_cmp_and_jump_insns (tmp, old, NE, 0, mode, 1, label);

  return ret;
}

/* Expand val_ and bool_compare_and_swap.  For val_ we want:

     ar.ccv = oldval
     mf
     cmpxchgsz.acq ret = [ptr], newval, ar.ccv
     return ret

   For bool_ it's the same except return ret == oldval.
*/

static rtx
ia64_expand_compare_and_swap (rmode, mode, boolp, arglist, target)
     enum machine_mode rmode;
     enum machine_mode mode;
     int boolp;
     tree arglist;
     rtx target;
{
  tree arg0, arg1, arg2;
  rtx mem, old, new, ccv, tmp, insn;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  mem = expand_expr (arg0, NULL_RTX, ptr_mode, 0);
  old = expand_expr (arg1, NULL_RTX, mode, 0);
  new = expand_expr (arg2, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (ptr_mode, mem));
  MEM_VOLATILE_P (mem) = 1;

  if (GET_MODE (old) != mode)
    old = convert_to_mode (mode, old, /*unsignedp=*/1);
  if (GET_MODE (new) != mode)
    new = convert_to_mode (mode, new, /*unsignedp=*/1);

  if (! register_operand (old, mode))
    old = copy_to_mode_reg (mode, old);
  if (! register_operand (new, mode))
    new = copy_to_mode_reg (mode, new);

  if (! boolp && target && register_operand (target, mode))
    tmp = target;
  else
    tmp = gen_reg_rtx (mode);

  ccv = gen_rtx_REG (DImode, AR_CCV_REGNUM);
  convert_move (ccv, old, /*unsignedp=*/1);
  emit_insn (gen_mf ());
  if (mode == SImode)
    insn = gen_cmpxchg_acq_si (tmp, mem, new, ccv);
  else
    insn = gen_cmpxchg_acq_di (tmp, mem, new, ccv);
  emit_insn (insn);

  if (boolp)
    {
      if (! target)
	target = gen_reg_rtx (rmode);
      return emit_store_flag_force (target, EQ, tmp, old, mode, 1, 1);
    }
  else
    return tmp;
}

/* Expand lock_test_and_set.  I.e. `xchgsz ret = [ptr], new'.  */

static rtx
ia64_expand_lock_test_and_set (mode, arglist, target)
     enum machine_mode mode;
     tree arglist;
     rtx target;
{
  tree arg0, arg1;
  rtx mem, new, ret, insn;

  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  mem = expand_expr (arg0, NULL_RTX, ptr_mode, 0);
  new = expand_expr (arg1, NULL_RTX, mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (ptr_mode, mem));
  MEM_VOLATILE_P (mem) = 1;
  if (! register_operand (new, mode))
    new = copy_to_mode_reg (mode, new);

  if (target && register_operand (target, mode))
    ret = target;
  else
    ret = gen_reg_rtx (mode);

  if (mode == SImode)
    insn = gen_xchgsi (ret, mem, new);
  else
    insn = gen_xchgdi (ret, mem, new);
  emit_insn (insn);

  return ret;
}

/* Expand lock_release.  I.e. `stsz.rel [ptr] = r0'.  */

static rtx
ia64_expand_lock_release (mode, arglist, target)
     enum machine_mode mode;
     tree arglist;
     rtx target ATTRIBUTE_UNUSED;
{
  tree arg0;
  rtx mem;

  arg0 = TREE_VALUE (arglist);
  mem = expand_expr (arg0, NULL_RTX, ptr_mode, 0);

  mem = gen_rtx_MEM (mode, force_reg (ptr_mode, mem));
  MEM_VOLATILE_P (mem) = 1;

  emit_move_insn (mem, const0_rtx);

  return const0_rtx;
}

rtx
ia64_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arglist = TREE_OPERAND (exp, 1);
  enum machine_mode rmode = VOIDmode;

  switch (fcode)
    {
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI:
      mode = SImode;
      rmode = SImode;
      break;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_SI:
    case IA64_BUILTIN_LOCK_RELEASE_SI:
    case IA64_BUILTIN_FETCH_AND_ADD_SI:
    case IA64_BUILTIN_FETCH_AND_SUB_SI:
    case IA64_BUILTIN_FETCH_AND_OR_SI:
    case IA64_BUILTIN_FETCH_AND_AND_SI:
    case IA64_BUILTIN_FETCH_AND_XOR_SI:
    case IA64_BUILTIN_FETCH_AND_NAND_SI:
    case IA64_BUILTIN_ADD_AND_FETCH_SI:
    case IA64_BUILTIN_SUB_AND_FETCH_SI:
    case IA64_BUILTIN_OR_AND_FETCH_SI:
    case IA64_BUILTIN_AND_AND_FETCH_SI:
    case IA64_BUILTIN_XOR_AND_FETCH_SI:
    case IA64_BUILTIN_NAND_AND_FETCH_SI:
      mode = SImode;
      break;

    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI:
      mode = DImode;
      rmode = SImode;
      break;

    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI:
      mode = DImode;
      rmode = DImode;
      break;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_DI:
    case IA64_BUILTIN_LOCK_RELEASE_DI:
    case IA64_BUILTIN_FETCH_AND_ADD_DI:
    case IA64_BUILTIN_FETCH_AND_SUB_DI:
    case IA64_BUILTIN_FETCH_AND_OR_DI:
    case IA64_BUILTIN_FETCH_AND_AND_DI:
    case IA64_BUILTIN_FETCH_AND_XOR_DI:
    case IA64_BUILTIN_FETCH_AND_NAND_DI:
    case IA64_BUILTIN_ADD_AND_FETCH_DI:
    case IA64_BUILTIN_SUB_AND_FETCH_DI:
    case IA64_BUILTIN_OR_AND_FETCH_DI:
    case IA64_BUILTIN_AND_AND_FETCH_DI:
    case IA64_BUILTIN_XOR_AND_FETCH_DI:
    case IA64_BUILTIN_NAND_AND_FETCH_DI:
      mode = DImode;
      break;

    default:
      break;
    }

  switch (fcode)
    {
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (rmode, mode, 1, arglist,
					   target);

    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI:
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (rmode, mode, 0, arglist,
					   target);

    case IA64_BUILTIN_SYNCHRONIZE:
      emit_insn (gen_mf ());
      return const0_rtx;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_SI:
    case IA64_BUILTIN_LOCK_TEST_AND_SET_DI:
      return ia64_expand_lock_test_and_set (mode, arglist, target);

    case IA64_BUILTIN_LOCK_RELEASE_SI:
    case IA64_BUILTIN_LOCK_RELEASE_DI:
      return ia64_expand_lock_release (mode, arglist, target);

    case IA64_BUILTIN_BSP:
      if (! target || ! register_operand (target, DImode))
	target = gen_reg_rtx (DImode);
      emit_insn (gen_bsp_value (target));
      return target;

    case IA64_BUILTIN_FLUSHRS:
      emit_insn (gen_flushrs ());
      return const0_rtx;

    case IA64_BUILTIN_FETCH_AND_ADD_SI:
    case IA64_BUILTIN_FETCH_AND_ADD_DI:
      return ia64_expand_fetch_and_op (add_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_SUB_SI:
    case IA64_BUILTIN_FETCH_AND_SUB_DI:
      return ia64_expand_fetch_and_op (sub_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_OR_SI:
    case IA64_BUILTIN_FETCH_AND_OR_DI:
      return ia64_expand_fetch_and_op (ior_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_AND_SI:
    case IA64_BUILTIN_FETCH_AND_AND_DI:
      return ia64_expand_fetch_and_op (and_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_XOR_SI:
    case IA64_BUILTIN_FETCH_AND_XOR_DI:
      return ia64_expand_fetch_and_op (xor_optab, mode, arglist, target);

    case IA64_BUILTIN_FETCH_AND_NAND_SI:
    case IA64_BUILTIN_FETCH_AND_NAND_DI:
      return ia64_expand_fetch_and_op (one_cmpl_optab, mode, arglist, target);

    case IA64_BUILTIN_ADD_AND_FETCH_SI:
    case IA64_BUILTIN_ADD_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (add_optab, mode, arglist, target);

    case IA64_BUILTIN_SUB_AND_FETCH_SI:
    case IA64_BUILTIN_SUB_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (sub_optab, mode, arglist, target);

    case IA64_BUILTIN_OR_AND_FETCH_SI:
    case IA64_BUILTIN_OR_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (ior_optab, mode, arglist, target);

    case IA64_BUILTIN_AND_AND_FETCH_SI:
    case IA64_BUILTIN_AND_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (and_optab, mode, arglist, target);

    case IA64_BUILTIN_XOR_AND_FETCH_SI:
    case IA64_BUILTIN_XOR_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (xor_optab, mode, arglist, target);

    case IA64_BUILTIN_NAND_AND_FETCH_SI:
    case IA64_BUILTIN_NAND_AND_FETCH_DI:
      return ia64_expand_op_and_fetch (one_cmpl_optab, mode, arglist, target);

    default:
      break;
    }

  return NULL_RTX;
}

/* For the HP-UX IA64 aggregate parameters are passed stored in the
   most significant bits of the stack slot.  */

enum direction
ia64_hpux_function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
   /* Exception to normal case for structures/unions/etc.  */

   if (type && AGGREGATE_TYPE_P (type)
       && int_size_in_bytes (type) < UNITS_PER_WORD)
     return upward;

   /* This is the standard FUNCTION_ARG_PADDING with !BYTES_BIG_ENDIAN
      hardwired to be true.  */

   return((mode == BLKmode
       ? (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
          && int_size_in_bytes (type) < (PARM_BOUNDARY / BITS_PER_UNIT))
       : GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)
      ? downward : upward);
}

/* Linked list of all external functions that are to be emitted by GCC.
   We output the name if and only if TREE_SYMBOL_REFERENCED is set in
   order to avoid putting out names that are never really used.  */

struct extern_func_list
{
  struct extern_func_list *next; /* next external */
  char *name;                    /* name of the external */
} *extern_func_head = 0;

static void
ia64_hpux_add_extern_decl (name)
        const char *name;
{
  struct extern_func_list *p;

  p = (struct extern_func_list *) xmalloc (sizeof (struct extern_func_list));
  p->name = xmalloc (strlen (name) + 1);
  strcpy(p->name, name);
  p->next = extern_func_head;
  extern_func_head = p;
}

/* Print out the list of used global functions.  */

void
ia64_hpux_asm_file_end (file)
	FILE *file;
{
  while (extern_func_head)
    {
      const char *real_name;
      tree decl;

      real_name = (* targetm.strip_name_encoding) (extern_func_head->name);
      decl = maybe_get_identifier (real_name);

      if (!decl
	  || (! TREE_ASM_WRITTEN (decl) && TREE_SYMBOL_REFERENCED (decl)))
        {
	  if (decl)
	    TREE_ASM_WRITTEN (decl) = 1;
	  (*targetm.asm_out.globalize_label) (file, extern_func_head->name);
	  fprintf (file, "%s", TYPE_ASM_OP);
	  assemble_name (file, extern_func_head->name);
	  putc (',', file);
	  fprintf (file, TYPE_OPERAND_FMT, "function");
	  putc ('\n', file);
        }
      extern_func_head = extern_func_head->next;
    }
}


/* Switch to the section to which we should output X.  The only thing
   special we do here is to honor small data.  */

static void
ia64_select_rtx_section (mode, x, align)
     enum machine_mode mode;
     rtx x;
     unsigned HOST_WIDE_INT align;
{
  if (GET_MODE_SIZE (mode) > 0
      && GET_MODE_SIZE (mode) <= ia64_section_threshold)
    sdata_section ();
  else
    default_elf_select_rtx_section (mode, x, align);
}

/* It is illegal to have relocations in shared segments on AIX and HPUX.
   Pretend flag_pic is always set.  */

static void
ia64_rwreloc_select_section (exp, reloc, align)
     tree exp;
     int reloc;
     unsigned HOST_WIDE_INT align;
{
  default_elf_select_section_1 (exp, reloc, align, true);
}

static void
ia64_rwreloc_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  default_unique_section_1 (decl, reloc, true);
}

static void
ia64_rwreloc_select_rtx_section (mode, x, align)
     enum machine_mode mode;
     rtx x;
     unsigned HOST_WIDE_INT align;
{
  int save_pic = flag_pic;
  flag_pic = 1;
  ia64_select_rtx_section (mode, x, align);
  flag_pic = save_pic;
}

static unsigned int
ia64_rwreloc_section_type_flags (decl, name, reloc)
     tree decl;
     const char *name;
     int reloc;
{
  return default_section_type_flags_1 (decl, name, reloc, true);
}


/* Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is non-zero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
ia64_output_mi_thunk (file, thunk, delta, vcall_offset, function)
     FILE *file;
     tree thunk ATTRIBUTE_UNUSED;
     HOST_WIDE_INT delta;
     HOST_WIDE_INT vcall_offset;
     tree function;
{
  rtx this, insn, funexp;

  reload_completed = 1;
  no_new_pseudos = 1;

  /* Set things up as ia64_expand_prologue might.  */
  last_scratch_gr_reg = 15;

  memset (&current_frame_info, 0, sizeof (current_frame_info));
  current_frame_info.spill_cfa_off = -16;
  current_frame_info.n_input_regs = 1;
  current_frame_info.need_regstk = (TARGET_REG_NAMES != 0);

  if (!TARGET_REG_NAMES)
    reg_names[IN_REG (0)] = ia64_reg_numbers[0];

  /* Mark the end of the (empty) prologue.  */
  emit_note (NULL, NOTE_INSN_PROLOGUE_END);

  this = gen_rtx_REG (Pmode, IN_REG (0));
  if (TARGET_ILP32)
    emit_insn (gen_ptr_extend (this,
			       gen_rtx_REG (ptr_mode, IN_REG (0))));

  /* Apply the constant offset, if required.  */
  if (delta)
    {
      rtx delta_rtx = GEN_INT (delta);

      if (!CONST_OK_FOR_I (delta))
	{
	  rtx tmp = gen_rtx_REG (Pmode, 2);
	  emit_move_insn (tmp, delta_rtx);
	  delta_rtx = tmp;
	}
      emit_insn (gen_adddi3 (this, this, delta_rtx));
    }

  /* Apply the offset from the vtable, if required.  */
  if (vcall_offset)
    {
      rtx vcall_offset_rtx = GEN_INT (vcall_offset);
      rtx tmp = gen_rtx_REG (Pmode, 2);

      if (TARGET_ILP32)
	{
	  rtx t = gen_rtx_REG (ptr_mode, 2);
	  emit_move_insn (t, gen_rtx_MEM (ptr_mode, this));
	  emit_insn (gen_ptr_extend (tmp, t));
	}
      else
	emit_move_insn (tmp, gen_rtx_MEM (Pmode, this));

      if (!CONST_OK_FOR_J (vcall_offset))
	{
	  rtx tmp2 = gen_rtx_REG (Pmode, next_scratch_gr_reg ());
	  emit_move_insn (tmp2, vcall_offset_rtx);
	  vcall_offset_rtx = tmp2;
	}
      emit_insn (gen_adddi3 (tmp, tmp, vcall_offset_rtx));

      if (TARGET_ILP32)
	emit_move_insn (gen_rtx_REG (ptr_mode, 2), 
			gen_rtx_MEM (ptr_mode, tmp));
      else
	emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));

      emit_insn (gen_adddi3 (this, this, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (! TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  ia64_expand_call (NULL_RTX, funexp, NULL_RTX, 1);
  insn = get_last_insn ();
  SIBLING_CALL_P (insn) = 1;

  /* Code generation for calls relies on splitting.  */
  reload_completed = 1;
  try_split (PATTERN (insn), insn, 0);

  emit_barrier ();

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  Note that use_thunk calls
     assemble_start_function and assemble_end_function.  */

  insn = get_insns ();
  emit_all_insn_group_barriers (NULL, insn);
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1, 0);
  final_end_function ();

  reload_completed = 0;
  no_new_pseudos = 0;
}

#include "gt-ia64.h"
