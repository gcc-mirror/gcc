/* Subroutines for insn-output.c for HPPA.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003 Free Software Foundation, Inc.
   Contributed by Tim Moore (moore@cs.utah.edu), based on sparc.c

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
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "output.h"
#include "except.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "reload.h"
#include "c-tree.h"
#include "integrate.h"
#include "function.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "recog.h"
#include "predict.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

static int hppa_use_dfa_pipeline_interface PARAMS ((void));

#undef TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE 
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE hppa_use_dfa_pipeline_interface

static int
hppa_use_dfa_pipeline_interface ()
{
  return 1;
}

/* Return nonzero if there is a bypass for the output of 
   OUT_INSN and the fp store IN_INSN.  */
int
hppa_fpstore_bypass_p (out_insn, in_insn)
     rtx out_insn, in_insn;
{
  enum machine_mode store_mode;
  enum machine_mode other_mode;
  rtx set;

  if (recog_memoized (in_insn) < 0
      || get_attr_type (in_insn) != TYPE_FPSTORE
      || recog_memoized (out_insn) < 0)
    return 0;

  store_mode = GET_MODE (SET_SRC (PATTERN (in_insn)));

  set = single_set (out_insn);
  if (!set)
    return 0;

  other_mode = GET_MODE (SET_SRC (set));

  return (GET_MODE_SIZE (store_mode) == GET_MODE_SIZE (other_mode));
}
  

#ifndef DO_FRAME_NOTES
#ifdef INCOMING_RETURN_ADDR_RTX
#define DO_FRAME_NOTES 1
#else
#define DO_FRAME_NOTES 0
#endif
#endif

static inline rtx force_mode PARAMS ((enum machine_mode, rtx));
static void pa_combine_instructions PARAMS ((rtx));
static int pa_can_combine_p PARAMS ((rtx, rtx, rtx, int, rtx, rtx, rtx));
static int forward_branch_p PARAMS ((rtx));
static int shadd_constant_p PARAMS ((int));
static void compute_zdepwi_operands PARAMS ((unsigned HOST_WIDE_INT, unsigned *));
static int compute_movstrsi_length PARAMS ((rtx));
static bool pa_assemble_integer PARAMS ((rtx, unsigned int, int));
static void remove_useless_addtr_insns PARAMS ((rtx, int));
static void store_reg PARAMS ((int, int, int));
static void store_reg_modify PARAMS ((int, int, int));
static void load_reg PARAMS ((int, int, int));
static void set_reg_plus_d PARAMS ((int, int, int, int));
static void pa_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void pa_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static int pa_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int pa_adjust_priority PARAMS ((rtx, int));
static int pa_issue_rate PARAMS ((void));
static void pa_select_section PARAMS ((tree, int, unsigned HOST_WIDE_INT))
     ATTRIBUTE_UNUSED;
static void pa_encode_section_info PARAMS ((tree, int));
static const char *pa_strip_name_encoding PARAMS ((const char *));
static void pa_globalize_label PARAMS ((FILE *, const char *))
     ATTRIBUTE_UNUSED;
static void pa_asm_output_mi_thunk PARAMS ((FILE *, tree, HOST_WIDE_INT,
					    HOST_WIDE_INT, tree));
#if !defined(USE_COLLECT2)
static void pa_asm_out_constructor PARAMS ((rtx, int));
static void pa_asm_out_destructor PARAMS ((rtx, int));
#endif
static void pa_init_builtins PARAMS ((void));
static void copy_fp_args PARAMS ((rtx)) ATTRIBUTE_UNUSED;
static int length_fp_args PARAMS ((rtx)) ATTRIBUTE_UNUSED;
static struct deferred_plabel *get_plabel PARAMS ((const char *))
     ATTRIBUTE_UNUSED;

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */
rtx hppa_compare_op0, hppa_compare_op1;
enum cmp_type hppa_branch_type;

/* Which cpu we are scheduling for.  */
enum processor_type pa_cpu;

/* String to hold which cpu we are scheduling for.  */
const char *pa_cpu_string;

/* Which architecture we are generating code for.  */
enum architecture_type pa_arch;

/* String to hold which architecture we are generating code for.  */
const char *pa_arch_string;

/* Counts for the number of callee-saved general and floating point
   registers which were saved by the current function's prologue.  */
static int gr_saved, fr_saved;

static rtx find_addr_reg PARAMS ((rtx));

/* Keep track of the number of bytes we have output in the CODE subspaces
   during this compilation so we'll know when to emit inline long-calls.  */
unsigned long total_code_bytes;

/* Variables to handle plabels that we discover are necessary at assembly
   output time.  They are output after the current function.  */
struct deferred_plabel GTY(())
{
  rtx internal_label;
  const char *name;
};
static GTY((length ("n_deferred_plabels"))) struct deferred_plabel *
  deferred_plabels;
static size_t n_deferred_plabels = 0;

/* Initialize the GCC target structure.  */

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.dword\t"
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER pa_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE pa_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE pa_output_function_epilogue

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST pa_adjust_cost
#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY pa_adjust_priority
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE pa_issue_rate

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO pa_encode_section_info
#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING pa_strip_name_encoding

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK pa_asm_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#if !defined(USE_COLLECT2)
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR pa_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR pa_asm_out_destructor
#endif

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS pa_init_builtins

struct gcc_target targetm = TARGET_INITIALIZER;

void
override_options ()
{
  if (pa_cpu_string == NULL)
    pa_cpu_string = TARGET_SCHED_DEFAULT;

  if (! strcmp (pa_cpu_string, "8000"))
    {
      pa_cpu_string = "8000";
      pa_cpu = PROCESSOR_8000;
    }
  else if (! strcmp (pa_cpu_string, "7100"))
    {
      pa_cpu_string = "7100";
      pa_cpu = PROCESSOR_7100;
    }
  else if (! strcmp (pa_cpu_string, "700"))
    {
      pa_cpu_string = "700";
      pa_cpu = PROCESSOR_700;
    }
  else if (! strcmp (pa_cpu_string, "7100LC"))
    {
      pa_cpu_string = "7100LC";
      pa_cpu = PROCESSOR_7100LC;
    }
  else if (! strcmp (pa_cpu_string, "7200"))
    {
      pa_cpu_string = "7200";
      pa_cpu = PROCESSOR_7200;
    }
  else if (! strcmp (pa_cpu_string, "7300"))
    {
      pa_cpu_string = "7300";
      pa_cpu = PROCESSOR_7300;
    }
  else
    {
      warning ("unknown -mschedule= option (%s).\nValid options are 700, 7100, 7100LC, 7200, 7300, and 8000\n", pa_cpu_string);
    }

  /* Set the instruction set architecture.  */
  if (pa_arch_string && ! strcmp (pa_arch_string, "1.0"))
    {
      pa_arch_string = "1.0";
      pa_arch = ARCHITECTURE_10;
      target_flags &= ~(MASK_PA_11 | MASK_PA_20);
    }
  else if (pa_arch_string && ! strcmp (pa_arch_string, "1.1"))
    {
      pa_arch_string = "1.1";
      pa_arch = ARCHITECTURE_11;
      target_flags &= ~MASK_PA_20;
      target_flags |= MASK_PA_11;
    }
  else if (pa_arch_string && ! strcmp (pa_arch_string, "2.0"))
    {
      pa_arch_string = "2.0";
      pa_arch = ARCHITECTURE_20;
      target_flags |= MASK_PA_11 | MASK_PA_20;
    }
  else if (pa_arch_string)
    {
      warning ("unknown -march= option (%s).\nValid options are 1.0, 1.1, and 2.0\n", pa_arch_string);
    }

  /* Unconditional branches in the delay slot are not compatible with dwarf2
     call frame information.  There is no benefit in using this optimization
     on PA8000 and later processors.  */
  if (pa_cpu >= PROCESSOR_8000
      || (! USING_SJLJ_EXCEPTIONS && flag_exceptions)
      || flag_unwind_tables)
    target_flags &= ~MASK_JUMP_IN_DELAY;

  if (flag_pic && TARGET_PORTABLE_RUNTIME)
    {
      warning ("PIC code generation is not supported in the portable runtime model\n");
    }

  if (flag_pic && TARGET_FAST_INDIRECT_CALLS)
   {
      warning ("PIC code generation is not compatible with fast indirect calls\n");
   }

  if (! TARGET_GAS && write_symbols != NO_DEBUG)
    {
      warning ("-g is only supported when using GAS on this processor,");
      warning ("-g option disabled");
      write_symbols = NO_DEBUG;
    }

  /* We only support the "big PIC" model now.  And we always generate PIC
     code when in 64bit mode.  */
  if (flag_pic == 1 || TARGET_64BIT)
    flag_pic = 2;

  /* We can't guarantee that .dword is available for 32-bit targets.  */
  if (UNITS_PER_WORD == 4)
    targetm.asm_out.aligned_op.di = NULL;

  /* The unaligned ops are only available when using GAS.  */
  if (!TARGET_GAS)
    {
      targetm.asm_out.unaligned_op.hi = NULL;
      targetm.asm_out.unaligned_op.si = NULL;
      targetm.asm_out.unaligned_op.di = NULL;
    }
}

static void
pa_init_builtins ()
{
#ifdef DONT_HAVE_FPUTC_UNLOCKED
  built_in_decls[(int) BUILT_IN_FPUTC_UNLOCKED] = NULL_TREE;
#endif
}

/* Return nonzero only if OP is a register of mode MODE,
   or CONST0_RTX.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == CONST0_RTX (mode) || register_operand (op, mode));
}

/* Return nonzero if OP is suitable for use in a call to a named
   function.

   For 2.5 try to eliminate either call_operand_address or
   function_label_operand, they perform very similar functions.  */
int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_MODE (op) == word_mode
	  && CONSTANT_P (op) && ! TARGET_PORTABLE_RUNTIME);
}

/* Return 1 if X contains a symbolic expression.  We know these
   expressions will have one of a few well defined forms, so
   we need only check those forms.  */
int
symbolic_expression_p (x)
     register rtx x;
{

  /* Strip off any HIGH.  */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  return (symbolic_operand (x, VOIDmode));
}

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Return 1 if the operand is either a register, zero, or a memory operand
   that is not symbolic.  */

int
reg_or_0_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (op == CONST0_RTX (mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Return 1 if the operand is a register operand or a non-symbolic memory
   operand after reload.  This predicate is used for branch patterns that
   internally handle register reloading.  We need to accept non-symbolic
   memory operands after reload to ensure that the pattern is still valid
   if reload didn't find a hard register for the operand.  */

int
reg_before_reload_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  /* Don't accept a SUBREG since it will need a reload.  */
  if (GET_CODE (op) == SUBREG)
    return 0;

  if (register_operand (op, mode))
    return 1;

  if (reload_completed
      && memory_operand (op, mode)
      && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Accept any constant that can be moved in one instruction into a
   general register.  */
int
cint_ok_for_move (intval)
     HOST_WIDE_INT intval;
{
  /* OK if ldo, ldil, or zdepi, can be used.  */
  return (CONST_OK_FOR_LETTER_P (intval, 'J')
	  || CONST_OK_FOR_LETTER_P (intval, 'N')
	  || CONST_OK_FOR_LETTER_P (intval, 'K'));
}

/* Accept anything that can be moved in one instruction into a general
   register.  */
int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONSTANT_P_RTX)
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);

  /* We consider a LO_SUM DLT reference a move_operand now since it has
     been merged into the normal movsi/movdi patterns.  */
  if (GET_CODE (op) == LO_SUM
      && GET_CODE (XEXP (op, 0)) == REG
      && REG_OK_FOR_BASE_P (XEXP (op, 0))
      && GET_CODE (XEXP (op, 1)) == UNSPEC
      && GET_MODE (op) == Pmode)
    return 1;

  /* Since move_operand is only used for source operands, we can always
     allow scaled indexing!  */
  if (! TARGET_DISABLE_INDEXING
      && GET_CODE (op) == PLUS
      && ((GET_CODE (XEXP (op, 0)) == MULT
	   && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
	   && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
	   && INTVAL (XEXP (XEXP (op, 0), 1))
	      == (HOST_WIDE_INT) GET_MODE_SIZE (mode)
	   && GET_CODE (XEXP (op, 1)) == REG)
	  || (GET_CODE (XEXP (op, 1)) == MULT
	      &&GET_CODE (XEXP (XEXP (op, 1), 0)) == REG
	      && GET_CODE (XEXP (XEXP (op, 1), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (op, 1), 1))
		 == (HOST_WIDE_INT) GET_MODE_SIZE (mode)
	      && GET_CODE (XEXP (op, 0)) == REG)))
    return 1;

  return memory_address_p (mode, op);
}

/* Accept REG and any CONST_INT that can be moved in one instruction into a
   general register.  */
int
reg_or_cint_move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  return 0;
}

int
pic_label_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (!flag_pic)
    return 0;

  switch (GET_CODE (op))
    {
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (XEXP (op, 0)) == LABEL_REF
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return reg_renumber && FP_REG_P (op);
}



/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 14-bit signed integers.  */
int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_14_BITS (op)));
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 11-bit signed integers.  */
int
arith11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_11_BITS (op)));
}

/* Return truth value of whether OP can be used as an operand in a
   adddi3 insn.  */
int
adddi3_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (TARGET_64BIT ? INT_14_BITS (op) : INT_11_BITS (op))));
}

/* A constant integer suitable for use in a PRE_MODIFY memory
   reference.  */
int
pre_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= -0x2000 && INTVAL (op) < 0x10);
}

/* A constant integer suitable for use in a POST_MODIFY memory
   reference.  */
int
post_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) < 0x2000 && INTVAL (op) >= -0x10);
}

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && GET_MODE (op) == mode
	      && VAL_14_BITS_P (CONST_DOUBLE_LOW (op))
	      && ((CONST_DOUBLE_HIGH (op) >= 0)
		  == ((CONST_DOUBLE_LOW (op) & 0x1000) == 0))));
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in three-address insns, or
   is an integer register.  */

int
ireg_or_int5_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT && INT_5_BITS (op))
	  || (GET_CODE (op) == REG && REGNO (op) > 0 && REGNO (op) < 32));
}

/* Return nonzero if OP is an integer register, else return zero.  */
int
ireg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == REG && REGNO (op) > 0 && REGNO (op) < 32);
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in three-address insns.  */

int
int5_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && INT_5_BITS (op));
}

int
uint5_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && INT_U5_BITS (op));
}

int
int11_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && INT_11_BITS (op));
}

int
uint32_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) < (HOST_WIDE_INT) 1 << 32));
#else
  return (GET_CODE (op) == CONST_INT
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0));
#endif
}

int
arith5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || int5_operand (op, mode);
}

/* True iff zdepi can be used to generate this CONST_INT.
   zdepi first sign extends a 5 bit signed number to a given field
   length, then places this field anywhere in a zero.  */
int
zdepi_cint_p (x)
     unsigned HOST_WIDE_INT x;
{
  unsigned HOST_WIDE_INT lsb_mask, t;

  /* This might not be obvious, but it's at least fast.
     This function is critical; we don't have the time loops would take.  */
  lsb_mask = x & -x;
  t = ((x >> 4) + lsb_mask) & ~(lsb_mask - 1);
  /* Return true iff t is a power of two.  */
  return ((t & (t - 1)) == 0);
}

/* True iff depi or extru can be used to compute (reg & mask).
   Accept bit pattern like these:
   0....01....1
   1....10....0
   1..10..01..1  */
int
and_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask = ~mask;
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi or extru can be used to compute (reg & OP).  */
int
and_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && and_mask_p (INTVAL (op))));
}

/* True iff depi can be used to compute (reg | MASK).  */
int
ior_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi can be used to compute (reg | OP).  */
int
ior_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && ior_mask_p (INTVAL (op)));
}

int
lhs_lshift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || lhs_lshift_cint_operand (op, mode);
}

/* True iff OP is a CONST_INT of the forms 0...0xxxx or 0...01...1xxxx.
   Such values can be the left hand side x in (x << r), using the zvdepi
   instruction.  */
int
lhs_lshift_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  unsigned HOST_WIDE_INT x;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  x = INTVAL (op) >> 4;
  return (x & (x + 1)) == 0;
}

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || GET_CODE (op) == CONST_INT;
}

int
pc_or_label_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == PC || GET_CODE (op) == LABEL_REF);
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig, reg;
     enum machine_mode mode;
{
  rtx pic_ref = orig;

  /* Labels need special handling.  */
  if (pic_label_operand (orig, mode))
    {
      /* We do not want to go through the movXX expanders here since that
	 would create recursion.

	 Nor do we really want to call a generator for a named pattern
	 since that requires multiple patterns if we want to support
	 multiple word sizes.

	 So instead we just emit the raw set, which avoids the movXX
	 expanders completely.  */
      emit_insn (gen_rtx_SET (VOIDmode, reg, orig));
      current_function_uses_pic_offset_table = 1;
      return reg;
    }
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == 0)
	abort ();

      emit_move_insn (reg,
		      gen_rtx_PLUS (word_mode, pic_offset_table_rtx,
				    gen_rtx_HIGH (word_mode, orig)));
      pic_ref
	= gen_rtx_MEM (Pmode,
		       gen_rtx_LO_SUM (Pmode, reg,
				       gen_rtx_UNSPEC (Pmode,
						       gen_rtvec (1, orig),
						       0)));

      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	abort ();

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else abort ();
      if (GET_CODE (orig) == CONST_INT)
	{
	  if (INT_14_BITS (orig))
	    return plus_constant (base, INTVAL (orig));
	  orig = force_reg (Pmode, orig);
	}
      pic_ref = gen_rtx_PLUS (Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the PA, transform:

	memory(X + <large int>)

   into:

	if (<large int> & mask) >= 16
	  Y = (<large int> & ~mask) + mask + 1	Round up.
	else
	  Y = (<large int> & ~mask)		Round down.
	Z = X + Y
	memory (Z + (<large int> - Y));

   This is for CSE to find several similar references, and only use one Z.

   X can either be a SYMBOL_REF or REG, but because combine can not
   perform a 4->2 combination we do nothing for SYMBOL_REF + D where
   D will not fit in 14 bits.

   MODE_FLOAT references allow displacements which fit in 5 bits, so use
   0x1f as the mask.

   MODE_INT references allow displacements which fit in 14 bits, so use
   0x3fff as the mask.

   This relies on the fact that most mode MODE_FLOAT references will use FP
   registers and most mode MODE_INT references will use integer registers.
   (In the rare case of an FP register used in an integer MODE, we depend
   on secondary reloads to clean things up.)


   It is also beneficial to handle (plus (mult (X) (Y)) (Z)) in a special
   manner if Y is 2, 4, or 8.  (allows more shadd insns and shifted indexed
   addressing modes to be used).

   Put X and Z into registers.  Then put the entire expression into
   a register.  */

rtx
hppa_legitimize_address (x, oldx, mode)
     rtx x, oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  rtx orig = x;

  if (flag_pic)
    return legitimize_pic_address (x, mode, gen_reg_rtx (Pmode));

  /* Strip off CONST.  */
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  /* Special case.  Get the SYMBOL_REF into a register and use indexing.
     That should always be safe.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == SYMBOL_REF)
    {
      rtx reg = force_reg (Pmode, XEXP (x, 1));
      return force_reg (Pmode, gen_rtx_PLUS (Pmode, reg, XEXP (x, 0)));
    }

  /* Note we must reject symbols which represent function addresses
     since the assembler/linker can't handle arithmetic on plabels.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ((GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	   && !FUNCTION_NAME_P (XSTR (XEXP (x, 0), 0)))
	  || GET_CODE (XEXP (x, 0)) == REG))
    {
      rtx int_part, ptr_reg;
      int newoffset;
      int offset = INTVAL (XEXP (x, 1));
      int mask;

      mask = (GET_MODE_CLASS (mode) == MODE_FLOAT
	      ? (TARGET_PA_20 ? 0x3fff : 0x1f) : 0x3fff);

      /* Choose which way to round the offset.  Round up if we
	 are >= halfway to the next boundary.  */
      if ((offset & mask) >= ((mask + 1) / 2))
	newoffset = (offset & ~ mask) + mask + 1;
      else
	newoffset = (offset & ~ mask);

      /* If the newoffset will not fit in 14 bits (ldo), then
	 handling this would take 4 or 5 instructions (2 to load
	 the SYMBOL_REF + 1 or 2 to load the newoffset + 1 to
	 add the new offset and the SYMBOL_REF.)  Combine can
	 not handle 4->2 or 5->2 combinations, so do not create
	 them.  */
      if (! VAL_14_BITS_P (newoffset)
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
	{
	  rtx const_part = plus_constant (XEXP (x, 0), newoffset);
	  rtx tmp_reg
	    = force_reg (Pmode,
			 gen_rtx_HIGH (Pmode, const_part));
	  ptr_reg
	    = force_reg (Pmode,
			 gen_rtx_LO_SUM (Pmode,
					 tmp_reg, const_part));
	}
      else
	{
	  if (! VAL_14_BITS_P (newoffset))
	    int_part = force_reg (Pmode, GEN_INT (newoffset));
	  else
	    int_part = GEN_INT (newoffset);

	  ptr_reg = force_reg (Pmode,
			       gen_rtx_PLUS (Pmode,
					     force_reg (Pmode, XEXP (x, 0)),
					     int_part));
	}
      return plus_constant (ptr_reg, offset - newoffset);
    }

  /* Handle (plus (mult (a) (shadd_constant)) (b)).  */

  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1)))
      && (GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == 'o'
	  || GET_CODE (XEXP (x, 1)) == SUBREG)
      && GET_CODE (XEXP (x, 1)) != CONST)
    {
      int val = INTVAL (XEXP (XEXP (x, 0), 1));
      rtx reg1, reg2;

      reg1 = XEXP (x, 1);
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      reg2 = XEXP (XEXP (x, 0), 0);
      if (GET_CODE (reg2) != REG)
        reg2 = force_reg (Pmode, force_operand (reg2, 0));

      return force_reg (Pmode, gen_rtx_PLUS (Pmode,
					     gen_rtx_MULT (Pmode,
							   reg2,
							   GEN_INT (val)),
					     reg1));
    }

  /* Similarly for (plus (plus (mult (a) (shadd_constant)) (b)) (c)).

     Only do so for floating point modes since this is more speculative
     and we lose if it's an integer store.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
      && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)))
      && (mode == SFmode || mode == DFmode))
    {

      /* First, try and figure out what to use as a base register.  */
      rtx reg1, reg2, base, idx, orig_base;

      reg1 = XEXP (XEXP (x, 0), 1);
      reg2 = XEXP (x, 1);
      base = NULL_RTX;
      idx = NULL_RTX;

      /* Make sure they're both regs.  If one was a SYMBOL_REF [+ const],
	 then emit_move_sequence will turn on REG_POINTER so we'll know
	 it's a base register below.  */
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      if (GET_CODE (reg2) != REG)
	reg2 = force_reg (Pmode, force_operand (reg2, 0));

      /* Figure out what the base and index are.  */

      if (GET_CODE (reg1) == REG
	  && REG_POINTER (reg1))
	{
	  base = reg1;
	  orig_base = XEXP (XEXP (x, 0), 1);
	  idx = gen_rtx_PLUS (Pmode,
			      gen_rtx_MULT (Pmode,
					    XEXP (XEXP (XEXP (x, 0), 0), 0),
					    XEXP (XEXP (XEXP (x, 0), 0), 1)),
			      XEXP (x, 1));
	}
      else if (GET_CODE (reg2) == REG
	       && REG_POINTER (reg2))
	{
	  base = reg2;
	  orig_base = XEXP (x, 1);
	  idx = XEXP (x, 0);
	}

      if (base == 0)
	return orig;

      /* If the index adds a large constant, try to scale the
	 constant so that it can be loaded with only one insn.  */
      if (GET_CODE (XEXP (idx, 1)) == CONST_INT
	  && VAL_14_BITS_P (INTVAL (XEXP (idx, 1))
			    / INTVAL (XEXP (XEXP (idx, 0), 1)))
	  && INTVAL (XEXP (idx, 1)) % INTVAL (XEXP (XEXP (idx, 0), 1)) == 0)
	{
	  /* Divide the CONST_INT by the scale factor, then add it to A.  */
	  int val = INTVAL (XEXP (idx, 1));

	  val /= INTVAL (XEXP (XEXP (idx, 0), 1));
	  reg1 = XEXP (XEXP (idx, 0), 0);
	  if (GET_CODE (reg1) != REG)
	    reg1 = force_reg (Pmode, force_operand (reg1, 0));

	  reg1 = force_reg (Pmode, gen_rtx_PLUS (Pmode, reg1, GEN_INT (val)));

	  /* We can now generate a simple scaled indexed address.  */
	  return
	    force_reg
	      (Pmode, gen_rtx_PLUS (Pmode,
				    gen_rtx_MULT (Pmode, reg1,
						  XEXP (XEXP (idx, 0), 1)),
				    base));
	}

      /* If B + C is still a valid base register, then add them.  */
      if (GET_CODE (XEXP (idx, 1)) == CONST_INT
	  && INTVAL (XEXP (idx, 1)) <= 4096
	  && INTVAL (XEXP (idx, 1)) >= -4096)
	{
	  int val = INTVAL (XEXP (XEXP (idx, 0), 1));
	  rtx reg1, reg2;

	  reg1 = force_reg (Pmode, gen_rtx_PLUS (Pmode, base, XEXP (idx, 1)));

	  reg2 = XEXP (XEXP (idx, 0), 0);
	  if (GET_CODE (reg2) != CONST_INT)
	    reg2 = force_reg (Pmode, force_operand (reg2, 0));

	  return force_reg (Pmode, gen_rtx_PLUS (Pmode,
						 gen_rtx_MULT (Pmode,
							       reg2,
							       GEN_INT (val)),
						 reg1));
	}

      /* Get the index into a register, then add the base + index and
	 return a register holding the result.  */

      /* First get A into a register.  */
      reg1 = XEXP (XEXP (idx, 0), 0);
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      /* And get B into a register.  */
      reg2 = XEXP (idx, 1);
      if (GET_CODE (reg2) != REG)
	reg2 = force_reg (Pmode, force_operand (reg2, 0));

      reg1 = force_reg (Pmode,
			gen_rtx_PLUS (Pmode,
				      gen_rtx_MULT (Pmode, reg1,
						    XEXP (XEXP (idx, 0), 1)),
				      reg2));

      /* Add the result to our base register and return.  */
      return force_reg (Pmode, gen_rtx_PLUS (Pmode, base, reg1));

    }

  /* Uh-oh.  We might have an address for x[n-100000].  This needs
     special handling to avoid creating an indexed memory address
     with x-100000 as the base.

     If the constant part is small enough, then it's still safe because
     there is a guard page at the beginning and end of the data segment.

     Scaled references are common enough that we want to try and rearrange the
     terms so that we can use indexing for these addresses too.  Only
     do the optimization for floatint point modes.  */

  if (GET_CODE (x) == PLUS
      && symbolic_expression_p (XEXP (x, 1)))
    {
      /* Ugly.  We modify things here so that the address offset specified
	 by the index expression is computed first, then added to x to form
	 the entire address.  */

      rtx regx1, regx2, regy1, regy2, y;

      /* Strip off any CONST.  */
      y = XEXP (x, 1);
      if (GET_CODE (y) == CONST)
	y = XEXP (y, 0);

      if (GET_CODE (y) == PLUS || GET_CODE (y) == MINUS)
	{
	  /* See if this looks like
		(plus (mult (reg) (shadd_const))
		      (const (plus (symbol_ref) (const_int))))

	     Where const_int is small.  In that case the const
	     expression is a valid pointer for indexing.

	     If const_int is big, but can be divided evenly by shadd_const
	     and added to (reg).  This allows more scaled indexed addresses.  */
	  if (GET_CODE (XEXP (y, 0)) == SYMBOL_REF
	      && GET_CODE (XEXP (x, 0)) == MULT
	      && GET_CODE (XEXP (y, 1)) == CONST_INT
	      && INTVAL (XEXP (y, 1)) >= -4096
	      && INTVAL (XEXP (y, 1)) <= 4095
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1))))
	    {
	      int val = INTVAL (XEXP (XEXP (x, 0), 1));
	      rtx reg1, reg2;

	      reg1 = XEXP (x, 1);
	      if (GET_CODE (reg1) != REG)
		reg1 = force_reg (Pmode, force_operand (reg1, 0));

	      reg2 = XEXP (XEXP (x, 0), 0);
	      if (GET_CODE (reg2) != REG)
	        reg2 = force_reg (Pmode, force_operand (reg2, 0));

	      return force_reg (Pmode,
				gen_rtx_PLUS (Pmode,
					      gen_rtx_MULT (Pmode,
							    reg2,
							    GEN_INT (val)),
					      reg1));
	    }
	  else if ((mode == DFmode || mode == SFmode)
		   && GET_CODE (XEXP (y, 0)) == SYMBOL_REF
		   && GET_CODE (XEXP (x, 0)) == MULT
		   && GET_CODE (XEXP (y, 1)) == CONST_INT
		   && INTVAL (XEXP (y, 1)) % INTVAL (XEXP (XEXP (x, 0), 1)) == 0
		   && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
		   && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1))))
	    {
	      regx1
		= force_reg (Pmode, GEN_INT (INTVAL (XEXP (y, 1))
					     / INTVAL (XEXP (XEXP (x, 0), 1))));
	      regx2 = XEXP (XEXP (x, 0), 0);
	      if (GET_CODE (regx2) != REG)
		regx2 = force_reg (Pmode, force_operand (regx2, 0));
	      regx2 = force_reg (Pmode, gen_rtx_fmt_ee (GET_CODE (y), Pmode,
							regx2, regx1));
	      return
		force_reg (Pmode,
			   gen_rtx_PLUS (Pmode,
					 gen_rtx_MULT (Pmode, regx2,
						       XEXP (XEXP (x, 0), 1)),
					 force_reg (Pmode, XEXP (y, 0))));
	    }
	  else if (GET_CODE (XEXP (y, 1)) == CONST_INT
		   && INTVAL (XEXP (y, 1)) >= -4096
		   && INTVAL (XEXP (y, 1)) <= 4095)
	    {
	      /* This is safe because of the guard page at the
		 beginning and end of the data space.  Just
		 return the original address.  */
	      return orig;
	    }
	  else
	    {
	      /* Doesn't look like one we can optimize.  */
	      regx1 = force_reg (Pmode, force_operand (XEXP (x, 0), 0));
	      regy1 = force_reg (Pmode, force_operand (XEXP (y, 0), 0));
	      regy2 = force_reg (Pmode, force_operand (XEXP (y, 1), 0));
	      regx1 = force_reg (Pmode,
				 gen_rtx_fmt_ee (GET_CODE (y), Pmode,
						 regx1, regy2));
	      return force_reg (Pmode, gen_rtx_PLUS (Pmode, regx1, regy1));
	    }
	}
    }

  return orig;
}

/* For the HPPA, REG and REG+CONST is cost 0
   and addresses involving symbolic constants are cost 2.

   PIC addresses are very expensive.

   It is no coincidence that this has the same structure
   as GO_IF_LEGITIMATE_ADDRESS.  */
int
hppa_address_cost (X)
     rtx X;
{
  if (GET_CODE (X) == PLUS)
      return 1;
  else if (GET_CODE (X) == LO_SUM)
    return 1;
  else if (GET_CODE (X) == HIGH)
    return 2;
  return 4;
}

/* Ensure mode of ORIG, a REG rtx, is MODE.  Returns either ORIG or a
   new rtx with the correct mode.  */
static inline rtx
force_mode (mode, orig)
     enum machine_mode mode;
     rtx orig;
{
  if (mode == GET_MODE (orig))
    return orig;

  if (REGNO (orig) >= FIRST_PSEUDO_REGISTER)
    abort ();

  return gen_rtx_REG (mode, REGNO (orig));
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.

   Note SCRATCH_REG may not be in the proper mode depending on how it
   will be used.  This routine is resposible for creating a new copy
   of SCRATCH_REG in the proper mode.  */

int
emit_move_sequence (operands, mode, scratch_reg)
     rtx *operands;
     enum machine_mode mode;
     rtx scratch_reg;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];
  register rtx tem;

  if (scratch_reg
      && reload_in_progress && GET_CODE (operand0) == REG
      && REGNO (operand0) >= FIRST_PSEUDO_REGISTER)
    operand0 = reg_equiv_mem[REGNO (operand0)];
  else if (scratch_reg
	   && reload_in_progress && GET_CODE (operand0) == SUBREG
	   && GET_CODE (SUBREG_REG (operand0)) == REG
	   && REGNO (SUBREG_REG (operand0)) >= FIRST_PSEUDO_REGISTER)
    {
     /* We must not alter SUBREG_BYTE (operand0) since that would confuse
	the code which tracks sets/uses for delete_output_reload.  */
      rtx temp = gen_rtx_SUBREG (GET_MODE (operand0),
				 reg_equiv_mem [REGNO (SUBREG_REG (operand0))],
				 SUBREG_BYTE (operand0));
      operand0 = alter_subreg (&temp);
    }

  if (scratch_reg
      && reload_in_progress && GET_CODE (operand1) == REG
      && REGNO (operand1) >= FIRST_PSEUDO_REGISTER)
    operand1 = reg_equiv_mem[REGNO (operand1)];
  else if (scratch_reg
	   && reload_in_progress && GET_CODE (operand1) == SUBREG
	   && GET_CODE (SUBREG_REG (operand1)) == REG
	   && REGNO (SUBREG_REG (operand1)) >= FIRST_PSEUDO_REGISTER)
    {
     /* We must not alter SUBREG_BYTE (operand0) since that would confuse
	the code which tracks sets/uses for delete_output_reload.  */
      rtx temp = gen_rtx_SUBREG (GET_MODE (operand1),
				 reg_equiv_mem [REGNO (SUBREG_REG (operand1))],
				 SUBREG_BYTE (operand1));
      operand1 = alter_subreg (&temp);
    }

  if (scratch_reg && reload_in_progress && GET_CODE (operand0) == MEM
      && ((tem = find_replacement (&XEXP (operand0, 0)))
	  != XEXP (operand0, 0)))
    operand0 = gen_rtx_MEM (GET_MODE (operand0), tem);
  if (scratch_reg && reload_in_progress && GET_CODE (operand1) == MEM
      && ((tem = find_replacement (&XEXP (operand1, 0)))
	  != XEXP (operand1, 0)))
    operand1 = gen_rtx_MEM (GET_MODE (operand1), tem);

  /* Handle secondary reloads for loads/stores of FP registers from
     REG+D addresses where D does not fit in 5 or 14 bits, including
     (subreg (mem (addr))) cases.  */
  if (fp_reg_operand (operand0, mode)
      && ((GET_CODE (operand1) == MEM
	   && !memory_address_p ((GET_MODE_SIZE (mode) == 4 ? SFmode : DFmode),
				 XEXP (operand1, 0)))
	  || ((GET_CODE (operand1) == SUBREG
	       && GET_CODE (XEXP (operand1, 0)) == MEM
	       && !memory_address_p ((GET_MODE_SIZE (mode) == 4
				      ? SFmode : DFmode),
				     XEXP (XEXP (operand1, 0), 0)))))
      && scratch_reg)
    {
      if (GET_CODE (operand1) == SUBREG)
	operand1 = XEXP (operand1, 0);

      /* SCRATCH_REG will hold an address and maybe the actual data.  We want
	 it in WORD_MODE regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (word_mode, scratch_reg);

      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (Pmode, XEXP (operand1, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand1, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx_fmt_ee (GET_CODE (XEXP (operand1, 0)),
						       Pmode,
						       XEXP (XEXP (operand1, 0), 0),
						       scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand1, 0));
      emit_insn (gen_rtx_SET (VOIDmode, operand0,
			      gen_rtx_MEM (mode, scratch_reg)));
      return 1;
    }
  else if (fp_reg_operand (operand1, mode)
	   && ((GET_CODE (operand0) == MEM
		&& !memory_address_p ((GET_MODE_SIZE (mode) == 4
				       ? SFmode : DFmode),
				      XEXP (operand0, 0)))
	       || ((GET_CODE (operand0) == SUBREG)
		   && GET_CODE (XEXP (operand0, 0)) == MEM
		   && !memory_address_p ((GET_MODE_SIZE (mode) == 4
					  ? SFmode : DFmode),
					 XEXP (XEXP (operand0, 0), 0))))
	   && scratch_reg)
    {
      if (GET_CODE (operand0) == SUBREG)
	operand0 = XEXP (operand0, 0);

      /* SCRATCH_REG will hold an address and maybe the actual data.  We want
	 it in WORD_MODE regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (word_mode, scratch_reg);

      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (Pmode, XEXP (operand0, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand0, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx_fmt_ee (GET_CODE (XEXP (operand0,
								        0)),
						       Pmode,
						       XEXP (XEXP (operand0, 0),
								   0),
						       scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand0, 0));
      emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (mode, scratch_reg),
			      operand1));
      return 1;
    }
  /* Handle secondary reloads for loads of FP registers from constant
     expressions by forcing the constant into memory.

     use scratch_reg to hold the address of the memory location.

     The proper fix is to change PREFERRED_RELOAD_CLASS to return
     NO_REGS when presented with a const_int and an register class
     containing only FP registers.  Doing so unfortunately creates
     more problems than it solves.   Fix this for 2.5.  */
  else if (fp_reg_operand (operand0, mode)
	   && CONSTANT_P (operand1)
	   && scratch_reg)
    {
      rtx xoperands[2];

      /* SCRATCH_REG will hold an address and maybe the actual data.  We want
	 it in WORD_MODE regardless of what mode it was originally given
	 to us.  */
      scratch_reg = force_mode (word_mode, scratch_reg);

      /* Force the constant into memory and put the address of the
	 memory location into scratch_reg.  */
      xoperands[0] = scratch_reg;
      xoperands[1] = XEXP (force_const_mem (mode, operand1), 0);
      emit_move_sequence (xoperands, Pmode, 0);

      /* Now load the destination register.  */
      emit_insn (gen_rtx_SET (mode, operand0,
			      gen_rtx_MEM (mode, scratch_reg)));
      return 1;
    }
  /* Handle secondary reloads for SAR.  These occur when trying to load
     the SAR from memory, FP register, or with a constant.  */
  else if (GET_CODE (operand0) == REG
	   && REGNO (operand0) < FIRST_PSEUDO_REGISTER
	   && REGNO_REG_CLASS (REGNO (operand0)) == SHIFT_REGS
	   && (GET_CODE (operand1) == MEM
	       || GET_CODE (operand1) == CONST_INT
	       || (GET_CODE (operand1) == REG
		   && FP_REG_CLASS_P (REGNO_REG_CLASS (REGNO (operand1)))))
	   && scratch_reg)
    {
      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (GET_CODE (operand1) == MEM
	  && !memory_address_p (Pmode, XEXP (operand1, 0)))
	{
	  /* We are reloading the address into the scratch register, so we
	     want to make sure the scratch register is a full register.  */
	  scratch_reg = force_mode (word_mode, scratch_reg);

	  emit_move_insn (scratch_reg, XEXP (XEXP (operand1, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx_fmt_ee (GET_CODE (XEXP (operand1,
								        0)),
						       Pmode,
						       XEXP (XEXP (operand1, 0),
						       0),
						       scratch_reg));

	  /* Now we are going to load the scratch register from memory,
	     we want to load it in the same width as the original MEM,
	     which must be the same as the width of the ultimate destination,
	     OPERAND0.  */
	  scratch_reg = force_mode (GET_MODE (operand0), scratch_reg);

	  emit_move_insn (scratch_reg, gen_rtx_MEM (GET_MODE (operand0),
						    scratch_reg));
	}
      else
	{
	  /* We want to load the scratch register using the same mode as
	     the ultimate destination.  */
	  scratch_reg = force_mode (GET_MODE (operand0), scratch_reg);

	  emit_move_insn (scratch_reg, operand1);
	}

      /* And emit the insn to set the ultimate destination.  We know that
	 the scratch register has the same mode as the destination at this
	 point.  */
      emit_move_insn (operand0, scratch_reg);
      return 1;
    }
  /* Handle most common case: storing into a register.  */
  else if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT
	      && cint_ok_for_move (INTVAL (operand1)))
	  || (operand1 == CONST0_RTX (mode))
	  || (GET_CODE (operand1) == HIGH
	      && !symbolic_operand (XEXP (operand1, 0), VOIDmode))
	  /* Only `general_operands' can come here, so MEM is ok.  */
	  || GET_CODE (operand1) == MEM)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx_SET (VOIDmode, operand0, operand1));
	  return 1;
	}
    }
  else if (GET_CODE (operand0) == MEM)
    {
      if (mode == DFmode && operand1 == CONST0_RTX (mode)
	  && !(reload_in_progress || reload_completed))
	{
	  rtx temp = gen_reg_rtx (DFmode);

	  emit_insn (gen_rtx_SET (VOIDmode, temp, operand1));
	  emit_insn (gen_rtx_SET (VOIDmode, operand0, temp));
	  return 1;
	}
      if (register_operand (operand1, mode) || operand1 == CONST0_RTX (mode))
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx_SET (VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! (reload_in_progress || reload_completed))
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  /* Simplify the source if we need to.
     Note we do have to handle function labels here, even though we do
     not consider them legitimate constants.  Loop optimizations can
     call the emit_move_xxx with one as a source.  */
  if ((GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
      || function_label_operand (operand1, mode)
      || (GET_CODE (operand1) == HIGH
	  && symbolic_operand (XEXP (operand1, 0), mode)))
    {
      int ishighonly = 0;

      if (GET_CODE (operand1) == HIGH)
	{
	  ishighonly = 1;
	  operand1 = XEXP (operand1, 0);
	}
      if (symbolic_operand (operand1, mode))
	{
	  /* Argh.  The assembler and linker can't handle arithmetic
	     involving plabels.

	     So we force the plabel into memory, load operand0 from
	     the memory location, then add in the constant part.  */
	  if ((GET_CODE (operand1) == CONST
	       && GET_CODE (XEXP (operand1, 0)) == PLUS
	       && function_label_operand (XEXP (XEXP (operand1, 0), 0), Pmode))
	      || function_label_operand (operand1, mode))
	    {
	      rtx temp, const_part;

	      /* Figure out what (if any) scratch register to use.  */
	      if (reload_in_progress || reload_completed)
		{
		  scratch_reg = scratch_reg ? scratch_reg : operand0;
		  /* SCRATCH_REG will hold an address and maybe the actual
		     data.  We want it in WORD_MODE regardless of what mode it
		     was originally given to us.  */
		  scratch_reg = force_mode (word_mode, scratch_reg);
		}
	      else if (flag_pic)
		scratch_reg = gen_reg_rtx (Pmode);

	      if (GET_CODE (operand1) == CONST)
		{
		  /* Save away the constant part of the expression.  */
		  const_part = XEXP (XEXP (operand1, 0), 1);
		  if (GET_CODE (const_part) != CONST_INT)
		    abort ();

		  /* Force the function label into memory.  */
		  temp = force_const_mem (mode, XEXP (XEXP (operand1, 0), 0));
		}
	      else
		{
		  /* No constant part.  */
		  const_part = NULL_RTX;

		  /* Force the function label into memory.  */
		  temp = force_const_mem (mode, operand1);
		}


	      /* Get the address of the memory location.  PIC-ify it if
		 necessary.  */
	      temp = XEXP (temp, 0);
	      if (flag_pic)
		temp = legitimize_pic_address (temp, mode, scratch_reg);

	      /* Put the address of the memory location into our destination
		 register.  */
	      operands[1] = temp;
	      emit_move_sequence (operands, mode, scratch_reg);

	      /* Now load from the memory location into our destination
		 register.  */
	      operands[1] = gen_rtx_MEM (Pmode, operands[0]);
	      emit_move_sequence (operands, mode, scratch_reg);

	      /* And add back in the constant part.  */
	      if (const_part != NULL_RTX)
		expand_inc (operand0, const_part);

	      return 1;
	    }

	  if (flag_pic)
	    {
	      rtx temp;

	      if (reload_in_progress || reload_completed)
		{
		  temp = scratch_reg ? scratch_reg : operand0;
		  /* TEMP will hold an address and maybe the actual
		     data.  We want it in WORD_MODE regardless of what mode it
		     was originally given to us.  */
		  temp = force_mode (word_mode, temp);
		}
	      else
		temp = gen_reg_rtx (Pmode);

	      /* (const (plus (symbol) (const_int))) must be forced to
		 memory during/after reload if the const_int will not fit
		 in 14 bits.  */
	      if (GET_CODE (operand1) == CONST
		       && GET_CODE (XEXP (operand1, 0)) == PLUS
		       && GET_CODE (XEXP (XEXP (operand1, 0), 1)) == CONST_INT
		       && !INT_14_BITS (XEXP (XEXP (operand1, 0), 1))
		       && (reload_completed || reload_in_progress)
		       && flag_pic)
		{
		  operands[1] = force_const_mem (mode, operand1);
		  operands[1] = legitimize_pic_address (XEXP (operands[1], 0),
							mode, temp);
		  operands[1] = gen_rtx_MEM (mode, operands[1]);
		  emit_move_sequence (operands, mode, temp);
		}
	      else
		{
		  operands[1] = legitimize_pic_address (operand1, mode, temp);
		  emit_insn (gen_rtx_SET (VOIDmode, operand0, operands[1]));
		}
	    }
	  /* On the HPPA, references to data space are supposed to use dp,
	     register 27, but showing it in the RTL inhibits various cse
	     and loop optimizations.  */
	  else
	    {
	      rtx temp, set;

	      if (reload_in_progress || reload_completed)
		{
		  temp = scratch_reg ? scratch_reg : operand0;
		  /* TEMP will hold an address and maybe the actual
		     data.  We want it in WORD_MODE regardless of what mode it
		     was originally given to us.  */
		  temp = force_mode (word_mode, temp);
		}
	      else
		temp = gen_reg_rtx (mode);

	      /* Loading a SYMBOL_REF into a register makes that register
		 safe to be used as the base in an indexed address.

		 Don't mark hard registers though.  That loses.  */
	      if (GET_CODE (operand0) == REG
		  && REGNO (operand0) >= FIRST_PSEUDO_REGISTER)
		REG_POINTER (operand0) = 1;
	      if (REGNO (temp) >= FIRST_PSEUDO_REGISTER)
		REG_POINTER (temp) = 1;
	      if (ishighonly)
		set = gen_rtx_SET (mode, operand0, temp);
	      else
		set = gen_rtx_SET (VOIDmode,
				   operand0,
				   gen_rtx_LO_SUM (mode, temp, operand1));

	      emit_insn (gen_rtx_SET (VOIDmode,
				      temp,
				      gen_rtx_HIGH (mode, operand1)));
	      emit_insn (set);

	    }
	  return 1;
	}
      else if (GET_CODE (operand1) != CONST_INT
	       || ! cint_ok_for_move (INTVAL (operand1)))
	{
	  rtx extend = NULL_RTX;
	  rtx temp;

	  if (TARGET_64BIT && GET_CODE (operand1) == CONST_INT
	      && HOST_BITS_PER_WIDE_INT > 32
	      && GET_MODE_BITSIZE (GET_MODE (operand0)) > 32)
	    {
	      HOST_WIDE_INT val = INTVAL (operand1);
	      HOST_WIDE_INT nval;

	      /* Extract the low order 32 bits of the value and sign extend.
		 If the new value is the same as the original value, we can
		 can use the original value as-is.  If the new value is
		 different, we use it and insert the most-significant 32-bits
		 of the original value into the final result.  */
	      nval = ((val & (((HOST_WIDE_INT) 2 << 31) - 1))
		      ^ ((HOST_WIDE_INT) 1 << 31)) - ((HOST_WIDE_INT) 1 << 31);
	      if (val != nval)
		{
#if HOST_BITS_PER_WIDE_INT > 32
		  extend = GEN_INT (val >> 32);
#endif
		  operand1 = GEN_INT (nval);
		}
	    }

	  if (reload_in_progress || reload_completed)
	    temp = operand0;
	  else
	    temp = gen_reg_rtx (mode);

	  /* We don't directly split DImode constants on 32-bit targets
	     because PLUS uses an 11-bit immediate and the insn sequence
	     generated is not as efficient as the one using HIGH/LO_SUM.  */
	  if (GET_CODE (operand1) == CONST_INT
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	    {
	      /* Directly break constant into high and low parts.  This
		 provides better optimization opportunities because various
		 passes recognize constants split with PLUS but not LO_SUM.
		 We use a 14-bit signed low part except when the addition
		 of 0x4000 to the high part might change the sign of the
		 high part.  */
	      HOST_WIDE_INT value = INTVAL (operand1);
	      HOST_WIDE_INT low = value & 0x3fff;
	      HOST_WIDE_INT high = value & ~ 0x3fff;

	      if (low >= 0x2000)
		{
		  if (high == 0x7fffc000 || (mode == HImode && high == 0x4000))
		    high += 0x2000;
		  else
		    high += 0x4000;
		}

	      low = value - high;

	      emit_insn (gen_rtx_SET (VOIDmode, temp, GEN_INT (high)));
	      operands[1] = gen_rtx_PLUS (mode, temp, GEN_INT (low));
	    }
	  else
	    {
	      emit_insn (gen_rtx_SET (VOIDmode, temp,
				      gen_rtx_HIGH (mode, operand1)));
	      operands[1] = gen_rtx_LO_SUM (mode, temp, operand1);
	    }

	  emit_move_insn (operands[0], operands[1]);

	  if (extend != NULL_RTX)
	    emit_insn (gen_insv (operands[0], GEN_INT (32), const0_rtx,
				 extend));

	  return 1;
	}
    }
  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Examine EXP and return nonzero if it contains an ADDR_EXPR (meaning
   it will need a link/runtime reloc).  */

int
reloc_needed (exp)
     tree exp;
{
  int reloc = 0;

  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      return 1;

    case PLUS_EXPR:
    case MINUS_EXPR:
      reloc = reloc_needed (TREE_OPERAND (exp, 0));
      reloc |= reloc_needed (TREE_OPERAND (exp, 1));
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      reloc = reloc_needed (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	register tree link;
	for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	  if (TREE_VALUE (link) != 0)
	    reloc |= reloc_needed (TREE_VALUE (link));
      }
      break;

    case ERROR_MARK:
      break;

    default:
      break;
    }
  return reloc;
}

/* Does operand (which is a symbolic_operand) live in text space?
   If so, SYMBOL_REF_FLAG, which is set by pa_encode_section_info,
   will be true.  */

int
read_only_operand (operand, mode)
     rtx operand;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (operand) == CONST)
    operand = XEXP (XEXP (operand, 0), 0);
  if (flag_pic)
    {
      if (GET_CODE (operand) == SYMBOL_REF)
	return SYMBOL_REF_FLAG (operand) && !CONSTANT_POOL_ADDRESS_P (operand);
    }
  else
    {
      if (GET_CODE (operand) == SYMBOL_REF)
	return SYMBOL_REF_FLAG (operand) || CONSTANT_POOL_ADDRESS_P (operand);
    }
  return 1;
}


/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.   */
const char *
singlemove_string (operands)
     rtx *operands;
{
  HOST_WIDE_INT intval;

  if (GET_CODE (operands[0]) == MEM)
    return "stw %r1,%0";
  if (GET_CODE (operands[1]) == MEM)
    return "ldw %1,%0";
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      long i;
      REAL_VALUE_TYPE d;

      if (GET_MODE (operands[1]) != SFmode)
	abort ();

      /* Translate the CONST_DOUBLE to a CONST_INT with the same target
	 bit pattern.  */
      REAL_VALUE_FROM_CONST_DOUBLE (d, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (d, i);

      operands[1] = GEN_INT (i);
      /* Fall through to CONST_INT case.  */
    }
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      intval = INTVAL (operands[1]);

      if (VAL_14_BITS_P (intval))
	return "ldi %1,%0";
      else if ((intval & 0x7ff) == 0)
	return "ldil L'%1,%0";
      else if (zdepi_cint_p (intval))
	return "{zdepi %Z1,%0|depwi,z %Z1,%0}";
      else
	return "ldil L'%1,%0\n\tldo R'%1(%0),%0";
    }
  return "copy %1,%0";
}


/* Compute position (in OP[1]) and width (in OP[2])
   useful for copying IMM to a register using the zdepi
   instructions.  Store the immediate value to insert in OP[0].  */
static void
compute_zdepwi_operands (imm, op)
     unsigned HOST_WIDE_INT imm;
     unsigned *op;
{
  int lsb, len;

  /* Find the least significant set bit in IMM.  */
  for (lsb = 0; lsb < 32; lsb++)
    {
      if ((imm & 1) != 0)
        break;
      imm >>= 1;
    }

  /* Choose variants based on *sign* of the 5-bit field.  */
  if ((imm & 0x10) == 0)
    len = (lsb <= 28) ? 4 : 32 - lsb;
  else
    {
      /* Find the width of the bitstring in IMM.  */
      for (len = 5; len < 32; len++)
	{
	  if ((imm & (1 << len)) == 0)
	    break;
	}

      /* Sign extend IMM as a 5-bit value.  */
      imm = (imm & 0xf) - 0x10;
    }

  op[0] = imm;
  op[1] = 31 - lsb;
  op[2] = len;
}

/* Compute position (in OP[1]) and width (in OP[2])
   useful for copying IMM to a register using the depdi,z
   instructions.  Store the immediate value to insert in OP[0].  */
void
compute_zdepdi_operands (imm, op)
     unsigned HOST_WIDE_INT imm;
     unsigned *op;
{
  HOST_WIDE_INT lsb, len;

  /* Find the least significant set bit in IMM.  */
  for (lsb = 0; lsb < HOST_BITS_PER_WIDE_INT; lsb++)
    {
      if ((imm & 1) != 0)
        break;
      imm >>= 1;
    }

  /* Choose variants based on *sign* of the 5-bit field.  */
  if ((imm & 0x10) == 0)
    len = ((lsb <= HOST_BITS_PER_WIDE_INT - 4)
	   ? 4 : HOST_BITS_PER_WIDE_INT - lsb);
  else
    {
      /* Find the width of the bitstring in IMM.  */
      for (len = 5; len < HOST_BITS_PER_WIDE_INT; len++)
	{
	  if ((imm & ((unsigned HOST_WIDE_INT) 1 << len)) == 0)
	    break;
	}

      /* Sign extend IMM as a 5-bit value.  */
      imm = (imm & 0xf) - 0x10;
    }

  op[0] = imm;
  op[1] = 63 - lsb;
  op[2] = len;
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

const char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 != REGOP && optype1 != REGOP)
    abort ();

   /* Handle auto decrementing and incrementing loads and stores
     specifically, since the structure of the function doesn't work
     for them without major modification.  Do it better when we learn
     this port about the general inc/dec addressing of PA.
     (This was written by tege.  Chide him if it doesn't work.)  */

  if (optype0 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[0], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx_SUBREG (SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "{stws|stw},ma %1,8(%0)\n\tstw %R1,-4(%0)";
	      return "{stws|stw},ma %1,-8(%0)\n\tstw %R1,12(%0)";
	    }
	  else
	    abort ();
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx_SUBREG (SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "{stws|stw},mb %1,8(%0)\n\tstw %R1,4(%0)";
	      return "{stws|stw},mb %1,-8(%0)\n\tstw %R1,4(%0)";
	    }
	  else
	    abort ();
	}
    }
  if (optype1 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[1], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx_SUBREG (SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "{ldws|ldw},ma 8(%1),%0\n\tldw -4(%1),%R0";
	      return "{ldws|ldw},ma -8(%1),%0\n\tldw 12(%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldw 4(%1),%R0\n\t{ldws|ldw},ma 8(%1),%0";
	      return "ldw 4(%1),%R0\n\t{ldws|ldw},ma -8(%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx_SUBREG (SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "{ldws|ldw},mb 8(%1),%0\n\tldw 4(%1),%R0";
	      return "{ldws|ldw},mb -8(%1),%0\n\tldw 4(%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldw 12(%1),%R0\n\t{ldws|ldw},mb 8(%1),%0";
	      return "ldw -4(%1),%R0\n\t{ldws|ldw},mb -8(%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PLUS
	       && GET_CODE (XEXP (addr, 0)) == MULT)
	{
	  rtx high_reg = gen_rtx_SUBREG (SImode, operands[0], 0);

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      rtx xoperands[3];

	      xoperands[0] = high_reg;
	      xoperands[1] = XEXP (addr, 1);
	      xoperands[2] = XEXP (XEXP (addr, 0), 0);
	      xoperands[3] = XEXP (XEXP (addr, 0), 1);
	      output_asm_insn ("{sh%O3addl %2,%1,%0|shladd,l %2,%O3,%1,%0}",
			       xoperands);
	      return "ldw 4(%0),%R0\n\tldw 0(%0),%0";
	    }
	  else
	    {
	      rtx xoperands[3];

	      xoperands[0] = high_reg;
	      xoperands[1] = XEXP (addr, 1);
	      xoperands[2] = XEXP (XEXP (addr, 0), 0);
	      xoperands[3] = XEXP (XEXP (addr, 0), 1);
	      output_asm_insn ("{sh%O3addl %2,%1,%R0|shladd,l %2,%O3,%1,%R0}",
			       xoperands);
	      return "ldw 0(%R0),%0\n\tldw 4(%R0),%R0";
	    }
	}
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adjust_address (operands[0], SImode, 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adjust_address (operands[1], SImode, 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     This can happen in two cases:

	mem -> register where the first half of the destination register
 	is the same register used in the memory's address.  Reload
	can create such insns.

	mem in this case will be either register indirect or register
	indirect plus a valid offset.

	register -> register move where REGNO(dst) == REGNO(src + 1)
	someone (Tim/Tege?) claimed this can happen for parameter loads.

     Handle mem -> register case first.  */
  if (optype0 == REGOP
      && (optype1 == MEMOP || optype1 == OFFSOP)
      && refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			    operands[1], 0))
    {
      /* Do the late half first.  */
      if (addreg1)
	output_asm_insn ("ldo 4(%0),%0", &addreg1);
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Then clobber.  */
      if (addreg1)
	output_asm_insn ("ldo -4(%0),%0", &addreg1);
      return singlemove_string (operands);
    }

  /* Now handle register -> register case.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (operands[1]) + 1)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("ldo 4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo 4(%0),%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("ldo -4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo -4(%0),%0", &addreg1);

  return "";
}

const char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1])
	  || operands[1] == CONST0_RTX (GET_MODE (operands[0])))
	output_asm_insn ("fcpy,dbl %f1,%0", operands);
      else
	output_asm_insn ("fldd%F1 %1,%0", operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      output_asm_insn ("fstd%F0 %1,%0", operands);
    }
  else if (operands[1] == CONST0_RTX (GET_MODE (operands[0])))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
	  xoperands[0] = operands[0];
	  output_asm_insn ("copy %%r0,%0\n\tcopy %%r0,%1", xoperands);
	}
      /* This is a pain.  You have to be prepared to deal with an
	 arbitrary address here including pre/post increment/decrement.

	 so avoid this in the MD.  */
      else
	abort ();
    }
  else abort ();
  return "";
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination pointer as a REG, clobbered.
   OPERANDS[1] is the source pointer as a REG, clobbered.
   OPERANDS[2] is a register for temporary storage.
   OPERANDS[4] is the size as a CONST_INT
   OPERANDS[3] is a register for temporary storage.
   OPERANDS[5] is the alignment safe to use, as a CONST_INT.
   OPERANDS[6] is another temporary register.   */

const char *
output_block_move (operands, size_is_constant)
     rtx *operands;
     int size_is_constant ATTRIBUTE_UNUSED;
{
  int align = INTVAL (operands[5]);
  unsigned long n_bytes = INTVAL (operands[4]);

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  /* Note that we know each loop below will execute at least twice
     (else we would have open-coded the copy).  */
  switch (align)
    {
      case 4:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 8);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("{ldws|ldw},ma 4(%1),%3", operands);
	output_asm_insn ("{ldws|ldw},ma 4(%1),%6", operands);
	output_asm_insn ("{stws|stw},ma %3,4(%0)", operands);
	output_asm_insn ("addib,>= -8,%2,.-12", operands);
	output_asm_insn ("{stws|stw},ma %6,4(%0)", operands);

	/* Handle the residual.  There could be up to 7 bytes of
	   residual to copy!  */
	if (n_bytes % 8 != 0)
	  {
	    operands[4] = GEN_INT (n_bytes % 4);
	    if (n_bytes % 8 >= 4)
	      output_asm_insn ("{ldws|ldw},ma 4(%1),%3", operands);
	    if (n_bytes % 4 != 0)
	      output_asm_insn ("ldw 0(%1),%6", operands);
	    if (n_bytes % 8 >= 4)
	      output_asm_insn ("{stws|stw},ma %3,4(%0)", operands);
	    if (n_bytes % 4 != 0)
	      output_asm_insn ("{stbys|stby},e %6,%4(%0)", operands);
	  }
	return "";

      case 2:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 4);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("{ldhs|ldh},ma 2(%1),%3", operands);
	output_asm_insn ("{ldhs|ldh},ma 2(%1),%6", operands);
	output_asm_insn ("{sths|sth},ma %3,2(%0)", operands);
	output_asm_insn ("addib,>= -4,%2,.-12", operands);
	output_asm_insn ("{sths|sth},ma %6,2(%0)", operands);

	/* Handle the residual.  */
	if (n_bytes % 4 != 0)
	  {
	    if (n_bytes % 4 >= 2)
	      output_asm_insn ("{ldhs|ldh},ma 2(%1),%3", operands);
	    if (n_bytes % 2 != 0)
	      output_asm_insn ("ldb 0(%1),%6", operands);
	    if (n_bytes % 4 >= 2)
	      output_asm_insn ("{sths|sth},ma %3,2(%0)", operands);
	    if (n_bytes % 2 != 0)
	      output_asm_insn ("stb %6,0(%0)", operands);
	  }
	return "";

      case 1:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 2);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("{ldbs|ldb},ma 1(%1),%3", operands);
	output_asm_insn ("{ldbs|ldb},ma 1(%1),%6", operands);
	output_asm_insn ("{stbs|stb},ma %3,1(%0)", operands);
	output_asm_insn ("addib,>= -2,%2,.-12", operands);
	output_asm_insn ("{stbs|stb},ma %6,1(%0)", operands);

	/* Handle the residual.  */
	if (n_bytes % 2 != 0)
	  {
	    output_asm_insn ("ldb 0(%1),%3", operands);
	    output_asm_insn ("stb %3,0(%0)", operands);
	  }
	return "";

      default:
	abort ();
    }
}

/* Count the number of insns necessary to handle this block move.

   Basic structure is the same as emit_block_move, except that we
   count insns rather than emit them.  */

static int
compute_movstrsi_length (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  unsigned int align = INTVAL (XEXP (XVECEXP (pat, 0, 7), 0));
  unsigned long n_bytes = INTVAL (XEXP (XVECEXP (pat, 0, 6), 0));
  unsigned int n_insns = 0;

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  /* The basic copying loop.  */
  n_insns = 6;

  /* Residuals.  */
  if (n_bytes % (2 * align) != 0)
    {
      if ((n_bytes % (2 * align)) >= align)
	n_insns += 2;

      if ((n_bytes % align) != 0)
	n_insns += 2;
    }

  /* Lengths are expressed in bytes now; each insn is 4 bytes.  */
  return n_insns * 4;
}


const char *
output_and (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    {
      unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
      int ls0, ls1, ms0, p, len;

      for (ls0 = 0; ls0 < 32; ls0++)
	if ((mask & (1 << ls0)) == 0)
	  break;

      for (ls1 = ls0; ls1 < 32; ls1++)
	if ((mask & (1 << ls1)) != 0)
	  break;

      for (ms0 = ls1; ms0 < 32; ms0++)
	if ((mask & (1 << ms0)) == 0)
	  break;

      if (ms0 != 32)
	abort ();

      if (ls1 == 32)
	{
	  len = ls0;

	  if (len == 0)
	    abort ();

	  operands[2] = GEN_INT (len);
	  return "{extru|extrw,u} %1,31,%2,%0";
	}
      else
	{
	  /* We could use this `depi' for the case above as well, but `depi'
	     requires one more register file access than an `extru'.  */

	  p = 31 - ls0;
	  len = ls1 - ls0;

	  operands[2] = GEN_INT (p);
	  operands[3] = GEN_INT (len);
	  return "{depi|depwi} 0,%2,%3,%0";
	}
    }
  else
    return "and %1,%2,%0";
}

/* Return a string to perform a bitwise-and of operands[1] with operands[2]
   storing the result in operands[0].  */
const char *
output_64bit_and (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    {
      unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
      int ls0, ls1, ms0, p, len;

      for (ls0 = 0; ls0 < HOST_BITS_PER_WIDE_INT; ls0++)
	if ((mask & ((unsigned HOST_WIDE_INT) 1 << ls0)) == 0)
	  break;

      for (ls1 = ls0; ls1 < HOST_BITS_PER_WIDE_INT; ls1++)
	if ((mask & ((unsigned HOST_WIDE_INT) 1 << ls1)) != 0)
	  break;

      for (ms0 = ls1; ms0 < HOST_BITS_PER_WIDE_INT; ms0++)
	if ((mask & ((unsigned HOST_WIDE_INT) 1 << ms0)) == 0)
	  break;

      if (ms0 != HOST_BITS_PER_WIDE_INT)
	abort ();

      if (ls1 == HOST_BITS_PER_WIDE_INT)
	{
	  len = ls0;

	  if (len == 0)
	    abort ();

	  operands[2] = GEN_INT (len);
	  return "extrd,u %1,63,%2,%0";
	}
      else
	{
	  /* We could use this `depi' for the case above as well, but `depi'
	     requires one more register file access than an `extru'.  */

	  p = 63 - ls0;
	  len = ls1 - ls0;

	  operands[2] = GEN_INT (p);
	  operands[3] = GEN_INT (len);
	  return "depdi 0,%2,%3,%0";
	}
    }
  else
    return "and %1,%2,%0";
}

const char *
output_ior (operands)
     rtx *operands;
{
  unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
  int bs0, bs1, p, len;

  if (INTVAL (operands[2]) == 0)
    return "copy %1,%0";

  for (bs0 = 0; bs0 < 32; bs0++)
    if ((mask & (1 << bs0)) != 0)
      break;

  for (bs1 = bs0; bs1 < 32; bs1++)
    if ((mask & (1 << bs1)) == 0)
      break;

  if (bs1 != 32 && ((unsigned HOST_WIDE_INT) 1 << bs1) <= mask)
    abort ();

  p = 31 - bs0;
  len = bs1 - bs0;

  operands[2] = GEN_INT (p);
  operands[3] = GEN_INT (len);
  return "{depi|depwi} -1,%2,%3,%0";
}

/* Return a string to perform a bitwise-and of operands[1] with operands[2]
   storing the result in operands[0].  */
const char *
output_64bit_ior (operands)
     rtx *operands;
{
  unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
  int bs0, bs1, p, len;

  if (INTVAL (operands[2]) == 0)
    return "copy %1,%0";

  for (bs0 = 0; bs0 < HOST_BITS_PER_WIDE_INT; bs0++)
    if ((mask & ((unsigned HOST_WIDE_INT) 1 << bs0)) != 0)
      break;

  for (bs1 = bs0; bs1 < HOST_BITS_PER_WIDE_INT; bs1++)
    if ((mask & ((unsigned HOST_WIDE_INT) 1 << bs1)) == 0)
      break;

  if (bs1 != HOST_BITS_PER_WIDE_INT
      && ((unsigned HOST_WIDE_INT) 1 << bs1) <= mask)
    abort ();

  p = 63 - bs0;
  len = bs1 - bs0;

  operands[2] = GEN_INT (p);
  operands[3] = GEN_INT (len);
  return "depdi -1,%2,%3,%0";
}

/* Target hook for assembling integer objects.  This code handles
   aligned SI and DI integers specially, since function references must
   be preceded by P%.  */

static bool
pa_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (size == UNITS_PER_WORD && aligned_p
      && function_label_operand (x, VOIDmode))
    {
      fputs (size == 8? "\t.dword\tP%" : "\t.word\tP%", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputc ('\n', asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Output an ascii string.  */
void
output_ascii (file, p, size)
     FILE *file;
     const char *p;
     int size;
{
  int i;
  int chars_output;
  unsigned char partial_output[16];	/* Max space 4 chars can occupy.   */

  /* The HP assembler can only take strings of 256 characters at one
     time.  This is a limitation on input line length, *not* the
     length of the string.  Sigh.  Even worse, it seems that the
     restriction is in number of input characters (see \xnn &
     \whatever).  So we have to do this very carefully.  */

  fputs ("\t.STRING \"", file);

  chars_output = 0;
  for (i = 0; i < size; i += 4)
    {
      int co = 0;
      int io = 0;
      for (io = 0, co = 0; io < MIN (4, size - i); io++)
	{
	  register unsigned int c = (unsigned char) p[i + io];

	  if (c == '\"' || c == '\\')
	    partial_output[co++] = '\\';
	  if (c >= ' ' && c < 0177)
	    partial_output[co++] = c;
	  else
	    {
	      unsigned int hexd;
	      partial_output[co++] = '\\';
	      partial_output[co++] = 'x';
	      hexd =  c  / 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	      hexd =  c % 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	    }
	}
      if (chars_output + co > 243)
	{
	  fputs ("\"\n\t.STRING \"", file);
	  chars_output = 0;
	}
      fwrite (partial_output, 1, (size_t) co, file);
      chars_output += co;
      co = 0;
    }
  fputs ("\"\n", file);
}

/* Try to rewrite floating point comparisons & branches to avoid
   useless add,tr insns.

   CHECK_NOTES is nonzero if we should examine REG_DEAD notes
   to see if FPCC is dead.  CHECK_NOTES is nonzero for the
   first attempt to remove useless add,tr insns.  It is zero
   for the second pass as reorg sometimes leaves bogus REG_DEAD
   notes lying around.

   When CHECK_NOTES is zero we can only eliminate add,tr insns
   when there's a 1:1 correspondence between fcmp and ftest/fbranch
   instructions.  */
static void
remove_useless_addtr_insns (insns, check_notes)
     rtx insns;
     int check_notes;
{
  rtx insn;
  static int pass = 0;

  /* This is fairly cheap, so always run it when optimizing.  */
  if (optimize > 0)
    {
      int fcmp_count = 0;
      int fbranch_count = 0;

      /* Walk all the insns in this function looking for fcmp & fbranch
	 instructions.  Keep track of how many of each we find.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = next_insn (insn))
	{
	  rtx tmp;

	  /* Ignore anything that isn't an INSN or a JUMP_INSN.  */
	  if (GET_CODE (insn) != INSN && GET_CODE (insn) != JUMP_INSN)
	    continue;

	  tmp = PATTERN (insn);

	  /* It must be a set.  */
	  if (GET_CODE (tmp) != SET)
	    continue;

	  /* If the destination is CCFP, then we've found an fcmp insn.  */
	  tmp = SET_DEST (tmp);
	  if (GET_CODE (tmp) == REG && REGNO (tmp) == 0)
	    {
	      fcmp_count++;
	      continue;
	    }

	  tmp = PATTERN (insn);
	  /* If this is an fbranch instruction, bump the fbranch counter.  */
	  if (GET_CODE (tmp) == SET
	      && SET_DEST (tmp) == pc_rtx
	      && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
	      && GET_CODE (XEXP (SET_SRC (tmp), 0)) == NE
	      && GET_CODE (XEXP (XEXP (SET_SRC (tmp), 0), 0)) == REG
	      && REGNO (XEXP (XEXP (SET_SRC (tmp), 0), 0)) == 0)
	    {
	      fbranch_count++;
	      continue;
	    }
	}


      /* Find all floating point compare + branch insns.  If possible,
	 reverse the comparison & the branch to avoid add,tr insns.  */
      for (insn = insns; insn; insn = next_insn (insn))
	{
	  rtx tmp, next;

	  /* Ignore anything that isn't an INSN.  */
	  if (GET_CODE (insn) != INSN)
	    continue;

	  tmp = PATTERN (insn);

	  /* It must be a set.  */
	  if (GET_CODE (tmp) != SET)
	    continue;

	  /* The destination must be CCFP, which is register zero.  */
	  tmp = SET_DEST (tmp);
	  if (GET_CODE (tmp) != REG || REGNO (tmp) != 0)
	    continue;

	  /* INSN should be a set of CCFP.

	     See if the result of this insn is used in a reversed FP
	     conditional branch.  If so, reverse our condition and
	     the branch.  Doing so avoids useless add,tr insns.  */
	  next = next_insn (insn);
	  while (next)
	    {
	      /* Jumps, calls and labels stop our search.  */
	      if (GET_CODE (next) == JUMP_INSN
		  || GET_CODE (next) == CALL_INSN
		  || GET_CODE (next) == CODE_LABEL)
		break;

	      /* As does another fcmp insn.  */
	      if (GET_CODE (next) == INSN
		  && GET_CODE (PATTERN (next)) == SET
		  && GET_CODE (SET_DEST (PATTERN (next))) == REG
		  && REGNO (SET_DEST (PATTERN (next))) == 0)
		break;

	      next = next_insn (next);
	    }

	  /* Is NEXT_INSN a branch?  */
	  if (next
	      && GET_CODE (next) == JUMP_INSN)
	    {
	      rtx pattern = PATTERN (next);

	      /* If it a reversed fp conditional branch (eg uses add,tr)
		 and CCFP dies, then reverse our conditional and the branch
		 to avoid the add,tr.  */
	      if (GET_CODE (pattern) == SET
		  && SET_DEST (pattern) == pc_rtx
		  && GET_CODE (SET_SRC (pattern)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC (pattern), 0)) == NE
		  && GET_CODE (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == REG
		  && REGNO (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == 0
		  && GET_CODE (XEXP (SET_SRC (pattern), 1)) == PC
		  && (fcmp_count == fbranch_count
		      || (check_notes
			  && find_regno_note (next, REG_DEAD, 0))))
		{
		  /* Reverse the branch.  */
		  tmp = XEXP (SET_SRC (pattern), 1);
		  XEXP (SET_SRC (pattern), 1) = XEXP (SET_SRC (pattern), 2);
		  XEXP (SET_SRC (pattern), 2) = tmp;
		  INSN_CODE (next) = -1;

		  /* Reverse our condition.  */
		  tmp = PATTERN (insn);
		  PUT_CODE (XEXP (tmp, 1),
			    (reverse_condition_maybe_unordered
			     (GET_CODE (XEXP (tmp, 1)))));
		}
	    }
	}
    }

  pass = !pass;

}

/* You may have trouble believing this, but this is the 32 bit HP-PA
   stack layout.  Wow.

   Offset		Contents

   Variable arguments	(optional; any number may be allocated)

   SP-(4*(N+9))		arg word N
   	:		    :
      SP-56		arg word 5
      SP-52		arg word 4

   Fixed arguments	(must be allocated; may remain unused)

      SP-48		arg word 3
      SP-44		arg word 2
      SP-40		arg word 1
      SP-36		arg word 0

   Frame Marker

      SP-32		External Data Pointer (DP)
      SP-28		External sr4
      SP-24		External/stub RP (RP')
      SP-20		Current RP
      SP-16		Static Link
      SP-12		Clean up
      SP-8		Calling Stub RP (RP'')
      SP-4		Previous SP

   Top of Frame

      SP-0		Stack Pointer (points to next available address)

*/

/* This function saves registers as follows.  Registers marked with ' are
   this function's registers (as opposed to the previous function's).
   If a frame_pointer isn't needed, r4 is saved as a general register;
   the space for the frame pointer is still allocated, though, to keep
   things simple.


   Top of Frame

       SP (FP')		Previous FP
       SP + 4		Alignment filler (sigh)
       SP + 8		Space for locals reserved here.
       .
       .
       .
       SP + n		All call saved register used.
       .
       .
       .
       SP + o		All call saved fp registers used.
       .
       .
       .
       SP + p (SP')	points to next available address.

*/

/* Global variables set by output_function_prologue().  */
/* Size of frame.  Need to know this to emit return insns from
   leaf procedures.  */
static int actual_fsize;
static int local_fsize, save_fregs;

/* Emit RTL to store REG at the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const patterns.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/

static void
store_reg (reg, disp, base)
     int reg, disp, base;
{
  rtx insn, dest, src, basereg;

  src = gen_rtx_REG (word_mode, reg);
  basereg = gen_rtx_REG (Pmode, base);
  if (VAL_14_BITS_P (disp))
    {
      dest = gen_rtx_MEM (word_mode, plus_constant (basereg, disp));
      insn = emit_move_insn (dest, src);
    }
  else
    {
      rtx delta = GEN_INT (disp);
      rtx high = gen_rtx_PLUS (Pmode, basereg, gen_rtx_HIGH (Pmode, delta));
      rtx tmpreg = gen_rtx_REG (Pmode, 1);
      emit_move_insn (tmpreg, high);
      dest = gen_rtx_MEM (word_mode, gen_rtx_LO_SUM (Pmode, tmpreg, delta));
      insn = emit_move_insn (dest, src);
      if (DO_FRAME_NOTES)
	{
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode,
			     gen_rtx_MEM (word_mode,
					  gen_rtx_PLUS (word_mode, basereg,
							delta)),
                             src),
                REG_NOTES (insn));
	}
    }

  if (DO_FRAME_NOTES)
    RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit RTL to store REG at the memory location specified by BASE and then
   add MOD to BASE.  MOD must be <= 8k.  */

static void
store_reg_modify (base, reg, mod)
     int base, reg, mod;
{
  rtx insn, basereg, srcreg, delta;

  if (! VAL_14_BITS_P (mod))
    abort ();

  basereg = gen_rtx_REG (Pmode, base);
  srcreg = gen_rtx_REG (word_mode, reg);
  delta = GEN_INT (mod);

  insn = emit_insn (gen_post_store (basereg, srcreg, delta));
  if (DO_FRAME_NOTES)
    {
      RTX_FRAME_RELATED_P (insn) = 1;

      /* RTX_FRAME_RELATED_P must be set on each frame related set
	 in a parallel with more than one element.  Don't set
	 RTX_FRAME_RELATED_P in the first set if reg is temporary
	 register 1. The effect of this operation is recorded in
	 the initial copy.  */
      if (reg != 1)
	{
	  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 0)) = 1;
	  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
	}
      else
	{
	  /* The first element of a PARALLEL is always processed if it is
	     a SET.  Thus, we need an expression list for this case.  */
	  REG_NOTES (insn)
	    = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode, basereg,
			     gen_rtx_PLUS (word_mode, basereg, delta)),
                REG_NOTES (insn));
	}
    }
}

/* Emit RTL to set REG to the value specified by BASE+DISP.  Handle case
   where DISP > 8k by using the add_high_const patterns.  NOTE indicates
   whether to add a frame note or not.

   In the DISP > 8k case, we leave the high part of the address in %r1.
   There is code in expand_hppa_{prologue,epilogue} that knows about this.  */

static void
set_reg_plus_d (reg, base, disp, note)
     int reg, base, disp, note;
{
  rtx insn;

  if (VAL_14_BITS_P (disp))
    {
      insn = emit_move_insn (gen_rtx_REG (Pmode, reg),
			     plus_constant (gen_rtx_REG (Pmode, base), disp));
    }
  else
    {
      rtx basereg = gen_rtx_REG (Pmode, base);
      rtx delta = GEN_INT (disp);

      emit_move_insn (gen_rtx_REG (Pmode, 1),
		      gen_rtx_PLUS (Pmode, basereg,
				    gen_rtx_HIGH (Pmode, delta)));
      insn = emit_move_insn (gen_rtx_REG (Pmode, reg),
			     gen_rtx_LO_SUM (Pmode, gen_rtx_REG (Pmode, 1),
					     delta));
    }

  if (DO_FRAME_NOTES && note)
    RTX_FRAME_RELATED_P (insn) = 1;
}

int
compute_frame_size (size, fregs_live)
     int size;
     int *fregs_live;
{
  int i, fsize;

  /* Space for frame pointer + filler. If any frame is allocated
     we need to add this in because of STARTING_FRAME_OFFSET.

     Similar code also appears in hppa_expand_prologue.  Change both
     of them at the same time.  */
  fsize = size + (size || frame_pointer_needed ? STARTING_FRAME_OFFSET : 0);

  /* If the current function calls __builtin_eh_return, then we need
     to allocate stack space for registers that will hold data for
     the exception handler.  */
  if (DO_FRAME_NOTES && current_function_calls_eh_return)
    {
      unsigned int i;

      for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; ++i)
	continue;
      fsize += i * UNITS_PER_WORD;
    }

  /* Account for space used by the callee general register saves.  */
  for (i = 18; i >= 3; i--)
    if (regs_ever_live[i])
      fsize += UNITS_PER_WORD;

  /* Round the stack.  */
  fsize = (fsize + 7) & ~7;

  /* Account for space used by the callee floating point register saves.  */
  for (i = FP_SAVED_REG_LAST; i >= FP_SAVED_REG_FIRST; i -= FP_REG_STEP)
    if (regs_ever_live[i]
	|| (! TARGET_64BIT && regs_ever_live[i + 1]))
      {
	if (fregs_live)
	  *fregs_live = 1;

	/* We always save both halves of the FP register, so always
	   increment the frame size by 8 bytes.  */
	fsize += 8;
      }

  /* The various ABIs include space for the outgoing parameters in the
     size of the current function's stack frame.  */
  fsize += current_function_outgoing_args_size;

  /* Allocate space for the fixed frame marker.  This space must be
     allocated for any function that makes calls or allocates
     stack space.  */
  if (!current_function_is_leaf || fsize)
    fsize += TARGET_64BIT ? 48 : 32;

  return ((fsize + PREFERRED_STACK_BOUNDARY / 8 - 1)
	  & ~(PREFERRED_STACK_BOUNDARY / 8 - 1));
}

/* Generate the assembly code for function entry.  FILE is a stdio
   stream to output the code to.  SIZE is an int: how many units of
   temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers to
   save; `regs_ever_live[I]' is nonzero if register number I is ever
   used in the function.  This function is responsible for knowing
   which registers should not be saved even if used.  */

/* On HP-PA, move-double insns between fpu and cpu need an 8-byte block
   of memory.  If any fpu reg is used in the function, we allocate
   such a block here, at the bottom of the frame, just in case it's needed.

   If this function is a leaf procedure, then we may choose not
   to do a "save" insn.  The decision about whether or not
   to do this is made in regclass.c.  */

static void
pa_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  /* The function's label and associated .PROC must never be
     separated and must be output *after* any profiling declarations
     to avoid changing spaces/subspaces within a procedure.  */
  ASM_OUTPUT_LABEL (file, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));
  fputs ("\t.PROC\n", file);

  /* hppa_expand_prologue does the dirty work now.  We just need
     to output the assembler directives which denote the start
     of a function.  */
  fprintf (file, "\t.CALLINFO FRAME=%d", actual_fsize);
  if (regs_ever_live[2])
    fputs (",CALLS,SAVE_RP", file);
  else
    fputs (",NO_CALLS", file);

  /* The SAVE_SP flag is used to indicate that register %r3 is stored
     at the beginning of the frame and that it is used as the frame
     pointer for the frame.  We do this because our current frame
     layout doesn't conform to that specified in the the HP runtime
     documentation and we need a way to indicate to programs such as
     GDB where %r3 is saved.  The SAVE_SP flag was chosen because it
     isn't used by HP compilers but is supported by the assembler.
     However, SAVE_SP is supposed to indicate that the previous stack
     pointer has been saved in the frame marker.  */
  if (frame_pointer_needed)
    fputs (",SAVE_SP", file);

  /* Pass on information about the number of callee register saves
     performed in the prologue.

     The compiler is supposed to pass the highest register number
     saved, the assembler then has to adjust that number before
     entering it into the unwind descriptor (to account for any
     caller saved registers with lower register numbers than the
     first callee saved register).  */
  if (gr_saved)
    fprintf (file, ",ENTRY_GR=%d", gr_saved + 2);

  if (fr_saved)
    fprintf (file, ",ENTRY_FR=%d", fr_saved + 11);

  fputs ("\n\t.ENTRY\n", file);

  remove_useless_addtr_insns (get_insns (), 0);
}

void
hppa_expand_prologue ()
{
  int size = get_frame_size ();
  int merge_sp_adjust_with_store = 0;
  int i, offset;
  rtx insn, tmpreg;

  gr_saved = 0;
  fr_saved = 0;
  save_fregs = 0;

  /* Allocate space for frame pointer + filler. If any frame is allocated
     we need to add this in because of STARTING_FRAME_OFFSET.

     Similar code also appears in compute_frame_size.  Change both
     of them at the same time.  */
  local_fsize = size + (size || frame_pointer_needed
			? STARTING_FRAME_OFFSET : 0);

  actual_fsize = compute_frame_size (size, &save_fregs);

  /* Compute a few things we will use often.  */
  tmpreg = gen_rtx_REG (word_mode, 1);

  /* Save RP first.  The calling conventions manual states RP will
     always be stored into the caller's frame at sp - 20 or sp - 16
     depending on which ABI is in use.  */
  if (regs_ever_live[2] || current_function_calls_eh_return)
    store_reg (2, TARGET_64BIT ? -16 : -20, STACK_POINTER_REGNUM);

  /* Allocate the local frame and set up the frame pointer if needed.  */
  if (actual_fsize != 0)
    {
      if (frame_pointer_needed)
	{
	  /* Copy the old frame pointer temporarily into %r1.  Set up the
	     new stack pointer, then store away the saved old frame pointer
	     into the stack at sp and at the same time update the stack
	     pointer by actual_fsize bytes.  Two versions, first
	     handles small (<8k) frames.  The second handles large (>=8k)
	     frames.  */
	  insn = emit_move_insn (tmpreg, frame_pointer_rtx);
	  if (DO_FRAME_NOTES)
	    {
	      /* We need to record the frame pointer save here since the
	         new frame pointer is set in the following insn.  */
	      RTX_FRAME_RELATED_P (insn) = 1;
	      REG_NOTES (insn)
		= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (word_mode, stack_pointer_rtx),
			         frame_pointer_rtx),
		    REG_NOTES (insn));
	    }

	  insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
	  if (DO_FRAME_NOTES)
	    RTX_FRAME_RELATED_P (insn) = 1;

	  if (VAL_14_BITS_P (actual_fsize))
	    store_reg_modify (STACK_POINTER_REGNUM, 1, actual_fsize);
	  else
	    {
	      /* It is incorrect to store the saved frame pointer at *sp,
		 then increment sp (writes beyond the current stack boundary).

		 So instead use stwm to store at *sp and post-increment the
		 stack pointer as an atomic operation.  Then increment sp to
		 finish allocating the new frame.  */
	      int adjust1 = 8192 - 64;
	      int adjust2 = actual_fsize - adjust1;

	      store_reg_modify (STACK_POINTER_REGNUM, 1, adjust1);
	      set_reg_plus_d (STACK_POINTER_REGNUM, STACK_POINTER_REGNUM,
			      adjust2, 1);
	    }

	  /* We set SAVE_SP in frames that need a frame pointer.  Thus,
	     we need to store the previous stack pointer (frame pointer)
	     into the frame marker on targets that use the HP unwind
	     library.  This allows the HP unwind library to be used to
	     unwind GCC frames.  However, we are not fully compatible
	     with the HP library because our frame layout differs from
	     that specified in the HP runtime specification.

	     We don't want a frame note on this instruction as the frame
	     marker moves during dynamic stack allocation.

	     This instruction also serves as a blockage to prevent
	     register spills from being scheduled before the stack
	     pointer is raised.  This is necessary as we store
	     registers using the frame pointer as a base register,
	     and the frame pointer is set before sp is raised.  */
	  if (TARGET_HPUX_UNWIND_LIBRARY)
	    {
	      rtx addr = gen_rtx_PLUS (word_mode, stack_pointer_rtx,
				       GEN_INT (TARGET_64BIT ? -8 : -4));

	      emit_move_insn (gen_rtx_MEM (word_mode, addr),
			      frame_pointer_rtx);
	    }
	  else
	    emit_insn (gen_blockage ());
	}
      /* no frame pointer needed.  */
      else
	{
	  /* In some cases we can perform the first callee register save
	     and allocating the stack frame at the same time.   If so, just
	     make a note of it and defer allocating the frame until saving
	     the callee registers.  */
	  if (VAL_14_BITS_P (actual_fsize) && local_fsize == 0)
	    merge_sp_adjust_with_store = 1;
	  /* Can not optimize.  Adjust the stack frame by actual_fsize
	     bytes.  */
	  else
	    set_reg_plus_d (STACK_POINTER_REGNUM, STACK_POINTER_REGNUM,
			    actual_fsize, 1);
	}
    }

  /* Normal register save.

     Do not save the frame pointer in the frame_pointer_needed case.  It
     was done earlier.  */
  if (frame_pointer_needed)
    {
      offset = local_fsize;

      /* Saving the EH return data registers in the frame is the simplest
	 way to get the frame unwind information emitted.  We put them
	 just before the general registers.  */
      if (DO_FRAME_NOTES && current_function_calls_eh_return)
	{
	  unsigned int i, regno;

	  for (i = 0; ; ++i)
	    {
	      regno = EH_RETURN_DATA_REGNO (i);
	      if (regno == INVALID_REGNUM)
		break;

	      store_reg (regno, offset, FRAME_POINTER_REGNUM);
	      offset += UNITS_PER_WORD;
	    }
	}

      for (i = 18; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    store_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += UNITS_PER_WORD;
	    gr_saved++;
	  }
      /* Account for %r3 which is saved in a special place.  */
      gr_saved++;
    }
  /* No frame pointer needed.  */
  else
    {
      offset = local_fsize - actual_fsize;

      /* Saving the EH return data registers in the frame is the simplest
         way to get the frame unwind information emitted.  */
      if (DO_FRAME_NOTES && current_function_calls_eh_return)
	{
	  unsigned int i, regno;

	  for (i = 0; ; ++i)
	    {
	      regno = EH_RETURN_DATA_REGNO (i);
	      if (regno == INVALID_REGNUM)
		break;

	      /* If merge_sp_adjust_with_store is nonzero, then we can
		 optimize the first save.  */
	      if (merge_sp_adjust_with_store)
		{
		  store_reg_modify (STACK_POINTER_REGNUM, regno, -offset);
		  merge_sp_adjust_with_store = 0;
		}
	      else
		store_reg (regno, offset, STACK_POINTER_REGNUM);
	      offset += UNITS_PER_WORD;
	    }
	}

      for (i = 18; i >= 3; i--)
      	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    /* If merge_sp_adjust_with_store is nonzero, then we can
	       optimize the first GR save.  */
	    if (merge_sp_adjust_with_store)
	      {
		store_reg_modify (STACK_POINTER_REGNUM, i, -offset);
		merge_sp_adjust_with_store = 0;
	      }
	    else
	      store_reg (i, offset, STACK_POINTER_REGNUM);
	    offset += UNITS_PER_WORD;
	    gr_saved++;
	  }

      /* If we wanted to merge the SP adjustment with a GR save, but we never
	 did any GR saves, then just emit the adjustment here.  */
      if (merge_sp_adjust_with_store)
	set_reg_plus_d (STACK_POINTER_REGNUM, STACK_POINTER_REGNUM,
			actual_fsize, 1);
    }

  /* The hppa calling conventions say that %r19, the pic offset
     register, is saved at sp - 32 (in this function's frame)
     when generating PIC code.  FIXME:  What is the correct thing
     to do for functions which make no calls and allocate no
     frame?  Do we need to allocate a frame, or can we just omit
     the save?   For now we'll just omit the save.  */
  if (flag_pic && actual_fsize != 0 && !TARGET_64BIT)
    store_reg (PIC_OFFSET_TABLE_REGNUM, -32, STACK_POINTER_REGNUM);

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* Floating point register store.  */
  if (save_fregs)
    {
      rtx base;

      /* First get the frame or stack pointer to the start of the FP register
	 save area.  */
      if (frame_pointer_needed)
	{
	  set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset, 0);
	  base = frame_pointer_rtx;
	}
      else
	{
	  set_reg_plus_d (1, STACK_POINTER_REGNUM, offset, 0);
	  base = stack_pointer_rtx;
	}

      /* Now actually save the FP registers.  */
      for (i = FP_SAVED_REG_LAST; i >= FP_SAVED_REG_FIRST; i -= FP_REG_STEP)
	{
	  if (regs_ever_live[i]
	      || (! TARGET_64BIT && regs_ever_live[i + 1]))
	    {
	      rtx addr, insn, reg;
	      addr = gen_rtx_MEM (DFmode, gen_rtx_POST_INC (DFmode, tmpreg));
	      reg = gen_rtx_REG (DFmode, i);
	      insn = emit_move_insn (addr, reg);
	      if (DO_FRAME_NOTES)
		{
		  RTX_FRAME_RELATED_P (insn) = 1;
		  if (TARGET_64BIT)
		    {
		      rtx mem = gen_rtx_MEM (DFmode,
					     plus_constant (base, offset));
		      REG_NOTES (insn)
			= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					     gen_rtx_SET (VOIDmode, mem, reg),
					     REG_NOTES (insn));
		    }
		  else
		    {
		      rtx meml = gen_rtx_MEM (SFmode,
					      plus_constant (base, offset));
		      rtx memr = gen_rtx_MEM (SFmode,
					      plus_constant (base, offset + 4));
		      rtx regl = gen_rtx_REG (SFmode, i);
		      rtx regr = gen_rtx_REG (SFmode, i + 1);
		      rtx setl = gen_rtx_SET (VOIDmode, meml, regl);
		      rtx setr = gen_rtx_SET (VOIDmode, memr, regr);
		      rtvec vec;

		      RTX_FRAME_RELATED_P (setl) = 1;
		      RTX_FRAME_RELATED_P (setr) = 1;
		      vec = gen_rtvec (2, setl, setr);
		      REG_NOTES (insn)
			= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					     gen_rtx_SEQUENCE (VOIDmode, vec),
					     REG_NOTES (insn));
		    }
		}
	      offset += GET_MODE_SIZE (DFmode);
	      fr_saved++;
	    }
	}
    }

  /* FIXME: expand_call and expand_millicode_call need to be fixed to
     prevent insns with frame notes being scheduled in the delay slot
     of calls.  This causes problems because the dwarf2 output code
     processes the insn list serially.  For now, limit the migration
     of prologue insns with a blockage.  */
  if (DO_FRAME_NOTES)
    emit_insn (gen_blockage ());
}

/* Emit RTL to load REG from the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const patterns.  */

static void
load_reg (reg, disp, base)
     int reg, disp, base;
{
  rtx src, dest, basereg;

  dest = gen_rtx_REG (word_mode, reg);
  basereg = gen_rtx_REG (Pmode, base);
  if (VAL_14_BITS_P (disp))
    {
      src = gen_rtx_MEM (word_mode, plus_constant (basereg, disp));
      emit_move_insn (dest, src);
    }
  else
    {
      rtx delta = GEN_INT (disp);
      rtx high = gen_rtx_PLUS (Pmode, basereg, gen_rtx_HIGH (Pmode, delta));
      rtx tmpreg = gen_rtx_REG (Pmode, 1);
      emit_move_insn (tmpreg, high);
      src = gen_rtx_MEM (word_mode, gen_rtx_LO_SUM (Pmode, tmpreg, delta));
      emit_move_insn (dest, src);
    }
}

/* This function generates the assembly code for function exit.
   Args are as for output_function_prologue ().

   The function epilogue should not depend on the current stack
   pointer!  It should use the frame pointer only.  This is mandatory
   because of alloca; we also take advantage of it to omit stack
   adjustments before returning.  */

static void
pa_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int last_address = 0;
  rtx insn = get_last_insn ();

  /* hppa_expand_epilogue does the dirty work now.  We just need
     to output the assembler directives which denote the end
     of a function.

     To make debuggers happy, emit a nop if the epilogue was completely
     eliminated due to a volatile call as the last insn in the
     current function.  That way the return address (in %r2) will
     always point to a valid instruction in the current function.  */

  /* Get the last real insn.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_real_insn (insn);

  /* If it is a sequence, then look inside.  */
  if (insn && GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  /* If insn is a CALL_INSN, then it must be a call to a volatile
     function (otherwise there would be epilogue insns).  */
  if (insn && GET_CODE (insn) == CALL_INSN)
    {
      fputs ("\tnop\n", file);
      last_address += 4;
    }

  fputs ("\t.EXIT\n\t.PROCEND\n", file);

  /* Finally, update the total number of code bytes output so far.  */
  if ((TARGET_PORTABLE_RUNTIME || !TARGET_GAS || !TARGET_SOM)
      && !flag_function_sections)
    {
      if (INSN_ADDRESSES_SET_P ())
	{
	  unsigned long old_total = total_code_bytes;

	  insn = get_last_nonnote_insn ();
	  last_address += INSN_ADDRESSES (INSN_UID (insn));
	  if (INSN_P (insn))
	    last_address += insn_default_length (insn);

	  total_code_bytes += last_address;
	  total_code_bytes += FUNCTION_BOUNDARY / BITS_PER_UNIT;

	  /* Be prepared to handle overflows.  */
	  if (old_total > total_code_bytes)
	    total_code_bytes = -1;
	}
      else
	total_code_bytes = -1;
    }
}

void
hppa_expand_epilogue ()
{
  rtx tmpreg;
  int offset, i;
  int merge_sp_adjust_with_load = 0;
  int ret_off = 0;

  /* We will use this often.  */
  tmpreg = gen_rtx_REG (word_mode, 1);

  /* Try to restore RP early to avoid load/use interlocks when
     RP gets used in the return (bv) instruction.  This appears to still
     be necessary even when we schedule the prologue and epilogue.  */
  if (regs_ever_live [2] || current_function_calls_eh_return)
    {
      ret_off = TARGET_64BIT ? -16 : -20;
      if (frame_pointer_needed)
	{
	  load_reg (2, ret_off, FRAME_POINTER_REGNUM);
	  ret_off = 0;
	}
      else
	{
	  /* No frame pointer, and stack is smaller than 8k.  */
	  if (VAL_14_BITS_P (ret_off - actual_fsize))
	    {
	      load_reg (2, ret_off - actual_fsize, STACK_POINTER_REGNUM);
	      ret_off = 0;
	    }
	}
    }

  /* General register restores.  */
  if (frame_pointer_needed)
    {
      offset = local_fsize;

      /* If the current function calls __builtin_eh_return, then we need
         to restore the saved EH data registers.  */
      if (DO_FRAME_NOTES && current_function_calls_eh_return)
	{
	  unsigned int i, regno;

	  for (i = 0; ; ++i)
	    {
	      regno = EH_RETURN_DATA_REGNO (i);
	      if (regno == INVALID_REGNUM)
		break;

	      load_reg (regno, offset, FRAME_POINTER_REGNUM);
	      offset += UNITS_PER_WORD;
	    }
	}

      for (i = 18; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    load_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += UNITS_PER_WORD;
	  }
    }
  else
    {
      offset = local_fsize - actual_fsize;

      /* If the current function calls __builtin_eh_return, then we need
         to restore the saved EH data registers.  */
      if (DO_FRAME_NOTES && current_function_calls_eh_return)
	{
	  unsigned int i, regno;

	  for (i = 0; ; ++i)
	    {
	      regno = EH_RETURN_DATA_REGNO (i);
	      if (regno == INVALID_REGNUM)
		break;

	      /* Only for the first load.
	         merge_sp_adjust_with_load holds the register load
	         with which we will merge the sp adjustment.  */
	      if (merge_sp_adjust_with_load == 0
		  && local_fsize == 0
		  && VAL_14_BITS_P (-actual_fsize))
	        merge_sp_adjust_with_load = regno;
	      else
		load_reg (regno, offset, STACK_POINTER_REGNUM);
	      offset += UNITS_PER_WORD;
	    }
	}

      for (i = 18; i >= 3; i--)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    {
	      /* Only for the first load.
	         merge_sp_adjust_with_load holds the register load
	         with which we will merge the sp adjustment.  */
	      if (merge_sp_adjust_with_load == 0
		  && local_fsize == 0
		  && VAL_14_BITS_P (-actual_fsize))
	        merge_sp_adjust_with_load = i;
	      else
		load_reg (i, offset, STACK_POINTER_REGNUM);
	      offset += UNITS_PER_WORD;
	    }
	}
    }

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* FP register restores.  */
  if (save_fregs)
    {
      /* Adjust the register to index off of.  */
      if (frame_pointer_needed)
	set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset, 0);
      else
	set_reg_plus_d (1, STACK_POINTER_REGNUM, offset, 0);

      /* Actually do the restores now.  */
      for (i = FP_SAVED_REG_LAST; i >= FP_SAVED_REG_FIRST; i -= FP_REG_STEP)
	if (regs_ever_live[i]
	    || (! TARGET_64BIT && regs_ever_live[i + 1]))
	  {
	    rtx src = gen_rtx_MEM (DFmode, gen_rtx_POST_INC (DFmode, tmpreg));
	    rtx dest = gen_rtx_REG (DFmode, i);
	    emit_move_insn (dest, src);
	  }
    }

  /* Emit a blockage insn here to keep these insns from being moved to
     an earlier spot in the epilogue, or into the main instruction stream.

     This is necessary as we must not cut the stack back before all the
     restores are finished.  */
  emit_insn (gen_blockage ());

  /* Reset stack pointer (and possibly frame pointer).  The stack
     pointer is initially set to fp + 64 to avoid a race condition.  */
  if (frame_pointer_needed)
    {
      rtx delta = GEN_INT (-64);

      set_reg_plus_d (STACK_POINTER_REGNUM, FRAME_POINTER_REGNUM, 64, 0);
      emit_insn (gen_pre_load (frame_pointer_rtx, stack_pointer_rtx, delta));
    }
  /* If we were deferring a callee register restore, do it now.  */
  else if (merge_sp_adjust_with_load)
    {
      rtx delta = GEN_INT (-actual_fsize);
      rtx dest = gen_rtx_REG (word_mode, merge_sp_adjust_with_load);

      emit_insn (gen_pre_load (dest, stack_pointer_rtx, delta));
    }
  else if (actual_fsize != 0)
    set_reg_plus_d (STACK_POINTER_REGNUM, STACK_POINTER_REGNUM,
		    - actual_fsize, 0);

  /* If we haven't restored %r2 yet (no frame pointer, and a stack
     frame greater than 8k), do so now.  */
  if (ret_off != 0)
    load_reg (2, ret_off, STACK_POINTER_REGNUM);

  if (DO_FRAME_NOTES && current_function_calls_eh_return)
    {
      rtx sa = EH_RETURN_STACKADJ_RTX;

      emit_insn (gen_blockage ());
      emit_insn (TARGET_64BIT
		 ? gen_subdi3 (stack_pointer_rtx, stack_pointer_rtx, sa)
		 : gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, sa));
    }
}

rtx
hppa_pic_save_rtx ()
{
  return get_hard_reg_initial_val (word_mode, PIC_OFFSET_TABLE_REGNUM);
}

void
hppa_profile_hook (label_no)
     int label_no;
{
  rtx begin_label_rtx, call_insn;
  char begin_label_name[16];

  ASM_GENERATE_INTERNAL_LABEL (begin_label_name, FUNC_BEGIN_PROLOG_LABEL,
			       label_no);
  begin_label_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (begin_label_name));

  if (TARGET_64BIT)
    emit_move_insn (arg_pointer_rtx,
		    gen_rtx_PLUS (word_mode, virtual_outgoing_args_rtx,
				  GEN_INT (64)));

  emit_move_insn (gen_rtx_REG (word_mode, 26), gen_rtx_REG (word_mode, 2));

#ifndef NO_PROFILE_COUNTERS
  {
    rtx count_label_rtx, addr, r24;
    char count_label_name[16];

    ASM_GENERATE_INTERNAL_LABEL (count_label_name, "LP", label_no);
    count_label_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (count_label_name));

    addr = force_reg (Pmode, count_label_rtx);
    r24 = gen_rtx_REG (Pmode, 24);
    emit_move_insn (r24, addr);

    /* %r25 is set from within the output pattern.  */
    call_insn =
      emit_call_insn (gen_call_profiler (gen_rtx_SYMBOL_REF (Pmode, "_mcount"),
					 GEN_INT (TARGET_64BIT ? 24 : 12),
					 begin_label_rtx));

    use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), r24);
  }
#else
    /* %r25 is set from within the output pattern.  */
  call_insn =
    emit_call_insn (gen_call_profiler (gen_rtx_SYMBOL_REF (Pmode, "_mcount"),
				       GEN_INT (TARGET_64BIT ? 16 : 8),
				       begin_label_rtx));
#endif

  /* Indicate the _mcount call cannot throw, nor will it execute a
     non-local goto.  */
  REG_NOTES (call_insn)
    = gen_rtx_EXPR_LIST (REG_EH_REGION, constm1_rtx, REG_NOTES (call_insn));

  if (flag_pic)
    {
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), pic_offset_table_rtx);
      if (TARGET_64BIT)
	use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), arg_pointer_rtx);

      emit_move_insn (pic_offset_table_rtx, hppa_pic_save_rtx ());
    }
}

/* Fetch the return address for the frame COUNT steps up from
   the current frame, after the prologue.  FRAMEADDR is the
   frame pointer of the COUNT frame.

   We want to ignore any export stub remnants here.  To handle this,
   we examine the code at the return address, and if it is an export
   stub, we return a memory rtx for the stub return address stored
   at frame-24.

   The value returned is used in two different ways:

	1. To find a function's caller.

	2. To change the return address for a function.

   This function handles most instances of case 1; however, it will
   fail if there are two levels of stubs to execute on the return
   path.  The only way I believe that can happen is if the return value
   needs a parameter relocation, which never happens for C code.

   This function handles most instances of case 2; however, it will
   fail if we did not originally have stub code on the return path
   but will need stub code on the new return path.  This can happen if
   the caller & callee are both in the main program, but the new
   return location is in a shared library.  */

rtx
return_addr_rtx (count, frameaddr)
     int count;
     rtx frameaddr;
{
  rtx label;
  rtx rp;
  rtx saved_rp;
  rtx ins;

  if (count != 0)
    return NULL_RTX;

  rp = get_hard_reg_initial_val (Pmode, 2);

  if (TARGET_64BIT || TARGET_NO_SPACE_REGS)
    return rp;

  saved_rp = gen_reg_rtx (Pmode);
  emit_move_insn (saved_rp, rp);

  /* Get pointer to the instruction stream.  We have to mask out the
     privilege level from the two low order bits of the return address
     pointer here so that ins will point to the start of the first
     instruction that would have been executed if we returned.  */
  ins = copy_to_reg (gen_rtx_AND (Pmode, rp, MASK_RETURN_ADDR));
  label = gen_label_rtx ();

  /* Check the instruction stream at the normal return address for the
     export stub:

	0x4bc23fd1 | stub+8:   ldw -18(sr0,sp),rp
	0x004010a1 | stub+12:  ldsid (sr0,rp),r1
	0x00011820 | stub+16:  mtsp r1,sr0
	0xe0400002 | stub+20:  be,n 0(sr0,rp)

     If it is an export stub, than our return address is really in
     -24[frameaddr].  */

  emit_cmp_insn (gen_rtx_MEM (SImode, ins), GEN_INT (0x4bc23fd1), NE,
		 NULL_RTX, SImode, 1);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx_MEM (SImode, plus_constant (ins, 4)),
		 GEN_INT (0x004010a1), NE, NULL_RTX, SImode, 1);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx_MEM (SImode, plus_constant (ins, 8)),
		 GEN_INT (0x00011820), NE, NULL_RTX, SImode, 1);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx_MEM (SImode, plus_constant (ins, 12)),
		 GEN_INT (0xe0400002), NE, NULL_RTX, SImode, 1);

  /* If there is no export stub then just use the value saved from
     the return pointer register.  */

  emit_jump_insn (gen_bne (label));

  /* Here we know that our return address points to an export
     stub.  We don't want to return the address of the export stub,
     but rather the return address of the export stub.  That return
     address is stored at -24[frameaddr].  */

  emit_move_insn (saved_rp,
		  gen_rtx_MEM (Pmode,
			       memory_address (Pmode,
					       plus_constant (frameaddr,
							      -24))));

  emit_label (label);
  return saved_rp;
}

/* This is only valid once reload has completed because it depends on
   knowing exactly how much (if any) frame there is and...

   It's only valid if there is no frame marker to de-allocate and...

   It's only valid if %r2 hasn't been saved into the caller's frame
   (we're not profiling and %r2 isn't live anywhere).  */
int
hppa_can_use_return_insn_p ()
{
  return (reload_completed
	  && (compute_frame_size (get_frame_size (), 0) ? 0 : 1)
	  && ! regs_ever_live[2]
	  && ! frame_pointer_needed);
}

void
emit_bcond_fp (code, operand0)
     enum rtx_code code;
     rtx operand0;
{
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode,
						     gen_rtx_fmt_ee (code,
							      VOIDmode,
							      gen_rtx_REG (CCFPmode, 0),
							      const0_rtx),
						     gen_rtx_LABEL_REF (VOIDmode, operand0),
						     pc_rtx)));

}

rtx
gen_cmp_fp (code, operand0, operand1)
     enum rtx_code code;
     rtx operand0, operand1;
{
  return gen_rtx_SET (VOIDmode, gen_rtx_REG (CCFPmode, 0),
		      gen_rtx_fmt_ee (code, CCFPmode, operand0, operand1));
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
pa_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  enum attr_type attr_type;

  /* Don't adjust costs for a pa8000 chip, also do not adjust any
     true dependencies as they are described with bypasses now.  */
  if (pa_cpu >= PROCESSOR_8000 || REG_NOTE_KIND (link) == 0)
    return cost;

  if (! recog_memoized (insn))
    return 0;

  attr_type = get_attr_type (insn);

  if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
    {
      /* Anti dependency; DEP_INSN reads a register that INSN writes some
	 cycles later.  */

      if (attr_type == TYPE_FPLOAD)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPALU:
		case TYPE_FPMULSGL:
		case TYPE_FPMULDBL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* A fpload can't be issued until one cycle before a
		     preceding arithmetic operation has finished if
		     the target of the fpload is any of the sources
		     (or destination) of the arithmetic operation.  */
		  return insn_default_latency (dep_insn) - 1;

		default:
		  return 0;
		}
	    }
	}
      else if (attr_type == TYPE_FPALU)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* An ALU flop can't be issued until two cycles before a
		     preceding divide or sqrt operation has finished if
		     the target of the ALU flop is any of the sources
		     (or destination) of the divide or sqrt operation.  */
		  return insn_default_latency (dep_insn) - 2;

		default:
		  return 0;
		}
	    }
	}

      /* For other anti dependencies, the cost is 0.  */
      return 0;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    {
      /* Output dependency; DEP_INSN writes a register that INSN writes some
	 cycles later.  */
      if (attr_type == TYPE_FPLOAD)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_DEST (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPALU:
		case TYPE_FPMULSGL:
		case TYPE_FPMULDBL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* A fpload can't be issued until one cycle before a
		     preceding arithmetic operation has finished if
		     the target of the fpload is the destination of the
		     arithmetic operation. 

		     Exception: For PA7100LC, PA7200 and PA7300, the cost
		     is 3 cycles, unless they bundle together.   We also
		     pay the penalty if the second insn is a fpload.  */
		  return insn_default_latency (dep_insn) - 1;

		default:
		  return 0;
		}
	    }
	}
      else if (attr_type == TYPE_FPALU)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_DEST (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* An ALU flop can't be issued until two cycles before a
		     preceding divide or sqrt operation has finished if
		     the target of the ALU flop is also the target of
		     the divide or sqrt operation.  */
		  return insn_default_latency (dep_insn) - 2;

		default:
		  return 0;
		}
	    }
	}

      /* For other output dependencies, the cost is 0.  */
      return 0;
    }
  else
    abort ();
}

/* Adjust scheduling priorities.  We use this to try and keep addil
   and the next use of %r1 close together.  */
static int
pa_adjust_priority (insn, priority)
     rtx insn;
     int priority;
{
  rtx set = single_set (insn);
  rtx src, dest;
  if (set)
    {
      src = SET_SRC (set);
      dest = SET_DEST (set);
      if (GET_CODE (src) == LO_SUM
	  && symbolic_operand (XEXP (src, 1), VOIDmode)
	  && ! read_only_operand (XEXP (src, 1), VOIDmode))
	priority >>= 3;

      else if (GET_CODE (src) == MEM
	       && GET_CODE (XEXP (src, 0)) == LO_SUM
	       && symbolic_operand (XEXP (XEXP (src, 0), 1), VOIDmode)
	       && ! read_only_operand (XEXP (XEXP (src, 0), 1), VOIDmode))
	priority >>= 1;

      else if (GET_CODE (dest) == MEM
	       && GET_CODE (XEXP (dest, 0)) == LO_SUM
	       && symbolic_operand (XEXP (XEXP (dest, 0), 1), VOIDmode)
	       && ! read_only_operand (XEXP (XEXP (dest, 0), 1), VOIDmode))
	priority >>= 3;
    }
  return priority;
}

/* The 700 can only issue a single insn at a time.
   The 7XXX processors can issue two insns at a time.
   The 8000 can issue 4 insns at a time.  */
static int
pa_issue_rate ()
{
  switch (pa_cpu)
    {
    case PROCESSOR_700:		return 1;
    case PROCESSOR_7100:	return 2;
    case PROCESSOR_7100LC:	return 2;
    case PROCESSOR_7200:	return 2;
    case PROCESSOR_7300:	return 2;
    case PROCESSOR_8000:	return 4;

    default:
      abort ();
    }
}



/* Return any length adjustment needed by INSN which already has its length
   computed as LENGTH.   Return zero if no adjustment is necessary.

   For the PA: function calls, millicode calls, and backwards short
   conditional branches with unfilled delay slots need an adjustment by +1
   (to account for the NOP which will be inserted into the instruction stream).

   Also compute the length of an inline block move here as it is too
   complicated to express as a length attribute in pa.md.  */
int
pa_adjust_insn_length (insn, length)
    rtx insn;
    int length;
{
  rtx pat = PATTERN (insn);

  /* Call insns which are *not* indirect and have unfilled delay slots.  */
  if (GET_CODE (insn) == CALL_INSN)
    {

      if (GET_CODE (XVECEXP (pat, 0, 0)) == CALL
	  && GET_CODE (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0)) == SYMBOL_REF)
	return 4;
      else if (GET_CODE (XVECEXP (pat, 0, 0)) == SET
	       && GET_CODE (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0))
		  == SYMBOL_REF)
	return 4;
      else
	return 0;
    }
  /* Jumps inside switch tables which have unfilled delay slots
     also need adjustment.  */
  else if (GET_CODE (insn) == JUMP_INSN
	   && simplejump_p (insn)
	   && GET_MODE (insn) == SImode)
    return 4;
  /* Millicode insn with an unfilled delay slot.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) != SEQUENCE
	   && GET_CODE (pat) != USE
	   && GET_CODE (pat) != CLOBBER
	   && get_attr_type (insn) == TYPE_MILLI)
    return 4;
  /* Block move pattern.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) == PARALLEL
	   && GET_CODE (XVECEXP (pat, 0, 0)) == SET
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 0)) == MEM
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 1)) == MEM
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 0)) == BLKmode
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 1)) == BLKmode)
    return compute_movstrsi_length (insn) - 4;
  /* Conditional branch with an unfilled delay slot.  */
  else if (GET_CODE (insn) == JUMP_INSN && ! simplejump_p (insn))
    {
      /* Adjust a short backwards conditional with an unfilled delay slot.  */
      if (GET_CODE (pat) == SET
	  && length == 4
	  && ! forward_branch_p (insn))
	return 4;
      else if (GET_CODE (pat) == PARALLEL
	       && get_attr_type (insn) == TYPE_PARALLEL_BRANCH
	       && length == 4)
	return 4;
      /* Adjust dbra insn with short backwards conditional branch with
	 unfilled delay slot -- only for case where counter is in a
	 general register register.  */
      else if (GET_CODE (pat) == PARALLEL
	       && GET_CODE (XVECEXP (pat, 0, 1)) == SET
	       && GET_CODE (XEXP (XVECEXP (pat, 0, 1), 0)) == REG
 	       && ! FP_REG_P (XEXP (XVECEXP (pat, 0, 1), 0))
	       && length == 4
	       && ! forward_branch_p (insn))
	return 4;
      else
	return 0;
    }
  return 0;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;
    case '*':
      /* Output an nullification completer if there's nothing for the */
      /* delay slot or nullification is requested.  */
      if (dbr_sequence_length () == 0 ||
	  (final_sequence &&
	   INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))))
        fputs (",n", file);
      return;
    case 'R':
      /* Print out the second register name of a register pair.
	 I.e., R (6) => 7.  */
      fputs (reg_names[REGNO (x) + 1], file);
      return;
    case 'r':
      /* A register or zero.  */
      if (x == const0_rtx
	  || (x == CONST0_RTX (DFmode))
	  || (x == CONST0_RTX (SFmode)))
	{
	  fputs ("%r0", file);
	  return;
	}
      else
	break;
    case 'f':
      /* A register or zero (floating point).  */
      if (x == const0_rtx
	  || (x == CONST0_RTX (DFmode))
	  || (x == CONST0_RTX (SFmode)))
	{
	  fputs ("%fr0", file);
	  return;
	}
      else
	break;
    case 'A':
      {
	rtx xoperands[2];

	xoperands[0] = XEXP (XEXP (x, 0), 0);
	xoperands[1] = XVECEXP (XEXP (XEXP (x, 0), 1), 0, 0);
	output_global_address (file, xoperands[1], 0);
        fprintf (file, "(%s)", reg_names [REGNO (xoperands[0])]);
	return;
      }

    case 'C':			/* Plain (C)ondition */
    case 'X':
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("=", file);  break;
	case NE:
	  fputs ("<>", file);  break;
	case GT:
	  fputs (">", file);  break;
	case GE:
	  fputs (">=", file);  break;
	case GEU:
	  fputs (">>=", file);  break;
	case GTU:
	  fputs (">>", file);  break;
	case LT:
	  fputs ("<", file);  break;
	case LE:
	  fputs ("<=", file);  break;
	case LEU:
	  fputs ("<<=", file);  break;
	case LTU:
	  fputs ("<<", file);  break;
	default:
	  abort ();
	}
      return;
    case 'N':			/* Condition, (N)egated */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("<>", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs ("<=", file);  break;
	case GE:
	  fputs ("<", file);  break;
	case GEU:
	  fputs ("<<", file);  break;
	case GTU:
	  fputs ("<<=", file);  break;
	case LT:
	  fputs (">=", file);  break;
	case LE:
	  fputs (">", file);  break;
	case LEU:
	  fputs (">>", file);  break;
	case LTU:
	  fputs (">>=", file);  break;
	default:
	  abort ();
	}
      return;
    /* For floating point comparisons.  Note that the output
       predicates are the complement of the desired mode.  */
    case 'Y':
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("!=", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs ("!>", file);  break;
	case GE:
	  fputs ("!>=", file);  break;
	case LT:
	  fputs ("!<", file);  break;
	case LE:
	  fputs ("!<=", file);  break;
	case LTGT:
	  fputs ("!<>", file);  break;
	case UNLE:
	  fputs (">", file);  break;
	case UNLT:
	  fputs (">=", file);  break;
	case UNGE:
	  fputs ("<", file);  break;
	case UNGT:
	  fputs ("<=", file);  break;
	case UNEQ:
	  fputs ("<>", file);  break;
	case UNORDERED:
	  fputs ("<=>", file);  break;
	case ORDERED:
	  fputs ("!<=>", file);  break;
	default:
	  abort ();
	}
      return;
    case 'S':			/* Condition, operands are (S)wapped.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("=", file);  break;
	case NE:
	  fputs ("<>", file);  break;
	case GT:
	  fputs ("<", file);  break;
	case GE:
	  fputs ("<=", file);  break;
	case GEU:
	  fputs ("<<=", file);  break;
	case GTU:
	  fputs ("<<", file);  break;
	case LT:
	  fputs (">", file);  break;
	case LE:
	  fputs (">=", file);  break;
	case LEU:
	  fputs (">>=", file);  break;
	case LTU:
	  fputs (">>", file);  break;
	default:
	  abort ();
	}
      return;
    case 'B':			/* Condition, (B)oth swapped and negate.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("<>", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs (">=", file);  break;
	case GE:
	  fputs (">", file);  break;
	case GEU:
	  fputs (">>", file);  break;
	case GTU:
	  fputs (">>=", file);  break;
	case LT:
	  fputs ("<=", file);  break;
	case LE:
	  fputs ("<", file);  break;
	case LEU:
	  fputs ("<<", file);  break;
	case LTU:
	  fputs ("<<=", file);  break;
	default:
	  abort ();
	}
      return;
    case 'k':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~INTVAL (x));
	  return;
	}
      abort ();
    case 'Q':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, 64 - (INTVAL (x) & 63));
	  return;
	}
      abort ();
    case 'L':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, 32 - (INTVAL (x) & 31));
	  return;
	}
      abort ();
    case 'O':
      if (GET_CODE (x) == CONST_INT && exact_log2 (INTVAL (x)) >= 0)
	{
	  fprintf (file, "%d", exact_log2 (INTVAL (x)));
	  return;
	}
      abort ();
    case 'p':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, 63 - (INTVAL (x) & 63));
	  return;
	}
      abort ();
    case 'P':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, 31 - (INTVAL (x) & 31));
	  return;
	}
      abort ();
    case 'I':
      if (GET_CODE (x) == CONST_INT)
	fputs ("i", file);
      return;
    case 'M':
    case 'F':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  if (ASSEMBLER_DIALECT == 0)
	    fputs ("s,mb", file);
	  else
	    fputs (",mb", file);
	  break;
	case POST_DEC:
	case POST_INC:
	  if (ASSEMBLER_DIALECT == 0)
	    fputs ("s,ma", file);
	  else
	    fputs (",ma", file);
	  break;
	case PLUS:
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	      || GET_CODE (XEXP (XEXP (x, 0), 1)) == MULT)
	    {
	      if (ASSEMBLER_DIALECT == 0)
		fputs ("x,s", file);
	      else
		fputs (",s", file);
	    }
	  else if (code == 'F' && ASSEMBLER_DIALECT == 0)
	    fputs ("s", file);
	  break;
	default:
	  if (code == 'F' && ASSEMBLER_DIALECT == 0)
	    fputs ("s", file);
	  break;
	}
      return;
    case 'G':
      output_global_address (file, x, 0);
      return;
    case 'H':
      output_global_address (file, x, 1);
      return;
    case 0:			/* Don't do anything special */
      break;
    case 'Z':
      {
	unsigned op[3];
	compute_zdepwi_operands (INTVAL (x), op);
	fprintf (file, "%d,%d,%d", op[0], op[1], op[2]);
	return;
      }
    case 'z':
      {
	unsigned op[3];
	compute_zdepdi_operands (INTVAL (x), op);
	fprintf (file, "%d,%d,%d", op[0], op[1], op[2]);
	return;
      }
    case 'c':
      /* We can get here from a .vtable_inherit due to our
	 CONSTANT_ADDRESS_P rejecting perfectly good constant
	 addresses.  */
      break;
    default:
      abort ();
    }
  if (GET_CODE (x) == REG)
    {
      fputs (reg_names [REGNO (x)], file);
      if (TARGET_64BIT && FP_REG_P (x) && GET_MODE_SIZE (GET_MODE (x)) <= 4)
	{
	  fputs ("R", file);
	  return;
	}
      if (FP_REG_P (x)
	  && GET_MODE_SIZE (GET_MODE (x)) <= 4
	  && (REGNO (x) & 1) == 0)
	fputs ("L", file);
    }
  else if (GET_CODE (x) == MEM)
    {
      int size = GET_MODE_SIZE (GET_MODE (x));
      rtx base = NULL_RTX;
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case POST_DEC:
          base = XEXP (XEXP (x, 0), 0);
	  fprintf (file, "-%d(%s)", size, reg_names [REGNO (base)]);
	  break;
	case PRE_INC:
	case POST_INC:
          base = XEXP (XEXP (x, 0), 0);
	  fprintf (file, "%d(%s)", size, reg_names [REGNO (base)]);
	  break;
	default:
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT)
	    fprintf (file, "%s(%s)",
		     reg_names [REGNO (XEXP (XEXP (XEXP (x, 0), 0), 0))],
		     reg_names [REGNO (XEXP (XEXP (x, 0), 1))]);
	  else if (GET_CODE (XEXP (x, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (x, 0), 1)) == MULT)
	    fprintf (file, "%s(%s)",
		     reg_names [REGNO (XEXP (XEXP (XEXP (x, 0), 1), 0))],
		     reg_names [REGNO (XEXP (XEXP (x, 0), 0))]);
	  else
	    output_address (XEXP (x, 0));
	  break;
	}
    }
  else
    output_addr_const (file, x);
}

/* output a SYMBOL_REF or a CONST expression involving a SYMBOL_REF.  */

void
output_global_address (file, x, round_constant)
     FILE *file;
     rtx x;
     int round_constant;
{

  /* Imagine  (high (const (plus ...))).  */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  if (GET_CODE (x) == SYMBOL_REF && read_only_operand (x, VOIDmode))
    assemble_name (file, XSTR (x, 0));
  else if (GET_CODE (x) == SYMBOL_REF && !flag_pic)
    {
      assemble_name (file, XSTR (x, 0));
      fputs ("-$global$", file);
    }
  else if (GET_CODE (x) == CONST)
    {
      const char *sep = "";
      int offset = 0;		/* assembler wants -$global$ at end */
      rtx base = NULL_RTX;

      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 0);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 0)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0), 0));
      else abort ();

      if (GET_CODE (XEXP (XEXP (x, 0), 1)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 1);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0), 1));
      else abort ();

      /* How bogus.  The compiler is apparently responsible for
	 rounding the constant if it uses an LR field selector.

	 The linker and/or assembler seem a better place since
	 they have to do this kind of thing already.

	 If we fail to do this, HP's optimizing linker may eliminate
	 an addil, but not update the ldw/stw/ldo instruction that
	 uses the result of the addil.  */
      if (round_constant)
	offset = ((offset + 0x1000) & ~0x1fff);

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  if (offset < 0)
	    {
	      offset = -offset;
	      sep = "-";
	    }
	  else
	    sep = "+";
	}
      else if (GET_CODE (XEXP (x, 0)) == MINUS
	       && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
	sep = "-";
      else abort ();

      if (!read_only_operand (base, VOIDmode) && !flag_pic)
	fputs ("-$global$", file);
      if (offset)
	fprintf (file, "%s%d", sep, offset);
    }
  else
    output_addr_const (file, x);
}

static struct deferred_plabel *
get_plabel (fname)
     const char *fname;
{
  size_t i;

  /* See if we have already put this function on the list of deferred
     plabels.  This list is generally small, so a liner search is not
     too ugly.  If it proves too slow replace it with something faster.  */
  for (i = 0; i < n_deferred_plabels; i++)
    if (strcmp (fname, deferred_plabels[i].name) == 0)
      break;

  /* If the deferred plabel list is empty, or this entry was not found
     on the list, create a new entry on the list.  */
  if (deferred_plabels == NULL || i == n_deferred_plabels)
    {
      const char *real_name;

      if (deferred_plabels == 0)
	deferred_plabels = (struct deferred_plabel *)
	  ggc_alloc (sizeof (struct deferred_plabel));
      else
	deferred_plabels = (struct deferred_plabel *)
	  ggc_realloc (deferred_plabels,
		       ((n_deferred_plabels + 1)
			* sizeof (struct deferred_plabel)));

      i = n_deferred_plabels++;
      deferred_plabels[i].internal_label = gen_label_rtx ();
      deferred_plabels[i].name = ggc_strdup (fname);

      /* Gross.  We have just implicitly taken the address of this function,
	 mark it as such.  */
      real_name = (*targetm.strip_name_encoding) (fname);
      TREE_SYMBOL_REFERENCED (get_identifier (real_name)) = 1;
    }

  return &deferred_plabels[i];
}

void
output_deferred_plabels (file)
     FILE *file;
{
  size_t i;
  /* If we have deferred plabels, then we need to switch into the data
     section and align it to a 4 byte boundary before we output the
     deferred plabels.  */
  if (n_deferred_plabels)
    {
      data_section ();
      ASM_OUTPUT_ALIGN (file, TARGET_64BIT ? 3 : 2);
    }

  /* Now output the deferred plabels.  */
  for (i = 0; i < n_deferred_plabels; i++)
    {
      rtx label = deferred_plabels[i].internal_label;

      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (label));
      assemble_integer (gen_rtx_SYMBOL_REF (Pmode, deferred_plabels[i].name),
			TARGET_64BIT ? 8 : 4, TARGET_64BIT ? 64 : 32, 1);
    }
}

/* HP's millicode routines mean something special to the assembler.
   Keep track of which ones we have used.  */

enum millicodes { remI, remU, divI, divU, mulI, end1000 };
static void import_milli			PARAMS ((enum millicodes));
static char imported[(int) end1000];
static const char * const milli_names[] = {"remI", "remU", "divI", "divU", "mulI"};
static const char import_string[] = ".IMPORT $$....,MILLICODE";
#define MILLI_START 10

static void
import_milli (code)
     enum millicodes code;
{
  char str[sizeof (import_string)];

  if (!imported[(int) code])
    {
      imported[(int) code] = 1;
      strcpy (str, import_string);
      strncpy (str + MILLI_START, milli_names[(int) code], 4);
      output_asm_insn (str, 0);
    }
}

/* The register constraints have put the operands and return value in
   the proper registers.  */

const char *
output_mul_insn (unsignedp, insn)
     int unsignedp ATTRIBUTE_UNUSED;
     rtx insn;
{
  import_milli (mulI);
  return output_millicode_call (insn, gen_rtx_SYMBOL_REF (Pmode, "$$mulI"));
}

/* Emit the rtl for doing a division by a constant.  */

/* Do magic division millicodes exist for this value? */
static const int magic_milli[]= {0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0,
				 1, 1};

/* We'll use an array to keep track of the magic millicodes and
   whether or not we've used them already. [n][0] is signed, [n][1] is
   unsigned.  */

static int div_milli[16][2];

int
div_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (mode == SImode
	  && ((GET_CODE (op) == REG && REGNO (op) == 25)
	      || (GET_CODE (op) == CONST_INT && INTVAL (op) > 0
		  && INTVAL (op) < 16 && magic_milli[INTVAL (op)])));
}

int
emit_hpdiv_const (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) < 16
      && magic_milli[INTVAL (operands[2])])
    {
      rtx ret = gen_rtx_REG (SImode, TARGET_64BIT ? 2 : 31);

      emit_move_insn (gen_rtx_REG (SImode, 26), operands[1]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (6, gen_rtx_SET (VOIDmode, gen_rtx_REG (SImode, 29),
				     gen_rtx_fmt_ee (unsignedp ? UDIV : DIV,
						     SImode,
						     gen_rtx_REG (SImode, 26),
						     operands[2])),
		     gen_rtx_CLOBBER (VOIDmode, operands[4]),
		     gen_rtx_CLOBBER (VOIDmode, operands[3]),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, 26)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, 25)),
		     gen_rtx_CLOBBER (VOIDmode, ret))));
      emit_move_insn (operands[0], gen_rtx_REG (SImode, 29));
      return 1;
    }
  return 0;
}

const char *
output_div_insn (operands, unsignedp, insn)
     rtx *operands;
     int unsignedp;
     rtx insn;
{
  int divisor;

  /* If the divisor is a constant, try to use one of the special
     opcodes .*/
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      static char buf[100];
      divisor = INTVAL (operands[0]);
      if (!div_milli[divisor][unsignedp])
	{
	  div_milli[divisor][unsignedp] = 1;
	  if (unsignedp)
	    output_asm_insn (".IMPORT $$divU_%0,MILLICODE", operands);
	  else
	    output_asm_insn (".IMPORT $$divI_%0,MILLICODE", operands);
	}
      if (unsignedp)
	{
	  sprintf (buf, "$$divU_");
	  sprintf (buf + 7, HOST_WIDE_INT_PRINT_DEC, INTVAL (operands[0]));
	  return output_millicode_call (insn,
					gen_rtx_SYMBOL_REF (SImode, buf));
	}
      else
	{
	  sprintf (buf, "$$divI_");
	  sprintf (buf + 7, HOST_WIDE_INT_PRINT_DEC, INTVAL (operands[0]));
	  return output_millicode_call (insn,
					gen_rtx_SYMBOL_REF (SImode, buf));
	}
    }
  /* Divisor isn't a special constant.  */
  else
    {
      if (unsignedp)
	{
	  import_milli (divU);
	  return output_millicode_call (insn,
					gen_rtx_SYMBOL_REF (SImode, "$$divU"));
	}
      else
	{
	  import_milli (divI);
	  return output_millicode_call (insn,
					gen_rtx_SYMBOL_REF (SImode, "$$divI"));
	}
    }
}

/* Output a $$rem millicode to do mod.  */

const char *
output_mod_insn (unsignedp, insn)
     int unsignedp;
     rtx insn;
{
  if (unsignedp)
    {
      import_milli (remU);
      return output_millicode_call (insn,
				    gen_rtx_SYMBOL_REF (SImode, "$$remU"));
    }
  else
    {
      import_milli (remI);
      return output_millicode_call (insn,
				    gen_rtx_SYMBOL_REF (SImode, "$$remI"));
    }
}

void
output_arg_descriptor (call_insn)
     rtx call_insn;
{
  const char *arg_regs[4];
  enum machine_mode arg_mode;
  rtx link;
  int i, output_flag = 0;
  int regno;

  /* We neither need nor want argument location descriptors for the
     64bit runtime environment or the ELF32 environment.  */
  if (TARGET_64BIT || TARGET_ELF32)
    return;

  for (i = 0; i < 4; i++)
    arg_regs[i] = 0;

  /* Specify explicitly that no argument relocations should take place
     if using the portable runtime calling conventions.  */
  if (TARGET_PORTABLE_RUNTIME)
    {
      fputs ("\t.CALL ARGW0=NO,ARGW1=NO,ARGW2=NO,ARGW3=NO,RETVAL=NO\n",
	     asm_out_file);
      return;
    }

  if (GET_CODE (call_insn) != CALL_INSN)
    abort ();
  for (link = CALL_INSN_FUNCTION_USAGE (call_insn); link; link = XEXP (link, 1))
    {
      rtx use = XEXP (link, 0);

      if (! (GET_CODE (use) == USE
	     && GET_CODE (XEXP (use, 0)) == REG
	     && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	continue;

      arg_mode = GET_MODE (XEXP (use, 0));
      regno = REGNO (XEXP (use, 0));
      if (regno >= 23 && regno <= 26)
	{
	  arg_regs[26 - regno] = "GR";
	  if (arg_mode == DImode)
	    arg_regs[25 - regno] = "GR";
	}
      else if (regno >= 32 && regno <= 39)
	{
	  if (arg_mode == SFmode)
	    arg_regs[(regno - 32) / 2] = "FR";
	  else
	    {
#ifndef HP_FP_ARG_DESCRIPTOR_REVERSED
	      arg_regs[(regno - 34) / 2] = "FR";
	      arg_regs[(regno - 34) / 2 + 1] = "FU";
#else
	      arg_regs[(regno - 34) / 2] = "FU";
	      arg_regs[(regno - 34) / 2 + 1] = "FR";
#endif
	    }
	}
    }
  fputs ("\t.CALL ", asm_out_file);
  for (i = 0; i < 4; i++)
    {
      if (arg_regs[i])
	{
	  if (output_flag++)
	    fputc (',', asm_out_file);
	  fprintf (asm_out_file, "ARGW%d=%s", i, arg_regs[i]);
	}
    }
  fputc ('\n', asm_out_file);
}

/* Return the class of any secondary reload register that is needed to
   move IN into a register in class CLASS using mode MODE.

   Profiling has showed this routine and its descendants account for
   a significant amount of compile time (~7%).  So it has been
   optimized to reduce redundant computations and eliminate useless
   function calls.

   It might be worthwhile to try and make this a leaf function too.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno, is_symbolic;

  /* Trying to load a constant into a FP register during PIC code
     generation will require %r1 as a scratch register.  */
  if (flag_pic
      && GET_MODE_CLASS (mode) == MODE_INT
      && FP_REG_CLASS_P (class)
      && (GET_CODE (in) == CONST_INT || GET_CODE (in) == CONST_DOUBLE))
    return R1_REGS;

  /* Profiling showed the PA port spends about 1.3% of its compilation
     time in true_regnum from calls inside secondary_reload_class.  */

  if (GET_CODE (in) == REG)
    {
      regno = REGNO (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = true_regnum (in);
    }
  else if (GET_CODE (in) == SUBREG)
    regno = true_regnum (in);
  else
    regno = -1;

  /* If we have something like (mem (mem (...)), we can safely assume the
     inner MEM will end up in a general register after reloading, so there's
     no need for a secondary reload.  */
  if (GET_CODE (in) == MEM
      && GET_CODE (XEXP (in, 0)) == MEM)
    return NO_REGS;

  /* Handle out of range displacement for integer mode loads/stores of
     FP registers.  */
  if (((regno >= FIRST_PSEUDO_REGISTER || regno == -1)
       && GET_MODE_CLASS (mode) == MODE_INT
       && FP_REG_CLASS_P (class))
      || (class == SHIFT_REGS && (regno <= 0 || regno >= 32)))
    return GENERAL_REGS;

  /* A SAR<->FP register copy requires a secondary register (GPR) as
     well as secondary memory.  */
  if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER
      && ((REGNO_REG_CLASS (regno) == SHIFT_REGS && FP_REG_CLASS_P (class))
	  || (class == SHIFT_REGS && FP_REG_CLASS_P (REGNO_REG_CLASS (regno)))))
    return GENERAL_REGS;

  if (GET_CODE (in) == HIGH)
    in = XEXP (in, 0);

  /* Profiling has showed GCC spends about 2.6% of its compilation
     time in symbolic_operand from calls inside secondary_reload_class.

     We use an inline copy and only compute its return value once to avoid
     useless work.  */
  switch (GET_CODE (in))
    {
      rtx tmp;

      case SYMBOL_REF:
      case LABEL_REF:
        is_symbolic = 1;
        break;
      case CONST:
	tmp = XEXP (in, 0);
	is_symbolic = ((GET_CODE (XEXP (tmp, 0)) == SYMBOL_REF
			|| GET_CODE (XEXP (tmp, 0)) == LABEL_REF)
		       && GET_CODE (XEXP (tmp, 1)) == CONST_INT);
        break;

      default:
        is_symbolic = 0;
        break;
    }

  if (!flag_pic
      && is_symbolic
      && read_only_operand (in, VOIDmode))
    return NO_REGS;

  if (class != R1_REGS && is_symbolic)
    return R1_REGS;

  return NO_REGS;
}

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  if (mode == BLKmode
      || (TARGET_64BIT && type && AGGREGATE_TYPE_P (type)))
    {
      /* Return none if justification is not required.  */
      if (type
	  && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	  && (int_size_in_bytes (type) * BITS_PER_UNIT) % PARM_BOUNDARY == 0)
	return none;

      /* The directions set here are ignored when a BLKmode argument larger
	 than a word is placed in a register.  Different code is used for
	 the stack and registers.  This makes it difficult to have a
	 consistent data representation for both the stack and registers.
	 For both runtimes, the justification and padding for arguments on
	 the stack and in registers should be identical.  */
      if (TARGET_64BIT)
	/* The 64-bit runtime specifies left justification for aggregates.  */
        return upward;
      else
	/* The 32-bit runtime architecture specifies right justification.
	   When the argument is passed on the stack, the argument is padded
	   with garbage on the left.  The HP compiler pads with zeros.  */
	return downward;
    }

  if (GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)
    return downward;
  else
    return none;
}


/* Do what is necessary for `va_start'.  We look at the current function
   to determine if stdargs or varargs is used and fill in an initial
   va_list.  A pointer to this constructor is returned.  */

struct rtx_def *
hppa_builtin_saveregs ()
{
  rtx offset, dest;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!(TYPE_ARG_TYPES (fntype) != 0
		   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		       != void_type_node)))
		? UNITS_PER_WORD : 0);

  if (argadj)
    offset = plus_constant (current_function_arg_offset_rtx, argadj);
  else
    offset = current_function_arg_offset_rtx;

  if (TARGET_64BIT)
    {
      int i, off;

      /* Adjust for varargs/stdarg differences.  */
      if (argadj)
	offset = plus_constant (current_function_arg_offset_rtx, -argadj);
      else
	offset = current_function_arg_offset_rtx;

      /* We need to save %r26 .. %r19 inclusive starting at offset -64
	 from the incoming arg pointer and growing to larger addresses.  */
      for (i = 26, off = -64; i >= 19; i--, off += 8)
	emit_move_insn (gen_rtx_MEM (word_mode,
				     plus_constant (arg_pointer_rtx, off)),
			gen_rtx_REG (word_mode, i));

      /* The incoming args pointer points just beyond the flushback area;
	 normally this is not a serious concern.  However, when we are doing
	 varargs/stdargs we want to make the arg pointer point to the start
	 of the incoming argument area.  */
      emit_move_insn (virtual_incoming_args_rtx,
		      plus_constant (arg_pointer_rtx, -64));

      /* Now return a pointer to the first anonymous argument.  */
      return copy_to_reg (expand_binop (Pmode, add_optab,
					virtual_incoming_args_rtx,
					offset, 0, 0, OPTAB_LIB_WIDEN));
    }

  /* Store general registers on the stack.  */
  dest = gen_rtx_MEM (BLKmode,
		      plus_constant (current_function_internal_arg_pointer,
				     -16));
  set_mem_alias_set (dest, get_varargs_alias_set ());
  set_mem_align (dest, BITS_PER_WORD);
  move_block_from_reg (23, dest, 4, 4 * UNITS_PER_WORD);

  /* move_block_from_reg will emit code to store the argument registers
     individually as scalar stores.

     However, other insns may later load from the same addresses for
     a structure load (passing a struct to a varargs routine).

     The alias code assumes that such aliasing can never happen, so we
     have to keep memory referencing insns from moving up beyond the
     last argument register store.  So we emit a blockage insn here.  */
  emit_insn (gen_blockage ());

  return copy_to_reg (expand_binop (Pmode, add_optab,
				    current_function_internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

void
hppa_va_start (valist, nextarg)
     tree valist;
     rtx nextarg;
{
  nextarg = expand_builtin_saveregs ();
  std_expand_builtin_va_start (valist, nextarg);
}

rtx
hppa_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT ofs;
  tree t, ptr, pptr;

  if (TARGET_64BIT)
    {
      /* Every argument in PA64 is supposed to be passed by value
	 (including large structs).  However, as a GCC extension, we
	 pass zero and variable sized arguments by reference.  Empty
	 structures are a GCC extension not supported by the HP
	 compilers.  Thus, passing them by reference isn't likely
	 to conflict with the ABI.  For variable sized arguments,
	 GCC doesn't have the infrastructure to allocate these to
	 registers.  */

      /* Arguments with a size greater than 8 must be aligned 0 MOD 16.  */

      if (size > UNITS_PER_WORD)
        {
          t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
                     build_int_2 (2 * UNITS_PER_WORD - 1, 0));
          t = build (BIT_AND_EXPR, TREE_TYPE (t), t,
                     build_int_2 (-2 * UNITS_PER_WORD, -1));
          t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
          TREE_SIDE_EFFECTS (t) = 1;
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
        }

      if (size > 0)
	return std_expand_builtin_va_arg (valist, type);
      else
	{
	  ptr = build_pointer_type (type);

	  /* Args grow upward.  */
	  t = build (POSTINCREMENT_EXPR, TREE_TYPE (valist), valist,
		     build_int_2 (POINTER_SIZE / BITS_PER_UNIT, 0));
	  TREE_SIDE_EFFECTS (t) = 1;

	  pptr = build_pointer_type (ptr);
	  t = build1 (NOP_EXPR, pptr, t);
	  TREE_SIDE_EFFECTS (t) = 1;

	  t = build1 (INDIRECT_REF, ptr, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	}
    }
  else /* !TARGET_64BIT */
    {
      ptr = build_pointer_type (type);

      /* "Large" and variable sized types are passed by reference.  */
      if (size > 8 || size <= 0)
	{
	  /* Args grow downward.  */
	  t = build (PREDECREMENT_EXPR, TREE_TYPE (valist), valist,
		     build_int_2 (POINTER_SIZE / BITS_PER_UNIT, 0));
	  TREE_SIDE_EFFECTS (t) = 1;

	  pptr = build_pointer_type (ptr);
	  t = build1 (NOP_EXPR, pptr, t);
	  TREE_SIDE_EFFECTS (t) = 1;

	  t = build1 (INDIRECT_REF, ptr, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	}
      else
	{
	  t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
		     build_int_2 (-size, -1));

	  /* Copied from va-pa.h, but we probably don't need to align to
	     word size, since we generate and preserve that invariant.  */
	  t = build (BIT_AND_EXPR, TREE_TYPE (valist), t,
		     build_int_2 ((size > 4 ? -8 : -4), -1));

	  t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
	  TREE_SIDE_EFFECTS (t) = 1;

	  ofs = (8 - size) % 4;
	  if (ofs)
	    {
	      t = build (PLUS_EXPR, TREE_TYPE (valist), t,
			 build_int_2 (ofs, 0));
	      TREE_SIDE_EFFECTS (t) = 1;
	    }

	  t = build1 (NOP_EXPR, ptr, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	}
    }

  /* Calculate!  */
  return expand_expr (t, NULL_RTX, VOIDmode, EXPAND_NORMAL);
}



/* This routine handles all the normal conditional branch sequences we
   might need to generate.  It handles compare immediate vs compare
   register, nullification of delay slots, varying length branches,
   negated branches, and all combinations of the above.  It returns the
   output appropriate to emit the branch corresponding to all given
   parameters.  */

const char *
output_cbranch (operands, nullify, length, negated, insn)
     rtx *operands;
     int nullify, length, negated;
     rtx insn;
{
  static char buf[100];
  int useskip = 0;
  rtx xoperands[5];

  /* A conditional branch to the following instruction (eg the delay slot)
     is asking for a disaster.  This can happen when not optimizing and
     when jump optimization fails.

     While it is usually safe to emit nothing, this can fail if the
     preceding instruction is a nullified branch with an empty delay
     slot and the same branch target as this branch.  We could check
     for this but jump optimization should eliminate nop jumps.  It
     is always safe to emit a nop.  */
  if (next_real_insn (JUMP_LABEL (insn)) == next_real_insn (insn))
    return "nop";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     comclr instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */
  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {
      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "{com%I2clr,|cmp%I2clr,}");
	else
	  strcpy (buf, "{com%I2b,|cmp%I2b,}");
	if (GET_MODE (operands[1]) == DImode)
	  strcat (buf, "*");
	if (negated)
	  strcat (buf, "%B3");
	else
	  strcat (buf, "%S3");
	if (useskip)
	  strcat (buf, " %2,%r1,%%r0");
	else if (nullify)
	  strcat (buf, ",n %2,%r1,%0");
	else
	  strcat (buf, " %2,%r1,%0");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "{com%I2b,|cmp%I2b,}");
	    if (GET_MODE (operands[1]) == DImode)
	      strcat (buf, "*");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    strcat (buf, ",n %2,%r1,.+12\n\tb %0");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a comb;nop rather than comiclr;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && INSN_ADDRESSES_SET_P ()
		 && VAL_14_BITS_P (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (insn)))
				    - INSN_ADDRESSES (INSN_UID (insn)) - 8))
	  {
	    strcpy (buf, "{com%I2b,|cmp%I2b,}");
	    if (GET_MODE (operands[1]) == DImode)
	      strcat (buf, "*");
	    if (negated)
	      strcat (buf, "%B3 %2,%r1,%0%#");
	    else
	      strcat (buf, "%S3 %2,%r1,%0%#");
	  }
	else
	  {
	    strcpy (buf, "{com%I2clr,|cmp%I2clr,}");
	    if (GET_MODE (operands[1]) == DImode)
	      strcat (buf, "*");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    if (nullify)
	      strcat (buf, " %2,%r1,%%r0\n\tb,n %0");
	    else
	      strcat (buf, " %2,%r1,%%r0\n\tb %0");
	  }
	break;

      case 20:
      case 28:
	xoperands[0] = operands[0];
	xoperands[1] = operands[1];
	xoperands[2] = operands[2];
	xoperands[3] = operands[3];

	/* The reversed conditional branch must branch over one additional
	   instruction if the delay slot is filled.  If the delay slot
	   is empty, the instruction after the reversed condition branch
	   must be nullified.  */
	nullify = dbr_sequence_length () == 0;
	xoperands[4] = nullify ? GEN_INT (length) : GEN_INT (length + 4);

	/* Create a reversed conditional branch which branches around
	   the following insns.  */
	if (GET_MODE (operands[1]) != DImode)
	  {
	    if (nullify)
	      {
		if (negated)
		  strcpy (buf,
		    "{com%I2b,%S3,n %2,%r1,.+%4|cmp%I2b,%S3,n %2,%r1,.+%4}");
		else
		  strcpy (buf,
		    "{com%I2b,%B3,n %2,%r1,.+%4|cmp%I2b,%B3,n %2,%r1,.+%4}");
	      }
	    else
	      {
		if (negated)
		  strcpy (buf,
		    "{com%I2b,%S3 %2,%r1,.+%4|cmp%I2b,%S3 %2,%r1,.+%4}");
		else
		  strcpy (buf,
		    "{com%I2b,%B3 %2,%r1,.+%4|cmp%I2b,%B3 %2,%r1,.+%4}");
	      }
	  }
	else
	  {
	    if (nullify)
	      {
		if (negated)
		  strcpy (buf,
		    "{com%I2b,*%S3,n %2,%r1,.+%4|cmp%I2b,*%S3,n %2,%r1,.+%4}");
		else
		  strcpy (buf,
		    "{com%I2b,*%B3,n %2,%r1,.+%4|cmp%I2b,*%B3,n %2,%r1,.+%4}");
	      }
	    else
	      {
		if (negated)
		  strcpy (buf,
		    "{com%I2b,*%S3 %2,%r1,.+%4|cmp%I2b,*%S3 %2,%r1,.+%4}");
		else
		  strcpy (buf,
		    "{com%I2b,*%B3 %2,%r1,.+%4|cmp%I2b,*%B3 %2,%r1,.+%4}");
	      }
	  }

	output_asm_insn (buf, xoperands);
	return output_lbranch (operands[0], insn);

      default:
	abort ();
    }
  return buf;
}

/* This routine handles long unconditional branches that exceed the
   maximum range of a simple branch instruction.  */

const char *
output_lbranch (dest, insn)
     rtx dest, insn;
{
  rtx xoperands[2];
 
  xoperands[0] = dest;

  /* First, free up the delay slot.  */
  if (dbr_sequence_length () != 0)
    {
      /* We can't handle a jump in the delay slot.  */
      if (GET_CODE (NEXT_INSN (insn)) == JUMP_INSN)
	abort ();

      final_scan_insn (NEXT_INSN (insn), asm_out_file,
		       optimize, 0, 0);

      /* Now delete the delay insn.  */
      PUT_CODE (NEXT_INSN (insn), NOTE);
      NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
      NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
    }

  /* Output an insn to save %r1.  The runtime documentation doesn't
     specify whether the "Clean Up" slot in the callers frame can
     be clobbered by the callee.  It isn't copied by HP's builtin
     alloca, so this suggests that it can be clobbered if necessary.
     The "Static Link" location is copied by HP builtin alloca, so
     we avoid using it.  Using the cleanup slot might be a problem
     if we have to interoperate with languages that pass cleanup
     information.  However, it should be possible to handle these
     situations with GCC's asm feature.

     The "Current RP" slot is reserved for the called procedure, so
     we try to use it when we don't have a frame of our own.  It's
     rather unlikely that we won't have a frame when we need to emit
     a very long branch.

     Really the way to go long term is a register scavenger; goto
     the target of the jump and find a register which we can use
     as a scratch to hold the value in %r1.  Then, we wouldn't have
     to free up the delay slot or clobber a slot that may be needed
     for other purposes.  */
  if (TARGET_64BIT)
    {
      if (actual_fsize == 0 && !regs_ever_live[2])
	/* Use the return pointer slot in the frame marker.  */
	output_asm_insn ("std %%r1,-16(%%r30)", xoperands);
      else
	/* Use the slot at -40 in the frame marker since HP builtin
	   alloca doesn't copy it.  */
	output_asm_insn ("std %%r1,-40(%%r30)", xoperands);
    }
  else
    {
      if (actual_fsize == 0 && !regs_ever_live[2])
	/* Use the return pointer slot in the frame marker.  */
	output_asm_insn ("stw %%r1,-20(%%r30)", xoperands);
      else
	/* Use the "Clean Up" slot in the frame marker.  In GCC,
	   the only other use of this location is for copying a
	   floating point double argument from a floating-point
	   register to two general registers.  The copy is done
	   as an "atomic" operation when outputing a call, so it
	   won't interfere with our using the location here.  */
	output_asm_insn ("stw %%r1,-12(%%r30)", xoperands);
    }

  if (flag_pic)
    {
      output_asm_insn ("{bl|b,l} .+8,%%r1", xoperands);
      if (TARGET_SOM || !TARGET_GAS)
	{
	  xoperands[1] = gen_label_rtx ();
	  output_asm_insn ("addil L'%l0-%l1,%%r1", xoperands);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (xoperands[1]));
	  output_asm_insn ("ldo R'%l0-%l1(%%r1),%%r1", xoperands);
	}
      else
	{
	  output_asm_insn ("addil L'%l0-$PIC_pcrel$0+4,%%r1", xoperands);
	  output_asm_insn ("ldo R'%l0-$PIC_pcrel$0+8(%%r1),%%r1", xoperands);
	}
      output_asm_insn ("bv %%r0(%%r1)", xoperands);
    }
  else
    /* Now output a very long branch to the original target.  */
    output_asm_insn ("ldil L'%l0,%%r1\n\tbe R'%l0(%%sr4,%%r1)", xoperands);

  /* Now restore the value of %r1 in the delay slot.  */
  if (TARGET_64BIT)
    {
      if (actual_fsize == 0 && !regs_ever_live[2])
	return "ldd -16(%%r30),%%r1";
      else
	return "ldd -40(%%r30),%%r1";
    }
  else
    {
      if (actual_fsize == 0 && !regs_ever_live[2])
	return "ldw -20(%%r30),%%r1";
      else
	return "ldw -12(%%r30),%%r1";
    }
}

/* This routine handles all the branch-on-bit conditional branch sequences we
   might need to generate.  It handles nullification of delay slots,
   varying length branches, negated branches and all combinations of the
   above.  it returns the appropriate output template to emit the branch.  */

const char *
output_bb (operands, nullify, length, negated, insn, which)
     rtx *operands ATTRIBUTE_UNUSED;
     int nullify, length, negated;
     rtx insn;
     int which;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  I do not think this can happen as this pattern
     is only used when optimizing; jump optimization should eliminate the
     jump.  But be prepared just in case.  */

  if (next_real_insn (JUMP_LABEL (insn)) == next_real_insn (insn))
    return "nop";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     extrs instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */

  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {

      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "{extrs,|extrw,s,}");
	else
	  strcpy (buf, "bb,");
	if (useskip && GET_MODE (operands[0]) == DImode)
	  strcpy (buf, "extrd,s,*");
	else if (GET_MODE (operands[0]) == DImode)
	  strcpy (buf, "bb,*");
	if ((which == 0 && negated)
	     || (which == 1 && ! negated))
	  strcat (buf, ">=");
	else
	  strcat (buf, "<");
	if (useskip)
	  strcat (buf, " %0,%1,1,%%r0");
	else if (nullify && negated)
	  strcat (buf, ",n %0,%1,%3");
	else if (nullify && ! negated)
	  strcat (buf, ",n %0,%1,%2");
	else if (! nullify && negated)
	  strcat (buf, "%0,%1,%3");
	else if (! nullify && ! negated)
	  strcat (buf, " %0,%1,%2");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "bb,");
	    if (GET_MODE (operands[0]) == DImode)
	      strcat (buf, "*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (negated)
	      strcat (buf, ",n %0,%1,.+12\n\tb %3");
	    else
	      strcat (buf, ",n %0,%1,.+12\n\tb %2");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a bb;nop rather than extrs;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && INSN_ADDRESSES_SET_P ()
		 && VAL_14_BITS_P (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (insn)))
				    - INSN_ADDRESSES (INSN_UID (insn)) - 8))
	  {
	    strcpy (buf, "bb,");
	    if (GET_MODE (operands[0]) == DImode)
	      strcat (buf, "*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, ">=");
	    else
	      strcat (buf, "<");
	    if (negated)
	      strcat (buf, " %0,%1,%3%#");
	    else
	      strcat (buf, " %0,%1,%2%#");
	  }
	else
	  {
	    strcpy (buf, "{extrs,|extrw,s,}");
	    if (GET_MODE (operands[0]) == DImode)
	      strcpy (buf, "extrd,s,*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (nullify && negated)
	      strcat (buf, " %0,%1,1,%%r0\n\tb,n %3");
	    else if (nullify && ! negated)
	      strcat (buf, " %0,%1,1,%%r0\n\tb,n %2");
	    else if (negated)
	      strcat (buf, " %0,%1,1,%%r0\n\tb %3");
	    else
	      strcat (buf, " %0,%1,1,%%r0\n\tb %2");
	  }
	break;

      default:
	abort ();
    }
  return buf;
}

/* This routine handles all the branch-on-variable-bit conditional branch
   sequences we might need to generate.  It handles nullification of delay
   slots, varying length branches, negated branches and all combinations
   of the above.  it returns the appropriate output template to emit the
   branch.  */

const char *
output_bvb (operands, nullify, length, negated, insn, which)
     rtx *operands ATTRIBUTE_UNUSED;
     int nullify, length, negated;
     rtx insn;
     int which;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  I do not think this can happen as this pattern
     is only used when optimizing; jump optimization should eliminate the
     jump.  But be prepared just in case.  */

  if (next_real_insn (JUMP_LABEL (insn)) == next_real_insn (insn))
    return "nop";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     extrs instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */

  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {

      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "{vextrs,|extrw,s,}");
	else
	  strcpy (buf, "{bvb,|bb,}");
	if (useskip && GET_MODE (operands[0]) == DImode)
	  strcpy (buf, "extrd,s,*");
	else if (GET_MODE (operands[0]) == DImode)
	  strcpy (buf, "bb,*");
	if ((which == 0 && negated)
	     || (which == 1 && ! negated))
	  strcat (buf, ">=");
	else
	  strcat (buf, "<");
	if (useskip)
	  strcat (buf, "{ %0,1,%%r0| %0,%%sar,1,%%r0}");
	else if (nullify && negated)
	  strcat (buf, "{,n %0,%3|,n %0,%%sar,%3}");
	else if (nullify && ! negated)
	  strcat (buf, "{,n %0,%2|,n %0,%%sar,%2}");
	else if (! nullify && negated)
	  strcat (buf, "{%0,%3|%0,%%sar,%3}");
	else if (! nullify && ! negated)
	  strcat (buf, "{ %0,%2| %0,%%sar,%2}");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "{bvb,|bb,}");
	    if (GET_MODE (operands[0]) == DImode)
	      strcat (buf, "*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (negated)
	      strcat (buf, "{,n %0,.+12\n\tb %3|,n %0,%%sar,.+12\n\tb %3}");
	    else
	      strcat (buf, "{,n %0,.+12\n\tb %2|,n %0,%%sar,.+12\n\tb %2}");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a bb;nop rather than extrs;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && INSN_ADDRESSES_SET_P ()
		 && VAL_14_BITS_P (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (insn)))
				    - INSN_ADDRESSES (INSN_UID (insn)) - 8))
	  {
	    strcpy (buf, "{bvb,|bb,}");
	    if (GET_MODE (operands[0]) == DImode)
	      strcat (buf, "*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, ">=");
	    else
	      strcat (buf, "<");
	    if (negated)
	      strcat (buf, "{ %0,%3%#| %0,%%sar,%3%#}");
	    else
	      strcat (buf, "{ %0,%2%#| %0,%%sar,%2%#}");
	  }
	else
	  {
	    strcpy (buf, "{vextrs,|extrw,s,}");
	    if (GET_MODE (operands[0]) == DImode)
	      strcpy (buf, "extrd,s,*");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (nullify && negated)
	      strcat (buf, "{ %0,1,%%r0\n\tb,n %3| %0,%%sar,1,%%r0\n\tb,n %3}");
	    else if (nullify && ! negated)
	      strcat (buf, "{ %0,1,%%r0\n\tb,n %2| %0,%%sar,1,%%r0\n\tb,n %2}");
	    else if (negated)
	      strcat (buf, "{ %0,1,%%r0\n\tb %3| %0,%%sar,1,%%r0\n\tb %3}");
	    else
	      strcat (buf, "{ %0,1,%%r0\n\tb %2| %0,%%sar,1,%%r0\n\tb %2}");
	  }
	break;

      default:
	abort ();
    }
  return buf;
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
const char *
output_dbra (operands, insn, which_alternative)
     rtx *operands;
     rtx insn;
     int which_alternative;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (next_real_insn (JUMP_LABEL (insn)) == next_real_insn (insn))
    {
      if (which_alternative == 0)
	return "ldo %1(%0),%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("{fstws|fstw} %0,-16(%%r30)", operands);
	  output_asm_insn ("ldw -16(%%r30),%4", operands);
	  output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(%%r30)", operands);
	  return "{fldws|fldw} -16(%%r30),%0";
	}
      else
	{
	  output_asm_insn ("ldw %0,%4", operands);
	  return "ldo %1(%4),%4\n\tstw %4,%0";
	}
    }

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "addib,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "addib,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a fulled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "addib,%N2,n %1,%0,.+12\n\tb %3";
	  /* Handle short backwards branch with an unfilled delay slot.
	     Using a addb;nop rather than addi;bl saves 1 cycle for both
	     taken and untaken branches.  */
	  else if (dbr_sequence_length () == 0
		   && ! forward_branch_p (insn)
		   && INSN_ADDRESSES_SET_P ()
		   && VAL_14_BITS_P (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (insn)))
				      - INSN_ADDRESSES (INSN_UID (insn)) - 8))
	      return "addib,%C2 %1,%0,%3%#";

	  /* Handle normal cases.  */
	  if (nullify)
	    return "addi,%N2 %1,%0,%0\n\tb,n %3";
	  else
	    return "addi,%N2 %1,%0,%0\n\tb %3";
	}
      else
	abort ();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("{fstws|fstw} %0,-16(%%r30)\n\tldw -16(%%r30),%4",
		       operands);
      output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(%%r30)", operands);
      if (get_attr_length (insn) == 24)
	return "{comb|cmpb},%S2 %%r0,%4,%3\n\t{fldws|fldw} -16(%%r30),%0";
      else
	return "{comclr|cmpclr},%B2 %%r0,%4,%%r0\n\tb %3\n\t{fldws|fldw} -16(%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      output_asm_insn ("ldw %0,%4", operands);
      if (get_attr_length (insn) == 12)
	return "addib,%C2 %1,%4,%3\n\tstw %4,%0";
      else
	return "addi,%N2 %1,%4,%4\n\tb %3\n\tstw %4,%0";
    }
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
const char *
output_movb (operands, insn, which_alternative, reverse_comparison)
     rtx *operands;
     rtx insn;
     int which_alternative;
     int reverse_comparison;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (next_real_insn (JUMP_LABEL (insn)) == next_real_insn (insn))
    {
      if (which_alternative == 0)
	return "copy %1,%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("stw %1,-16(%%r30)", operands);
	  return "{fldws|fldw} -16(%%r30),%0";
	}
      else if (which_alternative == 2)
	return "stw %1,%0";
      else
	return "mtsar %r1";
    }

  /* Support the second variant.  */
  if (reverse_comparison)
    PUT_CODE (operands[2], reverse_condition (GET_CODE (operands[2])));

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "movb,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "movb,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a filled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "movb,%N2,n %1,%0,.+12\n\tb %3";

	  /* Handle short backwards branch with an unfilled delay slot.
	     Using a movb;nop rather than or;bl saves 1 cycle for both
	     taken and untaken branches.  */
	  else if (dbr_sequence_length () == 0
		   && ! forward_branch_p (insn)
		   && INSN_ADDRESSES_SET_P ()
		   && VAL_14_BITS_P (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (insn)))
				      - INSN_ADDRESSES (INSN_UID (insn)) - 8))
	    return "movb,%C2 %1,%0,%3%#";
	  /* Handle normal cases.  */
	  if (nullify)
	    return "or,%N2 %1,%%r0,%0\n\tb,n %3";
	  else
	    return "or,%N2 %1,%%r0,%0\n\tb %3";
	}
      else
	abort ();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("stw %1,-16(%%r30)", operands);
      if (get_attr_length (insn) == 12)
	return "{comb|cmpb},%S2 %%r0,%1,%3\n\t{fldws|fldw} -16(%%r30),%0";
      else
	return "{comclr|cmpclr},%B2 %%r0,%1,%%r0\n\tb %3\n\t{fldws|fldw} -16(%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else if (which_alternative == 2)
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      if (get_attr_length (insn) == 8)
	return "{comb|cmpb},%S2 %%r0,%1,%3\n\tstw %1,%0";
      else
	return "{comclr|cmpclr},%B2 %%r0,%1,%%r0\n\tb %3\n\tstw %1,%0";
    }
  /* Handle SAR as a destination.  */
  else
    {
      if (get_attr_length (insn) == 8)
	return "{comb|cmpb},%S2 %%r0,%1,%3\n\tmtsar %r1";
      else
	return "{comclr|cmpclr},%B2 %%r0,%1,%%r0\n\tbl %3\n\tmtsar %r1";
    }
}

/* Copy any FP arguments in INSN into integer registers.  */
static void
copy_fp_args (insn)
     rtx insn;
{
  rtx link;
  rtx xoperands[2];

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      int arg_mode, regno;
      rtx use = XEXP (link, 0);

      if (! (GET_CODE (use) == USE
	  && GET_CODE (XEXP (use, 0)) == REG
	  && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	continue;

      arg_mode = GET_MODE (XEXP (use, 0));
      regno = REGNO (XEXP (use, 0));

      /* Is it a floating point register?  */
      if (regno >= 32 && regno <= 39)
	{
	  /* Copy the FP register into an integer register via memory.  */
	  if (arg_mode == SFmode)
	    {
	      xoperands[0] = XEXP (use, 0);
	      xoperands[1] = gen_rtx_REG (SImode, 26 - (regno - 32) / 2);
	      output_asm_insn ("{fstws|fstw} %0,-16(%%sr0,%%r30)", xoperands);
	      output_asm_insn ("ldw -16(%%sr0,%%r30),%1", xoperands);
	    }
	  else
	    {
	      xoperands[0] = XEXP (use, 0);
	      xoperands[1] = gen_rtx_REG (DImode, 25 - (regno - 34) / 2);
	      output_asm_insn ("{fstds|fstd} %0,-16(%%sr0,%%r30)", xoperands);
	      output_asm_insn ("ldw -12(%%sr0,%%r30),%R1", xoperands);
	      output_asm_insn ("ldw -16(%%sr0,%%r30),%1", xoperands);
	    }
	}
    }
}

/* Compute length of the FP argument copy sequence for INSN.  */
static int
length_fp_args (insn)
     rtx insn;
{
  int length = 0;
  rtx link;

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      int arg_mode, regno;
      rtx use = XEXP (link, 0);

      if (! (GET_CODE (use) == USE
	  && GET_CODE (XEXP (use, 0)) == REG
	  && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	continue;

      arg_mode = GET_MODE (XEXP (use, 0));
      regno = REGNO (XEXP (use, 0));

      /* Is it a floating point register?  */
      if (regno >= 32 && regno <= 39)
	{
	  if (arg_mode == SFmode)
	    length += 8;
	  else
	    length += 12;
	}
    }

  return length;
}

/* Return the attribute length for the millicode call instruction INSN.
   The length must match the code generated by output_millicode_call.
   We include the delay slot in the returned length as it is better to
   over estimate the length than to under estimate it.  */

int
attr_length_millicode_call (insn)
     rtx insn;
{
  unsigned long distance = -1;

  if (INSN_ADDRESSES_SET_P ())
    {
      distance = (total_code_bytes + insn_current_reference_address (insn));
      if (distance < total_code_bytes)
	distance = -1;
    }

  if (TARGET_64BIT)
    {
      if (!TARGET_LONG_CALLS && distance < 7600000)
	return 8;

      return 20;
    }
  else if (TARGET_PORTABLE_RUNTIME)
    return 24;
  else
    {
      if (!TARGET_LONG_CALLS && distance < 240000)
	return 8;

      if (TARGET_LONG_ABS_CALL && !flag_pic)
	return 12;

      return 24;
    }
}

/* INSN is a function call.  It may have an unconditional jump
   in its delay slot.

   CALL_DEST is the routine we are calling.  */

const char *
output_millicode_call (insn, call_dest)
     rtx insn;
     rtx call_dest;
{
  int attr_length = get_attr_length (insn);
  int seq_length = dbr_sequence_length ();
  int distance;
  rtx seq_insn;
  rtx xoperands[3];

  xoperands[0] = call_dest;
  xoperands[2] = gen_rtx_REG (Pmode, TARGET_64BIT ? 2 : 31);

  /* Handle the common case where we are sure that the branch will
     reach the beginning of the $CODE$ subspace.  The within reach
     form of the $$sh_func_adrs call has a length of 28.  Because
     it has an attribute type of multi, it never has a non-zero
     sequence length.  The length of the $$sh_func_adrs is the same
     as certain out of reach PIC calls to other routines.  */
  if (!TARGET_LONG_CALLS
      && ((seq_length == 0
	   && (attr_length == 12
	       || (attr_length == 28 && get_attr_type (insn) == TYPE_MULTI)))
	  || (seq_length != 0 && attr_length == 8)))
    {
      output_asm_insn ("{bl|b,l} %0,%2", xoperands);
    }
  else
    {
      if (TARGET_64BIT)
	{
	  /* It might seem that one insn could be saved by accessing
	     the millicode function using the linkage table.  However,
	     this doesn't work in shared libraries and other dynamically
	     loaded objects.  Using a pc-relative sequence also avoids
	     problems related to the implicit use of the gp register.  */
	  output_asm_insn ("b,l .+8,%%r1", xoperands);

	  if (TARGET_GAS)
	    {
	      output_asm_insn ("addil L'%0-$PIC_pcrel$0+4,%%r1", xoperands);
	      output_asm_insn ("ldo R'%0-$PIC_pcrel$0+8(%%r1),%%r1", xoperands);
	    }
	  else
	    {
	      xoperands[1] = gen_label_rtx ();
	      output_asm_insn ("addil L'%0-%l1,%%r1", xoperands);
	      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
					 CODE_LABEL_NUMBER (xoperands[1]));
	      output_asm_insn ("ldo R'%0-%l1(%%r1),%%r1", xoperands);
	    }

	  output_asm_insn ("bve,l (%%r1),%%r2", xoperands);
	}
      else if (TARGET_PORTABLE_RUNTIME)
	{
	  /* Pure portable runtime doesn't allow be/ble; we also don't
	     have PIC support in the assembler/linker, so this sequence
	     is needed.  */

	  /* Get the address of our target into %r1.  */
	  output_asm_insn ("ldil L'%0,%%r1", xoperands);
	  output_asm_insn ("ldo R'%0(%%r1),%%r1", xoperands);

	  /* Get our return address into %r31.  */
	  output_asm_insn ("{bl|b,l} .+8,%%r31", xoperands);
	  output_asm_insn ("addi 8,%%r31,%%r31", xoperands);

	  /* Jump to our target address in %r1.  */
	  output_asm_insn ("bv %%r0(%%r1)", xoperands);
	}
      else if (!flag_pic)
	{
	  output_asm_insn ("ldil L'%0,%%r1", xoperands);
	  if (TARGET_PA_20)
	    output_asm_insn ("be,l R'%0(%%sr4,%%r1),%%sr0,%%r31", xoperands);
	  else
	    output_asm_insn ("ble R'%0(%%sr4,%%r1)", xoperands);
	}
      else
	{
	  output_asm_insn ("{bl|b,l} .+8,%%r1", xoperands);
	  output_asm_insn ("addi 16,%%r1,%%r31", xoperands);

	  if (TARGET_SOM || !TARGET_GAS)
	    {
	      /* The HP assembler can generate relocations for the
		 difference of two symbols.  GAS can do this for a
		 millicode symbol but not an arbitrary external
		 symbol when generating SOM output.  */
	      xoperands[1] = gen_label_rtx ();
	      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
					 CODE_LABEL_NUMBER (xoperands[1]));
	      output_asm_insn ("addil L'%0-%l1,%%r1", xoperands);
	      output_asm_insn ("ldo R'%0-%l1(%%r1),%%r1", xoperands);
	    }
	  else
	    {
	      output_asm_insn ("addil L'%0-$PIC_pcrel$0+8,%%r1", xoperands);
	      output_asm_insn ("ldo R'%0-$PIC_pcrel$0+12(%%r1),%%r1",
			       xoperands);
	    }

	  /* Jump to our target address in %r1.  */
	  output_asm_insn ("bv %%r0(%%r1)", xoperands);
	}
    }

  if (seq_length == 0)
    output_asm_insn ("nop", xoperands);

  /* We are done if there isn't a jump in the delay slot.  */
  if (seq_length == 0 || GET_CODE (NEXT_INSN (insn)) != JUMP_INSN)
    return "";

  /* This call has an unconditional jump in its delay slot.  */
  xoperands[0] = XEXP (PATTERN (NEXT_INSN (insn)), 1);

  /* See if the return address can be adjusted.  Use the containing
     sequence insn's address.  */
  if (INSN_ADDRESSES_SET_P ())
    {
      seq_insn = NEXT_INSN (PREV_INSN (XVECEXP (final_sequence, 0, 0)));
      distance = (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (NEXT_INSN (insn))))
		  - INSN_ADDRESSES (INSN_UID (seq_insn)) - 8);

      if (VAL_14_BITS_P (distance))
	{
	  xoperands[1] = gen_label_rtx ();
	  output_asm_insn ("ldo %0-%1(%2),%2", xoperands);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (xoperands[1]));
	}
      else
	/* ??? This branch may not reach its target.  */
	output_asm_insn ("nop\n\tb,n %0", xoperands);
    }
  else
    /* ??? This branch may not reach its target.  */
    output_asm_insn ("nop\n\tb,n %0", xoperands);

  /* Delete the jump.  */
  PUT_CODE (NEXT_INSN (insn), NOTE);
  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;

  return "";
}

/* Return the attribute length of the call instruction INSN.  The SIBCALL
   flag indicates whether INSN is a regular call or a sibling call.  The
   length must match the code generated by output_call.  We include the delay
   slot in the returned length as it is better to over estimate the length
   than to under estimate it.  */

int
attr_length_call (insn, sibcall)
     rtx insn;
     int sibcall;
{
  unsigned long distance = -1;

  if (INSN_ADDRESSES_SET_P ())
    {
      distance = (total_code_bytes + insn_current_reference_address (insn));
      if (distance < total_code_bytes)
	distance = -1;
    }

  if (TARGET_64BIT)
    {
      if (!TARGET_LONG_CALLS
	  && ((!sibcall && distance < 7600000) || distance < 240000))
	return 8;

      return (sibcall ? 28 : 24);
    }
  else
    {
      if (!TARGET_LONG_CALLS
	  && ((TARGET_PA_20 && !sibcall && distance < 7600000)
	      || distance < 240000))
	return 8;

      if (TARGET_LONG_ABS_CALL && !flag_pic)
	return 12;

      if ((TARGET_SOM && TARGET_LONG_PIC_SDIFF_CALL)
	  || (TARGET_GAS && TARGET_LONG_PIC_PCREL_CALL))
	{
	  if (TARGET_PA_20)
	    return 20;

	  return 28;
	}
      else
	{
	  int length = 0;

	  if (TARGET_SOM)
	    length += length_fp_args (insn);

	  if (flag_pic)
	    length += 4;

	  if (TARGET_PA_20)
	    return (length + 32);

	  if (!TARGET_NO_SPACE_REGS)
	    length += 8;

	  if (!sibcall)
	    length += 8;

	  return (length + 32);
	}
    }
}

/* INSN is a function call.  It may have an unconditional jump
   in its delay slot.

   CALL_DEST is the routine we are calling.  */

const char *
output_call (insn, call_dest, sibcall)
     rtx insn;
     rtx call_dest;
     int sibcall;
{
  int delay_insn_deleted = 0;
  int delay_slot_filled = 0;
  int seq_length = dbr_sequence_length ();
  rtx xoperands[2];

  xoperands[0] = call_dest;

  /* Handle the common case where we're sure that the branch will reach
     the beginning of the $CODE$ subspace.  */
  if (!TARGET_LONG_CALLS && attr_length_call (insn, sibcall) == 8)
    {
      xoperands[1] = gen_rtx_REG (word_mode, sibcall ? 0 : 2);
      output_asm_insn ("{bl|b,l} %0,%1", xoperands);
    }
  else
    {
      if (TARGET_64BIT)
	{
	  /* ??? As far as I can tell, the HP linker doesn't support the
	     long pc-relative sequence described in the 64-bit runtime
	     architecture.  So, we use a slightly longer indirect call.  */
	  struct deferred_plabel *p = get_plabel (XSTR (call_dest, 0));

	  xoperands[0] = p->internal_label;
	  xoperands[1] = gen_label_rtx ();

	  /* If this isn't a sibcall, we put the load of %r27 into the
	     delay slot.  We can't do this in a sibcall as we don't
	     have a second call-clobbered scratch register available.  */
	  if (seq_length != 0
	      && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN
	      && !sibcall)
	    {
	      final_scan_insn (NEXT_INSN (insn), asm_out_file,
			       optimize, 0, 0);

	      /* Now delete the delay insn.  */
	      PUT_CODE (NEXT_INSN (insn), NOTE);
	      NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	      delay_insn_deleted = 1;
	    }

	  output_asm_insn ("addil LT'%0,%%r27", xoperands);
	  output_asm_insn ("ldd RT'%0(%%r1),%%r1", xoperands);
	  output_asm_insn ("ldd 0(%%r1),%%r1", xoperands);

	  if (sibcall)
	    {
	      output_asm_insn ("ldd 24(%%r1),%%r27", xoperands);
	      output_asm_insn ("ldd 16(%%r1),%%r1", xoperands);
	      output_asm_insn ("bve (%%r1)", xoperands);
	    }
	  else
	    {
	      output_asm_insn ("ldd 16(%%r1),%%r2", xoperands);
	      output_asm_insn ("bve,l (%%r2),%%r2", xoperands);
	      output_asm_insn ("ldd 24(%%r1),%%r27", xoperands);
	      delay_slot_filled = 1;
	    }
	}
      else
	{
	  int indirect_call = 0;

	  /* Emit a long call.  There are several different sequences
	     of increasing length and complexity.  In most cases,
             they don't allow an instruction in the delay slot.  */
	  if (!(TARGET_LONG_ABS_CALL && !flag_pic)
	      && !(TARGET_SOM && TARGET_LONG_PIC_SDIFF_CALL)
	      && !(TARGET_GAS && TARGET_LONG_PIC_PCREL_CALL))
	    indirect_call = 1;

	  if (seq_length != 0
	      && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN
	      && !sibcall
	      && (!TARGET_PA_20 || indirect_call))
	    {
	      /* A non-jump insn in the delay slot.  By definition we can
		 emit this insn before the call (and in fact before argument
		 relocating.  */
	      final_scan_insn (NEXT_INSN (insn), asm_out_file, optimize, 0, 0);

	      /* Now delete the delay insn.  */
	      PUT_CODE (NEXT_INSN (insn), NOTE);
	      NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	      delay_insn_deleted = 1;
	    }

	  if (TARGET_LONG_ABS_CALL && !flag_pic)
	    {
	      /* This is the best sequence for making long calls in
		 non-pic code.  Unfortunately, GNU ld doesn't provide
		 the stub needed for external calls, and GAS's support
		 for this with the SOM linker is buggy.  */
	      output_asm_insn ("ldil L'%0,%%r1", xoperands);
	      if (sibcall)
		output_asm_insn ("be R'%0(%%sr4,%%r1)", xoperands);
	      else
		{
		  if (TARGET_PA_20)
		    output_asm_insn ("be,l R'%0(%%sr4,%%r1),%%sr0,%%r31",
				     xoperands);
		  else
		    output_asm_insn ("ble R'%0(%%sr4,%%r1)", xoperands);

		  output_asm_insn ("copy %%r31,%%r2", xoperands);
		  delay_slot_filled = 1;
		}
	    }
	  else
	    {
	      if (TARGET_SOM && TARGET_LONG_PIC_SDIFF_CALL)
		{
		  /* The HP assembler and linker can handle relocations
		     for the difference of two symbols.  GAS and the HP
		     linker can't do this when one of the symbols is
		     external.  */
		  xoperands[1] = gen_label_rtx ();
		  output_asm_insn ("{bl|b,l} .+8,%%r1", xoperands);
		  output_asm_insn ("addil L'%0-%l1,%%r1", xoperands);
		  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
					     CODE_LABEL_NUMBER (xoperands[1]));
		  output_asm_insn ("ldo R'%0-%l1(%%r1),%%r1", xoperands);
		}
	      else if (TARGET_GAS && TARGET_LONG_PIC_PCREL_CALL)
		{
		  /*  GAS currently can't generate the relocations that
		      are needed for the SOM linker under HP-UX using this
		      sequence.  The GNU linker doesn't generate the stubs
		      that are needed for external calls on TARGET_ELF32
		      with this sequence.  For now, we have to use a
		      longer plabel sequence when using GAS.  */
		  output_asm_insn ("{bl|b,l} .+8,%%r1", xoperands);
		  output_asm_insn ("addil L'%0-$PIC_pcrel$0+4,%%r1",
				   xoperands);
		  output_asm_insn ("ldo R'%0-$PIC_pcrel$0+8(%%r1),%%r1",
				   xoperands);
		}
	      else
		{
		  /* Emit a long plabel-based call sequence.  This is
		     essentially an inline implementation of $$dyncall.
		     We don't actually try to call $$dyncall as this is
		     as difficult as calling the function itself.  */
		  struct deferred_plabel *p = get_plabel (XSTR (call_dest, 0));

		  xoperands[0] = p->internal_label;
		  xoperands[1] = gen_label_rtx ();

		  /* Since the call is indirect, FP arguments in registers
		     need to be copied to the general registers.  Then, the
		     argument relocation stub will copy them back.  */
		  if (TARGET_SOM)
		    copy_fp_args (insn);

		  if (flag_pic)
		    {
		      output_asm_insn ("addil LT'%0,%%r19", xoperands);
		      output_asm_insn ("ldw RT'%0(%%r1),%%r1", xoperands);
		      output_asm_insn ("ldw 0(%%r1),%%r1", xoperands);
		    }
		  else
		    {
		      output_asm_insn ("addil LR'%0-$global$,%%r27",
				       xoperands);
		      output_asm_insn ("ldw RR'%0-$global$(%%r1),%%r1",
				       xoperands);
		    }

		  output_asm_insn ("bb,>=,n %%r1,30,.+16", xoperands);
		  output_asm_insn ("depi 0,31,2,%%r1", xoperands);
		  output_asm_insn ("ldw 4(%%sr0,%%r1),%%r19", xoperands);
		  output_asm_insn ("ldw 0(%%sr0,%%r1),%%r1", xoperands);

		  if (!sibcall && !TARGET_PA_20)
		    {
		      output_asm_insn ("{bl|b,l} .+8,%%r2", xoperands);
		      if (TARGET_NO_SPACE_REGS)
			output_asm_insn ("addi 8,%%r2,%%r2", xoperands);
		      else
			output_asm_insn ("addi 16,%%r2,%%r2", xoperands);
		    }
		}

	      if (TARGET_PA_20)
		{
		  if (sibcall)
		    output_asm_insn ("bve (%%r1)", xoperands);
		  else
		    {
		      if (indirect_call)
			{
			  output_asm_insn ("bve,l (%%r1),%%r2", xoperands);
			  output_asm_insn ("stw %%r2,-24(%%sp)", xoperands);
			  delay_slot_filled = 1;
			}
		      else
			output_asm_insn ("bve,l (%%r1),%%r2", xoperands);
		    }
		}
	      else
		{
		  if (!TARGET_NO_SPACE_REGS)
		    output_asm_insn ("ldsid (%%r1),%%r31\n\tmtsp %%r31,%%sr0",
				     xoperands);

		  if (sibcall)
		    {
		      if (TARGET_NO_SPACE_REGS)
			output_asm_insn ("be 0(%%sr4,%%r1)", xoperands);
		      else
			output_asm_insn ("be 0(%%sr0,%%r1)", xoperands);
		    }
		  else
		    {
		      if (TARGET_NO_SPACE_REGS)
			output_asm_insn ("ble 0(%%sr4,%%r1)", xoperands);
		      else
			output_asm_insn ("ble 0(%%sr0,%%r1)", xoperands);

		      if (indirect_call)
			output_asm_insn ("stw %%r31,-24(%%sp)", xoperands);
		      else
			output_asm_insn ("copy %%r31,%%r2", xoperands);
		      delay_slot_filled = 1;
		    }
		}
	    }
	}
    }

  if (seq_length == 0 || (delay_insn_deleted && !delay_slot_filled))
    output_asm_insn ("nop", xoperands);

  /* We are done if there isn't a jump in the delay slot.  */
  if (seq_length == 0
      || delay_insn_deleted
      || GET_CODE (NEXT_INSN (insn)) != JUMP_INSN)
    return "";

  /* A sibcall should never have a branch in the delay slot.  */
  if (sibcall)
    abort ();

  /* This call has an unconditional jump in its delay slot.  */
  xoperands[0] = XEXP (PATTERN (NEXT_INSN (insn)), 1);

  if (!delay_slot_filled && INSN_ADDRESSES_SET_P ())
    {
      /* See if the return address can be adjusted.  Use the containing
         sequence insn's address.  */
      rtx seq_insn = NEXT_INSN (PREV_INSN (XVECEXP (final_sequence, 0, 0)));
      int distance = (INSN_ADDRESSES (INSN_UID (JUMP_LABEL (NEXT_INSN (insn))))
		      - INSN_ADDRESSES (INSN_UID (seq_insn)) - 8);

      if (VAL_14_BITS_P (distance))
	{
	  xoperands[1] = gen_label_rtx ();
	  output_asm_insn ("ldo %0-%1(%%r2),%%r2", xoperands);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (xoperands[1]));
	}
      else
	/* ??? This branch may not reach its target.  */
	output_asm_insn ("nop\n\tb,n %0", xoperands);
    }
  else
    /* ??? This branch may not reach its target.  */
    output_asm_insn ("b,n %0", xoperands);

  /* Delete the jump.  */
  PUT_CODE (NEXT_INSN (insn), NOTE);
  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;

  return "";
}

/* Return the attribute length of the indirect call instruction INSN.
   The length must match the code generated by output_indirect call.
   The returned length includes the delay slot.  Currently, the delay
   slot of an indirect call sequence is not exposed and it is used by
   the sequence itself.  */

int
attr_length_indirect_call (insn)
     rtx insn;
{
  unsigned long distance = -1;

  if (INSN_ADDRESSES_SET_P ())
    {
      distance = (total_code_bytes + insn_current_reference_address (insn));
      if (distance < total_code_bytes)
	distance = -1;
    }

  if (TARGET_64BIT)
    return 12;

  if (TARGET_FAST_INDIRECT_CALLS
      || (!TARGET_PORTABLE_RUNTIME
	  && ((TARGET_PA_20 && distance < 7600000) || distance < 240000)))
    return 8;

  if (flag_pic)
    return 24;

  if (TARGET_PORTABLE_RUNTIME)
    return 20;

  /* Out of reach, can use ble.  */
  return 12;
}

const char *
output_indirect_call (insn, call_dest)
     rtx insn;
     rtx call_dest;
{
  rtx xoperands[1];

  if (TARGET_64BIT)
    {
      xoperands[0] = call_dest;
      output_asm_insn ("ldd 16(%0),%%r2", xoperands);
      output_asm_insn ("bve,l (%%r2),%%r2\n\tldd 24(%0),%%r27", xoperands);
      return "";
    }

  /* First the special case for kernels, level 0 systems, etc.  */
  if (TARGET_FAST_INDIRECT_CALLS)
    return "ble 0(%%sr4,%%r22)\n\tcopy %%r31,%%r2"; 

  /* Now the normal case -- we can reach $$dyncall directly or
     we're sure that we can get there via a long-branch stub. 

     No need to check target flags as the length uniquely identifies
     the remaining cases.  */
  if (attr_length_indirect_call (insn) == 8)
    {
      /* The HP linker substitutes a BLE for millicode calls using
	 the short PIC PCREL form.  Thus, we must use %r31 as the
	 link register when generating PA 1.x code.  */
      if (TARGET_PA_20)
	return ".CALL\tARGW0=GR\n\tb,l $$dyncall,%%r2\n\tcopy %%r2,%%r31";
      else
	return ".CALL\tARGW0=GR\n\tbl $$dyncall,%%r31\n\tcopy %%r31,%%r2";
    }

  /* Long millicode call, but we are not generating PIC or portable runtime
     code.  */
  if (attr_length_indirect_call (insn) == 12)
    return ".CALL\tARGW0=GR\n\tldil L'$$dyncall,%%r2\n\tble R'$$dyncall(%%sr4,%%r2)\n\tcopy %%r31,%%r2";

  /* Long millicode call for portable runtime.  */
  if (attr_length_indirect_call (insn) == 20)
    return "ldil L'$$dyncall,%%r31\n\tldo R'$$dyncall(%%r31),%%r31\n\tblr %%r0,%%r2\n\tbv,n %%r0(%%r31)\n\tnop";

  /* We need a long PIC call to $$dyncall.  */
  xoperands[0] = NULL_RTX;
  output_asm_insn ("{bl|b,l} .+8,%%r1", xoperands);
  if (TARGET_SOM || !TARGET_GAS)
    {
      xoperands[0] = gen_label_rtx ();
      output_asm_insn ("addil L'$$dyncall-%0,%%r1", xoperands);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				 CODE_LABEL_NUMBER (xoperands[0]));
      output_asm_insn ("ldo R'$$dyncall-%0(%%r1),%%r1", xoperands);
    }
  else
    {
      output_asm_insn ("addil L'$$dyncall-$PIC_pcrel$0+4,%%r1", xoperands);
      output_asm_insn ("ldo R'$$dyncall-$PIC_pcrel$0+8(%%r1),%%r1",
		       xoperands);
    }
  output_asm_insn ("blr %%r0,%%r2", xoperands);
  output_asm_insn ("bv,n %%r0(%%r1)\n\tnop", xoperands);
  return "";
}

/* Return the total length of the save and restore instructions needed for
   the data linkage table pointer (i.e., the PIC register) across the call         instruction INSN.  No-return calls do not require a save and restore.           In addition, we may be able to avoid the save and restore for calls             within the same translation unit.  */

int
attr_length_save_restore_dltp (insn)
     rtx insn;
{
  if (find_reg_note (insn, REG_NORETURN, NULL_RTX))
    return 0;

  return 8;
}

/* In HPUX 8.0's shared library scheme, special relocations are needed
   for function labels if they might be passed to a function
   in a shared library (because shared libraries don't live in code
   space), and special magic is needed to construct their address.  */

void
hppa_encode_label (sym)
     rtx sym;
{
  const char *str = XSTR (sym, 0);
  int len = strlen (str) + 1;
  char *newstr, *p;

  p = newstr = alloca (len + 1);
  *p++ = '@';
  strcpy (p, str);

  XSTR (sym, 0) = ggc_alloc_string (newstr, len);
}

static void
pa_encode_section_info (decl, first)
     tree decl;
     int first;
{
  if (first && TEXT_SPACE_P (decl))
    {
      rtx rtl;
      if (TREE_CODE (decl) == FUNCTION_DECL
	  || TREE_CODE (decl) == VAR_DECL)
	rtl = DECL_RTL (decl);
      else
	rtl = TREE_CST_RTL (decl);
      SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	hppa_encode_label (XEXP (DECL_RTL (decl), 0));
    }
}

/* This is sort of inverse to pa_encode_section_info.  */

static const char *
pa_strip_name_encoding (str)
     const char *str;
{
  str += (*str == '@');
  str += (*str == '*');
  return str;
}

int
function_label_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == SYMBOL_REF && FUNCTION_NAME_P (XSTR (op, 0));
}

/* Returns 1 if OP is a function label involved in a simple addition
   with a constant.  Used to keep certain patterns from matching
   during instruction combination.  */
int
is_function_label_plus_const (op)
     rtx op;
{
  /* Strip off any CONST.  */
  if (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  return (GET_CODE (op) == PLUS
	  && function_label_operand (XEXP (op, 0), Pmode)
	  && GET_CODE (XEXP (op, 1)) == CONST_INT);
}

/* Output assembly code for a thunk to FUNCTION.  */

static void
pa_asm_output_mi_thunk (file, thunk_fndecl, delta, vcall_offset, function)
     FILE *file;
     tree thunk_fndecl;
     HOST_WIDE_INT delta;
     HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED;
     tree function;
{
  const char *target_name = XSTR (XEXP (DECL_RTL (function), 0), 0);
  static unsigned int current_thunk_number;
  char label[16];
  const char *lab;
  ASM_GENERATE_INTERNAL_LABEL (label, "LTHN", current_thunk_number);
  lab = (*targetm.strip_name_encoding) (label);
  target_name = (*targetm.strip_name_encoding) (target_name);
  /* FIXME: total_code_bytes is not handled correctly in files with
     mi thunks.  */
  pa_output_function_prologue (file, 0);
  if (VAL_14_BITS_P (delta))
    {
      if (!TARGET_64BIT && !TARGET_PORTABLE_RUNTIME && flag_pic)
	{
	  fprintf (file, "\taddil LT'%s,%%r19\n", lab);
	  fprintf (file, "\tldw RT'%s(%%r1),%%r22\n", lab);
	  fprintf (file, "\tldw 0(%%sr0,%%r22),%%r22\n");
	  fprintf (file, "\tbb,>=,n %%r22,30,.+16\n");
	  fprintf (file, "\tdepi 0,31,2,%%r22\n");
	  fprintf (file, "\tldw 4(%%sr0,%%r22),%%r19\n");
	  fprintf (file, "\tldw 0(%%sr0,%%r22),%%r22\n");
	  if (TARGET_NO_SPACE_REGS)
	    fprintf (file, "\tbe 0(%%sr4,%%r22)\n\tldo ");
	  else
	    {
	      fprintf (file, "\tldsid (%%sr0,%%r22),%%r1\n");
	      fprintf (file, "\tmtsp %%r1,%%sr0\n");
	      fprintf (file, "\tbe 0(%%sr0,%%r22)\n\tldo ");
	    }
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, "(%%r26),%%r26\n");
	}
      else
	{
	  fprintf (file, "\tb %s\n\tldo ", target_name);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, "(%%r26),%%r26\n");
	}
    }
  else
    {
      if (!TARGET_64BIT && !TARGET_PORTABLE_RUNTIME && flag_pic)
	{
	  fprintf (file, "\taddil L'");
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, ",%%r26\n\tldo R'");
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, "(%%r1),%%r26\n");
	  fprintf (file, "\taddil LT'%s,%%r19\n", lab);
	  fprintf (file, "\tldw RT'%s(%%r1),%%r22\n", lab);
	  fprintf (file, "\tldw 0(%%sr0,%%r22),%%r22\n");
	  fprintf (file, "\tbb,>=,n %%r22,30,.+16\n");
	  fprintf (file, "\tdepi 0,31,2,%%r22\n");
	  fprintf (file, "\tldw 4(%%sr0,%%r22),%%r19\n");
	  fprintf (file, "\tldw 0(%%sr0,%%r22),%%r22\n");
	  if (TARGET_NO_SPACE_REGS)
	    fprintf (file, "\tbe 0(%%sr4,%%r22)");
	  else
	    {
	      fprintf (file, "\tldsid (%%sr0,%%r22),%%r1\n");
	      fprintf (file, "\tmtsp %%r1,%%sr0\n");
	      fprintf (file, "\tbe,n 0(%%sr0,%%r22)\n");
	    }
	}
      else
	{
	  fprintf (file, "\taddil L'");
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, ",%%r26\n\tb %s\n\tldo R'", target_name);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, delta);
	  fprintf (file, "(%%r1),%%r26\n");
	}
    }
    
  fprintf (file, "\t.EXIT\n\t.PROCEND\n");
  if (! TARGET_64BIT && ! TARGET_PORTABLE_RUNTIME && flag_pic)
    {
      data_section ();
      fprintf (file, "\t.align 4\n");
      ASM_OUTPUT_INTERNAL_LABEL (file, "LTHN", current_thunk_number);
      fprintf (file, "\t.word P'%s\n", target_name);
      function_section (thunk_fndecl);
    }
  current_thunk_number++;
}

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpyadd instructions.  */
int
fmpyaddoperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* Must be a floating point mode.  */
  if (mode != SFmode && mode != DFmode)
    return 0;

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* All operands must be registers.  */
  if (! (GET_CODE (operands[1]) == REG
	 && GET_CODE (operands[2]) == REG
	 && GET_CODE (operands[3]) == REG
	 && GET_CODE (operands[4]) == REG
	 && GET_CODE (operands[5]) == REG))
    return 0;

  /* Only 2 real operands to the addition.  One of the input operands must
     be the same as the output operand.  */
  if (! rtx_equal_p (operands[3], operands[4])
      && ! rtx_equal_p (operands[3], operands[5]))
    return 0;

  /* Inout operand of add can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* multiply can not feed into addition operands.  */
  if (rtx_equal_p (operands[4], operands[0])
      || rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* SFmode limits the registers to the upper 32 of the 32bit FP regs.  */
  if (mode == SFmode
      && (REGNO_REG_CLASS (REGNO (operands[0])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[1])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[2])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[3])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[4])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[5])) != FPUPPER_REGS))
    return 0;

  /* Passed.  Operands are suitable for fmpyadd.  */
  return 1;
}

#if !defined(USE_COLLECT2)
static void
pa_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority;
{
  if (!function_label_operand (symbol, VOIDmode))
    hppa_encode_label (symbol);

#ifdef CTORS_SECTION_ASM_OP
  default_ctor_section_asm_out_constructor (symbol, priority);
#else
# ifdef TARGET_ASM_NAMED_SECTION
  default_named_section_asm_out_constructor (symbol, priority);
# else
  default_stabs_asm_out_constructor (symbol, priority);
# endif
#endif
}

static void
pa_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority;
{
  if (!function_label_operand (symbol, VOIDmode))
    hppa_encode_label (symbol);

#ifdef DTORS_SECTION_ASM_OP
  default_dtor_section_asm_out_destructor (symbol, priority);
#else
# ifdef TARGET_ASM_NAMED_SECTION
  default_named_section_asm_out_destructor (symbol, priority);
# else
  default_stabs_asm_out_destructor (symbol, priority);
# endif
#endif
}
#endif

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpysub instructions.  */
int
fmpysuboperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* Must be a floating point mode.  */
  if (mode != SFmode && mode != DFmode)
    return 0;

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* All operands must be registers.  */
  if (! (GET_CODE (operands[1]) == REG
	 && GET_CODE (operands[2]) == REG
	 && GET_CODE (operands[3]) == REG
	 && GET_CODE (operands[4]) == REG
	 && GET_CODE (operands[5]) == REG))
    return 0;

  /* Only 2 real operands to the subtraction.  Subtraction is not a commutative
     operation, so operands[4] must be the same as operand[3].  */
  if (! rtx_equal_p (operands[3], operands[4]))
    return 0;

  /* multiply can not feed into subtraction.  */
  if (rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* Inout operand of sub can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* SFmode limits the registers to the upper 32 of the 32bit FP regs.  */
  if (mode == SFmode
      && (REGNO_REG_CLASS (REGNO (operands[0])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[1])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[2])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[3])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[4])) != FPUPPER_REGS
	  || REGNO_REG_CLASS (REGNO (operands[5])) != FPUPPER_REGS))
    return 0;

  /* Passed.  Operands are suitable for fmpysub.  */
  return 1;
}

int
plus_xor_ior_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == PLUS || GET_CODE (op) == XOR
	  || GET_CODE (op) == IOR);
}

/* Return 1 if the given constant is 2, 4, or 8.  These are the valid
   constants for shadd instructions.  */
static int
shadd_constant_p (val)
     int val;
{
  if (val == 2 || val == 4 || val == 8)
    return 1;
  else
    return 0;
}

/* Return 1 if OP is a CONST_INT with the value 2, 4, or 8.  These are
   the valid constant for shadd instructions.  */
int
shadd_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && shadd_constant_p (INTVAL (op)));
}

/* Return 1 if OP is valid as a base register in a reg + reg address.  */

int
basereg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* cse will create some unscaled indexed addresses, however; it
     generally isn't a win on the PA, so avoid creating unscaled
     indexed addresses until after cse is finished.  */
  if (!cse_not_expected)
    return 0;

  /* Allow any register when TARGET_NO_SPACE_REGS is in effect since
     we don't have to worry about the braindamaged implicit space
     register selection from the basereg.  */
  if (TARGET_NO_SPACE_REGS)
    return (GET_CODE (op) == REG);

  /* While it's always safe to index off the frame pointer, it's not
     always profitable, particularly when the frame pointer is being
     eliminated.  */
  if (! flag_omit_frame_pointer && op == frame_pointer_rtx)
    return 1;

  return (GET_CODE (op) == REG
          && REG_POINTER (op)
          && register_operand (op, mode));
}

/* Return 1 if this operand is anything other than a hard register.  */

int
non_hard_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ! (GET_CODE (op) == REG && REGNO (op) < FIRST_PSEUDO_REGISTER);
}

/* Return 1 if INSN branches forward.  Should be using insn_addresses
   to avoid walking through all the insns...  */
static int
forward_branch_p (insn)
     rtx insn;
{
  rtx label = JUMP_LABEL (insn);

  while (insn)
    {
      if (insn == label)
	break;
      else
	insn = NEXT_INSN (insn);
    }

  return (insn == label);
}

/* Return 1 if OP is an equality comparison, else return 0.  */
int
eq_neq_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if OP is an operator suitable for use in a movb instruction.  */
int
movb_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE
	  || GET_CODE (op) == LT || GET_CODE (op) == GE);
}

/* Return 1 if INSN is in the delay slot of a call instruction.  */
int
jump_in_call_delay (insn)
     rtx insn;
{

  if (GET_CODE (insn) != JUMP_INSN)
    return 0;

  if (PREV_INSN (insn)
      && PREV_INSN (PREV_INSN (insn))
      && GET_CODE (next_real_insn (PREV_INSN (PREV_INSN (insn)))) == INSN)
    {
      rtx test_insn = next_real_insn (PREV_INSN (PREV_INSN (insn)));

      return (GET_CODE (PATTERN (test_insn)) == SEQUENCE
	      && XVECEXP (PATTERN (test_insn), 0, 1) == insn);

    }
  else
    return 0;
}

/* Output an unconditional move and branch insn.  */

const char *
output_parallel_movb (operands, length)
     rtx *operands;
     int length;
{
  /* These are the cases in which we win.  */
  if (length == 4)
    return "mov%I1b,tr %1,%0,%2";

  /* None of these cases wins, but they don't lose either.  */
  if (dbr_sequence_length () == 0)
    {
      /* Nothing in the delay slot, fake it by putting the combined
	 insn (the copy or add) in the delay slot of a bl.  */
      if (GET_CODE (operands[1]) == CONST_INT)
	return "b %2\n\tldi %1,%0";
      else
	return "b %2\n\tcopy %1,%0";
    }
  else
    {
      /* Something in the delay slot, but we've got a long branch.  */
      if (GET_CODE (operands[1]) == CONST_INT)
	return "ldi %1,%0\n\tb %2";
      else
	return "copy %1,%0\n\tb %2";
    }
}

/* Output an unconditional add and branch insn.  */

const char *
output_parallel_addb (operands, length)
     rtx *operands;
     int length;
{
  /* To make life easy we want operand0 to be the shared input/output
     operand and operand1 to be the readonly operand.  */
  if (operands[0] == operands[1])
    operands[1] = operands[2];

  /* These are the cases in which we win.  */
  if (length == 4)
    return "add%I1b,tr %1,%0,%3";

  /* None of these cases win, but they don't lose either.  */
  if (dbr_sequence_length () == 0)
    {
      /* Nothing in the delay slot, fake it by putting the combined
	 insn (the copy or add) in the delay slot of a bl.  */
      return "b %3\n\tadd%I1 %1,%0,%0";
    }
  else
    {
      /* Something in the delay slot, but we've got a long branch.  */
      return "add%I1 %1,%0,%0\n\tb %3";
    }
}

/* Return nonzero if INSN (a jump insn) immediately follows a call
   to a named function.  This is used to avoid filling the delay slot
   of the jump since it can usually be eliminated by modifying RP in
   the delay slot of the call.  */

int
following_call (insn)
     rtx insn;
{
  if (! TARGET_JUMP_IN_DELAY)
    return 0;

  /* Find the previous real insn, skipping NOTEs.  */
  insn = PREV_INSN (insn);
  while (insn && GET_CODE (insn) == NOTE)
    insn = PREV_INSN (insn);

  /* Check for CALL_INSNs and millicode calls.  */
  if (insn
      && ((GET_CODE (insn) == CALL_INSN
	   && get_attr_type (insn) != TYPE_DYNCALL)
	  || (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) != SEQUENCE
	      && GET_CODE (PATTERN (insn)) != USE
	      && GET_CODE (PATTERN (insn)) != CLOBBER
	      && get_attr_type (insn) == TYPE_MILLI)))
    return 1;

  return 0;
}

/* We use this hook to perform a PA specific optimization which is difficult
   to do in earlier passes.

   We want the delay slots of branches within jump tables to be filled.
   None of the compiler passes at the moment even has the notion that a
   PA jump table doesn't contain addresses, but instead contains actual
   instructions!

   Because we actually jump into the table, the addresses of each entry
   must stay constant in relation to the beginning of the table (which
   itself must stay constant relative to the instruction to jump into
   it).  I don't believe we can guarantee earlier passes of the compiler
   will adhere to those rules.

   So, late in the compilation process we find all the jump tables, and
   expand them into real code -- eg each entry in the jump table vector
   will get an appropriate label followed by a jump to the final target.

   Reorg and the final jump pass can then optimize these branches and
   fill their delay slots.  We end up with smaller, more efficient code.

   The jump instructions within the table are special; we must be able
   to identify them during assembly output (if the jumps don't get filled
   we need to emit a nop rather than nullifying the delay slot)).  We
   identify jumps in switch tables by marking the SET with DImode.

   We also surround the jump table itself with BEGIN_BRTAB and END_BRTAB
   insns.  This serves two purposes, first it prevents jump.c from
   noticing that the last N entries in the table jump to the instruction
   immediately after the table and deleting the jumps.  Second, those
   insns mark where we should emit .begin_brtab and .end_brtab directives
   when using GAS (allows for better link time optimizations).  */

void
pa_reorg (insns)
     rtx insns;
{
  rtx insn;

  remove_useless_addtr_insns (insns, 1);

  if (pa_cpu < PROCESSOR_8000)
    pa_combine_instructions (get_insns ());


  /* This is fairly cheap, so always run it if optimizing.  */
  if (optimize > 0 && !TARGET_BIG_SWITCH)
    {
      /* Find and explode all ADDR_VEC or ADDR_DIFF_VEC insns.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	{
	  rtx pattern, tmp, location;
	  unsigned int length, i;

	  /* Find an ADDR_VEC or ADDR_DIFF_VEC insn to explode.  */
	  if (GET_CODE (insn) != JUMP_INSN
	      || (GET_CODE (PATTERN (insn)) != ADDR_VEC
		  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC))
	    continue;

	  /* Emit marker for the beginning of the branch table.  */
	  emit_insn_before (gen_begin_brtab (), insn);

	  pattern = PATTERN (insn);
	  location = PREV_INSN (insn);
          length = XVECLEN (pattern, GET_CODE (pattern) == ADDR_DIFF_VEC);

	  for (i = 0; i < length; i++)
	    {
	      /* Emit a label before each jump to keep jump.c from
		 removing this code.  */
	      tmp = gen_label_rtx ();
	      LABEL_NUSES (tmp) = 1;
	      emit_label_after (tmp, location);
	      location = NEXT_INSN (location);

	      if (GET_CODE (pattern) == ADDR_VEC)
		{
		  /* Emit the jump itself.  */
		  tmp = gen_jump (XEXP (XVECEXP (pattern, 0, i), 0));
		  tmp = emit_jump_insn_after (tmp, location);
		  JUMP_LABEL (tmp) = XEXP (XVECEXP (pattern, 0, i), 0);
		  /* It is easy to rely on the branch table markers
		     during assembly output to trigger the correct code
		     for a switch table jump with an unfilled delay slot,

		     However, that requires state and assumes that we look
		     at insns in order.

		     We can't make such assumptions when computing the length
		     of instructions.  Ugh.  We could walk the insn chain to
		     determine if this instruction is in a branch table, but
		     that can get rather expensive, particularly during the
		     branch shortening phase of the compiler.

		     So instead we mark this jump as being special.  This is
		     far from ideal and knows that no code after this will
		     muck around with the mode of the JUMP_INSN itself.  */
		  PUT_MODE (tmp, SImode);
		  LABEL_NUSES (JUMP_LABEL (tmp))++;
		  location = NEXT_INSN (location);
		}
	      else
		{
		  /* Emit the jump itself.  */
		  tmp = gen_jump (XEXP (XVECEXP (pattern, 1, i), 0));
		  tmp = emit_jump_insn_after (tmp, location);
		  JUMP_LABEL (tmp) = XEXP (XVECEXP (pattern, 1, i), 0);
		  /* It is easy to rely on the branch table markers
		     during assembly output to trigger the correct code
		     for a switch table jump with an unfilled delay slot,

		     However, that requires state and assumes that we look
		     at insns in order.

		     We can't make such assumptions when computing the length
		     of instructions.  Ugh.  We could walk the insn chain to
		     determine if this instruction is in a branch table, but
		     that can get rather expensive, particularly during the
		     branch shortening phase of the compiler.

		     So instead we mark this jump as being special.  This is
		     far from ideal and knows that no code after this will
		     muck around with the mode of the JUMP_INSN itself.  */
		  PUT_MODE (tmp, SImode);
		  LABEL_NUSES (JUMP_LABEL (tmp))++;
		  location = NEXT_INSN (location);
		}

	      /* Emit a BARRIER after the jump.  */
	      emit_barrier_after (location);
	      location = NEXT_INSN (location);
	    }

	  /* Emit marker for the end of the branch table.  */
	  emit_insn_before (gen_end_brtab (), location);
	  location = NEXT_INSN (location);
	  emit_barrier_after (location);

	  /* Delete the ADDR_VEC or ADDR_DIFF_VEC.  */
	  delete_insn (insn);
	}
    }
  else
    {
      /* Sill need an end_brtab insn.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	{
	  /* Find an ADDR_VEC insn.  */
	  if (GET_CODE (insn) != JUMP_INSN
	      || (GET_CODE (PATTERN (insn)) != ADDR_VEC
		  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC))
	    continue;

	  /* Now generate markers for the beginning and end of the
	     branch table.  */
	  emit_insn_before (gen_begin_brtab (), insn);
	  emit_insn_after (gen_end_brtab (), insn);
	}
    }
}

/* The PA has a number of odd instructions which can perform multiple
   tasks at once.  On first generation PA machines (PA1.0 and PA1.1)
   it may be profitable to combine two instructions into one instruction
   with two outputs.  It's not profitable PA2.0 machines because the
   two outputs would take two slots in the reorder buffers.

   This routine finds instructions which can be combined and combines
   them.  We only support some of the potential combinations, and we
   only try common ways to find suitable instructions.

      * addb can add two registers or a register and a small integer
      and jump to a nearby (+-8k) location.  Normally the jump to the
      nearby location is conditional on the result of the add, but by
      using the "true" condition we can make the jump unconditional.
      Thus addb can perform two independent operations in one insn.

      * movb is similar to addb in that it can perform a reg->reg
      or small immediate->reg copy and jump to a nearby (+-8k location).

      * fmpyadd and fmpysub can perform a FP multiply and either an
      FP add or FP sub if the operands of the multiply and add/sub are
      independent (there are other minor restrictions).  Note both
      the fmpy and fadd/fsub can in theory move to better spots according
      to data dependencies, but for now we require the fmpy stay at a
      fixed location.

      * Many of the memory operations can perform pre & post updates
      of index registers.  GCC's pre/post increment/decrement addressing
      is far too simple to take advantage of all the possibilities.  This
      pass may not be suitable since those insns may not be independent.

      * comclr can compare two ints or an int and a register, nullify
      the following instruction and zero some other register.  This
      is more difficult to use as it's harder to find an insn which
      will generate a comclr than finding something like an unconditional
      branch.  (conditional moves & long branches create comclr insns).

      * Most arithmetic operations can conditionally skip the next
      instruction.  They can be viewed as "perform this operation
      and conditionally jump to this nearby location" (where nearby
      is an insns away).  These are difficult to use due to the
      branch length restrictions.  */

static void
pa_combine_instructions (insns)
     rtx insns ATTRIBUTE_UNUSED;
{
  rtx anchor, new;

  /* This can get expensive since the basic algorithm is on the
     order of O(n^2) (or worse).  Only do it for -O2 or higher
     levels of optimization.  */
  if (optimize < 2)
    return;

  /* Walk down the list of insns looking for "anchor" insns which
     may be combined with "floating" insns.  As the name implies,
     "anchor" instructions don't move, while "floating" insns may
     move around.  */
  new = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, NULL_RTX, NULL_RTX));
  new = make_insn_raw (new);

  for (anchor = get_insns (); anchor; anchor = NEXT_INSN (anchor))
    {
      enum attr_pa_combine_type anchor_attr;
      enum attr_pa_combine_type floater_attr;

      /* We only care about INSNs, JUMP_INSNs, and CALL_INSNs.
	 Also ignore any special USE insns.  */
      if ((GET_CODE (anchor) != INSN
	  && GET_CODE (anchor) != JUMP_INSN
	  && GET_CODE (anchor) != CALL_INSN)
	  || GET_CODE (PATTERN (anchor)) == USE
	  || GET_CODE (PATTERN (anchor)) == CLOBBER
	  || GET_CODE (PATTERN (anchor)) == ADDR_VEC
	  || GET_CODE (PATTERN (anchor)) == ADDR_DIFF_VEC)
	continue;

      anchor_attr = get_attr_pa_combine_type (anchor);
      /* See if anchor is an insn suitable for combination.  */
      if (anchor_attr == PA_COMBINE_TYPE_FMPY
	  || anchor_attr == PA_COMBINE_TYPE_FADDSUB
	  || (anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH
	      && ! forward_branch_p (anchor)))
	{
	  rtx floater;

	  for (floater = PREV_INSN (anchor);
	       floater;
	       floater = PREV_INSN (floater))
	    {
	      if (GET_CODE (floater) == NOTE
		  || (GET_CODE (floater) == INSN
		      && (GET_CODE (PATTERN (floater)) == USE
			  || GET_CODE (PATTERN (floater)) == CLOBBER)))
		continue;

	      /* Anything except a regular INSN will stop our search.  */
	      if (GET_CODE (floater) != INSN
		  || GET_CODE (PATTERN (floater)) == ADDR_VEC
		  || GET_CODE (PATTERN (floater)) == ADDR_DIFF_VEC)
		{
		  floater = NULL_RTX;
		  break;
		}

	      /* See if FLOATER is suitable for combination with the
		 anchor.  */
	      floater_attr = get_attr_pa_combine_type (floater);
	      if ((anchor_attr == PA_COMBINE_TYPE_FMPY
		   && floater_attr == PA_COMBINE_TYPE_FADDSUB)
		  || (anchor_attr == PA_COMBINE_TYPE_FADDSUB
		      && floater_attr == PA_COMBINE_TYPE_FMPY))
		{
		  /* If ANCHOR and FLOATER can be combined, then we're
		     done with this pass.  */
		  if (pa_can_combine_p (new, anchor, floater, 0,
					SET_DEST (PATTERN (floater)),
					XEXP (SET_SRC (PATTERN (floater)), 0),
					XEXP (SET_SRC (PATTERN (floater)), 1)))
		    break;
		}

	      else if (anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH
		       && floater_attr == PA_COMBINE_TYPE_ADDMOVE)
		{
		  if (GET_CODE (SET_SRC (PATTERN (floater))) == PLUS)
		    {
		      if (pa_can_combine_p (new, anchor, floater, 0,
					    SET_DEST (PATTERN (floater)),
					XEXP (SET_SRC (PATTERN (floater)), 0),
					XEXP (SET_SRC (PATTERN (floater)), 1)))
			break;
		    }
		  else
		    {
		      if (pa_can_combine_p (new, anchor, floater, 0,
					    SET_DEST (PATTERN (floater)),
					    SET_SRC (PATTERN (floater)),
					    SET_SRC (PATTERN (floater))))
			break;
		    }
		}
	    }

	  /* If we didn't find anything on the backwards scan try forwards.  */
	  if (!floater
	      && (anchor_attr == PA_COMBINE_TYPE_FMPY
		  || anchor_attr == PA_COMBINE_TYPE_FADDSUB))
	    {
	      for (floater = anchor; floater; floater = NEXT_INSN (floater))
		{
		  if (GET_CODE (floater) == NOTE
		      || (GET_CODE (floater) == INSN
			  && (GET_CODE (PATTERN (floater)) == USE
			      || GET_CODE (PATTERN (floater)) == CLOBBER)))

		    continue;

		  /* Anything except a regular INSN will stop our search.  */
		  if (GET_CODE (floater) != INSN
		      || GET_CODE (PATTERN (floater)) == ADDR_VEC
		      || GET_CODE (PATTERN (floater)) == ADDR_DIFF_VEC)
		    {
		      floater = NULL_RTX;
		      break;
		    }

		  /* See if FLOATER is suitable for combination with the
		     anchor.  */
		  floater_attr = get_attr_pa_combine_type (floater);
		  if ((anchor_attr == PA_COMBINE_TYPE_FMPY
		       && floater_attr == PA_COMBINE_TYPE_FADDSUB)
		      || (anchor_attr == PA_COMBINE_TYPE_FADDSUB
			  && floater_attr == PA_COMBINE_TYPE_FMPY))
		    {
		      /* If ANCHOR and FLOATER can be combined, then we're
			 done with this pass.  */
		      if (pa_can_combine_p (new, anchor, floater, 1,
					    SET_DEST (PATTERN (floater)),
					    XEXP (SET_SRC (PATTERN (floater)),
						  0),
					    XEXP (SET_SRC (PATTERN (floater)),
						  1)))
			break;
		    }
		}
	    }

	  /* FLOATER will be nonzero if we found a suitable floating
	     insn for combination with ANCHOR.  */
	  if (floater
	      && (anchor_attr == PA_COMBINE_TYPE_FADDSUB
		  || anchor_attr == PA_COMBINE_TYPE_FMPY))
	    {
	      /* Emit the new instruction and delete the old anchor.  */
	      emit_insn_before (gen_rtx_PARALLEL
				(VOIDmode,
				 gen_rtvec (2, PATTERN (anchor),
					    PATTERN (floater))),
				anchor);

	      PUT_CODE (anchor, NOTE);
	      NOTE_LINE_NUMBER (anchor) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (anchor) = 0;

	      /* Emit a special USE insn for FLOATER, then delete
		 the floating insn.  */
	      emit_insn_before (gen_rtx_USE (VOIDmode, floater), floater);
	      delete_insn (floater);

	      continue;
	    }
	  else if (floater
		   && anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH)
	    {
	      rtx temp;
	      /* Emit the new_jump instruction and delete the old anchor.  */
	      temp
		= emit_jump_insn_before (gen_rtx_PARALLEL
					 (VOIDmode,
					  gen_rtvec (2, PATTERN (anchor),
						     PATTERN (floater))),
					 anchor);

	      JUMP_LABEL (temp) = JUMP_LABEL (anchor);
	      PUT_CODE (anchor, NOTE);
	      NOTE_LINE_NUMBER (anchor) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (anchor) = 0;

	      /* Emit a special USE insn for FLOATER, then delete
		 the floating insn.  */
	      emit_insn_before (gen_rtx_USE (VOIDmode, floater), floater);
	      delete_insn (floater);
	      continue;
	    }
	}
    }
}

static int
pa_can_combine_p (new, anchor, floater, reversed, dest, src1, src2)
     rtx new, anchor, floater;
     int reversed;
     rtx dest, src1, src2;
{
  int insn_code_number;
  rtx start, end;

  /* Create a PARALLEL with the patterns of ANCHOR and
     FLOATER, try to recognize it, then test constraints
     for the resulting pattern.

     If the pattern doesn't match or the constraints
     aren't met keep searching for a suitable floater
     insn.  */
  XVECEXP (PATTERN (new), 0, 0) = PATTERN (anchor);
  XVECEXP (PATTERN (new), 0, 1) = PATTERN (floater);
  INSN_CODE (new) = -1;
  insn_code_number = recog_memoized (new);
  if (insn_code_number < 0
      || (extract_insn (new), ! constrain_operands (1)))
    return 0;

  if (reversed)
    {
      start = anchor;
      end = floater;
    }
  else
    {
      start = floater;
      end = anchor;
    }

  /* There's up to three operands to consider.  One
     output and two inputs.

     The output must not be used between FLOATER & ANCHOR
     exclusive.  The inputs must not be set between
     FLOATER and ANCHOR exclusive.  */

  if (reg_used_between_p (dest, start, end))
    return 0;

  if (reg_set_between_p (src1, start, end))
    return 0;

  if (reg_set_between_p (src2, start, end))
    return 0;

  /* If we get here, then everything is good.  */
  return 1;
}

/* Return nonzero if references for INSN are delayed.

   Millicode insns are actually function calls with some special
   constraints on arguments and register usage.

   Millicode calls always expect their arguments in the integer argument
   registers, and always return their result in %r29 (ret1).  They
   are expected to clobber their arguments, %r1, %r29, and the return
   pointer which is %r31 on 32-bit and %r2 on 64-bit, and nothing else.

   This function tells reorg that the references to arguments and
   millicode calls do not appear to happen until after the millicode call.
   This allows reorg to put insns which set the argument registers into the
   delay slot of the millicode call -- thus they act more like traditional
   CALL_INSNs.

   Note we can not consider side effects of the insn to be delayed because
   the branch and link insn will clobber the return pointer.  If we happened
   to use the return pointer in the delay slot of the call, then we lose.

   get_attr_type will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */
int
insn_refs_are_delayed (insn)
     rtx insn;
{
  return ((GET_CODE (insn) == INSN
	   && GET_CODE (PATTERN (insn)) != SEQUENCE
	   && GET_CODE (PATTERN (insn)) != USE
	   && GET_CODE (PATTERN (insn)) != CLOBBER
	   && get_attr_type (insn) == TYPE_MILLI));
}

/* On the HP-PA the value is found in register(s) 28(-29), unless
   the mode is SF or DF. Then the value is returned in fr4 (32).

   This must perform the same promotions as PROMOTE_MODE, else
   PROMOTE_FUNCTION_RETURN will not work correctly.

   Small structures must be returned in a PARALLEL on PA64 in order
   to match the HP Compiler ABI.  */

rtx
function_value (valtype, func)
    tree valtype;
    tree func ATTRIBUTE_UNUSED;
{
  enum machine_mode valmode;

  /* Aggregates with a size less than or equal to 128 bits are returned
     in GR 28(-29).  They are left justified.  The pad bits are undefined.
     Larger aggregates are returned in memory.  */
  if (TARGET_64BIT && AGGREGATE_TYPE_P (valtype))
    {
      rtx loc[2];
      int i, offset = 0;
      int ub = int_size_in_bytes (valtype) <= UNITS_PER_WORD ? 1 : 2;

      for (i = 0; i < ub; i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (DImode, 28 + i),
				      GEN_INT (offset));
	  offset += 8;
	}

      return gen_rtx_PARALLEL (BLKmode, gen_rtvec_v (ub, loc));
    }

  if ((INTEGRAL_TYPE_P (valtype)
       && TYPE_PRECISION (valtype) < BITS_PER_WORD)
      || POINTER_TYPE_P (valtype))
    valmode = word_mode;
  else
    valmode = TYPE_MODE (valtype);

  if (TREE_CODE (valtype) == REAL_TYPE
      && TYPE_MODE (valtype) != TFmode
      && !TARGET_SOFT_FLOAT)
    return gen_rtx_REG (valmode, 32);

  return gen_rtx_REG (valmode, 28);
}

/* Return the location of a parameter that is passed in a register or NULL
   if the parameter has any component that is passed in memory.

   This is new code and will be pushed to into the net sources after
   further testing.

   ??? We might want to restructure this so that it looks more like other
   ports.  */
rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int max_arg_words = (TARGET_64BIT ? 8 : 4);
  int alignment = 0;
  int arg_size;
  int fpr_reg_base;
  int gpr_reg_base;
  rtx retval;

  if (mode == VOIDmode)
    return NULL_RTX;

  arg_size = FUNCTION_ARG_SIZE (mode, type);

  /* If this arg would be passed partially or totally on the stack, then
     this routine should return zero.  FUNCTION_ARG_PARTIAL_NREGS will
     handle arguments which are split between regs and stack slots if
     the ABI mandates split arguments.  */
  if (! TARGET_64BIT)
    {
      /* The 32-bit ABI does not split arguments.  */
      if (cum->words + arg_size > max_arg_words)
	return NULL_RTX;
    }
  else
    {
      if (arg_size > 1)
	alignment = cum->words & 1;
      if (cum->words + alignment >= max_arg_words)
	return NULL_RTX;
    }

  /* The 32bit ABIs and the 64bit ABIs are rather different,
     particularly in their handling of FP registers.  We might
     be able to cleverly share code between them, but I'm not
     going to bother in the hope that splitting them up results
     in code that is more easily understood.  */

  if (TARGET_64BIT)
    {
      /* Advance the base registers to their current locations.

         Remember, gprs grow towards smaller register numbers while
	 fprs grow to higher register numbers.  Also remember that
	 although FP regs are 32-bit addressable, we pretend that
	 the registers are 64-bits wide.  */
      gpr_reg_base = 26 - cum->words;
      fpr_reg_base = 32 + cum->words;

      /* Arguments wider than one word and small aggregates need special
	 treatment.  */
      if (arg_size > 1
	  || mode == BLKmode
	  || (type && AGGREGATE_TYPE_P (type)))
	{
	  /* Double-extended precision (80-bit), quad-precision (128-bit)
	     and aggregates including complex numbers are aligned on
	     128-bit boundaries.  The first eight 64-bit argument slots
	     are associated one-to-one, with general registers r26
	     through r19, and also with floating-point registers fr4
	     through fr11.  Arguments larger than one word are always
	     passed in general registers.

	     Using a PARALLEL with a word mode register results in left
	     justified data on a big-endian target.  */

	  rtx loc[8];
	  int i, offset = 0, ub = arg_size;

	  /* Align the base register.  */
	  gpr_reg_base -= alignment;

	  ub = MIN (ub, max_arg_words - cum->words - alignment);
	  for (i = 0; i < ub; i++)
	    {
	      loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
					  gen_rtx_REG (DImode, gpr_reg_base),
					  GEN_INT (offset));
	      gpr_reg_base -= 1;
	      offset += 8;
	    }

	  return gen_rtx_PARALLEL (mode, gen_rtvec_v (ub, loc));
	}
     }
  else
    {
      /* If the argument is larger than a word, then we know precisely
	 which registers we must use.  */
      if (arg_size > 1)
	{
	  if (cum->words)
	    {
	      gpr_reg_base = 23;
	      fpr_reg_base = 38;
	    }
	  else
	    {
	      gpr_reg_base = 25;
	      fpr_reg_base = 34;
	    }

	  /* Structures 5 to 8 bytes in size are passed in the general
	     registers in the same manner as other non floating-point
	     objects.  The data is right-justified and zero-extended
	     to 64 bits.

	     This is magic.  Normally, using a PARALLEL results in left
	     justified data on a big-endian target.  However, using a
	     single double-word register provides the required right
	     justication for 5 to 8 byte structures.  This has nothing
	     to do with the direction of padding specified for the argument.
	     It has to do with how the data is widened and shifted into
	     and from the register.

	     Aside from adding load_multiple and store_multiple patterns,
	     this is the only way that I have found to obtain right
	     justification of BLKmode data when it has a size greater
	     than one word.  Splitting the operation into two SImode loads
	     or returning a DImode REG results in left justified data.  */
	  if (mode == BLKmode)
	    {
	      rtx loc = gen_rtx_EXPR_LIST (VOIDmode,
					   gen_rtx_REG (DImode, gpr_reg_base),
					   const0_rtx);
	      return gen_rtx_PARALLEL (mode, gen_rtvec (1, loc));
	    }
	}
      else
        {
	   /* We have a single word (32 bits).  A simple computation
	      will get us the register #s we need.  */
	   gpr_reg_base = 26 - cum->words;
	   fpr_reg_base = 32 + 2 * cum->words;
	}
    }

  /* Determine if the argument needs to be passed in both general and
     floating point registers.  */
  if (((TARGET_PORTABLE_RUNTIME || TARGET_64BIT || TARGET_ELF32)
       /* If we are doing soft-float with portable runtime, then there
	  is no need to worry about FP regs.  */
       && !TARGET_SOFT_FLOAT
       /* The parameter must be some kind of float, else we can just
	  pass it in integer registers.  */
       && FLOAT_MODE_P (mode)
       /* The target function must not have a prototype.  */
       && cum->nargs_prototype <= 0
       /* libcalls do not need to pass items in both FP and general
	  registers.  */
       && type != NULL_TREE
       /* All this hair applies to "outgoing" args only.  This includes
	  sibcall arguments setup with FUNCTION_INCOMING_ARG.  */
       && !cum->incoming)
      /* Also pass outgoing floating arguments in both registers in indirect
	 calls with the 32 bit ABI and the HP assembler since there is no
	 way to the specify argument locations in static functions.  */
      || (!TARGET_64BIT
	  && !TARGET_GAS
	  && !cum->incoming
	  && cum->indirect
	  && FLOAT_MODE_P (mode)))
    {
      retval
	= gen_rtx_PARALLEL
	    (mode,
	     gen_rtvec (2,
			gen_rtx_EXPR_LIST (VOIDmode,
					   gen_rtx_REG (mode, fpr_reg_base),
					   const0_rtx),
			gen_rtx_EXPR_LIST (VOIDmode,
					   gen_rtx_REG (mode, gpr_reg_base),
					   const0_rtx)));
    }
  else
    {
      /* See if we should pass this parameter in a general register.  */
      if (TARGET_SOFT_FLOAT
	  /* Indirect calls in the normal 32bit ABI require all arguments
	     to be passed in general registers.  */
	  || (!TARGET_PORTABLE_RUNTIME
	      && !TARGET_64BIT
	      && !TARGET_ELF32
	      && cum->indirect)
	  /* If the parameter is not a floating point parameter, then
	     it belongs in GPRs.  */
	  || !FLOAT_MODE_P (mode))
	retval = gen_rtx_REG (mode, gpr_reg_base);
      else
	retval = gen_rtx_REG (mode, fpr_reg_base);
    }
  return retval;
}


/* If this arg would be passed totally in registers or totally on the stack,
   then this routine should return zero. It is currently called only for
   the 64-bit target.  */
int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  unsigned int max_arg_words = 8;
  unsigned int offset = 0;

  if (FUNCTION_ARG_SIZE (mode, type) > 1 && (cum->words & 1))
    offset = 1;

  if (cum->words + offset + FUNCTION_ARG_SIZE (mode, type) <= max_arg_words)
    /* Arg fits fully into registers.  */
    return 0;
  else if (cum->words + offset >= max_arg_words)
    /* Arg fully on the stack.  */
    return 0;
  else
    /* Arg is split.  */
    return max_arg_words - cum->words - offset;
}


/* Return 1 if this is a comparison operator.  This allows the use of
   MATCH_OPERATOR to recognize all the branch insns.  */

int
cmpib_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
          && (GET_CODE (op) == EQ
	      || GET_CODE (op) == NE
	      || GET_CODE (op) == GT
	      || GET_CODE (op) == GTU
	      || GET_CODE (op) == GE
	      || GET_CODE (op) == LT
	      || GET_CODE (op) == LE
	      || GET_CODE (op) == LEU));
}

/* On hpux10, the linker will give an error if we have a reference
   in the read-only data section to a symbol defined in a shared
   library.  Therefore, expressions that might require a reloc can
   not be placed in the read-only data section.  */

static void
pa_select_section (exp, reloc, align)
     tree exp;
     int reloc;
     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
{
  if (TREE_CODE (exp) == VAR_DECL
      && TREE_READONLY (exp)
      && !TREE_THIS_VOLATILE (exp)
      && DECL_INITIAL (exp)
      && (DECL_INITIAL (exp) == error_mark_node
          || TREE_CONSTANT (DECL_INITIAL (exp)))
      && !reloc)
    readonly_data_section ();
  else if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'c'
	   && !(TREE_CODE (exp) == STRING_CST && flag_writable_strings)
	   && !reloc)
    readonly_data_section ();
  else
    data_section ();
}

static void
pa_globalize_label (stream, name)
     FILE *stream;
     const char *name;
{
  /* We only handle DATA objects here, functions are globalized in
     ASM_DECLARE_FUNCTION_NAME.  */
  if (! FUNCTION_NAME_P (name))
  {
    fputs ("\t.EXPORT ", stream);
    assemble_name (stream, name);
    fputs (",DATA\n", stream);
  }
}
#include "gt-pa.h"
