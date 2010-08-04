/* Copyright (C) 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "basic-block.h"
#include "integrate.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "reload.h"
#include "cfglayout.h"
#include "sched-int.h"
#include "params.h"
#include "assert.h"
#include "c-common.h"
#include "machmode.h"
#include "gimple.h"
#include "tm-constrs.h"
#include "ddg.h"
#include "sbitmap.h"
#include "timevar.h"
#include "df.h"

/* Builtin types, data and prototypes. */

enum spu_builtin_type_index
{
  SPU_BTI_END_OF_PARAMS,

  /* We create new type nodes for these. */
  SPU_BTI_V16QI,
  SPU_BTI_V8HI,
  SPU_BTI_V4SI,
  SPU_BTI_V2DI,
  SPU_BTI_V4SF,
  SPU_BTI_V2DF,
  SPU_BTI_UV16QI,
  SPU_BTI_UV8HI,
  SPU_BTI_UV4SI,
  SPU_BTI_UV2DI,

  /* A 16-byte type. (Implemented with V16QI_type_node) */
  SPU_BTI_QUADWORD,

  /* These all correspond to intSI_type_node */
  SPU_BTI_7,
  SPU_BTI_S7,
  SPU_BTI_U7,
  SPU_BTI_S10,
  SPU_BTI_S10_4,
  SPU_BTI_U14,
  SPU_BTI_16,
  SPU_BTI_S16,
  SPU_BTI_S16_2,
  SPU_BTI_U16,
  SPU_BTI_U16_2,
  SPU_BTI_U18,

  /* These correspond to the standard types */
  SPU_BTI_INTQI, 
  SPU_BTI_INTHI, 
  SPU_BTI_INTSI, 
  SPU_BTI_INTDI, 

  SPU_BTI_UINTQI,
  SPU_BTI_UINTHI,
  SPU_BTI_UINTSI,
  SPU_BTI_UINTDI,

  SPU_BTI_FLOAT, 
  SPU_BTI_DOUBLE,

  SPU_BTI_VOID,   
  SPU_BTI_PTR,   

  SPU_BTI_MAX
};

#define V16QI_type_node               (spu_builtin_types[SPU_BTI_V16QI])
#define V8HI_type_node                (spu_builtin_types[SPU_BTI_V8HI])
#define V4SI_type_node                (spu_builtin_types[SPU_BTI_V4SI])
#define V2DI_type_node                (spu_builtin_types[SPU_BTI_V2DI])
#define V4SF_type_node                (spu_builtin_types[SPU_BTI_V4SF])
#define V2DF_type_node                (spu_builtin_types[SPU_BTI_V2DF])
#define unsigned_V16QI_type_node      (spu_builtin_types[SPU_BTI_UV16QI])
#define unsigned_V8HI_type_node       (spu_builtin_types[SPU_BTI_UV8HI])
#define unsigned_V4SI_type_node       (spu_builtin_types[SPU_BTI_UV4SI])
#define unsigned_V2DI_type_node       (spu_builtin_types[SPU_BTI_UV2DI])

static GTY(()) tree spu_builtin_types[SPU_BTI_MAX];

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


/*  Target specific attribute specifications.  */
char regs_ever_allocated[FIRST_PSEUDO_REGISTER];

/*  Prototypes and external defs.  */
static void spu_init_builtins (void);
static unsigned char spu_scalar_mode_supported_p (enum machine_mode mode);
static unsigned char spu_vector_mode_supported_p (enum machine_mode mode);
static rtx adjust_operand (rtx op, HOST_WIDE_INT * start);
static rtx get_pic_reg (void);
static int need_to_save_reg (int regno, int saving);
static rtx frame_emit_store (int regno, rtx addr, HOST_WIDE_INT offset);
static rtx frame_emit_load (int regno, rtx addr, HOST_WIDE_INT offset);
static rtx frame_emit_add_imm (rtx dst, rtx src, HOST_WIDE_INT imm,
			       rtx scratch);
static void emit_nop_for_insn (rtx insn);
static bool insn_clobbers_hbr (rtx insn);
static void spu_emit_branch_hint (rtx before, rtx branch, rtx target,
				  int distance, sbitmap blocks);
static rtx spu_emit_vector_compare (enum rtx_code rcode, rtx op0, rtx op1,
	                            enum machine_mode dmode);
static rtx get_branch_target (rtx branch);
static void spu_machine_dependent_reorg (void);
static int spu_sched_issue_rate (void);
static int spu_sched_variable_issue (FILE * dump, int verbose, rtx insn,
				     int can_issue_more);
static int get_pipe (rtx insn);
static int spu_sched_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost);
static void spu_sched_init_global (FILE *, int, int);
static void spu_sched_init (FILE *, int, int);
static int spu_sched_reorder (FILE *, int, rtx *, int *, int);
static tree spu_handle_fndecl_attribute (tree * node, tree name, tree args,
					 int flags,
					 unsigned char *no_add_attrs);
static tree spu_handle_vector_attribute (tree * node, tree name, tree args,
					 int flags,
					 unsigned char *no_add_attrs);
static int spu_naked_function_p (tree func);
static unsigned char spu_pass_by_reference (CUMULATIVE_ARGS *cum, enum machine_mode mode,
					    const_tree type, unsigned char named);
static tree spu_build_builtin_va_list (void);
static void spu_va_start (tree, rtx);
static tree spu_gimplify_va_arg_expr (tree valist, tree type,
				      gimple_seq * pre_p, gimple_seq * post_p);
static int store_with_one_insn_p (rtx mem);
static int mem_is_padded_component_ref (rtx x);
static int reg_aligned_for_addr (rtx x);
static bool spu_assemble_integer (rtx x, unsigned int size, int aligned_p);
static void spu_asm_globalize_label (FILE * file, const char *name);
static unsigned char spu_rtx_costs (rtx x, int code, int outer_code,
				    int *total, bool speed);
static unsigned char spu_function_ok_for_sibcall (tree decl, tree exp);
static void spu_init_libfuncs (void);
static bool spu_return_in_memory (const_tree type, const_tree fntype);
static void fix_range (const char *);
static void spu_encode_section_info (tree, rtx, int);
static tree spu_builtin_mul_widen_even (tree);
static tree spu_builtin_mul_widen_odd (tree);
static tree spu_builtin_mask_for_load (void);
static int spu_builtin_vectorization_cost (bool);
static bool spu_vector_alignment_reachable (const_tree, bool);
static tree spu_builtin_vec_perm (tree, tree *);
static int spu_sms_res_mii (struct ddg *g);
static void asm_file_start (void);
static unsigned int spu_section_type_flags (tree, const char *, int);
static rtx spu_expand_load (rtx, rtx, rtx, int);

extern const char *reg_names[];
rtx spu_compare_op0, spu_compare_op1;

/* Which instruction set architecture to use.  */
int spu_arch;
/* Which cpu are we tuning for.  */
int spu_tune;

/* The hardware requires 8 insns between a hint and the branch it
   effects.  This variable describes how many rtl instructions the
   compiler needs to see before inserting a hint, and then the compiler
   will insert enough nops to make it at least 8 insns.  The default is
   for the compiler to allow up to 2 nops be emitted.  The nops are
   inserted in pairs, so we round down. */
int spu_hint_dist = (8*4) - (2*4);

/* Determines whether we run variable tracking in machine dependent
   reorganization.  */
static int spu_flag_var_tracking;

enum spu_immediate {
  SPU_NONE,
  SPU_IL,
  SPU_ILA,
  SPU_ILH,
  SPU_ILHU,
  SPU_ORI,
  SPU_ORHI,
  SPU_ORBI,
  SPU_IOHL
};
enum immediate_class
{
  IC_POOL,			/* constant pool */
  IC_IL1,			/* one il* instruction */
  IC_IL2,			/* both ilhu and iohl instructions */
  IC_IL1s,			/* one il* instruction */
  IC_IL2s,			/* both ilhu and iohl instructions */
  IC_FSMBI,			/* the fsmbi instruction */
  IC_CPAT,			/* one of the c*d instructions */
  IC_FSMBI2			/* fsmbi plus 1 other instruction */
};

static enum spu_immediate which_immediate_load (HOST_WIDE_INT val);
static enum spu_immediate which_logical_immediate (HOST_WIDE_INT val);
static int cpat_info(unsigned char *arr, int size, int *prun, int *pstart);
static enum immediate_class classify_immediate (rtx op,
						enum machine_mode mode);

static enum machine_mode spu_unwind_word_mode (void);

static enum machine_mode
spu_libgcc_cmp_return_mode (void);

static enum machine_mode
spu_libgcc_shift_count_mode (void);


/*  TARGET overrides.  */

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS spu_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN spu_expand_builtin

#undef TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE spu_unwind_word_mode

/* The .8byte directive doesn't seem to work well for a 32 bit
   architecture. */
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP NULL

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS spu_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_bool_0

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE spu_sched_issue_rate

#undef TARGET_SCHED_INIT_GLOBAL
#define TARGET_SCHED_INIT_GLOBAL spu_sched_init_global

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT spu_sched_init

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE spu_sched_variable_issue

#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER spu_sched_reorder

#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 spu_sched_reorder

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST spu_sched_adjust_cost

const struct attribute_spec spu_attribute_table[];
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE spu_attribute_table

#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER spu_assemble_integer

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P	spu_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P	spu_vector_mode_supported_p

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL spu_function_ok_for_sibcall

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL spu_asm_globalize_label

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE spu_pass_by_reference

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST spu_build_builtin_va_list

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START spu_va_start

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS spu_setup_incoming_varargs

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG spu_machine_dependent_reorg

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR spu_gimplify_va_arg_expr

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT)

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS spu_init_libfuncs

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY spu_return_in_memory

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO spu_encode_section_info

#undef TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_EVEN
#define TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_EVEN spu_builtin_mul_widen_even

#undef TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_ODD
#define TARGET_VECTORIZE_BUILTIN_MUL_WIDEN_ODD spu_builtin_mul_widen_odd

#undef TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD
#define TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD spu_builtin_mask_for_load

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST spu_builtin_vectorization_cost

#undef TARGET_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTOR_ALIGNMENT_REACHABLE spu_vector_alignment_reachable

#undef TARGET_VECTORIZE_BUILTIN_VEC_PERM
#define TARGET_VECTORIZE_BUILTIN_VEC_PERM spu_builtin_vec_perm

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE spu_libgcc_cmp_return_mode

#undef TARGET_LIBGCC_SHIFT_COUNT_MODE
#define TARGET_LIBGCC_SHIFT_COUNT_MODE spu_libgcc_shift_count_mode

#undef TARGET_SCHED_SMS_RES_MII
#define TARGET_SCHED_SMS_RES_MII spu_sms_res_mii

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START asm_file_start

#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS spu_section_type_flags

struct gcc_target targetm = TARGET_INITIALIZER;

void
spu_optimization_options (int level ATTRIBUTE_UNUSED, int size ATTRIBUTE_UNUSED)
{
  /* Override some of the default param values.  With so many registers
     larger values are better for these params.  */
  MAX_PENDING_LIST_LENGTH = 128;

  /* With so many registers this is better on by default. */
  flag_rename_registers = 1;
}

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   OVERRIDE_OPTIONS to take account of this. This macro, if defined, is
   executed once just after all the command options have been parsed.  */
void
spu_override_options (void)
{
  /* Small loops will be unpeeled at -O3.  For SPU it is more important
     to keep code small by default.  */
  if (!flag_unroll_loops && !flag_peel_loops
      && !PARAM_SET_P (PARAM_MAX_COMPLETELY_PEEL_TIMES))
    PARAM_VALUE (PARAM_MAX_COMPLETELY_PEEL_TIMES) = 1;

  flag_omit_frame_pointer = 1;

  /* Functions must be 8 byte aligned so we correctly handle dual issue */
  if (align_functions < 8)
    align_functions = 8;

  spu_hint_dist = 8*4 - spu_max_nops*4;
  if (spu_hint_dist < 0) 
    spu_hint_dist = 0;

  if (spu_fixed_range_string)
    fix_range (spu_fixed_range_string);

  /* Determine processor architectural level.  */
  if (spu_arch_string)
    {
      if (strcmp (&spu_arch_string[0], "cell") == 0)
        spu_arch = PROCESSOR_CELL;
      else if (strcmp (&spu_arch_string[0], "celledp") == 0)
        spu_arch = PROCESSOR_CELLEDP;
      else
        error ("Unknown architecture '%s'", &spu_arch_string[0]);
    }

  /* Determine processor to tune for.  */
  if (spu_tune_string)
    {
      if (strcmp (&spu_tune_string[0], "cell") == 0)
        spu_tune = PROCESSOR_CELL;
      else if (strcmp (&spu_tune_string[0], "celledp") == 0)
        spu_tune = PROCESSOR_CELLEDP;
      else
        error ("Unknown architecture '%s'", &spu_tune_string[0]);
    }

  /* Change defaults according to the processor architecture.  */
  if (spu_arch == PROCESSOR_CELLEDP)
    {
      /* If no command line option has been otherwise specified, change
	 the default to -mno-safe-hints on celledp -- only the original
	 Cell/B.E. processors require this workaround.  */
      if (!(target_flags_explicit & MASK_SAFE_HINTS))
	target_flags &= ~MASK_SAFE_HINTS;
    }

  REAL_MODE_FORMAT (SFmode) = &spu_single_format;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */

/*  Table of machine attributes.  */
const struct attribute_spec spu_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "naked",          0, 0, true,  false, false, spu_handle_fndecl_attribute },
  { "spu_vector",     0, 0, false, true,  false, spu_handle_vector_attribute },
  { NULL,             0, 0, false, false, false, NULL }
};

/* True if MODE is valid for the target.  By "valid", we mean able to
   be manipulated in non-trivial ways.  In particular, this means all
   the arithmetic is supported.  */
static bool
spu_scalar_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case QImode:
    case HImode:
    case SImode:
    case SFmode:
    case DImode:
    case TImode:
    case DFmode:
      return true;

    default:
      return false;
    }
}

/* Similarly for vector modes.  "Supported" here is less strict.  At
   least some operations are supported; need to check optabs or builtins
   for further details.  */
static bool
spu_vector_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case V16QImode:
    case V8HImode:
    case V4SImode:
    case V2DImode:
    case V4SFmode:
    case V2DFmode:
      return true;

    default:
      return false;
    }
}

/* GCC assumes that in a paradoxical SUBREG the inner mode occupies the
   least significant bytes of the outer mode.  This function returns
   TRUE for the SUBREG's where this is correct.  */
int
valid_subreg (rtx op)
{
  enum machine_mode om = GET_MODE (op);
  enum machine_mode im = GET_MODE (SUBREG_REG (op));
  return om != VOIDmode && im != VOIDmode
    && (GET_MODE_SIZE (im) == GET_MODE_SIZE (om)
	|| (GET_MODE_SIZE (im) <= 4 && GET_MODE_SIZE (om) <= 4)
	|| (GET_MODE_SIZE (im) >= 16 && GET_MODE_SIZE (om) >= 16));
}

/* When insv and ext[sz]v ar passed a TI SUBREG, we want to strip it off
   and adjust the start offset.  */
static rtx
adjust_operand (rtx op, HOST_WIDE_INT * start)
{
  enum machine_mode mode;
  int op_size;
  /* Strip any paradoxical SUBREG.  */
  if (GET_CODE (op) == SUBREG
      && (GET_MODE_BITSIZE (GET_MODE (op))
	  > GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op)))))
    {
      if (start)
	*start -=
	  GET_MODE_BITSIZE (GET_MODE (op)) -
	  GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op)));
      op = SUBREG_REG (op);
    }
  /* If it is smaller than SI, assure a SUBREG */
  op_size = GET_MODE_BITSIZE (GET_MODE (op));
  if (op_size < 32)
    {
      if (start)
	*start += 32 - op_size;
      op_size = 32;
    }
  /* If it is not a MODE_INT (and/or it is smaller than SI) add a SUBREG. */
  mode = mode_for_size (op_size, MODE_INT, 0);
  if (mode != GET_MODE (op))
    op = gen_rtx_SUBREG (mode, op, 0);
  return op;
}

void
spu_expand_extv (rtx ops[], int unsignedp)
{
  rtx dst = ops[0], src = ops[1];
  HOST_WIDE_INT width = INTVAL (ops[2]);
  HOST_WIDE_INT start = INTVAL (ops[3]);
  HOST_WIDE_INT align_mask;
  rtx s0, s1, mask, r0;

  gcc_assert (REG_P (dst) && GET_MODE (dst) == TImode);

  if (MEM_P (src))
    {
      /* First, determine if we need 1 TImode load or 2.  We need only 1
         if the bits being extracted do not cross the alignment boundary
         as determined by the MEM and its address. */

      align_mask = -MEM_ALIGN (src);
      if ((start & align_mask) == ((start + width - 1) & align_mask))
	{
	  /* Alignment is sufficient for 1 load. */
	  s0 = gen_reg_rtx (TImode);
	  r0 = spu_expand_load (s0, 0, src, start / 8);
	  start &= 7;
	  if (r0)
	    emit_insn (gen_rotqby_ti (s0, s0, r0));
	}
      else
	{
	  /* Need 2 loads. */
	  s0 = gen_reg_rtx (TImode);
	  s1 = gen_reg_rtx (TImode);
	  r0 = spu_expand_load (s0, s1, src, start / 8);
	  start &= 7;

	  gcc_assert (start + width <= 128);
	  if (r0)
	    {
	      rtx r1 = gen_reg_rtx (SImode);
	      mask = gen_reg_rtx (TImode);
	      emit_move_insn (mask, GEN_INT (-1));
	      emit_insn (gen_rotqby_ti (s0, s0, r0));
	      emit_insn (gen_rotqby_ti (s1, s1, r0));
	      if (GET_CODE (r0) == CONST_INT)
		r1 = GEN_INT (INTVAL (r0) & 15);
	      else
		emit_insn (gen_andsi3 (r1, r0, GEN_INT (15)));
	      emit_insn (gen_shlqby_ti (mask, mask, r1));
	      emit_insn (gen_selb (s0, s1, s0, mask));
	    }
	}

    }
  else if (GET_CODE (src) == SUBREG)
    {
      rtx r = SUBREG_REG (src);
      gcc_assert (REG_P (r) && SCALAR_INT_MODE_P (GET_MODE (r)));
      s0 = gen_reg_rtx (TImode);
      if (GET_MODE_SIZE (GET_MODE (r)) < GET_MODE_SIZE (TImode))
	emit_insn (gen_rtx_SET (VOIDmode, s0, gen_rtx_ZERO_EXTEND (TImode, r)));
      else
	emit_move_insn (s0, src);
    }
  else
    {
      gcc_assert (REG_P (src) && GET_MODE (src) == TImode);
      s0 = gen_reg_rtx (TImode);
      emit_move_insn (s0, src);
    }

  /* Now s0 is TImode and contains the bits to extract at start. */

  if (start)
    emit_insn (gen_rotlti3 (s0, s0, GEN_INT (start)));

  if (128 - width)
    {
      tree c = build_int_cst (NULL_TREE, 128 - width);
      s0 = expand_shift (RSHIFT_EXPR, TImode, s0, c, s0, unsignedp);
    }

  emit_move_insn (dst, s0);
}

void
spu_expand_insv (rtx ops[])
{
  HOST_WIDE_INT width = INTVAL (ops[1]);
  HOST_WIDE_INT start = INTVAL (ops[2]);
  HOST_WIDE_INT maskbits;
  enum machine_mode dst_mode, src_mode;
  rtx dst = ops[0], src = ops[3];
  int dst_size, src_size;
  rtx mask;
  rtx shift_reg;
  int shift;


  if (GET_CODE (ops[0]) == MEM)
    dst = gen_reg_rtx (TImode);
  else
    dst = adjust_operand (dst, &start);
  dst_mode = GET_MODE (dst);
  dst_size = GET_MODE_BITSIZE (GET_MODE (dst));

  if (CONSTANT_P (src))
    {
      enum machine_mode m =
	(width <= 32 ? SImode : width <= 64 ? DImode : TImode);
      src = force_reg (m, convert_to_mode (m, src, 0));
    }
  src = adjust_operand (src, 0);
  src_mode = GET_MODE (src);
  src_size = GET_MODE_BITSIZE (GET_MODE (src));

  mask = gen_reg_rtx (dst_mode);
  shift_reg = gen_reg_rtx (dst_mode);
  shift = dst_size - start - width;

  /* It's not safe to use subreg here because the compiler assumes
     that the SUBREG_REG is right justified in the SUBREG. */
  convert_move (shift_reg, src, 1);

  if (shift > 0)
    {
      switch (dst_mode)
	{
	case SImode:
	  emit_insn (gen_ashlsi3 (shift_reg, shift_reg, GEN_INT (shift)));
	  break;
	case DImode:
	  emit_insn (gen_ashldi3 (shift_reg, shift_reg, GEN_INT (shift)));
	  break;
	case TImode:
	  emit_insn (gen_ashlti3 (shift_reg, shift_reg, GEN_INT (shift)));
	  break;
	default:
	  abort ();
	}
    }
  else if (shift < 0)
    abort ();

  switch (dst_size)
    {
    case 32:
      maskbits = (-1ll << (32 - width - start));
      if (start)
	maskbits += (1ll << (32 - start));
      emit_move_insn (mask, GEN_INT (maskbits));
      break;
    case 64:
      maskbits = (-1ll << (64 - width - start));
      if (start)
	maskbits += (1ll << (64 - start));
      emit_move_insn (mask, GEN_INT (maskbits));
      break;
    case 128:
      {
	unsigned char arr[16];
	int i = start / 8;
	memset (arr, 0, sizeof (arr));
	arr[i] = 0xff >> (start & 7);
	for (i++; i <= (start + width - 1) / 8; i++)
	  arr[i] = 0xff;
	arr[i - 1] &= 0xff << (7 - ((start + width - 1) & 7));
	emit_move_insn (mask, array_to_constant (TImode, arr));
      }
      break;
    default:
      abort ();
    }
  if (GET_CODE (ops[0]) == MEM)
    {
      rtx low = gen_reg_rtx (SImode);
      rtx rotl = gen_reg_rtx (SImode);
      rtx mask0 = gen_reg_rtx (TImode);
      rtx addr;
      rtx addr0;
      rtx addr1;
      rtx mem;

      addr = force_reg (Pmode, XEXP (ops[0], 0));
      addr0 = gen_rtx_AND (Pmode, addr, GEN_INT (-16));
      emit_insn (gen_andsi3 (low, addr, GEN_INT (15)));
      emit_insn (gen_negsi2 (rotl, low));
      emit_insn (gen_rotqby_ti (shift_reg, shift_reg, rotl));
      emit_insn (gen_rotqmby_ti (mask0, mask, rotl));
      mem = change_address (ops[0], TImode, addr0);
      set_mem_alias_set (mem, 0);
      emit_move_insn (dst, mem);
      emit_insn (gen_selb (dst, dst, shift_reg, mask0));
      if (start + width > MEM_ALIGN (ops[0]))
	{
	  rtx shl = gen_reg_rtx (SImode);
	  rtx mask1 = gen_reg_rtx (TImode);
	  rtx dst1 = gen_reg_rtx (TImode);
	  rtx mem1;
	  addr1 = plus_constant (addr, 16);
	  addr1 = gen_rtx_AND (Pmode, addr1, GEN_INT (-16));
	  emit_insn (gen_subsi3 (shl, GEN_INT (16), low));
	  emit_insn (gen_shlqby_ti (mask1, mask, shl));
	  mem1 = change_address (ops[0], TImode, addr1);
	  set_mem_alias_set (mem1, 0);
	  emit_move_insn (dst1, mem1);
	  emit_insn (gen_selb (dst1, dst1, shift_reg, mask1));
	  emit_move_insn (mem1, dst1);
	}
      emit_move_insn (mem, dst);
    }
  else
    emit_insn (gen_selb (dst, copy_rtx (dst), shift_reg, mask));
}


int
spu_expand_block_move (rtx ops[])
{
  HOST_WIDE_INT bytes, align, offset;
  rtx src, dst, sreg, dreg, target;
  int i;
  if (GET_CODE (ops[2]) != CONST_INT
      || GET_CODE (ops[3]) != CONST_INT
      || INTVAL (ops[2]) > (HOST_WIDE_INT) (MOVE_RATIO (optimize_insn_for_speed_p ()) * 8))
    return 0;

  bytes = INTVAL (ops[2]);
  align = INTVAL (ops[3]);

  if (bytes <= 0)
    return 1;

  dst = ops[0];
  src = ops[1];

  if (align == 16)
    {
      for (offset = 0; offset + 16 <= bytes; offset += 16)
	{
	  dst = adjust_address (ops[0], V16QImode, offset);
	  src = adjust_address (ops[1], V16QImode, offset);
	  emit_move_insn (dst, src);
	}
      if (offset < bytes)
	{
	  rtx mask;
	  unsigned char arr[16] = { 0 };
	  for (i = 0; i < bytes - offset; i++)
	    arr[i] = 0xff;
	  dst = adjust_address (ops[0], V16QImode, offset);
	  src = adjust_address (ops[1], V16QImode, offset);
	  mask = gen_reg_rtx (V16QImode);
	  sreg = gen_reg_rtx (V16QImode);
	  dreg = gen_reg_rtx (V16QImode);
	  target = gen_reg_rtx (V16QImode);
	  emit_move_insn (mask, array_to_constant (V16QImode, arr));
	  emit_move_insn (dreg, dst);
	  emit_move_insn (sreg, src);
	  emit_insn (gen_selb (target, dreg, sreg, mask));
	  emit_move_insn (dst, target);
	}
      return 1;
    }
  return 0;
}

enum spu_comp_code
{ SPU_EQ, SPU_GT, SPU_GTU };

int spu_comp_icode[12][3] = {
 {CODE_FOR_ceq_qi, CODE_FOR_cgt_qi, CODE_FOR_clgt_qi},
 {CODE_FOR_ceq_hi, CODE_FOR_cgt_hi, CODE_FOR_clgt_hi},
 {CODE_FOR_ceq_si, CODE_FOR_cgt_si, CODE_FOR_clgt_si},
 {CODE_FOR_ceq_di, CODE_FOR_cgt_di, CODE_FOR_clgt_di},
 {CODE_FOR_ceq_ti, CODE_FOR_cgt_ti, CODE_FOR_clgt_ti},
 {CODE_FOR_ceq_sf, CODE_FOR_cgt_sf, 0},
 {CODE_FOR_ceq_df, CODE_FOR_cgt_df, 0},
 {CODE_FOR_ceq_v16qi, CODE_FOR_cgt_v16qi, CODE_FOR_clgt_v16qi},
 {CODE_FOR_ceq_v8hi,  CODE_FOR_cgt_v8hi,  CODE_FOR_clgt_v8hi},
 {CODE_FOR_ceq_v4si,  CODE_FOR_cgt_v4si,  CODE_FOR_clgt_v4si},
 {CODE_FOR_ceq_v4sf,  CODE_FOR_cgt_v4sf, 0},
 {CODE_FOR_ceq_v2df,  CODE_FOR_cgt_v2df, 0},
};

/* Generate a compare for CODE.  Return a brand-new rtx that represents
   the result of the compare.   GCC can figure this out too if we don't
   provide all variations of compares, but GCC always wants to use
   WORD_MODE, we can generate better code in most cases if we do it
   ourselves.  */
void
spu_emit_branch_or_set (int is_set, enum rtx_code code, rtx operands[])
{
  int reverse_compare = 0;
  int reverse_test = 0;
  rtx compare_result, eq_result;
  rtx comp_rtx, eq_rtx;
  rtx target = operands[0];
  enum machine_mode comp_mode;
  enum machine_mode op_mode;
  enum spu_comp_code scode, eq_code, ior_code;
  int index;
  int eq_test = 0;

  /* When spu_compare_op1 is a CONST_INT change (X >= C) to (X > C-1),
     and so on, to keep the constant in operand 1. */
  if (GET_CODE (spu_compare_op1) == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (spu_compare_op1) - 1;
      if (trunc_int_for_mode (val, GET_MODE (spu_compare_op0)) == val)
	switch (code)
	  {
	  case GE:
	    spu_compare_op1 = GEN_INT (val);
	    code = GT;
	    break;
	  case LT:
	    spu_compare_op1 = GEN_INT (val);
	    code = LE;
	    break;
	  case GEU:
	    spu_compare_op1 = GEN_INT (val);
	    code = GTU;
	    break;
	  case LTU:
	    spu_compare_op1 = GEN_INT (val);
	    code = LEU;
	    break;
	  default:
	    break;
	  }
    }

  comp_mode = SImode;
  op_mode = GET_MODE (spu_compare_op0);

  switch (code)
    {
    case GE:
      scode = SPU_GT;
      if (HONOR_NANS (op_mode))
	{
	  reverse_compare = 0;
	  reverse_test = 0;
	  eq_test = 1;
	  eq_code = SPU_EQ;
	}
      else
	{
	  reverse_compare = 1;
	  reverse_test = 1;
	}
      break;
    case LE:
      scode = SPU_GT;
      if (HONOR_NANS (op_mode))
	{
	  reverse_compare = 1;
	  reverse_test = 0;
	  eq_test = 1;
	  eq_code = SPU_EQ;
	}
      else
	{
	  reverse_compare = 0;
	  reverse_test = 1;
	}
      break;
    case LT:
      reverse_compare = 1;
      reverse_test = 0;
      scode = SPU_GT;
      break;
    case GEU:
      reverse_compare = 1;
      reverse_test = 1;
      scode = SPU_GTU;
      break;
    case LEU:
      reverse_compare = 0;
      reverse_test = 1;
      scode = SPU_GTU;
      break;
    case LTU:
      reverse_compare = 1;
      reverse_test = 0;
      scode = SPU_GTU;
      break;
    case NE:
      reverse_compare = 0;
      reverse_test = 1;
      scode = SPU_EQ;
      break;

    case EQ:
      scode = SPU_EQ;
      break;
    case GT:
      scode = SPU_GT;
      break;
    case GTU:
      scode = SPU_GTU;
      break;
    default:
      scode = SPU_EQ;
      break;
    }

  switch (op_mode)
    {
    case QImode:
      index = 0;
      comp_mode = QImode;
      break;
    case HImode:
      index = 1;
      comp_mode = HImode;
      break;
    case SImode:
      index = 2;
      break;
    case DImode:
      index = 3;
      break;
    case TImode:
      index = 4;
      break;
    case SFmode:
      index = 5;
      break;
    case DFmode:
      index = 6;
      break;
    case V16QImode:
      index = 7;
      comp_mode = op_mode;
      break;
    case V8HImode:
      index = 8;
      comp_mode = op_mode;
      break;
    case V4SImode:
      index = 9;
      comp_mode = op_mode;
      break;
    case V4SFmode:
      index = 10;
      comp_mode = V4SImode;
      break;
    case V2DFmode:
      index = 11;
      comp_mode = V2DImode;
      break;
    case V2DImode:
    default:
      abort ();
    }

  if (GET_MODE (spu_compare_op1) == DFmode
      && (scode != SPU_GT && scode != SPU_EQ))
    abort ();

  if (is_set == 0 && spu_compare_op1 == const0_rtx
      && (GET_MODE (spu_compare_op0) == SImode
	  || GET_MODE (spu_compare_op0) == HImode) && scode == SPU_EQ)
    {
      /* Don't need to set a register with the result when we are 
         comparing against zero and branching. */
      reverse_test = !reverse_test;
      compare_result = spu_compare_op0;
    }
  else
    {
      compare_result = gen_reg_rtx (comp_mode);

      if (reverse_compare)
	{
	  rtx t = spu_compare_op1;
	  spu_compare_op1 = spu_compare_op0;
	  spu_compare_op0 = t;
	}

      if (spu_comp_icode[index][scode] == 0)
	abort ();

      if (!(*insn_data[spu_comp_icode[index][scode]].operand[1].predicate)
	  (spu_compare_op0, op_mode))
	spu_compare_op0 = force_reg (op_mode, spu_compare_op0);
      if (!(*insn_data[spu_comp_icode[index][scode]].operand[2].predicate)
	  (spu_compare_op1, op_mode))
	spu_compare_op1 = force_reg (op_mode, spu_compare_op1);
      comp_rtx = GEN_FCN (spu_comp_icode[index][scode]) (compare_result,
							 spu_compare_op0,
							 spu_compare_op1);
      if (comp_rtx == 0)
	abort ();
      emit_insn (comp_rtx);

      if (eq_test)
        {
          eq_result = gen_reg_rtx (comp_mode);
          eq_rtx = GEN_FCN (spu_comp_icode[index][eq_code]) (eq_result,
							     spu_compare_op0,
							     spu_compare_op1);
          if (eq_rtx == 0)
	    abort ();
          emit_insn (eq_rtx);
          ior_code = ior_optab->handlers[(int)comp_mode].insn_code;
          gcc_assert (ior_code != CODE_FOR_nothing);
          emit_insn (GEN_FCN (ior_code)
		     (compare_result, compare_result, eq_result));
        }
    }

  if (is_set == 0)
    {
      rtx bcomp;
      rtx loc_ref;

      /* We don't have branch on QI compare insns, so we convert the
         QI compare result to a HI result. */
      if (comp_mode == QImode)
	{
	  rtx old_res = compare_result;
	  compare_result = gen_reg_rtx (HImode);
	  comp_mode = HImode;
	  emit_insn (gen_extendqihi2 (compare_result, old_res));
	}

      if (reverse_test)
	bcomp = gen_rtx_EQ (comp_mode, compare_result, const0_rtx);
      else
	bcomp = gen_rtx_NE (comp_mode, compare_result, const0_rtx);

      loc_ref = gen_rtx_LABEL_REF (VOIDmode, target);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
				   gen_rtx_IF_THEN_ELSE (VOIDmode, bcomp,
							 loc_ref, pc_rtx)));
    }
  else if (is_set == 2)
    {
      int compare_size = GET_MODE_BITSIZE (comp_mode);
      int target_size = GET_MODE_BITSIZE (GET_MODE (target));
      enum machine_mode mode = mode_for_size (target_size, MODE_INT, 0);
      rtx select_mask;
      rtx op_t = operands[2];
      rtx op_f = operands[3];

      /* The result of the comparison can be SI, HI or QI mode.  Create a
         mask based on that result. */
      if (target_size > compare_size)
	{
	  select_mask = gen_reg_rtx (mode);
	  emit_insn (gen_extend_compare (select_mask, compare_result));
	}
      else if (target_size < compare_size)
	select_mask =
	  gen_rtx_SUBREG (mode, compare_result,
			  (compare_size - target_size) / BITS_PER_UNIT);
      else if (comp_mode != mode)
	select_mask = gen_rtx_SUBREG (mode, compare_result, 0);
      else
	select_mask = compare_result;

      if (GET_MODE (target) != GET_MODE (op_t)
	  || GET_MODE (target) != GET_MODE (op_f))
	abort ();

      if (reverse_test)
	emit_insn (gen_selb (target, op_t, op_f, select_mask));
      else
	emit_insn (gen_selb (target, op_f, op_t, select_mask));
    }
  else
    {
      if (reverse_test)
	emit_insn (gen_rtx_SET (VOIDmode, compare_result,
				gen_rtx_NOT (comp_mode, compare_result)));
      if (GET_MODE (target) == SImode && GET_MODE (compare_result) == HImode)
	emit_insn (gen_extendhisi2 (target, compare_result));
      else if (GET_MODE (target) == SImode
	       && GET_MODE (compare_result) == QImode)
	emit_insn (gen_extend_compare (target, compare_result));
      else
	emit_move_insn (target, compare_result);
    }
}

HOST_WIDE_INT
const_double_to_hwint (rtx x)
{
  HOST_WIDE_INT val;
  REAL_VALUE_TYPE rv;
  if (GET_MODE (x) == SFmode)
    {
      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, val);
    }
  else if (GET_MODE (x) == DFmode)
    {
      long l[2];
      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, l);
      val = l[0];
      val = (val << 32) | (l[1] & 0xffffffff);
    }
  else
    abort ();
  return val;
}

rtx
hwint_to_const_double (enum machine_mode mode, HOST_WIDE_INT v)
{
  long tv[2];
  REAL_VALUE_TYPE rv;
  gcc_assert (mode == SFmode || mode == DFmode);

  if (mode == SFmode)
    tv[0] = (v << 32) >> 32;
  else if (mode == DFmode)
    {
      tv[1] = (v << 32) >> 32;
      tv[0] = v >> 32;
    }
  real_from_target (&rv, tv, mode);
  return CONST_DOUBLE_FROM_REAL_VALUE (rv, mode);
}

void
print_operand_address (FILE * file, register rtx addr)
{
  rtx reg;
  rtx offset;

  if (GET_CODE (addr) == AND
      && GET_CODE (XEXP (addr, 1)) == CONST_INT
      && INTVAL (XEXP (addr, 1)) == -16)
    addr = XEXP (addr, 0);

  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "0(%s)", reg_names[REGNO (addr)]);
      break;

    case PLUS:
      reg = XEXP (addr, 0);
      offset = XEXP (addr, 1);
      if (GET_CODE (offset) == REG)
	{
	  fprintf (file, "%s,%s", reg_names[REGNO (reg)],
		   reg_names[REGNO (offset)]);
	}
      else if (GET_CODE (offset) == CONST_INT)
	{
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC "(%s)",
		   INTVAL (offset), reg_names[REGNO (reg)]);
	}
      else
	abort ();
      break;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
      output_addr_const (file, addr);
      break;

    default:
      debug_rtx (addr);
      abort ();
    }
}

void
print_operand (FILE * file, rtx x, int code)
{
  enum machine_mode mode = GET_MODE (x);
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int xcode = GET_CODE (x);
  int i, info;
  if (GET_MODE (x) == VOIDmode)
    switch (code)
      {
      case 'L':			/* 128 bits, signed */
      case 'm':			/* 128 bits, signed */
      case 'T':			/* 128 bits, signed */
      case 't':			/* 128 bits, signed */
	mode = TImode;
	break;
      case 'K':			/* 64 bits, signed */
      case 'k':			/* 64 bits, signed */
      case 'D':			/* 64 bits, signed */
      case 'd':			/* 64 bits, signed */
	mode = DImode;
	break;
      case 'J':			/* 32 bits, signed */
      case 'j':			/* 32 bits, signed */
      case 's':			/* 32 bits, signed */
      case 'S':			/* 32 bits, signed */
	mode = SImode;
	break;
      }
  switch (code)
    {

    case 'j':			/* 32 bits, signed */
    case 'k':			/* 64 bits, signed */
    case 'm':			/* 128 bits, signed */
      if (xcode == CONST_INT
	  || xcode == CONST_DOUBLE || xcode == CONST_VECTOR)
	{
	  gcc_assert (logical_immediate_p (x, mode));
	  constant_to_array (mode, x, arr);
	  val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
	  val = trunc_int_for_mode (val, SImode);
	  switch (which_logical_immediate (val))
	  {
	  case SPU_ORI:
	    break;
	  case SPU_ORHI:
	    fprintf (file, "h");
	    break;
	  case SPU_ORBI:
	    fprintf (file, "b");
	    break;
	  default:
	    gcc_unreachable();
	  }
	}
      else
	gcc_unreachable();
      return;

    case 'J':			/* 32 bits, signed */
    case 'K':			/* 64 bits, signed */
    case 'L':			/* 128 bits, signed */
      if (xcode == CONST_INT
	  || xcode == CONST_DOUBLE || xcode == CONST_VECTOR)
	{
	  gcc_assert (logical_immediate_p (x, mode)
		      || iohl_immediate_p (x, mode));
	  constant_to_array (mode, x, arr);
	  val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
	  val = trunc_int_for_mode (val, SImode);
	  switch (which_logical_immediate (val))
	  {
	  case SPU_ORI:
	  case SPU_IOHL:
	    break;
	  case SPU_ORHI:
	    val = trunc_int_for_mode (val, HImode);
	    break;
	  case SPU_ORBI:
	    val = trunc_int_for_mode (val, QImode);
	    break;
	  default:
	    gcc_unreachable();
	  }
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
	}
      else
	gcc_unreachable();
      return;

    case 't':			/* 128 bits, signed */
    case 'd':			/* 64 bits, signed */
    case 's':			/* 32 bits, signed */
      if (CONSTANT_P (x))
	{
	  enum immediate_class c = classify_immediate (x, mode);
	  switch (c)
	    {
	    case IC_IL1:
	      constant_to_array (mode, x, arr);
	      val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
	      val = trunc_int_for_mode (val, SImode);
	      switch (which_immediate_load (val))
		{
		case SPU_IL:
		  break;
		case SPU_ILA:
		  fprintf (file, "a");
		  break;
		case SPU_ILH:
		  fprintf (file, "h");
		  break;
		case SPU_ILHU:
		  fprintf (file, "hu");
		  break;
		default:
		  gcc_unreachable ();
		}
	      break;
	    case IC_CPAT:
	      constant_to_array (mode, x, arr);
	      cpat_info (arr, GET_MODE_SIZE (mode), &info, 0);
	      if (info == 1)
		fprintf (file, "b");
	      else if (info == 2)
		fprintf (file, "h");
	      else if (info == 4)
		fprintf (file, "w");
	      else if (info == 8)
		fprintf (file, "d");
	      break;
	    case IC_IL1s:
	      if (xcode == CONST_VECTOR)
		{
		  x = CONST_VECTOR_ELT (x, 0);
		  xcode = GET_CODE (x);
		}
	      if (xcode == SYMBOL_REF || xcode == LABEL_REF || xcode == CONST)
		fprintf (file, "a");
	      else if (xcode == HIGH)
		fprintf (file, "hu");
	      break;
	    case IC_FSMBI:
	    case IC_FSMBI2:
	    case IC_IL2:
	    case IC_IL2s:
	    case IC_POOL:
	      abort ();
	    }
	}
      else
	gcc_unreachable ();
      return;

    case 'T':			/* 128 bits, signed */
    case 'D':			/* 64 bits, signed */
    case 'S':			/* 32 bits, signed */
      if (CONSTANT_P (x))
	{
	  enum immediate_class c = classify_immediate (x, mode);
	  switch (c)
	    {
	    case IC_IL1:
	      constant_to_array (mode, x, arr);
	      val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
	      val = trunc_int_for_mode (val, SImode);
	      switch (which_immediate_load (val))
		{
		case SPU_IL:
		case SPU_ILA:
		  break;
		case SPU_ILH:
		case SPU_ILHU:
		  val = trunc_int_for_mode (((arr[0] << 8) | arr[1]), HImode);
		  break;
		default:
		  gcc_unreachable ();
		}
	      fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
	      break;
	    case IC_FSMBI:
	      constant_to_array (mode, x, arr);
	      val = 0;
	      for (i = 0; i < 16; i++)
		{
		  val <<= 1;
		  val |= arr[i] & 1;
		}
	      print_operand (file, GEN_INT (val), 0);
	      break;
	    case IC_CPAT:
	      constant_to_array (mode, x, arr);
	      cpat_info (arr, GET_MODE_SIZE (mode), 0, &info);
	      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT)info);
	      break;
	    case IC_IL1s:
	      if (xcode == HIGH)
		x = XEXP (x, 0);
	      if (GET_CODE (x) == CONST_VECTOR)
		x = CONST_VECTOR_ELT (x, 0);
	      output_addr_const (file, x);
	      if (xcode == HIGH)
		fprintf (file, "@h");
	      break;
	    case IC_IL2:
	    case IC_IL2s:
	    case IC_FSMBI2:
	    case IC_POOL:
	      abort ();
	    }
	}
      else
	gcc_unreachable ();
      return;

    case 'C':
      if (xcode == CONST_INT)
	{
	  /* Only 4 least significant bits are relevant for generate
	     control word instructions. */
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 15);
	  return;
	}
      break;

    case 'M':			/* print code for c*d */
      if (GET_CODE (x) == CONST_INT)
	switch (INTVAL (x))
	  {
	  case 1:
	    fprintf (file, "b");
	    break;
	  case 2:
	    fprintf (file, "h");
	    break;
	  case 4:
	    fprintf (file, "w");
	    break;
	  case 8:
	    fprintf (file, "d");
	    break;
	  default:
	    gcc_unreachable();
	  }
      else
	gcc_unreachable();
      return;

    case 'N':			/* Negate the operand */
      if (xcode == CONST_INT)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, -INTVAL (x));
      else if (xcode == CONST_VECTOR)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 -INTVAL (CONST_VECTOR_ELT (x, 0)));
      return;

    case 'I':			/* enable/disable interrupts */
      if (xcode == CONST_INT)
	fprintf (file, "%s",  INTVAL (x) == 0 ? "d" : "e");
      return;

    case 'b':			/* branch modifiers */
      if (xcode == REG)
	fprintf (file, "%s", GET_MODE (x) == HImode ? "h" : "");
      else if (COMPARISON_P (x))
	fprintf (file, "%s", xcode == NE ? "n" : "");
      return;

    case 'i':			/* indirect call */
      if (xcode == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == REG)
	    /* Used in indirect function calls. */
	    fprintf (file, "%s", reg_names[REGNO (XEXP (x, 0))]);
	  else
	    output_address (XEXP (x, 0));
	}
      return;

    case 'p':			/* load/store */
      if (xcode == MEM)
	{
	  x = XEXP (x, 0);
	  xcode = GET_CODE (x);
	}
      if (xcode == AND)
	{
	  x = XEXP (x, 0);
	  xcode = GET_CODE (x);
	}
      if (xcode == REG)
	fprintf (file, "d");
      else if (xcode == CONST_INT)
	fprintf (file, "a");
      else if (xcode == CONST || xcode == SYMBOL_REF || xcode == LABEL_REF)
	fprintf (file, "r");
      else if (xcode == PLUS || xcode == LO_SUM)
	{
	  if (GET_CODE (XEXP (x, 1)) == REG)
	    fprintf (file, "x");
	  else
	    fprintf (file, "d");
	}
      return;

    case 'e':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val &= 0x7;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'f':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val &= 0x1f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'g':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val &= 0x3f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'h':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val = (val >> 3) & 0x1f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'E':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val = -val;
      val &= 0x7;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'F':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val = -val;
      val &= 0x1f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'G':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val = -val;
      val &= 0x3f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'H':
      val = xcode == CONST_INT ? INTVAL (x) : INTVAL (CONST_VECTOR_ELT (x, 0));
      val = -(val & -8ll);
      val = (val >> 3) & 0x1f;
      output_addr_const (file, GEN_INT (val));
      return;

    case 'v':
    case 'w':
      constant_to_array (mode, x, arr);
      val = (((arr[0] << 1) + (arr[1] >> 7)) & 0xff) - 127;
      output_addr_const (file, GEN_INT (code == 'w' ? -val : val));
      return;

    case 0:
      if (xcode == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (xcode == MEM)
	output_address (XEXP (x, 0));
      else if (xcode == CONST_VECTOR)
	print_operand (file, CONST_VECTOR_ELT (x, 0), 0);
      else
	output_addr_const (file, x);
      return;

      /* unused letters
	              o qr  u   yz
	AB            OPQR  UVWXYZ */
    default:
      output_operand_lossage ("invalid %%xn code");
    }
  gcc_unreachable ();
}

extern char call_used_regs[];

/* For PIC mode we've reserved PIC_OFFSET_TABLE_REGNUM, which is a
   caller saved register.  For leaf functions it is more efficient to
   use a volatile register because we won't need to save and restore the
   pic register.  This routine is only valid after register allocation
   is completed, so we can pick an unused register.  */
static rtx
get_pic_reg (void)
{
  rtx pic_reg = pic_offset_table_rtx;
  if (!reload_completed && !reload_in_progress)
    abort ();
  if (current_function_is_leaf && !df_regs_ever_live_p (LAST_ARG_REGNUM))
    pic_reg = gen_rtx_REG (SImode, LAST_ARG_REGNUM);
  return pic_reg;
}

/* Split constant addresses to handle cases that are too large. 
   Add in the pic register when in PIC mode.
   Split immediates that require more than 1 instruction. */
int
spu_split_immediate (rtx * ops)
{
  enum machine_mode mode = GET_MODE (ops[0]);
  enum immediate_class c = classify_immediate (ops[1], mode);

  switch (c)
    {
    case IC_IL2:
      {
	unsigned char arrhi[16];
	unsigned char arrlo[16];
	rtx to, temp, hi, lo;
	int i;
	enum machine_mode imode = mode;
	/* We need to do reals as ints because the constant used in the
	   IOR might not be a legitimate real constant. */
	imode = int_mode_for_mode (mode);
	constant_to_array (mode, ops[1], arrhi);
	if (imode != mode)
	  to = simplify_gen_subreg (imode, ops[0], mode, 0);
	else
	  to = ops[0];
	temp = !can_create_pseudo_p () ? to : gen_reg_rtx (imode);
	for (i = 0; i < 16; i += 4)
	  {
	    arrlo[i + 2] = arrhi[i + 2];
	    arrlo[i + 3] = arrhi[i + 3];
	    arrlo[i + 0] = arrlo[i + 1] = 0;
	    arrhi[i + 2] = arrhi[i + 3] = 0;
	  }
	hi = array_to_constant (imode, arrhi);
	lo = array_to_constant (imode, arrlo);
	emit_move_insn (temp, hi);
	emit_insn (gen_rtx_SET
		   (VOIDmode, to, gen_rtx_IOR (imode, temp, lo)));
	return 1;
      }
    case IC_FSMBI2:
      {
	unsigned char arr_fsmbi[16];
	unsigned char arr_andbi[16];
	rtx to, reg_fsmbi, reg_and;
	int i;
	enum machine_mode imode = mode;
	/* We need to do reals as ints because the constant used in the
	 * AND might not be a legitimate real constant. */
	imode = int_mode_for_mode (mode);
	constant_to_array (mode, ops[1], arr_fsmbi);
	if (imode != mode)
	  to = simplify_gen_subreg(imode, ops[0], GET_MODE (ops[0]), 0);
	else
	  to = ops[0];
	for (i = 0; i < 16; i++)
	  if (arr_fsmbi[i] != 0)
	    {
	      arr_andbi[0] = arr_fsmbi[i];
	      arr_fsmbi[i] = 0xff;
	    }
	for (i = 1; i < 16; i++)
	  arr_andbi[i] = arr_andbi[0];
	reg_fsmbi = array_to_constant (imode, arr_fsmbi);
	reg_and = array_to_constant (imode, arr_andbi);
	emit_move_insn (to, reg_fsmbi);
	emit_insn (gen_rtx_SET
		   (VOIDmode, to, gen_rtx_AND (imode, to, reg_and)));
	return 1;
      }
    case IC_POOL:
      if (reload_in_progress || reload_completed)
	{
	  rtx mem = force_const_mem (mode, ops[1]);
	  if (TARGET_LARGE_MEM)
	    {
	      rtx addr = gen_rtx_REG (Pmode, REGNO (ops[0]));
	      emit_move_insn (addr, XEXP (mem, 0));
	      mem = replace_equiv_address (mem, addr);
	    }
	  emit_move_insn (ops[0], mem);
	  return 1;
	}
      break;
    case IC_IL1s:
    case IC_IL2s:
      if (reload_completed && GET_CODE (ops[1]) != HIGH)
	{
	  if (c == IC_IL2s)
	    {
	      emit_move_insn (ops[0], gen_rtx_HIGH (mode, ops[1]));
	      emit_move_insn (ops[0], gen_rtx_LO_SUM (mode, ops[0], ops[1]));
	    }
	  else if (flag_pic)
	    emit_insn (gen_pic (ops[0], ops[1]));
	  if (flag_pic)
	    {
	      rtx pic_reg = get_pic_reg ();
	      emit_insn (gen_addsi3 (ops[0], ops[0], pic_reg));
	      crtl->uses_pic_offset_table = 1;
	    }
	  return flag_pic || c == IC_IL2s;
	}
      break;
    case IC_IL1:
    case IC_FSMBI:
    case IC_CPAT:
      break;
    }
  return 0;
}

/* SAVING is TRUE when we are generating the actual load and store
   instructions for REGNO.  When determining the size of the stack
   needed for saving register we must allocate enough space for the
   worst case, because we don't always have the information early enough
   to not allocate it.  But we can at least eliminate the actual loads
   and stores during the prologue/epilogue.  */
static int
need_to_save_reg (int regno, int saving)
{
  if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
    return 1;
  if (flag_pic
      && regno == PIC_OFFSET_TABLE_REGNUM
      && (!saving || crtl->uses_pic_offset_table)
      && (!saving
	  || !current_function_is_leaf || df_regs_ever_live_p (LAST_ARG_REGNUM)))
    return 1;
  return 0;
}

/* This function is only correct starting with local register
   allocation */
int
spu_saved_regs_size (void)
{
  int reg_save_size = 0;
  int regno;

  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; --regno)
    if (need_to_save_reg (regno, 0))
      reg_save_size += 0x10;
  return reg_save_size;
}

static rtx
frame_emit_store (int regno, rtx addr, HOST_WIDE_INT offset)
{
  rtx reg = gen_rtx_REG (V4SImode, regno);
  rtx mem =
    gen_frame_mem (V4SImode, gen_rtx_PLUS (Pmode, addr, GEN_INT (offset)));
  return emit_insn (gen_movv4si (mem, reg));
}

static rtx
frame_emit_load (int regno, rtx addr, HOST_WIDE_INT offset)
{
  rtx reg = gen_rtx_REG (V4SImode, regno);
  rtx mem =
    gen_frame_mem (V4SImode, gen_rtx_PLUS (Pmode, addr, GEN_INT (offset)));
  return emit_insn (gen_movv4si (reg, mem));
}

/* This happens after reload, so we need to expand it.  */
static rtx
frame_emit_add_imm (rtx dst, rtx src, HOST_WIDE_INT imm, rtx scratch)
{
  rtx insn;
  if (satisfies_constraint_K (GEN_INT (imm)))
    {
      insn = emit_insn (gen_addsi3 (dst, src, GEN_INT (imm)));
    }
  else
    {
      emit_insn (gen_movsi (scratch, gen_int_mode (imm, SImode)));
      insn = emit_insn (gen_addsi3 (dst, src, scratch));
      if (REGNO (src) == REGNO (scratch))
	abort ();
    }
  return insn;
}

/* Return nonzero if this function is known to have a null epilogue.  */

int
direct_return (void)
{
  if (reload_completed)
    {
      if (cfun->static_chain_decl == 0
	  && (spu_saved_regs_size ()
	      + get_frame_size ()
	      + crtl->outgoing_args_size
	      + crtl->args.pretend_args_size == 0)
	  && current_function_is_leaf)
	return 1;
    }
  return 0;
}

/*
   The stack frame looks like this:
         +-------------+
         |  incoming   | 
         |    args     | 
   AP -> +-------------+
         | $lr save    |
         +-------------+
 prev SP | back chain  | 
         +-------------+
         |  var args   | 
         |  reg save   | crtl->args.pretend_args_size bytes
         +-------------+
         |    ...      | 
         | saved regs  | spu_saved_regs_size() bytes
   FP -> +-------------+
         |    ...      | 
         |   vars      | get_frame_size()  bytes
  HFP -> +-------------+
         |    ...      | 
         |  outgoing   | 
         |    args     | crtl->outgoing_args_size bytes
         +-------------+
         | $lr of next |
         |   frame     | 
         +-------------+
         | back chain  | 
   SP -> +-------------+

*/
void
spu_expand_prologue (void)
{
  HOST_WIDE_INT size = get_frame_size (), offset, regno;
  HOST_WIDE_INT total_size;
  HOST_WIDE_INT saved_regs_size;
  rtx sp_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx scratch_reg_0, scratch_reg_1;
  rtx insn, real;

  /* A NOTE_INSN_DELETED is supposed to be at the start and end of
     the "toplevel" insn chain.  */
  emit_note (NOTE_INSN_DELETED);

  if (flag_pic && optimize == 0)
    crtl->uses_pic_offset_table = 1;

  if (spu_naked_function_p (current_function_decl))
    return;

  scratch_reg_0 = gen_rtx_REG (SImode, LAST_ARG_REGNUM + 1);
  scratch_reg_1 = gen_rtx_REG (SImode, LAST_ARG_REGNUM + 2);

  saved_regs_size = spu_saved_regs_size ();
  total_size = size + saved_regs_size
    + crtl->outgoing_args_size
    + crtl->args.pretend_args_size;

  if (!current_function_is_leaf
      || cfun->calls_alloca || total_size > 0)
    total_size += STACK_POINTER_OFFSET;

  /* Save this first because code after this might use the link
     register as a scratch register. */
  if (!current_function_is_leaf)
    {
      insn = frame_emit_store (LINK_REGISTER_REGNUM, sp_reg, 16);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (total_size > 0)
    {
      offset = -crtl->args.pretend_args_size;
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	if (need_to_save_reg (regno, 1))
	  {
	    offset -= 16;
	    insn = frame_emit_store (regno, sp_reg, offset);
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }
    }

  if (flag_pic && crtl->uses_pic_offset_table)
    {
      rtx pic_reg = get_pic_reg ();
      insn = emit_insn (gen_load_pic_offset (pic_reg, scratch_reg_0));
      insn = emit_insn (gen_subsi3 (pic_reg, pic_reg, scratch_reg_0));
    }

  if (total_size > 0)
    {
      if (flag_stack_check)
	{
	  /* We compare against total_size-1 because
	     ($sp >= total_size) <=> ($sp > total_size-1) */
	  rtx scratch_v4si = gen_rtx_REG (V4SImode, REGNO (scratch_reg_0));
	  rtx sp_v4si = gen_rtx_REG (V4SImode, STACK_POINTER_REGNUM);
	  rtx size_v4si = spu_const (V4SImode, total_size - 1);
	  if (!satisfies_constraint_K (GEN_INT (total_size - 1)))
	    {
	      emit_move_insn (scratch_v4si, size_v4si);
	      size_v4si = scratch_v4si;
	    }
	  emit_insn (gen_cgt_v4si (scratch_v4si, sp_v4si, size_v4si));
	  emit_insn (gen_vec_extractv4si
		     (scratch_reg_0, scratch_v4si, GEN_INT (1)));
	  emit_insn (gen_spu_heq (scratch_reg_0, GEN_INT (0)));
	}

      /* Adjust the stack pointer, and make sure scratch_reg_0 contains
         the value of the previous $sp because we save it as the back
         chain. */
      if (total_size <= 2000)
	{
	  /* In this case we save the back chain first. */
	  insn = frame_emit_store (STACK_POINTER_REGNUM, sp_reg, -total_size);
	  insn =
	    frame_emit_add_imm (sp_reg, sp_reg, -total_size, scratch_reg_0);
	}
      else if (satisfies_constraint_K (GEN_INT (-total_size)))
	{
	  insn = emit_move_insn (scratch_reg_0, sp_reg);
	  insn =
	    emit_insn (gen_addsi3 (sp_reg, sp_reg, GEN_INT (-total_size)));
	}
      else
	{
	  insn = emit_move_insn (scratch_reg_0, sp_reg);
	  insn =
	    frame_emit_add_imm (sp_reg, sp_reg, -total_size, scratch_reg_1);
	}
      RTX_FRAME_RELATED_P (insn) = 1;
      real = gen_addsi3 (sp_reg, sp_reg, GEN_INT (-total_size));
      REG_NOTES (insn) =
	gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, real, REG_NOTES (insn));

      if (total_size > 2000)
	{
	  /* Save the back chain ptr */
	  insn = frame_emit_store (REGNO (scratch_reg_0), sp_reg, 0);
	}

      if (frame_pointer_needed)
	{
	  rtx fp_reg = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
	  HOST_WIDE_INT fp_offset = STACK_POINTER_OFFSET
	    + crtl->outgoing_args_size;
	  /* Set the new frame_pointer */
	  insn = frame_emit_add_imm (fp_reg, sp_reg, fp_offset, scratch_reg_0);
	  RTX_FRAME_RELATED_P (insn) = 1;
	  real = gen_addsi3 (fp_reg, sp_reg, GEN_INT (fp_offset));
	  REG_NOTES (insn) = 
	    gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			       real, REG_NOTES (insn));
          REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = STACK_BOUNDARY;
	}
    }

  emit_note (NOTE_INSN_DELETED);
}

void
spu_expand_epilogue (bool sibcall_p)
{
  int size = get_frame_size (), offset, regno;
  HOST_WIDE_INT saved_regs_size, total_size;
  rtx sp_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx jump, scratch_reg_0;

  /* A NOTE_INSN_DELETED is supposed to be at the start and end of
     the "toplevel" insn chain.  */
  emit_note (NOTE_INSN_DELETED);

  if (spu_naked_function_p (current_function_decl))
    return;

  scratch_reg_0 = gen_rtx_REG (SImode, LAST_ARG_REGNUM + 1);

  saved_regs_size = spu_saved_regs_size ();
  total_size = size + saved_regs_size
    + crtl->outgoing_args_size
    + crtl->args.pretend_args_size;

  if (!current_function_is_leaf
      || cfun->calls_alloca || total_size > 0)
    total_size += STACK_POINTER_OFFSET;

  if (total_size > 0)
    {
      if (cfun->calls_alloca)
	frame_emit_load (STACK_POINTER_REGNUM, sp_reg, 0);
      else
	frame_emit_add_imm (sp_reg, sp_reg, total_size, scratch_reg_0);


      if (saved_regs_size > 0)
	{
	  offset = -crtl->args.pretend_args_size;
	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	    if (need_to_save_reg (regno, 1))
	      {
		offset -= 0x10;
		frame_emit_load (regno, sp_reg, offset);
	      }
	}
    }

  if (!current_function_is_leaf)
    frame_emit_load (LINK_REGISTER_REGNUM, sp_reg, 16);

  if (!sibcall_p)
    {
      emit_use (gen_rtx_REG (SImode, LINK_REGISTER_REGNUM));
      jump = emit_jump_insn (gen__return ());
      emit_barrier_after (jump);
    }

  emit_note (NOTE_INSN_DELETED);
}

rtx
spu_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return 0;
  /* This is inefficient because it ends up copying to a save-register
     which then gets saved even though $lr has already been saved.  But
     it does generate better code for leaf functions and we don't need
     to use RETURN_ADDRESS_POINTER_REGNUM to get it working.  It's only
     used for __builtin_return_address anyway, so maybe we don't care if
     it's inefficient. */
  return get_hard_reg_initial_val (Pmode, LINK_REGISTER_REGNUM);
}


/* Given VAL, generate a constant appropriate for MODE.
   If MODE is a vector mode, every element will be VAL.
   For TImode, VAL will be zero extended to 128 bits. */
rtx
spu_const (enum machine_mode mode, HOST_WIDE_INT val)
{
  rtx inner;
  rtvec v;
  int units, i;

  gcc_assert (GET_MODE_CLASS (mode) == MODE_INT
	      || GET_MODE_CLASS (mode) == MODE_FLOAT
	      || GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	      || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT);

  if (GET_MODE_CLASS (mode) == MODE_INT)
    return immed_double_const (val, 0, mode);

  /* val is the bit representation of the float */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return hwint_to_const_double (mode, val);

  if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    inner = immed_double_const (val, 0, GET_MODE_INNER (mode));
  else 
    inner = hwint_to_const_double (GET_MODE_INNER (mode), val);

  units = GET_MODE_NUNITS (mode);

  v = rtvec_alloc (units);

  for (i = 0; i < units; ++i)
    RTVEC_ELT (v, i) = inner;

  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Create a MODE vector constant from 4 ints. */
rtx
spu_const_from_ints(enum machine_mode mode, int a, int b, int c, int d)
{
  unsigned char arr[16];
  arr[0] = (a >> 24) & 0xff;
  arr[1] = (a >> 16) & 0xff;
  arr[2] = (a >> 8) & 0xff;
  arr[3] = (a >> 0) & 0xff;
  arr[4] = (b >> 24) & 0xff;
  arr[5] = (b >> 16) & 0xff;
  arr[6] = (b >> 8) & 0xff;
  arr[7] = (b >> 0) & 0xff;
  arr[8] = (c >> 24) & 0xff;
  arr[9] = (c >> 16) & 0xff;
  arr[10] = (c >> 8) & 0xff;
  arr[11] = (c >> 0) & 0xff;
  arr[12] = (d >> 24) & 0xff;
  arr[13] = (d >> 16) & 0xff;
  arr[14] = (d >> 8) & 0xff;
  arr[15] = (d >> 0) & 0xff;
  return array_to_constant(mode, arr);
}

/* branch hint stuff */

/* An array of these is used to propagate hints to predecessor blocks. */
struct spu_bb_info
{
  rtx prop_jump; /* propagated from another block */
  int bb_index;  /* the original block. */
};
static struct spu_bb_info *spu_bb_info;

#define STOP_HINT_P(INSN) \
		(GET_CODE(INSN) == CALL_INSN \
		 || INSN_CODE(INSN) == CODE_FOR_divmodsi4 \
		 || INSN_CODE(INSN) == CODE_FOR_udivmodsi4)

/* 1 when RTX is a hinted branch or its target.  We keep track of
   what has been hinted so the safe-hint code can test it easily.  */
#define HINTED_P(RTX)						\
  (RTL_FLAG_CHECK3("HINTED_P", (RTX), CODE_LABEL, JUMP_INSN, CALL_INSN)->unchanging)

/* 1 when RTX is an insn that must be scheduled on an even boundary. */
#define SCHED_ON_EVEN_P(RTX)						\
  (RTL_FLAG_CHECK2("SCHED_ON_EVEN_P", (RTX), JUMP_INSN, CALL_INSN)->in_struct)

/* Emit a nop for INSN such that the two will dual issue.  This assumes
   INSN is 8-byte aligned.  When INSN is inline asm we emit an lnop.
   We check for TImode to handle a MULTI1 insn which has dual issued its
   first instruction.  get_pipe returns -1 for MULTI0, inline asm, or
   ADDR_VEC insns. */
static void
emit_nop_for_insn (rtx insn)
{
  int p;
  rtx new_insn;
  p = get_pipe (insn);
  if ((CALL_P (insn) || JUMP_P (insn)) && SCHED_ON_EVEN_P (insn))
    new_insn = emit_insn_after (gen_lnop (), insn);
  else if (p == 1 && GET_MODE (insn) == TImode)
    {
      new_insn = emit_insn_before (gen_nopn (GEN_INT (127)), insn);
      PUT_MODE (new_insn, TImode);
      PUT_MODE (insn, VOIDmode);
    }
  else
    new_insn = emit_insn_after (gen_lnop (), insn);
  recog_memoized (new_insn);
}

/* Insert nops in basic blocks to meet dual issue alignment
   requirements.  Also make sure hbrp and hint instructions are at least
   one cycle apart, possibly inserting a nop.  */
static void
pad_bb(void)
{
  rtx insn, next_insn, prev_insn, hbr_insn = 0;
  int length;
  int addr;

  /* This sets up INSN_ADDRESSES. */
  shorten_branches (get_insns ());

  /* Keep track of length added by nops. */
  length = 0;

  prev_insn = 0;
  insn = get_insns ();
  if (!active_insn_p (insn))
    insn = next_active_insn (insn);
  for (; insn; insn = next_insn)
    {
      next_insn = next_active_insn (insn);
      if (INSN_CODE (insn) == CODE_FOR_iprefetch
	  || INSN_CODE (insn) == CODE_FOR_hbr)
	{
	  if (hbr_insn)
	    {
	      int a0 = INSN_ADDRESSES (INSN_UID (hbr_insn));
	      int a1 = INSN_ADDRESSES (INSN_UID (insn));
	      if ((a1 - a0 == 8 && GET_MODE (insn) != TImode)
		  || (a1 - a0 == 4))
		{
		  prev_insn = emit_insn_before (gen_lnop (), insn);
		  PUT_MODE (prev_insn, GET_MODE (insn));
		  PUT_MODE (insn, TImode);
		  length += 4;
		}
	    }
	  hbr_insn = insn;
	}
      if (INSN_CODE (insn) == CODE_FOR_blockage)
	{
	  if (GET_MODE (insn) == TImode)
	    PUT_MODE (next_insn, TImode);
	  insn = next_insn;
	  next_insn = next_active_insn (insn);
	}
      addr = INSN_ADDRESSES (INSN_UID (insn));
      if ((CALL_P (insn) || JUMP_P (insn)) && SCHED_ON_EVEN_P (insn))
	{
	  if (((addr + length) & 7) != 0)
	    {
	      emit_nop_for_insn (prev_insn);
	      length += 4;
	    }
	}
      else if (GET_MODE (insn) == TImode
	       && ((next_insn && GET_MODE (next_insn) != TImode)
		   || get_attr_type (insn) == TYPE_MULTI0)
	       && ((addr + length) & 7) != 0)
	{
	  /* prev_insn will always be set because the first insn is
	     always 8-byte aligned. */
	  emit_nop_for_insn (prev_insn);
	  length += 4;
	}
      prev_insn = insn;
    }
}


/* Routines for branch hints. */

static void
spu_emit_branch_hint (rtx before, rtx branch, rtx target,
		      int distance, sbitmap blocks)
{
  rtx branch_label = 0;
  rtx hint;
  rtx insn;
  rtx table;

  if (before == 0 || branch == 0 || target == 0)
    return;

  /* While scheduling we require hints to be no further than 600, so
     we need to enforce that here too */
  if (distance > 600)
    return;

  /* If we have a Basic block note, emit it after the basic block note.  */
  if (NOTE_INSN_BASIC_BLOCK_P (before))
    before = NEXT_INSN (before);

  branch_label = gen_label_rtx ();
  LABEL_NUSES (branch_label)++;
  LABEL_PRESERVE_P (branch_label) = 1;
  insn = emit_label_before (branch_label, branch);
  branch_label = gen_rtx_LABEL_REF (VOIDmode, branch_label);
  SET_BIT (blocks, BLOCK_FOR_INSN (branch)->index);

  hint = emit_insn_before (gen_hbr (branch_label, target), before);
  recog_memoized (hint);
  HINTED_P (branch) = 1;

  if (GET_CODE (target) == LABEL_REF)
    HINTED_P (XEXP (target, 0)) = 1;
  else if (tablejump_p (branch, 0, &table))
    {
      rtvec vec;
      int j;
      if (GET_CODE (PATTERN (table)) == ADDR_VEC)
	vec = XVEC (PATTERN (table), 0);
      else
	vec = XVEC (PATTERN (table), 1);
      for (j = GET_NUM_ELEM (vec) - 1; j >= 0; --j)
	HINTED_P (XEXP (RTVEC_ELT (vec, j), 0)) = 1;
    }

  if (distance >= 588)
    {
      /* Make sure the hint isn't scheduled any earlier than this point,
         which could make it too far for the branch offest to fit */
      recog_memoized (emit_insn_before (gen_blockage (), hint));
    }
  else if (distance <= 8 * 4)
    {
      /* To guarantee at least 8 insns between the hint and branch we
         insert nops. */
      int d;
      for (d = distance; d < 8 * 4; d += 4)
	{
	  insn =
	    emit_insn_after (gen_nopn_nv (gen_rtx_REG (SImode, 127)), hint);
	  recog_memoized (insn);
	}

      /* Make sure any nops inserted aren't scheduled before the hint. */
      recog_memoized (emit_insn_after (gen_blockage (), hint));

      /* Make sure any nops inserted aren't scheduled after the call. */
      if (CALL_P (branch) && distance < 8 * 4)
	recog_memoized (emit_insn_before (gen_blockage (), branch));
    }
}

/* Returns 0 if we don't want a hint for this branch.  Otherwise return
   the rtx for the branch target. */
static rtx
get_branch_target (rtx branch)
{
  if (GET_CODE (branch) == JUMP_INSN)
    {
      rtx set, src;

      /* Return statements */
      if (GET_CODE (PATTERN (branch)) == RETURN)
	return gen_rtx_REG (SImode, LINK_REGISTER_REGNUM);

      /* jump table */
      if (GET_CODE (PATTERN (branch)) == ADDR_VEC
	  || GET_CODE (PATTERN (branch)) == ADDR_DIFF_VEC)
	return 0;

      set = single_set (branch);
      src = SET_SRC (set);
      if (GET_CODE (SET_DEST (set)) != PC)
	abort ();

      if (GET_CODE (src) == IF_THEN_ELSE)
	{
	  rtx lab = 0;
	  rtx note = find_reg_note (branch, REG_BR_PROB, 0);
	  if (note)
	    {
	      /* If the more probable case is not a fall through, then
	         try a branch hint.  */
	      HOST_WIDE_INT prob = INTVAL (XEXP (note, 0));
	      if (prob > (REG_BR_PROB_BASE * 6 / 10)
		  && GET_CODE (XEXP (src, 1)) != PC)
		lab = XEXP (src, 1);
	      else if (prob < (REG_BR_PROB_BASE * 4 / 10)
		       && GET_CODE (XEXP (src, 2)) != PC)
		lab = XEXP (src, 2);
	    }
	  if (lab)
	    {
	      if (GET_CODE (lab) == RETURN)
		return gen_rtx_REG (SImode, LINK_REGISTER_REGNUM);
	      return lab;
	    }
	  return 0;
	}

      return src;
    }
  else if (GET_CODE (branch) == CALL_INSN)
    {
      rtx call;
      /* All of our call patterns are in a PARALLEL and the CALL is
         the first pattern in the PARALLEL. */
      if (GET_CODE (PATTERN (branch)) != PARALLEL)
	abort ();
      call = XVECEXP (PATTERN (branch), 0, 0);
      if (GET_CODE (call) == SET)
	call = SET_SRC (call);
      if (GET_CODE (call) != CALL)
	abort ();
      return XEXP (XEXP (call, 0), 0);
    }
  return 0;
}

/* The special $hbr register is used to prevent the insn scheduler from
   moving hbr insns across instructions which invalidate them.  It
   should only be used in a clobber, and this function searches for
   insns which clobber it.  */
static bool
insn_clobbers_hbr (rtx insn)
{
  if (INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      rtx parallel = PATTERN (insn);
      rtx clobber;
      int j;
      for (j = XVECLEN (parallel, 0) - 1; j >= 0; j--)
	{
	  clobber = XVECEXP (parallel, 0, j);
	  if (GET_CODE (clobber) == CLOBBER
	      && GET_CODE (XEXP (clobber, 0)) == REG
	      && REGNO (XEXP (clobber, 0)) == HBR_REGNUM)
	    return 1;
	}
    }
  return 0;
}

/* Search up to 32 insns starting at FIRST:
   - at any kind of hinted branch, just return
   - at any unconditional branch in the first 15 insns, just return
   - at a call or indirect branch, after the first 15 insns, force it to
     an even address and return
   - at any unconditional branch, after the first 15 insns, force it to
     an even address. 
   At then end of the search, insert an hbrp within 4 insns of FIRST,
   and an hbrp within 16 instructions of FIRST.
 */
static void
insert_hbrp_for_ilb_runout (rtx first)
{
  rtx insn, before_4 = 0, before_16 = 0;
  int addr = 0, length, first_addr = -1;
  int hbrp_addr0 = 128 * 4, hbrp_addr1 = 128 * 4;
  int insert_lnop_after = 0;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	if (first_addr == -1)
	  first_addr = INSN_ADDRESSES (INSN_UID (insn));
	addr = INSN_ADDRESSES (INSN_UID (insn)) - first_addr;
	length = get_attr_length (insn);

	if (before_4 == 0 && addr + length >= 4 * 4)
	  before_4 = insn;
	/* We test for 14 instructions because the first hbrp will add
	   up to 2 instructions. */
	if (before_16 == 0 && addr + length >= 14 * 4)
	  before_16 = insn;

	if (INSN_CODE (insn) == CODE_FOR_hbr)
	  {
	    /* Make sure an hbrp is at least 2 cycles away from a hint. 
	       Insert an lnop after the hbrp when necessary. */
	    if (before_4 == 0 && addr > 0)
	      {
		before_4 = insn;
		insert_lnop_after |= 1;
	      }
	    else if (before_4 && addr <= 4 * 4)
	      insert_lnop_after |= 1;
	    if (before_16 == 0 && addr > 10 * 4)
	      {
		before_16 = insn;
		insert_lnop_after |= 2;
	      }
	    else if (before_16 && addr <= 14 * 4)
	      insert_lnop_after |= 2;
	  }

	if (INSN_CODE (insn) == CODE_FOR_iprefetch)
	  {
	    if (addr < hbrp_addr0)
	      hbrp_addr0 = addr;
	    else if (addr < hbrp_addr1)
	      hbrp_addr1 = addr;
	  }

	if (CALL_P (insn) || JUMP_P (insn))
	  {
	    if (HINTED_P (insn))
	      return;

	    /* Any branch after the first 15 insns should be on an even
	       address to avoid a special case branch.  There might be
	       some nops and/or hbrps inserted, so we test after 10
	       insns. */
	    if (addr > 10 * 4)
	      SCHED_ON_EVEN_P (insn) = 1;
	  }

	if (CALL_P (insn) || tablejump_p (insn, 0, 0))
	  return;


	if (addr + length >= 32 * 4)
	  {
	    gcc_assert (before_4 && before_16);
	    if (hbrp_addr0 > 4 * 4)
	      {
		insn =
		  emit_insn_before (gen_iprefetch (GEN_INT (1)), before_4);
		recog_memoized (insn);
		INSN_ADDRESSES_NEW (insn,
				    INSN_ADDRESSES (INSN_UID (before_4)));
		PUT_MODE (insn, GET_MODE (before_4));
		PUT_MODE (before_4, TImode);
		if (insert_lnop_after & 1)
		  {
		    insn = emit_insn_before (gen_lnop (), before_4);
		    recog_memoized (insn);
		    INSN_ADDRESSES_NEW (insn,
					INSN_ADDRESSES (INSN_UID (before_4)));
		    PUT_MODE (insn, TImode);
		  }
	      }
	    if ((hbrp_addr0 <= 4 * 4 || hbrp_addr0 > 16 * 4)
		&& hbrp_addr1 > 16 * 4)
	      {
		insn =
		  emit_insn_before (gen_iprefetch (GEN_INT (2)), before_16);
		recog_memoized (insn);
		INSN_ADDRESSES_NEW (insn,
				    INSN_ADDRESSES (INSN_UID (before_16)));
		PUT_MODE (insn, GET_MODE (before_16));
		PUT_MODE (before_16, TImode);
		if (insert_lnop_after & 2)
		  {
		    insn = emit_insn_before (gen_lnop (), before_16);
		    recog_memoized (insn);
		    INSN_ADDRESSES_NEW (insn,
					INSN_ADDRESSES (INSN_UID
							(before_16)));
		    PUT_MODE (insn, TImode);
		  }
	      }
	    return;
	  }
      }
    else if (BARRIER_P (insn))
      return;

}

/* The SPU might hang when it executes 48 inline instructions after a
   hinted branch jumps to its hinted target.  The beginning of a
   function and the return from a call might have been hinted, and must
   be handled as well.  To prevent a hang we insert 2 hbrps.  The first
   should be within 6 insns of the branch target.  The second should be
   within 22 insns of the branch target.  When determining if hbrps are
   necessary, we look for only 32 inline instructions, because up to to
   12 nops and 4 hbrps could be inserted.  Similarily, when inserting
   new hbrps, we insert them within 4 and 16 insns of the target.  */
static void
insert_hbrp (void)
{
  rtx insn;
  if (TARGET_SAFE_HINTS)
    {
      shorten_branches (get_insns ());
      /* Insert hbrp at beginning of function */
      insn = next_active_insn (get_insns ());
      if (insn)
	insert_hbrp_for_ilb_runout (insn);
      /* Insert hbrp after hinted targets. */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if ((LABEL_P (insn) && HINTED_P (insn)) || CALL_P (insn))
	  insert_hbrp_for_ilb_runout (next_active_insn (insn));
    }
}

static int in_spu_reorg;

/* Insert branch hints.  There are no branch optimizations after this
   pass, so it's safe to set our branch hints now. */
static void
spu_machine_dependent_reorg (void)
{
  sbitmap blocks;
  basic_block bb;
  rtx branch, insn;
  rtx branch_target = 0;
  int branch_addr = 0, insn_addr, required_dist = 0;
  int i;
  unsigned int j;

  if (!TARGET_BRANCH_HINTS || optimize == 0)
    {
      /* We still do it for unoptimized code because an external
         function might have hinted a call or return. */
      insert_hbrp ();
      pad_bb ();
      return;
    }

  blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (blocks);

  in_spu_reorg = 1;
  compute_bb_for_insn ();

  compact_blocks ();

  spu_bb_info =
    (struct spu_bb_info *) xcalloc (n_basic_blocks,
				    sizeof (struct spu_bb_info));

  /* We need exact insn addresses and lengths.  */
  shorten_branches (get_insns ());

  for (i = n_basic_blocks - 1; i >= 0; i--)
    {
      bb = BASIC_BLOCK (i);
      branch = 0;
      if (spu_bb_info[i].prop_jump)
	{
	  branch = spu_bb_info[i].prop_jump;
	  branch_target = get_branch_target (branch);
	  branch_addr = INSN_ADDRESSES (INSN_UID (branch));
	  required_dist = spu_hint_dist;
	}
      /* Search from end of a block to beginning.   In this loop, find
         jumps which need a branch and emit them only when:
         - it's an indirect branch and we're at the insn which sets
         the register  
         - we're at an insn that will invalidate the hint. e.g., a
         call, another hint insn, inline asm that clobbers $hbr, and
         some inlined operations (divmodsi4).  Don't consider jumps
         because they are only at the end of a block and are
         considered when we are deciding whether to propagate
         - we're getting too far away from the branch.  The hbr insns
         only have a signed 10 bit offset
         We go back as far as possible so the branch will be considered
         for propagation when we get to the beginning of the block.  */
      for (insn = BB_END (bb); insn; insn = PREV_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      insn_addr = INSN_ADDRESSES (INSN_UID (insn));
	      if (branch
		  && ((GET_CODE (branch_target) == REG
		       && set_of (branch_target, insn) != NULL_RTX)
		      || insn_clobbers_hbr (insn)
		      || branch_addr - insn_addr > 600))
		{
		  rtx next = NEXT_INSN (insn);
		  int next_addr = INSN_ADDRESSES (INSN_UID (next));
		  if (insn != BB_END (bb)
		      && branch_addr - next_addr >= required_dist)
		    {
		      if (dump_file)
			fprintf (dump_file,
				 "hint for %i in block %i before %i\n",
				 INSN_UID (branch), bb->index,
				 INSN_UID (next));
		      spu_emit_branch_hint (next, branch, branch_target,
					    branch_addr - next_addr, blocks);
		    }
		  branch = 0;
		}

	      /* JUMP_P will only be true at the end of a block.  When
	         branch is already set it means we've previously decided
	         to propagate a hint for that branch into this block. */
	      if (CALL_P (insn) || (JUMP_P (insn) && !branch))
		{
		  branch = 0;
		  if ((branch_target = get_branch_target (insn)))
		    {
		      branch = insn;
		      branch_addr = insn_addr;
		      required_dist = spu_hint_dist;
		    }
		}
	    }
	  if (insn == BB_HEAD (bb))
	    break;
	}

      if (branch)
	{
	  /* If we haven't emitted a hint for this branch yet, it might
	     be profitable to emit it in one of the predecessor blocks,
	     especially for loops.  */
	  rtx bbend;
	  basic_block prev = 0, prop = 0, prev2 = 0;
	  int loop_exit = 0, simple_loop = 0;
	  int next_addr = INSN_ADDRESSES (INSN_UID (NEXT_INSN (insn)));

	  for (j = 0; j < EDGE_COUNT (bb->preds); j++)
	    if (EDGE_PRED (bb, j)->flags & EDGE_FALLTHRU)
	      prev = EDGE_PRED (bb, j)->src;
	    else
	      prev2 = EDGE_PRED (bb, j)->src;

	  for (j = 0; j < EDGE_COUNT (bb->succs); j++)
	    if (EDGE_SUCC (bb, j)->flags & EDGE_LOOP_EXIT)
	      loop_exit = 1;
	    else if (EDGE_SUCC (bb, j)->dest == bb)
	      simple_loop = 1;

	  /* If this branch is a loop exit then propagate to previous
	     fallthru block. This catches the cases when it is a simple
	     loop or when there is an initial branch into the loop. */
	  if (prev && (loop_exit || simple_loop)
	      && prev->loop_depth <= bb->loop_depth)
	    prop = prev;

	  /* If there is only one adjacent predecessor.  Don't propagate
	     outside this loop.  This loop_depth test isn't perfect, but
	     I'm not sure the loop_father member is valid at this point.  */
	  else if (prev && single_pred_p (bb)
		   && prev->loop_depth == bb->loop_depth)
	    prop = prev;

	  /* If this is the JOIN block of a simple IF-THEN then
	     propogate the hint to the HEADER block. */
	  else if (prev && prev2
		   && EDGE_COUNT (bb->preds) == 2
		   && EDGE_COUNT (prev->preds) == 1
		   && EDGE_PRED (prev, 0)->src == prev2
		   && prev2->loop_depth == bb->loop_depth
		   && GET_CODE (branch_target) != REG)
	    prop = prev;

	  /* Don't propagate when:
	     - this is a simple loop and the hint would be too far
	     - this is not a simple loop and there are 16 insns in
	     this block already
	     - the predecessor block ends in a branch that will be
	     hinted
	     - the predecessor block ends in an insn that invalidates
	     the hint */
	  if (prop
	      && prop->index >= 0
	      && (bbend = BB_END (prop))
	      && branch_addr - INSN_ADDRESSES (INSN_UID (bbend)) <
	      (simple_loop ? 600 : 16 * 4) && get_branch_target (bbend) == 0
	      && (JUMP_P (bbend) || !insn_clobbers_hbr (bbend)))
	    {
	      if (dump_file)
		fprintf (dump_file, "propagate from %i to %i (loop depth %i) "
			 "for %i (loop_exit %i simple_loop %i dist %i)\n",
			 bb->index, prop->index, bb->loop_depth,
			 INSN_UID (branch), loop_exit, simple_loop,
			 branch_addr - INSN_ADDRESSES (INSN_UID (bbend)));

	      spu_bb_info[prop->index].prop_jump = branch;
	      spu_bb_info[prop->index].bb_index = i;
	    }
	  else if (branch_addr - next_addr >= required_dist)
	    {
	      if (dump_file)
		fprintf (dump_file, "hint for %i in block %i before %i\n",
			 INSN_UID (branch), bb->index,
			 INSN_UID (NEXT_INSN (insn)));
	      spu_emit_branch_hint (NEXT_INSN (insn), branch, branch_target,
				    branch_addr - next_addr, blocks);
	    }
	  branch = 0;
	}
    }
  free (spu_bb_info);

  if (!sbitmap_empty_p (blocks))
    find_many_sub_basic_blocks (blocks);

  /* We have to schedule to make sure alignment is ok. */
  FOR_EACH_BB (bb) bb->flags &= ~BB_DISABLE_SCHEDULE;

  /* The hints need to be scheduled, so call it again. */
  schedule_insns ();

  insert_hbrp ();

  pad_bb ();

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (NONJUMP_INSN_P (insn) && INSN_CODE (insn) == CODE_FOR_hbr)
      {
	/* Adjust the LABEL_REF in a hint when we have inserted a nop
	   between its branch label and the branch .  We don't move the
	   label because GCC expects it at the beginning of the block. */
	rtx unspec = SET_SRC (XVECEXP (PATTERN (insn), 0, 0));
	rtx label_ref = XVECEXP (unspec, 0, 0);
	rtx label = XEXP (label_ref, 0);
	rtx branch;
	int offset = 0;
	for (branch = NEXT_INSN (label);
	     !JUMP_P (branch) && !CALL_P (branch);
	     branch = NEXT_INSN (branch))
	  if (NONJUMP_INSN_P (branch))
	    offset += get_attr_length (branch);
	if (offset > 0)
	  XVECEXP (unspec, 0, 0) = plus_constant (label_ref, offset);
      }

  if (spu_flag_var_tracking)
    {
      df_analyze ();
      timevar_push (TV_VAR_TRACKING);
      variable_tracking_main ();
      timevar_pop (TV_VAR_TRACKING);
      df_finish_pass (false);
    }

  free_bb_for_insn ();

  in_spu_reorg = 0;
}


/* Insn scheduling routines, primarily for dual issue. */
static int
spu_sched_issue_rate (void)
{
  return 2;
}

static int
uses_ls_unit(rtx insn)
{
  rtx set = single_set (insn);
  if (set != 0
      && (GET_CODE (SET_DEST (set)) == MEM
	  || GET_CODE (SET_SRC (set)) == MEM))
    return 1;
  return 0;
}

static int
get_pipe (rtx insn)
{
  enum attr_type t;
  /* Handle inline asm */
  if (INSN_CODE (insn) == -1)
    return -1;
  t = get_attr_type (insn);
  switch (t)
    {
    case TYPE_CONVERT:
      return -2;
    case TYPE_MULTI0:
      return -1;

    case TYPE_FX2:
    case TYPE_FX3:
    case TYPE_SPR:
    case TYPE_NOP:
    case TYPE_FXB:
    case TYPE_FPD:
    case TYPE_FP6:
    case TYPE_FP7:
      return 0;

    case TYPE_LNOP:
    case TYPE_SHUF:
    case TYPE_LOAD:
    case TYPE_STORE:
    case TYPE_BR:
    case TYPE_MULTI1:
    case TYPE_HBR:
    case TYPE_IPREFETCH:
      return 1;
    default:
      abort ();
    }
}


/* haifa-sched.c has a static variable that keeps track of the current
   cycle.  It is passed to spu_sched_reorder, and we record it here for
   use by spu_sched_variable_issue.  It won't be accurate if the
   scheduler updates it's clock_var between the two calls. */
static int clock_var;

/* This is used to keep track of insn alignment.  Set to 0 at the
   beginning of each block and increased by the "length" attr of each
   insn scheduled. */
static int spu_sched_length;

/* Record when we've issued pipe0 and pipe1 insns so we can reorder the
   ready list appropriately in spu_sched_reorder(). */
static int pipe0_clock;
static int pipe1_clock;

static int prev_clock_var;

static int prev_priority;

/* The SPU needs to load the next ilb sometime during the execution of
   the previous ilb.  There is a potential conflict if every cycle has a
   load or store.  To avoid the conflict we make sure the load/store
   unit is free for at least one cycle during the execution of insns in
   the previous ilb. */
static int spu_ls_first;
static int prev_ls_clock;

static void
spu_sched_init_global (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		       int max_ready ATTRIBUTE_UNUSED)
{
  spu_sched_length = 0;
}

static void
spu_sched_init (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		int max_ready ATTRIBUTE_UNUSED)
{
  if (align_labels > 4 || align_loops > 4 || align_jumps > 4)
    {
      /* When any block might be at least 8-byte aligned, assume they
         will all be at least 8-byte aligned to make sure dual issue
         works out correctly. */
      spu_sched_length = 0;
    }
  spu_ls_first = INT_MAX;
  clock_var = -1;
  prev_ls_clock = -1;
  pipe0_clock = -1;
  pipe1_clock = -1;
  prev_clock_var = -1;
  prev_priority = -1;
}

static int
spu_sched_variable_issue (FILE *file ATTRIBUTE_UNUSED,
			  int verbose ATTRIBUTE_UNUSED, rtx insn, int more)
{
  int len;
  int p;
  if (GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER
      || (len = get_attr_length (insn)) == 0)
    return more;

  spu_sched_length += len;

  /* Reset on inline asm */
  if (INSN_CODE (insn) == -1)
    {
      spu_ls_first = INT_MAX;
      pipe0_clock = -1;
      pipe1_clock = -1;
      return 0;
    }
  p = get_pipe (insn);
  if (p == 0)
    pipe0_clock = clock_var;
  else
    pipe1_clock = clock_var;

  if (in_spu_reorg)
    {
      if (clock_var - prev_ls_clock > 1
	  || INSN_CODE (insn) == CODE_FOR_iprefetch)
	spu_ls_first = INT_MAX;
      if (uses_ls_unit (insn))
	{
	  if (spu_ls_first == INT_MAX)
	    spu_ls_first = spu_sched_length;
	  prev_ls_clock = clock_var;
	}

      /* The scheduler hasn't inserted the nop, but we will later on.
         Include those nops in spu_sched_length. */
      if (prev_clock_var == clock_var && (spu_sched_length & 7))
	spu_sched_length += 4;
      prev_clock_var = clock_var;

      /* more is -1 when called from spu_sched_reorder for new insns
         that don't have INSN_PRIORITY */
      if (more >= 0)
	prev_priority = INSN_PRIORITY (insn);
    }

  /* Always try issueing more insns.  spu_sched_reorder will decide 
     when the cycle should be advanced. */
  return 1;
}

/* This function is called for both TARGET_SCHED_REORDER and
   TARGET_SCHED_REORDER2.  */
static int
spu_sched_reorder (FILE *file ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		   rtx *ready, int *nreadyp, int clock)
{
  int i, nready = *nreadyp;
  int pipe_0, pipe_1, pipe_hbrp, pipe_ls, schedule_i;
  rtx insn;

  clock_var = clock;

  if (nready <= 0 || pipe1_clock >= clock)
    return 0;

  /* Find any rtl insns that don't generate assembly insns and schedule
     them first. */
  for (i = nready - 1; i >= 0; i--)
    {
      insn = ready[i];
      if (INSN_CODE (insn) == -1
	  || INSN_CODE (insn) == CODE_FOR_blockage
	  || (INSN_P (insn) && get_attr_length (insn) == 0))
	{
	  ready[i] = ready[nready - 1];
	  ready[nready - 1] = insn;
	  return 1;
	}
    }

  pipe_0 = pipe_1 = pipe_hbrp = pipe_ls = schedule_i = -1;
  for (i = 0; i < nready; i++)
    if (INSN_CODE (ready[i]) != -1)
      {
	insn = ready[i];
	switch (get_attr_type (insn))
	  {
	  default:
	  case TYPE_MULTI0:
	  case TYPE_CONVERT:
	  case TYPE_FX2:
	  case TYPE_FX3:
	  case TYPE_SPR:
	  case TYPE_NOP:
	  case TYPE_FXB:
	  case TYPE_FPD:
	  case TYPE_FP6:
	  case TYPE_FP7:
	    pipe_0 = i;
	    break;
	  case TYPE_LOAD:
	  case TYPE_STORE:
	    pipe_ls = i;
	  case TYPE_LNOP:
	  case TYPE_SHUF:
	  case TYPE_BR:
	  case TYPE_MULTI1:
	  case TYPE_HBR:
	    pipe_1 = i;
	    break;
	  case TYPE_IPREFETCH:
	    pipe_hbrp = i;
	    break;
	  }
      }

  /* In the first scheduling phase, schedule loads and stores together
     to increase the chance they will get merged during postreload CSE. */
  if (!reload_completed && pipe_ls >= 0)
    {
      insn = ready[pipe_ls];
      ready[pipe_ls] = ready[nready - 1];
      ready[nready - 1] = insn;
      return 1;
    }

  /* If there is an hbrp ready, prefer it over other pipe 1 insns. */
  if (pipe_hbrp >= 0)
    pipe_1 = pipe_hbrp;

  /* When we have loads/stores in every cycle of the last 15 insns and
     we are about to schedule another load/store, emit an hbrp insn
     instead. */
  if (in_spu_reorg
      && spu_sched_length - spu_ls_first >= 4 * 15
      && !(pipe0_clock < clock && pipe_0 >= 0) && pipe_1 == pipe_ls)
    {
      insn = sched_emit_insn (gen_iprefetch (GEN_INT (3)));
      recog_memoized (insn);
      if (pipe0_clock < clock)
	PUT_MODE (insn, TImode);
      spu_sched_variable_issue (file, verbose, insn, -1);
      return 0;
    }

  /* In general, we want to emit nops to increase dual issue, but dual
     issue isn't faster when one of the insns could be scheduled later
     without effecting the critical path.  We look at INSN_PRIORITY to
     make a good guess, but it isn't perfect so -mdual-nops=n can be
     used to effect it. */
  if (in_spu_reorg && spu_dual_nops < 10)
    {
      /* When we are at an even address and we are not issueing nops to
         improve scheduling then we need to advance the cycle.  */
      if ((spu_sched_length & 7) == 0 && prev_clock_var == clock
	  && (spu_dual_nops == 0
	      || (pipe_1 != -1
		  && prev_priority >
		  INSN_PRIORITY (ready[pipe_1]) + spu_dual_nops)))
	return 0;

      /* When at an odd address, schedule the highest priority insn
         without considering pipeline. */
      if ((spu_sched_length & 7) == 4 && prev_clock_var != clock
	  && (spu_dual_nops == 0
	      || (prev_priority >
		  INSN_PRIORITY (ready[nready - 1]) + spu_dual_nops)))
	return 1;
    }


  /* We haven't issued a pipe0 insn yet this cycle, if there is a
     pipe0 insn in the ready list, schedule it. */
  if (pipe0_clock < clock && pipe_0 >= 0)
    schedule_i = pipe_0;

  /* Either we've scheduled a pipe0 insn already or there is no pipe0
     insn to schedule.  Put a pipe1 insn at the front of the ready list. */
  else
    schedule_i = pipe_1;

  if (schedule_i > -1)
    {
      insn = ready[schedule_i];
      ready[schedule_i] = ready[nready - 1];
      ready[nready - 1] = insn;
      return 1;
    }
  return 0;
}

/* INSN is dependent on DEP_INSN. */
static int
spu_sched_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  rtx set;

  /* The blockage pattern is used to prevent instructions from being
     moved across it and has no cost. */
  if (INSN_CODE (insn) == CODE_FOR_blockage
      || INSN_CODE (dep_insn) == CODE_FOR_blockage)
    return 0;

  if ((INSN_P (insn) && get_attr_length (insn) == 0)
      || (INSN_P (dep_insn) && get_attr_length (dep_insn) == 0))
    return 0;

  /* Make sure hbrps are spread out. */
  if (INSN_CODE (insn) == CODE_FOR_iprefetch
      && INSN_CODE (dep_insn) == CODE_FOR_iprefetch)
    return 8;

  /* Make sure hints and hbrps are 2 cycles apart. */
  if ((INSN_CODE (insn) == CODE_FOR_iprefetch
       || INSN_CODE (insn) == CODE_FOR_hbr)
       && (INSN_CODE (dep_insn) == CODE_FOR_iprefetch
	   || INSN_CODE (dep_insn) == CODE_FOR_hbr))
    return 2;

  /* An hbrp has no real dependency on other insns. */
  if (INSN_CODE (insn) == CODE_FOR_iprefetch
      || INSN_CODE (dep_insn) == CODE_FOR_iprefetch)
    return 0;

  /* Assuming that it is unlikely an argument register will be used in
     the first cycle of the called function, we reduce the cost for
     slightly better scheduling of dep_insn.  When not hinted, the
     mispredicted branch would hide the cost as well.  */
  if (CALL_P (insn))
  {
    rtx target = get_branch_target (insn);
    if (GET_CODE (target) != REG || !set_of (target, insn))
      return cost - 2;
    return cost;
  }

  /* And when returning from a function, let's assume the return values
     are completed sooner too. */
  if (CALL_P (dep_insn))
    return cost - 2;

  /* Make sure an instruction that loads from the back chain is schedule
     away from the return instruction so a hint is more likely to get
     issued. */
  if (INSN_CODE (insn) == CODE_FOR__return
      && (set = single_set (dep_insn))
      && GET_CODE (SET_DEST (set)) == REG
      && REGNO (SET_DEST (set)) == LINK_REGISTER_REGNUM)
    return 20;

  /* The dfa scheduler sets cost to 0 for all anti-dependencies and the
     scheduler makes every insn in a block anti-dependent on the final
     jump_insn.  We adjust here so higher cost insns will get scheduled
     earlier. */
  if (JUMP_P (insn) && REG_NOTE_KIND (link) == REG_DEP_ANTI)
    return insn_cost (dep_insn) - 3;

  return cost;
}

/* Create a CONST_DOUBLE from a string.  */
struct rtx_def *
spu_float_const (const char *string, enum machine_mode mode)
{
  REAL_VALUE_TYPE value;
  value = REAL_VALUE_ATOF (string, mode);
  return CONST_DOUBLE_FROM_REAL_VALUE (value, mode);
}

int
spu_constant_address_p (rtx x)
{
  return (GET_CODE (x) == LABEL_REF || GET_CODE (x) == SYMBOL_REF
	  || GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST
	  || GET_CODE (x) == HIGH);
}

static enum spu_immediate
which_immediate_load (HOST_WIDE_INT val)
{
  gcc_assert (val == trunc_int_for_mode (val, SImode));

  if (val >= -0x8000 && val <= 0x7fff)
    return SPU_IL;
  if (val >= 0 && val <= 0x3ffff)
    return SPU_ILA;
  if ((val & 0xffff) == ((val >> 16) & 0xffff))
    return SPU_ILH;
  if ((val & 0xffff) == 0)
    return SPU_ILHU;

  return SPU_NONE;
}

/* Return true when OP can be loaded by one of the il instructions, or
   when flow2 is not completed and OP can be loaded using ilhu and iohl. */
int
immediate_load_p (rtx op, enum machine_mode mode)
{
  if (CONSTANT_P (op))
    {
      enum immediate_class c = classify_immediate (op, mode);
      return c == IC_IL1 || c == IC_IL1s
	     || (!epilogue_completed && (c == IC_IL2 || c == IC_IL2s));
    }
  return 0;
}

/* Return true if the first SIZE bytes of arr is a constant that can be
   generated with cbd, chd, cwd or cdd.  When non-NULL, PRUN and PSTART
   represent the size and offset of the instruction to use. */
static int
cpat_info(unsigned char *arr, int size, int *prun, int *pstart)
{
  int cpat, run, i, start;
  cpat = 1;
  run = 0;
  start = -1;
  for (i = 0; i < size && cpat; i++)
    if (arr[i] != i+16)
      { 
	if (!run)
	  {
	    start = i;
	    if (arr[i] == 3)
	      run = 1;
	    else if (arr[i] == 2 && arr[i+1] == 3)
	      run = 2;
	    else if (arr[i] == 0)
	      {
		while (arr[i+run] == run && i+run < 16)
		  run++;
		if (run != 4 && run != 8)
		  cpat = 0;
	      }
	    else
	      cpat = 0;
	    if ((i & (run-1)) != 0)
	      cpat = 0;
	    i += run;
	  }
	else
	  cpat = 0;
      }
  if (cpat && (run || size < 16))
    {
      if (run == 0)
	run = 1;
      if (prun)
	*prun = run;
      if (pstart)
	*pstart = start == -1 ? 16-run : start;
      return 1;
    }
  return 0;
}

/* OP is a CONSTANT_P.  Determine what instructions can be used to load
   it into a register.  MODE is only valid when OP is a CONST_INT. */
static enum immediate_class
classify_immediate (rtx op, enum machine_mode mode)
{
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int i, j, repeated, fsmbi, repeat;

  gcc_assert (CONSTANT_P (op));

  if (GET_MODE (op) != VOIDmode)
    mode = GET_MODE (op);

  /* A V4SI const_vector with all identical symbols is ok. */
  if (!flag_pic
      && mode == V4SImode
      && GET_CODE (op) == CONST_VECTOR
      && GET_CODE (CONST_VECTOR_ELT (op, 0)) != CONST_INT
      && GET_CODE (CONST_VECTOR_ELT (op, 0)) != CONST_DOUBLE
      && CONST_VECTOR_ELT (op, 0) == CONST_VECTOR_ELT (op, 1)
      && CONST_VECTOR_ELT (op, 1) == CONST_VECTOR_ELT (op, 2)
      && CONST_VECTOR_ELT (op, 2) == CONST_VECTOR_ELT (op, 3))
    op = CONST_VECTOR_ELT (op, 0);

  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return TARGET_LARGE_MEM ? IC_IL2s : IC_IL1s;

    case CONST:
      /* We can never know if the resulting address fits in 18 bits and can be
	 loaded with ila.  For now, assume the address will not overflow if
	 the displacement is "small" (fits 'K' constraint).  */
      if (!TARGET_LARGE_MEM && GET_CODE (XEXP (op, 0)) == PLUS)
	{
	  rtx sym = XEXP (XEXP (op, 0), 0);
	  rtx cst = XEXP (XEXP (op, 0), 1);

	  if (GET_CODE (sym) == SYMBOL_REF
	      && GET_CODE (cst) == CONST_INT
	      && satisfies_constraint_K (cst))
	    return IC_IL1s;
	}
      return IC_IL2s;

    case HIGH:
      return IC_IL1s;

    case CONST_VECTOR:
      for (i = 0; i < GET_MODE_NUNITS (mode); i++)
	if (GET_CODE (CONST_VECTOR_ELT (op, i)) != CONST_INT
	    && GET_CODE (CONST_VECTOR_ELT (op, i)) != CONST_DOUBLE)
	  return IC_POOL;
      /* Fall through. */

    case CONST_INT:
    case CONST_DOUBLE:
      constant_to_array (mode, op, arr);

      /* Check that each 4-byte slot is identical. */
      repeated = 1;
      for (i = 4; i < 16; i += 4)
	for (j = 0; j < 4; j++)
	  if (arr[j] != arr[i + j])
	    repeated = 0;

      if (repeated)
	{
	  val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
	  val = trunc_int_for_mode (val, SImode);

	  if (which_immediate_load (val) != SPU_NONE)
	    return IC_IL1;
	}

      /* Any mode of 2 bytes or smaller can be loaded with an il
         instruction. */
      gcc_assert (GET_MODE_SIZE (mode) > 2);

      fsmbi = 1;
      repeat = 0;
      for (i = 0; i < 16 && fsmbi; i++)
	if (arr[i] != 0 && repeat == 0)
	  repeat = arr[i];
	else if (arr[i] != 0 && arr[i] != repeat)
	  fsmbi = 0;
      if (fsmbi)
	return repeat == 0xff ? IC_FSMBI : IC_FSMBI2;

      if (cpat_info (arr, GET_MODE_SIZE (mode), 0, 0))
	return IC_CPAT;

      if (repeated)
	return IC_IL2;

      return IC_POOL;
    default:
      break;
    }
  gcc_unreachable ();
}

static enum spu_immediate
which_logical_immediate (HOST_WIDE_INT val)
{
  gcc_assert (val == trunc_int_for_mode (val, SImode));

  if (val >= -0x200 && val <= 0x1ff)
    return SPU_ORI;
  if (val >= 0 && val <= 0xffff)
    return SPU_IOHL;
  if ((val & 0xffff) == ((val >> 16) & 0xffff))
    {
      val = trunc_int_for_mode (val, HImode);
      if (val >= -0x200 && val <= 0x1ff)
	return SPU_ORHI;
      if ((val & 0xff) == ((val >> 8) & 0xff))
	{
	  val = trunc_int_for_mode (val, QImode);
	  if (val >= -0x200 && val <= 0x1ff)
	    return SPU_ORBI;
	}
    }
  return SPU_NONE;
}

/* Return TRUE when X, a CONST_VECTOR, only contains CONST_INTs or
   CONST_DOUBLEs. */
static int
const_vector_immediate_p (rtx x)
{
  int i;
  gcc_assert (GET_CODE (x) == CONST_VECTOR);
  for (i = 0; i < GET_MODE_NUNITS (GET_MODE (x)); i++)
    if (GET_CODE (CONST_VECTOR_ELT (x, i)) != CONST_INT
	&& GET_CODE (CONST_VECTOR_ELT (x, i)) != CONST_DOUBLE)
      return 0;
  return 1;
}

int
logical_immediate_p (rtx op, enum machine_mode mode)
{
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int i, j;

  gcc_assert (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	      || GET_CODE (op) == CONST_VECTOR);

  if (GET_CODE (op) == CONST_VECTOR
      && !const_vector_immediate_p (op))
    return 0;

  if (GET_MODE (op) != VOIDmode)
    mode = GET_MODE (op);

  constant_to_array (mode, op, arr);

  /* Check that bytes are repeated. */
  for (i = 4; i < 16; i += 4)
    for (j = 0; j < 4; j++)
      if (arr[j] != arr[i + j])
	return 0;

  val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
  val = trunc_int_for_mode (val, SImode);

  i = which_logical_immediate (val);
  return i != SPU_NONE && i != SPU_IOHL;
}

int
iohl_immediate_p (rtx op, enum machine_mode mode)
{
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int i, j;

  gcc_assert (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	      || GET_CODE (op) == CONST_VECTOR);

  if (GET_CODE (op) == CONST_VECTOR
      && !const_vector_immediate_p (op))
    return 0;

  if (GET_MODE (op) != VOIDmode)
    mode = GET_MODE (op);

  constant_to_array (mode, op, arr);

  /* Check that bytes are repeated. */
  for (i = 4; i < 16; i += 4)
    for (j = 0; j < 4; j++)
      if (arr[j] != arr[i + j])
	return 0;

  val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
  val = trunc_int_for_mode (val, SImode);

  return val >= 0 && val <= 0xffff;
}

int
arith_immediate_p (rtx op, enum machine_mode mode,
		   HOST_WIDE_INT low, HOST_WIDE_INT high)
{
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int bytes, i, j;

  gcc_assert (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	      || GET_CODE (op) == CONST_VECTOR);

  if (GET_CODE (op) == CONST_VECTOR
      && !const_vector_immediate_p (op))
    return 0;

  if (GET_MODE (op) != VOIDmode)
    mode = GET_MODE (op);

  constant_to_array (mode, op, arr);

  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  bytes = GET_MODE_SIZE (mode);
  mode = mode_for_size (GET_MODE_BITSIZE (mode), MODE_INT, 0);

  /* Check that bytes are repeated. */
  for (i = bytes; i < 16; i += bytes)
    for (j = 0; j < bytes; j++)
      if (arr[j] != arr[i + j])
	return 0;

  val = arr[0];
  for (j = 1; j < bytes; j++)
    val = (val << 8) | arr[j];

  val = trunc_int_for_mode (val, mode);

  return val >= low && val <= high;
}

/* TRUE when op is an immediate and an exact power of 2, and given that
   OP is 2^scale, scale >= LOW && scale <= HIGH.  When OP is a vector,
   all entries must be the same. */
bool
exp2_immediate_p (rtx op, enum machine_mode mode, int low, int high)
{
  enum machine_mode int_mode;
  HOST_WIDE_INT val;
  unsigned char arr[16];
  int bytes, i, j;

  gcc_assert (GET_CODE (op) == CONST_INT || GET_CODE (op) == CONST_DOUBLE
	      || GET_CODE (op) == CONST_VECTOR);

  if (GET_CODE (op) == CONST_VECTOR
      && !const_vector_immediate_p (op))
    return 0;

  if (GET_MODE (op) != VOIDmode)
    mode = GET_MODE (op);

  constant_to_array (mode, op, arr);

  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  bytes = GET_MODE_SIZE (mode);
  int_mode = mode_for_size (GET_MODE_BITSIZE (mode), MODE_INT, 0);

  /* Check that bytes are repeated. */
  for (i = bytes; i < 16; i += bytes)
    for (j = 0; j < bytes; j++)
      if (arr[j] != arr[i + j])
	return 0;

  val = arr[0];
  for (j = 1; j < bytes; j++)
    val = (val << 8) | arr[j];

  val = trunc_int_for_mode (val, int_mode);

  /* Currently, we only handle SFmode */
  gcc_assert (mode == SFmode);
  if (mode == SFmode)
    {
      int exp = (val >> 23) - 127;
      return val > 0 && (val & 0x007fffff) == 0
	     &&  exp >= low && exp <= high;
    }
  return FALSE;
}

/* We accept:
   - any 32-bit constant (SImode, SFmode)
   - any constant that can be generated with fsmbi (any mode)
   - a 64-bit constant where the high and low bits are identical
     (DImode, DFmode)
   - a 128-bit constant where the four 32-bit words match.  */
int
spu_legitimate_constant_p (rtx x)
{
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);
  /* V4SI with all identical symbols is valid. */
  if (!flag_pic
      && GET_MODE (x) == V4SImode
      && (GET_CODE (CONST_VECTOR_ELT (x, 0)) == SYMBOL_REF
	  || GET_CODE (CONST_VECTOR_ELT (x, 0)) == LABEL_REF
	  || GET_CODE (CONST_VECTOR_ELT (x, 0)) == CONST))
    return CONST_VECTOR_ELT (x, 0) == CONST_VECTOR_ELT (x, 1)
	   && CONST_VECTOR_ELT (x, 1) == CONST_VECTOR_ELT (x, 2)
	   && CONST_VECTOR_ELT (x, 2) == CONST_VECTOR_ELT (x, 3);

  if (GET_CODE (x) == CONST_VECTOR
      && !const_vector_immediate_p (x))
    return 0;
  return 1;
}

/* Valid address are:
   - symbol_ref, label_ref, const
   - reg
   - reg + const_int, where const_int is 16 byte aligned
   - reg + reg, alignment doesn't matter
  The alignment matters in the reg+const case because lqd and stqd
  ignore the 4 least significant bits of the const.  We only care about
  16 byte modes because the expand phase will change all smaller MEM
  references to TImode.  */
int
spu_legitimate_address (enum machine_mode mode ATTRIBUTE_UNUSED,
			rtx x, int reg_ok_strict)
{
  int aligned = GET_MODE_SIZE (mode) >= 16;
  if (aligned
      && GET_CODE (x) == AND
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) == (HOST_WIDE_INT) - 16)
    x = XEXP (x, 0);
  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      return !TARGET_LARGE_MEM;

    case CONST_INT:
      return INTVAL (x) >= 0 && INTVAL (x) <= 0x3ffff;

    case SUBREG:
      x = XEXP (x, 0);
      if (REG_P (x))
	return 0;

    case REG:
      return INT_REG_OK_FOR_BASE_P (x, reg_ok_strict);

    case PLUS:
    case LO_SUM:
      {
	rtx op0 = XEXP (x, 0);
	rtx op1 = XEXP (x, 1);
	if (GET_CODE (op0) == SUBREG)
	  op0 = XEXP (op0, 0);
	if (GET_CODE (op1) == SUBREG)
	  op1 = XEXP (op1, 0);
	if (GET_CODE (op0) == REG
	    && INT_REG_OK_FOR_BASE_P (op0, reg_ok_strict)
	    && GET_CODE (op1) == CONST_INT
	    && INTVAL (op1) >= -0x2000
	    && INTVAL (op1) <= 0x1fff
	    && (!aligned || (INTVAL (op1) & 15) == 0))
	  return TRUE;
	if (GET_CODE (op0) == REG
	    && INT_REG_OK_FOR_BASE_P (op0, reg_ok_strict)
	    && GET_CODE (op1) == REG
	    && INT_REG_OK_FOR_INDEX_P (op1, reg_ok_strict))
	  return TRUE;
      }
      break;

    default:
      break;
    }
  return FALSE;
}

/* When the address is reg + const_int, force the const_int into a
   register.  */
rtx
spu_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			enum machine_mode mode)
{
  rtx op0, op1;
  /* Make sure both operands are registers.  */
  if (GET_CODE (x) == PLUS)
    {
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      if (ALIGNED_SYMBOL_REF_P (op0))
	{
	  op0 = force_reg (Pmode, op0);
	  mark_reg_pointer (op0, 128);
	}
      else if (GET_CODE (op0) != REG)
	op0 = force_reg (Pmode, op0);
      if (ALIGNED_SYMBOL_REF_P (op1))
	{
	  op1 = force_reg (Pmode, op1);
	  mark_reg_pointer (op1, 128);
	}
      else if (GET_CODE (op1) != REG)
	op1 = force_reg (Pmode, op1);
      x = gen_rtx_PLUS (Pmode, op0, op1);
      if (spu_legitimate_address (mode, x, 0))
	return x;
    }
  return NULL_RTX;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */
static tree
spu_handle_fndecl_attribute (tree * node,
			     tree name,
			     tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED, bool * no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (0, "`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle the "vector" attribute.  */
static tree
spu_handle_vector_attribute (tree * node, tree name,
			     tree args ATTRIBUTE_UNUSED,
			     int flags ATTRIBUTE_UNUSED, bool * no_add_attrs)
{
  tree type = *node, result = NULL_TREE;
  enum machine_mode mode;
  int unsigned_p;

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  mode = TYPE_MODE (type);

  unsigned_p = TYPE_UNSIGNED (type);
  switch (mode)
    {
    case DImode:
      result = (unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node);
      break;
    case SImode:
      result = (unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node);
      break;
    case HImode:
      result = (unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node);
      break;
    case QImode:
      result = (unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node);
      break;
    case SFmode:
      result = V4SF_type_node;
      break;
    case DFmode:
      result = V2DF_type_node;
      break;
    default:
      break;
    }

  /* Propagate qualifiers attached to the element type
     onto the vector type.  */
  if (result && result != type && TYPE_QUALS (type))
    result = build_qualified_type (result, TYPE_QUALS (type));

  *no_add_attrs = true;		/* No need to hang on to the attribute.  */

  if (!result)
    warning (0, "`%s' attribute ignored", IDENTIFIER_POINTER (name));
  else
    *node = lang_hooks.types.reconstruct_complex_type (*node, result);

  return NULL_TREE;
}

/* Return nonzero if FUNC is a naked function.  */
static int
spu_naked_function_p (tree func)
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  a = lookup_attribute ("naked", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

int
spu_initial_elimination_offset (int from, int to)
{
  int saved_regs_size = spu_saved_regs_size ();
  int sp_offset = 0;
  if (!current_function_is_leaf || crtl->outgoing_args_size
      || get_frame_size () || saved_regs_size)
    sp_offset = STACK_POINTER_OFFSET;
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return get_frame_size () + crtl->outgoing_args_size + sp_offset;
  else if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return get_frame_size ();
  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return sp_offset + crtl->outgoing_args_size
      + get_frame_size () + saved_regs_size + STACK_POINTER_OFFSET;
  else if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return get_frame_size () + saved_regs_size + sp_offset;
  else
    gcc_unreachable ();
}

rtx
spu_function_value (const_tree type, const_tree func ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = TYPE_MODE (type);
  int byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));

  /* Make sure small structs are left justified in a register. */
  if ((mode == BLKmode || (type && AGGREGATE_TYPE_P (type)))
      && byte_size <= UNITS_PER_WORD * MAX_REGISTER_RETURN && byte_size > 0)
    {
      enum machine_mode smode;
      rtvec v;
      int i;
      int nregs = (byte_size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      int n = byte_size / UNITS_PER_WORD;
      v = rtvec_alloc (nregs);
      for (i = 0; i < n; i++)
	{
	  RTVEC_ELT (v, i) = gen_rtx_EXPR_LIST (VOIDmode,
						gen_rtx_REG (TImode,
							     FIRST_RETURN_REGNUM
							     + i),
						GEN_INT (UNITS_PER_WORD * i));
	  byte_size -= UNITS_PER_WORD;
	}

      if (n < nregs)
	{
	  if (byte_size < 4)
	    byte_size = 4;
	  smode =
	    smallest_mode_for_size (byte_size * BITS_PER_UNIT, MODE_INT);
	  RTVEC_ELT (v, n) =
	    gen_rtx_EXPR_LIST (VOIDmode,
			       gen_rtx_REG (smode, FIRST_RETURN_REGNUM + n),
			       GEN_INT (UNITS_PER_WORD * n));
	}
      return gen_rtx_PARALLEL (mode, v);
    }
  return gen_rtx_REG (mode, FIRST_RETURN_REGNUM);
}

rtx
spu_function_arg (CUMULATIVE_ARGS cum,
		  enum machine_mode mode,
		  tree type, int named ATTRIBUTE_UNUSED)
{
  int byte_size;

  if (cum >= MAX_REGISTER_ARGS)
    return 0;

  byte_size = ((mode == BLKmode)
	       ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));

  /* The ABI does not allow parameters to be passed partially in
     reg and partially in stack. */
  if ((cum + (byte_size + 15) / 16) > MAX_REGISTER_ARGS)
    return 0;

  /* Make sure small structs are left justified in a register. */
  if ((mode == BLKmode || (type && AGGREGATE_TYPE_P (type)))
      && byte_size < UNITS_PER_WORD && byte_size > 0)
    {
      enum machine_mode smode;
      rtx gr_reg;
      if (byte_size < 4)
	byte_size = 4;
      smode = smallest_mode_for_size (byte_size * BITS_PER_UNIT, MODE_INT);
      gr_reg = gen_rtx_EXPR_LIST (VOIDmode,
				  gen_rtx_REG (smode, FIRST_ARG_REGNUM + cum),
				  const0_rtx);
      return gen_rtx_PARALLEL (mode, gen_rtvec (1, gr_reg));
    }
  else
    return gen_rtx_REG (mode, FIRST_ARG_REGNUM + cum);
}

/* Variable sized types are passed by reference.  */
static bool
spu_pass_by_reference (CUMULATIVE_ARGS * cum ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       const_tree type, bool named ATTRIBUTE_UNUSED)
{
  return type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST;
}


/* Var args. */

/* Create and return the va_list datatype.

   On SPU, va_list is an array type equivalent to

      typedef struct __va_list_tag
        {
            void *__args __attribute__((__aligned(16)));
            void *__skip __attribute__((__aligned(16)));
            
        } va_list[1];

   where __args points to the arg that will be returned by the next
   va_arg(), and __skip points to the previous stack frame such that
   when __args == __skip we should advance __args by 32 bytes. */
static tree
spu_build_builtin_va_list (void)
{
  tree f_args, f_skip, record, type_decl;
  bool owp;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);

  type_decl =
    build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_args = build_decl (FIELD_DECL, get_identifier ("__args"), ptr_type_node);
  f_skip = build_decl (FIELD_DECL, get_identifier ("__skip"), ptr_type_node);

  DECL_FIELD_CONTEXT (f_args) = record;
  DECL_ALIGN (f_args) = 128;
  DECL_USER_ALIGN (f_args) = 1;

  DECL_FIELD_CONTEXT (f_skip) = record;
  DECL_ALIGN (f_skip) = 128;
  DECL_USER_ALIGN (f_skip) = 1;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_args;
  TREE_CHAIN (f_args) = f_skip;

  /* We know this is being padded and we want it too.  It is an internal
     type so hide the warnings from the user. */
  owp = warn_padded;
  warn_padded = false;

  layout_type (record);

  warn_padded = owp;

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start by filling the va_list structure VALIST.
   NEXTARG points to the first anonymous stack argument.

   The following global variables are used to initialize
   the va_list structure:

     crtl->args.info;
       the CUMULATIVE_ARGS for this function

     crtl->args.arg_offset_rtx:
       holds the offset of the first anonymous stack argument
       (relative to the virtual arg pointer).  */

static void
spu_va_start (tree valist, rtx nextarg)
{
  tree f_args, f_skip;
  tree args, skip, t;

  f_args = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_skip = TREE_CHAIN (f_args);

  valist = build_va_arg_indirect_ref (valist);
  args =
    build3 (COMPONENT_REF, TREE_TYPE (f_args), valist, f_args, NULL_TREE);
  skip =
    build3 (COMPONENT_REF, TREE_TYPE (f_skip), valist, f_skip, NULL_TREE);

  /* Find the __args area.  */
  t = make_tree (TREE_TYPE (args), nextarg);
  if (crtl->args.pretend_args_size > 0)
    t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (args), t,
		size_int (-STACK_POINTER_OFFSET));
  t = build2 (MODIFY_EXPR, TREE_TYPE (args), args, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the __skip area.  */
  t = make_tree (TREE_TYPE (skip), virtual_incoming_args_rtx);
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (skip), t,
	      size_int (crtl->args.pretend_args_size
			 - STACK_POINTER_OFFSET));
  t = build2 (MODIFY_EXPR, TREE_TYPE (skip), skip, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Gimplify va_arg by updating the va_list structure 
   VALIST as required to retrieve an argument of type
   TYPE, and returning that argument. 
   
   ret = va_arg(VALIST, TYPE);

   generates code equivalent to:
   
    paddedsize = (sizeof(TYPE) + 15) & -16;
    if (VALIST.__args + paddedsize > VALIST.__skip
	&& VALIST.__args <= VALIST.__skip)
      addr = VALIST.__skip + 32;
    else
      addr = VALIST.__args;
    VALIST.__args = addr + paddedsize;
    ret = *(TYPE *)addr;
 */
static tree
spu_gimplify_va_arg_expr (tree valist, tree type, gimple_seq * pre_p,
			  gimple_seq * post_p ATTRIBUTE_UNUSED)
{
  tree f_args, f_skip;
  tree args, skip;
  HOST_WIDE_INT size, rsize;
  tree paddedsize, addr, tmp;
  bool pass_by_reference_p;

  f_args = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_skip = TREE_CHAIN (f_args);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  args =
    build3 (COMPONENT_REF, TREE_TYPE (f_args), valist, f_args, NULL_TREE);
  skip =
    build3 (COMPONENT_REF, TREE_TYPE (f_skip), valist, f_skip, NULL_TREE);

  addr = create_tmp_var (ptr_type_node, "va_arg");
  DECL_POINTER_ALIAS_SET (addr) = get_varargs_alias_set ();

  /* if an object is dynamically sized, a pointer to it is passed
     instead of the object itself. */
  pass_by_reference_p = spu_pass_by_reference (NULL, TYPE_MODE (type), type,
					       false);
  if (pass_by_reference_p)
    type = build_pointer_type (type);
  size = int_size_in_bytes (type);
  rsize = ((size + UNITS_PER_WORD - 1) / UNITS_PER_WORD) * UNITS_PER_WORD;

  /* build conditional expression to calculate addr. The expression
     will be gimplified later. */
  paddedsize = size_int (rsize);
  tmp = build2 (POINTER_PLUS_EXPR, ptr_type_node, unshare_expr (args), paddedsize);
  tmp = build2 (TRUTH_AND_EXPR, boolean_type_node,
		build2 (GT_EXPR, boolean_type_node, tmp, unshare_expr (skip)),
		build2 (LE_EXPR, boolean_type_node, unshare_expr (args),
		unshare_expr (skip)));

  tmp = build3 (COND_EXPR, ptr_type_node, tmp,
		build2 (POINTER_PLUS_EXPR, ptr_type_node, unshare_expr (skip),
			size_int (32)), unshare_expr (args));

  gimplify_assign (addr, tmp, pre_p);

  /* update VALIST.__args */
  tmp = build2 (POINTER_PLUS_EXPR, ptr_type_node, addr, paddedsize);
  gimplify_assign (unshare_expr (args), tmp, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (pass_by_reference_p)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}

/* Save parameter registers starting with the register that corresponds
   to the first unnamed parameters.  If the first unnamed parameter is
   in the stack then save no registers.  Set pretend_args_size to the
   amount of space needed to save the registers. */
void
spu_setup_incoming_varargs (CUMULATIVE_ARGS * cum, enum machine_mode mode,
			    tree type, int *pretend_size, int no_rtl)
{
  if (!no_rtl)
    {
      rtx tmp;
      int regno;
      int offset;
      int ncum = *cum;

      /* cum currently points to the last named argument, we want to
         start at the next argument. */
      FUNCTION_ARG_ADVANCE (ncum, mode, type, 1);

      offset = -STACK_POINTER_OFFSET;
      for (regno = ncum; regno < MAX_REGISTER_ARGS; regno++)
	{
	  tmp = gen_frame_mem (V4SImode,
			       plus_constant (virtual_incoming_args_rtx,
					      offset));
	  emit_move_insn (tmp,
			  gen_rtx_REG (V4SImode, FIRST_ARG_REGNUM + regno));
	  offset += 16;
	}
      *pretend_size = offset + STACK_POINTER_OFFSET;
    }
}

void
spu_conditional_register_usage (void)
{
  if (flag_pic)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
}

/* This is called any time we inspect the alignment of a register for
   addresses.  */
static int
reg_aligned_for_addr (rtx x)
{
  int regno =
    REGNO (x) < FIRST_PSEUDO_REGISTER ? ORIGINAL_REGNO (x) : REGNO (x);
  return REGNO_POINTER_ALIGN (regno) >= 128;
}

/* Encode symbol attributes (local vs. global, tls model) of a SYMBOL_REF
   into its SYMBOL_REF_FLAGS.  */
static void
spu_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  /* If a variable has a forced alignment to < 16 bytes, mark it with
     SYMBOL_FLAG_ALIGN1.  */
  if (TREE_CODE (decl) == VAR_DECL
      && DECL_USER_ALIGN (decl) && DECL_ALIGN (decl) < 128)
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= SYMBOL_FLAG_ALIGN1;
}

/* Return TRUE if we are certain the mem refers to a complete object
   which is both 16-byte aligned and padded to a 16-byte boundary.  This
   would make it safe to store with a single instruction. 
   We guarantee the alignment and padding for static objects by aligning
   all of them to 16-bytes. (DATA_ALIGNMENT and CONSTANT_ALIGNMENT.)
   FIXME: We currently cannot guarantee this for objects on the stack
   because assign_parm_setup_stack calls assign_stack_local with the
   alignment of the parameter mode and in that case the alignment never
   gets adjusted by LOCAL_ALIGNMENT. */
static int
store_with_one_insn_p (rtx mem)
{
  enum machine_mode mode = GET_MODE (mem);
  rtx addr = XEXP (mem, 0);
  if (mode == BLKmode)
    return 0;
  if (GET_MODE_SIZE (mode) >= 16)
    return 1;
  /* Only static objects. */
  if (GET_CODE (addr) == SYMBOL_REF)
    {
      /* We use the associated declaration to make sure the access is
         referring to the whole object.
         We check both MEM_EXPR and and SYMBOL_REF_DECL.  I'm not sure
         if it is necessary.  Will there be cases where one exists, and
         the other does not?  Will there be cases where both exist, but
         have different types?  */
      tree decl = MEM_EXPR (mem);
      if (decl
	  && TREE_CODE (decl) == VAR_DECL
	  && GET_MODE (mem) == TYPE_MODE (TREE_TYPE (decl)))
	return 1;
      decl = SYMBOL_REF_DECL (addr);
      if (decl
	  && TREE_CODE (decl) == VAR_DECL
	  && GET_MODE (mem) == TYPE_MODE (TREE_TYPE (decl)))
	return 1;
    }
  return 0;
}

/* Return 1 when the address is not valid for a simple load and store as
   required by the '_mov*' patterns.   We could make this less strict
   for loads, but we prefer mem's to look the same so they are more
   likely to be merged.  */
static int
address_needs_split (rtx mem)
{
  if (GET_MODE_SIZE (GET_MODE (mem)) < 16
      && (GET_MODE_SIZE (GET_MODE (mem)) < 4
	  || !(store_with_one_insn_p (mem)
	       || mem_is_padded_component_ref (mem))))
    return 1;

  return 0;
}

int
spu_expand_mov (rtx * ops, enum machine_mode mode)
{
  if (GET_CODE (ops[0]) == SUBREG && !valid_subreg (ops[0]))
    abort ();

  if (GET_CODE (ops[1]) == SUBREG && !valid_subreg (ops[1]))
    {
      rtx from = SUBREG_REG (ops[1]);
      enum machine_mode imode = int_mode_for_mode (GET_MODE (from));

      gcc_assert (GET_MODE_CLASS (mode) == MODE_INT
		  && GET_MODE_CLASS (imode) == MODE_INT
		  && subreg_lowpart_p (ops[1]));

      if (GET_MODE_SIZE (imode) < 4)
	imode = SImode;
      if (imode != GET_MODE (from))
	from = gen_rtx_SUBREG (imode, from, 0);

      if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (imode))
	{
	  enum insn_code icode = convert_optab_handler (trunc_optab, mode, imode)->insn_code;
	  emit_insn (GEN_FCN (icode) (ops[0], from));
	}
      else
	emit_insn (gen_extend_insn (ops[0], from, mode, imode, 1));
      return 1;
    }

  /* At least one of the operands needs to be a register. */
  if ((reload_in_progress | reload_completed) == 0
      && !register_operand (ops[0], mode) && !register_operand (ops[1], mode))
    {
      rtx temp = force_reg (mode, ops[1]);
      emit_move_insn (ops[0], temp);
      return 1;
    }
  if (reload_in_progress || reload_completed)
    {
      if (CONSTANT_P (ops[1]))
	return spu_split_immediate (ops);
      return 0;
    }

  /* Catch the SImode immediates greater than 0x7fffffff, and sign
     extend them. */
  if (GET_CODE (ops[1]) == CONST_INT)
    {
      HOST_WIDE_INT val = trunc_int_for_mode (INTVAL (ops[1]), mode);
      if (val != INTVAL (ops[1]))
	{
	  emit_move_insn (ops[0], GEN_INT (val));
	  return 1;
	}
    }
  if (MEM_P (ops[0]))
    return spu_split_store (ops);
  if (MEM_P (ops[1]))
    return spu_split_load (ops);

  return 0;
}

static void
spu_convert_move (rtx dst, rtx src)
{
  enum machine_mode mode = GET_MODE (dst);
  enum machine_mode int_mode = mode_for_size (GET_MODE_BITSIZE (mode), MODE_INT, 0);
  rtx reg;
  gcc_assert (GET_MODE (src) == TImode);
  reg = int_mode != mode ? gen_reg_rtx (int_mode) : dst;
  emit_insn (gen_rtx_SET (VOIDmode, reg,
	       gen_rtx_TRUNCATE (int_mode,
		 gen_rtx_LSHIFTRT (TImode, src,
		   GEN_INT (int_mode == DImode ? 64 : 96)))));
  if (int_mode != mode)
    {
      reg = simplify_gen_subreg (mode, reg, int_mode, 0);
      emit_move_insn (dst, reg);
    }
}

/* Load TImode values into DST0 and DST1 (when it is non-NULL) using
   the address from SRC and SRC+16.  Return a REG or CONST_INT that
   specifies how many bytes to rotate the loaded registers, plus any
   extra from EXTRA_ROTQBY.  The address and rotate amounts are
   normalized to improve merging of loads and rotate computations. */
static rtx
spu_expand_load (rtx dst0, rtx dst1, rtx src, int extra_rotby)
{
  rtx addr = XEXP (src, 0);
  rtx p0, p1, rot, addr0, addr1;
  int rot_amt;

  rot = 0;
  rot_amt = 0;

  if (MEM_ALIGN (src) >= 128)
    /* Address is already aligned; simply perform a TImode load.  */ ;
  else if (GET_CODE (addr) == PLUS)
    {
      /* 8 cases:
         aligned reg   + aligned reg     => lqx
         aligned reg   + unaligned reg   => lqx, rotqby
         aligned reg   + aligned const   => lqd
         aligned reg   + unaligned const => lqd, rotqbyi
         unaligned reg + aligned reg     => lqx, rotqby
         unaligned reg + unaligned reg   => lqx, a, rotqby (1 scratch)
         unaligned reg + aligned const   => lqd, rotqby
         unaligned reg + unaligned const -> not allowed by legitimate address
       */
      p0 = XEXP (addr, 0);
      p1 = XEXP (addr, 1);
      if (!reg_aligned_for_addr (p0))
	{
	  if (REG_P (p1) && !reg_aligned_for_addr (p1))
	    {
	      rot = gen_reg_rtx (SImode);
	      emit_insn (gen_addsi3 (rot, p0, p1));
	    }
	  else if (GET_CODE (p1) == CONST_INT && (INTVAL (p1) & 15))
	    {
	      if (INTVAL (p1) > 0
		  && REG_POINTER (p0)
		  && INTVAL (p1) * BITS_PER_UNIT
		     < REGNO_POINTER_ALIGN (REGNO (p0)))
		{
		  rot = gen_reg_rtx (SImode);
		  emit_insn (gen_addsi3 (rot, p0, p1));
		  addr = p0;
		}
	      else
		{
		  rtx x = gen_reg_rtx (SImode);
		  emit_move_insn (x, p1);
		  if (!spu_arith_operand (p1, SImode))
		    p1 = x;
		  rot = gen_reg_rtx (SImode);
		  emit_insn (gen_addsi3 (rot, p0, p1));
		  addr = gen_rtx_PLUS (Pmode, p0, x);
		}
	    }
	  else
	    rot = p0;
	}
      else
	{
	  if (GET_CODE (p1) == CONST_INT && (INTVAL (p1) & 15))
	    {
	      rot_amt = INTVAL (p1) & 15;
	      if (INTVAL (p1) & -16)
		{
		  p1 = GEN_INT (INTVAL (p1) & -16);
		  addr = gen_rtx_PLUS (SImode, p0, p1);
		}
	      else
		addr = p0;
	    }
	  else if (REG_P (p1) && !reg_aligned_for_addr (p1))
	    rot = p1;
	}
    }
  else if (REG_P (addr))
    {
      if (!reg_aligned_for_addr (addr))
	rot = addr;
    }
  else if (GET_CODE (addr) == CONST)
    {
      if (GET_CODE (XEXP (addr, 0)) == PLUS
	  && ALIGNED_SYMBOL_REF_P (XEXP (XEXP (addr, 0), 0))
	  && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT)
	{
	  rot_amt = INTVAL (XEXP (XEXP (addr, 0), 1));
	  if (rot_amt & -16)
	    addr = gen_rtx_CONST (Pmode,
				  gen_rtx_PLUS (Pmode,
						XEXP (XEXP (addr, 0), 0),
						GEN_INT (rot_amt & -16)));
	  else
	    addr = XEXP (XEXP (addr, 0), 0);
	}
      else
	{
	  rot = gen_reg_rtx (Pmode);
	  emit_move_insn (rot, addr);
	}
    }
  else if (GET_CODE (addr) == CONST_INT)
    {
      rot_amt = INTVAL (addr);
      addr = GEN_INT (rot_amt & -16);
    }
  else if (!ALIGNED_SYMBOL_REF_P (addr))
    {
      rot = gen_reg_rtx (Pmode);
      emit_move_insn (rot, addr);
    }

  rot_amt += extra_rotby;

  rot_amt &= 15;

  if (rot && rot_amt)
    {
      rtx x = gen_reg_rtx (SImode);
      emit_insn (gen_addsi3 (x, rot, GEN_INT (rot_amt)));
      rot = x;
      rot_amt = 0;
    }
  if (!rot && rot_amt)
    rot = GEN_INT (rot_amt);

  addr0 = copy_rtx (addr);
  addr0 = gen_rtx_AND (SImode, copy_rtx (addr), GEN_INT (-16));
  emit_insn (gen__movti (dst0, change_address (src, TImode, addr0)));

  if (dst1)
    {
      addr1 = plus_constant (copy_rtx (addr), 16);
      addr1 = gen_rtx_AND (SImode, addr1, GEN_INT (-16));
      emit_insn (gen__movti (dst1, change_address (src, TImode, addr1)));
    }

  return rot;
}

int
spu_split_load (rtx * ops)
{
  enum machine_mode mode = GET_MODE (ops[0]);
  rtx addr, load, rot;
  int rot_amt;

  if (GET_MODE_SIZE (mode) >= 16)
    return 0;

  addr = XEXP (ops[1], 0);
  gcc_assert (GET_CODE (addr) != AND);

  if (!address_needs_split (ops[1]))
    {
      ops[1] = change_address (ops[1], TImode, addr);
      load = gen_reg_rtx (TImode);
      emit_insn (gen__movti (load, ops[1]));
      spu_convert_move (ops[0], load);
      return 1;
    }

  rot_amt = GET_MODE_SIZE (mode) < 4 ? GET_MODE_SIZE (mode) - 4 : 0;

  load = gen_reg_rtx (TImode);
  rot = spu_expand_load (load, 0, ops[1], rot_amt);

  if (rot)
    emit_insn (gen_rotqby_ti (load, load, rot));

  spu_convert_move (ops[0], load);
  return 1;
}

int
spu_split_store (rtx * ops)
{
  enum machine_mode mode = GET_MODE (ops[0]);
  rtx reg;
  rtx addr, p0, p1, p1_lo, smem;
  int aform;
  int scalar;

  if (GET_MODE_SIZE (mode) >= 16)
    return 0;

  addr = XEXP (ops[0], 0);
  gcc_assert (GET_CODE (addr) != AND);

  if (!address_needs_split (ops[0]))
    {
      reg = gen_reg_rtx (TImode);
      emit_insn (gen_spu_convert (reg, ops[1]));
      ops[0] = change_address (ops[0], TImode, addr);
      emit_move_insn (ops[0], reg);
      return 1;
    }

  if (GET_CODE (addr) == PLUS)
    {
      /* 8 cases:
         aligned reg   + aligned reg     => lqx, c?x, shuf, stqx
         aligned reg   + unaligned reg   => lqx, c?x, shuf, stqx
         aligned reg   + aligned const   => lqd, c?d, shuf, stqx
         aligned reg   + unaligned const => lqd, c?d, shuf, stqx
         unaligned reg + aligned reg     => lqx, c?x, shuf, stqx
         unaligned reg + unaligned reg   => lqx, c?x, shuf, stqx
         unaligned reg + aligned const   => lqd, c?d, shuf, stqx
         unaligned reg + unaligned const -> lqx, c?d, shuf, stqx
       */
      aform = 0;
      p0 = XEXP (addr, 0);
      p1 = p1_lo = XEXP (addr, 1);
      if (REG_P (p0) && GET_CODE (p1) == CONST_INT)
	{
	  p1_lo = GEN_INT (INTVAL (p1) & 15);
	  if (reg_aligned_for_addr (p0))
	    {
	      p1 = GEN_INT (INTVAL (p1) & -16);
	      if (p1 == const0_rtx)
		addr = p0;
	      else
		addr = gen_rtx_PLUS (SImode, p0, p1);
	    }
	  else
	    {
	      rtx x = gen_reg_rtx (SImode);
	      emit_move_insn (x, p1);
	      addr = gen_rtx_PLUS (SImode, p0, x);
	    }
	}
    }
  else if (REG_P (addr))
    {
      aform = 0;
      p0 = addr;
      p1 = p1_lo = const0_rtx;
    }
  else
    {
      aform = 1;
      p0 = gen_rtx_REG (SImode, STACK_POINTER_REGNUM);
      p1 = 0;			/* aform doesn't use p1 */
      p1_lo = addr;
      if (ALIGNED_SYMBOL_REF_P (addr))
	p1_lo = const0_rtx;
      else if (GET_CODE (addr) == CONST
	       && GET_CODE (XEXP (addr, 0)) == PLUS
	       && ALIGNED_SYMBOL_REF_P (XEXP (XEXP (addr, 0), 0))
	       && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT)
	{
	  HOST_WIDE_INT v = INTVAL (XEXP (XEXP (addr, 0), 1));
	  if ((v & -16) != 0)
	    addr = gen_rtx_CONST (Pmode,
				  gen_rtx_PLUS (Pmode,
						XEXP (XEXP (addr, 0), 0),
						GEN_INT (v & -16)));
	  else
	    addr = XEXP (XEXP (addr, 0), 0);
	  p1_lo = GEN_INT (v & 15);
	}
      else if (GET_CODE (addr) == CONST_INT)
	{
	  p1_lo = GEN_INT (INTVAL (addr) & 15);
	  addr = GEN_INT (INTVAL (addr) & -16);
	}
      else
	{
	  p1_lo = gen_reg_rtx (SImode);
	  emit_move_insn (p1_lo, addr);
	}
    }

  reg = gen_reg_rtx (TImode);

  scalar = store_with_one_insn_p (ops[0]);
  if (!scalar)
    {
      /* We could copy the flags from the ops[0] MEM to mem here,
         We don't because we want this load to be optimized away if
         possible, and copying the flags will prevent that in certain
         cases, e.g. consider the volatile flag. */

      rtx pat = gen_reg_rtx (TImode);
      rtx lmem = change_address (ops[0], TImode, copy_rtx (addr));
      set_mem_alias_set (lmem, 0);
      emit_insn (gen_movti (reg, lmem));

      if (!p0 || reg_aligned_for_addr (p0))
	p0 = stack_pointer_rtx;
      if (!p1_lo)
	p1_lo = const0_rtx;

      emit_insn (gen_cpat (pat, p0, p1_lo, GEN_INT (GET_MODE_SIZE (mode))));
      emit_insn (gen_shufb (reg, ops[1], reg, pat));
    }
  else
    {
      if (GET_CODE (ops[1]) == REG)
	emit_insn (gen_spu_convert (reg, ops[1]));
      else if (GET_CODE (ops[1]) == SUBREG)
	emit_insn (gen_spu_convert (reg, SUBREG_REG (ops[1])));
      else
	abort ();
    }

  if (GET_MODE_SIZE (mode) < 4 && scalar)
    emit_insn (gen_ashlti3
	       (reg, reg, GEN_INT (32 - GET_MODE_BITSIZE (mode))));

  smem = change_address (ops[0], TImode, copy_rtx (addr));
  /* We can't use the previous alias set because the memory has changed
     size and can potentially overlap objects of other types.  */
  set_mem_alias_set (smem, 0);

  emit_insn (gen_movti (smem, reg));
  return 1;
}

/* Return TRUE if X is MEM which is a struct member reference
   and the member can safely be loaded and stored with a single
   instruction because it is padded. */
static int
mem_is_padded_component_ref (rtx x)
{
  tree t = MEM_EXPR (x);
  tree r;
  if (!t || TREE_CODE (t) != COMPONENT_REF)
    return 0;
  t = TREE_OPERAND (t, 1);
  if (!t || TREE_CODE (t) != FIELD_DECL
      || DECL_ALIGN (t) < 128 || AGGREGATE_TYPE_P (TREE_TYPE (t)))
    return 0;
  /* Only do this for RECORD_TYPEs, not UNION_TYPEs. */
  r = DECL_FIELD_CONTEXT (t);
  if (!r || TREE_CODE (r) != RECORD_TYPE)
    return 0;
  /* Make sure they are the same mode */
  if (GET_MODE (x) != TYPE_MODE (TREE_TYPE (t)))
    return 0;
  /* If there are no following fields then the field alignment assures
     the structure is padded to the alignment which means this field is
     padded too.  */
  if (TREE_CHAIN (t) == 0)
    return 1;
  /* If the following field is also aligned then this field will be
     padded. */
  t = TREE_CHAIN (t);
  if (TREE_CODE (t) == FIELD_DECL && DECL_ALIGN (t) >= 128)
    return 1;
  return 0;
}

/* Parse the -mfixed-range= option string.  */
static void
fix_range (const char *const_str)
{
  int i, first, last;
  char *str, *dash, *comma;
  
  /* str must be of the form REG1'-'REG2{,REG1'-'REG} where REG1 and
     REG2 are either register names or register numbers.  The effect
     of this option is to mark the registers in the range from REG1 to
     REG2 as ``fixed'' so they won't be used by the compiler.  */
  
  i = strlen (const_str);
  str = (char *) alloca (i + 1);
  memcpy (str, const_str, i + 1);
  
  while (1)
    {
      dash = strchr (str, '-');
      if (!dash)
	{
	  warning (0, "value of -mfixed-range must have form REG1-REG2");
	  return;
	}
      *dash = '\0';
      comma = strchr (dash + 1, ',');
      if (comma)
	*comma = '\0';
      
      first = decode_reg_name (str);
      if (first < 0)
	{
	  warning (0, "unknown register name: %s", str);
	  return;
	}
      
      last = decode_reg_name (dash + 1);
      if (last < 0)
	{
	  warning (0, "unknown register name: %s", dash + 1);
	  return;
	}
      
      *dash = '-';
      
      if (first > last)
	{
	  warning (0, "%s-%s is an empty range", str, dash + 1);
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

/* Return TRUE if x is a CONST_INT, CONST_DOUBLE or CONST_VECTOR that
   can be generated using the fsmbi instruction. */
int
fsmbi_const_p (rtx x)
{
  if (CONSTANT_P (x))
    {
      /* We can always choose TImode for CONST_INT because the high bits
         of an SImode will always be all 1s, i.e., valid for fsmbi. */
      enum immediate_class c = classify_immediate (x, TImode);
      return c == IC_FSMBI || (!epilogue_completed && c == IC_FSMBI2);
    }
  return 0;
}

/* Return TRUE if x is a CONST_INT, CONST_DOUBLE or CONST_VECTOR that
   can be generated using the cbd, chd, cwd or cdd instruction. */
int
cpat_const_p (rtx x, enum machine_mode mode)
{
  if (CONSTANT_P (x))
    {
      enum immediate_class c = classify_immediate (x, mode);
      return c == IC_CPAT;
    }
  return 0;
}

rtx
gen_cpat_const (rtx * ops)
{
  unsigned char dst[16];
  int i, offset, shift, isize;
  if (GET_CODE (ops[3]) != CONST_INT
      || GET_CODE (ops[2]) != CONST_INT
      || (GET_CODE (ops[1]) != CONST_INT
	  && GET_CODE (ops[1]) != REG))
    return 0;
  if (GET_CODE (ops[1]) == REG
      && (!REG_POINTER (ops[1])
	  || REGNO_POINTER_ALIGN (ORIGINAL_REGNO (ops[1])) < 128))
    return 0;

  for (i = 0; i < 16; i++)
    dst[i] = i + 16;
  isize = INTVAL (ops[3]);
  if (isize == 1)
    shift = 3;
  else if (isize == 2)
    shift = 2;
  else
    shift = 0;
  offset = (INTVAL (ops[2]) +
	    (GET_CODE (ops[1]) ==
	     CONST_INT ? INTVAL (ops[1]) : 0)) & 15;
  for (i = 0; i < isize; i++)
    dst[offset + i] = i + shift;
  return array_to_constant (TImode, dst);
}

/* Convert a CONST_INT, CONST_DOUBLE, or CONST_VECTOR into a 16 byte
   array.  Use MODE for CONST_INT's.  When the constant's mode is smaller
   than 16 bytes, the value is repeated across the rest of the array. */
void
constant_to_array (enum machine_mode mode, rtx x, unsigned char arr[16])
{
  HOST_WIDE_INT val;
  int i, j, first;

  memset (arr, 0, 16);
  mode = GET_MODE (x) != VOIDmode ? GET_MODE (x) : mode;
  if (GET_CODE (x) == CONST_INT
      || (GET_CODE (x) == CONST_DOUBLE
	  && (mode == SFmode || mode == DFmode)))
    {
      gcc_assert (mode != VOIDmode && mode != BLKmode);

      if (GET_CODE (x) == CONST_DOUBLE)
	val = const_double_to_hwint (x);
      else
	val = INTVAL (x);
      first = GET_MODE_SIZE (mode) - 1;
      for (i = first; i >= 0; i--)
	{
	  arr[i] = val & 0xff;
	  val >>= 8;
	}
      /* Splat the constant across the whole array. */
      for (j = 0, i = first + 1; i < 16; i++)
	{
	  arr[i] = arr[j];
	  j = (j == first) ? 0 : j + 1;
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      val = CONST_DOUBLE_LOW (x);
      for (i = 15; i >= 8; i--)
	{
	  arr[i] = val & 0xff;
	  val >>= 8;
	}
      val = CONST_DOUBLE_HIGH (x);
      for (i = 7; i >= 0; i--)
	{
	  arr[i] = val & 0xff;
	  val >>= 8;
	}
    }
  else if (GET_CODE (x) == CONST_VECTOR)
    {
      int units;
      rtx elt;
      mode = GET_MODE_INNER (mode);
      units = CONST_VECTOR_NUNITS (x);
      for (i = 0; i < units; i++)
	{
	  elt = CONST_VECTOR_ELT (x, i);
	  if (GET_CODE (elt) == CONST_INT || GET_CODE (elt) == CONST_DOUBLE)
	    {
	      if (GET_CODE (elt) == CONST_DOUBLE)
		val = const_double_to_hwint (elt);
	      else
		val = INTVAL (elt);
	      first = GET_MODE_SIZE (mode) - 1;
	      if (first + i * GET_MODE_SIZE (mode) > 16)
		abort ();
	      for (j = first; j >= 0; j--)
		{
		  arr[j + i * GET_MODE_SIZE (mode)] = val & 0xff;
		  val >>= 8;
		}
	    }
	}
    }
  else
    gcc_unreachable();
}

/* Convert a 16 byte array to a constant of mode MODE.  When MODE is
   smaller than 16 bytes, use the bytes that would represent that value
   in a register, e.g., for QImode return the value of arr[3].  */
rtx
array_to_constant (enum machine_mode mode, unsigned char arr[16])
{
  enum machine_mode inner_mode;
  rtvec v;
  int units, size, i, j, k;
  HOST_WIDE_INT val;

  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      j = GET_MODE_SIZE (mode);
      i = j < 4 ? 4 - j : 0;
      for (val = 0; i < j; i++)
	val = (val << 8) | arr[i];
      val = trunc_int_for_mode (val, mode);
      return GEN_INT (val);
    }

  if (mode == TImode)
    {
      HOST_WIDE_INT high;
      for (i = high = 0; i < 8; i++)
	high = (high << 8) | arr[i];
      for (i = 8, val = 0; i < 16; i++)
	val = (val << 8) | arr[i];
      return immed_double_const (val, high, TImode);
    }
  if (mode == SFmode)
    {
      val = (arr[0] << 24) | (arr[1] << 16) | (arr[2] << 8) | arr[3];
      val = trunc_int_for_mode (val, SImode);
      return hwint_to_const_double (SFmode, val);
    }
  if (mode == DFmode)
    {
      for (i = 0, val = 0; i < 8; i++)
	val = (val << 8) | arr[i];
      return hwint_to_const_double (DFmode, val);
    }

  if (!VECTOR_MODE_P (mode))
    abort ();

  units = GET_MODE_NUNITS (mode);
  size = GET_MODE_UNIT_SIZE (mode);
  inner_mode = GET_MODE_INNER (mode);
  v = rtvec_alloc (units);

  for (k = i = 0; i < units; ++i)
    {
      val = 0;
      for (j = 0; j < size; j++, k++)
	val = (val << 8) | arr[k];

      if (GET_MODE_CLASS (inner_mode) == MODE_FLOAT)
	RTVEC_ELT (v, i) = hwint_to_const_double (inner_mode, val);
      else
	RTVEC_ELT (v, i) = GEN_INT (trunc_int_for_mode (val, inner_mode));
    }
  if (k > 16)
    abort ();

  return gen_rtx_CONST_VECTOR (mode, v);
}

static void
reloc_diagnostic (rtx x)
{
  tree loc_decl, decl = 0;
  const char *msg;
  if (!flag_pic || !(TARGET_WARN_RELOC || TARGET_ERROR_RELOC))
    return;

  if (GET_CODE (x) == SYMBOL_REF)
    decl = SYMBOL_REF_DECL (x);
  else if (GET_CODE (x) == CONST
	   && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
    decl = SYMBOL_REF_DECL (XEXP (XEXP (x, 0), 0));

  /* SYMBOL_REF_DECL is not necessarily a DECL. */
  if (decl && !DECL_P (decl))
    decl = 0;

  /* We use last_assemble_variable_decl to get line information.  It's
     not always going to be right and might not even be close, but will
     be right for the more common cases. */
  if (!last_assemble_variable_decl || in_section == ctors_section)
    loc_decl = decl;
  else
    loc_decl = last_assemble_variable_decl;

  /* The decl could be a string constant.  */
  if (decl && DECL_P (decl))
    msg = "%Jcreating run-time relocation for %qD";
  else
    msg = "creating run-time relocation";

  if (TARGET_WARN_RELOC)
    warning (0, msg, loc_decl, decl);
  else
    error (msg, loc_decl, decl);
}

/* Hook into assemble_integer so we can generate an error for run-time
   relocations.  The SPU ABI disallows them. */
static bool
spu_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  /* By default run-time relocations aren't supported, but we allow them
     in case users support it in their own run-time loader.  And we provide
     a warning for those users that don't.  */
  if ((GET_CODE (x) == SYMBOL_REF)
      || GET_CODE (x) == LABEL_REF || GET_CODE (x) == CONST)
    reloc_diagnostic (x);

  return default_assemble_integer (x, size, aligned_p);
}

static void
spu_asm_globalize_label (FILE * file, const char *name)
{
  fputs ("\t.global\t", file);
  assemble_name (file, name);
  fputs ("\n", file);
}

static bool
spu_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED, int *total,
	       bool speed ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (x);
  int cost = COSTS_N_INSNS (2);

  /* Folding to a CONST_VECTOR will use extra space but there might
     be only a small savings in cycles.  We'd like to use a CONST_VECTOR
     only if it allows us to fold away multiple insns.  Changing the cost
     of a CONST_VECTOR here (or in CONST_COSTS) doesn't help though
     because this cost will only be compared against a single insn. 
     if (code == CONST_VECTOR)
       return (LEGITIMATE_CONSTANT_P(x)) ? cost : COSTS_N_INSNS(6);
   */

  /* Use defaults for float operations.  Not accurate but good enough. */
  if (mode == DFmode)
    {
      *total = COSTS_N_INSNS (13);
      return true;
    }
  if (mode == SFmode)
    {
      *total = COSTS_N_INSNS (6);
      return true;
    }
  switch (code)
    {
    case CONST_INT:
      if (satisfies_constraint_K (x))
	*total = 0;
      else if (INTVAL (x) >= -0x80000000ll && INTVAL (x) <= 0xffffffffll)
	*total = COSTS_N_INSNS (1);
      else
	*total = COSTS_N_INSNS (3);
      return true;

    case CONST:
      *total = COSTS_N_INSNS (3);
      return true;

    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (0);
      return true;

    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (5);
      return true;

    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case UNSIGNED_FIX:
      *total = COSTS_N_INSNS (7);
      return true;

    case PLUS:
      if (mode == TImode)
	{
	  *total = COSTS_N_INSNS (9);
	  return true;
	}
      break;

    case MULT:
      cost =
	GET_CODE (XEXP (x, 0)) ==
	REG ? COSTS_N_INSNS (12) : COSTS_N_INSNS (7);
      if (mode == SImode && GET_CODE (XEXP (x, 0)) == REG)
	{
	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
	      cost = COSTS_N_INSNS (14);
	      if ((val & 0xffff) == 0)
		cost = COSTS_N_INSNS (9);
	      else if (val > 0 && val < 0x10000)
		cost = COSTS_N_INSNS (11);
	    }
	}
      *total = cost;
      return true;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (20);
      return true;
    case ROTATE:
    case ROTATERT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      *total = COSTS_N_INSNS (4);
      return true;
    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_CONVERT)
	*total = COSTS_N_INSNS (0);
      else
	*total = COSTS_N_INSNS (4);
      return true;
    }
  /* Scale cost by mode size.  Except when initializing (cfun->decl == 0). */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) > GET_MODE_SIZE (SImode) && cfun && cfun->decl)
    cost = cost * (GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode))
      * (GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode));
  *total = cost;
  return true;
}

static enum machine_mode
spu_unwind_word_mode (void)
{
  return SImode;
}

/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */
static bool
spu_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  return decl && !TARGET_LARGE_MEM;
}

/* We need to correctly update the back chain pointer and the Available
   Stack Size (which is in the second slot of the sp register.) */
void
spu_allocate_stack (rtx op0, rtx op1)
{
  HOST_WIDE_INT v;
  rtx chain = gen_reg_rtx (V4SImode);
  rtx stack_bot = gen_frame_mem (V4SImode, stack_pointer_rtx);
  rtx sp = gen_reg_rtx (V4SImode);
  rtx splatted = gen_reg_rtx (V4SImode);
  rtx pat = gen_reg_rtx (TImode);

  /* copy the back chain so we can save it back again. */
  emit_move_insn (chain, stack_bot);

  op1 = force_reg (SImode, op1);

  v = 0x1020300010203ll;
  emit_move_insn (pat, immed_double_const (v, v, TImode));
  emit_insn (gen_shufb (splatted, op1, op1, pat));

  emit_insn (gen_spu_convert (sp, stack_pointer_rtx));
  emit_insn (gen_subv4si3 (sp, sp, splatted));

  if (flag_stack_check)
    {
      rtx avail = gen_reg_rtx(SImode);
      rtx result = gen_reg_rtx(SImode);
      emit_insn (gen_vec_extractv4si (avail, sp, GEN_INT (1)));
      emit_insn (gen_cgt_si(result, avail, GEN_INT (-1)));
      emit_insn (gen_spu_heq (result, GEN_INT(0) ));
    }

  emit_insn (gen_spu_convert (stack_pointer_rtx, sp));

  emit_move_insn (stack_bot, chain);

  emit_move_insn (op0, virtual_stack_dynamic_rtx);
}

void
spu_restore_stack_nonlocal (rtx op0 ATTRIBUTE_UNUSED, rtx op1)
{
  static unsigned char arr[16] =
    { 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3 };
  rtx temp = gen_reg_rtx (SImode);
  rtx temp2 = gen_reg_rtx (SImode);
  rtx temp3 = gen_reg_rtx (V4SImode);
  rtx temp4 = gen_reg_rtx (V4SImode);
  rtx pat = gen_reg_rtx (TImode);
  rtx sp = gen_rtx_REG (V4SImode, STACK_POINTER_REGNUM);

  /* Restore the backchain from the first word, sp from the second.  */
  emit_move_insn (temp2, adjust_address_nv (op1, SImode, 0));
  emit_move_insn (temp, adjust_address_nv (op1, SImode, 4));

  emit_move_insn (pat, array_to_constant (TImode, arr));

  /* Compute Available Stack Size for sp */
  emit_insn (gen_subsi3 (temp, temp, stack_pointer_rtx));
  emit_insn (gen_shufb (temp3, temp, temp, pat));

  /* Compute Available Stack Size for back chain */
  emit_insn (gen_subsi3 (temp2, temp2, stack_pointer_rtx));
  emit_insn (gen_shufb (temp4, temp2, temp2, pat));
  emit_insn (gen_addv4si3 (temp4, sp, temp4));

  emit_insn (gen_addv4si3 (sp, sp, temp3));
  emit_move_insn (gen_frame_mem (V4SImode, stack_pointer_rtx), temp4);
}

static void
spu_init_libfuncs (void)
{
  set_optab_libfunc (smul_optab, DImode, "__muldi3");
  set_optab_libfunc (sdiv_optab, DImode, "__divdi3");
  set_optab_libfunc (smod_optab, DImode, "__moddi3");
  set_optab_libfunc (udiv_optab, DImode, "__udivdi3");
  set_optab_libfunc (umod_optab, DImode, "__umoddi3");
  set_optab_libfunc (udivmod_optab, DImode, "__udivmoddi4");
  set_optab_libfunc (ffs_optab, DImode, "__ffsdi2");
  set_optab_libfunc (clz_optab, DImode, "__clzdi2");
  set_optab_libfunc (ctz_optab, DImode, "__ctzdi2");
  set_optab_libfunc (popcount_optab, DImode, "__popcountdi2");
  set_optab_libfunc (parity_optab, DImode, "__paritydi2");

  set_conv_libfunc (ufloat_optab, DFmode, SImode, "__float_unssidf");
  set_conv_libfunc (ufloat_optab, DFmode, DImode, "__float_unsdidf");

  set_optab_libfunc (smul_optab, TImode, "__multi3");
  set_optab_libfunc (sdiv_optab, TImode, "__divti3");
  set_optab_libfunc (smod_optab, TImode, "__modti3");
  set_optab_libfunc (udiv_optab, TImode, "__udivti3");
  set_optab_libfunc (umod_optab, TImode, "__umodti3");
  set_optab_libfunc (udivmod_optab, TImode, "__udivmodti4");
}

/* Make a subreg, stripping any existing subreg.  We could possibly just
   call simplify_subreg, but in this case we know what we want. */
rtx
spu_gen_subreg (enum machine_mode mode, rtx x)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (GET_MODE (x) == mode)
    return x;
  return gen_rtx_SUBREG (mode, x, 0);
}

static bool
spu_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  return (TYPE_MODE (type) == BLKmode
	  && ((type) == 0
	      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	      || int_size_in_bytes (type) >
	      (MAX_REGISTER_RETURN * UNITS_PER_WORD)));
}

/* Create the built-in types and functions */

enum spu_function_code
{
#define DEF_BUILTIN(fcode, icode, name, type, params) fcode,
#include "spu-builtins.def"
#undef DEF_BUILTIN
   NUM_SPU_BUILTINS
};

extern GTY(()) struct spu_builtin_description spu_builtins[NUM_SPU_BUILTINS];

struct spu_builtin_description spu_builtins[] = {
#define DEF_BUILTIN(fcode, icode, name, type, params) \
  {fcode, icode, name, type, params, NULL_TREE},
#include "spu-builtins.def"
#undef DEF_BUILTIN
};

static void
spu_init_builtins (void)
{
  struct spu_builtin_description *d;
  unsigned int i;

  V16QI_type_node = build_vector_type (intQI_type_node, 16);
  V8HI_type_node = build_vector_type (intHI_type_node, 8);
  V4SI_type_node = build_vector_type (intSI_type_node, 4);
  V2DI_type_node = build_vector_type (intDI_type_node, 2);
  V4SF_type_node = build_vector_type (float_type_node, 4);
  V2DF_type_node = build_vector_type (double_type_node, 2);

  unsigned_V16QI_type_node = build_vector_type (unsigned_intQI_type_node, 16);
  unsigned_V8HI_type_node = build_vector_type (unsigned_intHI_type_node, 8);
  unsigned_V4SI_type_node = build_vector_type (unsigned_intSI_type_node, 4);
  unsigned_V2DI_type_node = build_vector_type (unsigned_intDI_type_node, 2);

  spu_builtin_types[SPU_BTI_QUADWORD] = V16QI_type_node;

  spu_builtin_types[SPU_BTI_7] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_S7] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_U7] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_S10] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_S10_4] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_U14] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_16] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_S16] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_S16_2] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_U16] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_U16_2] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_U18] = global_trees[TI_INTSI_TYPE];

  spu_builtin_types[SPU_BTI_INTQI] = global_trees[TI_INTQI_TYPE];
  spu_builtin_types[SPU_BTI_INTHI] = global_trees[TI_INTHI_TYPE];
  spu_builtin_types[SPU_BTI_INTSI] = global_trees[TI_INTSI_TYPE];
  spu_builtin_types[SPU_BTI_INTDI] = global_trees[TI_INTDI_TYPE];
  spu_builtin_types[SPU_BTI_UINTQI] = global_trees[TI_UINTQI_TYPE];
  spu_builtin_types[SPU_BTI_UINTHI] = global_trees[TI_UINTHI_TYPE];
  spu_builtin_types[SPU_BTI_UINTSI] = global_trees[TI_UINTSI_TYPE];
  spu_builtin_types[SPU_BTI_UINTDI] = global_trees[TI_UINTDI_TYPE];

  spu_builtin_types[SPU_BTI_FLOAT] = global_trees[TI_FLOAT_TYPE];
  spu_builtin_types[SPU_BTI_DOUBLE] = global_trees[TI_DOUBLE_TYPE];

  spu_builtin_types[SPU_BTI_VOID] = global_trees[TI_VOID_TYPE];

  spu_builtin_types[SPU_BTI_PTR] =
    build_pointer_type (build_qualified_type
			(void_type_node,
			 TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE));

  /* For each builtin we build a new prototype.  The tree code will make
     sure nodes are shared. */
  for (i = 0, d = spu_builtins; i < NUM_SPU_BUILTINS; i++, d++)
    {
      tree p;
      char name[64];		/* build_function will make a copy. */
      int parm;

      if (d->name == 0)
	continue;

      /* Find last parm.  */
      for (parm = 1; d->parm[parm] != SPU_BTI_END_OF_PARAMS; parm++)
	;

      p = void_list_node;
      while (parm > 1)
	p = tree_cons (NULL_TREE, spu_builtin_types[d->parm[--parm]], p);

      p = build_function_type (spu_builtin_types[d->parm[0]], p);

      sprintf (name, "__builtin_%s", d->name);
      d->fndecl =
	add_builtin_function (name, p, END_BUILTINS + i, BUILT_IN_MD,
			      NULL, NULL_TREE);
      if (d->fcode == SPU_MASK_FOR_LOAD)
	TREE_READONLY (d->fndecl) = 1;	

      /* These builtins don't throw.  */
      TREE_NOTHROW (d->fndecl) = 1;
    }
}

void
spu_restore_stack_block (rtx op0 ATTRIBUTE_UNUSED, rtx op1)
{
  static unsigned char arr[16] =
    { 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3 };

  rtx temp = gen_reg_rtx (Pmode);
  rtx temp2 = gen_reg_rtx (V4SImode);
  rtx temp3 = gen_reg_rtx (V4SImode);
  rtx pat = gen_reg_rtx (TImode);
  rtx sp = gen_rtx_REG (V4SImode, STACK_POINTER_REGNUM);

  emit_move_insn (pat, array_to_constant (TImode, arr));

  /* Restore the sp.  */
  emit_move_insn (temp, op1);
  emit_move_insn (temp2, gen_frame_mem (V4SImode, stack_pointer_rtx));

  /* Compute available stack size for sp.  */
  emit_insn (gen_subsi3 (temp, temp, stack_pointer_rtx));
  emit_insn (gen_shufb (temp3, temp, temp, pat));

  emit_insn (gen_addv4si3 (sp, sp, temp3));
  emit_move_insn (gen_frame_mem (V4SImode, stack_pointer_rtx), temp2);
}

int
spu_safe_dma (HOST_WIDE_INT channel)
{
  return TARGET_SAFE_DMA && channel >= 21 && channel <= 27;
}

void
spu_builtin_splats (rtx ops[])
{
  enum machine_mode mode = GET_MODE (ops[0]);
  if (GET_CODE (ops[1]) == CONST_INT || GET_CODE (ops[1]) == CONST_DOUBLE)
    {
      unsigned char arr[16];
      constant_to_array (GET_MODE_INNER (mode), ops[1], arr);
      emit_move_insn (ops[0], array_to_constant (mode, arr));
    }
  else
    {
      rtx reg = gen_reg_rtx (TImode);
      rtx shuf;
      if (GET_CODE (ops[1]) != REG
	  && GET_CODE (ops[1]) != SUBREG)
	ops[1] = force_reg (GET_MODE_INNER (mode), ops[1]);
      switch (mode)
	{
	case V2DImode:
	case V2DFmode:
	  shuf =
	    immed_double_const (0x0001020304050607ll, 0x1011121314151617ll,
				TImode);
	  break;
	case V4SImode:
	case V4SFmode:
	  shuf =
	    immed_double_const (0x0001020300010203ll, 0x0001020300010203ll,
				TImode);
	  break;
	case V8HImode:
	  shuf =
	    immed_double_const (0x0203020302030203ll, 0x0203020302030203ll,
				TImode);
	  break;
	case V16QImode:
	  shuf =
	    immed_double_const (0x0303030303030303ll, 0x0303030303030303ll,
				TImode);
	  break;
	default:
	  abort ();
	}
      emit_move_insn (reg, shuf);
      emit_insn (gen_shufb (ops[0], ops[1], ops[1], reg));
    }
}

void
spu_builtin_extract (rtx ops[])
{
  enum machine_mode mode;
  rtx rot, from, tmp;

  mode = GET_MODE (ops[1]);

  if (GET_CODE (ops[2]) == CONST_INT)
    {
      switch (mode)
	{
	case V16QImode:
	  emit_insn (gen_vec_extractv16qi (ops[0], ops[1], ops[2]));
	  break;
	case V8HImode:
	  emit_insn (gen_vec_extractv8hi (ops[0], ops[1], ops[2]));
	  break;
	case V4SFmode:
	  emit_insn (gen_vec_extractv4sf (ops[0], ops[1], ops[2]));
	  break;
	case V4SImode:
	  emit_insn (gen_vec_extractv4si (ops[0], ops[1], ops[2]));
	  break;
	case V2DImode:
	  emit_insn (gen_vec_extractv2di (ops[0], ops[1], ops[2]));
	  break;
	case V2DFmode:
	  emit_insn (gen_vec_extractv2df (ops[0], ops[1], ops[2]));
	  break;
	default:
	  abort ();
	}
      return;
    }

  from = spu_gen_subreg (TImode, ops[1]);
  rot = gen_reg_rtx (TImode);
  tmp = gen_reg_rtx (SImode);

  switch (mode)
    {
    case V16QImode:
      emit_insn (gen_addsi3 (tmp, ops[2], GEN_INT (-3)));
      break;
    case V8HImode:
      emit_insn (gen_addsi3 (tmp, ops[2], ops[2]));
      emit_insn (gen_addsi3 (tmp, tmp, GEN_INT (-2)));
      break;
    case V4SFmode:
    case V4SImode:
      emit_insn (gen_ashlsi3 (tmp, ops[2], GEN_INT (2)));
      break;
    case V2DImode:
    case V2DFmode:
      emit_insn (gen_ashlsi3 (tmp, ops[2], GEN_INT (3)));
      break;
    default:
      abort ();
    }
  emit_insn (gen_rotqby_ti (rot, from, tmp));

  emit_insn (gen_spu_convert (ops[0], rot));
}

void
spu_builtin_insert (rtx ops[])
{
  enum machine_mode mode = GET_MODE (ops[0]);
  enum machine_mode imode = GET_MODE_INNER (mode);
  rtx mask = gen_reg_rtx (TImode);
  rtx offset;

  if (GET_CODE (ops[3]) == CONST_INT)
    offset = GEN_INT (INTVAL (ops[3]) * GET_MODE_SIZE (imode));
  else
    {
      offset = gen_reg_rtx (SImode);
      emit_insn (gen_mulsi3
		 (offset, ops[3], GEN_INT (GET_MODE_SIZE (imode))));
    }
  emit_insn (gen_cpat
	     (mask, stack_pointer_rtx, offset,
	      GEN_INT (GET_MODE_SIZE (imode))));
  emit_insn (gen_shufb (ops[0], ops[1], ops[2], mask));
}

void
spu_builtin_promote (rtx ops[])
{
  enum machine_mode mode, imode;
  rtx rot, from, offset;
  HOST_WIDE_INT pos;

  mode = GET_MODE (ops[0]);
  imode = GET_MODE_INNER (mode);

  from = gen_reg_rtx (TImode);
  rot = spu_gen_subreg (TImode, ops[0]);

  emit_insn (gen_spu_convert (from, ops[1]));

  if (GET_CODE (ops[2]) == CONST_INT)
    {
      pos = -GET_MODE_SIZE (imode) * INTVAL (ops[2]);
      if (GET_MODE_SIZE (imode) < 4)
	pos += 4 - GET_MODE_SIZE (imode);
      offset = GEN_INT (pos & 15);
    }
  else
    {
      offset = gen_reg_rtx (SImode);
      switch (mode)
	{
	case V16QImode:
	  emit_insn (gen_subsi3 (offset, GEN_INT (3), ops[2]));
	  break;
	case V8HImode:
	  emit_insn (gen_subsi3 (offset, GEN_INT (1), ops[2]));
	  emit_insn (gen_addsi3 (offset, offset, offset));
	  break;
	case V4SFmode:
	case V4SImode:
	  emit_insn (gen_subsi3 (offset, GEN_INT (0), ops[2]));
	  emit_insn (gen_ashlsi3 (offset, offset, GEN_INT (2)));
	  break;
	case V2DImode:
	case V2DFmode:
	  emit_insn (gen_ashlsi3 (offset, ops[2], GEN_INT (3)));
	  break;
	default:
	  abort ();
	}
    }
  emit_insn (gen_rotqby_ti (rot, from, offset));
}

void
spu_initialize_trampoline (rtx tramp, rtx fnaddr, rtx cxt)
{
  rtx shuf = gen_reg_rtx (V4SImode);
  rtx insn = gen_reg_rtx (V4SImode);
  rtx shufc;
  rtx insnc;
  rtx mem;

  fnaddr = force_reg (SImode, fnaddr);
  cxt = force_reg (SImode, cxt);

  if (TARGET_LARGE_MEM)
    {
      rtx rotl = gen_reg_rtx (V4SImode);
      rtx mask = gen_reg_rtx (V4SImode);
      rtx bi = gen_reg_rtx (SImode);
      unsigned char shufa[16] = {
	2, 3, 0, 1, 18, 19, 16, 17,
	0, 1, 2, 3, 16, 17, 18, 19
      };
      unsigned char insna[16] = {
	0x41, 0, 0, 79,
	0x41, 0, 0, STATIC_CHAIN_REGNUM,
	0x60, 0x80, 0, 79,
	0x60, 0x80, 0, STATIC_CHAIN_REGNUM
      };

      shufc = force_reg (TImode, array_to_constant (TImode, shufa));
      insnc = force_reg (V4SImode, array_to_constant (V4SImode, insna));

      emit_insn (gen_shufb (shuf, fnaddr, cxt, shufc));
      emit_insn (gen_vrotlv4si3 (rotl, shuf, spu_const (V4SImode, 7)));
      emit_insn (gen_movv4si (mask, spu_const (V4SImode, 0xffff << 7)));
      emit_insn (gen_selb (insn, insnc, rotl, mask));

      mem = memory_address (Pmode, tramp);
      emit_move_insn (gen_rtx_MEM (V4SImode, mem), insn);

      emit_move_insn (bi, GEN_INT (0x35000000 + (79 << 7)));
      mem = memory_address (Pmode, plus_constant (tramp, 16));
      emit_move_insn (gen_rtx_MEM (Pmode, mem), bi);
    }
  else
    {
      rtx scxt = gen_reg_rtx (SImode);
      rtx sfnaddr = gen_reg_rtx (SImode);
      unsigned char insna[16] = {
	0x42, 0, 0, STATIC_CHAIN_REGNUM,
	0x30, 0, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0
      };

      shufc = gen_reg_rtx (TImode);
      insnc = force_reg (V4SImode, array_to_constant (V4SImode, insna));

      /* By or'ing all of cxt with the ila opcode we are assuming cxt
	 fits 18 bits and the last 4 are zeros.  This will be true if
	 the stack pointer is initialized to 0x3fff0 at program start,
	 otherwise the ila instruction will be garbage. */

      emit_insn (gen_ashlsi3 (scxt, cxt, GEN_INT (7)));
      emit_insn (gen_ashlsi3 (sfnaddr, fnaddr, GEN_INT (5)));
      emit_insn (gen_cpat
		 (shufc, stack_pointer_rtx, GEN_INT (4), GEN_INT (4)));
      emit_insn (gen_shufb (shuf, sfnaddr, scxt, shufc));
      emit_insn (gen_iorv4si3 (insn, insnc, shuf));

      mem = memory_address (Pmode, tramp);
      emit_move_insn (gen_rtx_MEM (V4SImode, mem), insn);

    }
  emit_insn (gen_sync ());
}

void
spu_expand_sign_extend (rtx ops[])
{
  unsigned char arr[16];
  rtx pat = gen_reg_rtx (TImode);
  rtx sign, c;
  int i, last;
  last = GET_MODE (ops[0]) == DImode ? 7 : 15;
  if (GET_MODE (ops[1]) == QImode)
    {
      sign = gen_reg_rtx (HImode);
      emit_insn (gen_extendqihi2 (sign, ops[1]));
      for (i = 0; i < 16; i++)
	arr[i] = 0x12;
      arr[last] = 0x13;
    }
  else
    {
      for (i = 0; i < 16; i++)
	arr[i] = 0x10;
      switch (GET_MODE (ops[1]))
	{
	case HImode:
	  sign = gen_reg_rtx (SImode);
	  emit_insn (gen_extendhisi2 (sign, ops[1]));
	  arr[last] = 0x03;
	  arr[last - 1] = 0x02;
	  break;
	case SImode:
	  sign = gen_reg_rtx (SImode);
	  emit_insn (gen_ashrsi3 (sign, ops[1], GEN_INT (31)));
	  for (i = 0; i < 4; i++)
	    arr[last - i] = 3 - i;
	  break;
	case DImode:
	  sign = gen_reg_rtx (SImode);
	  c = gen_reg_rtx (SImode);
	  emit_insn (gen_spu_convert (c, ops[1]));
	  emit_insn (gen_ashrsi3 (sign, c, GEN_INT (31)));
	  for (i = 0; i < 8; i++)
	    arr[last - i] = 7 - i;
	  break;
	default:
	  abort ();
	}
    }
  emit_move_insn (pat, array_to_constant (TImode, arr));
  emit_insn (gen_shufb (ops[0], ops[1], sign, pat));
}

/* expand vector initialization. If there are any constant parts,
   load constant parts first. Then load any non-constant parts.  */
void
spu_expand_vector_init (rtx target, rtx vals)
{
  enum machine_mode mode = GET_MODE (target);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0;
  bool all_same = true;
  rtx first, x = NULL_RTX, first_constant = NULL_RTX;
  int i;

  first = XVECEXP (vals, 0, 0); 
  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x)
	    || GET_CODE (x) == CONST_DOUBLE
	    || GET_CODE (x) == CONST_FIXED))
	++n_var;
      else
	{
	  if (first_constant == NULL_RTX)
	    first_constant = x;
	}
      if (i > 0 && !rtx_equal_p (x, first))
	all_same = false;
    }

  /* if all elements are the same, use splats to repeat elements */
  if (all_same)
    {
      if (!CONSTANT_P (first)
	  && !register_operand (first, GET_MODE (x)))
	first = force_reg (GET_MODE (first), first);
      emit_insn (gen_spu_splats (target, first));
      return;
    }

  /* load constant parts */
  if (n_var != n_elts)
    {
      if (n_var == 0)
	{
	  emit_move_insn (target,
			  gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0)));
	}
      else
	{
	  rtx constant_parts_rtx = copy_rtx (vals);

	  gcc_assert (first_constant != NULL_RTX);
	  /* fill empty slots with the first constant, this increases
	     our chance of using splats in the recursive call below. */
	  for (i = 0; i < n_elts; ++i)
	    {
	      x = XVECEXP (constant_parts_rtx, 0, i);
	      if (!(CONST_INT_P (x)
		    || GET_CODE (x) == CONST_DOUBLE
		    || GET_CODE (x) == CONST_FIXED))
		XVECEXP (constant_parts_rtx, 0, i) = first_constant;
	    }

	  spu_expand_vector_init (target, constant_parts_rtx);
	}
    }

  /* load variable parts */
  if (n_var != 0)
    {
      rtx insert_operands[4];

      insert_operands[0] = target;
      insert_operands[2] = target;
      for (i = 0; i < n_elts; ++i)
	{
	  x = XVECEXP (vals, 0, i);
	  if (!(CONST_INT_P (x)
		|| GET_CODE (x) == CONST_DOUBLE
		|| GET_CODE (x) == CONST_FIXED))
	    {
	      if (!register_operand (x, GET_MODE (x)))
		x = force_reg (GET_MODE (x), x);
	      insert_operands[1] = x;
	      insert_operands[3] = GEN_INT (i);
	      spu_builtin_insert (insert_operands);
	    }
	}
    }
}

/* Return insn index for the vector compare instruction for given CODE,
   and DEST_MODE, OP_MODE. Return -1 if valid insn is not available.  */

static int
get_vec_cmp_insn (enum rtx_code code,
                  enum machine_mode dest_mode,
                  enum machine_mode op_mode)

{
  switch (code)
    {
    case EQ:
      if (dest_mode == V16QImode && op_mode == V16QImode)
        return CODE_FOR_ceq_v16qi;
      if (dest_mode == V8HImode && op_mode == V8HImode)
        return CODE_FOR_ceq_v8hi;
      if (dest_mode == V4SImode && op_mode == V4SImode)
        return CODE_FOR_ceq_v4si;
      if (dest_mode == V4SImode && op_mode == V4SFmode)
        return CODE_FOR_ceq_v4sf;
      if (dest_mode == V2DImode && op_mode == V2DFmode)
        return CODE_FOR_ceq_v2df;
      break;
    case GT:
      if (dest_mode == V16QImode && op_mode == V16QImode)
        return CODE_FOR_cgt_v16qi;
      if (dest_mode == V8HImode && op_mode == V8HImode)
        return CODE_FOR_cgt_v8hi;
      if (dest_mode == V4SImode && op_mode == V4SImode)
        return CODE_FOR_cgt_v4si;
      if (dest_mode == V4SImode && op_mode == V4SFmode)
        return CODE_FOR_cgt_v4sf;
      if (dest_mode == V2DImode && op_mode == V2DFmode)
        return CODE_FOR_cgt_v2df;
      break;
    case GTU:
      if (dest_mode == V16QImode && op_mode == V16QImode)
        return CODE_FOR_clgt_v16qi;
      if (dest_mode == V8HImode && op_mode == V8HImode)
        return CODE_FOR_clgt_v8hi;
      if (dest_mode == V4SImode && op_mode == V4SImode)
        return CODE_FOR_clgt_v4si;
      break;
    default:
      break;
    }
  return -1;
}

/* Emit vector compare for operands OP0 and OP1 using code RCODE.
   DMODE is expected destination mode. This is a recursive function.  */

static rtx
spu_emit_vector_compare (enum rtx_code rcode,
                         rtx op0, rtx op1,
                         enum machine_mode dmode)
{
  int vec_cmp_insn;
  rtx mask;
  enum machine_mode dest_mode;
  enum machine_mode op_mode = GET_MODE (op1);

  gcc_assert (GET_MODE (op0) == GET_MODE (op1));

  /* Floating point vector compare instructions uses destination V4SImode.
     Double floating point vector compare instructions uses destination V2DImode.
     Move destination to appropriate mode later.  */
  if (dmode == V4SFmode)
    dest_mode = V4SImode;
  else if (dmode == V2DFmode)
    dest_mode = V2DImode;
  else
    dest_mode = dmode;

  mask = gen_reg_rtx (dest_mode);
  vec_cmp_insn = get_vec_cmp_insn (rcode, dest_mode, op_mode);

  if (vec_cmp_insn == -1)
    {
      bool swap_operands = false;
      bool try_again = false;
      switch (rcode)
        {
        case LT:
          rcode = GT;
          swap_operands = true;
          try_again = true;
          break;
        case LTU:
          rcode = GTU;
          swap_operands = true;
          try_again = true;
          break;
        case NE:
          /* Treat A != B as ~(A==B).  */
          {
            enum insn_code nor_code;
            rtx eq_rtx = spu_emit_vector_compare (EQ, op0, op1, dest_mode);
            nor_code = optab_handler (one_cmpl_optab, (int)dest_mode)->insn_code;
            gcc_assert (nor_code != CODE_FOR_nothing);
            emit_insn (GEN_FCN (nor_code) (mask, eq_rtx));
            if (dmode != dest_mode)
              {
                rtx temp = gen_reg_rtx (dest_mode);
                convert_move (temp, mask, 0);
                return temp;
              }
            return mask;
          }
          break;
        case GE:
        case GEU:
        case LE:
        case LEU:
          /* Try GT/GTU/LT/LTU OR EQ */
          {
            rtx c_rtx, eq_rtx;
            enum insn_code ior_code;
            enum rtx_code new_code;

            switch (rcode)
              {
              case GE:  new_code = GT;  break;
              case GEU: new_code = GTU; break;
              case LE:  new_code = LT;  break;
              case LEU: new_code = LTU; break;
              default:
                gcc_unreachable ();
              }

            c_rtx = spu_emit_vector_compare (new_code, op0, op1, dest_mode);
            eq_rtx = spu_emit_vector_compare (EQ, op0, op1, dest_mode);

            ior_code = optab_handler (ior_optab, (int)dest_mode)->insn_code;
            gcc_assert (ior_code != CODE_FOR_nothing);
            emit_insn (GEN_FCN (ior_code) (mask, c_rtx, eq_rtx));
            if (dmode != dest_mode)
              {
                rtx temp = gen_reg_rtx (dest_mode);
                convert_move (temp, mask, 0);
                return temp;
              }
            return mask;
          }
          break;
        default:
          gcc_unreachable ();
        }

      /* You only get two chances.  */
      if (try_again)
          vec_cmp_insn = get_vec_cmp_insn (rcode, dest_mode, op_mode);

      gcc_assert (vec_cmp_insn != -1);

      if (swap_operands)
        {
          rtx tmp;
          tmp = op0;
          op0 = op1;
          op1 = tmp;
        }
    }

  emit_insn (GEN_FCN (vec_cmp_insn) (mask, op0, op1));
  if (dmode != dest_mode)
    {
      rtx temp = gen_reg_rtx (dest_mode);
      convert_move (temp, mask, 0);
      return temp;
    }
  return mask;
}


/* Emit vector conditional expression.
   DEST is destination. OP1 and OP2 are two VEC_COND_EXPR operands.
   CC_OP0 and CC_OP1 are the two operands for the relation operation COND.  */

int
spu_emit_vector_cond_expr (rtx dest, rtx op1, rtx op2,
                           rtx cond, rtx cc_op0, rtx cc_op1)
{   
  enum machine_mode dest_mode = GET_MODE (dest);
  enum rtx_code rcode = GET_CODE (cond);
  rtx mask;
    
  /* Get the vector mask for the given relational operations.  */
  mask = spu_emit_vector_compare (rcode, cc_op0, cc_op1, dest_mode);

  emit_insn(gen_selb (dest, op2, op1, mask));

  return 1;
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

static void
spu_check_builtin_parm (struct spu_builtin_description *d, rtx op, int p)
{
  HOST_WIDE_INT v = 0;
  int lsbits;
  /* Check the range of immediate operands. */
  if (p >= SPU_BTI_7 && p <= SPU_BTI_U18)
    {
      int range = p - SPU_BTI_7;

      if (!CONSTANT_P (op))
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
      else if (GET_CODE (op) == CONST_VECTOR
	       && GET_CODE (CONST_VECTOR_ELT (op, 0)) == CONST_INT)
	v = INTVAL (CONST_VECTOR_ELT (op, 0));

      /* The default for v is 0 which is valid in every range. */
      if (v < spu_builtin_range[range].low
	  || v > spu_builtin_range[range].high)
	error ("%s expects an integer literal in the range [%d, %d]. ("
	       HOST_WIDE_INT_PRINT_DEC ")",
	       d->name,
	       spu_builtin_range[range].low, spu_builtin_range[range].high,
	       v);

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
	  || (v & ((1 << lsbits) - 1)) != 0)
	warning (0, "%d least significant bits of %s are ignored.", lsbits,
		 d->name);
    }
}


static int
expand_builtin_args (struct spu_builtin_description *d, tree exp,
		     rtx target, rtx ops[])
{
  enum insn_code icode = d->icode;
  int i = 0, a;

  /* Expand the arguments into rtl. */

  if (d->parm[0] != SPU_BTI_VOID)
    ops[i++] = target;

  for (a = 0; d->parm[a+1] != SPU_BTI_END_OF_PARAMS; i++, a++)
    {
      tree arg = CALL_EXPR_ARG (exp, a);
      if (arg == 0)
	abort ();
      ops[i] = expand_expr (arg, NULL_RTX, VOIDmode, 0);
    }

  /* The insn pattern may have additional operands (SCRATCH).
     Return the number of actual non-SCRATCH operands.  */
  gcc_assert (i <= insn_data[icode].n_operands);
  return i;
}

static rtx
spu_expand_builtin_1 (struct spu_builtin_description *d,
		      tree exp, rtx target)
{
  rtx pat;
  rtx ops[8];
  enum insn_code icode = d->icode;
  enum machine_mode mode, tmode;
  int i, p;
  int n_operands;
  tree return_type;

  /* Set up ops[] with values from arglist. */
  n_operands = expand_builtin_args (d, exp, target, ops);

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

  if (d->fcode == SPU_MASK_FOR_LOAD)
    {
      enum machine_mode mode = insn_data[icode].operand[1].mode;
      tree arg;
      rtx addr, op, pat;

      /* get addr */
      arg = CALL_EXPR_ARG (exp, 0);
      gcc_assert (TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE);
      op = expand_expr (arg, NULL_RTX, Pmode, EXPAND_NORMAL);
      addr = memory_address (mode, op);

      /* negate addr */
      op = gen_reg_rtx (GET_MODE (addr));
      emit_insn (gen_rtx_SET (VOIDmode, op,
                 gen_rtx_NEG (GET_MODE (addr), addr)));
      op = gen_rtx_MEM (mode, op);

      pat = GEN_FCN (icode) (target, op);
      if (!pat) 
        return 0;
      emit_insn (pat);
      return target;
    }   

  /* Ignore align_hint, but still expand it's args in case they have
     side effects. */
  if (icode == CODE_FOR_spu_align_hint)
    return 0;

  /* Handle the rest of the operands. */
  for (p = 1; i < n_operands; i++, p++)
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

      spu_check_builtin_parm (d, ops[i], d->parm[p]);

      if (!(*insn_data[icode].operand[i].predicate) (ops[i], mode))
	ops[i] = spu_force_reg (mode, ops[i]);
    }

  switch (n_operands)
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
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl) - END_BUILTINS;
  struct spu_builtin_description *d;

  if (fcode < NUM_SPU_BUILTINS)
    {
      d = &spu_builtins[fcode];

      return spu_expand_builtin_1 (d, exp, target);
    }
  abort ();
}

/* Implement targetm.vectorize.builtin_mul_widen_even.  */
static tree
spu_builtin_mul_widen_even (tree type)
{
  switch (TYPE_MODE (type))
    {
    case V8HImode:
      if (TYPE_UNSIGNED (type))
	return spu_builtins[SPU_MULE_0].fndecl;
      else
	return spu_builtins[SPU_MULE_1].fndecl;
      break;
    default:
      return NULL_TREE;
    }
}

/* Implement targetm.vectorize.builtin_mul_widen_odd.  */
static tree
spu_builtin_mul_widen_odd (tree type)
{
  switch (TYPE_MODE (type))
    {
    case V8HImode:
      if (TYPE_UNSIGNED (type))
	return spu_builtins[SPU_MULO_1].fndecl;
      else
	return spu_builtins[SPU_MULO_0].fndecl; 
      break;
    default:
      return NULL_TREE;
    }
}

/* Implement targetm.vectorize.builtin_mask_for_load.  */
static tree
spu_builtin_mask_for_load (void)
{
  struct spu_builtin_description *d = &spu_builtins[SPU_MASK_FOR_LOAD];
  gcc_assert (d);
  return d->fndecl;
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int 
spu_builtin_vectorization_cost (bool runtime_test)
{
  /* If the branch of the runtime test is taken - i.e. - the vectorized
     version is skipped - this incurs a misprediction cost (because the
     vectorized version is expected to be the fall-through).  So we subtract
     the latency of a mispredicted branch from the costs that are incurred
     when the vectorized version is executed.  */
  if (runtime_test)
    return -19;
  else
    return 0;
}

/* Return true iff, data reference of TYPE can reach vector alignment (16)
   after applying N number of iterations.  This routine does not determine
   how may iterations are required to reach desired alignment.  */

static bool
spu_vector_alignment_reachable (const_tree type ATTRIBUTE_UNUSED, bool is_packed)
{
  if (is_packed)
    return false;

  /* All other types are naturally aligned.  */
  return true;
}

/* Implement targetm.vectorize.builtin_vec_perm.  */
tree
spu_builtin_vec_perm (tree type, tree *mask_element_type)
{
  struct spu_builtin_description *d;

  *mask_element_type = unsigned_char_type_node;

  switch (TYPE_MODE (type))
    {
    case V16QImode:
      if (TYPE_UNSIGNED (type))
        d = &spu_builtins[SPU_SHUFFLE_0];
      else
        d = &spu_builtins[SPU_SHUFFLE_1];
      break;

    case V8HImode:
      if (TYPE_UNSIGNED (type))
        d = &spu_builtins[SPU_SHUFFLE_2];
      else
        d = &spu_builtins[SPU_SHUFFLE_3];
      break;

    case V4SImode:
      if (TYPE_UNSIGNED (type))
        d = &spu_builtins[SPU_SHUFFLE_4];
      else
        d = &spu_builtins[SPU_SHUFFLE_5];
      break;

    case V2DImode:
      if (TYPE_UNSIGNED (type))
        d = &spu_builtins[SPU_SHUFFLE_6];
      else
        d = &spu_builtins[SPU_SHUFFLE_7];
      break;

    case V4SFmode:
      d = &spu_builtins[SPU_SHUFFLE_8];
      break;

    case V2DFmode:
      d = &spu_builtins[SPU_SHUFFLE_9];
      break;

    default:
      return NULL_TREE;
    }

  gcc_assert (d);
  return d->fndecl;
}

/* Count the total number of instructions in each pipe and return the
   maximum, which is used as the Minimum Iteration Interval (MII)
   in the modulo scheduler.  get_pipe() will return -2, -1, 0, or 1.
   -2 are instructions that can go in pipe0 or pipe1.  */
static int
spu_sms_res_mii (struct ddg *g)
{
  int i;
  unsigned t[4] = {0, 0, 0, 0};

  for (i = 0; i < g->num_nodes; i++)
    {
      rtx insn = g->nodes[i].insn;
      int p = get_pipe (insn) + 2;

      assert (p >= 0);
      assert (p < 4);

      t[p]++;
      if (dump_file && INSN_P (insn))
            fprintf (dump_file, "i%d %s %d %d\n",
                     INSN_UID (insn),
                     insn_data[INSN_CODE(insn)].name,
                     p, t[p]);
    }
  if (dump_file)
    fprintf (dump_file, "%d %d %d %d\n", t[0], t[1], t[2], t[3]);

  return MAX ((t[0] + t[2] + t[3] + 1) / 2, MAX (t[2], t[3]));
}


void
spu_init_expanders (void)
{
  if (cfun)
    {
      rtx r0, r1;
      /* HARD_FRAME_REGISTER is only 128 bit aligned when
         frame_pointer_needed is true.  We don't know that until we're
         expanding the prologue. */
      REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = 8;

      /* A number of passes use LAST_VIRTUAL_REGISTER+1 and
	 LAST_VIRTUAL_REGISTER+2 to test the back-end.  We want them
	 to be treated as aligned, so generate them here. */
      r0 = gen_reg_rtx (SImode);
      r1 = gen_reg_rtx (SImode);
      mark_reg_pointer (r0, 128);
      mark_reg_pointer (r1, 128);
      gcc_assert (REGNO (r0) == LAST_VIRTUAL_REGISTER + 1
		  && REGNO (r1) == LAST_VIRTUAL_REGISTER + 2);
    }
}

static enum machine_mode
spu_libgcc_cmp_return_mode (void)
{

/* For SPU word mode is TI mode so it is better to use SImode
   for compare returns.  */
  return SImode;
}

static enum machine_mode
spu_libgcc_shift_count_mode (void)
{
/* For SPU word mode is TI mode so it is better to use SImode
   for shift counts.  */
  return SImode;
}

/* An early place to adjust some flags after GCC has finished processing
 * them. */
static void
asm_file_start (void)
{
  /* Variable tracking should be run after all optimizations which
     change order of insns.  It also needs a valid CFG. */
  spu_flag_var_tracking = flag_var_tracking;
  flag_var_tracking = 0;

  default_file_start ();
}

/* Implement targetm.section_type_flags.  */
static unsigned int
spu_section_type_flags (tree decl, const char *name, int reloc)
{
  /* .toe needs to have type @nobits.  */
  if (strcmp (name, ".toe") == 0)
    return SECTION_BSS;
  return default_section_type_flags (decl, name, reloc);
}

/* Generate a constant or register which contains 2^SCALE.  We assume
   the result is valid for MODE.  Currently, MODE must be V4SFmode and
   SCALE must be SImode. */
rtx
spu_gen_exp2 (enum machine_mode mode, rtx scale)
{
  gcc_assert (mode == V4SFmode);
  gcc_assert (GET_MODE (scale) == SImode || GET_CODE (scale) == CONST_INT);
  if (GET_CODE (scale) != CONST_INT)
    {
      /* unsigned int exp = (127 + scale) << 23;
	__vector float m = (__vector float) spu_splats (exp); */
      rtx reg = force_reg (SImode, scale);
      rtx exp = gen_reg_rtx (SImode);
      rtx mul = gen_reg_rtx (mode);
      emit_insn (gen_addsi3 (exp, reg, GEN_INT (127)));
      emit_insn (gen_ashlsi3 (exp, exp, GEN_INT (23)));
      emit_insn (gen_spu_splats (mul, gen_rtx_SUBREG (GET_MODE_INNER (mode), exp, 0)));
      return mul;
    }
  else
    {
      HOST_WIDE_INT exp = 127 + INTVAL (scale);
      unsigned char arr[16];
      arr[0] = arr[4] = arr[8] = arr[12] = exp >> 1;
      arr[1] = arr[5] = arr[9] = arr[13] = exp << 7;
      arr[2] = arr[6] = arr[10] = arr[14] = 0;
      arr[3] = arr[7] = arr[11] = arr[15] = 0;
      return array_to_constant (mode, arr);
    }
}

/* After reload, just change the convert into a move instruction
   or a dead instruction. */
void
spu_split_convert (rtx ops[])
{
  if (REGNO (ops[0]) == REGNO (ops[1]))
    emit_note (NOTE_INSN_DELETED);
  else
    {
      /* Use TImode always as this might help hard reg copyprop.  */
      rtx op0 = gen_rtx_REG (TImode, REGNO (ops[0]));
      rtx op1 = gen_rtx_REG (TImode, REGNO (ops[1]));
      emit_insn (gen_move_insn (op0, op1));
    }
}

#include "gt-spu.h"

