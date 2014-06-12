/* Definitions for Toshiba Media Processor
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "tree.h"
#include "varasm.h"
#include "calls.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "except.h"
#include "function.h"
#include "optabs.h"
#include "reload.h"
#include "tm_p.h"
#include "ggc.h"
#include "diagnostic-core.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "df.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "vec.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "opts.h"
#include "dumpfile.h"
#include "builtins.h"

/* Structure of this file:

 + Command Line Option Support
 + Pattern support - constraints, predicates, expanders
 + Reload Support
 + Costs
 + Functions to save and restore machine-specific function data.
 + Frame/Epilog/Prolog Related
 + Operand Printing
 + Function args in registers
 + Handle pipeline hazards
 + Handle attributes
 + Trampolines
 + Machine-dependent Reorg
 + Builtins.  */

/* Symbol encodings:

   Symbols are encoded as @ <char> . <name> where <char> is one of these:

   b - based
   t - tiny
   n - near
   f - far
   i - io, near
   I - io, far
   c - cb (control bus)  */

struct GTY(()) machine_function
{
  int mep_frame_pointer_needed;
  
  /* For varargs. */
  int arg_regs_to_save;
  int regsave_filler;
  int frame_filler;
  int frame_locked;
  
  /* Records __builtin_return address.  */
  rtx eh_stack_adjust;
  
  int reg_save_size;
  int reg_save_slot[FIRST_PSEUDO_REGISTER];
  unsigned char reg_saved[FIRST_PSEUDO_REGISTER];
  
  /* 2 if the current function has an interrupt attribute, 1 if not, 0
     if unknown.  This is here because resource.c uses EPILOGUE_USES
     which needs it.  */
  int interrupt_handler;
  
  /* Likewise, for disinterrupt attribute.  */
  int disable_interrupts;

  /* Number of doloop tags used so far.  */
  int doloop_tags;

  /* True if the last tag was allocated to a doloop_end.  */
  bool doloop_tag_from_end;

  /* True if reload changes $TP.  */
  bool reload_changes_tp;

  /* 2 if there are asm()s without operands, 1 if not, 0 if unknown.
     We only set this if the function is an interrupt handler.  */
  int asms_without_operands;
};

#define MEP_CONTROL_REG(x) \
  (GET_CODE (x) == REG && ANY_CONTROL_REGNO_P (REGNO (x)))

static GTY(()) section * based_section;
static GTY(()) section * tinybss_section;
static GTY(()) section * far_section;
static GTY(()) section * farbss_section;
static GTY(()) section * frodata_section;
static GTY(()) section * srodata_section;

static GTY(()) section * vtext_section;
static GTY(()) section * vftext_section;
static GTY(()) section * ftext_section;

static void mep_set_leaf_registers (int);
static bool symbol_p (rtx);
static bool symbolref_p (rtx);
static void encode_pattern_1 (rtx);
static void encode_pattern (rtx);
static bool const_in_range (rtx, int, int);
static void mep_rewrite_mult (rtx, rtx);
static void mep_rewrite_mulsi3 (rtx, rtx, rtx, rtx);
static void mep_rewrite_maddsi3 (rtx, rtx, rtx, rtx, rtx);
static bool mep_reuse_lo_p_1 (rtx, rtx, rtx, bool);
static bool move_needs_splitting (rtx, rtx, enum machine_mode);
static bool mep_expand_setcc_1 (enum rtx_code, rtx, rtx, rtx);
static bool mep_nongeneral_reg (rtx);
static bool mep_general_copro_reg (rtx);
static bool mep_nonregister (rtx);
static struct machine_function* mep_init_machine_status (void);
static rtx mep_tp_rtx (void);
static rtx mep_gp_rtx (void);
static bool mep_interrupt_p (void);
static bool mep_disinterrupt_p (void);
static bool mep_reg_set_p (rtx, rtx);
static bool mep_reg_set_in_function (int);
static bool mep_interrupt_saved_reg (int);
static bool mep_call_saves_register (int);
static rtx F (rtx);
static void add_constant (int, int, int, int);
static rtx maybe_dead_move (rtx, rtx, bool);
static void mep_reload_pointer (int, const char *);
static void mep_start_function (FILE *, HOST_WIDE_INT);
static bool mep_function_ok_for_sibcall (tree, tree);
static int unique_bit_in (HOST_WIDE_INT);
static int bit_size_for_clip (HOST_WIDE_INT);
static int bytesize (const_tree, enum machine_mode);
static tree mep_validate_based_tiny (tree *, tree, tree, int, bool *);
static tree mep_validate_near_far (tree *, tree, tree, int, bool *);
static tree mep_validate_disinterrupt (tree *, tree, tree, int, bool *);
static tree mep_validate_interrupt (tree *, tree, tree, int, bool *);
static tree mep_validate_io_cb (tree *, tree, tree, int, bool *);
static tree mep_validate_vliw (tree *, tree, tree, int, bool *);
static bool mep_function_attribute_inlinable_p (const_tree);
static bool mep_can_inline_p (tree, tree);
static bool mep_lookup_pragma_disinterrupt (const char *);
static int mep_multiple_address_regions (tree, bool);
static int mep_attrlist_to_encoding (tree, tree);
static void mep_insert_attributes (tree, tree *);
static void mep_encode_section_info (tree, rtx, int);
static section * mep_select_section (tree, int, unsigned HOST_WIDE_INT);
static void mep_unique_section (tree, int);
static unsigned int mep_section_type_flags (tree, const char *, int);
static void mep_asm_named_section (const char *, unsigned int, tree);
static bool mep_mentioned_p (rtx, rtx, int);
static void mep_reorg_regmove (rtx);
static rtx mep_insert_repeat_label_last (rtx, rtx, bool, bool);
static void mep_reorg_repeat (rtx);
static bool mep_invertable_branch_p (rtx);
static void mep_invert_branch (rtx, rtx);
static void mep_reorg_erepeat (rtx);
static void mep_jmp_return_reorg (rtx);
static void mep_reorg_addcombine (rtx);
static void mep_reorg (void);
static void mep_init_intrinsics (void);
static void mep_init_builtins (void);
static void mep_intrinsic_unavailable (int);
static bool mep_get_intrinsic_insn (int, const struct cgen_insn **);
static bool mep_get_move_insn (int, const struct cgen_insn **);
static rtx mep_convert_arg (enum machine_mode, rtx);
static rtx mep_convert_regnum (const struct cgen_regnum_operand *, rtx);
static rtx mep_legitimize_arg (const struct insn_operand_data *, rtx, int);
static void mep_incompatible_arg (const struct insn_operand_data *, rtx, int, tree);
static rtx mep_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
static int mep_adjust_cost (rtx, rtx, rtx, int);
static int mep_issue_rate (void);
static rtx mep_find_ready_insn (rtx *, int, enum attr_slot, int);
static void mep_move_ready_insn (rtx *, int, rtx);
static int mep_sched_reorder (FILE *, int, rtx *, int *, int);
static rtx mep_make_bundle (rtx, rtx);
static void mep_bundle_insns (rtx);
static bool mep_rtx_cost (rtx, int, int, int, int *, bool);
static int mep_address_cost (rtx, enum machine_mode, addr_space_t, bool);
static void mep_setup_incoming_varargs (cumulative_args_t, enum machine_mode,
					tree, int *, int);
static bool mep_pass_by_reference (cumulative_args_t cum, enum machine_mode,
				   const_tree, bool);
static rtx mep_function_arg (cumulative_args_t, enum machine_mode,
			     const_tree, bool);
static void mep_function_arg_advance (cumulative_args_t, enum machine_mode,
				      const_tree, bool);
static bool mep_vector_mode_supported_p (enum machine_mode);
static rtx  mep_allocate_initial_value (rtx);
static void mep_asm_init_sections (void);
static int mep_comp_type_attributes (const_tree, const_tree);
static bool mep_narrow_volatile_bitfield (void);
static rtx mep_expand_builtin_saveregs (void);
static tree mep_build_builtin_va_list (void);
static void mep_expand_va_start (tree, rtx);
static tree mep_gimplify_va_arg_expr (tree, tree, gimple_seq *, gimple_seq *);
static bool mep_can_eliminate (const int, const int);
static void mep_conditional_register_usage (void);
static void mep_trampoline_init (rtx, tree, rtx);

#define WANT_GCC_DEFINITIONS
#include "mep-intrin.h"
#undef WANT_GCC_DEFINITIONS


/* Command Line Option Support.  */

char mep_leaf_registers [FIRST_PSEUDO_REGISTER];

/* True if we can use cmov instructions to move values back and forth
   between core and coprocessor registers.  */
bool mep_have_core_copro_moves_p;

/* True if we can use cmov instructions (or a work-alike) to move
   values between coprocessor registers.  */
bool mep_have_copro_copro_moves_p;

/* A table of all coprocessor instructions that can act like
   a coprocessor-to-coprocessor cmov.  */
static const int mep_cmov_insns[] = {
  mep_cmov,
  mep_cpmov,
  mep_fmovs,
  mep_caddi3,
  mep_csubi3,
  mep_candi3,
  mep_cori3,
  mep_cxori3,
  mep_cand3,
  mep_cor3
};


static void
mep_set_leaf_registers (int enable)
{
  int i;

  if (mep_leaf_registers[0] != enable)
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      mep_leaf_registers[i] = enable;
}

static void
mep_conditional_register_usage (void)
{
  int i;

  if (!TARGET_OPT_MULT && !TARGET_OPT_DIV)
    {
      fixed_regs[HI_REGNO] = 1;
      fixed_regs[LO_REGNO] = 1;
      call_used_regs[HI_REGNO] = 1;
      call_used_regs[LO_REGNO] = 1;
    }

  for (i = FIRST_SHADOW_REGISTER; i <= LAST_SHADOW_REGISTER; i++)
    global_regs[i] = 1;
}

static void
mep_option_override (void)
{
  unsigned int i;
  int j;
  cl_deferred_option *opt;
  vec<cl_deferred_option> *v = (vec<cl_deferred_option> *) mep_deferred_options;

  if (v)
    FOR_EACH_VEC_ELT (*v, i, opt)
      {
	switch (opt->opt_index)
	  {
	  case OPT_mivc2:
	    for (j = 0; j < 32; j++)
	      fixed_regs[j + 48] = 0;
	    for (j = 0; j < 32; j++)
	      call_used_regs[j + 48] = 1;
	    for (j = 6; j < 8; j++)
	      call_used_regs[j + 48] = 0;

#define RN(n,s) reg_names[FIRST_CCR_REGNO + n] = s
	    RN (0, "$csar0");
	    RN (1, "$cc");
	    RN (4, "$cofr0");
	    RN (5, "$cofr1");
	    RN (6, "$cofa0");
	    RN (7, "$cofa1");
	    RN (15, "$csar1");

	    RN (16, "$acc0_0");
	    RN (17, "$acc0_1");
	    RN (18, "$acc0_2");
	    RN (19, "$acc0_3");
	    RN (20, "$acc0_4");
	    RN (21, "$acc0_5");
	    RN (22, "$acc0_6");
	    RN (23, "$acc0_7");

	    RN (24, "$acc1_0");
	    RN (25, "$acc1_1");
	    RN (26, "$acc1_2");
	    RN (27, "$acc1_3");
	    RN (28, "$acc1_4");
	    RN (29, "$acc1_5");
	    RN (30, "$acc1_6");
	    RN (31, "$acc1_7");
#undef RN
	    break;

	  default:
	    gcc_unreachable ();
	  }
      }

  if (flag_pic == 1)
    warning (OPT_fpic, "-fpic is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "-fPIC is not supported");
  if (TARGET_S && TARGET_M)
    error ("only one of -ms and -mm may be given");
  if (TARGET_S && TARGET_L)
    error ("only one of -ms and -ml may be given");
  if (TARGET_M && TARGET_L)
    error ("only one of -mm and -ml may be given");
  if (TARGET_S && global_options_set.x_mep_tiny_cutoff)
    error ("only one of -ms and -mtiny= may be given");
  if (TARGET_M && global_options_set.x_mep_tiny_cutoff)
    error ("only one of -mm and -mtiny= may be given");
  if (TARGET_OPT_CLIP && ! TARGET_OPT_MINMAX)
    warning (0, "-mclip currently has no effect without -mminmax");

  if (mep_const_section)
    {
      if (strcmp (mep_const_section, "tiny") != 0
	  && strcmp (mep_const_section, "near") != 0
	  && strcmp (mep_const_section, "far") != 0)
	error ("-mc= must be -mc=tiny, -mc=near, or -mc=far");
    }

  if (TARGET_S)
    mep_tiny_cutoff = 65536;
  if (TARGET_M)
    mep_tiny_cutoff = 0;
  if (TARGET_L && ! global_options_set.x_mep_tiny_cutoff)
    mep_tiny_cutoff = 0;

  if (TARGET_64BIT_CR_REGS)
    flag_split_wide_types = 0;

  init_machine_status = mep_init_machine_status;
  mep_init_intrinsics ();
}
  
/* Pattern Support - constraints, predicates, expanders.  */

/* MEP has very few instructions that can refer to the span of
   addresses used by symbols, so it's common to check for them.  */

static bool
symbol_p (rtx x)
{
  int c = GET_CODE (x);

  return (c == CONST_INT
	  || c == CONST
	  || c == SYMBOL_REF);
}

static bool
symbolref_p (rtx x)
{
  int c;

  if (GET_CODE (x) != MEM)
    return false;

  c = GET_CODE (XEXP (x, 0));
  return (c == CONST_INT
	  || c == CONST
	  || c == SYMBOL_REF);
}

/* static const char *reg_class_names[] = REG_CLASS_NAMES; */

#define GEN_REG(R, STRICT)				\
  (GR_REGNO_P (R)					\
   || (!STRICT						\
       && ((R) == ARG_POINTER_REGNUM			\
	   || (R) >= FIRST_PSEUDO_REGISTER)))

static char pattern[12], *patternp;
static GTY(()) rtx patternr[12];
#define RTX_IS(x) (strcmp (pattern, x) == 0)

static void
encode_pattern_1 (rtx x)
{
  int i;

  if (patternp == pattern + sizeof (pattern) - 2)
    {
      patternp[-1] = '?';
      return;
    }

  patternr[patternp-pattern] = x;

  switch (GET_CODE (x))
    {
    case REG:
      *patternp++ = 'r';
      break;
    case MEM:
      *patternp++ = 'm';
    case CONST:
      encode_pattern_1 (XEXP(x, 0));
      break;
    case PLUS:
      *patternp++ = '+';
      encode_pattern_1 (XEXP(x, 0));
      encode_pattern_1 (XEXP(x, 1));
      break;
    case LO_SUM:
      *patternp++ = 'L';
      encode_pattern_1 (XEXP(x, 0));
      encode_pattern_1 (XEXP(x, 1));
      break;
    case HIGH:
      *patternp++ = 'H';
      encode_pattern_1 (XEXP(x, 0));
      break;
    case SYMBOL_REF:
      *patternp++ = 's';
      break;
    case LABEL_REF:
      *patternp++ = 'l';
      break;
    case CONST_INT:
    case CONST_DOUBLE:
      *patternp++ = 'i';
      break;
    case UNSPEC:
      *patternp++ = 'u';
      *patternp++ = '0' + XCINT(x, 1, UNSPEC);
      for (i=0; i<XVECLEN (x, 0); i++)
	encode_pattern_1 (XVECEXP (x, 0, i));
      break;
    case USE:
      *patternp++ = 'U';
      break;
    default:
      *patternp++ = '?';
#if 0
      fprintf (stderr, "can't encode pattern %s\n", GET_RTX_NAME(GET_CODE(x)));
      debug_rtx (x);
      gcc_unreachable ();
#endif
      break;
    }      
}

static void
encode_pattern (rtx x)
{
  patternp = pattern;
  encode_pattern_1 (x);
  *patternp = 0;
}

int
mep_section_tag (rtx x)
{
  const char *name;

  while (1)
    {
      switch (GET_CODE (x))
	{
	case MEM:
	case CONST:
	  x = XEXP (x, 0);
	  break;
	case UNSPEC:
	  x = XVECEXP (x, 0, 0);
	  break;
	case PLUS:
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    return 0;
	  x = XEXP (x, 0);
	  break;
	default:
	  goto done;
	}
    }
 done:
  if (GET_CODE (x) != SYMBOL_REF)
    return 0;
  name = XSTR (x, 0);
  if (name[0] == '@' && name[2] == '.')
    {
      if (name[1] == 'i' || name[1] == 'I')
	{
	  if (name[1] == 'I')
	    return 'f'; /* near */
	  return 'n'; /* far */
	}
      return name[1];
    }
  return 0;
}

int
mep_regno_reg_class (int regno)
{
  switch (regno)
    {
    case SP_REGNO:		return SP_REGS;
    case TP_REGNO:		return TP_REGS;
    case GP_REGNO:		return GP_REGS;
    case 0: 			return R0_REGS;
    case HI_REGNO:		return HI_REGS;
    case LO_REGNO:		return LO_REGS;
    case ARG_POINTER_REGNUM:	return GENERAL_REGS;
    }

  if (GR_REGNO_P (regno))
    return regno < FIRST_GR_REGNO + 8 ? TPREL_REGS : GENERAL_REGS;
  if (CONTROL_REGNO_P (regno))
    return CONTROL_REGS;

  if (CR_REGNO_P (regno))
    {
      int i, j;

      /* Search for the register amongst user-defined subclasses of
	 the coprocessor registers.  */
      for (i = USER0_REGS; i <= USER3_REGS; ++i)
	{
	  if (! TEST_HARD_REG_BIT (reg_class_contents[i], regno))
	    continue;
	  for (j = 0; j < N_REG_CLASSES; ++j)
	    {
	      enum reg_class sub = reg_class_subclasses[i][j];

	      if (sub == LIM_REG_CLASSES)
		return i;
	      if (TEST_HARD_REG_BIT (reg_class_contents[sub], regno))
		break;
	    }
	}

      return LOADABLE_CR_REGNO_P (regno) ? LOADABLE_CR_REGS : CR_REGS;
    }

  if (CCR_REGNO_P (regno))
    return CCR_REGS;

  gcc_assert (regno >= FIRST_SHADOW_REGISTER && regno <= LAST_SHADOW_REGISTER);
  return NO_REGS;
}

static bool
const_in_range (rtx x, int minv, int maxv)
{
  return (GET_CODE (x) == CONST_INT
	  && INTVAL (x) >= minv
	  && INTVAL (x) <= maxv);
}

/* Given three integer registers DEST, SRC1 and SRC2, return an rtx X
   such that "mulr DEST,X" will calculate DEST = SRC1 * SRC2.  If a move
   is needed, emit it before INSN if INSN is nonnull, otherwise emit it
   at the end of the insn stream.  */

rtx
mep_mulr_source (rtx insn, rtx dest, rtx src1, rtx src2)
{
  if (rtx_equal_p (dest, src1))
    return src2;
  else if (rtx_equal_p (dest, src2))
    return src1;
  else
    {
      if (insn == 0)
	emit_insn (gen_movsi (copy_rtx (dest), src1));
      else
	emit_insn_before (gen_movsi (copy_rtx (dest), src1), insn);
      return src2;
    }
}

/* Replace INSN's pattern with PATTERN, a multiplication PARALLEL.
   Change the last element of PATTERN from (clobber (scratch:SI))
   to (clobber (reg:SI HI_REGNO)).  */

static void
mep_rewrite_mult (rtx insn, rtx pattern)
{
  rtx hi_clobber;

  hi_clobber = XVECEXP (pattern, 0, XVECLEN (pattern, 0) - 1);
  XEXP (hi_clobber, 0) = gen_rtx_REG (SImode, HI_REGNO);
  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
}

/* Subroutine of mep_reuse_lo_p.  Rewrite instruction INSN so that it
   calculates SRC1 * SRC2 and stores the result in $lo.  Also make it
   store the result in DEST if nonnull.  */

static void
mep_rewrite_mulsi3 (rtx insn, rtx dest, rtx src1, rtx src2)
{
  rtx lo, pattern;

  lo = gen_rtx_REG (SImode, LO_REGNO);
  if (dest)
    pattern = gen_mulsi3r (lo, dest, copy_rtx (dest),
			   mep_mulr_source (insn, dest, src1, src2));
  else
    pattern = gen_mulsi3_lo (lo, src1, src2);
  mep_rewrite_mult (insn, pattern);
}

/* Like mep_rewrite_mulsi3, but calculate SRC1 * SRC2 + SRC3.  First copy
   SRC3 into $lo, then use either madd or maddr.  The move into $lo will
   be deleted by a peephole2 if SRC3 is already in $lo.  */

static void
mep_rewrite_maddsi3 (rtx insn, rtx dest, rtx src1, rtx src2, rtx src3)
{
  rtx lo, pattern;

  lo = gen_rtx_REG (SImode, LO_REGNO);
  emit_insn_before (gen_movsi (copy_rtx (lo), src3), insn);
  if (dest)
    pattern = gen_maddsi3r (lo, dest, copy_rtx (dest),
			    mep_mulr_source (insn, dest, src1, src2),
			    copy_rtx (lo));
  else
    pattern = gen_maddsi3_lo (lo, src1, src2, copy_rtx (lo));
  mep_rewrite_mult (insn, pattern);
}

/* Return true if $lo has the same value as integer register GPR when
   instruction INSN is reached.  If necessary, rewrite the instruction
   that sets $lo so that it uses a proper SET, not a CLOBBER.  LO is an
   rtx for (reg:SI LO_REGNO).

   This function is intended to be used by the peephole2 pass.  Since
   that pass goes from the end of a basic block to the beginning, and
   propagates liveness information on the way, there is no need to
   update register notes here.

   If GPR_DEAD_P is true on entry, and this function returns true,
   then the caller will replace _every_ use of GPR in and after INSN
   with LO.  This means that if the instruction that sets $lo is a
   mulr- or maddr-type instruction, we can rewrite it to use mul or
   madd instead.  In combination with the copy progagation pass,
   this allows us to replace sequences like:

	mov GPR,R1
	mulr GPR,R2

   with:

	mul R1,R2

   if GPR is no longer used.  */

static bool
mep_reuse_lo_p_1 (rtx lo, rtx gpr, rtx insn, bool gpr_dead_p)
{
  do
    {
      insn = PREV_INSN (insn);
      if (INSN_P (insn))
	switch (recog_memoized (insn))
	  {
	  case CODE_FOR_mulsi3_1:
	    extract_insn (insn);
	    if (rtx_equal_p (recog_data.operand[0], gpr))
	      {
		mep_rewrite_mulsi3 (insn,
				    gpr_dead_p ? NULL : recog_data.operand[0],
				    recog_data.operand[1],
				    recog_data.operand[2]);
		return true;
	      }
	    return false;

	  case CODE_FOR_maddsi3:
	    extract_insn (insn);
	    if (rtx_equal_p (recog_data.operand[0], gpr))
	      {
		mep_rewrite_maddsi3 (insn,
				     gpr_dead_p ? NULL : recog_data.operand[0],
				     recog_data.operand[1],
				     recog_data.operand[2],
				     recog_data.operand[3]);
		return true;
	      }
	    return false;

	  case CODE_FOR_mulsi3r:
	  case CODE_FOR_maddsi3r:
	    extract_insn (insn);
	    return rtx_equal_p (recog_data.operand[1], gpr);

	  default:
	    if (reg_set_p (lo, insn)
		|| reg_set_p (gpr, insn)
		|| volatile_insn_p (PATTERN (insn)))
	      return false;

	    if (gpr_dead_p && reg_referenced_p (gpr, PATTERN (insn)))
	      gpr_dead_p = false;
	    break;
	  }
    }
  while (!NOTE_INSN_BASIC_BLOCK_P (insn));
  return false;
}

/* A wrapper around mep_reuse_lo_p_1 that preserves recog_data.  */

bool
mep_reuse_lo_p (rtx lo, rtx gpr, rtx insn, bool gpr_dead_p)
{
  bool result = mep_reuse_lo_p_1 (lo, gpr, insn, gpr_dead_p);
  extract_insn (insn);
  return result;
}

/* Return true if SET can be turned into a post-modify load or store
   that adds OFFSET to GPR.  In other words, return true if SET can be
   changed into:

       (parallel [SET (set GPR (plus:SI GPR OFFSET))]).

   It's OK to change SET to an equivalent operation in order to
   make it match.  */

static bool
mep_use_post_modify_for_set_p (rtx set, rtx gpr, rtx offset)
{
  rtx *reg, *mem;
  unsigned int reg_bytes, mem_bytes;
  enum machine_mode reg_mode, mem_mode;

  /* Only simple SETs can be converted.  */
  if (GET_CODE (set) != SET)
    return false;

  /* Point REG to what we hope will be the register side of the set and
     MEM to what we hope will be the memory side.  */
  if (GET_CODE (SET_DEST (set)) == MEM)
    {
      mem = &SET_DEST (set);
      reg = &SET_SRC (set);
    }
  else
    {
      reg = &SET_DEST (set);
      mem = &SET_SRC (set);
      if (GET_CODE (*mem) == SIGN_EXTEND)
	mem = &XEXP (*mem, 0);
    }

  /* Check that *REG is a suitable coprocessor register.  */
  if (GET_CODE (*reg) != REG || !LOADABLE_CR_REGNO_P (REGNO (*reg)))
    return false;

  /* Check that *MEM is a suitable memory reference.  */
  if (GET_CODE (*mem) != MEM || !rtx_equal_p (XEXP (*mem, 0), gpr))
    return false;

  /* Get the number of bytes in each operand.  */
  mem_bytes = GET_MODE_SIZE (GET_MODE (*mem));
  reg_bytes = GET_MODE_SIZE (GET_MODE (*reg));

  /* Check that OFFSET is suitably aligned.  */
  if (INTVAL (offset) & (mem_bytes - 1))
    return false;

  /* Convert *MEM to a normal integer mode.  */
  mem_mode = mode_for_size (mem_bytes * BITS_PER_UNIT, MODE_INT, 0);
  *mem = change_address (*mem, mem_mode, NULL);

  /* Adjust *REG as well.  */
  *reg = shallow_copy_rtx (*reg);
  if (reg == &SET_DEST (set) && reg_bytes < UNITS_PER_WORD)
    {
      /* SET is a subword load.  Convert it to an explicit extension.  */
      PUT_MODE (*reg, SImode);
      *mem = gen_rtx_SIGN_EXTEND (SImode, *mem);
    }
  else
    {
      reg_mode = mode_for_size (reg_bytes * BITS_PER_UNIT, MODE_INT, 0);
      PUT_MODE (*reg, reg_mode);
    }
  return true;
}

/* Return the effect of frame-related instruction INSN.  */

static rtx
mep_frame_expr (rtx insn)
{
  rtx note, expr;

  note = find_reg_note (insn, REG_FRAME_RELATED_EXPR, 0);
  expr = (note != 0 ? XEXP (note, 0) : copy_rtx (PATTERN (insn)));
  RTX_FRAME_RELATED_P (expr) = 1;
  return expr;
}

/* Merge instructions INSN1 and INSN2 using a PARALLEL.  Store the
   new pattern in INSN1; INSN2 will be deleted by the caller.  */

static void
mep_make_parallel (rtx insn1, rtx insn2)
{
  rtx expr;

  if (RTX_FRAME_RELATED_P (insn2))
    {
      expr = mep_frame_expr (insn2);
      if (RTX_FRAME_RELATED_P (insn1))
	expr = gen_rtx_SEQUENCE (VOIDmode,
				 gen_rtvec (2, mep_frame_expr (insn1), expr));
      set_unique_reg_note (insn1, REG_FRAME_RELATED_EXPR, expr);
      RTX_FRAME_RELATED_P (insn1) = 1;
    }

  PATTERN (insn1) = gen_rtx_PARALLEL (VOIDmode,
				      gen_rtvec (2, PATTERN (insn1),
						 PATTERN (insn2)));
  INSN_CODE (insn1) = -1;
}

/* SET_INSN is an instruction that adds OFFSET to REG.  Go back through
   the basic block to see if any previous load or store instruction can
   be persuaded to do SET_INSN as a side-effect.  Return true if so.  */

static bool
mep_use_post_modify_p_1 (rtx set_insn, rtx reg, rtx offset)
{
  rtx insn;

  insn = set_insn;
  do
    {
      insn = PREV_INSN (insn);
      if (INSN_P (insn))
	{
	  if (mep_use_post_modify_for_set_p (PATTERN (insn), reg, offset))
	    {
	      mep_make_parallel (insn, set_insn);
	      return true;
	    }

	  if (reg_set_p (reg, insn)
	      || reg_referenced_p (reg, PATTERN (insn))
	      || volatile_insn_p (PATTERN (insn)))
	    return false;
	}
    }
  while (!NOTE_INSN_BASIC_BLOCK_P (insn));
  return false;
}

/* A wrapper around mep_use_post_modify_p_1 that preserves recog_data.  */

bool
mep_use_post_modify_p (rtx insn, rtx reg, rtx offset)
{
  bool result = mep_use_post_modify_p_1 (insn, reg, offset);
  extract_insn (insn);
  return result;
}

bool
mep_allow_clip (rtx ux, rtx lx, int s)
{
  HOST_WIDE_INT u = INTVAL (ux);
  HOST_WIDE_INT l = INTVAL (lx);
  int i;

  if (!TARGET_OPT_CLIP)
    return false;

  if (s)
    {
      for (i = 0; i < 30; i ++)
	if ((u == ((HOST_WIDE_INT) 1 << i) - 1)
	    && (l == - ((HOST_WIDE_INT) 1 << i)))
	  return true;
    }
  else
    {
      if (l != 0)
	return false;

      for (i = 0; i < 30; i ++)
	if ((u == ((HOST_WIDE_INT) 1 << i) - 1))
	  return true;
    }
  return false;
}

bool
mep_bit_position_p (rtx x, bool looking_for)
{
  if (GET_CODE (x) != CONST_INT)
    return false;
  switch ((int) INTVAL(x) & 0xff)
    {
    case 0x01: case 0x02: case 0x04: case 0x08:
    case 0x10: case 0x20: case 0x40: case 0x80:
      return looking_for;
    case 0xfe: case 0xfd: case 0xfb: case 0xf7:
    case 0xef: case 0xdf: case 0xbf: case 0x7f:
      return !looking_for;
    }
  return false;
}

static bool
move_needs_splitting (rtx dest, rtx src,
		      enum machine_mode mode ATTRIBUTE_UNUSED)
{
  int s = mep_section_tag (src);

  while (1)
    {
      if (GET_CODE (src) == CONST
	  || GET_CODE (src) == MEM)
	src = XEXP (src, 0);
      else if (GET_CODE (src) == SYMBOL_REF
	       || GET_CODE (src) == LABEL_REF
	       || GET_CODE (src) == PLUS)
	break;
      else
	return false;
    }
  if (s == 'f'
      || (GET_CODE (src) == PLUS
	  && GET_CODE (XEXP (src, 1)) == CONST_INT
	  && (INTVAL (XEXP (src, 1)) < -65536
	      || INTVAL (XEXP (src, 1)) > 0xffffff))
      || (GET_CODE (dest) == REG
	  && REGNO (dest) > 7 && REGNO (dest) < FIRST_PSEUDO_REGISTER))
    return true;
  return false;
}

bool
mep_split_mov (rtx *operands, int symbolic)
{
  if (symbolic)
    {
      if (move_needs_splitting (operands[0], operands[1], SImode))
	return true;
      return false;
    }

  if (GET_CODE (operands[1]) != CONST_INT)
    return false;

  if (constraint_satisfied_p (operands[1], CONSTRAINT_I)
      || constraint_satisfied_p (operands[1], CONSTRAINT_J)
      || constraint_satisfied_p (operands[1], CONSTRAINT_O))
    return false;

  if (((!reload_completed && !reload_in_progress)
       || (REG_P (operands[0]) && REGNO (operands[0]) < 8))
      && constraint_satisfied_p (operands[1], CONSTRAINT_K))
    return false;

  return true;
}

/* Irritatingly, the "jsrv" insn *toggles* PSW.OM rather than set
   it to one specific value.  So the insn chosen depends on whether
   the source and destination modes match.  */

bool
mep_vliw_mode_match (rtx tgt)
{
  bool src_vliw = mep_vliw_function_p (cfun->decl);
  bool tgt_vliw = INTVAL (tgt);

  return src_vliw == tgt_vliw;
}

/* Like the above, but also test for near/far mismatches.  */

bool
mep_vliw_jmp_match (rtx tgt)
{
  bool src_vliw = mep_vliw_function_p (cfun->decl);
  bool tgt_vliw = INTVAL (tgt);

  if (mep_section_tag (DECL_RTL (cfun->decl)) == 'f')
    return false;

  return src_vliw == tgt_vliw;
}

bool
mep_multi_slot (rtx x)
{
  return get_attr_slot (x) == SLOT_MULTI;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */

static bool
mep_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  /* We can't convert symbol values to gp- or tp-rel values after
     reload, as reload might have used $gp or $tp for other
     purposes.  */
  if (GET_CODE (x) == SYMBOL_REF && (reload_in_progress || reload_completed))
    {
      char e = mep_section_tag (x);
      return (e != 't' && e != 'b');
    }
  return 1;
}

/* Be careful not to use macros that need to be compiled one way for
   strict, and another way for not-strict, like REG_OK_FOR_BASE_P.  */

bool
mep_legitimate_address (enum machine_mode mode, rtx x, int strict)
{
  int the_tag;

#define DEBUG_LEGIT 0
#if DEBUG_LEGIT
  fprintf (stderr, "legit: mode %s strict %d ", mode_name[mode], strict);
  debug_rtx (x);
#endif

  if (GET_CODE (x) == LO_SUM
      && GET_CODE (XEXP (x, 0)) == REG
      && GEN_REG (REGNO (XEXP (x, 0)), strict)
      && CONSTANT_P (XEXP (x, 1)))
    {
      if (GET_MODE_SIZE (mode) > 4)
	{
	  /* We will end up splitting this, and lo_sums are not
	     offsettable for us.  */
#if DEBUG_LEGIT
	  fprintf(stderr, " - nope, %%lo(sym)[reg] not splittable\n");
#endif
	  return false;
	}
#if DEBUG_LEGIT
      fprintf (stderr, " - yup, %%lo(sym)[reg]\n");
#endif
      return true;
    }

  if (GET_CODE (x) == REG
      && GEN_REG (REGNO (x), strict))
    {
#if DEBUG_LEGIT
      fprintf (stderr, " - yup, [reg]\n");
#endif
      return true;
    }

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GEN_REG (REGNO (XEXP (x, 0)), strict)
      && const_in_range (XEXP (x, 1), -32768, 32767))
    {
#if DEBUG_LEGIT
      fprintf (stderr, " - yup, [reg+const]\n");
#endif
      return true;
    }

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GEN_REG (REGNO (XEXP (x, 0)), strict)
      && GET_CODE (XEXP (x, 1)) == CONST
      && (GET_CODE (XEXP (XEXP (x, 1), 0)) == UNSPEC
	  || (GET_CODE (XEXP (XEXP (x, 1), 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (XEXP (x, 1), 0), 0)) == UNSPEC
	      && GET_CODE (XEXP (XEXP (XEXP (x, 1), 0), 1)) == CONST_INT)))
    {
#if DEBUG_LEGIT
      fprintf (stderr, " - yup, [reg+unspec]\n");
#endif
      return true;
    }

  the_tag = mep_section_tag (x);

  if (the_tag == 'f')
    {
#if DEBUG_LEGIT
      fprintf (stderr, " - nope, [far]\n");
#endif
      return false;
    }

  if (mode == VOIDmode
      && GET_CODE (x) == SYMBOL_REF)
    {
#if DEBUG_LEGIT
      fprintf (stderr, " - yup, call [symbol]\n");
#endif
      return true;
    }

  if ((mode == SImode || mode == SFmode)
      && CONSTANT_P (x)
      && mep_legitimate_constant_p (mode, x)
      && the_tag != 't' && the_tag != 'b')
    {
      if (GET_CODE (x) != CONST_INT
	  || (INTVAL (x) <= 0xfffff
	      && INTVAL (x) >= 0
	      && (INTVAL (x) % 4) == 0))
	{
#if DEBUG_LEGIT
	  fprintf (stderr, " - yup, [const]\n");
#endif
	  return true;
	}
    }

#if DEBUG_LEGIT
  fprintf (stderr, " - nope.\n");
#endif
  return false;
}

int
mep_legitimize_reload_address (rtx *x, enum machine_mode mode, int opnum,
			       int type_i,
			       int ind_levels ATTRIBUTE_UNUSED)
{
  enum reload_type type = (enum reload_type) type_i;

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == MEM
      && GET_CODE (XEXP (*x, 1)) == REG)
    {
      /* GCC will by default copy the MEM into a REG, which results in
	 an invalid address.  For us, the best thing to do is move the
	 whole expression to a REG.  */
      push_reload (*x, NULL_RTX, x, NULL,
		   GENERAL_REGS, mode, VOIDmode,
		   0, 0, opnum, type);
      return 1;
    }

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == SYMBOL_REF
      && GET_CODE (XEXP (*x, 1)) == CONST_INT)
    {
      char e = mep_section_tag (XEXP (*x, 0));

      if (e != 't' && e != 'b')
	{
	  /* GCC thinks that (sym+const) is a valid address.  Well,
	     sometimes it is, this time it isn't.  The best thing to
	     do is reload the symbol to a register, since reg+int
	     tends to work, and we can't just add the symbol and
	     constant anyway.  */
	  push_reload (XEXP (*x, 0), NULL_RTX, &(XEXP(*x, 0)), NULL,
		       GENERAL_REGS, mode, VOIDmode,
		       0, 0, opnum, type);
	  return 1;
	}
    }
  return 0;
}

int
mep_core_address_length (rtx insn, int opn)
{
  rtx set = single_set (insn);
  rtx mem = XEXP (set, opn);
  rtx other = XEXP (set, 1-opn);
  rtx addr = XEXP (mem, 0);

  if (register_operand (addr, Pmode))
    return 2;
  if (GET_CODE (addr) == PLUS)
    {
      rtx addend = XEXP (addr, 1);

      gcc_assert (REG_P (XEXP (addr, 0)));

      switch (REGNO (XEXP (addr, 0)))
	{
	case STACK_POINTER_REGNUM:
	  if (GET_MODE_SIZE (GET_MODE (mem)) == 4
	      && mep_imm7a4_operand (addend, VOIDmode))
	    return 2;
	  break;

	case 13: /* TP */
	  gcc_assert (REG_P (other));

	  if (REGNO (other) >= 8)
	    break;

	  if (GET_CODE (addend) == CONST
	      && GET_CODE (XEXP (addend, 0)) == UNSPEC
	      && XINT (XEXP (addend, 0), 1) == UNS_TPREL)
	    return 2;

	  if (GET_CODE (addend) == CONST_INT
	      && INTVAL (addend) >= 0
	      && INTVAL (addend) <= 127
	      && INTVAL (addend) % GET_MODE_SIZE (GET_MODE (mem)) == 0)
	    return 2;
	  break;
	}
    }

  return 4;
}

int
mep_cop_address_length (rtx insn, int opn)
{
  rtx set = single_set (insn);
  rtx mem = XEXP (set, opn);
  rtx addr = XEXP (mem, 0);

  if (GET_CODE (mem) != MEM)
    return 2;
  if (register_operand (addr, Pmode))
    return 2;
  if (GET_CODE (addr) == POST_INC)
    return 2;

  return 4;
}

#define DEBUG_EXPAND_MOV 0
bool
mep_expand_mov (rtx *operands, enum machine_mode mode)
{
  int i, t;
  int tag[2];
  rtx tpsym, tpoffs;
  int post_reload = 0;

  tag[0] = mep_section_tag (operands[0]);
  tag[1] = mep_section_tag (operands[1]);

  if (!reload_in_progress
      && !reload_completed
      && GET_CODE (operands[0]) != REG
      && GET_CODE (operands[0]) != SUBREG
      && GET_CODE (operands[1]) != REG
      && GET_CODE (operands[1]) != SUBREG)
    operands[1] = copy_to_mode_reg (mode, operands[1]);
  
#if DEBUG_EXPAND_MOV
  fprintf(stderr, "expand move %s %d\n", mode_name[mode],
	  reload_in_progress || reload_completed);
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif

  if (mode == DImode || mode == DFmode)
    return false;

  if (reload_in_progress || reload_completed)
    {
      rtx r;

      if (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == TP_REGNO)
	cfun->machine->reload_changes_tp = true;

      if (tag[0] == 't' || tag[1] == 't')
	{
	  r = has_hard_reg_initial_val (Pmode, GP_REGNO);
	  if (!r || GET_CODE (r) != REG || REGNO (r) != GP_REGNO)
	    post_reload = 1;
	}
      if (tag[0] == 'b' || tag[1] == 'b')
	{
	  r = has_hard_reg_initial_val (Pmode, TP_REGNO);
	  if (!r || GET_CODE (r) != REG || REGNO (r) != TP_REGNO)
	    post_reload = 1;
	}
      if (cfun->machine->reload_changes_tp == true)
	post_reload = 1;
    }

  if (!post_reload)
    {
      rtx n;
      if (symbol_p (operands[1]))
	{
	  t = mep_section_tag (operands[1]);
	  if (t == 'b' || t == 't')
	    {

	      if (GET_CODE (operands[1]) == SYMBOL_REF)
		{
		  tpsym = operands[1];
		  n = gen_rtx_UNSPEC (mode,
				      gen_rtvec (1, operands[1]),
				      t == 'b' ? UNS_TPREL : UNS_GPREL);
		  n = gen_rtx_CONST (mode, n);
		}
	      else if (GET_CODE (operands[1]) == CONST
		       && GET_CODE (XEXP (operands[1], 0)) == PLUS
		       && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == SYMBOL_REF
		       && GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == CONST_INT)
		{
		  tpsym = XEXP (XEXP (operands[1], 0), 0);
		  tpoffs = XEXP (XEXP (operands[1], 0), 1);
		  n = gen_rtx_UNSPEC (mode,
				      gen_rtvec (1, tpsym),
				      t == 'b' ? UNS_TPREL : UNS_GPREL);
		  n = gen_rtx_PLUS (mode, n, tpoffs);
		  n = gen_rtx_CONST (mode, n);
		}
	      else if (GET_CODE (operands[1]) == CONST
		       && GET_CODE (XEXP (operands[1], 0)) == UNSPEC)
		return false;
	      else
		{
		  error ("unusual TP-relative address");
		  return false;
		}

	      n = gen_rtx_PLUS (mode, (t == 'b' ? mep_tp_rtx ()
				       : mep_gp_rtx ()), n);
	      n = emit_insn (gen_rtx_SET (mode, operands[0], n));
#if DEBUG_EXPAND_MOV
	      fprintf(stderr, "mep_expand_mov emitting ");
	      debug_rtx(n);
#endif
	      return true;
	    }
	}

      for (i=0; i < 2; i++)
	{
	  t = mep_section_tag (operands[i]);
	  if (GET_CODE (operands[i]) == MEM && (t == 'b' || t == 't'))
	    {
	      rtx sym, n, r;
	      int u;

	      sym = XEXP (operands[i], 0);
	      if (GET_CODE (sym) == CONST
		  && GET_CODE (XEXP (sym, 0)) == UNSPEC)
		sym = XVECEXP (XEXP (sym, 0), 0, 0);

	      if (t == 'b')
		{
		  r = mep_tp_rtx ();
		  u = UNS_TPREL;
		}
	      else
		{
		  r = mep_gp_rtx ();
		  u = UNS_GPREL;
		}

	      n = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, sym), u);
	      n = gen_rtx_CONST (Pmode, n);
	      n = gen_rtx_PLUS (Pmode, r, n);
	      operands[i] = replace_equiv_address (operands[i], n);
	    }
	}
    }

  if ((GET_CODE (operands[1]) != REG
       && MEP_CONTROL_REG (operands[0]))
      || (GET_CODE (operands[0]) != REG
	  && MEP_CONTROL_REG (operands[1])))
    {
      rtx temp;
#if DEBUG_EXPAND_MOV
      fprintf (stderr, "cr-mem, forcing op1 to reg\n");
#endif
      temp = gen_reg_rtx (mode);
      emit_move_insn (temp, operands[1]);
      operands[1] = temp;
    }

  if (symbolref_p (operands[0])
      && (mep_section_tag (XEXP (operands[0], 0)) == 'f'
	  || (GET_MODE_SIZE (mode) != 4)))
    {
      rtx temp;

      gcc_assert (!reload_in_progress && !reload_completed);

      temp = force_reg (Pmode, XEXP (operands[0], 0));
      operands[0] = replace_equiv_address (operands[0], temp);
      emit_move_insn (operands[0], operands[1]);
      return true;
    }

  if (!post_reload && (tag[1] == 't' || tag[1] == 'b'))
    tag[1] = 0;

  if (symbol_p (operands[1])
      && (tag[1] == 'f' || tag[1] == 't' || tag[1] == 'b'))
    {
      emit_insn (gen_movsi_topsym_s (operands[0], operands[1]));
      emit_insn (gen_movsi_botsym_s (operands[0], operands[0], operands[1]));
      return true;
    }

  if (symbolref_p (operands[1])
      && (tag[1] == 'f' || tag[1] == 't' || tag[1] == 'b'))
    {
      rtx temp;

      if (reload_in_progress || reload_completed)
	temp = operands[0];
      else
	temp = gen_reg_rtx (Pmode);

      emit_insn (gen_movsi_topsym_s (temp, operands[1]));
      emit_insn (gen_movsi_botsym_s (temp, temp, operands[1]));
      emit_move_insn (operands[0], replace_equiv_address (operands[1], temp));
      return true;
    }

  return false;
}

/* Cases where the pattern can't be made to use at all.  */

bool
mep_mov_ok (rtx *operands, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  int i;

#define DEBUG_MOV_OK 0
#if DEBUG_MOV_OK
  fprintf (stderr, "mep_mov_ok %s %c=%c\n", mode_name[mode], mep_section_tag (operands[0]),
	   mep_section_tag (operands[1]));
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif

  /* We want the movh patterns to get these.  */
  if (GET_CODE (operands[1]) == HIGH)
    return false;

  /* We can't store a register to a far variable without using a
     scratch register to hold the address.  Using far variables should
     be split by mep_emit_mov anyway.  */
  if (mep_section_tag (operands[0]) == 'f'
      || mep_section_tag (operands[1]) == 'f')
    {
#if DEBUG_MOV_OK
      fprintf (stderr, " - no, f\n");
#endif
      return false;
    }
  i = mep_section_tag (operands[1]);
  if ((i == 'b' || i == 't') && !reload_completed && !reload_in_progress)
    /* These are supposed to be generated with adds of the appropriate
       register.  During and after reload, however, we allow them to
       be accessed as normal symbols because adding a dependency on
       the base register now might cause problems.  */
    {
#if DEBUG_MOV_OK
      fprintf (stderr, " - no, bt\n");
#endif
      return false;
    }

  /* The only moves we can allow involve at least one general
     register, so require it.  */
  for (i = 0; i < 2; i ++)
    {
      /* Allow subregs too, before reload.  */
      rtx x = operands[i];

      if (GET_CODE (x) == SUBREG)
	x = XEXP (x, 0);
      if (GET_CODE (x) == REG
	  && ! MEP_CONTROL_REG (x))
	{
#if DEBUG_MOV_OK
	  fprintf (stderr, " - ok\n");
#endif
	  return true;
	}
    }
#if DEBUG_MOV_OK
  fprintf (stderr, " - no, no gen reg\n");
#endif
  return false;
}

#define DEBUG_SPLIT_WIDE_MOVE 0
void
mep_split_wide_move (rtx *operands, enum machine_mode mode)
{
  int i;

#if DEBUG_SPLIT_WIDE_MOVE
  fprintf (stderr, "\n\033[34mmep_split_wide_move\033[0m mode %s\n", mode_name[mode]);
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif

  for (i = 0; i <= 1; i++)
    {
      rtx op = operands[i], hi, lo;

      switch (GET_CODE (op))
	{
	case REG:
	  {
	    unsigned int regno = REGNO (op);

	    if (TARGET_64BIT_CR_REGS && CR_REGNO_P (regno))
	      {
		rtx i32;

		lo = gen_rtx_REG (SImode, regno);
		i32 = GEN_INT (32);
		hi = gen_rtx_ZERO_EXTRACT (SImode,
					   gen_rtx_REG (DImode, regno),
					   i32, i32);
	      }
	    else
	      {
		hi = gen_rtx_REG (SImode, regno + TARGET_LITTLE_ENDIAN);
		lo = gen_rtx_REG (SImode, regno + TARGET_BIG_ENDIAN);
	      }
	  }
	  break;

	case CONST_INT:
	case CONST_DOUBLE:
	case MEM:
	  hi = operand_subword (op, TARGET_LITTLE_ENDIAN, 0, mode);
	  lo = operand_subword (op, TARGET_BIG_ENDIAN, 0, mode);
	  break;

	default:
	  gcc_unreachable ();
	}

      /* The high part of CR <- GPR moves must be done after the low part.  */
      operands [i + 4] = lo;
      operands [i + 2] = hi;
    }

  if (reg_mentioned_p (operands[2], operands[5])
      || GET_CODE (operands[2]) == ZERO_EXTRACT
      || GET_CODE (operands[4]) == ZERO_EXTRACT)
    {
      rtx tmp;

      /* Overlapping register pairs -- make sure we don't
	 early-clobber ourselves.  */
      tmp = operands[2];
      operands[2] = operands[4];
      operands[4] = tmp;
      tmp = operands[3];
      operands[3] = operands[5];
      operands[5] = tmp;
    }

#if DEBUG_SPLIT_WIDE_MOVE
  fprintf(stderr, "\033[34m");
  debug_rtx (operands[2]);
  debug_rtx (operands[3]);
  debug_rtx (operands[4]);
  debug_rtx (operands[5]);
  fprintf(stderr, "\033[0m");
#endif
}

/* Emit a setcc instruction in its entirity.  */

static bool
mep_expand_setcc_1 (enum rtx_code code, rtx dest, rtx op1, rtx op2)
{
  rtx tmp;

  switch (code)
    {
    case GT:
    case GTU:
      tmp = op1, op1 = op2, op2 = tmp;
      code = swap_condition (code);
      /* FALLTHRU */

    case LT:
    case LTU:
      op1 = force_reg (SImode, op1);
      emit_insn (gen_rtx_SET (VOIDmode, dest,
			      gen_rtx_fmt_ee (code, SImode, op1, op2)));
      return true;

    case EQ:
      if (op2 != const0_rtx)
	op1 = expand_binop (SImode, sub_optab, op1, op2, NULL, 1, OPTAB_WIDEN);
      mep_expand_setcc_1 (LTU, dest, op1, const1_rtx);
      return true;

    case NE:
      /* Branchful sequence:
		mov dest, 0		16-bit
		beq op1, op2, Lover	16-bit (op2 < 16), 32-bit otherwise
		mov dest, 1		16-bit

	 Branchless sequence:
		add3 tmp, op1, -op2	32-bit (or mov + sub)
		sltu3 tmp, tmp, 1	16-bit
		xor3 dest, tmp, 1	32-bit
	*/
      if (optimize_size && op2 != const0_rtx)
	return false;

      if (op2 != const0_rtx)
	op1 = expand_binop (SImode, sub_optab, op1, op2, NULL, 1, OPTAB_WIDEN);

      op2 = gen_reg_rtx (SImode);
      mep_expand_setcc_1 (LTU, op2, op1, const1_rtx);

      emit_insn (gen_rtx_SET (VOIDmode, dest,
			      gen_rtx_XOR (SImode, op2, const1_rtx)));
      return true;

    case LE:
      if (GET_CODE (op2) != CONST_INT
	  || INTVAL (op2) == 0x7ffffff)
	return false;
      op2 = GEN_INT (INTVAL (op2) + 1);
      return mep_expand_setcc_1 (LT, dest, op1, op2);

    case LEU:
      if (GET_CODE (op2) != CONST_INT
	  || INTVAL (op2) == -1)
	return false;
      op2 = GEN_INT (trunc_int_for_mode (INTVAL (op2) + 1, SImode));
      return mep_expand_setcc_1 (LTU, dest, op1, op2);

    case GE:
      if (GET_CODE (op2) != CONST_INT
	  || INTVAL (op2) == trunc_int_for_mode (0x80000000, SImode))
	return false;
      op2 = GEN_INT (INTVAL (op2) - 1);
      return mep_expand_setcc_1 (GT, dest, op1, op2);

    case GEU:
      if (GET_CODE (op2) != CONST_INT
	  || op2 == const0_rtx)
	return false;
      op2 = GEN_INT (trunc_int_for_mode (INTVAL (op2) - 1, SImode));
      return mep_expand_setcc_1 (GTU, dest, op1, op2);

    default:
      gcc_unreachable ();
    }
}

bool
mep_expand_setcc (rtx *operands)
{
  rtx dest = operands[0];
  enum rtx_code code = GET_CODE (operands[1]);
  rtx op0 = operands[2];
  rtx op1 = operands[3];

  return mep_expand_setcc_1 (code, dest, op0, op1);
}

rtx
mep_expand_cbranch (rtx *operands)
{
  enum rtx_code code = GET_CODE (operands[0]);
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx tmp;

 restart:
  switch (code)
    {
    case LT:
      if (mep_imm4_operand (op1, SImode))
	break;

      tmp = gen_reg_rtx (SImode);
      gcc_assert (mep_expand_setcc_1 (LT, tmp, op0, op1));
      code = NE;
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case GE:
      if (mep_imm4_operand (op1, SImode))
	break;

      tmp = gen_reg_rtx (SImode);
      gcc_assert (mep_expand_setcc_1 (LT, tmp, op0, op1));

      code = EQ;
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case EQ:
    case NE:
      if (! mep_reg_or_imm4_operand (op1, SImode))
	op1 = force_reg (SImode, op1);
      break;

    case LE:
    case GT:
      if (GET_CODE (op1) == CONST_INT
	  && INTVAL (op1) != 0x7fffffff)
	{
	  op1 = GEN_INT (INTVAL (op1) + 1);
	  code = (code == LE ? LT : GE);
	  goto restart;
	}

      tmp = gen_reg_rtx (SImode);
      gcc_assert (mep_expand_setcc_1 (LT, tmp, op1, op0));

      code = (code == LE ? EQ : NE);
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case LTU:
      if (op1 == const1_rtx)
	{
	  code = EQ;
	  op1 = const0_rtx;
	  break;
	}

      tmp = gen_reg_rtx (SImode);
      gcc_assert (mep_expand_setcc_1 (LTU, tmp, op0, op1));
      code = NE;
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case LEU:
      tmp = gen_reg_rtx (SImode);
      if (mep_expand_setcc_1 (LEU, tmp, op0, op1))
	code = NE;
      else if (mep_expand_setcc_1 (LTU, tmp, op1, op0))
	code = EQ;
      else
	gcc_unreachable ();
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case GTU:
      tmp = gen_reg_rtx (SImode);
      gcc_assert (mep_expand_setcc_1 (GTU, tmp, op0, op1)
		  || mep_expand_setcc_1 (LTU, tmp, op1, op0));
      code = NE;
      op0 = tmp;
      op1 = const0_rtx;
      break;

    case GEU:
      tmp = gen_reg_rtx (SImode);
      if (mep_expand_setcc_1 (GEU, tmp, op0, op1))
	code = NE;
      else if (mep_expand_setcc_1 (LTU, tmp, op0, op1))
	code = EQ;
      else
	gcc_unreachable ();
      op0 = tmp;
      op1 = const0_rtx;
      break;

    default:
      gcc_unreachable ();
    }

  return gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
}

const char *
mep_emit_cbranch (rtx *operands, int ne)
{
  if (GET_CODE (operands[1]) == REG)
    return ne ? "bne\t%0, %1, %l2" : "beq\t%0, %1, %l2";
  else if (INTVAL (operands[1]) == 0 && !mep_vliw_function_p(cfun->decl))
    return ne ? "bnez\t%0, %l2" : "beqz\t%0, %l2";
  else
    return ne ? "bnei\t%0, %1, %l2" : "beqi\t%0, %1, %l2";
}

void
mep_expand_call (rtx *operands, int returns_value)
{
  rtx addr = operands[returns_value];
  rtx tp = mep_tp_rtx ();
  rtx gp = mep_gp_rtx ();

  gcc_assert (GET_CODE (addr) == MEM);

  addr = XEXP (addr, 0);

  if (! mep_call_address_operand (addr, VOIDmode))
    addr = force_reg (SImode, addr);

  if (! operands[returns_value+2])
    operands[returns_value+2] = const0_rtx;

  if (returns_value)
    emit_call_insn (gen_call_value_internal (operands[0], addr, operands[2],
					     operands[3], tp, gp));
  else
    emit_call_insn (gen_call_internal (addr, operands[1],
				       operands[2], tp, gp));
}

/* Aliasing Support.  */

/* If X is a machine specific address (i.e. a symbol or label being
   referenced as a displacement from the GOT implemented using an
   UNSPEC), then return the base term.  Otherwise return X.  */

rtx
mep_find_base_term (rtx x)
{
  rtx base, term;
  int unspec;

  if (GET_CODE (x) != PLUS)
    return x;
  base = XEXP (x, 0);
  term = XEXP (x, 1);

  if (has_hard_reg_initial_val(Pmode, TP_REGNO)
      && base == mep_tp_rtx ())
    unspec = UNS_TPREL;
  else if (has_hard_reg_initial_val(Pmode, GP_REGNO)
	   && base == mep_gp_rtx ())
    unspec = UNS_GPREL;
  else
    return x;

  if (GET_CODE (term) != CONST)
    return x;
  term = XEXP (term, 0);

  if (GET_CODE (term) != UNSPEC
      || XINT (term, 1) != unspec)
    return x;

  return XVECEXP (term, 0, 0);
}

/* Reload Support.  */

/* Return true if the registers in CLASS cannot represent the change from
   modes FROM to TO.  */

bool
mep_cannot_change_mode_class (enum machine_mode from, enum machine_mode to,
			       enum reg_class regclass)
{
  if (from == to)
    return false;

  /* 64-bit COP regs must remain 64-bit COP regs.  */
  if (TARGET_64BIT_CR_REGS
      && (regclass == CR_REGS
	  || regclass == LOADABLE_CR_REGS)
      && (GET_MODE_SIZE (to) < 8
	  || GET_MODE_SIZE (from) < 8))
    return true;

  return false;
}

#define MEP_NONGENERAL_CLASS(C) (!reg_class_subset_p (C, GENERAL_REGS))

static bool
mep_general_reg (rtx x)
{
  while (GET_CODE (x) == SUBREG)
    x = XEXP (x, 0);
  return GET_CODE (x) == REG && GR_REGNO_P (REGNO (x));
}

static bool
mep_nongeneral_reg (rtx x)
{
  while (GET_CODE (x) == SUBREG)
    x = XEXP (x, 0);
  return (GET_CODE (x) == REG
	  && !GR_REGNO_P (REGNO (x)) && REGNO (x) < FIRST_PSEUDO_REGISTER);
}

static bool
mep_general_copro_reg (rtx x)
{
  while (GET_CODE (x) == SUBREG)
    x = XEXP (x, 0);
  return (GET_CODE (x) == REG && CR_REGNO_P (REGNO (x)));
}

static bool
mep_nonregister (rtx x)
{
  while (GET_CODE (x) == SUBREG)
    x = XEXP (x, 0);
  return (GET_CODE (x) != REG || REGNO (x) >= FIRST_PSEUDO_REGISTER);
}

#define DEBUG_RELOAD 0

/* Return the secondary reload class needed for moving value X to or
   from a register in coprocessor register class CLASS.  */

static enum reg_class
mep_secondary_copro_reload_class (enum reg_class rclass, rtx x)
{
  if (mep_general_reg (x))
    /* We can do the move directly if mep_have_core_copro_moves_p,
       otherwise we need to go through memory.  Either way, no secondary
       register is needed.  */
    return NO_REGS;

  if (mep_general_copro_reg (x))
    {
      /* We can do the move directly if mep_have_copro_copro_moves_p.  */
      if (mep_have_copro_copro_moves_p)
	return NO_REGS;

      /* Otherwise we can use a temporary if mep_have_core_copro_moves_p.  */
      if (mep_have_core_copro_moves_p)
	return GENERAL_REGS;

      /* Otherwise we need to do it through memory.  No secondary
	 register is needed.  */
      return NO_REGS;
    }

  if (reg_class_subset_p (rclass, LOADABLE_CR_REGS)
      && constraint_satisfied_p (x, CONSTRAINT_U))
    /* X is a memory value that we can access directly.  */
    return NO_REGS;

  /* We have to move X into a GPR first and then copy it to
     the coprocessor register.  The move from the GPR to the
     coprocessor might be done directly or through memory,
     depending on mep_have_core_copro_moves_p. */
  return GENERAL_REGS;
}

/* Copying X to register in RCLASS.  */

enum reg_class
mep_secondary_input_reload_class (enum reg_class rclass,
				  enum machine_mode mode ATTRIBUTE_UNUSED,
				  rtx x)
{
  int rv = NO_REGS;

#if DEBUG_RELOAD
  fprintf (stderr, "secondary input reload copy to %s %s from ", reg_class_names[rclass], mode_name[mode]);
  debug_rtx (x);
#endif

  if (reg_class_subset_p (rclass, CR_REGS))
    rv = mep_secondary_copro_reload_class (rclass, x);
  else if (MEP_NONGENERAL_CLASS (rclass)
	   && (mep_nonregister (x) || mep_nongeneral_reg (x)))
    rv = GENERAL_REGS;

#if DEBUG_RELOAD
  fprintf (stderr, " - requires %s\n", reg_class_names[rv]);
#endif
  return (enum reg_class) rv;
}

/* Copying register in RCLASS to X.  */

enum reg_class
mep_secondary_output_reload_class (enum reg_class rclass,
				   enum machine_mode mode ATTRIBUTE_UNUSED,
				   rtx x)
{
  int rv = NO_REGS;

#if DEBUG_RELOAD
  fprintf (stderr, "secondary output reload copy from %s %s to ", reg_class_names[rclass], mode_name[mode]);
  debug_rtx (x);
#endif

  if (reg_class_subset_p (rclass, CR_REGS))
    rv = mep_secondary_copro_reload_class (rclass, x);
  else if (MEP_NONGENERAL_CLASS (rclass)
	   && (mep_nonregister (x) || mep_nongeneral_reg (x)))
    rv = GENERAL_REGS;

#if DEBUG_RELOAD
  fprintf (stderr, " - requires %s\n", reg_class_names[rv]);
#endif

  return (enum reg_class) rv;
}

/* Implement SECONDARY_MEMORY_NEEDED.  */

bool
mep_secondary_memory_needed (enum reg_class rclass1, enum reg_class rclass2,
			     enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (!mep_have_core_copro_moves_p)
    {
      if (reg_classes_intersect_p (rclass1, CR_REGS)
	  && reg_classes_intersect_p (rclass2, GENERAL_REGS))
	return true;
      if (reg_classes_intersect_p (rclass2, CR_REGS)
	  && reg_classes_intersect_p (rclass1, GENERAL_REGS))
	return true;
      if (!mep_have_copro_copro_moves_p
	  && reg_classes_intersect_p (rclass1, CR_REGS)
	  && reg_classes_intersect_p (rclass2, CR_REGS))
	return true;
    }
  return false;
}

void
mep_expand_reload (rtx *operands, enum machine_mode mode)
{
  /* There are three cases for each direction:
     register, farsym
     control, farsym
     control, nearsym */

  int s0 = mep_section_tag (operands[0]) == 'f';
  int s1 = mep_section_tag (operands[1]) == 'f';
  int c0 = mep_nongeneral_reg (operands[0]);
  int c1 = mep_nongeneral_reg (operands[1]);
  int which = (s0 ? 20:0) + (c0 ? 10:0) + (s1 ? 2:0) + (c1 ? 1:0);

#if DEBUG_RELOAD
  fprintf (stderr, "expand_reload %s\n", mode_name[mode]);
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif

  switch (which)
    {
    case 00: /* Don't know why this gets here.  */
    case 02: /* general = far */
      emit_move_insn (operands[0], operands[1]);
      return;

    case 10: /* cr = mem */
    case 11: /* cr = cr */
    case 01: /* mem = cr */
    case 12: /* cr = far */
      emit_move_insn (operands[2], operands[1]);
      emit_move_insn (operands[0], operands[2]);
      return;

    case 20: /* far = general */
      emit_move_insn (operands[2], XEXP (operands[1], 0));
      emit_move_insn (operands[0], gen_rtx_MEM (mode, operands[2]));
      return;

    case 21: /* far = cr */
    case 22: /* far = far */
    default:
      fprintf (stderr, "unsupported expand reload case %02d for mode %s\n",
	       which, mode_name[mode]);
      debug_rtx (operands[0]);
      debug_rtx (operands[1]);
      gcc_unreachable ();
    }
}

/* Implement PREFERRED_RELOAD_CLASS.  See whether X is a constant that
   can be moved directly into registers 0 to 7, but not into the rest.
   If so, and if the required class includes registers 0 to 7, restrict
   it to those registers.  */

enum reg_class
mep_preferred_reload_class (rtx x, enum reg_class rclass)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      if (INTVAL (x) >= 0x10000
	  && INTVAL (x) < 0x01000000
	  && (INTVAL (x) & 0xffff) != 0
	  && reg_class_subset_p (TPREL_REGS, rclass))
	rclass = TPREL_REGS;
      break;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (mep_section_tag (x) != 'f'
	  && reg_class_subset_p (TPREL_REGS, rclass))
	rclass = TPREL_REGS;
      break;

    default:
      break;
    }
  return rclass;
}

/* Implement REGISTER_MOVE_COST.  Return 2 for direct single-register
   moves, 4 for direct double-register moves, and 1000 for anything
   that requires a temporary register or temporary stack slot.  */

int
mep_register_move_cost (enum machine_mode mode, enum reg_class from, enum reg_class to)
{
  if (mep_have_copro_copro_moves_p
      && reg_class_subset_p (from, CR_REGS)
      && reg_class_subset_p (to, CR_REGS))
    {
      if (TARGET_32BIT_CR_REGS && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return 4;
      return 2;
    }
  if (reg_class_subset_p (from, CR_REGS)
      && reg_class_subset_p (to, CR_REGS))
    {
      if (TARGET_32BIT_CR_REGS && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return 8;
      return 4;
    }
  if (reg_class_subset_p (from, CR_REGS)
      || reg_class_subset_p (to, CR_REGS))
    {
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return 4;
      return 2;
    }
  if (mep_secondary_memory_needed (from, to, mode))
    return 1000;
  if (MEP_NONGENERAL_CLASS (from) && MEP_NONGENERAL_CLASS (to))
    return 1000;

  if (GET_MODE_SIZE (mode) > 4)
    return 4;

  return 2;
}


/* Functions to save and restore machine-specific function data.  */

static struct machine_function *
mep_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

static rtx
mep_allocate_initial_value (rtx reg)
{
  int rss;

  if (GET_CODE (reg) != REG)
    return NULL_RTX;

  if (REGNO (reg) >= FIRST_PSEUDO_REGISTER)
    return NULL_RTX;

  /* In interrupt functions, the "initial" values of $gp and $tp are
     provided by the prologue.  They are not necessarily the same as
     the values that the caller was using.  */
  if (REGNO (reg) == TP_REGNO || REGNO (reg) == GP_REGNO)
    if (mep_interrupt_p ())
      return NULL_RTX;

  if (! cfun->machine->reg_save_slot[REGNO(reg)])
    {
      cfun->machine->reg_save_size += 4;
      cfun->machine->reg_save_slot[REGNO(reg)] = cfun->machine->reg_save_size;
    }

  rss = cfun->machine->reg_save_slot[REGNO(reg)];
  return gen_rtx_MEM (SImode, plus_constant (Pmode, arg_pointer_rtx, -rss));
}

rtx
mep_return_addr_rtx (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, LP_REGNO);
}

static rtx
mep_tp_rtx (void)
{
  return get_hard_reg_initial_val (Pmode, TP_REGNO);
}

static rtx
mep_gp_rtx (void)
{
  return get_hard_reg_initial_val (Pmode, GP_REGNO);
}

static bool
mep_interrupt_p (void)
{
  if (cfun->machine->interrupt_handler == 0)
    {
      int interrupt_handler
	= (lookup_attribute ("interrupt",
			     DECL_ATTRIBUTES (current_function_decl))
	   != NULL_TREE);
      cfun->machine->interrupt_handler = interrupt_handler ? 2 : 1;
    }
  return cfun->machine->interrupt_handler == 2;
}

static bool
mep_disinterrupt_p (void)
{
  if (cfun->machine->disable_interrupts == 0)
    {
      int disable_interrupts
	= (lookup_attribute ("disinterrupt",
			     DECL_ATTRIBUTES (current_function_decl))
	   != NULL_TREE);
      cfun->machine->disable_interrupts = disable_interrupts ? 2 : 1;
    }
  return cfun->machine->disable_interrupts == 2;
}


/* Frame/Epilog/Prolog Related.  */

static bool
mep_reg_set_p (rtx reg, rtx insn)
{
  /* Similar to reg_set_p in rtlanal.c, but we ignore calls */
  if (INSN_P (insn))
    {
      if (FIND_REG_INC_NOTE (insn, reg))
	return true;
      insn = PATTERN (insn);
    }

  if (GET_CODE (insn) == SET
      && GET_CODE (XEXP (insn, 0)) == REG
      && GET_CODE (XEXP (insn, 1)) == REG
      && REGNO (XEXP (insn, 0)) == REGNO (XEXP (insn, 1)))
    return false;

  return set_of (reg, insn) != NULL_RTX;
}


#define MEP_SAVES_UNKNOWN 0
#define MEP_SAVES_YES 1
#define MEP_SAVES_MAYBE 2
#define MEP_SAVES_NO 3

static bool
mep_reg_set_in_function (int regno)
{
  rtx reg, insn;

  if (mep_interrupt_p () && df_regs_ever_live_p(regno))
    return true;

  if (regno == LP_REGNO && (profile_arc_flag > 0 || profile_flag > 0))
    return true;

  push_topmost_sequence ();
  insn = get_insns ();
  pop_topmost_sequence ();

  if (!insn)
    return false;

  reg = gen_rtx_REG (SImode, regno);

  for (insn = NEXT_INSN (insn); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && mep_reg_set_p (reg, insn))
      return true;
  return false;
}

static bool
mep_asm_without_operands_p (void)
{
  if (cfun->machine->asms_without_operands == 0)
    {
      rtx insn;

      push_topmost_sequence ();
      insn = get_insns ();
      pop_topmost_sequence ();

      cfun->machine->asms_without_operands = 1;
      while (insn)
	{
	  if (INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) == ASM_INPUT)
	    {
	      cfun->machine->asms_without_operands = 2;
	      break;
	    }
	  insn = NEXT_INSN (insn);
	}

    }
  return cfun->machine->asms_without_operands == 2;
}

/* Interrupt functions save/restore every call-preserved register, and
   any call-used register it uses (or all if it calls any function,
   since they may get clobbered there too).  Here we check to see
   which call-used registers need saving.  */

#define IVC2_ISAVED_REG(r) (TARGET_IVC2 \
			   && (r == FIRST_CCR_REGNO + 1 \
			       || (r >= FIRST_CCR_REGNO + 8 && r <= FIRST_CCR_REGNO + 11) \
			       || (r >= FIRST_CCR_REGNO + 16 && r <= FIRST_CCR_REGNO + 31)))

static bool
mep_interrupt_saved_reg (int r)
{
  if (!mep_interrupt_p ())
    return false;
  if (r == REGSAVE_CONTROL_TEMP
      || (TARGET_64BIT_CR_REGS && TARGET_COP && r == REGSAVE_CONTROL_TEMP+1))
    return true;
  if (mep_asm_without_operands_p ()
      && (!fixed_regs[r]
	  || (r == RPB_REGNO || r == RPE_REGNO || r == RPC_REGNO || r == LP_REGNO)
	  || IVC2_ISAVED_REG (r)))
    return true;
  if (!crtl->is_leaf)
    /* Function calls mean we need to save $lp.  */
    if (r == LP_REGNO || IVC2_ISAVED_REG (r))
      return true;
  if (!crtl->is_leaf || cfun->machine->doloop_tags > 0)
    /* The interrupt handler might use these registers for repeat blocks,
       or it might call a function that does so.  */
    if (r == RPB_REGNO || r == RPE_REGNO || r == RPC_REGNO)
      return true;
  if (crtl->is_leaf && call_used_regs[r] && !df_regs_ever_live_p(r))
    return false;
  /* Functions we call might clobber these.  */
  if (call_used_regs[r] && !fixed_regs[r])
    return true;
  /* Additional registers that need to be saved for IVC2.  */
  if (IVC2_ISAVED_REG (r))
    return true;

  return false;
}

static bool
mep_call_saves_register (int r)
{
  if (! cfun->machine->frame_locked)
    {
      int rv = MEP_SAVES_NO;

      if (cfun->machine->reg_save_slot[r])
  	rv = MEP_SAVES_YES;
      else if (r == LP_REGNO && (profile_arc_flag > 0 || profile_flag > 0))
	rv = MEP_SAVES_YES;
      else if (r == FRAME_POINTER_REGNUM && frame_pointer_needed)
	rv = MEP_SAVES_YES;
      else if ((!call_used_regs[r] || r == LP_REGNO) && df_regs_ever_live_p(r))
	rv = MEP_SAVES_YES;
      else if (crtl->calls_eh_return && (r == 10 || r == 11))
	/* We need these to have stack slots so that they can be set during
	   unwinding.  */
	rv = MEP_SAVES_YES;
      else if (mep_interrupt_saved_reg (r))
	rv = MEP_SAVES_YES;
      cfun->machine->reg_saved[r] = rv;
    }
  return cfun->machine->reg_saved[r] == MEP_SAVES_YES;
}

/* Return true if epilogue uses register REGNO.  */

bool
mep_epilogue_uses (int regno)
{
  /* Since $lp is a call-saved register, the generic code will normally
     mark it used in the epilogue if it needs to be saved and restored.
     However, when profiling is enabled, the profiling code will implicitly
     clobber $11.  This case has to be handled specially both here and in
     mep_call_saves_register.  */
  if (regno == LP_REGNO && (profile_arc_flag > 0 || profile_flag > 0))
    return true;
  /* Interrupt functions save/restore pretty much everything.  */
  return (reload_completed && mep_interrupt_saved_reg (regno));
}

static int
mep_reg_size (int regno)
{
  if (CR_REGNO_P (regno) && TARGET_64BIT_CR_REGS)
    return 8;
  return 4;
}

/* Worker function for TARGET_CAN_ELIMINATE.  */

bool
mep_can_eliminate (const int from, const int to)
{
  return  (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM
           ? ! frame_pointer_needed
           : true);
}

int
mep_elimination_offset (int from, int to)
{
  int reg_save_size;
  int i;
  int frame_size = get_frame_size () + crtl->outgoing_args_size;
  int total_size;

  if (!cfun->machine->frame_locked)
    memset (cfun->machine->reg_saved, 0, sizeof (cfun->machine->reg_saved));

  /* We don't count arg_regs_to_save in the arg pointer offset, because
     gcc thinks the arg pointer has moved along with the saved regs.
     However, we do count it when we adjust $sp in the prologue.  */
  reg_save_size = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (mep_call_saves_register (i))
      reg_save_size += mep_reg_size (i);

  if (reg_save_size % 8)
    cfun->machine->regsave_filler = 8 - (reg_save_size % 8);
  else
    cfun->machine->regsave_filler = 0;

  /* This is what our total stack adjustment looks like.  */
  total_size = (reg_save_size + frame_size + cfun->machine->regsave_filler);

  if (total_size % 8)
    cfun->machine->frame_filler = 8 - (total_size % 8);
  else
    cfun->machine->frame_filler = 0;


  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return reg_save_size + cfun->machine->regsave_filler;

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return cfun->machine->frame_filler + frame_size;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return reg_save_size + cfun->machine->regsave_filler + cfun->machine->frame_filler + frame_size;

  gcc_unreachable ();
}

static rtx
F (rtx x)
{
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* Since the prologue/epilogue code is generated after optimization,
   we can't rely on gcc to split constants for us.  So, this code
   captures all the ways to add a constant to a register in one logic
   chunk, including optimizing away insns we just don't need.  This
   makes the prolog/epilog code easier to follow.  */
static void
add_constant (int dest, int src, int value, int mark_frame)
{
  rtx insn;
  int hi, lo;

  if (src == dest && value == 0)
    return;

  if (value == 0)
    {
      insn = emit_move_insn (gen_rtx_REG (SImode, dest),
			     gen_rtx_REG (SImode, src));
      if (mark_frame)
	RTX_FRAME_RELATED_P(insn) = 1;
      return;
    }

  if (value >= -32768 && value <= 32767)
    {
      insn = emit_insn (gen_addsi3 (gen_rtx_REG (SImode, dest),
				    gen_rtx_REG (SImode, src),
				    GEN_INT (value)));
      if (mark_frame)
	RTX_FRAME_RELATED_P(insn) = 1;
      return;
    }

  /* Big constant, need to use a temp register.  We use
     REGSAVE_CONTROL_TEMP because it's call clobberable (the reg save
     area is always small enough to directly add to).  */

  hi = trunc_int_for_mode (value & 0xffff0000, SImode);
  lo = value & 0xffff;

  insn = emit_move_insn (gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
			 GEN_INT (hi));

  if (lo)
    {
      insn = emit_insn (gen_iorsi3 (gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
				    gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
				    GEN_INT (lo)));
    }

  insn = emit_insn (gen_addsi3 (gen_rtx_REG (SImode, dest),
				gen_rtx_REG (SImode, src),
				gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP)));
  if (mark_frame)
    {
      RTX_FRAME_RELATED_P(insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (SImode,
				 gen_rtx_REG (SImode, dest),
				 gen_rtx_PLUS (SImode,
					       gen_rtx_REG (SImode, dest),
					       GEN_INT (value))));
    }
}

/* Move SRC to DEST.  Mark the move as being potentially dead if
   MAYBE_DEAD_P.  */

static rtx
maybe_dead_move (rtx dest, rtx src, bool ATTRIBUTE_UNUSED maybe_dead_p)
{
  rtx insn = emit_move_insn (dest, src);
#if 0
  if (maybe_dead_p)
    REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx, NULL);
#endif
  return insn;
}

/* Used for interrupt functions, which can't assume that $tp and $gp
   contain the correct pointers.  */

static void
mep_reload_pointer (int regno, const char *symbol)
{
  rtx reg, sym;

  if (!df_regs_ever_live_p(regno) && crtl->is_leaf)
    return;

  reg = gen_rtx_REG (SImode, regno);
  sym = gen_rtx_SYMBOL_REF (SImode, symbol);
  emit_insn (gen_movsi_topsym_s (reg, sym));
  emit_insn (gen_movsi_botsym_s (reg, reg, sym));
}

/* Assign save slots for any register not already saved.  DImode
   registers go at the end of the reg save area; the rest go at the
   beginning.  This is for alignment purposes.  Returns true if a frame
   is really needed.  */
static bool
mep_assign_save_slots (int reg_save_size)
{
  bool really_need_stack_frame = false;
  int di_ofs = 0;
  int i;

  for (i=0; i<FIRST_PSEUDO_REGISTER; i++)
    if (mep_call_saves_register(i))
      {
	int regsize = mep_reg_size (i);

	if ((i != TP_REGNO && i != GP_REGNO && i != LP_REGNO)
	    || mep_reg_set_in_function (i))
	  really_need_stack_frame = true;

	if (cfun->machine->reg_save_slot[i])
	  continue;

	if (regsize < 8)
	  {
	    cfun->machine->reg_save_size += regsize;
	    cfun->machine->reg_save_slot[i] = cfun->machine->reg_save_size;
	  }
	else
	  {
	    cfun->machine->reg_save_slot[i] = reg_save_size - di_ofs;
	    di_ofs += 8;
	  }
      }
  cfun->machine->frame_locked = 1;
  return really_need_stack_frame;
}

void
mep_expand_prologue (void)
{
  int i, rss, sp_offset = 0;
  int reg_save_size;
  int frame_size;
  int really_need_stack_frame;

  /* We must not allow register renaming in interrupt functions,
     because that invalidates the correctness of the set of call-used
     registers we're going to save/restore.  */
  mep_set_leaf_registers (mep_interrupt_p () ? 0 : 1);

  if (mep_disinterrupt_p ())
    emit_insn (gen_mep_disable_int ());

  cfun->machine->mep_frame_pointer_needed = frame_pointer_needed;

  reg_save_size = mep_elimination_offset (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM);
  frame_size = mep_elimination_offset (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM);
  really_need_stack_frame = frame_size;

  really_need_stack_frame |= mep_assign_save_slots (reg_save_size);

  sp_offset = reg_save_size;
  if (sp_offset + frame_size < 128)
    sp_offset += frame_size ;

  add_constant (SP_REGNO, SP_REGNO, -sp_offset, 1);

  for (i=0; i<FIRST_PSEUDO_REGISTER; i++)
    if (mep_call_saves_register(i))
      {
	rtx mem;
	bool maybe_dead_p;
	enum machine_mode rmode;

	rss = cfun->machine->reg_save_slot[i];

  	if ((i == TP_REGNO || i == GP_REGNO || i == LP_REGNO)
	    && (!mep_reg_set_in_function (i)
		&& !mep_interrupt_p ()))
	  continue;

	if (mep_reg_size (i) == 8)
	  rmode = DImode;
	else
	  rmode = SImode;

	/* If there is a pseudo associated with this register's initial value,
	   reload might have already spilt it to the stack slot suggested by
	   ALLOCATE_INITIAL_VALUE.  The moves emitted here can then be safely
	   deleted as dead.  */
	mem = gen_rtx_MEM (rmode,
			   plus_constant (Pmode, stack_pointer_rtx,
					  sp_offset - rss));
	maybe_dead_p = rtx_equal_p (mem, has_hard_reg_initial_val (rmode, i));

	if (GR_REGNO_P (i) || LOADABLE_CR_REGNO_P (i))
	  F(maybe_dead_move (mem, gen_rtx_REG (rmode, i), maybe_dead_p));
	else if (rmode == DImode)
	  {
	    rtx insn;
	    int be = TARGET_BIG_ENDIAN ? 4 : 0;

	    mem = gen_rtx_MEM (SImode,
			       plus_constant (Pmode, stack_pointer_rtx,
					      sp_offset - rss + be));

	    maybe_dead_move (gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
			     gen_rtx_REG (SImode, i),
			     maybe_dead_p);
	    maybe_dead_move (gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP+1),
			     gen_rtx_ZERO_EXTRACT (SImode,
						   gen_rtx_REG (DImode, i),
						   GEN_INT (32),
						   GEN_INT (32)),
			     maybe_dead_p);
	    insn = maybe_dead_move (mem,
				    gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
				    maybe_dead_p);
	    RTX_FRAME_RELATED_P (insn) = 1;
	    
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_rtx_SET (VOIDmode,
				       copy_rtx (mem),
				       gen_rtx_REG (rmode, i)));
	    mem = gen_rtx_MEM (SImode,
			       plus_constant (Pmode, stack_pointer_rtx,
					      sp_offset - rss + (4-be)));
	    insn = maybe_dead_move (mem,
				    gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP+1),
				    maybe_dead_p);
	  }
	else
	  {
	    rtx insn;
	    maybe_dead_move (gen_rtx_REG (rmode, REGSAVE_CONTROL_TEMP),
			     gen_rtx_REG (rmode, i),
			     maybe_dead_p);
	    insn = maybe_dead_move (mem,
				    gen_rtx_REG (rmode, REGSAVE_CONTROL_TEMP),
				    maybe_dead_p);
	    RTX_FRAME_RELATED_P (insn) = 1;
	    
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_rtx_SET (VOIDmode,
				       copy_rtx (mem),
				       gen_rtx_REG (rmode, i)));
	  }
      }
  
  if (frame_pointer_needed)
    {
      /* We've already adjusted down by sp_offset.  Total $sp change
	 is reg_save_size + frame_size.  We want a net change here of
	 just reg_save_size.  */
      add_constant (FP_REGNO, SP_REGNO, sp_offset - reg_save_size, 1);
    }

  add_constant (SP_REGNO, SP_REGNO, sp_offset-(reg_save_size+frame_size), 1);

  if (mep_interrupt_p ())
    {
      mep_reload_pointer(GP_REGNO, "__sdabase");
      mep_reload_pointer(TP_REGNO, "__tpbase");
    }
}

static void
mep_start_function (FILE *file, HOST_WIDE_INT hwi_local)
{
  int local = hwi_local;
  int frame_size = local + crtl->outgoing_args_size;
  int reg_save_size;
  int ffill;
  int i, sp, skip;
  int sp_offset;
  int slot_map[FIRST_PSEUDO_REGISTER], si, sj;

  reg_save_size = mep_elimination_offset (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM);
  frame_size = mep_elimination_offset (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM);
  sp_offset = reg_save_size + frame_size;

  ffill = cfun->machine->frame_filler;

  if (cfun->machine->mep_frame_pointer_needed)
    reg_names[FP_REGNO] = "$fp";
  else
    reg_names[FP_REGNO] = "$8";

  if (sp_offset == 0)
    return;

  if (debug_info_level == DINFO_LEVEL_NONE)
    {
      fprintf (file, "\t# frame: %d", sp_offset);
      if (reg_save_size)
	fprintf (file, "   %d regs", reg_save_size);
      if (local)
	fprintf (file, "   %d locals", local);
      if (crtl->outgoing_args_size)
	fprintf (file, "   %d args", crtl->outgoing_args_size);
      fprintf (file, "\n");
      return;
    }

  fprintf (file, "\t#\n");
  fprintf (file, "\t# Initial Frame Information:\n");
  if (sp_offset || !frame_pointer_needed)
    fprintf (file, "\t# Entry   ---------- 0\n");

  /* Sort registers by save slots, so they're printed in the order
     they appear in memory, not the order they're saved in.  */
  for (si=0; si<FIRST_PSEUDO_REGISTER; si++)
    slot_map[si] = si;
  for (si=0; si<FIRST_PSEUDO_REGISTER-1; si++)
    for (sj=si+1; sj<FIRST_PSEUDO_REGISTER; sj++)
      if (cfun->machine->reg_save_slot[slot_map[si]]
	  > cfun->machine->reg_save_slot[slot_map[sj]])
	{
	  int t = slot_map[si];
	  slot_map[si] = slot_map[sj];
	  slot_map[sj] = t;
	}

  sp = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int rsize;
      int r = slot_map[i];
      int rss = cfun->machine->reg_save_slot[r];

      if (!mep_call_saves_register (r))
	continue;

      if ((r == TP_REGNO || r == GP_REGNO || r == LP_REGNO)
	  && (!mep_reg_set_in_function (r)
	      && !mep_interrupt_p ()))
	continue;

      rsize = mep_reg_size(r);
      skip = rss - (sp+rsize);
      if (skip)
	fprintf (file, "\t#         %3d bytes for alignment\n", skip);
      fprintf (file, "\t#         %3d bytes for saved %-3s   %3d($sp)\n",
	       rsize, reg_names[r], sp_offset - rss);
      sp = rss;
    }

  skip = reg_save_size - sp;
  if (skip)
    fprintf (file, "\t#         %3d bytes for alignment\n", skip);

  if (frame_pointer_needed)
    fprintf (file, "\t# FP ---> ---------- %d (sp-%d)\n", reg_save_size, sp_offset-reg_save_size);
  if (local)
    fprintf (file, "\t#         %3d bytes for local vars\n", local);
  if (ffill)
    fprintf (file, "\t#         %3d bytes for alignment\n", ffill);
  if (crtl->outgoing_args_size)
    fprintf (file, "\t#         %3d bytes for outgoing args\n",
	     crtl->outgoing_args_size);
  fprintf (file, "\t# SP ---> ---------- %d\n", sp_offset);
  fprintf (file, "\t#\n");
}


static int mep_prevent_lp_restore = 0;
static int mep_sibcall_epilogue = 0;

void
mep_expand_epilogue (void)
{
  int i, sp_offset = 0;
  int reg_save_size = 0;
  int frame_size;
  int lp_temp = LP_REGNO, lp_slot = -1;
  int really_need_stack_frame = get_frame_size() + crtl->outgoing_args_size;
  int interrupt_handler = mep_interrupt_p ();

  if (profile_arc_flag == 2)
    emit_insn (gen_mep_bb_trace_ret ());

  reg_save_size = mep_elimination_offset (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM);
  frame_size = mep_elimination_offset (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM);

  really_need_stack_frame |= mep_assign_save_slots (reg_save_size);

  if (frame_pointer_needed)
    {
      /* If we have a frame pointer, we won't have a reliable stack
	 pointer (alloca, you know), so rebase SP from FP */
      emit_move_insn (gen_rtx_REG (SImode, SP_REGNO),
		      gen_rtx_REG (SImode, FP_REGNO));
      sp_offset = reg_save_size;
    }
  else
    {
      /* SP is right under our local variable space.  Adjust it if
	 needed.  */
      sp_offset = reg_save_size + frame_size;
      if (sp_offset >= 128)
	{
	  add_constant (SP_REGNO, SP_REGNO, frame_size, 0);
	  sp_offset -= frame_size;
	}
    }

  /* This is backwards so that we restore the control and coprocessor
     registers before the temporary registers we use to restore
     them.  */
  for (i=FIRST_PSEUDO_REGISTER-1; i>=1; i--)
    if (mep_call_saves_register (i))
      {
	enum machine_mode rmode;
	int rss = cfun->machine->reg_save_slot[i];

	if (mep_reg_size (i) == 8)
	  rmode = DImode;
	else
	  rmode = SImode;

	if ((i == TP_REGNO || i == GP_REGNO || i == LP_REGNO)
	    && !(mep_reg_set_in_function (i) || interrupt_handler))
	  continue;
	if (mep_prevent_lp_restore && i == LP_REGNO)
	  continue;
	if (!mep_prevent_lp_restore
	    && !interrupt_handler
	    && (i == 10 || i == 11))
	  continue;
  
	if (GR_REGNO_P (i) || LOADABLE_CR_REGNO_P (i))
	  emit_move_insn (gen_rtx_REG (rmode, i),
			  gen_rtx_MEM (rmode,
				       plus_constant (Pmode, stack_pointer_rtx,
						      sp_offset - rss)));
	else
	  {
	    if (i == LP_REGNO && !mep_sibcall_epilogue && !interrupt_handler)
	      /* Defer this one so we can jump indirect rather than
		 copying the RA to $lp and "ret".  EH epilogues
		 automatically skip this anyway.  */
	      lp_slot = sp_offset-rss;
	    else
	      {
		emit_move_insn (gen_rtx_REG (rmode, REGSAVE_CONTROL_TEMP),
				gen_rtx_MEM (rmode,
					     plus_constant (Pmode,
							    stack_pointer_rtx,
							    sp_offset-rss)));
		emit_move_insn (gen_rtx_REG (rmode, i),
				gen_rtx_REG (rmode, REGSAVE_CONTROL_TEMP));
	      }
	  }
      }
  if (lp_slot != -1)
    {
      /* Restore this one last so we know it will be in the temp
	 register when we return by jumping indirectly via the temp.  */
      emit_move_insn (gen_rtx_REG (SImode, REGSAVE_CONTROL_TEMP),
		      gen_rtx_MEM (SImode,
				   plus_constant (Pmode, stack_pointer_rtx,
						  lp_slot)));
      lp_temp = REGSAVE_CONTROL_TEMP;
    }


  add_constant (SP_REGNO, SP_REGNO, sp_offset, 0);

  if (crtl->calls_eh_return && mep_prevent_lp_restore)
    emit_insn (gen_addsi3 (gen_rtx_REG (SImode, SP_REGNO),
			   gen_rtx_REG (SImode, SP_REGNO),
			   cfun->machine->eh_stack_adjust));

  if (mep_sibcall_epilogue)
    return;

  if (mep_disinterrupt_p ())
    emit_insn (gen_mep_enable_int ());

  if (mep_prevent_lp_restore)
    {
      emit_jump_insn (gen_eh_return_internal ());
      emit_barrier ();
    }
  else if (interrupt_handler)
    emit_jump_insn (gen_mep_reti ());
  else
    emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode, lp_temp)));
}

void
mep_expand_eh_return (rtx *operands)
{
  if (GET_CODE (operands[0]) != REG || REGNO (operands[0]) != LP_REGNO)
    {
      rtx ra = gen_rtx_REG (Pmode, LP_REGNO);
      emit_move_insn (ra, operands[0]);
      operands[0] = ra;
    }

  emit_insn (gen_eh_epilogue (operands[0]));
}

void
mep_emit_eh_epilogue (rtx *operands ATTRIBUTE_UNUSED)
{
  cfun->machine->eh_stack_adjust = gen_rtx_REG (Pmode, 0);
  mep_prevent_lp_restore = 1;
  mep_expand_epilogue ();
  mep_prevent_lp_restore = 0;
}

void
mep_expand_sibcall_epilogue (void)
{
  mep_sibcall_epilogue = 1;
  mep_expand_epilogue ();
  mep_sibcall_epilogue = 0;
}

static bool
mep_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  if (decl == NULL)
    return false;

  if (mep_section_tag (DECL_RTL (decl)) == 'f')
    return false;

  /* Can't call to a sibcall from an interrupt or disinterrupt function.  */
  if (mep_interrupt_p () || mep_disinterrupt_p ())
    return false;

  return true;
}

rtx
mep_return_stackadj_rtx (void)
{
  return gen_rtx_REG (SImode, 10);
}

rtx
mep_return_handler_rtx (void)
{
  return gen_rtx_REG (SImode, LP_REGNO);
}

void
mep_function_profiler (FILE *file)
{
  /* Always right at the beginning of the function.  */
  fprintf (file, "\t# mep function profiler\n");
  fprintf (file, "\tadd\t$sp, -8\n");
  fprintf (file, "\tsw\t$0, ($sp)\n");
  fprintf (file, "\tldc\t$0, $lp\n");
  fprintf (file, "\tsw\t$0, 4($sp)\n");
  fprintf (file, "\tbsr\t__mep_mcount\n");
  fprintf (file, "\tlw\t$0, 4($sp)\n");
  fprintf (file, "\tstc\t$0, $lp\n");
  fprintf (file, "\tlw\t$0, ($sp)\n");
  fprintf (file, "\tadd\t$sp, 8\n\n");
}

const char *
mep_emit_bb_trace_ret (void)
{
  fprintf (asm_out_file, "\t# end of block profiling\n");
  fprintf (asm_out_file, "\tadd\t$sp, -8\n");
  fprintf (asm_out_file, "\tsw\t$0, ($sp)\n");
  fprintf (asm_out_file, "\tldc\t$0, $lp\n");
  fprintf (asm_out_file, "\tsw\t$0, 4($sp)\n");
  fprintf (asm_out_file, "\tbsr\t__bb_trace_ret\n");
  fprintf (asm_out_file, "\tlw\t$0, 4($sp)\n");
  fprintf (asm_out_file, "\tstc\t$0, $lp\n");
  fprintf (asm_out_file, "\tlw\t$0, ($sp)\n");
  fprintf (asm_out_file, "\tadd\t$sp, 8\n\n");
  return "";
}

#undef SAVE
#undef RESTORE

/* Operand Printing.  */

void
mep_print_operand_address (FILE *stream, rtx address)
{
  if (GET_CODE (address) == MEM)
    address = XEXP (address, 0);
  else
    /* cf: gcc.dg/asm-4.c.  */
    gcc_assert (GET_CODE (address) == REG);

  mep_print_operand (stream, address, 0);
}

static struct
{
  char code;
  const char *pattern;
  const char *format;
}
const conversions[] =
{
  { 0, "r", "0" },
  { 0, "m+ri", "3(2)" },
  { 0, "mr", "(1)" },
  { 0, "ms", "(1)" },
  { 0, "ml", "(1)" },
  { 0, "mLrs", "%lo(3)(2)" },
  { 0, "mLr+si", "%lo(4+5)(2)" },
  { 0, "m+ru2s", "%tpoff(5)(2)" },
  { 0, "m+ru3s", "%sdaoff(5)(2)" },
  { 0, "m+r+u2si", "%tpoff(6+7)(2)" },
  { 0, "m+ru2+si", "%tpoff(6+7)(2)" },
  { 0, "m+r+u3si", "%sdaoff(6+7)(2)" },
  { 0, "m+ru3+si", "%sdaoff(6+7)(2)" },
  { 0, "mi", "(1)" },
  { 0, "m+si", "(2+3)" },
  { 0, "m+li", "(2+3)" },
  { 0, "i", "0" },
  { 0, "s", "0" },
  { 0, "+si", "1+2" },
  { 0, "+u2si", "%tpoff(3+4)" },
  { 0, "+u3si", "%sdaoff(3+4)" },
  { 0, "l", "0" },
  { 'b', "i", "0" },
  { 'B', "i", "0" },
  { 'U', "i", "0" },
  { 'h', "i", "0" },
  { 'h', "Hs", "%hi(1)" },
  { 'I', "i", "0" },
  { 'I', "u2s", "%tpoff(2)" },
  { 'I', "u3s", "%sdaoff(2)" },
  { 'I', "+u2si", "%tpoff(3+4)" },
  { 'I', "+u3si", "%sdaoff(3+4)" },
  { 'J', "i", "0" },
  { 'P', "mr", "(1\\+),\\0" },
  { 'x', "i", "0" },
  { 0, 0, 0 }
};

static int
unique_bit_in (HOST_WIDE_INT i)
{
  switch (i & 0xff)
    {
    case 0x01: case 0xfe: return 0;
    case 0x02: case 0xfd: return 1;
    case 0x04: case 0xfb: return 2;
    case 0x08: case 0xf7: return 3;
    case 0x10: case 0x7f: return 4;
    case 0x20: case 0xbf: return 5;
    case 0x40: case 0xdf: return 6;
    case 0x80: case 0xef: return 7;
    default:
      gcc_unreachable ();
    }
}

static int
bit_size_for_clip (HOST_WIDE_INT i)
{
  int rv;

  for (rv = 0; rv < 31; rv ++)
    if (((HOST_WIDE_INT) 1 << rv) > i)
      return rv + 1;
  gcc_unreachable ();
}

/* Print an operand to a assembler instruction.  */

void
mep_print_operand (FILE *file, rtx x, int code)
{
  int i, j;
  const char *real_name;

  if (code == '<')
    {
      /* Print a mnemonic to do CR <- CR moves.  Find out which intrinsic
	 we're using, then skip over the "mep_" part of its name.  */
      const struct cgen_insn *insn;

      if (mep_get_move_insn (mep_cmov, &insn))
	fputs (cgen_intrinsics[insn->intrinsic] + 4, file);
      else
	mep_intrinsic_unavailable (mep_cmov);
      return;
    }
  if (code == 'L')
    {
      switch (GET_CODE (x))
	{
	case AND:
	  fputs ("clr", file);
	  return;
	case IOR:
	  fputs ("set", file);
	  return;
	case XOR:
	  fputs ("not", file);
	  return;
	default:
	  output_operand_lossage ("invalid %%L code");
	}
    }
  if (code == 'M')
    {
      /* Print the second operand of a CR <- CR move.  If we're using
	 a two-operand instruction (i.e., a real cmov), then just print
	 the operand normally.  If we're using a "reg, reg, immediate"
	 instruction such as caddi3, print the operand followed by a
	 zero field.  If we're using a three-register instruction,
	 print the operand twice.  */
      const struct cgen_insn *insn;

      mep_print_operand (file, x, 0);
      if (mep_get_move_insn (mep_cmov, &insn)
	  && insn_data[insn->icode].n_operands == 3)
	{
	  fputs (", ", file);
	  if (insn_data[insn->icode].operand[2].predicate (x, VOIDmode))
	    mep_print_operand (file, x, 0);
	  else
	    mep_print_operand (file, const0_rtx, 0);
	}
      return;
    }

  encode_pattern (x);
  for (i = 0; conversions[i].pattern; i++)
    if (conversions[i].code == code
	&& strcmp(conversions[i].pattern, pattern) == 0)
      {
	for (j = 0; conversions[i].format[j]; j++)
	  if (conversions[i].format[j] == '\\')
	    {
	      fputc (conversions[i].format[j+1], file);
	      j++;
	    }
	  else if (ISDIGIT(conversions[i].format[j]))
	    {
	      rtx r = patternr[conversions[i].format[j] - '0'];
	      switch (GET_CODE (r))
		{
		case REG:
		  fprintf (file, "%s", reg_names [REGNO (r)]);
		  break;
		case CONST_INT:
		  switch (code)
		    {
		    case 'b':
		      fprintf (file, "%d", unique_bit_in (INTVAL (r)));
		      break;
		    case 'B':
		      fprintf (file, "%d", bit_size_for_clip (INTVAL (r)));
		      break;
		    case 'h':
		      fprintf (file, "0x%x", ((int) INTVAL (r) >> 16) & 0xffff);
		      break;
		    case 'U':
		      fprintf (file, "%d", bit_size_for_clip (INTVAL (r)) - 1);
		      break;
		    case 'J':
		      fprintf (file, "0x%x", (int) INTVAL (r) & 0xffff);
		      break;
		    case 'x':
		      if (INTVAL (r) & ~(HOST_WIDE_INT)0xff
			  && !(INTVAL (r) & 0xff))
			fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL(r));
		      else
			fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL(r));
		      break;
		    case 'I':
		      if (INTVAL (r) & ~(HOST_WIDE_INT)0xff
			  && conversions[i].format[j+1] == 0)
			{
			  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (r));
			  fprintf (file, " # 0x%x", (int) INTVAL(r) & 0xffff);
			}
		      else
			fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL(r));
		      break;
		    default:
		      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL(r));
		      break;
		    }
		  break;
		case CONST_DOUBLE:
		  fprintf(file, "[const_double 0x%lx]",
			  (unsigned long) CONST_DOUBLE_HIGH(r));
		  break;
		case SYMBOL_REF:
		  real_name = targetm.strip_name_encoding (XSTR (r, 0));
		  assemble_name (file, real_name);
		  break;
		case LABEL_REF:
		  output_asm_label (r);
		  break;
		default:
		  fprintf (stderr, "don't know how to print this operand:");
		  debug_rtx (r);
		  gcc_unreachable ();
		}
	    }
	  else
	    {
	      if (conversions[i].format[j] == '+'
		  && (!code || code == 'I')
		  && ISDIGIT (conversions[i].format[j+1])
		  && GET_CODE (patternr[conversions[i].format[j+1] - '0']) == CONST_INT
		  && INTVAL (patternr[conversions[i].format[j+1] - '0']) < 0)
		continue;
	      fputc(conversions[i].format[j], file);
	    }
	break;
      }
  if (!conversions[i].pattern)
    {
      error ("unconvertible operand %c %qs", code?code:'-', pattern);
      debug_rtx(x);
    }

  return;
}

void
mep_final_prescan_insn (rtx insn, rtx *operands ATTRIBUTE_UNUSED,
			int noperands ATTRIBUTE_UNUSED)
{
  /* Despite the fact that MeP is perfectly capable of branching and
     doing something else in the same bundle, gcc does jump
     optimization *after* scheduling, so we cannot trust the bundling
     flags on jump instructions.  */
  if (GET_MODE (insn) == BImode
      && get_attr_slots (insn) != SLOTS_CORE)
    fputc ('+', asm_out_file);
}

/* Function args in registers.  */

static void
mep_setup_incoming_varargs (cumulative_args_t cum,
			    enum machine_mode mode ATTRIBUTE_UNUSED,
			    tree type ATTRIBUTE_UNUSED, int *pretend_size,
			    int second_time ATTRIBUTE_UNUSED)
{
  int nsave = 4 - (get_cumulative_args (cum)->nregs + 1);

  if (nsave > 0)
    cfun->machine->arg_regs_to_save = nsave;
  *pretend_size = nsave * 4;
}

static int
bytesize (const_tree type, enum machine_mode mode)
{
  if (mode == BLKmode)
    return int_size_in_bytes (type);
  return GET_MODE_SIZE (mode);
}

static rtx
mep_expand_builtin_saveregs (void)
{
  int bufsize, i, ns;
  rtx regbuf;

  ns = cfun->machine->arg_regs_to_save;
  if (TARGET_IVC2)
    {
      bufsize = 8 * ((ns + 1) / 2) + 8 * ns;
      regbuf = assign_stack_local (SImode, bufsize, 64);
    }
  else
    {
      bufsize = ns * 4;
      regbuf = assign_stack_local (SImode, bufsize, 32);
    }

  move_block_from_reg (5-ns, regbuf, ns);

  if (TARGET_IVC2)
    {
      rtx tmp = gen_rtx_MEM (DImode, XEXP (regbuf, 0));
      int ofs = 8 * ((ns+1)/2);

      for (i=0; i<ns; i++)
	{
	  int rn = (4-ns) + i + 49;
	  rtx ptr;

	  ptr = offset_address (tmp, GEN_INT (ofs), 2);
	  emit_move_insn (ptr, gen_rtx_REG (DImode, rn));
	  ofs += 8;
	}
    }
  return XEXP (regbuf, 0);
}

#define VECTOR_TYPE_P(t) (TREE_CODE(t) == VECTOR_TYPE)

static tree
mep_build_builtin_va_list (void)
{
  tree f_next_gp, f_next_gp_limit, f_next_cop, f_next_stack;
  tree record;


  record = (*lang_hooks.types.make_type) (RECORD_TYPE);

  f_next_gp = build_decl (BUILTINS_LOCATION, FIELD_DECL,
			  get_identifier ("__va_next_gp"), ptr_type_node);
  f_next_gp_limit = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				get_identifier ("__va_next_gp_limit"),
				ptr_type_node);
  f_next_cop = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("__va_next_cop"),
			   ptr_type_node);
  f_next_stack = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("__va_next_stack"),
			     ptr_type_node);

  DECL_FIELD_CONTEXT (f_next_gp) = record;
  DECL_FIELD_CONTEXT (f_next_gp_limit) = record;
  DECL_FIELD_CONTEXT (f_next_cop) = record;
  DECL_FIELD_CONTEXT (f_next_stack) = record;

  TYPE_FIELDS (record) = f_next_gp;
  DECL_CHAIN (f_next_gp) = f_next_gp_limit;
  DECL_CHAIN (f_next_gp_limit) = f_next_cop;
  DECL_CHAIN (f_next_cop) = f_next_stack;

  layout_type (record);

  return record;
}

static void
mep_expand_va_start (tree valist, rtx nextarg)
{
  tree f_next_gp, f_next_gp_limit, f_next_cop, f_next_stack;
  tree next_gp, next_gp_limit, next_cop, next_stack;
  tree t, u;
  int ns;

  ns = cfun->machine->arg_regs_to_save;

  f_next_gp = TYPE_FIELDS (va_list_type_node);
  f_next_gp_limit = DECL_CHAIN (f_next_gp);
  f_next_cop = DECL_CHAIN (f_next_gp_limit);
  f_next_stack = DECL_CHAIN (f_next_cop);

  next_gp = build3 (COMPONENT_REF, TREE_TYPE (f_next_gp), valist, f_next_gp,
		    NULL_TREE);
  next_gp_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_gp_limit),
			  valist, f_next_gp_limit, NULL_TREE);
  next_cop = build3 (COMPONENT_REF, TREE_TYPE (f_next_cop), valist, f_next_cop,
		     NULL_TREE);
  next_stack = build3 (COMPONENT_REF, TREE_TYPE (f_next_stack),
		       valist, f_next_stack, NULL_TREE);

  /* va_list.next_gp = expand_builtin_saveregs (); */
  u = make_tree (sizetype, expand_builtin_saveregs ());
  u = fold_convert (ptr_type_node, u);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_gp, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* va_list.next_gp_limit = va_list.next_gp + 4 * ns; */
  u = fold_build_pointer_plus_hwi (u, 4 * ns);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_gp_limit, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  u = fold_build_pointer_plus_hwi (u, 8 * ((ns+1)/2));
  /* va_list.next_cop = ROUND_UP(va_list.next_gp_limit,8); */
  t = build2 (MODIFY_EXPR, ptr_type_node, next_cop, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* va_list.next_stack = nextarg; */
  u = make_tree (ptr_type_node, nextarg);
  t = build2 (MODIFY_EXPR, ptr_type_node, next_stack, u);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

static tree
mep_gimplify_va_arg_expr (tree valist, tree type,
			  gimple_seq *pre_p,
			  gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size, rsize;
  bool by_reference, ivc2_vec;
  tree f_next_gp, f_next_gp_limit, f_next_cop, f_next_stack;
  tree next_gp, next_gp_limit, next_cop, next_stack;
  tree label_sover, label_selse;
  tree tmp, res_addr;

  ivc2_vec = TARGET_IVC2 && VECTOR_TYPE_P (type);

  size = int_size_in_bytes (type);
  by_reference = (size > (ivc2_vec ? 8 : 4)) || (size <= 0);

  if (by_reference)
    {
      type = build_pointer_type (type);
      size = 4;
    }
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;

  f_next_gp = TYPE_FIELDS (va_list_type_node);
  f_next_gp_limit = DECL_CHAIN (f_next_gp);
  f_next_cop = DECL_CHAIN (f_next_gp_limit);
  f_next_stack = DECL_CHAIN (f_next_cop);

  next_gp = build3 (COMPONENT_REF, TREE_TYPE (f_next_gp), valist, f_next_gp,
		    NULL_TREE);
  next_gp_limit = build3 (COMPONENT_REF, TREE_TYPE (f_next_gp_limit),
			  valist, f_next_gp_limit, NULL_TREE);
  next_cop = build3 (COMPONENT_REF, TREE_TYPE (f_next_cop), valist, f_next_cop,
		     NULL_TREE);
  next_stack = build3 (COMPONENT_REF, TREE_TYPE (f_next_stack),
		       valist, f_next_stack, NULL_TREE);

  /* if f_next_gp < f_next_gp_limit
       IF (VECTOR_P && IVC2)
         val = *f_next_cop;
       ELSE
         val = *f_next_gp;
       f_next_gp += 4;
       f_next_cop += 8;
     else
       label_selse:
       val = *f_next_stack;
       f_next_stack += rsize;
     label_sover:
  */

  label_sover = create_artificial_label (UNKNOWN_LOCATION);
  label_selse = create_artificial_label (UNKNOWN_LOCATION);
  res_addr = create_tmp_var (ptr_type_node, NULL);

  tmp = build2 (GE_EXPR, boolean_type_node, next_gp,
		unshare_expr (next_gp_limit));
  tmp = build3 (COND_EXPR, void_type_node, tmp,
		build1 (GOTO_EXPR, void_type_node,
			unshare_expr (label_selse)),
		NULL_TREE);
  gimplify_and_add (tmp, pre_p);

  if (ivc2_vec)
    {
      tmp = build2 (MODIFY_EXPR, void_type_node, res_addr, next_cop);
      gimplify_and_add (tmp, pre_p);
    }
  else
    {
      tmp = build2 (MODIFY_EXPR, void_type_node, res_addr, next_gp);
      gimplify_and_add (tmp, pre_p);
    }

  tmp = fold_build_pointer_plus_hwi (unshare_expr (next_gp), 4);
  gimplify_assign (unshare_expr (next_gp), tmp, pre_p);

  tmp = fold_build_pointer_plus_hwi (unshare_expr (next_cop), 8);
  gimplify_assign (unshare_expr (next_cop), tmp, pre_p);

  tmp = build1 (GOTO_EXPR, void_type_node, unshare_expr (label_sover));
  gimplify_and_add (tmp, pre_p);

  /* - - */

  tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (label_selse));
  gimplify_and_add (tmp, pre_p);

  tmp = build2 (MODIFY_EXPR, void_type_node, res_addr, unshare_expr (next_stack));
  gimplify_and_add (tmp, pre_p);

  tmp = fold_build_pointer_plus_hwi (unshare_expr (next_stack), rsize);
  gimplify_assign (unshare_expr (next_stack), tmp, pre_p);

  /* - - */

  tmp = build1 (LABEL_EXPR, void_type_node, unshare_expr (label_sover));
  gimplify_and_add (tmp, pre_p);

  res_addr = fold_convert (build_pointer_type (type), res_addr);

  if (by_reference)
    res_addr = build_va_arg_indirect_ref (res_addr);

  return build_va_arg_indirect_ref (res_addr);
}

void
mep_init_cumulative_args (CUMULATIVE_ARGS *pcum, tree fntype,
			  rtx libname ATTRIBUTE_UNUSED,
			  tree fndecl ATTRIBUTE_UNUSED)
{
  pcum->nregs = 0;

  if (fntype && lookup_attribute ("vliw", TYPE_ATTRIBUTES (fntype)))
    pcum->vliw = 1;
  else
    pcum->vliw = 0;
}

/* The ABI is thus: Arguments are in $1, $2, $3, $4, stack.  Arguments
   larger than 4 bytes are passed indirectly.  Return value in 0,
   unless bigger than 4 bytes, then the caller passes a pointer as the
   first arg.  For varargs, we copy $1..$4 to the stack.  */

static rtx
mep_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		  const_tree type ATTRIBUTE_UNUSED,
		  bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  /* VOIDmode is a signal for the backend to pass data to the call
     expander via the second operand to the call pattern.  We use
     this to determine whether to use "jsr" or "jsrv".  */
  if (mode == VOIDmode)
    return GEN_INT (cum->vliw);

  /* If we havn't run out of argument registers, return the next.  */
  if (cum->nregs < 4)
    {
      if (type && TARGET_IVC2 && VECTOR_TYPE_P (type))
	return gen_rtx_REG (mode, cum->nregs + 49);
      else
	return gen_rtx_REG (mode, cum->nregs + 1);
    }

  /* Otherwise the argument goes on the stack.  */
  return NULL_RTX;
}

static bool
mep_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
		       enum machine_mode mode,
		       const_tree        type,
		       bool              named ATTRIBUTE_UNUSED)
{
  int size = bytesize (type, mode);

  /* This is non-obvious, but yes, large values passed after we've run
     out of registers are *still* passed by reference - we put the
     address of the parameter on the stack, as well as putting the
     parameter itself elsewhere on the stack.  */

  if (size <= 0 || size > 8)
    return true;
  if (size <= 4)
    return false;
  if (TARGET_IVC2 && get_cumulative_args (cum)->nregs < 4
      && type != NULL_TREE && VECTOR_TYPE_P (type))
    return false;
  return true;
}

static void
mep_function_arg_advance (cumulative_args_t pcum,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type ATTRIBUTE_UNUSED,
			  bool named ATTRIBUTE_UNUSED)
{
  get_cumulative_args (pcum)->nregs += 1;
}

bool
mep_return_in_memory (const_tree type, const_tree decl ATTRIBUTE_UNUSED)
{
  int size = bytesize (type, BLKmode);
  if (TARGET_IVC2 && VECTOR_TYPE_P (type))
    return size > 0 && size <= 8 ? 0 : 1;
  return size > 0 && size <= 4 ? 0 : 1;
}

static bool
mep_narrow_volatile_bitfield (void)
{
  return true;
  return false;
}

/* Implement FUNCTION_VALUE.  All values are returned in $0.  */

rtx
mep_function_value (const_tree type, const_tree func ATTRIBUTE_UNUSED)
{
  if (TARGET_IVC2 && VECTOR_TYPE_P (type))
    return gen_rtx_REG (TYPE_MODE (type), 48);
  return gen_rtx_REG (TYPE_MODE (type), RETURN_VALUE_REGNUM);
}

/* Implement LIBCALL_VALUE, using the same rules as mep_function_value.  */

rtx
mep_libcall_value (enum machine_mode mode)
{
  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* Handle pipeline hazards.  */

typedef enum { op_none, op_stc, op_fsft, op_ret } op_num;
static const char *opnames[] = { "", "stc", "fsft", "ret" };

static int prev_opcode = 0;

/* This isn't as optimal as it could be, because we don't know what
   control register the STC opcode is storing in.  We only need to add
   the nop if it's the relevant register, but we add it for irrelevant
   registers also.  */

void
mep_asm_output_opcode (FILE *file, const char *ptr)
{
  int this_opcode = op_none;
  const char *hazard = 0;

  switch (*ptr)
    {
    case 'f':
      if (strncmp (ptr, "fsft", 4) == 0 && !ISGRAPH (ptr[4]))
	this_opcode = op_fsft;
      break;
    case 'r':
      if (strncmp (ptr, "ret", 3) == 0 && !ISGRAPH (ptr[3]))
	this_opcode = op_ret;
      break;
    case 's':
      if (strncmp (ptr, "stc", 3) == 0 && !ISGRAPH (ptr[3]))
	this_opcode = op_stc;
      break;
    }

  if (prev_opcode == op_stc && this_opcode == op_fsft)
    hazard = "nop";
  if (prev_opcode == op_stc && this_opcode == op_ret)
    hazard = "nop";

  if (hazard)
    fprintf(file, "%s\t# %s-%s hazard\n\t",
	    hazard, opnames[prev_opcode], opnames[this_opcode]);

  prev_opcode = this_opcode;
}

/* Handle attributes.  */

static tree
mep_validate_based_tiny (tree *node, tree name, tree args,
			 int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  if (TREE_CODE (*node) != VAR_DECL
      && TREE_CODE (*node) != POINTER_TYPE
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (0, "%qE attribute only applies to variables", name);
      *no_add = true;
    }
  else if (args == NULL_TREE && TREE_CODE (*node) == VAR_DECL)
    {
      if (! (TREE_PUBLIC (*node) || TREE_STATIC (*node)))
	{
	  warning (0, "address region attributes not allowed with auto storage class");
	  *no_add = true;
	}
      /* Ignore storage attribute of pointed to variable: char __far * x;  */
      if (TREE_TYPE (*node) && TREE_CODE (TREE_TYPE (*node)) == POINTER_TYPE)
	{
	  warning (0, "address region attributes on pointed-to types ignored");
	  *no_add = true;
	}
    }
  
  return NULL_TREE;
}

static int
mep_multiple_address_regions (tree list, bool check_section_attr)
{
  tree a;
  int count_sections = 0;
  int section_attr_count = 0;

  for (a = list; a; a = TREE_CHAIN (a))
    {
      if (is_attribute_p ("based", TREE_PURPOSE (a))
	  || is_attribute_p ("tiny", TREE_PURPOSE (a))
	  || is_attribute_p ("near", TREE_PURPOSE (a))
	  || is_attribute_p ("far", TREE_PURPOSE (a))
	  || is_attribute_p ("io", TREE_PURPOSE (a)))
	count_sections ++;
      if (check_section_attr)
	section_attr_count += is_attribute_p ("section", TREE_PURPOSE (a));
    }
	
  if (check_section_attr)
    return section_attr_count;
  else
    return count_sections;
}

#define MEP_ATTRIBUTES(decl) \
  (TYPE_P (decl)) ? TYPE_ATTRIBUTES (decl) \
                : DECL_ATTRIBUTES (decl) \
                  ? (DECL_ATTRIBUTES (decl)) \
		  : TYPE_ATTRIBUTES (TREE_TYPE (decl))

static tree
mep_validate_near_far (tree *node, tree name, tree args,
		       int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  if (TREE_CODE (*node) != VAR_DECL
      && TREE_CODE (*node) != FUNCTION_DECL
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != POINTER_TYPE
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (0, "%qE attribute only applies to variables and functions",
	       name);
      *no_add = true;
    }
  else if (args == NULL_TREE && TREE_CODE (*node) == VAR_DECL)
    {
      if (! (TREE_PUBLIC (*node) || TREE_STATIC (*node)))
	{
	  warning (0, "address region attributes not allowed with auto storage class");
	  *no_add = true;
	}
      /* Ignore storage attribute of pointed to variable: char __far * x;  */
      if (TREE_TYPE (*node) && TREE_CODE (TREE_TYPE (*node)) == POINTER_TYPE)
	{
	  warning (0, "address region attributes on pointed-to types ignored");
	  *no_add = true;
	}
    }
  else if (mep_multiple_address_regions (MEP_ATTRIBUTES (*node), false) > 0)
    {
      warning (0, "duplicate address region attribute %qE in declaration of %qE on line %d",
	       name, DECL_NAME (*node), DECL_SOURCE_LINE (*node));
      DECL_ATTRIBUTES (*node) = NULL_TREE;
    }
  return NULL_TREE;
}

static tree
mep_validate_disinterrupt (tree *node, tree name, tree args ATTRIBUTE_UNUSED,
			   int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  if (TREE_CODE (*node) != FUNCTION_DECL
      && TREE_CODE (*node) != METHOD_TYPE)
    {
      warning (0, "%qE attribute only applies to functions", name);
      *no_add = true;
    }
  return NULL_TREE;
}

static tree
mep_validate_interrupt (tree *node, tree name, tree args ATTRIBUTE_UNUSED,
			int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  tree function_type;

  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (0, "%qE attribute only applies to functions", name);
      *no_add = true;
      return NULL_TREE;
    }

  if (DECL_DECLARED_INLINE_P (*node))
    error ("cannot inline interrupt function %qE", DECL_NAME (*node));
  DECL_UNINLINABLE (*node) = 1;

  function_type = TREE_TYPE (*node);

  if (TREE_TYPE (function_type) != void_type_node)
    error ("interrupt function must have return type of void");

  if (prototype_p (function_type)
      && (TREE_VALUE (TYPE_ARG_TYPES (function_type)) != void_type_node
	  || TREE_CHAIN (TYPE_ARG_TYPES (function_type)) != NULL_TREE))
    error ("interrupt function must have no arguments");

  return NULL_TREE;
}

static tree
mep_validate_io_cb (tree *node, tree name, tree args,
		    int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  if (TREE_CODE (*node) != VAR_DECL)
    {
      warning (0, "%qE attribute only applies to variables", name);
      *no_add = true;
    }

  if (args != NULL_TREE)
    {
      if (TREE_CODE (TREE_VALUE (args)) == NON_LVALUE_EXPR)
	TREE_VALUE (args) = TREE_OPERAND (TREE_VALUE (args), 0);
      if (TREE_CODE (TREE_VALUE (args)) != INTEGER_CST)
	{
	  warning (0, "%qE attribute allows only an integer constant argument",
		   name);
	  *no_add = true;
	}
    }

  if (*no_add == false && !TARGET_IO_NO_VOLATILE)
    TREE_THIS_VOLATILE (*node) = 1;

  return NULL_TREE;
}

static tree
mep_validate_vliw (tree *node, tree name, tree args ATTRIBUTE_UNUSED, 
		   int flags ATTRIBUTE_UNUSED, bool *no_add)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != FUNCTION_DECL
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      static int gave_pointer_note = 0;
      static int gave_array_note = 0;
      static const char * given_type = NULL;
 
      given_type = get_tree_code_name (TREE_CODE (*node));
      if (TREE_CODE (*node) == POINTER_TYPE)
 	given_type = "pointers";
      if (TREE_CODE (*node) == ARRAY_TYPE)
 	given_type = "arrays";
 
      if (given_type)
 	warning (0, "%qE attribute only applies to functions, not %s",
 		 name, given_type);
      else
 	warning (0, "%qE attribute only applies to functions",
 		 name);
      *no_add = true;
 
      if (TREE_CODE (*node) == POINTER_TYPE
 	  && !gave_pointer_note)
 	{
 	  inform (input_location,
 	          "to describe a pointer to a VLIW function, use syntax like this:\n%s",
 	          "   typedef int (__vliw *vfuncptr) ();");
 	  gave_pointer_note = 1;
 	}
 
      if (TREE_CODE (*node) == ARRAY_TYPE
 	  && !gave_array_note)
 	{
 	  inform (input_location,
 	          "to describe an array of VLIW function pointers, use syntax like this:\n%s",
 	          "   typedef int (__vliw *vfuncptr[]) ();");
 	  gave_array_note = 1;
 	}
    }
  if (!TARGET_VLIW)
    error ("VLIW functions are not allowed without a VLIW configuration");
  return NULL_TREE;
}

static const struct attribute_spec mep_attribute_table[11] =
{
  /* name         min max decl   type   func   handler
     affects_type_identity */
  { "based",        0, 0, false, false, false, mep_validate_based_tiny, false },
  { "tiny",         0, 0, false, false, false, mep_validate_based_tiny, false },
  { "near",         0, 0, false, false, false, mep_validate_near_far, false },
  { "far",          0, 0, false, false, false, mep_validate_near_far, false },
  { "disinterrupt", 0, 0, false, false, false, mep_validate_disinterrupt,
    false },
  { "interrupt",    0, 0, false, false, false, mep_validate_interrupt, false },
  { "io",           0, 1, false, false, false, mep_validate_io_cb, false },
  { "cb",           0, 1, false, false, false, mep_validate_io_cb, false },
  { "vliw",         0, 0, false, true,  false, mep_validate_vliw, false },
  { NULL,           0, 0, false, false, false, NULL, false }
};

static bool
mep_function_attribute_inlinable_p (const_tree callee)
{
  tree attrs = TYPE_ATTRIBUTES (TREE_TYPE (callee));
  if (!attrs) attrs = DECL_ATTRIBUTES (callee);
  return (lookup_attribute ("disinterrupt", attrs) == 0
	  && lookup_attribute ("interrupt", attrs) == 0);
}

static bool
mep_can_inline_p (tree caller, tree callee)
{
  if (TREE_CODE (callee) == ADDR_EXPR)
    callee = TREE_OPERAND (callee, 0);
 
  if (!mep_vliw_function_p (caller)
      && mep_vliw_function_p (callee))
    {
      return false;
    }
  return true;
}

#define FUNC_CALL		1
#define FUNC_DISINTERRUPT	2


struct GTY(()) pragma_entry {
  int used;
  int flag;
  const char *funcname;
};
typedef struct pragma_entry pragma_entry;

/* Hash table of farcall-tagged sections.  */
static GTY((param_is (pragma_entry))) htab_t pragma_htab;

static int
pragma_entry_eq (const void *p1, const void *p2)
{
  const pragma_entry *old = (const pragma_entry *) p1;
  const char *new_name = (const char *) p2;

  return strcmp (old->funcname, new_name) == 0;
}

static hashval_t
pragma_entry_hash (const void *p)
{
  const pragma_entry *old = (const pragma_entry *) p;
  return htab_hash_string (old->funcname);
}

static void
mep_note_pragma_flag (const char *funcname, int flag)
{
  pragma_entry **slot;

  if (!pragma_htab)
    pragma_htab = htab_create_ggc (31, pragma_entry_hash,
				    pragma_entry_eq, NULL);

  slot = (pragma_entry **)
    htab_find_slot_with_hash (pragma_htab, funcname,
			      htab_hash_string (funcname), INSERT);

  if (!*slot)
    {
      *slot = ggc_alloc<pragma_entry> ();
      (*slot)->flag = 0;
      (*slot)->used = 0;
      (*slot)->funcname = ggc_strdup (funcname);
    }
  (*slot)->flag |= flag;
}

static bool
mep_lookup_pragma_flag (const char *funcname, int flag)
{
  pragma_entry **slot;

  if (!pragma_htab)
    return false;

  if (funcname[0] == '@' && funcname[2] == '.')
    funcname += 3;

  slot = (pragma_entry **)
    htab_find_slot_with_hash (pragma_htab, funcname,
			      htab_hash_string (funcname), NO_INSERT);
  if (slot && *slot && ((*slot)->flag & flag))
    {
      (*slot)->used |= flag;
      return true;
    }
  return false;
}

bool
mep_lookup_pragma_call (const char *funcname)
{
  return mep_lookup_pragma_flag (funcname, FUNC_CALL);
}

void
mep_note_pragma_call (const char *funcname)
{
  mep_note_pragma_flag (funcname, FUNC_CALL);
}

bool
mep_lookup_pragma_disinterrupt (const char *funcname)
{
  return mep_lookup_pragma_flag (funcname, FUNC_DISINTERRUPT);
}

void
mep_note_pragma_disinterrupt (const char *funcname)
{
  mep_note_pragma_flag (funcname, FUNC_DISINTERRUPT);
}

static int
note_unused_pragma_disinterrupt (void **slot, void *data ATTRIBUTE_UNUSED)
{
  const pragma_entry *d = (const pragma_entry *)(*slot);

  if ((d->flag & FUNC_DISINTERRUPT)
      && !(d->used & FUNC_DISINTERRUPT))
    warning (0, "\"#pragma disinterrupt %s\" not used", d->funcname);
  return 1;
}

void
mep_file_cleanups (void)
{
  if (pragma_htab)
    htab_traverse (pragma_htab, note_unused_pragma_disinterrupt, NULL);
}

/* These three functions provide a bridge between the pramgas that
   affect register classes, and the functions that maintain them.  We
   can't call those functions directly as pragma handling is part of
   the front end and doesn't have direct access to them.  */

void
mep_save_register_info (void)
{
  save_register_info ();
}

void
mep_reinit_regs (void)
{
  reinit_regs ();
}

void
mep_init_regs (void)
{
  init_regs ();
}

     

static int
mep_attrlist_to_encoding (tree list, tree decl)
{
  if (mep_multiple_address_regions (list, false) > 1)
    {
      warning (0, "duplicate address region attribute %qE in declaration of %qE on line %d",
	       TREE_PURPOSE (TREE_CHAIN (list)),
	       DECL_NAME (decl),
	       DECL_SOURCE_LINE (decl));
      TREE_CHAIN (list) = NULL_TREE;
    }
      
  while (list)
    {
      if (is_attribute_p ("based", TREE_PURPOSE (list)))
	return 'b';
      if (is_attribute_p ("tiny", TREE_PURPOSE (list)))
	return 't';
      if (is_attribute_p ("near", TREE_PURPOSE (list)))
	return 'n';
      if (is_attribute_p ("far", TREE_PURPOSE (list)))
	return 'f';
      if (is_attribute_p ("io", TREE_PURPOSE (list)))
	{
	  if (TREE_VALUE (list)
	      && TREE_VALUE (TREE_VALUE (list))
	      && TREE_CODE (TREE_VALUE (TREE_VALUE (list))) == INTEGER_CST)
	    {
	      int location = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE(list)));
	      if (location >= 0
		  && location <= 0x1000000)
		return 'i';
	    }
	  return 'I';
	}
      if (is_attribute_p ("cb", TREE_PURPOSE (list)))
	return 'c';
      list = TREE_CHAIN (list);
    }
  if (TARGET_TF
      && TREE_CODE (decl) == FUNCTION_DECL
      && DECL_SECTION_NAME (decl) == 0)
    return 'f';
  return 0;
}

static int
mep_comp_type_attributes (const_tree t1, const_tree t2)
{
  int vliw1, vliw2;

  vliw1 = (lookup_attribute ("vliw", TYPE_ATTRIBUTES (t1)) != 0);
  vliw2 = (lookup_attribute ("vliw", TYPE_ATTRIBUTES (t2)) != 0);

  if (vliw1 != vliw2)
    return 0;

  return 1;
}

static void
mep_insert_attributes (tree decl, tree *attributes)
{
  int size;
  const char *secname = 0;
  tree attrib, attrlist;
  char encoding;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      const char *funcname = IDENTIFIER_POINTER (DECL_NAME (decl));

      if (mep_lookup_pragma_disinterrupt (funcname))
	{
	  attrib = build_tree_list (get_identifier ("disinterrupt"), NULL_TREE);
	  *attributes = chainon (*attributes, attrib);
	}
    }

  if (TREE_CODE (decl) != VAR_DECL
      || ! (TREE_PUBLIC (decl) || TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    return;

  if (TREE_READONLY (decl) && TARGET_DC)
    /* -mdc means that const variables default to the near section,
       regardless of the size cutoff.  */
    return;

  /* User specified an attribute, so override the default.
     Ignore storage attribute of pointed to variable. char __far * x;  */
  if (! (TREE_TYPE (decl) && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE))
    {
      if (TYPE_P (decl) && TYPE_ATTRIBUTES (decl) && *attributes)
	TYPE_ATTRIBUTES (decl) = NULL_TREE;
      else if (DECL_ATTRIBUTES (decl) && *attributes)
	DECL_ATTRIBUTES (decl) = NULL_TREE;
    }

  attrlist = *attributes ? *attributes : DECL_ATTRIBUTES (decl);
  encoding = mep_attrlist_to_encoding (attrlist, decl);
  if (!encoding && TYPE_P (TREE_TYPE (decl)))
    {
      attrlist = TYPE_ATTRIBUTES (TREE_TYPE (decl));
      encoding = mep_attrlist_to_encoding (attrlist, decl);
    }
  if (encoding)
    {	  
      /* This means that the declaration has a specific section
	 attribute, so we should not apply the default rules.  */

      if (encoding == 'i' || encoding == 'I')
	{
	  tree attr = lookup_attribute ("io", attrlist);
	  if (attr
	      && TREE_VALUE (attr)
	      && TREE_VALUE (TREE_VALUE(attr)))
	    {
	      int location = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE(attr)));
	      static tree previous_value = 0;
	      static int previous_location = 0;
	      static tree previous_name = 0;

	      /* We take advantage of the fact that gcc will reuse the
		 same tree pointer when applying an attribute to a
		 list of decls, but produce a new tree for attributes
		 on separate source lines, even when they're textually
		 identical.  This is the behavior we want.  */
	      if (TREE_VALUE (attr) == previous_value
		  && location == previous_location)
		{
		  warning(0, "__io address 0x%x is the same for %qE and %qE",
			  location, previous_name, DECL_NAME (decl));
		}
	      previous_name = DECL_NAME (decl);
	      previous_location = location;
	      previous_value = TREE_VALUE (attr);
	    }
	}
      return;
    }


  /* Declarations of arrays can change size.  Don't trust them.  */
  if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
    size = 0;
  else
    size = int_size_in_bytes (TREE_TYPE (decl));

  if (TARGET_RAND_TPGP && size <= 4 && size > 0)
    {
      if (TREE_PUBLIC (decl)
	  || DECL_EXTERNAL (decl)
	  || TREE_STATIC (decl))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
	  int key = 0;

	  while (*name)
	    key += *name++;

	  switch (key & 3)
	    {
	    case 0:
	      secname = "based";
	      break;
	    case 1:
	      secname = "tiny";
	      break;
	    case 2:
	      secname = "far";
	      break;
	    default:
	      ;
	    }
	}
    }
  else
    {
      if (size <= mep_based_cutoff && size > 0)
	secname = "based";
      else if (size <= mep_tiny_cutoff && size > 0)
	secname = "tiny";
      else if (TARGET_L)
	secname = "far";
    }

  if (mep_const_section && TREE_READONLY (decl))
    {
      if (strcmp (mep_const_section, "tiny") == 0)
	secname = "tiny";
      else if (strcmp (mep_const_section, "near") == 0)
	return;
      else if (strcmp (mep_const_section, "far") == 0)
	secname = "far";
    }

  if (!secname)
    return;

  if (!mep_multiple_address_regions (*attributes, true)
      && !mep_multiple_address_regions (DECL_ATTRIBUTES (decl), false))
    {
      attrib = build_tree_list (get_identifier (secname), NULL_TREE);

      /* Chain the attribute directly onto the variable's DECL_ATTRIBUTES
	 in order to avoid the POINTER_TYPE bypasses in mep_validate_near_far
	 and mep_validate_based_tiny.  */
      DECL_ATTRIBUTES (decl) = chainon (DECL_ATTRIBUTES (decl), attrib);
    }
}

static void
mep_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx rtlname;
  const char *oldname;
  const char *secname;
  char encoding;
  char *newname;
  tree idp;
  int maxsize;
  tree type;
  tree mep_attributes;

  if (! first)
    return;

  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != FUNCTION_DECL)
    return;

  rtlname = XEXP (rtl, 0);
  if (GET_CODE (rtlname) == SYMBOL_REF)
    oldname = XSTR (rtlname, 0);
  else if (GET_CODE (rtlname) == MEM
	   && GET_CODE (XEXP (rtlname, 0)) == SYMBOL_REF)
    oldname = XSTR (XEXP (rtlname, 0), 0);
  else
    gcc_unreachable ();

  type = TREE_TYPE (decl);
  if (type == error_mark_node)
    return;
  mep_attributes = MEP_ATTRIBUTES (decl);

  encoding = mep_attrlist_to_encoding (mep_attributes, decl);

  if (encoding)
    {
      newname = (char *) alloca (strlen (oldname) + 4);
      sprintf (newname, "@%c.%s", encoding, oldname);
      idp = get_identifier (newname);
      XEXP (rtl, 0) =
	gen_rtx_SYMBOL_REF (Pmode, IDENTIFIER_POINTER (idp));
      SYMBOL_REF_WEAK (XEXP (rtl, 0)) = DECL_WEAK (decl);
      SET_SYMBOL_REF_DECL (XEXP (rtl, 0), decl);

      switch (encoding)
	{
	case 'b':
	  maxsize = 128;
	  secname = "based";
	  break;
	case 't':
	  maxsize = 65536;
	  secname = "tiny";
	  break;
	case 'n':
	  maxsize = 0x1000000;
	  secname = "near";
	  break;
	default:
	  maxsize = 0;
	  secname = 0;
	  break;
	}
      if (maxsize && int_size_in_bytes (TREE_TYPE (decl)) > maxsize)
	{
	  warning (0, "variable %s (%ld bytes) is too large for the %s section (%d bytes)",
		   oldname,
		   (long) int_size_in_bytes (TREE_TYPE (decl)),
		   secname,
		   maxsize);
	}
    }
}

const char *
mep_strip_name_encoding (const char *sym)
{
  while (1)
    {
      if (*sym == '*')
	sym++;
      else if (*sym == '@' && sym[2] == '.')
	sym += 3;
      else
	return sym;
    }
}

static section *
mep_select_section (tree decl, int reloc ATTRIBUTE_UNUSED,
		    unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  int readonly = 1;
  int encoding;

  switch (TREE_CODE (decl))
    {
    case VAR_DECL:
      if (!TREE_READONLY (decl)
	  || TREE_SIDE_EFFECTS (decl)
	  || !DECL_INITIAL (decl)
	  || (DECL_INITIAL (decl) != error_mark_node
	      && !TREE_CONSTANT (DECL_INITIAL (decl))))
	readonly = 0;
      break;
    case CONSTRUCTOR:
      if (! TREE_CONSTANT (decl))
	readonly = 0;
      break;

    default:
      break;
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

      if (name[0] == '@' && name[2] == '.')
	encoding = name[1];
      else
	encoding = 0;

      if (flag_function_sections || DECL_COMDAT_GROUP (decl))
	mep_unique_section (decl, 0);
      else if (lookup_attribute ("vliw", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
	{
	  if (encoding == 'f')
	    return vftext_section;
	  else
	    return vtext_section;
	}
      else if (encoding == 'f')
	return ftext_section;
      else
	return text_section;
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

      if (name[0] == '@' && name[2] == '.')
	switch (name[1])
	  {
	  case 'b':
	    return based_section;

	  case 't':
	    if (readonly)
	      return srodata_section;
	    if (DECL_INITIAL (decl))
	      return sdata_section;
	    return tinybss_section;

	  case 'f':
	    if (readonly)
	      return frodata_section;
	    return far_section;

	  case 'i':
	  case 'I':
	    error_at (DECL_SOURCE_LOCATION (decl),
		      "variable %D of type %<io%> must be uninitialized", decl);
	    return data_section;

	  case 'c':
	    error_at (DECL_SOURCE_LOCATION (decl),
		      "variable %D of type %<cb%> must be uninitialized", decl);
	    return data_section;
	  }
    }

  if (readonly)
    return readonly_data_section;

  return data_section;
}

static void
mep_unique_section (tree decl, int reloc)
{
  static const char *prefixes[][2] =
  {
    { ".text.",   ".gnu.linkonce.t." },
    { ".rodata.", ".gnu.linkonce.r." },
    { ".data.",   ".gnu.linkonce.d." },
    { ".based.",   ".gnu.linkonce.based." },
    { ".sdata.",   ".gnu.linkonce.s." },
    { ".far.",     ".gnu.linkonce.far." },
    { ".ftext.",   ".gnu.linkonce.ft." },
    { ".frodata.", ".gnu.linkonce.frd." },
    { ".srodata.", ".gnu.linkonce.srd." },
    { ".vtext.",   ".gnu.linkonce.v." },
    { ".vftext.",   ".gnu.linkonce.vf." }
  };
  int sec = 2; /* .data */
  int len;
  const char *name, *prefix;
  char *string;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  if (DECL_RTL (decl))
    name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (lookup_attribute ("vliw", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
	sec = 9; /* .vtext */
      else
	sec = 0; /* .text */
    }
  else if (decl_readonly_section (decl, reloc))
    sec = 1; /* .rodata */

  if (name[0] == '@' && name[2] == '.')
    {
      switch (name[1])
	{
	case 'b':
	  sec = 3; /* .based */
	  break;
	case 't':
	  if (sec == 1)
	    sec = 8; /* .srodata */
	  else
	    sec = 4; /* .sdata */
	  break;
	case 'f':
	  if (sec == 0)
	    sec = 6; /* .ftext */
	  else if (sec == 9)
	    sec = 10; /* .vftext */
	  else if (sec == 1)
	    sec = 7; /* .frodata */
	  else
	    sec = 5; /* .far. */
	  break;
	}
      name += 3;
    }

  prefix = prefixes[sec][DECL_COMDAT_GROUP(decl) != NULL];
  len    = strlen (name) + strlen (prefix);
  string = (char *) alloca (len + 1);

  sprintf (string, "%s%s", prefix, name);

  set_decl_section_name (decl, string);
}

/* Given a decl, a section name, and whether the decl initializer
   has relocs, choose attributes for the section.  */

#define SECTION_MEP_VLIW	SECTION_MACH_DEP

static unsigned int
mep_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (decl && TREE_CODE (decl) == FUNCTION_DECL
      && lookup_attribute ("vliw", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
    flags |= SECTION_MEP_VLIW;

  return flags;
}

/* Switch to an arbitrary section NAME with attributes as specified
   by FLAGS.  ALIGN specifies any known alignment requirements for
   the section; 0 if the default should be used.

   Differs from the standard ELF version only in support of VLIW mode.  */

static void
mep_asm_named_section (const char *name, unsigned int flags, tree decl ATTRIBUTE_UNUSED)
{
  char flagchars[8], *f = flagchars;
  const char *type;

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  if (flags & SECTION_SMALL)
    *f++ = 's';
  if (flags & SECTION_MEP_VLIW)
    *f++ = 'v';
  *f = '\0';

  if (flags & SECTION_BSS)
    type = "nobits";
  else
    type = "progbits";

  fprintf (asm_out_file, "\t.section\t%s,\"%s\",@%s\n",
	   name, flagchars, type);

  if (flags & SECTION_CODE)
    fputs ((flags & SECTION_MEP_VLIW ? "\t.vliw\n" : "\t.core\n"),
	   asm_out_file);
}

void
mep_output_aligned_common (FILE *stream, tree decl, const char *name,
			   int size, int align, int global)
{
  /* We intentionally don't use mep_section_tag() here.  */
  if (name[0] == '@'
      && (name[1] == 'i' || name[1] == 'I' || name[1] == 'c')
      && name[2] == '.')
    {
      int location = -1;
      tree attr = lookup_attribute ((name[1] == 'c' ? "cb" : "io"),
				    DECL_ATTRIBUTES (decl));
      if (attr
	  && TREE_VALUE (attr)
	  && TREE_VALUE (TREE_VALUE(attr)))
	location = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE(attr)));
      if (location == -1)
	return;
      if (global)
	{
	  fprintf (stream, "\t.globl\t");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}
      assemble_name (stream, name);
      fprintf (stream, " = %d\n", location);
      return;
    }
  if (name[0] == '@' && name[2] == '.')
    {
      const char *sec = 0;
      switch (name[1])
	{
	case 'b':
	  switch_to_section (based_section);
	  sec = ".based";
	  break;
	case 't':
	  switch_to_section (tinybss_section);
	  sec = ".sbss";
	  break;
	case 'f':
	  switch_to_section (farbss_section);
	  sec = ".farbss";
	  break;
	}
      if (sec)
	{
	  const char *name2;
	  int p2align = 0;

	  while (align > BITS_PER_UNIT)
	    {
	      align /= 2;
	      p2align ++;
	    }
	  name2 = targetm.strip_name_encoding (name);
	  if (global)
	    fprintf (stream, "\t.globl\t%s\n", name2);
	  fprintf (stream, "\t.p2align %d\n", p2align);
	  fprintf (stream, "\t.type\t%s,@object\n", name2);
	  fprintf (stream, "\t.size\t%s,%d\n", name2, size);
	  fprintf (stream, "%s:\n\t.zero\t%d\n", name2, size);
	  return;
	}
    }

  if (!global)
    {
      fprintf (stream, "\t.local\t");
      assemble_name (stream, name);
      fprintf (stream, "\n");
    }
  fprintf (stream, "\t.comm\t");
  assemble_name (stream, name);
  fprintf (stream, ",%u,%u\n", size, align / BITS_PER_UNIT);
}

/* Trampolines.  */

static void
mep_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx addr = XEXP (m_tramp, 0);
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);

  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__mep_trampoline_helper"),
		     LCT_NORMAL, VOIDmode, 3,
		     addr, Pmode,
		     fnaddr, Pmode,
		     static_chain, Pmode);
}

/* Experimental Reorg.  */

static bool
mep_mentioned_p (rtx in,
		 rtx reg, /* NULL for mem */
		 int modes_too) /* if nonzero, modes must match also.  */
{
  const char *fmt;
  int i;
  enum rtx_code code;

  if (in == 0)
    return false;
  if (reg && GET_CODE (reg) != REG)
    return false;

  if (GET_CODE (in) == LABEL_REF)
    return (reg == 0);

  code = GET_CODE (in);

  switch (code)
    {
    case MEM:
      if (reg)
	return mep_mentioned_p (XEXP (in, 0), reg, modes_too);
      return true;

    case REG:
      if (!reg)
	return false;
      if (modes_too && (GET_MODE (in) != GET_MODE (reg)))
	return false;
      return (REGNO (in) == REGNO (reg));

    case SCRATCH:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST_DOUBLE:
      return false;

    default:
      break;
    }

  /* Set's source should be read-only.  */
  if (code == SET && !reg)
    return mep_mentioned_p (SET_DEST (in), reg, modes_too);

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    if (mep_mentioned_p (XVECEXP (in, i, j), reg, modes_too))
	      return true;
	}
      else if (fmt[i] == 'e'
	       && mep_mentioned_p (XEXP (in, i), reg, modes_too))
	return true;
    }
  return false;
}

#define EXPERIMENTAL_REGMOVE_REORG 1

#if EXPERIMENTAL_REGMOVE_REORG

static int
mep_compatible_reg_class (int r1, int r2)
{
  if (GR_REGNO_P (r1) && GR_REGNO_P (r2))
    return 1;
  if (CR_REGNO_P (r1) && CR_REGNO_P (r2))
    return 1;
  return 0;
}

static void
mep_reorg_regmove (rtx insns)
{
  rtx insn, next, pat, follow, *where;
  int count = 0, done = 0, replace, before = 0;

  if (dump_file)
    for (insn = insns; insn; insn = NEXT_INSN (insn))
      if (NONJUMP_INSN_P (insn))
	before++;

  /* We're looking for (set r2 r1) moves where r1 dies, followed by a
     set that uses the r2 and r2 dies there.  We replace r2 with r1
     and see if it's still a valid insn.  If so, delete the first set.
     Copied from reorg.c.  */

  while (!done)
    {
      done = 1;
      for (insn = insns; insn; insn = next)
	{
	  next = next_nonnote_nondebug_insn (insn);
	  if (! NONJUMP_INSN_P (insn))
	    continue;
	  pat = PATTERN (insn);

	  replace = 0;

	  if (GET_CODE (pat) == SET
	      && GET_CODE (SET_SRC (pat)) == REG
	      && GET_CODE (SET_DEST (pat)) == REG
	      && find_regno_note (insn, REG_DEAD, REGNO (SET_SRC (pat)))
	      && mep_compatible_reg_class (REGNO (SET_SRC (pat)), REGNO (SET_DEST (pat))))
	    {
	      follow = next_nonnote_nondebug_insn (insn);
	      if (dump_file)
		fprintf (dump_file, "superfluous moves: considering %d\n", INSN_UID (insn));

	      while (follow && NONJUMP_INSN_P (follow)
		     && GET_CODE (PATTERN (follow)) == SET
		     && !dead_or_set_p (follow, SET_SRC (pat))
		     && !mep_mentioned_p (PATTERN (follow), SET_SRC (pat), 0)
		     && !mep_mentioned_p (PATTERN (follow), SET_DEST (pat), 0))
		{
		  if (dump_file)
		    fprintf (dump_file, "\tskipping %d\n", INSN_UID (follow));
		  follow = next_nonnote_insn (follow);
		}

	      if (dump_file)
		fprintf (dump_file, "\tfollow is %d\n", INSN_UID (follow));
	      if (follow && NONJUMP_INSN_P (follow)
		  && GET_CODE (PATTERN (follow)) == SET
		  && find_regno_note (follow, REG_DEAD, REGNO (SET_DEST (pat))))
		{
		  if (GET_CODE (SET_DEST (PATTERN (follow))) == REG)
		    {
		      if (mep_mentioned_p (SET_SRC (PATTERN (follow)), SET_DEST (pat), 1))
			{
			  replace = 1;
			  where = & SET_SRC (PATTERN (follow));
			}
		    }
		  else if (GET_CODE (SET_DEST (PATTERN (follow))) == MEM)
		    {
		      if (mep_mentioned_p (PATTERN (follow), SET_DEST (pat), 1))
			{
			  replace = 1;
			  where = & PATTERN (follow);
			}
		    }
		}
	    }

	  /* If so, follow is the corresponding insn */
	  if (replace)
	    {
	      if (dump_file)
		{
		  rtx x;

		  fprintf (dump_file, "----- Candidate for superfluous move deletion:\n\n");
		  for (x = insn; x ;x = NEXT_INSN (x))
		    {
		      print_rtl_single (dump_file, x);
		      if (x == follow)
			break;
		      fprintf (dump_file, "\n");
		    }
		}

	      if (validate_replace_rtx_subexp (SET_DEST (pat), SET_SRC (pat),
					       follow, where))
		{
		  count ++;
		  delete_insn (insn);
		  if (dump_file)
		    {
		      fprintf (dump_file, "\n----- Success!  new insn:\n\n");
		      print_rtl_single (dump_file, follow);
		    }
		  done = 0;
		}
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n%d insn%s deleted out of %d.\n\n", count, count == 1 ? "" : "s", before);
      fprintf (dump_file, "=====\n");
    }
}
#endif


/* Figure out where to put LABEL, which is the label for a repeat loop.
   If INCLUDING, LAST_INSN is the last instruction in the loop, otherwise
   the loop ends just before LAST_INSN.  If SHARED, insns other than the
   "repeat" might use LABEL to jump to the loop's continuation point.

   Return the last instruction in the adjusted loop.  */

static rtx
mep_insert_repeat_label_last (rtx last_insn, rtx label, bool including,
			      bool shared)
{
  rtx next, prev;
  int count = 0, code, icode;

  if (dump_file)
    fprintf (dump_file, "considering end of repeat loop at insn %d\n",
	     INSN_UID (last_insn));

  /* Set PREV to the last insn in the loop.  */
  prev = last_insn;
  if (!including)
    prev = PREV_INSN (prev);

  /* Set NEXT to the next insn after the repeat label.  */
  next = last_insn;
  if (!shared)
    while (prev != 0)
      {
	code = GET_CODE (prev);
	if (code == CALL_INSN || code == CODE_LABEL || code == BARRIER)
	  break;

	if (INSN_P (prev))
	  {
	    if (GET_CODE (PATTERN (prev)) == SEQUENCE)
	      prev = XVECEXP (PATTERN (prev), 0, 1);

	    /* Other insns that should not be in the last two opcodes.  */
	    icode = recog_memoized (prev);
	    if (icode < 0
		|| icode == CODE_FOR_repeat
		|| icode == CODE_FOR_erepeat
		|| get_attr_may_trap (prev) == MAY_TRAP_YES)
	      break;

	    /* That leaves JUMP_INSN and INSN.  It will have BImode if it
	       is the second instruction in a VLIW bundle.  In that case,
	       loop again: if the first instruction also satisfies the
	       conditions above then we will reach here again and put
	       both of them into the repeat epilogue.  Otherwise both
	       should remain outside.  */
	    if (GET_MODE (prev) != BImode)
	      {
		count++;
		next = prev;
		if (dump_file)
		  print_rtl_single (dump_file, next);
		if (count == 2)
		  break;
	      }
	  }
	prev = PREV_INSN (prev);
      }

  /* See if we're adding the label immediately after the repeat insn.
     If so, we need to separate them with a nop.  */
  prev = prev_real_insn (next);
  if (prev)
    switch (recog_memoized (prev))
      {
      case CODE_FOR_repeat:
      case CODE_FOR_erepeat:
	if (dump_file)
	  fprintf (dump_file, "Adding nop inside loop\n");
	emit_insn_before (gen_nop (), next);
	break;

      default:
	break;
      }

  /* Insert the label.  */
  emit_label_before (label, next);

  /* Insert the nops.  */
  if (dump_file && count < 2)
    fprintf (dump_file, "Adding %d nop%s\n\n",
	     2 - count, count == 1 ? "" : "s");

  for (; count < 2; count++)
    if (including)
      last_insn = emit_insn_after (gen_nop (), last_insn);
    else
      emit_insn_before (gen_nop (), last_insn);

  return last_insn;
}


void
mep_emit_doloop (rtx *operands, int is_end)
{
  rtx tag;

  if (cfun->machine->doloop_tags == 0
      || cfun->machine->doloop_tag_from_end == is_end)
    {
      cfun->machine->doloop_tags++;
      cfun->machine->doloop_tag_from_end = is_end;
    }

  tag = GEN_INT (cfun->machine->doloop_tags - 1);
  if (is_end)
    emit_jump_insn (gen_doloop_end_internal (operands[0], operands[1], tag));
  else
    emit_insn (gen_doloop_begin_internal (operands[0], operands[0], tag));
}


/* Code for converting doloop_begins and doloop_ends into valid
   MeP instructions.  A doloop_begin is just a placeholder:

	$count = unspec ($count)

   where $count is initially the number of iterations - 1.
   doloop_end has the form:

	if ($count-- == 0) goto label

   The counter variable is private to the doloop insns, nothing else
   relies on its value.

   There are three cases, in decreasing order of preference:

      1. A loop has exactly one doloop_begin and one doloop_end.
	 The doloop_end branches to the first instruction after
	 the doloop_begin.

	 In this case we can replace the doloop_begin with a repeat
	 instruction and remove the doloop_end.  I.e.:

		$count1 = unspec ($count1)
	    label:
		...
		insn1
		insn2
		if ($count2-- == 0) goto label

	  becomes:

		repeat $count1,repeat_label
	    label:
		...
	    repeat_label:
		insn1
		insn2
		# end repeat

      2. As for (1), except there are several doloop_ends.  One of them
	 (call it X) falls through to a label L.  All the others fall
	 through to branches to L.

	 In this case, we remove X and replace the other doloop_ends
	 with branches to the repeat label.  For example:

		$count1 = unspec ($count1)
	    start:
		...
		if ($count2-- == 0) goto label
	    end:
		...
		if ($count3-- == 0) goto label
		goto end

	 becomes:

		repeat $count1,repeat_label
	    start:
		...
	    repeat_label:
		nop
		nop
		# end repeat
	    end:
		...
		goto repeat_label

      3. The fallback case.  Replace doloop_begins with:

		$count = $count + 1

	 Replace doloop_ends with the equivalent of:

		$count = $count - 1
		if ($count == 0) goto label

	 Note that this might need a scratch register if $count
	 is stored in memory.  */

/* A structure describing one doloop_begin.  */
struct mep_doloop_begin {
  /* The next doloop_begin with the same tag.  */
  struct mep_doloop_begin *next;

  /* The instruction itself.  */
  rtx insn;

  /* The initial counter value.  This is known to be a general register.  */
  rtx counter;
};

/* A structure describing a doloop_end.  */
struct mep_doloop_end {
  /* The next doloop_end with the same loop tag.  */
  struct mep_doloop_end *next;

  /* The instruction itself.  */
  rtx insn;

  /* The first instruction after INSN when the branch isn't taken.  */
  rtx fallthrough;

  /* The location of the counter value.  Since doloop_end_internal is a
     jump instruction, it has to allow the counter to be stored anywhere
     (any non-fixed register or memory location).  */
  rtx counter;

  /* The target label (the place where the insn branches when the counter
     isn't zero).  */
  rtx label;

  /* A scratch register.  Only available when COUNTER isn't stored
     in a general register.  */
  rtx scratch;
};


/* One do-while loop.  */
struct mep_doloop {
  /* All the doloop_begins for this loop (in no particular order).  */
  struct mep_doloop_begin *begin;

  /* All the doloop_ends.  When there is more than one, arrange things
     so that the first one is the most likely to be X in case (2) above.  */
  struct mep_doloop_end *end;
};


/* Return true if LOOP can be converted into repeat/repeat_end form
   (that is, if it matches cases (1) or (2) above).  */

static bool
mep_repeat_loop_p (struct mep_doloop *loop)
{
  struct mep_doloop_end *end;
  rtx fallthrough;

  /* There must be exactly one doloop_begin and at least one doloop_end.  */
  if (loop->begin == 0 || loop->end == 0 || loop->begin->next != 0)
    return false;

  /* The first doloop_end (X) must branch back to the insn after
     the doloop_begin.  */
  if (prev_real_insn (loop->end->label) != loop->begin->insn)
    return false;

  /* All the other doloop_ends must branch to the same place as X.
     When the branch isn't taken, they must jump to the instruction
     after X.  */
  fallthrough = loop->end->fallthrough;
  for (end = loop->end->next; end != 0; end = end->next)
    if (end->label != loop->end->label
	|| !simplejump_p (end->fallthrough)
	|| next_real_insn (JUMP_LABEL (end->fallthrough)) != fallthrough)
      return false;

  return true;
}


/* The main repeat reorg function.  See comment above for details.  */

static void
mep_reorg_repeat (rtx insns)
{
  rtx insn;
  struct mep_doloop *loops, *loop;
  struct mep_doloop_begin *begin;
  struct mep_doloop_end *end;

  /* Quick exit if we haven't created any loops.  */
  if (cfun->machine->doloop_tags == 0)
    return;

  /* Create an array of mep_doloop structures.  */
  loops = (struct mep_doloop *) alloca (sizeof (loops[0]) * cfun->machine->doloop_tags);
  memset (loops, 0, sizeof (loops[0]) * cfun->machine->doloop_tags);

  /* Search the function for do-while insns and group them by loop tag.  */
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      switch (recog_memoized (insn))
	{
	case CODE_FOR_doloop_begin_internal:
	  insn_extract (insn);
	  loop = &loops[INTVAL (recog_data.operand[2])];

	  begin = (struct mep_doloop_begin *) alloca (sizeof (struct mep_doloop_begin));
	  begin->next = loop->begin;
	  begin->insn = insn;
	  begin->counter = recog_data.operand[0];

	  loop->begin = begin;
	  break;

	case CODE_FOR_doloop_end_internal:
	  insn_extract (insn);
	  loop = &loops[INTVAL (recog_data.operand[2])];

	  end = (struct mep_doloop_end *) alloca (sizeof (struct mep_doloop_end));
	  end->insn = insn;
	  end->fallthrough = next_real_insn (insn);
	  end->counter = recog_data.operand[0];
	  end->label = recog_data.operand[1];
	  end->scratch = recog_data.operand[3];

	  /* If this insn falls through to an unconditional jump,
	     give it a lower priority than the others.  */
	  if (loop->end != 0 && simplejump_p (end->fallthrough))
	    {
	      end->next = loop->end->next;
	      loop->end->next = end;
	    }
	  else
	    {
	      end->next = loop->end;
	      loop->end = end;
	    }
	  break;
	}

  /* Convert the insns for each loop in turn.  */
  for (loop = loops; loop < loops + cfun->machine->doloop_tags; loop++)
    if (mep_repeat_loop_p (loop))
      {
	/* Case (1) or (2).  */
	rtx repeat_label, label_ref;

	/* Create a new label for the repeat insn.  */
	repeat_label = gen_label_rtx ();

	/* Replace the doloop_begin with a repeat.  */
	label_ref = gen_rtx_LABEL_REF (VOIDmode, repeat_label);
	emit_insn_before (gen_repeat (loop->begin->counter, label_ref),
			  loop->begin->insn);
	delete_insn (loop->begin->insn);

	/* Insert the repeat label before the first doloop_end.
	   Fill the gap with nops if there are other doloop_ends.  */
	mep_insert_repeat_label_last (loop->end->insn, repeat_label,
				      false, loop->end->next != 0);

	/* Emit a repeat_end (to improve the readability of the output).  */
	emit_insn_before (gen_repeat_end (), loop->end->insn);

	/* Delete the first doloop_end.  */
	delete_insn (loop->end->insn);

	/* Replace the others with branches to REPEAT_LABEL.  */
	for (end = loop->end->next; end != 0; end = end->next)
	  {
	    emit_jump_insn_before (gen_jump (repeat_label), end->insn);
	    delete_insn (end->insn);
	    delete_insn (end->fallthrough);
	  }
      }
    else
      {
	/* Case (3).  First replace all the doloop_begins with increment
	   instructions.  */
	for (begin = loop->begin; begin != 0; begin = begin->next)
	  {
	    emit_insn_before (gen_add3_insn (copy_rtx (begin->counter),
					     begin->counter, const1_rtx),
			      begin->insn);
	    delete_insn (begin->insn);
	  }

	/* Replace all the doloop_ends with decrement-and-branch sequences.  */
	for (end = loop->end; end != 0; end = end->next)
	  {
	    rtx reg;

	    start_sequence ();

	    /* Load the counter value into a general register.  */
	    reg = end->counter;
	    if (!REG_P (reg) || REGNO (reg) > 15)
	      {
		reg = end->scratch;
		emit_move_insn (copy_rtx (reg), copy_rtx (end->counter));
	      }

	    /* Decrement the counter.  */
	    emit_insn (gen_add3_insn (copy_rtx (reg), copy_rtx (reg),
				      constm1_rtx));

	    /* Copy it back to its original location.  */
	    if (reg != end->counter)
	      emit_move_insn (copy_rtx (end->counter), copy_rtx (reg));

	    /* Jump back to the start label.  */
	    insn = emit_jump_insn (gen_mep_bne_true (reg, const0_rtx,
						     end->label));
	    JUMP_LABEL (insn) = end->label;
	    LABEL_NUSES (end->label)++;

	    /* Emit the whole sequence before the doloop_end.  */
	    insn = get_insns ();
	    end_sequence ();
	    emit_insn_before (insn, end->insn);

	    /* Delete the doloop_end.  */
	    delete_insn (end->insn);
	  }
      }
}


static bool
mep_invertable_branch_p (rtx insn)
{
  rtx cond, set;
  enum rtx_code old_code;
  int i;

  set = PATTERN (insn);
  if (GET_CODE (set) != SET)
    return false;
  if (GET_CODE (XEXP (set, 1)) != IF_THEN_ELSE)
    return false;
  cond = XEXP (XEXP (set, 1), 0);
  old_code = GET_CODE (cond);
  switch (old_code)
    {
    case EQ:
      PUT_CODE (cond, NE);
      break;
    case NE:
      PUT_CODE (cond, EQ);
      break;
    case LT:
      PUT_CODE (cond, GE);
      break;
    case GE:
      PUT_CODE (cond, LT);
      break;
    default:
      return false;
    }
  INSN_CODE (insn) = -1;
  i = recog_memoized (insn);
  PUT_CODE (cond, old_code);
  INSN_CODE (insn) = -1;
  return i >= 0;
}

static void
mep_invert_branch (rtx insn, rtx after)
{
  rtx cond, set, label;
  int i;

  set = PATTERN (insn);

  gcc_assert (GET_CODE (set) == SET);
  gcc_assert (GET_CODE (XEXP (set, 1)) == IF_THEN_ELSE);

  cond = XEXP (XEXP (set, 1), 0);
  switch (GET_CODE (cond))
    {
    case EQ:
      PUT_CODE (cond, NE);
      break;
    case NE:
      PUT_CODE (cond, EQ);
      break;
    case LT:
      PUT_CODE (cond, GE);
      break;
    case GE:
      PUT_CODE (cond, LT);
      break;
    default:
      gcc_unreachable ();
    }
  label = gen_label_rtx ();
  emit_label_after (label, after);
  for (i=1; i<=2; i++)
    if (GET_CODE (XEXP (XEXP (set, 1), i)) == LABEL_REF)
      {
	rtx ref = XEXP (XEXP (set, 1), i);
	if (LABEL_NUSES (XEXP (ref, 0)) == 1)
	  delete_insn (XEXP (ref, 0));
	XEXP (ref, 0) = label;
	LABEL_NUSES (label) ++;
	JUMP_LABEL (insn) = label;
      }
  INSN_CODE (insn) = -1;
  i = recog_memoized (insn);
  gcc_assert (i >= 0);
}

static void
mep_reorg_erepeat (rtx insns)
{
  rtx insn, prev, l, x;
  int count;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (JUMP_P (insn)
	&& mep_invertable_branch_p (insn))
      {
	if (dump_file)
	  {
	    fprintf (dump_file, "\n------------------------------\n");
	    fprintf (dump_file, "erepeat: considering this jump:\n");
	    print_rtl_single (dump_file, insn);
	  }
	count = simplejump_p (insn) ? 0 : 1;
	for (prev = PREV_INSN (insn); prev; prev = PREV_INSN (prev))
	  {
	    if (CALL_P (prev) || BARRIER_P (prev))
	      break;

	    if (prev == JUMP_LABEL (insn))
	      {
		rtx newlast;
		if (dump_file)
		  fprintf (dump_file, "found loop top, %d insns\n", count);

		if (LABEL_NUSES (prev) == 1)
		  /* We're the only user, always safe */ ;
		else if (LABEL_NUSES (prev) == 2)
		  {
		    /* See if there's a barrier before this label.  If
		       so, we know nobody inside the loop uses it.
		       But we must be careful to put the erepeat
		       *after* the label.  */
		    rtx barrier;
		    for (barrier = PREV_INSN (prev);
			 barrier && NOTE_P (barrier);
			 barrier = PREV_INSN (barrier))
		      ;
		    if (barrier && ! BARRIER_P (barrier))
		      break;
		  }
		else
		  {
		    /* We don't know who else, within or without our loop, uses this */
		    if (dump_file)
		      fprintf (dump_file, "... but there are multiple users, too risky.\n");
		    break;
		  }

		/* Generate a label to be used by the erepat insn.  */
		l = gen_label_rtx ();

		/* Insert the erepeat after INSN's target label.  */
		x = gen_erepeat (gen_rtx_LABEL_REF (VOIDmode, l));
		LABEL_NUSES (l)++;
		emit_insn_after (x, prev);

		/* Insert the erepeat label.  */
		newlast = (mep_insert_repeat_label_last
			   (insn, l, !simplejump_p (insn), false));
		if (simplejump_p (insn))
		  {
		    emit_insn_before (gen_erepeat_end (), insn);
		    delete_insn (insn);
		  }
		else
		  {
		    mep_invert_branch (insn, newlast);
		    emit_insn_after (gen_erepeat_end (), newlast);
		  }
		break;
	      }

	    if (LABEL_P (prev))
	      {
		/* A label is OK if there is exactly one user, and we
		   can find that user before the next label.  */
		rtx user = 0;
		int safe = 0;
		if (LABEL_NUSES (prev) == 1)
		  {
		    for (user = PREV_INSN (prev);
			 user && (INSN_P (user) || NOTE_P (user));
			 user = PREV_INSN (user))
		      if (JUMP_P (user) && JUMP_LABEL (user) == prev)
			{
			  safe = INSN_UID (user);
			  break;
			}
		  }
		if (!safe)
		  break;
		if (dump_file)
		  fprintf (dump_file, "... ignoring jump from insn %d to %d\n",
			   safe, INSN_UID (prev));
	      }

	    if (INSN_P (prev))
	      {
		count ++;
	      }
	  }
      }
  if (dump_file)
    fprintf (dump_file, "\n==============================\n");
}
  
/* Replace a jump to a return, with a copy of the return.  GCC doesn't
   always do this on its own.  */

static void
mep_jmp_return_reorg (rtx insns)
{
  rtx insn, label, ret;
  int ret_code;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    if (simplejump_p (insn))
    {
      /* Find the fist real insn the jump jumps to.  */
      label = ret = JUMP_LABEL (insn);
      while (ret
	     && (NOTE_P (ret)
		 || LABEL_P (ret)
		 || GET_CODE (PATTERN (ret)) == USE))
	ret = NEXT_INSN (ret);

      if (ret)
	{
	  /* Is it a return?  */
	  ret_code = recog_memoized (ret);
	  if (ret_code == CODE_FOR_return_internal
	      || ret_code == CODE_FOR_eh_return_internal)
	    {
	      /* It is.  Replace the jump with a return.  */
	      LABEL_NUSES (label) --;
	      if (LABEL_NUSES (label) == 0)
		delete_insn (label);
	      PATTERN (insn) = copy_rtx (PATTERN (ret));
	      INSN_CODE (insn) = -1;
	    }
	}
    }
}


static void
mep_reorg_addcombine (rtx insns)
{
  rtx i, n;

  for (i = insns; i; i = NEXT_INSN (i))
    if (INSN_P (i)
	&& INSN_CODE (i) == CODE_FOR_addsi3
	&& GET_CODE (SET_DEST (PATTERN (i))) == REG
	&& GET_CODE (XEXP (SET_SRC (PATTERN (i)), 0)) == REG
	&& REGNO (SET_DEST (PATTERN (i))) == REGNO (XEXP (SET_SRC (PATTERN (i)), 0))
	&& GET_CODE (XEXP (SET_SRC (PATTERN (i)), 1)) == CONST_INT)
      {
	n = NEXT_INSN (i);
	if (INSN_P (n)
	    && INSN_CODE (n) == CODE_FOR_addsi3
	    && GET_CODE (SET_DEST (PATTERN (n))) == REG
	    && GET_CODE (XEXP (SET_SRC (PATTERN (n)), 0)) == REG
	    && REGNO (SET_DEST (PATTERN (n))) == REGNO (XEXP (SET_SRC (PATTERN (n)), 0))
	    && GET_CODE (XEXP (SET_SRC (PATTERN (n)), 1)) == CONST_INT)
	  {
	    int ic = INTVAL (XEXP (SET_SRC (PATTERN (i)), 1));
	    int nc = INTVAL (XEXP (SET_SRC (PATTERN (n)), 1));
	    if (REGNO (SET_DEST (PATTERN (i))) == REGNO (SET_DEST (PATTERN (n)))
		&& ic + nc < 32767
		&& ic + nc > -32768)
	      {
		XEXP (SET_SRC (PATTERN (i)), 1) = GEN_INT (ic + nc);
		NEXT_INSN (i) = NEXT_INSN (n);
		if (NEXT_INSN (i))
		  PREV_INSN (NEXT_INSN (i)) = i;
	      }
	  }
      }
}

/* If this insn adjusts the stack, return the adjustment, else return
   zero.  */
static int
add_sp_insn_p (rtx insn)
{
  rtx pat;

  if (! single_set (insn))
    return 0;
  pat = PATTERN (insn);
  if (GET_CODE (SET_DEST (pat)) != REG)
    return 0;
  if (REGNO (SET_DEST (pat)) != SP_REGNO)
    return 0;
  if (GET_CODE (SET_SRC (pat)) != PLUS)
    return 0;
  if (GET_CODE (XEXP (SET_SRC (pat), 0)) != REG)
    return 0;
  if (REGNO (XEXP (SET_SRC (pat), 0)) != SP_REGNO)
    return 0;
  if (GET_CODE (XEXP (SET_SRC (pat), 1)) != CONST_INT)
    return 0;
  return INTVAL (XEXP (SET_SRC (pat), 1));
}

/* Check for trivial functions that set up an unneeded stack
   frame.  */
static void
mep_reorg_noframe (rtx insns)
{
  rtx start_frame_insn;
  rtx end_frame_insn = 0;
  int sp_adjust, sp2;
  rtx sp;

  /* The first insn should be $sp = $sp + N */
  while (insns && ! INSN_P (insns))
    insns = NEXT_INSN (insns);
  if (!insns)
    return;

  sp_adjust = add_sp_insn_p (insns);
  if (sp_adjust == 0)
    return;

  start_frame_insn = insns;
  sp = SET_DEST (PATTERN (start_frame_insn));

  insns = next_real_insn (insns);

  while (insns)
    {
      rtx next = next_real_insn (insns);
      if (!next)
	break;

      sp2 = add_sp_insn_p (insns);
      if (sp2)
	{
	  if (end_frame_insn)
	    return;
	  end_frame_insn = insns;
	  if (sp2 != -sp_adjust)
	    return;
	}
      else if (mep_mentioned_p (insns, sp, 0))
	return;
      else if (CALL_P (insns))
	return;

      insns = next;
    }

  if (end_frame_insn)
    {
      delete_insn (start_frame_insn);
      delete_insn (end_frame_insn);
    }
}

static void
mep_reorg (void)
{
  rtx insns = get_insns ();

  /* We require accurate REG_DEAD notes.  */
  compute_bb_for_insn ();
  df_note_add_problem ();
  df_analyze ();

  mep_reorg_addcombine (insns);
#if EXPERIMENTAL_REGMOVE_REORG
  /* VLIW packing has been done already, so we can't just delete things.  */
  if (!mep_vliw_function_p (cfun->decl))
    mep_reorg_regmove (insns);
#endif
  mep_jmp_return_reorg (insns);
  mep_bundle_insns (insns);
  mep_reorg_repeat (insns);
  if (optimize
      && !profile_flag
      && !profile_arc_flag
      && TARGET_OPT_REPEAT
      && (!mep_interrupt_p () || mep_interrupt_saved_reg (RPB_REGNO)))
    mep_reorg_erepeat (insns);

  /* This may delete *insns so make sure it's last.  */
  mep_reorg_noframe (insns);

  df_finish_pass (false);
}



/*----------------------------------------------------------------------*/
/* Builtins								*/
/*----------------------------------------------------------------------*/

/* Element X gives the index into cgen_insns[] of the most general
   implementation of intrinsic X.  Unimplemented intrinsics are
   mapped to -1.  */
int mep_intrinsic_insn[ARRAY_SIZE (cgen_intrinsics)];

/* Element X gives the index of another instruction that is mapped to
   the same intrinsic as cgen_insns[X].  It is -1 when there is no other
   instruction.

   Things are set up so that mep_intrinsic_chain[X] < X.  */
static int mep_intrinsic_chain[ARRAY_SIZE (cgen_insns)];

/* The bitmask for the current ISA.  The ISA masks are declared
   in mep-intrin.h.  */
unsigned int mep_selected_isa;

struct mep_config {
  const char *config_name;
  unsigned int isa;
};

static struct mep_config mep_configs[] = {
#ifdef COPROC_SELECTION_TABLE
  COPROC_SELECTION_TABLE,
#endif
  { 0, 0 }
};

/* Initialize the global intrinsics variables above.  */

static void
mep_init_intrinsics (void)
{
  size_t i;

  /* Set MEP_SELECTED_ISA to the ISA flag for this configuration.  */
  mep_selected_isa = mep_configs[0].isa;
  if (mep_config_string != 0)
    for (i = 0; mep_configs[i].config_name; i++)
      if (strcmp (mep_config_string, mep_configs[i].config_name) == 0)
	{
	  mep_selected_isa = mep_configs[i].isa;
	  break;
	}

  /* Assume all intrinsics are unavailable.  */
  for (i = 0; i < ARRAY_SIZE (mep_intrinsic_insn); i++)
    mep_intrinsic_insn[i] = -1;

  /* Build up the global intrinsic tables.  */
  for (i = 0; i < ARRAY_SIZE (cgen_insns); i++)
    if ((cgen_insns[i].isas & mep_selected_isa) != 0)
      {
	mep_intrinsic_chain[i] = mep_intrinsic_insn[cgen_insns[i].intrinsic];
	mep_intrinsic_insn[cgen_insns[i].intrinsic] = i;
      }
  /* See whether we can directly move values between one coprocessor
     register and another.  */
  for (i = 0; i < ARRAY_SIZE (mep_cmov_insns); i++)
    if (MEP_INTRINSIC_AVAILABLE_P (mep_cmov_insns[i]))
      mep_have_copro_copro_moves_p = true;

  /* See whether we can directly move values between core and
     coprocessor registers.  */
  mep_have_core_copro_moves_p = (MEP_INTRINSIC_AVAILABLE_P (mep_cmov1)
                                 && MEP_INTRINSIC_AVAILABLE_P (mep_cmov2));

  mep_have_core_copro_moves_p = 1;
}

/* Declare all available intrinsic functions.  Called once only.  */

static tree cp_data_bus_int_type_node;
static tree opaque_vector_type_node;
static tree v8qi_type_node;
static tree v4hi_type_node;
static tree v2si_type_node;
static tree v8uqi_type_node;
static tree v4uhi_type_node;
static tree v2usi_type_node;

static tree
mep_cgen_regnum_to_type (enum cgen_regnum_operand_type cr)
{
  switch (cr)
    {
    case cgen_regnum_operand_type_POINTER:	return ptr_type_node;
    case cgen_regnum_operand_type_LONG:		return long_integer_type_node;
    case cgen_regnum_operand_type_ULONG:	return long_unsigned_type_node;
    case cgen_regnum_operand_type_SHORT:	return short_integer_type_node;
    case cgen_regnum_operand_type_USHORT:	return short_unsigned_type_node;
    case cgen_regnum_operand_type_CHAR:		return char_type_node;
    case cgen_regnum_operand_type_UCHAR:	return unsigned_char_type_node;
    case cgen_regnum_operand_type_SI:		return intSI_type_node;
    case cgen_regnum_operand_type_DI:		return intDI_type_node;
    case cgen_regnum_operand_type_VECTOR:	return opaque_vector_type_node;
    case cgen_regnum_operand_type_V8QI:		return v8qi_type_node;
    case cgen_regnum_operand_type_V4HI:		return v4hi_type_node;
    case cgen_regnum_operand_type_V2SI:		return v2si_type_node;
    case cgen_regnum_operand_type_V8UQI:	return v8uqi_type_node;
    case cgen_regnum_operand_type_V4UHI:	return v4uhi_type_node;
    case cgen_regnum_operand_type_V2USI:	return v2usi_type_node;
    case cgen_regnum_operand_type_CP_DATA_BUS_INT: return cp_data_bus_int_type_node;
    default:
      return void_type_node;
    }
}

static void
mep_init_builtins (void)
{
  size_t i;

  if (TARGET_64BIT_CR_REGS)
    cp_data_bus_int_type_node = long_long_integer_type_node;
  else
    cp_data_bus_int_type_node = long_integer_type_node;

  opaque_vector_type_node = build_opaque_vector_type (intQI_type_node, 8);
  v8qi_type_node = build_vector_type (intQI_type_node, 8);
  v4hi_type_node = build_vector_type (intHI_type_node, 4);
  v2si_type_node = build_vector_type (intSI_type_node, 2);
  v8uqi_type_node = build_vector_type (unsigned_intQI_type_node, 8);
  v4uhi_type_node = build_vector_type (unsigned_intHI_type_node, 4);
  v2usi_type_node = build_vector_type (unsigned_intSI_type_node, 2);

  add_builtin_type ("cp_data_bus_int", cp_data_bus_int_type_node);

  add_builtin_type ("cp_vector", opaque_vector_type_node);

  add_builtin_type ("cp_v8qi", v8qi_type_node);
  add_builtin_type ("cp_v4hi", v4hi_type_node);
  add_builtin_type ("cp_v2si", v2si_type_node);

  add_builtin_type ("cp_v8uqi", v8uqi_type_node);
  add_builtin_type ("cp_v4uhi", v4uhi_type_node);
  add_builtin_type ("cp_v2usi", v2usi_type_node);

  /* Intrinsics like mep_cadd3 are implemented with two groups of
     instructions, one which uses UNSPECs and one which uses a specific
     rtl code such as PLUS.  Instructions in the latter group belong
     to GROUP_KNOWN_CODE.

     In such cases, the intrinsic will have two entries in the global
     tables above.  The unspec form is accessed using builtin functions
     while the specific form is accessed using the mep_* enum in
     mep-intrin.h.

     The idea is that __cop arithmetic and builtin functions have
     different optimization requirements.  If mep_cadd3() appears in
     the source code, the user will surely except gcc to use cadd3
     rather than a work-alike such as add3.  However, if the user
     just writes "a + b", where a or b are __cop variables, it is
     reasonable for gcc to choose a core instruction rather than
     cadd3 if it believes that is more optimal.  */
  for (i = 0; i < ARRAY_SIZE (cgen_insns); i++)
    if ((cgen_insns[i].groups & GROUP_KNOWN_CODE) == 0
	&& mep_intrinsic_insn[cgen_insns[i].intrinsic] >= 0)
      {
	tree ret_type = void_type_node;
	tree bi_type;

	if (i > 0 && cgen_insns[i].intrinsic == cgen_insns[i-1].intrinsic)
	  continue;

	if (cgen_insns[i].cret_p)
	  ret_type = mep_cgen_regnum_to_type (cgen_insns[i].regnums[0].type);

	bi_type = build_function_type_list (ret_type, NULL_TREE);
	add_builtin_function (cgen_intrinsics[cgen_insns[i].intrinsic],
			      bi_type,
			      cgen_insns[i].intrinsic, BUILT_IN_MD, NULL, NULL);
      }
}

/* Report the unavailablity of the given intrinsic.  */

#if 1
static void
mep_intrinsic_unavailable (int intrinsic)
{
  static int already_reported_p[ARRAY_SIZE (cgen_intrinsics)];

  if (already_reported_p[intrinsic])
    return;

  if (mep_intrinsic_insn[intrinsic] < 0)
    error ("coprocessor intrinsic %qs is not available in this configuration",
	   cgen_intrinsics[intrinsic]);
  else if (CGEN_CURRENT_GROUP == GROUP_VLIW)
    error ("%qs is not available in VLIW functions",
	   cgen_intrinsics[intrinsic]);
  else
    error ("%qs is not available in non-VLIW functions",
	   cgen_intrinsics[intrinsic]);

  already_reported_p[intrinsic] = 1;
}
#endif


/* See if any implementation of INTRINSIC is available to the
   current function.  If so, store the most general implementation
   in *INSN_PTR and return true.  Return false otherwise.  */

static bool
mep_get_intrinsic_insn (int intrinsic ATTRIBUTE_UNUSED, const struct cgen_insn **insn_ptr ATTRIBUTE_UNUSED)
{
  int i;

  i = mep_intrinsic_insn[intrinsic];
  while (i >= 0 && !CGEN_ENABLE_INSN_P (i))
    i = mep_intrinsic_chain[i];

  if (i >= 0)
    {
      *insn_ptr = &cgen_insns[i];
      return true;
    }
  return false;
}


/* Like mep_get_intrinsic_insn, but with extra handling for moves.
   If INTRINSIC is mep_cmov, but there is no pure CR <- CR move insn,
   try using a work-alike instead.  In this case, the returned insn
   may have three operands rather than two.  */

static bool
mep_get_move_insn (int intrinsic, const struct cgen_insn **cgen_insn)
{
  size_t i;

  if (intrinsic == mep_cmov)
    {
      for (i = 0; i < ARRAY_SIZE (mep_cmov_insns); i++)
	if (mep_get_intrinsic_insn (mep_cmov_insns[i], cgen_insn))
	  return true;
      return false;
    }
  return mep_get_intrinsic_insn (intrinsic, cgen_insn);
}


/* If ARG is a register operand that is the same size as MODE, convert it
   to MODE using a subreg.  Otherwise return ARG as-is.  */

static rtx
mep_convert_arg (enum machine_mode mode, rtx arg)
{
  if (GET_MODE (arg) != mode
      && register_operand (arg, VOIDmode)
      && GET_MODE_SIZE (GET_MODE (arg)) == GET_MODE_SIZE (mode))
    return simplify_gen_subreg (mode, arg, GET_MODE (arg), 0);
  return arg;
}


/* Apply regnum conversions to ARG using the description given by REGNUM.
   Return the new argument on success and null on failure.  */

static rtx
mep_convert_regnum (const struct cgen_regnum_operand *regnum, rtx arg)
{
  if (regnum->count == 0)
    return arg;

  if (GET_CODE (arg) != CONST_INT
      || INTVAL (arg) < 0
      || INTVAL (arg) >= regnum->count)
    return 0;

  return gen_rtx_REG (SImode, INTVAL (arg) + regnum->base);
}


/* Try to make intrinsic argument ARG match the given operand.
   UNSIGNED_P is true if the argument has an unsigned type.  */

static rtx
mep_legitimize_arg (const struct insn_operand_data *operand, rtx arg,
		    int unsigned_p)
{
  if (GET_CODE (arg) == CONST_INT)
    {
      /* CONST_INTs can only be bound to integer operands.  */
      if (GET_MODE_CLASS (operand->mode) != MODE_INT)
	return 0;
    }
  else if (GET_CODE (arg) == CONST_DOUBLE)
    /* These hold vector constants.  */;
  else if (GET_MODE_SIZE (GET_MODE (arg)) != GET_MODE_SIZE (operand->mode))
    {
      /* If the argument is a different size from what's expected, we must
	 have a value in the right mode class in order to convert it.  */
      if (GET_MODE_CLASS (operand->mode) != GET_MODE_CLASS (GET_MODE (arg)))
	return 0;

      /* If the operand is an rvalue, promote or demote it to match the
	 operand's size.  This might not need extra instructions when
	 ARG is a register value.  */
      if (operand->constraint[0] != '=')
	arg = convert_to_mode (operand->mode, arg, unsigned_p);
    }

  /* If the operand is an lvalue, bind the operand to a new register.
     The caller will copy this value into ARG after the main
     instruction.  By doing this always, we produce slightly more
     optimal code.  */
  /* But not for control registers.  */
  if (operand->constraint[0] == '='
      && (! REG_P (arg)
	  || ! (CONTROL_REGNO_P (REGNO (arg))
		|| CCR_REGNO_P (REGNO (arg))
		|| CR_REGNO_P (REGNO (arg)))
	  ))
    return gen_reg_rtx (operand->mode);

  /* Try simple mode punning.  */
  arg = mep_convert_arg (operand->mode, arg);
  if (operand->predicate (arg, operand->mode))
    return arg;

  /* See if forcing the argument into a register will make it match.  */
  if (GET_CODE (arg) == CONST_INT || GET_CODE (arg) == CONST_DOUBLE)
    arg = force_reg (operand->mode, arg);
  else
    arg = mep_convert_arg (operand->mode, force_reg (GET_MODE (arg), arg));
  if (operand->predicate (arg, operand->mode))
    return arg;

  return 0;
}


/* Report that ARG cannot be passed to argument ARGNUM of intrinsic
   function FNNAME.  OPERAND describes the operand to which ARGNUM
   is mapped.  */

static void
mep_incompatible_arg (const struct insn_operand_data *operand, rtx arg,
		      int argnum, tree fnname)
{
  size_t i;

  if (GET_CODE (arg) == CONST_INT)
    for (i = 0; i < ARRAY_SIZE (cgen_immediate_predicates); i++)
      if (operand->predicate == cgen_immediate_predicates[i].predicate)
	{
	  const struct cgen_immediate_predicate *predicate;
	  HOST_WIDE_INT argval;

	  predicate = &cgen_immediate_predicates[i];
	  argval = INTVAL (arg);
	  if (argval < predicate->lower || argval >= predicate->upper)
	    error ("argument %d of %qE must be in the range %d...%d",
		   argnum, fnname, predicate->lower, predicate->upper - 1);
	  else
	    error ("argument %d of %qE must be a multiple of %d",
		   argnum, fnname, predicate->align);
	  return;
	}

  error ("incompatible type for argument %d of %qE", argnum, fnname);
}

static rtx
mep_expand_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
		    rtx subtarget ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED,
		    int ignore ATTRIBUTE_UNUSED)
{
  rtx pat, op[10], arg[10];
  unsigned int a;
  int opindex, unsigned_p[10];
  tree fndecl, args;
  unsigned int n_args;
  tree fnname;
  const struct cgen_insn *cgen_insn;
  const struct insn_data_d *idata;
  unsigned int first_arg = 0;
  unsigned int builtin_n_args;

  fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  fnname = DECL_NAME (fndecl);

  /* Find out which instruction we should emit.  Note that some coprocessor
     intrinsics may only be available in VLIW mode, or only in normal mode.  */
  if (!mep_get_intrinsic_insn (DECL_FUNCTION_CODE (fndecl), &cgen_insn))
    {
      mep_intrinsic_unavailable (DECL_FUNCTION_CODE (fndecl));
      return NULL_RTX;
    }
  idata = &insn_data[cgen_insn->icode];

  builtin_n_args = cgen_insn->num_args;

  if (cgen_insn->cret_p)
    {
      if (cgen_insn->cret_p > 1)
	builtin_n_args ++;
      first_arg = 1;
      mep_cgen_regnum_to_type (cgen_insn->regnums[0].type);
      builtin_n_args --;
    }

  /* Evaluate each argument.  */
  n_args = call_expr_nargs (exp);

  if (n_args < builtin_n_args)
    {
      error ("too few arguments to %qE", fnname);
      return NULL_RTX;
    }
  if (n_args > builtin_n_args)
    {
      error ("too many arguments to %qE", fnname);
      return NULL_RTX;
    }

  for (a = first_arg; a < builtin_n_args + first_arg; a++)
    {
      tree value;

      args = CALL_EXPR_ARG (exp, a - first_arg);

      value = args;

#if 0
      if (cgen_insn->regnums[a].reference_p)
	{
	  if (TREE_CODE (value) != ADDR_EXPR)
	    {
	      debug_tree(value);
	      error ("argument %d of %qE must be an address", a+1, fnname);
	      return NULL_RTX;
	    }
	  value = TREE_OPERAND (value, 0);
	}
#endif

      /* If the argument has been promoted to int, get the unpromoted
	 value.  This is necessary when sub-int memory values are bound
	 to reference parameters.  */
      if (TREE_CODE (value) == NOP_EXPR
	  && TREE_TYPE (value) == integer_type_node
	  && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0)))
	  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (value, 0)))
	      < TYPE_PRECISION (TREE_TYPE (value))))
	value = TREE_OPERAND (value, 0);

      /* If the argument has been promoted to double, get the unpromoted
	 SFmode value.  This is necessary for FMAX support, for example.  */
      if (TREE_CODE (value) == NOP_EXPR
	  && SCALAR_FLOAT_TYPE_P (TREE_TYPE (value))
	  && SCALAR_FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (value, 0)))
	  && TYPE_MODE (TREE_TYPE (value)) == DFmode
	  && TYPE_MODE (TREE_TYPE (TREE_OPERAND (value, 0))) == SFmode)
	value = TREE_OPERAND (value, 0);

      unsigned_p[a] = TYPE_UNSIGNED (TREE_TYPE (value));
      arg[a] = expand_expr (value, NULL, VOIDmode, EXPAND_NORMAL);
      arg[a] = mep_convert_regnum (&cgen_insn->regnums[a], arg[a]);
      if (cgen_insn->regnums[a].reference_p)
	{
	  tree pointed_to = TREE_TYPE (TREE_TYPE (value));
	  enum machine_mode pointed_mode = TYPE_MODE (pointed_to);

	  arg[a] = gen_rtx_MEM (pointed_mode, arg[a]);
	}
      if (arg[a] == 0)
	{
	  error ("argument %d of %qE must be in the range %d...%d",
		 a + 1, fnname, 0, cgen_insn->regnums[a].count - 1);
	  return NULL_RTX;
	}
    }

  for (a = 0; a < first_arg; a++)
    {
      if (a == 0 && target && GET_MODE (target) == idata->operand[0].mode)
	arg[a] = target;
      else
	arg[a] = gen_reg_rtx (idata->operand[0].mode);
    }

  /* Convert the arguments into a form suitable for the intrinsic.
     Report an error if this isn't possible.  */
  for (opindex = 0; opindex < idata->n_operands; opindex++)
    {
      a = cgen_insn->op_mapping[opindex];
      op[opindex] = mep_legitimize_arg (&idata->operand[opindex],
					arg[a], unsigned_p[a]);
      if (op[opindex] == 0)
	{
	  mep_incompatible_arg (&idata->operand[opindex],
				arg[a], a + 1 - first_arg, fnname);
	  return NULL_RTX;
	}
    }

  /* Emit the instruction.  */
  pat = idata->genfun (op[0], op[1], op[2], op[3], op[4],
		       op[5], op[6], op[7], op[8], op[9]);

  if (GET_CODE (pat) == SET
      && GET_CODE (SET_DEST (pat)) == PC
      && GET_CODE (SET_SRC (pat)) == IF_THEN_ELSE)
    emit_jump_insn (pat);
  else
    emit_insn (pat);

  /* Copy lvalues back to their final locations.  */
  for (opindex = 0; opindex < idata->n_operands; opindex++)
    if (idata->operand[opindex].constraint[0] == '=')
      {
	a = cgen_insn->op_mapping[opindex];
	if (a >= first_arg)
	  {
	    if (GET_MODE_CLASS (GET_MODE (arg[a]))
		!= GET_MODE_CLASS (GET_MODE (op[opindex])))
	      emit_move_insn (arg[a], gen_lowpart (GET_MODE (arg[a]),
						   op[opindex]));
	    else
	      {
		/* First convert the operand to the right mode, then copy it
		   into the destination.  Doing the conversion as a separate
		   step (rather than using convert_move) means that we can
		   avoid creating no-op moves when ARG[A] and OP[OPINDEX]
		   refer to the same register.  */
		op[opindex] = convert_to_mode (GET_MODE (arg[a]),
					       op[opindex], unsigned_p[a]);
		if (!rtx_equal_p (arg[a], op[opindex]))
		  emit_move_insn (arg[a], op[opindex]);
	      }
	  }
      }

  if (first_arg > 0 && target && target != op[0])
    {
      emit_move_insn (target, op[0]);
    }

  return target;
}

static bool
mep_vector_mode_supported_p (enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return false;
}

/* A subroutine of global_reg_mentioned_p, returns 1 if *LOC mentions
   a global register.  */

static int
global_reg_mentioned_p_1 (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  int regno;
  rtx x = *loc;

  if (! x)
    return 0;

  switch (GET_CODE (x))
    {
    case SUBREG:
      if (REG_P (SUBREG_REG (x)))
	{
	  if (REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER
	      && global_regs[subreg_regno (x)])
	    return 1;
	  return 0;
	}
      break;

    case REG:
      regno = REGNO (x);
      if (regno < FIRST_PSEUDO_REGISTER && global_regs[regno])
	return 1;
      return 0;

    case SCRATCH:
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case LABEL_REF:
      return 0;

    case CALL:
      /* A non-constant call might use a global register.  */
      return 1;

    default:
      break;
    }

  return 0;
}

/* Returns nonzero if X mentions a global register.  */

static int
global_reg_mentioned_p (rtx x)
{
  if (INSN_P (x))
    {
      if (CALL_P (x))
	{
	  if (! RTL_CONST_OR_PURE_CALL_P (x))
	    return 1;
	  x = CALL_INSN_FUNCTION_USAGE (x);
	  if (x == 0)
	    return 0;
	}
      else
	x = PATTERN (x);
    }

  return for_each_rtx (&x, global_reg_mentioned_p_1, NULL);
}
/* Scheduling hooks for VLIW mode.

   Conceptually this is very simple: we have a two-pack architecture
   that takes one core insn and one coprocessor insn to make up either
   a 32- or 64-bit instruction word (depending on the option bit set in
   the chip).  I.e. in VL32 mode, we can pack one 16-bit core insn and
   one 16-bit cop insn; in VL64 mode we can pack one 16-bit core insn
   and one 48-bit cop insn or two 32-bit core/cop insns.

   In practice, instruction selection will be a bear.  Consider in
   VL64 mode the following insns

	add $1, 1
	cmov $cr0, $0

   these cannot pack, since the add is a 16-bit core insn and cmov
   is a 32-bit cop insn.  However,

	add3 $1, $1, 1
	cmov $cr0, $0

   packs just fine.  For good VLIW code generation in VL64 mode, we
   will have to have 32-bit alternatives for many of the common core
   insns.  Not implemented.  */

static int
mep_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  int cost_specified;

  if (REG_NOTE_KIND (link) != 0)
    {
      /* See whether INSN and DEP_INSN are intrinsics that set the same
	 hard register.  If so, it is more important to free up DEP_INSN
	 than it is to free up INSN.

	 Note that intrinsics like mep_mulr are handled differently from
	 the equivalent mep.md patterns.  In mep.md, if we don't care
	 about the value of $lo and $hi, the pattern will just clobber
	 the registers, not set them.  Since clobbers don't count as
	 output dependencies, it is often possible to reorder two mulrs,
	 even after reload.

	 In contrast, mep_mulr() sets both $lo and $hi to specific values,
	 so any pair of mep_mulr()s will be inter-dependent.   We should
	 therefore give the first mep_mulr() a higher priority.  */
      if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT
	  && global_reg_mentioned_p (PATTERN (insn))
	  && global_reg_mentioned_p (PATTERN (dep_insn)))
	return 1;

      /* If the dependence is an anti or output dependence, assume it
	 has no cost.  */
      return 0;
    }

  /* If we can't recognize the insns, we can't really do anything.  */
  if (recog_memoized (dep_insn) < 0)
    return cost;

  /* The latency attribute doesn't apply to MeP-h1: we use the stall
     attribute instead.  */
  if (!TARGET_H1)
    {
      cost_specified = get_attr_latency (dep_insn);
      if (cost_specified != 0)
	return cost_specified;
    }

  return cost;
}

/* ??? We don't properly compute the length of a load/store insn,
   taking into account the addressing mode.  */

static int
mep_issue_rate (void)
{
  return TARGET_IVC2 ? 3 : 2;
}

/* Return true if function DECL was declared with the vliw attribute.  */

bool
mep_vliw_function_p (tree decl)
{
  return lookup_attribute ("vliw", TYPE_ATTRIBUTES (TREE_TYPE (decl))) != 0;
}

static rtx
mep_find_ready_insn (rtx *ready, int nready, enum attr_slot slot, int length)
{
  int i;

  for (i = nready - 1; i >= 0; --i)
    {
      rtx insn = ready[i];
      if (recog_memoized (insn) >= 0
	  && get_attr_slot (insn) == slot
	  && get_attr_length (insn) == length)
	return insn;
    }

  return NULL_RTX;
}

static void
mep_move_ready_insn (rtx *ready, int nready, rtx insn)
{
  int i;

  for (i = 0; i < nready; ++i)
    if (ready[i] == insn)
      {
	for (; i < nready - 1; ++i)
	  ready[i] = ready[i + 1];
	ready[i] = insn;
	return;
      }

  gcc_unreachable ();
}

static void
mep_print_sched_insn (FILE *dump, rtx insn)
{
  const char *slots = "none";
  const char *name = NULL;
  int code;
  char buf[30];

  if (GET_CODE (PATTERN (insn)) == SET
      || GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      switch (get_attr_slots (insn))
	{
	case SLOTS_CORE: slots = "core"; break;
	case SLOTS_C3: slots = "c3"; break;
	case SLOTS_P0: slots = "p0"; break;
	case SLOTS_P0_P0S: slots = "p0,p0s"; break;
	case SLOTS_P0_P1: slots = "p0,p1"; break;
	case SLOTS_P0S: slots = "p0s"; break;
	case SLOTS_P0S_P1: slots = "p0s,p1"; break;
	case SLOTS_P1: slots = "p1"; break;
	default:
	  sprintf(buf, "%d", get_attr_slots (insn));
	  slots = buf;
	  break;
	}
    }
  if (GET_CODE (PATTERN (insn)) == USE)
    slots = "use";

  code = INSN_CODE (insn);
  if (code >= 0)
    name = get_insn_name (code);
  if (!name)
    name = "{unknown}";

  fprintf (dump,
	   "insn %4d %4d  %8s  %s\n",
	   code,
	   INSN_UID (insn),
	   name,
	   slots);
}

static int
mep_sched_reorder (FILE *dump ATTRIBUTE_UNUSED,
		   int sched_verbose ATTRIBUTE_UNUSED, rtx *ready,
		   int *pnready, int clock ATTRIBUTE_UNUSED)
{
  int nready = *pnready;
  rtx core_insn, cop_insn;
  int i;

  if (dump && sched_verbose > 1)
    {
      fprintf (dump, "\nsched_reorder: clock %d nready %d\n", clock, nready);
      for (i=0; i<nready; i++)
	mep_print_sched_insn (dump, ready[i]);
      fprintf (dump, "\n");
    }

  if (!mep_vliw_function_p (cfun->decl))
    return 1;
  if (nready < 2)
    return 1;

  /* IVC2 uses a DFA to determine what's ready and what's not. */
  if (TARGET_IVC2)
    return nready;

  /* We can issue either a core or coprocessor instruction.
     Look for a matched pair of insns to reorder.  If we don't
     find any, don't second-guess the scheduler's priorities.  */

  if ((core_insn = mep_find_ready_insn (ready, nready, SLOT_CORE, 2))
      && (cop_insn = mep_find_ready_insn (ready, nready, SLOT_COP,
					  TARGET_OPT_VL64 ? 6 : 2)))
    ;
  else if (TARGET_OPT_VL64
	   && (core_insn = mep_find_ready_insn (ready, nready, SLOT_CORE, 4))
	   && (cop_insn = mep_find_ready_insn (ready, nready, SLOT_COP, 4)))
    ;
  else
    /* We didn't find a pair.  Issue the single insn at the head
       of the ready list.  */
    return 1;

  /* Reorder the two insns first.  */
  mep_move_ready_insn (ready, nready, core_insn);
  mep_move_ready_insn (ready, nready - 1, cop_insn);
  return 2;
}

/* A for_each_rtx callback.  Return true if *X is a register that is
   set by insn PREV.  */

static int
mep_store_find_set (rtx *x, void *prev)
{
  return REG_P (*x) && reg_set_p (*x, (const_rtx) prev);
}

/* Like mep_store_bypass_p, but takes a pattern as the second argument,
   not the containing insn.  */

static bool
mep_store_data_bypass_1 (rtx prev, rtx pat)
{
  /* Cope with intrinsics like swcpa.  */
  if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (mep_store_data_bypass_p (prev, XVECEXP (pat, 0, i)))
	  return true;

      return false;
    }

  /* Check for some sort of store.  */
  if (GET_CODE (pat) != SET
      || GET_CODE (SET_DEST (pat)) != MEM)
    return false;

  /* Intrinsics use patterns of the form (set (mem (scratch)) (unspec ...)).
     The first operand to the unspec is the store data and the other operands
     are used to calculate the address.  */
  if (GET_CODE (SET_SRC (pat)) == UNSPEC)
    {
      rtx src;
      int i;

      src = SET_SRC (pat);
      for (i = 1; i < XVECLEN (src, 0); i++)
	if (for_each_rtx (&XVECEXP (src, 0, i), mep_store_find_set, prev))
	  return false;

      return true;
    }

  /* Otherwise just check that PREV doesn't modify any register mentioned
     in the memory destination.  */
  return !for_each_rtx (&SET_DEST (pat), mep_store_find_set, prev);
}

/* Return true if INSN is a store instruction and if the store address
   has no true dependence on PREV.  */

bool
mep_store_data_bypass_p (rtx prev, rtx insn)
{
  return INSN_P (insn) ? mep_store_data_bypass_1 (prev, PATTERN (insn)) : false;
}

/* A for_each_rtx subroutine of mep_mul_hilo_bypass_p.  Return 1 if *X
   is a register other than LO or HI and if PREV sets *X.  */

static int
mep_mul_hilo_bypass_1 (rtx *x, void *prev)
{
  return (REG_P (*x)
	  && REGNO (*x) != LO_REGNO
	  && REGNO (*x) != HI_REGNO
	  && reg_set_p (*x, (const_rtx) prev));
}

/* Return true if, apart from HI/LO, there are no true dependencies
   between multiplication instructions PREV and INSN.  */

bool
mep_mul_hilo_bypass_p (rtx prev, rtx insn)
{
  rtx pat;

  pat = PATTERN (insn);
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);
  return (GET_CODE (pat) == SET
	  && !for_each_rtx (&SET_SRC (pat), mep_mul_hilo_bypass_1, prev));
}

/* Return true if INSN is an ldc instruction that issues to the
   MeP-h1 integer pipeline.  This is true for instructions that
   read from PSW, LP, SAR, HI and LO.  */

bool
mep_ipipe_ldc_p (rtx insn)
{
  rtx pat, src;

  pat = PATTERN (insn);

  /* Cope with instrinsics that set both a hard register and its shadow.
     The set of the hard register comes first.  */
  if (GET_CODE (pat) == PARALLEL)
    pat = XVECEXP (pat, 0, 0);

  if (GET_CODE (pat) == SET)
    {
      src = SET_SRC (pat);

      /* Cope with intrinsics.  The first operand to the unspec is
	 the source register.  */
      if (GET_CODE (src) == UNSPEC || GET_CODE (src) == UNSPEC_VOLATILE)
	src = XVECEXP (src, 0, 0);

      if (REG_P (src))
	switch (REGNO (src))
	  {
	  case PSW_REGNO:
	  case LP_REGNO:
	  case SAR_REGNO:
	  case HI_REGNO:
	  case LO_REGNO:
	    return true;
	  }
    }
  return false;
}

/* Create a VLIW bundle from core instruction CORE and coprocessor
   instruction COP.  COP always satisfies INSN_P, but CORE can be
   either a new pattern or an existing instruction.

   Emit the bundle in place of COP and return it.  */

static rtx
mep_make_bundle (rtx core, rtx cop)
{
  rtx insn;

  /* If CORE is an existing instruction, remove it, otherwise put
     the new pattern in an INSN harness.  */
  if (INSN_P (core))
    remove_insn (core);
  else
    core = make_insn_raw (core);

  /* Generate the bundle sequence and replace COP with it.  */
  insn = gen_rtx_SEQUENCE (VOIDmode, gen_rtvec (2, core, cop));
  insn = emit_insn_after (insn, cop);
  remove_insn (cop);

  /* Set up the links of the insns inside the SEQUENCE.  */
  PREV_INSN (core) = PREV_INSN (insn);
  NEXT_INSN (core) = cop;
  PREV_INSN (cop) = core;
  NEXT_INSN (cop) = NEXT_INSN (insn);

  /* Set the VLIW flag for the coprocessor instruction.  */
  PUT_MODE (core, VOIDmode);
  PUT_MODE (cop, BImode);

  /* Derive a location for the bundle.  Individual instructions cannot
     have their own location because there can be no assembler labels
     between CORE and COP.  */
  INSN_LOCATION (insn) = INSN_LOCATION (INSN_LOCATION (core) ? core : cop);
  INSN_LOCATION (core) = 0;
  INSN_LOCATION (cop) = 0;

  return insn;
}

/* A helper routine for ms1_insn_dependent_p called through note_stores.  */

static void
mep_insn_dependent_p_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data)
{
  rtx * pinsn = (rtx *) data;

  if (*pinsn && reg_mentioned_p (x, *pinsn))
    *pinsn = NULL_RTX;
}

/* Return true if anything in insn X is (anti,output,true) dependent on
   anything in insn Y.  */

static int
mep_insn_dependent_p (rtx x, rtx y)
{
  rtx tmp;

  gcc_assert (INSN_P (x));
  gcc_assert (INSN_P (y));

  tmp = PATTERN (y);
  note_stores (PATTERN (x), mep_insn_dependent_p_1, &tmp);
  if (tmp == NULL_RTX)
    return 1;

  tmp = PATTERN (x);
  note_stores (PATTERN (y), mep_insn_dependent_p_1, &tmp);
  if (tmp == NULL_RTX)
    return 1;

  return 0;
}

static int
core_insn_p (rtx insn)
{
  if (GET_CODE (PATTERN (insn)) == USE)
    return 0;
  if (get_attr_slot (insn) == SLOT_CORE)
    return 1;
  return 0;
}

/* Mark coprocessor instructions that can be bundled together with
   the immediately preceding core instruction.  This is later used
   to emit the "+" that tells the assembler to create a VLIW insn.

   For unbundled insns, the assembler will automatically add coprocessor
   nops, and 16-bit core nops.  Due to an apparent oversight in the
   spec, the assembler will _not_ automatically add 32-bit core nops,
   so we have to emit those here.

   Called from mep_insn_reorg.  */

static void
mep_bundle_insns (rtx insns)
{
  rtx insn, last = NULL_RTX, first = NULL_RTX;
  int saw_scheduling = 0;

  /* Only do bundling if we're in vliw mode.  */
  if (!mep_vliw_function_p (cfun->decl))
    return;

  /* The first insn in a bundle are TImode, the remainder are
     VOIDmode.  After this function, the first has VOIDmode and the
     rest have BImode.  */

  /* Note: this doesn't appear to be true for JUMP_INSNs.  */

  /* First, move any NOTEs that are within a bundle, to the beginning
     of the bundle.  */
  for (insn = insns; insn ; insn = NEXT_INSN (insn))
    {
      if (NOTE_P (insn) && first)
	/* Don't clear FIRST.  */;

      else if (NONJUMP_INSN_P (insn) && GET_MODE (insn) == TImode)
	first = insn;

      else if (NONJUMP_INSN_P (insn) && GET_MODE (insn) == VOIDmode && first)
	{
	  rtx note, prev;

	  /* INSN is part of a bundle; FIRST is the first insn in that
	     bundle.  Move all intervening notes out of the bundle.
	     In addition, since the debug pass may insert a label
	     whenever the current line changes, set the location info
	     for INSN to match FIRST.  */

	  INSN_LOCATION (insn) = INSN_LOCATION (first);

	  note = PREV_INSN (insn);
	  while (note && note != first)
	    {
	      prev = PREV_INSN (note);

	      if (NOTE_P (note))
		{
		  /* Remove NOTE from here... */
		  PREV_INSN (NEXT_INSN (note)) = PREV_INSN (note);
		  NEXT_INSN (PREV_INSN (note)) = NEXT_INSN (note);
		  /* ...and put it in here.  */
		  NEXT_INSN (note) = first;
		  PREV_INSN (note) = PREV_INSN (first);
		  NEXT_INSN (PREV_INSN (note)) = note;
		  PREV_INSN (NEXT_INSN (note)) = note;
		}

	      note = prev;
	    }
	}

      else if (!NONJUMP_INSN_P (insn))
	first = 0;
    }

  /* Now fix up the bundles.  */
  for (insn = insns; insn ; insn = NEXT_INSN (insn))
    {
      if (NOTE_P (insn))
	continue;

      if (!NONJUMP_INSN_P (insn))
	{
	  last = 0;
	  continue;
	}

      /* If we're not optimizing enough, there won't be scheduling
	 info.  We detect that here.  */
      if (GET_MODE (insn) == TImode)
	saw_scheduling = 1;
      if (!saw_scheduling)
	continue;

      if (TARGET_IVC2)
	{
	  rtx core_insn = NULL_RTX;

	  /* IVC2 slots are scheduled by DFA, so we just accept
	     whatever the scheduler gives us.  However, we must make
	     sure the core insn (if any) is the first in the bundle.
	     The IVC2 assembler can insert whatever NOPs are needed,
	     and allows a COP insn to be first.  */

	  if (NONJUMP_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) != USE
	      && GET_MODE (insn) == TImode)
	    {
	      for (last = insn;
		   NEXT_INSN (last)
		     && GET_MODE (NEXT_INSN (last)) == VOIDmode
		     && NONJUMP_INSN_P (NEXT_INSN (last));
		   last = NEXT_INSN (last))
		{
		  if (core_insn_p (last))
		    core_insn = last;
		}
	      if (core_insn_p (last))
		core_insn = last;

	      if (core_insn && core_insn != insn)
		{
		  /* Swap core insn to first in the bundle.  */

		  /* Remove core insn.  */
		  if (PREV_INSN (core_insn))
		    NEXT_INSN (PREV_INSN (core_insn)) = NEXT_INSN (core_insn);
		  if (NEXT_INSN (core_insn))
		    PREV_INSN (NEXT_INSN (core_insn)) = PREV_INSN (core_insn);

		  /* Re-insert core insn.  */
		  PREV_INSN (core_insn) = PREV_INSN (insn);
		  NEXT_INSN (core_insn) = insn;

		  if (PREV_INSN (core_insn))
		    NEXT_INSN (PREV_INSN (core_insn)) = core_insn;
		  PREV_INSN (insn) = core_insn;

		  PUT_MODE (core_insn, TImode);
		  PUT_MODE (insn, VOIDmode);
		}
	    }

	  /* The first insn has TImode, the rest have VOIDmode */
	  if (GET_MODE (insn) == TImode)
	    PUT_MODE (insn, VOIDmode);
	  else
	    PUT_MODE (insn, BImode);
	  continue;
	}

      PUT_MODE (insn, VOIDmode);
      if (recog_memoized (insn) >= 0
	  && get_attr_slot (insn) == SLOT_COP)
	{
	  if (JUMP_P (insn)
	      || ! last
	      || recog_memoized (last) < 0
	      || get_attr_slot (last) != SLOT_CORE
	      || (get_attr_length (insn)
		  != (TARGET_OPT_VL64 ? 8 : 4) - get_attr_length (last))
	      || mep_insn_dependent_p (insn, last))
	    {
	      switch (get_attr_length (insn))
		{
		case 8:
		  break;
		case 6:
		  insn = mep_make_bundle (gen_nop (), insn);
		  break;
		case 4:
		  if (TARGET_OPT_VL64)
		    insn = mep_make_bundle (gen_nop32 (), insn);
		  break;
		case 2:
		  if (TARGET_OPT_VL64)
		    error ("2 byte cop instructions are"
			   " not allowed in 64-bit VLIW mode");
		  else
		    insn = mep_make_bundle (gen_nop (), insn);
		  break;
		default:
		  error ("unexpected %d byte cop instruction",
			 get_attr_length (insn));
		  break;
		}
	    }
	  else
	    insn = mep_make_bundle (last, insn);
	}

      last = insn;
    }
}


/* Try to instantiate INTRINSIC with the operands given in OPERANDS.
   Return true on success.  This function can fail if the intrinsic
   is unavailable or if the operands don't satisfy their predicates.  */

bool
mep_emit_intrinsic (int intrinsic, const rtx *operands)
{
  const struct cgen_insn *cgen_insn;
  const struct insn_data_d *idata;
  rtx newop[10];
  int i;

  if (!mep_get_intrinsic_insn (intrinsic, &cgen_insn))
    return false;

  idata = &insn_data[cgen_insn->icode];
  for (i = 0; i < idata->n_operands; i++)
    {
      newop[i] = mep_convert_arg (idata->operand[i].mode, operands[i]);
      if (!idata->operand[i].predicate (newop[i], idata->operand[i].mode))
	return false;
    }

  emit_insn (idata->genfun (newop[0], newop[1], newop[2],
			    newop[3], newop[4], newop[5],
			    newop[6], newop[7], newop[8]));

  return true;
}


/* Apply the given unary intrinsic to OPERANDS[1] and store it on
   OPERANDS[0].  Report an error if the instruction could not
   be synthesized.  OPERANDS[1] is a register_operand.  For sign
   and zero extensions, it may be smaller than SImode.  */

bool
mep_expand_unary_intrinsic (int ATTRIBUTE_UNUSED intrinsic,
			    rtx * operands ATTRIBUTE_UNUSED)
{
  return false;
}


/* Likewise, but apply a binary operation to OPERANDS[1] and
   OPERANDS[2].  OPERANDS[1] is a register_operand, OPERANDS[2]
   can be a general_operand.

   IMMEDIATE and IMMEDIATE3 are intrinsics that take an immediate
   third operand.  REG and REG3 take register operands only.  */

bool
mep_expand_binary_intrinsic (int ATTRIBUTE_UNUSED immediate,
			     int ATTRIBUTE_UNUSED immediate3,
			     int ATTRIBUTE_UNUSED reg,
			     int ATTRIBUTE_UNUSED reg3,
			     rtx * operands ATTRIBUTE_UNUSED)
{
  return false;
}

static bool
mep_rtx_cost (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
	      int opno ATTRIBUTE_UNUSED, int *total,
	      bool ATTRIBUTE_UNUSED speed_t)
{
  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) >= -128 && INTVAL (x) < 127)
	*total = 0;
      else if (INTVAL (x) >= -32768 && INTVAL (x) < 65536)
	*total = 1;
      else
	*total = 3;
      return true;

    case SYMBOL_REF:
      *total = optimize_size ? COSTS_N_INSNS (0) : COSTS_N_INSNS (1);
      return true;

    case MULT:							
      *total = (GET_CODE (XEXP (x, 1)) == CONST_INT
		? COSTS_N_INSNS (3)
		: COSTS_N_INSNS (2));
      return true;
    }
  return false;
}

static int
mep_address_cost (rtx addr ATTRIBUTE_UNUSED,
		  enum machine_mode mode ATTRIBUTE_UNUSED,
		  addr_space_t as ATTRIBUTE_UNUSED,
		  bool ATTRIBUTE_UNUSED speed_p)
{
  return 1;
}

static void
mep_asm_init_sections (void)
{
  based_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   "\t.section .based,\"aw\"");

  tinybss_section
    = get_unnamed_section (SECTION_WRITE | SECTION_BSS, output_section_asm_op,
			   "\t.section .sbss,\"aw\"");

  sdata_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   "\t.section .sdata,\"aw\",@progbits");

  far_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   "\t.section .far,\"aw\"");

  farbss_section
    = get_unnamed_section (SECTION_WRITE | SECTION_BSS, output_section_asm_op,
			   "\t.section .farbss,\"aw\"");

  frodata_section
    = get_unnamed_section (0, output_section_asm_op,
			   "\t.section .frodata,\"a\"");

  srodata_section
    = get_unnamed_section (0, output_section_asm_op,
			   "\t.section .srodata,\"a\"");

  vtext_section
    = get_unnamed_section (SECTION_CODE | SECTION_MEP_VLIW, output_section_asm_op,
			   "\t.section .vtext,\"axv\"\n\t.vliw");

  vftext_section
    = get_unnamed_section (SECTION_CODE | SECTION_MEP_VLIW, output_section_asm_op,
			   "\t.section .vftext,\"axv\"\n\t.vliw");

  ftext_section
    = get_unnamed_section (SECTION_CODE, output_section_asm_op,
			   "\t.section .ftext,\"ax\"\n\t.core");

}

/* Initialize the GCC target structure.  */

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE	mep_start_function
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE		mep_attribute_table
#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES	mep_comp_type_attributes
#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES	mep_insert_attributes
#undef  TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P	mep_function_attribute_inlinable_p
#undef  TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P		mep_can_inline_p
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS	mep_section_type_flags
#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION	mep_asm_named_section
#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS		mep_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN		mep_expand_builtin
#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST	mep_adjust_cost
#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE		mep_issue_rate
#undef  TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER		mep_sched_reorder
#undef  TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING	mep_strip_name_encoding
#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION	mep_select_section
#undef  TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION	mep_unique_section
#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO	mep_encode_section_info
#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL	mep_function_ok_for_sibcall
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS		mep_rtx_cost
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST 		mep_address_cost
#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG  mep_reorg
#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS	mep_setup_incoming_varargs
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE        mep_pass_by_reference
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG             mep_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE     mep_function_arg_advance
#undef  TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P	mep_vector_mode_supported_p
#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE		mep_option_override
#undef  TARGET_ALLOCATE_INITIAL_VALUE
#define TARGET_ALLOCATE_INITIAL_VALUE   mep_allocate_initial_value
#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS 	mep_asm_init_sections
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY		mep_return_in_memory
#undef  TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD mep_narrow_volatile_bitfield
#undef	TARGET_EXPAND_BUILTIN_SAVEREGS
#define	TARGET_EXPAND_BUILTIN_SAVEREGS	mep_expand_builtin_saveregs
#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST	mep_build_builtin_va_list
#undef  TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START	mep_expand_va_start
#undef	TARGET_GIMPLIFY_VA_ARG_EXPR
#define	TARGET_GIMPLIFY_VA_ARG_EXPR	mep_gimplify_va_arg_expr
#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE            mep_can_eliminate
#undef  TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE	mep_conditional_register_usage
#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT		mep_trampoline_init
#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P	mep_legitimate_constant_p
#undef  TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P		can_use_doloop_if_innermost

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-mep.h"
