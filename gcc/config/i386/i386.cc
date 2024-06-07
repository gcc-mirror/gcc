/* Subroutines used for code generation on IA-32.
   Copyright (C) 1988-2024 Free Software Foundation, Inc.

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

#define INCLUDE_STRING
#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "cfgbuild.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "tm-constrs.h"
#include "cselib.h"
#include "sched-int.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "target-globals.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-vectorizer.h"
#include "shrink-wrap.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tree-iterator.h"
#include "dbgcnt.h"
#include "case-cfn-macros.h"
#include "dojump.h"
#include "fold-const-call.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "print-rtl.h"
#include "intl.h"
#include "ifcvt.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-options.h"
#include "i386-builtins.h"
#include "i386-expand.h"
#include "i386-features.h"
#include "function-abi.h"
#include "rtl-error.h"

/* This file should be included last.  */
#include "target-def.h"

static rtx legitimize_dllimport_symbol (rtx, bool);
static rtx legitimize_pe_coff_extern_decl (rtx, bool);
static void ix86_print_operand_address_as (FILE *, rtx, addr_space_t, bool);
static void ix86_emit_restore_reg_using_pop (rtx, bool = false);


#ifndef CHECK_STACK_LIMIT
#define CHECK_STACK_LIMIT (-1)
#endif

/* Return index of given mode in mult and division cost tables.  */
#define MODE_INDEX(mode)					\
  ((mode) == QImode ? 0						\
   : (mode) == HImode ? 1					\
   : (mode) == SImode ? 2					\
   : (mode) == DImode ? 3					\
   : 4)


/* Set by -mtune.  */
const struct processor_costs *ix86_tune_cost = NULL;

/* Set by -mtune or -Os.  */
const struct processor_costs *ix86_cost = NULL;

/* In case the average insn count for single function invocation is
   lower than this constant, emit fast (but longer) prologue and
   epilogue code.  */
#define FAST_PROLOGUE_INSN_COUNT 20

/* Names for 8 (low), 8 (high), and 16-bit registers, respectively.  */
static const char *const qi_reg_name[] = QI_REGISTER_NAMES;
static const char *const qi_high_reg_name[] = QI_HIGH_REGISTER_NAMES;
static const char *const hi_reg_name[] = HI_REGISTER_NAMES;

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS in i386.h.  */

enum reg_class const regclass_map[FIRST_PSEUDO_REGISTER] =
{
  /* ax, dx, cx, bx */
  AREG, DREG, CREG, BREG,
  /* si, di, bp, sp */
  SIREG, DIREG, NON_Q_REGS, NON_Q_REGS,
  /* FP registers */
  FP_TOP_REG, FP_SECOND_REG, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,
  /* arg pointer, flags, fpsr, frame */
  NON_Q_REGS, NO_REGS, NO_REGS, NON_Q_REGS,
  /* SSE registers */
  SSE_FIRST_REG, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  /* MMX registers */
  MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS,
  MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS,
  /* REX registers */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  /* SSE REX registers */
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  /* AVX-512 SSE registers */
  ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS,
  ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS,
  ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS,
  ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS, ALL_SSE_REGS,
  /* Mask registers.  */
  ALL_MASK_REGS, MASK_REGS, MASK_REGS, MASK_REGS,
  MASK_REGS, MASK_REGS, MASK_REGS, MASK_REGS,
  /* REX2 registers */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
};

/* The "default" register map used in 32bit mode.  */

int const debugger_register_map[FIRST_PSEUDO_REGISTER] =
{
  /* general regs */
  0, 2, 1, 3, 6, 7, 4, 5,
  /* fp regs */
  12, 13, 14, 15, 16, 17, 18, 19,
  /* arg, flags, fpsr, frame */
  IGNORED_DWARF_REGNUM, IGNORED_DWARF_REGNUM,
  IGNORED_DWARF_REGNUM, IGNORED_DWARF_REGNUM,
  /* SSE */
  21, 22, 23, 24, 25, 26, 27, 28,
  /* MMX */
  29, 30, 31, 32, 33, 34, 35, 36,
  /* extended integer registers */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* extended sse registers */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* AVX-512 registers 16-23 */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* AVX-512 registers 24-31 */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* Mask registers */
  93, 94, 95, 96, 97, 98, 99, 100
};

/* The "default" register map used in 64bit mode.  */

int const debugger64_register_map[FIRST_PSEUDO_REGISTER] =
{
  /* general regs */
  0, 1, 2, 3, 4, 5, 6, 7,
  /* fp regs */
  33, 34, 35, 36, 37, 38, 39, 40,
  /* arg, flags, fpsr, frame */
  IGNORED_DWARF_REGNUM, IGNORED_DWARF_REGNUM,
  IGNORED_DWARF_REGNUM, IGNORED_DWARF_REGNUM,
  /* SSE */
  17, 18, 19, 20, 21, 22, 23, 24,
  /* MMX */
  41, 42, 43, 44, 45, 46, 47, 48,
  /* extended integer registers */
  8, 9, 10, 11, 12, 13, 14, 15,
  /* extended SSE registers */
  25, 26, 27, 28, 29, 30, 31, 32,
  /* AVX-512 registers 16-23 */
  67, 68, 69, 70, 71, 72, 73, 74,
  /* AVX-512 registers 24-31 */
  75, 76, 77, 78, 79, 80, 81, 82,
  /* Mask registers */
  118, 119, 120, 121, 122, 123, 124, 125,
  /* rex2 extend interger registers */
  130, 131, 132, 133, 134, 135, 136, 137,
  138, 139, 140, 141, 142, 143, 144, 145
};

/* Define the register numbers to be used in Dwarf debugging information.
   The SVR4 reference port C compiler uses the following register numbers
   in its Dwarf output code:
	0 for %eax (gcc regno = 0)
	1 for %ecx (gcc regno = 2)
	2 for %edx (gcc regno = 1)
	3 for %ebx (gcc regno = 3)
	4 for %esp (gcc regno = 7)
	5 for %ebp (gcc regno = 6)
	6 for %esi (gcc regno = 4)
	7 for %edi (gcc regno = 5)
   The following three DWARF register numbers are never generated by
   the SVR4 C compiler or by the GNU compilers, but SDB on x86/svr4
   believed these numbers have these meanings.
	8  for %eip    (no gcc equivalent)
	9  for %eflags (gcc regno = 17)
	10 for %trapno (no gcc equivalent)
   It is not at all clear how we should number the FP stack registers
   for the x86 architecture.  If the version of SDB on x86/svr4 were
   a bit less brain dead with respect to floating-point then we would
   have a precedent to follow with respect to DWARF register numbers
   for x86 FP registers, but the SDB on x86/svr4 was so completely
   broken with respect to FP registers that it is hardly worth thinking
   of it as something to strive for compatibility with.
   The version of x86/svr4 SDB I had does (partially)
   seem to believe that DWARF register number 11 is associated with
   the x86 register %st(0), but that's about all.  Higher DWARF
   register numbers don't seem to be associated with anything in
   particular, and even for DWARF regno 11, SDB only seemed to under-
   stand that it should say that a variable lives in %st(0) (when
   asked via an `=' command) if we said it was in DWARF regno 11,
   but SDB still printed garbage when asked for the value of the
   variable in question (via a `/' command).
   (Also note that the labels SDB printed for various FP stack regs
   when doing an `x' command were all wrong.)
   Note that these problems generally don't affect the native SVR4
   C compiler because it doesn't allow the use of -O with -g and
   because when it is *not* optimizing, it allocates a memory
   location for each floating-point variable, and the memory
   location is what gets described in the DWARF AT_location
   attribute for the variable in question.
   Regardless of the severe mental illness of the x86/svr4 SDB, we
   do something sensible here and we use the following DWARF
   register numbers.  Note that these are all stack-top-relative
   numbers.
	11 for %st(0) (gcc regno = 8)
	12 for %st(1) (gcc regno = 9)
	13 for %st(2) (gcc regno = 10)
	14 for %st(3) (gcc regno = 11)
	15 for %st(4) (gcc regno = 12)
	16 for %st(5) (gcc regno = 13)
	17 for %st(6) (gcc regno = 14)
	18 for %st(7) (gcc regno = 15)
*/
int const svr4_debugger_register_map[FIRST_PSEUDO_REGISTER] =
{
  /* general regs */
  0, 2, 1, 3, 6, 7, 5, 4,
  /* fp regs */
  11, 12, 13, 14, 15, 16, 17, 18,
  /* arg, flags, fpsr, frame */
  IGNORED_DWARF_REGNUM, 9,
  IGNORED_DWARF_REGNUM, IGNORED_DWARF_REGNUM,
  /* SSE registers */
  21, 22, 23, 24, 25, 26, 27, 28,
  /* MMX registers */
  29, 30, 31, 32, 33, 34, 35, 36,
  /* extended integer registers */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* extended sse registers */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* AVX-512 registers 16-23 */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* AVX-512 registers 24-31 */
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM, INVALID_REGNUM,
  /* Mask registers */
  93, 94, 95, 96, 97, 98, 99, 100
};

/* Define parameter passing and return registers.  */

static int const x86_64_int_parameter_registers[6] =
{
  DI_REG, SI_REG, DX_REG, CX_REG, R8_REG, R9_REG
};

static int const x86_64_ms_abi_int_parameter_registers[4] =
{
  CX_REG, DX_REG, R8_REG, R9_REG
};

static int const x86_64_int_return_registers[4] =
{
  AX_REG, DX_REG, DI_REG, SI_REG
};

/* Define the structure for the machine field in struct function.  */

struct GTY(()) stack_local_entry {
  unsigned short mode;
  unsigned short n;
  rtx rtl;
  struct stack_local_entry *next;
};

/* Which cpu are we scheduling for.  */
enum attr_cpu ix86_schedule;

/* Which cpu are we optimizing for.  */
enum processor_type ix86_tune;

/* Which instruction set architecture to use.  */
enum processor_type ix86_arch;

/* True if processor has SSE prefetch instruction.  */
unsigned char ix86_prefetch_sse;

/* Preferred alignment for stack boundary in bits.  */
unsigned int ix86_preferred_stack_boundary;

/* Alignment for incoming stack boundary in bits specified at
   command line.  */
unsigned int ix86_user_incoming_stack_boundary;

/* Default alignment for incoming stack boundary in bits.  */
unsigned int ix86_default_incoming_stack_boundary;

/* Alignment for incoming stack boundary in bits.  */
unsigned int ix86_incoming_stack_boundary;

/* True if there is no direct access to extern symbols.  */
bool ix86_has_no_direct_extern_access;

/* Calling abi specific va_list type nodes.  */
tree sysv_va_list_type_node;
tree ms_va_list_type_node;

/* Prefix built by ASM_GENERATE_INTERNAL_LABEL.  */
char internal_label_prefix[16];
int internal_label_prefix_len;

/* Fence to use after loop using movnt.  */
tree x86_mfence;

/* Register class used for passing given 64bit part of the argument.
   These represent classes as documented by the PS ABI, with the exception
   of SSESF, SSEDF classes, that are basically SSE class, just gcc will
   use SF or DFmode move instead of DImode to avoid reformatting penalties.

   Similarly we play games with INTEGERSI_CLASS to use cheaper SImode moves
   whenever possible (upper half does contain padding).  */
enum x86_64_reg_class
  {
    X86_64_NO_CLASS,
    X86_64_INTEGER_CLASS,
    X86_64_INTEGERSI_CLASS,
    X86_64_SSE_CLASS,
    X86_64_SSEHF_CLASS,
    X86_64_SSESF_CLASS,
    X86_64_SSEDF_CLASS,
    X86_64_SSEUP_CLASS,
    X86_64_X87_CLASS,
    X86_64_X87UP_CLASS,
    X86_64_COMPLEX_X87_CLASS,
    X86_64_MEMORY_CLASS
  };

#define MAX_CLASSES 8

/* Table of constants used by fldpi, fldln2, etc....  */
static REAL_VALUE_TYPE ext_80387_constants_table [5];
static bool ext_80387_constants_init;


static rtx ix86_function_value (const_tree, const_tree, bool);
static bool ix86_function_value_regno_p (const unsigned int);
static unsigned int ix86_function_arg_boundary (machine_mode,
						const_tree);
static rtx ix86_static_chain (const_tree, bool);
static int ix86_function_regparm (const_tree, const_tree);
static void ix86_compute_frame_layout (void);
static tree ix86_canonical_va_list_type (tree);
static unsigned int split_stack_prologue_scratch_regno (void);
static bool i386_asm_output_addr_const_extra (FILE *, rtx);

static bool ix86_can_inline_p (tree, tree);
static unsigned int ix86_minimum_incoming_stack_boundary (bool);


/* Whether -mtune= or -march= were specified */
int ix86_tune_defaulted;
int ix86_arch_specified;

/* Return true if a red-zone is in use.  We can't use red-zone when
   there are local indirect jumps, like "indirect_jump" or "tablejump",
   which jumps to another place in the function, since "call" in the
   indirect thunk pushes the return address onto stack, destroying
   red-zone.

   TODO: If we can reserve the first 2 WORDs, for PUSH and, another
   for CALL, in red-zone, we can allow local indirect jumps with
   indirect thunk.  */

bool
ix86_using_red_zone (void)
{
  return (TARGET_RED_ZONE
	  && !TARGET_64BIT_MS_ABI
	  && (!cfun->machine->has_local_indirect_jump
	      || cfun->machine->indirect_branch_type == indirect_branch_keep));
}

/* Return true, if profiling code should be emitted before
   prologue. Otherwise it returns false.
   Note: For x86 with "hotfix" it is sorried.  */
static bool
ix86_profile_before_prologue (void)
{
  return flag_fentry != 0;
}

/* Update register usage after having seen the compiler flags.  */

static void
ix86_conditional_register_usage (void)
{
  int i, c_mask;

  /* If there are no caller-saved registers, preserve all registers.
     except fixed_regs and registers used for function return value
     since aggregate_value_p checks call_used_regs[regno] on return
     value.  */
  if (cfun
      && (cfun->machine->call_saved_registers
	  == TYPE_NO_CALLER_SAVED_REGISTERS))
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (!fixed_regs[i] && !ix86_function_value_regno_p (i))
	call_used_regs[i] = 0;

  /* For 32-bit targets, disable the REX registers.  */
  if (! TARGET_64BIT)
    {
      for (i = FIRST_REX_INT_REG; i <= LAST_REX_INT_REG; i++)
	CLEAR_HARD_REG_BIT (accessible_reg_set, i);
      for (i = FIRST_REX_SSE_REG; i <= LAST_REX_SSE_REG; i++)
	CLEAR_HARD_REG_BIT (accessible_reg_set, i);
      for (i = FIRST_EXT_REX_SSE_REG; i <= LAST_EXT_REX_SSE_REG; i++)
	CLEAR_HARD_REG_BIT (accessible_reg_set, i);
    }

  /*  See the definition of CALL_USED_REGISTERS in i386.h.  */
  c_mask = CALL_USED_REGISTERS_MASK (TARGET_64BIT_MS_ABI);
  
  CLEAR_HARD_REG_SET (reg_class_contents[(int)CLOBBERED_REGS]);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      /* Set/reset conditionally defined registers from
	 CALL_USED_REGISTERS initializer.  */
      if (call_used_regs[i] > 1)
	call_used_regs[i] = !!(call_used_regs[i] & c_mask);

      /* Calculate registers of CLOBBERED_REGS register set
	 as call used registers from GENERAL_REGS register set.  */
      if (TEST_HARD_REG_BIT (reg_class_contents[(int)GENERAL_REGS], i)
	  && call_used_regs[i])
	SET_HARD_REG_BIT (reg_class_contents[(int)CLOBBERED_REGS], i);
    }

  /* If MMX is disabled, disable the registers.  */
  if (! TARGET_MMX)
    accessible_reg_set &= ~reg_class_contents[MMX_REGS];

  /* If SSE is disabled, disable the registers.  */
  if (! TARGET_SSE)
    accessible_reg_set &= ~reg_class_contents[ALL_SSE_REGS];

  /* If the FPU is disabled, disable the registers.  */
  if (! (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387))
    accessible_reg_set &= ~reg_class_contents[FLOAT_REGS];

  /* If AVX512F is disabled, disable the registers.  */
  if (! TARGET_AVX512F)
    {
      for (i = FIRST_EXT_REX_SSE_REG; i <= LAST_EXT_REX_SSE_REG; i++)
	CLEAR_HARD_REG_BIT (accessible_reg_set, i);

      accessible_reg_set &= ~reg_class_contents[ALL_MASK_REGS];
    }

  /* If APX is disabled, disable the registers.  */
  if (! (TARGET_APX_EGPR && TARGET_64BIT))
    {
      for (i = FIRST_REX2_INT_REG; i <= LAST_REX2_INT_REG; i++)
	CLEAR_HARD_REG_BIT (accessible_reg_set, i);
    }
}

/* Canonicalize a comparison from one we don't have to one we do have.  */

static void
ix86_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
			      bool op0_preserve_value)
{
  /* The order of operands in x87 ficom compare is forced by combine in
     simplify_comparison () function. Float operator is treated as RTX_OBJ
     with a precedence over other operators and is always put in the first
     place. Swap condition and operands to match ficom instruction.  */
  if (!op0_preserve_value
      && GET_CODE (*op0) == FLOAT && MEM_P (XEXP (*op0, 0)) && REG_P (*op1))
    {
      enum rtx_code scode = swap_condition ((enum rtx_code) *code);

      /* We are called only for compares that are split to SAHF instruction.
	 Ensure that we have setcc/jcc insn for the swapped condition.  */
      if (ix86_fp_compare_code_to_integer (scode) != UNKNOWN)
	{
	  std::swap (*op0, *op1);
	  *code = (int) scode;
	}
    }
}


/* Hook to determine if one function can safely inline another.  */

static bool
ix86_can_inline_p (tree caller, tree callee)
{
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  /* Changes of those flags can be tolerated for always inlines. Lets hope
     user knows what he is doing.  */
  unsigned HOST_WIDE_INT always_inline_safe_mask
	 = (MASK_USE_8BIT_IDIV | MASK_ACCUMULATE_OUTGOING_ARGS
	    | MASK_NO_ALIGN_STRINGOPS | MASK_AVX256_SPLIT_UNALIGNED_LOAD
	    | MASK_AVX256_SPLIT_UNALIGNED_STORE | MASK_CLD
	    | MASK_NO_FANCY_MATH_387 | MASK_IEEE_FP | MASK_INLINE_ALL_STRINGOPS
	    | MASK_INLINE_STRINGOPS_DYNAMICALLY | MASK_RECIP | MASK_STACK_PROBE
	    | MASK_STV | MASK_TLS_DIRECT_SEG_REFS | MASK_VZEROUPPER
	    | MASK_NO_PUSH_ARGS | MASK_OMIT_LEAF_FRAME_POINTER);


  if (!callee_tree)
    callee_tree = target_option_default_node;
  if (!caller_tree)
    caller_tree = target_option_default_node;
  if (callee_tree == caller_tree)
    return true;

  struct cl_target_option *caller_opts = TREE_TARGET_OPTION (caller_tree);
  struct cl_target_option *callee_opts = TREE_TARGET_OPTION (callee_tree);
  bool ret = false;
  bool always_inline
    = (DECL_DISREGARD_INLINE_LIMITS (callee)
       && lookup_attribute ("always_inline",
			    DECL_ATTRIBUTES (callee)));

  /* If callee only uses GPRs, ignore MASK_80387.  */
  if (TARGET_GENERAL_REGS_ONLY_P (callee_opts->x_ix86_target_flags))
    always_inline_safe_mask |= MASK_80387;

  cgraph_node *callee_node = cgraph_node::get (callee);
  /* Callee's isa options should be a subset of the caller's, i.e. a SSE4
     function can inline a SSE2 function but a SSE2 function can't inline
     a SSE4 function.  */
  if (((caller_opts->x_ix86_isa_flags & callee_opts->x_ix86_isa_flags)
       != callee_opts->x_ix86_isa_flags)
      || ((caller_opts->x_ix86_isa_flags2 & callee_opts->x_ix86_isa_flags2)
	  != callee_opts->x_ix86_isa_flags2))
    ret = false;

  /* See if we have the same non-isa options.  */
  else if ((!always_inline
	    && caller_opts->x_target_flags != callee_opts->x_target_flags)
	   || (caller_opts->x_target_flags & ~always_inline_safe_mask)
	       != (callee_opts->x_target_flags & ~always_inline_safe_mask))
    ret = false;

  else if (caller_opts->x_ix86_fpmath != callee_opts->x_ix86_fpmath
	   /* If the calle doesn't use FP expressions differences in
	      ix86_fpmath can be ignored.  We are called from FEs
	      for multi-versioning call optimization, so beware of
	      ipa_fn_summaries not available.  */
	   && (! ipa_fn_summaries
	       || ipa_fn_summaries->get (callee_node) == NULL
	       || ipa_fn_summaries->get (callee_node)->fp_expressions))
    ret = false;

  /* At this point we cannot identify whether arch or tune setting
     comes from target attribute or not. So the most conservative way
     is to allow the callee that uses default arch and tune string to
     be inlined.  */
  else if (!strcmp (callee_opts->x_ix86_arch_string, "x86-64")
	   && !strcmp (callee_opts->x_ix86_tune_string, "generic"))
    ret = true;

  /* See if arch, tune, etc. are the same. As previous ISA flags already
     checks if callee's ISA is subset of caller's, do not block
     always_inline attribute for callee even it has different arch. */
  else if (!always_inline && caller_opts->arch != callee_opts->arch)
    ret = false;

  else if (!always_inline && caller_opts->tune != callee_opts->tune)
    ret = false;

  else if (!always_inline
	   && caller_opts->branch_cost != callee_opts->branch_cost)
    ret = false;

  else
    ret = true;

  return ret;
}

/* Return true if this goes in large data/bss.  */

static bool
ix86_in_large_data_p (tree exp)
{
  if (ix86_cmodel != CM_MEDIUM && ix86_cmodel != CM_MEDIUM_PIC
      && ix86_cmodel != CM_LARGE && ix86_cmodel != CM_LARGE_PIC)
    return false;

  if (exp == NULL_TREE)
    return false;

  /* Functions are never large data.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  /* Automatic variables are never large data.  */
  if (VAR_P (exp) && !is_global_var (exp))
    return false;

  if (VAR_P (exp) && DECL_SECTION_NAME (exp))
    {
      const char *section = DECL_SECTION_NAME (exp);
      if (strcmp (section, ".ldata") == 0
	  || strcmp (section, ".lbss") == 0)
	return true;
      return false;
    }
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
	 in data because it might be too big when completed.  Also,
	 int_size_in_bytes returns -1 if size can vary or is larger than
	 an integer in which case also it is safer to assume that it goes in
	 large data.  */
      if (size <= 0 || size > ix86_section_threshold)
	return true;
    }

  return false;
}

/* i386-specific section flag to mark large sections.  */
#define SECTION_LARGE SECTION_MACH_DEP

/* Switch to the appropriate section for output of DECL.
   DECL is either a `VAR_DECL' node or a constant of some sort.
   RELOC indicates whether forming the initial value of DECL requires
   link-time relocations.  */

ATTRIBUTE_UNUSED static section *
x86_64_elf_select_section (tree decl, int reloc,
			   unsigned HOST_WIDE_INT align)
{
  if (ix86_in_large_data_p (decl))
    {
      const char *sname = NULL;
      unsigned int flags = SECTION_WRITE | SECTION_LARGE;
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	  sname = ".ldata";
	  break;
	case SECCAT_DATA_REL:
	  sname = ".ldata.rel";
	  break;
	case SECCAT_DATA_REL_LOCAL:
	  sname = ".ldata.rel.local";
	  break;
	case SECCAT_DATA_REL_RO:
	  sname = ".ldata.rel.ro";
	  break;
	case SECCAT_DATA_REL_RO_LOCAL:
	  sname = ".ldata.rel.ro.local";
	  break;
	case SECCAT_BSS:
	  sname = ".lbss";
	  flags |= SECTION_BSS;
	  break;
	case SECCAT_RODATA:
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
	  sname = ".lrodata";
	  flags &= ~SECTION_WRITE;
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	case SECCAT_TEXT:
	case SECCAT_TDATA:
	case SECCAT_TBSS:
	  /* We don't split these for medium model.  Place them into
	     default sections and hope for best.  */
	  break;
	}
      if (sname)
	{
	  /* We might get called with string constants, but get_named_section
	     doesn't like them as they are not DECLs.  Also, we need to set
	     flags in that case.  */
	  if (!DECL_P (decl))
	    return get_section (sname, flags, NULL);
	  return get_named_section (decl, sname, reloc);
	}
    }
  return default_elf_select_section (decl, reloc, align);
}

/* Select a set of attributes for section NAME based on the properties
   of DECL and whether or not RELOC indicates that DECL's initializer
   might contain runtime relocations.  */

static unsigned int ATTRIBUTE_UNUSED
x86_64_elf_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  if (ix86_in_large_data_p (decl))
    flags |= SECTION_LARGE;

  if (decl == NULL_TREE
      && (strcmp (name, ".ldata.rel.ro") == 0
	  || strcmp (name, ".ldata.rel.ro.local") == 0))
    flags |= SECTION_RELRO;

  if (strcmp (name, ".lbss") == 0
      || startswith (name, ".lbss.")
      || startswith (name, ".gnu.linkonce.lb."))
    flags |= SECTION_BSS;

  return flags;
}

/* Build up a unique section name, expressed as a
   STRING_CST node, and assign it to DECL_SECTION_NAME (decl).
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  */

static void ATTRIBUTE_UNUSED
x86_64_elf_unique_section (tree decl, int reloc)
{
  if (ix86_in_large_data_p (decl))
    {
      const char *prefix = NULL;
      /* We only need to use .gnu.linkonce if we don't have COMDAT groups.  */
      bool one_only = DECL_COMDAT_GROUP (decl) && !HAVE_COMDAT_GROUP;

      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	case SECCAT_DATA_REL:
	case SECCAT_DATA_REL_LOCAL:
	case SECCAT_DATA_REL_RO:
	case SECCAT_DATA_REL_RO_LOCAL:
          prefix = one_only ? ".ld" : ".ldata";
	  break;
	case SECCAT_BSS:
          prefix = one_only ? ".lb" : ".lbss";
	  break;
	case SECCAT_RODATA:
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
          prefix = one_only ? ".lr" : ".lrodata";
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	case SECCAT_TEXT:
	case SECCAT_TDATA:
	case SECCAT_TBSS:
	  /* We don't split these for medium model.  Place them into
	     default sections and hope for best.  */
	  break;
	}
      if (prefix)
	{
	  const char *name, *linkonce;
	  char *string;

	  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  name = targetm.strip_name_encoding (name);

	  /* If we're using one_only, then there needs to be a .gnu.linkonce
     	     prefix to the section name.  */
	  linkonce = one_only ? ".gnu.linkonce" : "";

	  string = ACONCAT ((linkonce, prefix, ".", name, NULL));

	  set_decl_section_name (decl, string);
	  return;
	}
    }
  default_unique_section (decl, reloc);
}

#ifdef COMMON_ASM_OP

#ifndef LARGECOMM_SECTION_ASM_OP
#define LARGECOMM_SECTION_ASM_OP "\t.largecomm\t"
#endif

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.

   For medium model x86-64 we need to use LARGECOMM_SECTION_ASM_OP opcode for
   large objects.  */
void
x86_elf_aligned_decl_common (FILE *file, tree decl,
			const char *name, unsigned HOST_WIDE_INT size,
			unsigned align)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC
       || ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
      && size > (unsigned int)ix86_section_threshold)
    {
      switch_to_section (get_named_section (decl, ".lbss", 0));
      fputs (LARGECOMM_SECTION_ASM_OP, file);
    }
  else
    fputs (COMMON_ASM_OP, file);
  assemble_name (file, name);
  fprintf (file, "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n",
	   size, align / BITS_PER_UNIT);
}
#endif

/* Utility function for targets to use in implementing
   ASM_OUTPUT_ALIGNED_BSS.  */

void
x86_output_aligned_bss (FILE *file, tree decl, const char *name,
		       	unsigned HOST_WIDE_INT size, unsigned align)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC
       || ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
      && size > (unsigned int)ix86_section_threshold)
    switch_to_section (get_named_section (decl, ".lbss", 0));
  else
    switch_to_section (bss_section);
  ASM_OUTPUT_ALIGN (file, floor_log2 (align / BITS_PER_UNIT));
#ifdef ASM_DECLARE_OBJECT_NAME
  last_assemble_variable_decl = decl;
  ASM_DECLARE_OBJECT_NAME (file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */
  ASM_OUTPUT_SKIP (file, size ? size : 1);
}

/* Decide whether we must probe the stack before any space allocation
   on this target.  It's essentially TARGET_STACK_PROBE except when
   -fstack-check causes the stack to be already probed differently.  */

bool
ix86_target_stack_probe (void)
{
  /* Do not probe the stack twice if static stack checking is enabled.  */
  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    return false;

  return TARGET_STACK_PROBE;
}

/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
ix86_function_ok_for_sibcall (tree decl, tree exp)
{
  tree type, decl_or_type;
  rtx a, b;
  bool bind_global = decl && !targetm.binds_local_p (decl);

  if (ix86_function_naked (current_function_decl))
    return false;

  /* Sibling call isn't OK if there are no caller-saved registers
     since all registers must be preserved before return.  */
  if (cfun->machine->call_saved_registers
      == TYPE_NO_CALLER_SAVED_REGISTERS)
    return false;

  /* If we are generating position-independent code, we cannot sibcall
     optimize direct calls to global functions, as the PLT requires
     %ebx be live. (Darwin does not have a PLT.)  */
  if (!TARGET_MACHO
      && !TARGET_64BIT
      && flag_pic
      && flag_plt
      && bind_global)
    return false;

  /* If we need to align the outgoing stack, then sibcalling would
     unalign the stack, which may break the called function.  */
  if (ix86_minimum_incoming_stack_boundary (true)
      < PREFERRED_STACK_BOUNDARY)
    return false;

  if (decl)
    {
      decl_or_type = decl;
      type = TREE_TYPE (decl);
    }
  else
    {
      /* We're looking at the CALL_EXPR, we need the type of the function.  */
      type = CALL_EXPR_FN (exp);		/* pointer expression */
      type = TREE_TYPE (type);			/* pointer type */
      type = TREE_TYPE (type);			/* function type */
      decl_or_type = type;
    }

  /* Sibling call isn't OK if callee has no callee-saved registers
     and the calling function has callee-saved registers.  */
  if (cfun->machine->call_saved_registers != TYPE_NO_CALLEE_SAVED_REGISTERS
      && (cfun->machine->call_saved_registers
	  != TYPE_NO_CALLEE_SAVED_REGISTERS_EXCEPT_BP)
      && lookup_attribute ("no_callee_saved_registers",
			   TYPE_ATTRIBUTES (type)))
    return false;

  /* If outgoing reg parm stack space changes, we cannot do sibcall.  */
  if ((OUTGOING_REG_PARM_STACK_SPACE (type)
       != OUTGOING_REG_PARM_STACK_SPACE (TREE_TYPE (current_function_decl)))
      || (REG_PARM_STACK_SPACE (decl_or_type)
	  != REG_PARM_STACK_SPACE (current_function_decl)))
    {
      maybe_complain_about_tail_call (exp,
				      "inconsistent size of stack space"
				      " allocated for arguments which are"
				      " passed in registers");
      return false;
    }

  /* Check that the return value locations are the same.  Like
     if we are returning floats on the 80387 register stack, we cannot
     make a sibcall from a function that doesn't return a float to a
     function that does or, conversely, from a function that does return
     a float to a function that doesn't; the necessary stack adjustment
     would not be executed.  This is also the place we notice
     differences in the return value ABI.  Note that it is ok for one
     of the functions to have void return type as long as the return
     value of the other is passed in a register.  */
  a = ix86_function_value (TREE_TYPE (exp), decl_or_type, false);
  b = ix86_function_value (TREE_TYPE (DECL_RESULT (cfun->decl)),
			   cfun->decl, false);
  if (STACK_REG_P (a) || STACK_REG_P (b))
    {
      if (!rtx_equal_p (a, b))
	return false;
    }
  else if (VOID_TYPE_P (TREE_TYPE (DECL_RESULT (cfun->decl))))
    ;
  else if (!rtx_equal_p (a, b))
    return false;

  if (TARGET_64BIT)
    {
      /* The SYSV ABI has more call-clobbered registers;
	 disallow sibcalls from MS to SYSV.  */
      if (cfun->machine->call_abi == MS_ABI
	  && ix86_function_type_abi (type) == SYSV_ABI)
	return false;
    }
  else
    {
      /* If this call is indirect, we'll need to be able to use a
	 call-clobbered register for the address of the target function.
	 Make sure that all such registers are not used for passing
	 parameters.  Note that DLLIMPORT functions and call to global
	 function via GOT slot are indirect.  */
      if (!decl
	  || (bind_global && flag_pic && !flag_plt)
	  || (TARGET_DLLIMPORT_DECL_ATTRIBUTES && DECL_DLLIMPORT_P (decl))
	  || flag_force_indirect_call)
	{
	  /* Check if regparm >= 3 since arg_reg_available is set to
	     false if regparm == 0.  If regparm is 1 or 2, there is
	     always a call-clobbered register available.

	     ??? The symbol indirect call doesn't need a call-clobbered
	     register.  But we don't know if this is a symbol indirect
	     call or not here.  */
	  if (ix86_function_regparm (type, decl) >= 3
	      && !cfun->machine->arg_reg_available)
	    return false;
	}
    }

  if (decl && ix86_use_pseudo_pic_reg ())
    {
      /* When PIC register is used, it must be restored after ifunc
	 function returns.  */
       cgraph_node *node = cgraph_node::get (decl);
       if (node && node->ifunc_resolver)
	 return false;
    }

  /* Disable sibcall if callee has indirect_return attribute and
     caller doesn't since callee will return to the caller's caller
     via an indirect jump.  */
  if (((flag_cf_protection & (CF_RETURN | CF_BRANCH))
       == (CF_RETURN | CF_BRANCH))
      && lookup_attribute ("indirect_return", TYPE_ATTRIBUTES (type))
      && !lookup_attribute ("indirect_return",
			    TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl))))
    return false;

  /* Otherwise okay.  That also includes certain types of indirect calls.  */
  return true;
}

/* This function determines from TYPE the calling-convention.  */

unsigned int
ix86_get_callcvt (const_tree type)
{
  unsigned int ret = 0;
  bool is_stdarg;
  tree attrs;

  if (TARGET_64BIT)
    return IX86_CALLCVT_CDECL;

  attrs = TYPE_ATTRIBUTES (type);
  if (attrs != NULL_TREE)
    {
      if (lookup_attribute ("cdecl", attrs))
	ret |= IX86_CALLCVT_CDECL;
      else if (lookup_attribute ("stdcall", attrs))
	ret |= IX86_CALLCVT_STDCALL;
      else if (lookup_attribute ("fastcall", attrs))
	ret |= IX86_CALLCVT_FASTCALL;
      else if (lookup_attribute ("thiscall", attrs))
	ret |= IX86_CALLCVT_THISCALL;

      /* Regparam isn't allowed for thiscall and fastcall.  */
      if ((ret & (IX86_CALLCVT_THISCALL | IX86_CALLCVT_FASTCALL)) == 0)
	{
	  if (lookup_attribute ("regparm", attrs))
	    ret |= IX86_CALLCVT_REGPARM;
	  if (lookup_attribute ("sseregparm", attrs))
	    ret |= IX86_CALLCVT_SSEREGPARM;
	}

      if (IX86_BASE_CALLCVT(ret) != 0)
	return ret;
    }

  is_stdarg = stdarg_p (type);
  if (TARGET_RTD && !is_stdarg)
    return IX86_CALLCVT_STDCALL | ret;

  if (ret != 0
      || is_stdarg
      || TREE_CODE (type) != METHOD_TYPE
      || ix86_function_type_abi (type) != MS_ABI)
    return IX86_CALLCVT_CDECL | ret;

  return IX86_CALLCVT_THISCALL;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
ix86_comp_type_attributes (const_tree type1, const_tree type2)
{
  unsigned int ccvt1, ccvt2;

  if (TREE_CODE (type1) != FUNCTION_TYPE
      && TREE_CODE (type1) != METHOD_TYPE)
    return 1;

  ccvt1 = ix86_get_callcvt (type1);
  ccvt2 = ix86_get_callcvt (type2);
  if (ccvt1 != ccvt2)
    return 0;
  if (ix86_function_regparm (type1, NULL)
      != ix86_function_regparm (type2, NULL))
    return 0;

  if (lookup_attribute ("no_callee_saved_registers",
			TYPE_ATTRIBUTES (type1))
      != lookup_attribute ("no_callee_saved_registers",
			   TYPE_ATTRIBUTES (type2)))
    return 0;

  return 1;
}

/* Return the regparm value for a function with the indicated TYPE and DECL.
   DECL may be NULL when calling function indirectly
   or considering a libcall.  */

static int
ix86_function_regparm (const_tree type, const_tree decl)
{
  tree attr;
  int regparm;
  unsigned int ccvt;

  if (TARGET_64BIT)
    return (ix86_function_type_abi (type) == SYSV_ABI
	    ? X86_64_REGPARM_MAX : X86_64_MS_REGPARM_MAX);
  ccvt = ix86_get_callcvt (type);
  regparm = ix86_regparm;

  if ((ccvt & IX86_CALLCVT_REGPARM) != 0)
    {
      attr = lookup_attribute ("regparm", TYPE_ATTRIBUTES (type));
      if (attr)
	{
	  regparm = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr)));
	  return regparm;
	}
    }
  else if ((ccvt & IX86_CALLCVT_FASTCALL) != 0)
    return 2;
  else if ((ccvt & IX86_CALLCVT_THISCALL) != 0)
    return 1;

  /* Use register calling convention for local functions when possible.  */
  if (decl
      && TREE_CODE (decl) == FUNCTION_DECL)
    {
      cgraph_node *target = cgraph_node::get (decl);
      if (target)
	target = target->function_symbol ();

      /* Caller and callee must agree on the calling convention, so
	 checking here just optimize means that with
	 __attribute__((optimize (...))) caller could use regparm convention
	 and callee not, or vice versa.  Instead look at whether the callee
	 is optimized or not.  */
      if (target && opt_for_fn (target->decl, optimize)
	  && !(profile_flag && !flag_fentry))
	{
	  if (target->local && target->can_change_signature)
	    {
	      int local_regparm, globals = 0, regno;

	      /* Make sure no regparm register is taken by a
		 fixed register variable.  */
	      for (local_regparm = 0; local_regparm < REGPARM_MAX;
		   local_regparm++)
		if (fixed_regs[local_regparm])
		  break;

	      /* We don't want to use regparm(3) for nested functions as
		 these use a static chain pointer in the third argument.  */
	      if (local_regparm == 3 && DECL_STATIC_CHAIN (target->decl))
		local_regparm = 2;

	      /* Save a register for the split stack.  */
	      if (flag_split_stack)
		{
		  if (local_regparm == 3)
		    local_regparm = 2;
		  else if (local_regparm == 2
			   && DECL_STATIC_CHAIN (target->decl))
		    local_regparm = 1;
		}

	      /* Each fixed register usage increases register pressure,
		 so less registers should be used for argument passing.
		 This functionality can be overriden by an explicit
		 regparm value.  */
	      for (regno = AX_REG; regno <= DI_REG; regno++)
		if (fixed_regs[regno])
		  globals++;

	      local_regparm
		= globals < local_regparm ? local_regparm - globals : 0;

	      if (local_regparm > regparm)
		regparm = local_regparm;
	    }
	}
    }

  return regparm;
}

/* Return 1 or 2, if we can pass up to SSE_REGPARM_MAX SFmode (1) and
   DFmode (2) arguments in SSE registers for a function with the
   indicated TYPE and DECL.  DECL may be NULL when calling function
   indirectly or considering a libcall.  Return -1 if any FP parameter
   should be rejected by error.  This is used in siutation we imply SSE
   calling convetion but the function is called from another function with
   SSE disabled. Otherwise return 0.  */

static int
ix86_function_sseregparm (const_tree type, const_tree decl, bool warn)
{
  gcc_assert (!TARGET_64BIT);

  /* Use SSE registers to pass SFmode and DFmode arguments if requested
     by the sseregparm attribute.  */
  if (TARGET_SSEREGPARM
      || (type && lookup_attribute ("sseregparm", TYPE_ATTRIBUTES (type))))
    {
      if (!TARGET_SSE)
	{
	  if (warn)
	    {
	      if (decl)
		error ("calling %qD with attribute sseregparm without "
		       "SSE/SSE2 enabled", decl);
	      else
		error ("calling %qT with attribute sseregparm without "
		       "SSE/SSE2 enabled", type);
	    }
	  return 0;
	}

      return 2;
    }

  if (!decl)
    return 0;

  cgraph_node *target = cgraph_node::get (decl);
  if (target)
    target = target->function_symbol ();

  /* For local functions, pass up to SSE_REGPARM_MAX SFmode
     (and DFmode for SSE2) arguments in SSE registers.  */
  if (target
      /* TARGET_SSE_MATH */
      && (target_opts_for_fn (target->decl)->x_ix86_fpmath & FPMATH_SSE)
      && opt_for_fn (target->decl, optimize)
      && !(profile_flag && !flag_fentry))
    {
      if (target->local && target->can_change_signature)
	{
	  /* Refuse to produce wrong code when local function with SSE enabled
	     is called from SSE disabled function.
	     FIXME: We need a way to detect these cases cross-ltrans partition
	     and avoid using SSE calling conventions on local functions called
	     from function with SSE disabled.  For now at least delay the
	     warning until we know we are going to produce wrong code.
	     See PR66047  */
	  if (!TARGET_SSE && warn)
	    return -1;
	  return TARGET_SSE2_P (target_opts_for_fn (target->decl)
				->x_ix86_isa_flags) ? 2 : 1;
	}
    }

  return 0;
}

/* Return true if EAX is live at the start of the function.  Used by
   ix86_expand_prologue to determine if we need special help before
   calling allocate_stack_worker.  */

static bool
ix86_eax_live_at_start_p (void)
{
  /* Cheat.  Don't bother working forward from ix86_function_regparm
     to the function type to whether an actual argument is located in
     eax.  Instead just look at cfg info, which is still close enough
     to correct at this point.  This gives false positives for broken
     functions that might use uninitialized data that happens to be
     allocated in eax, but who cares?  */
  return REGNO_REG_SET_P (df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun)), 0);
}

static bool
ix86_keep_aggregate_return_pointer (tree fntype)
{
  tree attr;

  if (!TARGET_64BIT)
    {
      attr = lookup_attribute ("callee_pop_aggregate_return",
			       TYPE_ATTRIBUTES (fntype));
      if (attr)
	return (TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr))) == 0);

      /* For 32-bit MS-ABI the default is to keep aggregate
         return pointer.  */
      if (ix86_function_type_abi (fntype) == MS_ABI)
	return true;
    }
  return KEEP_AGGREGATE_RETURN_POINTER != 0;
}

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

static poly_int64
ix86_return_pops_args (tree fundecl, tree funtype, poly_int64 size)
{
  unsigned int ccvt;

  /* None of the 64-bit ABIs pop arguments.  */
  if (TARGET_64BIT)
    return 0;

  ccvt = ix86_get_callcvt (funtype);

  if ((ccvt & (IX86_CALLCVT_STDCALL | IX86_CALLCVT_FASTCALL
	       | IX86_CALLCVT_THISCALL)) != 0
      && ! stdarg_p (funtype))
    return size;

  /* Lose any fake structure return argument if it is passed on the stack.  */
  if (aggregate_value_p (TREE_TYPE (funtype), fundecl)
      && !ix86_keep_aggregate_return_pointer (funtype))
    {
      int nregs = ix86_function_regparm (funtype, fundecl);
      if (nregs == 0)
	return GET_MODE_SIZE (Pmode);
    }

  return 0;
}

/* Implement the TARGET_LEGITIMATE_COMBINED_INSN hook.  */

static bool
ix86_legitimate_combined_insn (rtx_insn *insn)
{
  int i;

  /* Check operand constraints in case hard registers were propagated
     into insn pattern.  This check prevents combine pass from
     generating insn patterns with invalid hard register operands.
     These invalid insns can eventually confuse reload to error out
     with a spill failure.  See also PRs 46829 and 46843.  */

  gcc_assert (INSN_CODE (insn) >= 0);

  extract_insn (insn);
  preprocess_constraints (insn);

  int n_operands = recog_data.n_operands;
  int n_alternatives = recog_data.n_alternatives;
  for (i = 0; i < n_operands; i++)
    {
      rtx op = recog_data.operand[i];
      machine_mode mode = GET_MODE (op);
      const operand_alternative *op_alt;
      int offset = 0;
      bool win;
      int j;

      /* A unary operator may be accepted by the predicate, but it
	 is irrelevant for matching constraints.  */
      if (UNARY_P (op))
	op = XEXP (op, 0);

      if (SUBREG_P (op))
	{
	  if (REG_P (SUBREG_REG (op))
	      && REGNO (SUBREG_REG (op)) < FIRST_PSEUDO_REGISTER)
	    offset = subreg_regno_offset (REGNO (SUBREG_REG (op)),
					  GET_MODE (SUBREG_REG (op)),
					  SUBREG_BYTE (op),
					  GET_MODE (op));
	  op = SUBREG_REG (op);
	}

      if (!(REG_P (op) && HARD_REGISTER_P (op)))
	continue;

      op_alt = recog_op_alt;

      /* Operand has no constraints, anything is OK.  */
      win = !n_alternatives;

      alternative_mask preferred = get_preferred_alternatives (insn);
      for (j = 0; j < n_alternatives; j++, op_alt += n_operands)
	{
	  if (!TEST_BIT (preferred, j))
	    continue;
	  if (op_alt[i].anything_ok
	      || (op_alt[i].matches != -1
		  && operands_match_p
		  (recog_data.operand[i],
		   recog_data.operand[op_alt[i].matches]))
	      || reg_fits_class_p (op, op_alt[i].cl, offset, mode))
	    {
	      win = true;
	      break;
	    }
	}

      if (!win)
	return false;
    }

  return true;
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
ix86_asan_shadow_offset (void)
{
  return SUBTARGET_SHADOW_OFFSET;
}

/* Argument support functions.  */

/* Return true when register may be used to pass function parameters.  */
bool
ix86_function_arg_regno_p (int regno)
{
  int i;
  enum calling_abi call_abi;
  const int *parm_regs;

  if (TARGET_SSE && SSE_REGNO_P (regno)
      && regno < FIRST_SSE_REG + SSE_REGPARM_MAX)
    return true;

   if (!TARGET_64BIT)
     return (regno < REGPARM_MAX
	     || (TARGET_MMX && MMX_REGNO_P (regno)
		 && regno < FIRST_MMX_REG + MMX_REGPARM_MAX));

  /* TODO: The function should depend on current function ABI but
     builtins.cc would need updating then. Therefore we use the
     default ABI.  */
  call_abi = ix86_cfun_abi ();

  /* RAX is used as hidden argument to va_arg functions.  */
  if (call_abi == SYSV_ABI && regno == AX_REG)
    return true;

  if (call_abi == MS_ABI)
    parm_regs = x86_64_ms_abi_int_parameter_registers;
  else
    parm_regs = x86_64_int_parameter_registers;

  for (i = 0; i < (call_abi == MS_ABI
		   ? X86_64_MS_REGPARM_MAX : X86_64_REGPARM_MAX); i++)
    if (regno == parm_regs[i])
      return true;
  return false;
}

/* Return if we do not know how to pass ARG solely in registers.  */

static bool
ix86_must_pass_in_stack (const function_arg_info &arg)
{
  if (must_pass_in_stack_var_size_or_pad (arg))
    return true;

  /* For 32-bit, we want TImode aggregates to go on the stack.  But watch out!
     The layout_type routine is crafty and tries to trick us into passing
     currently unsupported vector types on the stack by using TImode.  */
  return (!TARGET_64BIT && arg.mode == TImode
	  && arg.type && TREE_CODE (arg.type) != VECTOR_TYPE);
}

/* It returns the size, in bytes, of the area reserved for arguments passed
   in registers for the function represented by fndecl dependent to the used
   abi format.  */
int
ix86_reg_parm_stack_space (const_tree fndecl)
{
  enum calling_abi call_abi = SYSV_ABI;
  if (fndecl != NULL_TREE && TREE_CODE (fndecl) == FUNCTION_DECL)
    call_abi = ix86_function_abi (fndecl);
  else
    call_abi = ix86_function_type_abi (fndecl);
  if (TARGET_64BIT && call_abi == MS_ABI)
    return 32;
  return 0;
}

/* We add this as a workaround in order to use libc_has_function
   hook in i386.md.  */
bool
ix86_libc_has_function (enum function_class fn_class)
{
  return targetm.libc_has_function (fn_class, NULL_TREE);
}

/* Returns value SYSV_ABI, MS_ABI dependent on fntype,
   specifying the call abi used.  */
enum calling_abi
ix86_function_type_abi (const_tree fntype)
{
  enum calling_abi abi = ix86_abi;

  if (fntype == NULL_TREE || TYPE_ATTRIBUTES (fntype) == NULL_TREE)
    return abi;

  if (abi == SYSV_ABI
      && lookup_attribute ("ms_abi", TYPE_ATTRIBUTES (fntype)))
    {
      static int warned;
      if (TARGET_X32 && !warned)
	{
	  error ("X32 does not support %<ms_abi%> attribute");
	  warned = 1;
	}

      abi = MS_ABI;
    }
  else if (abi == MS_ABI
	   && lookup_attribute ("sysv_abi", TYPE_ATTRIBUTES (fntype)))
    abi = SYSV_ABI;

  return abi;
}

enum calling_abi
ix86_function_abi (const_tree fndecl)
{
  return fndecl ? ix86_function_type_abi (TREE_TYPE (fndecl)) : ix86_abi;
}

/* Returns value SYSV_ABI, MS_ABI dependent on cfun,
   specifying the call abi used.  */
enum calling_abi
ix86_cfun_abi (void)
{
  return cfun ? cfun->machine->call_abi : ix86_abi;
}

bool
ix86_function_ms_hook_prologue (const_tree fn)
{
  if (fn && lookup_attribute ("ms_hook_prologue", DECL_ATTRIBUTES (fn)))
    {
      if (decl_function_context (fn) != NULL_TREE)
	error_at (DECL_SOURCE_LOCATION (fn),
		  "%<ms_hook_prologue%> attribute is not compatible "
		  "with nested function");
      else
        return true;
    }
  return false;
}

bool
ix86_function_naked (const_tree fn)
{
  if (fn && lookup_attribute ("naked", DECL_ATTRIBUTES (fn)))
    return true;

  return false;
}

/* Write the extra assembler code needed to declare a function properly.  */

void
ix86_asm_output_function_label (FILE *out_file, const char *fname,
				tree decl)
{
  bool is_ms_hook = ix86_function_ms_hook_prologue (decl);

  if (cfun)
    cfun->machine->function_label_emitted = true;

  if (is_ms_hook)
    {
      int i, filler_count = (TARGET_64BIT ? 32 : 16);
      unsigned int filler_cc = 0xcccccccc;

      for (i = 0; i < filler_count; i += 4)
	fprintf (out_file, ASM_LONG " %#x\n", filler_cc);
    }

#ifdef SUBTARGET_ASM_UNWIND_INIT
  SUBTARGET_ASM_UNWIND_INIT (out_file);
#endif

  assemble_function_label_raw (out_file, fname);

  /* Output magic byte marker, if hot-patch attribute is set.  */
  if (is_ms_hook)
    {
      if (TARGET_64BIT)
	{
	  /* leaq [%rsp + 0], %rsp  */
	  fputs (ASM_BYTE "0x48, 0x8d, 0xa4, 0x24, 0x00, 0x00, 0x00, 0x00\n",
		 out_file);
	}
      else
	{
          /* movl.s %edi, %edi
	     push   %ebp
	     movl.s %esp, %ebp */
	  fputs (ASM_BYTE "0x8b, 0xff, 0x55, 0x8b, 0xec\n", out_file);
	}
    }
}

/* Implementation of call abi switching target hook. Specific to FNDECL
   the specific call register sets are set.  See also
   ix86_conditional_register_usage for more details.  */
void
ix86_call_abi_override (const_tree fndecl)
{
  cfun->machine->call_abi = ix86_function_abi (fndecl);
}

/* Return 1 if pseudo register should be created and used to hold
   GOT address for PIC code.  */
bool
ix86_use_pseudo_pic_reg (void)
{
  if ((TARGET_64BIT
       && (ix86_cmodel == CM_SMALL_PIC
	   || TARGET_PECOFF))
      || !flag_pic)
    return false;
  return true;
}

/* Initialize large model PIC register.  */

static void
ix86_init_large_pic_reg (unsigned int tmp_regno)
{
  rtx_code_label *label;
  rtx tmp_reg;

  gcc_assert (Pmode == DImode);
  label = gen_label_rtx ();
  emit_label (label);
  LABEL_PRESERVE_P (label) = 1;
  tmp_reg = gen_rtx_REG (Pmode, tmp_regno);
  gcc_assert (REGNO (pic_offset_table_rtx) != tmp_regno);
  emit_insn (gen_set_rip_rex64 (pic_offset_table_rtx,
				label));
  emit_insn (gen_set_got_offset_rex64 (tmp_reg, label));
  emit_insn (gen_add2_insn (pic_offset_table_rtx, tmp_reg));
  const char *name = LABEL_NAME (label);
  PUT_CODE (label, NOTE);
  NOTE_KIND (label) = NOTE_INSN_DELETED_LABEL;
  NOTE_DELETED_LABEL_NAME (label) = name;
}

/* Create and initialize PIC register if required.  */
static void
ix86_init_pic_reg (void)
{
  edge entry_edge;
  rtx_insn *seq;

  if (!ix86_use_pseudo_pic_reg ())
    return;

  start_sequence ();

  if (TARGET_64BIT)
    {
      if (ix86_cmodel == CM_LARGE_PIC)
	ix86_init_large_pic_reg (R11_REG);
      else
	emit_insn (gen_set_got_rex64 (pic_offset_table_rtx));
    }
  else
    {
      /*  If there is future mcount call in the function it is more profitable
	  to emit SET_GOT into ABI defined REAL_PIC_OFFSET_TABLE_REGNUM.  */
      rtx reg = crtl->profile
		? gen_rtx_REG (Pmode, REAL_PIC_OFFSET_TABLE_REGNUM)
		: pic_offset_table_rtx;
      rtx_insn *insn = emit_insn (gen_set_got (reg));
      RTX_FRAME_RELATED_P (insn) = 1;
      if (crtl->profile)
        emit_move_insn (pic_offset_table_rtx, reg);
      add_reg_note (insn, REG_CFA_FLUSH_QUEUE, NULL_RTX);
    }

  seq = get_insns ();
  end_sequence ();

  entry_edge = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  insert_insn_on_edge (seq, entry_edge);
  commit_one_edge_insertion (entry_edge);
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (CUMULATIVE_ARGS *cum,  /* Argument info to initialize */
		      tree fntype,	/* tree ptr for function decl */
		      rtx libname,	/* SYMBOL_REF of library name or 0 */
		      tree fndecl,
		      int caller)
{
  struct cgraph_node *local_info_node = NULL;
  struct cgraph_node *target = NULL;

  /* Set silent_p to false to raise an error for invalid calls when
     expanding function body.  */
  cfun->machine->silent_p = false;

  memset (cum, 0, sizeof (*cum));

  if (fndecl)
    {
      target = cgraph_node::get (fndecl);
      if (target)
	{
	  target = target->function_symbol ();
	  local_info_node = cgraph_node::local_info_node (target->decl);
	  cum->call_abi = ix86_function_abi (target->decl);
	}
      else
	cum->call_abi = ix86_function_abi (fndecl);
    }
  else
    cum->call_abi = ix86_function_type_abi (fntype);

  cum->caller = caller;

  /* Set up the number of registers to use for passing arguments.  */
  cum->nregs = ix86_regparm;
  if (TARGET_64BIT)
    {
      cum->nregs = (cum->call_abi == SYSV_ABI
                   ? X86_64_REGPARM_MAX
                   : X86_64_MS_REGPARM_MAX);
    }
  if (TARGET_SSE)
    {
      cum->sse_nregs = SSE_REGPARM_MAX;
      if (TARGET_64BIT)
        {
          cum->sse_nregs = (cum->call_abi == SYSV_ABI
                           ? X86_64_SSE_REGPARM_MAX
                           : X86_64_MS_SSE_REGPARM_MAX);
        }
    }
  if (TARGET_MMX)
    cum->mmx_nregs = MMX_REGPARM_MAX;
  cum->warn_avx512f = true;
  cum->warn_avx = true;
  cum->warn_sse = true;
  cum->warn_mmx = true;

  /* Because type might mismatch in between caller and callee, we need to
     use actual type of function for local calls.
     FIXME: cgraph_analyze can be told to actually record if function uses
     va_start so for local functions maybe_vaarg can be made aggressive
     helping K&R code.
     FIXME: once typesytem is fixed, we won't need this code anymore.  */
  if (local_info_node && local_info_node->local
      && local_info_node->can_change_signature)
    fntype = TREE_TYPE (target->decl);
  cum->stdarg = stdarg_p (fntype);
  cum->maybe_vaarg = (fntype
		      ? (!prototype_p (fntype) || stdarg_p (fntype))
		      : !libname);

  cum->decl = fndecl;

  cum->warn_empty = !warn_abi || cum->stdarg;
  if (!cum->warn_empty && fntype)
    {
      function_args_iterator iter;
      tree argtype;
      bool seen_empty_type = false;
      FOREACH_FUNCTION_ARGS (fntype, argtype, iter)
	{
	  if (argtype == error_mark_node || VOID_TYPE_P (argtype))
	    break;
	  if (TYPE_EMPTY_P (argtype))
	    seen_empty_type = true;
	  else if (seen_empty_type)
	    {
	      cum->warn_empty = true;
	      break;
	    }
	}
    }

  if (!TARGET_64BIT)
    {
      /* If there are variable arguments, then we won't pass anything
         in registers in 32-bit mode. */
      if (stdarg_p (fntype))
	{
	  cum->nregs = 0;
	  /* Since in 32-bit, variable arguments are always passed on
	     stack, there is scratch register available for indirect
	     sibcall.  */
	  cfun->machine->arg_reg_available = true;
	  cum->sse_nregs = 0;
	  cum->mmx_nregs = 0;
	  cum->warn_avx512f = false;
	  cum->warn_avx = false;
	  cum->warn_sse = false;
	  cum->warn_mmx = false;
	  return;
	}

      /* Use ecx and edx registers if function has fastcall attribute,
	 else look for regparm information.  */
      if (fntype)
	{
	  unsigned int ccvt = ix86_get_callcvt (fntype);
	  if ((ccvt & IX86_CALLCVT_THISCALL) != 0)
	    {
	      cum->nregs = 1;
	      cum->fastcall = 1; /* Same first register as in fastcall.  */
	    }
	  else if ((ccvt & IX86_CALLCVT_FASTCALL) != 0)
	    {
	      cum->nregs = 2;
	      cum->fastcall = 1;
	    }
	  else
	    cum->nregs = ix86_function_regparm (fntype, fndecl);
	}

      /* Set up the number of SSE registers used for passing SFmode
	 and DFmode arguments.  Warn for mismatching ABI.  */
      cum->float_in_sse = ix86_function_sseregparm (fntype, fndecl, true);
    }

  cfun->machine->arg_reg_available = (cum->nregs > 0);
}

/* Return the "natural" mode for TYPE.  In most cases, this is just TYPE_MODE.
   But in the case of vector types, it is some vector mode.

   When we have only some of our vector isa extensions enabled, then there
   are some modes for which vector_mode_supported_p is false.  For these
   modes, the generic vector support in gcc will choose some non-vector mode
   in order to implement the type.  By computing the natural mode, we'll
   select the proper ABI location for the operand and not depend on whatever
   the middle-end decides to do with these vector types.

   The midde-end can't deal with the vector types > 16 bytes.  In this
   case, we return the original mode and warn ABI change if CUM isn't
   NULL. 

   If INT_RETURN is true, warn ABI change if the vector mode isn't
   available for function return value.  */

static machine_mode
type_natural_mode (const_tree type, const CUMULATIVE_ARGS *cum,
		   bool in_return)
{
  machine_mode mode = TYPE_MODE (type);

  if (VECTOR_TYPE_P (type) && !VECTOR_MODE_P (mode))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if ((size == 8 || size == 16 || size == 32 || size == 64)
	  /* ??? Generic code allows us to create width 1 vectors.  Ignore.  */
	  && TYPE_VECTOR_SUBPARTS (type) > 1)
	{
	  machine_mode innermode = TYPE_MODE (TREE_TYPE (type));

	  /* There are no XFmode vector modes ...  */
	  if (innermode == XFmode)
	    return mode;

	  /* ... and no decimal float vector modes.  */
	  if (DECIMAL_FLOAT_MODE_P (innermode))
	    return mode;

	  if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (type)))
	    mode = MIN_MODE_VECTOR_FLOAT;
	  else
	    mode = MIN_MODE_VECTOR_INT;

	  /* Get the mode which has this inner mode and number of units.  */
	  FOR_EACH_MODE_FROM (mode, mode)
	    if (GET_MODE_NUNITS (mode) == TYPE_VECTOR_SUBPARTS (type)
		&& GET_MODE_INNER (mode) == innermode)
	      {
		if (size == 64 && (!TARGET_AVX512F || !TARGET_EVEX512)
		    && !TARGET_IAMCU)
		  {
		    static bool warnedavx512f;
		    static bool warnedavx512f_ret;

		    if (cum && cum->warn_avx512f && !warnedavx512f)
		      {
			if (warning (OPT_Wpsabi, "AVX512F vector argument "
				     "without AVX512F enabled changes the ABI"))
			  warnedavx512f = true;
		      }
		    else if (in_return && !warnedavx512f_ret)
		      {
			if (warning (OPT_Wpsabi, "AVX512F vector return "
				     "without AVX512F enabled changes the ABI"))
			  warnedavx512f_ret = true;
		      }

		    return TYPE_MODE (type);
		  }
		else if (size == 32 && !TARGET_AVX && !TARGET_IAMCU)
		  {
		    static bool warnedavx;
		    static bool warnedavx_ret;

		    if (cum && cum->warn_avx && !warnedavx)
		      {
			if (warning (OPT_Wpsabi, "AVX vector argument "
				     "without AVX enabled changes the ABI"))
			  warnedavx = true;
		      }
		    else if (in_return && !warnedavx_ret)
		      {
			if (warning (OPT_Wpsabi, "AVX vector return "
				     "without AVX enabled changes the ABI"))
			  warnedavx_ret = true;
		      }

		    return TYPE_MODE (type);
		  }
		else if (((size == 8 && TARGET_64BIT) || size == 16)
			 && !TARGET_SSE
			 && !TARGET_IAMCU)
		  {
		    static bool warnedsse;
		    static bool warnedsse_ret;

		    if (cum && cum->warn_sse && !warnedsse)
		      {
			if (warning (OPT_Wpsabi, "SSE vector argument "
				     "without SSE enabled changes the ABI"))
			  warnedsse = true;
		      }
		    else if (!TARGET_64BIT && in_return && !warnedsse_ret)
		      {
			if (warning (OPT_Wpsabi, "SSE vector return "
				     "without SSE enabled changes the ABI"))
			  warnedsse_ret = true;
		      }
		  }
		else if ((size == 8 && !TARGET_64BIT)
			 && (!cfun
			     || cfun->machine->func_type == TYPE_NORMAL)
			 && !TARGET_MMX
			 && !TARGET_IAMCU)
		  {
		    static bool warnedmmx;
		    static bool warnedmmx_ret;

		    if (cum && cum->warn_mmx && !warnedmmx)
		      {
			if (warning (OPT_Wpsabi, "MMX vector argument "
				     "without MMX enabled changes the ABI"))
			  warnedmmx = true;
		      }
		    else if (in_return && !warnedmmx_ret)
		      {
			if (warning (OPT_Wpsabi, "MMX vector return "
				     "without MMX enabled changes the ABI"))
			  warnedmmx_ret = true;
		      }
		  }
		return mode;
	      }

	  gcc_unreachable ();
	}
    }

  return mode;
}

/* We want to pass a value in REGNO whose "natural" mode is MODE.  However,
   this may not agree with the mode that the type system has chosen for the
   register, which is ORIG_MODE.  If ORIG_MODE is not BLKmode, then we can
   go ahead and use it.  Otherwise we have to build a PARALLEL instead.  */

static rtx
gen_reg_or_parallel (machine_mode mode, machine_mode orig_mode,
		     unsigned int regno)
{
  rtx tmp;

  if (orig_mode != BLKmode)
    tmp = gen_rtx_REG (orig_mode, regno);
  else
    {
      tmp = gen_rtx_REG (mode, regno);
      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, const0_rtx);
      tmp = gen_rtx_PARALLEL (orig_mode, gen_rtvec (1, tmp));
    }

  return tmp;
}

/* x86-64 register passing implementation.  See x86-64 ABI for details.  Goal
   of this code is to classify each 8bytes of incoming argument by the register
   class and assign registers accordingly.  */

/* Return the union class of CLASS1 and CLASS2.
   See the x86-64 PS ABI for details.  */

static enum x86_64_reg_class
merge_classes (enum x86_64_reg_class class1, enum x86_64_reg_class class2)
{
  /* Rule #1: If both classes are equal, this is the resulting class.  */
  if (class1 == class2)
    return class1;

  /* Rule #2: If one of the classes is NO_CLASS, the resulting class is
     the other class.  */
  if (class1 == X86_64_NO_CLASS)
    return class2;
  if (class2 == X86_64_NO_CLASS)
    return class1;

  /* Rule #3: If one of the classes is MEMORY, the result is MEMORY.  */
  if (class1 == X86_64_MEMORY_CLASS || class2 == X86_64_MEMORY_CLASS)
    return X86_64_MEMORY_CLASS;

  /* Rule #4: If one of the classes is INTEGER, the result is INTEGER.  */
  if ((class1 == X86_64_INTEGERSI_CLASS
       && (class2 == X86_64_SSESF_CLASS || class2 == X86_64_SSEHF_CLASS))
      || (class2 == X86_64_INTEGERSI_CLASS
	  && (class1 == X86_64_SSESF_CLASS || class1 == X86_64_SSEHF_CLASS)))
    return X86_64_INTEGERSI_CLASS;
  if (class1 == X86_64_INTEGER_CLASS || class1 == X86_64_INTEGERSI_CLASS
      || class2 == X86_64_INTEGER_CLASS || class2 == X86_64_INTEGERSI_CLASS)
    return X86_64_INTEGER_CLASS;

  /* Rule #5: If one of the classes is X87, X87UP, or COMPLEX_X87 class,
     MEMORY is used.  */
  if (class1 == X86_64_X87_CLASS
      || class1 == X86_64_X87UP_CLASS
      || class1 == X86_64_COMPLEX_X87_CLASS
      || class2 == X86_64_X87_CLASS
      || class2 == X86_64_X87UP_CLASS
      || class2 == X86_64_COMPLEX_X87_CLASS)
    return X86_64_MEMORY_CLASS;

  /* Rule #6: Otherwise class SSE is used.  */
  return X86_64_SSE_CLASS;
}

/* Classify the argument of type TYPE and mode MODE.
   CLASSES will be filled by the register class used to pass each word
   of the operand.  The number of words is returned.  In case the parameter
   should be passed in memory, 0 is returned. As a special case for zero
   sized containers, classes[0] will be NO_CLASS and 1 is returned.

   BIT_OFFSET is used internally for handling records and specifies offset
   of the offset in bits modulo 512 to avoid overflow cases.

   See the x86-64 PS ABI for details.
*/

static int
classify_argument (machine_mode mode, const_tree type,
		   enum x86_64_reg_class classes[MAX_CLASSES], int bit_offset,
		   int &zero_width_bitfields)
{
  HOST_WIDE_INT bytes
    = mode == BLKmode ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  int words = CEIL (bytes + (bit_offset % 64) / 8, UNITS_PER_WORD);

  /* Variable sized entities are always passed/returned in memory.  */
  if (bytes < 0)
    return 0;

  if (mode != VOIDmode)
    {
      /* The value of "named" doesn't matter.  */
      function_arg_info arg (const_cast<tree> (type), mode, /*named=*/true);
      if (targetm.calls.must_pass_in_stack (arg))
	return 0;
    }

  if (type && (AGGREGATE_TYPE_P (type)
	       || (TREE_CODE (type) == BITINT_TYPE && words > 1)))
    {
      int i;
      tree field;
      enum x86_64_reg_class subclasses[MAX_CLASSES];

      /* On x86-64 we pass structures larger than 64 bytes on the stack.  */
      if (bytes > 64)
	return 0;

      for (i = 0; i < words; i++)
	classes[i] = X86_64_NO_CLASS;

      /* Zero sized arrays or structures are NO_CLASS.  We return 0 to
	 signalize memory class, so handle it as special case.  */
      if (!words)
	{
	  classes[0] = X86_64_NO_CLASS;
	  return 1;
	}

      /* Classify each field of record and merge classes.  */
      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	  /* And now merge the fields of structure.  */
	  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;

		  if (TREE_TYPE (field) == error_mark_node)
		    continue;

		  /* Bitfields are always classified as integer.  Handle them
		     early, since later code would consider them to be
		     misaligned integers.  */
		  if (DECL_BIT_FIELD (field))
		    {
		      if (integer_zerop (DECL_SIZE (field)))
			{
			  if (DECL_FIELD_CXX_ZERO_WIDTH_BIT_FIELD (field))
			    continue;
			  if (zero_width_bitfields != 2)
			    {
			      zero_width_bitfields = 1;
			      continue;
			    }
			}
		      for (i = (int_bit_position (field)
				+ (bit_offset % 64)) / 8 / 8;
			   i < ((int_bit_position (field) + (bit_offset % 64))
			        + tree_to_shwi (DECL_SIZE (field))
				+ 63) / 8 / 8; i++)
			classes[i]
			  = merge_classes (X86_64_INTEGER_CLASS, classes[i]);
		    }
		  else
		    {
		      int pos;

		      type = TREE_TYPE (field);

		      /* Flexible array member is ignored.  */
		      if (TYPE_MODE (type) == BLKmode
			  && TREE_CODE (type) == ARRAY_TYPE
			  && TYPE_SIZE (type) == NULL_TREE
			  && TYPE_DOMAIN (type) != NULL_TREE
			  && (TYPE_MAX_VALUE (TYPE_DOMAIN (type))
			      == NULL_TREE))
			{
			  static bool warned;

			  if (!warned && warn_psabi)
			    {
			      warned = true;
			      inform (input_location,
				      "the ABI of passing struct with"
				      " a flexible array member has"
				      " changed in GCC 4.4");
			    }
			  continue;
			}
		      num = classify_argument (TYPE_MODE (type), type,
					       subclasses,
					       (int_bit_position (field)
						+ bit_offset) % 512,
					       zero_width_bitfields);
		      if (!num)
			return 0;
		      pos = (int_bit_position (field)
			     + (bit_offset % 64)) / 8 / 8;
		      for (i = 0; i < num && (i + pos) < words; i++)
			classes[i + pos]
			  = merge_classes (subclasses[i], classes[i + pos]);
		    }
		}
	    }
	  break;

	case ARRAY_TYPE:
	  /* Arrays are handled as small records.  */
	  {
	    int num;
	    num = classify_argument (TYPE_MODE (TREE_TYPE (type)),
				     TREE_TYPE (type), subclasses, bit_offset,
				     zero_width_bitfields);
	    if (!num)
	      return 0;

	    /* The partial classes are now full classes.  */
	    if (subclasses[0] == X86_64_SSESF_CLASS && bytes != 4)
	      subclasses[0] = X86_64_SSE_CLASS;
	    if (subclasses[0] == X86_64_SSEHF_CLASS && bytes != 2)
	      subclasses[0] = X86_64_SSE_CLASS;
	    if (subclasses[0] == X86_64_INTEGERSI_CLASS
		&& !((bit_offset % 64) == 0 && bytes == 4))
	      subclasses[0] = X86_64_INTEGER_CLASS;

	    for (i = 0; i < words; i++)
	      classes[i] = subclasses[i % num];

	    break;
	  }
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  /* Unions are similar to RECORD_TYPE but offset is always 0.
	     */
	  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;

		  if (TREE_TYPE (field) == error_mark_node)
		    continue;

		  num = classify_argument (TYPE_MODE (TREE_TYPE (field)),
					   TREE_TYPE (field), subclasses,
					   bit_offset, zero_width_bitfields);
		  if (!num)
		    return 0;
		  for (i = 0; i < num && i < words; i++)
		    classes[i] = merge_classes (subclasses[i], classes[i]);
		}
	    }
	  break;

	case BITINT_TYPE:
	  /* _BitInt(N) for N > 64 is passed as structure containing
	     (N + 63) / 64 64-bit elements.  */
	  if (words > 2)
	    return 0;
	  classes[0] = classes[1] = X86_64_INTEGER_CLASS;
	  return 2;

	default:
	  gcc_unreachable ();
	}

      if (words > 2)
	{
	  /* When size > 16 bytes, if the first one isn't
	     X86_64_SSE_CLASS or any other ones aren't
	     X86_64_SSEUP_CLASS, everything should be passed in
	     memory.  */
	  if (classes[0] != X86_64_SSE_CLASS)
	    return 0;

	  for (i = 1; i < words; i++)
	    if (classes[i] != X86_64_SSEUP_CLASS)
	      return 0;
	}

      /* Final merger cleanup.  */
      for (i = 0; i < words; i++)
	{
	  /* If one class is MEMORY, everything should be passed in
	     memory.  */
	  if (classes[i] == X86_64_MEMORY_CLASS)
	    return 0;

	  /* The X86_64_SSEUP_CLASS should be always preceded by
	     X86_64_SSE_CLASS or X86_64_SSEUP_CLASS.  */
	  if (classes[i] == X86_64_SSEUP_CLASS
	      && classes[i - 1] != X86_64_SSE_CLASS
	      && classes[i - 1] != X86_64_SSEUP_CLASS)
	    {
	      /* The first one should never be X86_64_SSEUP_CLASS.  */
	      gcc_assert (i != 0);
	      classes[i] = X86_64_SSE_CLASS;
	    }

	  /* If X86_64_X87UP_CLASS isn't preceded by X86_64_X87_CLASS,
	     everything should be passed in memory.  */
	  if (classes[i] == X86_64_X87UP_CLASS
	      && (classes[i - 1] != X86_64_X87_CLASS))
	    {
	      static bool warned;

	      /* The first one should never be X86_64_X87UP_CLASS.  */
	      gcc_assert (i != 0);
	      if (!warned && warn_psabi)
		{
		  warned = true;
		  inform (input_location,
			  "the ABI of passing union with %<long double%>"
			  " has changed in GCC 4.4");
		}
	      return 0;
	    }
	}
      return words;
    }

  /* Compute alignment needed.  We align all types to natural boundaries with
     exception of XFmode that is aligned to 64bits.  */
  if (mode != VOIDmode && mode != BLKmode)
    {
      int mode_alignment = GET_MODE_BITSIZE (mode);

      if (mode == XFmode)
	mode_alignment = 128;
      else if (mode == XCmode)
	mode_alignment = 256;
      if (COMPLEX_MODE_P (mode))
	mode_alignment /= 2;
      /* Misaligned fields are always returned in memory.  */
      if (bit_offset % mode_alignment)
	return 0;
    }

  /* for V1xx modes, just use the base mode */
  if (VECTOR_MODE_P (mode) && mode != V1DImode && mode != V1TImode
      && GET_MODE_UNIT_SIZE (mode) == bytes)
    mode = GET_MODE_INNER (mode);

  /* Classification of atomic types.  */
  switch (mode)
    {
    case E_SDmode:
    case E_DDmode:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case E_TDmode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case E_DImode:
    case E_SImode:
    case E_HImode:
    case E_QImode:
    case E_CSImode:
    case E_CHImode:
    case E_CQImode:
      {
	int size = bit_offset + (int) GET_MODE_BITSIZE (mode);

	/* Analyze last 128 bits only.  */
	size = (size - 1) & 0x7f;

	if (size < 32)
	  {
	    classes[0] = X86_64_INTEGERSI_CLASS;
	    return 1;
	  }
	else if (size < 64)
	  {
	    classes[0] = X86_64_INTEGER_CLASS;
	    return 1;
	  }
	else if (size < 64+32)
	  {
	    classes[0] = X86_64_INTEGER_CLASS;
	    classes[1] = X86_64_INTEGERSI_CLASS;
	    return 2;
	  }
	else if (size < 64+64)
	  {
	    classes[0] = classes[1] = X86_64_INTEGER_CLASS;
	    return 2;
	  }
	else
	  gcc_unreachable ();
      }
    case E_CDImode:
    case E_TImode:
      classes[0] = classes[1] = X86_64_INTEGER_CLASS;
      return 2;
    case E_COImode:
    case E_OImode:
      /* OImode shouldn't be used directly.  */
      gcc_unreachable ();
    case E_CTImode:
      return 0;
    case E_HFmode:
    case E_BFmode:
      if (!(bit_offset % 64))
	classes[0] = X86_64_SSEHF_CLASS;
      else
	classes[0] = X86_64_SSE_CLASS;
      return 1;
    case E_SFmode:
      if (!(bit_offset % 64))
	classes[0] = X86_64_SSESF_CLASS;
      else
	classes[0] = X86_64_SSE_CLASS;
      return 1;
    case E_DFmode:
      classes[0] = X86_64_SSEDF_CLASS;
      return 1;
    case E_XFmode:
      classes[0] = X86_64_X87_CLASS;
      classes[1] = X86_64_X87UP_CLASS;
      return 2;
    case E_TFmode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case E_HCmode:
    case E_BCmode:
      classes[0] = X86_64_SSE_CLASS;
      if (!(bit_offset % 64))
	return 1;
      else
	{
	  classes[1] = X86_64_SSEHF_CLASS;
	  return 2;
	}
    case E_SCmode:
      classes[0] = X86_64_SSE_CLASS;
      if (!(bit_offset % 64))
	return 1;
      else
	{
	  static bool warned;

	  if (!warned && warn_psabi)
	    {
	      warned = true;
	      inform (input_location,
		      "the ABI of passing structure with %<complex float%>"
		      " member has changed in GCC 4.4");
	    }
	  classes[1] = X86_64_SSESF_CLASS;
	  return 2;
	}
    case E_DCmode:
      classes[0] = X86_64_SSEDF_CLASS;
      classes[1] = X86_64_SSEDF_CLASS;
      return 2;
    case E_XCmode:
      classes[0] = X86_64_COMPLEX_X87_CLASS;
      return 1;
    case E_TCmode:
      /* This modes is larger than 16 bytes.  */
      return 0;
    case E_V8SFmode:
    case E_V8SImode:
    case E_V32QImode:
    case E_V16HFmode:
    case E_V16BFmode:
    case E_V16HImode:
    case E_V4DFmode:
    case E_V4DImode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      classes[2] = X86_64_SSEUP_CLASS;
      classes[3] = X86_64_SSEUP_CLASS;
      return 4;
    case E_V8DFmode:
    case E_V16SFmode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V8DImode:
    case E_V16SImode:
    case E_V32HImode:
    case E_V64QImode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      classes[2] = X86_64_SSEUP_CLASS;
      classes[3] = X86_64_SSEUP_CLASS;
      classes[4] = X86_64_SSEUP_CLASS;
      classes[5] = X86_64_SSEUP_CLASS;
      classes[6] = X86_64_SSEUP_CLASS;
      classes[7] = X86_64_SSEUP_CLASS;
      return 8;
    case E_V4SFmode:
    case E_V4SImode:
    case E_V16QImode:
    case E_V8HImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V2DFmode:
    case E_V2DImode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case E_V1TImode:
    case E_V1DImode:
    case E_V2SFmode:
    case E_V2SImode:
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2HFmode:
    case E_V2BFmode:
    case E_V8QImode:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case E_BLKmode:
    case E_VOIDmode:
      return 0;
    default:
      gcc_assert (VECTOR_MODE_P (mode));

      if (bytes > 16)
	return 0;

      gcc_assert (GET_MODE_CLASS (GET_MODE_INNER (mode)) == MODE_INT);

      if (bit_offset + GET_MODE_BITSIZE (mode) <= 32)
	classes[0] = X86_64_INTEGERSI_CLASS;
      else
	classes[0] = X86_64_INTEGER_CLASS;
      classes[1] = X86_64_INTEGER_CLASS;
      return 1 + (bytes > 8);
    }
}

/* Wrapper around classify_argument with the extra zero_width_bitfields
   argument, to diagnose GCC 12.1 ABI differences for C.  */

static int
classify_argument (machine_mode mode, const_tree type,
		   enum x86_64_reg_class classes[MAX_CLASSES], int bit_offset)
{
  int zero_width_bitfields = 0;
  static bool warned = false;
  int n = classify_argument (mode, type, classes, bit_offset,
			     zero_width_bitfields);
  if (!zero_width_bitfields || warned || !warn_psabi)
    return n;
  enum x86_64_reg_class alt_classes[MAX_CLASSES];
  zero_width_bitfields = 2;
  if (classify_argument (mode, type, alt_classes, bit_offset,
			 zero_width_bitfields) != n)
    zero_width_bitfields = 3;
  else
    for (int i = 0; i < n; i++)
      if (classes[i] != alt_classes[i])
	{
	  zero_width_bitfields = 3;
	  break;
	}
  if (zero_width_bitfields == 3)
    {
      warned = true;
      const char *url
	= CHANGES_ROOT_URL "gcc-12/changes.html#zero_width_bitfields";

      inform (input_location,
	      "the ABI of passing C structures with zero-width bit-fields"
	      " has changed in GCC %{12.1%}", url);
    }
  return n;
}

/* Examine the argument and return set number of register required in each
   class.  Return true iff parameter should be passed in memory.  */

static bool
examine_argument (machine_mode mode, const_tree type, int in_return,
		  int *int_nregs, int *sse_nregs)
{
  enum x86_64_reg_class regclass[MAX_CLASSES];
  int n = classify_argument (mode, type, regclass, 0);

  *int_nregs = 0;
  *sse_nregs = 0;

  if (!n)
    return true;
  for (n--; n >= 0; n--)
    switch (regclass[n])
      {
      case X86_64_INTEGER_CLASS:
      case X86_64_INTEGERSI_CLASS:
	(*int_nregs)++;
	break;
      case X86_64_SSE_CLASS:
      case X86_64_SSEHF_CLASS:
      case X86_64_SSESF_CLASS:
      case X86_64_SSEDF_CLASS:
	(*sse_nregs)++;
	break;
      case X86_64_NO_CLASS:
      case X86_64_SSEUP_CLASS:
	break;
      case X86_64_X87_CLASS:
      case X86_64_X87UP_CLASS:
      case X86_64_COMPLEX_X87_CLASS:
	if (!in_return)
	  return true;
	break;
      case X86_64_MEMORY_CLASS:
	gcc_unreachable ();
      }

  return false;
}

/* Construct container for the argument used by GCC interface.  See
   FUNCTION_ARG for the detailed description.  */

static rtx
construct_container (machine_mode mode, machine_mode orig_mode,
		     const_tree type, int in_return, int nintregs, int nsseregs,
		     const int *intreg, int sse_regno)
{
  /* The following variables hold the static issued_error state.  */
  static bool issued_sse_arg_error;
  static bool issued_sse_ret_error;
  static bool issued_x87_ret_error;

  machine_mode tmpmode;
  int bytes
    = mode == BLKmode ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  enum x86_64_reg_class regclass[MAX_CLASSES];
  int n;
  int i;
  int nexps = 0;
  int needed_sseregs, needed_intregs;
  rtx exp[MAX_CLASSES];
  rtx ret;

  n = classify_argument (mode, type, regclass, 0);
  if (!n)
    return NULL;
  if (examine_argument (mode, type, in_return, &needed_intregs,
			&needed_sseregs))
    return NULL;
  if (needed_intregs > nintregs || needed_sseregs > nsseregs)
    return NULL;

  /* We allowed the user to turn off SSE for kernel mode.  Don't crash if
     some less clueful developer tries to use floating-point anyway.  */
  if (needed_sseregs
      && (!TARGET_SSE || (VALID_SSE2_TYPE_MODE (mode) && !TARGET_SSE2)))
    {
      /* Return early if we shouldn't raise an error for invalid
	 calls.  */
      if (cfun != NULL && cfun->machine->silent_p)
	return NULL;
      if (in_return)
	{
	  if (!issued_sse_ret_error)
	    {
	      if (VALID_SSE2_TYPE_MODE (mode))
		error ("SSE register return with SSE2 disabled");
	      else
		error ("SSE register return with SSE disabled");
	      issued_sse_ret_error = true;
	    }
	}
      else if (!issued_sse_arg_error)
	{
	  if (VALID_SSE2_TYPE_MODE (mode))
	    error ("SSE register argument with SSE2 disabled");
	  else
	    error ("SSE register argument with SSE disabled");
	  issued_sse_arg_error = true;
	}
      return NULL;
    }

  /* Likewise, error if the ABI requires us to return values in the
     x87 registers and the user specified -mno-80387.  */
  if (!TARGET_FLOAT_RETURNS_IN_80387 && in_return)
    for (i = 0; i < n; i++)
      if (regclass[i] == X86_64_X87_CLASS
	  || regclass[i] == X86_64_X87UP_CLASS
	  || regclass[i] == X86_64_COMPLEX_X87_CLASS)
	{
	  /* Return early if we shouldn't raise an error for invalid
	     calls.  */
	  if (cfun != NULL && cfun->machine->silent_p)
	    return NULL;
	  if (!issued_x87_ret_error)
	    {
	      error ("x87 register return with x87 disabled");
	      issued_x87_ret_error = true;
	    }
	  return NULL;
	}

  /* First construct simple cases.  Avoid SCmode, since we want to use
     single register to pass this type.  */
  if (n == 1 && mode != SCmode && mode != HCmode)
    switch (regclass[0])
      {
      case X86_64_INTEGER_CLASS:
      case X86_64_INTEGERSI_CLASS:
	return gen_rtx_REG (mode, intreg[0]);
      case X86_64_SSE_CLASS:
      case X86_64_SSEHF_CLASS:
      case X86_64_SSESF_CLASS:
      case X86_64_SSEDF_CLASS:
	if (mode != BLKmode)
	  return gen_reg_or_parallel (mode, orig_mode,
				      GET_SSE_REGNO (sse_regno));
	break;
      case X86_64_X87_CLASS:
      case X86_64_COMPLEX_X87_CLASS:
	return gen_rtx_REG (mode, FIRST_STACK_REG);
      case X86_64_NO_CLASS:
	/* Zero sized array, struct or class.  */
	return NULL;
      default:
	gcc_unreachable ();
      }
  if (n == 2
      && regclass[0] == X86_64_SSE_CLASS
      && regclass[1] == X86_64_SSEUP_CLASS
      && mode != BLKmode)
    return gen_reg_or_parallel (mode, orig_mode,
				GET_SSE_REGNO (sse_regno));
  if (n == 4
      && regclass[0] == X86_64_SSE_CLASS
      && regclass[1] == X86_64_SSEUP_CLASS
      && regclass[2] == X86_64_SSEUP_CLASS
      && regclass[3] == X86_64_SSEUP_CLASS
      && mode != BLKmode)
    return gen_reg_or_parallel (mode, orig_mode,
				GET_SSE_REGNO (sse_regno));
  if (n == 8
      && regclass[0] == X86_64_SSE_CLASS
      && regclass[1] == X86_64_SSEUP_CLASS
      && regclass[2] == X86_64_SSEUP_CLASS
      && regclass[3] == X86_64_SSEUP_CLASS
      && regclass[4] == X86_64_SSEUP_CLASS
      && regclass[5] == X86_64_SSEUP_CLASS
      && regclass[6] == X86_64_SSEUP_CLASS
      && regclass[7] == X86_64_SSEUP_CLASS
      && mode != BLKmode)
    return gen_reg_or_parallel (mode, orig_mode,
				GET_SSE_REGNO (sse_regno));
  if (n == 2
      && regclass[0] == X86_64_X87_CLASS
      && regclass[1] == X86_64_X87UP_CLASS)
    return gen_rtx_REG (XFmode, FIRST_STACK_REG);

  if (n == 2
      && regclass[0] == X86_64_INTEGER_CLASS
      && regclass[1] == X86_64_INTEGER_CLASS
      && (mode == CDImode || mode == TImode || mode == BLKmode)
      && intreg[0] + 1 == intreg[1])
    {
      if (mode == BLKmode)
	{
	  /* Use TImode for BLKmode values in 2 integer registers.  */
	  exp[0] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (TImode, intreg[0]),
				      GEN_INT (0));
	  ret = gen_rtx_PARALLEL (mode, rtvec_alloc (1));
	  XVECEXP (ret, 0, 0) = exp[0];
	  return ret;
	}
      else
	return gen_rtx_REG (mode, intreg[0]);
    }

  /* Otherwise figure out the entries of the PARALLEL.  */
  for (i = 0; i < n; i++)
    {
      int pos;

      switch (regclass[i])
        {
	  case X86_64_NO_CLASS:
	    break;
	  case X86_64_INTEGER_CLASS:
	  case X86_64_INTEGERSI_CLASS:
	    /* Merge TImodes on aligned occasions here too.  */
	    if (i * 8 + 8 > bytes)
	      {
		unsigned int tmpbits = (bytes - i * 8) * BITS_PER_UNIT;
		if (!int_mode_for_size (tmpbits, 0).exists (&tmpmode))
		  /* We've requested 24 bytes we
		     don't have mode for.  Use DImode.  */
		  tmpmode = DImode;
	      }
	    else if (regclass[i] == X86_64_INTEGERSI_CLASS)
	      tmpmode = SImode;
	    else
	      tmpmode = DImode;
	    exp [nexps++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (tmpmode, *intreg),
				   GEN_INT (i*8));
	    intreg++;
	    break;
	  case X86_64_SSEHF_CLASS:
	    tmpmode = (mode == BFmode ? BFmode : HFmode);
	    exp [nexps++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (tmpmode,
						GET_SSE_REGNO (sse_regno)),
				   GEN_INT (i*8));
	    sse_regno++;
	    break;
	  case X86_64_SSESF_CLASS:
	    exp [nexps++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (SFmode,
						GET_SSE_REGNO (sse_regno)),
				   GEN_INT (i*8));
	    sse_regno++;
	    break;
	  case X86_64_SSEDF_CLASS:
	    exp [nexps++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (DFmode,
						GET_SSE_REGNO (sse_regno)),
				   GEN_INT (i*8));
	    sse_regno++;
	    break;
	  case X86_64_SSE_CLASS:
	    pos = i;
	    switch (n)
	      {
	      case 1:
		tmpmode = DImode;
		break;
	      case 2:
		if (i == 0 && regclass[1] == X86_64_SSEUP_CLASS)
		  {
		    tmpmode = TImode;
		    i++;
		  }
		else
		  tmpmode = DImode;
		break;
	      case 4:
		gcc_assert (i == 0
			    && regclass[1] == X86_64_SSEUP_CLASS
			    && regclass[2] == X86_64_SSEUP_CLASS
			    && regclass[3] == X86_64_SSEUP_CLASS);
		tmpmode = OImode;
		i += 3;
		break;
	      case 8:
		gcc_assert (i == 0
			    && regclass[1] == X86_64_SSEUP_CLASS
			    && regclass[2] == X86_64_SSEUP_CLASS
			    && regclass[3] == X86_64_SSEUP_CLASS
			    && regclass[4] == X86_64_SSEUP_CLASS
			    && regclass[5] == X86_64_SSEUP_CLASS
			    && regclass[6] == X86_64_SSEUP_CLASS
			    && regclass[7] == X86_64_SSEUP_CLASS);
		tmpmode = XImode;
		i += 7;
		break;
	      default:
		gcc_unreachable ();
	      }
	    exp [nexps++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (tmpmode,
						GET_SSE_REGNO (sse_regno)),
				   GEN_INT (pos*8));
	    sse_regno++;
	    break;
	  default:
	    gcc_unreachable ();
	}
    }

  /* Empty aligned struct, union or class.  */
  if (nexps == 0)
    return NULL;

  ret =  gen_rtx_PARALLEL (mode, rtvec_alloc (nexps));
  for (i = 0; i < nexps; i++)
    XVECEXP (ret, 0, i) = exp [i];
  return ret;
}

/* Update the data in CUM to advance over an argument of mode MODE
   and data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.)

   Return a number of integer regsiters advanced over.  */

static int
function_arg_advance_32 (CUMULATIVE_ARGS *cum, machine_mode mode,
			 const_tree type, HOST_WIDE_INT bytes,
			 HOST_WIDE_INT words)
{
  int res = 0;
  bool error_p = false;

  if (TARGET_IAMCU)
    {
      /* Intel MCU psABI passes scalars and aggregates no larger than 8
	 bytes in registers.  */
      if (!VECTOR_MODE_P (mode) && bytes <= 8)
	goto pass_in_reg;
      return res;
    }

  switch (mode)
    {
    default:
      break;

    case E_BLKmode:
      if (bytes < 0)
	break;
      /* FALLTHRU */

    case E_DImode:
    case E_SImode:
    case E_HImode:
    case E_QImode:
pass_in_reg:
      cum->words += words;
      cum->nregs -= words;
      cum->regno += words;
      if (cum->nregs >= 0)
	res = words;
      if (cum->nregs <= 0)
	{
	  cum->nregs = 0;
	  cfun->machine->arg_reg_available = false;
	  cum->regno = 0;
	}
      break;

    case E_OImode:
      /* OImode shouldn't be used directly.  */
      gcc_unreachable ();

    case E_DFmode:
      if (cum->float_in_sse == -1)
	error_p = true;
      if (cum->float_in_sse < 2)
	break;
      /* FALLTHRU */
    case E_SFmode:
      if (cum->float_in_sse == -1)
	error_p = true;
      if (cum->float_in_sse < 1)
	break;
      /* FALLTHRU */

    case E_V16HFmode:
    case E_V16BFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V64QImode:
    case E_V32HImode:
    case E_V16SImode:
    case E_V8DImode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V16SFmode:
    case E_V8DFmode:
    case E_V32QImode:
    case E_V16HImode:
    case E_V4DFmode:
    case E_V4DImode:
    case E_TImode:
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V4SFmode:
    case E_V2DFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  cum->sse_words += words;
	  cum->sse_nregs -= 1;
	  cum->sse_regno += 1;
	  if (cum->sse_nregs <= 0)
	    {
	      cum->sse_nregs = 0;
	      cum->sse_regno = 0;
	    }
	}
      break;

    case E_V8QImode:
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2SImode:
    case E_V2SFmode:
    case E_V1TImode:
    case E_V1DImode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  cum->mmx_words += words;
	  cum->mmx_nregs -= 1;
	  cum->mmx_regno += 1;
	  if (cum->mmx_nregs <= 0)
	    {
	      cum->mmx_nregs = 0;
	      cum->mmx_regno = 0;
	    }
	}
      break;
    }
  if (error_p)
    {
      cum->float_in_sse = 0;
      error ("calling %qD with SSE calling convention without "
	     "SSE/SSE2 enabled", cum->decl);
      sorry ("this is a GCC bug that can be worked around by adding "
	     "attribute used to function called");
    }

  return res;
}

static int
function_arg_advance_64 (CUMULATIVE_ARGS *cum, machine_mode mode,
			 const_tree type, HOST_WIDE_INT words, bool named)
{
  int int_nregs, sse_nregs;

  /* Unnamed 512 and 256bit vector mode parameters are passed on stack.  */
  if (!named && (VALID_AVX512F_REG_MODE (mode)
		 || VALID_AVX256_REG_MODE (mode)))
    return 0;

  if (!examine_argument (mode, type, 0, &int_nregs, &sse_nregs)
      && sse_nregs <= cum->sse_nregs && int_nregs <= cum->nregs)
    {
      cum->nregs -= int_nregs;
      cum->sse_nregs -= sse_nregs;
      cum->regno += int_nregs;
      cum->sse_regno += sse_nregs;
      return int_nregs;
    }
  else
    {
      int align = ix86_function_arg_boundary (mode, type) / BITS_PER_WORD;
      cum->words = ROUND_UP (cum->words, align);
      cum->words += words;
      return 0;
    }
}

static int
function_arg_advance_ms_64 (CUMULATIVE_ARGS *cum, HOST_WIDE_INT bytes,
			    HOST_WIDE_INT words)
{
  /* Otherwise, this should be passed indirect.  */
  gcc_assert (bytes == 1 || bytes == 2 || bytes == 4 || bytes == 8);

  cum->words += words;
  if (cum->nregs > 0)
    {
      cum->nregs -= 1;
      cum->regno += 1;
      return 1;
    }
  return 0;
}

/* Update the data in CUM to advance over argument ARG.  */

static void
ix86_function_arg_advance (cumulative_args_t cum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  machine_mode mode = arg.mode;
  HOST_WIDE_INT bytes, words;
  int nregs;

  /* The argument of interrupt handler is a special case and is
     handled in ix86_function_arg.  */
  if (!cum->caller && cfun->machine->func_type != TYPE_NORMAL)
    return;

  bytes = arg.promoted_size_in_bytes ();
  words = CEIL (bytes, UNITS_PER_WORD);

  if (arg.type)
    mode = type_natural_mode (arg.type, NULL, false);

  if (TARGET_64BIT)
    {
      enum calling_abi call_abi = cum ? cum->call_abi : ix86_abi;

      if (call_abi == MS_ABI)
	nregs = function_arg_advance_ms_64 (cum, bytes, words);
      else
	nregs = function_arg_advance_64 (cum, mode, arg.type, words,
					 arg.named);
    }
  else
    nregs = function_arg_advance_32 (cum, mode, arg.type, bytes, words);

  if (!nregs)
    {
      /* Track if there are outgoing arguments on stack.  */
      if (cum->caller)
	cfun->machine->outgoing_args_on_stack = true;
    }
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

static rtx
function_arg_32 (CUMULATIVE_ARGS *cum, machine_mode mode,
		 machine_mode orig_mode, const_tree type,
		 HOST_WIDE_INT bytes, HOST_WIDE_INT words)
{
  bool error_p = false;

  /* Avoid the AL settings for the Unix64 ABI.  */
  if (mode == VOIDmode)
    return constm1_rtx;

  if (TARGET_IAMCU)
    {
      /* Intel MCU psABI passes scalars and aggregates no larger than 8
	 bytes in registers.  */
      if (!VECTOR_MODE_P (mode) && bytes <= 8)
	goto pass_in_reg;
      return NULL_RTX;
    }

  switch (mode)
    {
    default:
      break;

    case E_BLKmode:
      if (bytes < 0)
	break;
      /* FALLTHRU */
    case E_DImode:
    case E_SImode:
    case E_HImode:
    case E_QImode:
pass_in_reg:
      if (words <= cum->nregs)
	{
	  int regno = cum->regno;

	  /* Fastcall allocates the first two DWORD (SImode) or
            smaller arguments to ECX and EDX if it isn't an
            aggregate type .  */
	  if (cum->fastcall)
	    {
	      if (mode == BLKmode
		  || mode == DImode
		  || (type && AGGREGATE_TYPE_P (type)))
	        break;

	      /* ECX not EAX is the first allocated register.  */
	      if (regno == AX_REG)
		regno = CX_REG;
	    }
	  return gen_rtx_REG (mode, regno);
	}
      break;

    case E_DFmode:
      if (cum->float_in_sse == -1)
	error_p = true;
      if (cum->float_in_sse < 2)
	break;
      /* FALLTHRU */
    case E_SFmode:
      if (cum->float_in_sse == -1)
	error_p = true;
      if (cum->float_in_sse < 1)
	break;
      /* FALLTHRU */
    case E_TImode:
      /* In 32bit, we pass TImode in xmm registers.  */
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V4SFmode:
    case E_V2DFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  if (cum->sse_nregs)
	    return gen_reg_or_parallel (mode, orig_mode,
				        cum->sse_regno + FIRST_SSE_REG);
	}
      break;

    case E_OImode:
    case E_XImode:
      /* OImode and XImode shouldn't be used directly.  */
      gcc_unreachable ();

    case E_V64QImode:
    case E_V32HImode:
    case E_V16SImode:
    case E_V8DImode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V16SFmode:
    case E_V8DFmode:
    case E_V16HFmode:
    case E_V16BFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V32QImode:
    case E_V16HImode:
    case E_V4DFmode:
    case E_V4DImode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  if (cum->sse_nregs)
	    return gen_reg_or_parallel (mode, orig_mode,
				        cum->sse_regno + FIRST_SSE_REG);
	}
      break;

    case E_V8QImode:
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2SImode:
    case E_V2SFmode:
    case E_V1TImode:
    case E_V1DImode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  if (cum->mmx_nregs)
	    return gen_reg_or_parallel (mode, orig_mode,
				        cum->mmx_regno + FIRST_MMX_REG);
	}
      break;
    }
  if (error_p)
    {
      cum->float_in_sse = 0;
      error ("calling %qD with SSE calling convention without "
	     "SSE/SSE2 enabled", cum->decl);
      sorry ("this is a GCC bug that can be worked around by adding "
	     "attribute used to function called");
    }

  return NULL_RTX;
}

static rtx
function_arg_64 (const CUMULATIVE_ARGS *cum, machine_mode mode,
		 machine_mode orig_mode, const_tree type, bool named)
{
  /* Handle a hidden AL argument containing number of registers
     for varargs x86-64 functions.  */
  if (mode == VOIDmode)
    return GEN_INT (cum->maybe_vaarg
		    ? (cum->sse_nregs < 0
		       ? X86_64_SSE_REGPARM_MAX
		       : cum->sse_regno)
		    : -1);

  switch (mode)
    {
    default:
      break;

    case E_V16HFmode:
    case E_V16BFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V32QImode:
    case E_V16HImode:
    case E_V4DFmode:
    case E_V4DImode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V16SFmode:
    case E_V16SImode:
    case E_V64QImode:
    case E_V32HImode:
    case E_V8DFmode:
    case E_V8DImode:
      /* Unnamed 256 and 512bit vector mode parameters are passed on stack.  */
      if (!named)
	return NULL;
      break;
    }

  return construct_container (mode, orig_mode, type, 0, cum->nregs,
			      cum->sse_nregs,
			      &x86_64_int_parameter_registers [cum->regno],
			      cum->sse_regno);
}

static rtx
function_arg_ms_64 (const CUMULATIVE_ARGS *cum, machine_mode mode,
		    machine_mode orig_mode, bool named, const_tree type,
		    HOST_WIDE_INT bytes)
{
  unsigned int regno;

  /* We need to add clobber for MS_ABI->SYSV ABI calls in expand_call.
     We use value of -2 to specify that current function call is MSABI.  */
  if (mode == VOIDmode)
    return GEN_INT (-2);

  /* If we've run out of registers, it goes on the stack.  */
  if (cum->nregs == 0)
    return NULL_RTX;

  regno = x86_64_ms_abi_int_parameter_registers[cum->regno];

  /* Only floating point modes are passed in anything but integer regs.  */
  if (TARGET_SSE && (mode == SFmode || mode == DFmode))
    {
      if (named)
	{
	  if (type == NULL_TREE || !AGGREGATE_TYPE_P (type))
	    regno = cum->regno + FIRST_SSE_REG;
	}
      else
	{
	  rtx t1, t2;

	  /* Unnamed floating parameters are passed in both the
	     SSE and integer registers.  */
	  t1 = gen_rtx_REG (mode, cum->regno + FIRST_SSE_REG);
	  t2 = gen_rtx_REG (mode, regno);
	  t1 = gen_rtx_EXPR_LIST (VOIDmode, t1, const0_rtx);
	  t2 = gen_rtx_EXPR_LIST (VOIDmode, t2, const0_rtx);
	  return gen_rtx_PARALLEL (mode, gen_rtvec (2, t1, t2));
	}
    }
  /* Handle aggregated types passed in register.  */
  if (orig_mode == BLKmode)
    {
      if (bytes > 0 && bytes <= 8)
        mode = (bytes > 4 ? DImode : SImode);
      if (mode == BLKmode)
        mode = DImode;
    }

  return gen_reg_or_parallel (mode, orig_mode, regno);
}

/* Return where to put the arguments to a function.
   Return zero to push the argument on the stack, or a hard register in which to store the argument.

   ARG describes the argument while CUM gives information about the
   preceding args and about the function being called.  */

static rtx
ix86_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  machine_mode mode = arg.mode;
  HOST_WIDE_INT bytes, words;
  rtx reg;

  if (!cum->caller && cfun->machine->func_type != TYPE_NORMAL)
    {
      gcc_assert (arg.type != NULL_TREE);
      if (POINTER_TYPE_P (arg.type))
	{
	  /* This is the pointer argument.  */
	  gcc_assert (TYPE_MODE (arg.type) == ptr_mode);
	  /* It is at -WORD(AP) in the current frame in interrupt and
	     exception handlers.  */
	  reg = plus_constant (Pmode, arg_pointer_rtx, -UNITS_PER_WORD);
	}
      else
	{
	  gcc_assert (cfun->machine->func_type == TYPE_EXCEPTION
		      && TREE_CODE (arg.type) == INTEGER_TYPE
		      && TYPE_MODE (arg.type) == word_mode);
	  /* The error code is the word-mode integer argument at
	     -2 * WORD(AP) in the current frame of the exception
	     handler.  */
	  reg = gen_rtx_MEM (word_mode,
			     plus_constant (Pmode,
					    arg_pointer_rtx,
					    -2 * UNITS_PER_WORD));
	}
      return reg;
    }

  bytes = arg.promoted_size_in_bytes ();
  words = CEIL (bytes, UNITS_PER_WORD);

  /* To simplify the code below, represent vector types with a vector mode
     even if MMX/SSE are not active.  */
  if (arg.type && VECTOR_TYPE_P (arg.type))
    mode = type_natural_mode (arg.type, cum, false);

  if (TARGET_64BIT)
    {
      enum calling_abi call_abi = cum ? cum->call_abi : ix86_abi;

      if (call_abi == MS_ABI)
	reg = function_arg_ms_64 (cum, mode, arg.mode, arg.named,
				  arg.type, bytes);
      else
	reg = function_arg_64 (cum, mode, arg.mode, arg.type, arg.named);
    }
  else
    reg = function_arg_32 (cum, mode, arg.mode, arg.type, bytes, words);

  /* Track if there are outgoing arguments on stack.  */
  if (reg == NULL_RTX && cum->caller)
    cfun->machine->outgoing_args_on_stack = true;

  return reg;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */

static bool
ix86_pass_by_reference (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (TARGET_64BIT)
    {
      enum calling_abi call_abi = cum ? cum->call_abi : ix86_abi;

      /* See Windows x64 Software Convention.  */
      if (call_abi == MS_ABI)
	{
	  HOST_WIDE_INT msize = GET_MODE_SIZE (arg.mode);

	  if (tree type = arg.type)
	    {
	      /* Arrays are passed by reference.  */
	      if (TREE_CODE (type) == ARRAY_TYPE)
		return true;

	      if (RECORD_OR_UNION_TYPE_P (type))
		{
		  /* Structs/unions of sizes other than 8, 16, 32, or 64 bits
		     are passed by reference.  */
		  msize = int_size_in_bytes (type);
		}
	    }

	  /* __m128 is passed by reference.  */
	  return msize != 1 && msize != 2 && msize != 4 && msize != 8;
	}
      else if (arg.type && int_size_in_bytes (arg.type) == -1)
	return true;
    }

  return false;
}

/* Return true when TYPE should be 128bit aligned for 32bit argument
   passing ABI.  XXX: This function is obsolete and is only used for
   checking psABI compatibility with previous versions of GCC.  */

static bool
ix86_compat_aligned_value_p (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);
  if (((TARGET_SSE && SSE_REG_MODE_P (mode))
       || mode == TDmode
       || mode == TFmode
       || mode == TCmode)
      && (!TYPE_USER_ALIGN (type) || TYPE_ALIGN (type) > 128))
    return true;
  if (TYPE_ALIGN (type) < 128)
    return false;

  if (AGGREGATE_TYPE_P (type))
    {
      /* Walk the aggregates recursively.  */
      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  {
	    tree field;

	    /* Walk all the structure fields.  */
	    for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	      {
		if (TREE_CODE (field) == FIELD_DECL
		    && ix86_compat_aligned_value_p (TREE_TYPE (field)))
		  return true;
	      }
	    break;
	  }

	case ARRAY_TYPE:
	  /* Just for use if some languages passes arrays by value.  */
	  if (ix86_compat_aligned_value_p (TREE_TYPE (type)))
	    return true;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  return false;
}

/* Return the alignment boundary for MODE and TYPE with alignment ALIGN.
   XXX: This function is obsolete and is only used for checking psABI
   compatibility with previous versions of GCC.  */

static unsigned int
ix86_compat_function_arg_boundary (machine_mode mode,
				   const_tree type, unsigned int align)
{
  /* In 32bit, only _Decimal128 and __float128 are aligned to their
     natural boundaries.  */
  if (!TARGET_64BIT && mode != TDmode && mode != TFmode)
    {
      /* i386 ABI defines all arguments to be 4 byte aligned.  We have to
	 make an exception for SSE modes since these require 128bit
	 alignment.

	 The handling here differs from field_alignment.  ICC aligns MMX
	 arguments to 4 byte boundaries, while structure fields are aligned
	 to 8 byte boundaries.  */
      if (!type)
	{
	  if (!(TARGET_SSE && SSE_REG_MODE_P (mode)))
	    align = PARM_BOUNDARY;
	}
      else
	{
	  if (!ix86_compat_aligned_value_p (type))
	    align = PARM_BOUNDARY;
	}
    }
  if (align > BIGGEST_ALIGNMENT)
    align = BIGGEST_ALIGNMENT;
  return align;
}

/* Return true when TYPE should be 128bit aligned for 32bit argument
   passing ABI.  */

static bool
ix86_contains_aligned_value_p (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (mode == XFmode || mode == XCmode)
    return false;

  if (TYPE_ALIGN (type) < 128)
    return false;

  if (AGGREGATE_TYPE_P (type))
    {
      /* Walk the aggregates recursively.  */
      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  {
	    tree field;

	    /* Walk all the structure fields.  */
	    for (field = TYPE_FIELDS (type);
		 field;
		 field = DECL_CHAIN (field))
	      {
		if (TREE_CODE (field) == FIELD_DECL
		    && ix86_contains_aligned_value_p (TREE_TYPE (field)))
		  return true;
	      }
	    break;
	  }

	case ARRAY_TYPE:
	  /* Just for use if some languages passes arrays by value.  */
	  if (ix86_contains_aligned_value_p (TREE_TYPE (type)))
	    return true;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    return TYPE_ALIGN (type) >= 128;

  return false;
}

/* Gives the alignment boundary, in bits, of an argument with the
   specified mode and type.  */

static unsigned int
ix86_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int align;
  if (type)
    {
      /* Since the main variant type is used for call, we convert it to
	 the main variant type.  */
      type = TYPE_MAIN_VARIANT (type);
      align = TYPE_ALIGN (type);
      if (TYPE_EMPTY_P (type))
	return PARM_BOUNDARY;
    }
  else
    align = GET_MODE_ALIGNMENT (mode);
  if (align < PARM_BOUNDARY)
    align = PARM_BOUNDARY;
  else
    {
      static bool warned;
      unsigned int saved_align = align;

      if (!TARGET_64BIT)
	{
	  /* i386 ABI defines XFmode arguments to be 4 byte aligned.  */
	  if (!type)
	    {
	      if (mode == XFmode || mode == XCmode)
		align = PARM_BOUNDARY;
	    }
	  else if (!ix86_contains_aligned_value_p (type))
	    align = PARM_BOUNDARY;

	  if (align < 128)
	    align = PARM_BOUNDARY;
	}

      if (warn_psabi
	  && !warned
	  && align != ix86_compat_function_arg_boundary (mode, type,
							 saved_align))
	{
	  warned = true;
	  inform (input_location,
		  "the ABI for passing parameters with %d-byte"
		  " alignment has changed in GCC 4.6",
		  align / BITS_PER_UNIT);
	}
    }

  return align;
}

/* Return true if N is a possible register number of function value.  */

static bool
ix86_function_value_regno_p (const unsigned int regno)
{
  switch (regno)
    {
    case AX_REG:
      return true;
    case DX_REG:
      return (!TARGET_64BIT || ix86_cfun_abi () != MS_ABI);
    case DI_REG:
    case SI_REG:
      return TARGET_64BIT && ix86_cfun_abi () != MS_ABI;

      /* Complex values are returned in %st(0)/%st(1) pair.  */
    case ST0_REG:
    case ST1_REG:
      /* TODO: The function should depend on current function ABI but
       builtins.cc would need updating then. Therefore we use the
       default ABI.  */
      if (TARGET_64BIT && ix86_cfun_abi () == MS_ABI)
	return false;
      return TARGET_FLOAT_RETURNS_IN_80387;

      /* Complex values are returned in %xmm0/%xmm1 pair.  */
    case XMM0_REG:
    case XMM1_REG:
      return TARGET_SSE;

    case MM0_REG:
      if (TARGET_MACHO || TARGET_64BIT)
	return false;
      return TARGET_MMX;
    }

  return false;
}

/* Check whether the register REGNO should be zeroed on X86.
   When ALL_SSE_ZEROED is true, all SSE registers have been zeroed
   together, no need to zero it again.
   When NEED_ZERO_MMX is true, MMX registers should be cleared.  */

static bool
zero_call_used_regno_p (const unsigned int regno,
			bool all_sse_zeroed,
			bool need_zero_mmx)
{
  return GENERAL_REGNO_P (regno)
	 || (!all_sse_zeroed && SSE_REGNO_P (regno))
	 || MASK_REGNO_P (regno)
	 || (need_zero_mmx && MMX_REGNO_P (regno));
}

/* Return the machine_mode that is used to zero register REGNO.  */

static machine_mode
zero_call_used_regno_mode (const unsigned int regno)
{
  /* NB: We only need to zero the lower 32 bits for integer registers
     and the lower 128 bits for vector registers since destination are
     zero-extended to the full register width.  */
  if (GENERAL_REGNO_P (regno))
    return SImode;
  else if (SSE_REGNO_P (regno))
    return V4SFmode;
  else if (MASK_REGNO_P (regno))
    return HImode;
  else if (MMX_REGNO_P (regno))
    return V2SImode;
  else
    gcc_unreachable ();
}

/* Generate a rtx to zero all vector registers together if possible,
   otherwise, return NULL.  */

static rtx
zero_all_vector_registers (HARD_REG_SET need_zeroed_hardregs)
{
  if (!TARGET_AVX)
    return NULL;

  for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((LEGACY_SSE_REGNO_P (regno)
	 || (TARGET_64BIT
	     && (REX_SSE_REGNO_P (regno)
		 || (TARGET_AVX512F && EXT_REX_SSE_REGNO_P (regno)))))
	&& !TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
      return NULL;

  return gen_avx_vzeroall ();
}

/* Generate insns to zero all st registers together.
   Return true when zeroing instructions are generated.
   Assume the number of st registers that are zeroed is num_of_st,
   we will emit the following sequence to zero them together:
		  fldz;		\
		  fldz;		\
		  ...
		  fldz;		\
		  fstp %%st(0);	\
		  fstp %%st(0);	\
		  ...
		  fstp %%st(0);
   i.e., num_of_st fldz followed by num_of_st fstp to clear the stack
   mark stack slots empty.

   How to compute the num_of_st:
   There is no direct mapping from stack registers to hard register
   numbers.  If one stack register needs to be cleared, we don't know
   where in the stack the value remains.  So, if any stack register
   needs to be cleared, the whole stack should be cleared.  However,
   x87 stack registers that hold the return value should be excluded.
   x87 returns in the top (two for complex values) register, so
   num_of_st should be 7/6 when x87 returns, otherwise it will be 8.
   return the value of num_of_st.  */


static int
zero_all_st_registers (HARD_REG_SET need_zeroed_hardregs)
{

  /* If the FPU is disabled, no need to zero all st registers.  */
  if (! (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387))
    return 0;

  unsigned int num_of_st = 0;
  for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((STACK_REGNO_P (regno) || MMX_REGNO_P (regno))
	&& TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
      {
	num_of_st++;
	break;
      }

  if (num_of_st == 0)
    return 0;

  bool return_with_x87 = false;
  return_with_x87 = (crtl->return_rtx
		     && (STACK_REG_P (crtl->return_rtx)));

  bool complex_return = false;
  complex_return = (crtl->return_rtx
		    && COMPLEX_MODE_P (GET_MODE (crtl->return_rtx)));

  if (return_with_x87)
    if (complex_return)
      num_of_st = 6;
    else
      num_of_st = 7;
  else
    num_of_st = 8;

  rtx st_reg = gen_rtx_REG (XFmode, FIRST_STACK_REG);
  for (unsigned int i = 0; i < num_of_st; i++)
    emit_insn (gen_rtx_SET (st_reg, CONST0_RTX (XFmode)));

  for (unsigned int i = 0; i < num_of_st; i++)
    {
      rtx insn;
      insn = emit_insn (gen_rtx_SET (st_reg, st_reg));
      add_reg_note (insn, REG_DEAD, st_reg);
    }
  return num_of_st;
}


/* When the routine exit in MMX mode, if any ST register needs
   to be zeroed, we should clear all MMX registers except the
   RET_MMX_REGNO that holds the return value.  */
static bool
zero_all_mm_registers (HARD_REG_SET need_zeroed_hardregs,
		       unsigned int ret_mmx_regno)
{
  bool need_zero_all_mm = false;
  for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (STACK_REGNO_P (regno)
	&& TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
      {
	need_zero_all_mm = true;
	break;
      }

  if (!need_zero_all_mm)
    return false;

  machine_mode mode = V2SImode;
  for (unsigned int regno = FIRST_MMX_REG; regno <= LAST_MMX_REG; regno++)
    if (regno != ret_mmx_regno)
      {
	rtx reg = gen_rtx_REG (mode, regno);
	emit_insn (gen_rtx_SET (reg, CONST0_RTX (mode)));
      }
  return true;
}

/* TARGET_ZERO_CALL_USED_REGS.  */
/* Generate a sequence of instructions that zero registers specified by
   NEED_ZEROED_HARDREGS.  Return the ZEROED_HARDREGS that are actually
   zeroed.  */
static HARD_REG_SET
ix86_zero_call_used_regs (HARD_REG_SET need_zeroed_hardregs)
{
  HARD_REG_SET zeroed_hardregs;
  bool all_sse_zeroed = false;
  int all_st_zeroed_num = 0;
  bool all_mm_zeroed = false;

  CLEAR_HARD_REG_SET (zeroed_hardregs);

  /* first, let's see whether we can zero all vector registers together.  */
  rtx zero_all_vec_insn = zero_all_vector_registers (need_zeroed_hardregs);
  if (zero_all_vec_insn)
    {
      emit_insn (zero_all_vec_insn);
      all_sse_zeroed = true;
    }

  /* mm/st registers are shared registers set, we should follow the following
     rules to clear them:
			MMX exit mode	      x87 exit mode
	-------------|----------------------|---------------
	uses x87 reg | clear all MMX	    | clear all x87
	uses MMX reg | clear individual MMX | clear all x87
	x87 + MMX    | clear all MMX	    | clear all x87

     first, we should decide which mode (MMX mode or x87 mode) the function
     exit with.  */

  bool exit_with_mmx_mode = (crtl->return_rtx
			     && (MMX_REG_P (crtl->return_rtx)));

  if (!exit_with_mmx_mode)
    /* x87 exit mode, we should zero all st registers together.  */
    {
      all_st_zeroed_num = zero_all_st_registers (need_zeroed_hardregs);

      if (all_st_zeroed_num > 0)
	for (unsigned int regno = FIRST_STACK_REG; regno <= LAST_STACK_REG; regno++)
	  /* x87 stack registers that hold the return value should be excluded.
	     x87 returns in the top (two for complex values) register.  */
	  if (all_st_zeroed_num == 8
	      || !((all_st_zeroed_num >= 6 && regno == REGNO (crtl->return_rtx))
		   || (all_st_zeroed_num == 6
		       && (regno == (REGNO (crtl->return_rtx) + 1)))))
	    SET_HARD_REG_BIT (zeroed_hardregs, regno);
    }
  else
    /* MMX exit mode, check whether we can zero all mm registers.  */
    {
      unsigned int exit_mmx_regno = REGNO (crtl->return_rtx);
      all_mm_zeroed = zero_all_mm_registers (need_zeroed_hardregs,
					     exit_mmx_regno);
      if (all_mm_zeroed)
	for (unsigned int regno = FIRST_MMX_REG; regno <= LAST_MMX_REG; regno++)
	  if (regno != exit_mmx_regno)
	    SET_HARD_REG_BIT (zeroed_hardregs, regno);
    }

  /* Now, generate instructions to zero all the other registers.  */

  for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (!TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
	continue;
      if (!zero_call_used_regno_p (regno, all_sse_zeroed,
				   exit_with_mmx_mode && !all_mm_zeroed))
	continue;

      SET_HARD_REG_BIT (zeroed_hardregs, regno);

      machine_mode mode = zero_call_used_regno_mode (regno);

      rtx reg = gen_rtx_REG (mode, regno);
      rtx tmp = gen_rtx_SET (reg, CONST0_RTX (mode));

      switch (mode)
	{
	case E_SImode:
	  if (!TARGET_USE_MOV0 || optimize_insn_for_size_p ())
	    {
	      rtx clob = gen_rtx_CLOBBER (VOIDmode,
					  gen_rtx_REG (CCmode,
						       FLAGS_REG));
	      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
							   tmp,
							   clob));
	    }
	  /* FALLTHRU.  */

	case E_V4SFmode:
	case E_HImode:
	case E_V2SImode:
	  emit_insn (tmp);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  return zeroed_hardregs;
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

static rtx
function_value_32 (machine_mode orig_mode, machine_mode mode,
		   const_tree fntype, const_tree fn)
{
  unsigned int regno;

  /* 8-byte vector modes in %mm0. See ix86_return_in_memory for where
     we normally prevent this case when mmx is not available.  However
     some ABIs may require the result to be returned like DImode.  */
  if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 8)
    regno = FIRST_MMX_REG;

  /* 16-byte vector modes in %xmm0.  See ix86_return_in_memory for where
     we prevent this case when sse is not available.  However some ABIs
     may require the result to be returned like integer TImode.  */
  else if (mode == TImode
	   || (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 16))
    regno = FIRST_SSE_REG;

  /* 32-byte vector modes in %ymm0.   */
  else if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 32)
    regno = FIRST_SSE_REG;

  /* 64-byte vector modes in %zmm0.   */
  else if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 64)
    regno = FIRST_SSE_REG;

  /* Floating point return values in %st(0) (unless -mno-fp-ret-in-387).  */
  else if (X87_FLOAT_MODE_P (mode) && TARGET_FLOAT_RETURNS_IN_80387)
    regno = FIRST_FLOAT_REG;
  else
    /* Most things go in %eax.  */
    regno = AX_REG;

  /* Return __bf16/ _Float16/_Complex _Foat16 by sse register.  */
  if (mode == HFmode || mode == BFmode)
    {
      if (!TARGET_SSE2)
	{
	  error ("SSE register return with SSE2 disabled");
	  regno = AX_REG;
	}
      else
	regno = FIRST_SSE_REG;
    }

  if (mode == HCmode)
    {
      if (!TARGET_SSE2)
	error ("SSE register return with SSE2 disabled");

      rtx ret = gen_rtx_PARALLEL (mode, rtvec_alloc(1));
      XVECEXP (ret, 0, 0)
	= gen_rtx_EXPR_LIST (VOIDmode,
			     gen_rtx_REG (SImode,
					  TARGET_SSE2 ? FIRST_SSE_REG : AX_REG),
			     GEN_INT (0));
      return ret;
    }

  /* Override FP return register with %xmm0 for local functions when
     SSE math is enabled or for functions with sseregparm attribute.  */
  if ((fn || fntype) && (mode == SFmode || mode == DFmode))
    {
      int sse_level = ix86_function_sseregparm (fntype, fn, false);
      if (sse_level == -1)
	{
	  error ("calling %qD with SSE calling convention without "
		 "SSE/SSE2 enabled", fn);
	  sorry ("this is a GCC bug that can be worked around by adding "
		 "attribute used to function called");
	}
      else if ((sse_level >= 1 && mode == SFmode)
	       || (sse_level == 2 && mode == DFmode))
	regno = FIRST_SSE_REG;
    }

  /* OImode shouldn't be used directly.  */
  gcc_assert (mode != OImode);

  return gen_rtx_REG (orig_mode, regno);
}

static rtx
function_value_64 (machine_mode orig_mode, machine_mode mode,
		   const_tree valtype)
{
  rtx ret;

  /* Handle libcalls, which don't provide a type node.  */
  if (valtype == NULL)
    {
      unsigned int regno;

      switch (mode)
	{
	case E_BFmode:
	case E_HFmode:
	case E_HCmode:
	case E_SFmode:
	case E_SCmode:
	case E_DFmode:
	case E_DCmode:
	case E_TFmode:
	case E_SDmode:
	case E_DDmode:
	case E_TDmode:
	  regno = FIRST_SSE_REG;
	  break;
	case E_XFmode:
	case E_XCmode:
	  regno = FIRST_FLOAT_REG;
	  break;
	case E_TCmode:
	  return NULL;
	default:
	  regno = AX_REG;
	}

      return gen_rtx_REG (mode, regno);
    }
  else if (POINTER_TYPE_P (valtype))
    {
      /* Pointers are always returned in word_mode.  */
      mode = word_mode;
    }

  ret = construct_container (mode, orig_mode, valtype, 1,
			     X86_64_REGPARM_MAX, X86_64_SSE_REGPARM_MAX,
			     x86_64_int_return_registers, 0);

  /* For zero sized structures, construct_container returns NULL, but we
     need to keep rest of compiler happy by returning meaningful value.  */
  if (!ret)
    ret = gen_rtx_REG (orig_mode, AX_REG);

  return ret;
}

static rtx
function_value_ms_32 (machine_mode orig_mode, machine_mode mode,
		      const_tree fntype, const_tree fn, const_tree valtype)
{
  unsigned int regno;

  /* Floating point return values in %st(0)
     (unless -mno-fp-ret-in-387 or aggregate type of up to 8 bytes).  */
  if (X87_FLOAT_MODE_P (mode) && TARGET_FLOAT_RETURNS_IN_80387
	   && (GET_MODE_SIZE (mode) > 8
	       || valtype == NULL_TREE || !AGGREGATE_TYPE_P (valtype)))
  {
    regno = FIRST_FLOAT_REG;
    return gen_rtx_REG (orig_mode, regno);
  }
  else
    return function_value_32(orig_mode, mode, fntype,fn);
}

static rtx
function_value_ms_64 (machine_mode orig_mode, machine_mode mode,
		      const_tree valtype)
{
  unsigned int regno = AX_REG;

  if (TARGET_SSE)
    {
      switch (GET_MODE_SIZE (mode))
	{
	case 16:
	  if (valtype != NULL_TREE
	      && !VECTOR_INTEGER_TYPE_P (valtype)
	      && !VECTOR_INTEGER_TYPE_P (valtype)
	      && !INTEGRAL_TYPE_P (valtype)
	      && !VECTOR_FLOAT_TYPE_P (valtype))
	    break;
	  if ((SCALAR_INT_MODE_P (mode) || VECTOR_MODE_P (mode))
	      && !COMPLEX_MODE_P (mode))
	    regno = FIRST_SSE_REG;
	  break;
	case 8:
	case 4:
	  if (valtype != NULL_TREE && AGGREGATE_TYPE_P (valtype))
	    break;
	  if (mode == SFmode || mode == DFmode)
	    regno = FIRST_SSE_REG;
	  break;
	default:
	  break;
        }
    }
  return gen_rtx_REG (orig_mode, regno);
}

static rtx
ix86_function_value_1 (const_tree valtype, const_tree fntype_or_decl,
		       machine_mode orig_mode, machine_mode mode)
{
  const_tree fn, fntype;

  fn = NULL_TREE;
  if (fntype_or_decl && DECL_P (fntype_or_decl))
    fn = fntype_or_decl;
  fntype = fn ? TREE_TYPE (fn) : fntype_or_decl;
  
  if (ix86_function_type_abi (fntype) == MS_ABI)
    {
      if (TARGET_64BIT)
	return function_value_ms_64 (orig_mode, mode, valtype);
      else
	return function_value_ms_32 (orig_mode, mode, fntype, fn, valtype);
    }
  else if (TARGET_64BIT)
    return function_value_64 (orig_mode, mode, valtype);
  else
    return function_value_32 (orig_mode, mode, fntype, fn);
}

static rtx
ix86_function_value (const_tree valtype, const_tree fntype_or_decl, bool)
{
  machine_mode mode, orig_mode;

  orig_mode = TYPE_MODE (valtype);
  mode = type_natural_mode (valtype, NULL, true);
  return ix86_function_value_1 (valtype, fntype_or_decl, orig_mode, mode);
}

/* Pointer function arguments and return values are promoted to
   word_mode for normal functions.  */

static machine_mode
ix86_promote_function_mode (const_tree type, machine_mode mode,
			    int *punsignedp, const_tree fntype,
			    int for_return)
{
  if (cfun->machine->func_type == TYPE_NORMAL
      && type != NULL_TREE
      && POINTER_TYPE_P (type))
    {
      *punsignedp = POINTERS_EXTEND_UNSIGNED;
      return word_mode;
    }
  return default_promote_function_mode (type, mode, punsignedp, fntype,
					for_return);
}

/* Return true if a structure, union or array with MODE containing FIELD
   should be accessed using BLKmode.  */

static bool
ix86_member_type_forces_blk (const_tree field, machine_mode mode)
{
  /* Union with XFmode must be in BLKmode.  */
  return (mode == XFmode
	  && (TREE_CODE (DECL_FIELD_CONTEXT (field)) == UNION_TYPE
	      || TREE_CODE (DECL_FIELD_CONTEXT (field)) == QUAL_UNION_TYPE));
}

rtx
ix86_libcall_value (machine_mode mode)
{
  return ix86_function_value_1 (NULL, NULL, mode, mode);
}

/* Return true iff type is returned in memory.  */

static bool
ix86_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const machine_mode mode = type_natural_mode (type, NULL, true);
  HOST_WIDE_INT size;

  if (TARGET_64BIT)
    {
      if (ix86_function_type_abi (fntype) == MS_ABI)
	{
	  size = int_size_in_bytes (type);

	  /* __m128 is returned in xmm0.  */
	  if ((!type || VECTOR_INTEGER_TYPE_P (type)
	       || INTEGRAL_TYPE_P (type)
	       || VECTOR_FLOAT_TYPE_P (type))
	      && (SCALAR_INT_MODE_P (mode) || VECTOR_MODE_P (mode))
	      && !COMPLEX_MODE_P (mode)
	      && (GET_MODE_SIZE (mode) == 16 || size == 16))
	    return false;

	  /* Otherwise, the size must be exactly in [1248]. */
	  return size != 1 && size != 2 && size != 4 && size != 8;
	}
      else
	{
	  int needed_intregs, needed_sseregs;

	  return examine_argument (mode, type, 1,
				   &needed_intregs, &needed_sseregs);
	}
    }
  else
    {
      size = int_size_in_bytes (type);

      /* Intel MCU psABI returns scalars and aggregates no larger than 8
	 bytes in registers.  */
      if (TARGET_IAMCU)
	return VECTOR_MODE_P (mode) || size < 0 || size > 8;

      if (mode == BLKmode)
	return true;

      if (MS_AGGREGATE_RETURN && AGGREGATE_TYPE_P (type) && size <= 8)
	return false;

      if (VECTOR_MODE_P (mode) || mode == TImode)
	{
	  /* User-created vectors small enough to fit in EAX.  */
	  if (size < 8)
	    return false;

	  /* Unless ABI prescibes otherwise,
	     MMX/3dNow values are returned in MM0 if available.  */
	     
	  if (size == 8)
	    return TARGET_VECT8_RETURNS || !TARGET_MMX;

	  /* SSE values are returned in XMM0 if available.  */
	  if (size == 16)
	    return !TARGET_SSE;

	  /* AVX values are returned in YMM0 if available.  */
	  if (size == 32)
	    return !TARGET_AVX;

	  /* AVX512F values are returned in ZMM0 if available.  */
	  if (size == 64)
	    return !TARGET_AVX512F || !TARGET_EVEX512;
	}

      if (mode == XFmode)
	return false;

      if (size > 12)
	return true;

      /* OImode shouldn't be used directly.  */
      gcc_assert (mode != OImode);

      return false;
    }
}

/* Implement TARGET_PUSH_ARGUMENT.  */

static bool
ix86_push_argument (unsigned int npush)
{
  /* If SSE2 is available, use vector move to put large argument onto
     stack.  NB:  In 32-bit mode, use 8-byte vector move.  */
  return ((!TARGET_SSE2 || npush < (TARGET_64BIT ? 16 : 8))
	  && TARGET_PUSH_ARGS
	  && !ACCUMULATE_OUTGOING_ARGS);
}


/* Create the va_list data type.  */

static tree
ix86_build_builtin_va_list_64 (void)
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  record = lang_hooks.types.make_type (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION,
			  TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("gp_offset"),
		      unsigned_type_node);
  f_fpr = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("fp_offset"),
		      unsigned_type_node);
  f_ovf = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("reg_save_area"),
		      ptr_type_node);

  va_list_gpr_counter_field = f_gpr;
  va_list_fpr_counter_field = f_fpr;

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  DECL_CHAIN (f_gpr) = f_fpr;
  DECL_CHAIN (f_fpr) = f_ovf;
  DECL_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  TYPE_ATTRIBUTES (record) = tree_cons (get_identifier ("sysv_abi va_list"),
					NULL_TREE, TYPE_ATTRIBUTES (record));

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Setup the builtin va_list data type and for 64-bit the additional
   calling convention specific va_list data types.  */

static tree
ix86_build_builtin_va_list (void)
{
  if (TARGET_64BIT)
    {
      /* Initialize ABI specific va_list builtin types.

	 In lto1, we can encounter two va_list types:
	 - one as a result of the type-merge across TUs, and
	 - the one constructed here.
	 These two types will not have the same TYPE_MAIN_VARIANT, and therefore
	 a type identity check in canonical_va_list_type based on
	 TYPE_MAIN_VARIANT (which we used to have) will not work.
	 Instead, we tag each va_list_type_node with its unique attribute, and
	 look for the attribute in the type identity check in
	 canonical_va_list_type.

	 Tagging sysv_va_list_type_node directly with the attribute is
	 problematic since it's a array of one record, which will degrade into a
	 pointer to record when used as parameter (see build_va_arg comments for
	 an example), dropping the attribute in the process.  So we tag the
	 record instead.  */

      /* For SYSV_ABI we use an array of one record.  */
      sysv_va_list_type_node = ix86_build_builtin_va_list_64 ();
	
      /* For MS_ABI we use plain pointer to argument area.  */
      tree char_ptr_type = build_pointer_type (char_type_node);
      tree attr = tree_cons (get_identifier ("ms_abi va_list"), NULL_TREE,
			     TYPE_ATTRIBUTES (char_ptr_type));
      ms_va_list_type_node = build_type_attribute_variant (char_ptr_type, attr);

      return ((ix86_abi == MS_ABI)
	      ? ms_va_list_type_node
	      : sysv_va_list_type_node);
    }
  else
    {
      /* For i386 we use plain pointer to argument area.  */
      return build_pointer_type (char_type_node);
    }
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
setup_incoming_varargs_64 (CUMULATIVE_ARGS *cum)
{
  rtx save_area, mem;
  alias_set_type set;
  int i, max;

  /* GPR size of varargs save area.  */
  if (cfun->va_list_gpr_size)
    ix86_varargs_gpr_size = X86_64_REGPARM_MAX * UNITS_PER_WORD;
  else
    ix86_varargs_gpr_size = 0;

  /* FPR size of varargs save area.  We don't need it if we don't pass
     anything in SSE registers.  */
  if (TARGET_SSE && cfun->va_list_fpr_size)
    ix86_varargs_fpr_size = X86_64_SSE_REGPARM_MAX * 16;
  else
    ix86_varargs_fpr_size = 0;

  if (! ix86_varargs_gpr_size && ! ix86_varargs_fpr_size)
    return;

  save_area = frame_pointer_rtx;
  set = get_varargs_alias_set ();

  max = cum->regno + cfun->va_list_gpr_size / UNITS_PER_WORD;
  if (max > X86_64_REGPARM_MAX)
    max = X86_64_REGPARM_MAX;

  for (i = cum->regno; i < max; i++)
    {
      mem = gen_rtx_MEM (word_mode,
			 plus_constant (Pmode, save_area, i * UNITS_PER_WORD));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);
      emit_move_insn (mem,
		      gen_rtx_REG (word_mode,
				   x86_64_int_parameter_registers[i]));
    }

  if (ix86_varargs_fpr_size)
    {
      machine_mode smode;
      rtx_code_label *label;
      rtx test;

      /* Now emit code to save SSE registers.  The AX parameter contains number
	 of SSE parameter registers used to call this function, though all we
	 actually check here is the zero/non-zero status.  */

      label = gen_label_rtx ();
      test = gen_rtx_EQ (VOIDmode, gen_rtx_REG (QImode, AX_REG), const0_rtx);
      emit_jump_insn (gen_cbranchqi4 (test, XEXP (test, 0), XEXP (test, 1),
				      label));

      /* ??? If !TARGET_SSE_TYPELESS_STORES, would we perform better if
	 we used movdqa (i.e. TImode) instead?  Perhaps even better would
	 be if we could determine the real mode of the data, via a hook
	 into pass_stdarg.  Ignore all that for now.  */
      smode = V4SFmode;
      if (crtl->stack_alignment_needed < GET_MODE_ALIGNMENT (smode))
	crtl->stack_alignment_needed = GET_MODE_ALIGNMENT (smode);

      max = cum->sse_regno + cfun->va_list_fpr_size / 16;
      if (max > X86_64_SSE_REGPARM_MAX)
	max = X86_64_SSE_REGPARM_MAX;

      for (i = cum->sse_regno; i < max; ++i)
	{
	  mem = plus_constant (Pmode, save_area,
			       i * 16 + ix86_varargs_gpr_size);
	  mem = gen_rtx_MEM (smode, mem);
	  MEM_NOTRAP_P (mem) = 1;
	  set_mem_alias_set (mem, set);
	  set_mem_align (mem, GET_MODE_ALIGNMENT (smode));

	  emit_move_insn (mem, gen_rtx_REG (smode, GET_SSE_REGNO (i)));
	}

      emit_label (label);
    }
}

static void
setup_incoming_varargs_ms_64 (CUMULATIVE_ARGS *cum)
{
  alias_set_type set = get_varargs_alias_set ();
  int i;

  /* Reset to zero, as there might be a sysv vaarg used
     before.  */
  ix86_varargs_gpr_size = 0;
  ix86_varargs_fpr_size = 0;

  for (i = cum->regno; i < X86_64_MS_REGPARM_MAX; i++)
    {
      rtx reg, mem;

      mem = gen_rtx_MEM (Pmode,
			 plus_constant (Pmode, virtual_incoming_args_rtx,
					i * UNITS_PER_WORD));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);

      reg = gen_rtx_REG (Pmode, x86_64_ms_abi_int_parameter_registers[i]);
      emit_move_insn (mem, reg);
    }
}

static void
ix86_setup_incoming_varargs (cumulative_args_t cum_v,
			     const function_arg_info &arg,
			     int *, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  CUMULATIVE_ARGS next_cum;
  tree fntype;

  /* This argument doesn't appear to be used anymore.  Which is good,
     because the old code here didn't suppress rtl generation.  */
  gcc_assert (!no_rtl);

  if (!TARGET_64BIT)
    return;

  fntype = TREE_TYPE (current_function_decl);

  /* For varargs, we do not want to skip the dummy va_dcl argument.
     For stdargs, we do want to skip the last named argument.  */
  next_cum = *cum;
  if ((!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
       || arg.type != NULL_TREE)
      && stdarg_p (fntype))
    ix86_function_arg_advance (pack_cumulative_args (&next_cum), arg);

  if (cum->call_abi == MS_ABI)
    setup_incoming_varargs_ms_64 (&next_cum);
  else
    setup_incoming_varargs_64 (&next_cum);
}

/* Checks if TYPE is of kind va_list char *.  */

static bool
is_va_list_char_pointer (tree type)
{
  tree canonic;

  /* For 32-bit it is always true.  */
  if (!TARGET_64BIT)
    return true;
  canonic = ix86_canonical_va_list_type (type);
  return (canonic == ms_va_list_type_node
          || (ix86_abi == MS_ABI && canonic == va_list_type_node));
}

/* Implement va_start.  */

static void
ix86_va_start (tree valist, rtx nextarg)
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;
  tree type;
  rtx ovf_rtx;

  if (flag_split_stack
      && cfun->machine->split_stack_varargs_pointer == NULL_RTX)
    {
      unsigned int scratch_regno;

      /* When we are splitting the stack, we can't refer to the stack
	 arguments using internal_arg_pointer, because they may be on
	 the old stack.  The split stack prologue will arrange to
	 leave a pointer to the old stack arguments in a scratch
	 register, which we here copy to a pseudo-register.  The split
	 stack prologue can't set the pseudo-register directly because
	 it (the prologue) runs before any registers have been saved.  */

      scratch_regno = split_stack_prologue_scratch_regno ();
      if (scratch_regno != INVALID_REGNUM)
	{
	  rtx reg;
	  rtx_insn *seq;

	  reg = gen_reg_rtx (Pmode);
	  cfun->machine->split_stack_varargs_pointer = reg;

	  start_sequence ();
	  emit_move_insn (reg, gen_rtx_REG (Pmode, scratch_regno));
	  seq = get_insns ();
	  end_sequence ();

	  push_topmost_sequence ();
	  emit_insn_after (seq, entry_of_function ());
	  pop_topmost_sequence ();
	}
    }

  /* Only 64bit target needs something special.  */
  if (is_va_list_char_pointer (TREE_TYPE (valist)))
    {
      if (cfun->machine->split_stack_varargs_pointer == NULL_RTX)
	std_expand_builtin_va_start (valist, nextarg);
      else
	{
	  rtx va_r, next;

	  va_r = expand_expr (valist, NULL_RTX, VOIDmode, EXPAND_WRITE);
	  next = expand_binop (ptr_mode, add_optab,
			       cfun->machine->split_stack_varargs_pointer,
			       crtl->args.arg_offset_rtx,
			       NULL_RTX, 0, OPTAB_LIB_WIDEN);
	  convert_move (va_r, next, 0);
	}
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (sysv_va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_ovf = DECL_CHAIN (f_fpr);
  f_sav = DECL_CHAIN (f_ovf);

  valist = build_simple_mem_ref (valist);
  TREE_TYPE (valist) = TREE_TYPE (sysv_va_list_type_node);
  /* The following should be folded into the MEM_REF offset.  */
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), unshare_expr (valist),
		f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), unshare_expr (valist),
		f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), unshare_expr (valist),
		f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), unshare_expr (valist),
		f_sav, NULL_TREE);

  /* Count number of gp and fp argument registers used.  */
  words = crtl->args.info.words;
  n_gpr = crtl->args.info.regno;
  n_fpr = crtl->args.info.sse_regno;

  if (cfun->va_list_gpr_size)
    {
      type = TREE_TYPE (gpr);
      t = build2 (MODIFY_EXPR, type,
		  gpr, build_int_cst (type, n_gpr * 8));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (TARGET_SSE && cfun->va_list_fpr_size)
    {
      type = TREE_TYPE (fpr);
      t = build2 (MODIFY_EXPR, type, fpr,
		  build_int_cst (type, n_fpr * 16 + 8*X86_64_REGPARM_MAX));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Find the overflow area.  */
  type = TREE_TYPE (ovf);
  if (cfun->machine->split_stack_varargs_pointer == NULL_RTX)
    ovf_rtx = crtl->args.internal_arg_pointer;
  else
    ovf_rtx = cfun->machine->split_stack_varargs_pointer;
  t = make_tree (type, ovf_rtx);
  if (words != 0)
    t = fold_build_pointer_plus_hwi (t, words * UNITS_PER_WORD);

  t = build2 (MODIFY_EXPR, type, ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  if (ix86_varargs_gpr_size || ix86_varargs_fpr_size)
    {
      /* Find the register save area.
	 Prologue of the function save it right above stack frame.  */
      type = TREE_TYPE (sav);
      t = make_tree (type, frame_pointer_rtx);
      if (!ix86_varargs_gpr_size)
	t = fold_build_pointer_plus_hwi (t, -8 * X86_64_REGPARM_MAX);

      t = build2 (MODIFY_EXPR, type, sav, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
}

/* Implement va_arg.  */

static tree
ix86_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
		      gimple_seq *post_p)
{
  static const int intreg[6] = { 0, 1, 2, 3, 4, 5 };
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;
  int size, rsize;
  tree lab_false, lab_over = NULL_TREE;
  tree addr, t2;
  rtx container;
  int indirect_p = 0;
  tree ptrtype;
  machine_mode nat_mode;
  unsigned int arg_boundary;
  unsigned int type_align;

  /* Only 64bit target needs something special.  */
  if (is_va_list_char_pointer (TREE_TYPE (valist)))
    return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);

  f_gpr = TYPE_FIELDS (TREE_TYPE (sysv_va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_ovf = DECL_CHAIN (f_fpr);
  f_sav = DECL_CHAIN (f_ovf);

  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr),
		valist, f_gpr, NULL_TREE);

  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  indirect_p = pass_va_arg_by_reference (type);
  if (indirect_p)
    type = build_pointer_type (type);
  size = arg_int_size_in_bytes (type);
  rsize = CEIL (size, UNITS_PER_WORD);

  nat_mode = type_natural_mode (type, NULL, false);
  switch (nat_mode)
    {
    case E_V16HFmode:
    case E_V16BFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V32QImode:
    case E_V16HImode:
    case E_V4DFmode:
    case E_V4DImode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V16SFmode:
    case E_V16SImode:
    case E_V64QImode:
    case E_V32HImode:
    case E_V8DFmode:
    case E_V8DImode:
      /* Unnamed 256 and 512bit vector mode parameters are passed on stack.  */
      if (!TARGET_64BIT_MS_ABI)
	{
	  container = NULL;
	  break;
	}
      /* FALLTHRU */

    default:
      container = construct_container (nat_mode, TYPE_MODE (type),
				       type, 0, X86_64_REGPARM_MAX,
				       X86_64_SSE_REGPARM_MAX, intreg,
				       0);
      break;
    }

  /* Pull the value out of the saved registers.  */

  addr = create_tmp_var (ptr_type_node, "addr");
  type_align = TYPE_ALIGN (type);

  if (container)
    {
      int needed_intregs, needed_sseregs;
      bool need_temp;
      tree int_addr, sse_addr;

      lab_false = create_artificial_label (UNKNOWN_LOCATION);
      lab_over = create_artificial_label (UNKNOWN_LOCATION);

      examine_argument (nat_mode, type, 0, &needed_intregs, &needed_sseregs);

      need_temp = (!REG_P (container)
		   && ((needed_intregs && TYPE_ALIGN (type) > 64)
		       || TYPE_ALIGN (type) > 128));

      /* In case we are passing structure, verify that it is consecutive block
         on the register save area.  If not we need to do moves.  */
      if (!need_temp && !REG_P (container))
	{
	  /* Verify that all registers are strictly consecutive  */
	  if (SSE_REGNO_P (REGNO (XEXP (XVECEXP (container, 0, 0), 0))))
	    {
	      int i;

	      for (i = 0; i < XVECLEN (container, 0) && !need_temp; i++)
		{
		  rtx slot = XVECEXP (container, 0, i);
		  if (REGNO (XEXP (slot, 0)) != FIRST_SSE_REG + (unsigned int) i
		      || INTVAL (XEXP (slot, 1)) != i * 16)
		    need_temp = true;
		}
	    }
	  else
	    {
	      int i;

	      for (i = 0; i < XVECLEN (container, 0) && !need_temp; i++)
		{
		  rtx slot = XVECEXP (container, 0, i);
		  if (REGNO (XEXP (slot, 0)) != (unsigned int) i
		      || INTVAL (XEXP (slot, 1)) != i * 8)
		    need_temp = true;
		}
	    }
	}
      if (!need_temp)
	{
	  int_addr = addr;
	  sse_addr = addr;
	}
      else
	{
	  int_addr = create_tmp_var (ptr_type_node, "int_addr");
	  sse_addr = create_tmp_var (ptr_type_node, "sse_addr");
	}

      /* First ensure that we fit completely in registers.  */
      if (needed_intregs)
	{
	  t = build_int_cst (TREE_TYPE (gpr),
			     (X86_64_REGPARM_MAX - needed_intregs + 1) * 8);
	  t = build2 (GE_EXPR, boolean_type_node, gpr, t);
	  t2 = build1 (GOTO_EXPR, void_type_node, lab_false);
	  t = build3 (COND_EXPR, void_type_node, t, t2, NULL_TREE);
	  gimplify_and_add (t, pre_p);
	}
      if (needed_sseregs)
	{
	  t = build_int_cst (TREE_TYPE (fpr),
			     (X86_64_SSE_REGPARM_MAX - needed_sseregs + 1) * 16
			     + X86_64_REGPARM_MAX * 8);
	  t = build2 (GE_EXPR, boolean_type_node, fpr, t);
	  t2 = build1 (GOTO_EXPR, void_type_node, lab_false);
	  t = build3 (COND_EXPR, void_type_node, t, t2, NULL_TREE);
	  gimplify_and_add (t, pre_p);
	}

      /* Compute index to start of area used for integer regs.  */
      if (needed_intregs)
	{
	  /* int_addr = gpr + sav; */
	  t = fold_build_pointer_plus (sav, gpr);
	  gimplify_assign (int_addr, t, pre_p);
	}
      if (needed_sseregs)
	{
	  /* sse_addr = fpr + sav; */
	  t = fold_build_pointer_plus (sav, fpr);
	  gimplify_assign (sse_addr, t, pre_p);
	}
      if (need_temp)
	{
	  int i, prev_size = 0;
	  tree temp = create_tmp_var (type, "va_arg_tmp");
	  TREE_ADDRESSABLE (temp) = 1;

	  /* addr = &temp; */
	  t = build1 (ADDR_EXPR, build_pointer_type (type), temp);
	  gimplify_assign (addr, t, pre_p);

	  for (i = 0; i < XVECLEN (container, 0); i++)
	    {
	      rtx slot = XVECEXP (container, 0, i);
	      rtx reg = XEXP (slot, 0);
	      machine_mode mode = GET_MODE (reg);
	      tree piece_type;
	      tree addr_type;
	      tree daddr_type;
	      tree src_addr, src;
	      int src_offset;
	      tree dest_addr, dest;
	      int cur_size = GET_MODE_SIZE (mode);

	      gcc_assert (prev_size <= INTVAL (XEXP (slot, 1)));
	      prev_size = INTVAL (XEXP (slot, 1));
	      if (prev_size + cur_size > size)
		{
		  cur_size = size - prev_size;
		  unsigned int nbits = cur_size * BITS_PER_UNIT;
		  if (!int_mode_for_size (nbits, 1).exists (&mode))
		    mode = QImode;
		}
	      piece_type = lang_hooks.types.type_for_mode (mode, 1);
	      if (mode == GET_MODE (reg))
		addr_type = build_pointer_type (piece_type);
	      else
		addr_type = build_pointer_type_for_mode (piece_type, ptr_mode,
							 true);
	      daddr_type = build_pointer_type_for_mode (piece_type, ptr_mode,
							true);

	      if (SSE_REGNO_P (REGNO (reg)))
		{
		  src_addr = sse_addr;
		  src_offset = (REGNO (reg) - FIRST_SSE_REG) * 16;
		}
	      else
		{
		  src_addr = int_addr;
		  src_offset = REGNO (reg) * 8;
		}
	      src_addr = fold_convert (addr_type, src_addr);
	      src_addr = fold_build_pointer_plus_hwi (src_addr, src_offset);

	      dest_addr = fold_convert (daddr_type, addr);
	      dest_addr = fold_build_pointer_plus_hwi (dest_addr, prev_size);
	      if (cur_size == GET_MODE_SIZE (mode))
		{
		  src = build_va_arg_indirect_ref (src_addr);
		  dest = build_va_arg_indirect_ref (dest_addr);

		  gimplify_assign (dest, src, pre_p);
		}
	      else
		{
		  tree copy
		    = build_call_expr (builtin_decl_implicit (BUILT_IN_MEMCPY),
				       3, dest_addr, src_addr,
				       size_int (cur_size));
		  gimplify_and_add (copy, pre_p);
		}
	      prev_size += cur_size;
	    }
	}

      if (needed_intregs)
	{
	  t = build2 (PLUS_EXPR, TREE_TYPE (gpr), gpr,
		      build_int_cst (TREE_TYPE (gpr), needed_intregs * 8));
	  gimplify_assign (gpr, t, pre_p);
	  /* The GPR save area guarantees only 8-byte alignment.  */
	  if (!need_temp)
	    type_align = MIN (type_align, 64);
	}

      if (needed_sseregs)
	{
	  t = build2 (PLUS_EXPR, TREE_TYPE (fpr), fpr,
		      build_int_cst (TREE_TYPE (fpr), needed_sseregs * 16));
	  gimplify_assign (unshare_expr (fpr), t, pre_p);
	}

      gimple_seq_add_stmt (pre_p, gimple_build_goto (lab_over));

      gimple_seq_add_stmt (pre_p, gimple_build_label (lab_false));
    }

  /* ... otherwise out of the overflow area.  */

  /* When we align parameter on stack for caller, if the parameter
     alignment is beyond MAX_SUPPORTED_STACK_ALIGNMENT, it will be
     aligned at MAX_SUPPORTED_STACK_ALIGNMENT.  We will match callee
     here with caller.  */
  arg_boundary = ix86_function_arg_boundary (VOIDmode, type);
  if ((unsigned int) arg_boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
    arg_boundary = MAX_SUPPORTED_STACK_ALIGNMENT;

  /* Care for on-stack alignment if needed.  */
  if (arg_boundary <= 64 || size == 0)
    t = ovf;
 else
    {
      HOST_WIDE_INT align = arg_boundary / 8;
      t = fold_build_pointer_plus_hwi (ovf, align - 1);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -align));
    }

  gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue);
  gimplify_assign (addr, t, pre_p);

  t = fold_build_pointer_plus_hwi (t, rsize * UNITS_PER_WORD);
  gimplify_assign (unshare_expr (ovf), t, pre_p);

  if (container)
    gimple_seq_add_stmt (pre_p, gimple_build_label (lab_over));

  type = build_aligned_type (type, type_align);
  ptrtype = build_pointer_type_for_mode (type, ptr_mode, true);
  addr = fold_convert (ptrtype, addr);

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);
  return build_va_arg_indirect_ref (addr);
}

/* Return true if OPNUM's MEM should be matched
   in movabs* patterns.  */

bool
ix86_check_movabs (rtx insn, int opnum)
{
  rtx set, mem;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  mem = XEXP (set, opnum);
  while (SUBREG_P (mem))
    mem = SUBREG_REG (mem);
  gcc_assert (MEM_P (mem));
  return volatile_ok || !MEM_VOLATILE_P (mem);
}

/* Return false if INSN contains a MEM with a non-default address space.  */
bool
ix86_check_no_addr_space (rtx insn)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, PATTERN (insn), ALL)
    {
      rtx x = *iter;
      if (MEM_P (x) && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (x)))
	return false;
    }
  return true;
}

/* Initialize the table of extra 80387 mathematical constants.  */

static void
init_ext_80387_constants (void)
{
  static const char * cst[5] =
  {
    "0.3010299956639811952256464283594894482",  /* 0: fldlg2  */
    "0.6931471805599453094286904741849753009",  /* 1: fldln2  */
    "1.4426950408889634073876517827983434472",  /* 2: fldl2e  */
    "3.3219280948873623478083405569094566090",  /* 3: fldl2t  */
    "3.1415926535897932385128089594061862044",  /* 4: fldpi   */
  };
  int i;

  for (i = 0; i < 5; i++)
    {
      real_from_string (&ext_80387_constants_table[i], cst[i]);
      /* Ensure each constant is rounded to XFmode precision.  */
      real_convert (&ext_80387_constants_table[i],
		    XFmode, &ext_80387_constants_table[i]);
    }

  ext_80387_constants_init = 1;
}

/* Return non-zero if the constant is something that
   can be loaded with a special instruction.  */

int
standard_80387_constant_p (rtx x)
{
  machine_mode mode = GET_MODE (x);

  const REAL_VALUE_TYPE *r;

  if (!(CONST_DOUBLE_P (x) && X87_FLOAT_MODE_P (mode)))
    return -1;

  if (x == CONST0_RTX (mode))
    return 1;
  if (x == CONST1_RTX (mode))
    return 2;

  r = CONST_DOUBLE_REAL_VALUE (x);

  /* For XFmode constants, try to find a special 80387 instruction when
     optimizing for size or on those CPUs that benefit from them.  */
  if (mode == XFmode
      && (optimize_function_for_size_p (cfun) || TARGET_EXT_80387_CONSTANTS)
      && !flag_rounding_math)
    {
      int i;

      if (! ext_80387_constants_init)
	init_ext_80387_constants ();

      for (i = 0; i < 5; i++)
        if (real_identical (r, &ext_80387_constants_table[i]))
	  return i + 3;
    }

  /* Load of the constant -0.0 or -1.0 will be split as
     fldz;fchs or fld1;fchs sequence.  */
  if (real_isnegzero (r))
    return 8;
  if (real_identical (r, &dconstm1))
    return 9;

  return 0;
}

/* Return the opcode of the special instruction to be used to load
   the constant X.  */

const char *
standard_80387_constant_opcode (rtx x)
{
  switch (standard_80387_constant_p (x))
    {
    case 1:
      return "fldz";
    case 2:
      return "fld1";
    case 3:
      return "fldlg2";
    case 4:
      return "fldln2";
    case 5:
      return "fldl2e";
    case 6:
      return "fldl2t";
    case 7:
      return "fldpi";
    case 8:
    case 9:
      return "#";
    default:
      gcc_unreachable ();
    }
}

/* Return the CONST_DOUBLE representing the 80387 constant that is
   loaded by the specified special instruction.  The argument IDX
   matches the return value from standard_80387_constant_p.  */

rtx
standard_80387_constant_rtx (int idx)
{
  int i;

  if (! ext_80387_constants_init)
    init_ext_80387_constants ();

  switch (idx)
    {
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      i = idx - 3;
      break;

    default:
      gcc_unreachable ();
    }

  return const_double_from_real_value (ext_80387_constants_table[i],
				       XFmode);
}

/* Return 1 if X is all bits 0, 2 if X is all bits 1
   and 3 if X is all bits 1 with zero extend
   in supported SSE/AVX vector mode.  */

int
standard_sse_constant_p (rtx x, machine_mode pred_mode)
{
  machine_mode mode;

  if (!TARGET_SSE)
    return 0;

  mode = GET_MODE (x);

  if (x == const0_rtx || const0_operand (x, mode))
    return 1;

  if (x == constm1_rtx
      || vector_all_ones_operand (x, mode)
      || ((GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	   || GET_MODE_CLASS (pred_mode) == MODE_VECTOR_FLOAT)
	  && float_vector_all_ones_operand (x, mode)))
    {
      /* VOIDmode integer constant, get mode from the predicate.  */
      if (mode == VOIDmode)
	mode = pred_mode;

      switch (GET_MODE_SIZE (mode))
	{
	case 64:
	  if (TARGET_AVX512F && TARGET_EVEX512)
	    return 2;
	  break;
	case 32:
	  if (TARGET_AVX2)
	    return 2;
	  break;
	case 16:
	  if (TARGET_SSE2)
	    return 2;
	  break;
	case 0:
	  /* VOIDmode */
	  gcc_unreachable ();
	default:
	  break;
	}
    }

  if (vector_all_ones_zero_extend_half_operand (x, mode)
      || vector_all_ones_zero_extend_quarter_operand (x, mode))
    return 3;

  return 0;
}

/* Return the opcode of the special instruction to be used to load
   the constant operands[1] into operands[0].  */

const char *
standard_sse_constant_opcode (rtx_insn *insn, rtx *operands)
{
  machine_mode mode;
  rtx x = operands[1];

  gcc_assert (TARGET_SSE);

  mode = GET_MODE (x);

  if (x == const0_rtx || const0_operand (x, mode))
    {
      switch (get_attr_mode (insn))
	{
	case MODE_TI:
	  if (!EXT_REX_SSE_REG_P (operands[0]))
	    return "%vpxor\t%0, %d0";
	  /* FALLTHRU */
	case MODE_XI:
	case MODE_OI:
	  if (EXT_REX_SSE_REG_P (operands[0]))
	    {
	      if (TARGET_AVX512VL)
		return "vpxord\t%x0, %x0, %x0";
	      else if (TARGET_EVEX512)
		return "vpxord\t%g0, %g0, %g0";
	      else
		gcc_unreachable ();
	    }
	  return "vpxor\t%x0, %x0, %x0";

	case MODE_V2DF:
	  if (!EXT_REX_SSE_REG_P (operands[0]))
	    return "%vxorpd\t%0, %d0";
	  /* FALLTHRU */
	case MODE_V8DF:
	case MODE_V4DF:
	  if (EXT_REX_SSE_REG_P (operands[0]))
	    {
	      if (TARGET_AVX512DQ)
		{
		  if (TARGET_AVX512VL)
		    return "vxorpd\t%x0, %x0, %x0";
		  else if (TARGET_EVEX512)
		    return "vxorpd\t%g0, %g0, %g0";
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  if (TARGET_AVX512VL)
		    return "vpxorq\t%x0, %x0, %x0";
		  else if (TARGET_EVEX512)
		    return "vpxorq\t%g0, %g0, %g0";
		  else
		    gcc_unreachable ();
		}
	    }
	  return "vxorpd\t%x0, %x0, %x0";

	case MODE_V4SF:
	  if (!EXT_REX_SSE_REG_P (operands[0]))
	    return "%vxorps\t%0, %d0";
	  /* FALLTHRU */
	case MODE_V16SF:
	case MODE_V8SF:
	  if (EXT_REX_SSE_REG_P (operands[0]))
	    {
	      if (TARGET_AVX512DQ)
		{
		  if (TARGET_AVX512VL)
		    return "vxorps\t%x0, %x0, %x0";
		  else if (TARGET_EVEX512)
		    return "vxorps\t%g0, %g0, %g0";
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  if (TARGET_AVX512VL)
		    return "vpxord\t%x0, %x0, %x0";
		  else if (TARGET_EVEX512)
		    return "vpxord\t%g0, %g0, %g0";
		  else
		    gcc_unreachable ();
		}
	    }
	  return "vxorps\t%x0, %x0, %x0";

	default:
	  gcc_unreachable ();
	}
    }
  else if (x == constm1_rtx
	   || vector_all_ones_operand (x, mode)
	   || (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
	       && float_vector_all_ones_operand (x, mode)))
    {
      enum attr_mode insn_mode = get_attr_mode (insn);
      
      switch (insn_mode)
	{
	case MODE_XI:
	case MODE_V8DF:
	case MODE_V16SF:
	  gcc_assert (TARGET_AVX512F && TARGET_EVEX512);
	  return "vpternlogd\t{$0xFF, %g0, %g0, %g0|%g0, %g0, %g0, 0xFF}";

	case MODE_OI:
	case MODE_V4DF:
	case MODE_V8SF:
	  gcc_assert (TARGET_AVX2);
	  /* FALLTHRU */
	case MODE_TI:
	case MODE_V2DF:
	case MODE_V4SF:
	  gcc_assert (TARGET_SSE2);
	  if (EXT_REX_SSE_REG_P (operands[0]))
	    {
	      if (TARGET_AVX512VL)
		return "vpternlogd\t{$0xFF, %0, %0, %0|%0, %0, %0, 0xFF}";
	      else if (TARGET_EVEX512)
		return "vpternlogd\t{$0xFF, %g0, %g0, %g0|%g0, %g0, %g0, 0xFF}";
	      else
		gcc_unreachable ();
	    }
	  return (TARGET_AVX
		  ? "vpcmpeqd\t%0, %0, %0"
		  : "pcmpeqd\t%0, %0");

	default:
	  gcc_unreachable ();
	}
   }
  else if (vector_all_ones_zero_extend_half_operand (x, mode))
    {
      if (GET_MODE_SIZE (mode) == 64)
	{
	  gcc_assert (TARGET_AVX512F && TARGET_EVEX512);
	  return "vpcmpeqd\t%t0, %t0, %t0";
	}
      else if (GET_MODE_SIZE (mode) == 32)
	{
	  gcc_assert (TARGET_AVX);
	  return "vpcmpeqd\t%x0, %x0, %x0";
	}
      gcc_unreachable ();
    }
  else if (vector_all_ones_zero_extend_quarter_operand (x, mode))
    {
      gcc_assert (TARGET_AVX512F && TARGET_EVEX512);
      return "vpcmpeqd\t%x0, %x0, %x0";
    }

  gcc_unreachable ();
}

/* Returns true if INSN can be transformed from a memory load
   to a supported FP constant load.  */

bool
ix86_standard_x87sse_constant_load_p (const rtx_insn *insn, rtx dst)
{
  rtx src = find_constant_src (insn);

  gcc_assert (REG_P (dst));

  if (src == NULL
      || (SSE_REGNO_P (REGNO (dst))
	  && standard_sse_constant_p (src, GET_MODE (dst)) != 1)
      || (!TARGET_AVX512VL
	  && EXT_REX_SSE_REGNO_P (REGNO (dst))
	  && standard_sse_constant_p (src, GET_MODE (dst)) == 1)
      || (STACK_REGNO_P (REGNO (dst))
	   && standard_80387_constant_p (src) < 1))
    return false;

  return true;
}

/* Predicate for pre-reload splitters with associated instructions,
   which can match any time before the split1 pass (usually combine),
   then are unconditionally split in that pass and should not be
   matched again afterwards.  */

bool
ix86_pre_reload_split (void)
{
  return (can_create_pseudo_p ()
	  && !(cfun->curr_properties & PROP_rtl_split_insns));
}

/* Return the opcode of the TYPE_SSEMOV instruction.  To move from
   or to xmm16-xmm31/ymm16-ymm31 registers, we either require
   TARGET_AVX512VL or it is a register to register move which can
   be done with zmm register move. */

static const char *
ix86_get_ssemov (rtx *operands, unsigned size,
		 enum attr_mode insn_mode, machine_mode mode)
{
  char buf[128];
  bool misaligned_p = (misaligned_operand (operands[0], mode)
		       || misaligned_operand (operands[1], mode));
  bool evex_reg_p = (size == 64
		     || EXT_REX_SSE_REG_P (operands[0])
		     || EXT_REX_SSE_REG_P (operands[1]));

  bool egpr_p = (TARGET_APX_EGPR
		 && (x86_extended_rex2reg_mentioned_p (operands[0])
		     || x86_extended_rex2reg_mentioned_p (operands[1])));
  bool egpr_vl = egpr_p && TARGET_AVX512VL;

  machine_mode scalar_mode;

  const char *opcode = NULL;
  enum
    {
      opcode_int,
      opcode_float,
      opcode_double
    } type = opcode_int;

  switch (insn_mode)
    {
    case MODE_V16SF:
    case MODE_V8SF:
    case MODE_V4SF:
      scalar_mode = E_SFmode;
      type = opcode_float;
      break;
    case MODE_V8DF:
    case MODE_V4DF:
    case MODE_V2DF:
      scalar_mode = E_DFmode;
      type = opcode_double;
      break;
    case MODE_XI:
    case MODE_OI:
    case MODE_TI:
      scalar_mode = GET_MODE_INNER (mode);
      break;
    default:
      gcc_unreachable ();
    }

  /* NB: To move xmm16-xmm31/ymm16-ymm31 registers without AVX512VL,
     we can only use zmm register move without memory operand.  */
  if (evex_reg_p
      && !TARGET_AVX512VL
      && GET_MODE_SIZE (mode) < 64)
    {
      /* NB: Even though ix86_hard_regno_mode_ok doesn't allow
	 xmm16-xmm31 nor ymm16-ymm31 in 128/256 bit modes when
	 AVX512VL is disabled, LRA can still generate reg to
	 reg moves with xmm16-xmm31 and ymm16-ymm31 in 128/256 bit
	 modes.  */
      if (memory_operand (operands[0], mode)
	  || memory_operand (operands[1], mode))
	gcc_unreachable ();
      size = 64;
      /* We need TARGET_EVEX512 to move into zmm register.  */
      gcc_assert (TARGET_EVEX512);
      switch (type)
	{
	case opcode_int:
	  if (scalar_mode == E_HFmode || scalar_mode == E_BFmode)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW ? "vmovdqu16" : "vmovdqu64")
		      : "vmovdqa64");
	  else
	    opcode = misaligned_p ? "vmovdqu32" : "vmovdqa32";
	  break;
	case opcode_float:
	  opcode = misaligned_p ? "vmovups" : "vmovaps";
	  break;
	case opcode_double:
	  opcode = misaligned_p ? "vmovupd" : "vmovapd";
	  break;
	}
    }
  else if (SCALAR_FLOAT_MODE_P (scalar_mode))
    {
      switch (scalar_mode)
	{
	case E_HFmode:
	case E_BFmode:
	  if (evex_reg_p || egpr_vl)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "vmovdqu64")
		      : "vmovdqa64");
	  else if (egpr_p)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "%vmovups")
		      : "%vmovaps");
	  else
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "%vmovdqu")
		      : "%vmovdqa");
	  break;
	case E_SFmode:
	  opcode = misaligned_p ? "%vmovups" : "%vmovaps";
	  break;
	case E_DFmode:
	  opcode = misaligned_p ? "%vmovupd" : "%vmovapd";
	  break;
	case E_TFmode:
	  if (evex_reg_p || egpr_vl)
	    opcode = misaligned_p ? "vmovdqu64" : "vmovdqa64";
	  else if (egpr_p)
	    opcode = misaligned_p ? "%vmovups" : "%vmovaps";
	  else
	    opcode = misaligned_p ? "%vmovdqu" : "%vmovdqa";
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else if (SCALAR_INT_MODE_P (scalar_mode))
    {
      switch (scalar_mode)
	{
	case E_QImode:
	  if (evex_reg_p || egpr_vl)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu8"
			 : "vmovdqu64")
		      : "vmovdqa64");
	  else if (egpr_p)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu8"
			 : "%vmovups")
		      : "%vmovaps");
	  else
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu8"
			 : "%vmovdqu")
		      : "%vmovdqa");
	  break;
	case E_HImode:
	  if (evex_reg_p || egpr_vl)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "vmovdqu64")
		      : "vmovdqa64");
	  else if (egpr_p)
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "%vmovups")
		      : "%vmovaps");
	  else
	    opcode = (misaligned_p
		      ? (TARGET_AVX512BW
			 ? "vmovdqu16"
			 : "%vmovdqu")
		      : "%vmovdqa");
	  break;
	case E_SImode:
	  if (evex_reg_p || egpr_vl)
	    opcode = misaligned_p ? "vmovdqu32" : "vmovdqa32";
	  else if (egpr_p)
	    opcode = misaligned_p ? "%vmovups" : "%vmovaps";
	  else
	    opcode = misaligned_p ? "%vmovdqu" : "%vmovdqa";
	  break;
	case E_DImode:
	case E_TImode:
	case E_OImode:
	  if (evex_reg_p || egpr_vl)
	    opcode = misaligned_p ? "vmovdqu64" : "vmovdqa64";
	  else if (egpr_p)
	    opcode = misaligned_p ? "%vmovups" : "%vmovaps";
	  else
	    opcode = misaligned_p ? "%vmovdqu" : "%vmovdqa";
	  break;
	case E_XImode:
	  opcode = misaligned_p ? "vmovdqu64" : "vmovdqa64";
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    gcc_unreachable ();

  switch (size)
    {
    case 64:
      snprintf (buf, sizeof (buf), "%s\t{%%g1, %%g0|%%g0, %%g1}",
		opcode);
      break;
    case 32:
      snprintf (buf, sizeof (buf), "%s\t{%%t1, %%t0|%%t0, %%t1}",
		opcode);
      break;
    case 16:
      snprintf (buf, sizeof (buf), "%s\t{%%x1, %%x0|%%x0, %%x1}",
		opcode);
      break;
    default:
      gcc_unreachable ();
    }
  output_asm_insn (buf, operands);
  return "";
}

/* Return the template of the TYPE_SSEMOV instruction to move
   operands[1] into operands[0].  */

const char *
ix86_output_ssemov (rtx_insn *insn, rtx *operands)
{
  machine_mode mode = GET_MODE (operands[0]);
  if (get_attr_type (insn) != TYPE_SSEMOV
      || mode != GET_MODE (operands[1]))
    gcc_unreachable ();

  enum attr_mode insn_mode = get_attr_mode (insn);

  switch (insn_mode)
    {
    case MODE_XI:
    case MODE_V8DF:
    case MODE_V16SF:
      return ix86_get_ssemov (operands, 64, insn_mode, mode);

    case MODE_OI:
    case MODE_V4DF:
    case MODE_V8SF:
      return ix86_get_ssemov (operands, 32, insn_mode, mode);

    case MODE_TI:
    case MODE_V2DF:
    case MODE_V4SF:
      return ix86_get_ssemov (operands, 16, insn_mode, mode);

    case MODE_DI:
      /* Handle broken assemblers that require movd instead of movq. */
      if (GENERAL_REG_P (operands[0]))
	{
	  if (HAVE_AS_IX86_INTERUNIT_MOVQ)
	    return "%vmovq\t{%1, %q0|%q0, %1}";
	  else
	    return "%vmovd\t{%1, %q0|%q0, %1}";
	}
      else if (GENERAL_REG_P (operands[1]))
	{
	  if (HAVE_AS_IX86_INTERUNIT_MOVQ)
	    return "%vmovq\t{%q1, %0|%0, %q1}";
	  else
	    return "%vmovd\t{%q1, %0|%0, %q1}";
	}
      else
	return "%vmovq\t{%1, %0|%0, %1}";

    case MODE_SI:
      if (GENERAL_REG_P (operands[0]))
	return "%vmovd\t{%1, %k0|%k0, %1}";
      else if (GENERAL_REG_P (operands[1]))
	return "%vmovd\t{%k1, %0|%0, %k1}";
      else
	return "%vmovd\t{%1, %0|%0, %1}";

    case MODE_HI:
      if (GENERAL_REG_P (operands[0]))
	return "vmovw\t{%1, %k0|%k0, %1}";
      else if (GENERAL_REG_P (operands[1]))
	return "vmovw\t{%k1, %0|%0, %k1}";
      else
	return "vmovw\t{%1, %0|%0, %1}";

    case MODE_DF:
      if (TARGET_AVX && REG_P (operands[0]) && REG_P (operands[1]))
	return "vmovsd\t{%d1, %0|%0, %d1}";
      else
	return "%vmovsd\t{%1, %0|%0, %1}";

    case MODE_SF:
      if (TARGET_AVX && REG_P (operands[0]) && REG_P (operands[1]))
	return "vmovss\t{%d1, %0|%0, %d1}";
      else
	return "%vmovss\t{%1, %0|%0, %1}";

    case MODE_HF:
    case MODE_BF:
      if (REG_P (operands[0]) && REG_P (operands[1]))
	return "vmovsh\t{%d1, %0|%0, %d1}";
      else
	return "vmovsh\t{%1, %0|%0, %1}";

    case MODE_V1DF:
      gcc_assert (!TARGET_AVX);
      return "movlpd\t{%1, %0|%0, %1}";

    case MODE_V2SF:
      if (TARGET_AVX && REG_P (operands[0]))
	return "vmovlps\t{%1, %d0|%d0, %1}";
      else
	return "%vmovlps\t{%1, %0|%0, %1}";

    default:
      gcc_unreachable ();
    }
}

/* Returns true if OP contains a symbol reference */

bool
symbolic_reference_mentioned_p (rtx op)
{
  const char *fmt;
  int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return true;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return true;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return true;
    }

  return false;
}

/* Return true if it is appropriate to emit `ret' instructions in the
   body of a function.  Do this only if the epilogue is simple, needing a
   couple of insns.  Prior to reloading, we can't tell how many registers
   must be saved, so return false then.  Return false if there is no frame
   marker to de-allocate.  */

bool
ix86_can_use_return_insn_p (void)
{
  if (ix86_function_ms_hook_prologue (current_function_decl))
    return false;

  if (ix86_function_naked (current_function_decl))
    return false;

  /* Don't use `ret' instruction in interrupt handler.  */
  if (! reload_completed
      || frame_pointer_needed
      || cfun->machine->func_type != TYPE_NORMAL)
    return 0;

  /* Don't allow more than 32k pop, since that's all we can do
     with one instruction.  */
  if (crtl->args.pops_args && crtl->args.size >= 32768)
    return 0;

  struct ix86_frame &frame = cfun->machine->frame;
  return (frame.stack_pointer_offset == UNITS_PER_WORD
	  && (frame.nregs + frame.nsseregs) == 0);
}

/* Return stack frame size.  get_frame_size () returns used stack slots
   during compilation, which may be optimized out later.  If stack frame
   is needed, stack_frame_required should be true.  */

static HOST_WIDE_INT
ix86_get_frame_size (void)
{
  if (cfun->machine->stack_frame_required)
    return get_frame_size ();
  else
    return 0;
}

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may
   be accessed via the stack pointer) in functions that seem suitable.  */

static bool
ix86_frame_pointer_required (void)
{
  /* If we accessed previous frames, then the generated code expects
     to be able to access the saved ebp value in our frame.  */
  if (cfun->machine->accesses_prev_frame)
    return true;

  /* Several x86 os'es need a frame pointer for other reasons,
     usually pertaining to setjmp.  */
  if (SUBTARGET_FRAME_POINTER_REQUIRED)
    return true;

  /* For older 32-bit runtimes setjmp requires valid frame-pointer.  */
  if (TARGET_32BIT_MS_ABI && cfun->calls_setjmp)
    return true;

  /* Win64 SEH, very large frames need a frame-pointer as maximum stack
     allocation is 4GB.  */
  if (TARGET_64BIT_MS_ABI && ix86_get_frame_size () > SEH_MAX_FRAME_SIZE)
    return true;

  /* SSE saves require frame-pointer when stack is misaligned.  */
  if (TARGET_64BIT_MS_ABI && ix86_incoming_stack_boundary < 128)
    return true;
  
  /* In ix86_option_override_internal, TARGET_OMIT_LEAF_FRAME_POINTER
     turns off the frame pointer by default.  Turn it back on now if
     we've not got a leaf function.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER
      && (!crtl->is_leaf
	  || ix86_current_function_calls_tls_descriptor))
    return true;

  /* Several versions of mcount for the x86 assumes that there is a
     frame, so we cannot allow profiling without a frame pointer.  */
  if (crtl->profile && !flag_fentry)
    return true;

  return false;
}

/* Record that the current function accesses previous call frames.  */

void
ix86_setup_frame_addresses (void)
{
  cfun->machine->accesses_prev_frame = 1;
}

#ifndef USE_HIDDEN_LINKONCE
# if defined(HAVE_GAS_HIDDEN) && (SUPPORTS_ONE_ONLY - 0)
#  define USE_HIDDEN_LINKONCE 1
# else
#  define USE_HIDDEN_LINKONCE 0
# endif
#endif

/* Label count for call and return thunks.  It is used to make unique
   labels in call and return thunks.  */
static int indirectlabelno;

/* True if call thunk function is needed.  */
static bool indirect_thunk_needed = false;

/* Bit masks of integer registers, which contain branch target, used
   by call thunk functions.  */
static HARD_REG_SET indirect_thunks_used;

/* True if return thunk function is needed.  */
static bool indirect_return_needed = false;

/* True if return thunk function via CX is needed.  */
static bool indirect_return_via_cx;

#ifndef INDIRECT_LABEL
# define INDIRECT_LABEL "LIND"
#endif

/* Indicate what prefix is needed for an indirect branch.  */
enum indirect_thunk_prefix
{
  indirect_thunk_prefix_none,
  indirect_thunk_prefix_nt
};

/* Return the prefix needed for an indirect branch INSN.  */

enum indirect_thunk_prefix
indirect_thunk_need_prefix (rtx_insn *insn)
{
  enum indirect_thunk_prefix need_prefix;
  if ((cfun->machine->indirect_branch_type
	    == indirect_branch_thunk_extern)
	   && ix86_notrack_prefixed_insn_p (insn))
    {
      /* NOTRACK prefix is only used with external thunk so that it
	 can be properly updated to support CET at run-time.  */
      need_prefix = indirect_thunk_prefix_nt;
    }
  else
    need_prefix = indirect_thunk_prefix_none;
  return need_prefix;
}

/* Fills in the label name that should be used for the indirect thunk.  */

static void
indirect_thunk_name (char name[32], unsigned int regno,
		     enum indirect_thunk_prefix need_prefix,
		     bool ret_p)
{
  if (regno != INVALID_REGNUM && regno != CX_REG && ret_p)
    gcc_unreachable ();

  if (USE_HIDDEN_LINKONCE)
    {
      const char *prefix;

      if (need_prefix == indirect_thunk_prefix_nt
	  && regno != INVALID_REGNUM)
	{
	  /* NOTRACK prefix is only used with external thunk via
	     register so that NOTRACK prefix can be added to indirect
	     branch via register to support CET at run-time.  */
	  prefix = "_nt";
	}
      else
	prefix = "";

      const char *ret = ret_p ? "return" : "indirect";

      if (regno != INVALID_REGNUM)
	{
	  const char *reg_prefix;
	  if (LEGACY_INT_REGNO_P (regno))
	    reg_prefix = TARGET_64BIT ? "r" : "e";
	  else
	    reg_prefix = "";
	  sprintf (name, "__x86_%s_thunk%s_%s%s",
		   ret, prefix, reg_prefix, reg_names[regno]);
	}
      else
	sprintf (name, "__x86_%s_thunk%s", ret, prefix);
    }
  else
    {
      if (regno != INVALID_REGNUM)
	ASM_GENERATE_INTERNAL_LABEL (name, "LITR", regno);
      else
	{
	  if (ret_p)
	    ASM_GENERATE_INTERNAL_LABEL (name, "LRT", 0);
	  else
	    ASM_GENERATE_INTERNAL_LABEL (name, "LIT", 0);
	}
    }
}

/* Output a call and return thunk for indirect branch.  If REGNO != -1,
   the function address is in REGNO and the call and return thunk looks like:

	call	L2
   L1:
	pause
	lfence
	jmp	L1
   L2:
	mov	%REG, (%sp)
	ret

   Otherwise, the function address is on the top of stack and the
   call and return thunk looks like:

	call L2
  L1:
	pause
	lfence
	jmp L1
  L2:
	lea WORD_SIZE(%sp), %sp
	ret
 */

static void
output_indirect_thunk (unsigned int regno)
{
  char indirectlabel1[32];
  char indirectlabel2[32];

  ASM_GENERATE_INTERNAL_LABEL (indirectlabel1, INDIRECT_LABEL,
			       indirectlabelno++);
  ASM_GENERATE_INTERNAL_LABEL (indirectlabel2, INDIRECT_LABEL,
			       indirectlabelno++);

  /* Call */
  fputs ("\tcall\t", asm_out_file);
  assemble_name_raw (asm_out_file, indirectlabel2);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel1);

  /* AMD and Intel CPUs prefer each a different instruction as loop filler.
     Usage of both pause + lfence is compromise solution.  */
  fprintf (asm_out_file, "\tpause\n\tlfence\n");

  /* Jump.  */
  fputs ("\tjmp\t", asm_out_file);
  assemble_name_raw (asm_out_file, indirectlabel1);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel2);

  /* The above call insn pushed a word to stack.  Adjust CFI info.  */
  if (flag_asynchronous_unwind_tables && dwarf2out_do_frame ())
    {
      if (! dwarf2out_do_cfi_asm ())
	{
	  dw_cfi_ref xcfi = ggc_cleared_alloc<dw_cfi_node> ();
	  xcfi->dw_cfi_opc = DW_CFA_advance_loc4;
	  xcfi->dw_cfi_oprnd1.dw_cfi_addr = ggc_strdup (indirectlabel2);
	  vec_safe_push (cfun->fde->dw_fde_cfi, xcfi);
	}
      dw_cfi_ref xcfi = ggc_cleared_alloc<dw_cfi_node> ();
      xcfi->dw_cfi_opc = DW_CFA_def_cfa_offset;
      xcfi->dw_cfi_oprnd1.dw_cfi_offset = 2 * UNITS_PER_WORD;
      vec_safe_push (cfun->fde->dw_fde_cfi, xcfi);
      dwarf2out_emit_cfi (xcfi);
    }

  if (regno != INVALID_REGNUM)
    {
      /* MOV.  */
      rtx xops[2];
      xops[0] = gen_rtx_MEM (word_mode, stack_pointer_rtx);
      xops[1] = gen_rtx_REG (word_mode, regno);
      output_asm_insn ("mov\t{%1, %0|%0, %1}", xops);
    }
  else
    {
      /* LEA.  */
      rtx xops[2];
      xops[0] = stack_pointer_rtx;
      xops[1] = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
      output_asm_insn ("lea\t{%E1, %0|%0, %E1}", xops);
    }

  fputs ("\tret\n", asm_out_file);
  if ((ix86_harden_sls & harden_sls_return))
    fputs ("\tint3\n", asm_out_file);
}

/* Output a funtion with a call and return thunk for indirect branch.
   If REGNO != INVALID_REGNUM, the function address is in REGNO.
   Otherwise, the function address is on the top of stack.  Thunk is
   used for function return if RET_P is true.  */

static void
output_indirect_thunk_function (enum indirect_thunk_prefix need_prefix,
				unsigned int regno, bool ret_p)
{
  char name[32];
  tree decl;

  /* Create __x86_indirect_thunk.  */
  indirect_thunk_name (name, regno, need_prefix, ret_p);
  decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
		     get_identifier (name),
		     build_function_type_list (void_type_node, NULL_TREE));
  DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
				   NULL_TREE, void_type_node);
  TREE_PUBLIC (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_IGNORED_P (decl) = 1;

#if TARGET_MACHO
  if (TARGET_MACHO)
    {
      switch_to_section (darwin_sections[picbase_thunk_section]);
      fputs ("\t.weak_definition\t", asm_out_file);
      assemble_name (asm_out_file, name);
      fputs ("\n\t.private_extern\t", asm_out_file);
      assemble_name (asm_out_file, name);
      putc ('\n', asm_out_file);
      ASM_OUTPUT_LABEL (asm_out_file, name);
      DECL_WEAK (decl) = 1;
    }
  else
#endif
    if (USE_HIDDEN_LINKONCE)
      {
	cgraph_node::create (decl)->set_comdat_group (DECL_ASSEMBLER_NAME (decl));

	targetm.asm_out.unique_section (decl, 0);
	switch_to_section (get_named_section (decl, NULL, 0));

	targetm.asm_out.globalize_label (asm_out_file, name);
	fputs ("\t.hidden\t", asm_out_file);
	assemble_name (asm_out_file, name);
	putc ('\n', asm_out_file);
	ASM_DECLARE_FUNCTION_NAME (asm_out_file, name, decl);
      }
    else
      {
	switch_to_section (text_section);
	ASM_OUTPUT_LABEL (asm_out_file, name);
      }

  DECL_INITIAL (decl) = make_node (BLOCK);
  current_function_decl = decl;
  allocate_struct_function (decl, false);
  init_function_start (decl);
  /* We're about to hide the function body from callees of final_* by
     emitting it directly; tell them we're a thunk, if they care.  */
  cfun->is_thunk = true;
  first_function_block_is_cold = false;
  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), asm_out_file, 1);

  output_indirect_thunk (regno);

  final_end_function ();
  init_insn_lengths ();
  free_after_compilation (cfun);
  set_cfun (NULL);
  current_function_decl = NULL;
}

static int pic_labels_used;

/* Fills in the label name that should be used for a pc thunk for
   the given register.  */

static void
get_pc_thunk_name (char name[32], unsigned int regno)
{
  gcc_assert (!TARGET_64BIT);

  if (USE_HIDDEN_LINKONCE)
    sprintf (name, "__x86.get_pc_thunk.%s", reg_names[regno]);
  else
    ASM_GENERATE_INTERNAL_LABEL (name, "LPR", regno);
}


/* This function generates code for -fpic that loads %ebx with
   the return address of the caller and then returns.  */

static void
ix86_code_end (void)
{
  rtx xops[2];
  unsigned int regno;

  if (indirect_return_needed)
    output_indirect_thunk_function (indirect_thunk_prefix_none,
				    INVALID_REGNUM, true);
  if (indirect_return_via_cx)
    output_indirect_thunk_function (indirect_thunk_prefix_none,
				    CX_REG, true);
  if (indirect_thunk_needed)
    output_indirect_thunk_function (indirect_thunk_prefix_none,
				    INVALID_REGNUM, false);

  for (regno = FIRST_REX_INT_REG; regno <= LAST_REX_INT_REG; regno++)
    {
      if (TEST_HARD_REG_BIT (indirect_thunks_used, regno))
	output_indirect_thunk_function (indirect_thunk_prefix_none,
					regno, false);
    }

  for (regno = FIRST_REX2_INT_REG; regno <= LAST_REX2_INT_REG; regno++)
    {
      if (TEST_HARD_REG_BIT (indirect_thunks_used, regno))
	output_indirect_thunk_function (indirect_thunk_prefix_none,
					regno, false);
    }

  for (regno = FIRST_INT_REG; regno <= LAST_INT_REG; regno++)
    {
      char name[32];
      tree decl;

      if (TEST_HARD_REG_BIT (indirect_thunks_used, regno))
	output_indirect_thunk_function (indirect_thunk_prefix_none,
					regno, false);

      if (!(pic_labels_used & (1 << regno)))
	continue;

      get_pc_thunk_name (name, regno);

      decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			 get_identifier (name),
			 build_function_type_list (void_type_node, NULL_TREE));
      DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
				       NULL_TREE, void_type_node);
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
      DECL_IGNORED_P (decl) = 1;

#if TARGET_MACHO
      if (TARGET_MACHO)
	{
	  switch_to_section (darwin_sections[picbase_thunk_section]);
	  fputs ("\t.weak_definition\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  fputs ("\n\t.private_extern\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  putc ('\n', asm_out_file);
	  ASM_OUTPUT_LABEL (asm_out_file, name);
	  DECL_WEAK (decl) = 1;
	}
      else
#endif
      if (USE_HIDDEN_LINKONCE)
	{
	  cgraph_node::create (decl)->set_comdat_group (DECL_ASSEMBLER_NAME (decl));

	  targetm.asm_out.unique_section (decl, 0);
	  switch_to_section (get_named_section (decl, NULL, 0));

	  targetm.asm_out.globalize_label (asm_out_file, name);
	  fputs ("\t.hidden\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  putc ('\n', asm_out_file);
	  ASM_DECLARE_FUNCTION_NAME (asm_out_file, name, decl);
	}
      else
	{
	  switch_to_section (text_section);
	  ASM_OUTPUT_LABEL (asm_out_file, name);
	}

      DECL_INITIAL (decl) = make_node (BLOCK);
      current_function_decl = decl;
      allocate_struct_function (decl, false);
      init_function_start (decl);
      /* We're about to hide the function body from callees of final_* by
	 emitting it directly; tell them we're a thunk, if they care.  */
      cfun->is_thunk = true;
      first_function_block_is_cold = false;
      /* Make sure unwind info is emitted for the thunk if needed.  */
      final_start_function (emit_barrier (), asm_out_file, 1);

      /* Pad stack IP move with 4 instructions (two NOPs count
	 as one instruction).  */
      if (TARGET_PAD_SHORT_FUNCTION)
	{
	  int i = 8;

	  while (i--)
	    fputs ("\tnop\n", asm_out_file);
	}

      xops[0] = gen_rtx_REG (Pmode, regno);
      xops[1] = gen_rtx_MEM (Pmode, stack_pointer_rtx);
      output_asm_insn ("mov%z0\t{%1, %0|%0, %1}", xops);
      fputs ("\tret\n", asm_out_file);
      final_end_function ();
      init_insn_lengths ();
      free_after_compilation (cfun);
      set_cfun (NULL);
      current_function_decl = NULL;
    }

  if (flag_split_stack)
    file_end_indicate_split_stack ();
}

/* Emit code for the SET_GOT patterns.  */

const char *
output_set_got (rtx dest, rtx label)
{
  rtx xops[3];

  xops[0] = dest;

  if (TARGET_VXWORKS_RTP && flag_pic)
    {
      /* Load (*VXWORKS_GOTT_BASE) into the PIC register.  */
      xops[2] = gen_rtx_MEM (Pmode,
			     gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_BASE));
      output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);

      /* Load (*VXWORKS_GOTT_BASE)[VXWORKS_GOTT_INDEX] into the PIC register.
	 Use %P and a local symbol in order to print VXWORKS_GOTT_INDEX as
	 an unadorned address.  */
      xops[2] = gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_INDEX);
      SYMBOL_REF_FLAGS (xops[2]) |= SYMBOL_FLAG_LOCAL;
      output_asm_insn ("mov{l}\t{%P2(%0), %0|%0, DWORD PTR %P2[%0]}", xops);
      return "";
    }

  xops[1] = gen_rtx_SYMBOL_REF (Pmode, GOT_SYMBOL_NAME);

  if (flag_pic)
    {
      char name[32];
      get_pc_thunk_name (name, REGNO (dest));
      pic_labels_used |= 1 << REGNO (dest);

      xops[2] = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
      xops[2] = gen_rtx_MEM (QImode, xops[2]);
      output_asm_insn ("%!call\t%X2", xops);

#if TARGET_MACHO
      /* Output the Mach-O "canonical" pic base label name ("Lxx$pb") here.
         This is what will be referenced by the Mach-O PIC subsystem.  */
      if (machopic_should_output_picbase_label () || !label)
	ASM_OUTPUT_LABEL (asm_out_file, MACHOPIC_FUNCTION_BASE_NAME);

      /* When we are restoring the pic base at the site of a nonlocal label,
         and we decided to emit the pic base above, we will still output a
         local label used for calculating the correction offset (even though
         the offset will be 0 in that case).  */
      if (label)
        targetm.asm_out.internal_label (asm_out_file, "L",
					   CODE_LABEL_NUMBER (label));
#endif
    }
  else
    {
      if (TARGET_MACHO)
	/* We don't need a pic base, we're not producing pic.  */
	gcc_unreachable ();

      xops[2] = gen_rtx_LABEL_REF (Pmode, label ? label : gen_label_rtx ());
      output_asm_insn ("mov%z0\t{%2, %0|%0, %2}", xops);
      targetm.asm_out.internal_label (asm_out_file, "L",
				      CODE_LABEL_NUMBER (XEXP (xops[2], 0)));
    }

  if (!TARGET_MACHO)
    output_asm_insn ("add%z0\t{%1, %0|%0, %1}", xops);

  return "";
}

/* Generate an "push" pattern for input ARG.  */

rtx
gen_push (rtx arg, bool ppx_p)
{
  struct machine_function *m = cfun->machine;

  if (m->fs.cfa_reg == stack_pointer_rtx)
    m->fs.cfa_offset += UNITS_PER_WORD;
  m->fs.sp_offset += UNITS_PER_WORD;

  if (REG_P (arg) && GET_MODE (arg) != word_mode)
    arg = gen_rtx_REG (word_mode, REGNO (arg));

  rtx stack = gen_rtx_MEM (word_mode,
			   gen_rtx_PRE_DEC (Pmode,
					    stack_pointer_rtx));
  return ppx_p ? gen_pushp_di (stack, arg) : gen_rtx_SET (stack, arg);
}

rtx
gen_pushfl (void)
{
  struct machine_function *m = cfun->machine;
  rtx flags, mem;

  if (m->fs.cfa_reg == stack_pointer_rtx)
    m->fs.cfa_offset += UNITS_PER_WORD;
  m->fs.sp_offset += UNITS_PER_WORD;

  flags = gen_rtx_REG (CCmode, FLAGS_REG);

  mem = gen_rtx_MEM (word_mode,
		     gen_rtx_PRE_DEC (Pmode, stack_pointer_rtx));

  return gen_pushfl2 (word_mode, mem, flags);
}

/* Generate an "pop" pattern for input ARG.  */

rtx
gen_pop (rtx arg, bool ppx_p)
{
  if (REG_P (arg) && GET_MODE (arg) != word_mode)
    arg = gen_rtx_REG (word_mode, REGNO (arg));

  rtx stack = gen_rtx_MEM (word_mode,
			   gen_rtx_POST_INC (Pmode,
					     stack_pointer_rtx));

  return ppx_p ? gen_popp_di (arg, stack) : gen_rtx_SET (arg, stack);
}

rtx
gen_popfl (void)
{
  rtx flags, mem;

  flags = gen_rtx_REG (CCmode, FLAGS_REG);

  mem = gen_rtx_MEM (word_mode,
		     gen_rtx_POST_INC (Pmode, stack_pointer_rtx));

  return gen_popfl1 (word_mode, flags, mem);
}

/* Generate a "push2" pattern for input ARG.  */
rtx
gen_push2 (rtx mem, rtx reg1, rtx reg2, bool ppx_p = false)
{
  struct machine_function *m = cfun->machine;
  const int offset = UNITS_PER_WORD * 2;

  if (m->fs.cfa_reg == stack_pointer_rtx)
    m->fs.cfa_offset += offset;
  m->fs.sp_offset += offset;

  if (REG_P (reg1) && GET_MODE (reg1) != word_mode)
    reg1 = gen_rtx_REG (word_mode, REGNO (reg1));

  if (REG_P (reg2) && GET_MODE (reg2) != word_mode)
    reg2 = gen_rtx_REG (word_mode, REGNO (reg2));

  return ppx_p ? gen_push2p_di (mem, reg1, reg2):
		 gen_push2_di (mem, reg1, reg2);
}

/* Return >= 0 if there is an unused call-clobbered register available
   for the entire function.  */

static unsigned int
ix86_select_alt_pic_regnum (void)
{
  if (ix86_use_pseudo_pic_reg ())
    return INVALID_REGNUM;

  if (crtl->is_leaf
      && !crtl->profile
      && !ix86_current_function_calls_tls_descriptor)
    {
      int i, drap;
      /* Can't use the same register for both PIC and DRAP.  */
      if (crtl->drap_reg)
	drap = REGNO (crtl->drap_reg);
      else
	drap = -1;
      for (i = 2; i >= 0; --i)
        if (i != drap && !df_regs_ever_live_p (i))
	  return i;
    }

  return INVALID_REGNUM;
}

/* Return true if REGNO is used by the epilogue.  */

bool
ix86_epilogue_uses (int regno)
{
  /* If there are no caller-saved registers, we preserve all registers,
     except for MMX and x87 registers which aren't supported when saving
     and restoring registers.  Don't explicitly save SP register since
     it is always preserved.  */
  return (epilogue_completed
	  && (cfun->machine->call_saved_registers
	      == TYPE_NO_CALLER_SAVED_REGISTERS)
	  && !fixed_regs[regno]
	  && !STACK_REGNO_P (regno)
	  && !MMX_REGNO_P (regno));
}

/* Return nonzero if register REGNO can be used as a scratch register
   in peephole2.  */

static bool
ix86_hard_regno_scratch_ok (unsigned int regno)
{
  /* If there are no caller-saved registers, we can't use any register
     as a scratch register after epilogue and use REGNO as scratch
     register only if it has been used before to avoid saving and
     restoring it.  */
  return ((cfun->machine->call_saved_registers
	   != TYPE_NO_CALLER_SAVED_REGISTERS)
	  || (!epilogue_completed
	      && df_regs_ever_live_p (regno)));
}

/* Return TRUE if we need to save REGNO.  */

bool
ix86_save_reg (unsigned int regno, bool maybe_eh_return, bool ignore_outlined)
{
  rtx reg;

  switch (cfun->machine->call_saved_registers)
    {
    case TYPE_DEFAULT_CALL_SAVED_REGISTERS:
      break;

    case TYPE_NO_CALLER_SAVED_REGISTERS:
      /* If there are no caller-saved registers, we preserve all
	 registers, except for MMX and x87 registers which aren't
	 supported when saving and restoring registers.  Don't
	 explicitly save SP register since it is always preserved.

	 Don't preserve registers used for function return value.  */
      reg = crtl->return_rtx;
      if (reg)
	{
	  unsigned int i = REGNO (reg);
	  unsigned int nregs = REG_NREGS (reg);
	  while (nregs-- > 0)
	    if ((i + nregs) == regno)
	      return false;
	}

      return (df_regs_ever_live_p (regno)
	      && !fixed_regs[regno]
	      && !STACK_REGNO_P (regno)
	      && !MMX_REGNO_P (regno)
	      && (regno != HARD_FRAME_POINTER_REGNUM
		  || !frame_pointer_needed));

    case TYPE_NO_CALLEE_SAVED_REGISTERS:
      return false;

    case TYPE_NO_CALLEE_SAVED_REGISTERS_EXCEPT_BP:
      if (regno != HARD_FRAME_POINTER_REGNUM)
	return false;
      break;
    }

  if (regno == REAL_PIC_OFFSET_TABLE_REGNUM
      && pic_offset_table_rtx)
    {
      if (ix86_use_pseudo_pic_reg ())
	{
	  /* REAL_PIC_OFFSET_TABLE_REGNUM used by call to
	  _mcount in prologue.  */
	  if (!TARGET_64BIT && flag_pic && crtl->profile)
	    return true;
	}
      else if (df_regs_ever_live_p (REAL_PIC_OFFSET_TABLE_REGNUM)
	       || crtl->profile
	       || crtl->calls_eh_return
	       || crtl->uses_const_pool
	       || cfun->has_nonlocal_label)
        return ix86_select_alt_pic_regnum () == INVALID_REGNUM;
    }

  if (crtl->calls_eh_return && maybe_eh_return)
    {
      unsigned i;
      for (i = 0; ; i++)
	{
	  unsigned test = EH_RETURN_DATA_REGNO (i);
	  if (test == INVALID_REGNUM)
	    break;
	  if (test == regno)
	    return true;
	}
    }

  if (ignore_outlined && cfun->machine->call_ms2sysv)
    {
      unsigned count = cfun->machine->call_ms2sysv_extra_regs
		       + xlogue_layout::MIN_REGS;
      if (xlogue_layout::is_stub_managed_reg (regno, count))
	return false;
    }

  if (crtl->drap_reg
      && regno == REGNO (crtl->drap_reg)
      && !cfun->machine->no_drap_save_restore)
    return true;

  return (df_regs_ever_live_p (regno)
	  && !call_used_or_fixed_reg_p (regno)
	  && (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed));
}

/* Return number of saved general prupose registers.  */

static int
ix86_nsaved_regs (void)
{
  int nregs = 0;
  int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, true, true))
      nregs ++;
  return nregs;
}

/* Return number of saved SSE registers.  */

static int
ix86_nsaved_sseregs (void)
{
  int nregs = 0;
  int regno;

  if (!TARGET_64BIT_MS_ABI)
    return 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (SSE_REGNO_P (regno) && ix86_save_reg (regno, true, true))
      nregs ++;
  return nregs;
}

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  If stack alignment is needed, we can only replace argument
   pointer with hard frame pointer, or replace frame pointer with stack
   pointer.  Otherwise, frame pointer elimination is automatically
   handled and all other eliminations are valid.  */

static bool
ix86_can_eliminate (const int from, const int to)
{
  if (stack_realign_fp)
    return ((from == ARG_POINTER_REGNUM
	     && to == HARD_FRAME_POINTER_REGNUM)
	    || (from == FRAME_POINTER_REGNUM
		&& to == STACK_POINTER_REGNUM));
  else
    return to == STACK_POINTER_REGNUM ? !frame_pointer_needed : true;
}

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

HOST_WIDE_INT
ix86_initial_elimination_offset (int from, int to)
{
  struct ix86_frame &frame = cfun->machine->frame;

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return frame.hard_frame_pointer_offset;
  else if (from == FRAME_POINTER_REGNUM
	   && to == HARD_FRAME_POINTER_REGNUM)
    return frame.hard_frame_pointer_offset - frame.frame_pointer_offset;
  else
    {
      gcc_assert (to == STACK_POINTER_REGNUM);

      if (from == ARG_POINTER_REGNUM)
	return frame.stack_pointer_offset;

      gcc_assert (from == FRAME_POINTER_REGNUM);
      return frame.stack_pointer_offset - frame.frame_pointer_offset;
    }
}

/* Emits a warning for unsupported msabi to sysv pro/epilogues.  */
void
warn_once_call_ms2sysv_xlogues (const char *feature)
{
  static bool warned_once = false;
  if (!warned_once)
    {
      warning (0, "%<-mcall-ms2sysv-xlogues%> is not compatible with %s",
	       feature);
      warned_once = true;
    }
}

/* Return the probing interval for -fstack-clash-protection.  */

static HOST_WIDE_INT
get_probe_interval (void)
{
  if (flag_stack_clash_protection)
    return (HOST_WIDE_INT_1U
	    << param_stack_clash_protection_probe_interval);
  else
    return (HOST_WIDE_INT_1U << STACK_CHECK_PROBE_INTERVAL_EXP);
}

/* When using -fsplit-stack, the allocation routines set a field in
   the TCB to the bottom of the stack plus this much space, measured
   in bytes.  */

#define SPLIT_STACK_AVAILABLE 256

/* Return true if push2/pop2 can be generated.  */

static bool
ix86_can_use_push2pop2 (void)
{
  /* Use push2/pop2 only if the incoming stack is 16-byte aligned.  */
  unsigned int incoming_stack_boundary
    = (crtl->parm_stack_boundary > ix86_incoming_stack_boundary
       ? crtl->parm_stack_boundary : ix86_incoming_stack_boundary);
  return incoming_stack_boundary % 128 == 0;
}

/* Helper function to determine whether push2/pop2 can be used in prologue or
   epilogue for register save/restore.  */
static bool
ix86_pro_and_epilogue_can_use_push2pop2 (int nregs)
{
  if (!ix86_can_use_push2pop2 ())
    return false;
  int aligned = cfun->machine->fs.sp_offset % 16 == 0;
  return TARGET_APX_PUSH2POP2
	 && !cfun->machine->frame.save_regs_using_mov
	 && cfun->machine->func_type == TYPE_NORMAL
	 && (nregs + aligned) >= 3;
}

/* Fill structure ix86_frame about frame of currently computed function.  */

static void
ix86_compute_frame_layout (void)
{
  struct ix86_frame *frame = &cfun->machine->frame;
  struct machine_function *m = cfun->machine;
  unsigned HOST_WIDE_INT stack_alignment_needed;
  HOST_WIDE_INT offset;
  unsigned HOST_WIDE_INT preferred_alignment;
  HOST_WIDE_INT size = ix86_get_frame_size ();
  HOST_WIDE_INT to_allocate;

  /* m->call_ms2sysv is initially enabled in ix86_expand_call for all 64-bit
   * ms_abi functions that call a sysv function.  We now need to prune away
   * cases where it should be disabled.  */
  if (TARGET_64BIT && m->call_ms2sysv)
    {
      gcc_assert (TARGET_64BIT_MS_ABI);
      gcc_assert (TARGET_CALL_MS2SYSV_XLOGUES);
      gcc_assert (!TARGET_SEH);
      gcc_assert (TARGET_SSE);
      gcc_assert (!ix86_using_red_zone ());

      if (crtl->calls_eh_return)
	{
	  gcc_assert (!reload_completed);
	  m->call_ms2sysv = false;
	  warn_once_call_ms2sysv_xlogues ("__builtin_eh_return");
	}

      else if (ix86_static_chain_on_stack)
	{
	  gcc_assert (!reload_completed);
	  m->call_ms2sysv = false;
	  warn_once_call_ms2sysv_xlogues ("static call chains");
	}

      /* Finally, compute which registers the stub will manage.  */
      else
	{
	  unsigned count = xlogue_layout::count_stub_managed_regs ();
	  m->call_ms2sysv_extra_regs = count - xlogue_layout::MIN_REGS;
	  m->call_ms2sysv_pad_in = 0;
	}
    }

  frame->nregs = ix86_nsaved_regs ();
  frame->nsseregs = ix86_nsaved_sseregs ();

  /* 64-bit MS ABI seem to require stack alignment to be always 16,
     except for function prologues, leaf functions and when the defult
     incoming stack boundary is overriden at command line or via
     force_align_arg_pointer attribute.

     Darwin's ABI specifies 128b alignment for both 32 and  64 bit variants
     at call sites, including profile function calls.

     For APX push2/pop2, the stack also requires 128b alignment.  */
  if ((ix86_pro_and_epilogue_can_use_push2pop2 (frame->nregs)
       && crtl->preferred_stack_boundary < 128)
      || (((TARGET_64BIT_MS_ABI || TARGET_MACHO)
	   && crtl->preferred_stack_boundary < 128)
	  && (!crtl->is_leaf || cfun->calls_alloca != 0
	      || ix86_current_function_calls_tls_descriptor
	      || (TARGET_MACHO && crtl->profile)
	      || ix86_incoming_stack_boundary < 128)))
    {
      crtl->preferred_stack_boundary = 128;
      if (crtl->stack_alignment_needed < 128)
	crtl->stack_alignment_needed = 128;
    }

  stack_alignment_needed = crtl->stack_alignment_needed / BITS_PER_UNIT;
  preferred_alignment = crtl->preferred_stack_boundary / BITS_PER_UNIT;

  gcc_assert (!size || stack_alignment_needed);
  gcc_assert (preferred_alignment >= STACK_BOUNDARY / BITS_PER_UNIT);
  gcc_assert (preferred_alignment <= stack_alignment_needed);

  /* The only ABI saving SSE regs should be 64-bit ms_abi.  */
  gcc_assert (TARGET_64BIT || !frame->nsseregs);
  if (TARGET_64BIT && m->call_ms2sysv)
    {
      gcc_assert (stack_alignment_needed >= 16);
      gcc_assert (!frame->nsseregs);
    }

  /* For SEH we have to limit the amount of code movement into the prologue.
     At present we do this via a BLOCKAGE, at which point there's very little
     scheduling that can be done, which means that there's very little point
     in doing anything except PUSHs.  */
  if (TARGET_SEH)
    m->use_fast_prologue_epilogue = false;
  else if (!optimize_bb_for_size_p (ENTRY_BLOCK_PTR_FOR_FN (cfun)))
    {
      int count = frame->nregs;
      struct cgraph_node *node = cgraph_node::get (current_function_decl);

      /* The fast prologue uses move instead of push to save registers.  This
         is significantly longer, but also executes faster as modern hardware
         can execute the moves in parallel, but can't do that for push/pop.

	 Be careful about choosing what prologue to emit:  When function takes
	 many instructions to execute we may use slow version as well as in
	 case function is known to be outside hot spot (this is known with
	 feedback only).  Weight the size of function by number of registers
	 to save as it is cheap to use one or two push instructions but very
	 slow to use many of them.

	 Calling this hook multiple times with the same frame requirements
	 must produce the same layout, since the RA might otherwise be
	 unable to reach a fixed point or might fail its final sanity checks.
	 This means that once we've assumed that a function does or doesn't
	 have a particular size, we have to stick to that assumption
	 regardless of how the function has changed since.  */
      if (count)
	count = (count - 1) * FAST_PROLOGUE_INSN_COUNT;
      if (node->frequency < NODE_FREQUENCY_NORMAL
	  || (flag_branch_probabilities
	      && node->frequency < NODE_FREQUENCY_HOT))
	m->use_fast_prologue_epilogue = false;
      else
	{
	  if (count != frame->expensive_count)
	    {
	      frame->expensive_count = count;
	      frame->expensive_p = expensive_function_p (count);
	    }
	  m->use_fast_prologue_epilogue = !frame->expensive_p;
	}
    }

  frame->save_regs_using_mov
    = TARGET_PROLOGUE_USING_MOVE && m->use_fast_prologue_epilogue;

  /* Skip return address and error code in exception handler.  */
  offset = INCOMING_FRAME_SP_OFFSET;

  /* Skip pushed static chain.  */
  if (ix86_static_chain_on_stack)
    offset += UNITS_PER_WORD;

  /* Skip saved base pointer.  */
  if (frame_pointer_needed)
    offset += UNITS_PER_WORD;
  frame->hfp_save_offset = offset;

  /* The traditional frame pointer location is at the top of the frame.  */
  frame->hard_frame_pointer_offset = offset;

  /* Register save area */
  offset += frame->nregs * UNITS_PER_WORD;
  frame->reg_save_offset = offset;

  /* Calculate the size of the va-arg area (not including padding, if any).  */
  frame->va_arg_size = ix86_varargs_gpr_size + ix86_varargs_fpr_size;

  /* Also adjust stack_realign_offset for the largest alignment of
     stack slot actually used.  */
  if (stack_realign_fp
      || (cfun->machine->max_used_stack_alignment != 0
	  && (offset % cfun->machine->max_used_stack_alignment) != 0))
    {
      /* We may need a 16-byte aligned stack for the remainder of the
	 register save area, but the stack frame for the local function
	 may require a greater alignment if using AVX/2/512.  In order
	 to avoid wasting space, we first calculate the space needed for
	 the rest of the register saves, add that to the stack pointer,
	 and then realign the stack to the boundary of the start of the
	 frame for the local function.  */
      HOST_WIDE_INT space_needed = 0;
      HOST_WIDE_INT sse_reg_space_needed = 0;

      if (TARGET_64BIT)
	{
	  if (m->call_ms2sysv)
	    {
	      m->call_ms2sysv_pad_in = 0;
	      space_needed = xlogue_layout::get_instance ().get_stack_space_used ();
	    }

	  else if (frame->nsseregs)
	    /* The only ABI that has saved SSE registers (Win64) also has a
	       16-byte aligned default stack.  However, many programs violate
	       the ABI, and Wine64 forces stack realignment to compensate.  */
	    space_needed = frame->nsseregs * 16;

	  sse_reg_space_needed = space_needed = ROUND_UP (space_needed, 16);

	  /* 64-bit frame->va_arg_size should always be a multiple of 16, but
	     rounding to be pedantic.  */
	  space_needed = ROUND_UP (space_needed + frame->va_arg_size, 16);
	}
      else
	space_needed = frame->va_arg_size;

      /* Record the allocation size required prior to the realignment AND.  */
      frame->stack_realign_allocate = space_needed;

      /* The re-aligned stack starts at frame->stack_realign_offset.  Values
	 before this point are not directly comparable with values below
	 this point.  Use sp_valid_at to determine if the stack pointer is
	 valid for a given offset, fp_valid_at for the frame pointer, or
	 choose_baseaddr to have a base register chosen for you.

	 Note that the result of (frame->stack_realign_offset
	 & (stack_alignment_needed - 1)) may not equal zero.  */
      offset = ROUND_UP (offset + space_needed, stack_alignment_needed);
      frame->stack_realign_offset = offset - space_needed;
      frame->sse_reg_save_offset = frame->stack_realign_offset
							+ sse_reg_space_needed;
    }
  else
    {
      frame->stack_realign_offset = offset;

      if (TARGET_64BIT && m->call_ms2sysv)
	{
	  m->call_ms2sysv_pad_in = !!(offset & UNITS_PER_WORD);
	  offset += xlogue_layout::get_instance ().get_stack_space_used ();
	}

      /* Align and set SSE register save area.  */
      else if (frame->nsseregs)
	{
	  /* If the incoming stack boundary is at least 16 bytes, or DRAP is
	     required and the DRAP re-alignment boundary is at least 16 bytes,
	     then we want the SSE register save area properly aligned.  */
	  if (ix86_incoming_stack_boundary >= 128
		  || (stack_realign_drap && stack_alignment_needed >= 16))
	    offset = ROUND_UP (offset, 16);
	  offset += frame->nsseregs * 16;
	}
      frame->sse_reg_save_offset = offset;
      offset += frame->va_arg_size;
    }

  /* Align start of frame for local function.  When a function call
     is removed, it may become a leaf function.  But if argument may
     be passed on stack, we need to align the stack when there is no
     tail call.  */
  if (m->call_ms2sysv
      || frame->va_arg_size != 0
      || size != 0
      || !crtl->is_leaf
      || (!crtl->tail_call_emit
	  && cfun->machine->outgoing_args_on_stack)
      || cfun->calls_alloca
      || ix86_current_function_calls_tls_descriptor)
    offset = ROUND_UP (offset, stack_alignment_needed);

  /* Frame pointer points here.  */
  frame->frame_pointer_offset = offset;

  offset += size;

  /* Add outgoing arguments area.  Can be skipped if we eliminated
     all the function calls as dead code.
     Skipping is however impossible when function calls alloca.  Alloca
     expander assumes that last crtl->outgoing_args_size
     of stack frame are unused.  */
  if (ACCUMULATE_OUTGOING_ARGS
      && (!crtl->is_leaf || cfun->calls_alloca
	  || ix86_current_function_calls_tls_descriptor))
    {
      offset += crtl->outgoing_args_size;
      frame->outgoing_arguments_size = crtl->outgoing_args_size;
    }
  else
    frame->outgoing_arguments_size = 0;

  /* Align stack boundary.  Only needed if we're calling another function
     or using alloca.  */
  if (!crtl->is_leaf || cfun->calls_alloca
      || ix86_current_function_calls_tls_descriptor)
    offset = ROUND_UP (offset, preferred_alignment);

  /* We've reached end of stack frame.  */
  frame->stack_pointer_offset = offset;

  /* Size prologue needs to allocate.  */
  to_allocate = offset - frame->sse_reg_save_offset;

  if ((!to_allocate && frame->nregs <= 1)
      || (TARGET_64BIT && to_allocate >= HOST_WIDE_INT_C (0x80000000))
       /* If static stack checking is enabled and done with probes,
	  the registers need to be saved before allocating the frame.  */
      || flag_stack_check == STATIC_BUILTIN_STACK_CHECK
      /* If stack clash probing needs a loop, then it needs a
	 scratch register.  But the returned register is only guaranteed
	 to be safe to use after register saves are complete.  So if
	 stack clash protections are enabled and the allocated frame is
	 larger than the probe interval, then use pushes to save
	 callee saved registers.  */
      || (flag_stack_clash_protection
	  && !ix86_target_stack_probe ()
	  && to_allocate > get_probe_interval ()))
    frame->save_regs_using_mov = false;

  if (ix86_using_red_zone ()
      && crtl->sp_is_unchanging
      && crtl->is_leaf
      && !ix86_pc_thunk_call_expanded
      && !ix86_current_function_calls_tls_descriptor)
    {
      frame->red_zone_size = to_allocate;
      if (frame->save_regs_using_mov)
	frame->red_zone_size += frame->nregs * UNITS_PER_WORD;
      if (frame->red_zone_size > RED_ZONE_SIZE - RED_ZONE_RESERVE)
	frame->red_zone_size = RED_ZONE_SIZE - RED_ZONE_RESERVE;
    }
  else
    frame->red_zone_size = 0;
  frame->stack_pointer_offset -= frame->red_zone_size;

  /* The SEH frame pointer location is near the bottom of the frame.
     This is enforced by the fact that the difference between the
     stack pointer and the frame pointer is limited to 240 bytes in
     the unwind data structure.  */
  if (TARGET_SEH)
    {
      /* Force the frame pointer to point at or below the lowest register save
	 area, see the SEH code in config/i386/winnt.cc for the rationale.  */
      frame->hard_frame_pointer_offset = frame->sse_reg_save_offset;

      /* If we can leave the frame pointer where it is, do so; however return
	 the establisher frame for __builtin_frame_address (0) or else if the
	 frame overflows the SEH maximum frame size.

	 Note that the value returned by __builtin_frame_address (0) is quite
	 constrained, because setjmp is piggybacked on the SEH machinery with
	 recent versions of MinGW:

	  #    elif defined(__SEH__)
	  #     if defined(__aarch64__) || defined(_ARM64_)
	  #      define setjmp(BUF) _setjmp((BUF), __builtin_sponentry())
	  #     elif (__MINGW_GCC_VERSION < 40702)
	  #      define setjmp(BUF) _setjmp((BUF), mingw_getsp())
	  #     else
	  #      define setjmp(BUF) _setjmp((BUF), __builtin_frame_address (0))
	  #     endif

	 and the second argument passed to _setjmp, if not null, is forwarded
	 to the TargetFrame parameter of RtlUnwindEx by longjmp (after it has
	 built an ExceptionRecord on the fly describing the setjmp buffer).  */
      const HOST_WIDE_INT diff
	= frame->stack_pointer_offset - frame->hard_frame_pointer_offset;
      if (diff <= 255 && !crtl->accesses_prior_frames)
	{
	  /* The resulting diff will be a multiple of 16 lower than 255,
	     i.e. at most 240 as required by the unwind data structure.  */
	  frame->hard_frame_pointer_offset += (diff & 15);
	}
      else if (diff <= SEH_MAX_FRAME_SIZE && !crtl->accesses_prior_frames)
	{
	  /* Ideally we'd determine what portion of the local stack frame
	     (within the constraint of the lowest 240) is most heavily used.
	     But without that complication, simply bias the frame pointer
	     by 128 bytes so as to maximize the amount of the local stack
	     frame that is addressable with 8-bit offsets.  */
	  frame->hard_frame_pointer_offset = frame->stack_pointer_offset - 128;
	}
      else
	frame->hard_frame_pointer_offset = frame->hfp_save_offset;
    }
}

/* This is semi-inlined memory_address_length, but simplified
   since we know that we're always dealing with reg+offset, and
   to avoid having to create and discard all that rtl.  */

static inline int
choose_baseaddr_len (unsigned int regno, HOST_WIDE_INT offset)
{
  int len = 4;

  if (offset == 0)
    {
      /* EBP and R13 cannot be encoded without an offset.  */
      len = (regno == BP_REG || regno == R13_REG);
    }
  else if (IN_RANGE (offset, -128, 127))
    len = 1;

  /* ESP and R12 must be encoded with a SIB byte.  */
  if (regno == SP_REG || regno == R12_REG)
    len++;

  return len;
}

/* Determine if the stack pointer is valid for accessing the CFA_OFFSET in
   the frame save area.  The register is saved at CFA - CFA_OFFSET.  */

static bool
sp_valid_at (HOST_WIDE_INT cfa_offset)
{
  const struct machine_frame_state &fs = cfun->machine->fs;
  if (fs.sp_realigned && cfa_offset <= fs.sp_realigned_offset)
    {
      /* Validate that the cfa_offset isn't in a "no-man's land".  */
      gcc_assert (cfa_offset <= fs.sp_realigned_fp_last);
      return false;
    }
  return fs.sp_valid;
}

/* Determine if the frame pointer is valid for accessing the CFA_OFFSET in
   the frame save area.  The register is saved at CFA - CFA_OFFSET.  */

static inline bool
fp_valid_at (HOST_WIDE_INT cfa_offset)
{
  const struct machine_frame_state &fs = cfun->machine->fs;
  if (fs.sp_realigned && cfa_offset > fs.sp_realigned_fp_last)
    {
      /* Validate that the cfa_offset isn't in a "no-man's land".  */
      gcc_assert (cfa_offset >= fs.sp_realigned_offset);
      return false;
    }
  return fs.fp_valid;
}

/* Choose a base register based upon alignment requested, speed and/or
   size.  */

static void
choose_basereg (HOST_WIDE_INT cfa_offset, rtx &base_reg,
		HOST_WIDE_INT &base_offset,
		unsigned int align_reqested, unsigned int *align)
{
  const struct machine_function *m = cfun->machine;
  unsigned int hfp_align;
  unsigned int drap_align;
  unsigned int sp_align;
  bool hfp_ok  = fp_valid_at (cfa_offset);
  bool drap_ok = m->fs.drap_valid;
  bool sp_ok   = sp_valid_at (cfa_offset);

  hfp_align = drap_align = sp_align = INCOMING_STACK_BOUNDARY;

  /* Filter out any registers that don't meet the requested alignment
     criteria.  */
  if (align_reqested)
    {
      if (m->fs.realigned)
	hfp_align = drap_align = sp_align = crtl->stack_alignment_needed;
      /* SEH unwind code does do not currently support REG_CFA_EXPRESSION
	 notes (which we would need to use a realigned stack pointer),
	 so disable on SEH targets.  */
      else if (m->fs.sp_realigned)
	sp_align = crtl->stack_alignment_needed;

      hfp_ok = hfp_ok && hfp_align >= align_reqested;
      drap_ok = drap_ok && drap_align >= align_reqested;
      sp_ok = sp_ok && sp_align >= align_reqested;
    }

  if (m->use_fast_prologue_epilogue)
    {
      /* Choose the base register most likely to allow the most scheduling
         opportunities.  Generally FP is valid throughout the function,
         while DRAP must be reloaded within the epilogue.  But choose either
         over the SP due to increased encoding size.  */

      if (hfp_ok)
	{
	  base_reg = hard_frame_pointer_rtx;
	  base_offset = m->fs.fp_offset - cfa_offset;
	}
      else if (drap_ok)
	{
	  base_reg = crtl->drap_reg;
	  base_offset = 0 - cfa_offset;
	}
      else if (sp_ok)
	{
	  base_reg = stack_pointer_rtx;
	  base_offset = m->fs.sp_offset - cfa_offset;
	}
    }
  else
    {
      HOST_WIDE_INT toffset;
      int len = 16, tlen;

      /* Choose the base register with the smallest address encoding.
         With a tie, choose FP > DRAP > SP.  */
      if (sp_ok)
	{
	  base_reg = stack_pointer_rtx;
	  base_offset = m->fs.sp_offset - cfa_offset;
          len = choose_baseaddr_len (STACK_POINTER_REGNUM, base_offset);
	}
      if (drap_ok)
	{
	  toffset = 0 - cfa_offset;
	  tlen = choose_baseaddr_len (REGNO (crtl->drap_reg), toffset);
	  if (tlen <= len)
	    {
	      base_reg = crtl->drap_reg;
	      base_offset = toffset;
	      len = tlen;
	    }
	}
      if (hfp_ok)
	{
	  toffset = m->fs.fp_offset - cfa_offset;
	  tlen = choose_baseaddr_len (HARD_FRAME_POINTER_REGNUM, toffset);
	  if (tlen <= len)
	    {
	      base_reg = hard_frame_pointer_rtx;
	      base_offset = toffset;
	    }
	}
    }

    /* Set the align return value.  */
    if (align)
      {
	if (base_reg == stack_pointer_rtx)
	  *align = sp_align;
	else if (base_reg == crtl->drap_reg)
	  *align = drap_align;
	else if (base_reg == hard_frame_pointer_rtx)
	  *align = hfp_align;
      }
}

/* Return an RTX that points to CFA_OFFSET within the stack frame and
   the alignment of address.  If ALIGN is non-null, it should point to
   an alignment value (in bits) that is preferred or zero and will
   recieve the alignment of the base register that was selected,
   irrespective of rather or not CFA_OFFSET is a multiple of that
   alignment value.  If it is possible for the base register offset to be
   non-immediate then SCRATCH_REGNO should specify a scratch register to
   use.

   The valid base registers are taken from CFUN->MACHINE->FS.  */

static rtx
choose_baseaddr (HOST_WIDE_INT cfa_offset, unsigned int *align,
		 unsigned int scratch_regno = INVALID_REGNUM)
{
  rtx base_reg = NULL;
  HOST_WIDE_INT base_offset = 0;

  /* If a specific alignment is requested, try to get a base register
     with that alignment first.  */
  if (align && *align)
    choose_basereg (cfa_offset, base_reg, base_offset, *align, align);

  if (!base_reg)
    choose_basereg (cfa_offset, base_reg, base_offset, 0, align);

  gcc_assert (base_reg != NULL);

  rtx base_offset_rtx = GEN_INT (base_offset);

  if (!x86_64_immediate_operand (base_offset_rtx, Pmode))
    {
      gcc_assert (scratch_regno != INVALID_REGNUM);

      rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);
      emit_move_insn (scratch_reg, base_offset_rtx);

      return gen_rtx_PLUS (Pmode, base_reg, scratch_reg);
    }

  return plus_constant (Pmode, base_reg, base_offset);
}

/* Emit code to save registers in the prologue.  */

static void
ix86_emit_save_regs (void)
{
  int regno;
  rtx_insn *insn;

  if (!TARGET_APX_PUSH2POP2
      || !ix86_can_use_push2pop2 ()
      || cfun->machine->func_type != TYPE_NORMAL)
    {
      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
	if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, true, true))
	  {
	    insn = emit_insn (gen_push (gen_rtx_REG (word_mode, regno),
					TARGET_APX_PPX));
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }
    }
  else
    {
      int regno_list[2];
      regno_list[0] = regno_list[1] = -1;
      int loaded_regnum = 0;
      bool aligned = cfun->machine->fs.sp_offset % 16 == 0;

      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
	if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, true, true))
	  {
	    if (aligned)
	      {
		regno_list[loaded_regnum++] = regno;
		if (loaded_regnum == 2)
		  {
		    gcc_assert (regno_list[0] != -1
				&& regno_list[1] != -1
				&& regno_list[0] != regno_list[1]);
		    const int offset = UNITS_PER_WORD * 2;
		    rtx mem = gen_rtx_MEM (TImode,
					   gen_rtx_PRE_DEC (Pmode,
							    stack_pointer_rtx));
		    insn = emit_insn (gen_push2 (mem,
						 gen_rtx_REG (word_mode,
							      regno_list[0]),
						 gen_rtx_REG (word_mode,
							      regno_list[1]),
						 TARGET_APX_PPX));
		    RTX_FRAME_RELATED_P (insn) = 1;
		    rtx dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (3));

		    for (int i = 0; i < 2; i++)
		      {
			rtx dwarf_reg = gen_rtx_REG (word_mode,
						     regno_list[i]);
			rtx sp_offset = plus_constant (Pmode,
						       stack_pointer_rtx,
						       + UNITS_PER_WORD
							 * (1 - i));
			rtx tmp = gen_rtx_SET (gen_frame_mem (DImode,
							      sp_offset),
					       dwarf_reg);
			RTX_FRAME_RELATED_P (tmp) = 1;
			XVECEXP (dwarf, 0, i + 1) = tmp;
		      }
		    rtx sp_tmp = gen_rtx_SET (stack_pointer_rtx,
					      plus_constant (Pmode,
							     stack_pointer_rtx,
							     -offset));
		    RTX_FRAME_RELATED_P (sp_tmp) = 1;
		    XVECEXP (dwarf, 0, 0) = sp_tmp;
		    add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);

		    loaded_regnum = 0;
		    regno_list[0] = regno_list[1] = -1;
		  }
	      }
	    else
	      {
		insn = emit_insn (gen_push (gen_rtx_REG (word_mode, regno),
					    TARGET_APX_PPX));
		RTX_FRAME_RELATED_P (insn) = 1;
		aligned = true;
	      }
	  }
      if (loaded_regnum == 1)
	{
	  insn = emit_insn (gen_push (gen_rtx_REG (word_mode,
						   regno_list[0]),
				      TARGET_APX_PPX));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Emit a single register save at CFA - CFA_OFFSET.  */

static void
ix86_emit_save_reg_using_mov (machine_mode mode, unsigned int regno,
			      HOST_WIDE_INT cfa_offset)
{
  struct machine_function *m = cfun->machine;
  rtx reg = gen_rtx_REG (mode, regno);
  rtx mem, addr, base, insn;
  unsigned int align = GET_MODE_ALIGNMENT (mode);

  addr = choose_baseaddr (cfa_offset, &align);
  mem = gen_frame_mem (mode, addr);

  /* The location aligment depends upon the base register.  */
  align = MIN (GET_MODE_ALIGNMENT (mode), align);
  gcc_assert (! (cfa_offset & (align / BITS_PER_UNIT - 1)));
  set_mem_align (mem, align);

  insn = emit_insn (gen_rtx_SET (mem, reg));
  RTX_FRAME_RELATED_P (insn) = 1;

  base = addr;
  if (GET_CODE (base) == PLUS)
    base = XEXP (base, 0);
  gcc_checking_assert (REG_P (base));

  /* When saving registers into a re-aligned local stack frame, avoid
     any tricky guessing by dwarf2out.  */
  if (m->fs.realigned)
    {
      gcc_checking_assert (stack_realign_drap);

      if (regno == REGNO (crtl->drap_reg))
	{
	  /* A bit of a hack.  We force the DRAP register to be saved in
	     the re-aligned stack frame, which provides us with a copy
	     of the CFA that will last past the prologue.  Install it.  */
	  gcc_checking_assert (cfun->machine->fs.fp_valid);
	  addr = plus_constant (Pmode, hard_frame_pointer_rtx,
				cfun->machine->fs.fp_offset - cfa_offset);
	  mem = gen_rtx_MEM (mode, addr);
	  add_reg_note (insn, REG_CFA_DEF_CFA, mem);
	}
      else
	{
	  /* The frame pointer is a stable reference within the
	     aligned frame.  Use it.  */
	  gcc_checking_assert (cfun->machine->fs.fp_valid);
	  addr = plus_constant (Pmode, hard_frame_pointer_rtx,
				cfun->machine->fs.fp_offset - cfa_offset);
	  mem = gen_rtx_MEM (mode, addr);
	  add_reg_note (insn, REG_CFA_EXPRESSION, gen_rtx_SET (mem, reg));
	}
    }

  else if (base == stack_pointer_rtx && m->fs.sp_realigned
	   && cfa_offset >= m->fs.sp_realigned_offset)
    {
      gcc_checking_assert (stack_realign_fp);
      add_reg_note (insn, REG_CFA_EXPRESSION, gen_rtx_SET (mem, reg));
    }

  /* The memory may not be relative to the current CFA register,
     which means that we may need to generate a new pattern for
     use by the unwind info.  */
  else if (base != m->fs.cfa_reg)
    {
      addr = plus_constant (Pmode, m->fs.cfa_reg,
			    m->fs.cfa_offset - cfa_offset);
      mem = gen_rtx_MEM (mode, addr);
      add_reg_note (insn, REG_CFA_OFFSET, gen_rtx_SET (mem, reg));
    }
}

/* Emit code to save registers using MOV insns.
   First register is stored at CFA - CFA_OFFSET.  */
static void
ix86_emit_save_regs_using_mov (HOST_WIDE_INT cfa_offset)
{
  unsigned int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, true, true))
      {
        ix86_emit_save_reg_using_mov (word_mode, regno, cfa_offset);
	cfa_offset -= UNITS_PER_WORD;
      }
}

/* Emit code to save SSE registers using MOV insns.
   First register is stored at CFA - CFA_OFFSET.  */
static void
ix86_emit_save_sse_regs_using_mov (HOST_WIDE_INT cfa_offset)
{
  unsigned int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (SSE_REGNO_P (regno) && ix86_save_reg (regno, true, true))
      {
	ix86_emit_save_reg_using_mov (V4SFmode, regno, cfa_offset);
	cfa_offset -= GET_MODE_SIZE (V4SFmode);
      }
}

static GTY(()) rtx queued_cfa_restores;

/* Add a REG_CFA_RESTORE REG note to INSN or queue them until next stack
   manipulation insn.  The value is on the stack at CFA - CFA_OFFSET.
   Don't add the note if the previously saved value will be left untouched
   within stack red-zone till return, as unwinders can find the same value
   in the register and on the stack.  */

static void
ix86_add_cfa_restore_note (rtx_insn *insn, rtx reg, HOST_WIDE_INT cfa_offset)
{
  if (!crtl->shrink_wrapped
      && cfa_offset <= cfun->machine->fs.red_zone_offset)
    return;

  if (insn)
    {
      add_reg_note (insn, REG_CFA_RESTORE, reg);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    queued_cfa_restores
      = alloc_reg_note (REG_CFA_RESTORE, reg, queued_cfa_restores);
}

/* Add queued REG_CFA_RESTORE notes if any to INSN.  */

static void
ix86_add_queued_cfa_restore_notes (rtx insn)
{
  rtx last;
  if (!queued_cfa_restores)
    return;
  for (last = queued_cfa_restores; XEXP (last, 1); last = XEXP (last, 1))
    ;
  XEXP (last, 1) = REG_NOTES (insn);
  REG_NOTES (insn) = queued_cfa_restores;
  queued_cfa_restores = NULL_RTX;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Expand prologue or epilogue stack adjustment.
   The pattern exist to put a dependency on all ebp-based memory accesses.
   STYLE should be negative if instructions should be marked as frame related,
   zero if %r11 register is live and cannot be freely used and positive
   otherwise.  */

static rtx
pro_epilogue_adjust_stack (rtx dest, rtx src, rtx offset,
			   int style, bool set_cfa)
{
  struct machine_function *m = cfun->machine;
  rtx addend = offset;
  rtx insn;
  bool add_frame_related_expr = false;

  if (!x86_64_immediate_operand (offset, Pmode))
    {
      /* r11 is used by indirect sibcall return as well, set before the
	 epilogue and used after the epilogue.  */
      if (style)
        addend = gen_rtx_REG (Pmode, R11_REG);
      else
	{
	  gcc_assert (src != hard_frame_pointer_rtx
		      && dest != hard_frame_pointer_rtx);
	  addend = hard_frame_pointer_rtx;
	}
      emit_insn (gen_rtx_SET (addend, offset));
      if (style < 0)
	add_frame_related_expr = true;
    }

  insn = emit_insn (gen_pro_epilogue_adjust_stack_add
		    (Pmode, dest, src, addend));
  if (style >= 0)
    ix86_add_queued_cfa_restore_notes (insn);

  if (set_cfa)
    {
      rtx r;

      gcc_assert (m->fs.cfa_reg == src);
      m->fs.cfa_offset += INTVAL (offset);
      m->fs.cfa_reg = dest;

      r = gen_rtx_PLUS (Pmode, src, offset);
      r = gen_rtx_SET (dest, r);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, r);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (style < 0)
    {
      RTX_FRAME_RELATED_P (insn) = 1;
      if (add_frame_related_expr)
	{
	  rtx r = gen_rtx_PLUS (Pmode, src, offset);
	  r = gen_rtx_SET (dest, r);
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, r);
	}
    }

  if (dest == stack_pointer_rtx)
    {
      HOST_WIDE_INT ooffset = m->fs.sp_offset;
      bool valid = m->fs.sp_valid;
      bool realigned = m->fs.sp_realigned;

      if (src == hard_frame_pointer_rtx)
	{
	  valid = m->fs.fp_valid;
	  realigned = false;
	  ooffset = m->fs.fp_offset;
	}
      else if (src == crtl->drap_reg)
	{
	  valid = m->fs.drap_valid;
	  realigned = false;
	  ooffset = 0;
	}
      else
	{
	  /* Else there are two possibilities: SP itself, which we set
	     up as the default above.  Or EH_RETURN_STACKADJ_RTX, which is
	     taken care of this by hand along the eh_return path.  */
	  gcc_checking_assert (src == stack_pointer_rtx
			       || offset == const0_rtx);
	}

      m->fs.sp_offset = ooffset - INTVAL (offset);
      m->fs.sp_valid = valid;
      m->fs.sp_realigned = realigned;
    }
  return insn;
}

/* Find an available register to be used as dynamic realign argument
   pointer regsiter.  Such a register will be written in prologue and
   used in begin of body, so it must not be
	1. parameter passing register.
	2. GOT pointer.
   We reuse static-chain register if it is available.  Otherwise, we
   use DI for i386 and R13 for x86-64.  We chose R13 since it has
   shorter encoding.

   Return: the regno of chosen register.  */

static unsigned int
find_drap_reg (void)
{
  tree decl = cfun->decl;

  /* Always use callee-saved register if there are no caller-saved
     registers.  */
  if (TARGET_64BIT)
    {
      /* Use R13 for nested function or function need static chain.
	 Since function with tail call may use any caller-saved
	 registers in epilogue, DRAP must not use caller-saved
	 register in such case.  */
      if (DECL_STATIC_CHAIN (decl)
	  || (cfun->machine->call_saved_registers
	      == TYPE_NO_CALLER_SAVED_REGISTERS)
	  || crtl->tail_call_emit)
	return R13_REG;

      return R10_REG;
    }
  else
    {
      /* Use DI for nested function or function need static chain.
	 Since function with tail call may use any caller-saved
	 registers in epilogue, DRAP must not use caller-saved
	 register in such case.  */
      if (DECL_STATIC_CHAIN (decl)
	  || (cfun->machine->call_saved_registers
	      == TYPE_NO_CALLER_SAVED_REGISTERS)
	  || crtl->tail_call_emit
	  || crtl->calls_eh_return)
	return DI_REG;

      /* Reuse static chain register if it isn't used for parameter
         passing.  */
      if (ix86_function_regparm (TREE_TYPE (decl), decl) <= 2)
	{
	  unsigned int ccvt = ix86_get_callcvt (TREE_TYPE (decl));
	  if ((ccvt & (IX86_CALLCVT_FASTCALL | IX86_CALLCVT_THISCALL)) == 0)
	    return CX_REG;
	}
      return DI_REG;
    }
}

/* Return minimum incoming stack alignment.  */

static unsigned int
ix86_minimum_incoming_stack_boundary (bool sibcall)
{
  unsigned int incoming_stack_boundary;

  /* Stack of interrupt handler is aligned to 128 bits in 64bit mode.  */
  if (cfun->machine->func_type != TYPE_NORMAL)
    incoming_stack_boundary = TARGET_64BIT ? 128 : MIN_STACK_BOUNDARY;
  /* Prefer the one specified at command line. */
  else if (ix86_user_incoming_stack_boundary)
    incoming_stack_boundary = ix86_user_incoming_stack_boundary;
  /* In 32bit, use MIN_STACK_BOUNDARY for incoming stack boundary
     if -mstackrealign is used, it isn't used for sibcall check and
     estimated stack alignment is 128bit.  */
  else if (!sibcall
	   && ix86_force_align_arg_pointer
	   && crtl->stack_alignment_estimated == 128)
    incoming_stack_boundary = MIN_STACK_BOUNDARY;
  else
    incoming_stack_boundary = ix86_default_incoming_stack_boundary;

  /* Incoming stack alignment can be changed on individual functions
     via force_align_arg_pointer attribute.  We use the smallest
     incoming stack boundary.  */
  if (incoming_stack_boundary > MIN_STACK_BOUNDARY
      && lookup_attribute ("force_align_arg_pointer",
			   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    incoming_stack_boundary = MIN_STACK_BOUNDARY;

  /* The incoming stack frame has to be aligned at least at
     parm_stack_boundary.  */
  if (incoming_stack_boundary < crtl->parm_stack_boundary)
    incoming_stack_boundary = crtl->parm_stack_boundary;

  /* Stack at entrance of main is aligned by runtime.  We use the
     smallest incoming stack boundary. */
  if (incoming_stack_boundary > MAIN_STACK_BOUNDARY
      && DECL_NAME (current_function_decl)
      && MAIN_NAME_P (DECL_NAME (current_function_decl))
      && DECL_FILE_SCOPE_P (current_function_decl))
    incoming_stack_boundary = MAIN_STACK_BOUNDARY;

  return incoming_stack_boundary;
}

/* Update incoming stack boundary and estimated stack alignment.  */

static void
ix86_update_stack_boundary (void)
{
  ix86_incoming_stack_boundary
    = ix86_minimum_incoming_stack_boundary (false);

  /* x86_64 vararg needs 16byte stack alignment for register save area.  */
  if (TARGET_64BIT
      && cfun->stdarg
      && crtl->stack_alignment_estimated < 128)
    crtl->stack_alignment_estimated = 128;

  /* __tls_get_addr needs to be called with 16-byte aligned stack.  */
  if (ix86_tls_descriptor_calls_expanded_in_cfun
      && crtl->preferred_stack_boundary < 128)
    crtl->preferred_stack_boundary = 128;
}

/* Handle the TARGET_GET_DRAP_RTX hook.  Return NULL if no DRAP is
   needed or an rtx for DRAP otherwise.  */

static rtx
ix86_get_drap_rtx (void)
{
  /* We must use DRAP if there are outgoing arguments on stack or
     the stack pointer register is clobbered by asm statment and
     ACCUMULATE_OUTGOING_ARGS is false.  */
  if (ix86_force_drap
      || ((cfun->machine->outgoing_args_on_stack
	   || crtl->sp_is_clobbered_by_asm)
	  && !ACCUMULATE_OUTGOING_ARGS))
    crtl->need_drap = true;

  if (stack_realign_drap)
    {
      /* Assign DRAP to vDRAP and returns vDRAP */
      unsigned int regno = find_drap_reg ();
      rtx drap_vreg;
      rtx arg_ptr;
      rtx_insn *seq, *insn;

      arg_ptr = gen_rtx_REG (Pmode, regno);
      crtl->drap_reg = arg_ptr;

      start_sequence ();
      drap_vreg = copy_to_reg (arg_ptr);
      seq = get_insns ();
      end_sequence ();

      insn = emit_insn_before (seq, NEXT_INSN (entry_of_function ()));
      if (!optimize)
	{
	  add_reg_note (insn, REG_CFA_SET_VDRAP, drap_vreg);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      return drap_vreg;
    }
  else
    return NULL;
}

/* Handle the TARGET_INTERNAL_ARG_POINTER hook.  */

static rtx
ix86_internal_arg_pointer (void)
{
  return virtual_incoming_args_rtx;
}

struct scratch_reg {
  rtx reg;
  bool saved;
};

/* Return a short-lived scratch register for use on function entry.
   In 32-bit mode, it is valid only after the registers are saved
   in the prologue.  This register must be released by means of
   release_scratch_register_on_entry once it is dead.  */

static void
get_scratch_register_on_entry (struct scratch_reg *sr)
{
  int regno;

  sr->saved = false;

  if (TARGET_64BIT)
    {
      /* We always use R11 in 64-bit mode.  */
      regno = R11_REG;
    }
  else
    {
      tree decl = current_function_decl, fntype = TREE_TYPE (decl);
      bool fastcall_p
	= lookup_attribute ("fastcall", TYPE_ATTRIBUTES (fntype)) != NULL_TREE;
      bool thiscall_p
	= lookup_attribute ("thiscall", TYPE_ATTRIBUTES (fntype)) != NULL_TREE;
      bool static_chain_p = DECL_STATIC_CHAIN (decl);
      int regparm = ix86_function_regparm (fntype, decl);
      int drap_regno
	= crtl->drap_reg ? REGNO (crtl->drap_reg) : INVALID_REGNUM;

      /* 'fastcall' sets regparm to 2, uses ecx/edx for arguments and eax
	  for the static chain register.  */
      if ((regparm < 1 || (fastcall_p && !static_chain_p))
	  && drap_regno != AX_REG)
	regno = AX_REG;
      /* 'thiscall' sets regparm to 1, uses ecx for arguments and edx
	  for the static chain register.  */
      else if (thiscall_p && !static_chain_p && drap_regno != AX_REG)
        regno = AX_REG;
      else if (regparm < 2 && !thiscall_p && drap_regno != DX_REG)
	regno = DX_REG;
      /* ecx is the static chain register.  */
      else if (regparm < 3 && !fastcall_p && !thiscall_p
	       && !static_chain_p
	       && drap_regno != CX_REG)
	regno = CX_REG;
      else if (ix86_save_reg (BX_REG, true, false))
	regno = BX_REG;
      /* esi is the static chain register.  */
      else if (!(regparm == 3 && static_chain_p)
	       && ix86_save_reg (SI_REG, true, false))
	regno = SI_REG;
      else if (ix86_save_reg (DI_REG, true, false))
	regno = DI_REG;
      else
	{
	  regno = (drap_regno == AX_REG ? DX_REG : AX_REG);
	  sr->saved = true;
	}
    }

  sr->reg = gen_rtx_REG (Pmode, regno);
  if (sr->saved)
    {
      rtx_insn *insn = emit_insn (gen_push (sr->reg));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Release a scratch register obtained from the preceding function.

   If RELEASE_VIA_POP is true, we just pop the register off the stack
   to release it.  This is what non-Linux systems use with -fstack-check.

   Otherwise we use OFFSET to locate the saved register and the
   allocated stack space becomes part of the local frame and is
   deallocated by the epilogue.  */

static void
release_scratch_register_on_entry (struct scratch_reg *sr, HOST_WIDE_INT offset,
				   bool release_via_pop)
{
  if (sr->saved)
    {
      if (release_via_pop)
	{
	  struct machine_function *m = cfun->machine;
	  rtx x, insn = emit_insn (gen_pop (sr->reg));

	  /* The RX FRAME_RELATED_P mechanism doesn't know about pop.  */
	  RTX_FRAME_RELATED_P (insn) = 1;
	  x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
	  x = gen_rtx_SET (stack_pointer_rtx, x);
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, x);
	  m->fs.sp_offset -= UNITS_PER_WORD;
	}
      else
	{
	  rtx x = plus_constant (Pmode, stack_pointer_rtx, offset);
	  x = gen_rtx_SET (sr->reg, gen_rtx_MEM (word_mode, x));
	  emit_insn (x);
	}
    }
}

/* Emit code to adjust the stack pointer by SIZE bytes while probing it.

   If INT_REGISTERS_SAVED is true, then integer registers have already been
   pushed on the stack.

   If PROTECTION AREA is true, then probe PROBE_INTERVAL plus a small dope
   beyond SIZE bytes.

   This assumes no knowledge of the current probing state, i.e. it is never
   allowed to allocate more than PROBE_INTERVAL bytes of stack space without
   a suitable probe.  */

static void
ix86_adjust_stack_and_probe (HOST_WIDE_INT size,
			     const bool int_registers_saved,
			     const bool protection_area)
{
  struct machine_function *m = cfun->machine;

  /* If this function does not statically allocate stack space, then
     no probes are needed.  */
  if (!size)
    {
      /* However, the allocation of space via pushes for register
	 saves could be viewed as allocating space, but without the
	 need to probe.  */
      if (m->frame.nregs || m->frame.nsseregs || frame_pointer_needed)
        dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
      else
	dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);
      return;
    }

  /* If we are a noreturn function, then we have to consider the
     possibility that we're called via a jump rather than a call.

     Thus we don't have the implicit probe generated by saving the
     return address into the stack at the call.  Thus, the stack
     pointer could be anywhere in the guard page.  The safe thing
     to do is emit a probe now.

     The probe can be avoided if we have already emitted any callee
     register saves into the stack or have a frame pointer (which will
     have been saved as well).  Those saves will function as implicit
     probes.

     ?!? This should be revamped to work like aarch64 and s390 where
     we track the offset from the most recent probe.  Normally that
     offset would be zero.  For a noreturn function we would reset
     it to PROBE_INTERVAL - (STACK_BOUNDARY / BITS_PER_UNIT).   Then
     we just probe when we cross PROBE_INTERVAL.  */
  if (TREE_THIS_VOLATILE (cfun->decl)
      && !(m->frame.nregs || m->frame.nsseregs || frame_pointer_needed))
    {
      /* We can safely use any register here since we're just going to push
	 its value and immediately pop it back.  But we do try and avoid
	 argument passing registers so as not to introduce dependencies in
	 the pipeline.  For 32 bit we use %esi and for 64 bit we use %rax.  */
      rtx dummy_reg = gen_rtx_REG (word_mode, TARGET_64BIT ? AX_REG : SI_REG);
      rtx_insn *insn_push = emit_insn (gen_push (dummy_reg));
      rtx_insn *insn_pop = emit_insn (gen_pop (dummy_reg));
      m->fs.sp_offset -= UNITS_PER_WORD;
      if (m->fs.cfa_reg == stack_pointer_rtx)
	{
	  m->fs.cfa_offset -= UNITS_PER_WORD;
	  rtx x = plus_constant (Pmode, stack_pointer_rtx, -UNITS_PER_WORD);
	  x = gen_rtx_SET (stack_pointer_rtx, x);
	  add_reg_note (insn_push, REG_CFA_ADJUST_CFA, x);
	  RTX_FRAME_RELATED_P (insn_push) = 1;
	  x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
	  x = gen_rtx_SET (stack_pointer_rtx, x);
	  add_reg_note (insn_pop, REG_CFA_ADJUST_CFA, x);
	  RTX_FRAME_RELATED_P (insn_pop) = 1;
	}
      emit_insn (gen_blockage ());
    }

  const HOST_WIDE_INT probe_interval = get_probe_interval ();
  const int dope = 4 * UNITS_PER_WORD;

  /* If there is protection area, take it into account in the size.  */
  if (protection_area)
    size += probe_interval + dope;

  /* If we allocate less than the size of the guard statically,
     then no probing is necessary, but we do need to allocate
     the stack.  */
  else if (size < (1 << param_stack_clash_protection_guard_size))
    {
      pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
			         GEN_INT (-size), -1,
			         m->fs.cfa_reg == stack_pointer_rtx);
      dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
      return;
    }

  /* We're allocating a large enough stack frame that we need to
     emit probes.  Either emit them inline or in a loop depending
     on the size.  */
  if (size <= 4 * probe_interval)
    {
      HOST_WIDE_INT i;
      for (i = probe_interval; i <= size; i += probe_interval)
	{
	  /* Allocate PROBE_INTERVAL bytes.  */
	  rtx insn
	    = pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
					 GEN_INT (-probe_interval), -1,
					 m->fs.cfa_reg == stack_pointer_rtx);
	  add_reg_note (insn, REG_STACK_CHECK, const0_rtx);

	  /* And probe at *sp.  */
	  emit_stack_probe (stack_pointer_rtx);
	  emit_insn (gen_blockage ());
	}

      /* We need to allocate space for the residual, but we do not need
	 to probe the residual...  */
      HOST_WIDE_INT residual = (i - probe_interval - size);
      if (residual)
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (residual), -1,
				     m->fs.cfa_reg == stack_pointer_rtx);

	  /* ...except if there is a protection area to maintain.  */
	  if (protection_area)
	    emit_stack_probe (stack_pointer_rtx);
	}

      dump_stack_clash_frame_info (PROBE_INLINE, residual != 0);
    }
  else
    {
      /* We expect the GP registers to be saved when probes are used
	 as the probing sequences might need a scratch register and
	 the routine to allocate one assumes the integer registers
	 have already been saved.  */
      gcc_assert (int_registers_saved);

      struct scratch_reg sr;
      get_scratch_register_on_entry (&sr);

      /* If we needed to save a register, then account for any space
	 that was pushed (we are not going to pop the register when
	 we do the restore).  */
      if (sr.saved)
	size -= UNITS_PER_WORD;

      /* Step 1: round SIZE down to a multiple of the interval.  */
      HOST_WIDE_INT rounded_size = size & -probe_interval;

      /* Step 2: compute final value of the loop counter.  Use lea if
	 possible.  */
      rtx addr = plus_constant (Pmode, stack_pointer_rtx, -rounded_size);
      rtx insn;
      if (address_no_seg_operand (addr, Pmode))
	insn = emit_insn (gen_rtx_SET (sr.reg, addr));
      else
	{
	  emit_move_insn (sr.reg, GEN_INT (-rounded_size));
	  insn = emit_insn (gen_rtx_SET (sr.reg,
					 gen_rtx_PLUS (Pmode, sr.reg,
						       stack_pointer_rtx)));
	}
      if (m->fs.cfa_reg == stack_pointer_rtx)
	{
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, sr.reg,
				       m->fs.cfa_offset + rounded_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Step 3: the loop.  */
      rtx size_rtx = GEN_INT (rounded_size);
      insn = emit_insn (gen_adjust_stack_and_probe (Pmode, sr.reg, sr.reg,
						    size_rtx));
      if (m->fs.cfa_reg == stack_pointer_rtx)
	{
	  m->fs.cfa_offset += rounded_size;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx,
				       m->fs.cfa_offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      m->fs.sp_offset += rounded_size;
      emit_insn (gen_blockage ());

      /* Step 4: adjust SP if we cannot assert at compile-time that SIZE
	 is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (rounded_size - size), -1,
				     m->fs.cfa_reg == stack_pointer_rtx);

	  if (protection_area)
	    emit_stack_probe (stack_pointer_rtx);
	}

      dump_stack_clash_frame_info (PROBE_LOOP, size != rounded_size);

      /* This does not deallocate the space reserved for the scratch
	 register.  That will be deallocated in the epilogue.  */
      release_scratch_register_on_entry (&sr, size, false);
    }

  /* Adjust back to account for the protection area.  */
  if (protection_area)
    pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (probe_interval + dope), -1,
			       m->fs.cfa_reg == stack_pointer_rtx);

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Adjust the stack pointer up to REG while probing it.  */

const char *
output_adjust_stack_and_probe (rtx reg)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* SP = SP + PROBE_INTERVAL.  */
  xops[0] = stack_pointer_rtx;
  xops[1] = GEN_INT (get_probe_interval ());
  output_asm_insn ("sub%z0\t{%1, %0|%0, %1}", xops);

  /* Probe at SP.  */
  xops[1] = const0_rtx;
  output_asm_insn ("or%z0\t{%1, (%0)|DWORD PTR [%0], %1}", xops);

  /* Test if SP == LAST_ADDR.  */
  xops[0] = stack_pointer_rtx;
  xops[1] = reg;
  output_asm_insn ("cmp%z0\t{%1, %0|%0, %1}", xops);

  /* Branch.  */
  fputs ("\tjne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.

   INT_REGISTERS_SAVED is true if integer registers have already been
   pushed on the stack.  */

static void
ix86_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size,
			     const bool int_registers_saved)
{
  const HOST_WIDE_INT probe_interval = get_probe_interval ();

  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  The run-time loop is made up of 6 insns in the
     generic case while the compile-time loop is made up of n insns for n #
     of intervals.  */
  if (size <= 6 * probe_interval)
    {
      HOST_WIDE_INT i;

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 1 until
	 it exceeds SIZE.  If only one probe is needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = probe_interval; i < size; i += probe_interval)
	emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					 -(first + i)));

      emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
				       -(first + size)));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      /* We expect the GP registers to be saved when probes are used
	 as the probing sequences might need a scratch register and
	 the routine to allocate one assumes the integer registers
	 have already been saved.  */
      gcc_assert (int_registers_saved);

      HOST_WIDE_INT rounded_size, last;
      struct scratch_reg sr;

      get_scratch_register_on_entry (&sr);


      /* Step 1: round SIZE to the previous multiple of the interval.  */

      rounded_size = ROUND_DOWN (size, probe_interval);


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_OFFSET = FIRST.  */
      emit_move_insn (sr.reg, GEN_INT (-first));

      /* LAST_OFFSET = FIRST + ROUNDED_SIZE.  */
      last = first + rounded_size;


      /* Step 3: the loop

	 do
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }
	 while (TEST_ADDR != LAST_ADDR)

         probes at FIRST + N * PROBE_INTERVAL for values of N from 1
         until it is equal to ROUNDED_SIZE.  */

      emit_insn
	(gen_probe_stack_range (Pmode, sr.reg, sr.reg, GEN_INT (-last)));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	emit_stack_probe (plus_constant (Pmode,
					 gen_rtx_PLUS (Pmode,
						       stack_pointer_rtx,
						       sr.reg),
					 rounded_size - size));

      release_scratch_register_on_entry (&sr, size, true);
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG to END, inclusive.  These are
   offsets from the current stack pointer.  */

const char *
output_probe_stack_range (rtx reg, rtx end)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[3];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg;
  xops[1] = GEN_INT (get_probe_interval ());
  output_asm_insn ("sub%z0\t{%1, %0|%0, %1}", xops);

  /* Probe at TEST_ADDR.  */
  xops[0] = stack_pointer_rtx;
  xops[1] = reg;
  xops[2] = const0_rtx;
  output_asm_insn ("or%z0\t{%2, (%0,%1)|DWORD PTR [%0+%1], %2}", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[0] = reg;
  xops[1] = end;
  output_asm_insn ("cmp%z0\t{%1, %0|%0, %1}", xops);

  /* Branch.  */
  fputs ("\tjne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Set stack_frame_required to false if stack frame isn't required.
   Update STACK_ALIGNMENT to the largest alignment, in bits, of stack
   slot used if stack frame is required and CHECK_STACK_SLOT is true.  */

static void
ix86_find_max_used_stack_alignment (unsigned int &stack_alignment,
				    bool check_stack_slot)
{
  HARD_REG_SET set_up_by_prologue, prologue_used;
  basic_block bb;

  CLEAR_HARD_REG_SET (prologue_used);
  CLEAR_HARD_REG_SET (set_up_by_prologue);
  add_to_hard_reg_set (&set_up_by_prologue, Pmode, STACK_POINTER_REGNUM);
  add_to_hard_reg_set (&set_up_by_prologue, Pmode, ARG_POINTER_REGNUM);
  add_to_hard_reg_set (&set_up_by_prologue, Pmode,
		       HARD_FRAME_POINTER_REGNUM);

  /* The preferred stack alignment is the minimum stack alignment.  */
  if (stack_alignment > crtl->preferred_stack_boundary)
    stack_alignment = crtl->preferred_stack_boundary;

  bool require_stack_frame = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;
      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn)
	    && requires_stack_frame_p (insn, prologue_used,
				       set_up_by_prologue))
	  {
	    require_stack_frame = true;

	    if (check_stack_slot)
	      {
		/* Find the maximum stack alignment.  */
		subrtx_iterator::array_type array;
		FOR_EACH_SUBRTX (iter, array, PATTERN (insn), ALL)
		  if (MEM_P (*iter)
		      && (reg_mentioned_p (stack_pointer_rtx,
					   *iter)
			  || reg_mentioned_p (frame_pointer_rtx,
					      *iter)))
		    {
		      unsigned int alignment = MEM_ALIGN (*iter);
		      if (alignment > stack_alignment)
			stack_alignment = alignment;
		    }
	      }
	  }
    }

  cfun->machine->stack_frame_required = require_stack_frame;
}

/* Finalize stack_realign_needed and frame_pointer_needed flags, which
   will guide prologue/epilogue to be generated in correct form.  */

static void
ix86_finalize_stack_frame_flags (void)
{
  /* Check if stack realign is really needed after reload, and
     stores result in cfun */
  unsigned int incoming_stack_boundary
    = (crtl->parm_stack_boundary > ix86_incoming_stack_boundary
       ? crtl->parm_stack_boundary : ix86_incoming_stack_boundary);
  unsigned int stack_alignment
    = (crtl->is_leaf && !ix86_current_function_calls_tls_descriptor
       ? crtl->max_used_stack_slot_alignment
       : crtl->stack_alignment_needed);
  unsigned int stack_realign
    = (incoming_stack_boundary < stack_alignment);
  bool recompute_frame_layout_p = false;

  if (crtl->stack_realign_finalized)
    {
      /* After stack_realign_needed is finalized, we can't no longer
	 change it.  */
      gcc_assert (crtl->stack_realign_needed == stack_realign);
      return;
    }

  /* It is always safe to compute max_used_stack_alignment.  We
     compute it only if 128-bit aligned load/store may be generated
     on misaligned stack slot which will lead to segfault. */
  bool check_stack_slot
    = (stack_realign || crtl->max_used_stack_slot_alignment >= 128);
  ix86_find_max_used_stack_alignment (stack_alignment,
				      check_stack_slot);

  /* If the only reason for frame_pointer_needed is that we conservatively
     assumed stack realignment might be needed or -fno-omit-frame-pointer
     is used, but in the end nothing that needed the stack alignment had
     been spilled nor stack access, clear frame_pointer_needed and say we
     don't need stack realignment.

     When vector register is used for piecewise move and store, we don't
     increase stack_alignment_needed as there is no register spill for
     piecewise move and store.  Since stack_realign_needed is set to true
     by checking stack_alignment_estimated which is updated by pseudo
     vector register usage, we also need to check stack_realign_needed to
     eliminate frame pointer.  */
  if ((stack_realign
       || (!flag_omit_frame_pointer && optimize)
       || crtl->stack_realign_needed)
      && frame_pointer_needed
      && crtl->is_leaf
      && crtl->sp_is_unchanging
      && !ix86_current_function_calls_tls_descriptor
      && !crtl->accesses_prior_frames
      && !cfun->calls_alloca
      && !crtl->calls_eh_return
      /* See ira_setup_eliminable_regset for the rationale.  */
      && !(STACK_CHECK_MOVING_SP
	   && flag_stack_check
	   && flag_exceptions
	   && cfun->can_throw_non_call_exceptions)
      && !ix86_frame_pointer_required ()
      && ix86_get_frame_size () == 0
      && ix86_nsaved_sseregs () == 0
      && ix86_varargs_gpr_size + ix86_varargs_fpr_size == 0)
    {
      if (cfun->machine->stack_frame_required)
	{
	  /* Stack frame is required.  If stack alignment needed is less
	     than incoming stack boundary, don't realign stack.  */
	  stack_realign = incoming_stack_boundary < stack_alignment;
	  if (!stack_realign)
	    {
	      crtl->max_used_stack_slot_alignment
		= incoming_stack_boundary;
	      crtl->stack_alignment_needed
		= incoming_stack_boundary;
	      /* Also update preferred_stack_boundary for leaf
	         functions.  */
	      crtl->preferred_stack_boundary
		= incoming_stack_boundary;
	    }
	}
      else
	{
	  /* If drap has been set, but it actually isn't live at the
	     start of the function, there is no reason to set it up.  */
	  if (crtl->drap_reg)
	    {
	      basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
	      if (! REGNO_REG_SET_P (DF_LR_IN (bb),
				     REGNO (crtl->drap_reg)))
		{
		  crtl->drap_reg = NULL_RTX;
		  crtl->need_drap = false;
		}
	    }
	  else
	    cfun->machine->no_drap_save_restore = true;

	  frame_pointer_needed = false;
	  stack_realign = false;
	  crtl->max_used_stack_slot_alignment = incoming_stack_boundary;
	  crtl->stack_alignment_needed = incoming_stack_boundary;
	  crtl->stack_alignment_estimated = incoming_stack_boundary;
	  if (crtl->preferred_stack_boundary > incoming_stack_boundary)
	    crtl->preferred_stack_boundary = incoming_stack_boundary;
	  df_finish_pass (true);
	  df_scan_alloc (NULL);
	  df_scan_blocks ();
	  df_compute_regs_ever_live (true);
	  df_analyze ();

	  if (flag_var_tracking)
	    {
	      /* Since frame pointer is no longer available, replace it with
		 stack pointer - UNITS_PER_WORD in debug insns.  */
	      df_ref ref, next;
	      for (ref = DF_REG_USE_CHAIN (HARD_FRAME_POINTER_REGNUM);
		   ref; ref = next)
		{
		  next = DF_REF_NEXT_REG (ref);
		  if (!DF_REF_INSN_INFO (ref))
		    continue;

		  /* Make sure the next ref is for a different instruction,
		     so that we're not affected by the rescan.  */
		  rtx_insn *insn = DF_REF_INSN (ref);
		  while (next && DF_REF_INSN (next) == insn)
		    next = DF_REF_NEXT_REG (next);

		  if (DEBUG_INSN_P (insn))
		    {
		      bool changed = false;
		      for (; ref != next; ref = DF_REF_NEXT_REG (ref))
			{
			  rtx *loc = DF_REF_LOC (ref);
			  if (*loc == hard_frame_pointer_rtx)
			    {
			      *loc = plus_constant (Pmode,
						    stack_pointer_rtx,
						    -UNITS_PER_WORD);
			      changed = true;
			    }
			}
		      if (changed)
			df_insn_rescan (insn);
		    }
		}
	    }

	  recompute_frame_layout_p = true;
	}
    }
  else if (crtl->max_used_stack_slot_alignment >= 128
	   && cfun->machine->stack_frame_required)
    {
      /* We don't need to realign stack.  max_used_stack_alignment is
	 used to decide how stack frame should be aligned.  This is
	 independent of any psABIs nor 32-bit vs 64-bit.  */
      cfun->machine->max_used_stack_alignment
	= stack_alignment / BITS_PER_UNIT;
    }

  if (crtl->stack_realign_needed != stack_realign)
    recompute_frame_layout_p = true;
  crtl->stack_realign_needed = stack_realign;
  crtl->stack_realign_finalized = true;
  if (recompute_frame_layout_p)
    ix86_compute_frame_layout ();
}

/* Delete SET_GOT right after entry block if it is allocated to reg.  */

static void
ix86_elim_entry_set_got (rtx reg)
{
  basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
  rtx_insn *c_insn = BB_HEAD (bb);
  if (!NONDEBUG_INSN_P (c_insn))
    c_insn = next_nonnote_nondebug_insn (c_insn);
  if (c_insn && NONJUMP_INSN_P (c_insn))
    {
      rtx pat = PATTERN (c_insn);
      if (GET_CODE (pat) == PARALLEL)
	{
	  rtx set = XVECEXP (pat, 0, 0);
	  if (GET_CODE (set) == SET
	      && GET_CODE (SET_SRC (set)) == UNSPEC
	      && XINT (SET_SRC (set), 1) == UNSPEC_SET_GOT
	      && REGNO (SET_DEST (set)) == REGNO (reg))
	    delete_insn (c_insn);
	}
    }
}

static rtx
gen_frame_set (rtx reg, rtx frame_reg, int offset, bool store)
{
  rtx addr, mem;

  if (offset)
    addr = plus_constant (Pmode, frame_reg, offset);
  mem = gen_frame_mem (GET_MODE (reg), offset ? addr : frame_reg);
  return gen_rtx_SET (store ? mem : reg, store ? reg : mem);
}

static inline rtx
gen_frame_load (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, false);
}

static inline rtx
gen_frame_store (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, true);
}

static void
ix86_emit_outlined_ms2sysv_save (const struct ix86_frame &frame)
{
  struct machine_function *m = cfun->machine;
  const unsigned ncregs = NUM_X86_64_MS_CLOBBERED_REGS
			  + m->call_ms2sysv_extra_regs;
  rtvec v = rtvec_alloc (ncregs + 1);
  unsigned int align, i, vi = 0;
  rtx_insn *insn;
  rtx sym, addr;
  rtx rax = gen_rtx_REG (word_mode, AX_REG);
  const class xlogue_layout &xlogue = xlogue_layout::get_instance ();

  /* AL should only be live with sysv_abi.  */
  gcc_assert (!ix86_eax_live_at_start_p ());
  gcc_assert (m->fs.sp_offset >= frame.sse_reg_save_offset);

  /* Setup RAX as the stub's base pointer.  We use stack_realign_offset rather
     we've actually realigned the stack or not.  */
  align = GET_MODE_ALIGNMENT (V4SFmode);
  addr = choose_baseaddr (frame.stack_realign_offset
			  + xlogue.get_stub_ptr_offset (), &align, AX_REG);
  gcc_assert (align >= GET_MODE_ALIGNMENT (V4SFmode));

  emit_insn (gen_rtx_SET (rax, addr));

  /* Get the stub symbol.  */
  sym = xlogue.get_stub_rtx (frame_pointer_needed ? XLOGUE_STUB_SAVE_HFP
						  : XLOGUE_STUB_SAVE);
  RTVEC_ELT (v, vi++) = gen_rtx_USE (VOIDmode, sym);

  for (i = 0; i < ncregs; ++i)
    {
      const xlogue_layout::reginfo &r = xlogue.get_reginfo (i);
      rtx reg = gen_rtx_REG ((SSE_REGNO_P (r.regno) ? V4SFmode : word_mode),
			     r.regno);
      RTVEC_ELT (v, vi++) = gen_frame_store (reg, rax, -r.offset);
    }

  gcc_assert (vi == (unsigned)GET_NUM_ELEM (v));

  insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, v));
  RTX_FRAME_RELATED_P (insn) = true;
}

/* Generate and return an insn body to AND X with Y.  */

static rtx_insn *
gen_and2_insn (rtx x, rtx y)
{
  enum insn_code icode = optab_handler (and_optab, GET_MODE (x));

  gcc_assert (insn_operand_matches (icode, 0, x));
  gcc_assert (insn_operand_matches (icode, 1, x));
  gcc_assert (insn_operand_matches (icode, 2, y));

  return GEN_FCN (icode) (x, x, y);
}

/* Expand the prologue into a bunch of separate insns.  */

void
ix86_expand_prologue (void)
{
  struct machine_function *m = cfun->machine;
  rtx insn, t;
  HOST_WIDE_INT allocate;
  bool int_registers_saved;
  bool sse_registers_saved;
  bool save_stub_call_needed;
  rtx static_chain = NULL_RTX;

  ix86_last_zero_store_uid = 0;
  if (ix86_function_naked (current_function_decl))
    {
      if (flag_stack_usage_info)
	current_function_static_stack_size = 0;
      return;
    }

  ix86_finalize_stack_frame_flags ();

  /* DRAP should not coexist with stack_realign_fp */
  gcc_assert (!(crtl->drap_reg && stack_realign_fp));

  memset (&m->fs, 0, sizeof (m->fs));

  /* Initialize CFA state for before the prologue.  */
  m->fs.cfa_reg = stack_pointer_rtx;
  m->fs.cfa_offset = INCOMING_FRAME_SP_OFFSET;

  /* Track SP offset to the CFA.  We continue tracking this after we've
     swapped the CFA register away from SP.  In the case of re-alignment
     this is fudged; we're interested to offsets within the local frame.  */
  m->fs.sp_offset = INCOMING_FRAME_SP_OFFSET;
  m->fs.sp_valid = true;
  m->fs.sp_realigned = false;

  const struct ix86_frame &frame = cfun->machine->frame;

  if (!TARGET_64BIT && ix86_function_ms_hook_prologue (current_function_decl))
    {
      /* We should have already generated an error for any use of
         ms_hook on a nested function.  */
      gcc_checking_assert (!ix86_static_chain_on_stack);

      /* Check if profiling is active and we shall use profiling before
         prologue variant. If so sorry.  */
      if (crtl->profile && flag_fentry != 0)
	sorry ("%<ms_hook_prologue%> attribute is not compatible "
	       "with %<-mfentry%> for 32-bit");

      /* In ix86_asm_output_function_label we emitted:
	 8b ff     movl.s %edi,%edi
	 55        push   %ebp
	 8b ec     movl.s %esp,%ebp

	 This matches the hookable function prologue in Win32 API
	 functions in Microsoft Windows XP Service Pack 2 and newer.
	 Wine uses this to enable Windows apps to hook the Win32 API
	 functions provided by Wine.

	 What that means is that we've already set up the frame pointer.  */

      if (frame_pointer_needed
	  && !(crtl->drap_reg && crtl->stack_realign_needed))
	{
	  rtx push, mov;

	  /* We've decided to use the frame pointer already set up.
	     Describe this to the unwinder by pretending that both
	     push and mov insns happen right here.

	     Putting the unwind info here at the end of the ms_hook
	     is done so that we can make absolutely certain we get
	     the required byte sequence at the start of the function,
	     rather than relying on an assembler that can produce
	     the exact encoding required.

	     However it does mean (in the unpatched case) that we have
	     a 1 insn window where the asynchronous unwind info is
	     incorrect.  However, if we placed the unwind info at
	     its correct location we would have incorrect unwind info
	     in the patched case.  Which is probably all moot since
	     I don't expect Wine generates dwarf2 unwind info for the
	     system libraries that use this feature.  */

	  insn = emit_insn (gen_blockage ());

	  push = gen_push (hard_frame_pointer_rtx);
	  mov = gen_rtx_SET (hard_frame_pointer_rtx,
			     stack_pointer_rtx);
	  RTX_FRAME_RELATED_P (push) = 1;
	  RTX_FRAME_RELATED_P (mov) = 1;

	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, push, mov)));

	  /* Note that gen_push incremented m->fs.cfa_offset, even
	     though we didn't emit the push insn here.  */
	  m->fs.cfa_reg = hard_frame_pointer_rtx;
	  m->fs.fp_offset = m->fs.cfa_offset;
	  m->fs.fp_valid = true;
	}
      else
	{
	  /* The frame pointer is not needed so pop %ebp again.
	     This leaves us with a pristine state.  */
	  emit_insn (gen_pop (hard_frame_pointer_rtx));
	}
    }

  /* The first insn of a function that accepts its static chain on the
     stack is to push the register that would be filled in by a direct
     call.  This insn will be skipped by the trampoline.  */
  else if (ix86_static_chain_on_stack)
    {
      static_chain = ix86_static_chain (cfun->decl, false);
      insn = emit_insn (gen_push (static_chain));
      emit_insn (gen_blockage ());

      /* We don't want to interpret this push insn as a register save,
	 only as a stack adjustment.  The real copy of the register as
	 a save will be done later, if needed.  */
      t = plus_constant (Pmode, stack_pointer_rtx, -UNITS_PER_WORD);
      t = gen_rtx_SET (stack_pointer_rtx, t);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, t);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Emit prologue code to adjust stack alignment and setup DRAP, in case
     of DRAP is needed and stack realignment is really needed after reload */
  if (stack_realign_drap)
    {
      int align_bytes = crtl->stack_alignment_needed / BITS_PER_UNIT;

      /* Can't use DRAP in interrupt function.  */
      if (cfun->machine->func_type != TYPE_NORMAL)
	sorry ("Dynamic Realign Argument Pointer (DRAP) not supported "
	       "in interrupt service routine.  This may be worked "
	       "around by avoiding functions with aggregate return.");

      /* Only need to push parameter pointer reg if it is caller saved.  */
      if (!call_used_or_fixed_reg_p (REGNO (crtl->drap_reg)))
	{
	  /* Push arg pointer reg */
	  insn = emit_insn (gen_push (crtl->drap_reg));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Grab the argument pointer.  */
      t = plus_constant (Pmode, stack_pointer_rtx, m->fs.sp_offset);
      insn = emit_insn (gen_rtx_SET (crtl->drap_reg, t));
      RTX_FRAME_RELATED_P (insn) = 1;
      m->fs.cfa_reg = crtl->drap_reg;
      m->fs.cfa_offset = 0;

      /* Align the stack.  */
      insn = emit_insn (gen_and2_insn (stack_pointer_rtx,
				       GEN_INT (-align_bytes)));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Replicate the return address on the stack so that return
	 address can be reached via (argp - 1) slot.  This is needed
	 to implement macro RETURN_ADDR_RTX and intrinsic function
	 expand_builtin_return_addr etc.  */
      t = plus_constant (Pmode, crtl->drap_reg, -UNITS_PER_WORD);
      t = gen_frame_mem (word_mode, t);
      insn = emit_insn (gen_push (t));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* For the purposes of frame and register save area addressing,
	 we've started over with a new frame.  */
      m->fs.sp_offset = INCOMING_FRAME_SP_OFFSET;
      m->fs.realigned = true;

      if (static_chain)
	{
	  /* Replicate static chain on the stack so that static chain
	     can be reached via (argp - 2) slot.  This is needed for
	     nested function with stack realignment.  */
	  insn = emit_insn (gen_push (static_chain));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  int_registers_saved = (frame.nregs == 0);
  sse_registers_saved = (frame.nsseregs == 0);
  save_stub_call_needed = (m->call_ms2sysv);
  gcc_assert (sse_registers_saved || !save_stub_call_needed);

  if (frame_pointer_needed && !m->fs.fp_valid)
    {
      /* Note: AT&T enter does NOT have reversed args.  Enter is probably
         slower on all targets.  Also sdb didn't like it.  */
      insn = emit_insn (gen_push (hard_frame_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (m->fs.sp_offset == frame.hard_frame_pointer_offset)
	{
	  insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  if (m->fs.cfa_reg == stack_pointer_rtx)
	    m->fs.cfa_reg = hard_frame_pointer_rtx;
	  m->fs.fp_offset = m->fs.sp_offset;
	  m->fs.fp_valid = true;
	}
    }

  if (!int_registers_saved)
    {
      /* If saving registers via PUSH, do so now.  */
      if (!frame.save_regs_using_mov)
	{
	  ix86_emit_save_regs ();
	  int_registers_saved = true;
	  gcc_assert (m->fs.sp_offset == frame.reg_save_offset);
	}

      /* When using red zone we may start register saving before allocating
	 the stack frame saving one cycle of the prologue.  However, avoid
	 doing this if we have to probe the stack; at least on x86_64 the
	 stack probe can turn into a call that clobbers a red zone location. */
      else if (ix86_using_red_zone ()
	       && (! TARGET_STACK_PROBE
		   || frame.stack_pointer_offset < CHECK_STACK_LIMIT))
	{
	  ix86_emit_save_regs_using_mov (frame.reg_save_offset);
	  cfun->machine->red_zone_used = true;
	  int_registers_saved = true;
	}
    }

  if (frame.red_zone_size != 0)
    cfun->machine->red_zone_used = true;

  if (stack_realign_fp)
    {
      int align_bytes = crtl->stack_alignment_needed / BITS_PER_UNIT;
      gcc_assert (align_bytes > MIN_STACK_BOUNDARY / BITS_PER_UNIT);

      /* Record last valid frame pointer offset.  */
      m->fs.sp_realigned_fp_last = frame.reg_save_offset;

      /* The computation of the size of the re-aligned stack frame means
	 that we must allocate the size of the register save area before
	 performing the actual alignment.  Otherwise we cannot guarantee
	 that there's enough storage above the realignment point.  */
      allocate = frame.reg_save_offset - m->fs.sp_offset
		 + frame.stack_realign_allocate;
      if (allocate)
        pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				   GEN_INT (-allocate), -1, false);

      /* Align the stack.  */
      emit_insn (gen_and2_insn (stack_pointer_rtx, GEN_INT (-align_bytes)));
      m->fs.sp_offset = ROUND_UP (m->fs.sp_offset, align_bytes);
      m->fs.sp_realigned_offset = m->fs.sp_offset
					      - frame.stack_realign_allocate;
      /* The stack pointer may no longer be equal to CFA - m->fs.sp_offset.
	 Beyond this point, stack access should be done via choose_baseaddr or
	 by using sp_valid_at and fp_valid_at to determine the correct base
	 register.  Henceforth, any CFA offset should be thought of as logical
	 and not physical.  */
      gcc_assert (m->fs.sp_realigned_offset >= m->fs.sp_realigned_fp_last);
      gcc_assert (m->fs.sp_realigned_offset == frame.stack_realign_offset);
      m->fs.sp_realigned = true;

      /* SEH unwind emit doesn't currently support REG_CFA_EXPRESSION, which
	 is needed to describe where a register is saved using a realigned
	 stack pointer, so we need to invalidate the stack pointer for that
	 target.  */
      if (TARGET_SEH)
	m->fs.sp_valid = false;

      /* If SP offset is non-immediate after allocation of the stack frame,
	 then emit SSE saves or stub call prior to allocating the rest of the
	 stack frame.  This is less efficient for the out-of-line stub because
	 we can't combine allocations across the call barrier, but it's better
	 than using a scratch register.  */
      else if (!x86_64_immediate_operand (GEN_INT (frame.stack_pointer_offset
						   - m->fs.sp_realigned_offset),
					  Pmode))
	{
	  if (!sse_registers_saved)
	    {
	      ix86_emit_save_sse_regs_using_mov (frame.sse_reg_save_offset);
	      sse_registers_saved = true;
	    }
	  else if (save_stub_call_needed)
	    {
	      ix86_emit_outlined_ms2sysv_save (frame);
	      save_stub_call_needed = false;
	    }
	}
    }

  allocate = frame.stack_pointer_offset - m->fs.sp_offset;

  if (flag_stack_usage_info)
    {
      /* We start to count from ARG_POINTER.  */
      HOST_WIDE_INT stack_size = frame.stack_pointer_offset;

      /* If it was realigned, take into account the fake frame.  */
      if (stack_realign_drap)
	{
	  if (ix86_static_chain_on_stack)
	    stack_size += UNITS_PER_WORD;

	  if (!call_used_or_fixed_reg_p (REGNO (crtl->drap_reg)))
	    stack_size += UNITS_PER_WORD;

	  /* This over-estimates by 1 minimal-stack-alignment-unit but
	     mitigates that by counting in the new return address slot.  */
	  current_function_dynamic_stack_size
	    += crtl->stack_alignment_needed / BITS_PER_UNIT;
	}

      current_function_static_stack_size = stack_size;
    }

  /* On SEH target with very large frame size, allocate an area to save
     SSE registers (as the very large allocation won't be described).  */
  if (TARGET_SEH
      && frame.stack_pointer_offset > SEH_MAX_FRAME_SIZE
      && !sse_registers_saved)
    {
      HOST_WIDE_INT sse_size
	= frame.sse_reg_save_offset - frame.reg_save_offset;

      gcc_assert (int_registers_saved);

      /* No need to do stack checking as the area will be immediately
	 written.  */
      pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
			         GEN_INT (-sse_size), -1,
				 m->fs.cfa_reg == stack_pointer_rtx);
      allocate -= sse_size;
      ix86_emit_save_sse_regs_using_mov (frame.sse_reg_save_offset);
      sse_registers_saved = true;
    }

  /* If stack clash protection is requested, then probe the stack, unless it
     is already probed on the target.  */
  if (allocate >= 0
      && flag_stack_clash_protection
      && !ix86_target_stack_probe ())
    {
      ix86_adjust_stack_and_probe (allocate, int_registers_saved, false);
      allocate = 0;
    }

  /* The stack has already been decremented by the instruction calling us
     so probe if the size is non-negative to preserve the protection area.  */
  else if (allocate >= 0 && flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      const HOST_WIDE_INT probe_interval = get_probe_interval ();

      if (STACK_CHECK_MOVING_SP)
	{
	  if (crtl->is_leaf
	      && !cfun->calls_alloca
	      && allocate <= probe_interval)
	    ;

	  else
	    {
	      ix86_adjust_stack_and_probe (allocate, int_registers_saved, true);
	      allocate = 0;
	    }
	}

      else
	{
	  HOST_WIDE_INT size = allocate;

	  if (TARGET_64BIT && size >= HOST_WIDE_INT_C (0x80000000))
	    size = 0x80000000 - get_stack_check_protect () - 1;

	  if (TARGET_STACK_PROBE)
	    {
	      if (crtl->is_leaf && !cfun->calls_alloca)
		{
		  if (size > probe_interval)
		    ix86_emit_probe_stack_range (0, size, int_registers_saved);
		}
	      else
		ix86_emit_probe_stack_range (0,
					     size + get_stack_check_protect (),
					     int_registers_saved);
	    }
	  else
	    {
	      if (crtl->is_leaf && !cfun->calls_alloca)
		{
		  if (size > probe_interval
		      && size > get_stack_check_protect ())
		    ix86_emit_probe_stack_range (get_stack_check_protect (),
						 (size
						  - get_stack_check_protect ()),
						 int_registers_saved);
		}
	      else
		ix86_emit_probe_stack_range (get_stack_check_protect (), size,
					     int_registers_saved);
	    }
	}
    }

  if (allocate == 0)
    ;
  else if (!ix86_target_stack_probe ()
	   || frame.stack_pointer_offset < CHECK_STACK_LIMIT)
    {
      pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
			         GEN_INT (-allocate), -1,
			         m->fs.cfa_reg == stack_pointer_rtx);
    }
  else
    {
      rtx eax = gen_rtx_REG (Pmode, AX_REG);
      rtx r10 = NULL;
      const bool sp_is_cfa_reg = (m->fs.cfa_reg == stack_pointer_rtx);
      bool eax_live = ix86_eax_live_at_start_p ();
      bool r10_live = false;

      if (TARGET_64BIT)
        r10_live = (DECL_STATIC_CHAIN (current_function_decl) != 0);

      if (eax_live)
	{
	  insn = emit_insn (gen_push (eax));
	  allocate -= UNITS_PER_WORD;
	  /* Note that SEH directives need to continue tracking the stack
	     pointer even after the frame pointer has been set up.  */
	  if (sp_is_cfa_reg || TARGET_SEH)
	    {
	      if (sp_is_cfa_reg)
		m->fs.cfa_offset += UNITS_PER_WORD;
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			    gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							-UNITS_PER_WORD)));
	    }
	}

      if (r10_live)
	{
	  r10 = gen_rtx_REG (Pmode, R10_REG);
	  insn = emit_insn (gen_push (r10));
	  allocate -= UNITS_PER_WORD;
	  if (sp_is_cfa_reg || TARGET_SEH)
	    {
	      if (sp_is_cfa_reg)
		m->fs.cfa_offset += UNITS_PER_WORD;
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			    gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							-UNITS_PER_WORD)));
	    }
	}

      emit_move_insn (eax, GEN_INT (allocate));
      emit_insn (gen_allocate_stack_worker_probe (Pmode, eax, eax));

      /* Use the fact that AX still contains ALLOCATE.  */
      insn = emit_insn (gen_pro_epilogue_adjust_stack_sub
			(Pmode, stack_pointer_rtx, stack_pointer_rtx, eax));

      if (sp_is_cfa_reg || TARGET_SEH)
	{
	  if (sp_is_cfa_reg)
	    m->fs.cfa_offset += allocate;
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    -allocate)));
	}
      m->fs.sp_offset += allocate;

      /* Use stack_pointer_rtx for relative addressing so that code works for
	 realigned stack.  But this means that we need a blockage to prevent
	 stores based on the frame pointer from being scheduled before.  */
      if (r10_live && eax_live)
        {
	  t = gen_rtx_PLUS (Pmode, stack_pointer_rtx, eax);
	  emit_move_insn (gen_rtx_REG (word_mode, R10_REG),
			  gen_frame_mem (word_mode, t));
	  t = plus_constant (Pmode, t, UNITS_PER_WORD);
	  emit_move_insn (gen_rtx_REG (word_mode, AX_REG),
			  gen_frame_mem (word_mode, t));
	  emit_insn (gen_memory_blockage ());
	}
      else if (eax_live || r10_live)
	{
	  t = gen_rtx_PLUS (Pmode, stack_pointer_rtx, eax);
	  emit_move_insn (gen_rtx_REG (word_mode,
				       (eax_live ? AX_REG : R10_REG)),
			  gen_frame_mem (word_mode, t));
	  emit_insn (gen_memory_blockage ());
	}
    }
  gcc_assert (m->fs.sp_offset == frame.stack_pointer_offset);

  /* If we havn't already set up the frame pointer, do so now.  */
  if (frame_pointer_needed && !m->fs.fp_valid)
    {
      insn = gen_add3_insn (hard_frame_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (frame.stack_pointer_offset
				     - frame.hard_frame_pointer_offset));
      insn = emit_insn (insn);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_ADJUST_CFA, NULL);

      if (m->fs.cfa_reg == stack_pointer_rtx)
	m->fs.cfa_reg = hard_frame_pointer_rtx;
      m->fs.fp_offset = frame.hard_frame_pointer_offset;
      m->fs.fp_valid = true;
    }

  if (!int_registers_saved)
    ix86_emit_save_regs_using_mov (frame.reg_save_offset);
  if (!sse_registers_saved)
    ix86_emit_save_sse_regs_using_mov (frame.sse_reg_save_offset);
  else if (save_stub_call_needed)
    ix86_emit_outlined_ms2sysv_save (frame);

  /* For the mcount profiling on 32 bit PIC mode we need to emit SET_GOT
     in PROLOGUE.  */
  if (!TARGET_64BIT && pic_offset_table_rtx && crtl->profile && !flag_fentry)
    {
      rtx pic = gen_rtx_REG (Pmode, REAL_PIC_OFFSET_TABLE_REGNUM);
      insn = emit_insn (gen_set_got (pic));
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_FLUSH_QUEUE, NULL_RTX);
      emit_insn (gen_prologue_use (pic));
      /* Deleting already emmitted SET_GOT if exist and allocated to
	 REAL_PIC_OFFSET_TABLE_REGNUM.  */
      ix86_elim_entry_set_got (pic);
    }

  if (crtl->drap_reg && !crtl->stack_realign_needed)
    {
      /* vDRAP is setup but after reload it turns out stack realign
         isn't necessary, here we will emit prologue to setup DRAP
         without stack realign adjustment */
      t = choose_baseaddr (0, NULL);
      emit_insn (gen_rtx_SET (crtl->drap_reg, t));
    }

  /* Prevent instructions from being scheduled into register save push
     sequence when access to the redzone area is done through frame pointer.
     The offset between the frame pointer and the stack pointer is calculated
     relative to the value of the stack pointer at the end of the function
     prologue, and moving instructions that access redzone area via frame
     pointer inside push sequence violates this assumption.  */
  if (frame_pointer_needed && frame.red_zone_size)
    emit_insn (gen_memory_blockage ());

  /* SEH requires that the prologue end within 256 bytes of the start of
     the function.  Prevent instruction schedules that would extend that.
     Further, prevent alloca modifications to the stack pointer from being
     combined with prologue modifications.  */
  if (TARGET_SEH)
    emit_insn (gen_prologue_use (stack_pointer_rtx));
}

/* Emit code to restore REG using a POP or POPP insn.  */

static void
ix86_emit_restore_reg_using_pop (rtx reg, bool ppx_p)
{
  struct machine_function *m = cfun->machine;
  rtx_insn *insn = emit_insn (gen_pop (reg, ppx_p));

  ix86_add_cfa_restore_note (insn, reg, m->fs.sp_offset);
  m->fs.sp_offset -= UNITS_PER_WORD;

  if (m->fs.cfa_reg == crtl->drap_reg
      && REGNO (reg) == REGNO (crtl->drap_reg))
    {
      /* Previously we'd represented the CFA as an expression
	 like *(%ebp - 8).  We've just popped that value from
	 the stack, which means we need to reset the CFA to
	 the drap register.  This will remain until we restore
	 the stack pointer.  */
      add_reg_note (insn, REG_CFA_DEF_CFA, reg);
      RTX_FRAME_RELATED_P (insn) = 1;

      /* This means that the DRAP register is valid for addressing too.  */
      m->fs.drap_valid = true;
      return;
    }

  if (m->fs.cfa_reg == stack_pointer_rtx)
    {
      rtx x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
      x = gen_rtx_SET (stack_pointer_rtx, x);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
      RTX_FRAME_RELATED_P (insn) = 1;

      m->fs.cfa_offset -= UNITS_PER_WORD;
    }

  /* When the frame pointer is the CFA, and we pop it, we are
     swapping back to the stack pointer as the CFA.  This happens
     for stack frames that don't allocate other data, so we assume
     the stack pointer is now pointing at the return address, i.e.
     the function entry state, which makes the offset be 1 word.  */
  if (reg == hard_frame_pointer_rtx)
    {
      m->fs.fp_valid = false;
      if (m->fs.cfa_reg == hard_frame_pointer_rtx)
	{
	  m->fs.cfa_reg = stack_pointer_rtx;
	  m->fs.cfa_offset -= UNITS_PER_WORD;

	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx,
				       m->fs.cfa_offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Emit code to restore REG using a POP2 insn.  */
static void
ix86_emit_restore_reg_using_pop2 (rtx reg1, rtx reg2, bool ppx_p = false)
{
  struct machine_function *m = cfun->machine;
  const int offset = UNITS_PER_WORD * 2;
  rtx_insn *insn;

  rtx mem = gen_rtx_MEM (TImode, gen_rtx_POST_INC (Pmode,
						   stack_pointer_rtx));

  if (ppx_p)
    insn = emit_insn (gen_pop2p_di (reg1, mem, reg2));
  else
    insn = emit_insn (gen_pop2_di (reg1, mem, reg2));

  RTX_FRAME_RELATED_P (insn) = 1;

  rtx dwarf = NULL_RTX;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg1, dwarf);
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg2, dwarf);
  REG_NOTES (insn) = dwarf;
  m->fs.sp_offset -= offset;

  if (m->fs.cfa_reg == crtl->drap_reg
      && (REGNO (reg1) == REGNO (crtl->drap_reg)
	  || REGNO (reg2) == REGNO (crtl->drap_reg)))
    {
      /* Previously we'd represented the CFA as an expression
	 like *(%ebp - 8).  We've just popped that value from
	 the stack, which means we need to reset the CFA to
	 the drap register.  This will remain until we restore
	 the stack pointer.  */
      add_reg_note (insn, REG_CFA_DEF_CFA,
		    REGNO (reg1) == REGNO (crtl->drap_reg) ? reg1 : reg2);
      RTX_FRAME_RELATED_P (insn) = 1;

      /* This means that the DRAP register is valid for addressing too.  */
      m->fs.drap_valid = true;
      return;
    }

  if (m->fs.cfa_reg == stack_pointer_rtx)
    {
      rtx x = plus_constant (Pmode, stack_pointer_rtx, offset);
      x = gen_rtx_SET (stack_pointer_rtx, x);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
      RTX_FRAME_RELATED_P (insn) = 1;

      m->fs.cfa_offset -= offset;
    }

  /* When the frame pointer is the CFA, and we pop it, we are
     swapping back to the stack pointer as the CFA.  This happens
     for stack frames that don't allocate other data, so we assume
     the stack pointer is now pointing at the return address, i.e.
     the function entry state, which makes the offset be 1 word.  */
  if (reg1 == hard_frame_pointer_rtx || reg2 == hard_frame_pointer_rtx)
    {
      m->fs.fp_valid = false;
      if (m->fs.cfa_reg == hard_frame_pointer_rtx)
	{
	  m->fs.cfa_reg = stack_pointer_rtx;
	  m->fs.cfa_offset -= offset;

	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx,
				       m->fs.cfa_offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Emit code to restore saved registers using POP insns.  */

static void
ix86_emit_restore_regs_using_pop (bool ppx_p)
{
  unsigned int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, false, true))
      ix86_emit_restore_reg_using_pop (gen_rtx_REG (word_mode, regno), ppx_p);
}

/* Emit code to restore saved registers using POP2 insns.  */

static void
ix86_emit_restore_regs_using_pop2 (void)
{
  int regno;
  int regno_list[2];
  regno_list[0] = regno_list[1] = -1;
  int loaded_regnum = 0;
  bool aligned = cfun->machine->fs.sp_offset % 16 == 0;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, false, true))
      {
	if (aligned)
	  {
	    regno_list[loaded_regnum++] = regno;
	    if (loaded_regnum == 2)
	      {
		gcc_assert (regno_list[0] != -1
			    && regno_list[1] != -1
			    && regno_list[0] != regno_list[1]);

		ix86_emit_restore_reg_using_pop2 (gen_rtx_REG (word_mode,
							       regno_list[0]),
						  gen_rtx_REG (word_mode,
							       regno_list[1]),
						  TARGET_APX_PPX);
		loaded_regnum = 0;
		regno_list[0] = regno_list[1] = -1;
	      }
	  }
	else
	  {
	    ix86_emit_restore_reg_using_pop (gen_rtx_REG (word_mode, regno),
					     TARGET_APX_PPX);
	    aligned = true;
	  }
      }

  if (loaded_regnum == 1)
    ix86_emit_restore_reg_using_pop (gen_rtx_REG (word_mode, regno_list[0]),
				     TARGET_APX_PPX);
}

/* Emit code and notes for the LEAVE instruction.  If insn is non-null,
   omits the emit and only attaches the notes.  */

static void
ix86_emit_leave (rtx_insn *insn)
{
  struct machine_function *m = cfun->machine;

  if (!insn)
    insn = emit_insn (gen_leave (word_mode));

  ix86_add_queued_cfa_restore_notes (insn);

  gcc_assert (m->fs.fp_valid);
  m->fs.sp_valid = true;
  m->fs.sp_realigned = false;
  m->fs.sp_offset = m->fs.fp_offset - UNITS_PER_WORD;
  m->fs.fp_valid = false;

  if (m->fs.cfa_reg == hard_frame_pointer_rtx)
    {
      m->fs.cfa_reg = stack_pointer_rtx;
      m->fs.cfa_offset = m->fs.sp_offset;

      add_reg_note (insn, REG_CFA_DEF_CFA,
		    plus_constant (Pmode, stack_pointer_rtx,
				   m->fs.sp_offset));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  ix86_add_cfa_restore_note (insn, hard_frame_pointer_rtx,
			     m->fs.fp_offset);
}

/* Emit code to restore saved registers using MOV insns.
   First register is restored from CFA - CFA_OFFSET.  */
static void
ix86_emit_restore_regs_using_mov (HOST_WIDE_INT cfa_offset,
				  bool maybe_eh_return)
{
  struct machine_function *m = cfun->machine;
  unsigned int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (GENERAL_REGNO_P (regno) && ix86_save_reg (regno, maybe_eh_return, true))
      {
	rtx reg = gen_rtx_REG (word_mode, regno);
	rtx mem;
	rtx_insn *insn;

	mem = choose_baseaddr (cfa_offset, NULL);
	mem = gen_frame_mem (word_mode, mem);
	insn = emit_move_insn (reg, mem);

        if (m->fs.cfa_reg == crtl->drap_reg && regno == REGNO (crtl->drap_reg))
	  {
	    /* Previously we'd represented the CFA as an expression
	       like *(%ebp - 8).  We've just popped that value from
	       the stack, which means we need to reset the CFA to
	       the drap register.  This will remain until we restore
	       the stack pointer.  */
	    add_reg_note (insn, REG_CFA_DEF_CFA, reg);
	    RTX_FRAME_RELATED_P (insn) = 1;

	    /* This means that the DRAP register is valid for addressing.  */
	    m->fs.drap_valid = true;
	  }
	else
	  ix86_add_cfa_restore_note (NULL, reg, cfa_offset);

	cfa_offset -= UNITS_PER_WORD;
      }
}

/* Emit code to restore saved registers using MOV insns.
   First register is restored from CFA - CFA_OFFSET.  */
static void
ix86_emit_restore_sse_regs_using_mov (HOST_WIDE_INT cfa_offset,
				      bool maybe_eh_return)
{
  unsigned int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (SSE_REGNO_P (regno) && ix86_save_reg (regno, maybe_eh_return, true))
      {
	rtx reg = gen_rtx_REG (V4SFmode, regno);
	rtx mem;
	unsigned int align = GET_MODE_ALIGNMENT (V4SFmode);

	mem = choose_baseaddr (cfa_offset, &align);
	mem = gen_rtx_MEM (V4SFmode, mem);

	/* The location aligment depends upon the base register.  */
	align = MIN (GET_MODE_ALIGNMENT (V4SFmode), align);
	gcc_assert (! (cfa_offset & (align / BITS_PER_UNIT - 1)));
	set_mem_align (mem, align);
	emit_insn (gen_rtx_SET (reg, mem));

	ix86_add_cfa_restore_note (NULL, reg, cfa_offset);

	cfa_offset -= GET_MODE_SIZE (V4SFmode);
      }
}

static void
ix86_emit_outlined_ms2sysv_restore (const struct ix86_frame &frame,
				  bool use_call, int style)
{
  struct machine_function *m = cfun->machine;
  const unsigned ncregs = NUM_X86_64_MS_CLOBBERED_REGS
			  + m->call_ms2sysv_extra_regs;
  rtvec v;
  unsigned int elems_needed, align, i, vi = 0;
  rtx_insn *insn;
  rtx sym, tmp;
  rtx rsi = gen_rtx_REG (word_mode, SI_REG);
  rtx r10 = NULL_RTX;
  const class xlogue_layout &xlogue = xlogue_layout::get_instance ();
  HOST_WIDE_INT stub_ptr_offset = xlogue.get_stub_ptr_offset ();
  HOST_WIDE_INT rsi_offset = frame.stack_realign_offset + stub_ptr_offset;
  rtx rsi_frame_load = NULL_RTX;
  HOST_WIDE_INT rsi_restore_offset = (HOST_WIDE_INT)-1;
  enum xlogue_stub stub;

  gcc_assert (!m->fs.fp_valid || frame_pointer_needed);

  /* If using a realigned stack, we should never start with padding.  */
  gcc_assert (!stack_realign_fp || !xlogue.get_stack_align_off_in ());

  /* Setup RSI as the stub's base pointer.  */
  align = GET_MODE_ALIGNMENT (V4SFmode);
  tmp = choose_baseaddr (rsi_offset, &align, SI_REG);
  gcc_assert (align >= GET_MODE_ALIGNMENT (V4SFmode));

  emit_insn (gen_rtx_SET (rsi, tmp));

  /* Get a symbol for the stub.  */
  if (frame_pointer_needed)
    stub = use_call ? XLOGUE_STUB_RESTORE_HFP
		    : XLOGUE_STUB_RESTORE_HFP_TAIL;
  else
    stub = use_call ? XLOGUE_STUB_RESTORE
		    : XLOGUE_STUB_RESTORE_TAIL;
  sym = xlogue.get_stub_rtx (stub);

  elems_needed = ncregs;
  if (use_call)
    elems_needed += 1;
  else
    elems_needed += frame_pointer_needed ? 5 : 3;
  v = rtvec_alloc (elems_needed);

  /* We call the epilogue stub when we need to pop incoming args or we are
     doing a sibling call as the tail.  Otherwise, we will emit a jmp to the
     epilogue stub and it is the tail-call.  */
  if (use_call)
      RTVEC_ELT (v, vi++) = gen_rtx_USE (VOIDmode, sym);
  else
    {
      RTVEC_ELT (v, vi++) = ret_rtx;
      RTVEC_ELT (v, vi++) = gen_rtx_USE (VOIDmode, sym);
      if (frame_pointer_needed)
	{
	  rtx rbp = gen_rtx_REG (DImode, BP_REG);
	  gcc_assert (m->fs.fp_valid);
	  gcc_assert (m->fs.cfa_reg == hard_frame_pointer_rtx);

	  tmp = plus_constant (DImode, rbp, 8);
	  RTVEC_ELT (v, vi++) = gen_rtx_SET (stack_pointer_rtx, tmp);
	  RTVEC_ELT (v, vi++) = gen_rtx_SET (rbp, gen_rtx_MEM (DImode, rbp));
	  tmp = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode));
	  RTVEC_ELT (v, vi++) = gen_rtx_CLOBBER (VOIDmode, tmp);
	}
      else
	{
	  /* If no hard frame pointer, we set R10 to the SP restore value.  */
	  gcc_assert (!m->fs.fp_valid);
	  gcc_assert (m->fs.cfa_reg == stack_pointer_rtx);
	  gcc_assert (m->fs.sp_valid);

	  r10 = gen_rtx_REG (DImode, R10_REG);
	  tmp = plus_constant (Pmode, rsi, stub_ptr_offset);
	  emit_insn (gen_rtx_SET (r10, tmp));

	  RTVEC_ELT (v, vi++) = gen_rtx_SET (stack_pointer_rtx, r10);
	}
    }

  /* Generate frame load insns and restore notes.  */
  for (i = 0; i < ncregs; ++i)
    {
      const xlogue_layout::reginfo &r = xlogue.get_reginfo (i);
      machine_mode mode = SSE_REGNO_P (r.regno) ? V4SFmode : word_mode;
      rtx reg, frame_load;

      reg = gen_rtx_REG (mode, r.regno);
      frame_load = gen_frame_load (reg, rsi, r.offset);

      /* Save RSI frame load insn & note to add last.  */
      if (r.regno == SI_REG)
	{
	  gcc_assert (!rsi_frame_load);
	  rsi_frame_load = frame_load;
	  rsi_restore_offset = r.offset;
	}
      else
	{
	  RTVEC_ELT (v, vi++) = frame_load;
	  ix86_add_cfa_restore_note (NULL, reg, r.offset);
	}
    }

  /* Add RSI frame load & restore note at the end.  */
  gcc_assert (rsi_frame_load);
  gcc_assert (rsi_restore_offset != (HOST_WIDE_INT)-1);
  RTVEC_ELT (v, vi++) = rsi_frame_load;
  ix86_add_cfa_restore_note (NULL, gen_rtx_REG (DImode, SI_REG),
			     rsi_restore_offset);

  /* Finally, for tail-call w/o a hard frame pointer, set SP to R10.  */
  if (!use_call && !frame_pointer_needed)
    {
      gcc_assert (m->fs.sp_valid);
      gcc_assert (!m->fs.sp_realigned);

      /* At this point, R10 should point to frame.stack_realign_offset.  */
      if (m->fs.cfa_reg == stack_pointer_rtx)
	m->fs.cfa_offset += m->fs.sp_offset - frame.stack_realign_offset;
      m->fs.sp_offset = frame.stack_realign_offset;
    }

  gcc_assert (vi == (unsigned int)GET_NUM_ELEM (v));
  tmp = gen_rtx_PARALLEL (VOIDmode, v);
  if (use_call)
      insn = emit_insn (tmp);
  else
    {
      insn = emit_jump_insn (tmp);
      JUMP_LABEL (insn) = ret_rtx;

      if (frame_pointer_needed)
	ix86_emit_leave (insn);
      else
	{
	  /* Need CFA adjust note.  */
	  tmp = gen_rtx_SET (stack_pointer_rtx, r10);
	  add_reg_note (insn, REG_CFA_ADJUST_CFA, tmp);
	}
    }

  RTX_FRAME_RELATED_P (insn) = true;
  ix86_add_queued_cfa_restore_notes (insn);

  /* If we're not doing a tail-call, we need to adjust the stack.  */
  if (use_call && m->fs.sp_valid)
    {
      HOST_WIDE_INT dealloc = m->fs.sp_offset - frame.stack_realign_offset;
      pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (dealloc), style,
				m->fs.cfa_reg == stack_pointer_rtx);
    }
}

/* Restore function stack, frame, and registers.  */

void
ix86_expand_epilogue (int style)
{
  struct machine_function *m = cfun->machine;
  struct machine_frame_state frame_state_save = m->fs;
  bool restore_regs_via_mov;
  bool using_drap;
  bool restore_stub_is_tail = false;

  if (ix86_function_naked (current_function_decl))
    {
      /* The program should not reach this point.  */
      emit_insn (gen_ud2 ());
      return;
    }

  ix86_finalize_stack_frame_flags ();
  const struct ix86_frame &frame = cfun->machine->frame;

  m->fs.sp_realigned = stack_realign_fp;
  m->fs.sp_valid = stack_realign_fp
		   || !frame_pointer_needed
		   || crtl->sp_is_unchanging;
  gcc_assert (!m->fs.sp_valid
	      || m->fs.sp_offset == frame.stack_pointer_offset);

  /* The FP must be valid if the frame pointer is present.  */
  gcc_assert (frame_pointer_needed == m->fs.fp_valid);
  gcc_assert (!m->fs.fp_valid
	      || m->fs.fp_offset == frame.hard_frame_pointer_offset);

  /* We must have *some* valid pointer to the stack frame.  */
  gcc_assert (m->fs.sp_valid || m->fs.fp_valid);

  /* The DRAP is never valid at this point.  */
  gcc_assert (!m->fs.drap_valid);

  /* See the comment about red zone and frame
     pointer usage in ix86_expand_prologue.  */
  if (frame_pointer_needed && frame.red_zone_size)
    emit_insn (gen_memory_blockage ());

  using_drap = crtl->drap_reg && crtl->stack_realign_needed;
  gcc_assert (!using_drap || m->fs.cfa_reg == crtl->drap_reg);

  /* Determine the CFA offset of the end of the red-zone.  */
  m->fs.red_zone_offset = 0;
  if (ix86_using_red_zone () && crtl->args.pops_args < 65536)
    {
      /* The red-zone begins below return address and error code in
	 exception handler.  */
      m->fs.red_zone_offset = RED_ZONE_SIZE + INCOMING_FRAME_SP_OFFSET;

      /* When the register save area is in the aligned portion of
         the stack, determine the maximum runtime displacement that
	 matches up with the aligned frame.  */
      if (stack_realign_drap)
	m->fs.red_zone_offset -= (crtl->stack_alignment_needed / BITS_PER_UNIT
				  + UNITS_PER_WORD);
    }

  HOST_WIDE_INT reg_save_offset = frame.reg_save_offset;

  /* Special care must be taken for the normal return case of a function
     using eh_return: the eax and edx registers are marked as saved, but
     not restored along this path.  Adjust the save location to match.  */
  if (crtl->calls_eh_return && style != 2)
    reg_save_offset -= 2 * UNITS_PER_WORD;

  /* EH_RETURN requires the use of moves to function properly.  */
  if (crtl->calls_eh_return)
    restore_regs_via_mov = true;
  /* SEH requires the use of pops to identify the epilogue.  */
  else if (TARGET_SEH)
    restore_regs_via_mov = false;
  /* If we're only restoring one register and sp cannot be used then
     using a move instruction to restore the register since it's
     less work than reloading sp and popping the register.  */
  else if (!sp_valid_at (frame.hfp_save_offset) && frame.nregs <= 1)
    restore_regs_via_mov = true;
  else if (TARGET_EPILOGUE_USING_MOVE
	   && cfun->machine->use_fast_prologue_epilogue
	   && (frame.nregs > 1
	       || m->fs.sp_offset != reg_save_offset))
    restore_regs_via_mov = true;
  else if (frame_pointer_needed
	   && !frame.nregs
	   && m->fs.sp_offset != reg_save_offset)
    restore_regs_via_mov = true;
  else if (frame_pointer_needed
	   && TARGET_USE_LEAVE
	   && cfun->machine->use_fast_prologue_epilogue
	   && frame.nregs == 1)
    restore_regs_via_mov = true;
  else
    restore_regs_via_mov = false;

  if (restore_regs_via_mov || frame.nsseregs)
    {
      /* Ensure that the entire register save area is addressable via
	 the stack pointer, if we will restore SSE regs via sp.  */
      if (TARGET_64BIT
	  && m->fs.sp_offset > 0x7fffffff
	  && sp_valid_at (frame.stack_realign_offset + 1)
	  && (frame.nsseregs + frame.nregs) != 0)
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (m->fs.sp_offset
					      - frame.sse_reg_save_offset),
				     style,
				     m->fs.cfa_reg == stack_pointer_rtx);
	}
    }

  /* If there are any SSE registers to restore, then we have to do it
     via moves, since there's obviously no pop for SSE regs.  */
  if (frame.nsseregs)
    ix86_emit_restore_sse_regs_using_mov (frame.sse_reg_save_offset,
					  style == 2);

  if (m->call_ms2sysv)
    {
      int pop_incoming_args = crtl->args.pops_args && crtl->args.size;

      /* We cannot use a tail-call for the stub if:
	 1. We have to pop incoming args,
	 2. We have additional int regs to restore, or
	 3. A sibling call will be the tail-call, or
	 4. We are emitting an eh_return_internal epilogue.

	 TODO: Item 4 has not yet tested!

	 If any of the above are true, we will call the stub rather than
	 jump to it.  */
      restore_stub_is_tail = !(pop_incoming_args || frame.nregs || style != 1);
      ix86_emit_outlined_ms2sysv_restore (frame, !restore_stub_is_tail, style);
    }

  /* If using out-of-line stub that is a tail-call, then...*/
  if (m->call_ms2sysv && restore_stub_is_tail)
    {
      /* TODO: parinoid tests. (remove eventually)  */
      gcc_assert (m->fs.sp_valid);
      gcc_assert (!m->fs.sp_realigned);
      gcc_assert (!m->fs.fp_valid);
      gcc_assert (!m->fs.realigned);
      gcc_assert (m->fs.sp_offset == UNITS_PER_WORD);
      gcc_assert (!crtl->drap_reg);
      gcc_assert (!frame.nregs);
    }
  else if (restore_regs_via_mov)
    {
      rtx t;

      if (frame.nregs)
	ix86_emit_restore_regs_using_mov (reg_save_offset, style == 2);

      /* eh_return epilogues need %ecx added to the stack pointer.  */
      if (style == 2)
	{
	  rtx sa = EH_RETURN_STACKADJ_RTX;
	  rtx_insn *insn;

	  /* Stack realignment doesn't work with eh_return.  */
	  if (crtl->stack_realign_needed)
	    sorry ("Stack realignment not supported with "
		   "%<__builtin_eh_return%>");

	  /* regparm nested functions don't work with eh_return.  */
	  if (ix86_static_chain_on_stack)
	    sorry ("regparm nested function not supported with "
		   "%<__builtin_eh_return%>");

	  if (frame_pointer_needed)
	    {
	      t = gen_rtx_PLUS (Pmode, hard_frame_pointer_rtx, sa);
	      t = plus_constant (Pmode, t, m->fs.fp_offset - UNITS_PER_WORD);
	      emit_insn (gen_rtx_SET (sa, t));

	      /* NB: eh_return epilogues must restore the frame pointer
		 in word_mode since the upper 32 bits of RBP register
		 can have any values.  */
	      t = gen_frame_mem (word_mode, hard_frame_pointer_rtx);
	      rtx frame_reg = gen_rtx_REG (word_mode,
					   HARD_FRAME_POINTER_REGNUM);
	      insn = emit_move_insn (frame_reg, t);

	      /* Note that we use SA as a temporary CFA, as the return
		 address is at the proper place relative to it.  We
		 pretend this happens at the FP restore insn because
		 prior to this insn the FP would be stored at the wrong
		 offset relative to SA, and after this insn we have no
		 other reasonable register to use for the CFA.  We don't
		 bother resetting the CFA to the SP for the duration of
		 the return insn, unless the control flow instrumentation
		 is done.  In this case the SP is used later and we have
		 to reset CFA to SP.  */
	      add_reg_note (insn, REG_CFA_DEF_CFA,
			    plus_constant (Pmode, sa, UNITS_PER_WORD));
	      ix86_add_queued_cfa_restore_notes (insn);
	      add_reg_note (insn, REG_CFA_RESTORE, frame_reg);
	      RTX_FRAME_RELATED_P (insn) = 1;

	      m->fs.cfa_reg = sa;
	      m->fs.cfa_offset = UNITS_PER_WORD;
	      m->fs.fp_valid = false;

	      pro_epilogue_adjust_stack (stack_pointer_rtx, sa,
					 const0_rtx, style,
					 flag_cf_protection);
	    }
	  else
	    {
	      t = gen_rtx_PLUS (Pmode, stack_pointer_rtx, sa);
	      t = plus_constant (Pmode, t, m->fs.sp_offset - UNITS_PER_WORD);
	      insn = emit_insn (gen_rtx_SET (stack_pointer_rtx, t));
	      ix86_add_queued_cfa_restore_notes (insn);

	      gcc_assert (m->fs.cfa_reg == stack_pointer_rtx);
	      if (m->fs.cfa_offset != UNITS_PER_WORD)
		{
		  m->fs.cfa_offset = UNITS_PER_WORD;
		  add_reg_note (insn, REG_CFA_DEF_CFA,
				plus_constant (Pmode, stack_pointer_rtx,
					       UNITS_PER_WORD));
		  RTX_FRAME_RELATED_P (insn) = 1;
		}
	    }
	  m->fs.sp_offset = UNITS_PER_WORD;
	  m->fs.sp_valid = true;
	  m->fs.sp_realigned = false;
	}
    }
  else
    {
      /* SEH requires that the function end with (1) a stack adjustment
	 if necessary, (2) a sequence of pops, and (3) a return or
	 jump instruction.  Prevent insns from the function body from
	 being scheduled into this sequence.  */
      if (TARGET_SEH)
	{
	  /* Prevent a catch region from being adjacent to the standard
	     epilogue sequence.  Unfortunately neither crtl->uses_eh_lsda
	     nor several other flags that would be interesting to test are
	     set up yet.  */
	  if (flag_non_call_exceptions)
	    emit_insn (gen_nops (const1_rtx));
	  else
	    emit_insn (gen_blockage ());
	}

      /* First step is to deallocate the stack frame so that we can
	 pop the registers.  If the stack pointer was realigned, it needs
	 to be restored now.  Also do it on SEH target for very large
	 frame as the emitted instructions aren't allowed by the ABI
	 in epilogues.  */
      if (!m->fs.sp_valid || m->fs.sp_realigned
 	  || (TARGET_SEH
	      && (m->fs.sp_offset - reg_save_offset
		  >= SEH_MAX_FRAME_SIZE)))
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx, hard_frame_pointer_rtx,
				     GEN_INT (m->fs.fp_offset
					      - reg_save_offset),
				     style, false);
	}
      else if (m->fs.sp_offset != reg_save_offset)
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				     GEN_INT (m->fs.sp_offset
					      - reg_save_offset),
				     style,
				     m->fs.cfa_reg == stack_pointer_rtx);
	}

      if (TARGET_APX_PUSH2POP2
	  && ix86_can_use_push2pop2 ()
	  && m->func_type == TYPE_NORMAL)
	ix86_emit_restore_regs_using_pop2 ();
      else
	ix86_emit_restore_regs_using_pop (TARGET_APX_PPX);
    }

  /* If we used a stack pointer and haven't already got rid of it,
     then do so now.  */
  if (m->fs.fp_valid)
    {
      /* If the stack pointer is valid and pointing at the frame
	 pointer store address, then we only need a pop.  */
      if (sp_valid_at (frame.hfp_save_offset)
	  && m->fs.sp_offset == frame.hfp_save_offset)
	ix86_emit_restore_reg_using_pop (hard_frame_pointer_rtx);
      /* Leave results in shorter dependency chains on CPUs that are
	 able to grok it fast.  */
      else if (TARGET_USE_LEAVE
	       || optimize_bb_for_size_p (EXIT_BLOCK_PTR_FOR_FN (cfun))
	       || !cfun->machine->use_fast_prologue_epilogue)
	ix86_emit_leave (NULL);
      else
        {
	  pro_epilogue_adjust_stack (stack_pointer_rtx,
				     hard_frame_pointer_rtx,
				     const0_rtx, style, !using_drap);
	  ix86_emit_restore_reg_using_pop (hard_frame_pointer_rtx);
        }
    }

  if (using_drap)
    {
      int param_ptr_offset = UNITS_PER_WORD;
      rtx_insn *insn;

      gcc_assert (stack_realign_drap);

      if (ix86_static_chain_on_stack)
	param_ptr_offset += UNITS_PER_WORD;
      if (!call_used_or_fixed_reg_p (REGNO (crtl->drap_reg)))
	param_ptr_offset += UNITS_PER_WORD;

      insn = emit_insn (gen_rtx_SET
			(stack_pointer_rtx,
			 plus_constant (Pmode, crtl->drap_reg,
					-param_ptr_offset)));
      m->fs.cfa_reg = stack_pointer_rtx;
      m->fs.cfa_offset = param_ptr_offset;
      m->fs.sp_offset = param_ptr_offset;
      m->fs.realigned = false;

      add_reg_note (insn, REG_CFA_DEF_CFA,
		    plus_constant (Pmode, stack_pointer_rtx,
				   param_ptr_offset));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (!call_used_or_fixed_reg_p (REGNO (crtl->drap_reg)))
	ix86_emit_restore_reg_using_pop (crtl->drap_reg);
    }

  /* At this point the stack pointer must be valid, and we must have
     restored all of the registers.  We may not have deallocated the
     entire stack frame.  We've delayed this until now because it may
     be possible to merge the local stack deallocation with the
     deallocation forced by ix86_static_chain_on_stack.   */
  gcc_assert (m->fs.sp_valid);
  gcc_assert (!m->fs.sp_realigned);
  gcc_assert (!m->fs.fp_valid);
  gcc_assert (!m->fs.realigned);
  if (m->fs.sp_offset != UNITS_PER_WORD)
    {
      pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				 GEN_INT (m->fs.sp_offset - UNITS_PER_WORD),
				 style, true);
    }
  else
    ix86_add_queued_cfa_restore_notes (get_last_insn ());

  /* Sibcall epilogues don't want a return instruction.  */
  if (style == 0)
    {
      m->fs = frame_state_save;
      return;
    }

  if (cfun->machine->func_type != TYPE_NORMAL)
    emit_jump_insn (gen_interrupt_return ());
  else if (crtl->args.pops_args && crtl->args.size)
    {
      rtx popc = GEN_INT (crtl->args.pops_args);

      /* i386 can only pop 64K bytes.  If asked to pop more, pop return
	 address, do explicit add, and jump indirectly to the caller.  */

      if (crtl->args.pops_args >= 65536)
	{
	  rtx ecx = gen_rtx_REG (SImode, CX_REG);
	  rtx_insn *insn;

	  /* There is no "pascal" calling convention in any 64bit ABI.  */
	  gcc_assert (!TARGET_64BIT);

	  insn = emit_insn (gen_pop (ecx));
	  m->fs.cfa_offset -= UNITS_PER_WORD;
	  m->fs.sp_offset -= UNITS_PER_WORD;

	  rtx x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
	  x = gen_rtx_SET (stack_pointer_rtx, x);
	  add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
	  add_reg_note (insn, REG_CFA_REGISTER, gen_rtx_SET (ecx, pc_rtx));
	  RTX_FRAME_RELATED_P (insn) = 1;

	  pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				     popc, -1, true);
	  emit_jump_insn (gen_simple_return_indirect_internal (ecx));
	}
      else
	emit_jump_insn (gen_simple_return_pop_internal (popc));
    }
  else if (!m->call_ms2sysv || !restore_stub_is_tail)
    {
      /* In case of return from EH a simple return cannot be used
	 as a return address will be compared with a shadow stack
	 return address.  Use indirect jump instead.  */
      if (style == 2 && flag_cf_protection)
	{
	  /* Register used in indirect jump must be in word_mode.  But
	     Pmode may not be the same as word_mode for x32.  */
	  rtx ecx = gen_rtx_REG (word_mode, CX_REG);
	  rtx_insn *insn;

	  insn = emit_insn (gen_pop (ecx));
	  m->fs.cfa_offset -= UNITS_PER_WORD;
	  m->fs.sp_offset -= UNITS_PER_WORD;

	  rtx x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
	  x = gen_rtx_SET (stack_pointer_rtx, x);
	  add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
	  add_reg_note (insn, REG_CFA_REGISTER, gen_rtx_SET (ecx, pc_rtx));
	  RTX_FRAME_RELATED_P (insn) = 1;

	  emit_jump_insn (gen_simple_return_indirect_internal (ecx));
	}
      else
	emit_jump_insn (gen_simple_return_internal ());
    }

  /* Restore the state back to the state from the prologue,
     so that it's correct for the next epilogue.  */
  m->fs = frame_state_save;
}

/* Reset from the function's potential modifications.  */

static void
ix86_output_function_epilogue (FILE *file ATTRIBUTE_UNUSED)
{
  if (pic_offset_table_rtx
      && !ix86_use_pseudo_pic_reg ())
    SET_REGNO (pic_offset_table_rtx, REAL_PIC_OFFSET_TABLE_REGNUM);

  if (TARGET_MACHO)
    {
      rtx_insn *insn = get_last_insn ();
      rtx_insn *deleted_debug_label = NULL;

      /* Mach-O doesn't support labels at the end of objects, so if
         it looks like we might want one, take special action.
        First, collect any sequence of deleted debug labels.  */
      while (insn
	     && NOTE_P (insn)
	     && NOTE_KIND (insn) != NOTE_INSN_DELETED_LABEL)
	{
	  /* Don't insert a nop for NOTE_INSN_DELETED_DEBUG_LABEL
	     notes only, instead set their CODE_LABEL_NUMBER to -1,
	     otherwise there would be code generation differences
	     in between -g and -g0.  */
	  if (NOTE_P (insn) && NOTE_KIND (insn)
	      == NOTE_INSN_DELETED_DEBUG_LABEL)
	    deleted_debug_label = insn;
	  insn = PREV_INSN (insn);
	}

      /* If we have:
	 label:
	    barrier
	  then this needs to be detected, so skip past the barrier.  */

      if (insn && BARRIER_P (insn))
	insn = PREV_INSN (insn);

      /* Up to now we've only seen notes or barriers.  */
      if (insn)
	{
	  if (LABEL_P (insn)
	      || (NOTE_P (insn)
		  && NOTE_KIND (insn) == NOTE_INSN_DELETED_LABEL))
	    /* Trailing label.  */
	    fputs ("\tnop\n", file);
	  else if (cfun && ! cfun->is_thunk)
	    {
	      /* See if we have a completely empty function body, skipping
	         the special case of the picbase thunk emitted as asm.  */
	      while (insn && ! INSN_P (insn))
		insn = PREV_INSN (insn);
	      /* If we don't find any insns, we've got an empty function body;
		 I.e. completely empty - without a return or branch.  This is
		 taken as the case where a function body has been removed
		 because it contains an inline __builtin_unreachable().  GCC
		 declares that reaching __builtin_unreachable() means UB so
		 we're not obliged to do anything special; however, we want
		 non-zero-sized function bodies.  To meet this, and help the
		 user out, let's trap the case.  */
	      if (insn == NULL)
		fputs ("\tud2\n", file);
	    }
	}
      else if (deleted_debug_label)
	for (insn = deleted_debug_label; insn; insn = NEXT_INSN (insn))
	  if (NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL)
	    CODE_LABEL_NUMBER (insn) = -1;
    }
}

/* Implement TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY.  */

void
ix86_print_patchable_function_entry (FILE *file,
				     unsigned HOST_WIDE_INT patch_area_size,
				     bool record_p)
{
  if (cfun->machine->function_label_emitted)
    {
      /* NB: When ix86_print_patchable_function_entry is called after
	 function table has been emitted, we have inserted or queued
	 a pseudo UNSPECV_PATCHABLE_AREA instruction at the proper
	 place.  There is nothing to do here.  */
      return;
    }

  default_print_patchable_function_entry (file, patch_area_size,
					  record_p);
}

/* Output patchable area.  NB: default_print_patchable_function_entry
   isn't available in i386.md.  */

void
ix86_output_patchable_area (unsigned int patch_area_size,
			    bool record_p)
{
  default_print_patchable_function_entry (asm_out_file,
					  patch_area_size,
					  record_p);
}

/* Return a scratch register to use in the split stack prologue.  The
   split stack prologue is used for -fsplit-stack.  It is the first
   instructions in the function, even before the regular prologue.
   The scratch register can be any caller-saved register which is not
   used for parameters or for the static chain.  */

static unsigned int
split_stack_prologue_scratch_regno (void)
{
  if (TARGET_64BIT)
    return R11_REG;
  else
    {
      bool is_fastcall, is_thiscall;
      int regparm;

      is_fastcall = (lookup_attribute ("fastcall",
				       TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl)))
		     != NULL);
      is_thiscall = (lookup_attribute ("thiscall",
				       TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl)))
		     != NULL);
      regparm = ix86_function_regparm (TREE_TYPE (cfun->decl), cfun->decl);

      if (is_fastcall)
	{
	  if (DECL_STATIC_CHAIN (cfun->decl))
	    {
	      sorry ("%<-fsplit-stack%> does not support fastcall with "
		     "nested function");
	      return INVALID_REGNUM;
	    }
	  return AX_REG;
	}
      else if (is_thiscall)
        {
	  if (!DECL_STATIC_CHAIN (cfun->decl))
	    return DX_REG;
	  return AX_REG;
	}
      else if (regparm < 3)
	{
	  if (!DECL_STATIC_CHAIN (cfun->decl))
	    return CX_REG;
	  else
	    {
	      if (regparm >= 2)
		{
		  sorry ("%<-fsplit-stack%> does not support 2 register "
			 "parameters for a nested function");
		  return INVALID_REGNUM;
		}
	      return DX_REG;
	    }
	}
      else
	{
	  /* FIXME: We could make this work by pushing a register
	     around the addition and comparison.  */
	  sorry ("%<-fsplit-stack%> does not support 3 register parameters");
	  return INVALID_REGNUM;
	}
    }
}

/* A SYMBOL_REF for the function which allocates new stackspace for
   -fsplit-stack.  */

static GTY(()) rtx split_stack_fn;

/* A SYMBOL_REF for the more stack function when using the large
   model.  */

static GTY(()) rtx split_stack_fn_large;

/* Return location of the stack guard value in the TLS block.  */

rtx
ix86_split_stack_guard (void)
{
  int offset;
  addr_space_t as = DEFAULT_TLS_SEG_REG;
  rtx r;

  gcc_assert (flag_split_stack);

#ifdef TARGET_THREAD_SPLIT_STACK_OFFSET
  offset = TARGET_THREAD_SPLIT_STACK_OFFSET;
#else
  gcc_unreachable ();
#endif

  r = GEN_INT (offset);
  r = gen_const_mem (Pmode, r);
  set_mem_addr_space (r, as);

  return r;
}

/* Handle -fsplit-stack.  These are the first instructions in the
   function, even before the regular prologue.  */

void
ix86_expand_split_stack_prologue (void)
{
  HOST_WIDE_INT allocate;
  unsigned HOST_WIDE_INT args_size;
  rtx_code_label *label;
  rtx limit, current, allocate_rtx, call_fusage;
  rtx_insn *call_insn;
  unsigned int scratch_regno = INVALID_REGNUM;
  rtx scratch_reg = NULL_RTX;
  rtx_code_label *varargs_label = NULL;
  rtx fn;

  gcc_assert (flag_split_stack && reload_completed);

  ix86_finalize_stack_frame_flags ();
  struct ix86_frame &frame = cfun->machine->frame;
  allocate = frame.stack_pointer_offset - INCOMING_FRAME_SP_OFFSET;

  /* This is the label we will branch to if we have enough stack
     space.  We expect the basic block reordering pass to reverse this
     branch if optimizing, so that we branch in the unlikely case.  */
  label = gen_label_rtx ();

  /* We need to compare the stack pointer minus the frame size with
     the stack boundary in the TCB.  The stack boundary always gives
     us SPLIT_STACK_AVAILABLE bytes, so if we need less than that we
     can compare directly.  Otherwise we need to do an addition.  */

  limit = ix86_split_stack_guard ();

  if (allocate >= SPLIT_STACK_AVAILABLE
      || flag_force_indirect_call)
    {
      scratch_regno = split_stack_prologue_scratch_regno ();
      if (scratch_regno == INVALID_REGNUM)
	return;
    }

  if (allocate >= SPLIT_STACK_AVAILABLE)
    {
      rtx offset;

      /* We need a scratch register to hold the stack pointer minus
	 the required frame size.  Since this is the very start of the
	 function, the scratch register can be any caller-saved
	 register which is not used for parameters.  */
      offset = GEN_INT (- allocate);

      scratch_reg = gen_rtx_REG (Pmode, scratch_regno);
      if (!TARGET_64BIT || x86_64_immediate_operand (offset, Pmode))
	{
	  /* We don't use gen_add in this case because it will
	     want to split to lea, but when not optimizing the insn
	     will not be split after this point.  */
	  emit_insn (gen_rtx_SET (scratch_reg,
				  gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						offset)));
	}
      else
	{
	  emit_move_insn (scratch_reg, offset);
	  emit_insn (gen_add2_insn (scratch_reg, stack_pointer_rtx));
	}
      current = scratch_reg;
    }
  else
    current = stack_pointer_rtx;

  ix86_expand_branch (GEU, current, limit, label);
  rtx_insn *jump_insn = get_last_insn ();
  JUMP_LABEL (jump_insn) = label;

  /* Mark the jump as very likely to be taken.  */
  add_reg_br_prob_note (jump_insn, profile_probability::very_likely ());

  if (split_stack_fn == NULL_RTX)
    {
      split_stack_fn = gen_rtx_SYMBOL_REF (Pmode, "__morestack");
      SYMBOL_REF_FLAGS (split_stack_fn) |= SYMBOL_FLAG_LOCAL;
    }
  fn = split_stack_fn;

  /* Get more stack space.  We pass in the desired stack space and the
     size of the arguments to copy to the new stack.  In 32-bit mode
     we push the parameters; __morestack will return on a new stack
     anyhow.  In 64-bit mode we pass the parameters in r10 and
     r11.  */
  allocate_rtx = GEN_INT (allocate);
  args_size = crtl->args.size >= 0 ? (HOST_WIDE_INT) crtl->args.size : 0;
  call_fusage = NULL_RTX;
  rtx pop = NULL_RTX;
  if (TARGET_64BIT)
    {
      rtx reg10, reg11;

      reg10 = gen_rtx_REG (DImode, R10_REG);
      reg11 = gen_rtx_REG (DImode, R11_REG);

      /* If this function uses a static chain, it will be in %r10.
	 Preserve it across the call to __morestack.  */
      if (DECL_STATIC_CHAIN (cfun->decl))
	{
	  rtx rax;

	  rax = gen_rtx_REG (word_mode, AX_REG);
	  emit_move_insn (rax, gen_rtx_REG (word_mode, R10_REG));
	  use_reg (&call_fusage, rax);
	}

      if (flag_force_indirect_call
	  || ix86_cmodel == CM_LARGE || ix86_cmodel == CM_LARGE_PIC)
	{
	  HOST_WIDE_INT argval;

	  if (split_stack_fn_large == NULL_RTX)
	    {
	      split_stack_fn_large
		= gen_rtx_SYMBOL_REF (Pmode, "__morestack_large_model");
	      SYMBOL_REF_FLAGS (split_stack_fn_large) |= SYMBOL_FLAG_LOCAL;
	    }

	  fn = split_stack_fn_large;

	  if (ix86_cmodel == CM_LARGE_PIC)
	    {
	      rtx_code_label *label;
	      rtx x;

	      gcc_assert (Pmode == DImode);

	      label = gen_label_rtx ();
	      emit_label (label);
	      LABEL_PRESERVE_P (label) = 1;
	      emit_insn (gen_set_rip_rex64 (reg10, label));
	      emit_insn (gen_set_got_offset_rex64 (reg11, label));
	      emit_insn (gen_add2_insn (reg10, reg11));
	      x = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, fn), UNSPEC_GOT);
	      x = gen_rtx_CONST (Pmode, x);
	      emit_move_insn (reg11, x);
	      x = gen_rtx_PLUS (Pmode, reg10, reg11);
	      x = gen_const_mem (Pmode, x);
	      fn = copy_to_suggested_reg (x, reg11, Pmode);
	    }
	  else if (ix86_cmodel == CM_LARGE)
	    fn = copy_to_suggested_reg (fn, reg11, Pmode);

	  /* When using the large model we need to load the address
	     into a register, and we've run out of registers.  So we
	     switch to a different calling convention, and we call a
	     different function: __morestack_large.  We pass the
	     argument size in the upper 32 bits of r10 and pass the
	     frame size in the lower 32 bits.  */
	  gcc_assert ((allocate & HOST_WIDE_INT_C (0xffffffff)) == allocate);
	  gcc_assert ((args_size & 0xffffffff) == args_size);

	  argval = ((args_size << 16) << 16) + allocate;
	  emit_move_insn (reg10, GEN_INT (argval));
	}
      else
	{
	  emit_move_insn (reg10, allocate_rtx);
	  emit_move_insn (reg11, GEN_INT (args_size));
	  use_reg (&call_fusage, reg11);
	}

      use_reg (&call_fusage, reg10);
    }
  else
    {
      if (flag_force_indirect_call && flag_pic)
	{
	  rtx x;

	  gcc_assert (Pmode == SImode);

	  scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

	  emit_insn (gen_set_got (scratch_reg));
	  x = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, split_stack_fn),
			      UNSPEC_GOT);
	  x = gen_rtx_CONST (Pmode, x);
	  x = gen_rtx_PLUS (Pmode, scratch_reg, x);
	  x = gen_const_mem (Pmode, x);
	  fn = copy_to_suggested_reg (x, scratch_reg, Pmode);
	}

      rtx_insn *insn = emit_insn (gen_push (GEN_INT (args_size)));
      add_reg_note (insn, REG_ARGS_SIZE, GEN_INT (UNITS_PER_WORD));
      insn = emit_insn (gen_push (allocate_rtx));
      add_reg_note (insn, REG_ARGS_SIZE, GEN_INT (2 * UNITS_PER_WORD));
      pop = GEN_INT (2 * UNITS_PER_WORD);
    }

  if (flag_force_indirect_call && !register_operand (fn, VOIDmode))
    {
      scratch_reg = gen_rtx_REG (word_mode, scratch_regno);

      if (GET_MODE (fn) != word_mode)
	fn = gen_rtx_ZERO_EXTEND (word_mode, fn);

      fn = copy_to_suggested_reg (fn, scratch_reg, word_mode);
    }

  call_insn = ix86_expand_call (NULL_RTX, gen_rtx_MEM (QImode, fn),
				GEN_INT (UNITS_PER_WORD), constm1_rtx,
				pop, false);
  add_function_usage_to (call_insn, call_fusage);
  if (!TARGET_64BIT)
    add_reg_note (call_insn, REG_ARGS_SIZE, GEN_INT (0));
  /* Indicate that this function can't jump to non-local gotos.  */
  make_reg_eh_region_note_nothrow_nononlocal (call_insn);

  /* In order to make call/return prediction work right, we now need
     to execute a return instruction.  See
     libgcc/config/i386/morestack.S for the details on how this works.

     For flow purposes gcc must not see this as a return
     instruction--we need control flow to continue at the subsequent
     label.  Therefore, we use an unspec.  */
  gcc_assert (crtl->args.pops_args < 65536);
  rtx_insn *ret_insn
    = emit_insn (gen_split_stack_return (GEN_INT (crtl->args.pops_args)));

  if ((flag_cf_protection & CF_BRANCH))
    {
      /* Insert ENDBR since __morestack will jump back here via indirect
	 call.  */
      rtx cet_eb = gen_nop_endbr ();
      emit_insn_after (cet_eb, ret_insn);
    }

  /* If we are in 64-bit mode and this function uses a static chain,
     we saved %r10 in %rax before calling _morestack.  */
  if (TARGET_64BIT && DECL_STATIC_CHAIN (cfun->decl))
    emit_move_insn (gen_rtx_REG (word_mode, R10_REG),
		    gen_rtx_REG (word_mode, AX_REG));

  /* If this function calls va_start, we need to store a pointer to
     the arguments on the old stack, because they may not have been
     all copied to the new stack.  At this point the old stack can be
     found at the frame pointer value used by __morestack, because
     __morestack has set that up before calling back to us.  Here we
     store that pointer in a scratch register, and in
     ix86_expand_prologue we store the scratch register in a stack
     slot.  */
  if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
    {
      rtx frame_reg;
      int words;

      scratch_regno = split_stack_prologue_scratch_regno ();
      scratch_reg = gen_rtx_REG (Pmode, scratch_regno);
      frame_reg = gen_rtx_REG (Pmode, BP_REG);

      /* 64-bit:
	 fp -> old fp value
	       return address within this function
	       return address of caller of this function
	       stack arguments
	 So we add three words to get to the stack arguments.

	 32-bit:
	 fp -> old fp value
	       return address within this function
               first argument to __morestack
               second argument to __morestack
               return address of caller of this function
               stack arguments
         So we add five words to get to the stack arguments.
      */
      words = TARGET_64BIT ? 3 : 5;
      emit_insn (gen_rtx_SET (scratch_reg,
			      plus_constant (Pmode, frame_reg,
					     words * UNITS_PER_WORD)));

      varargs_label = gen_label_rtx ();
      emit_jump_insn (gen_jump (varargs_label));
      JUMP_LABEL (get_last_insn ()) = varargs_label;

      emit_barrier ();
    }

  emit_label (label);
  LABEL_NUSES (label) = 1;

  /* If this function calls va_start, we now have to set the scratch
     register for the case where we do not call __morestack.  In this
     case we need to set it based on the stack pointer.  */
  if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
    {
      emit_insn (gen_rtx_SET (scratch_reg,
			      plus_constant (Pmode, stack_pointer_rtx,
					     UNITS_PER_WORD)));

      emit_label (varargs_label);
      LABEL_NUSES (varargs_label) = 1;
    }
}

/* We may have to tell the dataflow pass that the split stack prologue
   is initializing a scratch register.  */

static void
ix86_live_on_entry (bitmap regs)
{
  if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
    {
      gcc_assert (flag_split_stack);
      bitmap_set_bit (regs, split_stack_prologue_scratch_regno ());
    }
}

/* Extract the parts of an RTL expression that is a valid memory address
   for an instruction.  Return false if the structure of the address is
   grossly off.  */

bool
ix86_decompose_address (rtx addr, struct ix86_address *out)
{
  rtx base = NULL_RTX, index = NULL_RTX, disp = NULL_RTX;
  rtx base_reg, index_reg;
  HOST_WIDE_INT scale = 1;
  rtx scale_rtx = NULL_RTX;
  rtx tmp;
  addr_space_t seg = ADDR_SPACE_GENERIC;

  /* Allow zero-extended SImode addresses,
     they will be emitted with addr32 prefix.  */
  if (TARGET_64BIT && GET_MODE (addr) == DImode)
    {
      if (GET_CODE (addr) == ZERO_EXTEND
	  && GET_MODE (XEXP (addr, 0)) == SImode)
	{
	  addr = XEXP (addr, 0);
	  if (CONST_INT_P (addr))
	    return false;
	}	      
      else if (GET_CODE (addr) == AND
	       && const_32bit_mask (XEXP (addr, 1), DImode))
	{
	  addr = lowpart_subreg (SImode, XEXP (addr, 0), DImode);
	  if (addr == NULL_RTX)
	    return false;

	  if (CONST_INT_P (addr))
	    return false;
	}
      else if (GET_CODE (addr) == AND)
	{
	  /* For ASHIFT inside AND, combine will not generate
	     canonical zero-extend. Merge mask for AND and shift_count
	     to check if it is canonical zero-extend.  */
	  tmp = XEXP (addr, 0);
	  rtx mask = XEXP (addr, 1);
	  if (tmp && GET_CODE(tmp) == ASHIFT)
	    {
	      rtx shift_val = XEXP (tmp, 1);
	      if (CONST_INT_P (mask) && CONST_INT_P (shift_val)
		  && (((unsigned HOST_WIDE_INT) INTVAL(mask)
		      | ((HOST_WIDE_INT_1U << INTVAL(shift_val)) - 1))
		      == 0xffffffff))
		{
		  addr = lowpart_subreg (SImode, XEXP (addr, 0),
					 DImode);
		}
	    }

	}
    }

  /* Allow SImode subregs of DImode addresses,
     they will be emitted with addr32 prefix.  */
  if (TARGET_64BIT && GET_MODE (addr) == SImode)
    {
      if (SUBREG_P (addr)
	  && GET_MODE (SUBREG_REG (addr)) == DImode)
	{
	  addr = SUBREG_REG (addr);
	  if (CONST_INT_P (addr))
	    return false;
	}
    }

  if (REG_P (addr))
    base = addr;
  else if (SUBREG_P (addr))
    {
      if (REG_P (SUBREG_REG (addr)))
	base = addr;
      else
	return false;
    }
  else if (GET_CODE (addr) == PLUS)
    {
      rtx addends[4], op;
      int n = 0, i;

      op = addr;
      do
	{
	  if (n >= 4)
	    return false;
	  addends[n++] = XEXP (op, 1);
	  op = XEXP (op, 0);
	}
      while (GET_CODE (op) == PLUS);
      if (n >= 4)
	return false;
      addends[n] = op;

      for (i = n; i >= 0; --i)
	{
	  op = addends[i];
	  switch (GET_CODE (op))
	    {
	    case MULT:
	      if (index)
		return false;
	      index = XEXP (op, 0);
	      scale_rtx = XEXP (op, 1);
	      break;

	    case ASHIFT:
	      if (index)
		return false;
	      index = XEXP (op, 0);
	      tmp = XEXP (op, 1);
	      if (!CONST_INT_P (tmp))
		return false;
	      scale = INTVAL (tmp);
	      if ((unsigned HOST_WIDE_INT) scale > 3)
		return false;
	      scale = 1 << scale;
	      break;

	    case ZERO_EXTEND:
	      op = XEXP (op, 0);
	      if (GET_CODE (op) != UNSPEC)
		return false;
	      /* FALLTHRU */

	    case UNSPEC:
	      if (XINT (op, 1) == UNSPEC_TP
	          && TARGET_TLS_DIRECT_SEG_REFS
	          && seg == ADDR_SPACE_GENERIC)
		seg = DEFAULT_TLS_SEG_REG;
	      else
		return false;
	      break;

	    case SUBREG:
	      if (!REG_P (SUBREG_REG (op)))
		return false;
	      /* FALLTHRU */

	    case REG:
	      if (!base)
		base = op;
	      else if (!index)
		index = op;
	      else
		return false;
	      break;

	    case CONST:
	    case CONST_INT:
	    case SYMBOL_REF:
	    case LABEL_REF:
	      if (disp)
		return false;
	      disp = op;
	      break;

	    default:
	      return false;
	    }
	}
    }
  else if (GET_CODE (addr) == MULT)
    {
      index = XEXP (addr, 0);		/* index*scale */
      scale_rtx = XEXP (addr, 1);
    }
  else if (GET_CODE (addr) == ASHIFT)
    {
      /* We're called for lea too, which implements ashift on occasion.  */
      index = XEXP (addr, 0);
      tmp = XEXP (addr, 1);
      if (!CONST_INT_P (tmp))
	return false;
      scale = INTVAL (tmp);
      if ((unsigned HOST_WIDE_INT) scale > 3)
	return false;
      scale = 1 << scale;
    }
  else
    disp = addr;			/* displacement */

  if (index)
    {
      if (REG_P (index))
	;
      else if (SUBREG_P (index)
	       && REG_P (SUBREG_REG (index)))
	;
      else
	return false;
    }

  /* Extract the integral value of scale.  */
  if (scale_rtx)
    {
      if (!CONST_INT_P (scale_rtx))
	return false;
      scale = INTVAL (scale_rtx);
    }

  base_reg = base && SUBREG_P (base) ? SUBREG_REG (base) : base;
  index_reg = index && SUBREG_P (index) ? SUBREG_REG (index) : index;

  /* Avoid useless 0 displacement.  */
  if (disp == const0_rtx && (base || index))
    disp = NULL_RTX;

  /* Allow arg pointer and stack pointer as index if there is not scaling.  */
  if (base_reg && index_reg && scale == 1
      && (REGNO (index_reg) == ARG_POINTER_REGNUM
	  || REGNO (index_reg) == FRAME_POINTER_REGNUM
	  || REGNO (index_reg) == SP_REG))
    {
      std::swap (base, index);
      std::swap (base_reg, index_reg);
    }

  /* Special case: %ebp cannot be encoded as a base without a displacement.
     Similarly %r13.  */
  if (!disp && base_reg
      && (REGNO (base_reg) == ARG_POINTER_REGNUM
	  || REGNO (base_reg) == FRAME_POINTER_REGNUM
	  || REGNO (base_reg) == BP_REG
	  || REGNO (base_reg) == R13_REG))
    disp = const0_rtx;

  /* Special case: on K6, [%esi] makes the instruction vector decoded.
     Avoid this by transforming to [%esi+0].
     Reload calls address legitimization without cfun defined, so we need
     to test cfun for being non-NULL. */
  if (TARGET_CPU_P (K6) && cfun && optimize_function_for_speed_p (cfun)
      && base_reg && !index_reg && !disp
      && REGNO (base_reg) == SI_REG)
    disp = const0_rtx;

  /* Special case: encode reg+reg instead of reg*2.  */
  if (!base && index && scale == 2)
    base = index, base_reg = index_reg, scale = 1;

  /* Special case: scaling cannot be encoded without base or displacement.  */
  if (!base && !disp && index && scale != 1)
    disp = const0_rtx;

  out->base = base;
  out->index = index;
  out->disp = disp;
  out->scale = scale;
  out->seg = seg;

  return true;
}

/* Return cost of the memory address x.
   For i386, it is better to use a complex address than let gcc copy
   the address into a reg and make a new pseudo.  But not if the address
   requires to two regs - that would mean more pseudos with longer
   lifetimes.  */
static int
ix86_address_cost (rtx x, machine_mode, addr_space_t, bool)
{
  struct ix86_address parts;
  int cost = 1;
  int ok = ix86_decompose_address (x, &parts);

  gcc_assert (ok);

  if (parts.base && SUBREG_P (parts.base))
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && SUBREG_P (parts.index))
    parts.index = SUBREG_REG (parts.index);

  /* Attempt to minimize number of registers in the address by increasing
     address cost for each used register.  We don't increase address cost
     for "pic_offset_table_rtx".  When a memopt with "pic_offset_table_rtx"
     is not invariant itself it most likely means that base or index is not
     invariant.  Therefore only "pic_offset_table_rtx" could be hoisted out,
     which is not profitable for x86.  */
  if (parts.base
      && (!REG_P (parts.base) || REGNO (parts.base) >= FIRST_PSEUDO_REGISTER)
      && (current_pass->type == GIMPLE_PASS
	  || !pic_offset_table_rtx
	  || !REG_P (parts.base)
	  || REGNO (pic_offset_table_rtx) != REGNO (parts.base)))
    cost++;

  if (parts.index
      && (!REG_P (parts.index) || REGNO (parts.index) >= FIRST_PSEUDO_REGISTER)
      && (current_pass->type == GIMPLE_PASS
	  || !pic_offset_table_rtx
	  || !REG_P (parts.index)
	  || REGNO (pic_offset_table_rtx) != REGNO (parts.index)))
    cost++;

  /* AMD-K6 don't like addresses with ModR/M set to 00_xxx_100b,
     since it's predecode logic can't detect the length of instructions
     and it degenerates to vector decoded.  Increase cost of such
     addresses here.  The penalty is minimally 2 cycles.  It may be worthwhile
     to split such addresses or even refuse such addresses at all.

     Following addressing modes are affected:
      [base+scale*index]
      [scale*index+disp]
      [base+index]

     The first and last case  may be avoidable by explicitly coding the zero in
     memory address, but I don't have AMD-K6 machine handy to check this
     theory.  */

  if (TARGET_CPU_P (K6)
      && ((!parts.disp && parts.base && parts.index && parts.scale != 1)
	  || (parts.disp && !parts.base && parts.index && parts.scale != 1)
	  || (!parts.disp && parts.base && parts.index && parts.scale == 1)))
    cost += 10;

  return cost;
}

/* Allow {LABEL | SYMBOL}_REF - SYMBOL_REF-FOR-PICBASE for Mach-O as
   this is used for to form addresses to local data when -fPIC is in
   use.  */

static bool
darwin_local_data_pic (rtx disp)
{
  return (GET_CODE (disp) == UNSPEC
	  && XINT (disp, 1) == UNSPEC_MACHOPIC_OFFSET);
}

/* True if the function symbol operand X should be loaded from GOT.
   If CALL_P is true, X is a call operand.

   NB: -mno-direct-extern-access doesn't force load from GOT for
   call.

   NB: In 32-bit mode, only non-PIC is allowed in inline assembly
   statements, since a PIC register could not be available at the
   call site.  */

bool
ix86_force_load_from_GOT_p (rtx x, bool call_p)
{
  return ((TARGET_64BIT || (!flag_pic && HAVE_AS_IX86_GOT32X))
	  && !TARGET_PECOFF && !TARGET_MACHO
	  && (!flag_pic || this_is_asm_operands)
	  && ix86_cmodel != CM_LARGE
	  && ix86_cmodel != CM_LARGE_PIC
	  && GET_CODE (x) == SYMBOL_REF
	  && ((!call_p
	       && (!ix86_direct_extern_access
		   || (SYMBOL_REF_DECL (x)
		       && lookup_attribute ("nodirect_extern_access",
					    DECL_ATTRIBUTES (SYMBOL_REF_DECL (x))))))
	      || (SYMBOL_REF_FUNCTION_P (x)
		  && (!flag_plt
		      || (SYMBOL_REF_DECL (x)
			  && lookup_attribute ("noplt",
					       DECL_ATTRIBUTES (SYMBOL_REF_DECL (x)))))))
	  && !SYMBOL_REF_LOCAL_P (x));
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

static bool
ix86_legitimate_constant_p (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (!CONST_INT_P (XEXP (x, 1)))
	    return false;
	  x = XEXP (x, 0);
	}

      if (TARGET_MACHO && darwin_local_data_pic (x))
	return true;

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (x) == UNSPEC)
	switch (XINT (x, 1))
	  {
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	  case UNSPEC_PLTOFF:
	    return TARGET_64BIT;
	  case UNSPEC_TPOFF:
	  case UNSPEC_NTPOFF:
	    x = XVECEXP (x, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_EXEC);
	  case UNSPEC_DTPOFF:
	    x = XVECEXP (x, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC);
	  default:
	    return false;
	  }

      /* We must have drilled down to a symbol.  */
      if (GET_CODE (x) == LABEL_REF)
	return true;
      if (GET_CODE (x) != SYMBOL_REF)
	return false;
      /* FALLTHRU */

    case SYMBOL_REF:
      /* TLS symbols are never valid.  */
      if (SYMBOL_REF_TLS_MODEL (x))
	return false;

      /* DLLIMPORT symbols are never valid.  */
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && SYMBOL_REF_DLLIMPORT_P (x))
	return false;

#if TARGET_MACHO
      /* mdynamic-no-pic */
      if (MACHO_DYNAMIC_NO_PIC_P)
	return machopic_symbol_defined_p (x);
#endif

      /* External function address should be loaded
	 via the GOT slot to avoid PLT.  */
      if (ix86_force_load_from_GOT_p (x))
	return false;

      break;

    CASE_CONST_SCALAR_INT:
      if (ix86_endbr_immediate_operand (x, VOIDmode))
	return false;

      switch (mode)
	{
	case E_TImode:
	  if (TARGET_64BIT)
	    return true;
	  /* FALLTHRU */
	case E_OImode:
	case E_XImode:
	  if (!standard_sse_constant_p (x, mode)
	      && GET_MODE_SIZE (TARGET_AVX512F && TARGET_EVEX512
				? XImode
				: (TARGET_AVX
				   ? OImode
				   : (TARGET_SSE2
				      ? TImode : DImode))) < GET_MODE_SIZE (mode))
	    return false;
	default:
	  break;
	}
      break;

    case CONST_VECTOR:
      if (!standard_sse_constant_p (x, mode))
	return false;
      break;

    case CONST_DOUBLE:
      if (mode == E_BFmode)
	return false;

    default:
      break;
    }

  /* Otherwise we handle everything else in the move patterns.  */
  return true;
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible for the address of thread-local symbols, which
   is checked above.  */

static bool
ix86_cannot_force_const_mem (machine_mode mode, rtx x)
{
  /* We can put any immediate constant in memory.  */
  switch (GET_CODE (x))
    {
    CASE_CONST_ANY:
      return false;

    default:
      break;
    }

  return !ix86_legitimate_constant_p (mode, x);
}

/*  Nonzero if the symbol is marked as dllimport, or as stub-variable,
    otherwise zero.  */

static bool
is_imported_p (rtx x)
{
  if (!TARGET_DLLIMPORT_DECL_ATTRIBUTES
      || GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_DLLIMPORT_P (x) || SYMBOL_REF_STUBVAR_P (x);
}


/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P.  */

bool
legitimate_pic_operand_p (rtx x)
{
  rtx inner;

  switch (GET_CODE (x))
    {
    case CONST:
      inner = XEXP (x, 0);
      if (GET_CODE (inner) == PLUS
	  && CONST_INT_P (XEXP (inner, 1)))
	inner = XEXP (inner, 0);

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (inner) == UNSPEC)
	switch (XINT (inner, 1))
	  {
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	  case UNSPEC_PLTOFF:
	    return TARGET_64BIT;
	  case UNSPEC_TPOFF:
	    x = XVECEXP (inner, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_EXEC);
	  case UNSPEC_MACHOPIC_OFFSET:
	    return legitimate_pic_address_disp_p (x);
	  default:
	    return false;
	  }
      /* FALLTHRU */

    case SYMBOL_REF:
    case LABEL_REF:
      return legitimate_pic_address_disp_p (x);

    default:
      return true;
    }
}

/* Determine if a given CONST RTX is a valid memory displacement
   in PIC mode.  */

bool
legitimate_pic_address_disp_p (rtx disp)
{
  bool saw_plus;

  /* In 64bit mode we can allow direct addresses of symbols and labels
     when they are not dynamic symbols.  */
  if (TARGET_64BIT)
    {
      rtx op0 = disp, op1;

      switch (GET_CODE (disp))
	{
	case LABEL_REF:
	  return true;

	case CONST:
	  if (GET_CODE (XEXP (disp, 0)) != PLUS)
	    break;
	  op0 = XEXP (XEXP (disp, 0), 0);
	  op1 = XEXP (XEXP (disp, 0), 1);
	  if (!CONST_INT_P (op1))
	    break;
	  if (GET_CODE (op0) == UNSPEC
	      && (XINT (op0, 1) == UNSPEC_DTPOFF
		  || XINT (op0, 1) == UNSPEC_NTPOFF)
	      && trunc_int_for_mode (INTVAL (op1), SImode) == INTVAL (op1))
	    return true;
	  if (INTVAL (op1) >= 16*1024*1024
	      || INTVAL (op1) < -16*1024*1024)
	    break;
	  if (GET_CODE (op0) == LABEL_REF)
	    return true;
	  if (GET_CODE (op0) == CONST
	      && GET_CODE (XEXP (op0, 0)) == UNSPEC
	      && XINT (XEXP (op0, 0), 1) == UNSPEC_PCREL)
	    return true;
	  if (GET_CODE (op0) == UNSPEC
	      && XINT (op0, 1) == UNSPEC_PCREL)
	    return true;
	  if (GET_CODE (op0) != SYMBOL_REF)
	    break;
	  /* FALLTHRU */

	case SYMBOL_REF:
	  /* TLS references should always be enclosed in UNSPEC.
	     The dllimported symbol needs always to be resolved.  */
	  if (SYMBOL_REF_TLS_MODEL (op0)
	      || (TARGET_DLLIMPORT_DECL_ATTRIBUTES && SYMBOL_REF_DLLIMPORT_P (op0)))
	    return false;

	  if (TARGET_PECOFF)
	    {
	      if (is_imported_p (op0))
		return true;

	      if (SYMBOL_REF_FAR_ADDR_P (op0) || !SYMBOL_REF_LOCAL_P (op0))
		break;

	      /* Non-external-weak function symbols need to be resolved only
		 for the large model.  Non-external symbols don't need to be
		 resolved for large and medium models.  For the small model,
		 we don't need to resolve anything here.  */
	      if ((ix86_cmodel != CM_LARGE_PIC
		   && SYMBOL_REF_FUNCTION_P (op0)
		   && !(SYMBOL_REF_EXTERNAL_P (op0) && SYMBOL_REF_WEAK (op0)))
		  || !SYMBOL_REF_EXTERNAL_P (op0)
		  || ix86_cmodel == CM_SMALL_PIC)
		return true;
	    }
	  else if (!SYMBOL_REF_FAR_ADDR_P (op0)
		   && (SYMBOL_REF_LOCAL_P (op0)
		       || ((ix86_direct_extern_access
			    && !(SYMBOL_REF_DECL (op0)
				 && lookup_attribute ("nodirect_extern_access",
						      DECL_ATTRIBUTES (SYMBOL_REF_DECL (op0)))))
			   && HAVE_LD_PIE_COPYRELOC
			   && flag_pie
			   && !SYMBOL_REF_WEAK (op0)
			   && !SYMBOL_REF_FUNCTION_P (op0)))
		   && ix86_cmodel != CM_LARGE_PIC)
	    return true;
	  break;

	default:
	  break;
	}
    }
  if (GET_CODE (disp) != CONST)
    return false;
  disp = XEXP (disp, 0);

  if (TARGET_64BIT)
    {
      /* We are unsafe to allow PLUS expressions.  This limit allowed distance
         of GOT tables.  We should not need these anyway.  */
      if (GET_CODE (disp) != UNSPEC
	  || (XINT (disp, 1) != UNSPEC_GOTPCREL
	      && XINT (disp, 1) != UNSPEC_GOTOFF
	      && XINT (disp, 1) != UNSPEC_PCREL
	      && XINT (disp, 1) != UNSPEC_PLTOFF))
	return false;

      if (GET_CODE (XVECEXP (disp, 0, 0)) != SYMBOL_REF
	  && GET_CODE (XVECEXP (disp, 0, 0)) != LABEL_REF)
	return false;
      return true;
    }

  saw_plus = false;
  if (GET_CODE (disp) == PLUS)
    {
      if (!CONST_INT_P (XEXP (disp, 1)))
	return false;
      disp = XEXP (disp, 0);
      saw_plus = true;
    }

  if (TARGET_MACHO && darwin_local_data_pic (disp))
    return true;

  if (GET_CODE (disp) != UNSPEC)
    return false;

  switch (XINT (disp, 1))
    {
    case UNSPEC_GOT:
      if (saw_plus)
	return false;
      /* We need to check for both symbols and labels because VxWorks loads
	 text labels with @GOT rather than @GOTOFF.  See gotoff_operand for
	 details.  */
      return (GET_CODE (XVECEXP (disp, 0, 0)) == SYMBOL_REF
	      || GET_CODE (XVECEXP (disp, 0, 0)) == LABEL_REF);
    case UNSPEC_GOTOFF:
      /* Refuse GOTOFF in 64bit mode since it is always 64bit when used.
	 While ABI specify also 32bit relocation but we don't produce it in
	 small PIC model at all.  */
      if ((GET_CODE (XVECEXP (disp, 0, 0)) == SYMBOL_REF
	   || GET_CODE (XVECEXP (disp, 0, 0)) == LABEL_REF)
	  && !TARGET_64BIT)
        return !TARGET_PECOFF && gotoff_operand (XVECEXP (disp, 0, 0), Pmode);
      return false;
    case UNSPEC_GOTTPOFF:
    case UNSPEC_GOTNTPOFF:
    case UNSPEC_INDNTPOFF:
      if (saw_plus)
	return false;
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_INITIAL_EXEC);
    case UNSPEC_NTPOFF:
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_LOCAL_EXEC);
    case UNSPEC_DTPOFF:
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_LOCAL_DYNAMIC);
    }

  return false;
}

/* Determine if op is suitable RTX for an address register.
   Return naked register if a register or a register subreg is
   found, otherwise return NULL_RTX.  */

static rtx
ix86_validate_address_register (rtx op)
{
  machine_mode mode = GET_MODE (op);

  /* Only SImode or DImode registers can form the address.  */
  if (mode != SImode && mode != DImode)
    return NULL_RTX;

  if (REG_P (op))
    return op;
  else if (SUBREG_P (op))
    {
      rtx reg = SUBREG_REG (op);

      if (!REG_P (reg))
	return NULL_RTX;

      mode = GET_MODE (reg);

      /* Don't allow SUBREGs that span more than a word.  It can
	 lead to spill failures when the register is one word out
	 of a two word structure.  */
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return NULL_RTX;

      /* Allow only SUBREGs of non-eliminable hard registers.  */
      if (register_no_elim_operand (reg, mode))
	return reg;
    }

  /* Op is not a register.  */
  return NULL_RTX;
}

/* Determine which memory address register set insn can use.  */

static enum attr_addr
ix86_memory_address_reg_class (rtx_insn* insn)
{
  /* LRA can do some initialization with NULL insn,
     return maximum register class in this case.  */
  enum attr_addr addr_rclass = ADDR_GPR32;

  if (!insn)
    return addr_rclass;

  if (asm_noperands (PATTERN (insn)) >= 0
      || GET_CODE (PATTERN (insn)) == ASM_INPUT)
    return ix86_apx_inline_asm_use_gpr32 ? ADDR_GPR32 : ADDR_GPR16;

  /* Return maximum register class for unrecognized instructions.  */
  if (INSN_CODE (insn) < 0)
    return addr_rclass;

  /* Try to recognize the insn before calling get_attr_addr.
     Save current recog_data and current alternative.  */
  struct recog_data_d saved_recog_data = recog_data;
  int saved_alternative = which_alternative;

  /* Update recog_data for processing of alternatives.  */
  extract_insn_cached (insn);

  /* If current alternative is not set, loop throught enabled
     alternatives and get the most limited register class.  */
  if (saved_alternative == -1)
    {
      alternative_mask enabled = get_enabled_alternatives (insn);

      for (int i = 0; i < recog_data.n_alternatives; i++)
	{
	  if (!TEST_BIT (enabled, i))
	    continue;

	  which_alternative = i;
	  addr_rclass = MIN (addr_rclass, get_attr_addr (insn));
	}
    }
  else
    {
      which_alternative = saved_alternative;
      addr_rclass = get_attr_addr (insn);
    }

  recog_data = saved_recog_data;
  which_alternative = saved_alternative;

  return addr_rclass;
}

/* Return memory address register class insn can use.  */

enum reg_class
ix86_insn_base_reg_class (rtx_insn* insn)
{
  switch (ix86_memory_address_reg_class (insn))
    {
    case ADDR_GPR8:
      return LEGACY_GENERAL_REGS;
    case ADDR_GPR16:
      return GENERAL_GPR16;
    case ADDR_GPR32:
      break;
    default:
      gcc_unreachable ();
    }

  return BASE_REG_CLASS;
}

bool
ix86_regno_ok_for_insn_base_p (int regno, rtx_insn* insn)
{
  switch (ix86_memory_address_reg_class (insn))
    {
    case ADDR_GPR8:
      return LEGACY_INT_REGNO_P (regno);
    case ADDR_GPR16:
      return GENERAL_GPR16_REGNO_P (regno);
    case ADDR_GPR32:
      break;
    default:
      gcc_unreachable ();
    }

  return GENERAL_REGNO_P (regno);
}

enum reg_class
ix86_insn_index_reg_class (rtx_insn* insn)
{
  switch (ix86_memory_address_reg_class (insn))
    {
    case ADDR_GPR8:
      return LEGACY_INDEX_REGS;
    case ADDR_GPR16:
      return INDEX_GPR16;
    case ADDR_GPR32:
      break;
    default:
      gcc_unreachable ();
    }

  return INDEX_REG_CLASS;
}

/* Recognizes RTL expressions that are valid memory addresses for an
   instruction.  The MODE argument is the machine mode for the MEM
   expression that wants to use this address.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

static bool
ix86_legitimate_address_p (machine_mode, rtx addr, bool strict,
			   code_helper = ERROR_MARK)
{
  struct ix86_address parts;
  rtx base, index, disp;
  HOST_WIDE_INT scale;
  addr_space_t seg;

  if (ix86_decompose_address (addr, &parts) == 0)
    /* Decomposition failed.  */
    return false;

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;
  seg = parts.seg;

  /* Validate base register.  */
  if (base)
    {
      rtx reg = ix86_validate_address_register (base);

      if (reg == NULL_RTX)
	return false;

      unsigned int regno = REGNO (reg);
      if ((strict && !REGNO_OK_FOR_BASE_P (regno))
	  || (!strict && !REGNO_OK_FOR_BASE_NONSTRICT_P (regno)))
	/* Base is not valid.  */
	return false;
    }

  /* Validate index register.  */
  if (index)
    {
      rtx reg = ix86_validate_address_register (index);

      if (reg == NULL_RTX)
	return false;

      unsigned int regno = REGNO (reg);
      if ((strict && !REGNO_OK_FOR_INDEX_P (regno))
	  || (!strict && !REGNO_OK_FOR_INDEX_NONSTRICT_P (regno)))
	/* Index is not valid.  */
	return false;
    }

  /* Index and base should have the same mode.  */
  if (base && index
      && GET_MODE (base) != GET_MODE (index))
    return false;

  /* Address override works only on the (%reg) part of %fs:(%reg).  */
  if (seg != ADDR_SPACE_GENERIC
      && ((base && GET_MODE (base) != word_mode)
	  || (index && GET_MODE (index) != word_mode)))
    return false;

  /* Validate scale factor.  */
  if (scale != 1)
    {
      if (!index)
	/* Scale without index.  */
	return false;

      if (scale != 2 && scale != 4 && scale != 8)
	/* Scale is not a valid multiplier.  */
	return false;
    }

  /* Validate displacement.  */
  if (disp)
    {
      if (ix86_endbr_immediate_operand (disp, VOIDmode))
	return false;

      if (GET_CODE (disp) == CONST
	  && GET_CODE (XEXP (disp, 0)) == UNSPEC
	  && XINT (XEXP (disp, 0), 1) != UNSPEC_MACHOPIC_OFFSET)
	switch (XINT (XEXP (disp, 0), 1))
	  {
	  /* Refuse GOTOFF and GOT in 64bit mode since it is always 64bit
	     when used.  While ABI specify also 32bit relocations, we
	     don't produce them at all and use IP relative instead.
	     Allow GOT in 32bit mode for both PIC and non-PIC if symbol
	     should be loaded via GOT.  */
	  case UNSPEC_GOT:
	    if (!TARGET_64BIT
		&& ix86_force_load_from_GOT_p (XVECEXP (XEXP (disp, 0), 0, 0)))
	      goto is_legitimate_pic;
	    /* FALLTHRU */
	  case UNSPEC_GOTOFF:
	    gcc_assert (flag_pic);
	    if (!TARGET_64BIT)
	      goto is_legitimate_pic;

	    /* 64bit address unspec.  */
	    return false;

	  case UNSPEC_GOTPCREL:
	    if (ix86_force_load_from_GOT_p (XVECEXP (XEXP (disp, 0), 0, 0)))
	      goto is_legitimate_pic;
	    /* FALLTHRU */
	  case UNSPEC_PCREL:
	    gcc_assert (flag_pic);
	    goto is_legitimate_pic;

	  case UNSPEC_GOTTPOFF:
	  case UNSPEC_GOTNTPOFF:
	  case UNSPEC_INDNTPOFF:
	  case UNSPEC_NTPOFF:
	  case UNSPEC_DTPOFF:
	    break;

	  default:
	    /* Invalid address unspec.  */
	    return false;
	  }

      else if (SYMBOLIC_CONST (disp)
	       && (flag_pic
#if TARGET_MACHO
		   || (MACHOPIC_INDIRECT
		       && !machopic_operand_p (disp))
#endif
		  ))
	{

	is_legitimate_pic:
	  if (TARGET_64BIT && (index || base))
	    {
	      /* foo@dtpoff(%rX) is ok.  */
	      if (GET_CODE (disp) != CONST
		  || GET_CODE (XEXP (disp, 0)) != PLUS
		  || GET_CODE (XEXP (XEXP (disp, 0), 0)) != UNSPEC
		  || !CONST_INT_P (XEXP (XEXP (disp, 0), 1))
		  || (XINT (XEXP (XEXP (disp, 0), 0), 1) != UNSPEC_DTPOFF
		      && XINT (XEXP (XEXP (disp, 0), 0), 1) != UNSPEC_NTPOFF))
		/* Non-constant pic memory reference.  */
		return false;
	    }
	  else if ((!TARGET_MACHO || flag_pic)
		    && ! legitimate_pic_address_disp_p (disp))
	    /* Displacement is an invalid pic construct.  */
	    return false;
#if TARGET_MACHO
	  else if (MACHO_DYNAMIC_NO_PIC_P
		   && !ix86_legitimate_constant_p (Pmode, disp))
	    /* displacment must be referenced via non_lazy_pointer */
	    return false;
#endif

          /* This code used to verify that a symbolic pic displacement
	     includes the pic_offset_table_rtx register.

	     While this is good idea, unfortunately these constructs may
	     be created by "adds using lea" optimization for incorrect
	     code like:

	     int a;
	     int foo(int i)
	       {
	         return *(&a+i);
	       }

	     This code is nonsensical, but results in addressing
	     GOT table with pic_offset_table_rtx base.  We can't
	     just refuse it easily, since it gets matched by
	     "addsi3" pattern, that later gets split to lea in the
	     case output register differs from input.  While this
	     can be handled by separate addsi pattern for this case
	     that never results in lea, this seems to be easier and
	     correct fix for crash to disable this test.  */
	}
      else if (GET_CODE (disp) != LABEL_REF
	       && !CONST_INT_P (disp)
	       && (GET_CODE (disp) != CONST
		   || !ix86_legitimate_constant_p (Pmode, disp))
	       && (GET_CODE (disp) != SYMBOL_REF
		   || !ix86_legitimate_constant_p (Pmode, disp)))
	/* Displacement is not constant.  */
	return false;
      else if (TARGET_64BIT
	       && !x86_64_immediate_operand (disp, VOIDmode))
	/* Displacement is out of range.  */
	return false;
      /* In x32 mode, constant addresses are sign extended to 64bit, so
	 we have to prevent addresses from 0x80000000 to 0xffffffff.  */
      else if (TARGET_X32 && !(index || base)
	       && CONST_INT_P (disp)
	       && val_signbit_known_set_p (SImode, INTVAL (disp)))
	return false;
    }

  /* Everything looks valid.  */
  return true;
}

/* Determine if a given RTX is a valid constant address.  */

bool
constant_address_p (rtx x)
{
  return CONSTANT_P (x) && ix86_legitimate_address_p (Pmode, x, 1);
}

/* Return a unique alias set for the GOT.  */

alias_set_type
ix86_GOT_alias_set (void)
{
  static alias_set_type set = -1;
  if (set == -1)
    set = new_alias_set ();
  return set;
}

/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are two types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references, constant pool addresses, and code labels
      compute the address as an offset from the GOT, whose base is in
      the PIC reg.  Static data objects have SYMBOL_FLAG_LOCAL set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   TARGET_LEGITIMATE_ADDRESS_P rejects symbolic references unless the PIC
   reg also appears in the address.  */

rtx
legitimize_pic_address (rtx orig, rtx reg)
{
  rtx addr = orig;
  rtx new_rtx = orig;

#if TARGET_MACHO
  if (TARGET_MACHO && !TARGET_64BIT)
    {
      if (reg == 0)
	reg = gen_reg_rtx (Pmode);
      /* Use the generic Mach-O PIC machinery.  */
      return machopic_legitimize_pic_address (orig, GET_MODE (orig), reg);
    }
#endif

  if (TARGET_64BIT && TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    {
      rtx tmp = legitimize_pe_coff_symbol (addr, true);
      if (tmp)
        return tmp;
    }

  if (TARGET_64BIT && legitimate_pic_address_disp_p (addr))
    new_rtx = addr;
  else if ((!TARGET_64BIT
	    || /* TARGET_64BIT && */ ix86_cmodel != CM_SMALL_PIC)
	   && !TARGET_PECOFF
	   && gotoff_operand (addr, Pmode))
    {
      /* This symbol may be referenced via a displacement
	 from the PIC base address (@GOTOFF).  */
      if (GET_CODE (addr) == CONST)
	addr = XEXP (addr, 0);

      if (GET_CODE (addr) == PLUS)
	  {
            new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, XEXP (addr, 0)),
				      UNSPEC_GOTOFF);
	    new_rtx = gen_rtx_PLUS (Pmode, new_rtx, XEXP (addr, 1));
	  }
	else
          new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTOFF);

      new_rtx = gen_rtx_CONST (Pmode, new_rtx);

      if (TARGET_64BIT)
	new_rtx = copy_to_suggested_reg (new_rtx, reg, Pmode);

      if (reg != 0)
	{
 	  gcc_assert (REG_P (reg));
	  new_rtx = expand_simple_binop (Pmode, PLUS, pic_offset_table_rtx,
					 new_rtx, reg, 1, OPTAB_DIRECT);
 	}
      else
	new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);
    }
  else if ((GET_CODE (addr) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (addr) == 0)
	   /* We can't always use @GOTOFF for text labels
	      on VxWorks, see gotoff_operand.  */
	   || (TARGET_VXWORKS_RTP && GET_CODE (addr) == LABEL_REF))
    {
      rtx tmp = legitimize_pe_coff_symbol (addr, true);
      if (tmp)
        return tmp;

      /* For x64 PE-COFF there is no GOT table,
	 so we use address directly.  */
      if (TARGET_64BIT && TARGET_PECOFF)
	{
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_PCREL);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	}
      else if (TARGET_64BIT && ix86_cmodel != CM_LARGE_PIC)
	{
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr),
				    UNSPEC_GOTPCREL);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  new_rtx = gen_const_mem (Pmode, new_rtx);
	  set_mem_alias_set (new_rtx, ix86_GOT_alias_set ());
	}
      else
	{
	  /* This symbol must be referenced via a load
	     from the Global Offset Table (@GOT).  */
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOT);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);

	  if (TARGET_64BIT)
	    new_rtx = copy_to_suggested_reg (new_rtx, reg, Pmode);

	  if (reg != 0)
	    {
	      gcc_assert (REG_P (reg));
	      new_rtx = expand_simple_binop (Pmode, PLUS, pic_offset_table_rtx,
					     new_rtx, reg, 1, OPTAB_DIRECT);
	    }
	  else
	    new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);

	  new_rtx = gen_const_mem (Pmode, new_rtx);
	  set_mem_alias_set (new_rtx, ix86_GOT_alias_set ());
	}

      new_rtx = copy_to_suggested_reg (new_rtx, reg, Pmode);
    }
  else
    {
      if (CONST_INT_P (addr)
	  && !x86_64_immediate_operand (addr, VOIDmode))
	new_rtx = copy_to_suggested_reg (addr, reg, Pmode);
      else if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);

	  /* We must match stuff we generate before.  Assume the only
	     unspecs that can get here are ours.  Not that we could do
	     anything with them anyway....  */
	  if (GET_CODE (addr) == UNSPEC
	      || (GET_CODE (addr) == PLUS
		  && GET_CODE (XEXP (addr, 0)) == UNSPEC))
	    return orig;
	  gcc_assert (GET_CODE (addr) == PLUS);
	}

      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant
	     offset from a @GOTOFF symbol reference.  */
	  if (!TARGET_PECOFF
	      && gotoff_operand (op0, Pmode)
	      && CONST_INT_P (op1))
	    {
	      if (!TARGET_64BIT)
		{
		  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0),
					    UNSPEC_GOTOFF);
		  new_rtx = gen_rtx_PLUS (Pmode, new_rtx, op1);
		  new_rtx = gen_rtx_CONST (Pmode, new_rtx);

		  if (reg != 0)
		    {
		      gcc_assert (REG_P (reg));
		      new_rtx = expand_simple_binop (Pmode, PLUS,
						     pic_offset_table_rtx,
						     new_rtx, reg, 1,
						     OPTAB_DIRECT);
		    }
		  else
		    new_rtx
		      = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);
		}
	      else
		{
		  if (INTVAL (op1) < -16*1024*1024
		      || INTVAL (op1) >= 16*1024*1024)
		    {
		      if (!x86_64_immediate_operand (op1, Pmode))
			op1 = force_reg (Pmode, op1);

		      new_rtx
			= gen_rtx_PLUS (Pmode, force_reg (Pmode, op0), op1);
		    }
		}
	    }
	  else
	    {
	      rtx base = legitimize_pic_address (op0, reg);
	      machine_mode mode = GET_MODE (base);
	      new_rtx
	        = legitimize_pic_address (op1, base == reg ? NULL_RTX : reg);

	      if (CONST_INT_P (new_rtx))
		{
		  if (INTVAL (new_rtx) < -16*1024*1024
		      || INTVAL (new_rtx) >= 16*1024*1024)
		    {
		      if (!x86_64_immediate_operand (new_rtx, mode))
			new_rtx = force_reg (mode, new_rtx);

		      new_rtx
		        = gen_rtx_PLUS (mode, force_reg (mode, base), new_rtx);
		    }
		  else
		    new_rtx = plus_constant (mode, base, INTVAL (new_rtx));
		}
	      else
		{
		  /* For %rip addressing, we have to use
		     just disp32, not base nor index.  */
		  if (TARGET_64BIT
		      && (GET_CODE (base) == SYMBOL_REF
			  || GET_CODE (base) == LABEL_REF))
		    base = force_reg (mode, base);
		  if (GET_CODE (new_rtx) == PLUS
		      && CONSTANT_P (XEXP (new_rtx, 1)))
		    {
		      base = gen_rtx_PLUS (mode, base, XEXP (new_rtx, 0));
		      new_rtx = XEXP (new_rtx, 1);
		    }
		  new_rtx = gen_rtx_PLUS (mode, base, new_rtx);
		}
	    }
	}
    }
  return new_rtx;
}

/* Load the thread pointer.  If TO_REG is true, force it into a register.  */

static rtx
get_thread_pointer (machine_mode tp_mode, bool to_reg)
{
  rtx tp = gen_rtx_UNSPEC (ptr_mode, gen_rtvec (1, const0_rtx), UNSPEC_TP);

  if (GET_MODE (tp) != tp_mode)
    {
      gcc_assert (GET_MODE (tp) == SImode);
      gcc_assert (tp_mode == DImode);

      tp = gen_rtx_ZERO_EXTEND (tp_mode, tp);
    }

  if (to_reg)
    tp = copy_to_mode_reg (tp_mode, tp);

  return tp;
}

/* Construct the SYMBOL_REF for the tls_get_addr function.  */

static GTY(()) rtx ix86_tls_symbol;

static rtx
ix86_tls_get_addr (void)
{
  if (!ix86_tls_symbol)
    {
      const char *sym
	= ((TARGET_ANY_GNU_TLS && !TARGET_64BIT)
	   ? "___tls_get_addr" : "__tls_get_addr");

      ix86_tls_symbol = gen_rtx_SYMBOL_REF (Pmode, sym);
    }

  if (ix86_cmodel == CM_LARGE_PIC && !TARGET_PECOFF)
    {
      rtx unspec = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, ix86_tls_symbol),
				   UNSPEC_PLTOFF);
      return gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
			   gen_rtx_CONST (Pmode, unspec));
    }

  return ix86_tls_symbol;
}

/* Construct the SYMBOL_REF for the _TLS_MODULE_BASE_ symbol.  */

static GTY(()) rtx ix86_tls_module_base_symbol;

rtx
ix86_tls_module_base (void)
{
  if (!ix86_tls_module_base_symbol)
    {
      ix86_tls_module_base_symbol
	= gen_rtx_SYMBOL_REF (ptr_mode, "_TLS_MODULE_BASE_");

      SYMBOL_REF_FLAGS (ix86_tls_module_base_symbol)
	|= TLS_MODEL_GLOBAL_DYNAMIC << SYMBOL_FLAG_TLS_SHIFT;
    }

  return ix86_tls_module_base_symbol;
}

/* A subroutine of ix86_legitimize_address and ix86_expand_move.  FOR_MOV is
   false if we expect this to be used for a memory address and true if
   we expect to load the address into a register.  */

rtx
legitimize_tls_address (rtx x, enum tls_model model, bool for_mov)
{
  rtx dest, base, off;
  rtx pic = NULL_RTX, tp = NULL_RTX;
  machine_mode tp_mode = Pmode;
  int type;

  /* Fall back to global dynamic model if tool chain cannot support local
     dynamic.  */
  if (TARGET_SUN_TLS && !TARGET_64BIT
      && !HAVE_AS_IX86_TLSLDMPLT && !HAVE_AS_IX86_TLSLDM
      && model == TLS_MODEL_LOCAL_DYNAMIC)
    model = TLS_MODEL_GLOBAL_DYNAMIC;

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      if (!TARGET_64BIT)
	{
	  if (flag_pic && !TARGET_PECOFF)
	    pic = pic_offset_table_rtx;
	  else
	    {
	      pic = gen_reg_rtx (Pmode);
	      emit_insn (gen_set_got (pic));
	    }
	}

      if (TARGET_GNU2_TLS)
	{
	  dest = gen_reg_rtx (ptr_mode);
	  if (TARGET_64BIT)
	    emit_insn (gen_tls_dynamic_gnu2_64 (ptr_mode, dest, x));
	  else
	    emit_insn (gen_tls_dynamic_gnu2_32 (dest, x, pic));

	  tp = get_thread_pointer (ptr_mode, true);
	  dest = gen_rtx_PLUS (ptr_mode, tp, dest);
	  if (GET_MODE (dest) != Pmode)
	     dest = gen_rtx_ZERO_EXTEND (Pmode, dest);
	  dest = force_reg (Pmode, dest);

	  if (GET_MODE (x) != Pmode)
	    x = gen_rtx_ZERO_EXTEND (Pmode, x);

	  set_unique_reg_note (get_last_insn (), REG_EQUAL, x);
	}
      else
	{
	  rtx caddr = ix86_tls_get_addr ();

	  dest = gen_reg_rtx (Pmode);
	  if (TARGET_64BIT)
	    {
	      rtx rax = gen_rtx_REG (Pmode, AX_REG);
	      rtx_insn *insns;

	      start_sequence ();
	      emit_call_insn
		(gen_tls_global_dynamic_64 (Pmode, rax, x, caddr));
	      insns = get_insns ();
	      end_sequence ();

	      if (GET_MODE (x) != Pmode)
		x = gen_rtx_ZERO_EXTEND (Pmode, x);

	      RTL_CONST_CALL_P (insns) = 1;
	      emit_libcall_block (insns, dest, rax, x);
	    }
	  else
	    emit_insn (gen_tls_global_dynamic_32 (dest, x, pic, caddr));
	}
      break;

    case TLS_MODEL_LOCAL_DYNAMIC:
      if (!TARGET_64BIT)
	{
	  if (flag_pic)
	    pic = pic_offset_table_rtx;
	  else
	    {
	      pic = gen_reg_rtx (Pmode);
	      emit_insn (gen_set_got (pic));
	    }
	}

      if (TARGET_GNU2_TLS)
	{
	  rtx tmp = ix86_tls_module_base ();

	  base = gen_reg_rtx (ptr_mode);
	  if (TARGET_64BIT)
	    emit_insn (gen_tls_dynamic_gnu2_64 (ptr_mode, base, tmp));
	  else
	    emit_insn (gen_tls_dynamic_gnu2_32 (base, tmp, pic));

	  tp = get_thread_pointer (ptr_mode, true);
	  if (GET_MODE (base) != Pmode)
	    base = gen_rtx_ZERO_EXTEND (Pmode, base);
	  base = force_reg (Pmode, base);
	}
      else
	{
	  rtx caddr = ix86_tls_get_addr ();

	  base = gen_reg_rtx (Pmode);
	  if (TARGET_64BIT)
	    {
	      rtx rax = gen_rtx_REG (Pmode, AX_REG);
	      rtx_insn *insns;
	      rtx eqv;

	      start_sequence ();
	      emit_call_insn
		(gen_tls_local_dynamic_base_64 (Pmode, rax, caddr));
	      insns = get_insns ();
	      end_sequence ();

	      /* Attach a unique REG_EQUAL, to allow the RTL optimizers to
		 share the LD_BASE result with other LD model accesses.  */
	      eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
				    UNSPEC_TLS_LD_BASE);

	      RTL_CONST_CALL_P (insns) = 1;
	      emit_libcall_block (insns, base, rax, eqv);
	    }
	  else
	    emit_insn (gen_tls_local_dynamic_base_32 (base, pic, caddr));
	}

      off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_DTPOFF);
      off = gen_rtx_CONST (Pmode, off);

      dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, base, off));

      if (TARGET_GNU2_TLS)
	{
	  if (GET_MODE (tp) != Pmode)
	    {
	      dest = lowpart_subreg (ptr_mode, dest, Pmode);
	      dest = gen_rtx_PLUS (ptr_mode, tp, dest);
	      dest = gen_rtx_ZERO_EXTEND (Pmode, dest);
	    }
	  else
	    dest = gen_rtx_PLUS (Pmode, tp, dest);
	  dest = force_reg (Pmode, dest);

	  if (GET_MODE (x) != Pmode)
	    x = gen_rtx_ZERO_EXTEND (Pmode, x);

	  set_unique_reg_note (get_last_insn (), REG_EQUAL, x);
	}
      break;

    case TLS_MODEL_INITIAL_EXEC:
      if (TARGET_64BIT)
	{
	  if (TARGET_SUN_TLS && !TARGET_X32)
	    {
	      /* The Sun linker took the AMD64 TLS spec literally
		 and can only handle %rax as destination of the
		 initial executable code sequence.  */

	      dest = gen_reg_rtx (DImode);
	      emit_insn (gen_tls_initial_exec_64_sun (dest, x));
	      return dest;
	    }

	  /* Generate DImode references to avoid %fs:(%reg32)
	     problems and linker IE->LE relaxation bug.  */
	  tp_mode = DImode;
	  pic = NULL;
	  type = UNSPEC_GOTNTPOFF;
	}
      else if (flag_pic)
	{
	  pic = pic_offset_table_rtx;
	  type = TARGET_ANY_GNU_TLS ? UNSPEC_GOTNTPOFF : UNSPEC_GOTTPOFF;
	}
      else if (!TARGET_ANY_GNU_TLS)
	{
	  pic = gen_reg_rtx (Pmode);
	  emit_insn (gen_set_got (pic));
	  type = UNSPEC_GOTTPOFF;
	}
      else
	{
	  pic = NULL;
	  type = UNSPEC_INDNTPOFF;
	}

      off = gen_rtx_UNSPEC (tp_mode, gen_rtvec (1, x), type);
      off = gen_rtx_CONST (tp_mode, off);
      if (pic)
	off = gen_rtx_PLUS (tp_mode, pic, off);
      off = gen_const_mem (tp_mode, off);
      set_mem_alias_set (off, ix86_GOT_alias_set ());

      if (TARGET_64BIT || TARGET_ANY_GNU_TLS)
	{
	  base = get_thread_pointer (tp_mode,
				     for_mov || !TARGET_TLS_DIRECT_SEG_REFS);
	  off = force_reg (tp_mode, off);
	  dest = gen_rtx_PLUS (tp_mode, base, off);
	  if (tp_mode != Pmode)
	    dest = convert_to_mode (Pmode, dest, 1);
	}
      else
	{
	  base = get_thread_pointer (Pmode, true);
	  dest = gen_reg_rtx (Pmode);
	  emit_insn (gen_sub3_insn (dest, base, off));
	}
      break;

    case TLS_MODEL_LOCAL_EXEC:
      off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x),
			    (TARGET_64BIT || TARGET_ANY_GNU_TLS)
			    ? UNSPEC_NTPOFF : UNSPEC_TPOFF);
      off = gen_rtx_CONST (Pmode, off);

      if (TARGET_64BIT || TARGET_ANY_GNU_TLS)
	{
	  base = get_thread_pointer (Pmode,
				     for_mov || !TARGET_TLS_DIRECT_SEG_REFS);
	  return gen_rtx_PLUS (Pmode, base, off);
	}
      else
	{
	  base = get_thread_pointer (Pmode, true);
	  dest = gen_reg_rtx (Pmode);
	  emit_insn (gen_sub3_insn (dest, base, off));
	}
      break;

    default:
      gcc_unreachable ();
    }

  return dest;
}

/* Return true if the TLS address requires insn using integer registers.
   It's used to prevent KMOV/VMOV in TLS code sequences which require integer
   MOV instructions, refer to PR103275.  */
bool
ix86_gpr_tls_address_pattern_p (rtx mem)
{
  gcc_assert (MEM_P (mem));

  rtx addr = XEXP (mem, 0);
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, addr, ALL)
    {
      rtx op = *iter;
      if (GET_CODE (op) == UNSPEC)
	switch (XINT (op, 1))
	  {
	  case UNSPEC_GOTNTPOFF:
	    return true;
	  case UNSPEC_TPOFF:
	    if (!TARGET_64BIT)
	      return true;
	    break;
	  default:
	    break;
	  }
    }

  return false;
}

/* Return true if OP refers to a TLS address.  */
bool
ix86_tls_address_pattern_p (rtx op)
{
  subrtx_var_iterator::array_type array;
  FOR_EACH_SUBRTX_VAR (iter, array, op, ALL)
    {
      rtx op = *iter;
      if (MEM_P (op))
	{
	  rtx *x = &XEXP (op, 0);
	  while (GET_CODE (*x) == PLUS)
	    {
	      int i;
	      for (i = 0; i < 2; i++)
		{
		  rtx u = XEXP (*x, i);
		  if (GET_CODE (u) == ZERO_EXTEND)
		    u = XEXP (u, 0);
		  if (GET_CODE (u) == UNSPEC
		      && XINT (u, 1) == UNSPEC_TP)
		    return true;
		}
	      x = &XEXP (*x, 0);
	    }

	  iter.skip_subrtxes ();
	}
    }

  return false;
}

/* Rewrite *LOC so that it refers to a default TLS address space.  */
void
ix86_rewrite_tls_address_1 (rtx *loc)
{
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, loc, ALL)
    {
      rtx *loc = *iter;
      if (MEM_P (*loc))
	{
	  rtx addr = XEXP (*loc, 0);
	  rtx *x = &addr;
	  while (GET_CODE (*x) == PLUS)
	    {
	      int i;
	      for (i = 0; i < 2; i++)
		{
		  rtx u = XEXP (*x, i);
		  if (GET_CODE (u) == ZERO_EXTEND)
		    u = XEXP (u, 0);
		  if (GET_CODE (u) == UNSPEC
		      && XINT (u, 1) == UNSPEC_TP)
		    {
		      addr_space_t as = DEFAULT_TLS_SEG_REG;

		      *x = XEXP (*x, 1 - i);

		      *loc = replace_equiv_address_nv (*loc, addr, true);
		      set_mem_addr_space (*loc, as);
		      return;
		    }
		}
	      x = &XEXP (*x, 0);
	    }

	  iter.skip_subrtxes ();
	}
    }
}

/* Rewrite instruction pattern involvning TLS address
   so that it refers to a default TLS address space.  */
rtx
ix86_rewrite_tls_address (rtx pattern)
{
  pattern = copy_insn (pattern);
  ix86_rewrite_tls_address_1 (&pattern);
  return pattern;
}

/* Create or return the unique __imp_DECL dllimport symbol corresponding
   to symbol DECL if BEIMPORT is true.  Otherwise create or return the
   unique refptr-DECL symbol corresponding to symbol DECL.  */

struct dllimport_hasher : ggc_cache_ptr_hash<tree_map>
{
  static inline hashval_t hash (tree_map *m) { return m->hash; }
  static inline bool
  equal (tree_map *a, tree_map *b)
  {
    return a->base.from == b->base.from;
  }

  static int
  keep_cache_entry (tree_map *&m)
  {
    return ggc_marked_p (m->base.from);
  }
};

static GTY((cache)) hash_table<dllimport_hasher> *dllimport_map;

static tree
get_dllimport_decl (tree decl, bool beimport)
{
  struct tree_map *h, in;
  const char *name;
  const char *prefix;
  size_t namelen, prefixlen;
  char *imp_name;
  tree to;
  rtx rtl;

  if (!dllimport_map)
    dllimport_map = hash_table<dllimport_hasher>::create_ggc (512);

  in.hash = htab_hash_pointer (decl);
  in.base.from = decl;
  tree_map **loc = dllimport_map->find_slot_with_hash (&in, in.hash, INSERT);
  h = *loc;
  if (h)
    return h->to;

  *loc = h = ggc_alloc<tree_map> ();
  h->hash = in.hash;
  h->base.from = decl;
  h->to = to = build_decl (DECL_SOURCE_LOCATION (decl),
			   VAR_DECL, NULL, ptr_type_node);
  DECL_ARTIFICIAL (to) = 1;
  DECL_IGNORED_P (to) = 1;
  DECL_EXTERNAL (to) = 1;
  TREE_READONLY (to) = 1;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = targetm.strip_name_encoding (name);
  if (beimport)
    prefix = name[0] == FASTCALL_PREFIX || user_label_prefix[0] == 0
      ? "*__imp_" : "*__imp__";
  else
    prefix = user_label_prefix[0] == 0 ? "*.refptr." : "*refptr.";
  namelen = strlen (name);
  prefixlen = strlen (prefix);
  imp_name = (char *) alloca (namelen + prefixlen + 1);
  memcpy (imp_name, prefix, prefixlen);
  memcpy (imp_name + prefixlen, name, namelen + 1);

  name = ggc_alloc_string (imp_name, namelen + prefixlen);
  rtl = gen_rtx_SYMBOL_REF (Pmode, name);
  SET_SYMBOL_REF_DECL (rtl, to);
  SYMBOL_REF_FLAGS (rtl) = SYMBOL_FLAG_LOCAL | SYMBOL_FLAG_STUBVAR;
  if (!beimport)
    {
      SYMBOL_REF_FLAGS (rtl) |= SYMBOL_FLAG_EXTERNAL;
#ifdef SUB_TARGET_RECORD_STUB
      SUB_TARGET_RECORD_STUB (name);
#endif
    }      

  rtl = gen_const_mem (Pmode, rtl);
  set_mem_alias_set (rtl, ix86_GOT_alias_set ());

  SET_DECL_RTL (to, rtl);
  SET_DECL_ASSEMBLER_NAME (to, get_identifier (name));

  return to;
}

/* Expand SYMBOL into its corresponding far-address symbol.
   WANT_REG is true if we require the result be a register.  */

static rtx
legitimize_pe_coff_extern_decl (rtx symbol, bool want_reg)
{
  tree imp_decl;
  rtx x;

  gcc_assert (SYMBOL_REF_DECL (symbol));
  imp_decl = get_dllimport_decl (SYMBOL_REF_DECL (symbol), false);

  x = DECL_RTL (imp_decl);
  if (want_reg)
    x = force_reg (Pmode, x);
  return x;
}

/* Expand SYMBOL into its corresponding dllimport symbol.  WANT_REG is
   true if we require the result be a register.  */

static rtx
legitimize_dllimport_symbol (rtx symbol, bool want_reg)
{
  tree imp_decl;
  rtx x;

  gcc_assert (SYMBOL_REF_DECL (symbol));
  imp_decl = get_dllimport_decl (SYMBOL_REF_DECL (symbol), true);

  x = DECL_RTL (imp_decl);
  if (want_reg)
    x = force_reg (Pmode, x);
  return x;
}

/* Expand SYMBOL into its corresponding dllimport or refptr symbol.  WANT_REG 
   is true if we require the result be a register.  */

rtx
legitimize_pe_coff_symbol (rtx addr, bool inreg)
{
  if (!TARGET_PECOFF)
    return NULL_RTX;

  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    {
      if (GET_CODE (addr) == SYMBOL_REF && SYMBOL_REF_DLLIMPORT_P (addr))
	return legitimize_dllimport_symbol (addr, inreg);
      if (GET_CODE (addr) == CONST
	  && GET_CODE (XEXP (addr, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
	  && SYMBOL_REF_DLLIMPORT_P (XEXP (XEXP (addr, 0), 0)))
	{
	  rtx t = legitimize_dllimport_symbol (XEXP (XEXP (addr, 0), 0), inreg);
	  return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (addr, 0), 1));
	}
    }

  if (ix86_cmodel != CM_LARGE_PIC && ix86_cmodel != CM_MEDIUM_PIC)
    return NULL_RTX;
  if (GET_CODE (addr) == SYMBOL_REF
      && !is_imported_p (addr)
      && SYMBOL_REF_EXTERNAL_P (addr)
      && SYMBOL_REF_DECL (addr))
    return legitimize_pe_coff_extern_decl (addr, inreg);

  if (GET_CODE (addr) == CONST
      && GET_CODE (XEXP (addr, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
      && !is_imported_p (XEXP (XEXP (addr, 0), 0))
      && SYMBOL_REF_EXTERNAL_P (XEXP (XEXP (addr, 0), 0))
      && SYMBOL_REF_DECL (XEXP (XEXP (addr, 0), 0)))
    {
      rtx t = legitimize_pe_coff_extern_decl (XEXP (XEXP (addr, 0), 0), inreg);
      return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (addr, 0), 1));
    }
  return NULL_RTX;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.cc.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.cc for details.  */

static rtx
ix86_legitimize_address (rtx x, rtx, machine_mode mode)
{
  bool changed = false;
  unsigned log;

  log = GET_CODE (x) == SYMBOL_REF ? SYMBOL_REF_TLS_MODEL (x) : 0;
  if (log)
    return legitimize_tls_address (x, (enum tls_model) log, false);
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && (log = SYMBOL_REF_TLS_MODEL (XEXP (XEXP (x, 0), 0))))
    {
      rtx t = legitimize_tls_address (XEXP (XEXP (x, 0), 0),
				      (enum tls_model) log, false);
      return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (x, 0), 1));
    }

  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    {
      rtx tmp = legitimize_pe_coff_symbol (x, true);
      if (tmp)
        return tmp;
    }

  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

#if TARGET_MACHO
  if (MACHO_DYNAMIC_NO_PIC_P && SYMBOLIC_CONST (x))
    return machopic_indirect_data_reference (x, 0);
#endif

  /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
  if (GET_CODE (x) == ASHIFT
      && CONST_INT_P (XEXP (x, 1))
      && (unsigned HOST_WIDE_INT) INTVAL (XEXP (x, 1)) < 4)
    {
      changed = true;
      log = INTVAL (XEXP (x, 1));
      x = gen_rtx_MULT (Pmode, force_reg (Pmode, XEXP (x, 0)),
			GEN_INT (1 << log));
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Canonicalize shifts by 0, 1, 2, 3 into multiply.  */

      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (x, 0), 1)) < 4)
	{
	  changed = true;
	  log = INTVAL (XEXP (XEXP (x, 0), 1));
	  XEXP (x, 0) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 0), 0)),
				      GEN_INT (1 << log));
	}

      if (GET_CODE (XEXP (x, 1)) == ASHIFT
	  && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	  && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (x, 1), 1)) < 4)
	{
	  changed = true;
	  log = INTVAL (XEXP (XEXP (x, 1), 1));
	  XEXP (x, 1) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 1), 0)),
				      GEN_INT (1 << log));
	}

      /* Put multiply first if it isn't already.  */
      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  std::swap (XEXP (x, 0), XEXP (x, 1));
	  changed = true;
	}

      /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
	 created by virtual register instantiation, register elimination, and
	 similar optimizations.  */
      if (GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == PLUS)
	{
	  changed = true;
	  x = gen_rtx_PLUS (Pmode,
			    gen_rtx_PLUS (Pmode, XEXP (x, 0),
					  XEXP (XEXP (x, 1), 0)),
			    XEXP (XEXP (x, 1), 1));
	}

      /* Canonicalize
	 (plus (plus (mult (reg) (const)) (plus (reg) (const))) const)
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  */
      else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == PLUS
	       && CONSTANT_P (XEXP (x, 1)))
	{
	  rtx constant;
	  rtx other = NULL_RTX;

	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      constant = XEXP (x, 1);
	      other = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    }
	  else if (CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 1), 1)))
	    {
	      constant = XEXP (XEXP (XEXP (x, 0), 1), 1);
	      other = XEXP (x, 1);
	    }
	  else
	    constant = 0;

	  if (constant)
	    {
	      changed = true;
	      x = gen_rtx_PLUS (Pmode,
				gen_rtx_PLUS (Pmode, XEXP (XEXP (x, 0), 0),
					      XEXP (XEXP (XEXP (x, 0), 1), 0)),
				plus_constant (Pmode, other,
					       INTVAL (constant)));
	    }
	}

      if (changed && ix86_legitimate_address_p (mode, x, false))
	return x;

      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  changed = true;
	  XEXP (x, 0) = copy_addr_to_reg (XEXP (x, 0));
	}

      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  changed = true;
	  XEXP (x, 1) = copy_addr_to_reg (XEXP (x, 1));
	}

      if (changed
	  && REG_P (XEXP (x, 1))
	  && REG_P (XEXP (x, 0)))
	return x;

      if (flag_pic && SYMBOLIC_CONST (XEXP (x, 1)))
	{
	  changed = true;
	  x = legitimize_pic_address (x, 0);
	}

      if (changed && ix86_legitimate_address_p (mode, x, false))
	return x;

      if (REG_P (XEXP (x, 0)))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    {
	      val = convert_to_mode (Pmode, val, 1);
	      emit_move_insn (temp, val);
	    }

	  XEXP (x, 1) = temp;
	  return x;
	}

      else if (REG_P (XEXP (x, 1)))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    {
	      val = convert_to_mode (Pmode, val, 1);
	      emit_move_insn (temp, val);
	    }

	  XEXP (x, 0) = temp;
	  return x;
	}
    }

  return x;
}

/* Print an integer constant expression in assembler syntax.  Addition
   and subtraction are the only arithmetic that may appear in these
   expressions.  FILE is the stdio stream to write to, X is the rtx, and
   CODE is the operand print code from the output string.  */

static void
output_pic_addr_const (FILE *file, rtx x, int code)
{
  char buf[256];

  switch (GET_CODE (x))
    {
    case PC:
      gcc_assert (flag_pic);
      putc ('.', file);
      break;

    case SYMBOL_REF:
      if (TARGET_64BIT || ! TARGET_MACHO_SYMBOL_STUBS)
	output_addr_const (file, x);
      else
	{
	  const char *name = XSTR (x, 0);

	  /* Mark the decl as referenced so that cgraph will
	     output the function.  */
	  if (SYMBOL_REF_DECL (x))
	    mark_decl_referenced (SYMBOL_REF_DECL (x));

#if TARGET_MACHO
	  if (MACHOPIC_INDIRECT
	      && machopic_classify_symbol (x) == MACHOPIC_UNDEFINED_FUNCTION)
	    name = machopic_indirection_name (x, /*stub_p=*/true);
#endif
	  assemble_name (file, name);
	}
      if (!TARGET_MACHO && !(TARGET_64BIT && TARGET_PECOFF)
	  && code == 'P' && ix86_call_use_plt_p (x))
	fputs ("@PLT", file);
      break;

    case LABEL_REF:
      x = XEXP (x, 0);
      /* FALLTHRU */
    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (asm_out_file, buf);
      break;

    CASE_CONST_SCALAR_INT:
      output_addr_const (file, x);
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      /* We can't handle floating point constants;
	 TARGET_PRINT_OPERAND must handle them.  */
      output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear first.  */
      if (CONST_INT_P (XEXP (x, 0)))
	{
	  output_pic_addr_const (file, XEXP (x, 0), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else
	{
	  gcc_assert (CONST_INT_P (XEXP (x, 1)));
	  output_pic_addr_const (file, XEXP (x, 1), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 0), code);
	}
      break;

    case MINUS:
      if (!TARGET_MACHO)
	putc (ASSEMBLER_DIALECT == ASM_INTEL ? '(' : '[', file);
      output_pic_addr_const (file, XEXP (x, 0), code);
      putc ('-', file);
      output_pic_addr_const (file, XEXP (x, 1), code);
      if (!TARGET_MACHO)
	putc (ASSEMBLER_DIALECT == ASM_INTEL ? ')' : ']', file);
      break;

    case UNSPEC:
      gcc_assert (XVECLEN (x, 0) == 1);
      output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
      switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_GOTOFF:
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_PLTOFF:
	  fputs ("@PLTOFF", file);
	  break;
	case UNSPEC_PCREL:
	  fputs (ASSEMBLER_DIALECT == ASM_ATT ?
		 "(%rip)" : "[rip]", file);
	  break;
	case UNSPEC_GOTPCREL:
	  fputs (ASSEMBLER_DIALECT == ASM_ATT ?
		 "@GOTPCREL(%rip)" : "@GOTPCREL[rip]", file);
	  break;
	case UNSPEC_GOTTPOFF:
	  /* FIXME: This might be @TPOFF in Sun ld too.  */
	  fputs ("@gottpoff", file);
	  break;
	case UNSPEC_TPOFF:
	  fputs ("@tpoff", file);
	  break;
	case UNSPEC_NTPOFF:
	  if (TARGET_64BIT)
	    fputs ("@tpoff", file);
	  else
	    fputs ("@ntpoff", file);
	  break;
	case UNSPEC_DTPOFF:
	  fputs ("@dtpoff", file);
	  break;
	case UNSPEC_GOTNTPOFF:
	  if (TARGET_64BIT)
	    fputs (ASSEMBLER_DIALECT == ASM_ATT ?
		   "@gottpoff(%rip)": "@gottpoff[rip]", file);
	  else
	    fputs ("@gotntpoff", file);
	  break;
	case UNSPEC_INDNTPOFF:
	  fputs ("@indntpoff", file);
	  break;
#if TARGET_MACHO
	case UNSPEC_MACHOPIC_OFFSET:
	  putc ('-', file);
	  machopic_output_function_base_name (file);
	  break;
#endif
	default:
	  output_operand_lossage ("invalid UNSPEC as operand");
	  break;
	}
       break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* This is called from dwarf2out.cc via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void ATTRIBUTE_UNUSED
i386_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  fputs (ASM_LONG, file);
  output_addr_const (file, x);
  fputs ("@dtpoff", file);
  switch (size)
    {
    case 4:
      break;
    case 8:
      fputs (", 0", file);
      break;
    default:
      gcc_unreachable ();
   }
}

/* Return true if X is a representation of the PIC register.  This copes
   with calls from ix86_find_base_term, where the register might have
   been replaced by a cselib value.  */

static bool
ix86_pic_register_p (rtx x)
{
  if (GET_CODE (x) == VALUE && CSELIB_VAL_PTR (x))
    return (pic_offset_table_rtx
	    && rtx_equal_for_cselib_p (x, pic_offset_table_rtx));
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_SET_GOT)
    return true;
  else if (!REG_P (x))
    return false;
  else if (pic_offset_table_rtx)
    {
      if (REGNO (x) == REGNO (pic_offset_table_rtx))
	return true;
      if (HARD_REGISTER_P (x)
	  && !HARD_REGISTER_P (pic_offset_table_rtx)
	  && ORIGINAL_REGNO (x) == REGNO (pic_offset_table_rtx))
	return true;
      return false;
    }
  else
    return REGNO (x) == PIC_OFFSET_TABLE_REGNUM;
}

/* Helper function for ix86_delegitimize_address.
   Attempt to delegitimize TLS local-exec accesses.  */

static rtx
ix86_delegitimize_tls_address (rtx orig_x)
{
  rtx x = orig_x, unspec;
  struct ix86_address addr;

  if (!TARGET_TLS_DIRECT_SEG_REFS)
    return orig_x;
  if (MEM_P (x))
    x = XEXP (x, 0);
  if (GET_CODE (x) != PLUS || GET_MODE (x) != Pmode)
    return orig_x;
  if (ix86_decompose_address (x, &addr) == 0
      || addr.seg != DEFAULT_TLS_SEG_REG
      || addr.disp == NULL_RTX
      || GET_CODE (addr.disp) != CONST)
    return orig_x;
  unspec = XEXP (addr.disp, 0);
  if (GET_CODE (unspec) == PLUS && CONST_INT_P (XEXP (unspec, 1)))
    unspec = XEXP (unspec, 0);
  if (GET_CODE (unspec) != UNSPEC || XINT (unspec, 1) != UNSPEC_NTPOFF)
    return orig_x;
  x = XVECEXP (unspec, 0, 0);
  gcc_assert (GET_CODE (x) == SYMBOL_REF);
  if (unspec != XEXP (addr.disp, 0))
    x = gen_rtx_PLUS (Pmode, x, XEXP (XEXP (addr.disp, 0), 1));
  if (addr.index)
    {
      rtx idx = addr.index;
      if (addr.scale != 1)
	idx = gen_rtx_MULT (Pmode, idx, GEN_INT (addr.scale));
      x = gen_rtx_PLUS (Pmode, idx, x);
    }
  if (addr.base)
    x = gen_rtx_PLUS (Pmode, addr.base, x);
  if (MEM_P (orig_x))
    x = replace_equiv_address_nv (orig_x, x);
  return x;
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.

   On Darwin, this is necessary to avoid a crash, because Darwin
   has a different PIC label for each routine but the DWARF debugging
   information is not associated with any particular routine, so it's
   necessary to remove references to the PIC label from RTL stored by
   the DWARF output code.

   This helper is used in the normal ix86_delegitimize_address
   entrypoint (e.g. used in the target delegitimization hook) and
   in ix86_find_base_term.  As compile time memory optimization, we
   avoid allocating rtxes that will not change anything on the outcome
   of the callers (find_base_value and find_base_term).  */

static inline rtx
ix86_delegitimize_address_1 (rtx x, bool base_term_p)
{
  rtx orig_x = delegitimize_mem_from_attrs (x);
  /* addend is NULL or some rtx if x is something+GOTOFF where
     something doesn't include the PIC register.  */
  rtx addend = NULL_RTX;
  /* reg_addend is NULL or a multiple of some register.  */
  rtx reg_addend = NULL_RTX;
  /* const_addend is NULL or a const_int.  */
  rtx const_addend = NULL_RTX;
  /* This is the result, or NULL.  */
  rtx result = NULL_RTX;

  x = orig_x;

  if (MEM_P (x))
    x = XEXP (x, 0);

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) == CONST
          && GET_CODE (XEXP (x, 0)) == PLUS
          && GET_MODE (XEXP (x, 0)) == Pmode
          && CONST_INT_P (XEXP (XEXP (x, 0), 1))
          && GET_CODE (XEXP (XEXP (x, 0), 0)) == UNSPEC
          && XINT (XEXP (XEXP (x, 0), 0), 1) == UNSPEC_PCREL)
        {
	  /* find_base_{value,term} only care about MEMs with arg_pointer_rtx
	     base.  A CONST can't be arg_pointer_rtx based.  */
	  if (base_term_p && MEM_P (orig_x))
	    return orig_x;
	  rtx x2 = XVECEXP (XEXP (XEXP (x, 0), 0), 0, 0);
	  x = gen_rtx_PLUS (Pmode, XEXP (XEXP (x, 0), 1), x2);
	  if (MEM_P (orig_x))
	    x = replace_equiv_address_nv (orig_x, x);
	  return x;
	}

      if (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == UNSPEC
	  && (XINT (XEXP (x, 0), 1) == UNSPEC_GOTPCREL
	      || XINT (XEXP (x, 0), 1) == UNSPEC_PCREL)
	  && (MEM_P (orig_x) || XINT (XEXP (x, 0), 1) == UNSPEC_PCREL))
	{
	  x = XVECEXP (XEXP (x, 0), 0, 0);
	  if (GET_MODE (orig_x) != GET_MODE (x) && MEM_P (orig_x))
	    {
	      x = lowpart_subreg (GET_MODE (orig_x), x, GET_MODE (x));
	      if (x == NULL_RTX)
		return orig_x;
	    }
	  return x;
	}

      if (ix86_cmodel != CM_MEDIUM_PIC && ix86_cmodel != CM_LARGE_PIC)
	return ix86_delegitimize_tls_address (orig_x);

      /* Fall thru into the code shared with -m32 for -mcmodel=large -fpic
	 and -mcmodel=medium -fpic.  */
    }

  if (GET_CODE (x) != PLUS
      || GET_CODE (XEXP (x, 1)) != CONST)
    return ix86_delegitimize_tls_address (orig_x);

  if (ix86_pic_register_p (XEXP (x, 0)))
    /* %ebx + GOT/GOTOFF */
    ;
  else if (GET_CODE (XEXP (x, 0)) == PLUS)
    {
      /* %ebx + %reg * scale + GOT/GOTOFF */
      reg_addend = XEXP (x, 0);
      if (ix86_pic_register_p (XEXP (reg_addend, 0)))
	reg_addend = XEXP (reg_addend, 1);
      else if (ix86_pic_register_p (XEXP (reg_addend, 1)))
	reg_addend = XEXP (reg_addend, 0);
      else
	{
	  reg_addend = NULL_RTX;
	  addend = XEXP (x, 0);
	}
    }
  else
    addend = XEXP (x, 0);

  x = XEXP (XEXP (x, 1), 0);
  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1)))
    {
      const_addend = XEXP (x, 1);
      x = XEXP (x, 0);
    }

  if (GET_CODE (x) == UNSPEC
      && ((XINT (x, 1) == UNSPEC_GOT && MEM_P (orig_x) && !addend)
	  || (XINT (x, 1) == UNSPEC_GOTOFF && !MEM_P (orig_x))
	  || (XINT (x, 1) == UNSPEC_PLTOFF && ix86_cmodel == CM_LARGE_PIC
	      && !MEM_P (orig_x) && !addend)))
    result = XVECEXP (x, 0, 0);

  if (!TARGET_64BIT && TARGET_MACHO && darwin_local_data_pic (x)
      && !MEM_P (orig_x))
    result = XVECEXP (x, 0, 0);

  if (! result)
    return ix86_delegitimize_tls_address (orig_x);

  /* For (PLUS something CONST_INT) both find_base_{value,term} just
     recurse on the first operand.  */
  if (const_addend && !base_term_p)
    result = gen_rtx_CONST (Pmode, gen_rtx_PLUS (Pmode, result, const_addend));
  if (reg_addend)
    result = gen_rtx_PLUS (Pmode, reg_addend, result);
  if (addend)
    {
      /* If the rest of original X doesn't involve the PIC register, add
	 addend and subtract pic_offset_table_rtx.  This can happen e.g.
	 for code like:
	 leal (%ebx, %ecx, 4), %ecx
	 ...
	 movl foo@GOTOFF(%ecx), %edx
	 in which case we return (%ecx - %ebx) + foo
	 or (%ecx - _GLOBAL_OFFSET_TABLE_) + foo if pseudo_pic_reg
	 and reload has completed.  Don't do the latter for debug,
	 as _GLOBAL_OFFSET_TABLE_ can't be expressed in the assembly.  */
      if (pic_offset_table_rtx
	  && (!reload_completed || !ix86_use_pseudo_pic_reg ()))
        result = gen_rtx_PLUS (Pmode, gen_rtx_MINUS (Pmode, copy_rtx (addend),
						     pic_offset_table_rtx),
			       result);
      else if (base_term_p
	       && pic_offset_table_rtx
	       && !TARGET_MACHO
	       && !TARGET_VXWORKS_RTP)
	{
	  rtx tmp = gen_rtx_SYMBOL_REF (Pmode, GOT_SYMBOL_NAME);
	  tmp = gen_rtx_MINUS (Pmode, copy_rtx (addend), tmp);
	  result = gen_rtx_PLUS (Pmode, tmp, result);
	}
      else
	return orig_x;
    }
  if (GET_MODE (orig_x) != Pmode && MEM_P (orig_x))
    {
      result = lowpart_subreg (GET_MODE (orig_x), result, Pmode);
      if (result == NULL_RTX)
	return orig_x;
    }
  return result;
}

/* The normal instantiation of the above template.  */

static rtx
ix86_delegitimize_address (rtx x)
{
  return ix86_delegitimize_address_1 (x, false);
}

/* If X is a machine specific address (i.e. a symbol or label being
   referenced as a displacement from the GOT implemented using an
   UNSPEC), then return the base term.  Otherwise return X.  */

rtx
ix86_find_base_term (rtx x)
{
  rtx term;

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) != CONST)
	return x;
      term = XEXP (x, 0);
      if (GET_CODE (term) == PLUS
	  && CONST_INT_P (XEXP (term, 1)))
	term = XEXP (term, 0);
      if (GET_CODE (term) != UNSPEC
	  || (XINT (term, 1) != UNSPEC_GOTPCREL
	      && XINT (term, 1) != UNSPEC_PCREL))
	return x;

      return XVECEXP (term, 0, 0);
    }

  return ix86_delegitimize_address_1 (x, true);
}

/* Return true if X shouldn't be emitted into the debug info.
   Disallow UNSPECs other than @gotoff - we can't emit _GLOBAL_OFFSET_TABLE_
   symbol easily into the .debug_info section, so we need not to
   delegitimize, but instead assemble as @gotoff.
   Disallow _GLOBAL_OFFSET_TABLE_ SYMBOL_REF - the assembler magically
   assembles that as _GLOBAL_OFFSET_TABLE_-. expression.  */

static bool
ix86_const_not_ok_for_debug_p (rtx x)
{
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) != UNSPEC_GOTOFF)
    return true;

  if (SYMBOL_REF_P (x) && strcmp (XSTR (x, 0), GOT_SYMBOL_NAME) == 0)
    return true;

  return false;
}

static void
put_condition_code (enum rtx_code code, machine_mode mode, bool reverse,
		    bool fp, FILE *file)
{
  const char *suffix;

  if (mode == CCFPmode)
    {
      code = ix86_fp_compare_code_to_integer (code);
      mode = CCmode;
    }
  if (reverse)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:
      gcc_assert (mode != CCGZmode);
      switch (mode)
	{
	case E_CCAmode:
	  suffix = "a";
	  break;
	case E_CCCmode:
	  suffix = "c";
	  break;
	case E_CCOmode:
	  suffix = "o";
	  break;
	case E_CCPmode:
	  suffix = "p";
	  break;
	case E_CCSmode:
	  suffix = "s";
	  break;
	default:
	  suffix = "e";
	  break;
	}
      break;
    case NE:
      gcc_assert (mode != CCGZmode);
      switch (mode)
	{
	case E_CCAmode:
	  suffix = "na";
	  break;
	case E_CCCmode:
	  suffix = "nc";
	  break;
	case E_CCOmode:
	  suffix = "no";
	  break;
	case E_CCPmode:
	  suffix = "np";
	  break;
	case E_CCSmode:
	  suffix = "ns";
	  break;
	default:
	  suffix = "ne";
	  break;
	}
      break;
    case GT:
      gcc_assert (mode == CCmode || mode == CCNOmode || mode == CCGCmode);
      suffix = "g";
      break;
    case GTU:
      /* ??? Use "nbe" instead of "a" for fcmov lossage on some assemblers.
	 Those same assemblers have the same but opposite lossage on cmov.  */
      if (mode == CCmode)
	suffix = fp ? "nbe" : "a";
      else
	gcc_unreachable ();
      break;
    case LT:
      switch (mode)
	{
	case E_CCNOmode:
	case E_CCGOCmode:
	  suffix = "s";
	  break;

	case E_CCmode:
	case E_CCGCmode:
	case E_CCGZmode:
	  suffix = "l";
	  break;

	default:
	  gcc_unreachable ();
	}
      break;
    case LTU:
      if (mode == CCmode || mode == CCGZmode)
	suffix = "b";
      else if (mode == CCCmode)
	suffix = fp ? "b" : "c";
      else
	gcc_unreachable ();
      break;
    case GE:
      switch (mode)
	{
	case E_CCNOmode:
	case E_CCGOCmode:
	  suffix = "ns";
	  break;

	case E_CCmode:
	case E_CCGCmode:
	case E_CCGZmode:
	  suffix = "ge";
	  break;

	default:
	  gcc_unreachable ();
	}
      break;
    case GEU:
      if (mode == CCmode || mode == CCGZmode)
	suffix = "nb";
      else if (mode == CCCmode)
	suffix = fp ? "nb" : "nc";
      else
	gcc_unreachable ();
      break;
    case LE:
      gcc_assert (mode == CCmode || mode == CCGCmode || mode == CCNOmode);
      suffix = "le";
      break;
    case LEU:
      if (mode == CCmode)
	suffix = "be";
      else
	gcc_unreachable ();
      break;
    case UNORDERED:
      suffix = fp ? "u" : "p";
      break;
    case ORDERED:
      suffix = fp ? "nu" : "np";
      break;
    default:
      gcc_unreachable ();
    }
  fputs (suffix, file);
}

/* Print the name of register X to FILE based on its machine mode and number.
   If CODE is 'w', pretend the mode is HImode.
   If CODE is 'b', pretend the mode is QImode.
   If CODE is 'k', pretend the mode is SImode.
   If CODE is 'q', pretend the mode is DImode.
   If CODE is 'x', pretend the mode is V4SFmode.
   If CODE is 't', pretend the mode is V8SFmode.
   If CODE is 'g', pretend the mode is V16SFmode.
   If CODE is 'h', pretend the reg is the 'high' byte register.
   If CODE is 'y', print "st(0)" instead of "st", if the reg is stack op.
   If CODE is 'd', duplicate the operand for AVX instruction.
   If CODE is 'V', print naked full integer register name without %.
 */

void
print_reg (rtx x, int code, FILE *file)
{
  const char *reg;
  int msize;
  unsigned int regno;
  bool duplicated;

  if (ASSEMBLER_DIALECT == ASM_ATT && code != 'V')
    putc ('%', file);

  if (x == pc_rtx)
    {
      gcc_assert (TARGET_64BIT);
      fputs ("rip", file);
      return;
    }

  if (code == 'y' && STACK_TOP_P (x))
    {
      fputs ("st(0)", file);
      return;
    }

  if (code == 'w')
    msize = 2;
  else if (code == 'b')
    msize = 1;
  else if (code == 'k')
    msize = 4;
  else if (code == 'q')
    msize = 8;
  else if (code == 'h')
    msize = 0;
  else if (code == 'x')
    msize = 16;
  else if (code == 't')
    msize = 32;
  else if (code == 'g')
    msize = 64;
  else
    msize = GET_MODE_SIZE (GET_MODE (x));

  regno = REGNO (x);

  if (regno == ARG_POINTER_REGNUM
      || regno == FRAME_POINTER_REGNUM
      || regno == FPSR_REG)
    {
      output_operand_lossage
	("invalid use of register '%s'", reg_names[regno]);
      return;
    }
  else if (regno == FLAGS_REG)
    {
      output_operand_lossage ("invalid use of asm flag output");
      return;
    }

  if (code == 'V')
    {
      if (GENERAL_REGNO_P (regno))
	msize = GET_MODE_SIZE (word_mode);
      else
	error ("%<V%> modifier on non-integer register");
    }

  duplicated = code == 'd' && TARGET_AVX;

  switch (msize)
    {
    case 16:
    case 12:
    case 8:
      if (GENERAL_REGNO_P (regno) && msize > GET_MODE_SIZE (word_mode))
	warning (0, "unsupported size for integer register");
      /* FALLTHRU */
    case 4:
      if (LEGACY_INT_REGNO_P (regno))
	putc (msize > 4 && TARGET_64BIT ? 'r' : 'e', file);
      /* FALLTHRU */
    case 2:
    normal:
      reg = hi_reg_name[regno];
      break;
    case 1:
      if (regno >= ARRAY_SIZE (qi_reg_name))
	goto normal;
      if (!ANY_QI_REGNO_P (regno))
	error ("unsupported size for integer register");
      reg = qi_reg_name[regno];
      break;
    case 0:
      if (regno >= ARRAY_SIZE (qi_high_reg_name))
	goto normal;
      reg = qi_high_reg_name[regno];
      break;
    case 32:
    case 64:
      if (SSE_REGNO_P (regno))
	{
	  gcc_assert (!duplicated);
	  putc (msize == 32 ? 'y' : 'z', file);
	  reg = hi_reg_name[regno] + 1;
	  break;
	}
      goto normal;
    default:
      gcc_unreachable ();
    }

  fputs (reg, file);

  /* Irritatingly, AMD extended registers use
     different naming convention: "r%d[bwd]"  */
  if (REX_INT_REGNO_P (regno) || REX2_INT_REGNO_P (regno))
    {
      gcc_assert (TARGET_64BIT);
      switch (msize)
	{
	  case 0:
	    error ("extended registers have no high halves");
	    break;
	  case 1:
	    putc ('b', file);
	    break;
	  case 2:
	    putc ('w', file);
	    break;
	  case 4:
	    putc ('d', file);
	    break;
	  case 8:
	    /* no suffix */
	    break;
	  default:
	    error ("unsupported operand size for extended register");
	    break;
	}
      return;
    }

  if (duplicated)
    {
      if (ASSEMBLER_DIALECT == ASM_ATT)
	fprintf (file, ", %%%s", reg);
      else
	fprintf (file, ", %s", reg);
    }
}

/* Meaning of CODE:
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
   C -- print opcode suffix for set/cmov insn.
   c -- like C, but print reversed condition
   F,f -- likewise, but for floating-point.
   O -- if HAVE_AS_IX86_CMOV_SUN_SYNTAX, expand to "w.", "l." or "q.",
	otherwise nothing
   R -- print embedded rounding and sae.
   r -- print only sae.
   z -- print the opcode suffix for the size of the current operand.
   Z -- likewise, with special suffixes for x87 instructions.
   * -- print a star (in certain assembler syntax)
   A -- print an absolute memory reference.
   E -- print address with DImode register names if TARGET_64BIT.
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   s -- print a shift double count, followed by the assemblers argument
	delimiter.
   b -- print the QImode name of the register for the indicated operand.
	%b0 would print %al if operands[0] is reg 0.
   w --  likewise, print the HImode name of the register.
   k --  likewise, print the SImode name of the register.
   q --  likewise, print the DImode name of the register.
   x --  likewise, print the V4SFmode name of the register.
   t --  likewise, print the V8SFmode name of the register.
   g --  likewise, print the V16SFmode name of the register.
   h -- print the QImode name for a "high" register, either ah, bh, ch or dh.
   y -- print "st(0)" instead of "st" as a register.
   d -- print duplicated register operand for AVX instruction.
   D -- print condition for SSE cmp instruction.
   P -- if PIC, print an @PLT suffix.  For -fno-plt, load function
	address from GOT.
   p -- print raw symbol name.
   X -- don't print any sort of PIC '@' suffix for a symbol.
   & -- print some in-use local-dynamic symbol name.
   H -- print a memory address offset by 8; used for sse high-parts
   Y -- print condition for XOP pcom* instruction.
   V -- print naked full integer register name without %.
   + -- print a branch hint as 'cs' or 'ds' prefix
   ; -- print a semicolon (after prefixes due to bug in older gas).
   ~ -- print "i" if TARGET_AVX2, "f" otherwise.
   ^ -- print addr32 prefix if TARGET_64BIT and Pmode != word_mode
   M -- print addr32 prefix for TARGET_X32 with VSIB address.
   ! -- print NOTRACK prefix for jxx/call/ret instructions if required.
   N -- print maskz if it's constant 0 operand.
 */

void
ix86_print_operand (FILE *file, rtx x, int code)
{
  if (code)
    {
      switch (code)
	{
	case 'A':
	  switch (ASSEMBLER_DIALECT)
	    {
	    case ASM_ATT:
	      putc ('*', file);
	      break;

	    case ASM_INTEL:
	      /* Intel syntax. For absolute addresses, registers should not
		 be surrounded by braces.  */
	      if (!REG_P (x))
		{
		  putc ('[', file);
		  ix86_print_operand (file, x, 0);
		  putc (']', file);
		  return;
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  ix86_print_operand (file, x, 0);
	  return;

	case 'E':
	  /* Wrap address in an UNSPEC to declare special handling.  */
	  if (TARGET_64BIT)
	    x = gen_rtx_UNSPEC (DImode, gen_rtvec (1, x), UNSPEC_LEA_ADDR);

	  output_address (VOIDmode, x);
	  return;

	case 'L':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('l', file);
	  return;

	case 'W':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('w', file);
	  return;

	case 'B':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('b', file);
	  return;

	case 'Q':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('l', file);
	  return;

	case 'S':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('s', file);
	  return;

	case 'T':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('t', file);
	  return;

	case 'O':
#ifdef HAVE_AS_IX86_CMOV_SUN_SYNTAX
	  if (ASSEMBLER_DIALECT != ASM_ATT)
	    return;

	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 2:
	      putc ('w', file);
	      break;
  
	    case 4:
	      putc ('l', file);
	      break;

	    case 8:
	      putc ('q', file);
	      break;

	    default:
	      output_operand_lossage ("invalid operand size for operand "
				      "code 'O'");
	      return;
	    }

	  putc ('.', file);
#endif
	  return;

	case 'z':
	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
	    {
	      /* Opcodes don't get size suffixes if using Intel opcodes.  */
	      if (ASSEMBLER_DIALECT == ASM_INTEL)
		return;

	      switch (GET_MODE_SIZE (GET_MODE (x)))
		{
		case 1:
		  putc ('b', file);
		  return;

		case 2:
		  putc ('w', file);
		  return;

		case 4:
		  putc ('l', file);
		  return;

		case 8:
		  putc ('q', file);
		  return;

		default:
		  output_operand_lossage ("invalid operand size for operand "
					  "code 'z'");
		  return;
		}
	    }

	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	    {
	      if (this_is_asm_operands)
		warning_for_asm (this_is_asm_operands,
				 "non-integer operand used with operand code %<z%>");
	      else
		warning (0, "non-integer operand used with operand code %<z%>");
	    }
	  /* FALLTHRU */

	case 'Z':
	  /* 387 opcodes don't get size suffixes if using Intel opcodes.  */
	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    return;

	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
	    {
	      switch (GET_MODE_SIZE (GET_MODE (x)))
		{
		case 2:
#ifdef HAVE_AS_IX86_FILDS
		  putc ('s', file);
#endif
		  return;

		case 4:
		  putc ('l', file);
		  return;

		case 8:
#ifdef HAVE_AS_IX86_FILDQ
		  putc ('q', file);
#else
		  fputs ("ll", file);
#endif
		  return;

		default:
		  break;
		}
	    }
	  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	    {
	      /* 387 opcodes don't get size suffixes
		 if the operands are registers.  */
	      if (STACK_REG_P (x))
		return;

	      switch (GET_MODE_SIZE (GET_MODE (x)))
		{
		case 4:
		  putc ('s', file);
		  return;

		case 8:
		  putc ('l', file);
		  return;

		case 12:
		case 16:
		  putc ('t', file);
		  return;

		default:
		  break;
		}
	    }
	  else
	    {
	      output_operand_lossage ("invalid operand type used with "
				      "operand code '%c'", code);
	      return;
	    }

	  output_operand_lossage ("invalid operand size for operand code '%c'",
				  code);
	  return;

	case 'd':
	case 'b':
	case 'w':
	case 'k':
	case 'q':
	case 'h':
	case 't':
	case 'g':
	case 'y':
	case 'x':
	case 'X':
	case 'P':
	case 'p':
	case 'V':
	  break;

	case 's':
	  if (CONST_INT_P (x) || ! SHIFT_DOUBLE_OMITS_COUNT)
	    {
	      ix86_print_operand (file, x, 0);
	      fputs (", ", file);
	    }
	  return;

	case 'Y':
	  switch (GET_CODE (x))
	    {
	    case NE:
	      fputs ("neq", file);
	      break;
	    case EQ:
	      fputs ("eq", file);
	      break;
	    case GE:
	    case GEU:
	      fputs (INTEGRAL_MODE_P (GET_MODE (x)) ? "ge" : "unlt", file);
	      break;
	    case GT:
	    case GTU:
	      fputs (INTEGRAL_MODE_P (GET_MODE (x)) ? "gt" : "unle", file);
	      break;
	    case LE:
	    case LEU:
	      fputs ("le", file);
	      break;
	    case LT:
	    case LTU:
	      fputs ("lt", file);
	      break;
	    case UNORDERED:
	      fputs ("unord", file);
	      break;
	    case ORDERED:
	      fputs ("ord", file);
	      break;
	    case UNEQ:
	      fputs ("ueq", file);
	      break;
	    case UNGE:
	      fputs ("nlt", file);
	      break;
	    case UNGT:
	      fputs ("nle", file);
	      break;
	    case UNLE:
	      fputs ("ule", file);
	      break;
	    case UNLT:
	      fputs ("ult", file);
	      break;
	    case LTGT:
	      fputs ("une", file);
	      break;
	    default:
	      output_operand_lossage ("operand is not a condition code, "
				      "invalid operand code 'Y'");
	      return;
	    }
	  return;

	case 'D':
	  /* Little bit of braindamage here.  The SSE compare instructions
	     does use completely different names for the comparisons that the
	     fp conditional moves.  */
	  switch (GET_CODE (x))
	    {
	    case UNEQ:
	      if (TARGET_AVX)
		{
		  fputs ("eq_us", file);
		  break;
		}
	     /* FALLTHRU */
	    case EQ:
	      fputs ("eq", file);
	      break;
	    case UNLT:
	      if (TARGET_AVX)
		{
		  fputs ("nge", file);
		  break;
		}
	     /* FALLTHRU */
	    case LT:
	      fputs ("lt", file);
	      break;
	    case UNLE:
	      if (TARGET_AVX)
		{
		  fputs ("ngt", file);
		  break;
		}
	     /* FALLTHRU */
	    case LE:
	      fputs ("le", file);
	      break;
	    case UNORDERED:
	      fputs ("unord", file);
	      break;
	    case LTGT:
	      if (TARGET_AVX)
		{
		  fputs ("neq_oq", file);
		  break;
		}
	     /* FALLTHRU */
	    case NE:
	      fputs ("neq", file);
	      break;
	    case GE:
	      if (TARGET_AVX)
		{
		  fputs ("ge", file);
		  break;
		}
	     /* FALLTHRU */
	    case UNGE:
	      fputs ("nlt", file);
	      break;
	    case GT:
	      if (TARGET_AVX)
		{
		  fputs ("gt", file);
		  break;
		}
	     /* FALLTHRU */
	    case UNGT:
	      fputs ("nle", file);
	      break;
	    case ORDERED:
	      fputs ("ord", file);
	      break;
	    default:
	      output_operand_lossage ("operand is not a condition code, "
				      "invalid operand code 'D'");
	      return;
	    }
	  return;

	case 'F':
	case 'f':
#ifdef HAVE_AS_IX86_CMOV_SUN_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('.', file);
	  gcc_fallthrough ();
#endif

	case 'C':
	case 'c':
	  if (!COMPARISON_P (x))
	    {
	      output_operand_lossage ("operand is not a condition code, "
				      "invalid operand code '%c'", code);
	      return;
	    }
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)),
			      code == 'c' || code == 'f',
			      code == 'F' || code == 'f',
			      file);
	  return;

	case 'H':
	  if (!offsettable_memref_p (x))
	    {
	      output_operand_lossage ("operand is not an offsettable memory "
				      "reference, invalid operand code 'H'");
	      return;
	    }
	  /* It doesn't actually matter what mode we use here, as we're
	     only going to use this for printing.  */
	  x = adjust_address_nv (x, DImode, 8);
	  /* Output 'qword ptr' for intel assembler dialect.  */
	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    code = 'q';
	  break;

	case 'K':
	  if (!CONST_INT_P (x))
	    {
	      output_operand_lossage ("operand is not an integer, invalid "
				      "operand code 'K'");
	      return;
	    }

	  if (INTVAL (x) & IX86_HLE_ACQUIRE)
#ifdef HAVE_AS_IX86_HLE
	    fputs ("xacquire ", file);
#else
	    fputs ("\n" ASM_BYTE "0xf2\n\t", file);
#endif
	  else if (INTVAL (x) & IX86_HLE_RELEASE)
#ifdef HAVE_AS_IX86_HLE
	    fputs ("xrelease ", file);
#else
	    fputs ("\n" ASM_BYTE "0xf3\n\t", file);
#endif
	  /* We do not want to print value of the operand.  */
	  return;

	case 'N':
	  if (x == const0_rtx || x == CONST0_RTX (GET_MODE (x)))
	    fputs ("{z}", file);
	  return;

	case 'r':
	  if (!CONST_INT_P (x) || INTVAL (x) != ROUND_SAE)
	    {
	      output_operand_lossage ("operand is not a specific integer, "
				      "invalid operand code 'r'");
	      return;
	    }

	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    fputs (", ", file);

	  fputs ("{sae}", file);

	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    fputs (", ", file);

	  return;

	case 'R':
	  if (!CONST_INT_P (x))
	    {
	      output_operand_lossage ("operand is not an integer, invalid "
				      "operand code 'R'");
	      return;
	    }

	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    fputs (", ", file);

	  switch (INTVAL (x))
	    {
	    case ROUND_NEAREST_INT | ROUND_SAE:
	      fputs ("{rn-sae}", file);
	      break;
	    case ROUND_NEG_INF | ROUND_SAE:
	      fputs ("{rd-sae}", file);
	      break;
	    case ROUND_POS_INF | ROUND_SAE:
	      fputs ("{ru-sae}", file);
	      break;
	    case ROUND_ZERO | ROUND_SAE:
	      fputs ("{rz-sae}", file);
	      break;
	    default:
	      output_operand_lossage ("operand is not a specific integer, "
				      "invalid operand code 'R'");
	    }

	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    fputs (", ", file);

	  return;

	case '*':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('*', file);
	  return;

	case '&':
	  {
	    const char *name = get_some_local_dynamic_name ();
	    if (name == NULL)
	      output_operand_lossage ("'%%&' used without any "
				      "local dynamic TLS references");
	    else
	      assemble_name (file, name);
	    return;
	  }

	case '+':
	  {
	    rtx x;

	    if (!optimize
	        || optimize_function_for_size_p (cfun)
		|| (!TARGET_BRANCH_PREDICTION_HINTS_NOT_TAKEN
		    && !TARGET_BRANCH_PREDICTION_HINTS_TAKEN))
	      return;

	    x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	    if (x)
	      {
		int pred_val = profile_probability::from_reg_br_prob_note
				 (XINT (x, 0)).to_reg_br_prob_base ();

		bool taken = pred_val > REG_BR_PROB_BASE / 2;
		/* We use 3e (DS) prefix for taken branches and
		   2e (CS) prefix for not taken branches.  */
		if (taken && TARGET_BRANCH_PREDICTION_HINTS_TAKEN)
		  fputs ("ds ; ", file);
		else if (!taken && TARGET_BRANCH_PREDICTION_HINTS_NOT_TAKEN)
		  fputs ("cs ; ", file);
	      }
	    return;
	  }

	case ';':
#ifndef HAVE_AS_IX86_REP_LOCK_PREFIX
	  putc (';', file);
#endif
	  return;

	case '~':
	  putc (TARGET_AVX2 ? 'i' : 'f', file);
	  return;

	case 'M':
	  if (TARGET_X32)
	    {
	      /* NB: 32-bit indices in VSIB address are sign-extended
		 to 64 bits. In x32, if 32-bit address 0xf7fa3010 is
		 sign-extended to 0xfffffffff7fa3010 which is invalid
		 address.  Add addr32 prefix if there is no base
		 register nor symbol.  */
	      bool ok;
	      struct ix86_address parts;
	      ok = ix86_decompose_address (x, &parts);
	      gcc_assert (ok && parts.index == NULL_RTX);
	      if (parts.base == NULL_RTX
		  && (parts.disp == NULL_RTX
		      || !symbolic_operand (parts.disp,
					    GET_MODE (parts.disp))))
		fputs ("addr32 ", file);
	    }
	  return;

	case '^':
	  if (TARGET_64BIT && Pmode != word_mode)
	    fputs ("addr32 ", file);
	  return;

	case '!':
	  if (ix86_notrack_prefixed_insn_p (current_output_insn))
	    fputs ("notrack ", file);
	  return;

	default:
	  output_operand_lossage ("invalid operand code '%c'", code);
	}
    }

  if (REG_P (x))
    print_reg (x, code, file);

  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);

      /* No `byte ptr' prefix for call instructions ... */
      if (ASSEMBLER_DIALECT == ASM_INTEL && code != 'X' && code != 'P')
	{
	  machine_mode mode = GET_MODE (x);
	  const char *size;

	  /* Check for explicit size override codes.  */
	  if (code == 'b')
	    size = "BYTE";
	  else if (code == 'w')
	    size = "WORD";
	  else if (code == 'k')
	    size = "DWORD";
	  else if (code == 'q')
	    size = "QWORD";
	  else if (code == 'x')
	    size = "XMMWORD";
	  else if (code == 't')
	    size = "YMMWORD";
	  else if (code == 'g')
	    size = "ZMMWORD";
	  else if (mode == BLKmode)
	    /* ... or BLKmode operands, when not overridden.  */
	    size = NULL;
	  else
	    switch (GET_MODE_SIZE (mode))
	      {
	      case 1: size = "BYTE"; break;
	      case 2: size = "WORD"; break;
	      case 4: size = "DWORD"; break;
	      case 8: size = "QWORD"; break;
	      case 12: size = "TBYTE"; break;
	      case 16:
		if (mode == XFmode)
		  size = "TBYTE";
		else
		  size = "XMMWORD";
		break;
	      case 32: size = "YMMWORD"; break;
	      case 64: size = "ZMMWORD"; break;
	      default:
		gcc_unreachable ();
	      }
	  if (size)
	    {
	      fputs (size, file);
	      fputs (" PTR ", file);
	    }
	}

      if (this_is_asm_operands && ! address_operand (addr, VOIDmode))
	output_operand_lossage ("invalid constraints for operand");
      else
	ix86_print_operand_address_as
	  (file, addr, MEM_ADDR_SPACE (x), code == 'p' || code == 'P');
    }

  else if (CONST_DOUBLE_P (x) && GET_MODE (x) == HFmode)
    {
      long l = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (x),
			       REAL_MODE_FORMAT (HFmode));
      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('$', file);
      fprintf (file, "0x%04x", (unsigned int) l);
    }

  else if (CONST_DOUBLE_P (x) && GET_MODE (x) == SFmode)
    {
      long l;

      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);

      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('$', file);
      /* Sign extend 32bit SFmode immediate to 8 bytes.  */
      if (code == 'q')
	fprintf (file, "0x%08" HOST_LONG_LONG_FORMAT "x",
		 (unsigned long long) (int) l);
      else
	fprintf (file, "0x%08x", (unsigned int) l);
    }

  else if (CONST_DOUBLE_P (x) && GET_MODE (x) == DFmode)
    {
      long l[2];

      REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), l);

      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('$', file);
      fprintf (file, "0x%lx%08lx", l[1] & 0xffffffff, l[0] & 0xffffffff);
    }

  /* These float cases don't actually occur as immediate operands.  */
  else if (CONST_DOUBLE_P (x) && GET_MODE (x) == XFmode)
    {
      char dstr[30];

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fputs (dstr, file);
    }

  /* Print bcst_mem_operand.  */
  else if (GET_CODE (x) == VEC_DUPLICATE)
    {
      machine_mode vmode = GET_MODE (x);
      /* Must be bcst_memory_operand.  */
      gcc_assert (bcst_mem_operand (x, vmode));

      rtx mem = XEXP (x,0);
      ix86_print_operand (file, mem, 0);

      switch (vmode)
	{
	case E_V2DImode:
	case E_V2DFmode:
	  fputs ("{1to2}", file);
	  break;
	case E_V4SImode:
	case E_V4SFmode:
	case E_V4DImode:
	case E_V4DFmode:
	  fputs ("{1to4}", file);
	  break;
	case E_V8SImode:
	case E_V8SFmode:
	case E_V8DFmode:
	case E_V8DImode:
	case E_V8HFmode:
	  fputs ("{1to8}", file);
	  break;
	case E_V16SFmode:
	case E_V16SImode:
	case E_V16HFmode:
	  fputs ("{1to16}", file);
	  break;
	case E_V32HFmode:
	  fputs ("{1to32}", file);
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  else
    {
      /* We have patterns that allow zero sets of memory, for instance.
	 In 64-bit mode, we should probably support all 8-byte vectors,
	 since we can in fact encode that into an immediate.  */
      if (GET_CODE (x) == CONST_VECTOR)
	{
	  if (x != CONST0_RTX (GET_MODE (x)))
	    output_operand_lossage ("invalid vector immediate");
	  x = const0_rtx;
	}

      if (code == 'P')
	{
	  if (ix86_force_load_from_GOT_p (x, true))
	    {
	      /* For inline assembly statement, load function address
		 from GOT with 'P' operand modifier to avoid PLT.  */
	      x = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x),
				  (TARGET_64BIT
				   ? UNSPEC_GOTPCREL
				   : UNSPEC_GOT));
	      x = gen_rtx_CONST (Pmode, x);
	      x = gen_const_mem (Pmode, x);
	      ix86_print_operand (file, x, 'A');
	      return;
	    }
	}
      else if (code != 'p')
	{
	  if (CONST_INT_P (x))
	    {
	      if (ASSEMBLER_DIALECT == ASM_ATT)
		putc ('$', file);
	    }
	  else if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF
		   || GET_CODE (x) == LABEL_REF)
	    {
	      if (ASSEMBLER_DIALECT == ASM_ATT)
		putc ('$', file);
	      else
		fputs ("OFFSET FLAT:", file);
	    }
	}
      if (CONST_INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      else if (flag_pic || MACHOPIC_INDIRECT)
	output_pic_addr_const (file, x, code);
      else
	output_addr_const (file, x);
    }
}

static bool
ix86_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '*' || code == '+' || code == '&' || code == ';'
	  || code == '~' || code == '^' || code == '!');
}

/* Print a memory operand whose address is ADDR.  */

static void
ix86_print_operand_address_as (FILE *file, rtx addr,
			       addr_space_t as, bool raw)
{
  struct ix86_address parts;
  rtx base, index, disp;
  int scale;
  int ok;
  bool vsib = false;
  int code = 0;

  if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_VSIBADDR)
    {
      ok = ix86_decompose_address (XVECEXP (addr, 0, 0), &parts);
      gcc_assert (parts.index == NULL_RTX);
      parts.index = XVECEXP (addr, 0, 1);
      parts.scale = INTVAL (XVECEXP (addr, 0, 2));
      addr = XVECEXP (addr, 0, 0);
      vsib = true;
    }
  else if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_LEA_ADDR)
    {
      gcc_assert (TARGET_64BIT);
      ok = ix86_decompose_address (XVECEXP (addr, 0, 0), &parts);
      code = 'q';
    }
  else
    ok = ix86_decompose_address (addr, &parts);

  gcc_assert (ok);

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  if (ADDR_SPACE_GENERIC_P (as))
    as = parts.seg;
  else
    gcc_assert (ADDR_SPACE_GENERIC_P (parts.seg));

  if (!ADDR_SPACE_GENERIC_P (as) && !raw)
    {
      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('%', file);

      switch (as)
	{
	case ADDR_SPACE_SEG_FS:
	  fputs ("fs:", file);
	  break;
	case ADDR_SPACE_SEG_GS:
	  fputs ("gs:", file);
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  /* Use one byte shorter RIP relative addressing for 64bit mode.  */
  if (TARGET_64BIT && !base && !index && !raw)
    {
      rtx symbol = disp;

      if (GET_CODE (disp) == CONST
	  && GET_CODE (XEXP (disp, 0)) == PLUS
	  && CONST_INT_P (XEXP (XEXP (disp, 0), 1)))
	symbol = XEXP (XEXP (disp, 0), 0);

      if (GET_CODE (symbol) == LABEL_REF
	  || (GET_CODE (symbol) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (symbol) == 0))
	base = pc_rtx;
    }

  if (!base && !index)
    {
      /* Displacement only requires special attention.  */
      if (CONST_INT_P (disp))
	{
	  if (ASSEMBLER_DIALECT == ASM_INTEL && ADDR_SPACE_GENERIC_P (as))
	    fputs ("ds:", file);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (disp));
	}
      /* Load the external function address via the GOT slot to avoid PLT.  */
      else if (GET_CODE (disp) == CONST
	       && GET_CODE (XEXP (disp, 0)) == UNSPEC
	       && (XINT (XEXP (disp, 0), 1) == UNSPEC_GOTPCREL
		   || XINT (XEXP (disp, 0), 1) == UNSPEC_GOT)
	       && ix86_force_load_from_GOT_p (XVECEXP (XEXP (disp, 0), 0, 0)))
	output_pic_addr_const (file, disp, 0);
      else if (flag_pic)
	output_pic_addr_const (file, disp, 0);
      else
	output_addr_const (file, disp);
    }
  else
    {
      /* Print SImode register names to force addr32 prefix.  */
      if (SImode_address_operand (addr, VOIDmode))
	{
	  if (flag_checking)
	    {
	      gcc_assert (TARGET_64BIT);
	      switch (GET_CODE (addr))
		{
		case SUBREG:
		  gcc_assert (GET_MODE (addr) == SImode);
		  gcc_assert (GET_MODE (SUBREG_REG (addr)) == DImode);
		  break;
		case ZERO_EXTEND:
		case AND:
		  gcc_assert (GET_MODE (addr) == DImode);
		  break;
		default:
		  gcc_unreachable ();
		}
	    }
	  gcc_assert (!code);
	  code = 'k';
	}
      else if (code == 0
	       && TARGET_X32
	       && disp
	       && CONST_INT_P (disp)
	       && INTVAL (disp) < -16*1024*1024)
	{
	  /* X32 runs in 64-bit mode, where displacement, DISP, in
	     address DISP(%r64), is encoded as 32-bit immediate sign-
	     extended from 32-bit to 64-bit.  For -0x40000300(%r64),
	     address is %r64 + 0xffffffffbffffd00.  When %r64 <
	     0x40000300, like 0x37ffe064, address is 0xfffffffff7ffdd64,
	     which is invalid for x32.  The correct address is %r64
	     - 0x40000300 == 0xf7ffdd64.  To properly encode
	     -0x40000300(%r64) for x32, we zero-extend negative
	     displacement by forcing addr32 prefix which truncates
	     0xfffffffff7ffdd64 to 0xf7ffdd64.  In theory, we should
	     zero-extend all negative displacements, including -1(%rsp).
	     However, for small negative displacements, sign-extension
	     won't cause overflow.  We only zero-extend negative
	     displacements if they < -16*1024*1024, which is also used
	     to check legitimate address displacements for PIC.  */
	  code = 'k';
	}

      /* Since the upper 32 bits of RSP are always zero for x32,
	 we can encode %esp as %rsp to avoid 0x67 prefix if
	 there is no index register.  */
      if (TARGET_X32 && Pmode == SImode
	  && !index && base && REG_P (base) && REGNO (base) == SP_REG)
	code = 'q';

      if (ASSEMBLER_DIALECT == ASM_ATT)
	{
	  if (disp)
	    {
	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else
		output_addr_const (file, disp);
	    }

	  putc ('(', file);
	  if (base)
	    print_reg (base, code, file);
	  if (index)
	    {
	      putc (',', file);
	      print_reg (index, vsib ? 0 : code, file);
	      if (scale != 1 || vsib)
		fprintf (file, ",%d", scale);
	    }
	  putc (')', file);
	}
      else
	{
	  rtx offset = NULL_RTX;

	  if (disp)
	    {
	      /* Pull out the offset of a symbol; print any symbol itself.  */
	      if (GET_CODE (disp) == CONST
		  && GET_CODE (XEXP (disp, 0)) == PLUS
		  && CONST_INT_P (XEXP (XEXP (disp, 0), 1)))
		{
		  offset = XEXP (XEXP (disp, 0), 1);
		  disp = gen_rtx_CONST (VOIDmode,
					XEXP (XEXP (disp, 0), 0));
		}

	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else if (CONST_INT_P (disp))
		offset = disp;
	      else
		output_addr_const (file, disp);
	    }

	  putc ('[', file);
	  if (base)
	    {
	      print_reg (base, code, file);
	      if (offset)
		{
		  if (INTVAL (offset) >= 0)
		    putc ('+', file);
		  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
		}
	    }
	  else if (offset)
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
	  else
	    putc ('0', file);

	  if (index)
	    {
	      putc ('+', file);
	      print_reg (index, vsib ? 0 : code, file);
	      if (scale != 1 || vsib)
		fprintf (file, "*%d", scale);
	    }
	  putc (']', file);
	}
    }
}

static void
ix86_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{
  if (this_is_asm_operands && ! address_operand (addr, VOIDmode))
    output_operand_lossage ("invalid constraints for operand");
  else
    ix86_print_operand_address_as (file, addr, ADDR_SPACE_GENERIC, false);
}

/* Implementation of TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
i386_asm_output_addr_const_extra (FILE *file, rtx x)
{
  rtx op;

  if (GET_CODE (x) != UNSPEC)
    return false;

  op = XVECEXP (x, 0, 0);
  switch (XINT (x, 1))
    {
    case UNSPEC_GOTOFF:
      output_addr_const (file, op);
      fputs ("@gotoff", file);
      break;
    case UNSPEC_GOTTPOFF:
      output_addr_const (file, op);
      /* FIXME: This might be @TPOFF in Sun ld.  */
      fputs ("@gottpoff", file);
      break;
    case UNSPEC_TPOFF:
      output_addr_const (file, op);
      fputs ("@tpoff", file);
      break;
    case UNSPEC_NTPOFF:
      output_addr_const (file, op);
      if (TARGET_64BIT)
	fputs ("@tpoff", file);
      else
	fputs ("@ntpoff", file);
      break;
    case UNSPEC_DTPOFF:
      output_addr_const (file, op);
      fputs ("@dtpoff", file);
      break;
    case UNSPEC_GOTNTPOFF:
      output_addr_const (file, op);
      if (TARGET_64BIT)
	fputs (ASSEMBLER_DIALECT == ASM_ATT ?
	       "@gottpoff(%rip)" : "@gottpoff[rip]", file);
      else
	fputs ("@gotntpoff", file);
      break;
    case UNSPEC_INDNTPOFF:
      output_addr_const (file, op);
      fputs ("@indntpoff", file);
      break;
#if TARGET_MACHO
    case UNSPEC_MACHOPIC_OFFSET:
      output_addr_const (file, op);
      putc ('-', file);
      machopic_output_function_base_name (file);
      break;
#endif

    default:
      return false;
    }

  return true;
}


/* Output code to perform a 387 binary operation in INSN, one of PLUS,
   MINUS, MULT or DIV.  OPERANDS are the insn operands, where operands[3]
   is the expression of the binary operation.  The output may either be
   emitted here, or returned to the caller, like all output_* functions.

   There is no guarantee that the operands are the same mode, as they
   might be within FLOAT or FLOAT_EXTEND expressions.  */

#ifndef SYSV386_COMPAT
/* Set to 1 for compatibility with brain-damaged assemblers.  No-one
   wants to fix the assemblers because that causes incompatibility
   with gcc.  No-one wants to fix gcc because that causes
   incompatibility with assemblers...  You can use the option of
   -DSYSV386_COMPAT=0 if you recompile both gcc and gas this way.  */
#define SYSV386_COMPAT 1
#endif

const char *
output_387_binary_op (rtx_insn *insn, rtx *operands)
{
  static char buf[40];
  const char *p;
  bool is_sse
    = (SSE_REG_P (operands[0])
       || SSE_REG_P (operands[1]) || SSE_REG_P (operands[2]));

  if (is_sse)
    p = "%v";
  else if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	   || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
    p = "fi";
  else
    p = "f";

  strcpy (buf, p);

  switch (GET_CODE (operands[3]))
    {
    case PLUS:
      p = "add"; break;
    case MINUS:
      p = "sub"; break;
    case MULT:
      p = "mul"; break;
    case DIV:
      p = "div"; break;
    default:
      gcc_unreachable ();
    }

  strcat (buf, p);

  if (is_sse)
   {
     p = GET_MODE (operands[0]) == SFmode ? "ss" : "sd";
     strcat (buf, p);

     if (TARGET_AVX)
       p = "\t{%2, %1, %0|%0, %1, %2}";
     else
       p = "\t{%2, %0|%0, %2}";

     strcat (buf, p);
     return buf;
   }

  /* Even if we do not want to check the inputs, this documents input
     constraints.  Which helps in understanding the following code.  */
  if (flag_checking)
    {
      if (STACK_REG_P (operands[0])
	  && ((REG_P (operands[1])
	       && REGNO (operands[0]) == REGNO (operands[1])
	       && (STACK_REG_P (operands[2]) || MEM_P (operands[2])))
	      || (REG_P (operands[2])
		  && REGNO (operands[0]) == REGNO (operands[2])
		  && (STACK_REG_P (operands[1]) || MEM_P (operands[1]))))
	  && (STACK_TOP_P (operands[1]) || STACK_TOP_P (operands[2])))
	; /* ok */
      else
	gcc_unreachable ();
    }

  switch (GET_CODE (operands[3]))
    {
    case MULT:
    case PLUS:
      if (REG_P (operands[2]) && REGNO (operands[0]) == REGNO (operands[2]))
	std::swap (operands[1], operands[2]);

      /* know operands[0] == operands[1].  */

      if (MEM_P (operands[2]))
	{
	  p = "%Z2\t%2";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
	  if (STACK_TOP_P (operands[0]))
	    /* How is it that we are storing to a dead operand[2]?
	       Well, presumably operands[1] is dead too.  We can't
	       store the result to st(0) as st(0) gets popped on this
	       instruction.  Instead store to operands[2] (which I
	       think has to be st(1)).  st(1) will be popped later.
	       gcc <= 2.8.1 didn't have this check and generated
	       assembly code that the Unixware assembler rejected.  */
	    p = "p\t{%0, %2|%2, %0}";	/* st(1) = st(0) op st(1); pop */
	  else
	    p = "p\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0); pop */
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	p = "\t{%y2, %0|%0, %y2}";	/* st(0) = st(0) op st(r2) */
      else
	p = "\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0) */
      break;

    case MINUS:
    case DIV:
      if (MEM_P (operands[1]))
	{
	  p = "r%Z1\t%1";
	  break;
	}

      if (MEM_P (operands[2]))
	{
	  p = "%Z2\t%2";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
#if SYSV386_COMPAT
	  /* The SystemV/386 SVR3.2 assembler, and probably all AT&T
	     derived assemblers, confusingly reverse the direction of
	     the operation for fsub{r} and fdiv{r} when the
	     destination register is not st(0).  The Intel assembler
	     doesn't have this brain damage.  Read !SYSV386_COMPAT to
	     figure out what the hardware really does.  */
	  if (STACK_TOP_P (operands[0]))
	    p = "{p\t%0, %2|rp\t%2, %0}";
	  else
	    p = "{rp\t%2, %0|p\t%0, %2}";
#else
	  if (STACK_TOP_P (operands[0]))
	    /* As above for fmul/fadd, we can't store to st(0).  */
	    p = "rp\t{%0, %2|%2, %0}";	/* st(1) = st(0) op st(1); pop */
	  else
	    p = "p\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0); pop */
#endif
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	{
#if SYSV386_COMPAT
	  if (STACK_TOP_P (operands[0]))
	    p = "{rp\t%0, %1|p\t%1, %0}";
	  else
	    p = "{p\t%1, %0|rp\t%0, %1}";
#else
	  if (STACK_TOP_P (operands[0]))
	    p = "p\t{%0, %1|%1, %0}";	/* st(1) = st(1) op st(0); pop */
	  else
	    p = "rp\t{%1, %0|%0, %1}";	/* st(r2) = st(0) op st(r2); pop */
#endif
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	{
	  if (STACK_TOP_P (operands[1]))
	    p = "\t{%y2, %0|%0, %y2}";	/* st(0) = st(0) op st(r2) */
	  else
	    p = "r\t{%y1, %0|%0, %y1}";	/* st(0) = st(r1) op st(0) */
	  break;
	}
      else if (STACK_TOP_P (operands[1]))
	{
#if SYSV386_COMPAT
	  p = "{\t%1, %0|r\t%0, %1}";
#else
	  p = "r\t{%1, %0|%0, %1}";	/* st(r2) = st(0) op st(r2) */
#endif
	}
      else
	{
#if SYSV386_COMPAT
	  p = "{r\t%2, %0|\t%0, %2}";
#else
	  p = "\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0) */
#endif
	}
      break;

    default:
      gcc_unreachable ();
    }

  strcat (buf, p);
  return buf;
}

/* Return needed mode for entity in optimize_mode_switching pass.  */

static int
ix86_dirflag_mode_needed (rtx_insn *insn)
{
  if (CALL_P (insn))
    {
      if (cfun->machine->func_type == TYPE_NORMAL)
	return X86_DIRFLAG_ANY;
      else
	/* No need to emit CLD in interrupt handler for TARGET_CLD.  */
	return TARGET_CLD ? X86_DIRFLAG_ANY : X86_DIRFLAG_RESET;
    }

  if (recog_memoized (insn) < 0)
    return X86_DIRFLAG_ANY;

  if (get_attr_type (insn) == TYPE_STR)
    {
      /* Emit cld instruction if stringops are used in the function.  */
      if (cfun->machine->func_type == TYPE_NORMAL)
	return TARGET_CLD ? X86_DIRFLAG_RESET : X86_DIRFLAG_ANY;
      else
	return X86_DIRFLAG_RESET;
    }

  return X86_DIRFLAG_ANY;
}

/* Check if a 256bit or 512 bit AVX register is referenced inside of EXP.   */

static bool
ix86_check_avx_upper_register (const_rtx exp)
{
  return (SSE_REG_P (exp)
	  && !EXT_REX_SSE_REG_P (exp)
	  && GET_MODE_BITSIZE (GET_MODE (exp)) > 128);
}

/* Check if a 256bit or 512bit AVX register is referenced in stores.   */

static void
ix86_check_avx_upper_stores (rtx dest, const_rtx, void *data)
{
  if (ix86_check_avx_upper_register (dest))
    {
      bool *used = (bool *) data;
      *used = true;
    }
}

/* Return needed mode for entity in optimize_mode_switching pass.  */

static int
ix86_avx_u128_mode_needed (rtx_insn *insn)
{
  if (DEBUG_INSN_P (insn))
    return AVX_U128_ANY;

  if (CALL_P (insn))
    {
      rtx link;

      /* Needed mode is set to AVX_U128_CLEAN if there are
	 no 256bit or 512bit modes used in function arguments. */
      for (link = CALL_INSN_FUNCTION_USAGE (insn);
	   link;
	   link = XEXP (link, 1))
	{
	  if (GET_CODE (XEXP (link, 0)) == USE)
	    {
	      rtx arg = XEXP (XEXP (link, 0), 0);

	      if (ix86_check_avx_upper_register (arg))
		return AVX_U128_DIRTY;
	    }
	}

      /* Needed mode is set to AVX_U128_CLEAN if there are no 256bit
	 nor 512bit registers used in the function return register.  */
      bool avx_upper_reg_found = false;
      note_stores (insn, ix86_check_avx_upper_stores,
		   &avx_upper_reg_found);
      if (avx_upper_reg_found)
	return AVX_U128_DIRTY;

      /* If the function is known to preserve some SSE registers,
	 RA and previous passes can legitimately rely on that for
	 modes wider than 256 bits.  It's only safe to issue a
	 vzeroupper if all SSE registers are clobbered.  */
      const function_abi &abi = insn_callee_abi (insn);
      if (vzeroupper_pattern (PATTERN (insn), VOIDmode)
	  /* Should be safe to issue an vzeroupper before sibling_call_p.
	     Also there not mode_exit for sibling_call, so there could be
	     missing vzeroupper for that.  */
	  || !(SIBLING_CALL_P (insn)
	       || hard_reg_set_subset_p (reg_class_contents[SSE_REGS],
					 abi.mode_clobbers (V4DImode))))
	return AVX_U128_ANY;

      return AVX_U128_CLEAN;
    }

  subrtx_iterator::array_type array;

  rtx set = single_set (insn);
  if (set)
    {
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);
      if (ix86_check_avx_upper_register (dest))
	{
	  /* This is an YMM/ZMM load.  Return AVX_U128_DIRTY if the
	     source isn't zero.  */
	  if (standard_sse_constant_p (src, GET_MODE (dest)) != 1)
	    return AVX_U128_DIRTY;
	  else
	    return AVX_U128_ANY;
	}
      else
	{
	  FOR_EACH_SUBRTX (iter, array, src, NONCONST)
	    if (ix86_check_avx_upper_register (*iter))
	      return AVX_U128_DIRTY;
	}

      /* This isn't YMM/ZMM load/store.  */
      return AVX_U128_ANY;
    }

  /* Require DIRTY mode if a 256bit or 512bit AVX register is referenced.
     Hardware changes state only when a 256bit register is written to,
     but we need to prevent the compiler from moving optimal insertion
     point above eventual read from 256bit or 512 bit register.  */
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    if (ix86_check_avx_upper_register (*iter))
      return AVX_U128_DIRTY;

  return AVX_U128_ANY;
}

/* Return mode that i387 must be switched into
   prior to the execution of insn.  */

static int
ix86_i387_mode_needed (int entity, rtx_insn *insn)
{
  enum attr_i387_cw mode;

  /* The mode UNINITIALIZED is used to store control word after a
     function call or ASM pattern.  The mode ANY specify that function
     has no requirements on the control word and make no changes in the
     bits we are interested in.  */

  if (CALL_P (insn)
      || (NONJUMP_INSN_P (insn)
	  && (asm_noperands (PATTERN (insn)) >= 0
	      || GET_CODE (PATTERN (insn)) == ASM_INPUT)))
    return I387_CW_UNINITIALIZED;

  if (recog_memoized (insn) < 0)
    return I387_CW_ANY;

  mode = get_attr_i387_cw (insn);

  switch (entity)
    {
    case I387_ROUNDEVEN:
      if (mode == I387_CW_ROUNDEVEN)
	return mode;
      break;

    case I387_TRUNC:
      if (mode == I387_CW_TRUNC)
	return mode;
      break;

    case I387_FLOOR:
      if (mode == I387_CW_FLOOR)
	return mode;
      break;

    case I387_CEIL:
      if (mode == I387_CW_CEIL)
	return mode;
      break;

    default:
      gcc_unreachable ();
    }

  return I387_CW_ANY;
}

/* Return mode that entity must be switched into
   prior to the execution of insn.  */

static int
ix86_mode_needed (int entity, rtx_insn *insn, HARD_REG_SET)
{
  switch (entity)
    {
    case X86_DIRFLAG:
      return ix86_dirflag_mode_needed (insn);
    case AVX_U128:
      return ix86_avx_u128_mode_needed (insn);
    case I387_ROUNDEVEN:
    case I387_TRUNC:
    case I387_FLOOR:
    case I387_CEIL:
      return ix86_i387_mode_needed (entity, insn);
    default:
      gcc_unreachable ();
    }
  return 0;
}

/* Calculate mode of upper 128bit AVX registers after the insn.  */

static int
ix86_avx_u128_mode_after (int mode, rtx_insn *insn)
{
  rtx pat = PATTERN (insn);

  if (vzeroupper_pattern (pat, VOIDmode)
      || vzeroall_pattern (pat, VOIDmode))
    return AVX_U128_CLEAN;

  /* We know that state is clean after CALL insn if there are no
     256bit or 512bit registers used in the function return register. */
  if (CALL_P (insn))
    {
      bool avx_upper_reg_found = false;
      note_stores (insn, ix86_check_avx_upper_stores, &avx_upper_reg_found);

      if (avx_upper_reg_found)
	return AVX_U128_DIRTY;

      /* If the function desn't clobber any sse registers or only clobber
	 128-bit part, Then vzeroupper isn't issued before the function exit.
	 the status not CLEAN but ANY after the function.  */
      const function_abi &abi = insn_callee_abi (insn);
      if (!(SIBLING_CALL_P (insn)
	    || hard_reg_set_subset_p (reg_class_contents[SSE_REGS],
				      abi.mode_clobbers (V4DImode))))
	return AVX_U128_ANY;

      return  AVX_U128_CLEAN;
    }

  /* Otherwise, return current mode.  Remember that if insn
     references AVX 256bit or 512bit registers, the mode was already
     changed to DIRTY from MODE_NEEDED.  */
  return mode;
}

/* Return the mode that an insn results in.  */

static int
ix86_mode_after (int entity, int mode, rtx_insn *insn, HARD_REG_SET)
{
  switch (entity)
    {
    case X86_DIRFLAG:
      return mode;
    case AVX_U128:
      return ix86_avx_u128_mode_after (mode, insn);
    case I387_ROUNDEVEN:
    case I387_TRUNC:
    case I387_FLOOR:
    case I387_CEIL:
      return mode;
    default:
      gcc_unreachable ();
    }
}

static int
ix86_dirflag_mode_entry (void)
{
  /* For TARGET_CLD or in the interrupt handler we can't assume
     direction flag state at function entry.  */
  if (TARGET_CLD
      || cfun->machine->func_type != TYPE_NORMAL)
    return X86_DIRFLAG_ANY;

  return X86_DIRFLAG_RESET;
}

static int
ix86_avx_u128_mode_entry (void)
{
  tree arg;

  /* Entry mode is set to AVX_U128_DIRTY if there are
     256bit or 512bit modes used in function arguments.  */
  for (arg = DECL_ARGUMENTS (current_function_decl); arg;
       arg = TREE_CHAIN (arg))
    {
      rtx incoming = DECL_INCOMING_RTL (arg);

      if (incoming && ix86_check_avx_upper_register (incoming))
	return AVX_U128_DIRTY;
    }

  return AVX_U128_CLEAN;
}

/* Return a mode that ENTITY is assumed to be
   switched to at function entry.  */

static int
ix86_mode_entry (int entity)
{
  switch (entity)
    {
    case X86_DIRFLAG:
      return ix86_dirflag_mode_entry ();
    case AVX_U128:
      return ix86_avx_u128_mode_entry ();
    case I387_ROUNDEVEN:
    case I387_TRUNC:
    case I387_FLOOR:
    case I387_CEIL:
      return I387_CW_ANY;
    default:
      gcc_unreachable ();
    }
}

static int
ix86_avx_u128_mode_exit (void)
{
  rtx reg = crtl->return_rtx;

  /* Exit mode is set to AVX_U128_DIRTY if there are 256bit
     or 512 bit modes used in the function return register. */
  if (reg && ix86_check_avx_upper_register (reg))
    return AVX_U128_DIRTY;

  /* Exit mode is set to AVX_U128_DIRTY if there are 256bit or 512bit
     modes used in function arguments, otherwise return AVX_U128_CLEAN.
   */
  return ix86_avx_u128_mode_entry ();
}

/* Return a mode that ENTITY is assumed to be
   switched to at function exit.  */

static int
ix86_mode_exit (int entity)
{
  switch (entity)
    {
    case X86_DIRFLAG:
      return X86_DIRFLAG_ANY;
    case AVX_U128:
      return ix86_avx_u128_mode_exit ();
    case I387_ROUNDEVEN:
    case I387_TRUNC:
    case I387_FLOOR:
    case I387_CEIL:
      return I387_CW_ANY;
    default:
      gcc_unreachable ();
    }
}

static int
ix86_mode_priority (int, int n)
{
  return n;
}

/* Output code to initialize control word copies used by trunc?f?i and
   rounding patterns.  CURRENT_MODE is set to current control word,
   while NEW_MODE is set to new control word.  */

static void
emit_i387_cw_initialization (int mode)
{
  rtx stored_mode = assign_386_stack_local (HImode, SLOT_CW_STORED);
  rtx new_mode;

  enum ix86_stack_slot slot;

  rtx reg = gen_reg_rtx (HImode);

  emit_insn (gen_x86_fnstcw_1 (stored_mode));
  emit_move_insn (reg, copy_rtx (stored_mode));

  switch (mode)
    {
    case I387_CW_ROUNDEVEN:
      /* round to nearest */
      emit_insn (gen_andhi3 (reg, reg, GEN_INT (~0x0c00)));
      slot = SLOT_CW_ROUNDEVEN;
      break;

    case I387_CW_TRUNC:
      /* round toward zero (truncate) */
      emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0c00)));
      slot = SLOT_CW_TRUNC;
      break;

    case I387_CW_FLOOR:
      /* round down toward -oo */
      emit_insn (gen_andhi3 (reg, reg, GEN_INT (~0x0c00)));
      emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0400)));
      slot = SLOT_CW_FLOOR;
      break;

    case I387_CW_CEIL:
      /* round up toward +oo */
      emit_insn (gen_andhi3 (reg, reg, GEN_INT (~0x0c00)));
      emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0800)));
      slot = SLOT_CW_CEIL;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (slot < MAX_386_STACK_LOCALS);

  new_mode = assign_386_stack_local (HImode, slot);
  emit_move_insn (new_mode, reg);
}

/* Generate one or more insns to set ENTITY to MODE.  */

static void
ix86_emit_mode_set (int entity, int mode, int prev_mode ATTRIBUTE_UNUSED,
		    HARD_REG_SET regs_live ATTRIBUTE_UNUSED)
{
  switch (entity)
    {
    case X86_DIRFLAG:
      if (mode == X86_DIRFLAG_RESET)
	emit_insn (gen_cld ());
      break;
    case AVX_U128:
      if (mode == AVX_U128_CLEAN)
	ix86_expand_avx_vzeroupper ();
      break;
    case I387_ROUNDEVEN:
    case I387_TRUNC:
    case I387_FLOOR:
    case I387_CEIL:
      if (mode != I387_CW_ANY
	  && mode != I387_CW_UNINITIALIZED)
	emit_i387_cw_initialization (mode);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Output code for INSN to convert a float to a signed int.  OPERANDS
   are the insn operands.  The output may be [HSD]Imode and the input
   operand may be [SDX]Fmode.  */

const char *
output_fix_trunc (rtx_insn *insn, rtx *operands, bool fisttp)
{
  bool stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG);
  bool dimode_p = GET_MODE (operands[0]) == DImode;
  int round_mode = get_attr_i387_cw (insn);

  static char buf[40];
  const char *p;

  /* Jump through a hoop or two for DImode, since the hardware has no
     non-popping instruction.  We used to do this a different way, but
     that was somewhat fragile and broke with post-reload splitters.  */
  if ((dimode_p || fisttp) && !stack_top_dies)
    output_asm_insn ("fld\t%y1", operands);

  gcc_assert (STACK_TOP_P (operands[1]));
  gcc_assert (MEM_P (operands[0]));
  gcc_assert (GET_MODE (operands[1]) != TFmode);

  if (fisttp)
    return "fisttp%Z0\t%0";

  strcpy (buf, "fist");

  if (round_mode != I387_CW_ANY)
    output_asm_insn ("fldcw\t%3", operands);

  p = "p%Z0\t%0";
  strcat (buf, p + !(stack_top_dies || dimode_p));

  output_asm_insn (buf, operands);

  if (round_mode != I387_CW_ANY)
    output_asm_insn ("fldcw\t%2", operands);

  return "";
}

/* Output code for x87 ffreep insn.  The OPNO argument, which may only
   have the values zero or one, indicates the ffreep insn's operand
   from the OPERANDS array.  */

static const char *
output_387_ffreep (rtx *operands ATTRIBUTE_UNUSED, int opno)
{
  if (TARGET_USE_FFREEP)
#ifdef HAVE_AS_IX86_FFREEP
    return opno ? "ffreep\t%y1" : "ffreep\t%y0";
#else
    {
      static char retval[32];
      int regno = REGNO (operands[opno]);

      gcc_assert (STACK_REGNO_P (regno));

      regno -= FIRST_STACK_REG;

      snprintf (retval, sizeof (retval), ASM_SHORT "0xc%ddf", regno);
      return retval;
    }
#endif

  return opno ? "fstp\t%y1" : "fstp\t%y0";
}


/* Output code for INSN to compare OPERANDS.  EFLAGS_P is 1 when fcomi
   should be used.  UNORDERED_P is true when fucom should be used.  */

const char *
output_fp_compare (rtx_insn *insn, rtx *operands,
		   bool eflags_p, bool unordered_p)
{
  rtx *xops = eflags_p ? &operands[0] : &operands[1];
  bool stack_top_dies;

  static char buf[40];
  const char *p;

  gcc_assert (STACK_TOP_P (xops[0]));

  stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG);

  if (eflags_p)
    {
      p = unordered_p ? "fucomi" : "fcomi";
      strcpy (buf, p);

      p = "p\t{%y1, %0|%0, %y1}";
      strcat (buf, p + !stack_top_dies);

      return buf;
    }

  if (STACK_REG_P (xops[1])
      && stack_top_dies
      && find_regno_note (insn, REG_DEAD, FIRST_STACK_REG + 1))
    {
      gcc_assert (REGNO (xops[1]) == FIRST_STACK_REG + 1);

      /* If both the top of the 387 stack die, and the other operand
	 is also a stack register that dies, then this must be a
	 `fcompp' float compare.  */
      p = unordered_p ? "fucompp" : "fcompp";
      strcpy (buf, p);
    }
  else if (const0_operand (xops[1], VOIDmode))
    {
      gcc_assert (!unordered_p);
      strcpy (buf, "ftst");
    }
  else
    {
      if (GET_MODE_CLASS (GET_MODE (xops[1])) == MODE_INT)
	{
	  gcc_assert (!unordered_p);
	  p = "ficom";
	}
      else
	p = unordered_p ? "fucom" : "fcom";

      strcpy (buf, p);

      p = "p%Z2\t%y2";
      strcat (buf, p + !stack_top_dies);
    }

  output_asm_insn (buf, operands);
  return "fnstsw\t%0";
}

void
ix86_output_addr_vec_elt (FILE *file, int value)
{
  const char *directive = ASM_LONG;

#ifdef ASM_QUAD
  if (TARGET_LP64)
    directive = ASM_QUAD;
#else
  gcc_assert (!TARGET_64BIT);
#endif

  fprintf (file, "%s%s%d\n", directive, LPREFIX, value);
}

void
ix86_output_addr_diff_elt (FILE *file, int value, int rel)
{
  const char *directive = ASM_LONG;

#ifdef ASM_QUAD
  if (TARGET_64BIT && CASE_VECTOR_MODE == DImode)
    directive = ASM_QUAD;
#else
  gcc_assert (!TARGET_64BIT);
#endif
  /* We can't use @GOTOFF for text labels on VxWorks; see gotoff_operand.  */
  if (TARGET_64BIT || TARGET_VXWORKS_RTP)
    fprintf (file, "%s%s%d-%s%d\n",
	     directive, LPREFIX, value, LPREFIX, rel);
#if TARGET_MACHO
  else if (TARGET_MACHO)
    {
      fprintf (file, ASM_LONG "%s%d-", LPREFIX, value);
      machopic_output_function_base_name (file);
      putc ('\n', file);
    }
#endif
  else if (HAVE_AS_GOTOFF_IN_DATA)
    fprintf (file, ASM_LONG "%s%d@GOTOFF\n", LPREFIX, value);
  else
    asm_fprintf (file, ASM_LONG "%U%s+[.-%s%d]\n",
		 GOT_SYMBOL_NAME, LPREFIX, value);
}

#define LEA_MAX_STALL (3)
#define LEA_SEARCH_THRESHOLD (LEA_MAX_STALL << 1)

/* Increase given DISTANCE in half-cycles according to
   dependencies between PREV and NEXT instructions.
   Add 1 half-cycle if there is no dependency and
   go to next cycle if there is some dependecy.  */

static unsigned int
increase_distance (rtx_insn *prev, rtx_insn *next, unsigned int distance)
{
  df_ref def, use;

  if (!prev || !next)
    return distance + (distance & 1) + 2;

  if (!DF_INSN_USES (next) || !DF_INSN_DEFS (prev))
    return distance + 1;

  FOR_EACH_INSN_USE (use, next)
    FOR_EACH_INSN_DEF (def, prev)
      if (!DF_REF_IS_ARTIFICIAL (def)
	  && DF_REF_REGNO (use) == DF_REF_REGNO (def))
	return distance + (distance & 1) + 2;

  return distance + 1;
}

/* Function checks if instruction INSN defines register number
   REGNO1 or REGNO2.  */

bool
insn_defines_reg (unsigned int regno1, unsigned int regno2,
		  rtx_insn *insn)
{
  df_ref def;

  FOR_EACH_INSN_DEF (def, insn)
    if (DF_REF_REG_DEF_P (def)
	&& !DF_REF_IS_ARTIFICIAL (def)
	&& (regno1 == DF_REF_REGNO (def)
	    || regno2 == DF_REF_REGNO (def)))
      return true;

  return false;
}

/* Function checks if instruction INSN uses register number
   REGNO as a part of address expression.  */

static bool
insn_uses_reg_mem (unsigned int regno, rtx insn)
{
  df_ref use;

  FOR_EACH_INSN_USE (use, insn)
    if (DF_REF_REG_MEM_P (use) && regno == DF_REF_REGNO (use))
      return true;

  return false;
}

/* Search backward for non-agu definition of register number REGNO1
   or register number REGNO2 in basic block starting from instruction
   START up to head of basic block or instruction INSN.

   Function puts true value into *FOUND var if definition was found
   and false otherwise.

   Distance in half-cycles between START and found instruction or head
   of BB is added to DISTANCE and returned.  */

static int
distance_non_agu_define_in_bb (unsigned int regno1, unsigned int regno2,
			       rtx_insn *insn, int distance,
			       rtx_insn *start, bool *found)
{
  basic_block bb = start ? BLOCK_FOR_INSN (start) : NULL;
  rtx_insn *prev = start;
  rtx_insn *next = NULL;

  *found = false;

  while (prev
	 && prev != insn
	 && distance < LEA_SEARCH_THRESHOLD)
    {
      if (NONDEBUG_INSN_P (prev) && NONJUMP_INSN_P (prev))
	{
	  distance = increase_distance (prev, next, distance);
	  if (insn_defines_reg (regno1, regno2, prev))
	    {
	      if (recog_memoized (prev) < 0
		  || get_attr_type (prev) != TYPE_LEA)
		{
		  *found = true;
		  return distance;
		}
	    }

	  next = prev;
	}
      if (prev == BB_HEAD (bb))
	break;

      prev = PREV_INSN (prev);
    }

  return distance;
}

/* Search backward for non-agu definition of register number REGNO1
   or register number REGNO2 in INSN's basic block until
   1. Pass LEA_SEARCH_THRESHOLD instructions, or
   2. Reach neighbor BBs boundary, or
   3. Reach agu definition.
   Returns the distance between the non-agu definition point and INSN.
   If no definition point, returns -1.  */

static int
distance_non_agu_define (unsigned int regno1, unsigned int regno2,
			 rtx_insn *insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  int distance = 0;
  bool found = false;

  if (insn != BB_HEAD (bb))
    distance = distance_non_agu_define_in_bb (regno1, regno2, insn,
					      distance, PREV_INSN (insn),
					      &found);

  if (!found && distance < LEA_SEARCH_THRESHOLD)
    {
      edge e;
      edge_iterator ei;
      bool simple_loop = false;

      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src == bb)
	  {
	    simple_loop = true;
	    break;
	  }

      if (simple_loop)
	distance = distance_non_agu_define_in_bb (regno1, regno2,
						  insn, distance,
						  BB_END (bb), &found);
      else
	{
	  int shortest_dist = -1;
	  bool found_in_bb = false;

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      int bb_dist
		= distance_non_agu_define_in_bb (regno1, regno2,
						 insn, distance,
						 BB_END (e->src),
						 &found_in_bb);
	      if (found_in_bb)
		{
		  if (shortest_dist < 0)
		    shortest_dist = bb_dist;
		  else if (bb_dist > 0)
		    shortest_dist = MIN (bb_dist, shortest_dist);

		  found = true;
		}
	    }

	  distance = shortest_dist;
	}
    }

  if (!found)
    return -1;

  return distance >> 1;
}

/* Return the distance in half-cycles between INSN and the next
   insn that uses register number REGNO in memory address added
   to DISTANCE.  Return -1 if REGNO0 is set.

   Put true value into *FOUND if register usage was found and
   false otherwise.
   Put true value into *REDEFINED if register redefinition was
   found and false otherwise.  */

static int
distance_agu_use_in_bb (unsigned int regno,
			rtx_insn *insn, int distance, rtx_insn *start,
			bool *found, bool *redefined)
{
  basic_block bb = NULL;
  rtx_insn *next = start;
  rtx_insn *prev = NULL;

  *found = false;
  *redefined = false;

  if (start != NULL_RTX)
    {
      bb = BLOCK_FOR_INSN (start);
      if (start != BB_HEAD (bb))
	/* If insn and start belong to the same bb, set prev to insn,
	   so the call to increase_distance will increase the distance
	   between insns by 1.  */
	prev = insn;
    }

  while (next
	 && next != insn
	 && distance < LEA_SEARCH_THRESHOLD)
    {
      if (NONDEBUG_INSN_P (next) && NONJUMP_INSN_P (next))
	{
	  distance = increase_distance(prev, next, distance);
	  if (insn_uses_reg_mem (regno, next))
	    {
	      /* Return DISTANCE if OP0 is used in memory
		 address in NEXT.  */
	      *found = true;
	      return distance;
	    }

	  if (insn_defines_reg (regno, INVALID_REGNUM, next))
	    {
	      /* Return -1 if OP0 is set in NEXT.  */
	      *redefined = true;
	      return -1;
	    }

	  prev = next;
	}

      if (next == BB_END (bb))
	break;

      next = NEXT_INSN (next);
    }

  return distance;
}

/* Return the distance between INSN and the next insn that uses
   register number REGNO0 in memory address.  Return -1 if no such
   a use is found within LEA_SEARCH_THRESHOLD or REGNO0 is set.  */

static int
distance_agu_use (unsigned int regno0, rtx_insn *insn)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  int distance = 0;
  bool found = false;
  bool redefined = false;

  if (insn != BB_END (bb))
    distance = distance_agu_use_in_bb (regno0, insn, distance,
				       NEXT_INSN (insn),
				       &found, &redefined);

  if (!found && !redefined && distance < LEA_SEARCH_THRESHOLD)
    {
      edge e;
      edge_iterator ei;
      bool simple_loop = false;

      FOR_EACH_EDGE (e, ei, bb->succs)
        if (e->dest == bb)
	  {
	    simple_loop = true;
	    break;
	  }

      if (simple_loop)
	distance = distance_agu_use_in_bb (regno0, insn,
					   distance, BB_HEAD (bb),
					   &found, &redefined);
      else
	{
	  int shortest_dist = -1;
	  bool found_in_bb = false;
	  bool redefined_in_bb = false;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      int bb_dist
		= distance_agu_use_in_bb (regno0, insn,
					  distance, BB_HEAD (e->dest),
					  &found_in_bb, &redefined_in_bb);
	      if (found_in_bb)
		{
		  if (shortest_dist < 0)
		    shortest_dist = bb_dist;
		  else if (bb_dist > 0)
		    shortest_dist = MIN (bb_dist, shortest_dist);

		  found = true;
		}
	    }

	  distance = shortest_dist;
	}
    }

  if (!found || redefined)
    return -1;

  return distance >> 1;
}

/* Define this macro to tune LEA priority vs ADD, it take effect when
   there is a dilemma of choosing LEA or ADD
   Negative value: ADD is more preferred than LEA
   Zero: Neutral
   Positive value: LEA is more preferred than ADD.  */
#define IX86_LEA_PRIORITY 0

/* Return true if usage of lea INSN has performance advantage
   over a sequence of instructions.  Instructions sequence has
   SPLIT_COST cycles higher latency than lea latency.  */

static bool
ix86_lea_outperforms (rtx_insn *insn, unsigned int regno0, unsigned int regno1,
		      unsigned int regno2, int split_cost, bool has_scale)
{
  int dist_define, dist_use;

  /* For Atom processors newer than Bonnell, if using a 2-source or
     3-source LEA for non-destructive destination purposes, or due to
     wanting ability to use SCALE, the use of LEA is justified.  */
  if (!TARGET_CPU_P (BONNELL))
    {
      if (has_scale)
	return true;
      if (split_cost < 1)
	return false;
      if (regno0 == regno1 || regno0 == regno2)
	return false;
      return true;
    }

  /* Remember recog_data content.  */
  struct recog_data_d recog_data_save = recog_data;

  dist_define = distance_non_agu_define (regno1, regno2, insn);
  dist_use = distance_agu_use (regno0, insn);

  /* distance_non_agu_define can call get_attr_type which can call
     recog_memoized, restore recog_data back to previous content.  */
  recog_data = recog_data_save;

  if (dist_define < 0 || dist_define >= LEA_MAX_STALL)
    {
      /* If there is no non AGU operand definition, no AGU
	 operand usage and split cost is 0 then both lea
	 and non lea variants have same priority.  Currently
	 we prefer lea for 64 bit code and non lea on 32 bit
	 code.  */
      if (dist_use < 0 && split_cost == 0)
	return TARGET_64BIT || IX86_LEA_PRIORITY;
      else
	return true;
    }

  /* With longer definitions distance lea is more preferable.
     Here we change it to take into account splitting cost and
     lea priority.  */
  dist_define += split_cost + IX86_LEA_PRIORITY;

  /* If there is no use in memory addess then we just check
     that split cost exceeds AGU stall.  */
  if (dist_use < 0)
    return dist_define > LEA_MAX_STALL;

  /* If this insn has both backward non-agu dependence and forward
     agu dependence, the one with short distance takes effect.  */
  return dist_define >= dist_use;
}

/* Return true if we need to split op0 = op1 + op2 into a sequence of
   move and add to avoid AGU stalls.  */

bool
ix86_avoid_lea_for_add (rtx_insn *insn, rtx operands[])
{
  unsigned int regno0, regno1, regno2;

  /* Check if we need to optimize.  */
  if (!TARGET_OPT_AGU || optimize_function_for_size_p (cfun))
    return false;

  regno0 = true_regnum (operands[0]);
  regno1 = true_regnum (operands[1]);
  regno2 = true_regnum (operands[2]);

  /* We need to split only adds with non destructive
     destination operand.  */
  if (regno0 == regno1 || regno0 == regno2)
    return false;
  else
    return !ix86_lea_outperforms (insn, regno0, regno1, regno2, 1, false);
}

/* Return true if we should emit lea instruction instead of mov
   instruction.  */

bool
ix86_use_lea_for_mov (rtx_insn *insn, rtx operands[])
{
  unsigned int regno0, regno1;

  /* Check if we need to optimize.  */
  if (!TARGET_OPT_AGU || optimize_function_for_size_p (cfun))
    return false;

  /* Use lea for reg to reg moves only.  */
  if (!REG_P (operands[0]) || !REG_P (operands[1]))
    return false;

  regno0 = true_regnum (operands[0]);
  regno1 = true_regnum (operands[1]);

  return ix86_lea_outperforms (insn, regno0, regno1, INVALID_REGNUM, 0, false);
}

/* Return true if we need to split lea into a sequence of
   instructions to avoid AGU stalls during peephole2. */

bool
ix86_avoid_lea_for_addr (rtx_insn *insn, rtx operands[])
{
  unsigned int regno0, regno1, regno2;
  int split_cost;
  struct ix86_address parts;
  int ok;

  /* The "at least two components" test below might not catch simple
     move or zero extension insns if parts.base is non-NULL and parts.disp
     is const0_rtx as the only components in the address, e.g. if the
     register is %rbp or %r13.  As this test is much cheaper and moves or
     zero extensions are the common case, do this check first.  */
  if (REG_P (operands[1])
      || (SImode_address_operand (operands[1], VOIDmode)
	  && REG_P (XEXP (operands[1], 0))))
    return false;

  ok = ix86_decompose_address (operands[1], &parts);
  gcc_assert (ok);

  /* There should be at least two components in the address.  */
  if ((parts.base != NULL_RTX) + (parts.index != NULL_RTX)
      + (parts.disp != NULL_RTX) + (parts.scale > 1) < 2)
    return false;

  /* We should not split into add if non legitimate pic
     operand is used as displacement. */
  if (parts.disp && flag_pic && !LEGITIMATE_PIC_OPERAND_P (parts.disp))
    return false;

  regno0 = true_regnum (operands[0]) ;
  regno1 = INVALID_REGNUM;
  regno2 = INVALID_REGNUM;

  if (parts.base)
    regno1 = true_regnum (parts.base);
  if (parts.index)
    regno2 = true_regnum (parts.index);

  /* Use add for a = a + b and a = b + a since it is faster and shorter
     than lea for most processors.  For the processors like BONNELL, if
     the destination register of LEA holds an actual address which will
     be used soon, LEA is better and otherwise ADD is better.  */
  if (!TARGET_CPU_P (BONNELL)
      && parts.scale == 1
      && (!parts.disp || parts.disp == const0_rtx)
      && (regno0 == regno1 || regno0 == regno2))
    return true;

  /* Split with -Oz if the encoding requires fewer bytes.  */
  if (optimize_size > 1
      && parts.scale > 1
      && !parts.base
      && (!parts.disp || parts.disp == const0_rtx)) 
    return true;

  /* Check we need to optimize.  */
  if (!TARGET_AVOID_LEA_FOR_ADDR || optimize_function_for_size_p (cfun))
    return false;

  split_cost = 0;

  /* Compute how many cycles we will add to execution time
     if split lea into a sequence of instructions.  */
  if (parts.base || parts.index)
    {
      /* Have to use mov instruction if non desctructive
	 destination form is used.  */
      if (regno1 != regno0 && regno2 != regno0)
	split_cost += 1;

      /* Have to add index to base if both exist.  */
      if (parts.base && parts.index)
	split_cost += 1;

      /* Have to use shift and adds if scale is 2 or greater.  */
      if (parts.scale > 1)
	{
	  if (regno0 != regno1)
	    split_cost += 1;
	  else if (regno2 == regno0)
	    split_cost += 4;
	  else
	    split_cost += parts.scale;
	}

      /* Have to use add instruction with immediate if
	 disp is non zero.  */
      if (parts.disp && parts.disp != const0_rtx)
	split_cost += 1;

      /* Subtract the price of lea.  */
      split_cost -= 1;
    }

  return !ix86_lea_outperforms (insn, regno0, regno1, regno2, split_cost,
				parts.scale > 1);
}

/* Return true if it is ok to optimize an ADD operation to LEA
   operation to avoid flag register consumation.  For most processors,
   ADD is faster than LEA.  For the processors like BONNELL, if the
   destination register of LEA holds an actual address which will be
   used soon, LEA is better and otherwise ADD is better.  */

bool
ix86_lea_for_add_ok (rtx_insn *insn, rtx operands[])
{
  unsigned int regno0 = true_regnum (operands[0]);
  unsigned int regno1 = true_regnum (operands[1]);
  unsigned int regno2 = true_regnum (operands[2]);

  /* If a = b + c, (a!=b && a!=c), must use lea form. */
  if (regno0 != regno1 && regno0 != regno2)
    return true;

  if (!TARGET_OPT_AGU || optimize_function_for_size_p (cfun))
    return false;

  return ix86_lea_outperforms (insn, regno0, regno1, regno2, 0, false);
}

/* Return true if destination reg of SET_BODY is shift count of
   USE_BODY.  */

static bool
ix86_dep_by_shift_count_body (const_rtx set_body, const_rtx use_body)
{
  rtx set_dest;
  rtx shift_rtx;
  int i;

  /* Retrieve destination of SET_BODY.  */
  switch (GET_CODE (set_body))
    {
    case SET:
      set_dest = SET_DEST (set_body);
      if (!set_dest || !REG_P (set_dest))
	return false;
      break;
    case PARALLEL:
      for (i = XVECLEN (set_body, 0) - 1; i >= 0; i--)
	if (ix86_dep_by_shift_count_body (XVECEXP (set_body, 0, i),
					  use_body))
	  return true;
      /* FALLTHROUGH */
    default:
      return false;
    }

  /* Retrieve shift count of USE_BODY.  */
  switch (GET_CODE (use_body))
    {
    case SET:
      shift_rtx = XEXP (use_body, 1);
      break;
    case PARALLEL:
      for (i = XVECLEN (use_body, 0) - 1; i >= 0; i--)
	if (ix86_dep_by_shift_count_body (set_body,
					  XVECEXP (use_body, 0, i)))
	  return true;
      /* FALLTHROUGH */
    default:
      return false;
    }

  if (shift_rtx
      && (GET_CODE (shift_rtx) == ASHIFT
	  || GET_CODE (shift_rtx) == LSHIFTRT
	  || GET_CODE (shift_rtx) == ASHIFTRT
	  || GET_CODE (shift_rtx) == ROTATE
	  || GET_CODE (shift_rtx) == ROTATERT))
    {
      rtx shift_count = XEXP (shift_rtx, 1);

      /* Return true if shift count is dest of SET_BODY.  */
      if (REG_P (shift_count))
	{
	  /* Add check since it can be invoked before register
	     allocation in pre-reload schedule.  */
	  if (reload_completed
	      && true_regnum (set_dest) == true_regnum (shift_count))
	    return true;
	  else if (REGNO(set_dest) == REGNO(shift_count))
	    return true;
	}
    }

  return false;
}

/* Return true if destination reg of SET_INSN is shift count of
   USE_INSN.  */

bool
ix86_dep_by_shift_count (const_rtx set_insn, const_rtx use_insn)
{
  return ix86_dep_by_shift_count_body (PATTERN (set_insn),
				       PATTERN (use_insn));
}

/* Return TRUE if the operands to a vec_interleave_{high,low}v2df
   are ok, keeping in mind the possible movddup alternative.  */

bool
ix86_vec_interleave_v2df_operator_ok (rtx operands[3], bool high)
{
  if (MEM_P (operands[0]))
    return rtx_equal_p (operands[0], operands[1 + high]);
  if (MEM_P (operands[1]) && MEM_P (operands[2]))
    return false;
  return true;
}

/* A subroutine of ix86_build_signbit_mask.  If VECT is true,
   then replicate the value for all elements of the vector
   register.  */

rtx
ix86_build_const_vector (machine_mode mode, bool vect, rtx value)
{
  int i, n_elt;
  rtvec v;
  machine_mode scalar_mode;

  switch (mode)
    {
    case E_V64QImode:
    case E_V32QImode:
    case E_V16QImode:
    case E_V32HImode:
    case E_V16HImode:
    case E_V8HImode:
    case E_V16SImode:
    case E_V8SImode:
    case E_V4SImode:
    case E_V2SImode:
    case E_V8DImode:
    case E_V4DImode:
    case E_V2DImode:
      gcc_assert (vect);
      /* FALLTHRU */
    case E_V2HFmode:
    case E_V4HFmode:
    case E_V8HFmode:
    case E_V16HFmode:
    case E_V32HFmode:
    case E_V16SFmode:
    case E_V8SFmode:
    case E_V4SFmode:
    case E_V2SFmode:
    case E_V8DFmode:
    case E_V4DFmode:
    case E_V2DFmode:
      n_elt = GET_MODE_NUNITS (mode);
      v = rtvec_alloc (n_elt);
      scalar_mode = GET_MODE_INNER (mode);

      RTVEC_ELT (v, 0) = value;

      for (i = 1; i < n_elt; ++i)
	RTVEC_ELT (v, i) = vect ? value : CONST0_RTX (scalar_mode);

      return gen_rtx_CONST_VECTOR (mode, v);

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of ix86_expand_fp_absneg_operator, copysign expanders
   and ix86_expand_int_vcond.  Create a mask for the sign bit in MODE
   for an SSE register.  If VECT is true, then replicate the mask for
   all elements of the vector register.  If INVERT is true, then create
   a mask excluding the sign bit.  */

rtx
ix86_build_signbit_mask (machine_mode mode, bool vect, bool invert)
{
  machine_mode vec_mode, imode;
  wide_int w;
  rtx mask, v;

  switch (mode)
    {
    case E_V2HFmode:
    case E_V4HFmode:
    case E_V8HFmode:
    case E_V16HFmode:
    case E_V32HFmode:
      vec_mode = mode;
      imode = HImode;
      break;

    case E_V16SImode:
    case E_V16SFmode:
    case E_V8SImode:
    case E_V4SImode:
    case E_V8SFmode:
    case E_V4SFmode:
    case E_V2SFmode:
    case E_V2SImode:
      vec_mode = mode;
      imode = SImode;
      break;

    case E_V8DImode:
    case E_V4DImode:
    case E_V2DImode:
    case E_V8DFmode:
    case E_V4DFmode:
    case E_V2DFmode:
      vec_mode = mode;
      imode = DImode;
      break;

    case E_TImode:
    case E_TFmode:
      vec_mode = VOIDmode;
      imode = TImode;
      break;

    default:
      gcc_unreachable ();
    }

  machine_mode inner_mode = GET_MODE_INNER (mode);
  w = wi::set_bit_in_zero (GET_MODE_BITSIZE (inner_mode) - 1,
			   GET_MODE_BITSIZE (inner_mode));
  if (invert)
    w = wi::bit_not (w);

  /* Force this value into the low part of a fp vector constant.  */
  mask = immed_wide_int_const (w, imode);
  mask = gen_lowpart (inner_mode, mask);

  if (vec_mode == VOIDmode)
    return force_reg (inner_mode, mask);

  v = ix86_build_const_vector (vec_mode, vect, mask);
  return force_reg (vec_mode, v);
}

/* Return HOST_WIDE_INT for const vector OP in MODE.  */

HOST_WIDE_INT
ix86_convert_const_vector_to_integer (rtx op, machine_mode mode)
{
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    gcc_unreachable ();

  int nunits = GET_MODE_NUNITS (mode);
  wide_int val = wi::zero (GET_MODE_BITSIZE (mode));
  machine_mode innermode = GET_MODE_INNER (mode);
  unsigned int innermode_bits = GET_MODE_BITSIZE (innermode);

  switch (mode)
    {
    case E_V2QImode:
    case E_V4QImode:
    case E_V2HImode:
    case E_V8QImode:
    case E_V4HImode:
    case E_V2SImode:
      for (int i = 0; i < nunits; ++i)
	{
	  int v = INTVAL (XVECEXP (op, 0, i));
	  wide_int wv = wi::shwi (v, innermode_bits);
	  val = wi::insert (val, wv, innermode_bits * i, innermode_bits);
	}
      break;
    case E_V2HFmode:
    case E_V2BFmode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2SFmode:
      for (int i = 0; i < nunits; ++i)
	{
	  rtx x = XVECEXP (op, 0, i);
	  int v = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (x),
				  REAL_MODE_FORMAT (innermode));
	  wide_int wv = wi::shwi (v, innermode_bits);
	  val = wi::insert (val, wv, innermode_bits * i, innermode_bits);
	}
      break;
    default:
      gcc_unreachable ();
    }

  return val.to_shwi ();
}

/* Return TRUE or FALSE depending on whether the first SET in INSN
   has source and destination with matching CC modes, and that the
   CC mode is at least as constrained as REQ_MODE.  */

bool
ix86_match_ccmode (rtx insn, machine_mode req_mode)
{
  rtx set;
  machine_mode set_mode;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  gcc_assert (GET_CODE (SET_SRC (set)) == COMPARE);

  set_mode = GET_MODE (SET_DEST (set));
  switch (set_mode)
    {
    case E_CCNOmode:
      if (req_mode != CCNOmode
	  && (req_mode != CCmode
	      || XEXP (SET_SRC (set), 1) != const0_rtx))
	return false;
      break;
    case E_CCmode:
      if (req_mode == CCGCmode)
	return false;
      /* FALLTHRU */
    case E_CCGCmode:
      if (req_mode == CCGOCmode || req_mode == CCNOmode)
	return false;
      /* FALLTHRU */
    case E_CCGOCmode:
      if (req_mode == CCZmode)
	return false;
      /* FALLTHRU */
    case E_CCZmode:
      break;

    case E_CCGZmode:

    case E_CCAmode:
    case E_CCCmode:
    case E_CCOmode:
    case E_CCPmode:
    case E_CCSmode:
      if (set_mode != req_mode)
	return false;
      break;

    default:
      gcc_unreachable ();
    }

  return GET_MODE (SET_SRC (set)) == set_mode;
}

machine_mode
ix86_cc_mode (enum rtx_code code, rtx op0, rtx op1)
{
  machine_mode mode = GET_MODE (op0);

  if (SCALAR_FLOAT_MODE_P (mode))
    {
      gcc_assert (!DECIMAL_FLOAT_MODE_P (mode));
      return CCFPmode;
    }

  switch (code)
    {
      /* Only zero flag is needed.  */
    case EQ:			/* ZF=0 */
    case NE:			/* ZF!=0 */
      return CCZmode;
      /* Codes needing carry flag.  */
    case GEU:			/* CF=0 */
    case LTU:			/* CF=1 */
      rtx geu;
      /* Detect overflow checks.  They need just the carry flag.  */
      if (GET_CODE (op0) == PLUS
	  && (rtx_equal_p (op1, XEXP (op0, 0))
	      || rtx_equal_p (op1, XEXP (op0, 1))))
	return CCCmode;
      /* Similarly for *setcc_qi_addqi3_cconly_overflow_1_* patterns.
	 Match LTU of op0
	 (neg:QI (geu:QI (reg:CC_CCC FLAGS_REG) (const_int 0)))
	 and op1
	 (ltu:QI (reg:CC_CCC FLAGS_REG) (const_int 0))
	 where CC_CCC is either CC or CCC.  */
      else if (code == LTU
	       && GET_CODE (op0) == NEG
	       && GET_CODE (geu = XEXP (op0, 0)) == GEU
	       && REG_P (XEXP (geu, 0))
	       && (GET_MODE (XEXP (geu, 0)) == CCCmode
		   || GET_MODE (XEXP (geu, 0)) == CCmode)
	       && REGNO (XEXP (geu, 0)) == FLAGS_REG
	       && XEXP (geu, 1) == const0_rtx
	       && GET_CODE (op1) == LTU
	       && REG_P (XEXP (op1, 0))
	       && GET_MODE (XEXP (op1, 0)) == GET_MODE (XEXP (geu, 0))
	       && REGNO (XEXP (op1, 0)) == FLAGS_REG
	       && XEXP (op1, 1) == const0_rtx)
	return CCCmode;
      /* Similarly for *x86_cmc pattern.
	 Match LTU of op0 (neg:QI (ltu:QI (reg:CCC FLAGS_REG) (const_int 0)))
	 and op1 (geu:QI (reg:CCC FLAGS_REG) (const_int 0)).
	 It is sufficient to test that the operand modes are CCCmode.  */
      else if (code == LTU
	       && GET_CODE (op0) == NEG
	       && GET_CODE (XEXP (op0, 0)) == LTU
	       && GET_MODE (XEXP (XEXP (op0, 0), 0)) == CCCmode
	       && GET_CODE (op1) == GEU
	       && GET_MODE (XEXP (op1, 0)) == CCCmode)
	return CCCmode;
      else
	return CCmode;
    case GTU:			/* CF=0 & ZF=0 */
    case LEU:			/* CF=1 | ZF=1 */
      return CCmode;
      /* Codes possibly doable only with sign flag when
         comparing against zero.  */
    case GE:			/* SF=OF   or   SF=0 */
    case LT:			/* SF<>OF  or   SF=1 */
      if (op1 == const0_rtx)
	return CCGOCmode;
      else
	/* For other cases Carry flag is not required.  */
	return CCGCmode;
      /* Codes doable only with sign flag when comparing
         against zero, but we miss jump instruction for it
         so we need to use relational tests against overflow
         that thus needs to be zero.  */
    case GT:			/* ZF=0 & SF=OF */
    case LE:			/* ZF=1 | SF<>OF */
      if (op1 == const0_rtx)
	return CCNOmode;
      else
	return CCGCmode;
    default:
      /* CCmode should be used in all other cases.  */
      return CCmode;
    }
}

/* Return TRUE or FALSE depending on whether the ptest instruction
   INSN has source and destination with suitable matching CC modes.  */

bool
ix86_match_ptest_ccmode (rtx insn)
{
  rtx set, src;
  machine_mode set_mode;

  set = PATTERN (insn);
  gcc_assert (GET_CODE (set) == SET);
  src = SET_SRC (set);
  gcc_assert (GET_CODE (src) == UNSPEC
	      && XINT (src, 1) == UNSPEC_PTEST);

  set_mode = GET_MODE (src);
  if (set_mode != CCZmode
      && set_mode != CCCmode
      && set_mode != CCmode)
    return false;
  return GET_MODE (SET_DEST (set)) == set_mode;
}

/* Return the fixed registers used for condition codes.  */

static bool
ix86_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = FLAGS_REG;
  *p2 = INVALID_REGNUM;
  return true;
}

/* If two condition code modes are compatible, return a condition code
   mode which is compatible with both.  Otherwise, return
   VOIDmode.  */

static machine_mode
ix86_cc_modes_compatible (machine_mode m1, machine_mode m2)
{
  if (m1 == m2)
    return m1;

  if (GET_MODE_CLASS (m1) != MODE_CC || GET_MODE_CLASS (m2) != MODE_CC)
    return VOIDmode;

  if ((m1 == CCGCmode && m2 == CCGOCmode)
      || (m1 == CCGOCmode && m2 == CCGCmode))
    return CCGCmode;

  if ((m1 == CCNOmode && m2 == CCGOCmode)
      || (m1 == CCGOCmode && m2 == CCNOmode))
    return CCNOmode;

  if (m1 == CCZmode
      && (m2 == CCGCmode || m2 == CCGOCmode || m2 == CCNOmode))
    return m2;
  else if (m2 == CCZmode
	   && (m1 == CCGCmode || m1 == CCGOCmode || m1 == CCNOmode))
    return m1;

  switch (m1)
    {
    default:
      gcc_unreachable ();

    case E_CCmode:
    case E_CCGCmode:
    case E_CCGOCmode:
    case E_CCNOmode:
    case E_CCAmode:
    case E_CCCmode:
    case E_CCOmode:
    case E_CCPmode:
    case E_CCSmode:
    case E_CCZmode:
      switch (m2)
	{
	default:
	  return VOIDmode;

	case E_CCmode:
	case E_CCGCmode:
	case E_CCGOCmode:
	case E_CCNOmode:
	case E_CCAmode:
	case E_CCCmode:
	case E_CCOmode:
	case E_CCPmode:
	case E_CCSmode:
	case E_CCZmode:
	  return CCmode;
	}

    case E_CCFPmode:
      /* These are only compatible with themselves, which we already
	 checked above.  */
      return VOIDmode;
    }
}

/* Return strategy to use for floating-point.  We assume that fcomi is always
   preferrable where available, since that is also true when looking at size
   (2 bytes, vs. 3 for fnstsw+sahf and at least 5 for fnstsw+test).  */

enum ix86_fpcmp_strategy
ix86_fp_comparison_strategy (enum rtx_code)
{
  /* Do fcomi/sahf based test when profitable.  */

  if (TARGET_CMOVE)
    return IX86_FPCMP_COMI;

  if (TARGET_SAHF && (TARGET_USE_SAHF || optimize_insn_for_size_p ()))
    return IX86_FPCMP_SAHF;

  return IX86_FPCMP_ARITH;
}

/* Convert comparison codes we use to represent FP comparison to integer
   code that will result in proper branch.  Return UNKNOWN if no such code
   is available.  */

enum rtx_code
ix86_fp_compare_code_to_integer (enum rtx_code code)
{
  switch (code)
    {
    case GT:
      return GTU;
    case GE:
      return GEU;
    case ORDERED:
    case UNORDERED:
      return code;
    case UNEQ:
      return EQ;
    case UNLT:
      return LTU;
    case UNLE:
      return LEU;
    case LTGT:
      return NE;
    default:
      return UNKNOWN;
    }
}

/* Zero extend possibly SImode EXP to Pmode register.  */
rtx
ix86_zero_extend_to_Pmode (rtx exp)
{
  return force_reg (Pmode, convert_to_mode (Pmode, exp, 1));
}

/* Return true if the function is called via PLT.   */

bool
ix86_call_use_plt_p (rtx call_op)
{
  if (SYMBOL_REF_LOCAL_P (call_op))
    {
      if (SYMBOL_REF_DECL (call_op)
	  && TREE_CODE (SYMBOL_REF_DECL (call_op)) == FUNCTION_DECL)
	{
	  /* NB: All ifunc functions must be called via PLT.  */
	  cgraph_node *node
	    = cgraph_node::get (SYMBOL_REF_DECL (call_op));
	  if (node && node->ifunc_resolver)
	    return true;
	}
      return false;
    }
  return true;
}

/* Implement TARGET_IFUNC_REF_LOCAL_OK.  If this hook returns true,
   the PLT entry will be used as the function address for local IFUNC
   functions.  When the PIC register is needed for PLT call, indirect
   call via the PLT entry will fail since the PIC register may not be
   set up properly for indirect call.  In this case, we should return
   false.  */

static bool
ix86_ifunc_ref_local_ok (void)
{
  return !flag_pic || (TARGET_64BIT && ix86_cmodel != CM_LARGE_PIC);
}

/* Return true if the function being called was marked with attribute
   "noplt" or using -fno-plt and we are compiling for non-PIC.  We need
   to handle the non-PIC case in the backend because there is no easy
   interface for the front-end to force non-PLT calls to use the GOT.
   This is currently used only with 64-bit or 32-bit GOT32X ELF targets
   to call the function marked "noplt" indirectly.  */

static bool
ix86_nopic_noplt_attribute_p (rtx call_op)
{
  if (flag_pic || ix86_cmodel == CM_LARGE
      || !(TARGET_64BIT || HAVE_AS_IX86_GOT32X)
      || TARGET_MACHO || TARGET_SEH || TARGET_PECOFF
      || SYMBOL_REF_LOCAL_P (call_op))
    return false;

  tree symbol_decl = SYMBOL_REF_DECL (call_op);

  if (!flag_plt
      || (symbol_decl != NULL_TREE
          && lookup_attribute ("noplt", DECL_ATTRIBUTES (symbol_decl))))
    return true;

  return false;
}

/* Helper to output the jmp/call.  */
static void
ix86_output_jmp_thunk_or_indirect (const char *thunk_name, const int regno)
{
  if (thunk_name != NULL)
    {
      if ((REX_INT_REGNO_P (regno) || REX2_INT_REGNO_P (regno))
	  && ix86_indirect_branch_cs_prefix)
	fprintf (asm_out_file, "\tcs\n");
      fprintf (asm_out_file, "\tjmp\t");
      assemble_name (asm_out_file, thunk_name);
      putc ('\n', asm_out_file);
      if ((ix86_harden_sls & harden_sls_indirect_jmp))
	fputs ("\tint3\n", asm_out_file);
    }
  else
    output_indirect_thunk (regno);
}

/* Output indirect branch via a call and return thunk.  CALL_OP is a
   register which contains the branch target.  XASM is the assembly
   template for CALL_OP.  Branch is a tail call if SIBCALL_P is true.
   A normal call is converted to:

	call __x86_indirect_thunk_reg

   and a tail call is converted to:

	jmp __x86_indirect_thunk_reg
 */

static void
ix86_output_indirect_branch_via_reg (rtx call_op, bool sibcall_p)
{
  char thunk_name_buf[32];
  char *thunk_name;
  enum indirect_thunk_prefix need_prefix
    = indirect_thunk_need_prefix (current_output_insn);
  int regno = REGNO (call_op);

  if (cfun->machine->indirect_branch_type
      != indirect_branch_thunk_inline)
    {
      if (cfun->machine->indirect_branch_type == indirect_branch_thunk)
	SET_HARD_REG_BIT (indirect_thunks_used, regno);

      indirect_thunk_name (thunk_name_buf, regno, need_prefix, false);
      thunk_name = thunk_name_buf;
    }
  else
    thunk_name = NULL;

  if (sibcall_p)
     ix86_output_jmp_thunk_or_indirect (thunk_name, regno);
  else
    {
      if (thunk_name != NULL)
	{
	  if ((REX_INT_REGNO_P (regno) || REX_INT_REGNO_P (regno))
	      && ix86_indirect_branch_cs_prefix)
	    fprintf (asm_out_file, "\tcs\n");
	  fprintf (asm_out_file, "\tcall\t");
	  assemble_name (asm_out_file, thunk_name);
	  putc ('\n', asm_out_file);
	  return;
	}

      char indirectlabel1[32];
      char indirectlabel2[32];

      ASM_GENERATE_INTERNAL_LABEL (indirectlabel1,
				   INDIRECT_LABEL,
				   indirectlabelno++);
      ASM_GENERATE_INTERNAL_LABEL (indirectlabel2,
				   INDIRECT_LABEL,
				   indirectlabelno++);

      /* Jump.  */
      fputs ("\tjmp\t", asm_out_file);
      assemble_name_raw (asm_out_file, indirectlabel2);
      fputc ('\n', asm_out_file);

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel1);

     ix86_output_jmp_thunk_or_indirect (thunk_name, regno);

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel2);

      /* Call.  */
      fputs ("\tcall\t", asm_out_file);
      assemble_name_raw (asm_out_file, indirectlabel1);
      fputc ('\n', asm_out_file);
    }
}

/* Output indirect branch via a call and return thunk.  CALL_OP is
   the branch target.  XASM is the assembly template for CALL_OP.
   Branch is a tail call if SIBCALL_P is true.  A normal call is
   converted to:

	jmp L2
   L1:
	push CALL_OP
	jmp __x86_indirect_thunk
   L2:
	call L1

   and a tail call is converted to:

	push CALL_OP
	jmp __x86_indirect_thunk
 */

static void
ix86_output_indirect_branch_via_push (rtx call_op, const char *xasm,
				      bool sibcall_p)
{
  char thunk_name_buf[32];
  char *thunk_name;
  char push_buf[64];
  enum indirect_thunk_prefix need_prefix
    = indirect_thunk_need_prefix (current_output_insn);
  int regno = -1;

  if (cfun->machine->indirect_branch_type
      != indirect_branch_thunk_inline)
    {
      if (cfun->machine->indirect_branch_type == indirect_branch_thunk)
	indirect_thunk_needed = true;
      indirect_thunk_name (thunk_name_buf, regno, need_prefix, false);
      thunk_name = thunk_name_buf;
    }
  else
    thunk_name = NULL;

  snprintf (push_buf, sizeof (push_buf), "push{%c}\t%s",
	    TARGET_64BIT ? 'q' : 'l', xasm);

  if (sibcall_p)
    {
      output_asm_insn (push_buf, &call_op);
      ix86_output_jmp_thunk_or_indirect (thunk_name, regno);
    }
  else
    {
      char indirectlabel1[32];
      char indirectlabel2[32];

      ASM_GENERATE_INTERNAL_LABEL (indirectlabel1,
				   INDIRECT_LABEL,
				   indirectlabelno++);
      ASM_GENERATE_INTERNAL_LABEL (indirectlabel2,
				   INDIRECT_LABEL,
				   indirectlabelno++);

      /* Jump.  */
      fputs ("\tjmp\t", asm_out_file);
      assemble_name_raw (asm_out_file, indirectlabel2);
      fputc ('\n', asm_out_file);

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel1);

      /* An external function may be called via GOT, instead of PLT.  */
      if (MEM_P (call_op))
	{
	  struct ix86_address parts;
	  rtx addr = XEXP (call_op, 0);
	  if (ix86_decompose_address (addr, &parts)
	      && parts.base == stack_pointer_rtx)
	    {
	      /* Since call will adjust stack by -UNITS_PER_WORD,
		 we must convert "disp(stack, index, scale)" to
		 "disp+UNITS_PER_WORD(stack, index, scale)".  */
	      if (parts.index)
		{
		  addr = gen_rtx_MULT (Pmode, parts.index,
				       GEN_INT (parts.scale));
		  addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				       addr);
		}
	      else
		addr = stack_pointer_rtx;

	      rtx disp;
	      if (parts.disp != NULL_RTX)
		disp = plus_constant (Pmode, parts.disp,
				      UNITS_PER_WORD);
	      else
		disp = GEN_INT (UNITS_PER_WORD);

	      addr = gen_rtx_PLUS (Pmode, addr, disp);
	      call_op = gen_rtx_MEM (GET_MODE (call_op), addr);
	    }
	}

      output_asm_insn (push_buf, &call_op);

      ix86_output_jmp_thunk_or_indirect (thunk_name, regno);

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, indirectlabel2);

      /* Call.  */
      fputs ("\tcall\t", asm_out_file);
      assemble_name_raw (asm_out_file, indirectlabel1);
      fputc ('\n', asm_out_file);
    }
}

/* Output indirect branch via a call and return thunk.  CALL_OP is
   the branch target.  XASM is the assembly template for CALL_OP.
   Branch is a tail call if SIBCALL_P is true.   */

static void
ix86_output_indirect_branch (rtx call_op, const char *xasm,
			     bool sibcall_p)
{
  if (REG_P (call_op))
    ix86_output_indirect_branch_via_reg (call_op, sibcall_p);
  else
    ix86_output_indirect_branch_via_push (call_op, xasm, sibcall_p);
}

/* Output indirect jump.  CALL_OP is the jump target.  */

const char *
ix86_output_indirect_jmp (rtx call_op)
{
  if (cfun->machine->indirect_branch_type != indirect_branch_keep)
    {
      /* We can't have red-zone since "call" in the indirect thunk
         pushes the return address onto stack, destroying red-zone.  */
      if (ix86_red_zone_used)
	gcc_unreachable ();

      ix86_output_indirect_branch (call_op, "%0", true);
    }
  else
    output_asm_insn ("%!jmp\t%A0", &call_op);
  return (ix86_harden_sls & harden_sls_indirect_jmp) ? "int3" : "";
}

/* Output return instrumentation for current function if needed.  */

static void
output_return_instrumentation (void)
{
  if (ix86_instrument_return != instrument_return_none
      && flag_fentry
      && !DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (cfun->decl))
    {
      if (ix86_flag_record_return)
	fprintf (asm_out_file, "1:\n");
      switch (ix86_instrument_return)
	{
	case instrument_return_call:
	  fprintf (asm_out_file, "\tcall\t__return__\n");
	  break;
	case instrument_return_nop5:
	  /* 5 byte nop: nopl 0(%[re]ax,%[re]ax,1)  */
	  fprintf (asm_out_file, ASM_BYTE "0x0f, 0x1f, 0x44, 0x00, 0x00\n");
	  break;
	case instrument_return_none:
	  break;
	}

      if (ix86_flag_record_return)
	{
	  fprintf (asm_out_file, "\t.section __return_loc, \"a\",@progbits\n");
	  fprintf (asm_out_file, "\t.%s 1b\n", TARGET_64BIT ? "quad" : "long");
	  fprintf (asm_out_file, "\t.previous\n");
	}
    }
}

/* Output function return.  CALL_OP is the jump target.  Add a REP
   prefix to RET if LONG_P is true and function return is kept.  */

const char *
ix86_output_function_return (bool long_p)
{
  output_return_instrumentation ();

  if (cfun->machine->function_return_type != indirect_branch_keep)
    {
      char thunk_name[32];
      enum indirect_thunk_prefix need_prefix
	= indirect_thunk_need_prefix (current_output_insn);

      if (cfun->machine->function_return_type
	  != indirect_branch_thunk_inline)
	{
	  bool need_thunk = (cfun->machine->function_return_type
			     == indirect_branch_thunk);
	  indirect_thunk_name (thunk_name, INVALID_REGNUM, need_prefix,
			       true);
	  indirect_return_needed |= need_thunk;
	  fprintf (asm_out_file, "\tjmp\t");
	  assemble_name (asm_out_file, thunk_name);
	  putc ('\n', asm_out_file);
	}
      else
	output_indirect_thunk (INVALID_REGNUM);

      return "";
    }

  output_asm_insn (long_p ? "rep%; ret" : "ret", nullptr);
  return (ix86_harden_sls & harden_sls_return) ? "int3" : "";
}

/* Output indirect function return.  RET_OP is the function return
   target.  */

const char *
ix86_output_indirect_function_return (rtx ret_op)
{
  if (cfun->machine->function_return_type != indirect_branch_keep)
    {
      char thunk_name[32];
      enum indirect_thunk_prefix need_prefix
	= indirect_thunk_need_prefix (current_output_insn);
      unsigned int regno = REGNO (ret_op);
      gcc_assert (regno == CX_REG);

      if (cfun->machine->function_return_type
	  != indirect_branch_thunk_inline)
	{
	  bool need_thunk = (cfun->machine->function_return_type
			     == indirect_branch_thunk);
	  indirect_thunk_name (thunk_name, regno, need_prefix, true);

	  if (need_thunk)
	    {
	      indirect_return_via_cx = true;
	      SET_HARD_REG_BIT (indirect_thunks_used, CX_REG);
	    }
	  fprintf (asm_out_file, "\tjmp\t");
	  assemble_name (asm_out_file, thunk_name);
	  putc ('\n', asm_out_file);
	}
      else
	output_indirect_thunk (regno);
    }
  else
    {
      output_asm_insn ("%!jmp\t%A0", &ret_op);
      if (ix86_harden_sls & harden_sls_indirect_jmp)
	fputs ("\tint3\n", asm_out_file);
    }
  return "";
}

/* Output the assembly for a call instruction.  */

const char *
ix86_output_call_insn (rtx_insn *insn, rtx call_op)
{
  bool direct_p = constant_call_address_operand (call_op, VOIDmode);
  bool output_indirect_p
    = (!TARGET_SEH
       && cfun->machine->indirect_branch_type != indirect_branch_keep);
  bool seh_nop_p = false;
  const char *xasm;

  if (SIBLING_CALL_P (insn))
    {
      output_return_instrumentation ();
      if (direct_p)
	{
	  if (ix86_nopic_noplt_attribute_p (call_op))
	    {
	      direct_p = false;
	      if (TARGET_64BIT)
		{
		  if (output_indirect_p)
		    xasm = "{%p0@GOTPCREL(%%rip)|[QWORD PTR %p0@GOTPCREL[rip]]}";
		  else
		    xasm = "%!jmp\t{*%p0@GOTPCREL(%%rip)|[QWORD PTR %p0@GOTPCREL[rip]]}";
		}
	      else
		{
		  if (output_indirect_p)
		    xasm = "{%p0@GOT|[DWORD PTR %p0@GOT]}";
		  else
		    xasm = "%!jmp\t{*%p0@GOT|[DWORD PTR %p0@GOT]}";
		}
	    }
	  else
	    xasm = "%!jmp\t%P0";
	}
      /* SEH epilogue detection requires the indirect branch case
	 to include REX.W.  */
      else if (TARGET_SEH)
	xasm = "%!rex.W jmp\t%A0";
      else
	{
	  if (output_indirect_p)
	    xasm = "%0";
	  else
	    xasm = "%!jmp\t%A0";
	}

      if (output_indirect_p && !direct_p)
	ix86_output_indirect_branch (call_op, xasm, true);
      else
	{
	  output_asm_insn (xasm, &call_op);
	  if (!direct_p
	      && (ix86_harden_sls & harden_sls_indirect_jmp))
	    return "int3";
	}
      return "";
    }

  /* SEH unwinding can require an extra nop to be emitted in several
     circumstances.  Determine if we have one of those.  */
  if (TARGET_SEH)
    {
      rtx_insn *i;

      for (i = NEXT_INSN (insn); i ; i = NEXT_INSN (i))
	{
	  /* Prevent a catch region from being adjacent to a jump that would
	     be interpreted as an epilogue sequence by the unwinder.  */
	  if (JUMP_P(i) && CROSSING_JUMP_P (i))
	    {
	      seh_nop_p = true;
	      break;
	    }
	    
	  /* If we get to another real insn, we don't need the nop.  */
	  if (INSN_P (i))
	    break;

	  /* If we get to the epilogue note, prevent a catch region from
	     being adjacent to the standard epilogue sequence.  Note that,
	     if non-call exceptions are enabled, we already did it during
	     epilogue expansion, or else, if the insn can throw internally,
	     we already did it during the reorg pass.  */
	  if (NOTE_P (i) && NOTE_KIND (i) == NOTE_INSN_EPILOGUE_BEG
	      && !flag_non_call_exceptions
	      && !can_throw_internal (insn))
	    {
	      seh_nop_p = true;
	      break;
	    }
	}

      /* If we didn't find a real insn following the call, prevent the
	 unwinder from looking into the next function.  */
      if (i == NULL)
	seh_nop_p = true;
    }

  if (direct_p)
    {
      if (ix86_nopic_noplt_attribute_p (call_op))
	{
	  direct_p = false;
	  if (TARGET_64BIT)
	    {
	      if (output_indirect_p)
		xasm = "{%p0@GOTPCREL(%%rip)|[QWORD PTR %p0@GOTPCREL[rip]]}";
	      else
		xasm = "%!call\t{*%p0@GOTPCREL(%%rip)|[QWORD PTR %p0@GOTPCREL[rip]]}";
	    }
	  else
	    {
	      if (output_indirect_p)
		xasm = "{%p0@GOT|[DWORD PTR %p0@GOT]}";
	      else
		xasm = "%!call\t{*%p0@GOT|[DWORD PTR %p0@GOT]}";
	    }
	}
      else
	xasm = "%!call\t%P0";
    }
  else
    {
      if (output_indirect_p)
	xasm = "%0";
      else
	xasm = "%!call\t%A0";
    }

  if (output_indirect_p && !direct_p)
    ix86_output_indirect_branch (call_op, xasm, false);
  else
    output_asm_insn (xasm, &call_op);

  if (seh_nop_p)
    return "nop";

  return "";
}

/* Return a MEM corresponding to a stack slot with mode MODE.
   Allocate a new slot if necessary.

   The RTL for a function can have several slots available: N is
   which slot to use.  */

rtx
assign_386_stack_local (machine_mode mode, enum ix86_stack_slot n)
{
  struct stack_local_entry *s;

  gcc_assert (n < MAX_386_STACK_LOCALS);

  for (s = ix86_stack_locals; s; s = s->next)
    if (s->mode == mode && s->n == n)
      return validize_mem (copy_rtx (s->rtl));

  int align = 0;
  /* For DImode with SLOT_FLOATxFDI_387 use 32-bit
     alignment with -m32 -mpreferred-stack-boundary=2.  */
  if (mode == DImode
      && !TARGET_64BIT
      && n == SLOT_FLOATxFDI_387
      && ix86_preferred_stack_boundary < GET_MODE_ALIGNMENT (DImode))
    align = 32;
  s = ggc_alloc<stack_local_entry> ();
  s->n = n;
  s->mode = mode;
  s->rtl = assign_stack_local (mode, GET_MODE_SIZE (mode), align);

  s->next = ix86_stack_locals;
  ix86_stack_locals = s;
  return validize_mem (copy_rtx (s->rtl));
}

static void
ix86_instantiate_decls (void)
{
  struct stack_local_entry *s;

  for (s = ix86_stack_locals; s; s = s->next)
    if (s->rtl != NULL_RTX)
      instantiate_decl_rtl (s->rtl);
}

/* Check whether x86 address PARTS is a pc-relative address.  */

bool
ix86_rip_relative_addr_p (struct ix86_address *parts)
{
  rtx base, index, disp;

  base = parts->base;
  index = parts->index;
  disp = parts->disp;

  if (disp && !base && !index)
    {
      if (TARGET_64BIT)
	{
	  rtx symbol = disp;

	  if (GET_CODE (disp) == CONST)
	    symbol = XEXP (disp, 0);
	  if (GET_CODE (symbol) == PLUS
	      && CONST_INT_P (XEXP (symbol, 1)))
	    symbol = XEXP (symbol, 0);

	  if (GET_CODE (symbol) == LABEL_REF
	      || (GET_CODE (symbol) == SYMBOL_REF
		  && SYMBOL_REF_TLS_MODEL (symbol) == 0)
	      || (GET_CODE (symbol) == UNSPEC
		  && (XINT (symbol, 1) == UNSPEC_GOTPCREL
		      || XINT (symbol, 1) == UNSPEC_PCREL
		      || XINT (symbol, 1) == UNSPEC_GOTNTPOFF)))
	    return true;
	}
    }
  return false;
}

/* Calculate the length of the memory address in the instruction encoding.
   Includes addr32 prefix, does not include the one-byte modrm, opcode,
   or other prefixes.  We never generate addr32 prefix for LEA insn.  */

int
memory_address_length (rtx addr, bool lea)
{
  struct ix86_address parts;
  rtx base, index, disp;
  int len;
  int ok;

  if (GET_CODE (addr) == PRE_DEC
      || GET_CODE (addr) == POST_INC
      || GET_CODE (addr) == PRE_MODIFY
      || GET_CODE (addr) == POST_MODIFY)
    return 0;

  ok = ix86_decompose_address (addr, &parts);
  gcc_assert (ok);

  len = (parts.seg == ADDR_SPACE_GENERIC) ? 0 : 1;

  /*  If this is not LEA instruction, add the length of addr32 prefix.  */
  if (TARGET_64BIT && !lea
      && (SImode_address_operand (addr, VOIDmode)
	  || (parts.base && GET_MODE (parts.base) == SImode)
	  || (parts.index && GET_MODE (parts.index) == SImode)))
    len++;

  base = parts.base;
  index = parts.index;
  disp = parts.disp;

  if (base && SUBREG_P (base))
    base = SUBREG_REG (base);
  if (index && SUBREG_P (index))
    index = SUBREG_REG (index);

  gcc_assert (base == NULL_RTX || REG_P (base));
  gcc_assert (index == NULL_RTX || REG_P (index));

  /* Rule of thumb:
       - esp as the base always wants an index,
       - ebp as the base always wants a displacement,
       - r12 as the base always wants an index,
       - r13 as the base always wants a displacement.  */

  /* Register Indirect.  */
  if (base && !index && !disp)
    {
      /* esp (for its index) and ebp (for its displacement) need
	 the two-byte modrm form.  Similarly for r12 and r13 in 64-bit
	 code.  */
      if (base == arg_pointer_rtx
	  || base == frame_pointer_rtx
	  || REGNO (base) == SP_REG
	  || REGNO (base) == BP_REG
	  || REGNO (base) == R12_REG
	  || REGNO (base) == R13_REG)
	len++;
    }

  /* Direct Addressing.  In 64-bit mode mod 00 r/m 5
     is not disp32, but disp32(%rip), so for disp32
     SIB byte is needed, unless print_operand_address
     optimizes it into disp32(%rip) or (%rip) is implied
     by UNSPEC.  */
  else if (disp && !base && !index)
    {
      len += 4;
      if (!ix86_rip_relative_addr_p (&parts))
	len++;
    }
  else
    {
      /* Find the length of the displacement constant.  */
      if (disp)
	{
	  if (base && satisfies_constraint_K (disp))
	    len += 1;
	  else
	    len += 4;
	}
      /* ebp always wants a displacement.  Similarly r13.  */
      else if (base && (REGNO (base) == BP_REG || REGNO (base) == R13_REG))
	len++;

      /* An index requires the two-byte modrm form....  */
      if (index
	  /* ...like esp (or r12), which always wants an index.  */
	  || base == arg_pointer_rtx
	  || base == frame_pointer_rtx
	  || (base && (REGNO (base) == SP_REG || REGNO (base) == R12_REG)))
	len++;
    }

  return len;
}

/* Compute default value for "length_immediate" attribute.  When SHORTFORM
   is set, expect that insn have 8bit immediate alternative.  */
int
ix86_attr_length_immediate_default (rtx_insn *insn, bool shortform)
{
  int len = 0;
  int i;
  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (CONSTANT_P (recog_data.operand[i]))
      {
        enum attr_mode mode = get_attr_mode (insn);

	gcc_assert (!len);
	if (shortform && CONST_INT_P (recog_data.operand[i]))
	  {
	    HOST_WIDE_INT ival = INTVAL (recog_data.operand[i]);
	    switch (mode)
	      {
	      case MODE_QI:
		len = 1;
		continue;
	      case MODE_HI:
		ival = trunc_int_for_mode (ival, HImode);
		break;
	      case MODE_SI:
		ival = trunc_int_for_mode (ival, SImode);
		break;
	      default:
		break;
	      }
	    if (IN_RANGE (ival, -128, 127))
	      {
		len = 1;
		continue;
	      }
	  }
	switch (mode)
	  {
	  case MODE_QI:
	    len = 1;
	    break;
	  case MODE_HI:
	    len = 2;
	    break;
	  case MODE_SI:
	    len = 4;
	    break;
	  /* Immediates for DImode instructions are encoded
	     as 32bit sign extended values.  */
	  case MODE_DI:
	    len = 4;
	    break;
	  default:
	    fatal_insn ("unknown insn mode", insn);
	}
      }
  return len;
}

/* Compute default value for "length_address" attribute.  */
int
ix86_attr_length_address_default (rtx_insn *insn)
{
  int i;

  if (get_attr_type (insn) == TYPE_LEA)
    {
      rtx set = PATTERN (insn), addr;

      if (GET_CODE (set) == PARALLEL)
	set = XVECEXP (set, 0, 0);

      gcc_assert (GET_CODE (set) == SET);

      addr = SET_SRC (set);

      return memory_address_length (addr, true);
    }

  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    {
      rtx op = recog_data.operand[i];
      if (MEM_P (op))
	{
	  constrain_operands_cached (insn, reload_completed);
	  if (which_alternative != -1)
	    {
	      const char *constraints = recog_data.constraints[i];
	      int alt = which_alternative;

	      while (*constraints == '=' || *constraints == '+')
		constraints++;
	      while (alt-- > 0)
	        while (*constraints++ != ',')
		  ;
	      /* Skip ignored operands.  */
	      if (*constraints == 'X')
		continue;
	    }

	  int len = memory_address_length (XEXP (op, 0), false);

	  /* Account for segment prefix for non-default addr spaces.  */
	  if (!ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (op)))
	    len++;

	  return len;
	}
    }
  return 0;
}

/* Compute default value for "length_vex" attribute. It includes
   2 or 3 byte VEX prefix and 1 opcode byte.  */

int
ix86_attr_length_vex_default (rtx_insn *insn, bool has_0f_opcode,
			      bool has_vex_w)
{
  int i, reg_only = 2 + 1;
  bool has_mem = false;

  /* Only 0f opcode can use 2 byte VEX prefix and  VEX W bit uses 3
     byte VEX prefix.  */
  if (!has_0f_opcode || has_vex_w)
    return 3 + 1;

 /* We can always use 2 byte VEX prefix in 32bit.  */
  if (!TARGET_64BIT)
    return 2 + 1;

  extract_insn_cached (insn);

  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (REG_P (recog_data.operand[i]))
      {
	/* REX.W bit uses 3 byte VEX prefix.
	   REX2 with vex use extended EVEX prefix length is 4-byte.  */
	if (GET_MODE (recog_data.operand[i]) == DImode
	    && GENERAL_REG_P (recog_data.operand[i]))
	  return 3 + 1;

	/* REX.B bit requires 3-byte VEX. Right here we don't know which
	   operand will be encoded using VEX.B, so be conservative.
	   REX2 with vex use extended EVEX prefix length is 4-byte.  */
	if (REX_INT_REGNO_P (recog_data.operand[i])
	    || REX2_INT_REGNO_P (recog_data.operand[i])
	    || REX_SSE_REGNO_P (recog_data.operand[i]))
	  reg_only = 3 + 1;
      }
    else if (MEM_P (recog_data.operand[i]))
      {
	/* REX2.X or REX2.B bits use 3 byte VEX prefix.  */
	if (x86_extended_rex2reg_mentioned_p (recog_data.operand[i]))
	  return 4;

	/* REX.X or REX.B bits use 3 byte VEX prefix.  */
	if (x86_extended_reg_mentioned_p (recog_data.operand[i]))
	  return 3 + 1;

	has_mem = true;
      }

  return has_mem ? 2 + 1 : reg_only;
}


static bool
ix86_class_likely_spilled_p (reg_class_t);

/* Returns true if lhs of insn is HW function argument register and set up
   is_spilled to true if it is likely spilled HW register.  */
static bool
insn_is_function_arg (rtx insn, bool* is_spilled)
{
  rtx dst;

  if (!NONDEBUG_INSN_P (insn))
    return false;
  /* Call instructions are not movable, ignore it.  */
  if (CALL_P (insn))
    return false;
  insn = PATTERN (insn);
  if (GET_CODE (insn) == PARALLEL)
    insn = XVECEXP (insn, 0, 0);
  if (GET_CODE (insn) != SET)
    return false;
  dst = SET_DEST (insn);
  if (REG_P (dst) && HARD_REGISTER_P (dst)
      && ix86_function_arg_regno_p (REGNO (dst)))
    {
      /* Is it likely spilled HW register?  */
      if (!TEST_HARD_REG_BIT (fixed_reg_set, REGNO (dst))
	  && ix86_class_likely_spilled_p (REGNO_REG_CLASS (REGNO (dst))))
	*is_spilled = true;
      return true;
    }
  return false;
}

/* Add output dependencies for chain of function adjacent arguments if only
   there is a move to likely spilled HW register.  Return first argument
   if at least one dependence was added or NULL otherwise.  */
static rtx_insn *
add_parameter_dependencies (rtx_insn *call, rtx_insn *head)
{
  rtx_insn *insn;
  rtx_insn *last = call;
  rtx_insn *first_arg = NULL;
  bool is_spilled = false;

  head = PREV_INSN (head);

  /* Find nearest to call argument passing instruction.  */
  while (true)
    {
      last = PREV_INSN (last);
      if (last == head)
	return NULL;
      if (!NONDEBUG_INSN_P (last))
	continue;
      if (insn_is_function_arg (last, &is_spilled))
	break;
      return NULL;
    }

  first_arg = last;
  while (true)
    {
      insn = PREV_INSN (last);
      if (!INSN_P (insn))
	break;
      if (insn == head)
	break;
      if (!NONDEBUG_INSN_P (insn))
	{
	  last = insn;
	  continue;
	}
      if (insn_is_function_arg (insn, &is_spilled))
	{
	  /* Add output depdendence between two function arguments if chain
	     of output arguments contains likely spilled HW registers.  */
	  if (is_spilled)
	    add_dependence (first_arg, insn, REG_DEP_OUTPUT);
	  first_arg = last = insn;
	}
      else
	break;
    }
  if (!is_spilled)
    return NULL;
  return first_arg;
}

/* Add output or anti dependency from insn to first_arg to restrict its code
   motion.  */
static void
avoid_func_arg_motion (rtx_insn *first_arg, rtx_insn *insn)
{
  rtx set;
  rtx tmp;

  set = single_set (insn);
  if (!set)
    return;
  tmp = SET_DEST (set);
  if (REG_P (tmp))
    {
      /* Add output dependency to the first function argument.  */
      add_dependence (first_arg, insn, REG_DEP_OUTPUT);
      return;
    }
  /* Add anti dependency.  */
  add_dependence (first_arg, insn, REG_DEP_ANTI);
}

/* Avoid cross block motion of function argument through adding dependency
   from the first non-jump instruction in bb.  */
static void
add_dependee_for_func_arg (rtx_insn *arg, basic_block bb)
{
  rtx_insn *insn = BB_END (bb);

  while (insn)
    {
      if (NONDEBUG_INSN_P (insn) && NONJUMP_INSN_P (insn))
	{
	  rtx set = single_set (insn);
	  if (set)
	    {
	      avoid_func_arg_motion (arg, insn);
	      return;
	    }
	}
      if (insn == BB_HEAD (bb))
	return;
      insn = PREV_INSN (insn);
    }
}

/* Hook for pre-reload schedule - avoid motion of function arguments
   passed in likely spilled HW registers.  */
static void
ix86_dependencies_evaluation_hook (rtx_insn *head, rtx_insn *tail)
{
  rtx_insn *insn;
  rtx_insn *first_arg = NULL;
  if (reload_completed)
    return;
  while (head != tail && DEBUG_INSN_P (head))
    head = NEXT_INSN (head);
  for (insn = tail; insn != head; insn = PREV_INSN (insn))
    if (INSN_P (insn) && CALL_P (insn))
      {
	first_arg = add_parameter_dependencies (insn, head);
	if (first_arg)
	  {
	    /* Add dependee for first argument to predecessors if only
	       region contains more than one block.  */
	    basic_block bb =  BLOCK_FOR_INSN (insn);
	    int rgn = CONTAINING_RGN (bb->index);
	    int nr_blks = RGN_NR_BLOCKS (rgn);
	    /* Skip trivial regions and region head blocks that can have
	       predecessors outside of region.  */
	    if (nr_blks > 1 && BLOCK_TO_BB (bb->index) != 0)
	      {
		edge e;
		edge_iterator ei;

		/* Regions are SCCs with the exception of selective
		   scheduling with pipelining of outer blocks enabled.
		   So also check that immediate predecessors of a non-head
		   block are in the same region.  */
		FOR_EACH_EDGE (e, ei, bb->preds)
		  {
		    /* Avoid creating of loop-carried dependencies through
		       using topological ordering in the region.  */
		    if (rgn == CONTAINING_RGN (e->src->index)
			&& BLOCK_TO_BB (bb->index) > BLOCK_TO_BB (e->src->index))
		      add_dependee_for_func_arg (first_arg, e->src); 
		  }
	      }
	    insn = first_arg;
	    if (insn == head)
	      break;
	  }
      }
    else if (first_arg)
      avoid_func_arg_motion (first_arg, insn);
}

/* Hook for pre-reload schedule - set priority of moves from likely spilled
   HW registers to maximum, to schedule them at soon as possible. These are
   moves from function argument registers at the top of the function entry
   and moves from function return value registers after call.  */
static int
ix86_adjust_priority (rtx_insn *insn, int priority)
{
  rtx set;

  if (reload_completed)
    return priority;

  if (!NONDEBUG_INSN_P (insn))
    return priority;

  set = single_set (insn);
  if (set)
    {
      rtx tmp = SET_SRC (set);
      if (REG_P (tmp)
          && HARD_REGISTER_P (tmp)
          && !TEST_HARD_REG_BIT (fixed_reg_set, REGNO (tmp))
          && ix86_class_likely_spilled_p (REGNO_REG_CLASS (REGNO (tmp))))
	return current_sched_info->sched_max_insns_priority;
    }

  return priority;
}

/* Prepare for scheduling pass.  */
static void
ix86_sched_init_global (FILE *, int, int)
{
  /* Install scheduling hooks for current CPU.  Some of these hooks are used
     in time-critical parts of the scheduler, so we only set them up when
     they are actually used.  */
  switch (ix86_tune)
    {
    case PROCESSOR_CORE2:
    case PROCESSOR_NEHALEM:
    case PROCESSOR_SANDYBRIDGE:
    case PROCESSOR_HASWELL:
    case PROCESSOR_TREMONT:
    case PROCESSOR_ALDERLAKE:
    case PROCESSOR_GENERIC:
      /* Do not perform multipass scheduling for pre-reload schedule
         to save compile time.  */
      if (reload_completed)
	{
	  ix86_core2i7_init_hooks ();
	  break;
	}
      /* Fall through.  */
    default:
      targetm.sched.dfa_post_advance_cycle = NULL;
      targetm.sched.first_cycle_multipass_init = NULL;
      targetm.sched.first_cycle_multipass_begin = NULL;
      targetm.sched.first_cycle_multipass_issue = NULL;
      targetm.sched.first_cycle_multipass_backtrack = NULL;
      targetm.sched.first_cycle_multipass_end = NULL;
      targetm.sched.first_cycle_multipass_fini = NULL;
      break;
    }
}


/* Implement TARGET_STATIC_RTX_ALIGNMENT.  */

static HOST_WIDE_INT
ix86_static_rtx_alignment (machine_mode mode)
{
  if (mode == DFmode)
    return 64;
  if (ALIGN_MODE_128 (mode))
    return MAX (128, GET_MODE_ALIGNMENT (mode));
  return GET_MODE_ALIGNMENT (mode);
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  */

static HOST_WIDE_INT
ix86_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == REAL_CST || TREE_CODE (exp) == VECTOR_CST
      || TREE_CODE (exp) == INTEGER_CST)
    {
      machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
      HOST_WIDE_INT mode_align = ix86_static_rtx_alignment (mode);
      return MAX (mode_align, align);
    }
  else if (!optimize_size && TREE_CODE (exp) == STRING_CST
	   && TREE_STRING_LENGTH (exp) >= 31 && align < BITS_PER_WORD)
    return BITS_PER_WORD;

  return align;
}

/* Implement TARGET_EMPTY_RECORD_P.  */

static bool
ix86_is_empty_record (const_tree type)
{
  if (!TARGET_64BIT)
    return false;
  return default_is_empty_record (type);
}

/* Implement TARGET_WARN_PARAMETER_PASSING_ABI.  */

static void
ix86_warn_parameter_passing_abi (cumulative_args_t cum_v, tree type)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (!cum->warn_empty)
    return;

  if (!TYPE_EMPTY_P (type))
    return;

  /* Don't warn if the function isn't visible outside of the TU.  */
  if (cum->decl && !TREE_PUBLIC (cum->decl))
    return;

  const_tree ctx = get_ultimate_context (cum->decl);
  if (ctx != NULL_TREE
      && !TRANSLATION_UNIT_WARN_EMPTY_P (ctx))
    return;

  /* If the actual size of the type is zero, then there is no change
     in how objects of this size are passed.  */
  if (int_size_in_bytes (type) == 0)
    return;

  warning (OPT_Wabi, "empty class %qT parameter passing ABI "
	   "changes in %<-fabi-version=12%> (GCC 8)", type);

  /* Only warn once.  */
  cum->warn_empty = false;
}

/* This hook returns name of multilib ABI.  */

static const char *
ix86_get_multilib_abi_name (void)
{
  if (!(TARGET_64BIT_P (ix86_isa_flags)))
    return "i386";
  else if (TARGET_X32_P (ix86_isa_flags))
    return "x32";
  else
    return "x86_64";
}

/* Compute the alignment for a variable for Intel MCU psABI.  TYPE is
   the data type, and ALIGN is the alignment that the object would
   ordinarily have.  */

static int
iamcu_alignment (tree type, int align)
{
  machine_mode mode;

  if (align < 32 || TYPE_USER_ALIGN (type))
    return align;

  /* Intel MCU psABI specifies scalar types > 4 bytes aligned to 4
     bytes.  */
  type = strip_array_types (type);
  if (TYPE_ATOMIC (type))
    return align;

  mode = TYPE_MODE (type);
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_INT:
    case MODE_COMPLEX_INT:
    case MODE_COMPLEX_FLOAT:
    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
      return 32;
    default:
      return align;
    }
}

/* Compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this function is used
   instead of that alignment to align the object.  */

int
ix86_data_alignment (tree type, unsigned int align, bool opt)
{
  /* GCC 4.8 and earlier used to incorrectly assume this alignment even
     for symbols from other compilation units or symbols that don't need
     to bind locally.  In order to preserve some ABI compatibility with
     those compilers, ensure we don't decrease alignment from what we
     used to assume.  */

  unsigned int max_align_compat = MIN (256, MAX_OFILE_ALIGNMENT);

  /* A data structure, equal or greater than the size of a cache line
     (64 bytes in the Pentium 4 and other recent Intel processors, including
     processors based on Intel Core microarchitecture) should be aligned
     so that its base address is a multiple of a cache line size.  */

  unsigned int max_align
    = MIN ((unsigned) ix86_tune_cost->prefetch_block * 8, MAX_OFILE_ALIGNMENT);

  if (max_align < BITS_PER_WORD)
    max_align = BITS_PER_WORD;

  switch (ix86_align_data_type)
    {
    case ix86_align_data_type_abi: opt = false; break;
    case ix86_align_data_type_compat: max_align = BITS_PER_WORD; break;
    case ix86_align_data_type_cacheline: break;
    }

  if (TARGET_IAMCU)
    align = iamcu_alignment (type, align);

  if (opt
      && AGGREGATE_TYPE_P (type)
      && TYPE_SIZE (type)
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
    {
      if (wi::geu_p (wi::to_wide (TYPE_SIZE (type)), max_align_compat)
	  && align < max_align_compat)
	align = max_align_compat;
      if (wi::geu_p (wi::to_wide (TYPE_SIZE (type)), max_align)
	  && align < max_align)
	align = max_align;
    }

  /* x86-64 ABI requires arrays greater than 16 bytes to be aligned
     to 16byte boundary.  */
  if (TARGET_64BIT)
    {
      if ((opt ? AGGREGATE_TYPE_P (type) : TREE_CODE (type) == ARRAY_TYPE)
	  && TYPE_SIZE (type)
	  && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	  && wi::geu_p (wi::to_wide (TYPE_SIZE (type)), 128)
	  && align < 128)
	return 128;
    }

  if (!opt)
    return align;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_MODE (TREE_TYPE (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    {

      if (TYPE_MODE (type) == DCmode && align < 64)
	return 64;
      if ((TYPE_MODE (type) == XCmode
	   || TYPE_MODE (type) == TCmode) && align < 128)
	return 128;
    }
  else if (RECORD_OR_UNION_TYPE_P (type)
	   && TYPE_FIELDS (type))
    {
      if (DECL_MODE (TYPE_FIELDS (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (DECL_MODE (TYPE_FIELDS (type))) && align < 128)
	return 128;
    }
  else if (SCALAR_FLOAT_TYPE_P (type) || VECTOR_TYPE_P (type)
	   || TREE_CODE (type) == INTEGER_TYPE)
    {
      if (TYPE_MODE (type) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (type)) && align < 128)
	return 128;
    }

  return align;
}

/* Implememnt TARGET_LOWER_LOCAL_DECL_ALIGNMENT.  */
static void
ix86_lower_local_decl_alignment (tree decl)
{
  unsigned int new_align = ix86_local_alignment (decl, VOIDmode,
						 DECL_ALIGN (decl), true);
  if (new_align < DECL_ALIGN (decl))
    SET_DECL_ALIGN (decl, new_align);
}

/* Compute the alignment for a local variable or a stack slot.  EXP is
   the data type or decl itself, MODE is the widest mode available and
   ALIGN is the alignment that the object would ordinarily have.  The
   value of this macro is used instead of that alignment to align the
   object.  */

unsigned int
ix86_local_alignment (tree exp, machine_mode mode,
		      unsigned int align, bool may_lower)
{
  tree type, decl;

  if (exp && DECL_P (exp))
    {
      type = TREE_TYPE (exp);
      decl = exp;
    }
  else
    {
      type = exp;
      decl = NULL;
    }

  /* Don't do dynamic stack realignment for long long objects with
     -mpreferred-stack-boundary=2.  */
  if (may_lower
      && !TARGET_64BIT
      && align == 64
      && ix86_preferred_stack_boundary < 64
      && (mode == DImode || (type && TYPE_MODE (type) == DImode))
      && (!type || (!TYPE_USER_ALIGN (type)
		    && !TYPE_ATOMIC (strip_array_types (type))))
      && (!decl || !DECL_USER_ALIGN (decl)))
    align = 32;

  /* If TYPE is NULL, we are allocating a stack slot for caller-save
     register in MODE.  We will return the largest alignment of XF
     and DF.  */
  if (!type)
    {
      if (mode == XFmode && align < GET_MODE_ALIGNMENT (DFmode))
	align = GET_MODE_ALIGNMENT (DFmode);
      return align;
    }

  /* Don't increase alignment for Intel MCU psABI.  */
  if (TARGET_IAMCU)
    return align;

  /* x86-64 ABI requires arrays greater than 16 bytes to be aligned
     to 16byte boundary.  Exact wording is:

     An array uses the same alignment as its elements, except that a local or
     global array variable of length at least 16 bytes or
     a C99 variable-length array variable always has alignment of at least 16 bytes.

     This was added to allow use of aligned SSE instructions at arrays.  This
     rule is meant for static storage (where compiler cannot do the analysis
     by itself).  We follow it for automatic variables only when convenient.
     We fully control everything in the function compiled and functions from
     other unit cannot rely on the alignment.

     Exclude va_list type.  It is the common case of local array where
     we cannot benefit from the alignment.  

     TODO: Probably one should optimize for size only when var is not escaping.  */
  if (TARGET_64BIT && optimize_function_for_speed_p (cfun)
      && TARGET_SSE)
    {
      if (AGGREGATE_TYPE_P (type)
	  && (va_list_type_node == NULL_TREE
	      || (TYPE_MAIN_VARIANT (type)
		  != TYPE_MAIN_VARIANT (va_list_type_node)))
	  && TYPE_SIZE (type)
	  && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	  && wi::geu_p (wi::to_wide (TYPE_SIZE (type)), 128)
	  && align < 128)
	return 128;
    }
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_MODE (TREE_TYPE (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      if (TYPE_MODE (type) == DCmode && align < 64)
	return 64;
      if ((TYPE_MODE (type) == XCmode
	   || TYPE_MODE (type) == TCmode) && align < 128)
	return 128;
    }
  else if (RECORD_OR_UNION_TYPE_P (type)
	   && TYPE_FIELDS (type))
    {
      if (DECL_MODE (TYPE_FIELDS (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (DECL_MODE (TYPE_FIELDS (type))) && align < 128)
	return 128;
    }
  else if (SCALAR_FLOAT_TYPE_P (type) || VECTOR_TYPE_P (type)
	   || TREE_CODE (type) == INTEGER_TYPE)
    {

      if (TYPE_MODE (type) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (type)) && align < 128)
	return 128;
    }
  return align;
}

/* Compute the minimum required alignment for dynamic stack realignment
   purposes for a local variable, parameter or a stack slot.  EXP is
   the data type or decl itself, MODE is its mode and ALIGN is the
   alignment that the object would ordinarily have.  */

unsigned int
ix86_minimum_alignment (tree exp, machine_mode mode,
			unsigned int align)
{
  tree type, decl;

  if (exp && DECL_P (exp))
    {
      type = TREE_TYPE (exp);
      decl = exp;
    }
  else
    {
      type = exp;
      decl = NULL;
    }

  if (TARGET_64BIT || align != 64 || ix86_preferred_stack_boundary >= 64)
    return align;

  /* Don't do dynamic stack realignment for long long objects with
     -mpreferred-stack-boundary=2.  */
  if ((mode == DImode || (type && TYPE_MODE (type) == DImode))
      && (!type || (!TYPE_USER_ALIGN (type)
		    && !TYPE_ATOMIC (strip_array_types (type))))
      && (!decl || !DECL_USER_ALIGN (decl)))
    {
      gcc_checking_assert (!TARGET_STV);
      return 32;
    }

  return align;
}

/* Find a location for the static chain incoming to a nested function.
   This is a register, unless all free registers are used by arguments.  */

static rtx
ix86_static_chain (const_tree fndecl_or_type, bool incoming_p)
{
  unsigned regno;

  if (TARGET_64BIT)
    {
      /* We always use R10 in 64-bit mode.  */
      regno = R10_REG;
    }
  else
    {
      const_tree fntype, fndecl;
      unsigned int ccvt;

      /* By default in 32-bit mode we use ECX to pass the static chain.  */
      regno = CX_REG;

      if (TREE_CODE (fndecl_or_type) == FUNCTION_DECL)
	{
          fntype = TREE_TYPE (fndecl_or_type);
	  fndecl = fndecl_or_type;
	}
      else
	{
	  fntype = fndecl_or_type;
	  fndecl = NULL;
	}

      ccvt = ix86_get_callcvt (fntype);
      if ((ccvt & IX86_CALLCVT_FASTCALL) != 0)
	{
	  /* Fastcall functions use ecx/edx for arguments, which leaves
	     us with EAX for the static chain.
	     Thiscall functions use ecx for arguments, which also
	     leaves us with EAX for the static chain.  */
	  regno = AX_REG;
	}
      else if ((ccvt & IX86_CALLCVT_THISCALL) != 0)
	{
	  /* Thiscall functions use ecx for arguments, which leaves
	     us with EAX and EDX for the static chain.
	     We are using for abi-compatibility EAX.  */
	  regno = AX_REG;
	}
      else if (ix86_function_regparm (fntype, fndecl) == 3)
	{
	  /* For regparm 3, we have no free call-clobbered registers in
	     which to store the static chain.  In order to implement this,
	     we have the trampoline push the static chain to the stack.
	     However, we can't push a value below the return address when
	     we call the nested function directly, so we have to use an
	     alternate entry point.  For this we use ESI, and have the
	     alternate entry point push ESI, so that things appear the
	     same once we're executing the nested function.  */
	  if (incoming_p)
	    {
	      if (fndecl == current_function_decl
		  && !ix86_static_chain_on_stack)
		{
		  gcc_assert (!reload_completed);
		  ix86_static_chain_on_stack = true;
		}
	      return gen_frame_mem (SImode,
				    plus_constant (Pmode,
						   arg_pointer_rtx, -8));
	    }
	  regno = SI_REG;
	}
    }

  return gen_rtx_REG (Pmode, regno);
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNDECL is the decl of the target address; M_TRAMP is a MEM for
   the trampoline, and CHAIN_VALUE is an RTX for the static chain
   to be passed to the target function.  */

static void
ix86_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx mem, fnaddr;
  int opcode;
  int offset = 0;
  bool need_endbr = (flag_cf_protection & CF_BRANCH);

  fnaddr = XEXP (DECL_RTL (fndecl), 0);

  if (TARGET_64BIT)
    {
      int size;

      if (need_endbr)
	{
	  /* Insert ENDBR64.  */
	  mem = adjust_address (m_tramp, SImode, offset);
	  emit_move_insn (mem, gen_int_mode (0xfa1e0ff3, SImode));
	  offset += 4;
	}

      /* Load the function address to r11.  Try to load address using
	 the shorter movl instead of movabs.  We may want to support
	 movq for kernel mode, but kernel does not use trampolines at
	 the moment.  FNADDR is a 32bit address and may not be in
	 DImode when ptr_mode == SImode.  Always use movl in this
	 case.  */
      if (ptr_mode == SImode
	  || x86_64_zext_immediate_operand (fnaddr, VOIDmode))
	{
	  fnaddr = copy_addr_to_reg (fnaddr);

	  mem = adjust_address (m_tramp, HImode, offset);
	  emit_move_insn (mem, gen_int_mode (0xbb41, HImode));

	  mem = adjust_address (m_tramp, SImode, offset + 2);
	  emit_move_insn (mem, gen_lowpart (SImode, fnaddr));
	  offset += 6;
	}
      else
	{
	  mem = adjust_address (m_tramp, HImode, offset);
	  emit_move_insn (mem, gen_int_mode (0xbb49, HImode));

	  mem = adjust_address (m_tramp, DImode, offset + 2);
	  emit_move_insn (mem, fnaddr);
	  offset += 10;
	}

      /* Load static chain using movabs to r10.  Use the shorter movl
         instead of movabs when ptr_mode == SImode.  */
      if (ptr_mode == SImode)
	{
	  opcode = 0xba41;
	  size = 6;
	}
      else
	{
	  opcode = 0xba49;
	  size = 10;
	}

      mem = adjust_address (m_tramp, HImode, offset);
      emit_move_insn (mem, gen_int_mode (opcode, HImode));

      mem = adjust_address (m_tramp, ptr_mode, offset + 2);
      emit_move_insn (mem, chain_value);
      offset += size;

      /* Jump to r11; the last (unused) byte is a nop, only there to
	 pad the write out to a single 32-bit store.  */
      mem = adjust_address (m_tramp, SImode, offset);
      emit_move_insn (mem, gen_int_mode (0x90e3ff49, SImode));
      offset += 4;
    }
  else
    {
      rtx disp, chain;

      /* Depending on the static chain location, either load a register
	 with a constant, or push the constant to the stack.  All of the
	 instructions are the same size.  */
      chain = ix86_static_chain (fndecl, true);
      if (REG_P (chain))
	{
	  switch (REGNO (chain))
	    {
	    case AX_REG:
	      opcode = 0xb8; break;
	    case CX_REG:
	      opcode = 0xb9; break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	opcode = 0x68;

      if (need_endbr)
	{
	  /* Insert ENDBR32.  */
	  mem = adjust_address (m_tramp, SImode, offset);
	  emit_move_insn (mem, gen_int_mode (0xfb1e0ff3, SImode));
	  offset += 4;
	}

      mem = adjust_address (m_tramp, QImode, offset);
      emit_move_insn (mem, gen_int_mode (opcode, QImode));

      mem = adjust_address (m_tramp, SImode, offset + 1);
      emit_move_insn (mem, chain_value);
      offset += 5;

      mem = adjust_address (m_tramp, QImode, offset);
      emit_move_insn (mem, gen_int_mode (0xe9, QImode));

      mem = adjust_address (m_tramp, SImode, offset + 1);

      /* Compute offset from the end of the jmp to the target function.
	 In the case in which the trampoline stores the static chain on
	 the stack, we need to skip the first insn which pushes the
	 (call-saved) register static chain; this push is 1 byte.  */
      offset += 5;
      int skip = MEM_P (chain) ? 1 : 0;
      /* Skip ENDBR32 at the entry of the target function.  */
      if (need_endbr
	  && !cgraph_node::get (fndecl)->only_called_directly_p ())
	skip += 4;
      disp = expand_binop (SImode, sub_optab, fnaddr,
			   plus_constant (Pmode, XEXP (m_tramp, 0),
					  offset - skip),
			   NULL_RTX, 1, OPTAB_DIRECT);
      emit_move_insn (mem, disp);
    }

  gcc_assert (offset <= TRAMPOLINE_SIZE);

#ifdef HAVE_ENABLE_EXECUTE_STACK
#ifdef CHECK_EXECUTE_STACK_ENABLED
  if (CHECK_EXECUTE_STACK_ENABLED)
#endif
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
		     LCT_NORMAL, VOIDmode, XEXP (m_tramp, 0), Pmode);
#endif
}

static bool
ix86_allocate_stack_slots_for_args (void)
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return !ix86_function_naked (current_function_decl);
}

static bool
ix86_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return !ix86_function_naked (decl);
}

/* Return the shift count of a vector by scalar shift builtin second argument
   ARG1.  */
static tree
ix86_vector_shift_count (tree arg1)
{
  if (tree_fits_uhwi_p (arg1))
    return arg1;
  else if (TREE_CODE (arg1) == VECTOR_CST && CHAR_BIT == 8)
    {
      /* The count argument is weird, passed in as various 128-bit
	 (or 64-bit) vectors, the low 64 bits from it are the count.  */
      unsigned char buf[16];
      int len = native_encode_expr (arg1, buf, 16);
      if (len == 0)
	return NULL_TREE;
      tree t = native_interpret_expr (uint64_type_node, buf, len);
      if (t && tree_fits_uhwi_p (t))
	return t;
    }
  return NULL_TREE;
}

/* Return true if arg_mask is all ones, ELEMS is elements number of
   corresponding vector.  */
static bool
ix86_masked_all_ones (unsigned HOST_WIDE_INT elems, tree arg_mask)
{
  if (TREE_CODE (arg_mask) != INTEGER_CST)
    return false;

  unsigned HOST_WIDE_INT mask = TREE_INT_CST_LOW (arg_mask);
  if (elems == HOST_BITS_PER_WIDE_INT)
    return  mask == HOST_WIDE_INT_M1U;
  if ((mask | (HOST_WIDE_INT_M1U << elems)) != HOST_WIDE_INT_M1U)
    return false;

  return true;
}

static tree
ix86_fold_builtin (tree fndecl, int n_args,
		   tree *args, bool ignore ATTRIBUTE_UNUSED)
{
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    {
      enum ix86_builtins fn_code
	= (enum ix86_builtins) DECL_MD_FUNCTION_CODE (fndecl);
      enum rtx_code rcode;
      bool is_vshift;
      unsigned HOST_WIDE_INT mask;

      switch (fn_code)
	{
	case IX86_BUILTIN_CPU_IS:
	case IX86_BUILTIN_CPU_SUPPORTS:
	  gcc_assert (n_args == 1);
	  return fold_builtin_cpu (fndecl, args);

	case IX86_BUILTIN_NANQ:
	case IX86_BUILTIN_NANSQ:
	  {
	    tree type = TREE_TYPE (TREE_TYPE (fndecl));
	    const char *str = c_getstr (*args);
	    int quiet = fn_code == IX86_BUILTIN_NANQ;
	    REAL_VALUE_TYPE real;

	    if (str && real_nan (&real, str, quiet, TYPE_MODE (type)))
	      return build_real (type, real);
	    return NULL_TREE;
	  }

	case IX86_BUILTIN_INFQ:
	case IX86_BUILTIN_HUGE_VALQ:
	  {
	    tree type = TREE_TYPE (TREE_TYPE (fndecl));
	    REAL_VALUE_TYPE inf;
	    real_inf (&inf);
	    return build_real (type, inf);
	  }

	case IX86_BUILTIN_TZCNT16:
	case IX86_BUILTIN_CTZS:
	case IX86_BUILTIN_TZCNT32:
	case IX86_BUILTIN_TZCNT64:
	  gcc_assert (n_args == 1);
	  if (TREE_CODE (args[0]) == INTEGER_CST)
	    {
	      tree type = TREE_TYPE (TREE_TYPE (fndecl));
	      tree arg = args[0];
	      if (fn_code == IX86_BUILTIN_TZCNT16
		  || fn_code == IX86_BUILTIN_CTZS)
		arg = fold_convert (short_unsigned_type_node, arg);
	      if (integer_zerop (arg))
		return build_int_cst (type, TYPE_PRECISION (TREE_TYPE (arg)));
	      else
		return fold_const_call (CFN_CTZ, type, arg);
	    }
	  break;

	case IX86_BUILTIN_LZCNT16:
	case IX86_BUILTIN_CLZS:
	case IX86_BUILTIN_LZCNT32:
	case IX86_BUILTIN_LZCNT64:
	  gcc_assert (n_args == 1);
	  if (TREE_CODE (args[0]) == INTEGER_CST)
	    {
	      tree type = TREE_TYPE (TREE_TYPE (fndecl));
	      tree arg = args[0];
	      if (fn_code == IX86_BUILTIN_LZCNT16
		  || fn_code == IX86_BUILTIN_CLZS)
		arg = fold_convert (short_unsigned_type_node, arg);
	      if (integer_zerop (arg))
		return build_int_cst (type, TYPE_PRECISION (TREE_TYPE (arg)));
	      else
		return fold_const_call (CFN_CLZ, type, arg);
	    }
	  break;

	case IX86_BUILTIN_BEXTR32:
	case IX86_BUILTIN_BEXTR64:
	case IX86_BUILTIN_BEXTRI32:
	case IX86_BUILTIN_BEXTRI64:
	  gcc_assert (n_args == 2);
	  if (tree_fits_uhwi_p (args[1]))
	    {
	      unsigned HOST_WIDE_INT res = 0;
	      unsigned int prec = TYPE_PRECISION (TREE_TYPE (args[0]));
	      unsigned int start = tree_to_uhwi (args[1]);
	      unsigned int len = (start & 0xff00) >> 8;
	      start &= 0xff;
	      if (start >= prec || len == 0)
		res = 0;
	      else if (!tree_fits_uhwi_p (args[0]))
		break;
	      else
		res = tree_to_uhwi (args[0]) >> start;
	      if (len > prec)
		len = prec;
	      if (len < HOST_BITS_PER_WIDE_INT)
		res &= (HOST_WIDE_INT_1U << len) - 1;
	      return build_int_cstu (TREE_TYPE (TREE_TYPE (fndecl)), res);
	    }
	  break;

	case IX86_BUILTIN_BZHI32:
	case IX86_BUILTIN_BZHI64:
	  gcc_assert (n_args == 2);
	  if (tree_fits_uhwi_p (args[1]))
	    {
	      unsigned int idx = tree_to_uhwi (args[1]) & 0xff;
	      if (idx >= TYPE_PRECISION (TREE_TYPE (args[0])))
		return args[0];
	      if (idx == 0)
		return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), 0);
	      if (!tree_fits_uhwi_p (args[0]))
		break;
	      unsigned HOST_WIDE_INT res = tree_to_uhwi (args[0]);
	      res &= ~(HOST_WIDE_INT_M1U << idx);
	      return build_int_cstu (TREE_TYPE (TREE_TYPE (fndecl)), res);
	    }
	  break;

	case IX86_BUILTIN_PDEP32:
	case IX86_BUILTIN_PDEP64:
	  gcc_assert (n_args == 2);
	  if (tree_fits_uhwi_p (args[0]) && tree_fits_uhwi_p (args[1]))
	    {
	      unsigned HOST_WIDE_INT src = tree_to_uhwi (args[0]);
	      unsigned HOST_WIDE_INT mask = tree_to_uhwi (args[1]);
	      unsigned HOST_WIDE_INT res = 0;
	      unsigned HOST_WIDE_INT m, k = 1;
	      for (m = 1; m; m <<= 1)
		if ((mask & m) != 0)
		  {
		    if ((src & k) != 0)
		      res |= m;
		    k <<= 1;
		  }
	      return build_int_cstu (TREE_TYPE (TREE_TYPE (fndecl)), res);
	    }
	  break;

	case IX86_BUILTIN_PEXT32:
	case IX86_BUILTIN_PEXT64:
	  gcc_assert (n_args == 2);
	  if (tree_fits_uhwi_p (args[0]) && tree_fits_uhwi_p (args[1]))
	    {
	      unsigned HOST_WIDE_INT src = tree_to_uhwi (args[0]);
	      unsigned HOST_WIDE_INT mask = tree_to_uhwi (args[1]);
	      unsigned HOST_WIDE_INT res = 0;
	      unsigned HOST_WIDE_INT m, k = 1;
	      for (m = 1; m; m <<= 1)
		if ((mask & m) != 0)
		  {
		    if ((src & m) != 0)
		      res |= k;
		    k <<= 1;
		  }
	      return build_int_cstu (TREE_TYPE (TREE_TYPE (fndecl)), res);
	    }
	  break;

	case IX86_BUILTIN_MOVMSKPS:
	case IX86_BUILTIN_PMOVMSKB:
	case IX86_BUILTIN_MOVMSKPD:
	case IX86_BUILTIN_PMOVMSKB128:
	case IX86_BUILTIN_MOVMSKPD256:
	case IX86_BUILTIN_MOVMSKPS256:
	case IX86_BUILTIN_PMOVMSKB256:
	  gcc_assert (n_args == 1);
	  if (TREE_CODE (args[0]) == VECTOR_CST)
	    {
	      HOST_WIDE_INT res = 0;
	      for (unsigned i = 0; i < VECTOR_CST_NELTS (args[0]); ++i)
		{
		  tree e = VECTOR_CST_ELT (args[0], i);
		  if (TREE_CODE (e) == INTEGER_CST && !TREE_OVERFLOW (e))
		    {
		      if (wi::neg_p (wi::to_wide (e)))
			res |= HOST_WIDE_INT_1 << i;
		    }
		  else if (TREE_CODE (e) == REAL_CST && !TREE_OVERFLOW (e))
		    {
		      if (TREE_REAL_CST (e).sign)
			res |= HOST_WIDE_INT_1 << i;
		    }
		  else
		    return NULL_TREE;
		}
	      return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), res);
	    }
	  break;

	case IX86_BUILTIN_PSLLD:
	case IX86_BUILTIN_PSLLD128:
	case IX86_BUILTIN_PSLLD128_MASK:
	case IX86_BUILTIN_PSLLD256:
	case IX86_BUILTIN_PSLLD256_MASK:
	case IX86_BUILTIN_PSLLD512:
	case IX86_BUILTIN_PSLLDI:
	case IX86_BUILTIN_PSLLDI128:
	case IX86_BUILTIN_PSLLDI128_MASK:
	case IX86_BUILTIN_PSLLDI256:
	case IX86_BUILTIN_PSLLDI256_MASK:
	case IX86_BUILTIN_PSLLDI512:
	case IX86_BUILTIN_PSLLQ:
	case IX86_BUILTIN_PSLLQ128:
	case IX86_BUILTIN_PSLLQ128_MASK:
	case IX86_BUILTIN_PSLLQ256:
	case IX86_BUILTIN_PSLLQ256_MASK:
	case IX86_BUILTIN_PSLLQ512:
	case IX86_BUILTIN_PSLLQI:
	case IX86_BUILTIN_PSLLQI128:
	case IX86_BUILTIN_PSLLQI128_MASK:
	case IX86_BUILTIN_PSLLQI256:
	case IX86_BUILTIN_PSLLQI256_MASK:
	case IX86_BUILTIN_PSLLQI512:
	case IX86_BUILTIN_PSLLW:
	case IX86_BUILTIN_PSLLW128:
	case IX86_BUILTIN_PSLLW128_MASK:
	case IX86_BUILTIN_PSLLW256:
	case IX86_BUILTIN_PSLLW256_MASK:
	case IX86_BUILTIN_PSLLW512_MASK:
	case IX86_BUILTIN_PSLLWI:
	case IX86_BUILTIN_PSLLWI128:
	case IX86_BUILTIN_PSLLWI128_MASK:
	case IX86_BUILTIN_PSLLWI256:
	case IX86_BUILTIN_PSLLWI256_MASK:
	case IX86_BUILTIN_PSLLWI512_MASK:
	  rcode = ASHIFT;
	  is_vshift = false;
	  goto do_shift;
	case IX86_BUILTIN_PSRAD:
	case IX86_BUILTIN_PSRAD128:
	case IX86_BUILTIN_PSRAD128_MASK:
	case IX86_BUILTIN_PSRAD256:
	case IX86_BUILTIN_PSRAD256_MASK:
	case IX86_BUILTIN_PSRAD512:
	case IX86_BUILTIN_PSRADI:
	case IX86_BUILTIN_PSRADI128:
	case IX86_BUILTIN_PSRADI128_MASK:
	case IX86_BUILTIN_PSRADI256:
	case IX86_BUILTIN_PSRADI256_MASK:
	case IX86_BUILTIN_PSRADI512:
	case IX86_BUILTIN_PSRAQ128_MASK:
	case IX86_BUILTIN_PSRAQ256_MASK:
	case IX86_BUILTIN_PSRAQ512:
	case IX86_BUILTIN_PSRAQI128_MASK:
	case IX86_BUILTIN_PSRAQI256_MASK:
	case IX86_BUILTIN_PSRAQI512:
	case IX86_BUILTIN_PSRAW:
	case IX86_BUILTIN_PSRAW128:
	case IX86_BUILTIN_PSRAW128_MASK:
	case IX86_BUILTIN_PSRAW256:
	case IX86_BUILTIN_PSRAW256_MASK:
	case IX86_BUILTIN_PSRAW512:
	case IX86_BUILTIN_PSRAWI:
	case IX86_BUILTIN_PSRAWI128:
	case IX86_BUILTIN_PSRAWI128_MASK:
	case IX86_BUILTIN_PSRAWI256:
	case IX86_BUILTIN_PSRAWI256_MASK:
	case IX86_BUILTIN_PSRAWI512:
	  rcode = ASHIFTRT;
	  is_vshift = false;
	  goto do_shift;
	case IX86_BUILTIN_PSRLD:
	case IX86_BUILTIN_PSRLD128:
	case IX86_BUILTIN_PSRLD128_MASK:
	case IX86_BUILTIN_PSRLD256:
	case IX86_BUILTIN_PSRLD256_MASK:
	case IX86_BUILTIN_PSRLD512:
	case IX86_BUILTIN_PSRLDI:
	case IX86_BUILTIN_PSRLDI128:
	case IX86_BUILTIN_PSRLDI128_MASK:
	case IX86_BUILTIN_PSRLDI256:
	case IX86_BUILTIN_PSRLDI256_MASK:
	case IX86_BUILTIN_PSRLDI512:
	case IX86_BUILTIN_PSRLQ:
	case IX86_BUILTIN_PSRLQ128:
	case IX86_BUILTIN_PSRLQ128_MASK:
	case IX86_BUILTIN_PSRLQ256:
	case IX86_BUILTIN_PSRLQ256_MASK:
	case IX86_BUILTIN_PSRLQ512:
	case IX86_BUILTIN_PSRLQI:
	case IX86_BUILTIN_PSRLQI128:
	case IX86_BUILTIN_PSRLQI128_MASK:
	case IX86_BUILTIN_PSRLQI256:
	case IX86_BUILTIN_PSRLQI256_MASK:
	case IX86_BUILTIN_PSRLQI512:
	case IX86_BUILTIN_PSRLW:
	case IX86_BUILTIN_PSRLW128:
	case IX86_BUILTIN_PSRLW128_MASK:
	case IX86_BUILTIN_PSRLW256:
	case IX86_BUILTIN_PSRLW256_MASK:
	case IX86_BUILTIN_PSRLW512:
	case IX86_BUILTIN_PSRLWI:
	case IX86_BUILTIN_PSRLWI128:
	case IX86_BUILTIN_PSRLWI128_MASK:
	case IX86_BUILTIN_PSRLWI256:
	case IX86_BUILTIN_PSRLWI256_MASK:
	case IX86_BUILTIN_PSRLWI512:
	  rcode = LSHIFTRT;
	  is_vshift = false;
	  goto do_shift;
	case IX86_BUILTIN_PSLLVV16HI:
	case IX86_BUILTIN_PSLLVV16SI:
	case IX86_BUILTIN_PSLLVV2DI:
	case IX86_BUILTIN_PSLLVV2DI_MASK:
	case IX86_BUILTIN_PSLLVV32HI:
	case IX86_BUILTIN_PSLLVV4DI:
	case IX86_BUILTIN_PSLLVV4DI_MASK:
	case IX86_BUILTIN_PSLLVV4SI:
	case IX86_BUILTIN_PSLLVV4SI_MASK:
	case IX86_BUILTIN_PSLLVV8DI:
	case IX86_BUILTIN_PSLLVV8HI:
	case IX86_BUILTIN_PSLLVV8SI:
	case IX86_BUILTIN_PSLLVV8SI_MASK:
	  rcode = ASHIFT;
	  is_vshift = true;
	  goto do_shift;
	case IX86_BUILTIN_PSRAVQ128:
	case IX86_BUILTIN_PSRAVQ256:
	case IX86_BUILTIN_PSRAVV16HI:
	case IX86_BUILTIN_PSRAVV16SI:
	case IX86_BUILTIN_PSRAVV32HI:
	case IX86_BUILTIN_PSRAVV4SI:
	case IX86_BUILTIN_PSRAVV4SI_MASK:
	case IX86_BUILTIN_PSRAVV8DI:
	case IX86_BUILTIN_PSRAVV8HI:
	case IX86_BUILTIN_PSRAVV8SI:
	case IX86_BUILTIN_PSRAVV8SI_MASK:
	  rcode = ASHIFTRT;
	  is_vshift = true;
	  goto do_shift;
	case IX86_BUILTIN_PSRLVV16HI:
	case IX86_BUILTIN_PSRLVV16SI:
	case IX86_BUILTIN_PSRLVV2DI:
	case IX86_BUILTIN_PSRLVV2DI_MASK:
	case IX86_BUILTIN_PSRLVV32HI:
	case IX86_BUILTIN_PSRLVV4DI:
	case IX86_BUILTIN_PSRLVV4DI_MASK:
	case IX86_BUILTIN_PSRLVV4SI:
	case IX86_BUILTIN_PSRLVV4SI_MASK:
	case IX86_BUILTIN_PSRLVV8DI:
	case IX86_BUILTIN_PSRLVV8HI:
	case IX86_BUILTIN_PSRLVV8SI:
	case IX86_BUILTIN_PSRLVV8SI_MASK:
	  rcode = LSHIFTRT;
	  is_vshift = true;
	  goto do_shift;

	do_shift:
	  gcc_assert (n_args >= 2);
	  if (TREE_CODE (args[0]) != VECTOR_CST)
	    break;
	  mask = HOST_WIDE_INT_M1U;
	  if (n_args > 2)
	    {
	      /* This is masked shift.  */
	      if (!tree_fits_uhwi_p (args[n_args - 1])
		  || TREE_SIDE_EFFECTS (args[n_args - 2]))
		break;
	      mask = tree_to_uhwi (args[n_args - 1]);
	      unsigned elems = TYPE_VECTOR_SUBPARTS (TREE_TYPE (args[0]));
	      mask |= HOST_WIDE_INT_M1U << elems;
	      if (mask != HOST_WIDE_INT_M1U
		  && TREE_CODE (args[n_args - 2]) != VECTOR_CST)
		break;
	      if (mask == (HOST_WIDE_INT_M1U << elems))
		return args[n_args - 2];
	    }
	  if (is_vshift && TREE_CODE (args[1]) != VECTOR_CST)
	    break;
	  if (tree tem = (is_vshift ? integer_one_node
			  : ix86_vector_shift_count (args[1])))
	    {
	      unsigned HOST_WIDE_INT count = tree_to_uhwi (tem);
	      unsigned HOST_WIDE_INT prec
		= TYPE_PRECISION (TREE_TYPE (TREE_TYPE (args[0])));
	      if (count == 0 && mask == HOST_WIDE_INT_M1U)
		return args[0];
	      if (count >= prec)
		{
		  if (rcode == ASHIFTRT)
		    count = prec - 1;
		  else if (mask == HOST_WIDE_INT_M1U)
		    return build_zero_cst (TREE_TYPE (args[0]));
		}
	      tree countt = NULL_TREE;
	      if (!is_vshift)
		{
		  if (count >= prec)
		    countt = integer_zero_node;
		  else
		    countt = build_int_cst (integer_type_node, count);
		}
	      tree_vector_builder builder;
	      if (mask != HOST_WIDE_INT_M1U || is_vshift)
		builder.new_vector (TREE_TYPE (args[0]),
				    TYPE_VECTOR_SUBPARTS (TREE_TYPE (args[0])),
				    1);
	      else
		builder.new_unary_operation (TREE_TYPE (args[0]), args[0],
					     false);
	      unsigned int cnt = builder.encoded_nelts ();
	      for (unsigned int i = 0; i < cnt; ++i)
		{
		  tree elt = VECTOR_CST_ELT (args[0], i);
		  if (TREE_CODE (elt) != INTEGER_CST || TREE_OVERFLOW (elt))
		    return NULL_TREE;
		  tree type = TREE_TYPE (elt);
		  if (rcode == LSHIFTRT)
		    elt = fold_convert (unsigned_type_for (type), elt);
		  if (is_vshift)
		    {
		      countt = VECTOR_CST_ELT (args[1], i);
		      if (TREE_CODE (countt) != INTEGER_CST
			  || TREE_OVERFLOW (countt))
			return NULL_TREE;
		      if (wi::neg_p (wi::to_wide (countt))
			  || wi::to_widest (countt) >= prec)
			{
			  if (rcode == ASHIFTRT)
			    countt = build_int_cst (TREE_TYPE (countt),
						    prec - 1);
			  else
			    {
			      elt = build_zero_cst (TREE_TYPE (elt));
			      countt = build_zero_cst (TREE_TYPE (countt));
			    }
			}
		    }
		  else if (count >= prec)
		    elt = build_zero_cst (TREE_TYPE (elt));
		  elt = const_binop (rcode == ASHIFT
				     ? LSHIFT_EXPR : RSHIFT_EXPR,
				     TREE_TYPE (elt), elt, countt);
		  if (!elt || TREE_CODE (elt) != INTEGER_CST)
		    return NULL_TREE;
		  if (rcode == LSHIFTRT)
		    elt = fold_convert (type, elt);
		  if ((mask & (HOST_WIDE_INT_1U << i)) == 0)
		    {
		      elt = VECTOR_CST_ELT (args[n_args - 2], i);
		      if (TREE_CODE (elt) != INTEGER_CST
			  || TREE_OVERFLOW (elt))
			return NULL_TREE;
		    }
		  builder.quick_push (elt);
		}
	      return builder.build ();
	    }
	  break;

	default:
	  break;
	}
    }

#ifdef SUBTARGET_FOLD_BUILTIN
  return SUBTARGET_FOLD_BUILTIN (fndecl, n_args, args, ignore);
#endif

  return NULL_TREE;
}

/* Fold a MD builtin (use ix86_fold_builtin for folding into
   constant) in GIMPLE.  */

bool
ix86_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi), *g;
  gimple_seq stmts = NULL;
  tree fndecl = gimple_call_fndecl (stmt);
  gcc_checking_assert (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_MD));
  int n_args = gimple_call_num_args (stmt);
  enum ix86_builtins fn_code
    = (enum ix86_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  tree decl = NULL_TREE;
  tree arg0, arg1, arg2;
  enum rtx_code rcode;
  enum tree_code tcode;
  unsigned HOST_WIDE_INT count;
  bool is_vshift;
  unsigned HOST_WIDE_INT elems;
  location_t loc;

  /* Don't fold when there's isa mismatch.  */
  if (!ix86_check_builtin_isa_match (fn_code, NULL, NULL))
    return false;

  switch (fn_code)
    {
    case IX86_BUILTIN_TZCNT32:
      decl = builtin_decl_implicit (BUILT_IN_CTZ);
      goto fold_tzcnt_lzcnt;

    case IX86_BUILTIN_TZCNT64:
      decl = builtin_decl_implicit (BUILT_IN_CTZLL);
      goto fold_tzcnt_lzcnt;

    case IX86_BUILTIN_LZCNT32:
      decl = builtin_decl_implicit (BUILT_IN_CLZ);
      goto fold_tzcnt_lzcnt;

    case IX86_BUILTIN_LZCNT64:
      decl = builtin_decl_implicit (BUILT_IN_CLZLL);
      goto fold_tzcnt_lzcnt;

    fold_tzcnt_lzcnt:
      gcc_assert (n_args == 1);
      arg0 = gimple_call_arg (stmt, 0);
      if (TREE_CODE (arg0) == SSA_NAME && decl && gimple_call_lhs (stmt))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (arg0));
	  /* If arg0 is provably non-zero, optimize into generic
	     __builtin_c[tl]z{,ll} function the middle-end handles
	     better.  */
	  if (!expr_not_equal_to (arg0, wi::zero (prec)))
	    return false;

	  loc = gimple_location (stmt);
	  g = gimple_build_call (decl, 1, arg0);
	  gimple_set_location (g, loc);
	  tree lhs = make_ssa_name (integer_type_node);
	  gimple_call_set_lhs (g, lhs);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  g = gimple_build_assign (gimple_call_lhs (stmt), NOP_EXPR, lhs);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      break;

    case IX86_BUILTIN_BZHI32:
    case IX86_BUILTIN_BZHI64:
      gcc_assert (n_args == 2);
      arg1 = gimple_call_arg (stmt, 1);
      if (tree_fits_uhwi_p (arg1) && gimple_call_lhs (stmt))
	{
	  unsigned int idx = tree_to_uhwi (arg1) & 0xff;
	  arg0 = gimple_call_arg (stmt, 0);
	  if (idx < TYPE_PRECISION (TREE_TYPE (arg0)))
	    break;
	  loc = gimple_location (stmt);
	  g = gimple_build_assign (gimple_call_lhs (stmt), arg0);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      break;

    case IX86_BUILTIN_PDEP32:
    case IX86_BUILTIN_PDEP64:
    case IX86_BUILTIN_PEXT32:
    case IX86_BUILTIN_PEXT64:
      gcc_assert (n_args == 2);
      arg1 = gimple_call_arg (stmt, 1);
      if (integer_all_onesp (arg1) && gimple_call_lhs (stmt))
	{
	  loc = gimple_location (stmt);
	  arg0 = gimple_call_arg (stmt, 0);
	  g = gimple_build_assign (gimple_call_lhs (stmt), arg0);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      break;

    case IX86_BUILTIN_PBLENDVB256:
    case IX86_BUILTIN_BLENDVPS256:
    case IX86_BUILTIN_BLENDVPD256:
      /* pcmpeqb/d/q is under avx2, w/o avx2, it's veclower
	 to scalar operations and not combined back.  */
      if (!TARGET_AVX2)
	break;

      /* FALLTHRU.  */
    case IX86_BUILTIN_BLENDVPD:
      /* blendvpd is under sse4.1 but pcmpgtq is under sse4.2,
	 w/o sse4.2, it's veclowered to scalar operations and
	 not combined back.  */
      if (!TARGET_SSE4_2)
	break;
      /* FALLTHRU.  */
    case IX86_BUILTIN_PBLENDVB128:
    case IX86_BUILTIN_BLENDVPS:
      gcc_assert (n_args == 3);
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      arg2 = gimple_call_arg (stmt, 2);
      if (gimple_call_lhs (stmt))
	{
	  loc = gimple_location (stmt);
	  tree type = TREE_TYPE (arg2);
	  if (VECTOR_FLOAT_TYPE_P (type))
	    {
	      tree itype = GET_MODE_INNER (TYPE_MODE (type)) == E_SFmode
		? intSI_type_node : intDI_type_node;
	      type = get_same_sized_vectype (itype, type);
	    }
	  else
	    type = signed_type_for (type);
	  arg2 = gimple_build (&stmts, VIEW_CONVERT_EXPR, type, arg2);
	  tree zero_vec = build_zero_cst (type);
	  tree cmp_type = truth_type_for (type);
	  tree cmp = gimple_build (&stmts, LT_EXPR, cmp_type, arg2, zero_vec);
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  g = gimple_build_assign (gimple_call_lhs (stmt),
				   VEC_COND_EXPR, cmp,
				   arg1, arg0);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	}
      else
	gsi_replace (gsi, gimple_build_nop (), false);
      return true;


    case IX86_BUILTIN_PCMPEQB128:
    case IX86_BUILTIN_PCMPEQW128:
    case IX86_BUILTIN_PCMPEQD128:
    case IX86_BUILTIN_PCMPEQQ:
    case IX86_BUILTIN_PCMPEQB256:
    case IX86_BUILTIN_PCMPEQW256:
    case IX86_BUILTIN_PCMPEQD256:
    case IX86_BUILTIN_PCMPEQQ256:
      tcode = EQ_EXPR;
      goto do_cmp;

    case IX86_BUILTIN_PCMPGTB128:
    case IX86_BUILTIN_PCMPGTW128:
    case IX86_BUILTIN_PCMPGTD128:
    case IX86_BUILTIN_PCMPGTQ:
    case IX86_BUILTIN_PCMPGTB256:
    case IX86_BUILTIN_PCMPGTW256:
    case IX86_BUILTIN_PCMPGTD256:
    case IX86_BUILTIN_PCMPGTQ256:
      tcode = GT_EXPR;

    do_cmp:
      gcc_assert (n_args == 2);
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      if (gimple_call_lhs (stmt))
	{
	  loc = gimple_location (stmt);
	  tree type = TREE_TYPE (arg0);
	  tree zero_vec = build_zero_cst (type);
	  tree minus_one_vec = build_minus_one_cst (type);
	  tree cmp_type = truth_type_for (type);
	  tree cmp = gimple_build (&stmts, tcode, cmp_type, arg0, arg1);
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  g = gimple_build_assign (gimple_call_lhs (stmt),
				   VEC_COND_EXPR, cmp,
				   minus_one_vec, zero_vec);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	}
      else
	gsi_replace (gsi, gimple_build_nop (), false);
      return true;

    case IX86_BUILTIN_PSLLD:
    case IX86_BUILTIN_PSLLD128:
    case IX86_BUILTIN_PSLLD128_MASK:
    case IX86_BUILTIN_PSLLD256:
    case IX86_BUILTIN_PSLLD256_MASK:
    case IX86_BUILTIN_PSLLD512:
    case IX86_BUILTIN_PSLLDI:
    case IX86_BUILTIN_PSLLDI128:
    case IX86_BUILTIN_PSLLDI128_MASK:
    case IX86_BUILTIN_PSLLDI256:
    case IX86_BUILTIN_PSLLDI256_MASK:
    case IX86_BUILTIN_PSLLDI512:
    case IX86_BUILTIN_PSLLQ:
    case IX86_BUILTIN_PSLLQ128:
    case IX86_BUILTIN_PSLLQ128_MASK:
    case IX86_BUILTIN_PSLLQ256:
    case IX86_BUILTIN_PSLLQ256_MASK:
    case IX86_BUILTIN_PSLLQ512:
    case IX86_BUILTIN_PSLLQI:
    case IX86_BUILTIN_PSLLQI128:
    case IX86_BUILTIN_PSLLQI128_MASK:
    case IX86_BUILTIN_PSLLQI256:
    case IX86_BUILTIN_PSLLQI256_MASK:
    case IX86_BUILTIN_PSLLQI512:
    case IX86_BUILTIN_PSLLW:
    case IX86_BUILTIN_PSLLW128:
    case IX86_BUILTIN_PSLLW128_MASK:
    case IX86_BUILTIN_PSLLW256:
    case IX86_BUILTIN_PSLLW256_MASK:
    case IX86_BUILTIN_PSLLW512_MASK:
    case IX86_BUILTIN_PSLLWI:
    case IX86_BUILTIN_PSLLWI128:
    case IX86_BUILTIN_PSLLWI128_MASK:
    case IX86_BUILTIN_PSLLWI256:
    case IX86_BUILTIN_PSLLWI256_MASK:
    case IX86_BUILTIN_PSLLWI512_MASK:
      rcode = ASHIFT;
      is_vshift = false;
      goto do_shift;
    case IX86_BUILTIN_PSRAD:
    case IX86_BUILTIN_PSRAD128:
    case IX86_BUILTIN_PSRAD128_MASK:
    case IX86_BUILTIN_PSRAD256:
    case IX86_BUILTIN_PSRAD256_MASK:
    case IX86_BUILTIN_PSRAD512:
    case IX86_BUILTIN_PSRADI:
    case IX86_BUILTIN_PSRADI128:
    case IX86_BUILTIN_PSRADI128_MASK:
    case IX86_BUILTIN_PSRADI256:
    case IX86_BUILTIN_PSRADI256_MASK:
    case IX86_BUILTIN_PSRADI512:
    case IX86_BUILTIN_PSRAQ128_MASK:
    case IX86_BUILTIN_PSRAQ256_MASK:
    case IX86_BUILTIN_PSRAQ512:
    case IX86_BUILTIN_PSRAQI128_MASK:
    case IX86_BUILTIN_PSRAQI256_MASK:
    case IX86_BUILTIN_PSRAQI512:
    case IX86_BUILTIN_PSRAW:
    case IX86_BUILTIN_PSRAW128:
    case IX86_BUILTIN_PSRAW128_MASK:
    case IX86_BUILTIN_PSRAW256:
    case IX86_BUILTIN_PSRAW256_MASK:
    case IX86_BUILTIN_PSRAW512:
    case IX86_BUILTIN_PSRAWI:
    case IX86_BUILTIN_PSRAWI128:
    case IX86_BUILTIN_PSRAWI128_MASK:
    case IX86_BUILTIN_PSRAWI256:
    case IX86_BUILTIN_PSRAWI256_MASK:
    case IX86_BUILTIN_PSRAWI512:
      rcode = ASHIFTRT;
      is_vshift = false;
      goto do_shift;
    case IX86_BUILTIN_PSRLD:
    case IX86_BUILTIN_PSRLD128:
    case IX86_BUILTIN_PSRLD128_MASK:
    case IX86_BUILTIN_PSRLD256:
    case IX86_BUILTIN_PSRLD256_MASK:
    case IX86_BUILTIN_PSRLD512:
    case IX86_BUILTIN_PSRLDI:
    case IX86_BUILTIN_PSRLDI128:
    case IX86_BUILTIN_PSRLDI128_MASK:
    case IX86_BUILTIN_PSRLDI256:
    case IX86_BUILTIN_PSRLDI256_MASK:
    case IX86_BUILTIN_PSRLDI512:
    case IX86_BUILTIN_PSRLQ:
    case IX86_BUILTIN_PSRLQ128:
    case IX86_BUILTIN_PSRLQ128_MASK:
    case IX86_BUILTIN_PSRLQ256:
    case IX86_BUILTIN_PSRLQ256_MASK:
    case IX86_BUILTIN_PSRLQ512:
    case IX86_BUILTIN_PSRLQI:
    case IX86_BUILTIN_PSRLQI128:
    case IX86_BUILTIN_PSRLQI128_MASK:
    case IX86_BUILTIN_PSRLQI256:
    case IX86_BUILTIN_PSRLQI256_MASK:
    case IX86_BUILTIN_PSRLQI512:
    case IX86_BUILTIN_PSRLW:
    case IX86_BUILTIN_PSRLW128:
    case IX86_BUILTIN_PSRLW128_MASK:
    case IX86_BUILTIN_PSRLW256:
    case IX86_BUILTIN_PSRLW256_MASK:
    case IX86_BUILTIN_PSRLW512:
    case IX86_BUILTIN_PSRLWI:
    case IX86_BUILTIN_PSRLWI128:
    case IX86_BUILTIN_PSRLWI128_MASK:
    case IX86_BUILTIN_PSRLWI256:
    case IX86_BUILTIN_PSRLWI256_MASK:
    case IX86_BUILTIN_PSRLWI512:
      rcode = LSHIFTRT;
      is_vshift = false;
      goto do_shift;
    case IX86_BUILTIN_PSLLVV16HI:
    case IX86_BUILTIN_PSLLVV16SI:
    case IX86_BUILTIN_PSLLVV2DI:
    case IX86_BUILTIN_PSLLVV2DI_MASK:
    case IX86_BUILTIN_PSLLVV32HI:
    case IX86_BUILTIN_PSLLVV4DI:
    case IX86_BUILTIN_PSLLVV4DI_MASK:
    case IX86_BUILTIN_PSLLVV4SI:
    case IX86_BUILTIN_PSLLVV4SI_MASK:
    case IX86_BUILTIN_PSLLVV8DI:
    case IX86_BUILTIN_PSLLVV8HI:
    case IX86_BUILTIN_PSLLVV8SI:
    case IX86_BUILTIN_PSLLVV8SI_MASK:
      rcode = ASHIFT;
      is_vshift = true;
      goto do_shift;
    case IX86_BUILTIN_PSRAVQ128:
    case IX86_BUILTIN_PSRAVQ256:
    case IX86_BUILTIN_PSRAVV16HI:
    case IX86_BUILTIN_PSRAVV16SI:
    case IX86_BUILTIN_PSRAVV32HI:
    case IX86_BUILTIN_PSRAVV4SI:
    case IX86_BUILTIN_PSRAVV4SI_MASK:
    case IX86_BUILTIN_PSRAVV8DI:
    case IX86_BUILTIN_PSRAVV8HI:
    case IX86_BUILTIN_PSRAVV8SI:
    case IX86_BUILTIN_PSRAVV8SI_MASK:
      rcode = ASHIFTRT;
      is_vshift = true;
      goto do_shift;
    case IX86_BUILTIN_PSRLVV16HI:
    case IX86_BUILTIN_PSRLVV16SI:
    case IX86_BUILTIN_PSRLVV2DI:
    case IX86_BUILTIN_PSRLVV2DI_MASK:
    case IX86_BUILTIN_PSRLVV32HI:
    case IX86_BUILTIN_PSRLVV4DI:
    case IX86_BUILTIN_PSRLVV4DI_MASK:
    case IX86_BUILTIN_PSRLVV4SI:
    case IX86_BUILTIN_PSRLVV4SI_MASK:
    case IX86_BUILTIN_PSRLVV8DI:
    case IX86_BUILTIN_PSRLVV8HI:
    case IX86_BUILTIN_PSRLVV8SI:
    case IX86_BUILTIN_PSRLVV8SI_MASK:
      rcode = LSHIFTRT;
      is_vshift = true;
      goto do_shift;

    do_shift:
      gcc_assert (n_args >= 2);
      if (!gimple_call_lhs (stmt))
	{
	  gsi_replace (gsi, gimple_build_nop (), false);
	  return true;
	}
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      elems = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
      /* For masked shift, only optimize if the mask is all ones.  */
      if (n_args > 2
	  && !ix86_masked_all_ones (elems, gimple_call_arg (stmt, n_args - 1)))
	break;
      if (is_vshift)
	{
	  if (TREE_CODE (arg1) != VECTOR_CST)
	    break;
	  count = TYPE_PRECISION (TREE_TYPE (TREE_TYPE (arg0)));
	  if (integer_zerop (arg1))
	    count = 0;
	  else if (rcode == ASHIFTRT)
	    break;
	  else
	    for (unsigned int i = 0; i < VECTOR_CST_NELTS (arg1); ++i)
	      {
		tree elt = VECTOR_CST_ELT (arg1, i);
		if (!wi::neg_p (wi::to_wide (elt))
		    && wi::to_widest (elt) < count)
		  return false;
	      }
	}
      else
	{
	  arg1 = ix86_vector_shift_count (arg1);
	  if (!arg1)
	    break;
	  count = tree_to_uhwi (arg1);
	}
      if (count == 0)
	{
	  /* Just return the first argument for shift by 0.  */
	  loc = gimple_location (stmt);
	  g = gimple_build_assign (gimple_call_lhs (stmt), arg0);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      if (rcode != ASHIFTRT
	  && count >= TYPE_PRECISION (TREE_TYPE (TREE_TYPE (arg0))))
	{
	  /* For shift counts equal or greater than precision, except for
	     arithmetic right shift the result is zero.  */
	  loc = gimple_location (stmt);
	  g = gimple_build_assign (gimple_call_lhs (stmt),
				   build_zero_cst (TREE_TYPE (arg0)));
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      break;

    case IX86_BUILTIN_SHUFPD512:
    case IX86_BUILTIN_SHUFPS512:
    case IX86_BUILTIN_SHUFPD:
    case IX86_BUILTIN_SHUFPD256:
    case IX86_BUILTIN_SHUFPS:
    case IX86_BUILTIN_SHUFPS256:
      arg0 = gimple_call_arg (stmt, 0);
      elems = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
      /* This is masked shuffle.  Only optimize if the mask is all ones.  */
      if (n_args > 3
	  && !ix86_masked_all_ones (elems,
				    gimple_call_arg (stmt, n_args - 1)))
	break;
      arg2 = gimple_call_arg (stmt, 2);
      if (TREE_CODE (arg2) == INTEGER_CST && gimple_call_lhs (stmt))
	{
	  unsigned HOST_WIDE_INT shuffle_mask = TREE_INT_CST_LOW (arg2);
	  /* Check valid imm, refer to gcc.target/i386/testimm-10.c.  */
	  if (shuffle_mask > 255)
	    return false;

	  machine_mode imode = GET_MODE_INNER (TYPE_MODE (TREE_TYPE (arg0)));
	  loc = gimple_location (stmt);
	  tree itype = (imode == E_DFmode
			? long_long_integer_type_node : integer_type_node);
	  tree vtype = build_vector_type (itype, elems);
	  tree_vector_builder elts (vtype, elems, 1);


	  /* Transform integer shuffle_mask to vector perm_mask which
	     is used by vec_perm_expr, refer to shuflp[sd]256/512 in sse.md.  */
	  for (unsigned i = 0; i != elems; i++)
	    {
	      unsigned sel_idx;
	      /* Imm[1:0](if VL > 128, then use Imm[3:2],Imm[5:4],Imm[7:6])
		 provide 2 select constrols for each element of the
		 destination.  */
	      if (imode == E_DFmode)
		sel_idx = (i & 1) * elems + (i & ~1)
			  + ((shuffle_mask >> i) & 1);
	      else
		{
		  /* Imm[7:0](if VL > 128, also use Imm[7:0]) provide 4 select
		     controls for each element of the destination.  */
		  unsigned j = i % 4;
		  sel_idx = ((i >> 1) & 1) * elems + (i & ~3)
			    + ((shuffle_mask >> 2 * j) & 3);
		}
	      elts.quick_push (build_int_cst (itype, sel_idx));
	    }

	  tree perm_mask = elts.build ();
	  arg1 = gimple_call_arg (stmt, 1);
	  g = gimple_build_assign (gimple_call_lhs (stmt),
				   VEC_PERM_EXPR,
				   arg0, arg1, perm_mask);
	  gimple_set_location (g, loc);
	  gsi_replace (gsi, g, false);
	  return true;
	}
      // Do not error yet, the constant could be propagated later?
      break;

    case IX86_BUILTIN_PABSB:
    case IX86_BUILTIN_PABSW:
    case IX86_BUILTIN_PABSD:
      /* 64-bit vector abs<mode>2 is only supported under TARGET_MMX_WITH_SSE.  */
      if (!TARGET_MMX_WITH_SSE)
	break;
      /* FALLTHRU.  */
    case IX86_BUILTIN_PABSB128:
    case IX86_BUILTIN_PABSB256:
    case IX86_BUILTIN_PABSB512:
    case IX86_BUILTIN_PABSW128:
    case IX86_BUILTIN_PABSW256:
    case IX86_BUILTIN_PABSW512:
    case IX86_BUILTIN_PABSD128:
    case IX86_BUILTIN_PABSD256:
    case IX86_BUILTIN_PABSD512:
    case IX86_BUILTIN_PABSQ128:
    case IX86_BUILTIN_PABSQ256:
    case IX86_BUILTIN_PABSQ512:
    case IX86_BUILTIN_PABSB128_MASK:
    case IX86_BUILTIN_PABSB256_MASK:
    case IX86_BUILTIN_PABSW128_MASK:
    case IX86_BUILTIN_PABSW256_MASK:
    case IX86_BUILTIN_PABSD128_MASK:
    case IX86_BUILTIN_PABSD256_MASK:
      gcc_assert (n_args >= 1);
      if (!gimple_call_lhs (stmt))
	{
	  gsi_replace (gsi, gimple_build_nop (), false);
	  return true;
	}
      arg0 = gimple_call_arg (stmt, 0);
      elems = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
      /* For masked ABS, only optimize if the mask is all ones.  */
      if (n_args > 1
	  && !ix86_masked_all_ones (elems, gimple_call_arg (stmt, n_args - 1)))
	break;
      {
	tree utype, ures, vce;
	utype = unsigned_type_for (TREE_TYPE (arg0));
	/* PABSB/W/D/Q store the unsigned result in dst, use ABSU_EXPR
	   instead of ABS_EXPR to hanlde overflow case(TYPE_MIN).  */
	ures = gimple_build (&stmts, ABSU_EXPR, utype, arg0);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	loc = gimple_location (stmt);
	vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (arg0), ures);
	g = gimple_build_assign (gimple_call_lhs (stmt),
				 VIEW_CONVERT_EXPR, vce);
	gsi_replace (gsi, g, false);
      }
      return true;

    default:
      break;
    }

  return false;
}

/* Handler for an SVML-style interface to
   a library with vectorized intrinsics.  */

tree
ix86_veclibabi_svml (combined_fn fn, tree type_out, tree type_in)
{
  char name[20];
  tree fntype, new_fndecl, args;
  unsigned arity;
  const char *bname;
  machine_mode el_mode, in_mode;
  int n, in_n;

  /* The SVML is suitable for unsafe math only.  */
  if (!flag_unsafe_math_optimizations)
    return NULL_TREE;

  el_mode = TYPE_MODE (TREE_TYPE (type_out));
  n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);
  if (el_mode != in_mode
      || n != in_n)
    return NULL_TREE;

  switch (fn)
    {
    CASE_CFN_EXP:
    CASE_CFN_LOG:
    CASE_CFN_LOG10:
    CASE_CFN_POW:
    CASE_CFN_TANH:
    CASE_CFN_TAN:
    CASE_CFN_ATAN:
    CASE_CFN_ATAN2:
    CASE_CFN_ATANH:
    CASE_CFN_CBRT:
    CASE_CFN_SINH:
    CASE_CFN_SIN:
    CASE_CFN_ASINH:
    CASE_CFN_ASIN:
    CASE_CFN_COSH:
    CASE_CFN_COS:
    CASE_CFN_ACOSH:
    CASE_CFN_ACOS:
      if ((el_mode != DFmode || n != 2)
	  && (el_mode != SFmode || n != 4))
	return NULL_TREE;
      break;

    default:
      return NULL_TREE;
    }

  tree fndecl = mathfn_built_in (el_mode == DFmode
				 ? double_type_node : float_type_node, fn);
  bname = IDENTIFIER_POINTER (DECL_NAME (fndecl));

  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_LOGF)
    strcpy (name, "vmlsLn4");
  else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_LOG)
    strcpy (name, "vmldLn2");
  else if (n == 4)
    {
      sprintf (name, "vmls%s", bname+10);
      name[strlen (name)-1] = '4';
    }
  else
    sprintf (name, "vmld%s2", bname+10);

  /* Convert to uppercase. */
  name[4] &= ~0x20;

  arity = 0;
  for (args = DECL_ARGUMENTS (fndecl); args; args = TREE_CHAIN (args))
    arity++;

  if (arity == 1)
    fntype = build_function_type_list (type_out, type_in, NULL);
  else
    fntype = build_function_type_list (type_out, type_in, type_in, NULL);

  /* Build a function declaration for the vectorized function.  */
  new_fndecl = build_decl (BUILTINS_LOCATION,
			   FUNCTION_DECL, get_identifier (name), fntype);
  TREE_PUBLIC (new_fndecl) = 1;
  DECL_EXTERNAL (new_fndecl) = 1;
  DECL_IS_NOVOPS (new_fndecl) = 1;
  TREE_READONLY (new_fndecl) = 1;

  return new_fndecl;
}

/* Handler for an ACML-style interface to
   a library with vectorized intrinsics.  */

tree
ix86_veclibabi_acml (combined_fn fn, tree type_out, tree type_in)
{
  char name[20] = "__vr.._";
  tree fntype, new_fndecl, args;
  unsigned arity;
  const char *bname;
  machine_mode el_mode, in_mode;
  int n, in_n;

  /* The ACML is 64bits only and suitable for unsafe math only as
     it does not correctly support parts of IEEE with the required
     precision such as denormals.  */
  if (!TARGET_64BIT
      || !flag_unsafe_math_optimizations)
    return NULL_TREE;

  el_mode = TYPE_MODE (TREE_TYPE (type_out));
  n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);
  if (el_mode != in_mode
      || n != in_n)
    return NULL_TREE;

  switch (fn)
    {
    CASE_CFN_SIN:
    CASE_CFN_COS:
    CASE_CFN_EXP:
    CASE_CFN_LOG:
    CASE_CFN_LOG2:
    CASE_CFN_LOG10:
      if (el_mode == DFmode && n == 2)
	{
	  name[4] = 'd';
	  name[5] = '2';
	}
      else if (el_mode == SFmode && n == 4)
	{
	  name[4] = 's';
	  name[5] = '4';
	}
      else
	return NULL_TREE;
      break;

    default:
      return NULL_TREE;
    }

  tree fndecl = mathfn_built_in (el_mode == DFmode
				 ? double_type_node : float_type_node, fn);
  bname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  sprintf (name + 7, "%s", bname+10);

  arity = 0;
  for (args = DECL_ARGUMENTS (fndecl); args; args = TREE_CHAIN (args))
    arity++;

  if (arity == 1)
    fntype = build_function_type_list (type_out, type_in, NULL);
  else
    fntype = build_function_type_list (type_out, type_in, type_in, NULL);

  /* Build a function declaration for the vectorized function.  */
  new_fndecl = build_decl (BUILTINS_LOCATION,
			   FUNCTION_DECL, get_identifier (name), fntype);
  TREE_PUBLIC (new_fndecl) = 1;
  DECL_EXTERNAL (new_fndecl) = 1;
  DECL_IS_NOVOPS (new_fndecl) = 1;
  TREE_READONLY (new_fndecl) = 1;

  return new_fndecl;
}

/* Returns a decl of a function that implements scatter store with
   register type VECTYPE and index type INDEX_TYPE and SCALE.
   Return NULL_TREE if it is not available.  */

static tree
ix86_vectorize_builtin_scatter (const_tree vectype,
				const_tree index_type, int scale)
{
  bool si;
  enum ix86_builtins code;
  const machine_mode mode = TYPE_MODE (TREE_TYPE (vectype));

  if (!TARGET_AVX512F)
    return NULL_TREE;

  if (!TARGET_EVEX512 && GET_MODE_SIZE (mode) == 64)
    return NULL_TREE;

  if (known_eq (TYPE_VECTOR_SUBPARTS (vectype), 2u)
      ? !TARGET_USE_SCATTER_2PARTS
      : (known_eq (TYPE_VECTOR_SUBPARTS (vectype), 4u)
	 ? !TARGET_USE_SCATTER_4PARTS
	 : !TARGET_USE_SCATTER_8PARTS))
    return NULL_TREE;

  if ((TREE_CODE (index_type) != INTEGER_TYPE
       && !POINTER_TYPE_P (index_type))
      || (TYPE_MODE (index_type) != SImode
	  && TYPE_MODE (index_type) != DImode))
    return NULL_TREE;

  if (TYPE_PRECISION (index_type) > POINTER_SIZE)
    return NULL_TREE;

  /* v*scatter* insn sign extends index to pointer mode.  */
  if (TYPE_PRECISION (index_type) < POINTER_SIZE
      && TYPE_UNSIGNED (index_type))
    return NULL_TREE;

  /* Scale can be 1, 2, 4 or 8.  */
  if (scale <= 0
      || scale > 8
      || (scale & (scale - 1)) != 0)
    return NULL_TREE;

  si = TYPE_MODE (index_type) == SImode;
  switch (TYPE_MODE (vectype))
    {
    case E_V8DFmode:
      code = si ? IX86_BUILTIN_SCATTERALTSIV8DF : IX86_BUILTIN_SCATTERDIV8DF;
      break;
    case E_V8DImode:
      code = si ? IX86_BUILTIN_SCATTERALTSIV8DI : IX86_BUILTIN_SCATTERDIV8DI;
      break;
    case E_V16SFmode:
      code = si ? IX86_BUILTIN_SCATTERSIV16SF : IX86_BUILTIN_SCATTERALTDIV16SF;
      break;
    case E_V16SImode:
      code = si ? IX86_BUILTIN_SCATTERSIV16SI : IX86_BUILTIN_SCATTERALTDIV16SI;
      break;
    case E_V4DFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERALTSIV4DF : IX86_BUILTIN_SCATTERDIV4DF;
      else
	return NULL_TREE;
      break;
    case E_V4DImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERALTSIV4DI : IX86_BUILTIN_SCATTERDIV4DI;
      else
	return NULL_TREE;
      break;
    case E_V8SFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERSIV8SF : IX86_BUILTIN_SCATTERALTDIV8SF;
      else
	return NULL_TREE;
      break;
    case E_V8SImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERSIV8SI : IX86_BUILTIN_SCATTERALTDIV8SI;
      else
	return NULL_TREE;
      break;
    case E_V2DFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERALTSIV2DF : IX86_BUILTIN_SCATTERDIV2DF;
      else
	return NULL_TREE;
      break;
    case E_V2DImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERALTSIV2DI : IX86_BUILTIN_SCATTERDIV2DI;
      else
	return NULL_TREE;
      break;
    case E_V4SFmode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERSIV4SF : IX86_BUILTIN_SCATTERALTDIV4SF;
      else
	return NULL_TREE;
      break;
    case E_V4SImode:
      if (TARGET_AVX512VL)
	code = si ? IX86_BUILTIN_SCATTERSIV4SI : IX86_BUILTIN_SCATTERALTDIV4SI;
      else
	return NULL_TREE;
      break;
    default:
      return NULL_TREE;
    }

  return get_ix86_builtin (code);
}

/* Return true if it is safe to use the rsqrt optabs to optimize
   1.0/sqrt.  */

static bool
use_rsqrt_p (machine_mode mode)
{
  return ((mode == HFmode
	   || (TARGET_SSE && TARGET_SSE_MATH))
	  && flag_finite_math_only
	  && !flag_trapping_math
	  && flag_unsafe_math_optimizations);
}

/* Helper for avx_vpermilps256_operand et al.  This is also used by
   the expansion functions to turn the parallel back into a mask.
   The return value is 0 for no match and the imm8+1 for a match.  */

int
avx_vpermilp_parallel (rtx par, machine_mode mode)
{
  unsigned i, nelt = GET_MODE_NUNITS (mode);
  unsigned mask = 0;
  unsigned char ipar[16] = {};  /* Silence -Wuninitialized warning.  */

  if (XVECLEN (par, 0) != (int) nelt)
    return 0;

  /* Validate that all of the elements are constants, and not totally
     out of range.  Copy the data into an integral array to make the
     subsequent checks easier.  */
  for (i = 0; i < nelt; ++i)
    {
      rtx er = XVECEXP (par, 0, i);
      unsigned HOST_WIDE_INT ei;

      if (!CONST_INT_P (er))
	return 0;
      ei = INTVAL (er);
      if (ei >= nelt)
	return 0;
      ipar[i] = ei;
    }

  switch (mode)
    {
    case E_V8DFmode:
      /* In the 512-bit DFmode case, we can only move elements within
         a 128-bit lane.  First fill the second part of the mask,
	 then fallthru.  */
      for (i = 4; i < 6; ++i)
	{
	  if (ipar[i] < 4 || ipar[i] >= 6)
	    return 0;
	  mask |= (ipar[i] - 4) << i;
	}
      for (i = 6; i < 8; ++i)
	{
	  if (ipar[i] < 6)
	    return 0;
	  mask |= (ipar[i] - 6) << i;
	}
      /* FALLTHRU */

    case E_V4DFmode:
      /* In the 256-bit DFmode case, we can only move elements within
         a 128-bit lane.  */
      for (i = 0; i < 2; ++i)
	{
	  if (ipar[i] >= 2)
	    return 0;
	  mask |= ipar[i] << i;
	}
      for (i = 2; i < 4; ++i)
	{
	  if (ipar[i] < 2)
	    return 0;
	  mask |= (ipar[i] - 2) << i;
	}
      break;

    case E_V16SFmode:
      /* In 512 bit SFmode case, permutation in the upper 256 bits
	 must mirror the permutation in the lower 256-bits.  */
      for (i = 0; i < 8; ++i)
	if (ipar[i] + 8 != ipar[i + 8])
	  return 0;
      /* FALLTHRU */

    case E_V8SFmode:
      /* In 256 bit SFmode case, we have full freedom of
         movement within the low 128-bit lane, but the high 128-bit
         lane must mirror the exact same pattern.  */
      for (i = 0; i < 4; ++i)
	if (ipar[i] + 4 != ipar[i + 4])
	  return 0;
      nelt = 4;
      /* FALLTHRU */

    case E_V2DFmode:
    case E_V4SFmode:
      /* In the 128-bit case, we've full freedom in the placement of
	 the elements from the source operand.  */
      for (i = 0; i < nelt; ++i)
	mask |= ipar[i] << (i * (nelt / 2));
      break;

    default:
      gcc_unreachable ();
    }

  /* Make sure success has a non-zero value by adding one.  */
  return mask + 1;
}

/* Helper for avx_vperm2f128_v4df_operand et al.  This is also used by
   the expansion functions to turn the parallel back into a mask.
   The return value is 0 for no match and the imm8+1 for a match.  */

int
avx_vperm2f128_parallel (rtx par, machine_mode mode)
{
  unsigned i, nelt = GET_MODE_NUNITS (mode), nelt2 = nelt / 2;
  unsigned mask = 0;
  unsigned char ipar[8] = {};  /* Silence -Wuninitialized warning.  */

  if (XVECLEN (par, 0) != (int) nelt)
    return 0;

  /* Validate that all of the elements are constants, and not totally
     out of range.  Copy the data into an integral array to make the
     subsequent checks easier.  */
  for (i = 0; i < nelt; ++i)
    {
      rtx er = XVECEXP (par, 0, i);
      unsigned HOST_WIDE_INT ei;

      if (!CONST_INT_P (er))
	return 0;
      ei = INTVAL (er);
      if (ei >= 2 * nelt)
	return 0;
      ipar[i] = ei;
    }

  /* Validate that the halves of the permute are halves.  */
  for (i = 0; i < nelt2 - 1; ++i)
    if (ipar[i] + 1 != ipar[i + 1])
      return 0;
  for (i = nelt2; i < nelt - 1; ++i)
    if (ipar[i] + 1 != ipar[i + 1])
      return 0;

  /* Reconstruct the mask.  */
  for (i = 0; i < 2; ++i)
    {
      unsigned e = ipar[i * nelt2];
      if (e % nelt2)
	return 0;
      e /= nelt2;
      mask |= e << (i * 4);
    }

  /* Make sure success has a non-zero value by adding one.  */
  return mask + 1;
}

/* Return a mask of VPTERNLOG operands that do not affect output.  */

int
vpternlog_redundant_operand_mask (rtx pternlog_imm)
{
  int mask = 0;
  int imm8 = INTVAL (pternlog_imm);

  if (((imm8 >> 4) & 0x0F) == (imm8 & 0x0F))
    mask |= 1;
  if (((imm8 >> 2) & 0x33) == (imm8 & 0x33))
    mask |= 2;
  if (((imm8 >> 1) & 0x55) == (imm8 & 0x55))
    mask |= 4;

  return mask;
}

/* Eliminate false dependencies on operands that do not affect output
   by substituting other operands of a VPTERNLOG.  */

void
substitute_vpternlog_operands (rtx *operands)
{
  int mask = vpternlog_redundant_operand_mask (operands[4]);

  if (mask & 1) /* The first operand is redundant.  */
    operands[1] = operands[2];

  if (mask & 2) /* The second operand is redundant.  */
    operands[2] = operands[1];

  if (mask & 4) /* The third operand is redundant.  */
    operands[3] = operands[1];
  else if (REG_P (operands[3]))
    {
      if (mask & 1)
	operands[1] = operands[3];
      if (mask & 2)
	operands[2] = operands[3];
    }
}

/* Return a register priority for hard reg REGNO.  */
static int
ix86_register_priority (int hard_regno)
{
  /* ebp and r13 as the base always wants a displacement, r12 as the
     base always wants an index.  So discourage their usage in an
     address.  */
  if (hard_regno == R12_REG || hard_regno == R13_REG)
    return 0;
  if (hard_regno == BP_REG)
    return 1;
  /* New x86-64 int registers result in bigger code size.  Discourage them.  */
  if (REX_INT_REGNO_P (hard_regno))
    return 2;
  if (REX2_INT_REGNO_P (hard_regno))
    return 2;
  /* New x86-64 SSE registers result in bigger code size.  Discourage them.  */
  if (REX_SSE_REGNO_P (hard_regno))
    return 2;
  if (EXT_REX_SSE_REGNO_P (hard_regno))
    return 1;
  /* Usage of AX register results in smaller code.  Prefer it.  */
  if (hard_regno == AX_REG)
    return 4;
  return 3;
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.

   Put float CONST_DOUBLE in the constant pool instead of fp regs.
   QImode must go into class Q_REGS.
   Narrow ALL_REGS to GENERAL_REGS.  This supports allowing movsf and
   movdf to do mem-to-mem moves through integer regs.  */

static reg_class_t
ix86_preferred_reload_class (rtx x, reg_class_t regclass)
{
  machine_mode mode = GET_MODE (x);

  /* We're only allowed to return a subclass of CLASS.  Many of the
     following checks fail for NO_REGS, so eliminate that early.  */
  if (regclass == NO_REGS)
    return NO_REGS;

  /* All classes can load zeros.  */
  if (x == CONST0_RTX (mode))
    return regclass;

  /* Force constants into memory if we are loading a (nonzero) constant into
     an MMX, SSE or MASK register.  This is because there are no MMX/SSE/MASK
     instructions to load from a constant.  */
  if (CONSTANT_P (x)
      && (MAYBE_MMX_CLASS_P (regclass)
	  || MAYBE_SSE_CLASS_P (regclass)
	  || MAYBE_MASK_CLASS_P (regclass)))
    return NO_REGS;

  /* Floating-point constants need more complex checks.  */
  if (CONST_DOUBLE_P (x))
    {
      /* General regs can load everything.  */
      if (INTEGER_CLASS_P (regclass))
        return regclass;

      /* Floats can load 0 and 1 plus some others.  Note that we eliminated
	 zero above.  We only want to wind up preferring 80387 registers if
	 we plan on doing computation with them.  */
      if (IS_STACK_MODE (mode)
	  && standard_80387_constant_p (x) > 0)
	{
	  /* Limit class to FP regs.  */
	  if (FLOAT_CLASS_P (regclass))
	    return FLOAT_REGS;
	}

      return NO_REGS;
    }

  /* Prefer SSE if we can use them for math.  Also allow integer regs
     when moves between register units are cheap.  */
  if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
    {
      if (TARGET_INTER_UNIT_MOVES_FROM_VEC
	  && TARGET_INTER_UNIT_MOVES_TO_VEC
	  && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (word_mode))
	return INT_SSE_CLASS_P (regclass) ? regclass : NO_REGS;
      else
	return SSE_CLASS_P (regclass) ? regclass : NO_REGS;
    }

  /* Generally when we see PLUS here, it's the function invariant
     (plus soft-fp const_int).  Which can only be computed into general
     regs.  */
  if (GET_CODE (x) == PLUS)
    return INTEGER_CLASS_P (regclass) ? regclass : NO_REGS;

  /* QImode constants are easy to load, but non-constant QImode data
     must go into Q_REGS or ALL_MASK_REGS.  */
  if (GET_MODE (x) == QImode && !CONSTANT_P (x))
    {
      if (Q_CLASS_P (regclass))
	return regclass;
      else if (reg_class_subset_p (Q_REGS, regclass))
	return Q_REGS;
      else if (MASK_CLASS_P (regclass))
	return regclass;
      else
	return NO_REGS;
    }

  return regclass;
}

/* Discourage putting floating-point values in SSE registers unless
   SSE math is being used, and likewise for the 387 registers.  */
static reg_class_t
ix86_preferred_output_reload_class (rtx x, reg_class_t regclass)
{
  /* Restrict the output reload class to the register bank that we are doing
     math on.  If we would like not to return a subset of CLASS, reject this
     alternative: if reload cannot do this, it will still use its choice.  */
  machine_mode mode = GET_MODE (x);
  if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
    return MAYBE_SSE_CLASS_P (regclass) ? ALL_SSE_REGS : NO_REGS;

  if (IS_STACK_MODE (mode))
    return FLOAT_CLASS_P (regclass) ? regclass : NO_REGS;

  return regclass;
}

static reg_class_t
ix86_secondary_reload (bool in_p, rtx x, reg_class_t rclass,
		       machine_mode mode, secondary_reload_info *sri)
{
  /* Double-word spills from general registers to non-offsettable memory
     references (zero-extended addresses) require special handling.  */
  if (TARGET_64BIT
      && MEM_P (x)
      && GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && INTEGER_CLASS_P (rclass)
      && !offsettable_memref_p (x))
    {
      sri->icode = (in_p
		    ? CODE_FOR_reload_noff_load
		    : CODE_FOR_reload_noff_store);
      /* Add the cost of moving address to a temporary.  */
      sri->extra_cost = 1;

      return NO_REGS;
    }

  /* QImode spills from non-QI registers require
     intermediate register on 32bit targets.  */
  if (mode == QImode
      && ((!TARGET_64BIT && !in_p
	   && INTEGER_CLASS_P (rclass)
	   && MAYBE_NON_Q_CLASS_P (rclass))
	  || (!TARGET_AVX512DQ
	      && MAYBE_MASK_CLASS_P (rclass))))
    {
      int regno = true_regnum (x);

      /* Return Q_REGS if the operand is in memory.  */
      if (regno == -1)
	return Q_REGS;

      return NO_REGS;
    }

  /* Require movement to gpr, and then store to memory.  */
  if ((mode == HFmode || mode == HImode || mode == V2QImode
       || mode == BFmode)
      && !TARGET_SSE4_1
      && SSE_CLASS_P (rclass)
      && !in_p && MEM_P (x))
    {
      sri->extra_cost = 1;
      return GENERAL_REGS;
    }

  /* This condition handles corner case where an expression involving
     pointers gets vectorized.  We're trying to use the address of a
     stack slot as a vector initializer.

     (set (reg:V2DI 74 [ vect_cst_.2 ])
          (vec_duplicate:V2DI (reg/f:DI 20 frame)))

     Eventually frame gets turned into sp+offset like this:

     (set (reg:V2DI 21 xmm0 [orig:74 vect_cst_.2 ] [74])
          (vec_duplicate:V2DI (plus:DI (reg/f:DI 7 sp)
	                               (const_int 392 [0x188]))))

     That later gets turned into:

     (set (reg:V2DI 21 xmm0 [orig:74 vect_cst_.2 ] [74])
          (vec_duplicate:V2DI (plus:DI (reg/f:DI 7 sp)
	    (mem/u/c/i:DI (symbol_ref/u:DI ("*.LC0") [flags 0x2]) [0 S8 A64]))))

     We'll have the following reload recorded:

     Reload 0: reload_in (DI) =
           (plus:DI (reg/f:DI 7 sp)
            (mem/u/c/i:DI (symbol_ref/u:DI ("*.LC0") [flags 0x2]) [0 S8 A64]))
     reload_out (V2DI) = (reg:V2DI 21 xmm0 [orig:74 vect_cst_.2 ] [74])
     SSE_REGS, RELOAD_OTHER (opnum = 0), can't combine
     reload_in_reg: (plus:DI (reg/f:DI 7 sp) (const_int 392 [0x188]))
     reload_out_reg: (reg:V2DI 21 xmm0 [orig:74 vect_cst_.2 ] [74])
     reload_reg_rtx: (reg:V2DI 22 xmm1)

     Which isn't going to work since SSE instructions can't handle scalar
     additions.  Returning GENERAL_REGS forces the addition into integer
     register and reload can handle subsequent reloads without problems.  */

  if (in_p && GET_CODE (x) == PLUS
      && SSE_CLASS_P (rclass)
      && SCALAR_INT_MODE_P (mode))
    return GENERAL_REGS;

  return NO_REGS;
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.  */

static bool
ix86_class_likely_spilled_p (reg_class_t rclass)
{
  switch (rclass)
    {
      case AREG:
      case DREG:
      case CREG:
      case BREG:
      case AD_REGS:
      case SIREG:
      case DIREG:
      case SSE_FIRST_REG:
      case FP_TOP_REG:
      case FP_SECOND_REG:
	return true;

      default:
	break;
    }

  return false;
}

/* Return true if a set of DST by the expression SRC should be allowed.
   This prevents complex sets of likely_spilled hard regs before reload.  */

bool
ix86_hardreg_mov_ok (rtx dst, rtx src)
{
  /* Avoid complex sets of likely_spilled hard registers before reload.  */
  if (REG_P (dst) && HARD_REGISTER_P (dst)
      && !REG_P (src) && !MEM_P (src)
      && !(VECTOR_MODE_P (GET_MODE (dst))
	   ? standard_sse_constant_p (src, GET_MODE (dst))
	   : x86_64_immediate_operand (src, GET_MODE (dst)))
      && ix86_class_likely_spilled_p (REGNO_REG_CLASS (REGNO (dst)))
      && !reload_completed)
    return false;
  return true;
}

/* If we are copying between registers from different register sets
   (e.g. FP and integer), we may need a memory location.

   The function can't work reliably when one of the CLASSES is a class
   containing registers from multiple sets.  We avoid this by never combining
   different sets in a single alternative in the machine description.
   Ensure that this constraint holds to avoid unexpected surprises.

   When STRICT is false, we are being called from REGISTER_MOVE_COST,
   so do not enforce these sanity checks.

   To optimize register_move_cost performance, define inline variant.  */

static inline bool
inline_secondary_memory_needed (machine_mode mode, reg_class_t class1,
				reg_class_t class2, int strict)
{
  if (lra_in_progress && (class1 == NO_REGS || class2 == NO_REGS))
    return false;

  if (MAYBE_FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class1)
      || MAYBE_FLOAT_CLASS_P (class2) != FLOAT_CLASS_P (class2)
      || MAYBE_SSE_CLASS_P (class1) != SSE_CLASS_P (class1)
      || MAYBE_SSE_CLASS_P (class2) != SSE_CLASS_P (class2)
      || MAYBE_MMX_CLASS_P (class1) != MMX_CLASS_P (class1)
      || MAYBE_MMX_CLASS_P (class2) != MMX_CLASS_P (class2)
      || MAYBE_MASK_CLASS_P (class1) != MASK_CLASS_P (class1)
      || MAYBE_MASK_CLASS_P (class2) != MASK_CLASS_P (class2))
    {
      gcc_assert (!strict || lra_in_progress);
      return true;
    }

  if (FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class2))
    return true;

  /* ??? This is a lie.  We do have moves between mmx/general, and for
     mmx/sse2.  But by saying we need secondary memory we discourage the
     register allocator from using the mmx registers unless needed.  */
  if (MMX_CLASS_P (class1) != MMX_CLASS_P (class2))
    return true;

  /* Between mask and general, we have moves no larger than word size.  */
  if (MASK_CLASS_P (class1) != MASK_CLASS_P (class2))
    {
      if (!(INTEGER_CLASS_P (class1) || INTEGER_CLASS_P (class2))
	  || GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return true;
    }

  if (SSE_CLASS_P (class1) != SSE_CLASS_P (class2))
    {
      /* SSE1 doesn't have any direct moves from other classes.  */
      if (!TARGET_SSE2)
	return true;

      if (!(INTEGER_CLASS_P (class1) || INTEGER_CLASS_P (class2)))
	return true;

      int msize = GET_MODE_SIZE (mode);

      /* Between SSE and general, we have moves no larger than word size.  */
      if (msize > UNITS_PER_WORD)
	return true;

      /* In addition to SImode moves, HImode moves are supported for SSE2 and above,
	 Use vmovw with AVX512FP16, or pinsrw/pextrw without AVX512FP16.  */
      int minsize = GET_MODE_SIZE (TARGET_SSE2 ? HImode : SImode);

      if (msize < minsize)
	return true;

      /* If the target says that inter-unit moves are more expensive
	 than moving through memory, then don't generate them.  */
      if ((SSE_CLASS_P (class1) && !TARGET_INTER_UNIT_MOVES_FROM_VEC)
	  || (SSE_CLASS_P (class2) && !TARGET_INTER_UNIT_MOVES_TO_VEC))
	return true;
    }

  return false;
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED.  */

static bool
ix86_secondary_memory_needed (machine_mode mode, reg_class_t class1,
			      reg_class_t class2)
{
  return inline_secondary_memory_needed (mode, class1, class2, true);
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED_MODE.

   get_secondary_mem widens integral modes to BITS_PER_WORD.
   There is no need to emit full 64 bit move on 64 bit targets
   for integral modes that can be moved using 32 bit move.  */

static machine_mode
ix86_secondary_memory_needed_mode (machine_mode mode)
{
  if (GET_MODE_BITSIZE (mode) < 32 && INTEGRAL_MODE_P (mode))
    return mode_for_size (32, GET_MODE_CLASS (mode), 0).require ();
  return mode;
}

/* Implement the TARGET_CLASS_MAX_NREGS hook.

   On the 80386, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */

static unsigned char
ix86_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  if (MAYBE_INTEGER_CLASS_P (rclass))
    {
      if (mode == XFmode)
	return (TARGET_64BIT ? 2 : 3);
      else if (mode == XCmode)
	return (TARGET_64BIT ? 4 : 6);
      else
	return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
    }
  else
    {
      if (COMPLEX_MODE_P (mode))
	return 2;
      else
	return 1;
    }
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
ix86_can_change_mode_class (machine_mode from, machine_mode to,
			    reg_class_t regclass)
{
  if (from == to)
    return true;

  /* x87 registers can't do subreg at all, as all values are reformatted
     to extended precision.  */
  if (MAYBE_FLOAT_CLASS_P (regclass))
    return false;

  if (MAYBE_SSE_CLASS_P (regclass) || MAYBE_MMX_CLASS_P (regclass))
    {
      /* Vector registers do not support QI or HImode loads.  If we don't
	 disallow a change to these modes, reload will assume it's ok to
	 drop the subreg from (subreg:SI (reg:HI 100) 0).  This affects
	 the vec_dupv4hi pattern.
	 NB: SSE2 can load 16bit data to sse register via pinsrw.  */
      int mov_size = MAYBE_SSE_CLASS_P (regclass) && TARGET_SSE2 ? 2 : 4;
      if (GET_MODE_SIZE (from) < mov_size
	  || GET_MODE_SIZE (to) < mov_size)
	return false;
    }

  return true;
}

/* Return index of MODE in the sse load/store tables.  */

static inline int
sse_store_index (machine_mode mode)
{
  /* NB: Use SFmode cost for HFmode instead of adding HFmode load/store
     costs to processor_costs, which requires changes to all entries in
     processor cost table.  */
  if (mode == E_HFmode)
    mode = E_SFmode;

  switch (GET_MODE_SIZE (mode))
    {
    case 4:
      return 0;
    case 8:
      return 1;
    case 16:
      return 2;
    case 32:
      return 3;
    case 64:
      return 4;
    default:
      return -1;
    }
}

/* Return the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   This function is used extensively by register_move_cost that is used to
   build tables at startup.  Make it inline in this case.
   When IN is 2, return maximum of in and out move cost.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.

   Model also increased moving costs of QImode registers in non
   Q_REGS classes.
 */
static inline int
inline_memory_move_cost (machine_mode mode, enum reg_class regclass, int in)
{
  int cost;

  if (FLOAT_CLASS_P (regclass))
    {
      int index;
      switch (mode)
	{
	  case E_SFmode:
	    index = 0;
	    break;
	  case E_DFmode:
	    index = 1;
	    break;
	  case E_XFmode:
	    index = 2;
	    break;
	  default:
	    return 100;
	}
      if (in == 2)
        return MAX (ix86_cost->hard_register.fp_load [index],
		    ix86_cost->hard_register.fp_store [index]);
      return in ? ix86_cost->hard_register.fp_load [index]
		: ix86_cost->hard_register.fp_store [index];
    }
  if (SSE_CLASS_P (regclass))
    {
      int index = sse_store_index (mode);
      if (index == -1)
	return 100;
      if (in == 2)
        return MAX (ix86_cost->hard_register.sse_load [index],
		    ix86_cost->hard_register.sse_store [index]);
      return in ? ix86_cost->hard_register.sse_load [index]
		: ix86_cost->hard_register.sse_store [index];
    }
  if (MASK_CLASS_P (regclass))
    {
      int index;
      switch (GET_MODE_SIZE (mode))
	{
	case 1:
	  index = 0;
	  break;
	case 2:
	  index = 1;
	  break;
	/* DImode loads and stores assumed to cost the same as SImode.  */
	case 4:
	case 8:
	  index = 2;
	  break;
	default:
	  return 100;
	}

      if (in == 2)
	return MAX (ix86_cost->hard_register.mask_load[index],
		    ix86_cost->hard_register.mask_store[index]);
      return in ? ix86_cost->hard_register.mask_load[2]
		: ix86_cost->hard_register.mask_store[2];
    }
  if (MMX_CLASS_P (regclass))
    {
      int index;
      switch (GET_MODE_SIZE (mode))
	{
	  case 4:
	    index = 0;
	    break;
	  case 8:
	    index = 1;
	    break;
	  default:
	    return 100;
	}
      if (in == 2)
        return MAX (ix86_cost->hard_register.mmx_load [index],
		    ix86_cost->hard_register.mmx_store [index]);
      return in ? ix86_cost->hard_register.mmx_load [index]
		: ix86_cost->hard_register.mmx_store [index];
    }
  switch (GET_MODE_SIZE (mode))
    {
      case 1:
	if (Q_CLASS_P (regclass) || TARGET_64BIT)
	  {
	    if (!in)
	      return ix86_cost->hard_register.int_store[0];
	    if (TARGET_PARTIAL_REG_DEPENDENCY
	        && optimize_function_for_speed_p (cfun))
	      cost = ix86_cost->hard_register.movzbl_load;
	    else
	      cost = ix86_cost->hard_register.int_load[0];
	    if (in == 2)
	      return MAX (cost, ix86_cost->hard_register.int_store[0]);
	    return cost;
	  }
	else
	  {
	   if (in == 2)
	     return MAX (ix86_cost->hard_register.movzbl_load,
			 ix86_cost->hard_register.int_store[0] + 4);
	   if (in)
	     return ix86_cost->hard_register.movzbl_load;
	   else
	     return ix86_cost->hard_register.int_store[0] + 4;
	  }
	break;
      case 2:
	{
	  int cost;
	  if (in == 2)
	    cost = MAX (ix86_cost->hard_register.int_load[1],
			ix86_cost->hard_register.int_store[1]);
	  else
	    cost = in ? ix86_cost->hard_register.int_load[1]
		      : ix86_cost->hard_register.int_store[1];

	  if (mode == E_HFmode)
	    {
	      /* Prefer SSE over GPR for HFmode.  */
	      int sse_cost;
	      int index = sse_store_index (mode);
	      if (in == 2)
		sse_cost = MAX (ix86_cost->hard_register.sse_load[index],
				ix86_cost->hard_register.sse_store[index]);
	      else
		sse_cost = (in
			    ? ix86_cost->hard_register.sse_load [index]
			    : ix86_cost->hard_register.sse_store [index]);
	      if (sse_cost >= cost)
		cost = sse_cost + 1;
	    }
	  return cost;
	}
      default:
	if (in == 2)
	  cost = MAX (ix86_cost->hard_register.int_load[2],
		      ix86_cost->hard_register.int_store[2]);
	else if (in)
	  cost = ix86_cost->hard_register.int_load[2];
	else
	  cost = ix86_cost->hard_register.int_store[2];
	/* Multiply with the number of GPR moves needed.  */
	return cost * CEIL ((int) GET_MODE_SIZE (mode), UNITS_PER_WORD);
    }
}

static int
ix86_memory_move_cost (machine_mode mode, reg_class_t regclass, bool in)
{
  return inline_memory_move_cost (mode, (enum reg_class) regclass, in ? 1 : 0);
}


/* Return the cost of moving data from a register in class CLASS1 to
   one in class CLASS2.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.  */

static int
ix86_register_move_cost (machine_mode mode, reg_class_t class1_i,
			 reg_class_t class2_i)
{
  enum reg_class class1 = (enum reg_class) class1_i;
  enum reg_class class2 = (enum reg_class) class2_i;

  /* In case we require secondary memory, compute cost of the store followed
     by load.  In order to avoid bad register allocation choices, we need
     for this to be *at least* as high as the symmetric MEMORY_MOVE_COST.  */

  if (inline_secondary_memory_needed (mode, class1, class2, false))
    {
      int cost = 1;

      cost += inline_memory_move_cost (mode, class1, 2);
      cost += inline_memory_move_cost (mode, class2, 2);

      /* In case of copying from general_purpose_register we may emit multiple
         stores followed by single load causing memory size mismatch stall.
         Count this as arbitrarily high cost of 20.  */
      if (GET_MODE_BITSIZE (mode) > BITS_PER_WORD
	  && TARGET_MEMORY_MISMATCH_STALL
	  && targetm.class_max_nregs (class1, mode)
	     > targetm.class_max_nregs (class2, mode))
	cost += 20;

      /* In the case of FP/MMX moves, the registers actually overlap, and we
	 have to switch modes in order to treat them differently.  */
      if ((MMX_CLASS_P (class1) && MAYBE_FLOAT_CLASS_P (class2))
          || (MMX_CLASS_P (class2) && MAYBE_FLOAT_CLASS_P (class1)))
	cost += 20;

      return cost;
    }

  /* Moves between MMX and non-MMX units require secondary memory.  */
  if (MMX_CLASS_P (class1) != MMX_CLASS_P (class2))
    gcc_unreachable ();

  if (SSE_CLASS_P (class1) != SSE_CLASS_P (class2))
    return (SSE_CLASS_P (class1)
	    ? ix86_cost->hard_register.sse_to_integer
	    : ix86_cost->hard_register.integer_to_sse);

  /* Moves between mask register and GPR.  */
  if (MASK_CLASS_P (class1) != MASK_CLASS_P (class2))
    {
      return (MASK_CLASS_P (class1)
	      ? ix86_cost->hard_register.mask_to_integer
	      : ix86_cost->hard_register.integer_to_mask);
    }
  /* Moving between mask registers.  */
  if (MASK_CLASS_P (class1) && MASK_CLASS_P (class2))
    return ix86_cost->hard_register.mask_move;

  if (MAYBE_FLOAT_CLASS_P (class1))
    return ix86_cost->hard_register.fp_move;
  if (MAYBE_SSE_CLASS_P (class1))
    {
      if (GET_MODE_BITSIZE (mode) <= 128)
	return ix86_cost->hard_register.xmm_move;
      if (GET_MODE_BITSIZE (mode) <= 256)
	return ix86_cost->hard_register.ymm_move;
      return ix86_cost->hard_register.zmm_move;
    }
  if (MAYBE_MMX_CLASS_P (class1))
    return ix86_cost->hard_register.mmx_move;
  return 2;
}

/* Implement TARGET_HARD_REGNO_NREGS.  This is ordinarily the length in
   words of a value of mode MODE but can be less for certain modes in
   special long registers.

   Actually there are no two word move instructions for consecutive
   registers.  And only registers 0-3 may have mov byte instructions
   applied to them.  */

static unsigned int
ix86_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (GENERAL_REGNO_P (regno))
    {
      if (mode == XFmode)
	return TARGET_64BIT ? 2 : 3;
      if (mode == XCmode)
	return TARGET_64BIT ? 4 : 6;
      return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
    }
  if (COMPLEX_MODE_P (mode))
    return 2;
  /* Register pair for mask registers.  */
  if (mode == P2QImode || mode == P2HImode)
    return 2;
  if (mode == V64SFmode || mode == V64SImode)
    return 4;
  return 1;
}

/* Implement REGMODE_NATURAL_SIZE(MODE).  */
unsigned int
ix86_regmode_natural_size (machine_mode mode)
{
  if (mode == P2HImode || mode == P2QImode)
    return GET_MODE_SIZE (mode) / 2;
  return UNITS_PER_WORD;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
ix86_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* Flags and only flags can only hold CCmode values.  */
  if (CC_REGNO_P (regno))
    return GET_MODE_CLASS (mode) == MODE_CC;
  if (GET_MODE_CLASS (mode) == MODE_CC
      || GET_MODE_CLASS (mode) == MODE_RANDOM)
    return false;
  if (STACK_REGNO_P (regno))
    return VALID_FP_MODE_P (mode);
  if (MASK_REGNO_P (regno))
    {
      /* Register pair only starts at even register number.  */
      if ((mode == P2QImode || mode == P2HImode))
	return MASK_PAIR_REGNO_P(regno);

      return ((TARGET_AVX512F && VALID_MASK_REG_MODE (mode))
	      || (TARGET_AVX512BW && VALID_MASK_AVX512BW_MODE (mode)));
    }

  if (GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
    return false;

  if (SSE_REGNO_P (regno))
    {
      /* We implement the move patterns for all vector modes into and
	 out of SSE registers, even when no operation instructions
	 are available.  */

      /* For AVX-512 we allow, regardless of regno:
	  - XI mode
	  - any of 512-bit wide vector mode
	  - any scalar mode.  */
      if (TARGET_AVX512F
	  && ((VALID_AVX512F_REG_OR_XI_MODE (mode) && TARGET_EVEX512)
	      || VALID_AVX512F_SCALAR_MODE (mode)))
	return true;

      /* For AVX-5124FMAPS or AVX-5124VNNIW
	 allow V64SF and V64SI modes for special regnos.  */
      if ((TARGET_AVX5124FMAPS || TARGET_AVX5124VNNIW)
	  && (mode == V64SFmode || mode == V64SImode)
	  && MOD4_SSE_REGNO_P (regno))
	return true;

      /* TODO check for QI/HI scalars.  */
      /* AVX512VL allows sse regs16+ for 128/256 bit modes.  */
      if (TARGET_AVX512VL
	  && (VALID_AVX256_REG_OR_OI_MODE (mode)
	      || VALID_AVX512VL_128_REG_MODE (mode)))
	return true;

      /* xmm16-xmm31 are only available for AVX-512.  */
      if (EXT_REX_SSE_REGNO_P (regno))
	return false;

      /* Use pinsrw/pextrw to mov 16-bit data from/to sse to/from integer.  */
      if (TARGET_SSE2 && mode == HImode)
	return true;

      /* OImode and AVX modes are available only when AVX is enabled.  */
      return ((TARGET_AVX
	       && VALID_AVX256_REG_OR_OI_MODE (mode))
	      || VALID_SSE_REG_MODE (mode)
	      || VALID_SSE2_REG_MODE (mode)
	      || VALID_MMX_REG_MODE (mode)
	      || VALID_MMX_REG_MODE_3DNOW (mode));
    }
  if (MMX_REGNO_P (regno))
    {
      /* We implement the move patterns for 3DNOW modes even in MMX mode,
	 so if the register is available at all, then we can move data of
	 the given mode into or out of it.  */
      return (VALID_MMX_REG_MODE (mode)
	      || VALID_MMX_REG_MODE_3DNOW (mode));
    }

  if (mode == QImode)
    {
      /* Take care for QImode values - they can be in non-QI regs,
	 but then they do cause partial register stalls.  */
      if (ANY_QI_REGNO_P (regno))
	return true;
      if (!TARGET_PARTIAL_REG_STALL)
	return true;
      /* LRA checks if the hard register is OK for the given mode.
	 QImode values can live in non-QI regs, so we allow all
	 registers here.  */
      if (lra_in_progress)
       return true;
      return !can_create_pseudo_p ();
    }
  /* We handle both integer and floats in the general purpose registers.  */
  else if (VALID_INT_MODE_P (mode)
	   || VALID_FP_MODE_P (mode))
    return true;
  /* Lots of MMX code casts 8 byte vector modes to DImode.  If we then go
     on to use that value in smaller contexts, this can easily force a
     pseudo to be allocated to GENERAL_REGS.  Since this is no worse than
     supporting DImode, allow it.  */
  else if (VALID_MMX_REG_MODE_3DNOW (mode) || VALID_MMX_REG_MODE (mode))
    return true;

  return false;
}

/* Implement TARGET_INSN_CALLEE_ABI.  */

const predefined_function_abi &
ix86_insn_callee_abi (const rtx_insn *insn)
{
  unsigned int abi_id = 0;
  rtx pat = PATTERN (insn);
  if (vzeroupper_pattern (pat, VOIDmode))
    abi_id = ABI_VZEROUPPER;

  return function_abis[abi_id];
}

/* Initialize function_abis with corresponding abi_id,
   currently only handle vzeroupper.  */
void
ix86_initialize_callee_abi (unsigned int abi_id)
{
  gcc_assert (abi_id == ABI_VZEROUPPER);
  predefined_function_abi &vzeroupper_abi = function_abis[abi_id];
  if (!vzeroupper_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers;
      CLEAR_HARD_REG_SET (full_reg_clobbers);
      vzeroupper_abi.initialize (ABI_VZEROUPPER, full_reg_clobbers);
    }
}

void
ix86_expand_avx_vzeroupper (void)
{
  /* Initialize vzeroupper_abi here.  */
  ix86_initialize_callee_abi (ABI_VZEROUPPER);
  rtx_insn *insn = emit_call_insn (gen_avx_vzeroupper_callee_abi ());
  /* Return false for non-local goto in can_nonlocal_goto.  */
  make_reg_eh_region_note (insn, 0, INT_MIN);
  /* Flag used for call_insn indicates it's a fake call.  */
  RTX_FLAG (insn, used) = 1;
}


/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  The only ABI that
   saves SSE registers across calls is Win64 (thus no need to check the
   current ABI here), and with AVX enabled Win64 only guarantees that
   the low 16 bytes are saved.  */

static bool
ix86_hard_regno_call_part_clobbered (unsigned int abi_id, unsigned int regno,
				     machine_mode mode)
{
  /* Special ABI for vzeroupper which only clobber higher part of sse regs.  */
  if (abi_id == ABI_VZEROUPPER)
      return (GET_MODE_SIZE (mode) > 16
	      && ((TARGET_64BIT && REX_SSE_REGNO_P (regno))
		  || LEGACY_SSE_REGNO_P (regno)));

  return SSE_REGNO_P (regno) && GET_MODE_SIZE (mode) > 16;
}

/* A subroutine of ix86_modes_tieable_p.  Return true if MODE is a
   tieable integer mode.  */

static bool
ix86_tieable_integer_mode_p (machine_mode mode)
{
  switch (mode)
    {
    case E_HImode:
    case E_SImode:
      return true;

    case E_QImode:
      return TARGET_64BIT || !TARGET_PARTIAL_REG_STALL;

    case E_DImode:
      return TARGET_64BIT;

    default:
      return false;
    }
}

/* Implement TARGET_MODES_TIEABLE_P.

   Return true if MODE1 is accessible in a register that can hold MODE2
   without copying.  That is, all register classes that can hold MODE2
   can also hold MODE1.  */

static bool
ix86_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (mode1 == mode2)
    return true;

  if (ix86_tieable_integer_mode_p (mode1)
      && ix86_tieable_integer_mode_p (mode2))
    return true;

  /* MODE2 being XFmode implies fp stack or general regs, which means we
     can tie any smaller floating point modes to it.  Note that we do not
     tie this with TFmode.  */
  if (mode2 == XFmode)
    return mode1 == SFmode || mode1 == DFmode;

  /* MODE2 being DFmode implies fp stack, general or sse regs, which means
     that we can tie it with SFmode.  */
  if (mode2 == DFmode)
    return mode1 == SFmode;

  /* If MODE2 is only appropriate for an SSE register, then tie with
     any other mode acceptable to SSE registers.  */
  if (GET_MODE_SIZE (mode2) == 64
      && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 64
	    && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode1));
  if (GET_MODE_SIZE (mode2) == 32
      && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 32
	    && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode1));
  if (GET_MODE_SIZE (mode2) == 16
      && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 16
	    && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode1));

  /* If MODE2 is appropriate for an MMX register, then tie
     with any other mode acceptable to MMX registers.  */
  if (GET_MODE_SIZE (mode2) == 8
      && ix86_hard_regno_mode_ok (FIRST_MMX_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 8
	    && ix86_hard_regno_mode_ok (FIRST_MMX_REG, mode1));

  /* SCmode and DImode can be tied.  */
  if ((mode1 == E_SCmode && mode2 == E_DImode)
      || (mode1 == E_DImode && mode2 == E_SCmode))
    return TARGET_64BIT;

  /* [SD]Cmode and V2[SD]Fmode modes can be tied.  */
  if ((mode1 == E_SCmode && mode2 == E_V2SFmode)
      || (mode1 == E_V2SFmode && mode2 == E_SCmode)
      || (mode1 == E_DCmode && mode2 == E_V2DFmode)
      || (mode1 == E_V2DFmode && mode2 == E_DCmode))
    return true;

  return false;
}

/* Return the cost of moving between two registers of mode MODE.  */

static int
ix86_set_reg_reg_cost (machine_mode mode)
{
  unsigned int units = UNITS_PER_WORD;

  switch (GET_MODE_CLASS (mode))
    {
    default:
      break;

    case MODE_CC:
      units = GET_MODE_SIZE (CCmode);
      break;

    case MODE_FLOAT:
      if ((TARGET_SSE && mode == TFmode)
	  || (TARGET_80387 && mode == XFmode)
	  || ((TARGET_80387 || TARGET_SSE2) && mode == DFmode)
	  || ((TARGET_80387 || TARGET_SSE) && mode == SFmode))
	units = GET_MODE_SIZE (mode);
      break;

    case MODE_COMPLEX_FLOAT:
      if ((TARGET_SSE && mode == TCmode)
	  || (TARGET_80387 && mode == XCmode)
	  || ((TARGET_80387 || TARGET_SSE2) && mode == DCmode)
	  || ((TARGET_80387 || TARGET_SSE) && mode == SCmode))
	units = GET_MODE_SIZE (mode);
      break;

    case MODE_VECTOR_INT:
    case MODE_VECTOR_FLOAT:
      if ((TARGET_AVX512F && TARGET_EVEX512 && VALID_AVX512F_REG_MODE (mode))
	  || (TARGET_AVX && VALID_AVX256_REG_MODE (mode))
	  || (TARGET_SSE2 && VALID_SSE2_REG_MODE (mode))
	  || (TARGET_SSE && VALID_SSE_REG_MODE (mode))
	  || ((TARGET_MMX || TARGET_MMX_WITH_SSE)
	      && VALID_MMX_REG_MODE (mode)))
	units = GET_MODE_SIZE (mode);
    }

  /* Return the cost of moving between two registers of mode MODE,
     assuming that the move will be in pieces of at most UNITS bytes.  */
  return COSTS_N_INSNS (CEIL (GET_MODE_SIZE (mode), units));
}

/* Return cost of vector operation in MODE given that scalar version has
   COST.  */

static int
ix86_vec_cost (machine_mode mode, int cost)
{
  if (!VECTOR_MODE_P (mode))
    return cost;

  if (GET_MODE_BITSIZE (mode) == 128
      && TARGET_SSE_SPLIT_REGS)
    return cost * GET_MODE_BITSIZE (mode) / 64;
  else if (GET_MODE_BITSIZE (mode) > 128
      && TARGET_AVX256_SPLIT_REGS)
    return cost * GET_MODE_BITSIZE (mode) / 128;
  else if (GET_MODE_BITSIZE (mode) > 256
      && TARGET_AVX512_SPLIT_REGS)
    return cost * GET_MODE_BITSIZE (mode) / 256;
  return cost;
}

/* Return cost of vec_widen_<s>mult_hi/lo_<mode>,
   vec_widen_<s>mul_hi/lo_<mode> is only available for VI124_AVX2.  */
static int
ix86_widen_mult_cost (const struct processor_costs *cost,
		      enum machine_mode mode, bool uns_p)
{
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
  int extra_cost = 0;
  int basic_cost = 0;
  switch (mode)
    {
    case V8HImode:
    case V16HImode:
      if (!uns_p || mode == V16HImode)
	extra_cost = cost->sse_op * 2;
      basic_cost = cost->mulss * 2 + cost->sse_op * 4;
      break;
    case V4SImode:
    case V8SImode:
      /* pmulhw/pmullw can be used.  */
      basic_cost = cost->mulss * 2 + cost->sse_op * 2;
      break;
    case V2DImode:
      /* pmuludq under sse2, pmuldq under sse4.1, for sign_extend,
	 require extra 4 mul, 4 add, 4 cmp and 2 shift.  */
      if (!TARGET_SSE4_1 && !uns_p)
	extra_cost = (cost->mulss + cost->addss + cost->sse_op) * 4
		      + cost->sse_op * 2;
      /* Fallthru.  */
    case V4DImode:
      basic_cost = cost->mulss * 2 + cost->sse_op * 4;
      break;
    default:
      /* Not implemented.  */
      return 100;
    }
  return ix86_vec_cost (mode, basic_cost + extra_cost);
}

/* Return cost of multiplication in MODE.  */

static int
ix86_multiplication_cost (const struct processor_costs *cost,
			  enum machine_mode mode)
{
  machine_mode inner_mode = mode;
  if (VECTOR_MODE_P (mode))
    inner_mode = GET_MODE_INNER (mode);

  if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
    return inner_mode == DFmode ? cost->mulsd : cost->mulss;
  else if (X87_FLOAT_MODE_P (mode))
    return cost->fmul;
  else if (FLOAT_MODE_P (mode))
    return  ix86_vec_cost (mode,
			   inner_mode == DFmode ? cost->mulsd : cost->mulss);
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    {
      int nmults, nops;
      /* Cost of reading the memory.  */
      int extra;

      switch (mode)
	{
	case V4QImode:
	case V8QImode:
	  /* Partial V*QImode is emulated with 4-6 insns.  */
	  nmults = 1;
	  nops = 3;
	  extra = 0;

	  if (TARGET_AVX512BW && TARGET_AVX512VL)
	    ;
	  else if (TARGET_AVX2)
	    nops += 2;
	  else if (TARGET_XOP)
	    extra += cost->sse_load[2];
	  else
	    {
	      nops += 1;
	      extra += cost->sse_load[2];
	    }
	  goto do_qimode;

	case V16QImode:
	  /* V*QImode is emulated with 4-11 insns.  */
	  nmults = 1;
	  nops = 3;
	  extra = 0;

	  if (TARGET_AVX2 && !TARGET_PREFER_AVX128)
	    {
	      if (!(TARGET_AVX512BW && TARGET_AVX512VL))
		nops += 3;
	    }
	  else if (TARGET_XOP)
	    {
	      nmults += 1;
	      nops += 2;
	      extra += cost->sse_load[2];
	    }
	  else
	    {
	      nmults += 1;
	      nops += 4;
	      extra += cost->sse_load[2];
	    }
	  goto do_qimode;

	case V32QImode:
	  nmults = 1;
	  nops = 3;
	  extra = 0;

	  if (!TARGET_AVX512BW || TARGET_PREFER_AVX256)
	    {
	      nmults += 1;
	      nops += 4;
	      extra += cost->sse_load[3] * 2;
	    }
	  goto do_qimode;

	case V64QImode:
	  nmults = 2;
	  nops = 9;
	  extra = cost->sse_load[3] * 2 + cost->sse_load[4] * 2;

	do_qimode:
	  return ix86_vec_cost (mode, cost->mulss * nmults
				+ cost->sse_op * nops) + extra;

	case V4SImode:
	  /* pmulld is used in this case. No emulation is needed.  */
	  if (TARGET_SSE4_1)
	    goto do_native;
	  /* V4SImode is emulated with 7 insns.  */
	  else
	    return ix86_vec_cost (mode, cost->mulss * 2 + cost->sse_op * 5);

	case V2DImode:
	case V4DImode:
	  /* vpmullq is used in this case. No emulation is needed.  */
	  if (TARGET_AVX512DQ && TARGET_AVX512VL)
	    goto do_native;
	  /* V*DImode is emulated with 6-8 insns.  */
	  else if (TARGET_XOP && mode == V2DImode)
	    return ix86_vec_cost (mode, cost->mulss * 2 + cost->sse_op * 4);
	  /* FALLTHRU */
	case V8DImode:
	  /* vpmullq is used in this case. No emulation is needed.  */
	  if (TARGET_AVX512DQ && mode == V8DImode)
	    goto do_native;
	  else
	    return ix86_vec_cost (mode, cost->mulss * 3 + cost->sse_op * 5);

	default:
	do_native:
	  return ix86_vec_cost (mode, cost->mulss);
	}
    }
  else
    return (cost->mult_init[MODE_INDEX (mode)] + cost->mult_bit * 7);
}

/* Return cost of multiplication in MODE.  */

static int
ix86_division_cost (const struct processor_costs *cost,
			  enum machine_mode mode)
{
  machine_mode inner_mode = mode;
  if (VECTOR_MODE_P (mode))
    inner_mode = GET_MODE_INNER (mode);

  if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
    return inner_mode == DFmode ? cost->divsd : cost->divss;
  else if (X87_FLOAT_MODE_P (mode))
    return cost->fdiv;
  else if (FLOAT_MODE_P (mode))
    return ix86_vec_cost (mode,
			  inner_mode == DFmode ? cost->divsd : cost->divss);
  else
    return cost->divide[MODE_INDEX (mode)];
}

/* Return cost of shift in MODE.
   If CONSTANT_OP1 is true, the op1 value is known and set in OP1_VAL.
   AND_IN_OP1 specify in op1 is result of AND and SHIFT_AND_TRUNCATE
   if op1 is a result of subreg.

   SKIP_OP0/1 is set to true if cost of OP0/1 should be ignored.  */

static int
ix86_shift_rotate_cost (const struct processor_costs *cost,
			enum rtx_code code,
			enum machine_mode mode, bool constant_op1,
			HOST_WIDE_INT op1_val,
			bool and_in_op1,
			bool shift_and_truncate,
			bool *skip_op0, bool *skip_op1)
{
  if (skip_op0)
    *skip_op0 = *skip_op1 = false;

  if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    {
      int count;
      /* Cost of reading the memory.  */
      int extra;

      switch (mode)
	{
	case V4QImode:
	case V8QImode:
	  if (TARGET_AVX2)
	    /* Use vpbroadcast.  */
	    extra = cost->sse_op;
	  else
	    extra = cost->sse_load[2];

	  if (constant_op1)
	    {
	      if (code == ASHIFTRT)
		{
		  count = 4;
		  extra *= 2;
		}
	      else
		count = 2;
	    }
	  else if (TARGET_AVX512BW && TARGET_AVX512VL)
	    return ix86_vec_cost (mode, cost->sse_op * 4);
	  else if (TARGET_SSE4_1)
	    count = 5;
	  else if (code == ASHIFTRT)
	    count = 6;
	  else
	    count = 5;
	  return ix86_vec_cost (mode, cost->sse_op * count) + extra;

	case V16QImode:
	  if (TARGET_XOP)
	    {
	      /* For XOP we use vpshab, which requires a broadcast of the
		 value to the variable shift insn.  For constants this
		 means a V16Q const in mem; even when we can perform the
		 shift with one insn set the cost to prefer paddb.  */
	      if (constant_op1)
		{
		  extra = cost->sse_load[2];
		  return ix86_vec_cost (mode, cost->sse_op) + extra;
		}
	      else
		{
		  count = (code == ASHIFT) ? 3 : 4;
		  return ix86_vec_cost (mode, cost->sse_op * count);
		}
	    }
	  /* FALLTHRU */
	case V32QImode:
	  if (TARGET_AVX2)
	    /* Use vpbroadcast.  */
	    extra = cost->sse_op;
	  else
	    extra = (mode == V16QImode) ? cost->sse_load[2] : cost->sse_load[3];

	  if (constant_op1)
	    {
	      if (code == ASHIFTRT)
		{
		  count = 4;
		  extra *= 2;
		}
	      else
		count = 2;
	    }
	  else if (TARGET_AVX512BW
		   && ((mode == V32QImode && !TARGET_PREFER_AVX256)
		       || (mode == V16QImode && TARGET_AVX512VL
			   && !TARGET_PREFER_AVX128)))
	    return ix86_vec_cost (mode, cost->sse_op * 4);
	  else if (TARGET_AVX2
		   && mode == V16QImode && !TARGET_PREFER_AVX128)
	    count = 6;
	  else if (TARGET_SSE4_1)
	    count = 9;
	  else if (code == ASHIFTRT)
	    count = 10;
	  else
	    count = 9;
	  return ix86_vec_cost (mode, cost->sse_op * count) + extra;

	case V2DImode:
	case V4DImode:
	  /* V*DImode arithmetic right shift is emulated.  */
	  if (code == ASHIFTRT && !TARGET_AVX512VL)
	    {
	      if (constant_op1)
		{
		  if (op1_val == 63)
		    count = TARGET_SSE4_2 ? 1 : 2;
		  else if (TARGET_XOP)
		    count = 2;
		  else if (TARGET_SSE4_1)
		    count = 3;
		  else
		    count = 4;
		}
	      else if (TARGET_XOP)
		count = 3;
	      else if (TARGET_SSE4_2)
		count = 4;
	      else
		count = 5;

	      return ix86_vec_cost (mode, cost->sse_op * count);
	    }
	  /* FALLTHRU */
	default:
	  return ix86_vec_cost (mode, cost->sse_op);
	}
    }

  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    {
      if (constant_op1)
	{
	  if (op1_val > 32)
	    return cost->shift_const + COSTS_N_INSNS (2);
	  else
	    return cost->shift_const * 2;
	}
      else
	{
	  if (and_in_op1)
	    return cost->shift_var * 2;
	  else
	    return cost->shift_var * 6 + COSTS_N_INSNS (2);
	}
    }
  else
    {
      if (constant_op1)
	return cost->shift_const;
      else if (shift_and_truncate)
	{
	  if (skip_op0)
	    *skip_op0 = *skip_op1 = true;
	  /* Return the cost after shift-and truncation.  */
	  return cost->shift_var;
	}
      else
	return cost->shift_var;
    }
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
ix86_rtx_costs (rtx x, machine_mode mode, int outer_code_i, int opno,
		int *total, bool speed)
{
  rtx mask;
  enum rtx_code code = GET_CODE (x);
  enum rtx_code outer_code = (enum rtx_code) outer_code_i;
  const struct processor_costs *cost
    = speed ? ix86_tune_cost : &ix86_size_cost;
  int src_cost;

  switch (code)
    {
    case SET:
      if (register_operand (SET_DEST (x), VOIDmode)
	  && register_operand (SET_SRC (x), VOIDmode))
	{
	  *total = ix86_set_reg_reg_cost (GET_MODE (SET_DEST (x)));
	  return true;
	}

      if (register_operand (SET_SRC (x), VOIDmode))
	/* Avoid potentially incorrect high cost from rtx_costs
	   for non-tieable SUBREGs.  */
	src_cost = 0;
      else
	{
	  src_cost = rtx_cost (SET_SRC (x), mode, SET, 1, speed);

	  if (CONSTANT_P (SET_SRC (x)))
	    /* Constant costs assume a base value of COSTS_N_INSNS (1) and add
	       a small value, possibly zero for cheap constants.  */
	    src_cost += COSTS_N_INSNS (1);
	}

      *total = src_cost + rtx_cost (SET_DEST (x), mode, SET, 0, speed);
      return true;

    case CONST_INT:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      if (x86_64_immediate_operand (x, VOIDmode))
	*total = 0;
     else
	*total = 1;
      return true;

    case CONST_DOUBLE:
      if (IS_STACK_MODE (mode))
	switch (standard_80387_constant_p (x))
	  {
	  case -1:
	  case 0:
	    break;
	  case 1: /* 0.0 */
	    *total = 1;
	    return true;
	  default: /* Other constants */
	    *total = 2;
	    return true;
	  }
      /* FALLTHRU */

    case CONST_VECTOR:
      switch (standard_sse_constant_p (x, mode))
	{
	case 0:
	  break;
	case 1:  /* 0: xor eliminates false dependency */
	  *total = 0;
	  return true;
	default: /* -1: cmp contains false dependency */
	  *total = 1;
	  return true;
	}
      /* FALLTHRU */

    case CONST_WIDE_INT:
      /* Fall back to (MEM (SYMBOL_REF)), since that's where
	 it'll probably end up.  Add a penalty for size.  */
      *total = (COSTS_N_INSNS (1)
		+ (!TARGET_64BIT && flag_pic)
		+ (GET_MODE_SIZE (mode) <= 4
		   ? 0 : GET_MODE_SIZE (mode) <= 8 ? 1 : 2));
      return true;

    case ZERO_EXTEND:
      /* The zero extensions is often completely free on x86_64, so make
	 it as cheap as possible.  */
      if (TARGET_64BIT && mode == DImode
	  && GET_MODE (XEXP (x, 0)) == SImode)
	*total = 1;
      else if (TARGET_ZERO_EXTEND_WITH_AND)
	*total = cost->add;
      else
	*total = cost->movzx;
      return false;

    case SIGN_EXTEND:
      *total = cost->movsx;
      return false;

    case ASHIFT:
      if (SCALAR_INT_MODE_P (mode)
	  && GET_MODE_SIZE (mode) < UNITS_PER_WORD
	  && CONST_INT_P (XEXP (x, 1)))
	{
	  HOST_WIDE_INT value = INTVAL (XEXP (x, 1));
	  if (value == 1)
	    {
	      *total = cost->add;
	      return false;
	    }
	  if ((value == 2 || value == 3)
	      && cost->lea <= cost->shift_const)
	    {
	      *total = cost->lea;
	      return false;
	    }
	}
      /* FALLTHRU */

    case ROTATE:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      bool skip_op0, skip_op1;
      *total = ix86_shift_rotate_cost (cost, code, mode,
				       CONSTANT_P (XEXP (x, 1)),
				       CONST_INT_P (XEXP (x, 1))
					 ? INTVAL (XEXP (x, 1)) : -1,
				       GET_CODE (XEXP (x, 1)) == AND,
				       SUBREG_P (XEXP (x, 1))
				       && GET_CODE (XEXP (XEXP (x, 1),
							  0)) == AND,
				       &skip_op0, &skip_op1);
      if (skip_op0 || skip_op1)
	{
	  if (!skip_op0)
	    *total += rtx_cost (XEXP (x, 0), mode, code, 0, speed);
	  if (!skip_op1)
	    *total += rtx_cost (XEXP (x, 1), mode, code, 0, speed);
	  return true;
	}
      return false;

    case FMA:
      {
	rtx sub;

        gcc_assert (FLOAT_MODE_P (mode));
        gcc_assert (TARGET_FMA || TARGET_FMA4 || TARGET_AVX512F);

        *total = ix86_vec_cost (mode,
				GET_MODE_INNER (mode) == SFmode
				? cost->fmass : cost->fmasd);
	*total += rtx_cost (XEXP (x, 1), mode, FMA, 1, speed);

        /* Negate in op0 or op2 is free: FMS, FNMA, FNMS.  */
	sub = XEXP (x, 0);
	if (GET_CODE (sub) == NEG)
	  sub = XEXP (sub, 0);
	*total += rtx_cost (sub, mode, FMA, 0, speed);

	sub = XEXP (x, 2);
	if (GET_CODE (sub) == NEG)
	  sub = XEXP (sub, 0);
	*total += rtx_cost (sub, mode, FMA, 2, speed);
	return true;
      }

    case MULT:
      if (!FLOAT_MODE_P (mode) && !VECTOR_MODE_P (mode))
	{
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = XEXP (x, 1);
	  int nbits;
	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      unsigned HOST_WIDE_INT value = INTVAL (XEXP (x, 1));
	      for (nbits = 0; value != 0; value &= value - 1)
	        nbits++;
	    }
	  else
	    /* This is arbitrary.  */
	    nbits = 7;

	  /* Compute costs correctly for widening multiplication.  */
	  if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	      && GET_MODE_SIZE (GET_MODE (XEXP (op0, 0))) * 2
	         == GET_MODE_SIZE (mode))
	    {
	      int is_mulwiden = 0;
	      machine_mode inner_mode = GET_MODE (op0);

	      if (GET_CODE (op0) == GET_CODE (op1))
		is_mulwiden = 1, op1 = XEXP (op1, 0);
	      else if (CONST_INT_P (op1))
		{
		  if (GET_CODE (op0) == SIGN_EXTEND)
		    is_mulwiden = trunc_int_for_mode (INTVAL (op1), inner_mode)
			          == INTVAL (op1);
		  else
		    is_mulwiden = !(INTVAL (op1) & ~GET_MODE_MASK (inner_mode));
	        }

	      if (is_mulwiden)
	        op0 = XEXP (op0, 0), mode = GET_MODE (op0);
	    }

	  int mult_init;
	  // Double word multiplication requires 3 mults and 2 adds.
	  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	    {
	      mult_init = 3 * cost->mult_init[MODE_INDEX (word_mode)]
			  + 2 * cost->add;
	      nbits *= 3;
	    }
	  else mult_init = cost->mult_init[MODE_INDEX (mode)];

  	  *total = (mult_init
		    + nbits * cost->mult_bit
	            + rtx_cost (op0, mode, outer_code, opno, speed)
		    + rtx_cost (op1, mode, outer_code, opno, speed));

          return true;
	}
      *total = ix86_multiplication_cost (cost, mode);
      return false;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = ix86_division_cost (cost, mode);
      return false;

    case PLUS:
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	{
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	      && CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 0), 1))
	      && CONSTANT_P (XEXP (x, 1)))
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1));
	      if (val == 2 || val == 4 || val == 8)
		{
		  *total = cost->lea;
		  *total += rtx_cost (XEXP (XEXP (x, 0), 1), mode,
				      outer_code, opno, speed);
		  *total += rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 0), mode,
				      outer_code, opno, speed);
		  *total += rtx_cost (XEXP (x, 1), mode,
				      outer_code, opno, speed);
		  return true;
		}
	    }
	  else if (GET_CODE (XEXP (x, 0)) == MULT
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (x, 0), 1));
	      if (val == 2 || val == 4 || val == 8)
		{
		  *total = cost->lea;
		  *total += rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				      outer_code, opno, speed);
		  *total += rtx_cost (XEXP (x, 1), mode,
				      outer_code, opno, speed);
		  return true;
		}
	    }
	  else if (GET_CODE (XEXP (x, 0)) == PLUS)
	    {
	      rtx op = XEXP (XEXP (x, 0), 0);

	      /* Add with carry, ignore the cost of adding a carry flag.  */
	      if (ix86_carry_flag_operator (op, mode)
		  || ix86_carry_flag_unset_operator (op, mode))
		*total = cost->add;
	      else
		{
		  *total = cost->lea;
		  *total += rtx_cost (op, mode,
				      outer_code, opno, speed);
		}

	      *total += rtx_cost (XEXP (XEXP (x, 0), 1), mode,
				  outer_code, opno, speed);
	      *total += rtx_cost (XEXP (x, 1), mode,
				  outer_code, opno, speed);
	      return true;
	    }
	}
      /* FALLTHRU */

    case MINUS:
      /* Subtract with borrow, ignore the cost of subtracting a carry flag.  */
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	  && GET_CODE (XEXP (x, 0)) == MINUS
	  && (ix86_carry_flag_operator (XEXP (XEXP (x, 0), 1), mode)
	      || ix86_carry_flag_unset_operator (XEXP (XEXP (x, 0), 1), mode)))
	{
	  *total = cost->add;
	  *total += rtx_cost (XEXP (XEXP (x, 0), 0), mode,
			      outer_code, opno, speed);
	  *total += rtx_cost (XEXP (x, 1), mode,
			      outer_code, opno, speed);
	  return true;
	}

      if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = cost->addss;
      else if (X87_FLOAT_MODE_P (mode))
	*total = cost->fadd;
      else if (FLOAT_MODE_P (mode))
	*total = ix86_vec_cost (mode, cost->addss);
      else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	*total = ix86_vec_cost (mode, cost->sse_op);
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	*total = cost->add * 2;
      else
	*total = cost->add;
      return false;

    case IOR:
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	  || SSE_FLOAT_MODE_P (mode))
	{
	  /* (ior (not ...) ...) can be a single insn in AVX512.  */
	  if (GET_CODE (XEXP (x, 0)) == NOT && TARGET_AVX512F
	      && ((TARGET_EVEX512
		   && GET_MODE_SIZE (mode) == 64)
		  || (TARGET_AVX512VL
		      && (GET_MODE_SIZE (mode) == 32
			  || GET_MODE_SIZE (mode) == 16))))
	    {
	      rtx right = GET_CODE (XEXP (x, 1)) != NOT
			  ? XEXP (x, 1) : XEXP (XEXP (x, 1), 0);

	      *total = ix86_vec_cost (mode, cost->sse_op)
		       + rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (right, mode, outer_code, opno, speed);
	      return true;
	    }
	  *total = ix86_vec_cost (mode, cost->sse_op);
	}
      else if (TARGET_64BIT
	       && mode == TImode
	       && GET_CODE (XEXP (x, 0)) == ASHIFT
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND
	       && GET_MODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == DImode
	       && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	       && INTVAL (XEXP (XEXP (x, 0), 1)) == 64
	       && GET_CODE (XEXP (x, 1)) == ZERO_EXTEND
	       && GET_MODE (XEXP (XEXP (x, 1), 0)) == DImode)
	{
	  /* *concatditi3 is cheap.  */
	  rtx op0 = XEXP (XEXP (XEXP (x, 0), 0), 0);
	  rtx op1 = XEXP (XEXP (x, 1), 0);
	  *total = (SUBREG_P (op0) && GET_MODE (SUBREG_REG (op0)) == DFmode)
		   ? COSTS_N_INSNS (1)    /* movq.  */
		   : set_src_cost (op0, DImode, speed);
	  *total += (SUBREG_P (op1) && GET_MODE (SUBREG_REG (op1)) == DFmode)
		    ? COSTS_N_INSNS (1)    /* movq.  */
		    : set_src_cost (op1, DImode, speed);
	  return true;
	}
      else if (TARGET_64BIT
	       && mode == TImode
	       && GET_CODE (XEXP (x, 0)) == AND
	       && REG_P (XEXP (XEXP (x, 0), 0))
	       && CONST_WIDE_INT_P (XEXP (XEXP (x, 0), 1))
	       && CONST_WIDE_INT_NUNITS (XEXP (XEXP (x, 0), 1)) == 2
	       && CONST_WIDE_INT_ELT (XEXP (XEXP (x, 0), 1), 0) == -1
	       && CONST_WIDE_INT_ELT (XEXP (XEXP (x, 0), 1), 1) == 0
	       && GET_CODE (XEXP (x, 1)) == ASHIFT
	       && GET_CODE (XEXP (XEXP (x, 1), 0)) == ZERO_EXTEND
	       && GET_MODE (XEXP (XEXP (XEXP (x, 1), 0), 0)) == DImode
	       && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	       && INTVAL (XEXP (XEXP (x, 1), 1)) == 64)
	{
	  /* *insvti_highpart is cheap.  */
	  rtx op = XEXP (XEXP (XEXP (x, 1), 0), 0);
	  *total = COSTS_N_INSNS (1) + 1;
	  *total += (SUBREG_P (op) && GET_MODE (SUBREG_REG (op)) == DFmode)
		    ? COSTS_N_INSNS (1)    /* movq.  */
		    : set_src_cost (op, DImode, speed);
	  return true;
	}
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	*total = cost->add * 2;
      else
	*total = cost->add;
      return false;

    case XOR:
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	  || SSE_FLOAT_MODE_P (mode))
	*total = ix86_vec_cost (mode, cost->sse_op);
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	*total = cost->add * 2;
      else
	*total = cost->add;
      return false;

    case AND:
      if (address_no_seg_operand (x, mode))
	{
	  *total = cost->lea;
	  return true;
	}
      else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	       || SSE_FLOAT_MODE_P (mode))
	{
	  /* pandn is a single instruction.  */
	  if (GET_CODE (XEXP (x, 0)) == NOT)
	    {
	      rtx right = XEXP (x, 1);

	      /* (and (not ...) (not ...)) can be a single insn in AVX512.  */
	      if (GET_CODE (right) == NOT && TARGET_AVX512F
		  && ((TARGET_EVEX512
		       && GET_MODE_SIZE (mode) == 64)
		      || (TARGET_AVX512VL
			  && (GET_MODE_SIZE (mode) == 32
			      || GET_MODE_SIZE (mode) == 16))))
		right = XEXP (right, 0);

	      *total = ix86_vec_cost (mode, cost->sse_op)
		       + rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (right, mode, outer_code, opno, speed);
	      return true;
	    }
	  else if (GET_CODE (XEXP (x, 1)) == NOT)
	    {
	      *total = ix86_vec_cost (mode, cost->sse_op)
		       + rtx_cost (XEXP (x, 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (XEXP (XEXP (x, 1), 0), mode,
				   outer_code, opno, speed);
	      return true;
	    }
	  *total = ix86_vec_cost (mode, cost->sse_op);
	}
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	{
	  if (TARGET_BMI && GET_CODE (XEXP (x,0)) == NOT)
	    {
	      *total = cost->add * 2
		       + rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (XEXP (x, 1), mode,
				   outer_code, opno, speed);
	      return true;
	    }
	  else if (TARGET_BMI && GET_CODE (XEXP (x, 1)) == NOT)
	    {
	      *total = cost->add * 2
		       + rtx_cost (XEXP (x, 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (XEXP (XEXP (x, 1), 0), mode,
				   outer_code, opno, speed);
	      return true;
	    }
	  *total = cost->add * 2;
	}
      else if (TARGET_BMI && GET_CODE (XEXP (x,0)) == NOT)
	{
	  *total = cost->add
		   + rtx_cost (XEXP (XEXP (x, 0), 0), mode,
			       outer_code, opno, speed)
		   + rtx_cost (XEXP (x, 1), mode, outer_code, opno, speed);
	  return true;
	}
      else if (TARGET_BMI && GET_CODE (XEXP (x,1)) == NOT)
	{
	  *total = cost->add
		   + rtx_cost (XEXP (x, 0), mode, outer_code, opno, speed)
		   + rtx_cost (XEXP (XEXP (x, 1), 0), mode,
			       outer_code, opno, speed);
	  return true;
	}
      else
	*total = cost->add;
      return false;

    case NOT:
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	{
	  /* (not (xor ...)) can be a single insn in AVX512.  */
	  if (GET_CODE (XEXP (x, 0)) == XOR && TARGET_AVX512F
	      && ((TARGET_EVEX512
		   && GET_MODE_SIZE (mode) == 64)
		  || (TARGET_AVX512VL
		      && (GET_MODE_SIZE (mode) == 32
			  || GET_MODE_SIZE (mode) == 16))))
	    {
	      *total = ix86_vec_cost (mode, cost->sse_op)
		       + rtx_cost (XEXP (XEXP (x, 0), 0), mode,
				   outer_code, opno, speed)
		       + rtx_cost (XEXP (XEXP (x, 0), 1), mode,
				   outer_code, opno, speed);
	      return true;
	    }

	  // vnot is pxor -1.
	  *total = ix86_vec_cost (mode, cost->sse_op) + 1;
	}
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	*total = cost->add * 2;
      else
	*total = cost->add;
      return false;

    case NEG:
      if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = cost->sse_op;
      else if (X87_FLOAT_MODE_P (mode))
	*total = cost->fchs;
      else if (FLOAT_MODE_P (mode))
	*total = ix86_vec_cost (mode, cost->sse_op);
      else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	*total = ix86_vec_cost (mode, cost->sse_op);
      else if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	*total = cost->add * 3;
      else
	*total = cost->add;
      return false;

    case COMPARE:
      rtx op0, op1;
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      if (GET_CODE (op0) == ZERO_EXTRACT
	  && XEXP (op0, 1) == const1_rtx
	  && CONST_INT_P (XEXP (op0, 2))
	  && op1 == const0_rtx)
	{
	  /* This kind of construct is implemented using test[bwl].
	     Treat it as if we had an AND.  */
	  mode = GET_MODE (XEXP (op0, 0));
	  *total = (cost->add
		    + rtx_cost (XEXP (op0, 0), mode, outer_code,
				opno, speed)
		    + rtx_cost (const1_rtx, mode, outer_code, opno, speed));
	  return true;
	}

      if (GET_CODE (op0) == PLUS && rtx_equal_p (XEXP (op0, 0), op1))
	{
	  /* This is an overflow detection, count it as a normal compare.  */
	  *total = rtx_cost (op0, GET_MODE (op0), COMPARE, 0, speed);
	  return true;
	}

      rtx geu;
      /* Match x
	 (compare:CCC (neg:QI (geu:QI (reg:CC_CCC FLAGS_REG) (const_int 0)))
		      (ltu:QI (reg:CC_CCC FLAGS_REG) (const_int 0)))  */
      if (mode == CCCmode
	  && GET_CODE (op0) == NEG
	  && GET_CODE (geu = XEXP (op0, 0)) == GEU
	  && REG_P (XEXP (geu, 0))
	  && (GET_MODE (XEXP (geu, 0)) == CCCmode
	      || GET_MODE (XEXP (geu, 0)) == CCmode)
	  && REGNO (XEXP (geu, 0)) == FLAGS_REG
	  && XEXP (geu, 1) == const0_rtx
	  && GET_CODE (op1) == LTU
	  && REG_P (XEXP (op1, 0))
	  && GET_MODE (XEXP (op1, 0)) == GET_MODE (XEXP (geu, 0))
	  && REGNO (XEXP (op1, 0)) == FLAGS_REG
	  && XEXP (op1, 1) == const0_rtx)
	{
	  /* This is *setcc_qi_addqi3_cconly_overflow_1_* patterns, a nop.  */
	  *total = 0;
	  return true;
	}
      /* Match x
	 (compare:CCC (neg:QI (ltu:QI (reg:CCC FLAGS_REG) (const_int 0)))
		      (geu:QI (reg:CCC FLAGS_REG) (const_int 0)))  */
      if (mode == CCCmode
	  && GET_CODE (op0) == NEG
	  && GET_CODE (XEXP (op0, 0)) == LTU
	  && REG_P (XEXP (XEXP (op0, 0), 0))
	  && GET_MODE (XEXP (XEXP (op0, 0), 0)) == CCCmode
	  && REGNO (XEXP (XEXP (op0, 0), 0)) == FLAGS_REG
	  && XEXP (XEXP (op0, 0), 1) == const0_rtx
	  && GET_CODE (op1) == GEU
	  && REG_P (XEXP (op1, 0))
	  && GET_MODE (XEXP (op1, 0)) == CCCmode
	  && REGNO (XEXP (op1, 0)) == FLAGS_REG
	  && XEXP (op1, 1) == const0_rtx)
	{
	  /* This is *x86_cmc.  */
	  if (!speed)
	    *total = COSTS_N_BYTES (1);
	  else if (TARGET_SLOW_STC)
	    *total = COSTS_N_INSNS (2);
	  else 
	    *total = COSTS_N_INSNS (1);
	  return true;
	}

      if (SCALAR_INT_MODE_P (GET_MODE (op0))
	  && GET_MODE_SIZE (GET_MODE (op0)) > UNITS_PER_WORD)
	{
	  if (op1 == const0_rtx)
	    *total = cost->add
		     + rtx_cost (op0, GET_MODE (op0), outer_code, opno, speed);
	  else
	    *total = 3*cost->add
		     + rtx_cost (op0, GET_MODE (op0), outer_code, opno, speed)
		     + rtx_cost (op1, GET_MODE (op0), outer_code, opno, speed);
	  return true;
	}

      /* The embedded comparison operand is completely free.  */
      if (!general_operand (op0, GET_MODE (op0)) && op1 == const0_rtx)
	*total = 0;

      return false;

    case FLOAT_EXTEND:
      if (!SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = 0;
      else
        *total = ix86_vec_cost (mode, cost->addss);
      return false;

    case FLOAT_TRUNCATE:
      if (!SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = cost->fadd;
      else
        *total = ix86_vec_cost (mode, cost->addss);
      return false;

    case ABS:
      /* SSE requires memory load for the constant operand. It may make
	 sense to account for this.  Of course the constant operand may or
	 may not be reused. */
      if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = cost->sse_op;
      else if (X87_FLOAT_MODE_P (mode))
	*total = cost->fabs;
      else if (FLOAT_MODE_P (mode))
	*total = ix86_vec_cost (mode, cost->sse_op);
      return false;

    case SQRT:
      if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	*total = mode == SFmode ? cost->sqrtss : cost->sqrtsd;
      else if (X87_FLOAT_MODE_P (mode))
	*total = cost->fsqrt;
      else if (FLOAT_MODE_P (mode))
	*total = ix86_vec_cost (mode,
				mode == SFmode ? cost->sqrtss : cost->sqrtsd);
      return false;

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_TP)
	*total = 0;
      else if (XINT (x, 1) == UNSPEC_VTERNLOG)
	{
	  *total = cost->sse_op;
	  return true;
	}
      else if (XINT (x, 1) == UNSPEC_PTEST)
	{
	  *total = cost->sse_op;
	  rtx test_op0 = XVECEXP (x, 0, 0);
	  if (!rtx_equal_p (test_op0, XVECEXP (x, 0, 1)))
	    return false;
	  if (GET_CODE (test_op0) == AND)
	    {
	      rtx and_op0 = XEXP (test_op0, 0);
	      if (GET_CODE (and_op0) == NOT)
		and_op0 = XEXP (and_op0, 0);
	      *total += rtx_cost (and_op0, GET_MODE (and_op0),
				  AND, 0, speed)
			+ rtx_cost (XEXP (test_op0, 1), GET_MODE (and_op0),
				    AND, 1, speed);
	    }
	  else
	    *total = rtx_cost (test_op0, GET_MODE (test_op0),
			       UNSPEC, 0, speed);
	  return true;
	}
      return false;

    case VEC_SELECT:
    case VEC_CONCAT:
    case VEC_DUPLICATE:
      /* ??? Assume all of these vector manipulation patterns are
	 recognizable.  In which case they all pretty much have the
	 same cost.  */
     *total = cost->sse_op;
     return true;
    case VEC_MERGE:
      mask = XEXP (x, 2);
      /* This is masked instruction, assume the same cost,
	 as nonmasked variant.  */
      if (TARGET_AVX512F && register_operand (mask, GET_MODE (mask)))
	*total = rtx_cost (XEXP (x, 0), mode, outer_code, opno, speed);
      else
	*total = cost->sse_op;
      return true;

    case MEM:
      /* An insn that accesses memory is slightly more expensive
         than one that does not.  */
      if (speed)
        *total += 1;
      return false;

    case ZERO_EXTRACT:
      if (XEXP (x, 1) == const1_rtx
	  && GET_CODE (XEXP (x, 2)) == ZERO_EXTEND
	  && GET_MODE (XEXP (x, 2)) == SImode
	  && GET_MODE (XEXP (XEXP (x, 2), 0)) == QImode)
	{
	  /* Ignore cost of zero extension and masking of last argument.  */
	  *total += rtx_cost (XEXP (x, 0), mode, code, 0, speed);
	  *total += rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  *total += rtx_cost (XEXP (XEXP (x, 2), 0), mode, code, 2, speed);
	  return true;
	}
      return false;

    case IF_THEN_ELSE:
      if (TARGET_XOP
	  && VECTOR_MODE_P (mode)
	  && (GET_MODE_SIZE (mode) == 16 || GET_MODE_SIZE (mode) == 32))
	{
	  /* vpcmov.  */
	  *total = speed ? COSTS_N_INSNS (2) : COSTS_N_BYTES (6);
	  if (!REG_P (XEXP (x, 0)))
	    *total += rtx_cost (XEXP (x, 0), mode, code, 0, speed);
	  if (!REG_P (XEXP (x, 1)))
	    *total += rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  if (!REG_P (XEXP (x, 2)))
	    *total += rtx_cost (XEXP (x, 2), mode, code, 2, speed);
	  return true;
	}
      else if (TARGET_CMOVE
	       && SCALAR_INT_MODE_P (mode)
	       && GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	{
	  /* cmov.  */
	  *total = COSTS_N_INSNS (1);
	  if (!REG_P (XEXP (x, 0)))
	    *total += rtx_cost (XEXP (x, 0), mode, code, 0, speed);
	  if (!REG_P (XEXP (x, 1)))
	    *total += rtx_cost (XEXP (x, 1), mode, code, 1, speed);
	  if (!REG_P (XEXP (x, 2)))
	    *total += rtx_cost (XEXP (x, 2), mode, code, 2, speed);
	  return true;
	}
      return false;

    default:
      return false;
    }
}

#if TARGET_MACHO

static int current_machopic_label_num;

/* Given a symbol name and its associated stub, write out the
   definition of the stub.  */

void
machopic_output_stub (FILE *file, const char *symb, const char *stub)
{
  unsigned int length;
  char *binder_name, *symbol_name, lazy_ptr_name[32];
  int label = ++current_machopic_label_num;

  /* For 64-bit we shouldn't get here.  */
  gcc_assert (!TARGET_64BIT);

  /* Lose our funky encoding stuff so it doesn't contaminate the stub.  */
  symb = targetm.strip_name_encoding (symb);

  length = strlen (stub);
  binder_name = XALLOCAVEC (char, length + 32);
  GEN_BINDER_NAME_FOR_STUB (binder_name, stub, length);

  length = strlen (symb);
  symbol_name = XALLOCAVEC (char, length + 32);
  GEN_SYMBOL_NAME_FOR_SYMBOL (symbol_name, symb, length);

  sprintf (lazy_ptr_name, "L%d$lz", label);

  if (MACHOPIC_ATT_STUB)
    switch_to_section (darwin_sections[machopic_picsymbol_stub3_section]);
  else if (MACHOPIC_PURE)
    switch_to_section (darwin_sections[machopic_picsymbol_stub2_section]);
  else
    switch_to_section (darwin_sections[machopic_symbol_stub_section]);

  fprintf (file, "%s:\n", stub);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

  if (MACHOPIC_ATT_STUB)
    {
      fprintf (file, "\thlt ; hlt ; hlt ; hlt ; hlt\n");
    }
  else if (MACHOPIC_PURE)
    {
      /* PIC stub.  */
      /* 25-byte PIC stub using "CALL get_pc_thunk".  */
      rtx tmp = gen_rtx_REG (SImode, 2 /* ECX */);
      output_set_got (tmp, NULL_RTX);	/* "CALL ___<cpu>.get_pc_thunk.cx".  */
      fprintf (file, "LPC$%d:\tmovl\t%s-LPC$%d(%%ecx),%%ecx\n",
	       label, lazy_ptr_name, label);
      fprintf (file, "\tjmp\t*%%ecx\n");
    }
  else
    fprintf (file, "\tjmp\t*%s\n", lazy_ptr_name);

  /* The AT&T-style ("self-modifying") stub is not lazily bound, thus
     it needs no stub-binding-helper.  */
  if (MACHOPIC_ATT_STUB)
    return;

  fprintf (file, "%s:\n", binder_name);

  if (MACHOPIC_PURE)
    {
      fprintf (file, "\tlea\t%s-%s(%%ecx),%%ecx\n", lazy_ptr_name, binder_name);
      fprintf (file, "\tpushl\t%%ecx\n");
    }
  else
    fprintf (file, "\tpushl\t$%s\n", lazy_ptr_name);

  fputs ("\tjmp\tdyld_stub_binding_helper\n", file);

  /* N.B. Keep the correspondence of these
     'symbol_ptr/symbol_ptr2/symbol_ptr3' sections consistent with the
     old-pic/new-pic/non-pic stubs; altering this will break
     compatibility with existing dylibs.  */
  if (MACHOPIC_PURE)
    {
      /* 25-byte PIC stub using "CALL get_pc_thunk".  */
      switch_to_section (darwin_sections[machopic_lazy_symbol_ptr2_section]);
    }
  else
    /* 16-byte -mdynamic-no-pic stub.  */
    switch_to_section(darwin_sections[machopic_lazy_symbol_ptr3_section]);

  fprintf (file, "%s:\n", lazy_ptr_name);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);
  fprintf (file, ASM_LONG "%s\n", binder_name);
}
#endif /* TARGET_MACHO */

/* Order the registers for register allocator.  */

void
x86_order_regs_for_local_alloc (void)
{
   int pos = 0;
   int i;

   /* First allocate the local general purpose registers.  */
   for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
     if (GENERAL_REGNO_P (i) && call_used_or_fixed_reg_p (i))
	reg_alloc_order [pos++] = i;

   /* Global general purpose registers.  */
   for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
     if (GENERAL_REGNO_P (i) && !call_used_or_fixed_reg_p (i))
	reg_alloc_order [pos++] = i;

   /* x87 registers come first in case we are doing FP math
      using them.  */
   if (!TARGET_SSE_MATH)
     for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
       reg_alloc_order [pos++] = i;

   /* SSE registers.  */
   for (i = FIRST_SSE_REG; i <= LAST_SSE_REG; i++)
     reg_alloc_order [pos++] = i;
   for (i = FIRST_REX_SSE_REG; i <= LAST_REX_SSE_REG; i++)
     reg_alloc_order [pos++] = i;

   /* Extended REX SSE registers.  */
   for (i = FIRST_EXT_REX_SSE_REG; i <= LAST_EXT_REX_SSE_REG; i++)
     reg_alloc_order [pos++] = i;

   /* Mask register.  */
   for (i = FIRST_MASK_REG; i <= LAST_MASK_REG; i++)
     reg_alloc_order [pos++] = i;

   /* x87 registers.  */
   if (TARGET_SSE_MATH)
     for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
       reg_alloc_order [pos++] = i;

   for (i = FIRST_MMX_REG; i <= LAST_MMX_REG; i++)
     reg_alloc_order [pos++] = i;

   /* Initialize the rest of array as we do not allocate some registers
      at all.  */
   while (pos < FIRST_PSEUDO_REGISTER)
     reg_alloc_order [pos++] = 0;
}

static bool
ix86_ms_bitfield_layout_p (const_tree record_type)
{
  return ((TARGET_MS_BITFIELD_LAYOUT
	   && !lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (record_type)))
          || lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (record_type)));
}

/* Returns an expression indicating where the this parameter is
   located on entry to the FUNCTION.  */

static rtx
x86_this_parameter (tree function)
{
  tree type = TREE_TYPE (function);
  bool aggr = aggregate_value_p (TREE_TYPE (type), type) != 0;
  int nregs;

  if (TARGET_64BIT)
    {
      const int *parm_regs;

      if (ix86_function_type_abi (type) == MS_ABI)
        parm_regs = x86_64_ms_abi_int_parameter_registers;
      else
        parm_regs = x86_64_int_parameter_registers;
      return gen_rtx_REG (Pmode, parm_regs[aggr]);
    }

  nregs = ix86_function_regparm (type, function);

  if (nregs > 0 && !stdarg_p (type))
    {
      int regno;
      unsigned int ccvt = ix86_get_callcvt (type);

      if ((ccvt & IX86_CALLCVT_FASTCALL) != 0)
	regno = aggr ? DX_REG : CX_REG;
      else if ((ccvt & IX86_CALLCVT_THISCALL) != 0)
        {
	  regno = CX_REG;
	  if (aggr)
	    return gen_rtx_MEM (SImode,
				plus_constant (Pmode, stack_pointer_rtx, 4));
	}
      else
        {
	  regno = AX_REG;
	  if (aggr)
	    {
	      regno = DX_REG;
	      if (nregs == 1)
		return gen_rtx_MEM (SImode,
				    plus_constant (Pmode,
						   stack_pointer_rtx, 4));
	    }
	}
      return gen_rtx_REG (SImode, regno);
    }

  return gen_rtx_MEM (SImode, plus_constant (Pmode, stack_pointer_rtx,
					     aggr ? 8 : 4));
}

/* Determine whether x86_output_mi_thunk can succeed.  */

static bool
x86_can_output_mi_thunk (const_tree, HOST_WIDE_INT, HOST_WIDE_INT vcall_offset,
			 const_tree function)
{
  /* 64-bit can handle anything.  */
  if (TARGET_64BIT)
    return true;

  /* For 32-bit, everything's fine if we have one free register.  */
  if (ix86_function_regparm (TREE_TYPE (function), function) < 3)
    return true;

  /* Need a free register for vcall_offset.  */
  if (vcall_offset)
    return false;

  /* Need a free register for GOT references.  */
  if (flag_pic && !targetm.binds_local_p (function))
    return false;

  /* Otherwise ok.  */
  return true;
}

/* Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is nonzero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
x86_output_mi_thunk (FILE *file, tree thunk_fndecl, HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset, tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_param = x86_this_parameter (function);
  rtx this_reg, tmp, fnaddr;
  unsigned int tmp_regno;
  rtx_insn *insn;
  int saved_flag_force_indirect_call = flag_force_indirect_call;

  if (TARGET_64BIT)
    tmp_regno = R10_REG;
  else
    {
      unsigned int ccvt = ix86_get_callcvt (TREE_TYPE (function));
      if ((ccvt & IX86_CALLCVT_FASTCALL) != 0)
	tmp_regno = AX_REG;
      else if ((ccvt & IX86_CALLCVT_THISCALL) != 0)
	tmp_regno = DX_REG;
      else
	tmp_regno = CX_REG;

      if (flag_pic)
  flag_force_indirect_call = 0;
    }

  emit_note (NOTE_INSN_PROLOGUE_END);

  /* CET is enabled, insert EB instruction.  */
  if ((flag_cf_protection & CF_BRANCH))
    emit_insn (gen_nop_endbr ());

  /* If VCALL_OFFSET, we'll need THIS in a register.  Might as well
     pull it in now and let DELTA benefit.  */
  if (REG_P (this_param))
    this_reg = this_param;
  else if (vcall_offset)
    {
      /* Put the this parameter into %eax.  */
      this_reg = gen_rtx_REG (Pmode, AX_REG);
      emit_move_insn (this_reg, this_param);
    }
  else
    this_reg = NULL_RTX;

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      rtx delta_rtx = GEN_INT (delta);
      rtx delta_dst = this_reg ? this_reg : this_param;

      if (TARGET_64BIT)
	{
	  if (!x86_64_general_operand (delta_rtx, Pmode))
	    {
	      tmp = gen_rtx_REG (Pmode, tmp_regno);
	      emit_move_insn (tmp, delta_rtx);
	      delta_rtx = tmp;
	    }
	}

      ix86_emit_binop (PLUS, Pmode, delta_dst, delta_rtx);
    }

  /* Adjust the this parameter by a value stored in the vtable.  */
  if (vcall_offset)
    {
      rtx vcall_addr, vcall_mem, this_mem;

      tmp = gen_rtx_REG (Pmode, tmp_regno);

      this_mem = gen_rtx_MEM (ptr_mode, this_reg);
      if (Pmode != ptr_mode)
	this_mem = gen_rtx_ZERO_EXTEND (Pmode, this_mem);
      emit_move_insn (tmp, this_mem);

      /* Adjust the this parameter.  */
      vcall_addr = plus_constant (Pmode, tmp, vcall_offset);
      if (TARGET_64BIT
	  && !ix86_legitimate_address_p (ptr_mode, vcall_addr, true))
	{
	  rtx tmp2 = gen_rtx_REG (Pmode, R11_REG);
	  emit_move_insn (tmp2, GEN_INT (vcall_offset));
	  vcall_addr = gen_rtx_PLUS (Pmode, tmp, tmp2);
	}

      vcall_mem = gen_rtx_MEM (ptr_mode, vcall_addr);
      if (Pmode != ptr_mode)
	emit_insn (gen_addsi_1_zext (this_reg,
				     gen_rtx_REG (ptr_mode,
						  REGNO (this_reg)),
				     vcall_mem));
      else
	ix86_emit_binop (PLUS, Pmode, this_reg, vcall_mem);
    }

  /* If necessary, drop THIS back to its stack slot.  */
  if (this_reg && this_reg != this_param)
    emit_move_insn (this_param, this_reg);

  fnaddr = XEXP (DECL_RTL (function), 0);
  if (TARGET_64BIT)
    {
      if (!flag_pic || targetm.binds_local_p (function)
	  || TARGET_PECOFF)
	;
      else
	{
	  tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, fnaddr), UNSPEC_GOTPCREL);
	  tmp = gen_rtx_CONST (Pmode, tmp);
	  fnaddr = gen_const_mem (Pmode, tmp);
	}
    }
  else
    {
      if (!flag_pic || targetm.binds_local_p (function))
	;
#if TARGET_MACHO
      else if (TARGET_MACHO)
	{
	  fnaddr = machopic_indirect_call_target (DECL_RTL (function));
	  fnaddr = XEXP (fnaddr, 0);
	}
#endif /* TARGET_MACHO */
      else
	{
	  tmp = gen_rtx_REG (Pmode, CX_REG);
	  output_set_got (tmp, NULL_RTX);

	  fnaddr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, fnaddr), UNSPEC_GOT);
	  fnaddr = gen_rtx_CONST (Pmode, fnaddr);
	  fnaddr = gen_rtx_PLUS (Pmode, tmp, fnaddr);
	  fnaddr = gen_const_mem (Pmode, fnaddr);
	}
    }

  /* Our sibling call patterns do not allow memories, because we have no
     predicate that can distinguish between frame and non-frame memory.
     For our purposes here, we can get away with (ab)using a jump pattern,
     because we're going to do no optimization.  */
  if (MEM_P (fnaddr))
    {
      if (sibcall_insn_operand (fnaddr, word_mode))
	{
	  fnaddr = XEXP (DECL_RTL (function), 0);
	  tmp = gen_rtx_MEM (QImode, fnaddr);
	  tmp = gen_rtx_CALL (VOIDmode, tmp, const0_rtx);
	  tmp = emit_call_insn (tmp);
	  SIBLING_CALL_P (tmp) = 1;
	}
      else
	emit_jump_insn (gen_indirect_jump (fnaddr));
    }
  else
    {
      if (ix86_cmodel == CM_LARGE_PIC && SYMBOLIC_CONST (fnaddr))
	{
	  // CM_LARGE_PIC always uses pseudo PIC register which is
	  // uninitialized.  Since FUNCTION is local and calling it
	  // doesn't go through PLT, we use scratch register %r11 as
	  // PIC register and initialize it here.
	  pic_offset_table_rtx = gen_rtx_REG (Pmode, R11_REG);
	  ix86_init_large_pic_reg (tmp_regno);
	  fnaddr = legitimize_pic_address (fnaddr,
					   gen_rtx_REG (Pmode, tmp_regno));
	}

      if (!sibcall_insn_operand (fnaddr, word_mode))
	{
	  tmp = gen_rtx_REG (word_mode, tmp_regno);
	  if (GET_MODE (fnaddr) != word_mode)
	    fnaddr = gen_rtx_ZERO_EXTEND (word_mode, fnaddr);
	  emit_move_insn (tmp, fnaddr);
	  fnaddr = tmp;
	}

      tmp = gen_rtx_MEM (QImode, fnaddr);
      tmp = gen_rtx_CALL (VOIDmode, tmp, const0_rtx);
      tmp = emit_call_insn (tmp);
      SIBLING_CALL_P (tmp) = 1;
    }
  emit_barrier ();

  /* Emit just enough of rest_of_compilation to get the insns emitted.  */
  insn = get_insns ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  flag_force_indirect_call = saved_flag_force_indirect_call;
}

static void
x86_file_start (void)
{
  default_file_start ();
  if (TARGET_16BIT)
    fputs ("\t.code16gcc\n", asm_out_file);
#if TARGET_MACHO
  darwin_file_start ();
#endif
  if (X86_FILE_START_VERSION_DIRECTIVE)
    fputs ("\t.version\t\"01.01\"\n", asm_out_file);
  if (X86_FILE_START_FLTUSED)
    fputs ("\t.global\t__fltused\n", asm_out_file);
  if (ix86_asm_dialect == ASM_INTEL)
    fputs ("\t.intel_syntax noprefix\n", asm_out_file);
}

int
x86_field_alignment (tree type, int computed)
{
  machine_mode mode;

  if (TARGET_64BIT || TARGET_ALIGN_DOUBLE)
    return computed;
  if (TARGET_IAMCU)
    return iamcu_alignment (type, computed);
  type = strip_array_types (type);
  mode = TYPE_MODE (type);
  if (mode == DFmode || mode == DCmode
      || GET_MODE_CLASS (mode) == MODE_INT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    {
      if (TYPE_ATOMIC (type) && computed > 32)
	{
	  static bool warned;

	  if (!warned && warn_psabi)
	    {
	      const char *url
		= CHANGES_ROOT_URL "gcc-11/changes.html#ia32_atomic";

	      warned = true;
	      inform (input_location, "the alignment of %<_Atomic %T%> "
				      "fields changed in %{GCC 11.1%}",
		      TYPE_MAIN_VARIANT (type), url);
	    }
	}
      else
      return MIN (32, computed);
    }
  return computed;
}

/* Print call to TARGET to FILE.  */

static void
x86_print_call_or_nop (FILE *file, const char *target)
{
  if (flag_nop_mcount || !strcmp (target, "nop"))
    /* 5 byte nop: nopl 0(%[re]ax,%[re]ax,1) */
    fprintf (file, "1:" ASM_BYTE "0x0f, 0x1f, 0x44, 0x00, 0x00\n");
  else
    fprintf (file, "1:\tcall\t%s\n", target);
}

static bool
current_fentry_name (const char **name)
{
  tree attr = lookup_attribute ("fentry_name",
				DECL_ATTRIBUTES (current_function_decl));
  if (!attr)
    return false;
  *name = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)));
  return true;
}

static bool
current_fentry_section (const char **name)
{
  tree attr = lookup_attribute ("fentry_section",
				DECL_ATTRIBUTES (current_function_decl));
  if (!attr)
    return false;
  *name = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)));
  return true;
}

/* Return a caller-saved register which isn't live or a callee-saved
   register which has been saved on stack in the prologue at entry for
   profile.  */

static int
x86_64_select_profile_regnum (bool r11_ok ATTRIBUTE_UNUSED)
{
  /* Use %r10 if the profiler is emitted before the prologue or it isn't
     used by DRAP.  */
  if (ix86_profile_before_prologue ()
      || !crtl->drap_reg
      || REGNO (crtl->drap_reg) != R10_REG)
    return R10_REG;

  /* The profiler is emitted after the prologue.  If there is a
     caller-saved register which isn't live or a callee-saved
     register saved on stack in the prologue, use it.  */

  bitmap reg_live = df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  int i;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (GENERAL_REGNO_P (i)
	&& i != R10_REG
#ifdef NO_PROFILE_COUNTERS
	&& (r11_ok || i != R11_REG)
#else
	&& i != R11_REG
#endif
	&& TEST_HARD_REG_BIT (accessible_reg_set, i)
	&& (ix86_save_reg (i, true, true)
	    || (call_used_regs[i]
		&& !fixed_regs[i]
		&& !REGNO_REG_SET_P (reg_live, i))))
      return i;

  sorry ("no register available for profiling %<-mcmodel=large%s%>",
	 ix86_cmodel == CM_LARGE_PIC ? " -fPIC" : "");

  return R10_REG;
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
void
x86_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  if (cfun->machine->insn_queued_at_entrance)
    {
      if (cfun->machine->insn_queued_at_entrance == TYPE_ENDBR)
	fprintf (file, "\t%s\n", TARGET_64BIT ? "endbr64" : "endbr32");
      unsigned int patch_area_size
	= crtl->patch_area_size - crtl->patch_area_entry;
      if (patch_area_size)
	ix86_output_patchable_area (patch_area_size,
				    crtl->patch_area_entry == 0);
    }

  const char *mcount_name = MCOUNT_NAME;

  if (current_fentry_name (&mcount_name))
    ;
  else if (fentry_name)
    mcount_name = fentry_name;
  else if (flag_fentry)
    mcount_name = MCOUNT_NAME_BEFORE_PROLOGUE;

  if (TARGET_64BIT)
    {
#ifndef NO_PROFILE_COUNTERS
      if (ASSEMBLER_DIALECT == ASM_INTEL)
	fprintf (file, "\tlea\tr11, %sP%d[rip]\n", LPREFIX, labelno);
      else
	fprintf (file, "\tleaq\t%sP%d(%%rip), %%r11\n", LPREFIX, labelno);
#endif

      int scratch;
      const char *reg;
      char legacy_reg[4] = { 0 };

      if (!TARGET_PECOFF)
	{
	  switch (ix86_cmodel)
	    {
	    case CM_LARGE:
	      scratch = x86_64_select_profile_regnum (true);
	      reg = hi_reg_name[scratch];
	      if (LEGACY_INT_REGNO_P (scratch))
		{
		  legacy_reg[0] = 'r';
		  legacy_reg[1] = reg[0];
		  legacy_reg[2] = reg[1];
		  reg = legacy_reg;
		}
	      if (ASSEMBLER_DIALECT == ASM_INTEL)
		fprintf (file, "1:\tmovabs\t%s, OFFSET FLAT:%s\n"
			       "\tcall\t%s\n", reg, mcount_name, reg);
	      else
		fprintf (file, "1:\tmovabsq\t$%s, %%%s\n\tcall\t*%%%s\n",
			 mcount_name, reg, reg);
	      break;
	    case CM_LARGE_PIC:
#ifdef NO_PROFILE_COUNTERS
	      scratch = x86_64_select_profile_regnum (false);
	      reg = hi_reg_name[scratch];
	      if (LEGACY_INT_REGNO_P (scratch))
		{
		  legacy_reg[0] = 'r';
		  legacy_reg[1] = reg[0];
		  legacy_reg[2] = reg[1];
		  reg = legacy_reg;
		}
	      if (ASSEMBLER_DIALECT == ASM_INTEL)
		{
		  fprintf (file, "1:movabs\tr11, "
				 "OFFSET FLAT:_GLOBAL_OFFSET_TABLE_-1b\n");
		  fprintf (file, "\tlea\t%s, 1b[rip]\n", reg);
		  fprintf (file, "\tadd\t%s, r11\n", reg);
		  fprintf (file, "\tmovabs\tr11, OFFSET FLAT:%s@PLTOFF\n",
			   mcount_name);
		  fprintf (file, "\tadd\t%s, r11\n", reg);
		  fprintf (file, "\tcall\t%s\n", reg);
		  break;
		}
	      fprintf (file,
		       "1:\tmovabsq\t$_GLOBAL_OFFSET_TABLE_-1b, %%r11\n");
	      fprintf (file, "\tleaq\t1b(%%rip), %%%s\n", reg);
	      fprintf (file, "\taddq\t%%r11, %%%s\n", reg);
	      fprintf (file, "\tmovabsq\t$%s@PLTOFF, %%r11\n", mcount_name);
	      fprintf (file, "\taddq\t%%r11, %%%s\n", reg);
	      fprintf (file, "\tcall\t*%%%s\n", reg);
#else
	      sorry ("profiling %<-mcmodel=large%> with PIC is not supported");
#endif
	      break;
	    case CM_SMALL_PIC:
	    case CM_MEDIUM_PIC:
	      if (!ix86_direct_extern_access)
		{
		  if (ASSEMBLER_DIALECT == ASM_INTEL)
		    fprintf (file, "1:\tcall\t[QWORD PTR %s@GOTPCREL[rip]]\n",
			     mcount_name);
		  else
		    fprintf (file, "1:\tcall\t*%s@GOTPCREL(%%rip)\n",
			     mcount_name);
		  break;
		}
	      /* fall through */
	    default:
	      x86_print_call_or_nop (file, mcount_name);
	      break;
	    }
	}
      else
	x86_print_call_or_nop (file, mcount_name);
    }
  else if (flag_pic)
    {
#ifndef NO_PROFILE_COUNTERS
      if (ASSEMBLER_DIALECT == ASM_INTEL)
	fprintf (file,
		 "\tlea\t" PROFILE_COUNT_REGISTER ", %sP%d@GOTOFF[ebx]\n",
		 LPREFIX, labelno);
      else
	fprintf (file,
		 "\tleal\t%sP%d@GOTOFF(%%ebx), %%" PROFILE_COUNT_REGISTER "\n",
		 LPREFIX, labelno);
#endif
      if (ASSEMBLER_DIALECT == ASM_INTEL)
	fprintf (file, "1:\tcall\t[DWORD PTR %s@GOT[ebx]]\n", mcount_name);
      else
	fprintf (file, "1:\tcall\t*%s@GOT(%%ebx)\n", mcount_name);
    }
  else
    {
#ifndef NO_PROFILE_COUNTERS
      if (ASSEMBLER_DIALECT == ASM_INTEL)
	fprintf (file,
		 "\tmov\t" PROFILE_COUNT_REGISTER ", OFFSET FLAT:%sP%d\n",
		 LPREFIX, labelno);
      else
	fprintf (file, "\tmovl\t$%sP%d, %%" PROFILE_COUNT_REGISTER "\n",
		 LPREFIX, labelno);
#endif
      x86_print_call_or_nop (file, mcount_name);
    }

  if (flag_record_mcount
      || lookup_attribute ("fentry_section",
			   DECL_ATTRIBUTES (current_function_decl)))
    {
      const char *sname = "__mcount_loc";

      if (current_fentry_section (&sname))
	;
      else if (fentry_section)
	sname = fentry_section;

      fprintf (file, "\t.section %s, \"a\",@progbits\n", sname);
      fprintf (file, "\t.%s 1b\n", TARGET_64BIT ? "quad" : "long");
      fprintf (file, "\t.previous\n");
    }
}

/* We don't have exact information about the insn sizes, but we may assume
   quite safely that we are informed about all 1 byte insns and memory
   address sizes.  This is enough to eliminate unnecessary padding in
   99% of cases.  */

int
ix86_min_insn_size (rtx_insn *insn)
{
  int l = 0, len;

  if (!INSN_P (insn) || !active_insn_p (insn))
    return 0;

  /* Discard alignments we've emit and jump instructions.  */
  if (GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
      && XINT (PATTERN (insn), 1) == UNSPECV_ALIGN)
    return 0;

  /* Important case - calls are always 5 bytes.
     It is common to have many calls in the row.  */
  if (CALL_P (insn)
      && symbolic_reference_mentioned_p (PATTERN (insn))
      && !SIBLING_CALL_P (insn))
    return 5;
  len = get_attr_length (insn);
  if (len <= 1)
    return 1;

  /* For normal instructions we rely on get_attr_length being exact,
     with a few exceptions.  */
  if (!JUMP_P (insn))
    {
      enum attr_type type = get_attr_type (insn);

      switch (type)
	{
	case TYPE_MULTI:
	  if (GET_CODE (PATTERN (insn)) == ASM_INPUT
	      || asm_noperands (PATTERN (insn)) >= 0)
	    return 0;
	  break;
	case TYPE_OTHER:
	case TYPE_FCMP:
	  break;
	default:
	  /* Otherwise trust get_attr_length.  */
	  return len;
	}

      l = get_attr_length_address (insn);
      if (l < 4 && symbolic_reference_mentioned_p (PATTERN (insn)))
	l = 4;
    }
  if (l)
    return 1+l;
  else
    return 2;
}

#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN

/* AMD K8 core mispredicts jumps when there are more than 3 jumps in 16 byte
   window.  */

static void
ix86_avoid_jump_mispredicts (void)
{
  rtx_insn *insn, *start = get_insns ();
  int nbytes = 0, njumps = 0;
  bool isjump = false;

  /* Look for all minimal intervals of instructions containing 4 jumps.
     The intervals are bounded by START and INSN.  NBYTES is the total
     size of instructions in the interval including INSN and not including
     START.  When the NBYTES is smaller than 16 bytes, it is possible
     that the end of START and INSN ends up in the same 16byte page.

     The smallest offset in the page INSN can start is the case where START
     ends on the offset 0.  Offset of INSN is then NBYTES - sizeof (INSN).
     We add p2align to 16byte window with maxskip 15 - NBYTES + sizeof (INSN).

     Don't consider asm goto as jump, while it can contain a jump, it doesn't
     have to, control transfer to label(s) can be performed through other
     means, and also we estimate minimum length of all asm stmts as 0.  */
  for (insn = start; insn; insn = NEXT_INSN (insn))
    {
      int min_size;

      if (LABEL_P (insn))
	{
	  align_flags alignment = label_to_alignment (insn);
	  int align = alignment.levels[0].log;
	  int max_skip = alignment.levels[0].maxskip;

	  if (max_skip > 15)
	    max_skip = 15;
	  /* If align > 3, only up to 16 - max_skip - 1 bytes can be
	     already in the current 16 byte page, because otherwise
	     ASM_OUTPUT_MAX_SKIP_ALIGN could skip max_skip or fewer
	     bytes to reach 16 byte boundary.  */
	  if (align <= 0
	      || (align <= 3 && max_skip != (1 << align) - 1))
	    max_skip = 0;
	  if (dump_file)
	    fprintf (dump_file, "Label %i with max_skip %i\n",
		     INSN_UID (insn), max_skip);
	  if (max_skip)
	    {
	      while (nbytes + max_skip >= 16)
		{
		  start = NEXT_INSN (start);
		  if ((JUMP_P (start) && asm_noperands (PATTERN (start)) < 0)
		      || CALL_P (start))
		    njumps--, isjump = true;
		  else
		    isjump = false;
		  nbytes -= ix86_min_insn_size (start);
		}
	    }
	  continue;
	}

      min_size = ix86_min_insn_size (insn);
      nbytes += min_size;
      if (dump_file)
	fprintf (dump_file, "Insn %i estimated to %i bytes\n",
		 INSN_UID (insn), min_size);
      if ((JUMP_P (insn) && asm_noperands (PATTERN (insn)) < 0)
	  || CALL_P (insn))
	njumps++;
      else
	continue;

      while (njumps > 3)
	{
	  start = NEXT_INSN (start);
	  if ((JUMP_P (start) && asm_noperands (PATTERN (start)) < 0)
	      || CALL_P (start))
	    njumps--, isjump = true;
	  else
	    isjump = false;
	  nbytes -= ix86_min_insn_size (start);
	}
      gcc_assert (njumps >= 0);
      if (dump_file)
        fprintf (dump_file, "Interval %i to %i has %i bytes\n",
		 INSN_UID (start), INSN_UID (insn), nbytes);

      if (njumps == 3 && isjump && nbytes < 16)
	{
	  int padsize = 15 - nbytes + ix86_min_insn_size (insn);

	  if (dump_file)
	    fprintf (dump_file, "Padding insn %i by %i bytes!\n",
		     INSN_UID (insn), padsize);
	  emit_insn_before (gen_max_skip_align (GEN_INT (4), GEN_INT (padsize)), insn);
	}
    }
}
#endif

/* AMD Athlon works faster
   when RET is not destination of conditional jump or directly preceded
   by other jump instruction.  We avoid the penalty by inserting NOP just
   before the RET instructions in such cases.  */
static void
ix86_pad_returns (void)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      basic_block bb = e->src;
      rtx_insn *ret = BB_END (bb);
      rtx_insn *prev;
      bool replace = false;

      if (!JUMP_P (ret) || !ANY_RETURN_P (PATTERN (ret))
	  || optimize_bb_for_size_p (bb))
	continue;
      for (prev = PREV_INSN (ret); prev; prev = PREV_INSN (prev))
	if (active_insn_p (prev) || LABEL_P (prev))
	  break;
      if (prev && LABEL_P (prev))
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (EDGE_FREQUENCY (e) && e->src->index >= 0
		&& !(e->flags & EDGE_FALLTHRU))
	      {
		replace = true;
		break;
	      }
	}
      if (!replace)
	{
	  prev = prev_active_insn (ret);
	  if (prev
	      && ((JUMP_P (prev) && any_condjump_p (prev))
		  || CALL_P (prev)))
	    replace = true;
	  /* Empty functions get branch mispredict even when
	     the jump destination is not visible to us.  */
	  if (!prev && !optimize_function_for_size_p (cfun))
	    replace = true;
	}
      if (replace)
	{
	  emit_jump_insn_before (gen_simple_return_internal_long (), ret);
	  delete_insn (ret);
	}
    }
}

/* Count the minimum number of instructions in BB.  Return 4 if the
   number of instructions >= 4.  */

static int
ix86_count_insn_bb (basic_block bb)
{
  rtx_insn *insn;
  int insn_count = 0;

  /* Count number of instructions in this block.  Return 4 if the number
     of instructions >= 4.  */
  FOR_BB_INSNS (bb, insn)
    {
      /* Only happen in exit blocks.  */
      if (JUMP_P (insn)
	  && ANY_RETURN_P (PATTERN (insn)))
	break;

      if (NONDEBUG_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER)
	{
	  insn_count++;
	  if (insn_count >= 4)
	    return insn_count;
	}
    }

  return insn_count;
}


/* Count the minimum number of instructions in code path in BB.
   Return 4 if the number of instructions >= 4.  */

static int
ix86_count_insn (basic_block bb)
{
  edge e;
  edge_iterator ei;
  int min_prev_count;

  /* Only bother counting instructions along paths with no
     more than 2 basic blocks between entry and exit.  Given
     that BB has an edge to exit, determine if a predecessor
     of BB has an edge from entry.  If so, compute the number
     of instructions in the predecessor block.  If there
     happen to be multiple such blocks, compute the minimum.  */
  min_prev_count = 4;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      edge prev_e;
      edge_iterator prev_ei;

      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  min_prev_count = 0;
	  break;
	}
      FOR_EACH_EDGE (prev_e, prev_ei, e->src->preds)
	{
	  if (prev_e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    {
	      int count = ix86_count_insn_bb (e->src);
	      if (count < min_prev_count)
		min_prev_count = count;
	      break;
	    }
	}
    }

  if (min_prev_count < 4)
    min_prev_count += ix86_count_insn_bb (bb);

  return min_prev_count;
}

/* Pad short function to 4 instructions.   */

static void
ix86_pad_short_function (void)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      rtx_insn *ret = BB_END (e->src);
      if (JUMP_P (ret) && ANY_RETURN_P (PATTERN (ret)))
	{
	  int insn_count = ix86_count_insn (e->src);

	  /* Pad short function.  */
	  if (insn_count < 4)
	    {
	      rtx_insn *insn = ret;

	      /* Find epilogue.  */
	      while (insn
		     && (!NOTE_P (insn)
			 || NOTE_KIND (insn) != NOTE_INSN_EPILOGUE_BEG))
		insn = PREV_INSN (insn);

	      if (!insn)
		insn = ret;

	      /* Two NOPs count as one instruction.  */
	      insn_count = 2 * (4 - insn_count);
	      emit_insn_before (gen_nops (GEN_INT (insn_count)), insn);
	    }
	}
    }
}

/* Fix up a Windows system unwinder issue.  If an EH region falls through into
   the epilogue, the Windows system unwinder will apply epilogue logic and
   produce incorrect offsets.  This can be avoided by adding a nop between
   the last insn that can throw and the first insn of the epilogue.  */

static void
ix86_seh_fixup_eh_fallthru (void)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      rtx_insn *insn, *next;

      /* Find the beginning of the epilogue.  */
      for (insn = BB_END (e->src); insn != NULL; insn = PREV_INSN (insn))
	if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_EPILOGUE_BEG)
	  break;
      if (insn == NULL)
	continue;

      /* We only care about preceding insns that can throw.  */
      insn = prev_active_insn (insn);
      if (insn == NULL || !can_throw_internal (insn))
	continue;

      /* Do not separate calls from their debug information.  */
      for (next = NEXT_INSN (insn); next != NULL; next = NEXT_INSN (next))
	if (NOTE_P (next) && NOTE_KIND (next) == NOTE_INSN_VAR_LOCATION)
	  insn = next;
	else
	  break;

      emit_insn_after (gen_nops (const1_rtx), insn);
    }
}
/* Split vector load from parm_decl to elemental loads to avoid STLF
   stalls.  */
static void
ix86_split_stlf_stall_load ()
{
  rtx_insn* insn, *start = get_insns ();
  unsigned window = 0;

  for (insn = start; insn; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;
      window++;
      /* Insert 64 vaddps %xmm18, %xmm19, %xmm20(no dependence between each
	 other, just emulate for pipeline) before stalled load, stlf stall
	 case is as fast as no stall cases on CLX.
	 Since CFG is freed before machine_reorg, just do a rough
	 calculation of the window according to the layout.  */
      if (window > (unsigned) x86_stlf_window_ninsns)
	return;

      if (any_uncondjump_p (insn)
	  || ANY_RETURN_P (PATTERN (insn))
	  || CALL_P (insn))
	return;

      rtx set = single_set (insn);
      if (!set)
	continue;
      rtx src = SET_SRC (set);
      if (!MEM_P (src)
	  /* Only handle V2DFmode load since it doesn't need any scratch
	     register.  */
	  || GET_MODE (src) != E_V2DFmode
	  || !MEM_EXPR (src)
	  || TREE_CODE (get_base_address (MEM_EXPR (src))) != PARM_DECL)
	continue;

      rtx zero = CONST0_RTX (V2DFmode);
      rtx dest = SET_DEST (set);
      rtx m = adjust_address (src, DFmode, 0);
      rtx loadlpd = gen_sse2_loadlpd (dest, zero, m);
      emit_insn_before (loadlpd, insn);
      m = adjust_address (src, DFmode, 8);
      rtx loadhpd = gen_sse2_loadhpd (dest, dest, m);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fputs ("Due to potential STLF stall, split instruction:\n",
		 dump_file);
	  print_rtl_single (dump_file, insn);
	  fputs ("To:\n", dump_file);
	  print_rtl_single (dump_file, loadlpd);
	  print_rtl_single (dump_file, loadhpd);
	}
      PATTERN (insn) = loadhpd;
      INSN_CODE (insn) = -1;
      gcc_assert (recog_memoized (insn) != -1);
    }
}

/* When a hot loop can be fit into one cacheline,
   force align the loop without considering the max skip.  */
static void
ix86_align_loops ()
{
  basic_block bb;

  /* Don't do this when we don't know cache line size.  */
  if (ix86_cost->prefetch_block == 0)
    return;

  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  profile_count count_threshold = cfun->cfg->count_max / param_align_threshold;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *label = BB_HEAD (bb);
      bool has_fallthru = 0;
      edge e;
      edge_iterator ei;

      if (!LABEL_P (label))
	continue;

      profile_count fallthru_count = profile_count::zero ();
      profile_count branch_count = profile_count::zero ();

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    has_fallthru = 1, fallthru_count += e->count ();
	  else
	    branch_count += e->count ();
	}

      if (!fallthru_count.initialized_p () || !branch_count.initialized_p ())
	continue;

      if (bb->loop_father
	  && bb->loop_father->latch != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && (has_fallthru
	      ? (!(single_succ_p (bb)
		   && single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun))
		 && optimize_bb_for_speed_p (bb)
		 && branch_count + fallthru_count > count_threshold
		 && (branch_count > fallthru_count * param_align_loop_iterations))
	      /* In case there'no fallthru for the loop.
		 Nops inserted won't be executed.  */
	      : (branch_count > count_threshold
		 || (bb->count > bb->prev_bb->count * 10
		     && (bb->prev_bb->count
			 <= ENTRY_BLOCK_PTR_FOR_FN (cfun)->count / 2)))))
	{
	  rtx_insn* insn, *end_insn;
	  HOST_WIDE_INT size = 0;
	  bool padding_p = true;
	  basic_block tbb = bb;
	  unsigned cond_branch_num = 0;
	  bool detect_tight_loop_p = false;

	  for (unsigned int i = 0; i != bb->loop_father->num_nodes;
	       i++, tbb = tbb->next_bb)
	    {
	      /* Only handle continuous cfg layout. */
	      if (bb->loop_father != tbb->loop_father)
		{
		  padding_p = false;
		  break;
		}

	      FOR_BB_INSNS (tbb, insn)
		{
		  if (!NONDEBUG_INSN_P (insn))
		    continue;
		  size += ix86_min_insn_size (insn);

		  /* We don't know size of inline asm.
		     Don't align loop for call.  */
		  if (asm_noperands (PATTERN (insn)) >= 0
		      || CALL_P (insn))
		    {
		      size = -1;
		      break;
		    }
		}

	      if (size == -1 || size > ix86_cost->prefetch_block)
		{
		  padding_p = false;
		  break;
		}

	      FOR_EACH_EDGE (e, ei, tbb->succs)
		{
		  /* It could be part of the loop.  */
		  if (e->dest == bb)
		    {
		      detect_tight_loop_p = true;
		      break;
		    }
		}

	      if (detect_tight_loop_p)
		break;

	      end_insn = BB_END (tbb);
	      if (JUMP_P (end_insn))
		{
		  /* For decoded icache:
		     1. Up to two branches are allowed per Way.
		     2. A non-conditional branch is the last micro-op in a Way.
		  */
		  if (onlyjump_p (end_insn)
		      && (any_uncondjump_p (end_insn)
			  || single_succ_p (tbb)))
		    {
		      padding_p = false;
		      break;
		    }
		  else if (++cond_branch_num >= 2)
		    {
		      padding_p = false;
		      break;
		    }
		}

	    }

	  if (padding_p && detect_tight_loop_p)
	    {
	      emit_insn_before (gen_max_skip_align (GEN_INT (ceil_log2 (size)),
						    GEN_INT (0)), label);
	      /* End of function.  */
	      if (!tbb || tbb == EXIT_BLOCK_PTR_FOR_FN (cfun))
		break;
	      /* Skip bb which already fits into one cacheline.  */
	      bb = tbb;
	    }
	}
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
}

/* Implement machine specific optimizations.  We implement padding of returns
   for K8 CPUs and pass to avoid 4 jumps in the single 16 byte window.  */
static void
ix86_reorg (void)
{
  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  if (TARGET_SEH && current_function_has_exception_handlers ())
    ix86_seh_fixup_eh_fallthru ();

  if (optimize && optimize_function_for_speed_p (cfun))
    {
      if (TARGET_SSE2)
	ix86_split_stlf_stall_load ();
      if (TARGET_PAD_SHORT_FUNCTION)
	ix86_pad_short_function ();
      else if (TARGET_PAD_RETURNS)
	ix86_pad_returns ();
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
      if (TARGET_FOUR_JUMP_LIMIT)
	ix86_avoid_jump_mispredicts ();

      ix86_align_loops ();
#endif
    }
}

/* Return nonzero when QImode register that must be represented via REX prefix
   is used.  */
bool
x86_extended_QIreg_mentioned_p (rtx_insn *insn)
{
  int i;
  extract_insn_cached (insn);
  for (i = 0; i < recog_data.n_operands; i++)
    if (GENERAL_REG_P (recog_data.operand[i])
	&& !QI_REGNO_P (REGNO (recog_data.operand[i])))
       return true;
  return false;
}

/* Return true when INSN mentions register that must be encoded using REX
   prefix.  */
bool
x86_extended_reg_mentioned_p (rtx insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, INSN_P (insn) ? PATTERN (insn) : insn, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x)
	  && (REX_INT_REGNO_P (REGNO (x)) || REX_SSE_REGNO_P (REGNO (x))
	      || REX2_INT_REGNO_P (REGNO (x))))
	return true;
    }
  return false;
}

/* Return true when INSN mentions register that must be encoded using REX2
   prefix.  */
bool
x86_extended_rex2reg_mentioned_p (rtx insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, INSN_P (insn) ? PATTERN (insn) : insn, NONCONST)
    {
      const_rtx x = *iter;
      if (REG_P (x) && REX2_INT_REGNO_P (REGNO (x)))
	return true;
    }
  return false;
}

/* Return true when rtx operands mentions register that must be encoded using
   evex prefix.  */
bool
x86_evex_reg_mentioned_p (rtx operands[], int nops)
{
  int i;
  for (i = 0; i < nops; i++)
    if (EXT_REX_SSE_REG_P (operands[i])
	|| x86_extended_rex2reg_mentioned_p (operands[i]))
      return true;
  return false;
}

/* If profitable, negate (without causing overflow) integer constant
   of mode MODE at location LOC.  Return true in this case.  */
bool
x86_maybe_negate_const_int (rtx *loc, machine_mode mode)
{
  HOST_WIDE_INT val;

  if (!CONST_INT_P (*loc))
    return false;

  switch (mode)
    {
    case E_DImode:
      /* DImode x86_64 constants must fit in 32 bits.  */
      gcc_assert (x86_64_immediate_operand (*loc, mode));

      mode = SImode;
      break;

    case E_SImode:
    case E_HImode:
    case E_QImode:
      break;

    default:
      gcc_unreachable ();
    }

  /* Avoid overflows.  */
  if (mode_signbit_p (mode, *loc))
    return false;

  val = INTVAL (*loc);

  /* Make things pretty and `subl $4,%eax' rather than `addl $-4,%eax'.
     Exceptions: -128 encodes smaller than 128, so swap sign and op.  */
  if ((val < 0 && val != -128)
      || val == 128)
    {
      *loc = GEN_INT (-val);
      return true;
    }

  return false;
}

/* Generate an unsigned DImode/SImode to FP conversion.  This is the same code
   optabs would emit if we didn't have TFmode patterns.  */

void
x86_emit_floatuns (rtx operands[2])
{
  rtx_code_label *neglab, *donelab;
  rtx i0, i1, f0, in, out;
  machine_mode mode, inmode;

  inmode = GET_MODE (operands[1]);
  gcc_assert (inmode == SImode || inmode == DImode);

  out = operands[0];
  in = force_reg (inmode, operands[1]);
  mode = GET_MODE (out);
  neglab = gen_label_rtx ();
  donelab = gen_label_rtx ();
  f0 = gen_reg_rtx (mode);

  emit_cmp_and_jump_insns (in, const0_rtx, LT, const0_rtx, inmode, 0, neglab);

  expand_float (out, in, 0);

  emit_jump_insn (gen_jump (donelab));
  emit_barrier ();

  emit_label (neglab);

  i0 = expand_simple_binop (inmode, LSHIFTRT, in, const1_rtx, NULL,
			    1, OPTAB_DIRECT);
  i1 = expand_simple_binop (inmode, AND, in, const1_rtx, NULL,
			    1, OPTAB_DIRECT);
  i0 = expand_simple_binop (inmode, IOR, i0, i1, i0, 1, OPTAB_DIRECT);

  expand_float (f0, i0, 0);

  emit_insn (gen_rtx_SET (out, gen_rtx_PLUS (mode, f0, f0)));

  emit_label (donelab);
}

/* Return the diagnostic message string if conversion from FROMTYPE to
   TOTYPE is not allowed, NULL otherwise.  */

static const char *
ix86_invalid_conversion (const_tree fromtype, const_tree totype)
{
  machine_mode from_mode = element_mode (fromtype);
  machine_mode to_mode = element_mode (totype);

  if (!TARGET_SSE2 && from_mode != to_mode)
    {
      /* Do no allow conversions to/from BFmode/HFmode scalar types
	 when TARGET_SSE2 is not available.  */
      if (from_mode == BFmode)
	return N_("invalid conversion from type %<__bf16%> "
		  "without option %<-msse2%>");
      if (from_mode == HFmode)
	return N_("invalid conversion from type %<_Float16%> "
		  "without option %<-msse2%>");
      if (to_mode == BFmode)
	return N_("invalid conversion to type %<__bf16%> "
		  "without option %<-msse2%>");
      if (to_mode == HFmode)
	return N_("invalid conversion to type %<_Float16%> "
		  "without option %<-msse2%>");
    }

  /* Warn for silent implicit conversion between __bf16 and short,
     since __bfloat16 is refined as real __bf16 instead of short
     since GCC13.  */
  if (element_mode (fromtype) != element_mode (totype)
      && (TARGET_AVX512BF16 || TARGET_AVXNECONVERT))
    {
      /* Warn for silent implicit conversion where user may expect
	 a bitcast.  */
      if ((TYPE_MODE (fromtype) == BFmode
	   && TYPE_MODE (totype) == HImode)
	  || (TYPE_MODE (totype) == BFmode
	      && TYPE_MODE (fromtype) == HImode))
	warning (0, "%<__bfloat16%> is redefined from typedef %<short%> "
		"to real %<__bf16%> since GCC 13.1, be careful of "
		 "implicit conversion between %<__bf16%> and %<short%>; "
		 "an explicit bitcast may be needed here");
    }

  /* Conversion allowed.  */
  return NULL;
}

/* Return the diagnostic message string if the unary operation OP is
   not permitted on TYPE, NULL otherwise.  */

static const char *
ix86_invalid_unary_op (int op, const_tree type)
{
  machine_mode mmode = element_mode (type);
  /* Reject all single-operand operations on BFmode/HFmode except for &
     when TARGET_SSE2 is not available.  */
  if (!TARGET_SSE2 && op != ADDR_EXPR)
    {
      if (mmode == BFmode)
	return N_("operation not permitted on type %<__bf16%> "
		  "without option %<-msse2%>");
      if (mmode == HFmode)
	return N_("operation not permitted on type %<_Float16%> "
		  "without option %<-msse2%>");
    }

  /* Operation allowed.  */
  return NULL;
}

/* Return the diagnostic message string if the binary operation OP is
   not permitted on TYPE1 and TYPE2, NULL otherwise.  */

static const char *
ix86_invalid_binary_op (int op ATTRIBUTE_UNUSED, const_tree type1,
			const_tree type2)
{
  machine_mode type1_mode = element_mode (type1);
  machine_mode type2_mode = element_mode (type2);
  /* Reject all 2-operand operations on BFmode or HFmode
     when TARGET_SSE2 is not available.  */
  if (!TARGET_SSE2)
    {
      if (type1_mode == BFmode || type2_mode == BFmode)
	return N_("operation not permitted on type %<__bf16%> "
		  "without option %<-msse2%>");

      if (type1_mode == HFmode || type2_mode == HFmode)
	return N_("operation not permitted on type %<_Float16%> "
		  "without option %<-msse2%>");
    }

  /* Operation allowed.  */
  return NULL;
}


/* Target hook for scalar_mode_supported_p.  */
static bool
ix86_scalar_mode_supported_p (scalar_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode))
    return default_decimal_float_supported_p ();
  else if (mode == TFmode)
    return true;
  else if (mode == HFmode || mode == BFmode)
    return true;
  else
    return default_scalar_mode_supported_p (mode);
}

/* Implement TARGET_LIBGCC_FLOATING_POINT_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
ix86_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  /* NB: Always return TRUE for HFmode so that the _Float16 type will
     be defined by the C front-end for AVX512FP16 intrinsics.  We will
     issue an error in ix86_expand_move for HFmode if AVX512FP16 isn't
     enabled.  */
  return ((mode == HFmode || mode == BFmode)
	  ? true
	  : default_libgcc_floating_mode_supported_p (mode));
}

/* Implements target hook vector_mode_supported_p.  */
static bool
ix86_vector_mode_supported_p (machine_mode mode)
{
  /* For ia32, scalar TImode isn't supported and so V1TImode shouldn't be
     either.  */
  if (!TARGET_64BIT && GET_MODE_INNER (mode) == TImode)
    return false;
  if (TARGET_SSE && VALID_SSE_REG_MODE (mode))
    return true;
  if (TARGET_SSE2 && VALID_SSE2_REG_MODE (mode))
    return true;
  if (TARGET_AVX && VALID_AVX256_REG_MODE (mode))
    return true;
  if (TARGET_AVX512F && TARGET_EVEX512 && VALID_AVX512F_REG_MODE (mode))
    return true;
  if ((TARGET_MMX || TARGET_MMX_WITH_SSE)
      && VALID_MMX_REG_MODE (mode))
    return true;
  if ((TARGET_3DNOW || TARGET_MMX_WITH_SSE)
      && VALID_MMX_REG_MODE_3DNOW (mode))
    return true;
  if (mode == V2QImode)
    return true;
  return false;
}

/* Target hook for c_mode_for_suffix.  */
static machine_mode
ix86_c_mode_for_suffix (char suffix)
{
  if (suffix == 'q')
    return TFmode;
  if (suffix == 'w')
    return XFmode;

  return VOIDmode;
}

/* Helper function to map common constraints to non-EGPR ones.
   All related constraints have h prefix, and h plus Upper letter
   means the constraint is strictly EGPR enabled, while h plus
   lower letter indicates the constraint is strictly gpr16 only.

   Specially for "g" constraint, split it to rmi as there is
   no corresponding general constraint define for backend.

   Here is the full list to map constraints that may involve
   gpr to h prefixed.

   "g" -> "jrjmi"
   "r" -> "jr"
   "m" -> "jm"
   "<" -> "j<"
   ">" -> "j>"
   "o" -> "jo"
   "V" -> "jV"
   "p" -> "jp"
   "Bm" -> "ja"
*/

static void map_egpr_constraints (vec<const char *> &constraints)
{
  for (size_t i = 0; i < constraints.length(); i++)
    {
      const char *cur = constraints[i];

      if (startswith (cur, "=@cc"))
	continue;

      int len = strlen (cur);
      auto_vec<char> buf;

      for (int j = 0; j < len; j++)
	{
	  switch (cur[j])
	    {
	    case 'g':
	      buf.safe_push ('j');
	      buf.safe_push ('r');
	      buf.safe_push ('j');
	      buf.safe_push ('m');
	      buf.safe_push ('i');
	      break;
	    case 'r':
	    case 'm':
	    case '<':
	    case '>':
	    case 'o':
	    case 'V':
	    case 'p':
	      buf.safe_push ('j');
	      buf.safe_push (cur[j]);
	      break;
	    case 'B':
	      if (cur[j + 1] == 'm')
		{
		  buf.safe_push ('j');
		  buf.safe_push ('a');
		  j++;
		}
	      else
		{
		  buf.safe_push (cur[j]);
		  buf.safe_push (cur[j + 1]);
		  j++;
		}
	      break;
	    case 'T':
	    case 'Y':
	    case 'W':
	    case 'j':
	      buf.safe_push (cur[j]);
	      buf.safe_push (cur[j + 1]);
	      j++;
	      break;
	    default:
	      buf.safe_push (cur[j]);
	      break;
	    }
	}
      buf.safe_push ('\0');
      constraints[i] = xstrdup (buf.address ());
    }
}

/* Worker function for TARGET_MD_ASM_ADJUST.

   We implement asm flag outputs, and maintain source compatibility
   with the old cc0-based compiler.  */

static rtx_insn *
ix86_md_asm_adjust (vec<rtx> &outputs, vec<rtx> & /*inputs*/,
		    vec<machine_mode> & /*input_modes*/,
		    vec<const char *> &constraints, vec<rtx> &/*uses*/,
		    vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs,
		    location_t loc)
{
  bool saw_asm_flag = false;

  start_sequence ();

  if (TARGET_APX_EGPR && !ix86_apx_inline_asm_use_gpr32)
    map_egpr_constraints (constraints);

  for (unsigned i = 0, n = outputs.length (); i < n; ++i)
    {
      const char *con = constraints[i];
      if (!startswith (con, "=@cc"))
	continue;
      con += 4;
      if (strchr (con, ',') != NULL)
	{
	  error_at (loc, "alternatives not allowed in %<asm%> flag output");
	  continue;
	}

      bool invert = false;
      if (con[0] == 'n')
	invert = true, con++;

      machine_mode mode = CCmode;
      rtx_code code = UNKNOWN;

      switch (con[0])
	{
	case 'a':
	  if (con[1] == 0)
	    mode = CCAmode, code = EQ;
	  else if (con[1] == 'e' && con[2] == 0)
	    mode = CCCmode, code = NE;
	  break;
	case 'b':
	  if (con[1] == 0)
	    mode = CCCmode, code = EQ;
	  else if (con[1] == 'e' && con[2] == 0)
	    mode = CCAmode, code = NE;
	  break;
	case 'c':
	  if (con[1] == 0)
	    mode = CCCmode, code = EQ;
	  break;
	case 'e':
	  if (con[1] == 0)
	    mode = CCZmode, code = EQ;
	  break;
	case 'g':
	  if (con[1] == 0)
	    mode = CCGCmode, code = GT;
	  else if (con[1] == 'e' && con[2] == 0)
	    mode = CCGCmode, code = GE;
	  break;
	case 'l':
	  if (con[1] == 0)
	    mode = CCGCmode, code = LT;
	  else if (con[1] == 'e' && con[2] == 0)
	    mode = CCGCmode, code = LE;
	  break;
	case 'o':
	  if (con[1] == 0)
	    mode = CCOmode, code = EQ;
	  break;
	case 'p':
	  if (con[1] == 0)
	    mode = CCPmode, code = EQ;
	  break;
	case 's':
	  if (con[1] == 0)
	    mode = CCSmode, code = EQ;
	  break;
	case 'z':
	  if (con[1] == 0)
	    mode = CCZmode, code = EQ;
	  break;
	}
      if (code == UNKNOWN)
	{
	  error_at (loc, "unknown %<asm%> flag output %qs", constraints[i]);
	  continue;
	}
      if (invert)
	code = reverse_condition (code);

      rtx dest = outputs[i];
      if (!saw_asm_flag)
	{
	  /* This is the first asm flag output.  Here we put the flags
	     register in as the real output and adjust the condition to
	     allow it.  */
	  constraints[i] = "=Bf";
	  outputs[i] = gen_rtx_REG (CCmode, FLAGS_REG);
	  saw_asm_flag = true;
	}
      else
	{
	  /* We don't need the flags register as output twice.  */
	  constraints[i] = "=X";
	  outputs[i] = gen_rtx_SCRATCH (SImode);
	}

      rtx x = gen_rtx_REG (mode, FLAGS_REG);
      x = gen_rtx_fmt_ee (code, QImode, x, const0_rtx);

      machine_mode dest_mode = GET_MODE (dest);
      if (!SCALAR_INT_MODE_P (dest_mode))
	{
	  error_at (loc, "invalid type for %<asm%> flag output");
	  continue;
	}

      if (dest_mode == QImode)
	emit_insn (gen_rtx_SET (dest, x));
      else
	{
	  rtx reg = gen_reg_rtx (QImode);
	  emit_insn (gen_rtx_SET (reg, x));

	  reg = convert_to_mode (dest_mode, reg, 1);
	  emit_move_insn (dest, reg);
	}
    }

  rtx_insn *seq = get_insns ();
  end_sequence ();

  if (saw_asm_flag)
    return seq;
  else
    {
      /* If we had no asm flag outputs, clobber the flags.  */
      clobbers.safe_push (gen_rtx_REG (CCmode, FLAGS_REG));
      SET_HARD_REG_BIT (clobbered_regs, FLAGS_REG);
      return NULL;
    }
}

/* Implements target vector targetm.asm.encode_section_info.  */

static void ATTRIBUTE_UNUSED
ix86_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (ix86_in_large_data_p (decl))
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= SYMBOL_FLAG_FAR_ADDR;
}

/* Worker function for REVERSE_CONDITION.  */

enum rtx_code
ix86_reverse_condition (enum rtx_code code, machine_mode mode)
{
  return (mode == CCFPmode
	  ? reverse_condition_maybe_unordered (code)
	  : reverse_condition (code));
}

/* Output code to perform an x87 FP register move, from OPERANDS[1]
   to OPERANDS[0].  */

const char *
output_387_reg_move (rtx_insn *insn, rtx *operands)
{
  if (REG_P (operands[0]))
    {
      if (REG_P (operands[1])
	  && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	{
	  if (REGNO (operands[0]) == FIRST_STACK_REG)
	    return output_387_ffreep (operands, 0);
	  return "fstp\t%y0";
	}
      if (STACK_TOP_P (operands[0]))
	return "fld%Z1\t%y1";
      return "fst\t%y0";
    }
  else if (MEM_P (operands[0]))
    {
      gcc_assert (REG_P (operands[1]));
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%Z0\t%y0";
      else
	{
	  /* There is no non-popping store to memory for XFmode.
	     So if we need one, follow the store with a load.  */
	  if (GET_MODE (operands[0]) == XFmode)
	    return "fstp%Z0\t%y0\n\tfld%Z0\t%y0";
	  else
	    return "fst%Z0\t%y0";
	}
    }
  else
    gcc_unreachable();
}
#ifdef TARGET_SOLARIS
/* Solaris implementation of TARGET_ASM_NAMED_SECTION.  */

static void
i386_solaris_elf_named_section (const char *name, unsigned int flags,
				tree decl)
{
  /* With Binutils 2.15, the "@unwind" marker must be specified on
     every occurrence of the ".eh_frame" section, not just the first
     one.  */
  if (TARGET_64BIT
      && strcmp (name, ".eh_frame") == 0)
    {
      fprintf (asm_out_file, "\t.section\t%s,\"%s\",@unwind\n", name,
	       flags & SECTION_WRITE ? "aw" : "a");
      return;
    }

#ifndef USE_GAS
  if (HAVE_COMDAT_GROUP && flags & SECTION_LINKONCE)
    {
      solaris_elf_asm_comdat_section (name, flags, decl);
      return;
    }

  /* Solaris/x86 as uses the same syntax for the SHF_EXCLUDE flags as the
     SPARC assembler.  One cannot mix single-letter flags and #exclude, so
     only emit the latter here.  */
  if (flags & SECTION_EXCLUDE)
    {
      fprintf (asm_out_file, "\t.section\t%s,#exclude\n", name);
      return;
    }
#endif

  default_elf_asm_named_section (name, flags, decl);
}
#endif /* TARGET_SOLARIS */

/* Return the mangling of TYPE if it is an extended fundamental type.  */

static const char *
ix86_mangle_type (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) != VOID_TYPE && TREE_CODE (type) != BOOLEAN_TYPE
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    return NULL;

  if (type == float128_type_node || type == float64x_type_node)
    return NULL;

  switch (TYPE_MODE (type))
    {
    case E_BFmode:
      return "DF16b";
    case E_HFmode:
      /* _Float16 is "DF16_".
	 Align with clang's decision in https://reviews.llvm.org/D33719. */
      return "DF16_";
    case E_TFmode:
      /* __float128 is "g".  */
      return "g";
    case E_XFmode:
      /* "long double" or __float80 is "e".  */
      return "e";
    default:
      return NULL;
    }
}

/* Create C++ tinfo symbols for only conditionally available fundamental
   types.  */

static void
ix86_emit_support_tinfos (emit_support_tinfos_callback callback)
{
  extern tree ix86_float16_type_node;
  extern tree ix86_bf16_type_node;

  if (!TARGET_SSE2)
    {
      if (!float16_type_node)
	float16_type_node = ix86_float16_type_node;
      if (!bfloat16_type_node)
	bfloat16_type_node = ix86_bf16_type_node;
      callback (float16_type_node);
      callback (bfloat16_type_node);
      float16_type_node = NULL_TREE;
      bfloat16_type_node = NULL_TREE;
    }
}

static GTY(()) tree ix86_tls_stack_chk_guard_decl;

static tree
ix86_stack_protect_guard (void)
{
  if (TARGET_SSP_TLS_GUARD)
    {
      tree type_node = lang_hooks.types.type_for_mode (ptr_mode, 1);
      int qual = ENCODE_QUAL_ADDR_SPACE (ix86_stack_protector_guard_reg);
      tree type = build_qualified_type (type_node, qual);
      tree t;

      if (OPTION_SET_P (ix86_stack_protector_guard_symbol_str))
	{
	  t = ix86_tls_stack_chk_guard_decl;

	  if (t == NULL)
	    {
	      rtx x;

	      t = build_decl
		(UNKNOWN_LOCATION, VAR_DECL,
		 get_identifier (ix86_stack_protector_guard_symbol_str),
		 type);
	      TREE_STATIC (t) = 1;
	      TREE_PUBLIC (t) = 1;
	      DECL_EXTERNAL (t) = 1;
	      TREE_USED (t) = 1;
	      TREE_THIS_VOLATILE (t) = 1;
	      DECL_ARTIFICIAL (t) = 1;
	      DECL_IGNORED_P (t) = 1;

	      /* Do not share RTL as the declaration is visible outside of
		 current function.  */
	      x = DECL_RTL (t);
	      RTX_FLAG (x, used) = 1;

	      ix86_tls_stack_chk_guard_decl = t;
	    }
	}
      else
	{
	  tree asptrtype = build_pointer_type (type);

	  t = build_int_cst (asptrtype, ix86_stack_protector_guard_offset);
	  t = build2 (MEM_REF, asptrtype, t,
		      build_int_cst (asptrtype, 0));
	  TREE_THIS_VOLATILE (t) = 1;
	}

      return t;
    }

  return default_stack_protect_guard ();
}

/* For 32-bit code we can save PIC register setup by using
   __stack_chk_fail_local hidden function instead of calling
   __stack_chk_fail directly.  64-bit code doesn't need to setup any PIC
   register, so it is better to call __stack_chk_fail directly.  */

static tree ATTRIBUTE_UNUSED
ix86_stack_protect_fail (void)
{
  return TARGET_64BIT
	 ? default_external_stack_protect_fail ()
	 : default_hidden_stack_protect_fail ();
}

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   ??? All x86 object file formats are capable of representing this.
   After all, the relocation needed is the same as for the call insn.
   Whether or not a particular assembler allows us to enter such, I
   guess we'll have to see.  */

int
asm_preferred_eh_data_format (int code, int global)
{
  /* PE-COFF is effectively always -fPIC because of the .reloc section.  */
  if (flag_pic || TARGET_PECOFF || !ix86_direct_extern_access)
    {
      int type = DW_EH_PE_sdata8;
      if (ptr_mode == SImode
	  || ix86_cmodel == CM_SMALL_PIC
	  || (ix86_cmodel == CM_MEDIUM_PIC && (global || code)))
	type = DW_EH_PE_sdata4;
      return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
    }

  if (ix86_cmodel == CM_SMALL
      || (ix86_cmodel == CM_MEDIUM && code))
    return DW_EH_PE_udata4;

  return DW_EH_PE_absptr;
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
ix86_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
                                 tree vectype, int)
{
  bool fp = false;
  machine_mode mode = TImode;
  int index;
  if (vectype != NULL)
    {
      fp = FLOAT_TYPE_P (vectype);
      mode = TYPE_MODE (vectype);
    }

  switch (type_of_cost)
    {
      case scalar_stmt:
        return fp ? ix86_cost->addss : COSTS_N_INSNS (1);

      case scalar_load:
	/* load/store costs are relative to register move which is 2. Recompute
 	   it to COSTS_N_INSNS so everything have same base.  */
        return COSTS_N_INSNS (fp ? ix86_cost->sse_load[0]
			      : ix86_cost->int_load [2]) / 2;

      case scalar_store:
        return COSTS_N_INSNS (fp ? ix86_cost->sse_store[0]
			      : ix86_cost->int_store [2]) / 2;

      case vector_stmt:
        return ix86_vec_cost (mode,
			      fp ? ix86_cost->addss : ix86_cost->sse_op);

      case vector_load:
	index = sse_store_index (mode);
	/* See PR82713 - we may end up being called on non-vector type.  */
	if (index < 0)
	  index = 2;
        return COSTS_N_INSNS (ix86_cost->sse_load[index]) / 2;

      case vector_store:
	index = sse_store_index (mode);
	/* See PR82713 - we may end up being called on non-vector type.  */
	if (index < 0)
	  index = 2;
        return COSTS_N_INSNS (ix86_cost->sse_store[index]) / 2;

      case vec_to_scalar:
      case scalar_to_vec:
        return ix86_vec_cost (mode, ix86_cost->sse_op);

      /* We should have separate costs for unaligned loads and gather/scatter.
	 Do that incrementally.  */
      case unaligned_load:
	index = sse_store_index (mode);
	/* See PR82713 - we may end up being called on non-vector type.  */
	if (index < 0)
	  index = 2;
        return COSTS_N_INSNS (ix86_cost->sse_unaligned_load[index]) / 2;

      case unaligned_store:
	index = sse_store_index (mode);
	/* See PR82713 - we may end up being called on non-vector type.  */
	if (index < 0)
	  index = 2;
        return COSTS_N_INSNS (ix86_cost->sse_unaligned_store[index]) / 2;

      case vector_gather_load:
        return ix86_vec_cost (mode,
			      COSTS_N_INSNS
				 (ix86_cost->gather_static
				  + ix86_cost->gather_per_elt
				    * TYPE_VECTOR_SUBPARTS (vectype)) / 2);

      case vector_scatter_store:
        return ix86_vec_cost (mode,
			      COSTS_N_INSNS
				 (ix86_cost->scatter_static
				  + ix86_cost->scatter_per_elt
				    * TYPE_VECTOR_SUBPARTS (vectype)) / 2);

      case cond_branch_taken:
        return ix86_cost->cond_taken_branch_cost;

      case cond_branch_not_taken:
        return ix86_cost->cond_not_taken_branch_cost;

      case vec_perm:
      case vec_promote_demote:
        return ix86_vec_cost (mode, ix86_cost->sse_op);

      case vec_construct:
	{
	  int n = TYPE_VECTOR_SUBPARTS (vectype);
	  /* N - 1 element inserts into an SSE vector, the possible
	     GPR -> XMM move is accounted for in add_stmt_cost.  */
	  if (GET_MODE_BITSIZE (mode) <= 128)
	    return (n - 1) * ix86_cost->sse_op;
	  /* One vinserti128 for combining two SSE vectors for AVX256.  */
	  else if (GET_MODE_BITSIZE (mode) == 256)
	    return ((n - 2) * ix86_cost->sse_op
		    + ix86_vec_cost (mode, ix86_cost->addss));
	  /* One vinserti64x4 and two vinserti128 for combining SSE
	     and AVX256 vectors to AVX512.  */
	  else if (GET_MODE_BITSIZE (mode) == 512)
	    return ((n - 4) * ix86_cost->sse_op
		    + 3 * ix86_vec_cost (mode, ix86_cost->addss));
	  gcc_unreachable ();
	}

      default:
        gcc_unreachable ();
    }
}


/* This function returns the calling abi specific va_list type node.
   It returns  the FNDECL specific va_list type.  */

static tree
ix86_fn_abi_va_list (tree fndecl)
{
  if (!TARGET_64BIT)
    return va_list_type_node;
  gcc_assert (fndecl != NULL_TREE);

  if (ix86_function_abi ((const_tree) fndecl) == MS_ABI)
    return ms_va_list_type_node;
  else
    return sysv_va_list_type_node;
}

/* Returns the canonical va_list type specified by TYPE. If there
   is no valid TYPE provided, it return NULL_TREE.  */

static tree
ix86_canonical_va_list_type (tree type)
{
  if (TARGET_64BIT)
    {
      if (lookup_attribute ("ms_abi va_list", TYPE_ATTRIBUTES (type)))
	return ms_va_list_type_node;

      if ((TREE_CODE (type) == ARRAY_TYPE
	   && integer_zerop (array_type_nelts (type)))
	  || POINTER_TYPE_P (type))
	{
	  tree elem_type = TREE_TYPE (type);
	  if (TREE_CODE (elem_type) == RECORD_TYPE
	      && lookup_attribute ("sysv_abi va_list",
				   TYPE_ATTRIBUTES (elem_type)))
	    return sysv_va_list_type_node;
	}

      return NULL_TREE;
    }

  return std_canonical_va_list_type (type);
}

/* Iterate through the target-specific builtin types for va_list.
   IDX denotes the iterator, *PTREE is set to the result type of
   the va_list builtin, and *PNAME to its internal type.
   Returns zero if there is no element for this index, otherwise
   IDX should be increased upon the next call.
   Note, do not iterate a base builtin's name like __builtin_va_list.
   Used from c_common_nodes_and_builtins.  */

static int
ix86_enum_va_list (int idx, const char **pname, tree *ptree)
{
  if (TARGET_64BIT)
    {
      switch (idx)
	{
	default:
	  break;

	case 0:
	  *ptree = ms_va_list_type_node;
	  *pname = "__builtin_ms_va_list";
	  return 1;

	case 1:
	  *ptree = sysv_va_list_type_node;
	  *pname = "__builtin_sysv_va_list";
	  return 1;
	}
    }

  return 0;
}

#undef TARGET_SCHED_DISPATCH
#define TARGET_SCHED_DISPATCH ix86_bd_has_dispatch
#undef TARGET_SCHED_DISPATCH_DO
#define TARGET_SCHED_DISPATCH_DO ix86_bd_do_dispatch
#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH ix86_reassociation_width
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER ix86_atom_sched_reorder
#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY ix86_adjust_priority
#undef TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK
#define TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK \
  ix86_dependencies_evaluation_hook


/* Implementation of reassociation_width target hook used by
   reassoc phase to identify parallelism level in reassociated
   tree.  Statements tree_code is passed in OPC.  Arguments type
   is passed in MODE.  */

static int
ix86_reassociation_width (unsigned int op, machine_mode mode)
{
  int width = 1;
  /* Vector part.  */
  if (VECTOR_MODE_P (mode))
    {
      int div = 1;
      if (INTEGRAL_MODE_P (mode))
	width = ix86_cost->reassoc_vec_int;
      else if (FLOAT_MODE_P (mode))
	width = ix86_cost->reassoc_vec_fp;

      if (width == 1)
	return 1;

      /* Integer vector instructions execute in FP unit
	 and can execute 3 additions and one multiplication per cycle.  */
      if ((ix86_tune == PROCESSOR_ZNVER1 || ix86_tune == PROCESSOR_ZNVER2
	   || ix86_tune == PROCESSOR_ZNVER3 || ix86_tune == PROCESSOR_ZNVER4
	   || ix86_tune == PROCESSOR_ZNVER5)
   	  && INTEGRAL_MODE_P (mode) && op != PLUS && op != MINUS)
	return 1;

      /* Account for targets that splits wide vectors into multiple parts.  */
      if (TARGET_AVX512_SPLIT_REGS && GET_MODE_BITSIZE (mode) > 256)
	div = GET_MODE_BITSIZE (mode) / 256;
      else if (TARGET_AVX256_SPLIT_REGS && GET_MODE_BITSIZE (mode) > 128)
	div = GET_MODE_BITSIZE (mode) / 128;
      else if (TARGET_SSE_SPLIT_REGS && GET_MODE_BITSIZE (mode) > 64)
	div = GET_MODE_BITSIZE (mode) / 64;
      width = (width + div - 1) / div;
    }
  /* Scalar part.  */
  else if (INTEGRAL_MODE_P (mode))
    width = ix86_cost->reassoc_int;
  else if (FLOAT_MODE_P (mode))
    width = ix86_cost->reassoc_fp;

  /* Avoid using too many registers in 32bit mode.  */
  if (!TARGET_64BIT && width > 2)
    width = 2;
  return width;
}

/* ??? No autovectorization into MMX or 3DNOW until we can reliably
   place emms and femms instructions.  */

static machine_mode
ix86_preferred_simd_mode (scalar_mode mode)
{
  if (!TARGET_SSE)
    return word_mode;

  switch (mode)
    {
    case E_QImode:
      if (TARGET_AVX512BW && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V64QImode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V32QImode;
      else
	return V16QImode;

    case E_HImode:
      if (TARGET_AVX512BW && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V32HImode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V16HImode;
      else
	return V8HImode;

    case E_SImode:
      if (TARGET_AVX512F && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V16SImode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V8SImode;
      else
	return V4SImode;

    case E_DImode:
      if (TARGET_AVX512F && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V8DImode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V4DImode;
      else
	return V2DImode;

    case E_HFmode:
      if (TARGET_AVX512FP16)
	{
	  if (TARGET_AVX512VL)
	    {
	      if (TARGET_PREFER_AVX128)
		return V8HFmode;
	      else if (TARGET_PREFER_AVX256 || !TARGET_EVEX512)
		return V16HFmode;
	    }
	  if (TARGET_EVEX512)
	    return V32HFmode;
	}
      return word_mode;

    case E_SFmode:
      if (TARGET_AVX512F && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V16SFmode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V8SFmode;
      else
	return V4SFmode;

    case E_DFmode:
      if (TARGET_AVX512F && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
	return V8DFmode;
      else if (TARGET_AVX && !TARGET_PREFER_AVX128)
	return V4DFmode;
      else if (TARGET_SSE2)
	return V2DFmode;
      /* FALLTHRU */

    default:
      return word_mode;
    }
}

/* If AVX is enabled then try vectorizing with both 256bit and 128bit
   vectors.  If AVX512F is enabled then try vectorizing with 512bit,
   256bit and 128bit vectors.  */

static unsigned int
ix86_autovectorize_vector_modes (vector_modes *modes, bool all)
{
  if (TARGET_AVX512F && TARGET_EVEX512 && !TARGET_PREFER_AVX256)
    {
      modes->safe_push (V64QImode);
      modes->safe_push (V32QImode);
      modes->safe_push (V16QImode);
    }
  else if (TARGET_AVX512F && TARGET_EVEX512 && all)
    {
      modes->safe_push (V32QImode);
      modes->safe_push (V16QImode);
      modes->safe_push (V64QImode);
    }
  else if (TARGET_AVX && !TARGET_PREFER_AVX128)
    {
      modes->safe_push (V32QImode);
      modes->safe_push (V16QImode);
    }
  else if (TARGET_AVX && all)
    {
      modes->safe_push (V16QImode);
      modes->safe_push (V32QImode);
    }
  else if (TARGET_SSE2)
    modes->safe_push (V16QImode);

  if (TARGET_MMX_WITH_SSE)
    modes->safe_push (V8QImode);

  if (TARGET_SSE2)
    modes->safe_push (V4QImode);

  return 0;
}

/* Implemenation of targetm.vectorize.get_mask_mode.  */

static opt_machine_mode
ix86_get_mask_mode (machine_mode data_mode)
{
  unsigned vector_size = GET_MODE_SIZE (data_mode);
  unsigned nunits = GET_MODE_NUNITS (data_mode);
  unsigned elem_size = vector_size / nunits;

  /* Scalar mask case.  */
  if ((TARGET_AVX512F && TARGET_EVEX512 && vector_size == 64)
      || (TARGET_AVX512VL && (vector_size == 32 || vector_size == 16))
      /* AVX512FP16 only supports vector comparison
	 to kmask for _Float16.  */
      || (TARGET_AVX512VL && TARGET_AVX512FP16
	  && GET_MODE_INNER (data_mode) == E_HFmode))
    {
      if (elem_size == 4
	  || elem_size == 8
	  || (TARGET_AVX512BW && (elem_size == 1 || elem_size == 2)))
	return smallest_int_mode_for_size (nunits);
    }

  scalar_int_mode elem_mode
    = smallest_int_mode_for_size (elem_size * BITS_PER_UNIT);

  gcc_assert (elem_size * nunits == vector_size);

  return mode_for_vector (elem_mode, nunits);
}



/* Return class of registers which could be used for pseudo of MODE
   and of class RCLASS for spilling instead of memory.  Return NO_REGS
   if it is not possible or non-profitable.  */

/* Disabled due to PRs 70902, 71453, 71555, 71596 and 71657.  */

static reg_class_t
ix86_spill_class (reg_class_t rclass, machine_mode mode)
{
  if (0 && TARGET_GENERAL_REGS_SSE_SPILL
      && TARGET_SSE2
      && TARGET_INTER_UNIT_MOVES_TO_VEC
      && TARGET_INTER_UNIT_MOVES_FROM_VEC
      && (mode == SImode || (TARGET_64BIT && mode == DImode))
      && INTEGER_CLASS_P (rclass))
    return ALL_SSE_REGS;
  return NO_REGS;
}

/* Implement TARGET_MAX_NOCE_IFCVT_SEQ_COST.  Like the default implementation,
   but returns a lower bound.  */

static unsigned int
ix86_max_noce_ifcvt_seq_cost (edge e)
{
  bool predictable_p = predictable_edge_p (e);
  if (predictable_p)
    {
      if (OPTION_SET_P (param_max_rtl_if_conversion_predictable_cost))
	return param_max_rtl_if_conversion_predictable_cost;
    }
  else
    {
      if (OPTION_SET_P (param_max_rtl_if_conversion_unpredictable_cost))
	return param_max_rtl_if_conversion_unpredictable_cost;
    }

  return BRANCH_COST (true, predictable_p) * COSTS_N_INSNS (2);
}

/* Return true if SEQ is a good candidate as a replacement for the
   if-convertible sequence described in IF_INFO.  */

static bool
ix86_noce_conversion_profitable_p (rtx_insn *seq, struct noce_if_info *if_info)
{
  if (TARGET_ONE_IF_CONV_INSN && if_info->speed_p)
    {
      int cmov_cnt = 0;
      /* Punt if SEQ contains more than one CMOV or FCMOV instruction.
	 Maybe we should allow even more conditional moves as long as they
	 are used far enough not to stall the CPU, or also consider
	 IF_INFO->TEST_BB succ edge probabilities.  */
      for (rtx_insn *insn = seq; insn; insn = NEXT_INSN (insn))
	{
	  rtx set = single_set (insn);
	  if (!set)
	    continue;
	  if (GET_CODE (SET_SRC (set)) != IF_THEN_ELSE)
	    continue;
	  rtx src = SET_SRC (set);
	  machine_mode mode = GET_MODE (src);
	  if (GET_MODE_CLASS (mode) != MODE_INT
	      && GET_MODE_CLASS (mode) != MODE_FLOAT)
	    continue;
	  if ((!REG_P (XEXP (src, 1)) && !MEM_P (XEXP (src, 1)))
	      || (!REG_P (XEXP (src, 2)) && !MEM_P (XEXP (src, 2))))
	    continue;
	  /* insn is CMOV or FCMOV.  */
	  if (++cmov_cnt > 1)
	    return false;
	}
    }
  return default_noce_conversion_profitable_p (seq, if_info);
}

/* x86-specific vector costs.  */
class ix86_vector_costs : public vector_costs
{
public:
  ix86_vector_costs (vec_info *, bool);

  unsigned int add_stmt_cost (int count, vect_cost_for_stmt kind,
			      stmt_vec_info stmt_info, slp_tree node,
			      tree vectype, int misalign,
			      vect_cost_model_location where) override;
  void finish_cost (const vector_costs *) override;

private:

  /* Estimate register pressure of the vectorized code.  */
  void ix86_vect_estimate_reg_pressure ();
  /* Number of GENERAL_REGS/SSE_REGS used in the vectorizer, it's used for
     estimation of register pressure.
     ??? Currently it's only used by vec_construct/scalar_to_vec
     where we know it's not loaded from memory.  */
  unsigned m_num_gpr_needed[3];
  unsigned m_num_sse_needed[3];
};

ix86_vector_costs::ix86_vector_costs (vec_info* vinfo, bool costing_for_scalar)
  : vector_costs (vinfo, costing_for_scalar),
    m_num_gpr_needed (),
    m_num_sse_needed ()
{
}

/* Implement targetm.vectorize.create_costs.  */

static vector_costs *
ix86_vectorize_create_costs (vec_info *vinfo, bool costing_for_scalar)
{
  return new ix86_vector_costs (vinfo, costing_for_scalar);
}

unsigned
ix86_vector_costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
				  stmt_vec_info stmt_info, slp_tree node,
				  tree vectype, int misalign,
				  vect_cost_model_location where)
{
  unsigned retval = 0;
  bool scalar_p
    = (kind == scalar_stmt || kind == scalar_load || kind == scalar_store);
  int stmt_cost = - 1;

  bool fp = false;
  machine_mode mode = scalar_p ? SImode : TImode;

  if (vectype != NULL)
    {
      fp = FLOAT_TYPE_P (vectype);
      mode = TYPE_MODE (vectype);
      if (scalar_p)
	mode = TYPE_MODE (TREE_TYPE (vectype));
    }

  if ((kind == vector_stmt || kind == scalar_stmt)
      && stmt_info
      && stmt_info->stmt && gimple_code (stmt_info->stmt) == GIMPLE_ASSIGN)
    {
      tree_code subcode = gimple_assign_rhs_code (stmt_info->stmt);
      /*machine_mode inner_mode = mode;
      if (VECTOR_MODE_P (mode))
	inner_mode = GET_MODE_INNER (mode);*/

      switch (subcode)
	{
	case PLUS_EXPR:
	case POINTER_PLUS_EXPR:
	case MINUS_EXPR:
	  if (kind == scalar_stmt)
	    {
	      if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
		stmt_cost = ix86_cost->addss;
	      else if (X87_FLOAT_MODE_P (mode))
		stmt_cost = ix86_cost->fadd;
	      else
	        stmt_cost = ix86_cost->add;
	    }
	  else
	    stmt_cost = ix86_vec_cost (mode, fp ? ix86_cost->addss
				       : ix86_cost->sse_op);
	  break;

	case MULT_EXPR:
	  /* For MULT_HIGHPART_EXPR, x86 only supports pmulhw,
	     take it as MULT_EXPR.  */
	case MULT_HIGHPART_EXPR:
	  stmt_cost = ix86_multiplication_cost (ix86_cost, mode);
	  break;
	  /* There's no direct instruction for WIDEN_MULT_EXPR,
	     take emulation into account.  */
	case WIDEN_MULT_EXPR:
	  stmt_cost = ix86_widen_mult_cost (ix86_cost, mode,
					    TYPE_UNSIGNED (vectype));
	  break;

	case NEGATE_EXPR:
	  if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	    stmt_cost = ix86_cost->sse_op;
	  else if (X87_FLOAT_MODE_P (mode))
	    stmt_cost = ix86_cost->fchs;
	  else if (VECTOR_MODE_P (mode))
	    stmt_cost = ix86_vec_cost (mode, ix86_cost->sse_op);
	  else
	    stmt_cost = ix86_cost->add;
	  break;
	case TRUNC_DIV_EXPR:
	case CEIL_DIV_EXPR:
	case FLOOR_DIV_EXPR:
	case ROUND_DIV_EXPR:
	case TRUNC_MOD_EXPR:
	case CEIL_MOD_EXPR:
	case FLOOR_MOD_EXPR:
	case RDIV_EXPR:
	case ROUND_MOD_EXPR:
	case EXACT_DIV_EXPR:
	  stmt_cost = ix86_division_cost (ix86_cost, mode);
	  break;

	case RSHIFT_EXPR:
	case LSHIFT_EXPR:
	case LROTATE_EXPR:
	case RROTATE_EXPR:
	  {
	    tree op1 = gimple_assign_rhs1 (stmt_info->stmt);
	    tree op2 = gimple_assign_rhs2 (stmt_info->stmt);
	    stmt_cost = ix86_shift_rotate_cost
			   (ix86_cost,
			    (subcode == RSHIFT_EXPR
			     && !TYPE_UNSIGNED (TREE_TYPE (op1)))
			    ? ASHIFTRT : LSHIFTRT, mode,
		            TREE_CODE (op2) == INTEGER_CST,
			    cst_and_fits_in_hwi (op2)
			    ? int_cst_value (op2) : -1,
			    false, false, NULL, NULL);
	  }
	  break;
	case NOP_EXPR:
	  /* Only sign-conversions are free.  */
	  if (tree_nop_conversion_p
	        (TREE_TYPE (gimple_assign_lhs (stmt_info->stmt)),
		 TREE_TYPE (gimple_assign_rhs1 (stmt_info->stmt))))
	    stmt_cost = 0;
	  break;

	case BIT_IOR_EXPR:
	case ABS_EXPR:
	case ABSU_EXPR:
	case MIN_EXPR:
	case MAX_EXPR:
	case BIT_XOR_EXPR:
	case BIT_AND_EXPR:
	case BIT_NOT_EXPR:
	  if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
	    stmt_cost = ix86_cost->sse_op;
	  else if (VECTOR_MODE_P (mode))
	    stmt_cost = ix86_vec_cost (mode, ix86_cost->sse_op);
	  else
	    stmt_cost = ix86_cost->add;
	  break;
	default:
	  break;
	}
    }

  combined_fn cfn;
  if ((kind == vector_stmt || kind == scalar_stmt)
      && stmt_info
      && stmt_info->stmt
      && (cfn = gimple_call_combined_fn (stmt_info->stmt)) != CFN_LAST)
    switch (cfn)
      {
      case CFN_FMA:
	stmt_cost = ix86_vec_cost (mode,
				   mode == SFmode ? ix86_cost->fmass
				   : ix86_cost->fmasd);
	break;
      case CFN_MULH:
	stmt_cost = ix86_multiplication_cost (ix86_cost, mode);
	break;
      default:
	break;
      }

  /* If we do elementwise loads into a vector then we are bound by
     latency and execution resources for the many scalar loads
     (AGU and load ports).  Try to account for this by scaling the
     construction cost by the number of elements involved.  */
  if ((kind == vec_construct || kind == vec_to_scalar)
      && stmt_info
      && (STMT_VINFO_TYPE (stmt_info) == load_vec_info_type
	  || STMT_VINFO_TYPE (stmt_info) == store_vec_info_type)
      && ((STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) == VMAT_ELEMENTWISE
	   && (TREE_CODE (DR_STEP (STMT_VINFO_DATA_REF (stmt_info)))
	       != INTEGER_CST))
	  || STMT_VINFO_MEMORY_ACCESS_TYPE (stmt_info) == VMAT_GATHER_SCATTER))
    {
      stmt_cost = ix86_builtin_vectorization_cost (kind, vectype, misalign);
      stmt_cost *= (TYPE_VECTOR_SUBPARTS (vectype) + 1);
    }
  else if ((kind == vec_construct || kind == scalar_to_vec)
	   && node
	   && SLP_TREE_DEF_TYPE (node) == vect_external_def)
    {
      stmt_cost = ix86_builtin_vectorization_cost (kind, vectype, misalign);
      unsigned i;
      tree op;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	if (TREE_CODE (op) == SSA_NAME)
	  TREE_VISITED (op) = 0;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	{
	  if (TREE_CODE (op) != SSA_NAME
	      || TREE_VISITED (op))
	    continue;
	  TREE_VISITED (op) = 1;
	  gimple *def = SSA_NAME_DEF_STMT (op);
	  tree tem;
	  if (is_gimple_assign (def)
	      && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def))
	      && ((tem = gimple_assign_rhs1 (def)), true)
	      && TREE_CODE (tem) == SSA_NAME
	      /* A sign-change expands to nothing.  */
	      && tree_nop_conversion_p (TREE_TYPE (gimple_assign_lhs (def)),
					TREE_TYPE (tem)))
	    def = SSA_NAME_DEF_STMT (tem);
	  /* When the component is loaded from memory we can directly
	     move it to a vector register, otherwise we have to go
	     via a GPR or via vpinsr which involves similar cost.
	     Likewise with a BIT_FIELD_REF extracting from a vector
	     register we can hope to avoid using a GPR.  */
	  if (!is_gimple_assign (def)
	      || ((!gimple_assign_load_p (def)
		   || (!TARGET_SSE4_1
		       && GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (op))) == 1))
		  && (gimple_assign_rhs_code (def) != BIT_FIELD_REF
		      || !VECTOR_TYPE_P (TREE_TYPE
				(TREE_OPERAND (gimple_assign_rhs1 (def), 0))))))
	    {
	      if (fp)
		m_num_sse_needed[where]++;
	      else
		{
		  m_num_gpr_needed[where]++;
		  stmt_cost += ix86_cost->sse_to_integer;
		}
	    }
	}
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	if (TREE_CODE (op) == SSA_NAME)
	  TREE_VISITED (op) = 0;
    }
  if (stmt_cost == -1)
    stmt_cost = ix86_builtin_vectorization_cost (kind, vectype, misalign);

  /* Penalize DFmode vector operations for Bonnell.  */
  if (TARGET_CPU_P (BONNELL) && kind == vector_stmt
      && vectype && GET_MODE_INNER (TYPE_MODE (vectype)) == DFmode)
    stmt_cost *= 5;  /* FIXME: The value here is arbitrary.  */

  /* Statements in an inner loop relative to the loop being
     vectorized are weighted more heavily.  The value here is
     arbitrary and could potentially be improved with analysis.  */
  retval = adjust_cost_for_freq (stmt_info, where, count * stmt_cost);

  /* We need to multiply all vector stmt cost by 1.7 (estimated cost)
     for Silvermont as it has out of order integer pipeline and can execute
     2 scalar instruction per tick, but has in order SIMD pipeline.  */
  if ((TARGET_CPU_P (SILVERMONT) || TARGET_CPU_P (GOLDMONT)
       || TARGET_CPU_P (GOLDMONT_PLUS) || TARGET_CPU_P (INTEL))
      && stmt_info && stmt_info->stmt)
    {
      tree lhs_op = gimple_get_lhs (stmt_info->stmt);
      if (lhs_op && TREE_CODE (TREE_TYPE (lhs_op)) == INTEGER_TYPE)
	retval = (retval * 17) / 10;
    }

  m_costs[where] += retval;

  return retval;
}

void
ix86_vector_costs::ix86_vect_estimate_reg_pressure ()
{
  unsigned gpr_spill_cost = COSTS_N_INSNS (ix86_cost->int_store [2]) / 2;
  unsigned sse_spill_cost = COSTS_N_INSNS (ix86_cost->sse_store[0]) / 2;

  /* Any better way to have target available fp registers, currently use SSE_REGS.  */
  unsigned target_avail_sse = TARGET_64BIT ? (TARGET_AVX512F ? 32 : 16) : 8;
  for (unsigned i = 0; i != 3; i++)
    {
      if (m_num_gpr_needed[i] > target_avail_regs)
	m_costs[i] += gpr_spill_cost * (m_num_gpr_needed[i] - target_avail_regs);
      /* Only measure sse registers pressure.  */
      if (TARGET_SSE && (m_num_sse_needed[i] > target_avail_sse))
	m_costs[i] += sse_spill_cost * (m_num_sse_needed[i] - target_avail_sse);
    }
}

void
ix86_vector_costs::finish_cost (const vector_costs *scalar_costs)
{
  loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo);
  if (loop_vinfo && !m_costing_for_scalar)
    {
      /* We are currently not asking the vectorizer to compare costs
	 between different vector mode sizes.  When using predication
	 that will end up always choosing the prefered mode size even
	 if there's a smaller mode covering all lanes.  Test for this
	 situation and artificially reject the larger mode attempt.
	 ???  We currently lack masked ops for sub-SSE sized modes,
	 so we could restrict this rejection to AVX and AVX512 modes
	 but error on the safe side for now.  */
      if (LOOP_VINFO_USING_PARTIAL_VECTORS_P (loop_vinfo)
	  && !LOOP_VINFO_EPILOGUE_P (loop_vinfo)
	  && LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
	  && (exact_log2 (LOOP_VINFO_VECT_FACTOR (loop_vinfo).to_constant ())
	      > ceil_log2 (LOOP_VINFO_INT_NITERS (loop_vinfo))))
	m_costs[vect_body] = INT_MAX;
    }

  ix86_vect_estimate_reg_pressure ();

  vector_costs::finish_cost (scalar_costs);
}

/* Validate target specific memory model bits in VAL. */

static unsigned HOST_WIDE_INT
ix86_memmodel_check (unsigned HOST_WIDE_INT val)
{
  enum memmodel model = memmodel_from_int (val);
  bool strong;

  if (val & ~(unsigned HOST_WIDE_INT)(IX86_HLE_ACQUIRE|IX86_HLE_RELEASE
				      |MEMMODEL_MASK)
      || ((val & IX86_HLE_ACQUIRE) && (val & IX86_HLE_RELEASE)))
    {
      warning (OPT_Winvalid_memory_model,
	       "unknown architecture specific memory model");
      return MEMMODEL_SEQ_CST;
    }
  strong = (is_mm_acq_rel (model) || is_mm_seq_cst (model));
  if (val & IX86_HLE_ACQUIRE && !(is_mm_acquire (model) || strong))
    {
      warning (OPT_Winvalid_memory_model,
	      "%<HLE_ACQUIRE%> not used with %<ACQUIRE%> or stronger "
	       "memory model");
      return MEMMODEL_SEQ_CST | IX86_HLE_ACQUIRE;
    }
  if (val & IX86_HLE_RELEASE && !(is_mm_release (model) || strong))
    {
      warning (OPT_Winvalid_memory_model,
	      "%<HLE_RELEASE%> not used with %<RELEASE%> or stronger "
	       "memory model");
      return MEMMODEL_SEQ_CST | IX86_HLE_RELEASE;
    }
  return val;
}

/* Set CLONEI->vecsize_mangle, CLONEI->mask_mode, CLONEI->vecsize_int,
   CLONEI->vecsize_float and if CLONEI->simdlen is 0, also
   CLONEI->simdlen.  Return 0 if SIMD clones shouldn't be emitted,
   or number of vecsize_mangle variants that should be emitted.  */

static int
ix86_simd_clone_compute_vecsize_and_simdlen (struct cgraph_node *node,
					     struct cgraph_simd_clone *clonei,
					     tree base_type, int num,
					     bool explicit_p)
{
  int ret = 1;

  if (clonei->simdlen
      && (clonei->simdlen < 2
	  || clonei->simdlen > 1024
	  || (clonei->simdlen & (clonei->simdlen - 1)) != 0))
    {
      if (explicit_p)
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "unsupported simdlen %wd", clonei->simdlen.to_constant ());
      return 0;
    }

  tree ret_type = TREE_TYPE (TREE_TYPE (node->decl));
  if (TREE_CODE (ret_type) != VOID_TYPE)
    switch (TYPE_MODE (ret_type))
      {
      case E_QImode:
      case E_HImode:
      case E_SImode:
      case E_DImode:
      case E_SFmode:
      case E_DFmode:
      /* case E_SCmode: */
      /* case E_DCmode: */
	if (!AGGREGATE_TYPE_P (ret_type))
	  break;
	/* FALLTHRU */
      default:
	if (explicit_p)
	  warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		      "unsupported return type %qT for simd", ret_type);
	return 0;
      }

  tree t;
  int i;
  tree type_arg_types = TYPE_ARG_TYPES (TREE_TYPE (node->decl));
  bool decl_arg_p = (node->definition || type_arg_types == NULL_TREE);

  for (t = (decl_arg_p ? DECL_ARGUMENTS (node->decl) : type_arg_types), i = 0;
       t && t != void_list_node; t = TREE_CHAIN (t), i++)
    {
      tree arg_type = decl_arg_p ? TREE_TYPE (t) : TREE_VALUE (t);
      switch (TYPE_MODE (arg_type))
	{
	case E_QImode:
	case E_HImode:
	case E_SImode:
	case E_DImode:
	case E_SFmode:
	case E_DFmode:
	/* case E_SCmode: */
	/* case E_DCmode: */
	  if (!AGGREGATE_TYPE_P (arg_type))
	    break;
	  /* FALLTHRU */
	default:
	  if (clonei->args[i].arg_type == SIMD_CLONE_ARG_TYPE_UNIFORM)
	    break;
	  if (explicit_p)
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"unsupported argument type %qT for simd", arg_type);
	  return 0;
	}
    }

  if (!TREE_PUBLIC (node->decl) || !explicit_p)
    {
      /* If the function isn't exported, we can pick up just one ISA
	 for the clones.  */
      if (TARGET_AVX512F && TARGET_EVEX512)
	clonei->vecsize_mangle = 'e';
      else if (TARGET_AVX2)
	clonei->vecsize_mangle = 'd';
      else if (TARGET_AVX)
	clonei->vecsize_mangle = 'c';
      else
	clonei->vecsize_mangle = 'b';
      ret = 1;
    }
  else
    {
      clonei->vecsize_mangle = "bcde"[num];
      ret = 4;
    }
  clonei->mask_mode = VOIDmode;
  switch (clonei->vecsize_mangle)
    {
    case 'b':
      clonei->vecsize_int = 128;
      clonei->vecsize_float = 128;
      break;
    case 'c':
      clonei->vecsize_int = 128;
      clonei->vecsize_float = 256;
      break;
    case 'd':
      clonei->vecsize_int = 256;
      clonei->vecsize_float = 256;
      break;
    case 'e':
      clonei->vecsize_int = 512;
      clonei->vecsize_float = 512;
      if (TYPE_MODE (base_type) == QImode)
	clonei->mask_mode = DImode;
      else
	clonei->mask_mode = SImode;
      break;
    }
  if (clonei->simdlen == 0)
    {
      if (SCALAR_INT_MODE_P (TYPE_MODE (base_type)))
	clonei->simdlen = clonei->vecsize_int;
      else
	clonei->simdlen = clonei->vecsize_float;
      clonei->simdlen = clonei->simdlen
			/ GET_MODE_BITSIZE (TYPE_MODE (base_type));
    }
  else if (clonei->simdlen > 16)
    {
      /* For compatibility with ICC, use the same upper bounds
	 for simdlen.  In particular, for CTYPE below, use the return type,
	 unless the function returns void, in that case use the characteristic
	 type.  If it is possible for given SIMDLEN to pass CTYPE value
	 in registers (8 [XYZ]MM* regs for 32-bit code, 16 [XYZ]MM* regs
	 for 64-bit code), accept that SIMDLEN, otherwise warn and don't
	 emit corresponding clone.  */
      tree ctype = ret_type;
      if (VOID_TYPE_P (ret_type))
	ctype = base_type;
      int cnt = GET_MODE_BITSIZE (TYPE_MODE (ctype)) * clonei->simdlen;
      if (SCALAR_INT_MODE_P (TYPE_MODE (ctype)))
	cnt /= clonei->vecsize_int;
      else
	cnt /= clonei->vecsize_float;
      if (cnt > (TARGET_64BIT ? 16 : 8))
	{
	  if (explicit_p)
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"unsupported simdlen %wd",
			clonei->simdlen.to_constant ());
	  return 0;
	}
      }
  return ret;
}

/* If SIMD clone NODE can't be used in a vectorized loop
   in current function, return -1, otherwise return a badness of using it
   (0 if it is most desirable from vecsize_mangle point of view, 1
   slightly less desirable, etc.).  */

static int
ix86_simd_clone_usable (struct cgraph_node *node)
{
  switch (node->simdclone->vecsize_mangle)
    {
    case 'b':
      if (!TARGET_SSE2)
	return -1;
      if (!TARGET_AVX)
	return 0;
      return (TARGET_AVX512F && TARGET_EVEX512) ? 3 : TARGET_AVX2 ? 2 : 1;
    case 'c':
      if (!TARGET_AVX)
	return -1;
      return (TARGET_AVX512F && TARGET_EVEX512) ? 2 : TARGET_AVX2 ? 1 : 0;
    case 'd':
      if (!TARGET_AVX2)
	return -1;
      return (TARGET_AVX512F && TARGET_EVEX512) ? 1 : 0;
    case 'e':
      if (!TARGET_AVX512F || !TARGET_EVEX512)
	return -1;
      return 0;
    default:
      gcc_unreachable ();
    }
}

/* This function adjusts the unroll factor based on
   the hardware capabilities. For ex, bdver3 has
   a loop buffer which makes unrolling of smaller
   loops less important. This function decides the
   unroll factor using number of memory references
   (value 32 is used) as a heuristic. */

static unsigned
ix86_loop_unroll_adjust (unsigned nunroll, class loop *loop)
{
  basic_block *bbs;
  rtx_insn *insn;
  unsigned i;
  unsigned mem_count = 0;

  /* Unroll small size loop when unroll factor is not explicitly
     specified.  */
  if (ix86_unroll_only_small_loops && !loop->unroll)
    {
      if (loop->ninsns <= ix86_cost->small_unroll_ninsns)
	return MIN (nunroll, ix86_cost->small_unroll_factor);
      else
	return 1;
    }

  if (!TARGET_ADJUST_UNROLL)
     return nunroll;

  /* Count the number of memory references within the loop body.
     This value determines the unrolling factor for bdver3 and bdver4
     architectures. */
  subrtx_iterator::array_type array;
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    FOR_BB_INSNS (bbs[i], insn)
      if (NONDEBUG_INSN_P (insn))
	FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
	  if (const_rtx x = *iter)
	    if (MEM_P (x))
	      {
		machine_mode mode = GET_MODE (x);
		unsigned int n_words = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
		if (n_words > 4)
		  mem_count += 2;
		else
		  mem_count += 1;
	      }
  free (bbs);

  if (mem_count && mem_count <=32)
    return MIN (nunroll, 32 / mem_count);

  return nunroll;
}


/* Implement TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P.  */

static bool
ix86_float_exceptions_rounding_supported_p (void)
{
  /* For x87 floating point with standard excess precision handling,
     there is no adddf3 pattern (since x87 floating point only has
     XFmode operations) so the default hook implementation gets this
     wrong.  */
  return TARGET_80387 || (TARGET_SSE && TARGET_SSE_MATH);
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV.  */

static void
ix86_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_80387 && !(TARGET_SSE && TARGET_SSE_MATH))
    return;
  tree exceptions_var = create_tmp_var_raw (integer_type_node);
  if (TARGET_80387)
    {
      tree fenv_index_type = build_index_type (size_int (6));
      tree fenv_type = build_array_type (unsigned_type_node, fenv_index_type);
      tree fenv_var = create_tmp_var_raw (fenv_type);
      TREE_ADDRESSABLE (fenv_var) = 1;
      tree fenv_ptr = build_pointer_type (fenv_type);
      tree fenv_addr = build1 (ADDR_EXPR, fenv_ptr, fenv_var);
      fenv_addr = fold_convert (ptr_type_node, fenv_addr);
      tree fnstenv = get_ix86_builtin (IX86_BUILTIN_FNSTENV);
      tree fldenv = get_ix86_builtin (IX86_BUILTIN_FLDENV);
      tree fnstsw = get_ix86_builtin (IX86_BUILTIN_FNSTSW);
      tree fnclex = get_ix86_builtin (IX86_BUILTIN_FNCLEX);
      tree hold_fnstenv = build_call_expr (fnstenv, 1, fenv_addr);
      tree hold_fnclex = build_call_expr (fnclex, 0);
      fenv_var = build4 (TARGET_EXPR, fenv_type, fenv_var, hold_fnstenv,
			 NULL_TREE, NULL_TREE);
      *hold = build2 (COMPOUND_EXPR, void_type_node, fenv_var,
		      hold_fnclex);
      *clear = build_call_expr (fnclex, 0);
      tree sw_var = create_tmp_var_raw (short_unsigned_type_node);
      tree fnstsw_call = build_call_expr (fnstsw, 0);
      tree sw_mod = build4 (TARGET_EXPR, short_unsigned_type_node, sw_var,
			    fnstsw_call, NULL_TREE, NULL_TREE);
      tree exceptions_x87 = fold_convert (integer_type_node, sw_var);
      tree update_mod = build4 (TARGET_EXPR, integer_type_node,
				exceptions_var, exceptions_x87,
				NULL_TREE, NULL_TREE);
      *update = build2 (COMPOUND_EXPR, integer_type_node,
			sw_mod, update_mod);
      tree update_fldenv = build_call_expr (fldenv, 1, fenv_addr);
      *update = build2 (COMPOUND_EXPR, void_type_node, *update, update_fldenv);
    }
  if (TARGET_SSE && TARGET_SSE_MATH)
    {
      tree mxcsr_orig_var = create_tmp_var_raw (unsigned_type_node);
      tree mxcsr_mod_var = create_tmp_var_raw (unsigned_type_node);
      tree stmxcsr = get_ix86_builtin (IX86_BUILTIN_STMXCSR);
      tree ldmxcsr = get_ix86_builtin (IX86_BUILTIN_LDMXCSR);
      tree stmxcsr_hold_call = build_call_expr (stmxcsr, 0);
      tree hold_assign_orig = build4 (TARGET_EXPR, unsigned_type_node,
				      mxcsr_orig_var, stmxcsr_hold_call,
				      NULL_TREE, NULL_TREE);
      tree hold_mod_val = build2 (BIT_IOR_EXPR, unsigned_type_node,
				  mxcsr_orig_var,
				  build_int_cst (unsigned_type_node, 0x1f80));
      hold_mod_val = build2 (BIT_AND_EXPR, unsigned_type_node, hold_mod_val,
			     build_int_cst (unsigned_type_node, 0xffffffc0));
      tree hold_assign_mod = build4 (TARGET_EXPR, unsigned_type_node,
				     mxcsr_mod_var, hold_mod_val,
				     NULL_TREE, NULL_TREE);
      tree ldmxcsr_hold_call = build_call_expr (ldmxcsr, 1, mxcsr_mod_var);
      tree hold_all = build2 (COMPOUND_EXPR, unsigned_type_node,
			      hold_assign_orig, hold_assign_mod);
      hold_all = build2 (COMPOUND_EXPR, void_type_node, hold_all,
			 ldmxcsr_hold_call);
      if (*hold)
	*hold = build2 (COMPOUND_EXPR, void_type_node, *hold, hold_all);
      else
	*hold = hold_all;
      tree ldmxcsr_clear_call = build_call_expr (ldmxcsr, 1, mxcsr_mod_var);
      if (*clear)
	*clear = build2 (COMPOUND_EXPR, void_type_node, *clear,
			 ldmxcsr_clear_call);
      else
	*clear = ldmxcsr_clear_call;
      tree stxmcsr_update_call = build_call_expr (stmxcsr, 0);
      tree exceptions_sse = fold_convert (integer_type_node,
					  stxmcsr_update_call);
      if (*update)
	{
	  tree exceptions_mod = build2 (BIT_IOR_EXPR, integer_type_node,
					exceptions_var, exceptions_sse);
	  tree exceptions_assign = build2 (MODIFY_EXPR, integer_type_node,
					   exceptions_var, exceptions_mod);
	  *update = build2 (COMPOUND_EXPR, integer_type_node, *update,
			    exceptions_assign);
	}
      else
	*update = build4 (TARGET_EXPR, integer_type_node, exceptions_var,
			  exceptions_sse, NULL_TREE, NULL_TREE);
      tree ldmxcsr_update_call = build_call_expr (ldmxcsr, 1, mxcsr_orig_var);
      *update = build2 (COMPOUND_EXPR, void_type_node, *update,
			ldmxcsr_update_call);
    }
  tree atomic_feraiseexcept
    = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  tree atomic_feraiseexcept_call = build_call_expr (atomic_feraiseexcept,
						    1, exceptions_var);
  *update = build2 (COMPOUND_EXPR, void_type_node, *update,
		    atomic_feraiseexcept_call);
}

#if !TARGET_MACHO && !TARGET_DLLIMPORT_DECL_ATTRIBUTES
/* For i386, common symbol is local only for non-PIE binaries.  For
   x86-64, common symbol is local only for non-PIE binaries or linker
   supports copy reloc in PIE binaries.   */

static bool
ix86_binds_local_p (const_tree exp)
{
  bool direct_extern_access
    = (ix86_direct_extern_access
       && !(VAR_OR_FUNCTION_DECL_P (exp)
	    && lookup_attribute ("nodirect_extern_access",
				 DECL_ATTRIBUTES (exp))));
  if (!direct_extern_access)
    ix86_has_no_direct_extern_access = true;
  return default_binds_local_p_3 (exp, flag_shlib != 0, true,
				  direct_extern_access,
				  (direct_extern_access
				   && (!flag_pic
				       || (TARGET_64BIT
					   && HAVE_LD_PIE_COPYRELOC != 0))));
}

/* If flag_pic or ix86_direct_extern_access is false, then neither
   local nor global relocs should be placed in readonly memory.  */

static int
ix86_reloc_rw_mask (void)
{
  return (flag_pic || !ix86_direct_extern_access) ? 3 : 0;
}
#endif

/* Return true iff ADDR can be used as a symbolic base address.  */

static bool
symbolic_base_address_p (rtx addr)
{
  if (GET_CODE (addr) == SYMBOL_REF)
    return true;

  if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_GOTOFF)
    return true;

  return false;
}

/* Return true iff ADDR can be used as a base address.  */

static bool
base_address_p (rtx addr)
{
  if (REG_P (addr))
    return true;

  if (symbolic_base_address_p (addr))
    return true;

  return false;
}

/* If MEM is in the form of [(base+symbase)+offset], extract the three
   parts of address and set to BASE, SYMBASE and OFFSET, otherwise
   return false.  */

static bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *symbase, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);
  
  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (base_address_p (addr))
    {
      *base = addr;
      *symbase = const0_rtx;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && base_address_p (XEXP (addr, 0)))
    {
      rtx addend = XEXP (addr, 1);

      if (GET_CODE (addend) == CONST)
	addend = XEXP (addend, 0);

      if (CONST_INT_P (addend))
	{
	  *base = XEXP (addr, 0);
	  *symbase = const0_rtx;
	  *offset = addend;
	  return true;
	}

      /* Also accept REG + symbolic ref, with or without a CONST_INT
	 offset.  */
      if (REG_P (XEXP (addr, 0)))
	{
	  if (symbolic_base_address_p (addend))
	    {
	      *base = XEXP (addr, 0);
	      *symbase = addend;
	      *offset = const0_rtx;
	      return true;
	    }

	  if (GET_CODE (addend) == PLUS
	      && symbolic_base_address_p (XEXP (addend, 0))
	      && CONST_INT_P (XEXP (addend, 1)))
	    {
	      *base = XEXP (addr, 0);
	      *symbase = XEXP (addend, 0);
	      *offset = XEXP (addend, 1);
	      return true;
	    }
	}
    }

  return false;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into move multiple.  LOAD is true if they are load instructions.
   MODE is the mode of memory operands.  */

bool
ix86_operands_ok_for_move_multiple (rtx *operands, bool load,
				    machine_mode mode)
{
  HOST_WIDE_INT offval_1, offval_2, msize;
  rtx mem_1, mem_2, reg_1, reg_2, base_1, base_2,
    symbase_1, symbase_2, offset_1, offset_2;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
      reg_1 = operands[0];
      reg_2 = operands[2];
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
      reg_1 = operands[1];
      reg_2 = operands[3];
    }

  gcc_assert (REG_P (reg_1) && REG_P (reg_2));

  if (REGNO (reg_1) != REGNO (reg_2))
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  if (!extract_base_offset_in_addr (mem_1, &base_1, &symbase_1, &offset_1))
    return false;
  if (!extract_base_offset_in_addr (mem_2, &base_2, &symbase_2, &offset_2))
    return false;

  /* Check if the bases are the same.  */
  if (!rtx_equal_p (base_1, base_2) || !rtx_equal_p (symbase_1, symbase_2))
    return false;

  offval_1 = INTVAL (offset_1);
  offval_2 = INTVAL (offset_2);
  msize = GET_MODE_SIZE (mode);
  /* Check if mem_1 is adjacent to mem_2 and mem_1 has lower address.  */
  if (offval_1 + msize != offval_2)
    return false;

  return true;
}

/* Implement the TARGET_OPTAB_SUPPORTED_P hook.  */

static bool
ix86_optab_supported_p (int op, machine_mode mode1, machine_mode,
			optimization_type opt_type)
{
  switch (op)
    {
    case asin_optab:
    case acos_optab:
    case log1p_optab:
    case exp_optab:
    case exp10_optab:
    case exp2_optab:
    case expm1_optab:
    case ldexp_optab:
    case scalb_optab:
    case round_optab:
    case lround_optab:
      return opt_type == OPTIMIZE_FOR_SPEED;

    case rint_optab:
      if (SSE_FLOAT_MODE_P (mode1)
	  && TARGET_SSE_MATH
	  && !flag_trapping_math
	  && !TARGET_SSE4_1
	  && mode1 != HFmode)
	return opt_type == OPTIMIZE_FOR_SPEED;
      return true;

    case floor_optab:
    case ceil_optab:
    case btrunc_optab:
      if (((SSE_FLOAT_MODE_P (mode1)
	    && TARGET_SSE_MATH
	    && TARGET_SSE4_1)
	   || mode1 == HFmode)
	  && !flag_trapping_math)
	return true;
      return opt_type == OPTIMIZE_FOR_SPEED;

    case rsqrt_optab:
      return opt_type == OPTIMIZE_FOR_SPEED && use_rsqrt_p (mode1);

    default:
      return true;
    }
}

/* Address space support.

   This is not "far pointers" in the 16-bit sense, but an easy way
   to use %fs and %gs segment prefixes.  Therefore:

    (a) All address spaces have the same modes,
    (b) All address spaces have the same addresss forms,
    (c) While %fs and %gs are technically subsets of the generic
        address space, they are probably not subsets of each other.
    (d) Since we have no access to the segment base register values
        without resorting to a system call, we cannot convert a
        non-default address space to a default address space.
        Therefore we do not claim %fs or %gs are subsets of generic.

   Therefore we can (mostly) use the default hooks.  */

/* All use of segmentation is assumed to make address 0 valid.  */

static bool
ix86_addr_space_zero_address_valid (addr_space_t as)
{
  return as != ADDR_SPACE_GENERIC;
}

static void
ix86_init_libfuncs (void)
{
  if (TARGET_64BIT)
    {
      set_optab_libfunc (sdivmod_optab, TImode, "__divmodti4");
      set_optab_libfunc (udivmod_optab, TImode, "__udivmodti4");
    }
  else
    {
      set_optab_libfunc (sdivmod_optab, DImode, "__divmoddi4");
      set_optab_libfunc (udivmod_optab, DImode, "__udivmoddi4");
    }

#if TARGET_MACHO
  darwin_rename_builtins ();
#endif
}

/* Set the value of FLT_EVAL_METHOD in float.h.  When using only the
   FPU, assume that the fpcw is set to extended precision; when using
   only SSE, rounding is correct; when using both SSE and the FPU,
   the rounding precision is indeterminate, since either may be chosen
   apparently at random.  */

static enum flt_eval_method
ix86_get_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_FAST:
	/* The fastest type to promote to will always be the native type,
	   whether that occurs with implicit excess precision or
	   otherwise.  */
	return TARGET_AVX512FP16
	       ? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16
	       : FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
      case EXCESS_PRECISION_TYPE_STANDARD:
      case EXCESS_PRECISION_TYPE_IMPLICIT:
	/* Otherwise, the excess precision we want when we are
	   in a standards compliant mode, and the implicit precision we
	   provide would be identical were it not for the unpredictable
	   cases.  */
	if (TARGET_AVX512FP16 && TARGET_SSE_MATH)
	  return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
	else if (!TARGET_80387)
	  return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
	else if (!TARGET_MIX_SSE_I387)
	  {
	    if (!(TARGET_SSE && TARGET_SSE_MATH))
	      return FLT_EVAL_METHOD_PROMOTE_TO_LONG_DOUBLE;
	    else if (TARGET_SSE2)
	      return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
	  }

	/* If we are in standards compliant mode, but we know we will
	   calculate in unpredictable precision, return
	   FLT_EVAL_METHOD_FLOAT.  There is no reason to introduce explicit
	   excess precision if the target can't guarantee it will honor
	   it.  */
	return (type == EXCESS_PRECISION_TYPE_STANDARD
		? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT
		: FLT_EVAL_METHOD_UNPREDICTABLE);
      case EXCESS_PRECISION_TYPE_FLOAT16:
	if (TARGET_80387
	    && !(TARGET_SSE_MATH && TARGET_SSE))
	  error ("%<-fexcess-precision=16%> is not compatible with %<-mfpmath=387%>");
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
      default:
	gcc_unreachable ();
    }

  return FLT_EVAL_METHOD_UNPREDICTABLE;
}

/* Return true if _BitInt(N) is supported and fill its details into *INFO.  */
bool
ix86_bitint_type_info (int n, struct bitint_info *info)
{
  if (n <= 8)
    info->limb_mode = QImode;
  else if (n <= 16)
    info->limb_mode = HImode;
  else if (n <= 32 || (!TARGET_64BIT && n > 64))
    info->limb_mode = SImode;
  else
    info->limb_mode = DImode;
  info->abi_limb_mode = info->limb_mode;
  info->big_endian = false;
  info->extended = false;
  return true;
}

/* Returns modified FUNCTION_TYPE for cdtor callabi.  */
tree
ix86_cxx_adjust_cdtor_callabi_fntype (tree fntype)
{
  if (TARGET_64BIT
      || TARGET_RTD
      || ix86_function_type_abi (fntype) != MS_ABI)
    return fntype;
  /* For 32-bit MS ABI add thiscall attribute.  */
  tree attribs = tree_cons (get_identifier ("thiscall"), NULL_TREE,
			    TYPE_ATTRIBUTES (fntype));
  return build_type_attribute_variant (fntype, attribs);
}

/* Implement PUSH_ROUNDING.  On 386, we have pushw instruction that
   decrements by exactly 2 no matter what the position was, there is no pushb.

   But as CIE data alignment factor on this arch is -4 for 32bit targets
   and -8 for 64bit targets, we need to make sure all stack pointer adjustments
   are in multiple of 4 for 32bit targets and 8 for 64bit targets.  */

poly_int64
ix86_push_rounding (poly_int64 bytes)
{
  return ROUND_UP (bytes, UNITS_PER_WORD);
}

/* Use 8 bits metadata start from bit48 for LAM_U48,
   6 bits metadat start from bit57 for LAM_U57.  */
#define IX86_HWASAN_SHIFT (ix86_lam_type == lam_u48		\
			   ? 48					\
			   : (ix86_lam_type == lam_u57 ? 57 : 0))
#define IX86_HWASAN_TAG_SIZE (ix86_lam_type == lam_u48		\
			      ? 8				\
			      : (ix86_lam_type == lam_u57 ? 6 : 0))

/* Implement TARGET_MEMTAG_CAN_TAG_ADDRESSES.  */
bool
ix86_memtag_can_tag_addresses ()
{
  return ix86_lam_type != lam_none && TARGET_LP64;
}

/* Implement TARGET_MEMTAG_TAG_SIZE.  */
unsigned char
ix86_memtag_tag_size ()
{
  return IX86_HWASAN_TAG_SIZE;
}

/* Implement TARGET_MEMTAG_SET_TAG.  */
rtx
ix86_memtag_set_tag (rtx untagged, rtx tag, rtx target)
{
  /* default_memtag_insert_random_tag may
     generate tag with value more than 6 bits.  */
  if (ix86_lam_type == lam_u57)
    {
      unsigned HOST_WIDE_INT and_imm
	= (HOST_WIDE_INT_1U << IX86_HWASAN_TAG_SIZE) - 1;

      emit_insn (gen_andqi3 (tag, tag, GEN_INT (and_imm)));
    }
  tag = expand_simple_binop (Pmode, ASHIFT, tag,
			     GEN_INT (IX86_HWASAN_SHIFT), NULL_RTX,
			     /* unsignedp = */1, OPTAB_WIDEN);
  rtx ret = expand_simple_binop (Pmode, IOR, untagged, tag, target,
				 /* unsignedp = */1, OPTAB_DIRECT);
  return ret;
}

/* Implement TARGET_MEMTAG_EXTRACT_TAG.  */
rtx
ix86_memtag_extract_tag (rtx tagged_pointer, rtx target)
{
  rtx tag = expand_simple_binop (Pmode, LSHIFTRT, tagged_pointer,
				 GEN_INT (IX86_HWASAN_SHIFT), target,
				 /* unsignedp = */0,
				 OPTAB_DIRECT);
  rtx ret = gen_reg_rtx (QImode);
  /* Mask off bit63 when LAM_U57.  */
  if (ix86_lam_type == lam_u57)
    {
      unsigned HOST_WIDE_INT and_imm
	= (HOST_WIDE_INT_1U << IX86_HWASAN_TAG_SIZE) - 1;
      emit_insn (gen_andqi3 (ret, gen_lowpart (QImode, tag),
			     gen_int_mode (and_imm, QImode)));
    }
  else
    emit_move_insn (ret, gen_lowpart (QImode, tag));
  return ret;
}

/* The default implementation of TARGET_MEMTAG_UNTAGGED_POINTER.  */
rtx
ix86_memtag_untagged_pointer (rtx tagged_pointer, rtx target)
{
  /* Leave bit63 alone.  */
  rtx tag_mask = gen_int_mode (((HOST_WIDE_INT_1U << IX86_HWASAN_SHIFT)
				+ (HOST_WIDE_INT_1U << 63) - 1),
			       Pmode);
  rtx untagged_base = expand_simple_binop (Pmode, AND, tagged_pointer,
					   tag_mask, target, true,
					   OPTAB_DIRECT);
  gcc_assert (untagged_base);
  return untagged_base;
}

/* Implement TARGET_MEMTAG_ADD_TAG.  */
rtx
ix86_memtag_add_tag (rtx base, poly_int64 offset, unsigned char tag_offset)
{
  rtx base_tag = gen_reg_rtx (QImode);
  rtx base_addr = gen_reg_rtx (Pmode);
  rtx tagged_addr = gen_reg_rtx (Pmode);
  rtx new_tag = gen_reg_rtx (QImode);
  unsigned HOST_WIDE_INT and_imm
    = (HOST_WIDE_INT_1U << IX86_HWASAN_SHIFT) - 1;

  /* When there's "overflow" in tag adding,
     need to mask the most significant bit off.  */
  emit_move_insn (base_tag, ix86_memtag_extract_tag (base, NULL_RTX));
  emit_move_insn (base_addr,
		  ix86_memtag_untagged_pointer (base, NULL_RTX));
  emit_insn (gen_add2_insn (base_tag, gen_int_mode (tag_offset, QImode)));
  emit_move_insn (new_tag, base_tag);
  emit_insn (gen_andqi3 (new_tag, new_tag, gen_int_mode (and_imm, QImode)));
  emit_move_insn (tagged_addr,
		  ix86_memtag_set_tag (base_addr, new_tag, NULL_RTX));
  return plus_constant (Pmode, tagged_addr, offset);
}

/* Target-specific selftests.  */

#if CHECKING_P

namespace selftest {

/* Verify that hard regs are dumped as expected (in compact mode).  */

static void
ix86_test_dumping_hard_regs ()
{
  ASSERT_RTL_DUMP_EQ ("(reg:SI ax)", gen_raw_REG (SImode, 0));
  ASSERT_RTL_DUMP_EQ ("(reg:SI dx)", gen_raw_REG (SImode, 1));
}

/* Test dumping an insn with repeated references to the same SCRATCH,
   to verify the rtx_reuse code.  */

static void
ix86_test_dumping_memory_blockage ()
{
  set_new_first_and_last_insn (NULL, NULL);

  rtx pat = gen_memory_blockage ();
  rtx_reuse_manager r;
  r.preprocess (pat);

  /* Verify that the repeated references to the SCRATCH show use
     reuse IDS.  The first should be prefixed with a reuse ID,
     and the second should be dumped as a "reuse_rtx" of that ID.
     The expected string assumes Pmode == DImode.  */
  if (Pmode == DImode)
    ASSERT_RTL_DUMP_EQ_WITH_REUSE
      ("(cinsn 1 (set (mem/v:BLK (0|scratch:DI) [0  A8])\n"
       "        (unspec:BLK [\n"
       "                (mem/v:BLK (reuse_rtx 0) [0  A8])\n"
       "            ] UNSPEC_MEMORY_BLOCKAGE)))\n", pat, &r);
}

/* Verify loading an RTL dump; specifically a dump of copying
   a param on x86_64 from a hard reg into the frame.
   This test is target-specific since the dump contains target-specific
   hard reg names.  */

static void
ix86_test_loading_dump_fragment_1 ()
{
  rtl_dump_test t (SELFTEST_LOCATION,
		   locate_file ("x86_64/copy-hard-reg-into-frame.rtl"));

  rtx_insn *insn = get_insn_by_uid (1);

  /* The block structure and indentation here is purely for
     readability; it mirrors the structure of the rtx.  */
  tree mem_expr;
  {
    rtx pat = PATTERN (insn);
    ASSERT_EQ (SET, GET_CODE (pat));
    {
      rtx dest = SET_DEST (pat);
      ASSERT_EQ (MEM, GET_CODE (dest));
      /* Verify the "/c" was parsed.  */
      ASSERT_TRUE (RTX_FLAG (dest, call));
      ASSERT_EQ (SImode, GET_MODE (dest));
      {
	rtx addr = XEXP (dest, 0);
	ASSERT_EQ (PLUS, GET_CODE (addr));
	ASSERT_EQ (DImode, GET_MODE (addr));
	{
	  rtx lhs = XEXP (addr, 0);
	  /* Verify that the "frame" REG was consolidated.  */
	  ASSERT_RTX_PTR_EQ (frame_pointer_rtx, lhs);
	}
	{
	  rtx rhs = XEXP (addr, 1);
	  ASSERT_EQ (CONST_INT, GET_CODE (rhs));
	  ASSERT_EQ (-4, INTVAL (rhs));
	}
      }
      /* Verify the "[1 i+0 S4 A32]" was parsed.  */
      ASSERT_EQ (1, MEM_ALIAS_SET (dest));
      /* "i" should have been handled by synthesizing a global int
	 variable named "i".  */
      mem_expr = MEM_EXPR (dest);
      ASSERT_NE (mem_expr, NULL);
      ASSERT_EQ (VAR_DECL, TREE_CODE (mem_expr));
      ASSERT_EQ (integer_type_node, TREE_TYPE (mem_expr));
      ASSERT_EQ (IDENTIFIER_NODE, TREE_CODE (DECL_NAME (mem_expr)));
      ASSERT_STREQ ("i", IDENTIFIER_POINTER (DECL_NAME (mem_expr)));
      /* "+0".  */
      ASSERT_TRUE (MEM_OFFSET_KNOWN_P (dest));
      ASSERT_EQ (0, MEM_OFFSET (dest));
      /* "S4".  */
      ASSERT_EQ (4, MEM_SIZE (dest));
      /* "A32.  */
      ASSERT_EQ (32, MEM_ALIGN (dest));
    }
    {
      rtx src = SET_SRC (pat);
      ASSERT_EQ (REG, GET_CODE (src));
      ASSERT_EQ (SImode, GET_MODE (src));
      ASSERT_EQ (5, REGNO (src));
      tree reg_expr = REG_EXPR (src);
      /* "i" here should point to the same var as for the MEM_EXPR.  */
      ASSERT_EQ (reg_expr, mem_expr);
    }
  }
}

/* Verify that the RTL loader copes with a call_insn dump.
   This test is target-specific since the dump contains a target-specific
   hard reg name.  */

static void
ix86_test_loading_call_insn ()
{
  /* The test dump includes register "xmm0", where requires TARGET_SSE
     to exist.  */
  if (!TARGET_SSE)
    return;

  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("x86_64/call-insn.rtl"));

  rtx_insn *insn = get_insns ();
  ASSERT_EQ (CALL_INSN, GET_CODE (insn));

  /* "/j".  */
  ASSERT_TRUE (RTX_FLAG (insn, jump));

  rtx pat = PATTERN (insn);
  ASSERT_EQ (CALL, GET_CODE (SET_SRC (pat)));

  /* Verify REG_NOTES.  */
  {
    /* "(expr_list:REG_CALL_DECL".   */
    ASSERT_EQ (EXPR_LIST, GET_CODE (REG_NOTES (insn)));
    rtx_expr_list *note0 = as_a <rtx_expr_list *> (REG_NOTES (insn));
    ASSERT_EQ (REG_CALL_DECL, REG_NOTE_KIND (note0));

    /* "(expr_list:REG_EH_REGION (const_int 0 [0])".  */
    rtx_expr_list *note1 = note0->next ();
    ASSERT_EQ (REG_EH_REGION, REG_NOTE_KIND (note1));

    ASSERT_EQ (NULL, note1->next ());
  }

  /* Verify CALL_INSN_FUNCTION_USAGE.  */
  {
    /* "(expr_list:DF (use (reg:DF 21 xmm0))".  */
    rtx_expr_list *usage
      = as_a <rtx_expr_list *> (CALL_INSN_FUNCTION_USAGE (insn));
    ASSERT_EQ (EXPR_LIST, GET_CODE (usage));
    ASSERT_EQ (DFmode, GET_MODE (usage));
    ASSERT_EQ (USE, GET_CODE (usage->element ()));
    ASSERT_EQ (NULL, usage->next ());
  }
}

/* Verify that the RTL loader copes a dump from print_rtx_function.
   This test is target-specific since the dump contains target-specific
   hard reg names.  */

static void
ix86_test_loading_full_dump ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("x86_64/times-two.rtl"));

  ASSERT_STREQ ("times_two", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  rtx_insn *insn_1 = get_insn_by_uid (1);
  ASSERT_EQ (NOTE, GET_CODE (insn_1));

  rtx_insn *insn_7 = get_insn_by_uid (7);
  ASSERT_EQ (INSN, GET_CODE (insn_7));
  ASSERT_EQ (PARALLEL, GET_CODE (PATTERN (insn_7)));

  rtx_insn *insn_15 = get_insn_by_uid (15);
  ASSERT_EQ (INSN, GET_CODE (insn_15));
  ASSERT_EQ (USE, GET_CODE (PATTERN (insn_15)));

  /* Verify crtl->return_rtx.  */
  ASSERT_EQ (REG, GET_CODE (crtl->return_rtx));
  ASSERT_EQ (0, REGNO (crtl->return_rtx));
  ASSERT_EQ (SImode, GET_MODE (crtl->return_rtx));
}

/* Verify that the RTL loader copes with UNSPEC and UNSPEC_VOLATILE insns.
   In particular, verify that it correctly loads the 2nd operand.
   This test is target-specific since these are machine-specific
   operands (and enums).  */

static void
ix86_test_loading_unspec ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("x86_64/unspec.rtl"));

  ASSERT_STREQ ("test_unspec", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  ASSERT_TRUE (cfun);

  /* Test of an UNSPEC.  */
   rtx_insn *insn = get_insns ();
  ASSERT_EQ (INSN, GET_CODE (insn));
  rtx set = single_set (insn);
  ASSERT_NE (NULL, set);
  rtx dst = SET_DEST (set);
  ASSERT_EQ (MEM, GET_CODE (dst));
  rtx src = SET_SRC (set);
  ASSERT_EQ (UNSPEC, GET_CODE (src));
  ASSERT_EQ (BLKmode, GET_MODE (src));
  ASSERT_EQ (UNSPEC_MEMORY_BLOCKAGE, XINT (src, 1));

  rtx v0 = XVECEXP (src, 0, 0);

  /* Verify that the two uses of the first SCRATCH have pointer
     equality.  */
  rtx scratch_a = XEXP (dst, 0);
  ASSERT_EQ (SCRATCH, GET_CODE (scratch_a));

  rtx scratch_b = XEXP (v0, 0);
  ASSERT_EQ (SCRATCH, GET_CODE (scratch_b));

  ASSERT_EQ (scratch_a, scratch_b);

  /* Verify that the two mems are thus treated as equal.  */
  ASSERT_TRUE (rtx_equal_p (dst, v0));

  /* Verify that the insn is recognized.  */
  ASSERT_NE(-1, recog_memoized (insn));

  /* Test of an UNSPEC_VOLATILE, which has its own enum values.  */
  insn = NEXT_INSN (insn);
  ASSERT_EQ (INSN, GET_CODE (insn));

  set = single_set (insn);
  ASSERT_NE (NULL, set);

  src = SET_SRC (set);
  ASSERT_EQ (UNSPEC_VOLATILE, GET_CODE (src));
  ASSERT_EQ (UNSPECV_RDTSCP, XINT (src, 1));
}

/* Run all target-specific selftests.  */

static void
ix86_run_selftests (void)
{
  ix86_test_dumping_hard_regs ();
  ix86_test_dumping_memory_blockage ();

  /* Various tests of loading RTL dumps, here because they contain
     ix86-isms (e.g. names of hard regs).  */
  ix86_test_loading_dump_fragment_1 ();
  ix86_test_loading_call_insn ();
  ix86_test_loading_full_dump ();
  ix86_test_loading_unspec ();
}

} // namespace selftest

#endif /* CHECKING_P */

static const scoped_attribute_specs *const ix86_attribute_table[] =
{
  &ix86_gnu_attribute_table
};

/* Initialize the GCC target structure.  */
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY ix86_return_in_memory

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS ix86_legitimize_address

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ix86_attribute_table
#undef TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P hook_bool_const_tree_true
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#  undef TARGET_MERGE_DECL_ATTRIBUTES
#  define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef TARGET_INVALID_CONVERSION
#define TARGET_INVALID_CONVERSION ix86_invalid_conversion

#undef TARGET_INVALID_UNARY_OP
#define TARGET_INVALID_UNARY_OP ix86_invalid_unary_op

#undef TARGET_INVALID_BINARY_OP
#define TARGET_INVALID_BINARY_OP ix86_invalid_binary_op

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES ix86_comp_type_attributes

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS ix86_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL ix86_builtin_decl
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN ix86_expand_builtin

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  ix86_builtin_vectorized_function

#undef TARGET_VECTORIZE_BUILTIN_GATHER
#define TARGET_VECTORIZE_BUILTIN_GATHER ix86_vectorize_builtin_gather

#undef TARGET_VECTORIZE_BUILTIN_SCATTER
#define TARGET_VECTORIZE_BUILTIN_SCATTER ix86_vectorize_builtin_scatter

#undef TARGET_BUILTIN_RECIPROCAL
#define TARGET_BUILTIN_RECIPROCAL ix86_builtin_reciprocal

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE ix86_output_function_epilogue

#undef TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY
#define TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY \
  ix86_print_patchable_function_entry

#undef TARGET_ENCODE_SECTION_INFO
#ifndef SUBTARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO ix86_encode_section_info
#else
#define TARGET_ENCODE_SECTION_INFO SUBTARGET_ENCODE_SECTION_INFO
#endif

#undef TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""
#undef TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP ASM_BYTE

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP ASM_SHORT
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP ASM_LONG
#ifdef ASM_QUAD
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP ASM_QUAD
#endif

#undef TARGET_PROFILE_BEFORE_PROLOGUE
#define TARGET_PROFILE_BEFORE_PROLOGUE ix86_profile_before_prologue

#undef TARGET_MANGLE_DECL_ASSEMBLER_NAME
#define TARGET_MANGLE_DECL_ASSEMBLER_NAME ix86_mangle_decl_assembler_name

#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND ix86_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS ix86_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P ix86_print_operand_punct_valid_p
#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA i386_asm_output_addr_const_extra

#undef TARGET_SCHED_INIT_GLOBAL
#define TARGET_SCHED_INIT_GLOBAL ix86_sched_init_global
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST ix86_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE ix86_issue_rate
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  ia32_multipass_dfa_lookahead
#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P ix86_macro_fusion_p
#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P ix86_macro_fusion_pair_p

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL ix86_function_ok_for_sibcall

#undef TARGET_MEMMODEL_CHECK
#define TARGET_MEMMODEL_CHECK ix86_memmodel_check

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV ix86_atomic_assign_expand_fenv

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM ix86_cannot_force_const_mem
#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_true

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS ix86_delegitimize_address

#undef TARGET_CONST_NOT_OK_FOR_DEBUG_P
#define TARGET_CONST_NOT_OK_FOR_DEBUG_P ix86_const_not_ok_for_debug_p

#undef TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P ix86_ms_bitfield_layout_p

#if TARGET_MACHO
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P darwin_binds_local_p
#else
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P ix86_binds_local_p
#endif
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P i386_pe_binds_local_p
#endif

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK x86_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK x86_can_output_mi_thunk

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START x86_file_start

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE ix86_option_override

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST ix86_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST ix86_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS ix86_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST ix86_address_cost

#undef TARGET_OVERLAP_OP_BY_PIECES_P
#define TARGET_OVERLAP_OP_BY_PIECES_P hook_bool_void_true

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM FLAGS_REG
#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS ix86_fixed_condition_code_regs
#undef TARGET_CC_MODES_COMPATIBLE
#define TARGET_CC_MODES_COMPATIBLE ix86_cc_modes_compatible

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG ix86_reorg

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST ix86_build_builtin_va_list

#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN ix86_fold_builtin

#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN ix86_gimple_fold_builtin

#undef TARGET_COMPARE_VERSION_PRIORITY
#define TARGET_COMPARE_VERSION_PRIORITY ix86_compare_version_priority

#undef TARGET_GENERATE_VERSION_DISPATCHER_BODY
#define TARGET_GENERATE_VERSION_DISPATCHER_BODY \
  ix86_generate_version_dispatcher_body

#undef TARGET_GET_FUNCTION_VERSIONS_DISPATCHER
#define TARGET_GET_FUNCTION_VERSIONS_DISPATCHER \
  ix86_get_function_versions_dispatcher

#undef TARGET_ENUM_VA_LIST_P
#define TARGET_ENUM_VA_LIST_P ix86_enum_va_list

#undef TARGET_FN_ABI_VA_LIST
#define TARGET_FN_ABI_VA_LIST ix86_fn_abi_va_list

#undef TARGET_CANONICAL_VA_LIST_TYPE
#define TARGET_CANONICAL_VA_LIST_TYPE ix86_canonical_va_list_type

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START ix86_va_start

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST ix86_md_asm_adjust

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION ix86_get_excess_precision
#undef TARGET_C_BITINT_TYPE_INFO
#define TARGET_C_BITINT_TYPE_INFO ix86_bitint_type_info
#undef TARGET_CXX_ADJUST_CDTOR_CALLABI_FNTYPE
#define TARGET_CXX_ADJUST_CDTOR_CALLABI_FNTYPE ix86_cxx_adjust_cdtor_callabi_fntype
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef TARGET_PUSH_ARGUMENT
#define TARGET_PUSH_ARGUMENT ix86_push_argument
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS ix86_setup_incoming_varargs
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK ix86_must_pass_in_stack
#undef TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS ix86_allocate_stack_slots_for_args
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE ix86_function_arg_advance
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG ix86_function_arg
#undef TARGET_INIT_PIC_REG
#define TARGET_INIT_PIC_REG ix86_init_pic_reg
#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG ix86_use_pseudo_pic_reg
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY ix86_function_arg_boundary
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE ix86_pass_by_reference
#undef TARGET_INTERNAL_ARG_POINTER
#define TARGET_INTERNAL_ARG_POINTER ix86_internal_arg_pointer
#undef TARGET_UPDATE_STACK_BOUNDARY
#define TARGET_UPDATE_STACK_BOUNDARY ix86_update_stack_boundary
#undef TARGET_GET_DRAP_RTX
#define TARGET_GET_DRAP_RTX ix86_get_drap_rtx
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_STATIC_CHAIN
#define TARGET_STATIC_CHAIN ix86_static_chain
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT ix86_trampoline_init
#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS ix86_return_pops_args

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN ix86_warn_func_return

#undef TARGET_LEGITIMATE_COMBINED_INSN
#define TARGET_LEGITIMATE_COMBINED_INSN ix86_legitimate_combined_insn

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET ix86_asan_shadow_offset

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR ix86_gimplify_va_arg

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P ix86_scalar_mode_supported_p

#undef TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P
#define TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P	\
ix86_libgcc_floating_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P ix86_vector_mode_supported_p

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX ix86_c_mode_for_suffix

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL i386_output_dwarf_dtprel
#endif

#ifdef SUBTARGET_INSERT_ATTRIBUTES
#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES SUBTARGET_INSERT_ATTRIBUTES
#endif

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE ix86_mangle_type

#undef TARGET_EMIT_SUPPORT_TINFOS
#define TARGET_EMIT_SUPPORT_TINFOS ix86_emit_support_tinfos

#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD ix86_stack_protect_guard

#if !TARGET_MACHO
#undef TARGET_STACK_PROTECT_FAIL
#define TARGET_STACK_PROTECT_FAIL ix86_stack_protect_fail
#endif

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE ix86_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P ix86_function_value_regno_p

#undef TARGET_ZERO_CALL_USED_REGS
#define TARGET_ZERO_CALL_USED_REGS ix86_zero_call_used_regs

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE ix86_promote_function_mode

#undef  TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE ix86_override_options_after_change

#undef TARGET_MEMBER_TYPE_FORCES_BLK
#define TARGET_MEMBER_TYPE_FORCES_BLK ix86_member_type_forces_blk

#undef TARGET_INSTANTIATE_DECLS
#define TARGET_INSTANTIATE_DECLS ix86_instantiate_decls

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD ix86_secondary_reload
#undef TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED ix86_secondary_memory_needed
#undef TARGET_SECONDARY_MEMORY_NEEDED_MODE
#define TARGET_SECONDARY_MEMORY_NEEDED_MODE ix86_secondary_memory_needed_mode

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS ix86_class_max_nregs

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS ix86_preferred_reload_class
#undef TARGET_PREFERRED_OUTPUT_RELOAD_CLASS
#define TARGET_PREFERRED_OUTPUT_RELOAD_CLASS ix86_preferred_output_reload_class
#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P ix86_class_likely_spilled_p

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  ix86_builtin_vectorization_cost
#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST ix86_vectorize_vec_perm_const
#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE \
  ix86_preferred_simd_mode
#undef TARGET_VECTORIZE_SPLIT_REDUCTION
#define TARGET_VECTORIZE_SPLIT_REDUCTION \
  ix86_split_reduction
#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  ix86_autovectorize_vector_modes
#undef TARGET_VECTORIZE_GET_MASK_MODE
#define TARGET_VECTORIZE_GET_MASK_MODE ix86_get_mask_mode
#undef TARGET_VECTORIZE_CREATE_COSTS
#define TARGET_VECTORIZE_CREATE_COSTS ix86_vectorize_create_costs

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION ix86_set_current_function

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P ix86_valid_target_attribute_p

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE ix86_function_specific_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE ix86_function_specific_restore

#undef TARGET_OPTION_POST_STREAM_IN
#define TARGET_OPTION_POST_STREAM_IN ix86_function_specific_post_stream_in

#undef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT ix86_function_specific_print

#undef TARGET_OPTION_FUNCTION_VERSIONS
#define TARGET_OPTION_FUNCTION_VERSIONS common_function_versions

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P ix86_can_inline_p

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P ix86_legitimate_address_p

#undef TARGET_REGISTER_PRIORITY
#define TARGET_REGISTER_PRIORITY ix86_register_priority

#undef TARGET_REGISTER_USAGE_LEVELING_P
#define TARGET_REGISTER_USAGE_LEVELING_P hook_bool_void_true

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P ix86_legitimate_constant_p

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT ix86_compute_frame_layout

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED ix86_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE ix86_can_eliminate

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY ix86_live_on_entry

#undef TARGET_ASM_CODE_END
#define TARGET_ASM_CODE_END ix86_code_end

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE ix86_conditional_register_usage

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON ix86_canonicalize_comparison

#undef TARGET_LOOP_UNROLL_ADJUST
#define TARGET_LOOP_UNROLL_ADJUST ix86_loop_unroll_adjust

/* Disabled due to PRs 70902, 71453, 71555, 71596 and 71657.  */
#undef TARGET_SPILL_CLASS
#define TARGET_SPILL_CLASS ix86_spill_class

#undef TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN
#define TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN \
  ix86_simd_clone_compute_vecsize_and_simdlen

#undef TARGET_SIMD_CLONE_ADJUST
#define TARGET_SIMD_CLONE_ADJUST ix86_simd_clone_adjust

#undef TARGET_SIMD_CLONE_USABLE
#define TARGET_SIMD_CLONE_USABLE ix86_simd_clone_usable

#undef TARGET_OMP_DEVICE_KIND_ARCH_ISA
#define TARGET_OMP_DEVICE_KIND_ARCH_ISA ix86_omp_device_kind_arch_isa

#undef TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P
#define TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P \
  ix86_float_exceptions_rounding_supported_p

#undef TARGET_MODE_EMIT
#define TARGET_MODE_EMIT ix86_emit_mode_set

#undef TARGET_MODE_NEEDED
#define TARGET_MODE_NEEDED ix86_mode_needed

#undef TARGET_MODE_AFTER
#define TARGET_MODE_AFTER ix86_mode_after

#undef TARGET_MODE_ENTRY
#define TARGET_MODE_ENTRY ix86_mode_entry

#undef TARGET_MODE_EXIT
#define TARGET_MODE_EXIT ix86_mode_exit

#undef TARGET_MODE_PRIORITY
#define TARGET_MODE_PRIORITY ix86_mode_priority

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_OFFLOAD_OPTIONS
#define TARGET_OFFLOAD_OPTIONS \
  ix86_offload_options

#undef TARGET_ABSOLUTE_BIGGEST_ALIGNMENT
#define TARGET_ABSOLUTE_BIGGEST_ALIGNMENT 512

#undef TARGET_OPTAB_SUPPORTED_P
#define TARGET_OPTAB_SUPPORTED_P ix86_optab_supported_p

#undef TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK ix86_hard_regno_scratch_ok

#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS X86_CUSTOM_FUNCTION_TEST

#undef TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID
#define TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID ix86_addr_space_zero_address_valid

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS ix86_init_libfuncs

#undef TARGET_EXPAND_DIVMOD_LIBFUNC
#define TARGET_EXPAND_DIVMOD_LIBFUNC ix86_expand_divmod_libfunc

#undef TARGET_MAX_NOCE_IFCVT_SEQ_COST
#define TARGET_MAX_NOCE_IFCVT_SEQ_COST ix86_max_noce_ifcvt_seq_cost

#undef TARGET_NOCE_CONVERSION_PROFITABLE_P
#define TARGET_NOCE_CONVERSION_PROFITABLE_P ix86_noce_conversion_profitable_p

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS ix86_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK ix86_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P ix86_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  ix86_hard_regno_call_part_clobbered

#undef TARGET_INSN_CALLEE_ABI
#define TARGET_INSN_CALLEE_ABI ix86_insn_callee_abi

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS ix86_can_change_mode_class

#undef TARGET_LOWER_LOCAL_DECL_ALIGNMENT
#define TARGET_LOWER_LOCAL_DECL_ALIGNMENT ix86_lower_local_decl_alignment

#undef TARGET_STATIC_RTX_ALIGNMENT
#define TARGET_STATIC_RTX_ALIGNMENT ix86_static_rtx_alignment
#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT ix86_constant_alignment

#undef TARGET_EMPTY_RECORD_P
#define TARGET_EMPTY_RECORD_P ix86_is_empty_record

#undef TARGET_WARN_PARAMETER_PASSING_ABI
#define TARGET_WARN_PARAMETER_PASSING_ABI ix86_warn_parameter_passing_abi

#undef TARGET_GET_MULTILIB_ABI_NAME
#define TARGET_GET_MULTILIB_ABI_NAME \
  ix86_get_multilib_abi_name

#undef TARGET_IFUNC_REF_LOCAL_OK
#define TARGET_IFUNC_REF_LOCAL_OK ix86_ifunc_ref_local_ok

#if !TARGET_MACHO && !TARGET_DLLIMPORT_DECL_ATTRIBUTES
# undef TARGET_ASM_RELOC_RW_MASK
# define TARGET_ASM_RELOC_RW_MASK ix86_reloc_rw_mask
#endif

#undef TARGET_MEMTAG_CAN_TAG_ADDRESSES
#define TARGET_MEMTAG_CAN_TAG_ADDRESSES ix86_memtag_can_tag_addresses

#undef TARGET_MEMTAG_ADD_TAG
#define TARGET_MEMTAG_ADD_TAG ix86_memtag_add_tag

#undef TARGET_MEMTAG_SET_TAG
#define TARGET_MEMTAG_SET_TAG ix86_memtag_set_tag

#undef TARGET_MEMTAG_EXTRACT_TAG
#define TARGET_MEMTAG_EXTRACT_TAG ix86_memtag_extract_tag

#undef TARGET_MEMTAG_UNTAGGED_POINTER
#define TARGET_MEMTAG_UNTAGGED_POINTER ix86_memtag_untagged_pointer

#undef TARGET_MEMTAG_TAG_SIZE
#define TARGET_MEMTAG_TAG_SIZE ix86_memtag_tag_size

static bool
ix86_libc_has_fast_function (int fcode ATTRIBUTE_UNUSED)
{
#ifdef OPTION_GLIBC
  if (OPTION_GLIBC)
    return (built_in_function)fcode == BUILT_IN_MEMPCPY;
  else
    return false;
#else
  return false;
#endif
}

#undef TARGET_LIBC_HAS_FAST_FUNCTION
#define TARGET_LIBC_HAS_FAST_FUNCTION ix86_libc_has_fast_function

static unsigned
ix86_libm_function_max_error (unsigned cfn, machine_mode mode,
			      bool boundary_p)
{
#ifdef OPTION_GLIBC
  bool glibc_p = OPTION_GLIBC;
#else
  bool glibc_p = false;
#endif
  if (glibc_p)
    {
      /* If __FAST_MATH__ is defined, glibc provides libmvec.  */
      unsigned int libmvec_ret = 0;
      if (!flag_trapping_math
	  && flag_unsafe_math_optimizations
	  && flag_finite_math_only
	  && !flag_signed_zeros
	  && !flag_errno_math)
	switch (cfn)
	  {
	  CASE_CFN_COS:
	  CASE_CFN_COS_FN:
	  CASE_CFN_SIN:
	  CASE_CFN_SIN_FN:
	    if (!boundary_p)
	      {
		/* With non-default rounding modes, libmvec provides
		   complete garbage in results.  E.g.
		   _ZGVcN8v_sinf for 1.40129846e-45f in FE_UPWARD
		   returns 0.00333309174f rather than 1.40129846e-45f.  */
		if (flag_rounding_math)
		  return ~0U;
		/* https://www.gnu.org/software/libc/manual/html_node/Errors-in-Math-Functions.html
		   claims libmvec maximum error is 4ulps.
		   My own random testing indicates 2ulps for SFmode and
		   0.5ulps for DFmode, but let's go with the 4ulps.  */
		libmvec_ret = 4;
	      }
	    break;
	  default:
	    break;
	  }
      unsigned int ret = glibc_linux_libm_function_max_error (cfn, mode,
							      boundary_p);
      return MAX (ret, libmvec_ret);
    }
  return default_libm_function_max_error (cfn, mode, boundary_p);
}

#undef TARGET_LIBM_FUNCTION_MAX_ERROR
#define TARGET_LIBM_FUNCTION_MAX_ERROR ix86_libm_function_max_error

#if CHECKING_P
#undef TARGET_RUN_TARGET_SELFTESTS
#define TARGET_RUN_TARGET_SELFTESTS selftest::ix86_run_selftests
#endif /* #if CHECKING_P */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-i386.h"
