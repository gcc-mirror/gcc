/* Copyright (C) 1997, 1998, 1999, 2000, 2001, 2004
   Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "reload.h"
#include "expr.h"
#include "obstack.h"
#include "except.h"
#include "function.h"
#include "optabs.h"
#include "toplev.h"
#include "basic-block.h"
#include "tm_p.h"
#include "ggc.h"
#include <ctype.h>
#include "target.h"
#include "target-def.h"

#ifndef FRV_INLINE
#define FRV_INLINE inline
#endif

/* Temporary register allocation support structure.  */
typedef struct frv_tmp_reg_struct
  {
    HARD_REG_SET regs;		/* possible registers to allocate */
    int next_reg[N_REG_CLASSES];	/* next register to allocate per class */
  }
frv_tmp_reg_t;

/* Register state information for VLIW re-packing phase.  These values must fit
   within an unsigned char.  */
#define REGSTATE_DEAD		0x00	/* register is currently dead */
#define REGSTATE_CC_MASK	0x07	/* Mask to isolate CCn for cond exec */
#define REGSTATE_LIVE		0x08	/* register is live */
#define REGSTATE_MODIFIED	0x10	/* reg modified in current VLIW insn */
#define REGSTATE_IF_TRUE	0x20	/* reg modified in cond exec true */
#define REGSTATE_IF_FALSE	0x40	/* reg modified in cond exec false */
#define REGSTATE_UNUSED		0x80	/* bit for hire */
#define REGSTATE_MASK		0xff	/* mask for the bits to set */

					/* conditional expression used */
#define REGSTATE_IF_EITHER	(REGSTATE_IF_TRUE | REGSTATE_IF_FALSE)

/* The following is not sure in the reg_state bytes, so can have a larger value
   than 0xff.  */
#define REGSTATE_CONDJUMP	0x100	/* conditional jump done in VLIW insn */

/* Used in frv_frame_accessor_t to indicate the direction of a register-to-
   memory move.  */
enum frv_stack_op
{
  FRV_LOAD,
  FRV_STORE
};

/* Information required by frv_frame_access.  */
typedef struct
{
  /* This field is FRV_LOAD if registers are to be loaded from the stack and
     FRV_STORE if they should be stored onto the stack.  FRV_STORE implies
     the move is being done by the prologue code while FRV_LOAD implies it
     is being done by the epilogue.  */
  enum frv_stack_op op;

  /* The base register to use when accessing the stack.  This may be the
     frame pointer, stack pointer, or a temporary.  The choice of register
     depends on which part of the frame is being accessed and how big the
     frame is.  */
  rtx base;

  /* The offset of BASE from the bottom of the current frame, in bytes.  */
  int base_offset;
} frv_frame_accessor_t;

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */
rtx frv_compare_op0;
rtx frv_compare_op1;

/* Conditional execution support gathered together in one structure.  */
typedef struct
  {
    /* Linked list of insns to add if the conditional execution conversion was
       successful.  Each link points to an EXPR_LIST which points to the pattern
       of the insn to add, and the insn to be inserted before.  */
    rtx added_insns_list;

    /* Identify which registers are safe to allocate for if conversions to
       conditional execution.  We keep the last allocated register in the
       register classes between COND_EXEC statements.  This will mean we allocate
       different registers for each different COND_EXEC group if we can.  This
       might allow the scheduler to intermix two different COND_EXEC sections.  */
    frv_tmp_reg_t tmp_reg;

    /* For nested IFs, identify which CC registers are used outside of setting
       via a compare isnsn, and using via a check insn.  This will allow us to
       know if we can rewrite the register to use a different register that will
       be paired with the CR register controlling the nested IF-THEN blocks.  */
    HARD_REG_SET nested_cc_ok_rewrite;

    /* Temporary registers allocated to hold constants during conditional
       execution.  */
    rtx scratch_regs[FIRST_PSEUDO_REGISTER];

    /* Current number of temp registers available.  */
    int cur_scratch_regs;

    /* Number of nested conditional execution blocks.  */
    int num_nested_cond_exec;

    /* Map of insns that set up constants in scratch registers.  */
    bitmap scratch_insns_bitmap;

    /* Conditional execution test register (CC0..CC7).  */
    rtx cr_reg;

    /* Conditional execution compare register that is paired with cr_reg, so that
       nested compares can be done.  The csubcc and caddcc instructions don't
       have enough bits to specify both a CC register to be set and a CR register
       to do the test on, so the same bit number is used for both.  Needless to
       say, this is rather inconvenient for GCC.  */
    rtx nested_cc_reg;

    /* Extra CR registers used for &&, ||.  */
    rtx extra_int_cr;
    rtx extra_fp_cr;

    /* Previous CR used in nested if, to make sure we are dealing with the same
       nested if as the previous statement.  */
    rtx last_nested_if_cr;
  }
frv_ifcvt_t;

static /* GTY(()) */ frv_ifcvt_t frv_ifcvt;

/* Map register number to smallest register class.  */
enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];

/* Map class letter into register class.  */
enum reg_class reg_class_from_letter[256];

/* Cached value of frv_stack_info.  */
static frv_stack_t *frv_stack_cache = (frv_stack_t *)0;

/* -mbranch-cost= support */
const char *frv_branch_cost_string;
int frv_branch_cost_int = DEFAULT_BRANCH_COST;

/* -mcpu= support */
const char *frv_cpu_string;		/* -mcpu= option */
frv_cpu_t frv_cpu_type = CPU_TYPE;	/* value of -mcpu= */

/* -mcond-exec-insns= support */
const char *frv_condexec_insns_str;		 /* -mcond-exec-insns= option */
int frv_condexec_insns = DEFAULT_CONDEXEC_INSNS; /* value of -mcond-exec-insns*/

/* -mcond-exec-temps= support */
const char *frv_condexec_temps_str;		 /* -mcond-exec-temps= option */
int frv_condexec_temps = DEFAULT_CONDEXEC_TEMPS; /* value of -mcond-exec-temps*/

/* -msched-lookahead=n */
const char *frv_sched_lookahead_str;	 /* -msched-lookahead=n */
int frv_sched_lookahead = 4;		 /* -msched-lookahead=n */

/* Forward references */
static int frv_default_flags_for_cpu		(void);
static int frv_string_begins_with		(tree, const char *);
static FRV_INLINE int const_small_data_p	(rtx);
static FRV_INLINE int plus_small_data_p		(rtx, rtx);
static void frv_print_operand_memory_reference_reg
						(FILE *, rtx);
static void frv_print_operand_memory_reference	(FILE *, rtx, int);
static int frv_print_operand_jump_hint		(rtx);
static FRV_INLINE int frv_regno_ok_for_base_p	(int, int);
static rtx single_set_pattern			(rtx);
static int frv_function_contains_far_jump	(void);
static rtx frv_alloc_temp_reg			(frv_tmp_reg_t *,
						 enum reg_class,
						 enum machine_mode,
						 int, int);
static rtx frv_frame_offset_rtx			(int);
static rtx frv_frame_mem			(enum machine_mode, rtx, int);
static rtx frv_dwarf_store			(rtx, int);
static void frv_frame_insn			(rtx, rtx);
static void frv_frame_access			(frv_frame_accessor_t*,
						 rtx, int);
static void frv_frame_access_multi		(frv_frame_accessor_t*,
						 frv_stack_t *, int);
static void frv_frame_access_standard_regs	(enum frv_stack_op,
						 frv_stack_t *);
static struct machine_function *frv_init_machine_status		(void);
static int frv_legitimate_memory_operand	(rtx, enum machine_mode, int);
static rtx frv_int_to_acc			(enum insn_code, int, rtx);
static enum machine_mode frv_matching_accg_mode	(enum machine_mode);
static rtx frv_read_argument			(tree *);
static int frv_check_constant_argument		(enum insn_code, int, rtx);
static rtx frv_legitimize_target		(enum insn_code, rtx);
static rtx frv_legitimize_argument		(enum insn_code, int, rtx);
static rtx frv_expand_set_builtin		(enum insn_code, tree, rtx);
static rtx frv_expand_unop_builtin		(enum insn_code, tree, rtx);
static rtx frv_expand_binop_builtin		(enum insn_code, tree, rtx);
static rtx frv_expand_cut_builtin		(enum insn_code, tree, rtx);
static rtx frv_expand_binopimm_builtin		(enum insn_code, tree, rtx);
static rtx frv_expand_voidbinop_builtin		(enum insn_code, tree);
static rtx frv_expand_voidtriop_builtin		(enum insn_code, tree);
static rtx frv_expand_voidaccop_builtin		(enum insn_code, tree);
static rtx frv_expand_mclracc_builtin		(tree);
static rtx frv_expand_mrdacc_builtin		(enum insn_code, tree);
static rtx frv_expand_mwtacc_builtin		(enum insn_code, tree);
static rtx frv_expand_noargs_builtin		(enum insn_code);
static rtx frv_emit_comparison			(enum rtx_code, rtx, rtx);
static int frv_clear_registers_used		(rtx *, void *);
static void frv_ifcvt_add_insn			(rtx, rtx, int);
static rtx frv_ifcvt_rewrite_mem		(rtx, enum machine_mode, rtx);
static rtx frv_ifcvt_load_value			(rtx, rtx);
static void frv_registers_update		(rtx, unsigned char [],
						 int [], int *, int);
static int frv_registers_used_p			(rtx, unsigned char [], int);
static int frv_registers_set_p			(rtx, unsigned char [], int);
static int frv_issue_rate			(void);
static int frv_use_dfa_pipeline_interface	(void);
static void frv_pack_insns			(void);
static void frv_function_prologue		(FILE *, HOST_WIDE_INT);
static void frv_function_epilogue		(FILE *, HOST_WIDE_INT);
static bool frv_assemble_integer		(rtx, unsigned, int);
static void frv_init_builtins			(void);
static rtx frv_expand_builtin			(tree, rtx, rtx, enum machine_mode, int);
static void frv_init_libfuncs			(void);
static bool frv_in_small_data_p			(tree);
static void frv_asm_output_mi_thunk
  (FILE *, tree, HOST_WIDE_INT, HOST_WIDE_INT, tree);
static bool frv_rtx_costs			(rtx, int, int, int*);
static void frv_asm_out_constructor		(rtx, int);
static void frv_asm_out_destructor		(rtx, int);

/* Initialize the GCC target structure.  */
#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE frv_function_prologue
#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE frv_function_epilogue
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER frv_assemble_integer
#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS frv_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN frv_expand_builtin
#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS frv_init_libfuncs
#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P frv_in_small_data_p
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS frv_rtx_costs
#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR frv_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR frv_asm_out_destructor

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK frv_asm_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE frv_issue_rate
#undef  TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE frv_use_dfa_pipeline_interface

struct gcc_target targetm = TARGET_INITIALIZER;

/* Given a CONST, return true if the symbol_ref points to small data.  */

static FRV_INLINE int
const_small_data_p (rtx x)
{
  rtx x0, x1;

  if (GET_CODE (XEXP (x, 0)) != PLUS)
    return FALSE;

  x0 = XEXP (XEXP (x, 0), 0);
  if (GET_CODE (x0) != SYMBOL_REF || !SYMBOL_REF_SMALL_P (x0))
    return FALSE;

  x1 = XEXP (XEXP (x, 0), 1);
  if (GET_CODE (x1) != CONST_INT
      || !IN_RANGE_P (INTVAL (x1), -2048, 2047))
    return FALSE;

  return TRUE;
}

/* Given a PLUS, return true if this is a small data reference.  */

static FRV_INLINE int
plus_small_data_p (rtx op0, rtx op1)
{
  if (GET_MODE (op0) == SImode
      && GET_CODE (op0) == REG
      && REGNO (op0) == SDA_BASE_REG)
    {
      if (GET_CODE (op1) == SYMBOL_REF)
	return SYMBOL_REF_SMALL_P (op1);

      if (GET_CODE (op1) == CONST)
	return const_small_data_p (op1);
    }

  return FALSE;
}


static int
frv_default_flags_for_cpu (void)
{
  switch (frv_cpu_type)
    {
    case FRV_CPU_GENERIC:
      return MASK_DEFAULT_FRV;

    case FRV_CPU_FR500:
    case FRV_CPU_TOMCAT:
      return MASK_DEFAULT_FR500;

    case FRV_CPU_FR400:
      return MASK_DEFAULT_FR400;

    case FRV_CPU_FR300:
    case FRV_CPU_SIMPLE:
      return MASK_DEFAULT_SIMPLE;
    }
  abort ();
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
frv_override_options (void)
{
  int regno, i;

  /* Set the cpu type.  */
  if (frv_cpu_string)
    {
      if (strcmp (frv_cpu_string, "simple") == 0)
	frv_cpu_type = FRV_CPU_SIMPLE;

      else if (strcmp (frv_cpu_string, "tomcat") == 0)
	frv_cpu_type = FRV_CPU_TOMCAT;

      else if (strncmp (frv_cpu_string, "fr", sizeof ("fr")-1) != 0)
	error ("Unknown cpu: -mcpu=%s", frv_cpu_string);

      else
	{
	  const char *p = frv_cpu_string + sizeof ("fr") - 1;
	  if (strcmp (p, "500") == 0)
	    frv_cpu_type = FRV_CPU_FR500;

	  else if (strcmp (p, "400") == 0)
	    frv_cpu_type = FRV_CPU_FR400;

	  else if (strcmp (p, "300") == 0)
	    frv_cpu_type = FRV_CPU_FR300;

	  else if (strcmp (p, "v") == 0)
	    frv_cpu_type = FRV_CPU_GENERIC;

	  else
	    error ("Unknown cpu: -mcpu=%s", frv_cpu_string);
	}
    }

  target_flags |= (frv_default_flags_for_cpu () & ~target_flags_explicit);

  /* -mlibrary-pic sets -fPIC and -G0 and also suppresses warnings from the
     linker about linking pic and non-pic code.  */
  if (TARGET_LIBPIC)
    {
      if (!flag_pic)		/* -fPIC */
	flag_pic = 2;

      if (! g_switch_set)	/* -G0 */
	{
	  g_switch_set = 1;
	  g_switch_value = 0;
	}
    }

  /* Both -fpic and -gdwarf want to use .previous and the assembler only keeps
     one level.  */
  if (write_symbols == DWARF_DEBUG && flag_pic)
    error ("-fpic and -gdwarf are incompatible (-fpic and -g/-gdwarf-2 are fine)");

  /* Change the branch cost value.  */
  if (frv_branch_cost_string)
    frv_branch_cost_int = atoi (frv_branch_cost_string);

  /* Change the # of insns to be converted to conditional execution.  */
  if (frv_condexec_insns_str)
    frv_condexec_insns = atoi (frv_condexec_insns_str);

  /* Change # of temporary registers used to hold integer constants.  */
  if (frv_condexec_temps_str)
    frv_condexec_temps = atoi (frv_condexec_temps_str);

  /* Change scheduling look ahead.  */
  if (frv_sched_lookahead_str)
    frv_sched_lookahead = atoi (frv_sched_lookahead_str);

  /* A C expression whose value is a register class containing hard
     register REGNO.  In general there is more than one such class;
     choose a class which is "minimal", meaning that no smaller class
     also contains the register.  */

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      enum reg_class class;

      if (GPR_P (regno))
	{
	  int gpr_reg = regno - GPR_FIRST;
	  if ((gpr_reg & 3) == 0)
	    class = QUAD_REGS;

	  else if ((gpr_reg & 1) == 0)
	    class = EVEN_REGS;

	  else
	    class = GPR_REGS;
	}

      else if (FPR_P (regno))
	{
	  int fpr_reg = regno - GPR_FIRST;
	  if ((fpr_reg & 3) == 0)
	    class = QUAD_FPR_REGS;

	  else if ((fpr_reg & 1) == 0)
	    class = FEVEN_REGS;

	  else
	    class = FPR_REGS;
	}

      else if (regno == LR_REGNO)
	class = LR_REG;

      else if (regno == LCR_REGNO)
	class = LCR_REG;

      else if (ICC_P (regno))
	class = ICC_REGS;

      else if (FCC_P (regno))
	class = FCC_REGS;

      else if (ICR_P (regno))
	class = ICR_REGS;

      else if (FCR_P (regno))
	class = FCR_REGS;

      else if (ACC_P (regno))
	{
	  int r = regno - ACC_FIRST;
	  if ((r & 3) == 0)
	    class = QUAD_ACC_REGS;
	  else if ((r & 1) == 0)
	    class = EVEN_ACC_REGS;
	  else
	    class = ACC_REGS;
	}

      else if (ACCG_P (regno))
	class = ACCG_REGS;

      else
	class = NO_REGS;

      regno_reg_class[regno] = class;
    }

  /* Check for small data option */
  if (!g_switch_set)
    g_switch_value = SDATA_DEFAULT_SIZE;

  /* A C expression which defines the machine-dependent operand
     constraint letters for register classes.  If CHAR is such a
     letter, the value should be the register class corresponding to
     it.  Otherwise, the value should be `NO_REGS'.  The register
     letter `r', corresponding to class `GENERAL_REGS', will not be
     passed to this macro; you do not need to handle it.

     The following letters are unavailable, due to being used as
     constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

  for (i = 0; i < 256; i++)
    reg_class_from_letter[i] = NO_REGS;

  reg_class_from_letter['a'] = ACC_REGS;
  reg_class_from_letter['b'] = EVEN_ACC_REGS;
  reg_class_from_letter['c'] = CC_REGS;
  reg_class_from_letter['d'] = GPR_REGS;
  reg_class_from_letter['e'] = EVEN_REGS;
  reg_class_from_letter['f'] = FPR_REGS;
  reg_class_from_letter['h'] = FEVEN_REGS;
  reg_class_from_letter['l'] = LR_REG;
  reg_class_from_letter['q'] = QUAD_REGS;
  reg_class_from_letter['t'] = ICC_REGS;
  reg_class_from_letter['u'] = FCC_REGS;
  reg_class_from_letter['v'] = ICR_REGS;
  reg_class_from_letter['w'] = FCR_REGS;
  reg_class_from_letter['x'] = QUAD_FPR_REGS;
  reg_class_from_letter['y'] = LCR_REG;
  reg_class_from_letter['z'] = SPR_REGS;
  reg_class_from_letter['A'] = QUAD_ACC_REGS;
  reg_class_from_letter['B'] = ACCG_REGS;
  reg_class_from_letter['C'] = CR_REGS;

  /* There is no single unaligned SI op for PIC code.  Sometimes we
     need to use ".4byte" and sometimes we need to use ".picptr".
     See frv_assemble_integer for details.  */
  if (flag_pic)
    targetm.asm_out.unaligned_op.si = 0;

  init_machine_status = frv_init_machine_status;
}


/* Some machines may desire to change what optimizations are performed for
   various optimization levels.  This macro, if defined, is executed once just
   after the optimization level is determined and before the remainder of the
   command options have been parsed.  Values set in this macro are used as the
   default values for the other command line options.

   LEVEL is the optimization level specified; 2 if `-O2' is specified, 1 if
   `-O' is specified, and 0 if neither is specified.

   SIZE is nonzero if `-Os' is specified, 0 otherwise.

   You should not use this macro to change options that are not
   machine-specific.  These should uniformly selected by the same optimization
   level on all supported machines.  Use this macro to enable machbine-specific
   optimizations.

   *Do not examine `write_symbols' in this macro!* The debugging options are
   *not supposed to alter the generated code.  */

/* On the FRV, possibly disable VLIW packing which is done by the 2nd
   scheduling pass at the current time.  */
void
frv_optimization_options (int level, int size ATTRIBUTE_UNUSED)
{
  if (level >= 2)
    {
#ifdef DISABLE_SCHED2
      flag_schedule_insns_after_reload = 0;
#endif
#ifdef ENABLE_RCSP
      flag_rcsp = 1;
#endif
    }
}


/* Return true if NAME (a STRING_CST node) begins with PREFIX.  */

static int
frv_string_begins_with (tree name, const char *prefix)
{
  int prefix_len = strlen (prefix);

  /* Remember: NAME's length includes the null terminator.  */
  return (TREE_STRING_LENGTH (name) > prefix_len
	  && strncmp (TREE_STRING_POINTER (name), prefix, prefix_len) == 0);
}

/* Zero or more C statements that may conditionally modify two variables
   `fixed_regs' and `call_used_regs' (both of type `char []') after they have
   been initialized from the two preceding macros.

   This is necessary in case the fixed or call-clobbered registers depend on
   target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target flags,
   you may indicate this to GCC by using this macro to modify `fixed_regs' and
   `call_used_regs' to 1 for each of the registers in the classes which should
   not be used by GCC.  Also define the macro `REG_CLASS_FROM_LETTER' to return
   `NO_REGS' if it is called with a letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all of the
   insn patterns whose constraints permit this class are controlled by target
   switches, then GCC will automatically avoid using these registers when the
   target switches are opposed to them.)  */

void
frv_conditional_register_usage (void)
{
  int i;

  for (i = GPR_FIRST + NUM_GPRS; i <= GPR_LAST; i++)
    fixed_regs[i] = call_used_regs[i] = 1;

  for (i = FPR_FIRST + NUM_FPRS; i <= FPR_LAST; i++)
    fixed_regs[i] = call_used_regs[i] = 1;

  for (i = ACC_FIRST + NUM_ACCS; i <= ACC_LAST; i++)
    fixed_regs[i] = call_used_regs[i] = 1;

  for (i = ACCG_FIRST + NUM_ACCS; i <= ACCG_LAST; i++)
    fixed_regs[i] = call_used_regs[i] = 1;

  /* Reserve the registers used for conditional execution.  At present, we need
     1 ICC and 1 ICR register.  */
  fixed_regs[ICC_TEMP] = call_used_regs[ICC_TEMP] = 1;
  fixed_regs[ICR_TEMP] = call_used_regs[ICR_TEMP] = 1;

  if (TARGET_FIXED_CC)
    {
      fixed_regs[ICC_FIRST] = call_used_regs[ICC_FIRST] = 1;
      fixed_regs[FCC_FIRST] = call_used_regs[FCC_FIRST] = 1;
      fixed_regs[ICR_FIRST] = call_used_regs[ICR_FIRST] = 1;
      fixed_regs[FCR_FIRST] = call_used_regs[FCR_FIRST] = 1;
    }

#if 0
  /* If -fpic, SDA_BASE_REG is the PIC register.  */
  if (g_switch_value == 0 && !flag_pic)
    fixed_regs[SDA_BASE_REG] = call_used_regs[SDA_BASE_REG] = 0;

  if (!flag_pic)
    fixed_regs[PIC_REGNO] = call_used_regs[PIC_REGNO] = 0;
#endif
}


/*
 * Compute the stack frame layout
 *
 * Register setup:
 * +---------------+-----------------------+-----------------------+
 * |Register       |type                   |caller-save/callee-save|
 * +---------------+-----------------------+-----------------------+
 * |GR0            |Zero register          |        -              |
 * |GR1            |Stack pointer(SP)      |        -              |
 * |GR2            |Frame pointer(FP)      |        -              |
 * |GR3            |Hidden parameter       |        caller save    |
 * |GR4-GR7        |        -              |        caller save    |
 * |GR8-GR13       |Argument register      |        caller save    |
 * |GR14-GR15      |        -              |        caller save    |
 * |GR16-GR31      |        -              |        callee save    |
 * |GR32-GR47      |        -              |        caller save    |
 * |GR48-GR63      |        -              |        callee save    |
 * |FR0-FR15       |        -              |        caller save    |
 * |FR16-FR31      |        -              |        callee save    |
 * |FR32-FR47      |        -              |        caller save    |
 * |FR48-FR63      |        -              |        callee save    |
 * +---------------+-----------------------+-----------------------+
 *
 * Stack frame setup:
 * Low
 *     SP-> |-----------------------------------|
 *	    |         Argument area		|
 *	    |-----------------------------------|
 *	    |	 Register save area		|
 *	    |-----------------------------------|
 *	    |	Local variable save area	|
 *     FP-> |-----------------------------------|
 *	    |	    Old FP			|
 *	    |-----------------------------------|
 *	    |    Hidden parameter save area     |
 *	    |-----------------------------------|
 *	    | Return address(LR) storage area   |
 *	    |-----------------------------------|
 *	    |     Padding for alignment         |
 *	    |-----------------------------------|
 *	    |     Register argument area	|
 * OLD SP-> |-----------------------------------|
 *          |       Parameter area		|
 *          |-----------------------------------|
 * High
 *
 * Argument area/Parameter area:
 *
 * When a function is called, this area is used for argument transfer.  When
 * the argument is set up by the caller function, this area is referred to as
 * the argument area.  When the argument is referenced by the callee function,
 * this area is referred to as the parameter area.  The area is allocated when
 * all arguments cannot be placed on the argument register at the time of
 * argument transfer.
 *
 * Register save area:
 *
 * This is a register save area that must be guaranteed for the caller
 * function.  This area is not secured when the register save operation is not
 * needed.
 *
 * Local variable save area:
 *
 * This is the area for local variables and temporary variables.
 *
 * Old FP:
 *
 * This area stores the FP value of the caller function.
 *
 * Hidden parameter save area:
 *
 * This area stores the start address of the return value storage
 * area for a struct/union return function.
 * When a struct/union is used as the return value, the caller
 * function stores the return value storage area start address in
 * register GR3 and passes it to the caller function.
 * The callee function interprets the address stored in the GR3
 * as the return value storage area start address.
 * When register GR3 needs to be saved into memory, the callee
 * function saves it in the hidden parameter save area.  This
 * area is not secured when the save operation is not needed.
 *
 * Return address(LR) storage area:
 *
 * This area saves the LR.  The LR stores the address of a return to the caller
 * function for the purpose of function calling.
 *
 * Argument register area:
 *
 * This area saves the argument register.  This area is not secured when the
 * save operation is not needed.
 *
 * Argument:
 *
 * Arguments, the count of which equals the count of argument registers (6
 * words), are positioned in registers GR8 to GR13 and delivered to the callee
 * function.  When a struct/union return function is called, the return value
 * area address is stored in register GR3.  Arguments not placed in the
 * argument registers will be stored in the stack argument area for transfer
 * purposes.  When an 8-byte type argument is to be delivered using registers,
 * it is divided into two and placed in two registers for transfer.  When
 * argument registers must be saved to memory, the callee function secures an
 * argument register save area in the stack.  In this case, a continuous
 * argument register save area must be established in the parameter area.  The
 * argument register save area must be allocated as needed to cover the size of
 * the argument register to be saved.  If the function has a variable count of
 * arguments, it saves all argument registers in the argument register save
 * area.
 *
 * Argument Extension Format:
 *
 * When an argument is to be stored in the stack, its type is converted to an
 * extended type in accordance with the individual argument type.  The argument
 * is freed by the caller function after the return from the callee function is
 * made.
 *
 * +-----------------------+---------------+------------------------+
 * |    Argument Type      |Extended Type  |Stack Storage Size(byte)|
 * +-----------------------+---------------+------------------------+
 * |char                   |int            |        4		    |
 * |signed char            |int            |        4		    |
 * |unsigned char          |int            |        4		    |
 * |[signed] short int     |int            |        4		    |
 * |unsigned short int     |int            |        4		    |
 * |[signed] int           |No extension   |        4		    |
 * |unsigned int           |No extension   |        4		    |
 * |[signed] long int      |No extension   |        4		    |
 * |unsigned long int      |No extension   |        4		    |
 * |[signed] long long int |No extension   |        8		    |
 * |unsigned long long int |No extension   |        8		    |
 * |float                  |double         |        8		    |
 * |double                 |No extension   |        8		    |
 * |long double            |No extension   |        8		    |
 * |pointer                |No extension   |        4		    |
 * |struct/union           |-              |        4 (*1)	    |
 * +-----------------------+---------------+------------------------+
 *
 * When a struct/union is to be delivered as an argument, the caller copies it
 * to the local variable area and delivers the address of that area.
 *
 * Return Value:
 *
 * +-------------------------------+----------------------+
 * |Return Value Type              |Return Value Interface|
 * +-------------------------------+----------------------+
 * |void                           |None                  |
 * |[signed|unsigned] char         |GR8                   |
 * |[signed|unsigned] short int    |GR8                   |
 * |[signed|unsigned] int          |GR8                   |
 * |[signed|unsigned] long int     |GR8                   |
 * |pointer                        |GR8                   |
 * |[signed|unsigned] long long int|GR8 & GR9             |
 * |float                          |GR8                   |
 * |double                         |GR8 & GR9             |
 * |long double                    |GR8 & GR9             |
 * |struct/union                   |(*1)                  |
 * +-------------------------------+----------------------+
 *
 * When a struct/union is used as the return value, the caller function stores
 * the start address of the return value storage area into GR3 and then passes
 * it to the callee function.  The callee function interprets GR3 as the start
 * address of the return value storage area.  When this address needs to be
 * saved in memory, the callee function secures the hidden parameter save area
 * and saves the address in that area.
 */

frv_stack_t *
frv_stack_info (void)
{
  static frv_stack_t info, zero_info;
  frv_stack_t *info_ptr	= &info;
  tree fndecl		= current_function_decl;
  int varargs_p		= 0;
  tree cur_arg;
  tree next_arg;
  int range;
  int alignment;
  int offset;

  /* If we've already calculated the values and reload is complete,
     just return now.  */
  if (frv_stack_cache)
    return frv_stack_cache;

  /* Zero all fields.  */
  info = zero_info;

  /* Set up the register range information.  */
  info_ptr->regs[STACK_REGS_GPR].name         = "gpr";
  info_ptr->regs[STACK_REGS_GPR].first        = LAST_ARG_REGNUM + 1;
  info_ptr->regs[STACK_REGS_GPR].last         = GPR_LAST;
  info_ptr->regs[STACK_REGS_GPR].dword_p      = TRUE;

  info_ptr->regs[STACK_REGS_FPR].name         = "fpr";
  info_ptr->regs[STACK_REGS_FPR].first        = FPR_FIRST;
  info_ptr->regs[STACK_REGS_FPR].last         = FPR_LAST;
  info_ptr->regs[STACK_REGS_FPR].dword_p      = TRUE;

  info_ptr->regs[STACK_REGS_LR].name          = "lr";
  info_ptr->regs[STACK_REGS_LR].first         = LR_REGNO;
  info_ptr->regs[STACK_REGS_LR].last          = LR_REGNO;
  info_ptr->regs[STACK_REGS_LR].special_p     = 1;

  info_ptr->regs[STACK_REGS_CC].name          = "cc";
  info_ptr->regs[STACK_REGS_CC].first         = CC_FIRST;
  info_ptr->regs[STACK_REGS_CC].last          = CC_LAST;
  info_ptr->regs[STACK_REGS_CC].field_p       = TRUE;

  info_ptr->regs[STACK_REGS_LCR].name         = "lcr";
  info_ptr->regs[STACK_REGS_LCR].first        = LCR_REGNO;
  info_ptr->regs[STACK_REGS_LCR].last         = LCR_REGNO;

  info_ptr->regs[STACK_REGS_STDARG].name      = "stdarg";
  info_ptr->regs[STACK_REGS_STDARG].first     = FIRST_ARG_REGNUM;
  info_ptr->regs[STACK_REGS_STDARG].last      = LAST_ARG_REGNUM;
  info_ptr->regs[STACK_REGS_STDARG].dword_p   = 1;
  info_ptr->regs[STACK_REGS_STDARG].special_p = 1;

  info_ptr->regs[STACK_REGS_STRUCT].name      = "struct";
  info_ptr->regs[STACK_REGS_STRUCT].first     = STRUCT_VALUE_REGNUM;
  info_ptr->regs[STACK_REGS_STRUCT].last      = STRUCT_VALUE_REGNUM;
  info_ptr->regs[STACK_REGS_STRUCT].special_p = 1;

  info_ptr->regs[STACK_REGS_FP].name          = "fp";
  info_ptr->regs[STACK_REGS_FP].first         = FRAME_POINTER_REGNUM;
  info_ptr->regs[STACK_REGS_FP].last          = FRAME_POINTER_REGNUM;
  info_ptr->regs[STACK_REGS_FP].special_p     = 1;

  /* Determine if this is a stdarg function.  If so, allocate space to store
     the 6 arguments.  */
  if (cfun->stdarg)
    varargs_p = 1;

  else
    {
      /* Find the last argument, and see if it is __builtin_va_alist.  */
      for (cur_arg = DECL_ARGUMENTS (fndecl); cur_arg != (tree)0; cur_arg = next_arg)
	{
	  next_arg = TREE_CHAIN (cur_arg);
	  if (next_arg == (tree)0)
	    {
	      if (DECL_NAME (cur_arg)
		  && !strcmp (IDENTIFIER_POINTER (DECL_NAME (cur_arg)), "__builtin_va_alist"))
		varargs_p = 1;

	      break;
	    }
	}
    }

  /* Iterate over all of the register ranges.  */
  for (range = 0; range < STACK_REGS_MAX; range++)
    {
      frv_stack_regs_t *reg_ptr = &(info_ptr->regs[range]);
      int first = reg_ptr->first;
      int last = reg_ptr->last;
      int size_1word = 0;
      int size_2words = 0;
      int regno;

      /* Calculate which registers need to be saved & save area size.  */
      switch (range)
	{
	default:
	  for (regno = first; regno <= last; regno++)
	    {
	      if ((regs_ever_live[regno] && !call_used_regs[regno])
		  || (current_function_calls_eh_return
		      && (regno >= FIRST_EH_REGNUM && regno <= LAST_EH_REGNUM))
		  || (flag_pic && cfun->uses_pic_offset_table && regno == PIC_REGNO))
		{
		  info_ptr->save_p[regno] = REG_SAVE_1WORD;
		  size_1word += UNITS_PER_WORD;
		}
	    }
	  break;

	  /* Calculate whether we need to create a frame after everything else
             has been processed.  */
	case STACK_REGS_FP:
	  break;

	case STACK_REGS_LR:
	  if (regs_ever_live[LR_REGNO]
              || profile_flag
              || frame_pointer_needed
              || (flag_pic && cfun->uses_pic_offset_table))
	    {
	      info_ptr->save_p[LR_REGNO] = REG_SAVE_1WORD;
	      size_1word += UNITS_PER_WORD;
	    }
	  break;

	case STACK_REGS_STDARG:
	  if (varargs_p)
	    {
	      /* If this is a stdarg function with a non varardic
		 argument split between registers and the stack,
		 adjust the saved registers downward.  */
	      last -= (ADDR_ALIGN (cfun->pretend_args_size, UNITS_PER_WORD)
		       / UNITS_PER_WORD);

	      for (regno = first; regno <= last; regno++)
		{
		  info_ptr->save_p[regno] = REG_SAVE_1WORD;
		  size_1word += UNITS_PER_WORD;
		}

	      info_ptr->stdarg_size = size_1word;
	    }
	  break;

	case STACK_REGS_STRUCT:
	  if (cfun->returns_struct)
	    {
	      info_ptr->save_p[STRUCT_VALUE_REGNUM] = REG_SAVE_1WORD;
	      size_1word += UNITS_PER_WORD;
	    }
	  break;
	}


      if (size_1word)
	{
	  /* If this is a field, it only takes one word.  */
	  if (reg_ptr->field_p)
	    size_1word = UNITS_PER_WORD;

	  /* Determine which register pairs can be saved together.  */
	  else if (reg_ptr->dword_p && TARGET_DWORD)
	    {
	      for (regno = first; regno < last; regno += 2)
		{
		  if (info_ptr->save_p[regno] && info_ptr->save_p[regno+1])
		    {
		      size_2words += 2 * UNITS_PER_WORD;
		      size_1word -= 2 * UNITS_PER_WORD;
		      info_ptr->save_p[regno] = REG_SAVE_2WORDS;
		      info_ptr->save_p[regno+1] = REG_SAVE_NO_SAVE;
		    }
		}
	    }

	  reg_ptr->size_1word = size_1word;
	  reg_ptr->size_2words = size_2words;

	  if (! reg_ptr->special_p)
	    {
	      info_ptr->regs_size_1word += size_1word;
	      info_ptr->regs_size_2words += size_2words;
	    }
	}
    }

  /* Set up the sizes of each each field in the frame body, making the sizes
     of each be divisible by the size of a dword if dword operations might
     be used, or the size of a word otherwise.  */
  alignment = (TARGET_DWORD? 2 * UNITS_PER_WORD : UNITS_PER_WORD);

  info_ptr->parameter_size = ADDR_ALIGN (cfun->outgoing_args_size, alignment);
  info_ptr->regs_size = ADDR_ALIGN (info_ptr->regs_size_2words
				    + info_ptr->regs_size_1word,
				    alignment);
  info_ptr->vars_size = ADDR_ALIGN (get_frame_size (), alignment);

  info_ptr->pretend_size = cfun->pretend_args_size;

  /* Work out the size of the frame, excluding the header.  Both the frame
     body and register parameter area will be dword-aligned.  */
  info_ptr->total_size
    = (ADDR_ALIGN (info_ptr->parameter_size
		   + info_ptr->regs_size
		   + info_ptr->vars_size,
		   2 * UNITS_PER_WORD)
       + ADDR_ALIGN (info_ptr->pretend_size
		     + info_ptr->stdarg_size,
		     2 * UNITS_PER_WORD));

  /* See if we need to create a frame at all, if so add header area.  */
  if (info_ptr->total_size  > 0
      || info_ptr->regs[STACK_REGS_LR].size_1word > 0
      || info_ptr->regs[STACK_REGS_STRUCT].size_1word > 0)
    {
      offset = info_ptr->parameter_size;
      info_ptr->header_size = 4 * UNITS_PER_WORD;
      info_ptr->total_size += 4 * UNITS_PER_WORD;

      /* Calculate the offsets to save normal register pairs.  */
      for (range = 0; range < STACK_REGS_MAX; range++)
	{
	  frv_stack_regs_t *reg_ptr = &(info_ptr->regs[range]);
	  if (! reg_ptr->special_p)
	    {
	      int first = reg_ptr->first;
	      int last = reg_ptr->last;
	      int regno;

	      for (regno = first; regno <= last; regno++)
		if (info_ptr->save_p[regno] == REG_SAVE_2WORDS
		    && regno != FRAME_POINTER_REGNUM
		    && (regno < FIRST_ARG_REGNUM
			|| regno > LAST_ARG_REGNUM))
		  {
		    info_ptr->reg_offset[regno] = offset;
		    offset += 2 * UNITS_PER_WORD;
		  }
	    }
	}

      /* Calculate the offsets to save normal single registers.  */
      for (range = 0; range < STACK_REGS_MAX; range++)
	{
	  frv_stack_regs_t *reg_ptr = &(info_ptr->regs[range]);
	  if (! reg_ptr->special_p)
	    {
	      int first = reg_ptr->first;
	      int last = reg_ptr->last;
	      int regno;

	      for (regno = first; regno <= last; regno++)
		if (info_ptr->save_p[regno] == REG_SAVE_1WORD
		    && regno != FRAME_POINTER_REGNUM
		    && (regno < FIRST_ARG_REGNUM
			|| regno > LAST_ARG_REGNUM))
		  {
		    info_ptr->reg_offset[regno] = offset;
		    offset += UNITS_PER_WORD;
		  }
	    }
	}

      /* Calculate the offset to save the local variables at.  */
      offset = ADDR_ALIGN (offset, alignment);
      if (info_ptr->vars_size)
	{
	  info_ptr->vars_offset = offset;
	  offset += info_ptr->vars_size;
	}

      /* Align header to a dword-boundary.  */
      offset = ADDR_ALIGN (offset, 2 * UNITS_PER_WORD);

      /* Calculate the offsets in the fixed frame.  */
      info_ptr->save_p[FRAME_POINTER_REGNUM] = REG_SAVE_1WORD;
      info_ptr->reg_offset[FRAME_POINTER_REGNUM] = offset;
      info_ptr->regs[STACK_REGS_FP].size_1word = UNITS_PER_WORD;

      info_ptr->save_p[LR_REGNO] = REG_SAVE_1WORD;
      info_ptr->reg_offset[LR_REGNO] = offset + 2*UNITS_PER_WORD;
      info_ptr->regs[STACK_REGS_LR].size_1word = UNITS_PER_WORD;

      if (cfun->returns_struct)
	{
	  info_ptr->save_p[STRUCT_VALUE_REGNUM] = REG_SAVE_1WORD;
	  info_ptr->reg_offset[STRUCT_VALUE_REGNUM] = offset + UNITS_PER_WORD;
	  info_ptr->regs[STACK_REGS_STRUCT].size_1word = UNITS_PER_WORD;
	}

      /* Calculate the offsets to store the arguments passed in registers
         for stdarg functions.  The register pairs are first and the single
         register if any is last.  The register save area starts on a
         dword-boundary.  */
      if (info_ptr->stdarg_size)
	{
	  int first = info_ptr->regs[STACK_REGS_STDARG].first;
	  int last  = info_ptr->regs[STACK_REGS_STDARG].last;
	  int regno;

	  /* Skip the header.  */
	  offset += 4 * UNITS_PER_WORD;
	  for (regno = first; regno <= last; regno++)
	    {
	      if (info_ptr->save_p[regno] == REG_SAVE_2WORDS)
		{
		  info_ptr->reg_offset[regno] = offset;
		  offset += 2 * UNITS_PER_WORD;
		}
	      else if (info_ptr->save_p[regno] == REG_SAVE_1WORD)
		{
		  info_ptr->reg_offset[regno] = offset;
		  offset += UNITS_PER_WORD;
		}
	    }
	}
    }

  if (reload_completed)
    frv_stack_cache = info_ptr;

  return info_ptr;
}


/* Print the information about the frv stack offsets, etc. when debugging.  */

void
frv_debug_stack (frv_stack_t *info)
{
  int range;

  if (!info)
    info = frv_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  fprintf (stderr, "\ttotal_size\t= %6d\n", info->total_size);
  fprintf (stderr, "\tvars_size\t= %6d\n", info->vars_size);
  fprintf (stderr, "\tparam_size\t= %6d\n", info->parameter_size);
  fprintf (stderr, "\tregs_size\t= %6d, 1w = %3d, 2w = %3d\n",
	   info->regs_size, info->regs_size_1word, info->regs_size_2words);

  fprintf (stderr, "\theader_size\t= %6d\n", info->header_size);
  fprintf (stderr, "\tpretend_size\t= %6d\n", info->pretend_size);
  fprintf (stderr, "\tvars_offset\t= %6d\n", info->vars_offset);
  fprintf (stderr, "\tregs_offset\t= %6d\n", info->regs_offset);

  for (range = 0; range < STACK_REGS_MAX; range++)
    {
      frv_stack_regs_t *regs = &(info->regs[range]);
      if ((regs->size_1word + regs->size_2words) > 0)
	{
	  int first = regs->first;
	  int last  = regs->last;
	  int regno;

	  fprintf (stderr, "\t%s\tsize\t= %6d, 1w = %3d, 2w = %3d, save =",
		   regs->name, regs->size_1word + regs->size_2words,
		   regs->size_1word, regs->size_2words);

	  for (regno = first; regno <= last; regno++)
	    {
	      if (info->save_p[regno] == REG_SAVE_1WORD)
		fprintf (stderr, " %s (%d)", reg_names[regno],
			 info->reg_offset[regno]);

	      else if (info->save_p[regno] == REG_SAVE_2WORDS)
		fprintf (stderr, " %s-%s (%d)", reg_names[regno],
			 reg_names[regno+1], info->reg_offset[regno]);
	    }

	  fputc ('\n', stderr);
	}
    }

  fflush (stderr);
}




/* The following variable value is TRUE if the next output insn should
   finish cpu cycle.  In order words the insn will have packing bit
   (which means absence of asm code suffix `.p' on assembler.  */

static int frv_insn_packing_flag;

/* True if the current function contains a far jump.  */

static int
frv_function_contains_far_jump (void)
{
  rtx insn = get_insns ();
  while (insn != NULL
	 && !(GET_CODE (insn) == JUMP_INSN
	      /* Ignore tablejump patterns.  */
	      && GET_CODE (PATTERN (insn)) != ADDR_VEC
	      && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
	      && get_attr_far_jump (insn) == FAR_JUMP_YES))
    insn = NEXT_INSN (insn);
  return (insn != NULL);
}

/* For the FRV, this function makes sure that a function with far jumps
   will return correctly.  It also does the VLIW packing.  */

static void
frv_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  /* If no frame was created, check whether the function uses a call
     instruction to implement a far jump.  If so, save the link in gr3 and
     replace all returns to LR with returns to GR3.  GR3 is used because it
     is call-clobbered, because is not available to the register allocator,
     and because all functions that take a hidden argument pointer will have
     a stack frame.  */
  if (frv_stack_info ()->total_size == 0 && frv_function_contains_far_jump ())
    {
      rtx insn;

      /* Just to check that the above comment is true.  */
      if (regs_ever_live[GPR_FIRST + 3])
	abort ();

      /* Generate the instruction that saves the link register.  */
      fprintf (file, "\tmovsg lr,gr3\n");

      /* Replace the LR with GR3 in *return_internal patterns.  The insn
	 will now return using jmpl @(gr3,0) rather than bralr.  We cannot
	 simply emit a different assembly directive because bralr and jmpl
	 execute in different units.  */
      for (insn = get_insns(); insn != NULL; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == JUMP_INSN)
	  {
	    rtx pattern = PATTERN (insn);
	    if (GET_CODE (pattern) == PARALLEL
		&& XVECLEN (pattern, 0) >= 2
		&& GET_CODE (XVECEXP (pattern, 0, 0)) == RETURN
		&& GET_CODE (XVECEXP (pattern, 0, 1)) == USE)
	      {
		rtx address = XEXP (XVECEXP (pattern, 0, 1), 0);
		if (GET_CODE (address) == REG && REGNO (address) == LR_REGNO)
		  REGNO (address) = GPR_FIRST + 3;
	      }
	  }
    }

  frv_pack_insns ();
  frv_insn_packing_flag = TRUE;
}


/* Return the next available temporary register in a given class.  */

static rtx
frv_alloc_temp_reg (
     frv_tmp_reg_t *info,	/* which registers are available */
     enum reg_class class,	/* register class desired */
     enum machine_mode mode,	/* mode to allocate register with */
     int mark_as_used,		/* register not available after allocation */
     int no_abort)		/* return NULL instead of aborting */
{
  int regno = info->next_reg[ (int)class ];
  int orig_regno = regno;
  HARD_REG_SET *reg_in_class = &reg_class_contents[ (int)class ];
  int i, nr;

  for (;;)
    {
      if (TEST_HARD_REG_BIT (*reg_in_class, regno)
	  && TEST_HARD_REG_BIT (info->regs, regno))
	  break;

      if (++regno >= FIRST_PSEUDO_REGISTER)
	regno = 0;
      if (regno == orig_regno)
	{
	  if (no_abort)
	    return NULL_RTX;
	  else
	    abort ();
	}
    }

  nr = HARD_REGNO_NREGS (regno, mode);
  info->next_reg[ (int)class ] = regno + nr;

  if (mark_as_used)
    for (i = 0; i < nr; i++)
      CLEAR_HARD_REG_BIT (info->regs, regno+i);

  return gen_rtx_REG (mode, regno);
}


/* Return an rtx with the value OFFSET, which will either be a register or a
   signed 12-bit integer.  It can be used as the second operand in an "add"
   instruction, or as the index in a load or store.

   The function returns a constant rtx if OFFSET is small enough, otherwise
   it loads the constant into register OFFSET_REGNO and returns that.  */
static rtx
frv_frame_offset_rtx (int offset)
{
  rtx offset_rtx = GEN_INT (offset);
  if (IN_RANGE_P (offset, -2048, 2047))
    return offset_rtx;
  else
    {
      rtx reg_rtx = gen_rtx_REG (SImode, OFFSET_REGNO);
      if (IN_RANGE_P (offset, -32768, 32767))
	emit_insn (gen_movsi (reg_rtx, offset_rtx));
      else
	{
	  emit_insn (gen_movsi_high (reg_rtx, offset_rtx));
	  emit_insn (gen_movsi_lo_sum (reg_rtx, offset_rtx));
	}
      return reg_rtx;
    }
}

/* Generate (mem:MODE (plus:Pmode BASE (frv_frame_offset OFFSET)))).  The
   prologue and epilogue uses such expressions to access the stack.  */
static rtx
frv_frame_mem (enum machine_mode mode, rtx base, int offset)
{
  return gen_rtx_MEM (mode, gen_rtx_PLUS (Pmode,
					  base,
					  frv_frame_offset_rtx (offset)));
}

/* Generate a frame-related expression:

	(set REG (mem (plus (sp) (const_int OFFSET)))).

   Such expressions are used in FRAME_RELATED_EXPR notes for more complex
   instructions.  Marking the expressions as frame-related is superfluous if
   the note contains just a single set.  But if the note contains a PARALLEL
   or SEQUENCE that has several sets, each set must be individually marked
   as frame-related.  */
static rtx
frv_dwarf_store (rtx reg, int offset)
{
  rtx set = gen_rtx_SET (VOIDmode,
			 gen_rtx_MEM (GET_MODE (reg),
				      plus_constant (stack_pointer_rtx,
						     offset)),
			 reg);
  RTX_FRAME_RELATED_P (set) = 1;
  return set;
}

/* Emit a frame-related instruction whose pattern is PATTERN.  The
   instruction is the last in a sequence that cumulatively performs the
   operation described by DWARF_PATTERN.  The instruction is marked as
   frame-related and has a REG_FRAME_RELATED_EXPR note containing
   DWARF_PATTERN.  */
static void
frv_frame_insn (rtx pattern, rtx dwarf_pattern)
{
  rtx insn = emit_insn (pattern);
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR,
				      dwarf_pattern,
				      REG_NOTES (insn));
}

/* Emit instructions that transfer REG to or from the memory location (sp +
   STACK_OFFSET).  The register is stored in memory if ACCESSOR->OP is
   FRV_STORE and loaded if it is FRV_LOAD.  Only the prologue uses this
   function to store registers and only the epilogue uses it to load them.

   The caller sets up ACCESSOR so that BASE is equal to (sp + BASE_OFFSET).
   The generated instruction will use BASE as its base register.  BASE may
   simply be the stack pointer, but if several accesses are being made to a
   region far away from the stack pointer, it may be more efficient to set
   up a temporary instead.

   Store instructions will be frame-related and will be annotated with the
   overall effect of the store.  Load instructions will be followed by a
   (use) to prevent later optimizations from zapping them.

   The function takes care of the moves to and from SPRs, using TEMP_REGNO
   as a temporary in such cases.  */
static void
frv_frame_access (frv_frame_accessor_t *accessor, rtx reg, int stack_offset)
{
  enum machine_mode mode = GET_MODE (reg);
  rtx mem = frv_frame_mem (mode,
			   accessor->base,
			   stack_offset - accessor->base_offset);

  if (accessor->op == FRV_LOAD)
    {
      if (SPR_P (REGNO (reg)))
	{
	  rtx temp = gen_rtx_REG (mode, TEMP_REGNO);
	  emit_insn (gen_rtx_SET (VOIDmode, temp, mem));
	  emit_insn (gen_rtx_SET (VOIDmode, reg, temp));
	}
      else
	emit_insn (gen_rtx_SET (VOIDmode, reg, mem));
      emit_insn (gen_rtx_USE (VOIDmode, reg));
    }
  else
    {
      if (SPR_P (REGNO (reg)))
	{
	  rtx temp = gen_rtx_REG (mode, TEMP_REGNO);
	  emit_insn (gen_rtx_SET (VOIDmode, temp, reg));
	  frv_frame_insn (gen_rtx_SET (Pmode, mem, temp),
			  frv_dwarf_store (reg, stack_offset));
	}
      else if (GET_MODE (reg) == DImode)
	{
	  /* For DImode saves, the dwarf2 version needs to be a SEQUENCE
	     with a separate save for each register.  */
	  rtx reg1 = gen_rtx_REG (SImode, REGNO (reg));
	  rtx reg2 = gen_rtx_REG (SImode, REGNO (reg) + 1);
	  rtx set1 = frv_dwarf_store (reg1, stack_offset);
	  rtx set2 = frv_dwarf_store (reg2, stack_offset + 4);
	  frv_frame_insn (gen_rtx_SET (Pmode, mem, reg),
			  gen_rtx_PARALLEL (VOIDmode,
					    gen_rtvec (2, set1, set2)));
	}
      else
	frv_frame_insn (gen_rtx_SET (Pmode, mem, reg),
			frv_dwarf_store (reg, stack_offset));
    }
}

/* A function that uses frv_frame_access to transfer a group of registers to
   or from the stack.  ACCESSOR is passed directly to frv_frame_access, INFO
   is the stack information generated by frv_stack_info, and REG_SET is the
   number of the register set to transfer.  */
static void
frv_frame_access_multi (frv_frame_accessor_t *accessor,
                        frv_stack_t *info,
                        int reg_set)
{
  frv_stack_regs_t *regs_info;
  int regno;

  regs_info = &info->regs[reg_set];
  for (regno = regs_info->first; regno <= regs_info->last; regno++)
    if (info->save_p[regno])
      frv_frame_access (accessor,
			info->save_p[regno] == REG_SAVE_2WORDS
			? gen_rtx_REG (DImode, regno)
			: gen_rtx_REG (SImode, regno),
			info->reg_offset[regno]);
}

/* Save or restore callee-saved registers that are kept outside the frame
   header.  The function saves the registers if OP is FRV_STORE and restores
   them if OP is FRV_LOAD.  INFO is the stack information generated by
   frv_stack_info.  */
static void
frv_frame_access_standard_regs (enum frv_stack_op op, frv_stack_t *info)
{
  frv_frame_accessor_t accessor;

  accessor.op = op;
  accessor.base = stack_pointer_rtx;
  accessor.base_offset = 0;
  frv_frame_access_multi (&accessor, info, STACK_REGS_GPR);
  frv_frame_access_multi (&accessor, info, STACK_REGS_FPR);
  frv_frame_access_multi (&accessor, info, STACK_REGS_LCR);
}


/* Called after register allocation to add any instructions needed for the
   prologue.  Using a prologue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.  */
void
frv_expand_prologue (void)
{
  frv_stack_t *info = frv_stack_info ();
  rtx sp = stack_pointer_rtx;
  rtx fp = frame_pointer_rtx;
  frv_frame_accessor_t accessor;

  if (TARGET_DEBUG_STACK)
    frv_debug_stack (info);

  if (info->total_size == 0)
    return;

  /* We're interested in three areas of the frame here:

         A: the register save area
	 B: the old FP
	 C: the header after B

     If the frame pointer isn't used, we'll have to set up A, B and C
     using the stack pointer.  If the frame pointer is used, we'll access
     them as follows:

         A: set up using sp
	 B: set up using sp or a temporary (see below)
	 C: set up using fp

     We set up B using the stack pointer if the frame is small enough.
     Otherwise, it's more efficient to copy the old stack pointer into a
     temporary and use that.

     Note that it's important to make sure the prologue and epilogue use the
     same registers to access A and C, since doing otherwise will confuse
     the aliasing code.  */

  /* Set up ACCESSOR for accessing region B above.  If the frame pointer
     isn't used, the same method will serve for C.  */
  accessor.op = FRV_STORE;
  if (frame_pointer_needed && info->total_size > 2048)
    {
      rtx insn;

      accessor.base = gen_rtx_REG (Pmode, OLD_SP_REGNO);
      accessor.base_offset = info->total_size;
      insn = emit_insn (gen_movsi (accessor.base, sp));
    }
  else
    {
      accessor.base = stack_pointer_rtx;
      accessor.base_offset = 0;
    }

  /* Allocate the stack space.  */
  {
    rtx asm_offset = frv_frame_offset_rtx (-info->total_size);
    rtx dwarf_offset = GEN_INT (-info->total_size);

    frv_frame_insn (gen_stack_adjust (sp, sp, asm_offset),
		    gen_rtx_SET (Pmode,
				 sp,
				 gen_rtx_PLUS (Pmode, sp, dwarf_offset)));
  }

  /* If the frame pointer is needed, store the old one at (sp + FP_OFFSET)
     and point the new one to that location.  */
  if (frame_pointer_needed)
    {
      int fp_offset = info->reg_offset[FRAME_POINTER_REGNUM];

      /* ASM_SRC and DWARF_SRC both point to the frame header.  ASM_SRC is
	 based on ACCESSOR.BASE but DWARF_SRC is always based on the stack
	 pointer.  */
      rtx asm_src = plus_constant (accessor.base,
				   fp_offset - accessor.base_offset);
      rtx dwarf_src = plus_constant (sp, fp_offset);

      /* Store the old frame pointer at (sp + FP_OFFSET).  */
      frv_frame_access (&accessor, fp, fp_offset);

      /* Set up the new frame pointer.  */
      frv_frame_insn (gen_rtx_SET (VOIDmode, fp, asm_src),
		      gen_rtx_SET (VOIDmode, fp, dwarf_src));

      /* Access region C from the frame pointer.  */
      accessor.base = fp;
      accessor.base_offset = fp_offset;
    }

  /* Set up region C.  */
  frv_frame_access_multi (&accessor, info, STACK_REGS_STRUCT);
  frv_frame_access_multi (&accessor, info, STACK_REGS_LR);
  frv_frame_access_multi (&accessor, info, STACK_REGS_STDARG);

  /* Set up region A.  */
  frv_frame_access_standard_regs (FRV_STORE, info);

  /* If this is a varargs/stdarg function, issue a blockage to prevent the
     scheduler from moving loads before the stores saving the registers.  */
  if (info->stdarg_size > 0)
    emit_insn (gen_blockage ());

  /* Set up pic register/small data register for this function.  */
  if (flag_pic && cfun->uses_pic_offset_table)
    emit_insn (gen_pic_prologue (gen_rtx_REG (Pmode, PIC_REGNO),
				 gen_rtx_REG (Pmode, LR_REGNO),
				 gen_rtx_REG (SImode, OFFSET_REGNO)));
}


/* Under frv, all of the work is done via frv_expand_epilogue, but
   this function provides a convenient place to do cleanup.  */

static void
frv_function_epilogue (FILE *file ATTRIBUTE_UNUSED,
                       HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  frv_stack_cache = (frv_stack_t *)0;

  /* Zap last used registers for conditional execution.  */
  memset (&frv_ifcvt.tmp_reg, 0, sizeof (frv_ifcvt.tmp_reg));

  /* Release the bitmap of created insns.  */
  BITMAP_XFREE (frv_ifcvt.scratch_insns_bitmap);
}


/* Called after register allocation to add any instructions needed for the
   epilogue.  Using an epilogue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.

   If SIBCALL_P is true, the final branch back to the calling function is
   omitted, and is used for sibling call (aka tail call) sites.  For sibcalls,
   we must not clobber any arguments used for parameter passing or any stack
   slots for arguments passed to the current function.  */

void
frv_expand_epilogue (int sibcall_p)
{
  frv_stack_t *info = frv_stack_info ();
  rtx fp = frame_pointer_rtx;
  rtx sp = stack_pointer_rtx;
  rtx return_addr;
  int fp_offset;

  fp_offset = info->reg_offset[FRAME_POINTER_REGNUM];

  /* Restore the stack pointer to its original value if alloca or the like
     is used.  */
  if (! current_function_sp_is_unchanging)
    emit_insn (gen_addsi3 (sp, fp, frv_frame_offset_rtx (-fp_offset)));

  /* Restore the callee-saved registers that were used in this function.  */
  frv_frame_access_standard_regs (FRV_LOAD, info);

  /* Set RETURN_ADDR to the address we should return to.  Set it to NULL if
     no return instruction should be emitted.  */
  if (sibcall_p)
    return_addr = 0;
  else if (info->save_p[LR_REGNO])
    {
      int lr_offset;
      rtx mem;

      /* Use the same method to access the link register's slot as we did in
	 the prologue.  In other words, use the frame pointer if available,
	 otherwise use the stack pointer.

	 LR_OFFSET is the offset of the link register's slot from the start
	 of the frame and MEM is a memory rtx for it.  */
      lr_offset = info->reg_offset[LR_REGNO];
      if (frame_pointer_needed)
	mem = frv_frame_mem (Pmode, fp, lr_offset - fp_offset);
      else
	mem = frv_frame_mem (Pmode, sp, lr_offset);

      /* Load the old link register into a GPR.  */
      return_addr = gen_rtx_REG (Pmode, TEMP_REGNO);
      emit_insn (gen_rtx_SET (VOIDmode, return_addr, mem));
    }
  else
    return_addr = gen_rtx_REG (Pmode, LR_REGNO);

  /* Restore the old frame pointer.  Emit a USE afterwards to make sure
     the load is preserved.  */
  if (frame_pointer_needed)
    {
      emit_insn (gen_rtx_SET (VOIDmode, fp, gen_rtx_MEM (Pmode, fp)));
      emit_insn (gen_rtx_USE (VOIDmode, fp));
    }

  /* Deallocate the stack frame.  */
  if (info->total_size != 0)
    {
      rtx offset = frv_frame_offset_rtx (info->total_size);
      emit_insn (gen_stack_adjust (sp, sp, offset));
    }

  /* If this function uses eh_return, add the final stack adjustment now.  */
  if (current_function_calls_eh_return)
    emit_insn (gen_stack_adjust (sp, sp, EH_RETURN_STACKADJ_RTX));

  if (return_addr)
    emit_jump_insn (gen_epilogue_return (return_addr));
}


/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the adjusted
   first argument.  This macro is responsible for emitting all of the code for
   a thunk function; `FUNCTION_PROLOGUE' and `FUNCTION_EPILOGUE' are not
   invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.

   If you do not define this macro, the target-independent code in the C++
   frontend will generate a less efficient heavyweight thunk that calls
   FUNCTION instead of jumping to it.  The generic approach does not support
   varargs.  */

static void
frv_asm_output_mi_thunk (FILE *file,
                         tree thunk_fndecl ATTRIBUTE_UNUSED,
                         HOST_WIDE_INT delta,
                         HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
                         tree function)
{
  const char *name_func = XSTR (XEXP (DECL_RTL (function), 0), 0);
  const char *name_arg0 = reg_names[FIRST_ARG_REGNUM];
  const char *name_jmp = reg_names[JUMP_REGNO];
  const char *parallel = ((PACKING_FLAG_USED_P ()) ? ".p" : "");

  /* Do the add using an addi if possible.  */
  if (IN_RANGE_P (delta, -2048, 2047))
    fprintf (file, "\taddi %s,#%d,%s\n", name_arg0, (int) delta, name_arg0);
  else
    {
      const char *const name_add = reg_names[TEMP_REGNO];
      fprintf (file, "\tsethi%s #hi(" HOST_WIDE_INT_PRINT_DEC "),%s\n",
	       parallel, delta, name_add);
      fprintf (file, "\tsetlo #lo(" HOST_WIDE_INT_PRINT_DEC "),%s\n",
	       delta, name_add);
      fprintf (file, "\tadd %s,%s,%s\n", name_add, name_arg0, name_arg0);
    }

  if (!flag_pic)
    {
      fprintf (file, "\tsethi%s #hi(", parallel);
      assemble_name (file, name_func);
      fprintf (file, "),%s\n", name_jmp);

      fprintf (file, "\tsetlo #lo(");
      assemble_name (file, name_func);
      fprintf (file, "),%s\n", name_jmp);
    }
  else
    {
      /* Use JUMP_REGNO as a temporary PIC register.  */
      const char *name_lr = reg_names[LR_REGNO];
      const char *name_gppic = name_jmp;
      const char *name_tmp = reg_names[TEMP_REGNO];

      fprintf (file, "\tmovsg %s,%s\n", name_lr, name_tmp);
      fprintf (file, "\tcall 1f\n");
      fprintf (file, "1:\tmovsg %s,%s\n", name_lr, name_gppic);
      fprintf (file, "\tmovgs %s,%s\n", name_tmp, name_lr);
      fprintf (file, "\tsethi%s #gprelhi(1b),%s\n", parallel, name_tmp);
      fprintf (file, "\tsetlo #gprello(1b),%s\n", name_tmp);
      fprintf (file, "\tsub %s,%s,%s\n", name_gppic, name_tmp, name_gppic);

      fprintf (file, "\tsethi%s #gprelhi(", parallel);
      assemble_name (file, name_func);
      fprintf (file, "),%s\n", name_tmp);

      fprintf (file, "\tsetlo #gprello(");
      assemble_name (file, name_func);
      fprintf (file, "),%s\n", name_tmp);

      fprintf (file, "\tadd %s,%s,%s\n", name_gppic, name_tmp, name_jmp);
    }

  /* Jump to the function address.  */
  fprintf (file, "\tjmpl @(%s,%s)\n", name_jmp, reg_names[GPR_FIRST+0]);
}


/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */

/* On frv, create a frame whenever we need to create stack.  */

int
frv_frame_pointer_required (void)
{
  if (! current_function_is_leaf)
    return TRUE;

  if (get_frame_size () != 0)
    return TRUE;

  if (cfun->stdarg)
    return TRUE;

  if (!current_function_sp_is_unchanging)
    return TRUE;

  if (flag_pic && cfun->uses_pic_offset_table)
    return TRUE;

  if (profile_flag)
    return TRUE;

  if (cfun->machine->frame_needed)
    return TRUE;

  return FALSE;
}


/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */

/* See frv_stack_info for more details on the frv stack frame.  */

int
frv_initial_elimination_offset (int from, int to)
{
  frv_stack_t *info = frv_stack_info ();
  int ret = 0;

  if (to == STACK_POINTER_REGNUM && from == ARG_POINTER_REGNUM)
    ret = info->total_size - info->pretend_size;

  else if (to == STACK_POINTER_REGNUM && from == FRAME_POINTER_REGNUM)
    ret = info->reg_offset[FRAME_POINTER_REGNUM];

  else if (to == FRAME_POINTER_REGNUM && from == ARG_POINTER_REGNUM)
    ret = (info->total_size
	   - info->reg_offset[FRAME_POINTER_REGNUM]
	   - info->pretend_size);

  else
    abort ();

  if (TARGET_DEBUG_STACK)
    fprintf (stderr, "Eliminate %s to %s by adding %d\n",
	     reg_names [from], reg_names[to], ret);

  return ret;
}


/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

void
frv_setup_incoming_varargs (CUMULATIVE_ARGS *cum,
                            enum machine_mode mode,
                            tree type ATTRIBUTE_UNUSED,
                            int *pretend_size,
                            int second_time)
{
  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "setup_vararg: words = %2d, mode = %4s, pretend_size = %d, second_time = %d\n",
	     *cum, GET_MODE_NAME (mode), *pretend_size, second_time);
}


/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */

rtx
frv_expand_builtin_saveregs (void)
{
  int offset = UNITS_PER_WORD * FRV_NUM_ARG_REGS;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "expand_builtin_saveregs: offset from ap = %d\n",
	     offset);

  return gen_rtx (PLUS, Pmode, virtual_incoming_args_rtx, GEN_INT (- offset));
}


/* Expand __builtin_va_start to do the va_start macro.  */

void
frv_expand_builtin_va_start (tree valist, rtx nextarg)
{
  tree t;
  int num = cfun->args_info - FIRST_ARG_REGNUM - FRV_NUM_ARG_REGS;

  nextarg = gen_rtx_PLUS (Pmode, virtual_incoming_args_rtx,
			  GEN_INT (UNITS_PER_WORD * num));

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "va_start: args_info = %d, num = %d\n",
	       cfun->args_info, num);

      debug_rtx (nextarg);
    }

  t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
	     make_tree (ptr_type_node, nextarg));
  TREE_SIDE_EFFECTS (t) = 1;

  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}


/* Expand __builtin_va_arg to do the va_arg macro.  */

rtx
frv_expand_builtin_va_arg (tree valist, tree type)
{
  rtx addr;
  rtx mem;
  rtx reg;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "va_arg:\n");
      debug_tree (type);
    }

  if (! AGGREGATE_TYPE_P (type))
    return std_expand_builtin_va_arg (valist, type);

  addr = std_expand_builtin_va_arg (valist, ptr_type_node);
  mem  = gen_rtx_MEM (Pmode, addr);
  reg  = gen_reg_rtx (Pmode);

  set_mem_alias_set (mem, get_varargs_alias_set ());
  emit_move_insn (reg, mem);

  return reg;
}


/* Expand a block move operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

/* Maximum number of loads to do before doing the stores */
#ifndef MAX_MOVE_REG
#define MAX_MOVE_REG 4
#endif

/* Maximum number of total loads to do.  */
#ifndef TOTAL_MOVE_REG
#define TOTAL_MOVE_REG 8
#endif

int
frv_expand_block_move (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx orig_src	= operands[1];
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
  int bytes;
  int offset;
  int num_reg;
  int i;
  rtx src_reg;
  rtx dest_reg;
  rtx src_addr;
  rtx dest_addr;
  rtx src_mem;
  rtx dest_mem;
  rtx tmp_reg;
  rtx stores[MAX_MOVE_REG];
  int move_bytes;
  enum machine_mode mode;

  /* If this is not a fixed size move, just call memcpy.  */
  if (! constp)
    return FALSE;

  /* If this is not a fixed size alignment, abort.  */
  if (GET_CODE (align_rtx) != CONST_INT)
    abort ();

  align = INTVAL (align_rtx);

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return TRUE;

  /* Don't support real large moves.  */
  if (bytes > TOTAL_MOVE_REG*align)
    return FALSE;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (orig_dest, 0));
  src_reg  = copy_addr_to_reg (XEXP (orig_src,  0));

  num_reg = offset = 0;
  for ( ; bytes > 0; (bytes -= move_bytes), (offset += move_bytes))
    {
      /* Calculate the correct offset for src/dest.  */
      if (offset == 0)
	{
	  src_addr  = src_reg;
	  dest_addr = dest_reg;
	}
      else
	{
	  src_addr = plus_constant (src_reg, offset);
	  dest_addr = plus_constant (dest_reg, offset);
	}

      /* Generate the appropriate load and store, saving the stores
	 for later.  */
      if (bytes >= 4 && align >= 4)
	mode = SImode;
      else if (bytes >= 2 && align >= 2)
	mode = HImode;
      else
	mode = QImode;

      move_bytes = GET_MODE_SIZE (mode);
      tmp_reg = gen_reg_rtx (mode);
      src_mem = change_address (orig_src, mode, src_addr);
      dest_mem = change_address (orig_dest, mode, dest_addr);
      emit_insn (gen_rtx_SET (VOIDmode, tmp_reg, src_mem));
      stores[num_reg++] = gen_rtx_SET (VOIDmode, dest_mem, tmp_reg);

      if (num_reg >= MAX_MOVE_REG)
	{
	  for (i = 0; i < num_reg; i++)
	    emit_insn (stores[i]);
	  num_reg = 0;
	}
    }

  for (i = 0; i < num_reg; i++)
    emit_insn (stores[i]);

  return TRUE;
}


/* Expand a block clear operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the length
   operands[2] is the alignment */

int
frv_expand_block_clear (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx bytes_rtx	= operands[1];
  rtx align_rtx = operands[2];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
  int bytes;
  int offset;
  int num_reg;
  rtx dest_reg;
  rtx dest_addr;
  rtx dest_mem;
  int clear_bytes;
  enum machine_mode mode;

  /* If this is not a fixed size move, just call memcpy.  */
  if (! constp)
    return FALSE;

  /* If this is not a fixed size alignment, abort.  */
  if (GET_CODE (align_rtx) != CONST_INT)
    abort ();

  align = INTVAL (align_rtx);

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return TRUE;

  /* Don't support real large clears.  */
  if (bytes > TOTAL_MOVE_REG*align)
    return FALSE;

  /* Move the address into a scratch register.  */
  dest_reg = copy_addr_to_reg (XEXP (orig_dest, 0));

  num_reg = offset = 0;
  for ( ; bytes > 0; (bytes -= clear_bytes), (offset += clear_bytes))
    {
      /* Calculate the correct offset for src/dest.  */
      dest_addr = ((offset == 0)
		   ? dest_reg
		   : plus_constant (dest_reg, offset));

      /* Generate the appropriate store of gr0.  */
      if (bytes >= 4 && align >= 4)
	mode = SImode;
      else if (bytes >= 2 && align >= 2)
	mode = HImode;
      else
	mode = QImode;

      clear_bytes = GET_MODE_SIZE (mode);
      dest_mem = change_address (orig_dest, mode, dest_addr);
      emit_insn (gen_rtx_SET (VOIDmode, dest_mem, const0_rtx));
    }

  return TRUE;
}


/* The following variable is used to output modifiers of assembler
   code of the current output insn.  */

static rtx *frv_insn_operands;

/* The following function is used to add assembler insn code suffix .p
   if it is necessary.  */

const char *
frv_asm_output_opcode (FILE *f, const char *ptr)
{
  int c;

  if (! PACKING_FLAG_USED_P())
    return ptr;

  for (; *ptr && *ptr != ' ' && *ptr != '\t';)
    {
      c = *ptr++;
      if (c == '%' && ((*ptr >= 'a' && *ptr <= 'z')
		       || (*ptr >= 'A' && *ptr <= 'Z')))
	{
	  int letter = *ptr++;

	  c = atoi (ptr);
	  frv_print_operand (f, frv_insn_operands [c], letter);
	  while ((c = *ptr) >= '0' && c <= '9')
	    ptr++;
	}
      else
	fputc (c, f);
    }

  if (!frv_insn_packing_flag)
    fprintf (f, ".p");

  return ptr;
}

/* The following function sets up the packing bit for the current
   output insn.  Remember that the function is not called for asm
   insns.  */

void
frv_final_prescan_insn (rtx insn, rtx *opvec, int noperands ATTRIBUTE_UNUSED)
{
  if (! PACKING_FLAG_USED_P())
    return;

  if (!INSN_P (insn))
    return;

  frv_insn_operands = opvec;

  /* Look for the next printable instruction.  frv_pack_insns () has set
     things up so that any printable instruction will have TImode if it
     starts a new packet and VOIDmode if it should be packed with the
     previous instruction.

     Printable instructions will be asm_operands or match one of the .md
     patterns.  Since asm instructions cannot be packed -- and will
     therefore have TImode -- this loop terminates on any recognizable
     instruction, and on any unrecognizable instruction with TImode.  */
  for (insn = NEXT_INSN (insn); insn; insn = NEXT_INSN (insn))
    {
      if (NOTE_P (insn))
	continue;
      else if (!INSN_P (insn))
	break;
      else if (GET_MODE (insn) == TImode || INSN_CODE (insn) != -1)
	break;
    }

  /* Set frv_insn_packing_flag to FALSE if the next instruction should
     be packed with this one.  Set it to TRUE otherwise.  If the next
     instruction is an asm instruction, this statement will set the
     flag to TRUE, and that value will still hold when the asm operands
     themselves are printed.  */
  frv_insn_packing_flag = ! (insn && INSN_P (insn)
			     && GET_MODE (insn) != TImode);
}



/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */

/* The default is correct, but we need to make sure the frame gets created.  */
rtx
frv_dynamic_chain_address (rtx frame)
{
  cfun->machine->frame_needed = 1;
  return frame;
}


/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */

rtx
frv_return_addr_rtx (int count ATTRIBUTE_UNUSED, rtx frame)
{
  cfun->machine->frame_needed = 1;
  return gen_rtx_MEM (Pmode, plus_constant (frame, 8));
}

/* Given a memory reference MEMREF, interpret the referenced memory as
   an array of MODE values, and return a reference to the element
   specified by INDEX.  Assume that any pre-modification implicit in
   MEMREF has already happened.

   MEMREF must be a legitimate operand for modes larger than SImode.
   GO_IF_LEGITIMATE_ADDRESS forbids register+register addresses, which
   this function cannot handle.  */
rtx
frv_index_memory (rtx memref, enum machine_mode mode, int index)
{
  rtx base = XEXP (memref, 0);
  if (GET_CODE (base) == PRE_MODIFY)
    base = XEXP (base, 0);
  return change_address (memref, mode,
			 plus_constant (base, index * GET_MODE_SIZE (mode)));
}


/* Print a memory address as an operand to reference that memory location.  */
void
frv_print_operand_address (FILE * stream, rtx x)
{
  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [ REGNO (x)], stream);
      return;

    case CONST_INT:
      fprintf (stream, "%ld", (long) INTVAL (x));
      return;

    case SYMBOL_REF:
      assemble_name (stream, XSTR (x, 0));
      return;

    case LABEL_REF:
    case CONST:
      output_addr_const (stream, x);
      return;

    default:
      break;
    }

  fatal_insn ("Bad insn to frv_print_operand_address:", x);
}


static void
frv_print_operand_memory_reference_reg (FILE * stream, rtx x)
{
  int regno = true_regnum (x);
  if (GPR_P (regno))
    fputs (reg_names[regno], stream);
  else
    fatal_insn ("Bad register to frv_print_operand_memory_reference_reg:", x);
}

/* Print a memory reference suitable for the ld/st instructions.  */

static void
frv_print_operand_memory_reference (FILE * stream, rtx x, int addr_offset)
{
  rtx x0 = NULL_RTX;
  rtx x1 = NULL_RTX;

  switch (GET_CODE (x))
    {
    case SUBREG:
    case REG:
      x0 = x;
      break;

    case PRE_MODIFY:		/* (pre_modify (reg) (plus (reg) (reg))) */
      x0 = XEXP (x, 0);
      x1 = XEXP (XEXP (x, 1), 1);
      break;

    case CONST_INT:
      x1 = x;
      break;

    case PLUS:
      x0 = XEXP (x, 0);
      x1 = XEXP (x, 1);
      if (GET_CODE (x0) == CONST_INT)
	{
	  x0 = XEXP (x, 1);
	  x1 = XEXP (x, 0);
	}
      break;

    default:
      fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);
      break;

    }

  if (addr_offset)
    {
      if (!x1)
	x1 = const0_rtx;
      else if (GET_CODE (x1) != CONST_INT)
	fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);
    }

  fputs ("@(", stream);
  if (!x0)
    fputs (reg_names[GPR_R0], stream);
  else if (GET_CODE (x0) == REG || GET_CODE (x0) == SUBREG)
    frv_print_operand_memory_reference_reg (stream, x0);
  else
    fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);

  fputs (",", stream);
  if (!x1)
    fputs (reg_names [GPR_R0], stream);

  else
    {
      switch (GET_CODE (x1))
	{
	case SUBREG:
	case REG:
	  frv_print_operand_memory_reference_reg (stream, x1);
	  break;

	case CONST_INT:
	  fprintf (stream, "%ld", (long) (INTVAL (x1) + addr_offset));
	  break;

	case SYMBOL_REF:
	  if (x0 && GET_CODE (x0) == REG && REGNO (x0) == SDA_BASE_REG
	      && SYMBOL_REF_SMALL_P (x1))
	    {
	      fputs ("#gprel12(", stream);
	      assemble_name (stream, XSTR (x1, 0));
	      fputs (")", stream);
	    }
	  else
	    fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);
	  break;

	case CONST:
	  if (x0 && GET_CODE (x0) == REG && REGNO (x0) == SDA_BASE_REG
	      && const_small_data_p (x1))
	    {
	      fputs ("#gprel12(", stream);
	      assemble_name (stream, XSTR (XEXP (XEXP (x1, 0), 0), 0));
	      fprintf (stream, "+"HOST_WIDE_INT_PRINT_DEC")",
		       INTVAL (XEXP (XEXP (x1, 0), 1)));
	    }
	  else
	    fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);
	  break;

	default:
	  fatal_insn ("Bad insn to frv_print_operand_memory_reference:", x);
	}
    }

  fputs (")", stream);
}


/* Return 2 for likely branches and 0 for non-likely branches  */

#define FRV_JUMP_LIKELY 2
#define FRV_JUMP_NOT_LIKELY 0

static int
frv_print_operand_jump_hint (rtx insn)
{
  rtx note;
  rtx labelref;
  int ret;
  HOST_WIDE_INT prob = -1;
  enum { UNKNOWN, BACKWARD, FORWARD } jump_type = UNKNOWN;

  if (GET_CODE (insn) != JUMP_INSN)
    abort ();

  /* Assume any non-conditional jump is likely.  */
  if (! any_condjump_p (insn))
    ret = FRV_JUMP_LIKELY;

  else
    {
      labelref = condjump_label (insn);
      if (labelref)
	{
	  rtx label = XEXP (labelref, 0);
	  jump_type = (insn_current_address > INSN_ADDRESSES (INSN_UID (label))
		       ? BACKWARD
		       : FORWARD);
	}

      note = find_reg_note (insn, REG_BR_PROB, 0);
      if (!note)
	ret = ((jump_type == BACKWARD) ? FRV_JUMP_LIKELY : FRV_JUMP_NOT_LIKELY);

      else
	{
	  prob = INTVAL (XEXP (note, 0));
	  ret = ((prob >= (REG_BR_PROB_BASE / 2))
		 ? FRV_JUMP_LIKELY
		 : FRV_JUMP_NOT_LIKELY);
	}
    }

#if 0
  if (TARGET_DEBUG)
    {
      char *direction;

      switch (jump_type)
	{
	default:
	case UNKNOWN:	direction = "unknown jump direction";	break;
	case BACKWARD:	direction = "jump backward";		break;
	case FORWARD:	direction = "jump forward";		break;
	}

      fprintf (stderr,
	       "%s: uid %ld, %s, probability = %ld, max prob. = %ld, hint = %d\n",
	       IDENTIFIER_POINTER (DECL_NAME (current_function_decl)),
	       (long)INSN_UID (insn), direction, (long)prob,
	       (long)REG_BR_PROB_BASE, ret);
    }
#endif

  return ret;
}


/* Print an operand to an assembler instruction.

   `%' followed by a letter and a digit says to output an operand in an
   alternate fashion.  Four letters have standard, built-in meanings described
   below.  The machine description macro `PRINT_OPERAND' can define additional
   letters with nonstandard meanings.

   `%cDIGIT' can be used to substitute an operand that is a constant value
   without the syntax that normally indicates an immediate operand.

   `%nDIGIT' is like `%cDIGIT' except that the value of the constant is negated
   before printing.

   `%aDIGIT' can be used to substitute an operand as if it were a memory
   reference, with the actual operand treated as the address.  This may be
   useful when outputting a "load address" instruction, because often the
   assembler syntax for such an instruction requires you to write the operand
   as if it were a memory reference.

   `%lDIGIT' is used to substitute a `label_ref' into a jump instruction.

   `%=' outputs a number which is unique to each instruction in the entire
   compilation.  This is useful for making local labels to be referred to more
   than once in a single template that generates multiple assembler
   instructions.

   `%' followed by a punctuation character specifies a substitution that does
   not use an operand.  Only one case is standard: `%%' outputs a `%' into the
   assembler code.  Other nonstandard cases can be defined in the
   `PRINT_OPERAND' macro.  You must also define which punctuation characters
   are valid with the `PRINT_OPERAND_PUNCT_VALID_P' macro.  */

void
frv_print_operand (FILE * file, rtx x, int code)
{
  HOST_WIDE_INT value;
  int offset;

  if (code != 0 && !isalpha (code))
    value = 0;

  else if (GET_CODE (x) == CONST_INT)
    value = INTVAL (x);

  else if (GET_CODE (x) == CONST_DOUBLE)
    {
      if (GET_MODE (x) == SFmode)
	{
	  REAL_VALUE_TYPE rv;
	  long l;

	  REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	  REAL_VALUE_TO_TARGET_SINGLE (rv, l);
	  value = l;
	}

      else if (GET_MODE (x) == VOIDmode)
	value = CONST_DOUBLE_LOW (x);

      else
        fatal_insn ("Bad insn in frv_print_operand, bad const_double", x);
    }

  else
    value = 0;

  switch (code)
    {

    case '.':
      /* Output r0.  */
      fputs (reg_names[GPR_R0], file);
      break;

    case '#':
      fprintf (file, "%d", frv_print_operand_jump_hint (current_output_insn));
      break;

    case '@':
      /* Output small data area base register (gr16).  */
      fputs (reg_names[SDA_BASE_REG], file);
      break;

    case '~':
      /* Output pic register (gr17).  */
      fputs (reg_names[PIC_REGNO], file);
      break;

    case '*':
      /* Output the temporary integer CCR register.  */
      fputs (reg_names[ICR_TEMP], file);
      break;

    case '&':
      /* Output the temporary integer CC register.  */
      fputs (reg_names[ICC_TEMP], file);
      break;

    /* case 'a': print an address.  */

    case 'C':
      /* Print appropriate test for integer branch false operation.  */
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'C' modifier:", x);

	case EQ:  fputs ("ne", file); break;
	case NE:  fputs ("eq", file); break;
	case LT:  fputs ("ge", file); break;
	case LE:  fputs ("gt", file); break;
	case GT:  fputs ("le", file); break;
	case GE:  fputs ("lt", file); break;
	case LTU: fputs ("nc", file); break;
	case LEU: fputs ("hi", file); break;
	case GTU: fputs ("ls", file); break;
	case GEU: fputs ("c",  file); break;
	}
      break;

    /* case 'c': print a constant without the constant prefix.  If
       CONSTANT_ADDRESS_P(x) is not true, PRINT_OPERAND is called.  */

    case 'c':
      /* Print appropriate test for integer branch true operation.  */
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'c' modifier:", x);

	case EQ:  fputs ("eq", file); break;
	case NE:  fputs ("ne", file); break;
	case LT:  fputs ("lt", file); break;
	case LE:  fputs ("le", file); break;
	case GT:  fputs ("gt", file); break;
	case GE:  fputs ("ge", file); break;
	case LTU: fputs ("c",  file); break;
	case LEU: fputs ("ls", file); break;
	case GTU: fputs ("hi", file); break;
	case GEU: fputs ("nc", file); break;
	}
      break;

    case 'e':
      /* Print 1 for a NE and 0 for an EQ to give the final argument
	 for a conditional instruction.  */
      if (GET_CODE (x) == NE)
	fputs ("1", file);

      else if (GET_CODE (x) == EQ)
	fputs ("0", file);

      else
	fatal_insn ("Bad insn to frv_print_operand, 'e' modifier:", x);
      break;

    case 'F':
      /* Print appropriate test for floating point branch false operation.  */
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'F' modifier:", x);

	case EQ:  fputs ("ne",  file); break;
	case NE:  fputs ("eq",  file); break;
	case LT:  fputs ("uge", file); break;
	case LE:  fputs ("ug",  file); break;
	case GT:  fputs ("ule", file); break;
	case GE:  fputs ("ul",  file); break;
	}
      break;

    case 'f':
      /* Print appropriate test for floating point branch true operation.  */
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'f' modifier:", x);

	case EQ:  fputs ("eq",  file); break;
	case NE:  fputs ("ne",  file); break;
	case LT:  fputs ("lt",  file); break;
	case LE:  fputs ("le",  file); break;
	case GT:  fputs ("gt",  file); break;
	case GE:  fputs ("ge",  file); break;
	}
      break;

    case 'I':
      /* Print 'i' if the operand is a constant, or is a memory reference that
         adds a constant.  */
      if (GET_CODE (x) == MEM)
	x = ((GET_CODE (XEXP (x, 0)) == PLUS)
	     ? XEXP (XEXP (x, 0), 1)
	     : XEXP (x, 0));

      switch (GET_CODE (x))
	{
	default:
	  break;

	case CONST_INT:
	case SYMBOL_REF:
	case CONST:
	  fputs ("i", file);
	  break;
	}
      break;

    case 'i':
      /* For jump instructions, print 'i' if the operand is a constant or
         is an expression that adds a constant.  */
      if (GET_CODE (x) == CONST_INT)
        fputs ("i", file);

      else
        {
          if (GET_CODE (x) == CONST_INT
              || (GET_CODE (x) == PLUS
                  && (GET_CODE (XEXP (x, 1)) == CONST_INT
                      || GET_CODE (XEXP (x, 0)) == CONST_INT)))
            fputs ("i", file);
        }
      break;

    case 'L':
      /* Print the lower register of a double word register pair */
      if (GET_CODE (x) == REG)
	fputs (reg_names[ REGNO (x)+1 ], file);
      else
	fatal_insn ("Bad insn to frv_print_operand, 'L' modifier:", x);
      break;

    /* case 'l': print a LABEL_REF.  */

    case 'M':
    case 'N':
      /* Print a memory reference for ld/st/jmp, %N prints a memory reference
         for the second word of double memory operations.  */
      offset = (code == 'M') ? 0 : UNITS_PER_WORD;
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'M/N' modifier:", x);

	case MEM:
	  frv_print_operand_memory_reference (file, XEXP (x, 0), offset);
	  break;

	case REG:
	case SUBREG:
	case CONST_INT:
	case PLUS:
        case SYMBOL_REF:
	  frv_print_operand_memory_reference (file, x, offset);
	  break;
	}
      break;

    case 'O':
      /* Print the opcode of a command.  */
      switch (GET_CODE (x))
	{
	default:
	  fatal_insn ("Bad insn to frv_print_operand, 'O' modifier:", x);

	case PLUS:     fputs ("add", file); break;
	case MINUS:    fputs ("sub", file); break;
	case AND:      fputs ("and", file); break;
	case IOR:      fputs ("or",  file); break;
	case XOR:      fputs ("xor", file); break;
	case ASHIFT:   fputs ("sll", file); break;
	case ASHIFTRT: fputs ("sra", file); break;
	case LSHIFTRT: fputs ("srl", file); break;
	}
      break;

    /* case 'n': negate and print a constant int.  */

    case 'P':
      /* Print PIC label using operand as the number.  */
      if (GET_CODE (x) != CONST_INT)
	fatal_insn ("Bad insn to frv_print_operand, P modifier:", x);

      fprintf (file, ".LCF%ld", (long)INTVAL (x));
      break;

    case 'U':
      /* Print 'u' if the operand is a update load/store.  */
      if (GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	fputs ("u", file);
      break;

    case 'z':
      /* If value is 0, print gr0, otherwise it must be a register.  */
      if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0)
	fputs (reg_names[GPR_R0], file);

      else if (GET_CODE (x) == REG)
        fputs (reg_names [REGNO (x)], file);

      else
        fatal_insn ("Bad insn in frv_print_operand, z case", x);
      break;

    case 'x':
      /* Print constant in hex.  */
      if (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
        {
	  fprintf (file, "%s0x%.4lx", IMMEDIATE_PREFIX, (long) value);
	  break;
	}

      /* Fall through.  */

    case '\0':
      if (GET_CODE (x) == REG)
        fputs (reg_names [REGNO (x)], file);

      else if (GET_CODE (x) == CONST_INT
              || GET_CODE (x) == CONST_DOUBLE)
        fprintf (file, "%s%ld", IMMEDIATE_PREFIX, (long) value);

      else if (GET_CODE (x) == MEM)
        frv_print_operand_address (file, XEXP (x, 0));

      else if (CONSTANT_ADDRESS_P (x))
        frv_print_operand_address (file, x);

      else
        fatal_insn ("Bad insn in frv_print_operand, 0 case", x);

      break;

    default:
      fatal_insn ("frv_print_operand: unknown code", x);
      break;
    }

  return;
}


/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

void
frv_init_cumulative_args (CUMULATIVE_ARGS *cum,
                          tree fntype,
                          rtx libname,
                          tree fndecl,
                          int incoming)
{
  *cum = FIRST_ARG_REGNUM;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (!fndecl && fntype)
	fputs (" indirect", stderr);

      if (incoming)
	fputs (" incoming", stderr);

      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " return=%s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

      if (libname && GET_CODE (libname) == SYMBOL_REF)
	fprintf (stderr, " libname=%s", XSTR (libname, 0));

      if (cfun->returns_struct)
	fprintf (stderr, " return-struct");

      putc ('\n', stderr);
    }
}


/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

int
frv_function_arg_boundary (enum machine_mode mode ATTRIBUTE_UNUSED,
                           tree type ATTRIBUTE_UNUSED)
{
  return BITS_PER_WORD;
}


/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes (in a way
   defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE) all of the previous
   arguments so far passed in registers; MODE, the machine mode of the argument;
   TYPE, the data type of the argument as a tree node or 0 if that is not known
   (which happens for C support library functions); and NAMED, which is 1 for an
   ordinary argument and 0 for nameless arguments that correspond to `...' in the
   called function's prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the VAX and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns nonzero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */

rtx
frv_function_arg (CUMULATIVE_ARGS *cum,
                  enum machine_mode mode,
                  tree type ATTRIBUTE_UNUSED,
                  int named,
                  int incoming ATTRIBUTE_UNUSED)
{
  enum machine_mode xmode = (mode == BLKmode) ? SImode : mode;
  int arg_num = *cum;
  rtx ret;
  const char *debstr;

  /* Return a marker for use in the call instruction.  */
  if (xmode == VOIDmode)
    {
      ret = const0_rtx;
      debstr = "<0>";
    }

  else if (arg_num <= LAST_ARG_REGNUM)
    {
      ret = gen_rtx (REG, xmode, arg_num);
      debstr = reg_names[arg_num];
    }

  else
    {
      ret = NULL_RTX;
      debstr = "memory";
    }

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_arg: words = %2d, mode = %4s, named = %d, size = %3d, arg = %s\n",
	     arg_num, GET_MODE_NAME (mode), named, GET_MODE_SIZE (mode), debstr);

  return ret;
}


/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */

void
frv_function_arg_advance (CUMULATIVE_ARGS *cum,
                          enum machine_mode mode,
                          tree type ATTRIBUTE_UNUSED,
                          int named)
{
  enum machine_mode xmode = (mode == BLKmode) ? SImode : mode;
  int bytes = GET_MODE_SIZE (xmode);
  int words = (bytes + UNITS_PER_WORD  - 1) / UNITS_PER_WORD;
  int arg_num = *cum;

  *cum = arg_num + words;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_adv: words = %2d, mode = %4s, named = %d, size = %3d\n",
	     arg_num, GET_MODE_NAME (mode), named, words * UNITS_PER_WORD);
}


/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */

int
frv_function_arg_partial_nregs (CUMULATIVE_ARGS *cum,
                                enum machine_mode mode,
                                tree type ATTRIBUTE_UNUSED,
                                int named ATTRIBUTE_UNUSED)
{
  enum machine_mode xmode = (mode == BLKmode) ? SImode : mode;
  int bytes = GET_MODE_SIZE (xmode);
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int arg_num = *cum;
  int ret;

  ret = ((arg_num <= LAST_ARG_REGNUM && arg_num + words > LAST_ARG_REGNUM+1)
	 ? LAST_ARG_REGNUM - arg_num + 1
	 : 0);

  if (TARGET_DEBUG_ARG && ret)
    fprintf (stderr, "function_arg_partial_nregs: %d\n", ret);

  return ret;

}



/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */

int
frv_function_arg_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
                                    enum machine_mode mode,
                                    tree type,
                                    int named ATTRIBUTE_UNUSED)
{
  return MUST_PASS_IN_STACK (mode, type);
}

/* If defined, a C expression that indicates when it is the called function's
   responsibility to make a copy of arguments passed by invisible reference.
   Normally, the caller makes a copy and passes the address of the copy to the
   routine being called.  When FUNCTION_ARG_CALLEE_COPIES is defined and is
   nonzero, the caller does not make a copy.  Instead, it passes a pointer to
   the "live" value.  The called function must not modify this value.  If it
   can be determined that the value won't be modified, it need not make a copy;
   otherwise a copy must be made.  */

int
frv_function_arg_callee_copies (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
                                enum machine_mode mode ATTRIBUTE_UNUSED,
                                tree type ATTRIBUTE_UNUSED,
                                int named ATTRIBUTE_UNUSED)
{
  return 0;
}

/* If defined, a C expression that indicates when it is more desirable to keep
   an argument passed by invisible reference as a reference, rather than
   copying it to a pseudo register.  */

int
frv_function_arg_keep_as_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
                                    enum machine_mode mode ATTRIBUTE_UNUSED,
                                    tree type ATTRIBUTE_UNUSED,
                                    int named ATTRIBUTE_UNUSED)
{
  return 0;
}


/* Return true if a register is ok to use as a base or index register.  */

static FRV_INLINE int
frv_regno_ok_for_base_p (int regno, int strict_p)
{
  if (GPR_P (regno))
    return TRUE;

  if (strict_p)
    return (reg_renumber[regno] >= 0 && GPR_P (reg_renumber[regno]));

  if (regno == ARG_POINTER_REGNUM)
    return TRUE;

  return (regno >= FIRST_PSEUDO_REGISTER);
}


/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.

   It usually pays to define several simpler macros to serve as subroutines for
   this one.  Otherwise it may be too complicated to understand.

   This macro must exist in two variants: a strict variant and a non-strict
   one.  The strict variant is used in the reload pass.  It must be defined so
   that any pseudo-register that has not been allocated a hard register is
   considered a memory reference.  In contexts where some kind of register is
   required, a pseudo-register with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be defined to
   accept all pseudo-registers in every context where some kind of register is
   required.

   Compiler source files that want to use the strict variant of this macro
   define the macro `REG_OK_STRICT'.  You should use an `#ifdef REG_OK_STRICT'
   conditional to define the strict variant in that case and the non-strict
   variant otherwise.

   Subroutines to check for acceptable registers for various purposes (one for
   base registers, one for index registers, and so on) are typically among the
   subroutines used to define `GO_IF_LEGITIMATE_ADDRESS'.  Then only these
   subroutine macros need have two variants; the higher levels of macros may be
   the same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref' and an
   integer are stored inside a `const' RTX to mark them as constant.
   Therefore, there is no need to recognize such sums specifically as
   legitimate addresses.  Normally you would simply recognize any `const' as
   legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle constant sums that
   are not marked with `const'.  It assumes that a naked `plus' indicates
   indexing.  If so, then you *must* reject such naked constant sums as
   illegitimate addresses, so that none of them will be given to
   `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate depends on the
   section that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  When you see a `const', you will have to look
   inside it to find the `symbol_ref' in order to determine the section.

   The best way to modify the name string is by adding text to the beginning,
   with suitable punctuation to prevent any ambiguity.  Allocate the new name
   in `saveable_obstack'.  You will have to modify `ASM_OUTPUT_LABELREF' to
   remove and decode the added text and output the name accordingly, and define
   `(* targetm.strip_name_encoding)' to access the original name string.

   You can check the information stored here into the `symbol_ref' in the
   definitions of the macros `GO_IF_LEGITIMATE_ADDRESS' and
   `PRINT_OPERAND_ADDRESS'.  */

int
frv_legitimate_address_p (enum machine_mode mode,
                          rtx x,
                          int strict_p,
                          int condexec_p)
{
  rtx x0, x1;
  int ret = 0;
  HOST_WIDE_INT value;
  unsigned regno0;

  switch (GET_CODE (x))
    {
    default:
      break;

    case SUBREG:
      x = SUBREG_REG (x);
      if (GET_CODE (x) != REG)
        break;

      /* Fall through.  */

    case REG:
      ret = frv_regno_ok_for_base_p (REGNO (x), strict_p);
      break;

    case PRE_MODIFY:
      x0 = XEXP (x, 0);
      x1 = XEXP (x, 1);
      if (GET_CODE (x0) != REG
	  || ! frv_regno_ok_for_base_p (REGNO (x0), strict_p)
	  || GET_CODE (x1) != PLUS
	  || ! rtx_equal_p (x0, XEXP (x1, 0))
	  || GET_CODE (XEXP (x1, 1)) != REG
	  || ! frv_regno_ok_for_base_p (REGNO (XEXP (x1, 1)), strict_p))
	break;

      ret = 1;
      break;

    case CONST_INT:
      /* 12 bit immediate */
      if (condexec_p)
	ret = FALSE;
      else
	{
	  ret = IN_RANGE_P (INTVAL (x), -2048, 2047);

	  /* If we can't use load/store double operations, make sure we can
	     address the second word.  */
	  if (ret && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	    ret = IN_RANGE_P (INTVAL (x) + GET_MODE_SIZE (mode) - 1,
			      -2048, 2047);
	}
      break;

    case PLUS:
      x0 = XEXP (x, 0);
      x1 = XEXP (x, 1);

      if (GET_CODE (x0) == SUBREG)
	x0 = SUBREG_REG (x0);

      if (GET_CODE (x0) != REG)
	break;

      regno0 = REGNO (x0);
      if (!frv_regno_ok_for_base_p (regno0, strict_p))
	break;

      switch (GET_CODE (x1))
	{
	default:
	  break;

	case SUBREG:
	  x1 = SUBREG_REG (x1);
	  if (GET_CODE (x1) != REG)
	    break;

	  /* Fall through.  */

	case REG:
	  /* Do not allow reg+reg addressing for modes > 1 word if we
	     can't depend on having move double instructions.  */
	  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	    ret = FALSE;
	  else
	    ret = frv_regno_ok_for_base_p (REGNO (x1), strict_p);
	  break;

	case CONST_INT:
          /* 12 bit immediate */
	  if (condexec_p)
	    ret = FALSE;
	  else
	    {
	      value = INTVAL (x1);
	      ret = IN_RANGE_P (value, -2048, 2047);

	      /* If we can't use load/store double operations, make sure we can
		 address the second word.  */
	      if (ret && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
		ret = IN_RANGE_P (value + GET_MODE_SIZE (mode) - 1, -2048, 2047);
	    }
	  break;

	case SYMBOL_REF:
	  if (!condexec_p
	      && regno0 == SDA_BASE_REG
	      && SYMBOL_REF_SMALL_P (x1))
	    ret = TRUE;
	  break;

	case CONST:
	  if (!condexec_p && regno0 == SDA_BASE_REG && const_small_data_p (x1))
	    ret = TRUE;
	  break;

	}
      break;
    }

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n========== GO_IF_LEGITIMATE_ADDRESS, mode = %s, result = %d, addresses are %sstrict%s\n",
	       GET_MODE_NAME (mode), ret, (strict_p) ? "" : "not ",
	       (condexec_p) ? ", inside conditional code" : "");
      debug_rtx (x);
    }

  return ret;
}


/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */

rtx
frv_legitimize_address (rtx x,
                        rtx oldx ATTRIBUTE_UNUSED,
                        enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx ret = NULL_RTX;

  /* Don't try to legitimize addresses if we are not optimizing, since the
     address we generate is not a general operand, and will horribly mess
     things up when force_reg is called to try and put it in a register because
     we aren't optimizing.  */
  if (optimize
      && ((GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_SMALL_P (x))
	  || (GET_CODE (x) == CONST && const_small_data_p (x))))
    {
      ret = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, SDA_BASE_REG), x);
      if (flag_pic)
	cfun->uses_pic_offset_table = TRUE;
    }

  if (TARGET_DEBUG_ADDR && ret != NULL_RTX)
    {
      fprintf (stderr, "\n========== LEGITIMIZE_ADDRESS, mode = %s, modified address\n",
	       GET_MODE_NAME (mode));
      debug_rtx (ret);
    }

  return ret;
}

/* Return 1 if operand is a valid FRV address.  CONDEXEC_P is true if
   the operand is used by a predicated instruction.  */

static int
frv_legitimate_memory_operand (rtx op, enum machine_mode mode, int condexec_p)
{
  return ((GET_MODE (op) == mode || mode == VOIDmode)
	  && GET_CODE (op) == MEM
	  && frv_legitimate_address_p (mode, XEXP (op, 0),
				       reload_completed, condexec_p));
}


/* Return 1 is OP is a memory operand, or will be turned into one by
   reload.  */

int
frv_load_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (reload_in_progress)
    {
      rtx tmp = op;
      if (GET_CODE (tmp) == SUBREG)
	tmp = SUBREG_REG (tmp);
      if (GET_CODE (tmp) == REG
	  && REGNO (tmp) >= FIRST_PSEUDO_REGISTER)
	op = reg_equiv_memory_loc[REGNO (tmp)];
    }

  return op && memory_operand (op, mode);
}


/* Return 1 if operand is a GPR register or a FPR register.  */

int
gpr_or_fpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (GPR_P (regno) || FPR_P (regno) || regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  return FALSE;
}

/* Return 1 if operand is a GPR register or 12 bit signed immediate.  */

int
gpr_or_int12_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -2048, 2047);

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return 1 if operand is a GPR register, or a FPR register, or a 12 bit
   signed immediate.  */

int
gpr_fpr_or_int12_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -2048, 2047);

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (GPR_P (regno) || FPR_P (regno) || regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  return FALSE;
}

/* Return 1 if operand is a register or 6 bit signed immediate.  */

int
fpr_or_int6_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -32, 31);

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return FPR_OR_PSEUDO_P (REGNO (op));
}

/* Return 1 if operand is a register or 10 bit signed immediate.  */

int
gpr_or_int10_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT)
    return IN_RANGE_P (INTVAL (op), -512, 511);

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return 1 if operand is a register or an integer immediate.  */

int
gpr_or_int_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT)
    return TRUE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return 1 if operand is a 12 bit signed immediate.  */

int
int12_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != CONST_INT)
    return FALSE;

  return IN_RANGE_P (INTVAL (op), -2048, 2047);
}

/* Return 1 if operand is a 6 bit signed immediate.  */

int
int6_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != CONST_INT)
    return FALSE;

  return IN_RANGE_P (INTVAL (op), -32, 31);
}

/* Return 1 if operand is a 5 bit signed immediate.  */

int
int5_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && IN_RANGE_P (INTVAL (op), -16, 15);
}

/* Return 1 if operand is a 5 bit unsigned immediate.  */

int
uint5_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && IN_RANGE_P (INTVAL (op), 0, 31);
}

/* Return 1 if operand is a 4 bit unsigned immediate.  */

int
uint4_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && IN_RANGE_P (INTVAL (op), 0, 15);
}

/* Return 1 if operand is a 1 bit unsigned immediate (0 or 1).  */

int
uint1_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return GET_CODE (op) == CONST_INT && IN_RANGE_P (INTVAL (op), 0, 1);
}

/* Return 1 if operand is an integer constant that takes 2 instructions
   to load up and can be split into sethi/setlo instructions..  */

int
int_2word_operand(rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT value;
  REAL_VALUE_TYPE rv;
  long l;

  switch (GET_CODE (op))
    {
    default:
      break;

    case LABEL_REF:
      return (flag_pic == 0);

    case CONST:
      /* small data references are already 1 word */
      return (flag_pic == 0) && (! const_small_data_p (op));

    case SYMBOL_REF:
      /* small data references are already 1 word */
      return (flag_pic == 0) && (! SYMBOL_REF_SMALL_P (op));

    case CONST_INT:
      return ! IN_RANGE_P (INTVAL (op), -32768, 32767);

    case CONST_DOUBLE:
      if (GET_MODE (op) == SFmode)
	{
	  REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
	  REAL_VALUE_TO_TARGET_SINGLE (rv, l);
	  value = l;
	  return ! IN_RANGE_P (value, -32768, 32767);
	}
      else if (GET_MODE (op) == VOIDmode)
	{
	  value = CONST_DOUBLE_LOW (op);
	  return ! IN_RANGE_P (value, -32768, 32767);
	}
      break;
    }

  return FALSE;
}

/* Return 1 if operand is the pic address register.  */
int
pic_register_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (! flag_pic)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  if (REGNO (op) != PIC_REGNO)
    return FALSE;

  return TRUE;
}

/* Return 1 if operand is a symbolic reference when a PIC option is specified
   that takes 3 separate instructions to form.  */

int
pic_symbolic_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (! flag_pic)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      break;

    case LABEL_REF:
      return TRUE;

    case SYMBOL_REF:
      /* small data references are already 1 word */
      return ! SYMBOL_REF_SMALL_P (op);

    case CONST:
      /* small data references are already 1 word */
      return ! const_small_data_p (op);
    }

  return FALSE;
}

/* Return 1 if operand is the small data register.  */
int
small_data_register_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != REG)
    return FALSE;

  if (REGNO (op) != SDA_BASE_REG)
    return FALSE;

  return TRUE;
}

/* Return 1 if operand is a symbolic reference to a small data area static or
   global object.  */

int
small_data_symbolic_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST:
      return const_small_data_p (op);

    case SYMBOL_REF:
      return SYMBOL_REF_SMALL_P (op);
    }

  return FALSE;
}

/* Return 1 if operand is a 16 bit unsigned immediate.  */

int
uint16_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != CONST_INT)
    return FALSE;

  return IN_RANGE_P (INTVAL (op), 0, 0xffff);
}

/* Return 1 if operand is an integer constant with the bottom 16 bits
   clear.  */

int
upper_int16_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) != CONST_INT)
    return FALSE;

  return ((INTVAL (op) & 0xffff) == 0);
}

/* Return true if operand is a GPR register.  */

int
integer_register_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a GPR register.  Do not allow SUBREG's
   here, in order to prevent a combine bug.  */

int
gpr_no_subreg_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  return GPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is a FPR register.  */

int
fpr_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return FPR_OR_PSEUDO_P (REGNO (op));
}

/* Return true if operand is an even GPR or FPR register.  */

int
even_reg_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  if (GPR_P (regno))
    return (((regno - GPR_FIRST) & 1) == 0);

  if (FPR_P (regno))
    return (((regno - FPR_FIRST) & 1) == 0);

  return FALSE;
}

/* Return true if operand is an odd GPR register.  */

int
odd_reg_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  /* Assume that reload will give us an even register.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    return FALSE;

  if (GPR_P (regno))
    return (((regno - GPR_FIRST) & 1) != 0);

  if (FPR_P (regno))
    return (((regno - FPR_FIRST) & 1) != 0);

  return FALSE;
}

/* Return true if operand is an even GPR register.  */

int
even_gpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  if (! GPR_P (regno))
    return FALSE;

  return (((regno - GPR_FIRST) & 1) == 0);
}

/* Return true if operand is an odd GPR register.  */

int
odd_gpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  /* Assume that reload will give us an even register.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    return FALSE;

  if (! GPR_P (regno))
    return FALSE;

  return (((regno - GPR_FIRST) & 1) != 0);
}

/* Return true if operand is a quad aligned FPR register.  */

int
quad_fpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  if (! FPR_P (regno))
    return FALSE;

  return (((regno - FPR_FIRST) & 3) == 0);
}

/* Return true if operand is an even FPR register.  */

int
even_fpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (regno >= FIRST_PSEUDO_REGISTER)
    return TRUE;

  if (! FPR_P (regno))
    return FALSE;

  return (((regno - FPR_FIRST) & 1) == 0);
}

/* Return true if operand is an odd FPR register.  */

int
odd_fpr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
        return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  /* Assume that reload will give us an even register.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    return FALSE;

  if (! FPR_P (regno))
    return FALSE;

  return (((regno - FPR_FIRST) & 1) != 0);
}

/* Return true if operand is a 2 word memory address that can be loaded in one
   instruction to load or store.  We assume the stack and frame pointers are
   suitably aligned, and variables in the small data area.  FIXME -- at some we
   should recognize other globals and statics. We can't assume that any old
   pointer is aligned, given that arguments could be passed on an odd word on
   the stack and the address taken and passed through to another function.  */

int
dbl_memory_one_insn_operand (rtx op, enum machine_mode mode)
{
  rtx addr;
  rtx addr_reg;

  if (! TARGET_DWORD)
    return FALSE;

  if (GET_CODE (op) != MEM)
    return FALSE;

  if (mode != VOIDmode && GET_MODE_SIZE (mode) != 2*UNITS_PER_WORD)
    return FALSE;

  addr = XEXP (op, 0);
  if (GET_CODE (addr) == REG)
    addr_reg = addr;

  else if (GET_CODE (addr) == PLUS)
    {
      rtx addr0 = XEXP (addr, 0);
      rtx addr1 = XEXP (addr, 1);

      if (GET_CODE (addr0) != REG)
	return FALSE;

      if (plus_small_data_p (addr0, addr1))
	return TRUE;

      if (GET_CODE (addr1) != CONST_INT)
	return FALSE;

      if ((INTVAL (addr1) & 7) != 0)
	return FALSE;

      addr_reg = addr0;
    }

  else
    return FALSE;

  if (addr_reg == frame_pointer_rtx || addr_reg == stack_pointer_rtx)
    return TRUE;

  return FALSE;
}

/* Return true if operand is a 2 word memory address that needs to
   use two instructions to load or store.  */

int
dbl_memory_two_insn_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) != MEM)
    return FALSE;

  if (mode != VOIDmode && GET_MODE_SIZE (mode) != 2*UNITS_PER_WORD)
    return FALSE;

  if (! TARGET_DWORD)
    return TRUE;

  return ! dbl_memory_one_insn_operand (op, mode);
}

/* Return true if operand is something that can be an output for a move
   operation.  */

int
move_destination_operand (rtx op, enum machine_mode mode)
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return frv_legitimate_address_p (mode, XEXP (subreg, 0),
					 reload_completed, FALSE);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
        return TRUE;

      return frv_legitimate_memory_operand (op, mode, FALSE);
    }

  return FALSE;
}

/* Return true if operand is something that can be an input for a move
   operation.  */

int
move_source_operand (rtx op, enum machine_mode mode)
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return immediate_operand (op, mode);

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return frv_legitimate_address_p (mode, XEXP (subreg, 0),
					 reload_completed, FALSE);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
        return TRUE;

      return frv_legitimate_memory_operand (op, mode, FALSE);
    }

  return FALSE;
}

/* Return true if operand is something that can be an output for a conditional
   move operation.  */

int
condexec_dest_operand (rtx op, enum machine_mode mode)
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return frv_legitimate_address_p (mode, XEXP (subreg, 0),
					 reload_completed, TRUE);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
        return TRUE;

      return frv_legitimate_memory_operand (op, mode, TRUE);
    }

  return FALSE;
}

/* Return true if operand is something that can be an input for a conditional
   move operation.  */

int
condexec_source_operand (rtx op, enum machine_mode mode)
{
  rtx subreg;
  enum rtx_code code;

  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
    case CONST_DOUBLE:
      return ZERO_P (op);

    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      subreg = SUBREG_REG (op);
      code = GET_CODE (subreg);
      if (code == MEM)
	return frv_legitimate_address_p (mode, XEXP (subreg, 0),
					 reload_completed, TRUE);

      return (code == REG);

    case REG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
        return FALSE;

      return TRUE;

    case MEM:
      if (GET_CODE (XEXP (op, 0)) == ADDRESSOF)
        return TRUE;

      return frv_legitimate_memory_operand (op, mode, TRUE);
    }

  return FALSE;
}

/* Return true if operand is a register of any flavor or a 0 of the
   appropriate type.  */

int
reg_or_0_operand (rtx op, enum machine_mode mode)
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case REG:
    case SUBREG:
      if (GET_MODE (op) != mode && mode != VOIDmode)
	return FALSE;

      return register_operand (op, mode);

    case CONST_INT:
    case CONST_DOUBLE:
      return ZERO_P (op);
    }

  return FALSE;
}

/* Return true if operand is the link register.  */

int
lr_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) != REG)
    return FALSE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (REGNO (op) != LR_REGNO && REGNO (op) < FIRST_PSEUDO_REGISTER)
    return FALSE;

  return TRUE;
}

/* Return true if operand is a gpr register or a valid memory operation.  */

int
gpr_or_memory_operand (rtx op, enum machine_mode mode)
{
  return (integer_register_operand (op, mode)
	  || frv_legitimate_memory_operand (op, mode, FALSE));
}

/* Return true if operand is a fpr register or a valid memory operation.  */

int
fpr_or_memory_operand (rtx op, enum machine_mode mode)
{
  return (fpr_operand (op, mode)
	  || frv_legitimate_memory_operand (op, mode, FALSE));
}

/* Return true if operand is an icc register.  */

int
icc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return ICC_OR_PSEUDO_P (regno);
}

/* Return true if operand is an fcc register.  */

int
fcc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return FCC_OR_PSEUDO_P (regno);
}

/* Return true if operand is either an fcc or icc register.  */

int
cc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (CC_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operand is an integer CCR register.  */

int
icr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return ICR_OR_PSEUDO_P (regno);
}

/* Return true if operand is an fcc register.  */

int
fcr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return FCR_OR_PSEUDO_P (regno);
}

/* Return true if operand is either an fcc or icc register.  */

int
cr_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  if (CR_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operand is a memory reference suitable for a call.  */

int
call_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode && GET_CODE (op) != CONST_INT)
    return FALSE;

  if (GET_CODE (op) == SYMBOL_REF)
    return TRUE;

  /* Note this doesn't allow reg+reg or reg+imm12 addressing (which should
     never occur anyway), but prevents reload from not handling the case
     properly of a call through a pointer on a function that calls
     vfork/setjmp, etc. due to the need to flush all of the registers to stack.  */
  return gpr_or_int12_operand (op, mode);
}

/* Return true if operator is a kind of relational operator.  */

int
relational_operator (rtx op, enum machine_mode mode)
{
  rtx op0;
  rtx op1;
  int regno;

  if (mode != VOIDmode && mode != GET_MODE (op))
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case EQ:
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
    case LEU:
    case LTU:
    case GEU:
    case GTU:
      break;
    }

  op1 = XEXP (op, 1);
  if (op1 != const0_rtx)
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) != REG)
    return FALSE;

  regno = REGNO (op0);
  switch (GET_MODE (op0))
    {
    default:
      break;

    case CCmode:
    case CC_UNSmode:
      return ICC_OR_PSEUDO_P (regno);

    case CC_FPmode:
      return FCC_OR_PSEUDO_P (regno);

    case CC_CCRmode:
      return CR_OR_PSEUDO_P (regno);
    }

  return FALSE;
}

/* Return true if operator is a signed integer relational operator.  */

int
signed_relational_operator (rtx op, enum machine_mode mode)
{
  rtx op0;
  rtx op1;
  int regno;

  if (mode != VOIDmode && mode != GET_MODE (op))
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case EQ:
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
      break;
    }

  op1 = XEXP (op, 1);
  if (op1 != const0_rtx)
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) != REG)
    return FALSE;

  regno = REGNO (op0);
  if (GET_MODE (op0) == CCmode && ICC_OR_PSEUDO_P (regno))
    return TRUE;

  if (GET_MODE (op0) == CC_CCRmode && CR_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operator is a signed integer relational operator.  */

int
unsigned_relational_operator (rtx op, enum machine_mode mode)
{
  rtx op0;
  rtx op1;
  int regno;

  if (mode != VOIDmode && mode != GET_MODE (op))
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case LEU:
    case LTU:
    case GEU:
    case GTU:
      break;
    }

  op1 = XEXP (op, 1);
  if (op1 != const0_rtx)
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) != REG)
    return FALSE;

  regno = REGNO (op0);
  if (GET_MODE (op0) == CC_UNSmode && ICC_OR_PSEUDO_P (regno))
    return TRUE;

  if (GET_MODE (op0) == CC_CCRmode && CR_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operator is a floating point relational operator.  */

int
float_relational_operator (rtx op, enum machine_mode mode)
{
  rtx op0;
  rtx op1;
  int regno;

  if (mode != VOIDmode && mode != GET_MODE (op))
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case EQ: case NE:
    case LE: case LT:
    case GE: case GT:
#if 0
    case UEQ: case UNE:
    case ULE: case ULT:
    case UGE: case UGT:
    case ORDERED:
    case UNORDERED:
#endif
      break;
    }

  op1 = XEXP (op, 1);
  if (op1 != const0_rtx)
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) != REG)
    return FALSE;

  regno = REGNO (op0);
  if (GET_MODE (op0) == CC_FPmode && FCC_OR_PSEUDO_P (regno))
    return TRUE;

  if (GET_MODE (op0) == CC_CCRmode && CR_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operator is EQ/NE of a conditional execution register.  */

int
ccr_eqne_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);
  rtx op0;
  rtx op1;
  int regno;

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case EQ:
    case NE:
      break;
    }

  op1 = XEXP (op, 1);
  if (op1 != const0_rtx)
    return FALSE;

  op0 = XEXP (op, 0);
  if (GET_CODE (op0) != REG)
    return FALSE;

  regno = REGNO (op0);
  if (op_mode == CC_CCRmode && CR_OR_PSEUDO_P (regno))
    return TRUE;

  return FALSE;
}

/* Return true if operator is a minimum or maximum operator (both signed and
   unsigned).  */

int
minmax_operator (rtx op, enum machine_mode mode)
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case SMIN:
    case SMAX:
    case UMIN:
    case UMAX:
      break;
    }

  if (! integer_register_operand (XEXP (op, 0), mode))
    return FALSE;

  if (! gpr_or_int10_operand (XEXP (op, 1), mode))
    return FALSE;

  return TRUE;
}

/* Return true if operator is an integer binary operator that can executed
   conditionally and takes 1 cycle.  */

int
condexec_si_binary_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case PLUS:
    case MINUS:
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      return TRUE;
    }
}

/* Return true if operator is an integer binary operator that can be
   executed conditionally by a media instruction.  */

int
condexec_si_media_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case AND:
    case IOR:
    case XOR:
      return TRUE;
    }
}

/* Return true if operator is an integer division operator that can executed
   conditionally.  */

int
condexec_si_divide_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case DIV:
    case UDIV:
      return TRUE;
    }
}

/* Return true if operator is an integer unary operator that can executed
   conditionally.  */

int
condexec_si_unary_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case NEG:
    case NOT:
      return TRUE;
    }
}

/* Return true if operator is a conversion-type expression that can be
   evaluated conditionally by floating-point instructions.  */

int
condexec_sf_conv_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case NEG:
    case ABS:
      return TRUE;
    }
}

/* Return true if operator is an addition or subtraction expression.
   Such expressions can be evaluated conditionally by floating-point
   instructions.  */

int
condexec_sf_add_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case PLUS:
    case MINUS:
      return TRUE;
    }
}

/* Return true if the memory operand is one that can be conditionally
   executed.  */

int
condexec_memory_operand (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);
  rtx addr;

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (op_mode)
    {
    default:
      return FALSE;

    case QImode:
    case HImode:
    case SImode:
    case SFmode:
      break;
    }

  if (GET_CODE (op) != MEM)
    return FALSE;

  addr = XEXP (op, 0);
  if (GET_CODE (addr) == ADDRESSOF)
    return TRUE;

  return frv_legitimate_address_p (mode, addr, reload_completed, TRUE);
}

/* Return true if operator is an integer binary operator that can be combined
   with a setcc operation.  Do not allow the arithmetic operations that could
   potentially overflow since the FR-V sets the condition code based on the
   "true" value of the result, not the result after truncating to a 32-bit
   register.  */

int
intop_compare_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case AND:
    case IOR:
    case XOR:
    case ASHIFTRT:
    case LSHIFTRT:
      break;
    }

  if (! integer_register_operand (XEXP (op, 0), SImode))
    return FALSE;

  if (! gpr_or_int10_operand (XEXP (op, 1), SImode))
    return FALSE;

  return TRUE;
}

/* Return true if operator is an integer binary operator that can be combined
   with a setcc operation inside of a conditional execution.  */

int
condexec_intop_cmp_operator (rtx op, enum machine_mode mode)
{
  enum machine_mode op_mode = GET_MODE (op);

  if (mode != VOIDmode && op_mode != mode)
    return FALSE;

  switch (GET_CODE (op))
    {
    default:
      return FALSE;

    case AND:
    case IOR:
    case XOR:
    case ASHIFTRT:
    case LSHIFTRT:
      break;
    }

  if (! integer_register_operand (XEXP (op, 0), SImode))
    return FALSE;

  if (! integer_register_operand (XEXP (op, 1), SImode))
    return FALSE;

  return TRUE;
}

/* Return 1 if operand is a valid ACC register number.  */

int
acc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return ACC_OR_PSEUDO_P (regno);
}

/* Return 1 if operand is a valid even ACC register number.  */

int
even_acc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return (ACC_OR_PSEUDO_P (regno) && ((regno - ACC_FIRST) & 1) == 0);
}

/* Return 1 if operand is zero or four.  */

int
quad_acc_operand (rtx op, enum machine_mode mode)
{
  int regno;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  regno = REGNO (op);
  return (ACC_OR_PSEUDO_P (regno) && ((regno - ACC_FIRST) & 3) == 0);
}

/* Return 1 if operand is a valid ACCG register number.  */

int
accg_operand (rtx op, enum machine_mode mode)
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (op)) != REG)
	return register_operand (op, mode);

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) != REG)
    return FALSE;

  return ACCG_OR_PSEUDO_P (REGNO (op));
}


/* Return true if the bare return instruction can be used outside of the
   epilog code.  For frv, we only do it if there was no stack allocation.  */

int
direct_return_p (void)
{
  frv_stack_t *info;

  if (!reload_completed)
    return FALSE;

  info = frv_stack_info ();
  return (info->total_size == 0);
}


/* Emit code to handle a MOVSI, adding in the small data register or pic
   register if needed to load up addresses.  Return TRUE if the appropriate
   instructions are emitted.  */

int
frv_emit_movsi (rtx dest, rtx src)
{
  int base_regno = -1;

  if (!reload_in_progress
      && !reload_completed
      && !register_operand (dest, SImode)
      && (!reg_or_0_operand (src, SImode)
	     /* Virtual registers will almost always be replaced by an
		add instruction, so expose this to CSE by copying to
		an intermediate register.  */
	  || (GET_CODE (src) == REG
	      && IN_RANGE_P (REGNO (src),
			     FIRST_VIRTUAL_REGISTER,
			     LAST_VIRTUAL_REGISTER))))
    {
      emit_insn (gen_rtx_SET (VOIDmode, dest, copy_to_mode_reg (SImode, src)));
      return TRUE;
    }

  /* Explicitly add in the PIC or small data register if needed.  */
  switch (GET_CODE (src))
    {
    default:
      break;

    case LABEL_REF:
      if (flag_pic)
	base_regno = PIC_REGNO;

      break;

    case CONST:
      if (const_small_data_p (src))
	base_regno = SDA_BASE_REG;

      else if (flag_pic)
	base_regno = PIC_REGNO;

      break;

    case SYMBOL_REF:
      if (SYMBOL_REF_SMALL_P (src))
	base_regno = SDA_BASE_REG;

      else if (flag_pic)
	base_regno = PIC_REGNO;

      break;
    }

  if (base_regno >= 0)
    {
      emit_insn (gen_rtx_SET (VOIDmode, dest,
			      gen_rtx_PLUS (Pmode,
					    gen_rtx_REG (Pmode, base_regno),
					    src)));

      if (base_regno == PIC_REGNO)
	cfun->uses_pic_offset_table = TRUE;

      return TRUE;
    }

  return FALSE;
}


/* Return a string to output a single word move.  */

const char *
output_move_single (rtx operands[], rtx insn)
{
  rtx dest = operands[0];
  rtx src  = operands[1];

  if (GET_CODE (dest) == REG)
    {
      int dest_regno = REGNO (dest);
      enum machine_mode mode = GET_MODE (dest);

      if (GPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* gpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "mov %1, %0";

	      else if (FPR_P (src_regno))
		return "movfg %1, %0";

	      else if (SPR_P (src_regno))
		return "movsg %1, %0";
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* gpr <- memory */
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "ldsb%I1%U1 %M1,%0";

		case HImode:
		  return "ldsh%I1%U1 %M1,%0";

		case SImode:
		case SFmode:
		  return "ld%I1%U1 %M1, %0";
		}
	    }

	  else if (GET_CODE (src) == CONST_INT
		   || GET_CODE (src) == CONST_DOUBLE)
	    {
	      /* gpr <- integer/floating constant */
	      HOST_WIDE_INT value;

	      if (GET_CODE (src) == CONST_INT)
		value = INTVAL (src);

	      else if (mode == SFmode)
		{
		  REAL_VALUE_TYPE rv;
		  long l;

		  REAL_VALUE_FROM_CONST_DOUBLE (rv, src);
		  REAL_VALUE_TO_TARGET_SINGLE (rv, l);
		  value = l;
		}

	      else
		value = CONST_DOUBLE_LOW (src);

	      if (IN_RANGE_P (value, -32768, 32767))
		return "setlos %1, %0";

	      return "#";
	    }

          else if (GET_CODE (src) == SYMBOL_REF
		   || GET_CODE (src) == LABEL_REF
		   || GET_CODE (src) == CONST)
	    {
	      /* Silently fix up instances where the small data pointer is not
                 used in the address.  */
	      if (small_data_symbolic_operand (src, GET_MODE (src)))
		return "addi %@, #gprel12(%1), %0";

	      return "#";
	    }
	}

      else if (FPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* fpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "movgf %1, %0";

	      else if (FPR_P (src_regno))
		{
		  if (TARGET_HARD_FLOAT)
		    return "fmovs %1, %0";
		  else
		    return "mor %1, %1, %0";
		}
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* fpr <- memory */
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "ldbf%I1%U1 %M1,%0";

		case HImode:
		  return "ldhf%I1%U1 %M1,%0";

		case SImode:
		case SFmode:
		  return "ldf%I1%U1 %M1, %0";
		}
	    }

	  else if (ZERO_P (src))
	    return "movgf %., %0";
	}

      else if (SPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* spr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "movgs %1, %0";
	    }
	}
    }

  else if (GET_CODE (dest) == MEM)
    {
      if (GET_CODE (src) == REG)
	{
	  int src_regno = REGNO (src);
	  enum machine_mode mode = GET_MODE (dest);

	  if (GPR_P (src_regno))
	    {
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "stb%I0%U0 %1, %M0";

		case HImode:
		  return "sth%I0%U0 %1, %M0";

		case SImode:
		case SFmode:
		  return "st%I0%U0 %1, %M0";
		}
	    }

	  else if (FPR_P (src_regno))
	    {
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "stbf%I0%U0 %1, %M0";

		case HImode:
		  return "sthf%I0%U0 %1, %M0";

		case SImode:
		case SFmode:
		  return "stf%I0%U0 %1, %M0";
		}
	    }
	}

      else if (ZERO_P (src))
	{
	  switch (GET_MODE (dest))
	    {
	    default:
	      break;

	    case QImode:
	      return "stb%I0%U0 %., %M0";

	    case HImode:
	      return "sth%I0%U0 %., %M0";

	    case SImode:
	    case SFmode:
	      return "st%I0%U0 %., %M0";
	    }
	}
    }

  fatal_insn ("Bad output_move_single operand", insn);
  return "";
}


/* Return a string to output a double word move.  */

const char *
output_move_double (rtx operands[], rtx insn)
{
  rtx dest = operands[0];
  rtx src  = operands[1];
  enum machine_mode mode = GET_MODE (dest);

  if (GET_CODE (dest) == REG)
    {
      int dest_regno = REGNO (dest);

      if (GPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* gpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "#";

	      else if (FPR_P (src_regno))
		{
		  if (((dest_regno - GPR_FIRST) & 1) == 0
		      && ((src_regno - FPR_FIRST) & 1) == 0)
		    return "movfgd %1, %0";

		  return "#";
		}
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* gpr <- memory */
	      if (dbl_memory_one_insn_operand (src, mode))
		return "ldd%I1%U1 %M1, %0";

	      return "#";
	    }

	  else if (GET_CODE (src) == CONST_INT
		   || GET_CODE (src) == CONST_DOUBLE)
	    return "#";
	}

      else if (FPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* fpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		{
		  if (((dest_regno - FPR_FIRST) & 1) == 0
		      && ((src_regno - GPR_FIRST) & 1) == 0)
		    return "movgfd %1, %0";

		  return "#";
		}

	      else if (FPR_P (src_regno))
		{
		  if (TARGET_DOUBLE
		      && ((dest_regno - FPR_FIRST) & 1) == 0
		      && ((src_regno - FPR_FIRST) & 1) == 0)
		    return "fmovd %1, %0";

		  return "#";
		}
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* fpr <- memory */
	      if (dbl_memory_one_insn_operand (src, mode))
		return "lddf%I1%U1 %M1, %0";

	      return "#";
	    }

	  else if (ZERO_P (src))
	    return "#";
	}
    }

  else if (GET_CODE (dest) == MEM)
    {
      if (GET_CODE (src) == REG)
	{
	  int src_regno = REGNO (src);

	  if (GPR_P (src_regno))
	    {
	      if (((src_regno - GPR_FIRST) & 1) == 0
		  && dbl_memory_one_insn_operand (dest, mode))
		return "std%I0%U0 %1, %M0";

	      return "#";
	    }

	  if (FPR_P (src_regno))
	    {
	      if (((src_regno - FPR_FIRST) & 1) == 0
		  && dbl_memory_one_insn_operand (dest, mode))
		return "stdf%I0%U0 %1, %M0";

	      return "#";
	    }
	}

      else if (ZERO_P (src))
	{
	  if (dbl_memory_one_insn_operand (dest, mode))
	    return "std%I0%U0 %., %M0";

	  return "#";
	}
    }

  fatal_insn ("Bad output_move_double operand", insn);
  return "";
}


/* Return a string to output a single word conditional move.
   Operand0 -- EQ/NE of ccr register and 0
   Operand1 -- CCR register
   Operand2 -- destination
   Operand3 -- source  */

const char *
output_condmove_single (rtx operands[], rtx insn)
{
  rtx dest = operands[2];
  rtx src  = operands[3];

  if (GET_CODE (dest) == REG)
    {
      int dest_regno = REGNO (dest);
      enum machine_mode mode = GET_MODE (dest);

      if (GPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* gpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "cmov %z3, %2, %1, %e0";

	      else if (FPR_P (src_regno))
		return "cmovfg %3, %2, %1, %e0";
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* gpr <- memory */
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "cldsb%I3%U3 %M3, %2, %1, %e0";

		case HImode:
		  return "cldsh%I3%U3 %M3, %2, %1, %e0";

		case SImode:
		case SFmode:
		  return "cld%I3%U3 %M3, %2, %1, %e0";
		}
	    }

	  else if (ZERO_P (src))
	    return "cmov %., %2, %1, %e0";
	}

      else if (FPR_P (dest_regno))
	{
	  if (GET_CODE (src) == REG)
	    {
	      /* fpr <- some sort of register */
	      int src_regno = REGNO (src);

	      if (GPR_P (src_regno))
		return "cmovgf %3, %2, %1, %e0";

	      else if (FPR_P (src_regno))
		{
		  if (TARGET_HARD_FLOAT)
		    return "cfmovs %3,%2,%1,%e0";
		  else
		    return "cmor %3, %3, %2, %1, %e0";
		}
	    }

	  else if (GET_CODE (src) == MEM)
	    {
	      /* fpr <- memory */
	      if (mode == SImode || mode == SFmode)
		return "cldf%I3%U3 %M3, %2, %1, %e0";
	    }

	  else if (ZERO_P (src))
	    return "cmovgf %., %2, %1, %e0";
	}
    }

  else if (GET_CODE (dest) == MEM)
    {
      if (GET_CODE (src) == REG)
	{
	  int src_regno = REGNO (src);
	  enum machine_mode mode = GET_MODE (dest);

	  if (GPR_P (src_regno))
	    {
	      switch (mode)
		{
		default:
		  break;

		case QImode:
		  return "cstb%I2%U2 %3, %M2, %1, %e0";

		case HImode:
		  return "csth%I2%U2 %3, %M2, %1, %e0";

		case SImode:
		case SFmode:
		  return "cst%I2%U2 %3, %M2, %1, %e0";
		}
	    }

	  else if (FPR_P (src_regno) && (mode == SImode || mode == SFmode))
	    return "cstf%I2%U2 %3, %M2, %1, %e0";
	}

      else if (ZERO_P (src))
	{
	  enum machine_mode mode = GET_MODE (dest);
	  switch (mode)
	    {
	    default:
	      break;

	    case QImode:
	      return "cstb%I2%U2 %., %M2, %1, %e0";

	    case HImode:
	      return "csth%I2%U2 %., %M2, %1, %e0";

	    case SImode:
	    case SFmode:
	      return "cst%I2%U2 %., %M2, %1, %e0";
	    }
	}
    }

  fatal_insn ("Bad output_condmove_single operand", insn);
  return "";
}


/* Emit the appropriate code to do a comparison, returning the register the
   comparison was done it.  */

static rtx
frv_emit_comparison (enum rtx_code test, rtx op0, rtx op1)
{
  enum machine_mode cc_mode;
  rtx cc_reg;

  /* Floating point doesn't have comparison against a constant.  */
  if (GET_MODE (op0) == CC_FPmode && GET_CODE (op1) != REG)
    op1 = force_reg (GET_MODE (op0), op1);

  /* Possibly disable using anything but a fixed register in order to work
     around cse moving comparisons past function calls.  */
  cc_mode = SELECT_CC_MODE (test, op0, op1);
  cc_reg = ((TARGET_ALLOC_CC)
	    ? gen_reg_rtx (cc_mode)
	    : gen_rtx_REG (cc_mode,
			   (cc_mode == CC_FPmode) ? FCC_FIRST : ICC_FIRST));

  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (cc_mode, op0, op1)));

  return cc_reg;
}


/* Emit code for a conditional branch.  The comparison operands were previously
   stored in frv_compare_op0 and frv_compare_op1.

   XXX: I originally wanted to add a clobber of a CCR register to use in
   conditional execution, but that confuses the rest of the compiler.  */

int
frv_emit_cond_branch (enum rtx_code test, rtx label)
{
  rtx test_rtx;
  rtx label_ref;
  rtx if_else;
  rtx cc_reg = frv_emit_comparison (test, frv_compare_op0, frv_compare_op1);
  enum machine_mode cc_mode = GET_MODE (cc_reg);

  /* Branches generate:
	(set (pc)
	     (if_then_else (<test>, <cc_reg>, (const_int 0))
			    (label_ref <branch_label>)
			    (pc))) */
  label_ref = gen_rtx_LABEL_REF (VOIDmode, label);
  test_rtx = gen_rtx (test, cc_mode, cc_reg, const0_rtx);
  if_else = gen_rtx_IF_THEN_ELSE (cc_mode, test_rtx, label_ref, pc_rtx);
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, if_else));
  return TRUE;
}


/* Emit code to set a gpr to 1/0 based on a comparison.  The comparison
   operands were previously stored in frv_compare_op0 and frv_compare_op1.  */

int
frv_emit_scc (enum rtx_code test, rtx target)
{
  rtx set;
  rtx test_rtx;
  rtx clobber;
  rtx cr_reg;
  rtx cc_reg = frv_emit_comparison (test, frv_compare_op0, frv_compare_op1);

  /* SCC instructions generate:
	(parallel [(set <target> (<test>, <cc_reg>, (const_int 0))
		   (clobber (<ccr_reg>))])  */
  test_rtx = gen_rtx_fmt_ee (test, SImode, cc_reg, const0_rtx);
  set = gen_rtx_SET (VOIDmode, target, test_rtx);

  cr_reg = ((TARGET_ALLOC_CC)
	    ? gen_reg_rtx (CC_CCRmode)
	    : gen_rtx_REG (CC_CCRmode,
			   ((GET_MODE (cc_reg) == CC_FPmode)
			    ? FCR_FIRST
			    : ICR_FIRST)));

  clobber = gen_rtx_CLOBBER (VOIDmode, cr_reg);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
  return TRUE;
}


/* Split a SCC instruction into component parts, returning a SEQUENCE to hold
   the separate insns.  */

rtx
frv_split_scc (rtx dest, rtx test, rtx cc_reg, rtx cr_reg, HOST_WIDE_INT value)
{
  rtx ret;

  start_sequence ();

  /* Set the appropriate CCR bit.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cr_reg,
			  gen_rtx_fmt_ee (GET_CODE (test),
					  GET_MODE (cr_reg),
					  cc_reg,
					  const0_rtx)));

  /* Move the value into the destination.  */
  emit_move_insn (dest, GEN_INT (value));

  /* Move 0 into the destination if the test failed */
  emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				gen_rtx_EQ (GET_MODE (cr_reg),
					    cr_reg,
					    const0_rtx),
				gen_rtx_SET (VOIDmode, dest, const0_rtx)));

  /* Finish up, return sequence.  */
  ret = get_insns ();
  end_sequence ();
  return ret;
}


/* Emit the code for a conditional move, return TRUE if we could do the
   move.  */

int
frv_emit_cond_move (rtx dest, rtx test_rtx, rtx src1, rtx src2)
{
  rtx set;
  rtx clobber_cc;
  rtx test2;
  rtx cr_reg;
  rtx if_rtx;
  enum rtx_code test = GET_CODE (test_rtx);
  rtx cc_reg = frv_emit_comparison (test, frv_compare_op0, frv_compare_op1);
  enum machine_mode cc_mode = GET_MODE (cc_reg);

  /* Conditional move instructions generate:
	(parallel [(set <target>
			(if_then_else (<test> <cc_reg> (const_int 0))
				      <src1>
				      <src2>))
		   (clobber (<ccr_reg>))])  */

  /* Handle various cases of conditional move involving two constants.  */
  if (GET_CODE (src1) == CONST_INT && GET_CODE (src2) == CONST_INT)
    {
      HOST_WIDE_INT value1 = INTVAL (src1);
      HOST_WIDE_INT value2 = INTVAL (src2);

      /* Having 0 as one of the constants can be done by loading the other
         constant, and optionally moving in gr0.  */
      if (value1 == 0 || value2 == 0)
	;

      /* If the first value is within an addi range and also the difference
         between the two fits in an addi's range, load up the difference, then
         conditionally move in 0, and then unconditionally add the first
	 value.  */
      else if (IN_RANGE_P (value1, -2048, 2047)
	       && IN_RANGE_P (value2 - value1, -2048, 2047))
	;

      /* If neither condition holds, just force the constant into a
	 register.  */
      else
	{
	  src1 = force_reg (GET_MODE (dest), src1);
	  src2 = force_reg (GET_MODE (dest), src2);
	}
    }

  /* If one value is a register, insure the other value is either 0 or a
     register.  */
  else
    {
      if (GET_CODE (src1) == CONST_INT && INTVAL (src1) != 0)
	src1 = force_reg (GET_MODE (dest), src1);

      if (GET_CODE (src2) == CONST_INT && INTVAL (src2) != 0)
	src2 = force_reg (GET_MODE (dest), src2);
    }

  test2 = gen_rtx_fmt_ee (test, cc_mode, cc_reg, const0_rtx);
  if_rtx = gen_rtx_IF_THEN_ELSE (GET_MODE (dest), test2, src1, src2);

  set = gen_rtx_SET (VOIDmode, dest, if_rtx);

  cr_reg = ((TARGET_ALLOC_CC)
	    ? gen_reg_rtx (CC_CCRmode)
	    : gen_rtx_REG (CC_CCRmode,
			   (cc_mode == CC_FPmode) ? FCR_FIRST : ICR_FIRST));

  clobber_cc = gen_rtx_CLOBBER (VOIDmode, cr_reg);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber_cc)));
  return TRUE;
}


/* Split a conditional move into constituent parts, returning a SEQUENCE
   containing all of the insns.  */

rtx
frv_split_cond_move (rtx operands[])
{
  rtx dest	= operands[0];
  rtx test	= operands[1];
  rtx cc_reg	= operands[2];
  rtx src1	= operands[3];
  rtx src2	= operands[4];
  rtx cr_reg	= operands[5];
  rtx ret;
  enum machine_mode cr_mode = GET_MODE (cr_reg);

  start_sequence ();

  /* Set the appropriate CCR bit.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cr_reg,
			  gen_rtx_fmt_ee (GET_CODE (test),
					  GET_MODE (cr_reg),
					  cc_reg,
					  const0_rtx)));

  /* Handle various cases of conditional move involving two constants.  */
  if (GET_CODE (src1) == CONST_INT && GET_CODE (src2) == CONST_INT)
    {
      HOST_WIDE_INT value1 = INTVAL (src1);
      HOST_WIDE_INT value2 = INTVAL (src2);

      /* Having 0 as one of the constants can be done by loading the other
         constant, and optionally moving in gr0.  */
      if (value1 == 0)
	{
	  emit_move_insn (dest, src2);
	  emit_insn (gen_rtx_COND_EXEC (VOIDmode,
					gen_rtx_NE (cr_mode, cr_reg,
						    const0_rtx),
					gen_rtx_SET (VOIDmode, dest, src1)));
	}

      else if (value2 == 0)
	{
	  emit_move_insn (dest, src1);
	  emit_insn (gen_rtx_COND_EXEC (VOIDmode,
					gen_rtx_EQ (cr_mode, cr_reg,
						    const0_rtx),
					gen_rtx_SET (VOIDmode, dest, src2)));
	}

      /* If the first value is within an addi range and also the difference
         between the two fits in an addi's range, load up the difference, then
         conditionally move in 0, and then unconditionally add the first
	 value.  */
      else if (IN_RANGE_P (value1, -2048, 2047)
	       && IN_RANGE_P (value2 - value1, -2048, 2047))
	{
	  rtx dest_si = ((GET_MODE (dest) == SImode)
			 ? dest
			 : gen_rtx_SUBREG (SImode, dest, 0));

	  emit_move_insn (dest_si, GEN_INT (value2 - value1));
	  emit_insn (gen_rtx_COND_EXEC (VOIDmode,
					gen_rtx_NE (cr_mode, cr_reg,
						    const0_rtx),
					gen_rtx_SET (VOIDmode, dest_si,
						     const0_rtx)));
	  emit_insn (gen_addsi3 (dest_si, dest_si, src1));
	}

      else
	abort ();
    }
  else
    {
      /* Emit the conditional move for the test being true if needed.  */
      if (! rtx_equal_p (dest, src1))
	emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				      gen_rtx_NE (cr_mode, cr_reg, const0_rtx),
				      gen_rtx_SET (VOIDmode, dest, src1)));

      /* Emit the conditional move for the test being false if needed.  */
      if (! rtx_equal_p (dest, src2))
	emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				      gen_rtx_EQ (cr_mode, cr_reg, const0_rtx),
				      gen_rtx_SET (VOIDmode, dest, src2)));
    }

  /* Finish up, return sequence.  */
  ret = get_insns ();
  end_sequence ();
  return ret;
}


/* Split (set DEST SOURCE), where DEST is a double register and SOURCE is a
   memory location that is not known to be dword-aligned.  */
void
frv_split_double_load (rtx dest, rtx source)
{
  int regno = REGNO (dest);
  rtx dest1 = gen_highpart (SImode, dest);
  rtx dest2 = gen_lowpart (SImode, dest);
  rtx address = XEXP (source, 0);

  /* If the address is pre-modified, load the lower-numbered register
     first, then load the other register using an integer offset from
     the modified base register.  This order should always be safe,
     since the pre-modification cannot affect the same registers as the
     load does.

     The situation for other loads is more complicated.  Loading one
     of the registers could affect the value of ADDRESS, so we must
     be careful which order we do them in.  */
  if (GET_CODE (address) == PRE_MODIFY
      || ! refers_to_regno_p (regno, regno + 1, address, NULL))
    {
      /* It is safe to load the lower-numbered register first.  */
      emit_move_insn (dest1, change_address (source, SImode, NULL));
      emit_move_insn (dest2, frv_index_memory (source, SImode, 1));
    }
  else
    {
      /* ADDRESS is not pre-modified and the address depends on the
         lower-numbered register.  Load the higher-numbered register
         first.  */
      emit_move_insn (dest2, frv_index_memory (source, SImode, 1));
      emit_move_insn (dest1, change_address (source, SImode, NULL));
    }
}

/* Split (set DEST SOURCE), where DEST refers to a dword memory location
   and SOURCE is either a double register or the constant zero.  */
void
frv_split_double_store (rtx dest, rtx source)
{
  rtx dest1 = change_address (dest, SImode, NULL);
  rtx dest2 = frv_index_memory (dest, SImode, 1);
  if (ZERO_P (source))
    {
      emit_move_insn (dest1, CONST0_RTX (SImode));
      emit_move_insn (dest2, CONST0_RTX (SImode));
    }
  else
    {
      emit_move_insn (dest1, gen_highpart (SImode, source));
      emit_move_insn (dest2, gen_lowpart (SImode, source));
    }
}


/* Split a min/max operation returning a SEQUENCE containing all of the
   insns.  */

rtx
frv_split_minmax (rtx operands[])
{
  rtx dest	= operands[0];
  rtx minmax	= operands[1];
  rtx src1	= operands[2];
  rtx src2	= operands[3];
  rtx cc_reg	= operands[4];
  rtx cr_reg	= operands[5];
  rtx ret;
  enum rtx_code test_code;
  enum machine_mode cr_mode = GET_MODE (cr_reg);

  start_sequence ();

  /* Figure out which test to use.  */
  switch (GET_CODE (minmax))
    {
    default:
      abort ();

    case SMIN: test_code = LT;  break;
    case SMAX: test_code = GT;  break;
    case UMIN: test_code = LTU; break;
    case UMAX: test_code = GTU; break;
    }

  /* Issue the compare instruction.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cc_reg,
			  gen_rtx_COMPARE (GET_MODE (cc_reg),
					   src1, src2)));

  /* Set the appropriate CCR bit.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cr_reg,
			  gen_rtx_fmt_ee (test_code,
					  GET_MODE (cr_reg),
					  cc_reg,
					  const0_rtx)));

  /* If are taking the min/max of a nonzero constant, load that first, and
     then do a conditional move of the other value.  */
  if (GET_CODE (src2) == CONST_INT && INTVAL (src2) != 0)
    {
      if (rtx_equal_p (dest, src1))
	abort ();

      emit_move_insn (dest, src2);
      emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				    gen_rtx_NE (cr_mode, cr_reg, const0_rtx),
				    gen_rtx_SET (VOIDmode, dest, src1)));
    }

  /* Otherwise, do each half of the move.  */
  else
    {
      /* Emit the conditional move for the test being true if needed.  */
      if (! rtx_equal_p (dest, src1))
	emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				      gen_rtx_NE (cr_mode, cr_reg, const0_rtx),
				      gen_rtx_SET (VOIDmode, dest, src1)));

      /* Emit the conditional move for the test being false if needed.  */
      if (! rtx_equal_p (dest, src2))
	emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				      gen_rtx_EQ (cr_mode, cr_reg, const0_rtx),
				      gen_rtx_SET (VOIDmode, dest, src2)));
    }

  /* Finish up, return sequence.  */
  ret = get_insns ();
  end_sequence ();
  return ret;
}


/* Split an integer abs operation returning a SEQUENCE containing all of the
   insns.  */

rtx
frv_split_abs (rtx operands[])
{
  rtx dest	= operands[0];
  rtx src	= operands[1];
  rtx cc_reg	= operands[2];
  rtx cr_reg	= operands[3];
  rtx ret;

  start_sequence ();

  /* Issue the compare < 0 instruction.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cc_reg,
			  gen_rtx_COMPARE (CCmode, src, const0_rtx)));

  /* Set the appropriate CCR bit.  */
  emit_insn (gen_rtx_SET (VOIDmode,
			  cr_reg,
			  gen_rtx_fmt_ee (LT, CC_CCRmode, cc_reg, const0_rtx)));

  /* Emit the conditional negate if the value is negative.  */
  emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				gen_rtx_NE (CC_CCRmode, cr_reg, const0_rtx),
				gen_negsi2 (dest, src)));

  /* Emit the conditional move for the test being false if needed.  */
  if (! rtx_equal_p (dest, src))
    emit_insn (gen_rtx_COND_EXEC (VOIDmode,
				  gen_rtx_EQ (CC_CCRmode, cr_reg, const0_rtx),
				  gen_rtx_SET (VOIDmode, dest, src)));

  /* Finish up, return sequence.  */
  ret = get_insns ();
  end_sequence ();
  return ret;
}


/* An internal function called by for_each_rtx to clear in a hard_reg set each
   register used in an insn.  */

static int
frv_clear_registers_used (rtx *ptr, void *data)
{
  if (GET_CODE (*ptr) == REG)
    {
      int regno = REGNO (*ptr);
      HARD_REG_SET *p_regs = (HARD_REG_SET *)data;

      if (regno < FIRST_PSEUDO_REGISTER)
	{
	  int reg_max = regno + HARD_REGNO_NREGS (regno, GET_MODE (*ptr));

	  while (regno < reg_max)
	    {
	      CLEAR_HARD_REG_BIT (*p_regs, regno);
	      regno++;
	    }
	}
    }

  return 0;
}


/* Initialize the extra fields provided by IFCVT_EXTRA_FIELDS.  */

/* On the FR-V, we don't have any extra fields per se, but it is useful hook to
   initialize the static storage.  */
void
frv_ifcvt_init_extra_fields (ce_if_block_t *ce_info ATTRIBUTE_UNUSED)
{
  frv_ifcvt.added_insns_list = NULL_RTX;
  frv_ifcvt.cur_scratch_regs = 0;
  frv_ifcvt.num_nested_cond_exec = 0;
  frv_ifcvt.cr_reg = NULL_RTX;
  frv_ifcvt.nested_cc_reg = NULL_RTX;
  frv_ifcvt.extra_int_cr = NULL_RTX;
  frv_ifcvt.extra_fp_cr = NULL_RTX;
  frv_ifcvt.last_nested_if_cr = NULL_RTX;
}


/* Internal function to add a potenial insn to the list of insns to be inserted
   if the conditional execution conversion is successful.  */

static void
frv_ifcvt_add_insn (rtx pattern, rtx insn, int before_p)
{
  rtx link = alloc_EXPR_LIST (VOIDmode, pattern, insn);

  link->jump = before_p;	/* Mark to add this before or after insn.  */
  frv_ifcvt.added_insns_list = alloc_EXPR_LIST (VOIDmode, link,
						frv_ifcvt.added_insns_list);

  if (TARGET_DEBUG_COND_EXEC)
    {
      fprintf (stderr,
	       "\n:::::::::: frv_ifcvt_add_insn: add the following %s insn %d:\n",
	       (before_p) ? "before" : "after",
	       (int)INSN_UID (insn));

      debug_rtx (pattern);
    }
}


/* A C expression to modify the code described by the conditional if
   information CE_INFO, possibly updating the tests in TRUE_EXPR, and
   FALSE_EXPR for converting if-then and if-then-else code to conditional
   instructions.  Set either TRUE_EXPR or FALSE_EXPR to a null pointer if the
   tests cannot be converted.  */

void
frv_ifcvt_modify_tests (ce_if_block_t *ce_info, rtx *p_true, rtx *p_false)
{
  basic_block test_bb = ce_info->test_bb;	/* test basic block */
  basic_block then_bb = ce_info->then_bb;	/* THEN */
  basic_block else_bb = ce_info->else_bb;	/* ELSE or NULL */
  basic_block join_bb = ce_info->join_bb;	/* join block or NULL */
  rtx true_expr = *p_true;
  rtx cr;
  rtx cc;
  rtx nested_cc;
  enum machine_mode mode = GET_MODE (true_expr);
  int j;
  basic_block *bb;
  int num_bb;
  frv_tmp_reg_t *tmp_reg = &frv_ifcvt.tmp_reg;
  rtx check_insn;
  rtx sub_cond_exec_reg;
  enum rtx_code code;
  enum rtx_code code_true;
  enum rtx_code code_false;
  enum reg_class cc_class;
  enum reg_class cr_class;
  int cc_first;
  int cc_last;

  /* Make sure we are only dealing with hard registers.  Also honor the
     -mno-cond-exec switch, and -mno-nested-cond-exec switches if
     applicable.  */
  if (!reload_completed || TARGET_NO_COND_EXEC
      || (TARGET_NO_NESTED_CE && ce_info->pass > 1))
    goto fail;

  /* Figure out which registers we can allocate for our own purposes.  Only
     consider registers that are not preserved across function calls and are
     not fixed.  However, allow the ICC/ICR temporary registers to be allocated
     if we did not need to use them in reloading other registers.  */
  memset (&tmp_reg->regs, 0, sizeof (tmp_reg->regs));
  COPY_HARD_REG_SET (tmp_reg->regs, call_used_reg_set);
  AND_COMPL_HARD_REG_SET (tmp_reg->regs, fixed_reg_set);
  SET_HARD_REG_BIT (tmp_reg->regs, ICC_TEMP);
  SET_HARD_REG_BIT (tmp_reg->regs, ICR_TEMP);

  /* If this is a nested IF, we need to discover whether the CC registers that
     are set/used inside of the block are used anywhere else.  If not, we can
     change them to be the CC register that is paired with the CR register that
     controls the outermost IF block.  */
  if (ce_info->pass > 1)
    {
      CLEAR_HARD_REG_SET (frv_ifcvt.nested_cc_ok_rewrite);
      for (j = CC_FIRST; j <= CC_LAST; j++)
	if (TEST_HARD_REG_BIT (tmp_reg->regs, j))
	  {
	    if (REGNO_REG_SET_P (then_bb->global_live_at_start, j))
	      continue;

	    if (else_bb && REGNO_REG_SET_P (else_bb->global_live_at_start, j))
	      continue;

	    if (join_bb && REGNO_REG_SET_P (join_bb->global_live_at_start, j))
	      continue;

	    SET_HARD_REG_BIT (frv_ifcvt.nested_cc_ok_rewrite, j);
	  }
    }

  for (j = 0; j < frv_ifcvt.cur_scratch_regs; j++)
    frv_ifcvt.scratch_regs[j] = NULL_RTX;

  frv_ifcvt.added_insns_list = NULL_RTX;
  frv_ifcvt.cur_scratch_regs = 0;

  bb = (basic_block *) alloca ((2 + ce_info->num_multiple_test_blocks)
			       * sizeof (basic_block));

  if (join_bb)
    {
      int regno;

      /* Remove anything live at the beginning of the join block from being
         available for allocation.  */
      EXECUTE_IF_SET_IN_REG_SET (join_bb->global_live_at_start, 0, regno,
				 {
				   if (regno < FIRST_PSEUDO_REGISTER)
				     CLEAR_HARD_REG_BIT (tmp_reg->regs, regno);
				 });
    }

  /* Add in all of the blocks in multiple &&/|| blocks to be scanned.  */
  num_bb = 0;
  if (ce_info->num_multiple_test_blocks)
    {
      basic_block multiple_test_bb = ce_info->last_test_bb;

      while (multiple_test_bb != test_bb)
	{
	  bb[num_bb++] = multiple_test_bb;
	  multiple_test_bb = multiple_test_bb->pred->src;
	}
    }

  /* Add in the THEN and ELSE blocks to be scanned.  */
  bb[num_bb++] = then_bb;
  if (else_bb)
    bb[num_bb++] = else_bb;

  sub_cond_exec_reg = NULL_RTX;
  frv_ifcvt.num_nested_cond_exec = 0;

  /* Scan all of the blocks for registers that must not be allocated.  */
  for (j = 0; j < num_bb; j++)
    {
      rtx last_insn = BB_END (bb[j]);
      rtx insn = BB_HEAD (bb[j]);
      int regno;

      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Scanning %s block %d, start %d, end %d\n",
		 (bb[j] == else_bb) ? "else" : ((bb[j] == then_bb) ? "then" : "test"),
		 (int) bb[j]->index,
		 (int) INSN_UID (BB_HEAD (bb[j])),
		 (int) INSN_UID (BB_END (bb[j])));

      /* Anything live at the beginning of the block is obviously unavailable
         for allocation.  */
      EXECUTE_IF_SET_IN_REG_SET (bb[j]->global_live_at_start, 0, regno,
				 {
				   if (regno < FIRST_PSEUDO_REGISTER)
				     CLEAR_HARD_REG_BIT (tmp_reg->regs, regno);
				 });

      /* Loop through the insns in the block.  */
      for (;;)
	{
	  /* Mark any new registers that are created as being unavailable for
             allocation.  Also see if the CC register used in nested IFs can be
             reallocated.  */
	  if (INSN_P (insn))
	    {
	      rtx pattern;
	      rtx set;
	      int skip_nested_if = FALSE;

	      for_each_rtx (&PATTERN (insn), frv_clear_registers_used,
			    (void *)&tmp_reg->regs);

	      pattern = PATTERN (insn);
	      if (GET_CODE (pattern) == COND_EXEC)
		{
		  rtx reg = XEXP (COND_EXEC_TEST (pattern), 0);

		  if (reg != sub_cond_exec_reg)
		    {
		      sub_cond_exec_reg = reg;
		      frv_ifcvt.num_nested_cond_exec++;
		    }
		}

	      set = single_set_pattern (pattern);
	      if (set)
		{
		  rtx dest = SET_DEST (set);
		  rtx src = SET_SRC (set);

		  if (GET_CODE (dest) == REG)
		    {
		      int regno = REGNO (dest);
		      enum rtx_code src_code = GET_CODE (src);

		      if (CC_P (regno) && src_code == COMPARE)
			skip_nested_if = TRUE;

		      else if (CR_P (regno)
			       && (src_code == IF_THEN_ELSE
				   || GET_RTX_CLASS (src_code) == '<'))
			skip_nested_if = TRUE;
		    }
		}

	      if (! skip_nested_if)
		for_each_rtx (&PATTERN (insn), frv_clear_registers_used,
			      (void *)&frv_ifcvt.nested_cc_ok_rewrite);
	    }

	  if (insn == last_insn)
	    break;

	  insn = NEXT_INSN (insn);
	}
    }

  /* If this is a nested if, rewrite the CC registers that are available to
     include the ones that can be rewritten, to increase the chance of being
     able to allocate a paired CC/CR register combination.  */
  if (ce_info->pass > 1)
    {
      for (j = CC_FIRST; j <= CC_LAST; j++)
	if (TEST_HARD_REG_BIT (frv_ifcvt.nested_cc_ok_rewrite, j))
	  SET_HARD_REG_BIT (tmp_reg->regs, j);
	else
	  CLEAR_HARD_REG_BIT (tmp_reg->regs, j);
    }

  if (rtl_dump_file)
    {
      int num_gprs = 0;
      fprintf (rtl_dump_file, "Available GPRs: ");

      for (j = GPR_FIRST; j <= GPR_LAST; j++)
	if (TEST_HARD_REG_BIT (tmp_reg->regs, j))
	  {
	    fprintf (rtl_dump_file, " %d [%s]", j, reg_names[j]);
	    if (++num_gprs > GPR_TEMP_NUM+2)
	      break;
	  }

      fprintf (rtl_dump_file, "%s\nAvailable CRs:  ",
	       (num_gprs > GPR_TEMP_NUM+2) ? " ..." : "");

      for (j = CR_FIRST; j <= CR_LAST; j++)
	if (TEST_HARD_REG_BIT (tmp_reg->regs, j))
	  fprintf (rtl_dump_file, " %d [%s]", j, reg_names[j]);

      fputs ("\n", rtl_dump_file);

      if (ce_info->pass > 1)
	{
	  fprintf (rtl_dump_file, "Modifiable CCs: ");
	  for (j = CC_FIRST; j <= CC_LAST; j++)
	    if (TEST_HARD_REG_BIT (tmp_reg->regs, j))
	      fprintf (rtl_dump_file, " %d [%s]", j, reg_names[j]);

	  fprintf (rtl_dump_file, "\n%d nested COND_EXEC statements\n",
		   frv_ifcvt.num_nested_cond_exec);
	}
    }

  /* Allocate the appropriate temporary condition code register.  Try to
     allocate the ICR/FCR register that corresponds to the ICC/FCC register so
     that conditional cmp's can be done.  */
  if (mode == CCmode || mode == CC_UNSmode)
    {
      cr_class = ICR_REGS;
      cc_class = ICC_REGS;
      cc_first = ICC_FIRST;
      cc_last = ICC_LAST;
    }
  else if (mode == CC_FPmode)
    {
      cr_class = FCR_REGS;
      cc_class = FCC_REGS;
      cc_first = FCC_FIRST;
      cc_last = FCC_LAST;
    }
  else
    {
      cc_first = cc_last = 0;
      cr_class = cc_class = NO_REGS;
    }

  cc = XEXP (true_expr, 0);
  nested_cc = cr = NULL_RTX;
  if (cc_class != NO_REGS)
    {
      /* For nested IFs and &&/||, see if we can find a CC and CR register pair
         so we can execute a csubcc/caddcc/cfcmps instruction.  */
      int cc_regno;

      for (cc_regno = cc_first; cc_regno <= cc_last; cc_regno++)
	{
	  int cr_regno = cc_regno - CC_FIRST + CR_FIRST;

	  if (TEST_HARD_REG_BIT (frv_ifcvt.tmp_reg.regs, cc_regno)
	      && TEST_HARD_REG_BIT (frv_ifcvt.tmp_reg.regs, cr_regno))
	    {
	      frv_ifcvt.tmp_reg.next_reg[ (int)cr_class ] = cr_regno;
	      cr = frv_alloc_temp_reg (tmp_reg, cr_class, CC_CCRmode, TRUE,
				       TRUE);

	      frv_ifcvt.tmp_reg.next_reg[ (int)cc_class ] = cc_regno;
	      nested_cc = frv_alloc_temp_reg (tmp_reg, cc_class, CCmode,
						  TRUE, TRUE);
	      break;
	    }
	}
    }

  if (! cr)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Could not allocate a CR temporary register\n");

      goto fail;
    }

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     "Will use %s for conditional execution, %s for nested comparisons\n",
	     reg_names[ REGNO (cr)],
	     (nested_cc) ? reg_names[ REGNO (nested_cc) ] : "<none>");

  /* Set the CCR bit.  Note for integer tests, we reverse the condition so that
     in an IF-THEN-ELSE sequence, we are testing the TRUE case against the CCR
     bit being true.  We don't do this for floating point, because of NaNs.  */
  code = GET_CODE (true_expr);
  if (GET_MODE (cc) != CC_FPmode)
    {
      code = reverse_condition (code);
      code_true = EQ;
      code_false = NE;
    }
  else
    {
      code_true = NE;
      code_false = EQ;
    }

  check_insn = gen_rtx_SET (VOIDmode, cr,
			    gen_rtx_fmt_ee (code, CC_CCRmode, cc, const0_rtx));

  /* Record the check insn to be inserted later.  */
  frv_ifcvt_add_insn (check_insn, BB_END (test_bb), TRUE);

  /* Update the tests.  */
  frv_ifcvt.cr_reg = cr;
  frv_ifcvt.nested_cc_reg = nested_cc;
  *p_true = gen_rtx_fmt_ee (code_true, CC_CCRmode, cr, const0_rtx);
  *p_false = gen_rtx_fmt_ee (code_false, CC_CCRmode, cr, const0_rtx);
  return;

  /* Fail, don't do this conditional execution.  */
 fail:
  *p_true = NULL_RTX;
  *p_false = NULL_RTX;
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Disabling this conditional execution.\n");

  return;
}


/* A C expression to modify the code described by the conditional if
   information CE_INFO, for the basic block BB, possibly updating the tests in
   TRUE_EXPR, and FALSE_EXPR for converting the && and || parts of if-then or
   if-then-else code to conditional instructions.  Set either TRUE_EXPR or
   FALSE_EXPR to a null pointer if the tests cannot be converted.  */

/* p_true and p_false are given expressions of the form:

	(and (eq:CC_CCR (reg:CC_CCR)
			(const_int 0))
	     (eq:CC (reg:CC)
		    (const_int 0))) */

void
frv_ifcvt_modify_multiple_tests (ce_if_block_t *ce_info,
                                 basic_block bb,
                                 rtx *p_true,
                                 rtx *p_false)
{
  rtx old_true = XEXP (*p_true, 0);
  rtx old_false = XEXP (*p_false, 0);
  rtx true_expr = XEXP (*p_true, 1);
  rtx false_expr = XEXP (*p_false, 1);
  rtx test_expr;
  rtx old_test;
  rtx cr = XEXP (old_true, 0);
  rtx check_insn;
  rtx new_cr = NULL_RTX;
  rtx *p_new_cr = (rtx *)0;
  rtx if_else;
  rtx compare;
  rtx cc;
  enum reg_class cr_class;
  enum machine_mode mode = GET_MODE (true_expr);
  rtx (*logical_func)(rtx, rtx, rtx);

  if (TARGET_DEBUG_COND_EXEC)
    {
      fprintf (stderr,
	       "\n:::::::::: frv_ifcvt_modify_multiple_tests, before modification for %s\ntrue insn:\n",
	       ce_info->and_and_p ? "&&" : "||");

      debug_rtx (*p_true);

      fputs ("\nfalse insn:\n", stderr);
      debug_rtx (*p_false);
    }

  if (TARGET_NO_MULTI_CE)
    goto fail;

  if (GET_CODE (cr) != REG)
    goto fail;

  if (mode == CCmode || mode == CC_UNSmode)
    {
      cr_class = ICR_REGS;
      p_new_cr = &frv_ifcvt.extra_int_cr;
    }
  else if (mode == CC_FPmode)
    {
      cr_class = FCR_REGS;
      p_new_cr = &frv_ifcvt.extra_fp_cr;
    }
  else
    goto fail;

  /* Allocate a temp CR, reusing a previously allocated temp CR if we have 3 or
     more &&/|| tests.  */
  new_cr = *p_new_cr;
  if (! new_cr)
    {
      new_cr = *p_new_cr = frv_alloc_temp_reg (&frv_ifcvt.tmp_reg, cr_class,
					       CC_CCRmode, TRUE, TRUE);
      if (! new_cr)
	goto fail;
    }

  if (ce_info->and_and_p)
    {
      old_test = old_false;
      test_expr = true_expr;
      logical_func = (GET_CODE (old_true) == EQ) ? gen_andcr : gen_andncr;
      *p_true = gen_rtx_NE (CC_CCRmode, cr, const0_rtx);
      *p_false = gen_rtx_EQ (CC_CCRmode, cr, const0_rtx);
    }
  else
    {
      old_test = old_false;
      test_expr = false_expr;
      logical_func = (GET_CODE (old_false) == EQ) ? gen_orcr : gen_orncr;
      *p_true = gen_rtx_EQ (CC_CCRmode, cr, const0_rtx);
      *p_false = gen_rtx_NE (CC_CCRmode, cr, const0_rtx);
    }

  /* First add the andcr/andncr/orcr/orncr, which will be added after the
     conditional check instruction, due to frv_ifcvt_add_insn being a LIFO
     stack.  */
  frv_ifcvt_add_insn ((*logical_func) (cr, cr, new_cr), BB_END (bb), TRUE);

  /* Now add the conditional check insn.  */
  cc = XEXP (test_expr, 0);
  compare = gen_rtx_fmt_ee (GET_CODE (test_expr), CC_CCRmode, cc, const0_rtx);
  if_else = gen_rtx_IF_THEN_ELSE (CC_CCRmode, old_test, compare, const0_rtx);

  check_insn = gen_rtx_SET (VOIDmode, new_cr, if_else);

  /* Add the new check insn to the list of check insns that need to be
     inserted.  */
  frv_ifcvt_add_insn (check_insn, BB_END (bb), TRUE);

  if (TARGET_DEBUG_COND_EXEC)
    {
      fputs ("\n:::::::::: frv_ifcvt_modify_multiple_tests, after modification\ntrue insn:\n",
	     stderr);

      debug_rtx (*p_true);

      fputs ("\nfalse insn:\n", stderr);
      debug_rtx (*p_false);
    }

  return;

 fail:
  *p_true = *p_false = NULL_RTX;

  /* If we allocated a CR register, release it.  */
  if (new_cr)
    {
      CLEAR_HARD_REG_BIT (frv_ifcvt.tmp_reg.regs, REGNO (new_cr));
      *p_new_cr = NULL_RTX;
    }

  if (TARGET_DEBUG_COND_EXEC)
    fputs ("\n:::::::::: frv_ifcvt_modify_multiple_tests, failed.\n", stderr);

  return;
}


/* Return a register which will be loaded with a value if an IF block is
   converted to conditional execution.  This is used to rewrite instructions
   that use constants to ones that just use registers.  */

static rtx
frv_ifcvt_load_value (rtx value, rtx insn ATTRIBUTE_UNUSED)
{
  int num_alloc = frv_ifcvt.cur_scratch_regs;
  int i;
  rtx reg;

  /* We know gr0 == 0, so replace any errant uses.  */
  if (value == const0_rtx)
    return gen_rtx_REG (SImode, GPR_FIRST);

  /* First search all registers currently loaded to see if we have an
     applicable constant.  */
  if (CONSTANT_P (value)
      || (GET_CODE (value) == REG && REGNO (value) == LR_REGNO))
    {
      for (i = 0; i < num_alloc; i++)
	{
	  if (rtx_equal_p (SET_SRC (frv_ifcvt.scratch_regs[i]), value))
	    return SET_DEST (frv_ifcvt.scratch_regs[i]);
	}
    }

  /* Have we exhausted the number of registers available?  */
  if (num_alloc >= GPR_TEMP_NUM)
    {
      if (rtl_dump_file)
	fprintf (rtl_dump_file, "Too many temporary registers allocated\n");

      return NULL_RTX;
    }

  /* Allocate the new register.  */
  reg = frv_alloc_temp_reg (&frv_ifcvt.tmp_reg, GPR_REGS, SImode, TRUE, TRUE);
  if (! reg)
    {
      if (rtl_dump_file)
	fputs ("Could not find a scratch register\n", rtl_dump_file);

      return NULL_RTX;
    }

  frv_ifcvt.cur_scratch_regs++;
  frv_ifcvt.scratch_regs[num_alloc] = gen_rtx_SET (VOIDmode, reg, value);

  if (rtl_dump_file)
    {
      if (GET_CODE (value) == CONST_INT)
	fprintf (rtl_dump_file, "Register %s will hold %ld\n",
		 reg_names[ REGNO (reg)], (long)INTVAL (value));

      else if (GET_CODE (value) == REG && REGNO (value) == LR_REGNO)
	fprintf (rtl_dump_file, "Register %s will hold LR\n",
		 reg_names[ REGNO (reg)]);

      else
	fprintf (rtl_dump_file, "Register %s will hold a saved value\n",
		 reg_names[ REGNO (reg)]);
    }

  return reg;
}


/* Update a MEM used in conditional code that might contain an offset to put
   the offset into a scratch register, so that the conditional load/store
   operations can be used.  This function returns the original pointer if the
   MEM is valid to use in conditional code, NULL if we can't load up the offset
   into a temporary register, or the new MEM if we were successful.  */

static rtx
frv_ifcvt_rewrite_mem (rtx mem, enum machine_mode mode, rtx insn)
{
  rtx addr = XEXP (mem, 0);

  if (!frv_legitimate_address_p (mode, addr, reload_completed, TRUE))
    {
      if (GET_CODE (addr) == PLUS)
	{
	  rtx addr_op0 = XEXP (addr, 0);
	  rtx addr_op1 = XEXP (addr, 1);

	  if (plus_small_data_p (addr_op0, addr_op1))
	    addr = frv_ifcvt_load_value (addr, insn);

	  else if (GET_CODE (addr_op0) == REG && CONSTANT_P (addr_op1))
	    {
	      rtx reg = frv_ifcvt_load_value (addr_op1, insn);
	      if (!reg)
		return NULL_RTX;

	      addr = gen_rtx_PLUS (Pmode, addr_op0, reg);
	    }

	  else
	    return NULL_RTX;
	}

      else if (CONSTANT_P (addr))
	addr = frv_ifcvt_load_value (addr, insn);

      else
	return NULL_RTX;

      if (addr == NULL_RTX)
	return NULL_RTX;

      else if (XEXP (mem, 0) != addr)
	return change_address (mem, mode, addr);
    }

  return mem;
}


/* Given a PATTERN, return a SET expression if this PATTERN has only a single
   SET, possibly conditionally executed.  It may also have CLOBBERs, USEs.  */

static rtx
single_set_pattern (rtx pattern)
{
  rtx set;
  int i;

  if (GET_CODE (pattern) == COND_EXEC)
    pattern = COND_EXEC_CODE (pattern);

  if (GET_CODE (pattern) == SET)
    return pattern;

  else if (GET_CODE (pattern) == PARALLEL)
    {
      for (i = 0, set = 0; i < XVECLEN (pattern, 0); i++)
	{
	  rtx sub = XVECEXP (pattern, 0, i);

	  switch (GET_CODE (sub))
	    {
	    case USE:
	    case CLOBBER:
	      break;

	    case SET:
	      if (set)
		return 0;
	      else
		set = sub;
	      break;

	    default:
	      return 0;
	    }
	}
      return set;
    }

  return 0;
}


/* A C expression to modify the code described by the conditional if
   information CE_INFO with the new PATTERN in INSN.  If PATTERN is a null
   pointer after the IFCVT_MODIFY_INSN macro executes, it is assumed that that
   insn cannot be converted to be executed conditionally.  */

rtx
frv_ifcvt_modify_insn (ce_if_block_t *ce_info,
                       rtx pattern,
                       rtx insn)
{
  rtx orig_ce_pattern = pattern;
  rtx set;
  rtx op0;
  rtx op1;
  rtx test;

  if (GET_CODE (pattern) != COND_EXEC)
    abort ();

  test = COND_EXEC_TEST (pattern);
  if (GET_CODE (test) == AND)
    {
      rtx cr = frv_ifcvt.cr_reg;
      rtx test_reg;

      op0 = XEXP (test, 0);
      if (! rtx_equal_p (cr, XEXP (op0, 0)))
	goto fail;

      op1 = XEXP (test, 1);
      test_reg = XEXP (op1, 0);
      if (GET_CODE (test_reg) != REG)
	goto fail;

      /* Is this the first nested if block in this sequence?  If so, generate
         an andcr or andncr.  */
      if (! frv_ifcvt.last_nested_if_cr)
	{
	  rtx and_op;

	  frv_ifcvt.last_nested_if_cr = test_reg;
	  if (GET_CODE (op0) == NE)
	    and_op = gen_andcr (test_reg, cr, test_reg);
	  else
	    and_op = gen_andncr (test_reg, cr, test_reg);

	  frv_ifcvt_add_insn (and_op, insn, TRUE);
	}

      /* If this isn't the first statement in the nested if sequence, see if we
         are dealing with the same register.  */
      else if (! rtx_equal_p (test_reg, frv_ifcvt.last_nested_if_cr))
	goto fail;

      COND_EXEC_TEST (pattern) = test = op1;
    }

  /* If this isn't a nested if, reset state variables.  */
  else
    {
      frv_ifcvt.last_nested_if_cr = NULL_RTX;
    }

  set = single_set_pattern (pattern);
  if (set)
    {
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);
      enum machine_mode mode = GET_MODE (dest);

      /* Check for normal binary operators.  */
      if (mode == SImode
	  && (GET_RTX_CLASS (GET_CODE (src)) == '2'
	      || GET_RTX_CLASS (GET_CODE (src)) == 'c'))
	{
	  op0 = XEXP (src, 0);
	  op1 = XEXP (src, 1);

	  /* Special case load of small data address which looks like:
	     r16+symbol_ref */
	  if (GET_CODE (src) == PLUS && plus_small_data_p (op0, op1))
	    {
	      src = frv_ifcvt_load_value (src, insn);
	      if (src)
		COND_EXEC_CODE (pattern) = gen_rtx_SET (VOIDmode, dest, src);
	      else
		goto fail;
	    }

	  else if (integer_register_operand (op0, SImode) && CONSTANT_P (op1))
	    {
	      op1 = frv_ifcvt_load_value (op1, insn);
	      if (op1)
		COND_EXEC_CODE (pattern)
		  = gen_rtx_SET (VOIDmode, dest, gen_rtx_fmt_ee (GET_CODE (src),
								 GET_MODE (src),
								 op0, op1));
	      else
		goto fail;
	    }
	}

      /* For multiply by a constant, we need to handle the sign extending
         correctly.  Add a USE of the value after the multiply to prevent flow
         from cratering because only one register out of the two were used.  */
      else if (mode == DImode && GET_CODE (src) == MULT)
	{
	  op0 = XEXP (src, 0);
	  op1 = XEXP (src, 1);
	  if (GET_CODE (op0) == SIGN_EXTEND && GET_CODE (op1) == CONST_INT)
	    {
	      op1 = frv_ifcvt_load_value (op1, insn);
	      if (op1)
		{
		  op1 = gen_rtx_SIGN_EXTEND (DImode, op1);
		  COND_EXEC_CODE (pattern)
		    = gen_rtx_SET (VOIDmode, dest,
				   gen_rtx_MULT (DImode, op0, op1));
		}
	      else
		goto fail;
	    }

	  frv_ifcvt_add_insn (gen_rtx_USE (VOIDmode, dest), insn, FALSE);
	}

      /* If we are just loading a constant created for a nested conditional
         execution statement, just load the constant without any conditional
         execution, since we know that the constant will not interfere with any
         other registers.  */
      else if (frv_ifcvt.scratch_insns_bitmap
	       && bitmap_bit_p (frv_ifcvt.scratch_insns_bitmap,
				INSN_UID (insn))
	       /* We must not unconditionally set a reg set used as
		  scratch in the THEN branch if the same reg is live
		  in the ELSE branch.  */
	       && REG_P (SET_DEST (set))
	       && (! ce_info->else_bb
		   || BLOCK_FOR_INSN (insn) == ce_info->else_bb
		   || ! (REGNO_REG_SET_P
			 (ce_info->else_bb->global_live_at_start,
			  REGNO (SET_DEST (set))))))
	pattern = set;

      else if (mode == QImode || mode == HImode || mode == SImode
	       || mode == SFmode)
	{
	  int changed_p = FALSE;

	  /* Check for just loading up a constant */
	  if (CONSTANT_P (src) && integer_register_operand (dest, mode))
	    {
	      src = frv_ifcvt_load_value (src, insn);
	      if (!src)
		goto fail;

	      changed_p = TRUE;
	    }

	  /* See if we need to fix up stores */
	  if (GET_CODE (dest) == MEM)
	    {
	      rtx new_mem = frv_ifcvt_rewrite_mem (dest, mode, insn);

	      if (!new_mem)
		goto fail;

	      else if (new_mem != dest)
		{
		  changed_p = TRUE;
		  dest = new_mem;
		}
	    }

	  /* See if we need to fix up loads */
	  if (GET_CODE (src) == MEM)
	    {
	      rtx new_mem = frv_ifcvt_rewrite_mem (src, mode, insn);

	      if (!new_mem)
		goto fail;

	      else if (new_mem != src)
		{
		  changed_p = TRUE;
		  src = new_mem;
		}
	    }

	  /* If either src or destination changed, redo SET.  */
	  if (changed_p)
	    COND_EXEC_CODE (pattern) = gen_rtx_SET (VOIDmode, dest, src);
	}

      /* Rewrite a nested set cccr in terms of IF_THEN_ELSE.  Also deal with
         rewriting the CC register to be the same as the paired CC/CR register
         for nested ifs.  */
      else if (mode == CC_CCRmode && GET_RTX_CLASS (GET_CODE (src)) == '<')
	{
	  int regno = REGNO (XEXP (src, 0));
	  rtx if_else;

	  if (ce_info->pass > 1
	      && regno != (int)REGNO (frv_ifcvt.nested_cc_reg)
	      && TEST_HARD_REG_BIT (frv_ifcvt.nested_cc_ok_rewrite, regno))
	    {
	      src = gen_rtx_fmt_ee (GET_CODE (src),
				    CC_CCRmode,
				    frv_ifcvt.nested_cc_reg,
				    XEXP (src, 1));
	    }

	  if_else = gen_rtx_IF_THEN_ELSE (CC_CCRmode, test, src, const0_rtx);
	  pattern = gen_rtx_SET (VOIDmode, dest, if_else);
	}

      /* Remap a nested compare instruction to use the paired CC/CR reg.  */
      else if (ce_info->pass > 1
	       && GET_CODE (dest) == REG
	       && CC_P (REGNO (dest))
	       && REGNO (dest) != REGNO (frv_ifcvt.nested_cc_reg)
	       && TEST_HARD_REG_BIT (frv_ifcvt.nested_cc_ok_rewrite,
				     REGNO (dest))
	       && GET_CODE (src) == COMPARE)
	{
	  PUT_MODE (frv_ifcvt.nested_cc_reg, GET_MODE (dest));
	  COND_EXEC_CODE (pattern)
	    = gen_rtx_SET (VOIDmode, frv_ifcvt.nested_cc_reg, copy_rtx (src));
	}
    }

  if (TARGET_DEBUG_COND_EXEC)
    {
      rtx orig_pattern = PATTERN (insn);

      PATTERN (insn) = pattern;
      fprintf (stderr,
	       "\n:::::::::: frv_ifcvt_modify_insn: pass = %d, insn after modification:\n",
	       ce_info->pass);

      debug_rtx (insn);
      PATTERN (insn) = orig_pattern;
    }

  return pattern;

 fail:
  if (TARGET_DEBUG_COND_EXEC)
    {
      rtx orig_pattern = PATTERN (insn);

      PATTERN (insn) = orig_ce_pattern;
      fprintf (stderr,
	       "\n:::::::::: frv_ifcvt_modify_insn: pass = %d, insn could not be modified:\n",
	       ce_info->pass);

      debug_rtx (insn);
      PATTERN (insn) = orig_pattern;
    }

  return NULL_RTX;
}


/* A C expression to perform any final machine dependent modifications in
   converting code to conditional execution in the code described by the
   conditional if information CE_INFO.  */

void
frv_ifcvt_modify_final (ce_if_block_t *ce_info ATTRIBUTE_UNUSED)
{
  rtx existing_insn;
  rtx check_insn;
  rtx p = frv_ifcvt.added_insns_list;
  int i;

  /* Loop inserting the check insns.  The last check insn is the first test,
     and is the appropriate place to insert constants.  */
  if (! p)
    abort ();

  do
    {
      rtx check_and_insert_insns = XEXP (p, 0);
      rtx old_p = p;

      check_insn = XEXP (check_and_insert_insns, 0);
      existing_insn = XEXP (check_and_insert_insns, 1);
      p = XEXP (p, 1);

      /* The jump bit is used to say that the new insn is to be inserted BEFORE
         the existing insn, otherwise it is to be inserted AFTER.  */
      if (check_and_insert_insns->jump)
	{
	  emit_insn_before (check_insn, existing_insn);
	  check_and_insert_insns->jump = 0;
	}
      else
	emit_insn_after (check_insn, existing_insn);

      free_EXPR_LIST_node (check_and_insert_insns);
      free_EXPR_LIST_node (old_p);
    }
  while (p != NULL_RTX);

  /* Load up any constants needed into temp gprs */
  for (i = 0; i < frv_ifcvt.cur_scratch_regs; i++)
    {
      rtx insn = emit_insn_before (frv_ifcvt.scratch_regs[i], existing_insn);
      if (! frv_ifcvt.scratch_insns_bitmap)
	frv_ifcvt.scratch_insns_bitmap = BITMAP_XMALLOC ();
      bitmap_set_bit (frv_ifcvt.scratch_insns_bitmap, INSN_UID (insn));
      frv_ifcvt.scratch_regs[i] = NULL_RTX;
    }

  frv_ifcvt.added_insns_list = NULL_RTX;
  frv_ifcvt.cur_scratch_regs = 0;
}


/* A C expression to cancel any machine dependent modifications in converting
   code to conditional execution in the code described by the conditional if
   information CE_INFO.  */

void
frv_ifcvt_modify_cancel (ce_if_block_t *ce_info ATTRIBUTE_UNUSED)
{
  int i;
  rtx p = frv_ifcvt.added_insns_list;

  /* Loop freeing up the EXPR_LIST's allocated.  */
  while (p != NULL_RTX)
    {
      rtx check_and_jump = XEXP (p, 0);
      rtx old_p = p;

      p = XEXP (p, 1);
      free_EXPR_LIST_node (check_and_jump);
      free_EXPR_LIST_node (old_p);
    }

  /* Release any temporary gprs allocated.  */
  for (i = 0; i < frv_ifcvt.cur_scratch_regs; i++)
    frv_ifcvt.scratch_regs[i] = NULL_RTX;

  frv_ifcvt.added_insns_list = NULL_RTX;
  frv_ifcvt.cur_scratch_regs = 0;
  return;
}

/* A C expression for the size in bytes of the trampoline, as an integer.
   The template is:

	setlo #0, <jmp_reg>
	setlo #0, <static_chain>
	sethi #0, <jmp_reg>
	sethi #0, <static_chain>
	jmpl @(gr0,<jmp_reg>) */

int
frv_trampoline_size (void)
{
  return 5 /* instructions */ * 4 /* instruction size */;
}


/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.

   The template is:

	setlo #0, <jmp_reg>
	setlo #0, <static_chain>
	sethi #0, <jmp_reg>
	sethi #0, <static_chain>
	jmpl @(gr0,<jmp_reg>) */

void
frv_initialize_trampoline (rtx addr, rtx fnaddr, rtx static_chain)
{
  rtx sc_reg = force_reg (Pmode, static_chain);

  emit_library_call (gen_rtx_SYMBOL_REF (SImode, "__trampoline_setup"),
		     FALSE, VOIDmode, 4,
		     addr, Pmode,
		     GEN_INT (frv_trampoline_size ()), SImode,
		     fnaddr, Pmode,
		     sc_reg, Pmode);
}


/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the SPARC when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of CLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required..  These patterns, which will normally be implemented with a
   `define_expand', should be similar to the `movM' patterns, except that
   operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   CLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

enum reg_class
frv_secondary_reload_class (enum reg_class class,
                            enum machine_mode mode ATTRIBUTE_UNUSED,
                            rtx x,
                            int in_p ATTRIBUTE_UNUSED)
{
  enum reg_class ret;

  switch (class)
    {
    default:
      ret = NO_REGS;
      break;

      /* Accumulators/Accumulator guard registers need to go through floating
         point registers.  */
    case QUAD_REGS:
    case EVEN_REGS:
    case GPR_REGS:
      ret = NO_REGS;
      if (x && GET_CODE (x) == REG)
	{
	  int regno = REGNO (x);

	  if (ACC_P (regno) || ACCG_P (regno))
	    ret = FPR_REGS;
	}
      break;

      /* Nonzero constants should be loaded into an FPR through a GPR.  */
    case QUAD_FPR_REGS:
    case FEVEN_REGS:
    case FPR_REGS:
      if (x && CONSTANT_P (x) && !ZERO_P (x))
	ret = GPR_REGS;
      else
	ret = NO_REGS;
      break;

      /* All of these types need gpr registers.  */
    case ICC_REGS:
    case FCC_REGS:
    case CC_REGS:
    case ICR_REGS:
    case FCR_REGS:
    case CR_REGS:
    case LCR_REG:
    case LR_REG:
      ret = GPR_REGS;
      break;

      /* The accumulators need fpr registers */
    case ACC_REGS:
    case EVEN_ACC_REGS:
    case QUAD_ACC_REGS:
    case ACCG_REGS:
      ret = FPR_REGS;
      break;
    }

  return ret;
}


/* A C expression whose value is nonzero if pseudos that have been assigned to
   registers of class CLASS would likely be spilled because registers of CLASS
   are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one register
   and zero otherwise.  On most machines, this default should be used.  Only
   define this macro to some other expression if pseudo allocated by
   `local-alloc.c' end up in memory because their hard registers were needed
   for spill registers.  If this macro returns nonzero for those classes, those
   pseudos will only be allocated by `global.c', which knows how to reallocate
   the pseudo to another register.  If there would not be another register
   available for reallocation, you should not change the definition of this
   macro since the only effect of such a definition would be to slow down
   register allocation.  */

int
frv_class_likely_spilled_p (enum reg_class class)
{
  switch (class)
    {
    default:
      break;

    case ICC_REGS:
    case FCC_REGS:
    case CC_REGS:
    case ICR_REGS:
    case FCR_REGS:
    case CR_REGS:
    case LCR_REG:
    case LR_REG:
    case SPR_REGS:
    case QUAD_ACC_REGS:
    case EVEN_ACC_REGS:
    case ACC_REGS:
    case ACCG_REGS:
      return TRUE;
    }

  return FALSE;
}


/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way is COMPUTED.  GCC uses this
   value instead of the value in `BIGGEST_ALIGNMENT' or
   `BIGGEST_FIELD_ALIGNMENT', if defined, for structure fields only.  */

/* The definition type of the bit field data is either char, short, long or
   long long. The maximum bit size is the number of bits of its own type.

   The bit field data is assigned to a storage unit that has an adequate size
   for bit field data retention and is located at the smallest address.

   Consecutive bit field data are packed at consecutive bits having the same
   storage unit, with regard to the type, beginning with the MSB and continuing
   toward the LSB.

   If a field to be assigned lies over a bit field type boundary, its
   assignment is completed by aligning it with a boundary suitable for the
   type.

   When a bit field having a bit length of 0 is declared, it is forcibly
   assigned to the next storage unit.

   e.g)
	struct {
		int	a:2;
		int	b:6;
		char	c:4;
		int	d:10;
		int	 :0;
		int	f:2;
	} x;

		+0	  +1	    +2	      +3
	&x	00000000  00000000  00000000  00000000
		MLM----L
		a    b
	&x+4	00000000  00000000  00000000  00000000
		M--L
		c
	&x+8	00000000  00000000  00000000  00000000
		M----------L
		d
	&x+12	00000000  00000000  00000000  00000000
		ML
		f
*/

int
frv_adjust_field_align (tree field, int computed)
{
  /* Make sure that the bitfield is not wider than the type.  */
  if (DECL_BIT_FIELD (field)
      && !DECL_ARTIFICIAL (field))
    {
      tree parent = DECL_CONTEXT (field);
      tree prev = NULL_TREE;
      tree cur;

      for (cur = TYPE_FIELDS (parent); cur && cur != field; cur = TREE_CHAIN (cur))
	{
	  if (TREE_CODE (cur) != FIELD_DECL)
	    continue;

	  prev = cur;
	}

      if (!cur)
	abort ();

      /* If this isn't a :0 field and if the previous element is a bitfield
	 also, see if the type is different, if so, we will need to align the
	 bit-field to the next boundary.  */
      if (prev
	  && ! DECL_PACKED (field)
	  && ! integer_zerop (DECL_SIZE (field))
	  && DECL_BIT_FIELD_TYPE (field) != DECL_BIT_FIELD_TYPE (prev))
	{
	  int prev_align = TYPE_ALIGN (TREE_TYPE (prev));
	  int cur_align  = TYPE_ALIGN (TREE_TYPE (field));
	  computed = (prev_align > cur_align) ? prev_align : cur_align;
	}
    }

  return computed;
}


/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  For a machine where all registers are equivalent, a suitable
   definition is

        #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of fixed
   registers, because the allocation mechanism considers them to be always
   occupied.

   On some machines, double-precision values must be kept in even/odd register
   pairs.  The way to implement that is to define this macro to reject odd
   register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that the
   `movMODE' instruction pattern support moves between the register and any
   other hard register for which the mode is OK; and that moving a value into
   the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all narrower
   integer modes, it is not necessary on any machine for `HARD_REGNO_MODE_OK'
   to distinguish between these modes, provided you define patterns `movhi',
   etc., to take advantage of this.  This is useful because of the interaction
   between `HARD_REGNO_MODE_OK' and `MODES_TIEABLE_P'; it is very desirable for
   all integer modes to be tieable.

   Many machines have special registers for floating point arithmetic.  Often
   people assume that floating point machine modes are allowed only in floating
   point registers.  This is not true.  Any registers that can hold integers
   can safely *hold* a floating point machine mode, whether or not floating
   arithmetic can be done on it in those registers.  Integer move instructions
   can be used to move the values.

   On some machines, though, the converse is true: fixed-point machine modes
   may not go in floating registers.  This is true if the floating registers
   normalize any value stored in them, because storing a non-floating value
   there would garble it.  In this case, `HARD_REGNO_MODE_OK' should reject
   fixed-point machine modes in floating registers.  But if the floating
   registers do not automatically normalize, if you can store any bit pattern
   in one and retrieve it unchanged without a trap, then any machine mode may
   go in a floating register, so you can define this macro to say so.

   The primary significance of special floating registers is rather that they
   are the registers acceptable in floating point arithmetic instructions.
   However, this is of no concern to `HARD_REGNO_MODE_OK'.  You handle it by
   writing the proper constraints for those instructions.

   On some machines, the floating registers are especially slow to access, so
   that it is better to store a value in a stack frame than in such a register
   if floating point arithmetic is not being done.  As long as the floating
   registers are not in class `GENERAL_REGS', they will not be used unless some
   pattern's constraint asks for one.  */

int
frv_hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  int base;
  int mask;

  switch (mode)
    {
    case CCmode:
    case CC_UNSmode:
      return ICC_P (regno) || GPR_P (regno);

    case CC_CCRmode:
      return CR_P (regno) || GPR_P (regno);

    case CC_FPmode:
      return FCC_P (regno) || GPR_P (regno);

    default:
      break;
    }

  /* Set BASE to the first register in REGNO's class.  Set MASK to the
     bits that must be clear in (REGNO - BASE) for the register to be
     well-aligned.  */
  if (INTEGRAL_MODE_P (mode) || FLOAT_MODE_P (mode) || VECTOR_MODE_P (mode))
    {
      if (ACCG_P (regno))
	{
	  /* ACCGs store one byte.  Two-byte quantities must start in
	     even-numbered registers, four-byte ones in registers whose
	     numbers are divisible by four, and so on.  */
	  base = ACCG_FIRST;
	  mask = GET_MODE_SIZE (mode) - 1;
	}
      else
	{
	   /* The other registers store one word.  */
	  if (GPR_P (regno) || regno == AP_FIRST)
	    base = GPR_FIRST;

	  else if (FPR_P (regno))
	    base = FPR_FIRST;

	  else if (ACC_P (regno))
	    base = ACC_FIRST;

	  else if (SPR_P (regno))
	    return mode == SImode;

	  /* Fill in the table.  */
	  else
	    return 0;

	  /* Anything smaller than an SI is OK in any word-sized register.  */
	  if (GET_MODE_SIZE (mode) < 4)
	    return 1;

	  mask = (GET_MODE_SIZE (mode) / 4) - 1;
	}
      return (((regno - base) & mask) == 0);
    }

  return 0;
}


/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */

/* On the FRV, make the CC_FP mode take 3 words in the integer registers, so
   that we can build the appropriate instructions to properly reload the
   values.  Also, make the byte-sized accumulator guards use one guard
   for each byte.  */

int
frv_hard_regno_nregs (int regno, enum machine_mode mode)
{
  if (ACCG_P (regno))
    return GET_MODE_SIZE (mode);
  else
    return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}


/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.

   This declaration is required.  */

int
frv_class_max_nregs (enum reg_class class, enum machine_mode mode)
{
  if (class == ACCG_REGS)
    /* An N-byte value requires N accumulator guards.  */
    return GET_MODE_SIZE (mode);
  else
    return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}


/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */

int
frv_legitimate_constant_p (rtx x)
{
  enum machine_mode mode = GET_MODE (x);

  /* All of the integer constants are ok.  */
  if (GET_CODE (x) != CONST_DOUBLE)
    return TRUE;

  /* double integer constants are ok.  */
  if (mode == VOIDmode || mode == DImode)
    return TRUE;

  /* 0 is always ok.  */
  if (x == CONST0_RTX (mode))
    return TRUE;

  /* If floating point is just emulated, allow any constant, since it will be
     constructed in the GPRs.  */
  if (!TARGET_HAS_FPRS)
    return TRUE;

  if (mode == DFmode && !TARGET_DOUBLE)
    return TRUE;

  /* Otherwise store the constant away and do a load.  */
  return FALSE;
}

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  The classes are expressed using the enumeration values
   such as `GENERAL_REGS'.  A value of 4 is the default; other values are
   interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   If reload sees an insn consisting of a single `set' between two hard
   registers, and if `REGISTER_MOVE_COST' applied to their classes returns a
   value of 2, reload does not check to ensure that the constraints of the insn
   are met.  Setting a cost of other than 2 will allow reload to verify that
   the constraints are met.  You should do this if the `movM' pattern's
   constraints do not allow such copying.  */

#define HIGH_COST 40
#define MEDIUM_COST 3
#define LOW_COST 1

int
frv_register_move_cost (enum reg_class from, enum reg_class to)
{
  switch (from)
    {
    default:
      break;

    case QUAD_REGS:
    case EVEN_REGS:
    case GPR_REGS:
      switch (to)
	{
	default:
	  break;

	case QUAD_REGS:
	case EVEN_REGS:
	case GPR_REGS:
	  return LOW_COST;

	case FEVEN_REGS:
	case FPR_REGS:
	  return LOW_COST;

	case LCR_REG:
	case LR_REG:
	case SPR_REGS:
	  return LOW_COST;
	}

    case FEVEN_REGS:
    case FPR_REGS:
      switch (to)
	{
	default:
	  break;

	case QUAD_REGS:
	case EVEN_REGS:
	case GPR_REGS:
	case ACC_REGS:
	case EVEN_ACC_REGS:
	case QUAD_ACC_REGS:
	case ACCG_REGS:
	  return MEDIUM_COST;

	case FEVEN_REGS:
	case FPR_REGS:
	  return LOW_COST;
	}

    case LCR_REG:
    case LR_REG:
    case SPR_REGS:
      switch (to)
	{
	default:
	  break;

	case QUAD_REGS:
	case EVEN_REGS:
	case GPR_REGS:
	  return MEDIUM_COST;
	}

    case ACC_REGS:
    case EVEN_ACC_REGS:
    case QUAD_ACC_REGS:
    case ACCG_REGS:
      switch (to)
	{
	default:
	  break;

	case FEVEN_REGS:
	case FPR_REGS:
	  return MEDIUM_COST;

	}
    }

  return HIGH_COST;
}

/* Implementation of TARGET_ASM_INTEGER.  In the FRV case we need to
   use ".picptr" to generate safe relocations for PIC code.  We also
   need a fixup entry for aligned (non-debugging) code.  */

static bool
frv_assemble_integer (rtx value, unsigned int size, int aligned_p)
{
  if (flag_pic && size == UNITS_PER_WORD)
    {
      if (GET_CODE (value) == CONST
	  || GET_CODE (value) == SYMBOL_REF
	  || GET_CODE (value) == LABEL_REF)
	{
	  if (aligned_p)
	    {
	      static int label_num = 0;
	      char buf[256];
	      const char *p;

	      ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", label_num++);
	      p = (* targetm.strip_name_encoding) (buf);

	      fprintf (asm_out_file, "%s:\n", p);
	      fprintf (asm_out_file, "%s\n", FIXUP_SECTION_ASM_OP);
	      fprintf (asm_out_file, "\t.picptr\t%s\n", p);
	      fprintf (asm_out_file, "\t.previous\n");
	    }
	  assemble_integer_with_op ("\t.picptr\t", value);
	  return true;
	}
      if (!aligned_p)
	{
	  /* We've set the unaligned SI op to NULL, so we always have to
	     handle the unaligned case here.  */
	  assemble_integer_with_op ("\t.4byte\t", value);
	  return true;
	}
    }
  return default_assemble_integer (value, size, aligned_p);
}

/* Function to set up the backend function structure.  */

static struct machine_function *
frv_init_machine_status (void)
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

/* Implement TARGET_SCHED_ISSUE_RATE.  */

static int
frv_issue_rate (void)
{
  if (!TARGET_PACK)
    return 1;

  switch (frv_cpu_type)
    {
    default:
    case FRV_CPU_FR300:
    case FRV_CPU_SIMPLE:
      return 1;

    case FRV_CPU_FR400:
      return 2;

    case FRV_CPU_GENERIC:
    case FRV_CPU_FR500:
    case FRV_CPU_TOMCAT:
      return 4;
    }
}


/* Implement TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE.  */

static int
frv_use_dfa_pipeline_interface (void)
{
  return true;
}

/* Update the register state information, to know about which registers are set
   or clobbered.  */

static void
frv_registers_update (rtx x,
                      unsigned char reg_state[],
                      int modified[],
                      int *p_num_mod,
                      int flag)
{
  int regno, reg_max;
  rtx reg;
  rtx cond;
  const char *format;
  int length;
  int j;

  switch (GET_CODE (x))
    {
    default:
      break;

      /* Clobber just modifies a register, it doesn't make it live.  */
    case CLOBBER:
      frv_registers_update (XEXP (x, 0), reg_state, modified, p_num_mod,
			    flag | REGSTATE_MODIFIED);
      return;

      /* Pre modify updates the first argument, just references the second.  */
    case PRE_MODIFY:
    case SET:
      frv_registers_update (XEXP (x, 0), reg_state, modified, p_num_mod,
			    flag | REGSTATE_MODIFIED | REGSTATE_LIVE);
      frv_registers_update (XEXP (x, 1), reg_state, modified, p_num_mod, flag);
      return;

      /* For COND_EXEC, pass the appropriate flag to evaluate the conditional
         statement, but just to be sure, make sure it is the type of cond_exec
         we expect.  */
    case COND_EXEC:
      cond = XEXP (x, 0);
      if ((GET_CODE (cond) == EQ || GET_CODE (cond) == NE)
	  && GET_CODE (XEXP (cond, 0)) == REG
	  && CR_P (REGNO (XEXP (cond, 0)))
	  && GET_CODE (XEXP (cond, 1)) == CONST_INT
	  && INTVAL (XEXP (cond, 1)) == 0
	  && (flag & (REGSTATE_MODIFIED | REGSTATE_IF_EITHER)) == 0)
	{
	  frv_registers_update (cond, reg_state, modified, p_num_mod, flag);
	  flag |= ((REGNO (XEXP (cond, 0)) - CR_FIRST)
		   | ((GET_CODE (cond) == NE)
		      ? REGSTATE_IF_TRUE
		      : REGSTATE_IF_FALSE));

	  frv_registers_update (XEXP (x, 1), reg_state, modified, p_num_mod,
				flag);
	  return;
	}
      else
	fatal_insn ("frv_registers_update", x);

      /* MEM resets the modification bits.  */
    case MEM:
      flag &= ~REGSTATE_MODIFIED;
      break;

      /* See if we need to set the modified flag.  */
    case SUBREG:
      reg = SUBREG_REG (x);
      if (GET_CODE (reg) == REG)
	{
	  regno = subreg_regno (x);
	  reg_max = REGNO (reg) + HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  goto reg_common;
	}
      break;

    case REG:
      regno = REGNO (x);
      reg_max = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));
      /* Fall through.  */

    reg_common:
      if (flag & REGSTATE_MODIFIED)
	{
	  flag &= REGSTATE_MASK;
	  while (regno < reg_max)
	    {
	      int rs = reg_state[regno];

	      if (flag != rs)
		{
		  if ((rs & REGSTATE_MODIFIED) == 0)
		    {
		      modified[ *p_num_mod ] = regno;
		      (*p_num_mod)++;
		    }

		  /* If the previous register state had the register as
                     modified, possibly in some conditional execution context,
                     and the current insn modifies in some other context, or
                     outside of conditional execution, just mark the variable
                     as modified.  */
		  else
		    flag &= ~(REGSTATE_IF_EITHER | REGSTATE_CC_MASK);

		  reg_state[regno] = (rs | flag);
		}
	      regno++;
	    }
	}
      return;
    }


  length = GET_RTX_LENGTH (GET_CODE (x));
  format = GET_RTX_FORMAT (GET_CODE (x));

  for (j = 0; j < length; ++j)
    {
      switch (format[j])
	{
	case 'e':
	  frv_registers_update (XEXP (x, j), reg_state, modified, p_num_mod,
				flag);
	  break;

	case 'V':
	case 'E':
	  if (XVEC (x, j) != 0)
	    {
	      int k;
	      for (k = 0; k < XVECLEN (x, j); ++k)
		frv_registers_update (XVECEXP (x, j, k), reg_state, modified,
				      p_num_mod, flag);
	    }
	  break;

	default:
	  /* Nothing to do.  */
	  break;
	}
    }

  return;
}


/* Return if any registers in a hard register set were used an insn.  */

static int
frv_registers_used_p (rtx x, unsigned char reg_state[], int flag)
{
  int regno, reg_max;
  rtx reg;
  rtx cond;
  rtx dest;
  const char *format;
  int result;
  int length;
  int j;

  switch (GET_CODE (x))
    {
    default:
      break;

      /* Skip clobber, that doesn't use the previous value.  */
    case CLOBBER:
      return FALSE;

      /* For SET, if a conditional jump has occurred in the same insn, only
	 allow a set of a CR register if that register is not currently live.
	 This is because on the FR-V, B0/B1 instructions are always last.
	 Otherwise, don't look at the result, except within a MEM, but do look
	 at the source.  */
    case SET:
      dest = SET_DEST (x);
      if (flag & REGSTATE_CONDJUMP
	  && GET_CODE (dest) == REG && CR_P (REGNO (dest))
	  && (reg_state[ REGNO (dest) ] & REGSTATE_LIVE) != 0)
	return TRUE;

      if (GET_CODE (dest) == MEM)
	{
	  result = frv_registers_used_p (XEXP (dest, 0), reg_state, flag);
	  if (result)
	    return result;
	}

      return frv_registers_used_p (SET_SRC (x), reg_state, flag);

      /* For COND_EXEC, pass the appropriate flag to evaluate the conditional
         statement, but just to be sure, make sure it is the type of cond_exec
         we expect.  */
    case COND_EXEC:
      cond = XEXP (x, 0);
      if ((GET_CODE (cond) == EQ || GET_CODE (cond) == NE)
	  && GET_CODE (XEXP (cond, 0)) == REG
	  && CR_P (REGNO (XEXP (cond, 0)))
	  && GET_CODE (XEXP (cond, 1)) == CONST_INT
	  && INTVAL (XEXP (cond, 1)) == 0
	  && (flag & (REGSTATE_MODIFIED | REGSTATE_IF_EITHER)) == 0)
	{
	  result = frv_registers_used_p (cond, reg_state, flag);
	  if (result)
	    return result;

	  flag |= ((REGNO (XEXP (cond, 0)) - CR_FIRST)
		   | ((GET_CODE (cond) == NE)
		      ? REGSTATE_IF_TRUE
		      : REGSTATE_IF_FALSE));

	  return frv_registers_used_p (XEXP (x, 1), reg_state, flag);
	}
      else
	fatal_insn ("frv_registers_used_p", x);

      /* See if a register or subreg was modified in the same VLIW insn.  */
    case SUBREG:
      reg = SUBREG_REG (x);
      if (GET_CODE (reg) == REG)
	{
	  regno = subreg_regno (x);
	  reg_max = REGNO (reg) + HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  goto reg_common;
	}
      break;

    case REG:
      regno = REGNO (x);
      reg_max = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));
      /* Fall through.  */

    reg_common:
      while (regno < reg_max)
	{
	  int rs = reg_state[regno];

	  if (rs & REGSTATE_MODIFIED)
	    {
	      int rs_if = rs & REGSTATE_IF_EITHER;
	      int flag_if = flag & REGSTATE_IF_EITHER;

	      /* Simple modification, no conditional execution */
	      if ((rs & REGSTATE_IF_EITHER) == 0)
		return TRUE;

	      /* See if the variable is only modified in a conditional
		 execution expression opposite to the conditional execution
		 expression that governs this expression (ie, true vs. false
		 for the same CC register).  If this isn't two halves of the
		 same conditional expression, consider the register
		 modified.  */
	      if (((rs_if == REGSTATE_IF_TRUE && flag_if == REGSTATE_IF_FALSE)
		   || (rs_if == REGSTATE_IF_FALSE && flag_if == REGSTATE_IF_TRUE))
		  && ((rs & REGSTATE_CC_MASK) == (flag & REGSTATE_CC_MASK)))
		;
	      else
		return TRUE;
	    }

	  regno++;
	}
      return FALSE;
    }


  length = GET_RTX_LENGTH (GET_CODE (x));
  format = GET_RTX_FORMAT (GET_CODE (x));

  for (j = 0; j < length; ++j)
    {
      switch (format[j])
	{
	case 'e':
	  result = frv_registers_used_p (XEXP (x, j), reg_state, flag);
	  if (result != 0)
	    return result;
	  break;

	case 'V':
	case 'E':
	  if (XVEC (x, j) != 0)
	    {
	      int k;
	      for (k = 0; k < XVECLEN (x, j); ++k)
		{
		  result = frv_registers_used_p (XVECEXP (x, j, k), reg_state,
						 flag);
		  if (result != 0)
		    return result;
		}
	    }
	  break;

	default:
	  /* Nothing to do.  */
	  break;
	}
    }

  return 0;
}

/* Return if any registers in a hard register set were set in an insn.  */

static int
frv_registers_set_p (rtx x, unsigned char reg_state[], int modify_p)
{
  int regno, reg_max;
  rtx reg;
  rtx cond;
  const char *format;
  int length;
  int j;

  switch (GET_CODE (x))
    {
    default:
      break;

    case CLOBBER:
      return frv_registers_set_p (XEXP (x, 0), reg_state, TRUE);

    case PRE_MODIFY:
    case SET:
      return (frv_registers_set_p (XEXP (x, 0), reg_state, TRUE)
	      || frv_registers_set_p (XEXP (x, 1), reg_state, FALSE));

    case COND_EXEC:
      cond = XEXP (x, 0);
      /* Just to be sure, make sure it is the type of cond_exec we
         expect.  */
      if ((GET_CODE (cond) == EQ || GET_CODE (cond) == NE)
	  && GET_CODE (XEXP (cond, 0)) == REG
	  && CR_P (REGNO (XEXP (cond, 0)))
	  && GET_CODE (XEXP (cond, 1)) == CONST_INT
	  && INTVAL (XEXP (cond, 1)) == 0
	  && !modify_p)
	return frv_registers_set_p (XEXP (x, 1), reg_state, modify_p);
      else
	fatal_insn ("frv_registers_set_p", x);

      /* MEM resets the modification bits.  */
    case MEM:
      modify_p = FALSE;
      break;

      /* See if we need to set the modified modify_p.  */
    case SUBREG:
      reg = SUBREG_REG (x);
      if (GET_CODE (reg) == REG)
	{
	  regno = subreg_regno (x);
	  reg_max = REGNO (reg) + HARD_REGNO_NREGS (regno, GET_MODE (reg));
	  goto reg_common;
	}
      break;

    case REG:
      regno = REGNO (x);
      reg_max = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));
      /* Fall through.  */

    reg_common:
      if (modify_p)
	while (regno < reg_max)
	  {
	    int rs = reg_state[regno];

	    if (rs & REGSTATE_MODIFIED)
	      return TRUE;
	    regno++;
	  }
      return FALSE;
    }


  length = GET_RTX_LENGTH (GET_CODE (x));
  format = GET_RTX_FORMAT (GET_CODE (x));

  for (j = 0; j < length; ++j)
    {
      switch (format[j])
	{
	case 'e':
	  if (frv_registers_set_p (XEXP (x, j), reg_state, modify_p))
	    return TRUE;
	  break;

	case 'V':
	case 'E':
	  if (XVEC (x, j) != 0)
	    {
	      int k;
	      for (k = 0; k < XVECLEN (x, j); ++k)
		if (frv_registers_set_p (XVECEXP (x, j, k), reg_state,
					 modify_p))
		  return TRUE;
	    }
	  break;

	default:
	  /* Nothing to do.  */
	  break;
	}
    }

  return FALSE;
}


/* On the FR-V, this pass is used to rescan the insn chain, and pack
   conditional branches/calls/jumps, etc. with previous insns where it can.  It
   does not reorder the instructions.  We assume the scheduler left the flow
   information in a reasonable state.  */

static void
frv_pack_insns (void)
{
  state_t frv_state;			/* frv state machine */
  int cur_start_vliw_p;			/* current insn starts a VLIW insn */
  int next_start_vliw_p;		/* next insn starts a VLIW insn */
  int cur_condjump_p;			/* flag if current insn is a cond jump*/
  int next_condjump_p;			/* flag if next insn is a cond jump */
  rtx insn;
  rtx link;
  int j;
  int num_mod = 0;			/* # of modified registers */
  int modified[FIRST_PSEUDO_REGISTER];	/* registers modified in current VLIW */
					/* register state information */
  unsigned char reg_state[FIRST_PSEUDO_REGISTER];

  /* If we weren't going to pack the insns, don't bother with this pass.  */
  if (!optimize
      || !flag_schedule_insns_after_reload
      || TARGET_NO_VLIW_BRANCH
      || frv_issue_rate () == 1)
    return;

  /* Set up the instruction and register states.  */
  dfa_start ();
  frv_state = (state_t) xmalloc (state_size ());
  memset (reg_state, REGSTATE_DEAD, sizeof (reg_state));

  /* Go through the insns, and repack the insns.  */
  state_reset (frv_state);
  cur_start_vliw_p = FALSE;
  next_start_vliw_p = TRUE;
  cur_condjump_p = 0;
  next_condjump_p = 0;

  for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
    {
      enum rtx_code code = GET_CODE (insn);
      enum rtx_code pattern_code;

      /* For basic block begin notes redo the live information, and skip other
         notes.  */
      if (code == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == (int)NOTE_INSN_BASIC_BLOCK)
	    {
	      regset live;

	      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
		reg_state[j] &= ~ REGSTATE_LIVE;

	      live = NOTE_BASIC_BLOCK (insn)->global_live_at_start;
	      EXECUTE_IF_SET_IN_REG_SET(live, 0, j,
					{
					  reg_state[j] |= REGSTATE_LIVE;
					});
	    }

	  continue;
	}

      /* Things like labels reset everything.  */
      if (GET_RTX_CLASS (code) != 'i')
	{
	  next_start_vliw_p = TRUE;
	  continue;
	}

      /* Clear the VLIW start flag on random USE and CLOBBER insns, which is
         set on the USE insn that precedes the return, and potentially on
         CLOBBERs for setting multiword variables.  Also skip the ADDR_VEC
         holding the case table labels.  */
      pattern_code = GET_CODE (PATTERN (insn));
      if (pattern_code == USE || pattern_code == CLOBBER
	  || pattern_code == ADDR_VEC || pattern_code == ADDR_DIFF_VEC)
	{
	  CLEAR_VLIW_START (insn);
	  continue;
	}

      cur_start_vliw_p = next_start_vliw_p;
      next_start_vliw_p = FALSE;

      cur_condjump_p |= next_condjump_p;
      next_condjump_p = 0;

      /* Unconditional branches and calls end the current VLIW insn.  */
      if (code == CALL_INSN)
	{
	  next_start_vliw_p = TRUE;

	  /* On a TOMCAT, calls must be alone in the VLIW insns.  */
	  if (frv_cpu_type == FRV_CPU_TOMCAT)
	    cur_start_vliw_p = TRUE;
	}
      else if (code == JUMP_INSN)
	{
	  if (any_condjump_p (insn))
	    next_condjump_p = REGSTATE_CONDJUMP;
	  else
	    next_start_vliw_p = TRUE;
	}

      /* Only allow setting a CCR register after a conditional branch.  */
      else if (((cur_condjump_p & REGSTATE_CONDJUMP) != 0)
	       && get_attr_type (insn) != TYPE_CCR)
	cur_start_vliw_p = TRUE;

      /* Determine if we need to start a new VLIW instruction.  */
      if (cur_start_vliw_p
	  /* Do not check for register conflicts in a setlo instruction
	     because any output or true dependencies will be with the
	     partnering sethi instruction, with which it can be packed.

	     Although output dependencies are rare they are still
	     possible.  So check output dependencies in VLIW insn.  */
	  || (get_attr_type (insn) != TYPE_SETLO
	      && (frv_registers_used_p (PATTERN (insn),
					reg_state,
					cur_condjump_p)
		  || frv_registers_set_p (PATTERN (insn), reg_state, FALSE)))
	  || state_transition (frv_state, insn) >= 0)
	{
	  SET_VLIW_START (insn);
	  state_reset (frv_state);
	  state_transition (frv_state, insn);
	  cur_condjump_p = 0;

	  /* Update the modified registers.  */
	  for (j = 0; j < num_mod; j++)
	    reg_state[ modified[j] ] &= ~(REGSTATE_CC_MASK
					  | REGSTATE_IF_EITHER
					  | REGSTATE_MODIFIED);

	  num_mod = 0;
	}
      else
	CLEAR_VLIW_START (insn);

      /* Record which registers are modified.  */
      frv_registers_update (PATTERN (insn), reg_state, modified, &num_mod, 0);

      /* Process the death notices.  */
      for (link = REG_NOTES (insn);
	   link != NULL_RTX;
	   link = XEXP (link, 1))
	{
	  rtx reg = XEXP (link, 0);

	  if (REG_NOTE_KIND (link) == REG_DEAD && GET_CODE (reg) == REG)
	    {
	      int regno = REGNO (reg);
	      int n = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
	      for (; regno < n; regno++)
		reg_state[regno] &= ~REGSTATE_LIVE;
	    }
	}
    }

  free (frv_state);
  dfa_finish ();
  return;
}


#define def_builtin(name, type, code) \
  builtin_function ((name), (type), (code), BUILT_IN_MD, NULL, NULL)

struct builtin_description
{
  enum insn_code icode;
  const char *name;
  enum frv_builtins code;
  enum rtx_code comparison;
  unsigned int flag;
};

/* Media intrinsics that take a single, constant argument.  */

static struct builtin_description bdesc_set[] =
{
  { CODE_FOR_mhdsets, "__MHDSETS", FRV_BUILTIN_MHDSETS, 0, 0 }
};

/* Media intrinsics that take just one argument.  */

static struct builtin_description bdesc_1arg[] =
{
  { CODE_FOR_mnot, "__MNOT", FRV_BUILTIN_MNOT, 0, 0 },
  { CODE_FOR_munpackh, "__MUNPACKH", FRV_BUILTIN_MUNPACKH, 0, 0 },
  { CODE_FOR_mbtoh, "__MBTOH", FRV_BUILTIN_MBTOH, 0, 0 },
  { CODE_FOR_mhtob, "__MHTOB", FRV_BUILTIN_MHTOB, 0, 0 },
  { CODE_FOR_mabshs, "__MABSHS", FRV_BUILTIN_MABSHS, 0, 0 }
};

/* Media intrinsics that take two arguments.  */

static struct builtin_description bdesc_2arg[] =
{
  { CODE_FOR_mand, "__MAND", FRV_BUILTIN_MAND, 0, 0 },
  { CODE_FOR_mor, "__MOR", FRV_BUILTIN_MOR, 0, 0 },
  { CODE_FOR_mxor, "__MXOR", FRV_BUILTIN_MXOR, 0, 0 },
  { CODE_FOR_maveh, "__MAVEH", FRV_BUILTIN_MAVEH, 0, 0 },
  { CODE_FOR_msaths, "__MSATHS", FRV_BUILTIN_MSATHS, 0, 0 },
  { CODE_FOR_msathu, "__MSATHU", FRV_BUILTIN_MSATHU, 0, 0 },
  { CODE_FOR_maddhss, "__MADDHSS", FRV_BUILTIN_MADDHSS, 0, 0 },
  { CODE_FOR_maddhus, "__MADDHUS", FRV_BUILTIN_MADDHUS, 0, 0 },
  { CODE_FOR_msubhss, "__MSUBHSS", FRV_BUILTIN_MSUBHSS, 0, 0 },
  { CODE_FOR_msubhus, "__MSUBHUS", FRV_BUILTIN_MSUBHUS, 0, 0 },
  { CODE_FOR_mqaddhss, "__MQADDHSS", FRV_BUILTIN_MQADDHSS, 0, 0 },
  { CODE_FOR_mqaddhus, "__MQADDHUS", FRV_BUILTIN_MQADDHUS, 0, 0 },
  { CODE_FOR_mqsubhss, "__MQSUBHSS", FRV_BUILTIN_MQSUBHSS, 0, 0 },
  { CODE_FOR_mqsubhus, "__MQSUBHUS", FRV_BUILTIN_MQSUBHUS, 0, 0 },
  { CODE_FOR_mpackh, "__MPACKH", FRV_BUILTIN_MPACKH, 0, 0 },
  { CODE_FOR_mdpackh, "__MDPACKH", FRV_BUILTIN_MDPACKH, 0, 0 },
  { CODE_FOR_mcop1, "__Mcop1", FRV_BUILTIN_MCOP1, 0, 0 },
  { CODE_FOR_mcop2, "__Mcop2", FRV_BUILTIN_MCOP2, 0, 0 },
  { CODE_FOR_mwcut, "__MWCUT", FRV_BUILTIN_MWCUT, 0, 0 },
  { CODE_FOR_mqsaths, "__MQSATHS", FRV_BUILTIN_MQSATHS, 0, 0 }
};

/* Media intrinsics that take two arguments, the first being an ACC number.  */

static struct builtin_description bdesc_cut[] =
{
  { CODE_FOR_mcut, "__MCUT", FRV_BUILTIN_MCUT, 0, 0 },
  { CODE_FOR_mcutss, "__MCUTSS", FRV_BUILTIN_MCUTSS, 0, 0 },
  { CODE_FOR_mdcutssi, "__MDCUTSSI", FRV_BUILTIN_MDCUTSSI, 0, 0 }
};

/* Two-argument media intrinsics with an immediate second argument.  */

static struct builtin_description bdesc_2argimm[] =
{
  { CODE_FOR_mrotli, "__MROTLI", FRV_BUILTIN_MROTLI, 0, 0 },
  { CODE_FOR_mrotri, "__MROTRI", FRV_BUILTIN_MROTRI, 0, 0 },
  { CODE_FOR_msllhi, "__MSLLHI", FRV_BUILTIN_MSLLHI, 0, 0 },
  { CODE_FOR_msrlhi, "__MSRLHI", FRV_BUILTIN_MSRLHI, 0, 0 },
  { CODE_FOR_msrahi, "__MSRAHI", FRV_BUILTIN_MSRAHI, 0, 0 },
  { CODE_FOR_mexpdhw, "__MEXPDHW", FRV_BUILTIN_MEXPDHW, 0, 0 },
  { CODE_FOR_mexpdhd, "__MEXPDHD", FRV_BUILTIN_MEXPDHD, 0, 0 },
  { CODE_FOR_mdrotli, "__MDROTLI", FRV_BUILTIN_MDROTLI, 0, 0 },
  { CODE_FOR_mcplhi, "__MCPLHI", FRV_BUILTIN_MCPLHI, 0, 0 },
  { CODE_FOR_mcpli, "__MCPLI", FRV_BUILTIN_MCPLI, 0, 0 },
  { CODE_FOR_mhsetlos, "__MHSETLOS", FRV_BUILTIN_MHSETLOS, 0, 0 },
  { CODE_FOR_mhsetloh, "__MHSETLOH", FRV_BUILTIN_MHSETLOH, 0, 0 },
  { CODE_FOR_mhsethis, "__MHSETHIS", FRV_BUILTIN_MHSETHIS, 0, 0 },
  { CODE_FOR_mhsethih, "__MHSETHIH", FRV_BUILTIN_MHSETHIH, 0, 0 },
  { CODE_FOR_mhdseth, "__MHDSETH", FRV_BUILTIN_MHDSETH, 0, 0 }
};

/* Media intrinsics that take two arguments and return void, the first argument
   being a pointer to 4 words in memory.  */

static struct builtin_description bdesc_void2arg[] =
{
  { CODE_FOR_mdunpackh, "__MDUNPACKH", FRV_BUILTIN_MDUNPACKH, 0, 0 },
  { CODE_FOR_mbtohe, "__MBTOHE", FRV_BUILTIN_MBTOHE, 0, 0 },
};

/* Media intrinsics that take three arguments, the first being a const_int that
   denotes an accumulator, and that return void.  */

static struct builtin_description bdesc_void3arg[] =
{
  { CODE_FOR_mcpxrs, "__MCPXRS", FRV_BUILTIN_MCPXRS, 0, 0 },
  { CODE_FOR_mcpxru, "__MCPXRU", FRV_BUILTIN_MCPXRU, 0, 0 },
  { CODE_FOR_mcpxis, "__MCPXIS", FRV_BUILTIN_MCPXIS, 0, 0 },
  { CODE_FOR_mcpxiu, "__MCPXIU", FRV_BUILTIN_MCPXIU, 0, 0 },
  { CODE_FOR_mmulhs, "__MMULHS", FRV_BUILTIN_MMULHS, 0, 0 },
  { CODE_FOR_mmulhu, "__MMULHU", FRV_BUILTIN_MMULHU, 0, 0 },
  { CODE_FOR_mmulxhs, "__MMULXHS", FRV_BUILTIN_MMULXHS, 0, 0 },
  { CODE_FOR_mmulxhu, "__MMULXHU", FRV_BUILTIN_MMULXHU, 0, 0 },
  { CODE_FOR_mmachs, "__MMACHS", FRV_BUILTIN_MMACHS, 0, 0 },
  { CODE_FOR_mmachu, "__MMACHU", FRV_BUILTIN_MMACHU, 0, 0 },
  { CODE_FOR_mmrdhs, "__MMRDHS", FRV_BUILTIN_MMRDHS, 0, 0 },
  { CODE_FOR_mmrdhu, "__MMRDHU", FRV_BUILTIN_MMRDHU, 0, 0 },
  { CODE_FOR_mqcpxrs, "__MQCPXRS", FRV_BUILTIN_MQCPXRS, 0, 0 },
  { CODE_FOR_mqcpxru, "__MQCPXRU", FRV_BUILTIN_MQCPXRU, 0, 0 },
  { CODE_FOR_mqcpxis, "__MQCPXIS", FRV_BUILTIN_MQCPXIS, 0, 0 },
  { CODE_FOR_mqcpxiu, "__MQCPXIU", FRV_BUILTIN_MQCPXIU, 0, 0 },
  { CODE_FOR_mqmulhs, "__MQMULHS", FRV_BUILTIN_MQMULHS, 0, 0 },
  { CODE_FOR_mqmulhu, "__MQMULHU", FRV_BUILTIN_MQMULHU, 0, 0 },
  { CODE_FOR_mqmulxhs, "__MQMULXHS", FRV_BUILTIN_MQMULXHS, 0, 0 },
  { CODE_FOR_mqmulxhu, "__MQMULXHU", FRV_BUILTIN_MQMULXHU, 0, 0 },
  { CODE_FOR_mqmachs, "__MQMACHS", FRV_BUILTIN_MQMACHS, 0, 0 },
  { CODE_FOR_mqmachu, "__MQMACHU", FRV_BUILTIN_MQMACHU, 0, 0 },
  { CODE_FOR_mqxmachs, "__MQXMACHS", FRV_BUILTIN_MQXMACHS, 0, 0 },
  { CODE_FOR_mqxmacxhs, "__MQXMACXHS", FRV_BUILTIN_MQXMACXHS, 0, 0 },
  { CODE_FOR_mqmacxhs, "__MQMACXHS", FRV_BUILTIN_MQMACXHS, 0, 0 }
};

/* Media intrinsics that take two accumulator numbers as argument and
   return void.  */

static struct builtin_description bdesc_voidacc[] =
{
  { CODE_FOR_maddaccs, "__MADDACCS", FRV_BUILTIN_MADDACCS, 0, 0 },
  { CODE_FOR_msubaccs, "__MSUBACCS", FRV_BUILTIN_MSUBACCS, 0, 0 },
  { CODE_FOR_masaccs, "__MASACCS", FRV_BUILTIN_MASACCS, 0, 0 },
  { CODE_FOR_mdaddaccs, "__MDADDACCS", FRV_BUILTIN_MDADDACCS, 0, 0 },
  { CODE_FOR_mdsubaccs, "__MDSUBACCS", FRV_BUILTIN_MDSUBACCS, 0, 0 },
  { CODE_FOR_mdasaccs, "__MDASACCS", FRV_BUILTIN_MDASACCS, 0, 0 }
};

/* Initialize media builtins.  */

static void
frv_init_builtins (void)
{
  tree endlink = void_list_node;
  tree accumulator = integer_type_node;
  tree integer = integer_type_node;
  tree voidt = void_type_node;
  tree uhalf = short_unsigned_type_node;
  tree sword1 = long_integer_type_node;
  tree uword1 = long_unsigned_type_node;
  tree sword2 = long_long_integer_type_node;
  tree uword2 = long_long_unsigned_type_node;
  tree uword4 = build_pointer_type (uword1);

#define UNARY(RET, T1) \
  build_function_type (RET, tree_cons (NULL_TREE, T1, endlink))

#define BINARY(RET, T1, T2) \
  build_function_type (RET, tree_cons (NULL_TREE, T1, \
			    tree_cons (NULL_TREE, T2, endlink)))

#define TRINARY(RET, T1, T2, T3) \
  build_function_type (RET, tree_cons (NULL_TREE, T1, \
			    tree_cons (NULL_TREE, T2, \
			    tree_cons (NULL_TREE, T3, endlink))))

  tree void_ftype_void = build_function_type (voidt, endlink);

  tree void_ftype_acc = UNARY (voidt, accumulator);
  tree void_ftype_uw4_uw1 = BINARY (voidt, uword4, uword1);
  tree void_ftype_uw4_uw2 = BINARY (voidt, uword4, uword2);
  tree void_ftype_acc_uw1 = BINARY (voidt, accumulator, uword1);
  tree void_ftype_acc_acc = BINARY (voidt, accumulator, accumulator);
  tree void_ftype_acc_uw1_uw1 = TRINARY (voidt, accumulator, uword1, uword1);
  tree void_ftype_acc_sw1_sw1 = TRINARY (voidt, accumulator, sword1, sword1);
  tree void_ftype_acc_uw2_uw2 = TRINARY (voidt, accumulator, uword2, uword2);
  tree void_ftype_acc_sw2_sw2 = TRINARY (voidt, accumulator, sword2, sword2);

  tree uw1_ftype_uw1 = UNARY (uword1, uword1);
  tree uw1_ftype_sw1 = UNARY (uword1, sword1);
  tree uw1_ftype_uw2 = UNARY (uword1, uword2);
  tree uw1_ftype_acc = UNARY (uword1, accumulator);
  tree uw1_ftype_uh_uh = BINARY (uword1, uhalf, uhalf);
  tree uw1_ftype_uw1_uw1 = BINARY (uword1, uword1, uword1);
  tree uw1_ftype_uw1_int = BINARY (uword1, uword1, integer);
  tree uw1_ftype_acc_uw1 = BINARY (uword1, accumulator, uword1);
  tree uw1_ftype_acc_sw1 = BINARY (uword1, accumulator, sword1);
  tree uw1_ftype_uw2_uw1 = BINARY (uword1, uword2, uword1);
  tree uw1_ftype_uw2_int = BINARY (uword1, uword2, integer);

  tree sw1_ftype_int = UNARY (sword1, integer);
  tree sw1_ftype_sw1_sw1 = BINARY (sword1, sword1, sword1);
  tree sw1_ftype_sw1_int = BINARY (sword1, sword1, integer);

  tree uw2_ftype_uw1 = UNARY (uword2, uword1);
  tree uw2_ftype_uw1_int = BINARY (uword2, uword1, integer);
  tree uw2_ftype_uw2_uw2 = BINARY (uword2, uword2, uword2);
  tree uw2_ftype_uw2_int = BINARY (uword2, uword2, integer);
  tree uw2_ftype_acc_int = BINARY (uword2, accumulator, integer);

  tree sw2_ftype_sw2_sw2 = BINARY (sword2, sword2, sword2);

  def_builtin ("__MAND", uw1_ftype_uw1_uw1, FRV_BUILTIN_MAND);
  def_builtin ("__MOR", uw1_ftype_uw1_uw1, FRV_BUILTIN_MOR);
  def_builtin ("__MXOR", uw1_ftype_uw1_uw1, FRV_BUILTIN_MXOR);
  def_builtin ("__MNOT", uw1_ftype_uw1, FRV_BUILTIN_MNOT);
  def_builtin ("__MROTLI", uw1_ftype_uw1_int, FRV_BUILTIN_MROTLI);
  def_builtin ("__MROTRI", uw1_ftype_uw1_int, FRV_BUILTIN_MROTRI);
  def_builtin ("__MWCUT", uw1_ftype_uw2_uw1, FRV_BUILTIN_MWCUT);
  def_builtin ("__MAVEH", uw1_ftype_uw1_uw1, FRV_BUILTIN_MAVEH);
  def_builtin ("__MSLLHI", uw1_ftype_uw1_int, FRV_BUILTIN_MSLLHI);
  def_builtin ("__MSRLHI", uw1_ftype_uw1_int, FRV_BUILTIN_MSRLHI);
  def_builtin ("__MSRAHI", sw1_ftype_sw1_int, FRV_BUILTIN_MSRAHI);
  def_builtin ("__MSATHS", sw1_ftype_sw1_sw1, FRV_BUILTIN_MSATHS);
  def_builtin ("__MSATHU", uw1_ftype_uw1_uw1, FRV_BUILTIN_MSATHU);
  def_builtin ("__MADDHSS", sw1_ftype_sw1_sw1, FRV_BUILTIN_MADDHSS);
  def_builtin ("__MADDHUS", uw1_ftype_uw1_uw1, FRV_BUILTIN_MADDHUS);
  def_builtin ("__MSUBHSS", sw1_ftype_sw1_sw1, FRV_BUILTIN_MSUBHSS);
  def_builtin ("__MSUBHUS", uw1_ftype_uw1_uw1, FRV_BUILTIN_MSUBHUS);
  def_builtin ("__MMULHS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MMULHS);
  def_builtin ("__MMULHU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MMULHU);
  def_builtin ("__MMULXHS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MMULXHS);
  def_builtin ("__MMULXHU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MMULXHU);
  def_builtin ("__MMACHS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MMACHS);
  def_builtin ("__MMACHU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MMACHU);
  def_builtin ("__MMRDHS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MMRDHS);
  def_builtin ("__MMRDHU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MMRDHU);
  def_builtin ("__MQADDHSS", sw2_ftype_sw2_sw2, FRV_BUILTIN_MQADDHSS);
  def_builtin ("__MQADDHUS", uw2_ftype_uw2_uw2, FRV_BUILTIN_MQADDHUS);
  def_builtin ("__MQSUBHSS", sw2_ftype_sw2_sw2, FRV_BUILTIN_MQSUBHSS);
  def_builtin ("__MQSUBHUS", uw2_ftype_uw2_uw2, FRV_BUILTIN_MQSUBHUS);
  def_builtin ("__MQMULHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQMULHS);
  def_builtin ("__MQMULHU", void_ftype_acc_uw2_uw2, FRV_BUILTIN_MQMULHU);
  def_builtin ("__MQMULXHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQMULXHS);
  def_builtin ("__MQMULXHU", void_ftype_acc_uw2_uw2, FRV_BUILTIN_MQMULXHU);
  def_builtin ("__MQMACHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQMACHS);
  def_builtin ("__MQMACHU", void_ftype_acc_uw2_uw2, FRV_BUILTIN_MQMACHU);
  def_builtin ("__MCPXRS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MCPXRS);
  def_builtin ("__MCPXRU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MCPXRU);
  def_builtin ("__MCPXIS", void_ftype_acc_sw1_sw1, FRV_BUILTIN_MCPXIS);
  def_builtin ("__MCPXIU", void_ftype_acc_uw1_uw1, FRV_BUILTIN_MCPXIU);
  def_builtin ("__MQCPXRS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQCPXRS);
  def_builtin ("__MQCPXRU", void_ftype_acc_uw2_uw2, FRV_BUILTIN_MQCPXRU);
  def_builtin ("__MQCPXIS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQCPXIS);
  def_builtin ("__MQCPXIU", void_ftype_acc_uw2_uw2, FRV_BUILTIN_MQCPXIU);
  def_builtin ("__MCUT", uw1_ftype_acc_uw1, FRV_BUILTIN_MCUT);
  def_builtin ("__MCUTSS", uw1_ftype_acc_sw1, FRV_BUILTIN_MCUTSS);
  def_builtin ("__MEXPDHW", uw1_ftype_uw1_int, FRV_BUILTIN_MEXPDHW);
  def_builtin ("__MEXPDHD", uw2_ftype_uw1_int, FRV_BUILTIN_MEXPDHD);
  def_builtin ("__MPACKH", uw1_ftype_uh_uh, FRV_BUILTIN_MPACKH);
  def_builtin ("__MUNPACKH", uw2_ftype_uw1, FRV_BUILTIN_MUNPACKH);
  def_builtin ("__MDPACKH", uw2_ftype_uw2_uw2, FRV_BUILTIN_MDPACKH);
  def_builtin ("__MDUNPACKH", void_ftype_uw4_uw2, FRV_BUILTIN_MDUNPACKH);
  def_builtin ("__MBTOH", uw2_ftype_uw1, FRV_BUILTIN_MBTOH);
  def_builtin ("__MHTOB", uw1_ftype_uw2, FRV_BUILTIN_MHTOB);
  def_builtin ("__MBTOHE", void_ftype_uw4_uw1, FRV_BUILTIN_MBTOHE);
  def_builtin ("__MCLRACC", void_ftype_acc, FRV_BUILTIN_MCLRACC);
  def_builtin ("__MCLRACCA", void_ftype_void, FRV_BUILTIN_MCLRACCA);
  def_builtin ("__MRDACC", uw1_ftype_acc, FRV_BUILTIN_MRDACC);
  def_builtin ("__MRDACCG", uw1_ftype_acc, FRV_BUILTIN_MRDACCG);
  def_builtin ("__MWTACC", void_ftype_acc_uw1, FRV_BUILTIN_MWTACC);
  def_builtin ("__MWTACCG", void_ftype_acc_uw1, FRV_BUILTIN_MWTACCG);
  def_builtin ("__Mcop1", uw1_ftype_uw1_uw1, FRV_BUILTIN_MCOP1);
  def_builtin ("__Mcop2", uw1_ftype_uw1_uw1, FRV_BUILTIN_MCOP2);
  def_builtin ("__MTRAP", void_ftype_void, FRV_BUILTIN_MTRAP);
  def_builtin ("__MQXMACHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQXMACHS);
  def_builtin ("__MQXMACXHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQXMACXHS);
  def_builtin ("__MQMACXHS", void_ftype_acc_sw2_sw2, FRV_BUILTIN_MQMACXHS);
  def_builtin ("__MADDACCS", void_ftype_acc_acc, FRV_BUILTIN_MADDACCS);
  def_builtin ("__MSUBACCS", void_ftype_acc_acc, FRV_BUILTIN_MSUBACCS);
  def_builtin ("__MASACCS", void_ftype_acc_acc, FRV_BUILTIN_MASACCS);
  def_builtin ("__MDADDACCS", void_ftype_acc_acc, FRV_BUILTIN_MDADDACCS);
  def_builtin ("__MDSUBACCS", void_ftype_acc_acc, FRV_BUILTIN_MDSUBACCS);
  def_builtin ("__MDASACCS", void_ftype_acc_acc, FRV_BUILTIN_MDASACCS);
  def_builtin ("__MABSHS", uw1_ftype_sw1, FRV_BUILTIN_MABSHS);
  def_builtin ("__MDROTLI", uw2_ftype_uw2_int, FRV_BUILTIN_MDROTLI);
  def_builtin ("__MCPLHI", uw1_ftype_uw2_int, FRV_BUILTIN_MCPLHI);
  def_builtin ("__MCPLI", uw1_ftype_uw2_int, FRV_BUILTIN_MCPLI);
  def_builtin ("__MDCUTSSI", uw2_ftype_acc_int, FRV_BUILTIN_MDCUTSSI);
  def_builtin ("__MQSATHS", sw2_ftype_sw2_sw2, FRV_BUILTIN_MQSATHS);
  def_builtin ("__MHSETLOS", sw1_ftype_sw1_int, FRV_BUILTIN_MHSETLOS);
  def_builtin ("__MHSETHIS", sw1_ftype_sw1_int, FRV_BUILTIN_MHSETHIS);
  def_builtin ("__MHDSETS", sw1_ftype_int, FRV_BUILTIN_MHDSETS);
  def_builtin ("__MHSETLOH", uw1_ftype_uw1_int, FRV_BUILTIN_MHSETLOH);
  def_builtin ("__MHSETHIH", uw1_ftype_uw1_int, FRV_BUILTIN_MHSETHIH);
  def_builtin ("__MHDSETH", uw1_ftype_uw1_int, FRV_BUILTIN_MHDSETH);

#undef UNARY
#undef BINARY
#undef TRINARY
}

/* Set the names for various arithmetic operations according to the
   FRV ABI.  */
static void
frv_init_libfuncs (void)
{
  set_optab_libfunc (smod_optab,     SImode, "__modi");
  set_optab_libfunc (umod_optab,     SImode, "__umodi");

  set_optab_libfunc (add_optab,      DImode, "__addll");
  set_optab_libfunc (sub_optab,      DImode, "__subll");
  set_optab_libfunc (smul_optab,     DImode, "__mulll");
  set_optab_libfunc (sdiv_optab,     DImode, "__divll");
  set_optab_libfunc (smod_optab,     DImode, "__modll");
  set_optab_libfunc (umod_optab,     DImode, "__umodll");
  set_optab_libfunc (and_optab,      DImode, "__andll");
  set_optab_libfunc (ior_optab,      DImode, "__orll");
  set_optab_libfunc (xor_optab,      DImode, "__xorll");
  set_optab_libfunc (one_cmpl_optab, DImode, "__notll");

  set_optab_libfunc (add_optab,      SFmode, "__addf");
  set_optab_libfunc (sub_optab,      SFmode, "__subf");
  set_optab_libfunc (smul_optab,     SFmode, "__mulf");
  set_optab_libfunc (sdiv_optab,     SFmode, "__divf");

  set_optab_libfunc (add_optab,      DFmode, "__addd");
  set_optab_libfunc (sub_optab,      DFmode, "__subd");
  set_optab_libfunc (smul_optab,     DFmode, "__muld");
  set_optab_libfunc (sdiv_optab,     DFmode, "__divd");

  set_conv_libfunc (sext_optab,   DFmode, SFmode, "__ftod");
  set_conv_libfunc (trunc_optab,  SFmode, DFmode, "__dtof");

  set_conv_libfunc (sfix_optab,   SImode, SFmode, "__ftoi");
  set_conv_libfunc (sfix_optab,   DImode, SFmode, "__ftoll");
  set_conv_libfunc (sfix_optab,   SImode, DFmode, "__dtoi");
  set_conv_libfunc (sfix_optab,   DImode, DFmode, "__dtoll");

  set_conv_libfunc (ufix_optab,   SImode, SFmode, "__ftoui");
  set_conv_libfunc (ufix_optab,   DImode, SFmode, "__ftoull");
  set_conv_libfunc (ufix_optab,   SImode, DFmode, "__dtoui");
  set_conv_libfunc (ufix_optab,   DImode, DFmode, "__dtoull");

  set_conv_libfunc (sfloat_optab, SFmode, SImode, "__itof");
  set_conv_libfunc (sfloat_optab, SFmode, DImode, "__lltof");
  set_conv_libfunc (sfloat_optab, DFmode, SImode, "__itod");
  set_conv_libfunc (sfloat_optab, DFmode, DImode, "__lltod");
}

/* Convert an integer constant to an accumulator register.  ICODE is the
   code of the target instruction, OPNUM is the number of the
   accumulator operand and OPVAL is the constant integer.  Try both
   ACC and ACCG registers; only report an error if neither fit the
   instruction.  */

static rtx
frv_int_to_acc (enum insn_code icode, int opnum, rtx opval)
{
  rtx reg;

  if (GET_CODE (opval) != CONST_INT)
    {
      error ("accumulator is not a constant integer");
      return NULL_RTX;
    }
  if (! IN_RANGE_P (INTVAL (opval), 0, NUM_ACCS - 1))
    {
      error ("accumulator number is out of bounds");
      return NULL_RTX;
    }

  reg = gen_rtx_REG (insn_data[icode].operand[opnum].mode,
		     ACC_FIRST + INTVAL (opval));
  if (! (*insn_data[icode].operand[opnum].predicate) (reg, VOIDmode))
    REGNO (reg) = ACCG_FIRST + INTVAL (opval);

  if (! (*insn_data[icode].operand[opnum].predicate) (reg, VOIDmode))
    {
      error ("inappropriate accumulator for `%s'", insn_data[icode].name);
      return NULL_RTX;
    }
  return reg;
}

/* If an ACC rtx has mode MODE, return the mode that the matching ACCG
   should have.  */

static enum machine_mode
frv_matching_accg_mode (enum machine_mode mode)
{
  switch (mode)
    {
    case V4SImode:
      return V4QImode;

    case DImode:
      return HImode;

    case SImode:
      return QImode;

    default:
      abort ();
    }
}

/* Return the accumulator guard that should be paired with accumulator
   register ACC.  The mode of the returned register is in the same
   class as ACC, but is four times smaller.  */

rtx
frv_matching_accg_for_acc (rtx acc)
{
  return gen_rtx_REG (frv_matching_accg_mode (GET_MODE (acc)),
		      REGNO (acc) - ACC_FIRST + ACCG_FIRST);
}

/* Read a value from the head of the tree list pointed to by ARGLISTPTR.
   Return the value as an rtx and replace *ARGLISTPTR with the tail of the
   list.  */

static rtx
frv_read_argument (tree *arglistptr)
{
  tree next = TREE_VALUE (*arglistptr);
  *arglistptr = TREE_CHAIN (*arglistptr);
  return expand_expr (next, NULL_RTX, VOIDmode, 0);
}

/* Return true if OPVAL can be used for operand OPNUM of instruction ICODE.
   The instruction should require a constant operand of some sort.  The
   function prints an error if OPVAL is not valid.  */

static int
frv_check_constant_argument (enum insn_code icode, int opnum, rtx opval)
{
  if (GET_CODE (opval) != CONST_INT)
    {
      error ("`%s' expects a constant argument", insn_data[icode].name);
      return FALSE;
    }
  if (! (*insn_data[icode].operand[opnum].predicate) (opval, VOIDmode))
    {
      error ("constant argument out of range for `%s'", insn_data[icode].name);
      return FALSE;
    }
  return TRUE;
}

/* Return a legitimate rtx for instruction ICODE's return value.  Use TARGET
   if it's not null, has the right mode, and satisfies operand 0's
   predicate.  */

static rtx
frv_legitimize_target (enum insn_code icode, rtx target)
{
  enum machine_mode mode = insn_data[icode].operand[0].mode;

  if (! target
      || GET_MODE (target) != mode
      || ! (*insn_data[icode].operand[0].predicate) (target, mode))
    return gen_reg_rtx (mode);
  else
    return target;
}

/* Given that ARG is being passed as operand OPNUM to instruction ICODE,
   check whether ARG satisfies the operand's constraints.  If it doesn't,
   copy ARG to a temporary register and return that.  Otherwise return ARG
   itself.  */

static rtx
frv_legitimize_argument (enum insn_code icode, int opnum, rtx arg)
{
  enum machine_mode mode = insn_data[icode].operand[opnum].mode;

  if ((*insn_data[icode].operand[opnum].predicate) (arg, mode))
    return arg;
  else
    return copy_to_mode_reg (mode, arg);
}

/* Expand builtins that take a single, constant argument.  At the moment,
   only MHDSETS falls into this category.  */

static rtx
frv_expand_set_builtin (enum insn_code icode, tree arglist, rtx target)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);

  if (! frv_check_constant_argument (icode, 1, op0))
    return NULL_RTX;

  target = frv_legitimize_target (icode, target);
  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take one operand.  */

static rtx
frv_expand_unop_builtin (enum insn_code icode, tree arglist, rtx target)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);

  target = frv_legitimize_target (icode, target);
  op0 = frv_legitimize_argument (icode, 1, op0);
  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take two operands.  */

static rtx
frv_expand_binop_builtin (enum insn_code icode, tree arglist, rtx target)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);

  target = frv_legitimize_target (icode, target);
  op0 = frv_legitimize_argument (icode, 1, op0);
  op1 = frv_legitimize_argument (icode, 2, op1);
  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand cut-style builtins, which take two operands and an implicit ACCG
   one.  */

static rtx
frv_expand_cut_builtin (enum insn_code icode, tree arglist, rtx target)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);
  rtx op2;

  target = frv_legitimize_target (icode, target);
  op0 = frv_int_to_acc (icode, 1, op0);
  if (! op0)
    return NULL_RTX;

  if (icode == CODE_FOR_mdcutssi || GET_CODE (op1) == CONST_INT)
    {
      if (! frv_check_constant_argument (icode, 2, op1))
    	return NULL_RTX;
    }
  else
    op1 = frv_legitimize_argument (icode, 2, op1);

  op2 = frv_matching_accg_for_acc (op0);
  pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take two operands and the second is immediate.  */

static rtx
frv_expand_binopimm_builtin (enum insn_code icode, tree arglist, rtx target)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);

  if (! frv_check_constant_argument (icode, 2, op1))
    return NULL_RTX;

  target = frv_legitimize_target (icode, target);
  op0 = frv_legitimize_argument (icode, 1, op0);
  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand builtins that take two operands, the first operand being a pointer to
   ints and return void.  */

static rtx
frv_expand_voidbinop_builtin (enum insn_code icode, tree arglist)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);
  enum machine_mode mode0 = insn_data[icode].operand[0].mode;
  rtx addr;

  if (GET_CODE (op0) != MEM)
    {
      rtx reg = op0;

      if (! offsettable_address_p (0, mode0, op0))
	{
	  reg = gen_reg_rtx (Pmode);
	  emit_insn (gen_rtx_SET (VOIDmode, reg, op0));
	}

      op0 = gen_rtx_MEM (SImode, reg);
    }

  addr = XEXP (op0, 0);
  if (! offsettable_address_p (0, mode0, addr))
    addr = copy_to_mode_reg (Pmode, op0);

  op0 = change_address (op0, V4SImode, addr);
  op1 = frv_legitimize_argument (icode, 1, op1);
  pat = GEN_FCN (icode) (op0, op1);
  if (! pat)
    return 0;

  emit_insn (pat);
  return 0;
}

/* Expand builtins that take three operands and return void.  The first
   argument must be a constant that describes a pair or quad accumulators.  A
   fourth argument is created that is the accumulator guard register that
   corresponds to the accumulator.  */

static rtx
frv_expand_voidtriop_builtin (enum insn_code icode, tree arglist)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);
  rtx op2 = frv_read_argument (&arglist);
  rtx op3;

  op0 = frv_int_to_acc (icode, 0, op0);
  if (! op0)
    return NULL_RTX;

  op1 = frv_legitimize_argument (icode, 1, op1);
  op2 = frv_legitimize_argument (icode, 2, op2);
  op3 = frv_matching_accg_for_acc (op0);
  pat = GEN_FCN (icode) (op0, op1, op2, op3);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return NULL_RTX;
}

/* Expand builtins that perform accumulator-to-accumulator operations.
   These builtins take two accumulator numbers as argument and return
   void.  */

static rtx
frv_expand_voidaccop_builtin (enum insn_code icode, tree arglist)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);
  rtx op2;
  rtx op3;

  op0 = frv_int_to_acc (icode, 0, op0);
  if (! op0)
    return NULL_RTX;

  op1 = frv_int_to_acc (icode, 1, op1);
  if (! op1)
    return NULL_RTX;

  op2 = frv_matching_accg_for_acc (op0);
  op3 = frv_matching_accg_for_acc (op1);
  pat = GEN_FCN (icode) (op0, op1, op2, op3);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return NULL_RTX;
}

/* Expand the MCLRACC builtin.  This builtin takes a single accumulator
   number as argument.  */

static rtx
frv_expand_mclracc_builtin (tree arglist)
{
  enum insn_code icode = CODE_FOR_mclracc;
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);

  op0 = frv_int_to_acc (icode, 0, op0);
  if (! op0)
    return NULL_RTX;

  pat = GEN_FCN (icode) (op0);
  if (pat)
    emit_insn (pat);

  return NULL_RTX;
}

/* Expand builtins that take no arguments.  */

static rtx
frv_expand_noargs_builtin (enum insn_code icode)
{
  rtx pat = GEN_FCN (icode) (GEN_INT (0));
  if (pat)
    emit_insn (pat);

  return NULL_RTX;
}

/* Expand MRDACC and MRDACCG.  These builtins take a single accumulator
   number or accumulator guard number as argument and return an SI integer.  */

static rtx
frv_expand_mrdacc_builtin (enum insn_code icode, tree arglist)
{
  rtx pat;
  rtx target = gen_reg_rtx (SImode);
  rtx op0 = frv_read_argument (&arglist);

  op0 = frv_int_to_acc (icode, 1, op0);
  if (! op0)
    return NULL_RTX;

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Expand MWTACC and MWTACCG.  These builtins take an accumulator or
   accumulator guard as their first argument and an SImode value as their
   second.  */

static rtx
frv_expand_mwtacc_builtin (enum insn_code icode, tree arglist)
{
  rtx pat;
  rtx op0 = frv_read_argument (&arglist);
  rtx op1 = frv_read_argument (&arglist);

  op0 = frv_int_to_acc (icode, 0, op0);
  if (! op0)
    return NULL_RTX;

  op1 = frv_legitimize_argument (icode, 1, op1);
  pat = GEN_FCN (icode) (op0, op1);
  if (pat)
    emit_insn (pat);

  return NULL_RTX;
}

/* Expand builtins.  */

static rtx
frv_expand_builtin (tree exp,
                    rtx target,
                    rtx subtarget ATTRIBUTE_UNUSED,
                    enum machine_mode mode ATTRIBUTE_UNUSED,
                    int ignore ATTRIBUTE_UNUSED)
{
  tree arglist = TREE_OPERAND (exp, 1);
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned fcode = (unsigned)DECL_FUNCTION_CODE (fndecl);
  unsigned i;
  struct builtin_description *d;

  if (! TARGET_MEDIA)
    {
      error ("media functions are not available unless -mmedia is used");
      return NULL_RTX;
    }

  switch (fcode)
    {
    case FRV_BUILTIN_MCOP1:
    case FRV_BUILTIN_MCOP2:
    case FRV_BUILTIN_MDUNPACKH:
    case FRV_BUILTIN_MBTOHE:
      if (! TARGET_MEDIA_REV1)
	{
	  error ("this media function is only available on the fr500");
	  return NULL_RTX;
	}
      break;

    case FRV_BUILTIN_MQXMACHS:
    case FRV_BUILTIN_MQXMACXHS:
    case FRV_BUILTIN_MQMACXHS:
    case FRV_BUILTIN_MADDACCS:
    case FRV_BUILTIN_MSUBACCS:
    case FRV_BUILTIN_MASACCS:
    case FRV_BUILTIN_MDADDACCS:
    case FRV_BUILTIN_MDSUBACCS:
    case FRV_BUILTIN_MDASACCS:
    case FRV_BUILTIN_MABSHS:
    case FRV_BUILTIN_MDROTLI:
    case FRV_BUILTIN_MCPLHI:
    case FRV_BUILTIN_MCPLI:
    case FRV_BUILTIN_MDCUTSSI:
    case FRV_BUILTIN_MQSATHS:
    case FRV_BUILTIN_MHSETLOS:
    case FRV_BUILTIN_MHSETLOH:
    case FRV_BUILTIN_MHSETHIS:
    case FRV_BUILTIN_MHSETHIH:
    case FRV_BUILTIN_MHDSETS:
    case FRV_BUILTIN_MHDSETH:
      if (! TARGET_MEDIA_REV2)
	{
	  error ("this media function is only available on the fr400");
	  return NULL_RTX;
	}
      break;

    default:
      break;
    }

  /* Expand unique builtins.  */

  switch (fcode)
    {
    case FRV_BUILTIN_MTRAP:
      return frv_expand_noargs_builtin (CODE_FOR_mtrap);

    case FRV_BUILTIN_MCLRACC:
      return frv_expand_mclracc_builtin (arglist);

    case FRV_BUILTIN_MCLRACCA:
      if (TARGET_ACC_8)
	return frv_expand_noargs_builtin (CODE_FOR_mclracca8);
      else
	return frv_expand_noargs_builtin (CODE_FOR_mclracca4);

    case FRV_BUILTIN_MRDACC:
      return frv_expand_mrdacc_builtin (CODE_FOR_mrdacc, arglist);

    case FRV_BUILTIN_MRDACCG:
      return frv_expand_mrdacc_builtin (CODE_FOR_mrdaccg, arglist);

    case FRV_BUILTIN_MWTACC:
      return frv_expand_mwtacc_builtin (CODE_FOR_mwtacc, arglist);

    case FRV_BUILTIN_MWTACCG:
      return frv_expand_mwtacc_builtin (CODE_FOR_mwtaccg, arglist);

    default:
      break;
    }

  /* Expand groups of builtins.  */

  for (i = 0, d = bdesc_set; i < ARRAY_SIZE (bdesc_set); i++, d++)
    if (d->code == fcode)
      return frv_expand_set_builtin (d->icode, arglist, target);

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return frv_expand_unop_builtin (d->icode, arglist, target);

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return frv_expand_binop_builtin (d->icode, arglist, target);

  for (i = 0, d = bdesc_cut; i < ARRAY_SIZE (bdesc_cut); i++, d++)
    if (d->code == fcode)
      return frv_expand_cut_builtin (d->icode, arglist, target);

  for (i = 0, d = bdesc_2argimm; i < ARRAY_SIZE (bdesc_2argimm); i++, d++)
    if (d->code == fcode)
      return frv_expand_binopimm_builtin (d->icode, arglist, target);

  for (i = 0, d = bdesc_void2arg; i < ARRAY_SIZE (bdesc_void2arg); i++, d++)
    if (d->code == fcode)
      return frv_expand_voidbinop_builtin (d->icode, arglist);

  for (i = 0, d = bdesc_void3arg; i < ARRAY_SIZE (bdesc_void3arg); i++, d++)
    if (d->code == fcode)
      return frv_expand_voidtriop_builtin (d->icode, arglist);

  for (i = 0, d = bdesc_voidacc; i < ARRAY_SIZE (bdesc_voidacc); i++, d++)
    if (d->code == fcode)
      return frv_expand_voidaccop_builtin (d->icode, arglist);

  return 0;
}

static bool
frv_in_small_data_p (tree decl)
{
  HOST_WIDE_INT size;
  tree section_name;

  /* Don't apply the -G flag to internal compiler structures.  We
     should leave such structures in the main data section, partly
     for efficiency and partly because the size of some of them
     (such as C++ typeinfos) is not known until later.  */
  if (TREE_CODE (decl) != VAR_DECL || DECL_ARTIFICIAL (decl))
    return false;

  /* If we already know which section the decl should be in, see if
     it's a small data section.  */
  section_name = DECL_SECTION_NAME (decl);
  if (section_name)
    {
      if (TREE_CODE (section_name) != STRING_CST)
	abort ();
      if (frv_string_begins_with (section_name, ".sdata"))
	return true;
      if (frv_string_begins_with (section_name, ".sbss"))
	return true;
      return false;
    }

  size = int_size_in_bytes (TREE_TYPE (decl));
  if (size > 0 && (unsigned HOST_WIDE_INT) size <= g_switch_value)
    return true;

  return false;
}

static bool
frv_rtx_costs (rtx x,
               int code ATTRIBUTE_UNUSED,
               int outer_code ATTRIBUTE_UNUSED,
               int *total)
{
  switch (code)
    {
    case CONST_INT:
      /* Make 12 bit integers really cheap.  */
      if (IN_RANGE_P (INTVAL (x), -2048, 2047))
	{
	  *total = 0;
	  return true;
	}
      /* Fall through.  */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case PLUS:
    case MINUS:
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case NOT:
    case NEG:
    case COMPARE:
      if (GET_MODE (x) == SImode)
	*total = COSTS_N_INSNS (1);
      else if (GET_MODE (x) == DImode)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (3);
      return true;

    case MULT:
      if (GET_MODE (x) == SImode)
        *total = COSTS_N_INSNS (2);
      else
        *total = COSTS_N_INSNS (6);	/* guess */
      return true;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (18);
      return true;

    default:
      return false;
    }
}

static void
frv_asm_out_constructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  ctors_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer_with_op ("\t.picptr\t", symbol);
}

static void
frv_asm_out_destructor (rtx symbol, int priority ATTRIBUTE_UNUSED)
{
  dtors_section ();
  assemble_align (POINTER_SIZE);
  assemble_integer_with_op ("\t.picptr\t", symbol);
}
