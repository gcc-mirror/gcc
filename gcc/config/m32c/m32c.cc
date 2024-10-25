/* Target Code for R8C/M16C/M32C
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

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
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "builtins.h"
#include "opts.h"

/* This file should be included last.  */
#include "target-def.h"

/* Prototypes */

/* Used by m32c_pushm_popm.  */
typedef enum
{
  PP_pushm,
  PP_popm,
  PP_justcount
} Push_Pop_Type;

static bool m32c_function_needs_enter (void);
static tree interrupt_handler (tree *, tree, tree, int, bool *);
static tree function_vector_handler (tree *, tree, tree, int, bool *);
static int interrupt_p (tree node);
static int bank_switch_p (tree node);
static int fast_interrupt_p (tree node);
static int interrupt_p (tree node);
static bool m32c_asm_integer (rtx, unsigned int, int);
static int m32c_comp_type_attributes (const_tree, const_tree);
static bool m32c_fixed_condition_code_regs (unsigned int *, unsigned int *);
static struct machine_function *m32c_init_machine_status (void);
static void m32c_insert_attributes (tree, tree *);
static bool m32c_legitimate_address_p (machine_mode, rtx, bool,
				       code_helper = ERROR_MARK);
static bool m32c_addr_space_legitimate_address_p (machine_mode, rtx, bool,
						  addr_space_t,
						  code_helper = ERROR_MARK);
static rtx m32c_function_arg (cumulative_args_t, const function_arg_info &);
static bool m32c_pass_by_reference (cumulative_args_t,
				    const function_arg_info &);
static void m32c_function_arg_advance (cumulative_args_t,
				       const function_arg_info &);
static unsigned int m32c_function_arg_boundary (machine_mode, const_tree);
static int m32c_pushm_popm (Push_Pop_Type);
static bool m32c_strict_argument_naming (cumulative_args_t);
static rtx m32c_struct_value_rtx (tree, int);
static rtx m32c_subreg (machine_mode, rtx, machine_mode, int);
static int need_to_save (int);
static rtx m32c_function_value (const_tree, const_tree, bool);
static rtx m32c_libcall_value (machine_mode, const_rtx);

/* Returns true if an address is specified, else false.  */
static bool m32c_get_pragma_address (const char *varname, unsigned *addr);

static bool m32c_hard_regno_mode_ok (unsigned int, machine_mode);

#define SYMBOL_FLAG_FUNCVEC_FUNCTION    (SYMBOL_FLAG_MACH_DEP << 0)

#define streq(a,b) (strcmp ((a), (b)) == 0)

/* Internal support routines */

/* Debugging statements are tagged with DEBUG0 only so that they can
   be easily enabled individually, by replacing the '0' with '1' as
   needed.  */
#define DEBUG0 0
#define DEBUG1 1

#if DEBUG0
#include "print-tree.h"
/* This is needed by some of the commented-out debug statements
   below.  */
static char const *class_names[LIM_REG_CLASSES] = REG_CLASS_NAMES;
#endif
static int class_contents[LIM_REG_CLASSES][1] = REG_CLASS_CONTENTS;

/* These are all to support encode_pattern().  */
static char pattern[30], *patternp;
static GTY(()) rtx patternr[30];
#define RTX_IS(x) (streq (pattern, x))

/* Some macros to simplify the logic throughout this file.  */
#define IS_MEM_REGNO(regno) ((regno) >= MEM0_REGNO && (regno) <= MEM7_REGNO)
#define IS_MEM_REG(rtx) (GET_CODE (rtx) == REG && IS_MEM_REGNO (REGNO (rtx)))

#define IS_CR_REGNO(regno) ((regno) >= SB_REGNO && (regno) <= PC_REGNO)
#define IS_CR_REG(rtx) (GET_CODE (rtx) == REG && IS_CR_REGNO (REGNO (rtx)))

static int
far_addr_space_p (rtx x)
{
  if (GET_CODE (x) != MEM)
    return 0;
#if DEBUG0
  fprintf(stderr, "\033[35mfar_addr_space: "); debug_rtx(x);
  fprintf(stderr, " = %d\033[0m\n", MEM_ADDR_SPACE (x) == ADDR_SPACE_FAR);
#endif
  return MEM_ADDR_SPACE (x) == ADDR_SPACE_FAR;
}

/* We do most RTX matching by converting the RTX into a string, and
   using string compares.  This vastly simplifies the logic in many of
   the functions in this file.

   On exit, pattern[] has the encoded string (use RTX_IS("...") to
   compare it) and patternr[] has pointers to the nodes in the RTX
   corresponding to each character in the encoded string.  The latter
   is mostly used by print_operand().

   Unrecognized patterns have '?' in them; this shows up when the
   assembler complains about syntax errors.
*/

static void
encode_pattern_1 (rtx x)
{
  int i;

  if (patternp == pattern + sizeof (pattern) - 2)
    {
      patternp[-1] = '?';
      return;
    }

  patternr[patternp - pattern] = x;

  switch (GET_CODE (x))
    {
    case REG:
      *patternp++ = 'r';
      break;
    case SUBREG:
      if (GET_MODE_SIZE (GET_MODE (x)) !=
	  GET_MODE_SIZE (GET_MODE (XEXP (x, 0))))
	*patternp++ = 'S';
      if (GET_MODE (x) == PSImode
	  && GET_CODE (XEXP (x, 0)) == REG)
	*patternp++ = 'S';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case MEM:
      *patternp++ = 'm';
      /* FALLTHRU */
    case CONST:
      encode_pattern_1 (XEXP (x, 0));
      break;
    case SIGN_EXTEND:
      *patternp++ = '^';
      *patternp++ = 'S';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case ZERO_EXTEND:
      *patternp++ = '^';
      *patternp++ = 'Z';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case PLUS:
      *patternp++ = '+';
      encode_pattern_1 (XEXP (x, 0));
      encode_pattern_1 (XEXP (x, 1));
      break;
    case PRE_DEC:
      *patternp++ = '>';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case POST_INC:
      *patternp++ = '<';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case LO_SUM:
      *patternp++ = 'L';
      encode_pattern_1 (XEXP (x, 0));
      encode_pattern_1 (XEXP (x, 1));
      break;
    case HIGH:
      *patternp++ = 'H';
      encode_pattern_1 (XEXP (x, 0));
      break;
    case SYMBOL_REF:
      *patternp++ = 's';
      break;
    case LABEL_REF:
      *patternp++ = 'l';
      break;
    case CODE_LABEL:
      *patternp++ = 'c';
      break;
    case CONST_INT:
    case CONST_DOUBLE:
      *patternp++ = 'i';
      break;
    case UNSPEC:
      *patternp++ = 'u';
      *patternp++ = '0' + XCINT (x, 1, UNSPEC);
      for (i = 0; i < XVECLEN (x, 0); i++)
	encode_pattern_1 (XVECEXP (x, 0, i));
      break;
    case USE:
      *patternp++ = 'U';
      break;
    case PARALLEL:
      *patternp++ = '|';
      for (i = 0; i < XVECLEN (x, 0); i++)
	encode_pattern_1 (XVECEXP (x, 0, i));
      break;
    case EXPR_LIST:
      *patternp++ = 'E';
      encode_pattern_1 (XEXP (x, 0));
      if (XEXP (x, 1))
	encode_pattern_1 (XEXP (x, 1));
      break;
    default:
      *patternp++ = '?';
#if DEBUG0
      fprintf (stderr, "can't encode pattern %s\n",
	       GET_RTX_NAME (GET_CODE (x)));
      debug_rtx (x);
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

/* Since register names indicate the mode they're used in, we need a
   way to determine which name to refer to the register with.  Called
   by print_operand().  */

static const char *
reg_name_with_mode (int regno, machine_mode mode)
{
  int mlen = GET_MODE_SIZE (mode);
  if (regno == R0_REGNO && mlen == 1)
    return "r0l";
  if (regno == R0_REGNO && (mlen == 3 || mlen == 4))
    return "r2r0";
  if (regno == R0_REGNO && mlen == 6)
    return "r2r1r0";
  if (regno == R0_REGNO && mlen == 8)
    return "r3r1r2r0";
  if (regno == R1_REGNO && mlen == 1)
    return "r1l";
  if (regno == R1_REGNO && (mlen == 3 || mlen == 4))
    return "r3r1";
  if (regno == A0_REGNO && TARGET_A16 && (mlen == 3 || mlen == 4))
    return "a1a0";
  return reg_names[regno];
}

/* How many bytes a register uses on stack when it's pushed.  We need
   to know this because the push opcode needs to explicitly indicate
   the size of the register, even though the name of the register
   already tells it that.  Used by m32c_output_reg_{push,pop}, which
   is only used through calls to ASM_OUTPUT_REG_{PUSH,POP}.  */

static int
reg_push_size (int regno)
{
  switch (regno)
    {
    case R0_REGNO:
    case R1_REGNO:
      return 2;
    case R2_REGNO:
    case R3_REGNO:
    case FLG_REGNO:
      return 2;
    case A0_REGNO:
    case A1_REGNO:
    case SB_REGNO:
    case FB_REGNO:
    case SP_REGNO:
      if (TARGET_A16)
	return 2;
      else
	return 3;
    default:
      gcc_unreachable ();
    }
}

/* Given two register classes, find the largest intersection between
   them.  If there is no intersection, return RETURNED_IF_EMPTY
   instead.  */
static reg_class_t
reduce_class (reg_class_t original_class, reg_class_t limiting_class,
	      reg_class_t returned_if_empty)
{
  HARD_REG_SET cc;
  int i;
  reg_class_t best = NO_REGS;
  unsigned int best_size = 0;

  if (original_class == limiting_class)
    return original_class;

  cc = reg_class_contents[original_class] & reg_class_contents[limiting_class];

  for (i = 0; i < LIM_REG_CLASSES; i++)
    {
      if (hard_reg_set_subset_p (reg_class_contents[i], cc))
	if (best_size < reg_class_size[i])
	  {
	    best = (reg_class_t) i;
	    best_size = reg_class_size[i];
	  }

    }
  if (best == NO_REGS)
    return returned_if_empty;
  return best;
}

/* Used by m32c_register_move_cost to determine if a move is
   impossibly expensive.  */
static bool
class_can_hold_mode (reg_class_t rclass, machine_mode mode)
{
  /* Cache the results:  0=untested  1=no  2=yes */
  static char results[LIM_REG_CLASSES][MAX_MACHINE_MODE];

  if (results[(int) rclass][mode] == 0)
    {
      int r;
      results[rclass][mode] = 1;
      for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	if (in_hard_reg_set_p (reg_class_contents[(int) rclass], mode, r)
	    && m32c_hard_regno_mode_ok (r, mode))
	  {
	    results[rclass][mode] = 2;
	    break;
	  }
    }

#if DEBUG0
  fprintf (stderr, "class %s can hold %s? %s\n",
	   class_names[(int) rclass], mode_name[mode],
	   (results[rclass][mode] == 2) ? "yes" : "no");
#endif
  return results[(int) rclass][mode] == 2;
}

/* Run-time Target Specification.  */

/* Memregs are memory locations that gcc treats like general
   registers, as there are a limited number of true registers and the
   m32c families can use memory in most places that registers can be
   used.

   However, since memory accesses are more expensive than registers,
   we allow the user to limit the number of memregs available, in
   order to try to persuade gcc to try harder to use real registers.

   Memregs are provided by lib1funcs.S.
*/

int ok_to_change_target_memregs = TRUE;

/* Implements TARGET_OPTION_OVERRIDE.  */

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE m32c_option_override

static void
m32c_option_override (void)
{
  /* We limit memregs to 0..16, and provide a default.  */
  if (OPTION_SET_P (target_memregs))
    {
      if (target_memregs < 0 || target_memregs > 16)
	error ("invalid target memregs value %<%d%>", target_memregs);
    }
  else
    target_memregs = 16;

  if (TARGET_A24)
    flag_ivopts = 0;

  /* This target defaults to strict volatile bitfields.  */
  if (flag_strict_volatile_bitfields < 0 && abi_version_at_least(2))
    flag_strict_volatile_bitfields = 1;

  /* r8c/m16c have no 16-bit indirect call, so thunks are involved.
     This is always worse than an absolute call.  */
  if (TARGET_A16)
    flag_no_function_cse = 1;

  /* This wants to put insns between compares and their jumps.  */
  /* FIXME: The right solution is to properly trace the flags register
     values, but that is too much work for stage 4.  */
  flag_combine_stack_adjustments = 0;
}

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE m32c_override_options_after_change

static void
m32c_override_options_after_change (void)
{
  if (TARGET_A16)
    flag_no_function_cse = 1;
}

/* Defining data structures for per-function information */

/* The usual; we set up our machine_function data.  */
static struct machine_function *
m32c_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Implements INIT_EXPANDERS.  We just set up to call the above
   function.  */
void
m32c_init_expanders (void)
{
  init_machine_status = m32c_init_machine_status;
}

/* Storage Layout */

/* Register Basics */

/* Basic Characteristics of Registers */

/* Whether a mode fits in a register is complex enough to warrant a
   table.  */
static struct
{
  char qi_regs;
  char hi_regs;
  char pi_regs;
  char si_regs;
  char di_regs;
} nregs_table[FIRST_PSEUDO_REGISTER] =
{
  { 1, 1, 2, 2, 4 },		/* r0 */
  { 0, 1, 0, 0, 0 },		/* r2 */
  { 1, 1, 2, 2, 0 },		/* r1 */
  { 0, 1, 0, 0, 0 },		/* r3 */
  { 0, 1, 1, 0, 0 },		/* a0 */
  { 0, 1, 1, 0, 0 },		/* a1 */
  { 0, 1, 1, 0, 0 },		/* sb */
  { 0, 1, 1, 0, 0 },		/* fb */
  { 0, 1, 1, 0, 0 },		/* sp */
  { 1, 1, 1, 0, 0 },		/* pc */
  { 0, 0, 0, 0, 0 },		/* fl */
  { 1, 1, 1, 0, 0 },		/* ap */
  { 1, 1, 2, 2, 4 },		/* mem0 */
  { 1, 1, 2, 2, 4 },		/* mem1 */
  { 1, 1, 2, 2, 4 },		/* mem2 */
  { 1, 1, 2, 2, 4 },		/* mem3 */
  { 1, 1, 2, 2, 4 },		/* mem4 */
  { 1, 1, 2, 2, 0 },		/* mem5 */
  { 1, 1, 2, 2, 0 },		/* mem6 */
  { 1, 1, 0, 0, 0 },		/* mem7 */
};

/* Implements TARGET_CONDITIONAL_REGISTER_USAGE.  We adjust the number
   of available memregs, and select which registers need to be preserved
   across calls based on the chip family.  */

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE m32c_conditional_register_usage
void
m32c_conditional_register_usage (void)
{
  int i;

  if (target_memregs >= 0 && target_memregs <= 16)
    {
      /* The command line option is bytes, but our "registers" are
	 16-bit words.  */
      for (i = (target_memregs+1)/2; i < 8; i++)
	{
	  fixed_regs[MEM0_REGNO + i] = 1;
	  CLEAR_HARD_REG_BIT (reg_class_contents[MEM_REGS], MEM0_REGNO + i);
	}
    }

  /* M32CM and M32C preserve more registers across function calls.  */
  if (TARGET_A24)
    {
      call_used_regs[R1_REGNO] = 0;
      call_used_regs[R2_REGNO] = 0;
      call_used_regs[R3_REGNO] = 0;
      call_used_regs[A0_REGNO] = 0;
      call_used_regs[A1_REGNO] = 0;
    }
}

/* How Values Fit in Registers */

/* Implements TARGET_HARD_REGNO_NREGS.  This is complicated by the fact that
   different registers are different sizes from each other, *and* may
   be different sizes in different chip families.  */
static unsigned int
m32c_hard_regno_nregs_1 (unsigned int regno, machine_mode mode)
{
  if (regno == FLG_REGNO && mode == CCmode)
    return 1;
  if (regno >= FIRST_PSEUDO_REGISTER)
    return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);

  if (regno >= MEM0_REGNO && regno <= MEM7_REGNO)
    return (GET_MODE_SIZE (mode) + 1) / 2;

  if (GET_MODE_SIZE (mode) <= 1)
    return nregs_table[regno].qi_regs;
  if (GET_MODE_SIZE (mode) <= 2)
    return nregs_table[regno].hi_regs;
  if (regno == A0_REGNO && mode == SImode && TARGET_A16)
    return 2;
  if ((GET_MODE_SIZE (mode) <= 3 || mode == PSImode) && TARGET_A24)
    return nregs_table[regno].pi_regs;
  if (GET_MODE_SIZE (mode) <= 4)
    return nregs_table[regno].si_regs;
  if (GET_MODE_SIZE (mode) <= 8)
    return nregs_table[regno].di_regs;
  return 0;
}

static unsigned int
m32c_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  unsigned int rv = m32c_hard_regno_nregs_1 (regno, mode);
  return rv ? rv : 1;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  The above function does the work
   already; just test its return value.  */
static bool
m32c_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return m32c_hard_regno_nregs_1 (regno, mode) != 0;
}

/* Implement TARGET_MODES_TIEABLE_P.  In general, modes aren't tieable since
   registers are all different sizes.  However, since most modes are
   bigger than our registers anyway, it's easier to implement this
   function that way, leaving QImode as the only unique case.  */
static bool
m32c_modes_tieable_p (machine_mode m1, machine_mode m2)
{
  if (GET_MODE_SIZE (m1) == GET_MODE_SIZE (m2))
    return 1;

#if 0
  if (m1 == QImode || m2 == QImode)
    return 0;
#endif

  return 1;
}

/* Register Classes */

/* Implements REGNO_REG_CLASS.  */
enum reg_class
m32c_regno_reg_class (int regno)
{
  switch (regno)
    {
    case R0_REGNO:
      return R0_REGS;
    case R1_REGNO:
      return R1_REGS;
    case R2_REGNO:
      return R2_REGS;
    case R3_REGNO:
      return R3_REGS;
    case A0_REGNO:
      return A0_REGS;
    case A1_REGNO:
      return A1_REGS;
    case SB_REGNO:
      return SB_REGS;
    case FB_REGNO:
      return FB_REGS;
    case SP_REGNO:
      return SP_REGS;
    case FLG_REGNO:
      return FLG_REGS;
    default:
      if (IS_MEM_REGNO (regno))
	return MEM_REGS;
      return ALL_REGS;
    }
}

/* Implements REGNO_OK_FOR_BASE_P.  */
int
m32c_regno_ok_for_base_p (int regno)
{
  if (regno == A0_REGNO
      || regno == A1_REGNO || regno >= FIRST_PSEUDO_REGISTER)
    return 1;
  return 0;
}

/* Implements TARGET_PREFERRED_RELOAD_CLASS.  In general, prefer general
   registers of the appropriate size.  */

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS m32c_preferred_reload_class

static reg_class_t
m32c_preferred_reload_class (rtx x, reg_class_t rclass)
{
  reg_class_t newclass = rclass;

#if DEBUG0
  fprintf (stderr, "\npreferred_reload_class for %s is ",
	   class_names[rclass]);
#endif
  if (rclass == NO_REGS)
    rclass = GET_MODE (x) == QImode ? HL_REGS : R03_REGS;

  if (reg_classes_intersect_p (rclass, CR_REGS))
    {
      switch (GET_MODE (x))
	{
	case E_QImode:
	  newclass = HL_REGS;
	  break;
	default:
	  /*      newclass = HI_REGS; */
	  break;
	}
    }

  else if (newclass == QI_REGS && GET_MODE_SIZE (GET_MODE (x)) > 2)
    newclass = SI_REGS;
  else if (GET_MODE_SIZE (GET_MODE (x)) > 4
	   && ! reg_class_subset_p (R03_REGS, rclass))
    newclass = DI_REGS;

  rclass = reduce_class (rclass, newclass, rclass);

  if (GET_MODE (x) == QImode)
    rclass = reduce_class (rclass, HL_REGS, rclass);

#if DEBUG0
  fprintf (stderr, "%s\n", class_names[rclass]);
  debug_rtx (x);

  if (GET_CODE (x) == MEM
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == PLUS)
    fprintf (stderr, "Glorm!\n");
#endif
  return rclass;
}

/* Implements TARGET_PREFERRED_OUTPUT_RELOAD_CLASS.  */

#undef TARGET_PREFERRED_OUTPUT_RELOAD_CLASS
#define TARGET_PREFERRED_OUTPUT_RELOAD_CLASS m32c_preferred_output_reload_class

static reg_class_t
m32c_preferred_output_reload_class (rtx x, reg_class_t rclass)
{
  return m32c_preferred_reload_class (x, rclass);
}

/* Implements LIMIT_RELOAD_CLASS.  We basically want to avoid using
   address registers for reloads since they're needed for address
   reloads.  */
int
m32c_limit_reload_class (machine_mode mode, int rclass)
{
#if DEBUG0
  fprintf (stderr, "limit_reload_class for %s: %s ->",
	   mode_name[mode], class_names[rclass]);
#endif

  if (mode == QImode)
    rclass = reduce_class (rclass, HL_REGS, rclass);
  else if (mode == HImode)
    rclass = reduce_class (rclass, HI_REGS, rclass);
  else if (mode == SImode)
    rclass = reduce_class (rclass, SI_REGS, rclass);

  if (rclass != A_REGS)
    rclass = reduce_class (rclass, DI_REGS, rclass);

#if DEBUG0
  fprintf (stderr, " %s\n", class_names[rclass]);
#endif
  return rclass;
}

/* Implements SECONDARY_RELOAD_CLASS.  QImode have to be reloaded in
   r0 or r1, as those are the only real QImode registers.  CR regs get
   reloaded through appropriately sized general or address
   registers.  */
int
m32c_secondary_reload_class (int rclass, machine_mode mode, rtx x)
{
  int cc = class_contents[rclass][0];
#if DEBUG0
  fprintf (stderr, "\nsecondary reload class %s %s\n",
	   class_names[rclass], mode_name[mode]);
  debug_rtx (x);
#endif
  if (mode == QImode
      && GET_CODE (x) == MEM && (cc & ~class_contents[R23_REGS][0]) == 0)
    return QI_REGS;
  if (reg_classes_intersect_p (rclass, CR_REGS)
      && GET_CODE (x) == REG
      && REGNO (x) >= SB_REGNO && REGNO (x) <= SP_REGNO)
    return (TARGET_A16 || mode == HImode) ? HI_REGS : A_REGS;
  return NO_REGS;
}

/* Implements TARGET_CLASS_LIKELY_SPILLED_P.  A_REGS is needed for address
   reloads.  */

#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P m32c_class_likely_spilled_p

static bool
m32c_class_likely_spilled_p (reg_class_t regclass)
{
  if (regclass == A_REGS)
    return true;

  return (reg_class_size[(int) regclass] == 1);
}

/* Implements TARGET_CLASS_MAX_NREGS.  We calculate this according to its
   documented meaning, to avoid potential inconsistencies with actual
   class definitions.  */

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS m32c_class_max_nregs

static unsigned char
m32c_class_max_nregs (reg_class_t regclass, machine_mode mode)
{
  int rn;
  unsigned char max = 0;

  for (rn = 0; rn < FIRST_PSEUDO_REGISTER; rn++)
    if (TEST_HARD_REG_BIT (reg_class_contents[(int) regclass], rn))
      {
	unsigned char n = m32c_hard_regno_nregs (rn, mode);
	if (max < n)
	  max = n;
      }
  return max;
}

/* Implements TARGET_CAN_CHANGE_MODE_CLASS.  Only r0 and r1 can change to
   QI (r0l, r1l) because the chip doesn't support QI ops on other
   registers (well, it does on a0/a1 but if we let gcc do that, reload
   suffers).  Otherwise, we allow changes to larger modes.  */
static bool
m32c_can_change_mode_class (machine_mode from,
			    machine_mode to, reg_class_t rclass)
{
  int rn;
#if DEBUG0
  fprintf (stderr, "can change from %s to %s in %s\n",
	   mode_name[from], mode_name[to], class_names[rclass]);
#endif

  /* If the larger mode isn't allowed in any of these registers, we
     can't allow the change.  */
  for (rn = 0; rn < FIRST_PSEUDO_REGISTER; rn++)
    if (class_contents[rclass][0] & (1 << rn))
      if (! m32c_hard_regno_mode_ok (rn, to))
	return false;

  if (to == QImode)
    return (class_contents[rclass][0] & 0x1ffa) == 0;

  if (class_contents[rclass][0] & 0x0005	/* r0, r1 */
      && GET_MODE_SIZE (from) > 1)
    return true;
  if (GET_MODE_SIZE (from) > 2)	/* all other regs */
    return true;

  return false;
}

/* Helpers for the rest of the file.  */
/* TRUE if the rtx is a REG rtx for the given register.  */
#define IS_REG(rtx,regno) (GET_CODE (rtx) == REG \
			   && REGNO (rtx) == regno)
/* TRUE if the rtx is a pseudo - specifically, one we can use as a
   base register in address calculations (hence the "strict"
   argument).  */
#define IS_PSEUDO(rtx,strict) (!strict && GET_CODE (rtx) == REG \
			       && (REGNO (rtx) == AP_REGNO \
				   || REGNO (rtx) >= FIRST_PSEUDO_REGISTER))

#define A0_OR_PSEUDO(x) (IS_REG(x, A0_REGNO) || REGNO (x) >= FIRST_PSEUDO_REGISTER)

/* Implements matching for constraints (see next function too).  'S' is
   for memory constraints, plus "Rpa" for PARALLEL rtx's we use for
   call return values.  */
bool
m32c_matches_constraint_p (rtx value, int constraint)
{
  encode_pattern (value);

  switch (constraint) {
  case CONSTRAINT_SF:
    return (far_addr_space_p (value)
	    && ((RTX_IS ("mr")
		 && A0_OR_PSEUDO (patternr[1])
		 && GET_MODE (patternr[1]) == SImode)
		|| (RTX_IS ("m+^Sri")
		    && A0_OR_PSEUDO (patternr[4])
		    && GET_MODE (patternr[4]) == HImode)
		|| (RTX_IS ("m+^Srs")
		    && A0_OR_PSEUDO (patternr[4])
		    && GET_MODE (patternr[4]) == HImode)
		|| (RTX_IS ("m+^S+ris")
		    && A0_OR_PSEUDO (patternr[5])
		    && GET_MODE (patternr[5]) == HImode)
		|| RTX_IS ("ms")));
  case CONSTRAINT_Sd:
    {
      /* This is the common "src/dest" address */
      rtx r;
      if (GET_CODE (value) == MEM && CONSTANT_P (XEXP (value, 0)))
	return true;
      if (RTX_IS ("ms") || RTX_IS ("m+si"))
	return true;
      if (RTX_IS ("m++rii"))
	{
	  if (REGNO (patternr[3]) == FB_REGNO
	      && INTVAL (patternr[4]) == 0)
	    return true;
	}
      if (RTX_IS ("mr"))
	r = patternr[1];
      else if (RTX_IS ("m+ri") || RTX_IS ("m+rs") || RTX_IS ("m+r+si"))
	r = patternr[2];
      else
	return false;
      if (REGNO (r) == SP_REGNO)
	return false;
      return m32c_legitimate_address_p (GET_MODE (value), XEXP (value, 0), 1);
    }
  case CONSTRAINT_Sa:
    {
      rtx r;
      if (RTX_IS ("mr"))
	r = patternr[1];
      else if (RTX_IS ("m+ri"))
	r = patternr[2];
      else
	return false;
      return (IS_REG (r, A0_REGNO) || IS_REG (r, A1_REGNO));
    }
  case CONSTRAINT_Si:
    return (RTX_IS ("mi") || RTX_IS ("ms") || RTX_IS ("m+si"));
  case CONSTRAINT_Ss:
    return ((RTX_IS ("mr")
	     && (IS_REG (patternr[1], SP_REGNO)))
	    || (RTX_IS ("m+ri") && (IS_REG (patternr[2], SP_REGNO))));
  case CONSTRAINT_Sf:
    return ((RTX_IS ("mr")
	     && (IS_REG (patternr[1], FB_REGNO)))
	    || (RTX_IS ("m+ri") && (IS_REG (patternr[2], FB_REGNO))));
  case CONSTRAINT_Sb:
    return ((RTX_IS ("mr")
	     && (IS_REG (patternr[1], SB_REGNO)))
	    || (RTX_IS ("m+ri") && (IS_REG (patternr[2], SB_REGNO))));
  case CONSTRAINT_Sp:
    /* Absolute addresses 0..0x1fff used for bit addressing (I/O ports) */
    return (RTX_IS ("mi")
	    && !(INTVAL (patternr[1]) & ~0x1fff));
  case CONSTRAINT_S1:
    return r1h_operand (value, QImode);
  case CONSTRAINT_Rpa:
    return GET_CODE (value) == PARALLEL;
  default:
    return false;
  }
}

/* STACK AND CALLING */

/* Frame Layout */

/* Implements RETURN_ADDR_RTX.  Note that R8C and M16C push 24 bits
   (yes, THREE bytes) onto the stack for the return address, but we
   don't support pointers bigger than 16 bits on those chips.  This
   will likely wreak havoc with exception unwinding.  FIXME.  */
rtx
m32c_return_addr_rtx (int count)
{
  machine_mode mode;
  int offset;
  rtx ra_mem;

  if (count)
    return NULL_RTX;
  /* we want 2[$fb] */

  if (TARGET_A24)
    {
      /* It's four bytes */
      mode = PSImode;
      offset = 4;
    }
  else
    {
      /* FIXME: it's really 3 bytes */
      mode = HImode;
      offset = 2;
    }

  ra_mem =
    gen_rtx_MEM (mode, plus_constant (Pmode, gen_rtx_REG (Pmode, FP_REGNO),
				      offset));
  return copy_to_mode_reg (mode, ra_mem);
}

/* Implements INCOMING_RETURN_ADDR_RTX.  See comment above.  */
rtx
m32c_incoming_return_addr_rtx (void)
{
  /* we want [sp] */
  return gen_rtx_MEM (PSImode, gen_rtx_REG (PSImode, SP_REGNO));
}

/* Exception Handling Support */

/* Implements EH_RETURN_DATA_REGNO.  Choose registers able to hold
   pointers.  */
int
m32c_eh_return_data_regno (int n)
{
  switch (n)
    {
    case 0:
      return MEM0_REGNO;
    case 1:
      return MEM0_REGNO+4;
    default:
      return INVALID_REGNUM;
    }
}

/* Implements EH_RETURN_STACKADJ_RTX.  Saved and used later in
   m32c_emit_eh_epilogue.  */
rtx
m32c_eh_return_stackadj_rtx (void)
{
  if (!cfun->machine->eh_stack_adjust)
    {
      rtx sa;

      sa = gen_rtx_REG (Pmode, R0_REGNO);
      cfun->machine->eh_stack_adjust = sa;
    }
  return cfun->machine->eh_stack_adjust;
}

/* Registers That Address the Stack Frame */

/* Implements DWARF_FRAME_REGNUM and DEBUGGER_REGNO.  Note that
   the original spec called for dwarf numbers to vary with register
   width as well, for example, r0l, r0, and r2r0 would each have
   different dwarf numbers.  GCC doesn't support this, and we don't do
   it, and gdb seems to like it this way anyway.  */
unsigned int
m32c_dwarf_frame_regnum (int n)
{
  switch (n)
    {
    case R0_REGNO:
      return 5;
    case R1_REGNO:
      return 6;
    case R2_REGNO:
      return 7;
    case R3_REGNO:
      return 8;
    case A0_REGNO:
      return 9;
    case A1_REGNO:
      return 10;
    case FB_REGNO:
      return 11;
    case SB_REGNO:
      return 19;

    case SP_REGNO:
      return 12;
    case PC_REGNO:
      return 13;
    default:
      return DWARF_FRAME_REGISTERS + 1;
    }
}

/* The frame looks like this:

   ap -> +------------------------------
         | Return address (3 or 4 bytes)
	 | Saved FB (2 or 4 bytes)
   fb -> +------------------------------
	 | local vars
         | register saves fb
	 |        through r0 as needed
   sp -> +------------------------------
*/

/* We use this to wrap all emitted insns in the prologue.  */
static rtx
F (rtx x)
{
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* This maps register numbers to the PUSHM/POPM bitfield, and tells us
   how much the stack pointer moves for each, for each cpu family.  */
static struct
{
  int reg1;
  int bit;
  int a16_bytes;
  int a24_bytes;
} pushm_info[] =
{
  /* These are in reverse push (nearest-to-sp) order.  */
  { R0_REGNO, 0x80, 2, 2 },
  { R1_REGNO, 0x40, 2, 2 },
  { R2_REGNO, 0x20, 2, 2 },
  { R3_REGNO, 0x10, 2, 2 },
  { A0_REGNO, 0x08, 2, 4 },
  { A1_REGNO, 0x04, 2, 4 },
  { SB_REGNO, 0x02, 2, 4 },
  { FB_REGNO, 0x01, 2, 4 }
};

#define PUSHM_N (ARRAY_SIZE (pushm_info))

/* Returns TRUE if we need to save/restore the given register.  We
   save everything for exception handlers, so that any register can be
   unwound.  For interrupt handlers, we save everything if the handler
   calls something else (because we don't know what *that* function
   might do), but try to be a bit smarter if the handler is a leaf
   function.  We always save $a0, though, because we use that in the
   epilogue to copy $fb to $sp.  */
static int
need_to_save (int regno)
{
  if (fixed_regs[regno])
    return 0;
  if (crtl->calls_eh_return)
    return 1;
  if (regno == FP_REGNO)
    return 0;
  if (cfun->machine->is_interrupt
      && (!cfun->machine->is_leaf
	  || (regno == A0_REGNO
	      && m32c_function_needs_enter ())
	  ))
    return 1;
  if (df_regs_ever_live_p (regno)
      && (!call_used_or_fixed_reg_p (regno) || cfun->machine->is_interrupt))
    return 1;
  return 0;
}

/* This function contains all the intelligence about saving and
   restoring registers.  It always figures out the register save set.
   When called with PP_justcount, it merely returns the size of the
   save set (for eliminating the frame pointer, for example).  When
   called with PP_pushm or PP_popm, it emits the appropriate
   instructions for saving (pushm) or restoring (popm) the
   registers.  */
static int
m32c_pushm_popm (Push_Pop_Type ppt)
{
  int reg_mask = 0;
  int byte_count = 0, bytes;
  int i;
  rtx dwarf_set[PUSHM_N];
  int n_dwarfs = 0;
  int nosave_mask = 0;

  if (crtl->return_rtx
      && GET_CODE (crtl->return_rtx) == PARALLEL
      && !(crtl->calls_eh_return || cfun->machine->is_interrupt))
    {
      rtx exp = XVECEXP (crtl->return_rtx, 0, 0);
      rtx rv = XEXP (exp, 0);
      int rv_bytes = GET_MODE_SIZE (GET_MODE (rv));

      if (rv_bytes > 2)
	nosave_mask |= 0x20;	/* PSI, SI */
      else
	nosave_mask |= 0xf0;	/* DF */
      if (rv_bytes > 4)
	nosave_mask |= 0x50;	/* DI */
    }

  for (i = 0; i < (int) PUSHM_N; i++)
    {
      /* Skip if neither register needs saving.  */
      if (!need_to_save (pushm_info[i].reg1))
	continue;

      if (pushm_info[i].bit & nosave_mask)
	continue;

      reg_mask |= pushm_info[i].bit;
      bytes = TARGET_A16 ? pushm_info[i].a16_bytes : pushm_info[i].a24_bytes;

      if (ppt == PP_pushm)
	{
	  machine_mode mode = (bytes == 2) ? HImode : SImode;
	  rtx addr;

	  /* Always use stack_pointer_rtx instead of calling
	     rtx_gen_REG ourselves.  Code elsewhere in GCC assumes
	     that there is a single rtx representing the stack pointer,
	     namely stack_pointer_rtx, and uses == to recognize it.  */
	  addr = stack_pointer_rtx;

	  if (byte_count != 0)
	    addr = gen_rtx_PLUS (GET_MODE (addr), addr, GEN_INT (byte_count));

	  dwarf_set[n_dwarfs++] =
	    gen_rtx_SET (gen_rtx_MEM (mode, addr),
			 gen_rtx_REG (mode, pushm_info[i].reg1));
	  F (dwarf_set[n_dwarfs - 1]);

	}
      byte_count += bytes;
    }

  if (cfun->machine->is_interrupt)
    {
      cfun->machine->intr_pushm = reg_mask & 0xfe;
      reg_mask = 0;
      byte_count = 0;
    }

  if (cfun->machine->is_interrupt)
    for (i = MEM0_REGNO; i <= MEM7_REGNO; i++)
      if (need_to_save (i))
	{
	  byte_count += 2;
	  cfun->machine->intr_pushmem[i - MEM0_REGNO] = 1;
	}

  if (ppt == PP_pushm && byte_count)
    {
      rtx note = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (n_dwarfs + 1));
      rtx pushm;

      if (reg_mask)
	{
	  XVECEXP (note, 0, 0)
	    = gen_rtx_SET (stack_pointer_rtx,
			   gen_rtx_PLUS (GET_MODE (stack_pointer_rtx),
					 stack_pointer_rtx,
					 GEN_INT (-byte_count)));
	  F (XVECEXP (note, 0, 0));

	  for (i = 0; i < n_dwarfs; i++)
	    XVECEXP (note, 0, i + 1) = dwarf_set[i];

	  pushm = F (emit_insn (gen_pushm (GEN_INT (reg_mask))));

	  add_reg_note (pushm, REG_FRAME_RELATED_EXPR, note);
	}

      if (cfun->machine->is_interrupt)
	for (i = MEM0_REGNO; i <= MEM7_REGNO; i++)
	  if (cfun->machine->intr_pushmem[i - MEM0_REGNO])
	    {
	      if (TARGET_A16)
		pushm = emit_insn (gen_pushhi_16 (gen_rtx_REG (HImode, i)));
	      else
		pushm = emit_insn (gen_pushhi_24 (gen_rtx_REG (HImode, i)));
	      F (pushm);
	    }
    }
  if (ppt == PP_popm && byte_count)
    {
      if (cfun->machine->is_interrupt)
	for (i = MEM7_REGNO; i >= MEM0_REGNO; i--)
	  if (cfun->machine->intr_pushmem[i - MEM0_REGNO])
	    {
	      if (TARGET_A16)
		emit_insn (gen_pophi_16 (gen_rtx_REG (HImode, i)));
	      else
		emit_insn (gen_pophi_24 (gen_rtx_REG (HImode, i)));
	    }
      if (reg_mask)
	emit_insn (gen_popm (GEN_INT (reg_mask)));
    }

  return byte_count;
}

/* Implements INITIAL_ELIMINATION_OFFSET.  See the comment above that
   diagrams our call frame.  */
int
m32c_initial_elimination_offset (int from, int to)
{
  int ofs = 0;

  if (from == AP_REGNO)
    {
      if (TARGET_A16)
	ofs += 5;
      else
	ofs += 8;
    }

  if (to == SP_REGNO)
    {
      ofs += m32c_pushm_popm (PP_justcount);
      ofs += get_frame_size ();
    }

  /* Account for push rounding.  */
  if (TARGET_A24)
    ofs = (ofs + 1) & ~1;
#if DEBUG0
  fprintf (stderr, "initial_elimination_offset from=%d to=%d, ofs=%d\n", from,
	   to, ofs);
#endif
  return ofs;
}

/* Passing Function Arguments on the Stack */

/* Implements PUSH_ROUNDING.  The R8C and M16C have byte stacks, the
   M32C has word stacks.  */
poly_int64
m32c_push_rounding (poly_int64 n)
{
  if (TARGET_R8C || TARGET_M16C)
    return n;
  return (n + 1) & ~1;
}

#undef TARGET_PUSH_ARGUMENT
#define TARGET_PUSH_ARGUMENT hook_bool_uint_true

/* Passing Arguments in Registers */

/* Implements TARGET_FUNCTION_ARG.  Arguments are passed partly in
   registers, partly on stack.  If our function returns a struct, a
   pointer to a buffer for it is at the top of the stack (last thing
   pushed).  The first few real arguments may be in registers as
   follows:

   R8C/M16C:	arg1 in r1 if it's QI or HI (else it's pushed on stack)
		arg2 in r2 if it's HI (else pushed on stack)
		rest on stack
   M32C:        arg1 in r0 if it's QI or HI (else it's pushed on stack)
		rest on stack

   Structs are not passed in registers, even if they fit.  Only
   integer and pointer types are passed in registers.

   Note that when arg1 doesn't fit in r1, arg2 may still be passed in
   r2 if it fits.  */
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG m32c_function_arg
static rtx
m32c_function_arg (cumulative_args_t ca_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  /* Can return a reg, parallel, or 0 for stack */
  rtx rv = NULL_RTX;
#if DEBUG0
  fprintf (stderr, "func_arg %d (%s, %d)\n",
	   ca->parm_num, mode_name[arg.mode], arg.named);
  debug_tree (arg.type);
#endif

  if (arg.end_marker_p ())
    return GEN_INT (0);

  if (ca->force_mem || !arg.named)
    {
#if DEBUG0
      fprintf (stderr, "func arg: force %d named %d, mem\n", ca->force_mem,
	       arg.named);
#endif
      return NULL_RTX;
    }

  if (arg.type && INTEGRAL_TYPE_P (arg.type) && POINTER_TYPE_P (arg.type))
    return NULL_RTX;

  if (arg.aggregate_type_p ())
    return NULL_RTX;

  switch (ca->parm_num)
    {
    case 1:
      if (GET_MODE_SIZE (arg.mode) == 1 || GET_MODE_SIZE (arg.mode) == 2)
	rv = gen_rtx_REG (arg.mode, TARGET_A16 ? R1_REGNO : R0_REGNO);
      break;

    case 2:
      if (TARGET_A16 && GET_MODE_SIZE (arg.mode) == 2)
	rv = gen_rtx_REG (arg.mode, R2_REGNO);
      break;
    }

#if DEBUG0
  debug_rtx (rv);
#endif
  return rv;
}

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE m32c_pass_by_reference
static bool
m32c_pass_by_reference (cumulative_args_t, const function_arg_info &)
{
  return 0;
}

/* Implements INIT_CUMULATIVE_ARGS.  */
void
m32c_init_cumulative_args (CUMULATIVE_ARGS * ca,
			   tree fntype,
			   rtx libname ATTRIBUTE_UNUSED,
			   tree fndecl,
			   int n_named_args ATTRIBUTE_UNUSED)
{
  if (fntype && aggregate_value_p (TREE_TYPE (fntype), fndecl))
    ca->force_mem = 1;
  else
    ca->force_mem = 0;
  ca->parm_num = 1;
}

/* Implements TARGET_FUNCTION_ARG_ADVANCE.  force_mem is set for
   functions returning structures, so we always reset that.  Otherwise,
   we only need to know the sequence number of the argument to know what
   to do with it.  */
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE m32c_function_arg_advance
static void
m32c_function_arg_advance (cumulative_args_t ca_v,
			   const function_arg_info &)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (ca_v);

  if (ca->force_mem)
    ca->force_mem = 0;
  else
    ca->parm_num++;
}

/* Implements TARGET_FUNCTION_ARG_BOUNDARY.  */
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY m32c_function_arg_boundary
static unsigned int
m32c_function_arg_boundary (machine_mode mode ATTRIBUTE_UNUSED,
			    const_tree type ATTRIBUTE_UNUSED)
{
  return (TARGET_A16 ? 8 : 16);
}

/* Implements FUNCTION_ARG_REGNO_P.  */
int
m32c_function_arg_regno_p (int r)
{
  if (TARGET_A24)
    return (r == R0_REGNO);
  return (r == R1_REGNO || r == R2_REGNO);
}

/* HImode and PSImode are the two "native" modes as far as GCC is
   concerned, but the chips also support a 32-bit mode which is used
   for some opcodes in R8C/M16C and for reset vectors and such.  */
#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE m32c_valid_pointer_mode
static bool
m32c_valid_pointer_mode (scalar_int_mode mode)
{
  if (mode == HImode
      || mode == PSImode
      || mode == SImode
      )
    return 1;
  return 0;
}

/* How Scalar Function Values Are Returned */

/* Implements TARGET_LIBCALL_VALUE.  Most values are returned in $r0, or some
   combination of registers starting there (r2r0 for longs, r3r1r2r0
   for long long, r3r2r1r0 for doubles), except that that ABI
   currently doesn't work because it ends up using all available
   general registers and gcc often can't compile it.  So, instead, we
   return anything bigger than 16 bits in "mem0" (effectively, a
   memory location).  */

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE m32c_libcall_value

static rtx
m32c_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  /* return reg or parallel */
#if 0
  /* FIXME: GCC has difficulty returning large values in registers,
     because that ties up most of the general registers and gives the
     register allocator little to work with.  Until we can resolve
     this, large values are returned in memory.  */
  if (mode == DFmode)
    {
      rtx rv;

      rv = gen_rtx_PARALLEL (mode, rtvec_alloc (4));
      XVECEXP (rv, 0, 0) = gen_rtx_EXPR_LIST (VOIDmode,
					      gen_rtx_REG (HImode,
							   R0_REGNO),
					      GEN_INT (0));
      XVECEXP (rv, 0, 1) = gen_rtx_EXPR_LIST (VOIDmode,
					      gen_rtx_REG (HImode,
							   R1_REGNO),
					      GEN_INT (2));
      XVECEXP (rv, 0, 2) = gen_rtx_EXPR_LIST (VOIDmode,
					      gen_rtx_REG (HImode,
							   R2_REGNO),
					      GEN_INT (4));
      XVECEXP (rv, 0, 3) = gen_rtx_EXPR_LIST (VOIDmode,
					      gen_rtx_REG (HImode,
							   R3_REGNO),
					      GEN_INT (6));
      return rv;
    }

  if (TARGET_A24 && GET_MODE_SIZE (mode) > 2)
    {
      rtx rv;

      rv = gen_rtx_PARALLEL (mode, rtvec_alloc (1));
      XVECEXP (rv, 0, 0) = gen_rtx_EXPR_LIST (VOIDmode,
					      gen_rtx_REG (mode,
							   R0_REGNO),
					      GEN_INT (0));
      return rv;
    }
#endif

  if (GET_MODE_SIZE (mode) > 2)
    return gen_rtx_REG (mode, MEM0_REGNO);
  return gen_rtx_REG (mode, R0_REGNO);
}

/* Implements TARGET_FUNCTION_VALUE.  Functions and libcalls have the same
   conventions.  */

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE m32c_function_value

static rtx
m32c_function_value (const_tree valtype,
		     const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  /* return reg or parallel */
  const machine_mode mode = TYPE_MODE (valtype);
  return m32c_libcall_value (mode, NULL_RTX);
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.  */

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P m32c_function_value_regno_p

static bool
m32c_function_value_regno_p (const unsigned int regno)
{
  return (regno == R0_REGNO || regno == MEM0_REGNO);
}

/* How Large Values Are Returned */

/* We return structures by pushing the address on the stack, even if
   we use registers for the first few "real" arguments.  */
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX m32c_struct_value_rtx
static rtx
m32c_struct_value_rtx (tree fndecl ATTRIBUTE_UNUSED,
		       int incoming ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Function Entry and Exit */

/* Implements EPILOGUE_USES.  Interrupts restore all registers.  */
int
m32c_epilogue_uses (int regno ATTRIBUTE_UNUSED)
{
  if (cfun->machine->is_interrupt)
    return 1;
  return 0;
}

/* Implementing the Varargs Macros */

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING m32c_strict_argument_naming
static bool
m32c_strict_argument_naming (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  return 1;
}

/* Trampolines for Nested Functions */

/*
   m16c:
   1 0000 75C43412              mov.w   #0x1234,a0
   2 0004 FC000000              jmp.a   label

   m32c:
   1 0000 BC563412              mov.l:s #0x123456,a0
   2 0004 CC000000              jmp.a   label
*/

/* Implements TRAMPOLINE_SIZE.  */
int
m32c_trampoline_size (void)
{
  /* Allocate extra space so we can avoid the messy shifts when we
     initialize the trampoline; we just write past the end of the
     opcode.  */
  return TARGET_A16 ? 8 : 10;
}

/* Implements TRAMPOLINE_ALIGNMENT.  */
int
m32c_trampoline_alignment (void)
{
  return 2;
}

/* Implements TARGET_TRAMPOLINE_INIT.  */

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT m32c_trampoline_init
static void
m32c_trampoline_init (rtx m_tramp, tree fndecl, rtx chainval)
{
  rtx function = XEXP (DECL_RTL (fndecl), 0);

#define A0(m,i) adjust_address (m_tramp, m, i)
  if (TARGET_A16)
    {
      /* Note: we subtract a "word" because the moves want signed
	 constants, not unsigned constants.  */
      emit_move_insn (A0 (HImode, 0), GEN_INT (0xc475 - 0x10000));
      emit_move_insn (A0 (HImode, 2), chainval);
      emit_move_insn (A0 (QImode, 4), GEN_INT (0xfc - 0x100));
      /* We use 16-bit addresses here, but store the zero to turn it
	 into a 24-bit offset.  */
      emit_move_insn (A0 (HImode, 5), function);
      emit_move_insn (A0 (QImode, 7), GEN_INT (0x00));
    }
  else
    {
      /* Note that the PSI moves actually write 4 bytes.  Make sure we
	 write stuff out in the right order, and leave room for the
	 extra byte at the end.  */
      emit_move_insn (A0 (QImode, 0), GEN_INT (0xbc - 0x100));
      emit_move_insn (A0 (PSImode, 1), chainval);
      emit_move_insn (A0 (QImode, 4), GEN_INT (0xcc - 0x100));
      emit_move_insn (A0 (PSImode, 5), function);
    }
#undef A0
}

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

/* Addressing Modes */

/* The r8c/m32c family supports a wide range of non-orthogonal
   addressing modes, including the ability to double-indirect on *some*
   of them.  Not all insns support all modes, either, but we rely on
   predicates and constraints to deal with that.  */
#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P m32c_legitimate_address_p
bool
m32c_legitimate_address_p (machine_mode mode, rtx x, bool strict, code_helper)
{
  int mode_adjust;
  if (CONSTANT_P (x))
    return 1;

  if (TARGET_A16 && GET_MODE (x) != HImode && GET_MODE (x) != SImode)
    return 0;
  if (TARGET_A24 && GET_MODE (x) != PSImode)
    return 0;

  /* Wide references to memory will be split after reload, so we must
     ensure that all parts of such splits remain legitimate
     addresses.  */
  mode_adjust = GET_MODE_SIZE (mode) - 1;

  /* allowing PLUS yields mem:HI(plus:SI(mem:SI(plus:SI in m32c_split_move */
  if (GET_CODE (x) == PRE_DEC
      || GET_CODE (x) == POST_INC || GET_CODE (x) == PRE_MODIFY)
    {
      return (GET_CODE (XEXP (x, 0)) == REG
	      && REGNO (XEXP (x, 0)) == SP_REGNO);
    }

#if 0
  /* This is the double indirection detection, but it currently
     doesn't work as cleanly as this code implies, so until we've had
     a chance to debug it, leave it disabled.  */
  if (TARGET_A24 && GET_CODE (x) == MEM && GET_CODE (XEXP (x, 0)) != PLUS)
    {
#if DEBUG_DOUBLE
      fprintf (stderr, "double indirect\n");
#endif
      x = XEXP (x, 0);
    }
#endif

  encode_pattern (x);
  if (RTX_IS ("r"))
    {
      /* Most indexable registers can be used without displacements,
	 although some of them will be emitted with an explicit zero
	 to please the assembler.  */
      switch (REGNO (patternr[0]))
	{
	case A1_REGNO:
	case SB_REGNO:
	case FB_REGNO:
	case SP_REGNO:
	  if (TARGET_A16 && GET_MODE (x) == SImode)
	    return 0;
	  /* FALLTHRU */
	case A0_REGNO:
	  return 1;

	default:
	  if (IS_PSEUDO (patternr[0], strict))
	    return 1;
	  return 0;
	}
    }

  if (TARGET_A16 && GET_MODE (x) == SImode)
    return 0;

  if (RTX_IS ("+ri"))
    {
      /* This is more interesting, because different base registers
	 allow for different displacements - both range and signedness
	 - and it differs from chip series to chip series too.  */
      int rn = REGNO (patternr[1]);
      HOST_WIDE_INT offs = INTVAL (patternr[2]);
      switch (rn)
	{
	case A0_REGNO:
	case A1_REGNO:
	case SB_REGNO:
	  /* The syntax only allows positive offsets, but when the
	     offsets span the entire memory range, we can simulate
	     negative offsets by wrapping.  */
	  if (TARGET_A16)
	    return (offs >= -65536 && offs <= 65535 - mode_adjust);
	  if (rn == SB_REGNO)
	    return (offs >= 0 && offs <= 65535 - mode_adjust);
	  /* A0 or A1 */
	  return (offs >= -16777216 && offs <= 16777215);

	case FB_REGNO:
	  if (TARGET_A16)
	    return (offs >= -128 && offs <= 127 - mode_adjust);
	  return (offs >= -65536 && offs <= 65535 - mode_adjust);

	case SP_REGNO:
	  return (offs >= -128 && offs <= 127 - mode_adjust);

	default:
	  if (IS_PSEUDO (patternr[1], strict))
	    return 1;
	  return 0;
	}
    }
  if (RTX_IS ("+rs") || RTX_IS ("+r+si"))
    {
      rtx reg = patternr[1];

      /* We don't know where the symbol is, so only allow base
	 registers which support displacements spanning the whole
	 address range.  */
      switch (REGNO (reg))
	{
	case A0_REGNO:
	case A1_REGNO:
	  /* $sb needs a secondary reload, but since it's involved in
	     memory address reloads too, we don't deal with it very
	     well.  */
	  /*    case SB_REGNO: */
	  return 1;
	default:
	  if (GET_CODE (reg) == SUBREG)
	    return 0;
	  if (IS_PSEUDO (reg, strict))
	    return 1;
	  return 0;
	}
    }
  return 0;
}

/* Implements REG_OK_FOR_BASE_P.  */
int
m32c_reg_ok_for_base_p (rtx x, int strict)
{
  if (GET_CODE (x) != REG)
    return 0;
  switch (REGNO (x))
    {
    case A0_REGNO:
    case A1_REGNO:
    case SB_REGNO:
    case FB_REGNO:
    case SP_REGNO:
      return 1;
    default:
      if (IS_PSEUDO (x, strict))
	return 1;
      return 0;
    }
}

/* We have three choices for choosing fb->aN offsets.  If we choose -128,
   we need one MOVA -128[fb],aN opcode and 16-bit aN displacements,
   like this:
       EB 4B FF    mova    -128[$fb],$a0
       D8 0C FF FF mov.w:Q #0,-1[$a0]

   Alternately, we subtract the frame size, and hopefully use 8-bit aN
   displacements:
       7B F4       stc $fb,$a0
       77 54 00 01 sub #256,$a0
       D8 08 01    mov.w:Q #0,1[$a0]

   If we don't offset (i.e. offset by zero), we end up with:
       7B F4       stc $fb,$a0
       D8 0C 00 FF mov.w:Q #0,-256[$a0]

   We have to subtract *something* so that we have a PLUS rtx to mark
   that we've done this reload.  The -128 offset will never result in
   an 8-bit aN offset, and the payoff for the second case is five
   loads *if* those loads are within 256 bytes of the other end of the
   frame, so the third case seems best.  Note that we subtract the
   zero, but detect that in the addhi3 pattern.  */

#define BIG_FB_ADJ 0

/* Implements LEGITIMIZE_ADDRESS.  The only address we really have to
   worry about is frame base offsets, as $fb has a limited
   displacement range.  We deal with this by attempting to reload $fb
   itself into an address register; that seems to result in the best
   code.  */
#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS m32c_legitimize_address
static rtx
m32c_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			 machine_mode mode)
{
#if DEBUG0
  fprintf (stderr, "m32c_legitimize_address for mode %s\n", mode_name[mode]);
  debug_rtx (x);
  fprintf (stderr, "\n");
#endif

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) == FB_REGNO
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (INTVAL (XEXP (x, 1)) < -128
	  || INTVAL (XEXP (x, 1)) > (128 - GET_MODE_SIZE (mode))))
    {
      /* reload FB to A_REGS */
      rtx temp = gen_reg_rtx (Pmode);
      x = copy_rtx (x);
      emit_insn (gen_rtx_SET (temp, XEXP (x, 0)));
      XEXP (x, 0) = temp;
    }

  return x;
}

/* Implements LEGITIMIZE_RELOAD_ADDRESS.  See comment above.  */
int
m32c_legitimize_reload_address (rtx * x,
				machine_mode mode,
				int opnum,
				int type, int ind_levels ATTRIBUTE_UNUSED)
{
#if DEBUG0
  fprintf (stderr, "\nm32c_legitimize_reload_address for mode %s\n",
	   mode_name[mode]);
  debug_rtx (*x);
#endif

  /* At one point, this function tried to get $fb copied to an address
     register, which in theory would maximize sharing, but gcc was
     *also* still trying to reload the whole address, and we'd run out
     of address registers.  So we let gcc do the naive (but safe)
     reload instead, when the above function doesn't handle it for
     us.

     The code below is a second attempt at the above.  */

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == REG
      && REGNO (XEXP (*x, 0)) == FB_REGNO
      && GET_CODE (XEXP (*x, 1)) == CONST_INT
      && (INTVAL (XEXP (*x, 1)) < -128
	  || INTVAL (XEXP (*x, 1)) > (128 - GET_MODE_SIZE (mode))))
    {
      rtx sum;
      int offset = INTVAL (XEXP (*x, 1));
      int adjustment = -BIG_FB_ADJ;

      sum = gen_rtx_PLUS (Pmode, XEXP (*x, 0),
			  GEN_INT (adjustment));
      *x = gen_rtx_PLUS (Pmode, sum, GEN_INT (offset - adjustment));
      if (type == RELOAD_OTHER)
	type = RELOAD_FOR_OTHER_ADDRESS;
      push_reload (sum, NULL_RTX, &XEXP (*x, 0), NULL,
		   A_REGS, Pmode, VOIDmode, 0, 0, opnum,
		   (enum reload_type) type);
      return 1;
    }

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (*x, 0), 0)) == REG
      && REGNO (XEXP (XEXP (*x, 0), 0)) == FB_REGNO
      && GET_CODE (XEXP (XEXP (*x, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (*x, 1)) == CONST_INT
      )
    {
      if (type == RELOAD_OTHER)
	type = RELOAD_FOR_OTHER_ADDRESS;
      push_reload (XEXP (*x, 0), NULL_RTX, &XEXP (*x, 0), NULL,
		   A_REGS, Pmode, VOIDmode, 0, 0, opnum,
		   (enum reload_type) type);
      return 1;
    }

  if (TARGET_A24 && GET_MODE (*x) == PSImode)
    {
      push_reload (*x, NULL_RTX, x, NULL,
		   A_REGS, PSImode, VOIDmode, 0, 0, opnum,
		   (enum reload_type) type);
      return 1;
    }

  return 0;
}

/* Return the appropriate mode for a named address pointer.  */
#undef TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE m32c_addr_space_pointer_mode
static scalar_int_mode
m32c_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_GENERIC:
      return TARGET_A24 ? PSImode : HImode;
    case ADDR_SPACE_FAR:
      return SImode;
    default:
      gcc_unreachable ();
    }
}

/* Return the appropriate mode for a named address address.  */
#undef TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE m32c_addr_space_address_mode
static scalar_int_mode
m32c_addr_space_address_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_GENERIC:
      return TARGET_A24 ? PSImode : HImode;
    case ADDR_SPACE_FAR:
      return SImode;
    default:
      gcc_unreachable ();
    }
}

/* Like m32c_legitimate_address_p, except with named addresses.  */
#undef TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P \
  m32c_addr_space_legitimate_address_p
static bool
m32c_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				      addr_space_t as, code_helper ch)
{
  if (as == ADDR_SPACE_FAR)
    {
      if (TARGET_A24)
	return 0;
      encode_pattern (x);
      if (RTX_IS ("r"))
	{
	  if (GET_MODE (x) != SImode)
	    return 0;
	  switch (REGNO (patternr[0]))
	    {
	    case A0_REGNO:
	      return 1;

	    default:
	      if (IS_PSEUDO (patternr[0], strict))
		return 1;
	      return 0;
	    }
	}
      if (RTX_IS ("+^Sri"))
	{
	  int rn = REGNO (patternr[3]);
	  HOST_WIDE_INT offs = INTVAL (patternr[4]);
	  if (GET_MODE (patternr[3]) != HImode)
	    return 0;
	  switch (rn)
	    {
	    case A0_REGNO:
	      return (offs >= 0 && offs <= 0xfffff);

	    default:
	      if (IS_PSEUDO (patternr[3], strict))
		return 1;
	      return 0;
	    }
	}
      if (RTX_IS ("+^Srs"))
	{
	  int rn = REGNO (patternr[3]);
	  if (GET_MODE (patternr[3]) != HImode)
	    return 0;
	  switch (rn)
	    {
	    case A0_REGNO:
	      return 1;

	    default:
	      if (IS_PSEUDO (patternr[3], strict))
		return 1;
	      return 0;
	    }
	}
      if (RTX_IS ("+^S+ris"))
	{
	  int rn = REGNO (patternr[4]);
	  if (GET_MODE (patternr[4]) != HImode)
	    return 0;
	  switch (rn)
	    {
	    case A0_REGNO:
	      return 1;

	    default:
	      if (IS_PSEUDO (patternr[4], strict))
		return 1;
	      return 0;
	    }
	}
      if (RTX_IS ("s"))
	{
	  return 1;
	}
      return 0;
    }

  else if (as != ADDR_SPACE_GENERIC)
    gcc_unreachable ();

  return m32c_legitimate_address_p (mode, x, strict, ch);
}

/* Like m32c_legitimate_address, except with named address support.  */
#undef TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS m32c_addr_space_legitimize_address
static rtx
m32c_addr_space_legitimize_address (rtx x, rtx oldx, machine_mode mode,
				    addr_space_t as)
{
  if (as != ADDR_SPACE_GENERIC)
    {
#if DEBUG0
      fprintf (stderr, "\033[36mm32c_addr_space_legitimize_address for mode %s\033[0m\n", mode_name[mode]);
      debug_rtx (x);
      fprintf (stderr, "\n");
#endif

      if (GET_CODE (x) != REG)
	{
	  x = force_reg (SImode, x);
	}
      return x;
    }

  return m32c_legitimize_address (x, oldx, mode);
}

/* Determine if one named address space is a subset of another.  */
#undef TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P m32c_addr_space_subset_p
static bool
m32c_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  gcc_assert (subset == ADDR_SPACE_GENERIC || subset == ADDR_SPACE_FAR);
  gcc_assert (superset == ADDR_SPACE_GENERIC || superset == ADDR_SPACE_FAR);

  if (subset == superset)
    return true;

  else
    return (subset == ADDR_SPACE_GENERIC && superset == ADDR_SPACE_FAR);
}

#undef TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT m32c_addr_space_convert
/* Convert from one address space to another.  */
static rtx
m32c_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  addr_space_t from_as = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t to_as = TYPE_ADDR_SPACE (TREE_TYPE (to_type));
  rtx result;

  gcc_assert (from_as == ADDR_SPACE_GENERIC || from_as == ADDR_SPACE_FAR);
  gcc_assert (to_as == ADDR_SPACE_GENERIC || to_as == ADDR_SPACE_FAR);

  if (to_as == ADDR_SPACE_GENERIC && from_as == ADDR_SPACE_FAR)
    {
      /* This is unpredictable, as we're truncating off usable address
	 bits.  */

      result = gen_reg_rtx (HImode);
      emit_move_insn (result, simplify_subreg (HImode, op, SImode, 0));
      return result;
    }
  else if (to_as == ADDR_SPACE_FAR && from_as == ADDR_SPACE_GENERIC)
    {
      /* This always works.  */
      result = gen_reg_rtx (SImode);
      emit_insn (gen_zero_extendhisi2 (result, op));
      return result;
    }
  else
    gcc_unreachable ();
}

/* Condition Code Status */

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS m32c_fixed_condition_code_regs
static bool
m32c_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = FLG_REGNO;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Describing Relative Costs of Operations */

/* Implements TARGET_REGISTER_MOVE_COST.  We make impossible moves
   prohibitively expensive, like trying to put QIs in r2/r3 (there are
   no opcodes to do that).  We also discourage use of mem* registers
   since they're really memory.  */

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST m32c_register_move_cost

static int
m32c_register_move_cost (machine_mode mode, reg_class_t from,
			 reg_class_t to)
{
  int cost = COSTS_N_INSNS (3);
  HARD_REG_SET cc;

/* FIXME: pick real values, but not 2 for now.  */
  cc = reg_class_contents[from] | reg_class_contents[(int) to];

  if (mode == QImode
      && hard_reg_set_intersect_p (cc, reg_class_contents[R23_REGS]))
    {
      if (hard_reg_set_subset_p (cc, reg_class_contents[R23_REGS]))
	cost = COSTS_N_INSNS (1000);
      else
	cost = COSTS_N_INSNS (80);
    }

  if (!class_can_hold_mode (from, mode) || !class_can_hold_mode (to, mode))
    cost = COSTS_N_INSNS (1000);

  if (reg_classes_intersect_p (from, CR_REGS))
    cost += COSTS_N_INSNS (5);

  if (reg_classes_intersect_p (to, CR_REGS))
    cost += COSTS_N_INSNS (5);

  if (from == MEM_REGS || to == MEM_REGS)
    cost += COSTS_N_INSNS (50);
  else if (reg_classes_intersect_p (from, MEM_REGS)
	   || reg_classes_intersect_p (to, MEM_REGS))
    cost += COSTS_N_INSNS (10);

#if DEBUG0
  fprintf (stderr, "register_move_cost %s from %s to %s = %d\n",
	   mode_name[mode], class_names[(int) from], class_names[(int) to],
	   cost);
#endif
  return cost;
}

/*  Implements TARGET_MEMORY_MOVE_COST.  */

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST m32c_memory_move_cost

static int
m32c_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
		       reg_class_t rclass ATTRIBUTE_UNUSED,
		       bool in ATTRIBUTE_UNUSED)
{
  /* FIXME: pick real values.  */
  return COSTS_N_INSNS (10);
}

/* Here we try to describe when we use multiple opcodes for one RTX so
   that gcc knows when to use them.  */
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS m32c_rtx_costs
static bool
m32c_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED,
		int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (REGNO (x) >= MEM0_REGNO && REGNO (x) <= MEM7_REGNO)
	*total += COSTS_N_INSNS (500);
      else
	*total += COSTS_N_INSNS (1);
      return true;

    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	{
	  /* mov.b r1l, r1h */
	  *total +=  COSTS_N_INSNS (1);
	  return true;
	}
      if (INTVAL (XEXP (x, 1)) > 8
	  || INTVAL (XEXP (x, 1)) < -8)
	{
	  /* mov.b #N, r1l */
	  /* mov.b r1l, r1h */
	  *total +=  COSTS_N_INSNS (2);
	  return true;
	}
      return true;

    case LE:
    case LEU:
    case LT:
    case LTU:
    case GT:
    case GTU:
    case GE:
    case GEU:
    case NE:
    case EQ:
      if (outer_code == SET)
	{
	  *total += COSTS_N_INSNS (2);
	  return true;
	}
      break;

    case ZERO_EXTRACT:
      {
	rtx dest = XEXP (x, 0);
	rtx addr = XEXP (dest, 0);
	switch (GET_CODE (addr))
	  {
	  case CONST_INT:
	    *total += COSTS_N_INSNS (1);
	    break;
	  case SYMBOL_REF:
	    *total += COSTS_N_INSNS (3);
	    break;
	  default:
	    *total += COSTS_N_INSNS (2);
	    break;
	  }
	return true;
      }
      break;

    default:
      /* Reasonable default.  */
      if (TARGET_A16 && mode == SImode)
	*total += COSTS_N_INSNS (2);
      break;
    }
  return false;
}

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST m32c_address_cost
static int
m32c_address_cost (rtx addr, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  int i;
  /*  fprintf(stderr, "\naddress_cost\n");
      debug_rtx(addr);*/
  switch (GET_CODE (addr))
    {
    case CONST_INT:
      i = INTVAL (addr);
      if (i == 0)
	return COSTS_N_INSNS(1);
      if (i > 0 && i <= 255)
	return COSTS_N_INSNS(2);
      if (i > 0 && i <= 65535)
	return COSTS_N_INSNS(3);
      return COSTS_N_INSNS(4);
    case SYMBOL_REF:
      return COSTS_N_INSNS(4);
    case REG:
      return COSTS_N_INSNS(1);
    case PLUS:
      if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	{
	  i = INTVAL (XEXP (addr, 1));
	  if (i == 0)
	    return COSTS_N_INSNS(1);
	  if (i > 0 && i <= 255)
	    return COSTS_N_INSNS(2);
	  if (i > 0 && i <= 65535)
	    return COSTS_N_INSNS(3);
	}
      return COSTS_N_INSNS(4);
    default:
      return 0;
    }
}

/* Defining the Output Assembler Language */

/* Output of Data */

/* We may have 24 bit sizes, which is the native address size.
   Currently unused, but provided for completeness.  */
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER m32c_asm_integer
static bool
m32c_asm_integer (rtx x, unsigned int size, int aligned_p)
{
  switch (size)
    {
    case 3:
      fprintf (asm_out_file, "\t.3byte\t");
      output_addr_const (asm_out_file, x);
      fputc ('\n', asm_out_file);
      return true;
    case 4:
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  fprintf (asm_out_file, "\t.long\t");
	  output_addr_const (asm_out_file, x);
	  fputc ('\n', asm_out_file);
	  return true;
	}
      break;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Output of Assembler Instructions */

/* We use a lookup table because the addressing modes are non-orthogonal.  */

static struct
{
  char code;
  char const *pattern;
  char const *format;
}
const conversions[] = {
  { 0, "r", "0" },

  { 0, "mr", "z[1]" },
  { 0, "m+ri", "3[2]" },
  { 0, "m+rs", "3[2]" },
  { 0, "m+^Zrs", "5[4]" },
  { 0, "m+^Zri", "5[4]" },
  { 0, "m+^Z+ris", "7+6[5]" },
  { 0, "m+^Srs", "5[4]" },
  { 0, "m+^Sri", "5[4]" },
  { 0, "m+^S+ris", "7+6[5]" },
  { 0, "m+r+si", "4+5[2]" },
  { 0, "ms", "1" },
  { 0, "mi", "1" },
  { 0, "m+si", "2+3" },

  { 0, "mmr", "[z[2]]" },
  { 0, "mm+ri", "[4[3]]" },
  { 0, "mm+rs", "[4[3]]" },
  { 0, "mm+r+si", "[5+6[3]]" },
  { 0, "mms", "[[2]]" },
  { 0, "mmi", "[[2]]" },
  { 0, "mm+si", "[4[3]]" },

  { 0, "i", "#0" },
  { 0, "s", "#0" },
  { 0, "+si", "#1+2" },
  { 0, "l", "#0" },

  { 'l', "l", "0" },
  { 'd', "i", "0" },
  { 'd', "s", "0" },
  { 'd', "+si", "1+2" },
  { 'D', "i", "0" },
  { 'D', "s", "0" },
  { 'D', "+si", "1+2" },
  { 'x', "i", "#0" },
  { 'X', "i", "#0" },
  { 'm', "i", "#0" },
  { 'b', "i", "#0" },
  { 'B', "i", "0" },
  { 'p', "i", "0" },

  { 0, 0, 0 }
};

/* This is in order according to the bitfield that pushm/popm use.  */
static char const *pushm_regs[] = {
  "fb", "sb", "a1", "a0", "r3", "r2", "r1", "r0"
};

/* Implements TARGET_PRINT_OPERAND.  */

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND m32c_print_operand

static void
m32c_print_operand (FILE * file, rtx x, int code)
{
  int i, j, b;
  const char *comma;
  HOST_WIDE_INT ival;
  int unsigned_const = 0;
  int force_sign;

  /* Multiplies; constants are converted to sign-extended format but
   we need unsigned, so 'u' and 'U' tell us what size unsigned we
   need.  */
  if (code == 'u')
    {
      unsigned_const = 2;
      code = 0;
    }
  if (code == 'U')
    {
      unsigned_const = 1;
      code = 0;
    }
  /* This one is only for debugging; you can put it in a pattern to
     force this error.  */
  if (code == '!')
    {
      fprintf (stderr, "dj: unreviewed pattern:");
      if (current_output_insn)
	debug_rtx (current_output_insn);
      gcc_unreachable ();
    }
  /* PSImode operations are either .w or .l depending on the target.  */
  if (code == '&')
    {
      if (TARGET_A16)
	fprintf (file, "w");
      else
	fprintf (file, "l");
      return;
    }
  /* Inverted conditionals.  */
  if (code == 'C')
    {
      switch (GET_CODE (x))
	{
	case LE:
	  fputs ("gt", file);
	  break;
	case LEU:
	  fputs ("gtu", file);
	  break;
	case LT:
	  fputs ("ge", file);
	  break;
	case LTU:
	  fputs ("geu", file);
	  break;
	case GT:
	  fputs ("le", file);
	  break;
	case GTU:
	  fputs ("leu", file);
	  break;
	case GE:
	  fputs ("lt", file);
	  break;
	case GEU:
	  fputs ("ltu", file);
	  break;
	case NE:
	  fputs ("eq", file);
	  break;
	case EQ:
	  fputs ("ne", file);
	  break;
	default:
	  gcc_unreachable ();
	}
      return;
    }
  /* Regular conditionals.  */
  if (code == 'c')
    {
      switch (GET_CODE (x))
	{
	case LE:
	  fputs ("le", file);
	  break;
	case LEU:
	  fputs ("leu", file);
	  break;
	case LT:
	  fputs ("lt", file);
	  break;
	case LTU:
	  fputs ("ltu", file);
	  break;
	case GT:
	  fputs ("gt", file);
	  break;
	case GTU:
	  fputs ("gtu", file);
	  break;
	case GE:
	  fputs ("ge", file);
	  break;
	case GEU:
	  fputs ("geu", file);
	  break;
	case NE:
	  fputs ("ne", file);
	  break;
	case EQ:
	  fputs ("eq", file);
	  break;
	default:
	  gcc_unreachable ();
	}
      return;
    }
  /* Used in negsi2 to do HImode ops on the two parts of an SImode
     operand.  */
  if (code == 'h' && GET_MODE (x) == SImode)
    {
      x = m32c_subreg (HImode, x, SImode, 0);
      code = 0;
    }
  if (code == 'H' && GET_MODE (x) == SImode)
    {
      x = m32c_subreg (HImode, x, SImode, 2);
      code = 0;
    }
  if (code == 'h' && GET_MODE (x) == HImode)
    {
      x = m32c_subreg (QImode, x, HImode, 0);
      code = 0;
    }
  if (code == 'H' && GET_MODE (x) == HImode)
    {
      /* We can't actually represent this as an rtx.  Do it here.  */
      if (GET_CODE (x) == REG)
	{
	  switch (REGNO (x))
	    {
	    case R0_REGNO:
	      fputs ("r0h", file);
	      return;
	    case R1_REGNO:
	      fputs ("r1h", file);
	      return;
	    default:
	      gcc_unreachable();
	    }
	}
      /* This should be a MEM.  */
      x = m32c_subreg (QImode, x, HImode, 1);
      code = 0;
    }
  /* This is for BMcond, which always wants word register names.  */
  if (code == 'h' && GET_MODE (x) == QImode)
    {
      if (GET_CODE (x) == REG)
	x = gen_rtx_REG (HImode, REGNO (x));
      code = 0;
    }
  /* 'x' and 'X' need to be ignored for non-immediates.  */
  if ((code == 'x' || code == 'X') && GET_CODE (x) != CONST_INT)
    code = 0;

  encode_pattern (x);
  force_sign = 0;
  for (i = 0; conversions[i].pattern; i++)
    if (conversions[i].code == code
	&& streq (conversions[i].pattern, pattern))
      {
	for (j = 0; conversions[i].format[j]; j++)
	  /* backslash quotes the next character in the output pattern.  */
	  if (conversions[i].format[j] == '\\')
	    {
	      fputc (conversions[i].format[j + 1], file);
	      j++;
	    }
	  /* Digits in the output pattern indicate that the
	     corresponding RTX is to be output at that point.  */
	  else if (ISDIGIT (conversions[i].format[j]))
	    {
	      rtx r = patternr[conversions[i].format[j] - '0'];
	      switch (GET_CODE (r))
		{
		case REG:
		  fprintf (file, "%s",
			   reg_name_with_mode (REGNO (r), GET_MODE (r)));
		  break;
		case CONST_INT:
		  switch (code)
		    {
		    case 'b':
		    case 'B':
		      {
			int v = INTVAL (r);
			int i = (int) exact_log2 (v);
			if (i == -1)
			  i = (int) exact_log2 ((v ^ 0xffff) & 0xffff);
			if (i == -1)
			  i = (int) exact_log2 ((v ^ 0xff) & 0xff);
			/* Bit position.  */
			fprintf (file, "%d", i);
		      }
		      break;
		    case 'x':
		      /* Unsigned byte.  */
		      fprintf (file, HOST_WIDE_INT_PRINT_HEX,
			       INTVAL (r) & 0xff);
		      break;
		    case 'X':
		      /* Unsigned word.  */
		      fprintf (file, HOST_WIDE_INT_PRINT_HEX,
			       INTVAL (r) & 0xffff);
		      break;
		    case 'p':
		      /* pushm and popm encode a register set into a single byte.  */
		      comma = "";
		      for (b = 7; b >= 0; b--)
			if (INTVAL (r) & (1 << b))
			  {
			    fprintf (file, "%s%s", comma, pushm_regs[b]);
			    comma = ",";
			  }
		      break;
		    case 'm':
		      /* "Minus".  Output -X  */
		      ival = (-INTVAL (r) & 0xffff);
		      if (ival & 0x8000)
			ival = ival - 0x10000;
		      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
		      break;
		    default:
		      ival = INTVAL (r);
		      if (conversions[i].format[j + 1] == '[' && ival < 0)
			{
			  /* We can simulate negative displacements by
			     taking advantage of address space
			     wrapping when the offset can span the
			     entire address range.  */
			  rtx base =
			    patternr[conversions[i].format[j + 2] - '0'];
			  if (GET_CODE (base) == REG)
			    switch (REGNO (base))
			      {
			      case A0_REGNO:
			      case A1_REGNO:
				if (TARGET_A24)
				  ival = 0x1000000 + ival;
				else
				  ival = 0x10000 + ival;
				break;
			      case SB_REGNO:
				if (TARGET_A16)
				  ival = 0x10000 + ival;
				break;
			      }
			}
		      else if (code == 'd' && ival < 0 && j == 0)
			/* The "mova" opcode is used to do addition by
			   computing displacements, but again, we need
			   displacements to be unsigned *if* they're
			   the only component of the displacement
			   (i.e. no "symbol-4" type displacement).  */
			ival = (TARGET_A24 ? 0x1000000 : 0x10000) + ival;

		      if (conversions[i].format[j] == '0')
			{
			  /* More conversions to unsigned.  */
			  if (unsigned_const == 2)
			    ival &= 0xffff;
			  if (unsigned_const == 1)
			    ival &= 0xff;
			}
		      if (streq (conversions[i].pattern, "mi")
			  || streq (conversions[i].pattern, "mmi"))
			{
			  /* Integers used as addresses are unsigned.  */
			  ival &= (TARGET_A24 ? 0xffffff : 0xffff);
			}
		      if (force_sign && ival >= 0)
			fputc ('+', file);
		      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
		      break;
		    }
		  break;
		case CONST_DOUBLE:
		  /* We don't have const_double constants.  If it
		     happens, make it obvious.  */
		  fprintf (file, "[const_double 0x%lx]",
			   (unsigned long) CONST_DOUBLE_HIGH (r));
		  break;
		case SYMBOL_REF:
		  assemble_name (file, XSTR (r, 0));
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
	      if (conversions[i].format[j] == 'z')
		{
		  /* Some addressing modes *must* have a displacement,
		     so insert a zero here if needed.  */
		  int k;
		  for (k = j + 1; conversions[i].format[k]; k++)
		    if (ISDIGIT (conversions[i].format[k]))
		      {
			rtx reg = patternr[conversions[i].format[k] - '0'];
			if (GET_CODE (reg) == REG
			    && (REGNO (reg) == SB_REGNO
				|| REGNO (reg) == FB_REGNO
				|| REGNO (reg) == SP_REGNO))
			  fputc ('0', file);
		      }
		  continue;
		}
	      /* Signed displacements off symbols need to have signs
		 blended cleanly.  */
	      if (conversions[i].format[j] == '+'
		  && (!code || code == 'D' || code == 'd')
		  && ISDIGIT (conversions[i].format[j + 1])
		  && (GET_CODE (patternr[conversions[i].format[j + 1] - '0'])
		      == CONST_INT))
		{
		  force_sign = 1;
		  continue;
		}
	      fputc (conversions[i].format[j], file);
	    }
	break;
      }
  if (!conversions[i].pattern)
    {
      fprintf (stderr, "unconvertible operand %c `%s'", code ? code : '-',
	       pattern);
      debug_rtx (x);
      fprintf (file, "[%c.%s]", code ? code : '-', pattern);
    }

  return;
}

/* Implements TARGET_PRINT_OPERAND_PUNCT_VALID_P.

   See m32c_print_operand above for descriptions of what these do.  */

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P m32c_print_operand_punct_valid_p

static bool
m32c_print_operand_punct_valid_p (unsigned char c)
{
  if (c == '&' || c == '!')
    return true;

  return false;
}

/* Implements TARGET_PRINT_OPERAND_ADDRESS.  Nothing unusual here.  */

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS m32c_print_operand_address

static void
m32c_print_operand_address (FILE * stream, machine_mode /*mode*/, rtx address)
{
  if (GET_CODE (address) == MEM)
    address = XEXP (address, 0);
  else
    /* cf: gcc.dg/asm-4.c.  */
    gcc_assert (GET_CODE (address) == REG);

  m32c_print_operand (stream, address, 0);
}

/* Implements ASM_OUTPUT_REG_PUSH.  Control registers are pushed
   differently than general registers.  */
void
m32c_output_reg_push (FILE * s, int regno)
{
  if (regno == FLG_REGNO)
    fprintf (s, "\tpushc\tflg\n");
  else
    fprintf (s, "\tpush.%c\t%s\n",
	     " bwll"[reg_push_size (regno)], reg_names[regno]);
}

/* Likewise for ASM_OUTPUT_REG_POP.  */
void
m32c_output_reg_pop (FILE * s, int regno)
{
  if (regno == FLG_REGNO)
    fprintf (s, "\tpopc\tflg\n");
  else
    fprintf (s, "\tpop.%c\t%s\n",
	     " bwll"[reg_push_size (regno)], reg_names[regno]);
}

/* Defining target-specific uses of `__attribute__' */

/* Used to simplify the logic below.  Find the attributes wherever
   they may be.  */
#define M32C_ATTRIBUTES(decl) \
  (TYPE_P (decl)) ? TYPE_ATTRIBUTES (decl) \
                : DECL_ATTRIBUTES (decl) \
                  ? (DECL_ATTRIBUTES (decl)) \
		  : TYPE_ATTRIBUTES (TREE_TYPE (decl))

/* Returns TRUE if the given tree has the "interrupt" attribute.  */
static int
interrupt_p (tree node ATTRIBUTE_UNUSED)
{
  tree list = M32C_ATTRIBUTES (node);
  while (list)
    {
      if (is_attribute_p ("interrupt", TREE_PURPOSE (list)))
	return 1;
      list = TREE_CHAIN (list);
    }
  return fast_interrupt_p (node);
}

/* Returns TRUE if the given tree has the "bank_switch" attribute.  */
static int
bank_switch_p (tree node ATTRIBUTE_UNUSED)
{
  tree list = M32C_ATTRIBUTES (node);
  while (list)
    {
      if (is_attribute_p ("bank_switch", TREE_PURPOSE (list)))
	return 1;
      list = TREE_CHAIN (list);
    }
  return 0;
}

/* Returns TRUE if the given tree has the "fast_interrupt" attribute.  */
static int
fast_interrupt_p (tree node ATTRIBUTE_UNUSED)
{
  tree list = M32C_ATTRIBUTES (node);
  while (list)
    {
      if (is_attribute_p ("fast_interrupt", TREE_PURPOSE (list)))
	return 1;
      list = TREE_CHAIN (list);
    }
  return 0;
}

static tree
interrupt_handler (tree * node ATTRIBUTE_UNUSED,
		   tree name ATTRIBUTE_UNUSED,
		   tree args ATTRIBUTE_UNUSED,
		   int flags ATTRIBUTE_UNUSED,
		   bool * no_add_attrs ATTRIBUTE_UNUSED)
{
  return NULL_TREE;
}

/* Returns TRUE if given tree has the "function_vector" attribute. */
int
m32c_special_page_vector_p (tree func)
{
  tree list;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return 0;

  list = M32C_ATTRIBUTES (func);
  while (list)
    {
      if (is_attribute_p ("function_vector", TREE_PURPOSE (list)))
        return 1;
      list = TREE_CHAIN (list);
    }
  return 0;
}

static tree
function_vector_handler (tree * node ATTRIBUTE_UNUSED,
                         tree name ATTRIBUTE_UNUSED,
                         tree args ATTRIBUTE_UNUSED,
                         int flags ATTRIBUTE_UNUSED,
                         bool * no_add_attrs ATTRIBUTE_UNUSED)
{
  if (TARGET_R8C)
    {
      /* The attribute is not supported for R8C target.  */
      warning (OPT_Wattributes,
                "%qE attribute is not supported for R8C target",
                name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      /* The attribute must be applied to functions only.  */
      warning (OPT_Wattributes,
                "%qE attribute applies only to functions",
                name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (TREE_VALUE (args)) != INTEGER_CST)
    {
      /* The argument must be a constant integer.  */
      warning (OPT_Wattributes,
                "%qE attribute argument not an integer constant",
                name);
      *no_add_attrs = true;
    }
  else if (TREE_INT_CST_LOW (TREE_VALUE (args)) < 18
           || TREE_INT_CST_LOW (TREE_VALUE (args)) > 255)
    {
      /* The argument value must be between 18 to 255.  */
      warning (OPT_Wattributes,
                "%qE attribute argument should be between 18 to 255",
                name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* If the function is assigned the attribute 'function_vector', it
   returns the function vector number, otherwise returns zero.  */
int
current_function_special_page_vector (rtx x)
{
  int num;

  if ((GET_CODE(x) == SYMBOL_REF)
      && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_FUNCVEC_FUNCTION))
    {
      tree list;
      tree t = SYMBOL_REF_DECL (x);

      if (TREE_CODE (t) != FUNCTION_DECL)
        return 0;

      list = M32C_ATTRIBUTES (t);
      while (list)
        {
          if (is_attribute_p ("function_vector", TREE_PURPOSE (list)))
            {
              num = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (list)));
              return num;
            }

          list = TREE_CHAIN (list);
        }

      return 0;
    }
  else
    return 0;
}

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE m32c_attribute_table
TARGET_GNU_ATTRIBUTES (m32c_attribute_table, {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "interrupt", 0, 0, false, false, false, false, interrupt_handler, NULL },
  { "bank_switch", 0, 0, false, false, false, false, interrupt_handler, NULL },
  { "fast_interrupt", 0, 0, false, false, false, false,
    interrupt_handler, NULL },
  { "function_vector", 1, 1, true,  false, false, false,
    function_vector_handler, NULL }
});

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES m32c_comp_type_attributes
static int
m32c_comp_type_attributes (const_tree type1 ATTRIBUTE_UNUSED,
			   const_tree type2 ATTRIBUTE_UNUSED)
{
  /* 0=incompatible 1=compatible 2=warning */
  return 1;
}

#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES m32c_insert_attributes
static void
m32c_insert_attributes (tree node ATTRIBUTE_UNUSED,
			tree * attr_ptr ATTRIBUTE_UNUSED)
{
  unsigned addr;
  /* See if we need to make #pragma address variables volatile.  */

  if (VAR_P (node))
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (node));
      if (m32c_get_pragma_address  (name, &addr))
	{
	  TREE_THIS_VOLATILE (node) = true;
	}
    }
}

/* Hash table of pragma info.  */
static GTY(()) hash_map<nofree_string_hash, unsigned> *pragma_htab;

void
m32c_note_pragma_address (const char *varname, unsigned address)
{
  if (!pragma_htab)
    pragma_htab = hash_map<nofree_string_hash, unsigned>::create_ggc (31);

  const char *name = ggc_strdup (varname);
  unsigned int *slot = &pragma_htab->get_or_insert (name);
  *slot = address;
}

static bool
m32c_get_pragma_address (const char *varname, unsigned *address)
{
  if (!pragma_htab)
    return false;

  unsigned int *slot = pragma_htab->get (varname);
  if (slot)
    {
      *address = *slot;
      return true;
    }
  return false;
}

void
m32c_output_aligned_common (FILE *stream, tree decl ATTRIBUTE_UNUSED,
			    const char *name,
			    int size, int align, int global)
{
  unsigned address;

  if (m32c_get_pragma_address (name, &address))
    {
      /* We never output these as global.  */
      assemble_name (stream, name);
      fprintf (stream, " = 0x%04x\n", address);
      return;
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

/* Predicates */

/* This is a list of legal subregs of hard regs.  */
static const struct {
  unsigned char outer_mode_size;
  unsigned char inner_mode_size;
  unsigned char byte_mask;
  unsigned char legal_when;
  unsigned int regno;
} legal_subregs[] = {
  {1, 2, 0x03, 1, R0_REGNO}, /* r0h r0l */
  {1, 2, 0x03, 1, R1_REGNO}, /* r1h r1l */
  {1, 2, 0x01, 1, A0_REGNO},
  {1, 2, 0x01, 1, A1_REGNO},

  {1, 4, 0x01, 1, A0_REGNO},
  {1, 4, 0x01, 1, A1_REGNO},

  {2, 4, 0x05, 1, R0_REGNO}, /* r2 r0 */
  {2, 4, 0x05, 1, R1_REGNO}, /* r3 r1 */
  {2, 4, 0x05, 16, A0_REGNO}, /* a1 a0 */
  {2, 4, 0x01, 24, A0_REGNO}, /* a1 a0 */
  {2, 4, 0x01, 24, A1_REGNO}, /* a1 a0 */

  {4, 8, 0x55, 1, R0_REGNO}, /* r3 r1 r2 r0 */
};

/* Returns TRUE if OP is a subreg of a hard reg which we don't
   support.  We also bail on MEMs with illegal addresses.  */
bool
m32c_illegal_subreg_p (rtx op)
{
  int offset;
  unsigned int i;
  machine_mode src_mode, dest_mode;

  if (GET_CODE (op) == MEM
      && ! m32c_legitimate_address_p (Pmode, XEXP (op, 0), false))
    {
      return true;
    }

  if (GET_CODE (op) != SUBREG)
    return false;

  dest_mode = GET_MODE (op);
  offset = SUBREG_BYTE (op);
  op = SUBREG_REG (op);
  src_mode = GET_MODE (op);

  if (GET_MODE_SIZE (dest_mode) == GET_MODE_SIZE (src_mode))
    return false;
  if (GET_CODE (op) != REG)
    return false;
  if (REGNO (op) >= MEM0_REGNO)
    return false;

  offset = (1 << offset);

  for (i = 0; i < ARRAY_SIZE (legal_subregs); i ++)
    if (legal_subregs[i].outer_mode_size == GET_MODE_SIZE (dest_mode)
	&& legal_subregs[i].regno == REGNO (op)
	&& legal_subregs[i].inner_mode_size == GET_MODE_SIZE (src_mode)
	&& legal_subregs[i].byte_mask & offset)
      {
	switch (legal_subregs[i].legal_when)
	  {
	  case 1:
	    return false;
	  case 16:
	    if (TARGET_A16)
	      return false;
	    break;
	  case 24:
	    if (TARGET_A24)
	      return false;
	    break;
	  }
      }
  return true;
}

/* Returns TRUE if we support a move between the first two operands.
   At the moment, we just want to discourage mem to mem moves until
   after reload, because reload has a hard time with our limited
   number of address registers, and we can get into a situation where
   we need three of them when we only have two.  */
bool
m32c_mov_ok (rtx * operands, machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (TARGET_A24)
    return true;

#define DEBUG_MOV_OK 0
#if DEBUG_MOV_OK
  fprintf (stderr, "m32c_mov_ok %s\n", mode_name[mode]);
  debug_rtx (op0);
  debug_rtx (op1);
#endif

  if (GET_CODE (op0) == SUBREG)
    op0 = XEXP (op0, 0);
  if (GET_CODE (op1) == SUBREG)
    op1 = XEXP (op1, 0);

  if (GET_CODE (op0) == MEM
      && GET_CODE (op1) == MEM
      && ! reload_completed)
    {
#if DEBUG_MOV_OK
      fprintf (stderr, " - no, mem to mem\n");
#endif
      return false;
    }

#if DEBUG_MOV_OK
  fprintf (stderr, " - ok\n");
#endif
  return true;
}

/* Returns TRUE if two consecutive HImode mov instructions, generated
   for moving an immediate double data to a double data type variable
   location, can be combined into single SImode mov instruction.  */
bool
m32c_immd_dbl_mov (rtx * operands ATTRIBUTE_UNUSED,
		   machine_mode mode ATTRIBUTE_UNUSED)
{
  /* ??? This relied on the now-defunct MEM_SCALAR and MEM_IN_STRUCT_P
     flags.  */
  return false;
}

/* Expanders */

/* Subregs are non-orthogonal for us, because our registers are all
   different sizes.  */
static rtx
m32c_subreg (machine_mode outer,
	     rtx x, machine_mode inner, int byte)
{
  int r, nr = -1;

  /* Converting MEMs to different types that are the same size, we
     just rewrite them.  */
  if (GET_CODE (x) == SUBREG
      && SUBREG_BYTE (x) == 0
      && GET_CODE (SUBREG_REG (x)) == MEM
      && (GET_MODE_SIZE (GET_MODE (x))
	  == GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
    {
      rtx oldx = x;
      x = gen_rtx_MEM (GET_MODE (x), XEXP (SUBREG_REG (x), 0));
      MEM_COPY_ATTRIBUTES (x, SUBREG_REG (oldx));
    }

  /* Push/pop get done as smaller push/pops.  */
  if (GET_CODE (x) == MEM
      && (GET_CODE (XEXP (x, 0)) == PRE_DEC
	  || GET_CODE (XEXP (x, 0)) == POST_INC))
    return gen_rtx_MEM (outer, XEXP (x, 0));
  if (GET_CODE (x) == SUBREG
      && GET_CODE (XEXP (x, 0)) == MEM
      && (GET_CODE (XEXP (XEXP (x, 0), 0)) == PRE_DEC
	  || GET_CODE (XEXP (XEXP (x, 0), 0)) == POST_INC))
    return gen_rtx_MEM (outer, XEXP (XEXP (x, 0), 0));

  if (GET_CODE (x) != REG)
    {
      rtx r = simplify_gen_subreg (outer, x, inner, byte);
      if (GET_CODE (r) == SUBREG
	  && GET_CODE (x) == MEM
	  && MEM_VOLATILE_P (x))
	{
	  /* Volatile MEMs don't get simplified, but we need them to
	     be.  We are little endian, so the subreg byte is the
	     offset.  */
	  r = adjust_address_nv (x, outer, byte);
	}
      return r;
    }

  r = REGNO (x);
  if (r >= FIRST_PSEUDO_REGISTER || r == AP_REGNO)
    return simplify_gen_subreg (outer, x, inner, byte);

  if (IS_MEM_REGNO (r))
    return simplify_gen_subreg (outer, x, inner, byte);

  /* This is where the complexities of our register layout are
     described.  */
  if (byte == 0)
    nr = r;
  else if (outer == HImode)
    {
      if (r == R0_REGNO && byte == 2)
	nr = R2_REGNO;
      else if (r == R0_REGNO && byte == 4)
	nr = R1_REGNO;
      else if (r == R0_REGNO && byte == 6)
	nr = R3_REGNO;
      else if (r == R1_REGNO && byte == 2)
	nr = R3_REGNO;
      else if (r == A0_REGNO && byte == 2)
	nr = A1_REGNO;
    }
  else if (outer == SImode)
    {
      if (r == R0_REGNO && byte == 0)
	nr = R0_REGNO;
      else if (r == R0_REGNO && byte == 4)
	nr = R1_REGNO;
    }
  if (nr == -1)
    {
      fprintf (stderr, "m32c_subreg %s %s %d\n",
	       mode_name[outer], mode_name[inner], byte);
      debug_rtx (x);
      gcc_unreachable ();
    }
  return gen_rtx_REG (outer, nr);
}

/* Used to emit move instructions.  We split some moves,
   and avoid mem-mem moves.  */
int
m32c_prepare_move (rtx * operands, machine_mode mode)
{
  if (far_addr_space_p (operands[0])
      && CONSTANT_P (operands[1]))
    {
      operands[1] = force_reg (GET_MODE (operands[0]), operands[1]);
    }
  if (TARGET_A16 && mode == PSImode)
    return m32c_split_move (operands, mode, 1);
  if ((GET_CODE (operands[0]) == MEM)
      && (GET_CODE (XEXP (operands[0], 0)) == PRE_MODIFY))
    {
      rtx pmv = XEXP (operands[0], 0);
      rtx dest_reg = XEXP (pmv, 0);
      rtx dest_mod = XEXP (pmv, 1);

      emit_insn (gen_rtx_SET (dest_reg, dest_mod));
      operands[0] = gen_rtx_MEM (mode, dest_reg);
    }
  if (can_create_pseudo_p () && MEM_P (operands[0]) && MEM_P (operands[1]))
    operands[1] = copy_to_mode_reg (mode, operands[1]);
  return 0;
}

#define DEBUG_SPLIT 0

/* Returns TRUE if the given PSImode move should be split.  We split
   for all r8c/m16c moves, since it doesn't support them, and for
   POP.L as we can only *push* SImode.  */
int
m32c_split_psi_p (rtx * operands)
{
#if DEBUG_SPLIT
  fprintf (stderr, "\nm32c_split_psi_p\n");
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif
  if (TARGET_A16)
    {
#if DEBUG_SPLIT
      fprintf (stderr, "yes, A16\n");
#endif
      return 1;
    }
  if (GET_CODE (operands[1]) == MEM
      && GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    {
#if DEBUG_SPLIT
      fprintf (stderr, "yes, pop.l\n");
#endif
      return 1;
    }
#if DEBUG_SPLIT
  fprintf (stderr, "no, default\n");
#endif
  return 0;
}

/* Split the given move.  SPLIT_ALL is 0 if splitting is optional
   (define_expand), 1 if it is not optional (define_insn_and_split),
   and 3 for define_split (alternate api). */
int
m32c_split_move (rtx * operands, machine_mode mode, int split_all)
{
  rtx s[4], d[4];
  int parts, si, di, rev = 0;
  int rv = 0, opi = 2;
  machine_mode submode = HImode;
  rtx *ops, local_ops[10];

  /* define_split modifies the existing operands, but the other two
     emit new insns.  OPS is where we store the operand pairs, which
     we emit later.  */
  if (split_all == 3)
    ops = operands;
  else
    ops = local_ops;

  /* Else HImode.  */
  if (mode == DImode)
    submode = SImode;

  /* Before splitting mem-mem moves, force one operand into a
     register.  */
  if (can_create_pseudo_p () && MEM_P (operands[0]) && MEM_P (operands[1]))
    {
#if DEBUG0
      fprintf (stderr, "force_reg...\n");
      debug_rtx (operands[1]);
#endif
      operands[1] = force_reg (mode, operands[1]);
#if DEBUG0
      debug_rtx (operands[1]);
#endif
    }

  parts = 2;

#if DEBUG_SPLIT
  fprintf (stderr, "\nsplit_move %d all=%d\n", !can_create_pseudo_p (),
	   split_all);
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
#endif

  /* Note that split_all is not used to select the api after this
     point, so it's safe to set it to 3 even with define_insn.  */
  /* None of the chips can move SI operands to sp-relative addresses,
     so we always split those.  */
  if (satisfies_constraint_Ss (operands[0]))
    split_all = 3;

  if (TARGET_A16
      && (far_addr_space_p (operands[0])
	  || far_addr_space_p (operands[1])))
    split_all |= 1;

  /* We don't need to split these.  */
  if (TARGET_A24
      && split_all != 3
      && (mode == SImode || mode == PSImode)
      && !(GET_CODE (operands[1]) == MEM
	   && GET_CODE (XEXP (operands[1], 0)) == POST_INC))
    return 0;

  /* First, enumerate the subregs we'll be dealing with.  */
  for (si = 0; si < parts; si++)
    {
      d[si] =
	m32c_subreg (submode, operands[0], mode,
		     si * GET_MODE_SIZE (submode));
      s[si] =
	m32c_subreg (submode, operands[1], mode,
		     si * GET_MODE_SIZE (submode));
    }

  /* Split pushes by emitting a sequence of smaller pushes.  */
  if (GET_CODE (d[0]) == MEM && GET_CODE (XEXP (d[0], 0)) == PRE_DEC)
    {
      for (si = parts - 1; si >= 0; si--)
	{
	  ops[opi++] = gen_rtx_MEM (submode,
				    gen_rtx_PRE_DEC (Pmode,
						     gen_rtx_REG (Pmode,
								  SP_REGNO)));
	  ops[opi++] = s[si];
	}

      rv = 1;
    }
  /* Likewise for pops.  */
  else if (GET_CODE (s[0]) == MEM && GET_CODE (XEXP (s[0], 0)) == POST_INC)
    {
      for (di = 0; di < parts; di++)
	{
	  ops[opi++] = d[di];
	  ops[opi++] = gen_rtx_MEM (submode,
				    gen_rtx_POST_INC (Pmode,
						      gen_rtx_REG (Pmode,
								   SP_REGNO)));
	}
      rv = 1;
    }
  else if (split_all)
    {
      /* if d[di] == s[si] for any di < si, we'll early clobber. */
      for (di = 0; di < parts - 1; di++)
	for (si = di + 1; si < parts; si++)
	  if (reg_mentioned_p (d[di], s[si]))
	    rev = 1;

      if (rev)
	for (si = 0; si < parts; si++)
	  {
	    ops[opi++] = d[si];
	    ops[opi++] = s[si];
	  }
      else
	for (si = parts - 1; si >= 0; si--)
	  {
	    ops[opi++] = d[si];
	    ops[opi++] = s[si];
	  }
      rv = 1;
    }
  /* Now emit any moves we may have accumulated.  */
  if (rv && split_all != 3)
    {
      int i;
      for (i = 2; i < opi; i += 2)
	emit_move_insn (ops[i], ops[i + 1]);
    }
  return rv;
}

/* The m32c has a number of opcodes that act like memcpy, strcmp, and
   the like.  For the R8C they expect one of the addresses to be in
   R1L:An so we need to arrange for that.  Otherwise, it's just a
   matter of picking out the operands we want and emitting the right
   pattern for them.  All these expanders, which correspond to
   patterns in blkmov.md, must return nonzero if they expand the insn,
   or zero if they should FAIL.  */

/* This is a memset() opcode.  All operands are implied, so we need to
   arrange for them to be in the right registers.  The opcode wants
   addresses, not [mem] syntax.  $0 is the destination (MEM:BLK), $1
   the count (HI), and $2 the value (QI).  */
int
m32c_expand_setmemhi(rtx *operands)
{
  rtx desta, count, val;
  rtx desto, counto;

  desta = XEXP (operands[0], 0);
  count = operands[1];
  val = operands[2];

  desto = gen_reg_rtx (Pmode);
  counto = gen_reg_rtx (HImode);

  if (GET_CODE (desta) != REG
      || REGNO (desta) < FIRST_PSEUDO_REGISTER)
    desta = copy_to_mode_reg (Pmode, desta);

  /* This looks like an arbitrary restriction, but this is by far the
     most common case.  For counts 8..14 this actually results in
     smaller code with no speed penalty because the half-sized
     constant can be loaded with a shorter opcode.  */
  if (GET_CODE (count) == CONST_INT
      && GET_CODE (val) == CONST_INT
      && ! (INTVAL (count) & 1)
      && (INTVAL (count) > 1)
      && (INTVAL (val) <= 7 && INTVAL (val) >= -8))
    {
      unsigned v = INTVAL (val) & 0xff;
      v = v | (v << 8);
      count = copy_to_mode_reg (HImode, GEN_INT (INTVAL (count) / 2));
      val = copy_to_mode_reg (HImode, GEN_INT (v));
      if (TARGET_A16)
	emit_insn (gen_setmemhi_whi_op (desto, counto, val, desta, count));
      else
	emit_insn (gen_setmemhi_wpsi_op (desto, counto, val, desta, count));
      return 1;
    }

  /* This is the generalized memset() case.  */
  if (GET_CODE (val) != REG
      || REGNO (val) < FIRST_PSEUDO_REGISTER)
    val = copy_to_mode_reg (QImode, val);

  if (GET_CODE (count) != REG
      || REGNO (count) < FIRST_PSEUDO_REGISTER)
    count = copy_to_mode_reg (HImode, count);

  if (TARGET_A16)
    emit_insn (gen_setmemhi_bhi_op (desto, counto, val, desta, count));
  else
    emit_insn (gen_setmemhi_bpsi_op (desto, counto, val, desta, count));

  return 1;
}

/* This is a memcpy() opcode.  All operands are implied, so we need to
   arrange for them to be in the right registers.  The opcode wants
   addresses, not [mem] syntax.  $0 is the destination (MEM:BLK), $1
   is the source (MEM:BLK), and $2 the count (HI).  */
int
m32c_expand_cpymemhi(rtx *operands)
{
  rtx desta, srca, count;
  rtx desto, srco, counto;

  desta = XEXP (operands[0], 0);
  srca = XEXP (operands[1], 0);
  count = operands[2];

  desto = gen_reg_rtx (Pmode);
  srco = gen_reg_rtx (Pmode);
  counto = gen_reg_rtx (HImode);

  if (GET_CODE (desta) != REG
      || REGNO (desta) < FIRST_PSEUDO_REGISTER)
    desta = copy_to_mode_reg (Pmode, desta);

  if (GET_CODE (srca) != REG
      || REGNO (srca) < FIRST_PSEUDO_REGISTER)
    srca = copy_to_mode_reg (Pmode, srca);

  /* Similar to setmem, but we don't need to check the value.  */
  if (GET_CODE (count) == CONST_INT
      && ! (INTVAL (count) & 1)
      && (INTVAL (count) > 1))
    {
      count = copy_to_mode_reg (HImode, GEN_INT (INTVAL (count) / 2));
      if (TARGET_A16)
	emit_insn (gen_cpymemhi_whi_op (desto, srco, counto, desta, srca, count));
      else
	emit_insn (gen_cpymemhi_wpsi_op (desto, srco, counto, desta, srca, count));
      return 1;
    }

  /* This is the generalized memset() case.  */
  if (GET_CODE (count) != REG
      || REGNO (count) < FIRST_PSEUDO_REGISTER)
    count = copy_to_mode_reg (HImode, count);

  if (TARGET_A16)
    emit_insn (gen_cpymemhi_bhi_op (desto, srco, counto, desta, srca, count));
  else
    emit_insn (gen_cpymemhi_bpsi_op (desto, srco, counto, desta, srca, count));

  return 1;
}

/* This is a stpcpy() opcode.  $0 is the destination (MEM:BLK) after
   the copy, which should point to the NUL at the end of the string,
   $1 is the destination (MEM:BLK), and $2 is the source (MEM:BLK).
   Since our opcode leaves the destination pointing *after* the NUL,
   we must emit an adjustment.  */
int
m32c_expand_movstr(rtx *operands)
{
  rtx desta, srca;
  rtx desto, srco;

  desta = XEXP (operands[1], 0);
  srca = XEXP (operands[2], 0);

  desto = gen_reg_rtx (Pmode);
  srco = gen_reg_rtx (Pmode);

  if (GET_CODE (desta) != REG
      || REGNO (desta) < FIRST_PSEUDO_REGISTER)
    desta = copy_to_mode_reg (Pmode, desta);

  if (GET_CODE (srca) != REG
      || REGNO (srca) < FIRST_PSEUDO_REGISTER)
    srca = copy_to_mode_reg (Pmode, srca);

  emit_insn (gen_movstr_op (desto, srco, desta, srca));
  /* desto ends up being a1, which allows this type of add through MOVA.  */
  emit_insn (gen_addpsi3 (operands[0], desto, GEN_INT (-1)));

  return 1;
}

/* This is a strcmp() opcode.  $0 is the destination (HI) which holds
   <=>0 depending on the comparison, $1 is one string (MEM:BLK), and
   $2 is the other (MEM:BLK).  We must do the comparison, and then
   convert the flags to a signed integer result.  */
int
m32c_expand_cmpstr(rtx *operands)
{
  rtx src1a, src2a;

  src1a = XEXP (operands[1], 0);
  src2a = XEXP (operands[2], 0);

  if (GET_CODE (src1a) != REG
      || REGNO (src1a) < FIRST_PSEUDO_REGISTER)
    src1a = copy_to_mode_reg (Pmode, src1a);

  if (GET_CODE (src2a) != REG
      || REGNO (src2a) < FIRST_PSEUDO_REGISTER)
    src2a = copy_to_mode_reg (Pmode, src2a);

  emit_insn (gen_cmpstrhi_op (src1a, src2a, src1a, src2a));
  emit_insn (gen_cond_to_int (operands[0]));

  return 1;
}


typedef rtx (*shift_gen_func)(rtx, rtx, rtx);

static shift_gen_func
shift_gen_func_for (int mode, int code)
{
#define GFF(m,c,f) if (mode == m && code == c) return f
  GFF(QImode,  ASHIFT,   gen_ashlqi3_i);
  GFF(QImode,  ASHIFTRT, gen_ashrqi3_i);
  GFF(QImode,  LSHIFTRT, gen_lshrqi3_i);
  GFF(HImode,  ASHIFT,   gen_ashlhi3_i);
  GFF(HImode,  ASHIFTRT, gen_ashrhi3_i);
  GFF(HImode,  LSHIFTRT, gen_lshrhi3_i);
  GFF(PSImode, ASHIFT,   gen_ashlpsi3_i);
  GFF(PSImode, ASHIFTRT, gen_ashrpsi3_i);
  GFF(PSImode, LSHIFTRT, gen_lshrpsi3_i);
  GFF(SImode,  ASHIFT,   TARGET_A16 ? gen_ashlsi3_16 : gen_ashlsi3_24);
  GFF(SImode,  ASHIFTRT, TARGET_A16 ? gen_ashrsi3_16 : gen_ashrsi3_24);
  GFF(SImode,  LSHIFTRT, TARGET_A16 ? gen_lshrsi3_16 : gen_lshrsi3_24);
#undef GFF
  gcc_unreachable ();
}

/* The m32c only has one shift, but it takes a signed count.  GCC
   doesn't want this, so we fake it by negating any shift count when
   we're pretending to shift the other way.  Also, the shift count is
   limited to -8..8.  It's slightly better to use two shifts for 9..15
   than to load the count into r1h, so we do that too.  */
int
m32c_prepare_shift (rtx * operands, int scale, int shift_code)
{
  machine_mode mode = GET_MODE (operands[0]);
  shift_gen_func func = shift_gen_func_for (mode, shift_code);
  rtx temp;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      int maxc = TARGET_A24 && (mode == PSImode || mode == SImode) ? 32 : 8;
      int count = INTVAL (operands[2]) * scale;

      while (count > maxc)
	{
	  temp = gen_reg_rtx (mode);
	  emit_insn (func (temp, operands[1], GEN_INT (maxc)));
	  operands[1] = temp;
	  count -= maxc;
	}
      while (count < -maxc)
	{
	  temp = gen_reg_rtx (mode);
	  emit_insn (func (temp, operands[1], GEN_INT (-maxc)));
	  operands[1] = temp;
	  count += maxc;
	}
      emit_insn (func (operands[0], operands[1], GEN_INT (count)));
      return 1;
    }

  temp = gen_reg_rtx (QImode);
  if (scale < 0)
    /* The pattern has a NEG that corresponds to this. */
    emit_move_insn (temp, gen_rtx_NEG (QImode, operands[2]));
  else if (TARGET_A16 && mode == SImode)
    /* We do this because the code below may modify this, we don't
       want to modify the origin of this value.  */
    emit_move_insn (temp, operands[2]);
  else
    /* We'll only use it for the shift, no point emitting a move.  */
    temp = operands[2];

  if (TARGET_A16 && GET_MODE_SIZE (mode) == 4)
    {
      /* The m16c has a limit of -16..16 for SI shifts, even when the
	 shift count is in a register.  Since there are so many targets
	 of these shifts, it's better to expand the RTL here than to
	 call a helper function.

	 The resulting code looks something like this:

		cmp.b	r1h,-16
		jge.b	1f
		shl.l	-16,dest
		add.b	r1h,16
	1f:	cmp.b	r1h,16
		jle.b	1f
		shl.l	16,dest
		sub.b	r1h,16
	1f:	shl.l	r1h,dest

	 We take advantage of the fact that "negative" shifts are
	 undefined to skip one of the comparisons.  */

      rtx count;
      rtx tempvar;
      rtx_insn *insn;

      emit_move_insn (operands[0], operands[1]);

      count = temp;
      rtx_code_label *label = gen_label_rtx ();
      LABEL_NUSES (label) ++;

      tempvar = gen_reg_rtx (mode);

      if (shift_code == ASHIFT)
	{
	  /* This is a left shift.  We only need check positive counts.  */
	  emit_jump_insn (gen_cbranchqi4 (gen_rtx_LE (VOIDmode, 0, 0),
					  count, GEN_INT (16), label));
	  emit_insn (func (tempvar, operands[0], GEN_INT (8)));
	  emit_insn (func (operands[0], tempvar, GEN_INT (8)));
	  insn = emit_insn (gen_addqi3 (count, count, GEN_INT (-16)));
	  emit_label_after (label, insn);
	}
      else
	{
	  /* This is a right shift.  We only need check negative counts.  */
	  emit_jump_insn (gen_cbranchqi4 (gen_rtx_GE (VOIDmode, 0, 0),
					  count, GEN_INT (-16), label));
	  emit_insn (func (tempvar, operands[0], GEN_INT (-8)));
	  emit_insn (func (operands[0], tempvar, GEN_INT (-8)));
	  insn = emit_insn (gen_addqi3 (count, count, GEN_INT (16)));
	  emit_label_after (label, insn);
	}
      operands[1] = operands[0];
      emit_insn (func (operands[0], operands[0], count));
      return 1;
    }

  operands[2] = temp;
  return 0;
}

/* The m32c has a limited range of operations that work on PSImode
   values; we have to expand to SI, do the math, and truncate back to
   PSI.  Yes, this is expensive, but hopefully gcc will learn to avoid
   those cases.  */
void
m32c_expand_neg_mulpsi3 (rtx * operands)
{
  /* operands: a = b * i */
  rtx temp1; /* b as SI */
  rtx scale /* i as SI */;
  rtx temp2; /* a*b as SI */

  temp1 = gen_reg_rtx (SImode);
  temp2 = gen_reg_rtx (SImode);
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      scale = gen_reg_rtx (SImode);
      emit_insn (gen_zero_extendpsisi2 (scale, operands[2]));
    }
  else
    scale = copy_to_mode_reg (SImode, operands[2]);

  emit_insn (gen_zero_extendpsisi2 (temp1, operands[1]));
  temp2 = expand_simple_binop (SImode, MULT, temp1, scale, temp2, 1, OPTAB_LIB);
  emit_insn (gen_truncsipsi2 (operands[0], temp2));
}

/* Pattern Output Functions */

int
m32c_expand_movcc (rtx *operands)
{
  rtx rel = operands[1];

  if (GET_CODE (rel) != EQ && GET_CODE (rel) != NE)
    return 1;
  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT)
    return 1;
  if (GET_CODE (rel) == NE)
    {
      rtx tmp = operands[2];
      operands[2] = operands[3];
      operands[3] = tmp;
      rel = gen_rtx_EQ (GET_MODE (rel), XEXP (rel, 0), XEXP (rel, 1));
    }

  emit_move_insn (operands[0],
		  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
					rel,
					operands[2],
					operands[3]));
  return 0;
}

/* Used for the "insv" pattern.  Return nonzero to fail, else done.  */
int
m32c_expand_insv (rtx *operands)
{
  rtx op0, src0, p;
  int mask;

  if (INTVAL (operands[1]) != 1)
    return 1;

  /* Our insv opcode (bset, bclr) can only insert a one-bit constant.  */
  if (GET_CODE (operands[3]) != CONST_INT)
    return 1;
  if (INTVAL (operands[3]) != 0
      && INTVAL (operands[3]) != 1
      && INTVAL (operands[3]) != -1)
    return 1;

  mask = 1 << INTVAL (operands[2]);

  op0 = operands[0];
  if (GET_CODE (op0) == SUBREG
      && SUBREG_BYTE (op0) == 0)
    {
      rtx sub = SUBREG_REG (op0);
      if (GET_MODE (sub) == HImode || GET_MODE (sub) == QImode)
	op0 = sub;
    }

  if (!can_create_pseudo_p ()
      || (GET_CODE (op0) == MEM && MEM_VOLATILE_P (op0)))
    src0 = op0;
  else
    {
      src0 = gen_reg_rtx (GET_MODE (op0));
      emit_move_insn (src0, op0);
    }

  if (GET_MODE (op0) == HImode
      && INTVAL (operands[2]) >= 8
      && GET_CODE (op0) == MEM)
    {
      /* We are little endian.  */
      rtx new_mem = gen_rtx_MEM (QImode, plus_constant (Pmode,
							XEXP (op0, 0), 1));
      MEM_COPY_ATTRIBUTES (new_mem, op0);
      mask >>= 8;
    }

  /* First, we generate a mask with the correct polarity.  If we are
     storing a zero, we want an AND mask, so invert it.  */
  if (INTVAL (operands[3]) == 0)
    {
      /* Storing a zero, use an AND mask */
      if (GET_MODE (op0) == HImode)
	mask ^= 0xffff;
      else
	mask ^= 0xff;
    }
  /* Now we need to properly sign-extend the mask in case we need to
     fall back to an AND or OR opcode.  */
  if (GET_MODE (op0) == HImode)
    {
      if (mask & 0x8000)
	mask -= 0x10000;
    }
  else
    {
      if (mask & 0x80)
	mask -= 0x100;
    }

  switch (  (INTVAL (operands[3]) ? 4 : 0)
	  + ((GET_MODE (op0) == HImode) ? 2 : 0)
	  + (TARGET_A24 ? 1 : 0))
    {
    case 0: p = gen_andqi3_16 (op0, src0, GEN_INT (mask)); break;
    case 1: p = gen_andqi3_24 (op0, src0, GEN_INT (mask)); break;
    case 2: p = gen_andhi3_16 (op0, src0, GEN_INT (mask)); break;
    case 3: p = gen_andhi3_24 (op0, src0, GEN_INT (mask)); break;
    case 4: p = gen_iorqi3_16 (op0, src0, GEN_INT (mask)); break;
    case 5: p = gen_iorqi3_24 (op0, src0, GEN_INT (mask)); break;
    case 6: p = gen_iorhi3_16 (op0, src0, GEN_INT (mask)); break;
    case 7: p = gen_iorhi3_24 (op0, src0, GEN_INT (mask)); break;
    default: p = NULL_RTX; break; /* Not reached, but silences a warning.  */
    }

  emit_insn (p);
  return 0;
}

const char *
m32c_scc_pattern(rtx *operands, RTX_CODE code)
{
  static char buf[30];
  if (GET_CODE (operands[0]) == REG
      && REGNO (operands[0]) == R0_REGNO)
    {
      if (code == EQ)
	return "stzx\t#1,#0,r0l";
      if (code == NE)
	return "stzx\t#0,#1,r0l";
    }
  sprintf(buf, "bm%s\t0,%%h0\n\tand.b\t#1,%%0", GET_RTX_NAME (code));
  return buf;
}

/* Encode symbol attributes of a SYMBOL_REF into its
   SYMBOL_REF_FLAGS. */
static void
m32c_encode_section_info (tree decl, rtx rtl, int first)
{
  int extra_flags = 0;

  default_encode_section_info (decl, rtl, first);
  if (TREE_CODE (decl) == FUNCTION_DECL
      && m32c_special_page_vector_p (decl))

    extra_flags = SYMBOL_FLAG_FUNCVEC_FUNCTION;

  if (extra_flags)
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= extra_flags;
}

/* Returns TRUE if the current function is a leaf, and thus we can
   determine which registers an interrupt function really needs to
   save.  The logic below is mostly about finding the insn sequence
   that's the function, versus any sequence that might be open for the
   current insn.  */
static int
m32c_leaf_function_p (void)
{
  int rv;

  push_topmost_sequence ();
  rv = leaf_function_p ();
  pop_topmost_sequence ();
  return rv;
}

/* Returns TRUE if the current function needs to use the ENTER/EXIT
   opcodes.  If the function doesn't need the frame base or stack
   pointer, it can use the simpler RTS opcode.  */
static bool
m32c_function_needs_enter (void)
{
  rtx_insn *insn;
  rtx sp = gen_rtx_REG (Pmode, SP_REGNO);
  rtx fb = gen_rtx_REG (Pmode, FB_REGNO);

  for (insn = get_topmost_sequence ()->first; insn; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      {
	if (reg_mentioned_p (sp, insn))
	  return true;
	if (reg_mentioned_p (fb, insn))
	  return true;
      }
  return false;
}

/* Mark all the subexpressions of the PARALLEL rtx PAR as
   frame-related.  Return PAR.

   dwarf2out.cc:dwarf2out_frame_debug_expr ignores sub-expressions of a
   PARALLEL rtx other than the first if they do not have the
   FRAME_RELATED flag set on them.  So this function is handy for
   marking up 'enter' instructions.  */
static rtx
m32c_all_frame_related (rtx par)
{
  int len = XVECLEN (par, 0);
  int i;

  for (i = 0; i < len; i++)
    F (XVECEXP (par, 0, i));

  return par;
}

/* Emits the prologue.  See the frame layout comment earlier in this
   file.  We can reserve up to 256 bytes with the ENTER opcode, beyond
   that we manually update sp.  */
void
m32c_emit_prologue (void)
{
  int frame_size, extra_frame_size = 0, reg_save_size;
  int complex_prologue = 0;

  cfun->machine->is_leaf = m32c_leaf_function_p ();
  if (interrupt_p (cfun->decl))
    {
      cfun->machine->is_interrupt = 1;
      complex_prologue = 1;
    }
  else if (bank_switch_p (cfun->decl))
    warning (OPT_Wattributes,
	     "%<bank_switch%> has no effect on non-interrupt functions");

  reg_save_size = m32c_pushm_popm (PP_justcount);

  if (interrupt_p (cfun->decl))
    {
      if (bank_switch_p (cfun->decl))
	emit_insn (gen_fset_b ());
      else if (cfun->machine->intr_pushm)
	emit_insn (gen_pushm (GEN_INT (cfun->machine->intr_pushm)));
    }

  frame_size =
    m32c_initial_elimination_offset (FB_REGNO, SP_REGNO) - reg_save_size;
  if (frame_size == 0
      && !m32c_function_needs_enter ())
    cfun->machine->use_rts = 1;

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_size;

  if (frame_size > 254)
    {
      extra_frame_size = frame_size - 254;
      frame_size = 254;
    }
  if (cfun->machine->use_rts == 0)
    F (emit_insn (m32c_all_frame_related
		  (TARGET_A16
		   ? gen_prologue_enter_16 (GEN_INT (frame_size + 2))
		   : gen_prologue_enter_24 (GEN_INT (frame_size + 4)))));

  if (extra_frame_size)
    {
      complex_prologue = 1;
      if (TARGET_A16)
	F (emit_insn (gen_addhi3 (gen_rtx_REG (HImode, SP_REGNO),
				  gen_rtx_REG (HImode, SP_REGNO),
				  GEN_INT (-extra_frame_size))));
      else
	F (emit_insn (gen_addpsi3 (gen_rtx_REG (PSImode, SP_REGNO),
				   gen_rtx_REG (PSImode, SP_REGNO),
				   GEN_INT (-extra_frame_size))));
    }

  complex_prologue += m32c_pushm_popm (PP_pushm);

  /* This just emits a comment into the .s file for debugging.  */
  if (complex_prologue)
    emit_insn (gen_prologue_end ());
}

/* Likewise, for the epilogue.  The only exception is that, for
   interrupts, we must manually unwind the frame as the REIT opcode
   doesn't do that.  */
void
m32c_emit_epilogue (void)
{
  int popm_count = m32c_pushm_popm (PP_justcount);

  /* This just emits a comment into the .s file for debugging.  */
  if (popm_count > 0 || cfun->machine->is_interrupt)
    emit_insn (gen_epilogue_start ());

  if (popm_count > 0)
    m32c_pushm_popm (PP_popm);

  if (cfun->machine->is_interrupt)
    {
      machine_mode spmode = TARGET_A16 ? HImode : PSImode;

      /* REIT clears B flag and restores $fp for us, but we still
	 have to fix up the stack.  USE_RTS just means we didn't
	 emit ENTER.  */
      if (!cfun->machine->use_rts)
	{
	  emit_move_insn (gen_rtx_REG (spmode, A0_REGNO),
			  gen_rtx_REG (spmode, FP_REGNO));
	  emit_move_insn (gen_rtx_REG (spmode, SP_REGNO),
			  gen_rtx_REG (spmode, A0_REGNO));
	  /* We can't just add this to the POPM because it would be in
	     the wrong order, and wouldn't fix the stack if we're bank
	     switching.  */
	  if (TARGET_A16)
	    emit_insn (gen_pophi_16 (gen_rtx_REG (HImode, FP_REGNO)));
	  else
	    emit_insn (gen_poppsi (gen_rtx_REG (PSImode, FP_REGNO)));
	}
      if (!bank_switch_p (cfun->decl) && cfun->machine->intr_pushm)
	emit_insn (gen_popm (GEN_INT (cfun->machine->intr_pushm)));

      /* The FREIT (Fast REturn from InTerrupt) instruction should be
         generated only for M32C/M32CM targets (generate the REIT
         instruction otherwise).  */
      if (fast_interrupt_p (cfun->decl))
        {
          /* Check if fast_attribute is set for M32C or M32CM.  */
          if (TARGET_A24)
            {
              emit_jump_insn (gen_epilogue_freit ());
            }
          /* If fast_interrupt attribute is set for an R8C or M16C
             target ignore this attribute and generated REIT
             instruction.  */
          else
	    {
	      warning (OPT_Wattributes,
		       "%<fast_interrupt%> attribute directive ignored");
	      emit_jump_insn (gen_epilogue_reit_16 ());
	    }
        }
      else if (TARGET_A16)
	emit_jump_insn (gen_epilogue_reit_16 ());
      else
	emit_jump_insn (gen_epilogue_reit_24 ());
    }
  else if (cfun->machine->use_rts)
    emit_jump_insn (gen_epilogue_rts ());
  else if (TARGET_A16)
    emit_jump_insn (gen_epilogue_exitd_16 ());
  else
    emit_jump_insn (gen_epilogue_exitd_24 ());
}

void
m32c_emit_eh_epilogue (rtx ret_addr)
{
  /* R0[R2] has the stack adjustment.  R1[R3] has the address to
     return to.  We have to fudge the stack, pop everything, pop SP
     (fudged), and return (fudged).  This is actually easier to do in
     assembler, so punt to libgcc.  */
  emit_jump_insn (gen_eh_epilogue (ret_addr, cfun->machine->eh_stack_adjust));
  /*  emit_clobber (gen_rtx_REG (HImode, R0L_REGNO)); */
}

/* Indicate which flags must be properly set for a given conditional.  */
static int
flags_needed_for_conditional (rtx cond)
{
  switch (GET_CODE (cond))
    {
    case LE:
    case GT:
      return FLAGS_OSZ;
    case LEU:
    case GTU:
      return FLAGS_ZC;
    case LT:
    case GE:
      return FLAGS_OS;
    case LTU:
    case GEU:
      return FLAGS_C;
    case EQ:
    case NE:
      return FLAGS_Z;
    default:
      return FLAGS_N;
    }
}

#define DEBUG_CMP 0

/* Returns true if a compare insn is redundant because it would only
   set flags that are already set correctly.  */
static bool
m32c_compare_redundant (rtx_insn *cmp, rtx *operands)
{
  int flags_needed;
  int pflags;
  rtx_insn *prev;
  rtx pp, next;
  rtx op0, op1;
#if DEBUG_CMP
  int prev_icode, i;
#endif

  op0 = operands[0];
  op1 = operands[1];

#if DEBUG_CMP
  fprintf(stderr, "\n\033[32mm32c_compare_redundant\033[0m\n");
  debug_rtx(cmp);
  for (i=0; i<2; i++)
    {
      fprintf(stderr, "operands[%d] = ", i);
      debug_rtx(operands[i]);
    }
#endif

  next = next_nonnote_insn (cmp);
  if (!next || !INSN_P (next))
    {
#if DEBUG_CMP
      fprintf(stderr, "compare not followed by insn\n");
      debug_rtx(next);
#endif
      return false;
    }
  if (GET_CODE (PATTERN (next)) == SET
      && GET_CODE (XEXP ( PATTERN (next), 1)) == IF_THEN_ELSE)
    {
      next = XEXP (XEXP (PATTERN (next), 1), 0);
    }
  else if (GET_CODE (PATTERN (next)) == SET)
    {
      /* If this is a conditional, flags_needed will be something
	 other than FLAGS_N, which we test below.  */
      next = XEXP (PATTERN (next), 1);
    }
  else
    {
#if DEBUG_CMP
      fprintf(stderr, "compare not followed by conditional\n");
      debug_rtx(next);
#endif
      return false;
    }
#if DEBUG_CMP
  fprintf(stderr, "conditional is: ");
  debug_rtx(next);
#endif

  flags_needed = flags_needed_for_conditional (next);
  if (flags_needed == FLAGS_N)
    {
#if DEBUG_CMP
      fprintf(stderr, "compare not followed by conditional\n");
      debug_rtx(next);
#endif
      return false;
    }

  /* Compare doesn't set overflow and carry the same way that
     arithmetic instructions do, so we can't replace those.  */
  if (flags_needed & FLAGS_OC)
    return false;

  prev = cmp;
  do {
    prev = prev_nonnote_insn (prev);
    if (!prev)
      {
#if DEBUG_CMP
	fprintf(stderr, "No previous insn.\n");
#endif
	return false;
      }
    if (!INSN_P (prev))
      {
#if DEBUG_CMP
	fprintf(stderr, "Previous insn is a non-insn.\n");
#endif
	return false;
      }
    pp = PATTERN (prev);
    if (GET_CODE (pp) != SET)
      {
#if DEBUG_CMP
	fprintf(stderr, "Previous insn is not a SET.\n");
#endif
	return false;
      }
    pflags = get_attr_flags (prev);

    /* Looking up attributes of previous insns corrupted the recog
       tables.  */
    INSN_UID (cmp) = -1;
    recog (PATTERN (cmp), cmp, 0);

    if (pflags == FLAGS_N
	&& reg_mentioned_p (op0, pp))
      {
#if DEBUG_CMP
	fprintf(stderr, "intermediate non-flags insn uses op:\n");
	debug_rtx(prev);
#endif
	return false;
      }

    /* Check for comparisons against memory - between volatiles and
       aliases, we just can't risk this one.  */
    if (GET_CODE (operands[0]) == MEM
	|| GET_CODE (operands[0]) == MEM)
      {
#if DEBUG_CMP
	fprintf(stderr, "comparisons with memory:\n");
	debug_rtx(prev);
#endif
	return false;
      }

    /* Check for PREV changing a register that's used to compute a
       value in CMP, even if it doesn't otherwise change flags.  */
    if (GET_CODE (operands[0]) == REG
	&& rtx_referenced_p (SET_DEST (PATTERN (prev)), operands[0]))
      {
#if DEBUG_CMP
	fprintf(stderr, "sub-value affected, op0:\n");
	debug_rtx(prev);
#endif
	return false;
      }
    if (GET_CODE (operands[1]) == REG
	&& rtx_referenced_p (SET_DEST (PATTERN (prev)), operands[1]))
      {
#if DEBUG_CMP
	fprintf(stderr, "sub-value affected, op1:\n");
	debug_rtx(prev);
#endif
	return false;
      }

  } while (pflags == FLAGS_N);
#if DEBUG_CMP
  fprintf(stderr, "previous flag-setting insn:\n");
  debug_rtx(prev);
  debug_rtx(pp);
#endif

  if (GET_CODE (pp) == SET
      && GET_CODE (XEXP (pp, 0)) == REG
      && REGNO (XEXP (pp, 0)) == FLG_REGNO
      && GET_CODE (XEXP (pp, 1)) == COMPARE)
    {
      /* Adjacent cbranches must have the same operands to be
	 redundant.  */
      rtx pop0 = XEXP (XEXP (pp, 1), 0);
      rtx pop1 = XEXP (XEXP (pp, 1), 1);
#if DEBUG_CMP
      fprintf(stderr, "adjacent cbranches\n");
      debug_rtx(pop0);
      debug_rtx(pop1);
#endif
      if (rtx_equal_p (op0, pop0)
	  && rtx_equal_p (op1, pop1))
	return true;
#if DEBUG_CMP
      fprintf(stderr, "prev cmp not same\n");
#endif
      return false;
    }

  /* Else the previous insn must be a SET, with either the source or
     dest equal to operands[0], and operands[1] must be zero.  */

  if (!rtx_equal_p (op1, const0_rtx))
    {
#if DEBUG_CMP
      fprintf(stderr, "operands[1] not const0_rtx\n");
#endif
      return false;
    }
  if (GET_CODE (pp) != SET)
    {
#if DEBUG_CMP
      fprintf (stderr, "pp not set\n");
#endif
      return false;
    }
  if (!rtx_equal_p (op0, SET_SRC (pp))
      && !rtx_equal_p (op0, SET_DEST (pp)))
    {
#if DEBUG_CMP
      fprintf(stderr, "operands[0] not found in set\n");
#endif
      return false;
    }

#if DEBUG_CMP
  fprintf(stderr, "cmp flags %x prev flags %x\n", flags_needed, pflags);
#endif
  if ((pflags & flags_needed) == flags_needed)
    return true;

  return false;
}

/* Return the pattern for a compare.  This will be commented out if
   the compare is redundant, else a normal pattern is returned.  Thus,
   the assembler output says where the compare would have been.  */
char *
m32c_output_compare (rtx_insn *insn, rtx *operands)
{
  static char templ[] = ";cmp.b\t%1,%0";
  /*                             ^ 5  */

  templ[5] = " bwll"[GET_MODE_SIZE(GET_MODE(operands[0]))];
  if (m32c_compare_redundant (insn, operands))
    {
#if DEBUG_CMP
      fprintf(stderr, "cbranch: cmp not needed\n");
#endif
      return templ;
    }

#if DEBUG_CMP
  fprintf(stderr, "cbranch: cmp needed: `%s'\n", templ + 1);
#endif
  return templ + 1;
}

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO m32c_encode_section_info

/* If the frame pointer isn't used, we detect it manually.  But the
   stack pointer doesn't have as flexible addressing as the frame
   pointer, so we always assume we have it.  */

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED hook_bool_void_true

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS m32c_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK m32c_hard_regno_mode_ok
#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P m32c_modes_tieable_p

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS m32c_can_change_mode_class

/* The Global `targetm' Variable. */

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-m32c.h"
