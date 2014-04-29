/* Subroutines used for code generation on the Renesas M32R cpu.
   Copyright (C) 1996-2014 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "stringpool.h"
#include "calls.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "dbxout.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "function.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "ggc.h"
#include "df.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "tm-constrs.h"
#include "opts.h"

/* Array of valid operand punctuation characters.  */
static char m32r_punct_chars[256];

/* Machine-specific symbol_ref flags.  */
#define SYMBOL_FLAG_MODEL_SHIFT		SYMBOL_FLAG_MACH_DEP_SHIFT
#define SYMBOL_REF_MODEL(X) \
  ((enum m32r_model) ((SYMBOL_REF_FLAGS (X) >> SYMBOL_FLAG_MODEL_SHIFT) & 3))

/* For string literals, etc.  */
#define LIT_NAME_P(NAME) ((NAME)[0] == '*' && (NAME)[1] == '.')

/* Forward declaration.  */
static void  m32r_option_override (void);
static void  init_reg_tables (void);
static void  block_move_call (rtx, rtx, rtx);
static int   m32r_is_insn (rtx);
static bool  m32r_legitimate_address_p (enum machine_mode, rtx, bool);
static rtx   m32r_legitimize_address (rtx, rtx, enum machine_mode);
static bool  m32r_mode_dependent_address_p (const_rtx, addr_space_t);
static tree  m32r_handle_model_attribute (tree *, tree, tree, int, bool *);
static void  m32r_print_operand (FILE *, rtx, int);
static void  m32r_print_operand_address (FILE *, rtx);
static bool  m32r_print_operand_punct_valid_p (unsigned char code);
static void  m32r_output_function_prologue (FILE *, HOST_WIDE_INT);
static void  m32r_output_function_epilogue (FILE *, HOST_WIDE_INT);

static void  m32r_file_start (void);

static int    m32r_adjust_priority (rtx, int);
static int    m32r_issue_rate (void);

static void m32r_encode_section_info (tree, rtx, int);
static bool m32r_in_small_data_p (const_tree);
static bool m32r_return_in_memory (const_tree, const_tree);
static rtx m32r_function_value (const_tree, const_tree, bool);
static rtx m32r_libcall_value (enum machine_mode, const_rtx);
static bool m32r_function_value_regno_p (const unsigned int);
static void m32r_setup_incoming_varargs (cumulative_args_t, enum machine_mode,
					 tree, int *, int);
static void init_idents (void);
static bool m32r_rtx_costs (rtx, int, int, int, int *, bool speed);
static int m32r_memory_move_cost (enum machine_mode, reg_class_t, bool);
static bool m32r_pass_by_reference (cumulative_args_t, enum machine_mode,
				    const_tree, bool);
static int m32r_arg_partial_bytes (cumulative_args_t, enum machine_mode,
				   tree, bool);
static rtx m32r_function_arg (cumulative_args_t, enum machine_mode,
			      const_tree, bool);
static void m32r_function_arg_advance (cumulative_args_t, enum machine_mode,
				       const_tree, bool);
static bool m32r_can_eliminate (const int, const int);
static void m32r_conditional_register_usage (void);
static void m32r_trampoline_init (rtx, tree, rtx);
static bool m32r_legitimate_constant_p (enum machine_mode, rtx);

/* M32R specific attributes.  */

static const struct attribute_spec m32r_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "interrupt", 0, 0, true,  false, false, NULL, false },
  { "model",     1, 1, true,  false, false, m32r_handle_model_attribute,
    false },
  { NULL,        0, 0, false, false, false, NULL, false }
};

/* Initialize the GCC target structure.  */
#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE m32r_attribute_table

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P m32r_legitimate_address_p
#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS m32r_legitimize_address
#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P m32r_mode_dependent_address_p

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND m32r_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS m32r_print_operand_address
#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P m32r_print_operand_punct_valid_p

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE m32r_output_function_prologue
#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE m32r_output_function_epilogue

#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START m32r_file_start

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY m32r_adjust_priority
#undef  TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE m32r_issue_rate

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE m32r_option_override

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO m32r_encode_section_info
#undef  TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P m32r_in_small_data_p


#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST m32r_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS m32r_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY m32r_return_in_memory

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE m32r_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE m32r_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P m32r_function_value_regno_p

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS m32r_setup_incoming_varargs
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE m32r_pass_by_reference
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES m32r_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG m32r_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE m32r_function_arg_advance

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE m32r_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE m32r_conditional_register_usage

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT m32r_trampoline_init

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P m32r_legitimate_constant_p

struct gcc_target targetm = TARGET_INITIALIZER;

/* Called by m32r_option_override to initialize various things.  */

void
m32r_init (void)
{
  init_reg_tables ();

  /* Initialize array for TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */
  memset (m32r_punct_chars, 0, sizeof (m32r_punct_chars));
  m32r_punct_chars['#'] = 1;
  m32r_punct_chars['@'] = 1; /* ??? no longer used */

  /* Provide default value if not specified.  */
  if (!global_options_set.x_g_switch_value)
    g_switch_value = SDATA_DEFAULT_SIZE;
}

static void
m32r_option_override (void)
{
  /* These need to be done at start up.
     It's convenient to do them here.  */
  m32r_init ();
  SUBTARGET_OVERRIDE_OPTIONS;
}

/* Vectors to keep interesting information about registers where it can easily
   be got.  We use to use the actual mode value as the bit number, but there
   is (or may be) more than 32 modes now.  Instead we use two tables: one
   indexed by hard register number, and one indexed by mode.  */

/* The purpose of m32r_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32-bit word (again).  Each real mode is
   mapped into one m32r_mode_class mode.  */

enum m32r_mode_class
{
  C_MODE,
  S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE, A_MODE
};

/* Modes for condition codes.  */
#define C_MODES (1 << (int) C_MODE)

/* Modes for single-word and smaller quantities.  */
#define S_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << DF_MODE))

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Modes for accumulators.  */
#define A_MODES (1 << (int) A_MODE)

/* Value is 1 if register/mode pair is acceptable on arc.  */

const unsigned int m32r_hard_regno_mode_ok[FIRST_PSEUDO_REGISTER] =
{
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, C_MODES, A_MODES, A_MODES
};

unsigned int m32r_mode_class [NUM_MACHINE_MODES];

enum reg_class m32r_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
init_reg_tables (void)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      enum machine_mode m = (enum machine_mode) i;
      
      switch (GET_MODE_CLASS (m))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (GET_MODE_SIZE (m) <= 4)
	    m32r_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (m) == 8)
	    m32r_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (m) == 16)
	    m32r_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (m) == 32)
	    m32r_mode_class[i] = 1 << (int) O_MODE;
	  else
	    m32r_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (m) <= 4)
	    m32r_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (m) == 8)
	    m32r_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (m) == 16)
	    m32r_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (m) == 32)
	    m32r_mode_class[i] = 1 << (int) OF_MODE;
	  else
	    m32r_mode_class[i] = 0;
	  break;
	case MODE_CC:
	  m32r_mode_class[i] = 1 << (int) C_MODE;
	  break;
	default:
	  m32r_mode_class[i] = 0;
	  break;
	}
    }

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (GPR_P (i))
	m32r_regno_reg_class[i] = GENERAL_REGS;
      else if (i == ARG_POINTER_REGNUM)
	m32r_regno_reg_class[i] = GENERAL_REGS;
      else
	m32r_regno_reg_class[i] = NO_REGS;
    }
}

/* M32R specific attribute support.

   interrupt - for interrupt functions

   model - select code model used to access object

	small: addresses use 24 bits, use bl to make calls
	medium: addresses use 32 bits, use bl to make calls
	large: addresses use 32 bits, use seth/add3/jl to make calls

	Grep for MODEL in m32r.h for more info.  */

static tree small_ident1;
static tree small_ident2;
static tree medium_ident1;
static tree medium_ident2;
static tree large_ident1;
static tree large_ident2;

static void
init_idents (void)
{
  if (small_ident1 == 0)
    {
      small_ident1 = get_identifier ("small");
      small_ident2 = get_identifier ("__small__");
      medium_ident1 = get_identifier ("medium");
      medium_ident2 = get_identifier ("__medium__");
      large_ident1 = get_identifier ("large");
      large_ident2 = get_identifier ("__large__");
    }
}

/* Handle an "model" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
m32r_handle_model_attribute (tree *node ATTRIBUTE_UNUSED, tree name,
			     tree args, int flags ATTRIBUTE_UNUSED,
			     bool *no_add_attrs)
{
  tree arg;

  init_idents ();
  arg = TREE_VALUE (args);

  if (arg != small_ident1
      && arg != small_ident2
      && arg != medium_ident1
      && arg != medium_ident2
      && arg != large_ident1
      && arg != large_ident2)
    {
      warning (OPT_Wattributes, "invalid argument of %qs attribute",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Encode section information of DECL, which is either a VAR_DECL,
   FUNCTION_DECL, STRING_CST, CONSTRUCTOR, or ???.

   For the M32R we want to record:

   - whether the object lives in .sdata/.sbss.
   - what code model should be used to access the object
*/

static void
m32r_encode_section_info (tree decl, rtx rtl, int first)
{
  int extra_flags = 0;
  tree model_attr;
  enum m32r_model model;

  default_encode_section_info (decl, rtl, first);

  if (!DECL_P (decl))
    return;

  model_attr = lookup_attribute ("model", DECL_ATTRIBUTES (decl));
  if (model_attr)
    {
      tree id;

      init_idents ();

      id = TREE_VALUE (TREE_VALUE (model_attr));

      if (id == small_ident1 || id == small_ident2)
	model = M32R_MODEL_SMALL;
      else if (id == medium_ident1 || id == medium_ident2)
	model = M32R_MODEL_MEDIUM;
      else if (id == large_ident1 || id == large_ident2)
	model = M32R_MODEL_LARGE;
      else
	gcc_unreachable (); /* shouldn't happen */
    }
  else
    {
      if (TARGET_MODEL_SMALL)
	model = M32R_MODEL_SMALL;
      else if (TARGET_MODEL_MEDIUM)
	model = M32R_MODEL_MEDIUM;
      else if (TARGET_MODEL_LARGE)
	model = M32R_MODEL_LARGE;
      else
	gcc_unreachable (); /* shouldn't happen */
    }
  extra_flags |= model << SYMBOL_FLAG_MODEL_SHIFT;

  if (extra_flags)
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= extra_flags;
}

/* Only mark the object as being small data area addressable if
   it hasn't been explicitly marked with a code model.

   The user can explicitly put an object in the small data area with the
   section attribute.  If the object is in sdata/sbss and marked with a
   code model do both [put the object in .sdata and mark it as being
   addressed with a specific code model - don't mark it as being addressed
   with an SDA reloc though].  This is ok and might be useful at times.  If
   the object doesn't fit the linker will give an error.  */

static bool
m32r_in_small_data_p (const_tree decl)
{
  const_tree section;

  if (TREE_CODE (decl) != VAR_DECL)
    return false;

  if (lookup_attribute ("model", DECL_ATTRIBUTES (decl)))
    return false;

  section = DECL_SECTION_NAME (decl);
  if (section)
    {
      const char *const name = TREE_STRING_POINTER (section);
      if (strcmp (name, ".sdata") == 0 || strcmp (name, ".sbss") == 0)
	return true;
    }
  else
    {
      if (! TREE_READONLY (decl) && ! TARGET_SDATA_NONE)
	{
	  int size = int_size_in_bytes (TREE_TYPE (decl));

	  if (size > 0 && size <= g_switch_value)
	    return true;
	}
    }

  return false;
}

/* Do anything needed before RTL is emitted for each function.  */

void
m32r_init_expanders (void)
{
  /* ??? At one point there was code here.  The function is left in
     to make it easy to experiment.  */
}

int
call_operand (rtx op, enum machine_mode mode)
{
  if (!MEM_P (op))
    return 0;
  op = XEXP (op, 0);
  return call_address_operand (op, mode);
}

/* Return 1 if OP is a reference to an object in .sdata/.sbss.  */

int
small_data_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (! TARGET_SDATA_USE)
    return 0;

  if (GET_CODE (op) == SYMBOL_REF)
    return SYMBOL_REF_SMALL_P (op);

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && satisfies_constraint_J (XEXP (XEXP (op, 0), 1)))
    return SYMBOL_REF_SMALL_P (XEXP (XEXP (op, 0), 0));

  return 0;
}

/* Return 1 if OP is a symbol that can use 24-bit addressing.  */

int
addr24_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx sym;

  if (flag_pic)
    return 0;

  if (GET_CODE (op) == LABEL_REF)
    return TARGET_ADDR24;

  if (GET_CODE (op) == SYMBOL_REF)
    sym = op;
  else if (GET_CODE (op) == CONST
	   && GET_CODE (XEXP (op, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	   && satisfies_constraint_M (XEXP (XEXP (op, 0), 1)))
    sym = XEXP (XEXP (op, 0), 0);
  else
    return 0;

  if (SYMBOL_REF_MODEL (sym) == M32R_MODEL_SMALL)
    return 1;

  if (TARGET_ADDR24
      && (CONSTANT_POOL_ADDRESS_P (sym)
	  || LIT_NAME_P (XSTR (sym, 0))))
    return 1;

  return 0;
}

/* Return 1 if OP is a symbol that needs 32-bit addressing.  */

int
addr32_operand (rtx op, enum machine_mode mode)
{
  rtx sym;

  if (GET_CODE (op) == LABEL_REF)
    return TARGET_ADDR32;

  if (GET_CODE (op) == SYMBOL_REF)
    sym = op;
  else if (GET_CODE (op) == CONST
	   && GET_CODE (XEXP (op, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	   && CONST_INT_P (XEXP (XEXP (op, 0), 1))
	   && ! flag_pic)
    sym = XEXP (XEXP (op, 0), 0);
  else
    return 0;

  return (! addr24_operand (sym, mode)
	  && ! small_data_operand (sym, mode));
}

/* Return 1 if OP is a function that can be called with the `bl' insn.  */

int
call26_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (flag_pic)
    return 1;

  if (GET_CODE (op) == SYMBOL_REF)
    return SYMBOL_REF_MODEL (op) != M32R_MODEL_LARGE;

  return TARGET_CALL26;
}

/* Return 1 if OP is a DImode const we want to handle inline.
   This must match the code in the movdi pattern.
   It is used by the 'G' CONST_DOUBLE_OK_FOR_LETTER.  */

int
easy_di_const (rtx op)
{
  rtx high_rtx, low_rtx;
  HOST_WIDE_INT high, low;

  split_double (op, &high_rtx, &low_rtx);
  high = INTVAL (high_rtx);
  low = INTVAL (low_rtx);
  /* Pick constants loadable with 2 16-bit `ldi' insns.  */
  if (high >= -128 && high <= 127
      && low >= -128 && low <= 127)
    return 1;
  return 0;
}

/* Return 1 if OP is a DFmode const we want to handle inline.
   This must match the code in the movdf pattern.
   It is used by the 'H' CONST_DOUBLE_OK_FOR_LETTER.  */

int
easy_df_const (rtx op)
{
  REAL_VALUE_TYPE r;
  long l[2];

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);
  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
  if (l[0] == 0 && l[1] == 0)
    return 1;
  if ((l[0] & 0xffff) == 0 && l[1] == 0)
    return 1;
  return 0;
}

/* Return 1 if OP is (mem (reg ...)).
   This is used in insn length calcs.  */

int
memreg_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return MEM_P (op) && REG_P (XEXP (op, 0));
}

/* Return nonzero if TYPE must be passed by indirect reference.  */

static bool
m32r_pass_by_reference (cumulative_args_t ca ATTRIBUTE_UNUSED,
			enum machine_mode mode, const_tree type,
			bool named ATTRIBUTE_UNUSED)
{
  int size;

  if (type)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  return (size < 0 || size > 8);
}

/* Comparisons.  */

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for compare [arg0 of the if_then_else].
   If need_compare is true then the comparison insn must be generated, rather
   than being subsumed into the following branch instruction.  */

rtx
gen_compare (enum rtx_code code, rtx x, rtx y, int need_compare)
{
  enum rtx_code compare_code;
  enum rtx_code branch_code;
  rtx cc_reg = gen_rtx_REG (CCmode, CARRY_REGNUM);
  int must_swap = 0;

  switch (code)
    {
    case EQ:  compare_code = EQ;  branch_code = NE; break;
    case NE:  compare_code = EQ;  branch_code = EQ; break;
    case LT:  compare_code = LT;  branch_code = NE; break;
    case LE:  compare_code = LT;  branch_code = EQ; must_swap = 1; break;
    case GT:  compare_code = LT;  branch_code = NE; must_swap = 1; break;
    case GE:  compare_code = LT;  branch_code = EQ; break;
    case LTU: compare_code = LTU; branch_code = NE; break;
    case LEU: compare_code = LTU; branch_code = EQ; must_swap = 1; break;
    case GTU: compare_code = LTU; branch_code = NE; must_swap = 1; break;
    case GEU: compare_code = LTU; branch_code = EQ; break;

    default:
      gcc_unreachable ();
    }

  if (need_compare)
    {
      switch (compare_code)
	{
	case EQ:
	  if (satisfies_constraint_P (y)		/* Reg equal to small const.  */
	      && y != const0_rtx)
	    {
	      rtx tmp = gen_reg_rtx (SImode);

	      emit_insn (gen_addsi3 (tmp, x, GEN_INT (-INTVAL (y))));
	      x = tmp;
	      y = const0_rtx;
	    }
	  else if (CONSTANT_P (y))			/* Reg equal to const.  */
	    {
	      rtx tmp = force_reg (GET_MODE (x), y);
	      y = tmp;
	    }

	  if (register_operand (y, SImode) 		/* Reg equal to reg.  */
	      || y == const0_rtx) 	   		/* Reg equal to zero.  */
	    {
	      emit_insn (gen_cmp_eqsi_insn (x, y));

	      return gen_rtx_fmt_ee (code, CCmode, cc_reg, const0_rtx);
	    }
	  break;

	case LT:
	  if (register_operand (y, SImode)
	      || satisfies_constraint_P (y))
	    {
	      rtx tmp = gen_reg_rtx (SImode);	      /* Reg compared to reg.  */

	      switch (code)
		{
		case LT:
		  emit_insn (gen_cmp_ltsi_insn (x, y));
		  code = EQ;
		  break;
		case LE:
		  if (y == const0_rtx)
		    tmp = const1_rtx;
		  else
		    emit_insn (gen_addsi3 (tmp, y, constm1_rtx));
		  emit_insn (gen_cmp_ltsi_insn (x, tmp));
		  code = EQ;
		  break;
		case GT:
		  if (CONST_INT_P (y))
		    tmp = gen_rtx_PLUS (SImode, y, const1_rtx);
		  else
		    emit_insn (gen_addsi3 (tmp, y, constm1_rtx));
		  emit_insn (gen_cmp_ltsi_insn (x, tmp));
		  code = NE;
		  break;
		case GE:
		  emit_insn (gen_cmp_ltsi_insn (x, y));
		  code = NE;
		  break;
		default:
		  gcc_unreachable ();
		}

	      return gen_rtx_fmt_ee (code, CCmode, cc_reg, const0_rtx);
	    }
	  break;

	case LTU:
	  if (register_operand (y, SImode)
	      || satisfies_constraint_P (y))
	    {
	      rtx tmp = gen_reg_rtx (SImode);	      /* Reg (unsigned) compared to reg.  */

	      switch (code)
		{
		case LTU:
		  emit_insn (gen_cmp_ltusi_insn (x, y));
		  code = EQ;
		  break;
		case LEU:
		  if (y == const0_rtx)
		    tmp = const1_rtx;
		  else
		    emit_insn (gen_addsi3 (tmp, y, constm1_rtx));
		  emit_insn (gen_cmp_ltusi_insn (x, tmp));
		  code = EQ;
		  break;
		case GTU:
		  if (CONST_INT_P (y))
		    tmp = gen_rtx_PLUS (SImode, y, const1_rtx);
		  else
		    emit_insn (gen_addsi3 (tmp, y, constm1_rtx));
		  emit_insn (gen_cmp_ltusi_insn (x, tmp));
		  code = NE;
		  break;
		case GEU:
		  emit_insn (gen_cmp_ltusi_insn (x, y));
		  code = NE;
		  break;
		default:
		  gcc_unreachable ();
		}

	      return gen_rtx_fmt_ee (code, CCmode, cc_reg, const0_rtx);
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* Reg/reg equal comparison.  */
      if (compare_code == EQ
	  && register_operand (y, SImode))
	return gen_rtx_fmt_ee (code, CCmode, x, y);

      /* Reg/zero signed comparison.  */
      if ((compare_code == EQ || compare_code == LT)
	  && y == const0_rtx)
	return gen_rtx_fmt_ee (code, CCmode, x, y);

      /* Reg/smallconst equal comparison.  */
      if (compare_code == EQ
	  && satisfies_constraint_P (y))
	{
	  rtx tmp = gen_reg_rtx (SImode);

	  emit_insn (gen_addsi3 (tmp, x, GEN_INT (-INTVAL (y))));
	  return gen_rtx_fmt_ee (code, CCmode, tmp, const0_rtx);
	}

      /* Reg/const equal comparison.  */
      if (compare_code == EQ
	  && CONSTANT_P (y))
	{
	  rtx tmp = force_reg (GET_MODE (x), y);

	  return gen_rtx_fmt_ee (code, CCmode, x, tmp);
	}
    }

  if (CONSTANT_P (y))
    {
      if (must_swap)
	y = force_reg (GET_MODE (x), y);
      else
	{
	  int ok_const = reg_or_int16_operand (y, GET_MODE (y));

	  if (! ok_const)
	    y = force_reg (GET_MODE (x), y);
	}
    }

  switch (compare_code)
    {
    case EQ :
      emit_insn (gen_cmp_eqsi_insn (must_swap ? y : x, must_swap ? x : y));
      break;
    case LT :
      emit_insn (gen_cmp_ltsi_insn (must_swap ? y : x, must_swap ? x : y));
      break;
    case LTU :
      emit_insn (gen_cmp_ltusi_insn (must_swap ? y : x, must_swap ? x : y));
      break;

    default:
      gcc_unreachable ();
    }

  return gen_rtx_fmt_ee (branch_code, VOIDmode, cc_reg, CONST0_RTX (CCmode));
}

bool
gen_cond_store (enum rtx_code code, rtx op0, rtx op1, rtx op2)
{
  enum machine_mode mode = GET_MODE (op0);

  gcc_assert (mode == SImode);
  switch (code)
    {
    case EQ:
      if (!register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      if (TARGET_M32RX || TARGET_M32R2)
	{
	  if (!reg_or_zero_operand (op2, mode))
	    op2 = force_reg (mode, op2);

	  emit_insn (gen_seq_insn_m32rx (op0, op1, op2));
	  return true;
	}
      if (CONST_INT_P (op2) && INTVAL (op2) == 0)
	{
	  emit_insn (gen_seq_zero_insn (op0, op1));
	  return true;
	}

      if (!reg_or_eq_int16_operand (op2, mode))
	op2 = force_reg (mode, op2);

      emit_insn (gen_seq_insn (op0, op1, op2));
      return true;

    case NE:
      if (!CONST_INT_P (op2)
	  || (INTVAL (op2) != 0 && satisfies_constraint_K (op2)))
	{
	  rtx reg;

	  if (reload_completed || reload_in_progress)
	    return false;

	  reg = gen_reg_rtx (SImode);
	  emit_insn (gen_xorsi3 (reg, op1, op2));
	  op1 = reg;

	  if (!register_operand (op1, mode))
	    op1 = force_reg (mode, op1);

	  emit_insn (gen_sne_zero_insn (op0, op1));
	  return true;
	}
      return false;

    case LT:
    case GT:
      if (code == GT)
	{
	  rtx tmp = op2;
	  op2 = op1;
	  op1 = tmp;
	  code = LT;
	}

      if (!register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      if (!reg_or_int16_operand (op2, mode))
	op2 = force_reg (mode, op2);

      emit_insn (gen_slt_insn (op0, op1, op2));
      return true;

    case LTU:
    case GTU:
      if (code == GTU)
	{
	  rtx tmp = op2;
	  op2 = op1;
	  op1 = tmp;
	  code = LTU;
	}

      if (!register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      if (!reg_or_int16_operand (op2, mode))
	op2 = force_reg (mode, op2);

      emit_insn (gen_sltu_insn (op0, op1, op2));
      return true;

    case GE:
    case GEU:
      if (!register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      if (!reg_or_int16_operand (op2, mode))
	op2 = force_reg (mode, op2);

      if (code == GE)
	emit_insn (gen_sge_insn (op0, op1, op2));
      else
	emit_insn (gen_sgeu_insn (op0, op1, op2));
      return true;

    case LE:
    case LEU:
      if (!register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      if (CONST_INT_P (op2))
	{
	  HOST_WIDE_INT value = INTVAL (op2);
	  if (value >= 2147483647)
	    {
	      emit_move_insn (op0, const1_rtx);
	      return true;
	    }

	  op2 = GEN_INT (value + 1);
	  if (value < -32768 || value >= 32767)
	    op2 = force_reg (mode, op2);

          if (code == LEU)
	    emit_insn (gen_sltu_insn (op0, op1, op2));
	  else
	    emit_insn (gen_slt_insn (op0, op1, op2));
	  return true;
	}

      if (!register_operand (op2, mode))
	op2 = force_reg (mode, op2);

      if (code == LEU)
        emit_insn (gen_sleu_insn (op0, op1, op2));
      else
        emit_insn (gen_sle_insn (op0, op1, op2));
      return true;

    default:
      gcc_unreachable ();
    }
}


/* Split a 2 word move (DI or DF) into component parts.  */

rtx
gen_split_move_double (rtx operands[])
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

  /* We might have (SUBREG (MEM)) here, so just get rid of the
     subregs to make this code simpler.  It is safe to call
     alter_subreg any time after reload.  */
  if (GET_CODE (dest) == SUBREG)
    alter_subreg (&dest, true);
  if (GET_CODE (src) == SUBREG)
    alter_subreg (&src, true);

  start_sequence ();
  if (REG_P (dest))
    {
      int dregno = REGNO (dest);

      /* Reg = reg.  */
      if (REG_P (src))
	{
	  int sregno = REGNO (src);

	  int reverse = (dregno == sregno + 1);

	  /* We normally copy the low-numbered register first.  However, if
	     the first register operand 0 is the same as the second register of
	     operand 1, we must copy in the opposite order.  */
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, reverse, TRUE, mode),
				  operand_subword (src,  reverse, TRUE, mode)));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, !reverse, TRUE, mode),
				  operand_subword (src,  !reverse, TRUE, mode)));
	}

      /* Reg = constant.  */
      else if (CONST_INT_P (src) || GET_CODE (src) == CONST_DOUBLE)
	{
	  rtx words[2];
	  split_double (src, &words[0], &words[1]);
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 0, TRUE, mode),
				  words[0]));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 1, TRUE, mode),
				  words[1]));
	}

      /* Reg = mem.  */
      else if (MEM_P (src))
	{
	  /* If the high-address word is used in the address, we must load it
	     last.  Otherwise, load it first.  */
	  int reverse
	    = (refers_to_regno_p (dregno, dregno + 1, XEXP (src, 0), 0) != 0);

	  /* We used to optimize loads from single registers as

		ld r1,r3+; ld r2,r3

	     if r3 were not used subsequently.  However, the REG_NOTES aren't
	     propagated correctly by the reload phase, and it can cause bad
	     code to be generated.  We could still try:

		ld r1,r3+; ld r2,r3; addi r3,-4

	     which saves 2 bytes and doesn't force longword alignment.  */
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, reverse, TRUE, mode),
				  adjust_address (src, SImode,
						  reverse * UNITS_PER_WORD)));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, !reverse, TRUE, mode),
				  adjust_address (src, SImode,
						  !reverse * UNITS_PER_WORD)));
	}
      else
	gcc_unreachable ();
    }

  /* Mem = reg.  */
  /* We used to optimize loads from single registers as

	st r1,r3; st r2,+r3

     if r3 were not used subsequently.  However, the REG_NOTES aren't
     propagated correctly by the reload phase, and it can cause bad
     code to be generated.  We could still try:

	st r1,r3; st r2,+r3; addi r3,-4

     which saves 2 bytes and doesn't force longword alignment.  */
  else if (MEM_P (dest) && REG_P (src))
    {
      emit_insn (gen_rtx_SET (VOIDmode,
			      adjust_address (dest, SImode, 0),
			      operand_subword (src, 0, TRUE, mode)));

      emit_insn (gen_rtx_SET (VOIDmode,
			      adjust_address (dest, SImode, UNITS_PER_WORD),
			      operand_subword (src, 1, TRUE, mode)));
    }

  else
    gcc_unreachable ();

  val = get_insns ();
  end_sequence ();
  return val;
}


static int
m32r_arg_partial_bytes (cumulative_args_t cum_v, enum machine_mode mode,
			tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  int words;
  unsigned int size =
    (((mode == BLKmode && type)
      ? (unsigned int) int_size_in_bytes (type)
      : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
    / UNITS_PER_WORD;

  if (*cum >= M32R_MAX_PARM_REGS)
    words = 0;
  else if (*cum + size > M32R_MAX_PARM_REGS)
    words = (*cum + size) - M32R_MAX_PARM_REGS;
  else
    words = 0;

  return words * UNITS_PER_WORD;
}

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
  ((MODE) == BLKmode				\
   ? ROUND_ADVANCE ((unsigned int) int_size_in_bytes (TYPE))	\
   : ROUND_ADVANCE ((unsigned int) GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) (CUM)

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   This macro is only used in this file.  */
#define PASS_IN_REG_P(CUM, MODE, TYPE) \
  (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) < M32R_MAX_PARM_REGS)

/* Determine where to put an argument to a function.
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
/* On the M32R the first M32R_MAX_PARM_REGS args are normally in registers
   and the rest are pushed.  */

static rtx
m32r_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  return (PASS_IN_REG_P (*cum, mode, type)
	  ? gen_rtx_REG (mode, ROUND_ADVANCE_CUM (*cum, mode, type))
	  : NULL_RTX);
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

static void
m32r_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum = (ROUND_ADVANCE_CUM (*cum, mode, type)
	  + ROUND_ADVANCE_ARG (mode, type));
}

/* Worker function for TARGET_RETURN_IN_MEMORY.  */

static bool
m32r_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  cumulative_args_t dummy = pack_cumulative_args (NULL);

  return m32r_pass_by_reference (dummy, TYPE_MODE (type), type, false);
}

/* Worker function for TARGET_FUNCTION_VALUE.  */

static rtx
m32r_function_value (const_tree valtype,
		const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), 0);
}

/* Worker function for TARGET_LIBCALL_VALUE.  */

static rtx
m32r_libcall_value (enum machine_mode mode,
		const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 0);
}

/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.

  ??? What about r1 in DI/DF values.  */

static bool
m32r_function_value_regno_p (const unsigned int regno)
{
  return (regno == 0);
}

/* Do any needed setup for a variadic function.  For the M32R, we must
   create a register parameter block, and then copy any anonymous arguments
   in registers to memory.

   CUM has not been updated for the last named argument which has type TYPE
   and mode MODE, and we rely on this fact.  */

static void
m32r_setup_incoming_varargs (cumulative_args_t cum, enum machine_mode mode,
			     tree type, int *pretend_size, int no_rtl)
{
  int first_anon_arg;

  if (no_rtl)
    return;

  /* All BLKmode values are passed by reference.  */
  gcc_assert (mode != BLKmode);

  first_anon_arg = (ROUND_ADVANCE_CUM (*get_cumulative_args (cum), mode, type)
		    + ROUND_ADVANCE_ARG (mode, type));

  if (first_anon_arg < M32R_MAX_PARM_REGS)
    {
      /* Note that first_reg_offset < M32R_MAX_PARM_REGS.  */
      int first_reg_offset = first_anon_arg;
      /* Size in words to "pretend" allocate.  */
      int size = M32R_MAX_PARM_REGS - first_reg_offset;
      rtx regblock;

      regblock = gen_frame_mem (BLKmode,
				plus_constant (Pmode, arg_pointer_rtx,
					       FIRST_PARM_OFFSET (0)));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      move_block_from_reg (first_reg_offset, regblock, size);

      *pretend_size = (size * UNITS_PER_WORD);
    }
}


/* Return true if INSN is real instruction bearing insn.  */

static int
m32r_is_insn (rtx insn)
{
  return (NONDEBUG_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER);
}

/* Increase the priority of long instructions so that the
   short instructions are scheduled ahead of the long ones.  */

static int
m32r_adjust_priority (rtx insn, int priority)
{
  if (m32r_is_insn (insn)
      && get_attr_insn_size (insn) != INSN_SIZE_SHORT)
    priority <<= 3;

  return priority;
}


/* Indicate how many instructions can be issued at the same time.
   This is sort of a lie.  The m32r can issue only 1 long insn at
   once, but it can issue 2 short insns.  The default therefore is
   set at 2, but this can be overridden by the command line option
   -missue-rate=1.  */

static int
m32r_issue_rate (void)
{
  return ((TARGET_LOW_ISSUE_RATE) ? 1 : 2);
}

/* Cost functions.  */
/* Memory is 3 times as expensive as registers.
   ??? Is that the right way to look at it?  */

static int
m32r_memory_move_cost (enum machine_mode mode,
		       reg_class_t rclass ATTRIBUTE_UNUSED,
		       bool in ATTRIBUTE_UNUSED)
{
  if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
    return 6;
  else
    return 12;
}

static bool
m32r_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		int opno ATTRIBUTE_UNUSED, int *total,
		bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
      /* Small integers are as cheap as registers.  4 byte values can be
         fetched as immediate constants - let's give that the cost of an
         extra insn.  */
    case CONST_INT:
      if (INT16_P (INTVAL (x)))
	{
	  *total = 0;
	  return true;
	}
      /* FALLTHRU */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (1);
      return true;

    case CONST_DOUBLE:
      {
	rtx high, low;

	split_double (x, &high, &low);
	*total = COSTS_N_INSNS (!INT16_P (INTVAL (high))
			        + !INT16_P (INTVAL (low)));
	return true;
      }

    case MULT:
      *total = COSTS_N_INSNS (3);
      return true;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (10);
      return true;

    default:
      return false;
    }
}

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

enum m32r_function_type
m32r_compute_function_type (tree decl)
{
  /* Cached value.  */
  static enum m32r_function_type fn_type = M32R_FUNCTION_UNKNOWN;
  /* Last function we were called for.  */
  static tree last_fn = NULL_TREE;

  /* Resetting the cached value?  */
  if (decl == NULL_TREE)
    {
      fn_type = M32R_FUNCTION_UNKNOWN;
      last_fn = NULL_TREE;
      return fn_type;
    }

  if (decl == last_fn && fn_type != M32R_FUNCTION_UNKNOWN)
    return fn_type;

  /* Compute function type.  */
  fn_type = (lookup_attribute ("interrupt", DECL_ATTRIBUTES (current_function_decl)) != NULL_TREE
	     ? M32R_FUNCTION_INTERRUPT
	     : M32R_FUNCTION_NORMAL);

  last_fn = decl;
  return fn_type;
}
/* Function prologue/epilogue handlers.  */

/* M32R stack frames look like:

             Before call                       After call
        +-----------------------+       +-----------------------+
        |                       |       |                       |
   high |  local variables,     |       |  local variables,     |
   mem  |  reg save area, etc.  |       |  reg save area, etc.  |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        |  arguments on stack.  |       |  arguments on stack.  |
        |                       |       |                       |
  SP+0->+-----------------------+       +-----------------------+
                                        |  reg parm save area,  |
                                        |  only created for     |
                                        |  variable argument    |
                                        |  functions            |
					+-----------------------+
                                        |   previous frame ptr  |
                                        +-----------------------+
                                        |                       |
                                        |  register save area   |
                                        |                       |
					+-----------------------+
                                        |    return address     |
                                        +-----------------------+
                                        |                       |
                                        |  local variables      |
                                        |                       |
                                        +-----------------------+
                                        |                       |
                                        |  alloca allocations   |
                                        |                       |
                                        +-----------------------+
                                        |                       |
   low                                  |  arguments on stack   |
   memory                               |                       |
                                  SP+0->+-----------------------+

Notes:
1) The "reg parm save area" does not exist for non variable argument fns.
2) The "reg parm save area" can be eliminated completely if we saved regs
   containing anonymous args separately but that complicates things too
   much (so it's not done).
3) The return address is saved after the register save area so as to have as
   many insns as possible between the restoration of `lr' and the `jmp lr'.  */

/* Structure to be filled in by m32r_compute_frame_size with register
   save masks, and offsets for the current function.  */
struct m32r_frame_info
{
  unsigned int total_size;	/* # bytes that the entire frame takes up.  */
  unsigned int extra_size;	/* # bytes of extra stuff.  */
  unsigned int pretend_size;	/* # bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # bytes needed to store regs.  */
  unsigned int var_size;	/* # bytes that variables take up.  */
  unsigned int gmask;		/* Mask of saved gp registers.  */
  unsigned int save_fp;		/* Nonzero if fp must be saved.  */
  unsigned int save_lr;		/* Nonzero if lr (return addr) must be saved.  */
  int          initialized;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by m32r_compute_frame_size.  */
static struct m32r_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct m32r_frame_info zero_frame_info;

#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK   (1 << (RETURN_ADDR_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.  */
#define MUST_SAVE_REGISTER(regno, interrupt_p) \
  ((regno) != RETURN_ADDR_REGNUM && (regno) != FRAME_POINTER_REGNUM \
   && (df_regs_ever_live_p (regno) && (!call_really_used_regs[regno] || interrupt_p)))

#define MUST_SAVE_FRAME_POINTER (df_regs_ever_live_p (FRAME_POINTER_REGNUM))
#define MUST_SAVE_RETURN_ADDR   (df_regs_ever_live_p (RETURN_ADDR_REGNUM) || crtl->profile)

#define SHORT_INSN_SIZE 2	/* Size of small instructions.  */
#define LONG_INSN_SIZE 4	/* Size of long instructions.  */

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

unsigned int
m32r_compute_frame_size (int size)	/* # of var. bytes allocated.  */
{
  unsigned int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size;
  unsigned int gmask;
  enum m32r_function_type fn_type;
  int interrupt_p;
  int pic_reg_used = flag_pic && (crtl->uses_pic_offset_table
                                  | crtl->profile);

  var_size	= M32R_STACK_ALIGN (size);
  args_size	= M32R_STACK_ALIGN (crtl->outgoing_args_size);
  pretend_size	= crtl->args.pretend_args_size;
  extra_size	= FIRST_PARM_OFFSET (0);
  total_size	= extra_size + pretend_size + args_size + var_size;
  reg_size	= 0;
  gmask		= 0;

  /* See if this is an interrupt handler.  Call used registers must be saved
     for them too.  */
  fn_type = m32r_compute_function_type (current_function_decl);
  interrupt_p = M32R_INTERRUPT_P (fn_type);

  /* Calculate space needed for registers.  */
  for (regno = 0; regno < M32R_MAX_INT_REGS; regno++)
    {
      if (MUST_SAVE_REGISTER (regno, interrupt_p)
          || (regno == PIC_OFFSET_TABLE_REGNUM && pic_reg_used))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
	}
    }

  current_frame_info.save_fp = MUST_SAVE_FRAME_POINTER;
  current_frame_info.save_lr = MUST_SAVE_RETURN_ADDR || pic_reg_used;

  reg_size += ((current_frame_info.save_fp + current_frame_info.save_lr)
	       * UNITS_PER_WORD);
  total_size += reg_size;

  /* ??? Not sure this is necessary, and I don't think the epilogue
     handler will do the right thing if this changes total_size.  */
  total_size = M32R_STACK_ALIGN (total_size);

  /* frame_size = total_size - (pretend_size + reg_size); */

  /* Save computed information.  */
  current_frame_info.total_size   = total_size;
  current_frame_info.extra_size   = extra_size;
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size	  = reg_size;
  current_frame_info.gmask	  = gmask;
  current_frame_info.initialized  = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

/* Worker function for TARGET_CAN_ELIMINATE.  */

bool
m32r_can_eliminate (const int from, const int to)
{
  return (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM
          ? ! frame_pointer_needed
          : true);
}


/* The table we use to reference PIC data.  */
static rtx global_offset_table;

static void
m32r_reload_lr (rtx sp, int size)
{
  rtx lr = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);

  if (size == 0)
    emit_insn (gen_movsi (lr, gen_frame_mem (Pmode, sp)));
  else if (size < 32768)
    emit_insn (gen_movsi (lr, gen_frame_mem (Pmode,
					     gen_rtx_PLUS (Pmode, sp,
							   GEN_INT (size)))));
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);

      emit_insn (gen_movsi (tmp, GEN_INT (size)));
      emit_insn (gen_addsi3 (tmp, tmp, sp));
      emit_insn (gen_movsi (lr, gen_frame_mem (Pmode, tmp)));
    }

  emit_use (lr);
}

void
m32r_load_pic_register (void)
{
  global_offset_table = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  emit_insn (gen_get_pc (pic_offset_table_rtx, global_offset_table,
                         GEN_INT (TARGET_MODEL_SMALL)));

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.  */
  emit_use (pic_offset_table_rtx);
}

/* Expand the m32r prologue as a series of insns.  */

void
m32r_expand_prologue (void)
{
  int regno;
  int frame_size;
  unsigned int gmask;
  int pic_reg_used = flag_pic && (crtl->uses_pic_offset_table
                                  | crtl->profile);

  if (! current_frame_info.initialized)
    m32r_compute_frame_size (get_frame_size ());

  gmask = current_frame_info.gmask;

  /* These cases shouldn't happen.  Catch them now.  */
  gcc_assert (current_frame_info.total_size || !gmask);

  /* Allocate space for register arguments if this is a variadic function.  */
  if (current_frame_info.pretend_size != 0)
    {
      /* Use a HOST_WIDE_INT temporary, since negating an unsigned int gives
	 the wrong result on a 64-bit host.  */
      HOST_WIDE_INT pretend_size = current_frame_info.pretend_size;
      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT (-pretend_size)));
    }

  /* Save any registers we need to and set up fp.  */
  if (current_frame_info.save_fp)
    emit_insn (gen_movsi_push (stack_pointer_rtx, frame_pointer_rtx));

  gmask &= ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK);

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler).  */
  for (regno = 0; regno <= M32R_MAX_INT_REGS; ++regno)
    {
      if ((gmask & (1 << regno)) != 0)
	emit_insn (gen_movsi_push (stack_pointer_rtx,
				   gen_rtx_REG (Pmode, regno)));
    }

  if (current_frame_info.save_lr)
    emit_insn (gen_movsi_push (stack_pointer_rtx,
			       gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)));

  /* Allocate the stack frame.  */
  frame_size = (current_frame_info.total_size
		- (current_frame_info.pretend_size
		   + current_frame_info.reg_size));

  if (frame_size == 0)
    ; /* Nothing to do.  */
  else if (frame_size <= 32768)
    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   GEN_INT (-frame_size)));
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);

      emit_insn (gen_movsi (tmp, GEN_INT (frame_size)));
      emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, tmp));
    }

  if (frame_pointer_needed)
    emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));

  if (crtl->profile)
    /* Push lr for mcount (form_pc, x).  */
    emit_insn (gen_movsi_push (stack_pointer_rtx,
                               gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)));

  if (pic_reg_used)
    {
      m32r_load_pic_register ();
      m32r_reload_lr (stack_pointer_rtx,
                      (crtl->profile ? 0 : frame_size));
    }

  if (crtl->profile && !pic_reg_used)
    emit_insn (gen_blockage ());
}


/* Set up the stack and frame pointer (if desired) for the function.
   Note, if this is changed, you need to mirror the changes in
   m32r_compute_frame_size which calculates the prolog size.  */

static void
m32r_output_function_prologue (FILE * file, HOST_WIDE_INT size)
{
  enum m32r_function_type fn_type = m32r_compute_function_type (current_function_decl);

  /* If this is an interrupt handler, mark it as such.  */
  if (M32R_INTERRUPT_P (fn_type))
    fprintf (file, "\t%s interrupt handler\n", ASM_COMMENT_START);

  if (! current_frame_info.initialized)
    m32r_compute_frame_size (size);

  /* This is only for the human reader.  */
  fprintf (file,
	   "\t%s PROLOGUE, vars= %d, regs= %d, args= %d, extra= %d\n",
	   ASM_COMMENT_START,
	   current_frame_info.var_size,
	   current_frame_info.reg_size / 4,
	   current_frame_info.args_size,
	   current_frame_info.extra_size);
}

/* Output RTL to pop register REGNO from the stack.  */

static void
pop (int regno)
{
  rtx x;

  x = emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, regno),
				stack_pointer_rtx));
  add_reg_note (x, REG_INC, stack_pointer_rtx);
}

/* Expand the m32r epilogue as a series of insns.  */

void
m32r_expand_epilogue (void)
{
  int regno;
  int noepilogue = FALSE;
  int total_size;

  gcc_assert (current_frame_info.initialized);
  total_size = current_frame_info.total_size;

  if (total_size == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (insn && NOTE_P (insn))
	insn = prev_nonnote_insn (insn);
      if (insn && BARRIER_P (insn))
	noepilogue = TRUE;
    }

  if (!noepilogue)
    {
      unsigned int var_size = current_frame_info.var_size;
      unsigned int args_size = current_frame_info.args_size;
      unsigned int gmask = current_frame_info.gmask;
      int can_trust_sp_p = !cfun->calls_alloca;

      if (flag_exceptions)
        emit_insn (gen_blockage ());

      /* The first thing to do is point the sp at the bottom of the register
	 save area.  */
      if (can_trust_sp_p)
	{
	  unsigned int reg_offset = var_size + args_size;

	  if (reg_offset == 0)
	    ; /* Nothing to do.  */
	  else if (reg_offset < 32768)
	    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   GEN_INT (reg_offset)));
	  else
	    {
	      rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);

	      emit_insn (gen_movsi (tmp, GEN_INT (reg_offset)));
	      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				     tmp));
	    }
	}
      else if (frame_pointer_needed)
	{
	  unsigned int reg_offset = var_size + args_size;

	  if (reg_offset == 0)
	    emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
	  else if (reg_offset < 32768)
	    emit_insn (gen_addsi3 (stack_pointer_rtx, frame_pointer_rtx,
			   GEN_INT (reg_offset)));
	  else
	    {
	      rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);

	      emit_insn (gen_movsi (tmp, GEN_INT (reg_offset)));
	      emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
	      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				     tmp));
	    }
	}
      else
	gcc_unreachable ();

      if (current_frame_info.save_lr)
	pop (RETURN_ADDR_REGNUM);

      /* Restore any saved registers, in reverse order of course.  */
      gmask &= ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK);
      for (regno = M32R_MAX_INT_REGS - 1; regno >= 0; --regno)
	{
	  if ((gmask & (1L << regno)) != 0)
	    pop (regno);
	}

      if (current_frame_info.save_fp)
	pop (FRAME_POINTER_REGNUM);

      /* Remove varargs area if present.  */
      if (current_frame_info.pretend_size != 0)
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (current_frame_info.pretend_size)));

      emit_insn (gen_blockage ());
    }
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */

static void
m32r_output_function_epilogue (FILE * file ATTRIBUTE_UNUSED,
			       HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
  m32r_compute_function_type (NULL_TREE);
}

/* Return nonzero if this function is known to have a null or 1 instruction
   epilogue.  */

int
direct_return (void)
{
  if (!reload_completed)
    return FALSE;

  if (M32R_INTERRUPT_P (m32r_compute_function_type (current_function_decl)))
    return FALSE;

  if (! current_frame_info.initialized)
    m32r_compute_frame_size (get_frame_size ());

  return current_frame_info.total_size == 0;
}


/* PIC.  */

int
m32r_legitimate_pic_operand_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    return 0;

  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
          || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
      && (CONST_INT_P (XEXP (XEXP (x, 0), 1))))
    return 0;

  return 1;
}

rtx
m32r_legitimize_pic_address (rtx orig, rtx reg)
{
#ifdef DEBUG_PIC
  printf("m32r_legitimize_pic_address()\n");
#endif

  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      rtx pic_ref, address;
      int subregs = 0;

      if (reg == 0)
        {
          gcc_assert (!reload_in_progress && !reload_completed);
	  reg = gen_reg_rtx (Pmode);

          subregs = 1;
        }

      if (subregs)
        address = gen_reg_rtx (Pmode);
      else
        address = reg;

      crtl->uses_pic_offset_table = 1;

      if (GET_CODE (orig) == LABEL_REF
          || (GET_CODE (orig) == SYMBOL_REF && SYMBOL_REF_LOCAL_P (orig)))
        {
          emit_insn (gen_gotoff_load_addr (reg, orig));
          emit_insn (gen_addsi3 (reg, reg, pic_offset_table_rtx));
          return reg;
        }

      emit_insn (gen_pic_load_addr (address, orig));

      emit_insn (gen_addsi3 (address, address, pic_offset_table_rtx));
      pic_ref = gen_const_mem (Pmode, address);
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
          && XEXP (XEXP (orig, 0), 1) == pic_offset_table_rtx)
        return orig;

      if (reg == 0)
        {
          gcc_assert (!reload_in_progress && !reload_completed);
	  reg = gen_reg_rtx (Pmode);
        }

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
        {
          base = m32r_legitimize_pic_address (XEXP (XEXP (orig, 0), 0), reg);
          if (base == reg)
            offset = m32r_legitimize_pic_address (XEXP (XEXP (orig, 0), 1), NULL_RTX);
          else
            offset = m32r_legitimize_pic_address (XEXP (XEXP (orig, 0), 1), reg);
        }
      else
        return orig;

      if (CONST_INT_P (offset))
        {
          if (INT16_P (INTVAL (offset)))
            return plus_constant (Pmode, base, INTVAL (offset));
          else
	    {
	      gcc_assert (! reload_in_progress && ! reload_completed);
	      offset = force_reg (Pmode, offset);
	    }
        }

      return gen_rtx_PLUS (Pmode, base, offset);
    }

  return orig;
}

static rtx
m32r_legitimize_address (rtx x, rtx orig_x ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (flag_pic)
    return m32r_legitimize_pic_address (x, NULL_RTX);
  else
    return x;
}

/* Worker function for TARGET_MODE_DEPENDENT_ADDRESS_P.  */

static bool
m32r_mode_dependent_address_p (const_rtx addr, addr_space_t as ATTRIBUTE_UNUSED)
{
  if (GET_CODE (addr) == LO_SUM)
    return true;

  return false;
}

/* Nested function support.  */

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void
m32r_initialize_trampoline (rtx tramp ATTRIBUTE_UNUSED,
			    rtx fnaddr ATTRIBUTE_UNUSED,
			    rtx cxt ATTRIBUTE_UNUSED)
{
}

static void
m32r_file_start (void)
{
  default_file_start ();

  if (flag_verbose_asm)
    fprintf (asm_out_file,
	     "%s M32R/D special options: -G %d\n",
	     ASM_COMMENT_START, g_switch_value);

  if (TARGET_LITTLE_ENDIAN)
    fprintf (asm_out_file, "\t.little\n");
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

static void
m32r_print_operand (FILE * file, rtx x, int code)
{
  rtx addr;

  switch (code)
    {
      /* The 's' and 'p' codes are used by output_block_move() to
	 indicate post-increment 's'tores and 'p're-increment loads.  */
    case 's':
      if (REG_P (x))
	fprintf (file, "@+%s", reg_names [REGNO (x)]);
      else
	output_operand_lossage ("invalid operand to %%s code");
      return;

    case 'p':
      if (REG_P (x))
	fprintf (file, "@%s+", reg_names [REGNO (x)]);
      else
	output_operand_lossage ("invalid operand to %%p code");
      return;

    case 'R' :
      /* Write second word of DImode or DFmode reference,
	 register or memory.  */
      if (REG_P (x))
	fputs (reg_names[REGNO (x)+1], file);
      else if (MEM_P (x))
	{
	  fprintf (file, "@(");
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of four.  */
	  /* ??? This is taken from rs6000.c I think.  I don't think it is
	     currently necessary, but keep it around.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 4));
	  else
	    output_address (plus_constant (Pmode, XEXP (x, 0), 4));
	  fputc (')', file);
	}
      else
	output_operand_lossage ("invalid operand to %%R code");
      return;

    case 'H' : /* High word.  */
    case 'L' : /* Low word.  */
      if (REG_P (x))
	{
	  /* L = least significant word, H = most significant word.  */
	  if ((WORDS_BIG_ENDIAN != 0) ^ (code == 'L'))
	    fputs (reg_names[REGNO (x)], file);
	  else
	    fputs (reg_names[REGNO (x)+1], file);
	}
      else if (CONST_INT_P (x)
	       || GET_CODE (x) == CONST_DOUBLE)
	{
	  rtx first, second;

	  split_double (x, &first, &second);
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		   code == 'L' ? INTVAL (first) : INTVAL (second));
	}
      else
	output_operand_lossage ("invalid operand to %%H/%%L code");
      return;

    case 'A' :
      {
	char str[30];

	if (GET_CODE (x) != CONST_DOUBLE
	    || GET_MODE_CLASS (GET_MODE (x)) != MODE_FLOAT)
	  fatal_insn ("bad insn for 'A'", x);

	real_to_decimal (str, CONST_DOUBLE_REAL_VALUE (x), sizeof (str), 0, 1);
	fprintf (file, "%s", str);
	return;
      }

    case 'B' : /* Bottom half.  */
    case 'T' : /* Top half.  */
      /* Output the argument to a `seth' insn (sets the Top half-word).
	 For constants output arguments to a seth/or3 pair to set Top and
	 Bottom halves.  For symbols output arguments to a seth/add3 pair to
	 set Top and Bottom halves.  The difference exists because for
	 constants seth/or3 is more readable but for symbols we need to use
	 the same scheme as `ld' and `st' insns (16-bit addend is signed).  */
      switch (GET_CODE (x))
	{
	case CONST_INT :
	case CONST_DOUBLE :
	  {
	    rtx first, second;

	    split_double (x, &first, &second);
	    x = WORDS_BIG_ENDIAN ? second : first;
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		     (code == 'B'
		      ? INTVAL (x) & 0xffff
		      : (INTVAL (x) >> 16) & 0xffff));
	  }
	  return;
	case CONST :
	case SYMBOL_REF :
	  if (code == 'B'
	      && small_data_operand (x, VOIDmode))
	    {
	      fputs ("sda(", file);
	      output_addr_const (file, x);
	      fputc (')', file);
	      return;
	    }
	  /* fall through */
	case LABEL_REF :
	  fputs (code == 'T' ? "shigh(" : "low(", file);
	  output_addr_const (file, x);
	  fputc (')', file);
	  return;
	default :
	  output_operand_lossage ("invalid operand to %%T/%%B code");
	  return;
	}
      break;

    case 'U' :
      /* ??? wip */
      /* Output a load/store with update indicator if appropriate.  */
      if (MEM_P (x))
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fputs (".a", file);
	}
      else
	output_operand_lossage ("invalid operand to %%U code");
      return;

    case 'N' :
      /* Print a constant value negated.  */
      if (CONST_INT_P (x))
	output_addr_const (file, GEN_INT (- INTVAL (x)));
      else
	output_operand_lossage ("invalid operand to %%N code");
      return;

    case 'X' :
      /* Print a const_int in hex.  Used in comments.  */
      if (CONST_INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case '#' :
      fputs (IMMEDIATE_PREFIX, file);
      return;

    case 0 :
      /* Do nothing special.  */
      break;

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
      addr = XEXP (x, 0);
      if (GET_CODE (addr) == PRE_INC)
	{
	  if (!REG_P (XEXP (addr, 0)))
	    fatal_insn ("pre-increment address is not a register", x);

	  fprintf (file, "@+%s", reg_names[REGNO (XEXP (addr, 0))]);
	}
      else if (GET_CODE (addr) == PRE_DEC)
	{
	  if (!REG_P (XEXP (addr, 0)))
	    fatal_insn ("pre-decrement address is not a register", x);

	  fprintf (file, "@-%s", reg_names[REGNO (XEXP (addr, 0))]);
	}
      else if (GET_CODE (addr) == POST_INC)
	{
	  if (!REG_P (XEXP (addr, 0)))
	    fatal_insn ("post-increment address is not a register", x);

	  fprintf (file, "@%s+", reg_names[REGNO (XEXP (addr, 0))]);
	}
      else
	{
	  fputs ("@(", file);
	  output_address (XEXP (x, 0));
	  fputc (')', file);
	}
      break;

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
      output_addr_const (file, x);
      break;
    }
}

/* Print a memory address as an operand to reference that memory location.  */

static void
m32r_print_operand_address (FILE * file, rtx addr)
{
  rtx base;
  rtx index = 0;
  int offset = 0;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;

    case PLUS :
      if (CONST_INT_P (XEXP (addr, 0)))
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);
      else if (CONST_INT_P (XEXP (addr, 1)))
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);
      if (REG_P (base))
	{
	  /* Print the offset first (if present) to conform to the manual.  */
	  if (index == 0)
	    {
	      if (offset != 0)
		fprintf (file, "%d,", offset);
	      fputs (reg_names[REGNO (base)], file);
	    }
	  /* The chip doesn't support this, but left in for generality.  */
	  else if (REG_P (index))
	    fprintf (file, "%s,%s",
		     reg_names[REGNO (base)], reg_names[REGNO (index)]);
	  /* Not sure this can happen, but leave in for now.  */
	  else if (GET_CODE (index) == SYMBOL_REF)
	    {
	      output_addr_const (file, index);
	      fputc (',', file);
	      fputs (reg_names[REGNO (base)], file);
	    }
	  else
	    fatal_insn ("bad address", addr);
	}
      else if (GET_CODE (base) == LO_SUM)
	{
	  gcc_assert (!index && REG_P (XEXP (base, 0)));
	  if (small_data_operand (XEXP (base, 1), VOIDmode))
	    fputs ("sda(", file);
	  else
	    fputs ("low(", file);
	  output_addr_const (file, plus_constant (Pmode, XEXP (base, 1),
						  offset));
	  fputs ("),", file);
	  fputs (reg_names[REGNO (XEXP (base, 0))], file);
	}
      else
	fatal_insn ("bad address", addr);
      break;

    case LO_SUM :
      if (!REG_P (XEXP (addr, 0)))
	fatal_insn ("lo_sum not of register", addr);
      if (small_data_operand (XEXP (addr, 1), VOIDmode))
	fputs ("sda(", file);
      else
	fputs ("low(", file);
      output_addr_const (file, XEXP (addr, 1));
      fputs ("),", file);
      fputs (reg_names[REGNO (XEXP (addr, 0))], file);
      break;

    case PRE_INC :	/* Assume SImode.  */
      fprintf (file, "+%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PRE_DEC :	/* Assume SImode.  */
      fprintf (file, "-%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC :	/* Assume SImode.  */
      fprintf (file, "%s+", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    default :
      output_addr_const (file, addr);
      break;
    }
}

static bool
m32r_print_operand_punct_valid_p (unsigned char code)
{
  return m32r_punct_chars[code];
}

/* Return true if the operands are the constants 0 and 1.  */

int
zero_and_one (rtx operand1, rtx operand2)
{
  return
       CONST_INT_P (operand1)
    && CONST_INT_P (operand2)
    && (  ((INTVAL (operand1) == 0) && (INTVAL (operand2) == 1))
	||((INTVAL (operand1) == 1) && (INTVAL (operand2) == 0)));
}

/* Generate the correct assembler code to handle the conditional loading of a
   value into a register.  It is known that the operands satisfy the
   conditional_move_operand() function above.  The destination is operand[0].
   The condition is operand [1].  The 'true' value is operand [2] and the
   'false' value is operand [3].  */

char *
emit_cond_move (rtx * operands, rtx insn ATTRIBUTE_UNUSED)
{
  static char buffer [100];
  const char * dest = reg_names [REGNO (operands [0])];

  buffer [0] = 0;

  /* Destination must be a register.  */
  gcc_assert (REG_P (operands [0]));
  gcc_assert (conditional_move_operand (operands [2], SImode));
  gcc_assert (conditional_move_operand (operands [3], SImode));

  /* Check to see if the test is reversed.  */
  if (GET_CODE (operands [1]) == NE)
    {
      rtx tmp = operands [2];
      operands [2] = operands [3];
      operands [3] = tmp;
    }

  sprintf (buffer, "mvfc %s, cbr", dest);

  /* If the true value was '0' then we need to invert the results of the move.  */
  if (INTVAL (operands [2]) == 0)
    sprintf (buffer + strlen (buffer), "\n\txor3 %s, %s, #1",
	     dest, dest);

  return buffer;
}

/* Returns true if the registers contained in the two
   rtl expressions are different.  */

int
m32r_not_same_reg (rtx a, rtx b)
{
  int reg_a = -1;
  int reg_b = -2;

  while (GET_CODE (a) == SUBREG)
    a = SUBREG_REG (a);

  if (REG_P (a))
    reg_a = REGNO (a);

  while (GET_CODE (b) == SUBREG)
    b = SUBREG_REG (b);

  if (REG_P (b))
    reg_b = REGNO (b);

  return reg_a != reg_b;
}


rtx
m32r_function_symbol (const char *name)
{
  int extra_flags = 0;
  enum m32r_model model;
  rtx sym = gen_rtx_SYMBOL_REF (Pmode, name);

  if (TARGET_MODEL_SMALL)
    model = M32R_MODEL_SMALL;
  else if (TARGET_MODEL_MEDIUM)
    model = M32R_MODEL_MEDIUM;
  else if (TARGET_MODEL_LARGE)
    model = M32R_MODEL_LARGE;
  else
    gcc_unreachable (); /* Shouldn't happen.  */
  extra_flags |= model << SYMBOL_FLAG_MODEL_SHIFT;

  if (extra_flags)
    SYMBOL_REF_FLAGS (sym) |= extra_flags;

  return sym;
}

/* Use a library function to move some bytes.  */

static void
block_move_call (rtx dest_reg, rtx src_reg, rtx bytes_rtx)
{
  /* We want to pass the size as Pmode, which will normally be SImode
     but will be DImode if we are using 64-bit longs and pointers.  */
  if (GET_MODE (bytes_rtx) != VOIDmode
      && GET_MODE (bytes_rtx) != Pmode)
    bytes_rtx = convert_to_mode (Pmode, bytes_rtx, 1);

  emit_library_call (m32r_function_symbol ("memcpy"), LCT_NORMAL,
		     VOIDmode, 3, dest_reg, Pmode, src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype), bytes_rtx,
				      TYPE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
}

/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.

   Returns 1 upon success, 0 otherwise.  */

int
m32r_expand_block_move (rtx operands[])
{
  rtx           orig_dst  = operands[0];
  rtx           orig_src  = operands[1];
  rtx           bytes_rtx = operands[2];
  rtx           align_rtx = operands[3];
  int           constp    = CONST_INT_P (bytes_rtx);
  HOST_WIDE_INT bytes     = constp ? INTVAL (bytes_rtx) : 0;
  int           align     = INTVAL (align_rtx);
  int           leftover;
  rtx           src_reg;
  rtx           dst_reg;

  if (constp && bytes <= 0)
    return 1;

  /* Move the address into scratch registers.  */
  dst_reg = copy_addr_to_reg (XEXP (orig_dst, 0));
  src_reg = copy_addr_to_reg (XEXP (orig_src, 0));

  if (align > UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* If we prefer size over speed, always use a function call.
     If we do not know the size, use a function call.
     If the blocks are not word aligned, use a function call.  */
  if (optimize_size || ! constp || align != UNITS_PER_WORD)
    {
      block_move_call (dst_reg, src_reg, bytes_rtx);
      return 0;
    }

  leftover = bytes % MAX_MOVE_BYTES;
  bytes   -= leftover;

  /* If necessary, generate a loop to handle the bulk of the copy.  */
  if (bytes)
    {
      rtx label = NULL_RTX;
      rtx final_src = NULL_RTX;
      rtx at_a_time = GEN_INT (MAX_MOVE_BYTES);
      rtx rounded_total = GEN_INT (bytes);
      rtx new_dst_reg = gen_reg_rtx (SImode);
      rtx new_src_reg = gen_reg_rtx (SImode);

      /* If we are going to have to perform this loop more than
	 once, then generate a label and compute the address the
	 source register will contain upon completion of the final
	 iteration.  */
      if (bytes > MAX_MOVE_BYTES)
	{
	  final_src = gen_reg_rtx (Pmode);

	  if (INT16_P(bytes))
	    emit_insn (gen_addsi3 (final_src, src_reg, rounded_total));
	  else
	    {
	      emit_insn (gen_movsi (final_src, rounded_total));
	      emit_insn (gen_addsi3 (final_src, final_src, src_reg));
	    }

	  label = gen_label_rtx ();
	  emit_label (label);
	}

      /* It is known that output_block_move() will update src_reg to point
	 to the word after the end of the source block, and dst_reg to point
	 to the last word of the destination block, provided that the block
	 is MAX_MOVE_BYTES long.  */
      emit_insn (gen_movmemsi_internal (dst_reg, src_reg, at_a_time,
					new_dst_reg, new_src_reg));
      emit_move_insn (dst_reg, new_dst_reg);
      emit_move_insn (src_reg, new_src_reg);
      emit_insn (gen_addsi3 (dst_reg, dst_reg, GEN_INT (4)));

      if (bytes > MAX_MOVE_BYTES)
	{
	  rtx test = gen_rtx_NE (VOIDmode, src_reg, final_src);
	  emit_jump_insn (gen_cbranchsi4 (test, src_reg, final_src, label));
	}
    }

  if (leftover)
    emit_insn (gen_movmemsi_internal (dst_reg, src_reg, GEN_INT (leftover),
				      gen_reg_rtx (SImode),
				      gen_reg_rtx (SImode)));
  return 1;
}


/* Emit load/stores for a small constant word aligned block_move.

   operands[0] is the memory address of the destination.
   operands[1] is the memory address of the source.
   operands[2] is the number of bytes to move.
   operands[3] is a temp register.
   operands[4] is a temp register.  */

void
m32r_output_block_move (rtx insn ATTRIBUTE_UNUSED, rtx operands[])
{
  HOST_WIDE_INT bytes = INTVAL (operands[2]);
  int		first_time;
  int		got_extra = 0;

  gcc_assert (bytes >= 1 && bytes <= MAX_MOVE_BYTES);

  /* We do not have a post-increment store available, so the first set of
     stores are done without any increment, then the remaining ones can use
     the pre-increment addressing mode.

     Note: expand_block_move() also relies upon this behavior when building
     loops to copy large blocks.  */
  first_time = 1;

  while (bytes > 0)
    {
      if (bytes >= 8)
	{
	  if (first_time)
	    {
	      output_asm_insn ("ld\t%5, %p1", operands);
	      output_asm_insn ("ld\t%6, %p1", operands);
	      output_asm_insn ("st\t%5, @%0", operands);
	      output_asm_insn ("st\t%6, %s0", operands);
	    }
	  else
	    {
	      output_asm_insn ("ld\t%5, %p1", operands);
	      output_asm_insn ("ld\t%6, %p1", operands);
	      output_asm_insn ("st\t%5, %s0", operands);
	      output_asm_insn ("st\t%6, %s0", operands);
	    }

	  bytes -= 8;
	}
      else if (bytes >= 4)
	{
	  if (bytes > 4)
	    got_extra = 1;

	  output_asm_insn ("ld\t%5, %p1", operands);

	  if (got_extra)
	    output_asm_insn ("ld\t%6, %p1", operands);

	  if (first_time)
	    output_asm_insn ("st\t%5, @%0", operands);
	  else
	    output_asm_insn ("st\t%5, %s0", operands);

	  bytes -= 4;
	}
      else
	{
	  /* Get the entire next word, even though we do not want all of it.
	     The saves us from doing several smaller loads, and we assume that
	     we cannot cause a page fault when at least part of the word is in
	     valid memory [since we don't get called if things aren't properly
	     aligned].  */
	  int dst_offset = first_time ? 0 : 4;
	  /* The amount of increment we have to make to the
	     destination pointer.  */
	  int dst_inc_amount = dst_offset + bytes - 4;
	  /* The same for the source pointer.  */
	  int src_inc_amount = bytes;
	  int last_shift;
	  rtx my_operands[3];

	  /* If got_extra is true then we have already loaded
	     the next word as part of loading and storing the previous word.  */
	  if (! got_extra)
	    output_asm_insn ("ld\t%6, @%1", operands);

	  if (bytes >= 2)
	    {
	      bytes -= 2;

	      output_asm_insn ("sra3\t%5, %6, #16", operands);
	      my_operands[0] = operands[5];
	      my_operands[1] = GEN_INT (dst_offset);
	      my_operands[2] = operands[0];
	      output_asm_insn ("sth\t%0, @(%1,%2)", my_operands);

	      /* If there is a byte left to store then increment the
		 destination address and shift the contents of the source
		 register down by 8 bits.  We could not do the address
		 increment in the store half word instruction, because it does
		 not have an auto increment mode.  */
	      if (bytes > 0)  /* assert (bytes == 1) */
		{
		  dst_offset += 2;
		  last_shift = 8;
		}
	    }
	  else
	    last_shift = 24;

	  if (bytes > 0)
	    {
	      my_operands[0] = operands[6];
	      my_operands[1] = GEN_INT (last_shift);
	      output_asm_insn ("srai\t%0, #%1", my_operands);
	      my_operands[0] = operands[6];
	      my_operands[1] = GEN_INT (dst_offset);
	      my_operands[2] = operands[0];
	      output_asm_insn ("stb\t%0, @(%1,%2)", my_operands);
	    }

	  /* Update the destination pointer if needed.  We have to do
	     this so that the patterns matches what we output in this
	     function.  */
	  if (dst_inc_amount
	      && !find_reg_note (insn, REG_UNUSED, operands[0]))
	    {
	      my_operands[0] = operands[0];
	      my_operands[1] = GEN_INT (dst_inc_amount);
	      output_asm_insn ("addi\t%0, #%1", my_operands);
	    }

	  /* Update the source pointer if needed.  We have to do this
	     so that the patterns matches what we output in this
	     function.  */
	  if (src_inc_amount
	      && !find_reg_note (insn, REG_UNUSED, operands[1]))
	    {
	      my_operands[0] = operands[1];
	      my_operands[1] = GEN_INT (src_inc_amount);
	      output_asm_insn ("addi\t%0, #%1", my_operands);
	    }

	  bytes = 0;
	}

      first_time = 0;
    }
}

/* Return true if using NEW_REG in place of OLD_REG is ok.  */

int
m32r_hard_regno_rename_ok (unsigned int old_reg ATTRIBUTE_UNUSED,
			   unsigned int new_reg)
{
  /* Interrupt routines can't clobber any register that isn't already used.  */
  if (lookup_attribute ("interrupt", DECL_ATTRIBUTES (current_function_decl))
      && !df_regs_ever_live_p (new_reg))
    return 0;

  return 1;
}

rtx
m32r_return_addr (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);
}

static void
m32r_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  emit_move_insn (adjust_address (m_tramp, SImode, 0),
		  gen_int_mode (TARGET_LITTLE_ENDIAN ?
				0x017e8e17 : 0x178e7e01, SImode));
  emit_move_insn (adjust_address (m_tramp, SImode, 4),
		  gen_int_mode (TARGET_LITTLE_ENDIAN ?
				0x0c00ae86 : 0x86ae000c, SImode));
  emit_move_insn (adjust_address (m_tramp, SImode, 8),
		  gen_int_mode (TARGET_LITTLE_ENDIAN ?
				0xe627871e : 0x1e8727e6, SImode));
  emit_move_insn (adjust_address (m_tramp, SImode, 12),
		  gen_int_mode (TARGET_LITTLE_ENDIAN ?
				0xc616c626 : 0x26c61fc6, SImode));
  emit_move_insn (adjust_address (m_tramp, SImode, 16),
		  chain_value);
  emit_move_insn (adjust_address (m_tramp, SImode, 20),
		  XEXP (DECL_RTL (fndecl), 0));

  if (m32r_cache_flush_trap >= 0)
    emit_insn (gen_flush_icache
	       (validize_mem (adjust_address (m_tramp, SImode, 0)),
		gen_int_mode (m32r_cache_flush_trap, SImode)));
  else if (m32r_cache_flush_func && m32r_cache_flush_func[0])
    emit_library_call (m32r_function_symbol (m32r_cache_flush_func),
		       LCT_NORMAL, VOIDmode, 3, XEXP (m_tramp, 0), Pmode,
		       gen_int_mode (TRAMPOLINE_SIZE, SImode), SImode,
		       GEN_INT (3), SImode);
}

/* True if X is a reg that can be used as a base reg.  */

static bool
m32r_rtx_ok_for_base_p (const_rtx x, bool strict)
{
  if (! REG_P (x))
    return false;

  if (strict)
    {
      if (GPR_P (REGNO (x)))
	return true;
    }
  else
    {
      if (GPR_P (REGNO (x))
	  || REGNO (x) == ARG_POINTER_REGNUM
	  || ! HARD_REGISTER_P (x))
	return true;
    }

  return false;
}

static inline bool
m32r_rtx_ok_for_offset_p (const_rtx x)
{
  return (CONST_INT_P (x) && INT16_P (INTVAL (x)));
}

static inline bool
m32r_legitimate_offset_addres_p (enum machine_mode mode ATTRIBUTE_UNUSED,
				 const_rtx x, bool strict)
{
  if (GET_CODE (x) == PLUS
      && m32r_rtx_ok_for_base_p (XEXP (x, 0), strict)
      && m32r_rtx_ok_for_offset_p (XEXP (x, 1)))
    return true;

  return false;
}

/* For LO_SUM addresses, do not allow them if the MODE is > 1 word,
   since more than one instruction will be required.  */

static inline bool
m32r_legitimate_lo_sum_addres_p (enum machine_mode mode, const_rtx x,
				 bool strict)
{
  if (GET_CODE (x) == LO_SUM
      && (mode != BLKmode && GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
      && m32r_rtx_ok_for_base_p (XEXP (x, 0), strict)
      && CONSTANT_P (XEXP (x, 1)))
    return true;

  return false;
}

/* Is this a load and increment operation.  */

static inline bool
m32r_load_postinc_p (enum machine_mode mode, const_rtx x, bool strict)
{
  if ((mode == SImode || mode == SFmode)
      && GET_CODE (x) == POST_INC
      && REG_P (XEXP (x, 0))
      && m32r_rtx_ok_for_base_p (XEXP (x, 0), strict))
    return true;

  return false;
}

/* Is this an increment/decrement and store operation.  */

static inline bool
m32r_store_preinc_predec_p (enum machine_mode mode, const_rtx x, bool strict)
{
  if ((mode == SImode || mode == SFmode)
      && (GET_CODE (x) == PRE_INC || GET_CODE (x) == PRE_DEC)
      && REG_P (XEXP (x, 0))                           \
      && m32r_rtx_ok_for_base_p (XEXP (x, 0), strict))
    return true;

  return false;
}

/* Implement  TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
m32r_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  if (m32r_rtx_ok_for_base_p (x, strict)
      || m32r_legitimate_offset_addres_p (mode, x, strict)
      || m32r_legitimate_lo_sum_addres_p (mode, x, strict)
      || m32r_load_postinc_p (mode, x, strict)
      || m32r_store_preinc_predec_p (mode, x, strict))
    return true;

  return false;
}

static void
m32r_conditional_register_usage (void)
{
  if (flag_pic)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P

   We don't allow (plus symbol large-constant) as the relocations can't
   describe it.  INTVAL > 32767 handles both 16-bit and 24-bit relocations.
   We allow all CONST_DOUBLE's as the md file patterns will force the
   constant to memory if they can't handle them.  */

static bool
m32r_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return !(GET_CODE (x) == CONST
	   && GET_CODE (XEXP (x, 0)) == PLUS
	   && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && UINTVAL (XEXP (XEXP (x, 0), 1)) > 32767);
}
