/* Subroutines used for code generation on the Mitsubishi M32R cpu.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

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
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "function.h"
#include "recog.h"
#include "toplev.h"
#include "ggc.h"
#include "m32r-protos.h"
#include "target.h"
#include "target-def.h"

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */
rtx m32r_compare_op0, m32r_compare_op1;

/* Array of valid operand punctuation characters.  */
char m32r_punct_chars[256];

/* Selected code model.  */
const char * m32r_model_string = M32R_MODEL_DEFAULT;
enum m32r_model m32r_model;

/* Selected SDA support.  */
const char * m32r_sdata_string = M32R_SDATA_DEFAULT;
enum m32r_sdata m32r_sdata;

/* Scheduler support */
static int m32r_sched_odd_word_p;

/* Forward declaration.  */
static void  init_reg_tables			PARAMS ((void));
static void  block_move_call			PARAMS ((rtx, rtx, rtx));
static int   m32r_is_insn			PARAMS ((rtx));
const struct attribute_spec m32r_attribute_table[];
static tree  m32r_handle_model_attribute PARAMS ((tree *, tree, tree, int, bool *));
static void  m32r_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void  m32r_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

static int    m32r_adjust_cost 	   PARAMS ((rtx, rtx, rtx, int));
static int    m32r_adjust_priority PARAMS ((rtx, int));
static void   m32r_sched_init	   PARAMS ((FILE *, int, int));
static int    m32r_sched_reorder   PARAMS ((FILE *, int, rtx *, int *, int));
static int    m32r_variable_issue  PARAMS ((FILE *, int, rtx, int));
static int    m32r_issue_rate	   PARAMS ((void));


/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE m32r_attribute_table

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE m32r_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE m32r_output_function_epilogue

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST m32r_adjust_cost
#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY m32r_adjust_priority
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE m32r_issue_rate
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE m32r_variable_issue
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT m32r_sched_init
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER m32r_sched_reorder

struct gcc_target targetm = TARGET_INITIALIZER;

/* Called by OVERRIDE_OPTIONS to initialize various things.  */

void
m32r_init ()
{
  init_reg_tables ();

  /* Initialize array for PRINT_OPERAND_PUNCT_VALID_P.  */
  memset (m32r_punct_chars, 0, sizeof (m32r_punct_chars));
  m32r_punct_chars['#'] = 1;
  m32r_punct_chars['@'] = 1; /* ??? no longer used */

  /* Provide default value if not specified.  */
  if (!g_switch_set)
    g_switch_value = SDATA_DEFAULT_SIZE;

  if (strcmp (m32r_model_string, "small") == 0)
    m32r_model = M32R_MODEL_SMALL;
  else if (strcmp (m32r_model_string, "medium") == 0)
    m32r_model = M32R_MODEL_MEDIUM;
  else if (strcmp (m32r_model_string, "large") == 0)
    m32r_model = M32R_MODEL_LARGE;
  else
    error ("bad value (%s) for -mmodel switch", m32r_model_string);

  if (strcmp (m32r_sdata_string, "none") == 0)
    m32r_sdata = M32R_SDATA_NONE;
  else if (strcmp (m32r_sdata_string, "sdata") == 0)
    m32r_sdata = M32R_SDATA_SDATA;
  else if (strcmp (m32r_sdata_string, "use") == 0)
    m32r_sdata = M32R_SDATA_USE;
  else
    error ("bad value (%s) for -msdata switch", m32r_sdata_string);
}

/* Vectors to keep interesting information about registers where it can easily
   be got.  We use to use the actual mode value as the bit number, but there
   is (or may be) more than 32 modes now.  Instead we use two tables: one
   indexed by hard register number, and one indexed by mode.  */

/* The purpose of m32r_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32 bit word (again).  Each real mode is
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

unsigned int m32r_hard_regno_mode_ok[FIRST_PSEUDO_REGISTER] =
{
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, C_MODES, A_MODES, A_MODES
};

unsigned int m32r_mode_class [NUM_MACHINE_MODES];

enum reg_class m32r_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
init_reg_tables ()
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
	    m32r_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    m32r_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    m32r_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    m32r_mode_class[i] = 1 << (int) O_MODE;
	  else 
	    m32r_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (i) <= 4)
	    m32r_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    m32r_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    m32r_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    m32r_mode_class[i] = 1 << (int) OF_MODE;
	  else 
	    m32r_mode_class[i] = 0;
	  break;
	case MODE_CC:
	default:
	  /* mode_class hasn't been initialized yet for EXTRA_CC_MODES, so
	     we must explicitly check for them here.  */
	  if (i == (int) CCmode)
	    m32r_mode_class[i] = 1 << (int) C_MODE;
	  else
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

	Grep for MODEL in m32r.h for more info.
*/

static tree small_ident1;
static tree small_ident2;
static tree medium_ident1;
static tree medium_ident2;
static tree large_ident1;
static tree large_ident2;

static void
init_idents PARAMS ((void))
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

const struct attribute_spec m32r_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "interrupt", 0, 0, true,  false, false, NULL },
  { "model",     1, 1, true,  false, false, m32r_handle_model_attribute },
  { NULL,        0, 0, false, false, false, NULL }
};


/* Handle an "model" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
m32r_handle_model_attribute (node, name, args, flags, no_add_attrs)
     tree *node ATTRIBUTE_UNUSED;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
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
      warning ("invalid argument of `%s' attribute",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

void
m32r_select_section (decl, reloc)
     tree decl;
     int reloc;
{
  if (TREE_CODE (decl) == STRING_CST)
    {
      if (! flag_writable_strings)
	const_section ();
      else
	data_section ();
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if (SDATA_NAME_P (XSTR (XEXP (DECL_RTL (decl), 0), 0)))
	sdata_section ();
      else if ((flag_pic && reloc)
	       || !TREE_READONLY (decl)
	       || TREE_SIDE_EFFECTS (decl)
	       || !DECL_INITIAL (decl)
	       || (DECL_INITIAL (decl) != error_mark_node
		   && !TREE_CONSTANT (DECL_INITIAL (decl))))
	data_section ();
      else
	const_section ();
    }
  else
    const_section ();
}

/* Encode section information of DECL, which is either a VAR_DECL,
   FUNCTION_DECL, STRING_CST, CONSTRUCTOR, or ???.

   For the M32R we want to record:

   - whether the object lives in .sdata/.sbss.
     objects living in .sdata/.sbss are prefixed with SDATA_FLAG_CHAR

   - what code model should be used to access the object
     small: recorded with no flag - for space efficiency since they'll
            be the most common
     medium: prefixed with MEDIUM_FLAG_CHAR
     large: prefixed with LARGE_FLAG_CHAR
*/

void
m32r_encode_section_info (decl)
     tree decl;
{
  char prefix = 0;
  tree model = 0;

  switch (TREE_CODE (decl))
    {
    case VAR_DECL :
    case FUNCTION_DECL :
      model = lookup_attribute ("model", DECL_ATTRIBUTES (decl));
      break;
    case STRING_CST :
    case CONSTRUCTOR :
      /* ??? document all others that can appear here */
    default :
      return;
    }

  /* Only mark the object as being small data area addressable if
     it hasn't been explicitly marked with a code model.

     The user can explicitly put an object in the small data area with the
     section attribute.  If the object is in sdata/sbss and marked with a
     code model do both [put the object in .sdata and mark it as being
     addressed with a specific code model - don't mark it as being addressed
     with an SDA reloc though].  This is ok and might be useful at times.  If
     the object doesn't fit the linker will give an error.  */

  if (! model)
    {
      if (TREE_CODE_CLASS (TREE_CODE (decl)) == 'd'
	  && DECL_SECTION_NAME (decl) != NULL_TREE)
	{
	  char *name = (char *) TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
	  if (! strcmp (name, ".sdata") || ! strcmp (name, ".sbss"))
	    {
#if 0 /* ??? There's no reason to disallow this, is there?  */
	      if (TREE_READONLY (decl))
		error_with_decl (decl, "const objects cannot go in .sdata/.sbss");
#endif
	      prefix = SDATA_FLAG_CHAR;
	    }
	}
      else
	{
	  if (TREE_CODE (decl) == VAR_DECL
	      && ! TREE_READONLY (decl)
	      && ! TARGET_SDATA_NONE)
	    {
	      int size = int_size_in_bytes (TREE_TYPE (decl));

	      if (size > 0 && size <= g_switch_value)
		prefix = SDATA_FLAG_CHAR;
	    }
	}
    }

  /* If data area not decided yet, check for a code model.  */
  if (prefix == 0)
    {
      if (model)
	{
	  tree id;
	  
	  init_idents ();

	  id = TREE_VALUE (TREE_VALUE (model));

	  if (id == small_ident1 || id == small_ident2)
	    ; /* don't mark the symbol specially */
	  else if (id == medium_ident1 || id == medium_ident2)
	    prefix = MEDIUM_FLAG_CHAR;
	  else if (id == large_ident1 || id == large_ident2)
	    prefix = LARGE_FLAG_CHAR;
	  else
	    abort (); /* shouldn't happen */
	}
      else
	{
	  if (TARGET_MODEL_SMALL)
	    ; /* don't mark the symbol specially */
	  else if (TARGET_MODEL_MEDIUM)
	    prefix = MEDIUM_FLAG_CHAR;
	  else if (TARGET_MODEL_LARGE)
	    prefix = LARGE_FLAG_CHAR;
	  else
	    abort (); /* shouldn't happen */
	}
    }

  if (prefix != 0)
    {
      rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'
                 ? TREE_CST_RTL (decl) : DECL_RTL (decl));
      const char *str = XSTR (XEXP (rtl, 0), 0);
      int len = strlen (str);
      char *newstr = ggc_alloc (len + 2);

      strcpy (newstr + 1, str);
      *newstr = prefix;
      /* Note - we cannot leave the string in the ggc_alloc'ed space.
         It must reside in the stringtable's domain.  */
      newstr = (char *) ggc_alloc_string (newstr, len + 2);

      XSTR (XEXP (rtl, 0), 0) = newstr;
    }
}

/* Do anything needed before RTL is emitted for each function.  */

void
m32r_init_expanders ()
{
  /* ??? At one point there was code here.  The function is left in
     to make it easy to experiment.  */
}

/* Acceptable arguments to the call insn.  */

int
call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return symbolic_operand (op, mode);

/* Constants and values in registers are not OK, because
   the m32r BL instruction can only support PC relative branching.  */ 
}

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return call_address_operand (op, mode);
}

/* Returns 1 if OP is a symbol reference.  */

int
symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST :
      return 1;

    default:
      return 0;
    }
}

/* Return 1 if OP is a reference to an object in .sdata/.sbss.  */

int
small_data_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (! TARGET_SDATA_USE)
    return 0;

  if (GET_CODE (op) == SYMBOL_REF)
    return SDATA_NAME_P (XSTR (op, 0));

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && INT16_P (INTVAL (XEXP (XEXP (op, 0), 1))))
    return SDATA_NAME_P (XSTR (XEXP (XEXP (op, 0), 0), 0));

  return 0;
}

/* Return 1 if OP is a symbol that can use 24 bit addressing.  */

int
addr24_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == LABEL_REF)
    return TARGET_ADDR24;

  if (GET_CODE (op) == SYMBOL_REF)
    return (SMALL_NAME_P (XSTR (op, 0))
	    || (TARGET_ADDR24
		&& (CONSTANT_POOL_ADDRESS_P (op)
		    || LIT_NAME_P (XSTR (op, 0)))));

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && UINT24_P (INTVAL (XEXP (XEXP (op, 0), 1))))
    {
      rtx sym = XEXP (XEXP (op, 0), 0);
      return (SMALL_NAME_P (XSTR (sym, 0))
	      || (TARGET_ADDR24
		  && (CONSTANT_POOL_ADDRESS_P (op)
		      || LIT_NAME_P (XSTR (op, 0)))));
    }

  return 0;
}

/* Return 1 if OP is a symbol that needs 32 bit addressing.  */

int
addr32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == LABEL_REF)
    return TARGET_ADDR32;

  if (GET_CODE (op) == SYMBOL_REF)
    return (! addr24_operand (op, mode)
	    && ! small_data_operand (op, mode));

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT)
    {
      return (! addr24_operand (op, mode)
	      && ! small_data_operand (op, mode));
    }

  return 0;
}

/* Return 1 if OP is a function that can be called with the `bl' insn.  */

int
call26_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF)
    return ! LARGE_NAME_P (XSTR (op, 0));

  return TARGET_CALL26;
}

/* Returns 1 if OP is an acceptable operand for seth/add3.  */

int
seth_add3_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF
      || GET_CODE (op) == LABEL_REF)
    return 1;

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && INT16_P (INTVAL (XEXP (XEXP (op, 0), 1))))
    return 1;

  return 0;
}

/* Return true if OP is a signed 8 bit immediate value.  */

int
int8_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return INT8_P (INTVAL (op));
}

/* Return true if OP is a signed 16 bit immediate value
   useful in comparisons.  */

int
cmp_int16_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return CMP_INT16_P (INTVAL (op));
}

/* Return true if OP is an unsigned 16 bit immediate value.  */

int
uint16_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return UINT16_P (INTVAL (op));
}

/* Return true if OP is a register or signed 16 bit value.  */

int
reg_or_int16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return INT16_P (INTVAL (op));
}

/* Return true if OP is a register or an unsigned 16 bit value.  */

int
reg_or_uint16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return UINT16_P (INTVAL (op));
}

/* Return true if OP is a register or an integer value that can be
   used is SEQ/SNE.  We can use either XOR of the value or ADD of
   the negative of the value for the constant.  Don't allow 0,
   because that is special cased.  */

int
reg_or_eq_int16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  HOST_WIDE_INT value;

  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);

  if (GET_CODE (op) != CONST_INT)
    return 0;

  value = INTVAL (op);
  return (value != 0) && (UINT16_P (value) || CMP_INT16_P (-value));
}

/* Return true if OP is a register or signed 16 bit value for compares.  */

int
reg_or_cmp_int16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return CMP_INT16_P (INTVAL (op));
}

/* Return true if OP is a register or the constant 0.  */

int
reg_or_zero_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);

  if (GET_CODE (op) != CONST_INT)
    return 0;

  return INTVAL (op) == 0;
}

/* Return true if OP is a const_int requiring two instructions to load.  */

int
two_insn_const_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  if (INT16_P (INTVAL (op))
      || UINT24_P (INTVAL (op))
      || UPPER16_P (INTVAL (op)))
    return 0;
  return 1;
}

/* Return true if OP is an acceptable argument for a single word
   move source.  */

int
move_src_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case CONST :
      return addr24_operand (op, mode);
    case CONST_INT :
      /* ??? We allow more cse opportunities if we only allow constants
	 loadable with one insn, and split the rest into two.  The instances
	 where this would help should be rare and the current way is
	 simpler.  */
      if (HOST_BITS_PER_WIDE_INT > 32)
	{
	  HOST_WIDE_INT rest = INTVAL (op) >> 31;
	  return (rest == 0 || rest == -1);
	}
      else
	return 1;
    case LABEL_REF :
      return TARGET_ADDR24;
    case CONST_DOUBLE :
      if (mode == SFmode)
	return 1;
      else if (mode == SImode)
	{
	  /* Large unsigned constants are represented as const_double's.  */
	  unsigned HOST_WIDE_INT low, high;

	  low = CONST_DOUBLE_LOW (op);
	  high = CONST_DOUBLE_HIGH (op);
	  return high == 0 && low <= 0xffffffff;
	}
      else
	return 0;
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      if (GET_CODE (XEXP (op, 0)) == PRE_INC
	  || GET_CODE (XEXP (op, 0)) == PRE_DEC)
	return 0;		/* loads can't do pre-{inc,dec} */
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
}

/* Return true if OP is an acceptable argument for a double word
   move source.  */

int
move_double_src_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case CONST_INT :
    case CONST_DOUBLE :
      return 1;
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return move_double_src_operand (SUBREG_REG (op), mode);
      else
	return register_operand (op, mode);
    case MEM :
      /* Disallow auto inc/dec for now.  */
      if (GET_CODE (XEXP (op, 0)) == PRE_DEC
	  || GET_CODE (XEXP (op, 0)) == PRE_INC)
	return 0;
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
}

/* Return true if OP is an acceptable argument for a move destination.  */

int
move_dest_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      if (GET_CODE (XEXP (op, 0)) == POST_INC)
	return 0;		/* stores can't do post inc */
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
}

/* Return 1 if OP is a DImode const we want to handle inline.
   This must match the code in the movdi pattern.
   It is used by the 'G' CONST_DOUBLE_OK_FOR_LETTER.  */

int
easy_di_const (op)
     rtx op;
{
  rtx high_rtx, low_rtx;
  HOST_WIDE_INT high, low;

  split_double (op, &high_rtx, &low_rtx);
  high = INTVAL (high_rtx);
  low = INTVAL (low_rtx);
  /* Pick constants loadable with 2 16 bit `ldi' insns.  */
  if (high >= -128 && high <= 127
      && low >= -128 && low <= 127)
    return 1;
  return 0;
}

/* Return 1 if OP is a DFmode const we want to handle inline.
   This must match the code in the movdf pattern.
   It is used by the 'H' CONST_DOUBLE_OK_FOR_LETTER.  */

int
easy_df_const (op)
     rtx op;
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

/* Return 1 if OP is an EQ or NE comparison operator.  */

int
eqne_comparison_operator (op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;
  return (code == EQ || code == NE);
}

/* Return 1 if OP is a signed comparison operator.  */

int
signed_comparison_operator (op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;
  return (code == EQ || code == NE
	  || code == LT || code == LE || code == GT || code == GE);
}

/* Return 1 if OP is (mem (reg ...)).
   This is used in insn length calcs.  */

int
memreg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == REG;
}

/* Return true if OP is an acceptable input argument for a zero/sign extend
   operation.  */

int
extend_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr;

  switch (GET_CODE (op))
    {
    case REG :
    case SUBREG :
      return register_operand (op, mode);

    case MEM :
      addr = XEXP (op, 0);
      if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	return 0;		/* loads can't do pre inc/pre dec */

      return address_operand (addr, mode);

    default :
      return 0;
    }
}

/* Return non-zero if the operand is an insn that is a small insn.
   Allow const_int 0 as well, which is a placeholder for NOP slots.  */

int
small_insn_p (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT && INTVAL (op) == 0)
    return 1;

  if (! INSN_P (op))
    return 0;

  return get_attr_length (op) == 2;
}

/* Return non-zero if the operand is an insn that is a large insn.  */

int
large_insn_p (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (! INSN_P (op))
    return 0;

  return get_attr_length (op) != 2;
}


/* Comparisons.  */

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

int
m32r_select_cc_mode (op, x, y)
     int op ATTRIBUTE_UNUSED;
     rtx x ATTRIBUTE_UNUSED;
     rtx y ATTRIBUTE_UNUSED;
{
  return (int) CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for compare [arg0 of the if_then_else].
   If need_compare is true then the comparison insn must be generated, rather
   than being susummed into the following branch instruction.  */

rtx
gen_compare (code, x, y, need_compare)
     enum rtx_code code;
     rtx x, y;
     int need_compare;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  enum rtx_code compare_code, branch_code;
  rtx cc_reg = gen_rtx_REG (mode, CARRY_REGNUM);
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
      abort ();
    }

  if (need_compare)
    {
      switch (compare_code)
	{
	case EQ:
	  if (GET_CODE (y) == CONST_INT
	      && CMP_INT16_P (INTVAL (y))		/* reg equal to small const.  */
	      && y != const0_rtx)
	    {
	      rtx tmp = gen_reg_rtx (SImode);		
	      
	      emit_insn (gen_cmp_ne_small_const_insn (tmp, x, y));
	      x = tmp;
	      y = const0_rtx;
	    }
	  else if (CONSTANT_P (y))			/* reg equal to const.  */
	    {
	      rtx tmp = force_reg (GET_MODE (x), y);
	      y = tmp;
	    }

	  if (register_operand (y, SImode) 		/* reg equal to reg.  */
	      || y == const0_rtx) 	   		/* req equal to zero. */
	    {
	      emit_insn (gen_cmp_eqsi_insn (x, y));
		
	      return gen_rtx (code, mode, cc_reg, const0_rtx);
	    }
	  break;
      
	case LT:
	  if (register_operand (y, SImode)
	      || (GET_CODE (y) == CONST_INT && CMP_INT16_P (INTVAL (y))))
	    {
	      rtx tmp = gen_reg_rtx (SImode);	      /* reg compared to reg. */
	      
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
		    emit_insn (gen_cmp_ne_small_const_insn (tmp, y, const1_rtx));
		  emit_insn (gen_cmp_ltsi_insn (x, tmp));
		  code = EQ;
		  break;
		case GT:
		  if (GET_CODE (y) == CONST_INT)
		    tmp = gen_rtx (PLUS, SImode, y, const1_rtx);
		  else
		    emit_insn (gen_cmp_ne_small_const_insn (tmp, y, const1_rtx));
		  emit_insn (gen_cmp_ltsi_insn (x, tmp));
		  code = NE;
		  break;
		case GE:
		  emit_insn (gen_cmp_ltsi_insn (x, y));
		  code = NE;
		  break;
		default:
		  abort ();
		}
	      
	      return gen_rtx (code, mode, cc_reg, const0_rtx);
	    }
	  break;
	  
	case LTU:
	  if (register_operand (y, SImode)
	      || (GET_CODE (y) == CONST_INT && CMP_INT16_P (INTVAL (y))))
	    {
	      rtx tmp = gen_reg_rtx (SImode);	      /* reg (unsigned) compared to reg. */
	      
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
		    emit_insn (gen_cmp_ne_small_const_insn (tmp, y, const1_rtx));
		  emit_insn (gen_cmp_ltusi_insn (x, tmp));
		  code = EQ;
		  break;
		case GTU:
		  if (GET_CODE (y) == CONST_INT)
		    tmp = gen_rtx (PLUS, SImode, y, const1_rtx);
		  else
		    emit_insn (gen_cmp_ne_small_const_insn (tmp, y, const1_rtx));
		  emit_insn (gen_cmp_ltusi_insn (x, tmp));
		  code = NE;
		  break;
		case GEU:
		  emit_insn (gen_cmp_ltusi_insn (x, y));
		  code = NE;
		  break;
		default:
		  abort();
		}
	      
	      return gen_rtx (code, mode, cc_reg, const0_rtx);
	    }
	  break;

	default:
	  abort();
	}
    }
  else
    {
      /* reg/reg equal comparison */
      if (compare_code == EQ
	  && register_operand (y, SImode))
	return gen_rtx (code, mode, x, y);
      
      /* reg/zero signed comparison */
      if ((compare_code == EQ || compare_code == LT)
	  && y == const0_rtx)
	return gen_rtx (code, mode, x, y);
      
      /* reg/smallconst equal comparison */
      if (compare_code == EQ
	  && GET_CODE (y) == CONST_INT
	  && CMP_INT16_P (INTVAL (y)))
	{
	  rtx tmp = gen_reg_rtx (SImode);
	  emit_insn (gen_cmp_ne_small_const_insn (tmp, x, y));
	  return gen_rtx (code, mode, tmp, const0_rtx);
	}
      
      /* reg/const equal comparison */
      if (compare_code == EQ
	  && CONSTANT_P (y))
	{
	  rtx tmp = force_reg (GET_MODE (x), y);
	  return gen_rtx (code, mode, x, tmp);
	}
    }

  if (CONSTANT_P (y))
    {
      if (must_swap)
	y = force_reg (GET_MODE (x), y);
      else
	{
	  int ok_const =
	    (code == LTU || code == LEU || code == GTU || code == GEU)
	    ? uint16_operand (y, GET_MODE (y))
	    : reg_or_cmp_int16_operand (y, GET_MODE (y));
	  
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
      abort ();
    }

  return gen_rtx (branch_code, VOIDmode, cc_reg, CONST0_RTX (mode));
}

/* Split a 2 word move (DI or DF) into component parts.  */

rtx
gen_split_move_double (operands)
     rtx operands[];
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

  /* We might have (SUBREG (MEM)) here, so just get rid of the
     subregs to make this code simpler.  It is safe to call
     alter_subreg any time after reload.  */
  if (GET_CODE (dest) == SUBREG)
    alter_subreg (&dest);
  if (GET_CODE (src) == SUBREG)
    alter_subreg (&src);

  start_sequence ();
  if (GET_CODE (dest) == REG)
    {
      int dregno = REGNO (dest);

      /* reg = reg */
      if (GET_CODE (src) == REG)
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

      /* reg = constant */
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
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

      /* reg = mem */
      else if (GET_CODE (src) == MEM)
	{
	  /* If the high-address word is used in the address, we must load it
	     last.  Otherwise, load it first.  */
	  int reverse
	    = (refers_to_regno_p (dregno, dregno + 1, XEXP (src, 0), 0) != 0);

	  /* We used to optimize loads from single registers as

		ld r1,r3+; ld r2,r3

	     if r3 were not used subsequently.  However, the REG_NOTES aren't
	     propigated correctly by the reload phase, and it can cause bad
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
	abort ();
    }

  /* mem = reg */
  /* We used to optimize loads from single registers as

	st r1,r3; st r2,+r3

     if r3 were not used subsequently.  However, the REG_NOTES aren't
     propigated correctly by the reload phase, and it can cause bad
     code to be generated.  We could still try:

	st r1,r3; st r2,+r3; addi r3,-4

     which saves 2 bytes and doesn't force longword alignment.  */
  else if (GET_CODE (dest) == MEM && GET_CODE (src) == REG)
    {
      emit_insn (gen_rtx_SET (VOIDmode,
			      adjust_address (dest, SImode, 0),
			      operand_subword (src, 0, TRUE, mode)));

      emit_insn (gen_rtx_SET (VOIDmode,
			      adjust_address (dest, SImode, UNITS_PER_WORD),
			      operand_subword (src, 1, TRUE, mode)));
    }

  else
    abort ();

  val = gen_sequence ();
  end_sequence ();
  return val;
}


/* Implements the FUNCTION_ARG_PARTIAL_NREGS macro.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int ret;
  unsigned int size =
    (((mode == BLKmode && type)
      ? (unsigned int) int_size_in_bytes (type)
      : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
    / UNITS_PER_WORD;

  if (*cum >= M32R_MAX_PARM_REGS)
    ret = 0;
  else if (*cum + size > M32R_MAX_PARM_REGS)
    ret = (*cum + size) - M32R_MAX_PARM_REGS;
  else
    ret = 0;

  return ret;
}

/* Do any needed setup for a variadic function.  For the M32R, we must
   create a register parameter block, and then copy any anonymous arguments
   in registers to memory.

   CUM has not been updated for the last named argument which has type TYPE
   and mode MODE, and we rely on this fact.  */

void
m32r_setup_incoming_varargs (cum, mode, type, pretend_size, no_rtl)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int *pretend_size;
     int no_rtl;
{
  int first_anon_arg;

  if (no_rtl)
    return;

  /* All BLKmode values are passed by reference.  */
  if (mode == BLKmode)
    abort ();

  /* We must treat `__builtin_va_alist' as an anonymous arg.  */
  if (current_function_varargs)
    first_anon_arg = *cum;
  else
    first_anon_arg = (ROUND_ADVANCE_CUM (*cum, mode, type)
		      + ROUND_ADVANCE_ARG (mode, type));

  if (first_anon_arg < M32R_MAX_PARM_REGS)
    {
      /* Note that first_reg_offset < M32R_MAX_PARM_REGS.  */
      int first_reg_offset = first_anon_arg;
      /* Size in words to "pretend" allocate.  */
      int size = M32R_MAX_PARM_REGS - first_reg_offset;
      rtx regblock;

      regblock = gen_rtx_MEM (BLKmode,
			      plus_constant (arg_pointer_rtx,
					     FIRST_PARM_OFFSET (0)));
      set_mem_alias_set (regblock, get_varargs_alias_set ());
      move_block_from_reg (first_reg_offset, regblock,
			   size, size * UNITS_PER_WORD);

      *pretend_size = (size * UNITS_PER_WORD);
    }
}


/* Implement `va_arg'.  */

rtx
m32r_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT size, rsize;
  tree t;
  rtx addr_rtx;

  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;

  if (size > 8)
    {
      tree type_ptr, type_ptr_ptr;

      /* Pass by reference.  */

      type_ptr = build_pointer_type (type);
      type_ptr_ptr = build_pointer_type (type_ptr);

      t = build (POSTINCREMENT_EXPR, va_list_type_node, valist, 
		 build_int_2 (UNITS_PER_WORD, 0));
      TREE_SIDE_EFFECTS (t) = 1;
      t = build1 (NOP_EXPR, type_ptr_ptr, t);
      TREE_SIDE_EFFECTS (t) = 1;
      t = build1 (INDIRECT_REF, type_ptr, t);

      addr_rtx = expand_expr (t, NULL_RTX, Pmode, EXPAND_NORMAL);
    }
  else
    {
      /* Pass by value.  */

      if (size < UNITS_PER_WORD)
	{
	  /* Care for bigendian correction on the aligned address.  */
	  t = build (PLUS_EXPR, ptr_type_node, valist,
		     build_int_2 (rsize - size, 0));
	  addr_rtx = expand_expr (t, NULL_RTX, Pmode, EXPAND_NORMAL);
	  addr_rtx = copy_to_reg (addr_rtx);

	  /* Increment AP.  */
	  t = build (PLUS_EXPR, va_list_type_node, valist,
		     build_int_2 (rsize, 0));
	  t = build (MODIFY_EXPR, va_list_type_node, valist, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	}
      else
	{
	  t = build (POSTINCREMENT_EXPR, va_list_type_node, valist, 
		     build_int_2 (rsize, 0));
	  TREE_SIDE_EFFECTS (t) = 1;
	  addr_rtx = expand_expr (t, NULL_RTX, Pmode, EXPAND_NORMAL);
	}
    }

  return addr_rtx;
}

static int
m32r_adjust_cost (insn, link, dep_insn, cost)
     rtx insn ATTRIBUTE_UNUSED;
     rtx link ATTRIBUTE_UNUSED;
     rtx dep_insn ATTRIBUTE_UNUSED;
     int cost;
{
  return cost;
}


/* Return true if INSN is real instruction bearing insn.  */

static int
m32r_is_insn (insn)
     rtx insn;
{
  return (INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER
	  && GET_CODE (PATTERN (insn)) != ADDR_VEC);
}

/* Increase the priority of long instructions so that the
   short instructions are scheduled ahead of the long ones.  */

static int
m32r_adjust_priority (insn, priority)
     rtx insn;
     int priority;
{
  if (m32r_is_insn (insn)
      && get_attr_insn_size (insn) != INSN_SIZE_SHORT)
    priority <<= 3;

  return priority;
}


/* Initialize for scheduling a group of instructions.  */

static void
m32r_sched_init (stream, verbose, max_ready)
     FILE * stream ATTRIBUTE_UNUSED;
     int verbose ATTRIBUTE_UNUSED;
     int max_ready ATTRIBUTE_UNUSED;
{
  m32r_sched_odd_word_p = FALSE;
}


/* Reorder the schedulers priority list if needed */

static int
m32r_sched_reorder (stream, verbose, ready, n_readyp, clock)
     FILE * stream;
     int verbose;
     rtx * ready;
     int *n_readyp;
     int clock ATTRIBUTE_UNUSED;
{
  int n_ready = *n_readyp;

  if (TARGET_DEBUG)
    return m32r_issue_rate ();

  if (verbose <= 7)
    stream = (FILE *)0;

  if (stream)
    fprintf (stream,
	     ";;\t\t::: Looking at %d insn(s) on ready list, boundary is %s word\n",
	     n_ready,
	     (m32r_sched_odd_word_p) ? "odd" : "even");

  if (n_ready > 1)
    {
      rtx * long_head = (rtx *) alloca (sizeof (rtx) * n_ready);
      rtx * long_tail = long_head;
      rtx * short_head = (rtx *) alloca (sizeof (rtx) * n_ready);
      rtx * short_tail = short_head;
      rtx * new_head = (rtx *) alloca (sizeof (rtx) * n_ready);
      rtx * new_tail = new_head + (n_ready - 1);
      int   i;

      /* Loop through the instructions, classifing them as short/long.  Try
	 to keep 2 short together and/or 1 long.  Note, the ready list is
	 actually ordered backwards, so keep it in that manner.  */
      for (i = n_ready-1; i >= 0; i--)
	{
	  rtx insn = ready[i];

	  if (! m32r_is_insn (insn))
	    {
	      /* Dump all current short/long insns just in case.  */
	      while (long_head != long_tail)
		*new_tail-- = *long_head++;

	      while (short_head != short_tail)
		*new_tail-- = *short_head++;

	      *new_tail-- = insn;
	      if (stream)
		fprintf (stream,
			 ";;\t\t::: Skipping non instruction %d\n",
			 INSN_UID (insn));

	    }

	  else
	    {
	      if (get_attr_insn_size (insn) != INSN_SIZE_SHORT)
		*long_tail++ = insn;

	      else
		*short_tail++ = insn;
	    }
	}

      /* If we are on an odd word, emit a single short instruction if
	 we can */
      if (m32r_sched_odd_word_p && short_head != short_tail)
	*new_tail-- = *short_head++;

      /* Now dump out all of the long instructions */
      while (long_head != long_tail)
	*new_tail-- = *long_head++;

      /* Now dump out all of the short instructions */
      while (short_head != short_tail)
	*new_tail-- = *short_head++;

      if (new_tail+1 != new_head)
	abort ();

      memcpy (ready, new_head, sizeof (rtx) * n_ready);
      if (stream)
	{
	  int i;
	  fprintf (stream, ";;\t\t::: New ready list:               ");
	  for (i = 0; i < n_ready; i++)
	    {
	      rtx insn = ready[i];

	      fprintf (stream, " %d", INSN_UID (ready[i]));

	      if (! m32r_is_insn (insn))
		fputs ("(?)", stream);

	      else if (get_attr_insn_size (insn) != INSN_SIZE_SHORT)
		fputs ("(l)", stream);

	      else
		fputs ("(s)", stream);
	    }

	  fprintf (stream, "\n");
	}
    }
  return m32r_issue_rate ();
}

/* Indicate how many instructions can be issued at the same time.
   This is sort of a lie.  The m32r can issue only 1 long insn at
   once, but it can issue 2 short insns.  The default therefore is
   set at 2, but this can be overridden by the command line option
   -missue-rate=1 */
static int
m32r_issue_rate ()
{
  return ((TARGET_LOW_ISSUE_RATE) ? 1 : 2);
}

/* If we have a machine that can issue a variable # of instructions
   per cycle, indicate how many more instructions can be issued
   after the current one.  */
static int
m32r_variable_issue (stream, verbose, insn, how_many)
     FILE * stream;
     int verbose;
     rtx insn;
     int how_many;
{
  int orig_odd_word_p = m32r_sched_odd_word_p;
  int short_p = FALSE;

  how_many--;
  if (how_many > 0 && !TARGET_DEBUG)
    {
      if (! m32r_is_insn (insn))
	how_many++;

      else if (get_attr_insn_size (insn) != INSN_SIZE_SHORT)
	{
	  how_many = 0;
	  m32r_sched_odd_word_p = 0;
	}
      else
	{
	  m32r_sched_odd_word_p = !m32r_sched_odd_word_p;
	  short_p = TRUE;
	}
    }

  if (verbose > 7 && stream)
    fprintf (stream,
	     ";;\t\t::: %s insn %d starts on an %s word, can emit %d more instruction(s)\n",
	     short_p ? "short" : "long",
	     INSN_UID (insn),
	     orig_odd_word_p ? "odd" : "even",
	     how_many);

  return how_many;
}

/* Cost functions.  */

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.

   This function is trivial at the moment.  This code doesn't live
   in m32r.h so it's easy to experiment.  */

int
m32r_address_cost (addr)
     rtx addr ATTRIBUTE_UNUSED;
{
  return 1;
}

/* Type of function DECL.

   The result is cached.  To reset the cache at the end of a function,
   call with DECL = NULL_TREE.  */

enum m32r_function_type
m32r_compute_function_type (decl)
     tree decl;
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
   many insns as possible between the restoration of `lr' and the `jmp lr'.
*/

/* Structure to be filled in by m32r_compute_frame_size with register
   save masks, and offsets for the current function.  */
struct m32r_frame_info
{
  unsigned int total_size;	/* # bytes that the entire frame takes up */
  unsigned int extra_size;	/* # bytes of extra stuff */
  unsigned int pretend_size;	/* # bytes we push and pretend caller did */
  unsigned int args_size;	/* # bytes that outgoing arguments take up */
  unsigned int reg_size;	/* # bytes needed to store regs */
  unsigned int var_size;	/* # bytes that variables take up */
  unsigned int gmask;		/* mask of saved gp registers */
  unsigned int save_fp;		/* nonzero if fp must be saved */
  unsigned int save_lr;		/* nonzero if lr (return addr) must be saved */
  int          initialized;	/* nonzero if frame size already calculated */
};

/* Current frame information calculated by m32r_compute_frame_size.  */
static struct m32r_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct m32r_frame_info zero_frame_info;

#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK (1 << (RETURN_ADDR_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.  */
#define MUST_SAVE_REGISTER(regno, interrupt_p) \
((regno) != RETURN_ADDR_REGNUM && (regno) != FRAME_POINTER_REGNUM \
 && (regs_ever_live[regno] && (!call_used_regs[regno] || interrupt_p)))

#define MUST_SAVE_FRAME_POINTER (regs_ever_live[FRAME_POINTER_REGNUM])
#define MUST_SAVE_RETURN_ADDR (regs_ever_live[RETURN_ADDR_REGNUM] || current_function_profile)

#define SHORT_INSN_SIZE 2	/* size of small instructions */
#define LONG_INSN_SIZE 4	/* size of long instructions */

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

unsigned int
m32r_compute_frame_size (size)
     int size;			/* # of var. bytes allocated.  */
{
  int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size, frame_size;
  unsigned int gmask;
  enum m32r_function_type fn_type;
  int interrupt_p;

  var_size	= M32R_STACK_ALIGN (size);
  args_size	= M32R_STACK_ALIGN (current_function_outgoing_args_size);
  pretend_size	= current_function_pretend_args_size;
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
      if (MUST_SAVE_REGISTER (regno, interrupt_p))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
	}
    }

  current_frame_info.save_fp = MUST_SAVE_FRAME_POINTER;
  current_frame_info.save_lr = MUST_SAVE_RETURN_ADDR;

  reg_size += ((current_frame_info.save_fp + current_frame_info.save_lr)
	       * UNITS_PER_WORD);
  total_size += reg_size;

  /* ??? Not sure this is necessary, and I don't think the epilogue
     handler will do the right thing if this changes total_size.  */
  total_size = M32R_STACK_ALIGN (total_size);

  frame_size = total_size - (pretend_size + reg_size);

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

/* When the `length' insn attribute is used, this macro specifies the
   value to be assigned to the address of the first insn in a
   function.  If not specified, 0 is used.  */

int
m32r_first_insn_address ()
{
  if (! current_frame_info.initialized)
    m32r_compute_frame_size (get_frame_size ());

  return 0;
}

/* Expand the m32r prologue as a series of insns.  */

void
m32r_expand_prologue ()
{
  int regno;
  int frame_size;
  unsigned int gmask;

  if (! current_frame_info.initialized)
    m32r_compute_frame_size (get_frame_size ());

  gmask = current_frame_info.gmask;

  /* These cases shouldn't happen.  Catch them now.  */
  if (current_frame_info.total_size == 0 && gmask)
    abort ();

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
    ; /* nothing to do */
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

  if (current_function_profile)
    emit_insn (gen_blockage ());
}


/* Set up the stack and frame pointer (if desired) for the function.
   Note, if this is changed, you need to mirror the changes in
   m32r_compute_frame_size which calculates the prolog size.  */

static void
m32r_output_function_prologue (file, size)
     FILE * file;
     HOST_WIDE_INT size;
{
  enum m32r_function_type fn_type = m32r_compute_function_type (current_function_decl);

  /* If this is an interrupt handler, mark it as such.  */
  if (M32R_INTERRUPT_P (fn_type))
    {
      fprintf (file, "\t%s interrupt handler\n",
	       ASM_COMMENT_START);
    }

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

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs. */

static void
m32r_output_function_epilogue (file, size)
     FILE * file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int regno;
  int noepilogue = FALSE;
  int total_size;
  enum m32r_function_type fn_type = m32r_compute_function_type (current_function_decl);

  /* This is only for the human reader.  */
  fprintf (file, "\t%s EPILOGUE\n", ASM_COMMENT_START);

  if (!current_frame_info.initialized)
    abort ();
  total_size = current_frame_info.total_size;

  if (total_size == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn && GET_CODE (insn) == BARRIER)
	noepilogue = TRUE;
    }

  if (!noepilogue)
    {
      unsigned int var_size = current_frame_info.var_size;
      unsigned int args_size = current_frame_info.args_size;
      unsigned int gmask = current_frame_info.gmask;
      int can_trust_sp_p = !current_function_calls_alloca;
      const char * sp_str = reg_names[STACK_POINTER_REGNUM];
      const char * fp_str = reg_names[FRAME_POINTER_REGNUM];

      /* The first thing to do is point the sp at the bottom of the register
	 save area.  */
      if (can_trust_sp_p)
	{
	  unsigned int reg_offset = var_size + args_size;
	  if (reg_offset == 0)
	    ; /* nothing to do */
	  else if (reg_offset < 128)
	    fprintf (file, "\taddi %s,%s%d\n",
		     sp_str, IMMEDIATE_PREFIX, reg_offset);
	  else if (reg_offset < 32768)
	    fprintf (file, "\tadd3 %s,%s,%s%d\n",
		     sp_str, sp_str, IMMEDIATE_PREFIX, reg_offset);
	  else
	    fprintf (file, "\tld24 %s,%s%d\n\tadd %s,%s\n",
		     reg_names[PROLOGUE_TMP_REGNUM],
		     IMMEDIATE_PREFIX, reg_offset,
		     sp_str, reg_names[PROLOGUE_TMP_REGNUM]);
	}
      else if (frame_pointer_needed)
	{
	  unsigned int reg_offset = var_size + args_size;
	  if (reg_offset == 0)
	    fprintf (file, "\tmv %s,%s\n", sp_str, fp_str);
	  else if (reg_offset < 32768)
	    fprintf (file, "\tadd3 %s,%s,%s%d\n",
		     sp_str, fp_str, IMMEDIATE_PREFIX, reg_offset);
	  else
	    fprintf (file, "\tld24 %s,%s%d\n\tadd %s,%s\n",
		     reg_names[PROLOGUE_TMP_REGNUM],
		     IMMEDIATE_PREFIX, reg_offset,
		     sp_str, reg_names[PROLOGUE_TMP_REGNUM]);
	}
      else
	abort ();

      if (current_frame_info.save_lr)
	fprintf (file, "\tpop %s\n", reg_names[RETURN_ADDR_REGNUM]);

      /* Restore any saved registers, in reverse order of course.  */
      gmask &= ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK);
      for (regno = M32R_MAX_INT_REGS - 1; regno >= 0; --regno)
	{
	  if ((gmask & (1L << regno)) != 0)
	    fprintf (file, "\tpop %s\n", reg_names[regno]);
	}

      if (current_frame_info.save_fp)
	fprintf (file, "\tpop %s\n", fp_str);

      /* Remove varargs area if present.  */
      if (current_frame_info.pretend_size != 0)
	fprintf (file, "\taddi %s,%s%d\n",
		 sp_str, IMMEDIATE_PREFIX, current_frame_info.pretend_size);
	
      /* Emit the return instruction.  */
      if (M32R_INTERRUPT_P (fn_type))
	fprintf (file, "\trte\n");
      else
	fprintf (file, "\tjmp %s\n", reg_names[RETURN_ADDR_REGNUM]);
    }

#if 0 /* no longer needed */
  /* Ensure the function cleanly ends on a 32 bit boundary.  */
  fprintf (file, "\t.fillinsn\n");
#endif

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
  m32r_compute_function_type (NULL_TREE);
}

/* Return non-zero if this function is known to have a null or 1 instruction
   epilogue.  */

int
direct_return ()
{
  if (!reload_completed)
    return FALSE;

  if (! current_frame_info.initialized)
    m32r_compute_frame_size (get_frame_size ());

  return current_frame_info.total_size == 0;
}


/* PIC */

/* Emit special PIC prologues and epilogues.  */

void
m32r_finalize_pic ()
{
  /* nothing to do */
}

/* Nested function support.  */

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void
m32r_initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp ATTRIBUTE_UNUSED;
     rtx fnaddr ATTRIBUTE_UNUSED;
     rtx cxt ATTRIBUTE_UNUSED;
{
}

/* Set the cpu type and print out other fancy things,
   at the top of the file.  */

void
m32r_asm_file_start (file)
     FILE * file;
{
  if (flag_verbose_asm)
    fprintf (file, "%s M32R/D special options: -G %d\n",
	     ASM_COMMENT_START, g_switch_value);
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
m32r_print_operand (file, x, code)
     FILE * file;
     rtx    x;
     int    code;
{
  rtx addr;

  switch (code)
    {
      /* The 's' and 'p' codes are used by output_block_move() to
	 indicate post-increment 's'tores and 'p're-increment loads.  */
    case 's':
      if (GET_CODE (x) == REG)
	fprintf (file, "@+%s", reg_names [REGNO (x)]);
      else
	output_operand_lossage ("invalid operand to %%s code");
      return;
      
    case 'p':
      if (GET_CODE (x) == REG)
	fprintf (file, "@%s+", reg_names [REGNO (x)]);
      else
	output_operand_lossage ("invalid operand to %%p code");
      return;

    case 'R' :
      /* Write second word of DImode or DFmode reference,
	 register or memory.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)+1], file);
      else if (GET_CODE (x) == MEM)
	{
	  fprintf (file, "@(");
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of four.  */
	  /* ??? This is taken from rs6000.c I think.  I don't think it is
	     currently necessary, but keep it around.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 4));
	  else
	    output_address (plus_constant (XEXP (x, 0), 4));
	  fputc (')', file);
	}
      else
	output_operand_lossage ("invalid operand to %%R code");
      return;

    case 'H' : /* High word */
    case 'L' : /* Low word */
      if (GET_CODE (x) == REG)
	{
	  /* L = least significant word, H = most significant word */
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
	  fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		   code == 'L' ? INTVAL (first) : INTVAL (second));
	}
      else
	output_operand_lossage ("invalid operand to %%H/%%L code");
      return;

    case 'A' :
      {
	REAL_VALUE_TYPE d;
	char str[30];

	if (GET_CODE (x) != CONST_DOUBLE
	    || GET_MODE_CLASS (GET_MODE (x)) != MODE_FLOAT)
	  fatal_insn ("bad insn for 'A'", x);
	REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	REAL_VALUE_TO_DECIMAL (d, "%.20e", str);
	fprintf (file, "%s", str);
	return;
      }

    case 'B' : /* Bottom half */
    case 'T' : /* Top half */
      /* Output the argument to a `seth' insn (sets the Top half-word).
	 For constants output arguments to a seth/or3 pair to set Top and
	 Bottom halves.  For symbols output arguments to a seth/add3 pair to
	 set Top and Bottom halves.  The difference exists because for
	 constants seth/or3 is more readable but for symbols we need to use
	 the same scheme as `ld' and `st' insns (16 bit addend is signed).  */
      switch (GET_CODE (x))
	{
	case CONST_INT :
	case CONST_DOUBLE :
	  {
	    rtx first, second;

	    split_double (x, &first, &second);
	    x = WORDS_BIG_ENDIAN ? second : first;
	    fprintf (file,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		     "0x%x",
#else
		     "0x%lx",
#endif
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
      if (GET_CODE (x) == MEM)
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
      if (GET_CODE (x) == CONST_INT)
	output_addr_const (file, GEN_INT (- INTVAL (x)));
      else
	output_operand_lossage ("invalid operand to %%N code");
      return;

    case 'X' :
      /* Print a const_int in hex.  Used in comments.  */
      if (GET_CODE (x) == CONST_INT)
	fprintf (file,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 "0x%x",
#else
		 "0x%lx",
#endif
		 INTVAL (x));
      return;

    case '#' :
      fputs (IMMEDIATE_PREFIX, file);
      return;

#if 0 /* ??? no longer used */
    case '@' :
      fputs (reg_names[SDA_REGNUM], file);
      return;
#endif

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
	  if (GET_CODE (XEXP (addr, 0)) != REG)
	    fatal_insn ("pre-increment address is not a register", x);

	  fprintf (file, "@+%s", reg_names[REGNO (XEXP (addr, 0))]);
	}
      else if (GET_CODE (addr) == PRE_DEC)
	{
	  if (GET_CODE (XEXP (addr, 0)) != REG)
	    fatal_insn ("pre-decrement address is not a register", x);

	  fprintf (file, "@-%s", reg_names[REGNO (XEXP (addr, 0))]);
	}
      else if (GET_CODE (addr) == POST_INC)
	{
	  if (GET_CODE (XEXP (addr, 0)) != REG)
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

void
m32r_print_operand_address (file, addr)
     FILE * file;
     rtx    addr;
{
  register rtx base;
  register rtx index = 0;
  int          offset = 0;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;

    case PLUS :
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);
      if (GET_CODE (base) == REG)
	{
	  /* Print the offset first (if present) to conform to the manual.  */
	  if (index == 0)
	    {
	      if (offset != 0)
		fprintf (file, "%d,", offset);
	      fputs (reg_names[REGNO (base)], file);
	    }
	  /* The chip doesn't support this, but left in for generality.  */
	  else if (GET_CODE (index) == REG)
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
	  if (index != 0
	      || GET_CODE (XEXP (base, 0)) != REG)
	    abort ();
	  if (small_data_operand (XEXP (base, 1), VOIDmode))
	    fputs ("sda(", file);
	  else
	    fputs ("low(", file);
	  output_addr_const (file, plus_constant (XEXP (base, 1), offset));
	  fputs ("),", file);
	  fputs (reg_names[REGNO (XEXP (base, 0))], file);
	}
      else
	fatal_insn ("bad address", addr);
      break;

    case LO_SUM :
      if (GET_CODE (XEXP (addr, 0)) != REG)
	fatal_insn ("lo_sum not of register", addr);
      if (small_data_operand (XEXP (addr, 1), VOIDmode))
	fputs ("sda(", file);
      else
	fputs ("low(", file);
      output_addr_const (file, XEXP (addr, 1));
      fputs ("),", file);
      fputs (reg_names[REGNO (XEXP (addr, 0))], file);
      break;

    case PRE_INC :	/* Assume SImode */
      fprintf (file, "+%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PRE_DEC :	/* Assume SImode */
      fprintf (file, "-%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC :	/* Assume SImode */
      fprintf (file, "%s+", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    default :
      output_addr_const (file, addr);
      break;
    }
}

/* Return true if the operands are the constants 0 and 1.  */
int
zero_and_one (operand1, operand2)
     rtx operand1;
     rtx operand2;
{
  return
       GET_CODE (operand1) == CONST_INT
    && GET_CODE (operand2) == CONST_INT
    && (  ((INTVAL (operand1) == 0) && (INTVAL (operand2) == 1))
	||((INTVAL (operand1) == 1) && (INTVAL (operand2) == 0)));
}

/* Return non-zero if the operand is suitable for use in a conditional move sequence.  */
int
conditional_move_operand (operand, mode)
     rtx operand;
     enum machine_mode mode;
{
  /* Only defined for simple integers so far... */
  if (mode != SImode && mode != HImode && mode != QImode)
    return FALSE;

  /* At the moment we can hanndle moving registers and loading constants.  */
  /* To be added: Addition/subtraction/bitops/multiplication of registers.  */

  switch (GET_CODE (operand))
    {
    case REG:
      return 1;

    case CONST_INT:
      return INT8_P (INTVAL (operand));

    default:
#if 0
      fprintf (stderr, "Test for cond move op of type: %s\n",
	       GET_RTX_NAME (GET_CODE (operand)));
#endif
      return 0;
    }
}

/* Return true if the code is a test of the carry bit */
int
carry_compare_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx x;

  if (GET_MODE (op) != CCmode && GET_MODE (op) != VOIDmode)
    return FALSE;

  if (GET_CODE (op) != NE && GET_CODE (op) != EQ)
    return FALSE;

  x = XEXP (op, 0);
  if (GET_CODE (x) != REG || REGNO (x) != CARRY_REGNUM)
    return FALSE;

  x = XEXP (op, 1);
  if (GET_CODE (x) != CONST_INT || INTVAL (x) != 0)
    return FALSE;

  return TRUE;
}

/* Generate the correct assembler code to handle the conditional loading of a
   value into a register.  It is known that the operands satisfy the
   conditional_move_operand() function above.  The destination is operand[0].
   The condition is operand [1].  The 'true' value is operand [2] and the
   'false' value is operand [3].  */
char *
emit_cond_move (operands, insn)
     rtx * operands;
     rtx   insn ATTRIBUTE_UNUSED;
{
  static char buffer [100];
  const char * dest = reg_names [REGNO (operands [0])];
  
  buffer [0] = 0;
  
  /* Destination must be a register.  */
  if (GET_CODE (operands [0]) != REG)
    abort();
  if (! conditional_move_operand (operands [2], SImode))
    abort();
  if (! conditional_move_operand (operands [3], SImode))
    abort();
      
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
   rtl expressions are different. */
int
m32r_not_same_reg (a, b)
     rtx a;
     rtx b;
{
  int reg_a = -1;
  int reg_b = -2;
  
  while (GET_CODE (a) == SUBREG)
    a = SUBREG_REG (a);
  
  if (GET_CODE (a) == REG)
    reg_a = REGNO (a);
  
  while (GET_CODE (b) == SUBREG)
    b = SUBREG_REG (b);
  
  if (GET_CODE (b) == REG)
    reg_b = REGNO (b);
  
  return reg_a != reg_b;
}


/* Use a library function to move some bytes.  */
static void
block_move_call (dest_reg, src_reg, bytes_rtx)
     rtx dest_reg;
     rtx src_reg;
     rtx bytes_rtx;
{
  /* We want to pass the size as Pmode, which will normally be SImode
     but will be DImode if we are using 64 bit longs and pointers.  */
  if (GET_MODE (bytes_rtx) != VOIDmode
      && GET_MODE (bytes_rtx) != Pmode)
    bytes_rtx = convert_to_mode (Pmode, bytes_rtx, 1);

#ifdef TARGET_MEM_FUNCTIONS
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
		     VOIDmode, 3, dest_reg, Pmode, src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype), bytes_rtx,
				      TREE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
#else
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"), 0,
		     VOIDmode, 3, src_reg, Pmode, dest_reg, Pmode,
		     convert_to_mode (TYPE_MODE (integer_type_node), bytes_rtx,
				      TREE_UNSIGNED (integer_type_node)),
		     TYPE_MODE (integer_type_node));
#endif
}

/* The maximum number of bytes to copy using pairs of load/store instructions.
   If a block is larger than this then a loop will be generated to copy
   MAX_MOVE_BYTES chunks at a time.  The value of 32 is a semi-arbitary choice.
   A customer uses Dhrystome as their benchmark, and Dhrystone has a 31 byte
   string copy in it.  */
#define MAX_MOVE_BYTES 32

/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

void
m32r_expand_block_move (operands)
     rtx operands[];
{
  rtx           orig_dst  = operands[0];
  rtx           orig_src  = operands[1];
  rtx           bytes_rtx = operands[2];
  rtx           align_rtx = operands[3];
  int           constp    = GET_CODE (bytes_rtx) == CONST_INT;
  HOST_WIDE_INT bytes     = constp ? INTVAL (bytes_rtx) : 0;
  int           align     = INTVAL (align_rtx);
  int           leftover;
  rtx           src_reg;
  rtx           dst_reg;

  if (constp && bytes <= 0)
    return;

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
      return;
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

      /* If we are going to have to perform this loop more than
	 once, then generate a label and compute the address the
	 source register will contain upon completion of the final
	 itteration.  */
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
      emit_insn (gen_movstrsi_internal (dst_reg, src_reg, at_a_time));
      emit_insn (gen_addsi3 (dst_reg, dst_reg, GEN_INT (4)));
      
      if (bytes > MAX_MOVE_BYTES)
	{
	  emit_insn (gen_cmpsi (src_reg, final_src));
	  emit_jump_insn (gen_bne (label));
	}
    }

  if (leftover)
    emit_insn (gen_movstrsi_internal (dst_reg, src_reg, GEN_INT (leftover)));
}


/* Emit load/stores for a small constant word aligned block_move. 

   operands[0] is the memory address of the destination.
   operands[1] is the memory address of the source.
   operands[2] is the number of bytes to move.
   operands[3] is a temp register.
   operands[4] is a temp register.  */

void
m32r_output_block_move (insn, operands)
     rtx insn ATTRIBUTE_UNUSED;
     rtx operands[];
{
  HOST_WIDE_INT bytes = INTVAL (operands[2]);
  int		first_time;
  int		got_extra = 0;
  
  if (bytes < 1 || bytes > MAX_MOVE_BYTES)
    abort ();
  
  /* We do not have a post-increment store available, so the first set of
     stores are done without any increment, then the remaining ones can use
     the pre-increment addressing mode.
     
     Note: expand_block_move() also relies upon this behaviour when building
     loops to copy large blocks.  */
  first_time = 1;
  
  while (bytes > 0)
    {
      if (bytes >= 8)
	{
	  if (first_time)
	    {
	      output_asm_insn ("ld\t%3, %p1", operands);
	      output_asm_insn ("ld\t%4, %p1", operands);
	      output_asm_insn ("st\t%3, @%0", operands);
	      output_asm_insn ("st\t%4, %s0", operands);
	    }
	  else
	    {
	      output_asm_insn ("ld\t%3, %p1", operands);
	      output_asm_insn ("ld\t%4, %p1", operands);
	      output_asm_insn ("st\t%3, %s0", operands);
	      output_asm_insn ("st\t%4, %s0", operands);
	    }

	  bytes -= 8;
	}
      else if (bytes >= 4)
	{
	  if (bytes > 4)
	    got_extra = 1;
	  
	  output_asm_insn ("ld\t%3, %p1", operands);
	  
	  if (got_extra)
	    output_asm_insn ("ld\t%4, %p1", operands);
		
	  if (first_time)
	    output_asm_insn ("st\t%3, @%0", operands);
	  else
	    output_asm_insn ("st\t%3, %s0", operands);

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
	  int last_shift;
	  rtx my_operands[3];

	  /* If got_extra is true then we have already loaded
	     the next word as part of loading and storing the previous word.  */
	  if (! got_extra)
	    output_asm_insn ("ld\t%4, @%1", operands);

	  if (bytes >= 2)
	    {
	      bytes -= 2;

	      output_asm_insn ("sra3\t%3, %4, #16", operands);
	      my_operands[0] = operands[3];
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
	      my_operands[0] = operands[4];
	      my_operands[1] = GEN_INT (last_shift);
	      output_asm_insn ("srai\t%0, #%1", my_operands);
	      my_operands[0] = operands[4];
	      my_operands[1] = GEN_INT (dst_offset);
	      my_operands[2] = operands[0];
	      output_asm_insn ("stb\t%0, @(%1,%2)", my_operands);
	    }
	  
	  bytes = 0;
	}

      first_time = 0;
    }
}

/* Return true if op is an integer constant, less than or equal to
   MAX_MOVE_BYTES.  */
int
m32r_block_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST_INT
      || INTVAL (op) > MAX_MOVE_BYTES
      || INTVAL (op) <= 0)
    return 0;

  return 1;
}
