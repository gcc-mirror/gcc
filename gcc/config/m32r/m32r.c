/* Subroutines used for code generation on the M32R/D cpu.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "expr.h"
#include "recog.h"

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */
rtx m32r_compare_op0, m32r_compare_op1;

/* Array of valid operand punctuation characters.  */
char m32r_punct_chars[256];

static void init_reg_tables ();

/* Selected code model.  */
char *m32r_model_string = M32R_MODEL_DEFAULT;
enum m32r_model m32r_model;

/* Selected SDA support.  */
char *m32r_sdata_string = M32R_SDATA_DEFAULT;
enum m32r_sdata m32r_sdata;

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

enum m32r_mode_class {
  C_MODE,
  S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE
};

/* Modes for condition codes.  */
#define C_MODES (1 << (int) C_MODE)

/* Modes for single-word and smaller quantities.  */
#define S_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << DF_MODE))

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Value is 1 if register/mode pair is acceptable on arc.  */

unsigned int m32r_hard_regno_mode_ok[FIRST_PSEUDO_REGISTER] = {
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, T_MODES,
  T_MODES, T_MODES, T_MODES, T_MODES, T_MODES, S_MODES, S_MODES, S_MODES,
  S_MODES, C_MODES
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

/* Return nonzero if IDENTIFIER is a valid decl attribute.  */

int
m32r_valid_machine_decl_attribute (type, attributes, identifier, args)
     tree type;
     tree attributes;
     tree identifier;
     tree args;
{
  static tree interrupt_ident, model_ident;
  static tree small_ident, medium_ident, large_ident;

  if (interrupt_ident == 0)
    {
      interrupt_ident = get_identifier ("__interrupt__");
      model_ident = get_identifier ("__model__");
      small_ident = get_identifier ("__small__");
      medium_ident = get_identifier ("__medium__");
      large_ident = get_identifier ("__large__");
    }

  if (identifier == interrupt_ident
      && list_length (args) == 0)
    return 1;

  if (identifier == model_ident
      && list_length (args) == 1
      && (TREE_VALUE (args) == small_ident
	  || TREE_VALUE (args) == medium_ident
	  || TREE_VALUE (args) == large_ident))
    return 1;

  return 0;
}

/* Return zero if TYPE1 and TYPE are incompatible, one if they are compatible,
   and two if they are nearly compatible (which causes a warning to be
   generated).  */

int
m32r_comp_type_attributes (type1, type2)
     tree type1, type2;
{
  return 1;
}

/* Set the default attributes for TYPE.  */

void
m32r_set_default_type_attributes (type)
     tree type;
{
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
      model = lookup_attribute ("model", DECL_MACHINE_ATTRIBUTES (decl));
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
	  char *name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
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
	  if (TREE_VALUE (TREE_VALUE (model)) == get_identifier ("__small__"))
	    ; /* don't mark the symbol specially */
	  else if (TREE_VALUE (TREE_VALUE (model)) == get_identifier ("__medium__"))
	    prefix = MEDIUM_FLAG_CHAR;
	  else if (TREE_VALUE (TREE_VALUE (model)) == get_identifier ("__large__"))
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
      char *str = XSTR (XEXP (rtl, 0), 0);
      int len = strlen (str);
      char *newstr = savealloc (len + 2);
      strcpy (newstr + 1, str);
      *newstr = prefix;
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
  return (symbolic_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && LEGITIMATE_CONSTANT_P (op))
	  || (GET_CODE (op) == REG));
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
     enum machine_mode mode;
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

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if OP is a reference to an object in .sdata/.sbss.  */

int
small_data_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
     enum machine_mode mode;
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
    return (! addr24_operand (op)
	    && ! small_data_operand (op));

  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT)
    {
      return (! addr24_operand (op)
	      && ! small_data_operand (op));
    }

  return 0;
}

/* Return 1 if OP is a function that can be called with the `bl' insn.  */

int
call26_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SYMBOL_REF)
    return ! LARGE_NAME_P (XSTR (op, 0));

  return TARGET_CALL26;
}

/* Return 1 if OP is a function that must be called with 32 bit addressing.  */

int
call32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ! call26_operand (op, mode);
}

/* Returns 1 if OP is an acceptable operand for seth/add3.  */

int
seth_add3_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return INT8_P (INTVAL (op));
}

/* Return true if OP is a signed 16 bit immediate value.  */

int
int16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return INT16_P (INTVAL (op));
}

/* Return true if OP is a signed 16 bit immediate value
   useful in comparisons.  */

int
cmp_int16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return CMP_INT16_P (INTVAL (op));
}

/* Return true if OP is an unsigned 16 bit immediate value.  */

int
uint16_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return UINT16_P (INTVAL (op));
}

/* Return true if OP is an unsigned 24 bit immediate value.  */

int
uint24_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return UINT24_P (INTVAL (op));
}

/* Return true if OP is a register or signed 8 bit value.  */

int
reg_or_int8_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return INT8_P (INTVAL (op));
}

/* Return true if OP is a register or signed 8 bit value.  */

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

/* Return true if OP is a register or signed 8 bit value.  */

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

/* Return true if OP is a const_int requiring two instructions to load.  */

int
two_insn_const_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
      return INT32_P (INTVAL (op));
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
      if (mode == DFmode)
	return easy_df_const (op);
      else
	return easy_di_const (op);
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
    enum machine_mode mode;
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
    enum machine_mode mode;
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
     enum machine_mode mode;
{
  return GET_CODE (op) == MEM && GET_CODE (XEXP (op, 0)) == REG;
}

/* Comparisons.  */

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */

enum machine_mode
m32r_select_cc_mode (op, x, y)
     enum rtx_code op;
     rtx x, y;
{
  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for compare [arg0 of the if_then_else].  */

rtx
gen_compare (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  enum rtx_code compare_code, branch_code;
  rtx cc_reg = gen_rtx (REG, mode, CARRY_REGNUM);
  int swap_p = 0;

  switch (code)
    {
    case EQ: compare_code = EQ; branch_code = NE; break;
    case NE: compare_code = EQ; branch_code = EQ; break;
    case LT: compare_code = LT; branch_code = NE; break;
    case LE: compare_code = LT; branch_code = EQ; swap_p = 1; break;
    case GT: compare_code = LT; branch_code = NE; swap_p = 1; break;
    case GE: compare_code = LT; branch_code = EQ; break;
    case LTU: compare_code = LTU; branch_code = NE; break;
    case LEU: compare_code = LTU; branch_code = EQ; swap_p = 1; break;
    case GTU: compare_code = LTU; branch_code = NE; swap_p = 1; break;
    case GEU: compare_code = LTU; branch_code = EQ; break;
    }

  if (! TARGET_OLD_COMPARE)
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

  if (swap_p && CONSTANT_P (y))
    y = force_reg (GET_MODE (x), y);
  else if (CONSTANT_P (y))
    {
      int ok_const_p =
	(code == LTU || code == LEU || code == GTU || code == GEU)
	  ? uint16_operand (y, GET_MODE (y))
	  : reg_or_cmp_int16_operand (y, GET_MODE (y));
      if (! ok_const_p)
	y = force_reg (GET_MODE (x), y);
    }

  switch (compare_code)
    {
    case EQ :
      emit_insn (gen_cmp_eqsi_insn (swap_p ? y : x, swap_p ? x : y));
      break;
    case LT :
      emit_insn (gen_cmp_ltsi_insn (swap_p ? y : x, swap_p ? x : y));
      break;
    case LTU :
      emit_insn (gen_cmp_ltusi_insn (swap_p ? y : x, swap_p ? x : y));
      break;
    }

  return gen_rtx (branch_code, VOIDmode, cc_reg, CONST0_RTX (mode));
}

/* Implements the FUNCTION_ARG_PARTIAL_NREGS macro.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int ret;
  int size = (((mode == BLKmode && type)
	       ? int_size_in_bytes (type)
	       : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

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

      regblock = gen_rtx (MEM, BLKmode,
			  plus_constant (arg_pointer_rtx,
					 FIRST_PARM_OFFSET (0)));
      move_block_from_reg (first_reg_offset, regblock,
			   size, size * UNITS_PER_WORD);

      *pretend_size = (size * UNITS_PER_WORD);
    }
}

/* Cost functions.  */

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.

   This function is trivial at the moment.  This code doesn't live
   in m32r.h so it's easy to experiment.  */

int
m32r_address_cost (addr)
     rtx addr;
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
  fn_type = (lookup_attribute ("interrupt", DECL_MACHINE_ATTRIBUTES (current_function_decl)) != NULL_TREE
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
#define MUST_SAVE_RETURN_ADDR (regs_ever_live[RETURN_ADDR_REGNUM])

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   SIZE is the size needed for local variables.  */

unsigned int
m32r_compute_frame_size (size)
     int size;			/* # of var. bytes allocated.  */
{
  int regno;
  unsigned int total_size, var_size, args_size, pretend_size, extra_size;
  unsigned int reg_size;
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

/* Set up the stack and frame pointer (if desired) for the function.  */

void
m32r_output_function_prologue (file, size)
     FILE *file;
     int size;
{
  int regno;
  int total_size, frame_size;
  char *sp_str = reg_names[STACK_POINTER_REGNUM];
  char *fp_str = reg_names[FRAME_POINTER_REGNUM];
  unsigned int gmask = current_frame_info.gmask;
  enum m32r_function_type fn_type = m32r_compute_function_type (current_function_decl);

  /* If this is an interrupt handler, mark it as such.  */
  if (M32R_INTERRUPT_P (fn_type))
    {
      fprintf (file, "\t%s interrupt handler\n",
	       ASM_COMMENT_START);
    }

  /* This is only for the human reader.  */
  fprintf (file, "\t%s BEGIN PROLOGUE %s vars= %d, regs= %d, args= %d, extra= %d\n",
	   ASM_COMMENT_START, ASM_COMMENT_START,
	   current_frame_info.var_size,
	   current_frame_info.reg_size / 4,
	   current_frame_info.args_size,
	   current_frame_info.extra_size);

  total_size = (! current_frame_info.initialized
		? m32r_compute_frame_size (size)
		: current_frame_info.total_size);

  /* These cases shouldn't happen.  Catch them now.  */
  if (total_size == 0 && gmask)
    abort ();

#if 1
  /* Allocate space for register arguments if this is a variadic function.  */
  if (current_frame_info.pretend_size != 0)
    fprintf (file, "\taddi %s,%s%d\n",
	     sp_str, IMMEDIATE_PREFIX,
	     -current_frame_info.pretend_size);
#else
  /* If there are unnamed args in registers, save them.  */
  if (current_function_stdarg || current_function_varargs)
    {
      int i;
      fprintf (file, "\taddi %s,%s%d\n",
	       sp_str, IMMEDIATE_PREFIX,
	       - M32R_MAX_PARM_REGS * UNITS_PER_WORD);
      for (i = 0; i < M32R_MAX_PARM_REGS; ++i)
	fprintf (file, "\tst %s,@(sp,%d)\n",
		 reg_names[i], i * UNITS_PER_WORD);
    }
#endif

  /* Save any registers we need to and set up fp.  */

  if (current_frame_info.save_fp)
    fprintf (file, "\tpush %s\n", fp_str);

  gmask &= ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK);

  /* Save any needed call-saved regs (and call-used if this is an
     interrupt handler).  */
  for (regno = 0; regno <= M32R_MAX_INT_REGS; ++regno)
    {
      if ((gmask & (1 << regno)) != 0)
	fprintf (file, "\tpush %s\n", reg_names[regno]);
    }

  if (current_frame_info.save_lr)
    fprintf (file, "\tpush %s\n", reg_names[RETURN_ADDR_REGNUM]);

  /* Allocate the stack frame.  */
  frame_size = total_size - (current_frame_info.pretend_size
			     + current_frame_info.reg_size);
  if (frame_size == 0)
    ; /* nothing to do */
  else if (frame_size <= 128)
    fprintf (file, "\taddi %s,%s%d\n",
	     sp_str, IMMEDIATE_PREFIX, -frame_size);
  else if (frame_size <= 32768)
    fprintf (file, "\tadd3 %s,%s,%s%d\n",
	     sp_str, sp_str, IMMEDIATE_PREFIX, -frame_size);
  else
    fprintf (file, "\tld24 %s,%s%d\n\tsub %s,%s\n",
	     reg_names[PROLOGUE_TMP_REGNUM],
	     IMMEDIATE_PREFIX, frame_size,
	     sp_str, reg_names[PROLOGUE_TMP_REGNUM]);

  if (frame_pointer_needed)
    fprintf (file, "\tmv %s,%s\n", fp_str, sp_str);

  fprintf (file, "\t%s END PROLOGUE\n", ASM_COMMENT_START);
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs. */

void
m32r_output_function_epilogue (file, size)
     FILE *file;
     int size;
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
      unsigned int pretend_size = current_frame_info.pretend_size;
      unsigned int frame_size = total_size - pretend_size;
      unsigned int var_size = current_frame_info.var_size;
      unsigned int args_size = current_frame_info.args_size;
      unsigned int gmask = current_frame_info.gmask;
      int can_trust_sp_p = !current_function_calls_alloca;
      char *sp_str = reg_names[STACK_POINTER_REGNUM];
      char *fp_str = reg_names[FRAME_POINTER_REGNUM];

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
     rtx tramp, fnaddr, cxt;
{
}

/* Set the cpu type and print out other fancy things,
   at the top of the file.  */

void
m32r_asm_file_start (file)
     FILE *file;
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
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
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
	output_operand_lossage ("invalid operand to %R code");
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
	  fprintf (file, "0x%08lx",
		   code == 'L' ? INTVAL (first) : INTVAL (second));
	}
      else
	output_operand_lossage ("invalid operand to %H/%L code");
      return;

    case 'A' :
      {
	REAL_VALUE_TYPE d;
	char str[30];

	if (GET_CODE (x) != CONST_DOUBLE
	    || GET_MODE_CLASS (GET_MODE (x)) != MODE_FLOAT)
	  abort ();
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
	  output_operand_lossage ("invalid operand to %T/%B code");
	  return;
	}
      break;

    case 'U' :
      /* FIXME: wip */
      /* Output a load/store with update indicator if appropriate.  */
      if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fputs (".a", file);
	}
      else
	output_operand_lossage ("invalid operand to %U code");
      return;

    case 'N' :
      /* Print a constant value negated.  */
      if (GET_CODE (x) == CONST_INT)
	output_addr_const (file, GEN_INT (- INTVAL (x)));
      else
	output_operand_lossage ("invalid operand to %N code");
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
      fprintf (file, "@(");
      if (GET_CODE (XEXP (x, 0)) == PRE_INC)
	output_address (plus_constant (XEXP (XEXP (x, 0), 0),
				       GET_MODE_SIZE (GET_MODE (x))));
      else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	output_address (plus_constant (XEXP (XEXP (x, 0), 0),
				       - GET_MODE_SIZE (GET_MODE (x))));
      else
	output_address (XEXP (x, 0));
      fputc (')', file);
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
     FILE *file;
     rtx addr;
{
  register rtx base, index = 0;
  int offset = 0;

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
	    abort ();
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
	abort ();
      break;

    case LO_SUM :
      if (GET_CODE (XEXP (addr, 0)) != REG)
	abort ();
      if (small_data_operand (XEXP (addr, 1), VOIDmode))
	fputs ("sda(", file);
      else
	fputs ("low(", file);
      output_addr_const (file, XEXP (addr, 1));
      fputs ("),", file);
      fputs (reg_names[REGNO (XEXP (addr, 0))], file);
      break;

    case PRE_INC :
    case PRE_DEC :
      /* We shouldn't get here as we've lost the mode of the memory object
	 (which says how much to inc/dec by).  */
      abort ();
      break;

    default :
      output_addr_const (file, addr);
      break;
    }
}
