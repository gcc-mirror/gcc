/* Printing of RTL in "slim", mnemonic like form.
   Copyright (C) 1992-2013 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

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

/* Historically this form of RTL dumping was introduced along with
   the Haifa instruction scheduling pass, hence the name of this file.
   But there is nothing in this file left that is scheduler-specific.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"	/* FIXME: To dump INSN_VAR_LOCATION_DECL.  */
#include "basic-block.h"
#include "dumpfile.h"	/* for the TDF_* flags */
#include "pretty-print.h"

/* The functions in this file try to print RTL in a form resembling assembler
   mnemonics.  Because this form is more concise than the "traditional" form
   of RTL printing in Lisp-style, the form printed by this file is called
   "slim".  RTL dumps in slim format can be obtained by appending the "-slim"
   option to -fdump-rtl-<pass>.  Control flow graph output as a DOT file is
   always printed in slim form.

   The normal interface to the functionality provided in this pretty-printer
   is through the dump_*_slim functions to print to a stream, or via the
   print_*_slim functions to print into a user's pretty-printer.
   
   It is also possible to obtain a string for a single pattern as a string
   pointer, via str_pattern_slim, but this usage is discouraged.  */

/* For insns we print patterns, and for some patterns we print insns...  */
static void print_insn_with_notes (pretty_printer *, const_rtx);

/* This recognizes rtx'en classified as expressions.  These are always
   represent some action on values or results of other expression, that
   may be stored in objects representing values.  */

static void
print_exp (pretty_printer *pp, const_rtx x, int verbose)
{
  const char *st[4];
  const char *fun;
  rtx op[4];
  int i;

  fun = (char *) 0;
  for (i = 0; i < 4; i++)
    {
      st[i] = (char *) 0;
      op[i] = NULL_RTX;
    }

  switch (GET_CODE (x))
    {
    case PLUS:
      op[0] = XEXP (x, 0);
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  st[1] = "-";
	  op[1] = GEN_INT (-INTVAL (XEXP (x, 1)));
	}
      else
	{
	  st[1] = "+";
	  op[1] = XEXP (x, 1);
	}
      break;
    case LO_SUM:
      op[0] = XEXP (x, 0);
      st[1] = "+low(";
      op[1] = XEXP (x, 1);
      st[2] = ")";
      break;
    case MINUS:
      op[0] = XEXP (x, 0);
      st[1] = "-";
      op[1] = XEXP (x, 1);
      break;
    case COMPARE:
      fun = "cmp";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NEG:
      st[0] = "-";
      op[0] = XEXP (x, 0);
      break;
    case FMA:
      st[0] = "{";
      op[0] = XEXP (x, 0);
      st[1] = "*";
      op[1] = XEXP (x, 1);
      st[2] = "+";
      op[2] = XEXP (x, 2);
      st[3] = "}";
      break;
    case MULT:
      op[0] = XEXP (x, 0);
      st[1] = "*";
      op[1] = XEXP (x, 1);
      break;
    case DIV:
      op[0] = XEXP (x, 0);
      st[1] = "/";
      op[1] = XEXP (x, 1);
      break;
    case UDIV:
      fun = "udiv";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case MOD:
      op[0] = XEXP (x, 0);
      st[1] = "%";
      op[1] = XEXP (x, 1);
      break;
    case UMOD:
      fun = "umod";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMIN:
      fun = "smin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMAX:
      fun = "smax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMIN:
      fun = "umin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMAX:
      fun = "umax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NOT:
      st[0] = "!";
      op[0] = XEXP (x, 0);
      break;
    case AND:
      op[0] = XEXP (x, 0);
      st[1] = "&";
      op[1] = XEXP (x, 1);
      break;
    case IOR:
      op[0] = XEXP (x, 0);
      st[1] = "|";
      op[1] = XEXP (x, 1);
      break;
    case XOR:
      op[0] = XEXP (x, 0);
      st[1] = "^";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFT:
      op[0] = XEXP (x, 0);
      st[1] = "<<";
      op[1] = XEXP (x, 1);
      break;
    case LSHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = " 0>>";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = ">>";
      op[1] = XEXP (x, 1);
      break;
    case ROTATE:
      op[0] = XEXP (x, 0);
      st[1] = "<-<";
      op[1] = XEXP (x, 1);
      break;
    case ROTATERT:
      op[0] = XEXP (x, 0);
      st[1] = ">->";
      op[1] = XEXP (x, 1);
      break;
    case NE:
      op[0] = XEXP (x, 0);
      st[1] = "!=";
      op[1] = XEXP (x, 1);
      break;
    case EQ:
      op[0] = XEXP (x, 0);
      st[1] = "==";
      op[1] = XEXP (x, 1);
      break;
    case GE:
      op[0] = XEXP (x, 0);
      st[1] = ">=";
      op[1] = XEXP (x, 1);
      break;
    case GT:
      op[0] = XEXP (x, 0);
      st[1] = ">";
      op[1] = XEXP (x, 1);
      break;
    case LE:
      op[0] = XEXP (x, 0);
      st[1] = "<=";
      op[1] = XEXP (x, 1);
      break;
    case LT:
      op[0] = XEXP (x, 0);
      st[1] = "<";
      op[1] = XEXP (x, 1);
      break;
    case SIGN_EXTRACT:
      fun = (verbose) ? "sign_extract" : "sxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case ZERO_EXTRACT:
      fun = (verbose) ? "zero_extract" : "zxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case SIGN_EXTEND:
      fun = (verbose) ? "sign_extend" : "sxn";
      op[0] = XEXP (x, 0);
      break;
    case ZERO_EXTEND:
      fun = (verbose) ? "zero_extend" : "zxn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_EXTEND:
      fun = (verbose) ? "float_extend" : "fxn";
      op[0] = XEXP (x, 0);
      break;
    case TRUNCATE:
      fun = (verbose) ? "trunc" : "trn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_TRUNCATE:
      fun = (verbose) ? "float_trunc" : "ftr";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT:
      fun = (verbose) ? "float" : "flt";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FLOAT:
      fun = (verbose) ? "uns_float" : "ufl";
      op[0] = XEXP (x, 0);
      break;
    case FIX:
      fun = "fix";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FIX:
      fun = (verbose) ? "uns_fix" : "ufx";
      op[0] = XEXP (x, 0);
      break;
    case PRE_DEC:
      st[0] = "--";
      op[0] = XEXP (x, 0);
      break;
    case PRE_INC:
      st[0] = "++";
      op[0] = XEXP (x, 0);
      break;
    case POST_DEC:
      op[0] = XEXP (x, 0);
      st[1] = "--";
      break;
    case POST_INC:
      op[0] = XEXP (x, 0);
      st[1] = "++";
      break;
    case PRE_MODIFY:
      st[0] = "pre ";
      op[0] = XEXP (XEXP (x, 1), 0);
      st[1] = "+=";
      op[1] = XEXP (XEXP (x, 1), 1);
      break;
    case POST_MODIFY:
      st[0] = "post ";
      op[0] = XEXP (XEXP (x, 1), 0);
      st[1] = "+=";
      op[1] = XEXP (XEXP (x, 1), 1);
      break;
    case CALL:
      st[0] = "call ";
      op[0] = XEXP (x, 0);
      if (verbose)
	{
	  st[1] = " argc:";
	  op[1] = XEXP (x, 1);
	}
      break;
    case IF_THEN_ELSE:
      st[0] = "{(";
      op[0] = XEXP (x, 0);
      st[1] = ")?";
      op[1] = XEXP (x, 1);
      st[2] = ":";
      op[2] = XEXP (x, 2);
      st[3] = "}";
      break;
    case TRAP_IF:
      fun = "trap_if";
      op[0] = TRAP_CONDITION (x);
      break;
    case PREFETCH:
      fun = "prefetch";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case UNSPEC:
    case UNSPEC_VOLATILE:
      {
	pp_string (pp, "unspec");
	if (GET_CODE (x) == UNSPEC_VOLATILE)
	  pp_string (pp, "/v");
	pp_left_bracket (pp);
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    if (i != 0)
	      pp_comma (pp);
	    print_pattern (pp, XVECEXP (x, 0, i), verbose);
	  }
	pp_string (pp, "] ");
	pp_decimal_int (pp, XINT (x, 1));
      }
      break;
    default:
      {
	/* Most unhandled codes can be printed as pseudo-functions.  */
        if (GET_RTX_CLASS (GET_CODE (x)) == RTX_UNARY)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	  }
        else if (GET_RTX_CLASS (GET_CODE (x)) == RTX_COMPARE
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_COMM_COMPARE
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_BIN_ARITH
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_COMM_ARITH)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	    op[1] = XEXP (x, 1);
	  }
        else if (GET_RTX_CLASS (GET_CODE (x)) == RTX_TERNARY)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	    op[1] = XEXP (x, 1);
	    op[2] = XEXP (x, 2);
	  }
	else
	  /* Give up, just print the RTX name.  */
	  st[0] = GET_RTX_NAME (GET_CODE (x));
      }
      break;
    }

  /* Print this as a function?  */
  if (fun)
    {
      pp_string (pp, fun);
      pp_left_paren (pp);
    }

  for (i = 0; i < 4; i++)
    {
      if (st[i])
        pp_string (pp, st[i]);

      if (op[i])
	{
	  if (fun && i != 0)
	    pp_comma (pp);
	  print_value (pp, op[i], verbose);
	}
    }

  if (fun)
    pp_right_paren (pp);
}		/* print_exp */

/* Prints rtxes, I customarily classified as values.  They're constants,
   registers, labels, symbols and memory accesses.  */

void
print_value (pretty_printer *pp, const_rtx x, int verbose)
{
  char tmp[1024];

  if (!x)
    {
      pp_string (pp, "(nil)");
      return;
    }
  switch (GET_CODE (x))
    {
    case CONST_INT:
      pp_scalar (pp, HOST_WIDE_INT_PRINT_HEX,
		 (unsigned HOST_WIDE_INT) INTVAL (x));
      break;
    case CONST_DOUBLE:
      if (FLOAT_MODE_P (GET_MODE (x)))
	{
	  real_to_decimal (tmp, CONST_DOUBLE_REAL_VALUE (x),
			   sizeof (tmp), 0, 1);
	  pp_string (pp, tmp);
	}
      else
	pp_printf (pp, "<%wx,%wx>",
		   (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (x),
		   (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (x));
      break;
    case CONST_FIXED:
      fixed_to_decimal (tmp, CONST_FIXED_VALUE (x), sizeof (tmp));
      pp_string (pp, tmp);
      break;
    case CONST_STRING:
      pp_printf (pp, "\"%s\"", XSTR (x, 0));
      break;
    case SYMBOL_REF:
      pp_printf (pp, "`%s'", XSTR (x, 0));
      break;
    case LABEL_REF:
      pp_printf (pp, "L%d", INSN_UID (XEXP (x, 0)));
      break;
    case CONST:
    case HIGH:
    case STRICT_LOW_PART:
      pp_printf (pp, "%s(", GET_RTX_NAME (GET_CODE (x)));
      print_value (pp, XEXP (x, 0), verbose);
      pp_right_paren (pp);
      break;
    case REG:
      if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	{
	  if (ISDIGIT (reg_names[REGNO (x)][0]))
	    pp_modulo (pp);
	  pp_string (pp, reg_names[REGNO (x)]);
	}
      else
	pp_printf (pp, "r%d", REGNO (x));
      if (verbose)
	pp_printf (pp, ":%s", GET_MODE_NAME (GET_MODE (x)));
      break;
    case SUBREG:
      print_value (pp, SUBREG_REG (x), verbose);
      pp_printf (pp, "#%d", SUBREG_BYTE (x));
      break;
    case SCRATCH:
    case CC0:
    case PC:
      pp_string (pp, GET_RTX_NAME (GET_CODE (x)));
      break;
    case MEM:
      pp_left_bracket (pp);
      print_value (pp, XEXP (x, 0), verbose);
      pp_right_bracket (pp);
      break;
    case DEBUG_EXPR:
      pp_printf (pp, "D#%i", DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x)));
      break;
    default:
      print_exp (pp, x, verbose);
      break;
    }
}				/* print_value */

/* The next step in insn detalization, its pattern recognition.  */

void
print_pattern (pretty_printer *pp, const_rtx x, int verbose)
{
  if (! x)
    {
      pp_string (pp, "(nil)");
      return;
    }

  switch (GET_CODE (x))
    {
    case SET:
      print_value (pp, SET_DEST (x), verbose);
      pp_equal (pp);
      print_value (pp, SET_SRC (x), verbose);
      break;
    case RETURN:
    case SIMPLE_RETURN:
    case EH_RETURN:
      pp_string (pp, GET_RTX_NAME (GET_CODE (x)));
      break;
    case CALL:
      print_exp (pp, x, verbose);
      break;
    case CLOBBER:
    case USE:
      pp_printf (pp, "%s ", GET_RTX_NAME (GET_CODE (x)));
      print_value (pp, XEXP (x, 0), verbose);
      break;
    case VAR_LOCATION:
      pp_string (pp, "loc ");
      print_value (pp, PAT_VAR_LOCATION_LOC (x), verbose);
      break;
    case COND_EXEC:
      pp_left_paren (pp);
      if (GET_CODE (COND_EXEC_TEST (x)) == NE
	  && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
	print_value (pp, XEXP (COND_EXEC_TEST (x), 0), verbose);
      else if (GET_CODE (COND_EXEC_TEST (x)) == EQ
	       && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
	{
	  pp_exclamation (pp);
	  print_value (pp, XEXP (COND_EXEC_TEST (x), 0), verbose);
	}
      else
	print_value (pp, COND_EXEC_TEST (x), verbose);
      pp_string (pp, ") ");
      print_pattern (pp, COND_EXEC_CODE (x), verbose);
      break;
    case PARALLEL:
      {
	int i;

	pp_left_brace (pp);
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (pp, XVECEXP (x, 0, i), verbose);
	    pp_semicolon (pp);
	  }
	pp_right_brace (pp);
      }
      break;
    case SEQUENCE:
      {
	pp_string (pp, "sequence{");
	if (INSN_P (XVECEXP (x, 0, 0)))
	  {
	    /* Print the sequence insns indented.  */
	    const char * save_print_rtx_head = print_rtx_head;
	    char indented_print_rtx_head[32];

	    pp_newline (pp);
	    gcc_assert (strlen (print_rtx_head) < sizeof (indented_print_rtx_head) - 4);
	    snprintf (indented_print_rtx_head,
		      sizeof (indented_print_rtx_head),
		      "%s     ", print_rtx_head);
	    print_rtx_head = indented_print_rtx_head;
	    for (int i = 0; i < XVECLEN (x, 0); i++)
	      print_insn_with_notes (pp, XVECEXP (x, 0, i));
	    pp_printf (pp, "%s      ", save_print_rtx_head);
	    print_rtx_head = save_print_rtx_head;
	  }
	else
	  {
	    for (int i = 0; i < XVECLEN (x, 0); i++)
	      {
		print_pattern (pp, XVECEXP (x, 0, i), verbose);
		pp_semicolon (pp);
	      }
	  }
	pp_right_brace (pp);
      }
      break;
    case ASM_INPUT:
      pp_printf (pp, "asm {%s}", XSTR (x, 0));
      break;
    case ADDR_VEC:
      /* Fall through.  */
    case ADDR_DIFF_VEC:
      print_value (pp, XEXP (x, 0), verbose);
      break;
    case TRAP_IF:
      pp_string (pp, "trap_if ");
      print_value (pp, TRAP_CONDITION (x), verbose);
      break;
    case UNSPEC:
    case UNSPEC_VOLATILE:
      /* Fallthru -- leave UNSPECs to print_exp.  */
    default:
      print_value (pp, x, verbose);
    }
}				/* print_pattern */

/* This is the main function in slim rtl visualization mechanism.

   X is an insn, to be printed into PP.

   This function tries to print it properly in human-readable form,
   resembling assembler mnemonics (instead of the older Lisp-style
   form).

   If VERBOSE is TRUE, insns are printed with more complete (but
   longer) pattern names and with extra information, and prefixed
   with their INSN_UIDs.  */

void
print_insn (pretty_printer *pp, const_rtx x, int verbose)
{
  if (verbose)
    {
      /* Blech, pretty-print can't print integers with a specified width.  */
      char uid_prefix[32];
      snprintf (uid_prefix, sizeof uid_prefix, " %4d: ", INSN_UID (x));
      pp_string (pp, uid_prefix);
    }

  switch (GET_CODE (x))
    {
    case INSN:
      print_pattern (pp, PATTERN (x), verbose);
      break;

    case DEBUG_INSN:
      {
	const char *name = "?";

	if (DECL_P (INSN_VAR_LOCATION_DECL (x)))
	  {
	    tree id = DECL_NAME (INSN_VAR_LOCATION_DECL (x));
	    char idbuf[32];
	    if (id)
	      name = IDENTIFIER_POINTER (id);
	    else if (TREE_CODE (INSN_VAR_LOCATION_DECL (x))
		     == DEBUG_EXPR_DECL)
	      {
		sprintf (idbuf, "D#%i",
			 DEBUG_TEMP_UID (INSN_VAR_LOCATION_DECL (x)));
		name = idbuf;
	      }
	    else
	      {
		sprintf (idbuf, "D.%i",
			 DECL_UID (INSN_VAR_LOCATION_DECL (x)));
		name = idbuf;
	      }
	  }
	pp_printf (pp, "debug %s => ", name);
	if (VAR_LOC_UNKNOWN_P (INSN_VAR_LOCATION_LOC (x)))
	  pp_string (pp, "optimized away");
	else
	  print_pattern (pp, INSN_VAR_LOCATION_LOC (x), verbose);
      }
      break;

    case JUMP_INSN:
      print_pattern (pp, PATTERN (x), verbose);
      break;
    case CALL_INSN:
      if (GET_CODE (PATTERN (x)) == PARALLEL)
        print_pattern (pp, XVECEXP (PATTERN (x), 0, 0), verbose);
      else
	print_pattern (pp, PATTERN (x), verbose);
      break;
    case CODE_LABEL:
      pp_printf (pp, "L%d:", INSN_UID (x));
      break;
    case JUMP_TABLE_DATA:
      pp_string (pp, "jump_table_data{\n");
      print_pattern (pp, PATTERN (x), verbose);
      pp_right_brace (pp);
      break;
    case BARRIER:
      pp_string (pp, "barrier");
      break;
    case NOTE:
      {
	pp_string (pp, GET_NOTE_INSN_NAME (NOTE_KIND (x)));
	switch (NOTE_KIND (x))
	  {
	  case NOTE_INSN_EH_REGION_BEG:
	  case NOTE_INSN_EH_REGION_END:
	    pp_printf (pp, " %d", NOTE_EH_HANDLER (x));
	    break;

	  case NOTE_INSN_BLOCK_BEG:
	  case NOTE_INSN_BLOCK_END:
	    pp_printf (pp, " %d", BLOCK_NUMBER (NOTE_BLOCK (x)));
	    break;

	  case NOTE_INSN_BASIC_BLOCK:
	    pp_printf (pp, " %d", NOTE_BASIC_BLOCK (x)->index);
	    break;

	  case NOTE_INSN_DELETED_LABEL:
	  case NOTE_INSN_DELETED_DEBUG_LABEL:
	    {
	      const char *label = NOTE_DELETED_LABEL_NAME (x);
	      if (label == NULL)
		label = "";
	      pp_printf (pp, " (\"%s\")", label);
	    }
	    break;

	  case NOTE_INSN_VAR_LOCATION:
	  case NOTE_INSN_CALL_ARG_LOCATION:
	    pp_left_brace (pp);
	    print_pattern (pp, NOTE_VAR_LOCATION (x), verbose);
	    pp_right_brace (pp);
	    break;

	  default:
	    break;
	  }
	break;
      }
    default:
      gcc_unreachable ();
    }
}				/* print_insn */

/* Pretty-print a slim dump of X (an insn) to PP, including any register
   note attached to the instruction.  */

static void
print_insn_with_notes (pretty_printer *pp, const_rtx x)
{
  pp_string (pp, print_rtx_head);
  print_insn (pp, x, 1);
  pp_newline (pp);
  if (INSN_P (x) && REG_NOTES (x))
    for (rtx note = REG_NOTES (x); note; note = XEXP (note, 1))
      {
	pp_printf (pp, "%s      %s ", print_rtx_head,
		   GET_REG_NOTE_NAME (REG_NOTE_KIND (note)));
	if (GET_CODE (note) == INT_LIST)
	  pp_printf (pp, "%d", XINT (note, 0));
	else
	  print_pattern (pp, XEXP (note, 0), 1);
	pp_newline (pp);
      }
}

/* Print X, an RTL value node, to file F in slim format.  Include
   additional information if VERBOSE is nonzero.

   Value nodes are constants, registers, labels, symbols and
   memory.  */

void
dump_value_slim (FILE *f, const_rtx x, int verbose)
{
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.buffer->stream = f;
  print_value (&rtl_slim_pp, x, verbose);
  pp_flush (&rtl_slim_pp);
}

/* Emit a slim dump of X (an insn) to the file F, including any register
   note attached to the instruction.  */
void
dump_insn_slim (FILE *f, const_rtx x)
{
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.buffer->stream = f;
  print_insn_with_notes (&rtl_slim_pp, x);
  pp_flush (&rtl_slim_pp);
}

/* Same as above, but stop at LAST or when COUNT == 0.
   If COUNT < 0 it will stop only at LAST or NULL rtx.  */

void
dump_rtl_slim (FILE *f, const_rtx first, const_rtx last,
	       int count, int flags ATTRIBUTE_UNUSED)
{
  const_rtx insn, tail;
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.buffer->stream = f;

  tail = last ? NEXT_INSN (last) : NULL_RTX;
  for (insn = first;
       (insn != NULL) && (insn != tail) && (count != 0);
       insn = NEXT_INSN (insn))
    {
      print_insn_with_notes (&rtl_slim_pp, insn);
      if (count > 0)
        count--;
    }

  pp_flush (&rtl_slim_pp);
}

/* Dumps basic block BB to pretty-printer PP in slim form and without and
   no indentation, for use as a label of a DOT graph record-node.  */

void
rtl_dump_bb_for_graph (pretty_printer *pp, basic_block bb)
{
  rtx insn;
  bool first = true;

  /* TODO: inter-bb stuff.  */
  FOR_BB_INSNS (bb, insn)
    {
      if (! first)
	{
	  pp_bar (pp);
	  pp_write_text_to_stream (pp);
	}
      first = false;
      print_insn_with_notes (pp, insn);
      pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);
    }
}

/* Pretty-print pattern X of some insn in non-verbose mode.
   Return a string pointer to the pretty-printer buffer.

   This function is only exported exists only to accommodate some older users
   of the slim RTL pretty printers.  Please do not use it for new code.  */

const char *
str_pattern_slim (const_rtx x)
{
  pretty_printer rtl_slim_pp;
  print_pattern (&rtl_slim_pp, x, 0);
  return ggc_strdup (pp_formatted_text (&rtl_slim_pp));
}

/* Emit a slim dump of X (an insn) to stderr.  */
extern void debug_insn_slim (const_rtx);
DEBUG_FUNCTION void
debug_insn_slim (const_rtx x)
{
  dump_insn_slim (stderr, x);
}

/* Same as above, but using dump_rtl_slim.  */
extern void debug_rtl_slim (FILE *, const_rtx, const_rtx, int, int);
DEBUG_FUNCTION void
debug_rtl_slim (const_rtx first, const_rtx last, int count, int flags)
{
  dump_rtl_slim (stderr, first, last, count, flags);
}

extern void debug_bb_slim (basic_block);
DEBUG_FUNCTION void
debug_bb_slim (basic_block bb)
{
  dump_bb (stderr, bb, 0, TDF_SLIM | TDF_BLOCKS);
}

extern void debug_bb_n_slim (int);
DEBUG_FUNCTION void
debug_bb_n_slim (int n)
{
  basic_block bb = BASIC_BLOCK (n);
  debug_bb_slim (bb);
}

