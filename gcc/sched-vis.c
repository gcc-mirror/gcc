/* Instruction scheduling pass.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2002 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-attr.h"
#include "sched-int.h"

#ifdef INSN_SCHEDULING
/* target_units bitmask has 1 for each unit in the cpu.  It should be
   possible to compute this variable from the machine description.
   But currently it is computed by examining the insn list.  Since
   this is only needed for visualization, it seems an acceptable
   solution.  (For understanding the mapping of bits to units, see
   definition of function_units[] in "insn-attrtab.c".)  */

static int target_units = 0;

static char *safe_concat PARAMS ((char *, char *, const char *));
static int get_visual_tbl_length PARAMS ((void));
static void print_exp PARAMS ((char *, rtx, int));
static void print_value PARAMS ((char *, rtx, int));
static void print_pattern PARAMS ((char *, rtx, int));
static void print_insn PARAMS ((char *, rtx, int));

/* Print names of units on which insn can/should execute, for debugging.  */

void
insn_print_units (insn)
     rtx insn;
{
  int i;
  int unit = insn_unit (insn);

  if (unit == -1)
    fprintf (sched_dump, "none");
  else if (unit >= 0)
    fprintf (sched_dump, "%s", function_units[unit].name);
  else
    {
      fprintf (sched_dump, "[");
      for (i = 0, unit = ~unit; unit; i++, unit >>= 1)
	if (unit & 1)
	  {
	    fprintf (sched_dump, "%s", function_units[i].name);
	    if (unit != 1)
	      fprintf (sched_dump, " ");
	  }
      fprintf (sched_dump, "]");
    }
}

/* MAX_VISUAL_LINES is the maximum number of lines in visualization table
   of a basic block.  If more lines are needed, table is splitted to two.
   n_visual_lines is the number of lines printed so far for a block.
   visual_tbl contains the block visualization info.
   vis_no_unit holds insns in a cycle that are not mapped to any unit.  */
#define MAX_VISUAL_LINES 100
#define INSN_LEN 30
int n_visual_lines;
static unsigned visual_tbl_line_length;
char *visual_tbl;
int n_vis_no_unit;
#define MAX_VISUAL_NO_UNIT 20
rtx vis_no_unit[MAX_VISUAL_NO_UNIT];

/* Finds units that are in use in this function.  Required only
   for visualization.  */

void
init_target_units ()
{
  rtx insn;
  int unit;

  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (! INSN_P (insn))
	continue;

      unit = insn_unit (insn);

      if (unit < 0)
	target_units |= ~unit;
      else
	target_units |= (1 << unit);
    }
}

/* Return the length of the visualization table.  */

static int
get_visual_tbl_length ()
{
  int unit, i;
  int n, n1;
  char *s;

  /* Compute length of one field in line.  */
  s = (char *) alloca (INSN_LEN + 6);
  sprintf (s, "  %33s", "uname");
  n1 = strlen (s);

  /* Compute length of one line.  */
  n = strlen (";; ");
  n += n1;
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	n += n1;
  n += n1;
  n += strlen ("\n") + 2;

  visual_tbl_line_length = n;

  /* Compute length of visualization string.  */
  return (MAX_VISUAL_LINES * n);
}

/* Init block visualization debugging info.  */

void
init_block_visualization ()
{
  strcpy (visual_tbl, "");
  n_visual_lines = 0;
  n_vis_no_unit = 0;
}

#define BUF_LEN 2048

static char *
safe_concat (buf, cur, str)
     char *buf;
     char *cur;
     const char *str;
{
  char *end = buf + BUF_LEN - 2;	/* Leave room for null.  */
  int c;

  if (cur > end)
    {
      *end = '\0';
      return end;
    }

  while (cur < end && (c = *str++) != '\0')
    *cur++ = c;

  *cur = '\0';
  return cur;
}

/* This recognizes rtx, I classified as expressions.  These are always
   represent some action on values or results of other expression, that
   may be stored in objects representing values.  */

static void
print_exp (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char tmp[BUF_LEN];
  const char *st[4];
  char *cur = buf;
  const char *fun = (char *) 0;
  const char *sep;
  rtx op[4];
  int i;

  for (i = 0; i < 4; i++)
    {
      st[i] = (char *) 0;
      op[i] = NULL_RTX;
    }

  switch (GET_CODE (x))
    {
    case PLUS:
      op[0] = XEXP (x, 0);
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
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
    case ABS:
      fun = "abs";
      op[0] = XEXP (x, 0);
      break;
    case SQRT:
      fun = "sqrt";
      op[0] = XEXP (x, 0);
      break;
    case FFS:
      fun = "ffs";
      op[0] = XEXP (x, 0);
      break;
    case EQ:
      op[0] = XEXP (x, 0);
      st[1] = "==";
      op[1] = XEXP (x, 1);
      break;
    case NE:
      op[0] = XEXP (x, 0);
      st[1] = "!=";
      op[1] = XEXP (x, 1);
      break;
    case GT:
      op[0] = XEXP (x, 0);
      st[1] = ">";
      op[1] = XEXP (x, 1);
      break;
    case GTU:
      fun = "gtu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case LT:
      op[0] = XEXP (x, 0);
      st[1] = "<";
      op[1] = XEXP (x, 1);
      break;
    case LTU:
      fun = "ltu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case GE:
      op[0] = XEXP (x, 0);
      st[1] = ">=";
      op[1] = XEXP (x, 1);
      break;
    case GEU:
      fun = "geu";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case LE:
      op[0] = XEXP (x, 0);
      st[1] = "<=";
      op[1] = XEXP (x, 1);
      break;
    case LEU:
      fun = "leu";
      op[0] = XEXP (x, 0);
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
	cur = safe_concat (buf, cur, "unspec");
	if (GET_CODE (x) == UNSPEC_VOLATILE)
	  cur = safe_concat (buf, cur, "/v");
	cur = safe_concat (buf, cur, "[");
	sep = "";
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (tmp, XVECEXP (x, 0, i), verbose);
	    cur = safe_concat (buf, cur, sep);
	    cur = safe_concat (buf, cur, tmp);
	    sep = ",";
	  }
	cur = safe_concat (buf, cur, "] ");
	sprintf (tmp, "%d", XINT (x, 1));
	cur = safe_concat (buf, cur, tmp);
      }
      break;
    default:
      /* If (verbose) debug_rtx (x);  */
      st[0] = GET_RTX_NAME (GET_CODE (x));
      break;
    }

  /* Print this as a function?  */
  if (fun)
    {
      cur = safe_concat (buf, cur, fun);
      cur = safe_concat (buf, cur, "(");
    }

  for (i = 0; i < 4; i++)
    {
      if (st[i])
	cur = safe_concat (buf, cur, st[i]);

      if (op[i])
	{
	  if (fun && i != 0)
	    cur = safe_concat (buf, cur, ",");

	  print_value (tmp, op[i], verbose);
	  cur = safe_concat (buf, cur, tmp);
	}
    }

  if (fun)
    cur = safe_concat (buf, cur, ")");
}		/* print_exp */

/* Prints rtxes, I customly classified as values.  They're constants,
   registers, labels, symbols and memory accesses.  */

static void
print_value (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t[BUF_LEN];
  char *cur = buf;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      sprintf (t, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_DOUBLE:
      sprintf (t, "<0x%lx,0x%lx>", (long) XWINT (x, 2), (long) XWINT (x, 3));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_STRING:
      cur = safe_concat (buf, cur, "\"");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "\"");
      break;
    case SYMBOL_REF:
      cur = safe_concat (buf, cur, "`");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "'");
      break;
    case LABEL_REF:
      sprintf (t, "L%d", INSN_UID (XEXP (x, 0)));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "const(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case HIGH:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "high(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case REG:
      if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	{
	  int c = reg_names[REGNO (x)][0];
	  if (ISDIGIT (c))
	    cur = safe_concat (buf, cur, "%");

	  cur = safe_concat (buf, cur, reg_names[REGNO (x)]);
	}
      else
	{
	  sprintf (t, "r%d", REGNO (x));
	  cur = safe_concat (buf, cur, t);
	}
      break;
    case SUBREG:
      print_value (t, SUBREG_REG (x), verbose);
      cur = safe_concat (buf, cur, t);
      sprintf (t, "#%d", SUBREG_BYTE (x));
      cur = safe_concat (buf, cur, t);
      break;
    case SCRATCH:
      cur = safe_concat (buf, cur, "scratch");
      break;
    case CC0:
      cur = safe_concat (buf, cur, "cc0");
      break;
    case PC:
      cur = safe_concat (buf, cur, "pc");
      break;
    case MEM:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "[");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, "]");
      break;
    default:
      print_exp (t, x, verbose);
      cur = safe_concat (buf, cur, t);
      break;
    }
}				/* print_value */

/* The next step in insn detalization, its pattern recognition.  */

static void
print_pattern (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t1[BUF_LEN], t2[BUF_LEN], t3[BUF_LEN];

  switch (GET_CODE (x))
    {
    case SET:
      print_value (t1, SET_DEST (x), verbose);
      print_value (t2, SET_SRC (x), verbose);
      sprintf (buf, "%s=%s", t1, t2);
      break;
    case RETURN:
      sprintf (buf, "return");
      break;
    case CALL:
      print_exp (buf, x, verbose);
      break;
    case CLOBBER:
      print_value (t1, XEXP (x, 0), verbose);
      sprintf (buf, "clobber %s", t1);
      break;
    case USE:
      print_value (t1, XEXP (x, 0), verbose);
      sprintf (buf, "use %s", t1);
      break;
    case COND_EXEC:
      if (GET_CODE (COND_EXEC_TEST (x)) == NE
	  && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
	print_value (t1, XEXP (COND_EXEC_TEST (x), 0), verbose);
      else if (GET_CODE (COND_EXEC_TEST (x)) == EQ
               && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
        {
	  t1[0] = '!';
	  print_value (t1 + 1, XEXP (COND_EXEC_TEST (x), 0), verbose);
	}
      else
        print_value (t1, COND_EXEC_TEST (x), verbose);
      print_pattern (t2, COND_EXEC_CODE (x), verbose);
      sprintf (buf, "(%s) %s", t1, t2);
      break;
    case PARALLEL:
      {
	int i;

	sprintf (t1, "{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case SEQUENCE:
      {
	int i;

	sprintf (t1, "%%{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_insn (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s%%}", t1);
      }
      break;
    case ASM_INPUT:
      sprintf (buf, "asm {%s}", XSTR (x, 0));
      break;
    case ADDR_VEC:
      break;
    case ADDR_DIFF_VEC:
      print_value (buf, XEXP (x, 0), verbose);
      break;
    case TRAP_IF:
      print_value (t1, TRAP_CONDITION (x), verbose);
      sprintf (buf, "trap_if %s", t1);
      break;
    case UNSPEC:
      {
	int i;

	sprintf (t1, "unspec{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case UNSPEC_VOLATILE:
      {
	int i;

	sprintf (t1, "unspec/v{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    default:
      print_value (buf, x, verbose);
    }
}				/* print_pattern */

/* This is the main function in rtl visualization mechanism. It
   accepts an rtx and tries to recognize it as an insn, then prints it
   properly in human readable form, resembling assembler mnemonics.
   For every insn it prints its UID and BB the insn belongs too.
   (Probably the last "option" should be extended somehow, since it
   depends now on sched.c inner variables ...)  */

static void
print_insn (buf, x, verbose)
     char *buf;
     rtx x;
     int verbose;
{
  char t[BUF_LEN];
  rtx insn = x;

  switch (GET_CODE (x))
    {
    case INSN:
      print_pattern (t, PATTERN (x), verbose);
      if (verbose)
	sprintf (buf, "%s: %s", (*current_sched_info->print_insn) (x, 1),
		 t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (x), t);
      break;
    case JUMP_INSN:
      print_pattern (t, PATTERN (x), verbose);
      if (verbose)
	sprintf (buf, "%s: jump %s", (*current_sched_info->print_insn) (x, 1),
		 t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (x), t);
      break;
    case CALL_INSN:
      x = PATTERN (insn);
      if (GET_CODE (x) == PARALLEL)
	{
	  x = XVECEXP (x, 0, 0);
	  print_pattern (t, x, verbose);
	}
      else
	strcpy (t, "call <...>");
      if (verbose)
	sprintf (buf, "%s: %s", (*current_sched_info->print_insn) (x, 1), t);
      else
	sprintf (buf, "%-4d %s", INSN_UID (insn), t);
      break;
    case CODE_LABEL:
      sprintf (buf, "L%d:", INSN_UID (x));
      break;
    case BARRIER:
      sprintf (buf, "i% 4d: barrier", INSN_UID (x));
      break;
    case NOTE:
      if (NOTE_LINE_NUMBER (x) > 0)
	sprintf (buf, "%4d note \"%s\" %d", INSN_UID (x),
		 NOTE_SOURCE_FILE (x), NOTE_LINE_NUMBER (x));
      else
	sprintf (buf, "%4d %s", INSN_UID (x),
		 GET_NOTE_INSN_NAME (NOTE_LINE_NUMBER (x)));
      break;
    default:
      if (verbose)
	{
	  sprintf (buf, "Not an INSN at all\n");
	  debug_rtx (x);
	}
      else
	sprintf (buf, "i%-4d  <What?>", INSN_UID (x));
    }
}				/* print_insn */

/* Print visualization debugging info.  */

void
print_block_visualization (s)
     const char *s;
{
  int unit, i;

  /* Print header.  */
  fprintf (sched_dump, "\n;;   ==================== scheduling visualization %s \n", s);

  /* Print names of units.  */
  fprintf (sched_dump, ";;   %-8s", "clock");
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	fprintf (sched_dump, "  %-33s", function_units[unit].name);
  fprintf (sched_dump, "  %-8s\n", "no-unit");

  fprintf (sched_dump, ";;   %-8s", "=====");
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	fprintf (sched_dump, "  %-33s", "==============================");
  fprintf (sched_dump, "  %-8s\n", "=======");

  /* Print insns in each cycle.  */
  fprintf (sched_dump, "%s\n", visual_tbl);
}

/* Print insns in the 'no_unit' column of visualization.  */

void
visualize_no_unit (insn)
     rtx insn;
{
  if (n_vis_no_unit < MAX_VISUAL_NO_UNIT)
    {
      vis_no_unit[n_vis_no_unit] = insn;
      n_vis_no_unit++;
    }
}

/* Print insns scheduled in clock, for visualization.  */

void
visualize_scheduled_insns (clock)
     int clock;
{
  int i, unit;

  /* If no more room, split table into two.  */
  if (n_visual_lines >= MAX_VISUAL_LINES)
    {
      print_block_visualization ("(incomplete)");
      init_block_visualization ();
    }

  n_visual_lines++;

  sprintf (visual_tbl + strlen (visual_tbl), ";;   %-8d", clock);
  for (unit = 0; unit < FUNCTION_UNITS_SIZE; unit++)
    if (function_units[unit].bitmask & target_units)
      for (i = 0; i < function_units[unit].multiplicity; i++)
	{
	  int instance = unit + i * FUNCTION_UNITS_SIZE;
	  rtx insn = get_unit_last_insn (instance);

	  /* Print insns that still keep the unit busy.  */
	  if (insn
	      && actual_hazard_this_instance (unit, instance, insn, clock, 0))
	    {
	      char str[BUF_LEN];
	      print_insn (str, insn, 0);
	      str[INSN_LEN] = '\0';
	      sprintf (visual_tbl + strlen (visual_tbl), "  %-33s", str);
	    }
	  else
	    sprintf (visual_tbl + strlen (visual_tbl), "  %-33s", "------------------------------");
	}

  /* Print insns that are not assigned to any unit.  */
  for (i = 0; i < n_vis_no_unit; i++)
    sprintf (visual_tbl + strlen (visual_tbl), "  %-8d",
	     INSN_UID (vis_no_unit[i]));
  n_vis_no_unit = 0;

  sprintf (visual_tbl + strlen (visual_tbl), "\n");
}

/* Print stalled cycles.  */

void
visualize_stall_cycles (stalls)
     int stalls;
{
  static const char *const prefix = ";;       ";
  const char *suffix = "\n";
  char *p;

  /* If no more room, split table into two.  */
  if (n_visual_lines >= MAX_VISUAL_LINES)
    {
      print_block_visualization ("(incomplete)");
      init_block_visualization ();
    }

  n_visual_lines++;

  p = visual_tbl + strlen (visual_tbl);
  strcpy (p, prefix);
  p += strlen (prefix);

  if ((unsigned) stalls >
      visual_tbl_line_length - strlen (prefix) - strlen (suffix))
    {
      suffix = "[...]\n";
      stalls = visual_tbl_line_length - strlen (prefix) - strlen (suffix);
    }

  memset (p, '.', stalls);
  p += stalls;

  strcpy (p, suffix);
}

/* Allocate data used for visualization during scheduling.  */

void
visualize_alloc ()
{
  visual_tbl = xmalloc (get_visual_tbl_length ());
}

/* Free data used for visualization.  */

void
visualize_free ()
{
  free (visual_tbl);
}
#endif
