/* Graph coloring register allocator
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Matz <matz@suse.de>
   and Daniel Berlin <dan@cgsoftware.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along
   with GCC; see the file COPYING.  If not, write to the Free Software
   Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "df.h"
#include "output.h"
#include "ra.h"
#include "tm_p.h"

/* This file contains various dumping and debug functions for
   the graph coloring register allocator.  */

static void ra_print_rtx_1op (FILE *, rtx);
static void ra_print_rtx_2op (FILE *, rtx);
static void ra_print_rtx_3op (FILE *, rtx);
static void ra_print_rtx_object (FILE *, rtx);

/* The hardregs as names, for debugging.  */
static const char *const reg_class_names[] = REG_CLASS_NAMES;

/* Print a message to the dump file, if debug_new_regalloc and LEVEL
   have any bits in common.  */

void
ra_debug_msg (unsigned int level, const char *format, ...)
{
  va_list ap;
  
  va_start (ap, format);
  if ((debug_new_regalloc & level) != 0 && rtl_dump_file != NULL)
    vfprintf (rtl_dump_file, format, ap);
  va_end (ap);
}


/* The following ra_print_xxx() functions print RTL expressions
   in concise infix form.  If the mode can be seen from context it's
   left out.  Most operators are represented by their graphical
   characters, e.g. LE as "<=".  Unknown constructs are currently
   printed with print_inline_rtx(), which disrupts the nice layout.
   Currently only the inline asm things are written this way.  */

/* Print rtx X, which is a one operand rtx (op:mode (Y)), as
   "op(Y)" to FILE.  */

static void
ra_print_rtx_1op (FILE *file, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  rtx op0 = XEXP (x, 0);
  switch (code)
    {
      case NEG:
      case NOT:
	  fputs ((code == NEG) ? "-(" : "~(", file);
	  ra_print_rtx (file, op0, 0);
	  fputs (")", file);
	  break;
      case HIGH:
	  fputs ("hi(", file);
	  ra_print_rtx (file, op0, 0);
	  fputs (")", file);
	  break;
      default:
	  fprintf (file, "%s", GET_RTX_NAME (code));
	  if (GET_MODE (x) != VOIDmode)
	    fprintf (file, ":%s(", GET_MODE_NAME (GET_MODE (x)));
	  else
	    fputs ("(", file);
	  ra_print_rtx (file, op0, 0);
	  fputs (")", file);
	  break;
    }
}

/* Print rtx X, which is a two operand rtx (op:mode (Y) (Z))
   as "(Y op Z)", if the operand is know, or as "op(Y, Z)", if not,
   to FILE.  */

static void
ra_print_rtx_2op (FILE *file, rtx x)
{
  int infix = 1;
  const char *opname = "shitop";
  enum rtx_code code = GET_CODE (x);
  rtx op0 = XEXP (x, 0);
  rtx op1 = XEXP (x, 1);
  switch (code)
    {
      /* class '2' */
      case COMPARE: opname = "?"; break;
      case MINUS: opname = "-"; break;
      case DIV: opname = "/"; break;
      case UDIV: opname = "u/"; break;
      case MOD: opname = "%"; break;
      case UMOD: opname = "u%"; break;
      case ASHIFT: opname = "<<"; break;
      case ASHIFTRT: opname = "a>>"; break;
      case LSHIFTRT: opname = "l>>"; break;
      /* class 'c' */
      case PLUS: opname = "+"; break;
      case MULT: opname = "*"; break;
      case AND: opname = "&"; break;
      case IOR: opname = "|"; break;
      case XOR: opname = "^"; break;
      /* class '<' */
      case NE: opname = "!="; break;
      case EQ: opname = "=="; break;
      case GE: opname = "s>="; break;
      case GT: opname = "s>"; break;
      case LE: opname = "s<="; break;
      case LT: opname = "s<"; break;
      case GEU: opname = "u>="; break;
      case GTU: opname = "u>"; break;
      case LEU: opname = "u<="; break;
      case LTU: opname = "u<"; break;
      default:
		infix = 0;
		opname = GET_RTX_NAME (code);
		break;
    }
  if (infix)
    {
      fputs ("(", file);
      ra_print_rtx (file, op0, 0);
      fprintf (file, " %s ", opname);
      ra_print_rtx (file, op1, 0);
      fputs (")", file);
    }
  else
    {
      fprintf (file, "%s(", opname);
      ra_print_rtx (file, op0, 0);
      fputs (", ", file);
      ra_print_rtx (file, op1, 0);
      fputs (")", file);
    }
}

/* Print rtx X, which a three operand rtx to FILE.
   I.e. X is either an IF_THEN_ELSE, or a bitmap operation.  */

static void
ra_print_rtx_3op (FILE *file, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  rtx op0 = XEXP (x, 0);
  rtx op1 = XEXP (x, 1);
  rtx op2 = XEXP (x, 2);
  if (code == IF_THEN_ELSE)
    {
      ra_print_rtx (file, op0, 0);
      fputs (" ? ", file);
      ra_print_rtx (file, op1, 0);
      fputs (" : ", file);
      ra_print_rtx (file, op2, 0);
    }
  else
    {
      /* Bitmap-operation */
      fprintf (file, "%s:%s(", GET_RTX_NAME (code),
	       GET_MODE_NAME (GET_MODE (x)));
      ra_print_rtx (file, op0, 0);
      fputs (", ", file);
      ra_print_rtx (file, op1, 0);
      fputs (", ", file);
      ra_print_rtx (file, op2, 0);
      fputs (")", file);
    }
}

/* Print rtx X, which represents an object (class 'o' or some constructs
   of class 'x' (e.g. subreg)), to FILE.
   (reg XX) rtl is represented as "pXX", of XX was a pseudo,
   as "name" it name is the nonnull hardreg name, or as "hXX", if XX
   is a hardreg, whose name is NULL, or empty.  */

static void
ra_print_rtx_object (FILE *file, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);
  switch (code)
    {
      case CONST_INT:
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, XWINT (x, 0));
	  break;
      case CONST_DOUBLE:
	    {
	      int i, num = 0;
	      const char *fmt = GET_RTX_FORMAT (code);
	      fputs ("dbl(", file);
	      for (i = 0; i < GET_RTX_LENGTH (code); i++)
		{
		  if (num)
		    fputs (", ", file);
		  if (fmt[i] == 'e' && XEXP (x, i))
		    /* The MEM or other stuff */
		    {
		      ra_print_rtx (file, XEXP (x, i), 0);
		      num++;
		    }
		  else if (fmt[i] == 'w')
		    {
		      fprintf (file, HOST_WIDE_INT_PRINT_HEX, XWINT (x, i));
		      num++;
		    }
		}
	      break;
	    }
      case CONST_STRING: fprintf (file, "\"%s\"", XSTR (x, 0)); break;
      case CONST: fputs ("const(", file);
		  ra_print_rtx (file, XEXP (x, 0), 0);
		  fputs (")", file);
		  break;
      case PC: fputs ("pc", file); break;
      case REG:
	       {
		 int regno = REGNO (x);
		 if (regno < FIRST_PSEUDO_REGISTER)
		   {
		     int i, nregs = HARD_REGNO_NREGS (regno, mode);
		     if (nregs > 1)
		       fputs ("[", file);
		     for (i = 0; i < nregs; i++)
		       {
			 if (i)
			   fputs (", ", file);
			 if (reg_names[regno+i] && *reg_names[regno + i])
			   fprintf (file, "%s", reg_names[regno + i]);
			 else
			   fprintf (file, "h%d", regno + i);
		       }
		     if (nregs > 1)
		       fputs ("]", file);
		   }
		 else
		   fprintf (file, "p%d", regno);
		 break;
	       }
      case SUBREG:
	       {
		 rtx sub = SUBREG_REG (x);
		 int ofs = SUBREG_BYTE (x);
		 if (GET_CODE (sub) == REG
		     && REGNO (sub) < FIRST_PSEUDO_REGISTER)
		   {
		     int regno = REGNO (sub);
		     int i, nregs = HARD_REGNO_NREGS (regno, mode);
		     regno += subreg_regno_offset (regno, GET_MODE (sub),
						   ofs, mode);
		     if (nregs > 1)
		       fputs ("[", file);
		     for (i = 0; i < nregs; i++)
		       {
			 if (i)
			   fputs (", ", file);
			 if (reg_names[regno+i])
			   fprintf (file, "%s", reg_names[regno + i]);
			 else
			   fprintf (file, "h%d", regno + i);
		       }
		     if (nregs > 1)
		       fputs ("]", file);
		   }
		 else
		   {
		     ra_print_rtx (file, sub, 0);
		     fprintf (file, ":[%s+%d]", GET_MODE_NAME (mode), ofs);
		   }
		 break;
	       }
      case SCRATCH: fputs ("scratch", file); break;
      case CONCAT: ra_print_rtx_2op (file, x); break;
      case HIGH: ra_print_rtx_1op (file, x); break;
      case LO_SUM:
		 fputs ("(", file);
		 ra_print_rtx (file, XEXP (x, 0), 0);
		 fputs (" + lo(", file);
		 ra_print_rtx (file, XEXP (x, 1), 0);
		 fputs ("))", file);
		 break;
      case MEM: fputs ("[", file);
		ra_print_rtx (file, XEXP (x, 0), 0);
		fprintf (file, "]:%s", GET_MODE_NAME (GET_MODE (x)));
		/* XXX print alias set too ?? */
		break;
      case LABEL_REF:
		  {
		    rtx sub = XEXP (x, 0);
		    if (GET_CODE (sub) == NOTE
			&& NOTE_LINE_NUMBER (sub) == NOTE_INSN_DELETED_LABEL)
		      fprintf (file, "(deleted uid=%d)", INSN_UID (sub));
		    else if (GET_CODE (sub) == CODE_LABEL)
		      fprintf (file, "L%d", CODE_LABEL_NUMBER (sub));
		    else
		      fprintf (file, "(nonlabel uid=%d)", INSN_UID (sub));
		  }
		break;
      case SYMBOL_REF:
		fprintf (file, "sym(\"%s\")", XSTR (x, 0)); break;
      case CC0: fputs ("cc0", file); break;
      default: print_inline_rtx (file, x, 0); break;
    }
}

/* Print a general rtx X to FILE in nice infix form.
   If WITH_PN is set, and X is one of the toplevel constructs
   (insns, notes, labels or barriers), then print also the UIDs of
   the preceding and following insn.  */

void
ra_print_rtx (FILE *file, rtx x, int with_pn)
{
  enum rtx_code code;
  char class;
  int unhandled = 0;
  if (!x)
    return;
  code = GET_CODE (x);
  class = GET_RTX_CLASS (code);

  /* First handle the insn like constructs.  */
  if (INSN_P (x) || code == NOTE || code == CODE_LABEL || code == BARRIER)
    {
      if (INSN_P (x))
	fputs ("  ", file);
      /* Non-insns are prefixed by a ';'.  */
      if (code == BARRIER)
	fputs ("; ", file);
      else if (code == NOTE)
	/* But notes are indented very far right.  */
	fprintf (file, "\t\t\t\t\t; ");
      else if (code == CODE_LABEL)
	/* And labels have their Lxx name first, before the actual UID.  */
	{
	  fprintf (file, "L%d:\t; ", CODE_LABEL_NUMBER (x));
	  if (LABEL_NAME (x))
	    fprintf (file, "(%s) ", LABEL_NAME (x));
	  switch (LABEL_KIND (x))
	    {
	    case LABEL_NORMAL: break;
	    case LABEL_STATIC_ENTRY: fputs (" (entry)", file); break;
	    case LABEL_GLOBAL_ENTRY: fputs (" (global entry)", file); break;
	    case LABEL_WEAK_ENTRY: fputs (" (weak entry)", file); break;
	    default: abort();
	    }
	  fprintf (file, " [%d uses] uid=(", LABEL_NUSES (x));
	}
      fprintf (file, "%d", INSN_UID (x));
      if (with_pn)
	fprintf (file, " %d %d", PREV_INSN (x) ? INSN_UID (PREV_INSN (x)) : 0,
		 NEXT_INSN (x) ? INSN_UID (NEXT_INSN (x)) : 0);
      if (code == BARRIER)
	fputs (" -------- barrier ---------", file);
      else if (code == CODE_LABEL)
	fputs (")", file);
      else if (code == NOTE)
	{
	  int ln = NOTE_LINE_NUMBER (x);
	  if (ln >= (int) NOTE_INSN_BIAS && ln < (int) NOTE_INSN_MAX)
	    fprintf (file, " %s", GET_NOTE_INSN_NAME (ln));
	  else
	    {
	      fprintf (file, " line %d", ln);
	      if (NOTE_SOURCE_FILE (x))
		fprintf (file, ":%s", NOTE_SOURCE_FILE (x));
	    }
	}
      else
	{
	  fprintf (file, "\t");
	  ra_print_rtx (file, PATTERN (x), 0);
	}
      return;
    }
  switch (code)
    {
      /* Top-level stuff.  */
      case PARALLEL:
	    {
	      int j;
	      for (j = 0; j < XVECLEN (x, 0); j++)
		{
		  if (j)
		    fputs ("\t;; ", file);
		  ra_print_rtx (file, XVECEXP (x, 0, j), 0);
		}
	      break;
	    }
      case UNSPEC: case UNSPEC_VOLATILE:
	    {
	      int j;
	      fprintf (file, "unspec%s(%d",
		       (code == UNSPEC) ? "" : "_vol", XINT (x, 1));
	      for (j = 0; j < XVECLEN (x, 0); j++)
		{
		  fputs (", ", file);
		  ra_print_rtx (file, XVECEXP (x, 0, j), 0);
		}
	      fputs (")", file);
	      break;
	    }
      case SET:
	  if (GET_CODE (SET_DEST (x)) == PC)
	    {
	      if (GET_CODE (SET_SRC (x)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC(x), 2)) == PC)
		{
		  fputs ("if ", file);
		  ra_print_rtx (file, XEXP (SET_SRC (x), 0), 0);
		  fputs (" jump ", file);
		  ra_print_rtx (file, XEXP (SET_SRC (x), 1), 0);
		}
	      else
		{
		  fputs ("jump ", file);
		  ra_print_rtx (file, SET_SRC (x), 0);
		}
	    }
	  else
	    {
	      ra_print_rtx (file, SET_DEST (x), 0);
	      fputs (" <= ", file);
	      ra_print_rtx (file, SET_SRC (x), 0);
	    }
	  break;
      case USE:
	      fputs ("use <= ", file);
	      ra_print_rtx (file, XEXP (x, 0), 0);
	      break;
      case CLOBBER:
	      ra_print_rtx (file, XEXP (x, 0), 0);
	      fputs (" <= clobber", file);
	      break;
      case CALL:
	      fputs ("call ", file);
	      ra_print_rtx (file, XEXP (x, 0), 0); /* Address */
	      fputs (" numargs=", file);
	      ra_print_rtx (file, XEXP (x, 1), 0); /* Num arguments */
	      break;
      case RETURN:
	      fputs ("return", file);
	      break;
      case TRAP_IF:
	      fputs ("if (", file);
	      ra_print_rtx (file, XEXP (x, 0), 0);
	      fputs (") trap ", file);
	      ra_print_rtx (file, XEXP (x, 1), 0);
	      break;
      case RESX:
	      fprintf (file, "resx from region %d", XINT (x, 0));
	      break;

      /* Different things of class 'x' */
      case SUBREG: ra_print_rtx_object (file, x); break;
      case STRICT_LOW_PART:
		   fputs ("low(", file);
		   ra_print_rtx (file, XEXP (x, 0), 0);
		   fputs (")", file);
		   break;
      default:
	unhandled = 1;
	break;
    }
  if (!unhandled)
    return;
  if (class == '1')
    ra_print_rtx_1op (file, x);
  else if (class == '2' || class == 'c' || class == '<')
    ra_print_rtx_2op (file, x);
  else if (class == '3' || class == 'b')
    ra_print_rtx_3op (file, x);
  else if (class == 'o')
    ra_print_rtx_object (file, x);
  else
    print_inline_rtx (file, x, 0);
}

/* This only calls ra_print_rtx(), but emits a final newline.  */

void
ra_print_rtx_top (FILE *file, rtx x, int with_pn)
{
  ra_print_rtx (file, x, with_pn);
  fprintf (file, "\n");
}

/* Callable from gdb.  This prints rtx X onto stderr.  */

void
ra_debug_rtx (rtx x)
{
  ra_print_rtx_top (stderr, x, 1);
}

/* This prints the content of basic block with index BBI.
   The first and last insn are emitted with UIDs of prev and next insns.  */

void
ra_debug_bbi (int bbi)
{
  basic_block bb = BASIC_BLOCK (bbi);
  rtx insn;
  for (insn = BB_HEAD (bb); insn; insn = NEXT_INSN (insn))
    {
      ra_print_rtx_top (stderr, insn,
			(insn == BB_HEAD (bb) || insn == BB_END (bb)));
      fprintf (stderr, "\n");
      if (insn == BB_END (bb))
	break;
    }
}

/* Beginning from INSN, emit NUM insns (if NUM is non-negative)
   or emit a window of NUM insns around INSN, to stderr.  */

void
ra_debug_insns (rtx insn, int num)
{
  int i, count = (num == 0 ? 1 : num < 0 ? -num : num);
  if (num < 0)
    for (i = count / 2; i > 0 && PREV_INSN (insn); i--)
      insn = PREV_INSN (insn);
  for (i = count; i > 0 && insn; insn = NEXT_INSN (insn), i--)
    {
      if (GET_CODE (insn) == CODE_LABEL)
	fprintf (stderr, "\n");
      ra_print_rtx_top (stderr, insn, (i == count || i == 1));
    }
}

/* Beginning with INSN, emit the whole insn chain into FILE.
   This also outputs comments when basic blocks start or end and omits
   some notes, if flag_ra_dump_notes is zero.  */

void
ra_print_rtl_with_bb (FILE *file, rtx insn)
{
  basic_block last_bb, bb;
  unsigned int num = 0;
  if (!insn)
    fputs ("nil", file);
  last_bb = NULL;
  for (; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == BARRIER)
	bb = NULL;
      else
	bb = BLOCK_FOR_INSN (insn);
      if (bb != last_bb)
	{
	  if (last_bb)
	    fprintf (file, ";; End of basic block %d\n", last_bb->index);
	  if (bb)
	    fprintf (file, ";; Begin of basic block %d\n", bb->index);
	  last_bb = bb;
	}
      if (GET_CODE (insn) == CODE_LABEL)
	fputc ('\n', file);
      if (GET_CODE (insn) == NOTE)
	{
	  /* Ignore basic block and maybe other notes not referencing
	     deleted things.  */
	  if (NOTE_LINE_NUMBER (insn) != NOTE_INSN_BASIC_BLOCK
	      && (flag_ra_dump_notes
		  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED
		  || NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED_LABEL))
	    {
	      ra_print_rtx_top (file, insn, (num == 0 || !NEXT_INSN (insn)));
	      num++;
	    }
	}
      else
	{
	  ra_print_rtx_top (file, insn, (num == 0 || !NEXT_INSN (insn)));
	  num++;
	}
    }
}

/* Count how many insns were seen how often, while building the interference
   graph, and prints the findings.  */

void
dump_number_seen (void)
{
#define N 17
  int num[N];
  int i;

  for (i = 0; i < N; i++)
    num[i] = 0;
  for (i = 0; i < get_max_uid (); i++)
    if (number_seen[i] < N - 1)
      num[number_seen[i]]++;
    else
      num[N - 1]++;
  for (i = 0; i < N - 1; i++)
    if (num[i])
      ra_debug_msg (DUMP_PROCESS, "%d insns seen %d times\n", num[i], i);
  if (num[N - 1])
    ra_debug_msg (DUMP_PROCESS, "%d insns seen %d and more times\n", num[i],
	       N - 1);
  ra_debug_msg (DUMP_PROCESS, "from overall %d insns\n", get_max_uid ());
#undef N
}

/* Dump the interference graph, the move list and the webs.  */

void
dump_igraph (struct df *df ATTRIBUTE_UNUSED)
{
  struct move_list *ml;
  unsigned int def1, def2;
  int num = 0;
  int num2;
  unsigned int i;
  if (!rtl_dump_file || (debug_new_regalloc & (DUMP_IGRAPH | DUMP_WEBS)) == 0)
    return;
  ra_debug_msg (DUMP_IGRAPH, "conflicts:\n  ");
  for (def1 = 0; def1 < num_webs; def1++)
    {
      int num1 = num;
      num2 = 0;
      for (def2 = 0; def2 < num_webs; def2++)
        if (def1 != def2 && TEST_BIT (igraph, igraph_index (def1, def2)))
	  {
	    if (num1 == num)
	      {
	        if (SUBWEB_P (ID2WEB (def1)))
		  ra_debug_msg (DUMP_IGRAPH, "%d (SUBREG %d, %d) with ", def1,
			     ID2WEB (def1)->regno,
			     SUBREG_BYTE (ID2WEB (def1)->orig_x));
	        else
	          ra_debug_msg (DUMP_IGRAPH, "%d (REG %d) with ", def1,
			     ID2WEB (def1)->regno);
	      }
	    if ((num2 % 9) == 8)
	      ra_debug_msg (DUMP_IGRAPH, "\n              ");
	    num++;
	    num2++;
	    if (SUBWEB_P (ID2WEB (def2)))
	      ra_debug_msg (DUMP_IGRAPH, "%d(%d,%d) ", def2, ID2WEB (def2)->regno,
			 SUBREG_BYTE (ID2WEB (def2)->orig_x));
	    else
	      ra_debug_msg (DUMP_IGRAPH, "%d(%d) ", def2, ID2WEB (def2)->regno);
	  }
      if (num1 != num)
	ra_debug_msg (DUMP_IGRAPH, "\n  ");
    }
  ra_debug_msg (DUMP_IGRAPH, "\n");
  for (ml = wl_moves; ml; ml = ml->next)
    if (ml->move)
      {
        ra_debug_msg (DUMP_IGRAPH, "move: insn %d: Web %d <-- Web %d\n",
	         INSN_UID (ml->move->insn), ml->move->target_web->id,
	         ml->move->source_web->id);
      }
  ra_debug_msg (DUMP_WEBS, "\nWebs:\n");
  for (i = 0; i < num_webs; i++)
    {
      struct web *web = ID2WEB (i);

      ra_debug_msg (DUMP_WEBS, "  %4d : regno %3d", i, web->regno);
      if (SUBWEB_P (web))
	{
	  ra_debug_msg (DUMP_WEBS, " sub %d", SUBREG_BYTE (web->orig_x));
	  ra_debug_msg (DUMP_WEBS, " par %d", find_web_for_subweb (web)->id);
	}
      ra_debug_msg (DUMP_WEBS, " +%d (span %d, cost "
		    HOST_WIDE_INT_PRINT_DEC ") (%s)",
		    web->add_hardregs, web->span_deaths, web->spill_cost,
		    reg_class_names[web->regclass]);
      if (web->spill_temp == 1)
	ra_debug_msg (DUMP_WEBS, " (spilltemp)");
      else if (web->spill_temp == 2)
	ra_debug_msg (DUMP_WEBS, " (spilltem2)");
      else if (web->spill_temp == 3)
	ra_debug_msg (DUMP_WEBS, " (short)");
      if (web->type == PRECOLORED)
        ra_debug_msg (DUMP_WEBS, " (precolored, color=%d)", web->color);
      else if (find_web_for_subweb (web)->num_uses == 0)
	ra_debug_msg (DUMP_WEBS, " dead");
      if (web->crosses_call)
	ra_debug_msg (DUMP_WEBS, " xcall");
      if (web->regno >= max_normal_pseudo)
	ra_debug_msg (DUMP_WEBS, " stack");
      ra_debug_msg (DUMP_WEBS, "\n");
    }
}

/* Dump the interference graph and webs in a format easily
   parsable by programs.  Used to emit real world interference graph
   to my custom graph colorizer.  */

void
dump_igraph_machine (void)
{
  unsigned int i;

  if (!rtl_dump_file || (debug_new_regalloc & DUMP_IGRAPH_M) == 0)
    return;
  ra_debug_msg (DUMP_IGRAPH_M, "g %d %d\n", num_webs - num_subwebs,
	     FIRST_PSEUDO_REGISTER);
  for (i = 0; i < num_webs - num_subwebs; i++)
    {
      struct web *web = ID2WEB (i);
      struct conflict_link *cl;
      int flags = 0;
      int numc = 0;
      int col = 0;
      flags = web->spill_temp & 0xF;
      flags |= ((web->type == PRECOLORED) ? 1 : 0) << 4;
      flags |= (web->add_hardregs & 0xF) << 5;
      for (cl = web->conflict_list; cl; cl = cl->next)
	if (cl->t->id < web->id)
	  numc++;
      ra_debug_msg (DUMP_IGRAPH_M, "n %d %d %d %d %d %d %d\n",
		 web->id, web->color, flags,
		 (unsigned int)web->spill_cost, web->num_defs, web->num_uses,
		 numc);
      if (web->type != PRECOLORED)
	{
	  ra_debug_msg (DUMP_IGRAPH_M, "s %d", web->id);
	  while (1)
	    {
	      unsigned int u = 0;
	      int n;
	      for (n = 0; n < 32 && col < FIRST_PSEUDO_REGISTER; n++, col++)
		if (TEST_HARD_REG_BIT (web->usable_regs, col))
		  u |= 1 << n;
	      ra_debug_msg (DUMP_IGRAPH_M, " %u", u);
	      if (col >= FIRST_PSEUDO_REGISTER)
		break;
	    }
	  ra_debug_msg (DUMP_IGRAPH_M, "\n");
	}
      if (numc)
	{
	  ra_debug_msg (DUMP_IGRAPH_M, "c %d", web->id);
	  for (cl = web->conflict_list; cl; cl = cl->next)
	    {
	      if (cl->t->id < web->id)
		ra_debug_msg (DUMP_IGRAPH_M, " %d", cl->t->id);
	    }
	  ra_debug_msg (DUMP_IGRAPH_M, "\n");
	}
    }
  ra_debug_msg (DUMP_IGRAPH_M, "e\n");
}

/* This runs after colorization and changing the insn stream.
   It temporarily replaces all pseudo registers with their colors,
   and emits information, if the resulting insns are strictly valid.  */

void
dump_constraints (void)
{
  rtx insn;
  int i;
  if (!rtl_dump_file || (debug_new_regalloc & DUMP_CONSTRAINTS) == 0)
    return;
  for (i = FIRST_PSEUDO_REGISTER; i < ra_max_regno; i++)
    if (regno_reg_rtx[i] && GET_CODE (regno_reg_rtx[i]) == REG)
      REGNO (regno_reg_rtx[i])
	  = ra_reg_renumber[i] >= 0 ? ra_reg_renumber[i] : i;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	int code;
	int uid = INSN_UID (insn);
	int o;
	/* Don't simply force rerecognition, as combine might left us
	   with some unrecognizable ones, which later leads to aborts
	   in regclass, if we now destroy the remembered INSN_CODE().  */
	/*INSN_CODE (insn) = -1;*/
	code = recog_memoized (insn);
	if (code < 0)
	  {
	    ra_debug_msg (DUMP_CONSTRAINTS,
		       "%d: asm insn or not recognizable.\n", uid);
	    continue;
	  }
	ra_debug_msg (DUMP_CONSTRAINTS,
		   "%d: code %d {%s}, %d operands, constraints: ",
		   uid, code, insn_data[code].name, recog_data.n_operands);
        extract_insn (insn);
	/*preprocess_constraints ();*/
	for (o = 0; o < recog_data.n_operands; o++)
	  {
	    ra_debug_msg (DUMP_CONSTRAINTS,
		       "%d:%s ", o, recog_data.constraints[o]);
	  }
	if (constrain_operands (1))
	  ra_debug_msg (DUMP_CONSTRAINTS, "matches strictly alternative %d",
		     which_alternative);
	else
	  ra_debug_msg (DUMP_CONSTRAINTS, "doesn't match strictly");
	ra_debug_msg (DUMP_CONSTRAINTS, "\n");
      }
  for (i = FIRST_PSEUDO_REGISTER; i < ra_max_regno; i++)
    if (regno_reg_rtx[i] && GET_CODE (regno_reg_rtx[i]) == REG)
      REGNO (regno_reg_rtx[i]) = i;
}

/* This counts and emits the cumulated cost of all spilled webs,
   preceded by a custom message MSG, with debug level LEVEL.  */

void
dump_graph_cost (unsigned int level, const char *msg)
{
  unsigned int i;
  unsigned HOST_WIDE_INT cost;
  if (!rtl_dump_file || (debug_new_regalloc & level) == 0)
    return;

  cost = 0;
  for (i = 0; i < num_webs; i++)
    {
      struct web *web = id2web[i];
      if (alias (web)->type == SPILLED)
	cost += web->orig_spill_cost;
    }
  ra_debug_msg (level, " spill cost of graph (%s) = "
		HOST_WIDE_INT_PRINT_UNSIGNED "\n",
		msg ? msg : "", cost);
}

/* Dump the color assignment per web, the coalesced and spilled webs.  */

void
dump_ra (struct df *df ATTRIBUTE_UNUSED)
{
  struct web *web;
  struct dlist *d;
  if (!rtl_dump_file || (debug_new_regalloc & DUMP_RESULTS) == 0)
    return;

  ra_debug_msg (DUMP_RESULTS, "\nColored:\n");
  for (d = WEBS(COLORED); d; d = d->next)
    {
      web = DLIST_WEB (d);
      ra_debug_msg (DUMP_RESULTS, "  %4d : color %d\n", web->id, web->color);
    }
  ra_debug_msg (DUMP_RESULTS, "\nCoalesced:\n");
  for (d = WEBS(COALESCED); d; d = d->next)
    {
      web = DLIST_WEB (d);
      ra_debug_msg (DUMP_RESULTS, "  %4d : to web %d, color %d\n", web->id,
	         alias (web)->id, web->color);
    }
  ra_debug_msg (DUMP_RESULTS, "\nSpilled:\n");
  for (d = WEBS(SPILLED); d; d = d->next)
    {
      web = DLIST_WEB (d);
      ra_debug_msg (DUMP_RESULTS, "  %4d\n", web->id);
    }
  ra_debug_msg (DUMP_RESULTS, "\n");
  dump_cost (DUMP_RESULTS);
}

/* Calculate and dump the cumulated costs of certain types of insns
   (loads, stores and copies).  */

void
dump_static_insn_cost (FILE *file, const char *message, const char *prefix)
{
  struct cost
    {
      unsigned HOST_WIDE_INT cost;
      unsigned int count;
    };
  basic_block bb;
  struct cost load, store, regcopy, selfcopy, overall;
  memset (&load, 0, sizeof(load));
  memset (&store, 0, sizeof(store));
  memset (&regcopy, 0, sizeof(regcopy));
  memset (&selfcopy, 0, sizeof(selfcopy));
  memset (&overall, 0, sizeof(overall));

  if (!file)
    return;

  FOR_EACH_BB (bb)
    {
      unsigned HOST_WIDE_INT block_cost = bb->frequency;
      rtx insn, set;
      for (insn = BB_HEAD (bb); insn; insn = NEXT_INSN (insn))
	{
	  /* Yes, yes.  We don't calculate the costs precisely.
	     Only for "simple enough" insns.  Those containing single
	     sets only.  */
	  if (INSN_P (insn) && ((set = single_set (insn)) != NULL))
	    {
	      rtx src = SET_SRC (set);
	      rtx dest = SET_DEST (set);
	      struct cost *pcost = NULL;
	      overall.cost += block_cost;
	      overall.count++;
	      if (rtx_equal_p (src, dest))
		pcost = &selfcopy;
	      else if (GET_CODE (src) == GET_CODE (dest)
		       && ((GET_CODE (src) == REG)
			   || (GET_CODE (src) == SUBREG
			       && GET_CODE (SUBREG_REG (src)) == REG
			       && GET_CODE (SUBREG_REG (dest)) == REG)))
		pcost = &regcopy;
	      else
		{
		  if (GET_CODE (src) == SUBREG)
		    src = SUBREG_REG (src);
		  if (GET_CODE (dest) == SUBREG)
		    dest = SUBREG_REG (dest);
		  if (GET_CODE (src) == MEM && GET_CODE (dest) != MEM
		      && memref_is_stack_slot (src))
		    pcost = &load;
		  else if (GET_CODE (src) != MEM && GET_CODE (dest) == MEM
			   && memref_is_stack_slot (dest))
		    pcost = &store;
		}
	      if (pcost)
		{
		  pcost->cost += block_cost;
		  pcost->count++;
		}
	    }
	  if (insn == BB_END (bb))
	    break;
	}
    }

  if (!prefix)
    prefix = "";
  fprintf (file, "static insn cost %s\n", message ? message : "");
  fprintf (file, "  %soverall:\tnum=%6d\tcost=% 8" HOST_WIDE_INT_PRINT "d\n",
	   prefix, overall.count, overall.cost);
  fprintf (file, "  %sloads:\tnum=%6d\tcost=% 8" HOST_WIDE_INT_PRINT "d\n",
	   prefix, load.count, load.cost);
  fprintf (file, "  %sstores:\tnum=%6d\tcost=% 8" HOST_WIDE_INT_PRINT "d\n",
	   prefix, store.count, store.cost);
  fprintf (file, "  %sregcopy:\tnum=%6d\tcost=% 8" HOST_WIDE_INT_PRINT "d\n",
	   prefix, regcopy.count, regcopy.cost);
  fprintf (file, "  %sselfcpy:\tnum=%6d\tcost=% 8" HOST_WIDE_INT_PRINT "d\n",
	   prefix, selfcopy.count, selfcopy.cost);
}

/* Returns nonzero, if WEB1 and WEB2 have some possible
   hardregs in common.  */

int
web_conflicts_p (struct web *web1, struct web *web2)
{
  if (web1->type == PRECOLORED && web2->type == PRECOLORED)
    return 0;

  if (web1->type == PRECOLORED)
    return TEST_HARD_REG_BIT (web2->usable_regs, web1->regno);

  if (web2->type == PRECOLORED)
    return TEST_HARD_REG_BIT (web1->usable_regs, web2->regno);

  return hard_regs_intersect_p (&web1->usable_regs, &web2->usable_regs);
}

/* Dump all uids of insns in which WEB is mentioned.  */

void
dump_web_insns (struct web *web)
{
  unsigned int i;

  ra_debug_msg (DUMP_EVER, "Web: %i(%i)+%i class: %s freedom: %i degree %i\n",
	     web->id, web->regno, web->add_hardregs,
	     reg_class_names[web->regclass],
	     web->num_freedom, web->num_conflicts);
  ra_debug_msg (DUMP_EVER, "   def insns:");

  for (i = 0; i < web->num_defs; ++i)
    {
      ra_debug_msg (DUMP_EVER, " %d ", INSN_UID (web->defs[i]->insn));
    }

  ra_debug_msg (DUMP_EVER, "\n   use insns:");
  for (i = 0; i < web->num_uses; ++i)
    {
      ra_debug_msg (DUMP_EVER, " %d ", INSN_UID (web->uses[i]->insn));
    }
  ra_debug_msg (DUMP_EVER, "\n");
}

/* Dump conflicts for web WEB.  */

void
dump_web_conflicts (struct web *web)
{
  int num = 0;
  unsigned int def2;

  ra_debug_msg (DUMP_EVER, "Web: %i(%i)+%i class: %s freedom: %i degree %i\n",
	     web->id, web->regno, web->add_hardregs,
	     reg_class_names[web->regclass],
	     web->num_freedom, web->num_conflicts);

  for (def2 = 0; def2 < num_webs; def2++)
    if (TEST_BIT (igraph, igraph_index (web->id, def2)) && web->id != def2)
      {
	if ((num % 9) == 5)
	  ra_debug_msg (DUMP_EVER, "\n             ");
	num++;

	ra_debug_msg (DUMP_EVER, " %d(%d)", def2, id2web[def2]->regno);
	if (id2web[def2]->add_hardregs)
	  ra_debug_msg (DUMP_EVER, "+%d", id2web[def2]->add_hardregs);

	if (web_conflicts_p (web, id2web[def2]))
	  ra_debug_msg (DUMP_EVER, "/x");

	if (id2web[def2]->type == SELECT)
	  ra_debug_msg (DUMP_EVER, "/s");

	if (id2web[def2]->type == COALESCED)
	  ra_debug_msg (DUMP_EVER,"/c/%d", alias (id2web[def2])->id);
      }
  ra_debug_msg (DUMP_EVER, "\n");
  {
    struct conflict_link *wl;
    num = 0;
    ra_debug_msg (DUMP_EVER, "By conflicts:     ");
    for (wl = web->conflict_list; wl; wl = wl->next)
      {
	struct web* w = wl->t;
	if ((num % 9) == 8)
	  ra_debug_msg (DUMP_EVER, "\n              ");
	num++;
	ra_debug_msg (DUMP_EVER, "%d(%d)%s ", w->id, w->regno,
		   web_conflicts_p (web, w) ? "+" : "");
      }
    ra_debug_msg (DUMP_EVER, "\n");
  }
}

/* Output HARD_REG_SET to stderr.  */

void
debug_hard_reg_set (HARD_REG_SET set)
{
  int i;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
    {
      if (TEST_HARD_REG_BIT (set, i))
	{
	  fprintf (stderr, "%s ", reg_names[i]);
	}
    }
  fprintf (stderr, "\n");
}

/*
vim:cinoptions={.5s,g0,p5,t0,(0,^-0.5s,n-0.5s:tw=78:cindent:sw=4:
*/
