/* Print RTL for GNU C Compiler.
   Copyright (C) 1987, 1988, 1992, 1997, 1998, 1999, 2000, 2002, 2003
   Free Software Foundation, Inc.

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
#include "rtl.h"

/* We don't want the tree code checking code for the access to the
   DECL_NAME to be included in the gen* programs.  */
#undef ENABLE_TREE_CHECKING
#include "tree.h"
#include "real.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"

/* How to print out a register name.
   We don't use PRINT_REG because some definitions of PRINT_REG
   don't work here.  */
#ifndef DEBUG_PRINT_REG
#define DEBUG_PRINT_REG(RTX, CODE, FILE) \
  fprintf ((FILE), "%d %s", REGNO (RTX), reg_names[REGNO (RTX)])
#endif

/* Array containing all of the register names */

#ifdef DEBUG_REGISTER_NAMES
static const char * const debug_reg_names[] = DEBUG_REGISTER_NAMES;
#define reg_names debug_reg_names
#else
const char * reg_names[] = REGISTER_NAMES;
#endif

static FILE *outfile;

static int sawclose = 0;

static int indent;

static void print_rtx		PARAMS ((rtx));

/* String printed at beginning of each RTL when it is dumped.
   This string is set to ASM_COMMENT_START when the RTL is dumped in
   the assembly output file.  */
const char *print_rtx_head = "";

/* Nonzero means suppress output of instruction numbers and line number
   notes in debugging dumps.
   This must be defined here so that programs like gencodes can be linked.  */
int flag_dump_unnumbered = 0;

/* Nonzero means use simplified format without flags, modes, etc.  */
int flag_simple = 0;

/* Nonzero if we are dumping graphical description.  */
int dump_for_graph;

/* Nonzero to dump all call_placeholder alternatives.  */
static int debug_call_placeholder_verbose;

void
print_mem_expr (outfile, expr)
     FILE *outfile;
     tree expr;
{
  if (TREE_CODE (expr) == COMPONENT_REF)
    {
      if (TREE_OPERAND (expr, 0))
	print_mem_expr (outfile, TREE_OPERAND (expr, 0));
      else
	fputs (" <variable>", outfile);
      if (DECL_NAME (TREE_OPERAND (expr, 1)))
	fprintf (outfile, ".%s",
		 IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (expr, 1))));
    }
  else if (TREE_CODE (expr) == INDIRECT_REF)
    {
      fputs (" (*", outfile);
      print_mem_expr (outfile, TREE_OPERAND (expr, 0));
      fputs (")", outfile);
    }
  else if (DECL_NAME (expr))
    fprintf (outfile, " %s", IDENTIFIER_POINTER (DECL_NAME (expr)));
  else if (TREE_CODE (expr) == RESULT_DECL)
    fputs (" <result>", outfile);
  else
    fputs (" <anonymous>", outfile);
}

/* Print IN_RTX onto OUTFILE.  This is the recursive part of printing.  */

static void
print_rtx (in_rtx)
     rtx in_rtx;
{
  int i = 0;
  int j;
  const char *format_ptr;
  int is_insn;
  rtx tem;

  if (sawclose)
    {
      if (flag_simple)
	fputc (' ', outfile);
      else
	fprintf (outfile, "\n%s%*s", print_rtx_head, indent * 2, "");
      sawclose = 0;
    }

  if (in_rtx == 0)
    {
      fputs ("(nil)", outfile);
      sawclose = 1;
      return;
    }
  else if (GET_CODE (in_rtx) > NUM_RTX_CODE)
    {
       fprintf (outfile, "(??? bad code %d\n)", GET_CODE (in_rtx));
       sawclose = 1;
       return;
    }

  is_insn = INSN_P (in_rtx);

  /* When printing in VCG format we write INSNs, NOTE, LABEL, and BARRIER
     in separate nodes and therefore have to handle them special here.  */
  if (dump_for_graph
      && (is_insn || GET_CODE (in_rtx) == NOTE
	  || GET_CODE (in_rtx) == CODE_LABEL || GET_CODE (in_rtx) == BARRIER))
    {
      i = 3;
      indent = 0;
    }
  else
    {
      /* Print name of expression code.  */
      if (flag_simple && GET_CODE (in_rtx) == CONST_INT)
	fputc ('(', outfile);
      else
	fprintf (outfile, "(%s", GET_RTX_NAME (GET_CODE (in_rtx)));

      if (! flag_simple)
	{
	  if (RTX_FLAG (in_rtx, in_struct))
	    fputs ("/s", outfile);

	  if (RTX_FLAG (in_rtx, volatil))
	    fputs ("/v", outfile);

	  if (RTX_FLAG (in_rtx, unchanging))
	    fputs ("/u", outfile);

	  if (RTX_FLAG (in_rtx, integrated))
	    fputs ("/i", outfile);

	  if (RTX_FLAG (in_rtx, frame_related))
	    fputs ("/f", outfile);

	  if (RTX_FLAG (in_rtx, jump))
	    fputs ("/j", outfile);

	  if (RTX_FLAG (in_rtx, call))
	    fputs ("/c", outfile);

	  if (GET_MODE (in_rtx) != VOIDmode)
	    {
	      /* Print REG_NOTE names for EXPR_LIST and INSN_LIST.  */
	      if (GET_CODE (in_rtx) == EXPR_LIST
		  || GET_CODE (in_rtx) == INSN_LIST)
		fprintf (outfile, ":%s",
			 GET_REG_NOTE_NAME (GET_MODE (in_rtx)));
	      else
		fprintf (outfile, ":%s", GET_MODE_NAME (GET_MODE (in_rtx)));
	    }
	}
    }

#ifndef GENERATOR_FILE
  if (GET_CODE (in_rtx) == CONST_DOUBLE && FLOAT_MODE_P (GET_MODE (in_rtx)))
    i = 5;
#endif

  /* Get the format string and skip the first elements if we have handled
     them already.  */
  format_ptr = GET_RTX_FORMAT (GET_CODE (in_rtx)) + i;
  for (; i < GET_RTX_LENGTH (GET_CODE (in_rtx)); i++)
    switch (*format_ptr++)
      {
	const char *str;

      case 'T':
	str = XTMPL (in_rtx, i);
	goto string;

      case 'S':
      case 's':
	str = XSTR (in_rtx, i);
      string:

	if (str == 0)
	  fputs (dump_for_graph ? " \\\"\\\"" : " \"\"", outfile);
	else
	  {
	    if (dump_for_graph)
	      fprintf (outfile, " (\\\"%s\\\")", str);
	    else
	      fprintf (outfile, " (\"%s\")", str);
	  }
	sawclose = 1;
	break;

	/* 0 indicates a field for internal use that should not be printed.
	   An exception is the third field of a NOTE, where it indicates
	   that the field has several different valid contents.  */
      case '0':
	if (i == 1 && GET_CODE (in_rtx) == REG)
	  {
	    if (REGNO (in_rtx) != ORIGINAL_REGNO (in_rtx))
	      fprintf (outfile, " [%d]", ORIGINAL_REGNO (in_rtx));
	    break;
	  }
	if (i == 4 && GET_CODE (in_rtx) == NOTE)
	  {
	    switch (NOTE_LINE_NUMBER (in_rtx))
	      {
	      case NOTE_INSN_EH_REGION_BEG:
	      case NOTE_INSN_EH_REGION_END:
		if (flag_dump_unnumbered)
		  fprintf (outfile, " #");
		else
		  fprintf (outfile, " %d", NOTE_EH_HANDLER (in_rtx));
		sawclose = 1;
		break;

	      case NOTE_INSN_BLOCK_BEG:
	      case NOTE_INSN_BLOCK_END:
		fprintf (outfile, " ");
		if (flag_dump_unnumbered)
		  fprintf (outfile, "#");
		else
		  fprintf (outfile, HOST_PTR_PRINTF,
			   (char *) NOTE_BLOCK (in_rtx));
		sawclose = 1;
		break;

	      case NOTE_INSN_BASIC_BLOCK:
		{
		  basic_block bb = NOTE_BASIC_BLOCK (in_rtx);
		  if (bb != 0)
		    fprintf (outfile, " [bb %d]", bb->index);
		  break;
	        }

	      case NOTE_INSN_EXPECTED_VALUE:
		indent += 2;
		if (!sawclose)
		  fprintf (outfile, " ");
		print_rtx (NOTE_EXPECTED_VALUE (in_rtx));
		indent -= 2;
		break;

	      case NOTE_INSN_DELETED_LABEL:
		if (NOTE_SOURCE_FILE (in_rtx))
		  fprintf (outfile, " (\"%s\")", NOTE_SOURCE_FILE (in_rtx));
		else
		  fprintf (outfile, " \"\"");
		break;

	      case NOTE_INSN_PREDICTION:
		if (NOTE_PREDICTION (in_rtx))
		  fprintf (outfile, " [ %d %d ] ",
			   (int)NOTE_PREDICTION_ALG (in_rtx),
			   (int) NOTE_PREDICTION_FLAGS (in_rtx));
		else
		  fprintf (outfile, " [ ERROR ]");
		break;

	      default:
		{
		  const char * const str = X0STR (in_rtx, i);

		  if (NOTE_LINE_NUMBER (in_rtx) < 0)
		    ;
		  else if (str == 0)
		    fputs (dump_for_graph ? " \\\"\\\"" : " \"\"", outfile);
		  else
		    {
		      if (dump_for_graph)
		        fprintf (outfile, " (\\\"%s\\\")", str);
		      else
		        fprintf (outfile, " (\"%s\")", str);
		    }
		  break;
		}
	      }
	  }
	break;

      case 'e':
      do_e:
	indent += 2;
	if (!sawclose)
	  fprintf (outfile, " ");
	print_rtx (XEXP (in_rtx, i));
	indent -= 2;
	break;

      case 'E':
      case 'V':
	indent += 2;
	if (sawclose)
	  {
	    fprintf (outfile, "\n%s%*s",
		     print_rtx_head, indent * 2, "");
	    sawclose = 0;
	  }
	fputs (" [", outfile);
	if (NULL != XVEC (in_rtx, i))
	  {
	    indent += 2;
	    if (XVECLEN (in_rtx, i))
	      sawclose = 1;

	    for (j = 0; j < XVECLEN (in_rtx, i); j++)
	      print_rtx (XVECEXP (in_rtx, i, j));

	    indent -= 2;
	  }
	if (sawclose)
	  fprintf (outfile, "\n%s%*s", print_rtx_head, indent * 2, "");

	fputs ("]", outfile);
	sawclose = 1;
	indent -= 2;
	break;

      case 'w':
	if (! flag_simple)
	  fprintf (outfile, " ");
	fprintf (outfile, HOST_WIDE_INT_PRINT_DEC, XWINT (in_rtx, i));
	if (! flag_simple)
	  {
	    fprintf (outfile, " [");
	    fprintf (outfile, HOST_WIDE_INT_PRINT_HEX, XWINT (in_rtx, i));
	    fprintf (outfile, "]");
	  }
	break;

      case 'i':
	if (i == 6 && GET_CODE (in_rtx) == NOTE)
	  {
	    /* This field is only used for NOTE_INSN_DELETED_LABEL, and
	       other times often contains garbage from INSN->NOTE death.  */
	    if (NOTE_LINE_NUMBER (in_rtx) == NOTE_INSN_DELETED_LABEL)
	      fprintf (outfile, " %d",  XINT (in_rtx, i));
	  }
	else
	  {
	    int value = XINT (in_rtx, i);
	    const char *name;

	    if (GET_CODE (in_rtx) == REG && value < FIRST_PSEUDO_REGISTER)
	      {
		fputc (' ', outfile);
		DEBUG_PRINT_REG (in_rtx, 0, outfile);
	      }
	    else if (GET_CODE (in_rtx) == REG
		     && value <= LAST_VIRTUAL_REGISTER)
	      {
		if (value == VIRTUAL_INCOMING_ARGS_REGNUM)
		  fprintf (outfile, " %d virtual-incoming-args", value);
		else if (value == VIRTUAL_STACK_VARS_REGNUM)
		  fprintf (outfile, " %d virtual-stack-vars", value);
		else if (value == VIRTUAL_STACK_DYNAMIC_REGNUM)
		  fprintf (outfile, " %d virtual-stack-dynamic", value);
		else if (value == VIRTUAL_OUTGOING_ARGS_REGNUM)
		  fprintf (outfile, " %d virtual-outgoing-args", value);
		else if (value == VIRTUAL_CFA_REGNUM)
		  fprintf (outfile, " %d virtual-cfa", value);
		else
		  fprintf (outfile, " %d virtual-reg-%d", value,
			   value-FIRST_VIRTUAL_REGISTER);
	      }
	    else if (flag_dump_unnumbered
		     && (is_insn || GET_CODE (in_rtx) == NOTE))
	      fputc ('#', outfile);
	    else
	      fprintf (outfile, " %d", value);

	    if (is_insn && &INSN_CODE (in_rtx) == &XINT (in_rtx, i)
		&& XINT (in_rtx, i) >= 0
		&& (name = get_insn_name (XINT (in_rtx, i))) != NULL)
	      fprintf (outfile, " {%s}", name);
	    sawclose = 0;
	  }
	break;

      /* Print NOTE_INSN names rather than integer codes.  */

      case 'n':
	if (XINT (in_rtx, i) >= (int) NOTE_INSN_BIAS
	    && XINT (in_rtx, i) < (int) NOTE_INSN_MAX)
	  fprintf (outfile, " %s", GET_NOTE_INSN_NAME (XINT (in_rtx, i)));
	else
	  fprintf (outfile, " %d", XINT (in_rtx, i));
	sawclose = 0;
	break;

      case 'u':
	if (XEXP (in_rtx, i) != NULL)
	  {
	    rtx sub = XEXP (in_rtx, i);
	    enum rtx_code subc = GET_CODE (sub);

	    if (GET_CODE (in_rtx) == LABEL_REF)
	      {
		if (subc == NOTE
		    && NOTE_LINE_NUMBER (sub) == NOTE_INSN_DELETED_LABEL)
		  {
		    if (flag_dump_unnumbered)
		      fprintf (outfile, " [# deleted]");
		    else
		      fprintf (outfile, " [%d deleted]", INSN_UID (sub));
		    sawclose = 0;
		    break;
		  }

		if (subc != CODE_LABEL)
		  goto do_e;
	      }

	    if (flag_dump_unnumbered)
	      fputs (" #", outfile);
	    else
	      fprintf (outfile, " %d", INSN_UID (sub));
	  }
	else
	  fputs (" 0", outfile);
	sawclose = 0;
	break;

      case 'b':
	if (XBITMAP (in_rtx, i) == NULL)
	  fputs (" {null}", outfile);
	else
	  bitmap_print (outfile, XBITMAP (in_rtx, i), " {", "}");
	sawclose = 0;
	break;

      case 't':
	putc (' ', outfile);
	fprintf (outfile, HOST_PTR_PRINTF, (char *) XTREE (in_rtx, i));
	break;

      case '*':
	fputs (" Unknown", outfile);
	sawclose = 0;
	break;

      case 'B':
	if (XBBDEF (in_rtx, i))
	  fprintf (outfile, " %i", XBBDEF (in_rtx, i)->index);
	break;

      default:
	fprintf (stderr,
		 "switch format wrong in rtl.print_rtx(). format was: %c.\n",
		 format_ptr[-1]);
	abort ();
      }

  switch (GET_CODE (in_rtx))
    {
#ifndef GENERATOR_FILE
    case MEM:
      fputs (" [", outfile);
      fprintf (outfile, HOST_WIDE_INT_PRINT_DEC, MEM_ALIAS_SET (in_rtx));

      if (MEM_EXPR (in_rtx))
	print_mem_expr (outfile, MEM_EXPR (in_rtx));

      if (MEM_OFFSET (in_rtx))
	{
	  fputc ('+', outfile);
	  fprintf (outfile, HOST_WIDE_INT_PRINT_DEC,
		   INTVAL (MEM_OFFSET (in_rtx)));
	}

      if (MEM_SIZE (in_rtx))
	{
	  fputs (" S", outfile);
	  fprintf (outfile, HOST_WIDE_INT_PRINT_DEC,
		   INTVAL (MEM_SIZE (in_rtx)));
	}

      if (MEM_ALIGN (in_rtx) != 1)
	fprintf (outfile, " A%u", MEM_ALIGN (in_rtx));

      fputc (']', outfile);
      break;

    case CONST_DOUBLE:
      if (FLOAT_MODE_P (GET_MODE (in_rtx)))
	{
	  char s[60];

	  real_to_decimal (s, CONST_DOUBLE_REAL_VALUE (in_rtx),
			   sizeof (s), 0, 1);
	  fprintf (outfile, " %s", s);

	  real_to_hexadecimal (s, CONST_DOUBLE_REAL_VALUE (in_rtx),
			       sizeof (s), 0, 1);
	  fprintf (outfile, " [%s]", s);
	}
      break;
#endif

    case CODE_LABEL:
      fprintf (outfile, " [%d uses]", LABEL_NUSES (in_rtx));
      switch (LABEL_KIND (in_rtx))
	{
	  case LABEL_NORMAL: break;
	  case LABEL_STATIC_ENTRY: fputs (" [entry]", outfile); break;
	  case LABEL_GLOBAL_ENTRY: fputs (" [global entry]", outfile); break;
	  case LABEL_WEAK_ENTRY: fputs (" [weak entry]", outfile); break;
	  default: abort();
	}
      break;

    case CALL_PLACEHOLDER:
      if (debug_call_placeholder_verbose)
	{
	  fputs (" (cond [\n  (const_string \"normal\") (sequence [", outfile);
	  for (tem = XEXP (in_rtx, 0); tem != 0; tem = NEXT_INSN (tem))
	    {
	      fputs ("\n    ", outfile);
	      print_inline_rtx (outfile, tem, 4);
	    }

	  tem = XEXP (in_rtx, 1);
	  if (tem)
	    fputs ("\n    ])\n  (const_string \"tail_call\") (sequence [",
		   outfile);
	  for (; tem != 0; tem = NEXT_INSN (tem))
	    {
	      fputs ("\n    ", outfile);
	      print_inline_rtx (outfile, tem, 4);
	    }

	  tem = XEXP (in_rtx, 2);
	  if (tem)
	    fputs ("\n    ])\n  (const_string \"tail_recursion\") (sequence [",
		   outfile);
	  for (; tem != 0; tem = NEXT_INSN (tem))
	    {
	      fputs ("\n    ", outfile);
	      print_inline_rtx (outfile, tem, 4);
	    }

	  fputs ("\n    ])\n  ])", outfile);
	  break;
	}

      for (tem = XEXP (in_rtx, 0); tem != 0; tem = NEXT_INSN (tem))
	if (GET_CODE (tem) == CALL_INSN)
	  {
	    fprintf (outfile, " ");
	    print_rtx (tem);
	    break;
	  }
      break;

    default:
      break;
    }

  if (dump_for_graph
      && (is_insn || GET_CODE (in_rtx) == NOTE
	  || GET_CODE (in_rtx) == CODE_LABEL || GET_CODE (in_rtx) == BARRIER))
    sawclose = 0;
  else
    {
      fputc (')', outfile);
      sawclose = 1;
    }
}

/* Print an rtx on the current line of FILE.  Initially indent IND
   characters.  */

void
print_inline_rtx (outf, x, ind)
     FILE *outf;
     rtx x;
     int ind;
{
  int oldsaw = sawclose;
  int oldindent = indent;

  sawclose = 0;
  indent = ind;
  outfile = outf;
  print_rtx (x);
  sawclose = oldsaw;
  indent = oldindent;
}

/* Call this function from the debugger to see what X looks like.  */

void
debug_rtx (x)
     rtx x;
{
  outfile = stderr;
  sawclose = 0;
  print_rtx (x);
  fprintf (stderr, "\n");
}

/* Count of rtx's to print with debug_rtx_list.
   This global exists because gdb user defined commands have no arguments.  */

int debug_rtx_count = 0;	/* 0 is treated as equivalent to 1 */

/* Call this function to print list from X on.

   N is a count of the rtx's to print. Positive values print from the specified
   rtx on.  Negative values print a window around the rtx.
   EG: -5 prints 2 rtx's on either side (in addition to the specified rtx).  */

void
debug_rtx_list (x, n)
     rtx x;
     int n;
{
  int i,count;
  rtx insn;

  count = n == 0 ? 1 : n < 0 ? -n : n;

  /* If we are printing a window, back up to the start.  */

  if (n < 0)
    for (i = count / 2; i > 0; i--)
      {
	if (PREV_INSN (x) == 0)
	  break;
	x = PREV_INSN (x);
      }

  for (i = count, insn = x; i > 0 && insn != 0; i--, insn = NEXT_INSN (insn))
    {
      debug_rtx (insn);
      fprintf (stderr, "\n");
    }
}

/* Call this function to print an rtx list from START to END inclusive.  */

void
debug_rtx_range (start, end)
     rtx start, end;
{
  while (1)
    {
      debug_rtx (start);
      fprintf (stderr, "\n");
      if (!start || start == end)
	break;
      start = NEXT_INSN (start);
    }
}

/* Call this function to search an rtx list to find one with insn uid UID,
   and then call debug_rtx_list to print it, using DEBUG_RTX_COUNT.
   The found insn is returned to enable further debugging analysis.  */

rtx
debug_rtx_find (x, uid)
     rtx x;
     int uid;
{
  while (x != 0 && INSN_UID (x) != uid)
    x = NEXT_INSN (x);
  if (x != 0)
    {
      debug_rtx_list (x, debug_rtx_count);
      return x;
    }
  else
    {
      fprintf (stderr, "insn uid %d not found\n", uid);
      return 0;
    }
}

/* External entry point for printing a chain of insns
   starting with RTX_FIRST onto file OUTF.
   A blank line separates insns.

   If RTX_FIRST is not an insn, then it alone is printed, with no newline.  */

void
print_rtl (outf, rtx_first)
     FILE *outf;
     rtx rtx_first;
{
  rtx tmp_rtx;

  outfile = outf;
  sawclose = 0;

  if (rtx_first == 0)
    {
      fputs (print_rtx_head, outf);
      fputs ("(nil)\n", outf);
    }
  else
    switch (GET_CODE (rtx_first))
      {
      case INSN:
      case JUMP_INSN:
      case CALL_INSN:
      case NOTE:
      case CODE_LABEL:
      case BARRIER:
	for (tmp_rtx = rtx_first; tmp_rtx != 0; tmp_rtx = NEXT_INSN (tmp_rtx))
	  if (! flag_dump_unnumbered
	      || GET_CODE (tmp_rtx) != NOTE || NOTE_LINE_NUMBER (tmp_rtx) < 0)
	    {
	      fputs (print_rtx_head, outfile);
	      print_rtx (tmp_rtx);
	      fprintf (outfile, "\n");
	    }
	break;

      default:
	fputs (print_rtx_head, outfile);
	print_rtx (rtx_first);
      }
}

/* Like print_rtx, except specify a file.  */
/* Return nonzero if we actually printed anything.  */

int
print_rtl_single (outf, x)
     FILE *outf;
     rtx x;
{
  outfile = outf;
  sawclose = 0;
  if (! flag_dump_unnumbered
      || GET_CODE (x) != NOTE || NOTE_LINE_NUMBER (x) < 0)
    {
      fputs (print_rtx_head, outfile);
      print_rtx (x);
      putc ('\n', outf);
      return 1;
    }
  return 0;
}


/* Like print_rtl except without all the detail; for example,
   if RTX is a CONST_INT then print in decimal format.  */

void
print_simple_rtl (outf, x)
     FILE *outf;
     rtx x;
{
  flag_simple = 1;
  print_rtl (outf, x);
  flag_simple = 0;
}
