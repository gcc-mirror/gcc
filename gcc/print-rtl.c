/* Print RTL for GNU C Compiler.
   Copyright (C) 1987-1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include <ctype.h>
#include <stdio.h>
#include "rtl.h"


/* How to print out a register name.
   We don't use PRINT_REG because some definitions of PRINT_REG
   don't work here.  */
#ifndef DEBUG_PRINT_REG
#define DEBUG_PRINT_REG(RTX, CODE, FILE) \
  fprintf ((FILE), "%d %s", REGNO (RTX), reg_names[REGNO (RTX)])
#endif

/* Array containing all of the register names */

#ifdef DEBUG_REGISTER_NAMES
static char *reg_names[] = DEBUG_REGISTER_NAMES;
#else
static char *reg_names[] = REGISTER_NAMES;
#endif

static FILE *outfile;

char spaces[] = "                                                                                                                                                                ";

static int sawclose = 0;

/* Names for patterns.  Non-zero only when linked with insn-output.c.  */

extern char **insn_name_ptr;

/* Print IN_RTX onto OUTFILE.  This is the recursive part of printing.  */

static void
print_rtx (in_rtx)
     register rtx in_rtx;
{
  static int indent;
  register int i, j;
  register char *format_ptr;
  register int is_insn;

  if (sawclose)
    {
      fprintf (outfile, "\n%s",
	       (spaces + (sizeof spaces - 1 - indent * 2)));
      sawclose = 0;
    }

  if (in_rtx == 0)
    {
      fprintf (outfile, "(nil)");
      sawclose = 1;
      return;
    }

  /* print name of expression code */
  fprintf (outfile, "(%s", GET_RTX_NAME (GET_CODE (in_rtx)));

  if (in_rtx->in_struct)
    fprintf (outfile, "/s");

  if (in_rtx->volatil)
    fprintf (outfile, "/v");

  if (in_rtx->unchanging)
    fprintf (outfile, "/u");

  if (in_rtx->integrated)
    fprintf (outfile, "/i");

  if (GET_MODE (in_rtx) != VOIDmode)
    {
      /* Print REG_NOTE names for EXPR_LIST and INSN_LIST.  */
      if (GET_CODE (in_rtx) == EXPR_LIST || GET_CODE (in_rtx) == INSN_LIST)
	fprintf (outfile, ":%s", GET_REG_NOTE_NAME (GET_MODE (in_rtx)));
      else
	fprintf (outfile, ":%s", GET_MODE_NAME (GET_MODE (in_rtx)));
    }

  is_insn = (GET_RTX_CLASS (GET_CODE (in_rtx)) == 'i');
  format_ptr = GET_RTX_FORMAT (GET_CODE (in_rtx));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (in_rtx)); i++)
    switch (*format_ptr++)
      {
      case 'S':
      case 's':
	if (XSTR (in_rtx, i) == 0)
	  fprintf (outfile, " \"\"");
	else
	  fprintf (outfile, " (\"%s\")", XSTR (in_rtx, i));
	sawclose = 1;
	break;

	/* 0 indicates a field for internal use that should not be printed.  */
      case '0':
	break;

      case 'e':
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
	    fprintf (outfile, "\n%s",
		     (spaces + (sizeof spaces - 1 - indent * 2)));
	    sawclose = 0;
	  }
	fprintf (outfile, "[ ");
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
	  fprintf (outfile, "\n%s",
		   (spaces + (sizeof spaces - 1 - indent * 2)));

	fprintf (outfile, "] ");
	sawclose = 1;
	indent -= 2;
	break;

      case 'w':
	fprintf (outfile,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 " %d",
#else
		 " %ld",
#endif
		 XWINT (in_rtx, i));
	break;

      case 'i':
	{
	  register int value = XINT (in_rtx, i);

	  if (GET_CODE (in_rtx) == REG && value < FIRST_PSEUDO_REGISTER)
	    {
	      fputc (' ', outfile);
	      DEBUG_PRINT_REG (in_rtx, 0, outfile);
	    }
	  else
	    fprintf (outfile, " %d", value);
	}
	if (is_insn && &INSN_CODE (in_rtx) == &XINT (in_rtx, i)
	    && insn_name_ptr
	    && XINT (in_rtx, i) >= 0)
	  fprintf (outfile, " {%s}", insn_name_ptr[XINT (in_rtx, i)]);
	sawclose = 0;
	break;

      /* Print NOTE_INSN names rather than integer codes.  */

      case 'n':
	if (XINT (in_rtx, i) <= 0)
	  fprintf (outfile, " %s", GET_NOTE_INSN_NAME (XINT (in_rtx, i)));
	else
	  fprintf (outfile, " %d", XINT (in_rtx, i));
	sawclose = 0;
	break;

      case 'u':
	if (XEXP (in_rtx, i) != NULL)
	  fprintf (outfile, " %d", INSN_UID (XEXP (in_rtx, i)));
	else
	  fprintf (outfile, " 0");
	sawclose = 0;
	break;

      case '*':
	fprintf (outfile, " Unknown");
	sawclose = 0;
	break;

      default:
	fprintf (stderr,
		 "switch format wrong in rtl.print_rtx(). format was: %c.\n",
		 format_ptr[-1]);
	abort ();
      }

  fprintf (outfile, ")");
  sawclose = 1;
}

/* Call this function from the debugger to see what X looks like.  */

void
debug_rtx (x)
     rtx x;
{
  outfile = stderr;
  print_rtx (x);
  fprintf (stderr, "\n");
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
  register rtx tmp_rtx;

  outfile = outf;
  sawclose = 0;

  if (rtx_first == 0)
    fprintf (outf, "(nil)\n");
  else
    switch (GET_CODE (rtx_first))
      {
      case INSN:
      case JUMP_INSN:
      case CALL_INSN:
      case NOTE:
      case CODE_LABEL:
      case BARRIER:
	for (tmp_rtx = rtx_first; NULL != tmp_rtx; tmp_rtx = NEXT_INSN (tmp_rtx))
	  {
	    print_rtx (tmp_rtx);
	    fprintf (outfile, "\n");
	  }
	break;

      default:
	print_rtx (rtx_first);
      }
}
