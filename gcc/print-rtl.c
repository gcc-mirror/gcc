/* Print RTL for GCC.
   Copyright (C) 1987, 1988, 1992, 1997, 1998, 1999, 2000, 2002, 2003,
   2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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

/* This file is compiled twice: once for the generator programs,
   once for the compiler.  */
#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"

/* These headers all define things which are not available in
   generator programs.  */
#ifndef GENERATOR_FILE
#include "tree.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "cselib.h"
#include "dumpfile.h"	/* for dump_flags */
#include "dwarf2out.h"
#endif

static FILE *outfile;

static int sawclose = 0;

static int indent;

static void print_rtx (const_rtx);

/* String printed at beginning of each RTL when it is dumped.
   This string is set to ASM_COMMENT_START when the RTL is dumped in
   the assembly output file.  */
const char *print_rtx_head = "";

#ifdef GENERATOR_FILE
/* These are defined from the .opt file when not used in generator
   programs.  */

/* Nonzero means suppress output of instruction numbers
   in debugging dumps.
   This must be defined here so that programs like gencodes can be linked.  */
int flag_dump_unnumbered = 0;

/* Nonzero means suppress output of instruction numbers for previous
   and next insns in debugging dumps.
   This must be defined here so that programs like gencodes can be linked.  */
int flag_dump_unnumbered_links = 0;
#endif

/* Nonzero means use simplified format without flags, modes, etc.  */
int flag_simple = 0;

/* Nonzero if we are dumping graphical description.  */
int dump_for_graph;

#ifndef GENERATOR_FILE
void
print_mem_expr (FILE *outfile, const_tree expr)
{
  fputc (' ', outfile);
  print_generic_expr (outfile, CONST_CAST_TREE (expr), dump_flags);
}
#endif

/* Print IN_RTX onto OUTFILE.  This is the recursive part of printing.  */

static void
print_rtx (const_rtx in_rtx)
{
  int i = 0;
  int j;
  const char *format_ptr;
  int is_insn;

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
       fprintf (outfile, "(??? bad code %d\n%s%*s)", GET_CODE (in_rtx),
		print_rtx_head, indent * 2, "");
       sawclose = 1;
       return;
    }

  is_insn = INSN_P (in_rtx);

  /* When printing in VCG format we write INSNs, NOTE, LABEL, and BARRIER
     in separate nodes and therefore have to handle them special here.  */
  if (dump_for_graph
      && (is_insn || NOTE_P (in_rtx)
	  || LABEL_P (in_rtx) || BARRIER_P (in_rtx)))
    {
      i = 3;
      indent = 0;
    }
  else
    {
      /* Print name of expression code.  */
      if (flag_simple && CONST_INT_P (in_rtx))
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

	  if (RTX_FLAG (in_rtx, frame_related))
	    fputs ("/f", outfile);

	  if (RTX_FLAG (in_rtx, jump))
	    fputs ("/j", outfile);

	  if (RTX_FLAG (in_rtx, call))
	    fputs ("/c", outfile);

	  if (RTX_FLAG (in_rtx, return_val))
	    fputs ("/i", outfile);

	  /* Print REG_NOTE names for EXPR_LIST and INSN_LIST.  */
	  if ((GET_CODE (in_rtx) == EXPR_LIST
	       || GET_CODE (in_rtx) == INSN_LIST)
	      && (int)GET_MODE (in_rtx) < REG_NOTE_MAX)
	    fprintf (outfile, ":%s",
		     GET_REG_NOTE_NAME (GET_MODE (in_rtx)));

	  /* For other rtl, print the mode if it's not VOID.  */
	  else if (GET_MODE (in_rtx) != VOIDmode)
	    fprintf (outfile, ":%s", GET_MODE_NAME (GET_MODE (in_rtx)));

#ifndef GENERATOR_FILE
	  if (GET_CODE (in_rtx) == VAR_LOCATION)
	    {
	      if (TREE_CODE (PAT_VAR_LOCATION_DECL (in_rtx)) == STRING_CST)
		fputs (" <debug string placeholder>", outfile);
	      else
		print_mem_expr (outfile, PAT_VAR_LOCATION_DECL (in_rtx));
	      fputc (' ', outfile);
	      print_rtx (PAT_VAR_LOCATION_LOC (in_rtx));
	      if (PAT_VAR_LOCATION_STATUS (in_rtx)
		  == VAR_INIT_STATUS_UNINITIALIZED)
		fprintf (outfile, " [uninit]");
	      sawclose = 1;
	      i = GET_RTX_LENGTH (VAR_LOCATION);
	    }
#endif
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
	if (i == 1 && REG_P (in_rtx))
	  {
	    if (REGNO (in_rtx) != ORIGINAL_REGNO (in_rtx))
	      fprintf (outfile, " [%d]", ORIGINAL_REGNO (in_rtx));
	  }
#ifndef GENERATOR_FILE
	else if (i == 1 && GET_CODE (in_rtx) == SYMBOL_REF)
	  {
	    int flags = SYMBOL_REF_FLAGS (in_rtx);
	    if (flags)
	      fprintf (outfile, " [flags %#x]", flags);
	  }
	else if (i == 2 && GET_CODE (in_rtx) == SYMBOL_REF)
	  {
	    tree decl = SYMBOL_REF_DECL (in_rtx);
	    if (decl)
	      print_node_brief (outfile, "", decl, dump_flags);
	  }
#endif
	else if (i == 4 && NOTE_P (in_rtx))
	  {
	    switch (NOTE_KIND (in_rtx))
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
#ifndef GENERATOR_FILE
		dump_addr (outfile, " ", NOTE_BLOCK (in_rtx));
#endif
		sawclose = 1;
		break;

	      case NOTE_INSN_BASIC_BLOCK:
		{
#ifndef GENERATOR_FILE
		  basic_block bb = NOTE_BASIC_BLOCK (in_rtx);
		  if (bb != 0)
		    fprintf (outfile, " [bb %d]", bb->index);
#endif
		  break;
	        }

	      case NOTE_INSN_DELETED_LABEL:
	      case NOTE_INSN_DELETED_DEBUG_LABEL:
		{
		  const char *label = NOTE_DELETED_LABEL_NAME (in_rtx);
		  if (label)
		    fprintf (outfile, " (\"%s\")", label);
		  else
		    fprintf (outfile, " \"\"");
		}
		break;

	      case NOTE_INSN_SWITCH_TEXT_SECTIONS:
		{
#ifndef GENERATOR_FILE
		  basic_block bb = NOTE_BASIC_BLOCK (in_rtx);
		  if (bb != 0)
		    fprintf (outfile, " [bb %d]", bb->index);
#endif
		  break;
		}

	      case NOTE_INSN_VAR_LOCATION:
	      case NOTE_INSN_CALL_ARG_LOCATION:
#ifndef GENERATOR_FILE
		fputc (' ', outfile);
		print_rtx (NOTE_VAR_LOCATION (in_rtx));
#endif
		break;

	      case NOTE_INSN_CFI:
#ifndef GENERATOR_FILE
		fputc ('\n', outfile);
		output_cfi_directive (outfile, NOTE_CFI (in_rtx));
		fputc ('\t', outfile);
#endif
		break;

	      default:
		break;
	      }
	  }
	else if (i == 8 && JUMP_P (in_rtx) && JUMP_LABEL (in_rtx) != NULL)
	  {
	    /* Output the JUMP_LABEL reference.  */
	    fprintf (outfile, "\n%s%*s -> ", print_rtx_head, indent * 2, "");
	    if (GET_CODE (JUMP_LABEL (in_rtx)) == RETURN)
	      fprintf (outfile, "return");
	    else if (GET_CODE (JUMP_LABEL (in_rtx)) == SIMPLE_RETURN)
	      fprintf (outfile, "simple_return");
	    else
	      fprintf (outfile, "%d", INSN_UID (JUMP_LABEL (in_rtx)));
	  }
	else if (i == 0 && GET_CODE (in_rtx) == VALUE)
	  {
#ifndef GENERATOR_FILE
	    cselib_val *val = CSELIB_VAL_PTR (in_rtx);

	    fprintf (outfile, " %u:%u", val->uid, val->hash);
	    dump_addr (outfile, " @", in_rtx);
	    dump_addr (outfile, "/", (void*)val);
#endif
	  }
	else if (i == 0 && GET_CODE (in_rtx) == DEBUG_EXPR)
	  {
#ifndef GENERATOR_FILE
	    fprintf (outfile, " D#%i",
		     DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (in_rtx)));
#endif
	  }
	else if (i == 0 && GET_CODE (in_rtx) == ENTRY_VALUE)
	  {
	    indent += 2;
	    if (!sawclose)
	      fprintf (outfile, " ");
	    print_rtx (ENTRY_VALUE_EXP (in_rtx));
	    indent -= 2;
	  }
	break;

      case 'e':
      do_e:
	indent += 2;
	if (i == 7 && INSN_P (in_rtx))
	  /* Put REG_NOTES on their own line.  */
	  fprintf (outfile, "\n%s%*s",
		   print_rtx_head, indent * 2, "");
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
	  fprintf (outfile, " [" HOST_WIDE_INT_PRINT_HEX "]",
		   (unsigned HOST_WIDE_INT) XWINT (in_rtx, i));
	break;

      case 'i':
	if (i == 5 && INSN_P (in_rtx))
	  {
#ifndef GENERATOR_FILE
	    /*  Pretty-print insn locators.  Ignore scoping as it is mostly
		redundant with line number information and do not print anything
		when there is no location information available.  */
	    if (INSN_LOCATOR (in_rtx) && insn_file (in_rtx))
	      fprintf(outfile, " %s:%i", insn_file (in_rtx), insn_line (in_rtx));
#endif
	  }
	else if (i == 6 && GET_CODE (in_rtx) == ASM_OPERANDS)
	  {
#ifndef GENERATOR_FILE
	    fprintf (outfile, " %s:%i",
		     locator_file (ASM_OPERANDS_SOURCE_LOCATION (in_rtx)),
		     locator_line (ASM_OPERANDS_SOURCE_LOCATION (in_rtx)));
#endif
	  }
	else if (i == 1 && GET_CODE (in_rtx) == ASM_INPUT)
	  {
#ifndef GENERATOR_FILE
	    fprintf (outfile, " %s:%i",
		     locator_file (ASM_INPUT_SOURCE_LOCATION (in_rtx)),
		     locator_line (ASM_INPUT_SOURCE_LOCATION (in_rtx)));
#endif
	  }
	else if (i == 6 && NOTE_P (in_rtx))
	  {
	    /* This field is only used for NOTE_INSN_DELETED_LABEL, and
	       other times often contains garbage from INSN->NOTE death.  */
	    if (NOTE_KIND (in_rtx) == NOTE_INSN_DELETED_LABEL
		|| NOTE_KIND (in_rtx) == NOTE_INSN_DELETED_DEBUG_LABEL)
	      fprintf (outfile, " %d",  XINT (in_rtx, i));
	  }
#if !defined(GENERATOR_FILE) && NUM_UNSPECV_VALUES > 0
	else if (i == 1
		 && GET_CODE (in_rtx) == UNSPEC_VOLATILE
		 && XINT (in_rtx, 1) >= 0
		 && XINT (in_rtx, 1) < NUM_UNSPECV_VALUES)
	  fprintf (outfile, " %s", unspecv_strings[XINT (in_rtx, 1)]);
#endif
#if !defined(GENERATOR_FILE) && NUM_UNSPEC_VALUES > 0
	else if (i == 1
		 && (GET_CODE (in_rtx) == UNSPEC
		     || GET_CODE (in_rtx) == UNSPEC_VOLATILE)
		 && XINT (in_rtx, 1) >= 0
		 && XINT (in_rtx, 1) < NUM_UNSPEC_VALUES)
	  fprintf (outfile, " %s", unspec_strings[XINT (in_rtx, 1)]);
#endif
	else
	  {
	    int value = XINT (in_rtx, i);
	    const char *name;

#ifndef GENERATOR_FILE
	    if (REG_P (in_rtx) && (unsigned) value < FIRST_PSEUDO_REGISTER)
	      fprintf (outfile, " %d %s", value, reg_names[value]);
	    else if (REG_P (in_rtx)
		     && (unsigned) value <= LAST_VIRTUAL_REGISTER)
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
		else if (value == VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM)
		  fprintf (outfile, " %d virtual-preferred-stack-boundary",
			   value);
		else
		  fprintf (outfile, " %d virtual-reg-%d", value,
			   value-FIRST_VIRTUAL_REGISTER);
	      }
	    else
#endif
	      if (flag_dump_unnumbered
		     && (is_insn || NOTE_P (in_rtx)))
	      fputc ('#', outfile);
	    else
	      fprintf (outfile, " %d", value);

#ifndef GENERATOR_FILE
	    if (REG_P (in_rtx) && REG_ATTRS (in_rtx))
	      {
		fputs (" [", outfile);
		if (ORIGINAL_REGNO (in_rtx) != REGNO (in_rtx))
		  fprintf (outfile, "orig:%i", ORIGINAL_REGNO (in_rtx));
		if (REG_EXPR (in_rtx))
		  print_mem_expr (outfile, REG_EXPR (in_rtx));

		if (REG_OFFSET (in_rtx))
		  fprintf (outfile, "+" HOST_WIDE_INT_PRINT_DEC,
			   REG_OFFSET (in_rtx));
		fputs (" ]", outfile);
	      }
#endif

	    if (is_insn && &INSN_CODE (in_rtx) == &XINT (in_rtx, i)
		&& XINT (in_rtx, i) >= 0
		&& (name = get_insn_name (XINT (in_rtx, i))) != NULL)
	      fprintf (outfile, " {%s}", name);
	    sawclose = 0;
	  }
	break;

      /* Print NOTE_INSN names rather than integer codes.  */

      case 'n':
	fprintf (outfile, " %s", GET_NOTE_INSN_NAME (XINT (in_rtx, i)));
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
		    && NOTE_KIND (sub) == NOTE_INSN_DELETED_LABEL)
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

	    if (flag_dump_unnumbered
		|| (flag_dump_unnumbered_links && (i == 1 || i == 2)
		    && (INSN_P (in_rtx) || NOTE_P (in_rtx)
			|| LABEL_P (in_rtx) || BARRIER_P (in_rtx))))
	      fputs (" #", outfile);
	    else
	      fprintf (outfile, " %d", INSN_UID (sub));
	  }
	else
	  fputs (" 0", outfile);
	sawclose = 0;
	break;

      case 't':
#ifndef GENERATOR_FILE
	if (i == 0 && GET_CODE (in_rtx) == DEBUG_IMPLICIT_PTR)
	  print_mem_expr (outfile, DEBUG_IMPLICIT_PTR_DECL (in_rtx));
	else if (i == 0 && GET_CODE (in_rtx) == DEBUG_PARAMETER_REF)
	  print_mem_expr (outfile, DEBUG_PARAMETER_REF_DECL (in_rtx));
	else
	  dump_addr (outfile, " ", XTREE (in_rtx, i));
#endif
	break;

      case '*':
	fputs (" Unknown", outfile);
	sawclose = 0;
	break;

      case 'B':
#ifndef GENERATOR_FILE
	if (XBBDEF (in_rtx, i))
	  fprintf (outfile, " %i", XBBDEF (in_rtx, i)->index);
#endif
	break;

      default:
	gcc_unreachable ();
      }

  switch (GET_CODE (in_rtx))
    {
#ifndef GENERATOR_FILE
    case MEM:
      if (__builtin_expect (final_insns_dump_p, false))
	fprintf (outfile, " [");
      else
	fprintf (outfile, " [" HOST_WIDE_INT_PRINT_DEC,
		 (HOST_WIDE_INT) MEM_ALIAS_SET (in_rtx));

      if (MEM_EXPR (in_rtx))
	print_mem_expr (outfile, MEM_EXPR (in_rtx));

      if (MEM_OFFSET_KNOWN_P (in_rtx))
	fprintf (outfile, "+" HOST_WIDE_INT_PRINT_DEC, MEM_OFFSET (in_rtx));

      if (MEM_SIZE_KNOWN_P (in_rtx))
	fprintf (outfile, " S" HOST_WIDE_INT_PRINT_DEC, MEM_SIZE (in_rtx));

      if (MEM_ALIGN (in_rtx) != 1)
	fprintf (outfile, " A%u", MEM_ALIGN (in_rtx));

      if (!ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (in_rtx)))
	fprintf (outfile, " AS%u", MEM_ADDR_SPACE (in_rtx));

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
	  default: gcc_unreachable ();
	}
      break;

    default:
      break;
    }

  if (dump_for_graph
      && (is_insn || NOTE_P (in_rtx)
	  || LABEL_P (in_rtx) || BARRIER_P (in_rtx)))
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
print_inline_rtx (FILE *outf, const_rtx x, int ind)
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

DEBUG_FUNCTION void
debug_rtx (const_rtx x)
{
  outfile = stderr;
  sawclose = 0;
  print_rtx (x);
  fprintf (stderr, "\n");
}

/* Count of rtx's to print with debug_rtx_list.
   This global exists because gdb user defined commands have no arguments.  */

DEBUG_VARIABLE int debug_rtx_count = 0;	/* 0 is treated as equivalent to 1 */

/* Call this function to print list from X on.

   N is a count of the rtx's to print. Positive values print from the specified
   rtx on.  Negative values print a window around the rtx.
   EG: -5 prints 2 rtx's on either side (in addition to the specified rtx).  */

DEBUG_FUNCTION void
debug_rtx_list (const_rtx x, int n)
{
  int i,count;
  const_rtx insn;

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

DEBUG_FUNCTION void
debug_rtx_range (const_rtx start, const_rtx end)
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

DEBUG_FUNCTION const_rtx
debug_rtx_find (const_rtx x, int uid)
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
print_rtl (FILE *outf, const_rtx rtx_first)
{
  const_rtx tmp_rtx;

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
print_rtl_single (FILE *outf, const_rtx x)
{
  outfile = outf;
  sawclose = 0;
  fputs (print_rtx_head, outfile);
  print_rtx (x);
  putc ('\n', outf);
  return 1;
}


/* Like print_rtl except without all the detail; for example,
   if RTX is a CONST_INT then print in decimal format.  */

void
print_simple_rtl (FILE *outf, const_rtx x)
{
  flag_simple = 1;
  print_rtl (outf, x);
  flag_simple = 0;
}
