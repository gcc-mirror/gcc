/* Print RTL for GCC.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "alias.h"
#include "tree.h"
#include "cfg.h"
#include "print-tree.h"
#include "flags.h"
#include "predict.h"
#include "function.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "alloc-pool.h"
#include "cselib.h"
#include "dumpfile.h"	/* for dump_flags */
#include "dwarf2out.h"
#include "pretty-print.h"
#endif

#include "print-rtl.h"

static FILE *outfile;

static int sawclose = 0;

static int indent;

static bool in_call_function_usage;

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
	   || GET_CODE (in_rtx) == INSN_LIST
	   || GET_CODE (in_rtx) == INT_LIST)
	  && (int)GET_MODE (in_rtx) < REG_NOTE_MAX
	  && !in_call_function_usage)
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

#ifndef GENERATOR_FILE
  if (CONST_DOUBLE_AS_FLOAT_P (in_rtx))
    i = 5;
#endif

  if (INSN_CHAIN_CODE_P (GET_CODE (in_rtx)))
    {
      if (flag_dump_unnumbered)
	fprintf (outfile, " #");
      else
	fprintf (outfile, " %d", INSN_UID (in_rtx));
    }

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
	  fputs (" \"\"", outfile);
	else
	  fprintf (outfile, " (\"%s\")", str);
	sawclose = 1;
	break;

	/* 0 indicates a field for internal use that should not be printed.
	   An exception is the third field of a NOTE, where it indicates
	   that the field has several different valid contents.  */
      case '0':
#ifndef GENERATOR_FILE
	if (i == 1 && GET_CODE (in_rtx) == SYMBOL_REF)
	  {
	    int flags = SYMBOL_REF_FLAGS (in_rtx);
	    if (flags)
	      fprintf (outfile, " [flags %#x]", flags);
	    tree decl = SYMBOL_REF_DECL (in_rtx);
	    if (decl)
	      print_node_brief (outfile, "", decl, dump_flags);
	  }
	else if (i == 3 && NOTE_P (in_rtx))
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
		dump_addr (outfile, " ", NOTE_BLOCK (in_rtx));
		sawclose = 1;
		break;

	      case NOTE_INSN_BASIC_BLOCK:
		{
		  basic_block bb = NOTE_BASIC_BLOCK (in_rtx);
		  if (bb != 0)
		    fprintf (outfile, " [bb %d]", bb->index);
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
		  basic_block bb = NOTE_BASIC_BLOCK (in_rtx);
		  if (bb != 0)
		    fprintf (outfile, " [bb %d]", bb->index);
		  break;
		}

	      case NOTE_INSN_VAR_LOCATION:
	      case NOTE_INSN_CALL_ARG_LOCATION:
		fputc (' ', outfile);
		print_rtx (NOTE_VAR_LOCATION (in_rtx));
		break;

	      case NOTE_INSN_CFI:
		fputc ('\n', outfile);
		output_cfi_directive (outfile, NOTE_CFI (in_rtx));
		fputc ('\t', outfile);
		break;

	      default:
		break;
	      }
	  }
	else if (i == 7 && JUMP_P (in_rtx) && JUMP_LABEL (in_rtx) != NULL)
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
	    cselib_val *val = CSELIB_VAL_PTR (in_rtx);

	    fprintf (outfile, " %u:%u", val->uid, val->hash);
	    dump_addr (outfile, " @", in_rtx);
	    dump_addr (outfile, "/", (void*)val);
	  }
	else if (i == 0 && GET_CODE (in_rtx) == DEBUG_EXPR)
	  {
	    fprintf (outfile, " D#%i",
		     DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (in_rtx)));
	  }
	else if (i == 0 && GET_CODE (in_rtx) == ENTRY_VALUE)
	  {
	    indent += 2;
	    if (!sawclose)
	      fprintf (outfile, " ");
	    print_rtx (ENTRY_VALUE_EXP (in_rtx));
	    indent -= 2;
	  }
#endif
	break;

      case 'e':
      do_e:
	indent += 2;
	if (i == 6 && INSN_P (in_rtx))
	  /* Put REG_NOTES on their own line.  */
	  fprintf (outfile, "\n%s%*s",
		   print_rtx_head, indent * 2, "");
	if (!sawclose)
	  fprintf (outfile, " ");
	if (i == 7 && CALL_P (in_rtx))
	  {
	    in_call_function_usage = true;
	    print_rtx (XEXP (in_rtx, i));
	    in_call_function_usage = false;
	  }
	else
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
	if (i == 4 && INSN_P (in_rtx))
	  {
#ifndef GENERATOR_FILE
	    const rtx_insn *in_insn = as_a <const rtx_insn *> (in_rtx);

	    /*  Pretty-print insn locations.  Ignore scoping as it is mostly
		redundant with line number information and do not print anything
		when there is no location information available.  */
	    if (INSN_HAS_LOCATION (in_insn))
	      {
		expanded_location xloc = insn_location (in_insn);
		fprintf (outfile, " %s:%i", xloc.file, xloc.line);
	      }
#endif
	  }
	else if (i == 6 && GET_CODE (in_rtx) == ASM_OPERANDS)
	  {
#ifndef GENERATOR_FILE
	    if (ASM_OPERANDS_SOURCE_LOCATION (in_rtx) != UNKNOWN_LOCATION)
	      fprintf (outfile, " %s:%i",
		       LOCATION_FILE (ASM_OPERANDS_SOURCE_LOCATION (in_rtx)),
		       LOCATION_LINE (ASM_OPERANDS_SOURCE_LOCATION (in_rtx)));
#endif
	  }
	else if (i == 1 && GET_CODE (in_rtx) == ASM_INPUT)
	  {
#ifndef GENERATOR_FILE
	    if (ASM_INPUT_SOURCE_LOCATION (in_rtx) != UNKNOWN_LOCATION)
	      fprintf (outfile, " %s:%i",
		       LOCATION_FILE (ASM_INPUT_SOURCE_LOCATION (in_rtx)),
		       LOCATION_LINE (ASM_INPUT_SOURCE_LOCATION (in_rtx)));
#endif
	  }
	else if (i == 5 && NOTE_P (in_rtx))
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

	    if (flag_dump_unnumbered
		&& (is_insn || NOTE_P (in_rtx)))
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

      case 'r':
	{
	  unsigned int regno = REGNO (in_rtx);
#ifndef GENERATOR_FILE
	  if (regno < FIRST_PSEUDO_REGISTER)
	    fprintf (outfile, " %d %s", regno, reg_names[regno]);
	  else if (regno <= LAST_VIRTUAL_REGISTER)
	    {
	      if (regno == VIRTUAL_INCOMING_ARGS_REGNUM)
		fprintf (outfile, " %d virtual-incoming-args", regno);
	      else if (regno == VIRTUAL_STACK_VARS_REGNUM)
		fprintf (outfile, " %d virtual-stack-vars", regno);
	      else if (regno == VIRTUAL_STACK_DYNAMIC_REGNUM)
		fprintf (outfile, " %d virtual-stack-dynamic", regno);
	      else if (regno == VIRTUAL_OUTGOING_ARGS_REGNUM)
		fprintf (outfile, " %d virtual-outgoing-args", regno);
	      else if (regno == VIRTUAL_CFA_REGNUM)
		fprintf (outfile, " %d virtual-cfa", regno);
	      else if (regno == VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM)
		fprintf (outfile, " %d virtual-preferred-stack-boundary",
			 regno);
	      else
		fprintf (outfile, " %d virtual-reg-%d", regno,
			 regno-FIRST_VIRTUAL_REGISTER);
	    }
	  else
#endif
	    if (flag_dump_unnumbered && is_insn)
	      fputc ('#', outfile);
	    else
	      fprintf (outfile, " %d", regno);

#ifndef GENERATOR_FILE
	  if (REG_ATTRS (in_rtx))
	    {
	      fputs (" [", outfile);
	      if (regno != ORIGINAL_REGNO (in_rtx))
		fprintf (outfile, "orig:%i", ORIGINAL_REGNO (in_rtx));
	      if (REG_EXPR (in_rtx))
		print_mem_expr (outfile, REG_EXPR (in_rtx));

	      if (REG_OFFSET (in_rtx))
		fprintf (outfile, "+" HOST_WIDE_INT_PRINT_DEC,
			 REG_OFFSET (in_rtx));
	      fputs (" ]", outfile);
	    }
	  if (regno != ORIGINAL_REGNO (in_rtx))
	    fprintf (outfile, " [%d]", ORIGINAL_REGNO (in_rtx));
#endif
	  break;
	}

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
		|| (flag_dump_unnumbered_links && i <= 1
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
      else
	fputc (' ', outfile);

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

    case CONST_WIDE_INT:
      fprintf (outfile, " ");
      cwi_output_hex (outfile, in_rtx);
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

  fputc (')', outfile);
  sawclose = 1;
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

/* Dump rtx REF.  */

DEBUG_FUNCTION void
debug (const rtx_def &ref)
{
  debug_rtx (&ref);
}

DEBUG_FUNCTION void
debug (const rtx_def *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Count of rtx's to print with debug_rtx_list.
   This global exists because gdb user defined commands have no arguments.  */

DEBUG_VARIABLE int debug_rtx_count = 0;	/* 0 is treated as equivalent to 1 */

/* Call this function to print list from X on.

   N is a count of the rtx's to print. Positive values print from the specified
   rtx_insn on.  Negative values print a window around the rtx_insn.
   EG: -5 prints 2 rtx_insn's on either side (in addition to the specified
   rtx_insn).  */

DEBUG_FUNCTION void
debug_rtx_list (const rtx_insn *x, int n)
{
  int i,count;
  const rtx_insn *insn;

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

/* Call this function to print an rtx_insn list from START to END
   inclusive.  */

DEBUG_FUNCTION void
debug_rtx_range (const rtx_insn *start, const rtx_insn *end)
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

/* Call this function to search an rtx_insn list to find one with insn uid UID,
   and then call debug_rtx_list to print it, using DEBUG_RTX_COUNT.
   The found insn is returned to enable further debugging analysis.  */

DEBUG_FUNCTION const rtx_insn *
debug_rtx_find (const rtx_insn *x, int uid)
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
  const rtx_insn *tmp_rtx;

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
      case JUMP_TABLE_DATA:
      case BARRIER:
	for (tmp_rtx = as_a <const rtx_insn *> (rtx_first);
	     tmp_rtx != 0;
	     tmp_rtx = NEXT_INSN (tmp_rtx))
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
  return print_rtl_single_with_indent (outf, x, 0);
}

/* Like print_rtl_single, except specify a file and indentation.  */

int
print_rtl_single_with_indent (FILE *outf, const_rtx x, int ind)
{
  int old_indent = indent;
  char *s_indent = (char *) alloca ((size_t) ind + 1);
  memset ((void *) s_indent, ' ', (size_t) ind);
  s_indent[ind] = '\0';

  indent = ind;
  outfile = outf;
  sawclose = 0;
  fputs (s_indent, outfile);
  fputs (print_rtx_head, outfile);
  print_rtx (x);
  putc ('\n', outf);
  indent = old_indent;
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

/* Print the elements of VEC to FILE.  */

void
print_rtx_insn_vec (FILE *file, const vec<rtx_insn *> &vec)
{
  fputc('{', file);

  unsigned int len = vec.length ();
  for (unsigned int i = 0; i < len; i++)
    {
      print_rtl (file, vec[i]);
      if (i < len - 1)
	fputs (", ", file);
    }

  fputc ('}', file);
}

#ifndef GENERATOR_FILE
/* The functions below  try to print RTL in a form resembling assembler
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
static void print_insn_with_notes (pretty_printer *, const rtx_insn *);

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

    case CONST_WIDE_INT:
      {
	const char *sep = "<";
	int i;
	for (i = CONST_WIDE_INT_NUNITS (x) - 1; i >= 0; i--)
	  {
	    pp_string (pp, sep);
	    sep = ",";
	    sprintf (tmp, HOST_WIDE_INT_PRINT_HEX,
		     (unsigned HOST_WIDE_INT) CONST_WIDE_INT_ELT (x, i));
	    pp_string (pp, tmp);
	  }
        pp_greater (pp);
      }
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
      pp_printf (pp, "L%d", INSN_UID (LABEL_REF_LABEL (x)));
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
	const rtx_sequence *seq = as_a <const rtx_sequence *> (x);
	pp_string (pp, "sequence{");
	if (INSN_P (seq->element (0)))
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
	    for (int i = 0; i < seq->len (); i++)
	      print_insn_with_notes (pp, seq->insn (i));
	    pp_printf (pp, "%s      ", save_print_rtx_head);
	    print_rtx_head = save_print_rtx_head;
	  }
	else
	  {
	    for (int i = 0; i < seq->len (); i++)
	      {
		print_pattern (pp, seq->element (i), verbose);
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
      for (int i = 0; i < XVECLEN (x, 0); i++)
	{
	  print_value (pp, XVECEXP (x, 0, i), verbose);
	  pp_semicolon (pp);
	}
      break;
    case ADDR_DIFF_VEC:
      for (int i = 0; i < XVECLEN (x, 1); i++)
	{
	  print_value (pp, XVECEXP (x, 1, i), verbose);
	  pp_semicolon (pp);
	}
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
print_insn (pretty_printer *pp, const rtx_insn *x, int verbose)
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
print_insn_with_notes (pretty_printer *pp, const rtx_insn *x)
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
dump_insn_slim (FILE *f, const rtx_insn *x)
{
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.buffer->stream = f;
  print_insn_with_notes (&rtl_slim_pp, x);
  pp_flush (&rtl_slim_pp);
}

/* Same as above, but stop at LAST or when COUNT == 0.
   If COUNT < 0 it will stop only at LAST or NULL rtx.  */

void
dump_rtl_slim (FILE *f, const rtx_insn *first, const rtx_insn *last,
	       int count, int flags ATTRIBUTE_UNUSED)
{
  const rtx_insn *insn, *tail;
  pretty_printer rtl_slim_pp;
  rtl_slim_pp.buffer->stream = f;

  tail = last ? NEXT_INSN (last) : NULL;
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
  rtx_insn *insn;
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
extern void debug_insn_slim (const rtx_insn *);
DEBUG_FUNCTION void
debug_insn_slim (const rtx_insn *x)
{
  dump_insn_slim (stderr, x);
}

/* Same as above, but using dump_rtl_slim.  */
extern void debug_rtl_slim (FILE *, const rtx_insn *, const rtx_insn *,
			    int, int);
DEBUG_FUNCTION void
debug_rtl_slim (const rtx_insn *first, const rtx_insn *last, int count,
		int flags)
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
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, n);
  debug_bb_slim (bb);
}

#endif
