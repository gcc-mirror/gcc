/* Generate from machine description:
   - some #define configuration flags.
   Copyright (C) 1987, 1991, 1997, 1998, 1999 Free Software Foundation, Inc.

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


#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* flags to determine output of machine description dependent #define's.  */
static int max_recog_operands;  /* Largest operand number seen.  */
static int max_dup_operands;    /* Largest number of match_dup in any insn.  */
static int max_clobbers_per_insn;
static int have_cc0_flag;
static int have_cmove_flag;
static int have_cond_arith_flag;
static int have_lo_sum_flag;
static int have_peephole_flag;
static int have_peephole2_flag;

/* Maximum number of insns seen in a split.  */
static int max_insns_per_split = 1;

static int clobbers_seen_this_insn;
static int dup_operands_seen_this_insn;

static void walk_insn_part PARAMS ((rtx, int, int));
static void gen_insn PARAMS ((rtx));
static void gen_expand PARAMS ((rtx));
static void gen_split PARAMS ((rtx));
static void gen_peephole PARAMS ((rtx));

/* RECOG_P will be non-zero if this pattern was seen in a context where it will
   be used to recognize, rather than just generate an insn. 

   NON_PC_SET_SRC will be non-zero if this pattern was seen in a SET_SRC
   of a SET whose destination is not (pc).  */

static void
walk_insn_part (part, recog_p, non_pc_set_src)
     rtx part;
     int recog_p;
     int non_pc_set_src;
{
  register int i, j;
  register RTX_CODE code;
  register const char *format_ptr;

  if (part == 0)
    return;

  code = GET_CODE (part);
  switch (code)
    {
    case CLOBBER:
      clobbers_seen_this_insn++;
      break;

    case MATCH_OPERAND:
      if (XINT (part, 0) > max_recog_operands)
	max_recog_operands = XINT (part, 0);
      return;

    case MATCH_OP_DUP:
    case MATCH_PAR_DUP:
      ++dup_operands_seen_this_insn;
    case MATCH_SCRATCH:
    case MATCH_PARALLEL:
    case MATCH_OPERATOR:
      if (XINT (part, 0) > max_recog_operands)
	max_recog_operands = XINT (part, 0);
      /* Now scan the rtl's in the vector inside the MATCH_OPERATOR or
	 MATCH_PARALLEL.  */
      break;

    case LABEL_REF:
      if (GET_CODE (XEXP (part, 0)) == MATCH_OPERAND)
	break;
      return;

    case MATCH_DUP:
      ++dup_operands_seen_this_insn;
      if (XINT (part, 0) > max_recog_operands)
	max_recog_operands = XINT (part, 0);
      return;

    case CC0:
      if (recog_p)
	have_cc0_flag = 1;
      return;

    case LO_SUM:
      if (recog_p)
	have_lo_sum_flag = 1;
      return;

    case SET:
      walk_insn_part (SET_DEST (part), 0, recog_p);
      walk_insn_part (SET_SRC (part), recog_p,
		      GET_CODE (SET_DEST (part)) != PC);
      return;

    case IF_THEN_ELSE:
      /* Only consider this machine as having a conditional move if the
	 two arms of the IF_THEN_ELSE are both MATCH_OPERAND.  Otherwise,
	 we have some specific IF_THEN_ELSE construct (like the doz
	 instruction on the RS/6000) that can't be used in the general
	 context we want it for.  If the first operand is an arithmetic
	 operation and the second is a MATCH_OPERNAND, show we have
	 conditional arithmetic.  */

      if (recog_p && non_pc_set_src
	  && GET_CODE (XEXP (part, 1)) == MATCH_OPERAND
	  && GET_CODE (XEXP (part, 2)) == MATCH_OPERAND)
	have_cmove_flag = 1;
      else if (recog_p && non_pc_set_src
	       && (GET_RTX_CLASS (GET_CODE (XEXP (part, 1))) == '1'
		   || GET_RTX_CLASS (GET_CODE (XEXP (part, 1))) == '2'
		   || GET_RTX_CLASS (GET_CODE (XEXP (part, 1))) == 'c')
	       && GET_CODE (XEXP (XEXP (part, 1), 0)) == MATCH_OPERAND
	       && GET_CODE (XEXP (part, 2)) == MATCH_OPERAND)
	have_cond_arith_flag = 1;
      break;

    case REG: case CONST_INT: case SYMBOL_REF:
    case PC:
      return;

    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	walk_insn_part (XEXP (part, i), recog_p, non_pc_set_src);
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    walk_insn_part (XVECEXP (part, i, j), recog_p, non_pc_set_src);
	break;
      }
}

static void
gen_insn (insn)
     rtx insn;
{
  int i;

  /* Walk the insn pattern to gather the #define's status.  */
  clobbers_seen_this_insn = 0;
  dup_operands_seen_this_insn = 0;
  if (XVEC (insn, 1) != 0)
    for (i = 0; i < XVECLEN (insn, 1); i++)
      walk_insn_part (XVECEXP (insn, 1, i), 1, 0);

  if (clobbers_seen_this_insn > max_clobbers_per_insn)
    max_clobbers_per_insn = clobbers_seen_this_insn;
  if (dup_operands_seen_this_insn > max_dup_operands)
    max_dup_operands = dup_operands_seen_this_insn;
}

/* Similar but scan a define_expand.  */

static void
gen_expand (insn)
     rtx insn;
{
  int i;

  /* Walk the insn pattern to gather the #define's status.  */

  /* Note that we don't bother recording the number of MATCH_DUPs
     that occur in a gen_expand, because only reload cares about that.  */
  if (XVEC (insn, 1) != 0)
    for (i = 0; i < XVECLEN (insn, 1); i++)
      {
	/* Compute the maximum SETs and CLOBBERS
	   in any one of the sub-insns;
	   don't sum across all of them.  */
	clobbers_seen_this_insn = 0;

	walk_insn_part (XVECEXP (insn, 1, i), 0, 0);

	if (clobbers_seen_this_insn > max_clobbers_per_insn)
	  max_clobbers_per_insn = clobbers_seen_this_insn;
      }
}

/* Similar but scan a define_split.  */

static void
gen_split (split)
     rtx split;
{
  int i;

  /* Look through the patterns that are matched
     to compute the maximum operand number.  */
  for (i = 0; i < XVECLEN (split, 0); i++)
    walk_insn_part (XVECEXP (split, 0, i), 1, 0);
  /* Look at the number of insns this insn could split into.  */
  if (XVECLEN (split, 2) > max_insns_per_split)
    max_insns_per_split = XVECLEN (split, 2);
}

static void
gen_peephole (peep)
     rtx peep;
{
  int i;

  /* Look through the patterns that are matched
     to compute the maximum operand number.  */
  for (i = 0; i < XVECLEN (peep, 0); i++)
    walk_insn_part (XVECEXP (peep, 0, i), 1, 0);
}

PTR
xmalloc (size)
  size_t size;
{
  register PTR val = (PTR) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");

  return val;
}

PTR
xrealloc (old, size)
  PTR old;
  size_t size;
{
  register PTR ptr;
  if (old)
    ptr = (PTR) realloc (old, size);
  else
    ptr = (PTR) malloc (size);
  if (!ptr)
    fatal ("virtual memory exhausted");
  return ptr;
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  register int c;

  progname = "genconfig";
  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      return (FATAL_EXIT_CODE);
    }
  read_rtx_filename = argv[1];

  printf ("/* Generated automatically by the program `genconfig'\n\
from the machine description file `md'.  */\n\n");

  /* Allow at least 10 operands for the sake of asm constructs.  */
  max_recog_operands = 9;  /* We will add 1 later.  */
  max_dup_operands = 1;

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	gen_insn (desc);
      if (GET_CODE (desc) == DEFINE_EXPAND)
	gen_expand (desc);
      if (GET_CODE (desc) == DEFINE_SPLIT)
	gen_split (desc);
      if (GET_CODE (desc) == DEFINE_PEEPHOLE2)
	{
	  have_peephole2_flag = 1;
	  gen_split (desc);
	}
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  have_peephole_flag = 1;
	  gen_peephole (desc);
	}
    }

  printf ("\n#define MAX_RECOG_OPERANDS %d\n", max_recog_operands + 1);

  printf ("\n#define MAX_DUP_OPERANDS %d\n", max_dup_operands);

  /* This is conditionally defined, in case the user writes code which emits
     more splits than we can readily see (and knows s/he does it).  */
  printf ("#ifndef MAX_INSNS_PER_SPLIT\n#define MAX_INSNS_PER_SPLIT %d\n#endif\n",
	  max_insns_per_split);

  if (have_cc0_flag)
    printf ("#define HAVE_cc0\n");

  if (have_cmove_flag)
    printf ("#define HAVE_conditional_move\n");

#if 0
  /* Disabled.  See the discussion in jump.c.  */
  if (have_cond_arith_flag)
    printf ("#define HAVE_conditional_arithmetic\n");
#endif

  if (have_lo_sum_flag)
    printf ("#define HAVE_lo_sum\n");

  if (have_peephole_flag)
    printf ("#define HAVE_peephole\n");

  if (have_peephole2_flag)
    printf ("#define HAVE_peephole2\n");

  fflush (stdout);
  return (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
const char *
get_insn_name (code)
     int code ATTRIBUTE_UNUSED;
{
  return NULL;
}
