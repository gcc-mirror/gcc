/* Generate code from machine description to emit insns as rtl.
   Copyright (C) 1987, 1988, 1991, 1994 Free Software Foundation, Inc.

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


#include <stdio.h>
#include "hconfig.h"
#include "rtl.h"
#include "obstack.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();
extern rtx read_rtx ();

char *xmalloc ();
static void fatal ();
void fancy_abort ();

static int max_opno;
static int max_dup_opno;
static int register_constraints;
static int insn_code_number;
static int insn_index_number;

/* Data structure for recording the patterns of insns that have CLOBBERs.
   We use this to output a function that adds these CLOBBERs to a 
   previously-allocated PARALLEL expression.  */

struct clobber_pat
{
  struct clobber_ent *insns;
  rtx pattern;
  int first_clobber;
  struct clobber_pat *next;
} *clobber_list;

/* Records one insn that uses the clobber list.  */

struct clobber_ent
{
  int code_number;		/* Counts only insns.  */
  struct clobber_ent *next;
};

static void
max_operand_1 (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  if (code == MATCH_OPERAND && XSTR (x, 2) != 0 && *XSTR (x, 2) != '\0')
    register_constraints = 1;
  if (code == MATCH_SCRATCH && XSTR (x, 1) != 0 && *XSTR (x, 1) != '\0')
    register_constraints = 1;
  if (code == MATCH_OPERAND || code == MATCH_OPERATOR
      || code == MATCH_PARALLEL)
    max_opno = MAX (max_opno, XINT (x, 0));
  if (code == MATCH_DUP || code == MATCH_OP_DUP || code == MATCH_PAR_DUP)
    max_dup_opno = MAX (max_dup_opno, XINT (x, 0));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	max_operand_1 (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    max_operand_1 (XVECEXP (x, i, j));
	}
    }
}

static int
max_operand_vec (insn, arg)
     rtx insn;
     int arg;
{
  register int len = XVECLEN (insn, arg);
  register int i;

  max_opno = -1;
  max_dup_opno = -1;

  for (i = 0; i < len; i++)
    max_operand_1 (XVECEXP (insn, arg, i));

  return max_opno + 1;
}

static void
print_code (code)
     RTX_CODE code;
{
  register char *p1;
  for (p1 = GET_RTX_NAME (code); *p1; p1++)
    {
      if (*p1 >= 'a' && *p1 <= 'z')
	putchar (*p1 + 'A' - 'a');
      else
	putchar (*p1);
    }
}

/* Print a C expression to construct an RTX just like X,
   substituting any operand references appearing within.  */

static void
gen_exp (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;

  if (x == 0)
    {
      printf ("NULL_RTX");
      return;
    }

  code = GET_CODE (x);

  switch (code)
    {
    case MATCH_OPERAND:
    case MATCH_DUP:
      printf ("operand%d", XINT (x, 0));
      return;

    case MATCH_OP_DUP:
      printf ("gen_rtx (GET_CODE (operand%d), GET_MODE (operand%d)",
	      XINT (x, 0), XINT (x, 0));
      for (i = 0; i < XVECLEN (x, 1); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (x, 1, i));
	}
      printf (")");
      return;

    case MATCH_OPERATOR:
      printf ("gen_rtx (GET_CODE (operand%d)", XINT (x, 0));
      printf (", %smode", GET_MODE_NAME (GET_MODE (x)));
      for (i = 0; i < XVECLEN (x, 2); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (x, 2, i));
	}
      printf (")");
      return;

    case MATCH_PARALLEL:
    case MATCH_PAR_DUP:
      printf ("operand%d", XINT (x, 0));
      return;

    case MATCH_SCRATCH:
      printf ("gen_rtx (SCRATCH, %smode, 0)", GET_MODE_NAME (GET_MODE (x)));
      return;

    case ADDRESS:
      fatal ("ADDRESS expression code used in named instruction pattern");

    case PC:
      printf ("pc_rtx");
      return;

    case CC0:
      printf ("cc0_rtx");
      return;

    case CONST_INT:
      if (INTVAL (x) == 0)
	printf ("const0_rtx");
      else if (INTVAL (x) == 1)
	printf ("const1_rtx");
      else if (INTVAL (x) == -1)
	printf ("constm1_rtx");
      else if (INTVAL (x) == STORE_FLAG_VALUE)
	printf ("const_true_rtx");
      else
	printf (
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT	     
		"GEN_INT (%d)",
#else
		"GEN_INT (%ld)",
#endif
		INTVAL (x));
      return;

    case CONST_DOUBLE:
      /* These shouldn't be written in MD files.  Instead, the appropriate
	 routines in varasm.c should be called.  */
      abort ();
    }

  printf ("gen_rtx (");
  print_code (code);
  printf (", %smode", GET_MODE_NAME (GET_MODE (x)));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == '0')
	break;
      printf (",\n\t");
      if (fmt[i] == 'e' || fmt[i] == 'u')
	gen_exp (XEXP (x, i));
      else if (fmt[i] == 'i')
	printf ("%u", XINT (x, i));
      else if (fmt[i] == 's')
	printf ("\"%s\"", XSTR (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  printf ("gen_rtvec (%d", XVECLEN (x, i));
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      printf (",\n\t\t");
	      gen_exp (XVECEXP (x, i, j));
	    }
	  printf (")");
	}
      else
	abort ();
    }
  printf (")");
}  

/* Generate the `gen_...' function for a DEFINE_INSN.  */

static void
gen_insn (insn)
     rtx insn;
{
  int operands;
  register int i;

  /* See if the pattern for this insn ends with a group of CLOBBERs of (hard)
     registers or MATCH_SCRATCHes.  If so, store away the information for
     later. */

  if (XVEC (insn, 1))
    {
      for (i = XVECLEN (insn, 1) - 1; i > 0; i--)
	if (GET_CODE (XVECEXP (insn, 1, i)) != CLOBBER
	    || (GET_CODE (XEXP (XVECEXP (insn, 1, i), 0)) != REG
		&& GET_CODE (XEXP (XVECEXP (insn, 1, i), 0)) != MATCH_SCRATCH))
	  break;

      if (i != XVECLEN (insn, 1) - 1)
	{
	  register struct clobber_pat *p;
	  register struct clobber_ent *link
	    = (struct clobber_ent *) xmalloc (sizeof (struct clobber_ent));
	  register int j;

	  link->code_number = insn_code_number;

	  /* See if any previous CLOBBER_LIST entry is the same as this
	     one.  */

	  for (p = clobber_list; p; p = p->next)
	    {
	      if (p->first_clobber != i + 1
		  || XVECLEN (p->pattern, 1) != XVECLEN (insn, 1))
		continue;

	      for (j = i + 1; j < XVECLEN (insn, 1); j++)
		{
		  rtx old = XEXP (XVECEXP (p->pattern, 1, j), 0);
		  rtx new = XEXP (XVECEXP (insn, 1, j), 0);

		  /* OLD and NEW are the same if both are to be a SCRATCH
		     of the same mode, 
		     or if both are registers of the same mode and number.  */
		  if (! (GET_MODE (old) == GET_MODE (new)
			 && ((GET_CODE (old) == MATCH_SCRATCH
			      && GET_CODE (new) == MATCH_SCRATCH)
			     || (GET_CODE (old) == REG && GET_CODE (new) == REG
				 && REGNO (old) == REGNO (new)))))
		    break;
		}
      
	      if (j == XVECLEN (insn, 1))
		break;
	    }

	  if (p == 0)
	    {
	      p = (struct clobber_pat *) xmalloc (sizeof (struct clobber_pat));
	  
	      p->insns = 0;
	      p->pattern = insn;
	      p->first_clobber = i + 1;
	      p->next = clobber_list;
	      clobber_list = p;
	    }

	  link->next = p->insns;
	  p->insns = link;
	}
    }

  /* Don't mention instructions whose names are the null string.
     They are in the machine description just to be recognized.  */
  if (strlen (XSTR (insn, 0)) == 0)
    return;

  /* Find out how many operands this function has,
     and also whether any of them have register constraints.  */
  register_constraints = 0;
  operands = max_operand_vec (insn, 1);
  if (max_dup_opno >= operands)
    fatal ("match_dup operand number has no match_operand");

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (insn, 0));
  for (i = 0; i < operands; i++)
    printf (i ? ", operand%d" : "operand%d", i);
  printf (")\n");
  for (i = 0; i < operands; i++)
    printf ("     rtx operand%d;\n", i);
  printf ("{\n");

  /* Output code to construct and return the rtl for the instruction body */

  if (XVECLEN (insn, 1) == 1)
    {
      printf ("  return ");
      gen_exp (XVECEXP (insn, 1, 0));
      printf (";\n}\n\n");
    }
  else
    {
      printf ("  return gen_rtx (PARALLEL, VOIDmode, gen_rtvec (%d", XVECLEN (insn, 1));
      for (i = 0; i < XVECLEN (insn, 1); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (insn, 1, i));
	}
      printf ("));\n}\n\n");
    }
}

/* Generate the `gen_...' function for a DEFINE_EXPAND.  */

static void
gen_expand (expand)
     rtx expand;
{
  int operands;
  register int i;

  if (strlen (XSTR (expand, 0)) == 0)
    fatal ("define_expand lacks a name");
  if (XVEC (expand, 1) == 0)
    fatal ("define_expand for %s lacks a pattern", XSTR (expand, 0));

  /* Find out how many operands this function has,
     and also whether any of them have register constraints.  */
  register_constraints = 0;

  operands = max_operand_vec (expand, 1);

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (expand, 0));
  for (i = 0; i < operands; i++)
    printf (i ? ", operand%d" : "operand%d", i);
  printf (")\n");
  for (i = 0; i < operands; i++)
    printf ("     rtx operand%d;\n", i);
  printf ("{\n");

  /* If we don't have any C code to write, only one insn is being written,
     and no MATCH_DUPs are present, we can just return the desired insn
     like we do for a DEFINE_INSN.  This saves memory.  */
  if ((XSTR (expand, 3) == 0 || *XSTR (expand, 3) == '\0')
      && operands > max_dup_opno
      && XVECLEN (expand, 1) == 1)
    {
      printf ("  return ");
      gen_exp (XVECEXP (expand, 1, 0));
      printf (";\n}\n\n");
      return;
    }

  /* For each operand referred to only with MATCH_DUPs,
     make a local variable.  */
  for (i = operands; i <= max_dup_opno; i++)
    printf ("  rtx operand%d;\n", i);
  if (operands > 0 || max_dup_opno >= 0)
    printf ("  rtx operands[%d];\n", MAX (operands, max_dup_opno + 1));
  printf ("  rtx _val = 0;\n");
  printf ("  start_sequence ();\n");

  /* The fourth operand of DEFINE_EXPAND is some code to be executed
     before the actual construction.
     This code expects to refer to `operands'
     just as the output-code in a DEFINE_INSN does,
     but here `operands' is an automatic array.
     So copy the operand values there before executing it.  */
  if (XSTR (expand, 3) && *XSTR (expand, 3))
    {
      /* Output code to copy the arguments into `operands'.  */
      for (i = 0; i < operands; i++)
	printf ("  operands[%d] = operand%d;\n", i, i);

      /* Output the special code to be executed before the sequence
	 is generated.  */
      printf ("%s\n", XSTR (expand, 3));

      /* Output code to copy the arguments back out of `operands'
	 (unless we aren't going to use them at all).  */
      if (XVEC (expand, 1) != 0)
	{
	  for (i = 0; i < operands; i++)
	    printf ("  operand%d = operands[%d];\n", i, i);
	  for (; i <= max_dup_opno; i++)
	    printf ("  operand%d = operands[%d];\n", i, i);
	}
    }

  /* Output code to construct the rtl for the instruction bodies.
     Use emit_insn to add them to the sequence being accumulated.
     But don't do this if the user's code has set `no_more' nonzero.  */

  for (i = 0; i < XVECLEN (expand, 1); i++)
    {
      rtx next = XVECEXP (expand, 1, i);
      if ((GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC)
	  || (GET_CODE (next) == PARALLEL
	      && GET_CODE (XVECEXP (next, 0, 0)) == SET
	      && GET_CODE (SET_DEST (XVECEXP (next, 0, 0))) == PC)
	  || GET_CODE (next) == RETURN)
	printf ("  emit_jump_insn (");
      else if ((GET_CODE (next) == SET && GET_CODE (SET_SRC (next)) == CALL)
	       || GET_CODE (next) == CALL
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == SET
		   && GET_CODE (SET_SRC (XVECEXP (next, 0, 0))) == CALL)
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == CALL))
	printf ("  emit_call_insn (");
      else if (GET_CODE (next) == CODE_LABEL)
	printf ("  emit_label (");
      else if (GET_CODE (next) == MATCH_OPERAND
	       || GET_CODE (next) == MATCH_OPERATOR
	       || GET_CODE (next) == MATCH_PARALLEL
	       || GET_CODE (next) == MATCH_OP_DUP
	       || GET_CODE (next) == MATCH_DUP
	       || GET_CODE (next) == PARALLEL)
	printf ("  emit (");
      else
	printf ("  emit_insn (");
      gen_exp (next);
      printf (");\n");
      if (GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC
	  && GET_CODE (SET_SRC (next)) == LABEL_REF)
	printf ("  emit_barrier ();");
    }

  /* Call `gen_sequence' to make a SEQUENCE out of all the
     insns emitted within this gen_... function.  */

  printf (" _done:\n");
  printf ("  _val = gen_sequence ();\n");
  printf (" _fail:\n");
  printf ("  end_sequence ();\n");
  printf ("  return _val;\n}\n\n");
}

/* Like gen_expand, but generates a SEQUENCE.  */
static void
gen_split (split)
     rtx split;
{
  register int i;
  int operands;

  if (XVEC (split, 0) == 0)
    fatal ("define_split (definition %d) lacks a pattern", insn_index_number);
  else if (XVEC (split, 2) == 0)
    fatal ("define_split (definition %d) lacks a replacement pattern",
	   insn_index_number);

  /* Find out how many operands this function has.  */

  max_operand_vec (split, 2);
  operands = MAX (max_opno, max_dup_opno) + 1;

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_split_%d (operands)\n     rtx *operands;\n",
	  insn_code_number);
  printf ("{\n");

  /* Declare all local variables.  */
  for (i = 0; i < operands; i++)
    printf ("  rtx operand%d;\n", i);
  printf ("  rtx _val = 0;\n");
  printf ("  start_sequence ();\n");

  /* The fourth operand of DEFINE_SPLIT is some code to be executed
     before the actual construction.  */

  if (XSTR (split, 3))
    printf ("%s\n", XSTR (split, 3));

  /* Output code to copy the arguments back out of `operands'  */
  for (i = 0; i < operands; i++)
    printf ("  operand%d = operands[%d];\n", i, i);

  /* Output code to construct the rtl for the instruction bodies.
     Use emit_insn to add them to the sequence being accumulated.
     But don't do this if the user's code has set `no_more' nonzero.  */

  for (i = 0; i < XVECLEN (split, 2); i++)
    {
      rtx next = XVECEXP (split, 2, i);
      if ((GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC)
	  || (GET_CODE (next) == PARALLEL
	      && GET_CODE (XVECEXP (next, 0, 0)) == SET
	      && GET_CODE (SET_DEST (XVECEXP (next, 0, 0))) == PC)
	  || GET_CODE (next) == RETURN)
	printf ("  emit_jump_insn (");
      else if ((GET_CODE (next) == SET && GET_CODE (SET_SRC (next)) == CALL)
	       || GET_CODE (next) == CALL
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == SET
		   && GET_CODE (SET_SRC (XVECEXP (next, 0, 0))) == CALL)
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == CALL))
	printf ("  emit_call_insn (");
      else if (GET_CODE (next) == CODE_LABEL)
	printf ("  emit_label (");
      else if (GET_CODE (next) == MATCH_OPERAND
	       || GET_CODE (next) == MATCH_OPERATOR
	       || GET_CODE (next) == MATCH_PARALLEL
	       || GET_CODE (next) == MATCH_OP_DUP
	       || GET_CODE (next) == MATCH_DUP
	       || GET_CODE (next) == PARALLEL)
	printf ("  emit (");
      else
	printf ("  emit_insn (");
      gen_exp (next);
      printf (");\n");
      if (GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC
	  && GET_CODE (SET_SRC (next)) == LABEL_REF)
	printf ("  emit_barrier ();");
    }

  /* Call `gen_sequence' to make a SEQUENCE out of all the
     insns emitted within this gen_... function.  */

  printf (" _done:\n");
  printf ("  _val = gen_sequence ();\n");
  printf (" _fail:\n");
  printf ("  end_sequence ();\n");
  printf ("  return _val;\n}\n\n");
}

/* Write a function, `add_clobbers', that is given a PARALLEL of sufficient
   size for the insn and an INSN_CODE, and inserts the required CLOBBERs at
   the end of the vector.  */

static void
output_add_clobbers ()
{
  struct clobber_pat *clobber;
  struct clobber_ent *ent;
  int i;

  printf ("\n\nvoid\nadd_clobbers (pattern, insn_code_number)\n");
  printf ("     rtx pattern;\n     int insn_code_number;\n");
  printf ("{\n");
  printf ("  int i;\n\n");
  printf ("  switch (insn_code_number)\n");
  printf ("    {\n");

  for (clobber = clobber_list; clobber; clobber = clobber->next)
    {
      for (ent = clobber->insns; ent; ent = ent->next)
	printf ("    case %d:\n", ent->code_number);

      for (i = clobber->first_clobber; i < XVECLEN (clobber->pattern, 1); i++)
	{
	  printf ("      XVECEXP (pattern, 0, %d) = ", i);
	  gen_exp (XVECEXP (clobber->pattern, 1, i));
	  printf (";\n");
	}

      printf ("      break;\n\n");
    }

  printf ("    default:\n");
  printf ("      abort ();\n");
  printf ("    }\n");
  printf ("}\n");
}

/* Write a function, init_mov_optab, that is called to set up entries
   in mov_optab for EXTRA_CC_MODES.  */

static void
output_init_mov_optab ()
{
#ifdef EXTRA_CC_NAMES
  static char *cc_names[] = { EXTRA_CC_NAMES };
  char *p;
  int i;

  printf ("\nvoid\ninit_mov_optab ()\n{\n");

  for (i = 0; i < sizeof cc_names / sizeof cc_names[0]; i++)
    {
      printf ("#ifdef HAVE_mov");
      for (p = cc_names[i]; *p; p++)
	printf ("%c", *p >= 'A' && *p <= 'Z' ? *p - 'A' + 'a' : *p);
      printf ("\n");
      printf ("  if (HAVE_mov");
      for (p = cc_names[i]; *p; p++)
	printf ("%c", *p >= 'A' && *p <= 'Z' ? *p - 'A' + 'a' : *p);
      printf (")\n");
      printf ("    mov_optab->handlers[(int) %smode].insn_code = CODE_FOR_mov",
	      cc_names[i]);
      for (p = cc_names[i]; *p; p++)
	printf ("%c", *p >= 'A' && *p <= 'Z' ? *p - 'A' + 'a' : *p);
      printf (";\n#endif\n");
    }

  printf ("}\n");
#endif
}

char *
xmalloc (size)
     unsigned size;
{
  register char *val = (char *) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");

  return val;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  char *result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

static void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genemit: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  register int c;

  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  init_rtl ();

  /* Assign sequential codes to all entries in the machine description
     in parallel with the tables in insn-output.c.  */

  insn_code_number = 0;
  insn_index_number = 0;

  printf ("/* Generated automatically by the program `genemit'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"expr.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"insn-config.h\"\n\n");
  printf ("#include \"insn-flags.h\"\n\n");
  printf ("#include \"insn-codes.h\"\n\n");
  printf ("extern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];\n\n");
  printf ("extern rtx recog_operand[];\n");
  printf ("#define operands emit_operand\n\n");
  printf ("#define FAIL goto _fail\n\n");
  printf ("#define DONE goto _done\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	{
	  gen_insn (desc);
	  ++insn_code_number;
	}
      if (GET_CODE (desc) == DEFINE_EXPAND)
	{
	  gen_expand (desc);
	  ++insn_code_number;
	}
      if (GET_CODE (desc) == DEFINE_SPLIT)
	{
	  gen_split (desc);
	  ++insn_code_number;
	}
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  ++insn_code_number;
	}
      ++insn_index_number;
    }

  /* Write out the routine to add CLOBBERs to a pattern.  */
  output_add_clobbers ();

  /* Write the routine to initialize mov_optab for the EXTRA_CC_MODES.  */
  output_init_mov_optab ();

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
