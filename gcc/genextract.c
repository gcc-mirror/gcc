/* Generate code from machine description to extract operands from insn as rtl.
   Copyright (C) 1987, 1991 Free Software Foundation, Inc.

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
#include "config.h"
#include "rtl.h"
#include "obstack.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();

/* Number instruction patterns handled, starting at 0 for first one.  */

static int insn_code_number;

/* Number the occurrences of MATCH_DUP in each instruction,
   starting at 0 for the first occurrence.  */

static int dup_count;

/* Record which operand numbers have been seen in the current pattern.
   This table is made longer as needed.  */

static char *operand_seen;

/* Current allocated length of operand_seen.  */

static int operand_seen_length;

/* Have we got any peephole patterns yet?  */

static int peephole_seen;

/* While tree-walking an instruction pattern, we keep a chain
   of these `struct link's to record how to get down to the
   current position.  In each one, POS is the operand number,
   and if the operand is a vector VEC is the element number.
   VEC is -1 if the operand is not a vector.  */

struct link
{
  struct link *next;
  int pos;
  int vecelt;
};

static void walk_rtx ();
static void print_path ();
char *xmalloc ();
char *xrealloc ();
static void fatal ();
static void mybzero ();
void fancy_abort ();

static void
gen_insn (insn)
     rtx insn;
{
  register int i;

  dup_count = 0;

  /* No operands seen so far in this pattern.  */
  mybzero (operand_seen, operand_seen_length);

  printf ("    case %d:\n", insn_code_number);

  /* Walk the insn's pattern, remembering at all times the path
     down to the walking point.  */

  if (XVECLEN (insn, 1) == 1)
    walk_rtx (XVECEXP (insn, 1, 0), 0);
  else
    for (i = XVECLEN (insn, 1) - 1; i >= 0; i--)
      {
	struct link link;
	link.next = 0;
	link.pos = 0;
	link.vecelt = i;
	walk_rtx (XVECEXP (insn, 1, i), &link);
      }

  /* If the operand numbers used in the pattern are not consecutive,
     don't leave an operand uninitialized.  */
  for (i = operand_seen_length - 1; i >= 0; i--)
    if (operand_seen[i])
      break;
  for (; i >= 0; i--)
    if (!operand_seen[i])
      {
	printf ("      recog_operand[%d] = const0_rtx;\n", i);
	printf ("      recog_operand_loc[%d] = &junk;\n", i);
      }
  printf ("      break;\n");
}

/* Record that we have seen an operand with number OPNO in this pattern.  */

static void
mark_operand_seen (opno)
     int opno;
{
  if (opno >= operand_seen_length)
    {
      operand_seen_length *= 2;
      operand_seen = (char *) xrealloc (operand_seen, operand_seen_length);
    }

  operand_seen[opno] = 1;
}

static void
walk_rtx (x, path)
     rtx x;
     struct link *path;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;
  struct link link;

  if (x == 0)
    return;

  code = GET_CODE (x);

  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case SYMBOL_REF:
      return;

    case MATCH_OPERAND:
    case MATCH_SCRATCH:
      mark_operand_seen (XINT (x, 0));
      printf ("      recog_operand[%d] = *(recog_operand_loc[%d]\n        = &",
	      XINT (x, 0), XINT (x, 0));
      print_path (path);
      printf (");\n");
      break;

    case MATCH_DUP:
    case MATCH_OP_DUP:
      printf ("      recog_dup_loc[%d] = &", dup_count);
      print_path (path);
      printf (";\n");
      printf ("      recog_dup_num[%d] = %d;\n", dup_count, XINT (x, 0));
      dup_count++;
      break;

    case MATCH_OPERATOR:
      mark_operand_seen (XINT (x, 0));
      printf ("      recog_operand[%d] = *(recog_operand_loc[%d]\n        = &",
	      XINT (x, 0), XINT (x, 0));
      print_path (path);
      printf (");\n");
      link.next = path;
      link.vecelt = -1;
      for (i = XVECLEN (x, 2) - 1; i >= 0; i--)
	{
	  link.pos = i;
	  walk_rtx (XVECEXP (x, 2, i), &link);
	}
      return;

    case MATCH_PARALLEL:
      mark_operand_seen (XINT (x, 0));
      printf ("      recog_operand[%d] = *(recog_operand_loc[%d]\n        = &",
	      XINT (x, 0), XINT (x, 0));
      print_path (path);
      printf (");\n");
      link.next = path;
      link.pos = 0;
      for (i = XVECLEN (x, 2) - 1; i >= 0; i--)
	{
	  link.vecelt = i;
	  walk_rtx (XVECEXP (x, 2, i), &link);
	}
      return;

    case ADDRESS:
      walk_rtx (XEXP (x, 0), path);
      return;
    }

  link.next = path;
  link.vecelt = -1;
  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      link.pos = i;
      if (fmt[i] == 'e' || fmt[i] == 'u')
	{
	  walk_rtx (XEXP (x, i), &link);
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      link.vecelt = j;
	      walk_rtx (XVECEXP (x, i, j), &link);
	    }
	}
    }
}

/* Given a PATH, representing a path down the instruction's
   pattern from the root to a certain point, output code to
   evaluate to the rtx at that point.  */

static void
print_path (path)
     struct link *path;
{
  if (path == 0)
    printf ("insn");
  else if (path->vecelt >= 0)
    {
      printf ("XVECEXP (");
      print_path (path->next);
      printf (", %d, %d)", path->pos, path->vecelt);
    }
  else
    {
      printf ("XEXP (");
      print_path (path->next);
      printf (", %d)", path->pos);
    }
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
  fprintf (stderr, "genextract: ");
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

static void
mybzero (b, length)
     register char *b;
     register unsigned length;
{
  while (length-- > 0)
    *b++ = 0;
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  extern rtx read_rtx ();
  register int c, i;

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

  operand_seen_length = 40;
  operand_seen = (char *) xmalloc (40);

  printf ("/* Generated automatically by the program `genextract'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n\n");

  /* This variable exists only so it can be the "location"
     of any missing operand whose numbers are skipped by a given pattern.  */
  printf ("static rtx junk;\n");
  printf ("extern rtx recog_operand[];\n");
  printf ("extern rtx *recog_operand_loc[];\n");
  printf ("extern rtx *recog_dup_loc[];\n");
  printf ("extern char recog_dup_num[];\n");
  printf ("extern void fatal_insn_not_found ();\n\n");

  printf ("void\ninsn_extract (insn)\n");
  printf ("     rtx insn;\n");
  printf ("{\n");
  printf ("  int insn_code = INSN_CODE (insn);\n");
  printf ("  if (insn_code == -1) fatal_insn_not_found (insn);\n");
  printf ("  insn = PATTERN (insn);\n");
  printf ("  switch (insn_code)\n");
  printf ("    {\n");

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
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  printf ("    case %d: goto peephole;\n", insn_code_number);
	  ++insn_code_number;
	  ++peephole_seen;
	}
      if (GET_CODE (desc) == DEFINE_EXPAND || GET_CODE (desc) == DEFINE_SPLIT)
	{
	  printf ("    case %d: break;\n", insn_code_number);
	  ++insn_code_number;
	}
    }

  /* This should never be reached.  */
  printf ("\n    default:\n      abort ();\n");

  if (peephole_seen)
    {
      /* The vector in the insn says how many operands it has.
	 And all it contains are operands.  In fact, the vector was
	 created just for the sake of this function.  */
      printf ("    peephole:\n");
      printf ("#if __GNUC__ > 1 && !defined (bcopy)\n");
      printf ("#define bcopy(FROM,TO,COUNT) __builtin_memcpy(TO,FROM,COUNT)\n");
      printf ("#endif\n");
      printf ("      bcopy (&XVECEXP (insn, 0, 0), recog_operand,\n");
      printf ("             sizeof (rtx) * XVECLEN (insn, 0));\n");
      printf ("      break;\n");
    }

  printf ("    }\n}\n");

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
