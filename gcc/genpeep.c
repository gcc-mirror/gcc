/* Generate code from machine description to perform peephole optimizations.
   Copyright (C) 1987, 89, 92, 97, 98, 1999 Free Software Foundation, Inc.

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

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
char **insn_name_ptr = 0;

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

void fatal PVPROTO ((const char *, ...))
  ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
void fancy_abort PROTO((void)) ATTRIBUTE_NORETURN;

static int max_opno;

/* Number of operands used in current peephole definition.  */

static int n_operands;

/* Peephole optimizations get insn codes just like insn patterns.
   Count them so we know the code of the define_peephole we are handling.  */

static int insn_code_number = 0;

static void gen_peephole PROTO((rtx));
static void match_rtx PROTO((rtx, struct link *, int));
static void print_path PROTO((struct link *));
static void print_code PROTO((RTX_CODE));

static void
gen_peephole (peep)
     rtx peep;
{
  int ninsns = XVECLEN (peep, 0);
  int i;

  n_operands = 0;

  printf ("  insn = ins1;\n");
#if 0
  printf ("  want_jump = 0;\n");
#endif

  for (i = 0; i < ninsns; i++)
    {
      if (i > 0)
	{
	  printf ("  do { insn = NEXT_INSN (insn);\n");
	  printf ("       if (insn == 0) goto L%d; }\n",
		  insn_code_number);
	  printf ("  while (GET_CODE (insn) == NOTE\n");
	  printf ("\t || (GET_CODE (insn) == INSN\n");
	  printf ("\t     && (GET_CODE (PATTERN (insn)) == USE\n");
	  printf ("\t\t || GET_CODE (PATTERN (insn)) == CLOBBER)));\n");

	  printf ("  if (GET_CODE (insn) == CODE_LABEL\n\
      || GET_CODE (insn) == BARRIER)\n    goto L%d;\n",
  		  insn_code_number);
	}

#if 0
      printf ("  if (GET_CODE (insn) == JUMP_INSN)\n");
      printf ("    want_jump = JUMP_LABEL (insn);\n");
#endif

      printf ("  pat = PATTERN (insn);\n");

      /* Walk the insn's pattern, remembering at all times the path
	 down to the walking point.  */

      match_rtx (XVECEXP (peep, 0, i), NULL_PTR, insn_code_number);
    }

  /* We get this far if the pattern matches.
     Now test the extra condition.  */

  if (XSTR (peep, 1) && XSTR (peep, 1)[0])
    printf ("  if (! (%s)) goto L%d;\n",
	    XSTR (peep, 1), insn_code_number);

  /* If that matches, construct new pattern and put it in the first insn.
     This new pattern will never be matched.
     It exists only so that insn-extract can get the operands back.
     So use a simple regular form: a PARALLEL containing a vector
     of all the operands.  */

  printf ("  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (%d, operands));\n", n_operands);

#if 0
  printf ("  if (want_jump && GET_CODE (ins1) != JUMP_INSN)\n");
  printf ("    {\n");
  printf ("      rtx insn2 = emit_jump_insn_before (PATTERN (ins1), ins1);\n");
  printf ("      delete_insn (ins1);\n");
  printf ("      ins1 = ins2;\n");
  printf ("    }\n");
#endif

  /* Record this define_peephole's insn code in the insn,
     as if it had been recognized to match this.  */
  printf ("  INSN_CODE (ins1) = %d;\n",
	  insn_code_number);

  /* Delete the remaining insns.  */
  if (ninsns > 1)
    printf ("  delete_for_peephole (NEXT_INSN (ins1), insn);\n");

  /* See reload1.c for insertion of NOTE which guarantees that this
     cannot be zero.  */
  printf ("  return NEXT_INSN (insn);\n");

  printf (" L%d:\n\n", insn_code_number);
}

static void
match_rtx (x, path, fail_label)
     rtx x;
     struct link *path;
     int fail_label;
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
    case MATCH_OPERAND:
      if (XINT (x, 0) > max_opno)
	max_opno = XINT (x, 0);
      if (XINT (x, 0) >= n_operands)
	n_operands = 1 + XINT (x, 0);

      printf ("  x = ");
      print_path (path);
      printf (";\n");

      printf ("  operands[%d] = x;\n", XINT (x, 0));
      if (XSTR (x, 1) && XSTR (x, 1)[0])
	printf ("  if (! %s (x, %smode)) goto L%d;\n",
		XSTR (x, 1), GET_MODE_NAME (GET_MODE (x)), fail_label);
      return;

    case MATCH_DUP:
    case MATCH_PAR_DUP:
      printf ("  x = ");
      print_path (path);
      printf (";\n");

      printf ("  if (!rtx_equal_p (operands[%d], x)) goto L%d;\n",
	      XINT (x, 0), fail_label);
      return;

    case MATCH_OP_DUP:
      printf ("  x = ");
      print_path (path);
      printf (";\n");

      printf ("  if (GET_CODE (operands[%d]) != GET_CODE (x)\n", XINT (x, 0));
      printf ("      || GET_MODE (operands[%d]) != GET_MODE (x)) goto L%d;\n",
	      XINT (x, 0), fail_label);
      printf ("  operands[%d] = x;\n", XINT (x, 0));
      link.next = path;
      link.vecelt = -1;
      for (i = 0; i < XVECLEN (x, 1); i++)
	{
	  link.pos = i;
	  match_rtx (XVECEXP (x, 1, i), &link, fail_label);
	}
      return;

    case MATCH_OPERATOR:
      if (XINT (x, 0) > max_opno)
	max_opno = XINT (x, 0);
      if (XINT (x, 0) >= n_operands)
	n_operands = 1 + XINT (x, 0);

      printf ("  x = ");
      print_path (path);
      printf (";\n");

      printf ("  operands[%d] = x;\n", XINT (x, 0));
      if (XSTR (x, 1) && XSTR (x, 1)[0])
	printf ("  if (! %s (x, %smode)) goto L%d;\n",
		XSTR (x, 1), GET_MODE_NAME (GET_MODE (x)), fail_label);
      link.next = path;
      link.vecelt = -1;
      for (i = 0; i < XVECLEN (x, 2); i++)
	{
	  link.pos = i;
	  match_rtx (XVECEXP (x, 2, i), &link, fail_label);
	}
      return;

    case MATCH_PARALLEL:
      if (XINT (x, 0) > max_opno)
	max_opno = XINT (x, 0);
      if (XINT (x, 0) >= n_operands)
	n_operands = 1 + XINT (x, 0);

      printf ("  x = ");
      print_path (path);
      printf (";\n");

      printf ("  if (GET_CODE (x) != PARALLEL) goto L%d;\n", fail_label);
      printf ("  operands[%d] = x;\n", XINT (x, 0));
      if (XSTR (x, 1) && XSTR (x, 1)[0])
	printf ("  if (! %s (x, %smode)) goto L%d;\n",
		XSTR (x, 1), GET_MODE_NAME (GET_MODE (x)), fail_label);
      link.next = path;
      link.pos = 0;
      for (i = 0; i < XVECLEN (x, 2); i++)
	{
	  link.vecelt = i;
	  match_rtx (XVECEXP (x, 2, i), &link, fail_label);
	}
      return;

    case ADDRESS:
      match_rtx (XEXP (x, 0), path, fail_label);
      return;
      
    default:
      break;
    }

  printf ("  x = ");
  print_path (path);
  printf (";\n");

  printf ("  if (GET_CODE (x) != ");
  print_code (code);
  printf (") goto L%d;\n", fail_label);

  if (GET_MODE (x) != VOIDmode)
    {
      printf ("  if (GET_MODE (x) != %smode) goto L%d;\n",
	      GET_MODE_NAME (GET_MODE (x)), fail_label);
    }

  link.next = path;
  link.vecelt = -1;
  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      link.pos = i;
      if (fmt[i] == 'e' || fmt[i] == 'u')
	match_rtx (XEXP (x, i), &link, fail_label);
      else if (fmt[i] == 'E')
	{
	  int j;
	  printf ("  if (XVECLEN (x, %d) != %d) goto L%d;\n",
		  i, XVECLEN (x, i), fail_label);
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      link.vecelt = j;
	      match_rtx (XVECEXP (x, i, j), &link, fail_label);
	    }
	}
      else if (fmt[i] == 'i')
	{
	  /* Make sure that at run time `x' is the RTX we want to test.  */
	  if (i != 0)
	    {
	      printf ("  x = ");
	      print_path (path);
	      printf (";\n");
	    }

	  printf ("  if (XINT (x, %d) != %d) goto L%d;\n",
		  i, XINT (x, i), fail_label);
	}
      else if (fmt[i] == 'w')
	{
	  /* Make sure that at run time `x' is the RTX we want to test.  */
	  if (i != 0)
	    {
	      printf ("  x = ");
	      print_path (path);
	      printf (";\n");
	    }

	  printf ("  if (XWINT (x, %d) != ", i);
	  printf (HOST_WIDE_INT_PRINT_DEC, XWINT (x, i));
	  printf (") goto L%d;\n", fail_label);
	}
      else if (fmt[i] == 's')
	{
	  /* Make sure that at run time `x' is the RTX we want to test.  */
	  if (i != 0)
	    {
	      printf ("  x = ");
	      print_path (path);
	      printf (";\n");
	    }

	  printf ("  if (strcmp (XSTR (x, %d), \"%s\")) goto L%d;\n",
		  i, XSTR (x, i), fail_label);
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
    printf ("pat");
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

void
fatal VPROTO ((const char *format, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *format;
#endif
  va_list ap;

  VA_START (ap, format);

#ifndef ANSI_PROTOTYPES
  format = va_arg (ap, const char *);
#endif

  fprintf (stderr, "genpeep: ");
  vfprintf (stderr, format, ap);
  va_end (ap);
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

  max_opno = -1;

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

  printf ("/* Generated automatically by the program `genpeep'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"except.h\"\n\n");

  printf ("extern rtx peep_operand[];\n\n");
  printf ("#define operands peep_operand\n\n");

  printf ("rtx\npeephole (ins1)\n     rtx ins1;\n{\n");
  printf ("  rtx insn ATTRIBUTE_UNUSED, x ATTRIBUTE_UNUSED, pat ATTRIBUTE_UNUSED;\n\n");

  /* Early out: no peepholes for insns followed by barriers.  */
  printf ("  if (NEXT_INSN (ins1)\n");
  printf ("      && GET_CODE (NEXT_INSN (ins1)) == BARRIER)\n");
  printf ("    return 0;\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  gen_peephole (desc);
	  insn_code_number++;
	}
      if (GET_CODE (desc) == DEFINE_INSN
	  || GET_CODE (desc) == DEFINE_EXPAND
	  || GET_CODE (desc) == DEFINE_SPLIT)
	{
	  insn_code_number++;
	}
    }

  printf ("  return 0;\n}\n\n");

  if (max_opno == -1)
    max_opno = 1;

  printf ("rtx peep_operand[%d];\n", max_opno + 1);

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
