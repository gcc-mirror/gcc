/* Generate attribute information (insn-attr.h) from machine description.
   Copyright (C) 1989 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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
extern int atoi ();

char *xmalloc ();
static void fatal ();
void fancy_abort ();

static void
write_upcase (str)
    char *str;
{
  for (; *str; str++)
    if (*str >= 'a' && *str <= 'z')
      printf ("%c", *str - 'a' + 'A');
    else
      printf ("%c", *str);
}

static void
gen_attr (attr)
     rtx attr;
{
  char *p;

  printf ("#define HAVE_ATTR_%s\n", XSTR (attr, 0));

  /* If numeric attribute, don't need to write an enum.  */
  if (*XSTR (attr, 1) == '\0')
    printf ("extern int get_attr_%s ();\n", XSTR (attr, 0));
  else
    {
      printf ("enum attr_%s {", XSTR (attr, 0));
      write_upcase (XSTR (attr, 0));
      printf ("_");

      for (p = XSTR (attr, 1); *p != '\0'; p++)
	{
	  if (*p == ',')
	    {
	      printf (", ");
	      write_upcase (XSTR (attr, 0));
	      printf ("_");
	    }
	  else if (*p >= 'a' && *p <= 'z')
	    printf ("%c", *p - 'a' + 'A');
	  else
	    printf ("%c", *p);
	}

      printf ("};\n");
      printf ("extern enum attr_%s get_attr_%s ();\n\n",
	      XSTR (attr, 0), XSTR (attr, 0));
    }

  /* If `length' attribute, write additional function definitions and define
     variables used by `insn_current_length'.  */
  if (! strcmp (XSTR (attr, 0), "length"))
    {
      printf ("extern void init_lengths ();\n");
      printf ("extern void shorten_branches ();\n");
      printf ("extern int insn_default_length ();\n");
      printf ("extern int insn_variable_length_p ();\n");
      printf ("extern int insn_current_length ();\n\n");
      printf ("extern int *insn_addresses;\n");
      printf ("extern int insn_current_address;\n\n");
    }
}

static void
write_units ()
{
  printf ("#define INSN_SCHEDULING\n\n");
  printf ("extern int result_ready_cost ();\n");
  printf ("extern int function_units_used ();\n\n");
  printf ("extern struct function_unit_desc\n");
  printf ("{\n");
  printf ("  char *name;\n");
  printf ("  int bitmask;\n");
  printf ("  int multiplicity;\n");
  printf ("  int simultaneity;\n");
  printf ("  int default_cost;\n");
  printf ("  int (*ready_cost_function) ();\n");
  printf ("  int (*conflict_cost_function) ();\n");
  printf ("} function_units[];\n\n");
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
  char * result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

static void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genattr: ");
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
  extern rtx read_rtx ();
  register int c;
  int have_delay = 0;
  int have_annul_true = 0;
  int have_annul_false = 0;
  int have_units = 0;
  int i;

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

  printf ("/* Generated automatically by the program `genattr'\n\
from the machine description file `md'.  */\n\n");

  /* For compatibility, define the attribute `alternative', which is just
     a reference to the variable `which_alternative'.  */

  printf ("#define HAVE_ATTR_alternative\n");
  printf ("#define get_attr_alternative(insn) which_alternative\n");
     
  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_ATTR)
	gen_attr (desc);

      else if (GET_CODE (desc) == DEFINE_DELAY)
        {
	  if (! have_delay)
	    {
	      printf ("#define DELAY_SLOTS\n");
	      printf ("extern int num_delay_slots ();\n");
	      printf ("extern int eligible_for_delay ();\n\n");
	      have_delay = 1;
	    }

	  for (i = 0; i < XVECLEN (desc, 1); i += 3)
	    {
	      if (XVECEXP (desc, 1, i + 1) && ! have_annul_true)
		{
		  printf ("#define ANNUL_IFTRUE_SLOTS\n");
		  printf ("extern int eligible_for_annul_true ();\n");
		  have_annul_true = 1;
		}

	      if (XVECEXP (desc, 1, i + 2) && ! have_annul_false)
		{
		  printf ("#define ANNUL_IFFALSE_SLOTS\n");
		  printf ("extern int eligible_for_annul_false ();\n");
		  have_annul_false = 1;
		}
	    }
        }

      else if (GET_CODE (desc) == DEFINE_FUNCTION_UNIT && ! have_units)
	{
	  have_units = 1;
	  write_units ();
	}
    }

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}

