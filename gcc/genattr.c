/* Generate attribute information (insn-attr.h) from machine description.
   Copyright (C) 1991 Free Software Foundation, Inc.
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
#include "hconfig.h"
#include "rtl.h"
#include "obstack.h"

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();
extern int atoi ();
extern rtx read_rtx ();

char *xmalloc ();
static void fatal ();
void fancy_abort ();

/* A range of values.  */

struct range
{
  int min;
  int max;
};

/* Record information about each function unit mentioned in a
   DEFINE_FUNCTION_UNIT.  */

struct function_unit
{
  char *name;			/* Function unit name.  */
  struct function_unit *next;	/* Next function unit.  */
  int multiplicity;		/* Number of units of this type.  */
  int simultaneity;		/* Maximum number of simultaneous insns
				   on this function unit or 0 if unlimited.  */
  struct range ready_cost;	/* Range of ready cost values.  */
  struct range issue_delay;	/* Range of issue delay values.  */
};

static void
extend_range (range, min, max)
     struct range *range;
     int min;
     int max;
{
  if (range->min > min) range->min = min;
  if (range->max < max) range->max = max;
}

static void
init_range (range)
     struct range *range;
{
  range->min = 100000;
  range->max = -1;
}

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
write_units (num_units, multiplicity, simultaneity,
	     ready_cost, issue_delay, blockage)
     int num_units;
     struct range *multiplicity;
     struct range *simultaneity;
     struct range *ready_cost;
     struct range *issue_delay;
     struct range *blockage;
{
  int i, q_size;

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
  printf ("  int max_issue_delay;\n");
  printf ("  int (*ready_cost_function) ();\n");
  printf ("  int (*conflict_cost_function) ();\n");
  printf ("  int max_blockage;\n");
  printf ("  unsigned int (*blockage_range_function) ();\n");
  printf ("  int (*blockage_function) ();\n");
  printf ("} function_units[];\n\n");
  printf ("#define FUNCTION_UNITS_SIZE %d\n", num_units);
  printf ("#define MIN_MULTIPLICITY %d\n", multiplicity->min);
  printf ("#define MAX_MULTIPLICITY %d\n", multiplicity->max);
  printf ("#define MIN_SIMULTANEITY %d\n", simultaneity->min);
  printf ("#define MAX_SIMULTANEITY %d\n", simultaneity->max);
  printf ("#define MIN_READY_COST %d\n", ready_cost->min);
  printf ("#define MAX_READY_COST %d\n", ready_cost->max);
  printf ("#define MIN_ISSUE_DELAY %d\n", issue_delay->min);
  printf ("#define MAX_ISSUE_DELAY %d\n", issue_delay->max);
  printf ("#define MIN_BLOCKAGE %d\n", blockage->min);
  printf ("#define MAX_BLOCKAGE %d\n", blockage->max);
  for (i = 0; (1 << i) < blockage->max; i++)
    ;
  printf ("#define BLOCKAGE_BITS %d\n", i + 1);

  /* INSN_QUEUE_SIZE is a power of two larger than MAX_BLOCKAGE and
     MAX_READY_COST.  This is the longest time an isnsn may be queued.  */
  i = MAX (blockage->max, ready_cost->max);
  for (q_size = 1; q_size <= i; q_size <<= 1)
    ;
  printf ("#define INSN_QUEUE_SIZE %d\n", q_size);
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
  register int c;
  int have_delay = 0;
  int have_annul_true = 0;
  int have_annul_false = 0;
  int num_units = 0;
  struct range all_simultaneity, all_multiplicity;
  struct range all_ready_cost, all_issue_delay, all_blockage;
  struct function_unit *units = 0, *unit;
  int i;

  init_range (&all_multiplicity);
  init_range (&all_simultaneity);
  init_range (&all_ready_cost);
  init_range (&all_issue_delay);
  init_range (&all_blockage);

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
	      printf ("extern int const_num_delay_slots ();\n\n");
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

      else if (GET_CODE (desc) == DEFINE_FUNCTION_UNIT)
	{
	  char *name = XSTR (desc, 0);
	  int multiplicity = XINT (desc, 1);
	  int simultaneity = XINT (desc, 2);
	  int ready_cost = MAX (XINT (desc, 4), 1);
	  int issue_delay = MAX (XINT (desc, 5), 1);
	  int issueexp_p = (XVEC (desc, 6) != 0);

	  for (unit = units; unit; unit = unit->next)
	    if (strcmp (unit->name, name) == 0)
	      break;

	  if (unit == 0)
	    {
	      int len = strlen (name) + 1;
	      unit = (struct function_unit *)
		alloca (sizeof (struct function_unit));
	      unit->name = (char *) alloca (len);
	      bcopy (name, unit->name, len);
	      unit->multiplicity = multiplicity;
	      unit->simultaneity = simultaneity;
	      unit->ready_cost.min = unit->ready_cost.max = ready_cost;
	      unit->issue_delay.min = unit->issue_delay.max = issue_delay;
	      unit->next = units;
	      units = unit;
	      num_units++;

	      extend_range (&all_multiplicity, multiplicity, multiplicity);
	      extend_range (&all_simultaneity, simultaneity, simultaneity);
	    }
	  else if (unit->multiplicity != multiplicity
		   || unit->simultaneity != simultaneity)
	    fatal ("Differing specifications given for `%s' function unit.",
		   unit->name);

	  extend_range (&unit->ready_cost, ready_cost, ready_cost);
	  extend_range (&unit->issue_delay,
			issueexp_p ? 1 : issue_delay, issue_delay);
	  extend_range (&all_ready_cost,
			unit->ready_cost.min, unit->ready_cost.max);
	  extend_range (&all_issue_delay,
			unit->issue_delay.min, unit->issue_delay.max);
	}
    }

  if (num_units > 0)
    {
      /* Compute the range of blockage cost values.  See genattrtab.c
	 for the derivation.  BLOCKAGE (E,C) when SIMULTANEITY is zero is

	     MAX (ISSUE-DELAY (E,C),
		  READY-COST (E) - (READY-COST (C) - 1))

	 and otherwise

	     MAX (ISSUE-DELAY (E,C),
		  READY-COST (E) - (READY-COST (C) - 1),
		  READY-COST (E) - FILL-TIME)  */

      for (unit = units; unit; unit = unit->next)
	{
	  struct range blockage;
	  int max_issue_time = MAX (unit->issue_delay.max, 1);

	  blockage = unit->issue_delay;
	  blockage.max = MAX (unit->ready_cost.max
			      - (unit->ready_cost.min - 1),
			      blockage.max);
	  blockage.min = MAX (1, blockage.min);

	  if (unit->simultaneity != 0)
	    {
	      int fill_time = ((unit->simultaneity - 1)
			       * unit->issue_delay.min);
	      blockage.min = MAX (unit->ready_cost.min - fill_time,
				  blockage.min);
	      blockage.max = MAX (unit->ready_cost.max - fill_time,
				  blockage.max);
	    }
	  extend_range (&all_blockage, blockage.min, blockage.max);
	}

      write_units (num_units, &all_multiplicity, &all_simultaneity,
		   &all_ready_cost, &all_issue_delay, &all_blockage);
    }

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
