/* Generate attribute information (insn-attr.h) from machine description.
   Copyright (C) 1991, 1994, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
#include "errors.h"
#include "gensupport.h"


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

static void extend_range PARAMS ((struct range *, int, int));
static void init_range PARAMS ((struct range *));
static void write_upcase PARAMS ((const char *));
static void gen_attr PARAMS ((rtx));
static void write_units PARAMS ((int, struct range *, struct range *,
			       struct range *, struct range *,
			       struct range *));
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
    const char *str;
{
  for (; *str; str++)
    putchar (TOUPPER(*str));
}

static void
gen_attr (attr)
     rtx attr;
{
  const char *p;
  int is_const = GET_CODE (XEXP (attr, 2)) == CONST;  

  printf ("#define HAVE_ATTR_%s\n", XSTR (attr, 0));

  /* If numeric attribute, don't need to write an enum.  */
  if (*XSTR (attr, 1) == '\0')
    printf ("extern int get_attr_%s PARAMS ((%s));\n", XSTR (attr, 0),
	    (is_const ? "void" : "rtx"));
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
	  else
	    putchar (TOUPPER(*p));
	}

      printf ("};\n");
      printf ("extern enum attr_%s get_attr_%s PARAMS ((%s));\n\n",
	      XSTR (attr, 0), XSTR (attr, 0), (is_const ? "void" : "rtx"));
    }

  /* If `length' attribute, write additional function definitions and define
     variables used by `insn_current_length'.  */
  if (! strcmp (XSTR (attr, 0), "length"))
    {
      printf ("extern void shorten_branches PARAMS ((rtx));\n");
      printf ("extern int insn_default_length PARAMS ((rtx));\n");
      printf ("extern int insn_variable_length_p PARAMS ((rtx));\n");
      printf ("extern int insn_current_length PARAMS ((rtx));\n\n");
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
  printf ("extern int result_ready_cost PARAMS ((rtx));\n");
  printf ("extern int function_units_used PARAMS ((rtx));\n\n");
  printf ("extern struct function_unit_desc\n");
  printf ("{\n");
  printf ("  const char *name;\n");
  printf ("  int bitmask;\n");
  printf ("  int multiplicity;\n");
  printf ("  int simultaneity;\n");
  printf ("  int default_cost;\n");
  printf ("  int max_issue_delay;\n");
  printf ("  int (*ready_cost_function) PARAMS ((rtx));\n");
  printf ("  int (*conflict_cost_function) PARAMS ((rtx, rtx));\n");
  printf ("  int max_blockage;\n");
  printf ("  unsigned int (*blockage_range_function) PARAMS ((rtx));\n");
  printf ("  int (*blockage_function) PARAMS ((rtx, rtx));\n");
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

  progname = "genattr";

  if (argc <= 1)
    fatal ("No input file name.");

  if (init_md_reader (argv[1]) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  printf ("/* Generated automatically by the program `genattr'\n\
from the machine description file `md'.  */\n\n");

  /* For compatibility, define the attribute `alternative', which is just
     a reference to the variable `which_alternative'.  */

  printf ("#define HAVE_ATTR_alternative\n");
  printf ("#define get_attr_alternative(insn) which_alternative\n");
     
  /* Read the machine description.  */

  while (1)
    {
      int line_no, insn_code_number;

      desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;

      if (GET_CODE (desc) == DEFINE_ATTR)
	gen_attr (desc);

      else if (GET_CODE (desc) == DEFINE_DELAY)
        {
	  if (! have_delay)
	    {
	      printf ("#define DELAY_SLOTS\n");
	      printf ("extern int num_delay_slots PARAMS ((rtx));\n");
	      printf ("extern int eligible_for_delay PARAMS ((rtx, int, rtx, int));\n\n");
	      printf ("extern int const_num_delay_slots PARAMS ((rtx));\n\n");
	      have_delay = 1;
	    }

	  for (i = 0; i < XVECLEN (desc, 1); i += 3)
	    {
	      if (XVECEXP (desc, 1, i + 1) && ! have_annul_true)
		{
		  printf ("#define ANNUL_IFTRUE_SLOTS\n");
		  printf ("extern int eligible_for_annul_true PARAMS ((rtx, int, rtx, int));\n");
		  have_annul_true = 1;
		}

	      if (XVECEXP (desc, 1, i + 2) && ! have_annul_false)
		{
		  printf ("#define ANNUL_IFFALSE_SLOTS\n");
		  printf ("extern int eligible_for_annul_false PARAMS ((rtx, int, rtx, int));\n");
		  have_annul_false = 1;
		}
	    }
        }

      else if (GET_CODE (desc) == DEFINE_FUNCTION_UNIT)
	{
	  const char *name = XSTR (desc, 0);
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

  /* Output flag masks for use by reorg.  

     Flags are used to hold branch direction and prediction information
     for use by eligible_for_...  */
  printf("\n#define ATTR_FLAG_forward\t0x1\n");
  printf("#define ATTR_FLAG_backward\t0x2\n");
  printf("#define ATTR_FLAG_likely\t0x4\n");
  printf("#define ATTR_FLAG_very_likely\t0x8\n");
  printf("#define ATTR_FLAG_unlikely\t0x10\n");
  printf("#define ATTR_FLAG_very_unlikely\t0x20\n");

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
