/* Generate attribute information (insn-attr.h) from machine description.
   Copyright (C) 1991, 1994, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


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
  const char *p, *tag;
  int is_const = GET_CODE (XEXP (attr, 2)) == CONST;  

  printf ("#define HAVE_ATTR_%s\n", XSTR (attr, 0));

  /* If numeric attribute, don't need to write an enum.  */
  p = XSTR (attr, 1);
  if (*p == '\0')
    printf ("extern int get_attr_%s PARAMS ((%s));\n", XSTR (attr, 0),
	    (is_const ? "void" : "rtx"));
  else
    {
      printf ("enum attr_%s {", XSTR (attr, 0));

      while ((tag = scan_comma_elt (&p)) != 0)
	{
	  write_upcase (XSTR (attr, 0));
	  putchar ('_');
	  while (tag != p)
	    putchar (TOUPPER (*tag++));
	  if (*p == ',')
	    fputs (", ", stdout);
	}

      fputs ("};\n", stdout);
      printf ("extern enum attr_%s get_attr_%s PARAMS ((%s));\n\n",
	      XSTR (attr, 0), XSTR (attr, 0), (is_const ? "void" : "rtx"));
    }

  /* If `length' attribute, write additional function definitions and define
     variables used by `insn_current_length'.  */
  if (! strcmp (XSTR (attr, 0), "length"))
    {
      puts ("\
extern void shorten_branches PARAMS ((rtx));\n\
extern int insn_default_length PARAMS ((rtx));\n\
extern int insn_variable_length_p PARAMS ((rtx));\n\
extern int insn_current_length PARAMS ((rtx));\n\n\
#include \"insn-addr.h\"\n");
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
  printf ("extern const struct function_unit_desc\n");
  printf ("{\n");
  printf ("  const char *const name;\n");
  printf ("  const int bitmask;\n");
  printf ("  const int multiplicity;\n");
  printf ("  const int simultaneity;\n");
  printf ("  const int default_cost;\n");
  printf ("  const int max_issue_delay;\n");
  printf ("  int (*const ready_cost_function) PARAMS ((rtx));\n");
  printf ("  int (*const conflict_cost_function) PARAMS ((rtx, rtx));\n");
  printf ("  const int max_blockage;\n");
  printf ("  unsigned int (*const blockage_range_function) PARAMS ((rtx));\n");
  printf ("  int (*const blockage_function) PARAMS ((rtx, rtx));\n");
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
  int num_insn_reservations = 0;
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
    fatal ("no input file name");

  if (init_md_reader_args (argc, argv) != SUCCESS_EXIT_CODE)
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `genattr'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_ATTR_H");
  puts ("#define GCC_INSN_ATTR_H\n");

  /* For compatibility, define the attribute `alternative', which is just
     a reference to the variable `which_alternative'.  */

  puts ("#define HAVE_ATTR_alternative");
  puts ("#define get_attr_alternative(insn) which_alternative");
     
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
	      unit = (struct function_unit *)
		xmalloc (sizeof (struct function_unit));
	      unit->name = xstrdup (name);
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
	    fatal ("Differing specifications given for `%s' function unit",
		   unit->name);

	  extend_range (&unit->ready_cost, ready_cost, ready_cost);
	  extend_range (&unit->issue_delay,
			issueexp_p ? 1 : issue_delay, issue_delay);
	  extend_range (&all_ready_cost,
			unit->ready_cost.min, unit->ready_cost.max);
	  extend_range (&all_issue_delay,
			unit->issue_delay.min, unit->issue_delay.max);
	}
      else if (GET_CODE (desc) == DEFINE_INSN_RESERVATION)
	num_insn_reservations++;
    }

  if (num_units > 0 || num_insn_reservations > 0)
    {
      if (num_units > 0)
	printf ("#define TRADITIONAL_PIPELINE_INTERFACE 1\n");

      if (num_insn_reservations > 0)
	printf ("#define DFA_PIPELINE_INTERFACE 1\n");

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

      /* Output interface for pipeline hazards recognition based on
	 DFA (deterministic finite state automata.  */
      printf ("\n/* DFA based pipeline interface.  */");
      printf ("\n#ifndef AUTOMATON_STATE_ALTS\n");
      printf ("#define AUTOMATON_STATE_ALTS 0\n");
      printf ("#endif\n\n");
      printf ("#ifndef CPU_UNITS_QUERY\n");
      printf ("#define CPU_UNITS_QUERY 0\n");
      printf ("#endif\n\n");
      /* Interface itself: */
      printf ("extern int max_dfa_issue_rate;\n\n");
      printf ("/* The following macro value is calculated from the\n");
      printf ("   automaton based pipeline description and is equal to\n");
      printf ("   maximal number of all insns described in constructions\n");
      printf ("   `define_insn_reservation' which can be issued on the\n");
      printf ("   same processor cycle. */\n");
      printf ("#define MAX_DFA_ISSUE_RATE max_dfa_issue_rate\n\n");
      printf ("/* Insn latency time defined in define_insn_reservation. */\n");
      printf ("extern int insn_default_latency PARAMS ((rtx));\n\n");
      printf ("/* Return nonzero if there is a bypass for given insn\n");
      printf ("   which is a data producer.  */\n");
      printf ("extern int bypass_p PARAMS ((rtx));\n\n");
      printf ("/* Insn latency time on data consumed by the 2nd insn.\n");
      printf ("   Use the function if bypass_p returns nonzero for\n");
      printf ("   the 1st insn. */\n");
      printf ("extern int insn_latency PARAMS ((rtx, rtx));\n\n");
      printf ("/* The following function returns number of alternative\n");
      printf ("   reservations of given insn.  It may be used for better\n");
      printf ("   insns scheduling heuristics. */\n");
      printf ("extern int insn_alts PARAMS ((rtx));\n\n");
      printf ("/* Maximal possible number of insns waiting results being\n");
      printf ("   produced by insns whose execution is not finished. */\n");
      printf ("extern int max_insn_queue_index;\n\n");
      printf ("/* Pointer to data describing current state of DFA.  */\n");
      printf ("typedef void *state_t;\n\n");
      printf ("/* Size of the data in bytes.  */\n");
      printf ("extern int state_size PARAMS ((void));\n\n");
      printf ("/* Initiate given DFA state, i.e. Set up the state\n");
      printf ("   as all functional units were not reserved.  */\n");
      printf ("extern void state_reset PARAMS ((state_t));\n");
      printf ("/* The following function returns negative value if given\n");
      printf ("   insn can be issued in processor state described by given\n");
      printf ("   DFA state.  In this case, the DFA state is changed to\n");
      printf ("   reflect the current and future reservations by given\n");
      printf ("   insn.  Otherwise the function returns minimal time\n");
      printf ("   delay to issue the insn.  This delay may be zero\n");
      printf ("   for superscalar or VLIW processors.  If the second\n");
      printf ("   parameter is NULL the function changes given DFA state\n");
      printf ("   as new processor cycle started.  */\n");
      printf ("extern int state_transition PARAMS ((state_t, rtx));\n");
      printf ("\n#if AUTOMATON_STATE_ALTS\n");
      printf ("/* The following function returns number of possible\n");
      printf ("   alternative reservations of given insn in given\n");
      printf ("   DFA state.  It may be used for better insns scheduling\n");
      printf ("   heuristics.  By default the function is defined if\n");
      printf ("   macro AUTOMATON_STATE_ALTS is defined because its\n");
      printf ("   implementation may require much memory.  */\n");
      printf ("extern int state_alts PARAMS ((state_t, rtx));\n");
      printf ("#endif\n\n");
      printf ("extern int min_issue_delay PARAMS ((state_t, rtx));\n");
      printf ("/* The following function returns nonzero if no one insn\n");
      printf ("   can be issued in current DFA state. */\n");
      printf ("extern int state_dead_lock_p PARAMS ((state_t));\n");
      printf ("/* The function returns minimal delay of issue of the 2nd\n");
      printf ("   insn after issuing the 1st insn in given DFA state.\n");
      printf ("   The 1st insn should be issued in given state (i.e.\n");
      printf ("    state_transition should return negative value for\n");
      printf ("    the insn and the state).  Data dependencies between\n");
      printf ("    the insns are ignored by the function.  */\n");
      printf
	("extern int min_insn_conflict_delay PARAMS ((state_t, rtx, rtx));\n");
      printf ("/* The following function outputs reservations for given\n");
      printf ("   insn as they are described in the corresponding\n");
      printf ("   define_insn_reservation.  */\n");
      printf ("extern void print_reservation PARAMS ((FILE *, rtx));\n");
      printf ("\n#if CPU_UNITS_QUERY\n");
      printf ("/* The following function returns code of functional unit\n");
      printf ("   with given name (see define_cpu_unit). */\n");
      printf ("extern int get_cpu_unit_code PARAMS ((const char *));\n");
      printf ("/* The following function returns nonzero if functional\n");
      printf ("   unit with given code is currently reserved in given\n");
      printf ("   DFA state.  */\n");
      printf ("extern int cpu_unit_reservation_p PARAMS ((state_t, int));\n");
      printf ("#endif\n\n");
      printf ("/* Initiate and finish work with DFA.  They should be\n");
      printf ("   called as the first and the last interface\n");
      printf ("   functions.  */\n");
      printf ("extern void dfa_start PARAMS ((void));\n");
      printf ("extern void dfa_finish PARAMS ((void));\n");
    }
  else
    {
      /* Otherwise we do no scheduling, but we need these typedefs
	 in order to avoid uglifying other code with more ifdefs.  */
      printf ("typedef void *state_t;\n\n");
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

  puts("\n#endif /* GCC_INSN_ATTR_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}

/* Define this so we can link with print-rtl.o to get debug_rtx function.  */
const char *
get_insn_name (code)
     int code ATTRIBUTE_UNUSED;
{
  return NULL;
}
