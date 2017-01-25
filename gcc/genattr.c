/* Generate attribute information (insn-attr.h) from machine description.
   Copyright (C) 1991-2017 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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


#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"


static vec<rtx> const_attrs, reservations;


static void
gen_attr (md_rtx_info *info)
{
  const char *p;
  rtx attr = info->def;
  int is_const = GET_CODE (XEXP (attr, 2)) == CONST;

  if (is_const)
    const_attrs.safe_push (attr);

  printf ("#define HAVE_ATTR_%s 1\n", XSTR (attr, 0));

  /* If numeric attribute, don't need to write an enum.  */
  if (GET_CODE (attr) == DEFINE_ENUM_ATTR)
    printf ("extern enum %s get_attr_%s (%s);\n\n",
	    XSTR (attr, 1), XSTR (attr, 0),
	    (is_const ? "void" : "rtx_insn *"));
  else
    {
      p = XSTR (attr, 1);
      if (*p == '\0')
	printf ("extern int get_attr_%s (%s);\n", XSTR (attr, 0),
		(is_const ? "void" : "rtx_insn *"));
      else
	printf ("extern enum attr_%s get_attr_%s (%s);\n\n",
		XSTR (attr, 0), XSTR (attr, 0),
		(is_const ? "void" : "rtx_insn *"));
    }

  /* If `length' attribute, write additional function definitions and define
     variables used by `insn_current_length'.  */
  if (! strcmp (XSTR (attr, 0), "length"))
    {
      puts ("\
extern void shorten_branches (rtx_insn *);\n\
extern int insn_default_length (rtx_insn *);\n\
extern int insn_min_length (rtx_insn *);\n\
extern int insn_variable_length_p (rtx_insn *);\n\
extern int insn_current_length (rtx_insn *);\n\n\
#include \"insn-addr.h\"\n");
    }
}

/* Check that attribute NAME is used in define_insn_reservation condition
   EXP.  Return true if it is.  */
static bool
check_tune_attr (const char *name, rtx exp)
{
  switch (GET_CODE (exp))
    {
    case AND:
      if (check_tune_attr (name, XEXP (exp, 0)))
	return true;
      return check_tune_attr (name, XEXP (exp, 1));

    case IOR:
      return (check_tune_attr (name, XEXP (exp, 0))
	      && check_tune_attr (name, XEXP (exp, 1)));

    case EQ_ATTR:
      return strcmp (XSTR (exp, 0), name) == 0;

    default:
      return false;
    }
}

/* Try to find a const attribute (usually cpu or tune) that is used
   in all define_insn_reservation conditions.  */
static bool
find_tune_attr (rtx exp)
{
  unsigned int i;
  rtx attr;

  switch (GET_CODE (exp))
    {
    case AND:
    case IOR:
      if (find_tune_attr (XEXP (exp, 0)))
	return true;
      return find_tune_attr (XEXP (exp, 1));

    case EQ_ATTR:
      if (strcmp (XSTR (exp, 0), "alternative") == 0)
	return false;

      FOR_EACH_VEC_ELT (const_attrs, i, attr)
	if (strcmp (XSTR (attr, 0), XSTR (exp, 0)) == 0)
	  {
	    unsigned int j;
	    rtx resv;

	    FOR_EACH_VEC_ELT (reservations, j, resv)
	      if (! check_tune_attr (XSTR (attr, 0), XEXP (resv, 2)))
		return false;
	    return true;
	  }
      return false;

    default:
      return false;
    }
}

int
main (int argc, const char **argv)
{
  bool have_annul_true = false;
  bool have_annul_false = false;
  int num_insn_reservations = 0;
  int i;

  progname = "genattr";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  puts ("/* Generated automatically by the program `genattr'");
  puts ("   from the machine description file `md'.  */\n");
  puts ("#ifndef GCC_INSN_ATTR_H");
  puts ("#define GCC_INSN_ATTR_H\n");

  puts ("#include \"insn-attr-common.h\"\n");

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    {
      rtx def = info.def;
      switch (GET_CODE (def))
	{
	case DEFINE_ATTR:
	case DEFINE_ENUM_ATTR:
	  gen_attr (&info);
	  break;

	case DEFINE_DELAY:
	  for (i = 0; i < XVECLEN (def, 1); i += 3)
	    {
	      if (XVECEXP (def, 1, i + 1))
		have_annul_true = true;

	      if (XVECEXP (def, 1, i + 2))
		have_annul_false = true;
	    }
	  break;

	case DEFINE_INSN_RESERVATION:
	  num_insn_reservations++;
	  reservations.safe_push (def);
	  break;

	default:
	  break;
	}
    }

  printf ("extern int num_delay_slots (rtx_insn *);\n");
  printf ("extern int eligible_for_delay (rtx_insn *, int, rtx_insn *, int);\n\n");
  printf ("extern int const_num_delay_slots (rtx_insn *);\n\n");
  printf ("#define ANNUL_IFTRUE_SLOTS %d\n", have_annul_true);
  printf ("extern int eligible_for_annul_true (rtx_insn *, int, rtx_insn *, int);\n");
  printf ("#define ANNUL_IFFALSE_SLOTS %d\n", have_annul_false);
  printf ("extern int eligible_for_annul_false (rtx_insn *, int, rtx_insn *, int);\n");

  if (num_insn_reservations > 0)
    {
      bool has_tune_attr
	= find_tune_attr (XEXP (reservations[0], 2));
      /* Output interface for pipeline hazards recognition based on
	 DFA (deterministic finite state automata.  */
      printf ("\n/* DFA based pipeline interface.  */");
      printf ("\n#ifndef AUTOMATON_ALTS\n");
      printf ("#define AUTOMATON_ALTS 0\n");
      printf ("#endif\n\n");
      printf ("\n#ifndef AUTOMATON_STATE_ALTS\n");
      printf ("#define AUTOMATON_STATE_ALTS 0\n");
      printf ("#endif\n\n");
      printf ("#ifndef CPU_UNITS_QUERY\n");
      printf ("#define CPU_UNITS_QUERY 0\n");
      printf ("#endif\n\n");
      /* Interface itself: */
      if (has_tune_attr)
	{
	  printf ("/* Initialize fn pointers for internal_dfa_insn_code\n");
	  printf ("   and insn_default_latency.  */\n");
	  printf ("extern void init_sched_attrs (void);\n\n");
	  printf ("/* Internal insn code number used by automata.  */\n");
	  printf ("extern int (*internal_dfa_insn_code) (rtx_insn *);\n\n");
	  printf ("/* Insn latency time defined in define_insn_reservation. */\n");
	  printf ("extern int (*insn_default_latency) (rtx_insn *);\n\n");
	}
      else
	{
	  printf ("#define init_sched_attrs() do { } while (0)\n\n");
	  printf ("/* Internal insn code number used by automata.  */\n");
	  printf ("extern int internal_dfa_insn_code (rtx_insn *);\n\n");
	  printf ("/* Insn latency time defined in define_insn_reservation. */\n");
	  printf ("extern int insn_default_latency (rtx_insn *);\n\n");
	}
      printf ("/* Return nonzero if there is a bypass for given insn\n");
      printf ("   which is a data producer.  */\n");
      printf ("extern int bypass_p (rtx_insn *);\n\n");
      printf ("/* Insn latency time on data consumed by the 2nd insn.\n");
      printf ("   Use the function if bypass_p returns nonzero for\n");
      printf ("   the 1st insn. */\n");
      printf ("extern int insn_latency (rtx_insn *, rtx_insn *);\n\n");
      printf ("/* Maximal insn latency time possible of all bypasses for this insn.\n");
      printf ("   Use the function if bypass_p returns nonzero for\n");
      printf ("   the 1st insn. */\n");
      printf ("extern int maximal_insn_latency (rtx_insn *);\n\n");
      printf ("\n#if AUTOMATON_ALTS\n");
      printf ("/* The following function returns number of alternative\n");
      printf ("   reservations of given insn.  It may be used for better\n");
      printf ("   insns scheduling heuristics. */\n");
      printf ("extern int insn_alts (rtx);\n\n");
      printf ("#endif\n\n");
      printf ("/* Maximal possible number of insns waiting results being\n");
      printf ("   produced by insns whose execution is not finished. */\n");
      printf ("extern const int max_insn_queue_index;\n\n");
      printf ("/* Pointer to data describing current state of DFA.  */\n");
      printf ("typedef void *state_t;\n\n");
      printf ("/* Size of the data in bytes.  */\n");
      printf ("extern int state_size (void);\n\n");
      printf ("/* Initiate given DFA state, i.e. Set up the state\n");
      printf ("   as all functional units were not reserved.  */\n");
      printf ("extern void state_reset (state_t);\n");
      printf ("/* The following function returns negative value if given\n");
      printf ("   insn can be issued in processor state described by given\n");
      printf ("   DFA state.  In this case, the DFA state is changed to\n");
      printf ("   reflect the current and future reservations by given\n");
      printf ("   insn.  Otherwise the function returns minimal time\n");
      printf ("   delay to issue the insn.  This delay may be zero\n");
      printf ("   for superscalar or VLIW processors.  If the second\n");
      printf ("   parameter is NULL the function changes given DFA state\n");
      printf ("   as new processor cycle started.  */\n");
      printf ("extern int state_transition (state_t, rtx);\n");
      printf ("\n#if AUTOMATON_STATE_ALTS\n");
      printf ("/* The following function returns number of possible\n");
      printf ("   alternative reservations of given insn in given\n");
      printf ("   DFA state.  It may be used for better insns scheduling\n");
      printf ("   heuristics.  By default the function is defined if\n");
      printf ("   macro AUTOMATON_STATE_ALTS is defined because its\n");
      printf ("   implementation may require much memory.  */\n");
      printf ("extern int state_alts (state_t, rtx);\n");
      printf ("#endif\n\n");
      printf ("extern int min_issue_delay (state_t, rtx_insn *);\n");
      printf ("/* The following function returns nonzero if no one insn\n");
      printf ("   can be issued in current DFA state. */\n");
      printf ("extern int state_dead_lock_p (state_t);\n");
      printf ("/* The function returns minimal delay of issue of the 2nd\n");
      printf ("   insn after issuing the 1st insn in given DFA state.\n");
      printf ("   The 1st insn should be issued in given state (i.e.\n");
      printf ("    state_transition should return negative value for\n");
      printf ("    the insn and the state).  Data dependencies between\n");
      printf ("    the insns are ignored by the function.  */\n");
      printf ("extern int "
              "min_insn_conflict_delay (state_t, rtx_insn *, rtx_insn *);\n");
      printf ("/* The following function outputs reservations for given\n");
      printf ("   insn as they are described in the corresponding\n");
      printf ("   define_insn_reservation.  */\n");
      printf ("extern void print_reservation (FILE *, rtx_insn *);\n");
      printf ("\n#if CPU_UNITS_QUERY\n");
      printf ("/* The following function returns code of functional unit\n");
      printf ("   with given name (see define_cpu_unit). */\n");
      printf ("extern int get_cpu_unit_code (const char *);\n");
      printf ("/* The following function returns nonzero if functional\n");
      printf ("   unit with given code is currently reserved in given\n");
      printf ("   DFA state.  */\n");
      printf ("extern int cpu_unit_reservation_p (state_t, int);\n");
      printf ("#endif\n\n");
      printf ("/* The following function returns true if insn\n");
      printf ("   has a dfa reservation.  */\n");
      printf ("extern bool insn_has_dfa_reservation_p (rtx_insn *);\n\n");
      printf ("/* Clean insn code cache.  It should be called if there\n");
      printf ("   is a chance that condition value in a\n");
      printf ("   define_insn_reservation will be changed after\n");
      printf ("   last call of dfa_start.  */\n");
      printf ("extern void dfa_clean_insn_cache (void);\n\n");
      printf ("extern void dfa_clear_single_insn_cache (rtx_insn *);\n\n");
      printf ("/* Initiate and finish work with DFA.  They should be\n");
      printf ("   called as the first and the last interface\n");
      printf ("   functions.  */\n");
      printf ("extern void dfa_start (void);\n");
      printf ("extern void dfa_finish (void);\n");
    }
  else
    {
      /* Otherwise we do no scheduling, but we need these typedefs
	 in order to avoid uglifying other code with more ifdefs.  */
      printf ("typedef void *state_t;\n\n");
    }

  /* Special-purpose attributes should be tested with if, not #ifdef.  */
  const char * const special_attrs[] = { "length", "enabled",
					 "preferred_for_size",
					 "preferred_for_speed", 0 };
  for (const char * const *p = special_attrs; *p; p++)
    {
      printf ("#ifndef HAVE_ATTR_%s\n"
	      "#define HAVE_ATTR_%s 0\n"
	      "#endif\n", *p, *p);
    }
  /* We make an exception here to provide stub definitions for
     insn_*_length* / get_attr_enabled functions.  */
  puts ("#if !HAVE_ATTR_length\n"
	"extern int hook_int_rtx_insn_unreachable (rtx_insn *);\n"
	"#define insn_default_length hook_int_rtx_insn_unreachable\n"
	"#define insn_min_length hook_int_rtx_insn_unreachable\n"
	"#define insn_variable_length_p hook_int_rtx_insn_unreachable\n"
	"#define insn_current_length hook_int_rtx_insn_unreachable\n"
	"#include \"insn-addr.h\"\n"
	"#endif\n"
	"extern int hook_int_rtx_1 (rtx);\n"
	"#if !HAVE_ATTR_enabled\n"
	"#define get_attr_enabled hook_int_rtx_1\n"
	"#endif\n"
	"#if !HAVE_ATTR_preferred_for_size\n"
	"#define get_attr_preferred_for_size hook_int_rtx_1\n"
	"#endif\n"
	"#if !HAVE_ATTR_preferred_for_speed\n"
	"#define get_attr_preferred_for_speed hook_int_rtx_1\n"
	"#endif\n");

  /* Output flag masks for use by reorg.

     Flags are used to hold branch direction for use by eligible_for_...  */
  printf ("\n#define ATTR_FLAG_forward\t0x1\n");
  printf ("#define ATTR_FLAG_backward\t0x2\n");

  puts ("\n#endif /* GCC_INSN_ATTR_H */");

  if (ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
