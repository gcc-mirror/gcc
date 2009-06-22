/* Compute different info about registers.
   Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994, 1995, 1996
   1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
   2009  Free Software Foundation, Inc.

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


/* This file contains regscan pass of the compiler and passes for
   dealing with info about modes of pseudo-registers inside
   subregisters.  It also defines some tables of information about the
   hardware registers, function init_reg_sets to initialize the
   tables, and other auxiliary functions to deal with info about
   registers and their classes.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "expr.h"
#include "tm_p.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "addresses.h"
#include "function.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "real.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"
#include "timevar.h"
#include "hashtab.h"
#include "target.h"
#include "tree-pass.h"
#include "df.h"
#include "ira.h"

/* Maximum register number used in this function, plus one.  */

int max_regno;


/* Register tables used by many passes.  */

/* Indexed by hard register number, contains 1 for registers
   that are fixed use (stack pointer, pc, frame pointer, etc.).
   These are the registers that cannot be used to allocate
   a pseudo reg for general use.  */
char fixed_regs[FIRST_PSEUDO_REGISTER];

/* Same info as a HARD_REG_SET.  */
HARD_REG_SET fixed_reg_set;

/* Data for initializing the above.  */
static const char initial_fixed_regs[] = FIXED_REGISTERS;

/* Indexed by hard register number, contains 1 for registers
   that are fixed use or are clobbered by function calls.
   These are the registers that cannot be used to allocate
   a pseudo reg whose life crosses calls unless we are able
   to save/restore them across the calls.  */
char call_used_regs[FIRST_PSEUDO_REGISTER];

/* Same info as a HARD_REG_SET.  */
HARD_REG_SET call_used_reg_set;

/* Data for initializing the above.  */
static const char initial_call_used_regs[] = CALL_USED_REGISTERS;

/* This is much like call_used_regs, except it doesn't have to
   be a superset of FIXED_REGISTERS. This vector indicates
   what is really call clobbered, and is used when defining
   regs_invalidated_by_call.  */
#ifdef CALL_REALLY_USED_REGISTERS
char call_really_used_regs[] = CALL_REALLY_USED_REGISTERS;
#endif

#ifdef CALL_REALLY_USED_REGISTERS
#define CALL_REALLY_USED_REGNO_P(X)  call_really_used_regs[X]
#else
#define CALL_REALLY_USED_REGNO_P(X)  call_used_regs[X]
#endif


/* Indexed by hard register number, contains 1 for registers that are
   fixed use or call used registers that cannot hold quantities across
   calls even if we are willing to save and restore them.  call fixed
   registers are a subset of call used registers.  */
char call_fixed_regs[FIRST_PSEUDO_REGISTER];

/* The same info as a HARD_REG_SET.  */
HARD_REG_SET call_fixed_reg_set;

/* Indexed by hard register number, contains 1 for registers
   that are being used for global register decls.
   These must be exempt from ordinary flow analysis
   and are also considered fixed.  */
char global_regs[FIRST_PSEUDO_REGISTER];

/* Contains 1 for registers that are set or clobbered by calls.  */
/* ??? Ideally, this would be just call_used_regs plus global_regs, but
   for someone's bright idea to have call_used_regs strictly include
   fixed_regs.  Which leaves us guessing as to the set of fixed_regs
   that are actually preserved.  We know for sure that those associated
   with the local stack frame are safe, but scant others.  */
HARD_REG_SET regs_invalidated_by_call;

/* Same information as REGS_INVALIDATED_BY_CALL but in regset form to be used
   in dataflow more conveniently.  */
regset regs_invalidated_by_call_regset;

/* The bitmap_obstack is used to hold some static variables that
   should not be reset after each function is compiled.  */
static bitmap_obstack persistent_obstack;

/* Table of register numbers in the order in which to try to use them.  */
#ifdef REG_ALLOC_ORDER
int reg_alloc_order[FIRST_PSEUDO_REGISTER] = REG_ALLOC_ORDER;

/* The inverse of reg_alloc_order.  */
int inv_reg_alloc_order[FIRST_PSEUDO_REGISTER];
#endif

/* For each reg class, a HARD_REG_SET saying which registers are in it.  */
HARD_REG_SET reg_class_contents[N_REG_CLASSES];

/* The same information, but as an array of unsigned ints.  We copy from
   these unsigned ints to the table above.  We do this so the tm.h files
   do not have to be aware of the wordsize for machines with <= 64 regs.
   Note that we hard-code 32 here, not HOST_BITS_PER_INT.  */
#define N_REG_INTS  \
  ((FIRST_PSEUDO_REGISTER + (32 - 1)) / 32)

static const unsigned int_reg_class_contents[N_REG_CLASSES][N_REG_INTS]
  = REG_CLASS_CONTENTS;

/* For each reg class, number of regs it contains.  */
unsigned int reg_class_size[N_REG_CLASSES];

/* For each reg class, table listing all the classes contained in it.  */
enum reg_class reg_class_subclasses[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   a largest reg class contained in their union.  */
enum reg_class reg_class_subunion[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   the smallest reg class containing their union.  */
enum reg_class reg_class_superunion[N_REG_CLASSES][N_REG_CLASSES];

/* Array containing all of the register names.  */
const char * reg_names[] = REGISTER_NAMES;

/* Array containing all of the register class names.  */
const char * reg_class_names[] = REG_CLASS_NAMES;

/* For each hard register, the widest mode object that it can contain.
   This will be a MODE_INT mode if the register can hold integers.  Otherwise
   it will be a MODE_FLOAT or a MODE_CC mode, whichever is valid for the
   register.  */
enum machine_mode reg_raw_mode[FIRST_PSEUDO_REGISTER];

/* 1 if there is a register of given mode.  */
bool have_regs_of_mode [MAX_MACHINE_MODE];

/* 1 if class does contain register of given mode.  */
char contains_reg_of_mode [N_REG_CLASSES] [MAX_MACHINE_MODE];

/* Maximum cost of moving from a register in one class to a register in
   another class.  Based on REGISTER_MOVE_COST.  */
move_table *move_cost[MAX_MACHINE_MODE];

/* Similar, but here we don't have to move if the first index is a subset
   of the second so in that case the cost is zero.  */
move_table *may_move_in_cost[MAX_MACHINE_MODE];

/* Similar, but here we don't have to move if the first index is a superset
   of the second so in that case the cost is zero.  */
move_table *may_move_out_cost[MAX_MACHINE_MODE];

/* Keep track of the last mode we initialized move costs for.  */
static int last_mode_for_init_move_cost;

/* Sample MEM values for use by memory_move_secondary_cost.  */
static GTY(()) rtx top_of_stack[MAX_MACHINE_MODE];

/* No more global register variables may be declared; true once
   reginfo has been initialized.  */
static int no_global_reg_vars = 0;

/* Specify number of hard registers given machine mode occupy.  */
unsigned char hard_regno_nregs[FIRST_PSEUDO_REGISTER][MAX_MACHINE_MODE];

/* Given a register bitmap, turn on the bits in a HARD_REG_SET that
   correspond to the hard registers, if any, set in that map.  This
   could be done far more efficiently by having all sorts of special-cases
   with moving single words, but probably isn't worth the trouble.  */
void
reg_set_to_hard_reg_set (HARD_REG_SET *to, const_bitmap from)
{
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (from, 0, i, bi)
    {
      if (i >= FIRST_PSEUDO_REGISTER)
	return;
      SET_HARD_REG_BIT (*to, i);
    }
}

/* Function called only once to initialize the above data on reg usage.
   Once this is done, various switches may override.  */
void
init_reg_sets (void)
{
  int i, j;

  /* First copy the register information from the initial int form into
     the regsets.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      CLEAR_HARD_REG_SET (reg_class_contents[i]);

      /* Note that we hard-code 32 here, not HOST_BITS_PER_INT.  */
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (int_reg_class_contents[i][j / 32]
	    & ((unsigned) 1 << (j % 32)))
	  SET_HARD_REG_BIT (reg_class_contents[i], j);
    }

  /* Sanity check: make sure the target macros FIXED_REGISTERS and
     CALL_USED_REGISTERS had the right number of initializers.  */
  gcc_assert (sizeof fixed_regs == sizeof initial_fixed_regs);
  gcc_assert (sizeof call_used_regs == sizeof initial_call_used_regs);

  memcpy (fixed_regs, initial_fixed_regs, sizeof fixed_regs);
  memcpy (call_used_regs, initial_call_used_regs, sizeof call_used_regs);
  memset (global_regs, 0, sizeof global_regs);
}

/* Initialize may_move_cost and friends for mode M.  */
void
init_move_cost (enum machine_mode m)
{
  static unsigned short last_move_cost[N_REG_CLASSES][N_REG_CLASSES];
  bool all_match = true;
  unsigned int i, j;

  gcc_assert (have_regs_of_mode[m]);
  for (i = 0; i < N_REG_CLASSES; i++)
    if (contains_reg_of_mode[i][m])
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  int cost;
	  if (!contains_reg_of_mode[j][m])
	    cost = 65535;
	  else
	    {
	      cost = REGISTER_MOVE_COST (m, (enum reg_class) i,
					 (enum reg_class) j);
	      gcc_assert (cost < 65535);
	    }
	  all_match &= (last_move_cost[i][j] == cost);
	  last_move_cost[i][j] = cost;
	}
  if (all_match && last_mode_for_init_move_cost != -1)
    {
      move_cost[m] = move_cost[last_mode_for_init_move_cost];
      may_move_in_cost[m] = may_move_in_cost[last_mode_for_init_move_cost];
      may_move_out_cost[m] = may_move_out_cost[last_mode_for_init_move_cost];
      return;
    }
  last_mode_for_init_move_cost = m;
  move_cost[m] = (move_table *)xmalloc (sizeof (move_table)
					* N_REG_CLASSES);
  may_move_in_cost[m] = (move_table *)xmalloc (sizeof (move_table)
					       * N_REG_CLASSES);
  may_move_out_cost[m] = (move_table *)xmalloc (sizeof (move_table)
					        * N_REG_CLASSES);
  for (i = 0; i < N_REG_CLASSES; i++)
    if (contains_reg_of_mode[i][m])
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  int cost;
	  enum reg_class *p1, *p2;

	  if (last_move_cost[i][j] == 65535)
	    {
	      move_cost[m][i][j] = 65535;
	      may_move_in_cost[m][i][j] = 65535;
	      may_move_out_cost[m][i][j] = 65535;
	    }
	  else
	    {
	      cost = last_move_cost[i][j];

	      for (p2 = &reg_class_subclasses[j][0];
		   *p2 != LIM_REG_CLASSES; p2++)
		if (*p2 != i && contains_reg_of_mode[*p2][m])
		  cost = MAX (cost, move_cost[m][i][*p2]);

	      for (p1 = &reg_class_subclasses[i][0];
		   *p1 != LIM_REG_CLASSES; p1++)
		if (*p1 != j && contains_reg_of_mode[*p1][m])
		  cost = MAX (cost, move_cost[m][*p1][j]);

	      gcc_assert (cost <= 65535);
	      move_cost[m][i][j] = cost;

	      if (reg_class_subset_p ((enum reg_class) i, (enum reg_class) j))
		may_move_in_cost[m][i][j] = 0;
	      else
		may_move_in_cost[m][i][j] = cost;

	      if (reg_class_subset_p ((enum reg_class) j, (enum reg_class) i))
		may_move_out_cost[m][i][j] = 0;
	      else
		may_move_out_cost[m][i][j] = cost;
	    }
	}
    else
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  move_cost[m][i][j] = 65535;
	  may_move_in_cost[m][i][j] = 65535;
	  may_move_out_cost[m][i][j] = 65535;
	}
}

/* We need to save copies of some of the register information which
   can be munged by command-line switches so we can restore it during
   subsequent back-end reinitialization.  */
static char saved_fixed_regs[FIRST_PSEUDO_REGISTER];
static char saved_call_used_regs[FIRST_PSEUDO_REGISTER];
#ifdef CALL_REALLY_USED_REGISTERS
static char saved_call_really_used_regs[FIRST_PSEUDO_REGISTER];
#endif
static const char *saved_reg_names[FIRST_PSEUDO_REGISTER];

/* Save the register information.  */
void
save_register_info (void)
{
  /* Sanity check:  make sure the target macros FIXED_REGISTERS and
     CALL_USED_REGISTERS had the right number of initializers.  */
  gcc_assert (sizeof fixed_regs == sizeof saved_fixed_regs);
  gcc_assert (sizeof call_used_regs == sizeof saved_call_used_regs);
  memcpy (saved_fixed_regs, fixed_regs, sizeof fixed_regs);
  memcpy (saved_call_used_regs, call_used_regs, sizeof call_used_regs);

  /* Likewise for call_really_used_regs.  */
#ifdef CALL_REALLY_USED_REGISTERS
  gcc_assert (sizeof call_really_used_regs
	      == sizeof saved_call_really_used_regs);
  memcpy (saved_call_really_used_regs, call_really_used_regs,
	  sizeof call_really_used_regs);
#endif

  /* And similarly for reg_names.  */
  gcc_assert (sizeof reg_names == sizeof saved_reg_names);
  memcpy (saved_reg_names, reg_names, sizeof reg_names);
}

/* Restore the register information.  */
static void
restore_register_info (void)
{
  memcpy (fixed_regs, saved_fixed_regs, sizeof fixed_regs);
  memcpy (call_used_regs, saved_call_used_regs, sizeof call_used_regs);

#ifdef CALL_REALLY_USED_REGISTERS
  memcpy (call_really_used_regs, saved_call_really_used_regs,
	  sizeof call_really_used_regs);
#endif

  memcpy (reg_names, saved_reg_names, sizeof reg_names);
}

/* After switches have been processed, which perhaps alter
   `fixed_regs' and `call_used_regs', convert them to HARD_REG_SETs.  */
static void
init_reg_sets_1 (void)
{
  unsigned int i, j;
  unsigned int /* enum machine_mode */ m;

  restore_register_info ();

#ifdef REG_ALLOC_ORDER
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    inv_reg_alloc_order[reg_alloc_order[i]] = i;
#endif

  /* This macro allows the fixed or call-used registers
     and the register classes to depend on target flags.  */

#ifdef CONDITIONAL_REGISTER_USAGE
  CONDITIONAL_REGISTER_USAGE;
#endif

  /* Compute number of hard regs in each class.  */

  memset (reg_class_size, 0, sizeof reg_class_size);
  for (i = 0; i < N_REG_CLASSES; i++)
    for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
      if (TEST_HARD_REG_BIT (reg_class_contents[i], j))
	reg_class_size[i]++;

  /* Initialize the table of subunions.
     reg_class_subunion[I][J] gets the largest-numbered reg-class
     that is contained in the union of classes I and J.  */

  memset (reg_class_subunion, 0, sizeof reg_class_subunion);
  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  HARD_REG_SET c;
	  int k;

	  COPY_HARD_REG_SET (c, reg_class_contents[i]);
	  IOR_HARD_REG_SET (c, reg_class_contents[j]);
	  for (k = 0; k < N_REG_CLASSES; k++)
	    if (hard_reg_set_subset_p (reg_class_contents[k], c)
		&& !hard_reg_set_subset_p (reg_class_contents[k],
					  reg_class_contents
					  [(int) reg_class_subunion[i][j]]))
	      reg_class_subunion[i][j] = (enum reg_class) k;
	}
    }

  /* Initialize the table of superunions.
     reg_class_superunion[I][J] gets the smallest-numbered reg-class
     containing the union of classes I and J.  */

  memset (reg_class_superunion, 0, sizeof reg_class_superunion);
  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  HARD_REG_SET c;
	  int k;

	  COPY_HARD_REG_SET (c, reg_class_contents[i]);
	  IOR_HARD_REG_SET (c, reg_class_contents[j]);
	  for (k = 0; k < N_REG_CLASSES; k++)
	    if (hard_reg_set_subset_p (c, reg_class_contents[k]))
	      break;

	  reg_class_superunion[i][j] = (enum reg_class) k;
	}
    }

  /* Initialize the tables of subclasses and superclasses of each reg class.
     First clear the whole table, then add the elements as they are found.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	reg_class_subclasses[i][j] = LIM_REG_CLASSES;
    }

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      if (i == (int) NO_REGS)
	continue;

      for (j = i + 1; j < N_REG_CLASSES; j++)
	if (hard_reg_set_subset_p (reg_class_contents[i],
				  reg_class_contents[j]))
	  {
	    /* Reg class I is a subclass of J.
	       Add J to the table of superclasses of I.  */
	    enum reg_class *p;

	    /* Add I to the table of superclasses of J.  */
	    p = &reg_class_subclasses[j][0];
	    while (*p != LIM_REG_CLASSES) p++;
	    *p = (enum reg_class) i;
	  }
    }

  /* Initialize "constant" tables.  */

  CLEAR_HARD_REG_SET (fixed_reg_set);
  CLEAR_HARD_REG_SET (call_used_reg_set);
  CLEAR_HARD_REG_SET (call_fixed_reg_set);
  CLEAR_HARD_REG_SET (regs_invalidated_by_call);
  if (!regs_invalidated_by_call_regset)
    {
      bitmap_obstack_initialize (&persistent_obstack);
      regs_invalidated_by_call_regset = ALLOC_REG_SET (&persistent_obstack);
    }
  else
    CLEAR_REG_SET (regs_invalidated_by_call_regset);

  memcpy (call_fixed_regs, fixed_regs, sizeof call_fixed_regs);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      /* call_used_regs must include fixed_regs.  */
      gcc_assert (!fixed_regs[i] || call_used_regs[i]);
#ifdef CALL_REALLY_USED_REGISTERS
      /* call_used_regs must include call_really_used_regs.  */
      gcc_assert (!call_really_used_regs[i] || call_used_regs[i]);
#endif

      if (fixed_regs[i])
	SET_HARD_REG_BIT (fixed_reg_set, i);

      if (call_used_regs[i])
	SET_HARD_REG_BIT (call_used_reg_set, i);
      if (call_fixed_regs[i])
	SET_HARD_REG_BIT (call_fixed_reg_set, i);

      /* There are a couple of fixed registers that we know are safe to
	 exclude from being clobbered by calls:

	 The frame pointer is always preserved across calls.  The arg pointer
	 is if it is fixed.  The stack pointer usually is, unless
	 RETURN_POPS_ARGS, in which case an explicit CLOBBER will be present.
	 If we are generating PIC code, the PIC offset table register is
	 preserved across calls, though the target can override that.  */

      if (i == STACK_POINTER_REGNUM)
	;
      else if (global_regs[i])
        {
	  SET_HARD_REG_BIT (regs_invalidated_by_call, i);
	  SET_REGNO_REG_SET (regs_invalidated_by_call_regset, i);
	}
      else if (i == FRAME_POINTER_REGNUM)
	;
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
      else if (i == HARD_FRAME_POINTER_REGNUM)
	;
#endif
#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
      else if (i == ARG_POINTER_REGNUM && fixed_regs[i])
	;
#endif
#ifndef PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
      else if (i == (unsigned) PIC_OFFSET_TABLE_REGNUM && fixed_regs[i])
	;
#endif
      else if (CALL_REALLY_USED_REGNO_P (i))
        {
	  SET_HARD_REG_BIT (regs_invalidated_by_call, i);
	  SET_REGNO_REG_SET (regs_invalidated_by_call_regset, i);
        }
    }

  /* Preserve global registers if called more than once.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (global_regs[i])
	{
	  fixed_regs[i] = call_used_regs[i] = call_fixed_regs[i] = 1;
	  SET_HARD_REG_BIT (fixed_reg_set, i);
	  SET_HARD_REG_BIT (call_used_reg_set, i);
	  SET_HARD_REG_BIT (call_fixed_reg_set, i);
	}
    }

  memset (have_regs_of_mode, 0, sizeof (have_regs_of_mode));
  memset (contains_reg_of_mode, 0, sizeof (contains_reg_of_mode));
  for (m = 0; m < (unsigned int) MAX_MACHINE_MODE; m++)
    {
      HARD_REG_SET ok_regs;
      CLEAR_HARD_REG_SET (ok_regs);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (!fixed_regs [j] && HARD_REGNO_MODE_OK (j, (enum machine_mode) m))
	  SET_HARD_REG_BIT (ok_regs, j);

      for (i = 0; i < N_REG_CLASSES; i++)
	if (((unsigned) CLASS_MAX_NREGS ((enum reg_class) i,
					 (enum machine_mode) m)
	     <= reg_class_size[i])
	    && hard_reg_set_intersect_p (ok_regs, reg_class_contents[i]))
	  {
	     contains_reg_of_mode [i][m] = 1;
	     have_regs_of_mode [m] = 1;
	  }
     }

  /* Reset move_cost and friends, making sure we only free shared
     table entries once.  */
  for (i = 0; i < MAX_MACHINE_MODE; i++)
    if (move_cost[i])
      {
	for (j = 0; j < i && move_cost[i] != move_cost[j]; j++)
	  ;
	if (i == j)
	  {
	    free (move_cost[i]);
	    free (may_move_in_cost[i]);
	    free (may_move_out_cost[i]);
	  }
      }
  memset (move_cost, 0, sizeof move_cost);
  memset (may_move_in_cost, 0, sizeof may_move_in_cost);
  memset (may_move_out_cost, 0, sizeof may_move_out_cost);
  last_mode_for_init_move_cost = -1;
}

/* Compute the table of register modes.
   These values are used to record death information for individual registers
   (as opposed to a multi-register mode).
   This function might be invoked more than once, if the target has support
   for changing register usage conventions on a per-function basis.
*/
void
init_reg_modes_target (void)
{
  int i, j;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    for (j = 0; j < MAX_MACHINE_MODE; j++)
      hard_regno_nregs[i][j] = HARD_REGNO_NREGS(i, (enum machine_mode)j);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      reg_raw_mode[i] = choose_hard_reg_mode (i, 1, false);

      /* If we couldn't find a valid mode, just use the previous mode.
         ??? One situation in which we need to do this is on the mips where
	 HARD_REGNO_NREGS (fpreg, [SD]Fmode) returns 2.  Ideally we'd like
	 to use DF mode for the even registers and VOIDmode for the odd
	 (for the cpu models where the odd ones are inaccessible).  */
      if (reg_raw_mode[i] == VOIDmode)
	reg_raw_mode[i] = i == 0 ? word_mode : reg_raw_mode[i-1];
    }
}

/* Finish initializing the register sets and initialize the register modes.
   This function might be invoked more than once, if the target has support
   for changing register usage conventions on a per-function basis.
*/
void
init_regs (void)
{
  /* This finishes what was started by init_reg_sets, but couldn't be done
     until after register usage was specified.  */
  init_reg_sets_1 ();
}

/* The same as previous function plus initializing IRA.  */
void
reinit_regs (void)
{
  init_regs ();
  ira_init ();
}

/* Initialize some fake stack-frame MEM references for use in
   memory_move_secondary_cost.  */
void
init_fake_stack_mems (void)
{
  int i;

  for (i = 0; i < MAX_MACHINE_MODE; i++)
    top_of_stack[i] = gen_rtx_MEM ((enum machine_mode) i, stack_pointer_rtx);
}


/* Compute extra cost of moving registers to/from memory due to reloads.
   Only needed if secondary reloads are required for memory moves.  */
int
memory_move_secondary_cost (enum machine_mode mode, enum reg_class rclass,
			    int in)
{
  enum reg_class altclass;
  int partial_cost = 0;
  /* We need a memory reference to feed to SECONDARY... macros.  */
  /* mem may be unused even if the SECONDARY_ macros are defined.  */
  rtx mem ATTRIBUTE_UNUSED = top_of_stack[(int) mode];

  altclass = secondary_reload_class (in ? 1 : 0, rclass, mode, mem);

  if (altclass == NO_REGS)
    return 0;

  if (in)
    partial_cost = REGISTER_MOVE_COST (mode, altclass, rclass);
  else
    partial_cost = REGISTER_MOVE_COST (mode, rclass, altclass);

  if (rclass == altclass)
    /* This isn't simply a copy-to-temporary situation.  Can't guess
       what it is, so MEMORY_MOVE_COST really ought not to be calling
       here in that case.

       I'm tempted to put in an assert here, but returning this will
       probably only give poor estimates, which is what we would've
       had before this code anyways.  */
    return partial_cost;

  /* Check if the secondary reload register will also need a
     secondary reload.  */
  return memory_move_secondary_cost (mode, altclass, in) + partial_cost;
}

/* Return a machine mode that is legitimate for hard reg REGNO and large
   enough to save nregs.  If we can't find one, return VOIDmode.
   If CALL_SAVED is true, only consider modes that are call saved.  */
enum machine_mode
choose_hard_reg_mode (unsigned int regno ATTRIBUTE_UNUSED,
		      unsigned int nregs, bool call_saved)
{
  unsigned int /* enum machine_mode */ m;
  enum machine_mode found_mode = VOIDmode, mode;

  /* We first look for the largest integer mode that can be validly
     held in REGNO.  If none, we look for the largest floating-point mode.
     If we still didn't find a valid mode, try CCmode.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if ((unsigned) hard_regno_nregs[regno][mode] == nregs
	&& HARD_REGNO_MODE_OK (regno, mode)
	&& (! call_saved || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if ((unsigned) hard_regno_nregs[regno][mode] == nregs
	&& HARD_REGNO_MODE_OK (regno, mode)
	&& (! call_saved || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if ((unsigned) hard_regno_nregs[regno][mode] == nregs
	&& HARD_REGNO_MODE_OK (regno, mode)
	&& (! call_saved || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if ((unsigned) hard_regno_nregs[regno][mode] == nregs
	&& HARD_REGNO_MODE_OK (regno, mode)
	&& (! call_saved || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  /* Iterate over all of the CCmodes.  */
  for (m = (unsigned int) CCmode; m < (unsigned int) NUM_MACHINE_MODES; ++m)
    {
      mode = (enum machine_mode) m;
      if ((unsigned) hard_regno_nregs[regno][mode] == nregs
	  && HARD_REGNO_MODE_OK (regno, mode)
	  && (! call_saved || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
	return mode;
    }

  /* We can't find a mode valid for this register.  */
  return VOIDmode;
}

/* Specify the usage characteristics of the register named NAME.
   It should be a fixed register if FIXED and a
   call-used register if CALL_USED.  */
void
fix_register (const char *name, int fixed, int call_used)
{
  int i;

  /* Decode the name and update the primary form of
     the register info.  */

  if ((i = decode_reg_name (name)) >= 0)
    {
      if ((i == STACK_POINTER_REGNUM
#ifdef HARD_FRAME_POINTER_REGNUM
	   || i == HARD_FRAME_POINTER_REGNUM
#else
	   || i == FRAME_POINTER_REGNUM
#endif
	   )
	  && (fixed == 0 || call_used == 0))
	{
	  static const char * const what_option[2][2] = {
	    { "call-saved", "call-used" },
	    { "no-such-option", "fixed" }};

	  error ("can't use '%s' as a %s register", name,
		 what_option[fixed][call_used]);
	}
      else
	{
	  fixed_regs[i] = fixed;
	  call_used_regs[i] = call_used;
#ifdef CALL_REALLY_USED_REGISTERS
	  if (fixed == 0)
	    call_really_used_regs[i] = call_used;
#endif
	}
    }
  else
    {
      warning (0, "unknown register name: %s", name);
    }
}

/* Mark register number I as global.  */
void
globalize_reg (int i)
{
  if (fixed_regs[i] == 0 && no_global_reg_vars)
    error ("global register variable follows a function definition");

  if (global_regs[i])
    {
      warning (0, "register used for two global register variables");
      return;
    }

  if (call_used_regs[i] && ! fixed_regs[i])
    warning (0, "call-clobbered register used for global register variable");

  global_regs[i] = 1;

  /* If we're globalizing the frame pointer, we need to set the
     appropriate regs_invalidated_by_call bit, even if it's already
     set in fixed_regs.  */
  if (i != STACK_POINTER_REGNUM)
    {
      SET_HARD_REG_BIT (regs_invalidated_by_call, i);
      SET_REGNO_REG_SET (regs_invalidated_by_call_regset, i);
    }

  /* If already fixed, nothing else to do.  */
  if (fixed_regs[i])
    return;

  fixed_regs[i] = call_used_regs[i] = call_fixed_regs[i] = 1;
#ifdef CALL_REALLY_USED_REGISTERS
  call_really_used_regs[i] = 1;
#endif

  SET_HARD_REG_BIT (fixed_reg_set, i);
  SET_HARD_REG_BIT (call_used_reg_set, i);
  SET_HARD_REG_BIT (call_fixed_reg_set, i);

  reinit_regs ();
}


/* Structure used to record preferences of given pseudo.  */
struct reg_pref
{
  /* (enum reg_class) prefclass is the preferred class.  May be
     NO_REGS if no class is better than memory.  */
  char prefclass;

  /* altclass is a register class that we should use for allocating
     pseudo if no register in the preferred class is available.
     If no register in this class is available, memory is preferred.

     It might appear to be more general to have a bitmask of classes here,
     but since it is recommended that there be a class corresponding to the
     union of most major pair of classes, that generality is not required.  */
  char altclass;
};

/* Record preferences of each pseudo.  This is available after RA is
   run.  */
static struct reg_pref *reg_pref;

/* Return the reg_class in which pseudo reg number REGNO is best allocated.
   This function is sometimes called before the info has been computed.
   When that happens, just return GENERAL_REGS, which is innocuous.  */
enum reg_class
reg_preferred_class (int regno)
{
  if (reg_pref == 0)
    return GENERAL_REGS;

  return (enum reg_class) reg_pref[regno].prefclass;
}

enum reg_class
reg_alternate_class (int regno)
{
  if (reg_pref == 0)
    return ALL_REGS;

  return (enum reg_class) reg_pref[regno].altclass;
}

/* Initialize some global data for this pass.  */
static unsigned int 
reginfo_init (void)
{
  if (df)
    df_compute_regs_ever_live (true);

  /* This prevents dump_flow_info from losing if called
     before reginfo is run.  */
  reg_pref = NULL;

  /* No more global register variables may be declared.  */
  no_global_reg_vars = 1;
  return 1;
}

struct rtl_opt_pass pass_reginfo_init =
{
 {
  RTL_PASS,
  "reginfo",                            /* name */
  NULL,                                 /* gate */
  reginfo_init,                         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};



/* Allocate space for reg info.  */
void
allocate_reg_info (void)
{
  int size = max_reg_num ();

  gcc_assert (! reg_pref && ! reg_renumber);
  reg_renumber = XNEWVEC (short, size);
  reg_pref = XCNEWVEC (struct reg_pref, size);
  memset (reg_renumber, -1, size * sizeof (short));
}


/* Resize reg info. The new elements will be uninitialized.  */
void
resize_reg_info (void)
{
  int size = max_reg_num ();

  gcc_assert (reg_pref && reg_renumber);
  reg_renumber = XRESIZEVEC (short, reg_renumber, size);
  reg_pref = XRESIZEVEC (struct reg_pref, reg_pref, size);
}


/* Free up the space allocated by allocate_reg_info.  */
void
free_reg_info (void)
{
  if (reg_pref)
    {
      free (reg_pref);
      reg_pref = NULL;
    }

  if (reg_renumber)
    {
      free (reg_renumber);
      reg_renumber = NULL;
    }
}




/* Set up preferred and alternate classes for REGNO as PREFCLASS and
   ALTCLASS.  */
void
setup_reg_classes (int regno,
		   enum reg_class prefclass, enum reg_class altclass)
{
  if (reg_pref == NULL)
    return;
  reg_pref[regno].prefclass = prefclass;
  reg_pref[regno].altclass = altclass;
}


/* This is the `regscan' pass of the compiler, run just before cse and
   again just before loop.  It finds the first and last use of each
   pseudo-register.  */

static void reg_scan_mark_refs (rtx, rtx);

void
reg_scan (rtx f, unsigned int nregs ATTRIBUTE_UNUSED)
{
  rtx insn;

  timevar_push (TV_REG_SCAN);

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	reg_scan_mark_refs (PATTERN (insn), insn);
	if (REG_NOTES (insn))
	  reg_scan_mark_refs (REG_NOTES (insn), insn);
      }

  timevar_pop (TV_REG_SCAN);
}


/* X is the expression to scan.  INSN is the insn it appears in.
   NOTE_FLAG is nonzero if X is from INSN's notes rather than its body.
   We should only record information for REGs with numbers
   greater than or equal to MIN_REGNO.  */
static void
reg_scan_mark_refs (rtx x, rtx insn)
{
  enum rtx_code code;
  rtx dest;
  rtx note;

  if (!x)
    return;
  code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
    case CONST_VECTOR:
    case CC0:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case REG:
      return;

    case EXPR_LIST:
      if (XEXP (x, 0))
	reg_scan_mark_refs (XEXP (x, 0), insn);
      if (XEXP (x, 1))
	reg_scan_mark_refs (XEXP (x, 1), insn);
      break;

    case INSN_LIST:
      if (XEXP (x, 1))
	reg_scan_mark_refs (XEXP (x, 1), insn);
      break;

    case CLOBBER:
      if (MEM_P (XEXP (x, 0)))
	reg_scan_mark_refs (XEXP (XEXP (x, 0), 0), insn);
      break;

    case SET:
      /* Count a set of the destination if it is a register.  */
      for (dest = SET_DEST (x);
	   GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART
	   || GET_CODE (dest) == ZERO_EXTEND;
	   dest = XEXP (dest, 0))
	;

      /* If this is setting a pseudo from another pseudo or the sum of a
	 pseudo and a constant integer and the other pseudo is known to be
	 a pointer, set the destination to be a pointer as well.

	 Likewise if it is setting the destination from an address or from a
	 value equivalent to an address or to the sum of an address and
	 something else.

	 But don't do any of this if the pseudo corresponds to a user
	 variable since it should have already been set as a pointer based
	 on the type.  */

      if (REG_P (SET_DEST (x))
	  && REGNO (SET_DEST (x)) >= FIRST_PSEUDO_REGISTER
	  /* If the destination pseudo is set more than once, then other
	     sets might not be to a pointer value (consider access to a
	     union in two threads of control in the presence of global
	     optimizations).  So only set REG_POINTER on the destination
	     pseudo if this is the only set of that pseudo.  */
	  && DF_REG_DEF_COUNT (REGNO (SET_DEST (x))) == 1
	  && ! REG_USERVAR_P (SET_DEST (x))
	  && ! REG_POINTER (SET_DEST (x))
	  && ((REG_P (SET_SRC (x))
	       && REG_POINTER (SET_SRC (x)))
	      || ((GET_CODE (SET_SRC (x)) == PLUS
		   || GET_CODE (SET_SRC (x)) == LO_SUM)
		  && CONST_INT_P (XEXP (SET_SRC (x), 1))
		  && REG_P (XEXP (SET_SRC (x), 0))
		  && REG_POINTER (XEXP (SET_SRC (x), 0)))
	      || GET_CODE (SET_SRC (x)) == CONST
	      || GET_CODE (SET_SRC (x)) == SYMBOL_REF
	      || GET_CODE (SET_SRC (x)) == LABEL_REF
	      || (GET_CODE (SET_SRC (x)) == HIGH
		  && (GET_CODE (XEXP (SET_SRC (x), 0)) == CONST
		      || GET_CODE (XEXP (SET_SRC (x), 0)) == SYMBOL_REF
		      || GET_CODE (XEXP (SET_SRC (x), 0)) == LABEL_REF))
	      || ((GET_CODE (SET_SRC (x)) == PLUS
		   || GET_CODE (SET_SRC (x)) == LO_SUM)
		  && (GET_CODE (XEXP (SET_SRC (x), 1)) == CONST
		      || GET_CODE (XEXP (SET_SRC (x), 1)) == SYMBOL_REF
		      || GET_CODE (XEXP (SET_SRC (x), 1)) == LABEL_REF))
	      || ((note = find_reg_note (insn, REG_EQUAL, 0)) != 0
		  && (GET_CODE (XEXP (note, 0)) == CONST
		      || GET_CODE (XEXP (note, 0)) == SYMBOL_REF
		      || GET_CODE (XEXP (note, 0)) == LABEL_REF))))
	REG_POINTER (SET_DEST (x)) = 1;

      /* If this is setting a register from a register or from a simple
	 conversion of a register, propagate REG_EXPR.  */
      if (REG_P (dest) && !REG_ATTRS (dest))
	{
	  rtx src = SET_SRC (x);

	  while (GET_CODE (src) == SIGN_EXTEND
		 || GET_CODE (src) == ZERO_EXTEND
		 || GET_CODE (src) == TRUNCATE
		 || (GET_CODE (src) == SUBREG && subreg_lowpart_p (src)))
	    src = XEXP (src, 0);

	  set_reg_attrs_from_value (dest, src);
	}

      /* ... fall through ...  */

    default:
      {
	const char *fmt = GET_RTX_FORMAT (code);
	int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  {
	    if (fmt[i] == 'e')
	      reg_scan_mark_refs (XEXP (x, i), insn);
	    else if (fmt[i] == 'E' && XVEC (x, i) != 0)
	      {
		int j;
		for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		  reg_scan_mark_refs (XVECEXP (x, i, j), insn);
	      }
	  }
      }
    }
}


/* Return nonzero if C1 is a subset of C2, i.e., if every register in C1
   is also in C2.  */
int
reg_class_subset_p (enum reg_class c1, enum reg_class c2)
{
  return (c1 == c2
	  || c2 == ALL_REGS
	  || hard_reg_set_subset_p (reg_class_contents[(int) c1],
				   reg_class_contents[(int) c2]));
}

/* Return nonzero if there is a register that is in both C1 and C2.  */
int
reg_classes_intersect_p (enum reg_class c1, enum reg_class c2)
{
  return (c1 == c2
	  || c1 == ALL_REGS
	  || c2 == ALL_REGS
	  || hard_reg_set_intersect_p (reg_class_contents[(int) c1],
				      reg_class_contents[(int) c2]));
}



/* Passes for keeping and updating info about modes of registers
   inside subregisters.  */

#ifdef CANNOT_CHANGE_MODE_CLASS

struct subregs_of_mode_node
{
  unsigned int block;
  unsigned char modes[MAX_MACHINE_MODE];
};

static htab_t subregs_of_mode;

static hashval_t
som_hash (const void *x)
{
  const struct subregs_of_mode_node *const a =
    (const struct subregs_of_mode_node *) x;
  return a->block;
}

static int
som_eq (const void *x, const void *y)
{
  const struct subregs_of_mode_node *const a =
    (const struct subregs_of_mode_node *) x;
  const struct subregs_of_mode_node *const b =
    (const struct subregs_of_mode_node *) y;
  return a->block == b->block;
}

static void
record_subregs_of_mode (rtx subreg)
{
  struct subregs_of_mode_node dummy, *node;
  enum machine_mode mode;
  unsigned int regno;
  void **slot;

  if (!REG_P (SUBREG_REG (subreg)))
    return;

  regno = REGNO (SUBREG_REG (subreg));
  mode = GET_MODE (subreg);

  if (regno < FIRST_PSEUDO_REGISTER)
    return;

  dummy.block = regno & -8;
  slot = htab_find_slot_with_hash (subregs_of_mode, &dummy,
				   dummy.block, INSERT);
  node = (struct subregs_of_mode_node *) *slot;
  if (node == NULL)
    {
      node = XCNEW (struct subregs_of_mode_node);
      node->block = regno & -8;
      *slot = node;
    }

  node->modes[mode] |= 1 << (regno & 7);
}

/* Call record_subregs_of_mode for all the subregs in X.  */
static void 
find_subregs_of_mode (rtx x)
{
  enum rtx_code code = GET_CODE (x);
  const char * const fmt = GET_RTX_FORMAT (code);
  int i;

  if (code == SUBREG)
    record_subregs_of_mode (x);
    
  /* Time for some deep diving.  */
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	find_subregs_of_mode (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    find_subregs_of_mode (XVECEXP (x, i, j));
	}
    }
}

static unsigned int
init_subregs_of_mode (void)
{
  basic_block bb;
  rtx insn;

  if (subregs_of_mode)
    htab_empty (subregs_of_mode);
  else
    subregs_of_mode = htab_create (100, som_hash, som_eq, free);

  FOR_EACH_BB (bb)
    FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      find_subregs_of_mode (PATTERN (insn));

  return 0;
}

/* Return 1 if REGNO has had an invalid mode change in CLASS from FROM
   mode.  */
bool
invalid_mode_change_p (unsigned int regno,
		       enum reg_class rclass ATTRIBUTE_UNUSED,
		       enum machine_mode from)
{
  struct subregs_of_mode_node dummy, *node;
  unsigned int to;
  unsigned char mask;

  gcc_assert (subregs_of_mode);
  dummy.block = regno & -8;
  node = (struct subregs_of_mode_node *)
    htab_find_with_hash (subregs_of_mode, &dummy, dummy.block);
  if (node == NULL)
    return false;

  mask = 1 << (regno & 7);
  for (to = VOIDmode; to < NUM_MACHINE_MODES; to++)
    if (node->modes[to] & mask)
      if (CANNOT_CHANGE_MODE_CLASS (from, (enum machine_mode) to, rclass))
	return true;

  return false;
}

static unsigned int
finish_subregs_of_mode (void)
{
  htab_delete (subregs_of_mode);
  subregs_of_mode = 0;
  return 0;
}
#else
static unsigned int
init_subregs_of_mode (void)
{
  return 0;
}
static unsigned int
finish_subregs_of_mode (void)
{
  return 0;
}

#endif /* CANNOT_CHANGE_MODE_CLASS */

static bool
gate_subregs_of_mode_init (void)
{
#ifdef CANNOT_CHANGE_MODE_CLASS
  return true;
#else
  return false;
#endif
}

struct rtl_opt_pass pass_subregs_of_mode_init =
{
 {
  RTL_PASS,
  "subregs_of_mode_init",               /* name */
  gate_subregs_of_mode_init,            /* gate */
  init_subregs_of_mode,                 /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};

struct rtl_opt_pass pass_subregs_of_mode_finish =
{
 {
  RTL_PASS,
  "subregs_of_mode_finish",               /* name */
  gate_subregs_of_mode_init,            /* gate */
  finish_subregs_of_mode,               /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};


#include "gt-reginfo.h"
