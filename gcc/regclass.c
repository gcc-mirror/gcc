/* Compute register class preferences for pseudo-registers.
   Copyright (C) 1987, 88, 91-96, 1997 Free Software Foundation, Inc.

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


/* This file contains two passes of the compiler: reg_scan and reg_class.
   It also defines some tables of information about the hardware registers
   and a function init_reg_sets to initialize the tables.  */

#include "config.h"
#include <stdio.h>
#include "rtl.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "real.h"
#include "bytecode.h"

#ifndef REGISTER_MOVE_COST
#define REGISTER_MOVE_COST(x, y) 2
#endif

#ifndef MEMORY_MOVE_COST
#define MEMORY_MOVE_COST(x) 4
#endif

/* If we have auto-increment or auto-decrement and we can have secondary
   reloads, we are not allowed to use classes requiring secondary
   reloads for pseudos auto-incremented since reload can't handle it.  */

#ifdef AUTO_INC_DEC
#if defined(SECONDARY_INPUT_RELOAD_CLASS) || defined(SECONDARY_OUTPUT_RELOAD_CLASS)
#define FORBIDDEN_INC_DEC_CLASSES
#endif
#endif

/* Register tables used by many passes.  */

/* Indexed by hard register number, contains 1 for registers
   that are fixed use (stack pointer, pc, frame pointer, etc.).
   These are the registers that cannot be used to allocate
   a pseudo reg whose life does not cross calls.  */

char fixed_regs[FIRST_PSEUDO_REGISTER];

/* Same info as a HARD_REG_SET.  */

HARD_REG_SET fixed_reg_set;

/* Data for initializing the above.  */

static char initial_fixed_regs[] = FIXED_REGISTERS;

/* Indexed by hard register number, contains 1 for registers
   that are fixed use or are clobbered by function calls.
   These are the registers that cannot be used to allocate
   a pseudo reg whose life crosses calls.  */

char call_used_regs[FIRST_PSEUDO_REGISTER];

/* Same info as a HARD_REG_SET.  */

HARD_REG_SET call_used_reg_set;

/* HARD_REG_SET of registers we want to avoid caller saving.  */
HARD_REG_SET losing_caller_save_reg_set;

/* Data for initializing the above.  */

static char initial_call_used_regs[] = CALL_USED_REGISTERS;
  
/* Indexed by hard register number, contains 1 for registers that are
   fixed use -- i.e. in fixed_regs -- or a function value return register
   or STRUCT_VALUE_REGNUM or STATIC_CHAIN_REGNUM.  These are the
   registers that cannot hold quantities across calls even if we are
   willing to save and restore them.  */

char call_fixed_regs[FIRST_PSEUDO_REGISTER];

/* The same info as a HARD_REG_SET.  */

HARD_REG_SET call_fixed_reg_set;

/* Number of non-fixed registers.  */

int n_non_fixed_regs;

/* Indexed by hard register number, contains 1 for registers
   that are being used for global register decls.
   These must be exempt from ordinary flow analysis
   and are also considered fixed.  */

char global_regs[FIRST_PSEUDO_REGISTER];
  
/* Table of register numbers in the order in which to try to use them.  */
#ifdef REG_ALLOC_ORDER
int reg_alloc_order[FIRST_PSEUDO_REGISTER] = REG_ALLOC_ORDER;
#endif

/* For each reg class, a HARD_REG_SET saying which registers are in it.  */

HARD_REG_SET reg_class_contents[N_REG_CLASSES];

/* The same information, but as an array of unsigned ints.  We copy from
   these unsigned ints to the table above.  We do this so the tm.h files
   do not have to be aware of the wordsize for machines with <= 64 regs.  */

#define N_REG_INTS  \
  ((FIRST_PSEUDO_REGISTER + (HOST_BITS_PER_INT - 1)) / HOST_BITS_PER_INT)

static unsigned int_reg_class_contents[N_REG_CLASSES][N_REG_INTS] 
  = REG_CLASS_CONTENTS;

/* For each reg class, number of regs it contains.  */

int reg_class_size[N_REG_CLASSES];

/* For each reg class, table listing all the containing classes.  */

enum reg_class reg_class_superclasses[N_REG_CLASSES][N_REG_CLASSES];

/* For each reg class, table listing all the classes contained in it.  */

enum reg_class reg_class_subclasses[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   a largest reg class contained in their union.  */

enum reg_class reg_class_subunion[N_REG_CLASSES][N_REG_CLASSES];

/* For each pair of reg classes,
   the smallest reg class containing their union.  */

enum reg_class reg_class_superunion[N_REG_CLASSES][N_REG_CLASSES];

/* Array containing all of the register names */

char *reg_names[] = REGISTER_NAMES;

/* For each hard register, the widest mode object that it can contain.
   This will be a MODE_INT mode if the register can hold integers.  Otherwise
   it will be a MODE_FLOAT or a MODE_CC mode, whichever is valid for the
   register.  */

enum machine_mode reg_raw_mode[FIRST_PSEUDO_REGISTER];

/* Maximum cost of moving from a register in one class to a register in
   another class.  Based on REGISTER_MOVE_COST.  */

static int move_cost[N_REG_CLASSES][N_REG_CLASSES];

/* Similar, but here we don't have to move if the first index is a subset
   of the second so in that case the cost is zero.  */

static int may_move_cost[N_REG_CLASSES][N_REG_CLASSES];

#ifdef FORBIDDEN_INC_DEC_CLASSES

/* These are the classes that regs which are auto-incremented or decremented
   cannot be put in.  */

static int forbidden_inc_dec_class[N_REG_CLASSES];

/* Indexed by n, is non-zero if (REG n) is used in an auto-inc or auto-dec
   context.  */

static char *in_inc_dec;

#endif /* FORBIDDEN_INC_DEC_CLASSES */

/* Function called only once to initialize the above data on reg usage.
   Once this is done, various switches may override.  */

void
init_reg_sets ()
{
  register int i, j;

  /* First copy the register information from the initial int form into
     the regsets.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      CLEAR_HARD_REG_SET (reg_class_contents[i]);

      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (int_reg_class_contents[i][j / HOST_BITS_PER_INT]
	    & ((unsigned) 1 << (j % HOST_BITS_PER_INT)))
	  SET_HARD_REG_BIT (reg_class_contents[i], j);
    }

  bcopy (initial_fixed_regs, fixed_regs, sizeof fixed_regs);
  bcopy (initial_call_used_regs, call_used_regs, sizeof call_used_regs);
  bzero (global_regs, sizeof global_regs);

  /* Compute number of hard regs in each class.  */

  bzero ((char *) reg_class_size, sizeof reg_class_size);
  for (i = 0; i < N_REG_CLASSES; i++)
    for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
      if (TEST_HARD_REG_BIT (reg_class_contents[i], j))
	reg_class_size[i]++;

  /* Initialize the table of subunions.
     reg_class_subunion[I][J] gets the largest-numbered reg-class
     that is contained in the union of classes I and J.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	{
#ifdef HARD_REG_SET
	  register		/* Declare it register if it's a scalar.  */
#endif
	    HARD_REG_SET c;
	  register int k;

	  COPY_HARD_REG_SET (c, reg_class_contents[i]);
	  IOR_HARD_REG_SET (c, reg_class_contents[j]);
	  for (k = 0; k < N_REG_CLASSES; k++)
	    {
	      GO_IF_HARD_REG_SUBSET (reg_class_contents[k], c,
				     subclass1);
	      continue;

	    subclass1:
	      /* keep the largest subclass */		/* SPEE 900308 */
	      GO_IF_HARD_REG_SUBSET (reg_class_contents[k],
				     reg_class_contents[(int) reg_class_subunion[i][j]],
				     subclass2);
	      reg_class_subunion[i][j] = (enum reg_class) k;
	    subclass2:
	      ;
	    }
	}
    }

  /* Initialize the table of superunions.
     reg_class_superunion[I][J] gets the smallest-numbered reg-class
     containing the union of classes I and J.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	{
#ifdef HARD_REG_SET
	  register		/* Declare it register if it's a scalar.  */
#endif
	    HARD_REG_SET c;
	  register int k;

	  COPY_HARD_REG_SET (c, reg_class_contents[i]);
	  IOR_HARD_REG_SET (c, reg_class_contents[j]);
	  for (k = 0; k < N_REG_CLASSES; k++)
	    GO_IF_HARD_REG_SUBSET (c, reg_class_contents[k], superclass);

	superclass:
	  reg_class_superunion[i][j] = (enum reg_class) k;
	}
    }

  /* Initialize the tables of subclasses and superclasses of each reg class.
     First clear the whole table, then add the elements as they are found.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      for (j = 0; j < N_REG_CLASSES; j++)
	{
	  reg_class_superclasses[i][j] = LIM_REG_CLASSES;
	  reg_class_subclasses[i][j] = LIM_REG_CLASSES;
	}
    }

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      if (i == (int) NO_REGS)
	continue;

      for (j = i + 1; j < N_REG_CLASSES; j++)
	{
	  enum reg_class *p;

	  GO_IF_HARD_REG_SUBSET (reg_class_contents[i], reg_class_contents[j],
				 subclass);
	  continue;
	subclass:
	  /* Reg class I is a subclass of J.
	     Add J to the table of superclasses of I.  */
	  p = &reg_class_superclasses[i][0];
	  while (*p != LIM_REG_CLASSES) p++;
	  *p = (enum reg_class) j;
	  /* Add I to the table of superclasses of J.  */
	  p = &reg_class_subclasses[j][0];
	  while (*p != LIM_REG_CLASSES) p++;
	  *p = (enum reg_class) i;
	}
    }

  /* Initialize the move cost table.  Find every subset of each class
     and take the maximum cost of moving any subset to any other.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    for (j = 0; j < N_REG_CLASSES; j++)
      {
	int cost = i == j ? 2 : REGISTER_MOVE_COST (i, j);
	enum reg_class *p1, *p2;

	for (p2 = &reg_class_subclasses[j][0]; *p2 != LIM_REG_CLASSES; p2++)
	  if (*p2 != i)
	    cost = MAX (cost, REGISTER_MOVE_COST (i, *p2));

	for (p1 = &reg_class_subclasses[i][0]; *p1 != LIM_REG_CLASSES; p1++)
	  {
	    if (*p1 != j)
	      cost = MAX (cost, REGISTER_MOVE_COST (*p1, j));

	    for (p2 = &reg_class_subclasses[j][0];
		 *p2 != LIM_REG_CLASSES; p2++)
	      if (*p1 != *p2)
		cost = MAX (cost, REGISTER_MOVE_COST (*p1, *p2));
	  }

	move_cost[i][j] = cost;

	if (reg_class_subset_p (i, j))
	  cost = 0;

	may_move_cost[i][j] = cost;
      }

  /* Do any additional initialization regsets may need */
  INIT_ONCE_REG_SET ();
}

/* After switches have been processed, which perhaps alter
   `fixed_regs' and `call_used_regs', convert them to HARD_REG_SETs.  */

static void
init_reg_sets_1 ()
{
  register int i;

  /* This macro allows the fixed or call-used registers
     to depend on target flags.  */

#ifdef CONDITIONAL_REGISTER_USAGE
  CONDITIONAL_REGISTER_USAGE;
#endif

  /* Initialize "constant" tables.  */

  CLEAR_HARD_REG_SET (fixed_reg_set);
  CLEAR_HARD_REG_SET (call_used_reg_set);
  CLEAR_HARD_REG_SET (call_fixed_reg_set);

  bcopy (fixed_regs, call_fixed_regs, sizeof call_fixed_regs);

  n_non_fixed_regs = 0;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (fixed_regs[i])
	SET_HARD_REG_BIT (fixed_reg_set, i);
      else
	n_non_fixed_regs++;

      if (call_used_regs[i])
	SET_HARD_REG_BIT (call_used_reg_set, i);
      if (call_fixed_regs[i])
	SET_HARD_REG_BIT (call_fixed_reg_set, i);
      if (CLASS_LIKELY_SPILLED_P (REGNO_REG_CLASS (i)))
	SET_HARD_REG_BIT (losing_caller_save_reg_set, i);
    }
}

/* Compute the table of register modes.
   These values are used to record death information for individual registers
   (as opposed to a multi-register mode).  */

static void
init_reg_modes ()
{
  register int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      reg_raw_mode[i] = choose_hard_reg_mode (i, 1);

      /* If we couldn't find a valid mode, fall back to `word_mode'.
	 ??? We assume `word_mode' has already been initialized.
         ??? One situation in which we need to do this is on the mips where
	 HARD_REGNO_NREGS (fpreg, [SD]Fmode) returns 2.  Ideally we'd like
	 to use DF mode for the even registers and VOIDmode for the odd
	 (for the cpu models where the odd ones are inaccessible).  */
      if (reg_raw_mode[i] == VOIDmode)
	reg_raw_mode[i] = word_mode;
    }
}

/* Finish initializing the register sets and
   initialize the register modes.  */

void
init_regs ()
{
  /* This finishes what was started by init_reg_sets, but couldn't be done
     until after register usage was specified.  */
  if (!output_bytecode)
    init_reg_sets_1 ();

  init_reg_modes ();
}

/* Return a machine mode that is legitimate for hard reg REGNO and large
   enough to save nregs.  If we can't find one, return VOIDmode.  */

enum machine_mode
choose_hard_reg_mode (regno, nregs)
     int regno;
     int nregs;
{
  enum machine_mode found_mode = VOIDmode, mode;

  /* We first look for the largest integer mode that can be validly
     held in REGNO.  If none, we look for the largest floating-point mode.
     If we still didn't find a valid mode, try CCmode.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (HARD_REGNO_NREGS (regno, mode) == nregs
	&& HARD_REGNO_MODE_OK (regno, mode))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (HARD_REGNO_NREGS (regno, mode) == nregs
	&& HARD_REGNO_MODE_OK (regno, mode))
      found_mode = mode;

  if (found_mode != VOIDmode)
    return found_mode;

  if (HARD_REGNO_NREGS (regno, CCmode) == nregs
      && HARD_REGNO_MODE_OK (regno, CCmode))
    return CCmode;

  /* We can't find a mode valid for this register.  */
  return VOIDmode;
}

/* Specify the usage characteristics of the register named NAME.
   It should be a fixed register if FIXED and a
   call-used register if CALL_USED.  */

void
fix_register (name, fixed, call_used)
     char *name;
     int fixed, call_used;
{
  int i;

  if (output_bytecode)
    {
      warning ("request to mark `%s' as %s ignored by bytecode compiler",
	       name, call_used ? "call-used" : "fixed");
      return;
    }

  /* Decode the name and update the primary form of
     the register info.  */

  if ((i = decode_reg_name (name)) >= 0)
    {
      fixed_regs[i] = fixed;
      call_used_regs[i] = call_used;
    }
  else
    {
      warning ("unknown register name: %s", name);
    }
}

/* Mark register number I as global.  */

void
globalize_reg (i)
     int i;
{
  if (global_regs[i])
    {
      warning ("register used for two global register variables");
      return;
    }

  if (call_used_regs[i] && ! fixed_regs[i])
    warning ("call-clobbered register used for global register variable");

  global_regs[i] = 1;

  /* If already fixed, nothing else to do.  */
  if (fixed_regs[i])
    return;

  fixed_regs[i] = call_used_regs[i] = call_fixed_regs[i] = 1;
  n_non_fixed_regs--;

  SET_HARD_REG_BIT (fixed_reg_set, i);
  SET_HARD_REG_BIT (call_used_reg_set, i);
  SET_HARD_REG_BIT (call_fixed_reg_set, i);
}

/* Now the data and code for the `regclass' pass, which happens
   just before local-alloc.  */

/* The `costs' struct records the cost of using a hard register of each class
   and of using memory for each pseudo.  We use this data to set up
   register class preferences.  */

struct costs
{
  int cost[N_REG_CLASSES];
  int mem_cost;
};

/* Record the cost of each class for each pseudo.  */

static struct costs *costs;

/* Record the same data by operand number, accumulated for each alternative
   in an insn.  The contribution to a pseudo is that of the minimum-cost
   alternative.  */

static struct costs op_costs[MAX_RECOG_OPERANDS];

/* (enum reg_class) prefclass[R] is the preferred class for pseudo number R.
   This is available after `regclass' is run.  */

static char *prefclass;

/* altclass[R] is a register class that we should use for allocating
   pseudo number R if no register in the preferred class is available.
   If no register in this class is available, memory is preferred.

   It might appear to be more general to have a bitmask of classes here,
   but since it is recommended that there be a class corresponding to the
   union of most major pair of classes, that generality is not required. 

   This is available after `regclass' is run.  */

static char *altclass;

/* Record the depth of loops that we are in.  */

static int loop_depth;

/* Account for the fact that insns within a loop are executed very commonly,
   but don't keep doing this as loops go too deep.  */

static int loop_cost;

static void record_reg_classes	PROTO((int, int, rtx *, enum machine_mode *,
				       char **, rtx));
static int copy_cost		PROTO((rtx, enum machine_mode, 
				       enum reg_class, int));
static void record_address_regs	PROTO((rtx, enum reg_class, int));
static auto_inc_dec_reg_p	PROTO((rtx, enum machine_mode));
static void reg_scan_mark_refs	PROTO((rtx, rtx, int));

/* Return the reg_class in which pseudo reg number REGNO is best allocated.
   This function is sometimes called before the info has been computed.
   When that happens, just return GENERAL_REGS, which is innocuous.  */

enum reg_class
reg_preferred_class (regno)
     int regno;
{
  if (prefclass == 0)
    return GENERAL_REGS;
  return (enum reg_class) prefclass[regno];
}

enum reg_class
reg_alternate_class (regno)
{
  if (prefclass == 0)
    return ALL_REGS;

  return (enum reg_class) altclass[regno];
}

/* This prevents dump_flow_info from losing if called
   before regclass is run.  */

void
regclass_init ()
{
  prefclass = 0;
}

/* This is a pass of the compiler that scans all instructions
   and calculates the preferred class for each pseudo-register.
   This information can be accessed later by calling `reg_preferred_class'.
   This pass comes just before local register allocation.  */

void
regclass (f, nregs)
     rtx f;
     int nregs;
{
#ifdef REGISTER_CONSTRAINTS
  register rtx insn;
  register int i, j;
  struct costs init_cost;
  rtx set;
  int pass;

  init_recog ();

  costs = (struct costs *) alloca (nregs * sizeof (struct costs));

#ifdef FORBIDDEN_INC_DEC_CLASSES

  in_inc_dec = (char *) alloca (nregs);

  /* Initialize information about which register classes can be used for
     pseudos that are auto-incremented or auto-decremented.  It would
     seem better to put this in init_reg_sets, but we need to be able
     to allocate rtx, which we can't do that early.  */

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      rtx r = gen_rtx (REG, VOIDmode, 0);
      enum machine_mode m;

      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (reg_class_contents[i], j))
	  {
	    REGNO (r) = j;

	    for (m = VOIDmode; (int) m < (int) MAX_MACHINE_MODE;
		 m = (enum machine_mode) ((int) m + 1))
	      if (HARD_REGNO_MODE_OK (j, m))
		{
		  PUT_MODE (r, m);

		  /* If a register is not directly suitable for an
		     auto-increment or decrement addressing mode and
		     requires secondary reloads, disallow its class from
		     being used in such addresses.  */

		  if ((0
#ifdef SECONDARY_RELOAD_CLASS
		       || (SECONDARY_RELOAD_CLASS (BASE_REG_CLASS, m, r)
			   != NO_REGS)
#else
#ifdef SECONDARY_INPUT_RELOAD_CLASS
		       || (SECONDARY_INPUT_RELOAD_CLASS (BASE_REG_CLASS, m, r)
			   != NO_REGS)
#endif
#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
		       || (SECONDARY_OUTPUT_RELOAD_CLASS (BASE_REG_CLASS, m, r)
			   != NO_REGS)
#endif
#endif
		       )
		      && ! auto_inc_dec_reg_p (r, m))
		    forbidden_inc_dec_class[i] = 1;
		}
	  }
    }
#endif /* FORBIDDEN_INC_DEC_CLASSES */

  init_cost.mem_cost = 10000;
  for (i = 0; i < N_REG_CLASSES; i++)
    init_cost.cost[i] = 10000;

  /* Normally we scan the insns once and determine the best class to use for
     each register.  However, if -fexpensive_optimizations are on, we do so
     twice, the second time using the tentative best classes to guide the
     selection.  */

  for (pass = 0; pass <= flag_expensive_optimizations; pass++)
    {
      /* Zero out our accumulation of the cost of each class for each reg.  */

      bzero ((char *) costs, nregs * sizeof (struct costs));

#ifdef FORBIDDEN_INC_DEC_CLASSES
      bzero (in_inc_dec, nregs);
#endif

      loop_depth = 0, loop_cost = 1;

      /* Scan the instructions and record each time it would
	 save code to put a certain register in a certain class.  */

      for (insn = f; insn; insn = NEXT_INSN (insn))
	{
	  char *constraints[MAX_RECOG_OPERANDS];
	  enum machine_mode modes[MAX_RECOG_OPERANDS];
	  int nalternatives;
	  int noperands;

	  /* Show that an insn inside a loop is likely to be executed three
	     times more than insns outside a loop.  This is much more aggressive
	     than the assumptions made elsewhere and is being tried as an
	     experiment.  */

	  if (GET_CODE (insn) == NOTE
	      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    loop_depth++, loop_cost = 1 << (2 * MIN (loop_depth, 5));
	  else if (GET_CODE (insn) == NOTE
		   && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    loop_depth--, loop_cost = 1 << (2 * MIN (loop_depth, 5));

	  else if ((GET_CODE (insn) == INSN
		    && GET_CODE (PATTERN (insn)) != USE
		    && GET_CODE (PATTERN (insn)) != CLOBBER
		    && GET_CODE (PATTERN (insn)) != ASM_INPUT)
		   || (GET_CODE (insn) == JUMP_INSN
		       && GET_CODE (PATTERN (insn)) != ADDR_VEC
		       && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
		   || GET_CODE (insn) == CALL_INSN)
	    {
	      if (GET_CODE (insn) == INSN
		  && (noperands = asm_noperands (PATTERN (insn))) >= 0)
		{
		  decode_asm_operands (PATTERN (insn), recog_operand, NULL_PTR,
				       constraints, modes);
		  nalternatives = (noperands == 0 ? 0
				   : n_occurrences (',', constraints[0]) + 1);
		}
	      else
		{
		  int insn_code_number = recog_memoized (insn);
		  rtx note;

		  set = single_set (insn);
		  insn_extract (insn);

		  nalternatives = insn_n_alternatives[insn_code_number];
		  noperands = insn_n_operands[insn_code_number];

		  /* If this insn loads a parameter from its stack slot, then
		     it represents a savings, rather than a cost, if the
		     parameter is stored in memory.  Record this fact.  */

		  if (set != 0 && GET_CODE (SET_DEST (set)) == REG
		      && GET_CODE (SET_SRC (set)) == MEM
		      && (note = find_reg_note (insn, REG_EQUIV,
						NULL_RTX)) != 0
		      && GET_CODE (XEXP (note, 0)) == MEM)
		    {
		      costs[REGNO (SET_DEST (set))].mem_cost
			-= (MEMORY_MOVE_COST (GET_MODE (SET_DEST (set)))
			    * loop_cost);
		      record_address_regs (XEXP (SET_SRC (set), 0),
					   BASE_REG_CLASS, loop_cost * 2);
		      continue;
		    }
	      
		  /* Improve handling of two-address insns such as
		     (set X (ashift CONST Y)) where CONST must be made to
		     match X. Change it into two insns: (set X CONST)
		     (set X (ashift X Y)).  If we left this for reloading, it
		     would probably get three insns because X and Y might go
		     in the same place. This prevents X and Y from receiving
		     the same hard reg.

		     We can only do this if the modes of operands 0 and 1
		     (which might not be the same) are tieable and we only need
		     do this during our first pass.  */

		  if (pass == 0 && optimize
		      && noperands >= 3
		      && insn_operand_constraint[insn_code_number][1][0] == '0'
		      && insn_operand_constraint[insn_code_number][1][1] == 0
		      && CONSTANT_P (recog_operand[1])
		      && ! rtx_equal_p (recog_operand[0], recog_operand[1])
		      && ! rtx_equal_p (recog_operand[0], recog_operand[2])
		      && GET_CODE (recog_operand[0]) == REG
		      && MODES_TIEABLE_P (GET_MODE (recog_operand[0]),
					  insn_operand_mode[insn_code_number][1]))
		    {
		      rtx previnsn = prev_real_insn (insn);
		      rtx dest
			= gen_lowpart (insn_operand_mode[insn_code_number][1],
				       recog_operand[0]);
		      rtx newinsn
			= emit_insn_before (gen_move_insn (dest,
							   recog_operand[1]),
					    insn);

		      /* If this insn was the start of a basic block,
			 include the new insn in that block.
			 We need not check for code_label here;
			 while a basic block can start with a code_label,
			 INSN could not be at the beginning of that block.  */
		      if (previnsn == 0 || GET_CODE (previnsn) == JUMP_INSN)
			{
			  int b;
			  for (b = 0; b < n_basic_blocks; b++)
			    if (insn == basic_block_head[b])
			      basic_block_head[b] = newinsn;
			}

		      /* This makes one more setting of new insns's dest.  */
		      REG_N_SETS (REGNO (recog_operand[0]))++;

		      *recog_operand_loc[1] = recog_operand[0];
		      for (i = insn_n_dups[insn_code_number] - 1; i >= 0; i--)
			if (recog_dup_num[i] == 1)
			  *recog_dup_loc[i] = recog_operand[0];

		      insn = PREV_INSN (newinsn);
		      continue;
		    }

		  for (i = 0; i < noperands; i++)
		    {
		      constraints[i]
			= insn_operand_constraint[insn_code_number][i];
		      modes[i] = insn_operand_mode[insn_code_number][i];
		    }
		}

	      /* If we get here, we are set up to record the costs of all the
		 operands for this insn.  Start by initializing the costs.
		 Then handle any address registers.  Finally record the desired
		 classes for any pseudos, doing it twice if some pair of
		 operands are commutative.  */
	     
	      for (i = 0; i < noperands; i++)
		{
		  op_costs[i] = init_cost;

		  if (GET_CODE (recog_operand[i]) == SUBREG)
		    recog_operand[i] = SUBREG_REG (recog_operand[i]);

		  if (GET_CODE (recog_operand[i]) == MEM)
		    record_address_regs (XEXP (recog_operand[i], 0),
					 BASE_REG_CLASS, loop_cost * 2);
		  else if (constraints[i][0] == 'p')
		    record_address_regs (recog_operand[i],
					 BASE_REG_CLASS, loop_cost * 2);
		}

	      /* Check for commutative in a separate loop so everything will
		 have been initialized.  We must do this even if one operand
		 is a constant--see addsi3 in m68k.md.  */
	      
	      for (i = 0; i < noperands - 1; i++)
		if (constraints[i][0] == '%')
		  {
		    char *xconstraints[MAX_RECOG_OPERANDS];
		    int j;

		    /* Handle commutative operands by swapping the constraints.
		       We assume the modes are the same.  */

		    for (j = 0; j < noperands; j++)
		      xconstraints[j] = constraints[j];

		    xconstraints[i] = constraints[i+1];
		    xconstraints[i+1] = constraints[i];
		    record_reg_classes (nalternatives, noperands,
					recog_operand, modes, xconstraints,
					insn);
		  }

	      record_reg_classes (nalternatives, noperands, recog_operand,
				  modes, constraints, insn);

	      /* Now add the cost for each operand to the total costs for
		 its register.  */

	      for (i = 0; i < noperands; i++)
		if (GET_CODE (recog_operand[i]) == REG
		    && REGNO (recog_operand[i]) >= FIRST_PSEUDO_REGISTER)
		  {
		    int regno = REGNO (recog_operand[i]);
		    struct costs *p = &costs[regno], *q = &op_costs[i];

		    p->mem_cost += q->mem_cost * loop_cost;
		    for (j = 0; j < N_REG_CLASSES; j++)
		      p->cost[j] += q->cost[j] * loop_cost;
		  }
	    }
	}

      /* Now for each register look at how desirable each class is
	 and find which class is preferred.  Store that in
	 `prefclass[REGNO]'.  Record in `altclass[REGNO]' the largest register
	 class any of whose registers is better than memory.  */
    
      if (pass == 0)
	{
	  prefclass = (char *) oballoc (nregs);
	  altclass = (char *) oballoc (nregs);
	}

      for (i = FIRST_PSEUDO_REGISTER; i < nregs; i++)
	{
	  register int best_cost = (1 << (HOST_BITS_PER_INT - 2)) - 1;
	  enum reg_class best = ALL_REGS, alt = NO_REGS;
	  /* This is an enum reg_class, but we call it an int
	     to save lots of casts.  */
	  register int class;
	  register struct costs *p = &costs[i];

	  for (class = (int) ALL_REGS - 1; class > 0; class--)
	    {
	      /* Ignore classes that are too small for this operand or
		 invalid for a operand that was auto-incremented.  */
	      if (CLASS_MAX_NREGS (class, PSEUDO_REGNO_MODE (i))
		  > reg_class_size[class]
#ifdef FORBIDDEN_INC_DEC_CLASSES
		  || (in_inc_dec[i] && forbidden_inc_dec_class[class])
#endif
		  )
		;
	      else if (p->cost[class] < best_cost)
		{
		  best_cost = p->cost[class];
		  best = (enum reg_class) class;
		}
	      else if (p->cost[class] == best_cost)
		best = reg_class_subunion[(int)best][class];
	    }

	  /* Record the alternate register class; i.e., a class for which
	     every register in it is better than using memory.  If adding a
	     class would make a smaller class (i.e., no union of just those
	     classes exists), skip that class.  The major unions of classes
	     should be provided as a register class.  Don't do this if we
	     will be doing it again later.  */

	  if (pass == 1 || ! flag_expensive_optimizations)
	    for (class = 0; class < N_REG_CLASSES; class++)
	      if (p->cost[class] < p->mem_cost
		  && (reg_class_size[(int) reg_class_subunion[(int) alt][class]]
		      > reg_class_size[(int) alt])
#ifdef FORBIDDEN_INC_DEC_CLASSES
		  && ! (in_inc_dec[i] && forbidden_inc_dec_class[class])
#endif
		  )
		alt = reg_class_subunion[(int) alt][class];
	  
	  /* If we don't add any classes, nothing to try.  */
	  if (alt == best)
	    alt = NO_REGS;

	  /* We cast to (int) because (char) hits bugs in some compilers.  */
	  prefclass[i] = (int) best;
	  altclass[i] = (int) alt;
	}
    }
#endif /* REGISTER_CONSTRAINTS */
}

#ifdef REGISTER_CONSTRAINTS

/* Record the cost of using memory or registers of various classes for
   the operands in INSN.

   N_ALTS is the number of alternatives.

   N_OPS is the number of operands.

   OPS is an array of the operands.

   MODES are the modes of the operands, in case any are VOIDmode.

   CONSTRAINTS are the constraints to use for the operands.  This array
   is modified by this procedure.

   This procedure works alternative by alternative.  For each alternative
   we assume that we will be able to allocate all pseudos to their ideal
   register class and calculate the cost of using that alternative.  Then
   we compute for each operand that is a pseudo-register, the cost of 
   having the pseudo allocated to each register class and using it in that
   alternative.  To this cost is added the cost of the alternative.

   The cost of each class for this insn is its lowest cost among all the
   alternatives.  */

static void
record_reg_classes (n_alts, n_ops, ops, modes, constraints, insn)
     int n_alts;
     int n_ops;
     rtx *ops;
     enum machine_mode *modes;
     char **constraints;
     rtx insn;
{
  int alt;
  enum op_type {OP_READ, OP_WRITE, OP_READ_WRITE} op_types[MAX_RECOG_OPERANDS];
  int i, j;
  rtx set;

  /* By default, each operand is an input operand.  */

  for (i = 0; i < n_ops; i++)
    op_types[i] = OP_READ;

  /* Process each alternative, each time minimizing an operand's cost with
     the cost for each operand in that alternative.  */

  for (alt = 0; alt < n_alts; alt++)
    {
      struct costs this_op_costs[MAX_RECOG_OPERANDS];
      int alt_fail = 0;
      int alt_cost = 0;
      enum reg_class classes[MAX_RECOG_OPERANDS];
      int class;

      for (i = 0; i < n_ops; i++)
	{
	  char *p = constraints[i];
	  rtx op = ops[i];
	  enum machine_mode mode = modes[i];
	  int allows_mem = 0;
	  int win = 0;
	  char c;

	  /* If this operand has no constraints at all, we can conclude 
	     nothing about it since anything is valid.  */

	  if (*p == 0)
	    {
	      if (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER)
		bzero ((char *) &this_op_costs[i], sizeof this_op_costs[i]);

	      continue;
	    }

	  if (*p == '%')
	    p++;

	  /* If this alternative is only relevant when this operand
	     matches a previous operand, we do different things depending
	     on whether this operand is a pseudo-reg or not.  */

	  if (p[0] >= '0' && p[0] <= '0' + i && (p[1] == ',' || p[1] == 0))
	    {
	      j = p[0] - '0';
	      classes[i] = classes[j];

	      if (GET_CODE (op) != REG || REGNO (op) < FIRST_PSEUDO_REGISTER)
		{
		  /* If this matches the other operand, we have no added
		     cost and we win.  */
		  if (rtx_equal_p (ops[j], op))
		    win = 1;

		  /* If we can put the other operand into a register, add to
		     the cost of this alternative the cost to copy this
		     operand to the register used for the other operand.  */

		  else if (classes[j] != NO_REGS)
		    alt_cost += copy_cost (op, mode, classes[j], 1), win = 1;
		}
	      else if (GET_CODE (ops[j]) != REG
		       || REGNO (ops[j]) < FIRST_PSEUDO_REGISTER)
		{
		  /* This op is a pseudo but the one it matches is not.  */
		  
		  /* If we can't put the other operand into a register, this
		     alternative can't be used.  */

		  if (classes[j] == NO_REGS)
		    alt_fail = 1;

		  /* Otherwise, add to the cost of this alternative the cost
		     to copy the other operand to the register used for this
		     operand.  */

		  else
		    alt_cost += copy_cost (ops[j], mode, classes[j], 1);
		}
	      else
		{
		  /* The costs of this operand are the same as that of the
		     other operand.  However, if we cannot tie them, this
		     alternative needs to do a copy, which is one
		     instruction.  */

		  this_op_costs[i] = this_op_costs[j];
		  if (REGNO (ops[i]) != REGNO (ops[j])
		      && ! find_reg_note (insn, REG_DEAD, op))
		    alt_cost += 2;

		  /* This is in place of ordinary cost computation
		     for this operand, so skip to the end of the
		     alternative (should be just one character).  */
		  while (*p && *p++ != ',')
		    ;

		  constraints[i] = p;
		  continue;
		}
	    }

	  /* Scan all the constraint letters.  See if the operand matches
	     any of the constraints.  Collect the valid register classes
	     and see if this operand accepts memory.  */

	  classes[i] = NO_REGS;
	  while (*p && (c = *p++) != ',')
	    switch (c)
	      {
	      case '=':
		op_types[i] = OP_WRITE;
		break;

	      case '+':
		op_types[i] = OP_READ_WRITE;
		break;

	      case '*':
		/* Ignore the next letter for this pass.  */
		p++;
		break;

	      case '%':
	      case '?':  case '!':  case '#':
	      case '&':
	      case '0':  case '1':  case '2':  case '3':  case '4':
	      case 'p':
		break;

	      case 'm':  case 'o':  case 'V':
		/* It doesn't seem worth distinguishing between offsettable
		   and non-offsettable addresses here.  */
		allows_mem = 1;
		if (GET_CODE (op) == MEM)
		  win = 1;
		break;

	      case '<':
		if (GET_CODE (op) == MEM
		    && (GET_CODE (XEXP (op, 0)) == PRE_DEC
			|| GET_CODE (XEXP (op, 0)) == POST_DEC))
		  win = 1;
		break;

	      case '>':
		if (GET_CODE (op) == MEM
		    && (GET_CODE (XEXP (op, 0)) == PRE_INC
			|| GET_CODE (XEXP (op, 0)) == POST_INC))
		  win = 1;
		break;

	      case 'E':
#ifndef REAL_ARITHMETIC
		/* Match any floating double constant, but only if
		   we can examine the bits of it reliably.  */
		if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
		     || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
		    && GET_MODE (op) != VOIDmode && ! flag_pretend_float)
		  break;
#endif
		if (GET_CODE (op) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'F':
		if (GET_CODE (op) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'G':
	      case 'H':
		if (GET_CODE (op) == CONST_DOUBLE
		    && CONST_DOUBLE_OK_FOR_LETTER_P (op, c))
		  win = 1;
		break;

	      case 's':
		if (GET_CODE (op) == CONST_INT
		    || (GET_CODE (op) == CONST_DOUBLE
			&& GET_MODE (op) == VOIDmode))
		  break;
	      case 'i':
		if (CONSTANT_P (op)
#ifdef LEGITIMATE_PIC_OPERAND_P
		    && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
		    )
		  win = 1;
		break;

	      case 'n':
		if (GET_CODE (op) == CONST_INT
		    || (GET_CODE (op) == CONST_DOUBLE
			&& GET_MODE (op) == VOIDmode))
		  win = 1;
		break;

	      case 'I':
	      case 'J':
	      case 'K':
	      case 'L':
	      case 'M':
	      case 'N':
	      case 'O':
	      case 'P':
		if (GET_CODE (op) == CONST_INT
		    && CONST_OK_FOR_LETTER_P (INTVAL (op), c))
		  win = 1;
		break;

	      case 'X':
		win = 1;
		break;

#ifdef EXTRA_CONSTRAINT
              case 'Q':
              case 'R':
              case 'S':
              case 'T':
              case 'U':
		if (EXTRA_CONSTRAINT (op, c))
		  win = 1;
		break;
#endif

	      case 'g':
		if (GET_CODE (op) == MEM
		    || (CONSTANT_P (op)
#ifdef LEGITIMATE_PIC_OPERAND_P
			&& (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))
#endif
			))
		  win = 1;
		allows_mem = 1;
	      case 'r':
		classes[i]
		  = reg_class_subunion[(int) classes[i]][(int) GENERAL_REGS];
		break;

	      default:
		classes[i]
		  = reg_class_subunion[(int) classes[i]]
		    [(int) REG_CLASS_FROM_LETTER (c)];
	      }

	  constraints[i] = p;

	  /* How we account for this operand now depends on whether it is  a
	     pseudo register or not.  If it is, we first check if any
	     register classes are valid.  If not, we ignore this alternative,
	     since we want to assume that all pseudos get allocated for
	     register preferencing.  If some register class is valid, compute
	     the costs of moving the pseudo into that class.  */

	  if (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER)
	    {
	      if (classes[i] == NO_REGS)
		alt_fail = 1;
	      else
		{
		  struct costs *pp = &this_op_costs[i];

		  for (class = 0; class < N_REG_CLASSES; class++)
		    pp->cost[class] = may_move_cost[class][(int) classes[i]];

		  /* If the alternative actually allows memory, make things
		     a bit cheaper since we won't need an extra insn to
		     load it.  */

		  pp->mem_cost = MEMORY_MOVE_COST (mode) - allows_mem;

		  /* If we have assigned a class to this register in our
		     first pass, add a cost to this alternative corresponding
		     to what we would add if this register were not in the
		     appropriate class.  */

		  if (prefclass)
		    alt_cost
		      += may_move_cost[prefclass[REGNO (op)]][(int) classes[i]];
		}
	    }

	  /* Otherwise, if this alternative wins, either because we
	     have already determined that or if we have a hard register of
	     the proper class, there is no cost for this alternative.  */

	  else if (win
		   || (GET_CODE (op) == REG
		       && reg_fits_class_p (op, classes[i], 0, GET_MODE (op))))
	    ;

	  /* If registers are valid, the cost of this alternative includes
	     copying the object to and/or from a register.  */

	  else if (classes[i] != NO_REGS)
	    {
	      if (op_types[i] != OP_WRITE)
		alt_cost += copy_cost (op, mode, classes[i], 1);

	      if (op_types[i] != OP_READ)
		alt_cost += copy_cost (op, mode, classes[i], 0);
	    }

	  /* The only other way this alternative can be used is if this is a
	     constant that could be placed into memory.  */

	  else if (CONSTANT_P (op) && allows_mem)
	    alt_cost += MEMORY_MOVE_COST (mode);
	  else
	    alt_fail = 1;
	}

      if (alt_fail)
	continue;

      /* Finally, update the costs with the information we've calculated
	 about this alternative.  */

      for (i = 0; i < n_ops; i++)
	if (GET_CODE (ops[i]) == REG
	    && REGNO (ops[i]) >= FIRST_PSEUDO_REGISTER)
	  {
	    struct costs *pp = &op_costs[i], *qq = &this_op_costs[i];
	    int scale = 1 + (op_types[i] == OP_READ_WRITE);

	    pp->mem_cost = MIN (pp->mem_cost,
				(qq->mem_cost + alt_cost) * scale);

	    for (class = 0; class < N_REG_CLASSES; class++)
	      pp->cost[class] = MIN (pp->cost[class],
				     (qq->cost[class] + alt_cost) * scale);
	  }
    }

  /* If this insn is a single set copying operand 1 to operand 0
     and one is a pseudo with the other a hard reg that is in its
     own register class, set the cost of that register class to -1.  */

  if ((set = single_set (insn)) != 0
      && ops[0] == SET_DEST (set) && ops[1] == SET_SRC (set)
      && GET_CODE (ops[0]) == REG && GET_CODE (ops[1]) == REG)
    for (i = 0; i <= 1; i++)
      if (REGNO (ops[i]) >= FIRST_PSEUDO_REGISTER)
	{
	  int regno = REGNO (ops[!i]);
	  enum machine_mode mode = GET_MODE (ops[!i]);
	  int class;
	  int nr;

	  if (regno >= FIRST_PSEUDO_REGISTER && prefclass != 0
	      && (reg_class_size[prefclass[regno]]
		  == CLASS_MAX_NREGS (prefclass[regno], mode)))
	    op_costs[i].cost[prefclass[regno]] = -1;
	  else if (regno < FIRST_PSEUDO_REGISTER)
	    for (class = 0; class < N_REG_CLASSES; class++)
	      if (TEST_HARD_REG_BIT (reg_class_contents[class], regno)
		  && reg_class_size[class] == CLASS_MAX_NREGS (class, mode))
		{
		  if (reg_class_size[class] == 1)
		    op_costs[i].cost[class] = -1;
		  else
		    {
		      for (nr = 0; nr < HARD_REGNO_NREGS(regno, mode); nr++)
			{
			  if (!TEST_HARD_REG_BIT (reg_class_contents[class], regno + nr))
			    break;
			}

		      if (nr == HARD_REGNO_NREGS(regno,mode))
			op_costs[i].cost[class] = -1;
		    }
		}
	}
}

/* Compute the cost of loading X into (if TO_P is non-zero) or from (if
   TO_P is zero) a register of class CLASS in mode MODE.

   X must not be a pseudo.  */

static int
copy_cost (x, mode, class, to_p)
     rtx x;
     enum machine_mode mode;
     enum reg_class class;
     int to_p;
{
  enum reg_class secondary_class = NO_REGS;

  /* If X is a SCRATCH, there is actually nothing to move since we are
     assuming optimal allocation.  */

  if (GET_CODE (x) == SCRATCH)
    return 0;

  /* Get the class we will actually use for a reload.  */
  class = PREFERRED_RELOAD_CLASS (x, class);

#ifdef HAVE_SECONDARY_RELOADS
  /* If we need a secondary reload (we assume here that we are using 
     the secondary reload as an intermediate, not a scratch register), the
     cost is that to load the input into the intermediate register, then
     to copy them.  We use a special value of TO_P to avoid recursion.  */

#ifdef SECONDARY_INPUT_RELOAD_CLASS
  if (to_p == 1)
    secondary_class = SECONDARY_INPUT_RELOAD_CLASS (class, mode, x);
#endif

#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
  if (! to_p)
    secondary_class = SECONDARY_OUTPUT_RELOAD_CLASS (class, mode, x);
#endif

  if (secondary_class != NO_REGS)
    return (move_cost[(int) secondary_class][(int) class]
	    + copy_cost (x, mode, secondary_class, 2));
#endif  /* HAVE_SECONDARY_RELOADS */

  /* For memory, use the memory move cost, for (hard) registers, use the
     cost to move between the register classes, and use 2 for everything
     else (constants).  */

  if (GET_CODE (x) == MEM || class == NO_REGS)
    return MEMORY_MOVE_COST (mode);

  else if (GET_CODE (x) == REG)
    return move_cost[(int) REGNO_REG_CLASS (REGNO (x))][(int) class];

  else
    /* If this is a constant, we may eventually want to call rtx_cost here.  */
    return 2;
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a memory address, X.

   CLASS is the class that the register needs to be in and is either
   BASE_REG_CLASS or INDEX_REG_CLASS.

   SCALE is twice the amount to multiply the cost by (it is twice so we
   can represent half-cost adjustments).  */

static void
record_address_regs (x, class, scale)
     rtx x;
     enum reg_class class;
     int scale;
{
  register enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CC0:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
      return;

    case PLUS:
      /* When we have an address that is a sum,
	 we must determine whether registers are "base" or "index" regs.
	 If there is a sum of two registers, we must choose one to be
	 the "base".  Luckily, we can use the REGNO_POINTER_FLAG
	 to make a good choice most of the time.  We only need to do this
	 on machines that can have two registers in an address and where
	 the base and index register classes are different.

	 ??? This code used to set REGNO_POINTER_FLAG in some cases, but
	 that seems bogus since it should only be set when we are sure
	 the register is being used as a pointer.  */

      {
	rtx arg0 = XEXP (x, 0);
	rtx arg1 = XEXP (x, 1);
	register enum rtx_code code0 = GET_CODE (arg0);
	register enum rtx_code code1 = GET_CODE (arg1);

	/* Look inside subregs.  */
	if (code0 == SUBREG)
	  arg0 = SUBREG_REG (arg0), code0 = GET_CODE (arg0);
	if (code1 == SUBREG)
	  arg1 = SUBREG_REG (arg1), code1 = GET_CODE (arg1);

	/* If this machine only allows one register per address, it must
	   be in the first operand.  */

	if (MAX_REGS_PER_ADDRESS == 1)
	  record_address_regs (arg0, class, scale);

	/* If index and base registers are the same on this machine, just
	   record registers in any non-constant operands.  We assume here,
	   as well as in the tests below, that all addresses are in 
	   canonical form.  */

	else if (INDEX_REG_CLASS == BASE_REG_CLASS)
	  {
	    record_address_regs (arg0, class, scale);
	    if (! CONSTANT_P (arg1))
	      record_address_regs (arg1, class, scale);
	  }

	/* If the second operand is a constant integer, it doesn't change
	   what class the first operand must be.  */

	else if (code1 == CONST_INT || code1 == CONST_DOUBLE)
	  record_address_regs (arg0, class, scale);

	/* If the second operand is a symbolic constant, the first operand
	   must be an index register.  */

	else if (code1 == SYMBOL_REF || code1 == CONST || code1 == LABEL_REF)
	  record_address_regs (arg0, INDEX_REG_CLASS, scale);

	/* If both operands are registers but one is already a hard register
	   of index or base class, give the other the class that the hard
	   register is not.  */

	else if (code0 == REG && code1 == REG
		 && REGNO (arg0) < FIRST_PSEUDO_REGISTER
		 && (REG_OK_FOR_BASE_P (arg0) || REG_OK_FOR_INDEX_P (arg0)))
	  record_address_regs (arg1,
			       REG_OK_FOR_BASE_P (arg0)
			       ? INDEX_REG_CLASS : BASE_REG_CLASS,
			       scale);
	else if (code0 == REG && code1 == REG
		 && REGNO (arg1) < FIRST_PSEUDO_REGISTER
		 && (REG_OK_FOR_BASE_P (arg1) || REG_OK_FOR_INDEX_P (arg1)))
	  record_address_regs (arg0,
			       REG_OK_FOR_BASE_P (arg1)
			       ? INDEX_REG_CLASS : BASE_REG_CLASS,
			       scale);

	/* If one operand is known to be a pointer, it must be the base
	   with the other operand the index.  Likewise if the other operand
	   is a MULT.  */

	else if ((code0 == REG && REGNO_POINTER_FLAG (REGNO (arg0)))
		 || code1 == MULT)
	  {
	    record_address_regs (arg0, BASE_REG_CLASS, scale);
	    record_address_regs (arg1, INDEX_REG_CLASS, scale);
	  }
	else if ((code1 == REG && REGNO_POINTER_FLAG (REGNO (arg1)))
		 || code0 == MULT)
	  {
	    record_address_regs (arg0, INDEX_REG_CLASS, scale);
	    record_address_regs (arg1, BASE_REG_CLASS, scale);
	  }

	/* Otherwise, count equal chances that each might be a base
	   or index register.  This case should be rare.  */

	else
	  {
	    record_address_regs (arg0, BASE_REG_CLASS, scale / 2);
	    record_address_regs (arg0, INDEX_REG_CLASS, scale / 2);
	    record_address_regs (arg1, BASE_REG_CLASS, scale / 2);
	    record_address_regs (arg1, INDEX_REG_CLASS, scale / 2);
	  }
      }
      break;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
      /* Double the importance of a pseudo register that is incremented
	 or decremented, since it would take two extra insns
	 if it ends up in the wrong place.  If the operand is a pseudo,
	 show it is being used in an INC_DEC context.  */

#ifdef FORBIDDEN_INC_DEC_CLASSES
      if (GET_CODE (XEXP (x, 0)) == REG
	  && REGNO (XEXP (x, 0)) >= FIRST_PSEUDO_REGISTER)
	in_inc_dec[REGNO (XEXP (x, 0))] = 1;
#endif

      record_address_regs (XEXP (x, 0), class, 2 * scale);
      break;

    case REG:
      {
	register struct costs *pp = &costs[REGNO (x)];
	register int i;

	pp->mem_cost += (MEMORY_MOVE_COST (Pmode) * scale) / 2;

	for (i = 0; i < N_REG_CLASSES; i++)
	  pp->cost[i] += (may_move_cost[i][(int) class] * scale) / 2;
      }
      break;

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  if (fmt[i] == 'e')
	    record_address_regs (XEXP (x, i), class, scale);
      }
    }
}

#ifdef FORBIDDEN_INC_DEC_CLASSES

/* Return 1 if REG is valid as an auto-increment memory reference
   to an object of MODE.  */

static 
auto_inc_dec_reg_p (reg, mode)
     rtx reg;
     enum machine_mode mode;
{
#ifdef HAVE_POST_INCREMENT
  if (memory_address_p (mode, gen_rtx (POST_INC, Pmode, reg)))
    return 1;
#endif

#ifdef HAVE_POST_DECREMENT
  if (memory_address_p (mode, gen_rtx (POST_DEC, Pmode, reg)))
    return 1;
#endif

#ifdef HAVE_PRE_INCREMENT
  if (memory_address_p (mode, gen_rtx (PRE_INC, Pmode, reg)))
    return 1;
#endif

#ifdef HAVE_PRE_DECREMENT
  if (memory_address_p (mode, gen_rtx (PRE_DEC, Pmode, reg)))
    return 1;
#endif

  return 0;
}
#endif

#endif /* REGISTER_CONSTRAINTS */

/* Allocate enough space to hold NUM_REGS registers for the tables used for
   reg_scan and flow_analysis that are indexed by the register number.  If
   NEW_P is non zero, initialize all of the registers, otherwise only
   initialize the new registers allocated.  The same table is kept from
   function to function, only reallocating it when we need more room.  If
   RENUMBER_P is non zero, allocate the reg_renumber array also.  */

void
allocate_reg_info (num_regs, new_p, renumber_p)
     int num_regs;
     int new_p;
     int renumber_p;
{
  static int regno_allocated = 0;
  static int regno_max = 0;
  static short *renumber = (short *)0;
  int i;
  int size_info;
  int size_renumber;
  int min = (new_p) ? 0 : regno_max;

  /* If this message come up, and you want to fix it, then all of the tables
     like reg_renumber, etc. that use short will have to be found and lengthed
     to int or HOST_WIDE_INT.  */

  /* Free up all storage allocated */
  if (num_regs < 0)
    {
      if (reg_n_info)
	{
	  free ((char *)reg_n_info);
	  free ((char *)renumber);
	  reg_n_info = (reg_info *)0;
	  renumber = (short *)0;
	}
      regno_allocated = 0;
      regno_max = 0;
      return;
    }

  if (num_regs > regno_allocated)
    {
      regno_allocated = num_regs + (num_regs / 20);	/* add some slop space */
      size_info = regno_allocated * sizeof (reg_info);
      size_renumber = regno_allocated * sizeof (short);

      if (!reg_n_info)
	{
	  reg_n_info = (reg_info *) xmalloc (size_info);
	  renumber = (short *) xmalloc (size_renumber);
	}

      else if (new_p)		/* if we're zapping everything, no need to realloc */
	{
	  free ((char *)reg_n_info);
	  free ((char *)renumber);
	  reg_n_info = (reg_info *) xmalloc (size_info);
	  renumber = (short *) xmalloc (size_renumber);
	}

      else
	{
	  reg_n_info = (reg_info *) xrealloc ((char *)reg_n_info, size_info);
	  renumber = (short *) xrealloc ((char *)renumber, size_renumber);
	}
    }

  if (min < num_regs)
    {
      bzero ((char *) &reg_n_info[min], (num_regs - min) * sizeof (reg_info));
      for (i = min; i < num_regs; i++)
	{
	  REG_BASIC_BLOCK (i) = REG_BLOCK_UNKNOWN;
	  renumber[i] = -1;
	}
    }

  if (renumber_p)
    reg_renumber = renumber;

  /* Tell the regset code about the new number of registers */
  MAX_REGNO_REG_SET (num_regs, new_p, renumber_p);

  regno_max = num_regs;
}


/* This is the `regscan' pass of the compiler, run just before cse
   and again just before loop.

   It finds the first and last use of each pseudo-register
   and records them in the vectors regno_first_uid, regno_last_uid
   and counts the number of sets in the vector reg_n_sets.

   REPEAT is nonzero the second time this is called.  */

/* Maximum number of parallel sets and clobbers in any insn in this fn.
   Always at least 3, since the combiner could put that many together
   and we want this to remain correct for all the remaining passes.  */

int max_parallel;

void
reg_scan (f, nregs, repeat)
     rtx f;
     int nregs;
     int repeat;
{
  register rtx insn;

  allocate_reg_info (nregs, TRUE, FALSE);
  max_parallel = 3;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN
	|| GET_CODE (insn) == CALL_INSN
	|| GET_CODE (insn) == JUMP_INSN)
      {
	if (GET_CODE (PATTERN (insn)) == PARALLEL
	    && XVECLEN (PATTERN (insn), 0) > max_parallel)
	  max_parallel = XVECLEN (PATTERN (insn), 0);
	reg_scan_mark_refs (PATTERN (insn), insn, 0);

	if (REG_NOTES (insn))
	  reg_scan_mark_refs (REG_NOTES (insn), insn, 1);
      }
}

/* X is the expression to scan.  INSN is the insn it appears in.
   NOTE_FLAG is nonzero if X is from INSN's notes rather than its body.  */

static void
reg_scan_mark_refs (x, insn, note_flag)
     rtx x;
     rtx insn;
     int note_flag;
{
  register enum rtx_code code = GET_CODE (x);
  register rtx dest;
  register rtx note;

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case REG:
      {
	register int regno = REGNO (x);

	REGNO_LAST_NOTE_UID (regno) = INSN_UID (insn);
	if (!note_flag)
	  REGNO_LAST_UID (regno) = INSN_UID (insn);
	if (REGNO_FIRST_UID (regno) == 0)
	  REGNO_FIRST_UID (regno) = INSN_UID (insn);
      }
      break;

    case EXPR_LIST:
      if (XEXP (x, 0))
	reg_scan_mark_refs (XEXP (x, 0), insn, note_flag);
      if (XEXP (x, 1))
	reg_scan_mark_refs (XEXP (x, 1), insn, note_flag);
      break;

    case INSN_LIST:
      if (XEXP (x, 1))
	reg_scan_mark_refs (XEXP (x, 1), insn, note_flag);
      break;

    case SET:
      /* Count a set of the destination if it is a register.  */
      for (dest = SET_DEST (x);
	   GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART
	   || GET_CODE (dest) == ZERO_EXTEND;
	   dest = XEXP (dest, 0))
	;

      if (GET_CODE (dest) == REG)
	REG_N_SETS (REGNO (dest))++;

      /* If this is setting a pseudo from another pseudo or the sum of a
	 pseudo and a constant integer and the other pseudo is known to be
	 a pointer, set the destination to be a pointer as well.

	 Likewise if it is setting the destination from an address or from a
	 value equivalent to an address or to the sum of an address and
	 something else.
		     
	 But don't do any of this if the pseudo corresponds to a user
	 variable since it should have already been set as a pointer based
	 on the type.  */

      if (GET_CODE (SET_DEST (x)) == REG
	  && REGNO (SET_DEST (x)) >= FIRST_PSEUDO_REGISTER
	  && ! REG_USERVAR_P (SET_DEST (x))
	  && ! REGNO_POINTER_FLAG (REGNO (SET_DEST (x)))
	  && ((GET_CODE (SET_SRC (x)) == REG
	       && REGNO_POINTER_FLAG (REGNO (SET_SRC (x))))
	      || ((GET_CODE (SET_SRC (x)) == PLUS
		   || GET_CODE (SET_SRC (x)) == LO_SUM)
		  && GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
		  && GET_CODE (XEXP (SET_SRC (x), 0)) == REG
		  && REGNO_POINTER_FLAG (REGNO (XEXP (SET_SRC (x), 0))))
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
	REGNO_POINTER_FLAG (REGNO (SET_DEST (x))) = 1;

      /* ... fall through ...  */

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  {
	    if (fmt[i] == 'e')
	      reg_scan_mark_refs (XEXP (x, i), insn, note_flag);
	    else if (fmt[i] == 'E' && XVEC (x, i) != 0)
	      {
		register int j;
		for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		  reg_scan_mark_refs (XVECEXP (x, i, j), insn, note_flag);
	      }
	  }
      }
    }
}

/* Return nonzero if C1 is a subset of C2, i.e., if every register in C1
   is also in C2.  */

int
reg_class_subset_p (c1, c2)
     register enum reg_class c1;
     register enum reg_class c2;
{
  if (c1 == c2) return 1;

  if (c2 == ALL_REGS)
  win:
    return 1;
  GO_IF_HARD_REG_SUBSET (reg_class_contents[(int)c1],
			 reg_class_contents[(int)c2],
			 win);
  return 0;
}

/* Return nonzero if there is a register that is in both C1 and C2.  */

int
reg_classes_intersect_p (c1, c2)
     register enum reg_class c1;
     register enum reg_class c2;
{
#ifdef HARD_REG_SET
  register
#endif
    HARD_REG_SET c;

  if (c1 == c2) return 1;

  if (c1 == ALL_REGS || c2 == ALL_REGS)
    return 1;

  COPY_HARD_REG_SET (c, reg_class_contents[(int) c1]);
  AND_HARD_REG_SET (c, reg_class_contents[(int) c2]);

  GO_IF_HARD_REG_SUBSET (c, reg_class_contents[(int) NO_REGS], lose);
  return 1;

 lose:
  return 0;
}

/* Release any memory allocated by register sets.  */

void
regset_release_memory ()
{
  if (basic_block_live_at_start)
    {
      free_regset_vector (basic_block_live_at_start, n_basic_blocks);
      basic_block_live_at_start = 0;
    }

  FREE_REG_SET (regs_live_at_setjmp);
  bitmap_release_memory ();
}
