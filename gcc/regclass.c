/* Compute register class preferences for pseudo-registers.
   Copyright (C) 1987, 1988, 1991 Free Software Foundation, Inc.

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


/* This file contains two passes of the compiler: reg_scan and reg_class.
   It also defines some tables of information about the hardware registers
   and a function init_reg_sets to initialize the tables.  */

#include "config.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "insn-config.h"
#include "recog.h"

#ifndef REGISTER_MOVE_COST
#define REGISTER_MOVE_COST(x, y) 2
#endif

#ifndef MEMORY_MOVE_COST
#define MEMORY_MOVE_COST(x) 2
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

HARD_REG_SET reg_class_contents[] = REG_CLASS_CONTENTS;

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


/* Indexed by n, gives number of times (REG n) is set or clobbered.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.

   This information applies to both hard registers and pseudo registers,
   unlike much of the information above.  */

short *reg_n_sets;

/* Function called only once to initialize the above data on reg usage.
   Once this is done, various switches may override.  */

void
init_reg_sets ()
{
  register int i, j;

  bcopy (initial_fixed_regs, fixed_regs, sizeof fixed_regs);
  bcopy (initial_call_used_regs, call_used_regs, sizeof call_used_regs);
  bzero (global_regs, sizeof global_regs);

  /* Compute number of hard regs in each class.  */

  bzero (reg_class_size, sizeof reg_class_size);
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
}

/* After switches have been processed, which perhaps alter
   `fixed_regs' and `call_used_regs', convert them to HARD_REG_SETs.  */

void
init_reg_sets_1 ()
{
  register int i;

  /* This macro allows the fixed or call-used registers
     to depend on target flags.  */

#ifdef CONDITIONAL_REGISTER_USAGE
  CONDITIONAL_REGISTER_USAGE;
#endif

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i])
      {
	if (call_used_regs[i] && ! fixed_regs[i])
	  warning ("call-clobbered register used for global register variable");
	fixed_regs[i] = 1;
	/* Prevent saving/restoring of this reg.  */
	call_used_regs[i] = 1;
      }

  /* Initialize "constant" tables.  */

  CLEAR_HARD_REG_SET (fixed_reg_set);
  CLEAR_HARD_REG_SET (call_used_reg_set);
  CLEAR_HARD_REG_SET (call_fixed_reg_set);

  bcopy (fixed_regs, call_fixed_regs, sizeof call_fixed_regs);
#ifdef STRUCT_VALUE_REGNUM
  call_fixed_regs[STRUCT_VALUE_REGNUM] = 1;
#endif
#ifdef STATIC_CHAIN_REGNUM
  call_fixed_regs[STATIC_CHAIN_REGNUM] = 1;
#endif

  n_non_fixed_regs = 0;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (FUNCTION_VALUE_REGNO_P (i))
	call_fixed_regs[i] = 1;
      if (fixed_regs[i])
	SET_HARD_REG_BIT (fixed_reg_set, i);
      else
	n_non_fixed_regs++;

      if (call_used_regs[i])
	SET_HARD_REG_BIT (call_used_reg_set, i);
      if (call_fixed_regs[i])
	SET_HARD_REG_BIT (call_fixed_reg_set, i);
    }
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

  /* Decode the name and update the primary form of
     the register info.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (reg_names[i][0] && ! strcmp (reg_names[i], name))
      {
	fixed_regs[i] = fixed;
	call_used_regs[i] = call_used;
	break;
      }

  if (i == FIRST_PSEUDO_REGISTER)
    {
      warning ("unknown register name: %s", name);
      return;
    }
}

/* Now the data and code for the `regclass' pass, which happens
   just before local-alloc.  */

/* savings[R].savings[CL] is twice the amount saved by putting register R
   in class CL.  This data is used within `regclass' and freed
   when it is finished.  */

struct savings
{
  short savings[N_REG_CLASSES];
  short memcost;
  short nrefs;
};

static struct savings *savings;

/* (enum reg_class) prefclass[R] is the preferred class for pseudo number R.
   This is available after `regclass' is run.  */

static char *prefclass;

/* preferred_or_nothing[R] is nonzero if we should put pseudo number R
   in memory if we can't get its perferred class.
   This is available after `regclass' is run.  */

static char *preferred_or_nothing;

/* Record the depth of loops that we are in, 1 for no loops.  */

static int loop_depth;

void reg_class_record ();
void record_address_regs ();


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

int
reg_preferred_or_nothing (regno)
{
  if (prefclass == 0)
    return 0;
  return preferred_or_nothing[regno];
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
  register int i;

  init_recog ();

  /* Zero out our accumulation of the cost of each class for each reg.  */

  savings = (struct savings *) alloca (nregs * sizeof (struct savings));
  bzero (savings, nregs * sizeof (struct savings));

  loop_depth = 1;

  /* Scan the instructions and record each time it would
     save code to put a certain register in a certain class.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	loop_depth++;
      else if (GET_CODE (insn) == NOTE
	       && NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	loop_depth--;
      else if ((GET_CODE (insn) == INSN
		&& GET_CODE (PATTERN (insn)) != USE
		&& GET_CODE (PATTERN (insn)) != CLOBBER
		&& GET_CODE (PATTERN (insn)) != ASM_INPUT)
	       || (GET_CODE (insn) == JUMP_INSN
		   && GET_CODE (PATTERN (insn)) != ADDR_VEC
		   && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	       || GET_CODE (insn) == CALL_INSN)
	{
	  if (GET_CODE (insn) == INSN && asm_noperands (PATTERN (insn)) >= 0)
	    {
	      int noperands = asm_noperands (PATTERN (insn));
	      /* We don't use alloca because alloca would not free
		 any of the space until this function returns.  */
	      rtx *operands = (rtx *) oballoc (noperands * sizeof (rtx));
	      char **constraints
		= (char **) oballoc (noperands * sizeof (char *));

	      decode_asm_operands (PATTERN (insn), operands, 0, constraints, 0);

	      for (i = noperands - 1; i >= 0; i--)
		reg_class_record (operands[i], i, constraints);

	      obfree (operands);
	    }
	  else
	    {
	      int insn_code_number = recog_memoized (insn);
	      rtx set = single_set (insn);

	      insn_extract (insn);

	      for (i = insn_n_operands[insn_code_number] - 1; i >= 0; i--)
		reg_class_record (recog_operand[i], i,
				  insn_operand_constraint[insn_code_number]);

	      /* If this insn loads a parameter from its stack slot,
		 then it represents a savings, rather than a cost,
		 if the parameter is stored in memory.  Record this fact.  */
	      if (set != 0 && GET_CODE (SET_DEST (set)) == REG
		  && GET_CODE (SET_SRC (set)) == MEM)
		{
		  rtx note = find_reg_note (insn, REG_EQUIV, 0);
		  if (note != 0 && GET_CODE (XEXP (note, 0)) == MEM)
		    savings[REGNO (SET_DEST (set))].memcost
		      -= (MEMORY_MOVE_COST (GET_MODE (SET_DEST (set)))
			  * loop_depth);
		}
	      
	      /* Improve handling of two-address insns such as
		 (set X (ashift CONST Y)) where CONST must be made to match X.
		 Change it into two insns: (set X CONST)  (set X (ashift X Y)).
		 If we left this for reloading, it would probably get three
		 insns because X and Y might go in the same place.
		 This prevents X and Y from receiving the same hard reg.

		 We can only do this if the modes of operands 0 and 1 (which
		 might not be the same) are tieable.  */

	      if (optimize
		  && insn_n_operands[insn_code_number] >= 3
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
		    = emit_insn_before (gen_move_insn (dest, recog_operand[1]),
					insn);

		  /* If this insn was the start of a basic block,
		     include the new insn in that block.  */
		  if (previnsn == 0 || GET_CODE (previnsn) == JUMP_INSN)
		    {
		      int b;
		      for (b = 0; b < n_basic_blocks; b++)
			if (insn == basic_block_head[b])
			  basic_block_head[b] = newinsn;
		    }

		  /* This makes one more setting of new insns's destination. */
		  reg_n_sets[REGNO (recog_operand[0])]++;

		  *recog_operand_loc[1] = recog_operand[0];
		  for (i = insn_n_dups[insn_code_number] - 1; i >= 0; i--)
		    if (recog_dup_num[i] == 1)
		      *recog_dup_loc[i] = recog_operand[0];
		}
	    }
	}
    }

  /* Now for each register look at how desirable each class is
     and find which class is preferred.  Store that in `prefclass[REGNO]'.  */
    
  prefclass = (char *) oballoc (nregs);
    
  preferred_or_nothing = (char *) oballoc (nregs);

  for (i = FIRST_PSEUDO_REGISTER; i < nregs; i++)
    {
      register int best_savings = 0;
      enum reg_class best = ALL_REGS;

      /* This is an enum reg_class, but we call it an int
	 to save lots of casts.  */
      register int class;
      register struct savings *p = &savings[i];

      for (class = (int) ALL_REGS - 1; class > 0; class--)
	{
	  if (p->savings[class] > best_savings)
	    {
	      best_savings = p->savings[class];
	      best = (enum reg_class) class;
	    }
	  else if (p->savings[class] == best_savings)
	    {
	      best = reg_class_subunion[(int)best][class];
	    }
	}

#if 0
      /* Note that best_savings is twice number of places something
	 is saved.  */
      if ((best_savings - p->savings[(int) GENERAL_REGS]) * 5 < reg_n_refs[i])
	prefclass[i] = (int) GENERAL_REGS;
      else
	prefclass[i] = (int) best;
#else
      /* We cast to (int) because (char) hits bugs in some compilers.  */
      prefclass[i] = (int) best;
#endif

      /* reg_n_refs + p->memcost measures the cost of putting in memory.
	 If a GENERAL_REG is no better, don't even try for one.
	 Since savings and memcost are 2 * number of refs,
	 this effectively counts each memory operand not needing reloading
	 as costing 1/2 of a reload insn.  */
      if (reg_n_refs != 0)
	preferred_or_nothing[i]
	  = ((best_savings - p->savings[(int) GENERAL_REGS])
	     >= p->nrefs + p->memcost);
    }
#endif /* REGISTER_CONSTRAINTS */
}

#ifdef REGISTER_CONSTRAINTS

/* Scan an operand OP for register class preferences.
   OPNO is the operand number, and CONSTRAINTS is the constraint
   vector for the insn.

   Record the preferred register classes from the constraint for OP
   if OP is a register.  If OP is a memory reference, record suitable
   preferences for registers used in the address.  */

void
reg_class_record (op, opno, constraints)
     rtx op;
     int opno;
     char **constraints;
{
  char *constraint = constraints[opno];
  register char *p;
  register enum reg_class class = NO_REGS;
  char *next = 0;
  int memok = 0;
  int double_cost = 0;

  if (op == 0)
    return;

  while (1)
    {
      if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);
      else break;
    }

  /* Memory reference: scan the address.  */

  if (GET_CODE (op) == MEM)
    record_address_regs (XEXP (op, 0), 2, 0);

  if (GET_CODE (op) != REG)
    {
      /* If the constraint says the operand is supposed to BE an address,
	 scan it as one.  */

      if (constraint != 0 && constraint[0] == 'p')
	record_address_regs (op, 2, 0);
      return;
    }

  /* Operand is a register: examine the constraint for specified classes.  */

  for (p = constraint; *p || next; p++)
    {
      enum reg_class new_class = NO_REGS;

      if (*p == 0)
	{
	  p = next;
	  next = 0;
	}
      switch (*p)
	{
	case '=':
	case '?':
	case '#':
	case '&':
	case '!':
	case '%':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'i':
	case 'n':
	case 's':
	case 'p':
	case ',':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
#ifdef EXTRA_CONSTRAINT
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
#endif
	case 'V':
	case 'X':
	  break;

	case '+':
	  /* An input-output operand is twice as costly if it loses.  */
	  double_cost = 1;
	  break;

	case 'm':
	case 'o':
	  memok = 1;
	  break;

	  /* * means ignore following letter
	     when choosing register preferences.  */
	case '*':
	  p++;
	  break;

	case 'g':
	case 'r':
	  new_class = GENERAL_REGS;
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	  /* If constraint says "match another operand",
	     use that operand's constraint to choose preferences.  */
	  next = constraints[*p - '0'];
	  break;

	default:
	  new_class = REG_CLASS_FROM_LETTER (*p);
	  break;
	}

      /* If this object can fit into the class requested, compute the subunion
	 of the requested class and classes found so far.  */
      if (CLASS_MAX_NREGS (new_class, GET_MODE (op))
	  <= reg_class_size[(int) new_class])
	class = reg_class_subunion[(int) class][(int) new_class];
    }

  {
    register int i;
    register struct savings *pp;
    register enum reg_class class1;
    int cost = 2 * (1 + double_cost) * loop_depth;
    pp = &savings[REGNO (op)];

    /* Increment the savings for this reg
       for each class contained in the one the constraint asks for.  */

    if (class != NO_REGS && class != ALL_REGS)
      {
	int extracost;

	pp->savings[(int) class] += cost;
	for (i = 0; ; i++)
	  {
	    class1 = reg_class_subclasses[(int)class][i];
	    if (class1 == LIM_REG_CLASSES)
	      break;
	    pp->savings[(int) class1] += cost;
	  }
	/* If it's slow to move data between this class and GENERAL_REGS,
	   record that fact.  */
	extracost = (REGISTER_MOVE_COST (class, GENERAL_REGS) - 2) * loop_depth;
	if (extracost > 0)
	  {
	    /* Check that this class and GENERAL_REGS don't overlap.
	       REGISTER_MOVE_COST is meaningless if there is overlap.  */
	    HARD_REG_SET temp;
	    COMPL_HARD_REG_SET (temp, reg_class_contents[(int) class]);
	    GO_IF_HARD_REG_SUBSET (reg_class_contents[(int) GENERAL_REGS],
				   temp, label1);
	    /* Overlap.  */
	    goto label2;

	  label1: /* No overlap.  */
	    /* Charge this extra cost to GENERAL_REGS
	       and all its subclasses (none of which overlap this class).  */
	    extracost = extracost * cost / (2 * loop_depth);
	    pp->savings[(int) GENERAL_REGS] -= extracost;
	    for (i = 0; ; i++)
	      {
		class1 = reg_class_subclasses[(int)GENERAL_REGS][i];
		if (class1 == LIM_REG_CLASSES)
		  break;
		pp->savings[(int) class1] -= extracost;
	      }

	  label2: ;
	  }
      }

    if (! memok)
      pp->memcost += (MEMORY_MOVE_COST (GET_MODE (op)) * (1 + double_cost)
		      - 1) * loop_depth;
    pp->nrefs += loop_depth;
  }
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a memory address, X.
   BCOST is the cost if X is a register and it fails to be in BASE_REG_CLASS.
   ICOST is the cost if it fails to be in INDEX_REG_CLASS. */

void
record_address_regs (x, bcost, icost)
     rtx x;
     int bcost, icost;
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
	 to make a good choice most of the time.  */
      {
	rtx arg0 = XEXP (x, 0);
	rtx arg1 = XEXP (x, 1);
	register enum rtx_code code0 = GET_CODE (arg0);
	register enum rtx_code code1 = GET_CODE (arg1);
	int icost0 = 0;
	int icost1 = 0;
	int suppress1 = 0;
	int suppress0 = 0;

	/* Look inside subregs.  */
	while (code0 == SUBREG)
	  arg0 = SUBREG_REG (arg0), code0 = GET_CODE (arg0);
	while (code1 == SUBREG)
	  arg1 = SUBREG_REG (arg1), code1 = GET_CODE (arg1);

	if (code0 == MULT || code1 == MEM)
	  icost0 = 2;
	else if (code1 == MULT || code0 == MEM)
	  icost1 = 2;
	else if (code0 == CONST_INT)
	  suppress0 = 1;
	else if (code1 == CONST_INT)
	  suppress1 = 1;
	else if (code0 == REG && code1 == REG)
	  {
	    if (REGNO_POINTER_FLAG (REGNO (arg0)))
	      icost1 = 2;
	    else if (REGNO_POINTER_FLAG (REGNO (arg1)))
	      icost0 = 2;
	    else
	      icost0 = icost1 = 1;
	  }
	else if (code0 == REG)
	  {
	    if (code1 == PLUS
		&& ! REGNO_POINTER_FLAG (REGNO (arg0)))
	      icost0 = 2;
	    else
	      REGNO_POINTER_FLAG (REGNO (arg0)) = 1;
	  }
	else if (code1 == REG)
	  {
	    if (code0 == PLUS
		&& ! REGNO_POINTER_FLAG (REGNO (arg1)))
	      icost1 = 2;
	    else
	      REGNO_POINTER_FLAG (REGNO (arg1)) = 1;
	  }

	/* ICOST0 determines whether we are treating operand 0
	   as a base register or as an index register.
	   SUPPRESS0 nonzero means it isn't a register at all.
	   ICOST1 and SUPPRESS1 are likewise for operand 1.  */

	if (! suppress0)
	  record_address_regs (arg0, 2 - icost0, icost0);
	if (! suppress1)
	  record_address_regs (arg1, 2 - icost1, icost1);
      }
      break;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
      /* Double the importance of a pseudo register that is incremented
	 or decremented, since it would take two extra insns
	 if it ends up in the wrong place.  */
      record_address_regs (XEXP (x, 0), 2 * bcost, 2 * icost);
      break;

    case REG:
      {
	register struct savings *pp;
	register enum reg_class class, class1;
	pp = &savings[REGNO (x)];
	pp->nrefs += loop_depth;

	/* We have an address (or part of one) that is just one register.  */

	/* Record BCOST worth of savings for classes contained
	   in BASE_REG_CLASS.  */

	class = BASE_REG_CLASS;
	if (class != NO_REGS && class != ALL_REGS)
	  {
	    register int i;
	    pp->savings[(int) class] += bcost * loop_depth;
	    for (i = 0; ; i++)
	      {
		class1 = reg_class_subclasses[(int)class][i];
		if (class1 == LIM_REG_CLASSES)
		  break;
		pp->savings[(int) class1] += bcost * loop_depth;
	      }
	  }

	/* Record ICOST worth of savings for classes contained
	   in INDEX_REG_CLASS.  */

	class = INDEX_REG_CLASS;
	if (icost != 0 && class != NO_REGS && class != ALL_REGS)
	  {
	    register int i;
	    pp->savings[(int) class] += icost * loop_depth;
	    for (i = 0; ; i++)
	      {
		class1 = reg_class_subclasses[(int)class][i];
		if (class1 == LIM_REG_CLASSES)
		  break;
		pp->savings[(int) class1] += icost * loop_depth;
	      }
	  }
      }
      break;

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  if (fmt[i] == 'e')
	    record_address_regs (XEXP (x, i), bcost, icost);
      }
    }
}
#endif /* REGISTER_CONSTRAINTS */

/* This is the `regscan' pass of the compiler, run just before cse
   and again just before loop.

   It finds the first and last use of each pseudo-register
   and records them in the vectors regno_first_uid, regno_last_uid
   and counts the number of sets in the vector reg_n_sets.

   REPEAT is nonzero the second time this is called.  */

/* Indexed by pseudo register number, gives uid of first insn using the reg
   (as of the time reg_scan is called).  */

short *regno_first_uid;

/* Indexed by pseudo register number, gives uid of last insn using the reg
   (as of the time reg_scan is called).  */

short *regno_last_uid;

/* Record the number of registers we used when we allocated the above two
   tables.  If we are called again with more than this, we must re-allocate
   the tables.  */

static int highest_regno_in_uid_map;

/* Maximum number of parallel sets and clobbers in any insn in this fn.
   Always at least 3, since the combiner could put that many togetherm
   and we want this to remain correct for all the remaining passes.  */

int max_parallel;

void reg_scan_mark_refs ();

void
reg_scan (f, nregs, repeat)
     rtx f;
     int nregs;
     int repeat;
{
  register rtx insn;

  if (!repeat || nregs > highest_regno_in_uid_map)
    {
      /* Leave some spare space in case more regs are allocated.  */
      highest_regno_in_uid_map = nregs + nregs / 20;
      regno_first_uid
	= (short *) oballoc (highest_regno_in_uid_map * sizeof (short));
      regno_last_uid
	= (short *) oballoc (highest_regno_in_uid_map * sizeof (short));
      reg_n_sets
	= (short *) oballoc (highest_regno_in_uid_map * sizeof (short));
    }

  bzero (regno_first_uid, highest_regno_in_uid_map * sizeof (short));
  bzero (regno_last_uid, highest_regno_in_uid_map * sizeof (short));
  bzero (reg_n_sets, highest_regno_in_uid_map * sizeof (short));

  max_parallel = 3;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN
	|| GET_CODE (insn) == CALL_INSN
	|| GET_CODE (insn) == JUMP_INSN)
      {
	if (GET_CODE (PATTERN (insn)) == PARALLEL
	    && XVECLEN (PATTERN (insn), 0) > max_parallel)
	  max_parallel = XVECLEN (PATTERN (insn), 0);
	reg_scan_mark_refs (PATTERN (insn), INSN_UID (insn));
      }
}

void
reg_scan_mark_refs (x, uid)
     rtx x;
     int uid;
{
  register enum rtx_code code = GET_CODE (x);
  register rtx dest;

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

	regno_last_uid[regno] = uid;
	if (regno_first_uid[regno] == 0)
	  regno_first_uid[regno] = uid;
      }
      break;

    case SET:
      /* Count a set of the destination if it is a register.  */
      for (dest = SET_DEST (x);
	   GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART
	   || GET_CODE (dest) == ZERO_EXTEND;
	   dest = XEXP (dest, 0))
	;

      if (GET_CODE (dest) == REG)
	reg_n_sets[REGNO (dest)]++;

      /* ... fall through ... */

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  {
	    if (fmt[i] == 'e')
	      reg_scan_mark_refs (XEXP (x, i), uid);
	    else if (fmt[i] == 'E' && XVEC (x, i) != 0)
	      {
		register int j;
		for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		  reg_scan_mark_refs (XVECEXP (x, i, j), uid);		  
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

