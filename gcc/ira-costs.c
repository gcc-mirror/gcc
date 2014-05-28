/* IRA hard register and memory cost calculation for allocnos or pseudos.
   Copyright (C) 2006-2014 Free Software Foundation, Inc.
   Contributed by Vladimir Makarov <vmakarov@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hash-table.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "expr.h"
#include "tm_p.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "addresses.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "diagnostic-core.h"
#include "target.h"
#include "params.h"
#include "ira-int.h"

/* The flags is set up every time when we calculate pseudo register
   classes through function ira_set_pseudo_classes.  */
static bool pseudo_classes_defined_p = false;

/* TRUE if we work with allocnos.  Otherwise we work with pseudos.  */
static bool allocno_p;

/* Number of elements in array `costs'.  */
static int cost_elements_num;

/* The `costs' struct records the cost of using hard registers of each
   class considered for the calculation and of using memory for each
   allocno or pseudo.  */
struct costs
{
  int mem_cost;
  /* Costs for register classes start here.  We process only some
     allocno classes.  */
  int cost[1];
};

#define max_struct_costs_size \
  (this_target_ira_int->x_max_struct_costs_size)
#define init_cost \
  (this_target_ira_int->x_init_cost)
#define temp_costs \
  (this_target_ira_int->x_temp_costs)
#define op_costs \
  (this_target_ira_int->x_op_costs)
#define this_op_costs \
  (this_target_ira_int->x_this_op_costs)

/* Costs of each class for each allocno or pseudo.  */
static struct costs *costs;

/* Accumulated costs of each class for each allocno.  */
static struct costs *total_allocno_costs;

/* It is the current size of struct costs.  */
static int struct_costs_size;

/* Return pointer to structure containing costs of allocno or pseudo
   with given NUM in array ARR.  */
#define COSTS(arr, num) \
  ((struct costs *) ((char *) (arr) + (num) * struct_costs_size))

/* Return index in COSTS when processing reg with REGNO.  */
#define COST_INDEX(regno) (allocno_p 					     \
                           ? ALLOCNO_NUM (ira_curr_regno_allocno_map[regno]) \
			   : (int) regno)

/* Record register class preferences of each allocno or pseudo.  Null
   value means no preferences.  It happens on the 1st iteration of the
   cost calculation.  */
static enum reg_class *pref;

/* Allocated buffers for pref.  */
static enum reg_class *pref_buffer;

/* Record allocno class of each allocno with the same regno.  */
static enum reg_class *regno_aclass;

/* Record cost gains for not allocating a register with an invariant
   equivalence.  */
static int *regno_equiv_gains;

/* Execution frequency of the current insn.  */
static int frequency;



/* Info about reg classes whose costs are calculated for a pseudo.  */
struct cost_classes
{
  /* Number of the cost classes in the subsequent array.  */
  int num;
  /* Container of the cost classes.  */
  enum reg_class classes[N_REG_CLASSES];
  /* Map reg class -> index of the reg class in the previous array.
     -1 if it is not a cost classe.  */
  int index[N_REG_CLASSES];
  /* Map hard regno index of first class in array CLASSES containing
     the hard regno, -1 otherwise.  */
  int hard_regno_index[FIRST_PSEUDO_REGISTER];
};

/* Types of pointers to the structure above.  */
typedef struct cost_classes *cost_classes_t;
typedef const struct cost_classes *const_cost_classes_t;

/* Info about cost classes for each pseudo.  */
static cost_classes_t *regno_cost_classes;

/* Helper for cost_classes hashing.  */

struct cost_classes_hasher
{
  typedef cost_classes value_type;
  typedef cost_classes compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Returns hash value for cost classes info HV.  */
inline hashval_t
cost_classes_hasher::hash (const value_type *hv)
{
  return iterative_hash (&hv->classes, sizeof (enum reg_class) * hv->num, 0);
}

/* Compares cost classes info HV1 and HV2.  */
inline bool
cost_classes_hasher::equal (const value_type *hv1, const compare_type *hv2)
{
  return (hv1->num == hv2->num
	  && memcmp (hv1->classes, hv2->classes,
		     sizeof (enum reg_class) * hv1->num) == 0);
}

/* Delete cost classes info V from the hash table.  */
inline void
cost_classes_hasher::remove (value_type *v)
{
  ira_free (v);
}

/* Hash table of unique cost classes.  */
static hash_table <cost_classes_hasher> cost_classes_htab;

/* Map allocno class -> cost classes for pseudo of given allocno
   class.  */
static cost_classes_t cost_classes_aclass_cache[N_REG_CLASSES];

/* Map mode -> cost classes for pseudo of give mode.  */
static cost_classes_t cost_classes_mode_cache[MAX_MACHINE_MODE];

/* Initialize info about the cost classes for each pseudo.  */
static void
initiate_regno_cost_classes (void)
{
  int size = sizeof (cost_classes_t) * max_reg_num ();

  regno_cost_classes = (cost_classes_t *) ira_allocate (size);
  memset (regno_cost_classes, 0, size);
  memset (cost_classes_aclass_cache, 0,
	  sizeof (cost_classes_t) * N_REG_CLASSES);
  memset (cost_classes_mode_cache, 0,
	  sizeof (cost_classes_t) * MAX_MACHINE_MODE);
  cost_classes_htab.create (200);
}

/* Create new cost classes from cost classes FROM and set up members
   index and hard_regno_index.  Return the new classes.  The function
   implements some common code of two functions
   setup_regno_cost_classes_by_aclass and
   setup_regno_cost_classes_by_mode.  */
static cost_classes_t
setup_cost_classes (cost_classes_t from)
{
  cost_classes_t classes_ptr;
  enum reg_class cl;
  int i, j, hard_regno;

  classes_ptr = (cost_classes_t) ira_allocate (sizeof (struct cost_classes));
  classes_ptr->num = from->num;
  for (i = 0; i < N_REG_CLASSES; i++)
    classes_ptr->index[i] = -1;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    classes_ptr->hard_regno_index[i] = -1;
  for (i = 0; i < from->num; i++)
    {
      cl = classes_ptr->classes[i] = from->classes[i];
      classes_ptr->index[cl] = i;
      for (j = ira_class_hard_regs_num[cl] - 1; j >= 0; j--)
	{
	  hard_regno = ira_class_hard_regs[cl][j];
	  if (classes_ptr->hard_regno_index[hard_regno] < 0)
	    classes_ptr->hard_regno_index[hard_regno] = i;
	}
    }
  return classes_ptr;
}

/* Setup cost classes for pseudo REGNO whose allocno class is ACLASS.
   This function is used when we know an initial approximation of
   allocno class of the pseudo already, e.g. on the second iteration
   of class cost calculation or after class cost calculation in
   register-pressure sensitive insn scheduling or register-pressure
   sensitive loop-invariant motion.  */
static void
setup_regno_cost_classes_by_aclass (int regno, enum reg_class aclass)
{
  static struct cost_classes classes;
  cost_classes_t classes_ptr;
  enum reg_class cl;
  int i;
  cost_classes **slot;
  HARD_REG_SET temp, temp2;
  bool exclude_p;

  if ((classes_ptr = cost_classes_aclass_cache[aclass]) == NULL)
    {
      COPY_HARD_REG_SET (temp, reg_class_contents[aclass]);
      AND_COMPL_HARD_REG_SET (temp, ira_no_alloc_regs);
      /* We exclude classes from consideration which are subsets of
	 ACLASS only if ACLASS is an uniform class.  */
      exclude_p = ira_uniform_class_p[aclass];
      classes.num = 0;
      for (i = 0; i < ira_important_classes_num; i++)
	{
	  cl = ira_important_classes[i];
	  if (exclude_p)
	    {
	      /* Exclude non-uniform classes which are subsets of
		 ACLASS.  */
	      COPY_HARD_REG_SET (temp2, reg_class_contents[cl]);
	      AND_COMPL_HARD_REG_SET (temp2, ira_no_alloc_regs);
	      if (hard_reg_set_subset_p (temp2, temp) && cl != aclass)
		continue;
	    }
	  classes.classes[classes.num++] = cl;
	}
      slot = cost_classes_htab.find_slot (&classes, INSERT);
      if (*slot == NULL)
	{
	  classes_ptr = setup_cost_classes (&classes);
	  *slot = classes_ptr;
	}
      classes_ptr = cost_classes_aclass_cache[aclass] = (cost_classes_t) *slot;
    }
  regno_cost_classes[regno] = classes_ptr;
}

/* Setup cost classes for pseudo REGNO with MODE.  Usage of MODE can
   decrease number of cost classes for the pseudo, if hard registers
   of some important classes can not hold a value of MODE.  So the
   pseudo can not get hard register of some important classes and cost
   calculation for such important classes is only waisting CPU
   time.  */
static void
setup_regno_cost_classes_by_mode (int regno, enum machine_mode mode)
{
  static struct cost_classes classes;
  cost_classes_t classes_ptr;
  enum reg_class cl;
  int i;
  cost_classes **slot;
  HARD_REG_SET temp;

  if ((classes_ptr = cost_classes_mode_cache[mode]) == NULL)
    {
      classes.num = 0;
      for (i = 0; i < ira_important_classes_num; i++)
	{
	  cl = ira_important_classes[i];
	  COPY_HARD_REG_SET (temp, ira_prohibited_class_mode_regs[cl][mode]);
	  IOR_HARD_REG_SET (temp, ira_no_alloc_regs);
	  if (hard_reg_set_subset_p (reg_class_contents[cl], temp))
	    continue;
	  classes.classes[classes.num++] = cl;
	}
      slot = cost_classes_htab.find_slot (&classes, INSERT);
      if (*slot == NULL)
	{
	  classes_ptr = setup_cost_classes (&classes);
	  *slot = classes_ptr;
	}
      else
	classes_ptr = (cost_classes_t) *slot;
      cost_classes_mode_cache[mode] = (cost_classes_t) *slot;
    }
  regno_cost_classes[regno] = classes_ptr;
}

/* Finilize info about the cost classes for each pseudo.  */
static void
finish_regno_cost_classes (void)
{
  ira_free (regno_cost_classes);
  cost_classes_htab.dispose ();
}



/* Compute the cost of loading X into (if TO_P is TRUE) or from (if
   TO_P is FALSE) a register of class RCLASS in mode MODE.  X must not
   be a pseudo register.  */
static int
copy_cost (rtx x, enum machine_mode mode, reg_class_t rclass, bool to_p,
	   secondary_reload_info *prev_sri)
{
  secondary_reload_info sri;
  reg_class_t secondary_class = NO_REGS;

  /* If X is a SCRATCH, there is actually nothing to move since we are
     assuming optimal allocation.  */
  if (GET_CODE (x) == SCRATCH)
    return 0;

  /* Get the class we will actually use for a reload.  */
  rclass = targetm.preferred_reload_class (x, rclass);

  /* If we need a secondary reload for an intermediate, the cost is
     that to load the input into the intermediate register, then to
     copy it.  */
  sri.prev_sri = prev_sri;
  sri.extra_cost = 0;
  secondary_class = targetm.secondary_reload (to_p, x, rclass, mode, &sri);

  if (secondary_class != NO_REGS)
    {
      ira_init_register_move_cost_if_necessary (mode);
      return (ira_register_move_cost[mode][(int) secondary_class][(int) rclass]
	      + sri.extra_cost
	      + copy_cost (x, mode, secondary_class, to_p, &sri));
    }

  /* For memory, use the memory move cost, for (hard) registers, use
     the cost to move between the register classes, and use 2 for
     everything else (constants).  */
  if (MEM_P (x) || rclass == NO_REGS)
    return sri.extra_cost
	   + ira_memory_move_cost[mode][(int) rclass][to_p != 0];
  else if (REG_P (x))
    {
      reg_class_t x_class = REGNO_REG_CLASS (REGNO (x));

      ira_init_register_move_cost_if_necessary (mode);
      return (sri.extra_cost
	      + ira_register_move_cost[mode][(int) x_class][(int) rclass]);
    }
  else
    /* If this is a constant, we may eventually want to call rtx_cost
       here.  */
    return sri.extra_cost + COSTS_N_INSNS (1);
}



/* Record the cost of using memory or hard registers of various
   classes for the operands in INSN.

   N_ALTS is the number of alternatives.
   N_OPS is the number of operands.
   OPS is an array of the operands.
   MODES are the modes of the operands, in case any are VOIDmode.
   CONSTRAINTS are the constraints to use for the operands.  This array
   is modified by this procedure.

   This procedure works alternative by alternative.  For each
   alternative we assume that we will be able to allocate all allocnos
   to their ideal register class and calculate the cost of using that
   alternative.  Then we compute, for each operand that is a
   pseudo-register, the cost of having the allocno allocated to each
   register class and using it in that alternative.  To this cost is
   added the cost of the alternative.

   The cost of each class for this insn is its lowest cost among all
   the alternatives.  */
static void
record_reg_classes (int n_alts, int n_ops, rtx *ops,
		    enum machine_mode *modes, const char **constraints,
		    rtx insn, enum reg_class *pref)
{
  int alt;
  int i, j, k;
  int insn_allows_mem[MAX_RECOG_OPERANDS];
  move_table *move_in_cost, *move_out_cost;
  short (*mem_cost)[2];

  for (i = 0; i < n_ops; i++)
    insn_allows_mem[i] = 0;

  /* Process each alternative, each time minimizing an operand's cost
     with the cost for each operand in that alternative.  */
  for (alt = 0; alt < n_alts; alt++)
    {
      enum reg_class classes[MAX_RECOG_OPERANDS];
      int allows_mem[MAX_RECOG_OPERANDS];
      enum reg_class rclass;
      int alt_fail = 0;
      int alt_cost = 0, op_cost_add;

      if (!TEST_BIT (recog_data.enabled_alternatives, alt))
	{
	  for (i = 0; i < recog_data.n_operands; i++)
	    constraints[i] = skip_alternative (constraints[i]);

	  continue;
	}

      for (i = 0; i < n_ops; i++)
	{
	  unsigned char c;
	  const char *p = constraints[i];
	  rtx op = ops[i];
	  enum machine_mode mode = modes[i];
	  int allows_addr = 0;
	  int win = 0;

	  /* Initially show we know nothing about the register class.  */
	  classes[i] = NO_REGS;
	  allows_mem[i] = 0;

	  /* If this operand has no constraints at all, we can
	     conclude nothing about it since anything is valid.  */
	  if (*p == 0)
	    {
	      if (REG_P (op) && REGNO (op) >= FIRST_PSEUDO_REGISTER)
		memset (this_op_costs[i], 0, struct_costs_size);
	      continue;
	    }

	  /* If this alternative is only relevant when this operand
	     matches a previous operand, we do different things
	     depending on whether this operand is a allocno-reg or not.
	     We must process any modifiers for the operand before we
	     can make this test.  */
	  while (*p == '%' || *p == '=' || *p == '+' || *p == '&')
	    p++;

	  if (p[0] >= '0' && p[0] <= '0' + i && (p[1] == ',' || p[1] == 0))
	    {
	      /* Copy class and whether memory is allowed from the
		 matching alternative.  Then perform any needed cost
		 computations and/or adjustments.  */
	      j = p[0] - '0';
	      classes[i] = classes[j];
	      allows_mem[i] = allows_mem[j];
	      if (allows_mem[i])
		insn_allows_mem[i] = 1;

	      if (! REG_P (op) || REGNO (op) < FIRST_PSEUDO_REGISTER)
		{
		  /* If this matches the other operand, we have no
		     added cost and we win.  */
		  if (rtx_equal_p (ops[j], op))
		    win = 1;
		  /* If we can put the other operand into a register,
		     add to the cost of this alternative the cost to
		     copy this operand to the register used for the
		     other operand.  */
		  else if (classes[j] != NO_REGS)
		    {
		      alt_cost += copy_cost (op, mode, classes[j], 1, NULL);
		      win = 1;
		    }
		}
	      else if (! REG_P (ops[j])
		       || REGNO (ops[j]) < FIRST_PSEUDO_REGISTER)
		{
		  /* This op is an allocno but the one it matches is
		     not.  */

		  /* If we can't put the other operand into a
		     register, this alternative can't be used.  */

		  if (classes[j] == NO_REGS)
		    alt_fail = 1;
		  /* Otherwise, add to the cost of this alternative
		     the cost to copy the other operand to the hard
		     register used for this operand.  */
		  else
		    alt_cost += copy_cost (ops[j], mode, classes[j], 1, NULL);
		}
	      else
		{
		  /* The costs of this operand are not the same as the
		     other operand since move costs are not symmetric.
		     Moreover, if we cannot tie them, this alternative
		     needs to do a copy, which is one insn.  */
		  struct costs *pp = this_op_costs[i];
		  int *pp_costs = pp->cost;
		  cost_classes_t cost_classes_ptr
		    = regno_cost_classes[REGNO (op)];
		  enum reg_class *cost_classes = cost_classes_ptr->classes;
		  bool in_p = recog_data.operand_type[i] != OP_OUT;
		  bool out_p = recog_data.operand_type[i] != OP_IN;
		  enum reg_class op_class = classes[i];

		  ira_init_register_move_cost_if_necessary (mode);
		  if (! in_p)
		    {
		      ira_assert (out_p);
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = mem_cost[rclass][0] * frequency;
			    }
			}
		      else
			{
			  move_out_cost = ira_may_move_out_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k]
				= move_out_cost[op_class][rclass] * frequency;
			    }
			}
		    }
		  else if (! out_p)
		    {
		      ira_assert (in_p);
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = mem_cost[rclass][1] * frequency;
			    }
			}
		      else
			{
			  move_in_cost = ira_may_move_in_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k]
				= move_in_cost[rclass][op_class] * frequency;
			    }
			}
		    }
		  else
		    {
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = ((mem_cost[rclass][0]
					      + mem_cost[rclass][1])
					     * frequency);
			    }
			}
		      else
			{
			  move_in_cost = ira_may_move_in_cost[mode];
			  move_out_cost = ira_may_move_out_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = ((move_in_cost[rclass][op_class]
					      + move_out_cost[op_class][rclass])
					     * frequency);
			    }
			}
		    }

		  /* If the alternative actually allows memory, make
		     things a bit cheaper since we won't need an extra
		     insn to load it.  */
		  pp->mem_cost
		    = ((out_p ? ira_memory_move_cost[mode][op_class][0] : 0)
		       + (in_p ? ira_memory_move_cost[mode][op_class][1] : 0)
		       - allows_mem[i]) * frequency;

		  /* If we have assigned a class to this allocno in
		     our first pass, add a cost to this alternative
		     corresponding to what we would add if this
		     allocno were not in the appropriate class.  */
		  if (pref)
		    {
		      enum reg_class pref_class = pref[COST_INDEX (REGNO (op))];

		      if (pref_class == NO_REGS)
			alt_cost
			  += ((out_p
			       ? ira_memory_move_cost[mode][op_class][0] : 0)
			      + (in_p
				 ? ira_memory_move_cost[mode][op_class][1]
				 : 0));
		      else if (ira_reg_class_intersect
			       [pref_class][op_class] == NO_REGS)
			alt_cost
			  += ira_register_move_cost[mode][pref_class][op_class];
		    }
		  if (REGNO (ops[i]) != REGNO (ops[j])
		      && ! find_reg_note (insn, REG_DEAD, op))
		    alt_cost += 2;

		  /* This is in place of ordinary cost computation for
		     this operand, so skip to the end of the
		     alternative (should be just one character).  */
		  while (*p && *p++ != ',')
		    ;

		  constraints[i] = p;
		  continue;
		}
	    }

	  /* Scan all the constraint letters.  See if the operand
	     matches any of the constraints.  Collect the valid
	     register classes and see if this operand accepts
	     memory.  */
	  while ((c = *p))
	    {
	      switch (c)
		{
		case ',':
		  break;
		case '*':
		  /* Ignore the next letter for this pass.  */
		  c = *++p;
		  break;

		case '?':
		  alt_cost += 2;
		case '!':  case '#':  case '&':
		case '0':  case '1':  case '2':  case '3':  case '4':
		case '5':  case '6':  case '7':  case '8':  case '9':
		  break;

		case 'p':
		  allows_addr = 1;
		  win = address_operand (op, GET_MODE (op));
		  /* We know this operand is an address, so we want it
		     to be allocated to a register that can be the
		     base of an address, i.e. BASE_REG_CLASS.  */
		  classes[i]
		    = ira_reg_class_subunion[classes[i]]
		      [base_reg_class (VOIDmode, ADDR_SPACE_GENERIC,
				       ADDRESS, SCRATCH)];
		  break;

		case 'm':  case 'o':  case 'V':
		  /* It doesn't seem worth distinguishing between
		     offsettable and non-offsettable addresses
		     here.  */
		  insn_allows_mem[i] = allows_mem[i] = 1;
		  if (MEM_P (op))
		    win = 1;
		  break;

		case '<':
		  if (MEM_P (op)
		      && (GET_CODE (XEXP (op, 0)) == PRE_DEC
			  || GET_CODE (XEXP (op, 0)) == POST_DEC))
		    win = 1;
		  break;

		case '>':
		  if (MEM_P (op)
		      && (GET_CODE (XEXP (op, 0)) == PRE_INC
			  || GET_CODE (XEXP (op, 0)) == POST_INC))
		    win = 1;
		  break;

		case 'E':
		case 'F':
		  if (CONST_DOUBLE_AS_FLOAT_P (op) 
		      || (GET_CODE (op) == CONST_VECTOR
			  && (GET_MODE_CLASS (GET_MODE (op))
			      == MODE_VECTOR_FLOAT)))
		    win = 1;
		  break;

		case 'G':
		case 'H':
		  if (CONST_DOUBLE_AS_FLOAT_P (op) 
		      && CONST_DOUBLE_OK_FOR_CONSTRAINT_P (op, c, p))
		    win = 1;
		  break;

		case 's':
		  if (CONST_SCALAR_INT_P (op)) 
		    break;

		case 'i':
		  if (CONSTANT_P (op)
		      && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op)))
		    win = 1;
		  break;

		case 'n':
		  if (CONST_SCALAR_INT_P (op)) 
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
		  if (CONST_INT_P (op)
		      && CONST_OK_FOR_CONSTRAINT_P (INTVAL (op), c, p))
		    win = 1;
		  break;

		case 'X':
		  win = 1;
		  break;

		case 'g':
		  if (MEM_P (op)
		      || (CONSTANT_P (op)
			  && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (op))))
		    win = 1;
		  insn_allows_mem[i] = allows_mem[i] = 1;
		case 'r':
		  classes[i] = ira_reg_class_subunion[classes[i]][GENERAL_REGS];
		  break;

		default:
		  if (REG_CLASS_FROM_CONSTRAINT (c, p) != NO_REGS)
		    classes[i] = ira_reg_class_subunion[classes[i]]
		                 [REG_CLASS_FROM_CONSTRAINT (c, p)];
#ifdef EXTRA_CONSTRAINT_STR
		  else if (EXTRA_CONSTRAINT_STR (op, c, p))
		    win = 1;

		  if (EXTRA_MEMORY_CONSTRAINT (c, p))
		    {
		      /* Every MEM can be reloaded to fit.  */
		      insn_allows_mem[i] = allows_mem[i] = 1;
		      if (MEM_P (op))
			win = 1;
		    }
		  if (EXTRA_ADDRESS_CONSTRAINT (c, p))
		    {
		      /* Every address can be reloaded to fit.  */
		      allows_addr = 1;
		      if (address_operand (op, GET_MODE (op)))
			win = 1;
		      /* We know this operand is an address, so we
			 want it to be allocated to a hard register
			 that can be the base of an address,
			 i.e. BASE_REG_CLASS.  */
		      classes[i]
			= ira_reg_class_subunion[classes[i]]
			  [base_reg_class (VOIDmode, ADDR_SPACE_GENERIC,
					   ADDRESS, SCRATCH)];
		    }
#endif
		  break;
		}
	      p += CONSTRAINT_LEN (c, p);
	      if (c == ',')
		break;
	    }

	  constraints[i] = p;

	  /* How we account for this operand now depends on whether it
	     is a pseudo register or not.  If it is, we first check if
	     any register classes are valid.  If not, we ignore this
	     alternative, since we want to assume that all allocnos get
	     allocated for register preferencing.  If some register
	     class is valid, compute the costs of moving the allocno
	     into that class.  */
	  if (REG_P (op) && REGNO (op) >= FIRST_PSEUDO_REGISTER)
	    {
	      if (classes[i] == NO_REGS && ! allows_mem[i])
		{
		  /* We must always fail if the operand is a REG, but
		     we did not find a suitable class and memory is
		     not allowed.

		     Otherwise we may perform an uninitialized read
		     from this_op_costs after the `continue' statement
		     below.  */
		  alt_fail = 1;
		}
	      else
		{
		  unsigned int regno = REGNO (op);
		  struct costs *pp = this_op_costs[i];
		  int *pp_costs = pp->cost;
		  cost_classes_t cost_classes_ptr = regno_cost_classes[regno];
		  enum reg_class *cost_classes = cost_classes_ptr->classes;
		  bool in_p = recog_data.operand_type[i] != OP_OUT;
		  bool out_p = recog_data.operand_type[i] != OP_IN;
		  enum reg_class op_class = classes[i];

		  ira_init_register_move_cost_if_necessary (mode);
		  if (! in_p)
		    {
		      ira_assert (out_p);
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = mem_cost[rclass][0] * frequency;
			    }
			}
		      else
			{
			  move_out_cost = ira_may_move_out_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k]
				= move_out_cost[op_class][rclass] * frequency;
			    }
			}
		    }
		  else if (! out_p)
		    {
		      ira_assert (in_p);
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = mem_cost[rclass][1] * frequency;
			    }
			}
		      else
			{
			  move_in_cost = ira_may_move_in_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k]
				= move_in_cost[rclass][op_class] * frequency;
			    }
			}
		    }
		  else
		    {
		      if (op_class == NO_REGS)
			{
			  mem_cost = ira_memory_move_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = ((mem_cost[rclass][0]
					      + mem_cost[rclass][1])
					     * frequency);
			    }
			}
		      else
			{
			  move_in_cost = ira_may_move_in_cost[mode];
			  move_out_cost = ira_may_move_out_cost[mode];
			  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			    {
			      rclass = cost_classes[k];
			      pp_costs[k] = ((move_in_cost[rclass][op_class]
					      + move_out_cost[op_class][rclass])
					     * frequency);
			    }
			}
		    }

		  if (op_class == NO_REGS)
		    /* Although we don't need insn to reload from
		       memory, still accessing memory is usually more
		       expensive than a register.  */
		    pp->mem_cost = frequency;
		  else
		    /* If the alternative actually allows memory, make
		       things a bit cheaper since we won't need an
		       extra insn to load it.  */
		    pp->mem_cost
		      = ((out_p ? ira_memory_move_cost[mode][op_class][0] : 0)
			 + (in_p ? ira_memory_move_cost[mode][op_class][1] : 0)
			 - allows_mem[i]) * frequency;
		  /* If we have assigned a class to this allocno in
		     our first pass, add a cost to this alternative
		     corresponding to what we would add if this
		     allocno were not in the appropriate class.  */
		  if (pref)
		    {
		      enum reg_class pref_class = pref[COST_INDEX (REGNO (op))];

		      if (pref_class == NO_REGS)
			{
			  if (op_class != NO_REGS)
			    alt_cost
			      += ((out_p
				   ? ira_memory_move_cost[mode][op_class][0]
				   : 0)
				  + (in_p
				     ? ira_memory_move_cost[mode][op_class][1]
				     : 0));
			}
		      else if (op_class == NO_REGS)
			alt_cost
			  += ((out_p
			       ? ira_memory_move_cost[mode][pref_class][1]
			       : 0)
			      + (in_p
				 ? ira_memory_move_cost[mode][pref_class][0]
				 : 0));
		      else if (ira_reg_class_intersect[pref_class][op_class]
			       == NO_REGS)
			alt_cost += (ira_register_move_cost
				     [mode][pref_class][op_class]);
		    }
		}
	    }

	  /* Otherwise, if this alternative wins, either because we
	     have already determined that or if we have a hard
	     register of the proper class, there is no cost for this
	     alternative.  */
	  else if (win || (REG_P (op)
			   && reg_fits_class_p (op, classes[i],
						0, GET_MODE (op))))
	    ;

	  /* If registers are valid, the cost of this alternative
	     includes copying the object to and/or from a
	     register.  */
	  else if (classes[i] != NO_REGS)
	    {
	      if (recog_data.operand_type[i] != OP_OUT)
		alt_cost += copy_cost (op, mode, classes[i], 1, NULL);

	      if (recog_data.operand_type[i] != OP_IN)
		alt_cost += copy_cost (op, mode, classes[i], 0, NULL);
	    }
	  /* The only other way this alternative can be used is if
	     this is a constant that could be placed into memory.  */
	  else if (CONSTANT_P (op) && (allows_addr || allows_mem[i]))
	    alt_cost += ira_memory_move_cost[mode][classes[i]][1];
	  else
	    alt_fail = 1;
	}

      if (alt_fail)
	continue;

      op_cost_add = alt_cost * frequency;
      /* Finally, update the costs with the information we've
	 calculated about this alternative.  */
      for (i = 0; i < n_ops; i++)
	if (REG_P (ops[i]) && REGNO (ops[i]) >= FIRST_PSEUDO_REGISTER)
	  {
	    struct costs *pp = op_costs[i], *qq = this_op_costs[i];
	    int *pp_costs = pp->cost, *qq_costs = qq->cost;
	    int scale = 1 + (recog_data.operand_type[i] == OP_INOUT);
	    cost_classes_t cost_classes_ptr
	      = regno_cost_classes[REGNO (ops[i])];

	    pp->mem_cost = MIN (pp->mem_cost,
				(qq->mem_cost + op_cost_add) * scale);

	    for (k = cost_classes_ptr->num - 1; k >= 0; k--)
	      pp_costs[k]
		= MIN (pp_costs[k], (qq_costs[k] + op_cost_add) * scale);
	  }
    }

  if (allocno_p)
    for (i = 0; i < n_ops; i++)
      {
	ira_allocno_t a;
	rtx op = ops[i];

	if (! REG_P (op) || REGNO (op) < FIRST_PSEUDO_REGISTER)
	  continue;
	a = ira_curr_regno_allocno_map [REGNO (op)];
	if (! ALLOCNO_BAD_SPILL_P (a) && insn_allows_mem[i] == 0)
	  ALLOCNO_BAD_SPILL_P (a) = true;
      }

}



/* Wrapper around REGNO_OK_FOR_INDEX_P, to allow pseudo registers.  */
static inline bool
ok_for_index_p_nonstrict (rtx reg)
{
  unsigned regno = REGNO (reg);

  return regno >= FIRST_PSEUDO_REGISTER || REGNO_OK_FOR_INDEX_P (regno);
}

/* A version of regno_ok_for_base_p for use here, when all
   pseudo-registers should count as OK.  Arguments as for
   regno_ok_for_base_p.  */
static inline bool
ok_for_base_p_nonstrict (rtx reg, enum machine_mode mode, addr_space_t as,
			 enum rtx_code outer_code, enum rtx_code index_code)
{
  unsigned regno = REGNO (reg);

  if (regno >= FIRST_PSEUDO_REGISTER)
    return true;
  return ok_for_base_p_1 (regno, mode, as, outer_code, index_code);
}

/* Record the pseudo registers we must reload into hard registers in a
   subexpression of a memory address, X.

   If CONTEXT is 0, we are looking at the base part of an address,
   otherwise we are looking at the index part.

   MODE and AS are the mode and address space of the memory reference;
   OUTER_CODE and INDEX_CODE give the context that the rtx appears in.
   These four arguments are passed down to base_reg_class.

   SCALE is twice the amount to multiply the cost by (it is twice so
   we can represent half-cost adjustments).  */
static void
record_address_regs (enum machine_mode mode, addr_space_t as, rtx x,
		     int context, enum rtx_code outer_code,
		     enum rtx_code index_code, int scale)
{
  enum rtx_code code = GET_CODE (x);
  enum reg_class rclass;

  if (context == 1)
    rclass = INDEX_REG_CLASS;
  else
    rclass = base_reg_class (mode, as, outer_code, index_code);

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
      /* When we have an address that is a sum, we must determine
	 whether registers are "base" or "index" regs.  If there is a
	 sum of two registers, we must choose one to be the "base".
	 Luckily, we can use the REG_POINTER to make a good choice
	 most of the time.  We only need to do this on machines that
	 can have two registers in an address and where the base and
	 index register classes are different.

	 ??? This code used to set REGNO_POINTER_FLAG in some cases,
	 but that seems bogus since it should only be set when we are
	 sure the register is being used as a pointer.  */
      {
	rtx arg0 = XEXP (x, 0);
	rtx arg1 = XEXP (x, 1);
	enum rtx_code code0 = GET_CODE (arg0);
	enum rtx_code code1 = GET_CODE (arg1);

	/* Look inside subregs.  */
	if (code0 == SUBREG)
	  arg0 = SUBREG_REG (arg0), code0 = GET_CODE (arg0);
	if (code1 == SUBREG)
	  arg1 = SUBREG_REG (arg1), code1 = GET_CODE (arg1);

	/* If this machine only allows one register per address, it
	   must be in the first operand.  */
	if (MAX_REGS_PER_ADDRESS == 1)
	  record_address_regs (mode, as, arg0, 0, PLUS, code1, scale);

	/* If index and base registers are the same on this machine,
	   just record registers in any non-constant operands.  We
	   assume here, as well as in the tests below, that all
	   addresses are in canonical form.  */
	else if (INDEX_REG_CLASS
		 == base_reg_class (VOIDmode, as, PLUS, SCRATCH))
	  {
	    record_address_regs (mode, as, arg0, context, PLUS, code1, scale);
	    if (! CONSTANT_P (arg1))
	      record_address_regs (mode, as, arg1, context, PLUS, code0, scale);
	  }

	/* If the second operand is a constant integer, it doesn't
	   change what class the first operand must be.  */
	else if (CONST_SCALAR_INT_P (arg1))
	  record_address_regs (mode, as, arg0, context, PLUS, code1, scale);
	/* If the second operand is a symbolic constant, the first
	   operand must be an index register.  */
	else if (code1 == SYMBOL_REF || code1 == CONST || code1 == LABEL_REF)
	  record_address_regs (mode, as, arg0, 1, PLUS, code1, scale);
	/* If both operands are registers but one is already a hard
	   register of index or reg-base class, give the other the
	   class that the hard register is not.  */
	else if (code0 == REG && code1 == REG
		 && REGNO (arg0) < FIRST_PSEUDO_REGISTER
		 && (ok_for_base_p_nonstrict (arg0, mode, as, PLUS, REG)
		     || ok_for_index_p_nonstrict (arg0)))
	  record_address_regs (mode, as, arg1,
			       ok_for_base_p_nonstrict (arg0, mode, as,
							PLUS, REG) ? 1 : 0,
			       PLUS, REG, scale);
	else if (code0 == REG && code1 == REG
		 && REGNO (arg1) < FIRST_PSEUDO_REGISTER
		 && (ok_for_base_p_nonstrict (arg1, mode, as, PLUS, REG)
		     || ok_for_index_p_nonstrict (arg1)))
	  record_address_regs (mode, as, arg0,
			       ok_for_base_p_nonstrict (arg1, mode, as,
							PLUS, REG) ? 1 : 0,
			       PLUS, REG, scale);
	/* If one operand is known to be a pointer, it must be the
	   base with the other operand the index.  Likewise if the
	   other operand is a MULT.  */
	else if ((code0 == REG && REG_POINTER (arg0)) || code1 == MULT)
	  {
	    record_address_regs (mode, as, arg0, 0, PLUS, code1, scale);
	    record_address_regs (mode, as, arg1, 1, PLUS, code0, scale);
	  }
	else if ((code1 == REG && REG_POINTER (arg1)) || code0 == MULT)
	  {
	    record_address_regs (mode, as, arg0, 1, PLUS, code1, scale);
	    record_address_regs (mode, as, arg1, 0, PLUS, code0, scale);
	  }
	/* Otherwise, count equal chances that each might be a base or
	   index register.  This case should be rare.  */
	else
	  {
	    record_address_regs (mode, as, arg0, 0, PLUS, code1, scale / 2);
	    record_address_regs (mode, as, arg0, 1, PLUS, code1, scale / 2);
	    record_address_regs (mode, as, arg1, 0, PLUS, code0, scale / 2);
	    record_address_regs (mode, as, arg1, 1, PLUS, code0, scale / 2);
	  }
      }
      break;

      /* Double the importance of an allocno that is incremented or
	 decremented, since it would take two extra insns if it ends
	 up in the wrong place.  */
    case POST_MODIFY:
    case PRE_MODIFY:
      record_address_regs (mode, as, XEXP (x, 0), 0, code,
			   GET_CODE (XEXP (XEXP (x, 1), 1)), 2 * scale);
      if (REG_P (XEXP (XEXP (x, 1), 1)))
	record_address_regs (mode, as, XEXP (XEXP (x, 1), 1), 1, code, REG,
			     2 * scale);
      break;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
      /* Double the importance of an allocno that is incremented or
	 decremented, since it would take two extra insns if it ends
	 up in the wrong place.  */
      record_address_regs (mode, as, XEXP (x, 0), 0, code, SCRATCH, 2 * scale);
      break;

    case REG:
      {
	struct costs *pp;
	int *pp_costs;
	enum reg_class i;
	int k, regno, add_cost;
	cost_classes_t cost_classes_ptr;
	enum reg_class *cost_classes;
	move_table *move_in_cost;

	if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	  break;

	regno = REGNO (x);
	if (allocno_p)
	  ALLOCNO_BAD_SPILL_P (ira_curr_regno_allocno_map[regno]) = true;
	pp = COSTS (costs, COST_INDEX (regno));
	add_cost = (ira_memory_move_cost[Pmode][rclass][1] * scale) / 2;
	if (INT_MAX - add_cost < pp->mem_cost)
	  pp->mem_cost = INT_MAX;
	else
	  pp->mem_cost += add_cost;
	cost_classes_ptr = regno_cost_classes[regno];
	cost_classes = cost_classes_ptr->classes;
	pp_costs = pp->cost;
	ira_init_register_move_cost_if_necessary (Pmode);
	move_in_cost = ira_may_move_in_cost[Pmode];
	for (k = cost_classes_ptr->num - 1; k >= 0; k--)
	  {
	    i = cost_classes[k];
	    add_cost = (move_in_cost[i][rclass] * scale) / 2;
	    if (INT_MAX - add_cost < pp_costs[k])
	      pp_costs[k] = INT_MAX;
	    else 
	      pp_costs[k] += add_cost;
	  }
      }
      break;

    default:
      {
	const char *fmt = GET_RTX_FORMAT (code);
	int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  if (fmt[i] == 'e')
	    record_address_regs (mode, as, XEXP (x, i), context, code, SCRATCH,
				 scale);
      }
    }
}



/* Calculate the costs of insn operands.  */
static void
record_operand_costs (rtx insn, enum reg_class *pref)
{
  const char *constraints[MAX_RECOG_OPERANDS];
  enum machine_mode modes[MAX_RECOG_OPERANDS];
  rtx ops[MAX_RECOG_OPERANDS];
  rtx set;
  int i;

  for (i = 0; i < recog_data.n_operands; i++)
    {
      constraints[i] = recog_data.constraints[i];
      modes[i] = recog_data.operand_mode[i];
    }

  /* If we get here, we are set up to record the costs of all the
     operands for this insn.  Start by initializing the costs.  Then
     handle any address registers.  Finally record the desired classes
     for any allocnos, doing it twice if some pair of operands are
     commutative.  */
  for (i = 0; i < recog_data.n_operands; i++)
    {
      memcpy (op_costs[i], init_cost, struct_costs_size);

      ops[i] = recog_data.operand[i];
      if (GET_CODE (recog_data.operand[i]) == SUBREG)
	recog_data.operand[i] = SUBREG_REG (recog_data.operand[i]);

      if (MEM_P (recog_data.operand[i]))
	record_address_regs (GET_MODE (recog_data.operand[i]),
			     MEM_ADDR_SPACE (recog_data.operand[i]),
			     XEXP (recog_data.operand[i], 0),
			     0, MEM, SCRATCH, frequency * 2);
      else if (constraints[i][0] == 'p'
	       || EXTRA_ADDRESS_CONSTRAINT (constraints[i][0],
					    constraints[i]))
	record_address_regs (VOIDmode, ADDR_SPACE_GENERIC,
			     recog_data.operand[i], 0, ADDRESS, SCRATCH,
			     frequency * 2);
    }
  
  /* Check for commutative in a separate loop so everything will have
     been initialized.  We must do this even if one operand is a
     constant--see addsi3 in m68k.md.  */
  for (i = 0; i < (int) recog_data.n_operands - 1; i++)
    if (constraints[i][0] == '%')
      {
	const char *xconstraints[MAX_RECOG_OPERANDS];
	int j;

	/* Handle commutative operands by swapping the constraints.
	   We assume the modes are the same.  */
	for (j = 0; j < recog_data.n_operands; j++)
	  xconstraints[j] = constraints[j];

	xconstraints[i] = constraints[i+1];
	xconstraints[i+1] = constraints[i];
	record_reg_classes (recog_data.n_alternatives, recog_data.n_operands,
			    recog_data.operand, modes,
			    xconstraints, insn, pref);
      }
  record_reg_classes (recog_data.n_alternatives, recog_data.n_operands,
		      recog_data.operand, modes,
		      constraints, insn, pref);

  /* If this insn is a single set copying operand 1 to operand 0 and
     one operand is an allocno with the other a hard reg or an allocno
     that prefers a hard register that is in its own register class
     then we may want to adjust the cost of that register class to -1.

     Avoid the adjustment if the source does not die to avoid
     stressing of register allocator by preferrencing two colliding
     registers into single class.

     Also avoid the adjustment if a copy between hard registers of the
     class is expensive (ten times the cost of a default copy is
     considered arbitrarily expensive).  This avoids losing when the
     preferred class is very expensive as the source of a copy
     instruction.  */
  if ((set = single_set (insn)) != NULL_RTX
      /* In rare cases the single set insn might have less 2 operands
	 as the source can be a fixed special reg.  */
      && recog_data.n_operands > 1
      && ops[0] == SET_DEST (set) && ops[1] == SET_SRC (set))
    {
      int regno, other_regno;
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);

      dest = SET_DEST (set);
      src = SET_SRC (set);
      if (GET_CODE (dest) == SUBREG
	  && (GET_MODE_SIZE (GET_MODE (dest))
	      == GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))))
	dest = SUBREG_REG (dest);
      if (GET_CODE (src) == SUBREG
	  && (GET_MODE_SIZE (GET_MODE (src))
	      == GET_MODE_SIZE (GET_MODE (SUBREG_REG (src)))))
	src = SUBREG_REG (src);
      if (REG_P (src) && REG_P (dest)
	  && find_regno_note (insn, REG_DEAD, REGNO (src))
	  && (((regno = REGNO (src)) >= FIRST_PSEUDO_REGISTER
	       && (other_regno = REGNO (dest)) < FIRST_PSEUDO_REGISTER)
	      || ((regno = REGNO (dest)) >= FIRST_PSEUDO_REGISTER
		  && (other_regno = REGNO (src)) < FIRST_PSEUDO_REGISTER)))
	{
	  enum machine_mode mode = GET_MODE (src);
	  cost_classes_t cost_classes_ptr = regno_cost_classes[regno];
	  enum reg_class *cost_classes = cost_classes_ptr->classes;
	  reg_class_t rclass;
	  int k, nr;

	  i = regno == (int) REGNO (src) ? 1 : 0;
	  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
	    {
	      rclass = cost_classes[k];
	      if (TEST_HARD_REG_BIT (reg_class_contents[rclass], other_regno)
		  && (reg_class_size[(int) rclass]
		      == ira_reg_class_max_nregs [(int) rclass][(int) mode]))
		{
		  if (reg_class_size[rclass] == 1)
		    op_costs[i]->cost[k] = -frequency;
		  else
		    {
		      for (nr = 0;
			   nr < hard_regno_nregs[other_regno][mode];
			   nr++)
			if (! TEST_HARD_REG_BIT (reg_class_contents[rclass],
						 other_regno + nr))
			  break;
		      
		      if (nr == hard_regno_nregs[other_regno][mode])
			op_costs[i]->cost[k] = -frequency;
		    }
		}
	    }
	}
    }
}



/* Process one insn INSN.  Scan it and record each time it would save
   code to put a certain allocnos in a certain class.  Return the last
   insn processed, so that the scan can be continued from there.  */
static rtx
scan_one_insn (rtx insn)
{
  enum rtx_code pat_code;
  rtx set, note;
  int i, k;
  bool counted_mem;

  if (!NONDEBUG_INSN_P (insn))
    return insn;

  pat_code = GET_CODE (PATTERN (insn));
  if (pat_code == USE || pat_code == CLOBBER || pat_code == ASM_INPUT)
    return insn;

  counted_mem = false;
  set = single_set (insn);
  extract_insn (insn);

  /* If this insn loads a parameter from its stack slot, then it
     represents a savings, rather than a cost, if the parameter is
     stored in memory.  Record this fact. 

     Similarly if we're loading other constants from memory (constant
     pool, TOC references, small data areas, etc) and this is the only
     assignment to the destination pseudo.

     Don't do this if SET_SRC (set) isn't a general operand, if it is
     a memory requiring special instructions to load it, decreasing
     mem_cost might result in it being loaded using the specialized
     instruction into a register, then stored into stack and loaded
     again from the stack.  See PR52208.
     
     Don't do this if SET_SRC (set) has side effect.  See PR56124.  */
  if (set != 0 && REG_P (SET_DEST (set)) && MEM_P (SET_SRC (set))
      && (note = find_reg_note (insn, REG_EQUIV, NULL_RTX)) != NULL_RTX
      && ((MEM_P (XEXP (note, 0))
	   && !side_effects_p (SET_SRC (set)))
	  || (CONSTANT_P (XEXP (note, 0))
	      && targetm.legitimate_constant_p (GET_MODE (SET_DEST (set)),
						XEXP (note, 0))
	      && REG_N_SETS (REGNO (SET_DEST (set))) == 1))
      && general_operand (SET_SRC (set), GET_MODE (SET_SRC (set))))
    {
      enum reg_class cl = GENERAL_REGS;
      rtx reg = SET_DEST (set);
      int num = COST_INDEX (REGNO (reg));

      COSTS (costs, num)->mem_cost
	-= ira_memory_move_cost[GET_MODE (reg)][cl][1] * frequency;
      record_address_regs (GET_MODE (SET_SRC (set)),
			   MEM_ADDR_SPACE (SET_SRC (set)),
			   XEXP (SET_SRC (set), 0), 0, MEM, SCRATCH,
			   frequency * 2);
      counted_mem = true;
    }

  record_operand_costs (insn, pref);

  /* Now add the cost for each operand to the total costs for its
     allocno.  */
  for (i = 0; i < recog_data.n_operands; i++)
    if (REG_P (recog_data.operand[i])
	&& REGNO (recog_data.operand[i]) >= FIRST_PSEUDO_REGISTER)
      {
	int regno = REGNO (recog_data.operand[i]);
	struct costs *p = COSTS (costs, COST_INDEX (regno));
	struct costs *q = op_costs[i];
	int *p_costs = p->cost, *q_costs = q->cost;
	cost_classes_t cost_classes_ptr = regno_cost_classes[regno];
	int add_cost;

	/* If the already accounted for the memory "cost" above, don't
	   do so again.  */
	if (!counted_mem)
	  {
	    add_cost = q->mem_cost;
	    if (add_cost > 0 && INT_MAX - add_cost < p->mem_cost)
	      p->mem_cost = INT_MAX;
	    else
	      p->mem_cost += add_cost;
	  }
	for (k = cost_classes_ptr->num - 1; k >= 0; k--)
	  {
	    add_cost = q_costs[k];
	    if (add_cost > 0 && INT_MAX - add_cost < p_costs[k])
	      p_costs[k] = INT_MAX;
	    else
	      p_costs[k] += add_cost;
	  }
      }

  return insn;
}



/* Print allocnos costs to file F.  */
static void
print_allocno_costs (FILE *f)
{
  int k;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  ira_assert (allocno_p);
  fprintf (f, "\n");
  FOR_EACH_ALLOCNO (a, ai)
    {
      int i, rclass;
      basic_block bb;
      int regno = ALLOCNO_REGNO (a);
      cost_classes_t cost_classes_ptr = regno_cost_classes[regno];
      enum reg_class *cost_classes = cost_classes_ptr->classes;

      i = ALLOCNO_NUM (a);
      fprintf (f, "  a%d(r%d,", i, regno);
      if ((bb = ALLOCNO_LOOP_TREE_NODE (a)->bb) != NULL)
	fprintf (f, "b%d", bb->index);
      else
	fprintf (f, "l%d", ALLOCNO_LOOP_TREE_NODE (a)->loop_num);
      fprintf (f, ") costs:");
      for (k = 0; k < cost_classes_ptr->num; k++)
	{
	  rclass = cost_classes[k];
	  if (contains_reg_of_mode[rclass][PSEUDO_REGNO_MODE (regno)]
#ifdef CANNOT_CHANGE_MODE_CLASS
	      && ! invalid_mode_change_p (regno, (enum reg_class) rclass)
#endif
	      )
	    {
	      fprintf (f, " %s:%d", reg_class_names[rclass],
		       COSTS (costs, i)->cost[k]);
	      if (flag_ira_region == IRA_REGION_ALL
		  || flag_ira_region == IRA_REGION_MIXED)
		fprintf (f, ",%d", COSTS (total_allocno_costs, i)->cost[k]);
	    }
	}
      fprintf (f, " MEM:%i", COSTS (costs, i)->mem_cost);
      if (flag_ira_region == IRA_REGION_ALL
	  || flag_ira_region == IRA_REGION_MIXED)
	fprintf (f, ",%d", COSTS (total_allocno_costs, i)->mem_cost);
      fprintf (f, "\n");
    }
}

/* Print pseudo costs to file F.  */
static void
print_pseudo_costs (FILE *f)
{
  int regno, k;
  int rclass;
  cost_classes_t cost_classes_ptr;
  enum reg_class *cost_classes;

  ira_assert (! allocno_p);
  fprintf (f, "\n");
  for (regno = max_reg_num () - 1; regno >= FIRST_PSEUDO_REGISTER; regno--)
    {
      if (REG_N_REFS (regno) <= 0)
	continue;
      cost_classes_ptr = regno_cost_classes[regno];
      cost_classes = cost_classes_ptr->classes;
      fprintf (f, "  r%d costs:", regno);
      for (k = 0; k < cost_classes_ptr->num; k++)
	{
	  rclass = cost_classes[k];
	  if (contains_reg_of_mode[rclass][PSEUDO_REGNO_MODE (regno)]
#ifdef CANNOT_CHANGE_MODE_CLASS
	      && ! invalid_mode_change_p (regno, (enum reg_class) rclass)
#endif
	      )
	    fprintf (f, " %s:%d", reg_class_names[rclass],
		     COSTS (costs, regno)->cost[k]);
	}
      fprintf (f, " MEM:%i\n", COSTS (costs, regno)->mem_cost);
    }
}

/* Traverse the BB represented by LOOP_TREE_NODE to update the allocno
   costs.  */
static void
process_bb_for_costs (basic_block bb)
{
  rtx insn;

  frequency = REG_FREQ_FROM_BB (bb);
  if (frequency == 0)
    frequency = 1;
  FOR_BB_INSNS (bb, insn)
    insn = scan_one_insn (insn);
}

/* Traverse the BB represented by LOOP_TREE_NODE to update the allocno
   costs.  */
static void
process_bb_node_for_costs (ira_loop_tree_node_t loop_tree_node)
{
  basic_block bb;

  bb = loop_tree_node->bb;
  if (bb != NULL)
    process_bb_for_costs (bb);
}

/* Find costs of register classes and memory for allocnos or pseudos
   and their best costs.  Set up preferred, alternative and allocno
   classes for pseudos.  */
static void
find_costs_and_classes (FILE *dump_file)
{
  int i, k, start, max_cost_classes_num;
  int pass;
  basic_block bb;
  enum reg_class *regno_best_class;

  init_recog ();
  regno_best_class
    = (enum reg_class *) ira_allocate (max_reg_num ()
				       * sizeof (enum reg_class));
  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
    regno_best_class[i] = NO_REGS;
  if (!resize_reg_info () && allocno_p
      && pseudo_classes_defined_p && flag_expensive_optimizations)
    {
      ira_allocno_t a;
      ira_allocno_iterator ai;

      pref = pref_buffer;
      max_cost_classes_num = 1;
      FOR_EACH_ALLOCNO (a, ai)
	{
	  pref[ALLOCNO_NUM (a)] = reg_preferred_class (ALLOCNO_REGNO (a));
	  setup_regno_cost_classes_by_aclass
	    (ALLOCNO_REGNO (a), pref[ALLOCNO_NUM (a)]);
	  max_cost_classes_num
	    = MAX (max_cost_classes_num,
		   regno_cost_classes[ALLOCNO_REGNO (a)]->num);
	}
      start = 1;
    }
  else
    {
      pref = NULL;
      max_cost_classes_num = ira_important_classes_num;
      for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
	if (regno_reg_rtx[i] != NULL_RTX)
 	  setup_regno_cost_classes_by_mode (i, PSEUDO_REGNO_MODE (i));
	else
	  setup_regno_cost_classes_by_aclass (i, ALL_REGS);
      start = 0;
    }
  if (allocno_p)
    /* Clear the flag for the next compiled function.  */
    pseudo_classes_defined_p = false;
  /* Normally we scan the insns once and determine the best class to
     use for each allocno.  However, if -fexpensive-optimizations are
     on, we do so twice, the second time using the tentative best
     classes to guide the selection.  */
  for (pass = start; pass <= flag_expensive_optimizations; pass++)
    {
      if ((!allocno_p || internal_flag_ira_verbose > 0) && dump_file)
	fprintf (dump_file,
		 "\nPass %i for finding pseudo/allocno costs\n\n", pass);

      if (pass != start)
	{
	  max_cost_classes_num = 1;
	  for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
	    {
	      setup_regno_cost_classes_by_aclass (i, regno_best_class[i]);
	      max_cost_classes_num
		= MAX (max_cost_classes_num, regno_cost_classes[i]->num);
	    }
	}

      struct_costs_size
	= sizeof (struct costs) + sizeof (int) * (max_cost_classes_num - 1);
      /* Zero out our accumulation of the cost of each class for each
	 allocno.  */
      memset (costs, 0, cost_elements_num * struct_costs_size);

      if (allocno_p)
	{
	  /* Scan the instructions and record each time it would save code
	     to put a certain allocno in a certain class.  */
	  ira_traverse_loop_tree (true, ira_loop_tree_root,
				  process_bb_node_for_costs, NULL);

	  memcpy (total_allocno_costs, costs,
		  max_struct_costs_size * ira_allocnos_num);
	}
      else
	{
	  basic_block bb;

	  FOR_EACH_BB_FN (bb, cfun)
	    process_bb_for_costs (bb);
	}

      if (pass == 0)
	pref = pref_buffer;

      /* Now for each allocno look at how desirable each class is and
	 find which class is preferred.  */
      for (i = max_reg_num () - 1; i >= FIRST_PSEUDO_REGISTER; i--)
	{
	  ira_allocno_t a, parent_a;
	  int rclass, a_num, parent_a_num, add_cost;
	  ira_loop_tree_node_t parent;
	  int best_cost, allocno_cost;
	  enum reg_class best, alt_class;
	  cost_classes_t cost_classes_ptr = regno_cost_classes[i];
	  enum reg_class *cost_classes = cost_classes_ptr->classes;
	  int *i_costs = temp_costs->cost;
	  int i_mem_cost;
	  int equiv_savings = regno_equiv_gains[i];

	  if (! allocno_p)
	    {
	      if (regno_reg_rtx[i] == NULL_RTX)
		continue;
	      memcpy (temp_costs, COSTS (costs, i), struct_costs_size);
	      i_mem_cost = temp_costs->mem_cost;
	    }
	  else
	    {
	      if (ira_regno_allocno_map[i] == NULL)
		continue;
	      memset (temp_costs, 0, struct_costs_size);
	      i_mem_cost = 0;
	      /* Find cost of all allocnos with the same regno.  */
	      for (a = ira_regno_allocno_map[i];
		   a != NULL;
		   a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
		{
		  int *a_costs, *p_costs;
		      
		  a_num = ALLOCNO_NUM (a);
		  if ((flag_ira_region == IRA_REGION_ALL
		       || flag_ira_region == IRA_REGION_MIXED)
		      && (parent = ALLOCNO_LOOP_TREE_NODE (a)->parent) != NULL
		      && (parent_a = parent->regno_allocno_map[i]) != NULL
		      /* There are no caps yet.  */
		      && bitmap_bit_p (ALLOCNO_LOOP_TREE_NODE
				       (a)->border_allocnos,
				       ALLOCNO_NUM (a)))
		    {
		      /* Propagate costs to upper levels in the region
			 tree.  */
		      parent_a_num = ALLOCNO_NUM (parent_a);
		      a_costs = COSTS (total_allocno_costs, a_num)->cost;
		      p_costs = COSTS (total_allocno_costs, parent_a_num)->cost;
		      for (k = cost_classes_ptr->num - 1; k >= 0; k--)
			{
			  add_cost = a_costs[k];
			  if (add_cost > 0 && INT_MAX - add_cost < p_costs[k])
			    p_costs[k] = INT_MAX;
			  else
			    p_costs[k] += add_cost;
			}
		      add_cost = COSTS (total_allocno_costs, a_num)->mem_cost;
		      if (add_cost > 0
			  && (INT_MAX - add_cost
			      < COSTS (total_allocno_costs,
				       parent_a_num)->mem_cost))
			COSTS (total_allocno_costs, parent_a_num)->mem_cost
			  = INT_MAX;
		      else
			COSTS (total_allocno_costs, parent_a_num)->mem_cost
			  += add_cost;

		      if (i >= first_moveable_pseudo && i < last_moveable_pseudo)
			COSTS (total_allocno_costs, parent_a_num)->mem_cost = 0;
		    }
		  a_costs = COSTS (costs, a_num)->cost;
		  for (k = cost_classes_ptr->num - 1; k >= 0; k--)
		    {
		      add_cost = a_costs[k];
		      if (add_cost > 0 && INT_MAX - add_cost < i_costs[k])
			i_costs[k] = INT_MAX;
		      else
			i_costs[k] += add_cost;
		    }
		  add_cost = COSTS (costs, a_num)->mem_cost;
		  if (add_cost > 0 && INT_MAX - add_cost < i_mem_cost)
		    i_mem_cost = INT_MAX;
		  else
		    i_mem_cost += add_cost;
		}
	    }
	  if (i >= first_moveable_pseudo && i < last_moveable_pseudo)
	    i_mem_cost = 0;
	  else if (equiv_savings < 0)
	    i_mem_cost = -equiv_savings;
	  else if (equiv_savings > 0)
	    {
	      i_mem_cost = 0;
	      for (k = cost_classes_ptr->num - 1; k >= 0; k--)
		i_costs[k] += equiv_savings;
	    }

	  best_cost = (1 << (HOST_BITS_PER_INT - 2)) - 1;
	  best = ALL_REGS;
	  alt_class = NO_REGS;
	  /* Find best common class for all allocnos with the same
	     regno.  */
	  for (k = 0; k < cost_classes_ptr->num; k++)
	    {
	      rclass = cost_classes[k];
	      /* Ignore classes that are too small or invalid for this
		 operand.  */
	      if (! contains_reg_of_mode[rclass][PSEUDO_REGNO_MODE (i)]
#ifdef CANNOT_CHANGE_MODE_CLASS
		  || invalid_mode_change_p (i, (enum reg_class) rclass)
#endif
		  )
		continue;
	      if (i_costs[k] < best_cost)
		{
		  best_cost = i_costs[k];
		  best = (enum reg_class) rclass;
		}
	      else if (i_costs[k] == best_cost)
		best = ira_reg_class_subunion[best][rclass];
	      if (pass == flag_expensive_optimizations
		  /* We still prefer registers to memory even at this
		     stage if their costs are the same.  We will make
		     a final decision during assigning hard registers
		     when we have all info including more accurate
		     costs which might be affected by assigning hard
		     registers to other pseudos because the pseudos
		     involved in moves can be coalesced.  */
		  && i_costs[k] <= i_mem_cost
		  && (reg_class_size[reg_class_subunion[alt_class][rclass]]
		      > reg_class_size[alt_class]))
		alt_class = reg_class_subunion[alt_class][rclass];
	    }
	  alt_class = ira_allocno_class_translate[alt_class];
	  if (best_cost > i_mem_cost)
	    regno_aclass[i] = NO_REGS;
	  else
	    {
	      /* Make the common class the biggest class of best and
		 alt_class.  */
	      regno_aclass[i]
		= ira_reg_class_superunion[best][alt_class];
	      ira_assert (regno_aclass[i] != NO_REGS
			  && ira_reg_allocno_class_p[regno_aclass[i]]);
	    }
	  if (pass == flag_expensive_optimizations)
	    {
	      if (best_cost > i_mem_cost)
		best = alt_class = NO_REGS;
	      else if (best == alt_class)
		alt_class = NO_REGS;
	      setup_reg_classes (i, best, alt_class, regno_aclass[i]);
	      if ((!allocno_p || internal_flag_ira_verbose > 2)
		  && dump_file != NULL)
		fprintf (dump_file,
			 "    r%d: preferred %s, alternative %s, allocno %s\n",
			 i, reg_class_names[best], reg_class_names[alt_class],
			 reg_class_names[regno_aclass[i]]);
	    }
	  regno_best_class[i] = best;
	  if (! allocno_p)
	    {
	      pref[i] = best_cost > i_mem_cost ? NO_REGS : best;
	      continue;
	    }
	  for (a = ira_regno_allocno_map[i];
	       a != NULL;
	       a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
	    {
	      enum reg_class aclass = regno_aclass[i];
	      int a_num = ALLOCNO_NUM (a);
	      int *total_a_costs = COSTS (total_allocno_costs, a_num)->cost;
	      int *a_costs = COSTS (costs, a_num)->cost;
	
	      if (aclass == NO_REGS)
		best = NO_REGS;
	      else
		{
		  /* Finding best class which is subset of the common
		     class.  */
		  best_cost = (1 << (HOST_BITS_PER_INT - 2)) - 1;
		  allocno_cost = best_cost;
		  best = ALL_REGS;
		  for (k = 0; k < cost_classes_ptr->num; k++)
		    {
		      rclass = cost_classes[k];
		      if (! ira_class_subset_p[rclass][aclass])
			continue;
		      /* Ignore classes that are too small or invalid
			 for this operand.  */
		      if (! contains_reg_of_mode[rclass][PSEUDO_REGNO_MODE (i)]
#ifdef CANNOT_CHANGE_MODE_CLASS
			  || invalid_mode_change_p (i, (enum reg_class) rclass)
#endif
			  )
			;
		      else if (total_a_costs[k] < best_cost)
			{
			  best_cost = total_a_costs[k];
			  allocno_cost = a_costs[k];
			  best = (enum reg_class) rclass;
			}
		      else if (total_a_costs[k] == best_cost)
			{
			  best = ira_reg_class_subunion[best][rclass];
			  allocno_cost = MAX (allocno_cost, a_costs[k]);
			}
		    }
		  ALLOCNO_CLASS_COST (a) = allocno_cost;
		}
	      if (internal_flag_ira_verbose > 2 && dump_file != NULL
		  && (pass == 0 || pref[a_num] != best))
		{
		  fprintf (dump_file, "    a%d (r%d,", a_num, i);
		  if ((bb = ALLOCNO_LOOP_TREE_NODE (a)->bb) != NULL)
		    fprintf (dump_file, "b%d", bb->index);
		  else
		    fprintf (dump_file, "l%d",
			     ALLOCNO_LOOP_TREE_NODE (a)->loop_num);
		  fprintf (dump_file, ") best %s, allocno %s\n",
			   reg_class_names[best],
			   reg_class_names[aclass]);
		}
	      pref[a_num] = best;
	      if (pass == flag_expensive_optimizations && best != aclass
		  && ira_class_hard_regs_num[best] > 0
		  && (ira_reg_class_max_nregs[best][ALLOCNO_MODE (a)]
		      >= ira_class_hard_regs_num[best]))
		{
		  int ind = cost_classes_ptr->index[aclass];

		  ira_assert (ind >= 0);
		  ira_init_register_move_cost_if_necessary (ALLOCNO_MODE (a));
		  ira_add_allocno_pref (a, ira_class_hard_regs[best][0],
					(a_costs[ind] - ALLOCNO_CLASS_COST (a))
					/ (ira_register_move_cost
					   [ALLOCNO_MODE (a)][best][aclass]));
		  for (k = 0; k < cost_classes_ptr->num; k++)
		    if (ira_class_subset_p[cost_classes[k]][best])
		      a_costs[k] = a_costs[ind];
		}
	    }
	}
      
      if (internal_flag_ira_verbose > 4 && dump_file)
	{
	  if (allocno_p)
	    print_allocno_costs (dump_file);
	  else
	    print_pseudo_costs (dump_file);
	  fprintf (dump_file,"\n");
	}
    }
  ira_free (regno_best_class);
}



/* Process moves involving hard regs to modify allocno hard register
   costs.  We can do this only after determining allocno class.  If a
   hard register forms a register class, then moves with the hard
   register are already taken into account in class costs for the
   allocno.  */
static void
process_bb_node_for_hard_reg_moves (ira_loop_tree_node_t loop_tree_node)
{
  int i, freq, src_regno, dst_regno, hard_regno, a_regno;
  bool to_p;
  ira_allocno_t a, curr_a;
  ira_loop_tree_node_t curr_loop_tree_node;
  enum reg_class rclass;
  basic_block bb;
  rtx insn, set, src, dst;

  bb = loop_tree_node->bb;
  if (bb == NULL)
    return;
  freq = REG_FREQ_FROM_BB (bb);
  if (freq == 0)
    freq = 1;
  FOR_BB_INSNS (bb, insn)
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;
      set = single_set (insn);
      if (set == NULL_RTX)
	continue;
      dst = SET_DEST (set);
      src = SET_SRC (set);
      if (! REG_P (dst) || ! REG_P (src))
	continue;
      dst_regno = REGNO (dst);
      src_regno = REGNO (src);
      if (dst_regno >= FIRST_PSEUDO_REGISTER
	  && src_regno < FIRST_PSEUDO_REGISTER)
	{
	  hard_regno = src_regno;
	  a = ira_curr_regno_allocno_map[dst_regno];
	  to_p = true;
	}
      else if (src_regno >= FIRST_PSEUDO_REGISTER
	       && dst_regno < FIRST_PSEUDO_REGISTER)
	{
	  hard_regno = dst_regno;
	  a = ira_curr_regno_allocno_map[src_regno];
	  to_p = false;
	}
      else
	continue;
      rclass = ALLOCNO_CLASS (a);
      if (! TEST_HARD_REG_BIT (reg_class_contents[rclass], hard_regno))
	continue;
      i = ira_class_hard_reg_index[rclass][hard_regno];
      if (i < 0)
	continue;
      a_regno = ALLOCNO_REGNO (a);
      for (curr_loop_tree_node = ALLOCNO_LOOP_TREE_NODE (a);
	   curr_loop_tree_node != NULL;
	   curr_loop_tree_node = curr_loop_tree_node->parent)
	if ((curr_a = curr_loop_tree_node->regno_allocno_map[a_regno]) != NULL)
	  ira_add_allocno_pref (curr_a, hard_regno, freq);
      {
	int cost;
	enum reg_class hard_reg_class;
	enum machine_mode mode;
	
	mode = ALLOCNO_MODE (a);
	hard_reg_class = REGNO_REG_CLASS (hard_regno);
	ira_init_register_move_cost_if_necessary (mode);
	cost = (to_p ? ira_register_move_cost[mode][hard_reg_class][rclass]
		: ira_register_move_cost[mode][rclass][hard_reg_class]) * freq;
	ira_allocate_and_set_costs (&ALLOCNO_HARD_REG_COSTS (a), rclass,
				    ALLOCNO_CLASS_COST (a));
	ira_allocate_and_set_costs (&ALLOCNO_CONFLICT_HARD_REG_COSTS (a),
				    rclass, 0);
	ALLOCNO_HARD_REG_COSTS (a)[i] -= cost;
	ALLOCNO_CONFLICT_HARD_REG_COSTS (a)[i] -= cost;
	ALLOCNO_CLASS_COST (a) = MIN (ALLOCNO_CLASS_COST (a),
				      ALLOCNO_HARD_REG_COSTS (a)[i]);
      }
    }
}

/* After we find hard register and memory costs for allocnos, define
   its class and modify hard register cost because insns moving
   allocno to/from hard registers.  */
static void
setup_allocno_class_and_costs (void)
{
  int i, j, n, regno, hard_regno, num;
  int *reg_costs;
  enum reg_class aclass, rclass;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  cost_classes_t cost_classes_ptr;

  ira_assert (allocno_p);
  FOR_EACH_ALLOCNO (a, ai)
    {
      i = ALLOCNO_NUM (a);
      regno = ALLOCNO_REGNO (a);
      aclass = regno_aclass[regno];
      cost_classes_ptr = regno_cost_classes[regno];
      ira_assert (pref[i] == NO_REGS || aclass != NO_REGS);
      ALLOCNO_MEMORY_COST (a) = COSTS (costs, i)->mem_cost;
      ira_set_allocno_class (a, aclass);
      if (aclass == NO_REGS)
	continue;
      if (optimize && ALLOCNO_CLASS (a) != pref[i])
	{
	  n = ira_class_hard_regs_num[aclass];
	  ALLOCNO_HARD_REG_COSTS (a)
	    = reg_costs = ira_allocate_cost_vector (aclass);
	  for (j = n - 1; j >= 0; j--)
	    {
	      hard_regno = ira_class_hard_regs[aclass][j];
	      if (TEST_HARD_REG_BIT (reg_class_contents[pref[i]], hard_regno))
		reg_costs[j] = ALLOCNO_CLASS_COST (a);
	      else
		{
		  rclass = REGNO_REG_CLASS (hard_regno);
		  num = cost_classes_ptr->index[rclass];
		  if (num < 0)
		    {
		      num = cost_classes_ptr->hard_regno_index[hard_regno];
		      ira_assert (num >= 0);
		    }
		  reg_costs[j] = COSTS (costs, i)->cost[num];
		}
	    }
	}
    }
  if (optimize)
    ira_traverse_loop_tree (true, ira_loop_tree_root,
			    process_bb_node_for_hard_reg_moves, NULL);
}



/* Function called once during compiler work.  */
void
ira_init_costs_once (void)
{
  int i;

  init_cost = NULL;
  for (i = 0; i < MAX_RECOG_OPERANDS; i++)
    {
      op_costs[i] = NULL;
      this_op_costs[i] = NULL;
    }
  temp_costs = NULL;
}

/* Free allocated temporary cost vectors.  */
static void
free_ira_costs (void)
{
  int i;

  free (init_cost);
  init_cost = NULL;
  for (i = 0; i < MAX_RECOG_OPERANDS; i++)
    {
      free (op_costs[i]);
      free (this_op_costs[i]);
      op_costs[i] = this_op_costs[i] = NULL;
    }
  free (temp_costs);
  temp_costs = NULL;
}

/* This is called each time register related information is
   changed.  */
void
ira_init_costs (void)
{
  int i;

  free_ira_costs ();
  max_struct_costs_size
    = sizeof (struct costs) + sizeof (int) * (ira_important_classes_num - 1);
  /* Don't use ira_allocate because vectors live through several IRA
     calls.  */
  init_cost = (struct costs *) xmalloc (max_struct_costs_size);
  init_cost->mem_cost = 1000000;
  for (i = 0; i < ira_important_classes_num; i++)
    init_cost->cost[i] = 1000000;
  for (i = 0; i < MAX_RECOG_OPERANDS; i++)
    {
      op_costs[i] = (struct costs *) xmalloc (max_struct_costs_size);
      this_op_costs[i] = (struct costs *) xmalloc (max_struct_costs_size);
    }
  temp_costs = (struct costs *) xmalloc (max_struct_costs_size);
}

/* Function called once at the end of compiler work.  */
void
ira_finish_costs_once (void)
{
  free_ira_costs ();
}



/* Common initialization function for ira_costs and
   ira_set_pseudo_classes.  */
static void
init_costs (void)
{
  init_subregs_of_mode ();
  costs = (struct costs *) ira_allocate (max_struct_costs_size
					 * cost_elements_num);
  pref_buffer = (enum reg_class *) ira_allocate (sizeof (enum reg_class)
						 * cost_elements_num);
  regno_aclass = (enum reg_class *) ira_allocate (sizeof (enum reg_class)
						 * max_reg_num ());
  regno_equiv_gains = (int *) ira_allocate (sizeof (int) * max_reg_num ());
  memset (regno_equiv_gains, 0, sizeof (int) * max_reg_num ());
}

/* Common finalization function for ira_costs and
   ira_set_pseudo_classes.  */
static void
finish_costs (void)
{
  finish_subregs_of_mode ();
  ira_free (regno_equiv_gains);
  ira_free (regno_aclass);
  ira_free (pref_buffer);
  ira_free (costs);
}

/* Entry function which defines register class, memory and hard
   register costs for each allocno.  */
void
ira_costs (void)
{
  allocno_p = true;
  cost_elements_num = ira_allocnos_num;
  init_costs ();
  total_allocno_costs = (struct costs *) ira_allocate (max_struct_costs_size
						       * ira_allocnos_num);
  initiate_regno_cost_classes ();
  calculate_elim_costs_all_insns ();
  find_costs_and_classes (ira_dump_file);
  setup_allocno_class_and_costs ();
  finish_regno_cost_classes ();
  finish_costs ();
  ira_free (total_allocno_costs);
}

/* Entry function which defines classes for pseudos.
   Set pseudo_classes_defined_p only if DEFINE_PSEUDO_CLASSES is true.  */
void
ira_set_pseudo_classes (bool define_pseudo_classes, FILE *dump_file)
{
  allocno_p = false;
  internal_flag_ira_verbose = flag_ira_verbose;
  cost_elements_num = max_reg_num ();
  init_costs ();
  initiate_regno_cost_classes ();
  find_costs_and_classes (dump_file);
  finish_regno_cost_classes ();
  if (define_pseudo_classes)
    pseudo_classes_defined_p = true;

  finish_costs ();
}



/* Change hard register costs for allocnos which lives through
   function calls.  This is called only when we found all intersected
   calls during building allocno live ranges.  */
void
ira_tune_allocno_costs (void)
{
  int j, n, regno;
  int cost, min_cost, *reg_costs;
  enum reg_class aclass, rclass;
  enum machine_mode mode;
  ira_allocno_t a;
  ira_allocno_iterator ai;
  ira_allocno_object_iterator oi;
  ira_object_t obj;
  bool skip_p;
  HARD_REG_SET *crossed_calls_clobber_regs;

  FOR_EACH_ALLOCNO (a, ai)
    {
      aclass = ALLOCNO_CLASS (a);
      if (aclass == NO_REGS)
	continue;
      mode = ALLOCNO_MODE (a);
      n = ira_class_hard_regs_num[aclass];
      min_cost = INT_MAX;
      if (ALLOCNO_CALLS_CROSSED_NUM (a)
	  != ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a))
	{
	  ira_allocate_and_set_costs
	    (&ALLOCNO_HARD_REG_COSTS (a), aclass,
	     ALLOCNO_CLASS_COST (a));
	  reg_costs = ALLOCNO_HARD_REG_COSTS (a);
	  for (j = n - 1; j >= 0; j--)
	    {
	      regno = ira_class_hard_regs[aclass][j];
	      skip_p = false;
	      FOR_EACH_ALLOCNO_OBJECT (a, obj, oi)
		{
		  if (ira_hard_reg_set_intersection_p (regno, mode,
						       OBJECT_CONFLICT_HARD_REGS
						       (obj)))
		    {
		      skip_p = true;
		      break;
		    }
		}
	      if (skip_p)
		continue;
	      rclass = REGNO_REG_CLASS (regno);
	      cost = 0;
	      crossed_calls_clobber_regs
		= &(ALLOCNO_CROSSED_CALLS_CLOBBERED_REGS (a));
	      if (ira_hard_reg_set_intersection_p (regno, mode,
						   *crossed_calls_clobber_regs))
		{
		  if (ira_hard_reg_set_intersection_p (regno, mode,
						       call_used_reg_set)
		      || HARD_REGNO_CALL_PART_CLOBBERED (regno, mode))
		    cost += (ALLOCNO_CALL_FREQ (a)
			     * (ira_memory_move_cost[mode][rclass][0]
				+ ira_memory_move_cost[mode][rclass][1]));
#ifdef IRA_HARD_REGNO_ADD_COST_MULTIPLIER
		  cost += ((ira_memory_move_cost[mode][rclass][0]
			    + ira_memory_move_cost[mode][rclass][1])
			   * ALLOCNO_FREQ (a)
			   * IRA_HARD_REGNO_ADD_COST_MULTIPLIER (regno) / 2);
#endif
		}
	      if (INT_MAX - cost < reg_costs[j])
		reg_costs[j] = INT_MAX;
	      else
		reg_costs[j] += cost;
	      if (min_cost > reg_costs[j])
		min_cost = reg_costs[j];
	    }
	}
      if (min_cost != INT_MAX)
	ALLOCNO_CLASS_COST (a) = min_cost;

      /* Some targets allow pseudos to be allocated to unaligned sequences
	 of hard registers.  However, selecting an unaligned sequence can
	 unnecessarily restrict later allocations.  So increase the cost of
	 unaligned hard regs to encourage the use of aligned hard regs.  */
      {
	const int nregs = ira_reg_class_max_nregs[aclass][ALLOCNO_MODE (a)];

	if (nregs > 1)
	  {
	    ira_allocate_and_set_costs
	      (&ALLOCNO_HARD_REG_COSTS (a), aclass, ALLOCNO_CLASS_COST (a));
	    reg_costs = ALLOCNO_HARD_REG_COSTS (a);
	    for (j = n - 1; j >= 0; j--)
	      {
		regno = ira_non_ordered_class_hard_regs[aclass][j];
		if ((regno % nregs) != 0)
		  {
		    int index = ira_class_hard_reg_index[aclass][regno];
		    ira_assert (index != -1);
		    reg_costs[index] += ALLOCNO_FREQ (a);
		  }
	      }
	  }
      }
    }
}

/* Add COST to the estimated gain for eliminating REGNO with its
   equivalence.  If COST is zero, record that no such elimination is
   possible.  */

void
ira_adjust_equiv_reg_cost (unsigned regno, int cost)
{
  if (cost == 0)
    regno_equiv_gains[regno] = 0;
  else
    regno_equiv_gains[regno] += cost;
}
