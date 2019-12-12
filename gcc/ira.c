/* Integrated Register Allocator (IRA) entry point.
   Copyright (C) 2006-2019 Free Software Foundation, Inc.
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

/* The integrated register allocator (IRA) is a
   regional register allocator performing graph coloring on a top-down
   traversal of nested regions.  Graph coloring in a region is based
   on Chaitin-Briggs algorithm.  It is called integrated because
   register coalescing, register live range splitting, and choosing a
   better hard register are done on-the-fly during coloring.  Register
   coalescing and choosing a cheaper hard register is done by hard
   register preferencing during hard register assigning.  The live
   range splitting is a byproduct of the regional register allocation.

   Major IRA notions are:

     o *Region* is a part of CFG where graph coloring based on
       Chaitin-Briggs algorithm is done.  IRA can work on any set of
       nested CFG regions forming a tree.  Currently the regions are
       the entire function for the root region and natural loops for
       the other regions.  Therefore data structure representing a
       region is called loop_tree_node.

     o *Allocno class* is a register class used for allocation of
       given allocno.  It means that only hard register of given
       register class can be assigned to given allocno.  In reality,
       even smaller subset of (*profitable*) hard registers can be
       assigned.  In rare cases, the subset can be even smaller
       because our modification of Chaitin-Briggs algorithm requires
       that sets of hard registers can be assigned to allocnos forms a
       forest, i.e. the sets can be ordered in a way where any
       previous set is not intersected with given set or is a superset
       of given set.

     o *Pressure class* is a register class belonging to a set of
       register classes containing all of the hard-registers available
       for register allocation.  The set of all pressure classes for a
       target is defined in the corresponding machine-description file
       according some criteria.  Register pressure is calculated only
       for pressure classes and it affects some IRA decisions as
       forming allocation regions.

     o *Allocno* represents the live range of a pseudo-register in a
       region.  Besides the obvious attributes like the corresponding
       pseudo-register number, allocno class, conflicting allocnos and
       conflicting hard-registers, there are a few allocno attributes
       which are important for understanding the allocation algorithm:

       - *Live ranges*.  This is a list of ranges of *program points*
         where the allocno lives.  Program points represent places
         where a pseudo can be born or become dead (there are
         approximately two times more program points than the insns)
         and they are represented by integers starting with 0.  The
         live ranges are used to find conflicts between allocnos.
         They also play very important role for the transformation of
         the IRA internal representation of several regions into a one
         region representation.  The later is used during the reload
         pass work because each allocno represents all of the
         corresponding pseudo-registers.

       - *Hard-register costs*.  This is a vector of size equal to the
         number of available hard-registers of the allocno class.  The
         cost of a callee-clobbered hard-register for an allocno is
         increased by the cost of save/restore code around the calls
         through the given allocno's life.  If the allocno is a move
         instruction operand and another operand is a hard-register of
         the allocno class, the cost of the hard-register is decreased
         by the move cost.

         When an allocno is assigned, the hard-register with minimal
         full cost is used.  Initially, a hard-register's full cost is
         the corresponding value from the hard-register's cost vector.
         If the allocno is connected by a *copy* (see below) to
         another allocno which has just received a hard-register, the
         cost of the hard-register is decreased.  Before choosing a
         hard-register for an allocno, the allocno's current costs of
         the hard-registers are modified by the conflict hard-register
         costs of all of the conflicting allocnos which are not
         assigned yet.

       - *Conflict hard-register costs*.  This is a vector of the same
         size as the hard-register costs vector.  To permit an
         unassigned allocno to get a better hard-register, IRA uses
         this vector to calculate the final full cost of the
         available hard-registers.  Conflict hard-register costs of an
         unassigned allocno are also changed with a change of the
         hard-register cost of the allocno when a copy involving the
         allocno is processed as described above.  This is done to
         show other unassigned allocnos that a given allocno prefers
         some hard-registers in order to remove the move instruction
         corresponding to the copy.

     o *Cap*.  If a pseudo-register does not live in a region but
       lives in a nested region, IRA creates a special allocno called
       a cap in the outer region.  A region cap is also created for a
       subregion cap.

     o *Copy*.  Allocnos can be connected by copies.  Copies are used
       to modify hard-register costs for allocnos during coloring.
       Such modifications reflects a preference to use the same
       hard-register for the allocnos connected by copies.  Usually
       copies are created for move insns (in this case it results in
       register coalescing).  But IRA also creates copies for operands
       of an insn which should be assigned to the same hard-register
       due to constraints in the machine description (it usually
       results in removing a move generated in reload to satisfy
       the constraints) and copies referring to the allocno which is
       the output operand of an instruction and the allocno which is
       an input operand dying in the instruction (creation of such
       copies results in less register shuffling).  IRA *does not*
       create copies between the same register allocnos from different
       regions because we use another technique for propagating
       hard-register preference on the borders of regions.

   Allocnos (including caps) for the upper region in the region tree
   *accumulate* information important for coloring from allocnos with
   the same pseudo-register from nested regions.  This includes
   hard-register and memory costs, conflicts with hard-registers,
   allocno conflicts, allocno copies and more.  *Thus, attributes for
   allocnos in a region have the same values as if the region had no
   subregions*.  It means that attributes for allocnos in the
   outermost region corresponding to the function have the same values
   as though the allocation used only one region which is the entire
   function.  It also means that we can look at IRA work as if the
   first IRA did allocation for all function then it improved the
   allocation for loops then their subloops and so on.

   IRA major passes are:

     o Building IRA internal representation which consists of the
       following subpasses:

       * First, IRA builds regions and creates allocnos (file
         ira-build.c) and initializes most of their attributes.

       * Then IRA finds an allocno class for each allocno and
         calculates its initial (non-accumulated) cost of memory and
         each hard-register of its allocno class (file ira-cost.c).

       * IRA creates live ranges of each allocno, calculates register
         pressure for each pressure class in each region, sets up
         conflict hard registers for each allocno and info about calls
         the allocno lives through (file ira-lives.c).

       * IRA removes low register pressure loops from the regions
         mostly to speed IRA up (file ira-build.c).

       * IRA propagates accumulated allocno info from lower region
         allocnos to corresponding upper region allocnos (file
         ira-build.c).

       * IRA creates all caps (file ira-build.c).

       * Having live-ranges of allocnos and their classes, IRA creates
         conflicting allocnos for each allocno.  Conflicting allocnos
         are stored as a bit vector or array of pointers to the
         conflicting allocnos whatever is more profitable (file
         ira-conflicts.c).  At this point IRA creates allocno copies.

     o Coloring.  Now IRA has all necessary info to start graph coloring
       process.  It is done in each region on top-down traverse of the
       region tree (file ira-color.c).  There are following subpasses:

       * Finding profitable hard registers of corresponding allocno
         class for each allocno.  For example, only callee-saved hard
         registers are frequently profitable for allocnos living
         through colors.  If the profitable hard register set of
         allocno does not form a tree based on subset relation, we use
         some approximation to form the tree.  This approximation is
         used to figure out trivial colorability of allocnos.  The
         approximation is a pretty rare case.

       * Putting allocnos onto the coloring stack.  IRA uses Briggs
         optimistic coloring which is a major improvement over
         Chaitin's coloring.  Therefore IRA does not spill allocnos at
         this point.  There is some freedom in the order of putting
         allocnos on the stack which can affect the final result of
         the allocation.  IRA uses some heuristics to improve the
         order.  The major one is to form *threads* from colorable
         allocnos and push them on the stack by threads.  Thread is a
         set of non-conflicting colorable allocnos connected by
         copies.  The thread contains allocnos from the colorable
         bucket or colorable allocnos already pushed onto the coloring
         stack.  Pushing thread allocnos one after another onto the
         stack increases chances of removing copies when the allocnos
         get the same hard reg.
	 
	 We also use a modification of Chaitin-Briggs algorithm which
         works for intersected register classes of allocnos.  To
         figure out trivial colorability of allocnos, the mentioned
         above tree of hard register sets is used.  To get an idea how
         the algorithm works in i386 example, let us consider an
         allocno to which any general hard register can be assigned.
         If the allocno conflicts with eight allocnos to which only
         EAX register can be assigned, given allocno is still
         trivially colorable because all conflicting allocnos might be
         assigned only to EAX and all other general hard registers are
         still free.

	 To get an idea of the used trivial colorability criterion, it
	 is also useful to read article "Graph-Coloring Register
	 Allocation for Irregular Architectures" by Michael D. Smith
	 and Glen Holloway.  Major difference between the article
	 approach and approach used in IRA is that Smith's approach
	 takes register classes only from machine description and IRA
	 calculate register classes from intermediate code too
	 (e.g. an explicit usage of hard registers in RTL code for
	 parameter passing can result in creation of additional
	 register classes which contain or exclude the hard
	 registers).  That makes IRA approach useful for improving
	 coloring even for architectures with regular register files
	 and in fact some benchmarking shows the improvement for
	 regular class architectures is even bigger than for irregular
	 ones.  Another difference is that Smith's approach chooses
	 intersection of classes of all insn operands in which a given
	 pseudo occurs.  IRA can use bigger classes if it is still
	 more profitable than memory usage.

       * Popping the allocnos from the stack and assigning them hard
         registers.  If IRA cannot assign a hard register to an
         allocno and the allocno is coalesced, IRA undoes the
         coalescing and puts the uncoalesced allocnos onto the stack in
         the hope that some such allocnos will get a hard register
         separately.  If IRA fails to assign hard register or memory
         is more profitable for it, IRA spills the allocno.  IRA
         assigns the allocno the hard-register with minimal full
         allocation cost which reflects the cost of usage of the
         hard-register for the allocno and cost of usage of the
         hard-register for allocnos conflicting with given allocno.

       * Chaitin-Briggs coloring assigns as many pseudos as possible
         to hard registers.  After coloring we try to improve
         allocation with cost point of view.  We improve the
         allocation by spilling some allocnos and assigning the freed
         hard registers to other allocnos if it decreases the overall
         allocation cost.

       * After allocno assigning in the region, IRA modifies the hard
         register and memory costs for the corresponding allocnos in
         the subregions to reflect the cost of possible loads, stores,
         or moves on the border of the region and its subregions.
         When default regional allocation algorithm is used
         (-fira-algorithm=mixed), IRA just propagates the assignment
         for allocnos if the register pressure in the region for the
         corresponding pressure class is less than number of available
         hard registers for given pressure class.

     o Spill/restore code moving.  When IRA performs an allocation
       by traversing regions in top-down order, it does not know what
       happens below in the region tree.  Therefore, sometimes IRA
       misses opportunities to perform a better allocation.  A simple
       optimization tries to improve allocation in a region having
       subregions and containing in another region.  If the
       corresponding allocnos in the subregion are spilled, it spills
       the region allocno if it is profitable.  The optimization
       implements a simple iterative algorithm performing profitable
       transformations while they are still possible.  It is fast in
       practice, so there is no real need for a better time complexity
       algorithm.

     o Code change.  After coloring, two allocnos representing the
       same pseudo-register outside and inside a region respectively
       may be assigned to different locations (hard-registers or
       memory).  In this case IRA creates and uses a new
       pseudo-register inside the region and adds code to move allocno
       values on the region's borders.  This is done during top-down
       traversal of the regions (file ira-emit.c).  In some
       complicated cases IRA can create a new allocno to move allocno
       values (e.g. when a swap of values stored in two hard-registers
       is needed).  At this stage, the new allocno is marked as
       spilled.  IRA still creates the pseudo-register and the moves
       on the region borders even when both allocnos were assigned to
       the same hard-register.  If the reload pass spills a
       pseudo-register for some reason, the effect will be smaller
       because another allocno will still be in the hard-register.  In
       most cases, this is better then spilling both allocnos.  If
       reload does not change the allocation for the two
       pseudo-registers, the trivial move will be removed by
       post-reload optimizations.  IRA does not generate moves for
       allocnos assigned to the same hard register when the default
       regional allocation algorithm is used and the register pressure
       in the region for the corresponding pressure class is less than
       number of available hard registers for given pressure class.
       IRA also does some optimizations to remove redundant stores and
       to reduce code duplication on the region borders.

     o Flattening internal representation.  After changing code, IRA
       transforms its internal representation for several regions into
       one region representation (file ira-build.c).  This process is
       called IR flattening.  Such process is more complicated than IR
       rebuilding would be, but is much faster.

     o After IR flattening, IRA tries to assign hard registers to all
       spilled allocnos.  This is implemented by a simple and fast
       priority coloring algorithm (see function
       ira_reassign_conflict_allocnos::ira-color.c).  Here new allocnos
       created during the code change pass can be assigned to hard
       registers.

     o At the end IRA calls the reload pass.  The reload pass
       communicates with IRA through several functions in file
       ira-color.c to improve its decisions in

       * sharing stack slots for the spilled pseudos based on IRA info
         about pseudo-register conflicts.

       * reassigning hard-registers to all spilled pseudos at the end
         of each reload iteration.

       * choosing a better hard-register to spill based on IRA info
         about pseudo-register live ranges and the register pressure
         in places where the pseudo-register lives.

   IRA uses a lot of data representing the target processors.  These
   data are initialized in file ira.c.

   If function has no loops (or the loops are ignored when
   -fira-algorithm=CB is used), we have classic Chaitin-Briggs
   coloring (only instead of separate pass of coalescing, we use hard
   register preferencing).  In such case, IRA works much faster
   because many things are not made (like IR flattening, the
   spill/restore optimization, and the code change).

   Literature is worth to read for better understanding the code:

   o Preston Briggs, Keith D. Cooper, Linda Torczon.  Improvements to
     Graph Coloring Register Allocation.

   o David Callahan, Brian Koblenz.  Register allocation via
     hierarchical graph coloring.

   o Keith Cooper, Anshuman Dasgupta, Jason Eckhardt. Revisiting Graph
     Coloring Register Allocation: A Study of the Chaitin-Briggs and
     Callahan-Koblenz Algorithms.

   o Guei-Yuan Lueh, Thomas Gross, and Ali-Reza Adl-Tabatabai. Global
     Register Allocation Based on Graph Fusion.

   o Michael D. Smith and Glenn Holloway.  Graph-Coloring Register
     Allocation for Irregular Architectures

   o Vladimir Makarov. The Integrated Register Allocator for GCC.

   o Vladimir Makarov.  The top-down register allocator for irregular
     register file architectures.

*/


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "ira.h"
#include "ira-int.h"
#include "diagnostic-core.h"
#include "cfgrtl.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "expr.h"
#include "tree-pass.h"
#include "output.h"
#include "reload.h"
#include "cfgloop.h"
#include "lra.h"
#include "dce.h"
#include "dbgcnt.h"
#include "rtl-iter.h"
#include "shrink-wrap.h"
#include "print-rtl.h"

struct target_ira default_target_ira;
class target_ira_int default_target_ira_int;
#if SWITCHABLE_TARGET
struct target_ira *this_target_ira = &default_target_ira;
class target_ira_int *this_target_ira_int = &default_target_ira_int;
#endif

/* A modified value of flag `-fira-verbose' used internally.  */
int internal_flag_ira_verbose;

/* Dump file of the allocator if it is not NULL.  */
FILE *ira_dump_file;

/* The number of elements in the following array.  */
int ira_spilled_reg_stack_slots_num;

/* The following array contains info about spilled pseudo-registers
   stack slots used in current function so far.  */
class ira_spilled_reg_stack_slot *ira_spilled_reg_stack_slots;

/* Correspondingly overall cost of the allocation, overall cost before
   reload, cost of the allocnos assigned to hard-registers, cost of
   the allocnos assigned to memory, cost of loads, stores and register
   move insns generated for pseudo-register live range splitting (see
   ira-emit.c).  */
int64_t ira_overall_cost, overall_cost_before;
int64_t ira_reg_cost, ira_mem_cost;
int64_t ira_load_cost, ira_store_cost, ira_shuffle_cost;
int ira_move_loops_num, ira_additional_jumps_num;

/* All registers that can be eliminated.  */

HARD_REG_SET eliminable_regset;

/* Value of max_reg_num () before IRA work start.  This value helps
   us to recognize a situation when new pseudos were created during
   IRA work.  */
static int max_regno_before_ira;

/* Temporary hard reg set used for a different calculation.  */
static HARD_REG_SET temp_hard_regset;

#define last_mode_for_init_move_cost \
  (this_target_ira_int->x_last_mode_for_init_move_cost)


/* The function sets up the map IRA_REG_MODE_HARD_REGSET.  */
static void
setup_reg_mode_hard_regset (void)
{
  int i, m, hard_regno;

  for (m = 0; m < NUM_MACHINE_MODES; m++)
    for (hard_regno = 0; hard_regno < FIRST_PSEUDO_REGISTER; hard_regno++)
      {
	CLEAR_HARD_REG_SET (ira_reg_mode_hard_regset[hard_regno][m]);
	for (i = hard_regno_nregs (hard_regno, (machine_mode) m) - 1;
	     i >= 0; i--)
	  if (hard_regno + i < FIRST_PSEUDO_REGISTER)
	    SET_HARD_REG_BIT (ira_reg_mode_hard_regset[hard_regno][m],
			      hard_regno + i);
      }
}


#define no_unit_alloc_regs \
  (this_target_ira_int->x_no_unit_alloc_regs)

/* The function sets up the three arrays declared above.  */
static void
setup_class_hard_regs (void)
{
  int cl, i, hard_regno, n;
  HARD_REG_SET processed_hard_reg_set;

  ira_assert (SHRT_MAX >= FIRST_PSEUDO_REGISTER);
  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    {
      temp_hard_regset = reg_class_contents[cl] & ~no_unit_alloc_regs;
      CLEAR_HARD_REG_SET (processed_hard_reg_set);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
	  ira_non_ordered_class_hard_regs[cl][i] = -1;
	  ira_class_hard_reg_index[cl][i] = -1;
	}
      for (n = 0, i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
#ifdef REG_ALLOC_ORDER
	  hard_regno = reg_alloc_order[i];
#else
	  hard_regno = i;
#endif
	  if (TEST_HARD_REG_BIT (processed_hard_reg_set, hard_regno))
	    continue;
	  SET_HARD_REG_BIT (processed_hard_reg_set, hard_regno);
      	  if (! TEST_HARD_REG_BIT (temp_hard_regset, hard_regno))
	    ira_class_hard_reg_index[cl][hard_regno] = -1;
	  else
	    {
	      ira_class_hard_reg_index[cl][hard_regno] = n;
	      ira_class_hard_regs[cl][n++] = hard_regno;
	    }
	}
      ira_class_hard_regs_num[cl] = n;
      for (n = 0, i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (temp_hard_regset, i))
	  ira_non_ordered_class_hard_regs[cl][n++] = i;
      ira_assert (ira_class_hard_regs_num[cl] == n);
    }
}

/* Set up global variables defining info about hard registers for the
   allocation.  These depend on USE_HARD_FRAME_P whose TRUE value means
   that we can use the hard frame pointer for the allocation.  */
static void
setup_alloc_regs (bool use_hard_frame_p)
{
#ifdef ADJUST_REG_ALLOC_ORDER
  ADJUST_REG_ALLOC_ORDER;
#endif
  no_unit_alloc_regs = fixed_nonglobal_reg_set;
  if (! use_hard_frame_p)
    add_to_hard_reg_set (&no_unit_alloc_regs, Pmode,
			 HARD_FRAME_POINTER_REGNUM);
  setup_class_hard_regs ();
}



#define alloc_reg_class_subclasses \
  (this_target_ira_int->x_alloc_reg_class_subclasses)

/* Initialize the table of subclasses of each reg class.  */
static void
setup_reg_subclasses (void)
{
  int i, j;
  HARD_REG_SET temp_hard_regset2;

  for (i = 0; i < N_REG_CLASSES; i++)
    for (j = 0; j < N_REG_CLASSES; j++)
      alloc_reg_class_subclasses[i][j] = LIM_REG_CLASSES;

  for (i = 0; i < N_REG_CLASSES; i++)
    {
      if (i == (int) NO_REGS)
	continue;

      temp_hard_regset = reg_class_contents[i] & ~no_unit_alloc_regs;
      if (hard_reg_set_empty_p (temp_hard_regset))
	continue;
      for (j = 0; j < N_REG_CLASSES; j++)
	if (i != j)
	  {
	    enum reg_class *p;

	    temp_hard_regset2 = reg_class_contents[j] & ~no_unit_alloc_regs;
	    if (! hard_reg_set_subset_p (temp_hard_regset,
					 temp_hard_regset2))
	      continue;
	    p = &alloc_reg_class_subclasses[j][0];
	    while (*p != LIM_REG_CLASSES) p++;
	    *p = (enum reg_class) i;
	  }
    }
}



/* Set up IRA_MEMORY_MOVE_COST and IRA_MAX_MEMORY_MOVE_COST.  */
static void
setup_class_subset_and_memory_move_costs (void)
{
  int cl, cl2, mode, cost;
  HARD_REG_SET temp_hard_regset2;

  for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
    ira_memory_move_cost[mode][NO_REGS][0]
      = ira_memory_move_cost[mode][NO_REGS][1] = SHRT_MAX;
  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    {
      if (cl != (int) NO_REGS)
	for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
	  {
	    ira_max_memory_move_cost[mode][cl][0]
	      = ira_memory_move_cost[mode][cl][0]
	      = memory_move_cost ((machine_mode) mode,
				  (reg_class_t) cl, false);
	    ira_max_memory_move_cost[mode][cl][1]
	      = ira_memory_move_cost[mode][cl][1]
	      = memory_move_cost ((machine_mode) mode,
				  (reg_class_t) cl, true);
	    /* Costs for NO_REGS are used in cost calculation on the
	       1st pass when the preferred register classes are not
	       known yet.  In this case we take the best scenario.  */
	    if (ira_memory_move_cost[mode][NO_REGS][0]
		> ira_memory_move_cost[mode][cl][0])
	      ira_max_memory_move_cost[mode][NO_REGS][0]
		= ira_memory_move_cost[mode][NO_REGS][0]
		= ira_memory_move_cost[mode][cl][0];
	    if (ira_memory_move_cost[mode][NO_REGS][1]
		> ira_memory_move_cost[mode][cl][1])
	      ira_max_memory_move_cost[mode][NO_REGS][1]
		= ira_memory_move_cost[mode][NO_REGS][1]
		= ira_memory_move_cost[mode][cl][1];
	  }
    }
  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    for (cl2 = (int) N_REG_CLASSES - 1; cl2 >= 0; cl2--)
      {
	temp_hard_regset = reg_class_contents[cl] & ~no_unit_alloc_regs;
	temp_hard_regset2 = reg_class_contents[cl2] & ~no_unit_alloc_regs;
	ira_class_subset_p[cl][cl2]
	  = hard_reg_set_subset_p (temp_hard_regset, temp_hard_regset2);
	if (! hard_reg_set_empty_p (temp_hard_regset2)
	    && hard_reg_set_subset_p (reg_class_contents[cl2],
				      reg_class_contents[cl]))
	  for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
	    {
	      cost = ira_memory_move_cost[mode][cl2][0];
	      if (cost > ira_max_memory_move_cost[mode][cl][0])
		ira_max_memory_move_cost[mode][cl][0] = cost;
	      cost = ira_memory_move_cost[mode][cl2][1];
	      if (cost > ira_max_memory_move_cost[mode][cl][1])
		ira_max_memory_move_cost[mode][cl][1] = cost;
	    }
      }
  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
      {
	ira_memory_move_cost[mode][cl][0]
	  = ira_max_memory_move_cost[mode][cl][0];
	ira_memory_move_cost[mode][cl][1]
	  = ira_max_memory_move_cost[mode][cl][1];
      }
  setup_reg_subclasses ();
}



/* Define the following macro if allocation through malloc if
   preferable.  */
#define IRA_NO_OBSTACK

#ifndef IRA_NO_OBSTACK
/* Obstack used for storing all dynamic data (except bitmaps) of the
   IRA.  */
static struct obstack ira_obstack;
#endif

/* Obstack used for storing all bitmaps of the IRA.  */
static struct bitmap_obstack ira_bitmap_obstack;

/* Allocate memory of size LEN for IRA data.  */
void *
ira_allocate (size_t len)
{
  void *res;

#ifndef IRA_NO_OBSTACK
  res = obstack_alloc (&ira_obstack, len);
#else
  res = xmalloc (len);
#endif
  return res;
}

/* Free memory ADDR allocated for IRA data.  */
void
ira_free (void *addr ATTRIBUTE_UNUSED)
{
#ifndef IRA_NO_OBSTACK
  /* do nothing */
#else
  free (addr);
#endif
}


/* Allocate and returns bitmap for IRA.  */
bitmap
ira_allocate_bitmap (void)
{
  return BITMAP_ALLOC (&ira_bitmap_obstack);
}

/* Free bitmap B allocated for IRA.  */
void
ira_free_bitmap (bitmap b ATTRIBUTE_UNUSED)
{
  /* do nothing */
}



/* Output information about allocation of all allocnos (except for
   caps) into file F.  */
void
ira_print_disposition (FILE *f)
{
  int i, n, max_regno;
  ira_allocno_t a;
  basic_block bb;

  fprintf (f, "Disposition:");
  max_regno = max_reg_num ();
  for (n = 0, i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    for (a = ira_regno_allocno_map[i];
	 a != NULL;
	 a = ALLOCNO_NEXT_REGNO_ALLOCNO (a))
      {
	if (n % 4 == 0)
	  fprintf (f, "\n");
	n++;
	fprintf (f, " %4d:r%-4d", ALLOCNO_NUM (a), ALLOCNO_REGNO (a));
	if ((bb = ALLOCNO_LOOP_TREE_NODE (a)->bb) != NULL)
	  fprintf (f, "b%-3d", bb->index);
	else
	  fprintf (f, "l%-3d", ALLOCNO_LOOP_TREE_NODE (a)->loop_num);
	if (ALLOCNO_HARD_REGNO (a) >= 0)
	  fprintf (f, " %3d", ALLOCNO_HARD_REGNO (a));
	else
	  fprintf (f, " mem");
      }
  fprintf (f, "\n");
}

/* Outputs information about allocation of all allocnos into
   stderr.  */
void
ira_debug_disposition (void)
{
  ira_print_disposition (stderr);
}



/* Set up ira_stack_reg_pressure_class which is the biggest pressure
   register class containing stack registers or NO_REGS if there are
   no stack registers.  To find this class, we iterate through all
   register pressure classes and choose the first register pressure
   class containing all the stack registers and having the biggest
   size.  */
static void
setup_stack_reg_pressure_class (void)
{
  ira_stack_reg_pressure_class = NO_REGS;
#ifdef STACK_REGS
  {
    int i, best, size;
    enum reg_class cl;
    HARD_REG_SET temp_hard_regset2;

    CLEAR_HARD_REG_SET (temp_hard_regset);
    for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
      SET_HARD_REG_BIT (temp_hard_regset, i);
    best = 0;
    for (i = 0; i < ira_pressure_classes_num; i++)
      {
	cl = ira_pressure_classes[i];
	temp_hard_regset2 = temp_hard_regset & reg_class_contents[cl];
	size = hard_reg_set_size (temp_hard_regset2);
	if (best < size)
	  {
	    best = size;
	    ira_stack_reg_pressure_class = cl;
	  }
      }
  }
#endif
}

/* Find pressure classes which are register classes for which we
   calculate register pressure in IRA, register pressure sensitive
   insn scheduling, and register pressure sensitive loop invariant
   motion.

   To make register pressure calculation easy, we always use
   non-intersected register pressure classes.  A move of hard
   registers from one register pressure class is not more expensive
   than load and store of the hard registers.  Most likely an allocno
   class will be a subset of a register pressure class and in many
   cases a register pressure class.  That makes usage of register
   pressure classes a good approximation to find a high register
   pressure.  */
static void
setup_pressure_classes (void)
{
  int cost, i, n, curr;
  int cl, cl2;
  enum reg_class pressure_classes[N_REG_CLASSES];
  int m;
  HARD_REG_SET temp_hard_regset2;
  bool insert_p;

  if (targetm.compute_pressure_classes)
    n = targetm.compute_pressure_classes (pressure_classes);
  else
    { 
      n = 0;
      for (cl = 0; cl < N_REG_CLASSES; cl++)
	{
	  if (ira_class_hard_regs_num[cl] == 0)
	    continue;
	  if (ira_class_hard_regs_num[cl] != 1
	      /* A register class without subclasses may contain a few
		 hard registers and movement between them is costly
		 (e.g. SPARC FPCC registers).  We still should consider it
		 as a candidate for a pressure class.  */
	      && alloc_reg_class_subclasses[cl][0] < cl)
	    {
	      /* Check that the moves between any hard registers of the
		 current class are not more expensive for a legal mode
		 than load/store of the hard registers of the current
		 class.  Such class is a potential candidate to be a
		 register pressure class.  */
	      for (m = 0; m < NUM_MACHINE_MODES; m++)
		{
		  temp_hard_regset
		    = (reg_class_contents[cl]
		       & ~(no_unit_alloc_regs
			   | ira_prohibited_class_mode_regs[cl][m]));
		  if (hard_reg_set_empty_p (temp_hard_regset))
		    continue;
		  ira_init_register_move_cost_if_necessary ((machine_mode) m);
		  cost = ira_register_move_cost[m][cl][cl];
		  if (cost <= ira_max_memory_move_cost[m][cl][1]
		      || cost <= ira_max_memory_move_cost[m][cl][0])
		    break;
		}
	      if (m >= NUM_MACHINE_MODES)
		continue;
	    }
	  curr = 0;
	  insert_p = true;
	  temp_hard_regset = reg_class_contents[cl] & ~no_unit_alloc_regs;
	  /* Remove so far added pressure classes which are subset of the
	     current candidate class.  Prefer GENERAL_REGS as a pressure
	     register class to another class containing the same
	     allocatable hard registers.  We do this because machine
	     dependent cost hooks might give wrong costs for the latter
	     class but always give the right cost for the former class
	     (GENERAL_REGS).  */
	  for (i = 0; i < n; i++)
	    {
	      cl2 = pressure_classes[i];
	      temp_hard_regset2 = (reg_class_contents[cl2]
				   & ~no_unit_alloc_regs);
	      if (hard_reg_set_subset_p (temp_hard_regset, temp_hard_regset2)
		  && (temp_hard_regset != temp_hard_regset2
		      || cl2 == (int) GENERAL_REGS))
		{
		  pressure_classes[curr++] = (enum reg_class) cl2;
		  insert_p = false;
		  continue;
		}
	      if (hard_reg_set_subset_p (temp_hard_regset2, temp_hard_regset)
		  && (temp_hard_regset2 != temp_hard_regset
		      || cl == (int) GENERAL_REGS))
		continue;
	      if (temp_hard_regset2 == temp_hard_regset)
		insert_p = false;
	      pressure_classes[curr++] = (enum reg_class) cl2;
	    }
	  /* If the current candidate is a subset of a so far added
	     pressure class, don't add it to the list of the pressure
	     classes.  */
	  if (insert_p)
	    pressure_classes[curr++] = (enum reg_class) cl;
	  n = curr;
	}
    }
#ifdef ENABLE_IRA_CHECKING
  {
    HARD_REG_SET ignore_hard_regs;

    /* Check pressure classes correctness: here we check that hard
       registers from all register pressure classes contains all hard
       registers available for the allocation.  */
    CLEAR_HARD_REG_SET (temp_hard_regset);
    CLEAR_HARD_REG_SET (temp_hard_regset2);
    ignore_hard_regs = no_unit_alloc_regs;
    for (cl = 0; cl < LIM_REG_CLASSES; cl++)
      {
	/* For some targets (like MIPS with MD_REGS), there are some
	   classes with hard registers available for allocation but
	   not able to hold value of any mode.  */
	for (m = 0; m < NUM_MACHINE_MODES; m++)
	  if (contains_reg_of_mode[cl][m])
	    break;
	if (m >= NUM_MACHINE_MODES)
	  {
	    ignore_hard_regs |= reg_class_contents[cl];
	    continue;
	  }
	for (i = 0; i < n; i++)
	  if ((int) pressure_classes[i] == cl)
	    break;
	temp_hard_regset2 |= reg_class_contents[cl];
	if (i < n)
	  temp_hard_regset |= reg_class_contents[cl];
      }
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      /* Some targets (like SPARC with ICC reg) have allocatable regs
	 for which no reg class is defined.  */
      if (REGNO_REG_CLASS (i) == NO_REGS)
	SET_HARD_REG_BIT (ignore_hard_regs, i);
    temp_hard_regset &= ~ignore_hard_regs;
    temp_hard_regset2 &= ~ignore_hard_regs;
    ira_assert (hard_reg_set_subset_p (temp_hard_regset2, temp_hard_regset));
  }
#endif
  ira_pressure_classes_num = 0;
  for (i = 0; i < n; i++)
    {
      cl = (int) pressure_classes[i];
      ira_reg_pressure_class_p[cl] = true;
      ira_pressure_classes[ira_pressure_classes_num++] = (enum reg_class) cl;
    }
  setup_stack_reg_pressure_class ();
}

/* Set up IRA_UNIFORM_CLASS_P.  Uniform class is a register class
   whose register move cost between any registers of the class is the
   same as for all its subclasses.  We use the data to speed up the
   2nd pass of calculations of allocno costs.  */
static void
setup_uniform_class_p (void)
{
  int i, cl, cl2, m;

  for (cl = 0; cl < N_REG_CLASSES; cl++)
    {
      ira_uniform_class_p[cl] = false;
      if (ira_class_hard_regs_num[cl] == 0)
	continue;
      /* We cannot use alloc_reg_class_subclasses here because move
	 cost hooks does not take into account that some registers are
	 unavailable for the subtarget.  E.g. for i686, INT_SSE_REGS
	 is element of alloc_reg_class_subclasses for GENERAL_REGS
	 because SSE regs are unavailable.  */
      for (i = 0; (cl2 = reg_class_subclasses[cl][i]) != LIM_REG_CLASSES; i++)
	{
	  if (ira_class_hard_regs_num[cl2] == 0)
	    continue;
      	  for (m = 0; m < NUM_MACHINE_MODES; m++)
	    if (contains_reg_of_mode[cl][m] && contains_reg_of_mode[cl2][m])
	      {
		ira_init_register_move_cost_if_necessary ((machine_mode) m);
		if (ira_register_move_cost[m][cl][cl]
		    != ira_register_move_cost[m][cl2][cl2])
		  break;
	      }
	  if (m < NUM_MACHINE_MODES)
	    break;
	}
      if (cl2 == LIM_REG_CLASSES)
	ira_uniform_class_p[cl] = true;
    }
}

/* Set up IRA_ALLOCNO_CLASSES, IRA_ALLOCNO_CLASSES_NUM,
   IRA_IMPORTANT_CLASSES, and IRA_IMPORTANT_CLASSES_NUM.

   Target may have many subtargets and not all target hard registers can
   be used for allocation, e.g. x86 port in 32-bit mode cannot use
   hard registers introduced in x86-64 like r8-r15).  Some classes
   might have the same allocatable hard registers, e.g.  INDEX_REGS
   and GENERAL_REGS in x86 port in 32-bit mode.  To decrease different
   calculations efforts we introduce allocno classes which contain
   unique non-empty sets of allocatable hard-registers.

   Pseudo class cost calculation in ira-costs.c is very expensive.
   Therefore we are trying to decrease number of classes involved in
   such calculation.  Register classes used in the cost calculation
   are called important classes.  They are allocno classes and other
   non-empty classes whose allocatable hard register sets are inside
   of an allocno class hard register set.  From the first sight, it
   looks like that they are just allocno classes.  It is not true.  In
   example of x86-port in 32-bit mode, allocno classes will contain
   GENERAL_REGS but not LEGACY_REGS (because allocatable hard
   registers are the same for the both classes).  The important
   classes will contain GENERAL_REGS and LEGACY_REGS.  It is done
   because a machine description insn constraint may refers for
   LEGACY_REGS and code in ira-costs.c is mostly base on investigation
   of the insn constraints.  */
static void
setup_allocno_and_important_classes (void)
{
  int i, j, n, cl;
  bool set_p;
  HARD_REG_SET temp_hard_regset2;
  static enum reg_class classes[LIM_REG_CLASSES + 1];

  n = 0;
  /* Collect classes which contain unique sets of allocatable hard
     registers.  Prefer GENERAL_REGS to other classes containing the
     same set of hard registers.  */
  for (i = 0; i < LIM_REG_CLASSES; i++)
    {
      temp_hard_regset = reg_class_contents[i] & ~no_unit_alloc_regs;
      for (j = 0; j < n; j++)
	{
	  cl = classes[j];
	  temp_hard_regset2 = reg_class_contents[cl] & ~no_unit_alloc_regs;
	  if (temp_hard_regset == temp_hard_regset2)
	    break;
	}
      if (j >= n || targetm.additional_allocno_class_p (i))
	classes[n++] = (enum reg_class) i;
      else if (i == GENERAL_REGS)
	/* Prefer general regs.  For i386 example, it means that
	   we prefer GENERAL_REGS over INDEX_REGS or LEGACY_REGS
	   (all of them consists of the same available hard
	   registers).  */
	classes[j] = (enum reg_class) i;
    }
  classes[n] = LIM_REG_CLASSES;

  /* Set up classes which can be used for allocnos as classes
     containing non-empty unique sets of allocatable hard
     registers.  */
  ira_allocno_classes_num = 0;
  for (i = 0; (cl = classes[i]) != LIM_REG_CLASSES; i++)
    if (ira_class_hard_regs_num[cl] > 0)
      ira_allocno_classes[ira_allocno_classes_num++] = (enum reg_class) cl;
  ira_important_classes_num = 0;
  /* Add non-allocno classes containing to non-empty set of
     allocatable hard regs.  */
  for (cl = 0; cl < N_REG_CLASSES; cl++)
    if (ira_class_hard_regs_num[cl] > 0)
      {
	temp_hard_regset = reg_class_contents[cl] & ~no_unit_alloc_regs;
	set_p = false;
	for (j = 0; j < ira_allocno_classes_num; j++)
	  {
	    temp_hard_regset2 = (reg_class_contents[ira_allocno_classes[j]]
				 & ~no_unit_alloc_regs);
	    if ((enum reg_class) cl == ira_allocno_classes[j])
	      break;
	    else if (hard_reg_set_subset_p (temp_hard_regset,
					    temp_hard_regset2))
	      set_p = true;
	  }
	if (set_p && j >= ira_allocno_classes_num)
	  ira_important_classes[ira_important_classes_num++]
	    = (enum reg_class) cl;
      }
  /* Now add allocno classes to the important classes.  */
  for (j = 0; j < ira_allocno_classes_num; j++)
    ira_important_classes[ira_important_classes_num++]
      = ira_allocno_classes[j];
  for (cl = 0; cl < N_REG_CLASSES; cl++)
    {
      ira_reg_allocno_class_p[cl] = false;
      ira_reg_pressure_class_p[cl] = false;
    }
  for (j = 0; j < ira_allocno_classes_num; j++)
    ira_reg_allocno_class_p[ira_allocno_classes[j]] = true;
  setup_pressure_classes ();
  setup_uniform_class_p ();
}

/* Setup translation in CLASS_TRANSLATE of all classes into a class
   given by array CLASSES of length CLASSES_NUM.  The function is used
   make translation any reg class to an allocno class or to an
   pressure class.  This translation is necessary for some
   calculations when we can use only allocno or pressure classes and
   such translation represents an approximate representation of all
   classes.

   The translation in case when allocatable hard register set of a
   given class is subset of allocatable hard register set of a class
   in CLASSES is pretty simple.  We use smallest classes from CLASSES
   containing a given class.  If allocatable hard register set of a
   given class is not a subset of any corresponding set of a class
   from CLASSES, we use the cheapest (with load/store point of view)
   class from CLASSES whose set intersects with given class set.  */
static void
setup_class_translate_array (enum reg_class *class_translate,
			     int classes_num, enum reg_class *classes)
{
  int cl, mode;
  enum reg_class aclass, best_class, *cl_ptr;
  int i, cost, min_cost, best_cost;

  for (cl = 0; cl < N_REG_CLASSES; cl++)
    class_translate[cl] = NO_REGS;

  for (i = 0; i < classes_num; i++)
    {
      aclass = classes[i];
      for (cl_ptr = &alloc_reg_class_subclasses[aclass][0];
	   (cl = *cl_ptr) != LIM_REG_CLASSES;
	   cl_ptr++)
	if (class_translate[cl] == NO_REGS)
	  class_translate[cl] = aclass;
      class_translate[aclass] = aclass;
    }
  /* For classes which are not fully covered by one of given classes
     (in other words covered by more one given class), use the
     cheapest class.  */
  for (cl = 0; cl < N_REG_CLASSES; cl++)
    {
      if (cl == NO_REGS || class_translate[cl] != NO_REGS)
	continue;
      best_class = NO_REGS;
      best_cost = INT_MAX;
      for (i = 0; i < classes_num; i++)
	{
	  aclass = classes[i];
	  temp_hard_regset = (reg_class_contents[aclass]
			      & reg_class_contents[cl]
			      & ~no_unit_alloc_regs);
	  if (! hard_reg_set_empty_p (temp_hard_regset))
	    {
	      min_cost = INT_MAX;
	      for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
		{
		  cost = (ira_memory_move_cost[mode][aclass][0]
			  + ira_memory_move_cost[mode][aclass][1]);
		  if (min_cost > cost)
		    min_cost = cost;
		}
	      if (best_class == NO_REGS || best_cost > min_cost)
		{
		  best_class = aclass;
		  best_cost = min_cost;
		}
	    }
	}
      class_translate[cl] = best_class;
    }
}

/* Set up array IRA_ALLOCNO_CLASS_TRANSLATE and
   IRA_PRESSURE_CLASS_TRANSLATE.  */
static void
setup_class_translate (void)
{
  setup_class_translate_array (ira_allocno_class_translate,
			       ira_allocno_classes_num, ira_allocno_classes);
  setup_class_translate_array (ira_pressure_class_translate,
			       ira_pressure_classes_num, ira_pressure_classes);
}

/* Order numbers of allocno classes in original target allocno class
   array, -1 for non-allocno classes.  */
static int allocno_class_order[N_REG_CLASSES];

/* The function used to sort the important classes.  */
static int
comp_reg_classes_func (const void *v1p, const void *v2p)
{
  enum reg_class cl1 = *(const enum reg_class *) v1p;
  enum reg_class cl2 = *(const enum reg_class *) v2p;
  enum reg_class tcl1, tcl2;
  int diff;

  tcl1 = ira_allocno_class_translate[cl1];
  tcl2 = ira_allocno_class_translate[cl2];
  if (tcl1 != NO_REGS && tcl2 != NO_REGS
      && (diff = allocno_class_order[tcl1] - allocno_class_order[tcl2]) != 0)
    return diff;
  return (int) cl1 - (int) cl2;
}

/* For correct work of function setup_reg_class_relation we need to
   reorder important classes according to the order of their allocno
   classes.  It places important classes containing the same
   allocatable hard register set adjacent to each other and allocno
   class with the allocatable hard register set right after the other
   important classes with the same set.

   In example from comments of function
   setup_allocno_and_important_classes, it places LEGACY_REGS and
   GENERAL_REGS close to each other and GENERAL_REGS is after
   LEGACY_REGS.  */
static void
reorder_important_classes (void)
{
  int i;

  for (i = 0; i < N_REG_CLASSES; i++)
    allocno_class_order[i] = -1;
  for (i = 0; i < ira_allocno_classes_num; i++)
    allocno_class_order[ira_allocno_classes[i]] = i;
  qsort (ira_important_classes, ira_important_classes_num,
	 sizeof (enum reg_class), comp_reg_classes_func);
  for (i = 0; i < ira_important_classes_num; i++)
    ira_important_class_nums[ira_important_classes[i]] = i;
}

/* Set up IRA_REG_CLASS_SUBUNION, IRA_REG_CLASS_SUPERUNION,
   IRA_REG_CLASS_SUPER_CLASSES, IRA_REG_CLASSES_INTERSECT, and
   IRA_REG_CLASSES_INTERSECT_P.  For the meaning of the relations,
   please see corresponding comments in ira-int.h.  */
static void
setup_reg_class_relations (void)
{
  int i, cl1, cl2, cl3;
  HARD_REG_SET intersection_set, union_set, temp_set2;
  bool important_class_p[N_REG_CLASSES];

  memset (important_class_p, 0, sizeof (important_class_p));
  for (i = 0; i < ira_important_classes_num; i++)
    important_class_p[ira_important_classes[i]] = true;
  for (cl1 = 0; cl1 < N_REG_CLASSES; cl1++)
    {
      ira_reg_class_super_classes[cl1][0] = LIM_REG_CLASSES;
      for (cl2 = 0; cl2 < N_REG_CLASSES; cl2++)
	{
	  ira_reg_classes_intersect_p[cl1][cl2] = false;
	  ira_reg_class_intersect[cl1][cl2] = NO_REGS;
	  ira_reg_class_subset[cl1][cl2] = NO_REGS;
	  temp_hard_regset = reg_class_contents[cl1] & ~no_unit_alloc_regs;
	  temp_set2 = reg_class_contents[cl2] & ~no_unit_alloc_regs;
	  if (hard_reg_set_empty_p (temp_hard_regset)
	      && hard_reg_set_empty_p (temp_set2))
	    {
	      /* The both classes have no allocatable hard registers
		 -- take all class hard registers into account and use
		 reg_class_subunion and reg_class_superunion.  */
	      for (i = 0;; i++)
		{
		  cl3 = reg_class_subclasses[cl1][i];
		  if (cl3 == LIM_REG_CLASSES)
		    break;
		  if (reg_class_subset_p (ira_reg_class_intersect[cl1][cl2],
					  (enum reg_class) cl3))
		    ira_reg_class_intersect[cl1][cl2] = (enum reg_class) cl3;
		}
	      ira_reg_class_subunion[cl1][cl2] = reg_class_subunion[cl1][cl2];
	      ira_reg_class_superunion[cl1][cl2] = reg_class_superunion[cl1][cl2];
	      continue;
	    }
	  ira_reg_classes_intersect_p[cl1][cl2]
	    = hard_reg_set_intersect_p (temp_hard_regset, temp_set2);
	  if (important_class_p[cl1] && important_class_p[cl2]
	      && hard_reg_set_subset_p (temp_hard_regset, temp_set2))
	    {
	      /* CL1 and CL2 are important classes and CL1 allocatable
		 hard register set is inside of CL2 allocatable hard
		 registers -- make CL1 a superset of CL2.  */
	      enum reg_class *p;

	      p = &ira_reg_class_super_classes[cl1][0];
	      while (*p != LIM_REG_CLASSES)
		p++;
	      *p++ = (enum reg_class) cl2;
	      *p = LIM_REG_CLASSES;
	    }
	  ira_reg_class_subunion[cl1][cl2] = NO_REGS;
	  ira_reg_class_superunion[cl1][cl2] = NO_REGS;
	  intersection_set = (reg_class_contents[cl1]
			      & reg_class_contents[cl2]
			      & ~no_unit_alloc_regs);
	  union_set = ((reg_class_contents[cl1] | reg_class_contents[cl2])
		       & ~no_unit_alloc_regs);
	  for (cl3 = 0; cl3 < N_REG_CLASSES; cl3++)
	    {
	      temp_hard_regset = reg_class_contents[cl3] & ~no_unit_alloc_regs;
	      if (hard_reg_set_subset_p (temp_hard_regset, intersection_set))
		{
		  /* CL3 allocatable hard register set is inside of
		     intersection of allocatable hard register sets
		     of CL1 and CL2.  */
		  if (important_class_p[cl3])
		    {
		      temp_set2
			= (reg_class_contents
			   [ira_reg_class_intersect[cl1][cl2]]);
		      temp_set2 &= ~no_unit_alloc_regs;
		      if (! hard_reg_set_subset_p (temp_hard_regset, temp_set2)
			  /* If the allocatable hard register sets are
			     the same, prefer GENERAL_REGS or the
			     smallest class for debugging
			     purposes.  */
			  || (temp_hard_regset == temp_set2
			      && (cl3 == GENERAL_REGS
				  || ((ira_reg_class_intersect[cl1][cl2]
				       != GENERAL_REGS)
				      && hard_reg_set_subset_p
				         (reg_class_contents[cl3],
					  reg_class_contents
					  [(int)
					   ira_reg_class_intersect[cl1][cl2]])))))
			ira_reg_class_intersect[cl1][cl2] = (enum reg_class) cl3;
		    }
		  temp_set2
		    = (reg_class_contents[ira_reg_class_subset[cl1][cl2]]
		       & ~no_unit_alloc_regs);
		  if (! hard_reg_set_subset_p (temp_hard_regset, temp_set2)
		      /* Ignore unavailable hard registers and prefer
			 smallest class for debugging purposes.  */
		      || (temp_hard_regset == temp_set2
			  && hard_reg_set_subset_p
			     (reg_class_contents[cl3],
			      reg_class_contents
			      [(int) ira_reg_class_subset[cl1][cl2]])))
		    ira_reg_class_subset[cl1][cl2] = (enum reg_class) cl3;
		}
	      if (important_class_p[cl3]
		  && hard_reg_set_subset_p (temp_hard_regset, union_set))
		{
		  /* CL3 allocatable hard register set is inside of
		     union of allocatable hard register sets of CL1
		     and CL2.  */
		  temp_set2
		    = (reg_class_contents[ira_reg_class_subunion[cl1][cl2]]
		       & ~no_unit_alloc_regs);
	 	  if (ira_reg_class_subunion[cl1][cl2] == NO_REGS
		      || (hard_reg_set_subset_p (temp_set2, temp_hard_regset)
			  
			  && (temp_set2 != temp_hard_regset
			      || cl3 == GENERAL_REGS
			      /* If the allocatable hard register sets are the
				 same, prefer GENERAL_REGS or the smallest
				 class for debugging purposes.  */
			      || (ira_reg_class_subunion[cl1][cl2] != GENERAL_REGS
				  && hard_reg_set_subset_p
				     (reg_class_contents[cl3],
				      reg_class_contents
				      [(int) ira_reg_class_subunion[cl1][cl2]])))))
		    ira_reg_class_subunion[cl1][cl2] = (enum reg_class) cl3;
		}
	      if (hard_reg_set_subset_p (union_set, temp_hard_regset))
		{
		  /* CL3 allocatable hard register set contains union
		     of allocatable hard register sets of CL1 and
		     CL2.  */
		  temp_set2
		    = (reg_class_contents[ira_reg_class_superunion[cl1][cl2]]
		       & ~no_unit_alloc_regs);
	 	  if (ira_reg_class_superunion[cl1][cl2] == NO_REGS
		      || (hard_reg_set_subset_p (temp_hard_regset, temp_set2)

			  && (temp_set2 != temp_hard_regset
			      || cl3 == GENERAL_REGS
			      /* If the allocatable hard register sets are the
				 same, prefer GENERAL_REGS or the smallest
				 class for debugging purposes.  */
			      || (ira_reg_class_superunion[cl1][cl2] != GENERAL_REGS
				  && hard_reg_set_subset_p
				     (reg_class_contents[cl3],
				      reg_class_contents
				      [(int) ira_reg_class_superunion[cl1][cl2]])))))
		    ira_reg_class_superunion[cl1][cl2] = (enum reg_class) cl3;
		}
	    }
	}
    }
}

/* Output all uniform and important classes into file F.  */
static void
print_uniform_and_important_classes (FILE *f)
{
  int i, cl;

  fprintf (f, "Uniform classes:\n");
  for (cl = 0; cl < N_REG_CLASSES; cl++)
    if (ira_uniform_class_p[cl])
      fprintf (f, " %s", reg_class_names[cl]);
  fprintf (f, "\nImportant classes:\n");
  for (i = 0; i < ira_important_classes_num; i++)
    fprintf (f, " %s", reg_class_names[ira_important_classes[i]]);
  fprintf (f, "\n");
}

/* Output all possible allocno or pressure classes and their
   translation map into file F.  */
static void
print_translated_classes (FILE *f, bool pressure_p)
{
  int classes_num = (pressure_p
		     ? ira_pressure_classes_num : ira_allocno_classes_num);
  enum reg_class *classes = (pressure_p
			     ? ira_pressure_classes : ira_allocno_classes);
  enum reg_class *class_translate = (pressure_p
				     ? ira_pressure_class_translate
				     : ira_allocno_class_translate);
  int i;

  fprintf (f, "%s classes:\n", pressure_p ? "Pressure" : "Allocno");
  for (i = 0; i < classes_num; i++)
    fprintf (f, " %s", reg_class_names[classes[i]]);
  fprintf (f, "\nClass translation:\n");
  for (i = 0; i < N_REG_CLASSES; i++)
    fprintf (f, " %s -> %s\n", reg_class_names[i],
	     reg_class_names[class_translate[i]]);
}

/* Output all possible allocno and translation classes and the
   translation maps into stderr.  */
void
ira_debug_allocno_classes (void)
{
  print_uniform_and_important_classes (stderr);
  print_translated_classes (stderr, false);
  print_translated_classes (stderr, true);
}

/* Set up different arrays concerning class subsets, allocno and
   important classes.  */
static void
find_reg_classes (void)
{
  setup_allocno_and_important_classes ();
  setup_class_translate ();
  reorder_important_classes ();
  setup_reg_class_relations ();
}



/* Set up the array above.  */
static void
setup_hard_regno_aclass (void)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
#if 1
      ira_hard_regno_allocno_class[i]
	= (TEST_HARD_REG_BIT (no_unit_alloc_regs, i)
	   ? NO_REGS
	   : ira_allocno_class_translate[REGNO_REG_CLASS (i)]);
#else
      int j;
      enum reg_class cl;
      ira_hard_regno_allocno_class[i] = NO_REGS;
      for (j = 0; j < ira_allocno_classes_num; j++)
 	{
	  cl = ira_allocno_classes[j];
 	  if (ira_class_hard_reg_index[cl][i] >= 0)
 	    {
	      ira_hard_regno_allocno_class[i] = cl;
 	      break;
 	    }
 	}
#endif
    }
}



/* Form IRA_REG_CLASS_MAX_NREGS and IRA_REG_CLASS_MIN_NREGS maps.  */
static void
setup_reg_class_nregs (void)
{
  int i, cl, cl2, m;

  for (m = 0; m < MAX_MACHINE_MODE; m++)
    {
      for (cl = 0; cl < N_REG_CLASSES; cl++)
	ira_reg_class_max_nregs[cl][m]
	  = ira_reg_class_min_nregs[cl][m]
	  = targetm.class_max_nregs ((reg_class_t) cl, (machine_mode) m);
      for (cl = 0; cl < N_REG_CLASSES; cl++)
	for (i = 0;
	     (cl2 = alloc_reg_class_subclasses[cl][i]) != LIM_REG_CLASSES;
	     i++)
	  if (ira_reg_class_min_nregs[cl2][m]
	      < ira_reg_class_min_nregs[cl][m])
	    ira_reg_class_min_nregs[cl][m] = ira_reg_class_min_nregs[cl2][m];
    }
}



/* Set up IRA_PROHIBITED_CLASS_MODE_REGS and IRA_CLASS_SINGLETON.
   This function is called once IRA_CLASS_HARD_REGS has been initialized.  */
static void
setup_prohibited_class_mode_regs (void)
{
  int j, k, hard_regno, cl, last_hard_regno, count;

  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    {
      temp_hard_regset = reg_class_contents[cl] & ~no_unit_alloc_regs;
      for (j = 0; j < NUM_MACHINE_MODES; j++)
	{
	  count = 0;
	  last_hard_regno = -1;
	  CLEAR_HARD_REG_SET (ira_prohibited_class_mode_regs[cl][j]);
	  for (k = ira_class_hard_regs_num[cl] - 1; k >= 0; k--)
	    {
	      hard_regno = ira_class_hard_regs[cl][k];
	      if (!targetm.hard_regno_mode_ok (hard_regno, (machine_mode) j))
		SET_HARD_REG_BIT (ira_prohibited_class_mode_regs[cl][j],
				  hard_regno);
	      else if (in_hard_reg_set_p (temp_hard_regset,
					  (machine_mode) j, hard_regno))
		{
		  last_hard_regno = hard_regno;
		  count++;
		}
	    }
	  ira_class_singleton[cl][j] = (count == 1 ? last_hard_regno : -1);
	}
    }
}

/* Clarify IRA_PROHIBITED_CLASS_MODE_REGS by excluding hard registers
   spanning from one register pressure class to another one.  It is
   called after defining the pressure classes.  */
static void
clarify_prohibited_class_mode_regs (void)
{
  int j, k, hard_regno, cl, pclass, nregs;

  for (cl = (int) N_REG_CLASSES - 1; cl >= 0; cl--)
    for (j = 0; j < NUM_MACHINE_MODES; j++)
      {
	CLEAR_HARD_REG_SET (ira_useful_class_mode_regs[cl][j]);
	for (k = ira_class_hard_regs_num[cl] - 1; k >= 0; k--)
	  {
	    hard_regno = ira_class_hard_regs[cl][k];
	    if (TEST_HARD_REG_BIT (ira_prohibited_class_mode_regs[cl][j], hard_regno))
	      continue;
	    nregs = hard_regno_nregs (hard_regno, (machine_mode) j);
	    if (hard_regno + nregs > FIRST_PSEUDO_REGISTER)
	      {
		SET_HARD_REG_BIT (ira_prohibited_class_mode_regs[cl][j],
				  hard_regno);
		 continue;
	      }
	    pclass = ira_pressure_class_translate[REGNO_REG_CLASS (hard_regno)];
	    for (nregs-- ;nregs >= 0; nregs--)
	      if (((enum reg_class) pclass
		   != ira_pressure_class_translate[REGNO_REG_CLASS
						   (hard_regno + nregs)]))
		{
		  SET_HARD_REG_BIT (ira_prohibited_class_mode_regs[cl][j],
				    hard_regno);
		  break;
		}
	    if (!TEST_HARD_REG_BIT (ira_prohibited_class_mode_regs[cl][j],
				    hard_regno))
	      add_to_hard_reg_set (&ira_useful_class_mode_regs[cl][j],
				   (machine_mode) j, hard_regno);
	  }
      }
}

/* Allocate and initialize IRA_REGISTER_MOVE_COST, IRA_MAY_MOVE_IN_COST
   and IRA_MAY_MOVE_OUT_COST for MODE.  */
void
ira_init_register_move_cost (machine_mode mode)
{
  static unsigned short last_move_cost[N_REG_CLASSES][N_REG_CLASSES];
  bool all_match = true;
  unsigned int i, cl1, cl2;
  HARD_REG_SET ok_regs;

  ira_assert (ira_register_move_cost[mode] == NULL
	      && ira_may_move_in_cost[mode] == NULL
	      && ira_may_move_out_cost[mode] == NULL);
  CLEAR_HARD_REG_SET (ok_regs);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (targetm.hard_regno_mode_ok (i, mode))
      SET_HARD_REG_BIT (ok_regs, i);

  /* Note that we might be asked about the move costs of modes that
     cannot be stored in any hard register, for example if an inline
     asm tries to create a register operand with an impossible mode.
     We therefore can't assert have_regs_of_mode[mode] here.  */
  for (cl1 = 0; cl1 < N_REG_CLASSES; cl1++)
    for (cl2 = 0; cl2 < N_REG_CLASSES; cl2++)
      {
	int cost;
	if (!hard_reg_set_intersect_p (ok_regs, reg_class_contents[cl1])
	    || !hard_reg_set_intersect_p (ok_regs, reg_class_contents[cl2]))
	  {
	    if ((ira_reg_class_max_nregs[cl1][mode]
		 > ira_class_hard_regs_num[cl1])
		|| (ira_reg_class_max_nregs[cl2][mode]
		    > ira_class_hard_regs_num[cl2]))
	      cost = 65535;
	    else
	      cost = (ira_memory_move_cost[mode][cl1][0]
		      + ira_memory_move_cost[mode][cl2][1]) * 2;
	  }
	else
	  {
	    cost = register_move_cost (mode, (enum reg_class) cl1,
				       (enum reg_class) cl2);
	    ira_assert (cost < 65535);
	  }
	all_match &= (last_move_cost[cl1][cl2] == cost);
	last_move_cost[cl1][cl2] = cost;
      }
  if (all_match && last_mode_for_init_move_cost != -1)
    {
      ira_register_move_cost[mode]
	= ira_register_move_cost[last_mode_for_init_move_cost];
      ira_may_move_in_cost[mode]
	= ira_may_move_in_cost[last_mode_for_init_move_cost];
      ira_may_move_out_cost[mode]
	= ira_may_move_out_cost[last_mode_for_init_move_cost];
      return;
    }
  last_mode_for_init_move_cost = mode;
  ira_register_move_cost[mode] = XNEWVEC (move_table, N_REG_CLASSES);
  ira_may_move_in_cost[mode] = XNEWVEC (move_table, N_REG_CLASSES);
  ira_may_move_out_cost[mode] = XNEWVEC (move_table, N_REG_CLASSES);
  for (cl1 = 0; cl1 < N_REG_CLASSES; cl1++)
    for (cl2 = 0; cl2 < N_REG_CLASSES; cl2++)
      {
	int cost;
	enum reg_class *p1, *p2;
	
	if (last_move_cost[cl1][cl2] == 65535)
	  {
	    ira_register_move_cost[mode][cl1][cl2] = 65535;
	    ira_may_move_in_cost[mode][cl1][cl2] = 65535;
	    ira_may_move_out_cost[mode][cl1][cl2] = 65535;
	  }
	else
	  {
	    cost = last_move_cost[cl1][cl2];
	    
	    for (p2 = &reg_class_subclasses[cl2][0];
		 *p2 != LIM_REG_CLASSES; p2++)
	      if (ira_class_hard_regs_num[*p2] > 0
		  && (ira_reg_class_max_nregs[*p2][mode]
		      <= ira_class_hard_regs_num[*p2]))
		cost = MAX (cost, ira_register_move_cost[mode][cl1][*p2]);
	    
	    for (p1 = &reg_class_subclasses[cl1][0];
		 *p1 != LIM_REG_CLASSES; p1++)
	      if (ira_class_hard_regs_num[*p1] > 0
		  && (ira_reg_class_max_nregs[*p1][mode]
		      <= ira_class_hard_regs_num[*p1]))
		cost = MAX (cost, ira_register_move_cost[mode][*p1][cl2]);
	    
	    ira_assert (cost <= 65535);
	    ira_register_move_cost[mode][cl1][cl2] = cost;
	    
	    if (ira_class_subset_p[cl1][cl2])
	      ira_may_move_in_cost[mode][cl1][cl2] = 0;
	    else
	      ira_may_move_in_cost[mode][cl1][cl2] = cost;
	    
	    if (ira_class_subset_p[cl2][cl1])
	      ira_may_move_out_cost[mode][cl1][cl2] = 0;
	    else
	      ira_may_move_out_cost[mode][cl1][cl2] = cost;
	  }
      }
}



/* This is called once during compiler work.  It sets up
   different arrays whose values don't depend on the compiled
   function.  */
void
ira_init_once (void)
{
  ira_init_costs_once ();
  lra_init_once ();

  ira_use_lra_p = targetm.lra_p ();
}

/* Free ira_max_register_move_cost, ira_may_move_in_cost and
   ira_may_move_out_cost for each mode.  */
void
target_ira_int::free_register_move_costs (void)
{
  int mode, i;

  /* Reset move_cost and friends, making sure we only free shared
     table entries once.  */
  for (mode = 0; mode < MAX_MACHINE_MODE; mode++)
    if (x_ira_register_move_cost[mode])
      {
	for (i = 0;
	     i < mode && (x_ira_register_move_cost[i]
			  != x_ira_register_move_cost[mode]);
	     i++)
	  ;
	if (i == mode)
	  {
	    free (x_ira_register_move_cost[mode]);
	    free (x_ira_may_move_in_cost[mode]);
	    free (x_ira_may_move_out_cost[mode]);
	  }
      }
  memset (x_ira_register_move_cost, 0, sizeof x_ira_register_move_cost);
  memset (x_ira_may_move_in_cost, 0, sizeof x_ira_may_move_in_cost);
  memset (x_ira_may_move_out_cost, 0, sizeof x_ira_may_move_out_cost);
  last_mode_for_init_move_cost = -1;
}

target_ira_int::~target_ira_int ()
{
  free_ira_costs ();
  free_register_move_costs ();
}

/* This is called every time when register related information is
   changed.  */
void
ira_init (void)
{
  this_target_ira_int->free_register_move_costs ();
  setup_reg_mode_hard_regset ();
  setup_alloc_regs (flag_omit_frame_pointer != 0);
  setup_class_subset_and_memory_move_costs ();
  setup_reg_class_nregs ();
  setup_prohibited_class_mode_regs ();
  find_reg_classes ();
  clarify_prohibited_class_mode_regs ();
  setup_hard_regno_aclass ();
  ira_init_costs ();
}


#define ira_prohibited_mode_move_regs_initialized_p \
  (this_target_ira_int->x_ira_prohibited_mode_move_regs_initialized_p)

/* Set up IRA_PROHIBITED_MODE_MOVE_REGS.  */
static void
setup_prohibited_mode_move_regs (void)
{
  int i, j;
  rtx test_reg1, test_reg2, move_pat;
  rtx_insn *move_insn;

  if (ira_prohibited_mode_move_regs_initialized_p)
    return;
  ira_prohibited_mode_move_regs_initialized_p = true;
  test_reg1 = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);
  test_reg2 = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 2);
  move_pat = gen_rtx_SET (test_reg1, test_reg2);
  move_insn = gen_rtx_INSN (VOIDmode, 0, 0, 0, move_pat, 0, -1, 0);
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      SET_HARD_REG_SET (ira_prohibited_mode_move_regs[i]);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	{
	  if (!targetm.hard_regno_mode_ok (j, (machine_mode) i))
	    continue;
	  set_mode_and_regno (test_reg1, (machine_mode) i, j);
	  set_mode_and_regno (test_reg2, (machine_mode) i, j);
	  INSN_CODE (move_insn) = -1;
	  recog_memoized (move_insn);
	  if (INSN_CODE (move_insn) < 0)
	    continue;
	  extract_insn (move_insn);
	  /* We don't know whether the move will be in code that is optimized
	     for size or speed, so consider all enabled alternatives.  */
	  if (! constrain_operands (1, get_enabled_alternatives (move_insn)))
	    continue;
	  CLEAR_HARD_REG_BIT (ira_prohibited_mode_move_regs[i], j);
	}
    }
}



/* Extract INSN and return the set of alternatives that we should consider.
   This excludes any alternatives whose constraints are obviously impossible
   to meet (e.g. because the constraint requires a constant and the operand
   is nonconstant).  It also excludes alternatives that are bound to need
   a spill or reload, as long as we have other alternatives that match
   exactly.  */
alternative_mask
ira_setup_alts (rtx_insn *insn)
{
  int nop, nalt;
  bool curr_swapped;
  const char *p;
  int commutative = -1;

  extract_insn (insn);
  preprocess_constraints (insn);
  alternative_mask preferred = get_preferred_alternatives (insn);
  alternative_mask alts = 0;
  alternative_mask exact_alts = 0;
  /* Check that the hard reg set is enough for holding all
     alternatives.  It is hard to imagine the situation when the
     assertion is wrong.  */
  ira_assert (recog_data.n_alternatives
	      <= (int) MAX (sizeof (HARD_REG_ELT_TYPE) * CHAR_BIT,
			    FIRST_PSEUDO_REGISTER));
  for (nop = 0; nop < recog_data.n_operands; nop++)
    if (recog_data.constraints[nop][0] == '%')
      {
	commutative = nop;
	break;
      }
  for (curr_swapped = false;; curr_swapped = true)
    {
      for (nalt = 0; nalt < recog_data.n_alternatives; nalt++)
	{
	  if (!TEST_BIT (preferred, nalt) || TEST_BIT (exact_alts, nalt))
	    continue;

	  const operand_alternative *op_alt
	    = &recog_op_alt[nalt * recog_data.n_operands];
	  int this_reject = 0;
	  for (nop = 0; nop < recog_data.n_operands; nop++)
	    {
	      int c, len;

	      this_reject += op_alt[nop].reject;

	      rtx op = recog_data.operand[nop];
	      p = op_alt[nop].constraint;
	      if (*p == 0 || *p == ',')
		continue;

	      bool win_p = false;
	      do
		switch (c = *p, len = CONSTRAINT_LEN (c, p), c)
		  {
		  case '#':
		  case ',':
		    c = '\0';
		    /* FALLTHRU */
		  case '\0':
		    len = 0;
		    break;
		  
		  case '%':
		    /* The commutative modifier is handled above.  */
		    break;

		  case '0':  case '1':  case '2':  case '3':  case '4':
		  case '5':  case '6':  case '7':  case '8':  case '9':
		    {
		      rtx other = recog_data.operand[c - '0'];
		      if (MEM_P (other)
			  ? rtx_equal_p (other, op)
			  : REG_P (op) || SUBREG_P (op))
			goto op_success;
		      win_p = true;
		    }
		    break;
		    
		  case 'g':
		    goto op_success;
		    break;
		    
		  default:
		    {
		      enum constraint_num cn = lookup_constraint (p);
		      switch (get_constraint_type (cn))
			{
			case CT_REGISTER:
			  if (reg_class_for_constraint (cn) != NO_REGS)
			    {
			      if (REG_P (op) || SUBREG_P (op))
				goto op_success;
			      win_p = true;
			    }
			  break;

			case CT_CONST_INT:
			  if (CONST_INT_P (op)
			      && (insn_const_int_ok_for_constraint
				  (INTVAL (op), cn)))
			    goto op_success;
			  break;

			case CT_ADDRESS:
			  goto op_success;

			case CT_MEMORY:
			case CT_SPECIAL_MEMORY:
			  if (MEM_P (op))
			    goto op_success;
			  win_p = true;
			  break;

			case CT_FIXED_FORM:
			  if (constraint_satisfied_p (op, cn))
			    goto op_success;
			  break;
			}
		      break;
		    }
		  }
	      while (p += len, c);
	      if (!win_p)
		break;
	      /* We can make the alternative match by spilling a register
		 to memory or loading something into a register.  Count a
		 cost of one reload (the equivalent of the '?' constraint).  */
	      this_reject += 6;
	    op_success:
	      ;
	    }

	  if (nop >= recog_data.n_operands)
	    {
	      alts |= ALTERNATIVE_BIT (nalt);
	      if (this_reject == 0)
		exact_alts |= ALTERNATIVE_BIT (nalt);
	    }
	}
      if (commutative < 0)
	break;
      /* Swap forth and back to avoid changing recog_data.  */
      std::swap (recog_data.operand[commutative],
		 recog_data.operand[commutative + 1]);
      if (curr_swapped)
	break;
    }
  return exact_alts ? exact_alts : alts;
}

/* Return the number of the output non-early clobber operand which
   should be the same in any case as operand with number OP_NUM (or
   negative value if there is no such operand).  ALTS is the mask
   of alternatives that we should consider.  */
int
ira_get_dup_out_num (int op_num, alternative_mask alts)
{
  int curr_alt, c, original, dup;
  bool ignore_p, use_commut_op_p;
  const char *str;

  if (op_num < 0 || recog_data.n_alternatives == 0)
    return -1;
  /* We should find duplications only for input operands.  */
  if (recog_data.operand_type[op_num] != OP_IN)
    return -1;
  str = recog_data.constraints[op_num];
  use_commut_op_p = false;
  for (;;)
    {
      rtx op = recog_data.operand[op_num];
      
      for (curr_alt = 0, ignore_p = !TEST_BIT (alts, curr_alt),
	   original = -1;;)
	{
	  c = *str;
	  if (c == '\0')
	    break;
	  if (c == '#')
	    ignore_p = true;
	  else if (c == ',')
	    {
	      curr_alt++;
	      ignore_p = !TEST_BIT (alts, curr_alt);
	    }
	  else if (! ignore_p)
	    switch (c)
	      {
	      case 'g':
		goto fail;
	      default:
		{
		  enum constraint_num cn = lookup_constraint (str);
		  enum reg_class cl = reg_class_for_constraint (cn);
		  if (cl != NO_REGS
		      && !targetm.class_likely_spilled_p (cl))
		    goto fail;
		  if (constraint_satisfied_p (op, cn))
		    goto fail;
		  break;
		}
		
	      case '0': case '1': case '2': case '3': case '4':
	      case '5': case '6': case '7': case '8': case '9':
		if (original != -1 && original != c)
		  goto fail;
		original = c;
		break;
	      }
	  str += CONSTRAINT_LEN (c, str);
	}
      if (original == -1)
	goto fail;
      dup = original - '0';
      if (recog_data.operand_type[dup] == OP_OUT)
	return dup;
    fail:
      if (use_commut_op_p)
	break;
      use_commut_op_p = true;
      if (recog_data.constraints[op_num][0] == '%')
	str = recog_data.constraints[op_num + 1];
      else if (op_num > 0 && recog_data.constraints[op_num - 1][0] == '%')
	str = recog_data.constraints[op_num - 1];
      else
	break;
    }
  return -1;
}



/* Search forward to see if the source register of a copy insn dies
   before either it or the destination register is modified, but don't
   scan past the end of the basic block.  If so, we can replace the
   source with the destination and let the source die in the copy
   insn.

   This will reduce the number of registers live in that range and may
   enable the destination and the source coalescing, thus often saving
   one register in addition to a register-register copy.  */

static void
decrease_live_ranges_number (void)
{
  basic_block bb;
  rtx_insn *insn;
  rtx set, src, dest, dest_death, note;
  rtx_insn *p, *q;
  int sregno, dregno;

  if (! flag_expensive_optimizations)
    return;

  if (ira_dump_file)
    fprintf (ira_dump_file, "Starting decreasing number of live ranges...\n");

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      {
	set = single_set (insn);
	if (! set)
	  continue;
	src = SET_SRC (set);
	dest = SET_DEST (set);
	if (! REG_P (src) || ! REG_P (dest)
	    || find_reg_note (insn, REG_DEAD, src))
	  continue;
	sregno = REGNO (src);
	dregno = REGNO (dest);
	
	/* We don't want to mess with hard regs if register classes
	   are small.  */
	if (sregno == dregno
	    || (targetm.small_register_classes_for_mode_p (GET_MODE (src))
		&& (sregno < FIRST_PSEUDO_REGISTER
		    || dregno < FIRST_PSEUDO_REGISTER))
	    /* We don't see all updates to SP if they are in an
	       auto-inc memory reference, so we must disallow this
	       optimization on them.  */
	    || sregno == STACK_POINTER_REGNUM
	    || dregno == STACK_POINTER_REGNUM)
	  continue;
	
	dest_death = NULL_RTX;

	for (p = NEXT_INSN (insn); p; p = NEXT_INSN (p))
	  {
	    if (! INSN_P (p))
	      continue;
	    if (BLOCK_FOR_INSN (p) != bb)
	      break;
	    
	    if (reg_set_p (src, p) || reg_set_p (dest, p)
		/* If SRC is an asm-declared register, it must not be
		   replaced in any asm.  Unfortunately, the REG_EXPR
		   tree for the asm variable may be absent in the SRC
		   rtx, so we can't check the actual register
		   declaration easily (the asm operand will have it,
		   though).  To avoid complicating the test for a rare
		   case, we just don't perform register replacement
		   for a hard reg mentioned in an asm.  */
		|| (sregno < FIRST_PSEUDO_REGISTER
		    && asm_noperands (PATTERN (p)) >= 0
		    && reg_overlap_mentioned_p (src, PATTERN (p)))
		/* Don't change hard registers used by a call.  */
		|| (CALL_P (p) && sregno < FIRST_PSEUDO_REGISTER
		    && find_reg_fusage (p, USE, src))
		/* Don't change a USE of a register.  */
		|| (GET_CODE (PATTERN (p)) == USE
		    && reg_overlap_mentioned_p (src, XEXP (PATTERN (p), 0))))
	      break;
	    
	    /* See if all of SRC dies in P.  This test is slightly
	       more conservative than it needs to be.  */
	    if ((note = find_regno_note (p, REG_DEAD, sregno))
		&& GET_MODE (XEXP (note, 0)) == GET_MODE (src))
	      {
		int failed = 0;
		
		/* We can do the optimization.  Scan forward from INSN
		   again, replacing regs as we go.  Set FAILED if a
		   replacement can't be done.  In that case, we can't
		   move the death note for SRC.  This should be
		   rare.  */
		
		/* Set to stop at next insn.  */
		for (q = next_real_insn (insn);
		     q != next_real_insn (p);
		     q = next_real_insn (q))
		  {
		    if (reg_overlap_mentioned_p (src, PATTERN (q)))
		      {
			/* If SRC is a hard register, we might miss
			   some overlapping registers with
			   validate_replace_rtx, so we would have to
			   undo it.  We can't if DEST is present in
			   the insn, so fail in that combination of
			   cases.  */
			if (sregno < FIRST_PSEUDO_REGISTER
			    && reg_mentioned_p (dest, PATTERN (q)))
			  failed = 1;
			
			/* Attempt to replace all uses.  */
			else if (!validate_replace_rtx (src, dest, q))
			  failed = 1;
			
			/* If this succeeded, but some part of the
			   register is still present, undo the
			   replacement.  */
			else if (sregno < FIRST_PSEUDO_REGISTER
				 && reg_overlap_mentioned_p (src, PATTERN (q)))
			  {
			    validate_replace_rtx (dest, src, q);
			    failed = 1;
			  }
		      }
		    
		    /* If DEST dies here, remove the death note and
		       save it for later.  Make sure ALL of DEST dies
		       here; again, this is overly conservative.  */
		    if (! dest_death
			&& (dest_death = find_regno_note (q, REG_DEAD, dregno)))
		      {
			if (GET_MODE (XEXP (dest_death, 0)) == GET_MODE (dest))
			  remove_note (q, dest_death);
			else
			  {
			    failed = 1;
			    dest_death = 0;
			  }
		      }
		  }
		
		if (! failed)
		  {
		    /* Move death note of SRC from P to INSN.  */
		    remove_note (p, note);
		    XEXP (note, 1) = REG_NOTES (insn);
		    REG_NOTES (insn) = note;
		  }
		
		/* DEST is also dead if INSN has a REG_UNUSED note for
		   DEST.  */
		if (! dest_death
		    && (dest_death
			= find_regno_note (insn, REG_UNUSED, dregno)))
		  {
		    PUT_REG_NOTE_KIND (dest_death, REG_DEAD);
		    remove_note (insn, dest_death);
		  }
		
		/* Put death note of DEST on P if we saw it die.  */
		if (dest_death)
		  {
		    XEXP (dest_death, 1) = REG_NOTES (p);
		    REG_NOTES (p) = dest_death;
		  }
		break;
	      }
	    
	    /* If SRC is a hard register which is set or killed in
	       some other way, we can't do this optimization.  */
	    else if (sregno < FIRST_PSEUDO_REGISTER && dead_or_set_p (p, src))
	      break;
	  }
      }
}



/* Return nonzero if REGNO is a particularly bad choice for reloading X.  */
static bool
ira_bad_reload_regno_1 (int regno, rtx x)
{
  int x_regno, n, i;
  ira_allocno_t a;
  enum reg_class pref;

  /* We only deal with pseudo regs.  */
  if (! x || GET_CODE (x) != REG)
    return false;

  x_regno = REGNO (x);
  if (x_regno < FIRST_PSEUDO_REGISTER)
    return false;

  /* If the pseudo prefers REGNO explicitly, then do not consider
     REGNO a bad spill choice.  */
  pref = reg_preferred_class (x_regno);
  if (reg_class_size[pref] == 1)
    return !TEST_HARD_REG_BIT (reg_class_contents[pref], regno);

  /* If the pseudo conflicts with REGNO, then we consider REGNO a
     poor choice for a reload regno.  */
  a = ira_regno_allocno_map[x_regno];
  n = ALLOCNO_NUM_OBJECTS (a);
  for (i = 0; i < n; i++)
    {
      ira_object_t obj = ALLOCNO_OBJECT (a, i);
      if (TEST_HARD_REG_BIT (OBJECT_TOTAL_CONFLICT_HARD_REGS (obj), regno))
	return true;
    }
  return false;
}

/* Return nonzero if REGNO is a particularly bad choice for reloading
   IN or OUT.  */
bool
ira_bad_reload_regno (int regno, rtx in, rtx out)
{
  return (ira_bad_reload_regno_1 (regno, in)
	  || ira_bad_reload_regno_1 (regno, out));
}

/* Add register clobbers from asm statements.  */
static void
compute_regs_asm_clobbered (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;
      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  df_ref def;

	  if (NONDEBUG_INSN_P (insn) && asm_noperands (PATTERN (insn)) >= 0)
	    FOR_EACH_INSN_DEF (def, insn)
	      {
		unsigned int dregno = DF_REF_REGNO (def);
		if (HARD_REGISTER_NUM_P (dregno))
		  add_to_hard_reg_set (&crtl->asm_clobbers,
				       GET_MODE (DF_REF_REAL_REG (def)),
				       dregno);
	      }
	}
    }
}


/* Set up ELIMINABLE_REGSET, IRA_NO_ALLOC_REGS, and
   REGS_EVER_LIVE.  */
void
ira_setup_eliminable_regset (void)
{
  int i;
  static const struct {const int from, to; } eliminables[] = ELIMINABLE_REGS;
  int fp_reg_count = hard_regno_nregs (HARD_FRAME_POINTER_REGNUM, Pmode);

  /* Setup is_leaf as frame_pointer_required may use it.  This function
     is called by sched_init before ira if scheduling is enabled.  */
  crtl->is_leaf = leaf_function_p ();

  /* FIXME: If EXIT_IGNORE_STACK is set, we will not save and restore
     sp for alloca.  So we can't eliminate the frame pointer in that
     case.  At some point, we should improve this by emitting the
     sp-adjusting insns for this case.  */
  frame_pointer_needed
    = (! flag_omit_frame_pointer
       || (cfun->calls_alloca && EXIT_IGNORE_STACK)
       /* We need the frame pointer to catch stack overflow exceptions if
	  the stack pointer is moving (as for the alloca case just above).  */
       || (STACK_CHECK_MOVING_SP
	   && flag_stack_check
	   && flag_exceptions
	   && cfun->can_throw_non_call_exceptions)
       || crtl->accesses_prior_frames
       || (SUPPORTS_STACK_ALIGNMENT && crtl->stack_realign_needed)
       || targetm.frame_pointer_required ());

    /* The chance that FRAME_POINTER_NEEDED is changed from inspecting
       RTL is very small.  So if we use frame pointer for RA and RTL
       actually prevents this, we will spill pseudos assigned to the
       frame pointer in LRA.  */

  if (frame_pointer_needed)
    for (i = 0; i < fp_reg_count; i++)
      df_set_regs_ever_live (HARD_FRAME_POINTER_REGNUM + i, true);
    
  ira_no_alloc_regs = no_unit_alloc_regs;
  CLEAR_HARD_REG_SET (eliminable_regset);

  compute_regs_asm_clobbered ();

  /* Build the regset of all eliminable registers and show we can't
     use those that we already know won't be eliminated.  */
  for (i = 0; i < (int) ARRAY_SIZE (eliminables); i++)
    {
      bool cannot_elim
	= (! targetm.can_eliminate (eliminables[i].from, eliminables[i].to)
	   || (eliminables[i].to == STACK_POINTER_REGNUM && frame_pointer_needed));

      if (!TEST_HARD_REG_BIT (crtl->asm_clobbers, eliminables[i].from))
	{
	    SET_HARD_REG_BIT (eliminable_regset, eliminables[i].from);

	    if (cannot_elim)
	      SET_HARD_REG_BIT (ira_no_alloc_regs, eliminables[i].from);
	}
      else if (cannot_elim)
	error ("%s cannot be used in %<asm%> here",
	       reg_names[eliminables[i].from]);
      else
	df_set_regs_ever_live (eliminables[i].from, true);
    }
  if (!HARD_FRAME_POINTER_IS_FRAME_POINTER)
    {
      for (i = 0; i < fp_reg_count; i++)
	if (!TEST_HARD_REG_BIT (crtl->asm_clobbers,
				HARD_FRAME_POINTER_REGNUM + i))
	  {
	    SET_HARD_REG_BIT (eliminable_regset,
			      HARD_FRAME_POINTER_REGNUM + i);
	    if (frame_pointer_needed)
	      SET_HARD_REG_BIT (ira_no_alloc_regs,
				HARD_FRAME_POINTER_REGNUM + i);
	  }
	else if (frame_pointer_needed)
	  error ("%s cannot be used in %<asm%> here",
		 reg_names[HARD_FRAME_POINTER_REGNUM + i]);
	else
	  df_set_regs_ever_live (HARD_FRAME_POINTER_REGNUM + i, true);
    }
}



/* Vector of substitutions of register numbers,
   used to map pseudo regs into hardware regs.
   This is set up as a result of register allocation.
   Element N is the hard reg assigned to pseudo reg N,
   or is -1 if no hard reg was assigned.
   If N is a hard reg number, element N is N.  */
short *reg_renumber;

/* Set up REG_RENUMBER and CALLER_SAVE_NEEDED (used by reload) from
   the allocation found by IRA.  */
static void
setup_reg_renumber (void)
{
  int regno, hard_regno;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  caller_save_needed = 0;
  FOR_EACH_ALLOCNO (a, ai)
    {
      if (ira_use_lra_p && ALLOCNO_CAP_MEMBER (a) != NULL)
	continue;
      /* There are no caps at this point.  */
      ira_assert (ALLOCNO_CAP_MEMBER (a) == NULL);
      if (! ALLOCNO_ASSIGNED_P (a))
	/* It can happen if A is not referenced but partially anticipated
	   somewhere in a region.  */
	ALLOCNO_ASSIGNED_P (a) = true;
      ira_free_allocno_updated_costs (a);
      hard_regno = ALLOCNO_HARD_REGNO (a);
      regno = ALLOCNO_REGNO (a);
      reg_renumber[regno] = (hard_regno < 0 ? -1 : hard_regno);
      if (hard_regno >= 0)
	{
	  int i, nwords;
	  enum reg_class pclass;
	  ira_object_t obj;
	  
	  pclass = ira_pressure_class_translate[REGNO_REG_CLASS (hard_regno)];
	  nwords = ALLOCNO_NUM_OBJECTS (a);
	  for (i = 0; i < nwords; i++)
	    {
	      obj = ALLOCNO_OBJECT (a, i);
	      OBJECT_TOTAL_CONFLICT_HARD_REGS (obj)
		|= ~reg_class_contents[pclass];
	    }
	  if (ira_need_caller_save_p (a, hard_regno))
	    {
	      ira_assert (!optimize || flag_caller_saves
			  || (ALLOCNO_CALLS_CROSSED_NUM (a)
			      == ALLOCNO_CHEAP_CALLS_CROSSED_NUM (a))
			  || regno >= ira_reg_equiv_len
			  || ira_equiv_no_lvalue_p (regno));
	      caller_save_needed = 1;
	    }
	}
    }
}

/* Set up allocno assignment flags for further allocation
   improvements.  */
static void
setup_allocno_assignment_flags (void)
{
  int hard_regno;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    {
      if (! ALLOCNO_ASSIGNED_P (a))
	/* It can happen if A is not referenced but partially anticipated
	   somewhere in a region.  */
	ira_free_allocno_updated_costs (a);
      hard_regno = ALLOCNO_HARD_REGNO (a);
      /* Don't assign hard registers to allocnos which are destination
	 of removed store at the end of loop.  It has no sense to keep
	 the same value in different hard registers.  It is also
	 impossible to assign hard registers correctly to such
	 allocnos because the cost info and info about intersected
	 calls are incorrect for them.  */
      ALLOCNO_ASSIGNED_P (a) = (hard_regno >= 0
				|| ALLOCNO_EMIT_DATA (a)->mem_optimized_dest_p
				|| (ALLOCNO_MEMORY_COST (a)
				    - ALLOCNO_CLASS_COST (a)) < 0);
      ira_assert
	(hard_regno < 0
	 || ira_hard_reg_in_set_p (hard_regno, ALLOCNO_MODE (a),
				   reg_class_contents[ALLOCNO_CLASS (a)]));
    }
}

/* Evaluate overall allocation cost and the costs for using hard
   registers and memory for allocnos.  */
static void
calculate_allocation_cost (void)
{
  int hard_regno, cost;
  ira_allocno_t a;
  ira_allocno_iterator ai;

  ira_overall_cost = ira_reg_cost = ira_mem_cost = 0;
  FOR_EACH_ALLOCNO (a, ai)
    {
      hard_regno = ALLOCNO_HARD_REGNO (a);
      ira_assert (hard_regno < 0
		  || (ira_hard_reg_in_set_p
		      (hard_regno, ALLOCNO_MODE (a),
		       reg_class_contents[ALLOCNO_CLASS (a)])));
      if (hard_regno < 0)
	{
	  cost = ALLOCNO_MEMORY_COST (a);
	  ira_mem_cost += cost;
	}
      else if (ALLOCNO_HARD_REG_COSTS (a) != NULL)
	{
	  cost = (ALLOCNO_HARD_REG_COSTS (a)
		  [ira_class_hard_reg_index
		   [ALLOCNO_CLASS (a)][hard_regno]]);
	  ira_reg_cost += cost;
	}
      else
	{
	  cost = ALLOCNO_CLASS_COST (a);
	  ira_reg_cost += cost;
	}
      ira_overall_cost += cost;
    }

  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    {
      fprintf (ira_dump_file,
	       "+++Costs: overall %" PRId64
	       ", reg %" PRId64
	       ", mem %" PRId64
	       ", ld %" PRId64
	       ", st %" PRId64
	       ", move %" PRId64,
	       ira_overall_cost, ira_reg_cost, ira_mem_cost,
	       ira_load_cost, ira_store_cost, ira_shuffle_cost);
      fprintf (ira_dump_file, "\n+++       move loops %d, new jumps %d\n",
	       ira_move_loops_num, ira_additional_jumps_num);
    }

}

#ifdef ENABLE_IRA_CHECKING
/* Check the correctness of the allocation.  We do need this because
   of complicated code to transform more one region internal
   representation into one region representation.  */
static void
check_allocation (void)
{
  ira_allocno_t a;
  int hard_regno, nregs, conflict_nregs;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    {
      int n = ALLOCNO_NUM_OBJECTS (a);
      int i;

      if (ALLOCNO_CAP_MEMBER (a) != NULL
	  || (hard_regno = ALLOCNO_HARD_REGNO (a)) < 0)
	continue;
      nregs = hard_regno_nregs (hard_regno, ALLOCNO_MODE (a));
      if (nregs == 1)
	/* We allocated a single hard register.  */
	n = 1;
      else if (n > 1)
	/* We allocated multiple hard registers, and we will test
	   conflicts in a granularity of single hard regs.  */
	nregs = 1;

      for (i = 0; i < n; i++)
	{
	  ira_object_t obj = ALLOCNO_OBJECT (a, i);
	  ira_object_t conflict_obj;
	  ira_object_conflict_iterator oci;
	  int this_regno = hard_regno;
	  if (n > 1)
	    {
	      if (REG_WORDS_BIG_ENDIAN)
		this_regno += n - i - 1;
	      else
		this_regno += i;
	    }
	  FOR_EACH_OBJECT_CONFLICT (obj, conflict_obj, oci)
	    {
	      ira_allocno_t conflict_a = OBJECT_ALLOCNO (conflict_obj);
	      int conflict_hard_regno = ALLOCNO_HARD_REGNO (conflict_a);
	      if (conflict_hard_regno < 0)
		continue;

	      conflict_nregs = hard_regno_nregs (conflict_hard_regno,
						 ALLOCNO_MODE (conflict_a));

	      if (ALLOCNO_NUM_OBJECTS (conflict_a) > 1
		  && conflict_nregs == ALLOCNO_NUM_OBJECTS (conflict_a))
		{
		  if (REG_WORDS_BIG_ENDIAN)
		    conflict_hard_regno += (ALLOCNO_NUM_OBJECTS (conflict_a)
					    - OBJECT_SUBWORD (conflict_obj) - 1);
		  else
		    conflict_hard_regno += OBJECT_SUBWORD (conflict_obj);
		  conflict_nregs = 1;
		}

	      if ((conflict_hard_regno <= this_regno
		 && this_regno < conflict_hard_regno + conflict_nregs)
		|| (this_regno <= conflict_hard_regno
		    && conflict_hard_regno < this_regno + nregs))
		{
		  fprintf (stderr, "bad allocation for %d and %d\n",
			   ALLOCNO_REGNO (a), ALLOCNO_REGNO (conflict_a));
		  gcc_unreachable ();
		}
	    }
	}
    }
}
#endif

/* Allocate REG_EQUIV_INIT.  Set up it from IRA_REG_EQUIV which should
   be already calculated.  */
static void
setup_reg_equiv_init (void)
{
  int i;
  int max_regno = max_reg_num ();

  for (i = 0; i < max_regno; i++)
    reg_equiv_init (i) = ira_reg_equiv[i].init_insns;
}

/* Update equiv regno from movement of FROM_REGNO to TO_REGNO.  INSNS
   are insns which were generated for such movement.  It is assumed
   that FROM_REGNO and TO_REGNO always have the same value at the
   point of any move containing such registers. This function is used
   to update equiv info for register shuffles on the region borders
   and for caller save/restore insns.  */
void
ira_update_equiv_info_by_shuffle_insn (int to_regno, int from_regno, rtx_insn *insns)
{
  rtx_insn *insn;
  rtx x, note;

  if (! ira_reg_equiv[from_regno].defined_p
      && (! ira_reg_equiv[to_regno].defined_p
	  || ((x = ira_reg_equiv[to_regno].memory) != NULL_RTX
	      && ! MEM_READONLY_P (x))))
    return;
  insn = insns;
  if (NEXT_INSN (insn) != NULL_RTX)
    {
      if (! ira_reg_equiv[to_regno].defined_p)
	{
	  ira_assert (ira_reg_equiv[to_regno].init_insns == NULL_RTX);
	  return;
	}
      ira_reg_equiv[to_regno].defined_p = false;
      ira_reg_equiv[to_regno].memory
	= ira_reg_equiv[to_regno].constant
	= ira_reg_equiv[to_regno].invariant
	= ira_reg_equiv[to_regno].init_insns = NULL;
      if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	fprintf (ira_dump_file,
		 "      Invalidating equiv info for reg %d\n", to_regno);
      return;
    }
  /* It is possible that FROM_REGNO still has no equivalence because
     in shuffles to_regno<-from_regno and from_regno<-to_regno the 2nd
     insn was not processed yet.  */
  if (ira_reg_equiv[from_regno].defined_p)
    {
      ira_reg_equiv[to_regno].defined_p = true;
      if ((x = ira_reg_equiv[from_regno].memory) != NULL_RTX)
	{
	  ira_assert (ira_reg_equiv[from_regno].invariant == NULL_RTX
		      && ira_reg_equiv[from_regno].constant == NULL_RTX);
	  ira_assert (ira_reg_equiv[to_regno].memory == NULL_RTX
		      || rtx_equal_p (ira_reg_equiv[to_regno].memory, x));
	  ira_reg_equiv[to_regno].memory = x;
	  if (! MEM_READONLY_P (x))
	    /* We don't add the insn to insn init list because memory
	       equivalence is just to say what memory is better to use
	       when the pseudo is spilled.  */
	    return;
	}
      else if ((x = ira_reg_equiv[from_regno].constant) != NULL_RTX)
	{
	  ira_assert (ira_reg_equiv[from_regno].invariant == NULL_RTX);
	  ira_assert (ira_reg_equiv[to_regno].constant == NULL_RTX
		      || rtx_equal_p (ira_reg_equiv[to_regno].constant, x));
	  ira_reg_equiv[to_regno].constant = x;
	}
      else
	{
	  x = ira_reg_equiv[from_regno].invariant;
	  ira_assert (x != NULL_RTX);
	  ira_assert (ira_reg_equiv[to_regno].invariant == NULL_RTX
		      || rtx_equal_p (ira_reg_equiv[to_regno].invariant, x));
	  ira_reg_equiv[to_regno].invariant = x;
	}
      if (find_reg_note (insn, REG_EQUIV, x) == NULL_RTX)
	{
	  note = set_unique_reg_note (insn, REG_EQUIV, copy_rtx (x));
	  gcc_assert (note != NULL_RTX);
	  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
	    {
	      fprintf (ira_dump_file,
		       "      Adding equiv note to insn %u for reg %d ",
		       INSN_UID (insn), to_regno);
	      dump_value_slim (ira_dump_file, x, 1);
	      fprintf (ira_dump_file, "\n");
	    }
	}
    }
  ira_reg_equiv[to_regno].init_insns
    = gen_rtx_INSN_LIST (VOIDmode, insn,
			 ira_reg_equiv[to_regno].init_insns);
  if (internal_flag_ira_verbose > 3 && ira_dump_file != NULL)
    fprintf (ira_dump_file,
	     "      Adding equiv init move insn %u to reg %d\n",
	     INSN_UID (insn), to_regno);
}

/* Fix values of array REG_EQUIV_INIT after live range splitting done
   by IRA.  */
static void
fix_reg_equiv_init (void)
{
  int max_regno = max_reg_num ();
  int i, new_regno, max;
  rtx set;
  rtx_insn_list *x, *next, *prev;
  rtx_insn *insn;

  if (max_regno_before_ira < max_regno)
    {
      max = vec_safe_length (reg_equivs);
      grow_reg_equivs ();
      for (i = FIRST_PSEUDO_REGISTER; i < max; i++)
	for (prev = NULL, x = reg_equiv_init (i);
	     x != NULL_RTX;
	     x = next)
	  {
	    next = x->next ();
	    insn = x->insn ();
	    set = single_set (insn);
	    ira_assert (set != NULL_RTX
			&& (REG_P (SET_DEST (set)) || REG_P (SET_SRC (set))));
	    if (REG_P (SET_DEST (set))
		&& ((int) REGNO (SET_DEST (set)) == i
		    || (int) ORIGINAL_REGNO (SET_DEST (set)) == i))
	      new_regno = REGNO (SET_DEST (set));
	    else if (REG_P (SET_SRC (set))
		     && ((int) REGNO (SET_SRC (set)) == i
			 || (int) ORIGINAL_REGNO (SET_SRC (set)) == i))
	      new_regno = REGNO (SET_SRC (set));
	    else
 	      gcc_unreachable ();
	    if (new_regno == i)
	      prev = x;
	    else
	      {
		/* Remove the wrong list element.  */
		if (prev == NULL_RTX)
		  reg_equiv_init (i) = next;
		else
		  XEXP (prev, 1) = next;
		XEXP (x, 1) = reg_equiv_init (new_regno);
		reg_equiv_init (new_regno) = x;
	      }
	  }
    }
}

#ifdef ENABLE_IRA_CHECKING
/* Print redundant memory-memory copies.  */
static void
print_redundant_copies (void)
{
  int hard_regno;
  ira_allocno_t a;
  ira_copy_t cp, next_cp;
  ira_allocno_iterator ai;

  FOR_EACH_ALLOCNO (a, ai)
    {
      if (ALLOCNO_CAP_MEMBER (a) != NULL)
	/* It is a cap.  */
	continue;
      hard_regno = ALLOCNO_HARD_REGNO (a);
      if (hard_regno >= 0)
	continue;
      for (cp = ALLOCNO_COPIES (a); cp != NULL; cp = next_cp)
	if (cp->first == a)
	  next_cp = cp->next_first_allocno_copy;
	else
	  {
	    next_cp = cp->next_second_allocno_copy;
	    if (internal_flag_ira_verbose > 4 && ira_dump_file != NULL
		&& cp->insn != NULL_RTX
		&& ALLOCNO_HARD_REGNO (cp->first) == hard_regno)
	      fprintf (ira_dump_file,
		       "        Redundant move from %d(freq %d):%d\n",
		       INSN_UID (cp->insn), cp->freq, hard_regno);
	  }
    }
}
#endif

/* Setup preferred and alternative classes for new pseudo-registers
   created by IRA starting with START.  */
static void
setup_preferred_alternate_classes_for_new_pseudos (int start)
{
  int i, old_regno;
  int max_regno = max_reg_num ();

  for (i = start; i < max_regno; i++)
    {
      old_regno = ORIGINAL_REGNO (regno_reg_rtx[i]);
      ira_assert (i != old_regno);
      setup_reg_classes (i, reg_preferred_class (old_regno),
			 reg_alternate_class (old_regno),
			 reg_allocno_class (old_regno));
      if (internal_flag_ira_verbose > 2 && ira_dump_file != NULL)
	fprintf (ira_dump_file,
		 "    New r%d: setting preferred %s, alternative %s\n",
		 i, reg_class_names[reg_preferred_class (old_regno)],
		 reg_class_names[reg_alternate_class (old_regno)]);
    }
}


/* The number of entries allocated in reg_info.  */
static int allocated_reg_info_size;

/* Regional allocation can create new pseudo-registers.  This function
   expands some arrays for pseudo-registers.  */
static void
expand_reg_info (void)
{
  int i;
  int size = max_reg_num ();

  resize_reg_info ();
  for (i = allocated_reg_info_size; i < size; i++)
    setup_reg_classes (i, GENERAL_REGS, ALL_REGS, GENERAL_REGS);
  setup_preferred_alternate_classes_for_new_pseudos (allocated_reg_info_size);
  allocated_reg_info_size = size;
}

/* Return TRUE if there is too high register pressure in the function.
   It is used to decide when stack slot sharing is worth to do.  */
static bool
too_high_register_pressure_p (void)
{
  int i;
  enum reg_class pclass;

  for (i = 0; i < ira_pressure_classes_num; i++)
    {
      pclass = ira_pressure_classes[i];
      if (ira_loop_tree_root->reg_pressure[pclass] > 10000)
	return true;
    }
  return false;
}



/* Indicate that hard register number FROM was eliminated and replaced with
   an offset from hard register number TO.  The status of hard registers live
   at the start of a basic block is updated by replacing a use of FROM with
   a use of TO.  */

void
mark_elimination (int from, int to)
{
  basic_block bb;
  bitmap r;

  FOR_EACH_BB_FN (bb, cfun)
    {
      r = DF_LR_IN (bb);
      if (bitmap_bit_p (r, from))
	{
	  bitmap_clear_bit (r, from);
	  bitmap_set_bit (r, to);
	}
      if (! df_live)
        continue;
      r = DF_LIVE_IN (bb);
      if (bitmap_bit_p (r, from))
	{
	  bitmap_clear_bit (r, from);
	  bitmap_set_bit (r, to);
	}
    }
}



/* The length of the following array.  */
int ira_reg_equiv_len;

/* Info about equiv. info for each register.  */
struct ira_reg_equiv_s *ira_reg_equiv;

/* Expand ira_reg_equiv if necessary.  */
void
ira_expand_reg_equiv (void)
{
  int old = ira_reg_equiv_len;

  if (ira_reg_equiv_len > max_reg_num ())
    return;
  ira_reg_equiv_len = max_reg_num () * 3 / 2 + 1;
  ira_reg_equiv
    = (struct ira_reg_equiv_s *) xrealloc (ira_reg_equiv,
					 ira_reg_equiv_len
					 * sizeof (struct ira_reg_equiv_s));
  gcc_assert (old < ira_reg_equiv_len);
  memset (ira_reg_equiv + old, 0,
	  sizeof (struct ira_reg_equiv_s) * (ira_reg_equiv_len - old));
}

static void
init_reg_equiv (void)
{
  ira_reg_equiv_len = 0;
  ira_reg_equiv = NULL;
  ira_expand_reg_equiv ();
}

static void
finish_reg_equiv (void)
{
  free (ira_reg_equiv);
}



struct equivalence
{
  /* Set when a REG_EQUIV note is found or created.  Use to
     keep track of what memory accesses might be created later,
     e.g. by reload.  */
  rtx replacement;
  rtx *src_p;

  /* The list of each instruction which initializes this register.

     NULL indicates we know nothing about this register's equivalence
     properties.

     An INSN_LIST with a NULL insn indicates this pseudo is already
     known to not have a valid equivalence.  */
  rtx_insn_list *init_insns;

  /* Loop depth is used to recognize equivalences which appear
     to be present within the same loop (or in an inner loop).  */
  short loop_depth;
  /* Nonzero if this had a preexisting REG_EQUIV note.  */
  unsigned char is_arg_equivalence : 1;
  /* Set when an attempt should be made to replace a register
     with the associated src_p entry.  */
  unsigned char replace : 1;
  /* Set if this register has no known equivalence.  */
  unsigned char no_equiv : 1;
  /* Set if this register is mentioned in a paradoxical subreg.  */
  unsigned char pdx_subregs : 1;
};

/* reg_equiv[N] (where N is a pseudo reg number) is the equivalence
   structure for that register.  */
static struct equivalence *reg_equiv;

/* Used for communication between the following two functions.  */
struct equiv_mem_data
{
  /* A MEM that we wish to ensure remains unchanged.  */
  rtx equiv_mem;

  /* Set true if EQUIV_MEM is modified.  */
  bool equiv_mem_modified;
};

/* If EQUIV_MEM is modified by modifying DEST, indicate that it is modified.
   Called via note_stores.  */
static void
validate_equiv_mem_from_store (rtx dest, const_rtx set ATTRIBUTE_UNUSED,
			       void *data)
{
  struct equiv_mem_data *info = (struct equiv_mem_data *) data;

  if ((REG_P (dest)
       && reg_overlap_mentioned_p (dest, info->equiv_mem))
      || (MEM_P (dest)
	  && anti_dependence (info->equiv_mem, dest)))
    info->equiv_mem_modified = true;
}

enum valid_equiv { valid_none, valid_combine, valid_reload };

/* Verify that no store between START and the death of REG invalidates
   MEMREF.  MEMREF is invalidated by modifying a register used in MEMREF,
   by storing into an overlapping memory location, or with a non-const
   CALL_INSN.

   Return VALID_RELOAD if MEMREF remains valid for both reload and
   combine_and_move insns, VALID_COMBINE if only valid for
   combine_and_move_insns, and VALID_NONE otherwise.  */
static enum valid_equiv
validate_equiv_mem (rtx_insn *start, rtx reg, rtx memref)
{
  rtx_insn *insn;
  rtx note;
  struct equiv_mem_data info = { memref, false };
  enum valid_equiv ret = valid_reload;

  /* If the memory reference has side effects or is volatile, it isn't a
     valid equivalence.  */
  if (side_effects_p (memref))
    return valid_none;

  for (insn = start; insn; insn = NEXT_INSN (insn))
    {
      if (!INSN_P (insn))
	continue;

      if (find_reg_note (insn, REG_DEAD, reg))
	return ret;

      if (CALL_P (insn))
	{
	  /* We can combine a reg def from one insn into a reg use in
	     another over a call if the memory is readonly or the call
	     const/pure.  However, we can't set reg_equiv notes up for
	     reload over any call.  The problem is the equivalent form
	     may reference a pseudo which gets assigned a call
	     clobbered hard reg.  When we later replace REG with its
	     equivalent form, the value in the call-clobbered reg has
	     been changed and all hell breaks loose.  */
	  ret = valid_combine;
	  if (!MEM_READONLY_P (memref)
	      && !RTL_CONST_OR_PURE_CALL_P (insn))
	    return valid_none;
	}

      note_stores (insn, validate_equiv_mem_from_store, &info);
      if (info.equiv_mem_modified)
	return valid_none;

      /* If a register mentioned in MEMREF is modified via an
	 auto-increment, we lose the equivalence.  Do the same if one
	 dies; although we could extend the life, it doesn't seem worth
	 the trouble.  */

      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	if ((REG_NOTE_KIND (note) == REG_INC
	     || REG_NOTE_KIND (note) == REG_DEAD)
	    && REG_P (XEXP (note, 0))
	    && reg_overlap_mentioned_p (XEXP (note, 0), memref))
	  return valid_none;
    }

  return valid_none;
}

/* Returns zero if X is known to be invariant.  */
static int
equiv_init_varies_p (rtx x)
{
  RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case MEM:
      return !MEM_READONLY_P (x) || equiv_init_varies_p (XEXP (x, 0));

    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case REG:
      return reg_equiv[REGNO (x)].replace == 0 && rtx_varies_p (x, 0);

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;

      /* Fall through.  */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      {
	if (equiv_init_varies_p (XEXP (x, i)))
	  return 1;
      }
    else if (fmt[i] == 'E')
      {
	int j;
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (equiv_init_varies_p (XVECEXP (x, i, j)))
	    return 1;
      }

  return 0;
}

/* Returns nonzero if X (used to initialize register REGNO) is movable.
   X is only movable if the registers it uses have equivalent initializations
   which appear to be within the same loop (or in an inner loop) and movable
   or if they are not candidates for local_alloc and don't vary.  */
static int
equiv_init_movable_p (rtx x, int regno)
{
  int i, j;
  const char *fmt;
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case SET:
      return equiv_init_movable_p (SET_SRC (x), regno);

    case CC0:
    case CLOBBER:
      return 0;

    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return 0;

    case REG:
      return ((reg_equiv[REGNO (x)].loop_depth >= reg_equiv[regno].loop_depth
	       && reg_equiv[REGNO (x)].replace)
	      || (REG_BASIC_BLOCK (REGNO (x)) < NUM_FIXED_BLOCKS
		  && ! rtx_varies_p (x, 0)));

    case UNSPEC_VOLATILE:
      return 0;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 0;

      /* Fall through.  */

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    switch (fmt[i])
      {
      case 'e':
	if (! equiv_init_movable_p (XEXP (x, i), regno))
	  return 0;
	break;
      case 'E':
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (! equiv_init_movable_p (XVECEXP (x, i, j), regno))
	    return 0;
	break;
      }

  return 1;
}

static bool memref_referenced_p (rtx memref, rtx x, bool read_p);

/* Auxiliary function for memref_referenced_p.  Process setting X for
   MEMREF store.  */
static bool
process_set_for_memref_referenced_p (rtx memref, rtx x)
{
  /* If we are setting a MEM, it doesn't count (its address does), but any
     other SET_DEST that has a MEM in it is referencing the MEM.  */
  if (MEM_P (x))
    {
      if (memref_referenced_p (memref, XEXP (x, 0), true))
	return true;
    }
  else if (memref_referenced_p (memref, x, false))
    return true;
  
  return false;
}

/* TRUE if X references a memory location (as a read if READ_P) that
   would be affected by a store to MEMREF.  */
static bool
memref_referenced_p (rtx memref, rtx x, bool read_p)
{
  int i, j;
  const char *fmt;
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    CASE_CONST_ANY:
    case PC:
    case CC0:
    case HIGH:
    case LO_SUM:
      return false;

    case REG:
      return (reg_equiv[REGNO (x)].replacement
	      && memref_referenced_p (memref,
				      reg_equiv[REGNO (x)].replacement, read_p));

    case MEM:
      /* Memory X might have another effective type than MEMREF.  */
      if (read_p || true_dependence (memref, VOIDmode, x))
	return true;
      break;

    case SET:
      if (process_set_for_memref_referenced_p (memref, SET_DEST (x)))
	return true;

      return memref_referenced_p (memref, SET_SRC (x), true);

    case CLOBBER:
      if (process_set_for_memref_referenced_p (memref, XEXP (x, 0)))
	return true;

      return false;

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
      if (process_set_for_memref_referenced_p (memref, XEXP (x, 0)))
	return true;

      return memref_referenced_p (memref, XEXP (x, 0), true);
      
    case POST_MODIFY:
    case PRE_MODIFY:
      /* op0 = op0 + op1 */
      if (process_set_for_memref_referenced_p (memref, XEXP (x, 0)))
	return true;

      if (memref_referenced_p (memref, XEXP (x, 0), true))
	return true;

      return memref_referenced_p (memref, XEXP (x, 1), true);

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    switch (fmt[i])
      {
      case 'e':
	if (memref_referenced_p (memref, XEXP (x, i), read_p))
	  return true;
	break;
      case 'E':
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (memref_referenced_p (memref, XVECEXP (x, i, j), read_p))
	    return true;
	break;
      }

  return false;
}

/* TRUE if some insn in the range (START, END] references a memory location
   that would be affected by a store to MEMREF.

   Callers should not call this routine if START is after END in the
   RTL chain.  */

static int
memref_used_between_p (rtx memref, rtx_insn *start, rtx_insn *end)
{
  rtx_insn *insn;

  for (insn = NEXT_INSN (start);
       insn && insn != NEXT_INSN (end);
       insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      if (memref_referenced_p (memref, PATTERN (insn), false))
	return 1;

      /* Nonconst functions may access memory.  */
      if (CALL_P (insn) && (! RTL_CONST_CALL_P (insn)))
	return 1;
    }

  gcc_assert (insn == NEXT_INSN (end));
  return 0;
}

/* Mark REG as having no known equivalence.
   Some instructions might have been processed before and furnished
   with REG_EQUIV notes for this register; these notes will have to be
   removed.
   STORE is the piece of RTL that does the non-constant / conflicting
   assignment - a SET, CLOBBER or REG_INC note.  It is currently not used,
   but needs to be there because this function is called from note_stores.  */
static void
no_equiv (rtx reg, const_rtx store ATTRIBUTE_UNUSED,
	  void *data ATTRIBUTE_UNUSED)
{
  int regno;
  rtx_insn_list *list;

  if (!REG_P (reg))
    return;
  regno = REGNO (reg);
  reg_equiv[regno].no_equiv = 1;
  list = reg_equiv[regno].init_insns;
  if (list && list->insn () == NULL)
    return;
  reg_equiv[regno].init_insns = gen_rtx_INSN_LIST (VOIDmode, NULL_RTX, NULL);
  reg_equiv[regno].replacement = NULL_RTX;
  /* This doesn't matter for equivalences made for argument registers, we
     should keep their initialization insns.  */
  if (reg_equiv[regno].is_arg_equivalence)
    return;
  ira_reg_equiv[regno].defined_p = false;
  ira_reg_equiv[regno].init_insns = NULL;
  for (; list; list = list->next ())
    {
      rtx_insn *insn = list->insn ();
      remove_note (insn, find_reg_note (insn, REG_EQUIV, NULL_RTX));
    }
}

/* Check whether the SUBREG is a paradoxical subreg and set the result
   in PDX_SUBREGS.  */

static void
set_paradoxical_subreg (rtx_insn *insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    {
      const_rtx subreg = *iter;
      if (GET_CODE (subreg) == SUBREG)
	{
	  const_rtx reg = SUBREG_REG (subreg);
	  if (REG_P (reg) && paradoxical_subreg_p (subreg))
	    reg_equiv[REGNO (reg)].pdx_subregs = true;
	}
    }
}

/* In DEBUG_INSN location adjust REGs from CLEARED_REGS bitmap to the
   equivalent replacement.  */

static rtx
adjust_cleared_regs (rtx loc, const_rtx old_rtx ATTRIBUTE_UNUSED, void *data)
{
  if (REG_P (loc))
    {
      bitmap cleared_regs = (bitmap) data;
      if (bitmap_bit_p (cleared_regs, REGNO (loc)))
	return simplify_replace_fn_rtx (copy_rtx (*reg_equiv[REGNO (loc)].src_p),
					NULL_RTX, adjust_cleared_regs, data);
    }
  return NULL_RTX;
}

/* Given register REGNO is set only once, return true if the defining
   insn dominates all uses.  */

static bool
def_dominates_uses (int regno)
{
  df_ref def = DF_REG_DEF_CHAIN (regno);

  struct df_insn_info *def_info = DF_REF_INSN_INFO (def);
  /* If this is an artificial def (eh handler regs, hard frame pointer
     for non-local goto, regs defined on function entry) then def_info
     is NULL and the reg is always live before any use.  We might
     reasonably return true in that case, but since the only call
     of this function is currently here in ira.c when we are looking
     at a defining insn we can't have an artificial def as that would
     bump DF_REG_DEF_COUNT.  */
  gcc_assert (DF_REG_DEF_COUNT (regno) == 1 && def_info != NULL);

  rtx_insn *def_insn = DF_REF_INSN (def);
  basic_block def_bb = BLOCK_FOR_INSN (def_insn);

  for (df_ref use = DF_REG_USE_CHAIN (regno);
       use;
       use = DF_REF_NEXT_REG (use))
    {
      struct df_insn_info *use_info = DF_REF_INSN_INFO (use);
      /* Only check real uses, not artificial ones.  */
      if (use_info)
	{
	  rtx_insn *use_insn = DF_REF_INSN (use);
	  if (!DEBUG_INSN_P (use_insn))
	    {
	      basic_block use_bb = BLOCK_FOR_INSN (use_insn);
	      if (use_bb != def_bb
		  ? !dominated_by_p (CDI_DOMINATORS, use_bb, def_bb)
		  : DF_INSN_INFO_LUID (use_info) < DF_INSN_INFO_LUID (def_info))
		return false;
	    }
	}
    }
  return true;
}

/* Scan the instructions before update_equiv_regs.  Record which registers
   are referenced as paradoxical subregs.  Also check for cases in which
   the current function needs to save a register that one of its call
   instructions clobbers.

   These things are logically unrelated, but it's more efficient to do
   them together.  */

static void
update_equiv_regs_prescan (void)
{
  basic_block bb;
  rtx_insn *insn;
  function_abi_aggregator callee_abis;

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      if (NONDEBUG_INSN_P (insn))
	{
	  set_paradoxical_subreg (insn);
	  if (CALL_P (insn))
	    callee_abis.note_callee_abi (insn_callee_abi (insn));
	}

  HARD_REG_SET extra_caller_saves = callee_abis.caller_save_regs (*crtl->abi);
  if (!hard_reg_set_empty_p (extra_caller_saves))
    for (unsigned int regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
      if (TEST_HARD_REG_BIT (extra_caller_saves, regno))
	df_set_regs_ever_live (regno, true);
}

/* Find registers that are equivalent to a single value throughout the
   compilation (either because they can be referenced in memory or are
   set once from a single constant).  Lower their priority for a
   register.

   If such a register is only referenced once, try substituting its
   value into the using insn.  If it succeeds, we can eliminate the
   register completely.

   Initialize init_insns in ira_reg_equiv array.  */
static void
update_equiv_regs (void)
{
  rtx_insn *insn;
  basic_block bb;

  /* Scan the insns and find which registers have equivalences.  Do this
     in a separate scan of the insns because (due to -fcse-follow-jumps)
     a register can be set below its use.  */
  bitmap setjmp_crosses = regstat_get_setjmp_crosses ();
  FOR_EACH_BB_FN (bb, cfun)
    {
      int loop_depth = bb_loop_depth (bb);

      for (insn = BB_HEAD (bb);
	   insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  rtx note;
	  rtx set;
	  rtx dest, src;
	  int regno;

	  if (! INSN_P (insn))
	    continue;

	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_INC)
	      no_equiv (XEXP (note, 0), note, NULL);

	  set = single_set (insn);

	  /* If this insn contains more (or less) than a single SET,
	     only mark all destinations as having no known equivalence.  */
	  if (set == NULL_RTX
	      || side_effects_p (SET_SRC (set)))
	    {
	      note_pattern_stores (PATTERN (insn), no_equiv, NULL);
	      continue;
	    }
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      int i;

	      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		{
		  rtx part = XVECEXP (PATTERN (insn), 0, i);
		  if (part != set)
		    note_pattern_stores (part, no_equiv, NULL);
		}
	    }

	  dest = SET_DEST (set);
	  src = SET_SRC (set);

	  /* See if this is setting up the equivalence between an argument
	     register and its stack slot.  */
	  note = find_reg_note (insn, REG_EQUIV, NULL_RTX);
	  if (note)
	    {
	      gcc_assert (REG_P (dest));
	      regno = REGNO (dest);

	      /* Note that we don't want to clear init_insns in
		 ira_reg_equiv even if there are multiple sets of this
		 register.  */
	      reg_equiv[regno].is_arg_equivalence = 1;

	      /* The insn result can have equivalence memory although
		 the equivalence is not set up by the insn.  We add
		 this insn to init insns as it is a flag for now that
		 regno has an equivalence.  We will remove the insn
		 from init insn list later.  */
	      if (rtx_equal_p (src, XEXP (note, 0)) || MEM_P (XEXP (note, 0)))
		ira_reg_equiv[regno].init_insns
		  = gen_rtx_INSN_LIST (VOIDmode, insn,
				       ira_reg_equiv[regno].init_insns);

	      /* Continue normally in case this is a candidate for
		 replacements.  */
	    }

	  if (!optimize)
	    continue;

	  /* We only handle the case of a pseudo register being set
	     once, or always to the same value.  */
	  /* ??? The mn10200 port breaks if we add equivalences for
	     values that need an ADDRESS_REGS register and set them equivalent
	     to a MEM of a pseudo.  The actual problem is in the over-conservative
	     handling of INPADDR_ADDRESS / INPUT_ADDRESS / INPUT triples in
	     calculate_needs, but we traditionally work around this problem
	     here by rejecting equivalences when the destination is in a register
	     that's likely spilled.  This is fragile, of course, since the
	     preferred class of a pseudo depends on all instructions that set
	     or use it.  */

	  if (!REG_P (dest)
	      || (regno = REGNO (dest)) < FIRST_PSEUDO_REGISTER
	      || (reg_equiv[regno].init_insns
		  && reg_equiv[regno].init_insns->insn () == NULL)
	      || (targetm.class_likely_spilled_p (reg_preferred_class (regno))
		  && MEM_P (src) && ! reg_equiv[regno].is_arg_equivalence))
	    {
	      /* This might be setting a SUBREG of a pseudo, a pseudo that is
		 also set somewhere else to a constant.  */
	      note_pattern_stores (set, no_equiv, NULL);
	      continue;
	    }

	  /* Don't set reg mentioned in a paradoxical subreg
	     equivalent to a mem.  */
	  if (MEM_P (src) && reg_equiv[regno].pdx_subregs)
	    {
	      note_pattern_stores (set, no_equiv, NULL);
	      continue;
	    }

	  note = find_reg_note (insn, REG_EQUAL, NULL_RTX);

	  /* cse sometimes generates function invariants, but doesn't put a
	     REG_EQUAL note on the insn.  Since this note would be redundant,
	     there's no point creating it earlier than here.  */
	  if (! note && ! rtx_varies_p (src, 0))
	    note = set_unique_reg_note (insn, REG_EQUAL, copy_rtx (src));

	  /* Don't bother considering a REG_EQUAL note containing an EXPR_LIST
	     since it represents a function call.  */
	  if (note && GET_CODE (XEXP (note, 0)) == EXPR_LIST)
	    note = NULL_RTX;

	  if (DF_REG_DEF_COUNT (regno) != 1)
	    {
	      bool equal_p = true;
	      rtx_insn_list *list;

	      /* If we have already processed this pseudo and determined it
		 cannot have an equivalence, then honor that decision.  */
	      if (reg_equiv[regno].no_equiv)
		continue;

	      if (! note
		  || rtx_varies_p (XEXP (note, 0), 0)
		  || (reg_equiv[regno].replacement
		      && ! rtx_equal_p (XEXP (note, 0),
					reg_equiv[regno].replacement)))
		{
		  no_equiv (dest, set, NULL);
		  continue;
		}

	      list = reg_equiv[regno].init_insns;
	      for (; list; list = list->next ())
		{
		  rtx note_tmp;
		  rtx_insn *insn_tmp;

		  insn_tmp = list->insn ();
		  note_tmp = find_reg_note (insn_tmp, REG_EQUAL, NULL_RTX);
		  gcc_assert (note_tmp);
		  if (! rtx_equal_p (XEXP (note, 0), XEXP (note_tmp, 0)))
		    {
		      equal_p = false;
		      break;
		    }
		}

	      if (! equal_p)
		{
		  no_equiv (dest, set, NULL);
		  continue;
		}
	    }

	  /* Record this insn as initializing this register.  */
	  reg_equiv[regno].init_insns
	    = gen_rtx_INSN_LIST (VOIDmode, insn, reg_equiv[regno].init_insns);

	  /* If this register is known to be equal to a constant, record that
	     it is always equivalent to the constant.
	     Note that it is possible to have a register use before
	     the def in loops (see gcc.c-torture/execute/pr79286.c)
	     where the reg is undefined on first use.  If the def insn
	     won't trap we can use it as an equivalence, effectively
	     choosing the "undefined" value for the reg to be the
	     same as the value set by the def.  */
	  if (DF_REG_DEF_COUNT (regno) == 1
	      && note
	      && !rtx_varies_p (XEXP (note, 0), 0)
	      && (!may_trap_or_fault_p (XEXP (note, 0))
		  || def_dominates_uses (regno)))
	    {
	      rtx note_value = XEXP (note, 0);
	      remove_note (insn, note);
	      set_unique_reg_note (insn, REG_EQUIV, note_value);
	    }

	  /* If this insn introduces a "constant" register, decrease the priority
	     of that register.  Record this insn if the register is only used once
	     more and the equivalence value is the same as our source.

	     The latter condition is checked for two reasons:  First, it is an
	     indication that it may be more efficient to actually emit the insn
	     as written (if no registers are available, reload will substitute
	     the equivalence).  Secondly, it avoids problems with any registers
	     dying in this insn whose death notes would be missed.

	     If we don't have a REG_EQUIV note, see if this insn is loading
	     a register used only in one basic block from a MEM.  If so, and the
	     MEM remains unchanged for the life of the register, add a REG_EQUIV
	     note.  */
	  note = find_reg_note (insn, REG_EQUIV, NULL_RTX);

	  rtx replacement = NULL_RTX;
	  if (note)
	    replacement = XEXP (note, 0);
	  else if (REG_BASIC_BLOCK (regno) >= NUM_FIXED_BLOCKS
		   && MEM_P (SET_SRC (set)))
	    {
	      enum valid_equiv validity;
	      validity = validate_equiv_mem (insn, dest, SET_SRC (set));
	      if (validity != valid_none)
		{
		  replacement = copy_rtx (SET_SRC (set));
		  if (validity == valid_reload)
		    note = set_unique_reg_note (insn, REG_EQUIV, replacement);
		}
	    }

	  /* If we haven't done so, record for reload that this is an
	     equivalencing insn.  */
	  if (note && !reg_equiv[regno].is_arg_equivalence)
	    ira_reg_equiv[regno].init_insns
	      = gen_rtx_INSN_LIST (VOIDmode, insn,
				   ira_reg_equiv[regno].init_insns);

	  if (replacement)
	    {
	      reg_equiv[regno].replacement = replacement;
	      reg_equiv[regno].src_p = &SET_SRC (set);
	      reg_equiv[regno].loop_depth = (short) loop_depth;

	      /* Don't mess with things live during setjmp.  */
	      if (optimize && !bitmap_bit_p (setjmp_crosses, regno))
		{
		  /* If the register is referenced exactly twice, meaning it is
		     set once and used once, indicate that the reference may be
		     replaced by the equivalence we computed above.  Do this
		     even if the register is only used in one block so that
		     dependencies can be handled where the last register is
		     used in a different block (i.e. HIGH / LO_SUM sequences)
		     and to reduce the number of registers alive across
		     calls.  */

		  if (REG_N_REFS (regno) == 2
		      && (rtx_equal_p (replacement, src)
			  || ! equiv_init_varies_p (src))
		      && NONJUMP_INSN_P (insn)
		      && equiv_init_movable_p (PATTERN (insn), regno))
		    reg_equiv[regno].replace = 1;
		}
	    }
	}
    }
}

/* For insns that set a MEM to the contents of a REG that is only used
   in a single basic block, see if the register is always equivalent
   to that memory location and if moving the store from INSN to the
   insn that sets REG is safe.  If so, put a REG_EQUIV note on the
   initializing insn.  */
static void
add_store_equivs (void)
{
  auto_bitmap seen_insns;

  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx set, src, dest;
      unsigned regno;
      rtx_insn *init_insn;

      bitmap_set_bit (seen_insns, INSN_UID (insn));

      if (! INSN_P (insn))
	continue;

      set = single_set (insn);
      if (! set)
	continue;

      dest = SET_DEST (set);
      src = SET_SRC (set);

      /* Don't add a REG_EQUIV note if the insn already has one.  The existing
	 REG_EQUIV is likely more useful than the one we are adding.  */
      if (MEM_P (dest) && REG_P (src)
	  && (regno = REGNO (src)) >= FIRST_PSEUDO_REGISTER
	  && REG_BASIC_BLOCK (regno) >= NUM_FIXED_BLOCKS
	  && DF_REG_DEF_COUNT (regno) == 1
	  && ! reg_equiv[regno].pdx_subregs
	  && reg_equiv[regno].init_insns != NULL
	  && (init_insn = reg_equiv[regno].init_insns->insn ()) != 0
	  && bitmap_bit_p (seen_insns, INSN_UID (init_insn))
	  && ! find_reg_note (init_insn, REG_EQUIV, NULL_RTX)
	  && validate_equiv_mem (init_insn, src, dest) == valid_reload
	  && ! memref_used_between_p (dest, init_insn, insn)
	  /* Attaching a REG_EQUIV note will fail if INIT_INSN has
	     multiple sets.  */
	  && set_unique_reg_note (init_insn, REG_EQUIV, copy_rtx (dest)))
	{
	  /* This insn makes the equivalence, not the one initializing
	     the register.  */
	  ira_reg_equiv[regno].init_insns
	    = gen_rtx_INSN_LIST (VOIDmode, insn, NULL_RTX);
	  df_notes_rescan (init_insn);
	  if (dump_file)
	    fprintf (dump_file,
		     "Adding REG_EQUIV to insn %d for source of insn %d\n",
		     INSN_UID (init_insn),
		     INSN_UID (insn));
	}
    }
}

/* Scan all regs killed in an insn to see if any of them are registers
   only used that once.  If so, see if we can replace the reference
   with the equivalent form.  If we can, delete the initializing
   reference and this register will go away.  If we can't replace the
   reference, and the initializing reference is within the same loop
   (or in an inner loop), then move the register initialization just
   before the use, so that they are in the same basic block.  */
static void
combine_and_move_insns (void)
{
  auto_bitmap cleared_regs;
  int max = max_reg_num ();

  for (int regno = FIRST_PSEUDO_REGISTER; regno < max; regno++)
    {
      if (!reg_equiv[regno].replace)
	continue;

      rtx_insn *use_insn = 0;
      for (df_ref use = DF_REG_USE_CHAIN (regno);
	   use;
	   use = DF_REF_NEXT_REG (use))
	if (DF_REF_INSN_INFO (use))
	  {
	    if (DEBUG_INSN_P (DF_REF_INSN (use)))
	      continue;
	    gcc_assert (!use_insn);
	    use_insn = DF_REF_INSN (use);
	  }
      gcc_assert (use_insn);

      /* Don't substitute into jumps.  indirect_jump_optimize does
	 this for anything we are prepared to handle.  */
      if (JUMP_P (use_insn))
	continue;

      /* Also don't substitute into a conditional trap insn -- it can become
	 an unconditional trap, and that is a flow control insn.  */
      if (GET_CODE (PATTERN (use_insn)) == TRAP_IF)
	continue;

      df_ref def = DF_REG_DEF_CHAIN (regno);
      gcc_assert (DF_REG_DEF_COUNT (regno) == 1 && DF_REF_INSN_INFO (def));
      rtx_insn *def_insn = DF_REF_INSN (def);

      /* We may not move instructions that can throw, since that
	 changes basic block boundaries and we are not prepared to
	 adjust the CFG to match.  */
      if (can_throw_internal (def_insn))
	continue;

      basic_block use_bb = BLOCK_FOR_INSN (use_insn);
      basic_block def_bb = BLOCK_FOR_INSN (def_insn);
      if (bb_loop_depth (use_bb) > bb_loop_depth (def_bb))
	continue;

      if (asm_noperands (PATTERN (def_insn)) < 0
	  && validate_replace_rtx (regno_reg_rtx[regno],
				   *reg_equiv[regno].src_p, use_insn))
	{
	  rtx link;
	  /* Append the REG_DEAD notes from def_insn.  */
	  for (rtx *p = &REG_NOTES (def_insn); (link = *p) != 0; )
	    {
	      if (REG_NOTE_KIND (XEXP (link, 0)) == REG_DEAD)
		{
		  *p = XEXP (link, 1);
		  XEXP (link, 1) = REG_NOTES (use_insn);
		  REG_NOTES (use_insn) = link;
		}
	      else
		p = &XEXP (link, 1);
	    }

	  remove_death (regno, use_insn);
	  SET_REG_N_REFS (regno, 0);
	  REG_FREQ (regno) = 0;
	  df_ref use;
	  FOR_EACH_INSN_USE (use, def_insn)
	    {
	      unsigned int use_regno = DF_REF_REGNO (use);
	      if (!HARD_REGISTER_NUM_P (use_regno))
		reg_equiv[use_regno].replace = 0;
	    }

	  delete_insn (def_insn);

	  reg_equiv[regno].init_insns = NULL;
	  ira_reg_equiv[regno].init_insns = NULL;
	  bitmap_set_bit (cleared_regs, regno);
	}

      /* Move the initialization of the register to just before
	 USE_INSN.  Update the flow information.  */
      else if (prev_nondebug_insn (use_insn) != def_insn)
	{
	  rtx_insn *new_insn;

	  new_insn = emit_insn_before (PATTERN (def_insn), use_insn);
	  REG_NOTES (new_insn) = REG_NOTES (def_insn);
	  REG_NOTES (def_insn) = 0;
	  /* Rescan it to process the notes.  */
	  df_insn_rescan (new_insn);

	  /* Make sure this insn is recognized before reload begins,
	     otherwise eliminate_regs_in_insn will die.  */
	  INSN_CODE (new_insn) = INSN_CODE (def_insn);

	  delete_insn (def_insn);

	  XEXP (reg_equiv[regno].init_insns, 0) = new_insn;

	  REG_BASIC_BLOCK (regno) = use_bb->index;
	  REG_N_CALLS_CROSSED (regno) = 0;

	  if (use_insn == BB_HEAD (use_bb))
	    BB_HEAD (use_bb) = new_insn;

	  /* We know regno dies in use_insn, but inside a loop
	     REG_DEAD notes might be missing when def_insn was in
	     another basic block.  However, when we move def_insn into
	     this bb we'll definitely get a REG_DEAD note and reload
	     will see the death.  It's possible that update_equiv_regs
	     set up an equivalence referencing regno for a reg set by
	     use_insn, when regno was seen as non-local.  Now that
	     regno is local to this block, and dies, such an
	     equivalence is invalid.  */
	  if (find_reg_note (use_insn, REG_EQUIV, regno_reg_rtx[regno]))
	    {
	      rtx set = single_set (use_insn);
	      if (set && REG_P (SET_DEST (set)))
		no_equiv (SET_DEST (set), set, NULL);
	    }

	  ira_reg_equiv[regno].init_insns
	    = gen_rtx_INSN_LIST (VOIDmode, new_insn, NULL_RTX);
	  bitmap_set_bit (cleared_regs, regno);
	}
    }

  if (!bitmap_empty_p (cleared_regs))
    {
      basic_block bb;

      FOR_EACH_BB_FN (bb, cfun)
	{
	  bitmap_and_compl_into (DF_LR_IN (bb), cleared_regs);
	  bitmap_and_compl_into (DF_LR_OUT (bb), cleared_regs);
	  if (!df_live)
	    continue;
	  bitmap_and_compl_into (DF_LIVE_IN (bb), cleared_regs);
	  bitmap_and_compl_into (DF_LIVE_OUT (bb), cleared_regs);
	}

      /* Last pass - adjust debug insns referencing cleared regs.  */
      if (MAY_HAVE_DEBUG_BIND_INSNS)
	for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
	  if (DEBUG_BIND_INSN_P (insn))
	    {
	      rtx old_loc = INSN_VAR_LOCATION_LOC (insn);
	      INSN_VAR_LOCATION_LOC (insn)
		= simplify_replace_fn_rtx (old_loc, NULL_RTX,
					   adjust_cleared_regs,
					   (void *) cleared_regs);
	      if (old_loc != INSN_VAR_LOCATION_LOC (insn))
		df_insn_rescan (insn);
	    }
    }
}

/* A pass over indirect jumps, converting simple cases to direct jumps.
   Combine does this optimization too, but only within a basic block.  */
static void
indirect_jump_optimize (void)
{
  basic_block bb;
  bool rebuild_p = false;

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      rtx_insn *insn = BB_END (bb);
      if (!JUMP_P (insn)
	  || find_reg_note (insn, REG_NON_LOCAL_GOTO, NULL_RTX))
	continue;

      rtx x = pc_set (insn);
      if (!x || !REG_P (SET_SRC (x)))
	continue;

      int regno = REGNO (SET_SRC (x));
      if (DF_REG_DEF_COUNT (regno) == 1)
	{
	  df_ref def = DF_REG_DEF_CHAIN (regno);
	  if (!DF_REF_IS_ARTIFICIAL (def))
	    {
	      rtx_insn *def_insn = DF_REF_INSN (def);
	      rtx lab = NULL_RTX;
	      rtx set = single_set (def_insn);
	      if (set && GET_CODE (SET_SRC (set)) == LABEL_REF)
		lab = SET_SRC (set);
	      else
		{
		  rtx eqnote = find_reg_note (def_insn, REG_EQUAL, NULL_RTX);
		  if (eqnote && GET_CODE (XEXP (eqnote, 0)) == LABEL_REF)
		    lab = XEXP (eqnote, 0);
		}
	      if (lab && validate_replace_rtx (SET_SRC (x), lab, insn))
		rebuild_p = true;
	    }
	}
    }

  if (rebuild_p)
    {
      timevar_push (TV_JUMP);
      rebuild_jump_labels (get_insns ());
      if (purge_all_dead_edges ())
	delete_unreachable_blocks ();
      timevar_pop (TV_JUMP);
    }
}

/* Set up fields memory, constant, and invariant from init_insns in
   the structures of array ira_reg_equiv.  */
static void
setup_reg_equiv (void)
{
  int i;
  rtx_insn_list *elem, *prev_elem, *next_elem;
  rtx_insn *insn;
  rtx set, x;

  for (i = FIRST_PSEUDO_REGISTER; i < ira_reg_equiv_len; i++)
    for (prev_elem = NULL, elem = ira_reg_equiv[i].init_insns;
	 elem;
	 prev_elem = elem, elem = next_elem)
      {
	next_elem = elem->next ();
	insn = elem->insn ();
	set = single_set (insn);
	
	/* Init insns can set up equivalence when the reg is a destination or
	   a source (in this case the destination is memory).  */
	if (set != 0 && (REG_P (SET_DEST (set)) || REG_P (SET_SRC (set))))
	  {
	    if ((x = find_reg_note (insn, REG_EQUIV, NULL_RTX)) != NULL)
	      {
		x = XEXP (x, 0);
		if (REG_P (SET_DEST (set))
		    && REGNO (SET_DEST (set)) == (unsigned int) i
		    && ! rtx_equal_p (SET_SRC (set), x) && MEM_P (x))
		  {
		    /* This insn reporting the equivalence but
		       actually not setting it.  Remove it from the
		       list.  */
		    if (prev_elem == NULL)
		      ira_reg_equiv[i].init_insns = next_elem;
		    else
		      XEXP (prev_elem, 1) = next_elem;
		    elem = prev_elem;
		  }
	      }
	    else if (REG_P (SET_DEST (set))
		     && REGNO (SET_DEST (set)) == (unsigned int) i)
	      x = SET_SRC (set);
	    else
	      {      
		gcc_assert (REG_P (SET_SRC (set))
			    && REGNO (SET_SRC (set)) == (unsigned int) i);
		x = SET_DEST (set);
	      }
	    if (! function_invariant_p (x)
		|| ! flag_pic
		/* A function invariant is often CONSTANT_P but may
		   include a register.  We promise to only pass
		   CONSTANT_P objects to LEGITIMATE_PIC_OPERAND_P.  */
		|| (CONSTANT_P (x) && LEGITIMATE_PIC_OPERAND_P (x)))
	      {
		/* It can happen that a REG_EQUIV note contains a MEM
		   that is not a legitimate memory operand.  As later
		   stages of reload assume that all addresses found in
		   the lra_regno_equiv_* arrays were originally
		   legitimate, we ignore such REG_EQUIV notes.  */
		if (memory_operand (x, VOIDmode))
		  {
		    ira_reg_equiv[i].defined_p = true;
		    ira_reg_equiv[i].memory = x;
		    continue;
		  }
		else if (function_invariant_p (x))
		  {
		    machine_mode mode;
		    
		    mode = GET_MODE (SET_DEST (set));
		    if (GET_CODE (x) == PLUS
			|| x == frame_pointer_rtx || x == arg_pointer_rtx)
		      /* This is PLUS of frame pointer and a constant,
			 or fp, or argp.  */
		      ira_reg_equiv[i].invariant = x;
		    else if (targetm.legitimate_constant_p (mode, x))
		      ira_reg_equiv[i].constant = x;
		    else
		      {
			ira_reg_equiv[i].memory = force_const_mem (mode, x);
			if (ira_reg_equiv[i].memory == NULL_RTX)
			  {
			    ira_reg_equiv[i].defined_p = false;
			    ira_reg_equiv[i].init_insns = NULL;
			    break;
			  }
		      }
		    ira_reg_equiv[i].defined_p = true;
		    continue;
		  }
	      }
	  }
	ira_reg_equiv[i].defined_p = false;
	ira_reg_equiv[i].init_insns = NULL;
	break;
      }
}



/* Print chain C to FILE.  */
static void
print_insn_chain (FILE *file, class insn_chain *c)
{
  fprintf (file, "insn=%d, ", INSN_UID (c->insn));
  bitmap_print (file, &c->live_throughout, "live_throughout: ", ", ");
  bitmap_print (file, &c->dead_or_set, "dead_or_set: ", "\n");
}


/* Print all reload_insn_chains to FILE.  */
static void
print_insn_chains (FILE *file)
{
  class insn_chain *c;
  for (c = reload_insn_chain; c ; c = c->next)
    print_insn_chain (file, c);
}

/* Return true if pseudo REGNO should be added to set live_throughout
   or dead_or_set of the insn chains for reload consideration.  */
static bool
pseudo_for_reload_consideration_p (int regno)
{
  /* Consider spilled pseudos too for IRA because they still have a
     chance to get hard-registers in the reload when IRA is used.  */
  return (reg_renumber[regno] >= 0 || ira_conflicts_p);
}

/* Return true if we can track the individual bytes of subreg X.
   When returning true, set *OUTER_SIZE to the number of bytes in
   X itself, *INNER_SIZE to the number of bytes in the inner register
   and *START to the offset of the first byte.  */
static bool
get_subreg_tracking_sizes (rtx x, HOST_WIDE_INT *outer_size,
			   HOST_WIDE_INT *inner_size, HOST_WIDE_INT *start)
{
  rtx reg = regno_reg_rtx[REGNO (SUBREG_REG (x))];
  return (GET_MODE_SIZE (GET_MODE (x)).is_constant (outer_size)
	  && GET_MODE_SIZE (GET_MODE (reg)).is_constant (inner_size)
	  && SUBREG_BYTE (x).is_constant (start));
}

/* Init LIVE_SUBREGS[ALLOCNUM] and LIVE_SUBREGS_USED[ALLOCNUM] for
   a register with SIZE bytes, making the register live if INIT_VALUE.  */
static void
init_live_subregs (bool init_value, sbitmap *live_subregs,
		   bitmap live_subregs_used, int allocnum, int size)
{
  gcc_assert (size > 0);

  /* Been there, done that.  */
  if (bitmap_bit_p (live_subregs_used, allocnum))
    return;

  /* Create a new one.  */
  if (live_subregs[allocnum] == NULL)
    live_subregs[allocnum] = sbitmap_alloc (size);

  /* If the entire reg was live before blasting into subregs, we need
     to init all of the subregs to ones else init to 0.  */
  if (init_value)
    bitmap_ones (live_subregs[allocnum]);
  else
    bitmap_clear (live_subregs[allocnum]);

  bitmap_set_bit (live_subregs_used, allocnum);
}

/* Walk the insns of the current function and build reload_insn_chain,
   and record register life information.  */
static void
build_insn_chain (void)
{
  unsigned int i;
  class insn_chain **p = &reload_insn_chain;
  basic_block bb;
  class insn_chain *c = NULL;
  class insn_chain *next = NULL;
  auto_bitmap live_relevant_regs;
  auto_bitmap elim_regset;
  /* live_subregs is a vector used to keep accurate information about
     which hardregs are live in multiword pseudos.  live_subregs and
     live_subregs_used are indexed by pseudo number.  The live_subreg
     entry for a particular pseudo is only used if the corresponding
     element is non zero in live_subregs_used.  The sbitmap size of
     live_subreg[allocno] is number of bytes that the pseudo can
     occupy.  */
  sbitmap *live_subregs = XCNEWVEC (sbitmap, max_regno);
  auto_bitmap live_subregs_used;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (eliminable_regset, i))
      bitmap_set_bit (elim_regset, i);
  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      bitmap_iterator bi;
      rtx_insn *insn;

      CLEAR_REG_SET (live_relevant_regs);
      bitmap_clear (live_subregs_used);

      EXECUTE_IF_SET_IN_BITMAP (df_get_live_out (bb), 0, i, bi)
	{
	  if (i >= FIRST_PSEUDO_REGISTER)
	    break;
	  bitmap_set_bit (live_relevant_regs, i);
	}

      EXECUTE_IF_SET_IN_BITMAP (df_get_live_out (bb),
				FIRST_PSEUDO_REGISTER, i, bi)
	{
	  if (pseudo_for_reload_consideration_p (i))
	    bitmap_set_bit (live_relevant_regs, i);
	}

      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  if (!NOTE_P (insn) && !BARRIER_P (insn))
	    {
	      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	      df_ref def, use;

	      c = new_insn_chain ();
	      c->next = next;
	      next = c;
	      *p = c;
	      p = &c->prev;

	      c->insn = insn;
	      c->block = bb->index;

	      if (NONDEBUG_INSN_P (insn))
		FOR_EACH_INSN_INFO_DEF (def, insn_info)
		  {
		    unsigned int regno = DF_REF_REGNO (def);

		    /* Ignore may clobbers because these are generated
		       from calls. However, every other kind of def is
		       added to dead_or_set.  */
		    if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
		      {
			if (regno < FIRST_PSEUDO_REGISTER)
			  {
			    if (!fixed_regs[regno])
			      bitmap_set_bit (&c->dead_or_set, regno);
			  }
			else if (pseudo_for_reload_consideration_p (regno))
			  bitmap_set_bit (&c->dead_or_set, regno);
		      }

		    if ((regno < FIRST_PSEUDO_REGISTER
			 || reg_renumber[regno] >= 0
			 || ira_conflicts_p)
			&& (!DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL)))
		      {
			rtx reg = DF_REF_REG (def);
			HOST_WIDE_INT outer_size, inner_size, start;

			/* We can usually track the liveness of individual
			   bytes within a subreg.  The only exceptions are
			   subregs wrapped in ZERO_EXTRACTs and subregs whose
			   size is not known; in those cases we need to be
			   conservative and treat the definition as a partial
			   definition of the full register rather than a full
			   definition of a specific part of the register.  */
			if (GET_CODE (reg) == SUBREG
			    && !DF_REF_FLAGS_IS_SET (def, DF_REF_ZERO_EXTRACT)
			    && get_subreg_tracking_sizes (reg, &outer_size,
							  &inner_size, &start))
			  {
			    HOST_WIDE_INT last = start + outer_size;

			    init_live_subregs
			      (bitmap_bit_p (live_relevant_regs, regno),
			       live_subregs, live_subregs_used, regno,
			       inner_size);

			    if (!DF_REF_FLAGS_IS_SET
				(def, DF_REF_STRICT_LOW_PART))
			      {
				/* Expand the range to cover entire words.
				   Bytes added here are "don't care".  */
				start
				  = start / UNITS_PER_WORD * UNITS_PER_WORD;
				last = ((last + UNITS_PER_WORD - 1)
					/ UNITS_PER_WORD * UNITS_PER_WORD);
			      }

			    /* Ignore the paradoxical bits.  */
			    if (last > SBITMAP_SIZE (live_subregs[regno]))
			      last = SBITMAP_SIZE (live_subregs[regno]);

			    while (start < last)
			      {
				bitmap_clear_bit (live_subregs[regno], start);
				start++;
			      }

			    if (bitmap_empty_p (live_subregs[regno]))
			      {
				bitmap_clear_bit (live_subregs_used, regno);
				bitmap_clear_bit (live_relevant_regs, regno);
			      }
			    else
			      /* Set live_relevant_regs here because
				 that bit has to be true to get us to
				 look at the live_subregs fields.  */
			      bitmap_set_bit (live_relevant_regs, regno);
			  }
			else
			  {
			    /* DF_REF_PARTIAL is generated for
			       subregs, STRICT_LOW_PART, and
			       ZERO_EXTRACT.  We handle the subreg
			       case above so here we have to keep from
			       modeling the def as a killing def.  */
			    if (!DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL))
			      {
				bitmap_clear_bit (live_subregs_used, regno);
				bitmap_clear_bit (live_relevant_regs, regno);
			      }
			  }
		      }
		  }

	      bitmap_and_compl_into (live_relevant_regs, elim_regset);
	      bitmap_copy (&c->live_throughout, live_relevant_regs);

	      if (NONDEBUG_INSN_P (insn))
		FOR_EACH_INSN_INFO_USE (use, insn_info)
		  {
		    unsigned int regno = DF_REF_REGNO (use);
		    rtx reg = DF_REF_REG (use);

		    /* DF_REF_READ_WRITE on a use means that this use
		       is fabricated from a def that is a partial set
		       to a multiword reg.  Here, we only model the
		       subreg case that is not wrapped in ZERO_EXTRACT
		       precisely so we do not need to look at the
		       fabricated use.  */
		    if (DF_REF_FLAGS_IS_SET (use, DF_REF_READ_WRITE)
			&& !DF_REF_FLAGS_IS_SET (use, DF_REF_ZERO_EXTRACT)
			&& DF_REF_FLAGS_IS_SET (use, DF_REF_SUBREG))
		      continue;

		    /* Add the last use of each var to dead_or_set.  */
		    if (!bitmap_bit_p (live_relevant_regs, regno))
		      {
			if (regno < FIRST_PSEUDO_REGISTER)
			  {
			    if (!fixed_regs[regno])
			      bitmap_set_bit (&c->dead_or_set, regno);
			  }
			else if (pseudo_for_reload_consideration_p (regno))
			  bitmap_set_bit (&c->dead_or_set, regno);
		      }

		    if (regno < FIRST_PSEUDO_REGISTER
			|| pseudo_for_reload_consideration_p (regno))
		      {
			HOST_WIDE_INT outer_size, inner_size, start;
			if (GET_CODE (reg) == SUBREG
			    && !DF_REF_FLAGS_IS_SET (use,
						     DF_REF_SIGN_EXTRACT
						     | DF_REF_ZERO_EXTRACT)
			    && get_subreg_tracking_sizes (reg, &outer_size,
							  &inner_size, &start))
			  {
			    HOST_WIDE_INT last = start + outer_size;

			    init_live_subregs
			      (bitmap_bit_p (live_relevant_regs, regno),
			       live_subregs, live_subregs_used, regno,
			       inner_size);

			    /* Ignore the paradoxical bits.  */
			    if (last > SBITMAP_SIZE (live_subregs[regno]))
			      last = SBITMAP_SIZE (live_subregs[regno]);

			    while (start < last)
			      {
				bitmap_set_bit (live_subregs[regno], start);
				start++;
			      }
			  }
			else
			  /* Resetting the live_subregs_used is
			     effectively saying do not use the subregs
			     because we are reading the whole
			     pseudo.  */
			  bitmap_clear_bit (live_subregs_used, regno);
			bitmap_set_bit (live_relevant_regs, regno);
		      }
		  }
	    }
	}

      /* FIXME!! The following code is a disaster.  Reload needs to see the
	 labels and jump tables that are just hanging out in between
	 the basic blocks.  See pr33676.  */
      insn = BB_HEAD (bb);

      /* Skip over the barriers and cruft.  */
      while (insn && (BARRIER_P (insn) || NOTE_P (insn)
		      || BLOCK_FOR_INSN (insn) == bb))
	insn = PREV_INSN (insn);

      /* While we add anything except barriers and notes, the focus is
	 to get the labels and jump tables into the
	 reload_insn_chain.  */
      while (insn)
	{
	  if (!NOTE_P (insn) && !BARRIER_P (insn))
	    {
	      if (BLOCK_FOR_INSN (insn))
		break;

	      c = new_insn_chain ();
	      c->next = next;
	      next = c;
	      *p = c;
	      p = &c->prev;

	      /* The block makes no sense here, but it is what the old
		 code did.  */
	      c->block = bb->index;
	      c->insn = insn;
	      bitmap_copy (&c->live_throughout, live_relevant_regs);
	    }
	  insn = PREV_INSN (insn);
	}
    }

  reload_insn_chain = c;
  *p = NULL;

  for (i = 0; i < (unsigned int) max_regno; i++)
    if (live_subregs[i] != NULL)
      sbitmap_free (live_subregs[i]);
  free (live_subregs);

  if (dump_file)
    print_insn_chains (dump_file);
}
 
/* Examine the rtx found in *LOC, which is read or written to as determined
   by TYPE.  Return false if we find a reason why an insn containing this
   rtx should not be moved (such as accesses to non-constant memory), true
   otherwise.  */
static bool
rtx_moveable_p (rtx *loc, enum op_type type)
{
  const char *fmt;
  rtx x = *loc;
  int i, j;

  enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
      return true;

    case PC:
      return type == OP_IN;

    case CC0:
      return false;

    case REG:
      if (x == frame_pointer_rtx)
	return true;
      if (HARD_REGISTER_P (x))
	return false;
      
      return true;

    case MEM:
      if (type == OP_IN && MEM_READONLY_P (x))
	return rtx_moveable_p (&XEXP (x, 0), OP_IN);
      return false;

    case SET:
      return (rtx_moveable_p (&SET_SRC (x), OP_IN)
	      && rtx_moveable_p (&SET_DEST (x), OP_OUT));

    case STRICT_LOW_PART:
      return rtx_moveable_p (&XEXP (x, 0), OP_OUT);

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      return (rtx_moveable_p (&XEXP (x, 0), type)
	      && rtx_moveable_p (&XEXP (x, 1), OP_IN)
	      && rtx_moveable_p (&XEXP (x, 2), OP_IN));

    case CLOBBER:
      return rtx_moveable_p (&SET_DEST (x), OP_OUT);

    case UNSPEC_VOLATILE:
      /* It is a bad idea to consider insns with such rtl
	 as moveable ones.  The insn scheduler also considers them as barrier
	 for a reason.  */
      return false;

    case ASM_OPERANDS:
      /* The same is true for volatile asm: it has unknown side effects, it
         cannot be moved at will.  */
      if (MEM_VOLATILE_P (x))
	return false;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (!rtx_moveable_p (&XEXP (x, i), type))
	    return false;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  {
	    if (!rtx_moveable_p (&XVECEXP (x, i, j), type))
	      return false;
	  }
    }
  return true;
}

/* A wrapper around dominated_by_p, which uses the information in UID_LUID
   to give dominance relationships between two insns I1 and I2.  */
static bool
insn_dominated_by_p (rtx i1, rtx i2, int *uid_luid)
{
  basic_block bb1 = BLOCK_FOR_INSN (i1);
  basic_block bb2 = BLOCK_FOR_INSN (i2);

  if (bb1 == bb2)
    return uid_luid[INSN_UID (i2)] < uid_luid[INSN_UID (i1)];
  return dominated_by_p (CDI_DOMINATORS, bb1, bb2);
}

/* Record the range of register numbers added by find_moveable_pseudos.  */
int first_moveable_pseudo, last_moveable_pseudo;

/* These two vectors hold data for every register added by
   find_movable_pseudos, with index 0 holding data for the
   first_moveable_pseudo.  */
/* The original home register.  */
static vec<rtx> pseudo_replaced_reg;

/* Look for instances where we have an instruction that is known to increase
   register pressure, and whose result is not used immediately.  If it is
   possible to move the instruction downwards to just before its first use,
   split its lifetime into two ranges.  We create a new pseudo to compute the
   value, and emit a move instruction just before the first use.  If, after
   register allocation, the new pseudo remains unallocated, the function
   move_unallocated_pseudos then deletes the move instruction and places
   the computation just before the first use.

   Such a move is safe and profitable if all the input registers remain live
   and unchanged between the original computation and its first use.  In such
   a situation, the computation is known to increase register pressure, and
   moving it is known to at least not worsen it.

   We restrict moves to only those cases where a register remains unallocated,
   in order to avoid interfering too much with the instruction schedule.  As
   an exception, we may move insns which only modify their input register
   (typically induction variables), as this increases the freedom for our
   intended transformation, and does not limit the second instruction
   scheduler pass.  */
   
static void
find_moveable_pseudos (void)
{
  unsigned i;
  int max_regs = max_reg_num ();
  int max_uid = get_max_uid ();
  basic_block bb;
  int *uid_luid = XNEWVEC (int, max_uid);
  rtx_insn **closest_uses = XNEWVEC (rtx_insn *, max_regs);
  /* A set of registers which are live but not modified throughout a block.  */
  bitmap_head *bb_transp_live = XNEWVEC (bitmap_head,
					 last_basic_block_for_fn (cfun));
  /* A set of registers which only exist in a given basic block.  */
  bitmap_head *bb_local = XNEWVEC (bitmap_head,
				   last_basic_block_for_fn (cfun));
  /* A set of registers which are set once, in an instruction that can be
     moved freely downwards, but are otherwise transparent to a block.  */
  bitmap_head *bb_moveable_reg_sets = XNEWVEC (bitmap_head,
					       last_basic_block_for_fn (cfun));
  auto_bitmap live, used, set, interesting, unusable_as_input;
  bitmap_iterator bi;

  first_moveable_pseudo = max_regs;
  pseudo_replaced_reg.release ();
  pseudo_replaced_reg.safe_grow_cleared (max_regs);

  df_analyze ();
  calculate_dominance_info (CDI_DOMINATORS);

  i = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;
      bitmap transp = bb_transp_live + bb->index;
      bitmap moveable = bb_moveable_reg_sets + bb->index;
      bitmap local = bb_local + bb->index;

      bitmap_initialize (local, 0);
      bitmap_initialize (transp, 0);
      bitmap_initialize (moveable, 0);
      bitmap_copy (live, df_get_live_out (bb));
      bitmap_and_into (live, df_get_live_in (bb));
      bitmap_copy (transp, live);
      bitmap_clear (moveable);
      bitmap_clear (live);
      bitmap_clear (used);
      bitmap_clear (set);
      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  {
	    df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	    df_ref def, use;

	    uid_luid[INSN_UID (insn)] = i++;
	    
	    def = df_single_def (insn_info);
	    use = df_single_use (insn_info);
	    if (use
		&& def
		&& DF_REF_REGNO (use) == DF_REF_REGNO (def)
		&& !bitmap_bit_p (set, DF_REF_REGNO (use))
		&& rtx_moveable_p (&PATTERN (insn), OP_IN))
	      {
		unsigned regno = DF_REF_REGNO (use);
		bitmap_set_bit (moveable, regno);
		bitmap_set_bit (set, regno);
		bitmap_set_bit (used, regno);
		bitmap_clear_bit (transp, regno);
		continue;
	      }
	    FOR_EACH_INSN_INFO_USE (use, insn_info)
	      {
		unsigned regno = DF_REF_REGNO (use);
		bitmap_set_bit (used, regno);
		if (bitmap_clear_bit (moveable, regno))
		  bitmap_clear_bit (transp, regno);
	      }

	    FOR_EACH_INSN_INFO_DEF (def, insn_info)
	      {
		unsigned regno = DF_REF_REGNO (def);
		bitmap_set_bit (set, regno);
		bitmap_clear_bit (transp, regno);
		bitmap_clear_bit (moveable, regno);
	      }
	  }
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap local = bb_local + bb->index;
      rtx_insn *insn;

      FOR_BB_INSNS (bb, insn)
	if (NONDEBUG_INSN_P (insn))
	  {
	    df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	    rtx_insn *def_insn;
	    rtx closest_use, note;
	    df_ref def, use;
	    unsigned regno;
	    bool all_dominated, all_local;
	    machine_mode mode;

	    def = df_single_def (insn_info);
	    /* There must be exactly one def in this insn.  */
	    if (!def || !single_set (insn))
	      continue;
	    /* This must be the only definition of the reg.  We also limit
	       which modes we deal with so that we can assume we can generate
	       move instructions.  */
	    regno = DF_REF_REGNO (def);
	    mode = GET_MODE (DF_REF_REG (def));
	    if (DF_REG_DEF_COUNT (regno) != 1
		|| !DF_REF_INSN_INFO (def)
		|| HARD_REGISTER_NUM_P (regno)
		|| DF_REG_EQ_USE_COUNT (regno) > 0
		|| (!INTEGRAL_MODE_P (mode) && !FLOAT_MODE_P (mode)))
	      continue;
	    def_insn = DF_REF_INSN (def);

	    for (note = REG_NOTES (def_insn); note; note = XEXP (note, 1))
	      if (REG_NOTE_KIND (note) == REG_EQUIV && MEM_P (XEXP (note, 0)))
		break;
		
	    if (note)
	      {
		if (dump_file)
		  fprintf (dump_file, "Ignoring reg %d, has equiv memory\n",
			   regno);
		bitmap_set_bit (unusable_as_input, regno);
		continue;
	      }

	    use = DF_REG_USE_CHAIN (regno);
	    all_dominated = true;
	    all_local = true;
	    closest_use = NULL_RTX;
	    for (; use; use = DF_REF_NEXT_REG (use))
	      {
		rtx_insn *insn;
		if (!DF_REF_INSN_INFO (use))
		  {
		    all_dominated = false;
		    all_local = false;
		    break;
		  }
		insn = DF_REF_INSN (use);
		if (DEBUG_INSN_P (insn))
		  continue;
		if (BLOCK_FOR_INSN (insn) != BLOCK_FOR_INSN (def_insn))
		  all_local = false;
		if (!insn_dominated_by_p (insn, def_insn, uid_luid))
		  all_dominated = false;
		if (closest_use != insn && closest_use != const0_rtx)
		  {
		    if (closest_use == NULL_RTX)
		      closest_use = insn;
		    else if (insn_dominated_by_p (closest_use, insn, uid_luid))
		      closest_use = insn;
		    else if (!insn_dominated_by_p (insn, closest_use, uid_luid))
		      closest_use = const0_rtx;
		  }
	      }
	    if (!all_dominated)
	      {
		if (dump_file)
		  fprintf (dump_file, "Reg %d not all uses dominated by set\n",
			   regno);
		continue;
	      }
	    if (all_local)
	      bitmap_set_bit (local, regno);
	    if (closest_use == const0_rtx || closest_use == NULL
		|| next_nonnote_nondebug_insn (def_insn) == closest_use)
	      {
		if (dump_file)
		  fprintf (dump_file, "Reg %d uninteresting%s\n", regno,
			   closest_use == const0_rtx || closest_use == NULL
			   ? " (no unique first use)" : "");
		continue;
	      }
	    if (HAVE_cc0 && reg_referenced_p (cc0_rtx, PATTERN (closest_use)))
	      {
		if (dump_file)
		  fprintf (dump_file, "Reg %d: closest user uses cc0\n",
			   regno);
		continue;
	      }

	    bitmap_set_bit (interesting, regno);
	    /* If we get here, we know closest_use is a non-NULL insn
	       (as opposed to const_0_rtx).  */
	    closest_uses[regno] = as_a <rtx_insn *> (closest_use);

	    if (dump_file && (all_local || all_dominated))
	      {
		fprintf (dump_file, "Reg %u:", regno);
		if (all_local)
		  fprintf (dump_file, " local to bb %d", bb->index);
		if (all_dominated)
		  fprintf (dump_file, " def dominates all uses");
		if (closest_use != const0_rtx)
		  fprintf (dump_file, " has unique first use");
		fputs ("\n", dump_file);
	      }
	  }
    }

  EXECUTE_IF_SET_IN_BITMAP (interesting, 0, i, bi)
    {
      df_ref def = DF_REG_DEF_CHAIN (i);
      rtx_insn *def_insn = DF_REF_INSN (def);
      basic_block def_block = BLOCK_FOR_INSN (def_insn);
      bitmap def_bb_local = bb_local + def_block->index;
      bitmap def_bb_moveable = bb_moveable_reg_sets + def_block->index;
      bitmap def_bb_transp = bb_transp_live + def_block->index;
      bool local_to_bb_p = bitmap_bit_p (def_bb_local, i);
      rtx_insn *use_insn = closest_uses[i];
      df_ref use;
      bool all_ok = true;
      bool all_transp = true;

      if (!REG_P (DF_REF_REG (def)))
	continue;

      if (!local_to_bb_p)
	{
	  if (dump_file)
	    fprintf (dump_file, "Reg %u not local to one basic block\n",
		     i);
	  continue;
	}
      if (reg_equiv_init (i) != NULL_RTX)
	{
	  if (dump_file)
	    fprintf (dump_file, "Ignoring reg %u with equiv init insn\n",
		     i);
	  continue;
	}
      if (!rtx_moveable_p (&PATTERN (def_insn), OP_IN))
	{
	  if (dump_file)
	    fprintf (dump_file, "Found def insn %d for %d to be not moveable\n",
		     INSN_UID (def_insn), i);
	  continue;
	}
      if (dump_file)
	fprintf (dump_file, "Examining insn %d, def for %d\n",
		 INSN_UID (def_insn), i);
      FOR_EACH_INSN_USE (use, def_insn)
	{
	  unsigned regno = DF_REF_REGNO (use);
	  if (bitmap_bit_p (unusable_as_input, regno))
	    {
	      all_ok = false;
	      if (dump_file)
		fprintf (dump_file, "  found unusable input reg %u.\n", regno);
	      break;
	    }
	  if (!bitmap_bit_p (def_bb_transp, regno))
	    {
	      if (bitmap_bit_p (def_bb_moveable, regno)
		  && !control_flow_insn_p (use_insn)
		  && (!HAVE_cc0 || !sets_cc0_p (use_insn)))
		{
		  if (modified_between_p (DF_REF_REG (use), def_insn, use_insn))
		    {
		      rtx_insn *x = NEXT_INSN (def_insn);
		      while (!modified_in_p (DF_REF_REG (use), x))
			{
			  gcc_assert (x != use_insn);
			  x = NEXT_INSN (x);
			}
		      if (dump_file)
			fprintf (dump_file, "  input reg %u modified but insn %d moveable\n",
				 regno, INSN_UID (x));
		      emit_insn_after (PATTERN (x), use_insn);
		      set_insn_deleted (x);
		    }
		  else
		    {
		      if (dump_file)
			fprintf (dump_file, "  input reg %u modified between def and use\n",
				 regno);
		      all_transp = false;
		    }
		}
	      else
		all_transp = false;
	    }
	}
      if (!all_ok)
	continue;
      if (!dbg_cnt (ira_move))
	break;
      if (dump_file)
	fprintf (dump_file, "  all ok%s\n", all_transp ? " and transp" : "");

      if (all_transp)
	{
	  rtx def_reg = DF_REF_REG (def);
	  rtx newreg = ira_create_new_reg (def_reg);
	  if (validate_change (def_insn, DF_REF_REAL_LOC (def), newreg, 0))
	    {
	      unsigned nregno = REGNO (newreg);
	      emit_insn_before (gen_move_insn (def_reg, newreg), use_insn);
	      nregno -= max_regs;
	      pseudo_replaced_reg[nregno] = def_reg;
	    }
	}
    }
  
  FOR_EACH_BB_FN (bb, cfun)
    {
      bitmap_clear (bb_local + bb->index);
      bitmap_clear (bb_transp_live + bb->index);
      bitmap_clear (bb_moveable_reg_sets + bb->index);
    }
  free (uid_luid);
  free (closest_uses);
  free (bb_local);
  free (bb_transp_live);
  free (bb_moveable_reg_sets);

  last_moveable_pseudo = max_reg_num ();

  fix_reg_equiv_init ();
  expand_reg_info ();
  regstat_free_n_sets_and_refs ();
  regstat_free_ri ();
  regstat_init_n_sets_and_refs ();
  regstat_compute_ri ();
  free_dominance_info (CDI_DOMINATORS);
}

/* If SET pattern SET is an assignment from a hard register to a pseudo which
   is live at CALL_DOM (if non-NULL, otherwise this check is omitted), return
   the destination.  Otherwise return NULL.  */

static rtx
interesting_dest_for_shprep_1 (rtx set, basic_block call_dom)
{
  rtx src = SET_SRC (set);
  rtx dest = SET_DEST (set);
  if (!REG_P (src) || !HARD_REGISTER_P (src)
      || !REG_P (dest) || HARD_REGISTER_P (dest)
      || (call_dom && !bitmap_bit_p (df_get_live_in (call_dom), REGNO (dest))))
    return NULL;
  return dest;
}

/* If insn is interesting for parameter range-splitting shrink-wrapping
   preparation, i.e. it is a single set from a hard register to a pseudo, which
   is live at CALL_DOM (if non-NULL, otherwise this check is omitted), or a
   parallel statement with only one such statement, return the destination.
   Otherwise return NULL.  */

static rtx
interesting_dest_for_shprep (rtx_insn *insn, basic_block call_dom)
{
  if (!INSN_P (insn))
    return NULL;
  rtx pat = PATTERN (insn);
  if (GET_CODE (pat) == SET)
    return interesting_dest_for_shprep_1 (pat, call_dom);

  if (GET_CODE (pat) != PARALLEL)
    return NULL;
  rtx ret = NULL;
  for (int i = 0; i < XVECLEN (pat, 0); i++)
    {
      rtx sub = XVECEXP (pat, 0, i);
      if (GET_CODE (sub) == USE || GET_CODE (sub) == CLOBBER)
	continue;
      if (GET_CODE (sub) != SET
	  || side_effects_p (sub))
	return NULL;
      rtx dest = interesting_dest_for_shprep_1 (sub, call_dom);
      if (dest && ret)
	return NULL;
      if (dest)
	ret = dest;
    }
  return ret;
}

/* Split live ranges of pseudos that are loaded from hard registers in the
   first BB in a BB that dominates all non-sibling call if such a BB can be
   found and is not in a loop.  Return true if the function has made any
   changes.  */

static bool
split_live_ranges_for_shrink_wrap (void)
{
  basic_block bb, call_dom = NULL;
  basic_block first = single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  rtx_insn *insn, *last_interesting_insn = NULL;
  auto_bitmap need_new, reachable;
  vec<basic_block> queue;

  if (!SHRINK_WRAPPING_ENABLED)
    return false;

  queue.create (n_basic_blocks_for_fn (cfun));

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      if (CALL_P (insn) && !SIBLING_CALL_P (insn))
	{
	  if (bb == first)
	    {
	      queue.release ();
	      return false;
	    }

	  bitmap_set_bit (need_new, bb->index);
	  bitmap_set_bit (reachable, bb->index);
	  queue.quick_push (bb);
	  break;
	}

  if (queue.is_empty ())
    {
      queue.release ();
      return false;
    }

  while (!queue.is_empty ())
    {
      edge e;
      edge_iterator ei;

      bb = queue.pop ();
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && bitmap_set_bit (reachable, e->dest->index))
	  queue.quick_push (e->dest);
    }
  queue.release ();

  FOR_BB_INSNS (first, insn)
    {
      rtx dest = interesting_dest_for_shprep (insn, NULL);
      if (!dest)
	continue;

      if (DF_REG_DEF_COUNT (REGNO (dest)) > 1)
	return false;

      for (df_ref use = DF_REG_USE_CHAIN (REGNO(dest));
	   use;
	   use = DF_REF_NEXT_REG (use))
	{
	  int ubbi = DF_REF_BB (use)->index;
	  if (bitmap_bit_p (reachable, ubbi))
	    bitmap_set_bit (need_new, ubbi);
	}
      last_interesting_insn = insn;
    }

  if (!last_interesting_insn)
    return false;

  call_dom = nearest_common_dominator_for_set (CDI_DOMINATORS, need_new);
  if (call_dom == first)
    return false;

  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  while (bb_loop_depth (call_dom) > 0)
    call_dom = get_immediate_dominator (CDI_DOMINATORS, call_dom);
  loop_optimizer_finalize ();

  if (call_dom == first)
    return false;

  calculate_dominance_info (CDI_POST_DOMINATORS);
  if (dominated_by_p (CDI_POST_DOMINATORS, first, call_dom))
    {
      free_dominance_info (CDI_POST_DOMINATORS);
      return false;
    }
  free_dominance_info (CDI_POST_DOMINATORS);

  if (dump_file)
    fprintf (dump_file, "Will split live ranges of parameters at BB %i\n",
	     call_dom->index);

  bool ret = false;
  FOR_BB_INSNS (first, insn)
    {
      rtx dest = interesting_dest_for_shprep (insn, call_dom);
      if (!dest || dest == pic_offset_table_rtx)
	continue;

      bool need_newreg = false;
      df_ref use, next;
      for (use = DF_REG_USE_CHAIN (REGNO (dest)); use; use = next)
	{
	  rtx_insn *uin = DF_REF_INSN (use);
	  next = DF_REF_NEXT_REG (use);

	  if (DEBUG_INSN_P (uin))
	    continue;

	  basic_block ubb = BLOCK_FOR_INSN (uin);
	  if (ubb == call_dom
	      || dominated_by_p (CDI_DOMINATORS, ubb, call_dom))
	    {
	      need_newreg = true;
	      break;
	    }
	}

      if (need_newreg)
	{
	  rtx newreg = ira_create_new_reg (dest);

	  for (use = DF_REG_USE_CHAIN (REGNO (dest)); use; use = next)
	    {
	      rtx_insn *uin = DF_REF_INSN (use);
	      next = DF_REF_NEXT_REG (use);

	      basic_block ubb = BLOCK_FOR_INSN (uin);
	      if (ubb == call_dom
		  || dominated_by_p (CDI_DOMINATORS, ubb, call_dom))
		validate_change (uin, DF_REF_REAL_LOC (use), newreg, true);
	    }

	  rtx_insn *new_move = gen_move_insn (newreg, dest);
	  emit_insn_after (new_move, bb_note (call_dom));
	  if (dump_file)
	    {
	      fprintf (dump_file, "Split live-range of register ");
	      print_rtl_single (dump_file, dest);
	    }
	  ret = true;
	}

      if (insn == last_interesting_insn)
	break;
    }
  apply_change_group ();
  return ret;
}

/* Perform the second half of the transformation started in
   find_moveable_pseudos.  We look for instances where the newly introduced
   pseudo remains unallocated, and remove it by moving the definition to
   just before its use, replacing the move instruction generated by
   find_moveable_pseudos.  */
static void
move_unallocated_pseudos (void)
{
  int i;
  for (i = first_moveable_pseudo; i < last_moveable_pseudo; i++)
    if (reg_renumber[i] < 0)
      {
	int idx = i - first_moveable_pseudo;
	rtx other_reg = pseudo_replaced_reg[idx];
	rtx_insn *def_insn = DF_REF_INSN (DF_REG_DEF_CHAIN (i));
	/* The use must follow all definitions of OTHER_REG, so we can
	   insert the new definition immediately after any of them.  */
	df_ref other_def = DF_REG_DEF_CHAIN (REGNO (other_reg));
	rtx_insn *move_insn = DF_REF_INSN (other_def);
	rtx_insn *newinsn = emit_insn_after (PATTERN (def_insn), move_insn);
	rtx set;
	int success;

	if (dump_file)
	  fprintf (dump_file, "moving def of %d (insn %d now) ",
		   REGNO (other_reg), INSN_UID (def_insn));

	delete_insn (move_insn);
	while ((other_def = DF_REG_DEF_CHAIN (REGNO (other_reg))))
	  delete_insn (DF_REF_INSN (other_def));
	delete_insn (def_insn);

	set = single_set (newinsn);
	success = validate_change (newinsn, &SET_DEST (set), other_reg, 0);
	gcc_assert (success);
	if (dump_file)
	  fprintf (dump_file, " %d) rather than keep unallocated replacement %d\n",
		   INSN_UID (newinsn), i);
	SET_REG_N_REFS (i, 0);
      }
}

/* If the backend knows where to allocate pseudos for hard
   register initial values, register these allocations now.  */
static void
allocate_initial_values (void)
{
  if (targetm.allocate_initial_value)
    {
      rtx hreg, preg, x;
      int i, regno;

      for (i = 0; HARD_REGISTER_NUM_P (i); i++)
	{
	  if (! initial_value_entry (i, &hreg, &preg))
	    break;

	  x = targetm.allocate_initial_value (hreg);
	  regno = REGNO (preg);
	  if (x && REG_N_SETS (regno) <= 1)
	    {
	      if (MEM_P (x))
		reg_equiv_memory_loc (regno) = x;
	      else
		{
		  basic_block bb;
		  int new_regno;

		  gcc_assert (REG_P (x));
		  new_regno = REGNO (x);
		  reg_renumber[regno] = new_regno;
		  /* Poke the regno right into regno_reg_rtx so that even
		     fixed regs are accepted.  */
		  SET_REGNO (preg, new_regno);
		  /* Update global register liveness information.  */
		  FOR_EACH_BB_FN (bb, cfun)
		    {
		      if (REGNO_REG_SET_P (df_get_live_in (bb), regno))
			SET_REGNO_REG_SET (df_get_live_in (bb), new_regno);
		      if (REGNO_REG_SET_P (df_get_live_out (bb), regno))
			SET_REGNO_REG_SET (df_get_live_out (bb), new_regno);
		    }
		}
	    }
	}

      gcc_checking_assert (! initial_value_entry (FIRST_PSEUDO_REGISTER,
						  &hreg, &preg));
    }
}


/* True when we use LRA instead of reload pass for the current
   function.  */
bool ira_use_lra_p;

/* True if we have allocno conflicts.  It is false for non-optimized
   mode or when the conflict table is too big.  */
bool ira_conflicts_p;

/* Saved between IRA and reload.  */
static int saved_flag_ira_share_spill_slots;

/* This is the main entry of IRA.  */
static void
ira (FILE *f)
{
  bool loops_p;
  int ira_max_point_before_emit;
  bool saved_flag_caller_saves = flag_caller_saves;
  enum ira_region saved_flag_ira_region = flag_ira_region;
  unsigned int i;
  int num_used_regs = 0;

  clear_bb_flags ();

  /* Determine if the current function is a leaf before running IRA
     since this can impact optimizations done by the prologue and
     epilogue thus changing register elimination offsets.
     Other target callbacks may use crtl->is_leaf too, including
     SHRINK_WRAPPING_ENABLED, so initialize as early as possible.  */
  crtl->is_leaf = leaf_function_p ();

  /* Perform target specific PIC register initialization.  */
  targetm.init_pic_reg ();

  ira_conflicts_p = optimize > 0;

  /* Determine the number of pseudos actually requiring coloring.  */
  for (i = FIRST_PSEUDO_REGISTER; i < DF_REG_SIZE (df); i++)
    num_used_regs += !!(DF_REG_USE_COUNT (i) + DF_REG_DEF_COUNT (i));

  /* If there are too many pseudos and/or basic blocks (e.g. 10K
     pseudos and 10K blocks or 100K pseudos and 1K blocks), we will
     use simplified and faster algorithms in LRA.  */
  lra_simple_p
    = (ira_use_lra_p
       && num_used_regs >= (1 << 26) / last_basic_block_for_fn (cfun));

  if (lra_simple_p)
    {
      /* It permits to skip live range splitting in LRA.  */
      flag_caller_saves = false;
      /* There is no sense to do regional allocation when we use
	 simplified LRA.  */
      flag_ira_region = IRA_REGION_ONE;
      ira_conflicts_p = false;
    }

#ifndef IRA_NO_OBSTACK
  gcc_obstack_init (&ira_obstack);
#endif
  bitmap_obstack_initialize (&ira_bitmap_obstack);

  /* LRA uses its own infrastructure to handle caller save registers.  */
  if (flag_caller_saves && !ira_use_lra_p)
    init_caller_save ();

  if (flag_ira_verbose < 10)
    {
      internal_flag_ira_verbose = flag_ira_verbose;
      ira_dump_file = f;
    }
  else
    {
      internal_flag_ira_verbose = flag_ira_verbose - 10;
      ira_dump_file = stderr;
    }

  setup_prohibited_mode_move_regs ();
  decrease_live_ranges_number ();
  df_note_add_problem ();

  /* DF_LIVE can't be used in the register allocator, too many other
     parts of the compiler depend on using the "classic" liveness
     interpretation of the DF_LR problem.  See PR38711.
     Remove the problem, so that we don't spend time updating it in
     any of the df_analyze() calls during IRA/LRA.  */
  if (optimize > 1)
    df_remove_problem (df_live);
  gcc_checking_assert (df_live == NULL);

  if (flag_checking)
    df->changeable_flags |= DF_VERIFY_SCHEDULED;

  df_analyze ();

  init_reg_equiv ();
  if (ira_conflicts_p)
    {
      calculate_dominance_info (CDI_DOMINATORS);

      if (split_live_ranges_for_shrink_wrap ())
	df_analyze ();

      free_dominance_info (CDI_DOMINATORS);
    }

  df_clear_flags (DF_NO_INSN_RESCAN);

  indirect_jump_optimize ();
  if (delete_trivially_dead_insns (get_insns (), max_reg_num ()))
    df_analyze ();

  regstat_init_n_sets_and_refs ();
  regstat_compute_ri ();

  /* If we are not optimizing, then this is the only place before
     register allocation where dataflow is done.  And that is needed
     to generate these warnings.  */
  if (warn_clobbered)
    generate_setjmp_warnings ();

  if (resize_reg_info () && flag_ira_loop_pressure)
    ira_set_pseudo_classes (true, ira_dump_file);

  init_alias_analysis ();
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  reg_equiv = XCNEWVEC (struct equivalence, max_reg_num ());
  update_equiv_regs_prescan ();
  update_equiv_regs ();

  /* Don't move insns if live range shrinkage or register
     pressure-sensitive scheduling were done because it will not
     improve allocation but likely worsen insn scheduling.  */
  if (optimize
      && !flag_live_range_shrinkage
      && !(flag_sched_pressure && flag_schedule_insns))
    combine_and_move_insns ();

  /* Gather additional equivalences with memory.  */
  if (optimize)
    add_store_equivs ();

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  end_alias_analysis ();
  free (reg_equiv);

  setup_reg_equiv ();
  grow_reg_equivs ();
  setup_reg_equiv_init ();

  allocated_reg_info_size = max_reg_num ();

  /* It is not worth to do such improvement when we use a simple
     allocation because of -O0 usage or because the function is too
     big.  */
  if (ira_conflicts_p)
    find_moveable_pseudos ();

  max_regno_before_ira = max_reg_num ();
  ira_setup_eliminable_regset ();

  ira_overall_cost = ira_reg_cost = ira_mem_cost = 0;
  ira_load_cost = ira_store_cost = ira_shuffle_cost = 0;
  ira_move_loops_num = ira_additional_jumps_num = 0;

  ira_assert (current_loops == NULL);
  if (flag_ira_region == IRA_REGION_ALL || flag_ira_region == IRA_REGION_MIXED)
    loop_optimizer_init (AVOID_CFG_MODIFICATIONS | LOOPS_HAVE_RECORDED_EXITS);

  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
    fprintf (ira_dump_file, "Building IRA IR\n");
  loops_p = ira_build ();

  ira_assert (ira_conflicts_p || !loops_p);

  saved_flag_ira_share_spill_slots = flag_ira_share_spill_slots;
  if (too_high_register_pressure_p () || cfun->calls_setjmp)
    /* It is just wasting compiler's time to pack spilled pseudos into
       stack slots in this case -- prohibit it.  We also do this if
       there is setjmp call because a variable not modified between
       setjmp and longjmp the compiler is required to preserve its
       value and sharing slots does not guarantee it.  */
    flag_ira_share_spill_slots = FALSE;

  ira_color ();

  ira_max_point_before_emit = ira_max_point;

  ira_initiate_emit_data ();

  ira_emit (loops_p);

  max_regno = max_reg_num ();
  if (ira_conflicts_p)
    {
      if (! loops_p)
	{
	  if (! ira_use_lra_p)
	    ira_initiate_assign ();
	}
      else
	{
	  expand_reg_info ();

	  if (ira_use_lra_p)
	    {
	      ira_allocno_t a;
	      ira_allocno_iterator ai;

	      FOR_EACH_ALLOCNO (a, ai)
                {
                  int old_regno = ALLOCNO_REGNO (a);
                  int new_regno = REGNO (ALLOCNO_EMIT_DATA (a)->reg);

                  ALLOCNO_REGNO (a) = new_regno;

                  if (old_regno != new_regno)
                    setup_reg_classes (new_regno, reg_preferred_class (old_regno),
                                       reg_alternate_class (old_regno),
                                       reg_allocno_class (old_regno));
                }
	    }
	  else
	    {
	      if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL)
		fprintf (ira_dump_file, "Flattening IR\n");
	      ira_flattening (max_regno_before_ira, ira_max_point_before_emit);
	    }
	  /* New insns were generated: add notes and recalculate live
	     info.  */
	  df_analyze ();

	  /* ??? Rebuild the loop tree, but why?  Does the loop tree
	     change if new insns were generated?  Can that be handled
	     by updating the loop tree incrementally?  */
	  loop_optimizer_finalize ();
	  free_dominance_info (CDI_DOMINATORS);
	  loop_optimizer_init (AVOID_CFG_MODIFICATIONS
			       | LOOPS_HAVE_RECORDED_EXITS);

	  if (! ira_use_lra_p)
	    {
	      setup_allocno_assignment_flags ();
	      ira_initiate_assign ();
	      ira_reassign_conflict_allocnos (max_regno);
	    }
	}
    }

  ira_finish_emit_data ();

  setup_reg_renumber ();

  calculate_allocation_cost ();

#ifdef ENABLE_IRA_CHECKING
  if (ira_conflicts_p && ! ira_use_lra_p)
    /* Opposite to reload pass, LRA does not use any conflict info
       from IRA.  We don't rebuild conflict info for LRA (through
       ira_flattening call) and cannot use the check here.  We could
       rebuild this info for LRA in the check mode but there is a risk
       that code generated with the check and without it will be a bit
       different.  Calling ira_flattening in any mode would be a
       wasting CPU time.  So do not check the allocation for LRA.  */
    check_allocation ();
#endif

  if (max_regno != max_regno_before_ira)
    {
      regstat_free_n_sets_and_refs ();
      regstat_free_ri ();
      regstat_init_n_sets_and_refs ();
      regstat_compute_ri ();
    }

  overall_cost_before = ira_overall_cost;
  if (! ira_conflicts_p)
    grow_reg_equivs ();
  else
    {
      fix_reg_equiv_init ();

#ifdef ENABLE_IRA_CHECKING
      print_redundant_copies ();
#endif
      if (! ira_use_lra_p)
	{
	  ira_spilled_reg_stack_slots_num = 0;
	  ira_spilled_reg_stack_slots
	    = ((class ira_spilled_reg_stack_slot *)
	       ira_allocate (max_regno
			     * sizeof (class ira_spilled_reg_stack_slot)));
	  memset ((void *)ira_spilled_reg_stack_slots, 0,
		  max_regno * sizeof (class ira_spilled_reg_stack_slot));
	}
    }
  allocate_initial_values ();

  /* See comment for find_moveable_pseudos call.  */
  if (ira_conflicts_p)
    move_unallocated_pseudos ();

  /* Restore original values.  */
  if (lra_simple_p)
    {
      flag_caller_saves = saved_flag_caller_saves;
      flag_ira_region = saved_flag_ira_region;
    }
}

static void
do_reload (void)
{
  basic_block bb;
  bool need_dce;
  unsigned pic_offset_table_regno = INVALID_REGNUM;

  if (flag_ira_verbose < 10)
    ira_dump_file = dump_file;

  /* If pic_offset_table_rtx is a pseudo register, then keep it so
     after reload to avoid possible wrong usages of hard reg assigned
     to it.  */
  if (pic_offset_table_rtx
      && REGNO (pic_offset_table_rtx) >= FIRST_PSEUDO_REGISTER)
    pic_offset_table_regno = REGNO (pic_offset_table_rtx);

  timevar_push (TV_RELOAD);
  if (ira_use_lra_p)
    {
      if (current_loops != NULL)
	{
	  loop_optimizer_finalize ();
	  free_dominance_info (CDI_DOMINATORS);
	}
      FOR_ALL_BB_FN (bb, cfun)
	bb->loop_father = NULL;
      current_loops = NULL;

      ira_destroy ();

      lra (ira_dump_file);
      /* ???!!! Move it before lra () when we use ira_reg_equiv in
	 LRA.  */
      vec_free (reg_equivs);
      reg_equivs = NULL;
      need_dce = false;
    }
  else
    {
      df_set_flags (DF_NO_INSN_RESCAN);
      build_insn_chain ();

      need_dce = reload (get_insns (), ira_conflicts_p);
    }

  timevar_pop (TV_RELOAD);

  timevar_push (TV_IRA);

  if (ira_conflicts_p && ! ira_use_lra_p)
    {
      ira_free (ira_spilled_reg_stack_slots);
      ira_finish_assign ();
    }

  if (internal_flag_ira_verbose > 0 && ira_dump_file != NULL
      && overall_cost_before != ira_overall_cost)
    fprintf (ira_dump_file, "+++Overall after reload %" PRId64 "\n",
	     ira_overall_cost);

  flag_ira_share_spill_slots = saved_flag_ira_share_spill_slots;

  if (! ira_use_lra_p)
    {
      ira_destroy ();
      if (current_loops != NULL)
	{
	  loop_optimizer_finalize ();
	  free_dominance_info (CDI_DOMINATORS);
	}
      FOR_ALL_BB_FN (bb, cfun)
	bb->loop_father = NULL;
      current_loops = NULL;
      
      regstat_free_ri ();
      regstat_free_n_sets_and_refs ();
    }

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);

  finish_reg_equiv ();

  bitmap_obstack_release (&ira_bitmap_obstack);
#ifndef IRA_NO_OBSTACK
  obstack_free (&ira_obstack, NULL);
#endif

  /* The code after the reload has changed so much that at this point
     we might as well just rescan everything.  Note that
     df_rescan_all_insns is not going to help here because it does not
     touch the artificial uses and defs.  */
  df_finish_pass (true);
  df_scan_alloc (NULL);
  df_scan_blocks ();

  if (optimize > 1)
    {
      df_live_add_problem ();
      df_live_set_all_dirty ();
    }

  if (optimize)
    df_analyze ();

  if (need_dce && optimize)
    run_fast_dce ();

  /* Diagnose uses of the hard frame pointer when it is used as a global
     register.  Often we can get away with letting the user appropriate
     the frame pointer, but we should let them know when code generation
     makes that impossible.  */
  if (global_regs[HARD_FRAME_POINTER_REGNUM] && frame_pointer_needed)
    {
      tree decl = global_regs_decl[HARD_FRAME_POINTER_REGNUM];
      error_at (DECL_SOURCE_LOCATION (current_function_decl),
                "frame pointer required, but reserved");
      inform (DECL_SOURCE_LOCATION (decl), "for %qD", decl);
    }

  /* If we are doing generic stack checking, give a warning if this
     function's frame size is larger than we expect.  */
  if (flag_stack_check == GENERIC_STACK_CHECK)
    {
      poly_int64 size = get_frame_size () + STACK_CHECK_FIXED_FRAME_SIZE;

      for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (df_regs_ever_live_p (i)
	    && !fixed_regs[i]
	    && !crtl->abi->clobbers_full_reg_p (i))
	  size += UNITS_PER_WORD;

      if (constant_lower_bound (size) > STACK_CHECK_MAX_FRAME_SIZE)
	warning (0, "frame size too large for reliable stack checking");
    }

  if (pic_offset_table_regno != INVALID_REGNUM)
    pic_offset_table_rtx = gen_rtx_REG (Pmode, pic_offset_table_regno);

  timevar_pop (TV_IRA);
}

/* Run the integrated register allocator.  */

namespace {

const pass_data pass_data_ira =
{
  RTL_PASS, /* type */
  "ira", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IRA, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_do_not_ggc_collect, /* todo_flags_finish */
};

class pass_ira : public rtl_opt_pass
{
public:
  pass_ira (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_ira, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return !targetm.no_register_allocation;
    }
  virtual unsigned int execute (function *)
    {
      ira (dump_file);
      return 0;
    }

}; // class pass_ira

} // anon namespace

rtl_opt_pass *
make_pass_ira (gcc::context *ctxt)
{
  return new pass_ira (ctxt);
}

namespace {

const pass_data pass_data_reload =
{
  RTL_PASS, /* type */
  "reload", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_RELOAD, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_reload : public rtl_opt_pass
{
public:
  pass_reload (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_reload, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return !targetm.no_register_allocation;
    }
  virtual unsigned int execute (function *)
    {
      do_reload ();
      return 0;
    }

}; // class pass_reload

} // anon namespace

rtl_opt_pass *
make_pass_reload (gcc::context *ctxt)
{
  return new pass_reload (ctxt);
}
