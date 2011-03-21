/* Integrated Register Allocator (IRA) intercommunication header file.
   Copyright (C) 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
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

#include "cfgloop.h"
#include "ira.h"
#include "alloc-pool.h"

/* To provide consistency in naming, all IRA external variables,
   functions, common typedefs start with prefix ira_.  */

#ifdef ENABLE_CHECKING
#define ENABLE_IRA_CHECKING
#endif

#ifdef ENABLE_IRA_CHECKING
#define ira_assert(c) gcc_assert (c)
#else
/* Always define and include C, so that warnings for empty body in an
  ‘if’ statement and unused variable do not occur.  */
#define ira_assert(c) ((void)(0 && (c)))
#endif

/* Compute register frequency from edge frequency FREQ.  It is
   analogous to REG_FREQ_FROM_BB.  When optimizing for size, or
   profile driven feedback is available and the function is never
   executed, frequency is always equivalent.  Otherwise rescale the
   edge frequency.  */
#define REG_FREQ_FROM_EDGE_FREQ(freq)					      \
  (optimize_size || (flag_branch_probabilities && !ENTRY_BLOCK_PTR->count)    \
   ? REG_FREQ_MAX : (freq * REG_FREQ_MAX / BB_FREQ_MAX)			      \
   ? (freq * REG_FREQ_MAX / BB_FREQ_MAX) : 1)

/* All natural loops.  */
extern struct loops ira_loops;

/* A modified value of flag `-fira-verbose' used internally.  */
extern int internal_flag_ira_verbose;

/* Dump file of the allocator if it is not NULL.  */
extern FILE *ira_dump_file;

/* Typedefs for pointers to allocno live range, allocno, and copy of
   allocnos.  */
typedef struct live_range *live_range_t;
typedef struct ira_allocno *ira_allocno_t;
typedef struct ira_allocno_copy *ira_copy_t;
typedef struct ira_object *ira_object_t;

/* Definition of vector of allocnos and copies.  */
DEF_VEC_P(ira_allocno_t);
DEF_VEC_ALLOC_P(ira_allocno_t, heap);
DEF_VEC_P(ira_object_t);
DEF_VEC_ALLOC_P(ira_object_t, heap);
DEF_VEC_P(ira_copy_t);
DEF_VEC_ALLOC_P(ira_copy_t, heap);

/* Typedef for pointer to the subsequent structure.  */
typedef struct ira_loop_tree_node *ira_loop_tree_node_t;

/* In general case, IRA is a regional allocator.  The regions are
   nested and form a tree.  Currently regions are natural loops.  The
   following structure describes loop tree node (representing basic
   block or loop).  We need such tree because the loop tree from
   cfgloop.h is not convenient for the optimization: basic blocks are
   not a part of the tree from cfgloop.h.  We also use the nodes for
   storing additional information about basic blocks/loops for the
   register allocation purposes.  */
struct ira_loop_tree_node
{
  /* The node represents basic block if children == NULL.  */
  basic_block bb;    /* NULL for loop.  */
  struct loop *loop; /* NULL for BB.  */
  /* NEXT/SUBLOOP_NEXT is the next node/loop-node of the same parent.
     SUBLOOP_NEXT is always NULL for BBs.  */
  ira_loop_tree_node_t subloop_next, next;
  /* CHILDREN/SUBLOOPS is the first node/loop-node immediately inside
     the node.  They are NULL for BBs.  */
  ira_loop_tree_node_t subloops, children;
  /* The node immediately containing given node.  */
  ira_loop_tree_node_t parent;

  /* Loop level in range [0, ira_loop_tree_height).  */
  int level;

  /* All the following members are defined only for nodes representing
     loops.  */

  /* True if the loop was marked for removal from the register
     allocation.  */
  bool to_remove_p;

  /* Allocnos in the loop corresponding to their regnos.  If it is
     NULL the loop does not form a separate register allocation region
     (e.g. because it has abnormal enter/exit edges and we can not put
     code for register shuffling on the edges if a different
     allocation is used for a pseudo-register on different sides of
     the edges).  Caps are not in the map (remember we can have more
     one cap with the same regno in a region).  */
  ira_allocno_t *regno_allocno_map;

  /* True if there is an entry to given loop not from its parent (or
     grandparent) basic block.  For example, it is possible for two
     adjacent loops inside another loop.  */
  bool entered_from_non_parent_p;

  /* Maximal register pressure inside loop for given register class
     (defined only for the cover classes).  */
  int reg_pressure[N_REG_CLASSES];

  /* Numbers of allocnos referred or living in the loop node (except
     for its subloops).  */
  bitmap all_allocnos;

  /* Numbers of allocnos living at the loop borders.  */
  bitmap border_allocnos;

  /* Regnos of pseudos modified in the loop node (including its
     subloops).  */
  bitmap modified_regnos;

  /* Numbers of copies referred in the corresponding loop.  */
  bitmap local_copies;
};

/* The root of the loop tree corresponding to the all function.  */
extern ira_loop_tree_node_t ira_loop_tree_root;

/* Height of the loop tree.  */
extern int ira_loop_tree_height;

/* All nodes representing basic blocks are referred through the
   following array.  We can not use basic block member `aux' for this
   because it is used for insertion of insns on edges.  */
extern ira_loop_tree_node_t ira_bb_nodes;

/* Two access macros to the nodes representing basic blocks.  */
#if defined ENABLE_IRA_CHECKING && (GCC_VERSION >= 2007)
#define IRA_BB_NODE_BY_INDEX(index) __extension__			\
(({ ira_loop_tree_node_t _node = (&ira_bb_nodes[index]);	\
     if (_node->children != NULL || _node->loop != NULL || _node->bb == NULL)\
       {								\
         fprintf (stderr,						\
                  "\n%s: %d: error in %s: it is not a block node\n",	\
                  __FILE__, __LINE__, __FUNCTION__);			\
         gcc_unreachable ();						\
       }								\
     _node; }))
#else
#define IRA_BB_NODE_BY_INDEX(index) (&ira_bb_nodes[index])
#endif

#define IRA_BB_NODE(bb) IRA_BB_NODE_BY_INDEX ((bb)->index)

/* All nodes representing loops are referred through the following
   array.  */
extern ira_loop_tree_node_t ira_loop_nodes;

/* Two access macros to the nodes representing loops.  */
#if defined ENABLE_IRA_CHECKING && (GCC_VERSION >= 2007)
#define IRA_LOOP_NODE_BY_INDEX(index) __extension__			\
(({ ira_loop_tree_node_t const _node = (&ira_loop_nodes[index]);\
     if (_node->children == NULL || _node->bb != NULL || _node->loop == NULL)\
       {								\
         fprintf (stderr,						\
                  "\n%s: %d: error in %s: it is not a loop node\n",	\
                  __FILE__, __LINE__, __FUNCTION__);			\
         gcc_unreachable ();						\
       }								\
     _node; }))
#else
#define IRA_LOOP_NODE_BY_INDEX(index) (&ira_loop_nodes[index])
#endif

#define IRA_LOOP_NODE(loop) IRA_LOOP_NODE_BY_INDEX ((loop)->num)


/* The structure describes program points where a given allocno lives.
   To save memory we store allocno conflicts only for the same cover
   class allocnos which is enough to assign hard registers.  To find
   conflicts for other allocnos (e.g. to assign stack memory slot) we
   use the live ranges.  If the live ranges of two allocnos are
   intersected, the allocnos are in conflict.  */
struct live_range
{
  /* Object whose live range is described by given structure.  */
  ira_object_t object;
  /* Program point range.  */
  int start, finish;
  /* Next structure describing program points where the allocno
     lives.  */
  live_range_t next;
  /* Pointer to structures with the same start/finish.  */
  live_range_t start_next, finish_next;
};

/* Program points are enumerated by numbers from range
   0..IRA_MAX_POINT-1.  There are approximately two times more program
   points than insns.  Program points are places in the program where
   liveness info can be changed.  In most general case (there are more
   complicated cases too) some program points correspond to places
   where input operand dies and other ones correspond to places where
   output operands are born.  */
extern int ira_max_point;

/* Arrays of size IRA_MAX_POINT mapping a program point to the allocno
   live ranges with given start/finish point.  */
extern live_range_t *ira_start_point_ranges, *ira_finish_point_ranges;

/* A structure representing conflict information for an allocno
   (or one of its subwords).  */
struct ira_object
{
  /* The allocno associated with this record.  */
  ira_allocno_t allocno;
  /* Vector of accumulated conflicting conflict_redords with NULL end
     marker (if OBJECT_CONFLICT_VEC_P is true) or conflict bit vector
     otherwise.  Only ira_objects belonging to allocnos with the
     same cover class are in the vector or in the bit vector.  */
  void *conflicts_array;
  /* Pointer to structures describing at what program point the
     object lives.  We always maintain the list in such way that *the
     ranges in the list are not intersected and ordered by decreasing
     their program points*.  */
  live_range_t live_ranges;
  /* The subword within ALLOCNO which is represented by this object.
     Zero means the lowest-order subword (or the entire allocno in case
     it is not being tracked in subwords).  */
  int subword;
  /* Allocated size of the conflicts array.  */
  unsigned int conflicts_array_size;
  /* A unique number for every instance of this structure, which is used
     to represent it in conflict bit vectors.  */
  int id;
  /* Before building conflicts, MIN and MAX are initialized to
     correspondingly minimal and maximal points of the accumulated
     live ranges.  Afterwards, they hold the minimal and maximal ids
     of other ira_objects that this one can conflict with.  */
  int min, max;
  /* Initial and accumulated hard registers conflicting with this
     object and as a consequences can not be assigned to the allocno.
     All non-allocatable hard regs and hard regs of cover classes
     different from given allocno one are included in the sets.  */
  HARD_REG_SET conflict_hard_regs, total_conflict_hard_regs;
  /* Number of accumulated conflicts in the vector of conflicting
     objects.  */
  int num_accumulated_conflicts;
  /* TRUE if conflicts are represented by a vector of pointers to
     ira_object structures.  Otherwise, we use a bit vector indexed
     by conflict ID numbers.  */
  unsigned int conflict_vec_p : 1;
};

/* A structure representing an allocno (allocation entity).  Allocno
   represents a pseudo-register in an allocation region.  If
   pseudo-register does not live in a region but it lives in the
   nested regions, it is represented in the region by special allocno
   called *cap*.  There may be more one cap representing the same
   pseudo-register in region.  It means that the corresponding
   pseudo-register lives in more one non-intersected subregion.  */
struct ira_allocno
{
  /* The allocno order number starting with 0.  Each allocno has an
     unique number and the number is never changed for the
     allocno.  */
  int num;
  /* Regno for allocno or cap.  */
  int regno;
  /* Mode of the allocno which is the mode of the corresponding
     pseudo-register.  */
  enum machine_mode mode;
  /* Hard register assigned to given allocno.  Negative value means
     that memory was allocated to the allocno.  During the reload,
     spilled allocno has value equal to the corresponding stack slot
     number (0, ...) - 2.  Value -1 is used for allocnos spilled by the
     reload (at this point pseudo-register has only one allocno) which
     did not get stack slot yet.  */
  int hard_regno;
  /* Final rtx representation of the allocno.  */
  rtx reg;
  /* Allocnos with the same regno are linked by the following member.
     Allocnos corresponding to inner loops are first in the list (it
     corresponds to depth-first traverse of the loops).  */
  ira_allocno_t next_regno_allocno;
  /* There may be different allocnos with the same regno in different
     regions.  Allocnos are bound to the corresponding loop tree node.
     Pseudo-register may have only one regular allocno with given loop
     tree node but more than one cap (see comments above).  */
  ira_loop_tree_node_t loop_tree_node;
  /* Accumulated usage references of the allocno.  Here and below,
     word 'accumulated' means info for given region and all nested
     subregions.  In this case, 'accumulated' means sum of references
     of the corresponding pseudo-register in this region and in all
     nested subregions recursively. */
  int nrefs;
  /* Accumulated frequency of usage of the allocno.  */
  int freq;
  /* Register class which should be used for allocation for given
     allocno.  NO_REGS means that we should use memory.  */
  enum reg_class cover_class;
  /* Minimal accumulated and updated costs of usage register of the
     cover class for the allocno.  */
  int cover_class_cost, updated_cover_class_cost;
  /* Minimal accumulated, and updated costs of memory for the allocno.
     At the allocation start, the original and updated costs are
     equal.  The updated cost may be changed after finishing
     allocation in a region and starting allocation in a subregion.
     The change reflects the cost of spill/restore code on the
     subregion border if we assign memory to the pseudo in the
     subregion.  */
  int memory_cost, updated_memory_cost;
  /* Accumulated number of points where the allocno lives and there is
     excess pressure for its class.  Excess pressure for a register
     class at some point means that there are more allocnos of given
     register class living at the point than number of hard-registers
     of the class available for the allocation.  */
  int excess_pressure_points_num;
  /* Copies to other non-conflicting allocnos.  The copies can
     represent move insn or potential move insn usually because of two
     operand insn constraints.  */
  ira_copy_t allocno_copies;
  /* It is a allocno (cap) representing given allocno on upper loop tree
     level.  */
  ira_allocno_t cap;
  /* It is a link to allocno (cap) on lower loop level represented by
     given cap.  Null if given allocno is not a cap.  */
  ira_allocno_t cap_member;
  /* Coalesced allocnos form a cyclic list.  One allocno given by
     FIRST_COALESCED_ALLOCNO represents all coalesced allocnos.  The
     list is chained by NEXT_COALESCED_ALLOCNO.  */
  ira_allocno_t first_coalesced_allocno;
  ira_allocno_t next_coalesced_allocno;
  /* The number of objects tracked in the following array.  */
  int num_objects;
  /* An array of structures describing conflict information and live
     ranges for each object associated with the allocno.  There may be
     more than one such object in cases where the allocno represents a
     multi-word register.  */
  ira_object_t objects[2];
  /* Accumulated frequency of calls which given allocno
     intersects.  */
  int call_freq;
  /* Accumulated number of the intersected calls.  */
  int calls_crossed_num;
  /* TRUE if the allocno assigned to memory was a destination of
     removed move (see ira-emit.c) at loop exit because the value of
     the corresponding pseudo-register is not changed inside the
     loop.  */
  unsigned int mem_optimized_dest_p : 1;
  /* TRUE if the corresponding pseudo-register has disjoint live
     ranges and the other allocnos of the pseudo-register except this
     one changed REG.  */
  unsigned int somewhere_renamed_p : 1;
  /* TRUE if allocno with the same REGNO in a subregion has been
     renamed, in other words, got a new pseudo-register.  */
  unsigned int child_renamed_p : 1;
  /* During the reload, value TRUE means that we should not reassign a
     hard register to the allocno got memory earlier.  It is set up
     when we removed memory-memory move insn before each iteration of
     the reload.  */
  unsigned int dont_reassign_p : 1;
#ifdef STACK_REGS
  /* Set to TRUE if allocno can't be assigned to the stack hard
     register correspondingly in this region and area including the
     region and all its subregions recursively.  */
  unsigned int no_stack_reg_p : 1, total_no_stack_reg_p : 1;
#endif
  /* TRUE value means that there is no sense to spill the allocno
     during coloring because the spill will result in additional
     reloads in reload pass.  */
  unsigned int bad_spill_p : 1;
  /* TRUE value means that the allocno was not removed yet from the
     conflicting graph during colouring.  */
  unsigned int in_graph_p : 1;
  /* TRUE if a hard register or memory has been assigned to the
     allocno.  */
  unsigned int assigned_p : 1;
  /* TRUE if it is put on the stack to make other allocnos
     colorable.  */
  unsigned int may_be_spilled_p : 1;
  /* TRUE if the allocno was removed from the splay tree used to
     choose allocn for spilling (see ira-color.c::.  */
  unsigned int splay_removed_p : 1;
  /* Non NULL if we remove restoring value from given allocno to
     MEM_OPTIMIZED_DEST at loop exit (see ira-emit.c) because the
     allocno value is not changed inside the loop.  */
  ira_allocno_t mem_optimized_dest;
  /* Array of usage costs (accumulated and the one updated during
     coloring) for each hard register of the allocno cover class.  The
     member value can be NULL if all costs are the same and equal to
     COVER_CLASS_COST.  For example, the costs of two different hard
     registers can be different if one hard register is callee-saved
     and another one is callee-used and the allocno lives through
     calls.  Another example can be case when for some insn the
     corresponding pseudo-register value should be put in specific
     register class (e.g. AREG for x86) which is a strict subset of
     the allocno cover class (GENERAL_REGS for x86).  We have updated
     costs to reflect the situation when the usage cost of a hard
     register is decreased because the allocno is connected to another
     allocno by a copy and the another allocno has been assigned to
     the hard register.  */
  int *hard_reg_costs, *updated_hard_reg_costs;
  /* Array of decreasing costs (accumulated and the one updated during
     coloring) for allocnos conflicting with given allocno for hard
     regno of the allocno cover class.  The member value can be NULL
     if all costs are the same.  These costs are used to reflect
     preferences of other allocnos not assigned yet during assigning
     to given allocno.  */
  int *conflict_hard_reg_costs, *updated_conflict_hard_reg_costs;
  /* Size (in hard registers) of the same cover class allocnos with
     TRUE in_graph_p value and conflicting with given allocno during
     each point of graph coloring.  */
  int left_conflicts_size;
  /* Number of hard registers of the allocno cover class really
     available for the allocno allocation.  */
  int available_regs_num;
  /* Allocnos in a bucket (used in coloring) chained by the following
     two members.  */
  ira_allocno_t next_bucket_allocno;
  ira_allocno_t prev_bucket_allocno;
  /* Used for temporary purposes.  */
  int temp;
};

/* All members of the allocno structures should be accessed only
   through the following macros.  */
#define ALLOCNO_NUM(A) ((A)->num)
#define ALLOCNO_REGNO(A) ((A)->regno)
#define ALLOCNO_REG(A) ((A)->reg)
#define ALLOCNO_NEXT_REGNO_ALLOCNO(A) ((A)->next_regno_allocno)
#define ALLOCNO_LOOP_TREE_NODE(A) ((A)->loop_tree_node)
#define ALLOCNO_CAP(A) ((A)->cap)
#define ALLOCNO_CAP_MEMBER(A) ((A)->cap_member)
#define ALLOCNO_NREFS(A) ((A)->nrefs)
#define ALLOCNO_FREQ(A) ((A)->freq)
#define ALLOCNO_HARD_REGNO(A) ((A)->hard_regno)
#define ALLOCNO_CALL_FREQ(A) ((A)->call_freq)
#define ALLOCNO_CALLS_CROSSED_NUM(A) ((A)->calls_crossed_num)
#define ALLOCNO_MEM_OPTIMIZED_DEST(A) ((A)->mem_optimized_dest)
#define ALLOCNO_MEM_OPTIMIZED_DEST_P(A) ((A)->mem_optimized_dest_p)
#define ALLOCNO_SOMEWHERE_RENAMED_P(A) ((A)->somewhere_renamed_p)
#define ALLOCNO_CHILD_RENAMED_P(A) ((A)->child_renamed_p)
#define ALLOCNO_DONT_REASSIGN_P(A) ((A)->dont_reassign_p)
#ifdef STACK_REGS
#define ALLOCNO_NO_STACK_REG_P(A) ((A)->no_stack_reg_p)
#define ALLOCNO_TOTAL_NO_STACK_REG_P(A) ((A)->total_no_stack_reg_p)
#endif
#define ALLOCNO_BAD_SPILL_P(A) ((A)->bad_spill_p)
#define ALLOCNO_IN_GRAPH_P(A) ((A)->in_graph_p)
#define ALLOCNO_ASSIGNED_P(A) ((A)->assigned_p)
#define ALLOCNO_MAY_BE_SPILLED_P(A) ((A)->may_be_spilled_p)
#define ALLOCNO_SPLAY_REMOVED_P(A) ((A)->splay_removed_p)
#define ALLOCNO_MODE(A) ((A)->mode)
#define ALLOCNO_COPIES(A) ((A)->allocno_copies)
#define ALLOCNO_HARD_REG_COSTS(A) ((A)->hard_reg_costs)
#define ALLOCNO_UPDATED_HARD_REG_COSTS(A) ((A)->updated_hard_reg_costs)
#define ALLOCNO_CONFLICT_HARD_REG_COSTS(A) \
  ((A)->conflict_hard_reg_costs)
#define ALLOCNO_UPDATED_CONFLICT_HARD_REG_COSTS(A) \
  ((A)->updated_conflict_hard_reg_costs)
#define ALLOCNO_LEFT_CONFLICTS_SIZE(A) ((A)->left_conflicts_size)
#define ALLOCNO_COVER_CLASS(A) ((A)->cover_class)
#define ALLOCNO_COVER_CLASS_COST(A) ((A)->cover_class_cost)
#define ALLOCNO_UPDATED_COVER_CLASS_COST(A) ((A)->updated_cover_class_cost)
#define ALLOCNO_MEMORY_COST(A) ((A)->memory_cost)
#define ALLOCNO_UPDATED_MEMORY_COST(A) ((A)->updated_memory_cost)
#define ALLOCNO_EXCESS_PRESSURE_POINTS_NUM(A) ((A)->excess_pressure_points_num)
#define ALLOCNO_AVAILABLE_REGS_NUM(A) ((A)->available_regs_num)
#define ALLOCNO_NEXT_BUCKET_ALLOCNO(A) ((A)->next_bucket_allocno)
#define ALLOCNO_PREV_BUCKET_ALLOCNO(A) ((A)->prev_bucket_allocno)
#define ALLOCNO_TEMP(A) ((A)->temp)
#define ALLOCNO_FIRST_COALESCED_ALLOCNO(A) ((A)->first_coalesced_allocno)
#define ALLOCNO_NEXT_COALESCED_ALLOCNO(A) ((A)->next_coalesced_allocno)
#define ALLOCNO_OBJECT(A,N) ((A)->objects[N])
#define ALLOCNO_NUM_OBJECTS(A) ((A)->num_objects)

#define OBJECT_ALLOCNO(C) ((C)->allocno)
#define OBJECT_SUBWORD(C) ((C)->subword)
#define OBJECT_CONFLICT_ARRAY(C) ((C)->conflicts_array)
#define OBJECT_CONFLICT_VEC(C) ((ira_object_t *)(C)->conflicts_array)
#define OBJECT_CONFLICT_BITVEC(C) ((IRA_INT_TYPE *)(C)->conflicts_array)
#define OBJECT_CONFLICT_ARRAY_SIZE(C) ((C)->conflicts_array_size)
#define OBJECT_CONFLICT_VEC_P(C) ((C)->conflict_vec_p)
#define OBJECT_NUM_CONFLICTS(C) ((C)->num_accumulated_conflicts)
#define OBJECT_CONFLICT_HARD_REGS(C) ((C)->conflict_hard_regs)
#define OBJECT_TOTAL_CONFLICT_HARD_REGS(C) ((C)->total_conflict_hard_regs)
#define OBJECT_MIN(C) ((C)->min)
#define OBJECT_MAX(C) ((C)->max)
#define OBJECT_CONFLICT_ID(C) ((C)->id)
#define OBJECT_LIVE_RANGES(A) ((A)->live_ranges)

/* Map regno -> allocnos with given regno (see comments for
   allocno member `next_regno_allocno').  */
extern ira_allocno_t *ira_regno_allocno_map;

/* Array of references to all allocnos.  The order number of the
   allocno corresponds to the index in the array.  Removed allocnos
   have NULL element value.  */
extern ira_allocno_t *ira_allocnos;

/* The size of the previous array.  */
extern int ira_allocnos_num;

/* Map a conflict id to its corresponding ira_object structure.  */
extern ira_object_t *ira_object_id_map;

/* The size of the previous array.  */
extern int ira_objects_num;

/* The following structure represents a copy of two allocnos.  The
   copies represent move insns or potential move insns usually because
   of two operand insn constraints.  To remove register shuffle, we
   also create copies between allocno which is output of an insn and
   allocno becoming dead in the insn.  */
struct ira_allocno_copy
{
  /* The unique order number of the copy node starting with 0.  */
  int num;
  /* Allocnos connected by the copy.  The first allocno should have
     smaller order number than the second one.  */
  ira_allocno_t first, second;
  /* Execution frequency of the copy.  */
  int freq;
  bool constraint_p;
  /* It is a move insn which is an origin of the copy.  The member
     value for the copy representing two operand insn constraints or
     for the copy created to remove register shuffle is NULL.  In last
     case the copy frequency is smaller than the corresponding insn
     execution frequency.  */
  rtx insn;
  /* All copies with the same allocno as FIRST are linked by the two
     following members.  */
  ira_copy_t prev_first_allocno_copy, next_first_allocno_copy;
  /* All copies with the same allocno as SECOND are linked by the two
     following members.  */
  ira_copy_t prev_second_allocno_copy, next_second_allocno_copy;
  /* Region from which given copy is originated.  */
  ira_loop_tree_node_t loop_tree_node;
};

/* Array of references to all copies.  The order number of the copy
   corresponds to the index in the array.  Removed copies have NULL
   element value.  */
extern ira_copy_t *ira_copies;

/* Size of the previous array.  */
extern int ira_copies_num;

/* The following structure describes a stack slot used for spilled
   pseudo-registers.  */
struct ira_spilled_reg_stack_slot
{
  /* pseudo-registers assigned to the stack slot.  */
  bitmap_head spilled_regs;
  /* RTL representation of the stack slot.  */
  rtx mem;
  /* Size of the stack slot.  */
  unsigned int width;
};

/* The number of elements in the following array.  */
extern int ira_spilled_reg_stack_slots_num;

/* The following array contains info about spilled pseudo-registers
   stack slots used in current function so far.  */
extern struct ira_spilled_reg_stack_slot *ira_spilled_reg_stack_slots;

/* Correspondingly overall cost of the allocation, cost of the
   allocnos assigned to hard-registers, cost of the allocnos assigned
   to memory, cost of loads, stores and register move insns generated
   for pseudo-register live range splitting (see ira-emit.c).  */
extern int ira_overall_cost;
extern int ira_reg_cost, ira_mem_cost;
extern int ira_load_cost, ira_store_cost, ira_shuffle_cost;
extern int ira_move_loops_num, ira_additional_jumps_num;

/* This page contains a bitset implementation called 'min/max sets' used to
   record conflicts in IRA.
   They are named min/maxs set since we keep track of a minimum and a maximum
   bit number for each set representing the bounds of valid elements.  Otherwise,
   the implementation resembles sbitmaps in that we store an array of integers
   whose bits directly represent the members of the set.  */

/* The type used as elements in the array, and the number of bits in
   this type.  */

#define IRA_INT_BITS HOST_BITS_PER_WIDE_INT
#define IRA_INT_TYPE HOST_WIDE_INT

/* Set, clear or test bit number I in R, a bit vector of elements with
   minimal index and maximal index equal correspondingly to MIN and
   MAX.  */
#if defined ENABLE_IRA_CHECKING && (GCC_VERSION >= 2007)

#define SET_MINMAX_SET_BIT(R, I, MIN, MAX) __extension__	        \
  (({ int _min = (MIN), _max = (MAX), _i = (I);				\
     if (_i < _min || _i > _max)					\
       {								\
         fprintf (stderr,						\
                  "\n%s: %d: error in %s: %d not in range [%d,%d]\n",   \
                  __FILE__, __LINE__, __FUNCTION__, _i, _min, _max);	\
         gcc_unreachable ();						\
       }								\
     ((R)[(unsigned) (_i - _min) / IRA_INT_BITS]			\
      |= ((IRA_INT_TYPE) 1 << ((unsigned) (_i - _min) % IRA_INT_BITS))); }))


#define CLEAR_MINMAX_SET_BIT(R, I, MIN, MAX) __extension__	        \
  (({ int _min = (MIN), _max = (MAX), _i = (I);				\
     if (_i < _min || _i > _max)					\
       {								\
         fprintf (stderr,						\
                  "\n%s: %d: error in %s: %d not in range [%d,%d]\n",   \
                  __FILE__, __LINE__, __FUNCTION__, _i, _min, _max);	\
         gcc_unreachable ();						\
       }								\
     ((R)[(unsigned) (_i - _min) / IRA_INT_BITS]			\
      &= ~((IRA_INT_TYPE) 1 << ((unsigned) (_i - _min) % IRA_INT_BITS))); }))

#define TEST_MINMAX_SET_BIT(R, I, MIN, MAX) __extension__	        \
  (({ int _min = (MIN), _max = (MAX), _i = (I);				\
     if (_i < _min || _i > _max)					\
       {								\
         fprintf (stderr,						\
                  "\n%s: %d: error in %s: %d not in range [%d,%d]\n",   \
                  __FILE__, __LINE__, __FUNCTION__, _i, _min, _max);	\
         gcc_unreachable ();						\
       }								\
     ((R)[(unsigned) (_i - _min) / IRA_INT_BITS]			\
      & ((IRA_INT_TYPE) 1 << ((unsigned) (_i - _min) % IRA_INT_BITS))); }))

#else

#define SET_MINMAX_SET_BIT(R, I, MIN, MAX)			\
  ((R)[(unsigned) ((I) - (MIN)) / IRA_INT_BITS]			\
   |= ((IRA_INT_TYPE) 1 << ((unsigned) ((I) - (MIN)) % IRA_INT_BITS)))

#define CLEAR_MINMAX_SET_BIT(R, I, MIN, MAX)			\
  ((R)[(unsigned) ((I) - (MIN)) / IRA_INT_BITS]			\
   &= ~((IRA_INT_TYPE) 1 << ((unsigned) ((I) - (MIN)) % IRA_INT_BITS)))

#define TEST_MINMAX_SET_BIT(R, I, MIN, MAX)			\
  ((R)[(unsigned) ((I) - (MIN)) / IRA_INT_BITS]			\
   & ((IRA_INT_TYPE) 1 << ((unsigned) ((I) - (MIN)) % IRA_INT_BITS)))

#endif

/* The iterator for min/max sets.  */
typedef struct {

  /* Array containing the bit vector.  */
  IRA_INT_TYPE *vec;

  /* The number of the current element in the vector.  */
  unsigned int word_num;

  /* The number of bits in the bit vector.  */
  unsigned int nel;

  /* The current bit index of the bit vector.  */
  unsigned int bit_num;

  /* Index corresponding to the 1st bit of the bit vector.   */
  int start_val;

  /* The word of the bit vector currently visited.  */
  unsigned IRA_INT_TYPE word;
} minmax_set_iterator;

/* Initialize the iterator I for bit vector VEC containing minimal and
   maximal values MIN and MAX.  */
static inline void
minmax_set_iter_init (minmax_set_iterator *i, IRA_INT_TYPE *vec, int min,
		      int max)
{
  i->vec = vec;
  i->word_num = 0;
  i->nel = max < min ? 0 : max - min + 1;
  i->start_val = min;
  i->bit_num = 0;
  i->word = i->nel == 0 ? 0 : vec[0];
}

/* Return TRUE if we have more allocnos to visit, in which case *N is
   set to the number of the element to be visited.  Otherwise, return
   FALSE.  */
static inline bool
minmax_set_iter_cond (minmax_set_iterator *i, int *n)
{
  /* Skip words that are zeros.  */
  for (; i->word == 0; i->word = i->vec[i->word_num])
    {
      i->word_num++;
      i->bit_num = i->word_num * IRA_INT_BITS;

      /* If we have reached the end, break.  */
      if (i->bit_num >= i->nel)
	return false;
    }

  /* Skip bits that are zero.  */
  for (; (i->word & 1) == 0; i->word >>= 1)
    i->bit_num++;

  *n = (int) i->bit_num + i->start_val;

  return true;
}

/* Advance to the next element in the set.  */
static inline void
minmax_set_iter_next (minmax_set_iterator *i)
{
  i->word >>= 1;
  i->bit_num++;
}

/* Loop over all elements of a min/max set given by bit vector VEC and
   their minimal and maximal values MIN and MAX.  In each iteration, N
   is set to the number of next allocno.  ITER is an instance of
   minmax_set_iterator used to iterate over the set.  */
#define FOR_EACH_BIT_IN_MINMAX_SET(VEC, MIN, MAX, N, ITER)	\
  for (minmax_set_iter_init (&(ITER), (VEC), (MIN), (MAX));	\
       minmax_set_iter_cond (&(ITER), &(N));			\
       minmax_set_iter_next (&(ITER)))

struct target_ira_int {
  /* Initialized once.  It is a maximal possible size of the allocated
     struct costs.  */
  int x_max_struct_costs_size;

  /* Allocated and initialized once, and used to initialize cost values
     for each insn.  */
  struct costs *x_init_cost;

  /* Allocated once, and used for temporary purposes.  */
  struct costs *x_temp_costs;

  /* Allocated once, and used for the cost calculation.  */
  struct costs *x_op_costs[MAX_RECOG_OPERANDS];
  struct costs *x_this_op_costs[MAX_RECOG_OPERANDS];

  /* Classes used for cost calculation.  They may be different on
     different iterations of the cost calculations or in different
     optimization modes.  */
  enum reg_class *x_cost_classes;

  /* Hard registers that can not be used for the register allocator for
     all functions of the current compilation unit.  */
  HARD_REG_SET x_no_unit_alloc_regs;

  /* Map: hard regs X modes -> set of hard registers for storing value
     of given mode starting with given hard register.  */
  HARD_REG_SET (x_ira_reg_mode_hard_regset
		[FIRST_PSEUDO_REGISTER][NUM_MACHINE_MODES]);

  /* Array based on TARGET_REGISTER_MOVE_COST.  Don't use
     ira_register_move_cost directly.  Use function of
     ira_get_may_move_cost instead.  */
  move_table *x_ira_register_move_cost[MAX_MACHINE_MODE];

  /* Similar to may_move_in_cost but it is calculated in IRA instead of
     regclass.  Another difference we take only available hard registers
     into account to figure out that one register class is a subset of
     the another one.  Don't use it directly.  Use function of
     ira_get_may_move_cost instead.  */
  move_table *x_ira_may_move_in_cost[MAX_MACHINE_MODE];

  /* Similar to may_move_out_cost but it is calculated in IRA instead of
     regclass.  Another difference we take only available hard registers
     into account to figure out that one register class is a subset of
     the another one.  Don't use it directly.  Use function of
     ira_get_may_move_cost instead.  */
  move_table *x_ira_may_move_out_cost[MAX_MACHINE_MODE];

  /* Register class subset relation: TRUE if the first class is a subset
     of the second one considering only hard registers available for the
     allocation.  */
  int x_ira_class_subset_p[N_REG_CLASSES][N_REG_CLASSES];

  /* Array of the number of hard registers of given class which are
     available for allocation.  The order is defined by the the hard
     register numbers.  */
  short x_ira_non_ordered_class_hard_regs[N_REG_CLASSES][FIRST_PSEUDO_REGISTER];

  /* Index (in ira_class_hard_regs; for given register class and hard
     register (in general case a hard register can belong to several
     register classes;.  The index is negative for hard registers
     unavailable for the allocation.  */
  short x_ira_class_hard_reg_index[N_REG_CLASSES][FIRST_PSEUDO_REGISTER];

  /* Array whose values are hard regset of hard registers available for
     the allocation of given register class whose HARD_REGNO_MODE_OK
     values for given mode are zero.  */
  HARD_REG_SET x_prohibited_class_mode_regs[N_REG_CLASSES][NUM_MACHINE_MODES];

  /* The value is number of elements in the subsequent array.  */
  int x_ira_important_classes_num;

  /* The array containing non-empty classes (including non-empty cover
     classes; which are subclasses of cover classes.  Such classes is
     important for calculation of the hard register usage costs.  */
  enum reg_class x_ira_important_classes[N_REG_CLASSES];

  /* The biggest important class inside of intersection of the two
     classes (that is calculated taking only hard registers available
     for allocation into account;.  If the both classes contain no hard
     registers available for allocation, the value is calculated with
     taking all hard-registers including fixed ones into account.  */
  enum reg_class x_ira_reg_class_intersect[N_REG_CLASSES][N_REG_CLASSES];

  /* True if the two classes (that is calculated taking only hard
     registers available for allocation into account; are
     intersected.  */
  bool x_ira_reg_classes_intersect_p[N_REG_CLASSES][N_REG_CLASSES];

  /* Classes with end marker LIM_REG_CLASSES which are intersected with
     given class (the first index;.  That includes given class itself.
     This is calculated taking only hard registers available for
     allocation into account.  */
  enum reg_class x_ira_reg_class_super_classes[N_REG_CLASSES][N_REG_CLASSES];

  /* The biggest important class inside of union of the two classes
     (that is calculated taking only hard registers available for
     allocation into account;.  If the both classes contain no hard
     registers available for allocation, the value is calculated with
     taking all hard-registers including fixed ones into account.  In
     other words, the value is the corresponding reg_class_subunion
     value.  */
  enum reg_class x_ira_reg_class_union[N_REG_CLASSES][N_REG_CLASSES];

  /* For each reg class, table listing all the classes contained in it
     (excluding the class itself.  Non-allocatable registers are
     excluded from the consideration;.  */
  enum reg_class x_alloc_reg_class_subclasses[N_REG_CLASSES][N_REG_CLASSES];

  /* Array whose values are hard regset of hard registers for which
     move of the hard register in given mode into itself is
     prohibited.  */
  HARD_REG_SET x_ira_prohibited_mode_move_regs[NUM_MACHINE_MODES];

  /* Flag of that the above array has been initialized.  */
  bool x_ira_prohibited_mode_move_regs_initialized_p;
};

extern struct target_ira_int default_target_ira_int;
#if SWITCHABLE_TARGET
extern struct target_ira_int *this_target_ira_int;
#else
#define this_target_ira_int (&default_target_ira_int)
#endif

#define ira_reg_mode_hard_regset \
  (this_target_ira_int->x_ira_reg_mode_hard_regset)
#define ira_register_move_cost \
  (this_target_ira_int->x_ira_register_move_cost)
#define ira_may_move_in_cost \
  (this_target_ira_int->x_ira_may_move_in_cost)
#define ira_may_move_out_cost \
  (this_target_ira_int->x_ira_may_move_out_cost)
#define ira_class_subset_p \
  (this_target_ira_int->x_ira_class_subset_p)
#define ira_non_ordered_class_hard_regs \
  (this_target_ira_int->x_ira_non_ordered_class_hard_regs)
#define ira_class_hard_reg_index \
  (this_target_ira_int->x_ira_class_hard_reg_index)
#define prohibited_class_mode_regs \
  (this_target_ira_int->x_prohibited_class_mode_regs)
#define ira_important_classes_num \
  (this_target_ira_int->x_ira_important_classes_num)
#define ira_important_classes \
  (this_target_ira_int->x_ira_important_classes)
#define ira_reg_class_intersect \
  (this_target_ira_int->x_ira_reg_class_intersect)
#define ira_reg_classes_intersect_p \
  (this_target_ira_int->x_ira_reg_classes_intersect_p)
#define ira_reg_class_super_classes \
  (this_target_ira_int->x_ira_reg_class_super_classes)
#define ira_reg_class_union \
  (this_target_ira_int->x_ira_reg_class_union)
#define ira_prohibited_mode_move_regs \
  (this_target_ira_int->x_ira_prohibited_mode_move_regs)

/* ira.c: */

extern void *ira_allocate (size_t);
extern void *ira_reallocate (void *, size_t);
extern void ira_free (void *addr);
extern bitmap ira_allocate_bitmap (void);
extern void ira_free_bitmap (bitmap);
extern void ira_print_disposition (FILE *);
extern void ira_debug_disposition (void);
extern void ira_debug_class_cover (void);
extern void ira_init_register_move_cost (enum machine_mode);

/* The length of the two following arrays.  */
extern int ira_reg_equiv_len;

/* The element value is TRUE if the corresponding regno value is
   invariant.  */
extern bool *ira_reg_equiv_invariant_p;

/* The element value is equiv constant of given pseudo-register or
   NULL_RTX.  */
extern rtx *ira_reg_equiv_const;

/* ira-build.c */

/* The current loop tree node and its regno allocno map.  */
extern ira_loop_tree_node_t ira_curr_loop_tree_node;
extern ira_allocno_t *ira_curr_regno_allocno_map;

extern void ira_debug_copy (ira_copy_t);
extern void ira_debug_copies (void);
extern void ira_debug_allocno_copies (ira_allocno_t);

extern void ira_traverse_loop_tree (bool, ira_loop_tree_node_t,
				    void (*) (ira_loop_tree_node_t),
				    void (*) (ira_loop_tree_node_t));
extern ira_allocno_t ira_parent_allocno (ira_allocno_t);
extern ira_allocno_t ira_parent_or_cap_allocno (ira_allocno_t);
extern ira_allocno_t ira_create_allocno (int, bool, ira_loop_tree_node_t);
extern void ira_create_allocno_objects (ira_allocno_t);
extern void ira_set_allocno_cover_class (ira_allocno_t, enum reg_class);
extern bool ira_conflict_vector_profitable_p (ira_object_t, int);
extern void ira_allocate_conflict_vec (ira_object_t, int);
extern void ira_allocate_object_conflicts (ira_object_t, int);
extern void ior_hard_reg_conflicts (ira_allocno_t, HARD_REG_SET *);
extern void ira_print_expanded_allocno (ira_allocno_t);
extern void ira_add_live_range_to_object (ira_object_t, int, int);
extern live_range_t ira_create_live_range (ira_object_t, int, int,
					   live_range_t);
extern live_range_t ira_copy_live_range_list (live_range_t);
extern live_range_t ira_merge_live_ranges (live_range_t, live_range_t);
extern bool ira_live_ranges_intersect_p (live_range_t, live_range_t);
extern void ira_finish_live_range (live_range_t);
extern void ira_finish_live_range_list (live_range_t);
extern void ira_free_allocno_updated_costs (ira_allocno_t);
extern ira_copy_t ira_create_copy (ira_allocno_t, ira_allocno_t,
				   int, bool, rtx, ira_loop_tree_node_t);
extern void ira_add_allocno_copy_to_list (ira_copy_t);
extern void ira_swap_allocno_copy_ends_if_necessary (ira_copy_t);
extern ira_copy_t ira_add_allocno_copy (ira_allocno_t, ira_allocno_t, int,
					bool, rtx, ira_loop_tree_node_t);

extern int *ira_allocate_cost_vector (enum reg_class);
extern void ira_free_cost_vector (int *, enum reg_class);

extern void ira_flattening (int, int);
extern bool ira_build (bool);
extern void ira_destroy (void);

/* ira-costs.c */
extern void ira_init_costs_once (void);
extern void ira_init_costs (void);
extern void ira_finish_costs_once (void);
extern void ira_costs (void);
extern void ira_tune_allocno_costs_and_cover_classes (void);

/* ira-lives.c */

extern void ira_rebuild_start_finish_chains (void);
extern void ira_print_live_range_list (FILE *, live_range_t);
extern void ira_debug_live_range_list (live_range_t);
extern void ira_debug_allocno_live_ranges (ira_allocno_t);
extern void ira_debug_live_ranges (void);
extern void ira_create_allocno_live_ranges (void);
extern void ira_compress_allocno_live_ranges (void);
extern void ira_finish_allocno_live_ranges (void);

/* ira-conflicts.c */
extern void ira_debug_conflicts (bool);
extern void ira_build_conflicts (void);

/* ira-color.c */
extern int ira_loop_edge_freq (ira_loop_tree_node_t, int, bool);
extern void ira_reassign_conflict_allocnos (int);
extern void ira_initiate_assign (void);
extern void ira_finish_assign (void);
extern void ira_color (void);

/* ira-emit.c */
extern void ira_emit (bool);



/* Return cost of moving value of MODE from register of class FROM to
   register of class TO.  */
static inline int
ira_get_register_move_cost (enum machine_mode mode,
			    enum reg_class from, enum reg_class to)
{
  if (ira_register_move_cost[mode] == NULL)
    ira_init_register_move_cost (mode);
  return ira_register_move_cost[mode][from][to];
}

/* Return cost of moving value of MODE from register of class FROM to
   register of class TO.  Return zero if IN_P is true and FROM is
   subset of TO or if IN_P is false and FROM is superset of TO.  */
static inline int
ira_get_may_move_cost (enum machine_mode mode,
		       enum reg_class from, enum reg_class to,
		       bool in_p)
{
  if (ira_register_move_cost[mode] == NULL)
    ira_init_register_move_cost (mode);
  return (in_p
	  ? ira_may_move_in_cost[mode][from][to]
	  : ira_may_move_out_cost[mode][from][to]);
}



/* The iterator for all allocnos.  */
typedef struct {
  /* The number of the current element in IRA_ALLOCNOS.  */
  int n;
} ira_allocno_iterator;

/* Initialize the iterator I.  */
static inline void
ira_allocno_iter_init (ira_allocno_iterator *i)
{
  i->n = 0;
}

/* Return TRUE if we have more allocnos to visit, in which case *A is
   set to the allocno to be visited.  Otherwise, return FALSE.  */
static inline bool
ira_allocno_iter_cond (ira_allocno_iterator *i, ira_allocno_t *a)
{
  int n;

  for (n = i->n; n < ira_allocnos_num; n++)
    if (ira_allocnos[n] != NULL)
      {
	*a = ira_allocnos[n];
	i->n = n + 1;
	return true;
      }
  return false;
}

/* Loop over all allocnos.  In each iteration, A is set to the next
   allocno.  ITER is an instance of ira_allocno_iterator used to iterate
   the allocnos.  */
#define FOR_EACH_ALLOCNO(A, ITER)			\
  for (ira_allocno_iter_init (&(ITER));			\
       ira_allocno_iter_cond (&(ITER), &(A));)

/* The iterator for all objects.  */
typedef struct {
  /* The number of the current element in ira_object_id_map.  */
  int n;
} ira_object_iterator;

/* Initialize the iterator I.  */
static inline void
ira_object_iter_init (ira_object_iterator *i)
{
  i->n = 0;
}

/* Return TRUE if we have more objects to visit, in which case *OBJ is
   set to the object to be visited.  Otherwise, return FALSE.  */
static inline bool
ira_object_iter_cond (ira_object_iterator *i, ira_object_t *obj)
{
  int n;

  for (n = i->n; n < ira_objects_num; n++)
    if (ira_object_id_map[n] != NULL)
      {
	*obj = ira_object_id_map[n];
	i->n = n + 1;
	return true;
      }
  return false;
}

/* Loop over all objects.  In each iteration, OBJ is set to the next
   object.  ITER is an instance of ira_object_iterator used to iterate
   the objects.  */
#define FOR_EACH_OBJECT(OBJ, ITER)			\
  for (ira_object_iter_init (&(ITER));			\
       ira_object_iter_cond (&(ITER), &(OBJ));)

/* The iterator for objects associated with an allocno.  */
typedef struct {
  /* The number of the element the allocno's object array.  */
  int n;
} ira_allocno_object_iterator;

/* Initialize the iterator I.  */
static inline void
ira_allocno_object_iter_init (ira_allocno_object_iterator *i)
{
  i->n = 0;
}

/* Return TRUE if we have more objects to visit in allocno A, in which
   case *O is set to the object to be visited.  Otherwise, return
   FALSE.  */
static inline bool
ira_allocno_object_iter_cond (ira_allocno_object_iterator *i, ira_allocno_t a,
			      ira_object_t *o)
{
  *o = ALLOCNO_OBJECT (a, i->n);
  return i->n++ < ALLOCNO_NUM_OBJECTS (a);
}

/* Loop over all objects associated with allocno A.  In each
   iteration, O is set to the next object.  ITER is an instance of
   ira_allocno_object_iterator used to iterate the conflicts.  */
#define FOR_EACH_ALLOCNO_OBJECT(A, O, ITER)			\
  for (ira_allocno_object_iter_init (&(ITER));			\
       ira_allocno_object_iter_cond (&(ITER), (A), &(O));)


/* The iterator for copies.  */
typedef struct {
  /* The number of the current element in IRA_COPIES.  */
  int n;
} ira_copy_iterator;

/* Initialize the iterator I.  */
static inline void
ira_copy_iter_init (ira_copy_iterator *i)
{
  i->n = 0;
}

/* Return TRUE if we have more copies to visit, in which case *CP is
   set to the copy to be visited.  Otherwise, return FALSE.  */
static inline bool
ira_copy_iter_cond (ira_copy_iterator *i, ira_copy_t *cp)
{
  int n;

  for (n = i->n; n < ira_copies_num; n++)
    if (ira_copies[n] != NULL)
      {
	*cp = ira_copies[n];
	i->n = n + 1;
	return true;
      }
  return false;
}

/* Loop over all copies.  In each iteration, C is set to the next
   copy.  ITER is an instance of ira_copy_iterator used to iterate
   the copies.  */
#define FOR_EACH_COPY(C, ITER)				\
  for (ira_copy_iter_init (&(ITER));			\
       ira_copy_iter_cond (&(ITER), &(C));)

/* The iterator for object conflicts.  */
typedef struct {

  /* TRUE if the conflicts are represented by vector of allocnos.  */
  bool conflict_vec_p;

  /* The conflict vector or conflict bit vector.  */
  void *vec;

  /* The number of the current element in the vector (of type
     ira_object_t or IRA_INT_TYPE).  */
  unsigned int word_num;

  /* The bit vector size.  It is defined only if
     OBJECT_CONFLICT_VEC_P is FALSE.  */
  unsigned int size;

  /* The current bit index of bit vector.  It is defined only if
     OBJECT_CONFLICT_VEC_P is FALSE.  */
  unsigned int bit_num;

  /* The object id corresponding to the 1st bit of the bit vector.  It
     is defined only if OBJECT_CONFLICT_VEC_P is FALSE.  */
  int base_conflict_id;

  /* The word of bit vector currently visited.  It is defined only if
     OBJECT_CONFLICT_VEC_P is FALSE.  */
  unsigned IRA_INT_TYPE word;
} ira_object_conflict_iterator;

/* Initialize the iterator I with ALLOCNO conflicts.  */
static inline void
ira_object_conflict_iter_init (ira_object_conflict_iterator *i,
			       ira_object_t obj)
{
  i->conflict_vec_p = OBJECT_CONFLICT_VEC_P (obj);
  i->vec = OBJECT_CONFLICT_ARRAY (obj);
  i->word_num = 0;
  if (i->conflict_vec_p)
    i->size = i->bit_num = i->base_conflict_id = i->word = 0;
  else
    {
      if (OBJECT_MIN (obj) > OBJECT_MAX (obj))
	i->size = 0;
      else
	i->size = ((OBJECT_MAX (obj) - OBJECT_MIN (obj)
		    + IRA_INT_BITS)
		   / IRA_INT_BITS) * sizeof (IRA_INT_TYPE);
      i->bit_num = 0;
      i->base_conflict_id = OBJECT_MIN (obj);
      i->word = (i->size == 0 ? 0 : ((IRA_INT_TYPE *) i->vec)[0]);
    }
}

/* Return TRUE if we have more conflicting allocnos to visit, in which
   case *A is set to the allocno to be visited.  Otherwise, return
   FALSE.  */
static inline bool
ira_object_conflict_iter_cond (ira_object_conflict_iterator *i,
			       ira_object_t *pobj)
{
  ira_object_t obj;

  if (i->conflict_vec_p)
    {
      obj = ((ira_object_t *) i->vec)[i->word_num];
      if (obj == NULL)
	return false;
    }
  else
    {
      /* Skip words that are zeros.  */
      for (; i->word == 0; i->word = ((IRA_INT_TYPE *) i->vec)[i->word_num])
	{
	  i->word_num++;

	  /* If we have reached the end, break.  */
	  if (i->word_num * sizeof (IRA_INT_TYPE) >= i->size)
	    return false;

	  i->bit_num = i->word_num * IRA_INT_BITS;
	}

      /* Skip bits that are zero.  */
      for (; (i->word & 1) == 0; i->word >>= 1)
	i->bit_num++;

      obj = ira_object_id_map[i->bit_num + i->base_conflict_id];
    }

  *pobj = obj;
  return true;
}

/* Advance to the next conflicting allocno.  */
static inline void
ira_object_conflict_iter_next (ira_object_conflict_iterator *i)
{
  if (i->conflict_vec_p)
    i->word_num++;
  else
    {
      i->word >>= 1;
      i->bit_num++;
    }
}

/* Loop over all objects conflicting with OBJ.  In each iteration,
   CONF is set to the next conflicting object.  ITER is an instance
   of ira_object_conflict_iterator used to iterate the conflicts.  */
#define FOR_EACH_OBJECT_CONFLICT(OBJ, CONF, ITER)			\
  for (ira_object_conflict_iter_init (&(ITER), (OBJ));			\
       ira_object_conflict_iter_cond (&(ITER), &(CONF));		\
       ira_object_conflict_iter_next (&(ITER)))



/* The function returns TRUE if hard registers starting with
   HARD_REGNO and containing value of MODE are not in set
   HARD_REGSET.  */
static inline bool
ira_hard_reg_not_in_set_p (int hard_regno, enum machine_mode mode,
			   HARD_REG_SET hard_regset)
{
  int i;

  ira_assert (hard_regno >= 0);
  for (i = hard_regno_nregs[hard_regno][mode] - 1; i >= 0; i--)
    if (TEST_HARD_REG_BIT (hard_regset, hard_regno + i))
      return false;
  return true;
}



/* To save memory we use a lazy approach for allocation and
   initialization of the cost vectors.  We do this only when it is
   really necessary.  */

/* Allocate cost vector *VEC for hard registers of COVER_CLASS and
   initialize the elements by VAL if it is necessary */
static inline void
ira_allocate_and_set_costs (int **vec, enum reg_class cover_class, int val)
{
  int i, *reg_costs;
  int len;

  if (*vec != NULL)
    return;
  *vec = reg_costs = ira_allocate_cost_vector (cover_class);
  len = ira_class_hard_regs_num[cover_class];
  for (i = 0; i < len; i++)
    reg_costs[i] = val;
}

/* Allocate cost vector *VEC for hard registers of COVER_CLASS and
   copy values of vector SRC into the vector if it is necessary */
static inline void
ira_allocate_and_copy_costs (int **vec, enum reg_class cover_class, int *src)
{
  int len;

  if (*vec != NULL || src == NULL)
    return;
  *vec = ira_allocate_cost_vector (cover_class);
  len = ira_class_hard_regs_num[cover_class];
  memcpy (*vec, src, sizeof (int) * len);
}

/* Allocate cost vector *VEC for hard registers of COVER_CLASS and
   add values of vector SRC into the vector if it is necessary */
static inline void
ira_allocate_and_accumulate_costs (int **vec, enum reg_class cover_class,
				   int *src)
{
  int i, len;

  if (src == NULL)
    return;
  len = ira_class_hard_regs_num[cover_class];
  if (*vec == NULL)
    {
      *vec = ira_allocate_cost_vector (cover_class);
      memset (*vec, 0, sizeof (int) * len);
    }
  for (i = 0; i < len; i++)
    (*vec)[i] += src[i];
}

/* Allocate cost vector *VEC for hard registers of COVER_CLASS and
   copy values of vector SRC into the vector or initialize it by VAL
   (if SRC is null).  */
static inline void
ira_allocate_and_set_or_copy_costs (int **vec, enum reg_class cover_class,
				    int val, int *src)
{
  int i, *reg_costs;
  int len;

  if (*vec != NULL)
    return;
  *vec = reg_costs = ira_allocate_cost_vector (cover_class);
  len = ira_class_hard_regs_num[cover_class];
  if (src != NULL)
    memcpy (reg_costs, src, sizeof (int) * len);
  else
    {
      for (i = 0; i < len; i++)
	reg_costs[i] = val;
    }
}
