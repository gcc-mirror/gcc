/* Register conflict graph computation routines.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC

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

/* References:

   Building an Optimizing Compiler
   Robert Morgan
   Butterworth-Heinemann, 1998 */

#include "config.h"
#include "system.h"
#include "obstack.h"
#include "hashtab.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "basic-block.h"

/* Use malloc to allocate obstack chunks.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* A register conflict graph is an undirected graph containing nodes
   for some or all of the regs used in a function.  Arcs represent
   conflicts, i.e. two nodes are connected by an arc if there is a
   point in the function at which the regs corresponding to the two
   nodes are both live.

   The conflict graph is represented by the data structures described
   in Morgan section 11.3.1.  Nodes are not stored explicitly; only
   arcs are.  An arc stores the numbers of the regs it connects.

   Arcs can be located by two methods:

     - The two reg numbers for each arc are hashed into a single
       value, and the arc is placed in a hash table according to this
       value.  This permits quick determination of whether a specific
       conflict is present in the graph.  

     - Additionally, the arc data structures are threaded by a set of
       linked lists by single reg number.  Since each arc references
       two regs, there are two next pointers, one for the
       smaller-numbered reg and one for the larger-numbered reg.  This
       permits the quick enumeration of conflicts for a single
       register.

   Arcs are allocated from an obstack.  */

/* An arc in a conflict graph.  */

struct conflict_graph_arc_def
{
  /* The next element of the list of conflicts involving the
     smaller-numbered reg, as an index in the table of arcs of this
     graph.   Contains NULL if this is the tail.  */
  struct conflict_graph_arc_def *smaller_next;

  /* The next element of the list of conflicts involving the
     larger-numbered reg, as an index in the table of arcs of this
     graph.  Contains NULL if this is the tail.  */
  struct conflict_graph_arc_def *larger_next;

  /* The smaller-numbered reg involved in this conflict.  */
  int smaller;

  /* The larger-numbered reg involved in this conflict.  */
  int larger;
};

typedef struct conflict_graph_arc_def *conflict_graph_arc;
typedef const struct conflict_graph_arc_def *const_conflict_graph_arc;


/* A conflict graph.  */

struct conflict_graph_def
{
  /* A hash table of arcs.  Used to search for a specific conflict.  */
  htab_t arc_hash_table;

  /* The number of regs this conflict graph handles.  */
  int num_regs;

  /* For each reg, the arc at the head of a list that threads through
     all the arcs involving that reg.  An entry is NULL if no
     conflicts exist involving that reg.  */
  conflict_graph_arc *neighbor_heads;

  /* Arcs are allocated from here.  */
  struct obstack arc_obstack;
};

/* The initial capacity (number of conflict arcs) for newly-created
   conflict graphs.  */
#define INITIAL_ARC_CAPACITY 64


/* Computes the hash value of the conflict graph arc connecting regs
   R1 and R2.  R1 is assumed to be smaller or equal to R2.  */
#define CONFLICT_HASH_FN(R1, R2) ((R2) * ((R2) - 1) / 2 + (R1))

static unsigned arc_hash	PARAMS ((const void *));
static int arc_eq		PARAMS ((const void *, const void *));
static int print_conflict	PARAMS ((int, int, void *));
static void mark_reg		PARAMS ((rtx, rtx, void *));

/* Callback function to compute the hash value of an arc.  Uses
   current_graph to locate the graph to which the arc belongs.  */

static unsigned
arc_hash (arcp)
     const void *arcp;
{
  const_conflict_graph_arc arc = (const_conflict_graph_arc) arcp;

  return CONFLICT_HASH_FN (arc->smaller, arc->larger);
}

/* Callback function to determine the equality of two arcs in the hash
   table.  */

static int
arc_eq (arcp1, arcp2)
     const void *arcp1;
     const void *arcp2;
{
  const_conflict_graph_arc arc1 = (const_conflict_graph_arc) arcp1;
  const_conflict_graph_arc arc2 = (const_conflict_graph_arc) arcp2;

  return arc1->smaller == arc2->smaller && arc1->larger == arc2->larger;
}

/* Creates an empty conflict graph to hold conflicts among NUM_REGS
   registers.  */

conflict_graph
conflict_graph_new (num_regs)
     int num_regs;
{
  conflict_graph graph
    = (conflict_graph) xmalloc (sizeof (struct conflict_graph_def));
  graph->num_regs = num_regs;

  /* Set up the hash table.  No delete action is specified; memory
     management of arcs is through the obstack.  */
  graph->arc_hash_table
    = htab_create (INITIAL_ARC_CAPACITY, &arc_hash, &arc_eq, NULL);

  /* Create an obstack for allocating arcs.  */
  obstack_init (&graph->arc_obstack);
	     
  /* Create and zero the lookup table by register number.  */
  graph->neighbor_heads
    = (conflict_graph_arc *) xmalloc (num_regs * sizeof (conflict_graph_arc));

  memset (graph->neighbor_heads, 0, num_regs * sizeof (conflict_graph_arc));
  return graph;
}

/* Deletes a conflict graph.  */

void
conflict_graph_delete (graph)
     conflict_graph graph;
{
  obstack_free (&graph->arc_obstack, NULL);
  htab_delete (graph->arc_hash_table);
  free (graph->neighbor_heads);
  free (graph);
}

/* Adds a conflict to GRAPH between regs REG1 and REG2, which must be
   distinct.  Returns non-zero, unless the conflict is already present
   in GRAPH, in which case it does nothing and returns zero.  */

int
conflict_graph_add (graph, reg1, reg2)
     conflict_graph graph;
     int reg1;
     int reg2;
{
  int smaller = MIN (reg1, reg2);
  int larger = MAX (reg1, reg2);
  struct conflict_graph_arc_def dummy;
  conflict_graph_arc arc;
  void **slot;

  /* A reg cannot conflict with itself.  */
  if (reg1 == reg2)
    abort ();

  dummy.smaller = smaller;
  dummy.larger = larger;
  slot = htab_find_slot (graph->arc_hash_table, (void *) &dummy, INSERT);
  
  /* If the conflict is already there, do nothing.  */
  if (*slot != NULL)
    return 0;

  /* Allocate an arc.  */
  arc
    = (conflict_graph_arc)
      obstack_alloc (&graph->arc_obstack,
		     sizeof (struct conflict_graph_arc_def));
  
  /* Record the reg numbers.  */
  arc->smaller = smaller;
  arc->larger = larger;

  /* Link the conflict into into two lists, one for each reg.  */
  arc->smaller_next = graph->neighbor_heads[smaller];
  graph->neighbor_heads[smaller] = arc;
  arc->larger_next = graph->neighbor_heads[larger];
  graph->neighbor_heads[larger] = arc;

  /* Put it in the hash table.  */
  *slot = (void *) arc;

  return 1;
}

/* Returns non-zero if a conflict exists in GRAPH between regs REG1
   and REG2.  */

int
conflict_graph_conflict_p (graph, reg1, reg2)
     conflict_graph graph;
     int reg1;
     int reg2;
{
  /* Build an arc to search for.  */
  struct conflict_graph_arc_def arc;
  arc.smaller = MIN (reg1, reg2);
  arc.larger = MAX (reg1, reg2);

  return htab_find (graph->arc_hash_table, (void *) &arc) != NULL;
}

/* Calls ENUM_FN for each conflict in GRAPH involving REG.  EXTRA is
   passed back to ENUM_FN.  */

void
conflict_graph_enum (graph, reg, enum_fn, extra)
     conflict_graph graph;
     int reg;
     conflict_graph_enum_fn enum_fn;
     void *extra;
{
  conflict_graph_arc arc = graph->neighbor_heads[reg];
  while (arc != NULL)
    {
      /* Invoke the callback.  */
      if ((*enum_fn) (arc->smaller, arc->larger, extra))
	/* Stop if requested.  */
	break;
      
      /* Which next pointer to follow depends on whether REG is the
	 smaller or larger reg in this conflict.  */
      if (reg < arc->larger)
	arc = arc->smaller_next;
      else
	arc = arc->larger_next;
    }
}

/* For each conflict between a register x and SRC in GRAPH, adds a
   conflict to GRAPH between x and TARGET.  */

void
conflict_graph_merge_regs (graph, target, src)
     conflict_graph graph;
     int target;
     int src;
{
  conflict_graph_arc arc = graph->neighbor_heads[src];

  if (target == src)
    return;

  while (arc != NULL)
    {
      int other = arc->smaller;

      if (other == src)
	other = arc->larger;

      conflict_graph_add (graph, target, other);

      /* Which next pointer to follow depends on whether REG is the
	 smaller or larger reg in this conflict.  */
      if (src < arc->larger)
	arc = arc->smaller_next;
      else
	arc = arc->larger_next;
    }
}

/* Holds context information while a conflict graph is being traversed
   for printing.  */

struct print_context
{
  /* The file pointer to which we're printing.  */
  FILE *fp;

  /* The reg whose conflicts we're printing.  */
  int reg;

  /* Whether a conflict has already been printed for this reg.  */
  int started;
};

/* Callback function when enumerating conflicts during printing.  */

static int
print_conflict (reg1, reg2, contextp)
     int reg1;
     int reg2;
     void *contextp;
{
  struct print_context *context = (struct print_context *) contextp;
  int reg;

  /* If this is the first conflict printed for this reg, start a new
     line.  */
  if (! context->started)
    {
      fprintf (context->fp, " %d:", context->reg);
      context->started = 1;
    }

  /* Figure out the reg whose conflicts we're printing.  The other reg
     is the interesting one.  */
  if (reg1 == context->reg)
    reg = reg2;
  else if (reg2 == context->reg)
    reg = reg1;
  else
    abort ();

  /* Print the conflict.  */
  fprintf (context->fp, " %d", reg);

  /* Continue enumerating.  */
  return 0;
}

/* Prints the conflicts in GRAPH to FP.  */

void
conflict_graph_print (graph, fp)
     conflict_graph graph;
     FILE *fp;
{
  int reg;
  struct print_context context;

  context.fp = fp;
  fprintf (fp, "Conflicts:\n");

  /* Loop over registers supported in this graph.  */
  for (reg = 0; reg < graph->num_regs; ++reg)
    {
      context.reg = reg;
      context.started = 0;

      /* Scan the conflicts for reg, printing as we go.  A label for
	 this line will be printed the first time a conflict is
	 printed for the reg; we won't start a new line if this reg
	 has no conflicts.  */
      conflict_graph_enum (graph, reg, &print_conflict, &context);

      /* If this reg does have conflicts, end the line.  */
      if (context.started)
	fputc ('\n', fp);
    }
}

/* Callback function for note_stores.  */

static void
mark_reg (reg, setter, data)
     rtx reg;
     rtx setter ATTRIBUTE_UNUSED;
     void *data;
{
  regset set = (regset) data;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  /* We're only interested in regs.  */
  if (GET_CODE (reg) != REG)
    return;

  SET_REGNO_REG_SET (set, REGNO (reg));
}

/* Allocates a conflict graph and computes conflicts over the current
   function for the registers set in REGS.  The caller is responsible
   for deallocating the return value.  

   Preconditions: the flow graph must be in SSA form, and life
   analysis (specifically, regs live at exit from each block) must be
   up-to-date.  

   This algorithm determines conflicts by walking the insns in each
   block backwards.  We maintain the set of live regs at each insn,
   starting with the regs live on exit from the block.  For each insn:
 
     1. If a reg is set in this insns, it must be born here, since
        we're in SSA.  Therefore, it was not live before this insns,
	so remove it from the set of live regs.  

     2. For each reg born in this insn, record a conflict between it
	and every other reg live coming into this insn.  For each
	existing conflict, one of the two regs must be born while the
	other is alive.  See Morgan or elsewhere for a proof of this.

     3. Regs clobbered by this insn must have been live coming into
        it, so record them as such.  

   The resulting conflict graph is not built for regs in REGS
   themselves; rather, partition P is used to obtain the canonical reg
   for each of these.  The nodes of the conflict graph are these
   canonical regs instead.  */

conflict_graph
conflict_graph_compute (regs, p)
     regset regs;
     partition p;
{
  int b;
  conflict_graph graph = conflict_graph_new (max_reg_num ());
  regset_head live_head;
  regset live = &live_head;
  regset_head born_head;
  regset born = &born_head;

  INIT_REG_SET (live);
  INIT_REG_SET (born);

  for (b = n_basic_blocks; --b >= 0; )
    {
      basic_block bb = BASIC_BLOCK (b);
      rtx insn;
      rtx head;

      /* Start with the regs that are live on exit, limited to those
	 we're interested in.  */
      COPY_REG_SET (live, bb->global_live_at_end);
      AND_REG_SET (live, regs);

      /* Walk the instruction stream backwards.  */
      head = bb->head;
      insn = bb->end;
      for (insn = bb->end; insn != head; insn = PREV_INSN (insn))
	{
	  int born_reg;
	  int live_reg;
	  rtx link;

	  /* Are we interested in this insn? */
	  if (INSN_P (insn))
	    {
	      /* Determine which regs are set in this insn.  Since
  	         we're in SSA form, if a reg is set here it isn't set
  	         anywhere else, so this insn is where the reg is born.  */
	      CLEAR_REG_SET (born);
	      note_stores (PATTERN (insn), mark_reg, born);
	      AND_REG_SET (born, regs);
	  
	      /* Regs born here were not live before this insn.  */
	      AND_COMPL_REG_SET (live, born);

	      /* For every reg born here, add a conflict with every other
  	         reg live coming into this insn.  */
	      EXECUTE_IF_SET_IN_REG_SET
		(born, FIRST_PSEUDO_REGISTER, born_reg,
		 {
		   EXECUTE_IF_SET_IN_REG_SET
		     (live, FIRST_PSEUDO_REGISTER, live_reg,
		      {
			/* Build the conflict graph in terms of canonical
			   regnos.  */
			int b = partition_find (p, born_reg);
			int l = partition_find (p, live_reg);

			if (b != l)
			  conflict_graph_add (graph, b, l);
		      });
		 });

	      /* Morgan's algorithm checks the operands of the insn
	         and adds them to the set of live regs.  Instead, we
	         use death information added by life analysis.  Regs
	         dead after this instruction were live before it.  */
	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD)
		  {
		    unsigned int regno = REGNO (XEXP (link, 0));

		    if (REGNO_REG_SET_P (regs, regno))
		      SET_REGNO_REG_SET (live, regno);
		  }
	    }
	}
    }

  FREE_REG_SET (live);
  FREE_REG_SET (born);

  return graph;
}
