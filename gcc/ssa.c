/* Static Single Assignment conversion routines for the GNU compiler.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

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
   Butterworth-Heinemann, 1998

   Static Single Assignment Construction
   Preston Briggs, Tim Harvey, Taylor Simpson
   Technical Report, Rice University, 1995
   ftp://ftp.cs.rice.edu/public/preston/optimizer/SSA.ps.gz.  */

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "expr.h"
#include "varray.h"
#include "partition.h"
#include "sbitmap.h"
#include "hashtab.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "function.h"
#include "real.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "ssa.h"

/* TODO: 

   Handle subregs better, maybe.  For now, if a reg that's set in a
   subreg expression is duplicated going into SSA form, an extra copy
   is inserted first that copies the entire reg into the duplicate, so
   that the other bits are preserved.  This isn't strictly SSA, since
   at least part of the reg is assigned in more than one place (though
   they are adjacent).

   ??? What to do about strict_low_part.  Probably I'll have to split
   them out of their current instructions first thing.

   Actually the best solution may be to have a kind of "mid-level rtl"
   in which the RTL encodes exactly what we want, without exposing a
   lot of niggling processor details.  At some later point we lower
   the representation, calling back into optabs to finish any necessary
   expansion.  */

/* All pseudo-registers and select hard registers are converted to SSA
   form.  When converting out of SSA, these select hard registers are
   guaranteed to be mapped to their original register number.  Each
   machine's .h file should define CONVERT_HARD_REGISTER_TO_SSA_P
   indicating which hard registers should be converted.

   When converting out of SSA, temporaries for all registers are
   partitioned.  The partition is checked to ensure that all uses of
   the same hard register in the same machine mode are in the same
   class.  */

/* If conservative_reg_partition is non-zero, use a conservative
   register partitioning algorithm (which leaves more regs after
   emerging from SSA) instead of the coalescing one.  This is being
   left in for a limited time only, as a debugging tool until the
   coalescing algorithm is validated.  */

static int conservative_reg_partition;

/* This flag is set when the CFG is in SSA form.  */
int in_ssa_form = 0;

/* Element I is the single instruction that sets register I.  */
varray_type ssa_definition;

/* Element I-PSEUDO is the normal register that originated the ssa
   register in question.  */
varray_type ssa_rename_from;

/* Element I is the normal register that originated the ssa
   register in question.

   A hash table stores the (register, rtl) pairs.  These are each
   xmalloc'ed and deleted when the hash table is destroyed.  */
htab_t ssa_rename_from_ht;

/* The running target ssa register for a given pseudo register.
   (Pseudo registers appear in only one mode.)  */
static rtx *ssa_rename_to_pseudo;
/* Similar, but for hard registers.  A hard register can appear in
   many modes, so we store an equivalent pseudo for each of the
   modes.  */
static rtx ssa_rename_to_hard[FIRST_PSEUDO_REGISTER][NUM_MACHINE_MODES];

/* ssa_rename_from maps pseudo registers to the original corresponding
   RTL.  It is implemented as using a hash table.  */

typedef struct {
  unsigned int reg;
  rtx original;
} ssa_rename_from_pair;

struct ssa_rename_from_hash_table_data {
  sbitmap canonical_elements;
  partition reg_partition;
};

static void ssa_rename_from_initialize
  PARAMS ((void));
static rtx ssa_rename_from_lookup
  PARAMS ((int reg));
static unsigned int original_register
  PARAMS ((unsigned int regno));
static void ssa_rename_from_insert
  PARAMS ((unsigned int reg, rtx r));
static void ssa_rename_from_free
  PARAMS ((void));
typedef int (*srf_trav) PARAMS ((int regno, rtx r, sbitmap canonical_elements, partition reg_partition));
static void ssa_rename_from_traverse
  PARAMS ((htab_trav callback_function, sbitmap canonical_elements, partition reg_partition));
/*static Avoid warnign message.  */ void ssa_rename_from_print
  PARAMS ((void));
static int ssa_rename_from_print_1
  PARAMS ((void **slot, void *data));
static hashval_t ssa_rename_from_hash_function
  PARAMS ((const void * srfp));
static int ssa_rename_from_equal
  PARAMS ((const void *srfp1, const void *srfp2));
static void ssa_rename_from_delete
  PARAMS ((void *srfp));

static rtx ssa_rename_to_lookup
  PARAMS ((rtx reg));
static void ssa_rename_to_insert
  PARAMS ((rtx reg, rtx r));

/* The number of registers that were live on entry to the SSA routines.  */
static unsigned int ssa_max_reg_num;

/* Local function prototypes.  */

struct rename_context;

static inline rtx * phi_alternative
  PARAMS ((rtx, int));
static void compute_dominance_frontiers_1
  PARAMS ((sbitmap *frontiers, int *idom, int bb, sbitmap done));
static void find_evaluations_1
  PARAMS ((rtx dest, rtx set, void *data));
static void find_evaluations
  PARAMS ((sbitmap *evals, int nregs));
static void compute_iterated_dominance_frontiers
  PARAMS ((sbitmap *idfs, sbitmap *frontiers, sbitmap *evals, int nregs));
static void insert_phi_node
  PARAMS ((int regno, int b));
static void insert_phi_nodes
  PARAMS ((sbitmap *idfs, sbitmap *evals, int nregs));
static void create_delayed_rename 
  PARAMS ((struct rename_context *, rtx *));
static void apply_delayed_renames 
  PARAMS ((struct rename_context *));
static int rename_insn_1 
  PARAMS ((rtx *ptr, void *data));
static void rename_block 
  PARAMS ((int b, int *idom));
static void rename_registers 
  PARAMS ((int nregs, int *idom));

static inline int ephi_add_node
  PARAMS ((rtx reg, rtx *nodes, int *n_nodes));
static int * ephi_forward
  PARAMS ((int t, sbitmap visited, sbitmap *succ, int *tstack));
static void ephi_backward
  PARAMS ((int t, sbitmap visited, sbitmap *pred, rtx *nodes));
static void ephi_create
  PARAMS ((int t, sbitmap visited, sbitmap *pred, sbitmap *succ, rtx *nodes));
static void eliminate_phi
  PARAMS ((edge e, partition reg_partition));
static int make_regs_equivalent_over_bad_edges 
  PARAMS ((int bb, partition reg_partition));

/* These are used only in the conservative register partitioning
   algorithms.  */
static int make_equivalent_phi_alternatives_equivalent 
  PARAMS ((int bb, partition reg_partition));
static partition compute_conservative_reg_partition 
  PARAMS ((void));
static int record_canonical_element_1
  PARAMS ((void **srfp, void *data));
static int check_hard_regs_in_partition
  PARAMS ((partition reg_partition));
static int rename_equivalent_regs_in_insn 
  PARAMS ((rtx *ptr, void *data));

/* These are used in the register coalescing algorithm.  */
static int coalesce_if_unconflicting
  PARAMS ((partition p, conflict_graph conflicts, int reg1, int reg2));
static int coalesce_regs_in_copies
  PARAMS ((basic_block bb, partition p, conflict_graph conflicts));
static int coalesce_reg_in_phi
  PARAMS ((rtx, int dest_regno, int src_regno, void *data));
static int coalesce_regs_in_successor_phi_nodes
  PARAMS ((basic_block bb, partition p, conflict_graph conflicts));
static partition compute_coalesced_reg_partition
  PARAMS ((void));
static int mark_reg_in_phi 
  PARAMS ((rtx *ptr, void *data));
static void mark_phi_and_copy_regs
  PARAMS ((regset phi_set));

static int rename_equivalent_regs_in_insn 
  PARAMS ((rtx *ptr, void *data));
static void rename_equivalent_regs 
  PARAMS ((partition reg_partition));

/* Deal with hard registers.  */
static int conflicting_hard_regs_p
  PARAMS ((int reg1, int reg2));

/* ssa_rename_to maps registers and machine modes to SSA pseudo registers.  */

/* Find the register associated with REG in the indicated mode.  */

static rtx
ssa_rename_to_lookup (reg)
     rtx reg;
{
  if (!HARD_REGISTER_P (reg))
    return ssa_rename_to_pseudo[REGNO (reg) - FIRST_PSEUDO_REGISTER];
  else
    return ssa_rename_to_hard[REGNO (reg)][GET_MODE (reg)];
}

/* Store a new value mapping REG to R in ssa_rename_to.  */

static void
ssa_rename_to_insert(reg, r)
     rtx reg;
     rtx r;
{
  if (!HARD_REGISTER_P (reg))
    ssa_rename_to_pseudo[REGNO (reg) - FIRST_PSEUDO_REGISTER] = r;
  else
    ssa_rename_to_hard[REGNO (reg)][GET_MODE (reg)] = r;
}

/* Prepare ssa_rename_from for use.  */

static void
ssa_rename_from_initialize ()
{
  /* We use an arbitrary initial hash table size of 64.  */
  ssa_rename_from_ht = htab_create (64,
				    &ssa_rename_from_hash_function,
				    &ssa_rename_from_equal,
				    &ssa_rename_from_delete);
}

/* Find the REG entry in ssa_rename_from.  Return NULL_RTX if no entry is
   found.  */

static rtx
ssa_rename_from_lookup (reg)
     int reg;
{
  ssa_rename_from_pair srfp;
  ssa_rename_from_pair *answer;
  srfp.reg = reg;
  srfp.original = NULL_RTX;
  answer = (ssa_rename_from_pair *)
    htab_find_with_hash (ssa_rename_from_ht, (void *) &srfp, reg);
  return (answer == 0 ? NULL_RTX : answer->original);
}

/* Find the number of the original register specified by REGNO.  If
   the register is a pseudo, return the original register's number.
   Otherwise, return this register number REGNO.  */

static unsigned int
original_register (regno)
     unsigned int regno;
{
  rtx original_rtx = ssa_rename_from_lookup (regno);
  return original_rtx != NULL_RTX ? REGNO (original_rtx) : regno;
}

/* Add mapping from R to REG to ssa_rename_from even if already present.  */

static void
ssa_rename_from_insert (reg, r)
     unsigned int reg;
     rtx r;
{
  void **slot;
  ssa_rename_from_pair *srfp = xmalloc (sizeof (ssa_rename_from_pair));
  srfp->reg = reg;
  srfp->original = r;
  slot = htab_find_slot_with_hash (ssa_rename_from_ht, (const void *) srfp,
				   reg, INSERT);
  if (*slot != 0)
    free ((void *) *slot);
  *slot = srfp;
}

/* Apply the CALLBACK_FUNCTION to each element in ssa_rename_from.
   CANONICAL_ELEMENTS and REG_PARTITION pass data needed by the only
   current use of this function.  */

static void
ssa_rename_from_traverse (callback_function,
			  canonical_elements, reg_partition)
     htab_trav callback_function;
     sbitmap canonical_elements;
     partition reg_partition;
{
  struct ssa_rename_from_hash_table_data srfhd;
  srfhd.canonical_elements = canonical_elements;
  srfhd.reg_partition = reg_partition;
  htab_traverse (ssa_rename_from_ht, callback_function, (void *) &srfhd);
}

/* Destroy ssa_rename_from.  */

static void
ssa_rename_from_free ()
{
  htab_delete (ssa_rename_from_ht);
}

/* Print the contents of ssa_rename_from.  */

/* static  Avoid erroneous error message.  */
void
ssa_rename_from_print ()
{
  printf ("ssa_rename_from's hash table contents:\n");
  htab_traverse (ssa_rename_from_ht, &ssa_rename_from_print_1, NULL);
}

/* Print the contents of the hash table entry SLOT, passing the unused
   sttribute DATA.  Used as a callback function with htab_traverse ().  */

static int
ssa_rename_from_print_1 (slot, data)
     void **slot;
     void *data ATTRIBUTE_UNUSED;
{
  ssa_rename_from_pair * p = *slot;
  printf ("ssa_rename_from maps pseudo %i to original %i.\n",
	  p->reg, REGNO (p->original));
  return 1;
}

/* Given a hash entry SRFP, yield a hash value.  */

static hashval_t
ssa_rename_from_hash_function (srfp)
     const void *srfp;
{
  return ((const ssa_rename_from_pair *) srfp)->reg;
}

/* Test whether two hash table entries SRFP1 and SRFP2 are equal.  */

static int
ssa_rename_from_equal (srfp1, srfp2)
     const void *srfp1;
     const void *srfp2;
{
  return ssa_rename_from_hash_function (srfp1) ==
    ssa_rename_from_hash_function (srfp2);
}

/* Delete the hash table entry SRFP.  */

static void
ssa_rename_from_delete (srfp)
     void *srfp;
{
  free (srfp);
}

/* Given the SET of a PHI node, return the address of the alternative
   for predecessor block C.  */

static inline rtx *
phi_alternative (set, c)
     rtx set;
     int c;
{
  rtvec phi_vec = XVEC (SET_SRC (set), 0);
  int v;

  for (v = GET_NUM_ELEM (phi_vec) - 2; v >= 0; v -= 2)
    if (INTVAL (RTVEC_ELT (phi_vec, v + 1)) == c)
      return &RTVEC_ELT (phi_vec, v);

  return NULL;
}

/* Given the SET of a phi node, remove the alternative for predecessor
   block C.  Return non-zero on success, or zero if no alternative is
   found for C.  */

int
remove_phi_alternative (set, block)
     rtx set;
     basic_block block;
{
  rtvec phi_vec = XVEC (SET_SRC (set), 0);
  int num_elem = GET_NUM_ELEM (phi_vec);
  int v, c;

  c = block->index;
  for (v = num_elem - 2; v >= 0; v -= 2)
    if (INTVAL (RTVEC_ELT (phi_vec, v + 1)) == c)
      {
	if (v < num_elem - 2)
	  {
	    RTVEC_ELT (phi_vec, v) = RTVEC_ELT (phi_vec, num_elem - 2);
	    RTVEC_ELT (phi_vec, v + 1) = RTVEC_ELT (phi_vec, num_elem - 1);
	  }
	PUT_NUM_ELEM (phi_vec, num_elem - 2);
	return 1;
      }

  return 0;
}

/* For all registers, find all blocks in which they are set.

   This is the transform of what would be local kill information that
   we ought to be getting from flow.  */

static sbitmap *fe_evals;
static int fe_current_bb;

static void
find_evaluations_1 (dest, set, data)
     rtx dest;
     rtx set ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  if (GET_CODE (dest) == REG
      && CONVERT_REGISTER_TO_SSA_P (REGNO (dest)))
    SET_BIT (fe_evals[REGNO (dest)], fe_current_bb);
}

static void
find_evaluations (evals, nregs)
     sbitmap *evals;
     int nregs;
{
  int bb;

  sbitmap_vector_zero (evals, nregs);
  fe_evals = evals;

  for (bb = n_basic_blocks; --bb >= 0; )
    {
      rtx p, last;

      fe_current_bb = bb;
      p = BLOCK_HEAD (bb);
      last = BLOCK_END (bb);
      while (1)
	{
	  if (INSN_P (p))
	    note_stores (PATTERN (p), find_evaluations_1, NULL);

	  if (p == last)
	    break;
	  p = NEXT_INSN (p);
	}
    }
}

/* Computing the Dominance Frontier:
  
   As decribed in Morgan, section 3.5, this may be done simply by 
   walking the dominator tree bottom-up, computing the frontier for
   the children before the parent.  When considering a block B,
   there are two cases:

   (1) A flow graph edge leaving B that does not lead to a child
   of B in the dominator tree must be a block that is either equal
   to B or not dominated by B.  Such blocks belong in the frontier
   of B.

   (2) Consider a block X in the frontier of one of the children C
   of B.  If X is not equal to B and is not dominated by B, it
   is in the frontier of B.
*/

static void
compute_dominance_frontiers_1 (frontiers, idom, bb, done)
     sbitmap *frontiers;
     int *idom;
     int bb;
     sbitmap done;
{
  basic_block b = BASIC_BLOCK (bb);
  edge e;
  int c;

  SET_BIT (done, bb);
  sbitmap_zero (frontiers[bb]);

  /* Do the frontier of the children first.  Not all children in the
     dominator tree (blocks dominated by this one) are children in the
     CFG, so check all blocks.  */
  for (c = 0; c < n_basic_blocks; ++c)
    if (idom[c] == bb && ! TEST_BIT (done, c))
      compute_dominance_frontiers_1 (frontiers, idom, c, done);

  /* Find blocks conforming to rule (1) above.  */
  for (e = b->succ; e; e = e->succ_next)
    {
      if (e->dest == EXIT_BLOCK_PTR)
	continue;
      if (idom[e->dest->index] != bb)
	SET_BIT (frontiers[bb], e->dest->index);
    }

  /* Find blocks conforming to rule (2).  */
  for (c = 0; c < n_basic_blocks; ++c)
    if (idom[c] == bb)
      {
	int x;
	EXECUTE_IF_SET_IN_SBITMAP (frontiers[c], 0, x,
	  {
	    if (idom[x] != bb)
	      SET_BIT (frontiers[bb], x);
	  });
      }
}

void
compute_dominance_frontiers (frontiers, idom)
     sbitmap *frontiers;
     int *idom;
{
  sbitmap done = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (done);

  compute_dominance_frontiers_1 (frontiers, idom, 0, done);

  sbitmap_free (done);
}

/* Computing the Iterated Dominance Frontier:

   This is the set of merge points for a given register.

   This is not particularly intuitive.  See section 7.1 of Morgan, in
   particular figures 7.3 and 7.4 and the immediately surrounding text.
*/

static void
compute_iterated_dominance_frontiers (idfs, frontiers, evals, nregs)
     sbitmap *idfs;
     sbitmap *frontiers;
     sbitmap *evals;
     int nregs;
{
  sbitmap worklist;
  int reg, passes = 0;

  worklist = sbitmap_alloc (n_basic_blocks);

  for (reg = 0; reg < nregs; ++reg)
    {
      sbitmap idf = idfs[reg];
      int b, changed;

      /* Start the iterative process by considering those blocks that
	 evaluate REG.  We'll add their dominance frontiers to the
	 IDF, and then consider the blocks we just added.  */
      sbitmap_copy (worklist, evals[reg]);

      /* Morgan's algorithm is incorrect here.  Blocks that evaluate
	 REG aren't necessarily in REG's IDF.  Start with an empty IDF.  */
      sbitmap_zero (idf);

      /* Iterate until the worklist is empty.  */
      do
	{
	  changed = 0;
	  passes++;
	  EXECUTE_IF_SET_IN_SBITMAP (worklist, 0, b,
	    {
	      RESET_BIT (worklist, b);
	      /* For each block on the worklist, add to the IDF all
		 blocks on its dominance frontier that aren't already
		 on the IDF.  Every block that's added is also added
		 to the worklist.  */
	      sbitmap_union_of_diff (worklist, worklist, frontiers[b], idf);
	      sbitmap_a_or_b (idf, idf, frontiers[b]);
	      changed = 1;
	    });
	}
      while (changed);
    }

  sbitmap_free (worklist);

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file,
	       "Iterated dominance frontier: %d passes on %d regs.\n",
	       passes, nregs);
    }
}

/* Insert the phi nodes.  */

static void
insert_phi_node (regno, bb)
     int regno, bb;
{
  basic_block b = BASIC_BLOCK (bb);
  edge e;
  int npred, i;
  rtvec vec;
  rtx phi, reg;
  rtx insn;
  int end_p;

  /* Find out how many predecessors there are.  */
  for (e = b->pred, npred = 0; e; e = e->pred_next)
    if (e->src != ENTRY_BLOCK_PTR)
      npred++;

  /* If this block has no "interesting" preds, then there is nothing to
     do.  Consider a block that only has the entry block as a pred.  */
  if (npred == 0)
    return;

  /* This is the register to which the phi function will be assigned.  */
  reg = regno_reg_rtx[regno];

  /* Construct the arguments to the PHI node.  The use of pc_rtx is just
     a placeholder; we'll insert the proper value in rename_registers.  */
  vec = rtvec_alloc (npred * 2);
  for (e = b->pred, i = 0; e ; e = e->pred_next, i += 2)
    if (e->src != ENTRY_BLOCK_PTR)
      {
	RTVEC_ELT (vec, i + 0) = pc_rtx;
	RTVEC_ELT (vec, i + 1) = GEN_INT (e->src->index);
      }

  phi = gen_rtx_PHI (VOIDmode, vec);
  phi = gen_rtx_SET (VOIDmode, reg, phi);

  insn = first_insn_after_basic_block_note (b);
  end_p = PREV_INSN (insn) == b->end;
  emit_insn_before (phi, insn);
  if (end_p)
    b->end = PREV_INSN (insn);
}

static void
insert_phi_nodes (idfs, evals, nregs)
     sbitmap *idfs;
     sbitmap *evals ATTRIBUTE_UNUSED;
     int nregs;
{
  int reg;

  for (reg = 0; reg < nregs; ++reg)
    if (CONVERT_REGISTER_TO_SSA_P (reg))
    {
      int b;
      EXECUTE_IF_SET_IN_SBITMAP (idfs[reg], 0, b,
	{
	  if (REGNO_REG_SET_P (BASIC_BLOCK (b)->global_live_at_start, reg))
	    insert_phi_node (reg, b);
	});
    }
}

/* Rename the registers to conform to SSA. 

   This is essentially the algorithm presented in Figure 7.8 of Morgan,
   with a few changes to reduce pattern search time in favour of a bit
   more memory usage.  */

/* One of these is created for each set.  It will live in a list local
   to its basic block for the duration of that block's processing.  */
struct rename_set_data
{
  struct rename_set_data *next;
  /* This is the SET_DEST of the (first) SET that sets the REG.  */
  rtx *reg_loc;
  /* This is what used to be at *REG_LOC.  */
  rtx old_reg;
  /* This is the REG that will replace OLD_REG.  It's set only
     when the rename data is moved onto the DONE_RENAMES queue.  */
  rtx new_reg;
  /* This is what to restore ssa_rename_to_lookup (old_reg) to.  It is
     usually the previous contents of ssa_rename_to_lookup (old_reg).  */
  rtx prev_reg;
  /* This is the insn that contains all the SETs of the REG.  */
  rtx set_insn;
};

/* This struct is used to pass information to callback functions while
   renaming registers.  */
struct rename_context
{
  struct rename_set_data *new_renames;
  struct rename_set_data *done_renames;
  rtx current_insn;
};

/* Queue the rename of *REG_LOC.  */
static void
create_delayed_rename (c, reg_loc)
     struct rename_context *c;
     rtx *reg_loc;
{
  struct rename_set_data *r;
  r = (struct rename_set_data *) xmalloc (sizeof(*r));
  
  if (GET_CODE (*reg_loc) != REG
      || !CONVERT_REGISTER_TO_SSA_P (REGNO (*reg_loc)))
    abort ();

  r->reg_loc = reg_loc;
  r->old_reg = *reg_loc;
  r->prev_reg = ssa_rename_to_lookup(r->old_reg);
  r->set_insn = c->current_insn;
  r->next = c->new_renames;
  c->new_renames = r;
}

/* This is part of a rather ugly hack to allow the pre-ssa regno to be
   reused.  If, during processing, a register has not yet been touched,
   ssa_rename_to[regno][machno] will be NULL.  Now, in the course of pushing
   and popping values from ssa_rename_to, when we would ordinarily 
   pop NULL back in, we pop RENAME_NO_RTX.  We treat this exactly the
   same as NULL, except that it signals that the original regno has
   already been reused.  */
#define RENAME_NO_RTX  pc_rtx

/* Move all the entries from NEW_RENAMES onto DONE_RENAMES by
   applying all the renames on NEW_RENAMES.  */

static void
apply_delayed_renames (c)
       struct rename_context *c;
{
  struct rename_set_data *r;
  struct rename_set_data *last_r = NULL;

  for (r = c->new_renames; r != NULL; r = r->next)
    {
      int new_regno;
      
      /* Failure here means that someone has a PARALLEL that sets
	 a register twice (bad!).  */
      if (ssa_rename_to_lookup (r->old_reg) != r->prev_reg)
	abort ();
      /* Failure here means we have changed REG_LOC before applying
	 the rename.  */
      /* For the first set we come across, reuse the original regno.  */
      if (r->prev_reg == NULL_RTX && !HARD_REGISTER_P (r->old_reg))
	{
	  r->new_reg = r->old_reg;
	  /* We want to restore RENAME_NO_RTX rather than NULL_RTX.  */
	  r->prev_reg = RENAME_NO_RTX;
	}
      else
	r->new_reg = gen_reg_rtx (GET_MODE (r->old_reg));
      new_regno = REGNO (r->new_reg);
      ssa_rename_to_insert (r->old_reg, r->new_reg);

      if (new_regno >= (int) ssa_definition->num_elements)
	{
	  int new_limit = new_regno * 5 / 4;
	  VARRAY_GROW (ssa_definition, new_limit);
	}

      VARRAY_RTX (ssa_definition, new_regno) = r->set_insn;
      ssa_rename_from_insert (new_regno, r->old_reg);
      last_r = r;
    }
  if (last_r != NULL)
    {
      last_r->next = c->done_renames;
      c->done_renames = c->new_renames;
      c->new_renames = NULL;
    }
}

/* Part one of the first step of rename_block, called through for_each_rtx. 
   Mark pseudos that are set for later update.  Transform uses of pseudos.  */

static int
rename_insn_1 (ptr, data)
     rtx *ptr;
     void *data;
{
  rtx x = *ptr;
  struct rename_context *context = data;

  if (x == NULL_RTX)
    return 0;

  switch (GET_CODE (x))
    {
    case SET:
      {
	rtx *destp = &SET_DEST (x);
	rtx dest = SET_DEST (x);

	/* An assignment to a paradoxical SUBREG does not read from
	   the destination operand, and thus does not need to be
	   wrapped into a SEQUENCE when translating into SSA form.
	   We merely strip off the SUBREG and proceed normally for
	   this case.  */
	if (GET_CODE (dest) == SUBREG
	    && (GET_MODE_SIZE (GET_MODE (dest))
		> GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest))))
	    && GET_CODE (SUBREG_REG (dest)) == REG
	    && CONVERT_REGISTER_TO_SSA_P (REGNO (SUBREG_REG (dest))))
	  {
	    destp = &XEXP (dest, 0);
	    dest = XEXP (dest, 0);
	  }

	/* Some SETs also use the REG specified in their LHS.
	   These can be detected by the presence of
	   STRICT_LOW_PART, SUBREG, SIGN_EXTRACT, and ZERO_EXTRACT
	   in the LHS.  Handle these by changing
	   (set (subreg (reg foo)) ...)
	   into
	   (sequence [(set (reg foo_1) (reg foo))
	              (set (subreg (reg foo_1)) ...)])  

	   FIXME: Much of the time this is too much.  For some constructs
	   we know that the output register is strictly an output
	   (paradoxical SUBREGs and some libcalls for example).

	   For those cases we are better off not making the false
	   dependency.  */
	if (GET_CODE (dest) == STRICT_LOW_PART
	    || GET_CODE (dest) == SUBREG
	    || GET_CODE (dest) == SIGN_EXTRACT
	    || GET_CODE (dest) == ZERO_EXTRACT)
	  {
	    rtx i, reg;
	    reg = dest;
	    
	    while (GET_CODE (reg) == STRICT_LOW_PART
		   || GET_CODE (reg) == SUBREG
		   || GET_CODE (reg) == SIGN_EXTRACT
		   || GET_CODE (reg) == ZERO_EXTRACT)
		reg = XEXP (reg, 0);
	    
	    if (GET_CODE (reg) == REG
		&& CONVERT_REGISTER_TO_SSA_P (REGNO (reg)))
	      {
		/* Generate (set reg reg), and do renaming on it so
		   that it becomes (set reg_1 reg_0), and we will
		   replace reg with reg_1 in the SUBREG.  */

		struct rename_set_data *saved_new_renames;
		saved_new_renames = context->new_renames;
		context->new_renames = NULL;
		i = emit_insn (gen_rtx_SET (VOIDmode, reg, reg));
		for_each_rtx (&i, rename_insn_1, data);
		apply_delayed_renames (context);
		context->new_renames = saved_new_renames;
	      }
	  }
	else if (GET_CODE (dest) == REG
		 && CONVERT_REGISTER_TO_SSA_P (REGNO (dest)))
	  {
	    /* We found a genuine set of an interesting register.  Tag
	       it so that we can create a new name for it after we finish
	       processing this insn.  */

	    create_delayed_rename (context, destp);

	    /* Since we do not wish to (directly) traverse the
	       SET_DEST, recurse through for_each_rtx for the SET_SRC
	       and return.  */
	    if (GET_CODE (x) == SET)
	      for_each_rtx (&SET_SRC (x), rename_insn_1, data);
	    return -1;
	  }

	/* Otherwise, this was not an interesting destination.  Continue
	   on, marking uses as normal.  */
	return 0;
      }

    case REG:
      if (CONVERT_REGISTER_TO_SSA_P (REGNO (x)) &&
	  REGNO (x) < ssa_max_reg_num)
	{
	  rtx new_reg = ssa_rename_to_lookup (x);

	  if (new_reg != NULL_RTX && new_reg != RENAME_NO_RTX)
	    {
	      if (GET_MODE (x) != GET_MODE (new_reg))
		abort ();
	      *ptr = new_reg;
	    }
	  /* Else this is a use before a set.  Warn?  */
	}
      return -1;

    case CLOBBER:
      /* There is considerable debate on how CLOBBERs ought to be
	 handled in SSA.  For now, we're keeping the CLOBBERs, which
	 means that we don't really have SSA form.  There are a couple
	 of proposals for how to fix this problem, but neither is
	 implemented yet.  */
      {
	rtx dest = XCEXP (x, 0, CLOBBER);
	if (REG_P (dest))
	  {
	    if (CONVERT_REGISTER_TO_SSA_P (REGNO (dest))
		&& REGNO (dest) < ssa_max_reg_num)
	      {
		rtx new_reg = ssa_rename_to_lookup (dest);
		if (new_reg != NULL_RTX && new_reg != RENAME_NO_RTX)
		    XCEXP (x, 0, CLOBBER) = new_reg;
	      }
	    /* Stop traversing.  */
	    return -1;
	  }	    
	else
	  /* Continue traversing.  */
	  return 0;
      }

    case PHI:
      /* Never muck with the phi.  We do that elsewhere, special-like.  */
      return -1;

    default:
      /* Anything else, continue traversing.  */
      return 0;
    }
}

static void
rename_block (bb, idom)
     int bb;
     int *idom;
{
  basic_block b = BASIC_BLOCK (bb);
  edge e;
  rtx insn, next, last;
  struct rename_set_data *set_data = NULL;
  int c;

  /* Step One: Walk the basic block, adding new names for sets and
     replacing uses.  */
     
  next = b->head;
  last = b->end;
  do
    {
      insn = next;
      if (INSN_P (insn))
	{
	  struct rename_context context;
	  context.done_renames = set_data;
	  context.new_renames = NULL;
	  context.current_insn = insn;

	  start_sequence ();
	  for_each_rtx (&PATTERN (insn), rename_insn_1, &context);
	  for_each_rtx (&REG_NOTES (insn), rename_insn_1, &context);

	  /* Sometimes, we end up with a sequence of insns that
	     SSA needs to treat as a single insn.  Wrap these in a
	     SEQUENCE.  (Any notes now get attached to the SEQUENCE,
	     not to the old version inner insn.)  */
	  if (get_insns () != NULL_RTX)
	    {
	      rtx seq;
	      int i;
	      
	      emit (PATTERN (insn));
	      seq = gen_sequence ();
	      /* We really want a SEQUENCE of SETs, not a SEQUENCE
		 of INSNs.  */
	      for (i = 0; i < XVECLEN (seq, 0); i++)
		XVECEXP (seq, 0, i) = PATTERN (XVECEXP (seq, 0, i));
	      PATTERN (insn) = seq;
	    }
	  end_sequence ();
	  
	  apply_delayed_renames (&context);
	  set_data = context.done_renames;
	}

      next = NEXT_INSN (insn);
    }
  while (insn != last);

  /* Step Two: Update the phi nodes of this block's successors.  */

  for (e = b->succ; e; e = e->succ_next)
    {
      if (e->dest == EXIT_BLOCK_PTR)
	continue;

      insn = first_insn_after_basic_block_note (e->dest);

      while (PHI_NODE_P (insn))
	{
	  rtx phi = PATTERN (insn);
	  rtx reg;

	  /* Find out which of our outgoing registers this node is
	     intended to replace.  Note that if this is not the first PHI
	     node to have been created for this register, we have to
	     jump through rename links to figure out which register
	     we're talking about.  This can easily be recognized by
	     noting that the regno is new to this pass.  */
	  reg = SET_DEST (phi);
	  if (REGNO (reg) >= ssa_max_reg_num)
	    reg = ssa_rename_from_lookup (REGNO (reg));
	  if (reg == NULL_RTX)
	    abort ();
	  reg = ssa_rename_to_lookup (reg);

	  /* It is possible for the variable to be uninitialized on
	     edges in.  Reduce the arity of the PHI so that we don't
	     consider those edges.  */
	  if (reg == NULL || reg == RENAME_NO_RTX)
	    {
	      if (! remove_phi_alternative (phi, b))
		abort ();
	    }
	  else
	    {
	      /* When we created the PHI nodes, we did not know what mode
		 the register should be.  Now that we've found an original,
		 we can fill that in.  */
	      if (GET_MODE (SET_DEST (phi)) == VOIDmode)
		PUT_MODE (SET_DEST (phi), GET_MODE (reg));
	      else if (GET_MODE (SET_DEST (phi)) != GET_MODE (reg))
		abort ();

	      *phi_alternative (phi, bb) = reg;
	    }

	  insn = NEXT_INSN (insn);
	}
    }

  /* Step Three: Do the same to the children of this block in
     dominator order.  */

  for (c = 0; c < n_basic_blocks; ++c)
    if (idom[c] == bb)
      rename_block (c, idom);

  /* Step Four: Update the sets to refer to their new register,
     and restore ssa_rename_to to its previous state.  */

  while (set_data)
    {
      struct rename_set_data *next;
      rtx old_reg = *set_data->reg_loc;

      if (*set_data->reg_loc != set_data->old_reg)
	abort ();
      *set_data->reg_loc = set_data->new_reg;

      ssa_rename_to_insert (old_reg, set_data->prev_reg);

      next = set_data->next;
      free (set_data);
      set_data = next;
    }      
}

static void
rename_registers (nregs, idom)
     int nregs;
     int *idom;
{
  VARRAY_RTX_INIT (ssa_definition, nregs * 3, "ssa_definition");
  ssa_rename_from_initialize ();

  ssa_rename_to_pseudo = (rtx *) alloca (nregs * sizeof(rtx));
  memset ((char *) ssa_rename_to_pseudo, 0, nregs * sizeof(rtx));
  memset ((char *) ssa_rename_to_hard, 0, 
	 FIRST_PSEUDO_REGISTER * NUM_MACHINE_MODES * sizeof (rtx));

  rename_block (0, idom);

  /* ??? Update basic_block_live_at_start, and other flow info 
     as needed.  */

  ssa_rename_to_pseudo = NULL;
}

/* The main entry point for moving to SSA.  */

void
convert_to_ssa ()
{
  /* Element I is the set of blocks that set register I.  */
  sbitmap *evals;

  /* Dominator bitmaps.  */
  sbitmap *dfs;
  sbitmap *idfs;

  /* Element I is the immediate dominator of block I.  */
  int *idom;

  int nregs;

  /* Don't do it twice.  */
  if (in_ssa_form)
    abort ();

  /* Need global_live_at_{start,end} up to date.  Do not remove any
     dead code.  We'll let the SSA optimizers do that.  */
  life_analysis (get_insns (), NULL, 0);

  idom = (int *) alloca (n_basic_blocks * sizeof (int));
  memset ((void *) idom, -1, (size_t) n_basic_blocks * sizeof (int));
  calculate_dominance_info (idom, NULL, CDI_DOMINATORS);

  if (rtl_dump_file)
    {
      int i;
      fputs (";; Immediate Dominators:\n", rtl_dump_file);
      for (i = 0; i < n_basic_blocks; ++i)
	fprintf (rtl_dump_file, ";\t%3d = %3d\n", i, idom[i]);
      fflush (rtl_dump_file);
    }

  /* Compute dominance frontiers.  */

  dfs = sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  compute_dominance_frontiers (dfs, idom);

  if (rtl_dump_file)
    {
      dump_sbitmap_vector (rtl_dump_file, ";; Dominance Frontiers:",
			   "; Basic Block", dfs, n_basic_blocks);
      fflush (rtl_dump_file);
    }

  /* Compute register evaluations.  */

  ssa_max_reg_num = max_reg_num ();
  nregs = ssa_max_reg_num;
  evals = sbitmap_vector_alloc (nregs, n_basic_blocks);
  find_evaluations (evals, nregs);

  /* Compute the iterated dominance frontier for each register.  */

  idfs = sbitmap_vector_alloc (nregs, n_basic_blocks);
  compute_iterated_dominance_frontiers (idfs, dfs, evals, nregs);

  if (rtl_dump_file)
    {
      dump_sbitmap_vector (rtl_dump_file, ";; Iterated Dominance Frontiers:",
			   "; Register", idfs, nregs);
      fflush (rtl_dump_file);
    }

  /* Insert the phi nodes.  */

  insert_phi_nodes (idfs, evals, nregs);

  /* Rename the registers to satisfy SSA.  */

  rename_registers (nregs, idom);

  /* All done!  Clean up and go home.  */

  sbitmap_vector_free (dfs);
  sbitmap_vector_free (evals);
  sbitmap_vector_free (idfs);
  in_ssa_form = 1;

  reg_scan (get_insns (), max_reg_num (), 1);
}

/* REG is the representative temporary of its partition.  Add it to the
   set of nodes to be processed, if it hasn't been already.  Return the
   index of this register in the node set.  */

static inline int
ephi_add_node (reg, nodes, n_nodes)
     rtx reg, *nodes;
     int *n_nodes;
{
  int i;
  for (i = *n_nodes - 1; i >= 0; --i)
    if (REGNO (reg) == REGNO (nodes[i]))
      return i;

  nodes[i = (*n_nodes)++] = reg;
  return i;
}

/* Part one of the topological sort.  This is a forward (downward) search
   through the graph collecting a stack of nodes to process.  Assuming no
   cycles, the nodes at top of the stack when we are finished will have
   no other dependencies.  */

static int *
ephi_forward (t, visited, succ, tstack)
     int t;
     sbitmap visited;
     sbitmap *succ;
     int *tstack;
{
  int s;

  SET_BIT (visited, t);

  EXECUTE_IF_SET_IN_SBITMAP (succ[t], 0, s,
    {
      if (! TEST_BIT (visited, s))
        tstack = ephi_forward (s, visited, succ, tstack);
    });

  *tstack++ = t;
  return tstack;
}

/* Part two of the topological sort.  The is a backward search through
   a cycle in the graph, copying the data forward as we go.  */

static void
ephi_backward (t, visited, pred, nodes)
     int t;
     sbitmap visited, *pred;
     rtx *nodes;
{
  int p;

  SET_BIT (visited, t);

  EXECUTE_IF_SET_IN_SBITMAP (pred[t], 0, p,
    {
      if (! TEST_BIT (visited, p))
	{
	  ephi_backward (p, visited, pred, nodes);
	  emit_move_insn (nodes[p], nodes[t]);
	}
    });
}

/* Part two of the topological sort.  Create the copy for a register
   and any cycle of which it is a member.  */

static void
ephi_create (t, visited, pred, succ, nodes)
     int t;
     sbitmap visited, *pred, *succ;
     rtx *nodes;
{
  rtx reg_u = NULL_RTX;
  int unvisited_predecessors = 0;
  int p;

  /* Iterate through the predecessor list looking for unvisited nodes.
     If there are any, we have a cycle, and must deal with that.  At 
     the same time, look for a visited predecessor.  If there is one,
     we won't need to create a temporary.  */

  EXECUTE_IF_SET_IN_SBITMAP (pred[t], 0, p,
    {
      if (! TEST_BIT (visited, p))
	unvisited_predecessors = 1;
      else if (!reg_u)
	reg_u = nodes[p];
    });

  if (unvisited_predecessors)
    {
      /* We found a cycle.  Copy out one element of the ring (if necessary),
	 then traverse the ring copying as we go.  */

      if (!reg_u)
	{
	  reg_u = gen_reg_rtx (GET_MODE (nodes[t]));
	  emit_move_insn (reg_u, nodes[t]);
	}

      EXECUTE_IF_SET_IN_SBITMAP (pred[t], 0, p,
	{
	  if (! TEST_BIT (visited, p))
	    {
	      ephi_backward (p, visited, pred, nodes);
	      emit_move_insn (nodes[p], reg_u);
	    }
	});
    }  
  else 
    {
      /* No cycle.  Just copy the value from a successor.  */

      int s;
      EXECUTE_IF_SET_IN_SBITMAP (succ[t], 0, s,
	{
	  SET_BIT (visited, t);
	  emit_move_insn (nodes[t], nodes[s]);
	  return;
	});
    }
}

/* Convert the edge to normal form.  */

static void
eliminate_phi (e, reg_partition)
     edge e;
     partition reg_partition;
{
  int n_nodes;
  sbitmap *pred, *succ;
  sbitmap visited;
  rtx *nodes;
  int *stack, *tstack;
  rtx insn;
  int i;

  /* Collect an upper bound on the number of registers needing processing.  */

  insn = first_insn_after_basic_block_note (e->dest);

  n_nodes = 0;
  while (PHI_NODE_P (insn))
    {
      insn = next_nonnote_insn (insn);
      n_nodes += 2;
    }

  if (n_nodes == 0)
    return;

  /* Build the auxiliary graph R(B). 

     The nodes of the graph are the members of the register partition
     present in Phi(B).  There is an edge from FIND(T0)->FIND(T1) for
     each T0 = PHI(...,T1,...), where T1 is for the edge from block C.  */

  nodes = (rtx *) alloca (n_nodes * sizeof(rtx));
  pred = sbitmap_vector_alloc (n_nodes, n_nodes);
  succ = sbitmap_vector_alloc (n_nodes, n_nodes);
  sbitmap_vector_zero (pred, n_nodes);
  sbitmap_vector_zero (succ, n_nodes);

  insn = first_insn_after_basic_block_note (e->dest);

  n_nodes = 0;
  for (; PHI_NODE_P (insn); insn = next_nonnote_insn (insn))
    {
      rtx* preg = phi_alternative (PATTERN (insn), e->src->index);
      rtx tgt = SET_DEST (PATTERN (insn));
      rtx reg;

      /* There may be no phi alternative corresponding to this edge.
	 This indicates that the phi variable is undefined along this
	 edge.  */
      if (preg == NULL)
	continue;
      reg = *preg;

      if (GET_CODE (reg) != REG || GET_CODE (tgt) != REG)
	abort ();

      reg = regno_reg_rtx[partition_find (reg_partition, REGNO (reg))];
      tgt = regno_reg_rtx[partition_find (reg_partition, REGNO (tgt))];
      /* If the two registers are already in the same partition, 
	 nothing will need to be done.  */
      if (reg != tgt)
	{
	  int ireg, itgt;

	  ireg = ephi_add_node (reg, nodes, &n_nodes);
	  itgt = ephi_add_node (tgt, nodes, &n_nodes);

	  SET_BIT (pred[ireg], itgt);
	  SET_BIT (succ[itgt], ireg);
	}
    }

  if (n_nodes == 0)
    goto out;

  /* Begin a topological sort of the graph.  */

  visited = sbitmap_alloc (n_nodes);
  sbitmap_zero (visited);

  tstack = stack = (int *) alloca (n_nodes * sizeof (int));

  for (i = 0; i < n_nodes; ++i)
    if (! TEST_BIT (visited, i))
      tstack = ephi_forward (i, visited, succ, tstack);

  sbitmap_zero (visited);

  /* As we find a solution to the tsort, collect the implementation 
     insns in a sequence.  */
  start_sequence ();
  
  while (tstack != stack)
    {
      i = *--tstack;
      if (! TEST_BIT (visited, i))
	ephi_create (i, visited, pred, succ, nodes);
    }

  insn = gen_sequence ();
  end_sequence ();
  insert_insn_on_edge (insn, e);
  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Emitting copy on edge (%d,%d)\n",
	     e->src->index, e->dest->index);

  sbitmap_free (visited);
out:
  sbitmap_vector_free (pred);
  sbitmap_vector_free (succ);
}

/* For basic block B, consider all phi insns which provide an
   alternative corresponding to an incoming abnormal critical edge.
   Place the phi alternative corresponding to that abnormal critical
   edge in the same register class as the destination of the set.  

   From Morgan, p. 178:

     For each abnormal critical edge (C, B), 
     if T0 = phi (T1, ..., Ti, ..., Tm) is a phi node in B, 
     and C is the ith predecessor of B, 
     then T0 and Ti must be equivalent. 

   Return non-zero iff any such cases were found for which the two
   regs were not already in the same class.  */

static int
make_regs_equivalent_over_bad_edges (bb, reg_partition)
     int bb;
     partition reg_partition;
{
  int changed = 0;
  basic_block b = BASIC_BLOCK (bb);
  rtx phi;

  /* Advance to the first phi node.  */
  phi = first_insn_after_basic_block_note (b);

  /* Scan all the phi nodes.  */
  for (; 
       PHI_NODE_P (phi);
       phi = next_nonnote_insn (phi))
    {
      edge e;
      int tgt_regno;
      rtx set = PATTERN (phi);
      rtx tgt = SET_DEST (set);

      /* The set target is expected to be an SSA register.  */
      if (GET_CODE (tgt) != REG 
	  || !CONVERT_REGISTER_TO_SSA_P (REGNO (tgt)))
	abort ();
      tgt_regno = REGNO (tgt);

      /* Scan incoming abnormal critical edges.  */
      for (e = b->pred; e; e = e->pred_next)
	if ((e->flags & EDGE_ABNORMAL) && EDGE_CRITICAL_P (e))
	  {
	    rtx *alt = phi_alternative (set, e->src->index);
	    int alt_regno;

	    /* If there is no alternative corresponding to this edge,
	       the value is undefined along the edge, so just go on.  */
	    if (alt == 0)
	      continue;

	    /* The phi alternative is expected to be an SSA register.  */
	    if (GET_CODE (*alt) != REG 
		|| !CONVERT_REGISTER_TO_SSA_P (REGNO (*alt)))
	      abort ();
	    alt_regno = REGNO (*alt);

	    /* If the set destination and the phi alternative aren't
	       already in the same class...  */
	    if (partition_find (reg_partition, tgt_regno) 
		!= partition_find (reg_partition, alt_regno))
	      {
		/* ... make them such.  */
		if (conflicting_hard_regs_p (tgt_regno, alt_regno))
		  /* It is illegal to unify a hard register with a
		     different register.  */
		  abort ();
		
		partition_union (reg_partition, 
				 tgt_regno, alt_regno);
		++changed;
	      }
	  }
    }

  return changed;
}

/* Consider phi insns in basic block BB pairwise.  If the set target
   of both isns are equivalent pseudos, make the corresponding phi
   alternatives in each phi corresponding equivalent.

   Return nonzero if any new register classes were unioned.  */

static int
make_equivalent_phi_alternatives_equivalent (bb, reg_partition)
     int bb;
     partition reg_partition;
{
  int changed = 0;
  basic_block b = BASIC_BLOCK (bb);
  rtx phi;

  /* Advance to the first phi node.  */
  phi = first_insn_after_basic_block_note (b);

  /* Scan all the phi nodes.  */
  for (; 
       PHI_NODE_P (phi);
       phi = next_nonnote_insn (phi))
    {
      rtx set = PATTERN (phi);
      /* The regno of the destination of the set.  */
      int tgt_regno = REGNO (SET_DEST (PATTERN (phi)));

      rtx phi2 = next_nonnote_insn (phi);

      /* Scan all phi nodes following this one.  */
      for (;
	   PHI_NODE_P (phi2);
	   phi2 = next_nonnote_insn (phi2))
	{
	  rtx set2 = PATTERN (phi2);
	  /* The regno of the destination of the set.  */
	  int tgt2_regno = REGNO (SET_DEST (set2));
		  
	  /* Are the set destinations equivalent regs?  */
	  if (partition_find (reg_partition, tgt_regno) ==
	      partition_find (reg_partition, tgt2_regno))
	    {
	      edge e;
	      /* Scan over edges.  */
	      for (e = b->pred; e; e = e->pred_next)
		{
		  int pred_block = e->src->index;
		  /* Identify the phi alternatives from both phi
		     nodes corresponding to this edge.  */
		  rtx *alt = phi_alternative (set, pred_block);
		  rtx *alt2 = phi_alternative (set2, pred_block);

		  /* If one of the phi nodes doesn't have a
		     corresponding alternative, just skip it.  */
		  if (alt == 0 || alt2 == 0)
		    continue;

		  /* Both alternatives should be SSA registers.  */
		  if (GET_CODE (*alt) != REG
		      || !CONVERT_REGISTER_TO_SSA_P (REGNO (*alt)))
		    abort ();
		  if (GET_CODE (*alt2) != REG
		      || !CONVERT_REGISTER_TO_SSA_P (REGNO (*alt2)))
		    abort ();

		  /* If the alternatives aren't already in the same
		     class ...  */
		  if (partition_find (reg_partition, REGNO (*alt)) 
		      != partition_find (reg_partition, REGNO (*alt2)))
		    {
		      /* ... make them so.  */
		      if (conflicting_hard_regs_p (REGNO (*alt), REGNO (*alt2)))
			/* It is illegal to unify a hard register with
			   a different register.  */
			abort ();

		      partition_union (reg_partition, 
				       REGNO (*alt), REGNO (*alt2));
		      ++changed;
		    }
		}
	    }
	}
    }

  return changed;
}

/* Compute a conservative partition of outstanding pseudo registers.
   See Morgan 7.3.1.  */

static partition
compute_conservative_reg_partition ()
{
  int bb;
  int changed = 0;

  /* We don't actually work with hard registers, but it's easier to
     carry them around anyway rather than constantly doing register
     number arithmetic.  */
  partition p = 
    partition_new (ssa_definition->num_elements);

  /* The first priority is to make sure registers that might have to
     be copied on abnormal critical edges are placed in the same
     partition.  This saves us from having to split abnormal critical
     edges.  */
  for (bb = n_basic_blocks; --bb >= 0; )
    changed += make_regs_equivalent_over_bad_edges (bb, p);
  
  /* Now we have to insure that corresponding arguments of phi nodes
     assigning to corresponding regs are equivalent.  Iterate until
     nothing changes.  */
  while (changed > 0)
    {
      changed = 0;
      for (bb = n_basic_blocks; --bb >= 0; )
	changed += make_equivalent_phi_alternatives_equivalent (bb, p);
    }

  return p;
}

/* The following functions compute a register partition that attempts
   to eliminate as many reg copies and phi node copies as possible by
   coalescing registers.   This is the strategy:

    1. As in the conservative case, the top priority is to coalesce
       registers that otherwise would cause copies to be placed on
       abnormal critical edges (which isn't possible).

    2. Figure out which regs are involved (in the LHS or RHS) of
       copies and phi nodes.  Compute conflicts among these regs.  

    3. Walk around the instruction stream, placing two regs in the
       same class of the partition if one appears on the LHS and the
       other on the RHS of a copy or phi node and the two regs don't
       conflict.  The conflict information of course needs to be
       updated.  

    4. If anything has changed, there may be new opportunities to
       coalesce regs, so go back to 2.
*/

/* If REG1 and REG2 don't conflict in CONFLICTS, place them in the
   same class of partition P, if they aren't already.  Update
   CONFLICTS appropriately.  

   Returns one if REG1 and REG2 were placed in the same class but were
   not previously; zero otherwise.  

   See Morgan figure 11.15.  */

static int 
coalesce_if_unconflicting (p, conflicts, reg1, reg2)
     partition p;
     conflict_graph conflicts;
     int reg1;
     int reg2;
{
  int reg;

  /* Work only on SSA registers.  */
  if (!CONVERT_REGISTER_TO_SSA_P (reg1) || !CONVERT_REGISTER_TO_SSA_P (reg2))
    return 0;

  /* Find the canonical regs for the classes containing REG1 and
     REG2.  */
  reg1 = partition_find (p, reg1);
  reg2 = partition_find (p, reg2);
  
  /* If they're already in the same class, there's nothing to do.  */
  if (reg1 == reg2)
    return 0;

  /* If the regs conflict, our hands are tied.  */
  if (conflicting_hard_regs_p (reg1, reg2) ||
      conflict_graph_conflict_p (conflicts, reg1, reg2))
    return 0;

  /* We're good to go.  Put the regs in the same partition.  */
  partition_union (p, reg1, reg2);

  /* Find the new canonical reg for the merged class.  */
  reg = partition_find (p, reg1);
  
  /* Merge conflicts from the two previous classes.  */
  conflict_graph_merge_regs (conflicts, reg, reg1);
  conflict_graph_merge_regs (conflicts, reg, reg2);

  return 1;
}

/* For each register copy insn in basic block BB, place the LHS and
   RHS regs in the same class in partition P if they do not conflict
   according to CONFLICTS.

   Returns the number of changes that were made to P.

   See Morgan figure 11.14.  */

static int
coalesce_regs_in_copies (bb, p, conflicts)
     basic_block bb;
     partition p;
     conflict_graph conflicts;
{
  int changed = 0;
  rtx insn;
  rtx end = bb->end;

  /* Scan the instruction stream of the block.  */
  for (insn = bb->head; insn != end; insn = NEXT_INSN (insn))
    {
      rtx pattern;
      rtx src;
      rtx dest;

      /* If this isn't a set insn, go to the next insn.  */
      if (GET_CODE (insn) != INSN)
	continue;
      pattern = PATTERN (insn);
      if (GET_CODE (pattern) != SET)
	continue;

      src = SET_SRC (pattern);
      dest = SET_DEST (pattern);

      /* We're only looking for copies.  */
      if (GET_CODE (src) != REG || GET_CODE (dest) != REG)
	continue;

      /* Coalesce only if the reg modes are the same.  As long as
	 each reg's rtx is unique, it can have only one mode, so two
	 pseudos of different modes can't be coalesced into one.  

         FIXME: We can probably get around this by inserting SUBREGs
         where appropriate, but for now we don't bother.  */
      if (GET_MODE (src) != GET_MODE (dest))
	continue;

      /* Found a copy; see if we can use the same reg for both the
	 source and destination (and thus eliminate the copy,
	 ultimately).  */
      changed += coalesce_if_unconflicting (p, conflicts, 
					    REGNO (src), REGNO (dest));
    }

  return changed;
}

struct phi_coalesce_context
{
  partition p;
  conflict_graph conflicts;
  int changed;
};

/* Callback function for for_each_successor_phi.  If the set
   destination and the phi alternative regs do not conflict, place
   them in the same paritition class.  DATA is a pointer to a
   phi_coalesce_context struct.  */

static int
coalesce_reg_in_phi (insn, dest_regno, src_regno, data)
     rtx insn ATTRIBUTE_UNUSED;
     int dest_regno;
     int src_regno;
     void *data;
{
  struct phi_coalesce_context *context = 
    (struct phi_coalesce_context *) data;
  
  /* Attempt to use the same reg, if they don't conflict.  */
  context->changed 
    += coalesce_if_unconflicting (context->p, context->conflicts, 
				  dest_regno, src_regno);
  return 0;
}

/* For each alternative in a phi function corresponding to basic block
   BB (in phi nodes in successor block to BB), place the reg in the
   phi alternative and the reg to which the phi value is set into the
   same class in partition P, if allowed by CONFLICTS.  

   Return the number of changes that were made to P.
   
   See Morgan figure 11.14.  */

static int
coalesce_regs_in_successor_phi_nodes (bb, p, conflicts)
     basic_block bb;
     partition p;
     conflict_graph conflicts;
{
  struct phi_coalesce_context context;
  context.p = p;
  context.conflicts = conflicts;
  context.changed = 0;

  for_each_successor_phi (bb, &coalesce_reg_in_phi, &context);

  return context.changed;
}

/* Compute and return a partition of pseudos.  Where possible,
   non-conflicting pseudos are placed in the same class.  

   The caller is responsible for deallocating the returned partition.  */

static partition
compute_coalesced_reg_partition ()
{
  int bb;
  int changed = 0;
  regset_head phi_set_head;
  regset phi_set = &phi_set_head;

  partition p = 
    partition_new (ssa_definition->num_elements);

  /* The first priority is to make sure registers that might have to
     be copied on abnormal critical edges are placed in the same
     partition.  This saves us from having to split abnormal critical
     edges (which can't be done).  */
  for (bb = n_basic_blocks; --bb >= 0; )
    make_regs_equivalent_over_bad_edges (bb, p);

  INIT_REG_SET (phi_set);

  do
    {
      conflict_graph conflicts;

      changed = 0;

      /* Build the set of registers involved in phi nodes, either as
	 arguments to the phi function or as the target of a set.  */
      CLEAR_REG_SET (phi_set);
      mark_phi_and_copy_regs (phi_set);

      /* Compute conflicts.  */
      conflicts = conflict_graph_compute (phi_set, p);

      /* FIXME: Better would be to process most frequently executed
	 blocks first, so that most frequently executed copies would
	 be more likely to be removed by register coalescing.  But any
	 order will generate correct, if non-optimal, results.  */
      for (bb = n_basic_blocks; --bb >= 0; )
	{
	  basic_block block = BASIC_BLOCK (bb);
	  changed += coalesce_regs_in_copies (block, p, conflicts);
	  changed += 
	    coalesce_regs_in_successor_phi_nodes (block, p, conflicts);
	}

      conflict_graph_delete (conflicts);
    }
  while (changed > 0);

  FREE_REG_SET (phi_set);

  return p;
}

/* Mark the regs in a phi node.  PTR is a phi expression or one of its
   components (a REG or a CONST_INT).  DATA is a reg set in which to
   set all regs.  Called from for_each_rtx.  */

static int
mark_reg_in_phi (ptr, data)
     rtx *ptr;
     void *data;
{
  rtx expr = *ptr;
  regset set = (regset) data;

  switch (GET_CODE (expr))
    {
    case REG:
      SET_REGNO_REG_SET (set, REGNO (expr));
      /* Fall through.  */
    case CONST_INT:
    case PHI:
      return 0;
    default:
      abort ();
    }
}

/* Mark in PHI_SET all pseudos that are used in a phi node -- either
   set from a phi expression, or used as an argument in one.  Also
   mark regs that are the source or target of a reg copy.  Uses
   ssa_definition.  */

static void
mark_phi_and_copy_regs (phi_set)
     regset phi_set;
{
  unsigned int reg;

  /* Scan the definitions of all regs.  */
  for (reg = 0; reg < VARRAY_SIZE (ssa_definition); ++reg)
    if (CONVERT_REGISTER_TO_SSA_P (reg))
      {
	rtx insn = VARRAY_RTX (ssa_definition, reg);
	rtx pattern;
	rtx src;

	if (insn == NULL
	    || (GET_CODE (insn) == NOTE
		&& NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED))
	  continue;
	pattern = PATTERN (insn);
	/* Sometimes we get PARALLEL insns.  These aren't phi nodes or
	   copies.  */
	if (GET_CODE (pattern) != SET)
	  continue;
	src = SET_SRC (pattern);

	if (GET_CODE (src) == REG)
	  {
	    /* It's a reg copy.  */
	    SET_REGNO_REG_SET (phi_set, reg);
	    SET_REGNO_REG_SET (phi_set, REGNO (src));
	  }
	else if (GET_CODE (src) == PHI)
	  {
	    /* It's a phi node.  Mark the reg being set.  */
	    SET_REGNO_REG_SET (phi_set, reg);
	    /* Mark the regs used in the phi function.  */
	    for_each_rtx (&src, mark_reg_in_phi, phi_set);
	  }
	/* ... else nothing to do.  */
      }
}

/* Rename regs in insn PTR that are equivalent.  DATA is the register
   partition which specifies equivalences.  */

static int
rename_equivalent_regs_in_insn (ptr, data)
     rtx *ptr;
     void* data;
{
  rtx x = *ptr;
  partition reg_partition = (partition) data;

  if (x == NULL_RTX)
    return 0;

  switch (GET_CODE (x))
    {
    case REG:
      if (CONVERT_REGISTER_TO_SSA_P (REGNO (x)))
	{
	  unsigned int regno = REGNO (x);
	  unsigned int new_regno = partition_find (reg_partition, regno);
	  rtx canonical_element_rtx = ssa_rename_from_lookup (new_regno);

	  if (canonical_element_rtx != NULL_RTX && 
	      HARD_REGISTER_P (canonical_element_rtx))
	    {
	      if (REGNO (canonical_element_rtx) != regno)
		*ptr = canonical_element_rtx;
	    }
	  else if (regno != new_regno)
	    {
	      rtx new_reg = regno_reg_rtx[new_regno];
	      if (GET_MODE (x) != GET_MODE (new_reg))
		abort ();
	      *ptr = new_reg;
	    }
	}
      return -1;

    case PHI:
      /* No need to rename the phi nodes.  We'll check equivalence
	 when inserting copies.  */
      return -1;

    default:
      /* Anything else, continue traversing.  */
      return 0;
    }
}

/* Record the register's canonical element stored in SRFP in the
   canonical_elements sbitmap packaged in DATA.  This function is used
   as a callback function for traversing ssa_rename_from.  */

static int
record_canonical_element_1 (srfp, data)
     void **srfp;
     void *data;
{
  unsigned int reg = ((ssa_rename_from_pair *) *srfp)->reg;
  sbitmap canonical_elements =
    ((struct ssa_rename_from_hash_table_data *) data)->canonical_elements;
  partition reg_partition =
    ((struct ssa_rename_from_hash_table_data *) data)->reg_partition;
  
  SET_BIT (canonical_elements, partition_find (reg_partition, reg));
  return 1;
}

/* For each class in the REG_PARTITION corresponding to a particular
   hard register and machine mode, check that there are no other
   classes with the same hard register and machine mode.  Returns
   nonzero if this is the case, i.e., the partition is acceptable.  */

static int
check_hard_regs_in_partition (reg_partition)
     partition reg_partition;
{
  /* CANONICAL_ELEMENTS has a nonzero bit if a class with the given register
     number and machine mode has already been seen.  This is a
     problem with the partition.  */
  sbitmap canonical_elements;
  int element_index;
  int already_seen[FIRST_PSEUDO_REGISTER][NUM_MACHINE_MODES];
  int reg;
  int mach_mode;

  /* Collect a list of canonical elements.  */
  canonical_elements = sbitmap_alloc (max_reg_num ());
  sbitmap_zero (canonical_elements);
  ssa_rename_from_traverse (&record_canonical_element_1,
			    canonical_elements, reg_partition);

  /* We have not seen any hard register uses.  */
  for (reg = 0; reg < FIRST_PSEUDO_REGISTER; ++reg)
    for (mach_mode = 0; mach_mode < NUM_MACHINE_MODES; ++mach_mode)
      already_seen[reg][mach_mode] = 0;

  /* Check for classes with the same hard register and machine mode.  */
  EXECUTE_IF_SET_IN_SBITMAP (canonical_elements, 0, element_index,
  {
    rtx hard_reg_rtx = ssa_rename_from_lookup (element_index);
    if (hard_reg_rtx != NULL_RTX &&
	HARD_REGISTER_P (hard_reg_rtx) &&
	already_seen[REGNO (hard_reg_rtx)][GET_MODE (hard_reg_rtx)] != 0)
	  /* Two distinct partition classes should be mapped to the same
	     hard register.  */
	  return 0;
  });

  sbitmap_free (canonical_elements);

  return 1;
}

/* Rename regs that are equivalent in REG_PARTITION.  Also collapse
   any SEQUENCE insns.  */

static void
rename_equivalent_regs (reg_partition)
     partition reg_partition;
{
  int bb;

  for (bb = n_basic_blocks; --bb >= 0; )
    {
      basic_block b = BASIC_BLOCK (bb);
      rtx next = b->head;
      rtx last = b->end;
      rtx insn;

      do
	{
	  insn = next;
	  if (INSN_P (insn))
	    {
	      for_each_rtx (&PATTERN (insn), 
			    rename_equivalent_regs_in_insn, 
			    reg_partition);
	      for_each_rtx (&REG_NOTES (insn), 
			    rename_equivalent_regs_in_insn, 
			    reg_partition);

	      if (GET_CODE (PATTERN (insn)) == SEQUENCE)
		{
		  rtx s = PATTERN (insn);
		  int slen = XVECLEN (s, 0);
		  int i;

		  if (slen <= 1)
		    abort ();

		  PATTERN (insn) = XVECEXP (s, 0, slen-1);
		  for (i = 0; i < slen - 1; i++)
		    emit_insn_before (XVECEXP (s, 0, i), insn);
		}
	    }

	  next = NEXT_INSN (insn);
	}
      while (insn != last);
    }
}

/* The main entry point for moving from SSA.  */

void
convert_from_ssa ()
{
  int bb;
  partition reg_partition;
  rtx insns = get_insns ();

  /* Need global_live_at_{start,end} up to date.  There should not be
     any significant dead code at this point, except perhaps dead
     stores.  So do not take the time to perform dead code elimination. 

     Register coalescing needs death notes, so generate them.  */
  life_analysis (insns, NULL, PROP_DEATH_NOTES);

  /* Figure out which regs in copies and phi nodes don't conflict and
     therefore can be coalesced.  */
  if (conservative_reg_partition)
    reg_partition = compute_conservative_reg_partition ();
  else
    reg_partition = compute_coalesced_reg_partition ();

  if (!check_hard_regs_in_partition (reg_partition))
    /* Two separate partitions should correspond to the same hard
       register but do not.  */
    abort ();

  rename_equivalent_regs (reg_partition);

  /* Eliminate the PHI nodes.  */
  for (bb = n_basic_blocks; --bb >= 0; )
    {
      basic_block b = BASIC_BLOCK (bb);
      edge e;

      for (e = b->pred; e; e = e->pred_next)
	if (e->src != ENTRY_BLOCK_PTR)
	  eliminate_phi (e, reg_partition);
    }

  partition_delete (reg_partition);

  /* Actually delete the PHI nodes.  */
  for (bb = n_basic_blocks; --bb >= 0; )
    {
      rtx insn = BLOCK_HEAD (bb);

      while (1)
	{
	  /* If this is a PHI node delete it.  */
	  if (PHI_NODE_P (insn))
	    {
	      if (insn == BLOCK_END (bb))
		BLOCK_END (bb) = PREV_INSN (insn);
	      insn = delete_insn (insn);
	    }
	  /* Since all the phi nodes come at the beginning of the
	     block, if we find an ordinary insn, we can stop looking
	     for more phi nodes.  */
	  else if (INSN_P (insn))
	    break;
	  /* If we've reached the end of the block, stop.  */
	  else if (insn == BLOCK_END (bb))
	    break;
	  else 
	    insn = NEXT_INSN (insn);
	}
    }

  /* Commit all the copy nodes needed to convert out of SSA form.  */
  commit_edge_insertions ();

  in_ssa_form = 0;

  count_or_remove_death_notes (NULL, 1);

  /* Deallocate the data structures.  */
  VARRAY_FREE (ssa_definition);
  ssa_rename_from_free ();
}

/* Scan phi nodes in successors to BB.  For each such phi node that
   has a phi alternative value corresponding to BB, invoke FN.  FN
   is passed the entire phi node insn, the regno of the set
   destination, the regno of the phi argument corresponding to BB,
   and DATA.

   If FN ever returns non-zero, stops immediately and returns this
   value.  Otherwise, returns zero.  */

int
for_each_successor_phi (bb, fn, data)
     basic_block bb;
     successor_phi_fn fn;
     void *data;
{
  edge e;
  
  if (bb == EXIT_BLOCK_PTR)
    return 0;

  /* Scan outgoing edges.  */
  for (e = bb->succ; e != NULL; e = e->succ_next)
    {
      rtx insn;

      basic_block successor = e->dest;
      if (successor == ENTRY_BLOCK_PTR 
	  || successor == EXIT_BLOCK_PTR)
	continue;

      /* Advance to the first non-label insn of the successor block.  */
      insn = first_insn_after_basic_block_note (successor);

      if (insn == NULL)
	continue;

      /* Scan phi nodes in the successor.  */
      for ( ; PHI_NODE_P (insn); insn = NEXT_INSN (insn))
	{
	  int result;
	  rtx phi_set = PATTERN (insn);
	  rtx *alternative = phi_alternative (phi_set, bb->index);
	  rtx phi_src;
	  
	  /* This phi function may not have an alternative
	     corresponding to the incoming edge, indicating the
	     assigned variable is not defined along the edge.  */
	  if (alternative == NULL)
	    continue;
	  phi_src = *alternative;

	  /* Invoke the callback.  */
	  result = (*fn) (insn, REGNO (SET_DEST (phi_set)), 
			  REGNO (phi_src), data);

	  /* Terminate if requested.  */
	  if (result != 0)
	    return result;
	}
    }

  return 0;
}

/* Assuming the ssa_rename_from mapping has been established, yields
   nonzero if 1) only one SSA register of REG1 and REG2 comes from a
   hard register or 2) both SSA registers REG1 and REG2 come from
   different hard registers.  */

static int
conflicting_hard_regs_p (reg1, reg2)
     int reg1;
     int reg2;
{
  int orig_reg1 = original_register (reg1);
  int orig_reg2 = original_register (reg2);
  if (HARD_REGISTER_NUM_P (orig_reg1) && HARD_REGISTER_NUM_P (orig_reg2)
      && orig_reg1 != orig_reg2)
    return 1;
  if (HARD_REGISTER_NUM_P (orig_reg1) && !HARD_REGISTER_NUM_P (orig_reg2))
    return 1;
  if (!HARD_REGISTER_NUM_P (orig_reg1) && HARD_REGISTER_NUM_P (orig_reg2))
    return 1;
  
  return 0;
}
