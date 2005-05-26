/* Dataflow support routines.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   Contributed by Michael P. Hayes (m.hayes@elec.canterbury.ac.nz,
                                    mhayes@redhat.com)

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
02111-1307, USA.


OVERVIEW:

This file provides some dataflow routines for computing reaching defs,
upward exposed uses, live variables, def-use chains, and use-def
chains.  The global dataflow is performed using simple iterative
methods with a worklist and could be sped up by ordering the blocks
with a depth first search order.

A `struct ref' data structure (ref) is allocated for every register
reference (def or use) and this records the insn and bb the ref is
found within.  The refs are linked together in chains of uses and defs
for each insn and for each register.  Each ref also has a chain field
that links all the use refs for a def or all the def refs for a use.
This is used to create use-def or def-use chains.


USAGE:

Here's an example of using the dataflow routines.

      struct df *df;

      df = df_init ();

      df_analyze (df, 0, DF_ALL);

      df_dump (df, DF_ALL, stderr);

      df_finish (df);


df_init simply creates a poor man's object (df) that needs to be
passed to all the dataflow routines.  df_finish destroys this
object and frees up any allocated memory.   DF_ALL says to analyze
everything.

df_analyze performs the following:

1. Records defs and uses by scanning the insns in each basic block
   or by scanning the insns queued by df_insn_modify.
2. Links defs and uses into insn-def and insn-use chains.
3. Links defs and uses into reg-def and reg-use chains.
4. Assigns LUIDs to each insn (for modified blocks).
5. Calculates local reaching definitions.
6. Calculates global reaching definitions.
7. Creates use-def chains.
8. Calculates local reaching uses (upwards exposed uses).
9. Calculates global reaching uses.
10. Creates def-use chains.
11. Calculates local live registers.
12. Calculates global live registers.
13. Calculates register lifetimes and determines local registers.


PHILOSOPHY:

Note that the dataflow information is not updated for every newly
deleted or created insn.  If the dataflow information requires
updating then all the changed, new, or deleted insns needs to be
marked with df_insn_modify (or df_insns_modify) either directly or
indirectly (say through calling df_insn_delete).  df_insn_modify
marks all the modified insns to get processed the next time df_analyze
 is called.

Beware that tinkering with insns may invalidate the dataflow information.
The philosophy behind these routines is that once the dataflow
information has been gathered, the user should store what they require
before they tinker with any insn.  Once a reg is replaced, for example,
then the reg-def/reg-use chains will point to the wrong place.  Once a
whole lot of changes have been made, df_analyze can be called again
to update the dataflow information.  Currently, this is not very smart
with regard to propagating changes to the dataflow so it should not
be called very often.


DATA STRUCTURES:

The basic object is a REF (reference) and this may either be a DEF
(definition) or a USE of a register.

These are linked into a variety of lists; namely reg-def, reg-use,
  insn-def, insn-use, def-use, and use-def lists.  For example,
the reg-def lists contain all the refs that define a given register
while the insn-use lists contain all the refs used by an insn.

Note that the reg-def and reg-use chains are generally short (except for
the hard registers) and thus it is much faster to search these chains
rather than searching the def or use bitmaps.

If the insns are in SSA form then the reg-def and use-def lists
should only contain the single defining ref.


TODO:

1) Incremental dataflow analysis.

Note that if a loop invariant insn is hoisted (or sunk), we do not
need to change the def-use or use-def chains.  All we have to do is to
change the bb field for all the associated defs and uses and to
renumber the LUIDs for the original and new basic blocks of the insn.

When shadowing loop mems we create new uses and defs for new pseudos
so we do not affect the existing dataflow information.

My current strategy is to queue up all modified, created, or deleted
insns so when df_analyze is called we can easily determine all the new
or deleted refs.  Currently the global dataflow information is
recomputed from scratch but this could be propagated more efficiently.

2) Reduced memory requirements.

We could operate a pool of ref structures.  When a ref is deleted it
gets returned to the pool (say by linking on to a chain of free refs).
This will require a pair of bitmaps for defs and uses so that we can
tell which ones have been changed.  Alternatively, we could
periodically squeeze the def and use tables and associated bitmaps and
renumber the def and use ids.

3) Ordering of reg-def and reg-use lists.

Should the first entry in the def list be the first def (within a BB)?
Similarly, should the first entry in the use list be the last use
(within a BB)?

4) Working with a sub-CFG.

Often the whole CFG does not need to be analyzed, for example,
when optimizing a loop, only certain registers are of interest.
Perhaps there should be a bitmap argument to df_analyze to specify
which registers should be analyzed?


NOTES:

Embedded addressing side-effects, such as POST_INC or PRE_INC, generate
both a use and a def.  These are both marked read/write to show that they
are dependent. For example, (set (reg 40) (mem (post_inc (reg 42))))
will generate a use of reg 42 followed by a def of reg 42 (both marked
read/write).  Similarly, (set (reg 40) (mem (pre_dec (reg 41))))
generates a use of reg 41 then a def of reg 41 (both marked read/write),
even though reg 41 is decremented before it is used for the memory
address in this second example.

A set to a REG inside a ZERO_EXTRACT, or a set to a non-paradoxical SUBREG
for which the number of word_mode units covered by the outer mode is
smaller than that covered by the inner mode, invokes a read-modify-write.
operation.  We generate both a use and a def and again mark them
read/write.
Paradoxical subreg writes don't leave a trace of the old content, so they
are write-only operations.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "regs.h"
#include "alloc-pool.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "df.h"

#define FOR_EACH_BB_IN_BITMAP(BITMAP, MIN, BB, CODE)	\
  do							\
    {							\
      unsigned int node_;				\
      bitmap_iterator bi;				\
      EXECUTE_IF_SET_IN_BITMAP (BITMAP, MIN, node_, bi)	\
	{						\
	  (BB) = BASIC_BLOCK (node_);			\
	  CODE;						\
	}						\
    }							\
  while (0)

static alloc_pool df_ref_pool;
static alloc_pool df_link_pool;
static struct df *ddf;

static void df_reg_table_realloc (struct df *, int);
static void df_insn_table_realloc (struct df *, unsigned int);
static void df_bb_table_realloc (struct df *, unsigned int);
static void df_bitmaps_alloc (struct df *, bitmap, int);
static void df_bitmaps_free (struct df *, int);
static void df_free (struct df *);
static void df_alloc (struct df *, int);

static rtx df_reg_use_gen (unsigned int);

static inline struct df_link *df_link_create (struct ref *, struct df_link *);
static struct df_link *df_ref_unlink (struct df_link **, struct ref *);
static void df_def_unlink (struct df *, struct ref *);
static void df_use_unlink (struct df *, struct ref *);
static void df_insn_refs_unlink (struct df *, basic_block, rtx);
#if 0
static void df_bb_refs_unlink (struct df *, basic_block);
static void df_refs_unlink (struct df *, bitmap);
#endif

static struct ref *df_ref_create (struct df *, rtx, rtx *, rtx,
				  enum df_ref_type, enum df_ref_flags);
static void df_ref_record_1 (struct df *, rtx, rtx *, rtx, enum df_ref_type,
			     enum df_ref_flags);
static void df_ref_record (struct df *, rtx, rtx *, rtx, enum df_ref_type,
			   enum df_ref_flags);
static void df_def_record_1 (struct df *, rtx, basic_block, rtx);
static void df_defs_record (struct df *, rtx, basic_block, rtx);
static void df_uses_record (struct df *, rtx *, enum df_ref_type,
			    basic_block, rtx, enum df_ref_flags);
static void df_insn_refs_record (struct df *, basic_block, rtx);
static void df_bb_refs_record (struct df *, basic_block);
static void df_refs_record (struct df *, bitmap);

static void df_bb_reg_def_chain_create (struct df *, basic_block);
static void df_reg_def_chain_create (struct df *, bitmap, bool);
static void df_bb_reg_use_chain_create (struct df *, basic_block);
static void df_reg_use_chain_create (struct df *, bitmap, bool);
static void df_bb_du_chain_create (struct df *, basic_block, bitmap);
static void df_du_chain_create (struct df *, bitmap);
static void df_bb_ud_chain_create (struct df *, basic_block);
static void df_ud_chain_create (struct df *, bitmap);
static void df_bb_rd_local_compute (struct df *, basic_block, bitmap);
static void df_rd_local_compute (struct df *, bitmap);
static void df_bb_ru_local_compute (struct df *, basic_block);
static void df_ru_local_compute (struct df *, bitmap);
static void df_bb_lr_local_compute (struct df *, basic_block);
static void df_lr_local_compute (struct df *, bitmap);
static void df_bb_reg_info_compute (struct df *, basic_block, bitmap);
static void df_reg_info_compute (struct df *, bitmap);

static int df_bb_luids_set (struct df *df, basic_block);
static int df_luids_set (struct df *df, bitmap);

static int df_modified_p (struct df *, bitmap);
static int df_refs_queue (struct df *);
static int df_refs_process (struct df *);
static int df_bb_refs_update (struct df *, basic_block);
static int df_refs_update (struct df *, bitmap);
static void df_analyze_1 (struct df *, bitmap, int, int);

static void df_insns_modify (struct df *, basic_block, rtx, rtx);
static int df_rtx_mem_replace (rtx *, void *);
static int df_rtx_reg_replace (rtx *, void *);
void df_refs_reg_replace (struct df *, bitmap, struct df_link *, rtx, rtx);

static int df_def_dominates_all_uses_p (struct df *, struct ref *def);
static int df_def_dominates_uses_p (struct df *, struct ref *def, bitmap);
static struct ref *df_bb_insn_regno_last_use_find (struct df *, basic_block,
						   rtx, unsigned int);
static struct ref *df_bb_insn_regno_first_def_find (struct df *, basic_block,
						    rtx, unsigned int);

static void df_chain_dump (struct df_link *, FILE *file);
static void df_chain_dump_regno (struct df_link *, FILE *file);
static void df_regno_debug (struct df *, unsigned int, FILE *);
static void df_ref_debug (struct df *, struct ref *, FILE *);
static void df_rd_transfer_function (int, int *, void *, void *, void *,
				     void *, void *);
static void df_ru_transfer_function (int, int *, void *, void *, void *,
				     void *, void *);
static void df_lr_transfer_function (int, int *, void *, void *, void *,
				     void *, void *);
static void hybrid_search (basic_block, struct dataflow *,
			   sbitmap, sbitmap, sbitmap);


/* Local memory allocation/deallocation routines.  */


/* Increase the insn info table to have space for at least SIZE + 1
   elements.  */
static void
df_insn_table_realloc (struct df *df, unsigned int size)
{
  size++;
  if (size <= df->insn_size)
    return;

  /* Make the table a little larger than requested, so we do not need
     to enlarge it so often.  */
  size += df->insn_size / 4;

  df->insns = xrealloc (df->insns, size * sizeof (struct insn_info));

  memset (df->insns + df->insn_size, 0,
	  (size - df->insn_size) * sizeof (struct insn_info));

  df->insn_size = size;

  if (! df->insns_modified)
    {
      df->insns_modified = BITMAP_ALLOC (NULL);
      bitmap_zero (df->insns_modified);
    }
}

/* Increase the bb info table to have space for at least SIZE + 1
   elements.  */

static void
df_bb_table_realloc (struct df *df, unsigned int size)
{
  size++;
  if (size <= df->n_bbs)
    return;

  /* Make the table a little larger than requested, so we do not need
     to enlarge it so often.  */
  size += df->n_bbs / 4;

  df->bbs = xrealloc (df->bbs, size * sizeof (struct bb_info));

  memset (df->bbs + df->n_bbs, 0, (size - df->n_bbs) * sizeof (struct bb_info));

  df->n_bbs = size;
}

/* Increase the reg info table by SIZE more elements.  */
static void
df_reg_table_realloc (struct df *df, int size)
{
  /* Make table 25 percent larger by default.  */
  if (! size)
    size = df->reg_size / 4;

  size += df->reg_size;
  if (size < max_reg_num ())
    size = max_reg_num ();

  df->regs = xrealloc (df->regs, size * sizeof (struct reg_info));
  df->reg_def_last = xrealloc (df->reg_def_last,
			       size * sizeof (struct ref *));

  /* Zero the new entries.  */
  memset (df->regs + df->reg_size, 0,
	  (size - df->reg_size) * sizeof (struct reg_info));

  df->reg_size = size;
}


/* Allocate bitmaps for each basic block.  */

static void
df_bitmaps_alloc (struct df *df, bitmap blocks, int flags)
{
  basic_block bb;

  df->n_defs = df->def_id;
  df->n_uses = df->use_id;

  if (!blocks)
    blocks = df->all_blocks;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      struct bb_info *bb_info = DF_BB_INFO (df, bb);

      if (flags & DF_RD)
	{
	  if (!bb_info->rd_in)
	    {
	      /* Allocate bitmaps for reaching definitions.  */
	      bb_info->rd_kill = BITMAP_ALLOC (NULL);
	      bb_info->rd_gen = BITMAP_ALLOC (NULL);
	      bb_info->rd_in = BITMAP_ALLOC (NULL);
	      bb_info->rd_out = BITMAP_ALLOC (NULL);
	    }
	  else
	    {
	      bitmap_clear (bb_info->rd_kill);
	      bitmap_clear (bb_info->rd_gen);
	      bitmap_clear (bb_info->rd_in);
	      bitmap_clear (bb_info->rd_out);
	    }
	}

      if (flags & DF_RU)
	{
	  if (!bb_info->ru_in)
	    {
	      /* Allocate bitmaps for upward exposed uses.  */
	      bb_info->ru_kill = BITMAP_ALLOC (NULL);
	      bb_info->ru_gen = BITMAP_ALLOC (NULL);
	      bb_info->ru_in = BITMAP_ALLOC (NULL);
	      bb_info->ru_out = BITMAP_ALLOC (NULL);
	    }
	  else
	    {
	      bitmap_clear (bb_info->ru_kill);
	      bitmap_clear (bb_info->ru_gen);
	      bitmap_clear (bb_info->ru_in);
	      bitmap_clear (bb_info->ru_out);
	    }
	}

      if (flags & DF_LR)
	{
	  if (!bb_info->lr_in)
	    {
	      /* Allocate bitmaps for live variables.  */
	      bb_info->lr_def = BITMAP_ALLOC (NULL);
	      bb_info->lr_use = BITMAP_ALLOC (NULL);
	      bb_info->lr_in = BITMAP_ALLOC (NULL);
	      bb_info->lr_out = BITMAP_ALLOC (NULL);
	    }
	  else
	    {
	      bitmap_clear (bb_info->lr_def);
	      bitmap_clear (bb_info->lr_use);
	      bitmap_clear (bb_info->lr_in);
	      bitmap_clear (bb_info->lr_out);
	    }
	}
    });
}


/* Free bitmaps for each basic block.  */
static void
df_bitmaps_free (struct df *df, int flags)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      struct bb_info *bb_info = DF_BB_INFO (df, bb);

      if (!bb_info)
	continue;

      if ((flags & DF_RD) && bb_info->rd_in)
	{
	  /* Free bitmaps for reaching definitions.  */
	  BITMAP_FREE (bb_info->rd_kill);
	  bb_info->rd_kill = NULL;
	  BITMAP_FREE (bb_info->rd_gen);
	  bb_info->rd_gen = NULL;
	  BITMAP_FREE (bb_info->rd_in);
	  bb_info->rd_in = NULL;
	  BITMAP_FREE (bb_info->rd_out);
	  bb_info->rd_out = NULL;
	}

      if ((flags & DF_RU) && bb_info->ru_in)
	{
	  /* Free bitmaps for upward exposed uses.  */
	  BITMAP_FREE (bb_info->ru_kill);
	  bb_info->ru_kill = NULL;
	  BITMAP_FREE (bb_info->ru_gen);
	  bb_info->ru_gen = NULL;
	  BITMAP_FREE (bb_info->ru_in);
	  bb_info->ru_in = NULL;
	  BITMAP_FREE (bb_info->ru_out);
	  bb_info->ru_out = NULL;
	}

      if ((flags & DF_LR) && bb_info->lr_in)
	{
	  /* Free bitmaps for live variables.  */
	  BITMAP_FREE (bb_info->lr_def);
	  bb_info->lr_def = NULL;
	  BITMAP_FREE (bb_info->lr_use);
	  bb_info->lr_use = NULL;
	  BITMAP_FREE (bb_info->lr_in);
	  bb_info->lr_in = NULL;
	  BITMAP_FREE (bb_info->lr_out);
	  bb_info->lr_out = NULL;
	}
    }
  df->flags &= ~(flags & (DF_RD | DF_RU | DF_LR));
}


/* Allocate and initialize dataflow memory.  */
static void
df_alloc (struct df *df, int n_regs)
{
  int n_insns;
  basic_block bb;

  df_link_pool = create_alloc_pool ("df_link pool", sizeof (struct df_link),
				    100);
  df_ref_pool  = create_alloc_pool ("df_ref pool", sizeof (struct ref), 100);

  /* Perhaps we should use LUIDs to save memory for the insn_refs
     table.  This is only a small saving; a few pointers.  */
  n_insns = get_max_uid () + 1;

  df->def_id = 0;
  df->n_defs = 0;
  /* Approximate number of defs by number of insns.  */
  df->def_size = n_insns;
  df->defs = xmalloc (df->def_size * sizeof (*df->defs));

  df->use_id = 0;
  df->n_uses = 0;
  /* Approximate number of uses by twice number of insns.  */
  df->use_size = n_insns * 2;
  df->uses = xmalloc (df->use_size * sizeof (*df->uses));

  df->n_regs = n_regs;
  df->n_bbs = last_basic_block;

  /* Allocate temporary working array used during local dataflow analysis.  */
  df_insn_table_realloc (df, n_insns);

  df_reg_table_realloc (df, df->n_regs);

  df->bbs_modified = BITMAP_ALLOC (NULL);
  bitmap_zero (df->bbs_modified);

  df->flags = 0;

  df->bbs = xcalloc (last_basic_block, sizeof (struct bb_info));

  df->all_blocks = BITMAP_ALLOC (NULL);
  FOR_EACH_BB (bb)
    bitmap_set_bit (df->all_blocks, bb->index);
}


/* Free all the dataflow info.  */
static void
df_free (struct df *df)
{
  df_bitmaps_free (df, DF_ALL);

  if (df->bbs)
    free (df->bbs);
  df->bbs = 0;

  if (df->insns)
    free (df->insns);
  df->insns = 0;
  df->insn_size = 0;

  if (df->defs)
    free (df->defs);
  df->defs = 0;
  df->def_size = 0;
  df->def_id = 0;

  if (df->uses)
    free (df->uses);
  df->uses = 0;
  df->use_size = 0;
  df->use_id = 0;

  if (df->regs)
    free (df->regs);
  df->regs = 0;
  df->reg_size = 0;

  BITMAP_FREE (df->bbs_modified);
  df->bbs_modified = 0;

  BITMAP_FREE (df->insns_modified);
  df->insns_modified = 0;

  BITMAP_FREE (df->all_blocks);
  df->all_blocks = 0;

  free_alloc_pool (df_ref_pool);
  free_alloc_pool (df_link_pool);
}

/* Local miscellaneous routines.  */

/* Return a USE for register REGNO.  */
static rtx df_reg_use_gen (unsigned int regno)
{
  rtx reg;
  rtx use;

  reg = regno_reg_rtx[regno];

  use = gen_rtx_USE (GET_MODE (reg), reg);
  return use;
}

/* Local chain manipulation routines.  */

/* Create a link in a def-use or use-def chain.  */
static inline struct df_link *
df_link_create (struct ref *ref, struct df_link *next)
{
  struct df_link *link;

  link = pool_alloc (df_link_pool);
  link->next = next;
  link->ref = ref;
  return link;
}

/* Releases members of the CHAIN.  */

static void
free_reg_ref_chain (struct df_link **chain)
{
  struct df_link *act, *next;

  for (act = *chain; act; act = next)
    {
      next = act->next;
      pool_free (df_link_pool, act);
    }

  *chain = NULL;
}

/* Add REF to chain head pointed to by PHEAD.  */
static struct df_link *
df_ref_unlink (struct df_link **phead, struct ref *ref)
{
  struct df_link *link = *phead;

  if (link)
    {
      if (! link->next)
	{
	  /* Only a single ref.  It must be the one we want.
	     If not, the def-use and use-def chains are likely to
	     be inconsistent.  */
	  gcc_assert (link->ref == ref);
	  
	  /* Now have an empty chain.  */
	  *phead = NULL;
	}
      else
	{
	  /* Multiple refs.  One of them must be us.  */
	  if (link->ref == ref)
	    *phead = link->next;
	  else
	    {
	      /* Follow chain.  */
	      for (; link->next; link = link->next)
		{
		  if (link->next->ref == ref)
		    {
		      /* Unlink from list.  */
		      link->next = link->next->next;
		      return link->next;
		    }
		}
	    }
	}
    }
  return link;
}


/* Unlink REF from all def-use/use-def chains, etc.  */
int
df_ref_remove (struct df *df, struct ref *ref)
{
  if (DF_REF_REG_DEF_P (ref))
    {
      df_def_unlink (df, ref);
      df_ref_unlink (&df->insns[DF_REF_INSN_UID (ref)].defs, ref);
    }
  else
    {
      df_use_unlink (df, ref);
      df_ref_unlink (&df->insns[DF_REF_INSN_UID (ref)].uses, ref);
    }
  return 1;
}


/* Unlink DEF from use-def and reg-def chains.  */
static void
df_def_unlink (struct df *df ATTRIBUTE_UNUSED, struct ref *def)
{
  struct df_link *du_link;
  unsigned int dregno = DF_REF_REGNO (def);

  /* Follow def-use chain to find all the uses of this def.  */
  for (du_link = DF_REF_CHAIN (def); du_link; du_link = du_link->next)
    {
      struct ref *use = du_link->ref;

      /* Unlink this def from the use-def chain.  */
      df_ref_unlink (&DF_REF_CHAIN (use), def);
    }
  DF_REF_CHAIN (def) = 0;

  /* Unlink def from reg-def chain.  */
  df_ref_unlink (&df->regs[dregno].defs, def);

  df->defs[DF_REF_ID (def)] = 0;
}


/* Unlink use from def-use and reg-use chains.  */
static void
df_use_unlink (struct df *df ATTRIBUTE_UNUSED, struct ref *use)
{
  struct df_link *ud_link;
  unsigned int uregno = DF_REF_REGNO (use);

  /* Follow use-def chain to find all the defs of this use.  */
  for (ud_link = DF_REF_CHAIN (use); ud_link; ud_link = ud_link->next)
    {
      struct ref *def = ud_link->ref;

      /* Unlink this use from the def-use chain.  */
      df_ref_unlink (&DF_REF_CHAIN (def), use);
    }
  DF_REF_CHAIN (use) = 0;

  /* Unlink use from reg-use chain.  */
  df_ref_unlink (&df->regs[uregno].uses, use);

  df->uses[DF_REF_ID (use)] = 0;
}

/* Local routines for recording refs.  */


/* Create a new ref of type DF_REF_TYPE for register REG at address
   LOC within INSN of BB.  */
static struct ref *
df_ref_create (struct df *df, rtx reg, rtx *loc, rtx insn,
	       enum df_ref_type ref_type, enum df_ref_flags ref_flags)
{
  struct ref *this_ref;

  this_ref = pool_alloc (df_ref_pool);
  DF_REF_REG (this_ref) = reg;
  DF_REF_LOC (this_ref) = loc;
  DF_REF_INSN (this_ref) = insn;
  DF_REF_CHAIN (this_ref) = 0;
  DF_REF_TYPE (this_ref) = ref_type;
  DF_REF_FLAGS (this_ref) = ref_flags;
  DF_REF_DATA (this_ref) = NULL;

  if (ref_type == DF_REF_REG_DEF)
    {
      if (df->def_id >= df->def_size)
	{
	  /* Make table 25 percent larger.  */
	  df->def_size += (df->def_size / 4);
	  df->defs = xrealloc (df->defs,
			       df->def_size * sizeof (*df->defs));
	}
      DF_REF_ID (this_ref) = df->def_id;
      df->defs[df->def_id++] = this_ref;
    }
  else
    {
      if (df->use_id >= df->use_size)
	{
	  /* Make table 25 percent larger.  */
	  df->use_size += (df->use_size / 4);
	  df->uses = xrealloc (df->uses,
			       df->use_size * sizeof (*df->uses));
	}
      DF_REF_ID (this_ref) = df->use_id;
      df->uses[df->use_id++] = this_ref;
    }
  return this_ref;
}


/* Create a new reference of type DF_REF_TYPE for a single register REG,
   used inside the LOC rtx of INSN.  */
static void
df_ref_record_1 (struct df *df, rtx reg, rtx *loc, rtx insn,
		 enum df_ref_type ref_type, enum df_ref_flags ref_flags)
{
  df_ref_create (df, reg, loc, insn, ref_type, ref_flags);
}


/* Create new references of type DF_REF_TYPE for each part of register REG
   at address LOC within INSN of BB.  */
static void
df_ref_record (struct df *df, rtx reg, rtx *loc, rtx insn,
	       enum df_ref_type ref_type, enum df_ref_flags ref_flags)
{
  unsigned int regno;

  gcc_assert (REG_P (reg) || GET_CODE (reg) == SUBREG);

  /* For the reg allocator we are interested in some SUBREG rtx's, but not
     all.  Notably only those representing a word extraction from a multi-word
     reg.  As written in the docu those should have the form
     (subreg:SI (reg:M A) N), with size(SImode) > size(Mmode).
     XXX Is that true?  We could also use the global word_mode variable.  */
  if ((df->flags & DF_SUBREGS) == 0
      && GET_CODE (reg) == SUBREG
      && (GET_MODE_SIZE (GET_MODE (reg)) < GET_MODE_SIZE (word_mode)
	  || GET_MODE_SIZE (GET_MODE (reg))
	       >= GET_MODE_SIZE (GET_MODE (SUBREG_REG (reg)))))
    {
      loc = &SUBREG_REG (reg);
      reg = *loc;
      ref_flags |= DF_REF_STRIPPED;
    }

  regno = REGNO (GET_CODE (reg) == SUBREG ? SUBREG_REG (reg) : reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int i;
      int endregno;

      if (! (df->flags & DF_HARD_REGS))
	return;

      /* GET_MODE (reg) is correct here.  We do not want to go into a SUBREG
         for the mode, because we only want to add references to regs, which
	 are really referenced.  E.g., a (subreg:SI (reg:DI 0) 0) does _not_
	 reference the whole reg 0 in DI mode (which would also include
	 reg 1, at least, if 0 and 1 are SImode registers).  */
      endregno = hard_regno_nregs[regno][GET_MODE (reg)];
      if (GET_CODE (reg) == SUBREG)
        regno += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
				      SUBREG_BYTE (reg), GET_MODE (reg));
      endregno += regno;

      for (i = regno; i < endregno; i++)
	df_ref_record_1 (df, regno_reg_rtx[i],
			 loc, insn, ref_type, ref_flags);
    }
  else
    {
      df_ref_record_1 (df, reg, loc, insn, ref_type, ref_flags);
    }
}


/* A set to a non-paradoxical SUBREG for which the number of word_mode units
   covered by the outer mode is smaller than that covered by the inner mode,
   is a read-modify-write operation.
   This function returns true iff the SUBREG X is such a SUBREG.  */
bool
read_modify_subreg_p (rtx x)
{
  unsigned int isize, osize;
  if (GET_CODE (x) != SUBREG)
    return false;
  isize = GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)));
  osize = GET_MODE_SIZE (GET_MODE (x));
  return (isize > osize && isize > UNITS_PER_WORD);
}


/* Process all the registers defined in the rtx, X.  */
static void
df_def_record_1 (struct df *df, rtx x, basic_block bb, rtx insn)
{
  rtx *loc;
  rtx dst;
  enum df_ref_flags flags = 0;

 /* We may recursively call ourselves on EXPR_LIST when dealing with PARALLEL
     construct.  */
  if (GET_CODE (x) == EXPR_LIST || GET_CODE (x) == CLOBBER)
    loc = &XEXP (x, 0);
  else
    loc = &SET_DEST (x);
  dst = *loc;

  /* Some targets place small structures in registers for
     return values of functions.  */
  if (GET_CODE (dst) == PARALLEL && GET_MODE (dst) == BLKmode)
    {
      int i;

      for (i = XVECLEN (dst, 0) - 1; i >= 0; i--)
	{
	  rtx temp = XVECEXP (dst, 0, i);
	  if (GET_CODE (temp) == EXPR_LIST || GET_CODE (temp) == CLOBBER
	      || GET_CODE (temp) == SET)
	    df_def_record_1 (df, temp, bb, insn);
	}
      return;
    }

  /* Maybe, we should flag the use of STRICT_LOW_PART somehow.  It might
     be handy for the reg allocator.  */
  while (GET_CODE (dst) == STRICT_LOW_PART
	 || GET_CODE (dst) == ZERO_EXTRACT
	 || read_modify_subreg_p (dst))
    {
      /* Strict low part always contains SUBREG, but we do not want to make
	 it appear outside, as whole register is always considered.  */
      if (GET_CODE (dst) == STRICT_LOW_PART)
	{
	  loc = &XEXP (dst, 0);
	  dst = *loc;
	}
      loc = &XEXP (dst, 0);
      dst = *loc;
      flags |= DF_REF_READ_WRITE;
    }

  if (REG_P (dst)
      || (GET_CODE (dst) == SUBREG && REG_P (SUBREG_REG (dst))))
    df_ref_record (df, dst, loc, insn, DF_REF_REG_DEF, flags);
}


/* Process all the registers defined in the pattern rtx, X.  */
static void
df_defs_record (struct df *df, rtx x, basic_block bb, rtx insn)
{
  RTX_CODE code = GET_CODE (x);

  if (code == SET || code == CLOBBER)
    {
      /* Mark the single def within the pattern.  */
      df_def_record_1 (df, x, bb, insn);
    }
  else if (code == PARALLEL)
    {
      int i;

      /* Mark the multiple defs within the pattern.  */
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  code = GET_CODE (XVECEXP (x, 0, i));
	  if (code == SET || code == CLOBBER)
	    df_def_record_1 (df, XVECEXP (x, 0, i), bb, insn);
	}
    }
}


/* Process all the registers used in the rtx at address LOC.  */
static void
df_uses_record (struct df *df, rtx *loc, enum df_ref_type ref_type,
		basic_block bb, rtx insn, enum df_ref_flags flags)
{
  RTX_CODE code;
  rtx x;
 retry:
  x = *loc;
  if (!x)
    return;
  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case CLOBBER:
      /* If we are clobbering a MEM, mark any registers inside the address
	 as being used.  */
      if (MEM_P (XEXP (x, 0)))
	df_uses_record (df, &XEXP (XEXP (x, 0), 0),
			DF_REF_REG_MEM_STORE, bb, insn, flags);

      /* If we're clobbering a REG then we have a def so ignore.  */
      return;

    case MEM:
      df_uses_record (df, &XEXP (x, 0), DF_REF_REG_MEM_LOAD, bb, insn, 0);
      return;

    case SUBREG:
      /* While we're here, optimize this case.  */

      /* In case the SUBREG is not of a REG, do not optimize.  */
      if (!REG_P (SUBREG_REG (x)))
	{
	  loc = &SUBREG_REG (x);
	  df_uses_record (df, loc, ref_type, bb, insn, flags);
	  return;
	}
      /* ... Fall through ...  */

    case REG:
      df_ref_record (df, x, loc, insn, ref_type, flags);
      return;

    case SET:
      {
	rtx dst = SET_DEST (x);

	df_uses_record (df, &SET_SRC (x), DF_REF_REG_USE, bb, insn, 0);

	switch (GET_CODE (dst))
	  {
	    case SUBREG:
	      if (read_modify_subreg_p (dst))
		{
		  df_uses_record (df, &SUBREG_REG (dst), DF_REF_REG_USE, bb,
				  insn, DF_REF_READ_WRITE);
		  break;
		}
	      /* Fall through.  */
	    case REG:
	    case PARALLEL:
	    case PC:
	    case CC0:
		break;
	    case MEM:
	      df_uses_record (df, &XEXP (dst, 0),
			      DF_REF_REG_MEM_STORE,
			      bb, insn, 0);
	      break;
	    case STRICT_LOW_PART:
	      /* A strict_low_part uses the whole REG and not just the
		 SUBREG.  */
	      dst = XEXP (dst, 0);
	      gcc_assert (GET_CODE (dst) == SUBREG);
	      df_uses_record (df, &SUBREG_REG (dst), DF_REF_REG_USE, bb,
			     insn, DF_REF_READ_WRITE);
	      break;
	    case ZERO_EXTRACT:
	    case SIGN_EXTRACT:
	      df_uses_record (df, &XEXP (dst, 0), DF_REF_REG_USE, bb, insn,
			      DF_REF_READ_WRITE);
	      df_uses_record (df, &XEXP (dst, 1), DF_REF_REG_USE, bb, insn, 0);
	      df_uses_record (df, &XEXP (dst, 2), DF_REF_REG_USE, bb, insn, 0);
	      dst = XEXP (dst, 0);
	      break;
	    default:
	      gcc_unreachable ();
	  }
	return;
      }

    case RETURN:
      break;

    case ASM_OPERANDS:
    case UNSPEC_VOLATILE:
    case TRAP_IF:
    case ASM_INPUT:
      {
	/* Traditional and volatile asm instructions must be considered to use
	   and clobber all hard registers, all pseudo-registers and all of
	   memory.  So must TRAP_IF and UNSPEC_VOLATILE operations.

	   Consider for instance a volatile asm that changes the fpu rounding
	   mode.  An insn should not be moved across this even if it only uses
	   pseudo-regs because it might give an incorrectly rounded result.

	   For now, just mark any regs we can find in ASM_OPERANDS as
	   used.  */

	/* For all ASM_OPERANDS, we must traverse the vector of input operands.
	   We can not just fall through here since then we would be confused
	   by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	   traditional asms unlike their normal usage.  */
	if (code == ASM_OPERANDS)
	  {
	    int j;

	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      df_uses_record (df, &ASM_OPERANDS_INPUT (x, j),
			      DF_REF_REG_USE, bb, insn, 0);
	    return;
	  }
	break;
      }

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
    case PRE_MODIFY:
    case POST_MODIFY:
      /* Catch the def of the register being modified.  */
      df_ref_record (df, XEXP (x, 0), &XEXP (x, 0), insn, DF_REF_REG_DEF, DF_REF_READ_WRITE);

      /* ... Fall through to handle uses ...  */

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */
  {
    const char *fmt = GET_RTX_FORMAT (code);
    int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    /* Tail recursive case: save a function call level.  */
	    if (i == 0)
	      {
		loc = &XEXP (x, 0);
		goto retry;
	      }
	    df_uses_record (df, &XEXP (x, i), ref_type, bb, insn, flags);
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      df_uses_record (df, &XVECEXP (x, i, j), ref_type,
			      bb, insn, flags);
	  }
      }
  }
}


/* Record all the df within INSN of basic block BB.  */
static void
df_insn_refs_record (struct df *df, basic_block bb, rtx insn)
{
  int i;

  if (INSN_P (insn))
    {
      rtx note;

      /* Record register defs.  */
      df_defs_record (df, PATTERN (insn), bb, insn);

      if (df->flags & DF_EQUIV_NOTES)
	for (note = REG_NOTES (insn); note;
	     note = XEXP (note, 1))
	  {
	    switch (REG_NOTE_KIND (note))
	      {
	      case REG_EQUIV:
	      case REG_EQUAL:
		df_uses_record (df, &XEXP (note, 0), DF_REF_REG_USE,
				bb, insn, 0);
	      default:
		break;
	      }
	  }

      if (CALL_P (insn))
	{
	  rtx note;
	  rtx x;

	  /* Record the registers used to pass arguments.  */
	  for (note = CALL_INSN_FUNCTION_USAGE (insn); note;
	       note = XEXP (note, 1))
	    {
	      if (GET_CODE (XEXP (note, 0)) == USE)
		df_uses_record (df, &XEXP (XEXP (note, 0), 0), DF_REF_REG_USE,
				bb, insn, 0);
	    }

	  /* The stack ptr is used (honorarily) by a CALL insn.  */
	  x = df_reg_use_gen (STACK_POINTER_REGNUM);
	  df_uses_record (df, &XEXP (x, 0), DF_REF_REG_USE, bb, insn, 0);

	  if (df->flags & DF_HARD_REGS)
	    {
	      /* Calls may also reference any of the global registers,
		 so they are recorded as used.  */
	      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
		if (global_regs[i])
		  {
		    x = df_reg_use_gen (i);
		    df_uses_record (df, &XEXP (x, 0),
				    DF_REF_REG_USE, bb, insn, 0);
		  }
	    }
	}

      /* Record the register uses.  */
      df_uses_record (df, &PATTERN (insn),
		      DF_REF_REG_USE, bb, insn, 0);

      if (CALL_P (insn))
	{
	  rtx note;

	  /* We do not record hard registers clobbered by the call,
	     since there are awfully many of them and "defs" created
	     through them are not interesting (since no use can be legally
	     reached by them).  So we must just make sure we include them when
	     computing kill bitmaps.  */

	  /* There may be extra registers to be clobbered.  */
	  for (note = CALL_INSN_FUNCTION_USAGE (insn);
	       note;
	       note = XEXP (note, 1))
	    if (GET_CODE (XEXP (note, 0)) == CLOBBER)
	      df_defs_record (df, XEXP (note, 0), bb, insn);
	}
    }
}


/* Record all the refs within the basic block BB.  */
static void
df_bb_refs_record (struct df *df, basic_block bb)
{
  rtx insn;

  /* Scan the block an insn at a time from beginning to end.  */
  FOR_BB_INSNS (bb, insn)
    {
      if (INSN_P (insn))
	{
	  /* Record defs within INSN.  */
	  df_insn_refs_record (df, bb, insn);
	}
    }
}


/* Record all the refs in the basic blocks specified by BLOCKS.  */
static void
df_refs_record (struct df *df, bitmap blocks)
{
  basic_block bb;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      df_bb_refs_record (df, bb);
    });
}

/* Dataflow analysis routines.  */

/* Create reg-def chains for basic block BB.  These are a list of
   definitions for each register.  */

static void
df_bb_reg_def_chain_create (struct df *df, basic_block bb)
{
  rtx insn;

  /* Perhaps the defs should be sorted using a depth first search
     of the CFG (or possibly a breadth first search).  */

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      struct df_link *link;
      unsigned int uid = INSN_UID (insn);

      if (! INSN_P (insn))
	continue;

      for (link = df->insns[uid].defs; link; link = link->next)
	{
	  struct ref *def = link->ref;
	  unsigned int dregno = DF_REF_REGNO (def);

          /* Do not add ref's to the chain twice, i.e., only add new
             refs.  XXX the same could be done by testing if the
             current insn is a modified (or a new) one.  This would be
             faster.  */
          if (DF_REF_ID (def) < df->def_id_save)
            continue;

	  df->regs[dregno].defs = df_link_create (def, df->regs[dregno].defs);
	}
    }
}


/* Create reg-def chains for each basic block within BLOCKS.  These
   are a list of definitions for each register.  If REDO is true, add
   all defs, otherwise just add the new defs.  */

static void
df_reg_def_chain_create (struct df *df, bitmap blocks, bool redo)
{
  basic_block bb;
#ifdef ENABLE_CHECKING
  unsigned regno;
#endif
  unsigned old_def_id_save = df->def_id_save;

  if (redo)
    {
#ifdef ENABLE_CHECKING
      for (regno = 0; regno < df->n_regs; regno++)
	gcc_assert (!df->regs[regno].defs);
#endif

      /* Pretend that all defs are new.  */
      df->def_id_save = 0;
    }

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      df_bb_reg_def_chain_create (df, bb);
    });

  df->def_id_save = old_def_id_save;
}

/* Remove all reg-def chains stored in the dataflow object DF.  */

static void
df_reg_def_chain_clean (struct df *df)
{
  unsigned regno;

  for (regno = 0; regno < df->n_regs; regno++)
    free_reg_ref_chain (&df->regs[regno].defs);
}

/* Create reg-use chains for basic block BB.  These are a list of uses
   for each register.  */

static void
df_bb_reg_use_chain_create (struct df *df, basic_block bb)
{
  rtx insn;

  /* Scan in forward order so that the last uses appear at the start
     of the chain.  */

  FOR_BB_INSNS (bb, insn)
    {
      struct df_link *link;
      unsigned int uid = INSN_UID (insn);

      if (! INSN_P (insn))
	continue;

      for (link = df->insns[uid].uses; link; link = link->next)
	{
	  struct ref *use = link->ref;
	  unsigned int uregno = DF_REF_REGNO (use);

          /* Do not add ref's to the chain twice, i.e., only add new
             refs.  XXX the same could be done by testing if the
             current insn is a modified (or a new) one.  This would be
             faster.  */
          if (DF_REF_ID (use) < df->use_id_save)
            continue;

	  df->regs[uregno].uses
	    = df_link_create (use, df->regs[uregno].uses);
	}
    }
}


/* Create reg-use chains for each basic block within BLOCKS.  These
   are a list of uses for each register.  If REDO is true, remove the
   old reg-use chains first, otherwise just add new uses to them.  */

static void
df_reg_use_chain_create (struct df *df, bitmap blocks, bool redo)
{
  basic_block bb;
#ifdef ENABLE_CHECKING
  unsigned regno;
#endif
  unsigned old_use_id_save = df->use_id_save;

  if (redo)
    {
#ifdef ENABLE_CHECKING
      for (regno = 0; regno < df->n_regs; regno++)
	gcc_assert (!df->regs[regno].uses);
#endif

      /* Pretend that all uses are new.  */
      df->use_id_save = 0;
    }

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      df_bb_reg_use_chain_create (df, bb);
    });

  df->use_id_save = old_use_id_save;
}

/* Remove all reg-use chains stored in the dataflow object DF.  */

static void
df_reg_use_chain_clean (struct df *df)
{
  unsigned regno;

  for (regno = 0; regno < df->n_regs; regno++)
    free_reg_ref_chain (&df->regs[regno].uses);
}

/* Create def-use chains from reaching use bitmaps for basic block BB.  */
static void
df_bb_du_chain_create (struct df *df, basic_block bb, bitmap ru)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  rtx insn;

  bitmap_copy (ru, bb_info->ru_out);

  /* For each def in BB create a linked list (chain) of uses
     reached from the def.  */
  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      struct df_link *def_link;
      struct df_link *use_link;
      unsigned int uid = INSN_UID (insn);

      if (! INSN_P (insn))
	continue;

      /* For each def in insn...  */
      for (def_link = df->insns[uid].defs; def_link; def_link = def_link->next)
	{
	  struct ref *def = def_link->ref;
	  unsigned int dregno = DF_REF_REGNO (def);

	  DF_REF_CHAIN (def) = 0;

	  /* While the reg-use chains are not essential, it
	     is _much_ faster to search these short lists rather
	     than all the reaching uses, especially for large functions.  */
	  for (use_link = df->regs[dregno].uses; use_link;
	       use_link = use_link->next)
	    {
	      struct ref *use = use_link->ref;

	      if (bitmap_bit_p (ru, DF_REF_ID (use)))
		{
		  DF_REF_CHAIN (def)
		    = df_link_create (use, DF_REF_CHAIN (def));

		  bitmap_clear_bit (ru, DF_REF_ID (use));
		}
	    }
	}

      /* For each use in insn...  */
      for (use_link = df->insns[uid].uses; use_link; use_link = use_link->next)
	{
	  struct ref *use = use_link->ref;
	  bitmap_set_bit (ru, DF_REF_ID (use));
	}
    }
}


/* Create def-use chains from reaching use bitmaps for basic blocks
   in BLOCKS.  */
static void
df_du_chain_create (struct df *df, bitmap blocks)
{
  bitmap ru;
  basic_block bb;

  ru = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      df_bb_du_chain_create (df, bb, ru);
    });

  BITMAP_FREE (ru);
}


/* Create use-def chains from reaching def bitmaps for basic block BB.  */
static void
df_bb_ud_chain_create (struct df *df, basic_block bb)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  struct ref **reg_def_last = df->reg_def_last;
  rtx insn;

  memset (reg_def_last, 0, df->n_regs * sizeof (struct ref *));

  /* For each use in BB create a linked list (chain) of defs
     that reach the use.  */
  FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_link *use_link;
      struct df_link *def_link;

      if (! INSN_P (insn))
	continue;

      /* For each use in insn...  */
      for (use_link = df->insns[uid].uses; use_link; use_link = use_link->next)
	{
	  struct ref *use = use_link->ref;
	  unsigned int regno = DF_REF_REGNO (use);

	  DF_REF_CHAIN (use) = 0;

	  /* Has regno been defined in this BB yet?  If so, use
	     the last def as the single entry for the use-def
	     chain for this use.  Otherwise, we need to add all
	     the defs using this regno that reach the start of
	     this BB.  */
	  if (reg_def_last[regno])
	    {
	      DF_REF_CHAIN (use)
		= df_link_create (reg_def_last[regno], 0);
	    }
	  else
	    {
	      /* While the reg-def chains are not essential, it is
		 _much_ faster to search these short lists rather than
		 all the reaching defs, especially for large
		 functions.  */
	      for (def_link = df->regs[regno].defs; def_link;
		   def_link = def_link->next)
		{
		  struct ref *def = def_link->ref;

		  if (bitmap_bit_p (bb_info->rd_in, DF_REF_ID (def)))
		    {
		      DF_REF_CHAIN (use)
			= df_link_create (def, DF_REF_CHAIN (use));
		    }
		}
	    }
	}


      /* For each def in insn... record the last def of each reg.  */
      for (def_link = df->insns[uid].defs; def_link; def_link = def_link->next)
	{
	  struct ref *def = def_link->ref;
	  int dregno = DF_REF_REGNO (def);

	  reg_def_last[dregno] = def;
	}
    }
}


/* Create use-def chains from reaching def bitmaps for basic blocks
   within BLOCKS.  */
static void
df_ud_chain_create (struct df *df, bitmap blocks)
{
  basic_block bb;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      df_bb_ud_chain_create (df, bb);
    });
}



static void
df_rd_transfer_function (int bb ATTRIBUTE_UNUSED, int *changed, void *in,
			 void *out, void *gen, void *kill,
			 void *data ATTRIBUTE_UNUSED)
{
  *changed = bitmap_ior_and_compl (out, gen, in, kill);
}


static void
df_ru_transfer_function (int bb ATTRIBUTE_UNUSED, int *changed, void *in,
			 void *out, void *gen, void *kill,
			 void *data ATTRIBUTE_UNUSED)
{
  *changed = bitmap_ior_and_compl (in, gen, out, kill);
}


static void
df_lr_transfer_function (int bb ATTRIBUTE_UNUSED, int *changed, void *in,
			 void *out, void *use, void *def,
			 void *data ATTRIBUTE_UNUSED)
{
  *changed = bitmap_ior_and_compl (in, use, out, def);
}


/* Compute local reaching def info for basic block BB.  */
static void
df_bb_rd_local_compute (struct df *df, basic_block bb, bitmap call_killed_defs)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  rtx insn;
  bitmap seen = BITMAP_ALLOC (NULL);
  bool call_seen = false;

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_link *def_link;

      if (! INSN_P (insn))
	continue;

      for (def_link = df->insns[uid].defs; def_link; def_link = def_link->next)
	{
	  struct ref *def = def_link->ref;
	  unsigned int regno = DF_REF_REGNO (def);
	  struct df_link *def2_link;

	  if (bitmap_bit_p (seen, regno)
	      || (call_seen
		  && regno < FIRST_PSEUDO_REGISTER
		  && TEST_HARD_REG_BIT (regs_invalidated_by_call, regno)))
	    continue;

	  for (def2_link = df->regs[regno].defs; def2_link;
	       def2_link = def2_link->next)
	    {
	      struct ref *def2 = def2_link->ref;

	      /* Add all defs of this reg to the set of kills.  This
		 is greedy since many of these defs will not actually
		 be killed by this BB but it keeps things a lot
		 simpler.  */
	      bitmap_set_bit (bb_info->rd_kill, DF_REF_ID (def2));
	    }

	  bitmap_set_bit (bb_info->rd_gen, DF_REF_ID (def));
	  bitmap_set_bit (seen, regno);
	}

      if (CALL_P (insn) && (df->flags & DF_HARD_REGS))
	{
	  bitmap_ior_into (bb_info->rd_kill, call_killed_defs);
	  call_seen = 1;
	}
    }

  BITMAP_FREE (seen);
}


/* Compute local reaching def info for each basic block within BLOCKS.  */
static void
df_rd_local_compute (struct df *df, bitmap blocks)
{
  basic_block bb;
  bitmap killed_by_call = NULL;
  unsigned regno;
  struct df_link *def_link;

  if (df->flags & DF_HARD_REGS)
    {
      killed_by_call = BITMAP_ALLOC (NULL);
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if (!TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
	    continue;
	  
	  for (def_link = df->regs[regno].defs;
	       def_link;
	       def_link = def_link->next)
	    bitmap_set_bit (killed_by_call, DF_REF_ID (def_link->ref));
	}
    }

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
  {
    df_bb_rd_local_compute (df, bb, killed_by_call);
  });

  if (df->flags & DF_HARD_REGS)
    BITMAP_FREE (killed_by_call);
}


/* Compute local reaching use (upward exposed use) info for basic
   block BB.  */
static void
df_bb_ru_local_compute (struct df *df, basic_block bb)
{
  /* This is much more tricky than computing reaching defs.  With
     reaching defs, defs get killed by other defs.  With upwards
     exposed uses, these get killed by defs with the same regno.  */

  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  rtx insn;


  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_link *def_link;
      struct df_link *use_link;

      if (! INSN_P (insn))
	continue;

      for (def_link = df->insns[uid].defs; def_link; def_link = def_link->next)
	{
	  struct ref *def = def_link->ref;
	  unsigned int dregno = DF_REF_REGNO (def);

	  for (use_link = df->regs[dregno].uses; use_link;
	       use_link = use_link->next)
	    {
	      struct ref *use = use_link->ref;

	      /* Add all uses of this reg to the set of kills.  This
		 is greedy since many of these uses will not actually
		 be killed by this BB but it keeps things a lot
		 simpler.  */
	      bitmap_set_bit (bb_info->ru_kill, DF_REF_ID (use));

	      /* Zap from the set of gens for this BB.  */
	      bitmap_clear_bit (bb_info->ru_gen, DF_REF_ID (use));
	    }
	}

      for (use_link = df->insns[uid].uses; use_link; use_link = use_link->next)
	{
	  struct ref *use = use_link->ref;
	  /* Add use to set of gens in this BB.  */
	  bitmap_set_bit (bb_info->ru_gen, DF_REF_ID (use));
	}
    }
}


/* Compute local reaching use (upward exposed use) info for each basic
   block within BLOCKS.  */
static void
df_ru_local_compute (struct df *df, bitmap blocks)
{
  basic_block bb;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
  {
    df_bb_ru_local_compute (df, bb);
  });
}


/* Compute local live variable info for basic block BB.  */
static void
df_bb_lr_local_compute (struct df *df, basic_block bb)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  rtx insn;

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_link *link;

      if (! INSN_P (insn))
	continue;

      for (link = df->insns[uid].defs; link; link = link->next)
	{
	  struct ref *def = link->ref;
	  unsigned int dregno = DF_REF_REGNO (def);

	  /* Add def to set of defs in this BB.  */
	  bitmap_set_bit (bb_info->lr_def, dregno);

	  bitmap_clear_bit (bb_info->lr_use, dregno);
	}

      for (link = df->insns[uid].uses; link; link = link->next)
	{
	  struct ref *use = link->ref;
	  /* Add use to set of uses in this BB.  */
	  bitmap_set_bit (bb_info->lr_use, DF_REF_REGNO (use));
	}
    }
}


/* Compute local live variable info for each basic block within BLOCKS.  */
static void
df_lr_local_compute (struct df *df, bitmap blocks)
{
  basic_block bb;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
  {
    df_bb_lr_local_compute (df, bb);
  });
}


/* Compute register info: lifetime, bb, and number of defs and uses
   for basic block BB.  */
static void
df_bb_reg_info_compute (struct df *df, basic_block bb, bitmap live)
{
  struct reg_info *reg_info = df->regs;
  struct bb_info *bb_info = DF_BB_INFO (df, bb);
  rtx insn;

  bitmap_copy (live, bb_info->lr_out);

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      unsigned int regno;
      struct df_link *link;
      bitmap_iterator bi;

      if (! INSN_P (insn))
	continue;

      for (link = df->insns[uid].defs; link; link = link->next)
	{
	  struct ref *def = link->ref;
	  unsigned int dregno = DF_REF_REGNO (def);

	  /* Kill this register.  */
	  bitmap_clear_bit (live, dregno);
	  reg_info[dregno].n_defs++;
	}

      for (link = df->insns[uid].uses; link; link = link->next)
	{
	  struct ref *use = link->ref;
	  unsigned int uregno = DF_REF_REGNO (use);

	  /* This register is now live.  */
	  bitmap_set_bit (live, uregno);
	  reg_info[uregno].n_uses++;
	}

      /* Increment lifetimes of all live registers.  */
      EXECUTE_IF_SET_IN_BITMAP (live, 0, regno, bi)
	{
	  reg_info[regno].lifetime++;
	}
    }
}


/* Compute register info: lifetime, bb, and number of defs and uses.  */
static void
df_reg_info_compute (struct df *df, bitmap blocks)
{
  basic_block bb;
  bitmap live;

  live = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
  {
    df_bb_reg_info_compute (df, bb, live);
  });

  BITMAP_FREE (live);
}


/* Assign LUIDs for BB.  */
static int
df_bb_luids_set (struct df *df, basic_block bb)
{
  rtx insn;
  int luid = 0;

  /* The LUIDs are monotonically increasing for each basic block.  */

  FOR_BB_INSNS (bb, insn)
    {
      if (INSN_P (insn))
	DF_INSN_LUID (df, insn) = luid++;
      DF_INSN_LUID (df, insn) = luid;
    }
  return luid;
}


/* Assign LUIDs for each basic block within BLOCKS.  */
static int
df_luids_set (struct df *df, bitmap blocks)
{
  basic_block bb;
  int total = 0;

  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      total += df_bb_luids_set (df, bb);
    });
  return total;
}


/* Perform dataflow analysis using existing DF structure for blocks
   within BLOCKS.  If BLOCKS is zero, use all basic blocks in the CFG.  */
static void
df_analyze_1 (struct df *df, bitmap blocks, int flags, int update)
{
  int aflags;
  int dflags;
  int i;
  basic_block bb;
  struct dataflow dflow;

  dflags = 0;
  aflags = flags;
  if (flags & DF_UD_CHAIN)
    aflags |= DF_RD | DF_RD_CHAIN;

  if (flags & DF_DU_CHAIN)
    aflags |= DF_RU;

  if (flags & DF_RU)
    aflags |= DF_RU_CHAIN;

  if (flags & DF_REG_INFO)
    aflags |= DF_LR;

  if (! blocks)
    blocks = df->all_blocks;

  df->flags = flags;
  if (update)
    {
      df_refs_update (df, NULL);
      /* More fine grained incremental dataflow analysis would be
	 nice.  For now recompute the whole shebang for the
	 modified blocks.  */
#if 0
      df_refs_unlink (df, blocks);
#endif
      /* All the def-use, use-def chains can be potentially
	 modified by changes in one block.  The size of the
	 bitmaps can also change.  */
    }
  else
    {
      /* Scan the function for all register defs and uses.  */
      df_refs_queue (df);
      df_refs_record (df, blocks);

      /* Link all the new defs and uses to the insns.  */
      df_refs_process (df);
    }

  /* Allocate the bitmaps now the total number of defs and uses are
     known.  If the number of defs or uses have changed, then
     these bitmaps need to be reallocated.  */
  df_bitmaps_alloc (df, NULL, aflags);

  /* Set the LUIDs for each specified basic block.  */
  df_luids_set (df, blocks);

  /* Recreate reg-def and reg-use chains from scratch so that first
     def is at the head of the reg-def chain and the last use is at
     the head of the reg-use chain.  This is only important for
     regs local to a basic block as it speeds up searching.  */
  if (aflags & DF_RD_CHAIN)
    {
      df_reg_def_chain_create (df, blocks, false);
    }

  if (aflags & DF_RU_CHAIN)
    {
      df_reg_use_chain_create (df, blocks, false);
    }

  df->dfs_order = xmalloc (sizeof (int) * n_basic_blocks);
  df->rc_order = xmalloc (sizeof (int) * n_basic_blocks);
  df->rts_order = xmalloc (sizeof (int) * n_basic_blocks);
  df->inverse_dfs_map = xmalloc (sizeof (int) * last_basic_block);
  df->inverse_rc_map = xmalloc (sizeof (int) * last_basic_block);
  df->inverse_rts_map = xmalloc (sizeof (int) * last_basic_block);

  flow_depth_first_order_compute (df->dfs_order, df->rc_order);
  flow_reverse_top_sort_order_compute (df->rts_order);
  for (i = 0; i < n_basic_blocks; i++)
    {
      df->inverse_dfs_map[df->dfs_order[i]] = i;
      df->inverse_rc_map[df->rc_order[i]] = i;
      df->inverse_rts_map[df->rts_order[i]] = i;
    }
  if (aflags & DF_RD)
    {
      /* Compute the sets of gens and kills for the defs of each bb.  */
      dflow.in = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.out = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.gen = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.kill = xmalloc (sizeof (bitmap) * last_basic_block);

      df_rd_local_compute (df, df->flags & DF_RD ? blocks : df->all_blocks);
      FOR_EACH_BB (bb)
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->rd_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->rd_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->rd_gen;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->rd_kill;
	}

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_FORWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_rd_transfer_function;
      dflow.n_blocks = n_basic_blocks;
      dflow.order = df->rc_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
      free (dflow.in);
      free (dflow.out);
      free (dflow.gen);
      free (dflow.kill);
    }

  if (aflags & DF_UD_CHAIN)
    {
      /* Create use-def chains.  */
      df_ud_chain_create (df, df->all_blocks);

      if (! (flags & DF_RD))
	dflags |= DF_RD;
    }

  if (aflags & DF_RU)
    {
      /* Compute the sets of gens and kills for the upwards exposed
	 uses in each bb.  */
      dflow.in = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.out = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.gen = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.kill = xmalloc (sizeof (bitmap) * last_basic_block);

      df_ru_local_compute (df, df->flags & DF_RU ? blocks : df->all_blocks);

      FOR_EACH_BB (bb)
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->ru_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->ru_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->ru_gen;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->ru_kill;
	}

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_BACKWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_ru_transfer_function;
      dflow.n_blocks = n_basic_blocks;
      dflow.order = df->rts_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
      free (dflow.in);
      free (dflow.out);
      free (dflow.gen);
      free (dflow.kill);
    }

  if (aflags & DF_DU_CHAIN)
    {
      /* Create def-use chains.  */
      df_du_chain_create (df, df->all_blocks);

      if (! (flags & DF_RU))
	dflags |= DF_RU;
    }

  /* Free up bitmaps that are no longer required.  */
  if (dflags)
    df_bitmaps_free (df, dflags);

  if (aflags & DF_LR)
    {
      /* Compute the sets of defs and uses of live variables.  */
      dflow.in = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.out = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.gen = xmalloc (sizeof (bitmap) * last_basic_block);
      dflow.kill = xmalloc (sizeof (bitmap) * last_basic_block);

      df_lr_local_compute (df, df->flags & DF_LR ? blocks : df->all_blocks);

      FOR_EACH_BB (bb)
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->lr_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->lr_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->lr_use;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->lr_def;
	}

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_BACKWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_lr_transfer_function;
      dflow.n_blocks = n_basic_blocks;
      dflow.order = df->rts_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
      free (dflow.in);
      free (dflow.out);
      free (dflow.gen);
      free (dflow.kill);
    }

  if (aflags & DF_REG_INFO)
    {
      df_reg_info_compute (df, df->all_blocks);
    }

  free (df->dfs_order);
  free (df->rc_order);
  free (df->rts_order);
  free (df->inverse_rc_map);
  free (df->inverse_dfs_map);
  free (df->inverse_rts_map);
}


/* Initialize dataflow analysis.  */
struct df *
df_init (void)
{
  struct df *df;

  df = xcalloc (1, sizeof (struct df));

  /* Squirrel away a global for debugging.  */
  ddf = df;

  return df;
}


/* Start queuing refs.  */
static int
df_refs_queue (struct df *df)
{
  df->def_id_save = df->def_id;
  df->use_id_save = df->use_id;
  /* ???? Perhaps we should save current obstack state so that we can
     unwind it.  */
  return 0;
}


/* Process queued refs.  */
static int
df_refs_process (struct df *df)
{
  unsigned int i;

  /* Build new insn-def chains.  */
  for (i = df->def_id_save; i != df->def_id; i++)
    {
      struct ref *def = df->defs[i];
      unsigned int uid = DF_REF_INSN_UID (def);

      /* Add def to head of def list for INSN.  */
      df->insns[uid].defs
	= df_link_create (def, df->insns[uid].defs);
    }

  /* Build new insn-use chains.  */
  for (i = df->use_id_save; i != df->use_id; i++)
    {
      struct ref *use = df->uses[i];
      unsigned int uid = DF_REF_INSN_UID (use);

      /* Add use to head of use list for INSN.  */
      df->insns[uid].uses
	= df_link_create (use, df->insns[uid].uses);
    }
  return 0;
}


/* Update refs for basic block BB.  */
static int
df_bb_refs_update (struct df *df, basic_block bb)
{
  rtx insn;
  int count = 0;

  /* While we have to scan the chain of insns for this BB, we do not
     need to allocate and queue a long chain of BB/INSN pairs.  Using
     a bitmap for insns_modified saves memory and avoids queuing
     duplicates.  */

  FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid;

      uid = INSN_UID (insn);

      if (bitmap_bit_p (df->insns_modified, uid))
	{
	  /* Delete any allocated refs of this insn.  MPH,  FIXME.  */
	  df_insn_refs_unlink (df, bb, insn);

	  /* Scan the insn for refs.  */
	  df_insn_refs_record (df, bb, insn);

	  count++;
	}
    }
  return count;
}


/* Process all the modified/deleted insns that were queued.  */
static int
df_refs_update (struct df *df, bitmap blocks)
{
  basic_block bb;
  unsigned count = 0, bbno;

  df->n_regs = max_reg_num ();
  if (df->n_regs >= df->reg_size)
    df_reg_table_realloc (df, 0);

  df_refs_queue (df);

  if (!blocks)
    {
      FOR_EACH_BB_IN_BITMAP (df->bbs_modified, 0, bb,
	{
	  count += df_bb_refs_update (df, bb);
	});
    }
  else
    {
      bitmap_iterator bi;

      EXECUTE_IF_AND_IN_BITMAP (df->bbs_modified, blocks, 0, bbno, bi)
	{
	  count += df_bb_refs_update (df, BASIC_BLOCK (bbno));
	}
    }

  df_refs_process (df);
  return count;
}


/* Return nonzero if any of the requested blocks in the bitmap
   BLOCKS have been modified.  */
static int
df_modified_p (struct df *df, bitmap blocks)
{
  int update = 0;
  basic_block bb;

  if (!df->n_bbs)
    return 0;

  FOR_EACH_BB (bb)
    if (bitmap_bit_p (df->bbs_modified, bb->index)
	&& (! blocks || (blocks == (bitmap) -1) || bitmap_bit_p (blocks, bb->index)))
    {
      update = 1;
      break;
    }

  return update;
}

/* Analyze dataflow info for the basic blocks specified by the bitmap
   BLOCKS, or for the whole CFG if BLOCKS is zero, or just for the
   modified blocks if BLOCKS is -1.  */

int
df_analyze (struct df *df, bitmap blocks, int flags)
{
  int update;

  /* We could deal with additional basic blocks being created by
     rescanning everything again.  */
  gcc_assert (!df->n_bbs || df->n_bbs == (unsigned int) last_basic_block);

  update = df_modified_p (df, blocks);
  if (update || (flags != df->flags))
    {
      if (! blocks)
	{
	  if (df->n_bbs)
	    {
	      /* Recompute everything from scratch.  */
	      df_free (df);
	    }
	  /* Allocate and initialize data structures.  */
	  df_alloc (df, max_reg_num ());
	  df_analyze_1 (df, 0, flags, 0);
	  update = 1;
	}
      else
	{
	  if (blocks == (bitmap) -1)
	    blocks = df->bbs_modified;

	  gcc_assert (df->n_bbs);

	  df_analyze_1 (df, blocks, flags, 1);
	  bitmap_zero (df->bbs_modified);
	  bitmap_zero (df->insns_modified);
	}
    }
  return update;
}

/* Remove the entries not in BLOCKS from the LIST of length LEN, preserving
   the order of the remaining entries.  Returns the length of the resulting
   list.  */

static unsigned
prune_to_subcfg (int list[], unsigned len, bitmap blocks)
{
  unsigned act, last;

  for (act = 0, last = 0; act < len; act++)
    if (bitmap_bit_p (blocks, list[act]))
      list[last++] = list[act];

  return last;
}

/* Alternative entry point to the analysis.  Analyze just the part of the cfg
   graph induced by BLOCKS.
   
   TODO I am not quite sure how to avoid code duplication with df_analyze_1
   here, and simultaneously not make even greater chaos in it.  We behave
   slightly differently in some details, especially in handling modified
   insns.  */

void
df_analyze_subcfg (struct df *df, bitmap blocks, int flags)
{
  rtx insn;
  basic_block bb;
  struct dataflow dflow;
  unsigned n_blocks;

  if (flags & DF_UD_CHAIN)
    flags |= DF_RD | DF_RD_CHAIN;
  if (flags & DF_DU_CHAIN)
    flags |= DF_RU;
  if (flags & DF_RU)
    flags |= DF_RU_CHAIN;
  if (flags & DF_REG_INFO)
    flags |= DF_LR;

  if (!df->n_bbs)
    {
      df_alloc (df, max_reg_num ());

      /* Mark all insns as modified.  */

      FOR_EACH_BB (bb)
	{
	  FOR_BB_INSNS (bb, insn)
	    {
	      df_insn_modify (df, bb, insn);
	    }
	}
    }
  
  df->flags = flags;

  df_reg_def_chain_clean (df);
  df_reg_use_chain_clean (df);

  df_refs_update (df, blocks);

  /* Clear the updated stuff from ``modified'' bitmaps.  */
  FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
    {
      if (bitmap_bit_p (df->bbs_modified, bb->index))
	{
	  FOR_BB_INSNS (bb, insn)
	    {
	      bitmap_clear_bit (df->insns_modified, INSN_UID (insn));
	    }

	  bitmap_clear_bit (df->bbs_modified, bb->index);
	}
    });

  /* Allocate the bitmaps now the total number of defs and uses are
     known.  If the number of defs or uses have changed, then
     these bitmaps need to be reallocated.  */
  df_bitmaps_alloc (df, blocks, flags);

  /* Set the LUIDs for each specified basic block.  */
  df_luids_set (df, blocks);

  /* Recreate reg-def and reg-use chains from scratch so that first
     def is at the head of the reg-def chain and the last use is at
     the head of the reg-use chain.  This is only important for
     regs local to a basic block as it speeds up searching.  */
  if (flags & DF_RD_CHAIN)
    {
      df_reg_def_chain_create (df, blocks, true);
    }

  if (flags & DF_RU_CHAIN)
    {
      df_reg_use_chain_create (df, blocks, true);
    }

  df->dfs_order = xmalloc (sizeof (int) * n_basic_blocks);
  df->rc_order = xmalloc (sizeof (int) * n_basic_blocks);
  df->rts_order = xmalloc (sizeof (int) * n_basic_blocks);

  flow_depth_first_order_compute (df->dfs_order, df->rc_order);
  flow_reverse_top_sort_order_compute (df->rts_order);

  n_blocks = prune_to_subcfg (df->dfs_order, n_basic_blocks, blocks);
  prune_to_subcfg (df->rc_order, n_basic_blocks, blocks);
  prune_to_subcfg (df->rts_order, n_basic_blocks, blocks);

  dflow.in = xmalloc (sizeof (bitmap) * last_basic_block);
  dflow.out = xmalloc (sizeof (bitmap) * last_basic_block);
  dflow.gen = xmalloc (sizeof (bitmap) * last_basic_block);
  dflow.kill = xmalloc (sizeof (bitmap) * last_basic_block);

  if (flags & DF_RD)
    {
      /* Compute the sets of gens and kills for the defs of each bb.  */
      df_rd_local_compute (df, blocks);

      FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->rd_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->rd_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->rd_gen;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->rd_kill;
	});

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_FORWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_rd_transfer_function;
      dflow.n_blocks = n_blocks;
      dflow.order = df->rc_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
    }

  if (flags & DF_UD_CHAIN)
    {
      /* Create use-def chains.  */
      df_ud_chain_create (df, blocks);
    }

  if (flags & DF_RU)
    {
      /* Compute the sets of gens and kills for the upwards exposed
	 uses in each bb.  */
      df_ru_local_compute (df, blocks);

      FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->ru_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->ru_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->ru_gen;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->ru_kill;
	});

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_BACKWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_ru_transfer_function;
      dflow.n_blocks = n_blocks;
      dflow.order = df->rts_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
    }

  if (flags & DF_DU_CHAIN)
    {
      /* Create def-use chains.  */
      df_du_chain_create (df, blocks);
    }

  if (flags & DF_LR)
    {
      /* Compute the sets of defs and uses of live variables.  */
      df_lr_local_compute (df, blocks);

      FOR_EACH_BB (bb)
	{
	  dflow.in[bb->index] = DF_BB_INFO (df, bb)->lr_in;
	  dflow.out[bb->index] = DF_BB_INFO (df, bb)->lr_out;
	  dflow.gen[bb->index] = DF_BB_INFO (df, bb)->lr_use;
	  dflow.kill[bb->index] = DF_BB_INFO (df, bb)->lr_def;
	}

      dflow.repr = SR_BITMAP;
      dflow.dir = DF_BACKWARD;
      dflow.conf_op = DF_UNION;
      dflow.transfun = df_lr_transfer_function;
      dflow.n_blocks = n_blocks;
      dflow.order = df->rts_order;
      dflow.data = NULL;

      iterative_dataflow (&dflow);
    }

  if (flags & DF_REG_INFO)
    {
      df_reg_info_compute (df, blocks);
    }

  free (dflow.in);
  free (dflow.out);
  free (dflow.gen);
  free (dflow.kill);

  free (df->dfs_order);
  free (df->rc_order);
  free (df->rts_order);
}

/* Free all the dataflow info and the DF structure.  */
void
df_finish (struct df *df)
{
  df_free (df);
  free (df);
}

/* Unlink INSN from its reference information.  */
static void
df_insn_refs_unlink (struct df *df, basic_block bb ATTRIBUTE_UNUSED, rtx insn)
{
  struct df_link *link;
  unsigned int uid;

  uid = INSN_UID (insn);

  /* Unlink all refs defined by this insn.  */
  for (link = df->insns[uid].defs; link; link = link->next)
    df_def_unlink (df, link->ref);

  /* Unlink all refs used by this insn.  */
  for (link = df->insns[uid].uses; link; link = link->next)
    df_use_unlink (df, link->ref);

  df->insns[uid].defs = 0;
  df->insns[uid].uses = 0;
}


#if 0
/* Unlink all the insns within BB from their reference information.  */
static void
df_bb_refs_unlink (struct df *df, basic_block bb)
{
  rtx insn;

  /* Scan the block an insn at a time from beginning to end.  */
  for (insn = BB_HEAD (bb); ; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  /* Unlink refs for INSN.  */
	  df_insn_refs_unlink (df, bb, insn);
	}
      if (insn == BB_END (bb))
	break;
    }
}


/* Unlink all the refs in the basic blocks specified by BLOCKS.
   Not currently used.  */
static void
df_refs_unlink (struct df *df, bitmap blocks)
{
  basic_block bb;

  if (blocks)
    {
      FOR_EACH_BB_IN_BITMAP (blocks, 0, bb,
      {
	df_bb_refs_unlink (df, bb);
      });
    }
  else
    {
      FOR_EACH_BB (bb)
	df_bb_refs_unlink (df, bb);
    }
}
#endif

/* Functions to modify insns.  */


/* Delete INSN and all its reference information.  */
rtx
df_insn_delete (struct df *df, basic_block bb ATTRIBUTE_UNUSED, rtx insn)
{
  /* If the insn is a jump, we should perhaps call delete_insn to
     handle the JUMP_LABEL?  */

  /* We should not be deleting the NOTE_INSN_BASIC_BLOCK or label.  */
  gcc_assert (insn != BB_HEAD (bb));

  /* Delete the insn.  */
  delete_insn (insn);

  df_insn_modify (df, bb, insn);

  return NEXT_INSN (insn);
}

/* Mark that basic block BB was modified.  */

static void
df_bb_modify (struct df *df, basic_block bb)
{
  if ((unsigned) bb->index >= df->n_bbs)
    df_bb_table_realloc (df, df->n_bbs);

  bitmap_set_bit (df->bbs_modified, bb->index);
}

/* Mark that INSN within BB may have changed  (created/modified/deleted).
   This may be called multiple times for the same insn.  There is no
   harm calling this function if the insn wasn't changed; it will just
   slow down the rescanning of refs.  */
void
df_insn_modify (struct df *df, basic_block bb, rtx insn)
{
  unsigned int uid;

  uid = INSN_UID (insn);
  if (uid >= df->insn_size)
    df_insn_table_realloc (df, uid);

  df_bb_modify (df, bb);
  bitmap_set_bit (df->insns_modified, uid);

  /* For incremental updating on the fly, perhaps we could make a copy
     of all the refs of the original insn and turn them into
     anti-refs.  When df_refs_update finds these anti-refs, it annihilates
     the original refs.  If validate_change fails then these anti-refs
     will just get ignored.  */
}

/* Check if INSN was marked as changed.  Of course the correctness of
   the information depends on whether the instruction was really modified
   at the time df_insn_modify was called.  */
bool
df_insn_modified_p (struct df *df, rtx insn)
{
  unsigned int uid;

  uid = INSN_UID (insn);
  return (df->insns_modified
	  && uid < df->insn_size
          && bitmap_bit_p (df->insns_modified, uid));
}

typedef struct replace_args
{
  rtx match;
  rtx replacement;
  rtx insn;
  int modified;
} replace_args;


/* Replace mem pointed to by PX with its associated pseudo register.
   DATA is actually a pointer to a structure describing the
   instruction currently being scanned and the MEM we are currently
   replacing.  */
static int
df_rtx_mem_replace (rtx *px, void *data)
{
  replace_args *args = (replace_args *) data;
  rtx mem = *px;

  if (mem == NULL_RTX)
    return 0;

  switch (GET_CODE (mem))
    {
    case MEM:
      break;

    case CONST_DOUBLE:
      /* We're not interested in the MEM associated with a
	 CONST_DOUBLE, so there's no need to traverse into one.  */
      return -1;

    default:
      /* This is not a MEM.  */
      return 0;
    }

  if (!rtx_equal_p (args->match, mem))
    /* This is not the MEM we are currently replacing.  */
    return 0;

  /* Actually replace the MEM.  */
  validate_change (args->insn, px, args->replacement, 1);
  args->modified++;

  return 0;
}


int
df_insn_mem_replace (struct df *df, basic_block bb, rtx insn, rtx mem, rtx reg)
{
  replace_args args;

  args.insn = insn;
  args.match = mem;
  args.replacement = reg;
  args.modified = 0;

  /* Search and replace all matching mems within insn.  */
  for_each_rtx (&insn, df_rtx_mem_replace, &args);

  if (args.modified)
    df_insn_modify (df, bb, insn);

  /* ???? FIXME.  We may have a new def or one or more new uses of REG
     in INSN.  REG should be a new pseudo so it won't affect the
     dataflow information that we currently have.  We should add
     the new uses and defs to INSN and then recreate the chains
     when df_analyze is called.  */
  return args.modified;
}


/* Replace one register with another.  Called through for_each_rtx; PX
   points to the rtx being scanned.  DATA is actually a pointer to a
   structure of arguments.  */
static int
df_rtx_reg_replace (rtx *px, void *data)
{
  rtx x = *px;
  replace_args *args = (replace_args *) data;

  if (x == NULL_RTX)
    return 0;

  if (x == args->match)
    {
      validate_change (args->insn, px, args->replacement, 1);
      args->modified++;
    }

  return 0;
}


/* Replace the reg within every ref on CHAIN that is within the set
   BLOCKS of basic blocks with NEWREG.  Also update the regs within
   REG_NOTES.  */
void
df_refs_reg_replace (struct df *df, bitmap blocks, struct df_link *chain, rtx oldreg, rtx newreg)
{
  struct df_link *link;
  replace_args args;

  if (! blocks)
    blocks = df->all_blocks;

  args.match = oldreg;
  args.replacement = newreg;
  args.modified = 0;

  for (link = chain; link; link = link->next)
    {
      struct ref *ref = link->ref;
      rtx insn = DF_REF_INSN (ref);

      if (! INSN_P (insn))
	continue;

      gcc_assert (bitmap_bit_p (blocks, DF_REF_BBNO (ref)));
      
      df_ref_reg_replace (df, ref, oldreg, newreg);

      /* Replace occurrences of the reg within the REG_NOTES.  */
      if ((! link->next || DF_REF_INSN (ref)
	   != DF_REF_INSN (link->next->ref))
	  && REG_NOTES (insn))
	{
	  args.insn = insn;
	  for_each_rtx (&REG_NOTES (insn), df_rtx_reg_replace, &args);
	}
    }
}


/* Replace all occurrences of register OLDREG with register NEWREG in
   blocks defined by bitmap BLOCKS.  This also replaces occurrences of
   OLDREG in the REG_NOTES but only for insns containing OLDREG.  This
   routine expects the reg-use and reg-def chains to be valid.  */
int
df_reg_replace (struct df *df, bitmap blocks, rtx oldreg, rtx newreg)
{
  unsigned int oldregno = REGNO (oldreg);

  df_refs_reg_replace (df, blocks, df->regs[oldregno].defs, oldreg, newreg);
  df_refs_reg_replace (df, blocks, df->regs[oldregno].uses, oldreg, newreg);
  return 1;
}


/* Try replacing the reg within REF with NEWREG.  Do not modify
   def-use/use-def chains.  */
int
df_ref_reg_replace (struct df *df, struct ref *ref, rtx oldreg, rtx newreg)
{
  /* Check that insn was deleted by being converted into a NOTE.  If
   so ignore this insn.  */
  if (! INSN_P (DF_REF_INSN (ref)))
    return 0;

  gcc_assert (!oldreg || oldreg == DF_REF_REG (ref));

  if (! validate_change (DF_REF_INSN (ref), DF_REF_LOC (ref), newreg, 1))
    return 0;

  df_insn_modify (df, DF_REF_BB (ref), DF_REF_INSN (ref));
  return 1;
}


struct ref*
df_bb_def_use_swap (struct df *df, basic_block bb, rtx def_insn, rtx use_insn, unsigned int regno)
{
  struct ref *def;
  struct ref *use;
  int def_uid;
  int use_uid;
  struct df_link *link;

  def = df_bb_insn_regno_first_def_find (df, bb, def_insn, regno);
  if (! def)
    return 0;

  use = df_bb_insn_regno_last_use_find (df, bb, use_insn, regno);
  if (! use)
    return 0;

  /* The USE no longer exists.  */
  use_uid = INSN_UID (use_insn);
  df_use_unlink (df, use);
  df_ref_unlink (&df->insns[use_uid].uses, use);

  /* The DEF requires shifting so remove it from DEF_INSN
     and add it to USE_INSN by reusing LINK.  */
  def_uid = INSN_UID (def_insn);
  link = df_ref_unlink (&df->insns[def_uid].defs, def);
  link->ref = def;
  link->next = df->insns[use_uid].defs;
  df->insns[use_uid].defs = link;

#if 0
  link = df_ref_unlink (&df->regs[regno].defs, def);
  link->ref = def;
  link->next = df->regs[regno].defs;
  df->insns[regno].defs = link;
#endif

  DF_REF_INSN (def) = use_insn;
  return def;
}


/* Record df between FIRST_INSN and LAST_INSN inclusive.  All new
   insns must be processed by this routine.  */
static void
df_insns_modify (struct df *df, basic_block bb, rtx first_insn, rtx last_insn)
{
  rtx insn;

  for (insn = first_insn; ; insn = NEXT_INSN (insn))
    {
      unsigned int uid;

      /* A non-const call should not have slipped through the net.  If
	 it does, we need to create a new basic block.  Ouch.  The
	 same applies for a label.  */
      gcc_assert ((!CALL_P (insn) || CONST_OR_PURE_CALL_P (insn))
		  && !LABEL_P (insn));

      uid = INSN_UID (insn);

      if (uid >= df->insn_size)
	df_insn_table_realloc (df, uid);

      df_insn_modify (df, bb, insn);

      if (insn == last_insn)
	break;
    }
}


/* Emit PATTERN before INSN within BB.  */
rtx
df_pattern_emit_before (struct df *df, rtx pattern, basic_block bb, rtx insn)
{
  rtx ret_insn;
  rtx prev_insn = PREV_INSN (insn);

  /* We should not be inserting before the start of the block.  */
  gcc_assert (insn != BB_HEAD (bb));
  ret_insn = emit_insn_before (pattern, insn);
  if (ret_insn == insn)
    return ret_insn;

  df_insns_modify (df, bb, NEXT_INSN (prev_insn), ret_insn);
  return ret_insn;
}


/* Emit PATTERN after INSN within BB.  */
rtx
df_pattern_emit_after (struct df *df, rtx pattern, basic_block bb, rtx insn)
{
  rtx ret_insn;

  ret_insn = emit_insn_after (pattern, insn);
  if (ret_insn == insn)
    return ret_insn;

  df_insns_modify (df, bb, NEXT_INSN (insn), ret_insn);
  return ret_insn;
}


/* Emit jump PATTERN after INSN within BB.  */
rtx
df_jump_pattern_emit_after (struct df *df, rtx pattern, basic_block bb, rtx insn)
{
  rtx ret_insn;

  ret_insn = emit_jump_insn_after (pattern, insn);
  if (ret_insn == insn)
    return ret_insn;

  df_insns_modify (df, bb, NEXT_INSN (insn), ret_insn);
  return ret_insn;
}


/* Move INSN within BB before BEFORE_INSN within BEFORE_BB.

   This function should only be used to move loop invariant insns
   out of a loop where it has been proven that the def-use info
   will still be valid.  */
rtx
df_insn_move_before (struct df *df, basic_block bb, rtx insn, basic_block before_bb, rtx before_insn)
{
  struct df_link *link;
  unsigned int uid;

  if (! bb)
    return df_pattern_emit_before (df, insn, before_bb, before_insn);

  uid = INSN_UID (insn);

  /* Change bb for all df defined and used by this insn.  */
  for (link = df->insns[uid].defs; link; link = link->next)
    DF_REF_BB (link->ref) = before_bb;
  for (link = df->insns[uid].uses; link; link = link->next)
    DF_REF_BB (link->ref) = before_bb;

  /* The lifetimes of the registers used in this insn will be reduced
     while the lifetimes of the registers defined in this insn
     are likely to be increased.  */

  /* ???? Perhaps all the insns moved should be stored on a list
     which df_analyze removes when it recalculates data flow.  */

  return emit_insn_before (insn, before_insn);
}

/* Functions to query dataflow information.  */


int
df_insn_regno_def_p (struct df *df, basic_block bb ATTRIBUTE_UNUSED,
		     rtx insn, unsigned int regno)
{
  unsigned int uid;
  struct df_link *link;

  uid = INSN_UID (insn);

  for (link = df->insns[uid].defs; link; link = link->next)
    {
      struct ref *def = link->ref;

      if (DF_REF_REGNO (def) == regno)
	return 1;
    }

  return 0;
}

/* Finds the reference corresponding to the definition of REG in INSN.
   DF is the dataflow object.  */

struct ref *
df_find_def (struct df *df, rtx insn, rtx reg)
{
  struct df_link *defs;

  for (defs = DF_INSN_DEFS (df, insn); defs; defs = defs->next)
    if (rtx_equal_p (DF_REF_REG (defs->ref), reg))
      return defs->ref;

  return NULL;
}

/* Return 1 if REG is referenced in INSN, zero otherwise.  */ 

int
df_reg_used (struct df *df, rtx insn, rtx reg)
{
  struct df_link *uses;

  for (uses = DF_INSN_USES (df, insn); uses; uses = uses->next)
    if (rtx_equal_p (DF_REF_REG (uses->ref), reg))
      return 1; 

  return 0;
}

static int
df_def_dominates_all_uses_p (struct df *df ATTRIBUTE_UNUSED, struct ref *def)
{
  struct df_link *du_link;

  /* Follow def-use chain to find all the uses of this def.  */
  for (du_link = DF_REF_CHAIN (def); du_link; du_link = du_link->next)
    {
      struct ref *use = du_link->ref;
      struct df_link *ud_link;

      /* Follow use-def chain to check all the defs for this use.  */
      for (ud_link = DF_REF_CHAIN (use); ud_link; ud_link = ud_link->next)
	if (ud_link->ref != def)
	  return 0;
    }
  return 1;
}


int
df_insn_dominates_all_uses_p (struct df *df, basic_block bb ATTRIBUTE_UNUSED,
			      rtx insn)
{
  unsigned int uid;
  struct df_link *link;

  uid = INSN_UID (insn);

  for (link = df->insns[uid].defs; link; link = link->next)
    {
      struct ref *def = link->ref;

      if (! df_def_dominates_all_uses_p (df, def))
	return 0;
    }

  return 1;
}


/* Return nonzero if all DF dominates all the uses within the bitmap
   BLOCKS.  */
static int
df_def_dominates_uses_p (struct df *df ATTRIBUTE_UNUSED, struct ref *def,
			 bitmap blocks)
{
  struct df_link *du_link;

  /* Follow def-use chain to find all the uses of this def.  */
  for (du_link = DF_REF_CHAIN (def); du_link; du_link = du_link->next)
    {
      struct ref *use = du_link->ref;
      struct df_link *ud_link;

      /* Only worry about the uses within BLOCKS.  For example,
      consider a register defined within a loop that is live at the
      loop exits.  */
      if (bitmap_bit_p (blocks, DF_REF_BBNO (use)))
	{
	  /* Follow use-def chain to check all the defs for this use.  */
	  for (ud_link = DF_REF_CHAIN (use); ud_link; ud_link = ud_link->next)
	    if (ud_link->ref != def)
	      return 0;
	}
    }
  return 1;
}


/* Return nonzero if all the defs of INSN within BB dominates
   all the corresponding uses.  */
int
df_insn_dominates_uses_p (struct df *df, basic_block bb ATTRIBUTE_UNUSED,
			  rtx insn, bitmap blocks)
{
  unsigned int uid;
  struct df_link *link;

  uid = INSN_UID (insn);

  for (link = df->insns[uid].defs; link; link = link->next)
    {
      struct ref *def = link->ref;

      /* Only consider the defs within BLOCKS.  */
      if (bitmap_bit_p (blocks, DF_REF_BBNO (def))
	  && ! df_def_dominates_uses_p (df, def, blocks))
	return 0;
    }
  return 1;
}


/* Return the basic block that REG referenced in or NULL if referenced
   in multiple basic blocks.  */
basic_block
df_regno_bb (struct df *df, unsigned int regno)
{
  struct df_link *defs = df->regs[regno].defs;
  struct df_link *uses = df->regs[regno].uses;
  struct ref *def = defs ? defs->ref : 0;
  struct ref *use = uses ? uses->ref : 0;
  basic_block bb_def = def ? DF_REF_BB (def) : 0;
  basic_block bb_use = use ? DF_REF_BB (use) : 0;

  /* Compare blocks of first def and last use.  ???? FIXME.  What if
     the reg-def and reg-use lists are not correctly ordered.  */
  return bb_def == bb_use ? bb_def : 0;
}


/* Return nonzero if REG used in multiple basic blocks.  */
int
df_reg_global_p (struct df *df, rtx reg)
{
  return df_regno_bb (df, REGNO (reg)) != 0;
}


/* Return total lifetime (in insns) of REG.  */
int
df_reg_lifetime (struct df *df, rtx reg)
{
  return df->regs[REGNO (reg)].lifetime;
}


/* Return nonzero if REG live at start of BB.  */
int
df_bb_reg_live_start_p (struct df *df, basic_block bb, rtx reg)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);

  gcc_assert (bb_info->lr_in);

  return bitmap_bit_p (bb_info->lr_in, REGNO (reg));
}


/* Return nonzero if REG live at end of BB.  */
int
df_bb_reg_live_end_p (struct df *df, basic_block bb, rtx reg)
{
  struct bb_info *bb_info = DF_BB_INFO (df, bb);

  gcc_assert (bb_info->lr_in);

  return bitmap_bit_p (bb_info->lr_out, REGNO (reg));
}


/* Return -1 if life of REG1 before life of REG2, 1 if life of REG1
   after life of REG2, or 0, if the lives overlap.  */
int
df_bb_regs_lives_compare (struct df *df, basic_block bb, rtx reg1, rtx reg2)
{
  unsigned int regno1 = REGNO (reg1);
  unsigned int regno2 = REGNO (reg2);
  struct ref *def1;
  struct ref *use1;
  struct ref *def2;
  struct ref *use2;


  /* The regs must be local to BB.  */
  gcc_assert (df_regno_bb (df, regno1) == bb
	      && df_regno_bb (df, regno2) == bb);

  def2 = df_bb_regno_first_def_find (df, bb, regno2);
  use1 = df_bb_regno_last_use_find (df, bb, regno1);

  if (DF_INSN_LUID (df, DF_REF_INSN (def2))
      > DF_INSN_LUID (df, DF_REF_INSN (use1)))
    return -1;

  def1 = df_bb_regno_first_def_find (df, bb, regno1);
  use2 = df_bb_regno_last_use_find (df, bb, regno2);

  if (DF_INSN_LUID (df, DF_REF_INSN (def1))
      > DF_INSN_LUID (df, DF_REF_INSN (use2)))
    return 1;

  return 0;
}


/* Return true if the definition DEF, which is in the same basic
   block as USE, is available at USE.  So DEF may as well be
   dead, in which case using it will extend its live range.  */
bool
df_local_def_available_p (struct df *df, struct ref *def, struct ref *use)
{
  struct df_link *link;
  int def_luid = DF_INSN_LUID (df, DF_REF_INSN (def));
  int in_bb = 0;
  unsigned int regno = REGNO (def->reg);
  basic_block bb;

  /* The regs must be local to BB.  */
  gcc_assert (DF_REF_BB (def) == DF_REF_BB (use));
  bb = DF_REF_BB (def);

  /* This assumes that the reg-def list is ordered such that for any
     BB, the first def is found first.  However, since the BBs are not
     ordered, the first def in the chain is not necessarily the first
     def in the function.  */
  for (link = df->regs[regno].defs; link; link = link->next)
    {
      struct ref *this_def = link->ref;
      if (DF_REF_BB (this_def) == bb)
	{
	  int this_luid = DF_INSN_LUID (df, DF_REF_INSN (this_def));
	  /* Do nothing with defs coming before DEF.  */
	  if (this_luid > def_luid)
	    return this_luid > DF_INSN_LUID (df, DF_REF_INSN (use));

	  in_bb = 1;
        }
      else if (in_bb)
	/* DEF was the last in its basic block.  */
        return 1;
    }

  /* DEF was the last in the function.  */
  return 1;
}


/* Return last use of REGNO within BB.  */
struct ref *
df_bb_regno_last_use_find (struct df *df, basic_block bb, unsigned int regno)
{
  struct df_link *link;

  /* This assumes that the reg-use list is ordered such that for any
     BB, the last use is found first.  However, since the BBs are not
     ordered, the first use in the chain is not necessarily the last
     use in the function.  */
  for (link = df->regs[regno].uses; link; link = link->next)
    {
      struct ref *use = link->ref;

      if (DF_REF_BB (use) == bb)
	return use;
    }
  return 0;
}


/* Return first def of REGNO within BB.  */
struct ref *
df_bb_regno_first_def_find (struct df *df, basic_block bb, unsigned int regno)
{
  struct df_link *link;

  /* This assumes that the reg-def list is ordered such that for any
     BB, the first def is found first.  However, since the BBs are not
     ordered, the first def in the chain is not necessarily the first
     def in the function.  */
  for (link = df->regs[regno].defs; link; link = link->next)
    {
      struct ref *def = link->ref;

      if (DF_REF_BB (def) == bb)
	return def;
    }
  return 0;
}

/* Return last def of REGNO within BB.  */
struct ref *
df_bb_regno_last_def_find (struct df *df, basic_block bb, unsigned int regno)
{
  struct df_link *link;
  struct ref *last_def = NULL;
  int in_bb = 0;

  /* This assumes that the reg-def list is ordered such that for any
     BB, the first def is found first.  However, since the BBs are not
     ordered, the first def in the chain is not necessarily the first
     def in the function.  */
  for (link = df->regs[regno].defs; link; link = link->next)
    {
      struct ref *def = link->ref;
      /* The first time in the desired block.  */ 
      if (DF_REF_BB (def) == bb)
	  in_bb = 1;
      /* The last def in the desired block.  */
      else if (in_bb)
        return last_def;
      last_def = def;
    }
  return last_def;
}

/* Return last use of REGNO inside INSN within BB.  */
static struct ref *
df_bb_insn_regno_last_use_find (struct df *df,
				basic_block bb ATTRIBUTE_UNUSED, rtx insn,
				unsigned int regno)
{
  unsigned int uid;
  struct df_link *link;

  uid = INSN_UID (insn);

  for (link = df->insns[uid].uses; link; link = link->next)
    {
      struct ref *use = link->ref;

      if (DF_REF_REGNO (use) == regno)
	return use;
    }

  return 0;
}


/* Return first def of REGNO inside INSN within BB.  */
static struct ref *
df_bb_insn_regno_first_def_find (struct df *df,
				 basic_block bb ATTRIBUTE_UNUSED, rtx insn,
				 unsigned int regno)
{
  unsigned int uid;
  struct df_link *link;

  uid = INSN_UID (insn);

  for (link = df->insns[uid].defs; link; link = link->next)
    {
      struct ref *def = link->ref;

      if (DF_REF_REGNO (def) == regno)
	return def;
    }

  return 0;
}


/* Return insn using REG if the BB contains only a single
   use and def of REG.  */
rtx
df_bb_single_def_use_insn_find (struct df *df, basic_block bb, rtx insn, rtx reg)
{
  struct ref *def;
  struct ref *use;
  struct df_link *du_link;

  def = df_bb_insn_regno_first_def_find (df, bb, insn, REGNO (reg));

  gcc_assert (def);

  du_link = DF_REF_CHAIN (def);

  if (! du_link)
    return NULL_RTX;

  use = du_link->ref;

  /* Check if def is dead.  */
  if (! use)
    return NULL_RTX;

  /* Check for multiple uses.  */
  if (du_link->next)
    return NULL_RTX;

  return DF_REF_INSN (use);
}

/* Functions for debugging/dumping dataflow information.  */


/* Dump a def-use or use-def chain for REF to FILE.  */
static void
df_chain_dump (struct df_link *link, FILE *file)
{
  fprintf (file, "{ ");
  for (; link; link = link->next)
    {
      fprintf (file, "%c%d ",
	       DF_REF_REG_DEF_P (link->ref) ? 'd' : 'u',
	       DF_REF_ID (link->ref));
    }
  fprintf (file, "}");
}


/* Dump a chain of refs with the associated regno.  */
static void
df_chain_dump_regno (struct df_link *link, FILE *file)
{
  fprintf (file, "{ ");
  for (; link; link = link->next)
    {
      fprintf (file, "%c%d(%d) ",
	       DF_REF_REG_DEF_P (link->ref) ? 'd' : 'u',
	       DF_REF_ID (link->ref),
	       DF_REF_REGNO (link->ref));
    }
  fprintf (file, "}");
}


/* Dump dataflow info.  */
void
df_dump (struct df *df, int flags, FILE *file)
{
  unsigned int j;
  basic_block bb;

  if (! df || ! file)
    return;

  fprintf (file, "\nDataflow summary:\n");
  fprintf (file, "n_regs = %d, n_defs = %d, n_uses = %d, n_bbs = %d\n",
	   df->n_regs, df->n_defs, df->n_uses, df->n_bbs);

  if (flags & DF_RD)
    {
      basic_block bb;

      fprintf (file, "Reaching defs:\n");
      FOR_EACH_BB (bb)
	{
	  struct bb_info *bb_info = DF_BB_INFO (df, bb);

	  if (! bb_info->rd_in)
	    continue;

	  fprintf (file, "bb %d in  \t", bb->index);
	  dump_bitmap (file, bb_info->rd_in);
	  fprintf (file, "bb %d gen \t", bb->index);
	  dump_bitmap (file, bb_info->rd_gen);
	  fprintf (file, "bb %d kill\t", bb->index);
	  dump_bitmap (file, bb_info->rd_kill);
	  fprintf (file, "bb %d out \t", bb->index);
	  dump_bitmap (file, bb_info->rd_out);
	}
    }

  if (flags & DF_UD_CHAIN)
    {
      fprintf (file, "Use-def chains:\n");
      for (j = 0; j < df->n_defs; j++)
	{
	  if (df->defs[j])
	    {
	      fprintf (file, "d%d bb %d luid %d insn %d reg %d ",
		       j, DF_REF_BBNO (df->defs[j]),
		       DF_INSN_LUID (df, DF_REF_INSN (df->defs[j])),
		       DF_REF_INSN_UID (df->defs[j]),
		       DF_REF_REGNO (df->defs[j]));
	      if (df->defs[j]->flags & DF_REF_READ_WRITE)
		fprintf (file, "read/write ");
	      df_chain_dump (DF_REF_CHAIN (df->defs[j]), file);
	      fprintf (file, "\n");
	    }
	}
    }

  if (flags & DF_RU)
    {
      fprintf (file, "Reaching uses:\n");
      FOR_EACH_BB (bb)
	{
	  struct bb_info *bb_info = DF_BB_INFO (df, bb);

	  if (! bb_info->ru_in)
	    continue;

	  fprintf (file, "bb %d in  \t", bb->index);
	  dump_bitmap (file, bb_info->ru_in);
	  fprintf (file, "bb %d gen \t", bb->index);
	  dump_bitmap (file, bb_info->ru_gen);
	  fprintf (file, "bb %d kill\t", bb->index);
	  dump_bitmap (file, bb_info->ru_kill);
	  fprintf (file, "bb %d out \t", bb->index);
	  dump_bitmap (file, bb_info->ru_out);
	}
    }

  if (flags & DF_DU_CHAIN)
    {
      fprintf (file, "Def-use chains:\n");
      for (j = 0; j < df->n_uses; j++)
	{
	  if (df->uses[j])
	    {
	      fprintf (file, "u%d bb %d luid %d insn %d reg %d ",
		       j, DF_REF_BBNO (df->uses[j]),
		       DF_INSN_LUID (df, DF_REF_INSN (df->uses[j])),
		       DF_REF_INSN_UID (df->uses[j]),
		       DF_REF_REGNO (df->uses[j]));
	      if (df->uses[j]->flags & DF_REF_READ_WRITE)
		fprintf (file, "read/write ");
	      df_chain_dump (DF_REF_CHAIN (df->uses[j]), file);
	      fprintf (file, "\n");
	    }
	}
    }

  if (flags & DF_LR)
    {
      fprintf (file, "Live regs:\n");
      FOR_EACH_BB (bb)
	{
	  struct bb_info *bb_info = DF_BB_INFO (df, bb);

	  if (! bb_info->lr_in)
	    continue;

	  fprintf (file, "bb %d in  \t", bb->index);
	  dump_bitmap (file, bb_info->lr_in);
	  fprintf (file, "bb %d use \t", bb->index);
	  dump_bitmap (file, bb_info->lr_use);
	  fprintf (file, "bb %d def \t", bb->index);
	  dump_bitmap (file, bb_info->lr_def);
	  fprintf (file, "bb %d out \t", bb->index);
	  dump_bitmap (file, bb_info->lr_out);
	}
    }

  if (flags & (DF_REG_INFO | DF_RD_CHAIN | DF_RU_CHAIN))
    {
      struct reg_info *reg_info = df->regs;

      fprintf (file, "Register info:\n");
      for (j = 0; j < df->n_regs; j++)
	{
	  if (((flags & DF_REG_INFO)
	       && (reg_info[j].n_uses || reg_info[j].n_defs))
	      || ((flags & DF_RD_CHAIN) && reg_info[j].defs)
	      || ((flags & DF_RU_CHAIN) && reg_info[j].uses))
	    {
	      fprintf (file, "reg %d", j);
	      if ((flags & DF_RD_CHAIN) && (flags & DF_RU_CHAIN))
		{
		  basic_block bb = df_regno_bb (df, j);

		  if (bb)
		    fprintf (file, " bb %d", bb->index);
		  else
		    fprintf (file, " bb ?");
		}
	      if (flags & DF_REG_INFO)
		{
		  fprintf (file, " life %d", reg_info[j].lifetime);
		}

	      if ((flags & DF_REG_INFO) || (flags & DF_RD_CHAIN))
		{
		  fprintf (file, " defs ");
		  if (flags & DF_REG_INFO)
		    fprintf (file, "%d ", reg_info[j].n_defs);
		  if (flags & DF_RD_CHAIN)
		    df_chain_dump (reg_info[j].defs, file);
		}

	      if ((flags & DF_REG_INFO) || (flags & DF_RU_CHAIN))
		{
		  fprintf (file, " uses ");
		  if (flags & DF_REG_INFO)
		    fprintf (file, "%d ", reg_info[j].n_uses);
		  if (flags & DF_RU_CHAIN)
		    df_chain_dump (reg_info[j].uses, file);
		}

	      fprintf (file, "\n");
	    }
	}
    }
  fprintf (file, "\n");
}


void
df_insn_debug (struct df *df, rtx insn, FILE *file)
{
  unsigned int uid;
  int bbi;

  uid = INSN_UID (insn);
  if (uid >= df->insn_size)
    return;

  if (df->insns[uid].defs)
    bbi = DF_REF_BBNO (df->insns[uid].defs->ref);
  else if (df->insns[uid].uses)
    bbi = DF_REF_BBNO (df->insns[uid].uses->ref);
  else
    bbi = -1;

  fprintf (file, "insn %d bb %d luid %d defs ",
	   uid, bbi, DF_INSN_LUID (df, insn));
  df_chain_dump (df->insns[uid].defs, file);
  fprintf (file, " uses ");
  df_chain_dump (df->insns[uid].uses, file);
  fprintf (file, "\n");
}


void
df_insn_debug_regno (struct df *df, rtx insn, FILE *file)
{
  unsigned int uid;
  int bbi;

  uid = INSN_UID (insn);
  if (uid >= df->insn_size)
    return;

  if (df->insns[uid].defs)
    bbi = DF_REF_BBNO (df->insns[uid].defs->ref);
  else if (df->insns[uid].uses)
    bbi = DF_REF_BBNO (df->insns[uid].uses->ref);
  else
    bbi = -1;

  fprintf (file, "insn %d bb %d luid %d defs ",
	   uid, bbi, DF_INSN_LUID (df, insn));
  df_chain_dump_regno (df->insns[uid].defs, file);
  fprintf (file, " uses ");
  df_chain_dump_regno (df->insns[uid].uses, file);
  fprintf (file, "\n");
}


static void
df_regno_debug (struct df *df, unsigned int regno, FILE *file)
{
  if (regno >= df->reg_size)
    return;

  fprintf (file, "reg %d life %d defs ",
	   regno, df->regs[regno].lifetime);
  df_chain_dump (df->regs[regno].defs, file);
  fprintf (file, " uses ");
  df_chain_dump (df->regs[regno].uses, file);
  fprintf (file, "\n");
}


static void
df_ref_debug (struct df *df, struct ref *ref, FILE *file)
{
  fprintf (file, "%c%d ",
	   DF_REF_REG_DEF_P (ref) ? 'd' : 'u',
	   DF_REF_ID (ref));
  fprintf (file, "reg %d bb %d luid %d insn %d chain ",
	   DF_REF_REGNO (ref),
	   DF_REF_BBNO (ref),
	   DF_INSN_LUID (df, DF_REF_INSN (ref)),
	   INSN_UID (DF_REF_INSN (ref)));
  df_chain_dump (DF_REF_CHAIN (ref), file);
  fprintf (file, "\n");
}

/* Functions for debugging from GDB.  */

void
debug_df_insn (rtx insn)
{
  df_insn_debug (ddf, insn, stderr);
  debug_rtx (insn);
}


void
debug_df_reg (rtx reg)
{
  df_regno_debug (ddf, REGNO (reg), stderr);
}


void
debug_df_regno (unsigned int regno)
{
  df_regno_debug (ddf, regno, stderr);
}


void
debug_df_ref (struct ref *ref)
{
  df_ref_debug (ddf, ref, stderr);
}


void
debug_df_defno (unsigned int defno)
{
  df_ref_debug (ddf, ddf->defs[defno], stderr);
}


void
debug_df_useno (unsigned int defno)
{
  df_ref_debug (ddf, ddf->uses[defno], stderr);
}


void
debug_df_chain (struct df_link *link)
{
  df_chain_dump (link, stderr);
  fputc ('\n', stderr);
}


/* Perform the set operation OP1 OP OP2, using set representation REPR, and
   storing the result in OP1.  */

static void
dataflow_set_a_op_b (enum set_representation repr,
		     enum df_confluence_op op,
		     void *op1, void *op2)
{
  switch (repr)
    {
    case SR_SBITMAP:
      switch (op)
	{
	case DF_UNION:
	  sbitmap_a_or_b (op1, op1, op2);
	  break;

	case DF_INTERSECTION:
	  sbitmap_a_and_b (op1, op1, op2);
	  break;

    	default:
	  gcc_unreachable ();
	}
      break;

    case SR_BITMAP:
      switch (op)
	{
	case DF_UNION:
	  bitmap_ior_into (op1, op2);
	  break;

	case DF_INTERSECTION:
	  bitmap_and_into (op1, op2);
	  break;

    	default:
	  gcc_unreachable ();
	}
      break;

    default:
      gcc_unreachable ();
    }
}

static void
dataflow_set_copy (enum set_representation repr, void *dest, void *src)
{
  switch (repr)
    {
    case SR_SBITMAP:
      sbitmap_copy (dest, src);
      break;

    case SR_BITMAP:
      bitmap_copy (dest, src);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Hybrid search algorithm from "Implementation Techniques for
   Efficient Data-Flow Analysis of Large Programs".  */

static void
hybrid_search (basic_block bb, struct dataflow *dataflow,
	       sbitmap visited, sbitmap pending, sbitmap considered)
{
  int changed;
  int i = bb->index;
  edge e;
  edge_iterator ei;

  SET_BIT (visited, bb->index);
  gcc_assert (TEST_BIT (pending, bb->index));
  RESET_BIT (pending, i);

#define HS(E_ANTI, E_ANTI_BB, E_ANTI_START_BB, IN_SET,			\
	   E, E_BB, E_START_BB, OUT_SET)				\
  do									\
    {									\
      /*  Calculate <conf_op> of predecessor_outs.  */			\
      bitmap_zero (IN_SET[i]);						\
      FOR_EACH_EDGE (e, ei, bb->E_ANTI)					\
	{								\
	  if (e->E_ANTI_BB == E_ANTI_START_BB)				\
	    continue;							\
	  if (!TEST_BIT (considered, e->E_ANTI_BB->index))		\
	    continue;							\
									\
	  dataflow_set_a_op_b (dataflow->repr, dataflow->conf_op,	\
			       IN_SET[i],      			        \
			       OUT_SET[e->E_ANTI_BB->index]);		\
	}								\
									\
      (*dataflow->transfun)(i, &changed,				\
			    dataflow->in[i], dataflow->out[i],		\
			    dataflow->gen[i], dataflow->kill[i],	\
			    dataflow->data);				\
									\
      if (!changed)							\
	break;								\
									\
      FOR_EACH_EDGE (e, ei, bb->E)						\
	{								\
	  if (e->E_BB == E_START_BB || e->E_BB->index == i)		\
	    continue;							\
									\
	  if (!TEST_BIT (considered, e->E_BB->index))			\
	    continue;							\
									\
	  SET_BIT (pending, e->E_BB->index);				\
      	}								\
									\
      FOR_EACH_EDGE (e, ei, bb->E)						\
	{								\
	  if (e->E_BB == E_START_BB || e->E_BB->index == i)		\
	    continue;							\
									\
	  if (!TEST_BIT (considered, e->E_BB->index))			\
	    continue;							\
									\
	  if (!TEST_BIT (visited, e->E_BB->index))			\
	    hybrid_search (e->E_BB, dataflow, visited, pending, considered); \
	}								\
    } while (0)

  if (dataflow->dir == DF_FORWARD)
    HS (preds, src, ENTRY_BLOCK_PTR, dataflow->in,
	succs, dest, EXIT_BLOCK_PTR, dataflow->out);
  else
    HS (succs, dest, EXIT_BLOCK_PTR, dataflow->out,
	preds, src, ENTRY_BLOCK_PTR, dataflow->in);
}

/* This function will perform iterative bitvector dataflow described by
   DATAFLOW, producing the in and out sets.  Only the part of the cfg
   induced by blocks in DATAFLOW->order is taken into account.

   For forward problems, you probably want to pass in a mapping of
   block number to rc_order (like df->inverse_rc_map).  */

void
iterative_dataflow (struct dataflow *dataflow)
{
  unsigned i, idx;
  sbitmap visited, pending, considered;

  pending = sbitmap_alloc (last_basic_block);
  visited = sbitmap_alloc (last_basic_block);
  considered = sbitmap_alloc (last_basic_block);
  sbitmap_zero (pending);
  sbitmap_zero (visited);
  sbitmap_zero (considered);

  for (i = 0; i < dataflow->n_blocks; i++)
    {
      idx = dataflow->order[i];
      SET_BIT (pending, idx);
      SET_BIT (considered, idx);
      if (dataflow->dir == DF_FORWARD)
	dataflow_set_copy (dataflow->repr,
			   dataflow->out[idx], dataflow->gen[idx]);
      else
	dataflow_set_copy (dataflow->repr,
			   dataflow->in[idx], dataflow->gen[idx]);
    };

  while (1)
    {
      for (i = 0; i < dataflow->n_blocks; i++)
	{
	  idx = dataflow->order[i];

	  if (TEST_BIT (pending, idx) && !TEST_BIT (visited, idx))
	    hybrid_search (BASIC_BLOCK (idx), dataflow,
			   visited, pending, considered);
	}

      if (sbitmap_first_set_bit (pending) == -1)
	break;

      sbitmap_zero (visited);
    }

  sbitmap_free (pending);
  sbitmap_free (visited);
  sbitmap_free (considered);
}
