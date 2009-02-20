/* Standard problems for dataflow support routines.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
   2008, 2009 Free Software Foundation, Inc.
   Originally contributed by Michael P. Hayes 
             (m.hayes@elec.canterbury.ac.nz, mhayes@redhat.com)
   Major rewrite contributed by Danny Berlin (dberlin@dberlin.org)
             and Kenneth Zadeck (zadeck@naturalbridge.com).

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
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "regs.h"
#include "output.h"
#include "alloc-pool.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "timevar.h"
#include "df.h"
#include "except.h"
#include "dce.h"
#include "vecprim.h"

/* Note that turning REG_DEAD_DEBUGGING on will cause
   gcc.c-torture/unsorted/dump-noaddr.c to fail because it prints
   addresses in the dumps.  */  
#if 0
#define REG_DEAD_DEBUGGING
#endif

#define DF_SPARSE_THRESHOLD 32

static bitmap seen_in_block = NULL;
static bitmap seen_in_insn = NULL;


/*----------------------------------------------------------------------------
   Public functions access functions for the dataflow problems.
----------------------------------------------------------------------------*/
/* Get the live at out set for BB no matter what problem happens to be
   defined.  This function is used by the register allocators who
   choose different dataflow problems depending on the optimization
   level.  */

bitmap
df_get_live_out (basic_block bb)
{
  gcc_assert (df_lr);

  if (df_live)
    return DF_LIVE_OUT (bb);
  else 
    return DF_LR_OUT (bb);
}

/* Get the live at in set for BB no matter what problem happens to be
   defined.  This function is used by the register allocators who
   choose different dataflow problems depending on the optimization
   level.  */

bitmap
df_get_live_in (basic_block bb)
{
  gcc_assert (df_lr);

  if (df_live)
    return DF_LIVE_IN (bb);
  else 
    return DF_LR_IN (bb);
}

/*----------------------------------------------------------------------------
   Utility functions.
----------------------------------------------------------------------------*/

/* Generic versions to get the void* version of the block info.  Only
   used inside the problem instance vectors.  */

/* Grow the bb_info array.  */

void
df_grow_bb_info (struct dataflow *dflow)
{
  unsigned int new_size = last_basic_block + 1;
  if (dflow->block_info_size < new_size)
    {
      new_size += new_size / 4;
      dflow->block_info = XRESIZEVEC (void *, dflow->block_info, new_size);
      memset (dflow->block_info + dflow->block_info_size, 0,
	      (new_size - dflow->block_info_size) *sizeof (void *));
      dflow->block_info_size = new_size;
    }
}

/* Dump a def-use or use-def chain for REF to FILE.  */

void
df_chain_dump (struct df_link *link, FILE *file)
{
  fprintf (file, "{ ");
  for (; link; link = link->next)
    {
      fprintf (file, "%c%d(bb %d insn %d) ",
	       DF_REF_REG_DEF_P (link->ref) ? 'd' : 'u',
	       DF_REF_ID (link->ref),
	       DF_REF_BBNO (link->ref),
	       DF_REF_IS_ARTIFICIAL (link->ref) ? -1 : DF_REF_INSN_UID (link->ref));
    }
  fprintf (file, "}");
}


/* Print some basic block info as part of df_dump.  */

void 
df_print_bb_index (basic_block bb, FILE *file)
{
  edge e;
  edge_iterator ei;

  fprintf (file, "\n( ");
    FOR_EACH_EDGE (e, ei, bb->preds)
    {
      basic_block pred = e->src;
      fprintf (file, "%d%s ", pred->index, e->flags & EDGE_EH ? "(EH)" : "");
    } 
  fprintf (file, ")->[%d]->( ", bb->index);
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      basic_block succ = e->dest;
      fprintf (file, "%d%s ", succ->index, e->flags & EDGE_EH ? "(EH)" : "");
    } 
  fprintf (file, ")\n");
}



/* Make sure that the seen_in_insn and seen_in_block sbitmaps are set
   up correctly. */

static void
df_set_seen (void)
{
  seen_in_block = BITMAP_ALLOC (&df_bitmap_obstack);
  seen_in_insn = BITMAP_ALLOC (&df_bitmap_obstack);
}


static void
df_unset_seen (void)
{
  BITMAP_FREE (seen_in_block);
  BITMAP_FREE (seen_in_insn);
}



/*----------------------------------------------------------------------------
   REACHING DEFINITIONS

   Find the locations in the function where each definition site for a
   pseudo reaches.  In and out bitvectors are built for each basic
   block.  The id field in the ref is used to index into these sets.
   See df.h for details.
   ----------------------------------------------------------------------------*/

/* This problem plays a large number of games for the sake of
   efficiency.  
   
   1) The order of the bits in the bitvectors.  After the scanning
   phase, all of the defs are sorted.  All of the defs for the reg 0
   are first, followed by all defs for reg 1 and so on.
   
   2) There are two kill sets, one if the number of defs is less or
   equal to DF_SPARSE_THRESHOLD and another if the number of defs is
   greater.

   <= : Data is built directly in the kill set.

   > : One level of indirection is used to keep from generating long
   strings of 1 bits in the kill sets.  Bitvectors that are indexed
   by the regnum are used to represent that there is a killing def
   for the register.  The confluence and transfer functions use
   these along with the bitmap_clear_range call to remove ranges of
   bits without actually generating a knockout vector.

   The kill and sparse_kill and the dense_invalidated_by_call and
   sparse_invalidated_by_call both play this game.  */

/* Private data used to compute the solution for this problem.  These
   data structures are not accessible outside of this module.  */
struct df_rd_problem_data
{
  /* The set of defs to regs invalidated by call.  */
  bitmap sparse_invalidated_by_call;  
  /* The set of defs to regs invalidate by call for rd.  */  
  bitmap dense_invalidated_by_call;
  /* An obstack for the bitmaps we need for this problem.  */
  bitmap_obstack rd_bitmaps;
};

/* Set basic block info.  */

static void
df_rd_set_bb_info (unsigned int index, 
		   struct df_rd_bb_info *bb_info)
{
  gcc_assert (df_rd);
  gcc_assert (index < df_rd->block_info_size);
  df_rd->block_info[index] = bb_info;
}


/* Free basic block info.  */

static void
df_rd_free_bb_info (basic_block bb ATTRIBUTE_UNUSED, 
		    void *vbb_info)
{
  struct df_rd_bb_info *bb_info = (struct df_rd_bb_info *) vbb_info;
  if (bb_info)
    {
      BITMAP_FREE (bb_info->kill);
      BITMAP_FREE (bb_info->sparse_kill);
      BITMAP_FREE (bb_info->gen);
      BITMAP_FREE (bb_info->in);
      BITMAP_FREE (bb_info->out);
      pool_free (df_rd->block_pool, bb_info);
    }
}


/* Allocate or reset bitmaps for DF_RD blocks. The solution bits are
   not touched unless the block is new.  */

static void 
df_rd_alloc (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;
  struct df_rd_problem_data *problem_data;

  if (!df_rd->block_pool)
    df_rd->block_pool = create_alloc_pool ("df_rd_block pool", 
					   sizeof (struct df_rd_bb_info), 50);

  if (df_rd->problem_data)
    {
      problem_data = (struct df_rd_problem_data *) df_rd->problem_data;
      bitmap_clear (problem_data->sparse_invalidated_by_call);
      bitmap_clear (problem_data->dense_invalidated_by_call);
    }
  else 
    {
      problem_data = XNEW (struct df_rd_problem_data);
      df_rd->problem_data = problem_data;

      bitmap_obstack_initialize (&problem_data->rd_bitmaps);
      problem_data->sparse_invalidated_by_call
	= BITMAP_ALLOC (&problem_data->rd_bitmaps);
      problem_data->dense_invalidated_by_call
	= BITMAP_ALLOC (&problem_data->rd_bitmaps);
    }

  df_grow_bb_info (df_rd);

  /* Because of the clustering of all use sites for the same pseudo,
     we have to process all of the blocks before doing the
     analysis.  */

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb_index);
      if (bb_info)
	{ 
	  bitmap_clear (bb_info->kill);
	  bitmap_clear (bb_info->sparse_kill);
	  bitmap_clear (bb_info->gen);
	}
      else
	{ 
	  bb_info = (struct df_rd_bb_info *) pool_alloc (df_rd->block_pool);
	  df_rd_set_bb_info (bb_index, bb_info);
	  bb_info->kill = BITMAP_ALLOC (&problem_data->rd_bitmaps);
	  bb_info->sparse_kill = BITMAP_ALLOC (&problem_data->rd_bitmaps);
	  bb_info->gen = BITMAP_ALLOC (&problem_data->rd_bitmaps);
	  bb_info->in = BITMAP_ALLOC (&problem_data->rd_bitmaps);
	  bb_info->out = BITMAP_ALLOC (&problem_data->rd_bitmaps);
	}
    }
  df_rd->optional_p = true;
}


/* Process a list of DEFs for df_rd_bb_local_compute.  */

static void
df_rd_bb_local_compute_process_def (struct df_rd_bb_info *bb_info, 
				    df_ref *def_rec,
				    enum df_ref_flags top_flag)
{
  while (*def_rec)
    {
      df_ref def = *def_rec;
      if (top_flag == (DF_REF_FLAGS (def) & DF_REF_AT_TOP))
	{
	  unsigned int regno = DF_REF_REGNO (def);
	  unsigned int begin = DF_DEFS_BEGIN (regno);
	  unsigned int n_defs = DF_DEFS_COUNT (regno);
	  
	  if ((!(df->changeable_flags & DF_NO_HARD_REGS))
	      || (regno >= FIRST_PSEUDO_REGISTER))
	    {
	      /* Only the last def(s) for a regno in the block has any
		 effect.  */ 
	      if (!bitmap_bit_p (seen_in_block, regno))
		{
		  /* The first def for regno in insn gets to knock out the
		     defs from other instructions.  */
		  if ((!bitmap_bit_p (seen_in_insn, regno))
		      /* If the def is to only part of the reg, it does
			 not kill the other defs that reach here.  */
		      && (!(DF_REF_FLAGS (def) & 
			    (DF_REF_PARTIAL | DF_REF_CONDITIONAL | DF_REF_MAY_CLOBBER))))
		    {
		      if (n_defs > DF_SPARSE_THRESHOLD)
			{
			  bitmap_set_bit (bb_info->sparse_kill, regno);
			  bitmap_clear_range(bb_info->gen, begin, n_defs);
			}
		      else
			{
			  bitmap_set_range (bb_info->kill, begin, n_defs);
			  bitmap_clear_range (bb_info->gen, begin, n_defs);
			}
		    }
		  
		  bitmap_set_bit (seen_in_insn, regno);
		  /* All defs for regno in the instruction may be put into
		     the gen set.  */
		  if (!(DF_REF_FLAGS (def) 
			& (DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER)))
		    bitmap_set_bit (bb_info->gen, DF_REF_ID (def));
		}
	    }
	}
      def_rec++;
    }
}

/* Compute local reaching def info for basic block BB.  */

static void
df_rd_bb_local_compute (unsigned int bb_index)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb_index);
  rtx insn;

  bitmap_clear (seen_in_block);
  bitmap_clear (seen_in_insn);

  /* Artificials are only hard regs.  */
  if (!(df->changeable_flags & DF_NO_HARD_REGS))
    df_rd_bb_local_compute_process_def (bb_info, 
					df_get_artificial_defs (bb_index),
					0);

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);

      if (!INSN_P (insn))
	continue;

      df_rd_bb_local_compute_process_def (bb_info, 
					  DF_INSN_UID_DEFS (uid), 0);

      /* This complex dance with the two bitmaps is required because
	 instructions can assign twice to the same pseudo.  This
	 generally happens with calls that will have one def for the
	 result and another def for the clobber.  If only one vector
	 is used and the clobber goes first, the result will be
	 lost.  */
      bitmap_ior_into (seen_in_block, seen_in_insn);
      bitmap_clear (seen_in_insn);
    }

  /* Process the artificial defs at the top of the block last since we
     are going backwards through the block and these are logically at
     the start.  */
  if (!(df->changeable_flags & DF_NO_HARD_REGS))
    df_rd_bb_local_compute_process_def (bb_info, 
					df_get_artificial_defs (bb_index),
					DF_REF_AT_TOP);
}


/* Compute local reaching def info for each basic block within BLOCKS.  */

static void
df_rd_local_compute (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;
  unsigned int regno;
  struct df_rd_problem_data *problem_data
    = (struct df_rd_problem_data *) df_rd->problem_data;
  bitmap sparse_invalidated = problem_data->sparse_invalidated_by_call;
  bitmap dense_invalidated = problem_data->dense_invalidated_by_call;

  df_set_seen ();

  df_maybe_reorganize_def_refs (DF_REF_ORDER_BY_REG);

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      df_rd_bb_local_compute (bb_index);
    }
  
  /* Set up the knockout bit vectors to be applied across EH_EDGES.  */
  EXECUTE_IF_SET_IN_BITMAP (regs_invalidated_by_call_regset, 0, regno, bi)
    {
      if (DF_DEFS_COUNT (regno) > DF_SPARSE_THRESHOLD)
	bitmap_set_bit (sparse_invalidated, regno);
      else
	bitmap_set_range (dense_invalidated, 
			  DF_DEFS_BEGIN (regno), 
			  DF_DEFS_COUNT (regno));
    }
  df_unset_seen ();
}


/* Initialize the solution bit vectors for problem.  */

static void 
df_rd_init_solution (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb_index);
      
      bitmap_copy (bb_info->out, bb_info->gen);
      bitmap_clear (bb_info->in);
    }
}

/* In of target gets or of out of source.  */

static void
df_rd_confluence_n (edge e)
{
  bitmap op1 = df_rd_get_bb_info (e->dest->index)->in;
  bitmap op2 = df_rd_get_bb_info (e->src->index)->out;

  if (e->flags & EDGE_FAKE) 
    return;

  if (e->flags & EDGE_EH)
    {
      struct df_rd_problem_data *problem_data
	= (struct df_rd_problem_data *) df_rd->problem_data;
      bitmap sparse_invalidated = problem_data->sparse_invalidated_by_call;
      bitmap dense_invalidated = problem_data->dense_invalidated_by_call;
      bitmap_iterator bi;
      unsigned int regno;
      bitmap tmp = BITMAP_ALLOC (&df_bitmap_obstack);

      bitmap_copy (tmp, op2);
      bitmap_and_compl_into (tmp, dense_invalidated);

      EXECUTE_IF_SET_IN_BITMAP (sparse_invalidated, 0, regno, bi)
 	{
 	  bitmap_clear_range (tmp, 
 			      DF_DEFS_BEGIN (regno), 
 			      DF_DEFS_COUNT (regno));
	}
      bitmap_ior_into (op1, tmp);
      BITMAP_FREE (tmp);
    }
  else
    bitmap_ior_into (op1, op2);
}


/* Transfer function.  */

static bool
df_rd_transfer_function (int bb_index)
{
  struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb_index);
  unsigned int regno;
  bitmap_iterator bi;
  bitmap in = bb_info->in;
  bitmap out = bb_info->out;
  bitmap gen = bb_info->gen;
  bitmap kill = bb_info->kill;
  bitmap sparse_kill = bb_info->sparse_kill;

  if (bitmap_empty_p (sparse_kill))
    return  bitmap_ior_and_compl (out, gen, in, kill);
  else 
    {
      struct df_rd_problem_data *problem_data;
      bool changed = false;
      bitmap tmp;

      /* Note that TMP is _not_ a temporary bitmap if we end up replacing
	 OUT with TMP.  Therefore, allocate TMP in the RD bitmaps obstack.  */
      problem_data = (struct df_rd_problem_data *) df_rd->problem_data;
      tmp = BITMAP_ALLOC (&problem_data->rd_bitmaps);

      bitmap_copy (tmp, in);
      EXECUTE_IF_SET_IN_BITMAP (sparse_kill, 0, regno, bi)
	{
	  bitmap_clear_range (tmp, 
			      DF_DEFS_BEGIN (regno), 
			      DF_DEFS_COUNT (regno));
	}
      bitmap_and_compl_into (tmp, kill);
      bitmap_ior_into (tmp, gen);
      changed = !bitmap_equal_p (tmp, out);
      if (changed)
	{
	  BITMAP_FREE (out);
	  bb_info->out = tmp;
	}
      else 
	  BITMAP_FREE (tmp);
      return changed;
    }
}


/* Free all storage associated with the problem.  */

static void
df_rd_free (void)
{
  struct df_rd_problem_data *problem_data
    = (struct df_rd_problem_data *) df_rd->problem_data;

  if (problem_data)
    {
      free_alloc_pool (df_rd->block_pool);
      bitmap_obstack_release (&problem_data->rd_bitmaps);
      
      df_rd->block_info_size = 0;
      free (df_rd->block_info);
      free (df_rd->problem_data);
    }
  free (df_rd);
}


/* Debugging info.  */

static void
df_rd_start_dump (FILE *file)
{
  struct df_rd_problem_data *problem_data
    = (struct df_rd_problem_data *) df_rd->problem_data;
  unsigned int m = DF_REG_SIZE(df);
  unsigned int regno;
  
  if (!df_rd->block_info) 
    return;

  fprintf (file, ";; Reaching defs:\n\n");

  fprintf (file, "  sparse invalidated \t");
  dump_bitmap (file, problem_data->sparse_invalidated_by_call);
  fprintf (file, "  dense invalidated \t");
  dump_bitmap (file, problem_data->dense_invalidated_by_call);

  for (regno = 0; regno < m; regno++)
    if (DF_DEFS_COUNT (regno))
      fprintf (file, "%d[%d,%d] ", regno, 
	       DF_DEFS_BEGIN (regno), 
	       DF_DEFS_COUNT (regno));
  fprintf (file, "\n");

}


/* Debugging info at top of bb.  */

static void
df_rd_top_dump (basic_block bb, FILE *file)
{
  struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb->index);
  if (!bb_info || !bb_info->in)
    return;
  
  fprintf (file, ";; rd  in  \t(%d)\n", (int) bitmap_count_bits (bb_info->in));
  dump_bitmap (file, bb_info->in);
  fprintf (file, ";; rd  gen \t(%d)\n", (int) bitmap_count_bits (bb_info->gen));
  dump_bitmap (file, bb_info->gen);
  fprintf (file, ";; rd  kill\t(%d)\n", (int) bitmap_count_bits (bb_info->kill));
  dump_bitmap (file, bb_info->kill);
}


/* Debugging info at top of bb.  */

static void
df_rd_bottom_dump (basic_block bb, FILE *file)
{
  struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb->index);
  if (!bb_info || !bb_info->out)
    return;
  
  fprintf (file, ";; rd  out \t(%d)\n", (int) bitmap_count_bits (bb_info->out));
  dump_bitmap (file, bb_info->out);
}

/* All of the information associated with every instance of the problem.  */

static struct df_problem problem_RD =
{
  DF_RD,                      /* Problem id.  */
  DF_FORWARD,                 /* Direction.  */
  df_rd_alloc,                /* Allocate the problem specific data.  */
  NULL,                       /* Reset global information.  */
  df_rd_free_bb_info,         /* Free basic block info.  */
  df_rd_local_compute,        /* Local compute function.  */
  df_rd_init_solution,        /* Init the solution specific data.  */
  df_worklist_dataflow,       /* Worklist solver.  */
  NULL,                       /* Confluence operator 0.  */ 
  df_rd_confluence_n,         /* Confluence operator n.  */ 
  df_rd_transfer_function,    /* Transfer function.  */
  NULL,                       /* Finalize function.  */
  df_rd_free,                 /* Free all of the problem information.  */
  df_rd_free,                 /* Remove this problem from the stack of dataflow problems.  */
  df_rd_start_dump,           /* Debugging.  */
  df_rd_top_dump,             /* Debugging start block.  */
  df_rd_bottom_dump,          /* Debugging end block.  */
  NULL,                       /* Incremental solution verify start.  */
  NULL,                       /* Incremental solution verify end.  */
  NULL,                       /* Dependent problem.  */
  TV_DF_RD,                   /* Timing variable.  */ 
  true                        /* Reset blocks on dropping out of blocks_to_analyze.  */
};



/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_rd_add_problem (void)
{
  df_add_problem (&problem_RD);
}



/*----------------------------------------------------------------------------
   LIVE REGISTERS

   Find the locations in the function where any use of a pseudo can
   reach in the backwards direction.  In and out bitvectors are built
   for each basic block.  The regno is used to index into these sets.
   See df.h for details.
   ----------------------------------------------------------------------------*/

/* Private data used to verify the solution for this problem.  */
struct df_lr_problem_data
{
  bitmap *in;
  bitmap *out;
};


/* Set basic block info.  */

static void
df_lr_set_bb_info (unsigned int index, 
		   struct df_lr_bb_info *bb_info)
{
  gcc_assert (df_lr);
  gcc_assert (index < df_lr->block_info_size);
  df_lr->block_info[index] = bb_info;
}

 
/* Free basic block info.  */

static void
df_lr_free_bb_info (basic_block bb ATTRIBUTE_UNUSED, 
		    void *vbb_info)
{
  struct df_lr_bb_info *bb_info = (struct df_lr_bb_info *) vbb_info;
  if (bb_info)
    {
      BITMAP_FREE (bb_info->use);
      BITMAP_FREE (bb_info->def);
      BITMAP_FREE (bb_info->in);
      BITMAP_FREE (bb_info->out);
      pool_free (df_lr->block_pool, bb_info);
    }
}


/* Allocate or reset bitmaps for DF_LR blocks. The solution bits are
   not touched unless the block is new.  */

static void 
df_lr_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  if (!df_lr->block_pool)
    df_lr->block_pool = create_alloc_pool ("df_lr_block pool", 
					   sizeof (struct df_lr_bb_info), 50);

  df_grow_bb_info (df_lr);

  EXECUTE_IF_SET_IN_BITMAP (df_lr->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb_index);
      if (bb_info)
	{ 
	  bitmap_clear (bb_info->def);
	  bitmap_clear (bb_info->use);
	}
      else
	{ 
	  bb_info = (struct df_lr_bb_info *) pool_alloc (df_lr->block_pool);
	  df_lr_set_bb_info (bb_index, bb_info);
	  bb_info->use = BITMAP_ALLOC (NULL);
	  bb_info->def = BITMAP_ALLOC (NULL);
	  bb_info->in = BITMAP_ALLOC (NULL);
	  bb_info->out = BITMAP_ALLOC (NULL);
	}
    }

  df_lr->optional_p = false;
}


/* Reset the global solution for recalculation.  */

static void 
df_lr_reset (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb_index);
      gcc_assert (bb_info);
      bitmap_clear (bb_info->in);
      bitmap_clear (bb_info->out);
    }
}


/* Compute local live register info for basic block BB.  */

static void
df_lr_bb_local_compute (unsigned int bb_index)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb_index);
  rtx insn;
  df_ref *def_rec;
  df_ref *use_rec;

  /* Process the registers set in an exception handler.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  bitmap_set_bit (bb_info->def, dregno);
	  bitmap_clear_bit (bb_info->use, dregno);
	}
    }

  /* Process the hardware registers that are always live.  */
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      /* Add use to set of uses in this BB.  */
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	bitmap_set_bit (bb_info->use, DF_REF_REGNO (use));
    }

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);

      if (!INSN_P (insn))
	continue;	

      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  /* If the def is to only part of the reg, it does
	     not kill the other defs that reach here.  */
	  if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
	    {
	      unsigned int dregno = DF_REF_REGNO (def);
	      bitmap_set_bit (bb_info->def, dregno);
	      bitmap_clear_bit (bb_info->use, dregno);
	    }
	}

      for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  /* Add use to set of uses in this BB.  */
	  bitmap_set_bit (bb_info->use, DF_REF_REGNO (use));
	}
    }

  /* Process the registers set in an exception handler or the hard
     frame pointer if this block is the target of a non local
     goto.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  bitmap_set_bit (bb_info->def, dregno);
	  bitmap_clear_bit (bb_info->use, dregno);
	}
    }
  
#ifdef EH_USES
  /* Process the uses that are live into an exception handler.  */
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      /* Add use to set of uses in this BB.  */
      if (DF_REF_FLAGS (use) & DF_REF_AT_TOP)
	bitmap_set_bit (bb_info->use, DF_REF_REGNO (use));
    }
#endif

  /* If the df_live problem is not defined, such as at -O0 and -O1, we
     still need to keep the luids up to date.  This is normally done
     in the df_live problem since this problem has a forwards
     scan.  */
  if (!df_live)
    df_recompute_luids (bb);
}


/* Compute local live register info for each basic block within BLOCKS.  */

static void
df_lr_local_compute (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;
    
  bitmap_clear (df->hardware_regs_used);
  
  /* The all-important stack pointer must always be live.  */
  bitmap_set_bit (df->hardware_regs_used, STACK_POINTER_REGNUM);
  
  /* Before reload, there are a few registers that must be forced
     live everywhere -- which might not already be the case for
     blocks within infinite loops.  */
  if (!reload_completed)
    {
      /* Any reference to any pseudo before reload is a potential
	 reference of the frame pointer.  */
      bitmap_set_bit (df->hardware_regs_used, FRAME_POINTER_REGNUM);
      
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      /* Pseudos with argument area equivalences may require
	 reloading via the argument pointer.  */
      if (fixed_regs[ARG_POINTER_REGNUM])
	bitmap_set_bit (df->hardware_regs_used, ARG_POINTER_REGNUM);
#endif
      
      /* Any constant, or pseudo with constant equivalences, may
	 require reloading from memory using the pic register.  */
      if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
	  && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
	bitmap_set_bit (df->hardware_regs_used, PIC_OFFSET_TABLE_REGNUM);
    }
  
  EXECUTE_IF_SET_IN_BITMAP (df_lr->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      if (bb_index == EXIT_BLOCK)
	{
	  /* The exit block is special for this problem and its bits are
	     computed from thin air.  */
	  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (EXIT_BLOCK);
	  bitmap_copy (bb_info->use, df->exit_block_uses);
	}
      else
	df_lr_bb_local_compute (bb_index);
    }

  bitmap_clear (df_lr->out_of_date_transfer_functions);
}


/* Initialize the solution vectors.  */

static void 
df_lr_init (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb_index);
      bitmap_copy (bb_info->in, bb_info->use);
      bitmap_clear (bb_info->out);
    }
}


/* Confluence function that processes infinite loops.  This might be a
   noreturn function that throws.  And even if it isn't, getting the
   unwind info right helps debugging.  */
static void
df_lr_confluence_0 (basic_block bb)
{
  bitmap op1 = df_lr_get_bb_info (bb->index)->out;
  if (bb != EXIT_BLOCK_PTR)
    bitmap_copy (op1, df->hardware_regs_used);
} 


/* Confluence function that ignores fake edges.  */

static void
df_lr_confluence_n (edge e)
{
  bitmap op1 = df_lr_get_bb_info (e->src->index)->out;
  bitmap op2 = df_lr_get_bb_info (e->dest->index)->in;
 
  /* Call-clobbered registers die across exception and call edges.  */
  /* ??? Abnormal call edges ignored for the moment, as this gets
     confused by sibling call edges, which crashes reg-stack.  */
  if (e->flags & EDGE_EH)
    bitmap_ior_and_compl_into (op1, op2, regs_invalidated_by_call_regset);
  else
    bitmap_ior_into (op1, op2);

  bitmap_ior_into (op1, df->hardware_regs_used);
} 


/* Transfer function.  */

static bool
df_lr_transfer_function (int bb_index)
{
  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb_index);
  bitmap in = bb_info->in;
  bitmap out = bb_info->out;
  bitmap use = bb_info->use;
  bitmap def = bb_info->def;

  return bitmap_ior_and_compl (in, use, out, def);
}


/* Run the fast dce as a side effect of building LR.  */

static void
df_lr_finalize (bitmap all_blocks)
{
  df_lr->solutions_dirty = false;
  if (df->changeable_flags & DF_LR_RUN_DCE)
    {
      run_fast_df_dce ();

      /* If dce deletes some instructions, we need to recompute the lr
	 solution before proceeding further.  The problem is that fast
	 dce is a pessimestic dataflow algorithm.  In the case where
	 it deletes a statement S inside of a loop, the uses inside of
	 S may not be deleted from the dataflow solution because they
	 were carried around the loop.  While it is conservatively
	 correct to leave these extra bits, the standards of df
	 require that we maintain the best possible (least fixed
	 point) solution.  The only way to do that is to redo the
	 iteration from the beginning.  See PR35805 for an
	 example.  */
      if (df_lr->solutions_dirty)
	{
	  df_clear_flags (DF_LR_RUN_DCE);
	  df_lr_alloc (all_blocks);
	  df_lr_local_compute (all_blocks);
	  df_worklist_dataflow (df_lr, all_blocks, df->postorder, df->n_blocks);
	  df_lr_finalize (all_blocks);
	  df_set_flags (DF_LR_RUN_DCE);
	}
    }
}


/* Free all storage associated with the problem.  */

static void
df_lr_free (void)
{
  if (df_lr->block_info)
    {
      unsigned int i;
      for (i = 0; i < df_lr->block_info_size; i++)
	{
	  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (i);
	  if (bb_info)
	    {
	      BITMAP_FREE (bb_info->use);
	      BITMAP_FREE (bb_info->def);
	      BITMAP_FREE (bb_info->in);
	      BITMAP_FREE (bb_info->out);
	    }
	}
      free_alloc_pool (df_lr->block_pool);
      
      df_lr->block_info_size = 0;
      free (df_lr->block_info);
    }

  BITMAP_FREE (df_lr->out_of_date_transfer_functions);
  free (df_lr);
}


/* Debugging info at top of bb.  */

static void
df_lr_top_dump (basic_block bb, FILE *file)
{
  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb->index);
  struct df_lr_problem_data *problem_data;
  if (!bb_info || !bb_info->in)
    return;
      
  fprintf (file, ";; lr  in  \t");
  df_print_regset (file, bb_info->in);
  if (df_lr->problem_data)
    {
      problem_data = (struct df_lr_problem_data *)df_lr->problem_data;
      fprintf (file, ";;  old in  \t");
      df_print_regset (file, problem_data->in[bb->index]);
    }
  fprintf (file, ";; lr  use \t");
  df_print_regset (file, bb_info->use);
  fprintf (file, ";; lr  def \t");
  df_print_regset (file, bb_info->def);
}  


/* Debugging info at bottom of bb.  */

static void
df_lr_bottom_dump (basic_block bb, FILE *file)
{
  struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb->index);
  struct df_lr_problem_data *problem_data;
  if (!bb_info || !bb_info->out)
    return;
  
  fprintf (file, ";; lr  out \t");
  df_print_regset (file, bb_info->out);
  if (df_lr->problem_data)
    {
      problem_data = (struct df_lr_problem_data *)df_lr->problem_data;
      fprintf (file, ";;  old out  \t");
      df_print_regset (file, problem_data->out[bb->index]);
    }
}  


/* Build the datastructure to verify that the solution to the dataflow
   equations is not dirty.  */

static void
df_lr_verify_solution_start (void)
{
  basic_block bb;
  struct df_lr_problem_data *problem_data;
  if (df_lr->solutions_dirty)
    {
      df_lr->problem_data = NULL;
      return;
    }

  /* Set it true so that the solution is recomputed.  */ 
  df_lr->solutions_dirty = true;

  problem_data = XNEW (struct df_lr_problem_data);
  df_lr->problem_data = problem_data;
  problem_data->in = XNEWVEC (bitmap, last_basic_block);
  problem_data->out = XNEWVEC (bitmap, last_basic_block);

  FOR_ALL_BB (bb)
    {
      problem_data->in[bb->index] = BITMAP_ALLOC (NULL);
      problem_data->out[bb->index] = BITMAP_ALLOC (NULL);
      bitmap_copy (problem_data->in[bb->index], DF_LR_IN (bb));
      bitmap_copy (problem_data->out[bb->index], DF_LR_OUT (bb));
    }
}


/* Compare the saved datastructure and the new solution to the dataflow
   equations.  */

static void
df_lr_verify_solution_end (void)
{
  struct df_lr_problem_data *problem_data;
  basic_block bb;

  if (df_lr->problem_data == NULL)
    return;

  problem_data = (struct df_lr_problem_data *)df_lr->problem_data;

  if (df_lr->solutions_dirty)
    /* Do not check if the solution is still dirty.  See the comment
       in df_lr_finalize for details.  */
    df_lr->solutions_dirty = false;
  else
    FOR_ALL_BB (bb)
      {
	if ((!bitmap_equal_p (problem_data->in[bb->index], DF_LR_IN (bb)))
	    || (!bitmap_equal_p (problem_data->out[bb->index], DF_LR_OUT (bb))))
	  {
	    /*df_dump (stderr);*/
	    gcc_unreachable ();
	  }
      }

  /* Cannot delete them immediately because you may want to dump them
     if the comparison fails.  */
  FOR_ALL_BB (bb)
    {
      BITMAP_FREE (problem_data->in[bb->index]);
      BITMAP_FREE (problem_data->out[bb->index]);
    }

  free (problem_data->in);
  free (problem_data->out);
  free (problem_data);
  df_lr->problem_data = NULL;
}


/* All of the information associated with every instance of the problem.  */

static struct df_problem problem_LR =
{
  DF_LR,                      /* Problem id.  */
  DF_BACKWARD,                /* Direction.  */
  df_lr_alloc,                /* Allocate the problem specific data.  */
  df_lr_reset,                /* Reset global information.  */
  df_lr_free_bb_info,         /* Free basic block info.  */
  df_lr_local_compute,        /* Local compute function.  */
  df_lr_init,                 /* Init the solution specific data.  */
  df_worklist_dataflow,       /* Worklist solver.  */
  df_lr_confluence_0,         /* Confluence operator 0.  */ 
  df_lr_confluence_n,         /* Confluence operator n.  */ 
  df_lr_transfer_function,    /* Transfer function.  */
  df_lr_finalize,             /* Finalize function.  */
  df_lr_free,                 /* Free all of the problem information.  */
  NULL,                       /* Remove this problem from the stack of dataflow problems.  */
  NULL,                       /* Debugging.  */
  df_lr_top_dump,             /* Debugging start block.  */
  df_lr_bottom_dump,          /* Debugging end block.  */
  df_lr_verify_solution_start,/* Incremental solution verify start.  */
  df_lr_verify_solution_end,  /* Incremental solution verify end.  */
  NULL,                       /* Dependent problem.  */
  TV_DF_LR,                   /* Timing variable.  */ 
  false                       /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_lr_add_problem (void)
{
  df_add_problem (&problem_LR);
  /* These will be initialized when df_scan_blocks processes each
     block.  */
  df_lr->out_of_date_transfer_functions = BITMAP_ALLOC (NULL);
}


/* Verify that all of the lr related info is consistent and
   correct.  */

void
df_lr_verify_transfer_functions (void)
{
  basic_block bb;
  bitmap saved_def;
  bitmap saved_use;
  bitmap saved_adef;
  bitmap saved_ause;
  bitmap all_blocks;

  if (!df)
    return;

  saved_def = BITMAP_ALLOC (NULL);
  saved_use = BITMAP_ALLOC (NULL);
  saved_adef = BITMAP_ALLOC (NULL);
  saved_ause = BITMAP_ALLOC (NULL);
  all_blocks = BITMAP_ALLOC (NULL);

  FOR_ALL_BB (bb)
    {
      struct df_lr_bb_info *bb_info = df_lr_get_bb_info (bb->index);
      bitmap_set_bit (all_blocks, bb->index);

      if (bb_info)
	{
	  /* Make a copy of the transfer functions and then compute
	     new ones to see if the transfer functions have
	     changed.  */
	  if (!bitmap_bit_p (df_lr->out_of_date_transfer_functions, 
			     bb->index))
	    {
	      bitmap_copy (saved_def, bb_info->def);
	      bitmap_copy (saved_use, bb_info->use);
	      bitmap_clear (bb_info->def);
	      bitmap_clear (bb_info->use);

	      df_lr_bb_local_compute (bb->index);
	      gcc_assert (bitmap_equal_p (saved_def, bb_info->def));
	      gcc_assert (bitmap_equal_p (saved_use, bb_info->use));
	    }
	}
      else
	{
	  /* If we do not have basic block info, the block must be in
	     the list of dirty blocks or else some one has added a
	     block behind our backs. */
	  gcc_assert (bitmap_bit_p (df_lr->out_of_date_transfer_functions, 
				    bb->index));
	}
      /* Make sure no one created a block without following
	 procedures.  */
      gcc_assert (df_scan_get_bb_info (bb->index));
    }

  /* Make sure there are no dirty bits in blocks that have been deleted.  */
  gcc_assert (!bitmap_intersect_compl_p (df_lr->out_of_date_transfer_functions, 
					 all_blocks)); 

  BITMAP_FREE (saved_def);
  BITMAP_FREE (saved_use);
  BITMAP_FREE (saved_adef);
  BITMAP_FREE (saved_ause);
  BITMAP_FREE (all_blocks);
}



/*----------------------------------------------------------------------------
   LIVE AND MUST-INITIALIZED REGISTERS.

   This problem first computes the IN and OUT bitvectors for the
   must-initialized registers problems, which is a forward problem.
   It gives the set of registers for which we MUST have an available
   definition on any path from the entry block to the entry/exit of
   a basic block.  Sets generate a definition, while clobbers kill
   a definition.

   In and out bitvectors are built for each basic block and are indexed by
   regnum (see df.h for details).  In and out bitvectors in struct
   df_live_bb_info actually refers to the must-initialized problem;

   Then, the in and out sets for the LIVE problem itself are computed.
   These are the logical AND of the IN and OUT sets from the LR problem
   and the must-initialized problem. 
----------------------------------------------------------------------------*/

/* Private data used to verify the solution for this problem.  */
struct df_live_problem_data
{
  bitmap *in;
  bitmap *out;
};

/* Scratch var used by transfer functions.  This is used to implement
   an optimization to reduce the amount of space used to compute the
   combined lr and live analysis.  */
static bitmap df_live_scratch;

/* Set basic block info.  */

static void
df_live_set_bb_info (unsigned int index, 
		   struct df_live_bb_info *bb_info)
{
  gcc_assert (df_live);
  gcc_assert (index < df_live->block_info_size);
  df_live->block_info[index] = bb_info;
}


/* Free basic block info.  */

static void
df_live_free_bb_info (basic_block bb ATTRIBUTE_UNUSED, 
		    void *vbb_info)
{
  struct df_live_bb_info *bb_info = (struct df_live_bb_info *) vbb_info;
  if (bb_info)
    {
      BITMAP_FREE (bb_info->gen);
      BITMAP_FREE (bb_info->kill);
      BITMAP_FREE (bb_info->in);
      BITMAP_FREE (bb_info->out);
      pool_free (df_live->block_pool, bb_info);
    }
}


/* Allocate or reset bitmaps for DF_LIVE blocks. The solution bits are
   not touched unless the block is new.  */

static void 
df_live_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  if (!df_live->block_pool)
    df_live->block_pool = create_alloc_pool ("df_live_block pool", 
					   sizeof (struct df_live_bb_info), 100);
  if (!df_live_scratch)
    df_live_scratch = BITMAP_ALLOC (NULL);

  df_grow_bb_info (df_live);

  EXECUTE_IF_SET_IN_BITMAP (df_live->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      struct df_live_bb_info *bb_info = df_live_get_bb_info (bb_index);
      if (bb_info)
	{ 
	  bitmap_clear (bb_info->kill);
	  bitmap_clear (bb_info->gen);
	}
      else
	{ 
	  bb_info = (struct df_live_bb_info *) pool_alloc (df_live->block_pool);
	  df_live_set_bb_info (bb_index, bb_info);
	  bb_info->kill = BITMAP_ALLOC (NULL);
	  bb_info->gen = BITMAP_ALLOC (NULL);
	  bb_info->in = BITMAP_ALLOC (NULL);
	  bb_info->out = BITMAP_ALLOC (NULL);
	}
    }
  df_live->optional_p = (optimize <= 1);
}


/* Reset the global solution for recalculation.  */

static void 
df_live_reset (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_live_bb_info *bb_info = df_live_get_bb_info (bb_index);
      gcc_assert (bb_info);
      bitmap_clear (bb_info->in);
      bitmap_clear (bb_info->out);
    }
}


/* Compute local uninitialized register info for basic block BB.  */

static void
df_live_bb_local_compute (unsigned int bb_index)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  struct df_live_bb_info *bb_info = df_live_get_bb_info (bb_index);
  rtx insn;
  df_ref *def_rec;
  int luid = 0;

  FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_insn_info *insn_info = DF_INSN_UID_GET (uid);

      /* Inserting labels does not always trigger the incremental
	 rescanning.  */
      if (!insn_info)
	{
	  gcc_assert (!INSN_P (insn));
	  insn_info = df_insn_create_insn_record (insn);
	}

      DF_INSN_INFO_LUID (insn_info) = luid;
      if (!INSN_P (insn))
	continue;

      luid++;
      for (def_rec = DF_INSN_INFO_DEFS (insn_info); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  unsigned int regno = DF_REF_REGNO (def);

	  if (DF_REF_FLAGS_IS_SET (def,
				   DF_REF_PARTIAL | DF_REF_CONDITIONAL))
	    /* All partial or conditional def
	       seen are included in the gen set. */
	    bitmap_set_bit (bb_info->gen, regno);
	  else if (DF_REF_FLAGS_IS_SET (def, DF_REF_MUST_CLOBBER))
	    /* Only must clobbers for the entire reg destroy the
	       value.  */
	    bitmap_set_bit (bb_info->kill, regno);
	  else if (! DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
	    bitmap_set_bit (bb_info->gen, regno);
	}
    }

  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      bitmap_set_bit (bb_info->gen, DF_REF_REGNO (def));
    }
}


/* Compute local uninitialized register info.  */

static void
df_live_local_compute (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  df_grow_insn_info ();

  EXECUTE_IF_SET_IN_BITMAP (df_live->out_of_date_transfer_functions, 
			    0, bb_index, bi)
    {
      df_live_bb_local_compute (bb_index);
    }

  bitmap_clear (df_live->out_of_date_transfer_functions);
}


/* Initialize the solution vectors.  */

static void 
df_live_init (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_live_bb_info *bb_info = df_live_get_bb_info (bb_index);
      struct df_lr_bb_info *bb_lr_info = df_lr_get_bb_info (bb_index);

      /* No register may reach a location where it is not used.  Thus
	 we trim the rr result to the places where it is used.  */
      bitmap_and (bb_info->out, bb_info->gen, bb_lr_info->out);
      bitmap_clear (bb_info->in);
    }
}

/* Forward confluence function that ignores fake edges.  */

static void
df_live_confluence_n (edge e)
{
  bitmap op1 = df_live_get_bb_info (e->dest->index)->in;
  bitmap op2 = df_live_get_bb_info (e->src->index)->out;
 
  if (e->flags & EDGE_FAKE) 
    return;

  bitmap_ior_into (op1, op2);
} 


/* Transfer function for the forwards must-initialized problem.  */

static bool
df_live_transfer_function (int bb_index)
{
  struct df_live_bb_info *bb_info = df_live_get_bb_info (bb_index);
  struct df_lr_bb_info *bb_lr_info = df_lr_get_bb_info (bb_index);
  bitmap in = bb_info->in;
  bitmap out = bb_info->out;
  bitmap gen = bb_info->gen;
  bitmap kill = bb_info->kill;

  /* We need to use a scratch set here so that the value returned from
     this function invocation properly reflects if the sets changed in
     a significant way; i.e. not just because the lr set was anded
     in.  */
  bitmap_and (df_live_scratch, gen, bb_lr_info->out);
  /* No register may reach a location where it is not used.  Thus
     we trim the rr result to the places where it is used.  */
  bitmap_and_into (in, bb_lr_info->in);

  return bitmap_ior_and_compl (out, df_live_scratch, in, kill);
}


/* And the LR info with the must-initialized registers, to produce the LIVE info.  */

static void
df_live_finalize (bitmap all_blocks)
{

  if (df_live->solutions_dirty)
    {
      bitmap_iterator bi;
      unsigned int bb_index;

      EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
	{
	  struct df_lr_bb_info *bb_lr_info = df_lr_get_bb_info (bb_index);
	  struct df_live_bb_info *bb_live_info = df_live_get_bb_info (bb_index);
  
	  /* No register may reach a location where it is not used.  Thus
	     we trim the rr result to the places where it is used.  */
	  bitmap_and_into (bb_live_info->in, bb_lr_info->in);
	  bitmap_and_into (bb_live_info->out, bb_lr_info->out);
	}
      
      df_live->solutions_dirty = false;
    }
}


/* Free all storage associated with the problem.  */

static void
df_live_free (void)
{
  if (df_live->block_info)
    {
      unsigned int i;
      
      for (i = 0; i < df_live->block_info_size; i++)
	{
	  struct df_live_bb_info *bb_info = df_live_get_bb_info (i);
	  if (bb_info)
	    {
	      BITMAP_FREE (bb_info->gen);
	      BITMAP_FREE (bb_info->kill);
	      BITMAP_FREE (bb_info->in);
	      BITMAP_FREE (bb_info->out);
	    }
	}
      
      free_alloc_pool (df_live->block_pool);
      df_live->block_info_size = 0;
      free (df_live->block_info);

      if (df_live_scratch)
	BITMAP_FREE (df_live_scratch);
    }
  BITMAP_FREE (df_live->out_of_date_transfer_functions);
  free (df_live);
}


/* Debugging info at top of bb.  */

static void
df_live_top_dump (basic_block bb, FILE *file)
{
  struct df_live_bb_info *bb_info = df_live_get_bb_info (bb->index);
  struct df_live_problem_data *problem_data;

  if (!bb_info || !bb_info->in)
    return;
      
  fprintf (file, ";; live  in  \t");
  df_print_regset (file, bb_info->in);
  if (df_live->problem_data)
    {
      problem_data = (struct df_live_problem_data *)df_live->problem_data;
      fprintf (file, ";;  old in  \t");
      df_print_regset (file, problem_data->in[bb->index]);
    }
  fprintf (file, ";; live  gen \t");
  df_print_regset (file, bb_info->gen);
  fprintf (file, ";; live  kill\t");
  df_print_regset (file, bb_info->kill);
}


/* Debugging info at bottom of bb.  */

static void
df_live_bottom_dump (basic_block bb, FILE *file)
{
  struct df_live_bb_info *bb_info = df_live_get_bb_info (bb->index);
  struct df_live_problem_data *problem_data;

  if (!bb_info || !bb_info->out)
    return;
      
  fprintf (file, ";; live  out \t");
  df_print_regset (file, bb_info->out);
  if (df_live->problem_data)
    {
      problem_data = (struct df_live_problem_data *)df_live->problem_data;
      fprintf (file, ";;  old out  \t");
      df_print_regset (file, problem_data->out[bb->index]);
    }
}


/* Build the datastructure to verify that the solution to the dataflow
   equations is not dirty.  */

static void
df_live_verify_solution_start (void)
{
  basic_block bb;
  struct df_live_problem_data *problem_data;
  if (df_live->solutions_dirty)
    {
      df_live->problem_data = NULL;
      return;
    }

  /* Set it true so that the solution is recomputed.  */ 
  df_live->solutions_dirty = true;

  problem_data = XNEW (struct df_live_problem_data);
  df_live->problem_data = problem_data;
  problem_data->in = XNEWVEC (bitmap, last_basic_block);
  problem_data->out = XNEWVEC (bitmap, last_basic_block);

  FOR_ALL_BB (bb)
    {
      problem_data->in[bb->index] = BITMAP_ALLOC (NULL);
      problem_data->out[bb->index] = BITMAP_ALLOC (NULL);
      bitmap_copy (problem_data->in[bb->index], DF_LIVE_IN (bb));
      bitmap_copy (problem_data->out[bb->index], DF_LIVE_OUT (bb));
    }
}


/* Compare the saved datastructure and the new solution to the dataflow
   equations.  */

static void
df_live_verify_solution_end (void)
{
  struct df_live_problem_data *problem_data;
  basic_block bb;

  if (df_live->problem_data == NULL)
    return;

  problem_data = (struct df_live_problem_data *)df_live->problem_data;

  FOR_ALL_BB (bb)
    {
      if ((!bitmap_equal_p (problem_data->in[bb->index], DF_LIVE_IN (bb)))
	  || (!bitmap_equal_p (problem_data->out[bb->index], DF_LIVE_OUT (bb))))
	{
	  /*df_dump (stderr);*/
	  gcc_unreachable ();
	}
    }

  /* Cannot delete them immediately because you may want to dump them
     if the comparison fails.  */
  FOR_ALL_BB (bb)
    {
      BITMAP_FREE (problem_data->in[bb->index]);
      BITMAP_FREE (problem_data->out[bb->index]);
    }

  free (problem_data->in);
  free (problem_data->out);
  free (problem_data);
  df_live->problem_data = NULL;
}


/* All of the information associated with every instance of the problem.  */

static struct df_problem problem_LIVE =
{
  DF_LIVE,                      /* Problem id.  */
  DF_FORWARD,                   /* Direction.  */
  df_live_alloc,                /* Allocate the problem specific data.  */
  df_live_reset,                /* Reset global information.  */
  df_live_free_bb_info,         /* Free basic block info.  */
  df_live_local_compute,        /* Local compute function.  */
  df_live_init,                 /* Init the solution specific data.  */
  df_worklist_dataflow,         /* Worklist solver.  */
  NULL,                         /* Confluence operator 0.  */ 
  df_live_confluence_n,         /* Confluence operator n.  */ 
  df_live_transfer_function,    /* Transfer function.  */
  df_live_finalize,             /* Finalize function.  */
  df_live_free,                 /* Free all of the problem information.  */
  df_live_free,                 /* Remove this problem from the stack of dataflow problems.  */
  NULL,                         /* Debugging.  */
  df_live_top_dump,             /* Debugging start block.  */
  df_live_bottom_dump,          /* Debugging end block.  */
  df_live_verify_solution_start,/* Incremental solution verify start.  */
  df_live_verify_solution_end,  /* Incremental solution verify end.  */
  &problem_LR,                  /* Dependent problem.  */
  TV_DF_LIVE,                   /* Timing variable.  */
  false                         /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_live_add_problem (void)
{
  df_add_problem (&problem_LIVE);
  /* These will be initialized when df_scan_blocks processes each
     block.  */
  df_live->out_of_date_transfer_functions = BITMAP_ALLOC (NULL);
}


/* Set all of the blocks as dirty.  This needs to be done if this
   problem is added after all of the insns have been scanned.  */

void
df_live_set_all_dirty (void)
{
  basic_block bb;
  FOR_ALL_BB (bb)
    bitmap_set_bit (df_live->out_of_date_transfer_functions, 
		    bb->index);
}


/* Verify that all of the lr related info is consistent and
   correct.  */

void
df_live_verify_transfer_functions (void)
{
  basic_block bb;
  bitmap saved_gen;
  bitmap saved_kill;
  bitmap all_blocks;

  if (!df)
    return;

  saved_gen = BITMAP_ALLOC (NULL);
  saved_kill = BITMAP_ALLOC (NULL);
  all_blocks = BITMAP_ALLOC (NULL);

  df_grow_insn_info ();

  FOR_ALL_BB (bb)
    {
      struct df_live_bb_info *bb_info = df_live_get_bb_info (bb->index);
      bitmap_set_bit (all_blocks, bb->index);

      if (bb_info)
	{
	  /* Make a copy of the transfer functions and then compute
	     new ones to see if the transfer functions have
	     changed.  */
	  if (!bitmap_bit_p (df_live->out_of_date_transfer_functions, 
			     bb->index))
	    {
	      bitmap_copy (saved_gen, bb_info->gen);
	      bitmap_copy (saved_kill, bb_info->kill);
	      bitmap_clear (bb_info->gen);
	      bitmap_clear (bb_info->kill);

	      df_live_bb_local_compute (bb->index);
	      gcc_assert (bitmap_equal_p (saved_gen, bb_info->gen));
	      gcc_assert (bitmap_equal_p (saved_kill, bb_info->kill));
	    }
	}
      else
	{
	  /* If we do not have basic block info, the block must be in
	     the list of dirty blocks or else some one has added a
	     block behind our backs. */
	  gcc_assert (bitmap_bit_p (df_live->out_of_date_transfer_functions, 
				    bb->index));
	}
      /* Make sure no one created a block without following
	 procedures.  */
      gcc_assert (df_scan_get_bb_info (bb->index));
    }

  /* Make sure there are no dirty bits in blocks that have been deleted.  */
  gcc_assert (!bitmap_intersect_compl_p (df_live->out_of_date_transfer_functions, 
					 all_blocks)); 
  BITMAP_FREE (saved_gen);
  BITMAP_FREE (saved_kill);
  BITMAP_FREE (all_blocks);
}

/*----------------------------------------------------------------------------
   CREATE DEF_USE (DU) and / or USE_DEF (UD) CHAINS

   Link either the defs to the uses and / or the uses to the defs.

   These problems are set up like the other dataflow problems so that
   they nicely fit into the framework.  They are much simpler and only
   involve a single traversal of instructions and an examination of
   the reaching defs information (the dependent problem).
----------------------------------------------------------------------------*/

#define df_chain_problem_p(FLAG) (((enum df_chain_flags)df_chain->local_flags)&(FLAG))

/* Create a du or ud chain from SRC to DST and link it into SRC.   */

struct df_link *
df_chain_create (df_ref src, df_ref dst)
{
  struct df_link *head = DF_REF_CHAIN (src);
  struct df_link *link = (struct df_link *) pool_alloc (df_chain->block_pool);
  
  DF_REF_CHAIN (src) = link;
  link->next = head;
  link->ref = dst;
  return link;
}


/* Delete any du or ud chains that start at REF and point to
   TARGET.  */ 
static void
df_chain_unlink_1 (df_ref ref, df_ref target)
{
  struct df_link *chain = DF_REF_CHAIN (ref);
  struct df_link *prev = NULL;

  while (chain)
    {
      if (chain->ref == target)
	{
	  if (prev)
	    prev->next = chain->next;
	  else
	    DF_REF_CHAIN (ref) = chain->next;
	  pool_free (df_chain->block_pool, chain);
	  return;
	}
      prev = chain;
      chain = chain->next;
    }
}


/* Delete a du or ud chain that leave or point to REF.  */

void
df_chain_unlink (df_ref ref)
{
  struct df_link *chain = DF_REF_CHAIN (ref);
  while (chain)
    {
      struct df_link *next = chain->next;
      /* Delete the other side if it exists.  */
      df_chain_unlink_1 (chain->ref, ref);
      pool_free (df_chain->block_pool, chain);
      chain = next;
    }
  DF_REF_CHAIN (ref) = NULL;
}


/* Copy the du or ud chain starting at FROM_REF and attach it to
   TO_REF.  */ 

void 
df_chain_copy (df_ref to_ref, 
	       struct df_link *from_ref)
{
  while (from_ref)
    {
      df_chain_create (to_ref, from_ref->ref);
      from_ref = from_ref->next;
    }
}


/* Remove this problem from the stack of dataflow problems.  */

static void
df_chain_remove_problem (void)
{
  bitmap_iterator bi;
  unsigned int bb_index;

  /* Wholesale destruction of the old chains.  */ 
  if (df_chain->block_pool)
    free_alloc_pool (df_chain->block_pool);

  EXECUTE_IF_SET_IN_BITMAP (df_chain->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      rtx insn;
      df_ref *def_rec;
      df_ref *use_rec;
      basic_block bb = BASIC_BLOCK (bb_index);

      if (df_chain_problem_p (DF_DU_CHAIN))
	for (def_rec = df_get_artificial_defs (bb->index); *def_rec; def_rec++)
	  DF_REF_CHAIN (*def_rec) = NULL;
      if (df_chain_problem_p (DF_UD_CHAIN))
	for (use_rec = df_get_artificial_uses (bb->index); *use_rec; use_rec++)
	  DF_REF_CHAIN (*use_rec) = NULL;
      
      FOR_BB_INSNS (bb, insn)
	{
	  unsigned int uid = INSN_UID (insn);
	  
	  if (INSN_P (insn))
	    {
	      if (df_chain_problem_p (DF_DU_CHAIN))
		for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
		  DF_REF_CHAIN (*def_rec) = NULL;
	      if (df_chain_problem_p (DF_UD_CHAIN))
		{
		  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
		    DF_REF_CHAIN (*use_rec) = NULL;
		  for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
		    DF_REF_CHAIN (*use_rec) = NULL;
		}
	    }
	}
    }

  bitmap_clear (df_chain->out_of_date_transfer_functions);
  df_chain->block_pool = NULL;
}


/* Remove the chain problem completely.  */

static void
df_chain_fully_remove_problem (void)
{
  df_chain_remove_problem ();
  BITMAP_FREE (df_chain->out_of_date_transfer_functions);
  free (df_chain);
}


/* Create def-use or use-def chains.  */

static void  
df_chain_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  df_chain_remove_problem ();
  df_chain->block_pool = create_alloc_pool ("df_chain_block pool", 
					 sizeof (struct df_link), 50);
  df_chain->optional_p = true;
}


/* Reset all of the chains when the set of basic blocks changes.  */

static void
df_chain_reset (bitmap blocks_to_clear ATTRIBUTE_UNUSED)
{
  df_chain_remove_problem ();
}


/* Create the chains for a list of USEs.  */

static void
df_chain_create_bb_process_use (bitmap local_rd,
				df_ref *use_rec,
				enum df_ref_flags top_flag)
{
  bitmap_iterator bi;
  unsigned int def_index;
  
  while (*use_rec)
    {
      df_ref use = *use_rec;
      unsigned int uregno = DF_REF_REGNO (use);
      if ((!(df->changeable_flags & DF_NO_HARD_REGS))
	  || (uregno >= FIRST_PSEUDO_REGISTER))
	{
	  /* Do not want to go through this for an uninitialized var.  */
	  int count = DF_DEFS_COUNT (uregno);
	  if (count)
	    {
	      if (top_flag == (DF_REF_FLAGS (use) & DF_REF_AT_TOP))
		{
		  unsigned int first_index = DF_DEFS_BEGIN (uregno);
		  unsigned int last_index = first_index + count - 1;
		  
		  EXECUTE_IF_SET_IN_BITMAP (local_rd, first_index, def_index, bi)
		    {
		      df_ref def;
		      if (def_index > last_index) 
			break;
		      
		      def = DF_DEFS_GET (def_index);
		      if (df_chain_problem_p (DF_DU_CHAIN))
			df_chain_create (def, use);
		      if (df_chain_problem_p (DF_UD_CHAIN))
			df_chain_create (use, def);
		    }
		}
	    }
	}

      use_rec++;
    }
}


/* Create chains from reaching defs bitmaps for basic block BB.  */

static void
df_chain_create_bb (unsigned int bb_index)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  struct df_rd_bb_info *bb_info = df_rd_get_bb_info (bb_index);
  rtx insn;
  bitmap cpy = BITMAP_ALLOC (NULL);
  df_ref *def_rec;

  bitmap_copy (cpy, bb_info->in);
  bitmap_set_bit (df_chain->out_of_date_transfer_functions, bb_index);

  /* Since we are going forwards, process the artificial uses first
     then the artificial defs second.  */

#ifdef EH_USES
  /* Create the chains for the artificial uses from the EH_USES at the
     beginning of the block.  */
  
  /* Artificials are only hard regs.  */
  if (!(df->changeable_flags & DF_NO_HARD_REGS))
    df_chain_create_bb_process_use (cpy,
				    df_get_artificial_uses (bb->index), 
				    DF_REF_AT_TOP);
#endif

  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
	    bitmap_clear_range (cpy, 
				DF_DEFS_BEGIN (dregno), 
				DF_DEFS_COUNT (dregno));
	  bitmap_set_bit (cpy, DF_REF_ID (def));
	}
    }
  
  /* Process the regular instructions next.  */
  FOR_BB_INSNS (bb, insn)
    {
      df_ref *def_rec;
      unsigned int uid = INSN_UID (insn);

      if (!INSN_P (insn))
	continue;

      /* Now scan the uses and link them up with the defs that remain
	 in the cpy vector.  */
      
      df_chain_create_bb_process_use (cpy, DF_INSN_UID_USES (uid), 0);

      if (df->changeable_flags & DF_EQ_NOTES)
	df_chain_create_bb_process_use (cpy, DF_INSN_UID_EQ_USES (uid), 0);


      /* Since we are going forwards, process the defs second.  This
         pass only changes the bits in cpy.  */
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  unsigned int dregno = DF_REF_REGNO (def);
	  if ((!(df->changeable_flags & DF_NO_HARD_REGS))
	      || (dregno >= FIRST_PSEUDO_REGISTER))
	    {
	      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
		bitmap_clear_range (cpy, 
				    DF_DEFS_BEGIN (dregno), 
				    DF_DEFS_COUNT (dregno));
	      if (!(DF_REF_FLAGS (def) 
		    & (DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER)))
		bitmap_set_bit (cpy, DF_REF_ID (def));
	    }
	}
    }

  /* Create the chains for the artificial uses of the hard registers
     at the end of the block.  */
  if (!(df->changeable_flags & DF_NO_HARD_REGS))
    df_chain_create_bb_process_use (cpy,
				    df_get_artificial_uses (bb->index), 
				    0);

  BITMAP_FREE (cpy);
}

/* Create def-use chains from reaching use bitmaps for basic blocks
   in BLOCKS.  */

static void
df_chain_finalize (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;
  
  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      df_chain_create_bb (bb_index);
    }
}


/* Free all storage associated with the problem.  */

static void
df_chain_free (void)
{
  free_alloc_pool (df_chain->block_pool);
  BITMAP_FREE (df_chain->out_of_date_transfer_functions);
  free (df_chain);
}


/* Debugging info.  */

static void
df_chain_top_dump (basic_block bb, FILE *file)
{
  if (df_chain_problem_p (DF_DU_CHAIN))
    {
      rtx insn;
      df_ref *def_rec = df_get_artificial_defs (bb->index);
      if (*def_rec)
	{
	  
	  fprintf (file, ";;  DU chains for artificial defs\n");
	  while (*def_rec)
	    {
	      df_ref def = *def_rec;
	      fprintf (file, ";;   reg %d ", DF_REF_REGNO (def));
	      df_chain_dump (DF_REF_CHAIN (def), file);
	      fprintf (file, "\n");
	      def_rec++;
	    }
	}      

      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	      def_rec = DF_INSN_INFO_DEFS (insn_info);
	      if (*def_rec)
		{
		  fprintf (file, ";;   DU chains for insn luid %d uid %d\n", 
			   DF_INSN_INFO_LUID (insn_info), INSN_UID (insn));
		  
		  while (*def_rec)
		    {
		      df_ref def = *def_rec;
		      fprintf (file, ";;      reg %d ", DF_REF_REGNO (def));
		      if (DF_REF_FLAGS (def) & DF_REF_READ_WRITE)
			fprintf (file, "read/write ");
		      df_chain_dump (DF_REF_CHAIN (def), file);
		      fprintf (file, "\n");
		      def_rec++;
		    }
		}
	    }
	}
    }
}


static void
df_chain_bottom_dump (basic_block bb, FILE *file)
{
  if (df_chain_problem_p (DF_UD_CHAIN))
    {
      rtx insn;
      df_ref *use_rec = df_get_artificial_uses (bb->index);

      if (*use_rec)
	{
	  fprintf (file, ";;  UD chains for artificial uses\n");
	  while (*use_rec)
	    {
	      df_ref use = *use_rec;
	      fprintf (file, ";;   reg %d ", DF_REF_REGNO (use));
	      df_chain_dump (DF_REF_CHAIN (use), file);
	      fprintf (file, "\n");
	      use_rec++;
	    }
	}      

      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	      df_ref *eq_use_rec = DF_INSN_INFO_EQ_USES (insn_info);
	      use_rec = DF_INSN_INFO_USES (insn_info);
	      if (*use_rec || *eq_use_rec)
		{
		  fprintf (file, ";;   UD chains for insn luid %d uid %d\n", 
			   DF_INSN_INFO_LUID (insn_info), INSN_UID (insn));
		  
		  while (*use_rec)
		    {
		      df_ref use = *use_rec;
		      fprintf (file, ";;      reg %d ", DF_REF_REGNO (use));
		      if (DF_REF_FLAGS (use) & DF_REF_READ_WRITE)
			fprintf (file, "read/write ");
		      df_chain_dump (DF_REF_CHAIN (use), file);
		      fprintf (file, "\n");
		      use_rec++;
		    }
		  while (*eq_use_rec)
		    {
		      df_ref use = *eq_use_rec;
		      fprintf (file, ";;   eq_note reg %d ", DF_REF_REGNO (use));
		      df_chain_dump (DF_REF_CHAIN (use), file);
		      fprintf (file, "\n");
		      eq_use_rec++;
		    }
		}
	    }
	}
    }
}


static struct df_problem problem_CHAIN =
{
  DF_CHAIN,                   /* Problem id.  */
  DF_NONE,                    /* Direction.  */
  df_chain_alloc,             /* Allocate the problem specific data.  */
  df_chain_reset,             /* Reset global information.  */
  NULL,                       /* Free basic block info.  */
  NULL,                       /* Local compute function.  */
  NULL,                       /* Init the solution specific data.  */
  NULL,                       /* Iterative solver.  */
  NULL,                       /* Confluence operator 0.  */ 
  NULL,                       /* Confluence operator n.  */ 
  NULL,                       /* Transfer function.  */
  df_chain_finalize,          /* Finalize function.  */
  df_chain_free,              /* Free all of the problem information.  */
  df_chain_fully_remove_problem,/* Remove this problem from the stack of dataflow problems.  */
  NULL,                       /* Debugging.  */
  df_chain_top_dump,          /* Debugging start block.  */
  df_chain_bottom_dump,       /* Debugging end block.  */
  NULL,                       /* Incremental solution verify start.  */
  NULL,                       /* Incremental solution verify end.  */
  &problem_RD,                /* Dependent problem.  */
  TV_DF_CHAIN,                /* Timing variable.  */
  false                       /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_chain_add_problem (enum df_chain_flags chain_flags)
{
  df_add_problem (&problem_CHAIN);
  df_chain->local_flags = (unsigned int)chain_flags;
  df_chain->out_of_date_transfer_functions = BITMAP_ALLOC (NULL);
}

#undef df_chain_problem_p


/*----------------------------------------------------------------------------
   BYTE LEVEL LIVE REGISTERS

   Find the locations in the function where any use of a pseudo can
   reach in the backwards direction.  In and out bitvectors are built
   for each basic block.  There are two mapping functions,
   df_byte_lr_get_regno_start and df_byte_lr_get_regno_len that are
   used to map regnos into bit vector positions.

   This problem differs from the regular df_lr function in the way
   that subregs, *_extracts and strict_low_parts are handled. In lr
   these are consider partial kills, here, the exact set of bytes is
   modeled.  Note that any reg that has none of these operations is
   only modeled with a single bit since all operations access the
   entire register.

   This problem is more brittle that the regular lr.  It currently can
   be used in dce incrementally, but cannot be used in an environment
   where insns are created or modified.  The problem is that the
   mapping of regnos to bitmap positions is relatively compact, in
   that if a pseudo does not do any of the byte wise operations, only
   one slot is allocated, rather than a slot for each byte.  If insn
   are created, where a subreg is used for a reg that had no subregs,
   the mapping would be wrong.  Likewise, there are no checks to see
   that new pseudos have been added.  These issues could be addressed
   by adding a problem specific flag to not use the compact mapping,
   if there was a need to do so.

   ----------------------------------------------------------------------------*/

/* Private data used to verify the solution for this problem.  */
struct df_byte_lr_problem_data
{
  /* Expanded versions of bitvectors used in lr.  */
  bitmap invalidated_by_call;
  bitmap hardware_regs_used;

  /* Indexed by regno, this is true if there are subregs, extracts or
     strict_low_parts for this regno.  */
  bitmap needs_expansion;

  /* The start position and len for each regno in the various bit
     vectors.  */ 
  unsigned int* regno_start;  
  unsigned int* regno_len;
  /* An obstack for the bitmaps we need for this problem.  */
  bitmap_obstack byte_lr_bitmaps;
};


/* Get the starting location for REGNO in the df_byte_lr bitmaps.  */

int 
df_byte_lr_get_regno_start (unsigned int regno)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;;
  return problem_data->regno_start[regno];
}


/* Get the len for REGNO in the df_byte_lr bitmaps.  */

int 
df_byte_lr_get_regno_len (unsigned int regno)
{  
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;;
  return problem_data->regno_len[regno];
}


/* Set basic block info.  */

static void
df_byte_lr_set_bb_info (unsigned int index, 
			struct df_byte_lr_bb_info *bb_info)
{
  gcc_assert (df_byte_lr);
  gcc_assert (index < df_byte_lr->block_info_size);
  df_byte_lr->block_info[index] = bb_info;
}

 
/* Free basic block info.  */

static void
df_byte_lr_free_bb_info (basic_block bb ATTRIBUTE_UNUSED, 
			 void *vbb_info)
{
  struct df_byte_lr_bb_info *bb_info = (struct df_byte_lr_bb_info *) vbb_info;
  if (bb_info)
    {
      BITMAP_FREE (bb_info->use);
      BITMAP_FREE (bb_info->def);
      BITMAP_FREE (bb_info->in);
      BITMAP_FREE (bb_info->out);
      pool_free (df_byte_lr->block_pool, bb_info);
    }
}


/* Check all of the refs in REF_REC to see if any of them are
   extracts, subregs or strict_low_parts.  */

static void
df_byte_lr_check_regs (df_ref *ref_rec)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;

  for (; *ref_rec; ref_rec++)
    {
      df_ref ref = *ref_rec;
      if (DF_REF_FLAGS_IS_SET (ref, DF_REF_SIGN_EXTRACT 
			       | DF_REF_ZERO_EXTRACT 
			       | DF_REF_STRICT_LOW_PART)
	  || GET_CODE (DF_REF_REG (ref)) == SUBREG)
	bitmap_set_bit (problem_data->needs_expansion, DF_REF_REGNO (ref));
    }
}


/* Expand bitmap SRC which is indexed by regno to DEST which is indexed by 
   regno_start and regno_len.  */

static void
df_byte_lr_expand_bitmap (bitmap dest, bitmap src)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  bitmap_iterator bi;
  unsigned int i;

  bitmap_clear (dest);
  EXECUTE_IF_SET_IN_BITMAP (src, 0, i, bi)
    {
      bitmap_set_range (dest, problem_data->regno_start[i], 
			problem_data->regno_len[i]);
    }
}


/* Allocate or reset bitmaps for DF_BYTE_LR blocks. The solution bits are
   not touched unless the block is new.  */

static void 
df_byte_lr_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;
  basic_block bb;
  unsigned int regno;
  unsigned int index = 0;
  unsigned int max_reg = max_reg_num();
  struct df_byte_lr_problem_data *problem_data 
    = problem_data = XNEW (struct df_byte_lr_problem_data);

  df_byte_lr->problem_data = problem_data;

  if (!df_byte_lr->block_pool)
    df_byte_lr->block_pool = create_alloc_pool ("df_byte_lr_block pool", 
					   sizeof (struct df_byte_lr_bb_info), 50);

  df_grow_bb_info (df_byte_lr);

  /* Create the mapping from regnos to slots. This does not change
     unless the problem is destroyed and recreated.  In particular, if
     we end up deleting the only insn that used a subreg, we do not
     want to redo the mapping because this would invalidate everything
     else.  */

  bitmap_obstack_initialize (&problem_data->byte_lr_bitmaps);
  problem_data->regno_start = XNEWVEC (unsigned int, max_reg);
  problem_data->regno_len = XNEWVEC (unsigned int, max_reg);
  problem_data->hardware_regs_used = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
  problem_data->invalidated_by_call = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
  problem_data->needs_expansion = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
  
  /* Discover which regno's use subregs, extracts or
     strict_low_parts.  */
  FOR_EACH_BB (bb)
    {
      rtx insn;
      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	      df_byte_lr_check_regs (DF_INSN_INFO_DEFS (insn_info));
	      df_byte_lr_check_regs (DF_INSN_INFO_USES (insn_info));
	    }
	}
      bitmap_set_bit (df_byte_lr->out_of_date_transfer_functions, bb->index);
    }

  bitmap_set_bit (df_byte_lr->out_of_date_transfer_functions, ENTRY_BLOCK);
  bitmap_set_bit (df_byte_lr->out_of_date_transfer_functions, EXIT_BLOCK);
  
  /* Allocate the slots for each regno.  */
  for (regno = 0; regno < max_reg; regno++)
    {
      int len;
      problem_data->regno_start[regno] = index;
      if (bitmap_bit_p (problem_data->needs_expansion, regno))
	len = GET_MODE_SIZE (GET_MODE (regno_reg_rtx[regno]));
      else 
	len = 1;
      
      problem_data->regno_len[regno] = len;
      index += len;
    }

  df_byte_lr_expand_bitmap (problem_data->hardware_regs_used, 
			    df->hardware_regs_used);
  df_byte_lr_expand_bitmap (problem_data->invalidated_by_call, 
			    regs_invalidated_by_call_regset);

  EXECUTE_IF_SET_IN_BITMAP (df_byte_lr->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb_index);
      if (bb_info)
	{ 
	  bitmap_clear (bb_info->def);
	  bitmap_clear (bb_info->use);
	}
      else
	{ 
	  bb_info = (struct df_byte_lr_bb_info *) pool_alloc (df_byte_lr->block_pool);
	  df_byte_lr_set_bb_info (bb_index, bb_info);
	  bb_info->use = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
	  bb_info->def = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
	  bb_info->in = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
	  bb_info->out = BITMAP_ALLOC (&problem_data->byte_lr_bitmaps);
	}
    }
  
  df_byte_lr->optional_p = true;
}


/* Reset the global solution for recalculation.  */

static void 
df_byte_lr_reset (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb_index);
      gcc_assert (bb_info);
      bitmap_clear (bb_info->in);
      bitmap_clear (bb_info->out);
    }
}


/* Compute local live register info for basic block BB.  */

static void
df_byte_lr_bb_local_compute (unsigned int bb_index)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  basic_block bb = BASIC_BLOCK (bb_index);
  struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb_index);
  rtx insn;
  df_ref *def_rec;
  df_ref *use_rec;

  /* Process the registers set in an exception handler.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  unsigned int start = problem_data->regno_start[dregno];
	  unsigned int len = problem_data->regno_len[dregno];
	  bitmap_set_range (bb_info->def, start, len);
	  bitmap_clear_range (bb_info->use, start, len);
	}
    }

  /* Process the hardware registers that are always live.  */
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      /* Add use to set of uses in this BB.  */
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int uregno = DF_REF_REGNO (use);
	  unsigned int start = problem_data->regno_start[uregno];
	  unsigned int len = problem_data->regno_len[uregno];
	  bitmap_set_range (bb_info->use, start, len);
	}
    }

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);

      if (!INSN_P (insn))
	continue;	

      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  /* If the def is to only part of the reg, it does
	     not kill the other defs that reach here.  */
	  if (!(DF_REF_FLAGS (def) & (DF_REF_CONDITIONAL)))
	    {
	      unsigned int dregno = DF_REF_REGNO (def);
	      unsigned int start = problem_data->regno_start[dregno];
	      unsigned int len = problem_data->regno_len[dregno];
	      unsigned int sb;
	      unsigned int lb;
	      if (!df_compute_accessed_bytes (def, DF_MM_MUST, &sb, &lb))
		{
		  start += sb;
		  len = lb - sb;
		}
	      if (len)
		{
		  bitmap_set_range (bb_info->def, start, len);
		  bitmap_clear_range (bb_info->use, start, len);
		}
	    }
	}

      for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  unsigned int uregno = DF_REF_REGNO (use);
	  unsigned int start = problem_data->regno_start[uregno];
	  unsigned int len = problem_data->regno_len[uregno];
	  unsigned int sb;
	  unsigned int lb;
	  if (!df_compute_accessed_bytes (use, DF_MM_MAY, &sb, &lb))
	    {
	      start += sb;
	      len = lb - sb;
	    }
	  /* Add use to set of uses in this BB.  */
	  if (len)
	    bitmap_set_range (bb_info->use, start, len);
	}
    }

  /* Process the registers set in an exception handler or the hard
     frame pointer if this block is the target of a non local
     goto.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  unsigned int start = problem_data->regno_start[dregno];
	  unsigned int len = problem_data->regno_len[dregno];
	  bitmap_set_range (bb_info->def, start, len);
	  bitmap_clear_range (bb_info->use, start, len);
	}
    }
  
#ifdef EH_USES
  /* Process the uses that are live into an exception handler.  */
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      /* Add use to set of uses in this BB.  */
      if (DF_REF_FLAGS (use) & DF_REF_AT_TOP)
	{
	  unsigned int uregno = DF_REF_REGNO (use);
	  unsigned int start = problem_data->regno_start[uregno];
	  unsigned int len = problem_data->regno_len[uregno];
	  bitmap_set_range (bb_info->use, start, len);
	}
    }
#endif
}


/* Compute local live register info for each basic block within BLOCKS.  */

static void
df_byte_lr_local_compute (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (df_byte_lr->out_of_date_transfer_functions, 0, bb_index, bi)
    {
      if (bb_index == EXIT_BLOCK)
	{
	  /* The exit block is special for this problem and its bits are
	     computed from thin air.  */
	  struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (EXIT_BLOCK);
	  df_byte_lr_expand_bitmap (bb_info->use, df->exit_block_uses);
	}
      else
	df_byte_lr_bb_local_compute (bb_index);
    }

  bitmap_clear (df_byte_lr->out_of_date_transfer_functions);
}


/* Initialize the solution vectors.  */

static void 
df_byte_lr_init (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
    {
      struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb_index);
      bitmap_copy (bb_info->in, bb_info->use);
      bitmap_clear (bb_info->out);
    }
}


/* Confluence function that processes infinite loops.  This might be a
   noreturn function that throws.  And even if it isn't, getting the
   unwind info right helps debugging.  */
static void
df_byte_lr_confluence_0 (basic_block bb)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  bitmap op1 = df_byte_lr_get_bb_info (bb->index)->out;
  if (bb != EXIT_BLOCK_PTR)
    bitmap_copy (op1, problem_data->hardware_regs_used);
} 


/* Confluence function that ignores fake edges.  */

static void
df_byte_lr_confluence_n (edge e)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  bitmap op1 = df_byte_lr_get_bb_info (e->src->index)->out;
  bitmap op2 = df_byte_lr_get_bb_info (e->dest->index)->in;
 
  /* Call-clobbered registers die across exception and call edges.  */
  /* ??? Abnormal call edges ignored for the moment, as this gets
     confused by sibling call edges, which crashes reg-stack.  */
  if (e->flags & EDGE_EH)
    bitmap_ior_and_compl_into (op1, op2, problem_data->invalidated_by_call);
  else
    bitmap_ior_into (op1, op2);

  bitmap_ior_into (op1, problem_data->hardware_regs_used);
} 


/* Transfer function.  */

static bool
df_byte_lr_transfer_function (int bb_index)
{
  struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb_index);
  bitmap in = bb_info->in;
  bitmap out = bb_info->out;
  bitmap use = bb_info->use;
  bitmap def = bb_info->def;

  return bitmap_ior_and_compl (in, use, out, def);
}


/* Free all storage associated with the problem.  */

static void
df_byte_lr_free (void)
{
  struct df_byte_lr_problem_data *problem_data
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;


  if (df_byte_lr->block_info)
    {
      free_alloc_pool (df_byte_lr->block_pool);
      df_byte_lr->block_info_size = 0;
      free (df_byte_lr->block_info);
    }

  BITMAP_FREE (df_byte_lr->out_of_date_transfer_functions);
  bitmap_obstack_release (&problem_data->byte_lr_bitmaps);
  free (problem_data->regno_start);
  free (problem_data->regno_len);
  free (problem_data);
  free (df_byte_lr);
}


/* Debugging info at top of bb.  */

static void
df_byte_lr_top_dump (basic_block bb, FILE *file)
{
  struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb->index);
  if (!bb_info || !bb_info->in)
    return;
      
  fprintf (file, ";; blr  in  \t");
  df_print_byte_regset (file, bb_info->in);
  fprintf (file, ";; blr  use \t");
  df_print_byte_regset (file, bb_info->use);
  fprintf (file, ";; blr  def \t");
  df_print_byte_regset (file, bb_info->def);
}  


/* Debugging info at bottom of bb.  */

static void
df_byte_lr_bottom_dump (basic_block bb, FILE *file)
{
  struct df_byte_lr_bb_info *bb_info = df_byte_lr_get_bb_info (bb->index);
  if (!bb_info || !bb_info->out)
    return;
  
  fprintf (file, ";; blr  out \t");
  df_print_byte_regset (file, bb_info->out);
}  


/* All of the information associated with every instance of the problem.  */

static struct df_problem problem_BYTE_LR =
{
  DF_BYTE_LR,                      /* Problem id.  */
  DF_BACKWARD,                     /* Direction.  */
  df_byte_lr_alloc,                /* Allocate the problem specific data.  */
  df_byte_lr_reset,                /* Reset global information.  */
  df_byte_lr_free_bb_info,         /* Free basic block info.  */
  df_byte_lr_local_compute,        /* Local compute function.  */
  df_byte_lr_init,                 /* Init the solution specific data.  */
  df_worklist_dataflow,            /* Worklist solver.  */
  df_byte_lr_confluence_0,         /* Confluence operator 0.  */ 
  df_byte_lr_confluence_n,         /* Confluence operator n.  */ 
  df_byte_lr_transfer_function,    /* Transfer function.  */
  NULL,                            /* Finalize function.  */
  df_byte_lr_free,                 /* Free all of the problem information.  */
  df_byte_lr_free,                 /* Remove this problem from the stack of dataflow problems.  */
  NULL,                            /* Debugging.  */
  df_byte_lr_top_dump,             /* Debugging start block.  */
  df_byte_lr_bottom_dump,          /* Debugging end block.  */
  NULL,                            /* Incremental solution verify start.  */
  NULL,                            /* Incremental solution verify end.  */
  NULL,                            /* Dependent problem.  */
  TV_DF_BYTE_LR,                   /* Timing variable.  */ 
  false                            /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_byte_lr_add_problem (void)
{
  df_add_problem (&problem_BYTE_LR);
  /* These will be initialized when df_scan_blocks processes each
     block.  */
  df_byte_lr->out_of_date_transfer_functions = BITMAP_ALLOC (NULL);
}


/* Simulate the effects of the defs of INSN on LIVE.  */

void
df_byte_lr_simulate_defs (rtx insn, bitmap live)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  df_ref *def_rec;
  unsigned int uid = INSN_UID (insn);

  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;

      /* If the def is to only part of the reg, it does
	 not kill the other defs that reach here.  */
      if (!(DF_REF_FLAGS (def) & DF_REF_CONDITIONAL))
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  unsigned int start = problem_data->regno_start[dregno];
	  unsigned int len = problem_data->regno_len[dregno];
	  unsigned int sb;
	  unsigned int lb;
	  if (!df_compute_accessed_bytes (def, DF_MM_MUST, &sb, &lb))
	    {
	      start += sb;
	      len = lb - sb;
	    }

	  if (len)
	    bitmap_clear_range (live, start, len);
	}
    }
}  


/* Simulate the effects of the uses of INSN on LIVE.  */

void 
df_byte_lr_simulate_uses (rtx insn, bitmap live)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  df_ref *use_rec;
  unsigned int uid = INSN_UID (insn);

  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      unsigned int uregno = DF_REF_REGNO (use);
      unsigned int start = problem_data->regno_start[uregno];
      unsigned int len = problem_data->regno_len[uregno];
      unsigned int sb;
      unsigned int lb;
      
      if (!df_compute_accessed_bytes (use, DF_MM_MAY, &sb, &lb))
	{
	  start += sb;
	  len = lb - sb;
	}
      
      /* Add use to set of uses in this BB.  */
      if (len)
	bitmap_set_range (live, start, len);
    }
}


/* Apply the artificial uses and defs at the top of BB in a forwards
   direction.  */

void 
df_byte_lr_simulate_artificial_refs_at_top (basic_block bb, bitmap live)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  df_ref *def_rec;
#ifdef EH_USES
  df_ref *use_rec;
#endif
  int bb_index = bb->index;
  
#ifdef EH_USES
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if (DF_REF_FLAGS (use) & DF_REF_AT_TOP)
	{
	  unsigned int uregno = DF_REF_REGNO (use);
	  unsigned int start = problem_data->regno_start[uregno];
	  unsigned int len = problem_data->regno_len[uregno];
	  bitmap_set_range (live, start, len);
	}
    }
#endif

  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	{      
	  unsigned int dregno = DF_REF_REGNO (def);
	  unsigned int start = problem_data->regno_start[dregno];
	  unsigned int len = problem_data->regno_len[dregno];
	  bitmap_clear_range (live, start, len);
	}
    }
}


/* Apply the artificial uses and defs at the end of BB in a backwards
   direction.  */

void 
df_byte_lr_simulate_artificial_refs_at_end (basic_block bb, bitmap live)
{
  struct df_byte_lr_problem_data *problem_data 
    = (struct df_byte_lr_problem_data *)df_byte_lr->problem_data;
  df_ref *def_rec;
  df_ref *use_rec;
  int bb_index = bb->index;
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int dregno = DF_REF_REGNO (def);
	  unsigned int start = problem_data->regno_start[dregno];
	  unsigned int len = problem_data->regno_len[dregno];
	  bitmap_clear_range (live, start, len);
	}
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int uregno = DF_REF_REGNO (use);
	  unsigned int start = problem_data->regno_start[uregno];
	  unsigned int len = problem_data->regno_len[uregno];
	  bitmap_set_range (live, start, len);
	}
    }
}



/*----------------------------------------------------------------------------
   This problem computes REG_DEAD and REG_UNUSED notes.
   ----------------------------------------------------------------------------*/

static void 
df_note_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  df_note->optional_p = true;
}

#ifdef REG_DEAD_DEBUGGING
static void 
df_print_note (const char *prefix, rtx insn, rtx note)
{
  if (dump_file)
    {
      fprintf (dump_file, "%s %d ", prefix, INSN_UID (insn));
      print_rtl (dump_file, note);
      fprintf (dump_file, "\n");
    }
}
#endif


/* After reg-stack, the x86 floating point stack regs are difficult to
   analyze because of all of the pushes, pops and rotations.  Thus, we
   just leave the notes alone. */

#ifdef STACK_REGS
static inline bool 
df_ignore_stack_reg (int regno)
{
  return regstack_completed
    && IN_RANGE (regno, FIRST_STACK_REG, LAST_STACK_REG);
}
#else
static inline bool 
df_ignore_stack_reg (int regno ATTRIBUTE_UNUSED)
{
  return false;
}
#endif


/* Remove all of the REG_DEAD or REG_UNUSED notes from INSN and add
   them to OLD_DEAD_NOTES and OLD_UNUSED_NOTES.  */

static void
df_kill_notes (rtx insn, rtx *old_dead_notes, rtx *old_unused_notes)
{
  rtx *pprev = &REG_NOTES (insn);
  rtx link = *pprev;
  rtx dead = NULL;
  rtx unused = NULL;

  while (link)
    {
      switch (REG_NOTE_KIND (link))
	{
	case REG_DEAD:
	  /* After reg-stack, we need to ignore any unused notes 
	     for the stack registers.  */
	  if (df_ignore_stack_reg (REGNO (XEXP (link, 0))))
	    {
	      pprev = &XEXP (link, 1);
	      link = *pprev;
	    }
	  else
	    {
	      rtx next = XEXP (link, 1);
#ifdef REG_DEAD_DEBUGGING
	      df_print_note ("deleting: ", insn, link);
#endif
	      XEXP (link, 1) = dead;
	      dead = link;
	      *pprev = link = next;
	    }
	  break;

	case REG_UNUSED:
	  /* After reg-stack, we need to ignore any unused notes 
	     for the stack registers.  */
	  if (df_ignore_stack_reg (REGNO (XEXP (link, 0))))
	    {
	      pprev = &XEXP (link, 1);
	      link = *pprev;
	    }
	  else
	    {
	      rtx next = XEXP (link, 1);
#ifdef REG_DEAD_DEBUGGING
	      df_print_note ("deleting: ", insn, link);
#endif
	      XEXP (link, 1) = unused;
	      unused = link;
	      *pprev = link = next;
	    }
	  break;
	  
	default:
	  pprev = &XEXP (link, 1);
	  link = *pprev;
	  break;
	}
    }

  *old_dead_notes = dead;
  *old_unused_notes = unused;
}


/* Set a NOTE_TYPE note for REG in INSN.  Try to pull it from the OLD
   list, otherwise create a new one.  */

static inline rtx
df_set_note (enum reg_note note_type, rtx insn, rtx old, rtx reg)
{
  rtx curr = old;
  rtx prev = NULL;

  while (curr)
    if (XEXP (curr, 0) == reg)
      {
	if (prev)
	  XEXP (prev, 1) = XEXP (curr, 1);
	else
	  old = XEXP (curr, 1);
	XEXP (curr, 1) = REG_NOTES (insn);
	REG_NOTES (insn) = curr;
	return old;
      }
    else
      {
	prev = curr;
	curr = XEXP (curr, 1);
      }
  
  /* Did not find the note.  */
  add_reg_note (insn, note_type, reg);
  return old;
}

/* A subroutine of df_set_unused_notes_for_mw, with a selection of its
   arguments.  Return true if the register value described by MWS's
   mw_reg is known to be completely unused, and if mw_reg can therefore
   be used in a REG_UNUSED note.  */

static bool
df_whole_mw_reg_unused_p (struct df_mw_hardreg *mws,
			  bitmap live, bitmap artificial_uses)
{
  unsigned int r;

  /* If MWS describes a partial reference, create REG_UNUSED notes for
     individual hard registers.  */
  if (mws->flags & DF_REF_PARTIAL)
    return false;

  /* Likewise if some part of the register is used.  */
  for (r = mws->start_regno; r <= mws->end_regno; r++)
    if (bitmap_bit_p (live, r)
	|| bitmap_bit_p (artificial_uses, r))
      return false;

  gcc_assert (REG_P (mws->mw_reg));
  return true;
}

/* Set the REG_UNUSED notes for the multiword hardreg defs in INSN
   based on the bits in LIVE.  Do not generate notes for registers in
   artificial uses.  DO_NOT_GEN is updated so that REG_DEAD notes are
   not generated if the reg is both read and written by the
   instruction.
*/

static rtx
df_set_unused_notes_for_mw (rtx insn, rtx old, struct df_mw_hardreg *mws,
			    bitmap live, bitmap do_not_gen, 
			    bitmap artificial_uses)
{
  unsigned int r;
  
#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    fprintf (dump_file, "mw_set_unused looking at mws[%d..%d]\n", 
	     mws->start_regno, mws->end_regno);
#endif

  if (df_whole_mw_reg_unused_p (mws, live, artificial_uses))
    {
      unsigned int regno = mws->start_regno;
      old = df_set_note (REG_UNUSED, insn, old, mws->mw_reg);

#ifdef REG_DEAD_DEBUGGING
      df_print_note ("adding 1: ", insn, REG_NOTES (insn));
#endif
      bitmap_set_bit (do_not_gen, regno);
      /* Only do this if the value is totally dead.  */
    }
  else
    for (r = mws->start_regno; r <= mws->end_regno; r++)
      {
	if (!bitmap_bit_p (live, r)
	    && !bitmap_bit_p (artificial_uses, r))
	  {
	    old = df_set_note (REG_UNUSED, insn, old, regno_reg_rtx[r]);
#ifdef REG_DEAD_DEBUGGING
	    df_print_note ("adding 2: ", insn, REG_NOTES (insn));
#endif
	  }
	bitmap_set_bit (do_not_gen, r);
      }
  return old;
}


/* A subroutine of df_set_dead_notes_for_mw, with a selection of its
   arguments.  Return true if the register value described by MWS's
   mw_reg is known to be completely dead, and if mw_reg can therefore
   be used in a REG_DEAD note.  */

static bool
df_whole_mw_reg_dead_p (struct df_mw_hardreg *mws,
			bitmap live, bitmap artificial_uses,
			bitmap do_not_gen)
{
  unsigned int r;

  /* If MWS describes a partial reference, create REG_DEAD notes for
     individual hard registers.  */
  if (mws->flags & DF_REF_PARTIAL)
    return false;

  /* Likewise if some part of the register is not dead.  */
  for (r = mws->start_regno; r <= mws->end_regno; r++)
    if (bitmap_bit_p (live, r)
	|| bitmap_bit_p (artificial_uses, r)
	|| bitmap_bit_p (do_not_gen, r))
      return false;

  gcc_assert (REG_P (mws->mw_reg));
  return true;
}

/* Set the REG_DEAD notes for the multiword hardreg use in INSN based
   on the bits in LIVE.  DO_NOT_GEN is used to keep REG_DEAD notes
   from being set if the instruction both reads and writes the
   register.  */

static rtx
df_set_dead_notes_for_mw (rtx insn, rtx old, struct df_mw_hardreg *mws,
			  bitmap live, bitmap do_not_gen,
			  bitmap artificial_uses)
{
  unsigned int r;
  
#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    {
      fprintf (dump_file, "mw_set_dead looking at mws[%d..%d]\n  do_not_gen =", 
	       mws->start_regno, mws->end_regno);
      df_print_regset (dump_file, do_not_gen);
      fprintf (dump_file, "  live =");
      df_print_regset (dump_file, live);
      fprintf (dump_file, "  artificial uses =");
      df_print_regset (dump_file, artificial_uses);
    }
#endif

  if (df_whole_mw_reg_dead_p (mws, live, artificial_uses, do_not_gen))
    {
      /* Add a dead note for the entire multi word register.  */
      old = df_set_note (REG_DEAD, insn, old, mws->mw_reg);
#ifdef REG_DEAD_DEBUGGING
      df_print_note ("adding 1: ", insn, REG_NOTES (insn));
#endif
    }
  else
    {
      for (r = mws->start_regno; r <= mws->end_regno; r++)
	if (!bitmap_bit_p (live, r)
	    && !bitmap_bit_p (artificial_uses, r)
	    && !bitmap_bit_p (do_not_gen, r))
	  {
	    old = df_set_note (REG_DEAD, insn, old, regno_reg_rtx[r]);
#ifdef REG_DEAD_DEBUGGING
	    df_print_note ("adding 2: ", insn, REG_NOTES (insn));
#endif
	  }
    }
  return old;
}


/* Create a REG_UNUSED note if necessary for DEF in INSN updating
   LIVE.  Do not generate notes for registers in ARTIFICIAL_USES.  */

static rtx
df_create_unused_note (rtx insn, rtx old, df_ref def, 
		       bitmap live, bitmap artificial_uses)
{
  unsigned int dregno = DF_REF_REGNO (def);
  
#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    {
      fprintf (dump_file, "  regular looking at def ");
      df_ref_debug (def, dump_file);
    }
#endif

  if (!(bitmap_bit_p (live, dregno)
	|| (DF_REF_FLAGS (def) & DF_REF_MW_HARDREG)
	|| bitmap_bit_p (artificial_uses, dregno)
	|| df_ignore_stack_reg (dregno)))
    {
      rtx reg = (DF_REF_LOC (def)) 
                ? *DF_REF_REAL_LOC (def): DF_REF_REG (def);
      old = df_set_note (REG_UNUSED, insn, old, reg);
#ifdef REG_DEAD_DEBUGGING
      df_print_note ("adding 3: ", insn, REG_NOTES (insn));
#endif
    }
  
  return old;
}


/* Recompute the REG_DEAD and REG_UNUSED notes and compute register
   info: lifetime, bb, and number of defs and uses for basic block
   BB.  The three bitvectors are scratch regs used here.  */

static void
df_note_bb_compute (unsigned int bb_index, 
		    bitmap live, bitmap do_not_gen, bitmap artificial_uses)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  rtx insn;
  df_ref *def_rec;
  df_ref *use_rec;

  bitmap_copy (live, df_get_live_out (bb));
  bitmap_clear (artificial_uses);

#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    {
      fprintf (dump_file, "live at bottom ");
      df_print_regset (dump_file, live);
    }
#endif

  /* Process the artificial defs and uses at the bottom of the block
     to begin processing.  */
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
#ifdef REG_DEAD_DEBUGGING
      if (dump_file)
	fprintf (dump_file, "artificial def %d\n", DF_REF_REGNO (def));
#endif

      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	{
	  unsigned int regno = DF_REF_REGNO (use);
	  bitmap_set_bit (live, regno);
	  
	  /* Notes are not generated for any of the artificial registers
	     at the bottom of the block.  */
	  bitmap_set_bit (artificial_uses, regno);
	}
    }
  
#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    {
      fprintf (dump_file, "live before artificials out ");
      df_print_regset (dump_file, live);
    }
#endif

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      struct df_mw_hardreg **mws_rec;
      rtx old_dead_notes;
      rtx old_unused_notes;
 
      if (!INSN_P (insn))
	continue;

      bitmap_clear (do_not_gen);
      df_kill_notes (insn, &old_dead_notes, &old_unused_notes);

      /* Process the defs.  */
      if (CALL_P (insn))
	{
#ifdef REG_DEAD_DEBUGGING
	  if (dump_file)
	    {
	      fprintf (dump_file, "processing call %d\n  live =", INSN_UID (insn));
	      df_print_regset (dump_file, live);
	    }
#endif
	  /* We only care about real sets for calls.  Clobbers cannot
	     be depended on to really die.  */
	  mws_rec = DF_INSN_UID_MWS (uid);
	  while (*mws_rec)
	    {
	      struct df_mw_hardreg *mws = *mws_rec; 
	      if ((DF_MWS_REG_DEF_P (mws)) 
		  && !df_ignore_stack_reg (mws->start_regno))
		old_unused_notes 
		  = df_set_unused_notes_for_mw (insn, old_unused_notes, 
						mws, live, do_not_gen, 
						artificial_uses);
	      mws_rec++;
	    }

	  /* All of the defs except the return value are some sort of
	     clobber.  This code is for the return.  */
	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      df_ref def = *def_rec;
	      unsigned int dregno = DF_REF_REGNO (def);
	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER))
		{
		  old_unused_notes
		    = df_create_unused_note (insn, old_unused_notes, 
					     def, live, artificial_uses);
		  bitmap_set_bit (do_not_gen, dregno);
		}

	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL | DF_REF_CONDITIONAL))
		bitmap_clear_bit (live, dregno);
	    }
	}
      else
	{
	  /* Regular insn.  */
	  mws_rec = DF_INSN_UID_MWS (uid);
	  while (*mws_rec)
	    {
	      struct df_mw_hardreg *mws = *mws_rec; 
	      if (DF_MWS_REG_DEF_P (mws))
		old_unused_notes
		  = df_set_unused_notes_for_mw (insn, old_unused_notes, 
						mws, live, do_not_gen, 
						artificial_uses);
	      mws_rec++;
	    }

	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      df_ref def = *def_rec;
	      unsigned int dregno = DF_REF_REGNO (def);
	      old_unused_notes
		= df_create_unused_note (insn, old_unused_notes, 
					 def, live, artificial_uses);

	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MUST_CLOBBER | DF_REF_MAY_CLOBBER))
		bitmap_set_bit (do_not_gen, dregno);

	      if (!DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL | DF_REF_CONDITIONAL))
		bitmap_clear_bit (live, dregno);
	    }
	}
      
      /* Process the uses.  */
      mws_rec = DF_INSN_UID_MWS (uid);
      while (*mws_rec)
	{
	  struct df_mw_hardreg *mws = *mws_rec; 
	  if ((DF_MWS_REG_DEF_P (mws))  
	      && !df_ignore_stack_reg (mws->start_regno))
	    old_dead_notes
	      = df_set_dead_notes_for_mw (insn, old_dead_notes, 
					  mws, live, do_not_gen,
					  artificial_uses);
	  mws_rec++;
	}

      for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  unsigned int uregno = DF_REF_REGNO (use);

#ifdef REG_DEAD_DEBUGGING
	  if (dump_file)
	    {
	      fprintf (dump_file, "  regular looking at use ");
	      df_ref_debug (use, dump_file);
	    }
#endif
	  if (!bitmap_bit_p (live, uregno))
	    {
	      if ( (!(DF_REF_FLAGS (use) & DF_REF_MW_HARDREG))
		   && (!bitmap_bit_p (do_not_gen, uregno))
		   && (!bitmap_bit_p (artificial_uses, uregno))
		   && (!(DF_REF_FLAGS (use) & DF_REF_READ_WRITE))
		   && (!df_ignore_stack_reg (uregno)))
		{
		  rtx reg = (DF_REF_LOC (use)) 
                            ? *DF_REF_REAL_LOC (use) : DF_REF_REG (use);
		  old_dead_notes = df_set_note (REG_DEAD, insn, old_dead_notes, reg);

#ifdef REG_DEAD_DEBUGGING
		  df_print_note ("adding 4: ", insn, REG_NOTES (insn));
#endif
		}
	      /* This register is now live.  */
	      bitmap_set_bit (live, uregno);
	    }
	}

      while (old_unused_notes)
	{
	  rtx next = XEXP (old_unused_notes, 1);
	  free_EXPR_LIST_node (old_unused_notes);
	  old_unused_notes = next;
	}
      while (old_dead_notes)
	{
	  rtx next = XEXP (old_dead_notes, 1);
	  free_EXPR_LIST_node (old_dead_notes);
	  old_dead_notes = next;
	}
    }
}


/* Compute register info: lifetime, bb, and number of defs and uses.  */
static void
df_note_compute (bitmap all_blocks)
{
  unsigned int bb_index;
  bitmap_iterator bi;
  bitmap live = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap do_not_gen = BITMAP_ALLOC (&df_bitmap_obstack);
  bitmap artificial_uses = BITMAP_ALLOC (&df_bitmap_obstack);

#ifdef REG_DEAD_DEBUGGING
  if (dump_file)
    print_rtl_with_bb (dump_file, get_insns());
#endif

  EXECUTE_IF_SET_IN_BITMAP (all_blocks, 0, bb_index, bi)
  {
    df_note_bb_compute (bb_index, live, do_not_gen, artificial_uses);
  }

  BITMAP_FREE (live);
  BITMAP_FREE (do_not_gen);
  BITMAP_FREE (artificial_uses);
}


/* Free all storage associated with the problem.  */

static void
df_note_free (void)
{
  free (df_note);
}


/* All of the information associated every instance of the problem.  */

static struct df_problem problem_NOTE =
{
  DF_NOTE,                    /* Problem id.  */
  DF_NONE,                    /* Direction.  */
  df_note_alloc,              /* Allocate the problem specific data.  */
  NULL,                       /* Reset global information.  */
  NULL,                       /* Free basic block info.  */
  df_note_compute,            /* Local compute function.  */
  NULL,                       /* Init the solution specific data.  */
  NULL,                       /* Iterative solver.  */
  NULL,                       /* Confluence operator 0.  */ 
  NULL,                       /* Confluence operator n.  */ 
  NULL,                       /* Transfer function.  */
  NULL,                       /* Finalize function.  */
  df_note_free,               /* Free all of the problem information.  */
  df_note_free,               /* Remove this problem from the stack of dataflow problems.  */
  NULL,                       /* Debugging.  */
  NULL,                       /* Debugging start block.  */
  NULL,                       /* Debugging end block.  */
  NULL,                       /* Incremental solution verify start.  */
  NULL,                       /* Incremental solution verify end.  */
  &problem_LR,                /* Dependent problem.  */
  TV_DF_NOTE,                 /* Timing variable.  */
  false                       /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_note_add_problem (void)
{
  df_add_problem (&problem_NOTE);
}




/*----------------------------------------------------------------------------
   Functions for simulating the effects of single insns.  

   You can either simulate in the forwards direction, starting from
   the top of a block or the backwards direction from the end of the
   block.  The main difference is that if you go forwards, the uses
   are examined first then the defs, and if you go backwards, the defs
   are examined first then the uses.

   If you start at the top of the block, use one of DF_LIVE_IN or
   DF_LR_IN.  If you start at the bottom of the block use one of
   DF_LIVE_OUT or DF_LR_OUT.  BE SURE TO PASS A COPY OF THESE SETS,
   THEY WILL BE DESTROYED.
----------------------------------------------------------------------------*/


/* Find the set of DEFs for INSN.  */

void
df_simulate_find_defs (rtx insn, bitmap defs)
{
  df_ref *def_rec;
  unsigned int uid = INSN_UID (insn);

  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      /* If the def is to only part of the reg, it does
	 not kill the other defs that reach here.  */
      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
	bitmap_set_bit (defs, DF_REF_REGNO (def));
    }
}


/* Simulate the effects of the defs of INSN on LIVE.  */

void
df_simulate_defs (rtx insn, bitmap live)
{
  df_ref *def_rec;
  unsigned int uid = INSN_UID (insn);

  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      unsigned int dregno = DF_REF_REGNO (def);

      /* If the def is to only part of the reg, it does
	 not kill the other defs that reach here.  */
      if (!(DF_REF_FLAGS (def) & (DF_REF_PARTIAL | DF_REF_CONDITIONAL)))
	bitmap_clear_bit (live, dregno);
    }
}  


/* Simulate the effects of the uses of INSN on LIVE.  */

void 
df_simulate_uses (rtx insn, bitmap live)
{
  df_ref *use_rec;
  unsigned int uid = INSN_UID (insn);

  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      /* Add use to set of uses in this BB.  */
      bitmap_set_bit (live, DF_REF_REGNO (use));
    }
}


/* Add back the always live regs in BB to LIVE.  */

static inline void
df_simulate_fixup_sets (basic_block bb, bitmap live)
{
  /* These regs are considered always live so if they end up dying
     because of some def, we need to bring the back again.  */
  if (bb_has_eh_pred (bb))
    bitmap_ior_into (live, df->eh_block_artificial_uses);
  else
    bitmap_ior_into (live, df->regular_block_artificial_uses);
}


/*----------------------------------------------------------------------------
   The following three functions are used only for BACKWARDS scanning:
   i.e. they process the defs before the uses.

   df_simulate_initialize_backwards should be called first with a
   bitvector copyied from the DF_LIVE_OUT or DF_LR_OUT.  Then
   df_simulate_one_insn_backwards should be called for each insn in
   the block, starting with the last on.  Finally,
   df_simulate_finalize_backwards can be called to get a new value
   of the sets at the top of the block (this is rarely used).
   ----------------------------------------------------------------------------*/

/* Apply the artificial uses and defs at the end of BB in a backwards
   direction.  */

void 
df_simulate_initialize_backwards (basic_block bb, bitmap live)
{
  df_ref *def_rec;
  df_ref *use_rec;
  int bb_index = bb->index;
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }

  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == 0)
	bitmap_set_bit (live, DF_REF_REGNO (use));
    }
}


/* Simulate the backwards effects of INSN on the bitmap LIVE.  */

void 
df_simulate_one_insn_backwards (basic_block bb, rtx insn, bitmap live)
{
  if (! INSN_P (insn))
    return;	
  
  df_simulate_defs (insn, live);
  df_simulate_uses (insn, live);
  df_simulate_fixup_sets (bb, live);
}


/* Apply the artificial uses and defs at the top of BB in a backwards
   direction.  */

void 
df_simulate_finalize_backwards (basic_block bb, bitmap live)
{
  df_ref *def_rec;
#ifdef EH_USES
  df_ref *use_rec;
#endif
  int bb_index = bb->index;
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }

#ifdef EH_USES
  for (use_rec = df_get_artificial_uses (bb_index); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if (DF_REF_FLAGS (use) & DF_REF_AT_TOP)
	bitmap_set_bit (live, DF_REF_REGNO (use));
    }
#endif
}
/*----------------------------------------------------------------------------
   The following three functions are used only for FORWARDS scanning:
   i.e. they process the defs and the REG_DEAD and REG_UNUSED notes.
   Thus it is important to add the DF_NOTES problem to the stack of 
   problems computed before using these functions.

   df_simulate_initialize_forwards should be called first with a
   bitvector copyied from the DF_LIVE_IN or DF_LR_IN.  Then
   df_simulate_one_insn_forwards should be called for each insn in
   the block, starting with the last on.  Finally,
   df_simulate_finalize_forwards can be called to get a new value
   of the sets at the bottom of the block (this is rarely used).
   ----------------------------------------------------------------------------*/

/* Apply the artificial uses and defs at the top of BB in a backwards
   direction.  */

void 
df_simulate_initialize_forwards (basic_block bb, bitmap live)
{
  df_ref *def_rec;
  int bb_index = bb->index;
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }
}

/* Simulate the backwards effects of INSN on the bitmap LIVE.  */

void 
df_simulate_one_insn_forwards (basic_block bb, rtx insn, bitmap live)
{
  rtx link;
  if (! INSN_P (insn))
    return;	

  /* Make sure that the DF_NOTES really is an active df problem.  */ 
  gcc_assert (df_note);

  df_simulate_defs (insn, live);

  /* Clear all of the registers that go dead.  */
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    {
      switch (REG_NOTE_KIND (link))
	case REG_DEAD:
	case REG_UNUSED:
	{
	  rtx reg = XEXP (link, 0);
	  int regno = REGNO (reg);
	  if (regno < FIRST_PSEUDO_REGISTER)
	    {
	      int n = hard_regno_nregs[regno][GET_MODE (reg)];
	      while (--n >= 0)
		bitmap_clear_bit (live, regno + n);
	    }
	  else 
	    bitmap_clear_bit (live, regno);
	  break;
	default:
	  break;
	}
    }
  df_simulate_fixup_sets (bb, live);
}


/* Apply the artificial uses and defs at the end of BB in a backwards
   direction.  */

void 
df_simulate_finalize_forwards (basic_block bb, bitmap live)
{
  df_ref *def_rec;
  int bb_index = bb->index;
  
  for (def_rec = df_get_artificial_defs (bb_index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) == 0)
	bitmap_clear_bit (live, DF_REF_REGNO (def));
    }
}


