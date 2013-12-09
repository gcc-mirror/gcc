/* Scanning of rtl for dataflow analysis.
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
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
#include "alloc-pool.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "dumpfile.h"
#include "tree.h"
#include "target.h"
#include "target-def.h"
#include "df.h"
#include "emit-rtl.h"  /* FIXME: Can go away once crtl is moved to rtl.h.  */


typedef struct df_mw_hardreg *df_mw_hardreg_ptr;


#ifndef HAVE_epilogue
#define HAVE_epilogue 0
#endif
#ifndef HAVE_prologue
#define HAVE_prologue 0
#endif
#ifndef HAVE_sibcall_epilogue
#define HAVE_sibcall_epilogue 0
#endif

#ifndef EPILOGUE_USES
#define EPILOGUE_USES(REGNO)  0
#endif

/* The following two macros free the vecs that hold either the refs or
   the mw refs.  They are a little tricky because the vec has 0
   elements is special and is not to be freed.  */
#define df_scan_free_ref_vec(V) \
  do { \
    if (V && *V) \
      free (V);  \
  } while (0)

#define df_scan_free_mws_vec(V) \
  do { \
    if (V && *V) \
      free (V);  \
  } while (0)

/* The set of hard registers in eliminables[i].from. */

static HARD_REG_SET elim_reg_set;

/* Initialize ur_in and ur_out as if all hard registers were partially
   available.  */

struct df_collection_rec
{
  stack_vec<df_ref, 128> def_vec;
  stack_vec<df_ref, 32> use_vec;
  stack_vec<df_ref, 32> eq_use_vec;
  stack_vec<df_mw_hardreg_ptr, 32> mw_vec;
};

static df_ref df_null_ref_rec[1];
static struct df_mw_hardreg * df_null_mw_rec[1];

static void df_ref_record (enum df_ref_class, struct df_collection_rec *,
			   rtx, rtx *,
			   basic_block, struct df_insn_info *,
			   enum df_ref_type, int ref_flags);
static void df_def_record_1 (struct df_collection_rec *, rtx *,
			     basic_block, struct df_insn_info *,
			     int ref_flags);
static void df_defs_record (struct df_collection_rec *, rtx,
			    basic_block, struct df_insn_info *,
			    int ref_flags);
static void df_uses_record (struct df_collection_rec *,
			    rtx *, enum df_ref_type,
			    basic_block, struct df_insn_info *,
			    int ref_flags);

static void df_install_ref_incremental (df_ref);
static df_ref df_ref_create_structure (enum df_ref_class,
				       struct df_collection_rec *, rtx, rtx *,
				       basic_block, struct df_insn_info *,
				       enum df_ref_type, int ref_flags);
static void df_insn_refs_collect (struct df_collection_rec*,
				  basic_block, struct df_insn_info *);
static void df_canonize_collection_rec (struct df_collection_rec *);

static void df_get_regular_block_artificial_uses (bitmap);
static void df_get_eh_block_artificial_uses (bitmap);

static void df_record_entry_block_defs (bitmap);
static void df_record_exit_block_uses (bitmap);
static void df_get_exit_block_use_set (bitmap);
static void df_get_entry_block_def_set (bitmap);
static void df_grow_ref_info (struct df_ref_info *, unsigned int);
static void df_ref_chain_delete_du_chain (df_ref *);
static void df_ref_chain_delete (df_ref *);

static void df_refs_add_to_chains (struct df_collection_rec *,
				   basic_block, rtx, unsigned int);

static bool df_insn_refs_verify (struct df_collection_rec *, basic_block, rtx, bool);
static void df_entry_block_defs_collect (struct df_collection_rec *, bitmap);
static void df_exit_block_uses_collect (struct df_collection_rec *, bitmap);
static void df_install_ref (df_ref, struct df_reg_info *,
			    struct df_ref_info *, bool);

static int df_ref_compare (const void *, const void *);
static int df_mw_compare (const void *, const void *);

static void df_insn_info_delete (unsigned int);

/* Indexed by hardware reg number, is true if that register is ever
   used in the current function.

   In df-scan.c, this is set up to record the hard regs used
   explicitly.  Reload adds in the hard regs used for holding pseudo
   regs.  Final uses it to generate the code in the function prologue
   and epilogue to save and restore registers as needed.  */

static bool regs_ever_live[FIRST_PSEUDO_REGISTER];

/* Flags used to tell df_refs_add_to_chains() which vectors it should copy. */
static const unsigned int copy_defs = 0x1;
static const unsigned int copy_uses = 0x2;
static const unsigned int copy_eq_uses = 0x4;
static const unsigned int copy_mw = 0x8;
static const unsigned int copy_all = copy_defs | copy_uses | copy_eq_uses
| copy_mw;

/*----------------------------------------------------------------------------
   SCANNING DATAFLOW PROBLEM

   There are several ways in which scanning looks just like the other
   dataflow problems.  It shares the all the mechanisms for local info
   as well as basic block info.  Where it differs is when and how often
   it gets run.  It also has no need for the iterative solver.
----------------------------------------------------------------------------*/

/* Problem data for the scanning dataflow function.  */
struct df_scan_problem_data
{
  alloc_pool ref_base_pool;
  alloc_pool ref_artificial_pool;
  alloc_pool ref_regular_pool;
  alloc_pool insn_pool;
  alloc_pool reg_pool;
  alloc_pool mw_reg_pool;
  bitmap_obstack reg_bitmaps;
  bitmap_obstack insn_bitmaps;
};

typedef struct df_scan_bb_info *df_scan_bb_info_t;


/* Internal function to shut down the scanning problem.  */
static void
df_scan_free_internal (void)
{
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;
  unsigned int i;
  basic_block bb;

  /* The vectors that hold the refs are not pool allocated because
     they come in many sizes.  This makes them impossible to delete
     all at once.  */
  for (i = 0; i < DF_INSN_SIZE (); i++)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_GET (i);
      /* Skip the insns that have no insn_info or have been
	 deleted.  */
      if (insn_info)
	{
	  df_scan_free_ref_vec (insn_info->defs);
	  df_scan_free_ref_vec (insn_info->uses);
	  df_scan_free_ref_vec (insn_info->eq_uses);
	  df_scan_free_mws_vec (insn_info->mw_hardregs);
	}
    }

  FOR_ALL_BB_FN (bb, cfun)
    {
      unsigned int bb_index = bb->index;
      struct df_scan_bb_info *bb_info = df_scan_get_bb_info (bb_index);
      if (bb_info)
	{
	  df_scan_free_ref_vec (bb_info->artificial_defs);
	  df_scan_free_ref_vec (bb_info->artificial_uses);
	}
    }

  free (df->def_info.refs);
  free (df->def_info.begin);
  free (df->def_info.count);
  memset (&df->def_info, 0, (sizeof (struct df_ref_info)));

  free (df->use_info.refs);
  free (df->use_info.begin);
  free (df->use_info.count);
  memset (&df->use_info, 0, (sizeof (struct df_ref_info)));

  free (df->def_regs);
  df->def_regs = NULL;
  free (df->use_regs);
  df->use_regs = NULL;
  free (df->eq_use_regs);
  df->eq_use_regs = NULL;
  df->regs_size = 0;
  DF_REG_SIZE (df) = 0;

  free (df->insns);
  df->insns = NULL;
  DF_INSN_SIZE () = 0;

  free (df_scan->block_info);
  df_scan->block_info = NULL;
  df_scan->block_info_size = 0;

  bitmap_clear (&df->hardware_regs_used);
  bitmap_clear (&df->regular_block_artificial_uses);
  bitmap_clear (&df->eh_block_artificial_uses);
  BITMAP_FREE (df->entry_block_defs);
  BITMAP_FREE (df->exit_block_uses);
  bitmap_clear (&df->insns_to_delete);
  bitmap_clear (&df->insns_to_rescan);
  bitmap_clear (&df->insns_to_notes_rescan);

  free_alloc_pool (problem_data->ref_base_pool);
  free_alloc_pool (problem_data->ref_artificial_pool);
  free_alloc_pool (problem_data->ref_regular_pool);
  free_alloc_pool (problem_data->insn_pool);
  free_alloc_pool (problem_data->reg_pool);
  free_alloc_pool (problem_data->mw_reg_pool);
  bitmap_obstack_release (&problem_data->reg_bitmaps);
  bitmap_obstack_release (&problem_data->insn_bitmaps);
  free (df_scan->problem_data);
}


/* Free basic block info.  */

static void
df_scan_free_bb_info (basic_block bb, void *vbb_info)
{
  struct df_scan_bb_info *bb_info = (struct df_scan_bb_info *) vbb_info;
  unsigned int bb_index = bb->index;

  /* See if bb_info is initialized.  */
  if (bb_info->artificial_defs)
    {
      rtx insn;
      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    df_insn_info_delete (INSN_UID (insn));
	}

      if (bb_index < df_scan->block_info_size)
	bb_info = df_scan_get_bb_info (bb_index);

      /* Get rid of any artificial uses or defs.  */
      if (bb_info->artificial_defs)
	{
	  df_ref_chain_delete_du_chain (bb_info->artificial_defs);
	  df_ref_chain_delete_du_chain (bb_info->artificial_uses);
	  df_ref_chain_delete (bb_info->artificial_defs);
	  df_ref_chain_delete (bb_info->artificial_uses);
	  bb_info->artificial_defs = NULL;
	  bb_info->artificial_uses = NULL;
	}
    }
}


/* Allocate the problem data for the scanning problem.  This should be
   called when the problem is created or when the entire function is to
   be rescanned.  */
void
df_scan_alloc (bitmap all_blocks ATTRIBUTE_UNUSED)
{
  struct df_scan_problem_data *problem_data;
  unsigned int insn_num = get_max_uid () + 1;
  unsigned int block_size = 512;
  basic_block bb;

  /* Given the number of pools, this is really faster than tearing
     everything apart.  */
  if (df_scan->problem_data)
    df_scan_free_internal ();

  problem_data = XNEW (struct df_scan_problem_data);
  df_scan->problem_data = problem_data;
  df_scan->computed = true;

  problem_data->ref_base_pool
    = create_alloc_pool ("df_scan ref base",
			 sizeof (struct df_base_ref), block_size);
  problem_data->ref_artificial_pool
    = create_alloc_pool ("df_scan ref artificial",
			 sizeof (struct df_artificial_ref), block_size);
  problem_data->ref_regular_pool
    = create_alloc_pool ("df_scan ref regular",
			 sizeof (struct df_regular_ref), block_size);
  problem_data->insn_pool
    = create_alloc_pool ("df_scan insn",
			 sizeof (struct df_insn_info), block_size);
  problem_data->reg_pool
    = create_alloc_pool ("df_scan reg",
			 sizeof (struct df_reg_info), block_size);
  problem_data->mw_reg_pool
    = create_alloc_pool ("df_scan mw_reg",
			 sizeof (struct df_mw_hardreg), block_size / 16);

  bitmap_obstack_initialize (&problem_data->reg_bitmaps);
  bitmap_obstack_initialize (&problem_data->insn_bitmaps);

  insn_num += insn_num / 4;
  df_grow_reg_info ();

  df_grow_insn_info ();
  df_grow_bb_info (df_scan);

  FOR_ALL_BB_FN (bb, cfun)
    {
      unsigned int bb_index = bb->index;
      struct df_scan_bb_info *bb_info = df_scan_get_bb_info (bb_index);
      bb_info->artificial_defs = NULL;
      bb_info->artificial_uses = NULL;
    }

  bitmap_initialize (&df->hardware_regs_used, &problem_data->reg_bitmaps);
  bitmap_initialize (&df->regular_block_artificial_uses, &problem_data->reg_bitmaps);
  bitmap_initialize (&df->eh_block_artificial_uses, &problem_data->reg_bitmaps);
  df->entry_block_defs = BITMAP_ALLOC (&problem_data->reg_bitmaps);
  df->exit_block_uses = BITMAP_ALLOC (&problem_data->reg_bitmaps);
  bitmap_initialize (&df->insns_to_delete, &problem_data->insn_bitmaps);
  bitmap_initialize (&df->insns_to_rescan, &problem_data->insn_bitmaps);
  bitmap_initialize (&df->insns_to_notes_rescan, &problem_data->insn_bitmaps);
  df_scan->optional_p = false;
}


/* Free all of the data associated with the scan problem.  */

static void
df_scan_free (void)
{
  if (df_scan->problem_data)
    df_scan_free_internal ();

  if (df->blocks_to_analyze)
    {
      BITMAP_FREE (df->blocks_to_analyze);
      df->blocks_to_analyze = NULL;
    }

  free (df_scan);
}

/* Dump the preamble for DF_SCAN dump. */
static void
df_scan_start_dump (FILE *file ATTRIBUTE_UNUSED)
{
  int i;
  int dcount = 0;
  int ucount = 0;
  int ecount = 0;
  int icount = 0;
  int ccount = 0;
  basic_block bb;
  rtx insn;

  fprintf (file, ";;  invalidated by call \t");
  df_print_regset (file, regs_invalidated_by_call_regset);
  fprintf (file, ";;  hardware regs used \t");
  df_print_regset (file, &df->hardware_regs_used);
  fprintf (file, ";;  regular block artificial uses \t");
  df_print_regset (file, &df->regular_block_artificial_uses);
  fprintf (file, ";;  eh block artificial uses \t");
  df_print_regset (file, &df->eh_block_artificial_uses);
  fprintf (file, ";;  entry block defs \t");
  df_print_regset (file, df->entry_block_defs);
  fprintf (file, ";;  exit block uses \t");
  df_print_regset (file, df->exit_block_uses);
  fprintf (file, ";;  regs ever live \t");
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (df_regs_ever_live_p (i))
      fprintf (file, " %d[%s]", i, reg_names[i]);
  fprintf (file, "\n;;  ref usage \t");

  for (i = 0; i < (int)df->regs_inited; i++)
    if (DF_REG_DEF_COUNT (i) || DF_REG_USE_COUNT (i) || DF_REG_EQ_USE_COUNT (i))
      {
	const char * sep = "";

	fprintf (file, "r%d={", i);
	if (DF_REG_DEF_COUNT (i))
	  {
	    fprintf (file, "%dd", DF_REG_DEF_COUNT (i));
	    sep = ",";
	    dcount += DF_REG_DEF_COUNT (i);
	  }
	if (DF_REG_USE_COUNT (i))
	  {
	    fprintf (file, "%s%du", sep, DF_REG_USE_COUNT (i));
	    sep = ",";
	    ucount += DF_REG_USE_COUNT (i);
	  }
	if (DF_REG_EQ_USE_COUNT (i))
	  {
	    fprintf (file, "%s%de", sep, DF_REG_EQ_USE_COUNT (i));
	    ecount += DF_REG_EQ_USE_COUNT (i);
	  }
	fprintf (file, "} ");
      }

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      if (INSN_P (insn))
	{
	  if (CALL_P (insn))
	    ccount++;
	  else
	    icount++;
	}

  fprintf (file, "\n;;    total ref usage %d{%dd,%du,%de}"
		 " in %d{%d regular + %d call} insns.\n",
		 dcount + ucount + ecount, dcount, ucount, ecount,
		 icount + ccount, icount, ccount);
}

/* Dump the bb_info for a given basic block. */
static void
df_scan_start_block (basic_block bb, FILE *file)
{
  struct df_scan_bb_info *bb_info
    = df_scan_get_bb_info (bb->index);

  if (bb_info)
    {
      fprintf (file, ";; bb %d artificial_defs: ", bb->index);
      df_refs_chain_dump (bb_info->artificial_defs, true, file);
      fprintf (file, "\n;; bb %d artificial_uses: ", bb->index);
      df_refs_chain_dump (bb_info->artificial_uses, true, file);
      fprintf (file, "\n");
    }
#if 0
  {
    rtx insn;
    FOR_BB_INSNS (bb, insn)
      if (INSN_P (insn))
	df_insn_debug (insn, false, file);
  }
#endif
}

static struct df_problem problem_SCAN =
{
  DF_SCAN,                    /* Problem id.  */
  DF_NONE,                    /* Direction.  */
  df_scan_alloc,              /* Allocate the problem specific data.  */
  NULL,                       /* Reset global information.  */
  df_scan_free_bb_info,       /* Free basic block info.  */
  NULL,                       /* Local compute function.  */
  NULL,                       /* Init the solution specific data.  */
  NULL,                       /* Iterative solver.  */
  NULL,                       /* Confluence operator 0.  */
  NULL,                       /* Confluence operator n.  */
  NULL,                       /* Transfer function.  */
  NULL,                       /* Finalize function.  */
  df_scan_free,               /* Free all of the problem information.  */
  NULL,                       /* Remove this problem from the stack of dataflow problems.  */
  df_scan_start_dump,         /* Debugging.  */
  df_scan_start_block,        /* Debugging start block.  */
  NULL,                       /* Debugging end block.  */
  NULL,                       /* Debugging start insn.  */
  NULL,                       /* Debugging end insn.  */
  NULL,                       /* Incremental solution verify start.  */
  NULL,                       /* Incremental solution verify end.  */
  NULL,                       /* Dependent problem.  */
  sizeof (struct df_scan_bb_info),/* Size of entry of block_info array.  */
  TV_DF_SCAN,                 /* Timing variable.  */
  false                       /* Reset blocks on dropping out of blocks_to_analyze.  */
};


/* Create a new DATAFLOW instance and add it to an existing instance
   of DF.  The returned structure is what is used to get at the
   solution.  */

void
df_scan_add_problem (void)
{
  df_add_problem (&problem_SCAN);
}


/*----------------------------------------------------------------------------
   Storage Allocation Utilities
----------------------------------------------------------------------------*/


/* First, grow the reg_info information.  If the current size is less than
   the number of pseudos, grow to 25% more than the number of
   pseudos.

   Second, assure that all of the slots up to max_reg_num have been
   filled with reg_info structures.  */

void
df_grow_reg_info (void)
{
  unsigned int max_reg = max_reg_num ();
  unsigned int new_size = max_reg;
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;
  unsigned int i;

  if (df->regs_size < new_size)
    {
      new_size += new_size / 4;
      df->def_regs = XRESIZEVEC (struct df_reg_info *, df->def_regs, new_size);
      df->use_regs = XRESIZEVEC (struct df_reg_info *, df->use_regs, new_size);
      df->eq_use_regs = XRESIZEVEC (struct df_reg_info *, df->eq_use_regs,
				    new_size);
      df->def_info.begin = XRESIZEVEC (unsigned, df->def_info.begin, new_size);
      df->def_info.count = XRESIZEVEC (unsigned, df->def_info.count, new_size);
      df->use_info.begin = XRESIZEVEC (unsigned, df->use_info.begin, new_size);
      df->use_info.count = XRESIZEVEC (unsigned, df->use_info.count, new_size);
      df->regs_size = new_size;
    }

  for (i = df->regs_inited; i < max_reg; i++)
    {
      struct df_reg_info *reg_info;

      reg_info = (struct df_reg_info *) pool_alloc (problem_data->reg_pool);
      memset (reg_info, 0, sizeof (struct df_reg_info));
      df->def_regs[i] = reg_info;
      reg_info = (struct df_reg_info *) pool_alloc (problem_data->reg_pool);
      memset (reg_info, 0, sizeof (struct df_reg_info));
      df->use_regs[i] = reg_info;
      reg_info = (struct df_reg_info *) pool_alloc (problem_data->reg_pool);
      memset (reg_info, 0, sizeof (struct df_reg_info));
      df->eq_use_regs[i] = reg_info;
      df->def_info.begin[i] = 0;
      df->def_info.count[i] = 0;
      df->use_info.begin[i] = 0;
      df->use_info.count[i] = 0;
    }

  df->regs_inited = max_reg;
}


/* Grow the ref information.  */

static void
df_grow_ref_info (struct df_ref_info *ref_info, unsigned int new_size)
{
  if (ref_info->refs_size < new_size)
    {
      ref_info->refs = XRESIZEVEC (df_ref, ref_info->refs, new_size);
      memset (ref_info->refs + ref_info->refs_size, 0,
	      (new_size - ref_info->refs_size) *sizeof (df_ref));
      ref_info->refs_size = new_size;
    }
}


/* Check and grow the ref information if necessary.  This routine
   guarantees total_size + BITMAP_ADDEND amount of entries in refs
   array.  It updates ref_info->refs_size only and does not change
   ref_info->total_size.  */

static void
df_check_and_grow_ref_info (struct df_ref_info *ref_info,
			    unsigned bitmap_addend)
{
  if (ref_info->refs_size < ref_info->total_size + bitmap_addend)
    {
      int new_size = ref_info->total_size + bitmap_addend;
      new_size += ref_info->total_size / 4;
      df_grow_ref_info (ref_info, new_size);
    }
}


/* Grow the ref information.  If the current size is less than the
   number of instructions, grow to 25% more than the number of
   instructions.  */

void
df_grow_insn_info (void)
{
  unsigned int new_size = get_max_uid () + 1;
  if (DF_INSN_SIZE () < new_size)
    {
      new_size += new_size / 4;
      df->insns = XRESIZEVEC (struct df_insn_info *, df->insns, new_size);
      memset (df->insns + df->insns_size, 0,
	      (new_size - DF_INSN_SIZE ()) *sizeof (struct df_insn_info *));
      DF_INSN_SIZE () = new_size;
    }
}




/*----------------------------------------------------------------------------
   PUBLIC INTERFACES FOR SMALL GRAIN CHANGES TO SCANNING.
----------------------------------------------------------------------------*/

/* Rescan all of the block_to_analyze or all of the blocks in the
   function if df_set_blocks if blocks_to_analyze is NULL;  */

void
df_scan_blocks (void)
{
  basic_block bb;

  df->def_info.ref_order = DF_REF_ORDER_NO_TABLE;
  df->use_info.ref_order = DF_REF_ORDER_NO_TABLE;

  df_get_regular_block_artificial_uses (&df->regular_block_artificial_uses);
  df_get_eh_block_artificial_uses (&df->eh_block_artificial_uses);

  bitmap_ior_into (&df->eh_block_artificial_uses,
		   &df->regular_block_artificial_uses);

  /* ENTRY and EXIT blocks have special defs/uses.  */
  df_get_entry_block_def_set (df->entry_block_defs);
  df_record_entry_block_defs (df->entry_block_defs);
  df_get_exit_block_use_set (df->exit_block_uses);
  df_record_exit_block_uses (df->exit_block_uses);
  df_set_bb_dirty (BASIC_BLOCK_FOR_FN (cfun, ENTRY_BLOCK));
  df_set_bb_dirty (BASIC_BLOCK_FOR_FN (cfun, EXIT_BLOCK));

  /* Regular blocks */
  FOR_EACH_BB_FN (bb, cfun)
    {
      unsigned int bb_index = bb->index;
      df_bb_refs_record (bb_index, true);
    }
}

/* Create new refs under address LOC within INSN.  This function is
   only used externally.  REF_FLAGS must be either 0 or DF_REF_IN_NOTE,
   depending on whether LOC is inside PATTERN (INSN) or a note.  */

void
df_uses_create (rtx *loc, rtx insn, int ref_flags)
{
  gcc_assert (!(ref_flags & ~DF_REF_IN_NOTE));
  df_uses_record (NULL, loc, DF_REF_REG_USE,
                  BLOCK_FOR_INSN (insn),
                  DF_INSN_INFO_GET (insn),
                  ref_flags);
}

/* Create a new ref of type DF_REF_TYPE for register REG at address
   LOC within INSN of BB.  This function is only used externally.  */

df_ref
df_ref_create (rtx reg, rtx *loc, rtx insn,
	       basic_block bb,
	       enum df_ref_type ref_type,
	       int ref_flags)
{
  enum df_ref_class cl;

  df_grow_reg_info ();

  /* You cannot hack artificial refs.  */
  gcc_assert (insn);

  if (loc)
    cl = DF_REF_REGULAR;
  else
    cl = DF_REF_BASE;

  return df_ref_create_structure (cl, NULL, reg, loc, bb,
                                  DF_INSN_INFO_GET (insn),
                                  ref_type, ref_flags);
}

static void
df_install_ref_incremental (df_ref ref)
{
  struct df_reg_info **reg_info;
  struct df_ref_info *ref_info;
  df_ref *ref_rec;
  df_ref **ref_rec_ptr;
  unsigned int count = 0;
  bool add_to_table;

  rtx insn = DF_REF_INSN (ref);
  basic_block bb = BLOCK_FOR_INSN (insn);

  if (DF_REF_REG_DEF_P (ref))
    {
      reg_info = df->def_regs;
      ref_info = &df->def_info;
      ref_rec_ptr = &DF_INSN_DEFS (insn);
      add_to_table = ref_info->ref_order != DF_REF_ORDER_NO_TABLE;
    }
  else if (DF_REF_FLAGS (ref) & DF_REF_IN_NOTE)
    {
      reg_info = df->eq_use_regs;
      ref_info = &df->use_info;
      ref_rec_ptr = &DF_INSN_EQ_USES (insn);
      switch (ref_info->ref_order)
	{
	case DF_REF_ORDER_UNORDERED_WITH_NOTES:
	case DF_REF_ORDER_BY_REG_WITH_NOTES:
	case DF_REF_ORDER_BY_INSN_WITH_NOTES:
	  add_to_table = true;
	  break;
	default:
	  add_to_table = false;
	  break;
	}
    }
  else
    {
      reg_info = df->use_regs;
      ref_info = &df->use_info;
      ref_rec_ptr = &DF_INSN_USES (insn);
      add_to_table = ref_info->ref_order != DF_REF_ORDER_NO_TABLE;
    }

  /* Do not add if ref is not in the right blocks.  */
  if (add_to_table && df->analyze_subset)
    add_to_table = bitmap_bit_p (df->blocks_to_analyze, bb->index);

  df_install_ref (ref, reg_info[DF_REF_REGNO (ref)], ref_info, add_to_table);

  if (add_to_table)
    switch (ref_info->ref_order)
      {
      case DF_REF_ORDER_UNORDERED_WITH_NOTES:
      case DF_REF_ORDER_BY_REG_WITH_NOTES:
      case DF_REF_ORDER_BY_INSN_WITH_NOTES:
	ref_info->ref_order = DF_REF_ORDER_UNORDERED_WITH_NOTES;
	break;
      default:
	ref_info->ref_order = DF_REF_ORDER_UNORDERED;
	break;
      }

  ref_rec = *ref_rec_ptr;
  while (*ref_rec)
    {
      count++;
      ref_rec++;
    }

  ref_rec = *ref_rec_ptr;
  if (count)
    {
      ref_rec = XRESIZEVEC (df_ref, ref_rec, count+2);
      *ref_rec_ptr = ref_rec;
      ref_rec[count] = ref;
      ref_rec[count+1] = NULL;
      qsort (ref_rec, count + 1, sizeof (df_ref), df_ref_compare);
    }
  else
    {
      df_ref *ref_rec = XNEWVEC (df_ref, 2);
      ref_rec[0] = ref;
      ref_rec[1] = NULL;
      *ref_rec_ptr = ref_rec;
    }

#if 0
  if (dump_file)
    {
      fprintf (dump_file, "adding ref ");
      df_ref_debug (ref, dump_file);
    }
#endif
  /* By adding the ref directly, df_insn_rescan my not find any
     differences even though the block will have changed.  So we need
     to mark the block dirty ourselves.  */
  if (!DEBUG_INSN_P (DF_REF_INSN (ref)))
    df_set_bb_dirty (bb);
}



/*----------------------------------------------------------------------------
   UTILITIES TO CREATE AND DESTROY REFS AND CHAINS.
----------------------------------------------------------------------------*/

static void
df_free_ref (df_ref ref)
{
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;

  switch (DF_REF_CLASS (ref))
    {
    case DF_REF_BASE:
      pool_free (problem_data->ref_base_pool, ref);
      break;

    case DF_REF_ARTIFICIAL:
      pool_free (problem_data->ref_artificial_pool, ref);
      break;

    case DF_REF_REGULAR:
      pool_free (problem_data->ref_regular_pool, ref);
      break;
    }
}


/* Unlink and delete REF at the reg_use, reg_eq_use or reg_def chain.
   Also delete the def-use or use-def chain if it exists.  */

static void
df_reg_chain_unlink (df_ref ref)
{
  df_ref next = DF_REF_NEXT_REG (ref);
  df_ref prev = DF_REF_PREV_REG (ref);
  int id = DF_REF_ID (ref);
  struct df_reg_info *reg_info;
  df_ref *refs = NULL;

  if (DF_REF_REG_DEF_P (ref))
    {
      int regno = DF_REF_REGNO (ref);
      reg_info = DF_REG_DEF_GET (regno);
      refs = df->def_info.refs;
    }
  else
    {
      if (DF_REF_FLAGS (ref) & DF_REF_IN_NOTE)
	{
	  reg_info = DF_REG_EQ_USE_GET (DF_REF_REGNO (ref));
	  switch (df->use_info.ref_order)
	    {
	    case DF_REF_ORDER_UNORDERED_WITH_NOTES:
	    case DF_REF_ORDER_BY_REG_WITH_NOTES:
	    case DF_REF_ORDER_BY_INSN_WITH_NOTES:
	      refs = df->use_info.refs;
	      break;
	    default:
	      break;
	    }
	}
      else
	{
	  reg_info = DF_REG_USE_GET (DF_REF_REGNO (ref));
	  refs = df->use_info.refs;
	}
    }

  if (refs)
    {
      if (df->analyze_subset)
	{
	  if (bitmap_bit_p (df->blocks_to_analyze, DF_REF_BBNO (ref)))
	    refs[id] = NULL;
	}
      else
	refs[id] = NULL;
    }

  /* Delete any def-use or use-def chains that start here. It is
     possible that there is trash in this field.  This happens for
     insns that have been deleted when rescanning has been deferred
     and the chain problem has also been deleted.  The chain tear down
     code skips deleted insns.  */
  if (df_chain && DF_REF_CHAIN (ref))
    df_chain_unlink (ref);

  reg_info->n_refs--;
  if (DF_REF_FLAGS_IS_SET (ref, DF_HARD_REG_LIVE))
    {
      gcc_assert (DF_REF_REGNO (ref) < FIRST_PSEUDO_REGISTER);
      df->hard_regs_live_count[DF_REF_REGNO (ref)]--;
    }

  /* Unlink from the reg chain.  If there is no prev, this is the
     first of the list.  If not, just join the next and prev.  */
  if (prev)
    DF_REF_NEXT_REG (prev) = next;
  else
    {
      gcc_assert (reg_info->reg_chain == ref);
      reg_info->reg_chain = next;
    }
  if (next)
    DF_REF_PREV_REG (next) = prev;

  df_free_ref (ref);
}


/* Remove REF from VEC.  */

static void
df_ref_compress_rec (df_ref **vec_ptr, df_ref ref)
{
  df_ref *vec = *vec_ptr;

  if (vec[1])
    {
      while (*vec && *vec != ref)
	vec++;

      while (*vec)
	{
	  *vec = *(vec+1);
	  vec++;
	}
    }
  else
    {
      free (vec);
      *vec_ptr = df_null_ref_rec;
    }
}


/* Unlink REF from all def-use/use-def chains, etc.  */

void
df_ref_remove (df_ref ref)
{
#if 0
  if (dump_file)
    {
      fprintf (dump_file, "removing ref ");
      df_ref_debug (ref, dump_file);
    }
#endif

  if (DF_REF_REG_DEF_P (ref))
    {
      if (DF_REF_IS_ARTIFICIAL (ref))
	{
	  struct df_scan_bb_info *bb_info
	    = df_scan_get_bb_info (DF_REF_BBNO (ref));
	  df_ref_compress_rec (&bb_info->artificial_defs, ref);
	}
      else
	{
	  unsigned int uid = DF_REF_INSN_UID (ref);
	  struct df_insn_info *insn_rec = DF_INSN_UID_GET (uid);
	  df_ref_compress_rec (&insn_rec->defs, ref);
	}
    }
  else
    {
      if (DF_REF_IS_ARTIFICIAL (ref))
	{
	  struct df_scan_bb_info *bb_info
	    = df_scan_get_bb_info (DF_REF_BBNO (ref));
	  df_ref_compress_rec (&bb_info->artificial_uses, ref);
	}
      else
	{
	  unsigned int uid = DF_REF_INSN_UID (ref);
	  struct df_insn_info *insn_rec = DF_INSN_UID_GET (uid);

	  if (DF_REF_FLAGS (ref) & DF_REF_IN_NOTE)
	    df_ref_compress_rec (&insn_rec->eq_uses, ref);
	  else
	    df_ref_compress_rec (&insn_rec->uses, ref);
	}
    }

  /* By deleting the ref directly, df_insn_rescan my not find any
     differences even though the block will have changed.  So we need
     to mark the block dirty ourselves.  */
  if (!DEBUG_INSN_P (DF_REF_INSN (ref)))
    df_set_bb_dirty (DF_REF_BB (ref));
  df_reg_chain_unlink (ref);
}


/* Create the insn record for INSN.  If there was one there, zero it
   out.  */

struct df_insn_info *
df_insn_create_insn_record (rtx insn)
{
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;
  struct df_insn_info *insn_rec;

  df_grow_insn_info ();
  insn_rec = DF_INSN_INFO_GET (insn);
  if (!insn_rec)
    {
      insn_rec = (struct df_insn_info *) pool_alloc (problem_data->insn_pool);
      DF_INSN_INFO_SET (insn, insn_rec);
    }
  memset (insn_rec, 0, sizeof (struct df_insn_info));
  insn_rec->insn = insn;
  return insn_rec;
}


/* Delete all du chain (DF_REF_CHAIN()) of all refs in the ref chain.  */

static void
df_ref_chain_delete_du_chain (df_ref *ref_rec)
{
  while (*ref_rec)
    {
      df_ref ref = *ref_rec;
      /* CHAIN is allocated by DF_CHAIN. So make sure to
         pass df_scan instance for the problem.  */
      if (DF_REF_CHAIN (ref))
        df_chain_unlink (ref);
      ref_rec++;
    }
}


/* Delete all refs in the ref chain.  */

static void
df_ref_chain_delete (df_ref *ref_rec)
{
  df_ref *start = ref_rec;
  while (*ref_rec)
    {
      df_reg_chain_unlink (*ref_rec);
      ref_rec++;
    }

  /* If the list is empty, it has a special shared element that is not
     to be deleted.  */
  if (*start)
    free (start);
}


/* Delete the hardreg chain.  */

static void
df_mw_hardreg_chain_delete (struct df_mw_hardreg **hardregs)
{
  struct df_scan_problem_data *problem_data;

  if (!hardregs)
    return;

  problem_data = (struct df_scan_problem_data *) df_scan->problem_data;

  while (*hardregs)
    {
      pool_free (problem_data->mw_reg_pool, *hardregs);
      hardregs++;
    }
}


/* Delete all of the refs information from the insn with UID.
   Internal helper for df_insn_delete, df_insn_rescan, and other
   df-scan routines that don't have to work in deferred mode
   and do not have to mark basic blocks for re-processing.  */

static void
df_insn_info_delete (unsigned int uid)
{
  struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);

  bitmap_clear_bit (&df->insns_to_delete, uid);
  bitmap_clear_bit (&df->insns_to_rescan, uid);
  bitmap_clear_bit (&df->insns_to_notes_rescan, uid);
  if (insn_info)
    {
      struct df_scan_problem_data *problem_data
	= (struct df_scan_problem_data *) df_scan->problem_data;

      /* In general, notes do not have the insn_info fields
	 initialized.  However, combine deletes insns by changing them
	 to notes.  How clever.  So we cannot just check if it is a
	 valid insn before short circuiting this code, we need to see
	 if we actually initialized it.  */
      if (insn_info->defs)
	{
	  df_mw_hardreg_chain_delete (insn_info->mw_hardregs);

	  if (df_chain)
	    {
	      df_ref_chain_delete_du_chain (insn_info->defs);
	      df_ref_chain_delete_du_chain (insn_info->uses);
	      df_ref_chain_delete_du_chain (insn_info->eq_uses);
	    }

	  df_ref_chain_delete (insn_info->defs);
	  df_ref_chain_delete (insn_info->uses);
	  df_ref_chain_delete (insn_info->eq_uses);
	  df_scan_free_mws_vec (insn_info->mw_hardregs);
	}
      pool_free (problem_data->insn_pool, insn_info);
      DF_INSN_UID_SET (uid, NULL);
    }
}

/* Delete all of the refs information from INSN, either right now
   or marked for later in deferred mode.  */

void
df_insn_delete (rtx insn)
{
  unsigned int uid;
  basic_block bb;

  gcc_checking_assert (INSN_P (insn));

  if (!df)
    return;

  uid = INSN_UID (insn);
  bb = BLOCK_FOR_INSN (insn);

  /* ??? bb can be NULL after pass_free_cfg.  At that point, DF should
     not exist anymore (as mentioned in df-core.c: "The only requirement
     [for DF] is that there be a correct control flow graph."  Clearly
     that isn't the case after pass_free_cfg.  But DF is freed much later
     because some back-ends want to use DF info even though the CFG is
     already gone.  It's not clear to me whether that is safe, actually.
     In any case, we expect BB to be non-NULL at least up to register
     allocation, so disallow a non-NULL BB up to there.  Not perfect
     but better than nothing...  */
  gcc_checking_assert (bb != NULL || reload_completed);

  df_grow_bb_info (df_scan);
  df_grow_reg_info ();

  /* The block must be marked as dirty now, rather than later as in
     df_insn_rescan and df_notes_rescan because it may not be there at
     rescanning time and the mark would blow up.
     DEBUG_INSNs do not make a block's data flow solution dirty (at
     worst the LUIDs are no longer contiguous).  */
  if (bb != NULL && NONDEBUG_INSN_P (insn))
    df_set_bb_dirty (bb);

  /* The client has deferred rescanning.  */
  if (df->changeable_flags & DF_DEFER_INSN_RESCAN)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	{
	  bitmap_clear_bit (&df->insns_to_rescan, uid);
	  bitmap_clear_bit (&df->insns_to_notes_rescan, uid);
	  bitmap_set_bit (&df->insns_to_delete, uid);
	}
      if (dump_file)
	fprintf (dump_file, "deferring deletion of insn with uid = %d.\n", uid);
      return;
    }

  if (dump_file)
    fprintf (dump_file, "deleting insn with uid = %d.\n", uid);

  df_insn_info_delete (uid);
}


/* Free all of the refs and the mw_hardregs in COLLECTION_REC.  */

static void
df_free_collection_rec (struct df_collection_rec *collection_rec)
{
  unsigned int ix;
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;
  df_ref ref;
  struct df_mw_hardreg *mw;

  FOR_EACH_VEC_ELT (collection_rec->def_vec, ix, ref)
    df_free_ref (ref);
  FOR_EACH_VEC_ELT (collection_rec->use_vec, ix, ref)
    df_free_ref (ref);
  FOR_EACH_VEC_ELT (collection_rec->eq_use_vec, ix, ref)
    df_free_ref (ref);
  FOR_EACH_VEC_ELT (collection_rec->mw_vec, ix, mw)
    pool_free (problem_data->mw_reg_pool, mw);

  collection_rec->def_vec.release ();
  collection_rec->use_vec.release ();
  collection_rec->eq_use_vec.release ();
  collection_rec->mw_vec.release ();
}

/* Rescan INSN.  Return TRUE if the rescanning produced any changes.  */

bool
df_insn_rescan (rtx insn)
{
  unsigned int uid = INSN_UID (insn);
  struct df_insn_info *insn_info = NULL;
  basic_block bb = BLOCK_FOR_INSN (insn);
  struct df_collection_rec collection_rec;

  if ((!df) || (!INSN_P (insn)))
    return false;

  if (!bb)
    {
      if (dump_file)
	fprintf (dump_file, "no bb for insn with uid = %d.\n", uid);
      return false;
    }

  /* The client has disabled rescanning and plans to do it itself.  */
  if (df->changeable_flags & DF_NO_INSN_RESCAN)
    return false;

  df_grow_bb_info (df_scan);
  df_grow_reg_info ();

  insn_info = DF_INSN_UID_SAFE_GET (uid);

  /* The client has deferred rescanning.  */
  if (df->changeable_flags & DF_DEFER_INSN_RESCAN)
    {
      if (!insn_info)
	{
	  insn_info = df_insn_create_insn_record (insn);
	  insn_info->defs = df_null_ref_rec;
	  insn_info->uses = df_null_ref_rec;
	  insn_info->eq_uses = df_null_ref_rec;
	  insn_info->mw_hardregs = df_null_mw_rec;
	}
      if (dump_file)
	fprintf (dump_file, "deferring rescan insn with uid = %d.\n", uid);

      bitmap_clear_bit (&df->insns_to_delete, uid);
      bitmap_clear_bit (&df->insns_to_notes_rescan, uid);
      bitmap_set_bit (&df->insns_to_rescan, INSN_UID (insn));
      return false;
    }

  bitmap_clear_bit (&df->insns_to_delete, uid);
  bitmap_clear_bit (&df->insns_to_rescan, uid);
  bitmap_clear_bit (&df->insns_to_notes_rescan, uid);
  if (insn_info)
    {
      int luid;
      bool the_same = df_insn_refs_verify (&collection_rec, bb, insn, false);
      /* If there's no change, return false. */
      if (the_same)
	{
	  df_free_collection_rec (&collection_rec);
	  if (dump_file)
	    fprintf (dump_file, "verify found no changes in insn with uid = %d.\n", uid);
	  return false;
	}
      if (dump_file)
	fprintf (dump_file, "rescanning insn with uid = %d.\n", uid);

      /* There's change - we need to delete the existing info.
	 Since the insn isn't moved, we can salvage its LUID.  */
      luid = DF_INSN_LUID (insn);
      df_insn_info_delete (uid);
      df_insn_create_insn_record (insn);
      DF_INSN_LUID (insn) = luid;
    }
  else
    {
      struct df_insn_info *insn_info = df_insn_create_insn_record (insn);
      df_insn_refs_collect (&collection_rec, bb, insn_info);
      if (dump_file)
	fprintf (dump_file, "scanning new insn with uid = %d.\n", uid);
    }

  df_refs_add_to_chains (&collection_rec, bb, insn, copy_all);
  if (!DEBUG_INSN_P (insn))
    df_set_bb_dirty (bb);

  return true;
}

/* Same as df_insn_rescan, but don't mark the basic block as
   dirty.  */

bool
df_insn_rescan_debug_internal (rtx insn)
{
  unsigned int uid = INSN_UID (insn);
  struct df_insn_info *insn_info;

  gcc_assert (DEBUG_INSN_P (insn)
	      && VAR_LOC_UNKNOWN_P (INSN_VAR_LOCATION_LOC (insn)));

  if (!df)
    return false;

  insn_info = DF_INSN_UID_SAFE_GET (INSN_UID (insn));
  if (!insn_info)
    return false;

  if (dump_file)
    fprintf (dump_file, "deleting debug_insn with uid = %d.\n", uid);

  bitmap_clear_bit (&df->insns_to_delete, uid);
  bitmap_clear_bit (&df->insns_to_rescan, uid);
  bitmap_clear_bit (&df->insns_to_notes_rescan, uid);

  if (!insn_info->defs)
    return false;

  if (insn_info->defs == df_null_ref_rec
      && insn_info->uses == df_null_ref_rec
      && insn_info->eq_uses == df_null_ref_rec
      && insn_info->mw_hardregs == df_null_mw_rec)
    return false;

  df_mw_hardreg_chain_delete (insn_info->mw_hardregs);

  if (df_chain)
    {
      df_ref_chain_delete_du_chain (insn_info->defs);
      df_ref_chain_delete_du_chain (insn_info->uses);
      df_ref_chain_delete_du_chain (insn_info->eq_uses);
    }

  df_ref_chain_delete (insn_info->defs);
  df_ref_chain_delete (insn_info->uses);
  df_ref_chain_delete (insn_info->eq_uses);
  df_scan_free_mws_vec (insn_info->mw_hardregs);

  insn_info->defs = df_null_ref_rec;
  insn_info->uses = df_null_ref_rec;
  insn_info->eq_uses = df_null_ref_rec;
  insn_info->mw_hardregs = df_null_mw_rec;

  return true;
}


/* Rescan all of the insns in the function.  Note that the artificial
   uses and defs are not touched.  This function will destroy def-use
   or use-def chains.  */

void
df_insn_rescan_all (void)
{
  bool no_insn_rescan = false;
  bool defer_insn_rescan = false;
  basic_block bb;
  bitmap_iterator bi;
  unsigned int uid;
  bitmap_head tmp;

  bitmap_initialize (&tmp, &df_bitmap_obstack);

  if (df->changeable_flags & DF_NO_INSN_RESCAN)
    {
      df_clear_flags (DF_NO_INSN_RESCAN);
      no_insn_rescan = true;
    }

  if (df->changeable_flags & DF_DEFER_INSN_RESCAN)
    {
      df_clear_flags (DF_DEFER_INSN_RESCAN);
      defer_insn_rescan = true;
    }

  bitmap_copy (&tmp, &df->insns_to_delete);
  EXECUTE_IF_SET_IN_BITMAP (&tmp, 0, uid, bi)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	df_insn_info_delete (uid);
    }

  bitmap_clear (&tmp);
  bitmap_clear (&df->insns_to_delete);
  bitmap_clear (&df->insns_to_rescan);
  bitmap_clear (&df->insns_to_notes_rescan);

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx insn;
      FOR_BB_INSNS (bb, insn)
	{
	  df_insn_rescan (insn);
	}
    }

  if (no_insn_rescan)
    df_set_flags (DF_NO_INSN_RESCAN);
  if (defer_insn_rescan)
    df_set_flags (DF_DEFER_INSN_RESCAN);
}


/* Process all of the deferred rescans or deletions.  */

void
df_process_deferred_rescans (void)
{
  bool no_insn_rescan = false;
  bool defer_insn_rescan = false;
  bitmap_iterator bi;
  unsigned int uid;
  bitmap_head tmp;

  bitmap_initialize (&tmp, &df_bitmap_obstack);

  if (df->changeable_flags & DF_NO_INSN_RESCAN)
    {
      df_clear_flags (DF_NO_INSN_RESCAN);
      no_insn_rescan = true;
    }

  if (df->changeable_flags & DF_DEFER_INSN_RESCAN)
    {
      df_clear_flags (DF_DEFER_INSN_RESCAN);
      defer_insn_rescan = true;
    }

  if (dump_file)
    fprintf (dump_file, "starting the processing of deferred insns\n");

  bitmap_copy (&tmp, &df->insns_to_delete);
  EXECUTE_IF_SET_IN_BITMAP (&tmp, 0, uid, bi)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	df_insn_info_delete (uid);
    }

  bitmap_copy (&tmp, &df->insns_to_rescan);
  EXECUTE_IF_SET_IN_BITMAP (&tmp, 0, uid, bi)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	df_insn_rescan (insn_info->insn);
    }

  bitmap_copy (&tmp, &df->insns_to_notes_rescan);
  EXECUTE_IF_SET_IN_BITMAP (&tmp, 0, uid, bi)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	df_notes_rescan (insn_info->insn);
    }

  if (dump_file)
    fprintf (dump_file, "ending the processing of deferred insns\n");

  bitmap_clear (&tmp);
  bitmap_clear (&df->insns_to_delete);
  bitmap_clear (&df->insns_to_rescan);
  bitmap_clear (&df->insns_to_notes_rescan);

  if (no_insn_rescan)
    df_set_flags (DF_NO_INSN_RESCAN);
  if (defer_insn_rescan)
    df_set_flags (DF_DEFER_INSN_RESCAN);

  /* If someone changed regs_ever_live during this pass, fix up the
     entry and exit blocks.  */
  if (df->redo_entry_and_exit)
    {
      df_update_entry_exit_and_calls ();
      df->redo_entry_and_exit = false;
    }
}


/* Count the number of refs. Include the defs if INCLUDE_DEFS. Include
   the uses if INCLUDE_USES. Include the eq_uses if
   INCLUDE_EQ_USES.  */

static unsigned int
df_count_refs (bool include_defs, bool include_uses,
	       bool include_eq_uses)
{
  unsigned int regno;
  int size = 0;
  unsigned int m = df->regs_inited;

  for (regno = 0; regno < m; regno++)
    {
      if (include_defs)
	size += DF_REG_DEF_COUNT (regno);
      if (include_uses)
	size += DF_REG_USE_COUNT (regno);
      if (include_eq_uses)
	size += DF_REG_EQ_USE_COUNT (regno);
    }
  return size;
}


/* Take build ref table for either the uses or defs from the reg-use
   or reg-def chains.  This version processes the refs in reg order
   which is likely to be best if processing the whole function.  */

static void
df_reorganize_refs_by_reg_by_reg (struct df_ref_info *ref_info,
				  bool include_defs,
				  bool include_uses,
				  bool include_eq_uses)
{
  unsigned int m = df->regs_inited;
  unsigned int regno;
  unsigned int offset = 0;
  unsigned int start;

  if (df->changeable_flags & DF_NO_HARD_REGS)
    {
      start = FIRST_PSEUDO_REGISTER;
      memset (ref_info->begin, 0, sizeof (int) * FIRST_PSEUDO_REGISTER);
      memset (ref_info->count, 0, sizeof (int) * FIRST_PSEUDO_REGISTER);
    }
  else
    start = 0;

  ref_info->total_size
    = df_count_refs (include_defs, include_uses, include_eq_uses);

  df_check_and_grow_ref_info (ref_info, 1);

  for (regno = start; regno < m; regno++)
    {
      int count = 0;
      ref_info->begin[regno] = offset;
      if (include_defs)
	{
	  df_ref ref = DF_REG_DEF_CHAIN (regno);
	  while (ref)
	    {
	      ref_info->refs[offset] = ref;
	      DF_REF_ID (ref) = offset++;
	      count++;
	      ref = DF_REF_NEXT_REG (ref);
	      gcc_checking_assert (offset < ref_info->refs_size);
	    }
	}
      if (include_uses)
	{
	  df_ref ref = DF_REG_USE_CHAIN (regno);
	  while (ref)
	    {
	      ref_info->refs[offset] = ref;
	      DF_REF_ID (ref) = offset++;
	      count++;
	      ref = DF_REF_NEXT_REG (ref);
	      gcc_checking_assert (offset < ref_info->refs_size);
	    }
	}
      if (include_eq_uses)
	{
	  df_ref ref = DF_REG_EQ_USE_CHAIN (regno);
	  while (ref)
	    {
	      ref_info->refs[offset] = ref;
	      DF_REF_ID (ref) = offset++;
	      count++;
	      ref = DF_REF_NEXT_REG (ref);
	      gcc_checking_assert (offset < ref_info->refs_size);
	    }
	}
      ref_info->count[regno] = count;
    }

  /* The bitmap size is not decremented when refs are deleted.  So
     reset it now that we have squished out all of the empty
     slots.  */
  ref_info->table_size = offset;
}


/* Take build ref table for either the uses or defs from the reg-use
   or reg-def chains.  This version processes the refs in insn order
   which is likely to be best if processing some segment of the
   function.  */

static void
df_reorganize_refs_by_reg_by_insn (struct df_ref_info *ref_info,
				   bool include_defs,
				   bool include_uses,
				   bool include_eq_uses)
{
  bitmap_iterator bi;
  unsigned int bb_index;
  unsigned int m = df->regs_inited;
  unsigned int offset = 0;
  unsigned int r;
  unsigned int start
    = (df->changeable_flags & DF_NO_HARD_REGS) ? FIRST_PSEUDO_REGISTER : 0;

  memset (ref_info->begin, 0, sizeof (int) * df->regs_inited);
  memset (ref_info->count, 0, sizeof (int) * df->regs_inited);

  ref_info->total_size = df_count_refs (include_defs, include_uses, include_eq_uses);
  df_check_and_grow_ref_info (ref_info, 1);

  EXECUTE_IF_SET_IN_BITMAP (df->blocks_to_analyze, 0, bb_index, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bb_index);
      rtx insn;
      df_ref *ref_rec;

      if (include_defs)
	for (ref_rec = df_get_artificial_defs (bb_index); *ref_rec; ref_rec++)
	  {
	    unsigned int regno = DF_REF_REGNO (*ref_rec);
	    ref_info->count[regno]++;
	  }
      if (include_uses)
	for (ref_rec = df_get_artificial_uses (bb_index); *ref_rec; ref_rec++)
	  {
	    unsigned int regno = DF_REF_REGNO (*ref_rec);
	    ref_info->count[regno]++;
	  }

      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      unsigned int uid = INSN_UID (insn);

	      if (include_defs)
		for (ref_rec = DF_INSN_UID_DEFS (uid); *ref_rec; ref_rec++)
		  {
		    unsigned int regno = DF_REF_REGNO (*ref_rec);
		    ref_info->count[regno]++;
		  }
	      if (include_uses)
		for (ref_rec = DF_INSN_UID_USES (uid); *ref_rec; ref_rec++)
		  {
		    unsigned int regno = DF_REF_REGNO (*ref_rec);
		    ref_info->count[regno]++;
		  }
	      if (include_eq_uses)
		for (ref_rec = DF_INSN_UID_EQ_USES (uid); *ref_rec; ref_rec++)
		  {
		    unsigned int regno = DF_REF_REGNO (*ref_rec);
		    ref_info->count[regno]++;
		  }
	    }
	}
    }

  for (r = start; r < m; r++)
    {
      ref_info->begin[r] = offset;
      offset += ref_info->count[r];
      ref_info->count[r] = 0;
    }

  EXECUTE_IF_SET_IN_BITMAP (df->blocks_to_analyze, 0, bb_index, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bb_index);
      rtx insn;
      df_ref *ref_rec;

      if (include_defs)
	for (ref_rec = df_get_artificial_defs (bb_index); *ref_rec; ref_rec++)
	  {
	    df_ref ref = *ref_rec;
	    unsigned int regno = DF_REF_REGNO (ref);
	    if (regno >= start)
	      {
		unsigned int id
		  = ref_info->begin[regno] + ref_info->count[regno]++;
		DF_REF_ID (ref) = id;
		ref_info->refs[id] = ref;
	      }
	  }
      if (include_uses)
	for (ref_rec = df_get_artificial_uses (bb_index); *ref_rec; ref_rec++)
	  {
	    df_ref ref = *ref_rec;
	    unsigned int regno = DF_REF_REGNO (ref);
	    if (regno >= start)
	      {
		unsigned int id
		  = ref_info->begin[regno] + ref_info->count[regno]++;
		DF_REF_ID (ref) = id;
		ref_info->refs[id] = ref;
	      }
	  }

      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      unsigned int uid = INSN_UID (insn);

	      if (include_defs)
		for (ref_rec = DF_INSN_UID_DEFS (uid); *ref_rec; ref_rec++)
		  {
		    df_ref ref = *ref_rec;
		    unsigned int regno = DF_REF_REGNO (ref);
		    if (regno >= start)
		      {
			unsigned int id
			  = ref_info->begin[regno] + ref_info->count[regno]++;
			DF_REF_ID (ref) = id;
			ref_info->refs[id] = ref;
		      }
		  }
	      if (include_uses)
		for (ref_rec = DF_INSN_UID_USES (uid); *ref_rec; ref_rec++)
		  {
		    df_ref ref = *ref_rec;
		    unsigned int regno = DF_REF_REGNO (ref);
		    if (regno >= start)
		      {
			unsigned int id
			  = ref_info->begin[regno] + ref_info->count[regno]++;
			DF_REF_ID (ref) = id;
			ref_info->refs[id] = ref;
		      }
		  }
	      if (include_eq_uses)
		for (ref_rec = DF_INSN_UID_EQ_USES (uid); *ref_rec; ref_rec++)
		  {
		    df_ref ref = *ref_rec;
		    unsigned int regno = DF_REF_REGNO (ref);
		    if (regno >= start)
		      {
			unsigned int id
			  = ref_info->begin[regno] + ref_info->count[regno]++;
			DF_REF_ID (ref) = id;
			ref_info->refs[id] = ref;
		      }
		  }
	    }
	}
    }

  /* The bitmap size is not decremented when refs are deleted.  So
     reset it now that we have squished out all of the empty
     slots.  */

  ref_info->table_size = offset;
}

/* Take build ref table for either the uses or defs from the reg-use
   or reg-def chains.  */

static void
df_reorganize_refs_by_reg (struct df_ref_info *ref_info,
			   bool include_defs,
			   bool include_uses,
			   bool include_eq_uses)
{
  if (df->analyze_subset)
    df_reorganize_refs_by_reg_by_insn (ref_info, include_defs,
				       include_uses, include_eq_uses);
  else
    df_reorganize_refs_by_reg_by_reg (ref_info, include_defs,
				       include_uses, include_eq_uses);
}


/* Add the refs in REF_VEC to the table in REF_INFO starting at OFFSET.  */
static unsigned int
df_add_refs_to_table (unsigned int offset,
		      struct df_ref_info *ref_info,
		      df_ref *ref_vec)
{
  while (*ref_vec)
    {
      df_ref ref = *ref_vec;
      if ((!(df->changeable_flags & DF_NO_HARD_REGS))
	  || (DF_REF_REGNO (ref) >= FIRST_PSEUDO_REGISTER))
	{
	  ref_info->refs[offset] = ref;
	  DF_REF_ID (*ref_vec) = offset++;
	}
      ref_vec++;
    }
  return offset;
}


/* Count the number of refs in all of the insns of BB. Include the
   defs if INCLUDE_DEFS. Include the uses if INCLUDE_USES. Include the
   eq_uses if INCLUDE_EQ_USES.  */

static unsigned int
df_reorganize_refs_by_insn_bb (basic_block bb, unsigned int offset,
			       struct df_ref_info *ref_info,
			       bool include_defs, bool include_uses,
			       bool include_eq_uses)
{
  rtx insn;

  if (include_defs)
    offset = df_add_refs_to_table (offset, ref_info,
				   df_get_artificial_defs (bb->index));
  if (include_uses)
    offset = df_add_refs_to_table (offset, ref_info,
				   df_get_artificial_uses (bb->index));

  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
	unsigned int uid = INSN_UID (insn);
	if (include_defs)
	  offset = df_add_refs_to_table (offset, ref_info,
					 DF_INSN_UID_DEFS (uid));
	if (include_uses)
	  offset = df_add_refs_to_table (offset, ref_info,
					 DF_INSN_UID_USES (uid));
	if (include_eq_uses)
	  offset = df_add_refs_to_table (offset, ref_info,
					 DF_INSN_UID_EQ_USES (uid));
      }
  return offset;
}


/* Organize the refs by insn into the table in REF_INFO.  If
   blocks_to_analyze is defined, use that set, otherwise the entire
   program.  Include the defs if INCLUDE_DEFS. Include the uses if
   INCLUDE_USES. Include the eq_uses if INCLUDE_EQ_USES.  */

static void
df_reorganize_refs_by_insn (struct df_ref_info *ref_info,
			    bool include_defs, bool include_uses,
			    bool include_eq_uses)
{
  basic_block bb;
  unsigned int offset = 0;

  ref_info->total_size = df_count_refs (include_defs, include_uses, include_eq_uses);
  df_check_and_grow_ref_info (ref_info, 1);
  if (df->blocks_to_analyze)
    {
      bitmap_iterator bi;
      unsigned int index;

      EXECUTE_IF_SET_IN_BITMAP (df->blocks_to_analyze, 0, index, bi)
	{
	  offset = df_reorganize_refs_by_insn_bb (BASIC_BLOCK_FOR_FN (cfun,
								      index),
						  offset, ref_info,
						  include_defs, include_uses,
						  include_eq_uses);
	}

      ref_info->table_size = offset;
    }
  else
    {
      FOR_ALL_BB_FN (bb, cfun)
	offset = df_reorganize_refs_by_insn_bb (bb, offset, ref_info,
						include_defs, include_uses,
						include_eq_uses);
      ref_info->table_size = offset;
    }
}


/* If the use refs in DF are not organized, reorganize them.  */

void
df_maybe_reorganize_use_refs (enum df_ref_order order)
{
  if (order == df->use_info.ref_order)
    return;

  switch (order)
    {
    case DF_REF_ORDER_BY_REG:
      df_reorganize_refs_by_reg (&df->use_info, false, true, false);
      break;

    case DF_REF_ORDER_BY_REG_WITH_NOTES:
      df_reorganize_refs_by_reg (&df->use_info, false, true, true);
      break;

    case DF_REF_ORDER_BY_INSN:
      df_reorganize_refs_by_insn (&df->use_info, false, true, false);
      break;

    case DF_REF_ORDER_BY_INSN_WITH_NOTES:
      df_reorganize_refs_by_insn (&df->use_info, false, true, true);
      break;

    case DF_REF_ORDER_NO_TABLE:
      free (df->use_info.refs);
      df->use_info.refs = NULL;
      df->use_info.refs_size = 0;
      break;

    case DF_REF_ORDER_UNORDERED:
    case DF_REF_ORDER_UNORDERED_WITH_NOTES:
      gcc_unreachable ();
      break;
    }

  df->use_info.ref_order = order;
}


/* If the def refs in DF are not organized, reorganize them.  */

void
df_maybe_reorganize_def_refs (enum df_ref_order order)
{
  if (order == df->def_info.ref_order)
    return;

  switch (order)
    {
    case DF_REF_ORDER_BY_REG:
      df_reorganize_refs_by_reg (&df->def_info, true, false, false);
      break;

    case DF_REF_ORDER_BY_INSN:
      df_reorganize_refs_by_insn (&df->def_info, true, false, false);
      break;

    case DF_REF_ORDER_NO_TABLE:
      free (df->def_info.refs);
      df->def_info.refs = NULL;
      df->def_info.refs_size = 0;
      break;

    case DF_REF_ORDER_BY_INSN_WITH_NOTES:
    case DF_REF_ORDER_BY_REG_WITH_NOTES:
    case DF_REF_ORDER_UNORDERED:
    case DF_REF_ORDER_UNORDERED_WITH_NOTES:
      gcc_unreachable ();
      break;
    }

  df->def_info.ref_order = order;
}


/* Change all of the basic block references in INSN to use the insn's
   current basic block.  This function is called from routines that move
   instructions from one block to another.  */

void
df_insn_change_bb (rtx insn, basic_block new_bb)
{
  basic_block old_bb = BLOCK_FOR_INSN (insn);
  struct df_insn_info *insn_info;
  unsigned int uid = INSN_UID (insn);

  if (old_bb == new_bb)
    return;

  set_block_for_insn (insn, new_bb);

  if (!df)
    return;

  if (dump_file)
    fprintf (dump_file, "changing bb of uid %d\n", uid);

  insn_info = DF_INSN_UID_SAFE_GET (uid);
  if (insn_info == NULL)
    {
      if (dump_file)
	fprintf (dump_file, "  unscanned insn\n");
      df_insn_rescan (insn);
      return;
    }

  if (!INSN_P (insn))
    return;

  df_set_bb_dirty (new_bb);
  if (old_bb)
    {
      if (dump_file)
	fprintf (dump_file, "  from %d to %d\n",
		 old_bb->index, new_bb->index);
      df_set_bb_dirty (old_bb);
    }
  else
    if (dump_file)
      fprintf (dump_file, "  to %d\n", new_bb->index);
}


/* Helper function for df_ref_change_reg_with_loc.  */

static void
df_ref_change_reg_with_loc_1 (struct df_reg_info *old_df,
			      struct df_reg_info *new_df,
			      int new_regno, rtx loc)
{
  df_ref the_ref = old_df->reg_chain;

  while (the_ref)
    {
      if ((!DF_REF_IS_ARTIFICIAL (the_ref))
	  && DF_REF_LOC (the_ref)
	  && (*DF_REF_LOC (the_ref) == loc))
	{
	  df_ref next_ref = DF_REF_NEXT_REG (the_ref);
	  df_ref prev_ref = DF_REF_PREV_REG (the_ref);
	  df_ref *ref_vec, *ref_vec_t;
	  struct df_insn_info *insn_info = DF_REF_INSN_INFO (the_ref);
	  unsigned int count = 0;

	  DF_REF_REGNO (the_ref) = new_regno;
	  DF_REF_REG (the_ref) = regno_reg_rtx[new_regno];

	  /* Pull the_ref out of the old regno chain.  */
	  if (prev_ref)
	    DF_REF_NEXT_REG (prev_ref) = next_ref;
	  else
	    old_df->reg_chain = next_ref;
	  if (next_ref)
	    DF_REF_PREV_REG (next_ref) = prev_ref;
	  old_df->n_refs--;

	  /* Put the ref into the new regno chain.  */
	  DF_REF_PREV_REG (the_ref) = NULL;
	  DF_REF_NEXT_REG (the_ref) = new_df->reg_chain;
	  if (new_df->reg_chain)
	    DF_REF_PREV_REG (new_df->reg_chain) = the_ref;
	  new_df->reg_chain = the_ref;
	  new_df->n_refs++;
	  if (DF_REF_BB (the_ref))
	    df_set_bb_dirty (DF_REF_BB (the_ref));

	  /* Need to sort the record again that the ref was in because
	     the regno is a sorting key.  First, find the right
	     record.  */
	  if (DF_REF_FLAGS (the_ref) & DF_REF_IN_NOTE)
	    ref_vec = insn_info->eq_uses;
	  else
	    ref_vec = insn_info->uses;
	  if (dump_file)
	    fprintf (dump_file, "changing reg in insn %d\n",
		     DF_REF_INSN_UID (the_ref));

	  ref_vec_t = ref_vec;

	  /* Find the length.  */
	  while (*ref_vec_t)
	    {
	      count++;
	      ref_vec_t++;
	    }
	  qsort (ref_vec, count, sizeof (df_ref ), df_ref_compare);

	  the_ref = next_ref;
	}
      else
	the_ref = DF_REF_NEXT_REG (the_ref);
    }
}


/* Change the regno of all refs that contained LOC from OLD_REGNO to
   NEW_REGNO.  Refs that do not match LOC are not changed which means
   that artificial refs are not changed since they have no loc.  This
   call is to support the SET_REGNO macro. */

void
df_ref_change_reg_with_loc (int old_regno, int new_regno, rtx loc)
{
  if ((!df) || (old_regno == -1) || (old_regno == new_regno))
    return;

  df_grow_reg_info ();

  df_ref_change_reg_with_loc_1 (DF_REG_DEF_GET (old_regno),
				DF_REG_DEF_GET (new_regno), new_regno, loc);
  df_ref_change_reg_with_loc_1 (DF_REG_USE_GET (old_regno),
				DF_REG_USE_GET (new_regno), new_regno, loc);
  df_ref_change_reg_with_loc_1 (DF_REG_EQ_USE_GET (old_regno),
				DF_REG_EQ_USE_GET (new_regno), new_regno, loc);
}


/* Delete the mw_hardregs that point into the eq_notes.  */

static unsigned int
df_mw_hardreg_chain_delete_eq_uses (struct df_insn_info *insn_info)
{
  struct df_mw_hardreg **mw_vec = insn_info->mw_hardregs;
  unsigned int deleted = 0;
  unsigned int count = 0;
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;

  if (!*mw_vec)
    return 0;

  while (*mw_vec)
    {
      if ((*mw_vec)->flags & DF_REF_IN_NOTE)
	{
	  struct df_mw_hardreg **temp_vec = mw_vec;

	  pool_free (problem_data->mw_reg_pool, *mw_vec);
	  temp_vec = mw_vec;
	  /* Shove the remaining ones down one to fill the gap.  While
	     this looks n**2, it is highly unusual to have any mw regs
	     in eq_notes and the chances of more than one are almost
	     non existent.  */
	  while (*temp_vec)
	    {
	      *temp_vec = *(temp_vec + 1);
	      temp_vec++;
	    }
	  deleted++;
	}
      else
	{
	  mw_vec++;
	  count++;
	}
    }

  if (count == 0)
    {
      df_scan_free_mws_vec (insn_info->mw_hardregs);
      insn_info->mw_hardregs = df_null_mw_rec;
      return 0;
    }
  return deleted;
}


/* Rescan only the REG_EQUIV/REG_EQUAL notes part of INSN.  */

void
df_notes_rescan (rtx insn)
{
  struct df_insn_info *insn_info;
  unsigned int uid = INSN_UID (insn);

  if (!df)
    return;

  /* The client has disabled rescanning and plans to do it itself.  */
  if (df->changeable_flags & DF_NO_INSN_RESCAN)
    return;

  /* Do nothing if the insn hasn't been emitted yet.  */
  if (!BLOCK_FOR_INSN (insn))
    return;

  df_grow_bb_info (df_scan);
  df_grow_reg_info ();

  insn_info = DF_INSN_UID_SAFE_GET (INSN_UID (insn));

  /* The client has deferred rescanning.  */
  if (df->changeable_flags & DF_DEFER_INSN_RESCAN)
    {
      if (!insn_info)
	{
	  insn_info = df_insn_create_insn_record (insn);
	  insn_info->defs = df_null_ref_rec;
	  insn_info->uses = df_null_ref_rec;
	  insn_info->eq_uses = df_null_ref_rec;
	  insn_info->mw_hardregs = df_null_mw_rec;
	}

      bitmap_clear_bit (&df->insns_to_delete, uid);
      /* If the insn is set to be rescanned, it does not need to also
	 be notes rescanned.  */
      if (!bitmap_bit_p (&df->insns_to_rescan, uid))
	bitmap_set_bit (&df->insns_to_notes_rescan, INSN_UID (insn));
      return;
    }

  bitmap_clear_bit (&df->insns_to_delete, uid);
  bitmap_clear_bit (&df->insns_to_notes_rescan, uid);

  if (insn_info)
    {
      basic_block bb = BLOCK_FOR_INSN (insn);
      rtx note;
      struct df_collection_rec collection_rec;
      unsigned int num_deleted;
      unsigned int mw_len;

      num_deleted = df_mw_hardreg_chain_delete_eq_uses (insn_info);
      df_ref_chain_delete (insn_info->eq_uses);
      insn_info->eq_uses = NULL;

      /* Process REG_EQUIV/REG_EQUAL notes */
      for (note = REG_NOTES (insn); note;
	   note = XEXP (note, 1))
	{
	  switch (REG_NOTE_KIND (note))
	    {
	    case REG_EQUIV:
	    case REG_EQUAL:
	      df_uses_record (&collection_rec,
			      &XEXP (note, 0), DF_REF_REG_USE,
			      bb, insn_info, DF_REF_IN_NOTE);
	    default:
	      break;
	    }
	}

      /* Find some place to put any new mw_hardregs.  */
      df_canonize_collection_rec (&collection_rec);
      mw_len = collection_rec.mw_vec.length ();
      if (mw_len)
	{
	  unsigned int count = 0;
	  struct df_mw_hardreg **mw_rec = insn_info->mw_hardregs;
	  while (*mw_rec)
	    {
	      count++;
	      mw_rec++;
	    }

	  if (count)
	    {
	      /* Append to the end of the existing record after
		 expanding it if necessary.  */
	      if (mw_len > num_deleted)
		{
		  insn_info->mw_hardregs =
		    XRESIZEVEC (struct df_mw_hardreg *,
				insn_info->mw_hardregs,
				count + 1 + mw_len);
		}
	      memcpy (&insn_info->mw_hardregs[count],
		      collection_rec.mw_vec.address (),
		      mw_len * sizeof (struct df_mw_hardreg *));
	      insn_info->mw_hardregs[count + mw_len] = NULL;
	      qsort (insn_info->mw_hardregs, count + mw_len,
		     sizeof (struct df_mw_hardreg *), df_mw_compare);
	    }
	  else
	    {
	      /* No vector there. */
	      insn_info->mw_hardregs
		= XNEWVEC (struct df_mw_hardreg*, 1 + mw_len);
	      memcpy (insn_info->mw_hardregs,
		      collection_rec.mw_vec.address (),
		      mw_len * sizeof (struct df_mw_hardreg *));
	      insn_info->mw_hardregs[mw_len] = NULL;
	    }
	}
      df_refs_add_to_chains (&collection_rec, bb, insn, copy_eq_uses);
    }
  else
    df_insn_rescan (insn);

}


/*----------------------------------------------------------------------------
   Hard core instruction scanning code.  No external interfaces here,
   just a lot of routines that look inside insns.
----------------------------------------------------------------------------*/


/* Return true if the contents of two df_ref's are identical.
   It ignores DF_REF_MARKER.  */

static bool
df_ref_equal_p (df_ref ref1, df_ref ref2)
{
  if (!ref2)
    return false;

  if (ref1 == ref2)
    return true;

  if (DF_REF_CLASS (ref1) != DF_REF_CLASS (ref2)
      || DF_REF_REGNO (ref1) != DF_REF_REGNO (ref2)
      || DF_REF_REG (ref1) != DF_REF_REG (ref2)
      || DF_REF_TYPE (ref1) != DF_REF_TYPE (ref2)
      || ((DF_REF_FLAGS (ref1) & ~(DF_REF_REG_MARKER + DF_REF_MW_HARDREG))
	  != (DF_REF_FLAGS (ref2) & ~(DF_REF_REG_MARKER + DF_REF_MW_HARDREG)))
      || DF_REF_BB (ref1) != DF_REF_BB (ref2)
      || DF_REF_INSN_INFO (ref1) != DF_REF_INSN_INFO (ref2))
    return false;

  switch (DF_REF_CLASS (ref1))
    {
    case DF_REF_ARTIFICIAL:
    case DF_REF_BASE:
      return true;

    case DF_REF_REGULAR:
      return DF_REF_LOC (ref1) == DF_REF_LOC (ref2);

    default:
      gcc_unreachable ();
    }
  return false;
}


/* Compare REF1 and REF2 for sorting.  This is only called from places
   where all of the refs are of the same type, in the same insn, and
   have the same bb.  So these fields are not checked.  */

static int
df_ref_compare (const void *r1, const void *r2)
{
  const df_ref ref1 = *(const df_ref *)r1;
  const df_ref ref2 = *(const df_ref *)r2;

  if (ref1 == ref2)
    return 0;

  if (DF_REF_CLASS (ref1) != DF_REF_CLASS (ref2))
    return (int)DF_REF_CLASS (ref1) - (int)DF_REF_CLASS (ref2);

  if (DF_REF_REGNO (ref1) != DF_REF_REGNO (ref2))
    return (int)DF_REF_REGNO (ref1) - (int)DF_REF_REGNO (ref2);

  if (DF_REF_TYPE (ref1) != DF_REF_TYPE (ref2))
    return (int)DF_REF_TYPE (ref1) - (int)DF_REF_TYPE (ref2);

  if (DF_REF_REG (ref1) != DF_REF_REG (ref2))
    return (int)DF_REF_ORDER (ref1) - (int)DF_REF_ORDER (ref2);

  /* Cannot look at the LOC field on artificial refs.  */
  if (DF_REF_CLASS (ref1) != DF_REF_ARTIFICIAL
      && DF_REF_LOC (ref1) != DF_REF_LOC (ref2))
    return (int)DF_REF_ORDER (ref1) - (int)DF_REF_ORDER (ref2);

  if (DF_REF_FLAGS (ref1) != DF_REF_FLAGS (ref2))
    {
      /* If two refs are identical except that one of them has is from
	 a mw and one is not, we need to have the one with the mw
	 first.  */
      if (DF_REF_FLAGS_IS_SET (ref1, DF_REF_MW_HARDREG) ==
	  DF_REF_FLAGS_IS_SET (ref2, DF_REF_MW_HARDREG))
	return DF_REF_FLAGS (ref1) - DF_REF_FLAGS (ref2);
      else if (DF_REF_FLAGS_IS_SET (ref1, DF_REF_MW_HARDREG))
	return -1;
      else
	return 1;
    }

  return (int)DF_REF_ORDER (ref1) - (int)DF_REF_ORDER (ref2);
}

static void
df_swap_refs (vec<df_ref, va_heap> *ref_vec, int i, int j)
{
  df_ref tmp = (*ref_vec)[i];
  (*ref_vec)[i] = (*ref_vec)[j];
  (*ref_vec)[j] = tmp;
}

/* Sort and compress a set of refs.  */

static void
df_sort_and_compress_refs (vec<df_ref, va_heap> *ref_vec)
{
  unsigned int count;
  unsigned int i;
  unsigned int dist = 0;

  count = ref_vec->length ();

  /* If there are 1 or 0 elements, there is nothing to do.  */
  if (count < 2)
    return;
  else if (count == 2)
    {
      df_ref r0 = (*ref_vec)[0];
      df_ref r1 = (*ref_vec)[1];
      if (df_ref_compare (&r0, &r1) > 0)
        df_swap_refs (ref_vec, 0, 1);
    }
  else
    {
      for (i = 0; i < count - 1; i++)
	{
	  df_ref r0 = (*ref_vec)[i];
	  df_ref r1 = (*ref_vec)[i + 1];
	  if (df_ref_compare (&r0, &r1) >= 0)
	    break;
	}
      /* If the array is already strictly ordered,
         which is the most common case for large COUNT case
         (which happens for CALL INSNs),
         no need to sort and filter out duplicate.
         Simply return the count.
         Make sure DF_GET_ADD_REFS adds refs in the increasing order
         of DF_REF_COMPARE.  */
      if (i == count - 1)
        return;
      ref_vec->qsort (df_ref_compare);
    }

  for (i=0; i<count-dist; i++)
    {
      /* Find the next ref that is not equal to the current ref.  */
      while (i + dist + 1 < count
	     && df_ref_equal_p ((*ref_vec)[i],
				(*ref_vec)[i + dist + 1]))
	{
	  df_free_ref ((*ref_vec)[i + dist + 1]);
	  dist++;
	}
      /* Copy it down to the next position.  */
      if (dist && i + dist + 1 < count)
	(*ref_vec)[i + 1] = (*ref_vec)[i + dist + 1];
    }

  count -= dist;
  ref_vec->truncate (count);
}


/* Return true if the contents of two df_ref's are identical.
   It ignores DF_REF_MARKER.  */

static bool
df_mw_equal_p (struct df_mw_hardreg *mw1, struct df_mw_hardreg *mw2)
{
  if (!mw2)
    return false;
  return (mw1 == mw2) ||
    (mw1->mw_reg == mw2->mw_reg
     && mw1->type == mw2->type
     && mw1->flags == mw2->flags
     && mw1->start_regno == mw2->start_regno
     && mw1->end_regno == mw2->end_regno);
}


/* Compare MW1 and MW2 for sorting.  */

static int
df_mw_compare (const void *m1, const void *m2)
{
  const struct df_mw_hardreg *const mw1 = *(const struct df_mw_hardreg *const*)m1;
  const struct df_mw_hardreg *const mw2 = *(const struct df_mw_hardreg *const*)m2;

  if (mw1 == mw2)
    return 0;

  if (mw1->type != mw2->type)
    return mw1->type - mw2->type;

  if (mw1->flags != mw2->flags)
    return mw1->flags - mw2->flags;

  if (mw1->start_regno != mw2->start_regno)
    return mw1->start_regno - mw2->start_regno;

  if (mw1->end_regno != mw2->end_regno)
    return mw1->end_regno - mw2->end_regno;

  if (mw1->mw_reg != mw2->mw_reg)
    return mw1->mw_order - mw2->mw_order;

  return 0;
}


/* Sort and compress a set of refs.  */

static void
df_sort_and_compress_mws (vec<df_mw_hardreg_ptr, va_heap> *mw_vec)
{
  unsigned int count;
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;
  unsigned int i;
  unsigned int dist = 0;

  count = mw_vec->length ();
  if (count < 2)
    return;
  else if (count == 2)
    {
      struct df_mw_hardreg *m0 = (*mw_vec)[0];
      struct df_mw_hardreg *m1 = (*mw_vec)[1];
      if (df_mw_compare (&m0, &m1) > 0)
        {
          struct df_mw_hardreg *tmp = (*mw_vec)[0];
	  (*mw_vec)[0] = (*mw_vec)[1];
	  (*mw_vec)[1] = tmp;
        }
    }
  else
    mw_vec->qsort (df_mw_compare);

  for (i=0; i<count-dist; i++)
    {
      /* Find the next ref that is not equal to the current ref.  */
      while (i + dist + 1 < count
	     && df_mw_equal_p ((*mw_vec)[i], (*mw_vec)[i + dist + 1]))
	{
	  pool_free (problem_data->mw_reg_pool,
		     (*mw_vec)[i + dist + 1]);
	  dist++;
	}
      /* Copy it down to the next position.  */
      if (dist && i + dist + 1 < count)
	(*mw_vec)[i + 1] = (*mw_vec)[i + dist + 1];
    }

  count -= dist;
  mw_vec->truncate (count);
}


/* Sort and remove duplicates from the COLLECTION_REC.  */

static void
df_canonize_collection_rec (struct df_collection_rec *collection_rec)
{
  df_sort_and_compress_refs (&collection_rec->def_vec);
  df_sort_and_compress_refs (&collection_rec->use_vec);
  df_sort_and_compress_refs (&collection_rec->eq_use_vec);
  df_sort_and_compress_mws (&collection_rec->mw_vec);
}


/* Add the new df_ref to appropriate reg_info/ref_info chains.  */

static void
df_install_ref (df_ref this_ref,
		struct df_reg_info *reg_info,
		struct df_ref_info *ref_info,
		bool add_to_table)
{
  unsigned int regno = DF_REF_REGNO (this_ref);
  /* Add the ref to the reg_{def,use,eq_use} chain.  */
  df_ref head = reg_info->reg_chain;

  reg_info->reg_chain = this_ref;
  reg_info->n_refs++;

  if (DF_REF_FLAGS_IS_SET (this_ref, DF_HARD_REG_LIVE))
    {
      gcc_assert (regno < FIRST_PSEUDO_REGISTER);
      df->hard_regs_live_count[regno]++;
    }

  gcc_checking_assert (DF_REF_NEXT_REG (this_ref) == NULL
		       && DF_REF_PREV_REG (this_ref) == NULL);

  DF_REF_NEXT_REG (this_ref) = head;

  /* We cannot actually link to the head of the chain.  */
  DF_REF_PREV_REG (this_ref) = NULL;

  if (head)
    DF_REF_PREV_REG (head) = this_ref;

  if (add_to_table)
    {
      gcc_assert (ref_info->ref_order != DF_REF_ORDER_NO_TABLE);
      df_check_and_grow_ref_info (ref_info, 1);
      DF_REF_ID (this_ref) = ref_info->table_size;
      /* Add the ref to the big array of defs.  */
      ref_info->refs[ref_info->table_size] = this_ref;
      ref_info->table_size++;
    }
  else
    DF_REF_ID (this_ref) = -1;

  ref_info->total_size++;
}


/* This function takes one of the groups of refs (defs, uses or
   eq_uses) and installs the entire group into the insn.  It also adds
   each of these refs into the appropriate chains.  */

static df_ref *
df_install_refs (basic_block bb,
		 const vec<df_ref, va_heap> *old_vec,
		 struct df_reg_info **reg_info,
		 struct df_ref_info *ref_info,
		 bool is_notes)
{
  unsigned int count = old_vec->length ();
  if (count)
    {
      df_ref *new_vec = XNEWVEC (df_ref, count + 1);
      bool add_to_table;
      df_ref this_ref;
      unsigned int ix;

      switch (ref_info->ref_order)
	{
	case DF_REF_ORDER_UNORDERED_WITH_NOTES:
	case DF_REF_ORDER_BY_REG_WITH_NOTES:
	case DF_REF_ORDER_BY_INSN_WITH_NOTES:
	  ref_info->ref_order = DF_REF_ORDER_UNORDERED_WITH_NOTES;
	  add_to_table = true;
	  break;
	case DF_REF_ORDER_UNORDERED:
	case DF_REF_ORDER_BY_REG:
	case DF_REF_ORDER_BY_INSN:
	  ref_info->ref_order = DF_REF_ORDER_UNORDERED;
	  add_to_table = !is_notes;
	  break;
	default:
	  add_to_table = false;
	  break;
	}

      /* Do not add if ref is not in the right blocks.  */
      if (add_to_table && df->analyze_subset)
	add_to_table = bitmap_bit_p (df->blocks_to_analyze, bb->index);

      FOR_EACH_VEC_ELT (*old_vec, ix, this_ref)
	{
	  new_vec[ix] = this_ref;
	  df_install_ref (this_ref, reg_info[DF_REF_REGNO (this_ref)],
			  ref_info, add_to_table);
	}

      new_vec[count] = NULL;
      return new_vec;
    }
  else
    return df_null_ref_rec;
}


/* This function takes the mws installs the entire group into the
   insn.  */

static struct df_mw_hardreg **
df_install_mws (const vec<df_mw_hardreg_ptr, va_heap> *old_vec)
{
  unsigned int count = old_vec->length ();
  if (count)
    {
      struct df_mw_hardreg **new_vec
	= XNEWVEC (struct df_mw_hardreg*, count + 1);
      memcpy (new_vec, old_vec->address (),
	      sizeof (struct df_mw_hardreg*) * count);
      new_vec[count] = NULL;
      return new_vec;
    }
  else
    return df_null_mw_rec;
}


/* Add a chain of df_refs to appropriate ref chain/reg_info/ref_info
   chains and update other necessary information.  */

static void
df_refs_add_to_chains (struct df_collection_rec *collection_rec,
		       basic_block bb, rtx insn, unsigned int flags)
{
  if (insn)
    {
      struct df_insn_info *insn_rec = DF_INSN_INFO_GET (insn);
      /* If there is a vector in the collection rec, add it to the
	 insn.  A null rec is a signal that the caller will handle the
	 chain specially.  */
      if (flags & copy_defs)
	{
	  df_scan_free_ref_vec (insn_rec->defs);
	  insn_rec->defs
	    = df_install_refs (bb, &collection_rec->def_vec,
			       df->def_regs,
			       &df->def_info, false);
	}
      if (flags & copy_uses)
	{
	  df_scan_free_ref_vec (insn_rec->uses);
	  insn_rec->uses
	    = df_install_refs (bb, &collection_rec->use_vec,
			       df->use_regs,
			       &df->use_info, false);
	}
      if (flags & copy_eq_uses)
	{
	  df_scan_free_ref_vec (insn_rec->eq_uses);
	  insn_rec->eq_uses
	    = df_install_refs (bb, &collection_rec->eq_use_vec,
			       df->eq_use_regs,
			       &df->use_info, true);
	}
      if (flags & copy_mw)
	{
	  df_scan_free_mws_vec (insn_rec->mw_hardregs);
	  insn_rec->mw_hardregs
	    = df_install_mws (&collection_rec->mw_vec);
	}
    }
  else
    {
      struct df_scan_bb_info *bb_info = df_scan_get_bb_info (bb->index);

      df_scan_free_ref_vec (bb_info->artificial_defs);
      bb_info->artificial_defs
	= df_install_refs (bb, &collection_rec->def_vec,
			   df->def_regs,
			   &df->def_info, false);
      df_scan_free_ref_vec (bb_info->artificial_uses);
      bb_info->artificial_uses
	= df_install_refs (bb, &collection_rec->use_vec,
			   df->use_regs,
			   &df->use_info, false);
    }
}


/* Allocate a ref and initialize its fields.  */

static df_ref
df_ref_create_structure (enum df_ref_class cl,
			 struct df_collection_rec *collection_rec,
			 rtx reg, rtx *loc,
			 basic_block bb, struct df_insn_info *info,
			 enum df_ref_type ref_type,
			 int ref_flags)
{
  df_ref this_ref = NULL;
  int regno = REGNO (GET_CODE (reg) == SUBREG ? SUBREG_REG (reg) : reg);
  struct df_scan_problem_data *problem_data
    = (struct df_scan_problem_data *) df_scan->problem_data;

  switch (cl)
    {
    case DF_REF_BASE:
      this_ref = (df_ref) pool_alloc (problem_data->ref_base_pool);
      gcc_checking_assert (loc == NULL);
      break;

    case DF_REF_ARTIFICIAL:
      this_ref = (df_ref) pool_alloc (problem_data->ref_artificial_pool);
      this_ref->artificial_ref.bb = bb;
      gcc_checking_assert (loc == NULL);
      break;

    case DF_REF_REGULAR:
      this_ref = (df_ref) pool_alloc (problem_data->ref_regular_pool);
      this_ref->regular_ref.loc = loc;
      gcc_checking_assert (loc);
      break;
    }

  DF_REF_CLASS (this_ref) = cl;
  DF_REF_ID (this_ref) = -1;
  DF_REF_REG (this_ref) = reg;
  DF_REF_REGNO (this_ref) =  regno;
  DF_REF_TYPE (this_ref) = ref_type;
  DF_REF_INSN_INFO (this_ref) = info;
  DF_REF_CHAIN (this_ref) = NULL;
  DF_REF_FLAGS (this_ref) = ref_flags;
  DF_REF_NEXT_REG (this_ref) = NULL;
  DF_REF_PREV_REG (this_ref) = NULL;
  DF_REF_ORDER (this_ref) = df->ref_order++;

  /* We need to clear this bit because fwprop, and in the future
     possibly other optimizations sometimes create new refs using ond
     refs as the model.  */
  DF_REF_FLAGS_CLEAR (this_ref, DF_HARD_REG_LIVE);

  /* See if this ref needs to have DF_HARD_REG_LIVE bit set.  */
  if (regno < FIRST_PSEUDO_REGISTER
      && !DF_REF_IS_ARTIFICIAL (this_ref)
      && !DEBUG_INSN_P (DF_REF_INSN (this_ref)))
    {
      if (DF_REF_REG_DEF_P (this_ref))
	{
	  if (!DF_REF_FLAGS_IS_SET (this_ref, DF_REF_MAY_CLOBBER))
	    DF_REF_FLAGS_SET (this_ref, DF_HARD_REG_LIVE);
	}
      else if (!(TEST_HARD_REG_BIT (elim_reg_set, regno)
		 && (regno == FRAME_POINTER_REGNUM
		     || regno == ARG_POINTER_REGNUM)))
	DF_REF_FLAGS_SET (this_ref, DF_HARD_REG_LIVE);
    }

  if (collection_rec)
    {
      if (DF_REF_REG_DEF_P (this_ref))
	collection_rec->def_vec.safe_push (this_ref);
      else if (DF_REF_FLAGS (this_ref) & DF_REF_IN_NOTE)
	collection_rec->eq_use_vec.safe_push (this_ref);
      else
	collection_rec->use_vec.safe_push (this_ref);
    }
  else
    df_install_ref_incremental (this_ref);

  return this_ref;
}


/* Create new references of type DF_REF_TYPE for each part of register REG
   at address LOC within INSN of BB.  */


static void
df_ref_record (enum df_ref_class cl,
	       struct df_collection_rec *collection_rec,
               rtx reg, rtx *loc,
	       basic_block bb, struct df_insn_info *insn_info,
	       enum df_ref_type ref_type,
	       int ref_flags)
{
  unsigned int regno;

  gcc_checking_assert (REG_P (reg) || GET_CODE (reg) == SUBREG);

  regno = REGNO (GET_CODE (reg) == SUBREG ? SUBREG_REG (reg) : reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      struct df_mw_hardreg *hardreg = NULL;
      struct df_scan_problem_data *problem_data
        = (struct df_scan_problem_data *) df_scan->problem_data;
      unsigned int i;
      unsigned int endregno;
      df_ref ref;

      if (GET_CODE (reg) == SUBREG)
	{
	  regno += subreg_regno_offset (regno, GET_MODE (SUBREG_REG (reg)),
					SUBREG_BYTE (reg), GET_MODE (reg));
	  endregno = regno + subreg_nregs (reg);
	}
      else
	endregno = END_HARD_REGNO (reg);

      /*  If this is a multiword hardreg, we create some extra
	  datastructures that will enable us to easily build REG_DEAD
	  and REG_UNUSED notes.  */
      if (collection_rec
	  && (endregno != regno + 1) && insn_info)
	{
	  /* Sets to a subreg of a multiword register are partial.
	     Sets to a non-subreg of a multiword register are not.  */
	  if (GET_CODE (reg) == SUBREG)
	    ref_flags |= DF_REF_PARTIAL;
	  ref_flags |= DF_REF_MW_HARDREG;

	  hardreg = (struct df_mw_hardreg *) pool_alloc (problem_data->mw_reg_pool);
	  hardreg->type = ref_type;
	  hardreg->flags = ref_flags;
	  hardreg->mw_reg = reg;
	  hardreg->start_regno = regno;
	  hardreg->end_regno = endregno - 1;
	  hardreg->mw_order = df->ref_order++;
	  collection_rec->mw_vec.safe_push (hardreg);
	}

      for (i = regno; i < endregno; i++)
	{
	  ref = df_ref_create_structure (cl, collection_rec, regno_reg_rtx[i], loc,
					 bb, insn_info, ref_type, ref_flags);

          gcc_assert (ORIGINAL_REGNO (DF_REF_REG (ref)) == i);
	}
    }
  else
    {
      df_ref_create_structure (cl, collection_rec, reg, loc, bb, insn_info,
			       ref_type, ref_flags);
    }
}


/* A set to a non-paradoxical SUBREG for which the number of word_mode units
   covered by the outer mode is smaller than that covered by the inner mode,
   is a read-modify-write operation.
   This function returns true iff the SUBREG X is such a SUBREG.  */

bool
df_read_modify_subreg_p (rtx x)
{
  unsigned int isize, osize;
  if (GET_CODE (x) != SUBREG)
    return false;
  isize = GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)));
  osize = GET_MODE_SIZE (GET_MODE (x));
  return isize > osize
	 && isize > REGMODE_NATURAL_SIZE (GET_MODE (SUBREG_REG (x)));
}


/* Process all the registers defined in the rtx pointed by LOC.
   Autoincrement/decrement definitions will be picked up by df_uses_record.
   Any change here has to be matched in df_find_hard_reg_defs_1.  */

static void
df_def_record_1 (struct df_collection_rec *collection_rec,
                 rtx *loc, basic_block bb, struct df_insn_info *insn_info,
		 int flags)
{
  rtx dst = *loc;

  /* It is legal to have a set destination be a parallel. */
  if (GET_CODE (dst) == PARALLEL)
    {
      int i;
      for (i = XVECLEN (dst, 0) - 1; i >= 0; i--)
	{
	  rtx temp = XVECEXP (dst, 0, i);
	  gcc_assert (GET_CODE (temp) == EXPR_LIST);
	  df_def_record_1 (collection_rec, &XEXP (temp, 0),
			   bb, insn_info, flags);
	}
      return;
    }

  if (GET_CODE (dst) == STRICT_LOW_PART)
    {
      flags |= DF_REF_READ_WRITE | DF_REF_PARTIAL | DF_REF_STRICT_LOW_PART;

      loc = &XEXP (dst, 0);
      dst = *loc;
    }

  if (GET_CODE (dst) == ZERO_EXTRACT)
    {
      flags |= DF_REF_READ_WRITE | DF_REF_PARTIAL | DF_REF_ZERO_EXTRACT;

      loc = &XEXP (dst, 0);
      dst = *loc;
    }

  /* At this point if we do not have a reg or a subreg, just return.  */
  if (REG_P (dst))
    {
      df_ref_record (DF_REF_REGULAR, collection_rec,
		     dst, loc, bb, insn_info, DF_REF_REG_DEF, flags);

      /* We want to keep sp alive everywhere - by making all
	 writes to sp also use of sp. */
      if (REGNO (dst) == STACK_POINTER_REGNUM)
	df_ref_record (DF_REF_BASE, collection_rec,
		       dst, NULL, bb, insn_info, DF_REF_REG_USE, flags);
    }
  else if (GET_CODE (dst) == SUBREG && REG_P (SUBREG_REG (dst)))
    {
      if (df_read_modify_subreg_p (dst))
	flags |= DF_REF_READ_WRITE | DF_REF_PARTIAL;

      flags |= DF_REF_SUBREG;

      df_ref_record (DF_REF_REGULAR, collection_rec,
		     dst, loc, bb, insn_info, DF_REF_REG_DEF, flags);
    }
}


/* Process all the registers defined in the pattern rtx, X.  Any change
   here has to be matched in df_find_hard_reg_defs.  */

static void
df_defs_record (struct df_collection_rec *collection_rec,
                rtx x, basic_block bb, struct df_insn_info *insn_info,
		int flags)
{
  RTX_CODE code = GET_CODE (x);
  int i;

  switch (code)
    {
    case SET:
      df_def_record_1 (collection_rec, &SET_DEST (x), bb, insn_info, flags);
      break;

    case CLOBBER:
      flags |= DF_REF_MUST_CLOBBER;
      df_def_record_1 (collection_rec, &XEXP (x, 0), bb, insn_info, flags);
      break;

    case COND_EXEC:
      df_defs_record (collection_rec, COND_EXEC_CODE (x),
		      bb, insn_info, DF_REF_CONDITIONAL);
      break;

    case PARALLEL:
      for (i = 0; i < XVECLEN (x, 0); i++)
	df_defs_record (collection_rec, XVECEXP (x, 0, i),
			bb, insn_info, flags);
      break;
    default:
      /* No DEFs to record in other cases */
      break;
    }
}

/* Set bits in *DEFS for hard registers found in the rtx DST, which is the
   destination of a set or clobber.  This has to match the logic in
   df_defs_record_1.  */

static void
df_find_hard_reg_defs_1 (rtx dst, HARD_REG_SET *defs)
{
  /* It is legal to have a set destination be a parallel. */
  if (GET_CODE (dst) == PARALLEL)
    {
      int i;
      for (i = XVECLEN (dst, 0) - 1; i >= 0; i--)
	{
	  rtx temp = XVECEXP (dst, 0, i);
	  gcc_assert (GET_CODE (temp) == EXPR_LIST);
	  df_find_hard_reg_defs_1 (XEXP (temp, 0), defs);
	}
      return;
    }

  if (GET_CODE (dst) == STRICT_LOW_PART)
      dst = XEXP (dst, 0);

  if (GET_CODE (dst) == ZERO_EXTRACT)
      dst = XEXP (dst, 0);

  /* At this point if we do not have a reg or a subreg, just return.  */
  if (REG_P (dst) && HARD_REGISTER_P (dst))
    SET_HARD_REG_BIT (*defs, REGNO (dst));
  else if (GET_CODE (dst) == SUBREG
	   && REG_P (SUBREG_REG (dst)) && HARD_REGISTER_P (dst))
    SET_HARD_REG_BIT (*defs, REGNO (SUBREG_REG (dst)));
}

/* Set bits in *DEFS for hard registers defined in the pattern X.  This
   has to match the logic in df_defs_record.  */

static void
df_find_hard_reg_defs (rtx x, HARD_REG_SET *defs)
{
  RTX_CODE code = GET_CODE (x);
  int i;

  switch (code)
    {
    case SET:
      df_find_hard_reg_defs_1 (SET_DEST (x), defs);
      break;

    case CLOBBER:
      df_find_hard_reg_defs_1 (XEXP (x, 0), defs);
      break;

    case COND_EXEC:
      df_find_hard_reg_defs (COND_EXEC_CODE (x), defs);
      break;

    case PARALLEL:
      for (i = 0; i < XVECLEN (x, 0); i++)
	df_find_hard_reg_defs (XVECEXP (x, 0, i), defs);
      break;
    default:
      /* No DEFs to record in other cases */
      break;
    }
}


/* Process all the registers used in the rtx at address LOC.  */

static void
df_uses_record (struct df_collection_rec *collection_rec,
                rtx *loc, enum df_ref_type ref_type,
		basic_block bb, struct df_insn_info *insn_info,
		int flags)
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
    case CONST:
    CASE_CONST_ANY:
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case CLOBBER:
      /* If we are clobbering a MEM, mark any registers inside the address
	 as being used.  */
      if (MEM_P (XEXP (x, 0)))
	df_uses_record (collection_rec,
			&XEXP (XEXP (x, 0), 0),
			DF_REF_REG_MEM_STORE,
		        bb, insn_info,
			flags);

      /* If we're clobbering a REG then we have a def so ignore.  */
      return;

    case MEM:
      df_uses_record (collection_rec,
		      &XEXP (x, 0), DF_REF_REG_MEM_LOAD,
		      bb, insn_info, flags & DF_REF_IN_NOTE);
      return;

    case SUBREG:
      /* While we're here, optimize this case.  */
      flags |= DF_REF_PARTIAL;
      /* In case the SUBREG is not of a REG, do not optimize.  */
      if (!REG_P (SUBREG_REG (x)))
	{
	  loc = &SUBREG_REG (x);
	  df_uses_record (collection_rec, loc, ref_type, bb, insn_info, flags);
	  return;
	}
      /* ... Fall through ...  */

    case REG:
      df_ref_record (DF_REF_REGULAR, collection_rec,
		     x, loc, bb, insn_info,
		     ref_type, flags);
      return;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      {
        df_uses_record (collection_rec,
                        &XEXP (x, 1), ref_type, bb, insn_info, flags);
        df_uses_record (collection_rec,
                        &XEXP (x, 2), ref_type, bb, insn_info, flags);

        /* If the parameters to the zero or sign extract are
           constants, strip them off and recurse, otherwise there is
           no information that we can gain from this operation.  */
        if (code == ZERO_EXTRACT)
          flags |= DF_REF_ZERO_EXTRACT;
        else
          flags |= DF_REF_SIGN_EXTRACT;

        df_uses_record (collection_rec,
                        &XEXP (x, 0), ref_type, bb, insn_info, flags);
        return;
      }
      break;

    case SET:
      {
	rtx dst = SET_DEST (x);
	gcc_assert (!(flags & DF_REF_IN_NOTE));
	df_uses_record (collection_rec,
			&SET_SRC (x), DF_REF_REG_USE, bb, insn_info, flags);

	switch (GET_CODE (dst))
	  {
	    case SUBREG:
	      if (df_read_modify_subreg_p (dst))
		{
		  df_uses_record (collection_rec, &SUBREG_REG (dst),
				  DF_REF_REG_USE, bb, insn_info,
				  flags | DF_REF_READ_WRITE | DF_REF_SUBREG);
		  break;
		}
	      /* Fall through.  */
	    case REG:
	    case PARALLEL:
	    case SCRATCH:
	    case PC:
	    case CC0:
		break;
	    case MEM:
	      df_uses_record (collection_rec, &XEXP (dst, 0),
			      DF_REF_REG_MEM_STORE, bb, insn_info, flags);
	      break;
	    case STRICT_LOW_PART:
	      {
		rtx *temp = &XEXP (dst, 0);
		/* A strict_low_part uses the whole REG and not just the
		 SUBREG.  */
		dst = XEXP (dst, 0);
		df_uses_record (collection_rec,
				(GET_CODE (dst) == SUBREG) ? &SUBREG_REG (dst) : temp,
				DF_REF_REG_USE, bb, insn_info,
				DF_REF_READ_WRITE | DF_REF_STRICT_LOW_PART);
	      }
	      break;
	    case ZERO_EXTRACT:
	      {
		df_uses_record (collection_rec, &XEXP (dst, 1),
				DF_REF_REG_USE, bb, insn_info, flags);
		df_uses_record (collection_rec, &XEXP (dst, 2),
				DF_REF_REG_USE, bb, insn_info, flags);
                if (GET_CODE (XEXP (dst,0)) == MEM)
                  df_uses_record (collection_rec, &XEXP (dst, 0),
                                  DF_REF_REG_USE, bb, insn_info,
                                  flags);
                else
                  df_uses_record (collection_rec, &XEXP (dst, 0),
                                  DF_REF_REG_USE, bb, insn_info,
                                  DF_REF_READ_WRITE | DF_REF_ZERO_EXTRACT);
	      }
	      break;

	    default:
	      gcc_unreachable ();
	  }
	return;
      }

    case RETURN:
    case SIMPLE_RETURN:
      break;

    case ASM_OPERANDS:
    case UNSPEC_VOLATILE:
    case TRAP_IF:
    case ASM_INPUT:
      {
	/* Traditional and volatile asm instructions must be
	   considered to use and clobber all hard registers, all
	   pseudo-registers and all of memory.  So must TRAP_IF and
	   UNSPEC_VOLATILE operations.

	   Consider for instance a volatile asm that changes the fpu
	   rounding mode.  An insn should not be moved across this
	   even if it only uses pseudo-regs because it might give an
	   incorrectly rounded result.

	   However, flow.c's liveness computation did *not* do this,
	   giving the reasoning as " ?!? Unfortunately, marking all
	   hard registers as live causes massive problems for the
	   register allocator and marking all pseudos as live creates
	   mountains of uninitialized variable warnings."

	   In order to maintain the status quo with regard to liveness
	   and uses, we do what flow.c did and just mark any regs we
	   can find in ASM_OPERANDS as used.  In global asm insns are
	   scanned and regs_asm_clobbered is filled out.

	   For all ASM_OPERANDS, we must traverse the vector of input
	   operands.  We can not just fall through here since then we
	   would be confused by the ASM_INPUT rtx inside ASM_OPERANDS,
	   which do not indicate traditional asms unlike their normal
	   usage.  */
	if (code == ASM_OPERANDS)
	  {
	    int j;

	    for (j = 0; j < ASM_OPERANDS_INPUT_LENGTH (x); j++)
	      df_uses_record (collection_rec, &ASM_OPERANDS_INPUT (x, j),
			      DF_REF_REG_USE, bb, insn_info, flags);
	    return;
	  }
	break;
      }

    case VAR_LOCATION:
      df_uses_record (collection_rec,
		      &PAT_VAR_LOCATION_LOC (x),
		      DF_REF_REG_USE, bb, insn_info, flags);
      return;

    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
    case PRE_MODIFY:
    case POST_MODIFY:
      gcc_assert (!DEBUG_INSN_P (insn_info->insn));
      /* Catch the def of the register being modified.  */
      df_ref_record (DF_REF_REGULAR, collection_rec, XEXP (x, 0), &XEXP (x, 0),
		     bb, insn_info,
		     DF_REF_REG_DEF,
                     flags | DF_REF_READ_WRITE | DF_REF_PRE_POST_MODIFY);

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
	    df_uses_record (collection_rec, &XEXP (x, i), ref_type,
			    bb, insn_info, flags);
	  }
	else if (fmt[i] == 'E')
	  {
	    int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      df_uses_record (collection_rec,
			      &XVECEXP (x, i, j), ref_type,
			      bb, insn_info, flags);
	  }
      }
  }

  return;
}


/* For all DF_REF_CONDITIONAL defs, add a corresponding uses.  */

static void
df_get_conditional_uses (struct df_collection_rec *collection_rec)
{
  unsigned int ix;
  df_ref ref;

  FOR_EACH_VEC_ELT (collection_rec->def_vec, ix, ref)
    {
      if (DF_REF_FLAGS_IS_SET (ref, DF_REF_CONDITIONAL))
        {
          df_ref use;

          use = df_ref_create_structure (DF_REF_CLASS (ref), collection_rec, DF_REF_REG (ref),
					 DF_REF_LOC (ref), DF_REF_BB (ref),
					 DF_REF_INSN_INFO (ref), DF_REF_REG_USE,
					 DF_REF_FLAGS (ref) & ~DF_REF_CONDITIONAL);
          DF_REF_REGNO (use) = DF_REF_REGNO (ref);
        }
    }
}


/* Get call's extra defs and uses (track caller-saved registers). */

static void
df_get_call_refs (struct df_collection_rec *collection_rec,
                  basic_block bb,
                  struct df_insn_info *insn_info,
                  int flags)
{
  rtx note;
  bool is_sibling_call;
  unsigned int i;
  HARD_REG_SET defs_generated;

  CLEAR_HARD_REG_SET (defs_generated);
  df_find_hard_reg_defs (PATTERN (insn_info->insn), &defs_generated);
  is_sibling_call = SIBLING_CALL_P (insn_info->insn);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i == STACK_POINTER_REGNUM)
	/* The stack ptr is used (honorarily) by a CALL insn.  */
	df_ref_record (DF_REF_BASE, collection_rec, regno_reg_rtx[i],
		       NULL, bb, insn_info, DF_REF_REG_USE,
		       DF_REF_CALL_STACK_USAGE | flags);
      else if (global_regs[i])
	{
	  /* Calls to const functions cannot access any global registers and
	     calls to pure functions cannot set them.  All other calls may
	     reference any of the global registers, so they are recorded as
	     used. */
	  if (!RTL_CONST_CALL_P (insn_info->insn))
	    {
	      df_ref_record (DF_REF_BASE, collection_rec, regno_reg_rtx[i],
			     NULL, bb, insn_info, DF_REF_REG_USE, flags);
	      if (!RTL_PURE_CALL_P (insn_info->insn))
		df_ref_record (DF_REF_BASE, collection_rec, regno_reg_rtx[i],
			       NULL, bb, insn_info, DF_REF_REG_DEF, flags);
	    }
	}
      else if (TEST_HARD_REG_BIT (regs_invalidated_by_call, i)
	       /* no clobbers for regs that are the result of the call */
	       && !TEST_HARD_REG_BIT (defs_generated, i)
	       && (!is_sibling_call
		   || !bitmap_bit_p (df->exit_block_uses, i)
		   || refers_to_regno_p (i, i+1,
				         crtl->return_rtx, NULL)))
	  df_ref_record (DF_REF_BASE, collection_rec, regno_reg_rtx[i],
			 NULL, bb, insn_info, DF_REF_REG_DEF,
			 DF_REF_MAY_CLOBBER | flags);
    }

  /* Record the registers used to pass arguments, and explicitly
     noted as clobbered.  */
  for (note = CALL_INSN_FUNCTION_USAGE (insn_info->insn); note;
       note = XEXP (note, 1))
    {
      if (GET_CODE (XEXP (note, 0)) == USE)
        df_uses_record (collection_rec, &XEXP (XEXP (note, 0), 0),
			DF_REF_REG_USE, bb, insn_info, flags);
      else if (GET_CODE (XEXP (note, 0)) == CLOBBER)
	{
	  if (REG_P (XEXP (XEXP (note, 0), 0)))
	    {
	      unsigned int regno = REGNO (XEXP (XEXP (note, 0), 0));
	      if (!TEST_HARD_REG_BIT (defs_generated, regno))
		df_defs_record (collection_rec, XEXP (note, 0), bb,
				insn_info, flags);
	    }
	  else
	    df_uses_record (collection_rec, &XEXP (note, 0),
		            DF_REF_REG_USE, bb, insn_info, flags);
	}
    }

  return;
}

/* Collect all refs in the INSN. This function is free of any
   side-effect - it will create and return a lists of df_ref's in the
   COLLECTION_REC without putting those refs into existing ref chains
   and reg chains. */

static void
df_insn_refs_collect (struct df_collection_rec *collection_rec,
		      basic_block bb, struct df_insn_info *insn_info)
{
  rtx note;
  bool is_cond_exec = (GET_CODE (PATTERN (insn_info->insn)) == COND_EXEC);

  /* Clear out the collection record.  */
  collection_rec->def_vec.truncate (0);
  collection_rec->use_vec.truncate (0);
  collection_rec->eq_use_vec.truncate (0);
  collection_rec->mw_vec.truncate (0);

  /* Process REG_EQUIV/REG_EQUAL notes.  */
  for (note = REG_NOTES (insn_info->insn); note;
       note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
        {
        case REG_EQUIV:
        case REG_EQUAL:
          df_uses_record (collection_rec,
                          &XEXP (note, 0), DF_REF_REG_USE,
                          bb, insn_info, DF_REF_IN_NOTE);
          break;
        case REG_NON_LOCAL_GOTO:
          /* The frame ptr is used by a non-local goto.  */
          df_ref_record (DF_REF_BASE, collection_rec,
                         regno_reg_rtx[FRAME_POINTER_REGNUM],
                         NULL, bb, insn_info,
                         DF_REF_REG_USE, 0);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
          df_ref_record (DF_REF_BASE, collection_rec,
                         regno_reg_rtx[HARD_FRAME_POINTER_REGNUM],
                         NULL, bb, insn_info,
                         DF_REF_REG_USE, 0);
#endif
          break;
        default:
          break;
        }
    }

  /* For CALL_INSNs, first record DF_REF_BASE register defs, as well as
     uses from CALL_INSN_FUNCTION_USAGE. */
  if (CALL_P (insn_info->insn))
    df_get_call_refs (collection_rec, bb, insn_info,
		      (is_cond_exec) ? DF_REF_CONDITIONAL : 0);

  /* Record other defs.  These should be mostly for DF_REF_REGULAR, so
     that a qsort on the defs is unnecessary in most cases.  */
  df_defs_record (collection_rec,
		  PATTERN (insn_info->insn), bb, insn_info, 0);

  /* Record the register uses.  */
  df_uses_record (collection_rec,
		  &PATTERN (insn_info->insn), DF_REF_REG_USE, bb, insn_info, 0);

  /* DF_REF_CONDITIONAL needs corresponding USES. */
  if (is_cond_exec)
    df_get_conditional_uses (collection_rec);

  df_canonize_collection_rec (collection_rec);
}

/* Recompute the luids for the insns in BB.  */

void
df_recompute_luids (basic_block bb)
{
  rtx insn;
  int luid = 0;

  df_grow_insn_info ();

  /* Scan the block an insn at a time from beginning to end.  */
  FOR_BB_INSNS (bb, insn)
    {
      struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
      /* Inserting labels does not always trigger the incremental
	 rescanning.  */
      if (!insn_info)
	{
	  gcc_assert (!INSN_P (insn));
	  insn_info = df_insn_create_insn_record (insn);
	}

      DF_INSN_INFO_LUID (insn_info) = luid;
      if (INSN_P (insn))
	luid++;
    }
}


/* Collect all artificial refs at the block level for BB and add them
   to COLLECTION_REC.  */

static void
df_bb_refs_collect (struct df_collection_rec *collection_rec, basic_block bb)
{
  collection_rec->def_vec.truncate (0);
  collection_rec->use_vec.truncate (0);
  collection_rec->eq_use_vec.truncate (0);
  collection_rec->mw_vec.truncate (0);

  if (bb->index == ENTRY_BLOCK)
    {
      df_entry_block_defs_collect (collection_rec, df->entry_block_defs);
      return;
    }
  else if (bb->index == EXIT_BLOCK)
    {
      df_exit_block_uses_collect (collection_rec, df->exit_block_uses);
      return;
    }

#ifdef EH_RETURN_DATA_REGNO
  if (bb_has_eh_pred (bb))
    {
      unsigned int i;
      /* Mark the registers that will contain data for the handler.  */
      for (i = 0; ; ++i)
	{
	  unsigned regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;
	  df_ref_record (DF_REF_ARTIFICIAL, collection_rec, regno_reg_rtx[regno], NULL,
			 bb, NULL, DF_REF_REG_DEF, DF_REF_AT_TOP);
	}
    }
#endif

  /* Add the hard_frame_pointer if this block is the target of a
     non-local goto.  */
  if (bb->flags & BB_NON_LOCAL_GOTO_TARGET)
    df_ref_record (DF_REF_ARTIFICIAL, collection_rec, hard_frame_pointer_rtx, NULL,
		   bb, NULL, DF_REF_REG_DEF, DF_REF_AT_TOP);

  /* Add the artificial uses.  */
  if (bb->index >= NUM_FIXED_BLOCKS)
    {
      bitmap_iterator bi;
      unsigned int regno;
      bitmap au = bb_has_eh_pred (bb)
	? &df->eh_block_artificial_uses
	: &df->regular_block_artificial_uses;

      EXECUTE_IF_SET_IN_BITMAP (au, 0, regno, bi)
	{
	  df_ref_record (DF_REF_ARTIFICIAL, collection_rec, regno_reg_rtx[regno], NULL,
			 bb, NULL, DF_REF_REG_USE, 0);
	}
    }

  df_canonize_collection_rec (collection_rec);
}


/* Record all the refs within the basic block BB_INDEX and scan the instructions if SCAN_INSNS.  */

void
df_bb_refs_record (int bb_index, bool scan_insns)
{
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bb_index);
  rtx insn;
  int luid = 0;

  if (!df)
    return;

  df_collection_rec collection_rec;
  df_grow_bb_info (df_scan);
  if (scan_insns)
    /* Scan the block an insn at a time from beginning to end.  */
    FOR_BB_INSNS (bb, insn)
      {
	struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);
	gcc_assert (!insn_info);

	insn_info = df_insn_create_insn_record (insn);
	if (INSN_P (insn))
	  {
	    /* Record refs within INSN.  */
	    DF_INSN_INFO_LUID (insn_info) = luid++;
	    df_insn_refs_collect (&collection_rec, bb, DF_INSN_INFO_GET (insn));
	    df_refs_add_to_chains (&collection_rec, bb, insn, copy_all);
	  }
	DF_INSN_INFO_LUID (insn_info) = luid;
      }

  /* Other block level artificial refs */
  df_bb_refs_collect (&collection_rec, bb);
  df_refs_add_to_chains (&collection_rec, bb, NULL, copy_all);

  /* Now that the block has been processed, set the block as dirty so
     LR and LIVE will get it processed.  */
  df_set_bb_dirty (bb);
}


/* Get the artificial use set for a regular (i.e. non-exit/non-entry)
   block. */

static void
df_get_regular_block_artificial_uses (bitmap regular_block_artificial_uses)
{
#ifdef EH_USES
  unsigned int i;
#endif

  bitmap_clear (regular_block_artificial_uses);

  if (reload_completed)
    {
      if (frame_pointer_needed)
	bitmap_set_bit (regular_block_artificial_uses, HARD_FRAME_POINTER_REGNUM);
    }
  else
    /* Before reload, there are a few registers that must be forced
       live everywhere -- which might not already be the case for
       blocks within infinite loops.  */
    {
      unsigned int picreg = PIC_OFFSET_TABLE_REGNUM;

      /* Any reference to any pseudo before reload is a potential
	 reference of the frame pointer.  */
      bitmap_set_bit (regular_block_artificial_uses, FRAME_POINTER_REGNUM);

#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
      bitmap_set_bit (regular_block_artificial_uses, HARD_FRAME_POINTER_REGNUM);
#endif

#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      /* Pseudos with argument area equivalences may require
	 reloading via the argument pointer.  */
      if (fixed_regs[ARG_POINTER_REGNUM])
	bitmap_set_bit (regular_block_artificial_uses, ARG_POINTER_REGNUM);
#endif

      /* Any constant, or pseudo with constant equivalences, may
	 require reloading from memory using the pic register.  */
      if (picreg != INVALID_REGNUM
	  && fixed_regs[picreg])
	bitmap_set_bit (regular_block_artificial_uses, picreg);
    }
  /* The all-important stack pointer must always be live.  */
  bitmap_set_bit (regular_block_artificial_uses, STACK_POINTER_REGNUM);

#ifdef EH_USES
  /* EH_USES registers are used:
     1) at all insns that might throw (calls or with -fnon-call-exceptions
	trapping insns)
     2) in all EH edges
     3) to support backtraces and/or debugging, anywhere between their
	initialization and where they the saved registers are restored
	from them, including the cases where we don't reach the epilogue
	(noreturn call or infinite loop).  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (EH_USES (i))
      bitmap_set_bit (regular_block_artificial_uses, i);
#endif
}


/* Get the artificial use set for an eh block. */

static void
df_get_eh_block_artificial_uses (bitmap eh_block_artificial_uses)
{
  bitmap_clear (eh_block_artificial_uses);

  /* The following code (down through the arg_pointer setting APPEARS
     to be necessary because there is nothing that actually
     describes what the exception handling code may actually need
     to keep alive.  */
  if (reload_completed)
    {
      if (frame_pointer_needed)
	{
	  bitmap_set_bit (eh_block_artificial_uses, FRAME_POINTER_REGNUM);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
	  bitmap_set_bit (eh_block_artificial_uses, HARD_FRAME_POINTER_REGNUM);
#endif
	}
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      if (fixed_regs[ARG_POINTER_REGNUM])
	bitmap_set_bit (eh_block_artificial_uses, ARG_POINTER_REGNUM);
#endif
    }
}



/*----------------------------------------------------------------------------
   Specialized hard register scanning functions.
----------------------------------------------------------------------------*/


/* Mark a register in SET.  Hard registers in large modes get all
   of their component registers set as well.  */

static void
df_mark_reg (rtx reg, void *vset)
{
  bitmap set = (bitmap) vset;
  int regno = REGNO (reg);

  gcc_assert (GET_MODE (reg) != BLKmode);

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int n = hard_regno_nregs[regno][GET_MODE (reg)];
      bitmap_set_range (set, regno, n);
    }
  else
    bitmap_set_bit (set, regno);
}


/* Set the bit for regs that are considered being defined at the entry. */

static void
df_get_entry_block_def_set (bitmap entry_block_defs)
{
  rtx r;
  int i;

  bitmap_clear (entry_block_defs);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (global_regs[i])
	bitmap_set_bit (entry_block_defs, i);
      if (FUNCTION_ARG_REGNO_P (i))
	bitmap_set_bit (entry_block_defs, INCOMING_REGNO (i));
    }

  /* The always important stack pointer.  */
  bitmap_set_bit (entry_block_defs, STACK_POINTER_REGNUM);

  /* Once the prologue has been generated, all of these registers
     should just show up in the first regular block.  */
  if (HAVE_prologue && epilogue_completed)
    {
      /* Defs for the callee saved registers are inserted so that the
	 pushes have some defining location.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if ((call_used_regs[i] == 0) && (df_regs_ever_live_p (i)))
	  bitmap_set_bit (entry_block_defs, i);
    }

  r = targetm.calls.struct_value_rtx (current_function_decl, true);
  if (r && REG_P (r))
    bitmap_set_bit (entry_block_defs, REGNO (r));

  /* If the function has an incoming STATIC_CHAIN, it has to show up
     in the entry def set.  */
  r = targetm.calls.static_chain (current_function_decl, true);
  if (r && REG_P (r))
    bitmap_set_bit (entry_block_defs, REGNO (r));

  if ((!reload_completed) || frame_pointer_needed)
    {
      /* Any reference to any pseudo before reload is a potential
	 reference of the frame pointer.  */
      bitmap_set_bit (entry_block_defs, FRAME_POINTER_REGNUM);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
      /* If they are different, also mark the hard frame pointer as live.  */
      if (!LOCAL_REGNO (HARD_FRAME_POINTER_REGNUM))
	bitmap_set_bit (entry_block_defs, HARD_FRAME_POINTER_REGNUM);
#endif
    }

  /* These registers are live everywhere.  */
  if (!reload_completed)
    {
#ifdef PIC_OFFSET_TABLE_REGNUM
      unsigned int picreg = PIC_OFFSET_TABLE_REGNUM;
#endif

#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      /* Pseudos with argument area equivalences may require
	 reloading via the argument pointer.  */
      if (fixed_regs[ARG_POINTER_REGNUM])
	bitmap_set_bit (entry_block_defs, ARG_POINTER_REGNUM);
#endif

#ifdef PIC_OFFSET_TABLE_REGNUM
      /* Any constant, or pseudo with constant equivalences, may
	 require reloading from memory using the pic register.  */
      if (picreg != INVALID_REGNUM
	  && fixed_regs[picreg])
	bitmap_set_bit (entry_block_defs, picreg);
#endif
    }

#ifdef INCOMING_RETURN_ADDR_RTX
  if (REG_P (INCOMING_RETURN_ADDR_RTX))
    bitmap_set_bit (entry_block_defs, REGNO (INCOMING_RETURN_ADDR_RTX));
#endif

  targetm.extra_live_on_entry (entry_block_defs);
}


/* Return the (conservative) set of hard registers that are defined on
   entry to the function.
   It uses df->entry_block_defs to determine which register
   reference to include.  */

static void
df_entry_block_defs_collect (struct df_collection_rec *collection_rec,
			     bitmap entry_block_defs)
{
  unsigned int i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (entry_block_defs, 0, i, bi)
    {
      df_ref_record (DF_REF_ARTIFICIAL, collection_rec, regno_reg_rtx[i], NULL,
		     ENTRY_BLOCK_PTR_FOR_FN (cfun), NULL, DF_REF_REG_DEF, 0);
    }

  df_canonize_collection_rec (collection_rec);
}


/* Record the (conservative) set of hard registers that are defined on
   entry to the function.  */

static void
df_record_entry_block_defs (bitmap entry_block_defs)
{
  struct df_collection_rec collection_rec;
  df_entry_block_defs_collect (&collection_rec, entry_block_defs);

  /* Process bb_refs chain */
  df_refs_add_to_chains (&collection_rec,
			 BASIC_BLOCK_FOR_FN (cfun, ENTRY_BLOCK),
			 NULL,
			 copy_defs);
}


/* Update the defs in the entry block.  */

void
df_update_entry_block_defs (void)
{
  bitmap_head refs;
  bool changed = false;

  bitmap_initialize (&refs, &df_bitmap_obstack);
  df_get_entry_block_def_set (&refs);
  if (df->entry_block_defs)
    {
      if (!bitmap_equal_p (df->entry_block_defs, &refs))
	{
	  struct df_scan_bb_info *bb_info = df_scan_get_bb_info (ENTRY_BLOCK);
	  df_ref_chain_delete_du_chain (bb_info->artificial_defs);
	  df_ref_chain_delete (bb_info->artificial_defs);
	  bb_info->artificial_defs = NULL;
	  changed = true;
	}
    }
  else
    {
      struct df_scan_problem_data *problem_data
	= (struct df_scan_problem_data *) df_scan->problem_data;
	gcc_unreachable ();
      df->entry_block_defs = BITMAP_ALLOC (&problem_data->reg_bitmaps);
      changed = true;
    }

  if (changed)
    {
      df_record_entry_block_defs (&refs);
      bitmap_copy (df->entry_block_defs, &refs);
      df_set_bb_dirty (BASIC_BLOCK_FOR_FN (cfun, ENTRY_BLOCK));
    }
  bitmap_clear (&refs);
}


/* Set the bit for regs that are considered being used at the exit. */

static void
df_get_exit_block_use_set (bitmap exit_block_uses)
{
  unsigned int i;
  unsigned int picreg = PIC_OFFSET_TABLE_REGNUM;

  bitmap_clear (exit_block_uses);

  /* Stack pointer is always live at the exit.  */
  bitmap_set_bit (exit_block_uses, STACK_POINTER_REGNUM);

  /* Mark the frame pointer if needed at the end of the function.
     If we end up eliminating it, it will be removed from the live
     list of each basic block by reload.  */

  if ((!reload_completed) || frame_pointer_needed)
    {
      bitmap_set_bit (exit_block_uses, FRAME_POINTER_REGNUM);
#if !HARD_FRAME_POINTER_IS_FRAME_POINTER
      /* If they are different, also mark the hard frame pointer as live.  */
      if (!LOCAL_REGNO (HARD_FRAME_POINTER_REGNUM))
	bitmap_set_bit (exit_block_uses, HARD_FRAME_POINTER_REGNUM);
#endif
    }

  /* Many architectures have a GP register even without flag_pic.
     Assume the pic register is not in use, or will be handled by
     other means, if it is not fixed.  */
  if (!PIC_OFFSET_TABLE_REG_CALL_CLOBBERED
      && picreg != INVALID_REGNUM
      && fixed_regs[picreg])
    bitmap_set_bit (exit_block_uses, picreg);

  /* Mark all global registers, and all registers used by the
     epilogue as being live at the end of the function since they
     may be referenced by our caller.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (global_regs[i] || EPILOGUE_USES (i))
      bitmap_set_bit (exit_block_uses, i);

  if (HAVE_epilogue && epilogue_completed)
    {
      /* Mark all call-saved registers that we actually used.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (df_regs_ever_live_p (i) && !LOCAL_REGNO (i)
	    && !TEST_HARD_REG_BIT (regs_invalidated_by_call, i))
	  bitmap_set_bit (exit_block_uses, i);
    }

#ifdef EH_RETURN_DATA_REGNO
  /* Mark the registers that will contain data for the handler.  */
  if (reload_completed && crtl->calls_eh_return)
    for (i = 0; ; ++i)
      {
	unsigned regno = EH_RETURN_DATA_REGNO (i);
	if (regno == INVALID_REGNUM)
	  break;
	bitmap_set_bit (exit_block_uses, regno);
      }
#endif

#ifdef EH_RETURN_STACKADJ_RTX
  if ((!HAVE_epilogue || ! epilogue_completed)
      && crtl->calls_eh_return)
    {
      rtx tmp = EH_RETURN_STACKADJ_RTX;
      if (tmp && REG_P (tmp))
	df_mark_reg (tmp, exit_block_uses);
    }
#endif

#ifdef EH_RETURN_HANDLER_RTX
  if ((!HAVE_epilogue || ! epilogue_completed)
      && crtl->calls_eh_return)
    {
      rtx tmp = EH_RETURN_HANDLER_RTX;
      if (tmp && REG_P (tmp))
	df_mark_reg (tmp, exit_block_uses);
    }
#endif

  /* Mark function return value.  */
  diddle_return_value (df_mark_reg, (void*) exit_block_uses);
}


/* Return the refs of hard registers that are used in the exit block.
   It uses df->exit_block_uses to determine register to include.  */

static void
df_exit_block_uses_collect (struct df_collection_rec *collection_rec, bitmap exit_block_uses)
{
  unsigned int i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (exit_block_uses, 0, i, bi)
    df_ref_record (DF_REF_ARTIFICIAL, collection_rec, regno_reg_rtx[i], NULL,
		   EXIT_BLOCK_PTR_FOR_FN (cfun), NULL, DF_REF_REG_USE, 0);

#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
  /* It is deliberate that this is not put in the exit block uses but
     I do not know why.  */
  if (reload_completed
      && !bitmap_bit_p (exit_block_uses, ARG_POINTER_REGNUM)
      && bb_has_eh_pred (EXIT_BLOCK_PTR_FOR_FN (cfun))
      && fixed_regs[ARG_POINTER_REGNUM])
    df_ref_record (DF_REF_ARTIFICIAL, collection_rec, regno_reg_rtx[ARG_POINTER_REGNUM], NULL,
		   EXIT_BLOCK_PTR_FOR_FN (cfun), NULL, DF_REF_REG_USE, 0);
#endif

  df_canonize_collection_rec (collection_rec);
}


/* Record the set of hard registers that are used in the exit block.
   It uses df->exit_block_uses to determine which bit to include.  */

static void
df_record_exit_block_uses (bitmap exit_block_uses)
{
  struct df_collection_rec collection_rec;
  df_exit_block_uses_collect (&collection_rec, exit_block_uses);

  /* Process bb_refs chain */
  df_refs_add_to_chains (&collection_rec,
			 BASIC_BLOCK_FOR_FN (cfun, EXIT_BLOCK),
			 NULL,
			 copy_uses);
}


/* Update the uses in the exit block.  */

void
df_update_exit_block_uses (void)
{
  bitmap_head refs;
  bool changed = false;

  bitmap_initialize (&refs, &df_bitmap_obstack);
  df_get_exit_block_use_set (&refs);
  if (df->exit_block_uses)
    {
      if (!bitmap_equal_p (df->exit_block_uses, &refs))
	{
	  struct df_scan_bb_info *bb_info = df_scan_get_bb_info (EXIT_BLOCK);
	  df_ref_chain_delete_du_chain (bb_info->artificial_uses);
	  df_ref_chain_delete (bb_info->artificial_uses);
	  bb_info->artificial_uses = NULL;
	  changed = true;
	}
    }
  else
    {
      struct df_scan_problem_data *problem_data
	= (struct df_scan_problem_data *) df_scan->problem_data;
	gcc_unreachable ();
      df->exit_block_uses = BITMAP_ALLOC (&problem_data->reg_bitmaps);
      changed = true;
    }

  if (changed)
    {
      df_record_exit_block_uses (&refs);
      bitmap_copy (df->exit_block_uses,& refs);
      df_set_bb_dirty (BASIC_BLOCK_FOR_FN (cfun, EXIT_BLOCK));
    }
  bitmap_clear (&refs);
}

static bool initialized = false;


/* Initialize some platform specific structures.  */

void
df_hard_reg_init (void)
{
#ifdef ELIMINABLE_REGS
  int i;
  static const struct {const int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif
  if (initialized)
    return;

  /* Record which registers will be eliminated.  We use this in
     mark_used_regs.  */
  CLEAR_HARD_REG_SET (elim_reg_set);

#ifdef ELIMINABLE_REGS
  for (i = 0; i < (int) ARRAY_SIZE (eliminables); i++)
    SET_HARD_REG_BIT (elim_reg_set, eliminables[i].from);
#else
  SET_HARD_REG_BIT (elim_reg_set, FRAME_POINTER_REGNUM);
#endif

  initialized = true;
}


/* Recompute the parts of scanning that are based on regs_ever_live
   because something changed in that array.  */

void
df_update_entry_exit_and_calls (void)
{
  basic_block bb;

  df_update_entry_block_defs ();
  df_update_exit_block_uses ();

  /* The call insns need to be rescanned because there may be changes
     in the set of registers clobbered across the call.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx insn;
      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn) && CALL_P (insn))
	    df_insn_rescan (insn);
	}
    }
}


/* Return true if hard REG is actually used in the some instruction.
   There are a fair number of conditions that affect the setting of
   this array.  See the comment in df.h for df->hard_regs_live_count
   for the conditions that this array is set. */

bool
df_hard_reg_used_p (unsigned int reg)
{
  return df->hard_regs_live_count[reg] != 0;
}


/* A count of the number of times REG is actually used in the some
   instruction.  There are a fair number of conditions that affect the
   setting of this array.  See the comment in df.h for
   df->hard_regs_live_count for the conditions that this array is
   set. */


unsigned int
df_hard_reg_used_count (unsigned int reg)
{
  return df->hard_regs_live_count[reg];
}


/* Get the value of regs_ever_live[REGNO].  */

bool
df_regs_ever_live_p (unsigned int regno)
{
  return regs_ever_live[regno];
}


/* Set regs_ever_live[REGNO] to VALUE.  If this cause regs_ever_live
   to change, schedule that change for the next update.  */

void
df_set_regs_ever_live (unsigned int regno, bool value)
{
  if (regs_ever_live[regno] == value)
    return;

  regs_ever_live[regno] = value;
  if (df)
    df->redo_entry_and_exit = true;
}


/* Compute "regs_ever_live" information from the underlying df
   information.  Set the vector to all false if RESET.  */

void
df_compute_regs_ever_live (bool reset)
{
  unsigned int i;
  bool changed = df->redo_entry_and_exit;

  if (reset)
    memset (regs_ever_live, 0, sizeof (regs_ever_live));

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if ((!regs_ever_live[i]) && df_hard_reg_used_p (i))
      {
	regs_ever_live[i] = true;
	changed = true;
      }
  if (changed)
    df_update_entry_exit_and_calls ();
  df->redo_entry_and_exit = false;
}


/*----------------------------------------------------------------------------
  Dataflow ref information verification functions.

  df_reg_chain_mark (refs, regno, is_def, is_eq_use)
  df_reg_chain_verify_unmarked (refs)
  df_refs_verify (vec<stack, va_df_ref>, ref*, bool)
  df_mws_verify (mw*, mw*, bool)
  df_insn_refs_verify (collection_rec, bb, insn, bool)
  df_bb_refs_verify (bb, refs, bool)
  df_bb_verify (bb)
  df_exit_block_bitmap_verify (bool)
  df_entry_block_bitmap_verify (bool)
  df_scan_verify ()
----------------------------------------------------------------------------*/


/* Mark all refs in the reg chain.  Verify that all of the registers
are in the correct chain.  */

static unsigned int
df_reg_chain_mark (df_ref refs, unsigned int regno,
		   bool is_def, bool is_eq_use)
{
  unsigned int count = 0;
  df_ref ref;
  for (ref = refs; ref; ref = DF_REF_NEXT_REG (ref))
    {
      gcc_assert (!DF_REF_IS_REG_MARKED (ref));

      /* If there are no def-use or use-def chains, make sure that all
	 of the chains are clear.  */
      if (!df_chain)
	gcc_assert (!DF_REF_CHAIN (ref));

      /* Check to make sure the ref is in the correct chain.  */
      gcc_assert (DF_REF_REGNO (ref) == regno);
      if (is_def)
	gcc_assert (DF_REF_REG_DEF_P (ref));
      else
	gcc_assert (!DF_REF_REG_DEF_P (ref));

      if (is_eq_use)
	gcc_assert ((DF_REF_FLAGS (ref) & DF_REF_IN_NOTE));
      else
	gcc_assert ((DF_REF_FLAGS (ref) & DF_REF_IN_NOTE) == 0);

      if (DF_REF_NEXT_REG (ref))
	gcc_assert (DF_REF_PREV_REG (DF_REF_NEXT_REG (ref)) == ref);
      count++;
      DF_REF_REG_MARK (ref);
    }
  return count;
}


/* Verify that all of the registers in the chain are unmarked.  */

static void
df_reg_chain_verify_unmarked (df_ref refs)
{
  df_ref ref;
  for (ref = refs; ref; ref = DF_REF_NEXT_REG (ref))
    gcc_assert (!DF_REF_IS_REG_MARKED (ref));
}


/* Verify that NEW_REC and OLD_REC have exactly the same members. */

static bool
df_refs_verify (const vec<df_ref, va_heap> *new_rec, df_ref *old_rec,
		bool abort_if_fail)
{
  unsigned int ix;
  df_ref new_ref;

  FOR_EACH_VEC_ELT (*new_rec, ix, new_ref)
    {
      if (*old_rec == NULL || !df_ref_equal_p (new_ref, *old_rec))
	{
	  if (abort_if_fail)
	    gcc_assert (0);
	  else
	    return false;
	}

      /* Abort if fail is called from the function level verifier.  If
	 that is the context, mark this reg as being seem.  */
      if (abort_if_fail)
	{
	  gcc_assert (DF_REF_IS_REG_MARKED (*old_rec));
	  DF_REF_REG_UNMARK (*old_rec);
	}

      old_rec++;
    }

  if (abort_if_fail)
    gcc_assert (*old_rec == NULL);
  else
    return *old_rec == NULL;
  return false;
}


/* Verify that NEW_REC and OLD_REC have exactly the same members. */

static bool
df_mws_verify (const vec<df_mw_hardreg_ptr, va_heap> *new_rec,
	       struct df_mw_hardreg **old_rec,
	       bool abort_if_fail)
{
  unsigned int ix;
  struct df_mw_hardreg *new_reg;

  FOR_EACH_VEC_ELT (*new_rec, ix, new_reg)
    {
      if (*old_rec == NULL || !df_mw_equal_p (new_reg, *old_rec))
	{
	  if (abort_if_fail)
	    gcc_assert (0);
	  else
	    return false;
	}
      old_rec++;
    }

  if (abort_if_fail)
    gcc_assert (*old_rec == NULL);
  else
    return *old_rec == NULL;
  return false;
}


/* Return true if the existing insn refs information is complete and
   correct. Otherwise (i.e. if there's any missing or extra refs),
   return the correct df_ref chain in REFS_RETURN.

   If ABORT_IF_FAIL, leave the refs that are verified (already in the
   ref chain) as DF_REF_MARKED(). If it's false, then it's a per-insn
   verification mode instead of the whole function, so unmark
   everything.

   If ABORT_IF_FAIL is set, this function never returns false.  */

static bool
df_insn_refs_verify (struct df_collection_rec *collection_rec,
		     basic_block bb,
                     rtx insn,
		     bool abort_if_fail)
{
  bool ret1, ret2, ret3, ret4;
  unsigned int uid = INSN_UID (insn);
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

  df_insn_refs_collect (collection_rec, bb, insn_info);

  if (!DF_INSN_UID_DEFS (uid))
    {
      /* The insn_rec was created but it was never filled out.  */
      if (abort_if_fail)
	gcc_assert (0);
      else
	return false;
    }

  /* Unfortunately we cannot opt out early if one of these is not
     right because the marks will not get cleared.  */
  ret1 = df_refs_verify (&collection_rec->def_vec, DF_INSN_UID_DEFS (uid),
			 abort_if_fail);
  ret2 = df_refs_verify (&collection_rec->use_vec, DF_INSN_UID_USES (uid),
			 abort_if_fail);
  ret3 = df_refs_verify (&collection_rec->eq_use_vec, DF_INSN_UID_EQ_USES (uid),
			 abort_if_fail);
  ret4 = df_mws_verify (&collection_rec->mw_vec, DF_INSN_UID_MWS (uid),
		       abort_if_fail);
  return (ret1 && ret2 && ret3 && ret4);
}


/* Return true if all refs in the basic block are correct and complete.
   Due to df_ref_chain_verify, it will cause all refs
   that are verified to have DF_REF_MARK bit set.  */

static bool
df_bb_verify (basic_block bb)
{
  rtx insn;
  struct df_scan_bb_info *bb_info = df_scan_get_bb_info (bb->index);
  struct df_collection_rec collection_rec;

  gcc_assert (bb_info);

  /* Scan the block, one insn at a time, from beginning to end.  */
  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      if (!INSN_P (insn))
        continue;
      df_insn_refs_verify (&collection_rec, bb, insn, true);
      df_free_collection_rec (&collection_rec);
    }

  /* Do the artificial defs and uses.  */
  df_bb_refs_collect (&collection_rec, bb);
  df_refs_verify (&collection_rec.def_vec, df_get_artificial_defs (bb->index), true);
  df_refs_verify (&collection_rec.use_vec, df_get_artificial_uses (bb->index), true);
  df_free_collection_rec (&collection_rec);

  return true;
}


/* Returns true if the entry block has correct and complete df_ref set.
   If not it either aborts if ABORT_IF_FAIL is true or returns false.  */

static bool
df_entry_block_bitmap_verify (bool abort_if_fail)
{
  bitmap_head entry_block_defs;
  bool is_eq;

  bitmap_initialize (&entry_block_defs, &df_bitmap_obstack);
  df_get_entry_block_def_set (&entry_block_defs);

  is_eq = bitmap_equal_p (&entry_block_defs, df->entry_block_defs);

  if (!is_eq && abort_if_fail)
    {
      fprintf (stderr, "entry_block_defs = ");
      df_print_regset (stderr, &entry_block_defs);
      fprintf (stderr, "df->entry_block_defs = ");
      df_print_regset (stderr, df->entry_block_defs);
      gcc_assert (0);
    }

  bitmap_clear (&entry_block_defs);

  return is_eq;
}


/* Returns true if the exit block has correct and complete df_ref set.
   If not it either aborts if ABORT_IF_FAIL is true or returns false. */

static bool
df_exit_block_bitmap_verify (bool abort_if_fail)
{
  bitmap_head exit_block_uses;
  bool is_eq;

  bitmap_initialize (&exit_block_uses, &df_bitmap_obstack);
  df_get_exit_block_use_set (&exit_block_uses);

  is_eq = bitmap_equal_p (&exit_block_uses, df->exit_block_uses);

  if (!is_eq && abort_if_fail)
    {
      fprintf (stderr, "exit_block_uses = ");
      df_print_regset (stderr, &exit_block_uses);
      fprintf (stderr, "df->exit_block_uses = ");
      df_print_regset (stderr, df->exit_block_uses);
      gcc_assert (0);
    }

  bitmap_clear (&exit_block_uses);

  return is_eq;
}


/* Return true if df_ref information for all insns in all blocks are
   correct and complete.  */

void
df_scan_verify (void)
{
  unsigned int i;
  basic_block bb;
  bitmap_head regular_block_artificial_uses;
  bitmap_head eh_block_artificial_uses;

  if (!df)
    return;

  /* Verification is a 4 step process. */

  /* (1) All of the refs are marked by going through the reg chains.  */
  for (i = 0; i < DF_REG_SIZE (df); i++)
    {
      gcc_assert (df_reg_chain_mark (DF_REG_DEF_CHAIN (i), i, true, false)
		  == DF_REG_DEF_COUNT (i));
      gcc_assert (df_reg_chain_mark (DF_REG_USE_CHAIN (i), i, false, false)
		  == DF_REG_USE_COUNT (i));
      gcc_assert (df_reg_chain_mark (DF_REG_EQ_USE_CHAIN (i), i, false, true)
		  == DF_REG_EQ_USE_COUNT (i));
    }

  /* (2) There are various bitmaps whose value may change over the
     course of the compilation.  This step recomputes them to make
     sure that they have not slipped out of date.  */
  bitmap_initialize (&regular_block_artificial_uses, &df_bitmap_obstack);
  bitmap_initialize (&eh_block_artificial_uses, &df_bitmap_obstack);

  df_get_regular_block_artificial_uses (&regular_block_artificial_uses);
  df_get_eh_block_artificial_uses (&eh_block_artificial_uses);

  bitmap_ior_into (&eh_block_artificial_uses,
		   &regular_block_artificial_uses);

  /* Check artificial_uses bitmaps didn't change. */
  gcc_assert (bitmap_equal_p (&regular_block_artificial_uses,
			      &df->regular_block_artificial_uses));
  gcc_assert (bitmap_equal_p (&eh_block_artificial_uses,
			      &df->eh_block_artificial_uses));

  bitmap_clear (&regular_block_artificial_uses);
  bitmap_clear (&eh_block_artificial_uses);

  /* Verify entry block and exit block. These only verify the bitmaps,
     the refs are verified in df_bb_verify.  */
  df_entry_block_bitmap_verify (true);
  df_exit_block_bitmap_verify (true);

  /* (3) All of the insns in all of the blocks are traversed and the
     marks are cleared both in the artificial refs attached to the
     blocks and the real refs inside the insns.  It is a failure to
     clear a mark that has not been set as this means that the ref in
     the block or insn was not in the reg chain.  */

  FOR_ALL_BB_FN (bb, cfun)
    df_bb_verify (bb);

  /* (4) See if all reg chains are traversed a second time.  This time
     a check is made that the marks are clear. A set mark would be a
     from a reg that is not in any insn or basic block.  */

  for (i = 0; i < DF_REG_SIZE (df); i++)
    {
      df_reg_chain_verify_unmarked (DF_REG_DEF_CHAIN (i));
      df_reg_chain_verify_unmarked (DF_REG_USE_CHAIN (i));
      df_reg_chain_verify_unmarked (DF_REG_EQ_USE_CHAIN (i));
    }
}
