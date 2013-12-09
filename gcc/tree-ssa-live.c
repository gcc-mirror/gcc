/* Liveness for SSA trees.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hash-table.h"
#include "tm.h"
#include "tree.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "sbitmap.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "expr.h"
#include "tree-dfa.h"
#include "timevar.h"
#include "dumpfile.h"
#include "tree-ssa-live.h"
#include "diagnostic-core.h"
#include "debug.h"
#include "flags.h"

#ifdef ENABLE_CHECKING
static void  verify_live_on_entry (tree_live_info_p);
#endif


/* VARMAP maintains a mapping from SSA version number to real variables.

   All SSA_NAMES are divided into partitions.  Initially each ssa_name is the
   only member of it's own partition.  Coalescing will attempt to group any
   ssa_names which occur in a copy or in a PHI node into the same partition.

   At the end of out-of-ssa, each partition becomes a "real" variable and is
   rewritten as a compiler variable.

   The var_map data structure is used to manage these partitions.  It allows
   partitions to be combined, and determines which partition belongs to what
   ssa_name or variable, and vice versa.  */


/* Hashtable helpers.  */

struct tree_int_map_hasher : typed_noop_remove <tree_int_map>
{
  typedef tree_int_map value_type;
  typedef tree_int_map compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

inline hashval_t
tree_int_map_hasher::hash (const value_type *v)
{
  return tree_map_base_hash (v);
}

inline bool
tree_int_map_hasher::equal (const value_type *v, const compare_type *c)
{
  return tree_int_map_eq (v, c);
}


/* This routine will initialize the basevar fields of MAP.  */

static void
var_map_base_init (var_map map)
{
  int x, num_part;
  tree var;
  hash_table <tree_int_map_hasher> tree_to_index;
  struct tree_int_map *m, *mapstorage;

  num_part = num_var_partitions (map);
  tree_to_index.create (num_part);
  /* We can have at most num_part entries in the hash tables, so it's
     enough to allocate so many map elements once, saving some malloc
     calls.  */
  mapstorage = m = XNEWVEC (struct tree_int_map, num_part);

  /* If a base table already exists, clear it, otherwise create it.  */
  free (map->partition_to_base_index);
  map->partition_to_base_index = (int *) xmalloc (sizeof (int) * num_part);

  /* Build the base variable list, and point partitions at their bases.  */
  for (x = 0; x < num_part; x++)
    {
      struct tree_int_map **slot;
      unsigned baseindex;
      var = partition_to_var (map, x);
      if (SSA_NAME_VAR (var)
	  && (!VAR_P (SSA_NAME_VAR (var))
	      || !DECL_IGNORED_P (SSA_NAME_VAR (var))))
	m->base.from = SSA_NAME_VAR (var);
      else
	/* This restricts what anonymous SSA names we can coalesce
	   as it restricts the sets we compute conflicts for.
	   Using TREE_TYPE to generate sets is the easies as
	   type equivalency also holds for SSA names with the same
	   underlying decl. 

	   Check gimple_can_coalesce_p when changing this code.  */
	m->base.from = (TYPE_CANONICAL (TREE_TYPE (var))
			? TYPE_CANONICAL (TREE_TYPE (var))
			: TREE_TYPE (var));
      /* If base variable hasn't been seen, set it up.  */
      slot = tree_to_index.find_slot (m, INSERT);
      if (!*slot)
	{
	  baseindex = m - mapstorage;
	  m->to = baseindex;
	  *slot = m;
	  m++;
	}
      else
	baseindex = (*slot)->to;
      map->partition_to_base_index[x] = baseindex;
    }

  map->num_basevars = m - mapstorage;

  free (mapstorage);
  tree_to_index. dispose ();
}


/* Remove the base table in MAP.  */

static void
var_map_base_fini (var_map map)
{
  /* Free the basevar info if it is present.  */
  if (map->partition_to_base_index != NULL)
    {
      free (map->partition_to_base_index);
      map->partition_to_base_index = NULL;
      map->num_basevars = 0;
    }
}
/* Create a variable partition map of SIZE, initialize and return it.  */

var_map
init_var_map (int size)
{
  var_map map;

  map = (var_map) xmalloc (sizeof (struct _var_map));
  map->var_partition = partition_new (size);

  map->partition_to_view = NULL;
  map->view_to_partition = NULL;
  map->num_partitions = size;
  map->partition_size = size;
  map->num_basevars = 0;
  map->partition_to_base_index = NULL;
  return map;
}


/* Free memory associated with MAP.  */

void
delete_var_map (var_map map)
{
  var_map_base_fini (map);
  partition_delete (map->var_partition);
  free (map->partition_to_view);
  free (map->view_to_partition);
  free (map);
}


/* This function will combine the partitions in MAP for VAR1 and VAR2.  It
   Returns the partition which represents the new partition.  If the two
   partitions cannot be combined, NO_PARTITION is returned.  */

int
var_union (var_map map, tree var1, tree var2)
{
  int p1, p2, p3;

  gcc_assert (TREE_CODE (var1) == SSA_NAME);
  gcc_assert (TREE_CODE (var2) == SSA_NAME);

  /* This is independent of partition_to_view. If partition_to_view is
     on, then whichever one of these partitions is absorbed will never have a
     dereference into the partition_to_view array any more.  */

  p1 = partition_find (map->var_partition, SSA_NAME_VERSION (var1));
  p2 = partition_find (map->var_partition, SSA_NAME_VERSION (var2));

  gcc_assert (p1 != NO_PARTITION);
  gcc_assert (p2 != NO_PARTITION);

  if (p1 == p2)
    p3 = p1;
  else
    p3 = partition_union (map->var_partition, p1, p2);

  if (map->partition_to_view)
    p3 = map->partition_to_view[p3];

  return p3;
}


/* Compress the partition numbers in MAP such that they fall in the range
   0..(num_partitions-1) instead of wherever they turned out during
   the partitioning exercise.  This removes any references to unused
   partitions, thereby allowing bitmaps and other vectors to be much
   denser.

   This is implemented such that compaction doesn't affect partitioning.
   Ie., once partitions are created and possibly merged, running one
   or more different kind of compaction will not affect the partitions
   themselves.  Their index might change, but all the same variables will
   still be members of the same partition group.  This allows work on reduced
   sets, and no loss of information when a larger set is later desired.

   In particular, coalescing can work on partitions which have 2 or more
   definitions, and then 'recompact' later to include all the single
   definitions for assignment to program variables.  */


/* Set MAP back to the initial state of having no partition view.  Return a
   bitmap which has a bit set for each partition number which is in use in the
   varmap.  */

static bitmap
partition_view_init (var_map map)
{
  bitmap used;
  int tmp;
  unsigned int x;

  used = BITMAP_ALLOC (NULL);

  /* Already in a view? Abandon the old one.  */
  if (map->partition_to_view)
    {
      free (map->partition_to_view);
      map->partition_to_view = NULL;
    }
  if (map->view_to_partition)
    {
      free (map->view_to_partition);
      map->view_to_partition = NULL;
    }

  /* Find out which partitions are actually referenced.  */
  for (x = 0; x < map->partition_size; x++)
    {
      tmp = partition_find (map->var_partition, x);
      if (ssa_name (tmp) != NULL_TREE && !virtual_operand_p (ssa_name (tmp))
	  && (!has_zero_uses (ssa_name (tmp))
	      || !SSA_NAME_IS_DEFAULT_DEF (ssa_name (tmp))))
	bitmap_set_bit (used, tmp);
    }

  map->num_partitions = map->partition_size;
  return used;
}


/* This routine will finalize the view data for MAP based on the partitions
   set in SELECTED.  This is either the same bitmap returned from
   partition_view_init, or a trimmed down version if some of those partitions
   were not desired in this view.  SELECTED is freed before returning.  */

static void
partition_view_fini (var_map map, bitmap selected)
{
  bitmap_iterator bi;
  unsigned count, i, x, limit;

  gcc_assert (selected);

  count = bitmap_count_bits (selected);
  limit = map->partition_size;

  /* If its a one-to-one ratio, we don't need any view compaction.  */
  if (count < limit)
    {
      map->partition_to_view = (int *)xmalloc (limit * sizeof (int));
      memset (map->partition_to_view, 0xff, (limit * sizeof (int)));
      map->view_to_partition = (int *)xmalloc (count * sizeof (int));

      i = 0;
      /* Give each selected partition an index.  */
      EXECUTE_IF_SET_IN_BITMAP (selected, 0, x, bi)
	{
	  map->partition_to_view[x] = i;
	  map->view_to_partition[i] = x;
	  i++;
	}
      gcc_assert (i == count);
      map->num_partitions = i;
    }

  BITMAP_FREE (selected);
}


/* Create a partition view which includes all the used partitions in MAP.  If
   WANT_BASES is true, create the base variable map as well.  */

void
partition_view_normal (var_map map, bool want_bases)
{
  bitmap used;

  used = partition_view_init (map);
  partition_view_fini (map, used);

  if (want_bases)
    var_map_base_init (map);
  else
    var_map_base_fini (map);
}


/* Create a partition view in MAP which includes just partitions which occur in
   the bitmap ONLY. If WANT_BASES is true, create the base variable map
   as well.  */

void
partition_view_bitmap (var_map map, bitmap only, bool want_bases)
{
  bitmap used;
  bitmap new_partitions = BITMAP_ALLOC (NULL);
  unsigned x, p;
  bitmap_iterator bi;

  used = partition_view_init (map);
  EXECUTE_IF_SET_IN_BITMAP (only, 0, x, bi)
    {
      p = partition_find (map->var_partition, x);
      gcc_assert (bitmap_bit_p (used, p));
      bitmap_set_bit (new_partitions, p);
    }
  partition_view_fini (map, new_partitions);

  if (want_bases)
    var_map_base_init (map);
  else
    var_map_base_fini (map);
}


static bitmap usedvars;

/* Mark VAR as used, so that it'll be preserved during rtl expansion.
   Returns true if VAR wasn't marked before.  */

static inline bool
set_is_used (tree var)
{
  return bitmap_set_bit (usedvars, DECL_UID (var));
}

/* Return true if VAR is marked as used.  */

static inline bool
is_used_p (tree var)
{
  return bitmap_bit_p (usedvars, DECL_UID (var));
}

static inline void mark_all_vars_used (tree *);

/* Helper function for mark_all_vars_used, called via walk_tree.  */

static tree
mark_all_vars_used_1 (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;
  enum tree_code_class c = TREE_CODE_CLASS (TREE_CODE (t));
  tree b;

  if (TREE_CODE (t) == SSA_NAME)
    {
      *walk_subtrees = 0;
      t = SSA_NAME_VAR (t);
      if (!t)
	return NULL;
    }

  if (IS_EXPR_CODE_CLASS (c)
      && (b = TREE_BLOCK (t)) != NULL)
    TREE_USED (b) = true;

  /* Ignore TMR_OFFSET and TMR_STEP for TARGET_MEM_REFS, as those
     fields do not contain vars.  */
  if (TREE_CODE (t) == TARGET_MEM_REF)
    {
      mark_all_vars_used (&TMR_BASE (t));
      mark_all_vars_used (&TMR_INDEX (t));
      mark_all_vars_used (&TMR_INDEX2 (t));
      *walk_subtrees = 0;
      return NULL;
    }

  /* Only need to mark VAR_DECLS; parameters and return results are not
     eliminated as unused.  */
  if (TREE_CODE (t) == VAR_DECL)
    {
      /* When a global var becomes used for the first time also walk its
         initializer (non global ones don't have any).  */
      if (set_is_used (t) && is_global_var (t))
	mark_all_vars_used (&DECL_INITIAL (t));
    }
  /* remove_unused_scope_block_p requires information about labels
     which are not DECL_IGNORED_P to tell if they might be used in the IL.  */
  else if (TREE_CODE (t) == LABEL_DECL)
    /* Although the TREE_USED values that the frontend uses would be
       acceptable (albeit slightly over-conservative) for our purposes,
       init_vars_expansion clears TREE_USED for LABEL_DECLs too, so we
       must re-compute it here.  */
    TREE_USED (t) = 1;

  if (IS_TYPE_OR_DECL_P (t))
    *walk_subtrees = 0;

  return NULL;
}

/* Mark the scope block SCOPE and its subblocks unused when they can be
   possibly eliminated if dead.  */

static void
mark_scope_block_unused (tree scope)
{
  tree t;
  TREE_USED (scope) = false;
  if (!(*debug_hooks->ignore_block) (scope))
    TREE_USED (scope) = true;
  for (t = BLOCK_SUBBLOCKS (scope); t ; t = BLOCK_CHAIN (t))
    mark_scope_block_unused (t);
}

/* Look if the block is dead (by possibly eliminating its dead subblocks)
   and return true if so.
   Block is declared dead if:
     1) No statements are associated with it.
     2) Declares no live variables
     3) All subblocks are dead
	or there is precisely one subblocks and the block
	has same abstract origin as outer block and declares
	no variables, so it is pure wrapper.
   When we are not outputting full debug info, we also eliminate dead variables
   out of scope blocks to let them to be recycled by GGC and to save copying work
   done by the inliner.  */

static bool
remove_unused_scope_block_p (tree scope)
{
  tree *t, *next;
  bool unused = !TREE_USED (scope);
  int nsubblocks = 0;

  for (t = &BLOCK_VARS (scope); *t; t = next)
    {
      next = &DECL_CHAIN (*t);

      /* Debug info of nested function refers to the block of the
	 function.  We might stil call it even if all statements
	 of function it was nested into was elliminated.

	 TODO: We can actually look into cgraph to see if function
	 will be output to file.  */
      if (TREE_CODE (*t) == FUNCTION_DECL)
	unused = false;

      /* If a decl has a value expr, we need to instantiate it
	 regardless of debug info generation, to avoid codegen
	 differences in memory overlap tests.  update_equiv_regs() may
	 indirectly call validate_equiv_mem() to test whether a
	 SET_DEST overlaps with others, and if the value expr changes
	 by virtual register instantiation, we may get end up with
	 different results.  */
      else if (TREE_CODE (*t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (*t))
	unused = false;

      /* Remove everything we don't generate debug info for.  */
      else if (DECL_IGNORED_P (*t))
	{
	  *t = DECL_CHAIN (*t);
	  next = t;
	}

      /* When we are outputting debug info, we usually want to output
	 info about optimized-out variables in the scope blocks.
	 Exception are the scope blocks not containing any instructions
	 at all so user can't get into the scopes at first place.  */
      else if (is_used_p (*t))
	unused = false;
      else if (TREE_CODE (*t) == LABEL_DECL && TREE_USED (*t))
	/* For labels that are still used in the IL, the decision to
	   preserve them must not depend DEBUG_INFO_LEVEL, otherwise we
	   risk having different ordering in debug vs.  non-debug builds
	   during inlining or versioning.
	   A label appearing here (we have already checked DECL_IGNORED_P)
	   should not be used in the IL unless it has been explicitly used
	   before, so we use TREE_USED as an approximation.  */
	/* In principle, we should do the same here as for the debug case
	   below, however, when debugging, there might be additional nested
	   levels that keep an upper level with a label live, so we have to
	   force this block to be considered used, too.  */
	unused = false;

      /* When we are not doing full debug info, we however can keep around
	 only the used variables for cfgexpand's memory packing saving quite
	 a lot of memory.

	 For sake of -g3, we keep around those vars but we don't count this as
	 use of block, so innermost block with no used vars and no instructions
	 can be considered dead.  We only want to keep around blocks user can
	 breakpoint into and ask about value of optimized out variables.

	 Similarly we need to keep around types at least until all
	 variables of all nested blocks are gone.  We track no
	 information on whether given type is used or not, so we have
	 to keep them even when not emitting debug information,
	 otherwise we may end up remapping variables and their (local)
	 types in different orders depending on whether debug
	 information is being generated.  */

      else if (TREE_CODE (*t) == TYPE_DECL
	       || debug_info_level == DINFO_LEVEL_NORMAL
	       || debug_info_level == DINFO_LEVEL_VERBOSE)
	;
      else
	{
	  *t = DECL_CHAIN (*t);
	  next = t;
	}
    }

  for (t = &BLOCK_SUBBLOCKS (scope); *t ;)
    if (remove_unused_scope_block_p (*t))
      {
	if (BLOCK_SUBBLOCKS (*t))
	  {
	    tree next = BLOCK_CHAIN (*t);
	    tree supercontext = BLOCK_SUPERCONTEXT (*t);

	    *t = BLOCK_SUBBLOCKS (*t);
	    while (BLOCK_CHAIN (*t))
	      {
	        BLOCK_SUPERCONTEXT (*t) = supercontext;
	        t = &BLOCK_CHAIN (*t);
	      }
	    BLOCK_CHAIN (*t) = next;
	    BLOCK_SUPERCONTEXT (*t) = supercontext;
	    t = &BLOCK_CHAIN (*t);
	    nsubblocks ++;
	  }
	else
	  *t = BLOCK_CHAIN (*t);
      }
    else
      {
        t = &BLOCK_CHAIN (*t);
	nsubblocks ++;
      }


   if (!unused)
     ;
   /* Outer scope is always used.  */
   else if (!BLOCK_SUPERCONTEXT (scope)
            || TREE_CODE (BLOCK_SUPERCONTEXT (scope)) == FUNCTION_DECL)
     unused = false;
   /* Innermost blocks with no live variables nor statements can be always
      eliminated.  */
   else if (!nsubblocks)
     ;
   /* When not generating debug info we can eliminate info on unused
      variables.  */
   else if (debug_info_level == DINFO_LEVEL_NONE)
     {
       /* Even for -g0 don't prune outer scopes from artificial
	  functions, otherwise diagnostics using tree_nonartificial_location
	  will not be emitted properly.  */
       if (inlined_function_outer_scope_p (scope))
	 {
	   tree ao = scope;

	   while (ao
		  && TREE_CODE (ao) == BLOCK
		  && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
	     ao = BLOCK_ABSTRACT_ORIGIN (ao);
	   if (ao
	       && TREE_CODE (ao) == FUNCTION_DECL
	       && DECL_DECLARED_INLINE_P (ao)
	       && lookup_attribute ("artificial", DECL_ATTRIBUTES (ao)))
	     unused = false;
	 }
     }
   else if (BLOCK_VARS (scope) || BLOCK_NUM_NONLOCALIZED_VARS (scope))
     unused = false;
   /* See if this block is important for representation of inlined function.
      Inlined functions are always represented by block with
      block_ultimate_origin being set to FUNCTION_DECL and DECL_SOURCE_LOCATION
      set...  */
   else if (inlined_function_outer_scope_p (scope))
     unused = false;
   else
   /* Verfify that only blocks with source location set
      are entry points to the inlined functions.  */
     gcc_assert (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (scope))
		 == UNKNOWN_LOCATION);

   TREE_USED (scope) = !unused;
   return unused;
}

/* Mark all VAR_DECLS under *EXPR_P as used, so that they won't be
   eliminated during the tree->rtl conversion process.  */

static inline void
mark_all_vars_used (tree *expr_p)
{
  walk_tree (expr_p, mark_all_vars_used_1, NULL, NULL);
}

/* Helper function for clear_unused_block_pointer, called via walk_tree.  */

static tree
clear_unused_block_pointer_1 (tree *tp, int *, void *)
{
  if (EXPR_P (*tp) && TREE_BLOCK (*tp)
      && !TREE_USED (TREE_BLOCK (*tp)))
    TREE_SET_BLOCK (*tp, NULL);
  return NULL_TREE;
}

/* Set all block pointer in debug or clobber stmt to NULL if the block
   is unused, so that they will not be streamed out.  */

static void
clear_unused_block_pointer (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	unsigned i;
	tree b;
	gimple stmt = gsi_stmt (gsi);

	if (!is_gimple_debug (stmt) && !gimple_clobber_p (stmt))
	  continue;
	b = gimple_block (stmt);
	if (b && !TREE_USED (b))
	  gimple_set_block (stmt, NULL);
	for (i = 0; i < gimple_num_ops (stmt); i++)
	  walk_tree (gimple_op_ptr (stmt, i), clear_unused_block_pointer_1,
		     NULL, NULL);
      }
}

/* Dump scope blocks starting at SCOPE to FILE.  INDENT is the
   indentation level and FLAGS is as in print_generic_expr.  */

static void
dump_scope_block (FILE *file, int indent, tree scope, int flags)
{
  tree var, t;
  unsigned int i;

  fprintf (file, "\n%*s{ Scope block #%i%s%s",indent, "" , BLOCK_NUMBER (scope),
  	   TREE_USED (scope) ? "" : " (unused)",
	   BLOCK_ABSTRACT (scope) ? " (abstract)": "");
  if (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (scope)) != UNKNOWN_LOCATION)
    {
      expanded_location s = expand_location (BLOCK_SOURCE_LOCATION (scope));
      fprintf (file, " %s:%i", s.file, s.line);
    }
  if (BLOCK_ABSTRACT_ORIGIN (scope))
    {
      tree origin = block_ultimate_origin (scope);
      if (origin)
	{
	  fprintf (file, " Originating from :");
	  if (DECL_P (origin))
	    print_generic_decl (file, origin, flags);
	  else
	    fprintf (file, "#%i", BLOCK_NUMBER (origin));
	}
    }
  fprintf (file, " \n");
  for (var = BLOCK_VARS (scope); var; var = DECL_CHAIN (var))
    {
      fprintf (file, "%*s", indent, "");
      print_generic_decl (file, var, flags);
      fprintf (file, "\n");
    }
  for (i = 0; i < BLOCK_NUM_NONLOCALIZED_VARS (scope); i++)
    {
      fprintf (file, "%*s",indent, "");
      print_generic_decl (file, BLOCK_NONLOCALIZED_VAR (scope, i),
      			  flags);
      fprintf (file, " (nonlocalized)\n");
    }
  for (t = BLOCK_SUBBLOCKS (scope); t ; t = BLOCK_CHAIN (t))
    dump_scope_block (file, indent + 2, t, flags);
  fprintf (file, "\n%*s}\n",indent, "");
}

/* Dump the tree of lexical scopes starting at SCOPE to stderr.  FLAGS
   is as in print_generic_expr.  */

DEBUG_FUNCTION void
debug_scope_block (tree scope, int flags)
{
  dump_scope_block (stderr, 0, scope, flags);
}


/* Dump the tree of lexical scopes of current_function_decl to FILE.
   FLAGS is as in print_generic_expr.  */

void
dump_scope_blocks (FILE *file, int flags)
{
  dump_scope_block (file, 0, DECL_INITIAL (current_function_decl), flags);
}


/* Dump the tree of lexical scopes of current_function_decl to stderr.
   FLAGS is as in print_generic_expr.  */

DEBUG_FUNCTION void
debug_scope_blocks (int flags)
{
  dump_scope_blocks (stderr, flags);
}

/* Remove local variables that are not referenced in the IL.  */

void
remove_unused_locals (void)
{
  basic_block bb;
  tree var;
  unsigned srcidx, dstidx, num;
  bool have_local_clobbers = false;

  /* Removing declarations from lexical blocks when not optimizing is
     not only a waste of time, it actually causes differences in stack
     layout.  */
  if (!optimize)
    return;

  timevar_push (TV_REMOVE_UNUSED);

  mark_scope_block_unused (DECL_INITIAL (current_function_decl));

  usedvars = BITMAP_ALLOC (NULL);

  /* Walk the CFG marking all referenced symbols.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      size_t i;
      edge_iterator ei;
      edge e;

      /* Walk the statements.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  tree b = gimple_block (stmt);

	  if (is_gimple_debug (stmt))
	    continue;

	  if (gimple_clobber_p (stmt))
	    {
	      have_local_clobbers = true;
	      continue;
	    }

	  if (b)
	    TREE_USED (b) = true;

	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    mark_all_vars_used (gimple_op_ptr (gsi_stmt (gsi), i));
	}

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
          use_operand_p arg_p;
          ssa_op_iter i;
	  tree def;
	  gimple phi = gsi_stmt (gsi);

	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;

	  def = gimple_phi_result (phi);
	  mark_all_vars_used (&def);

          FOR_EACH_PHI_ARG (arg_p, phi, i, SSA_OP_ALL_USES)
            {
	      tree arg = USE_FROM_PTR (arg_p);
	      int index = PHI_ARG_INDEX_FROM_USE (arg_p);
	      tree block =
		LOCATION_BLOCK (gimple_phi_arg_location (phi, index));
	      if (block != NULL)
		TREE_USED (block) = true;
	      mark_all_vars_used (&arg);
            }
        }

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (LOCATION_BLOCK (e->goto_locus) != NULL)
	  TREE_USED (LOCATION_BLOCK (e->goto_locus)) = true;
    }

  /* We do a two-pass approach about the out-of-scope clobbers.  We want
     to remove them if they are the only references to a local variable,
     but we want to retain them when there's any other.  So the first pass
     ignores them, and the second pass (if there were any) tries to remove
     them.  */
  if (have_local_clobbers)
    FOR_EACH_BB_FN (bb, cfun)
      {
	gimple_stmt_iterator gsi;

	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	  {
	    gimple stmt = gsi_stmt (gsi);
	    tree b = gimple_block (stmt);

	    if (gimple_clobber_p (stmt))
	      {
		tree lhs = gimple_assign_lhs (stmt);
		tree base = get_base_address (lhs);
		/* Remove clobbers referencing unused vars, or clobbers
		   with MEM_REF lhs referencing uninitialized pointers.  */
		if ((TREE_CODE (base) == VAR_DECL && !is_used_p (base))
		    || (TREE_CODE (lhs) == MEM_REF
			&& TREE_CODE (TREE_OPERAND (lhs, 0)) == SSA_NAME
			&& SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (lhs, 0))
			&& (TREE_CODE (SSA_NAME_VAR (TREE_OPERAND (lhs, 0)))
			    != PARM_DECL)))
		  {
		    unlink_stmt_vdef (stmt);
		    gsi_remove (&gsi, true);
		    release_defs (stmt);
		    continue;
		  }
		if (b)
		  TREE_USED (b) = true;
	      }
	    gsi_next (&gsi);
	  }
      }

  cfun->has_local_explicit_reg_vars = false;

  /* Remove unmarked local and global vars from local_decls.  */
  num = vec_safe_length (cfun->local_decls);
  for (srcidx = 0, dstidx = 0; srcidx < num; srcidx++)
    {
      var = (*cfun->local_decls)[srcidx];
      if (TREE_CODE (var) == VAR_DECL)
	{
	  if (!is_used_p (var))
	    {
	      tree def;
	      if (cfun->nonlocal_goto_save_area
		  && TREE_OPERAND (cfun->nonlocal_goto_save_area, 0) == var)
		cfun->nonlocal_goto_save_area = NULL;
	      /* Release any default def associated with var.  */
	      if ((def = ssa_default_def (cfun, var)) != NULL_TREE)
		{
		  set_ssa_default_def (cfun, var, NULL_TREE);
		  release_ssa_name (def);
		}
	      continue;
	    }
	}
      if (TREE_CODE (var) == VAR_DECL
	  && DECL_HARD_REGISTER (var)
	  && !is_global_var (var))
	cfun->has_local_explicit_reg_vars = true;

      if (srcidx != dstidx)
	(*cfun->local_decls)[dstidx] = var;
      dstidx++;
    }
  if (dstidx != num)
    {
      statistics_counter_event (cfun, "unused VAR_DECLs removed", num - dstidx);
      cfun->local_decls->truncate (dstidx);
    }

  remove_unused_scope_block_p (DECL_INITIAL (current_function_decl));
  clear_unused_block_pointer ();

  BITMAP_FREE (usedvars);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Scope blocks after cleanups:\n");
      dump_scope_blocks (dump_file, dump_flags);
    }

  timevar_pop (TV_REMOVE_UNUSED);
}

/* Obstack for globale liveness info bitmaps.  We don't want to put these
   on the default obstack because these bitmaps can grow quite large and
   we'll hold on to all that memory until the end of the compiler run.
   As a bonus, delete_tree_live_info can destroy all the bitmaps by just
   releasing the whole obstack.  */
static bitmap_obstack liveness_bitmap_obstack;

/* Allocate and return a new live range information object base on MAP.  */

static tree_live_info_p
new_tree_live_info (var_map map)
{
  tree_live_info_p live;
  basic_block bb;

  live = XNEW (struct tree_live_info_d);
  live->map = map;
  live->num_blocks = last_basic_block_for_fn (cfun);

  live->livein = XNEWVEC (bitmap_head, last_basic_block_for_fn (cfun));
  FOR_EACH_BB_FN (bb, cfun)
    bitmap_initialize (&live->livein[bb->index], &liveness_bitmap_obstack);

  live->liveout = XNEWVEC (bitmap_head, last_basic_block_for_fn (cfun));
  FOR_EACH_BB_FN (bb, cfun)
    bitmap_initialize (&live->liveout[bb->index], &liveness_bitmap_obstack);

  live->work_stack = XNEWVEC (int, last_basic_block_for_fn (cfun));
  live->stack_top = live->work_stack;

  live->global = BITMAP_ALLOC (&liveness_bitmap_obstack);
  return live;
}


/* Free storage for live range info object LIVE.  */

void
delete_tree_live_info (tree_live_info_p live)
{
  bitmap_obstack_release (&liveness_bitmap_obstack);
  free (live->work_stack);
  free (live->liveout);
  free (live->livein);
  free (live);
}


/* Visit basic block BB and propagate any required live on entry bits from
   LIVE into the predecessors.  VISITED is the bitmap of visited blocks.
   TMP is a temporary work bitmap which is passed in to avoid reallocating
   it each time.  */

static void
loe_visit_block (tree_live_info_p live, basic_block bb, sbitmap visited,
		 bitmap tmp)
{
  edge e;
  bool change;
  edge_iterator ei;
  basic_block pred_bb;
  bitmap loe;

  gcc_checking_assert (!bitmap_bit_p (visited, bb->index));
  bitmap_set_bit (visited, bb->index);

  loe = live_on_entry (live, bb);

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      pred_bb = e->src;
      if (pred_bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	continue;
      /* TMP is variables live-on-entry from BB that aren't defined in the
	 predecessor block.  This should be the live on entry vars to pred.
	 Note that liveout is the DEFs in a block while live on entry is
	 being calculated.  */
      bitmap_and_compl (tmp, loe, &live->liveout[pred_bb->index]);

      /* Add these bits to live-on-entry for the pred. if there are any
	 changes, and pred_bb has been visited already, add it to the
	 revisit stack.  */
      change = bitmap_ior_into (live_on_entry (live, pred_bb), tmp);
      if (bitmap_bit_p (visited, pred_bb->index) && change)
	{
	  bitmap_clear_bit (visited, pred_bb->index);
	  *(live->stack_top)++ = pred_bb->index;
	}
    }
}


/* Using LIVE, fill in all the live-on-entry blocks between the defs and uses
   of all the variables.  */

static void
live_worklist (tree_live_info_p live)
{
  unsigned b;
  basic_block bb;
  sbitmap visited = sbitmap_alloc (last_basic_block_for_fn (cfun) + 1);
  bitmap tmp = BITMAP_ALLOC (&liveness_bitmap_obstack);

  bitmap_clear (visited);

  /* Visit all the blocks in reverse order and propagate live on entry values
     into the predecessors blocks.  */
  FOR_EACH_BB_REVERSE (bb)
    loe_visit_block (live, bb, visited, tmp);

  /* Process any blocks which require further iteration.  */
  while (live->stack_top != live->work_stack)
    {
      b = *--(live->stack_top);
      loe_visit_block (live, BASIC_BLOCK_FOR_FN (cfun, b), visited, tmp);
    }

  BITMAP_FREE (tmp);
  sbitmap_free (visited);
}


/* Calculate the initial live on entry vector for SSA_NAME using immediate_use
   links.  Set the live on entry fields in LIVE.  Def's are marked temporarily
   in the liveout vector.  */

static void
set_var_live_on_entry (tree ssa_name, tree_live_info_p live)
{
  int p;
  gimple stmt;
  use_operand_p use;
  basic_block def_bb = NULL;
  imm_use_iterator imm_iter;
  bool global = false;

  p = var_to_partition (live->map, ssa_name);
  if (p == NO_PARTITION)
    return;

  stmt = SSA_NAME_DEF_STMT (ssa_name);
  if (stmt)
    {
      def_bb = gimple_bb (stmt);
      /* Mark defs in liveout bitmap temporarily.  */
      if (def_bb)
	bitmap_set_bit (&live->liveout[def_bb->index], p);
    }
  else
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  /* Visit each use of SSA_NAME and if it isn't in the same block as the def,
     add it to the list of live on entry blocks.  */
  FOR_EACH_IMM_USE_FAST (use, imm_iter, ssa_name)
    {
      gimple use_stmt = USE_STMT (use);
      basic_block add_block = NULL;

      if (gimple_code (use_stmt) == GIMPLE_PHI)
        {
	  /* Uses in PHI's are considered to be live at exit of the SRC block
	     as this is where a copy would be inserted.  Check to see if it is
	     defined in that block, or whether its live on entry.  */
	  int index = PHI_ARG_INDEX_FROM_USE (use);
	  edge e = gimple_phi_arg_edge (use_stmt, index);
	  if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    {
	      if (e->src != def_bb)
		add_block = e->src;
	    }
	}
      else if (is_gimple_debug (use_stmt))
	continue;
      else
        {
	  /* If its not defined in this block, its live on entry.  */
	  basic_block use_bb = gimple_bb (use_stmt);
	  if (use_bb != def_bb)
	    add_block = use_bb;
	}

      /* If there was a live on entry use, set the bit.  */
      if (add_block)
        {
	  global = true;
	  bitmap_set_bit (&live->livein[add_block->index], p);
	}
    }

  /* If SSA_NAME is live on entry to at least one block, fill in all the live
     on entry blocks between the def and all the uses.  */
  if (global)
    bitmap_set_bit (live->global, p);
}


/* Calculate the live on exit vectors based on the entry info in LIVEINFO.  */

void
calculate_live_on_exit (tree_live_info_p liveinfo)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  /* live on entry calculations used liveout vectors for defs, clear them.  */
  FOR_EACH_BB_FN (bb, cfun)
    bitmap_clear (&liveinfo->liveout[bb->index]);

  /* Set all the live-on-exit bits for uses in PHIs.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      size_t i;

      /* Mark the PHI arguments which are live on exit to the pred block.  */
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);
	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree t = PHI_ARG_DEF (phi, i);
	      int p;

	      if (TREE_CODE (t) != SSA_NAME)
		continue;

	      p = var_to_partition (liveinfo->map, t);
	      if (p == NO_PARTITION)
		continue;
	      e = gimple_phi_arg_edge (phi, i);
	      if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
		bitmap_set_bit (&liveinfo->liveout[e->src->index], p);
	    }
	}

      /* Add each successors live on entry to this bock live on exit.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	  bitmap_ior_into (&liveinfo->liveout[bb->index],
			   live_on_entry (liveinfo, e->dest));
    }
}


/* Given partition map MAP, calculate all the live on entry bitmaps for
   each partition.  Return a new live info object.  */

tree_live_info_p
calculate_live_ranges (var_map map)
{
  tree var;
  unsigned i;
  tree_live_info_p live;

  bitmap_obstack_initialize (&liveness_bitmap_obstack);
  live = new_tree_live_info (map);
  for (i = 0; i < num_var_partitions (map); i++)
    {
      var = partition_to_var (map, i);
      if (var != NULL_TREE)
	set_var_live_on_entry (var, live);
    }

  live_worklist (live);

#ifdef ENABLE_CHECKING
  verify_live_on_entry (live);
#endif

  calculate_live_on_exit (live);
  return live;
}


/* Output partition map MAP to file F.  */

void
dump_var_map (FILE *f, var_map map)
{
  int t;
  unsigned x, y;
  int p;

  fprintf (f, "\nPartition map \n\n");

  for (x = 0; x < map->num_partitions; x++)
    {
      if (map->view_to_partition != NULL)
	p = map->view_to_partition[x];
      else
	p = x;

      if (ssa_name (p) == NULL_TREE
	  || virtual_operand_p (ssa_name (p)))
        continue;

      t = 0;
      for (y = 1; y < num_ssa_names; y++)
        {
	  p = partition_find (map->var_partition, y);
	  if (map->partition_to_view)
	    p = map->partition_to_view[p];
	  if (p == (int)x)
	    {
	      if (t++ == 0)
	        {
		  fprintf (f, "Partition %d (", x);
		  print_generic_expr (f, partition_to_var (map, p), TDF_SLIM);
		  fprintf (f, " - ");
		}
	      fprintf (f, "%d ", y);
	    }
	}
      if (t != 0)
	fprintf (f, ")\n");
    }
  fprintf (f, "\n");
}


/* Generic dump for the above.  */

DEBUG_FUNCTION void
debug (_var_map &ref)
{
  dump_var_map (stderr, &ref);
}

DEBUG_FUNCTION void
debug (_var_map *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Output live range info LIVE to file F, controlled by FLAG.  */

void
dump_live_info (FILE *f, tree_live_info_p live, int flag)
{
  basic_block bb;
  unsigned i;
  var_map map = live->map;
  bitmap_iterator bi;

  if ((flag & LIVEDUMP_ENTRY) && live->livein)
    {
      FOR_EACH_BB_FN (bb, cfun)
	{
	  fprintf (f, "\nLive on entry to BB%d : ", bb->index);
	  EXECUTE_IF_SET_IN_BITMAP (&live->livein[bb->index], 0, i, bi)
	    {
	      print_generic_expr (f, partition_to_var (map, i), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }

  if ((flag & LIVEDUMP_EXIT) && live->liveout)
    {
      FOR_EACH_BB_FN (bb, cfun)
	{
	  fprintf (f, "\nLive on exit from BB%d : ", bb->index);
	  EXECUTE_IF_SET_IN_BITMAP (&live->liveout[bb->index], 0, i, bi)
	    {
	      print_generic_expr (f, partition_to_var (map, i), TDF_SLIM);
	      fprintf (f, "  ");
	    }
	  fprintf (f, "\n");
	}
    }
}


/* Generic dump for the above.  */

DEBUG_FUNCTION void
debug (tree_live_info_d &ref)
{
  dump_live_info (stderr, &ref, 0);
}

DEBUG_FUNCTION void
debug (tree_live_info_d *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


#ifdef ENABLE_CHECKING
/* Verify that SSA_VAR is a non-virtual SSA_NAME.  */

void
register_ssa_partition_check (tree ssa_var)
{
  gcc_assert (TREE_CODE (ssa_var) == SSA_NAME);
  if (virtual_operand_p (ssa_var))
    {
      fprintf (stderr, "Illegally registering a virtual SSA name :");
      print_generic_expr (stderr, ssa_var, TDF_SLIM);
      fprintf (stderr, " in the SSA->Normal phase.\n");
      internal_error ("SSA corruption");
    }
}


/* Verify that the info in LIVE matches the current cfg.  */

static void
verify_live_on_entry (tree_live_info_p live)
{
  unsigned i;
  tree var;
  gimple stmt;
  basic_block bb;
  edge e;
  int num;
  edge_iterator ei;
  var_map map = live->map;

   /* Check for live on entry partitions and report those with a DEF in
      the program. This will typically mean an optimization has done
      something wrong.  */
  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  num = 0;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      int entry_block = e->dest->index;
      if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
        continue;
      for (i = 0; i < (unsigned)num_var_partitions (map); i++)
	{
	  basic_block tmp;
	  tree d = NULL_TREE;
	  bitmap loe;
	  var = partition_to_var (map, i);
	  stmt = SSA_NAME_DEF_STMT (var);
	  tmp = gimple_bb (stmt);
	  if (SSA_NAME_VAR (var))
	    d = ssa_default_def (cfun, SSA_NAME_VAR (var));

	  loe = live_on_entry (live, e->dest);
	  if (loe && bitmap_bit_p (loe, i))
	    {
	      if (!gimple_nop_p (stmt))
		{
		  num++;
		  print_generic_expr (stderr, var, TDF_SLIM);
		  fprintf (stderr, " is defined ");
		  if (tmp)
		    fprintf (stderr, " in BB%d, ", tmp->index);
		  fprintf (stderr, "by:\n");
		  print_gimple_stmt (stderr, stmt, 0, TDF_SLIM);
		  fprintf (stderr, "\nIt is also live-on-entry to entry BB %d",
			   entry_block);
		  fprintf (stderr, " So it appears to have multiple defs.\n");
		}
	      else
	        {
		  if (d != var)
		    {
		      num++;
		      print_generic_expr (stderr, var, TDF_SLIM);
		      fprintf (stderr, " is live-on-entry to BB%d ",
			       entry_block);
		      if (d)
		        {
			  fprintf (stderr, " but is not the default def of ");
			  print_generic_expr (stderr, d, TDF_SLIM);
			  fprintf (stderr, "\n");
			}
		      else
			fprintf (stderr, " and there is no default def.\n");
		    }
		}
	    }
	  else
	    if (d == var)
	      {
		/* The only way this var shouldn't be marked live on entry is
		   if it occurs in a PHI argument of the block.  */
		size_t z;
		bool ok = false;
		gimple_stmt_iterator gsi;
		for (gsi = gsi_start_phis (e->dest);
		     !gsi_end_p (gsi) && !ok;
		     gsi_next (&gsi))
		  {
		    gimple phi = gsi_stmt (gsi);
		    for (z = 0; z < gimple_phi_num_args (phi); z++)
		      if (var == gimple_phi_arg_def (phi, z))
			{
			  ok = true;
			  break;
			}
		  }
		if (ok)
		  continue;
	        num++;
		print_generic_expr (stderr, var, TDF_SLIM);
		fprintf (stderr, " is not marked live-on-entry to entry BB%d ",
			 entry_block);
		fprintf (stderr, "but it is a default def so it should be.\n");
	      }
	}
    }
  gcc_assert (num <= 0);
}
#endif
