/* Callgraph based analysis of static variables.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

/* This file gathers information about how variables whose scope is
   confined to the compilation unit are used.

   The transitive call site specific clobber effects are computed
   for the variables whose scope is contained within this compilation
   unit.

   First each function and static variable initialization is analyzed
   to determine which local static variables are either read, written,
   or have their address taken.  Any local static that has its address
   taken is removed from consideration.  Once the local read and
   writes are determined, a transitive closure of this information is
   performed over the call graph to determine the worst case set of
   side effects of each call.  In later parts of the compiler, these
   local and global sets are examined to make the call clobbering less
   traumatic, promote some statics to registers, and improve aliasing
   information.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "splay-tree.h"
#include "ggc.h"
#include "ipa-utils.h"
#include "ipa-reference.h"
#include "gimple.h"
#include "cgraph.h"
#include "output.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "lto-streamer.h"
#include "toplev.h"

static void remove_node_data (struct cgraph_node *node,
			      void *data ATTRIBUTE_UNUSED);
static void duplicate_node_data (struct cgraph_node *src,
				 struct cgraph_node *dst,
				 void *data ATTRIBUTE_UNUSED);

/* The static variables defined within the compilation unit that are
   loaded or stored directly by function that owns this structure.  */

struct ipa_reference_local_vars_info_d
{
  bitmap statics_read;
  bitmap statics_written;

  /* Set when this function calls another function external to the
     compilation unit or if the function has a asm clobber of memory.
     In general, such calls are modeled as reading and writing all
     variables (both bits on) but sometime there are attributes on the
     called function so we can do better.  */
  bool calls_read_all;
  bool calls_write_all;
};

/* Statics that are read and written by some set of functions. The
   local ones are based on the loads and stores local to the function.
   The global ones are based on the local info as well as the
   transitive closure of the functions that are called. */

struct ipa_reference_global_vars_info_d
{
  bitmap statics_read;
  bitmap statics_written;
};

/* Information we save about every function after ipa-reference is completted.  */

struct ipa_reference_optimization_summary_d
{
  bitmap statics_not_read;
  bitmap statics_not_written;
};

typedef struct ipa_reference_local_vars_info_d *ipa_reference_local_vars_info_t;
typedef struct ipa_reference_global_vars_info_d *ipa_reference_global_vars_info_t;
typedef struct ipa_reference_optimization_summary_d *ipa_reference_optimization_summary_t;

struct ipa_reference_vars_info_d
{
  struct ipa_reference_local_vars_info_d local;
  struct ipa_reference_global_vars_info_d global;
};

typedef struct ipa_reference_vars_info_d *ipa_reference_vars_info_t;

/* This splay tree contains all of the static variables that are
   being considered by the compilation level alias analysis.  */
static splay_tree reference_vars_to_consider;

/* A bit is set for every module static we are considering.  This is
   ored into the local info when asm code is found that clobbers all
   memory. */
static bitmap all_module_statics;

/* Obstack holding bitmaps of local analysis (live from analysis to
   propagation)  */
static bitmap_obstack local_info_obstack;
/* Obstack holding global analysis live forever.  */
static bitmap_obstack optimization_summary_obstack;

/* Holders of ipa cgraph hooks: */
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;

/* Vector where the reference var infos are actually stored. */
DEF_VEC_P (ipa_reference_vars_info_t);
DEF_VEC_ALLOC_P (ipa_reference_vars_info_t, heap);
static VEC (ipa_reference_vars_info_t, heap) *ipa_reference_vars_vector;
DEF_VEC_P (ipa_reference_optimization_summary_t);
DEF_VEC_ALLOC_P (ipa_reference_optimization_summary_t, heap);
static VEC (ipa_reference_optimization_summary_t, heap) *ipa_reference_opt_sum_vector;

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_vars_info_t
get_reference_vars_info (struct cgraph_node *node)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t,
		     ipa_reference_vars_vector) <= (unsigned int) node->uid)
    return NULL;
  return VEC_index (ipa_reference_vars_info_t, ipa_reference_vars_vector,
		    node->uid);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_optimization_summary_t
get_reference_optimization_summary (struct cgraph_node *node)
{
  if (!ipa_reference_opt_sum_vector
      || (VEC_length (ipa_reference_optimization_summary_t,
		     ipa_reference_opt_sum_vector)
	  <= (unsigned int) node->uid))
    return NULL;
  return VEC_index (ipa_reference_optimization_summary_t, ipa_reference_opt_sum_vector,
		    node->uid);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_vars_info (struct cgraph_node *node,
			 ipa_reference_vars_info_t info)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t,
		     ipa_reference_vars_vector) <= (unsigned int) node->uid)
    VEC_safe_grow_cleared (ipa_reference_vars_info_t, heap,
			   ipa_reference_vars_vector, node->uid + 1);
  VEC_replace (ipa_reference_vars_info_t, ipa_reference_vars_vector,
	       node->uid, info);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_optimization_summary (struct cgraph_node *node,
				    ipa_reference_optimization_summary_t info)
{
  if (!ipa_reference_opt_sum_vector
      || (VEC_length (ipa_reference_optimization_summary_t,
		      ipa_reference_opt_sum_vector)
	  <= (unsigned int) node->uid))
    VEC_safe_grow_cleared (ipa_reference_optimization_summary_t,
			   heap, ipa_reference_opt_sum_vector, node->uid + 1);
  VEC_replace (ipa_reference_optimization_summary_t,
	       ipa_reference_opt_sum_vector, node->uid, info);
}

/* Return a bitmap indexed by_DECL_UID uid for the static variables
   that are not read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_not_read_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info;

  info = get_reference_optimization_summary (fn);
  if (info)
    return info->statics_not_read;
  else
    return NULL;
}

/* Return a bitmap indexed by DECL_UID uid for the static variables
   that are not written during the execution of the function FN.  Note
   that variables written may or may not be read during the function
   call.  Returns NULL if no data is available.  */

bitmap
ipa_reference_get_not_written_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info;

  info = get_reference_optimization_summary (fn);
  if (info)
    return info->statics_not_written;
  else
    return NULL;
}



/* Add VAR to all_module_statics and the two
   reference_vars_to_consider* sets.  */

static inline void
add_static_var (tree var)
{
  int uid = DECL_UID (var);
  gcc_assert (TREE_CODE (var) == VAR_DECL);
  if (!bitmap_bit_p (all_module_statics, uid))
    {
      if (dump_file)
	splay_tree_insert (reference_vars_to_consider,
			   uid, (splay_tree_value)var);
      bitmap_set_bit (all_module_statics, uid);
    }
}

/* Return true if the variable T is the right kind of static variable to
   perform compilation unit scope escape analysis.  */

static inline bool
is_proper_for_analysis (tree t)
{
  /* We handle only variables whose address is never taken.  */
  if (TREE_ADDRESSABLE (t))
    return false;
  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    return false;

  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    return false;

  /* We cannot touch decls where the type needs constructing.  */
  if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (t)))
    return false;

  /* This is a variable we care about.  Check if we have seen it
     before, and if not add it the set of variables we care about.  */
  if (all_module_statics
      && !bitmap_bit_p (all_module_statics, DECL_UID (t)))
    add_static_var (t);

  return true;
}

/* Lookup the tree node for the static variable that has UID and
   convert the name to a string for debugging.  */

static const char *
get_static_name (int index)
{
  splay_tree_node stn =
    splay_tree_lookup (reference_vars_to_consider, index);
  if (stn)
    return lang_hooks.decl_printable_name ((tree)(stn->value), 2);
  return NULL;
}

/* Or in all of the bits from every callee of X into X_GLOBAL, the caller's cycle,
   bit vector.  There are several cases to check to avoid the sparse
   bitmap oring.  */

static void
propagate_bits (ipa_reference_global_vars_info_t x_global, struct cgraph_node *x)
{
  struct cgraph_edge *e;
  for (e = x->callees; e; e = e->next_callee)
    {
      struct cgraph_node *y = e->callee;

      /* Only look into nodes we can propagate something.  */
      if (cgraph_function_body_availability (e->callee) > AVAIL_OVERWRITABLE)
	{
	  if (get_reference_vars_info (y))
	    {
	      ipa_reference_vars_info_t y_info
		= get_reference_vars_info (y);
	      ipa_reference_global_vars_info_t y_global = &y_info->global;

	      /* Calls in current cycle do not have global computed yet.  */
	      if (!y_global->statics_read)
		continue;

	      if (x_global->statics_read
		  != all_module_statics)
		{
		  if (y_global->statics_read
		      == all_module_statics)
		    {
		      BITMAP_FREE (x_global->statics_read);
		      x_global->statics_read
			= all_module_statics;
		    }
		  /* Skip bitmaps that are pointer equal to node's bitmap
		     (no reason to spin within the cycle).  */
		  else if (x_global->statics_read
			   != y_global->statics_read)
		    bitmap_ior_into (x_global->statics_read,
				     y_global->statics_read);
		}

	      if (x_global->statics_written
		  != all_module_statics)
		{
		  if (y_global->statics_written
		      == all_module_statics)
		    {
		      BITMAP_FREE (x_global->statics_written);
		      x_global->statics_written
			= all_module_statics;
		    }
		  /* Skip bitmaps that are pointer equal to node's bitmap
		     (no reason to spin within the cycle).  */
		  else if (x_global->statics_written
			   != y_global->statics_written)
		    bitmap_ior_into (x_global->statics_written,
				     y_global->statics_written);
		}
	    }
	  else
	    gcc_unreachable ();
	}
    }
}

/* The init routine for analyzing global static variable usage.  See
   comments at top for description.  */
static void
ipa_init (void)
{
  static bool init_p = false;

  if (init_p)
    return;

  init_p = true;

  if (dump_file)
    reference_vars_to_consider = splay_tree_new (splay_tree_compare_ints, 0, 0);

  bitmap_obstack_initialize (&local_info_obstack);
  bitmap_obstack_initialize (&optimization_summary_obstack);
  all_module_statics = BITMAP_ALLOC (&local_info_obstack);

  node_removal_hook_holder =
      cgraph_add_node_removal_hook (&remove_node_data, NULL);
  node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&duplicate_node_data, NULL);
}


/* Set up the persistent info for FN.  */

static ipa_reference_local_vars_info_t
init_function_info (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info
    = XCNEW (struct ipa_reference_vars_info_d);

  /* Add the info to the tree's annotation.  */
  set_reference_vars_info (fn, info);

  info->local.statics_read = BITMAP_ALLOC (&local_info_obstack);
  info->local.statics_written = BITMAP_ALLOC (&local_info_obstack);

  return &info->local;
}


/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static void
analyze_function (struct cgraph_node *fn)
{
  ipa_reference_local_vars_info_t local;
  struct ipa_ref *ref;
  int i;
  tree var;
  struct cgraph_edge *ie;

  local = init_function_info (fn);
  /* Process indirect calls.  All direct calles are handled at propagation
     time.  */
  for (ie = fn->indirect_calls; ie; ie = ie->next_callee)
    if (!(ie->indirect_info->ecf_flags & ECF_CONST))
      {
        local->calls_read_all = true;
        if (!(ie->indirect_info->ecf_flags & ECF_PURE)
	    && ((ie->indirect_info->ecf_flags & (ECF_NOTHROW | ECF_NORETURN))
		!=  (ECF_NOTHROW | ECF_NORETURN)))
	  local->calls_write_all = true;
      }
  for (i = 0; ipa_ref_list_reference_iterate (&fn->ref_list, i, ref); i++)
    {
      if (ref->refered_type != IPA_REF_VARPOOL)
	continue;
      var = ipa_ref_varpool_node (ref)->decl;
      if (ipa_ref_varpool_node (ref)->externally_visible
	  || !ipa_ref_varpool_node (ref)->analyzed
	  || !is_proper_for_analysis (var))
	continue;
      switch (ref->use)
	{
	case IPA_REF_LOAD:
          bitmap_set_bit (local->statics_read, DECL_UID (var));
	  break;
	case IPA_REF_STORE:
          bitmap_set_bit (local->statics_written, DECL_UID (var));
	  break;
	case IPA_REF_ADDR:
	  break;
	}
    }

  if ((flags_from_decl_or_type (fn->decl) & (ECF_NOTHROW | ECF_NORETURN))
      == (ECF_NOTHROW | ECF_NORETURN))
    {
      local->calls_write_all = false;
      bitmap_clear (local->statics_written);
    }

  /* Free bitmaps of direct references if we can not use them anyway.  */
  if (local->calls_write_all)
    BITMAP_FREE (local->statics_written);
  if (local->calls_read_all)
    BITMAP_FREE (local->statics_read);
}

static bitmap
copy_global_bitmap (bitmap src)
{
  bitmap dst;
  if (!src)
    return NULL;
  dst = BITMAP_ALLOC (&optimization_summary_obstack);
  bitmap_copy (dst, src);
  return dst;
}


/* Called when new clone is inserted to callgraph late.  */

static void
duplicate_node_data (struct cgraph_node *src, struct cgraph_node *dst,
	 	     void *data ATTRIBUTE_UNUSED)
{
  ipa_reference_optimization_summary_t ginfo;
  ipa_reference_optimization_summary_t dst_ginfo;

  ginfo = get_reference_optimization_summary (src);
  if (!ginfo)
    return;
  dst_ginfo = XCNEW (struct ipa_reference_optimization_summary_d);
  set_reference_optimization_summary (dst, dst_ginfo);
  dst_ginfo->statics_not_read = copy_global_bitmap (ginfo->statics_not_read);
  dst_ginfo->statics_not_written = copy_global_bitmap (ginfo->statics_not_written);
}

/* Called when node is removed.  */

static void
remove_node_data (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  ipa_reference_optimization_summary_t ginfo;
  ginfo = get_reference_optimization_summary (node);
  if (ginfo)
    {
      if (ginfo->statics_not_read
	  && ginfo->statics_not_read != all_module_statics)
	BITMAP_FREE (ginfo->statics_not_read);

      if (ginfo->statics_not_written
	  && ginfo->statics_not_written != all_module_statics)
	BITMAP_FREE (ginfo->statics_not_written);
      free (ginfo);
      set_reference_optimization_summary (node, NULL);
    }
}

/* Analyze each function in the cgraph to see which global or statics
   are read or written.  */

static void
generate_summary (void)
{
  struct cgraph_node *node;
  unsigned int index;
  bitmap_iterator bi;
  bitmap bm_temp;

  ipa_init ();
  bm_temp = BITMAP_ALLOC (&local_info_obstack);

  /* Process all of the functions next.  */
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      analyze_function (node);

  if (dump_file)
    EXECUTE_IF_SET_IN_BITMAP (all_module_statics, 0, index, bi)
      {
	fprintf (dump_file, "\nPromotable global:%s",
		 get_static_name (index));
      }

  BITMAP_FREE(bm_temp);

  if (dump_file)
    for (node = cgraph_nodes; node; node = node->next)
      if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
	{
	  ipa_reference_local_vars_info_t l;
	  unsigned int index;
	  bitmap_iterator bi;

	  l = &get_reference_vars_info (node)->local;
	  fprintf (dump_file,
		   "\nFunction name:%s/%i:",
		   cgraph_node_name (node), node->uid);
	  fprintf (dump_file, "\n  locals read: ");
	  if (l->statics_read)
	    EXECUTE_IF_SET_IN_BITMAP (l->statics_read,
				      0, index, bi)
	      {
	        fprintf (dump_file, "%s ",
		         get_static_name (index));
	      }
	  fprintf (dump_file, "\n  locals written: ");
	  if (l->statics_written)
	    EXECUTE_IF_SET_IN_BITMAP (l->statics_written,
				      0, index, bi)
	      {
	        fprintf(dump_file, "%s ",
		        get_static_name (index));
	      }
	  if (l->calls_read_all)
	     fprintf (dump_file, "\n  calls read all: ");
	  if (l->calls_write_all)
	     fprintf (dump_file, "\n  calls read all: ");
	}
}

/* Set READ_ALL/WRITE_ALL based on DECL flags.  */

static void
read_write_all_from_decl (tree decl, bool * read_all, bool * write_all)
{
  int flags = flags_from_decl_or_type (decl);
  if (flags & ECF_CONST)
    ;
  else if (flags & ECF_PURE)
    *read_all = true;
  else
    {
       /* TODO: To be able to produce sane results, we should also handle
	  common builtins, in particular throw.  */
      *read_all = true;
      /* When function does not return, it is safe to ignore anythign it writes
	 to, because the effect will never happen.  */
      if ((flags & (ECF_NOTHROW | ECF_NORETURN))
	  != (ECF_NOTHROW | ECF_NORETURN))
        *write_all = true;
    }
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by ipa_analyze_function */

static unsigned int
propagate (void)
{
  struct cgraph_node *node;
  struct cgraph_node *w;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  int order_pos = ipa_utils_reduced_inorder (order, false, true, NULL);
  int i;

  if (dump_file)
    dump_cgraph (dump_file);

  ipa_discover_readonly_nonaddressable_vars ();
  generate_summary ();

  /* Propagate the local information thru the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  order_pos = ipa_utils_reduced_inorder (order, true, true, NULL);
  if (dump_file)
    ipa_utils_print_order(dump_file, "reduced", order, order_pos);

  for (i = 0; i < order_pos; i++ )
    {
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;
      ipa_reference_local_vars_info_t node_l;
      struct cgraph_edge *e;

      bool read_all;
      bool write_all;
      struct ipa_dfs_info * w_info;

      node = order[i];
      node_info = get_reference_vars_info (node);
      if (!node_info)
	{
	  dump_cgraph_node (stderr, node);
	  dump_cgraph (stderr);
	  gcc_unreachable ();
	}

      node_l = &node_info->local;
      node_g = &node_info->global;

      read_all = node_l->calls_read_all;
      write_all = node_l->calls_write_all;

      /* When function is overwrittable, we can not assume anything.  */
      if (cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
        read_write_all_from_decl (node->decl, &read_all, &write_all);

      for (e = node->callees; e; e = e->next_callee)
        if (cgraph_function_body_availability (e->callee) <= AVAIL_OVERWRITABLE)
          read_write_all_from_decl (e->callee->decl, &read_all, &write_all);


      /* If any node in a cycle is calls_read_all or calls_write_all
	 they all are. */
      w_info = (struct ipa_dfs_info *) node->aux;
      w = w_info->next_cycle;
      while (w)
	{
	  ipa_reference_local_vars_info_t w_l =
	    &get_reference_vars_info (w)->local;

	  /* When function is overwrittable, we can not assume anything.  */
	  if (cgraph_function_body_availability (w) <= AVAIL_OVERWRITABLE)
	    read_write_all_from_decl (w->decl, &read_all, &write_all);

	  for (e = w->callees; e; e = e->next_callee)
	    if (cgraph_function_body_availability (e->callee) <= AVAIL_OVERWRITABLE)
	      read_write_all_from_decl (e->callee->decl, &read_all, &write_all);

	  read_all |= w_l->calls_read_all;
	  write_all |= w_l->calls_write_all;

	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}


      /* Initialized the bitmaps for the reduced nodes */
      if (read_all)
	node_g->statics_read = all_module_statics;
      else
	{
	  node_g->statics_read = BITMAP_ALLOC (&local_info_obstack);
	  bitmap_copy (node_g->statics_read,
		       node_l->statics_read);
	}
      if (write_all)
	node_g->statics_written = all_module_statics;
      else
	{
	  node_g->statics_written = BITMAP_ALLOC (&local_info_obstack);
	  bitmap_copy (node_g->statics_written,
		       node_l->statics_written);
	}

      propagate_bits (node_g, node);
      w_info = (struct ipa_dfs_info *) node->aux;
      w = w_info->next_cycle;
      while (w)
	{
	  ipa_reference_vars_info_t w_ri =
	    get_reference_vars_info (w);
	  ipa_reference_local_vars_info_t w_l = &w_ri->local;

	  /* These global bitmaps are initialized from the local info
	     of all of the nodes in the region.  However there is no
	     need to do any work if the bitmaps were set to
	     all_module_statics.  */
	  if (!read_all)
	    bitmap_ior_into (node_g->statics_read,
			     w_l->statics_read);
	  if (!write_all)
	    bitmap_ior_into (node_g->statics_written,
			     w_l->statics_written);
	  propagate_bits (node_g, w);
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* All nodes within a cycle have the same global info bitmaps.  */
      node_info->global = *node_g;
      w_info = (struct ipa_dfs_info *) node->aux;
      w = w_info->next_cycle;
      while (w)
	{
	  ipa_reference_vars_info_t w_ri =
	    get_reference_vars_info (w);

          w_ri->global = *node_g;

	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
    }

  if (dump_file)
    {
      for (i = 0; i < order_pos; i++ )
	{
	  ipa_reference_vars_info_t node_info;
	  ipa_reference_global_vars_info_t node_g;
	  ipa_reference_local_vars_info_t node_l;
	  unsigned int index;
	  bitmap_iterator bi;
	  struct ipa_dfs_info * w_info;

	  node = order[i];
	  node_info = get_reference_vars_info (node);
	  node_g = &node_info->global;
	  node_l = &node_info->local;
	  fprintf (dump_file,
		   "\nFunction name:%s/%i:",
		   cgraph_node_name (node), node->uid);
	  fprintf (dump_file, "\n  locals read: ");
	  if (node_l->statics_read)
	    EXECUTE_IF_SET_IN_BITMAP (node_l->statics_read,
				      0, index, bi)
	      {
		fprintf (dump_file, "%s ",
			 get_static_name (index));
	      }
	  fprintf (dump_file, "\n  locals written: ");
	  if (node_l->statics_written)
	    EXECUTE_IF_SET_IN_BITMAP (node_l->statics_written,
				      0, index, bi)
	      {
		fprintf(dump_file, "%s ",
			get_static_name (index));
	      }

	  w_info = (struct ipa_dfs_info *) node->aux;
	  w = w_info->next_cycle;
	  while (w)
	    {
	      ipa_reference_vars_info_t w_ri =
		get_reference_vars_info (w);
	      ipa_reference_local_vars_info_t w_l = &w_ri->local;
	      fprintf (dump_file, "\n  next cycle: %s/%i ",
		       cgraph_node_name (w), w->uid);
	      fprintf (dump_file, "\n    locals read: ");
	      if (w_l->statics_read)
		EXECUTE_IF_SET_IN_BITMAP (w_l->statics_read,
					  0, index, bi)
		  {
		    fprintf (dump_file, "%s ",
			     get_static_name (index));
		  }

	      fprintf (dump_file, "\n    locals written: ");
	      if (w_l->statics_written)
		EXECUTE_IF_SET_IN_BITMAP (w_l->statics_written,
					  0, index, bi)
		  {
		    fprintf (dump_file, "%s ",
			     get_static_name (index));
		  }

	      w_info = (struct ipa_dfs_info *) w->aux;
	      w = w_info->next_cycle;
	    }
	  fprintf (dump_file, "\n  globals read: ");
	  if (node_g->statics_read == all_module_statics)
	    fprintf (dump_file, "ALL");
	  else
	    EXECUTE_IF_SET_IN_BITMAP (node_g->statics_read,
				      0, index, bi)
	      {
	        fprintf (dump_file, "%s ",
		         get_static_name (index));
	      }
	  fprintf (dump_file, "\n  globals written: ");
	  if (node_g->statics_written == all_module_statics)
	    fprintf (dump_file, "ALL");
	  else
	    EXECUTE_IF_SET_IN_BITMAP (node_g->statics_written,
				      0, index, bi)
	      {
		fprintf (dump_file, "%s ",
			 get_static_name (index));
	      }
	}
    }

  /* Cleanup. */
  for (node = cgraph_nodes; node; node = node->next)
    {
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;
      ipa_reference_optimization_summary_t opt;

      if (!node->analyzed)
        continue;

      node_info = get_reference_vars_info (node);
      if (cgraph_function_body_availability (node) > AVAIL_OVERWRITABLE)
	{
	  node_g = &node_info->global;

	  opt = XCNEW (struct ipa_reference_optimization_summary_d);
	  set_reference_optimization_summary (node, opt);

	  /* Create the complimentary sets.  */
	  opt->statics_not_read = BITMAP_ALLOC (&optimization_summary_obstack);
	  opt->statics_not_written = BITMAP_ALLOC (&optimization_summary_obstack);

	  if (node_g->statics_read != all_module_statics)
	    bitmap_and_compl (opt->statics_not_read,
			      all_module_statics,
			      node_g->statics_read);

	  if (node_g->statics_written
	      != all_module_statics)
	    bitmap_and_compl (opt->statics_not_written,
			      all_module_statics,
			      node_g->statics_written);
	}
      if (node_info)
	free (node_info);
      if (node->aux)
	{
	  free (node->aux);
	  node->aux = NULL;
	}
   }

  free (order);

  bitmap_obstack_release (&local_info_obstack);
  VEC_free (ipa_reference_vars_info_t, heap, ipa_reference_vars_vector);
  ipa_reference_vars_vector = NULL;
  if (dump_file)
    splay_tree_delete (reference_vars_to_consider);
  reference_vars_to_consider = NULL;
  all_module_statics = NULL;
  return 0;
}

/* Return true if we need to write summary of NODE. */

static bool
write_node_summary_p (struct cgraph_node *node,
		      cgraph_node_set set,
		      varpool_node_set vset,
		      bitmap ltrans_statics)
{
  ipa_reference_optimization_summary_t info;

  /* See if we have (non-empty) info.  */
  if (!node->analyzed || node->global.inlined_to)
    return false;
  info = get_reference_optimization_summary (node);
  if (!info || (bitmap_empty_p (info->statics_not_read)
		&& bitmap_empty_p (info->statics_not_written)))
    return false;

  /* See if we want to encode it.
     Encode also referenced functions since constant folding might turn it into
     a direct call.

     In future we might also want to include summaries of functions references
     by initializers of constant variables references in current unit.  */
  if (!reachable_from_this_partition_p (node, set)
      && !referenced_from_this_partition_p (&node->ref_list, set, vset))
    return false;

  /* See if the info has non-empty intersections with vars we want to encode.  */
  if (!bitmap_intersect_p (info->statics_not_read, ltrans_statics)
      && !bitmap_intersect_p (info->statics_not_written, ltrans_statics))
    return false;
  return true;
}

/* Stream out BITS&LTRANS_STATICS as list of decls to OB.  */

static void
stream_out_bitmap (struct lto_simple_output_block *ob,
		   bitmap bits, bitmap ltrans_statics)
{
  unsigned int count = 0;
  unsigned int index;
  bitmap_iterator bi;
  EXECUTE_IF_AND_IN_BITMAP (bits, ltrans_statics, 0, index, bi)
    count ++;
  lto_output_uleb128_stream (ob->main_stream, count);
  if (!count)
    return;
  EXECUTE_IF_AND_IN_BITMAP (bits, ltrans_statics, 0, index, bi)
    {
      tree decl = (tree)splay_tree_lookup (reference_vars_to_consider, index)->value;
      lto_output_var_decl_index(ob->decl_state, ob->main_stream, decl);
    }
}

/* Serialize the ipa info for lto.  */

static void
ipa_reference_write_optimization_summary (cgraph_node_set set,
					  varpool_node_set vset)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_reference);
  unsigned int count = 0;
  lto_cgraph_encoder_t encoder = ob->decl_state->cgraph_node_encoder;
  bitmap ltrans_statics = BITMAP_ALLOC (NULL);

  reference_vars_to_consider = splay_tree_new (splay_tree_compare_ints, 0, 0);

  /* See what variables we are interested in.  */
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    if (referenced_from_this_partition_p (&vnode->ref_list, set, vset))
      {
	tree decl = vnode->decl;
	if (is_proper_for_analysis (decl))
	  {
	    bitmap_set_bit (ltrans_statics, DECL_UID (decl));
	    splay_tree_insert (reference_vars_to_consider,
			       DECL_UID (decl), (splay_tree_value)decl);
	  }
      }

  for (node = cgraph_nodes; node; node = node->next)
    if (write_node_summary_p (node, set, vset, ltrans_statics))
	count++;

  lto_output_uleb128_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (node = cgraph_nodes; node; node = node->next)
    if (write_node_summary_p (node, set, vset, ltrans_statics))
      {
	ipa_reference_optimization_summary_t info;
	int node_ref;

	info = get_reference_optimization_summary (node);
	node_ref = lto_cgraph_encoder_encode (encoder, node);
	lto_output_uleb128_stream (ob->main_stream, node_ref);

	stream_out_bitmap (ob, info->statics_not_read, ltrans_statics);
	stream_out_bitmap (ob, info->statics_not_written, ltrans_statics);
      }
  BITMAP_FREE (ltrans_statics);
  lto_destroy_simple_output_block (ob);
  splay_tree_delete (reference_vars_to_consider);
}

/* Deserialize the ipa info for lto.  */

static void
ipa_reference_read_optimization_summary (void)
{
  struct lto_file_decl_data ** file_data_vec
    = lto_get_file_decl_data ();
  struct lto_file_decl_data * file_data;
  unsigned int j = 0;
  bitmap_obstack_initialize (&optimization_summary_obstack);

  node_removal_hook_holder =
      cgraph_add_node_removal_hook (&remove_node_data, NULL);
  node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&duplicate_node_data, NULL);

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      struct lto_input_block *ib
	= lto_create_simple_input_block (file_data,
					 LTO_section_ipa_reference,
					 &data, &len);
      if (ib)
	{
	  unsigned int i;
	  unsigned int f_count = lto_input_uleb128 (ib);

	  for (i = 0; i < f_count; i++)
	    {
	      unsigned int j, index;
	      struct cgraph_node *node;
	      ipa_reference_optimization_summary_t info;
	      int v_count;
	      lto_cgraph_encoder_t encoder;

	      index = lto_input_uleb128 (ib);
	      encoder = file_data->cgraph_node_encoder;
	      node = lto_cgraph_encoder_deref (encoder, index);
	      info = XCNEW (struct ipa_reference_optimization_summary_d);
	      set_reference_optimization_summary (node, info);
	      info->statics_not_read = BITMAP_ALLOC (&optimization_summary_obstack);
	      info->statics_not_written = BITMAP_ALLOC (&optimization_summary_obstack);
	      if (dump_file)
		fprintf (dump_file,
			 "\nFunction name:%s/%i:\n  static not read:",
			 cgraph_node_name (node), node->uid);

	      /* Set the statics not read.  */
	      v_count = lto_input_uleb128 (ib);
	      for (j = 0; j < (unsigned int)v_count; j++)
		{
		  unsigned int var_index = lto_input_uleb128 (ib);
		  tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								 var_index);
		  bitmap_set_bit (info->statics_not_read, DECL_UID (v_decl));
		  if (dump_file)
		    fprintf (dump_file, " %s",
			     lang_hooks.decl_printable_name (v_decl, 2));
		}

	      if (dump_file)
		fprintf (dump_file,
			 "\n  static not written:");
	      /* Set the statics not written.  */
	      v_count = lto_input_uleb128 (ib);
	      for (j = 0; j < (unsigned int)v_count; j++)
		{
		  unsigned int var_index = lto_input_uleb128 (ib);
		  tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								 var_index);
		  bitmap_set_bit (info->statics_not_written, DECL_UID (v_decl));
		  if (dump_file)
		    fprintf (dump_file, " %s",
			     lang_hooks.decl_printable_name (v_decl, 2));
		}
	      if (dump_file)
		fprintf (dump_file, "\n");
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_reference,
					  ib, data, len);
	}
      else
	/* Fatal error here.  We do not want to support compiling ltrans units with
	   different version of compiler or different flags than the WPA unit, so
	   this should never happen.  */
	fatal_error ("ipa reference summary is missing in ltrans unit");
    }
}

static bool
gate_reference (void)
{
  return (flag_ipa_reference
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

struct ipa_opt_pass_d pass_ipa_reference =
{
 {
  IPA_PASS,
  "static-var",				/* name */
  gate_reference,			/* gate */
  propagate,			        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_REFERENCE,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,				        /* generate_summary */
 NULL,					/* write_summary */
 NULL,				 	/* read_summary */
 ipa_reference_write_optimization_summary,/* write_optimization_summary */
 ipa_reference_read_optimization_summary,/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};
