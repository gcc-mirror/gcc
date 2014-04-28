/* Callgraph based analysis of static variables.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.
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
#include "calls.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "splay-tree.h"
#include "ipa-utils.h"
#include "ipa-reference.h"
#include "flags.h"
#include "diagnostic.h"
#include "data-streamer.h"
#include "lto-streamer.h"

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

/* Information we save about every function after ipa-reference is completed.  */

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

/* Set of all interesting module statics.  A bit is set for every module
   static we are considering.  This is added to the local info when asm
   code is found that clobbers all memory.  */
static bitmap all_module_statics;

/* Obstack holding bitmaps of local analysis (live from analysis to
   propagation)  */
static bitmap_obstack local_info_obstack;
/* Obstack holding global analysis live forever.  */
static bitmap_obstack optimization_summary_obstack;

/* Holders of ipa cgraph hooks: */
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;

/* Vector where the reference var infos are actually stored. 
   Indexed by UID of call graph nodes.  */
static vec<ipa_reference_vars_info_t> ipa_reference_vars_vector;

static vec<ipa_reference_optimization_summary_t> ipa_reference_opt_sum_vector;

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_vars_info_t
get_reference_vars_info (struct cgraph_node *node)
{
  if (!ipa_reference_vars_vector.exists ()
      || ipa_reference_vars_vector.length () <= (unsigned int) node->uid)
    return NULL;
  return ipa_reference_vars_vector[node->uid];
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_optimization_summary_t
get_reference_optimization_summary (struct cgraph_node *node)
{
  if (!ipa_reference_opt_sum_vector.exists ()
      || (ipa_reference_opt_sum_vector.length () <= (unsigned int) node->uid))
    return NULL;
  return ipa_reference_opt_sum_vector[node->uid];
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_vars_info (struct cgraph_node *node,
			 ipa_reference_vars_info_t info)
{
  if (!ipa_reference_vars_vector.exists ()
      || ipa_reference_vars_vector.length () <= (unsigned int) node->uid)
    ipa_reference_vars_vector.safe_grow_cleared (node->uid + 1);
  ipa_reference_vars_vector[node->uid] = info;
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_optimization_summary (struct cgraph_node *node,
				    ipa_reference_optimization_summary_t info)
{
  if (!ipa_reference_opt_sum_vector.exists ()
      || (ipa_reference_opt_sum_vector.length () <= (unsigned int) node->uid))
    ipa_reference_opt_sum_vector.safe_grow_cleared (node->uid + 1);
  ipa_reference_opt_sum_vector[node->uid] = info;
}

/* Return a bitmap indexed by DECL_UID for the static variables that
   are *not* read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_not_read_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info =
    get_reference_optimization_summary (cgraph_function_node (fn, NULL));
  if (info)
    return info->statics_not_read;
  else if (flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return all_module_statics;
  else
    return NULL;
}

/* Return a bitmap indexed by DECL_UID for the static variables that
   are *not* written during the execution of the function FN.  Note
   that variables written may or may not be read during the function
   call.  Returns NULL if no data is available.  */

bitmap
ipa_reference_get_not_written_global (struct cgraph_node *fn)
{
  ipa_reference_optimization_summary_t info =
    get_reference_optimization_summary (fn);
  if (info)
    return info->statics_not_written;
  else if (flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return all_module_statics;
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
  if (dump_file)
    splay_tree_insert (reference_vars_to_consider,
		       uid, (splay_tree_value)var);
  bitmap_set_bit (all_module_statics, uid);
}

/* Return true if the variable T is the right kind of static variable to
   perform compilation unit scope escape analysis.  */

static inline bool
is_proper_for_analysis (tree t)
{
  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    return false;

  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    return false;

  /* We do not need to analyze readonly vars, we already know they do not
     alias.  */
  if (TREE_READONLY (t))
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
  return fndecl_name ((tree)(stn->value));
}

/* Dump a set of static vars to FILE.  */
static void
dump_static_vars_set_to_file (FILE *f, bitmap set)
{
  unsigned int index;
  bitmap_iterator bi;
  if (set == NULL)
    return;
  else if (set == all_module_statics)
    fprintf (f, "ALL");
  else
    EXECUTE_IF_SET_IN_BITMAP (set, 0, index, bi)
      {
        fprintf (f, "%s ", get_static_name (index));
      }
}

/* Compute X |= Y, taking into account the possibility that
   either X or Y is already the maximum set.
   Return true if X is the maximum set after taking the union with Y.  */

static bool
union_static_var_sets (bitmap &x, bitmap y)
{
  if (x != all_module_statics)
    {
      if (y == all_module_statics)
	{
	  BITMAP_FREE (x);
	  x = all_module_statics;
	}
      else if (bitmap_ior_into (x, y))
	{
	  /* The union may have reduced X to the maximum set.
	     In that case, we want to make that visible explicitly.
	     Even though bitmap_equal_p can be very expensive, it
	     turns out to be an overall win to check this here for
	     an LTO bootstrap of GCC itself.  Liberally extrapoliate
	     that result to be applicable to all cases.  */
	  if (bitmap_equal_p (x, all_module_statics))
	    {
	      BITMAP_FREE (x);
	      x = all_module_statics;
	    }
	}
    }
  return x == all_module_statics;
}

/* Compute X &= Y, taking into account the possibility that
   X may become the maximum set.  */

static bool
intersect_static_var_sets (bitmap &x, bitmap y)
{
  if (x != all_module_statics)
    {
      bitmap_and_into (x, y);
      /* As with union_static_var_sets, reducing to the maximum
	 set as early as possible is an overall win.  */
      if (bitmap_equal_p (x, all_module_statics))
	{
	  BITMAP_FREE (x);
	  x = all_module_statics;
	}
    }
  return x == all_module_statics;
}

/* Return a copy of SET on the bitmap obstack containing SET.
   But if SET is NULL or the maximum set, return that instead.  */

static bitmap
copy_static_var_set (bitmap set)
{
  if (set == NULL || set == all_module_statics)
    return set;
  bitmap_obstack *o = set->obstack;
  gcc_checking_assert (o);
  bitmap copy = BITMAP_ALLOC (o);
  bitmap_copy (copy, set);
  return copy;
}

/* Compute the union all of the statics read and written by every callee of X
   into X_GLOBAL->statics_read and X_GLOBAL->statics_written.  X_GLOBAL is
   actually the set representing the cycle containing X.  If the read and
   written sets of X_GLOBAL has been reduced to the maximum set, we don't
   have to look at the remaining callees.  */

static void
propagate_bits (ipa_reference_global_vars_info_t x_global, struct cgraph_node *x)
{
  struct cgraph_edge *e;
  bool read_all = x_global->statics_read == all_module_statics;
  bool write_all = x_global->statics_written == all_module_statics;
  for (e = x->callees;
       e && !(read_all && write_all);
       e = e->next_callee)
    {
      enum availability avail;
      struct cgraph_node *y = cgraph_function_node (e->callee, &avail);
      if (!y)
	continue;

      /* Only look into nodes we can propagate something.  */
      int flags = flags_from_decl_or_type (y->decl);
      if (avail > AVAIL_OVERWRITABLE
	  || (avail == AVAIL_OVERWRITABLE && (flags & ECF_LEAF)))
	{
	  if (get_reference_vars_info (y))
	    {
	      ipa_reference_vars_info_t y_info = get_reference_vars_info (y);
	      ipa_reference_global_vars_info_t y_global = &y_info->global;

	      /* Calls in the current cycle do not have their global set
		 computed yet (but everything else does because we're
		 visiting nodes in topological order).  */
	      if (!y_global->statics_read)
		continue;

	      /* If the function is const, it reads no memory even if it
		 seems so to local analysis.  */
	      if (flags & ECF_CONST)
		continue;

	      union_static_var_sets (x_global->statics_read,
				     y_global->statics_read);

	      /* If the function is pure, it has no stores even if it
		 seems so to local analysis.  If we cannot return from
		 the function, we can safely ignore the call.  */
	      if ((flags & ECF_PURE)
		  || cgraph_edge_cannot_lead_to_return (e))
		continue;

	      union_static_var_sets (x_global->statics_written,
				     y_global->statics_written);
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
  all_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);

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

  local = init_function_info (fn);
  for (i = 0; ipa_ref_list_reference_iterate (&fn->ref_list, i, ref); i++)
    {
      if (!is_a <varpool_node *> (ref->referred))
	continue;
      var = ipa_ref_varpool_node (ref)->decl;
      if (!is_proper_for_analysis (var))
	continue;
      switch (ref->use)
	{
	case IPA_REF_LOAD:
          bitmap_set_bit (local->statics_read, DECL_UID (var));
	  break;
	case IPA_REF_STORE:
	  if (ipa_ref_cannot_lead_to_return (ref))
	    break;
          bitmap_set_bit (local->statics_written, DECL_UID (var));
	  break;
	case IPA_REF_ADDR:
	  break;
	}
    }

  if (cgraph_node_cannot_return (fn))
    bitmap_clear (local->statics_written);
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
  dst_ginfo->statics_not_read =
    copy_static_var_set (ginfo->statics_not_read);
  dst_ginfo->statics_not_written =
    copy_static_var_set (ginfo->statics_not_written);
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

  ipa_init ();

  /* Process all of the functions next.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    analyze_function (node);

  if (dump_file)
    EXECUTE_IF_SET_IN_BITMAP (all_module_statics, 0, index, bi)
      {
	fprintf (dump_file, "\nPromotable global:%s (uid=%u)\n",
		 get_static_name (index), index);
      }

  if (dump_file)
    FOR_EACH_DEFINED_FUNCTION (node)
      if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
	{
	  ipa_reference_local_vars_info_t l;
	  unsigned int index;
	  bitmap_iterator bi;

	  l = &get_reference_vars_info (node)->local;
	  fprintf (dump_file,
		   "\nFunction name:%s/%i:",
		   node->asm_name (), node->order);
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
	        fprintf (dump_file, "%s ", get_static_name (index));
	      }
	}
}

/* Set READ_ALL/WRITE_ALL based on decl flags of NODE.  */

static void
read_write_all_from_decl (struct cgraph_node *node,
			  bool &read_all, bool &write_all)
{
  tree decl = node->decl;
  int flags = flags_from_decl_or_type (decl);
  if ((flags & ECF_LEAF)
      && cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
    ;
  else if (flags & ECF_CONST)
    ;
  else if ((flags & ECF_PURE)
	   || cgraph_node_cannot_return (node))
    {
      read_all = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
         fprintf (dump_file, "   %s/%i -> read all\n",
		  node->asm_name (), node->order);
    }
  else
    {
       /* TODO: To be able to produce sane results, we should also handle
	  common builtins, in particular throw.  */
      read_all = true;
      write_all = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
         fprintf (dump_file, "   %s/%i -> read all, write all\n",
		  node->asm_name (), node->order);
    }
}

/* Set READ_ALL/WRITE_ALL based on decl flags of NODE or any member
   in the cycle of NODE.  */

static void
get_read_write_all_from_node (struct cgraph_node *node,
			      bool &read_all, bool &write_all)
{
  struct cgraph_edge *e, *ie;

  /* When function is overwritable, we can not assume anything.  */
  if (cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
    read_write_all_from_decl (node, read_all, write_all);

  for (e = node->callees;
       e && !(read_all && write_all);
       e = e->next_callee)
    {
      enum availability avail;
      struct cgraph_node *callee = cgraph_function_node (e->callee, &avail);
      gcc_checking_assert (callee);
      if (avail <= AVAIL_OVERWRITABLE)
	read_write_all_from_decl (callee, read_all, write_all);
    }

  for (ie = node->indirect_calls;
       ie && !(read_all && write_all);
       ie = ie->next_callee)
    if (!(ie->indirect_info->ecf_flags & ECF_CONST))
      {
	read_all = true;
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "   indirect call -> read all\n");
	if (!cgraph_edge_cannot_lead_to_return (ie)
	    && !(ie->indirect_info->ecf_flags & ECF_PURE))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "   indirect call -> write all\n");
	    write_all = true;
	  }
      }
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by ipa_analyze_function.  */

static unsigned int
propagate (void)
{
  struct cgraph_node *node;
  varpool_node *vnode;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  int order_pos;
  int i;

  if (dump_file)
    dump_cgraph (dump_file);

  ipa_discover_readonly_nonaddressable_vars ();
  generate_summary ();

  /* Now we know what vars are really statics; prune out those that aren't.  */
  FOR_EACH_VARIABLE (vnode)
    if (vnode->externally_visible
	|| TREE_ADDRESSABLE (vnode->decl)
	|| TREE_READONLY (vnode->decl)
	|| !is_proper_for_analysis (vnode->decl)
	|| !vnode->definition)
      bitmap_clear_bit (all_module_statics, DECL_UID (vnode->decl));

  /* Forget info we collected "just for fun" on variables that turned out to be
     non-local.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      ipa_reference_local_vars_info_t node_l;
      node_l = &get_reference_vars_info (node)->local;
      intersect_static_var_sets (node_l->statics_read, all_module_statics);
      intersect_static_var_sets (node_l->statics_written, all_module_statics);
    }

  /* Propagate the local information through the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  order_pos = ipa_reduced_postorder (order, true, true, NULL);
  if (dump_file)
    ipa_print_order (dump_file, "reduced", order, order_pos);

  for (i = 0; i < order_pos; i++ )
    {
      unsigned x;
      struct cgraph_node *w;
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;
      ipa_reference_local_vars_info_t node_l;
      bool read_all = false;
      bool write_all = false;

      node = order[i];
      if (node->alias)
	continue;

      node_info = get_reference_vars_info (node);
      gcc_assert (node_info);
      node_l = &node_info->local;
      node_g = &node_info->global;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting cycle with %s/%i\n",
		  node->asm_name (), node->order);

      vec<cgraph_node_ptr> cycle_nodes = ipa_get_nodes_in_cycle (node);

      /* If any node in a cycle is read_all or write_all, they all are.  */
      FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Visiting %s/%i\n",
		     w->asm_name (), w->order);
	  get_read_write_all_from_node (w, read_all, write_all);
	  if (read_all && write_all)
	    break;
	}

      /* Initialized the bitmaps global sets for the reduced node.  */
      if (read_all)
	node_g->statics_read = all_module_statics;
      else
	node_g->statics_read = copy_static_var_set (node_l->statics_read);
      if (write_all)
	node_g->statics_written = all_module_statics;
      else
	node_g->statics_written = copy_static_var_set (node_l->statics_written);

      /* Merge the sets of this cycle with all sets of callees reached
         from this cycle.  */
      FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	{
	  if (read_all && write_all)
	    break;

	  if (w != node)
	    {
	      ipa_reference_vars_info_t w_ri = get_reference_vars_info (w);
	      ipa_reference_local_vars_info_t w_l = &w_ri->local;
	      int flags = flags_from_decl_or_type (w->decl);

	      if (!(flags & ECF_CONST))
		read_all = union_static_var_sets (node_g->statics_read,
						  w_l->statics_read);
	      if (!(flags & ECF_PURE)
		  && !cgraph_node_cannot_return (w))
		write_all = union_static_var_sets (node_g->statics_written,
						   w_l->statics_written);
	    }

	  propagate_bits (node_g, w);
	}

      /* All nodes within a cycle have the same global info bitmaps.  */
      FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	{
	  ipa_reference_vars_info_t w_ri = get_reference_vars_info (w);
          w_ri->global = *node_g;
	}

      cycle_nodes.release ();
    }

  if (dump_file)
    {
      for (i = 0; i < order_pos; i++)
	{
	  unsigned x;
	  struct cgraph_node *w;

	  node = order[i];
	  if (node->alias)
	    continue;

	  fprintf (dump_file,
		   "\nFunction name:%s/%i:",
		   node->asm_name (), node->order);

	  ipa_reference_vars_info_t node_info = get_reference_vars_info (node);
	  ipa_reference_global_vars_info_t node_g = &node_info->global;

	  vec<cgraph_node_ptr> cycle_nodes = ipa_get_nodes_in_cycle (node);
	  FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	    {
	      ipa_reference_vars_info_t w_ri = get_reference_vars_info (w);
	      ipa_reference_local_vars_info_t w_l = &w_ri->local;
	      if (w != node)
		fprintf (dump_file, "\n  next cycle: %s/%i ",
			 w->asm_name (), w->order);
	      fprintf (dump_file, "\n    locals read: ");
	      dump_static_vars_set_to_file (dump_file, w_l->statics_read);
	      fprintf (dump_file, "\n    locals written: ");
	      dump_static_vars_set_to_file (dump_file, w_l->statics_written);
	    }
	  cycle_nodes.release ();

	  fprintf (dump_file, "\n  globals read: ");
	  dump_static_vars_set_to_file (dump_file, node_g->statics_read);
	  fprintf (dump_file, "\n  globals written: ");
	  dump_static_vars_set_to_file (dump_file, node_g->statics_written);
	  fprintf (dump_file, "\n");
	}
    }

  /* Cleanup. */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;
      ipa_reference_optimization_summary_t opt;

      node_info = get_reference_vars_info (node);
      if (!node->alias
	  && (cgraph_function_body_availability (node) > AVAIL_OVERWRITABLE
	      || (flags_from_decl_or_type (node->decl) & ECF_LEAF)))
	{
	  node_g = &node_info->global;

	  opt = XCNEW (struct ipa_reference_optimization_summary_d);
	  set_reference_optimization_summary (node, opt);

	  /* Create the complimentary sets.  */

	  if (bitmap_empty_p (node_g->statics_read))
	    opt->statics_not_read = all_module_statics;
	  else
	    {
	      opt->statics_not_read
		 = BITMAP_ALLOC (&optimization_summary_obstack);
	      if (node_g->statics_read != all_module_statics)
		bitmap_and_compl (opt->statics_not_read,
				  all_module_statics,
				  node_g->statics_read);
	    }

	  if (bitmap_empty_p (node_g->statics_written))
	    opt->statics_not_written = all_module_statics;
	  else
	    {
	      opt->statics_not_written
	        = BITMAP_ALLOC (&optimization_summary_obstack);
	      if (node_g->statics_written != all_module_statics)
		bitmap_and_compl (opt->statics_not_written,
				  all_module_statics,
				  node_g->statics_written);
	    }
	}
      free (node_info);
   }

  ipa_free_postorder_info ();
  free (order);

  bitmap_obstack_release (&local_info_obstack);
  ipa_reference_vars_vector.release ();
  if (dump_file)
    splay_tree_delete (reference_vars_to_consider);
  reference_vars_to_consider = NULL;
  return 0;
}

/* Return true if we need to write summary of NODE. */

static bool
write_node_summary_p (struct cgraph_node *node,
		      lto_symtab_encoder_t encoder,
		      bitmap ltrans_statics)
{
  ipa_reference_optimization_summary_t info;

  /* See if we have (non-empty) info.  */
  if (!node->definition || node->global.inlined_to)
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
  if (!reachable_from_this_partition_p (node, encoder)
      && !referenced_from_this_partition_p (&node->ref_list, encoder))
    return false;

  /* See if the info has non-empty intersections with vars we want to encode.  */
  if (!bitmap_intersect_p (info->statics_not_read, ltrans_statics)
      && !bitmap_intersect_p (info->statics_not_written, ltrans_statics))
    return false;
  return true;
}

/* Stream out BITS&LTRANS_STATICS as list of decls to OB.
   LTRANS_STATICS_BITCOUNT specify number of bits in LTRANS_STATICS
   or -1.  When it is positive, just output -1 when
   BITS&LTRANS_STATICS == BITS&LTRANS_STATICS.  */

static void
stream_out_bitmap (struct lto_simple_output_block *ob,
		   bitmap bits, bitmap ltrans_statics,
		   int ltrans_statics_bitcount)
{
  int count = 0;
  unsigned int index;
  bitmap_iterator bi;
  if (bits == all_module_statics)
    {
      streamer_write_hwi_stream (ob->main_stream, -1);
      return;
    }
  EXECUTE_IF_AND_IN_BITMAP (bits, ltrans_statics, 0, index, bi)
    count ++;
  if (count == ltrans_statics_bitcount)
    {
      streamer_write_hwi_stream (ob->main_stream, -1);
      return;
    }
  streamer_write_hwi_stream (ob->main_stream, count);
  if (!count)
    return;
  EXECUTE_IF_AND_IN_BITMAP (bits, ltrans_statics, 0, index, bi)
    {
      tree decl = (tree)splay_tree_lookup (reference_vars_to_consider, index)->value;
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, decl);
    }
}

/* Serialize the ipa info for lto.  */

static void
ipa_reference_write_optimization_summary (void)
{
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_reference);
  unsigned int count = 0;
  int ltrans_statics_bitcount = 0;
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  bitmap ltrans_statics = BITMAP_ALLOC (NULL);
  int i;

  reference_vars_to_consider = splay_tree_new (splay_tree_compare_ints, 0, 0);

  /* See what variables we are interested in.  */
  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      varpool_node *vnode = dyn_cast <varpool_node *> (snode);
      if (vnode
	  && bitmap_bit_p (all_module_statics, DECL_UID (vnode->decl))
	  && referenced_from_this_partition_p (&vnode->ref_list, encoder))
	{
	  tree decl = vnode->decl;
	  bitmap_set_bit (ltrans_statics, DECL_UID (decl));
	  splay_tree_insert (reference_vars_to_consider,
			     DECL_UID (decl), (splay_tree_value)decl);
	  ltrans_statics_bitcount ++;
	}
    }


  if (ltrans_statics_bitcount)
    for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
      {
	symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
	cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
	if (cnode && write_node_summary_p (cnode, encoder, ltrans_statics))
	  count++;
      }

  streamer_write_uhwi_stream (ob->main_stream, count);
  if (count)
    stream_out_bitmap (ob, ltrans_statics, ltrans_statics,
		       -1);

  /* Process all of the functions.  */
  if (ltrans_statics_bitcount)
    for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
      {
	symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
	cgraph_node *cnode = dyn_cast <cgraph_node *> (snode);
	if (cnode && write_node_summary_p (cnode, encoder, ltrans_statics))
	  {
	    ipa_reference_optimization_summary_t info;
	    int node_ref;

	    info = get_reference_optimization_summary (cnode);
	    node_ref = lto_symtab_encoder_encode (encoder, snode);
	    streamer_write_uhwi_stream (ob->main_stream, node_ref);

	    stream_out_bitmap (ob, info->statics_not_read, ltrans_statics,
			       ltrans_statics_bitcount);
	    stream_out_bitmap (ob, info->statics_not_written, ltrans_statics,
			       ltrans_statics_bitcount);
	  }
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
  all_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);

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
	  unsigned int f_count = streamer_read_uhwi (ib);
	  int b_count;
	  if (!f_count)
	    continue;
	  b_count = streamer_read_hwi (ib);
	  if (dump_file)
	    fprintf (dump_file, "all module statics:");
	  for (i = 0; i < (unsigned int)b_count; i++)
	    {
	      unsigned int var_index = streamer_read_uhwi (ib);
	      tree v_decl = lto_file_decl_data_get_var_decl (file_data,
							     var_index);
	      bitmap_set_bit (all_module_statics, DECL_UID (v_decl));
	      if (dump_file)
		fprintf (dump_file, " %s", fndecl_name (v_decl));
	    }

	  for (i = 0; i < f_count; i++)
	    {
	      unsigned int j, index;
	      struct cgraph_node *node;
	      ipa_reference_optimization_summary_t info;
	      int v_count;
	      lto_symtab_encoder_t encoder;

	      index = streamer_read_uhwi (ib);
	      encoder = file_data->symtab_node_encoder;
	      node = cgraph (lto_symtab_encoder_deref (encoder, index));
	      info = XCNEW (struct ipa_reference_optimization_summary_d);
	      set_reference_optimization_summary (node, info);
	      info->statics_not_read = BITMAP_ALLOC (&optimization_summary_obstack);
	      info->statics_not_written = BITMAP_ALLOC (&optimization_summary_obstack);
	      if (dump_file)
		fprintf (dump_file,
			 "\nFunction name:%s/%i:\n  static not read:",
			 node->asm_name (), node->order);

	      /* Set the statics not read.  */
	      v_count = streamer_read_hwi (ib);
	      if (v_count == -1)
		{
		  info->statics_not_read = all_module_statics;
		  if (dump_file)
		    fprintf (dump_file, " all module statics");
		}
	      else
		for (j = 0; j < (unsigned int)v_count; j++)
		  {
		    unsigned int var_index = streamer_read_uhwi (ib);
		    tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								   var_index);
		    bitmap_set_bit (info->statics_not_read, DECL_UID (v_decl));
		    if (dump_file)
		      fprintf (dump_file, " %s", fndecl_name (v_decl));
		  }

	      if (dump_file)
		fprintf (dump_file,
			 "\n  static not written:");
	      /* Set the statics not written.  */
	      v_count = streamer_read_hwi (ib);
	      if (v_count == -1)
		{
		  info->statics_not_written = all_module_statics;
		  if (dump_file)
		    fprintf (dump_file, " all module statics");
		}
	      else
		for (j = 0; j < (unsigned int)v_count; j++)
		  {
		    unsigned int var_index = streamer_read_uhwi (ib);
		    tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								   var_index);
		    bitmap_set_bit (info->statics_not_written, DECL_UID (v_decl));
		    if (dump_file)
		      fprintf (dump_file, " %s", fndecl_name (v_decl));
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

namespace {

const pass_data pass_data_ipa_reference =
{
  IPA_PASS, /* type */
  "static-var", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_IPA_REFERENCE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_reference : public ipa_opt_pass_d
{
public:
  pass_ipa_reference (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_reference, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      ipa_reference_write_optimization_summary, /*
		      write_optimization_summary */
		      ipa_reference_read_optimization_summary, /*
		      read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
    {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (flag_ipa_reference
	      /* Don't bother doing anything if the program has errors.  */
	      && !seen_error ());
    }

  virtual unsigned int execute (function *) { return propagate (); }

}; // class pass_ipa_reference

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_reference (gcc::context *ctxt)
{
  return new pass_ipa_reference (ctxt);
}
