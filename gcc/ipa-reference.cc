/* Callgraph based analysis of static variables.
   Copyright (C) 2004-2022 Free Software Foundation, Inc.
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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "data-streamer.h"
#include "calls.h"
#include "ipa-utils.h"
#include "ipa-reference.h"
#include "alloc-pool.h"
#include "symbol-summary.h"

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
  bitmap statics_read;
  bitmap statics_written;
};

typedef ipa_reference_local_vars_info_d *ipa_reference_local_vars_info_t;
typedef ipa_reference_global_vars_info_d *ipa_reference_global_vars_info_t;
typedef ipa_reference_optimization_summary_d *
  ipa_reference_optimization_summary_t;

struct ipa_reference_vars_info_d
{
  struct ipa_reference_local_vars_info_d local;
  struct ipa_reference_global_vars_info_d global;
};

typedef struct ipa_reference_vars_info_d *ipa_reference_vars_info_t;

/* This map contains all of the static variables that are
   being considered by the compilation level alias analysis.  */
typedef hash_map<tree, int> reference_vars_map_t;
static reference_vars_map_t *ipa_reference_vars_map;
static int ipa_reference_vars_uids;
static vec<tree> *reference_vars_to_consider;
varpool_node_hook_list *varpool_node_hooks;

/* Set of all interesting module statics.  A bit is set for every module
   static we are considering.  This is added to the local info when asm
   code is found that clobbers all memory.  */
static bitmap all_module_statics;
/* Zero bitmap.  */
static bitmap no_module_statics;
/* Set of all statics that should be ignored because they are touched by
   -fno-ipa-reference code.  */
static bitmap ignore_module_statics;

/* Obstack holding bitmaps of local analysis (live from analysis to
   propagation)  */
static bitmap_obstack local_info_obstack;
/* Obstack holding global analysis live forever.  */
static bitmap_obstack optimization_summary_obstack;

class ipa_ref_var_info_summary_t: public fast_function_summary
			  <ipa_reference_vars_info_d *, va_heap>
{
public:
  ipa_ref_var_info_summary_t (symbol_table *symtab):
    fast_function_summary <ipa_reference_vars_info_d *, va_heap> (symtab) {}
};

static ipa_ref_var_info_summary_t *ipa_ref_var_info_summaries = NULL;

class ipa_ref_opt_summary_t: public fast_function_summary
			     <ipa_reference_optimization_summary_d *, va_heap>
{
public:
  ipa_ref_opt_summary_t (symbol_table *symtab):
    fast_function_summary <ipa_reference_optimization_summary_d *, va_heap> (symtab) {}

  void remove (cgraph_node *src_node,
	       ipa_reference_optimization_summary_d *data) final override;
  void duplicate (cgraph_node *src_node, cgraph_node *dst_node,
		  ipa_reference_optimization_summary_d *src_data,
		  ipa_reference_optimization_summary_d *dst_data) final override;
};

static ipa_ref_opt_summary_t *ipa_ref_opt_sum_summaries = NULL;

/* Return ID used by ipa-reference bitmaps.  -1 if failed.  */
int
ipa_reference_var_uid (tree t)
{
  if (!ipa_reference_vars_map)
    return -1;
  int *id = ipa_reference_vars_map->get
    (symtab_node::get (t)->ultimate_alias_target (NULL)->decl);
  if (!id)
    return -1;
  return *id;
}

/* Return ID used by ipa-reference bitmaps.  Create new entry if
   T is not in map.  Set EXISTED accordinly  */
int
ipa_reference_var_get_or_insert_uid (tree t, bool *existed)
{
  int &id = ipa_reference_vars_map->get_or_insert
    (symtab_node::get (t)->ultimate_alias_target (NULL)->decl, existed);
  if (!*existed)
    id = ipa_reference_vars_uids++;
  return id;
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_vars_info_t
get_reference_vars_info (struct cgraph_node *node)
{
  if (ipa_ref_var_info_summaries == NULL)
    return NULL;

  ipa_reference_vars_info_t v = ipa_ref_var_info_summaries->get (node);
  return v == NULL ? NULL : v;
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_optimization_summary_t
get_reference_optimization_summary (struct cgraph_node *node)
{
  if (ipa_ref_opt_sum_summaries == NULL)
    return NULL;

  ipa_reference_optimization_summary_t v
    = ipa_ref_opt_sum_summaries->get (node);

  return v == NULL ? NULL : v;
}

/* Return a bitmap indexed by ipa_reference_var_uid for the static variables
   that are *not* read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_read_global (struct cgraph_node *fn)
{
  if (!opt_for_fn (current_function_decl, flag_ipa_reference))
    return NULL;

  enum availability avail;
  struct cgraph_node *fn2 = fn->function_symbol (&avail);
  ipa_reference_optimization_summary_t info =
    get_reference_optimization_summary (fn2);

  if (info
      && (avail >= AVAIL_AVAILABLE
	  || (avail == AVAIL_INTERPOSABLE
	      && flags_from_decl_or_type (fn->decl) & ECF_LEAF))
      && opt_for_fn (fn2->decl, flag_ipa_reference))
    return info->statics_read;
  else if (avail == AVAIL_NOT_AVAILABLE
	   && flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return no_module_statics;
  else
    return NULL;
}

/* Return a bitmap indexed by ipa_reference_var_uid for the static variables
   that are *not* written during the execution of the function FN.  Note
   that variables written may or may not be read during the function
   call.  Returns NULL if no data is available.  */

bitmap
ipa_reference_get_written_global (struct cgraph_node *fn)
{
  if (!opt_for_fn (current_function_decl, flag_ipa_reference))
    return NULL;

  enum availability avail;
  struct cgraph_node *fn2 = fn->function_symbol (&avail);
  ipa_reference_optimization_summary_t info =
    get_reference_optimization_summary (fn2);

  if (info
      && (avail >= AVAIL_AVAILABLE
	  || (avail == AVAIL_INTERPOSABLE
	      && flags_from_decl_or_type (fn->decl) & ECF_LEAF))
      && opt_for_fn (fn2->decl, flag_ipa_reference))
    return info->statics_written;
  else if (avail == AVAIL_NOT_AVAILABLE
	   && flags_from_decl_or_type (fn->decl) & ECF_LEAF)
    return no_module_statics;
  else
    return NULL;
}


/* Hepler for is_proper_for_analysis.  */
static bool
is_improper (symtab_node *n, void *v ATTRIBUTE_UNUSED)
{
  tree t = n->decl;
  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    return true;

  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    return true;

  /* We do not need to analyze readonly vars, we already know they do not
     alias.  */
  if (TREE_READONLY (t))
    return true;

  /* We cannot track variables with address taken.  */
  if (TREE_ADDRESSABLE (t))
    return true;

  /* TODO: We could track public variables that are not addressable, but
     currently frontends don't give us those.  */
  if (TREE_PUBLIC (t))
    return true;

  return false;
}

/* Return true if the variable T is the right kind of static variable to
   perform compilation unit scope escape analysis.  */

static inline bool
is_proper_for_analysis (tree t)
{
  int id = ipa_reference_var_uid (t);

  if (id != -1 && bitmap_bit_p (ignore_module_statics, id))
    return false;

  if (symtab_node::get (t)
	->call_for_symbol_and_aliases (is_improper, NULL, true))
    return false;

  return true;
}

/* Lookup the tree node for the static variable that has UID and
   convert the name to a string for debugging.  */

static const char *
get_static_name (int index)
{
  return fndecl_name ((*reference_vars_to_consider)[index]);
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
  else if (set == no_module_statics)
    fprintf (f, "NO");
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

/* Return a copy of SET on the bitmap obstack containing SET.
   But if SET is NULL or the maximum set, return that instead.  */

static bitmap
copy_static_var_set (bitmap set, bool for_propagation)
{
  if (set == NULL || set == all_module_statics)
    return set;
  if (!for_propagation && set == no_module_statics)
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
      struct cgraph_node *y = e->callee->function_symbol (&avail);
      if (!y)
	continue;

      /* Only look into nodes we can propagate something.  */
      int flags = flags_from_decl_or_type (y->decl);
      if (opt_for_fn (y->decl, flag_ipa_reference)
	  && (avail > AVAIL_INTERPOSABLE
	      || (avail == AVAIL_INTERPOSABLE && (flags & ECF_LEAF))))
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
		  || e->cannot_lead_to_return_p ())
		continue;

	      union_static_var_sets (x_global->statics_written,
				     y_global->statics_written);
	    }
	  else
	    gcc_unreachable ();
	}
    }
}

/* Delete NODE from map.  */

static void
varpool_removal_hook (varpool_node *node, void *)
{
  ipa_reference_vars_map->remove (node->decl);
}

static bool ipa_init_p = false;

/* The init routine for analyzing global static variable usage.  See
   comments at top for description.  */
static void
ipa_init (void)
{
  if (ipa_init_p)
    return;

  ipa_init_p = true;

  if (dump_file)
    vec_alloc (reference_vars_to_consider, 10);

  if (ipa_ref_opt_sum_summaries != NULL)
    {
      delete ipa_ref_opt_sum_summaries;
      ipa_ref_opt_sum_summaries = NULL;
      delete ipa_reference_vars_map;
    }
  ipa_reference_vars_map = new reference_vars_map_t(257);
  varpool_node_hooks
	 = symtab->add_varpool_removal_hook (varpool_removal_hook, NULL);
  ipa_reference_vars_uids = 0;

  bitmap_obstack_initialize (&local_info_obstack);
  bitmap_obstack_initialize (&optimization_summary_obstack);
  all_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);
  no_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);
  ignore_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);

  if (ipa_ref_var_info_summaries == NULL)
    ipa_ref_var_info_summaries = new ipa_ref_var_info_summary_t (symtab);
}


/* Set up the persistent info for FN.  */

static ipa_reference_local_vars_info_t
init_function_info (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info
    = ipa_ref_var_info_summaries->get_create (fn);

  info->local.statics_read = BITMAP_ALLOC (&local_info_obstack);
  info->local.statics_written = BITMAP_ALLOC (&local_info_obstack);
  info->global.statics_read = NULL;

  return &info->local;
}


/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static void
analyze_function (struct cgraph_node *fn)
{
  ipa_reference_local_vars_info_t local;
  struct ipa_ref *ref = NULL;
  int i;
  tree var;

  if (!opt_for_fn (fn->decl, flag_ipa_reference))
    return;
  local = init_function_info (fn);
  for (i = 0; fn->iterate_reference (i, ref); i++)
    {
      int id;
      bool existed;
      if (!is_a <varpool_node *> (ref->referred))
	continue;
      var = ref->referred->decl;
      if (!is_proper_for_analysis (var))
	continue;
      /* This is a variable we care about.  Check if we have seen it
	 before, and if not add it the set of variables we care about.  */
      id = ipa_reference_var_get_or_insert_uid (var, &existed);
      if (!existed)
	{
	  bitmap_set_bit (all_module_statics, id);
	  if (dump_file)
	    reference_vars_to_consider->safe_push (var);
	}
      switch (ref->use)
	{
	case IPA_REF_LOAD:
          bitmap_set_bit (local->statics_read, id);
	  break;
	case IPA_REF_STORE:
	  if (ref->cannot_lead_to_return ())
	    break;
          bitmap_set_bit (local->statics_written, id);
	  break;
	case IPA_REF_ADDR:
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  if (fn->cannot_return_p ())
    bitmap_clear (local->statics_written);
}


/* Called when new clone is inserted to callgraph late.  */

void
ipa_ref_opt_summary_t::duplicate (cgraph_node *, cgraph_node *,
				  ipa_reference_optimization_summary_d *ginfo,
				  ipa_reference_optimization_summary_d
				  *dst_ginfo)
{
  dst_ginfo->statics_read =
    copy_static_var_set (ginfo->statics_read, false);
  dst_ginfo->statics_written =
    copy_static_var_set (ginfo->statics_written, false);
}

/* Called when node is removed.  */

void
ipa_ref_opt_summary_t::remove (cgraph_node *,
			       ipa_reference_optimization_summary_d *ginfo)
{
  if (ginfo->statics_read
      && ginfo->statics_read != all_module_statics
      && ginfo->statics_read != no_module_statics)
    BITMAP_FREE (ginfo->statics_read);

  if (ginfo->statics_written
      && ginfo->statics_written != all_module_statics
      && ginfo->statics_written != no_module_statics)
    BITMAP_FREE (ginfo->statics_written);
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
    if (!node->alias && !opt_for_fn (node->decl, flag_ipa_reference))
      {
        struct ipa_ref *ref = NULL;
        int i;
        tree var;
	for (i = 0; node->iterate_reference (i, ref); i++)
	  {
	    if (!is_a <varpool_node *> (ref->referred))
	      continue;
	    var = ref->referred->decl;
	    if (!is_proper_for_analysis (var))
	      continue;
	    bitmap_set_bit (ignore_module_statics, ipa_reference_var_uid (var));
	  }
      }
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
      if (node->get_availability () >= AVAIL_INTERPOSABLE
	  && opt_for_fn (node->decl, flag_ipa_reference))
	{
	  ipa_reference_local_vars_info_t l;
	  unsigned int index;
	  bitmap_iterator bi;

	  l = &get_reference_vars_info (node)->local;
	  fprintf (dump_file,
		   "\nFunction name:%s:", node->dump_name ());
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
      && node->get_availability () < AVAIL_INTERPOSABLE)
    ;
  else if (flags & ECF_CONST)
    ;
  else if ((flags & ECF_PURE) || node->cannot_return_p ())
    {
      read_all = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   %s -> read all\n", node->dump_name ());
    }
  else
    {
       /* TODO: To be able to produce sane results, we should also handle
	  common builtins, in particular throw.  */
      read_all = true;
      write_all = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "   %s -> read all, write all\n",
		  node->dump_name ());
    }
}

/* Set READ_ALL/WRITE_ALL based on decl flags of NODE or any member
   in the cycle of NODE.  */

static void
get_read_write_all_from_node (struct cgraph_node *node,
			      bool &read_all, bool &write_all)
{
  struct cgraph_edge *e, *ie;

  /* When function is overwritable, we cannot assume anything.  */
  if (node->get_availability () <= AVAIL_INTERPOSABLE
      || (node->analyzed && !opt_for_fn (node->decl, flag_ipa_reference)))
    read_write_all_from_decl (node, read_all, write_all);

  for (e = node->callees;
       e && !(read_all && write_all);
       e = e->next_callee)
    {
      enum availability avail;
      struct cgraph_node *callee = e->callee->function_symbol (&avail);
      gcc_checking_assert (callee);
      if (avail <= AVAIL_INTERPOSABLE
	  || (callee->analyzed && !opt_for_fn (callee->decl,
					       flag_ipa_reference)))
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
	if (!ie->cannot_lead_to_return_p ()
	    && !(ie->indirect_info->ecf_flags & ECF_PURE))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "   indirect call -> write all\n");
	    write_all = true;
	  }
      }
}

/* Skip edges from and to nodes without ipa_reference enabled.
   Ignore not available symbols.  This leave
   them out of strongly connected components and makes them easy to skip in the
   propagation loop bellow.  */

static bool
ignore_edge_p (cgraph_edge *e)
{
  enum availability avail;
  cgraph_node *ultimate_target
    = e->callee->function_or_virtual_thunk_symbol (&avail, e->caller);

  return (avail < AVAIL_INTERPOSABLE
	  || (avail == AVAIL_INTERPOSABLE
	      && !(flags_from_decl_or_type (e->callee->decl) & ECF_LEAF))
	  || !opt_for_fn (e->caller->decl, flag_ipa_reference)
          || !opt_for_fn (ultimate_target->decl, flag_ipa_reference));
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by ipa_analyze_function.  */

static unsigned int
propagate (void)
{
  struct cgraph_node *node;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  int order_pos;
  int i;
  bool remove_p;

  if (dump_file)
    cgraph_node::dump_cgraph (dump_file);

  remove_p = ipa_discover_variable_flags ();
  generate_summary ();

  /* Propagate the local information through the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  order_pos = ipa_reduced_postorder (order, true, ignore_edge_p);
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
      if (node->alias || !opt_for_fn (node->decl, flag_ipa_reference))
	continue;

      node_info = get_reference_vars_info (node);
      gcc_assert (node_info);
      node_l = &node_info->local;
      node_g = &node_info->global;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting cycle with %s\n", node->dump_name ());

      vec<cgraph_node *> cycle_nodes = ipa_get_nodes_in_cycle (node);

      /* If any node in a cycle is read_all or write_all, they all are.  */
      FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Visiting %s\n", w->dump_asm_name ());
	  get_read_write_all_from_node (w, read_all, write_all);
	  if (read_all && write_all)
	    break;
	}

      /* Initialized the bitmaps global sets for the reduced node.  */
      if (read_all)
	node_g->statics_read = all_module_statics;
      else
	node_g->statics_read = copy_static_var_set (node_l->statics_read, true);
      if (write_all)
	node_g->statics_written = all_module_statics;
      else
	node_g->statics_written
	  = copy_static_var_set (node_l->statics_written, true);

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
		  && !w->cannot_return_p ())
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
          if (node->alias || !opt_for_fn (node->decl, flag_ipa_reference))
	    continue;

	  fprintf (dump_file, "\nFunction name:%s:", node->dump_asm_name ());

	  ipa_reference_vars_info_t node_info = get_reference_vars_info (node);
	  ipa_reference_global_vars_info_t node_g = &node_info->global;

	  vec<cgraph_node *> cycle_nodes = ipa_get_nodes_in_cycle (node);
	  FOR_EACH_VEC_ELT (cycle_nodes, x, w)
	    {
	      ipa_reference_vars_info_t w_ri = get_reference_vars_info (w);
	      ipa_reference_local_vars_info_t w_l = &w_ri->local;
	      if (w != node)
		fprintf (dump_file, "\n  next cycle: %s ", w->dump_asm_name ());
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

  if (ipa_ref_opt_sum_summaries == NULL)
    {
      ipa_ref_opt_sum_summaries = new ipa_ref_opt_summary_t (symtab);
      ipa_ref_opt_sum_summaries->disable_insertion_hook ();
    }

  /* Cleanup. */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;

      /* No need to produce summaries for inline clones.  */
      if (node->inlined_to)
	continue;

      node_info = get_reference_vars_info (node);
      if (!node->alias && opt_for_fn (node->decl, flag_ipa_reference))
	{
	  node_g = &node_info->global;
	  bool read_all = 
		(node_g->statics_read == all_module_statics
		 || bitmap_equal_p (node_g->statics_read, all_module_statics));
	  bool written_all = 
		(node_g->statics_written == all_module_statics
		 || bitmap_equal_p (node_g->statics_written,
				    all_module_statics));

	  /* There is no need to produce summary if we collected nothing
	     useful.  */
	  if (read_all && written_all)
	    continue;

	  ipa_reference_optimization_summary_d *opt
	    = ipa_ref_opt_sum_summaries->get_create (node);

	  /* Create the complimentary sets.  */

	  if (bitmap_empty_p (node_g->statics_read))
	    opt->statics_read = no_module_statics;
	  else if (read_all)
	    opt->statics_read = all_module_statics;
	  else
	    {
	      opt->statics_read
		 = BITMAP_ALLOC (&optimization_summary_obstack);
	      bitmap_copy (opt->statics_read, node_g->statics_read);
	    }

	  if (bitmap_empty_p (node_g->statics_written))
	    opt->statics_written = no_module_statics;
	  else if (written_all)
	    opt->statics_written = all_module_statics;
	  else
	    {
	      opt->statics_written
	        = BITMAP_ALLOC (&optimization_summary_obstack);
	      bitmap_copy (opt->statics_written, node_g->statics_written);
	    }
	}
   }

  ipa_free_postorder_info ();
  free (order);

  bitmap_obstack_release (&local_info_obstack);

  if (ipa_ref_var_info_summaries != NULL)
    {
      delete ipa_ref_var_info_summaries;
      ipa_ref_var_info_summaries = NULL;
    }

  if (dump_file)
    {
      vec_free (reference_vars_to_consider);
      reference_vars_to_consider = NULL;
    }
  else
    gcc_checking_assert (!reference_vars_to_consider);
  return remove_p ? TODO_remove_functions : 0;
}

/* Return true if we need to write summary of NODE. */

static bool
write_node_summary_p (struct cgraph_node *node,
		      lto_symtab_encoder_t encoder,
		      bitmap ltrans_statics)
{
  ipa_reference_optimization_summary_t info;

  /* See if we have (non-empty) info.  */
  if (!node->definition || node->inlined_to)
    return false;
  info = get_reference_optimization_summary (node);
  if (!info)
    return false;

  /* See if we want to encode it.
     Encode also referenced functions since constant folding might turn it into
     a direct call.

     In future we might also want to include summaries of functions references
     by initializers of constant variables references in current unit.  */
  if (!reachable_from_this_partition_p (node, encoder)
      && !referenced_from_this_partition_p (node, encoder))
    return false;

  /* See if the info has non-empty intersections with vars we want to
     encode.  */
  bitmap_iterator bi;
  unsigned int i;
  EXECUTE_IF_AND_COMPL_IN_BITMAP (ltrans_statics, info->statics_read, 0,
				  i, bi)
    return true;
  EXECUTE_IF_AND_COMPL_IN_BITMAP (ltrans_statics, info->statics_written, 0,
				  i, bi)
    return true;
  return false;
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
      tree decl = (*reference_vars_to_consider) [index];
      lto_output_var_decl_ref (ob->decl_state, ob->main_stream, decl);
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
  auto_bitmap ltrans_statics;
  int i;

  gcc_checking_assert (!reference_vars_to_consider);
  vec_alloc (reference_vars_to_consider, ipa_reference_vars_uids);
  reference_vars_to_consider->safe_grow (ipa_reference_vars_uids, true);

  /* See what variables we are interested in.  */
  for (i = 0; i < lto_symtab_encoder_size (encoder); i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      varpool_node *vnode = dyn_cast <varpool_node *> (snode);
      int id;

      if (vnode
	  && (id = ipa_reference_var_uid (vnode->decl)) != -1
	  && referenced_from_this_partition_p (vnode, encoder))
	{
	  tree decl = vnode->decl;
	  bitmap_set_bit (ltrans_statics, id);
	  (*reference_vars_to_consider)[id] = decl;
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

	    stream_out_bitmap (ob, info->statics_read, ltrans_statics,
			       ltrans_statics_bitcount);
	    stream_out_bitmap (ob, info->statics_written, ltrans_statics,
			       ltrans_statics_bitcount);
	  }
      }
  lto_destroy_simple_output_block (ob);
  vec_free (reference_vars_to_consider);
  reference_vars_to_consider = NULL;
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

  gcc_checking_assert (ipa_ref_opt_sum_summaries == NULL);
  ipa_ref_opt_sum_summaries = new ipa_ref_opt_summary_t (symtab);
  ipa_ref_opt_sum_summaries->disable_insertion_hook ();
  ipa_reference_vars_map = new reference_vars_map_t(257);
  varpool_node_hooks
	 = symtab->add_varpool_removal_hook (varpool_removal_hook, NULL);
  ipa_reference_vars_uids = 0;

  all_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);
  no_module_statics = BITMAP_ALLOC (&optimization_summary_obstack);

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      class lto_input_block *ib
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
	      tree v_decl = lto_input_var_decl_ref (ib, file_data);
	      bool existed;
	      bitmap_set_bit (all_module_statics,
			      ipa_reference_var_get_or_insert_uid
				 (v_decl, &existed));
	      gcc_checking_assert (!existed);
	      if (dump_file)
		fprintf (dump_file, " %s", fndecl_name (v_decl));
	    }

	  for (i = 0; i < f_count; i++)
	    {
	      unsigned int j, index;
	      struct cgraph_node *node;
	      int v_count;
	      lto_symtab_encoder_t encoder;

	      index = streamer_read_uhwi (ib);
	      encoder = file_data->symtab_node_encoder;
	      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref
		(encoder, index));

	      ipa_reference_optimization_summary_d *info
		= ipa_ref_opt_sum_summaries->get_create (node);

	      if (dump_file)
		fprintf (dump_file,
			 "\nFunction name:%s:\n  static read:",
			 node->dump_asm_name ());

	      /* Set the statics read.  */
	      v_count = streamer_read_hwi (ib);
	      if (v_count == -1)
		{
		  info->statics_read = all_module_statics;
		  if (dump_file)
		    fprintf (dump_file, " all module statics");
		}
	      else if (v_count == 0)
		info->statics_read = no_module_statics;
	      else
		{
		  info->statics_read = BITMAP_ALLOC
		    (&optimization_summary_obstack);
		  for (j = 0; j < (unsigned int)v_count; j++)
		    {
		      tree v_decl = lto_input_var_decl_ref (ib, file_data);
		      bitmap_set_bit (info->statics_read,
				      ipa_reference_var_uid (v_decl));
		      if (dump_file)
			fprintf (dump_file, " %s", fndecl_name (v_decl));
		    }
		}

	      if (dump_file)
		fprintf (dump_file,
			 "\n  static written:");
	      /* Set the statics written.  */
	      v_count = streamer_read_hwi (ib);
	      if (v_count == -1)
		{
		  info->statics_written = all_module_statics;
		  if (dump_file)
		    fprintf (dump_file, " all module statics");
		}
	      else if (v_count == 0)
		info->statics_written = no_module_statics;
	      else
		{
		  info->statics_written = BITMAP_ALLOC
		    (&optimization_summary_obstack);
		  for (j = 0; j < (unsigned int)v_count; j++)
		    {
		      tree v_decl = lto_input_var_decl_ref (ib, file_data);
		      bitmap_set_bit (info->statics_written,
				      ipa_reference_var_uid (v_decl));
		      if (dump_file)
			fprintf (dump_file, " %s", fndecl_name (v_decl));
		    }
		}
	      if (dump_file)
		fprintf (dump_file, "\n");
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_reference,
					  ib, data, len);
	}
      else
	/* Fatal error here.  We do not want to support compiling ltrans units
	   with different version of compiler or different flags than
	   the WPA unit, so this should never happen.  */
	fatal_error (input_location,
		     "ipa reference summary is missing in ltrans unit");
    }
}

namespace {

const pass_data pass_data_ipa_reference =
{
  IPA_PASS, /* type */
  "static-var", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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
  bool gate (function *) final override
    {
      return ((in_lto_p || flag_ipa_reference)
	      /* Don't bother doing anything if the program has errors.  */
	      && !seen_error ());
    }

  unsigned int execute (function *) final override { return propagate (); }

}; // class pass_ipa_reference

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_reference (gcc::context *ctxt)
{
  return new pass_ipa_reference (ctxt);
}

/* Reset all state within ipa-reference.cc so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
ipa_reference_cc_finalize (void)
{
  if (ipa_ref_opt_sum_summaries != NULL)
    {
      delete ipa_ref_opt_sum_summaries;
      ipa_ref_opt_sum_summaries = NULL;
      delete ipa_reference_vars_map;
      ipa_reference_vars_map = NULL;
      symtab->remove_varpool_removal_hook (varpool_node_hooks);
    }

  if (ipa_init_p)
    {
      bitmap_obstack_release (&optimization_summary_obstack);
      ipa_init_p = false;
    }
}
