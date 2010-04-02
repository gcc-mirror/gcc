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

   There are two categories of information produced by this pass:

   1) The addressable (TREE_ADDRESSABLE) bit and readonly
   (TREE_READONLY) bit associated with these variables is properly set
   based on scanning all of the code withing the compilation unit.

   2) The transitive call site specific clobber effects are computed
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
   information.

   Currently must be run after inlining decisions have been made since
   otherwise, the local sets will not contain information that is
   consistent with post inlined state.  The global sets are not prone
   to this problem since they are by definition transitive.  */

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

static void add_new_function (struct cgraph_node *node,
			      void *data ATTRIBUTE_UNUSED);
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
   transitive closure of the functions that are called.  The
   structures are separated to allow the global structures to be
   shared between several functions since every function within a
   strongly connected component will have the same information.  This
   sharing saves both time and space in the computation of the vectors
   as well as their translation from decl_uid form to ann_uid
   form.  */

struct ipa_reference_global_vars_info_d
{
  bitmap statics_read;
  bitmap statics_written;
  bitmap statics_not_read;
  bitmap statics_not_written;
};

typedef struct ipa_reference_local_vars_info_d *ipa_reference_local_vars_info_t;
typedef struct ipa_reference_global_vars_info_d *ipa_reference_global_vars_info_t;
struct ipa_reference_vars_info_d
{
  ipa_reference_local_vars_info_t local;
  ipa_reference_global_vars_info_t global;
};

typedef struct ipa_reference_vars_info_d *ipa_reference_vars_info_t;

/* This splay tree contains all of the static variables that are
   being considered by the compilation level alias analysis.  For
   module_at_a_time compilation, this is the set of static but not
   public variables.  Any variables that either have their address
   taken or participate in otherwise unsavory operations are deleted
   from this list.  */
static GTY((param1_is(int), param2_is(tree)))
     splay_tree reference_vars_to_consider;

/* This bitmap is used to knock out the module static variables whose
   addresses have been taken and passed around.  */
static bitmap module_statics_escape;

/* This bitmap is used to knock out the module static variables that
   are not readonly.  */
static bitmap module_statics_written;

/* A bit is set for every module static we are considering.  This is
   ored into the local info when asm code is found that clobbers all
   memory. */
static bitmap all_module_statics;

static struct pointer_set_t *visited_nodes;

/* Obstack holding bitmaps of local analysis (live from analysis to
   propagation)  */
static bitmap_obstack local_info_obstack;
/* Obstack holding global analysis live forever.  */
static bitmap_obstack global_info_obstack;

/* Holders of ipa cgraph hooks: */
static struct cgraph_node_hook_list *function_insertion_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;

enum initialization_status_t
{
  UNINITIALIZED,
  RUNNING,
  FINISHED
};

tree memory_identifier_string;

/* Vector where the reference var infos are actually stored. */
DEF_VEC_P (ipa_reference_vars_info_t);
DEF_VEC_ALLOC_P (ipa_reference_vars_info_t, heap);
static VEC (ipa_reference_vars_info_t, heap) *ipa_reference_vars_vector;

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline ipa_reference_vars_info_t
get_reference_vars_info (struct cgraph_node *node)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t, ipa_reference_vars_vector) <= (unsigned int)node->uid)
    return NULL;
  return VEC_index (ipa_reference_vars_info_t, ipa_reference_vars_vector, node->uid);
}

/* Return the ipa_reference_vars structure starting from the cgraph NODE.  */
static inline void
set_reference_vars_info (struct cgraph_node *node, ipa_reference_vars_info_t info)
{
  if (!ipa_reference_vars_vector
      || VEC_length (ipa_reference_vars_info_t, ipa_reference_vars_vector) <= (unsigned int)node->uid)
     VEC_safe_grow_cleared (ipa_reference_vars_info_t, heap, ipa_reference_vars_vector, node->uid + 1);
  VEC_replace (ipa_reference_vars_info_t, ipa_reference_vars_vector, node->uid, info);
}

/* Get a bitmap that contains all of the locally referenced static
   variables for function FN.  */
static ipa_reference_local_vars_info_t
get_local_reference_vars_info (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info = get_reference_vars_info (fn);

  if (info)
    return info->local;
  else
    /* This phase was not run.  */
    return NULL;
}

/* Get a bitmap that contains all of the globally referenced static
   variables for function FN.  */

static ipa_reference_global_vars_info_t
get_global_reference_vars_info (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info = get_reference_vars_info (fn);

  if (info)
    return info->global;
  else
    /* This phase was not run.  */
    return NULL;
}

/* Return a bitmap indexed by VAR_DECL uid for the static variables
   that are read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_read_global (struct cgraph_node *fn)
{
  ipa_reference_global_vars_info_t g = get_global_reference_vars_info (fn);
  if (g)
    return g->statics_read;
  else
    return NULL;
}

/* Return a bitmap indexed by VAR_DECL uid for the static variables
   that are written during the execution of the function FN.  Note
   that variables written may or may not be read during the function
   call.  Returns NULL if no data is available.  */

bitmap
ipa_reference_get_written_global (struct cgraph_node *fn)
{
  ipa_reference_global_vars_info_t g = get_global_reference_vars_info (fn);
  if (g)
    return g->statics_written;
  else
    return NULL;
}

/* Return a bitmap indexed by_DECL_UID uid for the static variables
   that are not read during the execution of the function FN.  Returns
   NULL if no data is available.  */

bitmap
ipa_reference_get_not_read_global (struct cgraph_node *fn)
{
  ipa_reference_global_vars_info_t g = get_global_reference_vars_info (fn);
  if (g)
    return g->statics_not_read;
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
  ipa_reference_global_vars_info_t g = get_global_reference_vars_info (fn);
  if (g)
    return g->statics_not_written;
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
      splay_tree_insert (reference_vars_to_consider,
			 uid, (splay_tree_value)var);
      bitmap_set_bit (all_module_statics, uid);
    }
}

/* Return true if the variable T is the right kind of static variable to
   perform compilation unit scope escape analysis.  */

static inline bool
has_proper_scope_for_analysis (tree t)
{
  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    return false;

  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    return false;

  /* Do not care about a local automatic that is not static.  */
  if (!TREE_STATIC (t) && !DECL_EXTERNAL (t))
    return false;

  /* FIXME: for LTO we should include PUBLIC vars too.  This is bit difficult
     as summarie would need unsharing.  */
  if (DECL_EXTERNAL (t) || TREE_PUBLIC (t))
    return false;

  /* We cannot touch decls where the type needs constructing.  */
  if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (t)))
    return false;

  /* This is a variable we care about.  Check if we have seen it
     before, and if not add it the set of variables we care about.  */
  if (!bitmap_bit_p (all_module_statics, DECL_UID (t)))
    add_static_var (t);

  return true;
}

/* Mark tree T as having address taken.  */

static void
mark_address_taken (tree x)
{
  if (TREE_CODE (x) == VAR_DECL
      && module_statics_escape && has_proper_scope_for_analysis (x))
    bitmap_set_bit (module_statics_escape, DECL_UID (x));
}

/* Wrapper around mark_address_taken for the stmt walker.  */

static bool
mark_address (gimple stmt ATTRIBUTE_UNUSED, tree addr,
	      void *data ATTRIBUTE_UNUSED)
{
  while (handled_component_p (addr))
    addr = TREE_OPERAND (addr, 0);
  mark_address_taken (addr);
  return false;
}

/* Mark load of T.  */

static bool
mark_load (gimple stmt ATTRIBUTE_UNUSED, tree t, void *data)
{
  ipa_reference_local_vars_info_t local = (ipa_reference_local_vars_info_t)data;
  if (TREE_CODE (t) == VAR_DECL
      && has_proper_scope_for_analysis (t))
    bitmap_set_bit (local->statics_read, DECL_UID (t));
  return false;
}

/* Mark store of T.  */

static bool
mark_store (gimple stmt ATTRIBUTE_UNUSED, tree t, void *data)
{
  ipa_reference_local_vars_info_t local = (ipa_reference_local_vars_info_t)data;
  if (TREE_CODE (t) == VAR_DECL
      && has_proper_scope_for_analysis (t))
    {
      if (local)
	bitmap_set_bit (local->statics_written, DECL_UID (t));
      /* Mark the write so we can tell which statics are
	 readonly.  */
      if (module_statics_written)
	bitmap_set_bit (module_statics_written, DECL_UID (t));
    }
  return false;
}

/* Look for memory clobber and set read_all/write_all if present.  */

static void
check_asm_memory_clobber (ipa_reference_local_vars_info_t local, gimple stmt)
{
  size_t i;
  tree op;

  for (i = 0; i < gimple_asm_nclobbers (stmt); i++)
    {
      op = gimple_asm_clobber_op (stmt, i);
      if (simple_cst_equal(TREE_VALUE (op), memory_identifier_string) == 1)
	{
	  /* Abandon all hope, ye who enter here. */
	  local->calls_read_all = true;
	  local->calls_write_all = true;
	}
    }
}

/* Look for external calls and set read_all/write_all correspondingly.  */

static void
check_call (ipa_reference_local_vars_info_t local, gimple stmt)
{
  int flags = gimple_call_flags (stmt);
  tree callee_t = gimple_call_fndecl (stmt);

  /* Process indirect calls.  All direct calles are handled at propagation
     time.  */
  if (!callee_t)
    {
      if (flags & ECF_CONST)
	;
      else if (flags & ECF_PURE)
	local->calls_read_all = true;
      else
	{
	  local->calls_read_all = true;
	  /* When function does not reutrn, it is safe to ignore anythign it writes
	     to, because the effect will never happen.  */
	  if ((flags & (ECF_NOTHROW | ECF_NORETURN))
	      != (ECF_NOTHROW | ECF_NORETURN))
	    local->calls_write_all = true;
	}
    }
}

/* TP is the part of the tree currently under the microscope.
   WALK_SUBTREES is part of the walk_tree api but is unused here.
   DATA is cgraph_node of the function being walked.  */

static tree
scan_stmt_for_static_refs (gimple_stmt_iterator *gsip,
			   struct cgraph_node *fn)
{
  gimple stmt = gsi_stmt (*gsip);
  ipa_reference_local_vars_info_t local = NULL;

  if (is_gimple_debug (stmt))
    return NULL;

  if (fn)
    local = get_reference_vars_info (fn)->local;

  /* Look for direct loads and stores.  */
  walk_stmt_load_store_addr_ops (stmt, local, mark_load, mark_store,
				 mark_address);

  if (is_gimple_call (stmt))
    check_call (local, stmt);
  else if (gimple_code (stmt) == GIMPLE_ASM)
    check_asm_memory_clobber (local, stmt);

  return NULL;
}

/* Call-back to scan variable initializers for static references.
   Called using walk_tree.  */

static tree
scan_initializer_for_static_refs (tree *tp, int *walk_subtrees,
				  void *data ATTRIBUTE_UNUSED)
{
  tree t = *tp;

  if (TREE_CODE (t) == ADDR_EXPR)
    {
      mark_address_taken (get_base_var (t));
      *walk_subtrees = 0;
    }
  /* Save some cycles by not walking types and declaration as we
     won't find anything useful there anyway.  */
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;

  return NULL;
}

/* Lookup the tree node for the static variable that has UID.  */
static tree
get_static_decl (int index)
{
  splay_tree_node stn =
    splay_tree_lookup (reference_vars_to_consider, index);
  if (stn)
    return (tree)stn->value;
  return NULL;
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
	      ipa_reference_global_vars_info_t y_global = y_info->global;

	      /* Calls in current cycle do not have global computed yet.  */
	      if (!y_info->global)
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

  memory_identifier_string = build_string(7, "memory");

  reference_vars_to_consider =
    splay_tree_new_ggc (splay_tree_compare_ints);

  bitmap_obstack_initialize (&local_info_obstack);
  bitmap_obstack_initialize (&global_info_obstack);
  module_statics_escape = BITMAP_ALLOC (&local_info_obstack);
  module_statics_written = BITMAP_ALLOC (&local_info_obstack);
  all_module_statics = BITMAP_ALLOC (&global_info_obstack);

  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  visited_nodes = pointer_set_create ();

  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&add_new_function, NULL);
  node_removal_hook_holder =
      cgraph_add_node_removal_hook (&remove_node_data, NULL);
  node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&duplicate_node_data, NULL);
}

/* Check out the rhs of a static or global initialization VNODE to see
   if any of them contain addressof operations.  Note that some of
   these variables may  not even be referenced in the code in this
   compilation unit but their right hand sides may contain references
   to variables defined within this unit.  */

static void
analyze_variable (struct varpool_node *vnode)
{
  struct walk_stmt_info wi;
  tree global = vnode->decl;

  memset (&wi, 0, sizeof (wi));
  wi.pset = visited_nodes;
  walk_tree (&DECL_INITIAL (global), scan_initializer_for_static_refs,
             &wi, wi.pset);
}


/* Set up the persistent info for FN.  */

static ipa_reference_local_vars_info_t
init_function_info (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info
    = XCNEW (struct ipa_reference_vars_info_d);
  ipa_reference_local_vars_info_t l
    = XCNEW (struct ipa_reference_local_vars_info_d);

  /* Add the info to the tree's annotation.  */
  set_reference_vars_info (fn, info);

  info->local = l;
  l->statics_read = BITMAP_ALLOC (&local_info_obstack);
  l->statics_written = BITMAP_ALLOC (&local_info_obstack);

  return l;
}


/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static void
analyze_function (struct cgraph_node *fn)
{
  tree decl = fn->decl;
  struct function *this_cfun = DECL_STRUCT_FUNCTION (decl);
  basic_block this_block;
#ifdef ENABLE_CHECKING
  tree step;
#endif
  ipa_reference_local_vars_info_t local;

  if (dump_file)
    fprintf (dump_file, "\n local analysis of %s\n", cgraph_node_name (fn));

  push_cfun (DECL_STRUCT_FUNCTION (decl));
  current_function_decl = decl;

  init_function_info (fn);
  FOR_EACH_BB_FN (this_block, this_cfun)
    {
      gimple_stmt_iterator gsi;
      gimple phi;
      tree op;
      use_operand_p use;
      ssa_op_iter iter;

      /* Find the addresses taken in phi node arguments.  */
      for (gsi = gsi_start_phis (this_block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  phi = gsi_stmt (gsi);
	  FOR_EACH_PHI_ARG (use, phi, iter, SSA_OP_USE)
	    {
	      op = USE_FROM_PTR (use);
	      if (TREE_CODE (op) == ADDR_EXPR)
		mark_address_taken (get_base_var (op));
	    }
	}

      for (gsi = gsi_start_bb (this_block); !gsi_end_p (gsi); gsi_next (&gsi))
	scan_stmt_for_static_refs (&gsi, fn);
    }

  local = get_reference_vars_info (fn)->local;
  if ((flags_from_decl_or_type (decl) & (ECF_NOTHROW | ECF_NORETURN))
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


#ifdef ENABLE_CHECKING
  /* Verify that all local initializers was expanded by gimplifier.  */
  for (step = DECL_STRUCT_FUNCTION (decl)->local_decls;
       step;
       step = TREE_CHAIN (step))
    {
      tree var = TREE_VALUE (step);
      if (TREE_CODE (var) == VAR_DECL
	  && DECL_INITIAL (var)
	  && !TREE_STATIC (var))
	gcc_unreachable ();
    }
#endif
  pop_cfun ();
  current_function_decl = NULL;
}

/* Remove local data associated with function FN.  */
static void
clean_function_local_data (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info = get_reference_vars_info (fn);
  ipa_reference_local_vars_info_t l = info->local;
  if (l)
    {
      if (l->statics_read
	  && l->statics_read != all_module_statics)
	BITMAP_FREE (l->statics_read);
      if (l->statics_written
	  &&l->statics_written != all_module_statics)
	BITMAP_FREE (l->statics_written);
      free (l);
      info->local = NULL;
    }
}

/* Remove all data associated with function FN.  */

static void
clean_function (struct cgraph_node *fn)
{
  ipa_reference_vars_info_t info = get_reference_vars_info (fn);
  ipa_reference_global_vars_info_t g = info->global;

  clean_function_local_data (fn);
  if (g)
    {
      if (g->statics_read
	  && g->statics_read != all_module_statics)
	BITMAP_FREE (g->statics_read);

      if (g->statics_written
	  && g->statics_written != all_module_statics)
	BITMAP_FREE (g->statics_written);

      if (g->statics_not_read
	  && g->statics_not_read != all_module_statics)
	BITMAP_FREE (g->statics_not_read);

      if (g->statics_not_written
	  && g->statics_not_written != all_module_statics)
	BITMAP_FREE (g->statics_not_written);
      free (g);
      info->global = NULL;
    }

  free (get_reference_vars_info (fn));
  set_reference_vars_info (fn, NULL);
}

/* Called when new function is inserted to callgraph late.  */
static void
add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  analyze_function (node);
  visited_nodes = NULL;
}

static bitmap
copy_local_bitmap (bitmap src)
{
  bitmap dst;
  if (!src)
    return NULL;
  if (src == all_module_statics)
    return all_module_statics;
  dst = BITMAP_ALLOC (&local_info_obstack);
  bitmap_copy (dst, src);
  return dst;
}

static bitmap
copy_global_bitmap (bitmap src)
{
  bitmap dst;
  if (!src)
    return NULL;
  if (src == all_module_statics)
    return all_module_statics;
  dst = BITMAP_ALLOC (&global_info_obstack);
  bitmap_copy (dst, src);
  return dst;
}

/* Called when new clone is inserted to callgraph late.  */

static void
duplicate_node_data (struct cgraph_node *src, struct cgraph_node *dst,
	 	     void *data ATTRIBUTE_UNUSED)
{
  ipa_reference_global_vars_info_t ginfo;
  ipa_reference_local_vars_info_t linfo;
  ipa_reference_global_vars_info_t dst_ginfo;
  ipa_reference_local_vars_info_t dst_linfo;

  ginfo = get_global_reference_vars_info (src);
  linfo = get_local_reference_vars_info (src);
  if (!linfo && !ginfo)
    return;
  init_function_info (dst);
  if (linfo)
    {
      dst_linfo = get_local_reference_vars_info (dst);
      dst_linfo->statics_read = copy_local_bitmap (linfo->statics_read);
      dst_linfo->statics_written = copy_local_bitmap (linfo->statics_written);
      dst_linfo->calls_read_all = linfo->calls_read_all;
      dst_linfo->calls_write_all = linfo->calls_write_all;
    }
  if (ginfo)
    {
      get_reference_vars_info (dst)->global = XCNEW (struct ipa_reference_global_vars_info_d);
      dst_ginfo = get_global_reference_vars_info (dst);
      dst_ginfo->statics_read = copy_global_bitmap (ginfo->statics_read);
      dst_ginfo->statics_written = copy_global_bitmap (ginfo->statics_written);
      dst_ginfo->statics_not_read = copy_global_bitmap (ginfo->statics_not_read);
      dst_ginfo->statics_not_written = copy_global_bitmap (ginfo->statics_not_written);
    }
}

/* Called when node is removed.  */

static void
remove_node_data (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (get_reference_vars_info (node))
    clean_function (node);
}

/* Analyze each function in the cgraph to see which global or statics
   are read or written.  */

static void
generate_summary (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  unsigned int index;
  bitmap_iterator bi;
  bitmap module_statics_readonly;
  bitmap bm_temp;

  ipa_init ();
  module_statics_readonly = BITMAP_ALLOC (&local_info_obstack);
  bm_temp = BITMAP_ALLOC (&local_info_obstack);

  /* Process all of the variables first.  */
  FOR_EACH_STATIC_INITIALIZER (vnode)
    analyze_variable (vnode);

  /* Process all of the functions next.

     We do not want to process any of the clones so we check that this
     is a master clone.  However, we do need to process any
     AVAIL_OVERWRITABLE functions (these are never clones) because
     they may cause a static variable to escape.  The code that can
     overwrite such a function cannot access the statics because it
     would not be in the same compilation unit.  When the analysis is
     finished, the computed information of these AVAIL_OVERWRITABLE is
     replaced with worst case info.
  */
  for (node = cgraph_nodes; node; node = node->next)
    if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      analyze_function (node);

  pointer_set_destroy (visited_nodes);
  visited_nodes = NULL;

  /* Prune out the variables that were found to behave badly
     (i.e. have their address taken).  */
  EXECUTE_IF_SET_IN_BITMAP (module_statics_escape, 0, index, bi)
    {
      splay_tree_remove (reference_vars_to_consider, index);
    }

  bitmap_and_compl_into (all_module_statics,
			 module_statics_escape);

  bitmap_and_compl (module_statics_readonly, all_module_statics,
		    module_statics_written);

  /* If the address is not taken, we can unset the addressable bit
     on this variable.  */
  EXECUTE_IF_SET_IN_BITMAP (all_module_statics, 0, index, bi)
    {
      tree var = get_static_decl (index);
      TREE_ADDRESSABLE (var) = 0;
      if (dump_file)
	fprintf (dump_file, "Not TREE_ADDRESSABLE var %s\n",
		 get_static_name (index));
    }

  /* If the variable is never written, we can set the TREE_READONLY
     flag.  Additionally if it has a DECL_INITIAL that is made up of
     constants we can treat the entire global as a constant.  */

  bitmap_and_compl (module_statics_readonly, all_module_statics,
		    module_statics_written);
  EXECUTE_IF_SET_IN_BITMAP (module_statics_readonly, 0, index, bi)
    {
      tree var = get_static_decl (index);

      /* Ignore variables in named sections - changing TREE_READONLY
	 changes the section flags, potentially causing conflicts with
	 other variables in the same named section.  */
      if (DECL_SECTION_NAME (var) == NULL_TREE)
	{
	  TREE_READONLY (var) = 1;
	  if (dump_file)
	    fprintf (dump_file, "read-only var %s\n",
		     get_static_name (index));
	}
    }

  BITMAP_FREE(module_statics_escape);
  BITMAP_FREE(module_statics_written);
  module_statics_escape = NULL;
  module_statics_written = NULL;

  if (dump_file)
    EXECUTE_IF_SET_IN_BITMAP (all_module_statics, 0, index, bi)
      {
	fprintf (dump_file, "\nPromotable global:%s",
		 get_static_name (index));
      }

  for (node = cgraph_nodes; node; node = node->next)
    if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
      {
	ipa_reference_local_vars_info_t l;
	l = get_reference_vars_info (node)->local;

	/* Any variables that are not in all_module_statics are
	   removed from the local maps.  This will include all of the
	   variables that were found to escape in the function
	   scanning.  */
	if (l->statics_read)
	  bitmap_and_into (l->statics_read,
			   all_module_statics);
	if (l->statics_written)
	  bitmap_and_into (l->statics_written,
			   all_module_statics);
      }

  BITMAP_FREE(module_statics_readonly);
  BITMAP_FREE(bm_temp);

  if (dump_file)
    for (node = cgraph_nodes; node; node = node->next)
      if (cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE)
	{
	  ipa_reference_local_vars_info_t l;
	  unsigned int index;
	  bitmap_iterator bi;

	  l = get_reference_vars_info (node)->local;
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


/* Return true if we need to write summary of NODE. */

static bool
write_node_summary_p (struct cgraph_node *node)
{
  gcc_assert (node->global.inlined_to == NULL);
  return (node->analyzed
	  && cgraph_function_body_availability (node) >= AVAIL_OVERWRITABLE
	  && get_reference_vars_info (node) != NULL);
}

/* Serialize the ipa info for lto.  */

static void
ipa_reference_write_summary (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_reference);
  unsigned int count = 0;
  cgraph_node_set_iterator csi;

  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    if (write_node_summary_p (csi_node (csi)))
	count++;

  lto_output_uleb128_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (write_node_summary_p (node))
	{
	  ipa_reference_local_vars_info_t l
	    = get_reference_vars_info (node)->local;
	  unsigned int index;
	  bitmap_iterator bi;
	  lto_cgraph_encoder_t encoder;
	  int node_ref;

	  encoder = ob->decl_state->cgraph_node_encoder;
	  node_ref = lto_cgraph_encoder_encode (encoder, node);
	  lto_output_uleb128_stream (ob->main_stream, node_ref);

	  /* Stream out the statics read.  */
	  if (l->calls_read_all)
	    lto_output_sleb128_stream (ob->main_stream, -1);
	  else
	    {
	      lto_output_sleb128_stream (ob->main_stream,
					 bitmap_count_bits (l->statics_read));
	      EXECUTE_IF_SET_IN_BITMAP (l->statics_read, 0, index, bi)
		lto_output_var_decl_index(ob->decl_state, ob->main_stream,
					  get_static_decl (index));
	    }

	  /* Stream out the statics written.  */
	  if (l->calls_write_all)
	    lto_output_sleb128_stream (ob->main_stream, -1);
	  else
	    {
	      lto_output_sleb128_stream (ob->main_stream,
					 bitmap_count_bits (l->statics_written));
	      EXECUTE_IF_SET_IN_BITMAP (l->statics_written, 0, index, bi)
		lto_output_var_decl_index(ob->decl_state, ob->main_stream,
					  get_static_decl (index));
	    }
	}
    }
  lto_destroy_simple_output_block (ob);
}


/* Deserialize the ipa info for lto.  */

static void
ipa_reference_read_summary (void)
{
  struct lto_file_decl_data ** file_data_vec
    = lto_get_file_decl_data ();
  struct lto_file_decl_data * file_data;
  unsigned int j = 0;

  ipa_init ();

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
	      ipa_reference_local_vars_info_t l;
	      int v_count;
	      lto_cgraph_encoder_t encoder;

	      index = lto_input_uleb128 (ib);
	      encoder = file_data->cgraph_node_encoder;
	      node = lto_cgraph_encoder_deref (encoder, index);
	      l = init_function_info (node);

	      /* Set the statics read.  */
	      v_count = lto_input_sleb128 (ib);
	      if (v_count == -1)
	        l->calls_read_all = true;
	      else
		for (j = 0; j < (unsigned int)v_count; j++)
		  {
		    unsigned int var_index = lto_input_uleb128 (ib);
		    tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								   var_index);
		    add_static_var (v_decl);
		    bitmap_set_bit (l->statics_read, DECL_UID (v_decl));
		  }

	      /* Set the statics written.  */
	      v_count = lto_input_sleb128 (ib);
	      if (v_count == -1)
	        l->calls_write_all = true;
	      else
		for (j = 0; j < (unsigned int)v_count; j++)
		  {
		    unsigned int var_index = lto_input_uleb128 (ib);
		    tree v_decl = lto_file_decl_data_get_var_decl (file_data,
								   var_index);
		    add_static_var (v_decl);
		    bitmap_set_bit (l->statics_written, DECL_UID (v_decl));
		  }
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_reference,
					  ib, data, len);
	}
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
	  common builtins, in particular throw.
	  Indirect calls hsould be only counted and as inliner is replacing them
	  by direct calls, we can conclude if any indirect calls are left in body */
      *read_all = true;
      /* When function does not reutrn, it is safe to ignore anythign it writes
	 to, because the effect will never happen.  */
      if ((flags & (ECF_NOTHROW | ECF_NORETURN))
	  != (ECF_NOTHROW | ECF_NORETURN))
        *write_all = true;
    }
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by ipa_analyze_function
   and ipa_analyze_variable.  */

static unsigned int
propagate (void)
{
  struct cgraph_node *node;
  struct cgraph_node *w;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  int order_pos = ipa_utils_reduced_inorder (order, false, true, NULL);
  int i;

  cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  if (dump_file)
    dump_cgraph (dump_file);

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
      ipa_reference_global_vars_info_t node_g =
	XCNEW (struct ipa_reference_global_vars_info_d);
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

      gcc_assert (!node_info->global);
      node_l = node_info->local;

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
	    get_reference_vars_info (w)->local;

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
	  node_g->statics_read = BITMAP_ALLOC (&global_info_obstack);
	  bitmap_copy (node_g->statics_read,
		       node_l->statics_read);
	}
      if (write_all)
	node_g->statics_written = all_module_statics;
      else
	{
	  node_g->statics_written = BITMAP_ALLOC (&global_info_obstack);
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
	  ipa_reference_local_vars_info_t w_l = w_ri->local;

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
      node_info->global = node_g;
      w_info = (struct ipa_dfs_info *) node->aux;
      w = w_info->next_cycle;
      while (w)
	{
	  ipa_reference_vars_info_t w_ri =
	    get_reference_vars_info (w);

	  gcc_assert (!w_ri->global);
          w_ri->global = XCNEW (struct ipa_reference_global_vars_info_d);
	  w_ri->global->statics_read = copy_global_bitmap (node_g->statics_read);
	  w_ri->global->statics_written = copy_global_bitmap (node_g->statics_written);

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
	  node_g = node_info->global;
	  node_l = node_info->local;
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
	      ipa_reference_local_vars_info_t w_l = w_ri->local;
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
  for (i = 0; i < order_pos; i++ )
    {
      ipa_reference_vars_info_t node_info;
      ipa_reference_global_vars_info_t node_g;
      node = order[i];
      node_info = get_reference_vars_info (node);
      node_g = node_info->global;

      /* Create the complimentary sets.  These are more useful for
	 certain apis.  */
      node_g->statics_not_read = BITMAP_ALLOC (&global_info_obstack);
      node_g->statics_not_written = BITMAP_ALLOC (&global_info_obstack);

      if (node_g->statics_read != all_module_statics)
	bitmap_and_compl (node_g->statics_not_read,
			  all_module_statics,
			  node_g->statics_read);

      if (node_g->statics_written
	  != all_module_statics)
	bitmap_and_compl (node_g->statics_not_written,
			  all_module_statics,
			  node_g->statics_written);
   }

  free (order);

  for (node = cgraph_nodes; node; node = node->next)
    {
      ipa_reference_vars_info_t node_info;
      node_info = get_reference_vars_info (node);
      /* Get rid of the aux information.  */

      if (node->aux)
	{
	  free (node->aux);
	  node->aux = NULL;
	}

      if (cgraph_function_body_availability (node) == AVAIL_OVERWRITABLE)
	clean_function (node);
      else if (node_info)
	clean_function_local_data (node);
    }
  bitmap_obstack_release (&local_info_obstack);
  return 0;
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
 generate_summary,		        /* generate_summary */
 ipa_reference_write_summary,		/* write_summary */
 ipa_reference_read_summary,		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};

#include "gt-ipa-reference.h"
