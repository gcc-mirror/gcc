/* Pointer Bounds Checker IPA passes.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

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
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "stringpool.h"
#include "lto-streamer.h"
#include "stor-layout.h"
#include "calls.h"
#include "cgraph.h"
#include "tree-chkp.h"
#include "tree-inline.h"
#include "ipa-chkp.h"

/*  Pointer Bounds Checker has two IPA passes to support code instrumentation.

    In instrumented code each pointer is provided with bounds.  For input
    pointer parameters it means we also have bounds passed.  For calls it
    means we have additional bounds arguments for pointer arguments.

    To have all IPA optimizations working correctly we have to express
    dataflow between passed and received bounds explicitly via additional
    entries in function declaration arguments list and in function type.
    Since we may have both instrumented and not instrumented code at the
    same time, we cannot replace all original functions with their
    instrumented variants.  Therefore we create clones (versions) instead.

    Instrumentation clones creation is a separate IPA pass which is a part
    of early local passes.  Clones are created after SSA is built (because
    instrumentation pass works on SSA) and before any transformations
    which may change pointer flow and therefore lead to incorrect code
    instrumentation (possibly causing false bounds check failures).

    Instrumentation clones have pointer bounds arguments added right after
    pointer arguments.  Clones have assembler name of the original
    function with suffix added.  New assembler name is in transparent
    alias chain with the original name.  Thus we expect all calls to the
    original and instrumented functions look similar in assembler.

    During instrumentation versioning pass we create instrumented versions
    of all function with body and also for all their aliases and thunks.
    Clones for functions with no body are created on demand (usually
    during call instrumentation).

    Original and instrumented function nodes are connected with IPA
    reference IPA_REF_CHKP.  It is mostly done to have reachability
    analysis working correctly.  We may have no references to the
    instrumented function in the code but it still should be counted
    as reachable if the original function is reachable.

    When original function bodies are not needed anymore we release
    them and transform functions into a special kind of thunks.  Each
    thunk has a call edge to the instrumented version.  These thunks
    help to keep externally visible instrumented functions visible
    when linker resolution files are used.  Linker has no info about
    connection between original and instrumented function and
    therefore we may wrongly decide (due to difference in assembler
    names) that instrumented function version is local and can be
    removed.  */

#define CHKP_BOUNDS_OF_SYMBOL_PREFIX "__chkp_bounds_of_"
#define CHKP_WRAPPER_SYMBOL_PREFIX "__mpx_wrapper_"

/* Return 1 calls to FNDECL should be replaced with
   a call to wrapper function.  */
bool
chkp_wrap_function (tree fndecl)
{
  if (!flag_chkp_use_wrappers)
    return false;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_STRLEN:
	case BUILT_IN_STRCPY:
	case BUILT_IN_STRNCPY:
	case BUILT_IN_STPCPY:
	case BUILT_IN_STPNCPY:
	case BUILT_IN_STRCAT:
	case BUILT_IN_STRNCAT:
	case BUILT_IN_MEMCPY:
	case BUILT_IN_MEMPCPY:
	case BUILT_IN_MEMSET:
	case BUILT_IN_MEMMOVE:
	case BUILT_IN_BZERO:
	case BUILT_IN_MALLOC:
	case BUILT_IN_CALLOC:
	case BUILT_IN_REALLOC:
	  return 1;

	default:
	  return 0;
	}
    }

  return false;
}

static const char *
chkp_wrap_function_name (tree fndecl)
{
  gcc_assert (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_STRLEN:
      return CHKP_WRAPPER_SYMBOL_PREFIX "strlen";
    case BUILT_IN_STRCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "strcpy";
    case BUILT_IN_STRNCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "strncpy";
    case BUILT_IN_STPCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "stpcpy";
    case BUILT_IN_STPNCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "stpncpy";
    case BUILT_IN_STRCAT:
      return CHKP_WRAPPER_SYMBOL_PREFIX "strcat";
    case BUILT_IN_STRNCAT:
      return CHKP_WRAPPER_SYMBOL_PREFIX "strncat";
    case BUILT_IN_MEMCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "memcpy";
    case BUILT_IN_MEMPCPY:
      return CHKP_WRAPPER_SYMBOL_PREFIX "mempcpy";
    case BUILT_IN_MEMSET:
      return CHKP_WRAPPER_SYMBOL_PREFIX "memset";
    case BUILT_IN_MEMMOVE:
      return CHKP_WRAPPER_SYMBOL_PREFIX "memmove";
    case BUILT_IN_BZERO:
      return CHKP_WRAPPER_SYMBOL_PREFIX "bzero";
    case BUILT_IN_MALLOC:
      return CHKP_WRAPPER_SYMBOL_PREFIX "malloc";
    case BUILT_IN_CALLOC:
      return CHKP_WRAPPER_SYMBOL_PREFIX "calloc";
    case BUILT_IN_REALLOC:
      return CHKP_WRAPPER_SYMBOL_PREFIX "realloc";

    default:
      gcc_unreachable ();
    }

  return "";
}

/* Build a clone of FNDECL with a modified name.  */

static tree
chkp_build_instrumented_fndecl (tree fndecl)
{
  tree new_decl = copy_node (fndecl);
  tree new_name;
  std::string s;

  /* called_as_built_in checks DECL_NAME to identify calls to
     builtins.  We want instrumented calls to builtins to be
     recognized by called_as_built_in.  Therefore use original
     DECL_NAME for cloning with no prefixes.  */
  s = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  s += ".chkp";
  DECL_NAME (new_decl) = get_identifier (s.c_str ());

  /* References to the original and to the instrumented version
     should look the same in the output assembly.  And we cannot
     use the same assembler name for the instrumented version
     because it conflicts with decl merging algorithms in LTO.
     Achieve the result by using transparent alias name for the
     instrumented version.  */
  if (chkp_wrap_function(fndecl))
    {
      new_name = get_identifier (chkp_wrap_function_name (fndecl));
      DECL_VISIBILITY (new_decl) = VISIBILITY_DEFAULT;
    }
  else
    {
      s = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl));
      s += ".chkp";
      new_name = get_identifier (s.c_str ());
      IDENTIFIER_TRANSPARENT_ALIAS (new_name) = 1;
      TREE_CHAIN (new_name) = DECL_ASSEMBLER_NAME (fndecl);
    }
  SET_DECL_ASSEMBLER_NAME (new_decl, new_name);

  /* For functions with body versioning will make a copy of arguments.
     For functions with no body we need to do it here.  */
  if (!gimple_has_body_p (fndecl))
    DECL_ARGUMENTS (new_decl) = copy_list (DECL_ARGUMENTS (fndecl));

  /* We are going to modify attributes list and therefore should
     make own copy.  */
  DECL_ATTRIBUTES (new_decl) = copy_list (DECL_ATTRIBUTES (fndecl));

  /* Change builtin function code.  */
  if (DECL_BUILT_IN (new_decl))
    {
      gcc_assert (DECL_BUILT_IN_CLASS (new_decl) == BUILT_IN_NORMAL);
      gcc_assert (DECL_FUNCTION_CODE (new_decl) < BEGIN_CHKP_BUILTINS);
      DECL_FUNCTION_CODE (new_decl)
	= (enum built_in_function)(DECL_FUNCTION_CODE (new_decl)
				   + BEGIN_CHKP_BUILTINS + 1);
    }

  return new_decl;
}


/* Fix operands of attribute from ATTRS list named ATTR_NAME.
   Integer operands are replaced with values according to
   INDEXES map having LEN elements.  For operands out of len
   we just add DELTA.  */

static void
chkp_map_attr_arg_indexes (tree attrs, const char *attr_name,
			   unsigned *indexes, int len, int delta)
{
  tree attr = lookup_attribute (attr_name, attrs);
  tree op;

  if (!attr)
    return;

  TREE_VALUE (attr) = copy_list (TREE_VALUE (attr));
  for (op = TREE_VALUE (attr); op; op = TREE_CHAIN (op))
    {
      int idx;

      if (TREE_CODE (TREE_VALUE (op)) != INTEGER_CST)
	continue;

      idx = TREE_INT_CST_LOW (TREE_VALUE (op));

      /* If idx exceeds indexes length then we just
	 keep it at the same distance from the last
	 known arg.  */
      if (idx > len)
	idx += delta;
      else
	idx = indexes[idx - 1] + 1;
      TREE_VALUE (op) = build_int_cst (TREE_TYPE (TREE_VALUE (op)), idx);
    }
}

/* Make a copy of function type ORIG_TYPE adding pointer
   bounds as additional arguments.  */

tree
chkp_copy_function_type_adding_bounds (tree orig_type)
{
  tree type;
  tree arg_type, attrs;
  unsigned len = list_length (TYPE_ARG_TYPES (orig_type));
  unsigned *indexes = XALLOCAVEC (unsigned, len);
  unsigned idx = 0, new_idx = 0;

  for (arg_type = TYPE_ARG_TYPES (orig_type);
       arg_type;
       arg_type = TREE_CHAIN (arg_type))
    if (TREE_VALUE (arg_type) == void_type_node)
      continue;
    else if (BOUNDED_TYPE_P (TREE_VALUE (arg_type))
	     || pass_by_reference (NULL, TYPE_MODE (TREE_VALUE (arg_type)),
				   TREE_VALUE (arg_type), true)
	     || chkp_type_has_pointer (TREE_VALUE (arg_type)))
      break;

  /* We may use original type if there are no bounds passed.  */
  if (!arg_type)
    return orig_type;

  type = build_distinct_type_copy (orig_type);
  TYPE_ARG_TYPES (type) = copy_list (TYPE_ARG_TYPES (type));

  for (arg_type = TYPE_ARG_TYPES (type);
       arg_type;
       arg_type = TREE_CHAIN (arg_type))
    {
      indexes[idx++] = new_idx++;

      /* pass_by_reference returns 1 for void type,
	 so check for it first.  */
      if (TREE_VALUE (arg_type) == void_type_node)
	continue;
      else if (BOUNDED_TYPE_P (TREE_VALUE (arg_type))
	       || pass_by_reference (NULL, TYPE_MODE (TREE_VALUE (arg_type)),
				     TREE_VALUE (arg_type), true))
	{
	  tree new_type = build_tree_list (NULL_TREE,
					   pointer_bounds_type_node);
	  TREE_CHAIN (new_type) = TREE_CHAIN (arg_type);
	  TREE_CHAIN (arg_type) = new_type;

	  arg_type = TREE_CHAIN (arg_type);
	  new_idx++;
	}
      else if (chkp_type_has_pointer (TREE_VALUE (arg_type)))
	{
	  bitmap slots = BITMAP_ALLOC (NULL);
	  bitmap_iterator bi;
	  unsigned bnd_no;

	  chkp_find_bound_slots (TREE_VALUE (arg_type), slots);

	  EXECUTE_IF_SET_IN_BITMAP (slots, 0, bnd_no, bi)
	    {
	      tree new_type = build_tree_list (NULL_TREE,
					       pointer_bounds_type_node);
	      TREE_CHAIN (new_type) = TREE_CHAIN (arg_type);
	      TREE_CHAIN (arg_type) = new_type;

	      arg_type = TREE_CHAIN (arg_type);
	      new_idx++;
	    }
	  BITMAP_FREE (slots);
	}
    }

  /* If function type has attribute with arg indexes then
     we have to copy it fixing attribute ops.  Map for
     fixing is in indexes array.  */
  attrs = TYPE_ATTRIBUTES (type);
  if (lookup_attribute ("nonnull", attrs)
      || lookup_attribute ("format", attrs)
      || lookup_attribute ("format_arg", attrs))
    {
      int delta = new_idx - len;
      attrs = copy_list (TYPE_ATTRIBUTES (type));
      chkp_map_attr_arg_indexes (attrs, "nonnull", indexes, len, delta);
      chkp_map_attr_arg_indexes (attrs, "format", indexes, len, delta);
      chkp_map_attr_arg_indexes (attrs, "format_arg", indexes, len, delta);
      TYPE_ATTRIBUTES (type) = attrs;
    }

  return type;
}

/* For given function FNDECL add bounds arguments to arguments
   list.  */

static void
chkp_add_bounds_params_to_function (tree fndecl)
{
  tree arg;

  for (arg = DECL_ARGUMENTS (fndecl); arg; arg = DECL_CHAIN (arg))
    if (BOUNDED_P (arg))
      {
	std::string new_name = CHKP_BOUNDS_OF_SYMBOL_PREFIX;
	tree new_arg;

	if (DECL_NAME (arg))
	  new_name += IDENTIFIER_POINTER (DECL_NAME (arg));
	else
	  {
	    char uid[25];
	    snprintf (uid, 25, "D.%u", DECL_UID (arg));
	    new_name += uid;
	  }

	new_arg = build_decl (DECL_SOURCE_LOCATION (arg), PARM_DECL,
			      get_identifier (new_name.c_str ()),
			      pointer_bounds_type_node);
	DECL_ARG_TYPE (new_arg) = pointer_bounds_type_node;
	DECL_CONTEXT (new_arg) = DECL_CONTEXT (arg);
	DECL_ARTIFICIAL (new_arg) = 1;
	DECL_CHAIN (new_arg) = DECL_CHAIN (arg);
	DECL_CHAIN (arg) = new_arg;

	arg = DECL_CHAIN (arg);

      }
    else if (chkp_type_has_pointer (TREE_TYPE (arg)))
      {
	tree orig_arg = arg;
	bitmap slots = BITMAP_ALLOC (NULL);
	bitmap_iterator bi;
	unsigned bnd_no;

	chkp_find_bound_slots (TREE_TYPE (arg), slots);

	EXECUTE_IF_SET_IN_BITMAP (slots, 0, bnd_no, bi)
	  {
	    std::string new_name = CHKP_BOUNDS_OF_SYMBOL_PREFIX;
	    tree new_arg;
	    char offs[25];

	    if (DECL_NAME (orig_arg))
	      new_name += IDENTIFIER_POINTER (DECL_NAME (orig_arg));
	    else
	      {
		snprintf (offs, 25, "D.%u", DECL_UID (arg));
		new_name += offs;
	      }
	    snprintf (offs, 25, "__%u", bnd_no * POINTER_SIZE / BITS_PER_UNIT);

	    new_arg = build_decl (DECL_SOURCE_LOCATION (orig_arg),
				  PARM_DECL,
				  get_identifier (new_name.c_str ()),
				  pointer_bounds_type_node);
	    DECL_ARG_TYPE (new_arg) = pointer_bounds_type_node;
	    DECL_CONTEXT (new_arg) = DECL_CONTEXT (orig_arg);
	    DECL_ARTIFICIAL (new_arg) = 1;
	    DECL_CHAIN (new_arg) = DECL_CHAIN (arg);
	    DECL_CHAIN (arg) = new_arg;

	    arg = DECL_CHAIN (arg);
	  }
	BITMAP_FREE (slots);
      }

  TREE_TYPE (fndecl) =
    chkp_copy_function_type_adding_bounds (TREE_TYPE (fndecl));
}

/* Return an instrumentation clone for builtin function
   FNDECL.  Create one if needed.  */

tree
chkp_maybe_clone_builtin_fndecl (tree fndecl)
{
  tree clone;
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  gcc_assert (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	      && fcode < BEGIN_CHKP_BUILTINS);

  fcode = (enum built_in_function) (fcode + BEGIN_CHKP_BUILTINS + 1);
  clone = builtin_decl_explicit (fcode);
  if (clone)
    return clone;

  clone = chkp_build_instrumented_fndecl (fndecl);
  chkp_add_bounds_params_to_function (clone);

  gcc_assert (DECL_FUNCTION_CODE (clone) == fcode);

  set_builtin_decl (fcode, clone, false);

  return clone;
}

/* Return 1 if function FNDECL should be instrumented.  */

bool
chkp_instrumentable_p (tree fndecl)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  return (!lookup_attribute ("bnd_legacy", DECL_ATTRIBUTES (fndecl))
	  && (!flag_chkp_instrument_marked_only
	      || lookup_attribute ("bnd_instrument", DECL_ATTRIBUTES (fndecl)))
	  && (!fn || !copy_forbidden (fn)));
}

/* Return clone created for instrumentation of NODE or NULL.  */

cgraph_node *
chkp_maybe_create_clone (tree fndecl)
{
  cgraph_node *node = cgraph_node::get_create (fndecl);
  cgraph_node *clone = node->instrumented_version;

  gcc_assert (!node->instrumentation_clone);

  if (DECL_BUILT_IN (fndecl)
      && (DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL
	  || DECL_FUNCTION_CODE (fndecl) >= BEGIN_CHKP_BUILTINS))
    return NULL;

  clone = node->instrumented_version;

  /* Some instrumented builtin function calls may be optimized and
     cgraph nodes may be removed as unreachable.  Later optimizations
     may generate new calls to removed functions and in this case
     we have to recreate cgraph node.  FUNCTION_DECL for instrumented
     builtin still exists and should be reused in such case.  */
  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && fndecl == builtin_decl_explicit (DECL_FUNCTION_CODE (fndecl))
      && !clone)
    {
      enum built_in_function fncode = DECL_FUNCTION_CODE (fndecl);
      tree new_decl;

      fncode = (enum built_in_function) (fncode + BEGIN_CHKP_BUILTINS + 1);
      new_decl = builtin_decl_explicit (fncode);

      /* We've actually already created an instrumented clone once.
	 Restore it.  */
      if (new_decl)
	{
	  clone = cgraph_node::get (new_decl);

	  if (!clone)
	    {
	      gcc_assert (!gimple_has_body_p (fndecl));
	      clone = cgraph_node::get_create (new_decl);
	      clone->externally_visible = node->externally_visible;
	      clone->local = node->local;
	      clone->address_taken = node->address_taken;
	      clone->thunk = node->thunk;
	      clone->alias = node->alias;
	      clone->weakref = node->weakref;
	      clone->cpp_implicit_alias = node->cpp_implicit_alias;
	      clone->orig_decl = fndecl;
	      clone->instrumentation_clone = true;
	    }

	  clone->instrumented_version = node;
	  node->instrumented_version = clone;
	}
    }

  if (!clone)
    {
      tree new_decl = chkp_build_instrumented_fndecl (fndecl);
      struct cgraph_edge *e;
      struct ipa_ref *ref;
      int i;

      clone = node->create_version_clone (new_decl, vNULL, NULL);
      clone->externally_visible = node->externally_visible;
      clone->local = node->local;
      clone->address_taken = node->address_taken;
      clone->thunk = node->thunk;
      clone->alias = node->alias;
      clone->weakref = node->weakref;
      clone->cpp_implicit_alias = node->cpp_implicit_alias;
      clone->instrumented_version = node;
      clone->orig_decl = fndecl;
      clone->instrumentation_clone = true;
      node->instrumented_version = clone;

      if (gimple_has_body_p (fndecl))
	{
	  gcc_assert (chkp_instrumentable_p (fndecl));
	  tree_function_versioning (fndecl, new_decl, NULL, false,
				    NULL, false, NULL, NULL);
	  clone->lowered = true;
	}

      /* New params are inserted after versioning because it
	 actually copies args list from the original decl.  */
      chkp_add_bounds_params_to_function (new_decl);

      /* Remember builtin fndecl.  */
      if (DECL_BUILT_IN_CLASS (clone->decl) == BUILT_IN_NORMAL
	  && fndecl == builtin_decl_explicit (DECL_FUNCTION_CODE (fndecl)))
	{
	  gcc_assert (!builtin_decl_explicit (DECL_FUNCTION_CODE (clone->decl)));
	  set_builtin_decl (DECL_FUNCTION_CODE (clone->decl),
			    clone->decl, false);
	}

      /* Clones have the same comdat group as originals.  */
      if (node->same_comdat_group
	  || (DECL_ONE_ONLY (node->decl)
	      && !DECL_EXTERNAL (node->decl)))
	clone->add_to_same_comdat_group (node);

      if (gimple_has_body_p (fndecl))
	symtab->call_cgraph_insertion_hooks (clone);

      /* Clone all aliases.  */
      for (i = 0; node->iterate_direct_aliases (i, ref); i++)
	chkp_maybe_create_clone (ref->referring->decl);

      /* Clone all thunks.  */
      for (e = node->callers; e; e = e->next_caller)
	if (e->caller->thunk.thunk_p
	    && !e->caller->thunk.add_pointer_bounds_args
	    && !e->caller->instrumentation_clone)
	  {
	    struct cgraph_node *thunk
	      = chkp_maybe_create_clone (e->caller->decl);
	    /* Redirect thunk clone edge to the node clone.  */
	    thunk->callees->redirect_callee (clone);
	  }

      /* For aliases and thunks we should make sure target is cloned
	 to have proper references and edges.  */
      if (node->thunk.thunk_p)
	chkp_maybe_create_clone (node->callees->callee->decl);
      else if (node->alias)
	{
	  struct cgraph_node *target;

	  ref = node->ref_list.first_reference ();
	  if (ref)
	    {
	      target = chkp_maybe_create_clone (ref->referred->decl);
	      clone->create_reference (target, IPA_REF_ALIAS);
	    }

	  if (node->alias_target)
	    {
	      if (TREE_CODE (node->alias_target) == FUNCTION_DECL)
		{
		  target = chkp_maybe_create_clone (node->alias_target);
		  clone->alias_target = target->decl;
		}
	      else
		clone->alias_target = node->alias_target;
	    }
	}

      /* Add IPA reference.  It's main role is to keep instrumented
	 version reachable while original node is reachable.  */
      ref = node->create_reference (clone, IPA_REF_CHKP, NULL);
    }

  return clone;
}

/* Create clone for all functions to be instrumented.  */

static unsigned int
chkp_versioning (void)
{
  struct cgraph_node *node;
  const char *reason;

  bitmap_obstack_initialize (NULL);

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      tree decl = node->decl;
      if (!node->instrumentation_clone
	  && !node->instrumented_version
	  && !node->alias
	  && !node->thunk.thunk_p
	  && (!DECL_BUILT_IN (decl)
	      || (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
		  && DECL_FUNCTION_CODE (decl) < BEGIN_CHKP_BUILTINS)))
	{
	  if (chkp_instrumentable_p (decl))
	    chkp_maybe_create_clone (decl);
	  else if ((reason = copy_forbidden (DECL_STRUCT_FUNCTION (decl))))
	    {
	      if (warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wchkp,
			      "function cannot be instrumented"))
		inform (DECL_SOURCE_LOCATION (decl), reason, decl);
	    }
	}
    }

  /* Mark all aliases and thunks of functions with no instrumented
     version as legacy function.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      if (!node->instrumentation_clone
	  && !node->instrumented_version
	  && (node->alias || node->thunk.thunk_p)
	  && !lookup_attribute ("bnd_legacy", DECL_ATTRIBUTES (node->decl)))
	DECL_ATTRIBUTES (node->decl)
	  = tree_cons (get_identifier ("bnd_legacy"), NULL,
		       DECL_ATTRIBUTES (node->decl));
    }

  bitmap_obstack_release (NULL);

  return 0;
}

/* In this pass we remove bodies of functions having
   instrumented version.  Functions with removed bodies
   become a special kind of thunks to provide a connection
   between calls to the original version and instrumented
   function.  */

static unsigned int
chkp_produce_thunks (bool early)
{
  struct cgraph_node *node;

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      if (!node->instrumentation_clone
	  && node->instrumented_version
	  && gimple_has_body_p (node->decl)
	  && gimple_has_body_p (node->instrumented_version->decl)
	  && (!lookup_attribute ("always_inline", DECL_ATTRIBUTES (node->decl))
	      || !early))
	{
	  node->release_body ();
	  node->remove_callees ();
	  node->remove_all_references ();

	  node->thunk.thunk_p = true;
	  node->thunk.add_pointer_bounds_args = true;
	  node->create_edge (node->instrumented_version, NULL,
			     0, CGRAPH_FREQ_BASE);
	  node->create_reference (node->instrumented_version,
			       IPA_REF_CHKP, NULL);
	  /* Thunk shouldn't be a cdtor.  */
	  DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
	  DECL_STATIC_DESTRUCTOR (node->decl) = 0;
	}
    }

  /* Mark instrumentation clones created for aliases and thunks
     as insttrumented so they could be removed as unreachable
     now.  */
  if (!early)
    {
      FOR_EACH_DEFINED_FUNCTION (node)
      {
	if (node->instrumentation_clone
	    && (node->alias || node->thunk.thunk_p)
	    && !chkp_function_instrumented_p (node->decl))
	  chkp_function_mark_instrumented (node->decl);
      }
    }

  return TODO_remove_functions;
}

const pass_data pass_data_ipa_chkp_versioning =
{
  SIMPLE_IPA_PASS, /* type */
  "chkp_versioning", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0 /* todo_flags_finish */
};

const pass_data pass_data_ipa_chkp_early_produce_thunks =
{
  SIMPLE_IPA_PASS, /* type */
  "chkp_ecleanup", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0 /* todo_flags_finish */
};

const pass_data pass_data_ipa_chkp_produce_thunks =
{
  SIMPLE_IPA_PASS, /* type */
  "chkp_cleanup", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0 /* todo_flags_finish */
};

class pass_ipa_chkp_versioning : public simple_ipa_opt_pass
{
public:
  pass_ipa_chkp_versioning (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_chkp_versioning, ctxt)
  {}

  /* opt_pass methods: */
  virtual opt_pass * clone ()
    {
      return new pass_ipa_chkp_versioning (m_ctxt);
    }

  virtual bool gate (function *)
    {
      return flag_check_pointer_bounds;
    }

  virtual unsigned int execute (function *)
    {
      return chkp_versioning ();
    }

}; // class pass_ipa_chkp_versioning

class pass_ipa_chkp_early_produce_thunks : public simple_ipa_opt_pass
{
public:
  pass_ipa_chkp_early_produce_thunks (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_chkp_early_produce_thunks, ctxt)
  {}

  /* opt_pass methods: */
  virtual opt_pass * clone ()
    {
      return new pass_ipa_chkp_early_produce_thunks (m_ctxt);
    }

  virtual bool gate (function *)
    {
      return flag_check_pointer_bounds;
    }

  virtual unsigned int execute (function *)
    {
      return chkp_produce_thunks (true);
    }

}; // class pass_chkp_produce_thunks

class pass_ipa_chkp_produce_thunks : public simple_ipa_opt_pass
{
public:
  pass_ipa_chkp_produce_thunks (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_chkp_produce_thunks, ctxt)
  {}

  /* opt_pass methods: */
  virtual opt_pass * clone ()
    {
      return new pass_ipa_chkp_produce_thunks (m_ctxt);
    }

  virtual bool gate (function *)
    {
      return flag_check_pointer_bounds;
    }

  virtual unsigned int execute (function *)
    {
      return chkp_produce_thunks (false);
    }

}; // class pass_chkp_produce_thunks

simple_ipa_opt_pass *
make_pass_ipa_chkp_versioning (gcc::context *ctxt)
{
  return new pass_ipa_chkp_versioning (ctxt);
}

simple_ipa_opt_pass *
make_pass_ipa_chkp_early_produce_thunks (gcc::context *ctxt)
{
  return new pass_ipa_chkp_early_produce_thunks (ctxt);
}

simple_ipa_opt_pass *
make_pass_ipa_chkp_produce_thunks (gcc::context *ctxt)
{
  return new pass_ipa_chkp_produce_thunks (ctxt);
}
