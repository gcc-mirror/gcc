/* PRU target specific passes
   Copyright (C) 2017-2026 Free Software Foundation, Inc.
   Dimitar Dimitrov <dimitar@dinux.eu>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "context.h"
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "function.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-expr.h"
#include "cgraph.h"
#include "tree-pass.h"

#include "pru-protos.h"

namespace {

/* Scan the tree to ensure that the compiled code by GCC
   conforms to the TI ABI specification.  If GCC cannot
   output a conforming code, raise an error.  */
const pass_data pass_data_pru_tiabi_check =
{
  SIMPLE_IPA_PASS, /* type */
  "*pru_tiabi_check", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

/* Implementation class for the TI ABI compliance-check pass.  */
class pass_pru_tiabi_check : public simple_ipa_opt_pass
{
public:
  pass_pru_tiabi_check (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_pru_tiabi_check, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

  virtual bool gate (function *)
  {
    return pru_current_abi == PRU_ABI_TI;
  }

}; // class pass_pru_tiabi_check

/* Issue an error diagnostic if the given TYPE is not compatible with
   TI's ABI.  */
static void
check_type_tiabi_compatibility (tree type, location_t loc,
				hash_set<tree> *visited_nodes)
{
  /* Do not visit the same type twice.  */
  if (visited_nodes->add (type))
    return;

  if (POINTER_TYPE_P (type))
    {
      /* TODO: All declarations and statements share the same object for a
	 function pointer tree type.  So if there are several variables
	 with the same type (function pointer with same function signature),
	 only the first declaration would get a diagnostic.  */
      if (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (type)))
	{
	  location_t ptrloc = DECL_P (type) ? DECL_SOURCE_LOCATION (type) : loc;
	  error_at (ptrloc,
		    "function pointers not supported with %<-mabi=ti%> option");
	}
      else
	check_type_tiabi_compatibility (TREE_TYPE (type), loc, visited_nodes);
    }
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    if (DECL_BIT_FIELD (field))
	      error_at (DECL_SOURCE_LOCATION (field),
			"bit-fields not supported with %<-mabi=ti%> option");

	    check_type_tiabi_compatibility (TREE_TYPE (field),
					    DECL_SOURCE_LOCATION (field),
					    visited_nodes);
	  }
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    check_type_tiabi_compatibility (TREE_TYPE (type), loc, visited_nodes);
}

/* Check the GCC function declaration FNTYPE for TI ABI compatibility.  */
static void
check_function_decl (tree fntype, location_t loc,
		     hash_set<tree> *visited_nodes)
{
  /* Do not visit the same type twice.  */
  if (visited_nodes->add (fntype))
    return;

  /* GCC does not check if the RETURN VALUE pointer is NULL,
     so do not allow GCC functions with large return values.  */
  if (!VOID_TYPE_P (TREE_TYPE (fntype))
      && pru_return_in_memory (TREE_TYPE (fntype), fntype))
    error_at (loc,
	      "large return values not supported with %<-mabi=ti%> option");

  /* Rest of checks for the returned type.  */
  check_type_tiabi_compatibility (TREE_TYPE (fntype), loc, visited_nodes);

  /* Check this function's arguments.  */
  for (tree p = TYPE_ARG_TYPES (fntype); p; p = TREE_CHAIN (p))
    check_type_tiabi_compatibility (TREE_VALUE (p), loc, visited_nodes);
}

/* Callback for walk_gimple_seq that checks TP tree for TI ABI compliance.
   Note that TP would have been marked as visited before calling
   this function.  But the underlying type of TP may or may not have
   been visited before.  */
static tree
check_op_callback (tree *tp, int *, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;

  if (RECORD_OR_UNION_TYPE_P (*tp) || TREE_CODE (*tp) == ENUMERAL_TYPE)
    {
      /* Forward declarations have NULL tree type.  Skip them.  */
      if (TREE_TYPE (*tp) == NULL)
	return NULL;
    }

  /* TODO - why C++ leaves INTEGER_TYPE forward declarations around?  */
  if (TREE_TYPE (*tp) == NULL)
    return NULL;

  const tree type = TREE_TYPE (*tp);

  /* Direct function calls are allowed, obviously.  */
  gcall *call = dyn_cast <gcall *> (gsi_stmt (wi->gsi));
  if (call
      && tp == gimple_call_fn_ptr (call)
      && gimple_call_fndecl (call))
    return NULL;

  switch (TREE_CODE (type))
    {
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	/* Has this function already been inspected?  */
	if (wi->pset->add (type))
	  return NULL;

	/* Note: Do not enforce a small return value.  It is safe to
	   call any TI ABI function from GCC, since GCC will
	   never pass NULL.  */

	/* Check arguments for function pointers.  */
	for (tree p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	  check_type_tiabi_compatibility (TREE_VALUE (p),
					  gimple_location (wi->stmt),
					  wi->pset);
	break;
      }
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* walk_tree would not descend and visit field declarations.
	   So do it here.  */
	check_type_tiabi_compatibility (type,
					gimple_location (wi->stmt),
					wi->pset);
	break;
      }
    default:
      break;
    }
  return NULL;
}

/* Pass implementation.  */
unsigned
pass_pru_tiabi_check::execute (function *)
{
  struct cgraph_node *node;
  hash_set<tree> visited_nodes;
  symtab_node *snode;

  FOR_EACH_SYMBOL (snode)
    {
      tree decl = snode->decl;
      if (decl)
	{
	  if (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (decl)))
	    check_function_decl (TREE_TYPE (decl),
				 DECL_SOURCE_LOCATION (decl),
				 &visited_nodes);
	  else
	    check_type_tiabi_compatibility (TREE_TYPE (decl),
					    DECL_SOURCE_LOCATION (decl),
					    &visited_nodes);
	}
    }

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fn = node->get_fun ();
      gimple_stmt_iterator gsi;
      basic_block bb;

      FOR_EACH_BB_FN (bb, fn)
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      struct walk_stmt_info wi;
	      memset (&wi, 0, sizeof (wi));
	      wi.info = NULL;
	      wi.want_locations = true;

	      wi.pset = &visited_nodes;

	      /* Check the function body.  */
	      walk_gimple_stmt (&gsi, NULL, check_op_callback, &wi);
	    }
	}
    }
  return 0;
}

} // anon namespace

simple_ipa_opt_pass *
make_pru_tiabi_check (gcc::context *ctxt)
{
  return new pass_pru_tiabi_check (ctxt);
}

namespace {

/* Scan the tree to ensure that the compiled code by GCC
   conforms to the non-standard minimal runtime.  */
const pass_data pass_data_pru_minrt_check =
{
  GIMPLE_PASS, /* type */
  "*pru_minrt_check", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

/* Implementation class for the minrt compliance-check pass.  */
class pass_pru_minrt_check : public gimple_opt_pass
{
public:
  pass_pru_minrt_check (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_pru_minrt_check, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

  virtual bool gate (function *)
  {
    return TARGET_MINRT;
  }

}; // class pass_pru_minrt_check

/* Pass implementation.  */
unsigned
pass_pru_minrt_check::execute (function *fun)
{
  const_tree fntype = TREE_TYPE (fun->decl);

  if (id_equal (DECL_NAME (fun->decl), "main"))
    {
      /* Argument list always ends with VOID_TYPE, so subtract one
	 to get the number of function arguments.  */
      const unsigned num_args = list_length (TYPE_ARG_TYPES (fntype)) - 1;

      if (num_args != 0)
	error_at (DECL_SOURCE_LOCATION (fun->decl), "function %<main%> "
		  "must have no arguments when using the "
		  "%<-minrt%> option");

      /* The required CFG analysis to detect when a functions would never
	 return is available only with -O1 and higher.  */
      if (optimize >= 1 && !TREE_THIS_VOLATILE (fun->decl))
	error_at (DECL_SOURCE_LOCATION (fun->decl), "function %<main%> "
		  "must never return when using the "
		  "%<-minrt%> option");
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pru_minrt_check (gcc::context *ctxt)
{
  return new pass_pru_minrt_check (ctxt);
}
