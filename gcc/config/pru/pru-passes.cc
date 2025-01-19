/* PRU target specific passes
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
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
#include "tree-pass.h"

#include "pru-protos.h"

namespace {

/* Scan the tree to ensure that the compiled code by GCC
   conforms to the TI ABI specification.  If GCC cannot
   output a conforming code, raise an error.  */
const pass_data pass_data_pru_tiabi_check =
{
  GIMPLE_PASS, /* type */
  "*pru_tiabi_check", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

/* Implementation class for the TI ABI compliance-check pass.  */
class pass_pru_tiabi_check : public gimple_opt_pass
{
public:
  pass_pru_tiabi_check (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_pru_tiabi_check, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

  virtual bool gate (function *)
  {
    return pru_current_abi == PRU_ABI_TI;
  }

}; // class pass_pru_tiabi_check

/* Return 1 if type TYPE is a pointer to function type or a
   structure having a pointer to function type as one of its fields.
   Otherwise return 0.  */
static bool
chkp_type_has_function_pointer (const_tree type)
{
  bool res = false;

  if (POINTER_TYPE_P (type) && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (type)))
    res = true;
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  res = res || chkp_type_has_function_pointer (TREE_TYPE (field));
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    res = chkp_type_has_function_pointer (TREE_TYPE (type));

  return res;
}

/* Check the function declaration FNTYPE for TI ABI compatibility.  */
static void
chk_function_decl (const_tree fntype, location_t call_location)
{
  /* GCC does not check if the RETURN VALUE pointer is NULL,
     so do not allow GCC functions with large return values.  */
  if (!VOID_TYPE_P (TREE_TYPE (fntype))
      && pru_return_in_memory (TREE_TYPE (fntype), fntype))
    error_at (call_location,
	      "large return values not supported with %<-mabi=ti%> option");

  /* Check this function's arguments.  */
  for (tree p = TYPE_ARG_TYPES (fntype); p; p = TREE_CHAIN (p))
    {
      tree arg_type = TREE_VALUE (p);
      if (chkp_type_has_function_pointer (arg_type))
	error_at (call_location,
		  "function pointers not supported with %<-mabi=ti%> option");
    }
}

/* Callback for walk_gimple_seq that checks TP tree for TI ABI compliance.  */
static tree
check_op_callback (tree *tp, int *walk_subtrees, void *data)
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
	  /* Note: Do not enforce a small return value.  It is safe to
	     call any TI ABI function from GCC, since GCC will
	     never pass NULL.  */

	  /* Check arguments for function pointers.  */
	  for (tree p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	    {
	      tree arg_type = TREE_VALUE (p);
	      if (chkp_type_has_function_pointer (arg_type))
		error_at (gimple_location (wi->stmt), "function pointers "
			  "not supported with %<-mabi=ti%> option");
	    }
	  break;
	}
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case POINTER_TYPE:
	{
	  if (chkp_type_has_function_pointer (type))
	    {
	      error_at (gimple_location (wi->stmt),
			"function pointers not supported with "
			"%<-mabi=ti%> option");
	      *walk_subtrees = false;
	    }
	  break;
	}
    default:
	  break;
    }
  return NULL;
}

/* Pass implementation.  */
unsigned
pass_pru_tiabi_check::execute (function *fun)
{
  struct walk_stmt_info wi;
  const_tree fntype = TREE_TYPE (fun->decl);

  gimple_seq body = gimple_body (current_function_decl);

  memset (&wi, 0, sizeof (wi));
  wi.info = NULL;
  wi.want_locations = true;

  /* Check the function body.  */
  walk_gimple_seq (body, NULL, check_op_callback, &wi);

  /* Check the function declaration.  */
  chk_function_decl (fntype, fun->function_start_locus);

  return 0;
}

} // anon namespace

gimple_opt_pass *
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
