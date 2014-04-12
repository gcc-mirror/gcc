/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains the CilkPlus Intrinsics
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
   Contributed by Balaji V. Iyer <balaji.v.iyer@intel.com>,
   Intel Corporation

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "stringpool.h"
#include "calls.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "c-family/c-common.h"
#include "toplev.h" 
#include "cgraph.h"
#include "diagnostic.h"
#include "cilk.h"

enum add_variable_type {
    /* Reference to previously-defined variable.  */
    ADD_READ,
    /* Definition of a new variable in inner-scope.  */
    ADD_BIND,
    /* Write to possibly previously-defined variable.  */
    ADD_WRITE
};

enum cilk_block_type {
    /* Indicates a _Cilk_spawn block.  30 was an arbitary number picked for 
       ease of debugging.  */
    CILK_BLOCK_SPAWN = 30,
    /* Indicates _Cilk_for statement block.  */
    CILK_BLOCK_FOR
};

struct wrapper_data
{
  /* Kind of function to be created.  */
  enum cilk_block_type type;
  /* Signature of helper function.  */
  tree fntype;
  /* Containing function.  */
  tree context;
  /* Disposition of all variables in the inner statement.  */
  struct pointer_map_t *decl_map;
  /* True if this function needs a static chain.  */
  bool nested;
  /* Arguments to be passed to wrapper function, currently a list.  */
  tree arglist;
  /* Argument types, a list.  */
  tree argtypes;
  /* Incoming parameters.  */
  tree parms;
  /* Outer BLOCK object.  */
  tree block;
};

static void extract_free_variables (tree, struct wrapper_data *,
				    enum add_variable_type);
static HOST_WIDE_INT cilk_wrapper_count;

/* Marks the CALL_EXPR or FUNCTION_DECL, FCALL, as a spawned function call
   and the current function as a spawner.  Emit error if the function call
   is outside a function or if a non function-call is spawned.  */

inline bool
cilk_set_spawn_marker (location_t loc, tree fcall)
{
  if (!current_function_decl)
    {
      error_at (loc, "%<_Cilk_spawn%> may only be used inside a function");
      return false;
    }
  else if (fcall == error_mark_node)
    /* Error reporting here is not necessary here since if FCALL is an
       error_mark_node, the function marking it as error would have reported
       it.  */
    return false; 
  else if (TREE_CODE (fcall) != CALL_EXPR
	   /* In C++, TARGET_EXPR is generated when we have an overloaded
	      '=' operator.  */
	   && TREE_CODE (fcall) != TARGET_EXPR)
    { 
      error_at (loc, "only function calls can be spawned");
      return false;
    }
  else
    {
      cfun->calls_cilk_spawn = true;
      return true;
    }
}

/* This function will output the exit conditions for a spawn call.  */

tree
create_cilk_function_exit (tree frame, bool detaches, bool needs_sync)
{
  tree epi = alloc_stmt_list ();

  if (needs_sync) 
    append_to_statement_list (build_cilk_sync (), &epi);
  tree func_ptr = build1 (ADDR_EXPR, cilk_frame_ptr_type_decl, frame);
  tree pop_frame = build_call_expr (cilk_pop_fndecl, 1, func_ptr);
  tree worker = cilk_dot (frame, CILK_TI_FRAME_WORKER, 0);
  tree current = cilk_arrow (worker, CILK_TI_WORKER_CUR, 0);
  tree parent = cilk_dot (frame, CILK_TI_FRAME_PARENT, 0);
  tree set_current = build2 (MODIFY_EXPR, void_type_node, current, parent);
  append_to_statement_list (set_current, &epi);
  append_to_statement_list (pop_frame, &epi);
  tree call = build_call_expr (cilk_leave_fndecl, 1, func_ptr);
  if (!detaches)
    {
      tree flags = cilk_dot (frame, CILK_TI_FRAME_FLAGS, false);
      tree flags_cmp_expr = fold_build2 (NE_EXPR, TREE_TYPE (flags), flags, 
					 build_int_cst (TREE_TYPE (flags), 
							CILK_FRAME_VERSION));
      call = fold_build3 (COND_EXPR, void_type_node, flags_cmp_expr,
			  call, build_empty_stmt (EXPR_LOCATION (flags)));
    }
  append_to_statement_list (call, &epi);  
  return epi;
}

/* Trying to get the correct cfun for the FUNCTION_DECL indicated by OUTER.  */

static void
pop_cfun_to (tree outer)
{
  pop_cfun ();
  current_function_decl = outer;
  gcc_assert (cfun == DECL_STRUCT_FUNCTION (current_function_decl));
  gcc_assert (cfun->decl == current_function_decl);
}

/* This function does whatever is necessary to make the compiler emit a newly 
   generated function, FNDECL.  */

static void
call_graph_add_fn (tree fndecl)
{
  const tree outer = current_function_decl;
  struct function *f = DECL_STRUCT_FUNCTION (fndecl);
  gcc_assert (TREE_CODE (fndecl) == FUNCTION_DECL);

  f->is_cilk_function = 1;
  f->curr_properties = cfun->curr_properties;
  gcc_assert (cfun == DECL_STRUCT_FUNCTION (outer)); 
  gcc_assert (cfun->decl == outer);

  push_cfun (f);
  cgraph_create_node (fndecl);
  pop_cfun_to (outer);
}

/* Return true if this is a tree which is allowed to contain a spawn as 
   operand 0.
   A spawn call may be wrapped in a series of unary operations such
   as conversions.  These conversions need not be "useless"
   to be disregarded because they are retained in the spawned
   statement.  They are bypassed only to look for a spawn
   within.
   A comparison to constant is simple enough to allow, and
   is used to convert to bool.  */

static bool
cilk_ignorable_spawn_rhs_op (tree exp)
{
  enum tree_code code = TREE_CODE (exp);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_expression:
      return code == ADDR_EXPR;
    case tcc_comparison:
      /* We need the spawn as operand 0 for now.   That's where it
	 appears in the only case we really care about, conversion
	 to bool.  */
      return (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST);
    case tcc_unary:
    case tcc_reference:
      return true;
    default:
      return false;
    }
}

/* Helper function for walk_tree.  If *TP is a CILK_SPAWN_STMT, then unwrap
   this "wrapper."  The function returns NULL_TREE regardless.  */

static tree
unwrap_cilk_spawn_stmt (tree *tp, int *walk_subtrees, void *)
{
  if (TREE_CODE (*tp) == CILK_SPAWN_STMT)
    {
      *tp = CILK_SPAWN_FN (*tp);
      *walk_subtrees = 0;
    }
  return NULL_TREE;
}

/* Returns true when EXP is a CALL_EXPR with _Cilk_spawn in front.  Unwraps
   CILK_SPAWN_STMT wrapper from the CALL_EXPR in *EXP0 statement.  */

static bool
recognize_spawn (tree exp, tree *exp0)
{
  bool spawn_found = false;
  if (TREE_CODE (exp) == CILK_SPAWN_STMT)
    {
      /* Remove the CALL_EXPR from CILK_SPAWN_STMT wrapper.  */
      exp = CILK_SPAWN_FN (exp);
      walk_tree (exp0, unwrap_cilk_spawn_stmt, NULL, NULL);
      spawn_found = true;
    }
  /* _Cilk_spawn can't be wrapped in expression such as PLUS_EXPR.  */
  else if (contains_cilk_spawn_stmt (exp))
    error ("invalid use of %<_Cilk_spawn%>");
  return spawn_found;
}

/* Returns true if *EXP0 is a recognized form of spawn.  Recognized forms are,
   after conversion to void, a call expression at outer level or an assignment
   at outer level with the right hand side being a spawned call.
   In addition to this, it also unwraps the CILK_SPAWN_STMT cover from the
   CALL_EXPR that is being spawned.
   Note that `=' in C++ may turn into a CALL_EXPR rather than a MODIFY_EXPR.  */

bool
cilk_detect_spawn_and_unwrap (tree *exp0)
{
  tree exp = *exp0;

  if (!TREE_SIDE_EFFECTS (exp))
    return false;

  /* Strip off any conversion to void.  It does not affect whether spawn 
     is supported here.  */
  if (TREE_CODE (exp) == CONVERT_EXPR && VOID_TYPE_P (TREE_TYPE (exp)))
    exp = TREE_OPERAND (exp, 0);

  if (TREE_CODE (exp) == MODIFY_EXPR || TREE_CODE (exp) == INIT_EXPR)
    exp = TREE_OPERAND (exp, 1);

  while (cilk_ignorable_spawn_rhs_op (exp))
    exp = TREE_OPERAND (exp, 0);

  if (TREE_CODE (exp) == TARGET_EXPR)
    if (TARGET_EXPR_INITIAL (exp)
	&& TREE_CODE (TARGET_EXPR_INITIAL (exp)) != AGGR_INIT_EXPR)
      exp = TARGET_EXPR_INITIAL (exp);

  /* Happens with C++ TARGET_EXPR.  */
  if (exp == NULL_TREE)
    return false;

  while (TREE_CODE (exp) == CLEANUP_POINT_EXPR || TREE_CODE (exp) == EXPR_STMT)
    exp = TREE_OPERAND (exp, 0);
  
  /* Now we should have a CALL_EXPR with a CILK_SPAWN_STMT wrapper around 
     it, or return false.  */
  if (recognize_spawn (exp, exp0))
    return true;
  return false;
}

/* This function will build and return a FUNCTION_DECL using information 
   from *WD.  */

static tree
create_cilk_helper_decl (struct wrapper_data *wd)
{
  char name[20];
  if (wd->type == CILK_BLOCK_FOR)
    sprintf (name, "_cilk_for_" HOST_WIDE_INT_PRINT_DEC, cilk_wrapper_count++);
  else if (wd->type == CILK_BLOCK_SPAWN)
    sprintf (name, "_cilk_spn_" HOST_WIDE_INT_PRINT_DEC, cilk_wrapper_count++);
  else
    gcc_unreachable (); 
  
  clean_symbol_name (name);
  tree fndecl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, 
			    get_identifier (name), wd->fntype);

  TREE_PUBLIC (fndecl) = 0;
  TREE_STATIC (fndecl) = 1;
  TREE_USED (fndecl) = 1;
  DECL_ARTIFICIAL (fndecl) = 0;
  DECL_IGNORED_P (fndecl) = 0;
  DECL_EXTERNAL (fndecl) = 0;

  DECL_CONTEXT (fndecl) = wd->context; 
  tree block = make_node (BLOCK);
  DECL_INITIAL (fndecl) = block;
  TREE_USED (block) = 1;
  gcc_assert (!DECL_SAVED_TREE (fndecl));

  /* Inlining would defeat the purpose of this wrapper.
     Either it secretly switches stack frames or it allocates
     a stable stack frame to hold function arguments even if
     the parent stack frame is stolen.  */
  DECL_UNINLINABLE (fndecl) = 1;

  tree result_decl = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, 
				 void_type_node);
  DECL_ARTIFICIAL (result_decl) = 0;
  DECL_IGNORED_P (result_decl) = 1;
  DECL_CONTEXT (result_decl) = fndecl;
  DECL_RESULT (fndecl) = result_decl;
  
  return fndecl;
}

/* A function used by walk tree to find wrapper parms.  */

static bool
wrapper_parm_cb (const void *key0, void **val0, void *data)
{
  struct wrapper_data *wd = (struct wrapper_data *) data;
  tree arg = * (tree *)&key0;
  tree val = (tree)*val0;
  tree parm;

  if (val == error_mark_node || val == arg)
    return true;

  if (TREE_CODE (val) == PAREN_EXPR)
    {
      /* We should not reach here with a register receiver.
	 We may see a register variable modified in the
	 argument list.  Because register variables are
	 worker-local we don't need to work hard to support
	 them in code that spawns.  */
      if ((TREE_CODE (arg) == VAR_DECL) && DECL_HARD_REGISTER (arg))
	{
	  error_at (EXPR_LOCATION (arg),
		    "explicit register variable %qD may not be modified in "
		    "spawn", arg);
	  arg = null_pointer_node;
	}
      else
	arg = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (arg)), arg);
	
      val = TREE_OPERAND (val, 0);
      *val0 = val;
      gcc_assert (TREE_CODE (val) == INDIRECT_REF);
      parm = TREE_OPERAND (val, 0);
      STRIP_NOPS (parm);
    }
  else
    parm = val;
  TREE_CHAIN (parm) = wd->parms;
  wd->parms = parm;
  wd->argtypes = tree_cons (NULL_TREE, TREE_TYPE (parm), wd->argtypes); 
  wd->arglist = tree_cons (NULL_TREE, arg, wd->arglist); 
  return true;
}

/* This function is used to build a wrapper of a certain type.  */

static void
build_wrapper_type (struct wrapper_data *wd)
{
  wd->arglist = NULL_TREE;
  wd->parms = NULL_TREE;
  wd->argtypes = void_list_node;

  pointer_map_traverse (wd->decl_map, wrapper_parm_cb, wd);
  gcc_assert (wd->type != CILK_BLOCK_FOR);

  /* Now build a function.
     Its return type is void (all side effects are via explicit parameters).
     Its parameters are WRAPPER_PARMS with type WRAPPER_TYPES.
     Actual arguments in the caller are WRAPPER_ARGS.  */
  wd->fntype = build_function_type (void_type_node, wd->argtypes);
}

/* This function checks all the CALL_EXPRs in *TP found by cilk_outline.  */

static tree
check_outlined_calls (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED, 
		      void *data)
{
  bool *throws = (bool *) data;
  tree t = *tp;
  int flags;

  if (TREE_CODE (t) != CALL_EXPR)
    return 0;
  flags = call_expr_flags (t);

  if (!(flags & ECF_NOTHROW) && flag_exceptions)
    *throws = true;
  if (flags & ECF_RETURNS_TWICE)
    error_at (EXPR_LOCATION (t), 
	      "cannot spawn call to function that returns twice");
  return 0;
}

/* Each DECL in the source code (spawned statement) is passed to this function
   once.  Each instance of the DECL is replaced with the result of this 
   function.

   The parameters of the wrapper should have been entered into the map already.
   This function only deals with variables with scope limited to the 
   spawned expression.  */

static tree
copy_decl_for_cilk (tree decl, copy_body_data *id)
{
  switch (TREE_CODE (decl))
    {
    case VAR_DECL:
      return copy_decl_no_change (decl, id);

    case LABEL_DECL:
      error_at (EXPR_LOCATION (decl), "invalid use of label %q+D in "
		"%<_Cilk_spawn%>", 
		decl);
      return error_mark_node;

    case RESULT_DECL:
    case PARM_DECL:
      /* RESULT_DECL and PARM_DECL has already been entered into the map.  */
    default:
      gcc_unreachable ();
      return error_mark_node;
    }
}

/* Copy all local variables.  */

static bool
for_local_cb (const void *k_v, void **vp, void *p)
{
  tree k = *(tree *) &k_v;
  tree v = (tree) *vp;

  if (v == error_mark_node)
    *vp = copy_decl_no_change (k, (copy_body_data *) p);
  return true;
}

/* Copy all local declarations from a _Cilk_spawned function's body.  */

static bool
wrapper_local_cb (const void *k_v, void **vp, void *data)
{
  copy_body_data *id = (copy_body_data *) data;
  tree key = *(tree *) &k_v;
  tree val = (tree) *vp;

  if (val == error_mark_node)
    *vp = copy_decl_for_cilk (key, id);

  return true;
}

/* Alter a tree STMT from OUTER_FN to form the body of INNER_FN.  */

void
cilk_outline (tree inner_fn, tree *stmt_p, void *w)
{
  struct wrapper_data *wd = (struct wrapper_data *) w;
  const tree outer_fn = wd->context;	      
  const bool nested = (wd->type == CILK_BLOCK_FOR);
  copy_body_data id;
  bool throws;

  DECL_STATIC_CHAIN (outer_fn) = 1;

  memset (&id, 0, sizeof (id));
  /* Copy from the function containing the spawn...  */
  id.src_fn = outer_fn;

  /* ...to the wrapper.  */
  id.dst_fn = inner_fn; 
  id.src_cfun = DECL_STRUCT_FUNCTION (outer_fn);

  /* There shall be no RETURN in spawn helper.  */
  id.retvar = 0; 
  id.decl_map = wd->decl_map;
  id.copy_decl = nested ? copy_decl_no_change : copy_decl_for_cilk;
  id.block = DECL_INITIAL (inner_fn);
  id.transform_lang_insert_block = NULL;

  id.transform_new_cfg = true;
  id.transform_call_graph_edges = CB_CGE_MOVE;
  id.remap_var_for_cilk = true;
  id.regimplify = true; /* unused? */

  insert_decl_map (&id, wd->block, DECL_INITIAL (inner_fn));

  /* We don't want the private variables any more.  */
  pointer_map_traverse (wd->decl_map, nested ? for_local_cb : wrapper_local_cb,
			&id);
  walk_tree (stmt_p, copy_tree_body_r, (void *) &id, NULL);

  /* See if this function can throw or calls something that should
     not be spawned.  The exception part is only necessary if
     flag_exceptions && !flag_non_call_exceptions.  */
  throws = false ;
  (void) walk_tree_without_duplicates (stmt_p, check_outlined_calls, &throws);
}

/* Generate the body of a wrapper function that assigns the
   result of the expression RHS into RECEIVER.  RECEIVER must
   be NULL if this is not a spawn -- the wrapper will return
   a value.  If this is a spawn, the wrapper will return void.  */

static tree
create_cilk_wrapper_body (tree stmt, struct wrapper_data *wd)
{
  const tree outer = current_function_decl;
  tree fndecl;
  tree p;

   /* Build the type of the wrapper and its argument list from the
     variables that it requires.  */
  build_wrapper_type (wd);

  /* Emit a function that takes WRAPPER_PARMS incoming and applies ARGS 
     (modified) to the wrapped function.  Return the wrapper and modified ARGS 
     to the caller to generate a function call.  */
  fndecl = create_cilk_helper_decl (wd);
  push_struct_function (fndecl);
  if (wd->nested && (wd->type == CILK_BLOCK_FOR))
    {
      gcc_assert (TREE_VALUE (wd->arglist) == NULL_TREE);
      TREE_VALUE (wd->arglist) = build2 (FDESC_EXPR, ptr_type_node,
					 fndecl, integer_one_node);
    }
  DECL_ARGUMENTS (fndecl) = wd->parms;

  for (p = wd->parms; p; p = TREE_CHAIN (p))
    DECL_CONTEXT (p) = fndecl;

  gcc_assert (!DECL_SAVED_TREE (fndecl));
  cilk_install_body_with_frame_cleanup (fndecl, stmt, (void *) wd);
  gcc_assert (DECL_SAVED_TREE (fndecl));

  pop_cfun_to (outer);

  /* Recognize the new function.  */
  call_graph_add_fn (fndecl);
  return fndecl;
}

/* Initializes the wrapper data structure.  */

static void
init_wd (struct wrapper_data *wd, enum cilk_block_type type)
{
  wd->type = type;
  wd->fntype = NULL_TREE;
  wd->context = current_function_decl;
  wd->decl_map = pointer_map_create ();
  /* _Cilk_for bodies are always nested.  Others start off as 
     normal functions.  */
  wd->nested = (type == CILK_BLOCK_FOR);
  wd->arglist = NULL_TREE;
  wd->argtypes = NULL_TREE;
  wd->block = NULL_TREE;
}

/* Clears the wrapper data structure.  */

static void
free_wd (struct wrapper_data *wd)
{
  pointer_map_destroy (wd->decl_map);
  wd->nested = false;
  wd->arglist = NULL_TREE;
  wd->argtypes = NULL_TREE;
  wd->parms = NULL_TREE;
}


 /* Given a variable in an expression to be extracted into
   a helper function, declare the helper function parameter
   to receive it.

   On entry the value of the (key, value) pair may be

   (*, error_mark_node) -- Variable is private to helper function,
   do nothing.

   (var, var) -- Reference to outer scope (function or global scope).

   (var, integer 0) -- Capture by value, save newly-declared PARM_DECL
   for value in value slot.

   (var, integer 1) -- Capture by reference, declare pointer to type
   as new PARM_DECL and store (spawn_stmt (indirect_ref (parm)).
   
   (var, ???) -- Pure output argument, handled similarly to above.
*/

static bool
declare_one_free_variable (const void *var0, void **map0,
			   void *data ATTRIBUTE_UNUSED)
{
  const_tree var = (const_tree) var0;
  tree map = (tree)*map0;
  tree var_type = TREE_TYPE (var), arg_type;
  bool by_reference;
  tree parm;

  gcc_assert (DECL_P (var));

  /* Ignore truly local variables.  */
  if (map == error_mark_node)
    return true;
  /* Ignore references to the parent function.  */
  if (map == var)
    return true;

  gcc_assert (TREE_CODE (map) == INTEGER_CST);

  /* A value is passed by reference if:

     1. It is addressable, so that a copy may not be made.
     2. It is modified in the spawned statement.
     In the future this function may want to arrange
     a warning if the spawned statement is a loop body
     because an output argument would indicate a race.
     Note: Earlier passes must have marked the variable addressable.
     3. It is expensive to copy.  */
  by_reference =
    (TREE_ADDRESSABLE (var_type)
     /* Arrays must be passed by reference.  This is required for C
	semantics -- arrays are not first class objects.  Other
	aggregate types can and should be passed by reference if
	they are not passed to the spawned function.  We aren't yet
	distinguishing safe uses in argument calculation from unsafe
	uses as outgoing function arguments, so we make a copy to
	stabilize the value.  */
     || TREE_CODE (var_type) == ARRAY_TYPE
     || (tree) map == integer_one_node);

  if (by_reference)
    var_type = build_qualified_type (build_pointer_type (var_type),
				     TYPE_QUAL_RESTRICT);
  gcc_assert (!TREE_ADDRESSABLE (var_type));

  /* Maybe promote to int.  */
  if (INTEGRAL_TYPE_P (var_type) && COMPLETE_TYPE_P (var_type)
      && INT_CST_LT_UNSIGNED (TYPE_SIZE (var_type),
			      TYPE_SIZE (integer_type_node)))
    arg_type = integer_type_node;
  else
    arg_type = var_type;

  parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE, var_type);
  DECL_ARG_TYPE (parm) = arg_type;
  DECL_ARTIFICIAL (parm) = 0;
  TREE_READONLY (parm) = 1;
  
  if (by_reference)
    {
      parm = build1 (INDIRECT_REF, TREE_TYPE (var_type), parm);
      parm = build1 (PAREN_EXPR, void_type_node, parm);
    }
  *map0 = parm;
  return true;
}
 
/* Returns a wrapper function for a _Cilk_spawn.  */

static tree
create_cilk_wrapper (tree exp, tree *args_out)
{
  struct wrapper_data wd;
  tree fndecl;

  init_wd (&wd, CILK_BLOCK_SPAWN);

  if (TREE_CODE (exp) == CONVERT_EXPR)
    exp = TREE_OPERAND (exp, 0);
  
  /* Special handling for top level INIT_EXPR.  Usually INIT_EXPR means the 
     variable is defined in the spawned expression and can be private to the 
     spawn helper.  A top level INIT_EXPR defines a variable to be initialized 
     by spawn and the variable must remain in the outer function.  */
  if (TREE_CODE (exp) == INIT_EXPR)
    {
      extract_free_variables (TREE_OPERAND (exp, 0), &wd, ADD_WRITE);
      extract_free_variables (TREE_OPERAND (exp, 1), &wd, ADD_READ);
      /* TREE_TYPE should be void.  Be defensive.  */
      if (TREE_TYPE (exp) != void_type_node)
	extract_free_variables (TREE_TYPE (exp), &wd, ADD_READ);
    }
  else
    extract_free_variables (exp, &wd, ADD_READ);
  pointer_map_traverse (wd.decl_map, declare_one_free_variable, &wd);
  wd.block = TREE_BLOCK (exp);
  if (!wd.block)
    wd.block = DECL_INITIAL (current_function_decl);

  /* Now fvars maps the old variable to incoming variable.  Update
     the expression and arguments to refer to the new names.  */
  fndecl = create_cilk_wrapper_body (exp, &wd);
  *args_out = wd.arglist;
  
  free_wd (&wd);

  return fndecl;
}

/* Transform *SPAWN_P, a spawned CALL_EXPR, to gimple.  *SPAWN_P can be a
   CALL_EXPR, INIT_EXPR or MODIFY_EXPR.  Returns GS_OK if everything is fine,
   and GS_UNHANDLED, otherwise.  */

int
gimplify_cilk_spawn (tree *spawn_p)
{
  tree expr = *spawn_p;
  tree function, call1, call2, new_args;
  tree ii_args = NULL_TREE;
  int total_args = 0, ii = 0;
  tree *arg_array;
  tree setjmp_cond_expr = NULL_TREE;
  tree setjmp_expr, spawn_expr, setjmp_value = NULL_TREE;

  cfun->calls_cilk_spawn = 1;
  cfun->is_cilk_function = 1;

  /* Remove CLEANUP_POINT_EXPR and EXPR_STMT from *spawn_p.  */
  while (TREE_CODE (expr) == CLEANUP_POINT_EXPR
	 || TREE_CODE (expr) == EXPR_STMT)
    expr = TREE_OPERAND (expr, 0);
  
  new_args = NULL;
  function = create_cilk_wrapper (expr, &new_args);

  /* This should give the number of parameters.  */
  total_args = list_length (new_args);
  if (total_args)
    arg_array = XNEWVEC (tree, total_args);
  else
    arg_array = NULL;

  ii_args = new_args;
  for (ii = 0; ii < total_args; ii++)
    {
      arg_array[ii] = TREE_VALUE (ii_args);
      ii_args = TREE_CHAIN (ii_args);
    }
  
  TREE_USED (function) = 1;
  rest_of_decl_compilation (function, 0, 0);

  call1 = cilk_call_setjmp (cfun->cilk_frame_decl);

  if (arg_array == NULL || *arg_array == NULL_TREE)
    call2 = build_call_expr (function, 0);
  else 
    call2 = build_call_expr_loc_array (EXPR_LOCATION (*spawn_p), function, 
					 total_args, arg_array);
  *spawn_p = alloc_stmt_list ();
  tree f_ptr_type = build_pointer_type (TREE_TYPE (cfun->cilk_frame_decl));
  tree frame_ptr = build1 (ADDR_EXPR, f_ptr_type, cfun->cilk_frame_decl);
  tree save_fp = build_call_expr (cilk_save_fp_fndecl, 1, frame_ptr);
  append_to_statement_list (save_fp, spawn_p);		  
  setjmp_value = create_tmp_var (TREE_TYPE (call1), NULL);
  setjmp_expr = fold_build2 (MODIFY_EXPR, void_type_node, setjmp_value, call1);

  append_to_statement_list_force (setjmp_expr, spawn_p);
  
  setjmp_cond_expr = fold_build2 (EQ_EXPR, TREE_TYPE (call1), setjmp_value,
				  build_int_cst (TREE_TYPE (call1), 0));
  spawn_expr = fold_build3 (COND_EXPR, void_type_node, setjmp_cond_expr,
			    call2, build_empty_stmt (EXPR_LOCATION (call1)));
  append_to_statement_list (spawn_expr, spawn_p);

  return GS_OK;
}

/* Make the frames necessary for a spawn call.  */

tree
make_cilk_frame (tree fn)
{
  struct function *f = DECL_STRUCT_FUNCTION (fn);
  tree decl;

  if (f->cilk_frame_decl)
    return f->cilk_frame_decl;

  decl = build_decl (EXPR_LOCATION (fn), VAR_DECL, NULL_TREE, 
		     cilk_frame_type_decl);
  DECL_CONTEXT (decl) = fn;
  DECL_SEEN_IN_BIND_EXPR_P (decl) = 1;
  f->cilk_frame_decl = decl;
  return decl;
}

/* Returns a STATEMENT_LIST with all the pedigree operations required for
   install body with frame cleanup functions.  FRAME_PTR is the pointer to
   __cilkrts_stack_frame created by make_cilk_frame.  */

tree
cilk_install_body_pedigree_operations (tree frame_ptr)
{
  tree body_list = alloc_stmt_list ();
  tree enter_frame = build_call_expr (cilk_enter_fast_fndecl, 1, frame_ptr); 
  append_to_statement_list (enter_frame, &body_list);
  
  tree parent = cilk_arrow (frame_ptr, CILK_TI_FRAME_PARENT, 0);
  tree worker = cilk_arrow (frame_ptr, CILK_TI_FRAME_WORKER, 0);

  tree pedigree = cilk_arrow (frame_ptr, CILK_TI_FRAME_PEDIGREE, 0);
  tree pedigree_rank = cilk_dot (pedigree, CILK_TI_PEDIGREE_RANK, 0);
  tree parent_pedigree = cilk_dot (pedigree, CILK_TI_PEDIGREE_PARENT, 0);
  tree pedigree_parent = cilk_arrow (parent, CILK_TI_FRAME_PEDIGREE, 0);
  tree pedigree_parent_rank = cilk_dot (pedigree_parent, 
					CILK_TI_PEDIGREE_RANK, 0);
  tree pedigree_parent_parent = cilk_dot (pedigree_parent, 
				     CILK_TI_PEDIGREE_PARENT, 0);
  tree worker_pedigree = cilk_arrow (worker, CILK_TI_WORKER_PEDIGREE, 1);
  tree w_pedigree_rank = cilk_dot (worker_pedigree, CILK_TI_PEDIGREE_RANK, 0);
  tree w_pedigree_parent = cilk_dot (worker_pedigree, 
				     CILK_TI_PEDIGREE_PARENT, 0);

  /* sf.pedigree.rank = worker->pedigree.rank.  */
  tree exp1 = build2 (MODIFY_EXPR, void_type_node, pedigree_rank,
		     w_pedigree_rank);
  append_to_statement_list (exp1, &body_list);

  /* sf.pedigree.parent = worker->pedigree.parent.  */
  exp1 = build2 (MODIFY_EXPR, void_type_node, parent_pedigree,
		 w_pedigree_parent);
  append_to_statement_list (exp1, &body_list);

  /* sf.call_parent->pedigree.rank = worker->pedigree.rank.  */
  exp1 = build2 (MODIFY_EXPR, void_type_node, pedigree_parent_rank,
		 w_pedigree_rank);
  append_to_statement_list (exp1, &body_list);

  /* sf.call_parent->pedigree.parent = worker->pedigree.parent.  */
  exp1 = build2 (MODIFY_EXPR, void_type_node, pedigree_parent_parent,
		 w_pedigree_parent);
  append_to_statement_list (exp1, &body_list);

  /* sf->worker.pedigree.rank = 0.  */
  exp1 = build2 (MODIFY_EXPR, void_type_node, w_pedigree_rank, 
		 build_zero_cst (uint64_type_node));
  append_to_statement_list (exp1, &body_list);

  /* sf->pedigree.parent = &sf->pedigree.  */
  exp1 = build2 (MODIFY_EXPR, void_type_node, w_pedigree_parent,
		 build1 (ADDR_EXPR,
			 build_pointer_type (cilk_pedigree_type_decl),
			 pedigree));
  append_to_statement_list (exp1, &body_list);
  return body_list;
}

/* Add a new variable, VAR to a variable list in WD->DECL_MAP.  HOW indicates
   whether the variable is previously defined, currently defined, or a variable 
   that is being written to.  */

static void
add_variable (struct wrapper_data *wd, tree var, enum add_variable_type how)
{
  void **valp;
  
  valp = pointer_map_contains (wd->decl_map, (void *) var);
  if (valp)
    {
      tree val = (tree) *valp;
      /* If the variable is local, do nothing.  */
      if (val == error_mark_node)
	return;
      /* If the variable was entered with itself as value,
	 meaning it belongs to an outer scope, do not alter
	 the value.  */
      if (val == var) 
	return;
      /* A statement expression may cause a variable to be
	 bound twice, once in BIND_EXPR and again in a
	 DECL_EXPR.  That case caused a return in the 
	 test above.  Any other duplicate definition is
	 an error.  */
      gcc_assert (how != ADD_BIND);
      if (how != ADD_WRITE)
	return;
      /* This variable might have been entered as read but is now written.  */
      *valp = (void *) var;
      wd->nested = true;
      return;
    }
  else
    {
      tree val = NULL_TREE;

      /* Nested function rewriting silently discards hard register
	 assignments for function scope variables, and they wouldn't
	 work anyway.  Warn here.  This misses one case: if the
	 register variable is used as the loop bound or increment it
	 has already been added to the map.  */
      if ((how != ADD_BIND) && (TREE_CODE (var) == VAR_DECL)
	  && !DECL_EXTERNAL (var) && DECL_HARD_REGISTER (var))
	warning (0, "register assignment ignored for %qD used in Cilk block",
		 var);

      switch (how)
	{
	  /* ADD_BIND means always make a fresh new variable.  */
	case ADD_BIND:
	  val = error_mark_node;
	  break;
	  /* ADD_READ means
	     1. For cilk_for, refer to the outer scope definition as-is
	     2. For a spawned block, take a scalar in an rgument
	     and otherwise refer to the outer scope definition as-is.
	     3. For a spawned call, take a scalar in an argument.  */
	case ADD_READ:
	  switch (wd->type)
	    {
	    case CILK_BLOCK_FOR:
	      val = var;
	      break;
	    case CILK_BLOCK_SPAWN:
	      if (TREE_ADDRESSABLE (var))
		{
		  val = var;
		  wd->nested = true;
		  break;
		}
	      val = integer_zero_node;
	      break;
	    }
	  break;
	case ADD_WRITE:
	  switch (wd->type)
	    {
	    case CILK_BLOCK_FOR:
	      val = var;
	      wd->nested = true;
	      break;
	    case CILK_BLOCK_SPAWN:
	      if (TREE_ADDRESSABLE (var))
		val = integer_one_node;
	      else
		{
		  val = var;
		  wd->nested = true;
		}
	      break;
	    }
	}
      *pointer_map_insert (wd->decl_map, (void *) var) = val;
    }
}

/* Find the variables referenced in an expression T.  This does not avoid 
   duplicates because a variable may be read in one context and written in 
   another.  HOW describes the context in which the reference is seen.  If 
   NESTED is true a nested function is being generated and variables in the 
   original context should not be remapped.  */

static void
extract_free_variables (tree t, struct wrapper_data *wd,
			enum add_variable_type how)
{  
  if (t == NULL_TREE)
    return;

  enum tree_code code = TREE_CODE (t);
  bool is_expr = IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code));

  if (is_expr)
    extract_free_variables (TREE_TYPE (t), wd, ADD_READ);

  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case BLOCK:
    case PLACEHOLDER_EXPR:
    case FIELD_DECL:
    case VOID_TYPE:
    case REAL_TYPE:
      /* These do not contain variable references.  */
      return;

    case SSA_NAME:
      /* Currently we don't see SSA_NAME.  */
      extract_free_variables (SSA_NAME_VAR (t), wd, how);
      return;

    case LABEL_DECL:
      /* This might be a reference to a label outside the Cilk block,
	 which is an error, or a reference to a label in the Cilk block
	 that we haven't seen yet.  We can't tell.  Ignore it.  An
	 invalid use will cause an error later in copy_decl_for_cilk.  */
      return;

    case RESULT_DECL:
      if (wd->type != CILK_BLOCK_SPAWN)
	TREE_ADDRESSABLE (t) = 1;
    case VAR_DECL:
    case PARM_DECL:
      if (!TREE_STATIC (t) && !DECL_EXTERNAL (t))
	add_variable (wd, t, how);
      return;

    case NON_LVALUE_EXPR:
    case CONVERT_EXPR:
    case NOP_EXPR:
      extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_READ);
      return;

    case VEC_INIT_EXPR:
    case INIT_EXPR:
      extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_BIND);
      extract_free_variables (TREE_OPERAND (t, 1), wd, ADD_READ);
      return;

    case MODIFY_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      /* These write their result.  */
      extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_WRITE);
      extract_free_variables (TREE_OPERAND (t, 1), wd, ADD_READ);
      return;

    case ADDR_EXPR:
      /* This might modify its argument, and the value needs to be
	 passed by reference in any case to preserve identity and
	 type if is a promoting type.  In the case of a nested loop
	 just notice that we touch the variable.  It will already
	 be addressable, and marking it modified will cause a spurious
	 warning about writing the control variable.  */
      if (wd->type != CILK_BLOCK_SPAWN)
	extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_READ);
      else 
	extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_WRITE);
      return;

    case ARRAY_REF:
      /* Treating ARRAY_REF and BIT_FIELD_REF identically may
	 mark the array as written but the end result is correct
	 because the array is passed by pointer anyway.  */
    case BIT_FIELD_REF:
      /* Propagate the access type to the object part of which
	 is being accessed here.  As for ADDR_EXPR, don't do this
	 in a nested loop, unless the access is to a fixed index.  */
      if (wd->type != CILK_BLOCK_FOR || TREE_CONSTANT (TREE_OPERAND (t, 1)))
	extract_free_variables (TREE_OPERAND (t, 0), wd, how);
      else
	extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_READ);
      extract_free_variables (TREE_OPERAND (t, 1), wd, ADD_READ);
      extract_free_variables (TREE_OPERAND (t, 2), wd, ADD_READ);
      return;

    case TREE_LIST:
      extract_free_variables (TREE_PURPOSE (t), wd, ADD_READ);
      extract_free_variables (TREE_VALUE (t), wd, ADD_READ);
      extract_free_variables (TREE_CHAIN (t), wd, ADD_READ);
      return;

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (t);
	int i;
	for (i = 0; i < len; i++)
	  extract_free_variables (TREE_VEC_ELT (t, i), wd, ADD_READ);
	return;
      }

    case VECTOR_CST:
      {
	unsigned ii = 0;
	for (ii = 0; ii < VECTOR_CST_NELTS (t); ii++)
	  extract_free_variables (VECTOR_CST_ELT (t, ii), wd, ADD_READ); 
	break;
      }

    case COMPLEX_CST:
      extract_free_variables (TREE_REALPART (t), wd, ADD_READ);
      extract_free_variables (TREE_IMAGPART (t), wd, ADD_READ);
      return;

    case BIND_EXPR:
      {
	tree decl;
	for (decl = BIND_EXPR_VARS (t); decl; decl = TREE_CHAIN (decl))
	  {
	    add_variable (wd, decl, ADD_BIND);
	    /* A self-referential initialization is no problem because
	       we already entered the variable into the map as local.  */
	    extract_free_variables (DECL_INITIAL (decl), wd, ADD_READ);
	    extract_free_variables (DECL_SIZE (decl), wd, ADD_READ);
	    extract_free_variables (DECL_SIZE_UNIT (decl), wd, ADD_READ);
	  }
	extract_free_variables (BIND_EXPR_BODY (t), wd, ADD_READ);
	return;
      }

    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	  extract_free_variables (*tsi_stmt_ptr (i), wd, ADD_READ);
	return;
      }

    case TARGET_EXPR:
      {
	extract_free_variables (TREE_OPERAND (t, 0), wd, ADD_BIND);
	extract_free_variables (TREE_OPERAND (t, 1), wd, ADD_READ);
	extract_free_variables (TREE_OPERAND (t, 2), wd, ADD_READ);
	if (TREE_OPERAND (t, 3) != TREE_OPERAND (t, 1))
	  extract_free_variables (TREE_OPERAND (t, 3), wd, ADD_READ);
	return;
      }

    case RETURN_EXPR:
      if (TREE_NO_WARNING (t))
	{
	  gcc_assert (errorcount);
	  return;
	}
      return;

    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (t)) != TYPE_DECL)
	extract_free_variables (DECL_EXPR_DECL (t), wd, ADD_BIND);
      return;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      extract_free_variables (TYPE_MIN_VALUE (t), wd, ADD_READ);
      extract_free_variables (TYPE_MAX_VALUE (t), wd, ADD_READ);
      return;

    case POINTER_TYPE:
      extract_free_variables (TREE_TYPE (t), wd, ADD_READ);
      break;

    case ARRAY_TYPE:
      extract_free_variables (TREE_TYPE (t), wd, ADD_READ);
      extract_free_variables (TYPE_DOMAIN (t), wd, ADD_READ);
      return;

    case RECORD_TYPE:
      extract_free_variables (TYPE_FIELDS (t), wd, ADD_READ);
      return;
    
    case METHOD_TYPE:
      extract_free_variables (TYPE_ARG_TYPES (t), wd, ADD_READ);
      extract_free_variables (TYPE_METHOD_BASETYPE (t), wd, ADD_READ);
      return;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      {
	int len = 0;
	int ii = 0;
	if (TREE_CODE (TREE_OPERAND (t, 0)) == INTEGER_CST)
	  {
	    len = TREE_INT_CST_LOW (TREE_OPERAND (t, 0));

	    for (ii = 0; ii < len; ii++)
	      extract_free_variables (TREE_OPERAND (t, ii), wd, ADD_READ);
	    extract_free_variables (TREE_TYPE (t), wd, ADD_READ);
	  }
	break;
      }

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx = 0;
	constructor_elt *ce;
	for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (t), idx, &ce); idx++)
	  extract_free_variables (ce->value, wd, ADD_READ);
	break;
      }

    default:
      if (is_expr)
	{
	  int i, len;

	  /* Walk over all the sub-trees of this operand.  */
	  len = TREE_CODE_LENGTH (code);

	  /* Go through the subtrees.  We need to do this in forward order so
	     that the scope of a FOR_EXPR is handled properly.  */
	  for (i = 0; i < len; ++i)
	    extract_free_variables (TREE_OPERAND (t, i), wd, ADD_READ);
	}
    }
}

/* Add appropriate frames needed for a Cilk spawned function call, FNDECL. 
   Returns the __cilkrts_stack_frame * variable.  */

tree
insert_cilk_frame (tree fndecl)
{
  tree addr, body, enter, out, orig_body;
  location_t loc = EXPR_LOCATION (fndecl);

  if (!cfun || cfun->decl != fndecl)
    push_cfun (DECL_STRUCT_FUNCTION (fndecl)); 

  tree decl = cfun->cilk_frame_decl;
  if (!decl)
    {
      tree *saved_tree = &DECL_SAVED_TREE (fndecl);
      decl = make_cilk_frame (fndecl);
      add_local_decl (cfun, decl);

      addr = build1 (ADDR_EXPR, cilk_frame_ptr_type_decl, decl);
      enter = build_call_expr (cilk_enter_fndecl, 1, addr);
      out = create_cilk_function_exit (cfun->cilk_frame_decl, false, true);

      /* The new body will be:
	 __cilkrts_enter_frame_1 (&sf);
	 try {
	    orig_body;
	 } 
	 finally {
	     __cilkrts_pop_frame (&sf);
	     __cilkrts_leave_frame (&sf);
         }  */

      body = alloc_stmt_list ();
      orig_body = *saved_tree;

      if (TREE_CODE (orig_body) == BIND_EXPR)
	orig_body = BIND_EXPR_BODY (orig_body);
 
      append_to_statement_list (enter, &body);
      append_to_statement_list (build_stmt (loc, TRY_FINALLY_EXPR, orig_body, 
					    out), &body);
      if (TREE_CODE (*saved_tree) == BIND_EXPR)
	BIND_EXPR_BODY (*saved_tree) = body;
      else
	*saved_tree = body;
    }
  return decl;
}

/* Wraps CALL, a CALL_EXPR, into a CILK_SPAWN_STMT tree and returns it.  */

tree
build_cilk_spawn (location_t loc, tree call)
{
  if (!cilk_set_spawn_marker (loc, call))
    return error_mark_node;
  tree spawn_stmt = build1 (CILK_SPAWN_STMT, TREE_TYPE (call), call);
  TREE_SIDE_EFFECTS (spawn_stmt) = 1;
  return spawn_stmt;
}

/* Returns a tree of type CILK_SYNC_STMT.  */

tree
build_cilk_sync (void)
{
  tree sync = build0 (CILK_SYNC_STMT, void_type_node);
  TREE_SIDE_EFFECTS (sync) = 1;
  return sync;
}

/* Helper for contains_cilk_spawn_stmt, callback for walk_tree.  Return
   non-null tree if TP contains CILK_SPAWN_STMT.  */

static tree
contains_cilk_spawn_stmt_walker (tree *tp, int *, void *)
{
  if (TREE_CODE (*tp) == CILK_SPAWN_STMT)
    return *tp;
  else
    return NULL_TREE;
}

/* Returns true if EXPR or any of its subtrees contain CILK_SPAWN_STMT
   node.  */

bool
contains_cilk_spawn_stmt (tree expr)
{
  return walk_tree (&expr, contains_cilk_spawn_stmt_walker, NULL, NULL)
	 != NULL_TREE;
}
