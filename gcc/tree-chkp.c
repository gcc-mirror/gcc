/* Pointer Bounds Checker insrumentation pass.
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
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "varasm.h"
#include "tree-iterator.h"
#include "tree-cfg.h"
#include "langhooks.h"
#include "tree-ssa-address.h"
#include "tree-ssa-loop-niter.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "print-tree.h"
#include "calls.h"
#include "expr.h"
#include "tree-ssa-propagate.h"
#include "tree-chkp.h"
#include "gimple-walk.h"
#include "tree-dfa.h"
#include "ipa-chkp.h"
#include "params.h"

/*  Pointer Bounds Checker instruments code with memory checks to find
    out-of-bounds memory accesses.  Checks are performed by computing
    bounds for each pointer and then comparing address of accessed
    memory before pointer dereferencing.

    1. Function clones.

    See ipa-chkp.c.

    2. Instrumentation.

    There are few things to instrument:

    a) Memory accesses - add checker calls to check address of accessed memory
    against bounds of dereferenced pointer.  Obviously safe memory
    accesses like static variable access does not have to be instrumented
    with checks.

    Example:

      val_2 = *p_1;

      with 4 bytes access is transformed into:

      __builtin___chkp_bndcl (__bound_tmp.1_3, p_1);
      D.1_4 = p_1 + 3;
      __builtin___chkp_bndcu (__bound_tmp.1_3, D.1_4);
      val_2 = *p_1;

      where __bound_tmp.1_3 are bounds computed for pointer p_1,
      __builtin___chkp_bndcl is a lower bound check and
      __builtin___chkp_bndcu is an upper bound check.

    b) Pointer stores.

    When pointer is stored in memory we need to store its bounds.  To
    achieve compatibility of instrumented code with regular codes
    we have to keep data layout and store bounds in special bound tables
    via special checker call.  Implementation of bounds table may vary for
    different platforms.  It has to associate pointer value and its
    location (it is required because we may have two equal pointers
    with different bounds stored in different places) with bounds.
    Another checker builtin allows to get bounds for specified pointer
    loaded from specified location.

    Example:

      buf1[i_1] = &buf2;

      is transformed into:

      buf1[i_1] = &buf2;
      D.1_2 = &buf1[i_1];
      __builtin___chkp_bndstx (D.1_2, &buf2, __bound_tmp.1_2);

      where __bound_tmp.1_2 are bounds of &buf2.

    c) Static initialization.

    The special case of pointer store is static pointer initialization.
    Bounds initialization is performed in a few steps:
      - register all static initializations in front-end using
      chkp_register_var_initializer
      - when file compilation finishes we create functions with special
      attribute 'chkp ctor' and put explicit initialization code
      (assignments) for all statically initialized pointers.
      - when checker constructor is compiled checker pass adds required
      bounds initialization for all statically initialized pointers
      - since we do not actually need excess pointers initialization
      in checker constructor we remove such assignments from them

    d) Calls.

    For each call in the code we add additional arguments to pass
    bounds for pointer arguments.  We determine type of call arguments
    using arguments list from function declaration; if function
    declaration is not available we use function type; otherwise
    (e.g. for unnamed arguments) we use type of passed value. Function
    declaration/type is replaced with the instrumented one.

    Example:

      val_1 = foo (&buf1, &buf2, &buf1, 0);

      is translated into:

      val_1 = foo.chkp (&buf1, __bound_tmp.1_2, &buf2, __bound_tmp.1_3,
                        &buf1, __bound_tmp.1_2, 0);

    e) Returns.

    If function returns a pointer value we have to return bounds also.
    A new operand was added for return statement to hold returned bounds.

    Example:

      return &_buf1;

      is transformed into

      return &_buf1, __bound_tmp.1_1;

    3. Bounds computation.

    Compiler is fully responsible for computing bounds to be used for each
    memory access.  The first step for bounds computation is to find the
    origin of pointer dereferenced for memory access.  Basing on pointer
    origin we define a way to compute its bounds.  There are just few
    possible cases:

    a) Pointer is returned by call.

    In this case we use corresponding checker builtin method to obtain returned
    bounds.

    Example:

      buf_1 = malloc (size_2);
      foo (buf_1);

      is translated into:

      buf_1 = malloc (size_2);
      __bound_tmp.1_3 = __builtin___chkp_bndret (buf_1);
      foo (buf_1, __bound_tmp.1_3);

    b) Pointer is an address of an object.

    In this case compiler tries to compute objects size and create corresponding
    bounds.  If object has incomplete type then special checker builtin is used to
    obtain its size at runtime.

    Example:

      foo ()
      {
        <unnamed type> __bound_tmp.3;
	static int buf[100];

	<bb 3>:
	__bound_tmp.3_2 = __builtin___chkp_bndmk (&buf, 400);

	<bb 2>:
	return &buf, __bound_tmp.3_2;
      }

    Example:

      Address of an object 'extern int buf[]' with incomplete type is
      returned.

      foo ()
      {
        <unnamed type> __bound_tmp.4;
	long unsigned int __size_tmp.3;

	<bb 3>:
	__size_tmp.3_4 = __builtin_ia32_sizeof (buf);
	__bound_tmp.4_3 = __builtin_ia32_bndmk (&buf, __size_tmp.3_4);

	<bb 2>:
	return &buf, __bound_tmp.4_3;
      }

    c) Pointer is the result of object narrowing.

    It happens when we use pointer to an object to compute pointer to a part
    of an object.  E.g. we take pointer to a field of a structure. In this
    case we perform bounds intersection using bounds of original object and
    bounds of object's part (which are computed basing on its type).

    There may be some debatable questions about when narrowing should occur
    and when it should not.  To avoid false bound violations in correct
    programs we do not perform narrowing when address of an array element is
    obtained (it has address of the whole array) and when address of the first
    structure field is obtained (because it is guaranteed to be equal to
    address of the whole structure and it is legal to cast it back to structure).

    Default narrowing behavior may be changed using compiler flags.

    Example:

      In this example address of the second structure field is returned.

      foo (struct A * p, __bounds_type __bounds_of_p)
      {
        <unnamed type> __bound_tmp.3;
	int * _2;
	int * _5;

	<bb 2>:
	_5 = &p_1(D)->second_field;
	__bound_tmp.3_6 = __builtin___chkp_bndmk (_5, 4);
	__bound_tmp.3_8 = __builtin___chkp_intersect (__bound_tmp.3_6,
	                                              __bounds_of_p_3(D));
	_2 = &p_1(D)->second_field;
	return _2, __bound_tmp.3_8;
      }

    Example:

      In this example address of the first field of array element is returned.

      foo (struct A * p, __bounds_type __bounds_of_p, int i)
      {
	long unsigned int _3;
	long unsigned int _4;
	struct A * _6;
	int * _7;

	<bb 2>:
	_3 = (long unsigned int) i_1(D);
	_4 = _3 * 8;
	_6 = p_5(D) + _4;
	_7 = &_6->first_field;
	return _7, __bounds_of_p_2(D);
      }


    d) Pointer is the result of pointer arithmetic or type cast.

    In this case bounds of the base pointer are used.  In case of binary
    operation producing a pointer we are analyzing data flow further
    looking for operand's bounds.  One operand is considered as a base
    if it has some valid bounds.  If we fall into a case when none of
    operands (or both of them) has valid bounds, a default bounds value
    is used.

    Trying to find out bounds for binary operations we may fall into
    cyclic dependencies for pointers.  To avoid infinite recursion all
    walked phi nodes instantly obtain corresponding bounds but created
    bounds are marked as incomplete.  It helps us to stop DF walk during
    bounds search.

    When we reach pointer source, some args of incomplete bounds phi obtain
    valid bounds and those values are propagated further through phi nodes.
    If no valid bounds were found for phi node then we mark its result as
    invalid bounds.  Process stops when all incomplete bounds become either
    valid or invalid and we are able to choose a pointer base.

    e) Pointer is loaded from the memory.

    In this case we just need to load bounds from the bounds table.

    Example:

      foo ()
      {
        <unnamed type> __bound_tmp.3;
	static int * buf;
	int * _2;

	<bb 2>:
	_2 = buf;
	__bound_tmp.3_4 = __builtin___chkp_bndldx (&buf, _2);
	return _2, __bound_tmp.3_4;
      }

*/

typedef void (*assign_handler)(tree, tree, void *);

static tree chkp_get_zero_bounds ();
static tree chkp_find_bounds (tree ptr, gimple_stmt_iterator *iter);
static tree chkp_find_bounds_loaded (tree ptr, tree ptr_src,
				     gimple_stmt_iterator *iter);
static void chkp_parse_array_and_component_ref (tree node, tree *ptr,
						tree *elt, bool *safe,
						bool *bitfield,
						tree *bounds,
						gimple_stmt_iterator *iter,
						bool innermost_bounds);

#define chkp_bndldx_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDLDX))
#define chkp_bndstx_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDSTX))
#define chkp_checkl_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDCL))
#define chkp_checku_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDCU))
#define chkp_bndmk_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDMK))
#define chkp_ret_bnd_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDRET))
#define chkp_intersect_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_INTERSECT))
#define chkp_narrow_bounds_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_NARROW))
#define chkp_sizeof_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_SIZEOF))
#define chkp_extract_lower_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_EXTRACT_LOWER))
#define chkp_extract_upper_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_EXTRACT_UPPER))

static GTY (()) tree chkp_uintptr_type;

static GTY (()) tree chkp_zero_bounds_var;
static GTY (()) tree chkp_none_bounds_var;

static GTY (()) basic_block entry_block;
static GTY (()) tree zero_bounds;
static GTY (()) tree none_bounds;
static GTY (()) tree incomplete_bounds;
static GTY (()) tree tmp_var;
static GTY (()) tree size_tmp_var;
static GTY (()) bitmap chkp_abnormal_copies;

struct hash_set<tree> *chkp_invalid_bounds;
struct hash_set<tree> *chkp_completed_bounds_set;
struct hash_map<tree, tree> *chkp_reg_bounds;
struct hash_map<tree, tree> *chkp_bound_vars;
struct hash_map<tree, tree> *chkp_reg_addr_bounds;
struct hash_map<tree, tree> *chkp_incomplete_bounds_map;
struct hash_map<tree, tree> *chkp_bounds_map;
struct hash_map<tree, tree> *chkp_static_var_bounds;

static bool in_chkp_pass;

#define CHKP_BOUND_TMP_NAME "__bound_tmp"
#define CHKP_SIZE_TMP_NAME "__size_tmp"
#define CHKP_BOUNDS_OF_SYMBOL_PREFIX "__chkp_bounds_of_"
#define CHKP_STRING_BOUNDS_PREFIX "__chkp_string_bounds_"
#define CHKP_VAR_BOUNDS_PREFIX "__chkp_var_bounds_"
#define CHKP_ZERO_BOUNDS_VAR_NAME "__chkp_zero_bounds"
#define CHKP_NONE_BOUNDS_VAR_NAME "__chkp_none_bounds"

/* Static checker constructors may become very large and their
   compilation with optimization may take too much time.
   Therefore we put a limit to number of statements in one
   constructor.  Tests with 100 000 statically initialized
   pointers showed following compilation times on Sandy Bridge
   server (used -O2):
   limit    100 => ~18 sec.
   limit    300 => ~22 sec.
   limit   1000 => ~30 sec.
   limit   3000 => ~49 sec.
   limit   5000 => ~55 sec.
   limit  10000 => ~76 sec.
   limit 100000 => ~532 sec.  */
#define MAX_STMTS_IN_STATIC_CHKP_CTOR (PARAM_VALUE (PARAM_CHKP_MAX_CTOR_SIZE))

struct chkp_ctor_stmt_list
{
  tree stmts;
  int avail;
};

/* Return 1 if function FNDECL is instrumented by Pointer
   Bounds Checker.  */
bool
chkp_function_instrumented_p (tree fndecl)
{
  return fndecl
    && lookup_attribute ("chkp instrumented", DECL_ATTRIBUTES (fndecl));
}

/* Mark function FNDECL as instrumented.  */
void
chkp_function_mark_instrumented (tree fndecl)
{
  if (chkp_function_instrumented_p (fndecl))
    return;

  DECL_ATTRIBUTES (fndecl)
    = tree_cons (get_identifier ("chkp instrumented"), NULL,
		 DECL_ATTRIBUTES (fndecl));
}

/* Return true when STMT is builtin call to instrumentation function
   corresponding to CODE.  */

bool
chkp_gimple_call_builtin_p (gimple *call,
			    enum built_in_function code)
{
  tree fndecl;
  if (is_gimple_call (call)
      && (fndecl = targetm.builtin_chkp_function (code))
      && gimple_call_fndecl (call) == fndecl)
    return true;
  return false;
}

/* Emit code to build zero bounds and return RTL holding
   the result.  */
rtx
chkp_expand_zero_bounds ()
{
  tree zero_bnd;

  if (flag_chkp_use_static_const_bounds)
    zero_bnd = chkp_get_zero_bounds_var ();
  else
    zero_bnd = chkp_build_make_bounds_call (integer_zero_node,
					    integer_zero_node);
  return expand_normal (zero_bnd);
}

/* Emit code to store zero bounds for PTR located at MEM.  */
void
chkp_expand_bounds_reset_for_mem (tree mem, tree ptr)
{
  tree zero_bnd, bnd, addr, bndstx;

  if (flag_chkp_use_static_const_bounds)
    zero_bnd = chkp_get_zero_bounds_var ();
  else
    zero_bnd = chkp_build_make_bounds_call (integer_zero_node,
					    integer_zero_node);
  bnd = make_tree (pointer_bounds_type_node,
		   assign_temp (pointer_bounds_type_node, 0, 1));
  addr = build1 (ADDR_EXPR,
		 build_pointer_type (TREE_TYPE (mem)), mem);
  bndstx = chkp_build_bndstx_call (addr, ptr, bnd);

  expand_assignment (bnd, zero_bnd, false);
  expand_normal (bndstx);
}

/* Build retbnd call for returned value RETVAL.

   If BNDVAL is not NULL then result is stored
   in it.  Otherwise a temporary is created to
   hold returned value.

   GSI points to a position for a retbnd call
   and is set to created stmt.

   Cgraph edge is created for a new call if
   UPDATE_EDGE is 1.

   Obtained bounds are returned.  */
tree
chkp_insert_retbnd_call (tree bndval, tree retval,
			 gimple_stmt_iterator *gsi)
{
  gimple *call;

  if (!bndval)
    bndval = create_tmp_reg (pointer_bounds_type_node, "retbnd");

  call = gimple_build_call (chkp_ret_bnd_fndecl, 1, retval);
  gimple_call_set_lhs (call, bndval);
  gsi_insert_after (gsi, call, GSI_CONTINUE_LINKING);

  return bndval;
}

/* Build a GIMPLE_CALL identical to CALL but skipping bounds
   arguments.  */

gcall *
chkp_copy_call_skip_bounds (gcall *call)
{
  bitmap bounds;
  unsigned i;

  bitmap_obstack_initialize (NULL);
  bounds = BITMAP_ALLOC (NULL);

  for (i = 0; i < gimple_call_num_args (call); i++)
    if (POINTER_BOUNDS_P (gimple_call_arg (call, i)))
      bitmap_set_bit (bounds, i);

  if (!bitmap_empty_p (bounds))
    call = gimple_call_copy_skip_args (call, bounds);
  gimple_call_set_with_bounds (call, false);

  BITMAP_FREE (bounds);
  bitmap_obstack_release (NULL);

  return call;
}

/* Redirect edge E to the correct node according to call_stmt.
   Return 1 if bounds removal from call_stmt should be done
   instead of redirection.  */

bool
chkp_redirect_edge (cgraph_edge *e)
{
  bool instrumented = false;
  tree decl = e->callee->decl;

  if (e->callee->instrumentation_clone
      || chkp_function_instrumented_p (decl))
    instrumented = true;

  if (instrumented
      && !gimple_call_with_bounds_p (e->call_stmt))
    e->redirect_callee (cgraph_node::get_create (e->callee->orig_decl));
  else if (!instrumented
	   && gimple_call_with_bounds_p (e->call_stmt)
	   && !chkp_gimple_call_builtin_p (e->call_stmt, BUILT_IN_CHKP_BNDCL)
	   && !chkp_gimple_call_builtin_p (e->call_stmt, BUILT_IN_CHKP_BNDCU)
	   && !chkp_gimple_call_builtin_p (e->call_stmt, BUILT_IN_CHKP_BNDSTX))
    {
      if (e->callee->instrumented_version)
	e->redirect_callee (e->callee->instrumented_version);
      else
	{
	  tree args = TYPE_ARG_TYPES (TREE_TYPE (decl));
	  /* Avoid bounds removal if all args will be removed.  */
	  if (!args || TREE_VALUE (args) != void_type_node)
	    return true;
	  else
	    gimple_call_set_with_bounds (e->call_stmt, false);
	}
    }

  return false;
}

/* Mark statement S to not be instrumented.  */
static void
chkp_mark_stmt (gimple *s)
{
  gimple_set_plf (s, GF_PLF_1, true);
}

/* Mark statement S to be instrumented.  */
static void
chkp_unmark_stmt (gimple *s)
{
  gimple_set_plf (s, GF_PLF_1, false);
}

/* Return 1 if statement S should not be instrumented.  */
static bool
chkp_marked_stmt_p (gimple *s)
{
  return gimple_plf (s, GF_PLF_1);
}

/* Get var to be used for bound temps.  */
static tree
chkp_get_tmp_var (void)
{
  if (!tmp_var)
    tmp_var = create_tmp_reg (pointer_bounds_type_node, CHKP_BOUND_TMP_NAME);

  return tmp_var;
}

/* Get SSA_NAME to be used as temp.  */
static tree
chkp_get_tmp_reg (gimple *stmt)
{
  if (in_chkp_pass)
    return make_ssa_name (chkp_get_tmp_var (), stmt);

  return make_temp_ssa_name (pointer_bounds_type_node, stmt,
			     CHKP_BOUND_TMP_NAME);
}

/* Get var to be used for size temps.  */
static tree
chkp_get_size_tmp_var (void)
{
  if (!size_tmp_var)
    size_tmp_var = create_tmp_reg (chkp_uintptr_type, CHKP_SIZE_TMP_NAME);

  return size_tmp_var;
}

/* Register bounds BND for address of OBJ.  */
static void
chkp_register_addr_bounds (tree obj, tree bnd)
{
  if (bnd == incomplete_bounds)
    return;

  chkp_reg_addr_bounds->put (obj, bnd);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Regsitered bound ");
      print_generic_expr (dump_file, bnd, 0);
      fprintf (dump_file, " for address of ");
      print_generic_expr (dump_file, obj, 0);
      fprintf (dump_file, "\n");
    }
}

/* Return bounds registered for address of OBJ.  */
static tree
chkp_get_registered_addr_bounds (tree obj)
{
  tree *slot = chkp_reg_addr_bounds->get (obj);
  return slot ? *slot : NULL_TREE;
}

/* Mark BOUNDS as completed.  */
static void
chkp_mark_completed_bounds (tree bounds)
{
  chkp_completed_bounds_set->add (bounds);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Marked bounds ");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, " as completed\n");
    }
}

/* Return 1 if BOUNDS were marked as completed and 0 otherwise.  */
static bool
chkp_completed_bounds (tree bounds)
{
  return chkp_completed_bounds_set->contains (bounds);
}

/* Clear comleted bound marks.  */
static void
chkp_erase_completed_bounds (void)
{
  delete chkp_completed_bounds_set;
  chkp_completed_bounds_set = new hash_set<tree>;
}

/* Mark BOUNDS associated with PTR as incomplete.  */
static void
chkp_register_incomplete_bounds (tree bounds, tree ptr)
{
  chkp_incomplete_bounds_map->put (bounds, ptr);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Regsitered incomplete bounds ");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, " for ");
      print_generic_expr (dump_file, ptr, 0);
      fprintf (dump_file, "\n");
    }
}

/* Return 1 if BOUNDS are incomplete and 0 otherwise.  */
static bool
chkp_incomplete_bounds (tree bounds)
{
  if (bounds == incomplete_bounds)
    return true;

  if (chkp_completed_bounds (bounds))
    return false;

  return chkp_incomplete_bounds_map->get (bounds) != NULL;
}

/* Clear incomleted bound marks.  */
static void
chkp_erase_incomplete_bounds (void)
{
  delete chkp_incomplete_bounds_map;
  chkp_incomplete_bounds_map = new hash_map<tree, tree>;
}

/* Build and return bndmk call which creates bounds for structure
   pointed by PTR.  Structure should have complete type.  */
tree
chkp_make_bounds_for_struct_addr (tree ptr)
{
  tree type = TREE_TYPE (ptr);
  tree size;

  gcc_assert (POINTER_TYPE_P (type));

  size = TYPE_SIZE (TREE_TYPE (type));

  gcc_assert (size);

  return build_call_nary (pointer_bounds_type_node,
			  build_fold_addr_expr (chkp_bndmk_fndecl),
			  2, ptr, size);
}

/* Traversal function for chkp_may_finish_incomplete_bounds.
   Set RES to 0 if at least one argument of phi statement
   defining bounds (passed in KEY arg) is unknown.
   Traversal stops when first unknown phi argument is found.  */
bool
chkp_may_complete_phi_bounds (tree const &bounds, tree *slot ATTRIBUTE_UNUSED,
			      bool *res)
{
  gimple *phi;
  unsigned i;

  gcc_assert (TREE_CODE (bounds) == SSA_NAME);

  phi = SSA_NAME_DEF_STMT (bounds);

  gcc_assert (phi && gimple_code (phi) == GIMPLE_PHI);

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree phi_arg = gimple_phi_arg_def (phi, i);
      if (!phi_arg)
	{
	  *res = false;
	  /* Do not need to traverse further.  */
	  return false;
	}
    }

  return true;
}

/* Return 1 if all phi nodes created for bounds have their
   arguments computed.  */
static bool
chkp_may_finish_incomplete_bounds (void)
{
  bool res = true;

  chkp_incomplete_bounds_map
    ->traverse<bool *, chkp_may_complete_phi_bounds> (&res);

  return res;
}

/* Helper function for chkp_finish_incomplete_bounds.
   Recompute args for bounds phi node.  */
bool
chkp_recompute_phi_bounds (tree const &bounds, tree *slot,
			   void *res ATTRIBUTE_UNUSED)
{
  tree ptr = *slot;
  gphi *bounds_phi;
  gphi *ptr_phi;
  unsigned i;

  gcc_assert (TREE_CODE (bounds) == SSA_NAME);
  gcc_assert (TREE_CODE (ptr) == SSA_NAME);

  bounds_phi = as_a <gphi *> (SSA_NAME_DEF_STMT (bounds));
  ptr_phi = as_a <gphi *> (SSA_NAME_DEF_STMT (ptr));

  for (i = 0; i < gimple_phi_num_args (bounds_phi); i++)
    {
      tree ptr_arg = gimple_phi_arg_def (ptr_phi, i);
      tree bound_arg = chkp_find_bounds (ptr_arg, NULL);

      add_phi_arg (bounds_phi, bound_arg,
		   gimple_phi_arg_edge (ptr_phi, i),
		   UNKNOWN_LOCATION);
    }

  return true;
}

/* Mark BOUNDS as invalid.  */
static void
chkp_mark_invalid_bounds (tree bounds)
{
  chkp_invalid_bounds->add (bounds);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Marked bounds ");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, " as invalid\n");
    }
}

/* Return 1 if BOUNDS were marked as invalid and 0 otherwise.  */
static bool
chkp_valid_bounds (tree bounds)
{
  if (bounds == zero_bounds || bounds == none_bounds)
    return false;

  return !chkp_invalid_bounds->contains (bounds);
}

/* Helper function for chkp_finish_incomplete_bounds.
   Check all arguments of phi nodes trying to find
   valid completed bounds.  If there is at least one
   such arg then bounds produced by phi node are marked
   as valid completed bounds and all phi args are
   recomputed.  */
bool
chkp_find_valid_phi_bounds (tree const &bounds, tree *slot, bool *res)
{
  gimple *phi;
  unsigned i;

  gcc_assert (TREE_CODE (bounds) == SSA_NAME);

  if (chkp_completed_bounds (bounds))
    return true;

  phi = SSA_NAME_DEF_STMT (bounds);

  gcc_assert (phi && gimple_code (phi) == GIMPLE_PHI);

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree phi_arg = gimple_phi_arg_def (phi, i);

      gcc_assert (phi_arg);

      if (chkp_valid_bounds (phi_arg) && !chkp_incomplete_bounds (phi_arg))
	{
	  *res = true;
	  chkp_mark_completed_bounds (bounds);
	  chkp_recompute_phi_bounds (bounds, slot, NULL);
	  return true;
	}
    }

  return true;
}

/* Helper function for chkp_finish_incomplete_bounds.
   Marks all incompleted bounds as invalid.  */
bool
chkp_mark_invalid_bounds_walker (tree const &bounds,
				 tree *slot ATTRIBUTE_UNUSED,
				 void *res ATTRIBUTE_UNUSED)
{
  if (!chkp_completed_bounds (bounds))
    {
      chkp_mark_invalid_bounds (bounds);
      chkp_mark_completed_bounds (bounds);
    }
  return true;
}

/* When all bound phi nodes have all their args computed
   we have enough info to find valid bounds.  We iterate
   through all incompleted bounds searching for valid
   bounds.  Found valid bounds are marked as completed
   and all remaining incompleted bounds are recomputed.
   Process continues until no new valid bounds may be
   found.  All remained incompleted bounds are marked as
   invalid (i.e. have no valid source of bounds).  */
static void
chkp_finish_incomplete_bounds (void)
{
  bool found_valid = true;

  while (found_valid)
    {
      found_valid = false;

      chkp_incomplete_bounds_map->
	traverse<bool *, chkp_find_valid_phi_bounds> (&found_valid);

      if (found_valid)
	chkp_incomplete_bounds_map->
	  traverse<void *, chkp_recompute_phi_bounds> (NULL);
    }

  chkp_incomplete_bounds_map->
    traverse<void *, chkp_mark_invalid_bounds_walker> (NULL);
  chkp_incomplete_bounds_map->
    traverse<void *, chkp_recompute_phi_bounds> (NULL);

  chkp_erase_completed_bounds ();
  chkp_erase_incomplete_bounds ();
}

/* Return 1 if type TYPE is a pointer type or a
   structure having a pointer type as one of its fields.
   Otherwise return 0.  */
bool
chkp_type_has_pointer (const_tree type)
{
  bool res = false;

  if (BOUNDED_TYPE_P (type))
    res = true;
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  res = res || chkp_type_has_pointer (TREE_TYPE (field));
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    res = chkp_type_has_pointer (TREE_TYPE (type));

  return res;
}

unsigned
chkp_type_bounds_count (const_tree type)
{
  unsigned res = 0;

  if (!type)
    res = 0;
  else if (BOUNDED_TYPE_P (type))
    res = 1;
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      bitmap have_bound;

      bitmap_obstack_initialize (NULL);
      have_bound = BITMAP_ALLOC (NULL);
      chkp_find_bound_slots (type, have_bound);
      res = bitmap_count_bits (have_bound);
      BITMAP_FREE (have_bound);
      bitmap_obstack_release (NULL);
    }

  return res;
}

/* Get bounds associated with NODE via
   chkp_set_bounds call.  */
tree
chkp_get_bounds (tree node)
{
  tree *slot;

  if (!chkp_bounds_map)
    return NULL_TREE;

  slot = chkp_bounds_map->get (node);
  return slot ? *slot : NULL_TREE;
}

/* Associate bounds VAL with NODE.  */
void
chkp_set_bounds (tree node, tree val)
{
  if (!chkp_bounds_map)
    chkp_bounds_map = new hash_map<tree, tree>;

  chkp_bounds_map->put (node, val);
}

/* Check if statically initialized variable VAR require
   static bounds initialization.  If VAR is added into
   bounds initlization list then 1 is returned. Otherwise
   return 0.  */
extern bool
chkp_register_var_initializer (tree var)
{
  if (!flag_check_pointer_bounds
      || DECL_INITIAL (var) == error_mark_node)
    return false;

  gcc_assert (TREE_CODE (var) == VAR_DECL);
  gcc_assert (DECL_INITIAL (var));

  if (TREE_STATIC (var)
      && chkp_type_has_pointer (TREE_TYPE (var)))
    {
      varpool_node::get_create (var)->need_bounds_init = 1;
      return true;
    }

  return false;
}

/* Helper function for chkp_finish_file.

   Add new modification statement (RHS is assigned to LHS)
   into list of static initializer statementes (passed in ARG).
   If statements list becomes too big, emit checker constructor
   and start the new one.  */
static void
chkp_add_modification_to_stmt_list (tree lhs,
				    tree rhs,
				    void *arg)
{
  struct chkp_ctor_stmt_list *stmts = (struct chkp_ctor_stmt_list *)arg;
  tree modify;

  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
    rhs = build1 (CONVERT_EXPR, TREE_TYPE (lhs), rhs);

  modify = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, rhs);
  append_to_statement_list (modify, &stmts->stmts);

  stmts->avail--;
}

/* Build and return ADDR_EXPR for specified object OBJ.  */
static tree
chkp_build_addr_expr (tree obj)
{
  return TREE_CODE (obj) == TARGET_MEM_REF
    ? tree_mem_ref_addr (ptr_type_node, obj)
    : build_fold_addr_expr (obj);
}

/* Helper function for chkp_finish_file.
   Initialize bound variable BND_VAR with bounds of variable
   VAR to statements list STMTS.  If statements list becomes
   too big, emit checker constructor and start the new one.  */
static void
chkp_output_static_bounds (tree bnd_var, tree var,
			   struct chkp_ctor_stmt_list *stmts)
{
  tree lb, ub, size;

  if (TREE_CODE (var) == STRING_CST)
    {
      lb = build1 (CONVERT_EXPR, size_type_node, chkp_build_addr_expr (var));
      size = build_int_cst (size_type_node, TREE_STRING_LENGTH (var) - 1);
    }
  else if (DECL_SIZE (var)
	   && !chkp_variable_size_type (TREE_TYPE (var)))
    {
      /* Compute bounds using statically known size.  */
      lb = build1 (CONVERT_EXPR, size_type_node, chkp_build_addr_expr (var));
      size = size_binop (MINUS_EXPR, DECL_SIZE_UNIT (var), size_one_node);
    }
  else
    {
      /* Compute bounds using dynamic size.  */
      tree call;

      lb = build1 (CONVERT_EXPR, size_type_node, chkp_build_addr_expr (var));
      call = build1 (ADDR_EXPR,
		     build_pointer_type (TREE_TYPE (chkp_sizeof_fndecl)),
		     chkp_sizeof_fndecl);
      size = build_call_nary (TREE_TYPE (TREE_TYPE (chkp_sizeof_fndecl)),
			      call, 1, var);

      if (flag_chkp_zero_dynamic_size_as_infinite)
	{
	  tree max_size, cond;

	  max_size = build2 (MINUS_EXPR, size_type_node, size_zero_node, lb);
	  cond = build2 (NE_EXPR, boolean_type_node, size, size_zero_node);
	  size = build3 (COND_EXPR, size_type_node, cond, size, max_size);
	}

      size = size_binop (MINUS_EXPR, size, size_one_node);
    }

  ub = size_binop (PLUS_EXPR, lb, size);
  stmts->avail -= targetm.chkp_initialize_bounds (bnd_var, lb, ub,
						  &stmts->stmts);
  if (stmts->avail <= 0)
    {
      cgraph_build_static_cdtor ('B', stmts->stmts,
				 MAX_RESERVED_INIT_PRIORITY + 2);
      stmts->avail = MAX_STMTS_IN_STATIC_CHKP_CTOR;
      stmts->stmts = NULL;
    }
}

/* Return entry block to be used for checker initilization code.
   Create new block if required.  */
static basic_block
chkp_get_entry_block (void)
{
  if (!entry_block)
    entry_block
      = split_block_after_labels (ENTRY_BLOCK_PTR_FOR_FN (cfun))->dest;

  return entry_block;
}

/* Return a bounds var to be used for pointer var PTR_VAR.  */
static tree
chkp_get_bounds_var (tree ptr_var)
{
  tree bnd_var;
  tree *slot;

  slot = chkp_bound_vars->get (ptr_var);
  if (slot)
    bnd_var = *slot;
  else
    {
      bnd_var = create_tmp_reg (pointer_bounds_type_node,
				CHKP_BOUND_TMP_NAME);
      chkp_bound_vars->put (ptr_var, bnd_var);
    }

  return bnd_var;
}

/* If BND is an abnormal bounds copy, return a copied value.
   Otherwise return BND.  */
static tree
chkp_get_orginal_bounds_for_abnormal_copy (tree bnd)
{
  if (bitmap_bit_p (chkp_abnormal_copies, SSA_NAME_VERSION (bnd)))
    {
      gimple *bnd_def = SSA_NAME_DEF_STMT (bnd);
      gcc_checking_assert (gimple_code (bnd_def) == GIMPLE_ASSIGN);
      bnd = gimple_assign_rhs1 (bnd_def);
    }

  return bnd;
}

/* Register bounds BND for object PTR in global bounds table.
   A copy of bounds may be created for abnormal ssa names.
   Returns bounds to use for PTR.  */
static tree
chkp_maybe_copy_and_register_bounds (tree ptr, tree bnd)
{
  bool abnormal_ptr;

  if (!chkp_reg_bounds)
    return bnd;

  /* Do nothing if bounds are incomplete_bounds
     because it means bounds will be recomputed.  */
  if (bnd == incomplete_bounds)
    return bnd;

  abnormal_ptr = (TREE_CODE (ptr) == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ptr)
		  && gimple_code (SSA_NAME_DEF_STMT (ptr)) != GIMPLE_PHI);

  /* A single bounds value may be reused multiple times for
     different pointer values.  It may cause coalescing issues
     for abnormal SSA names.  To avoid it we create a bounds
     copy in case it is computed for abnormal SSA name.

     We also cannot reuse such created copies for other pointers  */
  if (abnormal_ptr
      || bitmap_bit_p (chkp_abnormal_copies, SSA_NAME_VERSION (bnd)))
    {
      tree bnd_var = NULL_TREE;

      if (abnormal_ptr)
	{
	  if (SSA_NAME_VAR (ptr))
	    bnd_var = chkp_get_bounds_var (SSA_NAME_VAR (ptr));
	}
      else
	bnd_var = chkp_get_tmp_var ();

      /* For abnormal copies we may just find original
	 bounds and use them.  */
      if (!abnormal_ptr && !SSA_NAME_IS_DEFAULT_DEF (bnd))
	bnd = chkp_get_orginal_bounds_for_abnormal_copy (bnd);
      /* For undefined values we usually use none bounds
	 value but in case of abnormal edge it may cause
	 coalescing failures.  Use default definition of
	 bounds variable instead to avoid it.  */
      else if (SSA_NAME_IS_DEFAULT_DEF (ptr)
	       && TREE_CODE (SSA_NAME_VAR (ptr)) != PARM_DECL)
	{
	  bnd = get_or_create_ssa_default_def (cfun, bnd_var);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Using default def bounds ");
	      print_generic_expr (dump_file, bnd, 0);
	      fprintf (dump_file, " for abnormal default def SSA name ");
	      print_generic_expr (dump_file, ptr, 0);
	      fprintf (dump_file, "\n");
	    }
	}
      else
	{
	  tree copy;
	  gimple *def = SSA_NAME_DEF_STMT (ptr);
	  gimple *assign;
	  gimple_stmt_iterator gsi;

	  if (bnd_var)
	    copy = make_ssa_name (bnd_var);
	  else
	    copy = make_temp_ssa_name (pointer_bounds_type_node,
				       NULL,
				       CHKP_BOUND_TMP_NAME);
	  bnd = chkp_get_orginal_bounds_for_abnormal_copy (bnd);
	  assign = gimple_build_assign (copy, bnd);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Creating a copy of bounds ");
	      print_generic_expr (dump_file, bnd, 0);
	      fprintf (dump_file, " for abnormal SSA name ");
	      print_generic_expr (dump_file, ptr, 0);
	      fprintf (dump_file, "\n");
	    }

	  if (gimple_code (def) == GIMPLE_NOP)
	    {
	      gsi = gsi_last_bb (chkp_get_entry_block ());
	      if (!gsi_end_p (gsi) && is_ctrl_stmt (gsi_stmt (gsi)))
		gsi_insert_before (&gsi, assign, GSI_CONTINUE_LINKING);
	      else
		gsi_insert_after (&gsi, assign, GSI_CONTINUE_LINKING);
	    }
	  else
	    {
	      gimple *bnd_def = SSA_NAME_DEF_STMT (bnd);
	      /* Sometimes (e.g. when we load a pointer from a
		 memory) bounds are produced later than a pointer.
		 We need to insert bounds copy appropriately.  */
	      if (gimple_code (bnd_def) != GIMPLE_NOP
		  && stmt_dominates_stmt_p (def, bnd_def))
		gsi = gsi_for_stmt (bnd_def);
	      else
		gsi = gsi_for_stmt (def);
	      gsi_insert_after (&gsi, assign, GSI_CONTINUE_LINKING);
	    }

	  bnd = copy;
	}

      if (abnormal_ptr)
	bitmap_set_bit (chkp_abnormal_copies, SSA_NAME_VERSION (bnd));
    }

  chkp_reg_bounds->put (ptr, bnd);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Regsitered bound ");
      print_generic_expr (dump_file, bnd, 0);
      fprintf (dump_file, " for pointer ");
      print_generic_expr (dump_file, ptr, 0);
      fprintf (dump_file, "\n");
    }

  return bnd;
}

/* Get bounds registered for object PTR in global bounds table.  */
static tree
chkp_get_registered_bounds (tree ptr)
{
  tree *slot;

  if (!chkp_reg_bounds)
    return NULL_TREE;

  slot = chkp_reg_bounds->get (ptr);
  return slot ? *slot : NULL_TREE;
}

/* Add bound retvals to return statement pointed by GSI.  */

static void
chkp_add_bounds_to_ret_stmt (gimple_stmt_iterator *gsi)
{
  greturn *ret = as_a <greturn *> (gsi_stmt (*gsi));
  tree retval = gimple_return_retval (ret);
  tree ret_decl = DECL_RESULT (cfun->decl);
  tree bounds;

  if (!retval)
    return;

  if (BOUNDED_P (ret_decl))
    {
      bounds = chkp_find_bounds (retval, gsi);
      bounds = chkp_maybe_copy_and_register_bounds (ret_decl, bounds);
      gimple_return_set_retbnd (ret, bounds);
    }

  update_stmt (ret);
}

/* Force OP to be suitable for using as an argument for call.
   New statements (if any) go to SEQ.  */
static tree
chkp_force_gimple_call_op (tree op, gimple_seq *seq)
{
  gimple_seq stmts;
  gimple_stmt_iterator si;

  op = force_gimple_operand (unshare_expr (op), &stmts, true, NULL_TREE);

  for (si = gsi_start (stmts); !gsi_end_p (si); gsi_next (&si))
    chkp_mark_stmt (gsi_stmt (si));

  gimple_seq_add_seq (seq, stmts);

  return op;
}

/* Generate lower bound check for memory access by ADDR.
   Check is inserted before the position pointed by ITER.
   DIRFLAG indicates whether memory access is load or store.  */
static void
chkp_check_lower (tree addr, tree bounds,
		  gimple_stmt_iterator iter,
		  location_t location,
		  tree dirflag)
{
  gimple_seq seq;
  gimple *check;
  tree node;

  if (!chkp_function_instrumented_p (current_function_decl)
      && bounds == chkp_get_zero_bounds ())
    return;

  if (dirflag == integer_zero_node
      && !flag_chkp_check_read)
    return;

  if (dirflag == integer_one_node
      && !flag_chkp_check_write)
    return;

  seq = NULL;

  node = chkp_force_gimple_call_op (addr, &seq);

  check = gimple_build_call (chkp_checkl_fndecl, 2, node, bounds);
  chkp_mark_stmt (check);
  gimple_call_set_with_bounds (check, true);
  gimple_set_location (check, location);
  gimple_seq_add_stmt (&seq, check);

  gsi_insert_seq_before (&iter, seq, GSI_SAME_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      gimple *before = gsi_stmt (iter);
      fprintf (dump_file, "Generated lower bound check for statement ");
      print_gimple_stmt (dump_file, before, 0, TDF_VOPS|TDF_MEMSYMS);
      fprintf (dump_file, "  ");
      print_gimple_stmt (dump_file, check, 0, TDF_VOPS|TDF_MEMSYMS);
    }
}

/* Generate upper bound check for memory access by ADDR.
   Check is inserted before the position pointed by ITER.
   DIRFLAG indicates whether memory access is load or store.  */
static void
chkp_check_upper (tree addr, tree bounds,
		  gimple_stmt_iterator iter,
		  location_t location,
		  tree dirflag)
{
  gimple_seq seq;
  gimple *check;
  tree node;

  if (!chkp_function_instrumented_p (current_function_decl)
      && bounds == chkp_get_zero_bounds ())
    return;

  if (dirflag == integer_zero_node
      && !flag_chkp_check_read)
    return;

  if (dirflag == integer_one_node
      && !flag_chkp_check_write)
    return;

  seq = NULL;

  node = chkp_force_gimple_call_op (addr, &seq);

  check = gimple_build_call (chkp_checku_fndecl, 2, node, bounds);
  chkp_mark_stmt (check);
  gimple_call_set_with_bounds (check, true);
  gimple_set_location (check, location);
  gimple_seq_add_stmt (&seq, check);

  gsi_insert_seq_before (&iter, seq, GSI_SAME_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      gimple *before = gsi_stmt (iter);
      fprintf (dump_file, "Generated upper bound check for statement ");
      print_gimple_stmt (dump_file, before, 0, TDF_VOPS|TDF_MEMSYMS);
      fprintf (dump_file, "  ");
      print_gimple_stmt (dump_file, check, 0, TDF_VOPS|TDF_MEMSYMS);
    }
}

/* Generate lower and upper bound checks for memory access
   to memory slot [FIRST, LAST] againsr BOUNDS.  Checks
   are inserted before the position pointed by ITER.
   DIRFLAG indicates whether memory access is load or store.  */
void
chkp_check_mem_access (tree first, tree last, tree bounds,
		       gimple_stmt_iterator iter,
		       location_t location,
		       tree dirflag)
{
  chkp_check_lower (first, bounds, iter, location, dirflag);
  chkp_check_upper (last, bounds, iter, location, dirflag);
}

/* Replace call to _bnd_chk_* pointed by GSI with
   bndcu and bndcl calls.  DIRFLAG determines whether
   check is for read or write.  */

void
chkp_replace_address_check_builtin (gimple_stmt_iterator *gsi,
				    tree dirflag)
{
  gimple_stmt_iterator call_iter = *gsi;
  gimple *call = gsi_stmt (*gsi);
  tree fndecl = gimple_call_fndecl (call);
  tree addr = gimple_call_arg (call, 0);
  tree bounds = chkp_find_bounds (addr, gsi);

  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_LBOUNDS
      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_BOUNDS)
    chkp_check_lower (addr, bounds, *gsi, gimple_location (call), dirflag);

  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_UBOUNDS)
    chkp_check_upper (addr, bounds, *gsi, gimple_location (call), dirflag);

  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_BOUNDS)
    {
      tree size = gimple_call_arg (call, 1);
      addr = fold_build_pointer_plus (addr, size);
      addr = fold_build_pointer_plus_hwi (addr, -1);
      chkp_check_upper (addr, bounds, *gsi, gimple_location (call), dirflag);
    }

  gsi_remove (&call_iter, true);
}

/* Replace call to _bnd_get_ptr_* pointed by GSI with
   corresponding bounds extract call.  */

void
chkp_replace_extract_builtin (gimple_stmt_iterator *gsi)
{
  gimple *call = gsi_stmt (*gsi);
  tree fndecl = gimple_call_fndecl (call);
  tree addr = gimple_call_arg (call, 0);
  tree bounds = chkp_find_bounds (addr, gsi);
  gimple *extract;

  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_GET_PTR_LBOUND)
    fndecl = chkp_extract_lower_fndecl;
  else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_GET_PTR_UBOUND)
    fndecl = chkp_extract_upper_fndecl;
  else
    gcc_unreachable ();

  extract = gimple_build_call (fndecl, 1, bounds);
  gimple_call_set_lhs (extract, gimple_call_lhs (call));
  chkp_mark_stmt (extract);

  gsi_replace (gsi, extract, false);
}

/* Return COMPONENT_REF accessing FIELD in OBJ.  */
static tree
chkp_build_component_ref (tree obj, tree field)
{
  tree res;

  /* If object is TMR then we do not use component_ref but
     add offset instead.  We need it to be able to get addr
     of the reasult later.  */
  if (TREE_CODE (obj) == TARGET_MEM_REF)
    {
      tree offs = TMR_OFFSET (obj);
      offs = fold_binary_to_constant (PLUS_EXPR, TREE_TYPE (offs),
				      offs, DECL_FIELD_OFFSET (field));

      gcc_assert (offs);

      res = copy_node (obj);
      TREE_TYPE (res) = TREE_TYPE (field);
      TMR_OFFSET (res) = offs;
    }
  else
    res = build3 (COMPONENT_REF, TREE_TYPE (field), obj, field, NULL_TREE);

  return res;
}

/* Return ARRAY_REF for array ARR and index IDX with
   specified element type ETYPE and element size ESIZE.  */
static tree
chkp_build_array_ref (tree arr, tree etype, tree esize,
		      unsigned HOST_WIDE_INT idx)
{
  tree index = build_int_cst (size_type_node, idx);
  tree res;

  /* If object is TMR then we do not use array_ref but
     add offset instead.  We need it to be able to get addr
     of the reasult later.  */
  if (TREE_CODE (arr) == TARGET_MEM_REF)
    {
      tree offs = TMR_OFFSET (arr);

      esize = fold_binary_to_constant (MULT_EXPR, TREE_TYPE (esize),
				     esize, index);
      gcc_assert(esize);

      offs = fold_binary_to_constant (PLUS_EXPR, TREE_TYPE (offs),
				    offs, esize);
      gcc_assert (offs);

      res = copy_node (arr);
      TREE_TYPE (res) = etype;
      TMR_OFFSET (res) = offs;
    }
  else
    res = build4 (ARRAY_REF, etype, arr, index, NULL_TREE, NULL_TREE);

  return res;
}

/* Helper function for chkp_add_bounds_to_call_stmt.
   Fill ALL_BOUNDS output array with created bounds.

   OFFS is used for recursive calls and holds basic
   offset of TYPE in outer structure in bits.

   ITER points a position where bounds are searched.

   ALL_BOUNDS[i] is filled with elem bounds if there
   is a field in TYPE which has pointer type and offset
   equal to i * POINTER_SIZE in bits.  */
static void
chkp_find_bounds_for_elem (tree elem, tree *all_bounds,
			   HOST_WIDE_INT offs,
			   gimple_stmt_iterator *iter)
{
  tree type = TREE_TYPE (elem);

  if (BOUNDED_TYPE_P (type))
    {
      if (!all_bounds[offs / POINTER_SIZE])
	{
	  tree temp = make_temp_ssa_name (type, NULL, "");
	  gimple *assign = gimple_build_assign (temp, elem);
	  gimple_stmt_iterator gsi;

	  gsi_insert_before (iter, assign, GSI_SAME_STMT);
	  gsi = gsi_for_stmt (assign);

	  all_bounds[offs / POINTER_SIZE] = chkp_find_bounds (temp, &gsi);
	}
    }
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    tree base = unshare_expr (elem);
	    tree field_ref = chkp_build_component_ref (base, field);
	    HOST_WIDE_INT field_offs
	      = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field));
	    if (DECL_FIELD_OFFSET (field))
	      field_offs += TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field)) * 8;

	    chkp_find_bounds_for_elem (field_ref, all_bounds,
				       offs + field_offs, iter);
	  }
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree maxval = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
      tree etype = TREE_TYPE (type);
      HOST_WIDE_INT esize = TREE_INT_CST_LOW (TYPE_SIZE (etype));
      unsigned HOST_WIDE_INT cur;

      if (!maxval || integer_minus_onep (maxval))
	return;

      for (cur = 0; cur <= TREE_INT_CST_LOW (maxval); cur++)
	{
	  tree base = unshare_expr (elem);
	  tree arr_elem = chkp_build_array_ref (base, etype,
						TYPE_SIZE (etype),
						cur);
	  chkp_find_bounds_for_elem (arr_elem, all_bounds, offs + cur * esize,
				     iter);
	}
    }
}

/* Fill HAVE_BOUND output bitmap with information about
   bounds requred for object of type TYPE.

   OFFS is used for recursive calls and holds basic
   offset of TYPE in outer structure in bits.

   HAVE_BOUND[i] is set to 1 if there is a field
   in TYPE which has pointer type and offset
   equal to i * POINTER_SIZE - OFFS in bits.  */
void
chkp_find_bound_slots_1 (const_tree type, bitmap have_bound,
			 HOST_WIDE_INT offs)
{
  if (BOUNDED_TYPE_P (type))
    bitmap_set_bit (have_bound, offs / POINTER_SIZE);
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL)
	  {
	    HOST_WIDE_INT field_offs = 0;
	    if (DECL_FIELD_BIT_OFFSET (field))
	      field_offs += TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field));
	    if (DECL_FIELD_OFFSET (field))
	      field_offs += TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field)) * 8;
	    chkp_find_bound_slots_1 (TREE_TYPE (field), have_bound,
				     offs + field_offs);
	  }
    }
  else if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    {
      /* The object type is an array of complete type, i.e., other
	 than a flexible array.  */
      tree maxval = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
      tree etype = TREE_TYPE (type);
      HOST_WIDE_INT esize = TREE_INT_CST_LOW (TYPE_SIZE (etype));
      unsigned HOST_WIDE_INT cur;

      if (!maxval
	  || TREE_CODE (maxval) != INTEGER_CST
	  || integer_minus_onep (maxval))
	return;

      for (cur = 0; cur <= TREE_INT_CST_LOW (maxval); cur++)
	chkp_find_bound_slots_1 (etype, have_bound, offs + cur * esize);
    }
}

/* Fill bitmap RES with information about bounds for
   type TYPE.  See chkp_find_bound_slots_1 for more
   details.  */
void
chkp_find_bound_slots (const_tree type, bitmap res)
{
  bitmap_clear (res);
  chkp_find_bound_slots_1 (type, res, 0);
}

/* Return 1 if call to FNDECL should be instrumented
   and 0 otherwise.  */

static bool
chkp_instrument_normal_builtin (tree fndecl)
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
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRNCMP:
    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_MALLOC:
    case BUILT_IN_CALLOC:
    case BUILT_IN_REALLOC:
      return 1;

    default:
      return 0;
    }
}

/* Add bound arguments to call statement pointed by GSI.
   Also performs a replacement of user checker builtins calls
   with internal ones.  */

static void
chkp_add_bounds_to_call_stmt (gimple_stmt_iterator *gsi)
{
  gcall *call = as_a <gcall *> (gsi_stmt (*gsi));
  unsigned arg_no = 0;
  tree fndecl = gimple_call_fndecl (call);
  tree fntype;
  tree first_formal_arg;
  tree arg;
  bool use_fntype = false;
  tree op;
  ssa_op_iter iter;
  gcall *new_call;

  /* Do nothing for internal functions.  */
  if (gimple_call_internal_p (call))
    return;

  fntype = TREE_TYPE (TREE_TYPE (gimple_call_fn (call)));

  /* Do nothing if back-end builtin is called.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return;

  /* Do nothing for some middle-end builtins.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_OBJECT_SIZE)
    return;

  /* Do nothing for calls to not instrumentable functions.  */
  if (fndecl && !chkp_instrumentable_p (fndecl))
    return;

  /* Ignore CHKP_INIT_PTR_BOUNDS, CHKP_NULL_PTR_BOUNDS
     and CHKP_COPY_PTR_BOUNDS.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_INIT_PTR_BOUNDS
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_NULL_PTR_BOUNDS
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_COPY_PTR_BOUNDS
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_SET_PTR_BOUNDS))
    return;

  /* Check user builtins are replaced with checks.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_LBOUNDS
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_UBOUNDS
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_CHECK_PTR_BOUNDS))
    {
      chkp_replace_address_check_builtin (gsi, integer_minus_one_node);
      return;
    }

  /* Check user builtins are replaced with bound extract.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_GET_PTR_LBOUND
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_GET_PTR_UBOUND))
    {
      chkp_replace_extract_builtin (gsi);
      return;
    }

  /* BUILT_IN_CHKP_NARROW_PTR_BOUNDS call is replaced with
     target narrow bounds call.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_NARROW_PTR_BOUNDS)
    {
      tree arg = gimple_call_arg (call, 1);
      tree bounds = chkp_find_bounds (arg, gsi);

      gimple_call_set_fndecl (call, chkp_narrow_bounds_fndecl);
      gimple_call_set_arg (call, 1, bounds);
      update_stmt (call);

      return;
    }

  /* BUILT_IN_CHKP_STORE_PTR_BOUNDS call is replaced with
     bndstx call.  */
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_STORE_PTR_BOUNDS)
    {
      tree addr = gimple_call_arg (call, 0);
      tree ptr = gimple_call_arg (call, 1);
      tree bounds = chkp_find_bounds (ptr, gsi);
      gimple_stmt_iterator iter = gsi_for_stmt (call);

      chkp_build_bndstx (addr, ptr, bounds, gsi);
      gsi_remove (&iter, true);

      return;
    }

  if (!flag_chkp_instrument_calls)
    return;

  /* We instrument only some subset of builtins.  We also instrument
     builtin calls to be inlined.  */
  if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && !chkp_instrument_normal_builtin (fndecl))
    {
      if (!lookup_attribute ("always_inline", DECL_ATTRIBUTES (fndecl)))
	return;

      struct cgraph_node *clone = chkp_maybe_create_clone (fndecl);
      if (!clone
	  || !gimple_has_body_p (clone->decl))
	return;
    }

  /* If function decl is available then use it for
     formal arguments list.  Otherwise use function type.  */
  if (fndecl
      && DECL_ARGUMENTS (fndecl)
      && gimple_call_fntype (call) == TREE_TYPE (fndecl))
    first_formal_arg = DECL_ARGUMENTS (fndecl);
  else
    {
      first_formal_arg = TYPE_ARG_TYPES (fntype);
      use_fntype = true;
    }

  /* Fill vector of new call args.  */
  vec<tree> new_args = vNULL;
  new_args.create (gimple_call_num_args (call));
  arg = first_formal_arg;
  for (arg_no = 0; arg_no < gimple_call_num_args (call); arg_no++)
    {
      tree call_arg = gimple_call_arg (call, arg_no);
      tree type;

      /* Get arg type using formal argument description
	 or actual argument type.  */
      if (arg)
	if (use_fntype)
	  if (TREE_VALUE (arg) != void_type_node)
	    {
	      type = TREE_VALUE (arg);
	      arg = TREE_CHAIN (arg);
	    }
	  else
	    type = TREE_TYPE (call_arg);
	else
	  {
	    type = TREE_TYPE (arg);
	    arg = TREE_CHAIN (arg);
	  }
      else
	type = TREE_TYPE (call_arg);

      new_args.safe_push (call_arg);

      if (BOUNDED_TYPE_P (type)
	  || pass_by_reference (NULL, TYPE_MODE (type), type, true))
	new_args.safe_push (chkp_find_bounds (call_arg, gsi));
      else if (chkp_type_has_pointer (type))
	{
	  HOST_WIDE_INT max_bounds
	    = TREE_INT_CST_LOW (TYPE_SIZE (type)) / POINTER_SIZE;
	  tree *all_bounds = (tree *)xmalloc (sizeof (tree) * max_bounds);
	  HOST_WIDE_INT bnd_no;

	  memset (all_bounds, 0, sizeof (tree) * max_bounds);

	  chkp_find_bounds_for_elem (call_arg, all_bounds, 0, gsi);

	  for (bnd_no = 0; bnd_no < max_bounds; bnd_no++)
	    if (all_bounds[bnd_no])
	      new_args.safe_push (all_bounds[bnd_no]);

           free (all_bounds);
	}
    }

  if (new_args.length () == gimple_call_num_args (call))
    new_call = call;
  else
    {
      new_call = gimple_build_call_vec (gimple_op (call, 1), new_args);
      gimple_call_set_lhs (new_call, gimple_call_lhs (call));
      gimple_call_copy_flags (new_call, call);
      gimple_call_set_chain (new_call, gimple_call_chain (call));
    }
  new_args.release ();

  /* For direct calls fndecl is replaced with instrumented version.  */
  if (fndecl)
    {
      tree new_decl = chkp_maybe_create_clone (fndecl)->decl;
      gimple_call_set_fndecl (new_call, new_decl);
      /* In case of a type cast we should modify used function
	 type instead of using type of new fndecl.  */
      if (gimple_call_fntype (call) != TREE_TYPE (fndecl))
	{
	  tree type = gimple_call_fntype (call);
	  type = chkp_copy_function_type_adding_bounds (type);
	  gimple_call_set_fntype (new_call, type);
	}
      else
	gimple_call_set_fntype (new_call, TREE_TYPE (new_decl));
    }
  /* For indirect call we should fix function pointer type if
     pass some bounds.  */
  else if (new_call != call)
    {
      tree type = gimple_call_fntype (call);
      type = chkp_copy_function_type_adding_bounds (type);
      gimple_call_set_fntype (new_call, type);
    }

  /* replace old call statement with the new one.  */
  if (call != new_call)
    {
      FOR_EACH_SSA_TREE_OPERAND (op, call, iter, SSA_OP_ALL_DEFS)
	{
	  SSA_NAME_DEF_STMT (op) = new_call;
	}
      gsi_replace (gsi, new_call, true);
    }
  else
    update_stmt (new_call);

  gimple_call_set_with_bounds (new_call, true);
}

/* Return constant static bounds var with specified bounds LB and UB.
   If such var does not exists then new var is created with specified NAME.  */
static tree
chkp_make_static_const_bounds (HOST_WIDE_INT lb,
			       HOST_WIDE_INT ub,
			       const char *name)
{
  tree id = get_identifier (name);
  tree var;
  varpool_node *node;
  symtab_node *snode;

  var  = build_decl (UNKNOWN_LOCATION, VAR_DECL, id,
		     pointer_bounds_type_node);
  TREE_STATIC (var) = 1;
  TREE_PUBLIC (var) = 1;

  /* With LTO we may have constant bounds already in varpool.
     Try to find it.  */
  if ((snode = symtab_node::get_for_asmname (DECL_ASSEMBLER_NAME (var))))
    {
      /* We don't allow this symbol usage for non bounds.  */
      if (snode->type != SYMTAB_VARIABLE
	  || !POINTER_BOUNDS_P (snode->decl))
	sorry ("-fcheck-pointer-bounds requires '%s' "
	       "name for internal usage",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (var)));

      return snode->decl;
    }

  TREE_USED (var) = 1;
  TREE_READONLY (var) = 1;
  TREE_ADDRESSABLE (var) = 0;
  DECL_ARTIFICIAL (var) = 1;
  DECL_READ_P (var) = 1;
  DECL_INITIAL (var) = targetm.chkp_make_bounds_constant (lb, ub);
  make_decl_one_only (var, DECL_ASSEMBLER_NAME (var));
  /* We may use this symbol during ctors generation in chkp_finish_file
     when all symbols are emitted.  Force output to avoid undefined
     symbols in ctors.  */
  node = varpool_node::get_create (var);
  node->force_output = 1;

  varpool_node::finalize_decl (var);

  return var;
}

/* Generate code to make bounds with specified lower bound LB and SIZE.
   if AFTER is 1 then code is inserted after position pointed by ITER
   otherwise code is inserted before position pointed by ITER.
   If ITER is NULL then code is added to entry block.  */
static tree
chkp_make_bounds (tree lb, tree size, gimple_stmt_iterator *iter, bool after)
{
  gimple_seq seq;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  tree bounds;

  if (iter)
    gsi = *iter;
  else
    gsi = gsi_start_bb (chkp_get_entry_block ());

  seq = NULL;

  lb = chkp_force_gimple_call_op (lb, &seq);
  size = chkp_force_gimple_call_op (size, &seq);

  stmt = gimple_build_call (chkp_bndmk_fndecl, 2, lb, size);
  chkp_mark_stmt (stmt);

  bounds = chkp_get_tmp_reg (stmt);
  gimple_call_set_lhs (stmt, bounds);

  gimple_seq_add_stmt (&seq, stmt);

  if (iter && after)
    gsi_insert_seq_after (&gsi, seq, GSI_SAME_STMT);
  else
    gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Made bounds: ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
      if (iter)
	{
	  fprintf (dump_file, "  inserted before statement: ");
	  print_gimple_stmt (dump_file, gsi_stmt (*iter), 0, TDF_VOPS|TDF_MEMSYMS);
	}
      else
	fprintf (dump_file, "  at function entry\n");
    }

  /* update_stmt (stmt); */

  return bounds;
}

/* Return var holding zero bounds.  */
tree
chkp_get_zero_bounds_var (void)
{
  if (!chkp_zero_bounds_var)
    chkp_zero_bounds_var
      = chkp_make_static_const_bounds (0, -1,
				       CHKP_ZERO_BOUNDS_VAR_NAME);
  return chkp_zero_bounds_var;
}

/* Return var holding none bounds.  */
tree
chkp_get_none_bounds_var (void)
{
  if (!chkp_none_bounds_var)
    chkp_none_bounds_var
      = chkp_make_static_const_bounds (-1, 0,
				       CHKP_NONE_BOUNDS_VAR_NAME);
  return chkp_none_bounds_var;
}

/* Return SSA_NAME used to represent zero bounds.  */
static tree
chkp_get_zero_bounds (void)
{
  if (zero_bounds)
    return zero_bounds;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Creating zero bounds...");

  if ((flag_chkp_use_static_bounds && flag_chkp_use_static_const_bounds)
      || flag_chkp_use_static_const_bounds > 0)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (chkp_get_entry_block ());
      gimple *stmt;

      zero_bounds = chkp_get_tmp_reg (NULL);
      stmt = gimple_build_assign (zero_bounds, chkp_get_zero_bounds_var ());
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
  else
    zero_bounds = chkp_make_bounds (integer_zero_node,
				    integer_zero_node,
				    NULL,
				    false);

  return zero_bounds;
}

/* Return SSA_NAME used to represent none bounds.  */
static tree
chkp_get_none_bounds (void)
{
  if (none_bounds)
    return none_bounds;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Creating none bounds...");


  if ((flag_chkp_use_static_bounds && flag_chkp_use_static_const_bounds)
      || flag_chkp_use_static_const_bounds > 0)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (chkp_get_entry_block ());
      gimple *stmt;

      none_bounds = chkp_get_tmp_reg (NULL);
      stmt = gimple_build_assign (none_bounds, chkp_get_none_bounds_var ());
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
  else
    none_bounds = chkp_make_bounds (integer_minus_one_node,
				    build_int_cst (size_type_node, 2),
				    NULL,
				    false);

  return none_bounds;
}

/* Return bounds to be used as a result of operation which
   should not create poiunter (e.g. MULT_EXPR).  */
static tree
chkp_get_invalid_op_bounds (void)
{
  return chkp_get_zero_bounds ();
}

/* Return bounds to be used for loads of non-pointer values.  */
static tree
chkp_get_nonpointer_load_bounds (void)
{
  return chkp_get_zero_bounds ();
}

/* Return 1 if may use bndret call to get bounds for pointer
   returned by CALL.  */
static bool
chkp_call_returns_bounds_p (gcall *call)
{
  if (gimple_call_internal_p (call))
    {
      if (gimple_call_internal_fn (call) == IFN_VA_ARG)
	return true;
      return false;
    }

  if (gimple_call_builtin_p (call, BUILT_IN_CHKP_NARROW_PTR_BOUNDS)
      || chkp_gimple_call_builtin_p (call, BUILT_IN_CHKP_NARROW))
    return true;

  if (gimple_call_with_bounds_p (call))
    return true;

  tree fndecl = gimple_call_fndecl (call);

  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return false;

  if (fndecl && !chkp_instrumentable_p (fndecl))
    return false;

  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      if (chkp_instrument_normal_builtin (fndecl))
	return true;

      if (!lookup_attribute ("always_inline", DECL_ATTRIBUTES (fndecl)))
	return false;

      struct cgraph_node *clone = chkp_maybe_create_clone (fndecl);
      return (clone && gimple_has_body_p (clone->decl));
    }

  return true;
}

/* Build bounds returned by CALL.  */
static tree
chkp_build_returned_bound (gcall *call)
{
  gimple_stmt_iterator gsi;
  tree bounds;
  gimple *stmt;
  tree fndecl = gimple_call_fndecl (call);
  unsigned int retflags;

  /* To avoid fixing alloca expands in targets we handle
     it separately.  */
  if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA
	  || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_ALLOCA_WITH_ALIGN))
    {
      tree size = gimple_call_arg (call, 0);
      tree lb = gimple_call_lhs (call);
      gimple_stmt_iterator iter = gsi_for_stmt (call);
      bounds = chkp_make_bounds (lb, size, &iter, true);
    }
  /* We know bounds returned by set_bounds builtin call.  */
  else if (fndecl
	   && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	   && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_SET_PTR_BOUNDS)
    {
      tree lb = gimple_call_arg (call, 0);
      tree size = gimple_call_arg (call, 1);
      gimple_stmt_iterator iter = gsi_for_stmt (call);
      bounds = chkp_make_bounds (lb, size, &iter, true);
    }
  /* Detect bounds initialization calls.  */
  else if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_INIT_PTR_BOUNDS)
    bounds = chkp_get_zero_bounds ();
  /* Detect bounds nullification calls.  */
  else if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_NULL_PTR_BOUNDS)
    bounds = chkp_get_none_bounds ();
  /* Detect bounds copy calls.  */
  else if (fndecl
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CHKP_COPY_PTR_BOUNDS)
    {
      gimple_stmt_iterator iter = gsi_for_stmt (call);
      bounds = chkp_find_bounds (gimple_call_arg (call, 1), &iter);
    }
  /* Do not use retbnd when returned bounds are equal to some
     of passed bounds.  */
  else if (((retflags = gimple_call_return_flags (call)) & ERF_RETURNS_ARG)
	   && (retflags & ERF_RETURN_ARG_MASK) < gimple_call_num_args (call))
    {
      gimple_stmt_iterator iter = gsi_for_stmt (call);
      unsigned int retarg = retflags & ERF_RETURN_ARG_MASK, argno;
      if (gimple_call_with_bounds_p (call))
	{
	  for (argno = 0; argno < gimple_call_num_args (call); argno++)
	    if (!POINTER_BOUNDS_P (gimple_call_arg (call, argno)))
	      {
		if (retarg)
		  retarg--;
		else
		  break;
	      }
	}
      else
	argno = retarg;

      bounds = chkp_find_bounds (gimple_call_arg (call, argno), &iter);
    }
  else if (chkp_call_returns_bounds_p (call))
    {
      gcc_assert (TREE_CODE (gimple_call_lhs (call)) == SSA_NAME);

      /* In general case build checker builtin call to
	 obtain returned bounds.  */
      stmt = gimple_build_call (chkp_ret_bnd_fndecl, 1,
				gimple_call_lhs (call));
      chkp_mark_stmt (stmt);

      gsi = gsi_for_stmt (call);
      gsi_insert_after (&gsi, stmt, GSI_SAME_STMT);

      bounds = chkp_get_tmp_reg (stmt);
      gimple_call_set_lhs (stmt, bounds);

      update_stmt (stmt);
    }
  else
    bounds = chkp_get_zero_bounds ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Built returned bounds (");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, ") for call: ");
      print_gimple_stmt (dump_file, call, 0, TDF_VOPS|TDF_MEMSYMS);
    }

  bounds = chkp_maybe_copy_and_register_bounds (gimple_call_lhs (call), bounds);

  return bounds;
}

/* Return bounds used as returned by call
   which produced SSA name VAL.  */
gcall *
chkp_retbnd_call_by_val (tree val)
{
  if (TREE_CODE (val) != SSA_NAME)
    return NULL;

  gcc_assert (gimple_code (SSA_NAME_DEF_STMT (val)) == GIMPLE_CALL);

  imm_use_iterator use_iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, use_iter, val)
    if (gimple_code (USE_STMT (use_p)) == GIMPLE_CALL
	&& gimple_call_fndecl (USE_STMT (use_p)) == chkp_ret_bnd_fndecl)
      return as_a <gcall *> (USE_STMT (use_p));

  return NULL;
}

/* Check the next parameter for the given PARM is bounds
   and return it's default SSA_NAME (create if required).  */
static tree
chkp_get_next_bounds_parm (tree parm)
{
  tree bounds = TREE_CHAIN (parm);
  gcc_assert (POINTER_BOUNDS_P (bounds));
  bounds = ssa_default_def (cfun, bounds);
  if (!bounds)
    {
      bounds = make_ssa_name (TREE_CHAIN (parm), gimple_build_nop ());
      set_ssa_default_def (cfun, TREE_CHAIN (parm), bounds);
    }
  return bounds;
}

/* Return bounds to be used for input argument PARM.  */
static tree
chkp_get_bound_for_parm (tree parm)
{
  tree decl = SSA_NAME_VAR (parm);
  tree bounds;

  gcc_assert (TREE_CODE (decl) == PARM_DECL);

  bounds = chkp_get_registered_bounds (parm);

  if (!bounds)
    bounds = chkp_get_registered_bounds (decl);

  if (!bounds)
    {
      tree orig_decl = cgraph_node::get (cfun->decl)->orig_decl;

      /* For static chain param we return zero bounds
	 because currently we do not check dereferences
	 of this pointer.  */
      if (cfun->static_chain_decl == decl)
	bounds = chkp_get_zero_bounds ();
      /* If non instrumented runtime is used then it may be useful
	 to use zero bounds for input arguments of main
	 function.  */
      else if (flag_chkp_zero_input_bounds_for_main
	       && strcmp (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (orig_decl)),
			  "main") == 0)
	bounds = chkp_get_zero_bounds ();
      else if (BOUNDED_P (parm))
	{
	  bounds = chkp_get_next_bounds_parm (decl);
	  bounds = chkp_maybe_copy_and_register_bounds (decl, bounds);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Built arg bounds (");
	      print_generic_expr (dump_file, bounds, 0);
	      fprintf (dump_file, ") for arg: ");
	      print_node (dump_file, "", decl, 0);
	    }
	}
      else
	bounds = chkp_get_zero_bounds ();
    }

  if (!chkp_get_registered_bounds (parm))
    bounds = chkp_maybe_copy_and_register_bounds (parm, bounds);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Using bounds ");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, " for parm ");
      print_generic_expr (dump_file, parm, 0);
      fprintf (dump_file, " of type ");
      print_generic_expr (dump_file, TREE_TYPE (parm), 0);
      fprintf (dump_file, ".\n");
    }

  return bounds;
}

/* Build and return CALL_EXPR for bndstx builtin with specified
   arguments.  */
tree
chkp_build_bndldx_call (tree addr, tree ptr)
{
  tree fn = build1 (ADDR_EXPR,
		    build_pointer_type (TREE_TYPE (chkp_bndldx_fndecl)),
		    chkp_bndldx_fndecl);
  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (chkp_bndldx_fndecl)),
			       fn, 2, addr, ptr);
  CALL_WITH_BOUNDS_P (call) = true;
  return call;
}

/* Insert code to load bounds for PTR located by ADDR.
   Code is inserted after position pointed by GSI.
   Loaded bounds are returned.  */
static tree
chkp_build_bndldx (tree addr, tree ptr, gimple_stmt_iterator *gsi)
{
  gimple_seq seq;
  gimple *stmt;
  tree bounds;

  seq = NULL;

  addr = chkp_force_gimple_call_op (addr, &seq);
  ptr = chkp_force_gimple_call_op (ptr, &seq);

  stmt = gimple_build_call (chkp_bndldx_fndecl, 2, addr, ptr);
  chkp_mark_stmt (stmt);
  bounds = chkp_get_tmp_reg (stmt);
  gimple_call_set_lhs (stmt, bounds);

  gimple_seq_add_stmt (&seq, stmt);

  gsi_insert_seq_after (gsi, seq, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Generated bndldx for pointer ");
      print_generic_expr (dump_file, ptr, 0);
      fprintf (dump_file, ": ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
    }

  return bounds;
}

/* Build and return CALL_EXPR for bndstx builtin with specified
   arguments.  */
tree
chkp_build_bndstx_call (tree addr, tree ptr, tree bounds)
{
  tree fn = build1 (ADDR_EXPR,
		    build_pointer_type (TREE_TYPE (chkp_bndstx_fndecl)),
		    chkp_bndstx_fndecl);
  tree call = build_call_nary (TREE_TYPE (TREE_TYPE (chkp_bndstx_fndecl)),
			       fn, 3, ptr, bounds, addr);
  CALL_WITH_BOUNDS_P (call) = true;
  return call;
}

/* Insert code to store BOUNDS for PTR stored by ADDR.
   New statements are inserted after position pointed
   by GSI.  */
void
chkp_build_bndstx (tree addr, tree ptr, tree bounds,
		   gimple_stmt_iterator *gsi)
{
  gimple_seq seq;
  gimple *stmt;

  seq = NULL;

  addr = chkp_force_gimple_call_op (addr, &seq);
  ptr = chkp_force_gimple_call_op (ptr, &seq);

  stmt = gimple_build_call (chkp_bndstx_fndecl, 3, ptr, bounds, addr);
  chkp_mark_stmt (stmt);
  gimple_call_set_with_bounds (stmt, true);

  gimple_seq_add_stmt (&seq, stmt);

  gsi_insert_seq_after (gsi, seq, GSI_CONTINUE_LINKING);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Generated bndstx for pointer store ");
      print_gimple_stmt (dump_file, gsi_stmt (*gsi), 0, TDF_VOPS|TDF_MEMSYMS);
      print_gimple_stmt (dump_file, stmt, 2, TDF_VOPS|TDF_MEMSYMS);
    }
}

/* This function is called when call statement
   is inlined and therefore we can't use bndret
   for its LHS anymore.  Function fixes bndret
   call using new RHS value if possible.  */
void
chkp_fixup_inlined_call (tree lhs, tree rhs)
{
  tree addr, bounds;
  gcall *retbnd, *bndldx;

  if (!BOUNDED_P (lhs))
    return;

  /* Search for retbnd call.  */
  retbnd = chkp_retbnd_call_by_val (lhs);
  if (!retbnd)
    return;

  /* Currently only handle cases when call is replaced
     with a memory access.  In this case bndret call
     may be replaced with bndldx call.  Otherwise we
     have to search for bounds which may cause wrong
     result due to various optimizations applied.  */
  switch (TREE_CODE (rhs))
    {
    case VAR_DECL:
      if (DECL_REGISTER (rhs))
	return;
      break;

    case MEM_REF:
      break;

    case ARRAY_REF:
    case COMPONENT_REF:
      addr = get_base_address (rhs);
      if (!DECL_P (addr)
	  && TREE_CODE (addr) != MEM_REF)
	return;
      if (DECL_P (addr) && DECL_REGISTER (addr))
	return;
      break;

    default:
      return;
    }

  /* Create a new statements sequence with bndldx call.  */
  gimple_stmt_iterator gsi = gsi_for_stmt (retbnd);
  addr = build_fold_addr_expr (rhs);
  chkp_build_bndldx (addr, lhs, &gsi);
  bndldx = as_a <gcall *> (gsi_stmt (gsi));

  /* Remove bndret call.  */
  bounds = gimple_call_lhs (retbnd);
  gsi = gsi_for_stmt (retbnd);
  gsi_remove (&gsi, true);

  /* Link new bndldx call.  */
  gimple_call_set_lhs (bndldx, bounds);
  update_stmt (bndldx);
}

/* Compute bounds for pointer NODE which was assigned in
   assignment statement ASSIGN.  Return computed bounds.  */
static tree
chkp_compute_bounds_for_assignment (tree node, gimple *assign)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (assign);
  tree rhs1 = gimple_assign_rhs1 (assign);
  tree bounds = NULL_TREE;
  gimple_stmt_iterator iter = gsi_for_stmt (assign);
  tree base = NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Computing bounds for assignment: ");
      print_gimple_stmt (dump_file, assign, 0, TDF_VOPS|TDF_MEMSYMS);
    }

  switch (rhs_code)
    {
    case MEM_REF:
    case TARGET_MEM_REF:
    case COMPONENT_REF:
    case ARRAY_REF:
      /* We need to load bounds from the bounds table.  */
      bounds = chkp_find_bounds_loaded (node, rhs1, &iter);
      break;

    case VAR_DECL:
    case SSA_NAME:
    case ADDR_EXPR:
    case POINTER_PLUS_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case INTEGER_CST:
      /* Bounds are just propagated from RHS.  */
      bounds = chkp_find_bounds (rhs1, &iter);
      base = rhs1;
      break;

    case VIEW_CONVERT_EXPR:
      /* Bounds are just propagated from RHS.  */
      bounds = chkp_find_bounds (TREE_OPERAND (rhs1, 0), &iter);
      break;

    case PARM_DECL:
      if (BOUNDED_P (rhs1))
	{
	  /* We need to load bounds from the bounds table.  */
	  bounds = chkp_build_bndldx (chkp_build_addr_expr (rhs1),
				      node, &iter);
	  TREE_ADDRESSABLE (rhs1) = 1;
	}
      else
	bounds = chkp_get_nonpointer_load_bounds ();
      break;

    case MINUS_EXPR:
    case PLUS_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (assign);
	tree bnd1 = chkp_find_bounds (rhs1, &iter);
	tree bnd2 = chkp_find_bounds (rhs2, &iter);

	/* First we try to check types of operands.  If it
	   does not help then look at bound values.

	   If some bounds are incomplete and other are
	   not proven to be valid (i.e. also incomplete
	   or invalid because value is not pointer) then
	   resulting value is incomplete and will be
	   recomputed later in chkp_finish_incomplete_bounds.  */
	if (BOUNDED_P (rhs1)
	    && !BOUNDED_P (rhs2))
	  bounds = bnd1;
	else if (BOUNDED_P (rhs2)
		 && !BOUNDED_P (rhs1)
		 && rhs_code != MINUS_EXPR)
	  bounds = bnd2;
	else if (chkp_incomplete_bounds (bnd1))
	  if (chkp_valid_bounds (bnd2) && rhs_code != MINUS_EXPR
	      && !chkp_incomplete_bounds (bnd2))
	    bounds = bnd2;
	  else
	    bounds = incomplete_bounds;
	else if (chkp_incomplete_bounds (bnd2))
	  if (chkp_valid_bounds (bnd1)
	      && !chkp_incomplete_bounds (bnd1))
	    bounds = bnd1;
	  else
	    bounds = incomplete_bounds;
	else if (!chkp_valid_bounds (bnd1))
	  if (chkp_valid_bounds (bnd2) && rhs_code != MINUS_EXPR)
	    bounds = bnd2;
	  else if (bnd2 == chkp_get_zero_bounds ())
	    bounds = bnd2;
	  else
	    bounds = bnd1;
	else if (!chkp_valid_bounds (bnd2))
	  bounds = bnd1;
	else
	  /* Seems both operands may have valid bounds
	     (e.g. pointer minus pointer).  In such case
	     use default invalid op bounds.  */
	  bounds = chkp_get_invalid_op_bounds ();

	base = (bounds == bnd1) ? rhs1 : (bounds == bnd2) ? rhs2 : NULL;
      }
      break;

    case BIT_NOT_EXPR:
    case NEGATE_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case MULT_EXPR:
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case EXACT_DIV_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* No valid bounds may be produced by these exprs.  */
      bounds = chkp_get_invalid_op_bounds ();
      break;

    case COND_EXPR:
      {
	tree val1 = gimple_assign_rhs2 (assign);
	tree val2 = gimple_assign_rhs3 (assign);
	tree bnd1 = chkp_find_bounds (val1, &iter);
	tree bnd2 = chkp_find_bounds (val2, &iter);
	gimple *stmt;

	if (chkp_incomplete_bounds (bnd1) || chkp_incomplete_bounds (bnd2))
	  bounds = incomplete_bounds;
	else if (bnd1 == bnd2)
	  bounds = bnd1;
	else
	  {
	    rhs1 = unshare_expr (rhs1);

	    bounds = chkp_get_tmp_reg (assign);
	    stmt = gimple_build_assign (bounds, COND_EXPR, rhs1, bnd1, bnd2);
	    gsi_insert_after (&iter, stmt, GSI_SAME_STMT);

	    if (!chkp_valid_bounds (bnd1) && !chkp_valid_bounds (bnd2))
	      chkp_mark_invalid_bounds (bounds);
	  }
      }
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (assign);
	tree bnd1 = chkp_find_bounds (rhs1, &iter);
	tree bnd2 = chkp_find_bounds (rhs2, &iter);

	if (chkp_incomplete_bounds (bnd1) || chkp_incomplete_bounds (bnd2))
	  bounds = incomplete_bounds;
	else if (bnd1 == bnd2)
	  bounds = bnd1;
	else
	  {
	    gimple *stmt;
	    tree cond = build2 (rhs_code == MAX_EXPR ? GT_EXPR : LT_EXPR,
				boolean_type_node, rhs1, rhs2);
	    bounds = chkp_get_tmp_reg (assign);
	    stmt = gimple_build_assign (bounds, COND_EXPR, cond, bnd1, bnd2);

	    gsi_insert_after (&iter, stmt, GSI_SAME_STMT);

	    if (!chkp_valid_bounds (bnd1) && !chkp_valid_bounds (bnd2))
	      chkp_mark_invalid_bounds (bounds);
	  }
      }
      break;

    default:
      bounds = chkp_get_zero_bounds ();
      warning (0, "pointer bounds were lost due to unexpected expression %s",
	       get_tree_code_name (rhs_code));
    }

  gcc_assert (bounds);

  /* We may reuse bounds of other pointer we copy/modify.  But it is not
     allowed for abnormal ssa names.  If we produced a pointer using
     abnormal ssa name, we better make a bounds copy to avoid coalescing
     issues.  */
  if (base
      && TREE_CODE (base) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (base))
    {
      gimple *stmt = gimple_build_assign (chkp_get_tmp_reg (NULL), bounds);
      gsi_insert_after (&iter, stmt, GSI_SAME_STMT);
      bounds = gimple_assign_lhs (stmt);
    }

  if (node)
    bounds = chkp_maybe_copy_and_register_bounds (node, bounds);

  return bounds;
}

/* Compute bounds for ssa name NODE defined by DEF_STMT pointed by ITER.

   There are just few statement codes allowed: NOP (for default ssa names),
   ASSIGN, CALL, PHI, ASM.

   Return computed bounds.  */
static tree
chkp_get_bounds_by_definition (tree node, gimple *def_stmt,
			       gphi_iterator *iter)
{
  tree var, bounds;
  enum gimple_code code = gimple_code (def_stmt);
  gphi *stmt;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Searching for bounds for node: ");
      print_generic_expr (dump_file, node, 0);

      fprintf (dump_file, " using its definition: ");
      print_gimple_stmt (dump_file, def_stmt, 0, TDF_VOPS|TDF_MEMSYMS);
    }

  switch (code)
    {
    case GIMPLE_NOP:
      var = SSA_NAME_VAR (node);
      switch (TREE_CODE (var))
	{
	case PARM_DECL:
	  bounds = chkp_get_bound_for_parm (node);
	  break;

	case VAR_DECL:
	  /* For uninitialized pointers use none bounds.  */
	  bounds = chkp_get_none_bounds ();
	  bounds = chkp_maybe_copy_and_register_bounds (node, bounds);
	  break;

	case RESULT_DECL:
	  {
	    tree base_type;

	    gcc_assert (TREE_CODE (TREE_TYPE (node)) == REFERENCE_TYPE);

	    base_type = TREE_TYPE (TREE_TYPE (node));

	    gcc_assert (TYPE_SIZE (base_type)
			&& TREE_CODE (TYPE_SIZE (base_type)) == INTEGER_CST
			&& tree_to_uhwi (TYPE_SIZE (base_type)) != 0);

	    bounds = chkp_make_bounds (node, TYPE_SIZE_UNIT (base_type),
				       NULL, false);
	    bounds = chkp_maybe_copy_and_register_bounds (node, bounds);
	  }
	  break;

	default:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Unexpected var with no definition\n");
	      print_generic_expr (dump_file, var, 0);
	    }
	  internal_error ("chkp_get_bounds_by_definition: Unexpected var of type %s",
			  get_tree_code_name (TREE_CODE (var)));
	}
      break;

    case GIMPLE_ASSIGN:
      bounds = chkp_compute_bounds_for_assignment (node, def_stmt);
      break;

    case GIMPLE_CALL:
      bounds = chkp_build_returned_bound (as_a <gcall *> (def_stmt));
      break;

    case GIMPLE_PHI:
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (node))
	if (SSA_NAME_VAR (node))
	  var = chkp_get_bounds_var (SSA_NAME_VAR (node));
	else
	  var = make_temp_ssa_name (pointer_bounds_type_node,
				    NULL,
				    CHKP_BOUND_TMP_NAME);
      else
	var = chkp_get_tmp_var ();
      stmt = create_phi_node (var, gimple_bb (def_stmt));
      bounds = gimple_phi_result (stmt);
      *iter = gsi_for_phi (stmt);

      bounds = chkp_maybe_copy_and_register_bounds (node, bounds);

      /* Created bounds do not have all phi args computed and
	 therefore we do not know if there is a valid source
	 of bounds for that node.  Therefore we mark bounds
	 as incomplete and then recompute them when all phi
	 args are computed.  */
      chkp_register_incomplete_bounds (bounds, node);
      break;

    case GIMPLE_ASM:
      bounds = chkp_get_zero_bounds ();
      bounds = chkp_maybe_copy_and_register_bounds (node, bounds);
      break;

    default:
      internal_error ("chkp_get_bounds_by_definition: Unexpected GIMPLE code %s",
		      gimple_code_name[code]);
    }

  return bounds;
}

/* Return CALL_EXPR for bndmk with specified LOWER_BOUND and SIZE.  */
tree
chkp_build_make_bounds_call (tree lower_bound, tree size)
{
  tree call = build1 (ADDR_EXPR,
		      build_pointer_type (TREE_TYPE (chkp_bndmk_fndecl)),
		      chkp_bndmk_fndecl);
  return build_call_nary (TREE_TYPE (TREE_TYPE (chkp_bndmk_fndecl)),
			  call, 2, lower_bound, size);
}

/* Create static bounds var of specfified OBJ which is
   is either VAR_DECL or string constant.  */
static tree
chkp_make_static_bounds (tree obj)
{
  static int string_id = 1;
  static int var_id = 1;
  tree *slot;
  const char *var_name;
  char *bnd_var_name;
  tree bnd_var;

  /* First check if we already have required var.  */
  if (chkp_static_var_bounds)
    {
      /* For vars we use assembler name as a key in
	 chkp_static_var_bounds map.  It allows to
	 avoid duplicating bound vars for decls
	 sharing assembler name.  */
      if (TREE_CODE (obj) == VAR_DECL)
	{
	  tree name = DECL_ASSEMBLER_NAME (obj);
	  slot = chkp_static_var_bounds->get (name);
	  if (slot)
	    return *slot;
	}
      else
	{
	  slot = chkp_static_var_bounds->get (obj);
	  if (slot)
	    return *slot;
	}
    }

  /* Build decl for bounds var.  */
  if (TREE_CODE (obj) == VAR_DECL)
    {
      if (DECL_IGNORED_P (obj))
	{
	  bnd_var_name = (char *) xmalloc (strlen (CHKP_VAR_BOUNDS_PREFIX) + 10);
	  sprintf (bnd_var_name, "%s%d", CHKP_VAR_BOUNDS_PREFIX, var_id++);
	}
      else
	{
	  var_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (obj));

	  /* For hidden symbols we want to skip first '*' char.  */
	  if (*var_name == '*')
	    var_name++;

	  bnd_var_name = (char *) xmalloc (strlen (var_name)
					   + strlen (CHKP_BOUNDS_OF_SYMBOL_PREFIX) + 1);
	  strcpy (bnd_var_name, CHKP_BOUNDS_OF_SYMBOL_PREFIX);
	  strcat (bnd_var_name, var_name);
	}

      bnd_var = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			    get_identifier (bnd_var_name),
			    pointer_bounds_type_node);

      /* Address of the obj will be used as lower bound.  */
      TREE_ADDRESSABLE (obj) = 1;
    }
  else
    {
      bnd_var_name = (char *) xmalloc (strlen (CHKP_STRING_BOUNDS_PREFIX) + 10);
      sprintf (bnd_var_name, "%s%d", CHKP_STRING_BOUNDS_PREFIX, string_id++);

      bnd_var = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			    get_identifier (bnd_var_name),
			    pointer_bounds_type_node);
    }

  free (bnd_var_name);

  TREE_PUBLIC (bnd_var) = 0;
  TREE_USED (bnd_var) = 1;
  TREE_READONLY (bnd_var) = 0;
  TREE_STATIC (bnd_var) = 1;
  TREE_ADDRESSABLE (bnd_var) = 0;
  DECL_ARTIFICIAL (bnd_var) = 1;
  DECL_COMMON (bnd_var) = 1;
  DECL_COMDAT (bnd_var) = 1;
  DECL_READ_P (bnd_var) = 1;
  DECL_INITIAL (bnd_var) = chkp_build_addr_expr (obj);
  /* Force output similar to constant bounds.
     See chkp_make_static_const_bounds. */
  varpool_node::get_create (bnd_var)->force_output = 1;
  /* Mark symbol as requiring bounds initialization.  */
  varpool_node::get_create (bnd_var)->need_bounds_init = 1;
  varpool_node::finalize_decl (bnd_var);

  /* Add created var to the map to use it for other references
     to obj.  */
  if (!chkp_static_var_bounds)
    chkp_static_var_bounds = new hash_map<tree, tree>;

  if (TREE_CODE (obj) == VAR_DECL)
    {
      tree name = DECL_ASSEMBLER_NAME (obj);
      chkp_static_var_bounds->put (name, bnd_var);
    }
  else
    chkp_static_var_bounds->put (obj, bnd_var);

  return bnd_var;
}

/* When var has incomplete type we cannot get size to
   compute its bounds.  In such cases we use checker
   builtin call which determines object size at runtime.  */
static tree
chkp_generate_extern_var_bounds (tree var)
{
  tree bounds, size_reloc, lb, size, max_size, cond;
  gimple_stmt_iterator gsi;
  gimple_seq seq = NULL;
  gimple *stmt;

  /* If instrumentation is not enabled for vars having
     incomplete type then just return zero bounds to avoid
     checks for this var.  */
  if (!flag_chkp_incomplete_type)
    return chkp_get_zero_bounds ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Generating bounds for extern symbol '");
      print_generic_expr (dump_file, var, 0);
      fprintf (dump_file, "'\n");
    }

  stmt = gimple_build_call (chkp_sizeof_fndecl, 1, var);

  size_reloc = create_tmp_reg (chkp_uintptr_type, CHKP_SIZE_TMP_NAME);
  gimple_call_set_lhs (stmt, size_reloc);

  gimple_seq_add_stmt (&seq, stmt);

  lb = chkp_build_addr_expr (var);
  size = make_ssa_name (chkp_get_size_tmp_var ());

  if (flag_chkp_zero_dynamic_size_as_infinite)
    {
      /* We should check that size relocation was resolved.
	 If it was not then use maximum possible size for the var.  */
      max_size = build2 (MINUS_EXPR, chkp_uintptr_type, integer_zero_node,
			 fold_convert (chkp_uintptr_type, lb));
      max_size = chkp_force_gimple_call_op (max_size, &seq);

      cond = build2 (NE_EXPR, boolean_type_node,
		     size_reloc, integer_zero_node);
      stmt = gimple_build_assign (size, COND_EXPR, cond, size_reloc, max_size);
      gimple_seq_add_stmt (&seq, stmt);
    }
  else
    {
      stmt = gimple_build_assign (size, size_reloc);
      gimple_seq_add_stmt (&seq, stmt);
    }

  gsi = gsi_start_bb (chkp_get_entry_block ());
  gsi_insert_seq_after (&gsi, seq, GSI_CONTINUE_LINKING);

  bounds = chkp_make_bounds (lb, size, &gsi, true);

  return bounds;
}

/* Return 1 if TYPE has fields with zero size or fields
   marked with chkp_variable_size attribute.  */
bool
chkp_variable_size_type (tree type)
{
  bool res = false;
  tree field;

  if (RECORD_OR_UNION_TYPE_P (type))
    for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
      {
	if (TREE_CODE (field) == FIELD_DECL)
	  res = res
	    || lookup_attribute ("bnd_variable_size", DECL_ATTRIBUTES (field))
	    || chkp_variable_size_type (TREE_TYPE (field));
      }
  else
    res = !TYPE_SIZE (type)
      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
      || tree_to_uhwi (TYPE_SIZE (type)) == 0;

  return res;
}

/* Compute and return bounds for address of DECL which is
   one of VAR_DECL, PARM_DECL, RESULT_DECL.  */
static tree
chkp_get_bounds_for_decl_addr (tree decl)
{
  tree bounds;

  gcc_assert (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL);

  bounds = chkp_get_registered_addr_bounds (decl);

  if (bounds)
    return bounds;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Building bounds for address of decl ");
      print_generic_expr (dump_file, decl, 0);
      fprintf (dump_file, "\n");
    }

  /* Use zero bounds if size is unknown and checks for
     unknown sizes are restricted.  */
  if ((!DECL_SIZE (decl)
       || (chkp_variable_size_type (TREE_TYPE (decl))
	   && (TREE_STATIC (decl)
	       || DECL_EXTERNAL (decl)
	       || TREE_PUBLIC (decl))))
      && !flag_chkp_incomplete_type)
      return chkp_get_zero_bounds ();

  if (flag_chkp_use_static_bounds
      && TREE_CODE (decl) == VAR_DECL
      && (TREE_STATIC (decl)
	      || DECL_EXTERNAL (decl)
	      || TREE_PUBLIC (decl))
      && !DECL_THREAD_LOCAL_P (decl))
    {
      tree bnd_var = chkp_make_static_bounds (decl);
      gimple_stmt_iterator gsi = gsi_start_bb (chkp_get_entry_block ());
      gimple *stmt;

      bounds = chkp_get_tmp_reg (NULL);
      stmt = gimple_build_assign (bounds, bnd_var);
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
  else if (!DECL_SIZE (decl)
      || (chkp_variable_size_type (TREE_TYPE (decl))
	  && (TREE_STATIC (decl)
	      || DECL_EXTERNAL (decl)
	      || TREE_PUBLIC (decl))))
    {
      gcc_assert (TREE_CODE (decl) == VAR_DECL);
      bounds = chkp_generate_extern_var_bounds (decl);
    }
  else
    {
      tree lb = chkp_build_addr_expr (decl);
      bounds = chkp_make_bounds (lb, DECL_SIZE_UNIT (decl), NULL, false);
    }

  return bounds;
}

/* Compute and return bounds for constant string.  */
static tree
chkp_get_bounds_for_string_cst (tree cst)
{
  tree bounds;
  tree lb;
  tree size;

  gcc_assert (TREE_CODE (cst) == STRING_CST);

  bounds = chkp_get_registered_bounds (cst);

  if (bounds)
    return bounds;

  if ((flag_chkp_use_static_bounds && flag_chkp_use_static_const_bounds)
      || flag_chkp_use_static_const_bounds > 0)
    {
      tree bnd_var = chkp_make_static_bounds (cst);
      gimple_stmt_iterator gsi = gsi_start_bb (chkp_get_entry_block ());
      gimple *stmt;

      bounds = chkp_get_tmp_reg (NULL);
      stmt = gimple_build_assign (bounds, bnd_var);
      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
    }
  else
    {
      lb = chkp_build_addr_expr (cst);
      size = build_int_cst (chkp_uintptr_type, TREE_STRING_LENGTH (cst));
      bounds = chkp_make_bounds (lb, size, NULL, false);
    }

  bounds = chkp_maybe_copy_and_register_bounds (cst, bounds);

  return bounds;
}

/* Generate code to instersect bounds BOUNDS1 and BOUNDS2 and
   return the result.  if ITER is not NULL then Code is inserted
   before position pointed by ITER.  Otherwise code is added to
   entry block.  */
static tree
chkp_intersect_bounds (tree bounds1, tree bounds2, gimple_stmt_iterator *iter)
{
  if (!bounds1 || bounds1 == chkp_get_zero_bounds ())
    return bounds2 ? bounds2 : bounds1;
  else if (!bounds2 || bounds2 == chkp_get_zero_bounds ())
    return bounds1;
  else
    {
      gimple_seq seq;
      gimple *stmt;
      tree bounds;

      seq = NULL;

      stmt = gimple_build_call (chkp_intersect_fndecl, 2, bounds1, bounds2);
      chkp_mark_stmt (stmt);

      bounds = chkp_get_tmp_reg (stmt);
      gimple_call_set_lhs (stmt, bounds);

      gimple_seq_add_stmt (&seq, stmt);

      /* We are probably doing narrowing for constant expression.
	 In such case iter may be undefined.  */
      if (!iter)
	{
	  gimple_stmt_iterator gsi = gsi_last_bb (chkp_get_entry_block ());
	  iter = &gsi;
	  gsi_insert_seq_after (iter, seq, GSI_SAME_STMT);
	}
      else
	gsi_insert_seq_before (iter, seq, GSI_SAME_STMT);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Bounds intersection: ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_VOPS|TDF_MEMSYMS);
	  fprintf (dump_file, "  inserted before statement: ");
	  print_gimple_stmt (dump_file, gsi_stmt (*iter), 0,
			     TDF_VOPS|TDF_MEMSYMS);
	}

      return bounds;
    }
}

/* Return 1 if we are allowed to narrow bounds for addressed FIELD
   and 0 othersize.  */
static bool
chkp_may_narrow_to_field (tree field)
{
  return DECL_SIZE (field) && TREE_CODE (DECL_SIZE (field)) == INTEGER_CST
    && tree_to_uhwi (DECL_SIZE (field)) != 0
    && (!DECL_FIELD_OFFSET (field)
	|| TREE_CODE (DECL_FIELD_OFFSET (field)) == INTEGER_CST)
    && (!DECL_FIELD_BIT_OFFSET (field)
	|| TREE_CODE (DECL_FIELD_BIT_OFFSET (field)) == INTEGER_CST)
    && !lookup_attribute ("bnd_variable_size", DECL_ATTRIBUTES (field))
    && !chkp_variable_size_type (TREE_TYPE (field));
}

/* Return 1 if bounds for FIELD should be narrowed to
   field's own size.  */
static bool
chkp_narrow_bounds_for_field (tree field)
{
  HOST_WIDE_INT offs;
  HOST_WIDE_INT bit_offs;

  if (!chkp_may_narrow_to_field (field))
    return false;

  /* Accesse to compiler generated fields should not cause
     bounds narrowing.  */
  if (DECL_ARTIFICIAL (field))
    return false;

  offs = tree_to_uhwi (DECL_FIELD_OFFSET (field));
  bit_offs = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field));

  return (flag_chkp_narrow_bounds
	  && (flag_chkp_first_field_has_own_bounds
	      || offs
	      || bit_offs));
}

/* Perform narrowing for BOUNDS using bounds computed for field
   access COMPONENT.  ITER meaning is the same as for
   chkp_intersect_bounds.  */
static tree
chkp_narrow_bounds_to_field (tree bounds, tree component,
			    gimple_stmt_iterator *iter)
{
  tree field = TREE_OPERAND (component, 1);
  tree size = DECL_SIZE_UNIT (field);
  tree field_ptr = chkp_build_addr_expr (component);
  tree field_bounds;

  field_bounds = chkp_make_bounds (field_ptr, size, iter, false);

  return chkp_intersect_bounds (field_bounds, bounds, iter);
}

/* Parse field or array access NODE.

   PTR ouput parameter holds a pointer to the outermost
   object.

   BITFIELD output parameter is set to 1 if bitfield is
   accessed and to 0 otherwise.  If it is 1 then ELT holds
   outer component for accessed bit field.

   SAFE outer parameter is set to 1 if access is safe and
   checks are not required.

   BOUNDS outer parameter holds bounds to be used to check
   access (may be NULL).

   If INNERMOST_BOUNDS is 1 then try to narrow bounds to the
   innermost accessed component.  */
static void
chkp_parse_array_and_component_ref (tree node, tree *ptr,
				    tree *elt, bool *safe,
				    bool *bitfield,
				    tree *bounds,
				    gimple_stmt_iterator *iter,
				    bool innermost_bounds)
{
  tree comp_to_narrow = NULL_TREE;
  tree last_comp = NULL_TREE;
  bool array_ref_found = false;
  tree *nodes;
  tree var;
  int len;
  int i;

  /* Compute tree height for expression.  */
  var = node;
  len = 1;
  while (TREE_CODE (var) == COMPONENT_REF
	 || TREE_CODE (var) == ARRAY_REF
	 || TREE_CODE (var) == VIEW_CONVERT_EXPR)
    {
      var = TREE_OPERAND (var, 0);
      len++;
    }

  gcc_assert (len > 1);

  /* It is more convenient for us to scan left-to-right,
     so walk tree again and put all node to nodes vector
     in reversed order.  */
  nodes = XALLOCAVEC (tree, len);
  nodes[len - 1] = node;
  for (i = len - 2; i >= 0; i--)
    nodes[i] = TREE_OPERAND (nodes[i + 1], 0);

  if (bounds)
    *bounds = NULL;
  *safe = true;
  *bitfield = (TREE_CODE (node) == COMPONENT_REF
	       && DECL_BIT_FIELD_TYPE (TREE_OPERAND (node, 1)));
  /* To get bitfield address we will need outer elemnt.  */
  if (*bitfield)
    *elt = nodes[len - 2];
  else
    *elt = NULL_TREE;

  /* If we have indirection in expression then compute
     outermost structure bounds.  Computed bounds may be
     narrowed later.  */
  if (TREE_CODE (nodes[0]) == MEM_REF || INDIRECT_REF_P (nodes[0]))
    {
      *safe = false;
      *ptr = TREE_OPERAND (nodes[0], 0);
      if (bounds)
	*bounds = chkp_find_bounds (*ptr, iter);
    }
  else
    {
      gcc_assert (TREE_CODE (var) == VAR_DECL
		  || TREE_CODE (var) == PARM_DECL
		  || TREE_CODE (var) == RESULT_DECL
		  || TREE_CODE (var) == STRING_CST
		  || TREE_CODE (var) == SSA_NAME);

      *ptr = chkp_build_addr_expr (var);
    }

  /* In this loop we are trying to find a field access
     requiring narrowing.  There are two simple rules
     for search:
     1.  Leftmost array_ref is chosen if any.
     2.  Rightmost suitable component_ref is chosen if innermost
     bounds are required and no array_ref exists.  */
  for (i = 1; i < len; i++)
    {
      var = nodes[i];

      if (TREE_CODE (var) == ARRAY_REF)
	{
	  *safe = false;
	  array_ref_found = true;
	  if (flag_chkp_narrow_bounds
	      && !flag_chkp_narrow_to_innermost_arrray
	      && (!last_comp
		  || chkp_may_narrow_to_field (TREE_OPERAND (last_comp, 1))))
	    {
	      comp_to_narrow = last_comp;
	      break;
	    }
	}
      else if (TREE_CODE (var) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (var, 1);

	  if (innermost_bounds
	      && !array_ref_found
	      && chkp_narrow_bounds_for_field (field))
	    comp_to_narrow = var;
	  last_comp = var;

	  if (flag_chkp_narrow_bounds
	      && flag_chkp_narrow_to_innermost_arrray
	      && TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE)
	    {
	      if (bounds)
		*bounds = chkp_narrow_bounds_to_field (*bounds, var, iter);
	      comp_to_narrow = NULL;
	    }
	}
      else if (TREE_CODE (var) == VIEW_CONVERT_EXPR)
	/* Nothing to do for it.  */
	;
      else
	gcc_unreachable ();
    }

  if (comp_to_narrow && DECL_SIZE (TREE_OPERAND (comp_to_narrow, 1)) && bounds)
    *bounds = chkp_narrow_bounds_to_field (*bounds, comp_to_narrow, iter);

  if (innermost_bounds && bounds && !*bounds)
    *bounds = chkp_find_bounds (*ptr, iter);
}

/* Compute and return bounds for address of OBJ.  */
static tree
chkp_make_addressed_object_bounds (tree obj, gimple_stmt_iterator *iter)
{
  tree bounds = chkp_get_registered_addr_bounds (obj);

  if (bounds)
    return bounds;

  switch (TREE_CODE (obj))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      bounds = chkp_get_bounds_for_decl_addr (obj);
      break;

    case STRING_CST:
      bounds = chkp_get_bounds_for_string_cst (obj);
      break;

    case ARRAY_REF:
    case COMPONENT_REF:
      {
	tree elt;
	tree ptr;
	bool safe;
	bool bitfield;

	chkp_parse_array_and_component_ref (obj, &ptr, &elt, &safe,
					    &bitfield, &bounds, iter, true);

	gcc_assert (bounds);
      }
      break;

    case FUNCTION_DECL:
    case LABEL_DECL:
      bounds = chkp_get_zero_bounds ();
      break;

    case MEM_REF:
      bounds = chkp_find_bounds (TREE_OPERAND (obj, 0), iter);
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      bounds = chkp_make_addressed_object_bounds (TREE_OPERAND (obj, 0), iter);
      break;

    default:
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "chkp_make_addressed_object_bounds: "
		   "unexpected object of type %s\n",
		   get_tree_code_name (TREE_CODE (obj)));
	  print_node (dump_file, "", obj, 0);
	}
      internal_error ("chkp_make_addressed_object_bounds: "
		      "Unexpected tree code %s",
		      get_tree_code_name (TREE_CODE (obj)));
    }

  chkp_register_addr_bounds (obj, bounds);

  return bounds;
}

/* Compute bounds for pointer PTR loaded from PTR_SRC.  Generate statements
   to compute bounds if required.  Computed bounds should be available at
   position pointed by ITER.

   If PTR_SRC is NULL_TREE then pointer definition is identified.

   If PTR_SRC is not NULL_TREE then ITER points to statements which loads
   PTR.  If PTR is a any memory reference then ITER points to a statement
   after which bndldx will be inserterd.  In both cases ITER will be updated
   to point to the inserted bndldx statement.  */

static tree
chkp_find_bounds_1 (tree ptr, tree ptr_src, gimple_stmt_iterator *iter)
{
  tree addr = NULL_TREE;
  tree bounds = NULL_TREE;

  if (!ptr_src)
    ptr_src = ptr;

  bounds = chkp_get_registered_bounds (ptr_src);

  if (bounds)
    return bounds;

  switch (TREE_CODE (ptr_src))
    {
    case MEM_REF:
    case VAR_DECL:
      if (BOUNDED_P (ptr_src))
	if (TREE_CODE (ptr) == VAR_DECL && DECL_REGISTER (ptr))
	  bounds = chkp_get_zero_bounds ();
	else
	  {
	    addr = chkp_build_addr_expr (ptr_src);
	    bounds = chkp_build_bndldx (addr, ptr, iter);
	  }
      else
	bounds = chkp_get_nonpointer_load_bounds ();
      break;

    case ARRAY_REF:
    case COMPONENT_REF:
      addr = get_base_address (ptr_src);
      if (DECL_P (addr)
	  || TREE_CODE (addr) == MEM_REF
	  || TREE_CODE (addr) == TARGET_MEM_REF)
	{
	  if (BOUNDED_P (ptr_src))
	    if (TREE_CODE (ptr) == VAR_DECL && DECL_REGISTER (ptr))
	      bounds = chkp_get_zero_bounds ();
	    else
	      {
		addr = chkp_build_addr_expr (ptr_src);
		bounds = chkp_build_bndldx (addr, ptr, iter);
	      }
	  else
	    bounds = chkp_get_nonpointer_load_bounds ();
	}
      else
	{
	  gcc_assert (TREE_CODE (addr) == SSA_NAME);
	  bounds = chkp_find_bounds (addr, iter);
	}
      break;

    case PARM_DECL:
      gcc_unreachable ();
      bounds = chkp_get_bound_for_parm (ptr_src);
      break;

    case TARGET_MEM_REF:
      addr = chkp_build_addr_expr (ptr_src);
      bounds = chkp_build_bndldx (addr, ptr, iter);
      break;

    case SSA_NAME:
      bounds = chkp_get_registered_bounds (ptr_src);
      if (!bounds)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (ptr_src);
	  gphi_iterator phi_iter;

	  bounds = chkp_get_bounds_by_definition (ptr_src, def_stmt, &phi_iter);

	  gcc_assert (bounds);

	  if (gphi *def_phi = dyn_cast <gphi *> (def_stmt))
	    {
	      unsigned i;

	      for (i = 0; i < gimple_phi_num_args (def_phi); i++)
		{
		  tree arg = gimple_phi_arg_def (def_phi, i);
		  tree arg_bnd;
		  gphi *phi_bnd;

		  arg_bnd = chkp_find_bounds (arg, NULL);

		  /* chkp_get_bounds_by_definition created new phi
		     statement and phi_iter points to it.

		     Previous call to chkp_find_bounds could create
		     new basic block and therefore change phi statement
		     phi_iter points to.  */
		  phi_bnd = phi_iter.phi ();

		  add_phi_arg (phi_bnd, arg_bnd,
			       gimple_phi_arg_edge (def_phi, i),
			       UNKNOWN_LOCATION);
		}

	      /* If all bound phi nodes have their arg computed
		 then we may finish its computation.  See
		 chkp_finish_incomplete_bounds for more details.  */
	      if (chkp_may_finish_incomplete_bounds ())
		chkp_finish_incomplete_bounds ();
	    }

	  gcc_assert (bounds == chkp_get_registered_bounds (ptr_src)
		      || chkp_incomplete_bounds (bounds));
	}
      break;

    case ADDR_EXPR:
    case WITH_SIZE_EXPR:
      bounds = chkp_make_addressed_object_bounds (TREE_OPERAND (ptr_src, 0), iter);
      break;

    case INTEGER_CST:
      if (integer_zerop (ptr_src))
	bounds = chkp_get_none_bounds ();
      else
	bounds = chkp_get_invalid_op_bounds ();
      break;

    default:
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "chkp_find_bounds: unexpected ptr of type %s\n",
		   get_tree_code_name (TREE_CODE (ptr_src)));
	  print_node (dump_file, "", ptr_src, 0);
	}
      internal_error ("chkp_find_bounds: Unexpected tree code %s",
		      get_tree_code_name (TREE_CODE (ptr_src)));
    }

  if (!bounds)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (stderr, "chkp_find_bounds: cannot find bounds for pointer\n");
	  print_node (dump_file, "", ptr_src, 0);
	}
      internal_error ("chkp_find_bounds: Cannot find bounds for pointer");
    }

  return bounds;
}

/* Normal case for bounds search without forced narrowing.  */
static tree
chkp_find_bounds (tree ptr, gimple_stmt_iterator *iter)
{
  return chkp_find_bounds_1 (ptr, NULL_TREE, iter);
}

/* Search bounds for pointer PTR loaded from PTR_SRC
   by statement *ITER points to.  */
static tree
chkp_find_bounds_loaded (tree ptr, tree ptr_src, gimple_stmt_iterator *iter)
{
  return chkp_find_bounds_1 (ptr, ptr_src, iter);
}

/* Helper function which checks type of RHS and finds all pointers in
   it.  For each found pointer we build it's accesses in LHS and RHS
   objects and then call HANDLER for them.  Function is used to copy
   or initilize bounds for copied object.  */
static void
chkp_walk_pointer_assignments (tree lhs, tree rhs, void *arg,
			       assign_handler handler)
{
  tree type = TREE_TYPE (lhs);

  /* We have nothing to do with clobbers.  */
  if (TREE_CLOBBER_P (rhs))
    return;

  if (BOUNDED_TYPE_P (type))
    handler (lhs, rhs, arg);
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      if (TREE_CODE (rhs) == CONSTRUCTOR)
	{
	  unsigned HOST_WIDE_INT cnt;
	  tree val;

	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (rhs), cnt, field, val)
	    {
	      if (chkp_type_has_pointer (TREE_TYPE (field)))
		{
		  tree lhs_field = chkp_build_component_ref (lhs, field);
		  chkp_walk_pointer_assignments (lhs_field, val, arg, handler);
		}
	    }
	}
      else
	for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	  if (TREE_CODE (field) == FIELD_DECL
	      && chkp_type_has_pointer (TREE_TYPE (field)))
	    {
	      tree rhs_field = chkp_build_component_ref (rhs, field);
	      tree lhs_field = chkp_build_component_ref (lhs, field);
	      chkp_walk_pointer_assignments (lhs_field, rhs_field, arg, handler);
	    }
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      unsigned HOST_WIDE_INT cur = 0;
      tree maxval = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
      tree etype = TREE_TYPE (type);
      tree esize = TYPE_SIZE (etype);

      if (TREE_CODE (rhs) == CONSTRUCTOR)
	{
	  unsigned HOST_WIDE_INT cnt;
	  tree purp, val, lhs_elem;

	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (rhs), cnt, purp, val)
	    {
	      if (purp && TREE_CODE (purp) == RANGE_EXPR)
		{
		  tree lo_index = TREE_OPERAND (purp, 0);
		  tree hi_index = TREE_OPERAND (purp, 1);

		  for (cur = (unsigned)tree_to_uhwi (lo_index);
		       cur <= (unsigned)tree_to_uhwi (hi_index);
		       cur++)
		    {
		      lhs_elem = chkp_build_array_ref (lhs, etype, esize, cur);
		      chkp_walk_pointer_assignments (lhs_elem, val, arg, handler);
		    }
		}
	      else
		{
		  if (purp)
		    {
		      gcc_assert (TREE_CODE (purp) == INTEGER_CST);
		      cur = tree_to_uhwi (purp);
		    }

		  lhs_elem = chkp_build_array_ref (lhs, etype, esize, cur++);

		  chkp_walk_pointer_assignments (lhs_elem, val, arg, handler);
		}
	    }
	}
      /* Copy array only when size is known.  */
      else if (maxval && !integer_minus_onep (maxval))
	for (cur = 0; cur <= TREE_INT_CST_LOW (maxval); cur++)
	  {
	    tree lhs_elem = chkp_build_array_ref (lhs, etype, esize, cur);
	    tree rhs_elem = chkp_build_array_ref (rhs, etype, esize, cur);
	    chkp_walk_pointer_assignments (lhs_elem, rhs_elem, arg, handler);
	  }
    }
  else
    internal_error("chkp_walk_pointer_assignments: unexpected RHS type: %s",
		   get_tree_code_name (TREE_CODE (type)));
}

/* Add code to copy bounds for assignment of RHS to LHS.
   ARG is an iterator pointing ne code position.  */
static void
chkp_copy_bounds_for_elem (tree lhs, tree rhs, void *arg)
{
  gimple_stmt_iterator *iter = (gimple_stmt_iterator *)arg;
  tree bounds = chkp_find_bounds (rhs, iter);
  tree addr = chkp_build_addr_expr(lhs);

  chkp_build_bndstx (addr, rhs, bounds, iter);
}

/* Emit static bound initilizers and size vars.  */
void
chkp_finish_file (void)
{
  struct varpool_node *node;
  struct chkp_ctor_stmt_list stmts;

  if (seen_error ())
    return;

  /* Iterate through varpool and generate bounds initialization
     constructors for all statically initialized pointers.  */
  stmts.avail = MAX_STMTS_IN_STATIC_CHKP_CTOR;
  stmts.stmts = NULL;
  FOR_EACH_VARIABLE (node)
    /* Check that var is actually emitted and we need and may initialize
       its bounds.  */
    if (node->need_bounds_init
	&& !POINTER_BOUNDS_P (node->decl)
	&& DECL_RTL (node->decl)
	&& MEM_P (DECL_RTL (node->decl))
	&& TREE_ASM_WRITTEN (node->decl))
      {
	chkp_walk_pointer_assignments (node->decl,
				       DECL_INITIAL (node->decl),
				       &stmts,
				       chkp_add_modification_to_stmt_list);

	if (stmts.avail <= 0)
	  {
	    cgraph_build_static_cdtor ('P', stmts.stmts,
				       MAX_RESERVED_INIT_PRIORITY + 3);
	    stmts.avail = MAX_STMTS_IN_STATIC_CHKP_CTOR;
	    stmts.stmts = NULL;
	  }
      }

  if (stmts.stmts)
    cgraph_build_static_cdtor ('P', stmts.stmts,
			       MAX_RESERVED_INIT_PRIORITY + 3);

  /* Iterate through varpool and generate bounds initialization
     constructors for all static bounds vars.  */
  stmts.avail = MAX_STMTS_IN_STATIC_CHKP_CTOR;
  stmts.stmts = NULL;
  FOR_EACH_VARIABLE (node)
    if (node->need_bounds_init
	&& POINTER_BOUNDS_P (node->decl)
	&& TREE_ASM_WRITTEN (node->decl))
      {
	tree bnd = node->decl;
	tree var;

	gcc_assert (DECL_INITIAL (bnd)
		    && TREE_CODE (DECL_INITIAL (bnd)) == ADDR_EXPR);

	var = TREE_OPERAND (DECL_INITIAL (bnd), 0);
	chkp_output_static_bounds (bnd, var, &stmts);
      }

  if (stmts.stmts)
    cgraph_build_static_cdtor ('B', stmts.stmts,
			       MAX_RESERVED_INIT_PRIORITY + 2);

  delete chkp_static_var_bounds;
  delete chkp_bounds_map;
}

/* An instrumentation function which is called for each statement
   having memory access we want to instrument.  It inserts check
   code and bounds copy code.

   ITER points to statement to instrument.

   NODE holds memory access in statement to check.

   LOC holds the location information for statement.

   DIRFLAGS determines whether access is read or write.

   ACCESS_OFFS should be added to address used in NODE
   before check.

   ACCESS_SIZE holds size of checked access.

   SAFE indicates if NODE access is safe and should not be
   checked.  */
static void
chkp_process_stmt (gimple_stmt_iterator *iter, tree node,
		   location_t loc, tree dirflag,
		   tree access_offs, tree access_size,
		   bool safe)
{
  tree node_type = TREE_TYPE (node);
  tree size = access_size ? access_size : TYPE_SIZE_UNIT (node_type);
  tree addr_first = NULL_TREE; /* address of the first accessed byte */
  tree addr_last = NULL_TREE; /* address of the last accessed byte */
  tree ptr = NULL_TREE; /* a pointer used for dereference */
  tree bounds = NULL_TREE;

  /* We do not need instrumentation for clobbers.  */
  if (dirflag == integer_one_node
      && gimple_code (gsi_stmt (*iter)) == GIMPLE_ASSIGN
      && TREE_CLOBBER_P (gimple_assign_rhs1 (gsi_stmt (*iter))))
    return;

  switch (TREE_CODE (node))
    {
    case ARRAY_REF:
    case COMPONENT_REF:
      {
	bool bitfield;
	tree elt;

	if (safe)
	  {
	    /* We are not going to generate any checks, so do not
	       generate bounds as well.  */
	    addr_first = chkp_build_addr_expr (node);
	    break;
	  }

	chkp_parse_array_and_component_ref (node, &ptr, &elt, &safe,
					    &bitfield, &bounds, iter, false);

	/* Break if there is no dereference and operation is safe.  */

	if (bitfield)
          {
            tree field = TREE_OPERAND (node, 1);

            if (TREE_CODE (DECL_SIZE_UNIT (field)) == INTEGER_CST)
              size = DECL_SIZE_UNIT (field);

	    if (elt)
	      elt = chkp_build_addr_expr (elt);
            addr_first = fold_convert_loc (loc, ptr_type_node, elt ? elt : ptr);
            addr_first = fold_build_pointer_plus_loc (loc,
						      addr_first,
						      byte_position (field));
          }
        else
          addr_first = chkp_build_addr_expr (node);
      }
      break;

    case INDIRECT_REF:
      ptr = TREE_OPERAND (node, 0);
      addr_first = ptr;
      break;

    case MEM_REF:
      ptr = TREE_OPERAND (node, 0);
      addr_first = chkp_build_addr_expr (node);
      break;

    case TARGET_MEM_REF:
      ptr = TMR_BASE (node);
      addr_first = chkp_build_addr_expr (node);
      break;

    case ARRAY_RANGE_REF:
      printf("ARRAY_RANGE_REF\n");
      debug_gimple_stmt(gsi_stmt(*iter));
      debug_tree(node);
      gcc_unreachable ();
      break;

    case BIT_FIELD_REF:
      {
	tree offs, rem, bpu;

	gcc_assert (!access_offs);
	gcc_assert (!access_size);

	bpu = fold_convert (size_type_node, bitsize_int (BITS_PER_UNIT));
	offs = fold_convert (size_type_node, TREE_OPERAND (node, 2));
	rem = size_binop_loc (loc, TRUNC_MOD_EXPR, offs, bpu);
	offs = size_binop_loc (loc, TRUNC_DIV_EXPR, offs, bpu);

	size = fold_convert (size_type_node, TREE_OPERAND (node, 1));
        size = size_binop_loc (loc, PLUS_EXPR, size, rem);
        size = size_binop_loc (loc, CEIL_DIV_EXPR, size, bpu);
        size = fold_convert (size_type_node, size);

	chkp_process_stmt (iter, TREE_OPERAND (node, 0), loc,
			 dirflag, offs, size, safe);
	return;
      }
      break;

    case VAR_DECL:
    case RESULT_DECL:
    case PARM_DECL:
      if (dirflag != integer_one_node
	  || DECL_REGISTER (node))
	return;

      safe = true;
      addr_first = chkp_build_addr_expr (node);
      break;

    default:
      return;
    }

  /* If addr_last was not computed then use (addr_first + size - 1)
     expression to compute it.  */
  if (!addr_last)
    {
      addr_last = fold_build_pointer_plus_loc (loc, addr_first, size);
      addr_last = fold_build_pointer_plus_hwi_loc (loc, addr_last, -1);
    }

  /* Shift both first_addr and last_addr by access_offs if specified.  */
  if (access_offs)
    {
      addr_first = fold_build_pointer_plus_loc (loc, addr_first, access_offs);
      addr_last = fold_build_pointer_plus_loc (loc, addr_last, access_offs);
    }

  /* Generate bndcl/bndcu checks if memory access is not safe.  */
  if (!safe)
    {
      gimple_stmt_iterator stmt_iter = *iter;

      if (!bounds)
	bounds = chkp_find_bounds (ptr, iter);

      chkp_check_mem_access (addr_first, addr_last, bounds,
			     stmt_iter, loc, dirflag);
    }

  /* We need to store bounds in case pointer is stored.  */
  if (dirflag == integer_one_node
      && chkp_type_has_pointer (node_type)
      && flag_chkp_store_bounds)
    {
      gimple *stmt = gsi_stmt (*iter);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);

      if (get_gimple_rhs_class (rhs_code) == GIMPLE_SINGLE_RHS)
	chkp_walk_pointer_assignments (node, rhs1, iter,
				       chkp_copy_bounds_for_elem);
      else
	{
	  bounds = chkp_compute_bounds_for_assignment (NULL_TREE, stmt);
	  chkp_build_bndstx (addr_first, rhs1, bounds, iter);
	}
    }
}

/* Add code to copy bounds for all pointers copied
   in ASSIGN created during inline of EDGE.  */
void
chkp_copy_bounds_for_assign (gimple *assign, struct cgraph_edge *edge)
{
  tree lhs = gimple_assign_lhs (assign);
  tree rhs = gimple_assign_rhs1 (assign);
  gimple_stmt_iterator iter = gsi_for_stmt (assign);

  if (!flag_chkp_store_bounds)
    return;

  chkp_walk_pointer_assignments (lhs, rhs, &iter, chkp_copy_bounds_for_elem);

  /* We should create edges for all created calls to bndldx and bndstx.  */
  while (gsi_stmt (iter) != assign)
    {
      gimple *stmt = gsi_stmt (iter);
      if (gimple_code (stmt) == GIMPLE_CALL)
	{
	  tree fndecl = gimple_call_fndecl (stmt);
	  struct cgraph_node *callee = cgraph_node::get_create (fndecl);
	  struct cgraph_edge *new_edge;

	  gcc_assert (fndecl == chkp_bndstx_fndecl
		      || fndecl == chkp_bndldx_fndecl
		      || fndecl == chkp_ret_bnd_fndecl);

	  new_edge = edge->caller->create_edge (callee,
						as_a <gcall *> (stmt),
						edge->count,
						edge->frequency);
	  new_edge->frequency = compute_call_stmt_bb_frequency
	    (edge->caller->decl, gimple_bb (stmt));
	}
      gsi_prev (&iter);
    }
}

/* Some code transformation made during instrumentation pass
   may put code into inconsistent state.  Here we find and fix
   such flaws.  */
void
chkp_fix_cfg ()
{
  basic_block bb;
  gimple_stmt_iterator i;

  /* We could insert some code right after stmt which ends bb.
     We wanted to put this code on fallthru edge but did not
     add new edges from the beginning because it may cause new
     phi node creation which may be incorrect due to incomplete
     bound phi nodes.  */
  FOR_ALL_BB_FN (bb, cfun)
    for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
      {
	gimple *stmt = gsi_stmt (i);
	gimple_stmt_iterator next = i;

	gsi_next (&next);

	if (stmt_ends_bb_p (stmt)
	    && !gsi_end_p (next))
	  {
	    edge fall = find_fallthru_edge (bb->succs);
	    basic_block dest = NULL;
	    int flags = 0;

	    gcc_assert (fall);

	    /* We cannot split abnormal edge.  Therefore we
	       store its params, make it regular and then
	       rebuild abnormal edge after split.  */
	    if (fall->flags & EDGE_ABNORMAL)
	      {
		flags = fall->flags & ~EDGE_FALLTHRU;
		dest = fall->dest;

		fall->flags &= ~EDGE_COMPLEX;
	      }

	    while (!gsi_end_p (next))
	      {
		gimple *next_stmt = gsi_stmt (next);
		gsi_remove (&next, false);
		gsi_insert_on_edge (fall, next_stmt);
	      }

	    gsi_commit_edge_inserts ();

	    /* Re-create abnormal edge.  */
	    if (dest)
	      make_edge (bb, dest, flags);
	  }
      }
}

/* Walker callback for chkp_replace_function_pointers.  Replaces
   function pointer in the specified operand with pointer to the
   instrumented function version.  */
static tree
chkp_replace_function_pointer (tree *op, int *walk_subtrees,
			       void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*op) == FUNCTION_DECL
      && chkp_instrumentable_p (*op)
      && (DECL_BUILT_IN_CLASS (*op) == NOT_BUILT_IN
	  /* For builtins we replace pointers only for selected
	     function and functions having definitions.  */
	  || (DECL_BUILT_IN_CLASS (*op) == BUILT_IN_NORMAL
	      && (chkp_instrument_normal_builtin (*op)
		  || gimple_has_body_p (*op)))))
    {
      struct cgraph_node *node = cgraph_node::get_create (*op);
      struct cgraph_node *clone = NULL;

      if (!node->instrumentation_clone)
	clone = chkp_maybe_create_clone (*op);

      if (clone)
	*op = clone->decl;
      *walk_subtrees = 0;
    }

  return NULL;
}

/* This function searches for function pointers in statement
   pointed by GSI and replaces them with pointers to instrumented
   function versions.  */
static void
chkp_replace_function_pointers (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  /* For calls we want to walk call args only.  */
  if (gimple_code (stmt) == GIMPLE_CALL)
    {
      unsigned i;
      for (i = 0; i < gimple_call_num_args (stmt); i++)
	walk_tree (gimple_call_arg_ptr (stmt, i),
		   chkp_replace_function_pointer, NULL, NULL);
    }
  else
    walk_gimple_stmt (gsi, NULL, chkp_replace_function_pointer, NULL);
}

/* This function instruments all statements working with memory,
   calls and rets.

   It also removes excess statements from static initializers.  */
static void
chkp_instrument_function (void)
{
  basic_block bb, next;
  gimple_stmt_iterator i;
  enum gimple_rhs_class grhs_class;
  bool safe = lookup_attribute ("chkp ctor", DECL_ATTRIBUTES (cfun->decl));

  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
  do
    {
      next = bb->next_bb;
      for (i = gsi_start_bb (bb); !gsi_end_p (i); )
        {
	  gimple *s = gsi_stmt (i);

	  /* Skip statement marked to not be instrumented.  */
	  if (chkp_marked_stmt_p (s))
	    {
	      gsi_next (&i);
	      continue;
	    }

	  chkp_replace_function_pointers (&i);

          switch (gimple_code (s))
            {
            case GIMPLE_ASSIGN:
	      chkp_process_stmt (&i, gimple_assign_lhs (s),
				 gimple_location (s), integer_one_node,
				 NULL_TREE, NULL_TREE, safe);
	      chkp_process_stmt (&i, gimple_assign_rhs1 (s),
				 gimple_location (s), integer_zero_node,
				 NULL_TREE, NULL_TREE, safe);
	      grhs_class = get_gimple_rhs_class (gimple_assign_rhs_code (s));
	      if (grhs_class == GIMPLE_BINARY_RHS)
		chkp_process_stmt (&i, gimple_assign_rhs2 (s),
				   gimple_location (s), integer_zero_node,
				   NULL_TREE, NULL_TREE, safe);
              break;

            case GIMPLE_RETURN:
	      {
		greturn *r = as_a <greturn *> (s);
		if (gimple_return_retval (r) != NULL_TREE)
		  {
		    chkp_process_stmt (&i, gimple_return_retval (r),
				       gimple_location (r),
				       integer_zero_node,
				       NULL_TREE, NULL_TREE, safe);

		    /* Additionally we need to add bounds
		       to return statement.  */
		    chkp_add_bounds_to_ret_stmt (&i);
		  }
	      }
	      break;

	    case GIMPLE_CALL:
	      chkp_add_bounds_to_call_stmt (&i);
	      break;

            default:
              ;
            }

	  gsi_next (&i);

	  /* We do not need any actual pointer stores in checker
	     static initializer.  */
	  if (lookup_attribute ("chkp ctor", DECL_ATTRIBUTES (cfun->decl))
	      && gimple_code (s) == GIMPLE_ASSIGN
	      && gimple_store_p (s))
	    {
	      gimple_stmt_iterator del_iter = gsi_for_stmt (s);
	      gsi_remove (&del_iter, true);
	      unlink_stmt_vdef (s);
	      release_defs(s);
	    }
        }
      bb = next;
    }
  while (bb);

  /* Some input params may have bounds and be address taken.  In this case
     we should store incoming bounds into bounds table.  */
  tree arg;
  if (flag_chkp_store_bounds)
    for (arg = DECL_ARGUMENTS (cfun->decl); arg; arg = DECL_CHAIN (arg))
      if (TREE_ADDRESSABLE (arg))
	{
	  if (BOUNDED_P (arg))
	    {
	      tree bounds = chkp_get_next_bounds_parm (arg);
	      tree def_ptr = ssa_default_def (cfun, arg);
	      gimple_stmt_iterator iter
		= gsi_start_bb (chkp_get_entry_block ());
	      chkp_build_bndstx (chkp_build_addr_expr (arg),
				 def_ptr ? def_ptr : arg,
				 bounds, &iter);

	      /* Skip bounds arg.  */
	      arg = TREE_CHAIN (arg);
	    }
	  else if (chkp_type_has_pointer (TREE_TYPE (arg)))
	    {
	      tree orig_arg = arg;
	      bitmap slots = BITMAP_ALLOC (NULL);
	      gimple_stmt_iterator iter
		= gsi_start_bb (chkp_get_entry_block ());
	      bitmap_iterator bi;
	      unsigned bnd_no;

	      chkp_find_bound_slots (TREE_TYPE (arg), slots);

	      EXECUTE_IF_SET_IN_BITMAP (slots, 0, bnd_no, bi)
		{
		  tree bounds = chkp_get_next_bounds_parm (arg);
		  HOST_WIDE_INT offs = bnd_no * POINTER_SIZE / BITS_PER_UNIT;
		  tree addr = chkp_build_addr_expr (orig_arg);
		  tree ptr = build2 (MEM_REF, ptr_type_node, addr,
				     build_int_cst (ptr_type_node, offs));
		  chkp_build_bndstx (chkp_build_addr_expr (ptr), ptr,
				     bounds, &iter);

		  arg = DECL_CHAIN (arg);
		}
	      BITMAP_FREE (slots);
	    }
	}
}

/* Find init/null/copy_ptr_bounds calls and replace them
   with assignments.  It should allow better code
   optimization.  */

static void
chkp_remove_useless_builtins ()
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        {
	  gimple *stmt = gsi_stmt (gsi);
	  tree fndecl;
	  enum built_in_function fcode;

	  /* Find builtins returning first arg and replace
	     them with assignments.  */
	  if (gimple_code (stmt) == GIMPLE_CALL
	      && (fndecl = gimple_call_fndecl (stmt))
	      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	      && (fcode = DECL_FUNCTION_CODE (fndecl))
	      && (fcode == BUILT_IN_CHKP_INIT_PTR_BOUNDS
		  || fcode == BUILT_IN_CHKP_NULL_PTR_BOUNDS
		  || fcode == BUILT_IN_CHKP_COPY_PTR_BOUNDS
		  || fcode == BUILT_IN_CHKP_SET_PTR_BOUNDS))
	    {
	      tree res = gimple_call_arg (stmt, 0);
	      update_call_from_tree (&gsi, res);
	      stmt = gsi_stmt (gsi);
	      update_stmt (stmt);
	    }
        }
    }
}

/* Initialize pass.  */
static void
chkp_init (void)
{
  basic_block bb;
  gimple_stmt_iterator i;

  in_chkp_pass = true;

  for (bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb; bb; bb = bb->next_bb)
    for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
      chkp_unmark_stmt (gsi_stmt (i));

  chkp_invalid_bounds = new hash_set<tree>;
  chkp_completed_bounds_set = new hash_set<tree>;
  delete chkp_reg_bounds;
  chkp_reg_bounds = new hash_map<tree, tree>;
  delete chkp_bound_vars;
  chkp_bound_vars = new hash_map<tree, tree>;
  chkp_reg_addr_bounds = new hash_map<tree, tree>;
  chkp_incomplete_bounds_map = new hash_map<tree, tree>;
  delete chkp_bounds_map;
  chkp_bounds_map = new hash_map<tree, tree>;
  chkp_abnormal_copies = BITMAP_GGC_ALLOC ();

  entry_block = NULL;
  zero_bounds = NULL_TREE;
  none_bounds = NULL_TREE;
  incomplete_bounds = integer_zero_node;
  tmp_var = NULL_TREE;
  size_tmp_var = NULL_TREE;

  chkp_uintptr_type = lang_hooks.types.type_for_mode (ptr_mode, true);

  /* We create these constant bounds once for each object file.
     These symbols go to comdat section and result in single copy
     of each one in the final binary.  */
  chkp_get_zero_bounds_var ();
  chkp_get_none_bounds_var ();

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  bitmap_obstack_initialize (NULL);
}

/* Finalize instrumentation pass.  */
static void
chkp_fini (void)
{
  in_chkp_pass = false;

  delete chkp_invalid_bounds;
  delete chkp_completed_bounds_set;
  delete chkp_reg_addr_bounds;
  delete chkp_incomplete_bounds_map;

  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);

  bitmap_obstack_release (NULL);

  entry_block = NULL;
  zero_bounds = NULL_TREE;
  none_bounds = NULL_TREE;
}

/* Main instrumentation pass function.  */
static unsigned int
chkp_execute (void)
{
  chkp_init ();

  chkp_instrument_function ();

  chkp_remove_useless_builtins ();

  chkp_function_mark_instrumented (cfun->decl);

  chkp_fix_cfg ();

  chkp_fini ();

  return 0;
}

/* Instrumentation pass gate.  */
static bool
chkp_gate (void)
{
  cgraph_node *node = cgraph_node::get (cfun->decl);
  return ((node != NULL
	   && node->instrumentation_clone)
	   || lookup_attribute ("chkp ctor", DECL_ATTRIBUTES (cfun->decl)));
}

namespace {

const pass_data pass_data_chkp =
{
  GIMPLE_PASS, /* type */
  "chkp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa | PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_il
  | TODO_update_ssa /* todo_flags_finish */
};

class pass_chkp : public gimple_opt_pass
{
public:
  pass_chkp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_chkp, ctxt)
  {}

  /* opt_pass methods: */
  virtual opt_pass * clone ()
    {
      return new pass_chkp (m_ctxt);
    }

  virtual bool gate (function *)
    {
      return chkp_gate ();
    }

  virtual unsigned int execute (function *)
    {
      return chkp_execute ();
    }

}; // class pass_chkp

} // anon namespace

gimple_opt_pass *
make_pass_chkp (gcc::context *ctxt)
{
  return new pass_chkp (ctxt);
}

#include "gt-tree-chkp.h"
