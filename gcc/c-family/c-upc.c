/* c-upc.c: implement UPC-related actions
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.
   Based on original implementation
     by Jesse M. Draper <jdraper@super.org>
     and William W. Carlson <wwc@super.org>.

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
#include "tree.h"
#include "tree-upc.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "input.h"
#include "c/c-tree.h"
#include "langhooks.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "output.h"
#include "toplev.h"
#include "attribs.h"
#include "basic-block.h"
#include "gimple-expr.h"
#include "ggc.h"
#include "tm.h"
#include "function.h"
#include "target.h"
#include "tree-iterator.h"
#include "common/common-target.h"
#include "cgraph.h"
#include "varasm.h"
#include "c-upc.h"
#include "c-upc-gasp.h"
#include "c-upc-low.h"
#include "c-upc-pts.h"
#include "c-upc-pts-ops.h"
#include "c-upc-rts-names.h"


static int recursive_count_upc_threads_refs (tree);
static int upc_sizeof_type_check (const char *, tree);


/* Return a UPC pointer-to-shared type with target type, TO_TYPE.
   If the UPC pointer-to-shared representation has a "register mode",
   then build a pointer type with that mode.  If the UPC pointer-to-shared
   representation type has BLKmode, then calculate its size based
   upon the representation type.  */

tree
upc_build_pointer_type (tree to_type)
{
  enum machine_mode pointer_mode;
  tree ptr_type;
  if (to_type == NULL_TREE || TREE_CODE (to_type) == ERROR_MARK)
    return error_mark_node;
  pointer_mode = TYPE_MODE (upc_pts_rep_type_node);
  ptr_type = build_pointer_type_for_mode (to_type, pointer_mode, false);
  if (!integer_zerop (TYPE_SIZE (ptr_type)))
    return ptr_type;
  /* If the UPC pointer-to-shared representation has a size of zero,
     then it must have BLKmode.  In that case, calculate the sizes
     and alignment from the underlying representation type.  This
     situation may arise when the 'struct PTS' representation is
     configured on targets that do not assign TImode to aligned
     128 bit structs.  */
  gcc_assert (pointer_mode == BLKmode);
  TYPE_SIZE (ptr_type)      = TYPE_SIZE (upc_pts_rep_type_node);
  TYPE_SIZE_UNIT (ptr_type) = TYPE_SIZE_UNIT (upc_pts_rep_type_node);
  TYPE_ALIGN (ptr_type)     = TYPE_ALIGN (upc_pts_rep_type_node);
  TYPE_UNSIGNED (ptr_type)  = TYPE_UNSIGNED (upc_pts_rep_type_node);
  TYPE_PRECISION (ptr_type) = TYPE_PRECISION (upc_pts_rep_type_node);
  return ptr_type;
}

/* Check the type of the operand passed to a
   upc_*sizeof () operator.
   
   The type must *not* be:
   - an error mark node
   - an incomplete type
   - a function type
   - a void type

   The type *must* be a UPC 'shared' type.

   UPC defines the following flavors of sizeof operators:
   upc_blocksizeof, upc_elemsizeof, and upc_localsizeof.
   These operations have similar syntax and constraints
   as the "C" language sizeof operator.  */

static int
upc_sizeof_type_check (const char *op_name, tree type)
{
  enum tree_code code = TREE_CODE (type);
  if (code == ERROR_MARK)
    {
      return 0;
    }
  else if (!COMPLETE_TYPE_P (type))
    {
      lang_hooks.types.incomplete_type_error (NULL_TREE, type);
      return 0;
    }
  else if (code == FUNCTION_TYPE)
    {
      error ("UPC operator %s applied to a function type", op_name);
      return 0;
    }
  else if (code == VOID_TYPE)
    {
      error ("UPC operator %s applied to a void type", op_name);
      return 0;
    }
  else if (!upc_shared_type_p (type))
    {
      error ("UPC operator %s applied to a non-shared type", op_name);
      return 0;
    }
  return 1;
}

/* Compute the value of the `upc_blocksizeof' operator.
   The UPC block size is the value of UPC's "layout qualifier".
   For example:

   Declaration				upc_blocksizeof()
   ----------- 				----------------
   shared int A[5*THREADS];     	1 (default) 
   shared [5] int A[5*THREADS];    	5
   shared [] int A[5*100];      	0 (indefinite)
   shared [*] int A[5*THREADS];     	5 (distributed by compiler) */

tree
upc_blocksizeof (location_t ARG_UNUSED (loc), tree type)
{
  tree block_factor = size_one_node;
  if (!type || TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (upc_sizeof_type_check ("upc_blocksizeof", type))
    block_factor = upc_get_block_factor (type);
  return block_factor;
}

/* Compute the value of the `upc_elemsizeof' operator.  */

tree
upc_elemsizeof (location_t loc, tree type)
{
  tree elem_size;

  if (!(type && upc_sizeof_type_check ("upc_elemsizeof", type)))
    return size_int (1);
  elem_size = c_sizeof (loc, strip_array_types (type));
  return elem_size;
}

/* Compute the value of the `upc_localsizeof' operator.
   Per the language spec:
   The upc_localsizeof operator returns the size, in bytes, of the
   local portion of its operand, which may be a shared object or a
   shared-qualified type.  It returns the same value on all threads; the
   value is an upper bound of the size allocated with affinity to any
   single thread and may include an unspecified amount of padding. The
   result of upc_localsizeof is an integer constant.  */

tree
upc_localsizeof (location_t loc, tree type)
{
  tree block_factor, local_size, total_size;

  if (!(type && upc_sizeof_type_check ("upc_localsizeof", type)))
    return size_one_node;

  /* for scalars, return sizeof */

  if (TREE_CODE (type) != ARRAY_TYPE)
    return c_sizeof (loc, type);

  block_factor = upc_blocksizeof (loc, type);
  block_factor = convert (bitsizetype, block_factor);
  total_size = TYPE_SIZE (type);

  if (integer_zerop (block_factor))
    {
      /* local size is total size, because the entire
         object lives on a single thread.  This is the
	 case for declarations of types with an "indefinite"
	 layout qualifier.  For example, given:
	   shared [] int A[100];
         the value returned for upc_localsizeof (A)
	 will be: 100 * sizeof (int).  */
      local_size = total_size;
    }
  else
    {
      tree elt_type, elt_size, n_elts;
      tree t_factor, n_full_blocks;
      tree n_full_blocks_per_thread, n_elts_in_full_blocks;
      tree n_rem_elts, n_local_elts;
      elt_type = strip_array_types (type);
      if (!elt_type || TREE_CODE (elt_type) == ERROR_MARK)
	return size_one_node;
      elt_size = TYPE_SIZE (elt_type);
      n_elts = size_binop (EXACT_DIV_EXPR, total_size, elt_size);
      /* Use the worst case size, if compiling in a dynamic
         threads environment.  The worst case size can
         be derived by setting T_FACTOR to 1 in the calculations
         that follow.  Otherwise T_FACTOR is equal to THREADS. */
      t_factor = flag_upc_threads ? upc_num_threads () : size_one_node;
      t_factor = convert (bitsizetype, t_factor);
      n_full_blocks = size_binop (FLOOR_DIV_EXPR, n_elts, block_factor);
      n_full_blocks_per_thread = size_binop (FLOOR_DIV_EXPR,
					     n_full_blocks, t_factor);
      n_elts_in_full_blocks = size_binop (MULT_EXPR,
					  size_binop (MULT_EXPR,
					       n_full_blocks_per_thread,
					       t_factor),
					  block_factor);
      n_rem_elts = size_binop (MINUS_EXPR, n_elts, n_elts_in_full_blocks);
      n_local_elts = size_binop (MULT_EXPR,
				 n_full_blocks_per_thread, block_factor);
      /* If any elements remain, add a full block size.  */
      if (!integer_zerop (n_rem_elts))
	n_local_elts = size_binop (PLUS_EXPR, n_local_elts, block_factor);
      local_size = size_binop (MULT_EXPR, n_local_elts, elt_size);
    }

  /* Convert local size into bytes, and return result. */

  local_size = convert (sizetype, local_size);
  local_size = size_binop (CEIL_DIV_EXPR, local_size,
			   size_int (BITS_PER_UNIT));
  return local_size;
}

/* Traverse the expression and return the number of times
   THREADS is referenced.  This is used to check the restriction
   on UPC shared array declarations, that the predefined THREADS
   variable can be mentioned only once.  */

static int
recursive_count_upc_threads_refs (tree expr)
{
  enum tree_code code;
  int i;
  int count = 0;
  if (expr == NULL_TREE)
    return 0;
  code = TREE_CODE (expr);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_statement:
      for (i = 0; i < TREE_CODE_LENGTH (code); i++)
	count += recursive_count_upc_threads_refs (TREE_OPERAND (expr, i));
      break;
    case tcc_declaration:
      if (expr == lookup_name (get_identifier ("THREADS")))
	count = 1;
      break;
    default:
      break;
    }
  return count;
}

/* Count the number of references to THREADS inside `expr'. */

int
count_upc_threads_refs (tree expr)
{
  return recursive_count_upc_threads_refs (expr);
}

/* Test that EXPR is an expression tree where THREADS appears on
   the left or the right hand side of a multiply, in a series
   of zero or more multiplies.  For proper operation, the caller
   should ensure that THREADS is referenced only once,
   by calling count_upc_threads_refs () prior to calling this routine. */

int
is_multiple_of_upc_threads (tree expr)
{
  enum tree_code code;
  if (expr == NULL_TREE)
    return 0;
  if (expr == lookup_name (get_identifier ("THREADS")))
    return 1;
  code = TREE_CODE (expr);
  if (code == MULT_EXPR)
    return is_multiple_of_upc_threads (TREE_OPERAND (expr, 0))
      | is_multiple_of_upc_threads (TREE_OPERAND (expr, 1));
  if ((code == NOP_EXPR) || (code == NON_LVALUE_EXPR)
      || (code == CONVERT_EXPR))
    return is_multiple_of_upc_threads (TREE_OPERAND (expr, 0));
  return 0;
}

/* Find all references to THREADS and change them into the constant `1'.
   This is done so that fold () when applied to the dimension of a
   UPC shared array will yield the local size of the array.  */

void
set_upc_threads_refs_to_one (tree *expr)
{
  enum tree_code code;
  int i;
  if (*expr == NULL_TREE)
    return;
  code = TREE_CODE (*expr);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_statement:
      for (i = 0; i < TREE_CODE_LENGTH (code); i++)
	set_upc_threads_refs_to_one (&TREE_OPERAND (*expr, i));
      break;
    case tcc_declaration:
      if (*expr == lookup_name (get_identifier ("THREADS")))
	*expr = integer_one_node;
      break;
    default:
      break;
    }
  return;
}

/* As part of declaration processing, for a particular kind
   of declaration, DECL_KIND, and a given LAYOUT_QUALIFIER, calculate
   the resulting blocking factor and return it.  Issue an error
   diagnostic if the LAYOUT_QUALIFIER specification is invalid.
   For array types, the TYPE parameter may be the MAIN_VARIANT,
   and not shared qualified; in that case - ELEM_BLOCK_FACTOR
   is the blocking factor derived from the original element type.
   If LAYOUT_QUALIFIER is NULL and ELEM_BLOCK_FACTOR is non-null,
   then the ELEM_BLOCK_FACTOR will be used.  This situation occurs
   when the element type is a typedef, for example.  If both
   LAYOUT_QUALIFIER and ELEM_BLOCK_FACTOR are non-NULL, then they
   must be equal.  */

tree
upc_grok_layout_qualifier (location_t loc, const enum tree_code decl_kind,
	                   tree type, tree elem_block_factor,
			   tree layout_qualifier)
{
  tree block_factor = NULL_TREE;

  if (!type || (TREE_CODE (type) == ERROR_MARK))
    return error_mark_node;

  if (TREE_CODE (type) == VOID_TYPE)
    {
      error_at (loc, "UPC layout qualifier cannot be applied to a void type");
      return NULL_TREE;
    }

  /* If no explicit layout qualifier was supplied, then
     use the blocking factor derived from the element type.  */
  if (!layout_qualifier && elem_block_factor)
    return elem_block_factor;

  /* The layout qualifier is given as the subscript operand
     of an array ref. */
  gcc_assert (layout_qualifier);
  gcc_assert (TREE_CODE (layout_qualifier) == ARRAY_REF);
  layout_qualifier = TREE_OPERAND (layout_qualifier, 1);

  if (layout_qualifier == NULL_TREE)
    {
      /* The layout qualifier is [], which is
         equivalent to specifying [0].  */
      block_factor = size_zero_node;
    }
  else if ((TREE_CODE (layout_qualifier) == INDIRECT_REF)
	   && ((TREE_OPERAND (layout_qualifier, 0)) == NULL_TREE))
    {
      tree elt_size, elt_type, n_threads;
      /* The layout qualifier is [*].  The compiler must calculate
         a blocking factor that evenly distributes the array's
	 elements over all the UPC threads.  */
      if (!COMPLETE_TYPE_P (type))
	{
	  error_at (loc, "UPC layout qualifier of the form [*] cannot be "
	                 "applied to an incomplete type");
	  return NULL_TREE;
	}
      if (decl_kind == POINTER_TYPE)
	{
	  error_at (loc, "UPC [*] qualifier may not be used in "
	                 "declaration of pointers");
	  return NULL_TREE;
	}
      /* The blocking factor is given by this expression:
         (sizeof (a) / upc_elemsizeof (a) + (THREADS - 1)) / THREADS,
         where 'a' is the array being distributed. */
      elt_type = strip_array_types (type);
      elt_size = TYPE_SIZE (elt_type);
      if (TYPE_HAS_THREADS_FACTOR (type))
	block_factor =
	  size_binop (FLOOR_DIV_EXPR, TYPE_SIZE (type), elt_size);
      else
	{
	  n_threads = convert (bitsizetype, upc_num_threads ());
	  if (TREE_CODE (n_threads) != INTEGER_CST)
	    {
	      error_at (loc, "a UPC layout qualifier of '[*]' requires that "
		             "the array size is either an integral constant "
		             "or an integral multiple of THREADS");
	      block_factor = size_one_node;
	    }
	  else
	    {
	      block_factor = size_binop (CEIL_DIV_EXPR,
				 size_binop (FLOOR_DIV_EXPR,
					     TYPE_SIZE (type),
					     elt_size),
				 n_threads);
	    }
	}
    }
  else
    {
      STRIP_NOPS (layout_qualifier);
      if (TREE_CODE (layout_qualifier) != INTEGER_CST)
        {
	  error_at (loc, "UPC layout qualifier is not an integral constant");
          block_factor = size_one_node;
	}
      else if (tree_to_shwi (layout_qualifier) < 0)
        {
	  error_at (loc, "UPC layout qualifier must be a non-negative "
	                 "integral constant");
          block_factor = size_one_node;
	}
      else
	block_factor = fold (layout_qualifier);
    }

  if (TREE_OVERFLOW_P (block_factor)
      || tree_to_shwi (block_factor) > UPC_MAX_BLOCK_SIZE)
    {
      error_at (loc, "the maximum UPC block size in this implementation "
                     "is %ld", (long int) UPC_MAX_BLOCK_SIZE);
      return NULL_TREE;
    }

  if (tree_int_cst_compare (block_factor, integer_zero_node) < 0)
    {
      error_at (loc, "UPC layout qualifier must be a "
                     "non-negative integral constant");
      return NULL_TREE;
    }

  /* Make sure that the UPC blocking factors are of type
     'size_t' so that a compare of the tree pointers
     is sufficient to match block sizes.  */
  if (block_factor)
    block_factor = convert (sizetype, block_factor);

  if ((block_factor && elem_block_factor)
      && block_factor != elem_block_factor)
    {
       error_at (loc, "UPC layout qualifier is incompatible with "
		      "the referenced type");
       return elem_block_factor;
    }

  /* A block size of [1] is the same as specifying no
     block size at all.  */
  if (block_factor == size_one_node)
    block_factor = NULL_TREE;

  return block_factor;
}

/* If DECL is a UPC shared variable, make sure that it ends up
   in the executable file.  */

void
upc_check_decl (tree decl)
{
  if (decl
      && TREE_CODE (decl) == VAR_DECL
      && TREE_TYPE (decl) && upc_shared_type_p (TREE_TYPE (decl)))
    {
      TREE_USED (decl) = 1;
      TREE_ADDRESSABLE (decl) = 1;
      TREE_STATIC (decl) = 1;
      /* Work-around a problem where the front-end doesn't
         properly process the used flags set above, on
         static variables when flag_unit_at_a_time isn't set. */
      if ((TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
	  && !flag_unit_at_a_time
	  && !lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
	{
	  tree used_id = get_identifier ("used");
	  tree used_attrib = tree_cons (used_id, NULL_TREE, NULL_TREE);
	  decl_attributes (&decl, used_attrib, 0);
	}
    }
}

/* Return TRUE if TYPE contains any references to UPC pointers-to-shared.  */

int
upc_contains_pts_refs_p (tree type)
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      return upc_shared_type_p (TREE_TYPE (type));

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree fields;
	/* For a type that has fields, see if the fields have pointers.  */
	for (fields = TYPE_FIELDS (type); fields;
	     fields = TREE_CHAIN (fields))
	  if (TREE_CODE (fields) == FIELD_DECL
	      && upc_contains_pts_refs_p (TREE_TYPE (fields)))
	    return 1;
	return 0;
      }

    case ARRAY_TYPE:
      /* An array type contains pointers if its element type does.  */
      return upc_contains_pts_refs_p (TREE_TYPE (type));

    default:
      return 0;
    }
}

/* Assign DECL to a specific linker section, if required.
   UPC shared variables are given their own link section on
   most target platforms, and if compiling in "pthreads mode"
   regular local file scope variables are made thread local.  */

void
upc_set_decl_section (tree decl)
{
  if (TREE_SHARED (decl))
    {
#ifdef UPC_SHARED_SECTION_NAME
      /* UPC shared variables are placed in their own shared section */
      int slen = strlen (UPC_SHARED_SECTION_NAME);
      DECL_SECTION_NAME (decl) = build_string (slen, UPC_SHARED_SECTION_NAME);
#endif
    }
  else if (flag_upc_pthreads
	   && ((TREE_STATIC (decl) && (DECL_SECTION_NAME (decl) == NULL_TREE))
	       || DECL_EXTERNAL (decl)))
    {
      /* If we're compiling with -fupc-pthreads-model-tls asserted
         and this is a regular "C" static scoped object which
         is either declared in a system header file,
         or is being compiled in a UPC setting,
         then assign the object to the thread local storage
	 (TLS) section.  */
      extern int c_header_level;	/* in c-lex.c */
      if (flag_upc && (c_header_level <= 0))
	{
	  if (upc_pthreads_model == upc_pthreads_tls_model)
	    {
	      DECL_TLS_MODEL (decl) = decl_default_tls_model (decl);
	      DECL_COMMON (decl) = 0;
	    }
	  else
	    /* Only the TLS model is currently implemented. */
	    gcc_unreachable ();
	}
    }
}

/* Return an external reference to an integer variable maintained
   by the compiler and runtime to track the dynamic nesting
   of 'upc_forall' statements.  The variable's name is given by
   UPC_FORALL_DEPTH_NAME.  */

tree
upc_rts_forall_depth_var (void)
{
  tree upc_forall_depth = lookup_name (
                                  get_identifier (UPC_FORALL_DEPTH_NAME));
  if (upc_forall_depth == NULL_TREE)
    internal_error ("the UPC runtime variable '" UPC_FORALL_DEPTH_NAME "' "
		    "cannot be located; this variable should be defined "
		    "in a compiler-supplied include file");
  assemble_external (upc_forall_depth);
  TREE_USED (upc_forall_depth) = 1;
  return upc_forall_depth;
}

/* Diagnose instances of UPC statements that were
   defined in very early UPC language specifications and that
   have since been deprecated.  */

int
upc_diagnose_deprecated_stmt (location_t loc, tree id)
{
  const char *name = IDENTIFIER_POINTER (id);
  struct deprecated_stmt_entry
  {
    const char *deprecated_id;
    const char *correct_id;
  };
  static const struct deprecated_stmt_entry deprecated_stmts[] =
    { {"barrier", "upc_barrier"},
      {"barrier_wait", "upc_wait"},
      {"barrier_notify", "upc_notify"},
      {"fence", "upc_fence"},
      {"forall", "upc_forall"} };
  const int n_deprecated_stmts = sizeof (deprecated_stmts)
    / sizeof (struct deprecated_stmt_entry);
  int i;
  for (i = 0; i < n_deprecated_stmts; ++i)
    {
      if (!strcmp (name, deprecated_stmts[i].deprecated_id))
	{
	  error_at (loc, "%qs was supported in version 1.0 of the UPC "
		    "specification, it has been deprecated, "
		    "use %qs instead",
		    name, deprecated_stmts[i].correct_id);
	  return 1;
	}
    }
  return 0;
}

/* Expand the pre/post increment/decrement of UPC pointer-to-shared
   into its equivalent expression tree. */

tree
upc_pts_increment (location_t location ATTRIBUTE_UNUSED,
		   enum tree_code code, tree arg)
{
  /* The result type is a pointer of the same type as the argument
     type after dropping the shared qualifier (for PTS's that happen
     to live in shared memory). */
  tree stable_arg = stabilize_reference (arg);
  tree val = (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
    ? stable_arg : save_expr (stable_arg);
  enum tree_code incr_op = (code == PREINCREMENT_EXPR
			    || code == POSTINCREMENT_EXPR)
    ? PLUS_EXPR : MINUS_EXPR;
  tree incr_val, result;
  incr_val = upc_pts_int_sum (location, incr_op, val, integer_one_node);
  TREE_SIDE_EFFECTS (incr_val) = 1;
  result = build_modify_expr (location, arg, NULL_TREE, NOP_EXPR,
			      location, incr_val, NULL_TREE);
  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    result = build2 (COMPOUND_EXPR, TREE_TYPE (incr_val), result, val);
  return result;
}

/* Return an expression that calculates the sum of a UPC
   pointer-to-shared value and an integer value.  The sum
   operator may be PLUS_EXPR or MINUS_EXPR.  The result is a
   POINTER_PLUS_EXPR with a properly scaled integer operand.
   This POINTER_PLUS_EXPR will be translated by the UPC lowering
   pass into the sequence of operations dictated both by the
   properties of the UPC pointer-to-shared type, and the UPC
   pointer-to-shared representation.  */

tree
upc_pts_int_sum (location_t loc,
		 enum tree_code resultcode, tree ptrop, tree intop)
{

  /* The result type is a pointer of the same type that is being added,
     after dropping the UPC shared qualifier.  For example, this would
     apply to UPC pointers-to-shared that happen to live in shared memory;
     the result of the expression must not be UPC shared qualified.  */

  const tree ttype = TREE_TYPE (ptrop);
  const int shared_quals =
    (TYPE_QUAL_UPC_SHARED | TYPE_QUAL_UPC_STRICT | TYPE_QUAL_UPC_RELAXED);
  const int quals_minus_shared = TYPE_QUALS (ttype) & ~shared_quals;
  const tree result_type = c_build_qualified_type (ttype, quals_minus_shared);
  const tree result_targ_type = TREE_TYPE (result_type);
  const tree base_type = strip_array_types (result_targ_type);
  tree result;

  if (TREE_CODE (result_targ_type) == VOID_TYPE)
    error_at (loc, "UPC does not allow a pointer of type %<shared void *%> "
	      "to be used in arithmetic");

  /* We have a pointer to a UPC shared object.  For pointers to
     simple objects, just build a "resultcode" tree with the intop and
     let upc_genericize() handle the arithmetic correctly.  For pointers to
     arrays, compute the number of elements represented by the intop
     and build a "resultcode" tree with the ptrop and that number. */

  if (result_targ_type != base_type)
    {
      tree elt_cnt;
      gcc_assert (TREE_CODE (result_targ_type) == ARRAY_TYPE);
      if (TREE_CODE (TYPE_SIZE (result_targ_type)) == INTEGER_CST)
	{
	  tree n_threads = convert (sizetype, upc_num_threads ());
	  int size = TREE_INT_CST_LOW (TYPE_SIZE (result_targ_type));
	  int elt_size = TREE_INT_CST_LOW (TYPE_SIZE (base_type));
	  elt_cnt = size_int (size / elt_size);
	  if (TYPE_HAS_THREADS_FACTOR (result_targ_type))
	    elt_cnt = size_binop (MULT_EXPR, n_threads, elt_cnt);
	}
      else
	{
	  tree size = TYPE_SIZE (result_targ_type);
	  tree elt_size = TYPE_SIZE (base_type);
	  elt_cnt = build2 (EXACT_DIV_EXPR, sizetype, size, elt_size);
	}
      intop = convert (sizetype, intop);
      intop = size_binop (MULT_EXPR, intop, elt_cnt);
    }
  gcc_assert (resultcode == PLUS_EXPR || resultcode == MINUS_EXPR);
  if (resultcode == MINUS_EXPR)
    intop = build1 (NEGATE_EXPR, TREE_TYPE (intop), intop);
  intop = fold (intop);

  /* POINTER_PLUS expects the operand to be sizetype, which
     is potentially unsigned.  This will have to be dealt
     with later, when expanding the UPC pointer-to-shared arithmetic.  */

  intop = convert (sizetype, intop);
  result = build2 (POINTER_PLUS_EXPR, result_type, ptrop, intop);

  /* Although there may be some specific cases where the
     addition of a constant integer to a UPC pointer-to-shared can
     be calculated at compile-time, in the more general
     cases the calculation must be made at runtime, so
     we mark the resulting sum as non-constant.  This will
     avoid situations where the compiler attempts to convert
     things like &A[14] where A is a shared array into a
     compile-time constant. */

  TREE_CONSTANT (result) = 0;
  return result;
}

/* Return an expression that calculates the difference between
   two UPC pointers-to-shared values.  */

tree
upc_pts_diff (tree op0, tree op1)
{
  const tree target_type = TREE_TYPE (TREE_TYPE (op0));
  tree result;

  /* The two pointers must both point to shared objects.  */

  if ((upc_shared_type_p (target_type)
       && !upc_shared_type_p (TREE_TYPE (TREE_TYPE (op1))))
      || (upc_shared_type_p (TREE_TYPE (TREE_TYPE (op1)))
	  && !upc_shared_type_p (target_type)))
    {
      error ("attempt to take the difference of a UPC pointer-to-shared "
	     "and a local pointer");
      return size_one_node;
    }
  result = build2 (MINUS_EXPR, ptrdiff_type_node, op0, op1);
  return result;
}

/* Return TRUE if EXP is a null UPC pointer-to-shared value.
   (Call the representation-specific hook routine to
   perform the check.)  */

int
upc_is_null_pts_p (tree exp)
{
  return (*upc_pts.is_null_p) (exp);
}

/* Return TRUE if the type of EXP is a UPC pointer-to-shared type.  */

int
upc_pts_is_valid_p (tree exp)
{
  tree type = TREE_TYPE (exp);
  return (TREE_CODE (type) == POINTER_TYPE)
    && upc_shared_type_p (TREE_TYPE (type));
}
