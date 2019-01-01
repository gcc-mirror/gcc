/* Manipulation of formal and actual parameters of functions and function
   calls.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-dfa.h"
#include "ipa-param-manipulation.h"
#include "print-tree.h"
#include "gimple-pretty-print.h"
#include "builtins.h"

/* Return a heap allocated vector containing formal parameters of FNDECL.  */

vec<tree>
ipa_get_vector_of_formal_parms (tree fndecl)
{
  vec<tree> args;
  int count;
  tree parm;

  gcc_assert (!flag_wpa);
  count = 0;
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    count++;

  args.create (count);
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    args.quick_push (parm);

  return args;
}

/* Return a heap allocated vector containing types of formal parameters of
   function type FNTYPE.  */

vec<tree>
ipa_get_vector_of_formal_parm_types (tree fntype)
{
  vec<tree> types;
  int count = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    count++;

  types.create (count);
  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    types.quick_push (TREE_VALUE (t));

  return types;
}

/* Modify the function declaration FNDECL and its type according to the plan in
   ADJUSTMENTS.  It also sets base fields of individual adjustments structures
   to reflect the actual parameters being modified which are determined by the
   base_index field.  */

void
ipa_modify_formal_parameters (tree fndecl, ipa_parm_adjustment_vec adjustments)
{
  vec<tree> oparms = ipa_get_vector_of_formal_parms (fndecl);
  tree orig_type = TREE_TYPE (fndecl);
  tree old_arg_types = TYPE_ARG_TYPES (orig_type);

  /* The following test is an ugly hack, some functions simply don't have any
     arguments in their type.  This is probably a bug but well... */
  bool care_for_types = (old_arg_types != NULL_TREE);
  bool last_parm_void;
  vec<tree> otypes;
  if (care_for_types)
    {
      last_parm_void = (TREE_VALUE (tree_last (old_arg_types))
			== void_type_node);
      otypes = ipa_get_vector_of_formal_parm_types (orig_type);
      if (last_parm_void)
	gcc_assert (oparms.length () + 1 == otypes.length ());
      else
	gcc_assert (oparms.length () == otypes.length ());
    }
  else
    {
      last_parm_void = false;
      otypes.create (0);
    }

  int len = adjustments.length ();
  tree *link = &DECL_ARGUMENTS (fndecl);
  tree new_arg_types = NULL;
  for (int i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      gcc_assert (link);

      adj = &adjustments[i];
      tree parm;
      if (adj->op == IPA_PARM_OP_NEW)
	parm = NULL;
      else
	parm = oparms[adj->base_index];
      adj->base = parm;

      if (adj->op == IPA_PARM_OP_COPY)
	{
	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, otypes[adj->base_index],
				       new_arg_types);
	  *link = parm;
	  link = &DECL_CHAIN (parm);
	}
      else if (adj->op != IPA_PARM_OP_REMOVE)
	{
	  tree new_parm;
	  tree ptype;

	  if (adj->by_ref)
	    ptype = build_pointer_type (adj->type);
	  else
	    {
	      ptype = adj->type;
	      if (is_gimple_reg_type (ptype)
		  && TYPE_MODE (ptype) != BLKmode)
		{
		  unsigned malign = GET_MODE_ALIGNMENT (TYPE_MODE (ptype));
		  if (TYPE_ALIGN (ptype) != malign)
		    ptype = build_aligned_type (ptype, malign);
		}
	    }

	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, ptype, new_arg_types);

	  new_parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE,
				 ptype);
	  const char *prefix = adj->arg_prefix ? adj->arg_prefix : "SYNTH";
	  DECL_NAME (new_parm) = create_tmp_var_name (prefix);
	  DECL_ARTIFICIAL (new_parm) = 1;
	  DECL_ARG_TYPE (new_parm) = ptype;
	  DECL_CONTEXT (new_parm) = fndecl;
	  TREE_USED (new_parm) = 1;
	  DECL_IGNORED_P (new_parm) = 1;
	  layout_decl (new_parm, 0);

	  if (adj->op == IPA_PARM_OP_NEW)
	    adj->base = NULL;
	  else
	    adj->base = parm;
	  adj->new_decl = new_parm;

	  *link = new_parm;
	  link = &DECL_CHAIN (new_parm);
	}
    }

  *link = NULL_TREE;

  tree new_reversed = NULL;
  if (care_for_types)
    {
      new_reversed = nreverse (new_arg_types);
      if (last_parm_void)
	{
	  if (new_reversed)
	    TREE_CHAIN (new_arg_types) = void_list_node;
	  else
	    new_reversed = void_list_node;
	}
    }

  /* Use copy_node to preserve as much as possible from original type
     (debug info, attribute lists etc.)
     Exception is METHOD_TYPEs must have THIS argument.
     When we are asked to remove it, we need to build new FUNCTION_TYPE
     instead.  */
  tree new_type = NULL;
  if (TREE_CODE (orig_type) != METHOD_TYPE
       || (adjustments[0].op == IPA_PARM_OP_COPY
	  && adjustments[0].base_index == 0))
    {
      new_type = build_distinct_type_copy (orig_type);
      TYPE_ARG_TYPES (new_type) = new_reversed;
    }
  else
    {
      new_type
        = build_distinct_type_copy (build_function_type (TREE_TYPE (orig_type),
							 new_reversed));
      TYPE_CONTEXT (new_type) = TYPE_CONTEXT (orig_type);
      DECL_VINDEX (fndecl) = NULL_TREE;
    }

  /* When signature changes, we need to clear builtin info.  */
  if (fndecl_built_in_p (fndecl))
    {
      DECL_BUILT_IN_CLASS (fndecl) = NOT_BUILT_IN;
      DECL_FUNCTION_CODE (fndecl) = (enum built_in_function) 0;
    }

  TREE_TYPE (fndecl) = new_type;
  DECL_VIRTUAL_P (fndecl) = 0;
  DECL_LANG_SPECIFIC (fndecl) = NULL;
  otypes.release ();
  oparms.release ();
}

/* Modify actual arguments of a function call CS as indicated in ADJUSTMENTS.
   If this is a directly recursive call, CS must be NULL.  Otherwise it must
   contain the corresponding call graph edge.  */

void
ipa_modify_call_arguments (struct cgraph_edge *cs, gcall *stmt,
			   ipa_parm_adjustment_vec adjustments)
{
  struct cgraph_node *current_node = cgraph_node::get (current_function_decl);
  vec<tree> vargs;
  vec<tree, va_gc> **debug_args = NULL;
  gcall *new_stmt;
  gimple_stmt_iterator gsi, prev_gsi;
  tree callee_decl;
  int i, len;

  len = adjustments.length ();
  vargs.create (len);
  callee_decl = !cs ? gimple_call_fndecl (stmt) : cs->callee->decl;
  current_node->remove_stmt_references (stmt);

  gsi = gsi_for_stmt (stmt);
  prev_gsi = gsi;
  gsi_prev (&prev_gsi);
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;

      adj = &adjustments[i];

      if (adj->op == IPA_PARM_OP_COPY)
	{
	  tree arg = gimple_call_arg (stmt, adj->base_index);

	  vargs.quick_push (arg);
	}
      else if (adj->op != IPA_PARM_OP_REMOVE)
	{
	  tree expr, base, off;
	  location_t loc;
	  unsigned int deref_align = 0;
	  bool deref_base = false;

	  /* We create a new parameter out of the value of the old one, we can
	     do the following kind of transformations:

	     - A scalar passed by reference is converted to a scalar passed by
               value.  (adj->by_ref is false and the type of the original
               actual argument is a pointer to a scalar).

             - A part of an aggregate is passed instead of the whole aggregate.
               The part can be passed either by value or by reference, this is
               determined by value of adj->by_ref.  Moreover, the code below
               handles both situations when the original aggregate is passed by
               value (its type is not a pointer) and when it is passed by
               reference (it is a pointer to an aggregate).

	     When the new argument is passed by reference (adj->by_ref is true)
	     it must be a part of an aggregate and therefore we form it by
	     simply taking the address of a reference inside the original
	     aggregate.  */

	  poly_int64 byte_offset = exact_div (adj->offset, BITS_PER_UNIT);
	  base = gimple_call_arg (stmt, adj->base_index);
	  loc = gimple_location (stmt);

	  if (TREE_CODE (base) != ADDR_EXPR
	      && POINTER_TYPE_P (TREE_TYPE (base)))
	    off = build_int_cst (adj->alias_ptr_type, byte_offset);
	  else
	    {
	      poly_int64 base_offset;
	      tree prev_base;
	      bool addrof;

	      if (TREE_CODE (base) == ADDR_EXPR)
		{
		  base = TREE_OPERAND (base, 0);
		  addrof = true;
		}
	      else
		addrof = false;
	      prev_base = base;
	      base = get_addr_base_and_unit_offset (base, &base_offset);
	      /* Aggregate arguments can have non-invariant addresses.  */
	      if (!base)
		{
		  base = build_fold_addr_expr (prev_base);
		  off = build_int_cst (adj->alias_ptr_type, byte_offset);
		}
	      else if (TREE_CODE (base) == MEM_REF)
		{
		  if (!addrof)
		    {
		      deref_base = true;
		      deref_align = TYPE_ALIGN (TREE_TYPE (base));
		    }
		  off = build_int_cst (adj->alias_ptr_type,
				       base_offset + byte_offset);
		  off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1),
					 off);
		  base = TREE_OPERAND (base, 0);
		}
	      else
		{
		  off = build_int_cst (adj->alias_ptr_type,
				       base_offset + byte_offset);
		  base = build_fold_addr_expr (base);
		}
	    }

	  if (!adj->by_ref)
	    {
	      tree type = adj->type;
	      unsigned int align;
	      unsigned HOST_WIDE_INT misalign;

	      if (deref_base)
		{
		  align = deref_align;
		  misalign = 0;
		}
	      else
		{
		  get_pointer_alignment_1 (base, &align, &misalign);
		  if (TYPE_ALIGN (type) > align)
		    align = TYPE_ALIGN (type);
		}
	      misalign += (offset_int::from (wi::to_wide (off),
					     SIGNED).to_short_addr ()
			   * BITS_PER_UNIT);
	      misalign = misalign & (align - 1);
	      if (misalign != 0)
		align = least_bit_hwi (misalign);
	      if (align < TYPE_ALIGN (type))
		type = build_aligned_type (type, align);
	      base = force_gimple_operand_gsi (&gsi, base,
					       true, NULL, true, GSI_SAME_STMT);
	      expr = fold_build2_loc (loc, MEM_REF, type, base, off);
	      REF_REVERSE_STORAGE_ORDER (expr) = adj->reverse;
	      /* If expr is not a valid gimple call argument emit
	         a load into a temporary.  */
	      if (is_gimple_reg_type (TREE_TYPE (expr)))
		{
		  gimple *tem = gimple_build_assign (NULL_TREE, expr);
		  if (gimple_in_ssa_p (cfun))
		    {
		      gimple_set_vuse (tem, gimple_vuse (stmt));
		      expr = make_ssa_name (TREE_TYPE (expr), tem);
		    }
		  else
		    expr = create_tmp_reg (TREE_TYPE (expr));
		  gimple_assign_set_lhs (tem, expr);
		  gimple_set_location (tem, loc);
		  gsi_insert_before (&gsi, tem, GSI_SAME_STMT);
		}
	    }
	  else
	    {
	      expr = fold_build2_loc (loc, MEM_REF, adj->type, base, off);
	      REF_REVERSE_STORAGE_ORDER (expr) = adj->reverse;
	      expr = build_fold_addr_expr (expr);
	      expr = force_gimple_operand_gsi (&gsi, expr,
					       true, NULL, true, GSI_SAME_STMT);
	    }
	  vargs.quick_push (expr);
	}
      if (adj->op != IPA_PARM_OP_COPY && MAY_HAVE_DEBUG_BIND_STMTS)
	{
	  unsigned int ix;
	  tree ddecl = NULL_TREE, origin = DECL_ORIGIN (adj->base), arg;
	  gimple *def_temp;

	  arg = gimple_call_arg (stmt, adj->base_index);
	  if (!useless_type_conversion_p (TREE_TYPE (origin), TREE_TYPE (arg)))
	    {
	      if (!fold_convertible_p (TREE_TYPE (origin), arg))
		continue;
	      arg = fold_convert_loc (gimple_location (stmt),
				      TREE_TYPE (origin), arg);
	    }
	  if (debug_args == NULL)
	    debug_args = decl_debug_args_insert (callee_decl);
	  for (ix = 0; vec_safe_iterate (*debug_args, ix, &ddecl); ix += 2)
	    if (ddecl == origin)
	      {
		ddecl = (**debug_args)[ix + 1];
		break;
	      }
	  if (ddecl == NULL)
	    {
	      ddecl = make_node (DEBUG_EXPR_DECL);
	      DECL_ARTIFICIAL (ddecl) = 1;
	      TREE_TYPE (ddecl) = TREE_TYPE (origin);
	      SET_DECL_MODE (ddecl, DECL_MODE (origin));

	      vec_safe_push (*debug_args, origin);
	      vec_safe_push (*debug_args, ddecl);
	    }
	  def_temp = gimple_build_debug_bind (ddecl, unshare_expr (arg), stmt);
	  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing stmt:");
      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0);
    }

  new_stmt = gimple_build_call_vec (callee_decl, vargs);
  vargs.release ();
  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  if (gimple_in_ssa_p (cfun))
    {
      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
      if (gimple_vdef (stmt))
	{
	  gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	  SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "with stmt:");
      print_gimple_stmt (dump_file, new_stmt, 0);
      fprintf (dump_file, "\n");
    }
  gsi_replace (&gsi, new_stmt, true);
  if (cs)
    cs->set_call_stmt (new_stmt);
  do
    {
      current_node->record_stmt_references (gsi_stmt (gsi));
      gsi_prev (&gsi);
    }
  while (gsi_stmt (gsi) != gsi_stmt (prev_gsi));
}

/* Return true iff BASE_INDEX is in ADJUSTMENTS more than once.  */

static bool
index_in_adjustments_multiple_times_p (int base_index,
				       ipa_parm_adjustment_vec adjustments)
{
  int i, len = adjustments.length ();
  bool one = false;

  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = &adjustments[i];

      if (adj->base_index == base_index)
	{
	  if (one)
	    return true;
	  else
	    one = true;
	}
    }
  return false;
}

/* Return adjustments that should have the same effect on function parameters
   and call arguments as if they were first changed according to adjustments in
   INNER and then by adjustments in OUTER.  */

ipa_parm_adjustment_vec
ipa_combine_adjustments (ipa_parm_adjustment_vec inner,
			 ipa_parm_adjustment_vec outer)
{
  int i, outlen = outer.length ();
  int inlen = inner.length ();
  int removals = 0;
  ipa_parm_adjustment_vec adjustments, tmp;

  tmp.create (inlen);
  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n;
      n = &inner[i];

      if (n->op == IPA_PARM_OP_REMOVE)
	removals++;
      else
	{
	  /* FIXME: Handling of new arguments are not implemented yet.  */
	  gcc_assert (n->op != IPA_PARM_OP_NEW);
	  tmp.quick_push (*n);
	}
    }

  adjustments.create (outlen + removals);
  for (i = 0; i < outlen; i++)
    {
      struct ipa_parm_adjustment r;
      struct ipa_parm_adjustment *out = &outer[i];
      struct ipa_parm_adjustment *in = &tmp[out->base_index];

      memset (&r, 0, sizeof (r));
      gcc_assert (in->op != IPA_PARM_OP_REMOVE);
      if (out->op == IPA_PARM_OP_REMOVE)
	{
	  if (!index_in_adjustments_multiple_times_p (in->base_index, tmp))
	    {
	      r.op = IPA_PARM_OP_REMOVE;
	      adjustments.quick_push (r);
	    }
	  continue;
	}
      else
	{
	  /* FIXME: Handling of new arguments are not implemented yet.  */
	  gcc_assert (out->op != IPA_PARM_OP_NEW);
	}

      r.base_index = in->base_index;
      r.type = out->type;

      /* FIXME:  Create nonlocal value too.  */

      if (in->op == IPA_PARM_OP_COPY && out->op == IPA_PARM_OP_COPY)
	r.op = IPA_PARM_OP_COPY;
      else if (in->op == IPA_PARM_OP_COPY)
	r.offset = out->offset;
      else if (out->op == IPA_PARM_OP_COPY)
	r.offset = in->offset;
      else
	r.offset = in->offset + out->offset;
      adjustments.quick_push (r);
    }

  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n = &inner[i];

      if (n->op == IPA_PARM_OP_REMOVE)
	adjustments.quick_push (*n);
    }

  tmp.release ();
  return adjustments;
}

/* If T is an SSA_NAME, return NULL if it is not a default def or
   return its base variable if it is.  If IGNORE_DEFAULT_DEF is true,
   the base variable is always returned, regardless if it is a default
   def.  Return T if it is not an SSA_NAME.  */

static tree
get_ssa_base_param (tree t, bool ignore_default_def)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      if (ignore_default_def || SSA_NAME_IS_DEFAULT_DEF (t))
	return SSA_NAME_VAR (t);
      else
	return NULL_TREE;
    }
  return t;
}

/* Given an expression, return an adjustment entry specifying the
   transformation to be done on EXPR.  If no suitable adjustment entry
   was found, returns NULL.

   If IGNORE_DEFAULT_DEF is set, consider SSA_NAMEs which are not a
   default def, otherwise bail on them.

   If CONVERT is non-NULL, this function will set *CONVERT if the
   expression provided is a component reference.  ADJUSTMENTS is the
   adjustments vector.  */

ipa_parm_adjustment *
ipa_get_adjustment_candidate (tree **expr, bool *convert,
			      ipa_parm_adjustment_vec adjustments,
			      bool ignore_default_def)
{
  if (TREE_CODE (**expr) == BIT_FIELD_REF
      || TREE_CODE (**expr) == IMAGPART_EXPR
      || TREE_CODE (**expr) == REALPART_EXPR)
    {
      *expr = &TREE_OPERAND (**expr, 0);
      if (convert)
	*convert = true;
    }

  poly_int64 offset, size, max_size;
  bool reverse;
  tree base
    = get_ref_base_and_extent (**expr, &offset, &size, &max_size, &reverse);
  if (!base || !known_size_p (size) || !known_size_p (max_size))
    return NULL;

  if (TREE_CODE (base) == MEM_REF)
    {
      offset += mem_ref_offset (base).force_shwi () * BITS_PER_UNIT;
      base = TREE_OPERAND (base, 0);
    }

  base = get_ssa_base_param (base, ignore_default_def);
  if (!base || TREE_CODE (base) != PARM_DECL)
    return NULL;

  struct ipa_parm_adjustment *cand = NULL;
  unsigned int len = adjustments.length ();
  for (unsigned i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj = &adjustments[i];

      if (adj->base == base
	  && (known_eq (adj->offset, offset) || adj->op == IPA_PARM_OP_REMOVE))
	{
	  cand = adj;
	  break;
	}
    }

  if (!cand || cand->op == IPA_PARM_OP_COPY || cand->op == IPA_PARM_OP_REMOVE)
    return NULL;
  return cand;
}

/* If the expression *EXPR should be replaced by a reduction of a parameter, do
   so.  ADJUSTMENTS is a pointer to a vector of adjustments.  CONVERT
   specifies whether the function should care about type incompatibility the
   current and new expressions.  If it is false, the function will leave
   incompatibility issues to the caller.  Return true iff the expression
   was modified. */

bool
ipa_modify_expr (tree *expr, bool convert,
		 ipa_parm_adjustment_vec adjustments)
{
  struct ipa_parm_adjustment *cand
    = ipa_get_adjustment_candidate (&expr, &convert, adjustments, false);
  if (!cand)
    return false;

  tree src;
  if (cand->by_ref)
    {
      src = build_simple_mem_ref (cand->new_decl);
      REF_REVERSE_STORAGE_ORDER (src) = cand->reverse;
    }
  else
    src = cand->new_decl;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "About to replace expr ");
      print_generic_expr (dump_file, *expr);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, src);
      fprintf (dump_file, "\n");
    }

  if (convert && !useless_type_conversion_p (TREE_TYPE (*expr), cand->type))
    {
      tree vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (*expr), src);
      *expr = vce;
    }
  else
    *expr = src;
  return true;
}

/* Dump the adjustments in the vector ADJUSTMENTS to dump_file in a human
   friendly way, assuming they are meant to be applied to FNDECL.  */

void
ipa_dump_param_adjustments (FILE *file, ipa_parm_adjustment_vec adjustments,
			    tree fndecl)
{
  int i, len = adjustments.length ();
  bool first = true;
  vec<tree> parms = ipa_get_vector_of_formal_parms (fndecl);

  fprintf (file, "IPA param adjustments: ");
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = &adjustments[i];

      if (!first)
	fprintf (file, "                 ");
      else
	first = false;

      fprintf (file, "%i. base_index: %i - ", i, adj->base_index);
      print_generic_expr (file, parms[adj->base_index]);
      if (adj->base)
	{
	  fprintf (file, ", base: ");
	  print_generic_expr (file, adj->base);
	}
      if (adj->new_decl)
	{
	  fprintf (file, ", new_decl: ");
	  print_generic_expr (file, adj->new_decl);
	}
      if (adj->new_ssa_base)
	{
	  fprintf (file, ", new_ssa_base: ");
	  print_generic_expr (file, adj->new_ssa_base);
	}

      if (adj->op == IPA_PARM_OP_COPY)
	fprintf (file, ", copy_param");
      else if (adj->op == IPA_PARM_OP_REMOVE)
	fprintf (file, ", remove_param");
      else
	{
	  fprintf (file, ", offset ");
	  print_dec (adj->offset, file);
	}
      if (adj->by_ref)
	fprintf (file, ", by_ref");
      print_node_brief (file, ", type: ", adj->type, 0);
      fprintf (file, "\n");
    }
  parms.release ();
}

