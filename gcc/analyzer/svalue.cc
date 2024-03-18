/* Symbolic values.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "bitmap.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/svalue.h"
#include "analyzer/region-model.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"

#if ENABLE_ANALYZER

namespace ana {

static int cmp_csts_and_types (const_tree cst1, const_tree cst2);

/* class svalue and its various subclasses.  */

/* class svalue.  */

/* Dump a representation of this svalue to stderr.  */

DEBUG_FUNCTION void
svalue::dump (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
  pp_flush (&pp);
}

/* Generate a textual representation of this svalue for debugging purposes.  */

label_text
svalue::get_desc (bool simple) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_to_pp (&pp, simple);
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* Return a new json::string describing the svalue.  */

json::value *
svalue::to_json () const
{
  label_text desc = get_desc (true);
  json::value *sval_js = new json::string (desc.get ());
  return sval_js;
}

/* Class for optionally adding open/close paren pairs within
   svalue::maybe_print_for_user.  */

class auto_add_parens
{
public:
  auto_add_parens (pretty_printer *pp,
		   const svalue *outer_sval,
		   const svalue &inner_sval)
  : m_pp (pp),
    m_needs_parens (needs_parens_p (outer_sval, inner_sval))
  {
    if (m_needs_parens)
      pp_string (m_pp, "(");
  }
  ~auto_add_parens ()
  {
    if (m_needs_parens)
      pp_string (m_pp, ")");
  }

private:
  static bool needs_parens_p (const svalue *outer_sval,
			      const svalue &inner_sval)
  {
    if (!outer_sval)
      return false;
    if (inner_sval.get_kind () == SK_BINOP)
      return true;
    return false;
  }

  pretty_printer *m_pp;
  bool m_needs_parens;
};

/* Attempt to print a user-facing description of this svalue to PP,
   using MODEL for extracting representative tree values if necessary.
   Use OUTER_SVAL (which can be null) to determine if we need to wrap
   this value in parentheses.  */

bool
svalue::maybe_print_for_user (pretty_printer *pp,
			      const region_model &model,
			      const svalue *outer_sval) const
{
  auto_add_parens p (pp, outer_sval, *this);

  switch (get_kind ())
    {
    default:
      break;
    case SK_CONSTANT:
      {
	const constant_svalue *sval = (const constant_svalue *)this;
	pp_printf (pp, "%E", sval->get_constant ());
	return true;
      }
    case SK_INITIAL:
      {
	const initial_svalue *sval = (const initial_svalue *)this;
	return sval->get_region ()->maybe_print_for_user (pp, model);
      }
    case SK_UNARYOP:
      {
	const unaryop_svalue *sval = (const unaryop_svalue *)this;
	if (sval->get_op () == NOP_EXPR)
	  {
	    if (!sval->get_arg ()->maybe_print_for_user (pp, model, outer_sval))
	      return false;
	    return true;
	  }
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *sval = (const binop_svalue *)this;
	switch (sval->get_op ())
	  {
	  default:
	    break;

	  case PLUS_EXPR:
	  case MINUS_EXPR:
	  case MULT_EXPR:
	    {
	      if (!sval->get_arg0 ()->maybe_print_for_user (pp, model, this))
		return false;
	      pp_printf (pp, " %s ", op_symbol_code (sval->get_op ()));
	      if (!sval->get_arg1 ()->maybe_print_for_user (pp, model, this))
		return false;
	      return true;
	    }
	  }
      }
      break;
    }

  if (tree expr = model.get_representative_tree (this))
    {
      expr = remove_ssa_names (expr);
      print_expr_for_user (pp, expr);
      return true;
    }

  return false;
}

/* If this svalue is a constant_svalue, return the underlying tree constant.
   Otherwise return NULL_TREE.  */

tree
svalue::maybe_get_constant () const
{
  const svalue *sval = unwrap_any_unmergeable ();
  if (const constant_svalue *cst_sval = sval->dyn_cast_constant_svalue ())
    return cst_sval->get_constant ();
  else
    return NULL_TREE;
}

/* If this svalue is a region_svalue, return the region it points to.
   Otherwise return NULL.  */

const region *
svalue::maybe_get_region () const
{
  if (const region_svalue *region_sval = dyn_cast_region_svalue ())
    return region_sval->get_pointee ();
  else
    return NULL;
}

/* If this svalue is a cast (i.e a unaryop NOP_EXPR or VIEW_CONVERT_EXPR),
   return the underlying svalue.
   Otherwise return NULL.  */

const svalue *
svalue::maybe_undo_cast () const
{
  if (const unaryop_svalue *unaryop_sval = dyn_cast_unaryop_svalue ())
    {
      enum tree_code op = unaryop_sval->get_op ();
      if (op == NOP_EXPR || op == VIEW_CONVERT_EXPR)
	return unaryop_sval->get_arg ();
    }
  return NULL;
}

/* If this svalue is an unmergeable decorator around another svalue, return
   the underlying svalue.
   Otherwise return this svalue.  */

const svalue *
svalue::unwrap_any_unmergeable () const
{
  if (const unmergeable_svalue *unmergeable = dyn_cast_unmergeable_svalue ())
    return unmergeable->get_arg ();
  return this;
}

/* Attempt to merge THIS with OTHER, returning the merged svalue.
   Return NULL if not mergeable.  */

const svalue *
svalue::can_merge_p (const svalue *other,
		     region_model_manager *mgr,
		     model_merger *merger) const
{
  if (!(get_type () && other->get_type ()))
    return NULL;

  if (!types_compatible_p (get_type (), other->get_type ()))
    return NULL;

  /* Reject attempts to merge unmergeable svalues.  */
  if ((get_kind () == SK_UNMERGEABLE)
      || (other->get_kind () == SK_UNMERGEABLE))
    return NULL;

  /* Reject attempts to merge poisoned svalues with other svalues
     (either non-poisoned, or other kinds of poison), so that e.g.
     we identify paths in which a variable is conditionally uninitialized.  */
  if (get_kind () == SK_POISONED
      || other->get_kind () == SK_POISONED)
    return NULL;

  /* Reject attempts to merge NULL pointers with not-NULL-pointers.  */
  if (POINTER_TYPE_P (get_type ()))
    {
      bool null0 = false;
      bool null1 = false;
      if (tree cst0 = maybe_get_constant ())
	if (zerop (cst0))
	  null0 = true;
      if (tree cst1 = other->maybe_get_constant ())
	if (zerop (cst1))
	  null1 = true;
      if (null0 != null1)
	return NULL;
    }

  /* Reject merging svalues that have non-purgable sm-state,
     to avoid falsely reporting memory leaks by merging them
     with something else.  */
  if (!merger->mergeable_svalue_p (this))
    return NULL;
  if (!merger->mergeable_svalue_p (other))
    return NULL;

  /* Widening.  */
  /* Merge: (new_cst, existing_cst) -> widen (existing, new).  */
  if (maybe_get_constant () && other->maybe_get_constant ())
    {
      return mgr->get_or_create_widening_svalue (other->get_type (),
						 merger->get_function_point (),
						 other, this);
    }

  /* Merger of:
	 this: BINOP (X, OP, CST)
	other: X, where X is non-widening
	   to: WIDENING (other, this).  */
  if (const binop_svalue *binop_sval = dyn_cast_binop_svalue ())
    if (binop_sval->get_arg0 () == other
	&& binop_sval->get_arg1 ()->get_kind () == SK_CONSTANT
	&& other->get_kind () != SK_WIDENING)
      return mgr->get_or_create_widening_svalue (other->get_type (),
						 merger->get_function_point (),
						 other, this);

  /* Merge: (Widen(existing_val, V), existing_val) -> Widen (existing_val, V)
     and thus get a fixed point.  */
  if (const widening_svalue *widen_sval = dyn_cast_widening_svalue ())
    {
      if (other == widen_sval->get_base_svalue ())
	return this;
      if (other == widen_sval->get_iter_svalue ())
	return this;
    }

  if (const binop_svalue *binop_sval = dyn_cast_binop_svalue ())
    if (const widening_svalue *widen_arg0
	= binop_sval->get_arg0 ()->dyn_cast_widening_svalue ())
      {
	if (other == binop_sval->get_arg1 ())
	  {
	    /* Merger of: (Widen(..., OTHER) BINOP X)
	       and      : OTHER
	       to       : (Widen(..., OTHER) BINOP X)
	       e.g. merge of Widen(0, 1) + 1 with 1 to the Widen(0, 1) + 1.  */
	    return this;
	  }

	/* Merger of : (Widen() BINOP X)
	   and       : Widen()
	   to        : Widen()
	   e.g. merge of Widen(0, 1) + 1 and Widen(0, 1) to Widen(0, 1).
	   However, we want to update constraints for this case, since we're
	   considering another iteration.
	   Presumably we also want to ensure that it converges; we don't want
	   a descending chain of constraints.  */
	if (other == widen_arg0)
	  {
	    merger->on_widening_reuse (widen_arg0);
	    return widen_arg0;
	  }

	/* Merger of:
	    this: BINOP(WIDENING(BASE, BINOP(BASE, X)), X)
	   other: BINOP(BASE, X)
	      to: WIDENING(BASE, BINOP(BASE, X)).  */
	if (widen_arg0->get_iter_svalue () == other)
	  if (const binop_svalue *other_binop_sval
		= other->dyn_cast_binop_svalue ())
	    if (other_binop_sval->get_arg0 () == widen_arg0->get_base_svalue ()
		&& other_binop_sval->get_arg1 () == binop_sval->get_arg1 ())
	      return widen_arg0;
      }

  return mgr->get_or_create_unknown_svalue (get_type ());
}

/* Determine if this svalue is either within LIVE_SVALUES, or is implicitly
   live with respect to LIVE_SVALUES and MODEL.
   LIVE_SVALUES can be NULL, in which case determine if this svalue is
   intrinsically live.  */

bool
svalue::live_p (const svalue_set *live_svalues,
		const region_model *model) const
{
  /* Determine if SVAL is explicitly live.  */
  if (live_svalues)
    if (const_cast<svalue_set *> (live_svalues)->contains (this))
      return true;

  /* Otherwise, determine if SVAL is implicitly live due to being made of
     other live svalues.  */
  return implicitly_live_p (live_svalues, model);
}

/* Base implementation of svalue::implicitly_live_p.  */

bool
svalue::implicitly_live_p (const svalue_set *, const region_model *) const
{
  return false;
}

/* Comparator for imposing a deterministic order on constants that are
   of the same type.  */

static int
cmp_csts_same_type (const_tree cst1, const_tree cst2)
{
  gcc_assert (TREE_TYPE (cst1) == TREE_TYPE (cst2));
  gcc_assert (TREE_CODE (cst1) == TREE_CODE (cst2));
  switch (TREE_CODE (cst1))
    {
    default:
      gcc_unreachable ();
    case INTEGER_CST:
      return tree_int_cst_compare (cst1, cst2);
    case STRING_CST:
      return strcmp (TREE_STRING_POINTER (cst1),
		     TREE_STRING_POINTER (cst2));
    case REAL_CST:
      /* Impose an arbitrary but deterministic order.  */
      return memcmp (TREE_REAL_CST_PTR (cst1),
		     TREE_REAL_CST_PTR (cst2),
		     sizeof (real_value));
    case COMPLEX_CST:
      if (int cmp_real = cmp_csts_and_types (TREE_REALPART (cst1),
					     TREE_REALPART (cst2)))
	return cmp_real;
      return cmp_csts_and_types (TREE_IMAGPART (cst1), TREE_IMAGPART (cst2));
    case VECTOR_CST:
      if (int cmp_log2_npatterns
	    = ((int)VECTOR_CST_LOG2_NPATTERNS (cst1)
	       - (int)VECTOR_CST_LOG2_NPATTERNS (cst2)))
	return cmp_log2_npatterns;
      if (int cmp_nelts_per_pattern
	    = ((int)VECTOR_CST_NELTS_PER_PATTERN (cst1)
	       - (int)VECTOR_CST_NELTS_PER_PATTERN (cst2)))
	return cmp_nelts_per_pattern;
      unsigned encoded_nelts = vector_cst_encoded_nelts (cst1);
      for (unsigned i = 0; i < encoded_nelts; i++)
	{
	  const_tree elt1 = VECTOR_CST_ENCODED_ELT (cst1, i);
	  const_tree elt2 = VECTOR_CST_ENCODED_ELT (cst2, i);
	  if (int el_cmp = cmp_csts_and_types (elt1, elt2))
	    return el_cmp;
	}
      return 0;
    }
}

/* Comparator for imposing a deterministic order on constants that might
   not be of the same type.  */

static int
cmp_csts_and_types (const_tree cst1, const_tree cst2)
{
  int t1 = TYPE_UID (TREE_TYPE (cst1));
  int t2 = TYPE_UID (TREE_TYPE (cst2));
  if (int cmp_type = t1 - t2)
    return cmp_type;
  return cmp_csts_same_type (cst1, cst2);
}

/* Comparator for imposing a deterministic order on svalues.  */

int
svalue::cmp_ptr (const svalue *sval1, const svalue *sval2)
{
  if (sval1 == sval2)
    return 0;
  if (int cmp_kind = sval1->get_kind () - sval2->get_kind ())
    return cmp_kind;
  int t1 = sval1->get_type () ? TYPE_UID (sval1->get_type ()) : -1;
  int t2 = sval2->get_type () ? TYPE_UID (sval2->get_type ()) : -1;
  if (int cmp_type = t1 - t2)
    return cmp_type;
  switch (sval1->get_kind ())
    {
    default:
      gcc_unreachable ();
    case SK_REGION:
      {
	const region_svalue *region_sval1 = (const region_svalue *)sval1;
	const region_svalue *region_sval2 = (const region_svalue *)sval2;
	return region::cmp_ids (region_sval1->get_pointee (),
				region_sval2->get_pointee ());
      }
      break;
    case SK_CONSTANT:
      {
	const constant_svalue *constant_sval1 = (const constant_svalue *)sval1;
	const constant_svalue *constant_sval2 = (const constant_svalue *)sval2;
	const_tree cst1 = constant_sval1->get_constant ();
	const_tree cst2 = constant_sval2->get_constant ();
	/* The svalues have the same type, but the underlying trees
	   might not (for the case where both svalues are typeless).  */
	return cmp_csts_and_types (cst1, cst2);
      }
      break;
    case SK_UNKNOWN:
      {
	gcc_assert (sval1 == sval2);
	return 0;
      }
      break;
    case SK_POISONED:
      {
	const poisoned_svalue *poisoned_sval1 = (const poisoned_svalue *)sval1;
	const poisoned_svalue *poisoned_sval2 = (const poisoned_svalue *)sval2;
	return (poisoned_sval1->get_poison_kind ()
		- poisoned_sval2->get_poison_kind ());
      }
      break;
    case SK_SETJMP:
      {
	const setjmp_svalue *setjmp_sval1 = (const setjmp_svalue *)sval1;
	const setjmp_svalue *setjmp_sval2 = (const setjmp_svalue *)sval2;
	const setjmp_record &rec1 = setjmp_sval1->get_setjmp_record ();
	const setjmp_record &rec2 = setjmp_sval2->get_setjmp_record ();
	return setjmp_record::cmp (rec1, rec2);
      }
      break;
    case SK_INITIAL:
      {
	const initial_svalue *initial_sval1 = (const initial_svalue *)sval1;
	const initial_svalue *initial_sval2 = (const initial_svalue *)sval2;
	return region::cmp_ids (initial_sval1->get_region (),
				initial_sval2->get_region ());
      }
      break;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_sval1 = (const unaryop_svalue *)sval1;
	const unaryop_svalue *unaryop_sval2 = (const unaryop_svalue *)sval2;
	if (int op_cmp = unaryop_sval1->get_op () - unaryop_sval2->get_op ())
	  return op_cmp;
	return svalue::cmp_ptr (unaryop_sval1->get_arg (),
				unaryop_sval2->get_arg ());
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *binop_sval1 = (const binop_svalue *)sval1;
	const binop_svalue *binop_sval2 = (const binop_svalue *)sval2;
	if (int op_cmp = binop_sval1->get_op () - binop_sval2->get_op ())
	  return op_cmp;
	if (int arg0_cmp = svalue::cmp_ptr (binop_sval1->get_arg0 (),
					    binop_sval2->get_arg0 ()))
	  return arg0_cmp;
	return svalue::cmp_ptr (binop_sval1->get_arg1 (),
				binop_sval2->get_arg1 ());
      }
      break;
    case SK_SUB:
      {
	const sub_svalue *sub_sval1 = (const sub_svalue *)sval1;
	const sub_svalue *sub_sval2 = (const sub_svalue *)sval2;
	if (int parent_cmp = svalue::cmp_ptr (sub_sval1->get_parent (),
					      sub_sval2->get_parent ()))
	  return parent_cmp;
	return region::cmp_ids (sub_sval1->get_subregion (),
				sub_sval2->get_subregion ());
      }
      break;
    case SK_REPEATED:
      {
	const repeated_svalue *repeated_sval1 = (const repeated_svalue *)sval1;
	const repeated_svalue *repeated_sval2 = (const repeated_svalue *)sval2;
	return svalue::cmp_ptr (repeated_sval1->get_inner_svalue (),
				repeated_sval2->get_inner_svalue ());
      }
      break;
    case SK_BITS_WITHIN:
      {
	const bits_within_svalue *bits_within_sval1
	  = (const bits_within_svalue *)sval1;
	const bits_within_svalue *bits_within_sval2
	  = (const bits_within_svalue *)sval2;
	if (int cmp = bit_range::cmp (bits_within_sval1->get_bits (),
				       bits_within_sval2->get_bits ()))
	  return cmp;
	return svalue::cmp_ptr (bits_within_sval1->get_inner_svalue (),
				bits_within_sval2->get_inner_svalue ());
      }
      break;
    case SK_UNMERGEABLE:
      {
	const unmergeable_svalue *unmergeable_sval1
	  = (const unmergeable_svalue *)sval1;
	const unmergeable_svalue *unmergeable_sval2
	  = (const unmergeable_svalue *)sval2;
	return svalue::cmp_ptr (unmergeable_sval1->get_arg (),
				unmergeable_sval2->get_arg ());
      }
      break;
    case SK_PLACEHOLDER:
      {
	const placeholder_svalue *placeholder_sval1
	  = (const placeholder_svalue *)sval1;
	const placeholder_svalue *placeholder_sval2
	  = (const placeholder_svalue *)sval2;
	return strcmp (placeholder_sval1->get_name (),
		       placeholder_sval2->get_name ());
      }
      break;
    case SK_WIDENING:
      {
	const widening_svalue *widening_sval1 = (const widening_svalue *)sval1;
	const widening_svalue *widening_sval2 = (const widening_svalue *)sval2;
	if (int point_cmp = function_point::cmp (widening_sval1->get_point (),
						 widening_sval2->get_point ()))
	  return point_cmp;
	if (int base_cmp = svalue::cmp_ptr (widening_sval1->get_base_svalue (),
					    widening_sval2->get_base_svalue ()))
	  return base_cmp;
	return svalue::cmp_ptr (widening_sval1->get_iter_svalue (),
				widening_sval2->get_iter_svalue ());
      }
      break;
    case SK_COMPOUND:
      {
	const compound_svalue *compound_sval1 = (const compound_svalue *)sval1;
	const compound_svalue *compound_sval2 = (const compound_svalue *)sval2;
	return binding_map::cmp (compound_sval1->get_map (),
				 compound_sval2->get_map ());
      }
      break;
    case SK_CONJURED:
      {
	const conjured_svalue *conjured_sval1 = (const conjured_svalue *)sval1;
	const conjured_svalue *conjured_sval2 = (const conjured_svalue *)sval2;
	if (int stmt_cmp = (conjured_sval1->get_stmt ()->uid
			    - conjured_sval2->get_stmt ()->uid))
	  return stmt_cmp;
	return region::cmp_ids (conjured_sval1->get_id_region (),
				conjured_sval2->get_id_region ());
      }
      break;
    case SK_ASM_OUTPUT:
      {
	const asm_output_svalue *asm_output_sval1
	  = (const asm_output_svalue *)sval1;
	const asm_output_svalue *asm_output_sval2
	  = (const asm_output_svalue *)sval2;
	if (int asm_string_cmp = strcmp (asm_output_sval1->get_asm_string (),
					 asm_output_sval2->get_asm_string ()))
	  return asm_string_cmp;
	if (int output_idx_cmp = ((int)asm_output_sval1->get_output_idx ()
				  - (int)asm_output_sval2->get_output_idx ()))
	  return output_idx_cmp;
	if (int cmp = ((int)asm_output_sval1->get_num_inputs ()
		       - (int)asm_output_sval2->get_num_inputs ()))
	  return cmp;
	for (unsigned i = 0; i < asm_output_sval1->get_num_inputs (); i++)
	  if (int input_cmp
	      = svalue::cmp_ptr (asm_output_sval1->get_input (i),
				 asm_output_sval2->get_input (i)))
	    return input_cmp;
	return 0;
      }
      break;
    case SK_CONST_FN_RESULT:
      {
	const const_fn_result_svalue *const_fn_result_sval1
	  = (const const_fn_result_svalue *)sval1;
	const const_fn_result_svalue *const_fn_result_sval2
	  = (const const_fn_result_svalue *)sval2;
	int d1 = DECL_UID (const_fn_result_sval1->get_fndecl ());
	int d2 = DECL_UID (const_fn_result_sval2->get_fndecl ());
	if (int cmp_fndecl = d1 - d2)
	  return cmp_fndecl;
	if (int cmp = ((int)const_fn_result_sval1->get_num_inputs ()
		       - (int)const_fn_result_sval2->get_num_inputs ()))
	  return cmp;
	for (unsigned i = 0; i < const_fn_result_sval1->get_num_inputs (); i++)
	  if (int input_cmp
	      = svalue::cmp_ptr (const_fn_result_sval1->get_input (i),
				 const_fn_result_sval2->get_input (i)))
	    return input_cmp;
	return 0;
      }
    }
}

/* Comparator for use by vec<const svalue *>::qsort.  */

int
svalue::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const svalue *sval1 = *(const svalue * const *)p1;
  const svalue *sval2 = *(const svalue * const *)p2;
  return cmp_ptr (sval1, sval2);
}

/* Subclass of visitor for use in implementing svalue::involves_p.  */

class involvement_visitor : public visitor
{
public:
  involvement_visitor (const svalue *needle)
  : m_needle (needle), m_found (false) {}

  void visit_initial_svalue (const initial_svalue *candidate) final override
  {
    if (candidate == m_needle)
      m_found = true;
  }

  void visit_conjured_svalue (const conjured_svalue *candidate) final override
  {
    if (candidate == m_needle)
      m_found = true;
  }

  void visit_widening_svalue (const widening_svalue *candidate) final override
  {
    if (candidate == m_needle)
      m_found = true;
  }

  bool found_p () const { return m_found; }

private:
  const svalue *m_needle;
  bool m_found;
};

/* Return true iff this svalue is defined in terms of OTHER.  */

bool
svalue::involves_p (const svalue *other) const
{
  /* Currently only implemented for these kinds.  */
  gcc_assert (other->get_kind () == SK_INITIAL
	      || other->get_kind () == SK_CONJURED
	      || other->get_kind () == SK_WIDENING);

  involvement_visitor v (other);
  accept (&v);
  return v.found_p ();
}

/* Extract SUBRANGE from this value, of type TYPE.  */

const svalue *
svalue::extract_bit_range (tree type,
			   const bit_range &subrange,
			   region_model_manager *mgr) const
{
  return mgr->get_or_create_bits_within (type, subrange, this);
}

/* Base implementation of svalue::maybe_fold_bits_within vfunc.  */

const svalue *
svalue::maybe_fold_bits_within (tree,
				const bit_range &,
				region_model_manager *) const
{
  /* By default, don't fold.  */
  return NULL;
}

/* Base implementation of svalue::all_zeroes_p.
   Return true if this value is known to be all zeroes.  */

bool
svalue::all_zeroes_p () const
{
  return false;
}

/* If this svalue is a pointer, attempt to determine the base region it points
   to.  Return NULL on any problems.  */

const region *
svalue::maybe_get_deref_base_region () const
{
  const svalue *iter = this;
  while (1)
    {
      switch (iter->get_kind ())
	{
	default:
	  return NULL;

	case SK_REGION:
	  {
	    const region_svalue *region_sval
	      = as_a <const region_svalue *> (iter);
	    return region_sval->get_pointee ()->get_base_region ();
	  }

	case SK_BINOP:
	  {
	    const binop_svalue *binop_sval
	      = as_a <const binop_svalue *> (iter);
	    switch (binop_sval->get_op ())
	      {
	      case POINTER_PLUS_EXPR:
		/* If we have a symbolic value expressing pointer arithmetic,
		   use the LHS.  */
		iter = binop_sval->get_arg0 ();
		continue;

	      default:
		return NULL;
	      }
	    return NULL;
	  }
	}
    }
}

/* class region_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for region_svalue.  */

void
region_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "&");
      m_reg->dump_to_pp (pp, simple);
    }
  else
    {
      pp_string (pp, "region_svalue(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      m_reg->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
}

/* Implementation of svalue::accept vfunc for region_svalue.  */

void
region_svalue::accept (visitor *v) const
{
  m_reg->accept (v);
  v->visit_region_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for region_svalue.  */

bool
region_svalue::implicitly_live_p (const svalue_set *,
				  const region_model *model) const
{
  /* Pointers into clusters that have escaped should be treated as live.  */
  const region *base_reg = get_pointee ()->get_base_region ();
  const store *store = model->get_store ();
  if (const binding_cluster *c = store->get_cluster (base_reg))
    if (c->escaped_p ())
	return true;

  return false;
}

/* Evaluate the condition LHS OP RHS.
   Subroutine of region_model::eval_condition for when we have a pair of
   pointers.  */

tristate
region_svalue::eval_condition (const region_svalue *lhs,
			       enum tree_code op,
			       const region_svalue *rhs)
{
  /* See if they point to the same region.  */
  const region *lhs_reg = lhs->get_pointee ();
  const region *rhs_reg = rhs->get_pointee ();
  bool ptr_equality = lhs_reg == rhs_reg;
  switch (op)
    {
    default:
      gcc_unreachable ();

    case EQ_EXPR:
      if (ptr_equality)
	return tristate::TS_TRUE;
      else
	return tristate::TS_FALSE;
      break;

    case NE_EXPR:
      if (ptr_equality)
	return tristate::TS_FALSE;
      else
	return tristate::TS_TRUE;
      break;

    case GE_EXPR:
    case LE_EXPR:
      if (ptr_equality)
	return tristate::TS_TRUE;
      break;

    case GT_EXPR:
    case LT_EXPR:
      if (ptr_equality)
	return tristate::TS_FALSE;
      break;
    }

  return tristate::TS_UNKNOWN;
}

/* class constant_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for constant_svalue.  */

void
constant_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "(");
      dump_tree (pp, get_type ());
      pp_string (pp, ")");
      dump_tree (pp, m_cst_expr);
    }
  else
    {
      pp_string (pp, "constant_svalue(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      dump_tree (pp, m_cst_expr);
      pp_string (pp, ")");
    }
}

/* Implementation of svalue::accept vfunc for constant_svalue.  */

void
constant_svalue::accept (visitor *v) const
{
  v->visit_constant_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for constant_svalue.
   Constants are implicitly live.  */

bool
constant_svalue::implicitly_live_p (const svalue_set *,
				    const region_model *) const
{
  return true;
}

/* Given EXPR, a non-NULL expression of boolean type, convert to
   a tristate based on whether this is known to be true, false,
   or is not known.  */

static tristate
tristate_from_boolean_tree_node (tree expr)
{
  gcc_assert (TREE_TYPE (expr) == boolean_type_node);

  if (expr == boolean_true_node)
    return tristate (tristate::TS_TRUE);
  else if (expr == boolean_false_node)
    return tristate (tristate::TS_FALSE);
  else
    return tristate (tristate::TS_UNKNOWN);
}

/* Evaluate the condition LHS OP RHS.
   Subroutine of region_model::eval_condition for when we have a pair of
   constants.  */

tristate
constant_svalue::eval_condition (const constant_svalue *lhs,
				 enum tree_code op,
				 const constant_svalue *rhs)
{
  tree lhs_const = lhs->get_constant ();
  tree rhs_const = rhs->get_constant ();

  gcc_assert (CONSTANT_CLASS_P (lhs_const));
  gcc_assert (CONSTANT_CLASS_P (rhs_const));

  if ((lhs->get_type () == NULL_TREE || rhs->get_type () == NULL_TREE)
      && TREE_CODE (lhs_const) == INTEGER_CST
      && TREE_CODE (rhs_const) == INTEGER_CST
      )
    {
     if (tree tree_cmp = const_binop (op, boolean_type_node,
				      lhs_const, rhs_const))
       {
	 tristate ts = tristate_from_boolean_tree_node (tree_cmp);
	 if (ts.is_known ())
	   return ts;
       }
    }

  /* Check for comparable types.  */
  if (types_compatible_p (TREE_TYPE (lhs_const), TREE_TYPE (rhs_const)))
    {
      tree tree_cmp
	= fold_binary (op, boolean_type_node, lhs_const, rhs_const);
      tristate ts = tristate_from_boolean_tree_node (tree_cmp);
      if (ts.is_known ())
	return ts;
    }
  return tristate::TS_UNKNOWN;
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for constant_svalue.  */

const svalue *
constant_svalue::maybe_fold_bits_within (tree type,
					 const bit_range &bits,
					 region_model_manager *mgr) const
{
  /* Bits within an all-zero value are also all zero.  */
  if (zerop (m_cst_expr))
    {
      if (type)
	return mgr->get_or_create_cast (type, this);
      else
	return this;
    }

  /* Handle the case of extracting a single bit. */
  if (bits.m_size_in_bits == 1
      && TREE_CODE (m_cst_expr) == INTEGER_CST
      && type
      && INTEGRAL_TYPE_P (type)
      && tree_fits_uhwi_p (m_cst_expr))
    {
      unsigned HOST_WIDE_INT bit = bits.m_start_bit_offset.to_uhwi ();
      unsigned HOST_WIDE_INT mask = (1 << bit);
      unsigned HOST_WIDE_INT val_as_hwi = tree_to_uhwi (m_cst_expr);
      unsigned HOST_WIDE_INT masked_val = val_as_hwi & mask;
      int result = masked_val ? 1 : 0;
      return mgr->get_or_create_int_cst (type, result);
    }

  /* Otherwise, don't fold.  */
  return NULL;
}

/* Implementation of svalue::all_zeroes_p for constant_svalue.  */

bool
constant_svalue::all_zeroes_p () const
{
  return zerop (m_cst_expr);
}

/* class unknown_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for unknown_svalue.  */

void
unknown_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "UNKNOWN(");
      if (get_type ())
	dump_tree (pp, get_type ());
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "unknown_svalue(");
      if (get_type ())
	dump_tree (pp, get_type ());
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for unknown_svalue.  */

void
unknown_svalue::accept (visitor *v) const
{
  v->visit_unknown_svalue (this);
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for unknown_svalue.  */

const svalue *
unknown_svalue::maybe_fold_bits_within (tree type,
					const bit_range &,
					region_model_manager *mgr) const
{
  /* Bits within an unknown_svalue are themselves unknown.  */
  return mgr->get_or_create_unknown_svalue (type);
}

/* Get a string for KIND for use in debug dumps.  */

const char *
poison_kind_to_str (enum poison_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case POISON_KIND_UNINIT:
      return "uninit";
    case POISON_KIND_FREED:
      return "freed";
    case POISON_KIND_DELETED:
      return "deleted";
    case POISON_KIND_POPPED_STACK:
      return "popped stack";
    }
}

/* class poisoned_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for poisoned_svalue.  */

void
poisoned_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "POISONED(");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ", %s)", poison_kind_to_str (m_kind));
    }
  else
    {
      pp_string (pp, "poisoned_svalue(");
      print_quoted_type (pp, get_type ());
      pp_printf (pp, ", %s)", poison_kind_to_str (m_kind));
    }
}

/* Implementation of svalue::accept vfunc for poisoned_svalue.  */

void
poisoned_svalue::accept (visitor *v) const
{
  v->visit_poisoned_svalue (this);
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for poisoned_svalue.  */

const svalue *
poisoned_svalue::maybe_fold_bits_within (tree type,
					 const bit_range &,
					 region_model_manager *mgr) const
{
  /* Bits within a poisoned value are also poisoned.  */
  return mgr->get_or_create_poisoned_svalue (m_kind, type);
}

/* class setjmp_svalue's implementation is in engine.cc, so that it can use
   the declaration of exploded_node.  */

/* class initial_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for initial_svalue.  */

void
initial_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "INIT_VAL(");
      m_reg->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
  else
    {
      pp_string (pp, "initial_svalue(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      m_reg->dump_to_pp (pp, simple);
      pp_string (pp, ")");
    }
}

/* Implementation of svalue::accept vfunc for initial_svalue.  */

void
initial_svalue::accept (visitor *v) const
{
  m_reg->accept (v);
  v->visit_initial_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for initial_svalue.  */

bool
initial_svalue::implicitly_live_p (const svalue_set *,
				   const region_model *model) const
{
  /* This svalue may be implicitly live if the region still implicitly
     has its initial value and is reachable.  */

  /* It must be a region that exists; we don't want to consider
     INIT_VAL(R) as still being implicitly reachable if R is in
     a popped stack frame.  */
  if (model->region_exists_p (m_reg))
    {
      const svalue *reg_sval = model->get_store_value (m_reg, NULL);
      if (reg_sval == this)
	return true;
    }

  /* Assume that the initial values of params for the top level frame
     are still live, because (presumably) they're still
     live in the external caller.  */
  if (initial_value_of_param_p ())
    if (const frame_region *frame_reg = m_reg->maybe_get_frame_region ())
      if (frame_reg->get_calling_frame () == NULL)
	return true;

  return false;
}

/* Return true if this is the initial value of a function parameter.  */

bool
initial_svalue::initial_value_of_param_p () const
{
  if (tree reg_decl = m_reg->maybe_get_decl ())
    if (TREE_CODE (reg_decl) == SSA_NAME)
      {
	tree ssa_name = reg_decl;
	if (SSA_NAME_IS_DEFAULT_DEF (ssa_name)
	    && SSA_NAME_VAR (ssa_name)
	    && TREE_CODE (SSA_NAME_VAR (ssa_name)) == PARM_DECL)
	  return true;
      }
  return false;
}

/* class unaryop_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for unaryop_svalue.  */

void
unaryop_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      if (m_op == VIEW_CONVERT_EXPR || m_op == NOP_EXPR)
	{
	  pp_string (pp, "CAST(");
	  dump_tree (pp, get_type ());
	  pp_string (pp, ", ");
	  m_arg->dump_to_pp (pp, simple);
	  pp_character (pp, ')');
	}
      else
	{
	  pp_character (pp, '(');
	  pp_string (pp, get_tree_code_name (m_op));
	  //pp_string (pp, op_symbol_code (m_op));
	  m_arg->dump_to_pp (pp, simple);
	  pp_character (pp, ')');
	}
    }
  else
    {
      pp_string (pp, "unaryop_svalue (");
      pp_string (pp, get_tree_code_name (m_op));
      pp_string (pp, ", ");
      m_arg->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for unaryop_svalue.  */

void
unaryop_svalue::accept (visitor *v) const
{
  m_arg->accept (v);
  v->visit_unaryop_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for unaryop_svalue.  */

bool
unaryop_svalue::implicitly_live_p (const svalue_set *live_svalues,
				   const region_model *model) const
{
  return get_arg ()->live_p (live_svalues, model);
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for unaryop_svalue.  */

const svalue *
unaryop_svalue::maybe_fold_bits_within (tree type,
					const bit_range &,
					region_model_manager *mgr) const
{
  switch (m_op)
    {
    default:
      break;
    case NOP_EXPR:
      /* A cast of zero is zero.  */
      if (tree cst = m_arg->maybe_get_constant ())
	if (zerop (cst))
	  {
	    if (type)
	      return mgr->get_or_create_cast (type, this);
	    else
	      return this;
	  }
      break;
    }
  /* Otherwise, don't fold.  */
  return NULL;
}

/* class binop_svalue : public svalue.  */

/* Return whether OP be printed as an infix operator.  */

static bool
infix_p (enum tree_code op)
{
  switch (op)
    {
    default:
      return true;
    case MAX_EXPR:
    case MIN_EXPR:
      return false;
    }
}

/* Implementation of svalue::dump_to_pp vfunc for binop_svalue.  */

void
binop_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      if (infix_p (m_op))
	{
	  /* Print "(A OP B)".  */
	  pp_character (pp, '(');
	  m_arg0->dump_to_pp (pp, simple);
	  pp_string (pp, op_symbol_code (m_op));
	  m_arg1->dump_to_pp (pp, simple);
	  pp_character (pp, ')');
	}
      else
	{
	  /* Print "OP(A, B)".  */
	  pp_string (pp, op_symbol_code (m_op));
	  pp_character (pp, '(');
	  m_arg0->dump_to_pp (pp, simple);
	  pp_string (pp, ", ");
	  m_arg1->dump_to_pp (pp, simple);
	  pp_character (pp, ')');
	}
    }
  else
    {
      pp_string (pp, "binop_svalue (");
      pp_string (pp, get_tree_code_name (m_op));
      pp_string (pp, ", ");
      m_arg0->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_arg1->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for binop_svalue.  */

void
binop_svalue::accept (visitor *v) const
{
  m_arg0->accept (v);
  m_arg1->accept (v);
  v->visit_binop_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for binop_svalue.  */

bool
binop_svalue::implicitly_live_p (const svalue_set *live_svalues,
				 const region_model *model) const
{
  return (get_arg0 ()->live_p (live_svalues, model)
	  && get_arg1 ()->live_p (live_svalues, model));
}

/* class sub_svalue : public svalue.  */

/* sub_svalue'c ctor.  */

sub_svalue::sub_svalue (symbol::id_t id,
			tree type, const svalue *parent_svalue,
			const region *subregion)
: svalue (complexity::from_pair (parent_svalue->get_complexity (),
				 subregion->get_complexity ()),
	  id,
	  type),
  m_parent_svalue (parent_svalue), m_subregion (subregion)
{
  gcc_assert (parent_svalue->can_have_associated_state_p ());
}

/* Implementation of svalue::dump_to_pp vfunc for sub_svalue.  */

void
sub_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "SUB(");
      m_parent_svalue->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_subregion->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "sub_svalue (");
      pp_string (pp, ", ");
      m_parent_svalue->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_subregion->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for sub_svalue.  */

void
sub_svalue::accept (visitor *v) const
{
  m_parent_svalue->accept (v);
  m_subregion->accept (v);
  v->visit_sub_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for sub_svalue.  */

bool
sub_svalue::implicitly_live_p (const svalue_set *live_svalues,
			       const region_model *model) const
{
  return get_parent ()->live_p (live_svalues, model);
}

/* class repeated_svalue : public svalue.  */

/* repeated_svalue'c ctor.  */

repeated_svalue::repeated_svalue (symbol::id_t id,
				  tree type,
				  const svalue *outer_size,
				  const svalue *inner_svalue)
: svalue (complexity::from_pair (outer_size, inner_svalue), id, type),
  m_outer_size (outer_size),
  m_inner_svalue (inner_svalue)
{
  gcc_assert (outer_size->can_have_associated_state_p ());
  gcc_assert (inner_svalue->can_have_associated_state_p ());
}

/* Implementation of svalue::dump_to_pp vfunc for repeated_svalue.  */

void
repeated_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "REPEATED(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_string (pp, "outer_size: ");
      m_outer_size->dump_to_pp (pp, simple);
      pp_string (pp, ", inner_val: ");
      m_inner_svalue->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "repeated_svalue (");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_string (pp, "outer_size: ");
      m_outer_size->dump_to_pp (pp, simple);
      pp_string (pp, ", inner_val: ");
      m_inner_svalue->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for repeated_svalue.  */

void
repeated_svalue::accept (visitor *v) const
{
  m_inner_svalue->accept (v);
  v->visit_repeated_svalue (this);
}

/* Implementation of svalue::all_zeroes_p for repeated_svalue.  */

bool
repeated_svalue::all_zeroes_p () const
{
  return m_inner_svalue->all_zeroes_p ();
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for repeated_svalue.  */

const svalue *
repeated_svalue::maybe_fold_bits_within (tree type,
					 const bit_range &bits,
					 region_model_manager *mgr) const
{
  const svalue *innermost_sval = m_inner_svalue;
  /* Fold
       BITS_WITHIN (range, REPEATED_SVALUE (ZERO))
     to:
       REPEATED_SVALUE (ZERO).  */
  if (all_zeroes_p ())
    {
      byte_range bytes (0,0);
      if (bits.as_byte_range (&bytes))
	{
	  const svalue *byte_size
	    = mgr->get_or_create_int_cst (size_type_node,
					  bytes.m_size_in_bytes.to_uhwi ());
	  return mgr->get_or_create_repeated_svalue (type, byte_size,
						     innermost_sval);
	}
    }

  /* Fold:
       BITS_WITHIN (range, REPEATED_SVALUE (INNERMOST_SVALUE))
     to:
       BITS_WITHIN (range - offset, INNERMOST_SVALUE)
     if range is fully within one instance of INNERMOST_SVALUE.  */
  if (tree innermost_type = innermost_sval->get_type ())
    {
      bit_size_t element_bit_size;
      if (int_size_in_bits (innermost_type, &element_bit_size)
	  && element_bit_size > 0)
	{
	  HOST_WIDE_INT start_idx
	    = (bits.get_start_bit_offset ()
	       / element_bit_size).to_shwi ();
	  HOST_WIDE_INT last_idx
	    = (bits.get_last_bit_offset ()
	       / element_bit_size).to_shwi ();
	  if (start_idx == last_idx)
	    {
	      bit_offset_t start_of_element
		= start_idx * element_bit_size;
	      bit_range range_within_element
		(bits.m_start_bit_offset - start_of_element,
		 bits.m_size_in_bits);
	      return mgr->get_or_create_bits_within (type,
						     range_within_element,
						     innermost_sval);
	    }
	}
    }

  return NULL;
}

/* class bits_within_svalue : public svalue.  */

/* bits_within_svalue'c ctor.  */

bits_within_svalue::bits_within_svalue (symbol::id_t id,
					tree type,
					const bit_range &bits,
					const svalue *inner_svalue)
: svalue (complexity (inner_svalue), id, type),
  m_bits (bits),
  m_inner_svalue (inner_svalue)
{
  gcc_assert (inner_svalue->can_have_associated_state_p ());
}

/* Implementation of svalue::dump_to_pp vfunc for bits_within_svalue.  */

void
bits_within_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "BITS_WITHIN(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      m_bits.dump_to_pp (pp);
      pp_string (pp, ", inner_val: ");
      m_inner_svalue->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "bits_within_svalue (");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      m_bits.dump_to_pp (pp);
      pp_string (pp, ", inner_val: ");
      m_inner_svalue->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for bits_within_svalue.  */

const svalue *
bits_within_svalue::maybe_fold_bits_within (tree type,
					    const bit_range &bits,
					    region_model_manager *mgr) const
{
  /* Fold:
       BITS_WITHIN (range1, BITS_WITHIN (range2, VAL))
     to:
       BITS_WITHIN (range1 in range 2, VAL).  */
  bit_range offset_bits (m_bits.get_start_bit_offset ()
			 + bits.m_start_bit_offset,
			 bits.m_size_in_bits);
  return mgr->get_or_create_bits_within (type, offset_bits, m_inner_svalue);
}

/* Implementation of svalue::accept vfunc for bits_within_svalue.  */

void
bits_within_svalue::accept (visitor *v) const
{
  m_inner_svalue->accept (v);
  v->visit_bits_within_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for bits_within_svalue.  */

bool
bits_within_svalue::implicitly_live_p (const svalue_set *live_svalues,
				       const region_model *model) const
{
  return m_inner_svalue->live_p (live_svalues, model);
}

/* class widening_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for widening_svalue.  */

void
widening_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "WIDENING(");
      pp_character (pp, '{');
      m_point.print (pp, format (false));
      pp_string (pp, "}, ");
      m_base_sval->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_iter_sval->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "widening_svalue (");
      pp_string (pp, ", ");
      pp_character (pp, '{');
      m_point.print (pp, format (false));
      pp_string (pp, "}, ");
      m_base_sval->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_iter_sval->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for widening_svalue.  */

void
widening_svalue::accept (visitor *v) const
{
  m_base_sval->accept (v);
  m_iter_sval->accept (v);
  v->visit_widening_svalue (this);
}

/* Attempt to determine in which direction this value is changing
   w.r.t. the initial value.  */

enum widening_svalue::direction_t
widening_svalue::get_direction () const
{
  tree base_cst = m_base_sval->maybe_get_constant ();
  if (base_cst == NULL_TREE)
    return DIR_UNKNOWN;
  tree iter_cst = m_iter_sval->maybe_get_constant ();
  if (iter_cst == NULL_TREE)
    return DIR_UNKNOWN;

  tree iter_gt_base = fold_binary (GT_EXPR, boolean_type_node,
				   iter_cst, base_cst);
  if (iter_gt_base == boolean_true_node)
    return DIR_ASCENDING;

  tree iter_lt_base = fold_binary (LT_EXPR, boolean_type_node,
				   iter_cst, base_cst);
  if (iter_lt_base == boolean_true_node)
    return DIR_DESCENDING;

  return DIR_UNKNOWN;
}

/* Compare this value against constant RHS_CST.  */

tristate
widening_svalue::eval_condition_without_cm (enum tree_code op,
					    tree rhs_cst) const
{
  tree base_cst = m_base_sval->maybe_get_constant ();
  if (base_cst == NULL_TREE)
    return tristate::TS_UNKNOWN;
  tree iter_cst = m_iter_sval->maybe_get_constant ();
  if (iter_cst == NULL_TREE)
    return tristate::TS_UNKNOWN;

  switch (get_direction ())
    {
    default:
      gcc_unreachable ();
    case DIR_ASCENDING:
      /* LHS is in [base_cst, +ve infinity), assuming no overflow.  */
      switch (op)
	{
	case LE_EXPR:
	case LT_EXPR:
	  {
	    /* [BASE, +INF) OP RHS:
	       This is either true or false at +ve ininity,
	       It can be true for points X where X OP RHS, so we have either
	       "false", or "unknown".  */
	    tree base_op_rhs = fold_binary (op, boolean_type_node,
					    base_cst, rhs_cst);
	    if (base_op_rhs == boolean_true_node)
	      return tristate::TS_UNKNOWN;
	    else
	      return tristate::TS_FALSE;
	  }

	case GE_EXPR:
	case GT_EXPR:
	  {
	    /* [BASE, +INF) OP RHS:
	       This is true at +ve infinity.  It will be true everywhere
	       in the range if BASE >= RHS.  */
	    tree base_op_rhs = fold_binary (op, boolean_type_node,
					    base_cst, rhs_cst);
	    if (base_op_rhs == boolean_true_node)
	      return tristate::TS_TRUE;
	    else
	      return tristate::TS_UNKNOWN;
	  }

	case EQ_EXPR:
	  {
	    /* [BASE, +INF) == RHS:
	       Could this be true at any point in the range?  If so we
	       have "unknown", otherwise we have "false".  */
	    tree base_le_rhs = fold_binary (LE_EXPR, boolean_type_node,
					    base_cst, rhs_cst);
	    if (base_le_rhs == boolean_true_node)
	      return tristate::TS_UNKNOWN;
	    else
	      return tristate::TS_FALSE;
	  }

	case NE_EXPR:
	  {
	    /* [BASE, +INF) != RHS:
	       Could we have equality at any point in the range?  If so we
	       have "unknown", otherwise we have "true".  */
	    tree base_le_rhs = fold_binary (LE_EXPR, boolean_type_node,
					    base_cst, rhs_cst);
	    if (base_le_rhs == boolean_true_node)
	      return tristate::TS_UNKNOWN;
	    else
	      return tristate::TS_TRUE;
	  }

	default:
	  return tristate::TS_UNKNOWN;
	}

    case DIR_DESCENDING:
      /* LHS is in (-ve infinity, base_cst], assuming no overflow.  */
      return tristate::TS_UNKNOWN;

    case DIR_UNKNOWN:
      return tristate::TS_UNKNOWN;
    }
}

/* class placeholder_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for placeholder_svalue.  */

void
placeholder_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "PLACEHOLDER(%qs)", m_name);
  else
    pp_printf (pp, "placeholder_svalue (%qs)", m_name);
}

/* Implementation of svalue::accept vfunc for placeholder_svalue.  */

void
placeholder_svalue::accept (visitor *v) const
{
  v->visit_placeholder_svalue (this);
}

/* class unmergeable_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for unmergeable_svalue.  */

void
unmergeable_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "UNMERGEABLE(");
      m_arg->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "unmergeable_svalue (");
      m_arg->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for unmergeable_svalue.  */

void
unmergeable_svalue::accept (visitor *v) const
{
  m_arg->accept (v);
  v->visit_unmergeable_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for unmergeable_svalue.  */

bool
unmergeable_svalue::implicitly_live_p (const svalue_set *live_svalues,
				       const region_model *model) const
{
  return get_arg ()->live_p (live_svalues, model);
}

/* class compound_svalue : public svalue.  */

compound_svalue::compound_svalue (symbol::id_t id,
				  tree type,
				  const binding_map &map)
: svalue (calc_complexity (map), id, type), m_map (map)
{
#if CHECKING_P
  for (iterator_t iter = begin (); iter != end (); ++iter)
    {
      /* All keys within the underlying binding_map are required to be concrete,
	 not symbolic.  */
      const binding_key *key = (*iter).first;
      gcc_assert (key->concrete_p ());

      /* We don't nest compound svalues.  */
      const svalue *sval = (*iter).second;
      gcc_assert (sval->get_kind () != SK_COMPOUND);
    }
#endif
}

/* Implementation of svalue::dump_to_pp vfunc for compound_svalue.  */

void
compound_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "COMPOUND(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_character (pp, '{');
      m_map.dump_to_pp (pp, simple, false);
      pp_string (pp, "})");
    }
  else
    {
      pp_string (pp, "compound_svalue (");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_character (pp, '{');
      m_map.dump_to_pp (pp, simple, false);
      pp_string (pp, "})");
    }
}

/* Implementation of svalue::accept vfunc for compound_svalue.  */

void
compound_svalue::accept (visitor *v) const
{
  for (binding_map::iterator_t iter = m_map.begin ();
       iter != m_map.end (); ++iter)
    {
      //(*iter).first.accept (v);
      (*iter).second->accept (v);
    }
  v->visit_compound_svalue (this);
}

/* Calculate what the complexity of a compound_svalue instance for MAP
   will be, based on the svalues bound within MAP.  */

complexity
compound_svalue::calc_complexity (const binding_map &map)
{
  unsigned num_child_nodes = 0;
  unsigned max_child_depth = 0;
  for (binding_map::iterator_t iter = map.begin ();
       iter != map.end (); ++iter)
    {
      const complexity &sval_c = (*iter).second->get_complexity ();
      num_child_nodes += sval_c.m_num_nodes;
      max_child_depth = MAX (max_child_depth, sval_c.m_max_depth);
    }
  return complexity (num_child_nodes + 1, max_child_depth + 1);
}

/* Implementation of svalue::maybe_fold_bits_within vfunc
   for compound_svalue.  */

const svalue *
compound_svalue::maybe_fold_bits_within (tree type,
					 const bit_range &bits,
					 region_model_manager *mgr) const
{
  binding_map result_map;
  for (auto iter : m_map)
    {
      const binding_key *key = iter.first;
      if (const concrete_binding *conc_key
	  = key->dyn_cast_concrete_binding ())
	{
	  /* Ignore concrete bindings outside BITS.  */
	  if (!conc_key->get_bit_range ().intersects_p (bits))
	    continue;

	  const svalue *sval = iter.second;
	  /* Get the position of conc_key relative to BITS.  */
	  bit_range result_location (conc_key->get_start_bit_offset ()
				     - bits.get_start_bit_offset (),
				     conc_key->get_size_in_bits ());
	  /* If conc_key starts after BITS, trim off leading bits
	     from the svalue and adjust binding location.  */
	  if (result_location.m_start_bit_offset < 0)
	    {
	      bit_size_t leading_bits_to_drop
		= -result_location.m_start_bit_offset;
	      result_location = bit_range
		(0, result_location.m_size_in_bits - leading_bits_to_drop);
	      bit_range bits_within_sval (leading_bits_to_drop,
					  result_location.m_size_in_bits);
	      /* Trim off leading bits from iter_sval.  */
	      sval = mgr->get_or_create_bits_within (NULL_TREE,
						     bits_within_sval,
						     sval);
	    }
	  /* If conc_key finishes after BITS, trim off trailing bits
	     from the svalue and adjust binding location.  */
	  if (conc_key->get_next_bit_offset ()
	      > bits.get_next_bit_offset ())
	    {
	      bit_size_t trailing_bits_to_drop
		= (conc_key->get_next_bit_offset ()
		   - bits.get_next_bit_offset ());
	      result_location = bit_range
		(result_location.m_start_bit_offset,
		 result_location.m_size_in_bits - trailing_bits_to_drop);
	      bit_range bits_within_sval (0,
					  result_location.m_size_in_bits);
	      /* Trim off leading bits from iter_sval.  */
	      sval = mgr->get_or_create_bits_within (NULL_TREE,
						     bits_within_sval,
						     sval);
	    }
	  const concrete_binding *offset_conc_key
	    = mgr->get_store_manager ()->get_concrete_binding
		(result_location);
	  result_map.put (offset_conc_key, sval);
	}
      else
	/* If we have any symbolic keys we can't get it as bits.  */
	return NULL;
    }
  return mgr->get_or_create_compound_svalue (type, result_map);
}

/* class conjured_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for conjured_svalue.  */

void
conjured_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "CONJURED(");
      pp_gimple_stmt_1 (pp, m_stmt, 0, (dump_flags_t)0);
      pp_string (pp, ", ");
      m_id_reg->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "conjured_svalue (");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_gimple_stmt_1 (pp, m_stmt, 0, (dump_flags_t)0);
      pp_string (pp, ", ");
      m_id_reg->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::accept vfunc for conjured_svalue.  */

void
conjured_svalue::accept (visitor *v) const
{
  m_id_reg->accept (v);
  v->visit_conjured_svalue (this);
}

/* Return true iff this conjured_svalue is for the LHS of the
   stmt that conjured it.  */

bool
conjured_svalue::lhs_value_p () const
{
  if (tree decl = m_id_reg->maybe_get_decl ())
    return decl == gimple_get_lhs (m_stmt);
  return false;
}

/* class asm_output_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for asm_output_svalue.  */

void
asm_output_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_printf (pp, "ASM_OUTPUT(%qs, %%%i, {",
		 get_asm_string (),
		 get_output_idx ());
      for (unsigned i = 0; i < m_num_inputs; i++)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  dump_input (pp, 0, m_input_arr[i], simple);
	}
      pp_string (pp, "})");
    }
  else
    {
      pp_printf (pp, "asm_output_svalue (%qs, %%%i, {",
		 get_asm_string (),
		 get_output_idx ());
      for (unsigned i = 0; i < m_num_inputs; i++)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  dump_input (pp, 0, m_input_arr[i], simple);
	}
      pp_string (pp, "})");
    }
}

/* Subroutine of asm_output_svalue::dump_to_pp.  */

void
asm_output_svalue::dump_input (pretty_printer *pp,
			       unsigned input_idx,
			       const svalue *sval,
			       bool simple) const
{
  pp_printf (pp, "%%%i: ", input_idx_to_asm_idx (input_idx));
  sval->dump_to_pp (pp, simple);
}

/* Convert INPUT_IDX from an index into the array of inputs
   into the index of all operands for the asm stmt.  */

unsigned
asm_output_svalue::input_idx_to_asm_idx (unsigned input_idx) const
{
  return input_idx + m_num_outputs;
}

/* Implementation of svalue::accept vfunc for asm_output_svalue.  */

void
asm_output_svalue::accept (visitor *v) const
{
  for (unsigned i = 0; i < m_num_inputs; i++)
    m_input_arr[i]->accept (v);
  v->visit_asm_output_svalue (this);
}

/* class const_fn_result_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for const_fn_result_svalue.  */

void
const_fn_result_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_printf (pp, "CONST_FN_RESULT(%qD, {", m_fndecl);
      for (unsigned i = 0; i < m_num_inputs; i++)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  dump_input (pp, i, m_input_arr[i], simple);
	}
      pp_string (pp, "})");
    }
  else
    {
      pp_printf (pp, "CONST_FN_RESULT(%qD, {", m_fndecl);
      for (unsigned i = 0; i < m_num_inputs; i++)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  dump_input (pp, i, m_input_arr[i], simple);
	}
      pp_string (pp, "})");
    }
}

/* Subroutine of const_fn_result_svalue::dump_to_pp.  */

void
const_fn_result_svalue::dump_input (pretty_printer *pp,
				    unsigned input_idx,
				    const svalue *sval,
				    bool simple) const
{
  pp_printf (pp, "arg%i: ", input_idx);
  sval->dump_to_pp (pp, simple);
}

/* Implementation of svalue::accept vfunc for const_fn_result_svalue.  */

void
const_fn_result_svalue::accept (visitor *v) const
{
  for (unsigned i = 0; i < m_num_inputs; i++)
    m_input_arr[i]->accept (v);
  v->visit_const_fn_result_svalue (this);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
