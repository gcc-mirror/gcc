/* Symbolic values.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "value-relation.h"
#include "range-op.h"

#include "text-art/dump.h"

#include "diagnostics/markup.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/svalue.h"
#include "analyzer/region-model.h"
#include "analyzer/liveness-context.h"

#if ENABLE_ANALYZER

namespace ana {

/* class svalue and its various subclasses.  */

/* class svalue.  */

DEBUG_FUNCTION void
svalue::dump () const
{
  symbolic::dump_node_tree_to_stderr<svalue> (*this);
}

void
svalue::print_dump_widget_label (pretty_printer *pp) const
{
  if (get_type ())
    pp_printf (pp, "%qT: ", get_type ());

  start_any_dump_color (pp);
  print_svalue_for_dump_widget_label (pp);
  end_any_dump_color (pp);
}

void
svalue::add_dump_widget_children (text_art::tree_widget &w,
				  symbolic::node_dump_state &nds) const
{
  /* It's overkill to print the value_range for constants.  */
  if (get_kind () == SK_CONSTANT)
    return;

  value_range out;
  if (maybe_get_value_range (out))
    {
      pretty_printer pp;
      pp_printf (&pp, "value_range: {");
      out.print (&pp);
      pp_string (&pp, "}");
      w.add_child (text_art::tree_widget::make (nds, &pp));
    }
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

std::unique_ptr<json::object>
svalue::make_json_obj (const char *type_name) const
{
  auto obj = value_node::make_json_obj (type_name);
  if (m_type)
    obj->set ("type", tree_to_json (m_type));
  return obj;
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
    case SK_BIT_VECTOR:
      {
	const auto *sval = (const svalue_from_bit_vector *)this;
	auto &bv = sval->get_bit_vector ();
	return bv.maybe_print_for_user (pp, model, outer_sval);
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

iter_result
svalue::for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
			 void *user_data) const
{
  return cb (*this, user_data);
}

/* Return true iff this svalue has a concrete size.
   If OUT is non-null, and returning true, then also write
   the size to *OUT.  */

bool
svalue::
has_concrete_size_p (concrete_bit_size *out,
		     symbolic::scalar_manager<concrete_bit_size> &bit_mgr) const
{
  if (!get_type ())
    {
      if (tree cst = maybe_get_constant ())
	if (TREE_CODE (cst) == RAW_DATA_CST)
	  {
	    concrete_byte_size byte_length (RAW_DATA_LENGTH (cst));
	    if (out)
	      *out = bits_from_bytes (byte_length);
	    return true;
	  }
      if (get_kind () == SK_POINTER)
	{
	  if (out)
	    *out = concrete_bit_size
		     (convert_from_integer_cst (TYPE_SIZE (ptr_type_node)));
	  return true;
	}
      if (auto *init_sval = dyn_cast_initial_svalue ())
	if (auto *sym_bit_size
	    = init_sval->get_region ()->maybe_get_bit_size (bit_mgr))
	  {
	    concrete_bit_size conc_bit_size;
	    if (concrete_from_symbolic (*sym_bit_size, &conc_bit_size))
	      {
		if (out)
		  *out = conc_bit_size;
		return true;
	      }
	  }
      return false;
    }

  tree type = get_type ();
  if (INTEGRAL_TYPE_P (type))
    {
      if (out)
	*out = concrete_bit_size (TYPE_PRECISION (type));
      return true;
    }

  tree type_size = TYPE_SIZE (type);
  if (!type_size)
    return false;
  if (TREE_CODE (type_size) != INTEGER_CST)
    return false;
  if (out)
    *out = concrete_bit_size (convert_from_integer_cst (type_size));
  return true;
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

/* If this svalue is a pointer_svalue, return a pointer to its
   symbolic::pointer.
   Otherwise return nullptr.  */

const symbolic::pointer *
svalue::maybe_get_symbolic_pointer () const
{
  if (const pointer_svalue *ptr_sval = dyn_cast_pointer_svalue ())
    return &ptr_sval->get_symbolic_pointer ();
  else
    return nullptr;
}

/* If this svalue is a cast (i.e a unaryop NOP_EXPR or VIEW_CONVERT_EXPR),
   return the underlying svalue.
   Otherwise return nullptr.  */

const svalue *
svalue::maybe_undo_cast () const
{
  if (const unaryop_svalue *unaryop_sval = dyn_cast_unaryop_svalue ())
    {
      enum tree_code op = unaryop_sval->get_op ();
      if (op == NOP_EXPR || op == VIEW_CONVERT_EXPR)
	return unaryop_sval->get_arg ();
    }
  return nullptr;
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
   Return nullptr if not mergeable.  */

const svalue *
svalue::can_merge_p (const svalue *other,
		     region_model_manager *mgr,
		     model_merger *merger) const
{
  if (this == other)
    return this;

  if (!(get_type () && other->get_type ()))
    return nullptr;

  if (!types_compatible_p (get_type (), other->get_type ()))
    return nullptr;

  /* Reject attempts to merge unmergeable svalues.  */
  if ((get_kind () == SK_UNMERGEABLE)
      || (other->get_kind () == SK_UNMERGEABLE))
    return nullptr;

  /* Reject attempts to merge poisoned svalues with other svalues
     (either non-poisoned, or other kinds of poison), so that e.g.
     we identify paths in which a variable is conditionally uninitialized.  */
  if (get_kind () == SK_POISONED
      || other->get_kind () == SK_POISONED)
    return nullptr;

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
	return nullptr;
    }

  /* Reject merging svalues that have non-purgeable sm-state,
     to avoid falsely reporting memory leaks by merging them
     with something else.  */
  if (!merger->mergeable_svalue_p (this))
    return nullptr;
  if (!merger->mergeable_svalue_p (other))
    return nullptr;

  /* Handle merger of symbolic pointers.  */
  if (auto *sym_ptr_this = maybe_get_symbolic_pointer ())
    if (auto *sym_ptr_other = other->maybe_get_symbolic_pointer ())
      {
	auto &this_region = sym_ptr_this->get_base_region ();
	auto &other_region = sym_ptr_other->get_base_region ();
	/* Same base region?  Consider merger of offsets.  */
	if (&this_region == &other_region)
	  {
	    const svalue *this_offset_sval
	      = mgr->get_svalue_from_scalar<concrete_byte_offset>
		 (ptrdiff_type_node, sym_ptr_this->get_byte_offset ());
	    const svalue *other_offset_sval
	      = mgr->get_svalue_from_scalar<concrete_byte_offset>
		  (ptrdiff_type_node, sym_ptr_other->get_byte_offset ());
	    if (auto *merged_offset
		  = this_offset_sval->can_merge_p (other_offset_sval,
						   mgr,
						   merger))
	      {
		auto &byte_mgr
		  = mgr->get_node_manager ().get_scalars ().get_bytes ();
		auto &merged_byte_offset
		  = byte_mgr.get_scalar_from_svalue (*merged_offset);
		symbolic::pointer merged_ptr (this_region,
					      merged_byte_offset);
		return mgr->get_ptr_svalue (get_type (), merged_ptr);
	      }
	    else
	      return nullptr;
	  }

	/* Reject merger where the base regions are different,
	   except where both are strings.  */
	if (this_region.get_kind () != RK_STRING
	      || other_region.get_kind () != RK_STRING)
	    return nullptr;
      }

  /* Widening.  */
  /* Merge: (new_cst, existing_cst) -> widen (existing, new).  */
  if (maybe_get_constant () && other->maybe_get_constant ())
    return mgr->get_or_create_widening_svalue (other->get_type (),
					       merger->get_supernode (),
					       other, this);

  /* Merger of:
	 this: BINOP (X, OP, CST)
	other: X, where X is non-widening
	   to: WIDENING (other, this).  */
  if (const binop_svalue *binop_sval = dyn_cast_binop_svalue ())
    if (binop_sval->get_arg0 () == other
	&& binop_sval->get_arg1 ()->get_kind () == SK_CONSTANT
	&& other->get_kind () != SK_WIDENING)
      return mgr->get_or_create_widening_svalue (other->get_type (),
						 merger->get_supernode (),
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
      if (TREE_STRING_LENGTH (cst1) < TREE_STRING_LENGTH (cst2))
	return -1;
      if (TREE_STRING_LENGTH (cst1) > TREE_STRING_LENGTH (cst2))
	return 1;
      return memcmp (TREE_STRING_POINTER (cst1),
		     TREE_STRING_POINTER (cst2),
		     TREE_STRING_LENGTH (cst1));
    case RAW_DATA_CST:
      if (RAW_DATA_LENGTH (cst1) < RAW_DATA_LENGTH (cst2))
	return -1;
      if (RAW_DATA_LENGTH (cst1) > RAW_DATA_LENGTH (cst2))
	return 1;
      return memcmp (RAW_DATA_POINTER (cst1),
		     RAW_DATA_POINTER (cst2),
		     RAW_DATA_LENGTH (cst1));
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

int
cmp_types (const_tree a, const_tree b)
{
  int t1 = a ? TYPE_UID (a) : -1;
  int t2 = b ? TYPE_UID (b) : -1;
  return t1 - t2;
}

/* Comparator for imposing a deterministic order on constants that might
   not be of the same type.  */

int
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
  return cmp (*sval1, *sval2);
}

/* Comparator for use by vec<const svalue *>::qsort.  */

int
svalue::cmp_ptr_ptr (const void *p1, const void *p2)
{
  const svalue *sval1 = *(const svalue * const *)p1;
  const svalue *sval2 = *(const svalue * const *)p2;
  return cmp_ptr (sval1, sval2);
}

/* Base implementation of svalue::all_zeroes_p.
   Return true if this value is known to be all zeroes.  */

bool
svalue::all_zeroes_p () const
{
  return false;
}

/* If this svalue is a pointer, attempt to determine the base region it points
   to.  Return nullptr on any problems.  */

const region *
svalue::maybe_get_deref_base_region () const
{
  const svalue *iter = this;
  while (1)
    {
      switch (iter->get_kind ())
	{
	default:
	  return nullptr;

	case SK_POINTER:
	  {
	    auto *ptr_sval = as_a<const pointer_svalue *> (iter);
	    return &ptr_sval->get_base_region ();
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
		return nullptr;
	      }
	    return nullptr;
	  }
	}
    }
}

/* If this svalue is a pointer to the typeinfo instance for a particular
   type, return that type.  Otherwise return NULL_TREE.  */

tree
svalue::maybe_get_type_from_typeinfo () const
{
  if (auto *sym_ptr = maybe_get_symbolic_pointer ())
    if (sym_ptr->get_byte_offset ().is_zero_p ())
      {
	auto &base_reg = sym_ptr->get_base_region ();
	if (const decl_region *decl_reg = base_reg.dyn_cast_decl_region ())
	  return TREE_TYPE (DECL_NAME (decl_reg->get_decl ()));
      }

  return NULL_TREE;
}

/* Base implementation of svalue::conjured_p.  */

bool
svalue::conjured_p () const
{
  return false;
}

/* Return true if this svalue is the initial value of a region,
   or derived directly from such a value (e.g. by casting or cropping).  */

bool
svalue::derived_from_initial_value_p () const
{
  switch (get_kind ())
    {
    default:
      return false;

    case SK_INITIAL:
      return true;

    case SK_UNARYOP:
      {
	const unaryop_svalue &unaryop = *as_a<const unaryop_svalue *> (this);
	return unaryop.get_arg ()->derived_from_initial_value_p ();
      }

    case SK_BIT_VECTOR:
      {
	const svalue_from_bit_vector &from_bv
	  = *as_a<const svalue_from_bit_vector *> (this);
	return from_bv.get_bit_vector ().derived_from_initial_value_p ();
      }
    }
}

/* Return true if we can get a value_range for TYPE (which could be
   NULL_TREE); false otherwise.  */

static bool
type_can_have_value_range_p (tree type)
{
  if (!type)
    return false;
  if (irange::supports_p (type))
    return true;
  if (frange::supports_p (type))
    return true;
  return false;
}

/* Base implementation of svalue::maybe_get_value_range_1 vfunc.
   If there is a suitable underlying type, write a "varying" for it to OUT
   (for "any value of that type") and return true; otherwise return false.  */

bool
svalue::maybe_get_value_range_1 (value_range &out) const
{
  tree type = get_type ();
  if (!type_can_have_value_range_p (type))
    return false;

  out.set_varying (type);
  return true;
}

/* class pointer_svalue : public svalue.  */

bool
pointer_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = m_ptr.cmp (other.m_ptr))
    return c < 0;
  return false;
}

complexity
pointer_svalue::key_t::get_complexity () const
{
  return complexity::from_pair (m_ptr.get_base_region (),
				m_ptr.get_byte_offset ());
}

/* Implementation of svalue::dump_to_pp vfunc for pointer_svalue.  */

void
pointer_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "&");
      get_base_region ().dump_to_pp (pp, simple);

      auto &byte_offset = get_byte_offset ();
      if (!byte_offset.is_zero_p ())
	{
	  pp_string (pp, " +(bytes) ");
	  byte_offset.dump_to_pp (pp);
	}
    }
  else
    {
      pp_printf (pp, "POINTER_SVALUE(reg: (%i), offset: (%i))",
		 get_base_region ().get_id (),
		 get_byte_offset ().get_id ());
    }
}

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   pointer_svalue.  */

void
pointer_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "pointer_svalue:");
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   pointer_svalue.  */

void
pointer_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_ptr.make_dump_widget (nds));
}

void
pointer_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    m_ptr.accept_node_visitor (v);
}

std::unique_ptr<json::object>
pointer_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("pointer_svalue");
  obj->set ("ptr", m_ptr.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
pointer_svalue::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  if (get_byte_offset ().is_zero_p ())
    {
      auto markup = std::make_unique<diagnostics::markup::compound_node> ();
      markup->add_child ("&");
      // TODO: parens?
      markup->add_child (get_base_region ().make_markup_node (markup_ctxt));
      return markup;
    }
  return diagnostics::markup::make_text_node ("pointer_svalue");
}

/* Implementation of svalue::accept vfunc for pointer_svalue.  */

void
pointer_svalue::accept (visitor *v) const
{
  m_ptr.get_base_region ().accept (v);
  v->visit_pointer_svalue (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for pointer_svalue.  */

bool
pointer_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  /* Pointers into clusters that have escaped should be treated as live.  */
  const region &base_reg = get_base_region ();
  const store *store = liveness_ctxt.get_model ()->get_store ();
  if (const binding_cluster *c = store->get_cluster (&base_reg))
    if (c->escaped_p ())
      return true;

  /* Any known-live pointer to BASE_REG in LIVENESS_CTXT allows us to
     treat all pointers to BASE_REG as live, so that different
     (X*)ptr and (Y*)ptr are effectively the same from
     a liveness perspective.  */
  if (liveness_ctxt.known_live_ptr_to_base_region_p (base_reg))
    return true;

  return false;
}

/* class constant_svalue : public svalue.  */

bool
constant_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = cmp_types (TREE_TYPE (m_cst), TREE_TYPE (other.m_cst)))
    return c < 0;
  gcc_assert (TREE_CODE (m_cst) == TREE_CODE (other.m_cst));
  return cmp_csts_same_type (m_cst, other.m_cst) < 0;
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   constant_svalue.  */

void
constant_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "constant_svalue (%qE)", m_cst_expr);
}

void
constant_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_svalue (*this);
}

std::unique_ptr<json::object>
constant_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("constant_svalue");
  obj->set ("constant", tree_to_json (m_cst_expr));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
constant_svalue::make_markup_node (symbolic::markup_context &) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_printf (&pp, "%E", m_cst_expr);
  return std::make_unique<diagnostics::markup::text> (pp_formatted_text (&pp));
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
constant_svalue::implicitly_live_p (symbolic::liveness_context &) const
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

/* Implementation of svalue::all_zeroes_p for constant_svalue.  */

bool
constant_svalue::all_zeroes_p () const
{
  return zerop (m_cst_expr);
}


/* Implementation of svalue::maybe_get_value_range_1 for constant_svalue.
   If there is a suitable underlying type, write the value_range for the
   single value of m_cst_expr to OUT and return true; otherwise return
   false.  */

bool
constant_svalue::maybe_get_value_range_1 (value_range &out) const
{
  if (!type_can_have_value_range_p (get_type ()))
    return false;

  out = value_range (m_cst_expr, m_cst_expr);
  return true;
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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   unknown_svalue.  */

void
unknown_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "unknown_svalue");
}

void
unknown_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_svalue (*this);
}

std::unique_ptr<json::object>
unknown_svalue::to_json () const
{
  return svalue::make_json_obj ("unknown_svalue");
}

std::unique_ptr<diagnostics::markup::node>
unknown_svalue::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("unknown");
}

/* Implementation of svalue::accept vfunc for unknown_svalue.  */

void
unknown_svalue::accept (visitor *v) const
{
  v->visit_unknown_svalue (this);
}

bool
unknown_svalue::maybe_get_value_range_1 (value_range &) const
{
  /* Don't attempt to participate in range ops.  */
  return false;
}


/* Get a string for KIND for use in debug dumps.  */

const char *
poison_kind_to_str (enum poison_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case poison_kind::uninit:
      return "uninit";
    case poison_kind::freed:
      return "freed";
    case poison_kind::deleted:
      return "deleted";
    case poison_kind::popped_stack:
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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   poisoned_svalue.  */

void
poisoned_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "poisoned_svalue(%s)", poison_kind_to_str (m_kind));
}

void
poisoned_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_svalue (*this);
}

std::unique_ptr<json::object>
poisoned_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("poisoned_svalue");
  obj->set_string ("kind", poison_kind_to_str (m_kind));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
poisoned_svalue::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("poisoned_svalue");
}

/* Implementation of svalue::accept vfunc for poisoned_svalue.  */

void
poisoned_svalue::accept (visitor *v) const
{
  v->visit_poisoned_svalue (this);
}

/* class setjmp_svalue's implementation is in setjmp-longjmp.cc.  */

/* class initial_svalue : public svalue.  */

/* Implementation of svalue::dump_to_pp vfunc for initial_svalue.  */

initial_svalue::initial_svalue (symbolic::node::id_t id, tree type, const region *reg)
: svalue (complexity (reg), id, type), m_reg (reg)
{
  gcc_assert (m_reg != nullptr);

  /* Must be a base region.  */
  gcc_assert (m_reg->base_region_p ());

  /* TODO(once we have a m_node_mgr):
     assert that m_reg->region_has_meaningful_size_p (m_node_mgr) ???  */
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   initial_svalue.  */

void
initial_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "initial_svalue");
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   initial_svalue.  */

void
initial_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_reg->make_dump_widget (nds, "m_reg"));
}

void
initial_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    m_reg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
initial_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("initial_svalue");
  obj->set ("reg", m_reg->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
initial_svalue::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  // TODO: show 'initial_value()' ?
  return m_reg->make_markup_node (markup_ctxt);
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
initial_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  /* This svalue may be implicitly live if the region still implicitly
     has its initial value and is reachable.  */

  /* It must be a region that exists; we don't want to consider
     INIT_VAL(R) as still being implicitly reachable if R is in
     a popped stack frame.  */
  auto *model = liveness_ctxt.get_model ();
  if (model->region_exists_p (m_reg))
    {
      const svalue *reg_sval = model->get_store_value (m_reg, nullptr);
      if (reg_sval == this)
	return true;
    }

  /* Assume that the initial values of params for the top level frame
     are still live, because (presumably) they're still
     live in the external caller.  */
  if (initial_value_of_param_p ())
    if (const frame_region *frame_reg = m_reg->maybe_get_frame_region ())
      if (frame_reg->get_calling_frame () == nullptr)
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

bool
unaryop_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = (int)m_op - (int)other.m_op)
    return c < 0;
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_arg, other.m_arg))
    return c < 0;
  return false;
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   unaryop_svalue.  */

void
unaryop_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp,
	     "unaryop_svalue(%s)",
	     get_tree_code_name (m_op));
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   unaryop_svalue.  */

void
unaryop_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_arg->make_dump_widget (nds));
}

void
unaryop_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    m_arg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
unaryop_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("unaryop_svalue");
  obj->set ("arg", m_arg->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
unaryop_svalue::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  gcc_assert (m_op == NOP_EXPR); // FIXME!!!
  return m_arg->make_markup_node (markup_ctxt);
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
unaryop_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return liveness_ctxt.node_live_p (*get_arg ());
}

/* Implementation of svalue::maybe_get_value_range_1 for unaryop_svalue.  */

bool
unaryop_svalue::maybe_get_value_range_1 (value_range &out) const
{
  tree type = get_type ();
  if (!type_can_have_value_range_p (type))
    return false;

  value_range arg_vr;
  if (m_arg->maybe_get_value_range (arg_vr))
    {
      range_op_handler handler (m_op);
      if (handler
	  && handler.operand_check_p (type, arg_vr.type (), type))
	{
	  /* For unary ops, range_op_hander::fold_range expects
	     a VARYING of the unknown value as the 2nd operand.  */
	  value_range varying (type);
	  varying.set_varying (type);
	  out.set_range_class (type);
	  if (handler.fold_range (out, type, arg_vr, varying))
	    return true;
	}
    }
  return false;
}

/* class binop_svalue : public svalue.  */

bool
binop_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = (int)m_op - (int)other.m_op)
    return c < 0;
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_arg0, other.m_arg0))
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_arg1, other.m_arg1))
    return c < 0;
  return false;
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   binop_svalue.  */

void
binop_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp,
	     "binop_svalue(%s: %qs)",
	     get_tree_code_name (m_op),
	     op_symbol_code (m_op));
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   binop_svalue.  */

void
binop_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_arg0->make_dump_widget (nds));
  w.add_child (m_arg1->make_dump_widget (nds));
}

void
binop_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    {
      m_arg0->accept_node_visitor (v);
      m_arg1->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
binop_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("binop_svalue");
  obj->set ("arg0", m_arg0->to_json ());
  obj->set ("arg1", m_arg1->to_json ());
  // FIXME: op?
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
binop_svalue::make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  return std::make_unique<diagnostics::markup::code::infix_binary_op>
    (m_arg0->make_markup_node (markup_ctxt),
     op_symbol_code (m_op),
     m_arg1->make_markup_node (markup_ctxt));
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
binop_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return (liveness_ctxt.node_live_p (*get_arg0 ())
	  && liveness_ctxt.node_live_p (*get_arg1 ()));
}

/* Implementation of svalue::maybe_get_value_range_1 for binop_svalue.  */

bool
binop_svalue::maybe_get_value_range_1 (value_range &out) const
{
  tree type = get_type ();
  if (!type_can_have_value_range_p (type))
    return false;

  /* Avoid cases where we have been sloppy about types.  */
  if (!m_arg0->get_type ())
    return false;
  if (!m_arg1->get_type ())
    return false;
  if (!range_compatible_p (m_arg0->get_type (), m_arg1->get_type ()))
    return false;

  value_range lhs, rhs;
  if (m_arg0->maybe_get_value_range (lhs))
    if (m_arg1->maybe_get_value_range (rhs))
      {
	range_op_handler handler (m_op);
	if (handler
	    && handler.operand_check_p (type, lhs.type (), rhs.type ()))
	  {
	    out.set_range_class (type);
	    if (handler.fold_range (out, get_type (), lhs, rhs))
	      return true;
	  }
      }
  return false;
}

/* class svalue_from_bit_vector : public svalue.  */

bool
svalue_from_bit_vector::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_bv, other.m_bv))
    return c < 0;
  return false;
}

/* svalue_from_bit_vector'c ctor.  */

svalue_from_bit_vector::
svalue_from_bit_vector (symbolic::node::id_t id,
			tree type,
			const symbolic::bit_vector &bv)
: svalue (/* Reuse the complexity of BV rather than adding one to it for now,
	     to stay closer to pre-symbolic::node behavior.  */
	  bv.get_complexity (),
	  id, type),
  m_bv (bv)
{
  if (CHECKING_P)
    {
      /* TODO: should we assert that the type's size matches that
	 of the bit_vector?  */

      // No nesting:
      gcc_assert (!m_bv.dyn_cast_bit_vector_from_svalue ());
    }
}

/* Implementation of svalue::dump_to_pp vfunc for svalue_from_bit_vector.  */

void
svalue_from_bit_vector::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "SVALUE_FROM_BIT_VECTOR(");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_printf (pp, "bit_vector: (%i)", m_bv.get_id ());
      pp_character (pp, ')');
    }
  else
    {
      pp_string (pp, "svalue_from_bit_vector (");
      if (get_type ())
	{
	  print_quoted_type (pp, get_type ());
	  pp_string (pp, ", ");
	}
      pp_printf (pp, "bits: (%i)", m_bv.get_id ());
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   svalue_from_bit_vector.  */

void
svalue_from_bit_vector::
print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_string (pp, "svalue_from_bit_vector");
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   svalue_from_bit_vector.  */

void
svalue_from_bit_vector::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_bv.make_dump_widget (nds, "m_bv"));
}

void
svalue_from_bit_vector::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    m_bv.accept_node_visitor (v);
}

std::unique_ptr<json::object>
svalue_from_bit_vector::to_json () const
{
  auto obj = svalue::make_json_obj ("svalue_from_bit_vector");
  obj->set ("bit_vector", m_bv.to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
svalue_from_bit_vector::
make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  return m_bv.make_markup_node (markup_ctxt);
}

/* Implementation of svalue::accept vfunc for svalue_from_bit_vector.  */

void
svalue_from_bit_vector::accept (visitor *v) const
{
  // FIXME:
  // m_bv.accept_node_visitor (v);

  v->visit_svalue_from_bit_vector (this);
}

/* Implementation of svalue::implicitly_live_p vfunc for svalue_from_bit_vector.  */

bool
svalue_from_bit_vector::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return liveness_ctxt.node_live_p (m_bv);
}

/* Implementation of svalue::all_zeroes_p for constant_svalue.  */

bool
svalue_from_bit_vector::all_zeroes_p () const
{
  if (auto *ts = m_bv.dyn_cast_tristate_bit_vector ())
    return ts->get_bit_value ().is_false ();
  return false;
}

bool
svalue_from_bit_vector::maybe_get_value_range_1 (value_range &out) const
{
  tree type = get_type ();
  if (!type_can_have_value_range_p (type))
    return false;

  out.set_varying (type);

  irange_bitmask bm;
  if (m_bv.maybe_get_irange_bitmask (&bm))
    out.update_bitmask (bm);

  return true;
}

bool
svalue_from_bit_vector::conjured_p () const
{
  return m_bv.conjured_p ();
}

/* class widening_svalue : public svalue.  */

bool
widening_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = m_snode->m_id - other.m_snode->m_id)
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_base_sval, other.m_base_sval))
    return c < 0;
  if (int c = symbolic::node::cmp_ids (m_iter_sval, other.m_iter_sval))
    return c < 0;
  return false;
}

/* Implementation of svalue::dump_to_pp vfunc for widening_svalue.  */

void
widening_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    {
      pp_string (pp, "WIDENING(");
      pp_character (pp, '{');
      m_snode->print (pp);
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
      m_snode->print (pp);
      pp_string (pp, "}, ");
      m_base_sval->dump_to_pp (pp, simple);
      pp_string (pp, ", ");
      m_iter_sval->dump_to_pp (pp, simple);
      pp_character (pp, ')');
    }
}

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   widening_svalue.  */

void
widening_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "widening_svalue at ");
  m_snode->print (pp);
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   widening_svalue.  */

void
widening_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_base_sval->make_dump_widget (nds, "m_base_sval"));
  w.add_child (m_iter_sval->make_dump_widget (nds, "m_iter_sval"));
}

void
widening_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    {
      m_base_sval->accept_node_visitor (v);
      m_iter_sval->accept_node_visitor (v);
    }
}

std::unique_ptr<json::object>
widening_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("widening_svalue");
  obj->set ("base_sval", m_base_sval->to_json ());
  obj->set ("iter_sval", m_iter_sval->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
widening_svalue::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("widening_svalue");
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
	       This is either true or false at +ve infinity,
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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   placeholder_svalue.  */

void
placeholder_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "placeholder_svalue: %qs", m_name);
}

void
placeholder_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  v.visit_svalue (*this);
}

std::unique_ptr<json::object>
placeholder_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("placeholder_svalue");
  obj->set_string ("name", m_name);
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
placeholder_svalue::make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node (m_name);
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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   unmergeable_svalue.  */

void
unmergeable_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "unmergeable_svalue");
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   unmergeable_svalue.  */

void
unmergeable_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  w.add_child (m_arg->make_dump_widget (nds));
}

void
unmergeable_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    m_arg->accept_node_visitor (v);
}

std::unique_ptr<json::object>
unmergeable_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("unmergeable_svalue");
  obj->set ("arg", m_arg->to_json ());
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
unmergeable_svalue::
make_markup_node (symbolic::markup_context &markup_ctxt) const
{
  if (markup_ctxt.m_user_facing)
    return m_arg->make_markup_node (markup_ctxt);
  else
    return diagnostics::markup::make_text_node ("unmergeable_svalue");
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
unmergeable_svalue::
implicitly_live_p (symbolic::liveness_context &liveness_ctxt) const
{
  return liveness_ctxt.node_live_p (*get_arg ());
}

/* class asm_output_svalue : public svalue.  */

bool
asm_output_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = strcmp (m_asm_string, other.m_asm_string))
    return c < 0;
  if (int c = (int)m_output_idx - (int)other.m_output_idx)
    return c < 0;
  if (int c = (int)m_num_inputs - (int)other.m_num_inputs)
    return c < 0;
  for (unsigned i = 0; i < m_num_inputs; i++)
    if (int c = symbolic::node::cmp_ids (m_input_arr[i], other.m_input_arr[i]))
      return c < 0;
  return false;
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   asm_output_svalue.  */

void
asm_output_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "asm_output_svalue(%qs, %%%i)",
	     get_asm_string (),
	     get_output_idx ());
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   asm_output_svalue.  */

void
asm_output_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  for (unsigned i = 0; i < m_num_inputs; i++)
    {
      pretty_printer pp;
      pp_printf (&pp, "arg %i", i);
      w.add_child (m_input_arr[i]->make_dump_widget (nds,
						     pp_formatted_text (&pp)));
    }
}

void
asm_output_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    for (unsigned i = 0; i < m_num_inputs; i++)
      m_input_arr[i]->accept_node_visitor (v);
}

std::unique_ptr<json::object>
asm_output_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("asm_output_svalue");
  obj->set_string ("id_reg", get_asm_string ());
  obj->set_integer ("output_idx", get_output_idx ());
  auto arr = std::make_unique<json::array> ();
  for (unsigned i = 0; i < m_num_inputs; i++)
    arr->append (m_input_arr[i]->to_json ());
  obj->set ("input_arr", std::move (arr));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
asm_output_svalue::
make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("asm_output_svalue");
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

bool
const_fn_result_svalue::key_t::operator< (const key_t &other) const
{
  if (int c = cmp_types (m_type, other.m_type))
    return c < 0;
  if (int c = DECL_UID (m_fndecl) - DECL_UID (other.m_fndecl))
    return c < 0;
  if (int c = (int)m_num_inputs - (int)other.m_num_inputs)
    return c < 0;
  for (unsigned i = 0; i < m_num_inputs; i++)
    if (int c = symbolic::node::cmp_ids (m_input_arr[i], other.m_input_arr[i]))
      return c < 0;
  return false;
}

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

/* Implementation of svalue::print_svalue_for_dump_widget_label vfunc for
   const_fn_result_svalue.  */

void
const_fn_result_svalue::print_svalue_for_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "const_fn_result_svalue: %qD", m_fndecl);
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   const_fn_result_svalue.  */

void
const_fn_result_svalue::
add_dump_widget_children (text_art::tree_widget &w,
			  symbolic::node_dump_state &nds) const
{
  svalue::add_dump_widget_children (w, nds);
  for (unsigned i = 0; i < m_num_inputs; i++)
    {
      pretty_printer pp;
      pp_printf (&pp, "arg %i", i);
      w.add_child (m_input_arr[i]->make_dump_widget (nds,
						     pp_formatted_text (&pp)));
    }
}

void
const_fn_result_svalue::accept_node_visitor (symbolic::node_visitor &v) const
{
  if (v.visit_svalue (*this))
    for (unsigned i = 0; i < m_num_inputs; i++)
      m_input_arr[i]->accept_node_visitor (v);
}

std::unique_ptr<json::object>
const_fn_result_svalue::to_json () const
{
  auto obj = svalue::make_json_obj ("const_fn_result_svalue");
  obj->set ("fn_decl", tree_to_json (m_fndecl));
  auto arr = std::make_unique<json::array> ();
  for (unsigned i = 0; i < m_num_inputs; i++)
    arr->append (m_input_arr[i]->to_json ());
  obj->set ("input_arr", std::move (arr));
  return obj;
}

std::unique_ptr<diagnostics::markup::node>
const_fn_result_svalue::
make_markup_node (symbolic::markup_context &) const
{
  return diagnostics::markup::make_text_node ("const_fn_result_svalue");
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
