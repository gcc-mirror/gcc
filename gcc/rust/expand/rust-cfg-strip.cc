// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-cfg-strip.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-path.h"
#include "rust-session-manager.h"
#include "rust-attribute-values.h"

namespace Rust {

/**
 * Determines whether any cfg predicate is false and hence item with attributes
 * should be stripped. Note that attributes must be expanded before calling.
 */
bool
fails_cfg (const AST::AttrVec &attrs)
{
  auto &session = Session::get_instance ();

  for (const auto &attr : attrs)
    {
      if (attr.get_path () == Values::Attributes::CFG
	  && !attr.check_cfg_predicate (session))
	return true;
    }
  return false;
}

/**
 * Determines whether any cfg predicate is false and hence item with attributes
 * should be stripped. Will expand attributes as well.
 */
bool
fails_cfg_with_expand (AST::AttrVec &attrs)
{
  auto &session = Session::get_instance ();

  // TODO: maybe have something that strips cfg attributes that evaluate true?
  for (auto &attr : attrs)
    {
      if (attr.get_path () == Values::Attributes::CFG)
	{
	  if (!attr.is_parsed_to_meta_item ())
	    attr.parse_attr_to_meta_item ();

	  // DEBUG
	  if (!attr.is_parsed_to_meta_item ())
	    rust_debug ("failed to parse attr to meta item, right before "
			"cfg predicate check");
	  else
	    rust_debug ("attr has been successfully parsed to meta item, "
			"right before cfg predicate check");

	  if (!attr.check_cfg_predicate (session))
	    {
	      // DEBUG
	      rust_debug (
		"cfg predicate failed for attribute: \033[0;31m'%s'\033[0m",
		attr.as_string ().c_str ());

	      return true;
	    }
	  else
	    {
	      // DEBUG
	      rust_debug ("cfg predicate succeeded for attribute: "
			  "\033[0;31m'%s'\033[0m",
			  attr.as_string ().c_str ());
	    }
	}
    }
  return false;
}

/**
 * Expands cfg_attr attributes.
 */
void
expand_cfg_attrs (AST::AttrVec &attrs)
{
  auto &session = Session::get_instance ();

  for (std::size_t i = 0; i < attrs.size (); i++)
    {
      auto &attr = attrs[i];
      if (attr.get_path () == Values::Attributes::CFG_ATTR)
	{
	  if (!attr.is_parsed_to_meta_item ())
	    attr.parse_attr_to_meta_item ();

	  if (attr.check_cfg_predicate (session))
	    {
	      // split off cfg_attr
	      AST::AttrVec new_attrs = attr.separate_cfg_attrs ();

	      // remove attr from vector
	      attrs.erase (attrs.begin () + i);

	      // add new attrs to vector
	      attrs.insert (attrs.begin () + i,
			    std::make_move_iterator (new_attrs.begin ()),
			    std::make_move_iterator (new_attrs.end ()));
	    }

	  /* do something - if feature (first token in tree) is in fact enabled,
	   * make tokens listed afterwards into attributes. i.e.: for
	   * [cfg_attr(feature = "wow", wow1, wow2)], if "wow" is true, then add
	   * attributes [wow1] and [wow2] to attribute list. This can also be
	   * recursive, so check for expanded attributes being recursive and
	   * possibly recursively call the expand_attrs? */
	}
      else
	{
	  i++;
	}
    }
  attrs.shrink_to_fit ();
}

void
CfgStrip::go (AST::Crate &crate)
{
  visit (crate);
}

void
CfgStrip::visit (AST::Crate &crate)
{
  // expand crate cfg_attr attributes
  expand_cfg_attrs (crate.inner_attrs);

  if (fails_cfg_with_expand (crate.inner_attrs))
    {
      // basically, delete whole crate
      crate.strip_crate ();
      // TODO: maybe create warning here? probably not desired behaviour
    }

  auto &items = crate.items;

  AST::DefaultASTVisitor::visit (crate);
  for (auto it = items.begin (); it != items.end ();)
    {
      auto &item = *it;
      if (item->is_marked_for_strip ())
	it = items.erase (it);
      else
	it++;
    }
  // expand module attributes?
}

// Visitor used to expand attributes.
void
CfgStrip::maybe_strip_struct_fields (std::vector<AST::StructField> &fields)
{
  for (auto it = fields.begin (); it != fields.end ();)
    {
      auto &field = *it;

      auto &field_attrs = field.get_outer_attrs ();
      expand_cfg_attrs (field_attrs);
      if (fails_cfg_with_expand (field_attrs))
	{
	  it = fields.erase (it);
	  continue;
	}

      // expand sub-types of type, but can't strip type itself
      auto &type = field.get_field_type ();
      type.accept_vis (*this);

      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");

      // if nothing else happens, increment
      ++it;
    }
}

void
CfgStrip::maybe_strip_struct_expr_fields (
  std::vector<std::unique_ptr<AST::StructExprField>> &fields)
{
  for (auto it = fields.begin (); it != fields.end ();)
    {
      auto &field = *it;

      auto &field_attrs = field->get_outer_attrs ();
      expand_cfg_attrs (field_attrs);
      if (fails_cfg_with_expand (field_attrs))
	{
	  it = fields.erase (it);
	  continue;
	}

      ++it;
    }
}

void
CfgStrip::maybe_strip_tuple_fields (std::vector<AST::TupleField> &fields)
{
  for (auto it = fields.begin (); it != fields.end ();)
    {
      auto &field = *it;

      auto &field_attrs = field.get_outer_attrs ();
      expand_cfg_attrs (field_attrs);
      if (fails_cfg_with_expand (field_attrs))
	{
	  it = fields.erase (it);
	  continue;
	}

      // expand sub-types of type, but can't strip type itself
      auto &type = field.get_field_type ();
      type.accept_vis (*this);
      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");

      // if nothing else happens, increment
      ++it;
    }
}

void
CfgStrip::maybe_strip_function_params (
  std::vector<std::unique_ptr<AST::Param>> &params)
{
  for (auto it = params.begin (); it != params.end ();)
    {
      if (!(*it)->is_self () && !(*it)->is_variadic ())
	{
	  auto param = static_cast<AST::FunctionParam *> (it->get ());

	  auto &param_attrs = param->get_outer_attrs ();
	  expand_cfg_attrs (param_attrs);
	  if (fails_cfg_with_expand (param_attrs))
	    {
	      it = params.erase (it);
	      continue;
	    }

	  // TODO: should an unwanted strip lead to break out of loop?
	  auto &pattern = param->get_pattern ();
	  pattern.accept_vis (*this);
	  if (pattern.is_marked_for_strip ())
	    rust_error_at (pattern.get_locus (),
			   "cannot strip pattern in this position");

	  auto &type = param->get_type ();
	  type.accept_vis (*this);

	  if (type.is_marked_for_strip ())
	    rust_error_at (type.get_locus (),
			   "cannot strip type in this position");
	}
      // increment
      ++it;
    }
}

void
CfgStrip::maybe_strip_generic_args (AST::GenericArgs &args)
{
  // lifetime args can't be expanded
  // FIXME: Can we have macro invocations for lifetimes?

  // expand type args - strip sub-types only
  for (auto &arg : args.get_generic_args ())
    {
      switch (arg.get_kind ())
	{
	  case AST::GenericArg::Kind::Type: {
	    auto &type = arg.get_type ();
	    type.accept_vis (*this);

	    if (type.is_marked_for_strip ())
	      rust_error_at (type.get_locus (),
			     "cannot strip type in this position");
	    break;
	  }
	  case AST::GenericArg::Kind::Const: {
	    auto &expr = arg.get_expression ();
	    expr.accept_vis (*this);

	    if (expr.is_marked_for_strip ())
	      rust_error_at (expr.get_locus (),
			     "cannot strip expression in this position");
	    break;
	  }
	default:
	  break;
	  // FIXME: Figure out what to do here if there is ambiguity. Since the
	  // resolver comes after the expansion, we need to figure out a way to
	  // strip ambiguous values here
	  // TODO: Arthur: Probably add a `mark_as_strip` method to `GenericArg`
	  // or something. This would clean up this whole thing
	}
    }

  // FIXME: Can we have macro invocations in generic type bindings?
  // expand binding args - strip sub-types only
  for (auto &binding : args.get_binding_args ())
    {
      auto &type = binding.get_type ();
      type.accept_vis (*this);

      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");
    }
}

void
CfgStrip::maybe_strip_qualified_path_type (AST::QualifiedPathType &path_type)
{
  auto &type = path_type.get_type ();
  type.accept_vis (*this);

  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  if (path_type.has_as_clause ())
    {
      auto &type_path = path_type.get_as_type_path ();
      visit (type_path);
      if (type_path.is_marked_for_strip ())
	rust_error_at (type_path.get_locus (),
		       "cannot strip type path in this position");
    }
}

void
CfgStrip::CfgStrip::maybe_strip_closure_params (
  std::vector<AST::ClosureParam> &params)
{
  for (auto it = params.begin (); it != params.end ();)
    {
      auto &param = *it;

      auto &param_attrs = param.get_outer_attrs ();
      expand_cfg_attrs (param_attrs);
      if (fails_cfg_with_expand (param_attrs))
	{
	  it = params.erase (it);
	  continue;
	}

      auto &pattern = param.get_pattern ();
      pattern.accept_vis (*this);
      if (pattern.is_marked_for_strip ())
	rust_error_at (pattern.get_locus (),
		       "cannot strip pattern in this position");

      if (param.has_type_given ())
	{
	  auto &type = param.get_type ();
	  type.accept_vis (*this);

	  if (type.is_marked_for_strip ())
	    rust_error_at (type.get_locus (),
			   "cannot strip type in this position");
	}

      // increment if found nothing else so far
      ++it;
    }
}

void
CfgStrip::maybe_strip_where_clause (AST::WhereClause &where_clause)
{
  // items cannot be stripped conceptually, so just accept visitor
  for (auto &item : where_clause.get_items ())
    item->accept_vis (*this);
}

void
CfgStrip::visit (AST::IdentifierExpr &ident_expr)
{
  // strip test based on outer attrs
  AST::DefaultASTVisitor::visit (ident_expr);
  expand_cfg_attrs (ident_expr.get_outer_attrs ());
  if (fails_cfg_with_expand (ident_expr.get_outer_attrs ()))
    {
      ident_expr.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::MacroInvocation &macro_invoc)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (macro_invoc.get_outer_attrs ());
  if (fails_cfg_with_expand (macro_invoc.get_outer_attrs ()))
    {
      macro_invoc.mark_for_strip ();
      return;
    }

  // can't strip simple path

  // I don't think any macro token trees can be stripped in any way

  // TODO: maybe have cfg! macro stripping behaviour here?
}

void
CfgStrip::visit (AST::PathInExpression &path)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (path.get_outer_attrs ());
  if (fails_cfg_with_expand (path.get_outer_attrs ()))
    {
      path.mark_for_strip ();
      return;
    }

  if (!path.is_lang_item ())
    {
      for (auto &segment : path.get_segments ())
	{
	  if (segment.has_generic_args ())
	    maybe_strip_generic_args (segment.get_generic_args ());
	}
    }
}

void
CfgStrip::visit (AST::TypePathSegmentGeneric &segment)
{
  // TODO: strip inside generic args

  if (!segment.has_generic_args ())
    return;

  maybe_strip_generic_args (segment.get_generic_args ());
}
void
CfgStrip::visit (AST::TypePathSegmentFunction &segment)
{
  AST::DefaultASTVisitor::visit (segment);
  auto &type_path_function = segment.get_type_path_function ();

  for (auto &type : type_path_function.get_params ())
    {
      if (type->is_marked_for_strip ())
	rust_error_at (type->get_locus (),
		       "cannot strip type in this position");
    }

  if (type_path_function.has_return_type ())
    {
      auto &return_type = type_path_function.get_return_type ();

      if (return_type.is_marked_for_strip ())
	rust_error_at (return_type.get_locus (),
		       "cannot strip type in this position");
    }
}

void
CfgStrip::visit (AST::QualifiedPathInExpression &path)
{
  // initial strip test based on outer attrs
  AST::DefaultASTVisitor::visit (path);

  expand_cfg_attrs (path.get_outer_attrs ());
  if (fails_cfg_with_expand (path.get_outer_attrs ()))
    {
      path.mark_for_strip ();
      return;
    }

  maybe_strip_qualified_path_type (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    {
      if (segment.has_generic_args ())
	maybe_strip_generic_args (segment.get_generic_args ());
    }
}

void
CfgStrip::visit (AST::QualifiedPathInType &path)
{
  maybe_strip_qualified_path_type (path.get_qualified_path_type ());

  // this shouldn't strip any segments, but can strip inside them
  AST::DefaultASTVisitor::visit (path);
}

void
CfgStrip::visit (AST::LiteralExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::BorrowExpr &expr)
{
  AST::DefaultASTVisitor::visit (expr);
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &borrowed_expr = expr.get_borrowed_expr ();
  if (borrowed_expr.is_marked_for_strip ())
    rust_error_at (borrowed_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::DereferenceExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &dereferenced_expr = expr.get_dereferenced_expr ();
  dereferenced_expr.accept_vis (*this);
  if (dereferenced_expr.is_marked_for_strip ())
    rust_error_at (dereferenced_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ErrorPropagationExpr &expr)
{
  AST::DefaultASTVisitor::visit (expr);

  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &propagating_expr = expr.get_propagating_expr ();
  if (propagating_expr.is_marked_for_strip ())
    rust_error_at (propagating_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::NegationExpr &expr)
{
  AST::DefaultASTVisitor::visit (expr);
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &negated_expr = expr.get_negated_expr ();
  if (negated_expr.is_marked_for_strip ())
    rust_error_at (negated_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  AST::DefaultASTVisitor::visit (expr);
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */

  // ensure that they are not marked for strip
  if (expr.get_left_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_left_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before binary op exprs");
  if (expr.get_right_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_right_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::ComparisonExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */
  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_left_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_left_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before binary op exprs");
  if (expr.get_right_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_right_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::LazyBooleanExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */
  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_left_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_left_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before binary op exprs");
  if (expr.get_right_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_right_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::TypeCastExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * direct descendant expression, can strip ones below that */
  AST::DefaultASTVisitor::visit (expr);

  auto &casted_expr = expr.get_casted_expr ();
  // ensure that they are not marked for strip
  if (casted_expr.is_marked_for_strip ())
    rust_error_at (casted_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed before cast exprs");

  // TODO: strip sub-types of type
  auto &type = expr.get_type_to_cast_to ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");
}
void
CfgStrip::visit (AST::AssignmentExpr &expr)
{
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }
  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_left_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_left_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before binary op exprs");
  if (expr.get_right_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_right_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::CompoundAssignmentExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */
  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_left_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_left_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before binary op exprs");
  if (expr.get_right_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_right_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::GroupedExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says these are inner
   * attributes, not outer attributes of inner expr */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  AST::DefaultASTVisitor::visit (expr);

  auto &inner_expr = expr.get_expr_in_parens ();
  if (inner_expr.is_marked_for_strip ())
    rust_error_at (inner_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ArrayElemsValues &elems)
{
  /* apparently outer attributes are allowed in "elements of array
   * expressions" according to spec */
  maybe_strip_pointer_allow_strip (elems.get_values ());
}
void
CfgStrip::visit (AST::ArrayElemsCopied &elems)
{
  /* apparently outer attributes are allowed in "elements of array
   * expressions" according to spec. on the other hand, it would not
   * make conceptual sense to be able to remove either expression. As
   * such, not implementing. TODO clear up the ambiguity here */
  AST::DefaultASTVisitor::visit (elems);

  // only intend stripping for internal sub-expressions
  auto &copied_expr = elems.get_elem_to_copy ();
  if (copied_expr.is_marked_for_strip ())
    rust_error_at (copied_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  auto &copy_count = elems.get_num_copies ();
  copy_count.accept_vis (*this);
  if (copy_count.is_marked_for_strip ())
    rust_error_at (copy_count.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ArrayExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says there are separate
   * inner attributes, not just outer attributes of inner exprs */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* assuming you can't strip away the ArrayElems type, but can strip
   * internal expressions and whatever */
  AST::DefaultASTVisitor::visit (expr);
}

void
CfgStrip::visit (AST::ArrayIndexExpr &expr)
{
  /* it is unclear whether outer attributes are supposed to be
   * allowed, but conceptually it wouldn't make much sense, but
   * having expansion code anyway. TODO */
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  AST::DefaultASTVisitor::visit (expr);

  const auto &array_expr = expr.get_array_expr ();
  if (array_expr.is_marked_for_strip ())
    rust_error_at (array_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  const auto &index_expr = expr.get_index_expr ();
  if (index_expr.is_marked_for_strip ())
    rust_error_at (index_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::TupleExpr &expr)
{
  /* according to spec, outer attributes are allowed on "elements of
   * tuple expressions" */

  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says these are inner
   * attributes, not outer attributes of inner expr */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* apparently outer attributes are allowed in "elements of tuple
   * expressions" according to spec */
  maybe_strip_pointer_allow_strip (expr.get_tuple_elems ());
}
void
CfgStrip::visit (AST::TupleIndexExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);
  /* wouldn't strip this directly (as outer attrs should be
   * associated with this level), but any sub-expressions would be
   * stripped. Thus, no need to erase when strip check called. */
  auto &tuple_expr = expr.get_tuple_expr ();
  if (tuple_expr.is_marked_for_strip ())
    rust_error_at (tuple_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::StructExprStruct &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says these are inner
   * attributes, not outer attributes of inner expr */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  // strip sub-exprs of path
  auto &struct_name = expr.get_struct_name ();
  visit (struct_name);
  if (struct_name.is_marked_for_strip ())
    rust_error_at (struct_name.get_locus (),
		   "cannot strip path in this position");
}

void
CfgStrip::visit (AST::StructExprFieldIdentifierValue &field)
{
  /* as no attrs possible (at moment, at least), only sub-expression
   * stripping is possible */
  AST::DefaultASTVisitor::visit (field);

  auto &value = field.get_value ();
  if (value.is_marked_for_strip ())
    rust_error_at (value.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::StructExprFieldIndexValue &field)
{
  /* as no attrs possible (at moment, at least), only sub-expression
   * stripping is possible */
  AST::DefaultASTVisitor::visit (field);

  auto &value = field.get_value ();
  if (value.is_marked_for_strip ())
    rust_error_at (value.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::StructExprStructFields &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says these are inner
   * attributes, not outer attributes of inner expr */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  // strip sub-exprs of path
  auto &struct_name = expr.get_struct_name ();
  visit (struct_name);
  if (struct_name.is_marked_for_strip ())
    rust_error_at (struct_name.get_locus (),
		   "cannot strip path in this position");

  /* spec does not specify whether expressions are allowed to be
   * stripped at top level of struct fields, but I wouldn't think
   * that they would be, so operating under the assumption that only
   * sub-expressions can be stripped. */
  AST::DefaultASTVisitor::visit (expr);

  /* struct base presumably can't be stripped, as the '..' is before
   * the expression. as such, can only strip sub-expressions. */
  if (expr.has_struct_base ())
    {
      auto &base_struct_expr = expr.get_struct_base ().get_base_struct ();
      base_struct_expr.accept_vis (*this);
      if (base_struct_expr.is_marked_for_strip ())
	rust_error_at (base_struct_expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }

  maybe_strip_struct_expr_fields (expr.get_fields ());
}

void
CfgStrip::visit (AST::StructExprStructBase &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says these are inner
   * attributes, not outer attributes of inner expr */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  // strip sub-exprs of path
  auto &struct_name = expr.get_struct_name ();
  visit (struct_name);
  if (struct_name.is_marked_for_strip ())
    rust_error_at (struct_name.get_locus (),
		   "cannot strip path in this position");

  /* struct base presumably can't be stripped, as the '..' is before
   * the expression. as such, can only strip sub-expressions. */
  rust_assert (!expr.get_struct_base ().is_invalid ());
  auto &base_struct_expr = expr.get_struct_base ().get_base_struct ();
  base_struct_expr.accept_vis (*this);
  if (base_struct_expr.is_marked_for_strip ())
    rust_error_at (base_struct_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::CallExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* should not be outer attrs on "function" expression - outer attrs
   * should be associated with call expr as a whole. only sub-expr
   * expansion is possible. */
  AST::DefaultASTVisitor::visit (expr);

  auto &function = expr.get_function_expr ();
  if (function.is_marked_for_strip ())
    rust_error_at (function.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  /* spec says outer attributes are specifically allowed for elements
   * of call expressions, so full stripping possible */
  // FIXME: Arthur: Figure out how to refactor this - This is similar to
  // expanding items in the crate or stmts in blocks
  maybe_strip_pointer_allow_strip (expr.get_params ());
}
void
CfgStrip::visit (AST::MethodCallExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* should not be outer attrs on "receiver" expression - outer attrs
   * should be associated with call expr as a whole. only sub-expr
   * expansion is possible. */
  AST::DefaultASTVisitor::visit (expr);

  auto &receiver = expr.get_receiver_expr ();
  if (receiver.is_marked_for_strip ())
    rust_error_at (receiver.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  auto &method_name = expr.get_method_name ();
  if (method_name.has_generic_args ())
    maybe_strip_generic_args (method_name.get_generic_args ());

  /* spec says outer attributes are specifically allowed for elements
   * of method call expressions, so full stripping possible */
  maybe_strip_pointer_allow_strip (expr.get_params ());
}
void
CfgStrip::visit (AST::FieldAccessExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* should not be outer attrs on "receiver" expression - outer attrs
   * should be associated with field expr as a whole. only sub-expr
   * expansion is possible. */
  AST::DefaultASTVisitor::visit (expr);

  auto &receiver = expr.get_receiver_expr ();
  if (receiver.is_marked_for_strip ())
    rust_error_at (receiver.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ClosureExprInner &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip closure parameters if required - this is specifically
   * allowed by spec */
  maybe_strip_closure_params (expr.get_params ());

  AST::DefaultASTVisitor::visit (expr);

  // can't strip expression itself, but can strip sub-expressions
  auto &definition_expr = expr.get_definition_expr ();
  if (definition_expr.is_marked_for_strip ())
    rust_error_at (definition_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::BlockExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip test based on inner attrs - spec says there are inner
   * attributes, not just outer attributes of inner stmts */
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  maybe_strip_pointer_allow_strip (expr.get_statements ());

  AST::DefaultASTVisitor::visit (expr);

  // strip tail expression if exists - can actually fully remove it
  if (expr.has_tail_expr ())
    {
      auto &tail_expr = expr.get_tail_expr ();

      if (tail_expr.is_marked_for_strip ())
	expr.strip_tail_expr ();
    }
}

void
CfgStrip::visit (AST::ClosureExprInnerTyped &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* strip closure parameters if required - this is specifically
   * allowed by spec */
  maybe_strip_closure_params (expr.get_params ());

  AST::DefaultASTVisitor::visit (expr);

  // can't strip return type, but can strip sub-types
  auto &type = expr.get_return_type ();

  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  // can't strip expression itself, but can strip sub-expressions
  auto &definition_block = expr.get_definition_block ();
  definition_block.accept_vis (*this);
  if (definition_block.is_marked_for_strip ())
    rust_error_at (definition_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ContinueExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }
}
void
CfgStrip::visit (AST::BreakExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }
  AST::DefaultASTVisitor::visit (expr);

  /* spec does not say that you can have outer attributes on
   * expression, so assuming you can't. stripping for sub-expressions
   * is the only thing that can be done */
  if (expr.has_break_expr ())
    {
      auto &break_expr = expr.get_break_expr ();

      if (break_expr.is_marked_for_strip ())
	rust_error_at (break_expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }
}
void
CfgStrip::visit (AST::RangeFromToExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */
  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_from_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_from_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before range exprs");
  if (expr.get_to_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_to_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::RangeFromExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * direct descendant expression, can strip ones below that */

  AST::DefaultASTVisitor::visit (expr);
  /* should have no possibility for outer attrs as would be parsed
   * with outer expr */
  auto &from_expr = expr.get_from_expr ();
  if (from_expr.is_marked_for_strip ())
    rust_error_at (from_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed before range exprs");
}
void
CfgStrip::visit (AST::RangeToExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * direct descendant expression, can strip ones below that */

  AST::DefaultASTVisitor::visit (expr);
  /* should syntactically not have outer attributes, though this may
   * not have worked in practice */
  auto &to_expr = expr.get_to_expr ();
  if (to_expr.is_marked_for_strip ())
    rust_error_at (to_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::RangeFromToInclExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * two direct descendant expressions, can strip ones below that */

  AST::DefaultASTVisitor::visit (expr);

  // ensure that they are not marked for strip
  if (expr.get_from_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_from_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes are never allowed "
		   "before range exprs");
  if (expr.get_to_expr ().is_marked_for_strip ())
    rust_error_at (expr.get_to_expr ().get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::RangeToInclExpr &expr)
{
  /* outer attributes never allowed before these. while cannot strip
   * direct descendant expression, can strip ones below that */

  AST::DefaultASTVisitor::visit (expr);
  /* should syntactically not have outer attributes, though this may
   * not have worked in practice */
  auto &to_expr = expr.get_to_expr ();
  if (to_expr.is_marked_for_strip ())
    rust_error_at (to_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ReturnExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  /* spec does not say that you can have outer attributes on
   * expression, so assuming you can't. stripping for sub-expressions
   * is the only thing that can be done */
  if (expr.has_returned_expr ())
    {
      auto &returned_expr = expr.get_returned_expr ();
      if (returned_expr.is_marked_for_strip ())
	rust_error_at (returned_expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }
  /* TODO: conceptually, you would maybe be able to remove a returned
   * expr - e.g. if you had conditional compilation returning void or
   * returning a type. On the other hand, I think that function
   * return type cannot be conditionally compiled, so I assumed you
   * can't do this either. */
}
void
CfgStrip::visit (AST::UnsafeBlockExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip block itself, but can strip sub-expressions
  auto &block_expr = expr.get_block_expr ();
  if (block_expr.is_marked_for_strip ())
    rust_error_at (block_expr.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::LoopExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip block itself, but can strip sub-expressions
  auto &loop_block = expr.get_loop_block ();
  if (loop_block.is_marked_for_strip ())
    rust_error_at (loop_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::WhileLoopExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);
  // can't strip predicate expr itself, but can strip sub-expressions
  auto &predicate_expr = expr.get_predicate_expr ();
  if (predicate_expr.is_marked_for_strip ())
    rust_error_at (predicate_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip block itself, but can strip sub-expressions
  auto &loop_block = expr.get_loop_block ();
  if (loop_block.is_marked_for_strip ())
    rust_error_at (loop_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::WhileLetLoopExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  for (auto &pattern : expr.get_patterns ())
    if (pattern->is_marked_for_strip ())
      rust_error_at (pattern->get_locus (),
		     "cannot strip pattern in this position");

  // can't strip scrutinee expr itself, but can strip sub-expressions
  auto &scrutinee_expr = expr.get_scrutinee_expr ();
  if (scrutinee_expr.is_marked_for_strip ())
    rust_error_at (scrutinee_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip block itself, but can strip sub-expressions
  auto &loop_block = expr.get_loop_block ();
  if (loop_block.is_marked_for_strip ())
    rust_error_at (loop_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::ForLoopExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);
  // strip sub-patterns of pattern
  auto &pattern = expr.get_pattern ();
  if (pattern.is_marked_for_strip ())
    rust_error_at (pattern.get_locus (),
		   "cannot strip pattern in this position");

  // can't strip scrutinee expr itself, but can strip sub-expressions
  auto &iterator_expr = expr.get_iterator_expr ();
  if (iterator_expr.is_marked_for_strip ())
    rust_error_at (iterator_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip block itself, but can strip sub-expressions
  auto &loop_block = expr.get_loop_block ();
  if (loop_block.is_marked_for_strip ())
    rust_error_at (loop_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::IfExpr &expr)
{
  // rust playground test shows that IfExpr does support outer attrs, at least
  // when used as statement

  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip condition expr itself, but can strip sub-expressions
  auto &condition_expr = expr.get_condition_expr ();
  if (condition_expr.is_marked_for_strip ())
    rust_error_at (condition_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip if block itself, but can strip sub-expressions
  auto &if_block = expr.get_if_block ();
  if (if_block.is_marked_for_strip ())
    rust_error_at (if_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::IfExprConseqElse &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip condition expr itself, but can strip sub-expressions
  auto &condition_expr = expr.get_condition_expr ();
  if (condition_expr.is_marked_for_strip ())
    rust_error_at (condition_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip if block itself, but can strip sub-expressions
  auto &if_block = expr.get_if_block ();
  if (if_block.is_marked_for_strip ())
    rust_error_at (if_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");

  // can't strip else block itself, but can strip sub-expressions
  auto &else_block = expr.get_else_block ();
  if (else_block.is_marked_for_strip ())
    rust_error_at (else_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::IfLetExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  for (auto &pattern : expr.get_patterns ())
    if (pattern->is_marked_for_strip ())
      rust_error_at (pattern->get_locus (),
		     "cannot strip pattern in this position");

  // can't strip value expr itself, but can strip sub-expressions
  auto &value_expr = expr.get_value_expr ();
  if (value_expr.is_marked_for_strip ())
    rust_error_at (value_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip if block itself, but can strip sub-expressions
  auto &if_block = expr.get_if_block ();
  if (if_block.is_marked_for_strip ())
    rust_error_at (if_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::IfLetExprConseqElse &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  for (auto &pattern : expr.get_patterns ())
    if (pattern->is_marked_for_strip ())
      rust_error_at (pattern->get_locus (),
		     "cannot strip pattern in this position");

  // can't strip value expr itself, but can strip sub-expressions
  auto &value_expr = expr.get_value_expr ();
  if (value_expr.is_marked_for_strip ())
    rust_error_at (value_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // can't strip if block itself, but can strip sub-expressions
  auto &if_block = expr.get_if_block ();
  if (if_block.is_marked_for_strip ())
    rust_error_at (if_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");

  // can't strip else block itself, but can strip sub-expressions
  auto &else_block = expr.get_else_block ();
  if (else_block.is_marked_for_strip ())
    rust_error_at (else_block.get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::MatchExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  // inner attr strip test
  expand_cfg_attrs (expr.get_inner_attrs ());
  if (fails_cfg_with_expand (expr.get_inner_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip scrutinee expr itself, but can strip sub-expressions
  auto &scrutinee_expr = expr.get_scrutinee_expr ();
  if (scrutinee_expr.is_marked_for_strip ())
    rust_error_at (scrutinee_expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");

  // strip match cases
  auto &match_cases = expr.get_match_cases ();
  for (auto it = match_cases.begin (); it != match_cases.end ();)
    {
      auto &match_case = *it;

      // strip match case based on outer attributes in match arm
      auto &match_arm = match_case.get_arm ();
      expand_cfg_attrs (match_arm.get_outer_attrs ());
      if (fails_cfg_with_expand (match_arm.get_outer_attrs ()))
	{
	  // strip match case
	  it = match_cases.erase (it);
	  continue;
	}

      for (auto &pattern : match_arm.get_patterns ())
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");

      /* assuming that guard expression cannot be stripped as
       * strictly speaking you would have to strip the whole guard to
       * make syntactical sense, which you can't do. as such, only
       * strip sub-expressions */
      if (match_arm.has_match_arm_guard ())
	{
	  auto &guard_expr = match_arm.get_guard_expr ();
	  if (guard_expr.is_marked_for_strip ())
	    rust_error_at (guard_expr.get_locus (),
			   "cannot strip expression in this position - outer "
			   "attributes not allowed");
	}

      // strip sub-expressions from match cases
      auto &case_expr = match_case.get_expr ();
      if (case_expr.is_marked_for_strip ())
	rust_error_at (case_expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");

      // increment to next case if haven't continued
      ++it;
    }
}

void
CfgStrip::visit (AST::AwaitExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  /* can't strip awaited expr itself, but can strip sub-expressions
   * - this is because you can't have no expr to await */
  auto &awaited_expr = expr.get_awaited_expr ();
  awaited_expr->accept_vis (*this);
  if (awaited_expr->is_marked_for_strip ())
    rust_error_at (awaited_expr->get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::AsyncBlockExpr &expr)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (expr.get_outer_attrs ());
  if (fails_cfg_with_expand (expr.get_outer_attrs ()))
    {
      expr.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (expr);

  // can't strip block itself, but can strip sub-expressions
  auto &block_expr = expr.get_block_expr ();
  if (block_expr->is_marked_for_strip ())
    rust_error_at (block_expr->get_locus (),
		   "cannot strip block expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::TypeParam &param)
{
  // outer attributes don't actually do anything, so ignore them

  AST::DefaultASTVisitor::visit (param);

  if (param.has_type () && param.get_type ().is_marked_for_strip ())
    rust_error_at (param.get_type ().get_locus (),
		   "cannot strip type in this position");
}

void
CfgStrip::visit (AST::TypeBoundWhereClauseItem &item)
{
  // for lifetimes shouldn't require
  AST::DefaultASTVisitor::visit (item);

  auto &type = item.get_type ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");
}

void
CfgStrip::visit (AST::Module &module)
{
  // strip test based on outer attrs
  expand_cfg_attrs (module.get_outer_attrs ());
  if (fails_cfg_with_expand (module.get_outer_attrs ()))
    {
      module.mark_for_strip ();
      return;
    }

  // A loaded module might have inner attributes
  if (module.get_kind () == AST::Module::ModuleKind::LOADED)
    {
      // strip test based on inner attrs
      expand_cfg_attrs (module.get_inner_attrs ());
      if (fails_cfg_with_expand (module.get_inner_attrs ()))
	{
	  module.mark_for_strip ();
	  return;
	}
    }

  // strip items if required
  maybe_strip_pointer_allow_strip (module.get_items ());
}

void
CfgStrip::visit (AST::ExternCrate &extern_crate)
{
  // strip test based on outer attrs
  expand_cfg_attrs (extern_crate.get_outer_attrs ());
  if (fails_cfg_with_expand (extern_crate.get_outer_attrs ()))
    {
      extern_crate.mark_for_strip ();
      return;
    }

  if (!extern_crate.references_self ())
    {
      Session &session = Session::get_instance ();
      session.load_extern_crate (extern_crate.get_referenced_crate (),
				 extern_crate.get_locus ());
    }
}

void
CfgStrip::visit (AST::UseDeclaration &use_decl)
{
  // strip test based on outer attrs
  expand_cfg_attrs (use_decl.get_outer_attrs ());
  if (fails_cfg_with_expand (use_decl.get_outer_attrs ()))
    {
      use_decl.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::Function &function)
{
  // initial test based on outer attrs
  expand_cfg_attrs (function.get_outer_attrs ());
  if (fails_cfg_with_expand (function.get_outer_attrs ()))
    {
      function.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (function);

  /* strip function parameters if required - this is specifically
   * allowed by spec */
  maybe_strip_function_params (function.get_function_params ());

  if (function.has_return_type ())
    {
      auto &return_type = function.get_return_type ();
      if (return_type.is_marked_for_strip ())
	rust_error_at (return_type.get_locus (),
		       "cannot strip type in this position");
    }

  /* body should always exist - if error state, should have returned
   * before now */
  // can't strip block itself, but can strip sub-expressions
  if (function.has_body ())
    {
      auto &block_expr = function.get_definition ();
      if (block_expr.value ()->is_marked_for_strip ())
	rust_error_at (block_expr.value ()->get_locus (),
		       "cannot strip block expression in this position - outer "
		       "attributes not allowed");
    }
}

void
CfgStrip::visit (AST::TypeAlias &type_alias)
{
  // initial test based on outer attrs
  expand_cfg_attrs (type_alias.get_outer_attrs ());
  if (fails_cfg_with_expand (type_alias.get_outer_attrs ()))
    {
      type_alias.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (type_alias);

  auto &type = type_alias.get_type_aliased ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");
}

void
CfgStrip::visit (AST::StructStruct &struct_item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (struct_item.get_outer_attrs ());
  if (fails_cfg_with_expand (struct_item.get_outer_attrs ()))
    {
      struct_item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (struct_item);

  /* strip struct fields if required - this is presumably
   * allowed by spec */
  maybe_strip_struct_fields (struct_item.get_fields ());
}
void
CfgStrip::visit (AST::TupleStruct &tuple_struct)
{
  // initial test based on outer attrs
  expand_cfg_attrs (tuple_struct.get_outer_attrs ());
  if (fails_cfg_with_expand (tuple_struct.get_outer_attrs ()))
    {
      tuple_struct.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (tuple_struct);

  /* strip struct fields if required - this is presumably
   * allowed by spec */
  maybe_strip_tuple_fields (tuple_struct.get_fields ());
}
void
CfgStrip::visit (AST::EnumItem &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::EnumItemTuple &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  /* strip item fields if required - this is presumably
   * allowed by spec */
  maybe_strip_tuple_fields (item.get_tuple_fields ());
}

void
CfgStrip::visit (AST::EnumItemStruct &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  /* strip item fields if required - this is presumably
   * allowed by spec */
  maybe_strip_struct_fields (item.get_struct_fields ());
}

void
CfgStrip::visit (AST::EnumItemDiscriminant &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (item);
  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &expr = item.get_expr ();
  if (expr.is_marked_for_strip ())
    rust_error_at (expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}
void
CfgStrip::visit (AST::Enum &enum_item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (enum_item.get_outer_attrs ());
  if (fails_cfg_with_expand (enum_item.get_outer_attrs ()))
    {
      enum_item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (enum_item);

  /* strip enum fields if required - this is presumably
   * allowed by spec */
  maybe_strip_pointer_allow_strip (enum_item.get_variants ());
}
void
CfgStrip::visit (AST::Union &union_item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (union_item.get_outer_attrs ());
  if (fails_cfg_with_expand (union_item.get_outer_attrs ()))
    {
      union_item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (union_item);

  /* strip union fields if required - this is presumably
   * allowed by spec */
  maybe_strip_struct_fields (union_item.get_variants ());
}
void
CfgStrip::visit (AST::ConstantItem &const_item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (const_item.get_outer_attrs ());
  if (fails_cfg_with_expand (const_item.get_outer_attrs ()))
    {
      const_item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (const_item);

  // strip any sub-types
  auto &type = const_item.get_type ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  if (const_item.has_expr ())
    {
      auto &expr = const_item.get_expr ();
      if (expr.is_marked_for_strip ())
	rust_error_at (expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }
}
void
CfgStrip::visit (AST::StaticItem &static_item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (static_item.get_outer_attrs ());
  if (fails_cfg_with_expand (static_item.get_outer_attrs ()))
    {
      static_item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (static_item);

  // strip any sub-types
  auto &type = static_item.get_type ();

  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped. */
  auto &expr = static_item.get_expr ();
  if (expr.is_marked_for_strip ())
    rust_error_at (expr.get_locus (),
		   "cannot strip expression in this position - outer "
		   "attributes not allowed");
}

void
CfgStrip::visit (AST::TraitItemConst &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (item);

  // strip any sub-types
  auto &type = item.get_type ();

  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped */
  if (item.has_expression ())
    {
      auto &expr = item.get_expr ();
      if (expr.is_marked_for_strip ())
	rust_error_at (expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }
}

void
CfgStrip::visit (AST::TraitItemType &item)
{
  // initial test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (item);
}

void
CfgStrip::visit (AST::Trait &trait)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (trait.get_outer_attrs ());
  if (fails_cfg_with_expand (trait.get_outer_attrs ()))
    {
      trait.mark_for_strip ();
      return;
    }

  // strip test based on inner attrs
  expand_cfg_attrs (trait.get_inner_attrs ());
  if (fails_cfg_with_expand (trait.get_inner_attrs ()))
    {
      trait.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (trait);

  maybe_strip_pointer_allow_strip (trait.get_trait_items ());
}

void
CfgStrip::visit (AST::InherentImpl &impl)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (impl.get_outer_attrs ());
  if (fails_cfg_with_expand (impl.get_outer_attrs ()))
    {
      impl.mark_for_strip ();
      return;
    }

  // strip test based on inner attrs
  expand_cfg_attrs (impl.get_inner_attrs ());
  if (fails_cfg_with_expand (impl.get_inner_attrs ()))
    {
      impl.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (impl);

  auto &type = impl.get_type ();

  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  maybe_strip_pointer_allow_strip (impl.get_impl_items ());
}

void
CfgStrip::visit (AST::TraitImpl &impl)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (impl.get_outer_attrs ());
  if (fails_cfg_with_expand (impl.get_outer_attrs ()))
    {
      impl.mark_for_strip ();
      return;
    }

  // strip test based on inner attrs
  expand_cfg_attrs (impl.get_inner_attrs ());
  if (fails_cfg_with_expand (impl.get_inner_attrs ()))
    {
      impl.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (impl);

  auto &type = impl.get_type ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");

  auto &trait_path = impl.get_trait_path ();
  visit (trait_path);
  if (trait_path.is_marked_for_strip ())
    rust_error_at (trait_path.get_locus (),
		   "cannot strip typepath in this position");

  maybe_strip_pointer_allow_strip (impl.get_impl_items ());
}

void
CfgStrip::visit (AST::ExternalTypeItem &item)
{
  expand_cfg_attrs (item.get_outer_attrs ());

  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    item.mark_for_strip ();

  // TODO: Can we do anything like expand a macro here?
  // extern "C" { type ffi_ty!(); }
  // ?
}

void
CfgStrip::visit (AST::ExternalStaticItem &item)
{
  // strip test based on outer attrs
  expand_cfg_attrs (item.get_outer_attrs ());
  if (fails_cfg_with_expand (item.get_outer_attrs ()))
    {
      item.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (item);

  auto &type = item.get_type ();
  if (type.is_marked_for_strip ())
    rust_error_at (type.get_locus (), "cannot strip type in this position");
}

void
CfgStrip::visit (AST::ExternBlock &block)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (block.get_outer_attrs ());
  if (fails_cfg_with_expand (block.get_outer_attrs ()))
    {
      block.mark_for_strip ();
      return;
    }

  // strip test based on inner attrs
  expand_cfg_attrs (block.get_inner_attrs ());
  if (fails_cfg_with_expand (block.get_inner_attrs ()))
    {
      block.mark_for_strip ();
      return;
    }

  maybe_strip_pointer_allow_strip (block.get_extern_items ());
}

void
CfgStrip::visit (AST::MacroRulesDefinition &rules_def)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (rules_def.get_outer_attrs ());
  if (fails_cfg_with_expand (rules_def.get_outer_attrs ()))
    {
      rules_def.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::IdentifierPattern &pattern)
{
  // can only strip sub-patterns of the inner pattern to bind
  if (!pattern.has_pattern_to_bind ())
    return;

  AST::DefaultASTVisitor::visit (pattern);

  auto &sub_pattern = pattern.get_pattern_to_bind ();
  if (sub_pattern.is_marked_for_strip ())
    rust_error_at (sub_pattern.get_locus (),
		   "cannot strip pattern in this position");
}

void
CfgStrip::visit (AST::RangePatternBoundPath &bound)
{
  // can expand path, but not strip it directly
  auto &path = bound.get_path ();
  visit (path);
  if (path.is_marked_for_strip ())
    rust_error_at (path.get_locus (), "cannot strip path in this position");
}

void
CfgStrip::visit (AST::RangePatternBoundQualPath &bound)
{
  // can expand path, but not strip it directly
  auto &path = bound.get_qualified_path ();
  visit (path);
  if (path.is_marked_for_strip ())
    rust_error_at (path.get_locus (), "cannot strip path in this position");
}

void
CfgStrip::visit (AST::ReferencePattern &pattern)
{
  AST::DefaultASTVisitor::visit (pattern);

  auto &sub_pattern = pattern.get_referenced_pattern ();
  if (sub_pattern.is_marked_for_strip ())
    rust_error_at (sub_pattern.get_locus (),
		   "cannot strip pattern in this position");
}
void
CfgStrip::visit (AST::StructPatternFieldTuplePat &field)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (field.get_outer_attrs ());
  if (fails_cfg_with_expand (field.get_outer_attrs ()))
    {
      field.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (field);

  // strip sub-patterns (can't strip top-level pattern)
  auto &sub_pattern = field.get_index_pattern ();
  if (sub_pattern.is_marked_for_strip ())
    rust_error_at (sub_pattern.get_locus (),
		   "cannot strip pattern in this position");
}

void
CfgStrip::visit (AST::StructPatternFieldIdentPat &field)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (field.get_outer_attrs ());
  if (fails_cfg_with_expand (field.get_outer_attrs ()))
    {
      field.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (field);
  // strip sub-patterns (can't strip top-level pattern)
  auto &sub_pattern = field.get_ident_pattern ();
  if (sub_pattern.is_marked_for_strip ())
    rust_error_at (sub_pattern.get_locus (),
		   "cannot strip pattern in this position");
}
void
CfgStrip::visit (AST::StructPatternFieldIdent &field)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (field.get_outer_attrs ());
  if (fails_cfg_with_expand (field.get_outer_attrs ()))
    {
      field.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::StructPattern &pattern)
{
  // expand (but don't strip) path
  auto &path = pattern.get_path ();
  visit (path);
  if (path.is_marked_for_strip ())
    rust_error_at (path.get_locus (), "cannot strip path in this position");

  /* TODO: apparently struct pattern fields can have outer attrs. so can they
   * be stripped? */
  if (!pattern.has_struct_pattern_elems ())
    return;

  auto &elems = pattern.get_struct_pattern_elems ();

  // assuming you can strip struct pattern fields
  maybe_strip_pointer_allow_strip (elems.get_struct_pattern_fields ());

  // assuming you can strip the ".." part
  if (elems.has_etc ())
    {
      expand_cfg_attrs (elems.get_etc_outer_attrs ());
      if (fails_cfg_with_expand (elems.get_etc_outer_attrs ()))
	elems.strip_etc ();
    }
}

void
CfgStrip::visit (AST::TupleStructItemsNoRange &tuple_items)
{
  AST::DefaultASTVisitor::visit (tuple_items);
  // can't strip individual patterns, only sub-patterns
  for (auto &pattern : tuple_items.get_patterns ())
    {
      if (pattern->is_marked_for_strip ())
	rust_error_at (pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}
void
CfgStrip::visit (AST::TupleStructItemsRange &tuple_items)
{
  AST::DefaultASTVisitor::visit (tuple_items);
  // can't strip individual patterns, only sub-patterns
  for (auto &lower_pattern : tuple_items.get_lower_patterns ())
    {
      if (lower_pattern->is_marked_for_strip ())
	rust_error_at (lower_pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
  for (auto &upper_pattern : tuple_items.get_upper_patterns ())
    {
      if (upper_pattern->is_marked_for_strip ())
	rust_error_at (upper_pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}

void
CfgStrip::visit (AST::TupleStructPattern &pattern)
{
  // expand (but don't strip) path
  auto &path = pattern.get_path ();
  visit (path);
  if (path.is_marked_for_strip ())
    rust_error_at (path.get_locus (), "cannot strip path in this position");

  AST::DefaultASTVisitor::visit (pattern);
}

void
CfgStrip::visit (AST::TuplePatternItemsMultiple &tuple_items)
{
  AST::DefaultASTVisitor::visit (tuple_items);

  // can't strip individual patterns, only sub-patterns
  for (auto &pattern : tuple_items.get_patterns ())
    {
      if (pattern->is_marked_for_strip ())
	rust_error_at (pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}

void
CfgStrip::visit (AST::TuplePatternItemsRanged &tuple_items)
{
  AST::DefaultASTVisitor::visit (tuple_items);

  // can't strip individual patterns, only sub-patterns
  for (auto &lower_pattern : tuple_items.get_lower_patterns ())
    {
      if (lower_pattern->is_marked_for_strip ())
	rust_error_at (lower_pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
  for (auto &upper_pattern : tuple_items.get_upper_patterns ())
    {
      if (upper_pattern->is_marked_for_strip ())
	rust_error_at (upper_pattern->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}

void
CfgStrip::visit (AST::GroupedPattern &pattern)
{
  AST::DefaultASTVisitor::visit (pattern);
  // can't strip inner pattern, only sub-patterns
  auto &pattern_in_parens = pattern.get_pattern_in_parens ();

  if (pattern_in_parens.is_marked_for_strip ())
    rust_error_at (pattern_in_parens.get_locus (),
		   "cannot strip pattern in this position");
}

void
CfgStrip::visit (AST::SlicePattern &pattern)
{
  AST::DefaultASTVisitor::visit (pattern);
  // can't strip individual patterns, only sub-patterns
  for (auto &item : pattern.get_items ())
    {
      if (item->is_marked_for_strip ())
	rust_error_at (item->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}

void
CfgStrip::visit (AST::AltPattern &pattern)
{
  AST::DefaultASTVisitor::visit (pattern);
  // can't strip individual patterns, only sub-patterns
  for (auto &alt : pattern.get_alts ())
    {
      if (alt->is_marked_for_strip ())
	rust_error_at (alt->get_locus (),
		       "cannot strip pattern in this position");
      // TODO: quit stripping now? or keep going?
    }
}

void
CfgStrip::visit (AST::LetStmt &stmt)
{
  // initial strip test based on outer attrs
  expand_cfg_attrs (stmt.get_outer_attrs ());
  if (fails_cfg_with_expand (stmt.get_outer_attrs ()))
    {
      stmt.mark_for_strip ();
      return;
    }

  AST::DefaultASTVisitor::visit (stmt);
  // can't strip pattern, but call for sub-patterns
  auto &pattern = stmt.get_pattern ();
  if (pattern.is_marked_for_strip ())
    rust_error_at (pattern.get_locus (),
		   "cannot strip pattern in this position");

  // similar for type
  if (stmt.has_type ())
    {
      auto &type = stmt.get_type ();

      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");
    }

  /* strip any internal sub-expressions - expression itself isn't
   * allowed to have external attributes in this position so can't be
   * stripped */
  if (stmt.has_init_expr ())
    {
      auto &init_expr = stmt.get_init_expr ();

      if (init_expr.is_marked_for_strip ())
	rust_error_at (init_expr.get_locus (),
		       "cannot strip expression in this position - outer "
		       "attributes not allowed");
    }
}

void
CfgStrip::visit (AST::ExprStmt &stmt)
{
  // outer attributes associated with expr, so rely on expr

  // guard - should prevent null pointer expr
  if (stmt.is_marked_for_strip ())
    return;

  AST::DefaultASTVisitor::visit (stmt);
  // strip if expr is to be stripped
  auto &expr = stmt.get_expr ();
  if (expr.is_marked_for_strip ())
    {
      stmt.mark_for_strip ();
      return;
    }
}

void
CfgStrip::visit (AST::TraitBound &bound)
{
  // nothing in for lifetimes to strip

  // expand but don't strip type path
  auto &path = bound.get_type_path ();
  visit (path);
  if (path.is_marked_for_strip ())
    rust_error_at (path.get_locus (),
		   "cannot strip type path in this position");
}

void
CfgStrip::visit (AST::ParenthesisedType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // expand but don't strip inner type
  auto &inner_type = type.get_type_in_parens ();
  if (inner_type->is_marked_for_strip ())
    rust_error_at (inner_type->get_locus (),
		   "cannot strip type in this position");
}

void
CfgStrip::visit (AST::TupleType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // TODO: assuming that types can't be stripped as types don't have outer
  // attributes
  for (auto &elem_type : type.get_elems ())
    {
      if (elem_type->is_marked_for_strip ())
	rust_error_at (elem_type->get_locus (),
		       "cannot strip type in this position");
    }
}

void
CfgStrip::visit (AST::RawPointerType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // expand but don't strip type pointed to
  auto &pointed_type = type.get_type_pointed_to ();
  if (pointed_type.is_marked_for_strip ())
    rust_error_at (pointed_type.get_locus (),
		   "cannot strip type in this position");
}

void
CfgStrip::visit (AST::ReferenceType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // expand but don't strip type referenced
  auto &referenced_type = type.get_type_referenced ();
  if (referenced_type.is_marked_for_strip ())
    rust_error_at (referenced_type.get_locus (),
		   "cannot strip type in this position");
}

void
CfgStrip::visit (AST::ArrayType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // expand but don't strip type referenced
  auto &base_type = type.get_elem_type ();
  if (base_type.is_marked_for_strip ())
    rust_error_at (base_type.get_locus (),
		   "cannot strip type in this position");

  // same for expression
  auto &size_expr = type.get_size_expr ();
  if (size_expr.is_marked_for_strip ())
    rust_error_at (size_expr.get_locus (),
		   "cannot strip expression in this position");
}
void
CfgStrip::visit (AST::SliceType &type)
{
  AST::DefaultASTVisitor::visit (type);
  // expand but don't strip elem type
  auto &elem_type = type.get_elem_type ();
  if (elem_type.is_marked_for_strip ())
    rust_error_at (elem_type.get_locus (),
		   "cannot strip type in this position");
}

void
CfgStrip::visit (AST::BareFunctionType &type)
{
  // seem to be no generics
  AST::DefaultASTVisitor::visit (type);

  // presumably function params can be stripped
  auto &params = type.get_function_params ();
  for (auto it = params.begin (); it != params.end ();)
    {
      auto &param = *it;

      auto &param_attrs = param.get_outer_attrs ();
      expand_cfg_attrs (param_attrs);
      if (fails_cfg_with_expand (param_attrs))
	{
	  it = params.erase (it);
	  continue;
	}

      auto &type = param.get_type ();
      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");

      // increment if nothing else happens
      ++it;
    }

  /* TODO: assuming that variadic nature cannot be stripped. If this
   * is not true, then have code here to do so. */

  if (type.has_return_type ())
    {
      // FIXME: Can we have type expansion in this position?
      // In that case, we need to handle AST::TypeNoBounds on top of just
      // AST::Types
      auto &return_type = type.get_return_type ();
      if (return_type.is_marked_for_strip ())
	rust_error_at (return_type.get_locus (),
		       "cannot strip type in this position");
    }

  // no where clause, apparently
}

void
CfgStrip::visit (AST::SelfParam &param)
{
  AST::DefaultASTVisitor::visit (param);

  if (param.has_type ())
    {
      auto &type = param.get_type ();
      if (type.is_marked_for_strip ())
	rust_error_at (type.get_locus (), "cannot strip type in this position");
    }
  /* TODO: maybe check for invariants being violated - e.g. both type and
   * lifetime? */
}

} // namespace Rust
