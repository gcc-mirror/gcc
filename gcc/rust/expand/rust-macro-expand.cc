// Copyright (C) 2020 Free Software Foundation, Inc.

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

#include "rust-macro-expand.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-diagnostics.h"

namespace Rust {
// Visitor used to expand attributes.
class AttrVisitor : public AST::ASTVisitor
{
private:
  MacroExpander &expander;

public:
  AttrVisitor (MacroExpander &expander) : expander (expander) {}

  void expand_struct_fields (std::vector<AST::StructField> &fields)
  {
    for (auto it = fields.begin (); it != fields.end ();)
      {
	auto &field = *it;

	auto &field_attrs = field.get_outer_attrs ();
	expander.expand_cfg_attrs (field_attrs);
	if (expander.fails_cfg_with_expand (field_attrs))
	  {
	    it = fields.erase (it);
	    continue;
	  }

	// expand sub-types of type, but can't strip type itself
	auto &type = field.get_field_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");

	// if nothing else happens, increment
	++it;
      }
  }

  void expand_tuple_fields (std::vector<AST::TupleField> &fields)
  {
    for (auto it = fields.begin (); it != fields.end ();)
      {
	auto &field = *it;

	auto &field_attrs = field.get_outer_attrs ();
	expander.expand_cfg_attrs (field_attrs);
	if (expander.fails_cfg_with_expand (field_attrs))
	  {
	    it = fields.erase (it);
	    continue;
	  }

	// expand sub-types of type, but can't strip type itself
	auto &type = field.get_field_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");

	// if nothing else happens, increment
	++it;
      }
  }

  void expand_function_params (std::vector<AST::FunctionParam> &params)
  {
    for (auto it = params.begin (); it != params.end ();)
      {
	auto &param = *it;

	auto &param_attrs = param.get_outer_attrs ();
	expander.expand_cfg_attrs (param_attrs);
	if (expander.fails_cfg_with_expand (param_attrs))
	  {
	    it = params.erase (it);
	    continue;
	  }

	// TODO: should an unwanted strip lead to break out of loop?
	auto &pattern = param.get_pattern ();
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");

	auto &type = param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");

	// increment
	++it;
      }
  }

  void expand_generic_args (AST::GenericArgs &args)
  {
    // lifetime args can't be expanded

    // expand type args - strip sub-types only
    for (auto &type : args.get_type_args ())
      {
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    // expand binding args - strip sub-types only
    for (auto &binding : args.get_binding_args ())
      {
	auto &type = binding.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }
  }

  void expand_qualified_path_type (AST::QualifiedPathType &path_type)
  {
    auto &type = path_type.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    if (path_type.has_as_clause ())
      {
	auto &type_path = path_type.get_as_type_path ();
	visit (type_path);
	if (type_path.is_marked_for_strip ())
	  rust_error_at (type_path.get_locus (),
			 "cannot strip type path in this position");
      }
  }

  void expand_closure_params (std::vector<AST::ClosureParam> &params)
  {
    for (auto it = params.begin (); it != params.end ();)
      {
	auto &param = *it;

	auto &param_attrs = param.get_outer_attrs ();
	expander.expand_cfg_attrs (param_attrs);
	if (expander.fails_cfg_with_expand (param_attrs))
	  {
	    it = params.erase (it);
	    continue;
	  }

	auto &pattern = param.get_pattern ();
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");

	if (param.has_type_given ())
	  {
	    auto &type = param.get_type ();
	    type->accept_vis (*this);
	    if (type->is_marked_for_strip ())
	      rust_error_at (type->get_locus_slow (),
			     "cannot strip type in this position");
	  }

	// increment if found nothing else so far
	++it;
      }
  }

  void expand_self_param (AST::SelfParam &self_param)
  {
    if (self_param.has_type ())
      {
	auto &type = self_param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }
    /* TODO: maybe check for invariants being violated - e.g. both type and
     * lifetime? */
  }

  void expand_where_clause (AST::WhereClause &where_clause)
  {
    // items cannot be stripped conceptually, so just accept visitor
    for (auto &item : where_clause.get_items ())
      item->accept_vis (*this);
  }

  void expand_trait_function_decl (AST::TraitFunctionDecl &decl)
  {
    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : decl.get_generic_params ())
      param->accept_vis (*this);

    /* strip function parameters if required - this is specifically
     * allowed by spec */
    expand_function_params (decl.get_function_params ());

    if (decl.has_return_type ())
      {
	auto &return_type = decl.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (decl.has_where_clause ())
      expand_where_clause (decl.get_where_clause ());
  }

  void expand_trait_method_decl (AST::TraitMethodDecl &decl)
  {
    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : decl.get_generic_params ())
      param->accept_vis (*this);

    /* assuming you can't strip self param - wouldn't be a method
     * anymore. spec allows outer attrs on self param, but doesn't
     * specify whether cfg is used. */
    expand_self_param (decl.get_self_param ());

    /* strip function parameters if required - this is specifically
     * allowed by spec */
    expand_function_params (decl.get_function_params ());

    if (decl.has_return_type ())
      {
	auto &return_type = decl.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (decl.has_where_clause ())
      expand_where_clause (decl.get_where_clause ());
  }

  template <typename T> void expand_pointer_allow_strip (T &values)
  {
    for (auto it = values.begin (); it != values.end ();)
      {
	auto &value = *it;

	// mark for stripping if required
	value->accept_vis (*this);

	if (value->is_marked_for_strip ())
	  it = values.erase (it);
	else
	  ++it;
      }
  }

  void visit (AST::Token &) override
  {
    // shouldn't require?
  }
  void visit (AST::DelimTokenTree &) override
  {
    // shouldn't require?
  }
  void visit (AST::AttrInputMetaItemContainer &) override
  {
    // shouldn't require?
  }
  void visit (AST::IdentifierExpr &ident_expr) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (ident_expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (ident_expr.get_outer_attrs ()))
      {
	ident_expr.mark_for_strip ();
	return;
      }
  }
  void visit (AST::Lifetime &) override
  {
    // shouldn't require?
  }
  void visit (AST::LifetimeParam &) override
  {
    // supposedly does not require - cfg does nothing
  }
  void visit (AST::MacroInvocationSemi &macro_invoc) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (macro_invoc.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (macro_invoc.get_outer_attrs ()))
      {
	macro_invoc.mark_for_strip ();
	return;
      }

    // can't strip simple path

    // I don't think any macro token trees can be stripped in any way

    // TODO: maybe have cfg! macro stripping behaviour here?
  }

  void visit (AST::PathInExpression &path) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (path.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (path.get_outer_attrs ()))
      {
	path.mark_for_strip ();
	return;
      }

    for (auto &segment : path.get_segments ())
      {
	if (segment.has_generic_args ())
	  expand_generic_args (segment.get_generic_args ());
      }
  }
  void visit (AST::TypePathSegment &) override
  {
    // shouldn't require
  }
  void visit (AST::TypePathSegmentGeneric &segment) override
  {
    // TODO: strip inside generic args

    if (!segment.has_generic_args ())
      return;

    expand_generic_args (segment.get_generic_args ());
  }
  void visit (AST::TypePathSegmentFunction &segment) override
  {
    auto &type_path_function = segment.get_type_path_function ();

    for (auto &type : type_path_function.get_params ())
      {
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (type_path_function.has_return_type ())
      {
	auto &return_type = type_path_function.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }
  }
  void visit (AST::TypePath &path) override
  {
    // this shouldn't strip any segments, but can strip inside them
    for (auto &segment : path.get_segments ())
      segment->accept_vis (*this);
  }
  void visit (AST::QualifiedPathInExpression &path) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (path.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (path.get_outer_attrs ()))
      {
	path.mark_for_strip ();
	return;
      }

    expand_qualified_path_type (path.get_qualified_path_type ());

    for (auto &segment : path.get_segments ())
      {
	if (segment.has_generic_args ())
	  expand_generic_args (segment.get_generic_args ());
      }
  }
  void visit (AST::QualifiedPathInType &path) override
  {
    expand_qualified_path_type (path.get_qualified_path_type ());

    // this shouldn't strip any segments, but can strip inside them
    for (auto &segment : path.get_segments ())
      segment->accept_vis (*this);
  }

  void visit (AST::LiteralExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }
  }
  void visit (AST::AttrInputLiteral &) override
  {
    // shouldn't require?
  }
  void visit (AST::MetaItemLitExpr &) override
  {
    // shouldn't require?
  }
  void visit (AST::MetaItemPathLit &) override
  {
    // shouldn't require?
  }
  void visit (AST::BorrowExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &borrowed_expr = expr.get_borrowed_expr ();
    borrowed_expr->accept_vis (*this);
    if (borrowed_expr->is_marked_for_strip ())
      rust_error_at (borrowed_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::DereferenceExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &dereferenced_expr = expr.get_dereferenced_expr ();
    dereferenced_expr->accept_vis (*this);
    if (dereferenced_expr->is_marked_for_strip ())
      rust_error_at (dereferenced_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ErrorPropagationExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &propagating_expr = expr.get_propagating_expr ();
    propagating_expr->accept_vis (*this);
    if (propagating_expr->is_marked_for_strip ())
      rust_error_at (propagating_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::NegationExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &negated_expr = expr.get_negated_expr ();
    negated_expr->accept_vis (*this);
    if (negated_expr->is_marked_for_strip ())
      rust_error_at (negated_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ArithmeticOrLogicalExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_left_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_right_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_left_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_left_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ComparisonExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_left_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_right_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_left_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_left_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::LazyBooleanExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_left_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_right_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_left_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_left_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::TypeCastExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * direct descendant expression, can strip ones below that */

    auto &casted_expr = expr.get_casted_expr ();
    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    casted_expr->accept_vis (*this);

    // ensure that they are not marked for strip
    if (casted_expr->is_marked_for_strip ())
      rust_error_at (casted_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed before cast exprs");

    // TODO: strip sub-types of type
    auto &type = expr.get_type_to_cast_to ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::AssignmentExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_left_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_right_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_left_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_left_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::CompoundAssignmentExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_left_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_right_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_left_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_left_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::GroupedExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &inner_expr = expr.get_expr_in_parens ();
    inner_expr->accept_vis (*this);
    if (inner_expr->is_marked_for_strip ())
      rust_error_at (inner_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ArrayElemsValues &elems) override
  {
    /* apparently outer attributes are allowed in "elements of array
     * expressions" according to spec */
    expand_pointer_allow_strip (elems.get_values ());
  }
  void visit (AST::ArrayElemsCopied &elems) override
  {
    /* apparently outer attributes are allowed in "elements of array
     * expressions" according to spec. on the other hand, it would not
     * make conceptual sense to be able to remove either expression. As
     * such, not implementing. TODO clear up the ambiguity here */

    // only intend stripping for internal sub-expressions
    auto &copied_expr = elems.get_elem_to_copy ();
    copied_expr->accept_vis (*this);
    if (copied_expr->is_marked_for_strip ())
      rust_error_at (copied_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    auto &copy_count = elems.get_num_copies ();
    copy_count->accept_vis (*this);
    if (copy_count->is_marked_for_strip ())
      rust_error_at (copy_count->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ArrayExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says there are separate
     * inner attributes, not just outer attributes of inner exprs */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* assuming you can't strip away the ArrayElems type, but can strip
     * internal expressions and whatever */
    expr.get_array_elems ()->accept_vis (*this);
  }
  void visit (AST::ArrayIndexExpr &expr) override
  {
    /* it is unclear whether outer attributes are supposed to be
     * allowed, but conceptually it wouldn't make much sense, but
     * having expansion code anyway. TODO */
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &array_expr = expr.get_array_expr ();
    array_expr->accept_vis (*this);
    if (array_expr->is_marked_for_strip ())
      rust_error_at (array_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    auto &index_expr = expr.get_index_expr ();
    index_expr->accept_vis (*this);
    if (index_expr->is_marked_for_strip ())
      rust_error_at (index_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::TupleExpr &expr) override
  {
    /* according to spec, outer attributes are allowed on "elements of
     * tuple expressions" */

    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* apparently outer attributes are allowed in "elements of tuple
     * expressions" according to spec */
    expand_pointer_allow_strip (expr.get_tuple_elems ());
  }
  void visit (AST::TupleIndexExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* wouldn't strip this directly (as outer attrs should be
     * associated with this level), but any sub-expressions would be
     * stripped. Thus, no need to erase when strip check called. */
    auto &tuple_expr = expr.get_tuple_expr ();
    tuple_expr->accept_vis (*this);
    if (tuple_expr->is_marked_for_strip ())
      rust_error_at (tuple_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::StructExprStruct &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
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
  void visit (AST::StructExprFieldIdentifier &) override
  {
    // as no attrs (at moment, at least), no stripping possible
  }
  void visit (AST::StructExprFieldIdentifierValue &field) override
  {
    /* as no attrs possible (at moment, at least), only sub-expression
     * stripping is possible */
    auto &value = field.get_value ();
    value->accept_vis (*this);
    if (value->is_marked_for_strip ())
      rust_error_at (value->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::StructExprFieldIndexValue &field) override
  {
    /* as no attrs possible (at moment, at least), only sub-expression
     * stripping is possible */
    auto &value = field.get_value ();
    value->accept_vis (*this);
    if (value->is_marked_for_strip ())
      rust_error_at (value->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::StructExprStructFields &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
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
    for (auto &field : expr.get_fields ())
      {
	field->accept_vis (*this);
	// shouldn't strip in this
      }

    /* struct base presumably can't be stripped, as the '..' is before
     * the expression. as such, can only strip sub-expressions. */
    if (expr.has_struct_base ())
      {
	auto &base_struct_expr = expr.get_struct_base ().get_base_struct ();
	base_struct_expr->accept_vis (*this);
	if (base_struct_expr->is_marked_for_strip ())
	  rust_error_at (base_struct_expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");
      }
  }
  void visit (AST::StructExprStructBase &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
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
    base_struct_expr->accept_vis (*this);
    if (base_struct_expr->is_marked_for_strip ())
      rust_error_at (base_struct_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::StructExprTuple &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says these are inner
     * attributes, not outer attributes of inner expr */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
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

    /* spec says outer attributes are specifically allowed for elements
     * of tuple-style struct expressions, so full stripping possible */
    expand_pointer_allow_strip (expr.get_elems ());
  }
  void visit (AST::StructExprUnit &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
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
  void visit (AST::EnumExprFieldIdentifier &) override
  {
    // as no attrs (at moment, at least), no stripping possible
  }
  void visit (AST::EnumExprFieldIdentifierValue &field) override
  {
    /* as no attrs possible (at moment, at least), only sub-expression
     * stripping is possible */
    auto &value = field.get_value ();
    value->accept_vis (*this);
    if (value->is_marked_for_strip ())
      rust_error_at (value->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::EnumExprFieldIndexValue &field) override
  {
    /* as no attrs possible (at moment, at least), only sub-expression
     * stripping is possible */
    auto &value = field.get_value ();
    value->accept_vis (*this);
    if (value->is_marked_for_strip ())
      rust_error_at (value->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::EnumExprStruct &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // supposedly spec doesn't allow inner attributes in enum exprs

    // strip sub-exprs of path
    auto &enum_path = expr.get_enum_variant_path ();
    visit (enum_path);
    if (enum_path.is_marked_for_strip ())
      rust_error_at (enum_path.get_locus (),
		     "cannot strip path in this position");

    /* spec does not specify whether expressions are allowed to be
     * stripped at top level of expression fields, but I wouldn't think
     * that they would be, so operating under the assumption that only
     * sub-expressions can be stripped. */
    for (auto &field : expr.get_fields ())
      {
	field->accept_vis (*this);
	// shouldn't strip in this
      }
  }
  void visit (AST::EnumExprTuple &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // supposedly spec doesn't allow inner attributes in enum exprs

    // strip sub-exprs of path
    auto &enum_path = expr.get_enum_variant_path ();
    visit (enum_path);
    if (enum_path.is_marked_for_strip ())
      rust_error_at (enum_path.get_locus (),
		     "cannot strip path in this position");

    /* spec says outer attributes are specifically allowed for elements
     * of tuple-style enum expressions, so full stripping possible */
    expand_pointer_allow_strip (expr.get_elems ());
  }
  void visit (AST::EnumExprFieldless &expr) override
  {
    // can't be stripped as no attrs

    // strip sub-exprs of path
    auto &enum_path = expr.get_enum_variant_path ();
    visit (enum_path);
    if (enum_path.is_marked_for_strip ())
      rust_error_at (enum_path.get_locus (),
		     "cannot strip path in this position");
  }
  void visit (AST::CallExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* should not be outer attrs on "function" expression - outer attrs
     * should be associated with call expr as a whole. only sub-expr
     * expansion is possible. */
    auto &function = expr.get_function_expr ();
    function->accept_vis (*this);
    if (function->is_marked_for_strip ())
      rust_error_at (function->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    /* spec says outer attributes are specifically allowed for elements
     * of call expressions, so full stripping possible */
    expand_pointer_allow_strip (expr.get_params ());
  }
  void visit (AST::MethodCallExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* should not be outer attrs on "receiver" expression - outer attrs
     * should be associated with call expr as a whole. only sub-expr
     * expansion is possible. */
    auto &receiver = expr.get_receiver_expr ();
    receiver->accept_vis (*this);
    if (receiver->is_marked_for_strip ())
      rust_error_at (receiver->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    auto &method_name = expr.get_method_name ();
    if (method_name.has_generic_args ())
      expand_generic_args (method_name.get_generic_args ());

    /* spec says outer attributes are specifically allowed for elements
     * of method call expressions, so full stripping possible */
    expand_pointer_allow_strip (expr.get_params ());
  }
  void visit (AST::FieldAccessExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* should not be outer attrs on "receiver" expression - outer attrs
     * should be associated with field expr as a whole. only sub-expr
     * expansion is possible. */
    auto &receiver = expr.get_receiver_expr ();
    receiver->accept_vis (*this);
    if (receiver->is_marked_for_strip ())
      rust_error_at (receiver->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ClosureExprInner &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip closure parameters if required - this is specifically
     * allowed by spec */
    expand_closure_params (expr.get_params ());

    // can't strip expression itself, but can strip sub-expressions
    auto &definition_expr = expr.get_definition_expr ();
    definition_expr->accept_vis (*this);
    if (definition_expr->is_marked_for_strip ())
      rust_error_at (definition_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::BlockExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip test based on inner attrs - spec says there are inner
     * attributes, not just outer attributes of inner stmts */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // strip all statements
    expand_pointer_allow_strip (expr.get_statements ());

    // strip tail expression if exists - can actually fully remove it
    if (expr.has_tail_expr ())
      {
	auto &tail_expr = expr.get_tail_expr ();

	tail_expr->accept_vis (*this);

	if (tail_expr->is_marked_for_strip ())
	  expr.strip_tail_expr ();
      }
  }
  void visit (AST::ClosureExprInnerTyped &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* strip closure parameters if required - this is specifically
     * allowed by spec */
    expand_closure_params (expr.get_params ());

    // can't strip return type, but can strip sub-types
    auto &type = expr.get_return_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    // can't strip expression itself, but can strip sub-expressions
    auto &definition_block = expr.get_definition_block ();
    definition_block->accept_vis (*this);
    if (definition_block->is_marked_for_strip ())
      rust_error_at (definition_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ContinueExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }
  }
  void visit (AST::BreakExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* spec does not say that you can have outer attributes on
     * expression, so assuming you can't. stripping for sub-expressions
     * is the only thing that can be done */
    if (expr.has_break_expr ())
      {
	auto &break_expr = expr.get_break_expr ();

	break_expr->accept_vis (*this);

	if (break_expr->is_marked_for_strip ())
	  rust_error_at (break_expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");
      }
  }
  void visit (AST::RangeFromToExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_from_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_to_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_from_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_from_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before range exprs");
    if (expr.get_to_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_to_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::RangeFromExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * direct descendant expression, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    auto &from_expr = expr.get_from_expr ();

    from_expr->accept_vis (*this);

    if (from_expr->is_marked_for_strip ())
      rust_error_at (from_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed before range exprs");
  }
  void visit (AST::RangeToExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * direct descendant expression, can strip ones below that */

    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    auto &to_expr = expr.get_to_expr ();

    to_expr->accept_vis (*this);

    if (to_expr->is_marked_for_strip ())
      rust_error_at (to_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::RangeFullExpr &) override
  {
    // outer attributes never allowed before these, so no stripping
  }
  void visit (AST::RangeFromToInclExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * two direct descendant expressions, can strip ones below that */

    /* should have no possibility for outer attrs as would be parsed
     * with outer expr */
    expr.get_from_expr ()->accept_vis (*this);
    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    expr.get_to_expr ()->accept_vis (*this);

    // ensure that they are not marked for strip
    if (expr.get_from_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_from_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before range exprs");
    if (expr.get_to_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_to_expr ()->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::RangeToInclExpr &expr) override
  {
    /* outer attributes never allowed before these. while cannot strip
     * direct descendant expression, can strip ones below that */

    /* should syntactically not have outer attributes, though this may
     * not have worked in practice */
    auto &to_expr = expr.get_to_expr ();

    to_expr->accept_vis (*this);

    if (to_expr->is_marked_for_strip ())
      rust_error_at (to_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ReturnExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* spec does not say that you can have outer attributes on
     * expression, so assuming you can't. stripping for sub-expressions
     * is the only thing that can be done */
    if (expr.has_returned_expr ())
      {
	auto &returned_expr = expr.get_returned_expr ();

	returned_expr->accept_vis (*this);

	if (returned_expr->is_marked_for_strip ())
	  rust_error_at (returned_expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");
      }
    /* TODO: conceptually, you would maybe be able to remove a returned
     * expr - e.g. if you had conditional compilation returning void or
     * returning a type. On the other hand, I think that function
     * return type cannot be conditionally compiled, so I assumed you
     * can't do this either. */
  }
  void visit (AST::UnsafeBlockExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip block itself, but can strip sub-expressions
    auto &block_expr = expr.get_block_expr ();
    block_expr->accept_vis (*this);
    if (block_expr->is_marked_for_strip ())
      rust_error_at (block_expr->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::LoopExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::WhileLoopExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip predicate expr itself, but can strip sub-expressions
    auto &predicate_expr = expr.get_predicate_expr ();
    predicate_expr->accept_vis (*this);
    if (predicate_expr->is_marked_for_strip ())
      rust_error_at (predicate_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::WhileLetLoopExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    for (auto &pattern : expr.get_patterns ())
      {
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
      }

    // can't strip scrutinee expr itself, but can strip sub-expressions
    auto &scrutinee_expr = expr.get_scrutinee_expr ();
    scrutinee_expr->accept_vis (*this);
    if (scrutinee_expr->is_marked_for_strip ())
      rust_error_at (scrutinee_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ForLoopExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // strip sub-patterns of pattern
    auto &pattern = expr.get_pattern ();
    pattern->accept_vis (*this);
    if (pattern->is_marked_for_strip ())
      rust_error_at (pattern->get_locus_slow (),
		     "cannot strip pattern in this position");

    // can't strip scrutinee expr itself, but can strip sub-expressions
    auto &iterator_expr = expr.get_iterator_expr ();
    iterator_expr->accept_vis (*this);
    if (iterator_expr->is_marked_for_strip ())
      rust_error_at (iterator_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::IfExpr &expr) override
  {
    // rust playground test shows that IfExpr does support outer attrs, at least
    // when used as statement

    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip condition expr itself, but can strip sub-expressions
    auto &condition_expr = expr.get_condition_expr ();
    condition_expr->accept_vis (*this);
    if (condition_expr->is_marked_for_strip ())
      rust_error_at (condition_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::IfExprConseqElse &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip condition expr itself, but can strip sub-expressions
    auto &condition_expr = expr.get_condition_expr ();
    condition_expr->accept_vis (*this);
    if (condition_expr->is_marked_for_strip ())
      rust_error_at (condition_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip else block itself, but can strip sub-expressions
    auto &else_block = expr.get_else_block ();
    else_block->accept_vis (*this);
    if (else_block->is_marked_for_strip ())
      rust_error_at (else_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::IfExprConseqIf &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip condition expr itself, but can strip sub-expressions
    auto &condition_expr = expr.get_condition_expr ();
    condition_expr->accept_vis (*this);
    if (condition_expr->is_marked_for_strip ())
      rust_error_at (condition_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if expr itself, but can strip sub-expressions
    auto &conseq_if_expr = expr.get_conseq_if_expr ();
    conseq_if_expr->accept_vis (*this);
    if (conseq_if_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_expr->get_locus_slow (),
		     "cannot strip consequent if expression in this "
		     "position - outer attributes not allowed");
  }
  void visit (AST::IfExprConseqIfLet &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip condition expr itself, but can strip sub-expressions
    auto &condition_expr = expr.get_condition_expr ();
    condition_expr->accept_vis (*this);
    if (condition_expr->is_marked_for_strip ())
      rust_error_at (condition_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if let expr itself, but can strip sub-expressions
    auto &conseq_if_let_expr = expr.get_conseq_if_let_expr ();
    conseq_if_let_expr->accept_vis (*this);
    if (conseq_if_let_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_let_expr->get_locus_slow (),
		     "cannot strip consequent if let expression in this "
		     "position - outer attributes not "
		     "allowed");
  }
  void visit (AST::IfLetExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    for (auto &pattern : expr.get_patterns ())
      {
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::IfLetExprConseqElse &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    for (auto &pattern : expr.get_patterns ())
      {
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip else block itself, but can strip sub-expressions
    auto &else_block = expr.get_else_block ();
    else_block->accept_vis (*this);
    if (else_block->is_marked_for_strip ())
      rust_error_at (else_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::IfLetExprConseqIf &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    for (auto &pattern : expr.get_patterns ())
      {
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if expr itself, but can strip sub-expressions
    auto &conseq_if_expr = expr.get_conseq_if_expr ();
    conseq_if_expr->accept_vis (*this);
    if (conseq_if_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_expr->get_locus_slow (),
		     "cannot strip consequent if expression in this "
		     "position - outer attributes not allowed");
  }
  void visit (AST::IfLetExprConseqIfLet &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    for (auto &pattern : expr.get_patterns ())
      {
	pattern->accept_vis (*this);
	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if let expr itself, but can strip sub-expressions
    auto &conseq_if_let_expr = expr.get_conseq_if_let_expr ();
    conseq_if_let_expr->accept_vis (*this);
    if (conseq_if_let_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_let_expr->get_locus_slow (),
		     "cannot strip consequent if let expression in this "
		     "position - outer attributes not "
		     "allowed");
  }
  void visit (AST::MatchExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // inner attr strip test
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip scrutinee expr itself, but can strip sub-expressions
    auto &scrutinee_expr = expr.get_scrutinee_expr ();
    scrutinee_expr->accept_vis (*this);
    if (scrutinee_expr->is_marked_for_strip ())
      rust_error_at (scrutinee_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // strip match cases
    auto &match_cases = expr.get_match_cases ();
    for (auto it = match_cases.begin (); it != match_cases.end ();)
      {
	auto &match_case = *it;

	// strip match case based on outer attributes in match arm
	auto &match_arm = match_case.get_arm ();
	expander.expand_cfg_attrs (match_arm.get_outer_attrs ());
	if (expander.fails_cfg_with_expand (match_arm.get_outer_attrs ()))
	  {
	    // strip match case
	    it = match_cases.erase (it);
	    continue;
	  }

	for (auto &pattern : match_arm.get_patterns ())
	  {
	    pattern->accept_vis (*this);
	    if (pattern->is_marked_for_strip ())
	      rust_error_at (pattern->get_locus_slow (),
			     "cannot strip pattern in this position");
	  }

	/* assuming that guard expression cannot be stripped as
	 * strictly speaking you would have to strip the whole guard to
	 * make syntactical sense, which you can't do. as such, only
	 * strip sub-expressions */
	if (match_arm.has_match_arm_guard ())
	  {
	    auto &guard_expr = match_arm.get_guard_expr ();
	    guard_expr->accept_vis (*this);
	    if (guard_expr->is_marked_for_strip ())
	      rust_error_at (guard_expr->get_locus_slow (),
			     "cannot strip expression in this position - outer "
			     "attributes not allowed");
	  }

	// strip sub-expressions from match cases
	auto &case_expr = match_case.get_expr ();
	case_expr->accept_vis (*this);
	if (case_expr->is_marked_for_strip ())
	  rust_error_at (case_expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");

	// increment to next case if haven't continued
	++it;
      }
  }
  void visit (AST::AwaitExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    /* can't strip awaited expr itself, but can strip sub-expressions
     * - this is because you can't have no expr to await */
    auto &awaited_expr = expr.get_awaited_expr ();
    awaited_expr->accept_vis (*this);
    if (awaited_expr->is_marked_for_strip ())
      rust_error_at (awaited_expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::AsyncBlockExpr &expr) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	return;
      }

    // can't strip block itself, but can strip sub-expressions
    auto &block_expr = expr.get_block_expr ();
    block_expr->accept_vis (*this);
    if (block_expr->is_marked_for_strip ())
      rust_error_at (block_expr->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }

  void visit (AST::TypeParam &param) override
  {
    // outer attributes don't actually do anything, so ignore them

    if (param.has_type_param_bounds ())
      {
	// don't strip directly, only components of bounds
	for (auto &bound : param.get_type_param_bounds ())
	  bound->accept_vis (*this);
      }

    if (param.has_type ())
      {
	auto &type = param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }
  }
  void visit (AST::LifetimeWhereClauseItem &) override
  {
    // shouldn't require
  }
  void visit (AST::TypeBoundWhereClauseItem &item) override
  {
    // for lifetimes shouldn't require

    auto &type = item.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    // don't strip directly, only components of bounds
    for (auto &bound : item.get_type_param_bounds ())
      bound->accept_vis (*this);
  }
  void visit (AST::Method &method) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (method.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (method.get_outer_attrs ()))
      {
	method.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : method.get_generic_params ())
      param->accept_vis (*this);

    /* assuming you can't strip self param - wouldn't be a method
     * anymore. spec allows outer attrs on self param, but doesn't
     * specify whether cfg is used. */
    expand_self_param (method.get_self_param ());

    /* strip method parameters if required - this is specifically
     * allowed by spec */
    expand_function_params (method.get_function_params ());

    if (method.has_return_type ())
      {
	auto &return_type = method.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (method.has_where_clause ())
      expand_where_clause (method.get_where_clause ());

    /* body should always exist - if error state, should have returned
     * before now */
    // can't strip block itself, but can strip sub-expressions
    auto &block_expr = method.get_definition ();
    block_expr->accept_vis (*this);
    if (block_expr->is_marked_for_strip ())
      rust_error_at (block_expr->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::ModuleBodied &module) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (module.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (module.get_outer_attrs ()))
      {
	module.mark_for_strip ();
	return;
      }

    // strip test based on inner attrs
    expander.expand_cfg_attrs (module.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (module.get_inner_attrs ()))
      {
	module.mark_for_strip ();
	return;
      }

    // strip items if required
    expand_pointer_allow_strip (module.get_items ());
  }
  void visit (AST::ModuleNoBody &module) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (module.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (module.get_outer_attrs ()))
      {
	module.mark_for_strip ();
	return;
      }
  }
  void visit (AST::ExternCrate &crate) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (crate.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (crate.get_outer_attrs ()))
      {
	crate.mark_for_strip ();
	return;
      }
  }
  void visit (AST::UseTreeGlob &) override
  {
    // shouldn't require?
  }
  void visit (AST::UseTreeList &) override
  {
    // shouldn't require?
  }
  void visit (AST::UseTreeRebind &) override
  {
    // shouldn't require?
  }
  void visit (AST::UseDeclaration &use_decl) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (use_decl.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (use_decl.get_outer_attrs ()))
      {
	use_decl.mark_for_strip ();
	return;
      }
  }
  void visit (AST::Function &function) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (function.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (function.get_outer_attrs ()))
      {
	function.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : function.get_generic_params ())
      param->accept_vis (*this);

    /* strip function parameters if required - this is specifically
     * allowed by spec */
    expand_function_params (function.get_function_params ());

    if (function.has_return_type ())
      {
	auto &return_type = function.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (function.has_where_clause ())
      expand_where_clause (function.get_where_clause ());

    /* body should always exist - if error state, should have returned
     * before now */
    // can't strip block itself, but can strip sub-expressions
    auto &block_expr = function.get_definition ();
    block_expr->accept_vis (*this);
    if (block_expr->is_marked_for_strip ())
      rust_error_at (block_expr->get_locus_slow (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::TypeAlias &type_alias) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (type_alias.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (type_alias.get_outer_attrs ()))
      {
	type_alias.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : type_alias.get_generic_params ())
      param->accept_vis (*this);

    if (type_alias.has_where_clause ())
      expand_where_clause (type_alias.get_where_clause ());

    auto &type = type_alias.get_type_aliased ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::StructStruct &struct_item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (struct_item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (struct_item.get_outer_attrs ()))
      {
	struct_item.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : struct_item.get_generic_params ())
      param->accept_vis (*this);

    if (struct_item.has_where_clause ())
      expand_where_clause (struct_item.get_where_clause ());

    /* strip struct fields if required - this is presumably
     * allowed by spec */
    expand_struct_fields (struct_item.get_fields ());
  }
  void visit (AST::TupleStruct &tuple_struct) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (tuple_struct.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (tuple_struct.get_outer_attrs ()))
      {
	tuple_struct.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : tuple_struct.get_generic_params ())
      param->accept_vis (*this);

    /* strip struct fields if required - this is presumably
     * allowed by spec */
    expand_tuple_fields (tuple_struct.get_fields ());

    if (tuple_struct.has_where_clause ())
      expand_where_clause (tuple_struct.get_where_clause ());
  }
  void visit (AST::EnumItem &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }
  }
  void visit (AST::EnumItemTuple &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    /* strip item fields if required - this is presumably
     * allowed by spec */
    expand_tuple_fields (item.get_tuple_fields ());
  }
  void visit (AST::EnumItemStruct &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    /* strip item fields if required - this is presumably
     * allowed by spec */
    expand_struct_fields (item.get_struct_fields ());
  }
  void visit (AST::EnumItemDiscriminant &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &expr = item.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      rust_error_at (expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::Enum &enum_item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (enum_item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (enum_item.get_outer_attrs ()))
      {
	enum_item.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : enum_item.get_generic_params ())
      param->accept_vis (*this);

    if (enum_item.has_where_clause ())
      expand_where_clause (enum_item.get_where_clause ());

    /* strip enum fields if required - this is presumably
     * allowed by spec */
    expand_pointer_allow_strip (enum_item.get_variants ());
  }
  void visit (AST::Union &union_item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (union_item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (union_item.get_outer_attrs ()))
      {
	union_item.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : union_item.get_generic_params ())
      param->accept_vis (*this);

    if (union_item.has_where_clause ())
      expand_where_clause (union_item.get_where_clause ());

    /* strip union fields if required - this is presumably
     * allowed by spec */
    expand_struct_fields (union_item.get_variants ());
  }
  void visit (AST::ConstantItem &const_item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (const_item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (const_item.get_outer_attrs ()))
      {
	const_item.mark_for_strip ();
	return;
      }

    // strip any sub-types
    auto &type = const_item.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &expr = const_item.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      rust_error_at (expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::StaticItem &static_item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (static_item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (static_item.get_outer_attrs ()))
      {
	static_item.mark_for_strip ();
	return;
      }

    // strip any sub-types
    auto &type = static_item.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &expr = static_item.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      rust_error_at (expr->get_locus_slow (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::TraitItemFunc &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    expand_trait_function_decl (item.get_trait_function_decl ());

    if (item.has_definition ())
      {
	/* strip any internal sub-expressions - expression itself isn't
	 * allowed to have external attributes in this position so can't be
	 * stripped. */
	auto &block = item.get_definition ();
	block->accept_vis (*this);
	if (block->is_marked_for_strip ())
	  rust_error_at (block->get_locus_slow (),
			 "cannot strip block expression in this "
			 "position - outer attributes not allowed");
      }
  }
  void visit (AST::TraitItemMethod &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    expand_trait_method_decl (item.get_trait_method_decl ());

    if (item.has_definition ())
      {
	/* strip any internal sub-expressions - expression itself isn't
	 * allowed to have external attributes in this position so can't be
	 * stripped. */
	auto &block = item.get_definition ();
	block->accept_vis (*this);
	if (block->is_marked_for_strip ())
	  rust_error_at (block->get_locus_slow (),
			 "cannot strip block expression in this "
			 "position - outer attributes not allowed");
      }
  }
  void visit (AST::TraitItemConst &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    // strip any sub-types
    auto &type = item.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped */
    if (item.has_expression ())
      {
	auto &expr = item.get_expr ();
	expr->accept_vis (*this);
	if (expr->is_marked_for_strip ())
	  rust_error_at (expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");
      }
  }
  void visit (AST::TraitItemType &item) override
  {
    // initial test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    if (item.has_type_param_bounds ())
      {
	// don't strip directly, only components of bounds
	for (auto &bound : item.get_type_param_bounds ())
	  bound->accept_vis (*this);
      }
  }
  void visit (AST::Trait &trait) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (trait.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (trait.get_outer_attrs ()))
      {
	trait.mark_for_strip ();
	return;
      }

    // strip test based on inner attrs
    expander.expand_cfg_attrs (trait.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (trait.get_inner_attrs ()))
      {
	trait.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : trait.get_generic_params ())
      param->accept_vis (*this);

    if (trait.has_type_param_bounds ())
      {
	// don't strip directly, only components of bounds
	for (auto &bound : trait.get_type_param_bounds ())
	  bound->accept_vis (*this);
      }

    if (trait.has_where_clause ())
      expand_where_clause (trait.get_where_clause ());

    // strip trait items if required
    expand_pointer_allow_strip (trait.get_trait_items ());
  }
  void visit (AST::InherentImpl &impl) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (impl.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (impl.get_outer_attrs ()))
      {
	impl.mark_for_strip ();
	return;
      }

    // strip test based on inner attrs
    expander.expand_cfg_attrs (impl.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (impl.get_inner_attrs ()))
      {
	impl.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : impl.get_generic_params ())
      param->accept_vis (*this);

    auto &type = impl.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    if (impl.has_where_clause ())
      expand_where_clause (impl.get_where_clause ());

    // strip inherent impl items if required
    expand_pointer_allow_strip (impl.get_impl_items ());
  }
  void visit (AST::TraitImpl &impl) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (impl.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (impl.get_outer_attrs ()))
      {
	impl.mark_for_strip ();
	return;
      }

    // strip test based on inner attrs
    expander.expand_cfg_attrs (impl.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (impl.get_inner_attrs ()))
      {
	impl.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : impl.get_generic_params ())
      param->accept_vis (*this);

    auto &type = impl.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");

    auto &trait_path = impl.get_trait_path ();
    visit (trait_path);
    if (trait_path.is_marked_for_strip ())
      rust_error_at (trait_path.get_locus (),
		     "cannot strip typepath in this position");

    if (impl.has_where_clause ())
      expand_where_clause (impl.get_where_clause ());

    // strip trait impl items if required
    expand_pointer_allow_strip (impl.get_impl_items ());
  }
  void visit (AST::ExternalStaticItem &item) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    auto &type = item.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::ExternalFunctionItem &item) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (item.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (item.get_outer_attrs ()))
      {
	item.mark_for_strip ();
	return;
      }

    // just expand sub-stuff - can't actually strip generic params themselves
    for (auto &param : item.get_generic_params ())
      param->accept_vis (*this);

    /* strip function parameters if required - this is specifically
     * allowed by spec */
    auto &params = item.get_function_params ();
    for (auto it = params.begin (); it != params.end ();)
      {
	auto &param = *it;

	auto &param_attrs = param.get_outer_attrs ();
	expander.expand_cfg_attrs (param_attrs);
	if (expander.fails_cfg_with_expand (param_attrs))
	  {
	    it = params.erase (it);
	    continue;
	  }

	auto &type = param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");

	// increment if nothing else happens
	++it;
      }
    /* NOTE: these are extern function params, which may have different
     * rules and restrictions to "normal" function params. So expansion
     * handled separately. */

    /* TODO: assuming that variadic nature cannot be stripped. If this
     * is not true, then have code here to do so. */

    if (item.has_return_type ())
      {
	auto &return_type = item.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    if (item.has_where_clause ())
      expand_where_clause (item.get_where_clause ());
  }
  void visit (AST::ExternBlock &block) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (block.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (block.get_outer_attrs ()))
      {
	block.mark_for_strip ();
	return;
      }

    // strip test based on inner attrs
    expander.expand_cfg_attrs (block.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (block.get_inner_attrs ()))
      {
	block.mark_for_strip ();
	return;
      }

    // strip external items if required
    expand_pointer_allow_strip (block.get_extern_items ());
  }

  // I don't think it would be possible to strip macros without expansion
  void visit (AST::MacroMatchFragment &) override {}
  void visit (AST::MacroMatchRepetition &) override {}
  void visit (AST::MacroMatcher &) override {}
  void visit (AST::MacroRulesDefinition &rules_def) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (rules_def.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (rules_def.get_outer_attrs ()))
      {
	rules_def.mark_for_strip ();
	return;
      }

    // I don't think any macro rules can be stripped in any way
  }
  void visit (AST::MacroInvocation &macro_invoc) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (macro_invoc.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (macro_invoc.get_outer_attrs ()))
      {
	macro_invoc.mark_for_strip ();
	return;
      }

    // I don't think any macro token trees can be stripped in any way

    // TODO: maybe have stripping behaviour for the cfg! macro here?
  }
  void visit (AST::MetaItemPath &) override {}
  void visit (AST::MetaItemSeq &) override {}
  void visit (AST::MetaWord &) override {}
  void visit (AST::MetaNameValueStr &) override {}
  void visit (AST::MetaListPaths &) override {}
  void visit (AST::MetaListNameValueStr &) override {}

  void visit (AST::LiteralPattern &) override
  {
    // not possible
  }
  void visit (AST::IdentifierPattern &pattern) override
  {
    // can only strip sub-patterns of the inner pattern to bind
    if (!pattern.has_pattern_to_bind ())
      return;

    auto &sub_pattern = pattern.get_pattern_to_bind ();
    sub_pattern->accept_vis (*this);
    if (sub_pattern->is_marked_for_strip ())
      rust_error_at (sub_pattern->get_locus_slow (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::WildcardPattern &) override
  {
    // not possible
  }
  void visit (AST::RangePatternBoundLiteral &) override
  {
    // not possible
  }
  void visit (AST::RangePatternBoundPath &bound) override
  {
    // can expand path, but not strip it directly
    auto &path = bound.get_path ();
    visit (path);
    if (path.is_marked_for_strip ())
      rust_error_at (path.get_locus (), "cannot strip path in this position");
  }
  void visit (AST::RangePatternBoundQualPath &bound) override
  {
    // can expand path, but not strip it directly
    auto &path = bound.get_qualified_path ();
    visit (path);
    if (path.is_marked_for_strip ())
      rust_error_at (path.get_locus (), "cannot strip path in this position");
  }
  void visit (AST::RangePattern &pattern) override
  {
    // should have no capability to strip lower or upper bounds, only expand
    pattern.get_lower_bound ()->accept_vis (*this);
    pattern.get_upper_bound ()->accept_vis (*this);
  }
  void visit (AST::ReferencePattern &pattern) override
  {
    auto &sub_pattern = pattern.get_referenced_pattern ();
    sub_pattern->accept_vis (*this);
    if (sub_pattern->is_marked_for_strip ())
      rust_error_at (sub_pattern->get_locus_slow (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::StructPatternFieldTuplePat &field) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (field.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (field.get_outer_attrs ()))
      {
	field.mark_for_strip ();
	return;
      }

    // strip sub-patterns (can't strip top-level pattern)
    auto &sub_pattern = field.get_index_pattern ();
    sub_pattern->accept_vis (*this);
    if (sub_pattern->is_marked_for_strip ())
      rust_error_at (sub_pattern->get_locus_slow (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::StructPatternFieldIdentPat &field) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (field.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (field.get_outer_attrs ()))
      {
	field.mark_for_strip ();
	return;
      }

    // strip sub-patterns (can't strip top-level pattern)
    auto &sub_pattern = field.get_ident_pattern ();
    sub_pattern->accept_vis (*this);
    if (sub_pattern->is_marked_for_strip ())
      rust_error_at (sub_pattern->get_locus_slow (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::StructPatternFieldIdent &field) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (field.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (field.get_outer_attrs ()))
      {
	field.mark_for_strip ();
	return;
      }
  }
  void visit (AST::StructPattern &pattern) override
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
    expand_pointer_allow_strip (elems.get_struct_pattern_fields ());

    // assuming you can strip the ".." part
    if (elems.has_etc ())
      {
	expander.expand_cfg_attrs (elems.get_etc_outer_attrs ());
	if (expander.fails_cfg_with_expand (elems.get_etc_outer_attrs ()))
	  elems.strip_etc ();
      }
  }
  void visit (AST::TupleStructItemsNoRange &tuple_items) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &pattern : tuple_items.get_patterns ())
      {
	pattern->accept_vis (*this);

	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
  }
  void visit (AST::TupleStructItemsRange &tuple_items) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &lower_pattern : tuple_items.get_lower_patterns ())
      {
	lower_pattern->accept_vis (*this);

	if (lower_pattern->is_marked_for_strip ())
	  rust_error_at (lower_pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
    for (auto &upper_pattern : tuple_items.get_upper_patterns ())
      {
	upper_pattern->accept_vis (*this);

	if (upper_pattern->is_marked_for_strip ())
	  rust_error_at (upper_pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
  }
  void visit (AST::TupleStructPattern &pattern) override
  {
    // expand (but don't strip) path
    auto &path = pattern.get_path ();
    visit (path);
    if (path.is_marked_for_strip ())
      rust_error_at (path.get_locus (), "cannot strip path in this position");

    if (pattern.has_items ())
      pattern.get_items ()->accept_vis (*this);
  }
  void visit (AST::TuplePatternItemsMultiple &tuple_items) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &pattern : tuple_items.get_patterns ())
      {
	pattern->accept_vis (*this);

	if (pattern->is_marked_for_strip ())
	  rust_error_at (pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
  }
  void visit (AST::TuplePatternItemsRanged &tuple_items) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &lower_pattern : tuple_items.get_lower_patterns ())
      {
	lower_pattern->accept_vis (*this);

	if (lower_pattern->is_marked_for_strip ())
	  rust_error_at (lower_pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
    for (auto &upper_pattern : tuple_items.get_upper_patterns ())
      {
	upper_pattern->accept_vis (*this);

	if (upper_pattern->is_marked_for_strip ())
	  rust_error_at (upper_pattern->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
  }
  void visit (AST::TuplePattern &pattern) override
  {
    if (pattern.has_tuple_pattern_items ())
      pattern.get_items ()->accept_vis (*this);
  }
  void visit (AST::GroupedPattern &pattern) override
  {
    // can't strip inner pattern, only sub-patterns
    auto &pattern_in_parens = pattern.get_pattern_in_parens ();

    pattern_in_parens->accept_vis (*this);

    if (pattern_in_parens->is_marked_for_strip ())
      rust_error_at (pattern_in_parens->get_locus_slow (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::SlicePattern &pattern) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &item : pattern.get_items ())
      {
	item->accept_vis (*this);

	if (item->is_marked_for_strip ())
	  rust_error_at (item->get_locus_slow (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
  }

  void visit (AST::EmptyStmt &) override
  {
    // assuming no outer attributes, so nothing can happen
  }
  void visit (AST::LetStmt &stmt) override
  {
    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (stmt.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (stmt.get_outer_attrs ()))
      {
	stmt.mark_for_strip ();
	return;
      }

    // can't strip pattern, but call for sub-patterns
    auto &pattern = stmt.get_pattern ();
    pattern->accept_vis (*this);
    if (pattern->is_marked_for_strip ())
      rust_error_at (pattern->get_locus_slow (),
		     "cannot strip pattern in this position");

    // similar for type
    if (stmt.has_type ())
      {
	auto &type = stmt.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped */
    if (stmt.has_init_expr ())
      {
	auto &init_expr = stmt.get_init_expr ();
	init_expr->accept_vis (*this);
	if (init_expr->is_marked_for_strip ())
	  rust_error_at (init_expr->get_locus_slow (),
			 "cannot strip expression in this position - outer "
			 "attributes not allowed");
      }
  }
  void visit (AST::ExprStmtWithoutBlock &stmt) override
  {
    // outer attributes associated with expr, so rely on expr

    // guard - should prevent null pointer expr
    if (stmt.is_marked_for_strip ())
      return;

    // strip if expr is to be stripped
    auto &expr = stmt.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      {
	stmt.mark_for_strip ();
	return;
      }
  }
  void visit (AST::ExprStmtWithBlock &stmt) override
  {
    // outer attributes associated with expr, so rely on expr

    // guard - should prevent null pointer expr
    if (stmt.is_marked_for_strip ())
      return;

    // strip if expr is to be stripped
    auto &expr = stmt.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      {
	stmt.mark_for_strip ();
	return;
      }
  }

  void visit (AST::TraitBound &bound) override
  {
    // nothing in for lifetimes to strip

    // expand but don't strip type path
    auto &path = bound.get_type_path ();
    visit (path);
    if (path.is_marked_for_strip ())
      rust_error_at (path.get_locus (),
		     "cannot strip type path in this position");
  }
  void visit (AST::ImplTraitType &type) override
  {
    // don't strip directly, only components of bounds
    for (auto &bound : type.get_type_param_bounds ())
      bound->accept_vis (*this);
  }
  void visit (AST::TraitObjectType &type) override
  {
    // don't strip directly, only components of bounds
    for (auto &bound : type.get_type_param_bounds ())
      bound->accept_vis (*this);
  }
  void visit (AST::ParenthesisedType &type) override
  {
    // expand but don't strip inner type
    auto &inner_type = type.get_type_in_parens ();
    inner_type->accept_vis (*this);
    if (inner_type->is_marked_for_strip ())
      rust_error_at (inner_type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::ImplTraitTypeOneBound &type) override
  {
    // no stripping possible
    visit (type.get_trait_bound ());
  }
  void visit (AST::TraitObjectTypeOneBound &type) override
  {
    // no stripping possible
    visit (type.get_trait_bound ());
  }
  void visit (AST::TupleType &type) override
  {
    // TODO: assuming that types can't be stripped as types don't have outer
    // attributes
    for (auto &elem_type : type.get_elems ())
      {
	elem_type->accept_vis (*this);
	if (elem_type->is_marked_for_strip ())
	  rust_error_at (elem_type->get_locus_slow (),
			 "cannot strip type in this position");
      }
  }
  void visit (AST::NeverType &) override
  {
    // no stripping possible
  }
  void visit (AST::RawPointerType &type) override
  {
    // expand but don't strip type pointed to
    auto &pointed_type = type.get_type_pointed_to ();
    pointed_type->accept_vis (*this);
    if (pointed_type->is_marked_for_strip ())
      rust_error_at (pointed_type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::ReferenceType &type) override
  {
    // expand but don't strip type referenced
    auto &referenced_type = type.get_type_referenced ();
    referenced_type->accept_vis (*this);
    if (referenced_type->is_marked_for_strip ())
      rust_error_at (referenced_type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::ArrayType &type) override
  {
    // expand but don't strip type referenced
    auto &base_type = type.get_elem_type ();
    base_type->accept_vis (*this);
    if (base_type->is_marked_for_strip ())
      rust_error_at (base_type->get_locus_slow (),
		     "cannot strip type in this position");

    // same for expression
    auto &size_expr = type.get_size_expr ();
    size_expr->accept_vis (*this);
    if (size_expr->is_marked_for_strip ())
      rust_error_at (size_expr->get_locus_slow (),
		     "cannot strip expression in this position");
  }
  void visit (AST::SliceType &type) override
  {
    // expand but don't strip elem type
    auto &elem_type = type.get_elem_type ();
    elem_type->accept_vis (*this);
    if (elem_type->is_marked_for_strip ())
      rust_error_at (elem_type->get_locus_slow (),
		     "cannot strip type in this position");
  }
  void visit (AST::InferredType &) override
  {
    // none possible
  }
  void visit (AST::BareFunctionType &type) override
  {
    // seem to be no generics

    // presumably function params can be stripped
    auto &params = type.get_function_params ();
    for (auto it = params.begin (); it != params.end ();)
      {
	auto &param = *it;

	auto &param_attrs = param.get_outer_attrs ();
	expander.expand_cfg_attrs (param_attrs);
	if (expander.fails_cfg_with_expand (param_attrs))
	  {
	    it = params.erase (it);
	    continue;
	  }

	auto &type = param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus_slow (),
			 "cannot strip type in this position");

	// increment if nothing else happens
	++it;
      }

    /* TODO: assuming that variadic nature cannot be stripped. If this
     * is not true, then have code here to do so. */

    if (type.has_return_type ())
      {
	auto &return_type = type.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus_slow (),
			 "cannot strip type in this position");
      }

    // no where clause, apparently
  }
};

void
MacroExpander::parse_macro_to_meta_item (AST::MacroInvocData &invoc)
{
  // only parse if not already parsed
  if (invoc.is_parsed ())
    return;

  std::unique_ptr<AST::AttrInputMetaItemContainer> converted_input (
    invoc.get_delim_tok_tree ().parse_to_meta_item ());

  if (converted_input == nullptr)
    {
      fprintf (stderr, "DEBUG: failed to parse macro to meta item\n");
      // TODO: do something now? is this an actual error?
    }
  else
    {
      std::vector<std::unique_ptr<AST::MetaItemInner> > meta_items (
	std::move (converted_input->get_items ()));
      invoc.set_meta_item_output (std::move (meta_items));
    }
}

AST::Literal
MacroExpander::expand_cfg_macro (AST::MacroInvocData &invoc)
{
  // only allow on cfg macros
  if (invoc.get_path () != "cfg")
    return AST::Literal::create_error ();

  parse_macro_to_meta_item (invoc);

  /* TODO: assuming that cfg! macros can only have one meta item inner, like cfg
   * attributes */
  if (invoc.get_meta_items ().size () != 1)
    return AST::Literal::create_error ();

  bool result = invoc.get_meta_items ()[0]->check_cfg_predicate (session);
  if (result)
    return AST::Literal ("true", AST::Literal::BOOL, CORETYPE_BOOL);
  else
    return AST::Literal ("false", AST::Literal::BOOL, CORETYPE_BOOL);
}

#if 0
AST::ASTFragment
MacroExpander::expand_decl_macro (AST::MacroInvocData &invoc,
				  AST::MacroRulesDefinition &rules_def)
{
  // ensure that both invocation and rules are in a valid state
  rust_assert (!invoc.is_marked_for_strip ());
  rust_assert (!rules_def.is_marked_for_strip ());
  rust_assert (rules_def.get_macro_rules ().size () > 0);

  /* probably something here about parsing invoc and rules def token trees to
   * token stream. if not, how would parser handle the captures of exprs and
   * stuff? on the other hand, token trees may be kind of useful in rules def as
   * creating a point where recursion can occur (like having
   * "compare_macro_match" and then it calling itself when it finds delimiters)
   */

  /* find matching rule to invoc token tree, based on macro rule's matcher. if
   * none exist, error.
   * - specifically, check each matcher in order. if one fails to match, move
   * onto next. */
  /* TODO: does doing this require parsing expressions and whatever in the
   * invoc? if so, might as well save the results if referenced using $ or
   * whatever. If not, do another pass saving them. Except this is probably
   * useless as different rules could have different starting points for exprs
   * or whatever. Decision trees could avoid this, but they have their own
   * issues. */
  /* TODO: will need to modify the parser so that it can essentially "catch"
   * errors - maybe "try_parse_expr" or whatever methods. */
  // this technically creates a back-tracking parser - this will be the
  // implementation style

  /* then, after results are saved, generate the macro output from the
   * transcriber token tree. if i understand this correctly, the macro
   * invocation gets replaced by the transcriber tokens, except with
   * substitutions made (e.g. for $i variables) */

  /* TODO: it is probably better to modify AST::Token to store a pointer to a
   * Lexer::Token (rather than being converted) - i.e. not so much have
   * AST::Token as a Token but rather a TokenContainer (as it is another type of
   * TokenTree). This will prevent re-conversion of Tokens between each type
   * all the time, while still allowing the heterogenous storage of token trees.
   */
}
#endif

void
MacroExpander::expand_invoc (std::unique_ptr<AST::MacroInvocation> &invoc)
{
  /* if current expansion depth > recursion limit, create an error (maybe fatal
   * error) and return */

  /* switch on type of macro:
      - '!' syntax macro (inner switch)
	  - procedural macro - "A token-based function-like macro"
	  - 'macro_rules' (by example/pattern-match) macro? or not? "an
     AST-based function-like macro"
	  - else is unreachable
      - attribute syntax macro (inner switch)
	  - procedural macro attribute syntax - "A token-based attribute macro"
	  - legacy macro attribute syntax? - "an AST-based attribute macro"
	  - non-macro attribute: mark known
	  - else is unreachable
      - derive macro (inner switch)
	  - derive or legacy derive - "token-based" vs "AST-based"
	  - else is unreachable
      - derive container macro - unreachable*/

#if 0
  // macro_rules macro test code
  auto rule_def = find_rules_def(invoc->get_path());
  if (rule_def != nullptr) {
    ASTFrag expanded = expand_decl_macro(invoc, rule_def);
    /* could make this a data structure containing vectors of exprs, patterns and types (for regular),
     * and then stmts and items (for semi). Except what about having an expr, then a type? Hmm. Might
     * have to do the "unified base type" thing OR just have a simulated union, and then have AST frag
     * be a vector of these simulated unions. */

    // how would errors be signalled? null fragment? something else?
    // what about error vs just not having stuff in rules definition yet?

    /* replace macro invocation with ast frag. actually, don't have any context here. maybe attach ast
     * frag to macro invocation, and then have a method above get it? Or just return the ast frag from
     * this method. */
  }
#endif
}

/* Determines whether any cfg predicate is false and hence item with attributes
 * should be stripped. Note that attributes must be expanded before calling. */
bool
MacroExpander::fails_cfg (const AST::AttrVec &attrs) const
{
  for (const auto &attr : attrs)
    {
      if (attr.get_path () == "cfg" && !attr.check_cfg_predicate (session))
	return true;
    }
  return false;
}

/* Determines whether any cfg predicate is false and hence item with attributes
 * should be stripped. Will expand attributes as well. */
bool
MacroExpander::fails_cfg_with_expand (AST::AttrVec &attrs) const
{
  // TODO: maybe have something that strips cfg attributes that evaluate true?
  for (auto &attr : attrs)
    {
      if (attr.get_path () == "cfg")
	{
	  if (!attr.is_parsed_to_meta_item ())
	    attr.parse_attr_to_meta_item ();

	  // DEBUG
	  if (!attr.is_parsed_to_meta_item ())
	    fprintf (stderr, "failed to parse attr to meta item, right before "
			     "cfg predicate check\n");
	  else
	    fprintf (stderr, "attr has been successfully parsed to meta item, "
			     "right before cfg predicate check\n");

	  if (!attr.check_cfg_predicate (session))
	    {
	      // DEBUG
	      fprintf (
		stderr,
		"cfg predicate failed for attribute: \033[0;31m'%s'\033[0m\n",
		attr.as_string ().c_str ());

	      return true;
	    }
	  else
	    {
	      // DEBUG
	      fprintf (stderr,
		       "cfg predicate succeeded for attribute: "
		       "\033[0;31m'%s'\033[0m\n",
		       attr.as_string ().c_str ());
	    }
	}
    }
  return false;
}

// Expands cfg_attr attributes.
void
MacroExpander::expand_cfg_attrs (AST::AttrVec &attrs)
{
  for (std::size_t i = 0; i < attrs.size (); i++)
    {
      auto &attr = attrs[i];
      if (attr.get_path () == "cfg_attr")
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
MacroExpander::expand_crate ()
{
  /* fill macro/decorator map from init list? not sure where init list comes
   * from? */

  // TODO: does cfg apply for inner attributes? research.
  // the apparent answer (from playground test) is yes

  // expand crate cfg_attr attributes
  expand_cfg_attrs (crate.inner_attrs);

  if (fails_cfg_with_expand (crate.inner_attrs))
    {
      // basically, delete whole crate
      crate.strip_crate ();
      // TODO: maybe create warning here? probably not desired behaviour
    }
  // expand module attributes?

  // expand attributes recursively and strip items if required
  AttrVisitor attr_visitor (*this);
  auto &items = crate.items;
  for (auto it = items.begin (); it != items.end ();)
    {
      auto &item = *it;

      // mark for stripping if required
      item->accept_vis (attr_visitor);

      if (item->is_marked_for_strip ())
	it = items.erase (it);
      else
	++it;
    }

  // TODO: should recursive attribute and macro expansion be done in the same
  // transversal? Or in separate ones like currently?

  // expand module tree recursively

  // post-process

  // extract exported macros?
}
} // namespace Rust
