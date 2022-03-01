// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "rust-parse.h"

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
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");

	auto &type = param.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (type->get_locus (),
			 "cannot strip type in this position");
      }

    // expand binding args - strip sub-types only
    for (auto &binding : args.get_binding_args ())
      {
	auto &type = binding.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus (),
			 "cannot strip type in this position");
      }
  }

  void expand_qualified_path_type (AST::QualifiedPathType &path_type)
  {
    auto &type = path_type.get_type ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus (), "cannot strip type in this position");

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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");

	if (param.has_type_given ())
	  {
	    auto &type = param.get_type ();
	    type->accept_vis (*this);
	    if (type->is_marked_for_strip ())
	      rust_error_at (type->get_locus (),
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
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (return_type->get_locus (),
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
	  rust_error_at (return_type->get_locus (),
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

  void visit (AST::MacroInvocation &macro_invoc) override
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

    if (macro_invoc.has_semicolon ())
      expander.expand_invoc_semi (macro_invoc);
    else
      expander.expand_invoc (macro_invoc);

    // we need to visit the expanded fragments since it may need cfg
    // expansion
    // and it may be recursive
    for (auto &node : macro_invoc.get_fragment ().get_nodes ())
      node.accept_vis (*this);
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
	  rust_error_at (type->get_locus (),
			 "cannot strip type in this position");
      }

    if (type_path_function.has_return_type ())
      {
	auto &return_type = type_path_function.get_return_type ();
	return_type->accept_vis (*this);
	if (return_type->is_marked_for_strip ())
	  rust_error_at (return_type->get_locus (),
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
      rust_error_at (borrowed_expr->get_locus (),
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
      rust_error_at (dereferenced_expr->get_locus (),
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
      rust_error_at (propagating_expr->get_locus (),
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
      rust_error_at (negated_expr->get_locus (),
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
      rust_error_at (expr.get_left_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus (),
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
      rust_error_at (expr.get_left_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus (),
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
      rust_error_at (expr.get_left_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus (),
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
      rust_error_at (casted_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed before cast exprs");

    // TODO: strip sub-types of type
    auto &type = expr.get_type_to_cast_to ();
    type->accept_vis (*this);
    if (type->is_marked_for_strip ())
      rust_error_at (type->get_locus (), "cannot strip type in this position");
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
      rust_error_at (expr.get_left_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus (),
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
      rust_error_at (expr.get_left_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before binary op exprs");
    if (expr.get_right_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_right_expr ()->get_locus (),
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
      rust_error_at (inner_expr->get_locus (),
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
      rust_error_at (copied_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    auto &copy_count = elems.get_num_copies ();
    copy_count->accept_vis (*this);
    if (copy_count->is_marked_for_strip ())
      rust_error_at (copy_count->get_locus (),
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
      rust_error_at (array_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    auto &index_expr = expr.get_index_expr ();
    index_expr->accept_vis (*this);
    if (index_expr->is_marked_for_strip ())
      rust_error_at (index_expr->get_locus (),
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
      rust_error_at (tuple_expr->get_locus (),
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
      rust_error_at (value->get_locus (),
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
      rust_error_at (value->get_locus (),
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
	  rust_error_at (base_struct_expr->get_locus (),
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
      rust_error_at (base_struct_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
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
      rust_error_at (function->get_locus (),
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
      rust_error_at (receiver->get_locus (),
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
      rust_error_at (receiver->get_locus (),
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
      rust_error_at (definition_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");
  }

  void visit (AST::BlockExpr &expr) override
  {
    expander.push_context (MacroExpander::BLOCK);

    // initial strip test based on outer attrs
    expander.expand_cfg_attrs (expr.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_outer_attrs ()))
      {
	expr.mark_for_strip ();
	expander.pop_context ();
	return;
      }

    /* strip test based on inner attrs - spec says there are inner
     * attributes, not just outer attributes of inner stmts */
    expander.expand_cfg_attrs (expr.get_inner_attrs ());
    if (expander.fails_cfg_with_expand (expr.get_inner_attrs ()))
      {
	expr.mark_for_strip ();
	expander.pop_context ();
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
    expander.pop_context ();
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

    // can't strip expression itself, but can strip sub-expressions
    auto &definition_block = expr.get_definition_block ();
    definition_block->accept_vis (*this);
    if (definition_block->is_marked_for_strip ())
      rust_error_at (definition_block->get_locus (),
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
	  rust_error_at (break_expr->get_locus (),
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
      rust_error_at (expr.get_from_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before range exprs");
    if (expr.get_to_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_to_expr ()->get_locus (),
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
      rust_error_at (from_expr->get_locus (),
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
      rust_error_at (to_expr->get_locus (),
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
      rust_error_at (expr.get_from_expr ()->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes are never allowed "
		     "before range exprs");
    if (expr.get_to_expr ()->is_marked_for_strip ())
      rust_error_at (expr.get_to_expr ()->get_locus (),
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
      rust_error_at (to_expr->get_locus (),
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
	  rust_error_at (returned_expr->get_locus (),
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
      rust_error_at (block_expr->get_locus (),
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
      rust_error_at (loop_block->get_locus (),
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
      rust_error_at (predicate_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");
      }

    // can't strip scrutinee expr itself, but can strip sub-expressions
    auto &scrutinee_expr = expr.get_scrutinee_expr ();
    scrutinee_expr->accept_vis (*this);
    if (scrutinee_expr->is_marked_for_strip ())
      rust_error_at (scrutinee_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus (),
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
      rust_error_at (pattern->get_locus (),
		     "cannot strip pattern in this position");

    // can't strip scrutinee expr itself, but can strip sub-expressions
    auto &iterator_expr = expr.get_iterator_expr ();
    iterator_expr->accept_vis (*this);
    if (iterator_expr->is_marked_for_strip ())
      rust_error_at (iterator_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip block itself, but can strip sub-expressions
    auto &loop_block = expr.get_loop_block ();
    loop_block->accept_vis (*this);
    if (loop_block->is_marked_for_strip ())
      rust_error_at (loop_block->get_locus (),
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
      rust_error_at (condition_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
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
      rust_error_at (condition_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip else block itself, but can strip sub-expressions
    auto &else_block = expr.get_else_block ();
    else_block->accept_vis (*this);
    if (else_block->is_marked_for_strip ())
      rust_error_at (else_block->get_locus (),
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
      rust_error_at (condition_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if expr itself, but can strip sub-expressions
    auto &conseq_if_expr = expr.get_conseq_if_expr ();
    conseq_if_expr->accept_vis (*this);
    if (conseq_if_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_expr->get_locus (),
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
      rust_error_at (condition_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if let expr itself, but can strip sub-expressions
    auto &conseq_if_let_expr = expr.get_conseq_if_let_expr ();
    conseq_if_let_expr->accept_vis (*this);
    if (conseq_if_let_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_let_expr->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip else block itself, but can strip sub-expressions
    auto &else_block = expr.get_else_block ();
    else_block->accept_vis (*this);
    if (else_block->is_marked_for_strip ())
      rust_error_at (else_block->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if expr itself, but can strip sub-expressions
    auto &conseq_if_expr = expr.get_conseq_if_expr ();
    conseq_if_expr->accept_vis (*this);
    if (conseq_if_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_expr->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
			 "cannot strip pattern in this position");
      }

    // can't strip value expr itself, but can strip sub-expressions
    auto &value_expr = expr.get_value_expr ();
    value_expr->accept_vis (*this);
    if (value_expr->is_marked_for_strip ())
      rust_error_at (value_expr->get_locus (),
		     "cannot strip expression in this position - outer "
		     "attributes not allowed");

    // can't strip if block itself, but can strip sub-expressions
    auto &if_block = expr.get_if_block ();
    if_block->accept_vis (*this);
    if (if_block->is_marked_for_strip ())
      rust_error_at (if_block->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");

    // can't strip if let expr itself, but can strip sub-expressions
    auto &conseq_if_let_expr = expr.get_conseq_if_let_expr ();
    conseq_if_let_expr->accept_vis (*this);
    if (conseq_if_let_expr->is_marked_for_strip ())
      rust_error_at (conseq_if_let_expr->get_locus (),
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
      rust_error_at (scrutinee_expr->get_locus (),
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
	      rust_error_at (pattern->get_locus (),
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
	      rust_error_at (guard_expr->get_locus (),
			     "cannot strip expression in this position - outer "
			     "attributes not allowed");
	  }

	// strip sub-expressions from match cases
	auto &case_expr = match_case.get_expr ();
	case_expr->accept_vis (*this);
	if (case_expr->is_marked_for_strip ())
	  rust_error_at (case_expr->get_locus (),
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
      rust_error_at (awaited_expr->get_locus (),
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
      rust_error_at (block_expr->get_locus (),
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
	  rust_error_at (type->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

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
	  rust_error_at (return_type->get_locus (),
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
      rust_error_at (block_expr->get_locus (),
		     "cannot strip block expression in this position - outer "
		     "attributes not allowed");
  }
  void visit (AST::Module &module) override
  {
    // strip test based on outer attrs
    expander.expand_cfg_attrs (module.get_outer_attrs ());
    if (expander.fails_cfg_with_expand (module.get_outer_attrs ()))
      {
	module.mark_for_strip ();
	return;
      }

    // A loaded module might have inner attributes
    if (module.get_kind () == AST::Module::ModuleKind::LOADED)
      {
	// strip test based on inner attrs
	expander.expand_cfg_attrs (module.get_inner_attrs ());
	if (expander.fails_cfg_with_expand (module.get_inner_attrs ()))
	  {
	    module.mark_for_strip ();
	    return;
	  }
      }

    // Parse the module's items if they haven't been expanded and the file
    // should be parsed (i.e isn't hidden behind an untrue or impossible cfg
    // directive)
    if (!module.is_marked_for_strip ()
	&& module.get_kind () == AST::Module::ModuleKind::UNLOADED)
      {
	module.load_items ();
      }

    // strip items if required
    expand_pointer_allow_strip (module.get_items ());
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
	  rust_error_at (return_type->get_locus (),
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
      rust_error_at (block_expr->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");
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
      rust_error_at (expr->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &expr = const_item.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      rust_error_at (expr->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped. */
    auto &expr = static_item.get_expr ();
    expr->accept_vis (*this);
    if (expr->is_marked_for_strip ())
      rust_error_at (expr->get_locus (),
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
	  rust_error_at (block->get_locus (),
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
	  rust_error_at (block->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

    /* strip any internal sub-expressions - expression itself isn't
     * allowed to have external attributes in this position so can't be
     * stripped */
    if (item.has_expression ())
      {
	auto &expr = item.get_expr ();
	expr->accept_vis (*this);
	if (expr->is_marked_for_strip ())
	  rust_error_at (expr->get_locus (),
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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

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
      rust_error_at (type->get_locus (), "cannot strip type in this position");

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
      rust_error_at (type->get_locus (), "cannot strip type in this position");
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
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (return_type->get_locus (),
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

    auto path = Resolver::CanonicalPath::new_seg (rules_def.get_node_id (),
						  rules_def.get_rule_name ());
    expander.resolver->get_macro_scope ().insert (path,
						  rules_def.get_node_id (),
						  rules_def.get_locus ());
    expander.mappings->insert_macro_def (&rules_def);
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
      rust_error_at (sub_pattern->get_locus (),
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
      rust_error_at (sub_pattern->get_locus (),
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
      rust_error_at (sub_pattern->get_locus (),
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
      rust_error_at (sub_pattern->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
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
	  rust_error_at (lower_pattern->get_locus (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
    for (auto &upper_pattern : tuple_items.get_upper_patterns ())
      {
	upper_pattern->accept_vis (*this);

	if (upper_pattern->is_marked_for_strip ())
	  rust_error_at (upper_pattern->get_locus (),
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
	  rust_error_at (pattern->get_locus (),
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
	  rust_error_at (lower_pattern->get_locus (),
			 "cannot strip pattern in this position");
	// TODO: quit stripping now? or keep going?
      }
    for (auto &upper_pattern : tuple_items.get_upper_patterns ())
      {
	upper_pattern->accept_vis (*this);

	if (upper_pattern->is_marked_for_strip ())
	  rust_error_at (upper_pattern->get_locus (),
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
      rust_error_at (pattern_in_parens->get_locus (),
		     "cannot strip pattern in this position");
  }
  void visit (AST::SlicePattern &pattern) override
  {
    // can't strip individual patterns, only sub-patterns
    for (auto &item : pattern.get_items ())
      {
	item->accept_vis (*this);

	if (item->is_marked_for_strip ())
	  rust_error_at (item->get_locus (),
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
      rust_error_at (pattern->get_locus (),
		     "cannot strip pattern in this position");

    // similar for type
    if (stmt.has_type ())
      {
	auto &type = stmt.get_type ();
	type->accept_vis (*this);
	if (type->is_marked_for_strip ())
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (init_expr->get_locus (),
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
      rust_error_at (inner_type->get_locus (),
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
	  rust_error_at (elem_type->get_locus (),
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
      rust_error_at (pointed_type->get_locus (),
		     "cannot strip type in this position");
  }
  void visit (AST::ReferenceType &type) override
  {
    // expand but don't strip type referenced
    auto &referenced_type = type.get_type_referenced ();
    referenced_type->accept_vis (*this);
    if (referenced_type->is_marked_for_strip ())
      rust_error_at (referenced_type->get_locus (),
		     "cannot strip type in this position");
  }
  void visit (AST::ArrayType &type) override
  {
    // expand but don't strip type referenced
    auto &base_type = type.get_elem_type ();
    base_type->accept_vis (*this);
    if (base_type->is_marked_for_strip ())
      rust_error_at (base_type->get_locus (),
		     "cannot strip type in this position");

    // same for expression
    auto &size_expr = type.get_size_expr ();
    size_expr->accept_vis (*this);
    if (size_expr->is_marked_for_strip ())
      rust_error_at (size_expr->get_locus (),
		     "cannot strip expression in this position");
  }
  void visit (AST::SliceType &type) override
  {
    // expand but don't strip elem type
    auto &elem_type = type.get_elem_type ();
    elem_type->accept_vis (*this);
    if (elem_type->is_marked_for_strip ())
      rust_error_at (elem_type->get_locus (),
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
	  rust_error_at (type->get_locus (),
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
	  rust_error_at (return_type->get_locus (),
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
      rust_debug ("DEBUG: failed to parse macro to meta item");
      // TODO: do something now? is this an actual error?
    }
  else
    {
      std::vector<std::unique_ptr<AST::MetaItemInner>> meta_items (
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

AST::ASTFragment
MacroExpander::expand_decl_macro (Location invoc_locus,
				  AST::MacroInvocData &invoc,
				  AST::MacroRulesDefinition &rules_def,
				  bool semicolon)
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

  AST::DelimTokenTree &invoc_token_tree = invoc.get_delim_tok_tree ();

  // find matching arm
  AST::MacroRule *matched_rule = nullptr;
  std::map<std::string, std::vector<MatchedFragment>> matched_fragments;
  for (auto &rule : rules_def.get_rules ())
    {
      sub_stack.push ();
      bool did_match_rule = try_match_rule (rule, invoc_token_tree);
      matched_fragments = sub_stack.pop ();

      if (did_match_rule)
	{
	  for (auto &kv : matched_fragments)
	    rust_debug ("[fragment]: %s (%ld)", kv.first.c_str (),
			kv.second.size ());

	  matched_rule = &rule;
	  break;
	}
    }

  if (matched_rule == nullptr)
    {
      RichLocation r (invoc_locus);
      r.add_range (rules_def.get_locus ());
      rust_error_at (r, "Failed to match any rule within macro");
      return AST::ASTFragment::create_empty ();
    }

  return transcribe_rule (*matched_rule, invoc_token_tree, matched_fragments,
			  semicolon, peek_context ());
}

void
MacroExpander::expand_invoc (AST::MacroInvocation &invoc)
{
  if (depth_exceeds_recursion_limit ())
    {
      rust_error_at (invoc.get_locus (), "reached recursion limit");
      return;
    }

  AST::MacroInvocData &invoc_data = invoc.get_invoc_data ();

  // ??
  // switch on type of macro:
  //  - '!' syntax macro (inner switch)
  //      - procedural macro - "A token-based function-like macro"
  //      - 'macro_rules' (by example/pattern-match) macro? or not? "an
  // AST-based function-like macro"
  //      - else is unreachable
  //  - attribute syntax macro (inner switch)
  //  - procedural macro attribute syntax - "A token-based attribute
  // macro"
  //      - legacy macro attribute syntax? - "an AST-based attribute macro"
  //      - non-macro attribute: mark known
  //      - else is unreachable
  //  - derive macro (inner switch)
  //      - derive or legacy derive - "token-based" vs "AST-based"
  //      - else is unreachable
  //  - derive container macro - unreachable

  // lookup the rules for this macro
  NodeId resolved_node = UNKNOWN_NODEID;
  bool found = resolver->get_macro_scope ().lookup (
    Resolver::CanonicalPath::new_seg (invoc.get_pattern_node_id (),
				      invoc_data.get_path ().as_string ()),
    &resolved_node);
  if (!found)
    {
      rust_error_at (invoc.get_locus (), "unknown macro");
      return;
    }

  // lookup the rules
  AST::MacroRulesDefinition *rules_def = nullptr;
  bool ok = mappings->lookup_macro_def (resolved_node, &rules_def);
  rust_assert (ok);

  auto fragment = AST::ASTFragment::create_empty ();

  if (rules_def->is_builtin ())
    fragment
      = rules_def->get_builtin_transcriber () (invoc.get_locus (), invoc_data);
  else
    fragment
      = expand_decl_macro (invoc.get_locus (), invoc_data, *rules_def, false);

  // lets attach this fragment to the invocation
  invoc.set_fragment (std::move (fragment));
}

void
MacroExpander::expand_invoc_semi (AST::MacroInvocation &invoc)
{
  if (depth_exceeds_recursion_limit ())
    {
      rust_error_at (invoc.get_locus (), "reached recursion limit");
      return;
    }

  AST::MacroInvocData &invoc_data = invoc.get_invoc_data ();

  // lookup the rules for this macro
  NodeId resolved_node = UNKNOWN_NODEID;
  bool found = resolver->get_macro_scope ().lookup (
    Resolver::CanonicalPath::new_seg (invoc.get_macro_node_id (),
				      invoc_data.get_path ().as_string ()),
    &resolved_node);
  if (!found)
    {
      rust_error_at (invoc.get_locus (), "unknown macro");
      return;
    }

  // lookup the rules
  AST::MacroRulesDefinition *rules_def = nullptr;
  bool ok = mappings->lookup_macro_def (resolved_node, &rules_def);
  rust_assert (ok);

  auto fragment = AST::ASTFragment::create_empty ();

  if (rules_def->is_builtin ())
    fragment
      = rules_def->get_builtin_transcriber () (invoc.get_locus (), invoc_data);
  else
    fragment
      = expand_decl_macro (invoc.get_locus (), invoc_data, *rules_def, true);

  // lets attach this fragment to the invocation
  invoc.set_fragment (std::move (fragment));
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
  NodeId scope_node_id = crate.get_node_id ();
  resolver->get_macro_scope ().push (scope_node_id);

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

  push_context (ITEM);

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

  pop_context ();

  // TODO: should recursive attribute and macro expansion be done in the same
  // transversal? Or in separate ones like currently?

  // expand module tree recursively

  // post-process

  // extract exported macros?
}

bool
MacroExpander::depth_exceeds_recursion_limit () const
{
  return expansion_depth >= cfg.recursion_limit;
}

bool
MacroExpander::try_match_rule (AST::MacroRule &match_rule,
			       AST::DelimTokenTree &invoc_token_tree)
{
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (std::move (lex));

  AST::MacroMatcher &matcher = match_rule.get_matcher ();

  expansion_depth++;
  if (!match_matcher (parser, matcher))
    {
      expansion_depth--;
      return false;
    }
  expansion_depth--;

  bool used_all_input_tokens = parser.skip_token (END_OF_FILE);
  return used_all_input_tokens;
}

bool
MacroExpander::match_fragment (Parser<MacroInvocLexer> &parser,
			       AST::MacroMatchFragment &fragment)
{
  switch (fragment.get_frag_spec ())
    {
    case AST::MacroFragSpec::EXPR:
      parser.parse_expr ();
      break;

    case AST::MacroFragSpec::BLOCK:
      parser.parse_block_expr ();
      break;

    case AST::MacroFragSpec::IDENT:
      parser.parse_identifier_pattern ();
      break;

    case AST::MacroFragSpec::LITERAL:
      parser.parse_literal_expr ();
      break;

    case AST::MacroFragSpec::ITEM:
      parser.parse_item (false);
      break;

    case AST::MacroFragSpec::TY:
      parser.parse_type ();
      break;

    case AST::MacroFragSpec::PAT:
      parser.parse_pattern ();
      break;

    case AST::MacroFragSpec::PATH:
      parser.parse_path_in_expression ();
      break;

    case AST::MacroFragSpec::VIS:
      parser.parse_visibility ();
      break;

    case AST::MacroFragSpec::STMT:
      parser.parse_stmt ();
      break;

    case AST::MacroFragSpec::LIFETIME:
      parser.parse_lifetime_params ();
      break;

      // is meta attributes?
    case AST::MacroFragSpec::META:
      // parser.parse_inner_attribute ?
      // parser.parse_outer_attribute ?
      // parser.parse_attribute_body ?
      // parser.parse_doc_comment ?
      gcc_unreachable ();
      break;

      // what is TT?
    case AST::MacroFragSpec::TT:
      // parser.parse_token_tree() ?
      gcc_unreachable ();
      break;

      // i guess we just ignore invalid and just error out
    case AST::MacroFragSpec::INVALID:
      return false;
    }

  // it matches if the parser did not produce errors trying to parse that type
  // of item
  return !parser.has_errors ();
}

bool
MacroExpander::match_matcher (Parser<MacroInvocLexer> &parser,
			      AST::MacroMatcher &matcher)
{
  if (depth_exceeds_recursion_limit ())
    {
      rust_error_at (matcher.get_match_locus (), "reached recursion limit");
      return false;
    }

  // this is used so we can check that we delimit the stream correctly.
  switch (matcher.get_delim_type ())
    {
      case AST::DelimType::PARENS: {
	if (!parser.skip_token (LEFT_PAREN))
	  return false;
      }
      break;

      case AST::DelimType::SQUARE: {
	if (!parser.skip_token (LEFT_SQUARE))
	  return false;
      }
      break;

      case AST::DelimType::CURLY: {
	if (!parser.skip_token (LEFT_CURLY))
	  return false;
      }
      break;
    }

  const MacroInvocLexer &source = parser.get_token_source ();

  for (auto &match : matcher.get_matches ())
    {
      size_t offs_begin = source.get_offs ();

      switch (match->get_macro_match_type ())
	{
	  case AST::MacroMatch::MacroMatchType::Fragment: {
	    AST::MacroMatchFragment *fragment
	      = static_cast<AST::MacroMatchFragment *> (match.get ());
	    if (!match_fragment (parser, *fragment))
	      return false;

	    // matched fragment get the offset in the token stream
	    size_t offs_end = source.get_offs ();
	    sub_stack.insert_fragment (
	      MatchedFragment (fragment->get_ident (), offs_begin, offs_end));
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Tok: {
	    AST::Token *tok = static_cast<AST::Token *> (match.get ());
	    if (!match_token (parser, *tok))
	      return false;
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Repetition: {
	    AST::MacroMatchRepetition *rep
	      = static_cast<AST::MacroMatchRepetition *> (match.get ());
	    if (!match_repetition (parser, *rep))
	      return false;
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Matcher: {
	    AST::MacroMatcher *m
	      = static_cast<AST::MacroMatcher *> (match.get ());
	    expansion_depth++;
	    if (!match_matcher (parser, *m))
	      {
		expansion_depth--;
		return false;
	      }
	    expansion_depth--;
	  }
	  break;
	}
    }

  switch (matcher.get_delim_type ())
    {
      case AST::DelimType::PARENS: {
	if (!parser.skip_token (RIGHT_PAREN))
	  return false;
      }
      break;

      case AST::DelimType::SQUARE: {
	if (!parser.skip_token (RIGHT_SQUARE))
	  return false;
      }
      break;

      case AST::DelimType::CURLY: {
	if (!parser.skip_token (RIGHT_CURLY))
	  return false;
      }
      break;
    }

  return true;
}

bool
MacroExpander::match_token (Parser<MacroInvocLexer> &parser, AST::Token &token)
{
  // FIXME this needs to actually match the content and the type
  return parser.skip_token (token.get_id ());
}

bool
MacroExpander::match_n_matches (
  Parser<MacroInvocLexer> &parser,
  std::vector<std::unique_ptr<AST::MacroMatch>> &matches, size_t &match_amount,
  size_t lo_bound, size_t hi_bound)
{
  match_amount = 0;

  const MacroInvocLexer &source = parser.get_token_source ();
  while (true)
    {
      // If the current token is a closing macro delimiter, break away.
      // TODO: Is this correct?
      auto t_id = parser.peek_current_token ()->get_id ();
      if (t_id == RIGHT_PAREN || t_id == RIGHT_SQUARE || t_id == RIGHT_CURLY)
	break;

      bool valid_current_match = false;
      for (auto &match : matches)
	{
	  size_t offs_begin = source.get_offs ();
	  switch (match->get_macro_match_type ())
	    {
	      case AST::MacroMatch::MacroMatchType::Fragment: {
		AST::MacroMatchFragment *fragment
		  = static_cast<AST::MacroMatchFragment *> (match.get ());
		valid_current_match = match_fragment (parser, *fragment);

		// matched fragment get the offset in the token stream
		size_t offs_end = source.get_offs ();
		sub_stack.insert_fragment (
		  MatchedFragment (fragment->get_ident (), offs_begin,
				   offs_end));
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Tok: {
		AST::Token *tok = static_cast<AST::Token *> (match.get ());
		valid_current_match = match_token (parser, *tok);
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Repetition: {
		AST::MacroMatchRepetition *rep
		  = static_cast<AST::MacroMatchRepetition *> (match.get ());
		valid_current_match = match_repetition (parser, *rep);
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Matcher: {
		AST::MacroMatcher *m
		  = static_cast<AST::MacroMatcher *> (match.get ());
		valid_current_match = match_matcher (parser, *m);
	      }
	      break;
	    }
	}
      // If we've encountered an error once, stop trying to match more
      // repetitions
      if (!valid_current_match)
	break;

      match_amount++;

      // Break early if we notice there's too many expressions already
      if (hi_bound && match_amount > hi_bound)
	break;
    }

  // Check if the amount of matches we got is valid: Is it more than the lower
  // bound and less than the higher bound?
  bool did_meet_lo_bound = match_amount >= lo_bound;
  bool did_meet_hi_bound = hi_bound ? match_amount <= hi_bound : true;

  return did_meet_lo_bound && did_meet_hi_bound;
}

bool
MacroExpander::match_repetition (Parser<MacroInvocLexer> &parser,
				 AST::MacroMatchRepetition &rep)
{
  size_t match_amount = 0;
  bool res = false;

  std::string lo_str;
  std::string hi_str;
  switch (rep.get_op ())
    {
    case AST::MacroMatchRepetition::MacroRepOp::ANY:
      lo_str = "0";
      hi_str = "+inf";
      res = match_n_matches (parser, rep.get_matches (), match_amount);
      break;
    case AST::MacroMatchRepetition::MacroRepOp::ONE_OR_MORE:
      lo_str = "1";
      hi_str = "+inf";
      res = match_n_matches (parser, rep.get_matches (), match_amount, 1);
      break;
    case AST::MacroMatchRepetition::MacroRepOp::ZERO_OR_ONE:
      lo_str = "0";
      hi_str = "1";
      res = match_n_matches (parser, rep.get_matches (), match_amount, 0, 1);
      break;
    default:
      gcc_unreachable ();
    }

  if (!res)
    rust_error_at (rep.get_match_locus (),
		   "invalid amount of matches for macro invocation. Expected "
		   "between %s and %s, got %lu",
		   lo_str.c_str (), hi_str.c_str (), match_amount);

  rust_debug_loc (rep.get_match_locus (), "%s matched %lu times",
		  res ? "successfully" : "unsuccessfully", match_amount);

  // We can now set the amount to each fragment we matched in the substack
  auto &stack_map = sub_stack.peek ();
  for (auto &match : rep.get_matches ())
    {
      if (match->get_macro_match_type ()
	  == AST::MacroMatch::MacroMatchType::Fragment)
	{
	  auto fragment = static_cast<AST::MacroMatchFragment *> (match.get ());
	  auto it = stack_map.find (fragment->get_ident ());

	  // If we can't find the fragment, but the result was valid, then
	  // it's a zero-matched fragment and we can insert it
	  if (it == stack_map.end ())
	    {
	      sub_stack.insert_fragment (
		MatchedFragment::zero (fragment->get_ident ()));
	    }
	  else
	    {
	      // We can just set the repetition amount on the first match
	      // FIXME: Make this more ergonomic and similar to what we fetch
	      // in `substitute_repetition`
	      it->second[0].set_match_amount (match_amount);
	    }
	}
    }

  return res;
}

AST::ASTFragment
MacroExpander::transcribe_rule (
  AST::MacroRule &match_rule, AST::DelimTokenTree &invoc_token_tree,
  std::map<std::string, std::vector<MatchedFragment>> &matched_fragments,
  bool semicolon, ContextType ctx)
{
  // we can manipulate the token tree to substitute the dollar identifiers so
  // that when we call parse its already substituted for us
  AST::MacroTranscriber &transcriber = match_rule.get_transcriber ();
  AST::DelimTokenTree &transcribe_tree = transcriber.get_token_tree ();

  auto invoc_stream = invoc_token_tree.to_token_stream ();
  auto macro_rule_tokens = transcribe_tree.to_token_stream ();

  std::vector<std::unique_ptr<AST::Token>> substituted_tokens
    = substitute_tokens (invoc_stream, macro_rule_tokens, matched_fragments);

  // // handy for debugging
  // for (auto &tok : substituted_tokens)
  //   {
  //     rust_debug ("tok: [%s]", tok->as_string ().c_str ());
  //   }

  // parse it to an ASTFragment
  MacroInvocLexer lex (std::move (substituted_tokens));
  Parser<MacroInvocLexer> parser (std::move (lex));

  // this is used so we can check that we delimit the stream correctly.
  switch (transcribe_tree.get_delim_type ())
    {
    case AST::DelimType::PARENS:
      rust_assert (parser.skip_token (LEFT_PAREN));
      break;

    case AST::DelimType::CURLY:
      rust_assert (parser.skip_token (LEFT_CURLY));
      break;

    case AST::DelimType::SQUARE:
      rust_assert (parser.skip_token (LEFT_SQUARE));
      break;
    }

  // see https://github.com/Rust-GCC/gccrs/issues/22
  // TL;DR:
  //   - Treat all macro invocations with parentheses, (), or square brackets,
  //   [], as expressions.
  //   - If the macro invocation has curly brackets, {}, it may be parsed as a
  //   statement depending on the context.
  //   - If the macro invocation has a semicolon at the end, it must be parsed
  //   as a statement (either via ExpressionStatement or
  //   MacroInvocationWithSemi)

  // parse the item
  std::vector<AST::SingleASTNode> nodes;
  switch (invoc_token_tree.get_delim_type ())
    {
    case AST::DelimType::PARENS:
      case AST::DelimType::SQUARE: {
	switch (ctx)
	  {
	    case ContextType::ITEM: {
	      auto item = parser.parse_item (true);
	      if (item != nullptr && !parser.has_errors ())
		{
		  rust_debug ("HELLO WORLD: [%s]", item->as_string ().c_str ());
		  nodes.push_back (std::move (item));
		}
	    }
	    break;

	    case ContextType::BLOCK: {
	      auto expr = parser.parse_expr ();
	      if (expr != nullptr && !parser.has_errors ())
		nodes.push_back (std::move (expr));
	    }
	    break;
	  }
      }
      break;

      case AST::DelimType::CURLY: {
	switch (ctx)
	  {
	    case ContextType::ITEM: {
	      auto item = parser.parse_item (true);
	      if (item != nullptr && !parser.has_errors ())
		nodes.push_back (std::move (item));
	    }
	    break;

	    case ContextType::BLOCK: {
	      auto stmt = parser.parse_stmt ();
	      if (stmt != nullptr && !parser.has_errors ())
		nodes.push_back (std::move (stmt));
	    }
	    break;
	  }
      }
      break;
    }

  // emit any errors
  if (parser.has_errors ())
    {
      for (auto &err : parser.get_errors ())
	{
	  rust_error_at (err.locus, "%s", err.message.c_str ());
	}
      return AST::ASTFragment::create_empty ();
    }

  // are all the tokens used?
  bool did_delimit = false;
  switch (transcribe_tree.get_delim_type ())
    {
    case AST::DelimType::PARENS:
      did_delimit = parser.skip_token (RIGHT_PAREN);
      break;
    case AST::DelimType::SQUARE:
      did_delimit = parser.skip_token (RIGHT_SQUARE);
      break;
    case AST::DelimType::CURLY:
      did_delimit = parser.skip_token (RIGHT_CURLY);
      break;
    }

  bool reached_end_of_stream = did_delimit && parser.skip_token (END_OF_FILE);
  if (!reached_end_of_stream)
    {
      const_TokenPtr current_token = parser.peek_current_token ();
      rust_error_at (current_token->get_locus (),
		     "tokens here and after are unparsed");
    }

  return AST::ASTFragment (std::move (nodes));
}

std::vector<std::unique_ptr<AST::Token>>
MacroExpander::substitute_metavar (
  std::vector<std::unique_ptr<AST::Token>> &input,
  std::map<std::string, std::vector<MatchedFragment>> &fragments,
  std::unique_ptr<AST::Token> &metavar)
{
  auto metavar_name = metavar->get_str ();

  std::vector<std::unique_ptr<AST::Token>> expanded;
  auto it = fragments.find (metavar_name);
  if (it == fragments.end ())
    {
      // Return a copy of the original token
      expanded.push_back (metavar->clone_token ());
    }
  else
    {
      // Replace
      // We only care about the vector when expanding repetitions. Just access
      // the first element of the vector.
      // FIXME: Clean this up so it makes more sense
      auto &frag = it->second[0];
      for (size_t offs = frag.token_offset_begin; offs < frag.token_offset_end;
	   offs++)
	{
	  auto &tok = input.at (offs);
	  expanded.push_back (tok->clone_token ());
	}
    }

  return expanded;
}

std::vector<std::unique_ptr<AST::Token>>
MacroExpander::substitute_repetition (
  std::vector<std::unique_ptr<AST::Token>> &input,
  std::vector<std::unique_ptr<AST::Token>> &macro,
  std::map<std::string, std::vector<MatchedFragment>> &fragments,
  size_t pattern_start, size_t pattern_end)
{
  rust_assert (pattern_end < macro.size ());

  rust_debug ("pattern start: %lu", pattern_start);
  rust_debug ("pattern end: %lu", pattern_end);

  std::vector<std::unique_ptr<AST::Token>> expanded;

  // Find the first fragment and get the amount of repetitions that we should
  // perform
  size_t repeat_amount = 0;
  for (size_t i = pattern_start; i < pattern_end; i++)
    {
      if (macro.at (i)->get_id () == DOLLAR_SIGN)
	{
	  auto &frag_token = macro.at (i + 1);
	  if (frag_token->get_id () == IDENTIFIER)
	    {
	      auto it = fragments.find (frag_token->get_str ());
	      if (it == fragments.end ())
		{
		  // If the repetition is not anything we know (ie no declared
		  // metavars, or metavars which aren't present in the
		  // fragment), we can just error out. No need to paste the
		  // tokens as if nothing had happened.
		  rust_error_at (frag_token->get_locus (),
				 "metavar %s used in repetition does not exist",
				 frag_token->get_str ().c_str ());
		  // FIXME:
		  return expanded;
		}

	      // FIXME: Refactor, ugly
	      repeat_amount = it->second[0].match_amount;
	    }
	}
    }

  rust_debug ("repetition amount to use: %lu", repeat_amount);
  std::vector<std::unique_ptr<AST::Token>> new_macro;

  // We want to generate a "new macro" to substitute with. This new macro
  // should contain only the tokens inside the pattern
  for (size_t tok_idx = pattern_start; tok_idx < pattern_end; tok_idx++)
    new_macro.emplace_back (macro.at (tok_idx)->clone_token ());

  // Then, we want to create a subset of the matches so that
  // `substitute_tokens()` can only see one fragment per metavar. Let's say we
  // have the following user input: (1 145 'h')
  // on the following match arm: ($($lit:literal)*)
  // which causes the following matches: { "lit": [1, 145, 'h'] }
  //
  // The pattern (new_macro) is `$lit:literal`
  // The first time we expand it, we want $lit to have the following token: 1
  // The second time, 145
  // The third and final time, 'h'
  //
  // In order to do so we must create "sub maps", which only contain parts of
  // the original matches
  // sub-maps: [ { "lit": 1 }, { "lit": 145 }, { "lit": 'h' } ]
  //
  // and give them to `substitute_tokens` one by one.

  for (size_t i = 0; i < repeat_amount; i++)
    {
      std::map<std::string, std::vector<MatchedFragment>> sub_map;
      for (auto &kv_match : fragments)
	{
	  std::vector<MatchedFragment> sub_vec;
	  sub_vec.emplace_back (kv_match.second[i]);

	  sub_map.insert ({kv_match.first, sub_vec});
	}

      auto new_tokens = substitute_tokens (input, new_macro, sub_map);

      for (auto &new_token : new_tokens)
	expanded.emplace_back (new_token->clone_token ());
    }

  // FIXME: We also need to make sure that all subsequent fragments
  // contain the same amount of repetitions as the first one

  return expanded;
}

std::pair<std::vector<std::unique_ptr<AST::Token>>, size_t>
MacroExpander::substitute_token (
  std::vector<std::unique_ptr<AST::Token>> &input,
  std::vector<std::unique_ptr<AST::Token>> &macro,
  std::map<std::string, std::vector<MatchedFragment>> &fragments,
  size_t token_idx)
{
  auto &token = macro.at (token_idx);
  switch (token->get_id ())
    {
    case IDENTIFIER:
      rust_debug ("expanding metavar: %s", token->get_str ().c_str ());
      return {substitute_metavar (input, fragments, token), 1};
      case LEFT_PAREN: {
	// We need to parse up until the closing delimiter and expand this
	// fragment->n times.
	rust_debug ("expanding repetition");
	std::vector<std::unique_ptr<AST::Token>> repetition_pattern;
	size_t pattern_start = token_idx + 1;
	size_t pattern_end = pattern_start;
	for (; pattern_end < macro.size ()
	       && macro.at (pattern_end)->get_id () != RIGHT_PAREN;
	     pattern_end++)
	  ;

	// FIXME: This skips whitespaces... Is that okay??
	// FIXME: Is there any existing parsing function that allows us to parse
	// a macro pattern?

	// FIXME: Add error handling in the case we haven't found a matching
	// closing delimiter

	// FIXME: We need to parse the repetition token now

	return {
	  substitute_repetition (input, macro, fragments, pattern_start,
				 pattern_end),
	  // + 2 for the opening and closing parentheses which are mandatory
	  // + 1 for the repetitor (+, *, ?)
	  pattern_end - pattern_start + 3};
      }
      // TODO: We need to check if the $ was alone. In that case, do
      // not error out: Simply act as if there was an empty identifier
      // with no associated fragment and paste the dollar sign in the
      // transcription. Unsure how to do that since we always have at
      // least the closing curly brace after an empty $...
    default:
      rust_error_at (token->get_locus (),
		     "unexpected token in macro transcribe: expected "
		     "%<(%> or identifier after %<$%>, got %<%s%>",
		     get_token_description (token->get_id ()));
    }

  // FIXME: gcc_unreachable() error case?
  return {std::vector<std::unique_ptr<AST::Token>> (), 0};
}

std::vector<std::unique_ptr<AST::Token>>
MacroExpander::substitute_tokens (
  std::vector<std::unique_ptr<AST::Token>> &input,
  std::vector<std::unique_ptr<AST::Token>> &macro,
  std::map<std::string, std::vector<MatchedFragment>> &fragments)
{
  std::vector<std::unique_ptr<AST::Token>> replaced_tokens;

  for (size_t i = 0; i < macro.size (); i++)
    {
      auto &tok = macro.at (i);
      if (tok->get_id () == DOLLAR_SIGN)
	{
	  // Aaaaah, if only we had C++17 :)
	  // auto [expanded, tok_to_skip] = ...
	  auto p = substitute_token (input, macro, fragments, i + 1);
	  auto expanded = std::move (p.first);
	  auto tok_to_skip = p.second;

	  i += tok_to_skip;

	  for (auto &token : expanded)
	    replaced_tokens.emplace_back (token->clone_token ());
	}
      else
	{
	  replaced_tokens.emplace_back (tok->clone_token ());
	}
    }

  return replaced_tokens;
}

} // namespace Rust
