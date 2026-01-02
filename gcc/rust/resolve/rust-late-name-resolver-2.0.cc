// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "optional.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"
#include "rust-hir-map.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"
#include "rust-name-resolution-context.h"
#include "rust-resolve-builtins.h"
#include "rust-path.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"
#include "rust-ice-finalizer.h"
#include "rust-ast.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx)
  : DefaultResolver (ctx), funny_error (false), block_big_self (false)
{}

void
Late::go (AST::Crate &crate)
{
  Builtins::setup_type_ctx ();

  visit (crate);
}

void
Late::new_label (Identifier name, NodeId id)
{
  // labels can always shadow, so `insert` should never fail. if it does, we're
  // in big trouble!
  auto ok = ctx.labels.insert (name, id);

  rust_assert (ok);
}

void
Late::visit (AST::ForLoopExpr &expr)
{
  visit_outer_attrs (expr);

  ctx.bindings.enter (BindingSource::For);

  visit (expr.get_pattern ());

  ctx.bindings.exit ();

  visit (expr.get_iterator_expr ());

  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());

  visit (expr.get_loop_block ());
}

void
Late::visit_if_let_patterns (AST::IfLetExpr &expr)
{
  ctx.bindings.enter (BindingSource::IfLet);

  DefaultResolver::visit_if_let_patterns (expr);

  ctx.bindings.exit ();
}

void
Late::visit (AST::MatchArm &arm)
{
  visit_outer_attrs (arm);

  ctx.bindings.enter (BindingSource::Match);

  for (auto &pattern : arm.get_patterns ())
    visit (pattern);

  ctx.bindings.exit ();

  if (arm.has_match_arm_guard ())
    visit (arm.get_guard_expr ());
}

void
Late::visit (AST::LetStmt &let)
{
  DefaultASTVisitor::visit_outer_attrs (let);
  if (let.has_type ())
    visit (let.get_type ());
  // visit expression before pattern
  // this makes variable shadowing work properly
  if (let.has_init_expr ())
    visit (let.get_init_expr ());

  ctx.bindings.enter (BindingSource::Let);

  visit (let.get_pattern ());

  ctx.bindings.exit ();

  if (let.has_else_expr ())
    visit (let.get_init_expr ());

  // how do we deal with the fact that `let a = blipbloup` should look for a
  // label and cannot go through function ribs, but `let a = blipbloup()` can?

  // how do we insert ribs here, and only pop them when we exit the current
  // function?
  // keep a list of ribs to pop when a scope exits? so only for blocks?
  // how do we pop ribs that need to be popped not in order?
  // I think it's not important if we have shadowing, correct?

  // if we have shadowing, it should work! we'll see

  // ctx.insert(Identifier name, NodeId id, Namespace ns)
  // ctx.scoped (Rib::Kind::Normal /* FIXME: Is that valid? */,
  // Namespace::Labels,
  //      let.get_node_id (), [] () {});
}

void
Late::visit (AST::WhileLetLoopExpr &while_let)
{
  DefaultASTVisitor::visit_outer_attrs (while_let);

  if (while_let.has_loop_label ())
    visit (while_let.get_loop_label ());

  // visit expression before pattern
  // this makes variable shadowing work properly
  visit (while_let.get_scrutinee_expr ());

  ctx.bindings.enter (BindingSource::WhileLet);

  for (auto &pattern : while_let.get_patterns ())
    visit (pattern);

  ctx.bindings.exit ();

  visit (while_let.get_loop_block ());
}

static void
visit_identifier_as_pattern (NameResolutionContext &ctx,
			     const Identifier &ident, location_t locus,
			     NodeId node_id, bool is_ref, bool is_mut)
{
  // do we insert in labels or in values
  // but values does not allow shadowing... since functions cannot shadow
  // do we insert functions in labels as well?

  if (ctx.bindings.peek ().is_and_bound (ident))
    {
      if (ctx.bindings.peek ().get_source () == BindingSource::Param)
	rust_error_at (
	  locus, ErrorCode::E0415,
	  "identifier %qs is bound more than once in the same parameter list",
	  ident.as_string ().c_str ());
      else
	rust_error_at (
	  locus, ErrorCode::E0416,
	  "identifier %qs is bound more than once in the same pattern",
	  ident.as_string ().c_str ());
      return;
    }

  ctx.bindings.peek ().insert_ident (ident.as_string (), locus, is_ref, is_mut);

  if (ctx.bindings.peek ().is_or_bound (ident))
    {
      auto res = ctx.values.get (ident);
      rust_assert (res.has_value () && !res->is_ambiguous ());
      ctx.map_usage (Usage (node_id), Definition (res->get_node_id ()));
    }
  else
    {
      // We do want to ignore duplicated data because some situations rely on
      // it.
      std::ignore = ctx.values.insert_shadowable (ident, node_id);
    }
}

void
Late::visit (AST::IdentifierPattern &identifier)
{
  DefaultResolver::visit (identifier);

  visit_identifier_as_pattern (ctx, identifier.get_ident (),
			       identifier.get_locus (),
			       identifier.get_node_id (),
			       identifier.get_is_ref (),
			       identifier.get_is_mut ());
}

void
Late::visit (AST::AltPattern &pattern)
{
  ctx.bindings.peek ().push (Binding::Kind::Or);
  for (auto &alt : pattern.get_alts ())
    {
      ctx.bindings.peek ().push (Binding::Kind::Product);
      visit (alt);
      ctx.bindings.peek ().merge ();
    }
  ctx.bindings.peek ().merge ();
}

void
Late::visit_function_params (AST::Function &function)
{
  ctx.bindings.enter (BindingSource::Param);

  for (auto &param : function.get_function_params ())
    visit (param);

  ctx.bindings.exit ();
}

void
Late::visit (AST::StructPatternFieldIdent &field)
{
  visit_identifier_as_pattern (ctx, field.get_identifier (), field.get_locus (),
			       field.get_node_id (), field.is_ref (),
			       field.is_mut ());
}

void
Late::visit (AST::SelfParam &param)
{
  // handle similar to AST::IdentifierPattern

  DefaultResolver::visit (param);
  // FIXME: this location should be a bit off
  // ex: would point to the beginning of "mut self" instead of the "self"
  std::ignore = ctx.values.insert (Identifier ("self", param.get_locus ()),
				   param.get_node_id ());
}

void
Late::visit (AST::BreakExpr &expr)
{
  if (expr.has_label ())
    resolve_label (expr.get_label_unchecked ().get_lifetime ());

  if (expr.has_break_expr ())
    {
      auto &break_expr = expr.get_break_expr ();
      if (break_expr.get_expr_kind () == AST::Expr::Kind::Identifier)
	{
	  /* This is a break with an expression, and the expression is
	     just a single identifier.  See if the identifier is either
	     "rust" or "gcc", in which case we have "break rust" or "break
	     gcc", and so may need to emit our funny error.  We cannot yet
	     emit the error here though, because the identifier may still
	     be in scope, and ICE'ing on valid programs would not be very
	     funny.  */
	  std::string ident
	    = static_cast<AST::IdentifierExpr &> (expr.get_break_expr ())
		.as_string ();
	  if (ident == "rust" || ident == "gcc")
	    funny_error = true;
	}
    }

  DefaultResolver::visit (expr);

  funny_error = false;
}

void
Late::visit (AST::LoopLabel &label)
{
  auto &lifetime = label.get_lifetime ();
  ctx.labels.insert (Identifier (lifetime.as_string (), lifetime.get_locus ()),
		     lifetime.get_node_id ());
}

void
Late::resolve_label (AST::Lifetime &lifetime)
{
  if (auto resolved = ctx.labels.get (lifetime.as_string ()))
    {
      if (resolved->get_node_id () != lifetime.get_node_id ())
	ctx.map_usage (Usage (lifetime.get_node_id ()),
		       Definition (resolved->get_node_id ()));
    }
  else
    rust_error_at (lifetime.get_locus (), ErrorCode::E0426,
		   "use of undeclared label %qs",
		   lifetime.as_string ().c_str ());
}

void
Late::visit (AST::ContinueExpr &expr)
{
  if (expr.has_label ())
    resolve_label (expr.get_label_unchecked ());

  DefaultResolver::visit (expr);
}

void
Late::visit (AST::IdentifierExpr &expr)
{
  // TODO: same thing as visit(PathInExpression) here?

  tl::optional<Rib::Definition> resolved = tl::nullopt;
  if (auto value = ctx.values.get (expr.get_ident ()))
    {
      resolved = value;
    }
  else if (auto type = ctx.types.get (expr.get_ident ()))
    {
      resolved = type;
    }
  else if (funny_error)
    {
      diagnostics::text_finalizer (global_dc)
	= Resolver::funny_ice_text_finalizer;
      emit_diagnostic (diagnostics::kind::ice_nobt, expr.get_locus (), -1,
		       "are you trying to break %s? how dare you?",
		       expr.as_string ().c_str ());
    }
  else
    {
      if (auto type = ctx.types.get_lang_prelude (expr.get_ident ()))
	{
	  resolved = type;
	}
      else
	{
	  rust_error_at (expr.get_locus (), ErrorCode::E0425,
			 "cannot find value %qs in this scope",
			 expr.get_ident ().as_string ().c_str ());
	  return;
	}
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (expr.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     expr.as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (resolved->get_node_id ()));

  // For empty types, do we perform a lookup in ctx.types or should the
  // toplevel instead insert a name in ctx.values? (like it currently does)
}

void
Late::visit (AST::StructExprFieldIdentifier &expr)
{
  tl::optional<Rib::Definition> resolved = tl::nullopt;

  if (auto value = ctx.values.get (expr.get_field_name ()))
    {
      resolved = value;
    }
  // seems like we don't need a type namespace lookup
  else
    {
      rust_error_at (expr.get_locus (), "could not resolve struct field: %qs",
		     expr.get_field_name ().as_string ().c_str ());
      return;
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (expr.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     expr.as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::PathInExpression &expr)
{
  // TODO: How do we have a nice error with `can't capture dynamic environment
  // in a function item` error here?
  // do we emit it in `get<Namespace::Labels>`?

  DefaultResolver::visit (expr);

  if (expr.is_lang_item ())
    {
      ctx.map_usage (Usage (expr.get_node_id ()),
		     Definition (Analysis::Mappings::get ().get_lang_item_node (
		       expr.get_lang_item ())));
      return;
    }

  auto resolved = ctx.resolve_path (expr, Namespace::Values, Namespace::Types);

  if (!resolved)
    {
      if (!ctx.lookup (expr.get_segments ().front ().get_node_id ()))
	rust_error_at (expr.get_locus (), ErrorCode::E0433,
		       "Cannot find path %qs in this scope",
		       expr.as_simple_path ().as_string ().c_str ());
      return;
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (expr.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     expr.as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (expr.get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit_impl_type (AST::Type &type)
{
  // TODO: does this have to handle reentrancy?
  rust_assert (!block_big_self);
  block_big_self = true;
  visit (type);
  block_big_self = false;
}

template <typename P>
static void
resolve_type_path_like (NameResolutionContext &ctx, bool block_big_self,
			P &type)
{
  // should we add type path resolution in `ForeverStack` directly? Since it's
  // quite more complicated.
  // maybe we can overload `resolve_path<Namespace::Types>` to only do
  // typepath-like path resolution? that sounds good

  // prevent "impl Self {}" and similar
  if (type.get_segments ().size () == 1
      && !unwrap_segment_get_lang_item (type.get_segments ().front ())
	    .has_value ()
      && unwrap_type_segment (type.get_segments ().front ()).is_big_self_seg ()
      && block_big_self)
    {
      rust_error_at (type.get_locus (),
		     "%<Self%> is not valid in the self type of an impl block");
      return;
    }

  // this *should* mostly work
  // TODO: make sure typepath-like path resolution (?) is working
  auto resolved = ctx.resolve_path (type, Namespace::Types);

  if (!resolved.has_value ())
    {
      if (!ctx.lookup (unwrap_segment_node_id (type.get_segments ().front ())))
	rust_error_at (type.get_locus (), ErrorCode::E0412,
		       "could not resolve type path %qs",
		       unwrap_segment_error_string (type).c_str ());
      return;
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (type.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     unwrap_segment_error_string (type).c_str ());
      return;
    }

  if (ctx.types.forward_declared (resolved->get_node_id (),
				  type.get_node_id ()))
    {
      rust_error_at (type.get_locus (), ErrorCode::E0128,
		     "type parameters with a default cannot use forward "
		     "declared identifiers");
    }

  ctx.map_usage (Usage (type.get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::TypePath &type)
{
  DefaultResolver::visit (type);

  resolve_type_path_like (ctx, block_big_self, type);
}

void
Late::visit (AST::Visibility &vis)
{
  if (!vis.has_path ())
    return;

  AST::SimplePath &path = vis.get_path ();

  rust_assert (path.get_segments ().size ());
  auto &first_seg = path.get_segments ()[0];

  auto mode = ResolutionMode::Normal;

  if (path.has_opening_scope_resolution ())
    {
      if (get_rust_edition () == Edition::E2015)
	mode = ResolutionMode::FromRoot;
      else
	mode = ResolutionMode::FromExtern;
    }
  else if (!first_seg.is_crate_path_seg () && !first_seg.is_super_path_seg ()
	   && !first_seg.is_lower_self_seg ())
    {
      if (get_rust_edition () == Edition::E2015)
	{
	  mode = ResolutionMode::FromRoot;
	}
      else
	{
	  rust_error_at (path.get_locus (),
			 "relative paths are not supported in visibilities in "
			 "2018 edition or later");
	  return;
	}
    }

  auto res = ctx.resolve_path (path.get_segments (), mode, Namespace::Types);

  if (!res.has_value ())
    {
      rust_error_at (path.get_locus (), ErrorCode::E0433,
		     "could not resolve path %qs", path.as_string ().c_str ());
      return;
    }

  // TODO: is this possible?
  if (res->is_ambiguous ())
    {
      rust_error_at (path.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     path.as_string ().c_str ());
      return;
    }

  ctx.map_usage (Usage (path.get_node_id ()), Definition (res->get_node_id ()));
}

void
Late::visit (AST::Trait &trait)
{
  // kind of weird how this is done
  // names are resolved to the node id of trait.get_implicit_self ()
  // which is then resolved to the node id of trait
  // we set up the latter mapping here
  ctx.map_usage (Usage (trait.get_implicit_self ().get_node_id ()),
		 Definition (trait.get_node_id ()));

  DefaultResolver::visit (trait);
}

void
Late::visit (AST::StructExprStruct &s)
{
  visit_outer_attrs (s);
  visit_inner_attrs (s);
  DefaultResolver::visit (s.get_struct_name ());

  resolve_type_path_like (ctx, block_big_self, s.get_struct_name ());
}

void
Late::visit (AST::StructExprStructBase &s)
{
  visit_outer_attrs (s);
  visit_inner_attrs (s);
  DefaultResolver::visit (s.get_struct_name ());
  visit (s.get_struct_base ());

  resolve_type_path_like (ctx, block_big_self, s.get_struct_name ());
}

void
Late::visit (AST::StructExprStructFields &s)
{
  visit_outer_attrs (s);
  visit_inner_attrs (s);
  DefaultResolver::visit (s.get_struct_name ());
  if (s.has_struct_base ())
    visit (s.get_struct_base ());
  for (auto &field : s.get_fields ())
    visit (field);

  resolve_type_path_like (ctx, block_big_self, s.get_struct_name ());
}

// needed because Late::visit (AST::GenericArg &) is non-virtual
void
Late::visit (AST::GenericArgs &args)
{
  for (auto &lifetime : args.get_lifetime_args ())
    visit (lifetime);

  for (auto &generic : args.get_generic_args ())
    visit (generic);

  for (auto &binding : args.get_binding_args ())
    visit (binding);
}

void
Late::visit (AST::GenericArg &arg)
{
  if (arg.get_kind () == AST::GenericArg::Kind::Either)
    {
      // prefer type parameter to const parameter on ambiguity
      auto type = ctx.types.get (arg.get_path ());
      auto value = ctx.values.get (arg.get_path ());

      if (!type.has_value () && value.has_value ())
	arg = arg.disambiguate_to_const ();
      else
	arg = arg.disambiguate_to_type ();
    }

  DefaultResolver::visit (arg);
}

void
Late::visit_closure_params (AST::ClosureExpr &closure)
{
  ctx.bindings.enter (BindingSource::Param);

  DefaultResolver::visit_closure_params (closure);

  ctx.bindings.exit ();
}

void
Late::visit (AST::ClosureExpr &expr)
{
  // add captures
  auto vals = ctx.values.peek ().get_values ();
  for (auto &val : vals)
    {
      ctx.mappings.add_capture (expr.get_node_id (), val.second.get_node_id ());
    }

  DefaultResolver::visit (expr);
}

} // namespace Resolver2_0
} // namespace Rust
