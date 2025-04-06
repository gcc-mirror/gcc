// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "rust-hir-map.h"
#include "rust-late-name-resolver-2.0.h"
#include "rust-default-resolver.h"
#include "rust-name-resolution-context.h"
#include "rust-path.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"
#include "rust-ice-finalizer.h"
#include "rust-ast.h"

namespace Rust {
namespace Resolver2_0 {

Late::Late (NameResolutionContext &ctx) : DefaultResolver (ctx) {}

static NodeId
next_node_id ()
{
  return Analysis::Mappings::get ().get_next_node_id ();
};

static HirId
next_hir_id ()
{
  return Analysis::Mappings::get ().get_next_hir_id ();
};

void
Late::setup_builtin_types ()
{
  // access the global type context to setup the TyTys
  auto &ty_ctx = *Resolver::TypeCheckContext::get ();

  // Late builtin type struct helper
  struct LType
  {
    std::string name;
    NodeId node_id;
    NodeId hir_id;
    TyTy::BaseType *type;

    explicit LType (std::string name, TyTy::BaseType *type)
      : name (name), node_id (next_node_id ()), hir_id (type->get_ref ()),
	type (type)
    {}
  };

  static const LType builtins[] = {
    {LType ("bool", new TyTy::BoolType (next_hir_id ()))},
    {LType ("u8", new TyTy::UintType (next_hir_id (), TyTy::UintType::U8))},
    {LType ("u16", new TyTy::UintType (next_hir_id (), TyTy::UintType::U16))},
    {LType ("u32", new TyTy::UintType (next_hir_id (), TyTy::UintType::U32))},
    {LType ("u64", new TyTy::UintType (next_hir_id (), TyTy::UintType::U64))},
    {LType ("u128", new TyTy::UintType (next_hir_id (), TyTy::UintType::U128))},
    {LType ("i8", new TyTy::IntType (next_hir_id (), TyTy::IntType::I8))},
    {LType ("i16", new TyTy::IntType (next_hir_id (), TyTy::IntType::I16))},
    {LType ("i32", new TyTy::IntType (next_hir_id (), TyTy::IntType::I32))},
    {LType ("i64", new TyTy::IntType (next_hir_id (), TyTy::IntType::I64))},
    {LType ("i128", new TyTy::IntType (next_hir_id (), TyTy::IntType::I128))},
    {LType ("f32", new TyTy::FloatType (next_hir_id (), TyTy::FloatType::F32))},
    {LType ("f64", new TyTy::FloatType (next_hir_id (), TyTy::FloatType::F64))},
    {LType ("usize", new TyTy::USizeType (next_hir_id ()))},
    {LType ("isize", new TyTy::ISizeType (next_hir_id ()))},
    {LType ("char", new TyTy::CharType (next_hir_id ()))},
    {LType ("str", new TyTy::StrType (next_hir_id ()))},
    {LType ("!", new TyTy::NeverType (next_hir_id ()))},

    // the unit type `()` does not play a part in name-resolution - so we only
    // insert it in the type context...
  };

  // There's a special Rib for putting prelude items, since prelude items need
  // to satisfy certain special rules.
  ctx.scoped (Rib::Kind::Prelude, 0, [this, &ty_ctx] (void) -> void {
    for (const auto &builtin : builtins)
      {
	auto ok = ctx.types.insert (builtin.name, builtin.node_id);
	rust_assert (ok);

	ctx.mappings.insert_node_to_hir (builtin.node_id, builtin.hir_id);
	ty_ctx.insert_builtin (builtin.hir_id, builtin.node_id, builtin.type);
      }
  });

  // ...here!
  auto *unit_type = TyTy::TupleType::get_unit_type ();
  ty_ctx.insert_builtin (unit_type->get_ref (), next_node_id (), unit_type);
}

void
Late::go (AST::Crate &crate)
{
  setup_builtin_types ();

  for (auto &item : crate.items)
    item->accept_vis (*this);
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
  visit (expr.get_loop_label ());
  visit (expr.get_loop_block ());
}

void
Late::visit (AST::IfLetExpr &expr)
{
  visit_outer_attrs (expr);

  ctx.bindings.enter (BindingSource::Let);

  for (auto &pattern : expr.get_patterns ())
    visit (pattern);

  ctx.bindings.exit ();

  visit (expr.get_value_expr ());
  visit (expr.get_if_block ());
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
Late::visit (AST::IdentifierPattern &identifier)
{
  // do we insert in labels or in values
  // but values does not allow shadowing... since functions cannot shadow
  // do we insert functions in labels as well?

  if (ctx.bindings.peek ().is_and_bound (identifier.get_ident ()))
    {
      if (ctx.bindings.peek ().get_source () == BindingSource::Param)
	rust_error_at (
	  identifier.get_locus (), ErrorCode::E0415,
	  "identifier %qs is bound more than once in the same parameter list",
	  identifier.as_string ().c_str ());
      else
	rust_error_at (
	  identifier.get_locus (), ErrorCode::E0416,
	  "identifier %qs is bound more than once in the same pattern",
	  identifier.as_string ().c_str ());
      return;
    }

  ctx.bindings.peek ().insert_ident (identifier.get_ident ());

  if (ctx.bindings.peek ().is_or_bound (identifier.get_ident ()))
    {
      // FIXME: map usage instead
      std::ignore = ctx.values.insert_shadowable (identifier.get_ident (),
						  identifier.get_node_id ());
    }
  else
    {
      // We do want to ignore duplicated data because some situations rely on
      // it.
      std::ignore = ctx.values.insert_shadowable (identifier.get_ident (),
						  identifier.get_node_id ());
    }
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
Late::visit (AST::Function &function)
{
  auto def_fn = [this, &function] () {
    visit_outer_attrs (function);
    visit (function.get_visibility ());
    visit (function.get_qualifiers ());
    for (auto &generic : function.get_generic_params ())
      visit (generic);

    // We only care about params
    ctx.bindings.enter (BindingSource::Param);

    for (auto &param : function.get_function_params ())
      visit (param);

    ctx.bindings.exit ();

    // Back to regular visit

    if (function.has_return_type ())
      visit (function.get_return_type ());
    if (function.has_where_clause ())
      visit (function.get_where_clause ());
    if (function.has_body ())
      visit (*function.get_definition ());
  };

  ctx.scoped (Rib::Kind::Function, function.get_node_id (), def_fn);
}

void
Late::visit (AST::StructPatternFieldIdent &field)
{
  // We do want to ignore duplicated data because some situations rely on it.
  std::ignore = ctx.values.insert_shadowable (field.get_identifier (),
					      field.get_node_id ());
}

void
Late::visit (AST::SelfParam &param)
{
  // handle similar to AST::IdentifierPattern

  DefaultResolver::visit (param);
  // FIXME: this location should be a bit off
  // ex: would point to the begining of "mut self" instead of the "self"
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
      diagnostic_finalizer (global_dc) = Resolver::funny_ice_finalizer;
      emit_diagnostic (DK_ICE_NOBT, expr.get_locus (), -1,
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
Late::visit (AST::TypePath &type)
{
  // should we add type path resolution in `ForeverStack` directly? Since it's
  // quite more complicated.
  // maybe we can overload `resolve_path<Namespace::Types>` to only do
  // typepath-like path resolution? that sounds good

  DefaultResolver::visit (type);

  // this *should* mostly work
  // TODO: make sure typepath-like path resolution (?) is working
  auto resolved = ctx.resolve_path (type, Namespace::Types);

  if (!resolved.has_value ())
    {
      if (!ctx.lookup (type.get_segments ().front ()->get_node_id ()))
	rust_error_at (type.get_locus (), "could not resolve type path %qs",
		       type.as_string ().c_str ());
      return;
    }

  if (resolved->is_ambiguous ())
    {
      rust_error_at (type.get_locus (), ErrorCode::E0659, "%qs is ambiguous",
		     type.as_string ().c_str ());
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
Late::visit (AST::StructStruct &s)
{
  auto s_vis = [this, &s] () { AST::DefaultASTVisitor::visit (s); };
  ctx.scoped (Rib::Kind::Item, s.get_node_id (), s_vis);
}

void
Late::visit (AST::StructExprStruct &s)
{
  visit_outer_attrs (s);
  visit_inner_attrs (s);
  DefaultResolver::visit (s.get_struct_name ());

  auto resolved = ctx.resolve_path (s.get_struct_name (), Namespace::Types);

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
}

void
Late::visit (AST::StructExprStructBase &s)
{
  visit_outer_attrs (s);
  visit_inner_attrs (s);
  DefaultResolver::visit (s.get_struct_name ());
  visit (s.get_struct_base ());

  auto resolved = ctx.resolve_path (s.get_struct_name (), Namespace::Types);

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
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

  auto resolved = ctx.resolve_path (s.get_struct_name (), Namespace::Types);

  ctx.map_usage (Usage (s.get_struct_name ().get_node_id ()),
		 Definition (resolved->get_node_id ()));
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

template <class Closure>
static void
add_captures (Closure &closure, NameResolutionContext &ctx)
{
  auto vals = ctx.values.peek ().get_values ();
  for (auto &val : vals)
    {
      ctx.mappings.add_capture (closure.get_node_id (),
				val.second.get_node_id ());
    }
}

void
Late::visit (AST::ClosureExprInner &closure)
{
  add_captures (closure, ctx);

  visit_outer_attrs (closure);

  ctx.bindings.enter (BindingSource::Param);

  for (auto &param : closure.get_params ())
    visit (param);

  ctx.bindings.exit ();

  visit (closure.get_definition_expr ());
}

void
Late::visit (AST::ClosureExprInnerTyped &closure)
{
  add_captures (closure, ctx);

  visit_outer_attrs (closure);

  ctx.bindings.enter (BindingSource::Param);

  for (auto &param : closure.get_params ())
    visit (param);

  ctx.bindings.exit ();

  visit (closure.get_return_type ());
  visit (closure.get_definition_block ());
}

} // namespace Resolver2_0
} // namespace Rust
