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

#ifndef RUST_BIR_BUILDER_INTERNAL_H
#define RUST_BIR_BUILDER_INTERNAL_H

#include "rust-bir-place.h"
#include "rust-hir-expr.h"
#include "rust-hir-item.h"
#include "rust-hir-type-check.h"
#include "rust-hir-visitor.h"
#include "rust-name-resolver.h"
#include "rust-bir.h"

namespace Rust {

namespace BIR {

/** Holds the context of BIR building so that it can be shared/passed between
 * different builders. */
struct BuilderContext
{
  class LifetimeResolver
  {
    using Index = uint32_t;
    using Value = std::string;

    Index next_index = FIRST_NORMAL_LIFETIME_ID;
    std::unordered_map<Value, Index> value_to_index;

  public:
    Index resolve (const Value &value)
    {
      auto found = value_to_index.find (value);
      if (found != value_to_index.end ())
	{
	  return found->second;
	}
      value_to_index.emplace (value, next_index);
      return next_index++;
    }

    /** Returns a new anonymous lifetime. */
    Index get_anonymous () { return next_index++; }
  };

  struct LoopAndLabelCtx
  {
    bool is_loop;      // Loop or labelled block
    NodeId label;      // UNKNOWN_NODEID if no label (loop)
    PlaceId label_var; // For break with value.
    BasicBlockId break_bb;
    BasicBlockId continue_bb; // Only valid for loops
    ScopeId continue_scope;
    // Break scope is the parent of the `continue_scope`.

    LoopAndLabelCtx (bool is_loop = false, NodeId label = UNKNOWN_NODEID,
		     PlaceId label_var = INVALID_PLACE,
		     BasicBlockId break_bb = INVALID_BB,
		     BasicBlockId continue_bb = INVALID_BB,
		     ScopeId continue_scope = INVALID_SCOPE)
      : is_loop (is_loop), label (label), label_var (label_var),
	break_bb (break_bb), continue_bb (continue_bb),
	continue_scope (continue_scope)
    {}
  };

  // External context.
  Resolver::TypeCheckContext &tyctx;
  Resolver::Resolver &resolver;

  // BIR output
  std::vector<BasicBlock> basic_blocks;
  size_t current_bb = 0;

  /**
   * Allocation and lookup of places (variables, temporaries, paths, and
   * constants)
   */
  PlaceDB place_db;
  LifetimeResolver lifetime_interner;
  // Used for cleaner dump.
  std::vector<PlaceId> arguments;
  /**
   * Since labels can be used to return values, we need to reserve a place for
   * them. This map associates labels with their respective places.
   */
  std::unordered_map<NodeId, PlaceId> label_place_map;

  /** Context for current situation (loop, label, etc.) */
  std::vector<LoopAndLabelCtx> loop_and_label_stack;

public:
  BuilderContext ()
    : tyctx (*Resolver::TypeCheckContext::get ()),
      resolver (*Resolver::Resolver::get ())
  {
    basic_blocks.emplace_back (); // StartBB
  }

  BasicBlock &get_current_bb () { return basic_blocks[current_bb]; }

  Lifetime lookup_lifetime (const tl::optional<HIR::Lifetime> &lifetime)
  {
    if (!lifetime.has_value ())
      return {lifetime_interner.get_anonymous ()};
    switch (lifetime->get_lifetime_type ())
      {
	case AST::Lifetime::NAMED: {
	  return {lifetime_interner.resolve (lifetime->get_name ())};
	}
	case AST::Lifetime::STATIC: {
	  return STATIC_LIFETIME;
	}
	case AST::Lifetime::WILDCARD: {
	  rust_sorry_at (lifetime->get_locus (),
			 "lifetime elision is not yet implemented");
	  return NO_LIFETIME;
	}
      }
    rust_unreachable ();
  };

  const LoopAndLabelCtx &lookup_label (NodeId label)
  {
    auto label_match = [label] (const LoopAndLabelCtx &info) {
      return info.label != UNKNOWN_NODEID && info.label == label;
    };

    auto found = std::find_if (loop_and_label_stack.rbegin (),
			       loop_and_label_stack.rend (), label_match);
    rust_assert (found != loop_and_label_stack.rend ());
    return *found;
  }
};

/** Common infrastructure for building BIR from HIR. */
class AbstractBuilder
{
protected:
  BuilderContext &ctx;

  /**
   * This emulates the return value of the visitor, to be able to use the
   * current visitor infrastructure, where the return value is forced to be
   * void.
   */
  PlaceId translated = INVALID_PLACE;

protected:
  explicit AbstractBuilder (BuilderContext &ctx) : ctx (ctx) {}

  PlaceId declare_variable (const Analysis::NodeMapping &node)
  {
    return declare_variable (node, lookup_type (node.get_hirid ()));
  }

  PlaceId declare_variable (const Analysis::NodeMapping &node,
			    TyTy::BaseType *ty)
  {
    const NodeId nodeid = node.get_nodeid ();

    // In debug mode, check that the variable is not already declared.
    rust_assert (ctx.place_db.lookup_variable (nodeid) == INVALID_PLACE);

    auto place = ctx.place_db.add_variable (nodeid, ty);

    if (ctx.place_db.get_current_scope_id () != 0)
      push_storage_live (place);

    return place;
  }

  void push_new_scope () { ctx.place_db.push_new_scope (); }

  void pop_scope ()
  {
    auto &scope = ctx.place_db.get_current_scope ();
    if (ctx.place_db.get_current_scope_id () != 0)
      {
	std::for_each (scope.locals.rbegin (), scope.locals.rend (),
		       [&] (PlaceId place) { push_storage_dead (place); });
      }
    ctx.place_db.pop_scope ();
  }

  void unwind_until (ScopeId final_scope)
  {
    auto current_scope_id = ctx.place_db.get_current_scope_id ();
    while (current_scope_id != final_scope)
      {
	auto &scope = ctx.place_db.get_scope (current_scope_id);
	std::for_each (scope.locals.rbegin (), scope.locals.rend (),
		       [&] (PlaceId place) { push_storage_dead (place); });
	current_scope_id = scope.parent;
      }
  }

protected: // Helpers to add BIR statements
  void push_assignment (PlaceId lhs, AbstractExpr *rhs)
  {
    ctx.get_current_bb ().statements.emplace_back (lhs, rhs);
    translated = lhs;
  }

  void push_assignment (PlaceId lhs, PlaceId rhs)
  {
    push_assignment (lhs, new Assignment (rhs));
  }

  void push_tmp_assignment (AbstractExpr *rhs, TyTy::BaseType *tyty)
  {
    PlaceId tmp = ctx.place_db.add_temporary (tyty);
    push_storage_live (tmp);
    push_assignment (tmp, rhs);
  }

  void push_tmp_assignment (PlaceId rhs)
  {
    push_tmp_assignment (new Assignment (rhs), ctx.place_db[rhs].tyty);
  }

  void push_switch (PlaceId switch_val,
		    std::initializer_list<BasicBlockId> destinations = {})
  {
    auto copy = move_place (switch_val);
    ctx.get_current_bb ().statements.emplace_back (Statement::Kind::SWITCH,
						   copy);
    ctx.get_current_bb ().successors.insert (
      ctx.get_current_bb ().successors.end (), destinations);
  }

  void push_goto (BasicBlockId bb)
  {
    ctx.get_current_bb ().statements.emplace_back (Statement::Kind::GOTO);
    if (bb != INVALID_BB) // INVALID_BB means the goto will be resolved later.
      ctx.get_current_bb ().successors.push_back (bb);
  }

  void push_storage_live (PlaceId place)
  {
    ctx.get_current_bb ().statements.emplace_back (
      Statement::Kind::STORAGE_LIVE, place);
  }

  void push_storage_dead (PlaceId place)
  {
    ctx.get_current_bb ().statements.emplace_back (
      Statement::Kind::STORAGE_DEAD, place);
  }

  PlaceId move_place (PlaceId arg)
  {
    if (ctx.place_db[arg].is_lvalue ())
      {
	push_tmp_assignment (arg);
	arg = translated;
      }

    return arg;
  }

  template <typename T> void move_all (T &args)
  {
    std::transform (args.begin (), args.end (), args.begin (),
		    [this] (PlaceId arg) { return move_place (arg); });
  }

protected: // CFG helpers
  BasicBlockId new_bb ()
  {
    ctx.basic_blocks.emplace_back ();
    return ctx.basic_blocks.size () - 1;
  }

  BasicBlockId start_new_consecutive_bb ()
  {
    BasicBlockId bb = new_bb ();
    if (!ctx.get_current_bb ().is_terminated ())
      {
	push_goto (bb);
      }
    else
      {
	add_jump_to (bb);
      }
    ctx.current_bb = bb;
    return bb;
  }

  void add_jump (BasicBlockId from, BasicBlockId to)
  {
    ctx.basic_blocks[from].successors.emplace_back (to);
  }

  void add_jump_to (BasicBlockId bb) { add_jump (ctx.current_bb, bb); }

protected: // HIR resolution helpers
  template <typename T>
  WARN_UNUSED_RESULT TyTy::BaseType *lookup_type (T &hir_node) const
  {
    return lookup_type (hir_node.get_mappings ().get_hirid ());
  }

  WARN_UNUSED_RESULT TyTy::BaseType *lookup_type (HirId hirid) const
  {
    TyTy::BaseType *type = nullptr;
    bool ok = ctx.tyctx.lookup_type (hirid, &type);
    rust_assert (ok);
    rust_assert (type != nullptr);
    return type;
  }

  template <typename T> NodeId resolve_label (T &expr)
  {
    NodeId resolved_label;
    bool ok
      = ctx.resolver.lookup_resolved_label (expr.get_mappings ().get_nodeid (),
					    &resolved_label);
    rust_assert (ok);
    return resolved_label;
  }

  template <typename T> PlaceId resolve_variable (T &variable)
  {
    NodeId variable_id;
    bool ok = ctx.resolver.lookup_resolved_name (
      variable.get_mappings ().get_nodeid (), &variable_id);
    rust_assert (ok);
    return ctx.place_db.lookup_variable (variable_id);
  }

  template <typename T>
  PlaceId resolve_variable_or_fn (T &variable, TyTy::BaseType *ty)
  {
    // Unlike variables,
    // functions do not have to be declared in PlaceDB before use.
    NodeId variable_id;
    bool ok = ctx.resolver.lookup_resolved_name (
      variable.get_mappings ().get_nodeid (), &variable_id);
    rust_assert (ok);
    return ctx.place_db.lookup_or_add_variable (variable_id,
						(ty) ? ty
						     : lookup_type (variable));
  }

protected: // Implicit conversions.
  /**
   * Performs implicit coercions on the `translated` place defined for a
   * coercion site.
   *
   * Reference: https://doc.rust-lang.org/reference/type-coercions.html
   *
   * The only coercion relevant to BIR is the autoderef. All other coercions
   * will be taken in account because the type is extracted from each node and
   * not derived from operations in HIR/BIR. The borrowck does not care about
   * type transitions. Lifetimes are not coerced, rather new are created with
   * defined bounds to the original ones.
   */
  void coercion_site (PlaceId &place, TyTy::BaseType *expected_ty)
  {
    auto count_ref_levels = [] (TyTy::BaseType *ty) {
      size_t count = 0;
      while (auto r = ty->try_as<TyTy::ReferenceType> ())
	{
	  ty = r->get_base ();
	  count++;
	}
      return count;
    };

    auto actual_ty = ctx.place_db[place].tyty;

    auto deref_count
      = count_ref_levels (actual_ty) - count_ref_levels (expected_ty);

    for (size_t i = 0; i < deref_count; ++i)
      {
	actual_ty = actual_ty->as<TyTy::ReferenceType> ()->get_base ();
	place
	  = ctx.place_db.lookup_or_add_path (Place::DEREF, actual_ty, place);
      }
  }

  /** Dereferences the `translated` place until it is at most one reference
   * and return the base type. */
  TyTy::BaseType *autoderef (PlaceId &place)
  {
    auto ty = ctx.place_db[place].tyty;
    while (auto ref_ty = ty->try_as<TyTy::ReferenceType> ())
      {
	ty = ref_ty->get_base ();
	place = ctx.place_db.lookup_or_add_path (Place::DEREF, ty, place);
      }
    return ty;
  }

  void autoref ()
  {
    if (ctx.place_db[translated].tyty->get_kind () != TyTy::REF)
      {
	auto ty = ctx.place_db[translated].tyty;
	push_tmp_assignment (
	  new BorrowExpr (translated),
	  new TyTy::ReferenceType (ty->get_ref (), TyTy::TyVar (ty->get_ref ()),
				   Mutability::Imm));
      }
  }
};

class AbstractExprBuilder : public AbstractBuilder,
			    public HIR::HIRExpressionVisitor
{
protected:
  /**
   * Optional place for the result of the evaluated expression.
   * Valid if value is not `INVALID_PLACE`.
   * Used when return place must be created by caller (return for if-else).
   */
  PlaceId expr_return_place = INVALID_PLACE;

protected:
  explicit AbstractExprBuilder (BuilderContext &ctx,
				PlaceId expr_return_place = INVALID_PLACE)
    : AbstractBuilder (ctx), expr_return_place (expr_return_place)
  {}

  /**
   * Wrapper that provides return value based API inside a visitor which has to
   * use global state to pass the data around.
   * @param dst_place Place to assign the produced value to, optionally
   * allocated by the caller.
   * */
  PlaceId visit_expr (HIR::Expr &expr, PlaceId dst_place = INVALID_PLACE)
  {
    // Save to support proper recursion.
    auto saved = expr_return_place;
    expr_return_place = dst_place;
    translated = INVALID_PLACE;
    expr.accept_vis (*this);
    expr_return_place = saved;
    auto result = translated;
    translated = INVALID_PLACE;
    return result;
  }

  /**
   * Create a return value of a subexpression, which produces an expression.
   * Use `return_place` for subexpression that only produce a place (look it up)
   * to avoid needless assignments.
   *
   * @param can_panic mark that expression can panic to insert jump to cleanup.
   */
  void return_expr (AbstractExpr *expr, TyTy::BaseType *ty,
		    bool can_panic = false)
  {
    if (expr_return_place != INVALID_PLACE)
      {
	push_assignment (expr_return_place, expr);
      }
    else
      {
	push_tmp_assignment (expr, ty);
      }

    if (can_panic)
      {
	start_new_consecutive_bb ();
      }
  }

  /** Mark place to be a result of processed subexpression. */
  void return_place (PlaceId place)
  {
    if (expr_return_place != INVALID_PLACE)
      {
	// Return place is already allocated, no need to defer assignment.
	push_assignment (expr_return_place, place);
      }
    else
      {
	translated = place;
      }
  }

  /** Explicitly return a unit value. Expression produces no value. */
  void return_unit (HIR::Expr &expr)
  {
    translated = ctx.place_db.get_constant (lookup_type (expr));
  }

  PlaceId take_or_create_return_place (TyTy::BaseType *type)
  {
    PlaceId result = INVALID_PLACE;
    if (expr_return_place != INVALID_PLACE)
      {
	result = expr_return_place;
	expr_return_place = INVALID_PLACE;
      }
    else
      {
	result = ctx.place_db.add_temporary (type);
	push_storage_live (result);
      }
    return result;
  }
};

/**
 * Helper to convert a pointer to an optional. Maps nullptr to nullopt.
 * Optionals are mainly used here to provide monadic operations (map) over
 * possibly null pointers.
 */
template <typename T>
tl::optional<T>
optional_from_ptr (T ptr)
{
  if (ptr != nullptr)
    return {ptr};
  else
    return tl::nullopt;
}

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_BUILDER_INTERNAL_H
