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

#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

void
TypeCheckExpr::visit (HIR::RangeFromToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());
  TyTy::BaseType *to_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());
  TyTy::BaseType *unified = from_ty->unify (to_ty);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFromExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FROM;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_TO;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFullExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FULL;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->is_unit ());

  infered = item_type;
}

void
TypeCheckExpr::visit (HIR::RangeFromToInclExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_INCLUSIVE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get ());
  TyTy::BaseType *to_ty = TypeCheckExpr::Resolve (expr.get_to_expr ().get ());
  TyTy::BaseType *unified = from_ty->unify (to_ty);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::ArrayIndexExpr &expr)
{
  auto array_expr_ty = TypeCheckExpr::Resolve (expr.get_array_expr ());
  if (array_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  auto index_expr_ty = TypeCheckExpr::Resolve (expr.get_index_expr ());
  if (index_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
    return;

  // is this a case of core::ops::index?
  auto lang_item_type = Analysis::RustLangItem::ItemType::INDEX;
  bool operator_overloaded
    = resolve_operator_overload (lang_item_type, expr, array_expr_ty,
				 index_expr_ty);
  if (operator_overloaded)
    {
      // index and index mut always return a reference to the element
      TyTy::BaseType *resolved = infered;
      rust_assert (resolved->get_kind () == TyTy::TypeKind::REF);
      TyTy::ReferenceType *ref = static_cast<TyTy::ReferenceType *> (resolved);

      infered = ref->get_base ()->clone ();
      return;
    }

  if (array_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      // lets try and deref it since rust allows this
      auto ref = static_cast<TyTy::ReferenceType *> (array_expr_ty);
      auto base = ref->get_base ();
      if (base->get_kind () == TyTy::TypeKind::ARRAY)
	array_expr_ty = base;
    }

  if (array_expr_ty->get_kind () != TyTy::TypeKind::ARRAY)
    {
      rust_error_at (expr.get_index_expr ()->get_locus (),
		     "expected an ArrayType got [%s]",
		     array_expr_ty->as_string ().c_str ());
      return;
    }

  TyTy::BaseType *size_ty;
  bool ok = context->lookup_builtin ("usize", &size_ty);
  rust_assert (ok);

  auto resolved_index_expr = size_ty->unify (index_expr_ty);
  if (resolved_index_expr->get_kind () == TyTy::TypeKind::ERROR)
    return;

  TyTy::ArrayType *array_type = static_cast<TyTy::ArrayType *> (array_expr_ty);
  infered = array_type->get_element_type ()->clone ();
}

bool
TypeCheckExpr::resolve_operator_overload (
  Analysis::RustLangItem::ItemType lang_item_type, HIR::OperatorExprMeta expr,
  TyTy::BaseType *lhs, TyTy::BaseType *rhs)
{
  // look up lang item for arithmetic type
  std::string associated_item_name
    = Analysis::RustLangItem::ToString (lang_item_type);
  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // probe for the lang-item
  if (!lang_item_defined)
    return false;

  auto segment = HIR::PathIdentSegment (associated_item_name);
  auto candidate
    = MethodResolver::Probe (lhs, HIR::PathIdentSegment (associated_item_name));

  bool have_implementation_for_lang_item = !candidate.is_error ();
  if (!have_implementation_for_lang_item)
    return false;

  // Get the adjusted self
  Adjuster adj (lhs);
  TyTy::BaseType *adjusted_self = adj.adjust_type (candidate.adjustments);

  // is this the case we are recursive
  // handle the case where we are within the impl block for this lang_item
  // otherwise we end up with a recursive operator overload such as the i32
  // operator overload trait
  TypeCheckContextItem &fn_context = context->peek_context ();
  if (fn_context.get_type () == TypeCheckContextItem::ItemType::IMPL_ITEM)
    {
      auto &impl_item = fn_context.get_impl_item ();
      HIR::ImplBlock *parent = impl_item.first;
      HIR::Function *fn = impl_item.second;

      if (parent->has_trait_ref ()
	  && fn->get_function_name ().compare (associated_item_name) == 0)
	{
	  TraitReference *trait_reference
	    = TraitResolver::Lookup (*parent->get_trait_ref ().get ());
	  if (!trait_reference->is_error ())
	    {
	      TyTy::BaseType *lookup = nullptr;
	      bool ok = context->lookup_type (fn->get_mappings ().get_hirid (),
					      &lookup);
	      rust_assert (ok);
	      rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);

	      TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup);
	      rust_assert (fntype->is_method ());

	      bool is_lang_item_impl
		= trait_reference->get_mappings ().get_defid ()
		  == respective_lang_item_id;
	      bool self_is_lang_item_self
		= fntype->get_self_type ()->is_equal (*adjusted_self);
	      bool recursive_operator_overload
		= is_lang_item_impl && self_is_lang_item_self;

	      if (recursive_operator_overload)
		return false;
	    }
	}
    }

  // store the adjustments for code-generation to know what to do
  context->insert_autoderef_mappings (expr.get_lvalue_mappings ().get_hirid (),
				      std::move (candidate.adjustments));

  // now its just like a method-call-expr
  context->insert_receiver (expr.get_mappings ().get_hirid (), lhs);

  PathProbeCandidate &resolved_candidate = candidate.candidate;
  TyTy::BaseType *lookup_tyty = candidate.candidate.ty;
  NodeId resolved_node_id
    = resolved_candidate.is_impl_candidate ()
	? resolved_candidate.item.impl.impl_item->get_impl_mappings ()
	    .get_nodeid ()
	: resolved_candidate.item.trait.item_ref->get_mappings ().get_nodeid ();

  rust_assert (lookup_tyty->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::BaseType *lookup = lookup_tyty;
  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
  rust_assert (fn->is_method ());

  auto root = lhs->get_root ();
  if (root->get_kind () == TyTy::TypeKind::ADT)
    {
      const TyTy::ADTType *adt = static_cast<const TyTy::ADTType *> (root);
      if (adt->has_substitutions () && fn->needs_substitution ())
	{
	  // consider the case where we have:
	  //
	  // struct Foo<X,Y>(X,Y);
	  //
	  // impl<T> Foo<T, i32> {
	  //   fn test<X>(self, a:X) -> (T,X) { (self.0, a) }
	  // }
	  //
	  // In this case we end up with an fn type of:
	  //
	  // fn <T,X> test(self:Foo<T,i32>, a:X) -> (T,X)
	  //
	  // This means the instance or self we are calling this method for
	  // will be substituted such that we can get the inherited type
	  // arguments but then need to use the turbo fish if available or
	  // infer the remaining arguments. Luckily rust does not allow for
	  // default types GenericParams on impl blocks since these must
	  // always be at the end of the list

	  auto s = fn->get_self_type ()->get_root ();
	  rust_assert (s->can_eq (adt, false));
	  rust_assert (s->get_kind () == TyTy::TypeKind::ADT);
	  const TyTy::ADTType *self_adt
	    = static_cast<const TyTy::ADTType *> (s);

	  // we need to grab the Self substitutions as the inherit type
	  // parameters for this
	  if (self_adt->needs_substitution ())
	    {
	      rust_assert (adt->was_substituted ());

	      TyTy::SubstitutionArgumentMappings used_args_in_prev_segment
		= GetUsedSubstArgs::From (adt);

	      TyTy::SubstitutionArgumentMappings inherit_type_args
		= self_adt->solve_mappings_from_receiver_for_self (
		  used_args_in_prev_segment);

	      // there may or may not be inherited type arguments
	      if (!inherit_type_args.is_error ())
		{
		  // need to apply the inherited type arguments to the
		  // function
		  lookup = fn->handle_substitions (inherit_type_args);
		}
	    }
	}
    }

  // handle generics
  if (lookup->needs_generic_substitutions ())
    lookup = SubstMapper::InferSubst (lookup, expr.get_locus ());

  // type check the arguments if required
  TyTy::FnType *type = static_cast<TyTy::FnType *> (lookup);
  rust_assert (type->num_params () > 0);
  auto fnparam = type->param_at (0);
  fnparam.second->unify (adjusted_self); // typecheck the self
  if (rhs == nullptr)
    {
      rust_assert (type->num_params () == 1);
    }
  else
    {
      rust_assert (type->num_params () == 2);
      auto fnparam = type->param_at (1);
      fnparam.second->unify (rhs); // typecheck the rhs
    }

  rust_assert (lookup->get_kind () == TyTy::TypeKind::FNDEF);
  fn = static_cast<TyTy::FnType *> (lookup);
  fn->monomorphize ();

  // get the return type
  TyTy::BaseType *function_ret_tyty
    = type->get_return_type ()->monomorphized_clone ();

  // store the expected fntype
  context->insert_operator_overload (expr.get_mappings ().get_hirid (), type);

  // set up the resolved name on the path
  resolver->insert_resolved_name (expr.get_mappings ().get_nodeid (),
				  resolved_node_id);

  // return the result of the function back
  infered = function_ret_tyty;

  return true;
}

} // namespace Resolver
} // namespace Rust
