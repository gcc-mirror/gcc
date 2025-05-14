// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

#include "rust-hir-full-decls.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-trait-resolve.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-trait-resolve.h"
#include "rust-type-util.h"

namespace Rust {
namespace Resolver {

TypeBoundsProbe::TypeBoundsProbe (const TyTy::BaseType *receiver)
  : TypeCheckBase (), receiver (receiver)
{}

std::vector<std::pair<TraitReference *, HIR::ImplBlock *>>
TypeBoundsProbe::Probe (const TyTy::BaseType *receiver)
{
  TypeBoundsProbe probe (receiver);
  probe.scan ();
  return probe.trait_references;
}

bool
TypeBoundsProbe::is_bound_satisfied_for_type (TyTy::BaseType *receiver,
					      TraitReference *ref)
{
  for (auto &bound : receiver->get_specified_bounds ())
    {
      const TraitReference *b = bound.get ();
      if (b->is_equal (*ref))
	return true;
    }

  std::vector<std::pair<TraitReference *, HIR::ImplBlock *>> bounds
    = Probe (receiver);
  for (auto &bound : bounds)
    {
      const TraitReference *b = bound.first;
      if (b->is_equal (*ref))
	return true;
    }

  return false;
}

void
TypeBoundsProbe::scan ()
{
  std::vector<std::pair<HIR::TypePath *, HIR::ImplBlock *>>
    possible_trait_paths;
  mappings.iterate_impl_blocks (
    [&] (HirId id, HIR::ImplBlock *impl) mutable -> bool {
      // we are filtering for trait-impl-blocks
      if (!impl->has_trait_ref ())
	return true;

      // can be recursive trait resolution
      HIR::Trait *t = TraitResolver::ResolveHirItem (impl->get_trait_ref ());
      if (t == nullptr)
	return true;
      DefId trait_id = t->get_mappings ().get_defid ();
      if (context->trait_query_in_progress (trait_id))
	return true;

      HirId impl_ty_id = impl->get_type ().get_mappings ().get_hirid ();
      TyTy::BaseType *impl_type = nullptr;
      if (!query_type (impl_ty_id, &impl_type))
	return true;

      if (!receiver->can_eq (impl_type, false))
	{
	  if (!impl_type->can_eq (receiver, false))
	    return true;
	}

      possible_trait_paths.push_back ({&impl->get_trait_ref (), impl});
      return true;
    });

  for (auto &path : possible_trait_paths)
    {
      HIR::TypePath *trait_path = path.first;
      TraitReference *trait_ref = TraitResolver::Resolve (*trait_path);

      if (!trait_ref->is_error ())
	trait_references.push_back ({trait_ref, path.second});
    }

  // marker traits...
  assemble_sized_builtin ();

  // add auto trait bounds
  for (auto *auto_trait : mappings.get_auto_traits ())
    add_trait_bound (auto_trait);
}

void
TypeBoundsProbe::assemble_sized_builtin ()
{
  const TyTy::BaseType *raw = receiver->destructure ();

  // https://runrust.miraheze.org/wiki/Dynamically_Sized_Type
  // everything is sized except for:
  //
  //   1. dyn traits
  //   2. slices
  //   3. str
  //   4. ADT's which contain any of the above
  //   t. tuples which contain any of the above
  switch (raw->get_kind ())
    {
    case TyTy::ARRAY:
    case TyTy::REF:
    case TyTy::POINTER:
    case TyTy::PARAM:
    case TyTy::FNDEF:
    case TyTy::FNPTR:
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::CLOSURE:
    case TyTy::INFER:
    case TyTy::NEVER:
    case TyTy::PLACEHOLDER:
    case TyTy::PROJECTION:
    case TyTy::OPAQUE:
      assemble_builtin_candidate (LangItem::Kind::SIZED);
      break;

      // FIXME str and slice need to be moved and test cases updated
    case TyTy::SLICE:
    case TyTy::STR:
    case TyTy::ADT:
    case TyTy::TUPLE:
      // FIXME add extra checks
      assemble_builtin_candidate (LangItem::Kind::SIZED);
      break;

    case TyTy::DYNAMIC:
    case TyTy::ERROR:
      break;
    }
}

void
TypeBoundsProbe::add_trait_bound (HIR::Trait *trait)
{
  auto trait_ref = TraitResolver::Resolve (*trait);

  trait_references.push_back ({trait_ref, mappings.lookup_builtin_marker ()});
}

void
TypeBoundsProbe::assemble_builtin_candidate (LangItem::Kind lang_item)
{
  auto lang_item_defined = mappings.lookup_lang_item (lang_item);
  if (!lang_item_defined)
    return;
  DefId &id = lang_item_defined.value ();

  auto defid = mappings.lookup_defid (id);
  if (!defid)
    return;
  auto item = defid.value ();

  rust_assert (item->get_item_kind () == HIR::Item::ItemKind::Trait);
  HIR::Trait *trait = static_cast<HIR::Trait *> (item);
  const TyTy::BaseType *raw = receiver->destructure ();

  add_trait_bound (trait);

  rust_debug ("Added builtin lang_item: %s for %s",
	      LangItem::ToString (lang_item).c_str (),
	      raw->get_name ().c_str ());
}

TraitReference *
TypeCheckBase::resolve_trait_path (HIR::TypePath &path)
{
  return TraitResolver::Resolve (path);
}

TyTy::TypeBoundPredicate
TypeCheckBase::get_predicate_from_bound (
  HIR::TypePath &type_path,
  tl::optional<std::reference_wrapper<HIR::Type>> associated_self,
  BoundPolarity polarity, bool is_qualified_type_path)
{
  TyTy::TypeBoundPredicate lookup = TyTy::TypeBoundPredicate::error ();
  bool already_resolved
    = context->lookup_predicate (type_path.get_mappings ().get_hirid (),
				 &lookup);
  if (already_resolved)
    return lookup;

  TraitReference *trait = resolve_trait_path (type_path);
  if (trait->is_error ())
    return TyTy::TypeBoundPredicate::error ();

  TyTy::TypeBoundPredicate predicate (*trait, polarity, type_path.get_locus ());
  HIR::GenericArgs args
    = HIR::GenericArgs::create_empty (type_path.get_locus ());

  auto &final_seg = type_path.get_final_segment ();
  switch (final_seg.get_type ())
    {
      case HIR::TypePathSegment::SegmentType::GENERIC: {
	auto &final_generic_seg
	  = static_cast<HIR::TypePathSegmentGeneric &> (final_seg);
	if (final_generic_seg.has_generic_args ())
	  {
	    args = final_generic_seg.get_generic_args ();
	    if (args.get_binding_args ().size () > 0
		&& associated_self.has_value () && is_qualified_type_path)
	      {
		auto &binding_args = args.get_binding_args ();

		rich_location r (line_table, args.get_locus ());
		for (auto it = binding_args.begin (); it != binding_args.end ();
		     it++)
		  {
		    auto &arg = *it;
		    r.add_fixit_remove (arg.get_locus ());
		  }
		rust_error_at (r, ErrorCode::E0229,
			       "associated type bindings are not allowed here");
	      }
	  }
      }
      break;

      case HIR::TypePathSegment::SegmentType::FUNCTION: {
	auto &final_function_seg
	  = static_cast<HIR::TypePathSegmentFunction &> (final_seg);
	auto &fn = final_function_seg.get_function_path ();

	// we need to make implicit generic args which must be an implicit
	// Tuple
	auto crate_num = mappings.get_current_crate ();
	HirId implicit_args_id = mappings.get_next_hir_id ();
	Analysis::NodeMapping mapping (crate_num,
				       final_seg.get_mappings ().get_nodeid (),
				       implicit_args_id, UNKNOWN_LOCAL_DEFID);

	std::vector<std::unique_ptr<HIR::Type>> params_copy;
	for (auto &p : fn.get_params ())
	  {
	    params_copy.push_back (p->clone_type ());
	  }

	std::vector<std::unique_ptr<HIR::Type>> inputs;
	inputs.push_back (
	  std::make_unique<HIR::TupleType> (mapping, std::move (params_copy),
					    final_seg.get_locus ()));

	// resolve the fn_once_output type which assumes there must be an output
	// set
	rust_assert (fn.has_return_type ());
	TypeCheckType::Resolve (fn.get_return_type ());

	HIR::TraitItem *trait_item
	  = mappings
	      .lookup_trait_item_lang_item (LangItem::Kind::FN_ONCE_OUTPUT,
					    final_seg.get_locus ())
	      .value ();

	std::vector<HIR::GenericArgsBinding> bindings;
	location_t output_locus = fn.get_return_type ().get_locus ();
	HIR::GenericArgsBinding binding (Identifier (
					   trait_item->trait_identifier ()),
					 fn.get_return_type ().clone_type (),
					 output_locus);
	bindings.push_back (std::move (binding));

	args = HIR::GenericArgs ({} /* lifetimes */,
				 std::move (inputs) /* type_args*/,
				 std::move (bindings) /* binding_args*/,
				 {} /* const_args */, final_seg.get_locus ());
      }
      break;

    default:
      /* nothing to do */
      break;
    }

  if (associated_self.has_value ())
    {
      std::vector<std::unique_ptr<HIR::Type>> type_args;
      type_args.push_back (std::unique_ptr<HIR::Type> (
	associated_self.value ().get ().clone_type ()));
      for (auto &arg : args.get_type_args ())
	{
	  type_args.push_back (std::unique_ptr<HIR::Type> (arg->clone_type ()));
	}

      args = HIR::GenericArgs (args.get_lifetime_args (), std::move (type_args),
			       args.get_binding_args (), args.get_const_args (),
			       args.get_locus ());
    }

  // we try to apply generic arguments when they are non empty and or when the
  // predicate requires them so that we get the relevant Foo expects x number
  // arguments but got zero see test case rust/compile/traits12.rs
  if (!args.is_empty () || predicate.requires_generic_args ())
    {
      // this is applying generic arguments to a trait reference
      predicate.apply_generic_arguments (&args, associated_self.has_value ());
    }

  context->insert_resolved_predicate (type_path.get_mappings ().get_hirid (),
				      predicate);

  return predicate;
}

} // namespace Resolver

namespace TyTy {

TypeBoundPredicate::TypeBoundPredicate (
  const Resolver::TraitReference &trait_reference, BoundPolarity polarity,
  location_t locus)
  : SubstitutionRef ({}, SubstitutionArgumentMappings::empty (), {}),
    reference (trait_reference.get_mappings ().get_defid ()), locus (locus),
    error_flag (false), polarity (polarity),
    super_traits (trait_reference.get_super_traits ())
{
  rust_assert (!trait_reference.get_trait_substs ().empty ());

  substitutions.clear ();
  for (const auto &p : trait_reference.get_trait_substs ())
    substitutions.push_back (p.clone ());

  // we setup a dummy implict self argument
  SubstitutionArg placeholder_self (&get_substs ().front (), nullptr);
  used_arguments.get_mappings ().push_back (placeholder_self);
}

TypeBoundPredicate::TypeBoundPredicate (
  DefId reference, std::vector<SubstitutionParamMapping> subst,
  BoundPolarity polarity, location_t locus)
  : SubstitutionRef ({}, SubstitutionArgumentMappings::empty (), {}),
    reference (reference), locus (locus), error_flag (false),
    polarity (polarity)
{
  rust_assert (!subst.empty ());

  substitutions.clear ();
  for (const auto &p : subst)
    substitutions.push_back (p.clone ());

  // we setup a dummy implict self argument
  SubstitutionArg placeholder_self (&get_substs ().front (), nullptr);
  used_arguments.get_mappings ().push_back (placeholder_self);
}

TypeBoundPredicate::TypeBoundPredicate (mark_is_error)
  : SubstitutionRef ({}, SubstitutionArgumentMappings::empty (), {}),
    reference (UNKNOWN_DEFID), locus (UNDEF_LOCATION), error_flag (true),
    polarity (BoundPolarity::RegularBound)
{}

TypeBoundPredicate::TypeBoundPredicate (const TypeBoundPredicate &other)
  : SubstitutionRef ({}, SubstitutionArgumentMappings::empty (), {}),
    reference (other.reference), locus (other.locus),
    error_flag (other.error_flag), polarity (other.polarity),
    super_traits (other.super_traits)
{
  substitutions.clear ();
  for (const auto &p : other.get_substs ())
    substitutions.push_back (p.clone ());

  std::vector<SubstitutionArg> mappings;
  for (size_t i = 0; i < other.used_arguments.get_mappings ().size (); i++)
    {
      const SubstitutionArg &oa = other.used_arguments.get_mappings ().at (i);
      SubstitutionArg arg (oa);
      mappings.push_back (std::move (arg));
    }

  // we need to remap the argument mappings based on this copied constructor
  std::vector<SubstitutionArg> copied_arg_mappings;
  size_t i = 0;
  for (const auto &m : other.used_arguments.get_mappings ())
    {
      TyTy::BaseType *argument
	= m.get_tyty () == nullptr ? nullptr : m.get_tyty ()->clone ();
      SubstitutionArg c (&substitutions.at (i++), argument);
      copied_arg_mappings.push_back (std::move (c));
    }

  used_arguments
    = SubstitutionArgumentMappings (copied_arg_mappings, {},
				    other.used_arguments.get_regions (),
				    other.used_arguments.get_locus ());
}

TypeBoundPredicate &
TypeBoundPredicate::operator= (const TypeBoundPredicate &other)
{
  reference = other.reference;
  locus = other.locus;
  error_flag = other.error_flag;
  polarity = other.polarity;
  used_arguments = SubstitutionArgumentMappings::empty ();

  substitutions.clear ();
  for (const auto &p : other.get_substs ())
    substitutions.push_back (p.clone ());

  if (other.is_error ())
    return *this;

  std::vector<SubstitutionArg> mappings;
  for (size_t i = 0; i < other.used_arguments.get_mappings ().size (); i++)
    {
      const SubstitutionArg &oa = other.used_arguments.get_mappings ().at (i);
      SubstitutionArg arg (oa);
      mappings.push_back (std::move (arg));
    }

  // we need to remap the argument mappings based on this copied constructor
  std::vector<SubstitutionArg> copied_arg_mappings;
  size_t i = 0;
  for (const auto &m : other.used_arguments.get_mappings ())
    {
      TyTy::BaseType *argument
	= m.get_tyty () == nullptr ? nullptr : m.get_tyty ()->clone ();
      SubstitutionArg c (&substitutions.at (i++), argument);
      copied_arg_mappings.push_back (std::move (c));
    }

  used_arguments
    = SubstitutionArgumentMappings (copied_arg_mappings, {},
				    other.used_arguments.get_regions (),
				    other.used_arguments.get_locus ());
  super_traits = other.super_traits;

  return *this;
}

TypeBoundPredicate
TypeBoundPredicate::error ()
{
  return TypeBoundPredicate (mark_is_error ());
}

std::string
TypeBoundPredicate::as_string () const
{
  return get ()->as_string () + subst_as_string ();
}

std::string
TypeBoundPredicate::as_name () const
{
  return get ()->get_name () + subst_as_string ();
}

const Resolver::TraitReference *
TypeBoundPredicate::get () const
{
  auto context = Resolver::TypeCheckContext::get ();

  Resolver::TraitReference *ref = nullptr;
  bool ok = context->lookup_trait_reference (reference, &ref);
  rust_assert (ok);

  return ref;
}

std::string
TypeBoundPredicate::get_name () const
{
  return get ()->get_name ();
}

bool
TypeBoundPredicate::is_object_safe (bool emit_error, location_t locus) const
{
  const Resolver::TraitReference *trait = get ();
  rust_assert (trait != nullptr);
  return trait->is_object_safe (emit_error, locus);
}

void
TypeBoundPredicate::apply_generic_arguments (HIR::GenericArgs *generic_args,
					     bool has_associated_self)
{
  rust_assert (!substitutions.empty ());
  if (has_associated_self)
    {
      used_arguments = SubstitutionArgumentMappings::empty ();
    }
  else
    {
      // we need to get the substitutions argument mappings but also remember
      // that we have an implicit Self argument which we must be careful to
      // respect
      rust_assert (!used_arguments.is_empty ());
    }

  // now actually perform a substitution
  auto args = get_mappings_from_generic_args (
    *generic_args,
    Resolver::TypeCheckContext::get ()->regions_from_generic_args (
      *generic_args));

  apply_argument_mappings (args);
}

void
TypeBoundPredicate::apply_argument_mappings (
  SubstitutionArgumentMappings &arguments)
{
  used_arguments = arguments;
  error_flag |= used_arguments.is_error ();
  auto &subst_mappings = used_arguments;
  for (auto &sub : get_substs ())
    {
      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok
	= subst_mappings.get_argument_for_symbol (sub.get_param_ty (), &arg);
      if (ok && arg.get_tyty () != nullptr)
	sub.fill_param_ty (subst_mappings, subst_mappings.get_locus ());
    }

  // associated argument mappings
  for (auto &it : subst_mappings.get_binding_args ())
    {
      std::string identifier = it.first;
      TyTy::BaseType *type = it.second;

      TypeBoundPredicateItem item = lookup_associated_item (identifier);
      rust_assert (!item.is_error ());

      const auto item_ref = item.get_raw_item ();
      item_ref->associated_type_set (type);
    }

  for (auto &super_trait : super_traits)
    {
      auto adjusted
	= super_trait.adjust_mappings_for_this (used_arguments,
						true /*trait mode*/);
      super_trait.apply_argument_mappings (adjusted);
    }
}

bool
TypeBoundPredicate::contains_item (const std::string &search) const
{
  auto trait_ref = get ();
  const Resolver::TraitItemReference *trait_item_ref = nullptr;
  return trait_ref->lookup_trait_item (search, &trait_item_ref);
}

TypeBoundPredicateItem
TypeBoundPredicate::lookup_associated_item (const std::string &search) const
{
  auto trait_ref = get ();
  const Resolver::TraitItemReference *trait_item_ref = nullptr;
  if (trait_ref->lookup_trait_item (search, &trait_item_ref,
				    false /*lookup supers*/))
    return TypeBoundPredicateItem (*this, trait_item_ref);

  for (auto &super_trait : super_traits)
    {
      auto lookup = super_trait.lookup_associated_item (search);
      if (!lookup.is_error ())
	return lookup;
    }

  return TypeBoundPredicateItem::error ();
}

TypeBoundPredicateItem::TypeBoundPredicateItem (
  const TypeBoundPredicate parent,
  const Resolver::TraitItemReference *trait_item_ref)
  : parent (parent), trait_item_ref (trait_item_ref)
{}

TypeBoundPredicateItem::TypeBoundPredicateItem (
  const TypeBoundPredicateItem &other)
  : parent (other.parent), trait_item_ref (other.trait_item_ref)
{}

TypeBoundPredicateItem &
TypeBoundPredicateItem::operator= (const TypeBoundPredicateItem &other)
{
  parent = other.parent;
  trait_item_ref = other.trait_item_ref;

  return *this;
}

TypeBoundPredicateItem
TypeBoundPredicateItem::error ()
{
  return TypeBoundPredicateItem (TypeBoundPredicate::error (), nullptr);
}

bool
TypeBoundPredicateItem::is_error () const
{
  return parent.is_error () || trait_item_ref == nullptr;
}

const TypeBoundPredicate *
TypeBoundPredicateItem::get_parent () const
{
  return &parent;
}

TypeBoundPredicateItem
TypeBoundPredicate::lookup_associated_item (
  const Resolver::TraitItemReference *ref) const
{
  return lookup_associated_item (ref->get_identifier ());
}

BaseType *
TypeBoundPredicateItem::get_tyty_for_receiver (const TyTy::BaseType *receiver)
{
  TyTy::BaseType *trait_item_tyty = get_raw_item ()->get_tyty ();
  if (parent.get_substitution_arguments ().is_empty ())
    return trait_item_tyty;

  const Resolver::TraitItemReference *tref = get_raw_item ();
  bool is_associated_type = tref->get_trait_item_type ();
  if (is_associated_type)
    return trait_item_tyty;

  // set up the self mapping
  SubstitutionArgumentMappings gargs = parent.get_substitution_arguments ();
  rust_assert (!gargs.is_empty ());

  // setup the adjusted mappings
  std::vector<SubstitutionArg> adjusted_mappings;
  for (size_t i = 0; i < gargs.get_mappings ().size (); i++)
    {
      auto &mapping = gargs.get_mappings ().at (i);

      bool is_implicit_self = i == 0;
      TyTy::BaseType *argument
	= is_implicit_self ? receiver->clone () : mapping.get_tyty ();

      SubstitutionArg arg (mapping.get_param_mapping (), argument);
      adjusted_mappings.push_back (std::move (arg));
    }

  SubstitutionArgumentMappings adjusted (adjusted_mappings, {},
					 gargs.get_regions (),
					 gargs.get_locus (),
					 gargs.get_subst_cb (),
					 true /* trait-mode-flag */);
  return Resolver::SubstMapperInternal::Resolve (trait_item_tyty, adjusted);
}
bool
TypeBoundPredicate::is_error () const
{
  auto context = Resolver::TypeCheckContext::get ();

  Resolver::TraitReference *ref = nullptr;
  bool ok = context->lookup_trait_reference (reference, &ref);

  return !ok || error_flag;
}

BaseType *
TypeBoundPredicate::handle_substitions (
  SubstitutionArgumentMappings &subst_mappings)
{
  for (auto &sub : get_substs ())
    {
      if (sub.get_param_ty () == nullptr)
	continue;

      ParamType *p = sub.get_param_ty ();
      BaseType *r = p->resolve ();
      BaseType *s = Resolver::SubstMapperInternal::Resolve (r, subst_mappings);

      p->set_ty_ref (s->get_ty_ref ());
    }

  // associated argument mappings
  for (auto &it : subst_mappings.get_binding_args ())
    {
      std::string identifier = it.first;
      TyTy::BaseType *type = it.second;

      TypeBoundPredicateItem item = lookup_associated_item (identifier);
      if (!item.is_error ())
	{
	  const auto item_ref = item.get_raw_item ();
	  item_ref->associated_type_set (type);
	}
    }

  // FIXME more error handling at some point
  // used_arguments = subst_mappings;
  // error_flag |= used_arguments.is_error ();

  return nullptr;
}

bool
TypeBoundPredicate::requires_generic_args () const
{
  if (is_error ())
    return false;

  return substitutions.size () > 1;
}

bool
TypeBoundPredicate::contains_associated_types () const
{
  return get_num_associated_bindings () > 0;
}

size_t
TypeBoundPredicate::get_num_associated_bindings () const
{
  size_t count = 0;
  auto trait_ref = get ();
  for (const auto &trait_item : trait_ref->get_trait_items ())
    {
      bool is_associated_type
	= trait_item.get_trait_item_type ()
	  == Resolver::TraitItemReference::TraitItemType::TYPE;
      if (is_associated_type)
	count++;
    }
  return count;
}

TypeBoundPredicateItem
TypeBoundPredicate::lookup_associated_type (const std::string &search)
{
  TypeBoundPredicateItem item = lookup_associated_item (search);

  // only need to check that it is infact an associated type because other
  // wise if it was not found it will just be an error node anyway
  if (!item.is_error ())
    {
      const auto raw = item.get_raw_item ();
      if (raw->get_trait_item_type ()
	  != Resolver::TraitItemReference::TraitItemType::TYPE)
	return TypeBoundPredicateItem::error ();
    }
  return item;
}

std::vector<TypeBoundPredicateItem>
TypeBoundPredicate::get_associated_type_items ()
{
  std::vector<TypeBoundPredicateItem> items;
  auto trait_ref = get ();
  for (const auto &trait_item : trait_ref->get_trait_items ())
    {
      bool is_associated_type
	= trait_item.get_trait_item_type ()
	  == Resolver::TraitItemReference::TraitItemType::TYPE;
      if (is_associated_type)
	{
	  TypeBoundPredicateItem item (*this, &trait_item);
	  items.push_back (std::move (item));
	}
    }
  return items;
}

bool
TypeBoundPredicate::is_equal (const TypeBoundPredicate &other) const
{
  // check they match the same trait reference
  if (reference != other.reference)
    return false;

  // check that the generics match
  if (get_num_substitutions () != other.get_num_substitutions ())
    return false;

  // then match the generics applied
  for (size_t i = 0; i < get_num_substitutions (); i++)
    {
      const SubstitutionParamMapping &a = substitutions.at (i);
      const SubstitutionParamMapping &b = other.substitutions.at (i);

      const ParamType *ap = a.get_param_ty ();
      const ParamType *bp = b.get_param_ty ();

      const BaseType *apd = ap->destructure ();
      const BaseType *bpd = bp->destructure ();

      // FIXME use the unify_and infer inteface or try coerce
      if (!apd->can_eq (bpd, false /*emit_errors*/))
	{
	  if (!bpd->can_eq (apd, false /*emit_errors*/))
	    return false;
	}
    }

  return true;
}

bool
TypeBoundPredicate::validate_type_implements_super_traits (
  TyTy::BaseType &self, HIR::Type &impl_type, HIR::Type &trait) const
{
  if (get_polarity () != BoundPolarity::RegularBound)
    return true;

  auto &ptref = *get ();
  for (auto &super : super_traits)
    {
      if (super.get_polarity () != BoundPolarity::RegularBound)
	continue;

      if (!super.validate_type_implements_this (self, impl_type, trait))
	{
	  auto &sptref = *super.get ();

	  // emit error
	  std::string fixit1
	    = "required by this bound in: " + ptref.get_name ();
	  std::string fixit2 = "the trait " + sptref.get_name ()
			       + " is not implemented for "
			       + impl_type.as_string ();

	  rich_location r (line_table, trait.get_locus ());
	  r.add_fixit_insert_after (super.get_locus (), fixit1.c_str ());
	  r.add_fixit_insert_after (trait.get_locus (), fixit2.c_str ());
	  rust_error_at (r, ErrorCode::E0277,
			 "the trait bound %<%s: %s%> is not satisfied",
			 impl_type.as_string ().c_str (),
			 sptref.get_name ().c_str ());

	  return false;
	}

      if (!super.validate_type_implements_super_traits (self, impl_type, trait))
	return false;
    }

  return true;
}

bool
TypeBoundPredicate::validate_type_implements_this (TyTy::BaseType &self,
						   HIR::Type &impl_type,
						   HIR::Type &trait) const
{
  const auto &ptref = *get ();
  auto probed_bounds = Resolver::TypeBoundsProbe::Probe (&self);
  for (auto &elem : probed_bounds)
    {
      auto &tref = *(elem.first);
      if (ptref.is_equal (tref))
	return true;
    }

  return false;
}

// trait item reference

const Resolver::TraitItemReference *
TypeBoundPredicateItem::get_raw_item () const
{
  return trait_item_ref;
}

bool
TypeBoundPredicateItem::needs_implementation () const
{
  return !get_raw_item ()->is_optional ();
}

location_t
TypeBoundPredicateItem::get_locus () const
{
  return get_raw_item ()->get_locus ();
}

// TypeBoundsMappings

TypeBoundsMappings::TypeBoundsMappings (
  std::vector<TypeBoundPredicate> specified_bounds)
  : specified_bounds (specified_bounds)
{}

std::vector<TypeBoundPredicate> &
TypeBoundsMappings::get_specified_bounds ()
{
  return specified_bounds;
}

const std::vector<TypeBoundPredicate> &
TypeBoundsMappings::get_specified_bounds () const
{
  return specified_bounds;
}

TypeBoundPredicate
TypeBoundsMappings::lookup_predicate (DefId id)
{
  for (auto &b : specified_bounds)
    {
      if (b.get_id () == id)
	return b;
    }
  return TypeBoundPredicate::error ();
}

size_t
TypeBoundsMappings::num_specified_bounds () const
{
  return specified_bounds.size ();
}

std::string
TypeBoundsMappings::raw_bounds_as_string () const
{
  std::string buf;
  for (size_t i = 0; i < specified_bounds.size (); i++)
    {
      const TypeBoundPredicate &b = specified_bounds.at (i);
      bool has_next = (i + 1) < specified_bounds.size ();
      buf += b.as_string () + (has_next ? " + " : "");
    }
  return buf;
}

std::string
TypeBoundsMappings::bounds_as_string () const
{
  return "bounds:[" + raw_bounds_as_string () + "]";
}

std::string
TypeBoundsMappings::raw_bounds_as_name () const
{
  std::string buf;
  for (size_t i = 0; i < specified_bounds.size (); i++)
    {
      const TypeBoundPredicate &b = specified_bounds.at (i);
      bool has_next = (i + 1) < specified_bounds.size ();
      buf += b.as_name () + (has_next ? " + " : "");
    }

  return buf;
}

void
TypeBoundsMappings::add_bound (TypeBoundPredicate predicate)
{
  for (auto &bound : specified_bounds)
    {
      bool same_trait_ref_p = bound.get_id () == predicate.get_id ();
      if (same_trait_ref_p)
	return;
    }

  specified_bounds.push_back (predicate);
}

} // namespace TyTy
} // namespace Rust
