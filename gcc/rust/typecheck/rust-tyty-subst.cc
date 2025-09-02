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

#include "rust-tyty-subst.h"

#include "rust-hir-generic-param.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-hir-type-check.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-compile-base.h"
#include "rust-type-util.h"
#include "tree.h"

namespace Rust {
namespace TyTy {

SubstitutionParamMapping::SubstitutionParamMapping (HIR::GenericParam &generic,
						    BaseGeneric *param)
  : generic (generic), param (param)
{}

SubstitutionParamMapping::SubstitutionParamMapping (
  const SubstitutionParamMapping &other)
  : generic (other.generic), param (other.param)
{}

std::string
SubstitutionParamMapping::as_string () const
{
  if (param == nullptr)
    return "nullptr";

  return param->get_name ();
}

SubstitutionParamMapping
SubstitutionParamMapping::clone () const
{
  return SubstitutionParamMapping (generic,
				   static_cast<ParamType *> (param->clone ()));
}

BaseGeneric *
SubstitutionParamMapping::get_param_ty ()
{
  return param;
}

const BaseGeneric *
SubstitutionParamMapping::get_param_ty () const
{
  return param;
}

HIR::GenericParam &
SubstitutionParamMapping::get_generic_param ()
{
  return generic;
}

const HIR::GenericParam &
SubstitutionParamMapping::get_generic_param () const
{
  return generic;
}

bool
SubstitutionParamMapping::needs_substitution () const
{
  return !(get_param_ty ()->is_concrete ());
}

Identifier
SubstitutionParamMapping::get_type_representation () const
{
  return param->get_symbol ();
}

location_t
SubstitutionParamMapping::get_param_locus () const
{
  return generic.get_locus ();
}

bool
SubstitutionParamMapping::param_has_default_ty () const
{
  if (generic.get_kind () == HIR::GenericParam::GenericKind::TYPE)
    {
      const auto &type_param = static_cast<const HIR::TypeParam &> (generic);
      return type_param.has_type ();
    }

  rust_assert (generic.get_kind () == HIR::GenericParam::GenericKind::CONST);
  const auto &const_param
    = static_cast<const HIR::ConstGenericParam &> (generic);
  return const_param.has_default_expression ();
}

BaseType *
SubstitutionParamMapping::get_default_ty () const
{
  if (generic.get_kind () == HIR::GenericParam::GenericKind::TYPE)
    {
      const auto &type_param = static_cast<const HIR::TypeParam &> (generic);
      TyVar var (type_param.get_type_mappings ().get_hirid ());
      return var.get_tyty ();
    }

  rust_assert (generic.get_kind () == HIR::GenericParam::GenericKind::CONST);
  const auto &const_param
    = static_cast<const HIR::ConstGenericParam &> (generic);
  rust_assert (const_param.has_default_expression ());

  const auto &expr = const_param.get_default_expression ();
  TyVar var (expr.get_mappings ().get_hirid ());
  return var.get_tyty ();
}

bool
SubstitutionParamMapping::need_substitution () const
{
  if (!param->can_resolve ())
    return true;

  auto resolved = param->resolve ();
  return !resolved->is_concrete ();
}

bool
SubstitutionParamMapping::fill_param_ty (
  SubstitutionArgumentMappings &subst_mappings, location_t locus,
  bool needs_bounds_check)
{
  SubstitutionArg arg = SubstitutionArg::error ();
  bool ok = subst_mappings.get_argument_for_symbol (get_param_ty (), &arg);
  if (!ok)
    return true;

  TyTy::BaseType &type = *arg.get_tyty ();
  if (type.get_kind () == TyTy::TypeKind::INFER)
    {
      type.inherit_bounds (*param);
    }

  if (type.get_kind () == TypeKind::PARAM)
    {
      param = static_cast<BaseGeneric *> (type.clone ());
    }
  else if (type.get_kind () == TyTy::TypeKind::CONST)
    {
      param = static_cast<BaseGeneric *> (type.clone ());
    }
  else if (param->get_kind () == TypeKind::PARAM)
    {
      auto &p = *static_cast<TyTy::ParamType *> (param);

      // check the substitution is compatible with bounds
      rust_debug_loc (locus,
		      "fill_param_ty bounds_compatible: param %s type %s",
		      param->get_name ().c_str (), type.get_name ().c_str ());
      if (needs_bounds_check && !p.is_implicit_self_trait ())
	{
	  if (!param->bounds_compatible (type, locus, true))
	    return false;
	}

      // recursively pass this down to all HRTB's
      for (auto &bound : param->get_specified_bounds ())
	bound.handle_substitions (subst_mappings);

      param->set_ty_ref (type.get_ref ());
      subst_mappings.on_param_subst (p, arg);
    }

  return true;
}

void
SubstitutionParamMapping::override_context ()
{
  if (!param->can_resolve ())
    return;

  auto &mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  context->insert_type (Analysis::NodeMapping (mappings.get_current_crate (),
					       UNKNOWN_NODEID,
					       param->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			param->resolve ());
}

SubstitutionArg::SubstitutionArg (const SubstitutionParamMapping *param,
				  BaseType *argument)
  : param (param), argument (argument)
{
  if (param != nullptr)
    original_param = param->get_param_ty ();
}

SubstitutionArg::SubstitutionArg (const SubstitutionArg &other)
  : param (other.param), original_param (other.original_param),
    argument (other.argument)
{}

SubstitutionArg &
SubstitutionArg::operator= (const SubstitutionArg &other)
{
  param = other.param;
  argument = other.argument;
  original_param = other.original_param;

  return *this;
}

BaseType *
SubstitutionArg::get_tyty () const
{
  return argument;
}

const SubstitutionParamMapping *
SubstitutionArg::get_param_mapping () const
{
  return param;
}

const BaseGeneric *
SubstitutionArg::get_param_ty () const
{
  return original_param;
}

SubstitutionArg
SubstitutionArg::error ()
{
  return SubstitutionArg (nullptr, nullptr);
}

bool
SubstitutionArg::is_error () const
{
  return param == nullptr || argument == nullptr;
}

bool
SubstitutionArg::is_conrete () const
{
  if (argument == nullptr)
    return false;

  if (argument->get_kind () == TyTy::TypeKind::PARAM)
    return false;

  return argument->is_concrete ();
}

std::string
SubstitutionArg::as_string () const
{
  return original_param->as_string ()
	 + (argument != nullptr ? ":" + argument->as_string () : "");
}

const RegionParamList &
SubstitutionArgumentMappings::get_regions () const
{
  return regions;
}

RegionParamList &
SubstitutionArgumentMappings::get_mut_regions ()
{
  return regions;
}

// SubstitutionArgumentMappings

SubstitutionArgumentMappings::SubstitutionArgumentMappings (
  std::vector<SubstitutionArg> mappings,
  std::map<std::string, BaseType *> binding_args, RegionParamList regions,
  location_t locus, ParamSubstCb param_subst_cb, bool trait_item_flag,
  bool error_flag)
  : mappings (std::move (mappings)), binding_args (binding_args),
    regions (regions), locus (locus), param_subst_cb (param_subst_cb),
    trait_item_flag (trait_item_flag), error_flag (error_flag)
{}

SubstitutionArgumentMappings::SubstitutionArgumentMappings (
  const SubstitutionArgumentMappings &other)
  : mappings (other.mappings), binding_args (other.binding_args),
    regions (other.regions), locus (other.locus), param_subst_cb (nullptr),
    trait_item_flag (other.trait_item_flag), error_flag (other.error_flag)
{}

SubstitutionArgumentMappings &
SubstitutionArgumentMappings::operator= (
  const SubstitutionArgumentMappings &other)
{
  mappings = other.mappings;
  binding_args = other.binding_args;
  regions = other.regions;
  locus = other.locus;
  param_subst_cb = nullptr;
  trait_item_flag = other.trait_item_flag;
  error_flag = other.error_flag;

  return *this;
}

SubstitutionArgumentMappings
SubstitutionArgumentMappings::error ()
{
  return SubstitutionArgumentMappings ({}, {}, 0, UNDEF_LOCATION, nullptr,
				       false, true);
}

SubstitutionArgumentMappings
SubstitutionArgumentMappings::empty (size_t num_regions)
{
  return SubstitutionArgumentMappings ({}, {}, num_regions, UNDEF_LOCATION,
				       nullptr, false, false);
}

bool
SubstitutionArgumentMappings::is_error () const
{
  return error_flag;
}

bool
SubstitutionArgumentMappings::get_argument_for_symbol (
  const BaseGeneric *param_to_find, SubstitutionArg *argument) const
{
  for (const auto &mapping : mappings)
    {
      const auto *p = mapping.get_param_ty ();
      if (p->get_symbol () == param_to_find->get_symbol ())
	{
	  *argument = mapping;
	  return true;
	}
    }
  return false;
}
tl::optional<size_t>
SubstitutionArgumentMappings::find_symbol (const ParamType &param_to_find) const
{
  auto it = std::find_if (mappings.begin (), mappings.end (),
			  [param_to_find] (const SubstitutionArg &arg) {
			    return arg.get_param_ty ()->get_symbol ()
				   == param_to_find.get_symbol ();
			  });
  if (it == mappings.end ())
    return tl::nullopt;
  return std::distance (mappings.begin (), it);
}

bool
SubstitutionArgumentMappings::get_argument_at (size_t index,
					       SubstitutionArg *argument)
{
  if (index > mappings.size ())
    return false;

  *argument = mappings.at (index);
  return true;
}

bool
SubstitutionArgumentMappings::is_concrete () const
{
  for (auto &mapping : mappings)
    {
      if (!mapping.is_conrete ())
	return false;
    }
  return true;
}

location_t
SubstitutionArgumentMappings::get_locus () const
{
  return locus;
}

size_t
SubstitutionArgumentMappings::size () const
{
  return mappings.size ();
}

bool
SubstitutionArgumentMappings::is_empty () const
{
  return size () == 0;
}

std::vector<SubstitutionArg> &
SubstitutionArgumentMappings::get_mappings ()
{
  return mappings;
}

const std::vector<SubstitutionArg> &
SubstitutionArgumentMappings::get_mappings () const
{
  return mappings;
}

std::map<std::string, BaseType *> &
SubstitutionArgumentMappings::get_binding_args ()
{
  return binding_args;
}

const std::map<std::string, BaseType *> &
SubstitutionArgumentMappings::get_binding_args () const
{
  return binding_args;
}

std::string
SubstitutionArgumentMappings::as_string () const
{
  std::string buffer;
  for (auto &mapping : mappings)
    {
      buffer += mapping.as_string () + ", ";
    }
  return "<" + buffer + ">";
}

void
SubstitutionArgumentMappings::on_param_subst (const ParamType &p,
					      const SubstitutionArg &a) const
{
  if (param_subst_cb == nullptr)
    return;

  param_subst_cb (p, a);
}

ParamSubstCb
SubstitutionArgumentMappings::get_subst_cb () const
{
  return param_subst_cb;
}

bool
SubstitutionArgumentMappings::trait_item_mode () const
{
  return trait_item_flag;
}

// SubstitutionRef

SubstitutionRef::SubstitutionRef (
  std::vector<SubstitutionParamMapping> substitutions,
  SubstitutionArgumentMappings arguments, RegionConstraints region_constraints)
  : substitutions (substitutions), used_arguments (arguments),
    region_constraints (region_constraints)
{}

bool
SubstitutionRef::has_substitutions () const
{
  return substitutions.size () > 0;
}

std::string
SubstitutionRef::subst_as_string () const
{
  std::string buffer;
  for (size_t i = 0; i < substitutions.size (); i++)
    {
      const SubstitutionParamMapping &sub = substitutions.at (i);
      buffer += sub.as_string ();

      if ((i + 1) < substitutions.size ())
	buffer += ", ";
    }

  return buffer.empty () ? "" : "<" + buffer + ">";
}

bool
SubstitutionRef::supports_associated_bindings () const
{
  return get_num_associated_bindings () > 0;
}

size_t
SubstitutionRef::get_num_associated_bindings () const
{
  return 0;
}

TypeBoundPredicateItem
SubstitutionRef::lookup_associated_type (const std::string &search)
{
  return TypeBoundPredicateItem::error ();
}

size_t
SubstitutionRef::get_num_substitutions () const
{
  return substitutions.size ();
}
size_t
SubstitutionRef::get_num_lifetime_params () const
{
  return used_arguments.get_regions ().size ();
}
size_t
SubstitutionRef::get_num_type_params () const
{
  return get_num_substitutions ();
}

std::vector<SubstitutionParamMapping> &
SubstitutionRef::get_substs ()
{
  return substitutions;
}

const std::vector<SubstitutionParamMapping> &
SubstitutionRef::get_substs () const
{
  return substitutions;
}

std::vector<SubstitutionParamMapping>
SubstitutionRef::clone_substs () const
{
  std::vector<SubstitutionParamMapping> clone;

  for (auto &sub : substitutions)
    clone.push_back (sub.clone ());

  return clone;
}

void
SubstitutionRef::override_context ()
{
  for (auto &sub : substitutions)
    {
      sub.override_context ();
    }
}

bool
SubstitutionRef::needs_substitution () const
{
  return std::any_of (substitutions.begin (), substitutions.end (),
		      std::mem_fn (
			&SubstitutionParamMapping::needs_substitution));
}

bool
SubstitutionRef::was_substituted () const
{
  return !needs_substitution ();
}

SubstitutionArgumentMappings &
SubstitutionRef::get_substitution_arguments ()
{
  return used_arguments;
}

const SubstitutionArgumentMappings &
SubstitutionRef::get_substitution_arguments () const
{
  return used_arguments;
}

size_t
SubstitutionRef::num_required_substitutions () const
{
  size_t n = 0;
  for (auto &p : substitutions)
    {
      if (p.needs_substitution ())
	n++;
    }
  return n;
}

size_t
SubstitutionRef::min_required_substitutions () const
{
  size_t n = 0;
  for (auto &p : substitutions)
    {
      if (p.needs_substitution () && !p.param_has_default_ty ())
	n++;
    }
  return n;
}

const SubstitutionArgumentMappings &
SubstitutionRef::get_used_arguments () const
{
  return used_arguments;
}

tl::optional<SubstitutionArg>
SubstitutionRef::get_arg_at (size_t i) const
{
  auto param_ty = get_substs ().at (i).get_param_ty ();
  SubstitutionArg arg = SubstitutionArg::error ();
  get_used_arguments ().get_argument_for_symbol (param_ty, &arg);
  if (arg.is_error ())
    return tl::nullopt;
  return arg;
}

const RegionConstraints &
SubstitutionRef::get_region_constraints () const
{
  return region_constraints;
}

SubstitutionArgumentMappings
SubstitutionRef::get_mappings_from_generic_args (
  HIR::GenericArgs &args, const std::vector<Region> &regions)
{
  std::map<std::string, BaseType *> binding_arguments;
  if (args.get_binding_args ().size () > 0)
    {
      if (supports_associated_bindings ())
	{
	  if (args.get_binding_args ().size () > get_num_associated_bindings ())
	    {
	      rich_location r (line_table, args.get_locus ());
	      rust_error_at (r,
			     "generic item takes at most %lu type binding "
			     "arguments but %lu were supplied",
			     (unsigned long) get_num_associated_bindings (),
			     (unsigned long) args.get_binding_args ().size ());
	      return SubstitutionArgumentMappings::error ();
	    }

	  for (auto &binding : args.get_binding_args ())
	    {
	      BaseType *resolved
		= Resolver::TypeCheckType::Resolve (binding.get_type ());
	      if (resolved == nullptr
		  || resolved->get_kind () == TyTy::TypeKind::ERROR)
		{
		  return SubstitutionArgumentMappings::error ();
		}

	      // resolve to relevant binding
	      auto binding_item = lookup_associated_type (
		binding.get_identifier ().as_string ());
	      if (binding_item.is_error ())
		{
		  rust_error_at (
		    binding.get_locus (), "unknown associated type binding: %s",
		    binding.get_identifier ().as_string ().c_str ());
		  return SubstitutionArgumentMappings::error ();
		}

	      binding_arguments[binding.get_identifier ().as_string ()]
		= resolved;
	    }
	}
      else
	{
	  rich_location r (line_table, args.get_locus ());
	  for (auto &binding : args.get_binding_args ())
	    r.add_range (binding.get_locus ());

	  rust_error_at (r, ErrorCode::E0229,
			 "associated type bindings are not allowed here");
	  return SubstitutionArgumentMappings::error ();
	}
    }

  // for inherited arguments
  size_t offs = used_arguments.size ();
  size_t total_arguments
    = args.get_type_args ().size () + args.get_const_args ().size () + offs;
  if (total_arguments > substitutions.size ())
    {
      rich_location r (line_table, args.get_locus ());
      if (!substitutions.empty ())
	{
	  const auto &subst = substitutions.front ();
	  const auto &generic = subst.get_generic_param ();
	  r.add_range (generic.get_locus ());
	}

      rust_error_at (
	r,
	"generic item takes at most %lu type arguments but %lu were supplied",
	(unsigned long) substitutions.size (),
	(unsigned long) args.get_type_args ().size ());
      return SubstitutionArgumentMappings::error ();
    }

  if (total_arguments < min_required_substitutions ())
    {
      rich_location r (line_table, args.get_locus ());
      if (!substitutions.empty ())
	{
	  const auto &subst = substitutions.front ();
	  const auto &generic = subst.get_generic_param ();
	  r.add_range (generic.get_locus ());
	}

      rust_error_at (
	r, ErrorCode::E0107,
	"generic item takes at least %lu type arguments but %lu were supplied",
	(unsigned long) (min_required_substitutions () - offs),
	(unsigned long) args.get_type_args ().size ());
      return SubstitutionArgumentMappings::error ();
    }

  std::vector<SubstitutionArg> mappings = used_arguments.get_mappings ();
  for (auto &arg : args.get_type_args ())
    {
      BaseType *resolved = Resolver::TypeCheckType::Resolve (*arg);
      if (resolved == nullptr || resolved->get_kind () == TyTy::TypeKind::ERROR)
	{
	  return SubstitutionArgumentMappings::error ();
	}

      const auto &param_mapping = substitutions.at (offs);
      const auto &generic = param_mapping.get_generic_param ();
      if (generic.get_kind () == HIR::GenericParam::GenericKind::TYPE)
	{
	  const auto &type_param
	    = static_cast<const HIR::TypeParam &> (generic);
	  if (type_param.from_impl_trait ())
	    {
	      rich_location r (line_table, arg->get_locus ());
	      r.add_fixit_remove (arg->get_locus ());
	      rust_error_at (r, ErrorCode::E0632,
			     "cannot provide explicit generic arguments when "
			     "%<impl Trait%> is used in argument position");
	      return SubstitutionArgumentMappings::error ();
	    }
	}
      else if (generic.get_kind () == HIR::GenericParam::GenericKind::CONST)
	{
	  if (!resolved->is<ConstType> ())
	    {
	      rich_location r (line_table, arg->get_locus ());
	      r.add_fixit_remove (arg->get_locus ());
	      rust_error_at (r, ErrorCode::E0747,
			     "type provided when a constant was expected");
	      return SubstitutionArgumentMappings::error ();
	    }
	}

      SubstitutionArg subst_arg (&param_mapping, resolved);
      offs++;
      mappings.push_back (std::move (subst_arg));
    }

  for (auto &arg : args.get_const_args ())
    {
      auto &expr = *arg.get_expression ().get ();
      BaseType *expr_type = Resolver::TypeCheckExpr::Resolve (expr);
      if (expr_type == nullptr || expr_type->is<ErrorType> ())
	return SubstitutionArgumentMappings::error ();

      // validate this param is really a const generic
      const auto &param_mapping = substitutions.at (offs);
      const auto &generic = param_mapping.get_generic_param ();
      if (generic.get_kind () != HIR::GenericParam::GenericKind::CONST)
	{
	  rich_location r (line_table, arg.get_locus ());
	  r.add_fixit_remove (expr.get_locus ());
	  rust_error_at (r, "invalid position for a const generic argument");
	  return SubstitutionArgumentMappings::error ();
	}

      // get the const generic specified type
      const auto base_generic = param_mapping.get_param_ty ();
      rust_assert (base_generic->is<ConstType> ());
      const auto const_param
	= static_cast<const TyTy::ConstType *> (base_generic);
      auto specified_type = const_param->get_ty ();

      // validate this const generic is of the correct type
      auto coereced_type
	= Resolver::coercion_site (expr.get_mappings ().get_hirid (),
				   TyTy::TyWithLocation (specified_type),
				   TyTy::TyWithLocation (expr_type,
							 expr.get_locus ()),
				   arg.get_locus ());
      if (coereced_type->is<ErrorType> ())
	return SubstitutionArgumentMappings::error ();

      // const fold it
      auto ctx = Compile::Context::get ();
      tree folded
	= Compile::HIRCompileBase::query_compile_const_expr (ctx, coereced_type,
							     expr);

      if (folded == error_mark_node)
	return SubstitutionArgumentMappings::error ();

      // create const type
      auto const_value
	= new TyTy::ConstType (TyTy::ConstType::ConstKind::Value, "",
			       coereced_type, folded, {}, expr.get_locus (),
			       expr.get_mappings ().get_hirid (),
			       expr.get_mappings ().get_hirid (), {});

      SubstitutionArg subst_arg (&param_mapping, const_value);
      offs++;
      mappings.push_back (std::move (subst_arg));
    }

  // we must need to fill out defaults
  size_t left_over
    = num_required_substitutions () - min_required_substitutions ();
  if (left_over > 0)
    {
      for (size_t offs = mappings.size (); offs < substitutions.size (); offs++)
	{
	  SubstitutionParamMapping &param = substitutions.at (offs);
	  rust_assert (param.param_has_default_ty ());

	  BaseType *resolved = param.get_default_ty ();
	  if (resolved->get_kind () == TypeKind::ERROR)
	    return SubstitutionArgumentMappings::error ();

	  // this resolved default might already contain default parameters
	  if (!resolved->is_concrete ())
	    {
	      SubstitutionArgumentMappings intermediate (
		mappings, binding_arguments,
		{used_arguments.get_regions ().size ()}, args.get_locus ());
	      resolved = Resolver::SubstMapperInternal::Resolve (resolved,
								 intermediate);

	      if (resolved->get_kind () == TypeKind::ERROR)
		return SubstitutionArgumentMappings::error ();
	    }

	  SubstitutionArg subst_arg (&param, resolved);
	  mappings.push_back (std::move (subst_arg));
	}
    }

  return {mappings, binding_arguments,
	  RegionParamList::from_subst (used_arguments.get_regions ().size (),
				       regions),
	  args.get_locus ()};
}

BaseType *
SubstitutionRef::infer_substitions (location_t locus)
{
  std::vector<SubstitutionArg> args;
  std::map<std::string, BaseType *> argument_mappings;
  for (auto &p : get_substs ())
    {
      if (p.needs_substitution ())
	{
	  const HIR::GenericParam &generic = p.get_generic_param ();
	  const std::string &symbol = p.get_param_ty ()->get_symbol ();
	  auto it = argument_mappings.find (symbol);
	  bool have_mapping = it != argument_mappings.end ();

	  if (have_mapping)
	    {
	      args.push_back (SubstitutionArg (&p, it->second));
	    }
	  else if (generic.get_kind () == HIR::GenericParam::GenericKind::TYPE)
	    {
	      TyVar infer_var = TyVar::get_implicit_infer_var (locus);
	      args.push_back (SubstitutionArg (&p, infer_var.get_tyty ()));
	      argument_mappings[symbol] = infer_var.get_tyty ();
	    }
	  else if (generic.get_kind () == HIR::GenericParam::GenericKind::CONST)
	    {
	      const auto const_param = p.get_param_ty ();
	      rust_assert (const_param->is<TyTy::ConstType> ());
	      const auto &const_type
		= *static_cast<const TyTy::ConstType *> (const_param);

	      TyVar infer_var
		= TyVar::get_implicit_const_infer_var (const_type, locus);
	      args.push_back (SubstitutionArg (&p, infer_var.get_tyty ()));
	      argument_mappings[symbol] = infer_var.get_tyty ();
	    }
	}
      else
	{
	  args.push_back (SubstitutionArg (&p, p.get_param_ty ()->resolve ()));
	}
    }

  // FIXME do we need to add inference variables to all the possible bindings?
  // it might just lead to inference variable hell not 100% sure if rustc does
  // this i think the language might needs this to be explicitly set

  SubstitutionArgumentMappings infer_arguments (std::move (args),
						{} /* binding_arguments */,
						used_arguments.get_regions (),
						locus);
  return handle_substitions (infer_arguments);
}

SubstitutionArgumentMappings
SubstitutionRef::adjust_mappings_for_this (
  SubstitutionArgumentMappings &mappings, bool trait_mode)
{
  std::vector<SubstitutionArg> resolved_mappings;
  for (size_t i = 0; i < substitutions.size (); i++)
    {
      auto &subst = substitutions.at (i);

      SubstitutionArg arg = SubstitutionArg::error ();
      if (mappings.size () == substitutions.size ())
	{
	  mappings.get_argument_at (i, &arg);
	}
      else
	{
	  if (subst.needs_substitution ())
	    {
	      // get from passed in mappings
	      mappings.get_argument_for_symbol (subst.get_param_ty (), &arg);
	    }
	  else
	    {
	      // we should already have this somewhere
	      used_arguments.get_argument_for_symbol (subst.get_param_ty (),
						      &arg);
	    }
	}

      bool ok = !arg.is_error ();
      if (ok || (trait_mode && i == 0))
	{
	  SubstitutionArg adjusted (&subst, arg.get_tyty ());
	  resolved_mappings.push_back (std::move (adjusted));
	}
    }

  if (resolved_mappings.empty ())
    return SubstitutionArgumentMappings::error ();

  return SubstitutionArgumentMappings (resolved_mappings,
				       mappings.get_binding_args (),
				       mappings.get_regions (),
				       mappings.get_locus (),
				       mappings.get_subst_cb (),
				       mappings.trait_item_mode ());
}

bool
SubstitutionRef::are_mappings_bound (SubstitutionArgumentMappings &mappings)
{
  std::vector<SubstitutionArg> resolved_mappings;
  for (size_t i = 0; i < substitutions.size (); i++)
    {
      auto &subst = substitutions.at (i);

      SubstitutionArg arg = SubstitutionArg::error ();
      if (mappings.size () == substitutions.size ())
	{
	  mappings.get_argument_at (i, &arg);
	}
      else
	{
	  if (subst.needs_substitution ())
	    {
	      // get from passed in mappings
	      mappings.get_argument_for_symbol (subst.get_param_ty (), &arg);
	    }
	  else
	    {
	      // we should already have this somewhere
	      used_arguments.get_argument_for_symbol (subst.get_param_ty (),
						      &arg);
	    }
	}

      bool ok = !arg.is_error ();
      if (ok)
	{
	  SubstitutionArg adjusted (&subst, arg.get_tyty ());
	  resolved_mappings.push_back (std::move (adjusted));
	}
    }

  return !resolved_mappings.empty ();
}

// this function assumes that the mappings being passed are for the same type as
// this new substitution reference so ordering matters here
SubstitutionArgumentMappings
SubstitutionRef::solve_mappings_from_receiver_for_self (
  SubstitutionArgumentMappings &mappings) const
{
  std::vector<SubstitutionArg> resolved_mappings;

  rust_assert (mappings.size () == get_num_substitutions ());
  for (size_t i = 0; i < get_num_substitutions (); i++)
    {
      const SubstitutionParamMapping &param_mapping = substitutions.at (i);
      SubstitutionArg &arg = mappings.get_mappings ().at (i);

      if (param_mapping.needs_substitution ())
	{
	  SubstitutionArg adjusted (&param_mapping, arg.get_tyty ());
	  resolved_mappings.push_back (std::move (adjusted));
	}
    }

  return SubstitutionArgumentMappings (resolved_mappings,
				       mappings.get_binding_args (),
				       mappings.get_regions (),
				       mappings.get_locus ());
}

void
SubstitutionRef::prepare_higher_ranked_bounds ()
{
  for (const auto &subst : get_substs ())
    {
      const auto pty = subst.get_param_ty ();
      for (const auto &bound : pty->get_specified_bounds ())
	{
	  const auto ref = bound.get ();
	  ref->clear_associated_type_projections ();
	}
    }
}

bool
SubstitutionRef::monomorphize ()
{
  for (const auto &subst : get_substs ())
    {
      const auto pty = subst.get_param_ty ();
      if (!pty->can_resolve ())
	continue;

      const TyTy::BaseType *binding = pty->resolve ();
      if (binding->get_kind () == TyTy::TypeKind::PARAM)
	continue;

      for (const auto &bound : pty->get_specified_bounds ())
	{
	  bool ambigious = false;
	  auto associated
	    = Resolver::lookup_associated_impl_block (bound, binding,
						      &ambigious);
	  if (associated != nullptr)
	    associated->setup_associated_types (binding, bound);
	}
    }

  return true;
}

} // namespace TyTy
} // namespace Rust
