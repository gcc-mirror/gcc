#include "rust-tyty-variance-analysis-private.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

BaseType *
lookup_type (HirId ref)
{
  BaseType *ty = nullptr;
  bool ok = Resolver::TypeCheckContext::get ()->lookup_type (ref, &ty);
  rust_assert (ok);
  return ty;
}

namespace VarianceAnalysis {

CrateCtx::CrateCtx () : private_ctx (new GenericTyPerCrateCtx ()) {}

// Must be here because of incomplete type.
CrateCtx::~CrateCtx () = default;

void
CrateCtx::add_type_constraints (ADTType &type)
{
  private_ctx->process_type (type);
}

void
CrateCtx::solve ()
{
  private_ctx->solve ();
  private_ctx->debug_print_solutions ();
}

std::vector<Variance>
CrateCtx::query_generic_variance (const ADTType &type)
{
  return private_ctx->query_generic_variance (type);
}

std::vector<Variance>
CrateCtx::query_type_variances (BaseType *type)
{
  TyVisitorCtx ctx (*private_ctx);
  return ctx.collect_variances (*type);
}

std::vector<Region>
CrateCtx::query_type_regions (BaseType *type)
{
  return private_ctx->query_type_regions (type);
}

FreeRegions
CrateCtx::query_field_regions (const ADTType *parent, size_t variant_index,
			       size_t field_index,
			       const FreeRegions &parent_regions)
{
  return private_ctx->query_field_regions (parent, variant_index, field_index,
					   parent_regions);
}

Variance
Variance::reverse () const
{
  switch (kind)
    {
    case BIVARIANT:
      return bivariant ();
    case COVARIANT:
      return contravariant ();
    case CONTRAVARIANT:
      return covariant ();
    case INVARIANT:
      return invariant ();
    }

  rust_unreachable ();
}

Variance
Variance::join (Variance lhs, Variance rhs)
{
  return {Kind (lhs.kind | rhs.kind)};
}

void
Variance::join (Variance rhs)
{
  *this = join (*this, rhs);
}

Variance
Variance::transform (Variance lhs, Variance rhs)
{
  switch (lhs.kind)
    {
    case BIVARIANT:
      return bivariant ();
    case COVARIANT:
      return rhs;
    case CONTRAVARIANT:
      return rhs.reverse ();
    case INVARIANT:
      return invariant ();
    }
  rust_unreachable ();
}

std::string
Variance::as_string () const
{
  switch (kind)
    {
    case BIVARIANT:
      return "o";
    case COVARIANT:
      return "+";
    case CONTRAVARIANT:
      return "-";
    case INVARIANT:
      return "*";
    }
  rust_unreachable ();
}

void
GenericTyPerCrateCtx::process_type (ADTType &type)
{
  GenericTyVisitorCtx (*this).process_type (type);
}

void
GenericTyPerCrateCtx::solve ()
{
  rust_debug ("Variance analysis solving started:");

  // Fix point iteration
  bool changed = true;
  while (changed)
    {
      changed = false;
      for (auto constraint : constraints)
	{
	  rust_debug ("\tapplying constraint: %s <= %s",
		      to_string (constraint.target_index).c_str (),
		      to_string (*constraint.term).c_str ());

	  auto old_solution = solutions[constraint.target_index];
	  auto new_solution
	    = Variance::join (old_solution, evaluate (constraint.term));

	  if (old_solution != new_solution)
	    {
	      rust_debug ("\t\tsolution changed: %s => %s",
			  old_solution.as_string ().c_str (),
			  new_solution.as_string ().c_str ());

	      changed = true;
	      solutions[constraint.target_index] = new_solution;
	    }
	}
    }

  constraints.clear ();
  constraints.shrink_to_fit ();
}

void
GenericTyPerCrateCtx::debug_print_solutions ()
{
  rust_debug ("Variance analysis results:");

  for (auto type : map_from_ty_orig_ref)
    {
      auto solution_index = type.second;
      auto ref = type.first;

      BaseType *ty = lookup_type (ref);

      std::string result = "\t";

      if (auto adt = ty->try_as<ADTType> ())
	{
	  result += adt->get_identifier ();
	  result += "<";

	  size_t i = solution_index;
	  for (auto &region : adt->get_used_arguments ().get_regions ())
	    {
	      (void) region;
	      if (i > solution_index)
		result += ", ";
	      result += solutions[i].as_string ();
	      i++;
	    }
	  for (auto &param : adt->get_substs ())
	    {
	      if (i > solution_index)
		result += ", ";
	      result += param.get_generic_param ()
			  .get_type_representation ()
			  .as_string ();
	      result += "=";
	      result += solutions[i].as_string ();
	      i++;
	    }

	  result += ">";
	}
      else
	{
	  rust_sorry_at (
	    ty->get_ref (),
	    "This is a compiler bug: Unhandled type in variance analysis");
	}
      rust_debug ("%s", result.c_str ());
    }
}

tl::optional<SolutionIndex>
GenericTyPerCrateCtx::lookup_type_index (HirId orig_ref)
{
  auto it = map_from_ty_orig_ref.find (orig_ref);
  if (it != map_from_ty_orig_ref.end ())
    {
      return it->second;
    }
  return tl::nullopt;
}

void
GenericTyVisitorCtx::process_type (ADTType &ty)
{
  rust_debug ("add_type_constraints: %s", ty.as_string ().c_str ());

  first_lifetime = lookup_or_add_type (ty.get_orig_ref ());
  first_type = first_lifetime + ty.get_used_arguments ().get_regions ().size ();

  for (auto &param : ty.get_substs ())
    param_names.push_back (
      param.get_generic_param ().get_type_representation ().as_string ());

  for (const auto &variant : ty.get_variants ())
    {
      if (variant->get_variant_type () != VariantDef::NUM)
	{
	  for (const auto &field : variant->get_fields ())
	    add_constraints_from_ty (field->get_field_type (),
				     Variance::covariant ());
	}
    }
}

std::string
GenericTyPerCrateCtx::to_string (const Term &term) const
{
  switch (term.kind)
    {
    case Term::CONST:
      return term.const_val.as_string ();
    case Term::REF:
      return "v(" + to_string (term.ref) + ")";
    case Term::TRANSFORM:
      return "(" + to_string (*term.transform.lhs) + " x "
	     + to_string (*term.transform.rhs) + ")";
    }
  rust_unreachable ();
}

std::string
GenericTyPerCrateCtx::to_string (SolutionIndex index) const
{
  // Search all values in def_id_to_solution_index_start and find key for
  // largest value smaller than index
  std::pair<HirId, SolutionIndex> best = {0, 0};

  for (const auto &ty_map : map_from_ty_orig_ref)
    {
      if (ty_map.second <= index && ty_map.first > best.first)
	best = ty_map;
    }
  rust_assert (best.first != 0);

  BaseType *ty = lookup_type (best.first);

  std::string result = "";
  if (auto adt = ty->try_as<ADTType> ())
    {
      result += (adt->get_identifier ());
    }
  else
    {
      result += ty->as_string ();
    }

  result += "[" + std::to_string (index - best.first) + "]";
  return result;
}

Variance
GenericTyPerCrateCtx::evaluate (Term *term)
{
  switch (term->kind)
    {
    case Term::CONST:
      return term->const_val;
    case Term::REF:
      return solutions[term->ref];
    case Term::TRANSFORM:
      return Variance::transform (evaluate (term->transform.lhs),
				  evaluate (term->transform.rhs));
    }
  rust_unreachable ();
}

std::vector<Variance>
GenericTyPerCrateCtx::query_generic_variance (const ADTType &type)
{
  auto solution_index = lookup_type_index (type.get_orig_ref ());
  rust_assert (solution_index.has_value ());
  auto num_lifetimes = type.get_num_lifetime_params ();
  auto num_types = type.get_num_type_params ();

  std::vector<Variance> result;
  for (size_t i = 0; i < num_lifetimes + num_types; ++i)
    {
      result.push_back (solutions[solution_index.value () + i]);
    }

  return result;
}

FreeRegions
GenericTyPerCrateCtx::query_field_regions (const ADTType *parent,
					   size_t variant_index,
					   size_t field_index,
					   const FreeRegions &parent_regions)
{
  auto orig = lookup_type (parent->get_orig_ref ());
  FieldVisitorCtx ctx (*this, *parent->as<const SubstitutionRef> (),
		       parent_regions);
  return ctx.collect_regions (*orig->as<const ADTType> ()
				 ->get_variants ()
				 .at (variant_index)
				 ->get_fields ()
				 .at (field_index)
				 ->get_field_type ());
}
std::vector<Region>
GenericTyPerCrateCtx::query_type_regions (BaseType *type)
{
  TyVisitorCtx ctx (*this);
  return ctx.collect_regions (*type);
}

SolutionIndex
GenericTyVisitorCtx::lookup_or_add_type (HirId hir_id)
{
  BaseType *ty = lookup_type (hir_id);
  auto index = ctx.lookup_type_index (hir_id);
  if (index.has_value ())
    {
      return index.value ();
    }

  SubstitutionRef *subst = nullptr;
  if (auto adt = ty->try_as<ADTType> ())
    {
      subst = adt;
    }
  else
    {
      rust_sorry_at (
	ty->get_locus (),
	"This is a compiler bug: Unhandled type in variance analysis");
    }
  rust_assert (subst != nullptr);

  auto solution_index = ctx.solutions.size ();
  ctx.map_from_ty_orig_ref.emplace (ty->get_orig_ref (), solution_index);

  auto num_lifetime_param = subst->get_used_arguments ().get_regions ().size ();
  auto num_type_param = subst->get_num_substitutions ();

  for (size_t i = 0; i < num_lifetime_param + num_type_param; ++i)
    ctx.solutions.emplace_back (Variance::bivariant ());

  return solution_index;
}

void
GenericTyVisitorCtx::add_constraints_from_ty (BaseType *type, Term variance)
{
  rust_debug ("\tadd_constraint_from_ty: %s with v=%s",
	      type->as_string ().c_str (), ctx.to_string (variance).c_str ());

  Visitor visitor (*this, variance);
  type->accept_vis (visitor);
}

void
GenericTyVisitorCtx::add_constraint (SolutionIndex index, Term term)
{
  rust_debug ("\t\tadd_constraint: %s", ctx.to_string (term).c_str ());

  if (term.kind == Term::CONST)
    {
      // Constant terms do not depend on other solutions, so we can
      // immediately apply them.
      ctx.solutions[index].join (term.const_val);
    }
  else
    {
      ctx.constraints.push_back ({index, new Term (term)});
    }
}

void
GenericTyVisitorCtx::add_constraints_from_region (const Region &region,
						  Term term)
{
  if (region.is_early_bound ())
    {
      add_constraint (first_lifetime + region.get_index (), term);
    }
}

void
GenericTyVisitorCtx::add_constraints_from_generic_args (HirId ref,
							SubstitutionRef &subst,
							Term variance,
							bool invariant_args)
{
  SolutionIndex solution_index = lookup_or_add_type (ref);

  size_t num_lifetimes = subst.get_used_arguments ().get_regions ().size ();
  size_t num_types = subst.get_substs ().size ();

  for (size_t i = 0; i < num_lifetimes + num_types; ++i)
    {
      // TODO: What about variance from other crates?
      auto variance_i
	= invariant_args
	    ? Term::make_transform (variance, Variance::invariant ())
	    : Term::make_transform (variance,
				    Term::make_ref (solution_index + i));

      if (i < num_lifetimes)
	{
	  auto region_i = i;
	  auto &region
	    = subst.get_substitution_arguments ().get_mut_regions ()[region_i];
	  add_constraints_from_region (region, variance_i);
	}
      else
	{
	  auto type_i = i - num_lifetimes;
	  auto arg = subst.get_arg_at (type_i);
	  if (arg.has_value ())
	    {
	      add_constraints_from_ty (arg.value ().get_tyty (), variance_i);
	    }
	}
    }
}
void
GenericTyVisitorCtx::add_constrints_from_param (ParamType &type, Term variance)
{
  auto it
    = std::find (param_names.begin (), param_names.end (), type.get_name ());
  rust_assert (it != param_names.end ());

  auto index = first_type + std::distance (param_names.begin (), it);

  add_constraint (index, variance);
}

Term
GenericTyVisitorCtx::contra (Term variance)
{
  return Term::make_transform (variance, Variance::contravariant ());
}

void
TyVisitorCtx::add_constraints_from_ty (BaseType *ty, Variance variance)
{
  Visitor visitor (*this, variance);
  ty->accept_vis (visitor);
}

void
TyVisitorCtx::add_constraints_from_region (const Region &region,
					   Variance variance)
{
  variances.push_back (variance);
  regions.push_back (region);
}

void
TyVisitorCtx::add_constraints_from_generic_args (HirId ref,
						 SubstitutionRef &subst,
						 Variance variance,
						 bool invariant_args)
{
  // Handle function
  auto variances
    = ctx.query_generic_variance (*lookup_type (ref)->as<ADTType> ());

  size_t num_lifetimes = subst.get_used_arguments ().get_regions ().size ();
  size_t num_types = subst.get_substs ().size ();

  for (size_t i = 0; i < num_lifetimes + num_types; ++i)
    {
      // TODO: What about variance from other crates?
      auto variance_i
	= invariant_args
	    ? Variance::transform (variance, Variance::invariant ())
	    : Variance::transform (variance, variances[i]);

      if (i < num_lifetimes)
	{
	  auto region_i = i;
	  auto &region = subst.get_used_arguments ().get_regions ()[region_i];
	  add_constraints_from_region (region, variance_i);
	}
      else
	{
	  auto type_i = i - num_lifetimes;
	  auto arg = subst.get_arg_at (type_i);
	  if (arg.has_value ())
	    {
	      add_constraints_from_ty (arg.value ().get_tyty (), variance_i);
	    }
	}
    }
}

FreeRegions
FieldVisitorCtx::collect_regions (BaseType &ty)
{
  // Segment the regions into ranges for each type parameter. Type parameter
  // at index i contains regions from type_param_ranges[i] to
  // type_param_ranges[i+1] (exclusive).;
  type_param_ranges.push_back (subst.get_num_lifetime_params ());

  for (size_t i = 0; i < subst.get_num_type_params (); i++)
    {
      auto arg = subst.get_arg_at (i);
      rust_assert (arg.has_value ());
      type_param_ranges.push_back (
	ctx.query_type_regions (arg.value ().get_tyty ()).size ());
    }

  add_constraints_from_ty (&ty, Variance::covariant ());
  return regions;
}

void
FieldVisitorCtx::add_constraints_from_ty (BaseType *ty, Variance variance)
{
  Visitor visitor (*this, variance);
  ty->accept_vis (visitor);
}

void
FieldVisitorCtx::add_constraints_from_region (const Region &region,
					      Variance variance)
{
  if (region.is_early_bound ())
    {
      regions.push_back (parent_regions[region.get_index ()]);
    }
  else if (region.is_late_bound ())
    {
      rust_debug ("Ignoring late bound region");
    }
}

void
FieldVisitorCtx::add_constrints_from_param (ParamType &param, Variance variance)
{
  size_t param_i = subst.get_used_arguments ().find_symbol (param).value ();
  for (size_t i = type_param_ranges[param_i];
       i < type_param_ranges[param_i + 1]; i++)
    {
      regions.push_back (parent_regions[i]);
    }
}

Variance
TyVisitorCtx::contra (Variance variance)
{
  return Variance::transform (variance, Variance::contravariant ());
}

Term
Term::make_ref (SolutionIndex index)
{
  Term term;
  term.kind = REF;
  term.ref = index;
  return term;
}

Term
Term::make_transform (Term lhs, Term rhs)
{
  if (lhs.is_const () && rhs.is_const ())
    {
      return Variance::transform (lhs.const_val, rhs.const_val);
    }

  Term term;
  term.kind = TRANSFORM;
  term.transform.lhs = new Term (lhs);
  term.transform.rhs = new Term (rhs);
  return term;
}

} // namespace VarianceAnalysis
} // namespace TyTy
} // namespace Rust
