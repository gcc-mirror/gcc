#ifndef RUST_TYTY_VARIANCE_ANALYSIS_PRIVATE_H
#define RUST_TYTY_VARIANCE_ANALYSIS_PRIVATE_H

#include "rust-tyty-variance-analysis.h"

#include "rust-tyty-visitor.h"

namespace Rust {
namespace TyTy {
namespace VarianceAnalysis {

using SolutionIndex = uint32_t;

/** Term descibing variance relations. */
struct Term
{
  enum Kind : uint8_t
  {
    CONST,
    REF,
    TRANSFORM,
  };

  Kind kind;
  union
  {
    struct
    {
      Term *lhs;
      Term *rhs;
    } transform;
    SolutionIndex ref;
    Variance const_val;
  };

  Term () {}

  Term (Variance variance) : kind (CONST), const_val (variance) {}

  WARN_UNUSED_RESULT bool is_const () const { return kind == CONST; }

  static Term make_ref (SolutionIndex index);

  static Term make_transform (Term lhs, Term rhs);
};

/** Variance constraint of a type parameter. */
struct Constraint
{
  SolutionIndex target_index;
  Term *term;
};

/** Abstract variance visitor context. */
template <typename VARIANCE> class VarianceVisitorCtx
{
public:
  virtual ~VarianceVisitorCtx () = default;

  virtual void add_constraints_from_ty (BaseType *ty, VARIANCE variance) = 0;
  virtual void add_constraints_from_region (const Region &region,
					    VARIANCE variance)
    = 0;
  void add_constraints_from_mutability (BaseType *type, Mutability mutability,
					VARIANCE variance)
  {
    switch (mutability)
      {
      case Mutability::Imm:
	return add_constraints_from_ty (type, variance);
      case Mutability::Mut:
	return add_constraints_from_ty (type, Variance::invariant ());
      }
  }
  virtual void
  add_constraints_from_generic_args (HirId ref, SubstitutionRef &subst,
				     VARIANCE variance, bool invariant_args)
    = 0;
  virtual void add_constrints_from_param (ParamType &param, VARIANCE variance)
    = 0;
  virtual VARIANCE contra (VARIANCE variance) = 0;
};

template <typename VARIANCE> class VisitorBase final : public TyVisitor
{
  VarianceVisitorCtx<VARIANCE> &ctx;
  VARIANCE variance;

public:
  VisitorBase (VarianceVisitorCtx<VARIANCE> &ctx, VARIANCE variance)
    : ctx (ctx), variance (variance)
  {}

  void visit (BoolType &type) override {}
  void visit (CharType &type) override {}
  void visit (IntType &type) override {}
  void visit (UintType &type) override {}
  void visit (FloatType &type) override {}
  void visit (USizeType &type) override {}
  void visit (ISizeType &type) override {}
  void visit (StrType &type) override {}
  void visit (NeverType &type) override {}

  void visit (ClosureType &type) override {}
  void visit (FnType &type) override
  {
    for (auto &region : type.get_used_arguments ().get_regions ())
      ctx.add_constraints_from_region (region, Variance::invariant ());
  }

  void visit (ReferenceType &type) override
  {
    ctx.add_constraints_from_region (type.get_region (), variance);
    ctx.add_constraints_from_mutability (type.get_base (), type.mutability (),
					 variance);
  }
  void visit (ArrayType &type) override
  {
    ctx.add_constraints_from_ty (type.get_element_type (), variance);
  }
  void visit (SliceType &type) override
  {
    ctx.add_constraints_from_ty (type.get_element_type (), variance);
  }
  void visit (PointerType &type) override
  {
    ctx.add_constraints_from_ty (type.get_base (), variance);
    ctx.add_constraints_from_mutability (type.get_base (), type.mutability (),
					 variance);
  }
  void visit (TupleType &type) override
  {
    for (auto &elem : type.get_fields ())
      ctx.add_constraints_from_ty (elem.get_tyty (), variance);
  }
  void visit (ADTType &type) override
  {
    ctx.add_constraints_from_generic_args (type.get_orig_ref (), type, variance,
					   false);
  }
  void visit (ProjectionType &type) override
  {
    ctx.add_constraints_from_generic_args (type.get_orig_ref (), type, variance,
					   true);
  }
  void visit (ParamType &type) override
  {
    ctx.add_constrints_from_param (type, variance);
  }
  void visit (FnPtr &type) override
  {
    auto contra = ctx.contra (variance);

    for (auto &param : type.get_params ())
      {
	ctx.add_constraints_from_ty (param.get_tyty (), contra);
      }

    ctx.add_constraints_from_ty (type.get_return_type (), variance);
  }

  void visit (ErrorType &type) override {}

  void visit (PlaceholderType &type) override { rust_unreachable (); }
  void visit (InferType &type) override { rust_unreachable (); }

  void visit (DynamicObjectType &type) override
  {
    // TODO
  }

  void visit (OpaqueType &type) override {}
};

/** Per crate context for generic type variance analysis. */
class GenericTyPerCrateCtx
{
public: // External API
  /** Add a type to context and process its variance constraints. */
  void process_type (ADTType &ty);

  /**
   * Solve for all variance constraints and clear temporary data.
   *
   * Only keeps the results.
   */
  void solve ();

  /** Prints solution debug output. To be called after solve. */
  void debug_print_solutions ();

  tl::optional<SolutionIndex> lookup_type_index (HirId orig_ref);

public: // Module internal API
  /** Format term tree to string. */
  WARN_UNUSED_RESULT std::string to_string (const Term &term) const;

  /** Formats as <type ident>`[`<param index>``]` */
  WARN_UNUSED_RESULT std::string to_string (SolutionIndex index) const;

  /** Evaluate a variance relation expression (term tree). */
  Variance evaluate (Term *term);

  std::vector<Variance> query_generic_variance (const ADTType &type);

  FreeRegions query_field_regions (const ADTType *parent, size_t variant_index,
				   size_t field_index,
				   const FreeRegions &parent_regions);

  std::vector<Region> query_type_regions (BaseType *base);

public: // Data used by visitors.
  // This whole class is private, therfore members can be public.

  /** Current solutions. Initiated to bivariant. */
  std::vector<Variance> solutions;

  /** Constrains on solutions. Iteratively applied until fixpoint. */
  std::vector<Constraint> constraints;

  /** Maps TyTy::orig_ref to an index of first solution for this type. */
  std::unordered_map<HirId, SolutionIndex> map_from_ty_orig_ref;
};

/** Visitor context for generic type variance analysis used for processing of a
 * single type. */
class GenericTyVisitorCtx : VarianceVisitorCtx<Term>
{
  using Visitor = VisitorBase<Term>;

public:
  explicit GenericTyVisitorCtx (GenericTyPerCrateCtx &ctx) : ctx (ctx) {}
  /** Entry point: Add a type to context and process its variance constraints.
   */
  void process_type (ADTType &ty);

private:
  /** Resolve a type from a TyTy::ref. */
  SolutionIndex lookup_or_add_type (HirId hir_id);

  /** Visit an inner type and add its constraints. */
  void add_constraints_from_ty (BaseType *ty, Term variance) override;

  void add_constraint (SolutionIndex index, Term term);

  void add_constraints_from_region (const Region &region, Term term) override;

  void add_constraints_from_generic_args (HirId ref, SubstitutionRef &subst,
					  Term variance,
					  bool invariant_args) override;

  void add_constrints_from_param (ParamType &type, Term variance) override;

  /** Construct a term for type in contravaraint position. */
  Term contra (Term variance) override;

private:
  GenericTyPerCrateCtx &ctx;

private: // Per type processing context
  /** Index of the solution first **lifetime param** for the current type. */
  SolutionIndex first_lifetime = 0;

  /** Index of the solution first **type param** for the current type. */
  SolutionIndex first_type = 0;

  /** Maps type param names to index among type params. */
  std::vector<std::string> param_names;
};

/** Visitor context for basic type variance analysis. */
class TyVisitorCtx : public VarianceVisitorCtx<Variance>
{
public:
  using Visitor = VisitorBase<Variance>;

  TyVisitorCtx (GenericTyPerCrateCtx &ctx) : ctx (ctx) {}

  std::vector<Variance> collect_variances (BaseType &ty)
  {
    add_constraints_from_ty (&ty, Variance::covariant ());
    return variances;
  }

  std::vector<Region> collect_regions (BaseType &ty)
  {
    add_constraints_from_ty (&ty, Variance::covariant ());
    return regions;
  }

  void add_constraints_from_ty (BaseType *ty, Variance variance) override;
  void add_constraints_from_region (const Region &region,
				    Variance variance) override;
  void add_constraints_from_generic_args (HirId ref, SubstitutionRef &subst,
					  Variance variance,
					  bool invariant_args) override;
  void add_constrints_from_param (ParamType &param, Variance variance) override
  {}
  Variance contra (Variance variance) override;

private:
  GenericTyPerCrateCtx &ctx;
  std::vector<Variance> variances;
  std::vector<Region> regions;
};

/** Extracts regions of a field from regions of parent ADT. */
class FieldVisitorCtx : public VarianceVisitorCtx<Variance>
{
public:
  using Visitor = VisitorBase<Variance>;

  FreeRegions collect_regions (BaseType &ty);

  FieldVisitorCtx (GenericTyPerCrateCtx &ctx, const SubstitutionRef &subst,
		   const FreeRegions &parent_regions)
    : ctx (ctx), subst (subst), parent_regions (parent_regions)
  {}

  void add_constraints_from_ty (BaseType *ty, Variance variance) override;
  void add_constraints_from_region (const Region &region,
				    Variance variance) override;
  void add_constraints_from_generic_args (HirId ref, SubstitutionRef &subst,
					  Variance variance,
					  bool invariant_args) override{};
  void add_constrints_from_param (ParamType &param, Variance variance) override;

  Variance contra (Variance variance) override
  {
    return Variance::transform (variance, Variance::contravariant ());
  }

private:
  GenericTyPerCrateCtx &ctx;
  const SubstitutionRef &subst;
  FreeRegions regions;
  FreeRegions parent_regions;
  std::vector<size_t> type_param_ranges;
};

} // namespace VarianceAnalysis

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_VARIANCE_ANALYSIS_PRIVATE_H
