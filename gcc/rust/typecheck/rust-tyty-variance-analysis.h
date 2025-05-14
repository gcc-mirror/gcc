#ifndef RUST_TYTY_VARIANCE_ANALYSIS_H
#define RUST_TYTY_VARIANCE_ANALYSIS_H

#include "rust-tyty.h"

#include <rust-bir-free-region.h>

namespace Rust {
namespace TyTy {
namespace VarianceAnalysis {

class Variance;
class GenericTyPerCrateCtx;

/** Per crate context for variance analysis. */
class CrateCtx
{
public:
  CrateCtx ();
  ~CrateCtx ();

  /** Add type to variance analysis context. */
  void add_type_constraints (ADTType &type);

  /** Solve all constraints and print debug output. */
  void solve ();

  /** Get variance of a type parameters. */
  std::vector<Variance> query_generic_variance (const ADTType &type);

  /** Get variance of a type body (members, fn parameters...). */
  std::vector<Variance> query_type_variances (BaseType *type);

  /** Get regions mentioned in a type. */
  std::vector<Region> query_type_regions (BaseType *type);
  FreeRegions query_field_regions (const ADTType *parent, size_t variant_index,
				   size_t field_index,
				   const FreeRegions &parent_regions);

private:
  std::unique_ptr<GenericTyPerCrateCtx> private_ctx;
};

std::vector<size_t>
query_field_regions (const ADTType *parent, size_t variant_index,
		     size_t field_index, const FreeRegions &parent_regions);

/** Variance semilattice */
class Variance
{
  enum Kind : uint8_t
  {
    BIVARIANT = 0,     // 0b00
    COVARIANT = 1,     // 0b01
    CONTRAVARIANT = 2, // 0b10
    INVARIANT = 3,     // 0b11
  } kind;

  static constexpr auto TOP = BIVARIANT;
  static constexpr auto BOTTOM = INVARIANT;

  constexpr Variance (Kind kind) : kind (kind) {}

public:
  constexpr Variance () : kind (TOP) {}

  constexpr bool is_bivariant () const { return kind == BIVARIANT; }
  constexpr bool is_covariant () const { return kind == COVARIANT; }
  constexpr bool is_contravariant () const { return kind == CONTRAVARIANT; }
  constexpr bool is_invariant () const { return kind == INVARIANT; }

  static constexpr Variance bivariant () { return {BIVARIANT}; }
  static constexpr Variance covariant () { return {COVARIANT}; }
  static constexpr Variance contravariant () { return {CONTRAVARIANT}; }
  static constexpr Variance invariant () { return {INVARIANT}; }

  WARN_UNUSED_RESULT Variance reverse () const;
  static WARN_UNUSED_RESULT Variance join (Variance lhs, Variance rhs);

  void join (Variance rhs);

  /**
   * Variance composition function.
   *
   * For `A<X>` and `B<X>` and the composition `A<B<X>>` the variance of
   * `v(A<B<X>>, X)` is defined as:
   * ```
   * v(A<B<X>>, X) = v(A<X>, X).transform(v(B<X>, X))
   * ```
   */
  static WARN_UNUSED_RESULT Variance transform (Variance lhs, Variance rhs);

  constexpr friend bool operator== (const Variance &lhs, const Variance &rhs)
  {
    return lhs.kind == rhs.kind;
  }
  constexpr friend bool operator!= (const Variance &lhs, const Variance &rhs)
  {
    return !(lhs == rhs);
  }

  WARN_UNUSED_RESULT std::string as_string () const;
};

} // namespace VarianceAnalysis

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_VARIANCE_ANALYSIS_H
