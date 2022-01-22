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

#ifndef RUST_COMPILE_BASE
#define RUST_COMPILE_BASE

#include "rust-compile-context.h"
#include "rust-compile-type.h"
#include "rust-hir-visitor.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Compile {

class HIRCompileBase : public HIR::HIRFullVisitorBase
{
public:
  virtual ~HIRCompileBase () {}

protected:
  HIRCompileBase (Context *ctx) : ctx (ctx) {}

  Context *ctx;

  Context *get_context () { return ctx; }

  void compile_function_body (tree fndecl, HIR::BlockExpr &function_body,
			      bool has_return_type);

  bool compile_locals_for_block (Resolver::Rib &rib, tree fndecl,
				 std::vector<Bvariable *> &locals);

  tree coercion_site (tree rvalue, TyTy::BaseType *actual,
		      TyTy::BaseType *expected, Location lvalue_locus,
		      Location rvalue_locus);

  tree coerce_to_dyn_object (tree compiled_ref, const TyTy::BaseType *actual,
			     const TyTy::BaseType *expected,
			     const TyTy::DynamicObjectType *ty, Location locus);

  tree compute_address_for_trait_item (
    const Resolver::TraitItemReference *ref,
    const TyTy::TypeBoundPredicate *predicate,
    std::vector<std::pair<Resolver::TraitReference *, HIR::ImplBlock *>>
      &receiver_bounds,
    const TyTy::BaseType *receiver, const TyTy::BaseType *root, Location locus);

  bool verify_array_capacities (tree ltype, tree rtype, Location ltype_locus,
				Location rtype_locus);

  tree query_compile (HirId ref, TyTy::BaseType *lookup,
		      const HIR::PathIdentSegment &final_segment,
		      const Analysis::NodeMapping &mappings,
		      Location expr_locus, bool is_qualified_path);

  tree resolve_adjustements (std::vector<Resolver::Adjustment> &adjustments,
			     tree expression, Location locus);

  tree resolve_deref_adjustment (Resolver::Adjustment &adjustment,
				 tree expression, Location locus);
};

} // namespace Compile
} // namespace Rust

#endif // RUST_COMPILE_BASE
