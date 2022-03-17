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

#include "rust-hir-type-check-base.h"

namespace Rust {
namespace Resolver {

bool
TypeCheckBase::check_for_unconstrained (
  const std::vector<TyTy::SubstitutionParamMapping> &params_to_constrain,
  const TyTy::SubstitutionArgumentMappings &constraint_a,
  const TyTy::SubstitutionArgumentMappings &constraint_b,
  const TyTy::BaseType *reference)
{
  std::set<HirId> symbols_to_constrain;
  std::map<HirId, Location> symbol_to_location;
  for (const auto &p : params_to_constrain)
    {
      HirId ref = p.get_param_ty ()->get_ref ();
      symbols_to_constrain.insert (ref);
      symbol_to_location.insert ({ref, p.get_param_locus ()});
    }

  // set up the set of constrained symbols
  std::set<HirId> constrained_symbols;
  for (const auto &c : constraint_a.get_mappings ())
    {
      const TyTy::BaseType *arg = c.get_tyty ();
      if (arg != nullptr)
	{
	  const TyTy::BaseType *p = arg->get_root ();
	  constrained_symbols.insert (p->get_ty_ref ());
	}
    }
  for (const auto &c : constraint_b.get_mappings ())
    {
      const TyTy::BaseType *arg = c.get_tyty ();
      if (arg != nullptr)
	{
	  const TyTy::BaseType *p = arg->get_root ();
	  constrained_symbols.insert (p->get_ty_ref ());
	}
    }

  const auto root = reference->get_root ();
  if (root->get_kind () == TyTy::TypeKind::PARAM)
    {
      const TyTy::ParamType *p = static_cast<const TyTy::ParamType *> (root);
      constrained_symbols.insert (p->get_ty_ref ());
    }

  // check for unconstrained
  bool unconstrained = false;
  for (auto &sym : symbols_to_constrain)
    {
      bool used = constrained_symbols.find (sym) != constrained_symbols.end ();
      if (!used)
	{
	  Location locus = symbol_to_location.at (sym);
	  rust_error_at (locus, "unconstrained type parameter");
	  unconstrained = true;
	}
    }
  return unconstrained;
}

} // namespace Resolver
} // namespace Rust
