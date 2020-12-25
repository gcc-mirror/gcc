// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_TYTY_RESOLVER
#define RUST_TYTY_RESOLVER

#include "rust-system.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-hir-type-check.h"
#include "rust-hir-full.h"
#include "rust-tyty-visitor.h"

namespace Rust {
namespace Resolver {

class TyTyResolver
{
public:
  static void Resolve (Rib *rib, Analysis::Mappings *mappings,
		       Resolver *resolver, TypeCheckContext *context)
  {
    TyTyResolver r (mappings, resolver, context);
    r.go (rib);
  }

  virtual ~TyTyResolver () {}

  void go (Rib *rib)
  {
    rib->iterate_decls ([&] (NodeId decl_node_id) mutable -> bool {
      // type inference in rust means we need to gather and examine all
      // references of this decl and combine each to make sure the type is
      // correctly inferred. Consider the example:
      // let mut x; x = 1;
      // we can only say x is an infer variable then at the assignment
      // we think x must be an integer

      std::vector<TyTy::TyBase *> gathered_types;
      rib->iterate_references_for_def (
	decl_node_id, [&] (NodeId ref_node) mutable -> bool {
	  HirId hir_node_ref;
	  bool ok
	    = mappings->lookup_node_to_hir (mappings->get_current_crate (),
					    ref_node, &hir_node_ref);
	  rust_assert (ok);

	  TyTy::TyBase *resolved = nullptr;
	  if (!context->lookup_type (hir_node_ref, &resolved))
	    {
	      // this could be an array/adt type
	      Definition d;
	      bool ok = resolver->lookup_definition (ref_node, &d);
	      rust_assert (ok);

	      ok = mappings->lookup_node_to_hir (mappings->get_current_crate (),
						 d.parent, &hir_node_ref);
	      rust_assert (ok);

	      printf ("failed lets try [%u]\n", hir_node_ref);

	      if (!context->lookup_type (hir_node_ref, &resolved))
		{
		  rust_fatal_error (
		    mappings->lookup_location (hir_node_ref),
		    "failed to lookup type for reference at node [%u]",
		    hir_node_ref);
		  return false;
		}
	    }

	  gathered_types.push_back (resolved);
	  return true;
	});

      Definition d;
      bool ok = resolver->lookup_definition (decl_node_id, &d);
      rust_assert (ok);

      HIR::Stmt *decl = nullptr;
      ok = mappings->resolve_nodeid_to_stmt (d.parent, &decl);
      rust_assert (ok);

      TyTy::TyBase *resolved_type = nullptr;
      ok = context->lookup_type (decl->get_mappings ().get_hirid (),
				 &resolved_type);
      rust_assert (ok);

      if (!resolved_type->is_unit ())
	{
	  return true;
	}

      auto resolved_tyty = resolved_type;
      for (auto it : gathered_types)
	{
	  auto combined = resolved_tyty->combine (it);
	  if (combined == nullptr)
	    break;

	  resolved_tyty = combined;
	}

      // something is not inferred we need to look at all references now
      if (resolved_tyty == nullptr || resolved_tyty->is_unit ())
	{
	  rust_fatal_error (decl->get_locus_slow (), "failed to resolve type");
	  return false;
	}

      // insert the new resolved definition
      context->insert_type (decl->get_mappings ().get_hirid (), resolved_tyty);
      return true;
    });
  }

protected:
private:
  TyTyResolver (Analysis::Mappings *mappings, Resolver *resolver,
		TypeCheckContext *context)
    : mappings (mappings), resolver (resolver), context (context)
  {}

  Analysis::Mappings *mappings;
  Resolver *resolver;
  TypeCheckContext *context;
};

class TyTyExtractorArray : public TyTy::TyVisitor
{
public:
  static TyTy::TyBase *ExtractElementTypeFromArray (TyTy::TyBase *base)
  {
    TyTyExtractorArray e;
    base->accept_vis (e);
    rust_assert (e.extracted != nullptr);
    return e.extracted;
  }

  virtual ~TyTyExtractorArray () {}

  void visit (TyTy::ArrayType &type) override { extracted = type.get_type (); }

private:
  TyTyExtractorArray () : extracted (nullptr) {}

  TyTy::TyBase *extracted;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_TYTY_RESOLVER
