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

#ifndef RUST_HIR_METHOD_RESOLVE_H
#define RUST_HIR_METHOD_RESOLVE_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class MethodResolution : public TypeCheckBase
{
public:
  static std::vector<HIR::Method *> Probe (TyTy::BaseType *receiver,
					   HIR::PathExprSegment method_name)
  {
    MethodResolution probe (receiver, method_name);

    // lookup impl items for this crate and find all methods that can resolve to
    // this receiver
    probe.mappings->iterate_impl_items (
      [&] (HirId id, HIR::InherentImplItem *item) mutable -> bool {
	item->accept_vis (probe);
	return true;
      });

    return probe.probed;
  }

  void visit (HIR::Method &method)
  {
    TyTy::BaseType *self_lookup = nullptr;
    if (!context->lookup_type (
	  method.get_self_param ().get_mappings ().get_hirid (), &self_lookup))
      {
	rust_error_at (method.get_self_param ().get_locus (),
		       "failed to lookup lookup self type in MethodProbe");
	return;
      }

    // are the names the same
    HIR::PathIdentSegment seg = method_name.get_segment ();
    if (seg.as_string ().compare (method.get_method_name ()) != 0)
      {
	// if the method name does not match then this is not a valid match
	return;
      }

    // FIXME this can be simplified with
    // https://github.com/Rust-GCC/gccrs/issues/187
    auto combined = receiver->combine (self_lookup);
    if (combined == nullptr)
      {
	// incompatible self argument then this is not a valid method for this
	// receiver
	return;
      }
    delete combined;

    probed.push_back (&method);
  }

private:
  MethodResolution (TyTy::BaseType *receiver, HIR::PathExprSegment method_name)
    : TypeCheckBase (), receiver (receiver), method_name (method_name)
  {}

  TyTy::BaseType *receiver;
  HIR::PathExprSegment method_name;

  std::vector<HIR::Method *> probed;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_METHOD_RESOLVE_H
