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

#include "rust-privacy-check.h"
#include "rust-reachability.h"
#include "rust-hir-type-check.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-visibility-resolver.h"
#include "rust-pub-restricted-visitor.h"
#include "rust-privacy-reporter.h"

extern bool
saw_errors (void);

namespace Rust {
namespace Privacy {

void
Resolver::resolve (HIR::Crate &crate)
{
  PrivacyContext ctx;
  auto mappings = Analysis::Mappings::get ();
  auto resolver = Rust::Resolver::Resolver::get ();
  auto ty_ctx = ::Rust::Resolver::TypeCheckContext::get ();

  VisibilityResolver (*mappings, *resolver).go (crate);
  PubRestrictedVisitor (*mappings).go (crate);
  PrivacyReporter (*mappings, *resolver, *ty_ctx).go (crate);

  auto visitor = ReachabilityVisitor (ctx, *ty_ctx);

  for (auto &item : crate.get_items ())
    {
      if (item->get_hir_kind () == HIR::Node::VIS_ITEM)
	{
	  auto vis_item = static_cast<HIR::VisItem *> (item.get ());
	  vis_item->accept_vis (visitor);
	}
    }

  if (saw_errors ())
    return;
}
} // namespace Privacy
} // namespace Rust
