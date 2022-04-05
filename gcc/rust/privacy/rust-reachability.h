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

#ifndef RUST_REACHABILITY_H
#define RUST_REACHABILITY_H

#include "rust-privacy-ctx.h"
#include "rust-hir-visitor.h"
#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"

namespace Rust {
namespace Privacy {

// FIXME: The EmbargoVisitor from rustc is a fixed-point visitor which tries
// to reach more and more nodes until nothing has changed anymore.
// Do we need to reproduce this behavior? How long does it take to do this?

class ReachabilityVisitor : public HIR::HIRVisItemVisitor
{
public:
  ReachabilityVisitor (PrivacyContext &ctx)
    : current_level (ReachLevel::Private), ctx (ctx)
  {}

  virtual void visit (HIR::Module &mod);
  virtual void visit (HIR::ExternCrate &crate);
  virtual void visit (HIR::UseDeclaration &use_decl);
  virtual void visit (HIR::Function &func);
  virtual void visit (HIR::TypeAlias &type_alias);
  virtual void visit (HIR::StructStruct &struct_item);
  virtual void visit (HIR::TupleStruct &tuple_struct);
  virtual void visit (HIR::Enum &enum_item);
  virtual void visit (HIR::Union &union_item);
  virtual void visit (HIR::ConstantItem &const_item);
  virtual void visit (HIR::StaticItem &static_item);
  virtual void visit (HIR::Trait &trait);
  virtual void visit (HIR::ImplBlock &impl);
  virtual void visit (HIR::ExternBlock &block);

private:
  ReachLevel current_level;
  PrivacyContext &ctx;
};
} // namespace Privacy
} // namespace Rust

#endif // !RUST_REACHABILITY_H
