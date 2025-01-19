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

#ifndef RUST_PUB_RESTRICTED_VISITOR_H
#define RUST_PUB_RESTRICTED_VISITOR_H

#include "rust-hir-visitor.h"
#include "rust-hir.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Privacy {

/**
 * This visitor takes care of reporting `pub(restricted)` violations:
 * A `pub(restricted)` violation is defined as the usage of a path restriction
 * on an item which does not restrict the item's visibility to one of its parent
 * modules. What this means is that an user is allowed to specify that an item
 * should be public for any of its parent modules, going all the way to the
 * `crate` module, but not for any of its children module.
 *
 * ```rust
 * mod a {
 * 	mod b {
 * 		pub (in a) struct A0;
 *
 * 		mod c {
 * 			mod d {
 * 				pub (in a) struct A1;
 * 			}
 * 		}
 *
 * 		pub (in c::d) struct A2;
 * 	}
 * }
 * ```
 *
 * The above `A0`'s visibility is valid: It is restricted to a path, `a`,
 * which is a parent of the current module, `b`.
 * Likewise, `A1` is also defined properly: `a` is a parent of `d`, albeit
 * a great-great-great-grandparant of it.
 *
 * `A2` visibility, however, is invalid: Where the struct is defined, the
 * current module is `b`. `c::d` (which refers to the `d` module) is a child of
 * `b`, and not one of its ancestors.
 *
 * Note that these resolution rules are also the ones of the 2015 rust edition:
 * All the `pub(restricted)` visibilities above would be invalid in the 2018
 * edition, as the paths there must be absolute and not relative (`c::d` would
 * become `crate::a::b::c::d` etc). Nonetheless, the logic stays the same.
 */
class PubRestrictedVisitor : public HIR::HIRVisItemVisitor
{
public:
  PubRestrictedVisitor (Analysis::Mappings &mappings);

  void go (HIR::Crate &crate);

  /**
   * Check if an item's restricted visibility (`pub (crate)`, `pub (self)`,
   * `pub(super)`, `pub (in <path>)`) is valid in the current context.
   * `pub restricted` visibilities are only allowed when the restriction path
   * is a parent module of the item being visited.
   *
   * In case of error, this function will emit the errors and return.
   *
   * @param item_id NodeId of the item to check the restriction of
   * @param locus Location of the item being checked
   *
   * @return true if the visibility restriction is valid, false otherwise.
   */
  bool is_restriction_valid (NodeId item_id, const location_t locus);

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
  /* Stack of ancestor modules visited by this visitor */
  std::vector<DefId> module_stack;

  // FIXME: Do we have to handle `self` and `super` as part of the name
  // resolution pass?

  Analysis::Mappings &mappings;
};

} // namespace Privacy
} // namespace Rust

#endif // !RUST_PUB_RESTRICTED_VISITOR_H
