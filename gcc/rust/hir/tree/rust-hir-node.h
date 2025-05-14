// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_NODE_H
#define RUST_HIR_NODE_H

namespace Rust {

namespace HIR {

class Node
{
public:
  // Kind for downcasting various HIR nodes to other base classes when visiting
  // them
  enum BaseKind
  {
    /* class ExternalItem */
    EXTERNAL,
    /* class TraitItem */
    TRAIT_ITEM,
    /* class VisItem */
    VIS_ITEM,
    /* class Item */
    ITEM,
    /* class ImplItem */
    IMPL,
    /* class Type */
    TYPE,
    /* class Stmt */
    STMT,
    /* class Expr */
    EXPR,
    /* class Pattern */
    PATTERN,
  };

  /**
   * Get the kind of HIR node we are dealing with. This is useful for
   * downcasting to more precise types when necessary, i.e going from an `Item*`
   * to a `VisItem*`
   */
  virtual BaseKind get_hir_kind () = 0;
};

} // namespace HIR
} // namespace Rust
#endif
