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

#ifndef RUST_HIR_PATTERN_ABSTRACT_H
#define RUST_HIR_PATTERN_ABSTRACT_H

#include "rust-hir-visitable.h"
#include "rust-hir-visitor.h"
#include "rust-hir-node.h"
#include "rust-system.h"

namespace Rust {
namespace HIR {

// Pattern base HIR node
class Pattern : public Node, virtual public FullVisitable
{
public:
  using FullVisitable::accept_vis;

  enum PatternType
  {
    PATH,
    LITERAL,
    IDENTIFIER,
    WILDCARD,
    RANGE,
    REFERENCE,
    STRUCT,
    TUPLE_STRUCT,
    TUPLE,
    GROUPED,
    SLICE,
    ALT
  };

  BaseKind get_hir_kind () override final { return PATTERN; }

  // Unique pointer custom clone function
  std::unique_ptr<Pattern> clone_pattern () const
  {
    return std::unique_ptr<Pattern> (clone_pattern_impl ());
  }

  // possible virtual methods: is_refutable()

  virtual ~Pattern () {}

  virtual std::string as_string () const = 0;

  virtual void accept_vis (HIRPatternVisitor &vis) = 0;

  virtual const Analysis::NodeMapping &get_mappings () const = 0;

  virtual location_t get_locus () const = 0;

  virtual PatternType get_pattern_type () const = 0;

protected:
  // Clone pattern implementation as pure virtual method
  virtual Pattern *clone_pattern_impl () const = 0;
};

} // namespace HIR
} // namespace Rust

#endif
