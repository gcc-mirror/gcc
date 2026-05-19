// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
namespace TyTy {
class BaseType;
}
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

  virtual ~Pattern () {}

  // Syntactic refutability. ICEs for patterns whose refutability depends
  // on type context (Path, Range, Slice, Alt). Callers that might
  // encounter such patterns must use the typed overload.
  virtual bool is_refutable () const = 0;
  // Type-aware refutability. Defaults to the syntactic answer for
  // patterns whose refutability is independent of the scrutinee type.

  // Virtual method overriden by classes that enable this.
  virtual bool
  is_refutable (const TyTy::BaseType &scrutinee ATTRIBUTE_UNUSED) const
  {
    return is_refutable ();
  }

  virtual std::string to_string () const = 0;

  std::string to_debug_string () const
  {
    return to_string () + get_mappings ().as_string ();
  }

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
