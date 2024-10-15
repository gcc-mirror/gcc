
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

#ifndef RUST_HIR_ATTRS_H
#define RUST_HIR_ATTRS_H

#include "rust-ast.h"

namespace Rust {
namespace HIR {

class WithOuterAttrs
{
protected:
  AST::AttrVec outer_attrs;

public:
  AST::AttrVec &get_outer_attrs () { return outer_attrs; }
  const AST::AttrVec &get_outer_attrs () const { return outer_attrs; }

  WithOuterAttrs (AST::AttrVec outer_attrs)
    : outer_attrs (std::move (outer_attrs)){};
};

class WithInnerAttrs
{
protected:
  AST::AttrVec inner_attrs;

public:
  AST::AttrVec get_inner_attrs () const { return inner_attrs; }

  WithInnerAttrs (AST::AttrVec inner_attrs)
    : inner_attrs (std::move (inner_attrs)){};
};

} // namespace HIR
} // namespace Rust

#endif
