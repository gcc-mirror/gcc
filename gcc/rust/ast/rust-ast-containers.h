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

#ifndef RUST_AST_CONTAINERS_H
#define RUST_AST_CONTAINERS_H
// crappy redefined AST maybe. may move

/* This is mainly the "logical", more "abstract" representation of the code,
 * while the "AST" itself is more high-level and matches the language better. */

// this is now deprecated and replaced with the proper AST
#error "rust-ast-containers.h was included by accident. Don't use."

namespace Rust {
namespace AST {
struct Module
{
public:
};

struct Crate
{
public:
  Module root_module;
};

// replace with rust-types.h version
struct AttributeList
{
public:
  //::std::vector<Attribute> attribs;
};

// replace with rust-types.h version
struct Visibility
{
};

/*enum VisibilityType {
    Private,
    PublicFull,
    PublicInPath,
    PublicCrate,
    PublicSuper,
    PublicSelfModule
};

// Represents visibility - maybe make into an enum or union or something
struct Visibility {
};

*/
} // namespace AST
} // namespace Rust

#endif
