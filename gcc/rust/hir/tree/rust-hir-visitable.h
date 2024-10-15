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

#ifndef RUST_HIR_VISITABLE_H
#define RUST_HIR_VISITABLE_H

namespace Rust {
namespace HIR {

class HIRFullVisitor;
class HIRTraitItemVisitor;
class HIRImplVisitor;
class HIRStmtVisitor;
class HIRExpressionVisitor;
class HIRTypeVisitor;
class HIRPatternVisitor;

class FullVisitable
{
public:
  virtual void accept_vis (HIRFullVisitor &vis) = 0;
};
} // namespace HIR
} // namespace Rust

#endif
