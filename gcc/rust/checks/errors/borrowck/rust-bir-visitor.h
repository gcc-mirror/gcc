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

#ifndef RUST_BIR_VISITOR_H
#define RUST_BIR_VISITOR_H

namespace Rust {
namespace BIR {

class Statement;
class InitializerExpr;
template <unsigned N> class Operator;
class Assignment;
class BorrowExpr;
class CallExpr;

class Visitor
{
public:
  virtual void visit (const Statement &stmt) = 0;
  virtual void visit (const InitializerExpr &expr) = 0;
  virtual void visit (const Operator<1> &expr) = 0;
  virtual void visit (const Operator<2> &expr) = 0;
  virtual void visit (const BorrowExpr &expr) = 0;
  virtual void visit (const Assignment &expr) = 0;
  virtual void visit (const CallExpr &expr) = 0;
};

class Visitable
{
public:
  virtual void accept_vis (Visitor &visitor) = 0;
};

template <typename BASE, typename T> class VisitableImpl : public BASE
{
public:
  template <typename... Args>
  explicit VisitableImpl (Args &&... args) : BASE (std::forward<Args> (args)...)
  {}

  void accept_vis (Visitor &visitor) override
  {
    visitor.visit (static_cast<T &> (*this));
  }
};

} // namespace BIR
} // namespace Rust

#endif // RUST_BIR_VISITOR_H
