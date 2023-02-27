// Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

// An improved implementation of the inline visitor.
// Original idea from https://members.accu.org/index.php/articles/2021

#ifndef RUST_INLINE_VISITOR
#define RUST_INLINE_VISITOR

namespace Rust {

// Wrapper for the target Visitor we're matching against.
// Consumes the final nullptr of the _args linked list.
template <typename TargetVisitor> struct EmptyVisitor : TargetVisitor
{
  EmptyVisitor (std::nullptr_t ptr) {}

  using TargetVisitor::visit;
};

// Wrapper for a (possibly incomplete) Visitor.
template <typename BaseVisitor, typename Args> struct VisitorWrapper
{
  // Lambdas are stored in _args as a linked list and passed to the actual
  // visitor when end_visitor() is called.
  Args _args;

  // The actual visitor being created.
  // Each visitor inherits from the last one and implements one more visit().
  template <typename T, typename F> struct Visitor : BaseVisitor
  {
    F _f;

    Visitor (std::pair<F, Args> &&args)
      : BaseVisitor (std::move (args.second)), _f (std::move (args.first))
    {}

    using BaseVisitor::visit;
    virtual void visit (T &t) final override { _f (t); }
  };

  VisitorWrapper (Args &&args) : _args (std::move (args)) {}

  // Add another visit() method to the visitor.
  // _args will be moved over, so don't keep the old wrapper around.
  template <typename T, typename F>
  VisitorWrapper<Visitor<T, F>, std::pair<F, Args>> on (F &&f)
  {
    return VisitorWrapper<Visitor<T, F>, std::pair<F, Args>> (
      std::make_pair (std::move (f), std::move (_args)));
  }

  // Returns the finished visitor.
  // NOTE: The reference implementation has a bug that exposes this method even
  // when BaseVisitor is still an abstract class. The C++11 standard states that
  // "An abstract class shall not be used [...] as a function return type". GCC
  // rejects the buggy code as expected, but Clang accepts the code as long as
  // the method is not actually called. Maybe this is a bug in Clang?
  template <typename T = BaseVisitor>
  typename std::enable_if<std::is_constructible<T, Args>::value, T>::type
  end_visitor ()
  {
    return T (std::move (_args));
  }
};

// The entry point.
template <typename TargetVisitor>
VisitorWrapper<EmptyVisitor<TargetVisitor>, std::nullptr_t>
begin_visitor ()
{
  return VisitorWrapper<EmptyVisitor<TargetVisitor>, std::nullptr_t> (nullptr);
}

} // namespace Rust

#endif
