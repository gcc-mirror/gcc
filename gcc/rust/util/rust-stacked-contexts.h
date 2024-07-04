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

#ifndef RUST_CONTEXT_STACK_H
#define RUST_CONTEXT_STACK_H

#include "rust-system.h"

namespace Rust {

/**
 * Context stack util class. This class is useful for situations where you can
 * enter the same kind of context multiple times. For example, when dealing with
 * unsafe contexts, you might be tempted to simply keep a boolean value.
 *
 * ```rust
 * let a = 15;
 * unsafe { // we set the boolean to true
 *     // Now unsafe operations are allowed!
 *     let b = *(&a as *const i32);
 *     let c = std::mem::transmute<i32, f32>(b); // Urgh!
 * } // we set it to false
 * ```
 *
 * However, since the language allows nested unsafe blocks, you may run into
 * this situation:
 *
 * ```rust
 * unsafe { // we set the boolean to true
 *     unsafe { // we set the boolean to true
 *     } // we set it to false
 *
 *     // Now unsafe operations are forbidden again, the boolean is false
 *     let f = std::mem::transmute<i32, f32>(15); // Error!
 * } // we set it to false
 * ```
 */
template <typename T> class StackedContexts
{
public:
  /**
   * Enter a special context
   */
  void enter (T value) { stack.emplace_back (value); }

  /**
   * Exit a special context
   */
  T exit ()
  {
    rust_assert (!stack.empty ());

    auto last = stack.back ();
    stack.pop_back ();

    return last;
  }

  /**
   * Are we currently inside of a special context?
   */
  bool is_in_context () const { return !stack.empty (); }

private:
  /* Actual data */
  std::vector<T> stack;
};

} // namespace Rust

#endif /* !RUST_CONTEXT_STACK_H */
