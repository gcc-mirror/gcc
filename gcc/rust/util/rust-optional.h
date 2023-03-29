// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_OPTIONAL_H
#define RUST_OPTIONAL_H

#include "config.h"
#include "rust-system.h"

#include "selftest.h"

namespace Rust {

/**
 * Tagged union to try and simulate a sum type. This is safer and more ergonomic
 * than one of the two alternatives we're currently using in the compiler:
 *
 * 1. Storing a raw pointer, which can be `nullptr` or valid
 *
 * This is wildly unsafe, and usable in conjunction with local references, stack
 * variables, or pointers managed elsewhere, which can cause crashes, hard to
 * debug issues or undefined behavior. Likewise, if you do not check for the
 * pointer's validity, this will cause a crash.
 *
 * 2. Storing an extra boolean alongside the object
 *
 * This causes implementors to use a "dummy object": Either an empty version or
 * an error version. But what happens if what you really wanted to store was
 * the empty or error version? You can also easily incorporate logic bugs if you
 * forget to check for the associated boolean.
 *
 * The `Optional<T>` type has the same "ergonomic" cost: You need to check
 * whether your option is valid or not. However, the main advantage is that it
 * is more restrictive: You can only acess the member it contains "safely".
 * It is similar to storing a value + an associated boolean, but has the
 * advantage of making up only one member in your class.
 * You also benefit from some helper methods such as `map()`.
 *
 * You also get helper functions and operator overloading to "seamlessly"
 * replace raw pointer alternatives.
 *
 * ```c++
 * MyType *raw_pointer = something_that_can_fail();
 * if (raw_pointer)
 *     raw_pointer->method();
 *
 * // or
 *
 * Optional<MyType> opt = something_that_can_fail2();
 * if (opt)
 *     opt->method();
 *
 * // equivalent to
 *
 * if (opt.is_some())
 *     opt.get().method();
 * ```
 */
template <typename T> class Optional
{
private:
  struct Empty
  {
  };

  enum Kind
  {
    Some,
    None
  } kind;

  union Content
  {
    Empty empty;
    T value;

    Content () = default;
  } content;

  Optional<T> (Kind kind, Content content) : kind (kind), content (content) {}

public:
  Optional (const Optional &other) = default;
  Optional &operator= (const Optional &other) = default;
  Optional (Optional &&other) = default;

  static Optional<T> some (T value)
  {
    Content content;
    content.value = value;

    return Optional (Kind::Some, content);
  }

  static Optional<T> none ()
  {
    Content content;
    content.empty = Empty ();

    return Optional (Kind::None, content);
  }

  bool is_some () const { return kind == Kind::Some; }
  bool is_none () const { return !is_some (); }

  /**
   * Enable boolean-like comparisons.
   */
  operator bool () { return is_some (); }

  /**
   * Enables dereferencing to access the contained value
   */
  T &operator* () { return get (); }
  const T &operator* () const { return get (); }
  T *operator-> () { return &get (); }
  const T *operator-> () const { return &get (); }

  const T &get () const
  {
    rust_assert (is_some ());

    return content.value;
  }

  T &get ()
  {
    rust_assert (is_some ());

    return content.value;
  }

  T take ()
  {
    rust_assert (is_some ());

    auto to_return = std::move (content.value);

    content.empty = Empty ();
    kind = Kind::None;

    return to_return;
  }

  template <typename U> Optional<U> map (std::function<U (T)> functor)
  {
    if (is_none ())
      return Optional::none ();

    auto value = functor (take ());

    return Optional::some (value);
  }
};

template <typename T> class Optional<T &>
{
private:
  struct Empty
  {
  };

  enum Kind
  {
    Some,
    None
  } kind;

  union Content
  {
    Empty empty;
    T *value;

    Content () = default;
  } content;

  Optional<T &> (Kind kind, Content content) : kind (kind), content (content) {}

public:
  Optional (const Optional &other) = default;
  Optional (Optional &&other) = default;
  Optional &operator= (Optional &&other) = default;

  static Optional<T &> some (T &value)
  {
    Content content;
    content.value = &value;

    return Optional (Kind::Some, content);
  }

  static Optional<T &> none ()
  {
    Content content;
    content.empty = Empty ();

    return Optional (Kind::None, content);
  }

  bool is_some () const { return kind == Kind::Some; }
  bool is_none () const { return !is_some (); }

  // FIXME: Can we factor this in a single class?

  /**
   * Enable boolean-like comparisons.
   */
  operator bool () { return is_some (); }

  /**
   * Enables dereferencing to access the contained value
   */
  T &operator* () { return get (); }
  const T &operator* () const { return get (); }
  T *operator-> () { return &get (); }
  const T *operator-> () const { return &get (); }

  const T &get () const
  {
    rust_assert (is_some ());

    return *content.value;
  }

  T &get ()
  {
    rust_assert (is_some ());

    return *content.value;
  }

  T &take ()
  {
    rust_assert (is_some ());

    auto to_return = std::move (content.value);

    content.empty = Empty ();
    kind = Kind::None;

    return *to_return;
  }

  template <typename U> Optional<U &> map (std::function<U &(T &)> functor)
  {
    if (is_none ())
      return Optional::none ();

    auto value = functor (take ());

    return Optional::some (value);
  }
};

} // namespace Rust

#ifdef CHECKING_P

void
rust_optional_test ();

#endif // !CHECKING_P

#endif // !RUST_OPTIONAL_H
