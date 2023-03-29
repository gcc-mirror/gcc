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

#include "rust-system.h"
#include "rust-optional.h"
#include "selftest.h"

#if CHECKING_P

static void
rust_optional_create ()
{
  auto opt = Rust::Optional<int>::some (15);

  ASSERT_TRUE (opt.is_some ());
  ASSERT_EQ (opt.get (), 15);

  Rust::Optional<int> const_opt = Rust::Optional<int>::some (15);
  const int &value = const_opt.get ();

  ASSERT_EQ (value, 15);
}

static void
rust_optional_operators ()
{
  auto opt = Rust::Optional<int>::some (15);

  // as bool
  ASSERT_TRUE (opt);

  // deref
  ASSERT_EQ (*opt, 15);

  class Methodable
  {
  public:
    int method () { return 15; }
  };

  auto m_opt = Rust::Optional<Methodable>::some (Methodable ());
  ASSERT_EQ (m_opt->method (), 15);
}

static void
rust_optional_take ()
{
  auto opt = Rust::Optional<int>::some (15);
  auto value = opt.take ();

  ASSERT_EQ (value, 15);
  ASSERT_TRUE (opt.is_none ());
}

static void
rust_optional_map ()
{
  auto opt = Rust::Optional<int>::some (15);
  auto twice = opt.map<int> ([] (int value) { return value * 2; });

  ASSERT_FALSE (opt);
  ASSERT_TRUE (twice);
  ASSERT_EQ (*twice, 30);
}

static void
rust_optional_reference ()
{
  auto value = std::vector<std::string> ();
  value.emplace_back ("rust");
  value.emplace_back ("+");
  value.emplace_back ("gcc");
  value.emplace_back ("=");
  value.emplace_back ("<3");

  auto opt = Rust::Optional<std::vector<std::string> &>::some (value);

  ASSERT_EQ (opt->at (0), "rust");
  ASSERT_EQ (opt->at (2), "gcc");
}

#endif /* #if CHECKING_P */

void
rust_optional_test ()
{
#if CHECKING_P
  rust_optional_create ();
  rust_optional_operators ();
  rust_optional_take ();
  rust_optional_map ();
  rust_optional_reference ();

#endif /* #if CHECKING_P */
}
