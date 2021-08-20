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

#ifndef RUST_CANONICAL_PATH
#define RUST_CANONICAL_PATH

#include "rust-system.h"
#include "rust-mapping-common.h"

namespace Rust {
namespace Resolver {

// https://doc.rust-lang.org/reference/paths.html#canonical-paths
//
// struct X - path X
// impl X { fn test - path X::test }
//
// struct X<T> - path X
//
// impl X<T>   { fn test - path X::test}
// impl X<i32> { fn test - path X<i32>::test }
// impl X<f32> { fn test - path X<f32>::test }
//
// pub trait Trait { // ::a::Trait
//   fn f(&self); // ::a::Trait::f
// }
//
// impl Trait for Struct {
//    fn f(&self) {} // <::a::Struct as ::a::Trait>::f
// }
class CanonicalPath
{
public:
  CanonicalPath (const CanonicalPath &other) : segs (other.segs) {}

  CanonicalPath &operator= (const CanonicalPath &other)
  {
    segs = other.segs;
    return *this;
  }

  static CanonicalPath new_seg (NodeId id, const std::string &path)
  {
    rust_assert (!path.empty ());
    return CanonicalPath ({std::pair<NodeId, std::string> (id, path)});
  }

  std::string get () const
  {
    std::string buf;
    for (size_t i = 0; i < segs.size (); i++)
      {
	bool have_more = (i + 1) < segs.size ();
	const std::string &seg = segs.at (i).second;
	buf += seg + (have_more ? "::" : "");
      }
    return buf;
  }

  static CanonicalPath get_big_self (NodeId id)
  {
    return CanonicalPath::new_seg (id, "Self");
  }

  static CanonicalPath create_empty () { return CanonicalPath ({}); }

  bool is_empty () const { return segs.size () == 0; }

  CanonicalPath append (const CanonicalPath &other) const
  {
    rust_assert (!other.is_empty ());
    if (is_empty ())
      return CanonicalPath (other.segs);

    std::vector<std::pair<NodeId, std::string>> copy (segs);
    for (auto &s : other.segs)
      copy.push_back (s);

    return CanonicalPath (copy);
  }

  // if we have the path A::B::C this will give a callback for each segment
  // example:
  //   A
  //   A::B
  //   A::B::C
  void iterate (std::function<bool (const CanonicalPath &)> cb) const
  {
    std::vector<std::pair<NodeId, std::string>> buf;
    for (auto &seg : segs)
      {
	buf.push_back (seg);
	if (!cb (CanonicalPath (buf)))
	  return;
      }
  }

  void iterate_segs (std::function<bool (const CanonicalPath &)> cb) const
  {
    for (auto &seg : segs)
      {
	std::vector<std::pair<NodeId, std::string>> buf;
	buf.push_back ({seg.first, seg.second});
	if (!cb (CanonicalPath (buf)))
	  return;
      }
  }

  size_t size () const { return segs.size (); }

  NodeId get_id () const
  {
    rust_assert (!segs.empty ());
    return segs.back ().first;
  }

  bool is_equal (const CanonicalPath &b) const
  {
    return get ().compare (b.get ()) == 0;
  }

  bool operator== (const CanonicalPath &b) const { return is_equal (b); }

  bool operator< (const CanonicalPath &b) const { return get () < b.get (); }

private:
  explicit CanonicalPath (std::vector<std::pair<NodeId, std::string>> path)
    : segs (path)
  {}

  std::vector<std::pair<NodeId, std::string>> segs;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_CANONICAL_PATH
