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

#ifndef RUST_SCOPE_H
#define RUST_SCOPE_H

#include "rust-system.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Analysis {

template <class T> class Scope
{
public:
  Scope () : scopeStack () {}

  ~Scope () {}

  bool Insert (std::string key, T val)
  {
    if (scopeStack.back ().find (key) != scopeStack.back ().end ())
      {
	return false;
      }

    scopeStack.back ().insert (std::make_pair (key, std::move (val)));
    return true;
  }

  bool Lookup (std::string key, T *result)
  {
    for (auto it = scopeStack.rbegin (); it != scopeStack.rend (); ++it)
      {
	auto lookup = it->find (key);
	if (lookup != it->end ())
	  {
	    *result = lookup->second;
	    return true;
	  }
      }
    return false;
  }

  void Push () { scopeStack.push_back ({}); }

  std ::map<std::string, T> Pop ()
  {
    auto toplevel = scopeStack.back ();
    scopeStack.pop_back ();
    return toplevel;
  }

  std ::map<std::string, T> Peek () { return scopeStack.back (); }

private:
  std::vector<std::map<std::string, T> > scopeStack;
};

} // namespace Analysis
} // namespace Rust

#endif // RUST_SCOPE_H
