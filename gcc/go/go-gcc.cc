// go-gcc.cc -- Go frontend to gcc IR.
// Copyright (C) 2011 Free Software Foundation, Inc.
// Contributed by Ian Lance Taylor, Google.

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

#include "go-system.h"

// This has to be included outside of extern "C", so we have to
// include it here before tree.h includes it later.
#include <gmp.h>

#ifndef ENABLE_BUILD_WITH_CXX
extern "C"
{
#endif

#include "tree.h"

#ifndef ENABLE_BUILD_WITH_CXX
}
#endif

#include "backend.h"

// A class wrapping a tree.

class Gcc_tree
{
 public:
  Gcc_tree(tree t)
    : t_(t)
  { }

  tree
  get_tree()
  { return this->t_; }

 private:
  tree t_;
};

// In gcc, types, expressions, and statements are all trees.
class Btype : public Gcc_tree
{
 public:
  Btype(tree t)
    : Gcc_tree(t)
  { }
};

class Bexpression : public Gcc_tree
{
 public:
  Bexpression(tree t)
    : Gcc_tree(t)
  { }
};

class Bstatement : public Gcc_tree
{
 public:
  Bstatement(tree t)
    : Gcc_tree(t)
  { }
};

// This file implements the interface between the Go frontend proper
// and the gcc IR.  This implements specific instantiations of
// abstract classes defined by the Go frontend proper.  The Go
// frontend proper class methods of these classes to generate the
// backend representation.

class Gcc_backend : public Backend
{
 public:
  // Types.

  Btype*
  error_type()
  { gcc_unreachable(); }

  Btype*
  void_type()
  { gcc_unreachable(); }

  Btype*
  bool_type()
  { gcc_unreachable(); }

  Btype*
  integer_type(bool /* is_unsigned */, int /* bits */)
  { gcc_unreachable(); }

  Btype*
  float_type(int /* bits */)
  { gcc_unreachable(); }

  Btype*
  string_type()
  { gcc_unreachable(); }

  Btype*
  function_type(const Function_type*, Btype* /* receiver */,
		const Btypes* /* parameters */,
		const Btypes* /* results */)
  { gcc_unreachable(); }

  Btype*
  struct_type(const Struct_type*, const Btypes* /* field_types */)
  { gcc_unreachable(); }

  Btype*
  array_type(const Btype* /* element_type */, const Bexpression* /* length */)
  { gcc_unreachable(); }

  Btype*
  slice_type(const Btype* /* element_type */)
  { gcc_unreachable(); }

  Btype*
  map_type(const Btype* /* key_type */, const Btype* /* value_type */,
	   source_location)
  { gcc_unreachable(); }

  Btype*
  channel_type(const Btype* /* element_type */)
  { gcc_unreachable(); }

  Btype*
  interface_type(const Interface_type*, const Btypes* /* method_types */)
  { gcc_unreachable(); }

  // Statements.

  // Create an assignment statement.
  Bstatement*
  assignment(Bexpression* lhs, Bexpression* rhs,
	     source_location location);

 private:
  // Make a Bstatement from a tree.
  Bstatement*
  make_statement(tree t)
  { return new Bstatement(t); }
};

// Assignment.

Bstatement*
Gcc_backend::assignment(Bexpression* lhs, Bexpression* rhs,
			source_location location)
{
  return this->make_statement(fold_build2_loc(location, MODIFY_EXPR,
					      void_type_node,
					      lhs->get_tree(),
					      rhs->get_tree()));
}

// The single backend.

static Gcc_backend gcc_backend;

// Return the backend generator.

Backend*
go_get_backend()
{
  return &gcc_backend;
}

// FIXME: Temporary functions while converting to the new backend
// interface.

Bexpression*
tree_to_expr(tree t)
{
  return new Bexpression(t);
}

tree
statement_to_tree(Bstatement* bs)
{
  return bs->get_tree();
}
