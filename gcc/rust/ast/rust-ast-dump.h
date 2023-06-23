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
#ifndef RUST_AST_DUMP_H
#define RUST_AST_DUMP_H

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-ast-full.h"
#include "rust-dump.h"

#include "rust-ast-collector.h"

namespace Rust {
namespace AST {

class Dump
{
public:
  Dump (std::ostream &stream);

  /**
   * Run the visitor on an entire crate and its items
   */
  void go (AST::Crate &crate);
  void go (AST::Item &item);

  template <typename T> void process (T &v)
  {
    TokenCollector collector;
    collector.visit (v);

    auto tokens = collector.collect_tokens ();
    if (!tokens.empty ())
      stream << tokens.front ()->as_string ();
    for (auto it = tokens.cbegin () + 1; it < tokens.cend (); it++)
      {
	if (require_spacing (*(it - 1), *it))
	  stream << " ";
	stream << (*it)->as_string ();
      }
  }

  // Helper method to get a quick debug dump to standard error output
  static void debug (Visitable &v);

private:
  std::ostream &stream;
  Indent indentation;

  static bool require_spacing (TokenPtr previous, TokenPtr current);
};

} // namespace AST
} // namespace Rust

// In the global namespace to make it easier to call from debugger
void
debug (Rust::AST::Visitable &v);

#endif // !RUST_AST_DUMP_H
