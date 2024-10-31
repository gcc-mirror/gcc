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
#ifndef RUST_AST_DUMP_H
#define RUST_AST_DUMP_H

#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-ast-full.h"
#include "rust-dump.h"
#include "rust-system.h"

#include "rust-ast-collector.h"

namespace Rust {
namespace AST {

class Dump
{
public:
  Dump (std::ostream &stream);
  Dump (std::ostream &stream, bool print_internal,
	std::set<std::string> excluded_node);

  /**
   * Run the visitor on an entire crate and its items
   */
  void go (AST::Crate &crate);
  void go (AST::Item &item);

  template <typename T> void process (T &v)
  {
    TokenCollector collector;
    collector.visit (v);

    TokenPtr previous = nullptr;
    for (auto item : collector.collect ())
      {
	switch (item.get_kind ())
	  {
	  case AST::CollectItem::Kind::Token:
	    {
	      TokenPtr current = item.get_token ();
	      if (require_spacing (previous, current))
		stream << " ";
	      stream << current->as_string ();
	      previous = current;
	    }
	    break;
	  case AST::CollectItem::Kind::Comment:
	    stream << " /* " << item.get_comment () << " */ ";
	    break;
	  case AST::CollectItem::Kind::Indentation:
	    for (size_t i = 0; i < item.get_indent_level (); i++)
	      {
		stream << "    ";
	      }
	    break;
	  case AST::CollectItem::Kind::Newline:
	    stream << "\n";
	    previous = nullptr;
	    break;
	  case AST::CollectItem::Kind::InternalComment:
	    if (print_internal)
	      {
		bool is_excluded = false;
		std::string comment = item.get_internal_comment ();
		for (auto &node : excluded_node)
		  {
		    if (comment.find (node) != std::string::npos)
		      {
			is_excluded = true;
			break;
		      }
		  }
		if (!is_excluded)
		  stream << " /* " << comment << " */ ";
	      }
	    break;
	  default:
	    rust_unreachable ();
	  }
      }
  }

  // Helper method to get a quick debug dump to standard error output
  static void debug (Visitable &v);

private:
  std::ostream &stream;
  Indent indentation;
  bool print_internal;
  std::set<std::string> excluded_node;

  static bool require_spacing (TokenPtr previous, TokenPtr current);
};

} // namespace AST
} // namespace Rust

// In the global namespace to make it easier to call from debugger
void debug (Rust::AST::Visitable &v);

#endif // !RUST_AST_DUMP_H
