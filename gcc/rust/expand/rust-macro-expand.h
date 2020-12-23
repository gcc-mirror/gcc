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

#ifndef RUST_MACRO_EXPAND_H
#define RUST_MACRO_EXPAND_H

#include "rust-ast.h"

// Provides objects and method prototypes for macro expansion

namespace Rust {
// forward decls for AST
namespace AST {
class MacroInvocation;
}

// Object used to store configuration data for macro expansion.
struct ExpansionCfg
{
  // features?
  unsigned int recursion_limit = 50; // TODO: determine default recursion limit
				// trace macros?
				// should test?
				// more default stuff?
};

// Object used to store shared data (between functions) for macro expansion.
struct MacroExpander
{
  ExpansionCfg cfg;
  unsigned int expansion_depth = 0;

  MacroExpander (AST::Crate &crate, ExpansionCfg cfg, Session &session)
    : cfg (cfg), crate (crate), session (session)
  {}

  ~MacroExpander () = default;

  // Expands all macros in the crate passed in.
  void expand_crate ();

  /* Expands a macro invocation (not macro invocation semi) - possibly make both
   * have similar duck-typed interface and use templates?*/
  // should this be public or private?
  void expand_invoc (std::unique_ptr<AST::MacroInvocation> &invoc);

  void expand_cfg_attrs (std::vector<AST::Attribute> &attrs);
  bool fails_cfg (std::vector<AST::Attribute> &attr);

  /* TODO: make it extend ASTVisitor so that individual items can be accessed
   * properly? */

private:
  AST::Crate &crate;
  Session &session;
};
} // namespace Rust

#endif
