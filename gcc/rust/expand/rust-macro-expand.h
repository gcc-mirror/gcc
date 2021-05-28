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
#include "rust-macro.h"

// Provides objects and method prototypes for macro expansion

namespace Rust {
// forward decls for AST
namespace AST {
class MacroInvocation;
}

// Object used to store configuration data for macro expansion.
// NOTE: Keep all these items complying with the latest rustc.
struct ExpansionCfg
{
  // features?
  // TODO: Add `features' when we have it.
  unsigned int recursion_limit = 1024;
  bool trace_mac = false;   // trace macro
  bool should_test = false; // strip #[test] nodes if false
  bool keep_macs = false;   // keep macro definitions
  std::string crate_name = "";
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

  // Expands a single declarative macro.
  AST::ASTFragment expand_decl_macro (AST::MacroInvocData &invoc,
				      AST::MacroRulesDefinition &rules_def);

  void expand_cfg_attrs (AST::AttrVec &attrs);
  bool fails_cfg (const AST::AttrVec &attr) const;
  bool fails_cfg_with_expand (AST::AttrVec &attrs) const;

  // Expand the data of a cfg! macro.
  void parse_macro_to_meta_item (AST::MacroInvocData &invoc);
  // Get the literal representation of a cfg! macro.
  AST::Literal expand_cfg_macro (AST::MacroInvocData &invoc);

private:
  AST::Crate &crate;
  Session &session;
};
} // namespace Rust

#endif
