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

#ifndef GCCRS_RUST_MACRO_BUILTINS_HELPERS_H
#define GCCRS_RUST_MACRO_BUILTINS_HELPERS_H
#include "rust-ast-fragment.h"
#include "rust-ast.h"
#include "rust-cfg-strip.h"
#include "rust-diagnostics.h"
#include "rust-early-name-resolver.h"
#include "rust-expr.h"
#include "rust-lex.h"
#include "rust-macro-builtins.h"
#include "rust-macro-invoc-lexer.h"
#include "rust-macro.h"
#include "rust-parse.h"
#include "rust-system.h"
#include "rust-token.h"
namespace Rust {

std::string
make_macro_path_str (BuiltinMacro kind);

std::vector<std::unique_ptr<AST::MacroInvocation>>
check_for_eager_invocations (
  std::vector<std::unique_ptr<AST::Expr>> &expressions);

// Shorthand function for creating unique_ptr tokens

std::unique_ptr<AST::Token>
make_token (const TokenPtr tok);

std::unique_ptr<AST::Expr>
make_string (location_t locus, std::string value);
// TODO: Is this correct?
AST::Fragment
make_eager_builtin_invocation (
  BuiltinMacro kind, location_t locus, AST::DelimTokenTree arguments,
  std::vector<std::unique_ptr<AST::MacroInvocation>> &&pending_invocations);
// Match the end token of a macro given the start delimiter of the macro
TokenId
macro_end_token (AST::DelimTokenTree &invoc_token_tree,
		 Parser<MacroInvocLexer> &parser);
// Expand and then extract a string literal from the macro
std::unique_ptr<AST::LiteralExpr>
try_extract_string_literal_from_fragment (const location_t &parent_locus,
					  std::unique_ptr<AST::Expr> &node);

std::vector<std::unique_ptr<AST::Expr>>
try_expand_many_expr (Parser<MacroInvocLexer> &parser,
		      const TokenId last_token_id, MacroExpander *expander,
		      bool &has_error);

// Parse a single string literal from the given delimited token tree,
//   and return the LiteralExpr for it. Allow for an optional trailing comma,
//   but otherwise enforce that these are the only tokens.

std::unique_ptr<AST::Expr>
parse_single_string_literal (BuiltinMacro kind,
			     AST::DelimTokenTree &invoc_token_tree,
			     location_t invoc_locus, MacroExpander *expander,
			     bool is_semicoloned = false);

// Treat PATH as a path relative to the source file currently being
// compiled, and return the absolute path for it.

std::string
source_relative_path (std::string path, location_t locus);

// Read the full contents of the file FILENAME and return them in a vector.
// FIXME: platform specific.
tl::optional<std::vector<uint8_t>>
load_file_bytes (location_t invoc_locus, const char *filename);
} // namespace Rust
#endif // GCCRS_RUST_MACRO_BUILTINS_HELPERS_H
