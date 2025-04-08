// Copyright (C) 2025 Free Software Foundation, Inc.

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

#ifndef RUST_TOKEN_TREE_DESUGAR_H
#define RUST_TOKEN_TREE_DESUGAR_H

#include "rust-ast-visitor.h"
#include "rust-system.h"
#include "rust-ast.h"

namespace Rust {
namespace AST {

/**
 * Desugar a given token-tree before parsing it for a macro invocation. At the
 * moment, the sole purpose of this desugar is to transform doc-comments into
 * their attribute form (/// comment -> #[doc = "comment"])
 */
class TokenTreeDesugar : public DefaultASTVisitor
{
public:
  TokenTreeDesugar () : desugared (std::vector<std::unique_ptr<TokenTree>> ())
  {}

  DelimTokenTree go (DelimTokenTree &tts);

private:
  std::vector<std::unique_ptr<TokenTree>> desugared;
  void append (TokenPtr &&new_token);
  void append (std::unique_ptr<TokenTree> &&new_token);

  using DefaultASTVisitor::visit;

  virtual void visit (Token &tts) override;
};

}; // namespace AST
}; // namespace Rust

#endif //! RUST_TOKEN_TREE_DESUGAR_H
