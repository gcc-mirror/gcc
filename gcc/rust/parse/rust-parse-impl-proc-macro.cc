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

#include "rust-parse-impl.hxx"
#include "rust-proc-macro-invoc-lexer.h"

namespace Rust {

template tl::expected<std::unique_ptr<AST::Item>, Parse::Error::Item>
Parser<ProcMacroInvocLexer>::parse_item (bool);

template std::unique_ptr<AST::Stmt>
  Parser<ProcMacroInvocLexer>::parse_stmt (ParseRestrictions);

// instantiate entire class (or just more functions) if necessary

// template class Parser<ProcMacroInvocLexer>;

} // namespace Rust
