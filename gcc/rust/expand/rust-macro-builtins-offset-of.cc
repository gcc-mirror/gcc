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

#include "optional.h"
#include "rust-ast-fragment.h"
#include "rust-ast.h"
#include "rust-builtin-ast-nodes.h"
#include "rust-diagnostics.h"
#include "rust-macro-builtins-helpers.h"
#include "rust-macro-builtins.h"
#include "rust-macro-invoc-lexer.h"
#include "rust-parse.h"

namespace Rust {

tl::optional<AST::Fragment>
MacroBuiltin::offset_of_handler (location_t invoc_locus,
				 AST::MacroInvocData &invoc,
				 AST::InvocKind semicolon)
{
  MacroInvocLexer lex (invoc.get_delim_tok_tree ().to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  auto last_token = macro_end_token (invoc.get_delim_tok_tree (), parser);

  auto type = parser.parse_type ();

  // if we don't see a type, there might be an eager macro expansion missing
  // FIXME: handle that
  if (!type)
    {
      rust_error_at (invoc_locus, "could not parse type argument for %qs",
		     "offset_of");

      // we skip so we can still parse the field arg and check if it is correct
      while (parser.peek_current_token ()->get_id () != COMMA
	     && parser.peek_current_token ()->get_id () != last_token)
	parser.skip_token ();
    }

  parser.skip_token (COMMA);

  auto field_tok = parser.parse_identifier_or_keyword_token ();
  auto invalid_field = !field_tok || !field_tok.value ()->should_have_str ();

  if (invalid_field)
    rust_error_at (invoc_locus, "could not parse field argument for %qs",
		   "offset_of");

  if (!type || invalid_field)
    return tl::nullopt;

  auto field = Identifier (field_tok.value ()->get_str ());

  // FIXME: Do we need to do anything to handle the optional comma at the end?
  parser.maybe_skip_token (COMMA);

  return AST::Fragment ({AST::SingleASTNode (std::make_unique<AST::OffsetOf> (
			  std::move (type), field, invoc_locus))},
			invoc.get_delim_tok_tree ().to_token_stream ());
}

} // namespace Rust
