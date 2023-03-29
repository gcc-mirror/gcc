/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#include "rust-cfg-parser.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-session-manager.h"
#include "selftest.h"

namespace Rust {
bool
parse_cfg_option (std::string &input, std::string &key, std::string &value)
{
  key.clear ();
  value.clear ();

  auto lexer = Lexer (input);
  auto parser = Parser<Lexer> (lexer);

  auto token = parser.peek_current_token ();
  if (token->get_id () != IDENTIFIER)
    {
      return false;
    }

  key = token->get_str ();

  rust_assert (parser.skip_token (IDENTIFIER));
  token = parser.peek_current_token ();

  switch (token->get_id ())
    {
    case END_OF_FILE:
      // we're done parsing, we had a valid key, return happily
      return true;
    case EQUAL:
      // We have an equal sign: Skip the token and parse an identifier
      {
	rust_assert (parser.skip_token (EQUAL));

	auto value_expr = parser.parse_literal_expr ();
	// We had an equal sign but no value, error out
	if (!value_expr)
	  return false;

	if (value_expr->get_lit_type () != AST::Literal::LitType::STRING)
	  return false;

	value = value_expr->get_literal ().as_string ();
	return true;
      }
    default:
      return false;
    }
}
} // namespace Rust

#if CHECKING_P

namespace selftest {

void
rust_cfg_parser_test (void)
{
  std::string key;
  std::string value;

  auto input = std::string ("key_no_value");

  ASSERT_TRUE (Rust::parse_cfg_option (input, key, value));
  ASSERT_EQ (key, "key_no_value");
  ASSERT_TRUE (value.empty ());

  input = std::string ("k=\"v\"");

  ASSERT_TRUE (Rust::parse_cfg_option (input, key, value));
  ASSERT_EQ (key, "k");
  ASSERT_EQ (value, "v");

  // values should be between double quotes
  input = std::string ("k=v");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  // No value is an error if there is an equal sign
  input = std::string ("k=");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  // No key is an error
  input = std::string ("=");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  input = std::string ("=value");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  // values that are not string literals are an error
  input = std::string ("key=b\"a\"");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  input = std::string ("key='v'");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  input = std::string ("key=155");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  input = std::string ("key=3.14");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));

  // kebab case is not valid for an identifier
  input = std::string ("key-no-value");
  ASSERT_FALSE (Rust::parse_cfg_option (input, key, value));
}
} // namespace selftest

#endif // CHECKING_P
