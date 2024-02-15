// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-ast.h"
#include "rust-macro-expand.h"

namespace Rust {
class SubstituteCtx
{
  std::vector<std::unique_ptr<AST::Token>> &input;
  std::vector<std::unique_ptr<AST::Token>> &macro;
  std::map<std::string, MatchedFragmentContainer *> &fragments;

  /**
   * Find the repetition amount to use when expanding a repetition, and
   * check that all fragments used respect that repetition amount
   *
   * @param pattern_start Start of the repetition pattern
   * @param pattern_end End of the repetition pattern
   * @param repeat_amount Reference to fill with the matched repetition amount
   */
  bool check_repetition_amount (size_t pattern_start, size_t pattern_end,
				size_t &repeat_amount);

public:
  SubstituteCtx (std::vector<std::unique_ptr<AST::Token>> &input,
		 std::vector<std::unique_ptr<AST::Token>> &macro,
		 std::map<std::string, MatchedFragmentContainer *> &fragments)
    : input (input), macro (macro), fragments (fragments)
  {}

  /**
   * Substitute a metavariable by its given fragment in a transcribing context,
   * i.e. replacing $var with the associated fragment.
   *
   * @param metavar Metavariable to try and replace
   * @param expanded Reference to a vector upon which expanded tokens will be
   * pushed
   *
   * @return True iff the substitution succeeded
   */
  bool substitute_metavar (std::unique_ptr<AST::Token> &metavar,
			   std::vector<std::unique_ptr<AST::Token>> &expanded);

  /**
   * Substitute a macro repetition by its given fragments
   *
   * @param pattern_start Start index of the pattern tokens
   * @param pattern_end End index of the patterns tokens
   * @param separator Optional separator to include when expanding tokens
   *
   * @return A vector containing the repeated pattern
   */
  std::vector<std::unique_ptr<AST::Token>>
  substitute_repetition (size_t pattern_start, size_t pattern_end,
			 std::unique_ptr<AST::Token> separator);

  /**
   * Substitute a given token by its appropriate representation
   *
   * @param token_idx Current token to try and substitute
   *
   * @return A token containing the associated fragment expanded into tokens if
   * any, or the cloned token if no fragment was associated, as well as the
   * amount of tokens that should be skipped before the next invocation. Since
   * this function may consume more than just one token, it is important to skip
   * ahead of the input to avoid mis-substitutions
   */
  std::pair<std::vector<std::unique_ptr<AST::Token>>, size_t>
  substitute_token (size_t token_idx);

  /**
   * Substitute all tokens by their appropriate representation
   *
   * @return A vector containing the substituted tokens
   */
  std::vector<std::unique_ptr<AST::Token>> substitute_tokens ();
};
} // namespace Rust
