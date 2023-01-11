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

#include "rust-system.h"
#include "rust-lex.h"
#include "rust-diagnostics.h"
#include "rust-linemap.h"
#include "rust-session-manager.h"
#include "safe-ctype.h"

namespace Rust {
// TODO: move to separate compilation unit?
// overload += for uint32_t to allow 32-bit encoded utf-8 to be added
std::string &
operator+= (std::string &str, Codepoint char32)
{
  if (char32.value < 0x80)
    {
      str += static_cast<char> (char32.value);
    }
  else if (char32.value < (0x1F + 1) << (1 * 6))
    {
      str += static_cast<char> (0xC0 | ((char32.value >> 6) & 0x1F));
      str += static_cast<char> (0x80 | ((char32.value >> 0) & 0x3F));
    }
  else if (char32.value < (0x0F + 1) << (2 * 6))
    {
      str += static_cast<char> (0xE0 | ((char32.value >> 12) & 0x0F));
      str += static_cast<char> (0x80 | ((char32.value >> 6) & 0x3F));
      str += static_cast<char> (0x80 | ((char32.value >> 0) & 0x3F));
    }
  else if (char32.value < (0x07 + 1) << (3 * 6))
    {
      str += static_cast<char> (0xF0 | ((char32.value >> 18) & 0x07));
      str += static_cast<char> (0x80 | ((char32.value >> 12) & 0x3F));
      str += static_cast<char> (0x80 | ((char32.value >> 6) & 0x3F));
      str += static_cast<char> (0x80 | ((char32.value >> 0) & 0x3F));
    }
  else
    {
      rust_debug ("Invalid unicode codepoint found: '%u' ", char32.value);
    }
  return str;
}

std::string
Codepoint::as_string ()
{
  std::string str;

  // str += Codepoint (value);
  str += *this;

  return str;
}

/* Includes all allowable float digits EXCEPT _ and . as that needs lookahead
 * for handling. */
bool
is_float_digit (char number)
{
  return ISDIGIT (number) || number == 'E' || number == 'e';
}

/* Basically ISXDIGIT from safe-ctype but may change if Rust's encoding or
 * whatever is different */
bool
is_x_digit (char number)
{
  return ISXDIGIT (number);
}

bool
is_octal_digit (char number)
{
  return number >= '0' && number <= '7';
}

bool
is_bin_digit (char number)
{
  return number == '0' || number == '1';
}

bool
check_valid_float_dot_end (char character)
{
  return character != '.' && character != '_' && !ISALPHA (character);
}

// ISSPACE from safe-ctype but may change in future
bool
is_whitespace (char character)
{
  return ISSPACE (character);
}

bool
is_non_decimal_int_literal_separator (char character)
{
  return character == 'x' || character == 'o' || character == 'b';
}

Lexer::Lexer (const std::string &input)
  : input (RAIIFile::create_error ()), current_line (1), current_column (1),
    line_map (nullptr), dump_lex_out (Optional<std::ofstream &>::none ()),
    raw_input_source (new BufferInputSource (input, 0)),
    input_queue{*raw_input_source}, token_queue (TokenSource (this))
{}

Lexer::Lexer (const char *filename, RAIIFile file_input, Linemap *linemap,
	      Optional<std::ofstream &> dump_lex_opt)
  : input (std::move (file_input)), current_line (1), current_column (1),
    line_map (linemap), dump_lex_out (dump_lex_opt),
    raw_input_source (new FileInputSource (input.get_raw ())),
    input_queue{*raw_input_source}, token_queue (TokenSource (this))
{
  // inform line_table that file is being entered and is in line 1
  if (linemap)
    line_map->start_file (filename, current_line);
}

Lexer::~Lexer ()
{
  /* ok apparently stop (which is equivalent of original code in destructor) is
   * meant to be called after all files have finished parsing, for cleanup. On
   * the other hand, actual code that it calls to leave a certain line map is
   * mentioned in GCC docs as being useful for "just leaving an included header"
   * and stuff like that, so this line mapping functionality may need fixing.
   * FIXME: find out whether this occurs. */

  // line_map->stop();
}

/* TODO: need to optimise somehow to avoid the virtual function call in the
 * tight loop. Best idea at the moment is CRTP, but that might make lexer
 * implementation annoying when storing the "base class" (i.e. would need
 * template parameter everywhere), although in practice it would mostly just
 * look ugly and make enclosing classes like Parser also require a type
 * parameter. At this point a macro might be better. OK I guess macros can be
 * replaced by constexpr if or something if possible. */
Location
Lexer::get_current_location ()
{
  if (line_map)
    return line_map->get_location (current_column);
  else
    // If we have no linemap, we're lexing something without proper locations
    return Location ();
}

int
Lexer::peek_input (int n)
{
  return input_queue.peek (n);
}

int
Lexer::peek_input ()
{
  return peek_input (0);
}

void
Lexer::skip_input (int n)
{
  input_queue.skip (n);
}

void
Lexer::skip_input ()
{
  skip_input (0);
}

void
Lexer::skip_token (int n)
{
  // dump tokens if dump-lex option is enabled
  if (dump_lex_out.is_some ())
    dump_and_skip (n);
  else
    token_queue.skip (n);
}

void
Lexer::dump_and_skip (int n)
{
  std::ofstream &out = dump_lex_out.get ();
  bool found_eof = false;
  const_TokenPtr tok;
  for (int i = 0; i < n + 1; i++)
    {
      if (!found_eof)
	{
	  tok = peek_token ();
	  found_eof |= tok->get_id () == Rust::END_OF_FILE;

	  Location loc = tok->get_locus ();

	  out << "<id=";
	  out << tok->token_id_to_str ();
	  out << (tok->has_str () ? (std::string (", text=") + tok->get_str ()
				     + std::string (", typehint=")
				     + std::string (tok->get_type_hint_str ()))
				  : "")
	      << " ";
	  out << get_line_map ()->to_string (loc) << " ";
	}

      token_queue.skip (0);
    }
}

void
Lexer::replace_current_token (TokenPtr replacement)
{
  token_queue.replace_current_value (replacement);

  rust_debug ("called 'replace_current_token' - this is deprecated");
}

/* shitty anonymous namespace that can only be accessed inside the compilation
 * unit - used for classify_keyword binary search in sorted array of keywords
 * created with x-macros. */
namespace {
// TODO: make constexpr when update to c++20
const std::string keyword_index[] = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(name, keyword) keyword,
  RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
};

constexpr TokenId keyword_keys[] = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(name, keyword) name,
  RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
};

constexpr int num_keywords = sizeof (keyword_index) / sizeof (*keyword_index);
} // namespace

/* Determines whether the string passed in is a keyword or not. If it is, it
 * returns the keyword name.  */
TokenId
Lexer::classify_keyword (const std::string &str)
{
  const std::string *last = keyword_index + num_keywords;
  const std::string *idx = std::lower_bound (keyword_index, last, str);

  if (idx == last || str != *idx)
    return IDENTIFIER;

  // TODO: possibly replace this x-macro system with something like hash map?

  // We now have the expected token ID of the reserved keyword. However, some
  // keywords are reserved starting in certain editions. For example, `try` is
  // only a reserved keyword in editions >=2018. The language might gain new
  // reserved keywords in the future.
  //
  // https://doc.rust-lang.org/reference/keywords.html#reserved-keywords
  auto id = keyword_keys[idx - keyword_index];

  // `try` is not a reserved keyword before 2018
  if (Session::get_instance ().options.get_edition ()
	== CompileOptions::Edition::E2015
      && id == TRY)
    return IDENTIFIER;

  return id;
}

TokenPtr
Lexer::build_token ()
{
  // loop to go through multiple characters to build a single token
  while (true)
    {
      Location loc = get_current_location ();
      current_char = peek_input ();
      skip_input ();

      // detect UTF8 bom
      //
      // Must be the first thing on the first line.
      // There might be an optional BOM (Byte Order Mark), which for UTF-8 is
      // the three bytes 0xEF, 0xBB and 0xBF. These can simply be skipped.
      if (current_line == 1 && current_column == 1 && current_char == 0xef
	  && peek_input () == 0xbb && peek_input (1) == 0xbf)
	{
	  skip_input (1);
	  current_char = peek_input ();
	  skip_input ();
	}

      // detect shebang
      // Must be the first thing on the first line, starting with #!
      // But since an attribute can also start with an #! we don't count it as a
      // shebang line when after any whitespace or comments there is a [. If it
      // is a shebang line we simple drop the line. Otherwise we don't consume
      // any characters and fall through to the real tokenizer.
      if (current_line == 1 && current_column == 1 && current_char == '#'
	  && peek_input () == '!')
	{
	  int n = 1;
	  while (true)
	    {
	      int next_char = peek_input (n);
	      if (is_whitespace (next_char))
		n++;
	      else if ((next_char == '/' && peek_input (n + 1) == '/'
			&& peek_input (n + 2) != '!'
			&& peek_input (n + 2) != '/')
		       || (next_char == '/' && peek_input (n + 1) == '/'
			   && peek_input (n + 2) == '/'
			   && peek_input (n + 3) == '/'))
		{
		  // two // or four ////
		  // A single line comment
		  // (but not an inner or outer doc comment)
		  n += 2;
		  next_char = peek_input (n);
		  while (next_char != '\n' && next_char != EOF)
		    {
		      n++;
		      next_char = peek_input (n);
		    }
		  if (next_char == '\n')
		    n++;
		}
	      else if (next_char == '/' && peek_input (n + 1) == '*'
		       && peek_input (n + 2) == '*'
		       && peek_input (n + 3) == '/')
		{
		  /**/
		  n += 4;
		}
	      else if (next_char == '/' && peek_input (n + 1) == '*'
		       && peek_input (n + 2) == '*' && peek_input (n + 3) == '*'
		       && peek_input (n + 4) == '/')
		{
		  /***/
		  n += 5;
		}
	      else if ((next_char == '/' && peek_input (n + 1) == '*'
			&& peek_input (n + 2) != '*'
			&& peek_input (n + 2) != '!')
		       || (next_char == '/' && peek_input (n + 1) == '*'
			   && peek_input (n + 2) == '*'
			   && peek_input (n + 3) == '*'))
		{
		  // one /* or three /***
		  // Start of a block comment
		  // (but not an inner or outer doc comment)
		  n += 2;
		  int level = 1;
		  while (level > 0)
		    {
		      if (peek_input (n) == EOF)
			break;
		      else if (peek_input (n) == '/'
			       && peek_input (n + 1) == '*')
			{
			  n += 2;
			  level += 1;
			}
		      else if (peek_input (n) == '*'
			       && peek_input (n + 1) == '/')
			{
			  n += 2;
			  level -= 1;
			}
		      else
			n++;
		    }
		}
	      else if (next_char != '[')
		{
		  // definitely shebang, ignore the first line
		  while (current_char != '\n' && current_char != EOF)
		    {
		      current_char = peek_input ();
		      skip_input ();
		    }

		  // newline
		  current_line++;
		  current_column = 1;
		  // tell line_table that new line starts
		  start_line (current_line, max_column_hint);
		  break;
		}
	      else
		break; /* Definitely not a shebang line. */
	    }
	}

      // return end of file token if end of file
      if (current_char == EOF)
	return Token::make (END_OF_FILE, loc);

      // if not end of file, start tokenising
      switch (current_char)
	{
	/* ignore whitespace characters for tokens but continue updating
	 * location */
	case '\n': // newline
	  current_line++;
	  current_column = 1;
	  // tell line_table that new line starts
	  start_line (current_line, max_column_hint);
	  continue;
	case '\r': // cr
	  // Ignore, we expect a newline (lf) soon.
	  continue;
	case ' ': // space
	  current_column++;
	  continue;
	case '\t': // tab
	  // width of a tab is not well-defined, assume 8 spaces
	  current_column += 8;
	  continue;

	// punctuation - actual tokens
	case '=':
	  if (peek_input () == '>')
	    {
	      // match arm arrow
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (MATCH_ARROW, loc);
	    }
	  else if (peek_input () == '=')
	    {
	      // equality operator
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (EQUAL_EQUAL, loc);
	    }
	  else
	    {
	      // assignment operator
	      current_column++;
	      return Token::make (EQUAL, loc);
	    }
	case '(':
	  current_column++;
	  return Token::make (LEFT_PAREN, loc);
	case '-':
	  if (peek_input () == '>')
	    {
	      // return type specifier
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (RETURN_TYPE, loc);
	    }
	  else if (peek_input () == '=')
	    {
	      // minus-assign
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (MINUS_EQ, loc);
	    }
	  else
	    {
	      // minus
	      current_column++;
	      return Token::make (MINUS, loc);
	    }
	case '+':
	  if (peek_input () == '=')
	    {
	      // add-assign
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (PLUS_EQ, loc);
	    }
	  else
	    {
	      // add
	      current_column++;
	      return Token::make (PLUS, loc);
	    }
	case ')':
	  current_column++;
	  return Token::make (RIGHT_PAREN, loc);
	case ';':
	  current_column++;
	  return Token::make (SEMICOLON, loc);
	case '*':
	  if (peek_input () == '=')
	    {
	      // multiplication-assign
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (ASTERISK_EQ, loc);
	    }
	  else
	    {
	      // multiplication
	      current_column++;
	      return Token::make (ASTERISK, loc);
	    }
	case ',':
	  current_column++;
	  return Token::make (COMMA, loc);
	case '/':
	  if (peek_input () == '=')
	    {
	      // division-assign
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (DIV_EQ, loc);
	    }
	  else if ((peek_input () == '/' && peek_input (1) != '!'
		    && peek_input (1) != '/')
		   || (peek_input () == '/' && peek_input (1) == '/'
		       && peek_input (2) == '/'))
	    {
	      // two // or four ////
	      // single line comment
	      // (but not an inner or outer doc comment)
	      skip_input ();
	      current_column += 2;
	      current_char = peek_input ();

	      // basically ignore until line finishes
	      while (current_char != '\n' && current_char != EOF)
		{
		  skip_input ();
		  current_column++; // not used
		  current_char = peek_input ();
		}
	      continue;
	    }
	  else if (peek_input () == '/'
		   && (peek_input (1) == '!' || peek_input (1) == '/'))
	    {
	      /* single line doc comment, inner or outer.  */
	      bool is_inner = peek_input (1) == '!';
	      skip_input (1);
	      current_column += 3;

	      std::string str;
	      str.reserve (32);
	      current_char = peek_input ();
	      while (current_char != '\n')
		{
		  skip_input ();
		  if (current_char == '\r')
		    {
		      char next_char = peek_input ();
		      if (next_char == '\n')
			{
			  current_char = '\n';
			  break;
			}
		      rust_error_at (
			loc, "Isolated CR %<\\r%> not allowed in doc comment");
		      current_char = next_char;
		      continue;
		    }
		  if (current_char == EOF)
		    {
		      rust_error_at (
			loc, "unexpected EOF while looking for end of comment");
		      break;
		    }
		  str += current_char;
		  current_char = peek_input ();
		}
	      skip_input ();
	      current_line++;
	      current_column = 1;
	      // tell line_table that new line starts
	      start_line (current_line, max_column_hint);

	      str.shrink_to_fit ();

	      loc += str.size () - 1;
	      if (is_inner)
		return Token::make_inner_doc_comment (loc, std::move (str));
	      else
		return Token::make_outer_doc_comment (loc, std::move (str));
	    }
	  else if (peek_input () == '*' && peek_input (1) == '*'
		   && peek_input (2) == '/')
	    {
	      /**/
	      skip_input (2);
	      current_column += 4;
	      continue;
	    }
	  else if (peek_input () == '*' && peek_input (1) == '*'
		   && peek_input (2) == '*' && peek_input (3) == '/')
	    {
	      /***/
	      skip_input (3);
	      current_column += 5;
	      continue;
	    }
	  else if ((peek_input () == '*' && peek_input (1) != '!'
		    && peek_input (1) != '*')
		   || (peek_input () == '*' && peek_input (1) == '*'
		       && peek_input (2) == '*'))
	    {
	      // one /* or three /***
	      // block comment
	      // (but not an inner or outer doc comment)
	      skip_input ();
	      current_column += 2;

	      int level = 1;
	      while (level > 0)
		{
		  current_char = peek_input ();

		  if (current_char == EOF)
		    {
		      rust_error_at (
			loc, "unexpected EOF while looking for end of comment");
		      break;
		    }

		  // if /* found
		  if (current_char == '/' && peek_input (1) == '*')
		    {
		      // skip /* characters
		      skip_input (1);

		      current_column += 2;

		      level += 1;
		      continue;
		    }

		  // ignore until */ is found
		  if (current_char == '*' && peek_input (1) == '/')
		    {
		      // skip */ characters
		      skip_input (1);

		      current_column += 2;

		      level -= 1;
		      continue;
		    }

		  if (current_char == '\n')
		    {
		      skip_input ();
		      current_line++;
		      current_column = 1;
		      // tell line_table that new line starts
		      start_line (current_line, max_column_hint);
		      continue;
		    }

		  skip_input ();
		  current_column++;
		}

	      // refresh new token
	      continue;
	    }
	  else if (peek_input () == '*'
		   && (peek_input (1) == '!' || peek_input (1) == '*'))
	    {
	      // block doc comment, inner /*! or outer /**
	      bool is_inner = peek_input (1) == '!';
	      skip_input (1);
	      current_column += 3;

	      std::string str;
	      str.reserve (96);

	      int level = 1;
	      while (level > 0)
		{
		  current_char = peek_input ();

		  if (current_char == EOF)
		    {
		      rust_error_at (
			loc, "unexpected EOF while looking for end of comment");
		      break;
		    }

		  // if /* found
		  if (current_char == '/' && peek_input (1) == '*')
		    {
		      // skip /* characters
		      skip_input (1);
		      current_column += 2;

		      level += 1;
		      str += "/*";
		      continue;
		    }

		  // ignore until */ is found
		  if (current_char == '*' && peek_input (1) == '/')
		    {
		      // skip */ characters
		      skip_input (1);
		      current_column += 2;

		      level -= 1;
		      if (level > 0)
			str += "*/";
		      continue;
		    }

		  if (current_char == '\r' && peek_input (1) != '\n')
		    rust_error_at (
		      loc, "Isolated CR %<\\r%> not allowed in doc comment");

		  if (current_char == '\n')
		    {
		      skip_input ();
		      current_line++;
		      current_column = 1;
		      // tell line_table that new line starts
		      start_line (current_line, max_column_hint);
		      str += '\n';
		      continue;
		    }

		  str += current_char;
		  skip_input ();
		  current_column++;
		}

	      str.shrink_to_fit ();

	      loc += str.size () - 1;
	      if (is_inner)
		return Token::make_inner_doc_comment (loc, std::move (str));
	      else
		return Token::make_outer_doc_comment (loc, std::move (str));
	    }
	  else
	    {
	      // division
	      current_column++;
	      return Token::make (DIV, loc);
	    }
	case '%':
	  if (peek_input () == '=')
	    {
	      // modulo-assign
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (PERCENT_EQ, loc);
	    }
	  else
	    {
	      // modulo
	      current_column++;
	      return Token::make (PERCENT, loc);
	    }
	case '^':
	  if (peek_input () == '=')
	    {
	      // xor-assign?
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (CARET_EQ, loc);
	    }
	  else
	    {
	      // xor?
	      current_column++;
	      return Token::make (CARET, loc);
	    }
	case '<':
	  if (peek_input () == '<')
	    {
	      if (peek_input (1) == '=')
		{
		  // left-shift assign
		  skip_input (1);
		  current_column += 3;
		  loc += 2;

		  return Token::make (LEFT_SHIFT_EQ, loc);
		}
	      else
		{
		  // left-shift
		  skip_input ();
		  current_column += 2;
		  loc += 1;

		  return Token::make (LEFT_SHIFT, loc);
		}
	    }
	  else if (peek_input () == '=')
	    {
	      // smaller than or equal to
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (LESS_OR_EQUAL, loc);
	    }
	  else
	    {
	      // smaller than
	      current_column++;
	      return Token::make (LEFT_ANGLE, loc);
	    }
	  break;
	case '>':
	  if (peek_input () == '>')
	    {
	      if (peek_input (1) == '=')
		{
		  // right-shift-assign
		  skip_input (1);
		  current_column += 3;
		  loc += 2;

		  return Token::make (RIGHT_SHIFT_EQ, loc);
		}
	      else
		{
		  // right-shift
		  skip_input ();
		  current_column += 2;
		  loc += 1;

		  return Token::make (RIGHT_SHIFT, loc);
		}
	    }
	  else if (peek_input () == '=')
	    {
	      // larger than or equal to
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (GREATER_OR_EQUAL, loc);
	    }
	  else
	    {
	      // larger than
	      current_column++;
	      return Token::make (RIGHT_ANGLE, loc);
	    }
	case ':':
	  if (peek_input () == ':')
	    {
	      // scope resolution ::
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (SCOPE_RESOLUTION, loc);
	    }
	  else
	    {
	      // single colon :
	      current_column++;
	      return Token::make (COLON, loc);
	    }
	case '!':
	  // no special handling for macros in lexer?
	  if (peek_input () == '=')
	    {
	      // not equal boolean operator
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (NOT_EQUAL, loc);
	    }
	  else
	    {
	      // not equal unary operator
	      current_column++;

	      return Token::make (EXCLAM, loc);
	    }
	case '?':
	  current_column++;
	  return Token::make (QUESTION_MARK, loc);
	case '#':
	  current_column++;
	  return Token::make (HASH, loc);
	case '[':
	  current_column++;
	  return Token::make (LEFT_SQUARE, loc);
	case ']':
	  current_column++;
	  return Token::make (RIGHT_SQUARE, loc);
	case '{':
	  current_column++;
	  return Token::make (LEFT_CURLY, loc);
	case '}':
	  current_column++;
	  return Token::make (RIGHT_CURLY, loc);
	case '@':
	  current_column++;
	  return Token::make (PATTERN_BIND, loc);
	case '$':
	  current_column++;
	  return Token::make (DOLLAR_SIGN, loc);
	case '~':
	  current_column++;
	  return Token::make (TILDE, loc);
	case '\\':
	  current_column++;
	  return Token::make (BACKSLASH, loc);
	case '`':
	  current_column++;
	  return Token::make (BACKTICK, loc);
	case '|':
	  if (peek_input () == '=')
	    {
	      // bitwise or-assign?
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (PIPE_EQ, loc);
	    }
	  else if (peek_input () == '|')
	    {
	      // logical or
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (OR, loc);
	    }
	  else
	    {
	      // bitwise or
	      current_column++;

	      return Token::make (PIPE, loc);
	    }
	case '&':
	  if (peek_input () == '=')
	    {
	      // bitwise and-assign?
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (AMP_EQ, loc);
	    }
	  else if (peek_input () == '&')
	    {
	      // logical and
	      skip_input ();
	      current_column += 2;
	      loc += 1;

	      return Token::make (LOGICAL_AND, loc);
	    }
	  else
	    {
	      // bitwise and/reference
	      current_column++;

	      return Token::make (AMP, loc);
	    }
	case '.':
	  if (peek_input () == '.')
	    {
	      if (peek_input (1) == '.')
		{
		  // ellipsis
		  skip_input (1);
		  current_column += 3;
		  loc += 2;

		  return Token::make (ELLIPSIS, loc);
		}
	      else if (peek_input (1) == '=')
		{
		  // ..=
		  skip_input (1);
		  current_column += 3;
		  loc += 2;

		  return Token::make (DOT_DOT_EQ, loc);
		}
	      else
		{
		  // ..
		  skip_input ();
		  current_column += 2;
		  loc += 1;

		  return Token::make (DOT_DOT, loc);
		}
	    }
	  else /*if (!ISDIGIT (peek_input ()))*/
	    {
	      // single dot .
	      // Only if followed by a non-number - otherwise is float
	      // nope, float cannot start with '.'.
	      current_column++;
	      return Token::make (DOT, loc);
	    }
	}
      // TODO: special handling of _ in the lexer? instead of being identifier

      // byte character, byte string and raw byte string literals
      if (current_char == 'b')
	{
	  if (peek_input () == '\'')
	    return parse_byte_char (loc);
	  else if (peek_input () == '"')
	    return parse_byte_string (loc);
	  else if (peek_input () == 'r'
		   && (peek_input (1) == '#' || peek_input (1) == '"'))
	    return parse_raw_byte_string (loc);
	}

      // raw identifiers and raw strings
      if (current_char == 'r')
	{
	  int peek = peek_input ();
	  int peek1 = peek_input (1);

	  if (peek == '#' && (ISALPHA (peek1) || peek1 == '_'))
	    {
	      TokenPtr raw_ident_ptr = parse_raw_identifier (loc);
	      if (raw_ident_ptr != nullptr)
		return raw_ident_ptr;
	      else
		continue; /* input got parsed, it just wasn't valid. An error
			     was produced. */
	    }
	  else
	    {
	      TokenPtr maybe_raw_string_ptr = maybe_parse_raw_string (loc);
	      if (maybe_raw_string_ptr != nullptr)
		return maybe_raw_string_ptr;
	    }
	}

      // find identifiers and keywords
      if (ISALPHA (current_char) || current_char == '_')
	return parse_identifier_or_keyword (loc);

      // int and float literals
      if (ISDIGIT (current_char))
	{ //  _ not allowed as first char
	  if (current_char == '0'
	      && is_non_decimal_int_literal_separator (peek_input ()))
	    {
	      // handle binary, octal, hex literals
	      TokenPtr non_dec_int_lit_ptr
		= parse_non_decimal_int_literals (loc);
	      if (non_dec_int_lit_ptr != nullptr)
		return non_dec_int_lit_ptr;
	    }
	  else
	    {
	      // handle decimals (integer or float)
	      TokenPtr decimal_or_float_ptr = parse_decimal_int_or_float (loc);
	      if (decimal_or_float_ptr != nullptr)
		return decimal_or_float_ptr;
	    }
	}

      // string literals
      if (current_char == '"')
	return parse_string (loc);

      // char literals and lifetime names
      if (current_char == '\'')
	{
	  TokenPtr char_or_lifetime_ptr = parse_char_or_lifetime (loc);
	  if (char_or_lifetime_ptr != nullptr)
	    return char_or_lifetime_ptr;
	}

      // DEBUG: check for specific character problems:
      if (current_char == '0')
	rust_debug ("'0' uncaught before unexpected character");
      else if (current_char == ']')
	rust_debug ("']' uncaught before unexpected character");
      else if (current_char == 0x5d)
	rust_debug ("whatever 0x5d is (not '0' or ']') uncaught before "
		    "unexpected character");

      // didn't match anything so error
      rust_error_at (loc, "unexpected character %<%x%>", current_char);
      current_column++;
    }
}

// Parses in a type suffix.
std::pair<PrimitiveCoreType, int>
Lexer::parse_in_type_suffix ()
{
  std::string suffix;
  suffix.reserve (5);

  int additional_length_offset = 0;

  // get suffix
  while (ISALPHA (current_char) || ISDIGIT (current_char)
	 || current_char == '_')
    {
      if (current_char == '_')
	{
	  // don't add _ to suffix
	  skip_input ();
	  current_char = peek_input ();

	  additional_length_offset++;

	  continue;
	}

      additional_length_offset++;

      suffix += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  if (suffix.empty ())
    {
      // no type suffix: do nothing but also no error
      return std::make_pair (CORETYPE_UNKNOWN, additional_length_offset);
    }
  else if (suffix == "f32")
    {
      return std::make_pair (CORETYPE_F32, additional_length_offset);
    }
  else if (suffix == "f64")
    {
      return std::make_pair (CORETYPE_F64, additional_length_offset);
    }
  else if (suffix == "i8")
    {
      return std::make_pair (CORETYPE_I8, additional_length_offset);
    }
  else if (suffix == "i16")
    {
      return std::make_pair (CORETYPE_I16, additional_length_offset);
    }
  else if (suffix == "i32")
    {
      return std::make_pair (CORETYPE_I32, additional_length_offset);
    }
  else if (suffix == "i64")
    {
      return std::make_pair (CORETYPE_I64, additional_length_offset);
    }
  else if (suffix == "i128")
    {
      return std::make_pair (CORETYPE_I128, additional_length_offset);
    }
  else if (suffix == "isize")
    {
      return std::make_pair (CORETYPE_ISIZE, additional_length_offset);
    }
  else if (suffix == "u8")
    {
      return std::make_pair (CORETYPE_U8, additional_length_offset);
    }
  else if (suffix == "u16")
    {
      return std::make_pair (CORETYPE_U16, additional_length_offset);
    }
  else if (suffix == "u32")
    {
      return std::make_pair (CORETYPE_U32, additional_length_offset);
    }
  else if (suffix == "u64")
    {
      return std::make_pair (CORETYPE_U64, additional_length_offset);
    }
  else if (suffix == "u128")
    {
      return std::make_pair (CORETYPE_U128, additional_length_offset);
    }
  else if (suffix == "usize")
    {
      return std::make_pair (CORETYPE_USIZE, additional_length_offset);
    }
  else
    {
      rust_error_at (get_current_location (), "unknown number suffix %qs",
		     suffix.c_str ());

      return std::make_pair (CORETYPE_UNKNOWN, additional_length_offset);
    }
}

// Parses in the exponent part (if any) of a float literal.
std::pair<std::string, int>
Lexer::parse_in_exponent_part ()
{
  int additional_length_offset = 0;
  std::string str;
  if (current_char == 'E' || current_char == 'e')
    {
      // add exponent to string as strtod works with it
      str += current_char;
      skip_input ();
      current_char = peek_input ();

      additional_length_offset++;

      // special - and + handling
      if (current_char == '-')
	{
	  str += '-';

	  skip_input ();
	  current_char = peek_input ();

	  additional_length_offset++;
	}
      else if (current_char == '+')
	{
	  // don't add + but still skip input
	  skip_input ();
	  current_char = peek_input ();

	  additional_length_offset++;
	}

      // parse another decimal number for exponent
      auto str_length = parse_in_decimal ();
      str += std::get<0> (str_length);
      additional_length_offset += std::get<1> (str_length);
    }
  return std::make_pair (str, additional_length_offset);
}

// Parses a decimal integer.
std::tuple<std::string, int, bool>
Lexer::parse_in_decimal ()
{
  /* A pure decimal contains only digits.  */
  bool pure_decimal = true;
  int additional_length_offset = 0;
  std::string str;
  while (ISDIGIT (current_char) || current_char == '_')
    {
      if (current_char == '_')
	{
	  pure_decimal = false;
	  // don't add _ to number
	  skip_input ();
	  current_char = peek_input ();

	  additional_length_offset++;

	  continue;
	}

      additional_length_offset++;

      str += current_char;
      skip_input ();
      current_char = peek_input ();
    }
  return std::make_tuple (str, additional_length_offset, pure_decimal);
}

/* Parses escapes (and string continues) in "byte" strings and characters. Does
 * not support unicode. */
std::tuple<char, int, bool>
Lexer::parse_escape (char opening_char)
{
  int additional_length_offset = 0;
  char output_char = 0;

  // skip to actual letter
  skip_input ();
  current_char = peek_input ();
  additional_length_offset++;

  switch (current_char)
    {
      case 'x': {
	auto hex_escape_pair = parse_partial_hex_escape ();
	long hexLong = hex_escape_pair.first;
	additional_length_offset += hex_escape_pair.second;

	if (hexLong > 255 || hexLong < 0)
	  rust_error_at (
	    get_current_location (),
	    "byte \\x escape %<\\x%x%> out of range - allows up to %<\\xFF%>",
	    static_cast<unsigned int> (hexLong));
	/* TODO: restore capital for escape output - gcc pretty-printer doesn't
	 * support %X directly */
	char hexChar = static_cast<char> (hexLong);

	output_char = hexChar;
      }
      break;
    case 'n':
      output_char = '\n';
      break;
    case 'r':
      output_char = '\r';
      break;
    case 't':
      output_char = '\t';
      break;
    case '\\':
      output_char = '\\';
      break;
    case '0':
      output_char = '\0';
      break;
    case '\'':
      output_char = '\'';
      break;
    case '"':
      output_char = '"';
      break;
    case 'u':
      rust_error_at (get_current_location (),
		     "cannot have a unicode escape \\u in a byte %s",
		     opening_char == '\'' ? "character" : "string");
      // Try to parse it anyway, just to skip it
      parse_partial_unicode_escape ();
      return std::make_tuple (output_char, additional_length_offset, false);
    case '\r':
    case '\n':
      // string continue
      return std::make_tuple (0, parse_partial_string_continue (), true);
    default:
      rust_error_at (get_current_location (),
		     "unknown escape sequence %<\\%c%>", current_char);
      // returns false if no parsing could be done
      // return false;
      return std::make_tuple (output_char, additional_length_offset, false);
      break;
    }
  // all non-special cases (string continue) should skip their used char
  skip_input ();
  current_char = peek_input ();
  additional_length_offset++;

  // returns true if parsing was successful
  // return true;
  return std::make_tuple (output_char, additional_length_offset, false);
}

/* Parses an escape (or string continue) in a string or character. Supports
 * unicode escapes. */
std::tuple<Codepoint, int, bool>
Lexer::parse_utf8_escape ()
{
  Codepoint output_char;
  int additional_length_offset = 0;

  // skip to actual letter
  skip_input ();
  current_char = peek_input ();
  additional_length_offset++;

  switch (current_char)
    {
      case 'x': {
	auto hex_escape_pair = parse_partial_hex_escape ();
	long hexLong = hex_escape_pair.first;
	additional_length_offset += hex_escape_pair.second;

	if (hexLong > 127 || hexLong < 0)
	  rust_error_at (
	    get_current_location (),
	    "ascii \\x escape %<\\x%x%> out of range - allows up to %<\\x7F%>",
	    static_cast<unsigned int> (hexLong));
	/* TODO: restore capital for escape output - gcc pretty-printer doesn't
	 * support %X directly */
	char hexChar = static_cast<char> (hexLong);

	output_char = hexChar;
      }
      break;
    case 'n':
      output_char = '\n';
      break;
    case 'r':
      output_char = '\r';
      break;
    case 't':
      output_char = '\t';
      break;
    case '\\':
      output_char = '\\';
      break;
    case '0':
      output_char = '\0';
      break;
    case '\'':
      output_char = '\'';
      break;
    case '"':
      output_char = '"';
      break;
      case 'u': {
	auto unicode_escape_pair = parse_partial_unicode_escape ();
	output_char = unicode_escape_pair.first;
	additional_length_offset += unicode_escape_pair.second;

	return std::make_tuple (output_char, additional_length_offset, false);
      }
      break;
    case '\r':
    case '\n':
      // string continue
      return std::make_tuple (0, parse_partial_string_continue (), true);
    default:
      rust_error_at (get_current_location (),
		     "unknown escape sequence %<\\%c%>", current_char);
      // returns false if no parsing could be done
      // return false;
      return std::make_tuple (output_char, additional_length_offset, false);
      break;
    }
  /* all non-special cases (unicode, string continue) should skip their used
   * char */
  skip_input ();
  current_char = peek_input ();
  additional_length_offset++;

  // returns true if parsing was successful
  // return true;
  return std::make_tuple (output_char, additional_length_offset, false);
}

// Parses the body of a string continue that has been found in an escape.
int
Lexer::parse_partial_string_continue ()
{
  int additional_length_offset = 1;

  // string continue
  while (is_whitespace (current_char))
    {
      if (current_char == '\n')
	{
	  current_line++;
	  current_column = 1;
	  // tell line_table that new line starts
	  start_line (current_line, max_column_hint);

	  // reset "length"
	  additional_length_offset = 1;

	  // get next char
	  skip_input ();
	  current_char = peek_input ();

	  continue;
	}

      skip_input ();
      current_char = peek_input ();
      additional_length_offset++;
    }

  return additional_length_offset;
}

/* Parses the body of a '\x' escape. Note that it does not check that the number
 * is valid and smaller than 255. */
std::pair<long, int>
Lexer::parse_partial_hex_escape ()
{
  // hex char string (null-terminated)
  char hexNum[3] = {0, 0, 0};

  // first hex char
  current_char = peek_input (1);
  int additional_length_offset = 1;

  if (!is_x_digit (current_char))
    {
      rust_error_at (get_current_location (),
		     "invalid character %<\\x%c%> in \\x sequence",
		     current_char);
      return std::make_pair (0, 0);
    }
  hexNum[0] = current_char;

  // second hex char
  skip_input ();
  current_char = peek_input (1);
  additional_length_offset++;

  if (!is_x_digit (current_char))
    {
      rust_error_at (get_current_location (),
		     "invalid character %<\\x%c%c%> in \\x sequence", hexNum[0],
		     current_char);
      return std::make_pair (0, 1);
    }
  skip_input ();
  hexNum[1] = current_char;

  long hexLong = std::strtol (hexNum, nullptr, 16);

  return std::make_pair (hexLong, additional_length_offset);
}

// Parses the body of a unicode escape.
std::pair<Codepoint, int>
Lexer::parse_partial_unicode_escape ()
{
  skip_input ();
  current_char = peek_input ();
  int additional_length_offset = 0;

  if (current_char != '{')
    {
      rust_error_at (get_current_location (),
		     "unicode escape should start with %<{%>");
      /* Skip what should probaby have been between brackets.  */
      while (is_x_digit (current_char) || current_char == '_')
	{
	  skip_input ();
	  current_char = peek_input ();
	  additional_length_offset++;
	}
      return std::make_pair (Codepoint (0), additional_length_offset);
    }

  skip_input ();
  current_char = peek_input ();
  additional_length_offset++;

  if (current_char == '_')
    {
      rust_error_at (get_current_location (),
		     "unicode escape cannot start with %<_%>");
      skip_input ();
      current_char = peek_input ();
      additional_length_offset++;
      // fallthrough and try to parse the rest anyway
    }

  // parse unicode escape - 1-6 hex digits
  std::string num_str;
  num_str.reserve (6);

  // loop through to add entire hex number to string
  while (is_x_digit (current_char) || current_char == '_')
    {
      if (current_char == '_')
	{
	  // don't add _ to number
	  skip_input ();
	  current_char = peek_input ();

	  additional_length_offset++;

	  continue;
	}

      additional_length_offset++;

      // add raw hex numbers
      num_str += current_char;

      skip_input ();
      current_char = peek_input ();
    }

  if (current_char == '}')
    {
      skip_input ();
      current_char = peek_input ();
      additional_length_offset++;
    }
  else
    {
      // actually an error, but allow propagation anyway Assume that
      // wrong bracketm whitespace or single/double quotes are wrong
      // termination, otherwise it is a wrong character, then skip to the actual
      // terminator.
      if (current_char == '{' || is_whitespace (current_char)
	  || current_char == '\'' || current_char == '"')
	{
	  rust_error_at (get_current_location (),
			 "expected terminating %<}%> in unicode escape");
	  return std::make_pair (Codepoint (0), additional_length_offset);
	}
      else
	{
	  rust_error_at (get_current_location (),
			 "invalid character %<%c%> in unicode escape",
			 current_char);
	  while (current_char != '}' && current_char != '{'
		 && !is_whitespace (current_char) && current_char != '\''
		 && current_char != '"')
	    {
	      skip_input ();
	      current_char = peek_input ();
	      additional_length_offset++;
	    }
	  // Consume the actual closing bracket if found
	  if (current_char == '}')
	    {
	      skip_input ();
	      current_char = peek_input ();
	      additional_length_offset++;
	    }
	  return std::make_pair (Codepoint (0), additional_length_offset);
	}
    }

  // ensure 1-6 hex characters
  if (num_str.length () > 6 || num_str.length () < 1)
    {
      rust_error_at (get_current_location (),
		     "unicode escape should be between 1 and 6 hex "
		     "characters; it is %lu",
		     (unsigned long) num_str.length ());
      // return false;
      return std::make_pair (Codepoint (0), additional_length_offset);
    }

  unsigned long hex_num = std::strtoul (num_str.c_str (), nullptr, 16);

  if (hex_num > 0xd7ff && hex_num < 0xe000)
    {
      rust_error_at (
	get_current_location (),
	"unicode escape cannot be a surrogate value (D800 to DFFF)");
      return std::make_pair (Codepoint (0), additional_length_offset);
    }

  if (hex_num > 0x10ffff)
    {
      rust_error_at (get_current_location (),
		     "unicode escape cannot be larger than 10FFFF");
      return std::make_pair (Codepoint (0), additional_length_offset);
    }

  // return true;
  return std::make_pair (Codepoint (static_cast<uint32_t> (hex_num)),
			 additional_length_offset);
}

// Parses a byte character.
TokenPtr
Lexer::parse_byte_char (Location loc)
{
  skip_input ();
  current_column++;
  // make current char the next character
  current_char = peek_input ();

  int length = 1;

  // char to save
  char byte_char = 0;

  // detect escapes
  if (current_char == '\\')
    {
      auto escape_length_pair = parse_escape ('\'');
      byte_char = std::get<0> (escape_length_pair);
      length += std::get<1> (escape_length_pair);

      current_char = peek_input ();

      if (current_char != '\'')
	{
	  rust_error_at (get_current_location (), "unclosed %<byte char%>");
	}

      skip_input ();
      current_char = peek_input ();
      length++; // go to next char
    }
  else if (current_char != '\'')
    {
      // otherwise, get character from direct input character
      byte_char = current_char;

      skip_input ();
      current_char = peek_input ();
      length++;

      if (current_char != '\'')
	{
	  rust_error_at (get_current_location (), "unclosed %<byte char%>");
	}

      skip_input ();
      current_char = peek_input ();
      length++; // go to next char
    }
  else
    {
      rust_error_at (get_current_location (),
		     "no character inside %<%> for %<byte char%>");
    }

  current_column += length;

  loc += length - 1;

  return Token::make_byte_char (loc, byte_char);
}

// Parses a byte string.
TokenPtr
Lexer::parse_byte_string (Location loc)
{
  // byte string

  // skip quote character
  skip_input ();
  current_column++;

  std::string str;
  str.reserve (16); // some sensible default

  int length = 1;
  current_char = peek_input ();

  while (current_char != '"' && current_char != EOF)
    {
      if (current_char == '\\')
	{
	  auto escape_length_pair = parse_escape ('"');
	  char output_char = std::get<0> (escape_length_pair);

	  if (output_char == 0 && std::get<2> (escape_length_pair))
	    length = std::get<1> (escape_length_pair) - 1;
	  else
	    length += std::get<1> (escape_length_pair);

	  if (output_char != 0 || !std::get<2> (escape_length_pair))
	    str += output_char;

	  continue;
	}

      length++;

      str += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  current_column += length;

  if (current_char == '"')
    {
      current_column++;

      skip_input ();
      current_char = peek_input ();
    }
  else if (current_char == EOF)
    {
      rust_error_at (get_current_location (), "unended byte string literal");
      return Token::make (END_OF_FILE, get_current_location ());
    }
  else
    {
      gcc_unreachable ();
    }

  str.shrink_to_fit ();
  loc += str.size () - 1;

  return Token::make_byte_string (loc, std::move (str));
}

// Parses a raw byte string.
TokenPtr
Lexer::parse_raw_byte_string (Location loc)
{
  // raw byte string literals
  std::string str;
  str.reserve (16); // some sensible default

  int length = 1;
  int hash_count = 0;

  // get hash count at beginnning
  skip_input ();
  current_char = peek_input ();
  length++;
  while (current_char == '#')
    {
      hash_count++;
      length++;

      skip_input ();
      current_char = peek_input ();
    }

  if (current_char != '"')
    {
      rust_error_at (get_current_location (),
		     "raw byte string has no opening %<\"%>");
    }

  skip_input ();
  current_char = peek_input ();
  length++;

  while (true)
    {
      if (current_char == '"')
	{
	  bool enough_hashes = true;

	  for (int i = 0; i < hash_count; i++)
	    {
	      if (peek_input (i + 1) != '#')
		{
		  enough_hashes = false;
		  break;
		}
	    }

	  if (enough_hashes)
	    {
	      // skip enough input and peek enough input
	      skip_input (hash_count);
	      current_char = peek_input ();
	      length += hash_count + 1;
	      break;
	    }
	}

      if ((unsigned char) current_char > 127)
	{
	  rust_error_at (get_current_location (),
			 "character %<%c%> in raw byte string out of range",
			 current_char);
	  current_char = 0;
	}

      length++;

      str += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  current_column += length;

  loc += length - 1;

  str.shrink_to_fit ();

  return Token::make_byte_string (loc, std::move (str));
}

// Parses a raw identifier.
TokenPtr
Lexer::parse_raw_identifier (Location loc)
{
  // raw identifier
  std::string str;
  str.reserve (16); // default

  skip_input ();
  current_char = peek_input ();

  current_column += 2;

  bool first_is_underscore = current_char == '_';

  int length = 0;
  current_char = peek_input ();
  // loop through entire name
  while (ISALPHA (current_char) || ISDIGIT (current_char)
	 || current_char == '_')
    {
      length++;

      str += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  current_column += length;

  // if just a single underscore, not an identifier
  if (first_is_underscore && length == 1)
    rust_error_at (get_current_location (),
		   "%<_%> is not a valid raw identifier");

  if (str == "crate" || str == "extern" || str == "self" || str == "super"
      || str == "Self")
    {
      rust_error_at (get_current_location (),
		     "%qs is a forbidden raw identifier", str.c_str ());

      return nullptr;
    }
  else
    {
      str.shrink_to_fit ();
      loc += length - 1;

      return Token::make_identifier (loc, std::move (str));
    }
}

// skip broken string input (unterminated strings)
void
Lexer::skip_broken_string_input (int current_char)
{
  while (current_char != '"' && current_char != EOF)
    {
      if (current_char == '\n')
	{
	  current_line++;
	  current_column = 1;
	}
      else
	{
	  current_column++;
	}
      skip_input ();
      current_char = peek_input ();
    }
  if (current_char == '"')
    {
      current_column++;

      skip_input ();
      current_char = peek_input ();
    }
  rust_debug ("skipped to %d:%d due to bad quotes", current_line,
	      current_column);
}

// Parses a unicode string.
TokenPtr
Lexer::parse_string (Location loc)
{
  Codepoint current_char32;

  std::string str;
  str.reserve (16); // some sensible default

  int length = 1;
  current_char32 = peek_codepoint_input ();

  // FIXME: This fails if the input ends. How do we check for EOF?
  while (current_char32.value != '"' && !current_char32.is_eof ())
    {
      if (current_char32.value == '\\')
	{
	  // parse escape
	  auto utf8_escape_pair = parse_utf8_escape ();
	  current_char32 = std::get<0> (utf8_escape_pair);

	  if (current_char32 == Codepoint (0) && std::get<2> (utf8_escape_pair))
	    length = std::get<1> (utf8_escape_pair) - 1;
	  else
	    length += std::get<1> (utf8_escape_pair);

	  if (current_char32 != Codepoint (0)
	      || !std::get<2> (utf8_escape_pair))
	    str += current_char32;

	  // required as parsing utf8 escape only changes current_char
	  current_char32 = peek_codepoint_input ();

	  continue;
	}

      length += get_input_codepoint_length ();

      str += current_char32;
      skip_codepoint_input ();
      current_char32 = peek_codepoint_input ();
    }

  current_column += length;

  if (current_char32.value == '"')
    {
      current_column++;

      skip_input ();
      current_char = peek_input ();
    }
  else if (current_char32.is_eof ())
    {
      rust_error_at (get_current_location (), "unended string literal");
      return Token::make (END_OF_FILE, get_current_location ());
    }
  else
    {
      gcc_unreachable ();
    }

  str.shrink_to_fit ();
  loc += length - 1;

  return Token::make_string (loc, std::move (str));
}

// Parses an identifier or keyword.
TokenPtr
Lexer::parse_identifier_or_keyword (Location loc)
{
  std::string str;
  str.reserve (16); // default
  str += current_char;

  bool first_is_underscore = current_char == '_';

  int length = 1;
  current_char = peek_input ();
  // loop through entire name
  while (ISALPHA (current_char) || ISDIGIT (current_char)
	 || current_char == '_')
    {
      length++;

      str += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  current_column += length;

  // if just a single underscore, not an identifier
  if (first_is_underscore && length == 1)
    return Token::make (UNDERSCORE, loc);

  str.shrink_to_fit ();

  loc += length - 1;

  TokenId keyword = classify_keyword (str);
  if (keyword == IDENTIFIER)
    return Token::make_identifier (loc, std::move (str));
  else
    return Token::make (keyword, loc);
}

// Possibly returns a raw string token if it exists - otherwise returns null.
TokenPtr
Lexer::maybe_parse_raw_string (Location loc)
{
  int peek_index = 0;
  while (peek_input (peek_index) == '#')
    peek_index++;

  if (peek_input (peek_index) == '"')
    return parse_raw_string (loc, peek_index);
  else
    return nullptr;
}

// Returns a raw string token.
TokenPtr
Lexer::parse_raw_string (Location loc, int initial_hash_count)
{
  // raw string literals
  std::string str;
  str.reserve (16); // some sensible default

  int length = 1 + initial_hash_count;

  if (initial_hash_count > 0)
    skip_input (initial_hash_count - 1);

  current_char = peek_input ();

  if (current_char != '"')
    rust_error_at (get_current_location (), "raw string has no opening %<\"%>");

  length++;
  skip_input ();
  Codepoint current_char32 = peek_codepoint_input ();

  while (!current_char32.is_eof ())
    {
      if (current_char32.value == '"')
	{
	  bool enough_hashes = true;

	  for (int i = 0; i < initial_hash_count; i++)
	    {
	      if (peek_input (i + 1) != '#')
		{
		  enough_hashes = false;
		  break;
		}
	    }

	  if (enough_hashes)
	    {
	      // skip enough input and peek enough input
	      skip_input (initial_hash_count);
	      current_char = peek_input ();
	      length += initial_hash_count + 1;
	      break;
	    }
	}

      length++;

      str += current_char32;
      skip_codepoint_input ();
      current_char32 = peek_codepoint_input ();
    }

  current_column += length;

  loc += length - 1;

  str.shrink_to_fit ();

  return Token::make_string (loc, std::move (str));
}

template <typename IsDigitFunc>
TokenPtr
Lexer::parse_non_decimal_int_literal (Location loc, IsDigitFunc is_digit_func,
				      std::string existent_str, int base)
{
  int length = 1;

  skip_input ();
  current_char = peek_input ();

  length++;

  // loop through to add entire number to string
  while (is_digit_func (current_char) || current_char == '_')
    {
      if (current_char == '_')
	{
	  // don't add _ to number
	  skip_input ();
	  current_char = peek_input ();

	  length++;

	  continue;
	}

      length++;

      // add raw numbers
      existent_str += current_char;
      skip_input ();
      current_char = peek_input ();
    }

  // convert value to decimal representation
  long dec_num = std::strtol (existent_str.c_str (), nullptr, base);

  existent_str = std::to_string (dec_num);

  // parse in type suffix if it exists
  auto type_suffix_pair = parse_in_type_suffix ();
  PrimitiveCoreType type_hint = type_suffix_pair.first;
  length += type_suffix_pair.second;

  current_column += length;

  if (type_hint == CORETYPE_F32 || type_hint == CORETYPE_F64)
    {
      rust_error_at (get_current_location (),
		     "invalid type suffix %qs for integer (%s) literal",
		     get_type_hint_string (type_hint),
		     base == 16
		       ? "hex"
		       : (base == 8 ? "octal"
				    : (base == 2 ? "binary"
						 : "<insert unknown base>")));
      return nullptr;
    }

  loc += length - 1;

  return Token::make_int (loc, std::move (existent_str), type_hint);
}

// Parses a hex, binary or octal int literal.
TokenPtr
Lexer::parse_non_decimal_int_literals (Location loc)
{
  std::string str;
  str.reserve (16); // some sensible default
  str += current_char;

  current_char = peek_input ();

  if (current_char == 'x')
    {
      // hex (integer only)
      return parse_non_decimal_int_literal (loc, is_x_digit, str + "x", 16);
    }
  else if (current_char == 'o')
    {
      // octal (integer only)
      return parse_non_decimal_int_literal (loc, is_octal_digit,
					    std::move (str), 8);
    }
  else if (current_char == 'b')
    {
      // binary (integer only)
      return parse_non_decimal_int_literal (loc, is_bin_digit, std::move (str),
					    2);
    }
  else
    {
      return nullptr;
    }
}

// Parses a decimal-based int literal or float literal.
TokenPtr
Lexer::parse_decimal_int_or_float (Location loc)
{
  std::string str;
  str.reserve (16); // some sensible default
  str += current_char;

  int length = 1;
  bool first_zero = current_char == '0';

  current_char = peek_input ();

  // parse initial decimal integer (or first integer part of float) literal
  auto initial_decimal = parse_in_decimal ();
  str += std::get<0> (initial_decimal);
  length += std::get<1> (initial_decimal);

  // detect float literal
  if (current_char == '.' && is_float_digit (peek_input (1)))
    {
      // float with a '.', parse another decimal into it

      // add . to str
      str += current_char;
      skip_input ();
      current_char = peek_input ();
      length++;

      // parse another decimal number for float
      auto second_decimal = parse_in_decimal ();
      str += std::get<0> (second_decimal);
      length += std::get<1> (second_decimal);

      // parse in exponent part if it exists
      auto exponent_pair = parse_in_exponent_part ();
      str += exponent_pair.first;
      length += exponent_pair.second;

      // parse in type suffix if it exists
      auto type_suffix_pair = parse_in_type_suffix ();
      PrimitiveCoreType type_hint = type_suffix_pair.first;
      length += type_suffix_pair.second;

      if (type_hint != CORETYPE_F32 && type_hint != CORETYPE_F64
	  && type_hint != CORETYPE_UNKNOWN)
	{
	  rust_error_at (get_current_location (),
			 "invalid type suffix %qs for floating-point literal",
			 get_type_hint_string (type_hint));
	  // ignore invalid type suffix as everything else seems fine
	  type_hint = CORETYPE_UNKNOWN;
	}

      current_column += length;

      loc += length - 1;

      str.shrink_to_fit ();
      return Token::make_float (loc, std::move (str), type_hint);
    }
  else if (current_char == '.' && check_valid_float_dot_end (peek_input (1)))
    {
      // float that is just an integer with a terminating '.' character

      // add . to str
      str += current_char;
      skip_input ();
      current_char = peek_input ();
      length++;

      // add a '0' after the . to prevent ambiguity
      str += '0';

      // type hint not allowed

      current_column += length;

      loc += length - 1;

      str.shrink_to_fit ();
      return Token::make_float (loc, std::move (str), CORETYPE_UNKNOWN);
    }
  else if (current_char == 'E' || current_char == 'e')
    {
      // exponent float with no '.' character

      // parse exponent part
      auto exponent_pair = parse_in_exponent_part ();
      str += exponent_pair.first;
      length += exponent_pair.second;

      // parse in type suffix if it exists
      auto type_suffix_pair = parse_in_type_suffix ();
      PrimitiveCoreType type_hint = type_suffix_pair.first;
      length += type_suffix_pair.second;

      if (type_hint != CORETYPE_F32 && type_hint != CORETYPE_F64
	  && type_hint != CORETYPE_UNKNOWN)
	{
	  rust_error_at (get_current_location (),
			 "invalid type suffix %qs for floating-point literal",
			 get_type_hint_string (type_hint));
	  // ignore invalid type suffix as everything else seems fine
	  type_hint = CORETYPE_UNKNOWN;
	}

      current_column += length;

      loc += length - 1;

      str.shrink_to_fit ();
      return Token::make_float (loc, std::move (str), type_hint);
    }
  else
    {
      // is an integer

      // parse in type suffix if it exists
      auto type_suffix_pair = parse_in_type_suffix ();
      PrimitiveCoreType type_hint = type_suffix_pair.first;
      /* A "real" pure decimal doesn't have a suffix and no zero prefix.  */
      if (type_hint == CORETYPE_UNKNOWN)
	{
	  bool pure_decimal = std::get<2> (initial_decimal);
	  if (pure_decimal && (!first_zero || str.size () == 1))
	    type_hint = CORETYPE_PURE_DECIMAL;
	}
      length += type_suffix_pair.second;

      current_column += length;

      loc += length - 1;

      str.shrink_to_fit ();
      return Token::make_int (loc, std::move (str), type_hint);
    }
}

TokenPtr
Lexer::parse_char_or_lifetime (Location loc)
{
  Codepoint current_char32;

  int length = 1;

  current_char32 = peek_codepoint_input ();
  if (current_char32.is_eof ())
    return nullptr;

  // parse escaped char literal
  if (current_char32.value == '\\')
    {
      // parse escape
      auto utf8_escape_pair = parse_utf8_escape ();
      current_char32 = std::get<0> (utf8_escape_pair);
      length += std::get<1> (utf8_escape_pair);

      if (peek_codepoint_input ().value != '\'')
	{
	  rust_error_at (get_current_location (), "unended character literal");
	}
      else
	{
	  skip_codepoint_input ();
	  current_char = peek_input ();
	  length++;
	}

      current_column += length;

      loc += length - 1;

      return Token::make_char (loc, current_char32);
    }
  else
    {
      skip_codepoint_input ();

      if (peek_codepoint_input ().value == '\'')
	{
	  // parse non-escaped char literal

	  // skip the ' character
	  skip_input ();
	  current_char = peek_input ();

	  // TODO fix due to different widths of utf-8 chars?
	  current_column += 3;

	  loc += 2;

	  return Token::make_char (loc, current_char32);
	}
      else if (ISDIGIT (current_char32.value) || ISALPHA (current_char32.value)
	       || current_char32.value == '_')
	{
	  // parse lifetime name
	  std::string str;
	  str += current_char32;
	  length++;

	  current_char = peek_input ();
	  while (ISDIGIT (current_char) || ISALPHA (current_char)
		 || current_char == '_')
	    {
	      str += current_char;
	      skip_input ();
	      current_char = peek_input ();
	      length++;
	    }

	  current_column += length;

	  loc += length - 1;

	  str.shrink_to_fit ();
	  return Token::make_lifetime (loc, std::move (str));
	}
      else
	{
	  rust_error_at (
	    get_current_location (),
	    "expected %' after character constant in character literal");
	  return nullptr;
	}
    }
}

// Returns the length of the codepoint at the current position.
int
Lexer::get_input_codepoint_length ()
{
  uint8_t input = peek_input ();

  if ((int8_t) input == EOF)
    return 0;

  if (input < 128)
    {
      // ascii -- 1 byte
      // return input;

      return 1;
    }
  else if ((input & 0xC0) == 0x80)
    {
      // invalid (continuation; can't be first char)
      // return 0xFFFE;

      return 0;
    }
  else if ((input & 0xE0) == 0xC0)
    {
      // 2 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      // uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
      // return output;
      return 2;
    }
  else if ((input & 0xF0) == 0xE0)
    {
      // 3 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      uint8_t input3 = peek_input (2);
      if ((input3 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      /*uint32_t output
	= ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) <<
      0); return output;*/
      return 3;
    }
  else if ((input & 0xF8) == 0xF0)
    {
      // 4 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      uint8_t input3 = peek_input (2);
      if ((input3 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      uint8_t input4 = peek_input (3);
      if ((input4 & 0xC0) != 0x80)
	return 0;
      // return 0xFFFE;

      /*uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
			| ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
      return output;*/
      return 4;
    }
  else
    {
      rust_error_at (get_current_location (),
		     "invalid UTF-8 [FIRST] (too long)");
      return 0;
    }
}

// Returns the codepoint at the current position.
Codepoint
Lexer::peek_codepoint_input ()
{
  uint8_t input = peek_input ();

  if ((int8_t) input == EOF)
    return Codepoint::eof ();

  if (input < 128)
    {
      // ascii -- 1 byte
      return {input};
    }
  else if ((input & 0xC0) == 0x80)
    {
      // invalid (continuation; can't be first char)
      return {0xFFFE};
    }
  else if ((input & 0xE0) == 0xC0)
    {
      // 2 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return {0xFFFE};

      uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
      return {output};
    }
  else if ((input & 0xF0) == 0xE0)
    {
      // 3 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return {0xFFFE};

      uint8_t input3 = peek_input (2);
      if ((input3 & 0xC0) != 0x80)
	return {0xFFFE};

      uint32_t output = ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6)
			| ((input3 & 0x3F) << 0);
      return {output};
    }
  else if ((input & 0xF8) == 0xF0)
    {
      // 4 bytes
      uint8_t input2 = peek_input (1);
      if ((input2 & 0xC0) != 0x80)
	return {0xFFFE};

      uint8_t input3 = peek_input (2);
      if ((input3 & 0xC0) != 0x80)
	return {0xFFFE};

      uint8_t input4 = peek_input (3);
      if ((input4 & 0xC0) != 0x80)
	return {0xFFFE};

      uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
			| ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
      return {output};
    }
  else
    {
      rust_error_at (get_current_location (),
		     "invalid UTF-8 [SECND] (too long)");
      return {0xFFFE};
    }
}

void
Lexer::skip_codepoint_input ()
{
  int toSkip = get_input_codepoint_length ();
  gcc_assert (toSkip >= 1);

  skip_input (toSkip - 1);
}

int
Lexer::test_get_input_codepoint_n_length (int n_start_offset)
{
  uint8_t input = peek_input (n_start_offset);

  if (input < 128)
    {
      // ascii -- 1 byte
      // return input;
      return 1;
    }
  else if ((input & 0xC0) == 0x80)
    {
      // invalid (continuation; can't be first char)
      // return 0xFFFE;
      return 0;
    }
  else if ((input & 0xE0) == 0xC0)
    {
      // 2 bytes
      uint8_t input2 = peek_input (n_start_offset + 1);
      if ((input2 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      // uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
      // return output;
      return 2;
    }
  else if ((input & 0xF0) == 0xE0)
    {
      // 3 bytes
      uint8_t input2 = peek_input (n_start_offset + 1);
      if ((input2 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      uint8_t input3 = peek_input (n_start_offset + 2);
      if ((input3 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      /*uint32_t output
	= ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 & 0x3F) <<
      0); return output;*/
      return 3;
    }
  else if ((input & 0xF8) == 0xF0)
    {
      // 4 bytes
      uint8_t input2 = peek_input (n_start_offset + 1);
      if ((input2 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      uint8_t input3 = peek_input (n_start_offset + 2);
      if ((input3 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      uint8_t input4 = peek_input (n_start_offset + 3);
      if ((input4 & 0xC0) != 0x80)
	// return 0xFFFE;
	return 0;

      /*uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
			| ((input3 & 0x3F) << 6) | ((input4 & 0x3F) << 0);
      return output;*/
      return 4;
    }
  else
    {
      rust_error_at (get_current_location (),
		     "invalid UTF-8 [THIRD] (too long)");
      return 0;
    }
}

// peeks the codepoint input at n codepoints ahead of current codepoint - try
// not to use
Codepoint
Lexer::test_peek_codepoint_input (int n)
{
  int totalOffset = 0;

  // add up all offsets into total offset? does this do what I want?
  for (int i = 0; i < n; i++)
    {
      totalOffset += test_get_input_codepoint_n_length (totalOffset);
    }
  // issues: this would have (at least) O(n) lookup time, not O(1) like the
  // rest?

  // TODO: implement if still needed

  // error out of function as it is not implemented
  gcc_assert (1 == 0);
  return {0};
  /*
	  uint8_t input = peek_input();

	  if (input < 128) {
	      // ascii -- 1 byte
	      return input;
	  } else if ((input & 0xC0) == 0x80) {
	      // invalid (continuation; can't be first char)
	      return 0xFFFE;
	  } else if ((input & 0xE0) == 0xC0) {
	      // 2 bytes
	      uint8_t input2 = peek_input(1);
	      if ((input2 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint32_t output = ((input & 0x1F) << 6) | ((input2 & 0x3F) << 0);
	      return output;
	  } else if ((input & 0xF0) == 0xE0) {
	      // 3 bytes
	      uint8_t input2 = peek_input(1);
	      if ((input2 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint8_t input3 = peek_input(2);
	      if ((input3 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint32_t output
		= ((input & 0x0F) << 12) | ((input2 & 0x3F) << 6) | ((input3 &
     0x3F) << 0); return output; } else if ((input & 0xF8) == 0xF0) {
	      // 4 bytes
	      uint8_t input2 = peek_input(1);
	      if ((input2 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint8_t input3 = peek_input(2);
	      if ((input3 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint8_t input4 = peek_input(3);
	      if ((input4 & 0xC0) != 0x80)
		  return 0xFFFE;

	      uint32_t output = ((input & 0x07) << 18) | ((input2 & 0x3F) << 12)
				| ((input3 & 0x3F) << 6) | ((input4 & 0x3F) <<
     0); return output; } else { rust_error_at(get_current_location(), "invalid
     UTF-8 (too long)"); return 0xFFFE;
	  }*/
}

void
Lexer::split_current_token (TokenId new_left, TokenId new_right)
{
  /* TODO: assert that this TokenId is a "simple token" like punctuation and not
   * like "IDENTIFIER"? */
  Location current_loc = peek_token ()->get_locus ();
  TokenPtr new_left_tok = Token::make (new_left, current_loc);
  TokenPtr new_right_tok = Token::make (new_right, current_loc + 1);

  token_queue.replace_current_value (std::move (new_left_tok));
  token_queue.insert (1, std::move (new_right_tok));
}

void
Lexer::start_line (int current_line, int current_column)
{
  if (line_map)
    line_map->start_line (current_line, current_column);
}

} // namespace Rust
