// lex.cc -- Go frontend lexer.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "lex.h"

// Manage mapping from keywords to the Keyword codes.

class Keywords
{
 public:
  // The structure which maps keywords to codes.
  struct Mapping
  {
    // Keyword string.
    const char* keystring;
    // Keyword code.
    Keyword keycode;
  };

  // Return the parsecode corresponding to KEYSTRING, or
  // KEYWORD_INVALID if it is not a keyword.
  Keyword
  keyword_to_code(const char* keyword, size_t len) const;

  // Return the string for a keyword.
  const char*
  keyword_to_string(Keyword) const;

 private:
  static const Mapping mapping_[];
  static const int count_;
};

// Mapping from keyword string to keyword code.  This array must be
// kept in sorted order, and the order must match the Keyword enum.
// Strings are looked up using bsearch.

const Keywords::Mapping
Keywords::mapping_[] =
{
  { NULL,	   KEYWORD_INVALID },
  { "__asm__",	   KEYWORD_ASM },
  { "break",	   KEYWORD_BREAK },
  { "case",	   KEYWORD_CASE },
  { "chan",	   KEYWORD_CHAN },
  { "const",	   KEYWORD_CONST },
  { "continue",	   KEYWORD_CONTINUE },
  { "default",	   KEYWORD_DEFAULT },
  { "defer",	   KEYWORD_DEFER },
  { "else",	   KEYWORD_ELSE },
  { "fallthrough", KEYWORD_FALLTHROUGH },
  { "for",	   KEYWORD_FOR },
  { "func",	   KEYWORD_FUNC },
  { "go",	   KEYWORD_GO },
  { "goto",	   KEYWORD_GOTO },
  { "if",	   KEYWORD_IF },
  { "import",	   KEYWORD_IMPORT },
  { "interface",   KEYWORD_INTERFACE },
  { "map",	   KEYWORD_MAP },
  { "package",	   KEYWORD_PACKAGE },
  { "range",	   KEYWORD_RANGE },
  { "return",	   KEYWORD_RETURN },
  { "select",	   KEYWORD_SELECT },
  { "struct",	   KEYWORD_STRUCT },
  { "switch",	   KEYWORD_SWITCH },
  { "type",	   KEYWORD_TYPE },
  { "var",	   KEYWORD_VAR }
};

// Number of entries in the map.

const int Keywords::count_ =
  sizeof(Keywords::mapping_) / sizeof(Keywords::mapping_[0]);

// Comparison function passed to bsearch.

extern "C"
{

struct Keywords_search_key
{
  const char* str;
  size_t len;
};

static int
keyword_compare(const void* keyv, const void* mapv)
{
  const Keywords_search_key* key =
    static_cast<const Keywords_search_key*>(keyv);
  const Keywords::Mapping* map =
    static_cast<const Keywords::Mapping*>(mapv);
  if (map->keystring == NULL)
    return 1;
  int i = strncmp(key->str, map->keystring, key->len);
  if (i != 0)
    return i;
  if (map->keystring[key->len] != '\0')
    return -1;
  return 0;
}

} // End extern "C".

// Convert a string to a keyword code.  Return KEYWORD_INVALID if the
// string is not a keyword.

Keyword
Keywords::keyword_to_code(const char* keyword, size_t len) const
{
  Keywords_search_key key;
  key.str = keyword;
  key.len = len;
  void* mapv = bsearch(&key,
                       this->mapping_,
                       this->count_,
                       sizeof(this->mapping_[0]),
                       keyword_compare);
  if (mapv == NULL)
    return KEYWORD_INVALID;
  Mapping* map = static_cast<Mapping*>(mapv);
  return map->keycode;
}

// Convert a keyword code to a string.

const char*
Keywords::keyword_to_string(Keyword code) const
{
  go_assert(code > KEYWORD_INVALID && code < this->count_);
  const Mapping* map = &this->mapping_[code];
  go_assert(map->keycode == code);
  return map->keystring;
}

// There is one instance of the Keywords class.

static Keywords keywords;

// Class Token.

// Make a general token.

Token::Token(Classification classification, Location location)
  : classification_(classification), location_(location)
{
}

// Destroy a token.

Token::~Token()
{
  this->clear();
}

// Clear a token--release memory.

void
Token::clear()
{
  if (this->classification_ == TOKEN_INTEGER
      || this->classification_ == TOKEN_CHARACTER)
    mpz_clear(this->u_.integer_value);
  else if (this->classification_ == TOKEN_FLOAT
	   || this->classification_ == TOKEN_IMAGINARY)
    mpfr_clear(this->u_.float_value);
}

// Construct a token.

Token::Token(const Token& tok)
  : classification_(tok.classification_), location_(tok.location_)
{
  switch (this->classification_)
    {
    case TOKEN_INVALID:
    case TOKEN_EOF:
      break;
    case TOKEN_KEYWORD:
      this->u_.keyword = tok.u_.keyword;
      break;
    case TOKEN_IDENTIFIER:
    case TOKEN_STRING:
      this->u_.string_value = tok.u_.string_value;
      break;
    case TOKEN_OPERATOR:
      this->u_.op = tok.u_.op;
      break;
    case TOKEN_CHARACTER:
    case TOKEN_INTEGER:
      mpz_init_set(this->u_.integer_value, tok.u_.integer_value);
      break;
    case TOKEN_FLOAT:
    case TOKEN_IMAGINARY:
      mpfr_init_set(this->u_.float_value, tok.u_.float_value, GMP_RNDN);
      break;
    default:
      go_unreachable();
    }
}

// Assign to a token.

Token&
Token::operator=(const Token& tok)
{
  this->clear();
  this->classification_ = tok.classification_;
  this->location_ = tok.location_;
  switch (tok.classification_)
    {
    case TOKEN_INVALID:
    case TOKEN_EOF:
      break;
    case TOKEN_KEYWORD:
      this->u_.keyword = tok.u_.keyword;
      break;
    case TOKEN_IDENTIFIER:
      this->u_.identifier_value.name = tok.u_.identifier_value.name;
      this->u_.identifier_value.is_exported =
	tok.u_.identifier_value.is_exported;
      break;
    case TOKEN_STRING:
      this->u_.string_value = tok.u_.string_value;
      break;
    case TOKEN_OPERATOR:
      this->u_.op = tok.u_.op;
      break;
    case TOKEN_CHARACTER:
    case TOKEN_INTEGER:
      mpz_init_set(this->u_.integer_value, tok.u_.integer_value);
      break;
    case TOKEN_FLOAT:
    case TOKEN_IMAGINARY:
      mpfr_init_set(this->u_.float_value, tok.u_.float_value, GMP_RNDN);
      break;
    default:
      go_unreachable();
    }
  return *this;
}

// Print the token for debugging.

void
Token::print(FILE* file) const
{
  switch (this->classification_)
    {
    case TOKEN_INVALID:
      fprintf(file, "invalid");
      break;
    case TOKEN_EOF:
      fprintf(file, "EOF");
      break;
    case TOKEN_KEYWORD:
      fprintf(file, "keyword %s", keywords.keyword_to_string(this->u_.keyword));
      break;
    case TOKEN_IDENTIFIER:
      fprintf(file, "identifier \"%s\"", this->u_.string_value->c_str());
      break;
    case TOKEN_STRING:
      fprintf(file, "quoted string \"%s\"", this->u_.string_value->c_str());
      break;
    case TOKEN_CHARACTER:
      fprintf(file, "character ");
      mpz_out_str(file, 10, this->u_.integer_value);
      break;
    case TOKEN_INTEGER:
      fprintf(file, "integer ");
      mpz_out_str(file, 10, this->u_.integer_value);
      break;
    case TOKEN_FLOAT:
      fprintf(file, "float ");
      mpfr_out_str(file, 10, 0, this->u_.float_value, GMP_RNDN);
      break;
    case TOKEN_IMAGINARY:
      fprintf(file, "imaginary ");
      mpfr_out_str(file, 10, 0, this->u_.float_value, GMP_RNDN);
      break;
    case TOKEN_OPERATOR:
      fprintf(file, "operator ");
      switch (this->u_.op)
	{
	case OPERATOR_INVALID:
	  fprintf(file, "invalid");
	  break;
	case OPERATOR_OROR:
	  fprintf(file, "||");
	  break;
	case OPERATOR_ANDAND:
	  fprintf(file, "&&");
	  break;
	case OPERATOR_EQEQ:
	  fprintf(file, "==");
	  break;
	case OPERATOR_NOTEQ:
	  fprintf(file, "!=");
	  break;
	case OPERATOR_LT:
	  fprintf(file, "<");
	  break;
	case OPERATOR_LE:
	  fprintf(file, "<=");
	  break;
	case OPERATOR_GT:
	  fprintf(file, ">");
	  break;
	case OPERATOR_GE:
	  fprintf(file, ">=");
	  break;
	case OPERATOR_PLUS:
	  fprintf(file, "+");
	  break;
	case OPERATOR_MINUS:
	  fprintf(file, "-");
	  break;
	case OPERATOR_OR:
	  fprintf(file, "|");
	  break;
	case OPERATOR_XOR:
	  fprintf(file, "^");
	  break;
	case OPERATOR_MULT:
	  fprintf(file, "*");
	  break;
	case OPERATOR_DIV:
	  fprintf(file, "/");
	  break;
	case OPERATOR_MOD:
	  fprintf(file, "%%");
	  break;
	case OPERATOR_LSHIFT:
	  fprintf(file, "<<");
	  break;
	case OPERATOR_RSHIFT:
	  fprintf(file, ">>");
	  break;
	case OPERATOR_AND:
	  fprintf(file, "&");
	  break;
	case OPERATOR_BITCLEAR:
	  fprintf(file, "&^");
	  break;
	case OPERATOR_NOT:
	  fprintf(file, "!");
	  break;
	case OPERATOR_CHANOP:
	  fprintf(file, "<-");
	  break;
	case OPERATOR_EQ:
	  fprintf(file, "=");
	  break;
	case OPERATOR_PLUSEQ:
	  fprintf(file, "+=");
	  break;
	case OPERATOR_MINUSEQ:
	  fprintf(file, "-=");
	  break;
	case OPERATOR_OREQ:
	  fprintf(file, "|=");
	  break;
	case OPERATOR_XOREQ:
	  fprintf(file, "^=");
	  break;
	case OPERATOR_MULTEQ:
	  fprintf(file, "*=");
	  break;
	case OPERATOR_DIVEQ:
	  fprintf(file, "/=");
	  break;
	case OPERATOR_MODEQ:
	  fprintf(file, "%%=");
	  break;
	case OPERATOR_LSHIFTEQ:
	  fprintf(file, "<<=");
	  break;
	case OPERATOR_RSHIFTEQ:
	  fprintf(file, ">>=");
	  break;
	case OPERATOR_ANDEQ:
	  fprintf(file, "&=");
	  break;
	case OPERATOR_BITCLEAREQ:
	  fprintf(file, "&^=");
	  break;
	case OPERATOR_PLUSPLUS:
	  fprintf(file, "++");
	  break;
	case OPERATOR_MINUSMINUS:
	  fprintf(file, "--");
	  break;
	case OPERATOR_COLON:
	  fprintf(file, ":");
	  break;
	case OPERATOR_COLONEQ:
	  fprintf(file, ":=");
	  break;
	case OPERATOR_SEMICOLON:
	  fprintf(file, ";");
	  break;
	case OPERATOR_DOT:
	  fprintf(file, ".");
	  break;
	case OPERATOR_COMMA:
	  fprintf(file, ",");
	  break;
	case OPERATOR_LPAREN:
	  fprintf(file, "(");
	  break;
	case OPERATOR_RPAREN:
	  fprintf(file, ")");
	  break;
	case OPERATOR_LCURLY:
	  fprintf(file, "{");
	  break;
	case OPERATOR_RCURLY:
	  fprintf(file, "}");
	  break;
	case OPERATOR_LSQUARE:
	  fprintf(file, "[");
	  break;
	case OPERATOR_RSQUARE:
	  fprintf(file, "]");
	  break;
	default:
	  go_unreachable();
	}
      break;
    default:
      go_unreachable();
    }
}

// Class Lex.

Lex::Lex(const char* input_file_name, FILE* input_file, Linemap* linemap)
  : input_file_name_(input_file_name), input_file_(input_file),
    linemap_(linemap), linebuf_(NULL), linebufsize_(120), linesize_(0),
    lineoff_(0), lineno_(0), add_semi_at_eol_(false), saw_nointerface_(false),
    extern_()
{
  this->linebuf_ = new char[this->linebufsize_];
  this->linemap_->start_file(input_file_name, 0);
}

Lex::~Lex()
{
  delete[] this->linebuf_;
}

// Read a new line from the file.

ssize_t
Lex::get_line()
{
  char* buf = this->linebuf_;
  size_t size = this->linebufsize_;

  FILE* file = this->input_file_;
  size_t cur = 0;
  while (true)
    {
      int c = getc(file);
      if (c == EOF)
	{
	  if (cur == 0)
	    return -1;
	  break;
	}
      if (cur + 1 >= size)
	{
	  size_t ns = 2 * size + 1;
	  if (ns < size || static_cast<ssize_t>(ns) < 0)
	    error_at(this->location(), "out of memory");
	  char* nb = new char[ns];
	  memcpy(nb, buf, cur);
	  delete[] buf;
	  buf = nb;
	  size = ns;
	}
      buf[cur] = c;
      ++cur;

      if (c == '\n')
	break;
    }

  buf[cur] = '\0';

  this->linebuf_ = buf;
  this->linebufsize_ = size;

  return cur;
}

// See if we need to read a new line.  Return true if there is a new
// line, false if we are at EOF.

bool
Lex::require_line()
{
  if (this->lineoff_ < this->linesize_)
    return true;

  ssize_t got = this->get_line();
  if (got < 0)
    return false;
  ++this->lineno_;
  this->linesize_= got;
  this->lineoff_ = 0;

  this->linemap_->start_line(this->lineno_, this->linesize_);

  return true;
}

// Get the current location.

Location
Lex::location() const
{
  return this->linemap_->get_location(this->lineoff_ + 1);
}

// Get a location slightly before the current one.  This is used for
// slightly more efficient handling of operator tokens.

Location
Lex::earlier_location(int chars) const
{
  return this->linemap_->get_location(this->lineoff_ + 1 - chars);
}

// Get the next token.

Token
Lex::next_token()
{
  bool saw_cpp_comment = false;
  while (true)
    {
      if (!this->require_line())
	{
	  bool add_semi_at_eol = this->add_semi_at_eol_;
	  this->add_semi_at_eol_ = false;
	  if (add_semi_at_eol)
	    return this->make_operator(OPERATOR_SEMICOLON, 1);
	  return this->make_eof_token();
	}

      if (!saw_cpp_comment)
	this->extern_.clear();
      saw_cpp_comment = false;

      const char* p = this->linebuf_ + this->lineoff_;
      const char* pend = this->linebuf_ + this->linesize_;

      while (p < pend)
	{
	  unsigned char cc = *p;
	  switch (cc)
	    {
	    case ' ': case '\t': case '\r':
	      ++p;
	      // Skip whitespace quickly.
	      while (*p == ' ' || *p == '\t' || *p == '\r')
		++p;
	      break;

	    case '\n':
	      {
		++p;
		bool add_semi_at_eol = this->add_semi_at_eol_;
		this->add_semi_at_eol_ = false;
		if (add_semi_at_eol)
		  {
		    this->lineoff_ = p - this->linebuf_;
		    return this->make_operator(OPERATOR_SEMICOLON, 1);
		  }
	      }
	      break;

	    case '/':
	      if (p[1] == '/')
		{
		  this->lineoff_ = p + 2 - this->linebuf_;
		  this->skip_cpp_comment();
		  p = pend;
		  if (p[-1] == '\n' && this->add_semi_at_eol_)
		    --p;
		  saw_cpp_comment = true;
		}
	      else if (p[1] == '*')
		{
		  this->lineoff_ = p - this->linebuf_;
		  Location location = this->location();
		  if (!this->skip_c_comment())
		    return Token::make_invalid_token(location);
		  p = this->linebuf_ + this->lineoff_;
		  pend = this->linebuf_ + this->linesize_;
		}
	      else if (p[1] == '=')
		{
		  this->add_semi_at_eol_ = false;
		  this->lineoff_ = p + 2 - this->linebuf_;
		  return this->make_operator(OPERATOR_DIVEQ, 2);
		}
	      else
		{
		  this->add_semi_at_eol_ = false;
		  this->lineoff_ = p + 1 - this->linebuf_;
		  return this->make_operator(OPERATOR_DIV, 1);
		}
	      break;

	    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
	    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	    case 'Y': case 'Z':
	    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	    case 'y': case 'z':
	    case '_':
	      this->lineoff_ = p - this->linebuf_;
	      return this->gather_identifier();

	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
	      this->add_semi_at_eol_ = true;
	      this->lineoff_ = p - this->linebuf_;
	      return this->gather_number();

	    case '\'':
	      this->add_semi_at_eol_ = true;
	      this->lineoff_ = p - this->linebuf_;
	      return this->gather_character();

	    case '"':
	      this->add_semi_at_eol_ = true;
	      this->lineoff_ = p - this->linebuf_;
	      return this->gather_string();

	    case '`':
	      this->add_semi_at_eol_ = true;
	      this->lineoff_ = p - this->linebuf_;
	      return this->gather_raw_string();

	    case '<':
	    case '>':
	    case '&':
	      if (p + 2 < pend)
		{
		  this->add_semi_at_eol_ = false;
		  Operator op = this->three_character_operator(cc, p[1], p[2]);
		  if (op != OPERATOR_INVALID)
		    {
		      this->lineoff_ = p + 3 - this->linebuf_;
		      return this->make_operator(op, 3);
		    }
		}
	      // Fall through.
	    case '|':
	    case '=':
	    case '!':
	    case '+':
	    case '-':
	    case '^':
	    case '*':
	      // '/' handled above.
	    case '%':
	    case ':':
	    case ';':
	    case ',':
	    case '(': case ')':
	    case '{': case '}':
	    case '[': case ']':
	      {
		this->add_semi_at_eol_ = false;
		Operator op = this->two_character_operator(cc, p[1]);
		int chars;
		if (op != OPERATOR_INVALID)
		  {
		    ++p;
		    chars = 2;
		  }
		else
		  {
		    op = this->one_character_operator(cc);
		    chars = 1;
		  }
		this->lineoff_ = p + 1 - this->linebuf_;
		return this->make_operator(op, chars);
	      }

	    case '.':
	      if (p[1] >= '0' && p[1] <= '9')
		{
		  this->add_semi_at_eol_ = true;
		  this->lineoff_ = p - this->linebuf_;
		  return this->gather_number();
		}
	      if (p[1] == '.' && p[2] == '.')
		{
		  this->add_semi_at_eol_ = false;
		  this->lineoff_ = p + 3 - this->linebuf_;
		  return this->make_operator(OPERATOR_ELLIPSIS, 3);
		}
	      this->add_semi_at_eol_ = false;
	      this->lineoff_ = p + 1 - this->linebuf_;
	      return this->make_operator(OPERATOR_DOT, 1);

	    default:
	      {
		unsigned int ci;
		bool issued_error;
		this->lineoff_ = p - this->linebuf_;
		const char *pnext = this->advance_one_utf8_char(p, &ci,
								&issued_error);

		// Ignore byte order mark at start of file.
		if (ci == 0xfeff)
		  {
		    p = pnext;
		    break;
		  }

		if (Lex::is_unicode_letter(ci))
		  return this->gather_identifier();

		if (!issued_error)
		  error_at(this->location(),
			   "invalid character 0x%x in input file",
			   ci);

		p = pend;

		break;
	      }
	    }
	}

      this->lineoff_ = p - this->linebuf_;
    }
}

// Fetch one UTF-8 character from a string.  Set *VALUE to the value.
// Return the number of bytes read from the string.  Returns 0 if the
// string does not point to a valid UTF-8 character.

int
Lex::fetch_char(const char* p, unsigned int* value)
{
  unsigned char c = *p;
  if (c <= 0x7f)
    {
      *value = c;
      return 1;
    }
  else if ((c & 0xe0) == 0xc0
	   && (p[1] & 0xc0) == 0x80)
    {
      *value = (((c & 0x1f) << 6)
		+ (p[1] & 0x3f));
      if (*value <= 0x7f)
	{
	  *value = 0xfffd;
	  return 0;
	}
      return 2;
    }
  else if ((c & 0xf0) == 0xe0
	   && (p[1] & 0xc0) == 0x80
	   && (p[2] & 0xc0) == 0x80)
    {
      *value = (((c & 0xf) << 12)
		+ ((p[1] & 0x3f) << 6)
		+ (p[2] & 0x3f));
      if (*value <= 0x7ff)
	{
	  *value = 0xfffd;
	  return 0;
	}
      return 3;
    }
  else if ((c & 0xf8) == 0xf0
	   && (p[1] & 0xc0) == 0x80
	   && (p[2] & 0xc0) == 0x80
	   && (p[3] & 0xc0) == 0x80)
    {
      *value = (((c & 0x7) << 18)
		+ ((p[1] & 0x3f) << 12)
		+ ((p[2] & 0x3f) << 6)
		+ (p[3] & 0x3f));
      if (*value <= 0xffff)
	{
	  *value = 0xfffd;
	  return 0;
	}
      return 4;
    }
  else
    {
      /* Invalid encoding. Return the Unicode replacement
	 character.  */
      *value = 0xfffd;
      return 0;
    }
}

// Advance one UTF-8 character.  Return the pointer beyond the
// character.  Set *VALUE to the value.  Set *ISSUED_ERROR if an error
// was issued.

const char*
Lex::advance_one_utf8_char(const char* p, unsigned int* value,
			   bool* issued_error)
{
  *issued_error = false;

  if (*p == '\0')
    {
      error_at(this->location(), "invalid NUL byte");
      *issued_error = true;
      *value = 0;
      return p + 1;
    }

  int adv = Lex::fetch_char(p, value);
  if (adv == 0)
    {
      error_at(this->location(), "invalid UTF-8 encoding");
      *issued_error = true;
      return p + 1;
    }

  // Warn about byte order mark, except at start of file.
  if (*value == 0xfeff && (this->lineno_ != 1 || this->lineoff_ != 0))
    {
      error_at(this->location(), "Unicode (UTF-8) BOM in middle of file");
      *issued_error = true;
    }

  return p + adv;
}

// Pick up an identifier.

Token
Lex::gather_identifier()
{
  const char* pstart = this->linebuf_ + this->lineoff_;
  const char* p = pstart;
  const char* pend = this->linebuf_ + this->linesize_;
  bool is_first = true;
  bool is_exported = false;
  bool has_non_ascii_char = false;
  std::string buf;
  while (p < pend)
    {
      unsigned char cc = *p;
      if (cc <= 0x7f)
	{
	  if ((cc < 'A' || cc > 'Z')
	      && (cc < 'a' || cc > 'z')
	      && cc != '_'
	      && (cc < '0' || cc > '9'))
	    break;
	  ++p;
	  if (is_first)
	    {
	      is_exported = cc >= 'A' && cc <= 'Z';
	      is_first = false;
	    }
	  if (has_non_ascii_char)
	    buf.push_back(cc);
	}
      else
	{
	  unsigned int ci;
	  bool issued_error;
	  this->lineoff_ = p - this->linebuf_;
	  const char* pnext = this->advance_one_utf8_char(p, &ci,
							  &issued_error);
	  bool is_invalid = false;
	  if (!Lex::is_unicode_letter(ci) && !Lex::is_unicode_digit(ci))
	    {
	      // There is no valid place for a non-ASCII character
	      // other than an identifier, so we get better error
	      // handling behaviour if we swallow this character after
	      // giving an error.
	      if (!issued_error)
		error_at(this->location(),
			 "invalid character 0x%x in identifier",
			 ci);
	      is_invalid = true;
	    }
	  if (is_first)
	    {
	      is_exported = Lex::is_unicode_uppercase(ci);
	      is_first = false;
	    }
	  if (!has_non_ascii_char)
	    {
	      buf.assign(pstart, p - pstart);
	      has_non_ascii_char = true;
	    }
	  if (is_invalid && !Lex::is_invalid_identifier(buf))
	    buf.append("$INVALID$");
	  buf.append(p, pnext - p);
	  p = pnext;
	}
    }
  Location location = this->location();
  this->add_semi_at_eol_ = true;
  this->lineoff_ = p - this->linebuf_;
  if (has_non_ascii_char)
    return Token::make_identifier_token(buf, is_exported, location);
  else
    {
      Keyword code = keywords.keyword_to_code(pstart, p - pstart);
      if (code == KEYWORD_INVALID)
	return Token::make_identifier_token(std::string(pstart, p - pstart),
					    is_exported, location);
      else
	{
	  switch (code)
	    {
	    case KEYWORD_BREAK:
	    case KEYWORD_CONTINUE:
	    case KEYWORD_FALLTHROUGH:
	    case KEYWORD_RETURN:
	      break;
	    default:
	      this->add_semi_at_eol_ = false;
	      break;
	    }
	  return Token::make_keyword_token(code, location);
	}
    }
}

// Return whether C is a hex digit.

bool
Lex::is_hex_digit(char c)
{
  return ((c >= '0' && c <= '9')
	  || (c >= 'A' && c <= 'F')
	  || (c >= 'a' && c <= 'f'));
}

// Return whether an exponent could start at P.

bool
Lex::could_be_exponent(const char* p, const char* pend)
{
  if (*p != 'e' && *p != 'E')
    return false;
  ++p;
  if (p >= pend)
    return false;
  if (*p == '+' || *p == '-')
    {
      ++p;
      if (p >= pend)
	return false;
    }
  return *p >= '0' && *p <= '9';
}

// Pick up a number.

Token
Lex::gather_number()
{
  const char* pstart = this->linebuf_ + this->lineoff_;
  const char* p = pstart;
  const char* pend = this->linebuf_ + this->linesize_;

  Location location = this->location();

  bool neg = false;
  if (*p == '+')
    ++p;
  else if (*p == '-')
    {
      ++p;
      neg = true;
    }

  const char* pnum = p;
  if (*p == '0')
    {
      int base;
      if ((p[1] == 'x' || p[1] == 'X')
	  && Lex::is_hex_digit(p[2]))
	{
	  base = 16;
	  p += 2;
	  pnum = p;
	  while (p < pend)
	    {
	      if (!Lex::is_hex_digit(*p))
		break;
	      ++p;
	    }
	}
      else
	{
	  base = 8;
	  pnum = p;
	  while (p < pend)
	    {
	      if (*p < '0' || *p > '7')
		break;
	      ++p;
	    }
	}

      // A partial token that looks like an octal literal might actually be the
      // beginning of a floating-point or imaginary literal.
      if (base == 16 || (*p != '.' && *p != 'i' && !Lex::could_be_exponent(p, pend)))
	{
	  std::string s(pnum, p - pnum);
	  mpz_t val;
	  int r = mpz_init_set_str(val, s.c_str(), base);
	  go_assert(r == 0);

	  if (neg)
	    mpz_neg(val, val);

	  this->lineoff_ = p - this->linebuf_;
	  Token ret = Token::make_integer_token(val, location);
	  mpz_clear(val);
	  return ret;
	}
    }

  while (p < pend)
    {
      if (*p < '0' || *p > '9')
	break;
      ++p;
    }

  if (*p != '.' && *p != 'i' && !Lex::could_be_exponent(p, pend))
    {
      std::string s(pnum, p - pnum);
      mpz_t val;
      int r = mpz_init_set_str(val, s.c_str(), 10);
      go_assert(r == 0);

      if (neg)
	mpz_neg(val, val);

      this->lineoff_ = p - this->linebuf_;
      Token ret = Token::make_integer_token(val, location);
      mpz_clear(val);
      return ret;
    }

  if (*p != 'i')
    {
      bool dot = *p == '.';

      ++p;

      if (!dot)
	{
	  if (*p == '+' || *p == '-')
	    ++p;
	}

      while (p < pend)
	{
	  if (*p < '0' || *p > '9')
	    break;
	  ++p;
	}

      if (dot && Lex::could_be_exponent(p, pend))
	{
	  ++p;
	  if (*p == '+' || *p == '-')
	    ++p;
	  while (p < pend)
	    {
	      if (*p < '0' || *p > '9')
		break;
	      ++p;
	    }
	}
    }

  std::string s(pnum, p - pnum);
  mpfr_t val;
  int r = mpfr_init_set_str(val, s.c_str(), 10, GMP_RNDN);
  go_assert(r == 0);

  if (neg)
    mpfr_neg(val, val, GMP_RNDN);

  bool is_imaginary = *p == 'i';
  if (is_imaginary)
    ++p;

  this->lineoff_ = p - this->linebuf_;
  if (is_imaginary)
    {
      Token ret = Token::make_imaginary_token(val, location);
      mpfr_clear(val);
      return ret;
    }
  else
    {
      Token ret = Token::make_float_token(val, location);
      mpfr_clear(val);
      return ret;
    }
}

// Advance one character, possibly escaped.  Return the pointer beyond
// the character.  Set *VALUE to the character.  Set *IS_CHARACTER if
// this is a character (e.g., 'a' or '\u1234') rather than a byte
// value (e.g., '\001').

const char*
Lex::advance_one_char(const char* p, bool is_single_quote, unsigned int* value,
		      bool* is_character)
{
  *value = 0;
  *is_character = true;
  if (*p != '\\')
    {
      bool issued_error;
      const char* ret = this->advance_one_utf8_char(p, value, &issued_error);
      if (is_single_quote
	  && (*value == '\'' || *value == '\n')
	  && !issued_error)
	error_at(this->location(), "invalid character literal");
      return ret;
    }
  else
    {
      ++p;
      switch (*p)
	{
	case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
	  *is_character = false;
	  if (p[1] >= '0' && p[1] <= '7'
	      && p[2] >= '0' && p[2] <= '7')
	    {
	      *value = ((Lex::octal_value(p[0]) << 6)
			+ (Lex::octal_value(p[1]) << 3)
			+ Lex::octal_value(p[2]));
	      if (*value > 255)
		{
		  error_at(this->location(), "invalid octal constant");
		  *value = 255;
		}
	      return p + 3;
	    }
	      error_at(this->location(), "invalid octal character");
	  return (p[1] >= '0' && p[1] <= '7'
		  ? p + 2
		  : p + 1);

	case 'x':
	case 'X':
	  *is_character = false;
	  if (Lex::is_hex_digit(p[1]) && Lex::is_hex_digit(p[2]))
	    {
	      *value = (hex_value(p[1]) << 4) + hex_value(p[2]);
	      return p + 3;
	    }
	  error_at(this->location(), "invalid hex character");
	  return (Lex::is_hex_digit(p[1])
		  ? p + 2
		  : p + 1);

	case 'a':
	  *value = '\a';
	  return p + 1;
	case 'b':
	  *value = '\b';
	  return p + 1;
	case 'f':
	  *value = '\f';
	  return p + 1;
	case 'n':
	  *value = '\n';
	  return p + 1;
	case 'r':
	  *value = '\r';
	  return p + 1;
	case 't':
	  *value = '\t';
	  return p + 1;
	case 'v':
	  *value = '\v';
	  return p + 1;
	case '\\':
	  *value = '\\';
	  return p + 1;
	case '\'':
	  if (!is_single_quote)
	    error_at(this->location(), "invalid quoted character");
	  *value = '\'';
	  return p + 1;
	case '"':
	  if (is_single_quote)
	    error_at(this->location(), "invalid quoted character");
	  *value = '"';
	  return p + 1;

	case 'u':
	  if (Lex::is_hex_digit(p[1]) && Lex::is_hex_digit(p[2])
	      && Lex::is_hex_digit(p[3]) && Lex::is_hex_digit(p[4]))
	    {
	      *value = ((hex_value(p[1]) << 12)
			+ (hex_value(p[2]) << 8)
			+ (hex_value(p[3]) << 4)
			+ hex_value(p[4]));
	      if (*value >= 0xd800 && *value < 0xe000)
		{
		  error_at(this->location(),
			   "invalid unicode code point 0x%x",
			   *value);
		  // Use the replacement character.
		  *value = 0xfffd;
		}
	      return p + 5;
	    }
	  error_at(this->location(), "invalid little unicode code point");
	  return p + 1;

	case 'U':
	  if (Lex::is_hex_digit(p[1]) && Lex::is_hex_digit(p[2])
	      && Lex::is_hex_digit(p[3]) && Lex::is_hex_digit(p[4])
	      && Lex::is_hex_digit(p[5]) && Lex::is_hex_digit(p[6])
	      && Lex::is_hex_digit(p[7]) && Lex::is_hex_digit(p[8]))
	    {
	      *value = ((hex_value(p[1]) << 28)
			+ (hex_value(p[2]) << 24)
			+ (hex_value(p[3]) << 20)
			+ (hex_value(p[4]) << 16)
			+ (hex_value(p[5]) << 12)
			+ (hex_value(p[6]) << 8)
			+ (hex_value(p[7]) << 4)
			+ hex_value(p[8]));
	      if (*value > 0x10ffff
		  || (*value >= 0xd800 && *value < 0xe000))
		{
		  error_at(this->location(), "invalid unicode code point 0x%x",
			   *value);
		  // Use the replacement character.
		  *value = 0xfffd;
		}
	      return p + 9;
	    }
	  error_at(this->location(), "invalid big unicode code point");
	  return p + 1;

	default:
	  error_at(this->location(), "invalid character after %<\\%>");
	  *value = *p;
	  return p + 1;
	}
    }
}

// Append V to STR.  IS_CHARACTER is true for a character which should
// be stored in UTF-8, false for a general byte value which should be
// stored directly.

void
Lex::append_char(unsigned int v, bool is_character, std::string* str,
		 Location location)
{
  char buf[4];
  size_t len;
  if (v <= 0x7f || !is_character)
    {
      buf[0] = v;
      len = 1;
    }
  else if (v <= 0x7ff)
    {
      buf[0] = 0xc0 + (v >> 6);
      buf[1] = 0x80 + (v & 0x3f);
      len = 2;
    }
  else
    {
      if (v > 0x10ffff)
	{
	  warning_at(location, 0,
		     "unicode code point 0x%x out of range in string", v);
	  // Turn it into the "replacement character".
	  v = 0xfffd;
	}
      if (v >= 0xd800 && v < 0xe000)
	{
	  warning_at(location, 0,
		     "unicode code point 0x%x is invalid surrogate pair", v);
	  v = 0xfffd;
	}
      if (v <= 0xffff)
	{
	  buf[0] = 0xe0 + (v >> 12);
	  buf[1] = 0x80 + ((v >> 6) & 0x3f);
	  buf[2] = 0x80 + (v & 0x3f);
	  len = 3;
	}
      else
	{
	  buf[0] = 0xf0 + (v >> 18);
	  buf[1] = 0x80 + ((v >> 12) & 0x3f);
	  buf[2] = 0x80 + ((v >> 6) & 0x3f);
	  buf[3] = 0x80 + (v & 0x3f);
	  len = 4;
	}
    }
  str->append(buf, len);
}

// Pick up a character literal.

Token
Lex::gather_character()
{
  ++this->lineoff_;
  const char* pstart = this->linebuf_ + this->lineoff_;
  const char* p = pstart;

  unsigned int value;
  bool is_character;
  p = this->advance_one_char(p, true, &value, &is_character);

  if (*p != '\'')
    {
      error_at(this->location(), "unterminated character constant");
      this->lineoff_ = p - this->linebuf_;
      return this->make_invalid_token();
    }

  mpz_t val;
  mpz_init_set_ui(val, value);

  Location location = this->location();
  this->lineoff_ = p + 1 - this->linebuf_;
  Token ret = Token::make_character_token(val, location);
  mpz_clear(val);
  return ret;
}

// Pick up a quoted string.

Token
Lex::gather_string()
{
  const char* pstart = this->linebuf_ + this->lineoff_ + 1;
  const char* p = pstart;
  const char* pend = this->linebuf_ + this->linesize_;

  std::string value;
  while (*p != '"')
    {
      Location loc = this->location();
      unsigned int c;
      bool is_character;
      this->lineoff_ = p - this->linebuf_;
      p = this->advance_one_char(p, false, &c, &is_character);
      if (p >= pend)
	{
	  error_at(this->location(), "unterminated string");
	  --p;
	  break;
	}
      Lex::append_char(c, is_character, &value, loc);
    }

  Location location = this->location();
  this->lineoff_ = p + 1 - this->linebuf_;
  return Token::make_string_token(value, location);
}

// Pick up a raw string.

Token
Lex::gather_raw_string()
{
  const char* p = this->linebuf_ + this->lineoff_ + 1;
  const char* pend = this->linebuf_ + this->linesize_;
  Location location = this->location();

  std::string value;
  while (true)
    {
      while (p < pend)
	{
	  if (*p == '`')
	    {
	      this->lineoff_ = p + 1 - this->linebuf_;
	      return Token::make_string_token(value, location);
	    }
	  Location loc = this->location();
	  unsigned int c;
	  bool issued_error;
	  this->lineoff_ = p - this->linebuf_;
	  p = this->advance_one_utf8_char(p, &c, &issued_error);
	  Lex::append_char(c, true, &value, loc);
	}
      this->lineoff_ = p - this->linebuf_;
      if (!this->require_line())
	{
	  error_at(location, "unterminated raw string");
	  return Token::make_string_token(value, location);
	}
      p = this->linebuf_ + this->lineoff_;
      pend = this->linebuf_ + this->linesize_;
    }
}

// If C1 C2 C3 are a three character operator, return the code.

Operator
Lex::three_character_operator(char c1, char c2, char c3)
{
  if (c3 == '=')
    {
      if (c1 == '<' && c2 == '<')
	return OPERATOR_LSHIFTEQ;
      else if (c1 == '>' && c2 == '>')
	return OPERATOR_RSHIFTEQ;
      else if (c1 == '&' && c2 == '^')
	return OPERATOR_BITCLEAREQ;
    }
  return OPERATOR_INVALID;
}

// If C1 C2 are a two character operator, return the code.

Operator
Lex::two_character_operator(char c1, char c2)
{
  switch (c1)
    {
    case '|':
      if (c2 == '|')
	return OPERATOR_OROR;
      else if (c2 == '=')
	return OPERATOR_OREQ;
      break;
    case '&':
      if (c2 == '&')
	return OPERATOR_ANDAND;
      else if (c2 == '^')
	return OPERATOR_BITCLEAR;
      else if (c2 == '=')
	return OPERATOR_ANDEQ;
      break;
    case '^':
      if (c2 == '=')
	return OPERATOR_XOREQ;
      break;
    case '=':
      if (c2 == '=')
	return OPERATOR_EQEQ;
      break;
    case '!':
      if (c2 == '=')
	return OPERATOR_NOTEQ;
      break;
    case '<':
      if (c2 == '=')
	return OPERATOR_LE;
      else if (c2 == '<')
	return OPERATOR_LSHIFT;
      else if (c2 == '-')
	return OPERATOR_CHANOP;
      break;
    case '>':
      if (c2 == '=')
	return OPERATOR_GE;
      else if (c2 == '>')
	return OPERATOR_RSHIFT;
      break;
    case '*':
      if (c2 == '=')
	return OPERATOR_MULTEQ;
      break;
    case '/':
      if (c2 == '=')
	return OPERATOR_DIVEQ;
      break;
    case '%':
      if (c2 == '=')
	return OPERATOR_MODEQ;
      break;
    case '+':
      if (c2 == '+')
	{
	  this->add_semi_at_eol_ = true;
	  return OPERATOR_PLUSPLUS;
	}
      else if (c2 == '=')
	return OPERATOR_PLUSEQ;
      break;
    case '-':
      if (c2 == '-')
	{
	  this->add_semi_at_eol_ = true;
	  return OPERATOR_MINUSMINUS;
	}
      else if (c2 == '=')
	return OPERATOR_MINUSEQ;
      break;
    case ':':
      if (c2 == '=')
	return OPERATOR_COLONEQ;
      break;
    default:
      break;
    }
  return OPERATOR_INVALID;
}

// If character C is an operator, return the code.

Operator
Lex::one_character_operator(char c)
{
  switch (c)
    {
    case '<':
      return OPERATOR_LT;
    case '>':
      return OPERATOR_GT;
    case '+':
      return OPERATOR_PLUS;
    case '-':
      return OPERATOR_MINUS;
    case '|':
      return OPERATOR_OR;
    case '^':
      return OPERATOR_XOR;
    case '*':
      return OPERATOR_MULT;
    case '/':
      return OPERATOR_DIV;
    case '%':
      return OPERATOR_MOD;
    case '&':
      return OPERATOR_AND;
    case '!':
      return OPERATOR_NOT;
    case '=':
      return OPERATOR_EQ;
    case ':':
      return OPERATOR_COLON;
    case ';':
      return OPERATOR_SEMICOLON;
    case '.':
      return OPERATOR_DOT;
    case ',':
      return OPERATOR_COMMA;
    case '(':
      return OPERATOR_LPAREN;
    case ')':
      this->add_semi_at_eol_ = true;
      return OPERATOR_RPAREN;
    case '{':
      return OPERATOR_LCURLY;
    case '}':
      this->add_semi_at_eol_ = true;
      return OPERATOR_RCURLY;
    case '[':
      return OPERATOR_LSQUARE;
    case ']':
      this->add_semi_at_eol_ = true;
      return OPERATOR_RSQUARE;
    default:
      return OPERATOR_INVALID;
    }
}

// Skip a C-style comment.

bool
Lex::skip_c_comment()
{
  while (true)
    {
      if (!this->require_line())
	{
	  error_at(this->location(), "unterminated comment");
	  return false;
	}

      const char* p = this->linebuf_ + this->lineoff_;
      const char* pend = this->linebuf_ + this->linesize_;

      while (p < pend)
	{
	  if (p[0] == '*' && p + 1 < pend && p[1] == '/')
	    {
	      this->lineoff_ = p + 2 - this->linebuf_;
	      return true;
	    }

	  this->lineoff_ = p - this->linebuf_;
	  unsigned int c;
	  bool issued_error;
	  p = this->advance_one_utf8_char(p, &c, &issued_error);
	}

      this->lineoff_ = p - this->linebuf_;
    }
}

// Skip a C++-style comment.

void
Lex::skip_cpp_comment()
{
  // Ensure that if EXTERN_ is set, it means that we just saw a
  // //extern comment.
  this->extern_.clear();

  const char* p = this->linebuf_ + this->lineoff_;
  const char* pend = this->linebuf_ + this->linesize_;

  // By convention, a C++ comment at the start of the line of the form
  //   //line FILE:LINENO
  // is interpreted as setting the file name and line number of the
  // next source line.

  if (this->lineoff_ == 2
      && pend - p > 5
      && memcmp(p, "line ", 5) == 0)
    {
      p += 5;
      while (p < pend && *p == ' ')
	++p;
      const char* pcolon = static_cast<const char*>(memchr(p, ':', pend - p));
      if (pcolon != NULL
	  && pcolon[1] >= '0'
	  && pcolon[1] <= '9')
	{
	  char* plend;
	  long lineno = strtol(pcolon + 1, &plend, 10);
	  if (plend > pcolon + 1
	      && (plend == pend
		  || *plend < '0'
		  || *plend > '9')
	      && lineno > 0
	      && lineno < 0x7fffffff)
	    {
	      unsigned int filelen = pcolon - p;
	      char* file = new char[filelen + 1];
	      memcpy(file, p, filelen);
	      file[filelen] = '\0';

              this->linemap_->start_file(file, lineno);
	      this->lineno_ = lineno - 1;

	      p = plend;
	    }
	}
    }

  // As a special gccgo extension, a C++ comment at the start of the
  // line of the form
  //   //extern NAME
  // which immediately precedes a function declaration means that the
  // external name of the function declaration is NAME.  This is
  // normally used to permit Go code to call a C function.
  if (this->lineoff_ == 2
      && pend - p > 7
      && memcmp(p, "extern ", 7) == 0)
    {
      p += 7;
      while (p < pend && (*p == ' ' || *p == '\t'))
	++p;
      const char* plend = pend;
      while (plend > p
	     && (plend[-1] == ' ' || plend[-1] == '\t' || plend[-1] == '\n'))
	--plend;
      if (plend > p)
	this->extern_ = std::string(p, plend - p);
    }

  // For field tracking analysis: a //go:nointerface comment means
  // that the next interface method should not be stored in the type
  // descriptor.  This permits it to be discarded if it is not needed.
  if (this->lineoff_ == 2 && memcmp(p, "go:nointerface", 14) == 0)
    this->saw_nointerface_ = true;

  while (p < pend)
    {
      this->lineoff_ = p - this->linebuf_;
      unsigned int c;
      bool issued_error;
      p = this->advance_one_utf8_char(p, &c, &issued_error);
      if (issued_error)
	this->extern_.clear();
    }
}

// The Unicode tables use this struct.

struct Unicode_range
{
  // The low end of the range.
  unsigned int low;
  // The high end of the range.
  unsigned int high;
  // The stride.  This entries represents low, low + stride, low + 2 *
  // stride, etc., up to high.
  unsigned int stride;
};

// A table of whitespace characters--Unicode code points classified as
// "Space", "C" locale whitespace characters, the "next line" control
// character (0085), the line separator (2028), the paragraph
// separator (2029), and the "zero-width non-break space" (feff).

static const Unicode_range unicode_space[] =
{
  { 0x0009, 0x000d, 1 },
  { 0x0020, 0x0020, 1 },
  { 0x0085, 0x0085, 1 },
  { 0x00a0, 0x00a0, 1 },
  { 0x1680, 0x1680, 1 },
  { 0x180e, 0x180e, 1 },
  { 0x2000, 0x200a, 1 },
  { 0x2028, 0x2029, 1 },
  { 0x202f, 0x202f, 1 },
  { 0x205f, 0x205f, 1 },
  { 0x3000, 0x3000, 1 },
  { 0xfeff, 0xfeff, 1 },
};

// A table of Unicode digits--Unicode code points classified as
// "Digit".

static const Unicode_range unicode_digits[] =
{
  { 0x0030, 0x0039, 1},
  { 0x0660, 0x0669, 1},
  { 0x06f0, 0x06f9, 1},
  { 0x07c0, 0x07c9, 1},
  { 0x0966, 0x096f, 1},
  { 0x09e6, 0x09ef, 1},
  { 0x0a66, 0x0a6f, 1},
  { 0x0ae6, 0x0aef, 1},
  { 0x0b66, 0x0b6f, 1},
  { 0x0be6, 0x0bef, 1},
  { 0x0c66, 0x0c6f, 1},
  { 0x0ce6, 0x0cef, 1},
  { 0x0d66, 0x0d6f, 1},
  { 0x0e50, 0x0e59, 1},
  { 0x0ed0, 0x0ed9, 1},
  { 0x0f20, 0x0f29, 1},
  { 0x1040, 0x1049, 1},
  { 0x17e0, 0x17e9, 1},
  { 0x1810, 0x1819, 1},
  { 0x1946, 0x194f, 1},
  { 0x19d0, 0x19d9, 1},
  { 0x1b50, 0x1b59, 1},
  { 0xff10, 0xff19, 1},
  { 0x104a0, 0x104a9, 1},
  { 0x1d7ce, 0x1d7ff, 1},
};

// A table of Unicode letters--Unicode code points classified as
// "Letter".

static const Unicode_range unicode_letters[] =
{
  { 0x0041, 0x005a, 1},
  { 0x0061, 0x007a, 1},
  { 0x00aa, 0x00b5, 11},
  { 0x00ba, 0x00ba, 1},
  { 0x00c0, 0x00d6, 1},
  { 0x00d8, 0x00f6, 1},
  { 0x00f8, 0x02c1, 1},
  { 0x02c6, 0x02d1, 1},
  { 0x02e0, 0x02e4, 1},
  { 0x02ec, 0x02ee, 2},
  { 0x0370, 0x0374, 1},
  { 0x0376, 0x0377, 1},
  { 0x037a, 0x037d, 1},
  { 0x0386, 0x0386, 1},
  { 0x0388, 0x038a, 1},
  { 0x038c, 0x038c, 1},
  { 0x038e, 0x03a1, 1},
  { 0x03a3, 0x03f5, 1},
  { 0x03f7, 0x0481, 1},
  { 0x048a, 0x0523, 1},
  { 0x0531, 0x0556, 1},
  { 0x0559, 0x0559, 1},
  { 0x0561, 0x0587, 1},
  { 0x05d0, 0x05ea, 1},
  { 0x05f0, 0x05f2, 1},
  { 0x0621, 0x064a, 1},
  { 0x066e, 0x066f, 1},
  { 0x0671, 0x06d3, 1},
  { 0x06d5, 0x06d5, 1},
  { 0x06e5, 0x06e6, 1},
  { 0x06ee, 0x06ef, 1},
  { 0x06fa, 0x06fc, 1},
  { 0x06ff, 0x0710, 17},
  { 0x0712, 0x072f, 1},
  { 0x074d, 0x07a5, 1},
  { 0x07b1, 0x07b1, 1},
  { 0x07ca, 0x07ea, 1},
  { 0x07f4, 0x07f5, 1},
  { 0x07fa, 0x07fa, 1},
  { 0x0904, 0x0939, 1},
  { 0x093d, 0x0950, 19},
  { 0x0958, 0x0961, 1},
  { 0x0971, 0x0972, 1},
  { 0x097b, 0x097f, 1},
  { 0x0985, 0x098c, 1},
  { 0x098f, 0x0990, 1},
  { 0x0993, 0x09a8, 1},
  { 0x09aa, 0x09b0, 1},
  { 0x09b2, 0x09b2, 1},
  { 0x09b6, 0x09b9, 1},
  { 0x09bd, 0x09ce, 17},
  { 0x09dc, 0x09dd, 1},
  { 0x09df, 0x09e1, 1},
  { 0x09f0, 0x09f1, 1},
  { 0x0a05, 0x0a0a, 1},
  { 0x0a0f, 0x0a10, 1},
  { 0x0a13, 0x0a28, 1},
  { 0x0a2a, 0x0a30, 1},
  { 0x0a32, 0x0a33, 1},
  { 0x0a35, 0x0a36, 1},
  { 0x0a38, 0x0a39, 1},
  { 0x0a59, 0x0a5c, 1},
  { 0x0a5e, 0x0a5e, 1},
  { 0x0a72, 0x0a74, 1},
  { 0x0a85, 0x0a8d, 1},
  { 0x0a8f, 0x0a91, 1},
  { 0x0a93, 0x0aa8, 1},
  { 0x0aaa, 0x0ab0, 1},
  { 0x0ab2, 0x0ab3, 1},
  { 0x0ab5, 0x0ab9, 1},
  { 0x0abd, 0x0ad0, 19},
  { 0x0ae0, 0x0ae1, 1},
  { 0x0b05, 0x0b0c, 1},
  { 0x0b0f, 0x0b10, 1},
  { 0x0b13, 0x0b28, 1},
  { 0x0b2a, 0x0b30, 1},
  { 0x0b32, 0x0b33, 1},
  { 0x0b35, 0x0b39, 1},
  { 0x0b3d, 0x0b3d, 1},
  { 0x0b5c, 0x0b5d, 1},
  { 0x0b5f, 0x0b61, 1},
  { 0x0b71, 0x0b83, 18},
  { 0x0b85, 0x0b8a, 1},
  { 0x0b8e, 0x0b90, 1},
  { 0x0b92, 0x0b95, 1},
  { 0x0b99, 0x0b9a, 1},
  { 0x0b9c, 0x0b9c, 1},
  { 0x0b9e, 0x0b9f, 1},
  { 0x0ba3, 0x0ba4, 1},
  { 0x0ba8, 0x0baa, 1},
  { 0x0bae, 0x0bb9, 1},
  { 0x0bd0, 0x0bd0, 1},
  { 0x0c05, 0x0c0c, 1},
  { 0x0c0e, 0x0c10, 1},
  { 0x0c12, 0x0c28, 1},
  { 0x0c2a, 0x0c33, 1},
  { 0x0c35, 0x0c39, 1},
  { 0x0c3d, 0x0c3d, 1},
  { 0x0c58, 0x0c59, 1},
  { 0x0c60, 0x0c61, 1},
  { 0x0c85, 0x0c8c, 1},
  { 0x0c8e, 0x0c90, 1},
  { 0x0c92, 0x0ca8, 1},
  { 0x0caa, 0x0cb3, 1},
  { 0x0cb5, 0x0cb9, 1},
  { 0x0cbd, 0x0cde, 33},
  { 0x0ce0, 0x0ce1, 1},
  { 0x0d05, 0x0d0c, 1},
  { 0x0d0e, 0x0d10, 1},
  { 0x0d12, 0x0d28, 1},
  { 0x0d2a, 0x0d39, 1},
  { 0x0d3d, 0x0d3d, 1},
  { 0x0d60, 0x0d61, 1},
  { 0x0d7a, 0x0d7f, 1},
  { 0x0d85, 0x0d96, 1},
  { 0x0d9a, 0x0db1, 1},
  { 0x0db3, 0x0dbb, 1},
  { 0x0dbd, 0x0dbd, 1},
  { 0x0dc0, 0x0dc6, 1},
  { 0x0e01, 0x0e30, 1},
  { 0x0e32, 0x0e33, 1},
  { 0x0e40, 0x0e46, 1},
  { 0x0e81, 0x0e82, 1},
  { 0x0e84, 0x0e84, 1},
  { 0x0e87, 0x0e88, 1},
  { 0x0e8a, 0x0e8d, 3},
  { 0x0e94, 0x0e97, 1},
  { 0x0e99, 0x0e9f, 1},
  { 0x0ea1, 0x0ea3, 1},
  { 0x0ea5, 0x0ea7, 2},
  { 0x0eaa, 0x0eab, 1},
  { 0x0ead, 0x0eb0, 1},
  { 0x0eb2, 0x0eb3, 1},
  { 0x0ebd, 0x0ebd, 1},
  { 0x0ec0, 0x0ec4, 1},
  { 0x0ec6, 0x0ec6, 1},
  { 0x0edc, 0x0edd, 1},
  { 0x0f00, 0x0f00, 1},
  { 0x0f40, 0x0f47, 1},
  { 0x0f49, 0x0f6c, 1},
  { 0x0f88, 0x0f8b, 1},
  { 0x1000, 0x102a, 1},
  { 0x103f, 0x103f, 1},
  { 0x1050, 0x1055, 1},
  { 0x105a, 0x105d, 1},
  { 0x1061, 0x1061, 1},
  { 0x1065, 0x1066, 1},
  { 0x106e, 0x1070, 1},
  { 0x1075, 0x1081, 1},
  { 0x108e, 0x108e, 1},
  { 0x10a0, 0x10c5, 1},
  { 0x10d0, 0x10fa, 1},
  { 0x10fc, 0x10fc, 1},
  { 0x1100, 0x1159, 1},
  { 0x115f, 0x11a2, 1},
  { 0x11a8, 0x11f9, 1},
  { 0x1200, 0x1248, 1},
  { 0x124a, 0x124d, 1},
  { 0x1250, 0x1256, 1},
  { 0x1258, 0x1258, 1},
  { 0x125a, 0x125d, 1},
  { 0x1260, 0x1288, 1},
  { 0x128a, 0x128d, 1},
  { 0x1290, 0x12b0, 1},
  { 0x12b2, 0x12b5, 1},
  { 0x12b8, 0x12be, 1},
  { 0x12c0, 0x12c0, 1},
  { 0x12c2, 0x12c5, 1},
  { 0x12c8, 0x12d6, 1},
  { 0x12d8, 0x1310, 1},
  { 0x1312, 0x1315, 1},
  { 0x1318, 0x135a, 1},
  { 0x1380, 0x138f, 1},
  { 0x13a0, 0x13f4, 1},
  { 0x1401, 0x166c, 1},
  { 0x166f, 0x1676, 1},
  { 0x1681, 0x169a, 1},
  { 0x16a0, 0x16ea, 1},
  { 0x1700, 0x170c, 1},
  { 0x170e, 0x1711, 1},
  { 0x1720, 0x1731, 1},
  { 0x1740, 0x1751, 1},
  { 0x1760, 0x176c, 1},
  { 0x176e, 0x1770, 1},
  { 0x1780, 0x17b3, 1},
  { 0x17d7, 0x17dc, 5},
  { 0x1820, 0x1877, 1},
  { 0x1880, 0x18a8, 1},
  { 0x18aa, 0x18aa, 1},
  { 0x1900, 0x191c, 1},
  { 0x1950, 0x196d, 1},
  { 0x1970, 0x1974, 1},
  { 0x1980, 0x19a9, 1},
  { 0x19c1, 0x19c7, 1},
  { 0x1a00, 0x1a16, 1},
  { 0x1b05, 0x1b33, 1},
  { 0x1b45, 0x1b4b, 1},
  { 0x1b83, 0x1ba0, 1},
  { 0x1bae, 0x1baf, 1},
  { 0x1c00, 0x1c23, 1},
  { 0x1c4d, 0x1c4f, 1},
  { 0x1c5a, 0x1c7d, 1},
  { 0x1d00, 0x1dbf, 1},
  { 0x1e00, 0x1f15, 1},
  { 0x1f18, 0x1f1d, 1},
  { 0x1f20, 0x1f45, 1},
  { 0x1f48, 0x1f4d, 1},
  { 0x1f50, 0x1f57, 1},
  { 0x1f59, 0x1f5d, 2},
  { 0x1f5f, 0x1f7d, 1},
  { 0x1f80, 0x1fb4, 1},
  { 0x1fb6, 0x1fbc, 1},
  { 0x1fbe, 0x1fbe, 1},
  { 0x1fc2, 0x1fc4, 1},
  { 0x1fc6, 0x1fcc, 1},
  { 0x1fd0, 0x1fd3, 1},
  { 0x1fd6, 0x1fdb, 1},
  { 0x1fe0, 0x1fec, 1},
  { 0x1ff2, 0x1ff4, 1},
  { 0x1ff6, 0x1ffc, 1},
  { 0x2071, 0x207f, 14},
  { 0x2090, 0x2094, 1},
  { 0x2102, 0x2107, 5},
  { 0x210a, 0x2113, 1},
  { 0x2115, 0x2115, 1},
  { 0x2119, 0x211d, 1},
  { 0x2124, 0x2128, 2},
  { 0x212a, 0x212d, 1},
  { 0x212f, 0x2139, 1},
  { 0x213c, 0x213f, 1},
  { 0x2145, 0x2149, 1},
  { 0x214e, 0x214e, 1},
  { 0x2183, 0x2184, 1},
  { 0x2c00, 0x2c2e, 1},
  { 0x2c30, 0x2c5e, 1},
  { 0x2c60, 0x2c6f, 1},
  { 0x2c71, 0x2c7d, 1},
  { 0x2c80, 0x2ce4, 1},
  { 0x2d00, 0x2d25, 1},
  { 0x2d30, 0x2d65, 1},
  { 0x2d6f, 0x2d6f, 1},
  { 0x2d80, 0x2d96, 1},
  { 0x2da0, 0x2da6, 1},
  { 0x2da8, 0x2dae, 1},
  { 0x2db0, 0x2db6, 1},
  { 0x2db8, 0x2dbe, 1},
  { 0x2dc0, 0x2dc6, 1},
  { 0x2dc8, 0x2dce, 1},
  { 0x2dd0, 0x2dd6, 1},
  { 0x2dd8, 0x2dde, 1},
  { 0x2e2f, 0x2e2f, 1},
  { 0x3005, 0x3006, 1},
  { 0x3031, 0x3035, 1},
  { 0x303b, 0x303c, 1},
  { 0x3041, 0x3096, 1},
  { 0x309d, 0x309f, 1},
  { 0x30a1, 0x30fa, 1},
  { 0x30fc, 0x30ff, 1},
  { 0x3105, 0x312d, 1},
  { 0x3131, 0x318e, 1},
  { 0x31a0, 0x31b7, 1},
  { 0x31f0, 0x31ff, 1},
  { 0x3400, 0x4db5, 1},
  { 0x4e00, 0x9fc3, 1},
  { 0xa000, 0xa48c, 1},
  { 0xa500, 0xa60c, 1},
  { 0xa610, 0xa61f, 1},
  { 0xa62a, 0xa62b, 1},
  { 0xa640, 0xa65f, 1},
  { 0xa662, 0xa66e, 1},
  { 0xa67f, 0xa697, 1},
  { 0xa717, 0xa71f, 1},
  { 0xa722, 0xa788, 1},
  { 0xa78b, 0xa78c, 1},
  { 0xa7fb, 0xa801, 1},
  { 0xa803, 0xa805, 1},
  { 0xa807, 0xa80a, 1},
  { 0xa80c, 0xa822, 1},
  { 0xa840, 0xa873, 1},
  { 0xa882, 0xa8b3, 1},
  { 0xa90a, 0xa925, 1},
  { 0xa930, 0xa946, 1},
  { 0xaa00, 0xaa28, 1},
  { 0xaa40, 0xaa42, 1},
  { 0xaa44, 0xaa4b, 1},
  { 0xac00, 0xd7a3, 1},
  { 0xf900, 0xfa2d, 1},
  { 0xfa30, 0xfa6a, 1},
  { 0xfa70, 0xfad9, 1},
  { 0xfb00, 0xfb06, 1},
  { 0xfb13, 0xfb17, 1},
  { 0xfb1d, 0xfb1d, 1},
  { 0xfb1f, 0xfb28, 1},
  { 0xfb2a, 0xfb36, 1},
  { 0xfb38, 0xfb3c, 1},
  { 0xfb3e, 0xfb3e, 1},
  { 0xfb40, 0xfb41, 1},
  { 0xfb43, 0xfb44, 1},
  { 0xfb46, 0xfbb1, 1},
  { 0xfbd3, 0xfd3d, 1},
  { 0xfd50, 0xfd8f, 1},
  { 0xfd92, 0xfdc7, 1},
  { 0xfdf0, 0xfdfb, 1},
  { 0xfe70, 0xfe74, 1},
  { 0xfe76, 0xfefc, 1},
  { 0xff21, 0xff3a, 1},
  { 0xff41, 0xff5a, 1},
  { 0xff66, 0xffbe, 1},
  { 0xffc2, 0xffc7, 1},
  { 0xffca, 0xffcf, 1},
  { 0xffd2, 0xffd7, 1},
  { 0xffda, 0xffdc, 1},
  { 0x10000, 0x1000b, 1},
  { 0x1000d, 0x10026, 1},
  { 0x10028, 0x1003a, 1},
  { 0x1003c, 0x1003d, 1},
  { 0x1003f, 0x1004d, 1},
  { 0x10050, 0x1005d, 1},
  { 0x10080, 0x100fa, 1},
  { 0x10280, 0x1029c, 1},
  { 0x102a0, 0x102d0, 1},
  { 0x10300, 0x1031e, 1},
  { 0x10330, 0x10340, 1},
  { 0x10342, 0x10349, 1},
  { 0x10380, 0x1039d, 1},
  { 0x103a0, 0x103c3, 1},
  { 0x103c8, 0x103cf, 1},
  { 0x10400, 0x1049d, 1},
  { 0x10800, 0x10805, 1},
  { 0x10808, 0x10808, 1},
  { 0x1080a, 0x10835, 1},
  { 0x10837, 0x10838, 1},
  { 0x1083c, 0x1083f, 3},
  { 0x10900, 0x10915, 1},
  { 0x10920, 0x10939, 1},
  { 0x10a00, 0x10a00, 1},
  { 0x10a10, 0x10a13, 1},
  { 0x10a15, 0x10a17, 1},
  { 0x10a19, 0x10a33, 1},
  { 0x12000, 0x1236e, 1},
  { 0x1d400, 0x1d454, 1},
  { 0x1d456, 0x1d49c, 1},
  { 0x1d49e, 0x1d49f, 1},
  { 0x1d4a2, 0x1d4a2, 1},
  { 0x1d4a5, 0x1d4a6, 1},
  { 0x1d4a9, 0x1d4ac, 1},
  { 0x1d4ae, 0x1d4b9, 1},
  { 0x1d4bb, 0x1d4bb, 1},
  { 0x1d4bd, 0x1d4c3, 1},
  { 0x1d4c5, 0x1d505, 1},
  { 0x1d507, 0x1d50a, 1},
  { 0x1d50d, 0x1d514, 1},
  { 0x1d516, 0x1d51c, 1},
  { 0x1d51e, 0x1d539, 1},
  { 0x1d53b, 0x1d53e, 1},
  { 0x1d540, 0x1d544, 1},
  { 0x1d546, 0x1d546, 1},
  { 0x1d54a, 0x1d550, 1},
  { 0x1d552, 0x1d6a5, 1},
  { 0x1d6a8, 0x1d6c0, 1},
  { 0x1d6c2, 0x1d6da, 1},
  { 0x1d6dc, 0x1d6fa, 1},
  { 0x1d6fc, 0x1d714, 1},
  { 0x1d716, 0x1d734, 1},
  { 0x1d736, 0x1d74e, 1},
  { 0x1d750, 0x1d76e, 1},
  { 0x1d770, 0x1d788, 1},
  { 0x1d78a, 0x1d7a8, 1},
  { 0x1d7aa, 0x1d7c2, 1},
  { 0x1d7c4, 0x1d7cb, 1},
  { 0x20000, 0x2a6d6, 1},
  { 0x2f800, 0x2fa1d, 1},
};

// A table of Unicode uppercase letters--Unicode code points
// classified as "Letter, uppercase".

static const Unicode_range unicode_uppercase_letters[] =
{
  { 0x0041, 0x005a, 1},
  { 0x00c0, 0x00d6, 1},
  { 0x00d8, 0x00de, 1},
  { 0x0100, 0x0136, 2},
  { 0x0139, 0x0147, 2},
  { 0x014a, 0x0176, 2},
  { 0x0178, 0x0179, 1},
  { 0x017b, 0x017d, 2},
  { 0x0181, 0x0182, 1},
  { 0x0184, 0x0184, 1},
  { 0x0186, 0x0187, 1},
  { 0x0189, 0x018b, 1},
  { 0x018e, 0x0191, 1},
  { 0x0193, 0x0194, 1},
  { 0x0196, 0x0198, 1},
  { 0x019c, 0x019d, 1},
  { 0x019f, 0x01a0, 1},
  { 0x01a2, 0x01a4, 2},
  { 0x01a6, 0x01a7, 1},
  { 0x01a9, 0x01ac, 3},
  { 0x01ae, 0x01af, 1},
  { 0x01b1, 0x01b3, 1},
  { 0x01b5, 0x01b5, 1},
  { 0x01b7, 0x01b8, 1},
  { 0x01bc, 0x01c4, 8},
  { 0x01c7, 0x01cd, 3},
  { 0x01cf, 0x01db, 2},
  { 0x01de, 0x01ee, 2},
  { 0x01f1, 0x01f4, 3},
  { 0x01f6, 0x01f8, 1},
  { 0x01fa, 0x0232, 2},
  { 0x023a, 0x023b, 1},
  { 0x023d, 0x023e, 1},
  { 0x0241, 0x0241, 1},
  { 0x0243, 0x0246, 1},
  { 0x0248, 0x024e, 2},
  { 0x0370, 0x0372, 2},
  { 0x0376, 0x0386, 16},
  { 0x0388, 0x038a, 1},
  { 0x038c, 0x038c, 1},
  { 0x038e, 0x038f, 1},
  { 0x0391, 0x03a1, 1},
  { 0x03a3, 0x03ab, 1},
  { 0x03cf, 0x03cf, 1},
  { 0x03d2, 0x03d4, 1},
  { 0x03d8, 0x03ee, 2},
  { 0x03f4, 0x03f7, 3},
  { 0x03f9, 0x03fa, 1},
  { 0x03fd, 0x042f, 1},
  { 0x0460, 0x0480, 2},
  { 0x048a, 0x04be, 2},
  { 0x04c0, 0x04c1, 1},
  { 0x04c3, 0x04cd, 2},
  { 0x04d0, 0x0522, 2},
  { 0x0531, 0x0556, 1},
  { 0x10a0, 0x10c5, 1},
  { 0x1e00, 0x1e94, 2},
  { 0x1e9e, 0x1efe, 2},
  { 0x1f08, 0x1f0f, 1},
  { 0x1f18, 0x1f1d, 1},
  { 0x1f28, 0x1f2f, 1},
  { 0x1f38, 0x1f3f, 1},
  { 0x1f48, 0x1f4d, 1},
  { 0x1f59, 0x1f5f, 2},
  { 0x1f68, 0x1f6f, 1},
  { 0x1fb8, 0x1fbb, 1},
  { 0x1fc8, 0x1fcb, 1},
  { 0x1fd8, 0x1fdb, 1},
  { 0x1fe8, 0x1fec, 1},
  { 0x1ff8, 0x1ffb, 1},
  { 0x2102, 0x2107, 5},
  { 0x210b, 0x210d, 1},
  { 0x2110, 0x2112, 1},
  { 0x2115, 0x2115, 1},
  { 0x2119, 0x211d, 1},
  { 0x2124, 0x2128, 2},
  { 0x212a, 0x212d, 1},
  { 0x2130, 0x2133, 1},
  { 0x213e, 0x213f, 1},
  { 0x2145, 0x2183, 62},
  { 0x2c00, 0x2c2e, 1},
  { 0x2c60, 0x2c60, 1},
  { 0x2c62, 0x2c64, 1},
  { 0x2c67, 0x2c6b, 2},
  { 0x2c6d, 0x2c6f, 1},
  { 0x2c72, 0x2c75, 3},
  { 0x2c80, 0x2ce2, 2},
  { 0xa640, 0xa65e, 2},
  { 0xa662, 0xa66c, 2},
  { 0xa680, 0xa696, 2},
  { 0xa722, 0xa72e, 2},
  { 0xa732, 0xa76e, 2},
  { 0xa779, 0xa77b, 2},
  { 0xa77d, 0xa77e, 1},
  { 0xa780, 0xa786, 2},
  { 0xa78b, 0xa78b, 1},
  { 0xff21, 0xff3a, 1},
  { 0x10400, 0x10427, 1},
  { 0x1d400, 0x1d419, 1},
  { 0x1d434, 0x1d44d, 1},
  { 0x1d468, 0x1d481, 1},
  { 0x1d49c, 0x1d49c, 1},
  { 0x1d49e, 0x1d49f, 1},
  { 0x1d4a2, 0x1d4a2, 1},
  { 0x1d4a5, 0x1d4a6, 1},
  { 0x1d4a9, 0x1d4ac, 1},
  { 0x1d4ae, 0x1d4b5, 1},
  { 0x1d4d0, 0x1d4e9, 1},
  { 0x1d504, 0x1d505, 1},
  { 0x1d507, 0x1d50a, 1},
  { 0x1d50d, 0x1d514, 1},
  { 0x1d516, 0x1d51c, 1},
  { 0x1d538, 0x1d539, 1},
  { 0x1d53b, 0x1d53e, 1},
  { 0x1d540, 0x1d544, 1},
  { 0x1d546, 0x1d546, 1},
  { 0x1d54a, 0x1d550, 1},
  { 0x1d56c, 0x1d585, 1},
  { 0x1d5a0, 0x1d5b9, 1},
  { 0x1d5d4, 0x1d5ed, 1},
  { 0x1d608, 0x1d621, 1},
  { 0x1d63c, 0x1d655, 1},
  { 0x1d670, 0x1d689, 1},
  { 0x1d6a8, 0x1d6c0, 1},
  { 0x1d6e2, 0x1d6fa, 1},
  { 0x1d71c, 0x1d734, 1},
  { 0x1d756, 0x1d76e, 1},
  { 0x1d790, 0x1d7a8, 1},
  { 0x1d7ca, 0x1d7ca, 1},
};

// Return true if C is in RANGES.

bool
Lex::is_in_unicode_range(unsigned int c, const Unicode_range* ranges,
			 size_t range_size)
{
  if (c < 0x100)
    {
      // The common case is a small value, and we know that it will be
      // in the first few entries of the table.  Do a linear scan
      // rather than a binary search.
      for (size_t i = 0; i < range_size; ++i)
	{
	  const Unicode_range* p = &ranges[i];
	  if (c <= p->high)
	    {
	      if (c < p->low)
		return false;
	      return (c - p->low) % p->stride == 0;
	    }
	}
      return false;
    }
  else
    {
      size_t lo = 0;
      size_t hi = range_size;
      while (lo < hi)
	{
	  size_t mid = lo + (hi - lo) / 2;
	  const Unicode_range* p = &ranges[mid];
	  if (c < p->low)
	    hi = mid;
	  else if (c > p->high)
	    lo = mid + 1;
	  else
	    return (c - p->low) % p->stride == 0;
	}
      return false;
    }
}

// Return whether C is a space character.

bool
Lex::is_unicode_space(unsigned int c)
{
  return Lex::is_in_unicode_range(c, unicode_space,
				  ARRAY_SIZE(unicode_space));
}

// Return whether C is a Unicode digit--a Unicode code point
// classified as "Digit".

bool
Lex::is_unicode_digit(unsigned int c)
{
  return Lex::is_in_unicode_range(c, unicode_digits,
				  ARRAY_SIZE(unicode_digits));
}

// Return whether C is a Unicode letter--a Unicode code point
// classified as "Letter".

bool
Lex::is_unicode_letter(unsigned int c)
{
  return Lex::is_in_unicode_range(c, unicode_letters,
				  ARRAY_SIZE(unicode_letters));
}

// Return whether C is a Unicode uppercase letter.  a Unicode code
// point classified as "Letter, uppercase".

bool
Lex::is_unicode_uppercase(unsigned int c)
{
  return Lex::is_in_unicode_range(c, unicode_uppercase_letters,
				  ARRAY_SIZE(unicode_uppercase_letters));
}

// Return whether the identifier NAME should be exported.  NAME is a
// mangled name which includes only ASCII characters.

bool
Lex::is_exported_name(const std::string& name)
{
  unsigned char c = name[0];
  if (c != '$')
    return c >= 'A' && c <= 'Z';
  else
    {
      const char* p = name.data();
      size_t len = name.length();
      if (len < 2 || p[1] != 'U')
	return false;
      unsigned int ci = 0;
      for (size_t i = 2; i < len && p[i] != '$'; ++i)
	{
	  c = p[i];
	  if (!hex_p(c))
	    return false;
	  ci <<= 4;
	  ci |= hex_value(c);
	}
      return Lex::is_unicode_uppercase(ci);
    }
}

// Return whether the identifier NAME contains an invalid character.
// This is based on how we handle invalid characters in
// gather_identifier.

bool
Lex::is_invalid_identifier(const std::string& name)
{
  return name.find("$INVALID$") != std::string::npos;
}
