// { dg-additional-options "-Wno-return-type" }

typedef unsigned int source_location;
typedef source_location location_t;
extern void error_at (location_t, const char *, ...)
  __attribute__ ((__format__ (__gcc_tdiag__, 2, 3)))
  __attribute__ ((__nonnull__ (2)));

class Lex
{
  static int fetch_char (const char *str, unsigned int *value);
  location_t location () const;
  const char *advance_one_utf8_char (const char *, unsigned int *, bool *);
  const char *advance_one_char (const char *, bool, unsigned int *, bool *);
  int lineoff_;
  int lineno_;
};

int
Lex::fetch_char (const char *p, unsigned int *value)
{
  unsigned char c = *p;
  if (c <= 0x7f)
    {
      return 1;
    }
  else if ((c & 0xe0) == 0xc0 && (p[1] & 0xc0) == 0x80)
    {
      *value = (((c & 0x1f) << 6) + (p[1] & 0x3f));
    }
  {
    *value = (((c & 0xf) << 12) + (p[2] & 0x3f));
  }
}

const char *
Lex::advance_one_utf8_char (const char *p, unsigned int *value,
			    bool * issued_error)
{
  *issued_error = false;
  if (*p == '\0')
    {
      *issued_error = true;
      return p + 1;
    }
  int adv = Lex::fetch_char (p, value);
  if (*value == 0xfeff && (this->lineno_ != 1 || this->lineoff_ != 0))
    {
      *issued_error = true;
    }
  return p + adv;
}

const char *
Lex::advance_one_char (const char *p, bool is_single_quote,
		       unsigned int *value, bool * is_character)
{
  {
    bool issued_error;
    const char *ret = this->advance_one_utf8_char (p, value, &issued_error);
    if (is_single_quote
	&& (*value == '\'' || *value == '\n') && !issued_error)
      error_at (this->location (), "invalid character literal");
  }

  return 0;
}
