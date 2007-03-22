// { dg-do assemble  }
// From: panisset@cae.ca (Jean-Francois Panisset)
// Subject: 2.6.0 pre-rel, internal error, regression, mips-sgi-irix4
// Date: Thu, 14 Jul 94 23:34:21 EDT

class Char
{
protected:
  char          rep;
public:
  Char (const char ) {}
  operator char() const;
  void  operator -= (const Char   );
};

inline  Char  operator -  (const Char    a, const Char    b) { return Char(0); }
inline  char  operator == (const Char    a, const char b) { return 0; }

char mystrcmp(Char s[31], Char t[31])
{
  for (; *s == *t; ++s, ++t) if (*s == '\0') return 0;
  return char(*s - *t);
}
