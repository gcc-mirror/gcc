extern "C" {

extern char *strcpy (char *__restrict __dest, __const char *__restrict __src)
     throw () __attribute__ ((__nonnull__ (1, 2)));

extern char *foo (char *__restrict __s) throw ();
}

class cset {
public:
  cset();
  int operator()(unsigned char) const;
private:
  char v[(127 * 2 + 1)+1];
};

inline int cset::operator()(unsigned char c) const
{
  return v[c];
}

extern cset csspace;

void baz()
{
  char *vec;
  char buf[512];

  char *p = buf;
  while (csspace(*p))
    p++;

  if (*p != '#' && (p = foo(buf)) != 0) {
    vec = new char[10+ 1];
    strcpy(vec, p);
  }
}

