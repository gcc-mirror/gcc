// PR c++/61038
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <cstring>
#include <cstdlib>

void
operator ""_s(const char *, size_t)
{ }

void
operator ""_t(const char)
{ }

#define QUOTE(s) #s
#define QQUOTE(s) QUOTE(s)

int
main()
{
  const char *s = QQUOTE(QUOTE("hello"_s));
  const char *t = QUOTE("\"hello\"_s");
  if (strcmp(s, t) != 0)
    abort();

  const char *c = QQUOTE(QUOTE('"'_t));
  const char *d = QUOTE("'\"'_t");
  if (strcmp(c, d) != 0)
    abort();

  const char *e = QQUOTE(QUOTE('\''_t));
  const char *f = QUOTE("'\\''_t");
  if (strcmp(e, f) != 0)
    abort();

  const char *g = QQUOTE(QUOTE('\\'_t));
  const char *h = QUOTE("'\\\\'_t");
  if (strcmp(g, h) != 0)
    abort();
}
