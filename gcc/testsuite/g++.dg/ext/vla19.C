// PR c++/83808
// { dg-additional-options "-Wno-vla" }

struct R { int r; };
void baz (char *, char *, char *, char *);

void
foo ()
{
  const R a = { 12 };
  char b[1][a.r] = { { "12345678901" } };
  char c[a.r] = { "12345678901" };
  char d[1][a.r] = { { '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '\0' } };
  char e[a.r] = { '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '\0' };
  baz (b[0], c, d[0], e);
}
