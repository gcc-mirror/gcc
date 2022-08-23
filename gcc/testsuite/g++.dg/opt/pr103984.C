// PR middle-end/103984
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wuninitialized" }

struct A {
  char *a;
  char b[4];
  A ();
  A (const A &);
  A (const char *);
  A (const char *, const char *);
  [[gnu::always_inline]] ~A () { if (a != b) delete a; }
};
struct B {
  const char *c = nullptr;
  const char *d = nullptr;
  A qux () const { return A (c, d); }
  B (const char *x) : c(x), d(x) { d += __builtin_strlen (x); }
  B (const B &x) { c = x.c; d = x.d; }
};
struct C { A e; int f; };
extern int baz (B);
void bar (C &&);

void
foo (char **x)
{
  const A g ("foo");
  const B h = x[0];
  bar (C { h.qux (), baz (h) });
}
