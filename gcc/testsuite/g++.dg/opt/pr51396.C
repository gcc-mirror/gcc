// PR tree-optimization/51396
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }
// { dg-additional-options "-mfma" { target i?86-*-* x86_64-*-* } }

double baz (double) throw ();

struct C
{
  C (double d = 0.0) : c (d) {}
  double c;
};

static inline void
foo (double x, const C &y)
{
  x ? (y.c * baz (x)) : (C (), y);
}

void
bar (double x, C y)
{
  foo (x, y);
}
