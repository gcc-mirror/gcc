// PR tree-optimization/94717
// Reported by Zdenek Sojka <zsojka@seznam.cz>

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -ftracer" }

int abs (int);

static inline void
bar (int d)
{
  d && abs (d);
}

struct S
{
  int a;
  int b;
  int c;
  S (unsigned a, unsigned b) : a (a), b (b) { }
};

void
foo (S *x)
{
  bar (x->c);
  new S (x->a, x->b);
  bar (0);
}
