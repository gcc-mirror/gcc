// PR tree-optimization/71643
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  void *operator new (__SIZE_TYPE__, double);
  void operator delete (void *, double) { __builtin_unreachable (); }
  A (int x);
  static A *bar (int x) { return new (3.0) A (x); }
};
void baz (A *, A *);

void
foo (int a, int b)
{
  A *p = A::bar (a);
  A *q = A::bar (b);
  baz (p, q);
}
