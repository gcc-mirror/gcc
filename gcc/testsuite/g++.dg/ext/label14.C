// PR c++/53812
// { dg-do compile }
// { dg-options "" }

struct T { T () : t(0) {}; int t; ~T (); };
struct S { void *operator [] (T); };
void bar (S &, void *, void *);

void
foo (S &x, T &y)
{
  bar (x, &&l1, &&l2);
l1:
  goto *x[y];
l2:
  bar (x, &&l1, &&l2);
}
