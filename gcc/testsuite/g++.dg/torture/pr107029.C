// PR tree-optimization/107029
// { dg-do compile }

struct S { long long a; int b; };
long long S::*a;
int S::*b;
struct A { void foo (bool, bool); void bar (); int c; };

void
A::foo (bool a, bool b)
{
  c = a || b;
}

void
A::bar()
{
  foo (a, b);
}
