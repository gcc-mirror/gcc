// PR c++/118169
// { dg-do compile { target c++14 } }
// { dg-options "-O" }

struct A { constexpr A (int *x) : s(x) {} ~A (); int *s; };
struct B { A t; int u = 0; };
void foo (B &&);

void
bar (int &x)
{
  foo ({ &x });
}
