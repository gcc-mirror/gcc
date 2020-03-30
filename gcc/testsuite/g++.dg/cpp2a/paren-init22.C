// PR c++/94155 - crash in gimplifier with paren init of aggregates.
// { dg-do compile { target c++2a } }

struct S { int i, j; };

struct A {
  S s;
  constexpr A(S e) : s(e) {}
};

void
f()
{
  A g[1]({{1, 1}});
}
