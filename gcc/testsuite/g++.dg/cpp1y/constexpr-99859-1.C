// PR c++/99859
// { dg-do compile { target c++14 } }

constexpr int
foo (int *x)
{
  return ++*x;
}

struct S { constexpr S () : a(0) { foo (&a); foo (&a); } int a; };
constexpr S s = S ();
static_assert (s.a == 2, "");

struct R { int *p; };

constexpr int
bar (R x)
{
  return ++*x.p;
}

struct T { int a = 0; constexpr T () { bar (R{&a}); bar (R{&a}); } };
constexpr T t = T ();
static_assert (t.a == 2, "");
