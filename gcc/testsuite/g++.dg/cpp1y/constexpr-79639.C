// PR c++/79639
// { dg-do compile { target c++14 } }

struct A
{
  void foo () {}
  void bar () {}
};
typedef void (A::*T) ();

constexpr T
foo (T f)
{
  f = 0;
  return f;
}

constexpr T
bar (T f)
{
  f = &A::bar;
  return f;
}

constexpr T a = foo (&A::foo);
constexpr T b = foo (&A::foo);
static_assert (a == nullptr, "");
