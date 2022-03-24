// PR c++/104568
// { dg-do compile { target c++20 } }
// { dg-options "" }

struct S { int s; constexpr S () : s (0) {} constexpr ~S () {} };
typedef int T[0];
typedef int U[0];

constexpr bool
foo ()
{
  auto p = new T[2];
  auto q1 = &p[0];
  auto q2 = &p[1];
  auto q3 = &p[2];
  delete[] p;
  return true;
}

constexpr bool
bar ()
{
  auto p = new U[2];
  auto q1 = &p[0];
  auto q2 = &p[1];
  auto q3 = &p[2];
  delete[] p;
  return true;
}

constexpr bool
baz ()
{
  auto p = new T[0];
  auto q1 = &p[0];
  delete[] p;
  return true;
}

constexpr bool a = foo ();
constexpr bool b = bar ();
constexpr bool c = baz ();
