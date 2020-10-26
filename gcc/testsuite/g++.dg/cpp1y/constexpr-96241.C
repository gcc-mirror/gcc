// PR c++/96241
// { dg-do compile { target c++14 } }

#define assert(expr) static_assert (expr, #expr)

enum E { o };

struct S {
  int e = o;
};

using T = S[3];

constexpr struct S s[1][1][1] = { };
assert (0 == s[0][0][0].e);

constexpr int
fn0 ()
{
  return T{}[0].e;
}
assert(fn0 () == 0);

constexpr int
fn1 ()
{
  S d[1];
  int x = d[0].e;
  return x;
}
assert(fn1 () == 0);

constexpr int
fn2 ()
{
  S d[1];
  return d[0].e;
}
assert(fn2 () == 0);

constexpr int
fn3 ()
{
  struct X { int e = o; } d[1]{};
  return d[0].e;
}
assert(fn3 () == 0);
