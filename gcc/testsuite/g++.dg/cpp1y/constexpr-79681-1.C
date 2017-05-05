// PR c++/79681
// { dg-do compile { target c++14 } }
// { dg-options "-O2" }

struct A
{
  int i : 4;
};

constexpr bool
foo ()
{
  A x[] = { 1 };
  return x[0].i;
}

static_assert (foo(), "");
