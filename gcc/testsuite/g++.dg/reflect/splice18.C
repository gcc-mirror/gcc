// PR c++/125631
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<auto N> int tv = N;
struct S { template<int N> static constexpr int vt = N; };

int v1 = template [:^^tv:]<0>;
constexpr int v2 = template [:^^S::vt:]<1>;
static_assert (v2 == 1);

template<auto R>
int
g ()
{
  return template [:R:]<2>;
}

template<auto R>
constexpr int
h ()
{
  return template [:R:]<3>;
}

int v3 = g<^^tv>();
static_assert (h<^^S::vt>() == 3);
