// PR c++/94038
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O -Wall" }

template<typename T>
constexpr int
foo()
{
  static_assert(T(1) == 0, "");
  return 0;
}

template<typename T>
constexpr int
bar()
{
  return foo<T>();
}

constexpr int
baz(int a)
{
  return a;
}

static_assert(decltype(baz(bar<int>())){} == 0, "");
