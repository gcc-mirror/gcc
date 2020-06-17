// PR c++/94038
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O -Wall" }

static constexpr int x = 0;

template<typename T>
constexpr const int&
foo()
{
  static_assert(T(1) == 0, "");
  return x;
}

template<typename T>
constexpr const int&
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
