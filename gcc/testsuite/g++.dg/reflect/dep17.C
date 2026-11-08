// PR c++/127749
// { dg-do compile { target c++26 } }
// { dg-options "-freflection" }

#include <meta>

struct S {
    int mem = 0;
};

template<std::meta::info member>
auto constexpr inline fn = [](auto...) {
    static_assert(std::meta::identifier_of(member) == "mem");
    if constexpr (std::meta::identifier_of(member) == "mem")
        throw 0;
};

template<std::meta::info member>
auto constexpr inline fn2 = []() {
    static_assert(std::meta::identifier_of(member) == "mem");
    if constexpr (std::meta::identifier_of(member) == "mem")
        throw 0;
};

template<class T>
constexpr inline auto foo = [](auto x)
{
  return std::meta::identifier_of(^^x) == "x";
};

void
g ()
{
  fn<^^S::mem>();
  fn2<^^S::mem>();
  constexpr auto a = foo<int>(1);
}

template<typename T>
void
h (T t)
{
  fn<^^S::mem>(t);
}

void
k ()
{
  h (0);
}
