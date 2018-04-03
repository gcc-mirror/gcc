// PR c++/85113
// { dg-do compile { target c++14 } }

template<bool> struct A {};

constexpr int foo()
{
  A<__builtin_constant_p(0)> a{};
  return 0;
}
