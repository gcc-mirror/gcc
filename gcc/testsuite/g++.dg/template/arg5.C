// PR c++/30534
// { dg-do compile }

template<bool> struct A;

template<int> void foo()
{
  A<__builtin_constant_p(.)> a;  // { dg-error "template argument|invalid" }
}
