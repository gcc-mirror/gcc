// PR c++/90019
// { dg-do compile { target c++11 } }

template<bool, typename T = void>
struct enable_if { };
template<typename T>
struct enable_if<true, T> { typedef T type; };
template<bool C, typename T = void>
using __enable_if_t = typename enable_if<C, T>::type;
template<bool C, typename T = void>
using enable_if_t = typename enable_if<C, T>::type;

template <int I, enable_if_t<I == 0, int>...> void foo() {}
template <int I, enable_if_t<I != 0, int>...> void foo() {}
template <int I, int=0, enable_if_t<I == 0, int>...> void bar() {}
template <int I, int=0, enable_if_t<I != 0, int>...> void bar() {}

void test()
{
  bar<0>();
  bar<0,0>();
  foo<0>();
}
