// { dg-do compile { target c++14 } }

// PR 78550 ICE with initializer_list and bitfield member

namespace std
{
  template <class T>
  struct initializer_list
    {
      const T *a;
      __SIZE_TYPE__ b;
      constexpr initializer_list (const T *x, __SIZE_TYPE__ y) : a(x), b(y) { }
    };
}
template <typename T>
struct A {
  A (std::initializer_list<T>);
};
struct B {
  int k : 1;
};
A<B> a{{0}};
