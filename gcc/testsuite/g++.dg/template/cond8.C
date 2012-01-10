// PR c++/51318

enum { e0, e1 };

template<bool B, int = B ? e0 : e1> struct A {};

template<typename T> struct B
{
  A<T::X> a;
};
