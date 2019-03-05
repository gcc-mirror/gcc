// PR c++/84355
// { dg-do compile { target c++17 } }

template <class, class> struct same;
template <class T> struct same<T,T> {};

template<typename T> struct A
{
  template<class U> struct B
  {
    B(U);
  };

  A() {
    B b(0);
    same<decltype(b),B<int>>{};
  }
};

struct C {};

A<C> a;
