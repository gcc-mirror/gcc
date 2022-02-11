// PR c++/81176
// { dg-do compile { target c++17 } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

template <class,class> struct same_type;
template <class T> struct same_type<T,T> {};

void
foo (A x)
{
  auto [ a, b ] = x;
  decltype(auto) c = a;
  same_type<decltype(a), int>{};
  same_type<decltype(c), int>{};
}

