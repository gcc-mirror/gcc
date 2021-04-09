// PR c++/99650
// { dg-do compile { target c++17 } }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> void get() { }
};

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = void; };

A a = { 42 };
auto [ x, y ] = a;	// { dg-error ".std::tuple_element<0, A>::type. is .void." }
// { dg-message "in initialization of structured binding variable 'x'" "" { target *-*-* } .-1 }
