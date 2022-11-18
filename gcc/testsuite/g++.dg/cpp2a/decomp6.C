// CWG2635 - Constrained structured bindings 
// { dg-do compile { target c++20 } }
// { dg-options "-pedantic" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  A(int x) : i(x) {}
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 2; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

template<class T> concept C = true;
C auto [x, y] = A{1}; // { dg-warning "structured binding declaration cannot have constrained 'auto' type 'auto \\\[requires ::C<<placeholder>, >\\\]'" }
