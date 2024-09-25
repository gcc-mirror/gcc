// { dg-options "-w" }
// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include<typeinfo>
#include<cassert>

// A simple typelist
template<typename... _Elements> struct types {};

// Simple bases implementation
template<typename T> struct b {
  typedef types<__bases(T)...> type;
};

// Simple direct_bases implementation
template<typename T> struct db {
  typedef types<__direct_bases(T)...> type;
};

template <class,class> struct assert_same_type;
template <class T> struct assert_same_type<T,T> {};

struct A {};
struct C : virtual A {};
struct D : public C {};
struct B : D, virtual A {};
struct E : C, virtual D, B {};
struct  F : A, B, E {};

int main() {
  assert_same_type<b<F>::type, types<A,C,D,A,C,D,B,C,C,D,B,E>>();
  assert_same_type<db<F>::type, types<A,B,E>>();
  assert_same_type<db<int>::type, types<>>();
  return 0;
}
