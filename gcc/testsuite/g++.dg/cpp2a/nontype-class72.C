// PR c++/113800
// P2308R1 - Template parameter initialization
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fno-implicit-constexpr" }
// Invalid cases.

namespace std {
template <typename T> class initializer_list {
  const T *_M_array;
  decltype (sizeof 0) _M_len;
};
}

template<auto>
struct X {};

struct A {
  int i;
};

template<A>
struct B { };

struct E {};

struct I {	  // { dg-message "not literal" }
  I(E) {};
};

template<typename T, T>
struct W {};

void
g ()
{
  X<{0}> x;	  // { dg-error "unable to deduce" }

  int i = 42;	  // { dg-message "not const" }
  B<{i}> b;	  // { dg-error "not usable" }

  W<I, {E{}}> w;  // { dg-error "not a valid type for a template non-type parameter" }
}
