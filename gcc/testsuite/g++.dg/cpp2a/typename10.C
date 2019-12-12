// P0634R3
// { dg-do compile { target c++2a } }

namespace N {
 // template<typename T> extern T::type v; // #1a
  template<typename T> T::type v(typename T::value); // #1b
}
template<typename T> T::type N::v(T::value); // #2

namespace N2 {
  template<typename T> extern T::type v; // #1a
  //template<typename T> T::type v(typename T::value); // #1b
}
template<typename T> T::type N2::v(T::value);

namespace A {
  inline namespace B { template<typename T> int f(typename T::foo); }
  inline namespace C { template<typename T> extern int f; }
}
template<typename T> int A::f(T::foo); // { dg-error "ambiguous" }
