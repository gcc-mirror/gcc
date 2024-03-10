// Verify we don't ICE on an invalid dereference of a variable
// template-id of reference type.
// { dg-do compile { target c++14 } }

template<class>
static constexpr const int& var = 0;

template<class T>
struct B {
  static constexpr int x = *var<T>; // { dg-error "argument of unary" }
  static constexpr const int& y = *var<T>; // { dg-error "argument of unary" }
};

template struct B<int>;
