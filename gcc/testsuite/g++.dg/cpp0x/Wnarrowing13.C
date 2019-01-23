// PR c++/78244
// { dg-do compile { target c++11 } }

template<typename>
struct S {
  static const int i{1.1}; // { dg-error "narrowing conversion" }
  static const int i2 = {1.1}; // { dg-error "narrowing conversion" }
};
