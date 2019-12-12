// { dg-do compile { target c++2a } }

template<typename T>
  concept Int = __is_same_as(T, int);

template<Int... Ts = int> struct S1; // { dg-error "default argument" }
template<Int... = int> struct S2; // { dg-error "default argument" }
template<Int auto... Ns = 0> struct S3; // { dg-error "default argument" }
template<Int auto... = 0> struct S4; // { dg-error "default argument" }
