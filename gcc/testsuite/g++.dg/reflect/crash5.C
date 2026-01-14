// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int j;
  int k;
};

// Forgot to define info and we crashed :-(
template <info R>   // { dg-error ".info. has not been declared" }
consteval int fn() {
  S s = {11, 13};
  return s.[:R:];   // { dg-error ".R. was not declared" }
}
