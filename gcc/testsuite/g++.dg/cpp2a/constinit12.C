// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++20 } }

struct S {
  S(int) { }
};

template <class T>
struct U {
  T m;
  constexpr U(int i) : m(i) { } // { dg-error "call to non-.constexpr. function" }
};

constinit U<S> u(42); // { dg-error "does not have a constant initializer|called in a constant expression" }
