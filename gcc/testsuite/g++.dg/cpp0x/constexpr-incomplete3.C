// PR c++/49015
// { dg-do compile { target c++11 } }

class A;

class B {
  friend constexpr B f(A); // Line 5
};

class A {};

constexpr B f(A) { return B(); } // Line 10
