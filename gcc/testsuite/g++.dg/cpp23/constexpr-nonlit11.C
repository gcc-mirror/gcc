// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++23 } }
// { dg-options "-Winvalid-constexpr -pedantic-errors" }

// [dcl.constexpr]/4 used to say:
// The definition of a constexpr constructor whose function-body
// is not = delete shall additionally satisfy the following requirements:
// (4.1) for a non-delegating constructor, every constructor selected to initialize non-static data members and base class subobjects shall be a constexpr constructor;
// (4.2) for a delegating constructor, the target constructor shall be a constexpr constructor.

// This continues to be OK.
struct Length {
  constexpr explicit Length(int i = 0) : val(i) { }
private:
  int val;
};

struct X {
  X() {}
  X(int i_) : i(i_) {}
  int i;
};

struct S {
  X x;
  // Calls a non-constexpr constructor X::X(int).
  constexpr S(int i) : x(i) { } // { dg-warning "call to" "" { target { ! implicit_constexpr } } }
  S(int, int) { }
  // Target constructor isn't constexpr.
  constexpr S() : S(42, 42) { } // { dg-warning "call to" "" { target { ! implicit_constexpr } } }
};

namespace N1 {
struct X {
  void x();
};
struct Y {
  X x;
  constexpr void y() { x.x(); } // { dg-warning "call to" }
};
}

void g();

struct A {
  constexpr A() { g(); } // { dg-warning "call to" }
};

struct B {
  constexpr B& operator=(const B&) { g(); return *this; } // { dg-warning "call to" }
  constexpr B& operator=(B&&) { g(); return *this; } // { dg-warning "call to" }
};
