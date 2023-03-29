// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++20 } }
// { dg-options "" }
// The definition of a constexpr destructor whose function-body is not
//  =delete shall additionally satisfy the following requirement:
//  (5.1) for every subobject of class type or (possibly multi-dimensional)
//  array thereof, that class type shall have a constexpr destructor.

struct B {
  B() { }
  ~B() { }
};

struct T : B {
  constexpr ~T() { }	// { dg-warning "call to" "" { target c++20_down } }
};

struct S {
  constexpr S() = default;              // was error: implicit S() is not constexpr, now OK
  ~S() noexcept(false) = default;       // OK, despite mismatched exception specification
private:
  int i;
  S(S&);                                // OK: private copy constructor
};
S::S(S&) = default;                     // OK: defines copy constructor
