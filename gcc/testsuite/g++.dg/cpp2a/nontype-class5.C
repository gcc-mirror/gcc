// Example from P0732
// { dg-do compile { target c++20 } }

template<class T, T p> class X {
  /* ... */
};

struct A {
  constexpr A(const char*) {}
  // auto operator<=> (const A&) = default;
};
X<A, "Pyrophoricity"> x3; // OK: string literal is a constructor argument to A
