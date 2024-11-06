// { dg-do compile { target c++11 } }
// { dg-options "-Wno-literal-suffix" }

// Test user-defined literals.
// Test "-Wno-literal-suffix" suppresses warnings on declaration without
// leading underscore.

long double operator"" nounder(long double); // { dg-bogus "reserved" }
// { dg-warning "space" "" { target c++23 } .-1 }

template<char...>
  int operator ""nounder(); // { dg-bogus "reserved" }
