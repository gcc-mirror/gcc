// { dg-do compile { target c++11 } }

// Test user-defined literals.
// Test warning on declaration without leading underscore.

long double operator ""nounder(long double); // { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" }

template<char...>
  int operator ""nounder(); // { dg-warning "literal operator suffixes not preceded by|are reserved for future standardization" }
