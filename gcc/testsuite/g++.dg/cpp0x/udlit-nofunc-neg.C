// { dg-options "-std=c++0x" }

// Test user-defined literals.
// Test error on non-function declaration.

double operator"" _baddecl; // { dg-error "as non-function" }

template<char...>
  int operator"" _badtmpldecl; // { dg-error "as non-function" }
