// { dg-do compile { target c++11 } }

// Test user-defined literals.
// Test error on non-function declaration.

double operator ""_baddecl; // { dg-error "8:declaration of .operator\"\"_baddecl. as non-function" }

template<char...>
  int operator ""_badtmpldecl; // { dg-error "7:declaration of .operator\"\"_badtmpldecl. as non-function" }
