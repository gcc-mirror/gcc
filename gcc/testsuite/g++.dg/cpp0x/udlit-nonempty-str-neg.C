// { dg-options "-std=c++11" }

// Test user-defined literals.
// Test error on non-empty string after 'operator' keyword.

double operator"hi" _badword(long double); // { dg-error "expected empty string after" }
