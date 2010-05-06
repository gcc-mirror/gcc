// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test assignment to int

const int n = nullptr;     // { dg-error "cannot convert " }
