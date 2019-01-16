// PR c++/69253 - [6 Regression] g++ ICE at -O0 on x86_64-linux-gnu
//                in "cxx_incomplete_type_diagnostic"
// { dg-do compile }

struct A {
  int n;
  char a[];   // { dg-error "8:ISO C\\+\\+ forbids flexible array member" }
};

void f ()
{
  // Compound literals and flexible array members are G++ extensions
  // accepted for compatibility with C and GCC.

  // The following use of a flexible array member in a compound literal
  // is invalid in C and rejected by GCC in C mode and so it's also
  // rejected in C++ mode.
  (struct A){ 1, "" };   // { dg-error "forbids compound-literals|initialization of a flexible array member|invalid use of a flexible array member" }
}
