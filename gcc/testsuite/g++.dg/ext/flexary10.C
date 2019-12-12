// PR c++/68490 - error initializing a structure with a flexible array member
// { dg-do compile }
// { dg-options "-Wpedantic" }

struct A {
  int n;
  int a[];  // { dg-warning "7:ISO C\\+\\+ forbids flexible array member" }
};

struct A foo (void)
{
  // Verify the initializer below is accepted for compatibility with gcc
  // (in C mode).
  static struct A
    a = { 2, { 1, 0 } };   // { dg-warning "initialization of a flexible array member" }

  return a;
}
