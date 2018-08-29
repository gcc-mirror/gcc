// PR c++/51413
// PR c++/85277
// { dg-options "-Wno-pointer-arith" }

struct A
{
  static void foo();
};

int i = __builtin_offsetof(A, foo[1]);  // { dg-error "offsetof" }
int j = __builtin_offsetof(volatile A, foo[0]);  // { dg-error "offsetof" }
