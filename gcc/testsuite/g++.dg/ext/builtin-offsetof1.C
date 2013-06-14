// PR c++/51413
// { dg-options "-w" }

struct A
{
  static void foo();
};

int i = __builtin_offsetof(A, foo[1]);  // { dg-error "neither a single identifier nor a sequence of member accesses and array references" }
