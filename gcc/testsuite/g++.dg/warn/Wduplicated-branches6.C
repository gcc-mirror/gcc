// PR c++/99565
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

struct A {
  union { int a; int b; };
  int& foo (bool x) { return x ? a : b; }	// { dg-bogus "this condition has identical branches" }
  void bar (bool x, int y) { if (x) a = y; else b = y; }
};
