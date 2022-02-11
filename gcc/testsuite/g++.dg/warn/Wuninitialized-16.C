// PR c++/19808
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

struct S {
  int a;
  int b;
  int c;
  S() : a((b = 42)), c(b) { }
  S(int) : a(((1, b) = 42)), c(b) { }
  S(char) : a(((c++, b) = 42)), c(b) { } // "field .S::c. is used uninitialized" but too complex for the FE
};
