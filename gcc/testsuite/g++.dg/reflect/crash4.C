// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct S {
  int j;
  static int i;
} s;

int X;

// Typo: should have been ^^s.
decltype([: ^^S :].j) x;  // { dg-error "expected a reflection of an expression" }
decltype([: ^^S :].i) z;  // { dg-error "expected a reflection of an expression" }
decltype([: ^^X :].x) y;  // { dg-error "request for member|expected" }
decltype([: ^^:: :].x w;  // { dg-error "expected a reflection of an expression" }
