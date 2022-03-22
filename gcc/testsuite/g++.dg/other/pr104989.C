// PR rtl-optimization/104989
// { dg-do compile }
// { dg-options "-fnon-call-exceptions" }

struct a {
  short b : -1ULL;
};
void c(...) { c(a()); }
// { dg-excess-errors "" }
