/* { dg-do compile } */

struct a {
  short b : -1ULL;
};
void c(...) { c(a()); }
// { dg-excess-errors "" }
