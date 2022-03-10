/* { dg-do compile } */

struct a {
  short b : -1ULL;  // { dg-warning "exceeds its type" }
};
void c(...) { c(a()); }  // { dg-message "sorry, unimplemented" }

