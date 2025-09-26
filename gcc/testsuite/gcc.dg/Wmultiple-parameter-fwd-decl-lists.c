/* { dg-do compile } */
/* { dg-options "-Wmultiple-parameter-fwd-decl-lists" } */

void f(int n, int m; int n, int m);
void g(int n; int m; int n, int m); /* { dg-warning "more than one list of forward declarations" } */
void h(int n; int n; int n); /* { dg-warning "more than one list of forward declarations" } */
