/* { dg-do compile } */
/* { dg-options "-fcallgraph-info" } */

void f() {}
void g() __attribute__ ((__alias__ ("f")));

/* { dg-final { scan-dump-times "ci" "triangle" 1 "ci" {{}} } } */
